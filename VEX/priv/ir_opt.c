/* -*- mode: C; c-basic-offset: 3; -*- */

/*---------------------------------------------------------------*/
/*--- begin                                          ir_opt.c ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2004-2015 OpenWorks LLP
      info@open-works.net

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
   02110-1301, USA.

   The GNU General Public License is contained in the file COPYING.

   Neither the names of the U.S. Department of Energy nor the
   University of California nor the names of its contributors may be
   used to endorse or promote products derived from this software
   without prior written permission.
*/

#include "libvex_basictypes.h"
#include "libvex_ir.h"
#include "libvex.h"

#include "main_util.h"
#include "main_globals.h"
#include "ir_opt.h"


/* Set to 1 for lots of debugging output. */
#define DEBUG_IROPT 0

/* Set to 1 to gather some statistics. Currently only for sameIRExprs. */
#define STATS_IROPT 0


/* What iropt does, 29 Dec 04.

   It takes an IRSB and produces a new one with the same meaning,
   defined thus:

   After execution of the new BB, all guest state and guest memory is
   the same as after execution of the original.  This is true
   regardless of how the block was exited (at the end vs side exit).

   In addition, parts of the guest state will be identical to that
   created by execution of the original at the following observation
   points:

   * In a dirty helper call, any parts of the guest state that the
     helper states that it reads or modifies will be up to date.
     Also, guest memory will be up to date.  Parts of the guest state
     not marked as being read or modified by the helper cannot be
     assumed to be up-to-date at the point where the helper is called.

   * If iropt_register_updates == VexRegUpdSpAtMemAccess :
     The guest state is only up to date only as explained above
     (i.e. at SB exits and as specified by dirty helper call).
     Also, the stack pointer register is up to date at memory
     exception points (as this is needed for the stack extension
     logic in m_signals.c).

   * If iropt_register_updates == VexRegUpdUnwindregsAtMemAccess :
     Immediately prior to any load or store, those parts of the guest
     state marked as requiring precise exceptions will be up to date.
     Also, guest memory will be up to date.  Parts of the guest state
     not marked as requiring precise exceptions cannot be assumed to
     be up-to-date at the point of the load/store.

   * If iropt_register_updates == VexRegUpdAllregsAtMemAccess:
     Same as minimal, but all the guest state is up to date at memory
     exception points.

   * If iropt_register_updates == VexRegUpdAllregsAtEachInsn :
     Guest state is up to date at each instruction.

   The relative order of loads and stores (including loads/stores of
   guest memory done by dirty helpers annotated as such) is not
   changed.  However, the relative order of loads with no intervening
   stores/modifies may be changed.  

   Transformation order
   ~~~~~~~~~~~~~~~~~~~~

   There are three levels of optimisation, controlled by
   vex_control.iropt_level.  Define first:

   "Cheap transformations" are the following sequence:
      * Redundant-Get removal
      * Redundant-Put removal
      * Constant propagation/folding
      * Dead code removal
      * Specialisation of clean helper functions
      * Dead code removal

   "Expensive transformations" are the following sequence:
      * CSE
      * Folding of add/sub chains
      * Redundant-GetI removal
      * Redundant-PutI removal
      * Dead code removal

   Then the transformations are as follows, as defined by
   vex_control.iropt_level:

   Level 0: 
      * Flatten into atomic form.

   Level 1: the following sequence:
      * Flatten into atomic form.
      * Cheap transformations.

   Level 2: the following sequence
      * Flatten into atomic form.
      * Cheap transformations.
      * If block contains any floating or vector types, CSE.
      * If block contains GetI or PutI, Expensive transformations.
      * Try unrolling loops.  Three possible outcomes:
        - No effect: do nothing more.
        - Unrolled a loop, and block does not contain GetI or PutI:
          Do: * CSE
              * Dead code removal
        - Unrolled a loop, and block contains GetI or PutI:
          Do: * Expensive transformations
              * Cheap transformations
*/

/* Implementation notes, 29 Dec 04.

   TODO (important): I think rPutI removal ignores precise exceptions
   and is therefore in a sense, wrong.  In the sense that PutIs are
   assumed not to write parts of the guest state that we need to have
   up-to-date at loads/stores.  So far on x86 guest that has not
   mattered since indeed only the x87 FP registers and tags are
   accessed using GetI/PutI, and there is no need so far for them to
   be up to date at mem exception points.  The rPutI pass should be
   fixed.

   TODO: improve pessimistic handling of precise exceptions
     in the tree builder.

   TODO: check interaction of rGetI and dirty helpers. 
   
   F64i constants are treated differently from other constants.
   They are not regarded as atoms, and instead lifted off and
   bound to temps.  This allows them to participate in CSE, which
   is important for getting good performance for x86 guest code.

   CSE up F64 literals (already doing F64is)

   CSE: consider carefully the requirement for precise exns
        prior to making CSE any more aggressive.  */


/*---------------------------------------------------------------*/
/*--- Finite mappery, of a sort                               ---*/
/*---------------------------------------------------------------*/

/* General map from HWord-sized thing HWord-sized thing.  Could be by
   hashing, but it's not clear whether or not this would really be any
   faster. */

typedef
   struct {
      Bool*  inuse;
      HWord* key;
      HWord* val;
      Int    size;
      Int    used;
   }
   HashHW;

static HashHW* newHHW ( void )
{
   HashHW* h = LibVEX_Alloc_inline(sizeof(HashHW));
   h->size   = 8;
   h->used   = 0;
   h->inuse  = LibVEX_Alloc_inline(h->size * sizeof(Bool));
   h->key    = LibVEX_Alloc_inline(h->size * sizeof(HWord));
   h->val    = LibVEX_Alloc_inline(h->size * sizeof(HWord));
   return h;
}


/* Look up key in the map. */

static Bool lookupHHW ( HashHW* h, /*OUT*/HWord* val, HWord key )
{
   Int i;
   /* vex_printf("lookupHHW(%llx)\n", key ); */
   for (i = 0; i < h->used; i++) {
      if (h->inuse[i] && h->key[i] == key) {
         if (val)
            *val = h->val[i];
         return True;
      }
   }
   return False;
}


/* Add key->val to the map.  Replaces any existing binding for key. */

static void addToHHW ( HashHW* h, HWord key, HWord val )
{
   Int i, j;
   /* vex_printf("addToHHW(%llx, %llx)\n", key, val); */

   /* Find and replace existing binding, if any. */
   for (i = 0; i < h->used; i++) {
      if (h->inuse[i] && h->key[i] == key) {
         h->val[i] = val;
         return;
      }
   }

   /* Ensure a space is available. */
   if (h->used == h->size) {
      /* Copy into arrays twice the size. */
      Bool*  inuse2 = LibVEX_Alloc_inline(2 * h->size * sizeof(Bool));
      HWord* key2   = LibVEX_Alloc_inline(2 * h->size * sizeof(HWord));
      HWord* val2   = LibVEX_Alloc_inline(2 * h->size * sizeof(HWord));
      for (i = j = 0; i < h->size; i++) {
         if (!h->inuse[i]) continue;
         inuse2[j] = True;
         key2[j] = h->key[i];
         val2[j] = h->val[i];
         j++;
      }
      h->used = j;
      h->size *= 2;
      h->inuse = inuse2;
      h->key = key2;
      h->val = val2;
   }

   /* Finally, add it. */
   vassert(h->used < h->size);
   h->inuse[h->used] = True;
   h->key[h->used] = key;
   h->val[h->used] = val;
   h->used++;
}


/*---------------------------------------------------------------*/
/*--- Flattening out a BB into atomic SSA form                ---*/
/*---------------------------------------------------------------*/

/* Non-critical helper, heuristic for reducing the number of tmp-tmp
   copies made by flattening.  If in doubt return False. */

static Bool isFlat ( IRExpr* e )
{
   if (e->tag == Iex_Get) 
      return True;
   if (e->tag == Iex_Binop)
      return toBool( isIRAtom(e->Iex.Binop.arg1) 
                     && isIRAtom(e->Iex.Binop.arg2) );
   if (e->tag == Iex_Load)
      return isIRAtom(e->Iex.Load.addr);
   return False;
}

/* Flatten out 'ex' so it is atomic, returning a new expression with
   the same value, after having appended extra IRTemp assignments to
   the end of 'bb'. */

static IRExpr* flatten_Expr ( IRSB* bb, IRExpr* ex )
{
   Int i;
   IRExpr** newargs;
   IRType ty = typeOfIRExpr(bb->tyenv, ex);
   IRTemp t1;

   switch (ex->tag) {

      case Iex_GetI:
         t1 = newIRTemp(bb->tyenv, ty);
         addStmtToIRSB(bb, IRStmt_WrTmp(t1,
            IRExpr_GetI(ex->Iex.GetI.descr,
                        flatten_Expr(bb, ex->Iex.GetI.ix),
                        ex->Iex.GetI.bias)));
         return IRExpr_RdTmp(t1);

      case Iex_Get:
         t1 = newIRTemp(bb->tyenv, ty);
         addStmtToIRSB(bb, 
            IRStmt_WrTmp(t1, ex));
         return IRExpr_RdTmp(t1);

      case Iex_Qop: {
         IRQop* qop = ex->Iex.Qop.details;
         t1 = newIRTemp(bb->tyenv, ty);
         addStmtToIRSB(bb, IRStmt_WrTmp(t1, 
            IRExpr_Qop(qop->op,
                         flatten_Expr(bb, qop->arg1),
                         flatten_Expr(bb, qop->arg2),
                         flatten_Expr(bb, qop->arg3),
                         flatten_Expr(bb, qop->arg4))));
         return IRExpr_RdTmp(t1);
      }

      case Iex_Triop: {
         IRTriop* triop = ex->Iex.Triop.details;
         t1 = newIRTemp(bb->tyenv, ty);
         addStmtToIRSB(bb, IRStmt_WrTmp(t1, 
            IRExpr_Triop(triop->op,
                         flatten_Expr(bb, triop->arg1),
                         flatten_Expr(bb, triop->arg2),
                         flatten_Expr(bb, triop->arg3))));
         return IRExpr_RdTmp(t1);
      }

      case Iex_Binop:
         t1 = newIRTemp(bb->tyenv, ty);
         addStmtToIRSB(bb, IRStmt_WrTmp(t1, 
            IRExpr_Binop(ex->Iex.Binop.op,
                         flatten_Expr(bb, ex->Iex.Binop.arg1),
                         flatten_Expr(bb, ex->Iex.Binop.arg2))));
         return IRExpr_RdTmp(t1);

      case Iex_Unop:
         t1 = newIRTemp(bb->tyenv, ty);
         addStmtToIRSB(bb, IRStmt_WrTmp(t1, 
            IRExpr_Unop(ex->Iex.Unop.op,
                        flatten_Expr(bb, ex->Iex.Unop.arg))));
         return IRExpr_RdTmp(t1);

      case Iex_Load:
         t1 = newIRTemp(bb->tyenv, ty);
         addStmtToIRSB(bb, IRStmt_WrTmp(t1,
            IRExpr_Load(ex->Iex.Load.end,
                        ex->Iex.Load.ty, 
                        flatten_Expr(bb, ex->Iex.Load.addr))));
         return IRExpr_RdTmp(t1);

      case Iex_CCall:
         newargs = shallowCopyIRExprVec(ex->Iex.CCall.args);
         for (i = 0; newargs[i]; i++)
            newargs[i] = flatten_Expr(bb, newargs[i]);
         t1 = newIRTemp(bb->tyenv, ty);
         addStmtToIRSB(bb, IRStmt_WrTmp(t1,
            IRExpr_CCall(ex->Iex.CCall.cee,
                         ex->Iex.CCall.retty,
                         newargs)));
         return IRExpr_RdTmp(t1);

      case Iex_ITE:
         t1 = newIRTemp(bb->tyenv, ty);
         addStmtToIRSB(bb, IRStmt_WrTmp(t1,
            IRExpr_ITE(flatten_Expr(bb, ex->Iex.ITE.cond),
                       flatten_Expr(bb, ex->Iex.ITE.iftrue),
                       flatten_Expr(bb, ex->Iex.ITE.iffalse))));
         return IRExpr_RdTmp(t1);

      case Iex_Const:
         /* Lift F64i constants out onto temps so they can be CSEd
            later. */
         if (ex->Iex.Const.con->tag == Ico_F64i) {
            t1 = newIRTemp(bb->tyenv, ty);
            addStmtToIRSB(bb, IRStmt_WrTmp(t1,
               IRExpr_Const(ex->Iex.Const.con)));
            return IRExpr_RdTmp(t1);
         } else {
            /* Leave all other constants alone. */
            return ex;
         }

      case Iex_RdTmp:
         return ex;

      default:
         vex_printf("\n");
         ppIRExpr(ex); 
         vex_printf("\n");
         vpanic("flatten_Expr");
   }
}


/* Append a completely flattened form of 'st' to the end of 'bb'. */

static void flatten_Stmt ( IRSB* bb, IRStmt* st )
{
   Int i;
   IRExpr   *e1, *e2, *e3, *e4, *e5;
   IRDirty  *d,  *d2;
   IRCAS    *cas, *cas2;
   IRPutI   *puti, *puti2;
   IRLoadG  *lg;
   IRStoreG *sg;
   switch (st->tag) {
      case Ist_Put:
         if (isIRAtom(st->Ist.Put.data)) {
            /* optimisation to reduce the amount of heap wasted
               by the flattener */
            addStmtToIRSB(bb, st);
         } else {
            /* general case, always correct */
            e1 = flatten_Expr(bb, st->Ist.Put.data);
            addStmtToIRSB(bb, IRStmt_Put(st->Ist.Put.offset, e1));
         }
         break;
      case Ist_PutI:
         puti = st->Ist.PutI.details;
         e1 = flatten_Expr(bb, puti->ix);
         e2 = flatten_Expr(bb, puti->data);
         puti2 = mkIRPutI(puti->descr, e1, puti->bias, e2);
         addStmtToIRSB(bb, IRStmt_PutI(puti2));
         break;
      case Ist_WrTmp:
         if (isFlat(st->Ist.WrTmp.data)) {
            /* optimisation, to reduce the number of tmp-tmp
               copies generated */
            addStmtToIRSB(bb, st);
         } else {
            /* general case, always correct */
            e1 = flatten_Expr(bb, st->Ist.WrTmp.data);
            addStmtToIRSB(bb, IRStmt_WrTmp(st->Ist.WrTmp.tmp, e1));
         }
         break;
      case Ist_Store:
         e1 = flatten_Expr(bb, st->Ist.Store.addr);
         e2 = flatten_Expr(bb, st->Ist.Store.data);
         addStmtToIRSB(bb, IRStmt_Store(st->Ist.Store.end, e1,e2));
         break;
      case Ist_StoreG:
         sg = st->Ist.StoreG.details;
         e1 = flatten_Expr(bb, sg->addr);
         e2 = flatten_Expr(bb, sg->data);
         e3 = flatten_Expr(bb, sg->guard);
         addStmtToIRSB(bb, IRStmt_StoreG(sg->end, e1, e2, e3));
         break;
      case Ist_LoadG:
         lg = st->Ist.LoadG.details;
         e1 = flatten_Expr(bb, lg->addr);
         e2 = flatten_Expr(bb, lg->alt);
         e3 = flatten_Expr(bb, lg->guard);
         addStmtToIRSB(bb, IRStmt_LoadG(lg->end, lg->cvt, lg->dst,
                                        e1, e2, e3));
         break;
      case Ist_CAS:
         cas  = st->Ist.CAS.details;
         e1   = flatten_Expr(bb, cas->addr);
         e2   = cas->expdHi ? flatten_Expr(bb, cas->expdHi) : NULL;
         e3   = flatten_Expr(bb, cas->expdLo);
         e4   = cas->dataHi ? flatten_Expr(bb, cas->dataHi) : NULL;
         e5   = flatten_Expr(bb, cas->dataLo);
         cas2 = mkIRCAS( cas->oldHi, cas->oldLo, cas->end,
                         e1, e2, e3, e4, e5 );
         addStmtToIRSB(bb, IRStmt_CAS(cas2));
         break;
      case Ist_LLSC:
         e1 = flatten_Expr(bb, st->Ist.LLSC.addr);
         e2 = st->Ist.LLSC.storedata
                 ? flatten_Expr(bb, st->Ist.LLSC.storedata)
                 : NULL;
         addStmtToIRSB(bb, IRStmt_LLSC(st->Ist.LLSC.end,
                                       st->Ist.LLSC.result, e1, e2));
         break;
      case Ist_Dirty:
         d = st->Ist.Dirty.details;
         d2 = emptyIRDirty();
         *d2 = *d;
         d2->args = shallowCopyIRExprVec(d2->args);
         if (d2->mFx != Ifx_None) {
            d2->mAddr = flatten_Expr(bb, d2->mAddr);
         } else {
            vassert(d2->mAddr == NULL);
         }
         d2->guard = flatten_Expr(bb, d2->guard);
         for (i = 0; d2->args[i]; i++) {
            IRExpr* arg = d2->args[i];
            if (LIKELY(!is_IRExpr_VECRET_or_BBPTR(arg)))
               d2->args[i] = flatten_Expr(bb, arg);
         }
         addStmtToIRSB(bb, IRStmt_Dirty(d2));
         break;
      case Ist_NoOp:
      case Ist_MBE:
      case Ist_IMark:
         addStmtToIRSB(bb, st);
         break;
      case Ist_AbiHint:
         e1 = flatten_Expr(bb, st->Ist.AbiHint.base);
         e2 = flatten_Expr(bb, st->Ist.AbiHint.nia);
         addStmtToIRSB(bb, IRStmt_AbiHint(e1, st->Ist.AbiHint.len, e2));
         break;
      case Ist_Exit:
         e1 = flatten_Expr(bb, st->Ist.Exit.guard);
         addStmtToIRSB(bb, IRStmt_Exit(e1, st->Ist.Exit.jk,
                                       st->Ist.Exit.dst,
                                       st->Ist.Exit.offsIP));
         break;
      default:
         vex_printf("\n");
         ppIRStmt(st); 
         vex_printf("\n");
         vpanic("flatten_Stmt");
   }
}


static IRSB* flatten_BB ( IRSB* in )
{
   Int   i;
   IRSB* out;
   out = emptyIRSB();
   out->tyenv = deepCopyIRTypeEnv( in->tyenv );
   for (i = 0; i < in->stmts_used; i++)
      if (in->stmts[i])
         flatten_Stmt( out, in->stmts[i] );
   out->next     = flatten_Expr( out, in->next );
   out->jumpkind = in->jumpkind;
   out->offsIP   = in->offsIP;
   return out;
}


/*---------------------------------------------------------------*/
/*--- In-place removal of redundant GETs                      ---*/
/*---------------------------------------------------------------*/

/* Scan forwards, building up an environment binding (min offset, max
   offset) pairs to values, which will either be temps or constants.

   On seeing 't = Get(minoff,maxoff)', look up (minoff,maxoff) in the
   env and if it matches, replace the Get with the stored value.  If
   there is no match, add a (minoff,maxoff) :-> t binding.

   On seeing 'Put (minoff,maxoff) = t or c', first remove in the env
   any binding which fully or partially overlaps with (minoff,maxoff).
   Then add a new (minoff,maxoff) :-> t or c binding.  */

/* Extract the min/max offsets from a guest state array descriptor. */

inline
static void getArrayBounds ( IRRegArray* descr, 
                             UInt* minoff, UInt* maxoff )
{
   *minoff = descr->base;
   *maxoff = *minoff + descr->nElems*sizeofIRType(descr->elemTy) - 1;
   vassert((*minoff & ~0xFFFF) == 0);
   vassert((*maxoff & ~0xFFFF) == 0);
   vassert(*minoff <= *maxoff);
}

/* Create keys, of the form ((minoffset << 16) | maxoffset). */

static UInt mk_key_GetPut ( Int offset, IRType ty )
{
   /* offset should fit in 16 bits. */
   UInt minoff = offset;
   UInt maxoff = minoff + sizeofIRType(ty) - 1;
   vassert((minoff & ~0xFFFF) == 0);
   vassert((maxoff & ~0xFFFF) == 0);
   return (minoff << 16) | maxoff;
}

static UInt mk_key_GetIPutI ( IRRegArray* descr )
{
   UInt minoff, maxoff;
   getArrayBounds( descr, &minoff, &maxoff );
   vassert((minoff & ~0xFFFF) == 0);
   vassert((maxoff & ~0xFFFF) == 0);
   return (minoff << 16) | maxoff;
}

/* Supposing h has keys of the form generated by mk_key_GetPut and
   mk_key_GetIPutI, invalidate any key which overlaps (k_lo
   .. k_hi). 
*/
static void invalidateOverlaps ( HashHW* h, UInt k_lo, UInt k_hi )
{
   Int  j;
   UInt e_lo, e_hi;
   vassert(k_lo <= k_hi);
   /* invalidate any env entries which in any way overlap (k_lo
      .. k_hi) */
   /* vex_printf("invalidate %d .. %d\n", k_lo, k_hi ); */

   for (j = 0; j < h->used; j++) {
      if (!h->inuse[j]) 
         continue;
      e_lo = (((UInt)h->key[j]) >> 16) & 0xFFFF;
      e_hi = ((UInt)h->key[j]) & 0xFFFF;
      vassert(e_lo <= e_hi);
      if (e_hi < k_lo || k_hi < e_lo)
         continue; /* no overlap possible */
      else
         /* overlap; invalidate */
         h->inuse[j] = False;
   }
}


static void redundant_get_removal_BB ( IRSB* bb )
{
   HashHW* env = newHHW();
   UInt    key = 0; /* keep gcc -O happy */
   Int     i, j;
   HWord   val;

   for (i = 0; i < bb->stmts_used; i++) {
      IRStmt* st = bb->stmts[i];

      if (st->tag == Ist_NoOp)
         continue;

      /* Deal with Gets */
      if (st->tag == Ist_WrTmp
          && st->Ist.WrTmp.data->tag == Iex_Get) {
         /* st is 't = Get(...)'.  Look up in the environment and see
            if the Get can be replaced. */
         IRExpr* get = st->Ist.WrTmp.data;
         key = (HWord)mk_key_GetPut( get->Iex.Get.offset, 
                                     get->Iex.Get.ty );
         if (lookupHHW(env, &val, (HWord)key)) {
            /* found it */
            /* Note, we could do better here.  If the types are
               different we don't do the substitution, since doing so
               could lead to invalidly-typed IR.  An improvement would
               be to stick in a reinterpret-style cast, although that
               would make maintaining flatness more difficult. */
            IRExpr* valE    = (IRExpr*)val;
            Bool    typesOK = toBool( typeOfIRExpr(bb->tyenv,valE) 
                                      == st->Ist.WrTmp.data->Iex.Get.ty );
            if (typesOK && DEBUG_IROPT) {
               vex_printf("rGET: "); ppIRExpr(get);
               vex_printf("  ->  "); ppIRExpr(valE);
               vex_printf("\n");
            }
            if (typesOK)
               bb->stmts[i] = IRStmt_WrTmp(st->Ist.WrTmp.tmp, valE);
         } else {
            /* Not found, but at least we know that t and the Get(...)
               are now associated.  So add a binding to reflect that
               fact. */
            addToHHW( env, (HWord)key, 
                           (HWord)(void*)(IRExpr_RdTmp(st->Ist.WrTmp.tmp)) );
         }
      }

      /* Deal with Puts: invalidate any env entries overlapped by this
         Put */
      if (st->tag == Ist_Put || st->tag == Ist_PutI) {
         UInt k_lo, k_hi;
         if (st->tag == Ist_Put) {
            key = mk_key_GetPut( st->Ist.Put.offset, 
                                 typeOfIRExpr(bb->tyenv,st->Ist.Put.data) );
         } else {
            vassert(st->tag == Ist_PutI);
            key = mk_key_GetIPutI( st->Ist.PutI.details->descr );
         }

         k_lo = (key >> 16) & 0xFFFF;
         k_hi = key & 0xFFFF;
         invalidateOverlaps(env, k_lo, k_hi);
      }
      else
      if (st->tag == Ist_Dirty) {
         /* Deal with dirty helpers which write or modify guest state.
            Invalidate the entire env.  We could do a lot better
            here. */
         IRDirty* d      = st->Ist.Dirty.details;
         Bool     writes = False;
         for (j = 0; j < d->nFxState; j++) {
            if (d->fxState[j].fx == Ifx_Modify 
                || d->fxState[j].fx == Ifx_Write)
            writes = True;
         }
         if (writes) {
            /* dump the entire env (not clever, but correct ...) */
            for (j = 0; j < env->used; j++)
               env->inuse[j] = False;
            if (0) vex_printf("rGET: trash env due to dirty helper\n");
         }
      }

      /* add this one to the env, if appropriate */
      if (st->tag == Ist_Put) {
         vassert(isIRAtom(st->Ist.Put.data));
         addToHHW( env, (HWord)key, (HWord)(st->Ist.Put.data));
      }

   } /* for (i = 0; i < bb->stmts_used; i++) */

}


/*---------------------------------------------------------------*/
/*--- In-place removal of redundant PUTs                      ---*/
/*---------------------------------------------------------------*/

/* Find any Get uses in st and invalidate any partially or fully
   overlapping ranges listed in env.  Due to the flattening phase, the
   only stmt kind we expect to find a Get on is IRStmt_WrTmp. */

static void handle_gets_Stmt ( 
               HashHW* env, 
               IRStmt* st,
               Bool (*preciseMemExnsFn)(Int,Int,VexRegisterUpdates),
               VexRegisterUpdates pxControl
            )
{
   Int     j;
   UInt    key = 0; /* keep gcc -O happy */
   Bool    isGet;
   Bool    memRW = False;
   IRExpr* e;

   switch (st->tag) {

      /* This is the only interesting case.  Deal with Gets in the RHS
         expression. */
      case Ist_WrTmp:
         e = st->Ist.WrTmp.data;
         switch (e->tag) {
            case Iex_Get:
               isGet = True;
               key = mk_key_GetPut ( e->Iex.Get.offset, e->Iex.Get.ty );
               break;
            case Iex_GetI:
               isGet = True;
               key = mk_key_GetIPutI ( e->Iex.GetI.descr );
               break;
            case Iex_Load:
               isGet = False;
               memRW = True;
               break;
            default: 
               isGet = False;
         }
         if (isGet) {
            UInt k_lo, k_hi;
            k_lo = (key >> 16) & 0xFFFF;
            k_hi = key & 0xFFFF;
            invalidateOverlaps(env, k_lo, k_hi);
         }
         break;

      /* Be very conservative for dirty helper calls; dump the entire
         environment.  The helper might read guest state, in which
         case it needs to be flushed first.  Also, the helper might
         access guest memory, in which case all parts of the guest
         state requiring precise exceptions needs to be flushed.  The
         crude solution is just to flush everything; we could easily
         enough do a lot better if needed. */
      /* Probably also overly-conservative, but also dump everything
         if we hit a memory bus event (fence, lock, unlock).  Ditto
         AbiHints, CASs, LLs and SCs. */
      case Ist_AbiHint:
         vassert(isIRAtom(st->Ist.AbiHint.base));
         vassert(isIRAtom(st->Ist.AbiHint.nia));
         /* fall through */
      case Ist_MBE:
      case Ist_Dirty:
      case Ist_CAS:
      case Ist_LLSC:
         for (j = 0; j < env->used; j++)
            env->inuse[j] = False;
         break;

      /* all other cases are boring. */
      case Ist_Store:
         vassert(isIRAtom(st->Ist.Store.addr));
         vassert(isIRAtom(st->Ist.Store.data));
         memRW = True;
         break;
      case Ist_StoreG: {
         IRStoreG* sg = st->Ist.StoreG.details;
         vassert(isIRAtom(sg->addr));
         vassert(isIRAtom(sg->data));
         vassert(isIRAtom(sg->guard));
         memRW = True;
         break;
      }
      case Ist_LoadG: {
         IRLoadG* lg = st->Ist.LoadG.details;
         vassert(isIRAtom(lg->addr));
         vassert(isIRAtom(lg->alt));
         vassert(isIRAtom(lg->guard));
         memRW = True;
         break;
      }
      case Ist_Exit:
         vassert(isIRAtom(st->Ist.Exit.guard));
         break;

      case Ist_Put:
         vassert(isIRAtom(st->Ist.Put.data));
         break;

      case Ist_PutI:
         vassert(isIRAtom(st->Ist.PutI.details->ix));
         vassert(isIRAtom(st->Ist.PutI.details->data));
         break;

      case Ist_NoOp:
      case Ist_IMark:
         break;

      default:
         vex_printf("\n");
         ppIRStmt(st);
         vex_printf("\n");
         vpanic("handle_gets_Stmt");
   }

   if (memRW) {
      /* This statement accesses memory.  So we might need to dump all parts
         of the environment corresponding to guest state that may not
         be reordered with respect to memory references.  That means
         at least the stack pointer. */
      switch (pxControl) {
         case VexRegUpdAllregsAtMemAccess:
            /* Precise exceptions required at mem access.
               Flush all guest state. */
            for (j = 0; j < env->used; j++)
               env->inuse[j] = False;
            break;
         case VexRegUpdSpAtMemAccess:
            /* We need to dump the stack pointer
               (needed for stack extension in m_signals.c).
               preciseMemExnsFn will use vex_control.iropt_register_updates
               to verify only the sp is to be checked. */
            /* fallthrough */
         case VexRegUpdUnwindregsAtMemAccess:
            for (j = 0; j < env->used; j++) {
               if (!env->inuse[j])
                  continue;
               /* Just flush the minimal amount required, as computed by
                  preciseMemExnsFn. */
               HWord k_lo = (env->key[j] >> 16) & 0xFFFF;
               HWord k_hi = env->key[j] & 0xFFFF;
               if (preciseMemExnsFn( k_lo, k_hi, pxControl ))
                  env->inuse[j] = False;
            }
            break;
         case VexRegUpdAllregsAtEachInsn:
            // VexRegUpdAllregsAtEachInsn cannot happen here.
            // fall through
         case VexRegUpd_INVALID:
         default:
            vassert(0);
      }
   } /* if (memRW) */

}


/* Scan backwards, building up a set of (min offset, max
   offset) pairs, indicating those parts of the guest state
   for which the next event is a write.

   On seeing a conditional exit, empty the set.

   On seeing 'Put (minoff,maxoff) = t or c', if (minoff,maxoff) is
   completely within the set, remove the Put.  Otherwise, add
   (minoff,maxoff) to the set.

   On seeing 'Get (minoff,maxoff)', remove any part of the set
   overlapping (minoff,maxoff).  The same has to happen for any events
   which implicitly read parts of the guest state: dirty helper calls
   and loads/stores.
*/

static void redundant_put_removal_BB ( 
               IRSB* bb,
               Bool (*preciseMemExnsFn)(Int,Int,VexRegisterUpdates),
               VexRegisterUpdates pxControl
            )
{
   Int     i, j;
   Bool    isPut;
   IRStmt* st;
   UInt    key = 0; /* keep gcc -O happy */

   vassert(pxControl < VexRegUpdAllregsAtEachInsn);

   HashHW* env = newHHW();

   /* Initialise the running env with the fact that the final exit
      writes the IP (or, whatever it claims to write.  We don't
      care.) */
   key = mk_key_GetPut(bb->offsIP, typeOfIRExpr(bb->tyenv, bb->next));
   addToHHW(env, (HWord)key, 0);

   /* And now scan backwards through the statements. */
   for (i = bb->stmts_used-1; i >= 0; i--) {
      st = bb->stmts[i];

      if (st->tag == Ist_NoOp)
         continue;

      /* Deal with conditional exits. */
      if (st->tag == Ist_Exit) {
         //Bool re_add;
         /* Need to throw out from the env, any part of it which
            doesn't overlap with the guest state written by this exit.
            Since the exit only writes one section, it's simplest to
            do this: (1) check whether env contains a write that
            completely overlaps the write done by this exit; (2) empty
            out env; and (3) if (1) was true, add the write done by
            this exit.

            To make (1) a bit simpler, merely search for a write that
            exactly matches the one done by this exit.  That's safe
            because it will fail as often or more often than a full
            overlap check, and failure to find an overlapping write in
            env is the safe case (we just nuke env if that
            happens). */
         //vassert(isIRAtom(st->Ist.Exit.guard));
         /* (1) */
         //key = mk_key_GetPut(st->Ist.Exit.offsIP,
         //                    typeOfIRConst(st->Ist.Exit.dst));
         //re_add = lookupHHW(env, NULL, key);
         /* (2) */
         for (j = 0; j < env->used; j++)
            env->inuse[j] = False;
         /* (3) */
         //if (0 && re_add) 
         //   addToHHW(env, (HWord)key, 0);
         continue;
      }

      /* Deal with Puts */
      switch (st->tag) {
         case Ist_Put: 
            isPut = True;
            key = mk_key_GetPut( st->Ist.Put.offset, 
                                 typeOfIRExpr(bb->tyenv,st->Ist.Put.data) );
            vassert(isIRAtom(st->Ist.Put.data));
            break;
         case Ist_PutI:
            isPut = True;
            key = mk_key_GetIPutI( st->Ist.PutI.details->descr );
            vassert(isIRAtom(st->Ist.PutI.details->ix));
            vassert(isIRAtom(st->Ist.PutI.details->data));
            break;
         default: 
            isPut = False;
      }
      if (isPut && st->tag != Ist_PutI) {
         /* See if any single entry in env overlaps this Put.  This is
            simplistic in that the transformation is valid if, say, two
            or more entries in the env overlap this Put, but the use of
            lookupHHW will only find a single entry which exactly
            overlaps this Put.  This is suboptimal but safe. */
         if (lookupHHW(env, NULL, (HWord)key)) {
            /* This Put is redundant because a later one will overwrite
               it.  So NULL (nop) it out. */
            if (DEBUG_IROPT) {
               vex_printf("rPUT: "); ppIRStmt(st);
               vex_printf("\n");
            }
            bb->stmts[i] = IRStmt_NoOp();
         } else {
            /* We can't demonstrate that this Put is redundant, so add it
               to the running collection. */
            addToHHW(env, (HWord)key, 0);
         }
         continue;
      }

      /* Deal with Gets.  These remove bits of the environment since
         appearance of a Get means that the next event for that slice
         of the guest state is no longer a write, but a read.  Also
         deals with implicit reads of guest state needed to maintain
         precise exceptions. */
      handle_gets_Stmt( env, st, preciseMemExnsFn, pxControl );
   }
}


/*---------------------------------------------------------------*/
/*--- Constant propagation and folding                        ---*/
/*---------------------------------------------------------------*/

#if STATS_IROPT
/* How often sameIRExprs was invoked */
static UInt invocation_count;
/* How often sameIRExprs recursed through IRTemp assignments */
static UInt recursion_count;
/* How often sameIRExprs found identical IRExprs */
static UInt success_count;
/* How often recursing through assignments to IRTemps helped
   establishing equality. */
static UInt recursion_success_count;
/* Whether or not recursing through an IRTemp assignment helped 
   establishing IRExpr equality for a given sameIRExprs invocation. */
static Bool recursion_helped;
/* Whether or not a given sameIRExprs invocation recursed through an
   IRTemp assignment */
static Bool recursed;
/* Maximum number of nodes ever visited when comparing two IRExprs. */
static UInt max_nodes_visited;
#endif /* STATS_IROPT */

/* Count the number of nodes visited for a given sameIRExprs invocation. */
static UInt num_nodes_visited;

/* Do not visit more than NODE_LIMIT nodes when comparing two IRExprs.
   This is to guard against performance degradation by visiting large
   trees without success. */
#define NODE_LIMIT 30


/* The env in this section is a map from IRTemp to IRExpr*,
   that is, an array indexed by IRTemp. */

/* Do both expressions compute the same value? The answer is generally
   conservative, i.e. it will report that the expressions do not compute
   the same value when in fact they do. The reason is that we do not
   keep track of changes in the guest state and memory. Thusly, two
   Get's, GetI's or Load's, even when accessing the same location, will be
   assumed to compute different values. After all the accesses may happen
   at different times and the guest state / memory can have changed in
   the meantime.

   XXX IMPORTANT XXX the two expressions must have the same IR type.
   DO NOT CALL HERE WITH DIFFERENTLY-TYPED EXPRESSIONS. */

/* JRS 20-Mar-2012: split sameIRExprs_aux into a fast inlineable
   wrapper that deals with the common tags-don't-match case, and a
   slower out of line general case.  Saves a few insns. */

__attribute__((noinline))
static Bool sameIRExprs_aux2 ( IRExpr** env, IRExpr* e1, IRExpr* e2 );

inline
static Bool sameIRExprs_aux ( IRExpr** env, IRExpr* e1, IRExpr* e2 )
{
   if (e1->tag != e2->tag) return False;
   return sameIRExprs_aux2(env, e1, e2);
}

__attribute__((noinline))
static Bool sameIRExprs_aux2 ( IRExpr** env, IRExpr* e1, IRExpr* e2 )
{
   if (num_nodes_visited++ > NODE_LIMIT) return False;

   switch (e1->tag) {
      case Iex_RdTmp:
         if (e1->Iex.RdTmp.tmp == e2->Iex.RdTmp.tmp) return True;

         if (env[e1->Iex.RdTmp.tmp] && env[e2->Iex.RdTmp.tmp]) {
            Bool same = sameIRExprs_aux(env, env[e1->Iex.RdTmp.tmp],
                                        env[e2->Iex.RdTmp.tmp]);
#if STATS_IROPT
            recursed = True;
            if (same) recursion_helped = True;
#endif
            return same;
         }
         return False;

      case Iex_Get:
      case Iex_GetI:
      case Iex_Load:
         /* Guest state / memory could have changed in the meantime. */
         return False;

      case Iex_Binop:
         return toBool( e1->Iex.Binop.op == e2->Iex.Binop.op
                        && sameIRExprs_aux( env, e1->Iex.Binop.arg1,
                                                 e2->Iex.Binop.arg1 )
                        && sameIRExprs_aux( env, e1->Iex.Binop.arg2,
                                                 e2->Iex.Binop.arg2 ));

      case Iex_Unop:
         return toBool( e1->Iex.Unop.op == e2->Iex.Unop.op
                        && sameIRExprs_aux( env, e1->Iex.Unop.arg,
                                                 e2->Iex.Unop.arg ));

      case Iex_Const: {
         IRConst *c1 = e1->Iex.Const.con;
         IRConst *c2 = e2->Iex.Const.con;
         vassert(c1->tag == c2->tag);
         switch (c1->tag) {
            case Ico_U1:   return toBool( c1->Ico.U1  == c2->Ico.U1 );
            case Ico_U8:   return toBool( c1->Ico.U8  == c2->Ico.U8 );
            case Ico_U16:  return toBool( c1->Ico.U16 == c2->Ico.U16 );
            case Ico_U32:  return toBool( c1->Ico.U32 == c2->Ico.U32 );
            case Ico_U64:  return toBool( c1->Ico.U64 == c2->Ico.U64 );
            default: break;
         }
         return False;
      }

      case Iex_Triop: {
         IRTriop *tri1 = e1->Iex.Triop.details;
         IRTriop *tri2 = e2->Iex.Triop.details;
         return toBool( tri1->op == tri2->op
                        && sameIRExprs_aux( env, tri1->arg1, tri2->arg1 )
                        && sameIRExprs_aux( env, tri1->arg2, tri2->arg2 )
                        && sameIRExprs_aux( env, tri1->arg3, tri2->arg3 ));
      }

      case Iex_ITE:
         return toBool(    sameIRExprs_aux( env, e1->Iex.ITE.cond,
                                                 e2->Iex.ITE.cond )
                        && sameIRExprs_aux( env, e1->Iex.ITE.iftrue,
                                                 e2->Iex.ITE.iftrue )
                        && sameIRExprs_aux( env, e1->Iex.ITE.iffalse,
                                                 e2->Iex.ITE.iffalse ));

      default:
         /* Not very likely to be "same". */
         break;
   }

   return False;
}

inline
static Bool sameIRExprs ( IRExpr** env, IRExpr* e1, IRExpr* e2 )
{
   Bool same;

   num_nodes_visited = 0;
   same = sameIRExprs_aux(env, e1, e2);

#if STATS_IROPT
   ++invocation_count;
   if (recursed) ++recursion_count;
   success_count += same;
   if (same && recursion_helped)
      ++recursion_success_count;
   if (num_nodes_visited > max_nodes_visited)
      max_nodes_visited = num_nodes_visited;
   recursed = False; /* reset */
   recursion_helped = False;  /* reset */
#endif /* STATS_IROPT */

   return same;
}


/* Debugging-only hack (not used in production runs): make a guess
   whether sameIRExprs might assert due to the two args being of
   different types.  If in doubt return False.  Is only used when
   --vex-iropt-level > 0, that is, vex_control.iropt_verbosity > 0.
   Bad because it duplicates functionality from typeOfIRExpr.  See
   comment on the single use point below for rationale. */
static
Bool debug_only_hack_sameIRExprs_might_assert ( IRExpr* e1, IRExpr* e2 )
{
   if (e1->tag != e2->tag) return False;
   switch (e1->tag) {
      case Iex_Const: {
         /* The only interesting case */
         IRConst *c1 = e1->Iex.Const.con;
         IRConst *c2 = e2->Iex.Const.con;
         return c1->tag != c2->tag;
      }
      default:
         break;
   }
   return False;
}


/* Is this literally IRExpr_Const(IRConst_U32(0)) ? */
static Bool isZeroU32 ( IRExpr* e )
{
   return toBool( e->tag == Iex_Const 
                  && e->Iex.Const.con->tag == Ico_U32
                  && e->Iex.Const.con->Ico.U32 == 0);
}

/* Is this literally IRExpr_Const(IRConst_U64(0)) ?
   Currently unused; commented out to avoid compiler warning */
#if 0
static Bool isZeroU64 ( IRExpr* e )
{
   return toBool( e->tag == Iex_Const 
                  && e->Iex.Const.con->tag == Ico_U64
                  && e->Iex.Const.con->Ico.U64 == 0);
}
#endif

/* Is this literally IRExpr_Const(IRConst_V128(0)) ? */
static Bool isZeroV128 ( IRExpr* e )
{
   return toBool( e->tag == Iex_Const 
                  && e->Iex.Const.con->tag == Ico_V128
                  && e->Iex.Const.con->Ico.V128 == 0x0000);
}

/* Is this literally IRExpr_Const(IRConst_V256(0)) ? */
static Bool isZeroV256 ( IRExpr* e )
{
   return toBool( e->tag == Iex_Const 
                  && e->Iex.Const.con->tag == Ico_V256
                  && e->Iex.Const.con->Ico.V256 == 0x00000000);
}

/* Is this an integer constant with value 0 ? */
static Bool isZeroU ( IRExpr* e )
{
   if (e->tag != Iex_Const) return False;
   switch (e->Iex.Const.con->tag) {
      case Ico_U1:    return toBool( e->Iex.Const.con->Ico.U1  == 0);
      case Ico_U8:    return toBool( e->Iex.Const.con->Ico.U8  == 0);
      case Ico_U16:   return toBool( e->Iex.Const.con->Ico.U16 == 0);
      case Ico_U32:   return toBool( e->Iex.Const.con->Ico.U32 == 0);
      case Ico_U64:   return toBool( e->Iex.Const.con->Ico.U64 == 0);
      default: vpanic("isZeroU");
   }
}

/* Is this an integer constant with value 1---1b ? */
static Bool isOnesU ( IRExpr* e )
{
   if (e->tag != Iex_Const) return False;
   switch (e->Iex.Const.con->tag) {
      case Ico_U8:    return toBool( e->Iex.Const.con->Ico.U8  == 0xFF);
      case Ico_U16:   return toBool( e->Iex.Const.con->Ico.U16 == 0xFFFF);
      case Ico_U32:   return toBool( e->Iex.Const.con->Ico.U32
                                     == 0xFFFFFFFF);
      case Ico_U64:   return toBool( e->Iex.Const.con->Ico.U64
                                     == 0xFFFFFFFFFFFFFFFFULL);
      default: ppIRExpr(e); vpanic("isOnesU");
   }
}

static Bool notBool ( Bool b )
{
   if (b == True) return False;
   if (b == False) return True;
   vpanic("notBool");
}

/* Make a zero which has the same type as the result of the given
   primop. */
static IRExpr* mkZeroOfPrimopResultType ( IROp op )
{
   switch (op) {
      case Iop_CmpNE32: return IRExpr_Const(IRConst_U1(toBool(0)));
      case Iop_Xor8:  return IRExpr_Const(IRConst_U8(0));
      case Iop_Xor16: return IRExpr_Const(IRConst_U16(0));
      case Iop_Sub32:
      case Iop_Xor32: return IRExpr_Const(IRConst_U32(0));
      case Iop_And64:
      case Iop_Sub64:
      case Iop_Xor64: return IRExpr_Const(IRConst_U64(0));
      case Iop_XorV128:
      case Iop_AndV128: return IRExpr_Const(IRConst_V128(0));
      case Iop_XorV256:
      case Iop_AndV256: return IRExpr_Const(IRConst_V256(0));
      default: vpanic("mkZeroOfPrimopResultType: bad primop");
   }
}

/* Make a value containing all 1-bits, which has the same type as the
   result of the given primop. */
static IRExpr* mkOnesOfPrimopResultType ( IROp op )
{
   switch (op) {
      case Iop_CmpEQ32:
      case Iop_CmpEQ64:
         return IRExpr_Const(IRConst_U1(toBool(1)));
      case Iop_Or8:
         return IRExpr_Const(IRConst_U8(0xFF));
      case Iop_Or16:
         return IRExpr_Const(IRConst_U16(0xFFFF));
      case Iop_Or32:
         return IRExpr_Const(IRConst_U32(0xFFFFFFFF));
      case Iop_CmpEQ8x8:
      case Iop_Or64:
         return IRExpr_Const(IRConst_U64(0xFFFFFFFFFFFFFFFFULL));
      case Iop_CmpEQ8x16:
      case Iop_CmpEQ16x8:
      case Iop_CmpEQ32x4:
         return IRExpr_Const(IRConst_V128(0xFFFF));
      default:
         ppIROp(op);
         vpanic("mkOnesOfPrimopResultType: bad primop");
   }
}

/* Helpers for folding Clz32/64. */
static UInt fold_Clz64 ( ULong value )
{
   UInt i;
   vassert(value != 0ULL); /* no defined semantics for arg==0 */
   for (i = 0; i < 64; ++i) {
      if (0ULL != (value & (((ULong)1) << (63 - i)))) return i;
   }
   vassert(0);
   /*NOTREACHED*/
   return 0;
}

static UInt fold_Clz32 ( UInt value )
{
   UInt i;
   vassert(value != 0); /* no defined semantics for arg==0 */
   for (i = 0; i < 32; ++i) {
      if (0 != (value & (((UInt)1) << (31 - i)))) return i;
   }
   vassert(0);
   /*NOTREACHED*/
   return 0;
}

/* V64 holds 8 summary-constant bits in V128/V256 style.  Convert to
   the corresponding real constant. */
//XXX re-check this before use
//static ULong de_summarise_V64 ( UChar v64 )
//{
//   ULong r = 0;
//   if (v64 & (1<<0)) r |= 0x00000000000000FFULL;
//   if (v64 & (1<<1)) r |= 0x000000000000FF00ULL;
//   if (v64 & (1<<2)) r |= 0x0000000000FF0000ULL;
//   if (v64 & (1<<3)) r |= 0x00000000FF000000ULL;
//   if (v64 & (1<<4)) r |= 0x000000FF00000000ULL;
//   if (v64 & (1<<5)) r |= 0x0000FF0000000000ULL;
//   if (v64 & (1<<6)) r |= 0x00FF000000000000ULL;
//   if (v64 & (1<<7)) r |= 0xFF00000000000000ULL;
//   return r;
//}

/* Helper for arbitrary expression pattern matching in flat IR.  If
   'e' is a reference to a tmp, look it up in env -- repeatedly, if
   necessary -- until it resolves to a non-tmp.  Note that this can
   return NULL if it can't resolve 'e' to a new expression, which will
   be the case if 'e' is instead defined by an IRStmt (IRDirty or
   LLSC). */
static IRExpr* chase ( IRExpr** env, IRExpr* e )
{
   /* Why is this loop guaranteed to terminate?  Because all tmps must
      have definitions before use, hence a tmp cannot be bound
      (directly or indirectly) to itself. */
   while (e->tag == Iex_RdTmp) {
      if (0) { vex_printf("chase "); ppIRExpr(e); vex_printf("\n"); }
      e = env[(Int)e->Iex.RdTmp.tmp];
      if (e == NULL) break;
   }
   return e;
}

/* Similar to |chase|, but follows at most one level of tmp reference. */
static IRExpr* chase1 ( IRExpr** env, IRExpr* e )
{
   if (e == NULL || e->tag != Iex_RdTmp)
      return e;
   else
      return env[(Int)e->Iex.RdTmp.tmp];
}

static IRExpr* fold_Expr ( IRExpr** env, IRExpr* e )
{
   Int     shift;
   IRExpr* e2 = e; /* e2 is the result of folding e, if possible */

   switch (e->tag) {
   case Iex_Unop:
      /* UNARY ops */
      if (e->Iex.Unop.arg->tag == Iex_Const) {
         switch (e->Iex.Unop.op) {
         case Iop_1Uto8:
            e2 = IRExpr_Const(IRConst_U8(toUChar(
                    e->Iex.Unop.arg->Iex.Const.con->Ico.U1
                    ? 1 : 0)));
            break;
         case Iop_1Uto32:
            e2 = IRExpr_Const(IRConst_U32(
                    e->Iex.Unop.arg->Iex.Const.con->Ico.U1
                    ? 1 : 0));
            break;
         case Iop_1Uto64:
            e2 = IRExpr_Const(IRConst_U64(
                    e->Iex.Unop.arg->Iex.Const.con->Ico.U1
                    ? 1 : 0));
            break;

         case Iop_1Sto8:
            e2 = IRExpr_Const(IRConst_U8(toUChar(
                    e->Iex.Unop.arg->Iex.Const.con->Ico.U1
                    ? 0xFF : 0)));
            break;
         case Iop_1Sto16:
            e2 = IRExpr_Const(IRConst_U16(toUShort(
                    e->Iex.Unop.arg->Iex.Const.con->Ico.U1
                    ? 0xFFFF : 0)));
            break;
         case Iop_1Sto32:
            e2 = IRExpr_Const(IRConst_U32(
                    e->Iex.Unop.arg->Iex.Const.con->Ico.U1
                    ? 0xFFFFFFFF : 0));
            break;
         case Iop_1Sto64:
            e2 = IRExpr_Const(IRConst_U64(
                    e->Iex.Unop.arg->Iex.Const.con->Ico.U1
                    ? 0xFFFFFFFFFFFFFFFFULL : 0));
            break;

         case Iop_8Sto32: {
            UInt u32 = e->Iex.Unop.arg->Iex.Const.con->Ico.U8;
            u32 <<= 24;
            u32 = (Int)u32 >> 24;   /* signed shift */
            e2 = IRExpr_Const(IRConst_U32(u32));
            break;
         }
         case Iop_16Sto32: {
            UInt u32 = e->Iex.Unop.arg->Iex.Const.con->Ico.U16;
            u32 <<= 16;
            u32 = (Int)u32 >> 16;   /* signed shift */
            e2 = IRExpr_Const(IRConst_U32(u32));
            break;
         }
         case Iop_8Uto64:
            e2 = IRExpr_Const(IRConst_U64(
                    0xFFULL & e->Iex.Unop.arg->Iex.Const.con->Ico.U8));
            break;
         case Iop_16Uto64:
            e2 = IRExpr_Const(IRConst_U64(
                    0xFFFFULL & e->Iex.Unop.arg->Iex.Const.con->Ico.U16));
            break;
         case Iop_8Uto32:
            e2 = IRExpr_Const(IRConst_U32(
                    0xFF & e->Iex.Unop.arg->Iex.Const.con->Ico.U8));
            break;
         case Iop_8Sto16: {
            UShort u16 = e->Iex.Unop.arg->Iex.Const.con->Ico.U8;
            u16 <<= 8;
            u16 = (Short)u16 >> 8;  /* signed shift */
            e2 = IRExpr_Const(IRConst_U16(u16));
            break;
         }
         case Iop_8Uto16:
            e2 = IRExpr_Const(IRConst_U16(
                    0xFF & e->Iex.Unop.arg->Iex.Const.con->Ico.U8));
            break;
         case Iop_16Uto32:
            e2 = IRExpr_Const(IRConst_U32(
                    0xFFFF & e->Iex.Unop.arg->Iex.Const.con->Ico.U16));
            break;
         case Iop_32to16:
            e2 = IRExpr_Const(IRConst_U16(toUShort(
                    0xFFFF & e->Iex.Unop.arg->Iex.Const.con->Ico.U32)));
            break;
         case Iop_32to8:
            e2 = IRExpr_Const(IRConst_U8(toUChar(
                    0xFF & e->Iex.Unop.arg->Iex.Const.con->Ico.U32)));
            break;
         case Iop_32to1:
            e2 = IRExpr_Const(IRConst_U1(toBool(
                    1 == (1 & e->Iex.Unop.arg->Iex.Const.con->Ico.U32)
                 )));
            break;
         case Iop_64to1:
            e2 = IRExpr_Const(IRConst_U1(toBool(
                    1 == (1 & e->Iex.Unop.arg->Iex.Const.con->Ico.U64)
                 )));
            break;

         case Iop_NotV128:
            e2 = IRExpr_Const(IRConst_V128(
                    ~ (e->Iex.Unop.arg->Iex.Const.con->Ico.V128)));
            break;
         case Iop_Not64:
            e2 = IRExpr_Const(IRConst_U64(
                    ~ (e->Iex.Unop.arg->Iex.Const.con->Ico.U64)));
            break;
         case Iop_Not32:
            e2 = IRExpr_Const(IRConst_U32(
                    ~ (e->Iex.Unop.arg->Iex.Const.con->Ico.U32)));
            break;
         case Iop_Not16:
            e2 = IRExpr_Const(IRConst_U16(toUShort(
                    ~ (e->Iex.Unop.arg->Iex.Const.con->Ico.U16))));
            break;
         case Iop_Not8:
            e2 = IRExpr_Const(IRConst_U8(toUChar(
                    ~ (e->Iex.Unop.arg->Iex.Const.con->Ico.U8))));
            break;

         case Iop_Not1:
            e2 = IRExpr_Const(IRConst_U1(
                    notBool(e->Iex.Unop.arg->Iex.Const.con->Ico.U1)));
            break;

         case Iop_64to8: {
            ULong w64 = e->Iex.Unop.arg->Iex.Const.con->Ico.U64;
            w64 &= 0xFFULL;
            e2 = IRExpr_Const(IRConst_U8( (UChar)w64 ));
            break;
         }
         case Iop_64to16: {
            ULong w64 = e->Iex.Unop.arg->Iex.Const.con->Ico.U64;
            w64 &= 0xFFFFULL;
            e2 = IRExpr_Const(IRConst_U16( (UShort)w64 ));
            break;
         }
         case Iop_64to32: {
            ULong w64 = e->Iex.Unop.arg->Iex.Const.con->Ico.U64;
            w64 &= 0x00000000FFFFFFFFULL;
            e2 = IRExpr_Const(IRConst_U32( (UInt)w64 ));
            break;
         }
         case Iop_64HIto32: {
            ULong w64 = e->Iex.Unop.arg->Iex.Const.con->Ico.U64;
            w64 >>= 32;
            e2 = IRExpr_Const(IRConst_U32( (UInt)w64 ));
            break;
         }
         case Iop_32Uto64:
            e2 = IRExpr_Const(IRConst_U64(
                    0xFFFFFFFFULL 
                    & e->Iex.Unop.arg->Iex.Const.con->Ico.U32));
            break;
         case Iop_16Sto64: {
            ULong u64 = e->Iex.Unop.arg->Iex.Const.con->Ico.U16;
            u64 <<= 48;
            u64 = (Long)u64 >> 48;   /* signed shift */
            e2 = IRExpr_Const(IRConst_U64(u64));
            break;
         }
         case Iop_32Sto64: {
            ULong u64 = e->Iex.Unop.arg->Iex.Const.con->Ico.U32;
            u64 <<= 32;
            u64 = (Long)u64 >> 32;   /* signed shift */
            e2 = IRExpr_Const(IRConst_U64(u64));
            break;
         }

         case Iop_16to8: {
            UShort w16 = e->Iex.Unop.arg->Iex.Const.con->Ico.U16;
            w16 &= 0xFF;
            e2 = IRExpr_Const(IRConst_U8( (UChar)w16 ));
            break;
         }
         case Iop_16HIto8: {
            UShort w16 = e->Iex.Unop.arg->Iex.Const.con->Ico.U16;
            w16 >>= 8;
            w16 &= 0xFF;
            e2 = IRExpr_Const(IRConst_U8( (UChar)w16 ));
            break;
         }

         case Iop_CmpNEZ8:
            e2 = IRExpr_Const(IRConst_U1(toBool(
                    0 != 
                    (0xFF & e->Iex.Unop.arg->Iex.Const.con->Ico.U8)
                 )));
            break;
         case Iop_CmpNEZ32:
            e2 = IRExpr_Const(IRConst_U1(toBool(
                    0 != 
                    (0xFFFFFFFF & e->Iex.Unop.arg->Iex.Const.con->Ico.U32)
                 )));
            break;
         case Iop_CmpNEZ64:
            e2 = IRExpr_Const(IRConst_U1(toBool(
                    0ULL != e->Iex.Unop.arg->Iex.Const.con->Ico.U64
                 )));
            break;

         case Iop_CmpwNEZ32: {
            UInt w32 = e->Iex.Unop.arg->Iex.Const.con->Ico.U32;
            if (w32 == 0)
               e2 = IRExpr_Const(IRConst_U32( 0 ));
            else
               e2 = IRExpr_Const(IRConst_U32( 0xFFFFFFFF ));
            break;
         }
         case Iop_CmpwNEZ64: {
            ULong w64 = e->Iex.Unop.arg->Iex.Const.con->Ico.U64;
            if (w64 == 0)
               e2 = IRExpr_Const(IRConst_U64( 0 ));
            else
               e2 = IRExpr_Const(IRConst_U64( 0xFFFFFFFFFFFFFFFFULL ));
            break;
         }

         case Iop_Left32: {
            UInt u32 = e->Iex.Unop.arg->Iex.Const.con->Ico.U32;
            Int  s32 = (Int)(u32 & 0xFFFFFFFF);
            s32 = (s32 | (-s32));
            e2 = IRExpr_Const( IRConst_U32( (UInt)s32 ));
            break;
         }

         case Iop_Left64: {
            ULong u64 = e->Iex.Unop.arg->Iex.Const.con->Ico.U64;
            Long  s64 = (Long)u64;
            s64 = (s64 | (-s64));
            e2 = IRExpr_Const( IRConst_U64( (ULong)s64 ));
            break;
         }

         case Iop_Clz32: {
            UInt u32 = e->Iex.Unop.arg->Iex.Const.con->Ico.U32;
            if (u32 != 0)
               e2 = IRExpr_Const(IRConst_U32(fold_Clz32(u32)));
            break;
         }
         case Iop_Clz64: {
            ULong u64 = e->Iex.Unop.arg->Iex.Const.con->Ico.U64;
            if (u64 != 0ULL)
               e2 = IRExpr_Const(IRConst_U64(fold_Clz64(u64)));
            break;
         }

         /* For these vector ones, can't fold all cases, but at least
            do the most obvious one.  Could do better here using
            summarise/desummarise of vector constants, but too
            difficult to verify; hence just handle the zero cases. */
         case Iop_32UtoV128: {
            UInt u32 = e->Iex.Unop.arg->Iex.Const.con->Ico.U32;
            if (0 == u32) {
               e2 = IRExpr_Const(IRConst_V128(0x0000));
            } else {
               goto unhandled;
            }
            break;
         }
         case Iop_V128to64: {
            UShort v128 = e->Iex.Unop.arg->Iex.Const.con->Ico.V128;
            if (0 == ((v128 >> 0) & 0xFF)) {
               e2 = IRExpr_Const(IRConst_U64(0));
            } else {
               goto unhandled;
            }
            break;
         }
         case Iop_V128HIto64: {
            UShort v128 = e->Iex.Unop.arg->Iex.Const.con->Ico.V128;
            if (0 == ((v128 >> 8) & 0xFF)) {
               e2 = IRExpr_Const(IRConst_U64(0));
            } else {
               goto unhandled;
            }
            break;
         }
         case Iop_64UtoV128: {
            ULong u64 = e->Iex.Unop.arg->Iex.Const.con->Ico.U64;
            if (0 == u64) {
               e2 = IRExpr_Const(IRConst_V128(0x0000));
            } else {
               goto unhandled;
            }
            break;
         }

         /* Even stupider (although still correct ..) */
         case Iop_V256to64_0: case Iop_V256to64_1:
         case Iop_V256to64_2: case Iop_V256to64_3: {
            UInt v256 = e->Iex.Unop.arg->Iex.Const.con->Ico.V256;
            if (v256 == 0x00000000) {
               e2 = IRExpr_Const(IRConst_U64(0));
            } else {
               goto unhandled;
            }
            break;
         }

         case Iop_ZeroHI64ofV128: {
            /* Could do better here -- only need to look at the bottom 64 bits
               of the argument, really. */
            UShort v128 = e->Iex.Unop.arg->Iex.Const.con->Ico.V128;
            if (v128 == 0x0000) {
               e2 = IRExpr_Const(IRConst_V128(0x0000));
            } else {
               goto unhandled;
            }
            break;
         }

         default: 
            goto unhandled;
      }
      }
      break;

   case Iex_Binop:
      /* BINARY ops */
      if (e->Iex.Binop.arg1->tag == Iex_Const
          && e->Iex.Binop.arg2->tag == Iex_Const) {
         /* cases where both args are consts */
         switch (e->Iex.Binop.op) {

            /* -- Or -- */
            case Iop_Or8:
               e2 = IRExpr_Const(IRConst_U8(toUChar( 
                       (e->Iex.Binop.arg1->Iex.Const.con->Ico.U8
                        | e->Iex.Binop.arg2->Iex.Const.con->Ico.U8))));
               break;
            case Iop_Or16:
               e2 = IRExpr_Const(IRConst_U16(toUShort(
                       (e->Iex.Binop.arg1->Iex.Const.con->Ico.U16
                        | e->Iex.Binop.arg2->Iex.Const.con->Ico.U16))));
               break;
            case Iop_Or32:
               e2 = IRExpr_Const(IRConst_U32(
                       (e->Iex.Binop.arg1->Iex.Const.con->Ico.U32
                        | e->Iex.Binop.arg2->Iex.Const.con->Ico.U32)));
               break;
            case Iop_Or64:
               e2 = IRExpr_Const(IRConst_U64(
                       (e->Iex.Binop.arg1->Iex.Const.con->Ico.U64
                        | e->Iex.Binop.arg2->Iex.Const.con->Ico.U64)));
               break;
            case Iop_OrV128:
               e2 = IRExpr_Const(IRConst_V128(
                       (e->Iex.Binop.arg1->Iex.Const.con->Ico.V128
                        | e->Iex.Binop.arg2->Iex.Const.con->Ico.V128)));
               break;

            /* -- Xor -- */
            case Iop_Xor8:
               e2 = IRExpr_Const(IRConst_U8(toUChar( 
                       (e->Iex.Binop.arg1->Iex.Const.con->Ico.U8
                        ^ e->Iex.Binop.arg2->Iex.Const.con->Ico.U8))));
               break;
            case Iop_Xor16:
               e2 = IRExpr_Const(IRConst_U16(toUShort(
                       (e->Iex.Binop.arg1->Iex.Const.con->Ico.U16
                        ^ e->Iex.Binop.arg2->Iex.Const.con->Ico.U16))));
               break;
            case Iop_Xor32:
               e2 = IRExpr_Const(IRConst_U32(
                       (e->Iex.Binop.arg1->Iex.Const.con->Ico.U32
                        ^ e->Iex.Binop.arg2->Iex.Const.con->Ico.U32)));
               break;
            case Iop_Xor64:
               e2 = IRExpr_Const(IRConst_U64(
                       (e->Iex.Binop.arg1->Iex.Const.con->Ico.U64
                        ^ e->Iex.Binop.arg2->Iex.Const.con->Ico.U64)));
               break;
            case Iop_XorV128:
               e2 = IRExpr_Const(IRConst_V128(
                       (e->Iex.Binop.arg1->Iex.Const.con->Ico.V128
                        ^ e->Iex.Binop.arg2->Iex.Const.con->Ico.V128)));
               break;

            /* -- And -- */
            case Iop_And8:
               e2 = IRExpr_Const(IRConst_U8(toUChar( 
                       (e->Iex.Binop.arg1->Iex.Const.con->Ico.U8
                        & e->Iex.Binop.arg2->Iex.Const.con->Ico.U8))));
               break;
            case Iop_And16:
               e2 = IRExpr_Const(IRConst_U16(toUShort(
                       (e->Iex.Binop.arg1->Iex.Const.con->Ico.U16
                        & e->Iex.Binop.arg2->Iex.Const.con->Ico.U16))));
               break;
            case Iop_And32:
               e2 = IRExpr_Const(IRConst_U32(
                       (e->Iex.Binop.arg1->Iex.Const.con->Ico.U32
                        & e->Iex.Binop.arg2->Iex.Const.con->Ico.U32)));
               break;
            case Iop_And64:
               e2 = IRExpr_Const(IRConst_U64(
                       (e->Iex.Binop.arg1->Iex.Const.con->Ico.U64
                        & e->Iex.Binop.arg2->Iex.Const.con->Ico.U64)));
               break;
            case Iop_AndV128:
               e2 = IRExpr_Const(IRConst_V128(
                       (e->Iex.Binop.arg1->Iex.Const.con->Ico.V128
                        & e->Iex.Binop.arg2->Iex.Const.con->Ico.V128)));
               break;

            /* -- Add -- */
            case Iop_Add8:
               e2 = IRExpr_Const(IRConst_U8(toUChar( 
                       (e->Iex.Binop.arg1->Iex.Const.con->Ico.U8
                        + e->Iex.Binop.arg2->Iex.Const.con->Ico.U8))));
               break;
            case Iop_Add32:
               e2 = IRExpr_Const(IRConst_U32(
                       (e->Iex.Binop.arg1->Iex.Const.con->Ico.U32
                        + e->Iex.Binop.arg2->Iex.Const.con->Ico.U32)));
               break;
            case Iop_Add64:
               e2 = IRExpr_Const(IRConst_U64(
                       (e->Iex.Binop.arg1->Iex.Const.con->Ico.U64
                        + e->Iex.Binop.arg2->Iex.Const.con->Ico.U64)));
               break;

            /* -- Sub -- */
            case Iop_Sub8:
               e2 = IRExpr_Const(IRConst_U8(toUChar( 
                       (e->Iex.Binop.arg1->Iex.Const.con->Ico.U8
                        - e->Iex.Binop.arg2->Iex.Const.con->Ico.U8))));
               break;
            case Iop_Sub32:
               e2 = IRExpr_Const(IRConst_U32(
                       (e->Iex.Binop.arg1->Iex.Const.con->Ico.U32
                        - e->Iex.Binop.arg2->Iex.Const.con->Ico.U32)));
               break;
            case Iop_Sub64:
               e2 = IRExpr_Const(IRConst_U64(
                       (e->Iex.Binop.arg1->Iex.Const.con->Ico.U64
                        - e->Iex.Binop.arg2->Iex.Const.con->Ico.U64)));
               break;

            /* -- Max32U -- */
            case Iop_Max32U: {
               UInt u32a = e->Iex.Binop.arg1->Iex.Const.con->Ico.U32;
               UInt u32b = e->Iex.Binop.arg2->Iex.Const.con->Ico.U32;
               UInt res  = u32a > u32b ? u32a : u32b;
               e2 = IRExpr_Const(IRConst_U32(res));
               break;
            }

            /* -- Mul -- */
            case Iop_Mul32:
               e2 = IRExpr_Const(IRConst_U32(
                       (e->Iex.Binop.arg1->Iex.Const.con->Ico.U32
                        * e->Iex.Binop.arg2->Iex.Const.con->Ico.U32)));
               break;
            case Iop_Mul64:
               e2 = IRExpr_Const(IRConst_U64(
                       (e->Iex.Binop.arg1->Iex.Const.con->Ico.U64
                        * e->Iex.Binop.arg2->Iex.Const.con->Ico.U64)));
               break;

            case Iop_MullS32: {
               /* very paranoid */
               UInt  u32a = e->Iex.Binop.arg1->Iex.Const.con->Ico.U32;
               UInt  u32b = e->Iex.Binop.arg2->Iex.Const.con->Ico.U32;
               Int   s32a = (Int)u32a;
               Int   s32b = (Int)u32b;
               Long  s64a = (Long)s32a;
               Long  s64b = (Long)s32b;
               Long  sres = s64a * s64b;
               ULong ures = (ULong)sres;
               e2 = IRExpr_Const(IRConst_U64(ures));
               break;
            }

            /* -- Shl -- */
            case Iop_Shl32:
               vassert(e->Iex.Binop.arg2->Iex.Const.con->tag == Ico_U8);
               shift = (Int)(e->Iex.Binop.arg2->Iex.Const.con->Ico.U8);
               if (shift >= 0 && shift <= 31)
                  e2 = IRExpr_Const(IRConst_U32(
                          (e->Iex.Binop.arg1->Iex.Const.con->Ico.U32
                           << shift)));
               break;
            case Iop_Shl64:
               vassert(e->Iex.Binop.arg2->Iex.Const.con->tag == Ico_U8);
               shift = (Int)(e->Iex.Binop.arg2->Iex.Const.con->Ico.U8);
               if (shift >= 0 && shift <= 63)
                  e2 = IRExpr_Const(IRConst_U64(
                          (e->Iex.Binop.arg1->Iex.Const.con->Ico.U64
                           << shift)));
               break;

            /* -- Sar -- */
            case Iop_Sar32: {
               /* paranoid ... */
               /*signed*/ Int s32;
               vassert(e->Iex.Binop.arg2->Iex.Const.con->tag == Ico_U8);
               s32   = (Int)(e->Iex.Binop.arg1->Iex.Const.con->Ico.U32);
               shift = (Int)(e->Iex.Binop.arg2->Iex.Const.con->Ico.U8);
               if (shift >= 0 && shift <= 31) {
                  s32 >>=/*signed*/ shift;
                  e2 = IRExpr_Const(IRConst_U32((UInt)s32));
               }
               break;
            }
            case Iop_Sar64: {
               /* paranoid ... */
               /*signed*/ Long s64;
               vassert(e->Iex.Binop.arg2->Iex.Const.con->tag == Ico_U8);
               s64   = (Long)(e->Iex.Binop.arg1->Iex.Const.con->Ico.U64);
               shift = (Int)(e->Iex.Binop.arg2->Iex.Const.con->Ico.U8);
               if (shift >= 0 && shift <= 63) {
                  s64 >>=/*signed*/ shift;
                  e2 = IRExpr_Const(IRConst_U64((ULong)s64));
               }
               break;
            }

            /* -- Shr -- */
            case Iop_Shr32: {
               /* paranoid ... */
               /*unsigned*/ UInt u32;
               vassert(e->Iex.Binop.arg2->Iex.Const.con->tag == Ico_U8);
               u32   = (UInt)(e->Iex.Binop.arg1->Iex.Const.con->Ico.U32);
               shift = (Int)(e->Iex.Binop.arg2->Iex.Const.con->Ico.U8);
               if (shift >= 0 && shift <= 31) {
                  u32 >>=/*unsigned*/ shift;
                  e2 = IRExpr_Const(IRConst_U32(u32));
               }
               break;
            }
            case Iop_Shr64: {
               /* paranoid ... */
               /*unsigned*/ ULong u64;
               vassert(e->Iex.Binop.arg2->Iex.Const.con->tag == Ico_U8);
               u64   = (ULong)(e->Iex.Binop.arg1->Iex.Const.con->Ico.U64);
               shift = (Int)(e->Iex.Binop.arg2->Iex.Const.con->Ico.U8);
               if (shift >= 0 && shift <= 63) {
                  u64 >>=/*unsigned*/ shift;
                  e2 = IRExpr_Const(IRConst_U64(u64));
               }
               break;
            }

            /* -- CmpEQ -- */
            case Iop_CmpEQ32:
               e2 = IRExpr_Const(IRConst_U1(toBool(
                       (e->Iex.Binop.arg1->Iex.Const.con->Ico.U32
                        == e->Iex.Binop.arg2->Iex.Const.con->Ico.U32))));
               break;
            case Iop_CmpEQ64:
               e2 = IRExpr_Const(IRConst_U1(toBool(
                       (e->Iex.Binop.arg1->Iex.Const.con->Ico.U64
                        == e->Iex.Binop.arg2->Iex.Const.con->Ico.U64))));
               break;

            /* -- CmpNE -- */
            case Iop_CmpNE8:
            case Iop_CasCmpNE8:
            case Iop_ExpCmpNE8:
               e2 = IRExpr_Const(IRConst_U1(toBool(
                       ((0xFF & e->Iex.Binop.arg1->Iex.Const.con->Ico.U8)
                        != (0xFF & e->Iex.Binop.arg2->Iex.Const.con->Ico.U8)))));
               break;
            case Iop_CmpNE32:
            case Iop_CasCmpNE32:
            case Iop_ExpCmpNE32:
               e2 = IRExpr_Const(IRConst_U1(toBool(
                       (e->Iex.Binop.arg1->Iex.Const.con->Ico.U32
                        != e->Iex.Binop.arg2->Iex.Const.con->Ico.U32))));
               break;
            case Iop_CmpNE64:
            case Iop_CasCmpNE64:
            case Iop_ExpCmpNE64:
               e2 = IRExpr_Const(IRConst_U1(toBool(
                       (e->Iex.Binop.arg1->Iex.Const.con->Ico.U64
                        != e->Iex.Binop.arg2->Iex.Const.con->Ico.U64))));
               break;

            /* -- CmpLEU -- */
            case Iop_CmpLE32U:
               e2 = IRExpr_Const(IRConst_U1(toBool(
                       ((UInt)(e->Iex.Binop.arg1->Iex.Const.con->Ico.U32)
                        <= (UInt)(e->Iex.Binop.arg2->Iex.Const.con->Ico.U32)))));
               break;
            case Iop_CmpLE64U:
               e2 = IRExpr_Const(IRConst_U1(toBool(
                       ((ULong)(e->Iex.Binop.arg1->Iex.Const.con->Ico.U64)
                        <= (ULong)(e->Iex.Binop.arg2->Iex.Const.con->Ico.U64)))));
               break;

            /* -- CmpLES -- */
            case Iop_CmpLE32S:
               e2 = IRExpr_Const(IRConst_U1(toBool(
                       ((Int)(e->Iex.Binop.arg1->Iex.Const.con->Ico.U32)
                        <= (Int)(e->Iex.Binop.arg2->Iex.Const.con->Ico.U32)))));
               break;
            case Iop_CmpLE64S:
               e2 = IRExpr_Const(IRConst_U1(toBool(
                       ((Long)(e->Iex.Binop.arg1->Iex.Const.con->Ico.U64)
                        <= (Long)(e->Iex.Binop.arg2->Iex.Const.con->Ico.U64)))));
               break;

            /* -- CmpLTS -- */
            case Iop_CmpLT32S:
               e2 = IRExpr_Const(IRConst_U1(toBool(
                       ((Int)(e->Iex.Binop.arg1->Iex.Const.con->Ico.U32)
                        < (Int)(e->Iex.Binop.arg2->Iex.Const.con->Ico.U32)))));
               break;
            case Iop_CmpLT64S:
               e2 = IRExpr_Const(IRConst_U1(toBool(
                       ((Long)(e->Iex.Binop.arg1->Iex.Const.con->Ico.U64)
                        < (Long)(e->Iex.Binop.arg2->Iex.Const.con->Ico.U64)))));
               break;

            /* -- CmpLTU -- */
            case Iop_CmpLT32U:
               e2 = IRExpr_Const(IRConst_U1(toBool(
                       ((UInt)(e->Iex.Binop.arg1->Iex.Const.con->Ico.U32)
                        < (UInt)(e->Iex.Binop.arg2->Iex.Const.con->Ico.U32)))));
               break;
            case Iop_CmpLT64U:
               e2 = IRExpr_Const(IRConst_U1(toBool(
                       ((ULong)(e->Iex.Binop.arg1->Iex.Const.con->Ico.U64)
                        < (ULong)(e->Iex.Binop.arg2->Iex.Const.con->Ico.U64)))));
               break;

            /* -- CmpORD -- */
            case Iop_CmpORD32S: {
               /* very paranoid */
               UInt  u32a = e->Iex.Binop.arg1->Iex.Const.con->Ico.U32;
               UInt  u32b = e->Iex.Binop.arg2->Iex.Const.con->Ico.U32;
               Int   s32a = (Int)u32a;
               Int   s32b = (Int)u32b;
               Int   r = 0x2; /* EQ */
               if (s32a < s32b) {
                  r = 0x8; /* LT */
               } 
               else if (s32a > s32b) {
                  r = 0x4; /* GT */
               }
               e2 = IRExpr_Const(IRConst_U32(r));
               break;
            }

            /* -- nHLto2n -- */
            case Iop_32HLto64:
               e2 = IRExpr_Const(IRConst_U64(
                       (((ULong)(e->Iex.Binop.arg1
                                  ->Iex.Const.con->Ico.U32)) << 32)
                       | ((ULong)(e->Iex.Binop.arg2->Iex.Const.con->Ico.U32)) 
                    ));
               break;
            case Iop_64HLto128:
               /* We can't fold this, because there is no way to
                  express he result in IR, but at least pretend to
                  handle it, so as to stop getting blasted with
                  no-rule-for-this-primop messages. */
               break;
            /* For this vector one, can't fold all cases, but at
               least do the most obvious one.  Could do better here
               using summarise/desummarise of vector constants, but
               too difficult to verify; hence just handle the zero
               cases. */
            case Iop_64HLtoV128: {
               ULong argHi = e->Iex.Binop.arg1->Iex.Const.con->Ico.U64;
               ULong argLo = e->Iex.Binop.arg2->Iex.Const.con->Ico.U64;
               if (0 == argHi && 0 == argLo) {
                  e2 = IRExpr_Const(IRConst_V128(0));
               } else {
                  goto unhandled;
               }
               break;
            }
            /* Same reasoning for the 256-bit version. */
            case Iop_V128HLtoV256: {
               IRExpr* argHi = e->Iex.Binop.arg1;
               IRExpr* argLo = e->Iex.Binop.arg2;
               if (isZeroV128(argHi) && isZeroV128(argLo)) {
                  e2 = IRExpr_Const(IRConst_V256(0));
               } else {
                  goto unhandled;
               }
               break;
            }

            /* -- V128 stuff -- */
            case Iop_InterleaveLO8x16: {
               /* This turns up a lot in Memcheck instrumentation of
                  Icc generated code.  I don't know why. */
               UShort arg1 =  e->Iex.Binop.arg1->Iex.Const.con->Ico.V128;
               UShort arg2 =  e->Iex.Binop.arg2->Iex.Const.con->Ico.V128;
               if (0 == arg1 && 0 == arg2) {
                  e2 = IRExpr_Const(IRConst_V128(0));
               } else {
                  goto unhandled;
               }
               break;
            }

            default:
               goto unhandled;
         }

      } else {

         /* other cases (identities, etc) */
         switch (e->Iex.Binop.op) {

            case Iop_Shl32:
            case Iop_Shl64:
            case Iop_Shr64:
            case Iop_Sar64:
               /* Shl32/Shl64/Shr64/Sar64(x,0) ==> x */
               if (isZeroU(e->Iex.Binop.arg2)) {
                  e2 = e->Iex.Binop.arg1;
                  break;
               }
               /* Shl32/Shl64/Shr64(0,x) ==> 0 */
               if (isZeroU(e->Iex.Binop.arg1)) {
                  e2 = e->Iex.Binop.arg1;
                  break;
               }
               break;

            case Iop_Sar32:
            case Iop_Shr32:
               /* Shr32/Sar32(x,0) ==> x */
               if (isZeroU(e->Iex.Binop.arg2)) {
                  e2 = e->Iex.Binop.arg1;
                  break;
               }
               break;

            case Iop_Or8:
            case Iop_Or16:
            case Iop_Or32:
            case Iop_Or64:
            case Iop_Max32U:
               /* Or8/Or16/Or32/Or64/Max32U(x,0) ==> x */
               if (isZeroU(e->Iex.Binop.arg2)) {
                  e2 = e->Iex.Binop.arg1;
                  break;
               }
               /* Or8/Or16/Or32/Or64/Max32U(0,x) ==> x */
               if (isZeroU(e->Iex.Binop.arg1)) {
                  e2 = e->Iex.Binop.arg2;
                  break;
               }
               /* Or8/Or16/Or32/Or64/Max32U(x,1---1b) ==> 1---1b */
               /* Or8/Or16/Or32/Or64/Max32U(1---1b,x) ==> 1---1b */
               if (isOnesU(e->Iex.Binop.arg1) || isOnesU(e->Iex.Binop.arg2)) {
                  e2 = mkOnesOfPrimopResultType(e->Iex.Binop.op);
                  break;
               }
               /* Or8/Or16/Or32/Or64/Max32U(t,t) ==> t, for some IRTemp t */
               if (sameIRExprs(env, e->Iex.Binop.arg1, e->Iex.Binop.arg2)) {
                  e2 = e->Iex.Binop.arg1;
                  break;
               }
               break;

            case Iop_Add8:
               /* Add8(t,t) ==> t << 1.
                  Memcheck doesn't understand that
                  x+x produces a defined least significant bit, and it seems
                  simplest just to get rid of the problem by rewriting it
                  out, since the opportunity to do so exists. */
               if (sameIRExprs(env, e->Iex.Binop.arg1, e->Iex.Binop.arg2)) {
                  e2 = IRExpr_Binop(Iop_Shl8, e->Iex.Binop.arg1,
                                    IRExpr_Const(IRConst_U8(1)));
                  break;
               }
               break;

               /* NB no Add16(t,t) case yet as no known test case exists */

            case Iop_Add32:
            case Iop_Add64:
               /* Add32/Add64(x,0) ==> x */
               if (isZeroU(e->Iex.Binop.arg2)) {
                  e2 = e->Iex.Binop.arg1;
                  break;
               }
               /* Add32/Add64(0,x) ==> x */
               if (isZeroU(e->Iex.Binop.arg1)) {
                  e2 = e->Iex.Binop.arg2;
                  break;
               }
               /* Add32/Add64(t,t) ==> t << 1. Same rationale as for Add8. */
               if (sameIRExprs(env, e->Iex.Binop.arg1, e->Iex.Binop.arg2)) {
                  e2 = IRExpr_Binop(
                          e->Iex.Binop.op == Iop_Add32 ? Iop_Shl32 : Iop_Shl64,
                          e->Iex.Binop.arg1, IRExpr_Const(IRConst_U8(1)));
                  break;
               }
               break;

            case Iop_Sub32:
            case Iop_Sub64:
               /* Sub32/Sub64(x,0) ==> x */
               if (isZeroU(e->Iex.Binop.arg2)) {
                  e2 = e->Iex.Binop.arg1;
                  break;
               }
               /* Sub32/Sub64(t,t) ==> 0, for some IRTemp t */
               if (sameIRExprs(env, e->Iex.Binop.arg1, e->Iex.Binop.arg2)) {
                  e2 = mkZeroOfPrimopResultType(e->Iex.Binop.op);
                  break;
               }
               break;
            case Iop_Sub8x16:
               /* Sub8x16(x,0) ==> x */
               if (isZeroV128(e->Iex.Binop.arg2)) {
                  e2 = e->Iex.Binop.arg1;
                  break;
               }
               break;

            case Iop_And8:
            case Iop_And16:
            case Iop_And32:
            case Iop_And64:
               /* And8/And16/And32/And64(x,1---1b) ==> x */
               if (isOnesU(e->Iex.Binop.arg2)) {
                  e2 = e->Iex.Binop.arg1;
                  break;
               }
               /* And8/And16/And32/And64(1---1b,x) ==> x */
               if (isOnesU(e->Iex.Binop.arg1)) {
                  e2 = e->Iex.Binop.arg2;
                  break;
               }
               /* And8/And16/And32/And64(x,0) ==> 0 */
               if (isZeroU(e->Iex.Binop.arg2)) {
                  e2 = e->Iex.Binop.arg2;
                  break;
               }
               /* And8/And16/And32/And64(0,x) ==> 0 */
               if (isZeroU(e->Iex.Binop.arg1)) {
                  e2 = e->Iex.Binop.arg1;
                  break;
               }
               /* And8/And16/And32/And64(t,t) ==> t, for some IRTemp t */
               if (sameIRExprs(env, e->Iex.Binop.arg1, e->Iex.Binop.arg2)) {
                  e2 = e->Iex.Binop.arg1;
                  break;
               }
               break;

            case Iop_AndV128:
            case Iop_AndV256:
               /* And8/And16/AndV128/AndV256(t,t) 
                  ==> t, for some IRTemp t */
               if (sameIRExprs(env, e->Iex.Binop.arg1, e->Iex.Binop.arg2)) {
                  e2 = e->Iex.Binop.arg1;
                  break;
               }
               /* Deal with either arg zero.  Could handle other And
                  cases here too. */
               if (e->Iex.Binop.op == Iop_AndV256
                   && (isZeroV256(e->Iex.Binop.arg1)
                       || isZeroV256(e->Iex.Binop.arg2))) {
                  e2 =  mkZeroOfPrimopResultType(e->Iex.Binop.op);
                  break;
               } else if (e->Iex.Binop.op == Iop_AndV128
                          && (isZeroV128(e->Iex.Binop.arg1)
                              || isZeroV128(e->Iex.Binop.arg2))) {
                  e2 =  mkZeroOfPrimopResultType(e->Iex.Binop.op);
                  break;
               }
               break;

            case Iop_OrV128:
            case Iop_OrV256:
               /* V128/V256(t,t) ==> t, for some IRTemp t */
               if (sameIRExprs(env, e->Iex.Binop.arg1, e->Iex.Binop.arg2)) {
                  e2 = e->Iex.Binop.arg1;
                  break;
               }
               /* OrV128(t,0) ==> t */
               if (e->Iex.Binop.op == Iop_OrV128) {
                  if (isZeroV128(e->Iex.Binop.arg2)) {
                     e2 = e->Iex.Binop.arg1;
                     break;
                  }
                  if (isZeroV128(e->Iex.Binop.arg1)) {
                     e2 = e->Iex.Binop.arg2;
                     break;
                  }
               }
               /* OrV256(t,0) ==> t */
               if (e->Iex.Binop.op == Iop_OrV256) {
                  if (isZeroV256(e->Iex.Binop.arg2)) {
                     e2 = e->Iex.Binop.arg1;
                     break;
                  }
                  //Disabled because there's no known test case right now.
                  //if (isZeroV256(e->Iex.Binop.arg1)) {
                  //   e2 = e->Iex.Binop.arg2;
                  //   break;
                  //}
               }
               break;

            case Iop_Xor8:
            case Iop_Xor16:
            case Iop_Xor32:
            case Iop_Xor64:
            case Iop_XorV128:
            case Iop_XorV256:
               /* Xor8/16/32/64/V128(t,t) ==> 0, for some IRTemp t */
               if (sameIRExprs(env, e->Iex.Binop.arg1, e->Iex.Binop.arg2)) {
                  e2 = mkZeroOfPrimopResultType(e->Iex.Binop.op);
                  break;
               }
               /* XorV128(t,0) ==> t */
               if (e->Iex.Binop.op == Iop_XorV128) {
                  if (isZeroV128(e->Iex.Binop.arg2)) {
                     e2 = e->Iex.Binop.arg1;
                     break;
                  }
                  //Disabled because there's no known test case right now.
                  //if (isZeroV128(e->Iex.Binop.arg1)) {
                  //   e2 = e->Iex.Binop.arg2;
                  //   break;
                  //}
               } else {
                  /* Xor8/16/32/64(0,t) ==> t */
                  if (isZeroU(e->Iex.Binop.arg1)) {
                     e2 = e->Iex.Binop.arg2;
                     break;
                  }
                  /* Xor8/16/32/64(t,0) ==> t */
                  if (isZeroU(e->Iex.Binop.arg2)) {
                     e2 = e->Iex.Binop.arg1;
                     break;
                  }
               }
               break;

            case Iop_CmpNE32:
               /* CmpNE32(t,t) ==> 0, for some IRTemp t */
               if (sameIRExprs(env, e->Iex.Binop.arg1, e->Iex.Binop.arg2)) {
                  e2 = mkZeroOfPrimopResultType(e->Iex.Binop.op);
                  break;
               }
               /* CmpNE32(1Uto32(b), 0) ==> b */
               if (isZeroU32(e->Iex.Binop.arg2)) {
                  IRExpr* a1 = chase(env, e->Iex.Binop.arg1);
                  if (a1 && a1->tag == Iex_Unop 
                         && a1->Iex.Unop.op == Iop_1Uto32) {
                     e2 = a1->Iex.Unop.arg;
                     break;
                  }
               }
               break;

            case Iop_CmpEQ32:
            case Iop_CmpEQ64:
            case Iop_CmpEQ8x8:
            case Iop_CmpEQ8x16:
            case Iop_CmpEQ16x8:
            case Iop_CmpEQ32x4:
               if (sameIRExprs(env, e->Iex.Binop.arg1, e->Iex.Binop.arg2)) {
                  e2 = mkOnesOfPrimopResultType(e->Iex.Binop.op);
                  break;
               }
               break;

            default:
               break;
         }
      }
      break;

   case Iex_ITE:
      /* ITE */
      /* is the discriminant is a constant? */
      if (e->Iex.ITE.cond->tag == Iex_Const) {
         /* assured us by the IR type rules */
         vassert(e->Iex.ITE.cond->Iex.Const.con->tag == Ico_U1);
         e2 = e->Iex.ITE.cond->Iex.Const.con->Ico.U1
                 ? e->Iex.ITE.iftrue : e->Iex.ITE.iffalse;
      }
      else
      /* are the arms identical? (pretty weedy test) */
      if (sameIRExprs(env, e->Iex.ITE.iftrue,
                           e->Iex.ITE.iffalse)) {
         e2 = e->Iex.ITE.iffalse;
      }
      break;

   default:
      /* not considered */
      break;
   }

   /* Show cases where we've found but not folded 'op(t,t)'.  Be
      careful not to call sameIRExprs with values of different types,
      though, else it will assert (and so it should!).  We can't
      conveniently call typeOfIRExpr on the two args without a whole
      bunch of extra plumbing to pass in a type env, so just use a
      hacky test to check the arguments are not anything that might
      sameIRExprs to assert.  This is only OK because this kludge is
      only used for debug printing, not for "real" operation.  For
      "real" operation (ie, all other calls to sameIRExprs), it is
      essential that the to args have the same type.
 
      The "right" solution is to plumb the containing block's
      IRTypeEnv through to here and use typeOfIRExpr to be sure.  But
      that's a bunch of extra parameter passing which will just slow
      down the normal case, for no purpose. */
   if (vex_control.iropt_verbosity > 0 
       && e == e2 
       && e->tag == Iex_Binop
       && !debug_only_hack_sameIRExprs_might_assert(e->Iex.Binop.arg1,
                                                    e->Iex.Binop.arg2)
       && sameIRExprs(env, e->Iex.Binop.arg1, e->Iex.Binop.arg2)) {
      vex_printf("vex iropt: fold_Expr: no ident rule for: ");
      ppIRExpr(e);
      vex_printf("\n");
   }

   /* Show the overall results of folding. */
   if (DEBUG_IROPT && e2 != e) {
      vex_printf("FOLD: "); 
      ppIRExpr(e); vex_printf("  ->  ");
      ppIRExpr(e2); vex_printf("\n");
   }

   return e2;

 unhandled:
#  if 0
   vex_printf("\n\n");
   ppIRExpr(e);
   vpanic("fold_Expr: no rule for the above");
#  else
   if (vex_control.iropt_verbosity > 0) {
      vex_printf("vex iropt: fold_Expr: no const rule for: ");
      ppIRExpr(e);
      vex_printf("\n");
   }
   return e2;
#  endif
}


/* Apply the subst to a simple 1-level expression -- guaranteed to be
   1-level due to previous flattening pass. */

static IRExpr* subst_Expr ( IRExpr** env, IRExpr* ex )
{
   switch (ex->tag) {
      case Iex_RdTmp:
         if (env[(Int)ex->Iex.RdTmp.tmp] != NULL) {
            IRExpr *rhs = env[(Int)ex->Iex.RdTmp.tmp];
            if (rhs->tag == Iex_RdTmp)
               return rhs;
            if (rhs->tag == Iex_Const
                && rhs->Iex.Const.con->tag != Ico_F64i)
               return rhs;
         }
         /* not bound in env */
         return ex;

      case Iex_Const:
      case Iex_Get:
         return ex;

      case Iex_GetI:
         vassert(isIRAtom(ex->Iex.GetI.ix));
         return IRExpr_GetI(
            ex->Iex.GetI.descr,
            subst_Expr(env, ex->Iex.GetI.ix),
            ex->Iex.GetI.bias
         );

      case Iex_Qop: {
         IRQop* qop = ex->Iex.Qop.details;
         vassert(isIRAtom(qop->arg1));
         vassert(isIRAtom(qop->arg2));
         vassert(isIRAtom(qop->arg3));
         vassert(isIRAtom(qop->arg4));
         return IRExpr_Qop(
                   qop->op,
                   subst_Expr(env, qop->arg1),
                   subst_Expr(env, qop->arg2),
                   subst_Expr(env, qop->arg3),
                   subst_Expr(env, qop->arg4)
                );
      }

      case Iex_Triop: {
         IRTriop* triop = ex->Iex.Triop.details;
         vassert(isIRAtom(triop->arg1));
         vassert(isIRAtom(triop->arg2));
         vassert(isIRAtom(triop->arg3));
         return IRExpr_Triop(
                   triop->op,
                   subst_Expr(env, triop->arg1),
                   subst_Expr(env, triop->arg2),
                   subst_Expr(env, triop->arg3)
                );
      }

      case Iex_Binop:
         vassert(isIRAtom(ex->Iex.Binop.arg1));
         vassert(isIRAtom(ex->Iex.Binop.arg2));
         return IRExpr_Binop(
                   ex->Iex.Binop.op,
                   subst_Expr(env, ex->Iex.Binop.arg1),
                   subst_Expr(env, ex->Iex.Binop.arg2)
                );

      case Iex_Unop:
         vassert(isIRAtom(ex->Iex.Unop.arg));
         return IRExpr_Unop(
                   ex->Iex.Unop.op,
                   subst_Expr(env, ex->Iex.Unop.arg)
                );

      case Iex_Load:
         vassert(isIRAtom(ex->Iex.Load.addr));
         return IRExpr_Load(
                   ex->Iex.Load.end,
                   ex->Iex.Load.ty,
                   subst_Expr(env, ex->Iex.Load.addr)
                );

      case Iex_CCall: {
         Int      i;
         IRExpr** args2 = shallowCopyIRExprVec(ex->Iex.CCall.args);
         for (i = 0; args2[i]; i++) {
            vassert(isIRAtom(args2[i]));
            args2[i] = subst_Expr(env, args2[i]);
         }
         return IRExpr_CCall(
                   ex->Iex.CCall.cee,
                   ex->Iex.CCall.retty,
                   args2 
                );
      }

      case Iex_ITE:
         vassert(isIRAtom(ex->Iex.ITE.cond));
         vassert(isIRAtom(ex->Iex.ITE.iftrue));
         vassert(isIRAtom(ex->Iex.ITE.iffalse));
         return IRExpr_ITE(
                   subst_Expr(env, ex->Iex.ITE.cond),
                   subst_Expr(env, ex->Iex.ITE.iftrue),
                   subst_Expr(env, ex->Iex.ITE.iffalse)
                );

      default:
         vex_printf("\n\n"); ppIRExpr(ex);
         vpanic("subst_Expr");
      
   }
}


/* Apply the subst to stmt, then fold the result as much as possible.
   Much simplified due to stmt being previously flattened.  As a
   result of this, the stmt may wind up being turned into a no-op.  
*/
static IRStmt* subst_and_fold_Stmt ( IRExpr** env, IRStmt* st )
{
#  if 0
   vex_printf("\nsubst and fold stmt\n");
   ppIRStmt(st);
   vex_printf("\n");
#  endif

   switch (st->tag) {
      case Ist_AbiHint:
         vassert(isIRAtom(st->Ist.AbiHint.base));
         vassert(isIRAtom(st->Ist.AbiHint.nia));
         return IRStmt_AbiHint(
                   fold_Expr(env, subst_Expr(env, st->Ist.AbiHint.base)),
                   st->Ist.AbiHint.len,
                   fold_Expr(env, subst_Expr(env, st->Ist.AbiHint.nia))
                );
      case Ist_Put:
         vassert(isIRAtom(st->Ist.Put.data));
         return IRStmt_Put(
                   st->Ist.Put.offset, 
                   fold_Expr(env, subst_Expr(env, st->Ist.Put.data)) 
                );

      case Ist_PutI: {
         IRPutI *puti, *puti2;
         puti = st->Ist.PutI.details;
         vassert(isIRAtom(puti->ix));
         vassert(isIRAtom(puti->data));
         puti2 = mkIRPutI(puti->descr,
                          fold_Expr(env, subst_Expr(env, puti->ix)),
                          puti->bias,
                          fold_Expr(env, subst_Expr(env, puti->data)));
         return IRStmt_PutI(puti2);
      }

      case Ist_WrTmp:
         /* This is the one place where an expr (st->Ist.WrTmp.data) is
            allowed to be more than just a constant or a tmp. */
         return IRStmt_WrTmp(
                   st->Ist.WrTmp.tmp,
                   fold_Expr(env, subst_Expr(env, st->Ist.WrTmp.data))
                );

      case Ist_Store:
         vassert(isIRAtom(st->Ist.Store.addr));
         vassert(isIRAtom(st->Ist.Store.data));
         return IRStmt_Store(
                   st->Ist.Store.end,
                   fold_Expr(env, subst_Expr(env, st->Ist.Store.addr)),
                   fold_Expr(env, subst_Expr(env, st->Ist.Store.data))
                );

      case Ist_StoreG: {
         IRStoreG* sg = st->Ist.StoreG.details;
         vassert(isIRAtom(sg->addr));
         vassert(isIRAtom(sg->data));
         vassert(isIRAtom(sg->guard));
         IRExpr* faddr  = fold_Expr(env, subst_Expr(env, sg->addr));
         IRExpr* fdata  = fold_Expr(env, subst_Expr(env, sg->data));
         IRExpr* fguard = fold_Expr(env, subst_Expr(env, sg->guard));
         if (fguard->tag == Iex_Const) {
            /* The condition on this store has folded down to a constant. */
            vassert(fguard->Iex.Const.con->tag == Ico_U1);
            if (fguard->Iex.Const.con->Ico.U1 == False) {
               return IRStmt_NoOp();
            } else {
               vassert(fguard->Iex.Const.con->Ico.U1 == True);
               return IRStmt_Store(sg->end, faddr, fdata);
            }
         }
         return IRStmt_StoreG(sg->end, faddr, fdata, fguard);
      }

      case Ist_LoadG: {
         /* This is complicated.  If the guard folds down to 'false',
            we can replace it with an assignment 'dst := alt', but if
            the guard folds down to 'true', we can't conveniently
            replace it with an unconditional load, because doing so
            requires generating a new temporary, and that is not easy
            to do at this point. */
         IRLoadG* lg = st->Ist.LoadG.details;
         vassert(isIRAtom(lg->addr));
         vassert(isIRAtom(lg->alt));
         vassert(isIRAtom(lg->guard));
         IRExpr* faddr  = fold_Expr(env, subst_Expr(env, lg->addr));
         IRExpr* falt   = fold_Expr(env, subst_Expr(env, lg->alt));
         IRExpr* fguard = fold_Expr(env, subst_Expr(env, lg->guard));
         if (fguard->tag == Iex_Const) {
            /* The condition on this load has folded down to a constant. */
            vassert(fguard->Iex.Const.con->tag == Ico_U1);
            if (fguard->Iex.Const.con->Ico.U1 == False) {
               /* The load is not going to happen -- instead 'alt' is
                  assigned to 'dst'.  */
               return IRStmt_WrTmp(lg->dst, falt);
            } else {
               vassert(fguard->Iex.Const.con->Ico.U1 == True);
               /* The load is always going to happen.  We want to
                  convert to an unconditional load and assign to 'dst'
                  (IRStmt_WrTmp).  Problem is we need an extra temp to
                  hold the loaded value, but none is available.
                  Instead, reconstitute the conditional load (with
                  folded args, of course) and let the caller of this
                  routine deal with the problem. */
            }
         }
         return IRStmt_LoadG(lg->end, lg->cvt, lg->dst, faddr, falt, fguard);
      }

      case Ist_CAS: {
         IRCAS *cas, *cas2;
         cas = st->Ist.CAS.details;
         vassert(isIRAtom(cas->addr));
         vassert(cas->expdHi == NULL || isIRAtom(cas->expdHi));
         vassert(isIRAtom(cas->expdLo));
         vassert(cas->dataHi == NULL || isIRAtom(cas->dataHi));
         vassert(isIRAtom(cas->dataLo));
         cas2 = mkIRCAS(
                   cas->oldHi, cas->oldLo, cas->end, 
                   fold_Expr(env, subst_Expr(env, cas->addr)),
                   cas->expdHi ? fold_Expr(env, subst_Expr(env, cas->expdHi))
                               : NULL,
                   fold_Expr(env, subst_Expr(env, cas->expdLo)),
                   cas->dataHi ? fold_Expr(env, subst_Expr(env, cas->dataHi))
                               : NULL,
                   fold_Expr(env, subst_Expr(env, cas->dataLo))
                );
         return IRStmt_CAS(cas2);
      }

      case Ist_LLSC:
         vassert(isIRAtom(st->Ist.LLSC.addr));
         if (st->Ist.LLSC.storedata)
            vassert(isIRAtom(st->Ist.LLSC.storedata));
         return IRStmt_LLSC(
                   st->Ist.LLSC.end,
                   st->Ist.LLSC.result,
                   fold_Expr(env, subst_Expr(env, st->Ist.LLSC.addr)),
                   st->Ist.LLSC.storedata
                      ? fold_Expr(env, subst_Expr(env, st->Ist.LLSC.storedata))
                      : NULL
                );

      case Ist_Dirty: {
         Int     i;
         IRDirty *d, *d2;
         d = st->Ist.Dirty.details;
         d2 = emptyIRDirty();
         *d2 = *d;
         d2->args = shallowCopyIRExprVec(d2->args);
         if (d2->mFx != Ifx_None) {
            vassert(isIRAtom(d2->mAddr));
            d2->mAddr = fold_Expr(env, subst_Expr(env, d2->mAddr));
         }
         vassert(isIRAtom(d2->guard));
         d2->guard = fold_Expr(env, subst_Expr(env, d2->guard));
         for (i = 0; d2->args[i]; i++) {
            IRExpr* arg = d2->args[i];
            if (LIKELY(!is_IRExpr_VECRET_or_BBPTR(arg))) {
               vassert(isIRAtom(arg));
               d2->args[i] = fold_Expr(env, subst_Expr(env, arg));
            }
         }
         return IRStmt_Dirty(d2);
      }

      case Ist_IMark:
         return IRStmt_IMark(st->Ist.IMark.addr,
                             st->Ist.IMark.len,
                             st->Ist.IMark.delta);

      case Ist_NoOp:
         return IRStmt_NoOp();

      case Ist_MBE:
         return IRStmt_MBE(st->Ist.MBE.event);

      case Ist_Exit: {
         IRExpr* fcond;
         vassert(isIRAtom(st->Ist.Exit.guard));
         fcond = fold_Expr(env, subst_Expr(env, st->Ist.Exit.guard));
         if (fcond->tag == Iex_Const) {
            /* Interesting.  The condition on this exit has folded down to
               a constant. */
            vassert(fcond->Iex.Const.con->tag == Ico_U1);
            if (fcond->Iex.Const.con->Ico.U1 == False) {
               /* exit is never going to happen, so dump the statement. */
               return IRStmt_NoOp();
            } else {
               vassert(fcond->Iex.Const.con->Ico.U1 == True);
               /* Hmmm.  The exit has become unconditional.  Leave it
                  as it is for now, since we'd have to truncate the BB
                  at this point, which is tricky.  Such truncation is
                  done later by the dead-code elimination pass. */
               /* fall out into the reconstruct-the-exit code. */
               if (vex_control.iropt_verbosity > 0) 
                  /* really a misuse of vex_control.iropt_verbosity */
                  vex_printf("vex iropt: IRStmt_Exit became unconditional\n");
            }
         }
         return IRStmt_Exit(fcond, st->Ist.Exit.jk,
                                   st->Ist.Exit.dst, st->Ist.Exit.offsIP);
      }

   default:
      vex_printf("\n"); ppIRStmt(st);
      vpanic("subst_and_fold_Stmt");
   }
}


IRSB* cprop_BB ( IRSB* in )
{
   Int      i;
   IRSB*    out;
   IRStmt*  st2;
   Int      n_tmps = in->tyenv->types_used;
   IRExpr** env = LibVEX_Alloc_inline(n_tmps * sizeof(IRExpr*));
   /* Keep track of IRStmt_LoadGs that we need to revisit after
      processing all the other statements. */
   const Int N_FIXUPS = 16;
   Int fixups[N_FIXUPS]; /* indices in the stmt array of 'out' */
   Int n_fixups = 0;

   out = emptyIRSB();
   out->tyenv = deepCopyIRTypeEnv( in->tyenv );

   /* Set up the env with which travels forward.  This holds a
      substitution, mapping IRTemps to IRExprs. The environment 
      is to be applied as we move along.  Keys are IRTemps.
      Values are IRExpr*s.
   */
   for (i = 0; i < n_tmps; i++)
      env[i] = NULL;

   /* For each original SSA-form stmt ... */
   for (i = 0; i < in->stmts_used; i++) {

      /* First apply the substitution to the current stmt.  This
         propagates in any constants and tmp-tmp assignments
         accumulated prior to this point.  As part of the subst_Stmt
         call, also then fold any constant expressions resulting. */

      st2 = in->stmts[i];

      /* perhaps st2 is already a no-op? */
      if (st2->tag == Ist_NoOp) continue;

      st2 = subst_and_fold_Stmt( env, st2 );

      /* Deal with some post-folding special cases. */
      switch (st2->tag) {

         /* If the statement has been folded into a no-op, forget
            it. */
         case Ist_NoOp:
            continue;

         /* If the statement assigns to an IRTemp add it to the
            running environment. This is for the benefit of copy
            propagation and to allow sameIRExpr look through
            IRTemps. */
         case Ist_WrTmp: {
            vassert(env[(Int)(st2->Ist.WrTmp.tmp)] == NULL);
            env[(Int)(st2->Ist.WrTmp.tmp)] = st2->Ist.WrTmp.data;

            /* 't1 = t2' -- don't add to BB; will be optimized out */
            if (st2->Ist.WrTmp.data->tag == Iex_RdTmp)
               continue;

            /* 't = const' && 'const != F64i' -- don't add to BB 
               Note, we choose not to propagate const when const is an
               F64i, so that F64i literals can be CSE'd later.  This
               helps x86 floating point code generation. */
            if (st2->Ist.WrTmp.data->tag == Iex_Const
                && st2->Ist.WrTmp.data->Iex.Const.con->tag != Ico_F64i) {
               continue;
            }
            /* else add it to the output, as normal */
            break;
         }

         case Ist_LoadG: {
            IRLoadG* lg    = st2->Ist.LoadG.details;
            IRExpr*  guard = lg->guard;
            if (guard->tag == Iex_Const) {
               /* The guard has folded to a constant, and that
                  constant must be 1:I1, since subst_and_fold_Stmt
                  folds out the case 0:I1 by itself. */
               vassert(guard->Iex.Const.con->tag == Ico_U1);
               vassert(guard->Iex.Const.con->Ico.U1 == True);
               /* Add a NoOp here as a placeholder, and make a note of
                  where it is in the output block.  Afterwards we'll
                  come back here and transform the NoOp and the LoadG
                  into a load-convert pair.  The fixups[] entry
                  refers to the inserted NoOp, and we expect to find
                  the relevant LoadG immediately after it. */
               vassert(n_fixups >= 0 && n_fixups <= N_FIXUPS);
               if (n_fixups < N_FIXUPS) {
                  fixups[n_fixups++] = out->stmts_used;
                  addStmtToIRSB( out, IRStmt_NoOp() );
               }
            }
            /* And always add the LoadG to the output, regardless. */
            break;
         }

      default:
         break;
      }

      /* Not interesting, copy st2 into the output block. */
      addStmtToIRSB( out, st2 );
   }

#  if STATS_IROPT
   vex_printf("sameIRExpr: invoked = %u/%u  equal = %u/%u max_nodes = %u\n",
              invocation_count, recursion_count, success_count,
              recursion_success_count, max_nodes_visited);
#  endif

   out->next     = subst_Expr( env, in->next );
   out->jumpkind = in->jumpkind;
   out->offsIP   = in->offsIP;

   /* Process any leftover unconditional LoadGs that we noticed
      in the main pass. */
   vassert(n_fixups >= 0 && n_fixups <= N_FIXUPS);
   for (i = 0; i < n_fixups; i++) {
      Int ix = fixups[i];
      /* Carefully verify that the LoadG has the expected form. */
      vassert(ix >= 0 && ix+1 < out->stmts_used);
      IRStmt* nop = out->stmts[ix];
      IRStmt* lgu = out->stmts[ix+1];
      vassert(nop->tag == Ist_NoOp);
      vassert(lgu->tag == Ist_LoadG);
      IRLoadG* lg    = lgu->Ist.LoadG.details;
      IRExpr*  guard = lg->guard;
      vassert(guard->Iex.Const.con->tag == Ico_U1);
      vassert(guard->Iex.Const.con->Ico.U1 == True);
      /* Figure out the load and result types, and the implied
         conversion operation. */
      IRType cvtRes = Ity_INVALID, cvtArg = Ity_INVALID;
      typeOfIRLoadGOp(lg->cvt, &cvtRes, &cvtArg);
      IROp cvtOp = Iop_INVALID;
      switch (lg->cvt) {
         case ILGop_IdentV128:
         case ILGop_Ident64:
         case ILGop_Ident32: break;
         case ILGop_8Uto32:  cvtOp = Iop_8Uto32;  break;
         case ILGop_8Sto32:  cvtOp = Iop_8Sto32;  break;
         case ILGop_16Uto32: cvtOp = Iop_16Uto32; break;
         case ILGop_16Sto32: cvtOp = Iop_16Sto32; break;
         default: vpanic("cprop_BB: unhandled ILGOp");
      }
      /* Replace the placeholder NoOp by the required unconditional
         load. */
      IRTemp tLoaded = newIRTemp(out->tyenv, cvtArg);
      out->stmts[ix] 
         = IRStmt_WrTmp(tLoaded,
                        IRExpr_Load(lg->end, cvtArg, lg->addr));
      /* Replace the LoadG by a conversion from the loaded value's
         type to the required result type. */
      out->stmts[ix+1]
         = IRStmt_WrTmp(
              lg->dst, cvtOp == Iop_INVALID
                          ? IRExpr_RdTmp(tLoaded)
                          : IRExpr_Unop(cvtOp, IRExpr_RdTmp(tLoaded)));
   }

   return out;
}


/*---------------------------------------------------------------*/
/*--- Dead code (t = E) removal                               ---*/
/*---------------------------------------------------------------*/

/* As a side effect, also removes all code following an unconditional
   side exit. */

/* The type of the HashHW map is: a map from IRTemp to nothing
   -- really just operating a set or IRTemps.
*/

inline
static void addUses_Temp ( Bool* set, IRTemp tmp )
{
   set[(Int)tmp] = True;
}

static void addUses_Expr ( Bool* set, IRExpr* e )
{
   Int i;
   switch (e->tag) {
      case Iex_GetI:
         addUses_Expr(set, e->Iex.GetI.ix);
         return;
      case Iex_ITE:
         addUses_Expr(set, e->Iex.ITE.cond);
         addUses_Expr(set, e->Iex.ITE.iftrue);
         addUses_Expr(set, e->Iex.ITE.iffalse);
         return;
      case Iex_CCall:
         for (i = 0; e->Iex.CCall.args[i]; i++)
            addUses_Expr(set, e->Iex.CCall.args[i]);
         return;
      case Iex_Load:
         addUses_Expr(set, e->Iex.Load.addr);
         return;
      case Iex_Qop:
         addUses_Expr(set, e->Iex.Qop.details->arg1);
         addUses_Expr(set, e->Iex.Qop.details->arg2);
         addUses_Expr(set, e->Iex.Qop.details->arg3);
         addUses_Expr(set, e->Iex.Qop.details->arg4);
         return;
      case Iex_Triop:
         addUses_Expr(set, e->Iex.Triop.details->arg1);
         addUses_Expr(set, e->Iex.Triop.details->arg2);
         addUses_Expr(set, e->Iex.Triop.details->arg3);
         return;
      case Iex_Binop:
         addUses_Expr(set, e->Iex.Binop.arg1);
         addUses_Expr(set, e->Iex.Binop.arg2);
         return;
      case Iex_Unop:
         addUses_Expr(set, e->Iex.Unop.arg);
         return;
      case Iex_RdTmp:
         addUses_Temp(set, e->Iex.RdTmp.tmp);
         return;
      case Iex_Const:
      case Iex_Get:
         return;
      default:
         vex_printf("\n");
         ppIRExpr(e);
         vpanic("addUses_Expr");
   }
}

static void addUses_Stmt ( Bool* set, IRStmt* st )
{
   Int      i;
   IRDirty* d;
   IRCAS*   cas;
   switch (st->tag) {
      case Ist_AbiHint:
         addUses_Expr(set, st->Ist.AbiHint.base);
         addUses_Expr(set, st->Ist.AbiHint.nia);
         return;
      case Ist_PutI:
         addUses_Expr(set, st->Ist.PutI.details->ix);
         addUses_Expr(set, st->Ist.PutI.details->data);
         return;
      case Ist_WrTmp:
         addUses_Expr(set, st->Ist.WrTmp.data);
         return;
      case Ist_Put:
         addUses_Expr(set, st->Ist.Put.data);
         return;
      case Ist_Store:
         addUses_Expr(set, st->Ist.Store.addr);
         addUses_Expr(set, st->Ist.Store.data);
         return;
      case Ist_StoreG: {
         IRStoreG* sg = st->Ist.StoreG.details;
         addUses_Expr(set, sg->addr);
         addUses_Expr(set, sg->data);
         addUses_Expr(set, sg->guard);
         return;
      }
      case Ist_LoadG: {
         IRLoadG* lg = st->Ist.LoadG.details;
         addUses_Expr(set, lg->addr);
         addUses_Expr(set, lg->alt);
         addUses_Expr(set, lg->guard);
         return;
      }
      case Ist_CAS:
         cas = st->Ist.CAS.details;
         addUses_Expr(set, cas->addr);
         if (cas->expdHi)
            addUses_Expr(set, cas->expdHi);
         addUses_Expr(set, cas->expdLo);
         if (cas->dataHi)
            addUses_Expr(set, cas->dataHi);
         addUses_Expr(set, cas->dataLo);
         return;
      case Ist_LLSC:
         addUses_Expr(set, st->Ist.LLSC.addr);
         if (st->Ist.LLSC.storedata)
            addUses_Expr(set, st->Ist.LLSC.storedata);
         return;
      case Ist_Dirty:
         d = st->Ist.Dirty.details;
         if (d->mFx != Ifx_None)
            addUses_Expr(set, d->mAddr);
         addUses_Expr(set, d->guard);
         for (i = 0; d->args[i] != NULL; i++) {
            IRExpr* arg = d->args[i];
            if (LIKELY(!is_IRExpr_VECRET_or_BBPTR(arg)))
               addUses_Expr(set, arg);
         }
         return;
      case Ist_NoOp:
      case Ist_IMark:
      case Ist_MBE:
         return;
      case Ist_Exit:
         addUses_Expr(set, st->Ist.Exit.guard);
         return;
      default:
         vex_printf("\n");
         ppIRStmt(st);
         vpanic("addUses_Stmt");
   }
}


/* Is this literally IRExpr_Const(IRConst_U1(False)) ? */
static Bool isZeroU1 ( IRExpr* e )
{
   return toBool( e->tag == Iex_Const
                  && e->Iex.Const.con->tag == Ico_U1
                  && e->Iex.Const.con->Ico.U1 == False );
}

/* Is this literally IRExpr_Const(IRConst_U1(True)) ? */
static Bool isOneU1 ( IRExpr* e )
{
   return toBool( e->tag == Iex_Const
                  && e->Iex.Const.con->tag == Ico_U1
                  && e->Iex.Const.con->Ico.U1 == True );
}


/* Note, this destructively modifies the given IRSB. */

/* Scan backwards through statements, carrying a set of IRTemps which
   are known to be used after the current point.  On encountering 't =
   E', delete the binding if it is not used.  Otherwise, add any temp
   uses to the set and keep on moving backwards.

   As an enhancement, the first (backwards) pass searches for IR exits
   with always-taken conditions and notes the location of the earliest
   one in the block.  If any such are found, a second pass copies the
   exit destination and jump kind to the bb-end.  Then, the exit and
   all statements following it are turned into no-ops.
*/

/* notstatic */ void do_deadcode_BB ( IRSB* bb )
{
   Int     i, i_unconditional_exit;
   Int     n_tmps = bb->tyenv->types_used;
   Bool*   set = LibVEX_Alloc_inline(n_tmps * sizeof(Bool));
   IRStmt* st;

   for (i = 0; i < n_tmps; i++)
      set[i] = False;

   /* start off by recording IRTemp uses in the next field. */
   addUses_Expr(set, bb->next);

   /* First pass */

   /* Work backwards through the stmts */
   i_unconditional_exit = -1;
   for (i = bb->stmts_used-1; i >= 0; i--) {
      st = bb->stmts[i];
      if (st->tag == Ist_NoOp)
         continue;
      /* take note of any unconditional exits */
      if (st->tag == Ist_Exit
          && isOneU1(st->Ist.Exit.guard))
         i_unconditional_exit = i;
      if (st->tag == Ist_WrTmp
          && set[(Int)(st->Ist.WrTmp.tmp)] == False) {
          /* it's an IRTemp which never got used.  Delete it. */
         if (DEBUG_IROPT) {
            vex_printf("DEAD: ");
            ppIRStmt(st);
            vex_printf("\n");
         }
         bb->stmts[i] = IRStmt_NoOp();
      }
      else
      if (st->tag == Ist_Dirty
          && st->Ist.Dirty.details->guard
          && isZeroU1(st->Ist.Dirty.details->guard)) {
         /* This is a dirty helper which will never get called.
            Delete it. */
         bb->stmts[i] = IRStmt_NoOp();
       }
       else {
         /* Note any IRTemp uses made by the current statement. */
         addUses_Stmt(set, st);
      }
   }

   /* Optional second pass: if any unconditional exits were found, 
      delete them and all following statements. */

   if (i_unconditional_exit != -1) {
      if (0) vex_printf("ZAPPING ALL FORWARDS from %d\n", 
                        i_unconditional_exit);
      vassert(i_unconditional_exit >= 0 
              && i_unconditional_exit < bb->stmts_used);
      bb->next 
         = IRExpr_Const( bb->stmts[i_unconditional_exit]->Ist.Exit.dst );
      bb->jumpkind
         = bb->stmts[i_unconditional_exit]->Ist.Exit.jk;
      bb->offsIP
         = bb->stmts[i_unconditional_exit]->Ist.Exit.offsIP;
      for (i = i_unconditional_exit; i < bb->stmts_used; i++)
         bb->stmts[i] = IRStmt_NoOp();
   }
}


/*---------------------------------------------------------------*/
/*--- Specialisation of helper function calls, in             ---*/
/*--- collaboration with the front end                        ---*/
/*---------------------------------------------------------------*/

static 
IRSB* spec_helpers_BB(
         IRSB* bb,
         IRExpr* (*specHelper) (const HChar*, IRExpr**, IRStmt**, Int)
      )
{
   Int     i;
   IRStmt* st;
   IRExpr* ex;
   Bool    any = False;

   for (i = bb->stmts_used-1; i >= 0; i--) {
      st = bb->stmts[i];

      if (st->tag != Ist_WrTmp
          || st->Ist.WrTmp.data->tag != Iex_CCall)
         continue;

      ex = (*specHelper)( st->Ist.WrTmp.data->Iex.CCall.cee->name,
                          st->Ist.WrTmp.data->Iex.CCall.args,
                          &bb->stmts[0], i );
      if (!ex)
        /* the front end can't think of a suitable replacement */
        continue;

      /* We got something better.  Install it in the bb. */
      any = True;
      bb->stmts[i]
         = IRStmt_WrTmp(st->Ist.WrTmp.tmp, ex);

      if (0) {
         vex_printf("SPEC: ");
         ppIRExpr(st->Ist.WrTmp.data);
         vex_printf("  -->  ");
         ppIRExpr(ex);
         vex_printf("\n");
      }
   }

   if (any)
      bb = flatten_BB(bb);
   return bb;
}


/*---------------------------------------------------------------*/
/*--- Determination of guest state aliasing relationships     ---*/
/*---------------------------------------------------------------*/

/* These are helper functions for CSE and GetI/PutI transformations.

   Determine, to the extent possible, the relationship between two
   guest state accesses.  The possible outcomes are:

   * Exact alias.  These two accesses denote precisely the same
     piece of the guest state.

   * Definitely no alias.  These two accesses are guaranteed not to
     overlap any part of the guest state.

   * Unknown -- if neither of the above can be established.

   If in doubt, return Unknown.  */

typedef
   enum { ExactAlias, NoAlias, UnknownAlias }
   GSAliasing;


/* Produces the alias relation between an indexed guest
   state access and a non-indexed access. */

static
GSAliasing getAliasingRelation_IC ( IRRegArray* descr1, IRExpr* ix1,
                                    Int offset2, IRType ty2 )
{
   UInt minoff1, maxoff1, minoff2, maxoff2;

   getArrayBounds( descr1, &minoff1, &maxoff1 );
   minoff2 = offset2;
   maxoff2 = minoff2 + sizeofIRType(ty2) - 1;

   if (maxoff1 < minoff2 || maxoff2 < minoff1)
      return NoAlias;

   /* Could probably do better here if required.  For the moment
      however just claim not to know anything more. */
   return UnknownAlias;
}


/* Produces the alias relation between two indexed guest state
   accesses. */

static
GSAliasing getAliasingRelation_II ( 
              IRRegArray* descr1, IRExpr* ix1, Int bias1,
              IRRegArray* descr2, IRExpr* ix2, Int bias2
           )
{
   UInt minoff1, maxoff1, minoff2, maxoff2;
   Int  iters;

   /* First try hard to show they don't alias. */
   getArrayBounds( descr1, &minoff1, &maxoff1 );
   getArrayBounds( descr2, &minoff2, &maxoff2 );
   if (maxoff1 < minoff2 || maxoff2 < minoff1)
      return NoAlias;

   /* So the two arrays at least partially overlap.  To get any
      further we'll have to be sure that the descriptors are
      identical. */
   if (!eqIRRegArray(descr1, descr2))
      return UnknownAlias;

   /* The descriptors are identical.  Now the only difference can be
      in the index expressions.  If they cannot be shown to be
      identical, we have to say we don't know what the aliasing
      relation will be.  Now, since the IR is flattened, the index
      expressions should be atoms -- either consts or tmps.  So that
      makes the comparison simple. */
   vassert(isIRAtom(ix1));
   vassert(isIRAtom(ix2));
   if (!eqIRAtom(ix1,ix2))
      return UnknownAlias;

   /* Ok, the index expressions are identical.  So now the only way
      they can be different is in the bias.  Normalise this
      paranoidly, to reliably establish equality/non-equality. */

   /* So now we know that the GetI and PutI index the same array
      with the same base.  Are the offsets the same, modulo the
      array size?  Do this paranoidly. */
   vassert(descr1->nElems == descr2->nElems);
   vassert(descr1->elemTy == descr2->elemTy);
   vassert(descr1->base   == descr2->base);
   iters = 0;
   while (bias1 < 0 || bias2 < 0) {
      bias1 += descr1->nElems;
      bias2 += descr1->nElems;
      iters++;
      if (iters > 10)
         vpanic("getAliasingRelation: iters");
   }
   vassert(bias1 >= 0 && bias2 >= 0);
   bias1 %= descr1->nElems;
   bias2 %= descr1->nElems;
   vassert(bias1 >= 0 && bias1 < descr1->nElems);
   vassert(bias2 >= 0 && bias2 < descr1->nElems);

   /* Finally, biasP and biasG are normalised into the range 
      0 .. descrP/G->nElems - 1.  And so we can establish
      equality/non-equality. */

   return bias1==bias2 ? ExactAlias : NoAlias;
}


/*---------------------------------------------------------------*/
/*--- Common Subexpression Elimination                        ---*/
/*---------------------------------------------------------------*/

/* Expensive in time and space. */

/* Uses two environments: 
   a IRTemp -> IRTemp mapping 
   a mapping from AvailExpr* to IRTemp 
*/

typedef
   struct {
      enum { TCc, TCt } tag;
      union { IRTemp tmp; IRConst* con; } u;
   }
   TmpOrConst;

static Bool eqTmpOrConst ( TmpOrConst* tc1, TmpOrConst* tc2 )
{
   if (tc1->tag != tc2->tag)
      return False;
   switch (tc1->tag) {
      case TCc:
         return eqIRConst(tc1->u.con, tc2->u.con);
      case TCt:
         return tc1->u.tmp == tc2->u.tmp;
      default:
         vpanic("eqTmpOrConst");
   }
}

static Bool eqIRCallee ( IRCallee* cee1, IRCallee* cee2 )
{
   Bool eq = cee1->addr == cee2->addr;
   if (eq) {
      vassert(cee1->regparms == cee2->regparms);
      vassert(cee1->mcx_mask == cee2->mcx_mask);
      /* Names should be the same too, but we don't bother to
         check. */
   }
   return eq;
}

/* Convert an atomic IRExpr* to a TmpOrConst. */
static void irExpr_to_TmpOrConst ( /*OUT*/TmpOrConst* tc, IRExpr* e )
{
   switch (e->tag) {
      case Iex_RdTmp:
         tc->tag   = TCt;
         tc->u.tmp = e->Iex.RdTmp.tmp;
         break;
      case Iex_Const:
         tc->tag   = TCc;
         tc->u.con = e->Iex.Const.con;
         break;
      default:
         /* Getting here is a serious error.  It means that the
            presented arg isn't an IR atom, as it should be. */
         vpanic("irExpr_to_TmpOrConst");
   }
}

/* Convert a TmpOrConst to an atomic IRExpr*. */
static IRExpr* tmpOrConst_to_IRExpr ( TmpOrConst* tc )
{
   switch (tc->tag) {
      case TCc: return IRExpr_Const(tc->u.con);
      case TCt: return IRExpr_RdTmp(tc->u.tmp);
      default:  vpanic("tmpOrConst_to_IRExpr");
   }
}

/* Convert a NULL terminated IRExpr* vector to an array of
   TmpOrConsts, and a length. */
static void irExprVec_to_TmpOrConsts ( /*OUT*/TmpOrConst** outs,
                                       /*OUT*/Int* nOuts,
                                       IRExpr** ins )
{
   Int i, n;
   /* We have to make two passes, one to count, one to copy. */
   for (n = 0; ins[n]; n++)
      ;
   *outs  = LibVEX_Alloc_inline(n * sizeof(TmpOrConst));
   *nOuts = n;
   /* and now copy .. */
   for (i = 0; i < n; i++) {
      IRExpr*     arg = ins[i];
      TmpOrConst* dst = &(*outs)[i];
      irExpr_to_TmpOrConst(dst, arg);
   }
}

typedef
   struct {
      enum { Ut, Btt, Btc, Bct, Cf64i, Ittt, Itct, Ittc, Itcc, GetIt,
             CCall, Load
      } tag;
      union {
         /* unop(tmp) */
         struct {
            IROp   op;
            IRTemp arg;
         } Ut;
         /* binop(tmp,tmp) */
         struct {
            IROp   op;
            IRTemp arg1;
            IRTemp arg2;
         } Btt;
         /* binop(tmp,const) */
         struct {
            IROp    op;
            IRTemp  arg1;
            IRConst con2;
         } Btc;
         /* binop(const,tmp) */
         struct {
            IROp    op;
            IRConst con1;
            IRTemp  arg2;
         } Bct;
         /* F64i-style const */
         struct {
            ULong f64i;
         } Cf64i;
         /* ITE(tmp,tmp,tmp) */
         struct {
            IRTemp co;
            IRTemp e1;
            IRTemp e0;
         } Ittt;
         /* ITE(tmp,tmp,const) */
         struct {
            IRTemp  co;
            IRTemp  e1;
            IRConst con0;
         } Ittc;
         /* ITE(tmp,const,tmp) */
         struct {
            IRTemp  co;
            IRConst con1;
            IRTemp  e0;
         } Itct;
         /* ITE(tmp,const,const) */
         struct {
            IRTemp  co;
            IRConst con1;
            IRConst con0;
         } Itcc;
         /* GetI(descr,tmp,bias)*/
         struct {
            IRRegArray* descr;
            IRTemp      ix;
            Int         bias;
         } GetIt;
         /* Clean helper call */
         struct {
            IRCallee*   cee;
            TmpOrConst* args;
            Int         nArgs;
            IRType      retty;
         } CCall;
         /* Load(end,ty,addr) */
         struct {
            IREndness  end;
            IRType     ty;
            TmpOrConst addr;
         } Load;
      } u;
   }
   AvailExpr;

static Bool eq_AvailExpr ( AvailExpr* a1, AvailExpr* a2 )
{
   if (LIKELY(a1->tag != a2->tag))
      return False;
   switch (a1->tag) {
      case Ut: 
         return toBool(
                a1->u.Ut.op == a2->u.Ut.op 
                && a1->u.Ut.arg == a2->u.Ut.arg);
      case Btt: 
         return toBool(
                a1->u.Btt.op == a2->u.Btt.op
                && a1->u.Btt.arg1 == a2->u.Btt.arg1
                && a1->u.Btt.arg2 == a2->u.Btt.arg2);
      case Btc: 
         return toBool(
                a1->u.Btc.op == a2->u.Btc.op
                && a1->u.Btc.arg1 == a2->u.Btc.arg1
                && eqIRConst(&a1->u.Btc.con2, &a2->u.Btc.con2));
      case Bct: 
         return toBool(
                a1->u.Bct.op == a2->u.Bct.op
                && a1->u.Bct.arg2 == a2->u.Bct.arg2
                && eqIRConst(&a1->u.Bct.con1, &a2->u.Bct.con1));
      case Cf64i: 
         return toBool(a1->u.Cf64i.f64i == a2->u.Cf64i.f64i);
      case Ittt:
         return toBool(a1->u.Ittt.co == a2->u.Ittt.co
                       && a1->u.Ittt.e1 == a2->u.Ittt.e1
                       && a1->u.Ittt.e0 == a2->u.Ittt.e0);
      case Ittc:
         return toBool(a1->u.Ittc.co == a2->u.Ittc.co
                       && a1->u.Ittc.e1 == a2->u.Ittc.e1
                       && eqIRConst(&a1->u.Ittc.con0, &a2->u.Ittc.con0));
      case Itct:
         return toBool(a1->u.Itct.co == a2->u.Itct.co
                       && eqIRConst(&a1->u.Itct.con1, &a2->u.Itct.con1)
                       && a1->u.Itct.e0 == a2->u.Itct.e0);
      case Itcc:
         return toBool(a1->u.Itcc.co == a2->u.Itcc.co
                       && eqIRConst(&a1->u.Itcc.con1, &a2->u.Itcc.con1)
                       && eqIRConst(&a1->u.Itcc.con0, &a2->u.Itcc.con0));
      case GetIt:
         return toBool(eqIRRegArray(a1->u.GetIt.descr, a2->u.GetIt.descr) 
                       && a1->u.GetIt.ix == a2->u.GetIt.ix
                       && a1->u.GetIt.bias == a2->u.GetIt.bias);
      case CCall: {
         Int  i, n;
         Bool eq = a1->u.CCall.nArgs == a2->u.CCall.nArgs
                   && eqIRCallee(a1->u.CCall.cee, a2->u.CCall.cee);
         if (eq) {
            n = a1->u.CCall.nArgs;
            for (i = 0; i < n; i++) {
               if (!eqTmpOrConst( &a1->u.CCall.args[i],
                                  &a2->u.CCall.args[i] )) {
                  eq = False;
                  break;
               }
            }
         }
         if (eq) vassert(a1->u.CCall.retty == a2->u.CCall.retty);
         return eq;  
      }
      case Load: {
         Bool eq = toBool(a1->u.Load.end == a2->u.Load.end
                          && a1->u.Load.ty == a2->u.Load.ty
                          && eqTmpOrConst(&a1->u.Load.addr, &a2->u.Load.addr));
         return eq;
      }
      default:
         vpanic("eq_AvailExpr");
   }
}

static IRExpr* availExpr_to_IRExpr ( AvailExpr* ae ) 
{
   IRConst *con, *con0, *con1;
   switch (ae->tag) {
      case Ut:
         return IRExpr_Unop( ae->u.Ut.op, IRExpr_RdTmp(ae->u.Ut.arg) );
      case Btt:
         return IRExpr_Binop( ae->u.Btt.op,
                              IRExpr_RdTmp(ae->u.Btt.arg1),
                              IRExpr_RdTmp(ae->u.Btt.arg2) );
      case Btc:
         con = LibVEX_Alloc_inline(sizeof(IRConst));
         *con = ae->u.Btc.con2;
         return IRExpr_Binop( ae->u.Btc.op,
                              IRExpr_RdTmp(ae->u.Btc.arg1), 
                              IRExpr_Const(con) );
      case Bct:
         con = LibVEX_Alloc_inline(sizeof(IRConst));
         *con = ae->u.Bct.con1;
         return IRExpr_Binop( ae->u.Bct.op,
                              IRExpr_Const(con), 
                              IRExpr_RdTmp(ae->u.Bct.arg2) );
      case Cf64i:
         return IRExpr_Const(IRConst_F64i(ae->u.Cf64i.f64i));
      case Ittt:
         return IRExpr_ITE(IRExpr_RdTmp(ae->u.Ittt.co), 
                           IRExpr_RdTmp(ae->u.Ittt.e1), 
                           IRExpr_RdTmp(ae->u.Ittt.e0));
      case Ittc:
         con0 = LibVEX_Alloc_inline(sizeof(IRConst));
         *con0 = ae->u.Ittc.con0;
         return IRExpr_ITE(IRExpr_RdTmp(ae->u.Ittc.co), 
                           IRExpr_RdTmp(ae->u.Ittc.e1),
                           IRExpr_Const(con0));
      case Itct:
         con1 = LibVEX_Alloc_inline(sizeof(IRConst));
         *con1 = ae->u.Itct.con1;
         return IRExpr_ITE(IRExpr_RdTmp(ae->u.Itct.co), 
                           IRExpr_Const(con1),
                           IRExpr_RdTmp(ae->u.Itct.e0));

      case Itcc:
         con0 = LibVEX_Alloc_inline(sizeof(IRConst));
         con1 = LibVEX_Alloc_inline(sizeof(IRConst));
         *con0 = ae->u.Itcc.con0;
         *con1 = ae->u.Itcc.con1;
         return IRExpr_ITE(IRExpr_RdTmp(ae->u.Itcc.co), 
                           IRExpr_Const(con1),
                           IRExpr_Const(con0));
      case GetIt:
         return IRExpr_GetI(ae->u.GetIt.descr,
                            IRExpr_RdTmp(ae->u.GetIt.ix),
                            ae->u.GetIt.bias);
      case CCall: {
         Int i, n = ae->u.CCall.nArgs;
         vassert(n >= 0);
         IRExpr** vec = LibVEX_Alloc_inline((n+1) * sizeof(IRExpr*));
         vec[n] = NULL;
         for (i = 0; i < n; i++) {
            vec[i] = tmpOrConst_to_IRExpr(&ae->u.CCall.args[i]);
         }
         return IRExpr_CCall(ae->u.CCall.cee,
                             ae->u.CCall.retty,
                             vec);
      }
      case Load:
         return IRExpr_Load(ae->u.Load.end, ae->u.Load.ty,
                            tmpOrConst_to_IRExpr(&ae->u.Load.addr));
      default:
         vpanic("availExpr_to_IRExpr");
   }
}

inline
static IRTemp subst_AvailExpr_Temp ( HashHW* env, IRTemp tmp )
{
   HWord res;
   /* env :: IRTemp -> IRTemp */
   if (lookupHHW( env, &res, (HWord)tmp ))
      return (IRTemp)res;
   else
      return tmp;
}

inline
static void subst_AvailExpr_TmpOrConst ( /*MB_MOD*/TmpOrConst* tc,
                                          HashHW* env )
{
   /* env :: IRTemp -> IRTemp */
   if (tc->tag == TCt) {
      tc->u.tmp = subst_AvailExpr_Temp( env, tc->u.tmp );
   }
}

static void subst_AvailExpr ( HashHW* env, AvailExpr* ae )
{
   /* env :: IRTemp -> IRTemp */
   switch (ae->tag) {
      case Ut:
         ae->u.Ut.arg = subst_AvailExpr_Temp( env, ae->u.Ut.arg );
         break;
      case Btt:
         ae->u.Btt.arg1 = subst_AvailExpr_Temp( env, ae->u.Btt.arg1 );
         ae->u.Btt.arg2 = subst_AvailExpr_Temp( env, ae->u.Btt.arg2 );
         break;
      case Btc:
         ae->u.Btc.arg1 = subst_AvailExpr_Temp( env, ae->u.Btc.arg1 );
         break;
      case Bct:
         ae->u.Bct.arg2 = subst_AvailExpr_Temp( env, ae->u.Bct.arg2 );
         break;
      case Cf64i:
         break;
      case Ittt:
         ae->u.Ittt.co = subst_AvailExpr_Temp( env, ae->u.Ittt.co );
         ae->u.Ittt.e1 = subst_AvailExpr_Temp( env, ae->u.Ittt.e1 );
         ae->u.Ittt.e0 = subst_AvailExpr_Temp( env, ae->u.Ittt.e0 );
         break;
      case Ittc:
         ae->u.Ittc.co = subst_AvailExpr_Temp( env, ae->u.Ittc.co );
         ae->u.Ittc.e1 = subst_AvailExpr_Temp( env, ae->u.Ittc.e1 );
         break;
      case Itct:
         ae->u.Itct.co = subst_AvailExpr_Temp( env, ae->u.Itct.co );
         ae->u.Itct.e0 = subst_AvailExpr_Temp( env, ae->u.Itct.e0 );
         break;
      case Itcc:
         ae->u.Itcc.co = subst_AvailExpr_Temp( env, ae->u.Itcc.co );
         break;
      case GetIt:
         ae->u.GetIt.ix = subst_AvailExpr_Temp( env, ae->u.GetIt.ix );
         break;
      case CCall: {
         Int i, n = ae->u.CCall.nArgs;;
         for (i = 0; i < n; i++) {
            subst_AvailExpr_TmpOrConst(&ae->u.CCall.args[i], env);
         }
         break;
      }
      case Load:
         subst_AvailExpr_TmpOrConst(&ae->u.Load.addr, env);
         break;
      default: 
         vpanic("subst_AvailExpr");
   }
}

static AvailExpr* irExpr_to_AvailExpr ( IRExpr* e, Bool allowLoadsToBeCSEd )
{
   AvailExpr* ae;

   switch (e->tag) {
      case Iex_Unop:
         if (e->Iex.Unop.arg->tag == Iex_RdTmp) {
            ae = LibVEX_Alloc_inline(sizeof(AvailExpr));
            ae->tag      = Ut;
            ae->u.Ut.op  = e->Iex.Unop.op;
            ae->u.Ut.arg = e->Iex.Unop.arg->Iex.RdTmp.tmp;
            return ae;
         }
         break;

      case Iex_Binop:
         if (e->Iex.Binop.arg1->tag == Iex_RdTmp) {
            if (e->Iex.Binop.arg2->tag == Iex_RdTmp) {
               ae = LibVEX_Alloc_inline(sizeof(AvailExpr));
               ae->tag        = Btt;
               ae->u.Btt.op   = e->Iex.Binop.op;
               ae->u.Btt.arg1 = e->Iex.Binop.arg1->Iex.RdTmp.tmp;
               ae->u.Btt.arg2 = e->Iex.Binop.arg2->Iex.RdTmp.tmp;
               return ae;
            }
            if (e->Iex.Binop.arg2->tag == Iex_Const) {
               ae = LibVEX_Alloc_inline(sizeof(AvailExpr));
               ae->tag        = Btc;
               ae->u.Btc.op   = e->Iex.Binop.op;
               ae->u.Btc.arg1 = e->Iex.Binop.arg1->Iex.RdTmp.tmp;
               ae->u.Btc.con2 = *(e->Iex.Binop.arg2->Iex.Const.con);
               return ae;
            }
         } else if (e->Iex.Binop.arg1->tag == Iex_Const
                    && e->Iex.Binop.arg2->tag == Iex_RdTmp) {
            ae = LibVEX_Alloc_inline(sizeof(AvailExpr));
            ae->tag        = Bct;
            ae->u.Bct.op   = e->Iex.Binop.op;
            ae->u.Bct.arg2 = e->Iex.Binop.arg2->Iex.RdTmp.tmp;
            ae->u.Bct.con1 = *(e->Iex.Binop.arg1->Iex.Const.con);
            return ae;
         }
         break;

      case Iex_Const:
         if (e->Iex.Const.con->tag == Ico_F64i) {
            ae = LibVEX_Alloc_inline(sizeof(AvailExpr));
            ae->tag          = Cf64i;
            ae->u.Cf64i.f64i = e->Iex.Const.con->Ico.F64i;
            return ae;
         }
         break;

      case Iex_ITE:
         if (e->Iex.ITE.cond->tag == Iex_RdTmp) {
            if (e->Iex.ITE.iffalse->tag == Iex_RdTmp) {
               if (e->Iex.ITE.iftrue->tag == Iex_RdTmp) {
                  ae = LibVEX_Alloc_inline(sizeof(AvailExpr));
                  ae->tag       = Ittt;
                  ae->u.Ittt.co = e->Iex.ITE.cond->Iex.RdTmp.tmp;
                  ae->u.Ittt.e1 = e->Iex.ITE.iftrue->Iex.RdTmp.tmp;
                  ae->u.Ittt.e0 = e->Iex.ITE.iffalse->Iex.RdTmp.tmp;
                  return ae;
               }
               if (e->Iex.ITE.iftrue->tag == Iex_Const) {
                  ae = LibVEX_Alloc_inline(sizeof(AvailExpr));
                  ae->tag       = Itct;
                  ae->u.Itct.co = e->Iex.ITE.cond->Iex.RdTmp.tmp;
                  ae->u.Itct.con1 = *(e->Iex.ITE.iftrue->Iex.Const.con);
                  ae->u.Itct.e0 = e->Iex.ITE.iffalse->Iex.RdTmp.tmp;
                  return ae;
               }
            } else if (e->Iex.ITE.iffalse->tag == Iex_Const) {
               if (e->Iex.ITE.iftrue->tag == Iex_RdTmp) {
                  ae = LibVEX_Alloc_inline(sizeof(AvailExpr));
                  ae->tag       = Ittc;
                  ae->u.Ittc.co = e->Iex.ITE.cond->Iex.RdTmp.tmp;
                  ae->u.Ittc.e1 = e->Iex.ITE.iftrue->Iex.RdTmp.tmp;
                  ae->u.Ittc.con0 = *(e->Iex.ITE.iffalse->Iex.Const.con);
                  return ae;
               }
               if (e->Iex.ITE.iftrue->tag == Iex_Const) {
                  ae = LibVEX_Alloc_inline(sizeof(AvailExpr));
                  ae->tag       = Itcc;
                  ae->u.Itcc.co = e->Iex.ITE.cond->Iex.RdTmp.tmp;
                  ae->u.Itcc.con1 = *(e->Iex.ITE.iftrue->Iex.Const.con);
                  ae->u.Itcc.con0 = *(e->Iex.ITE.iffalse->Iex.Const.con);
                  return ae;
               }
            }
         }
         break;

      case Iex_GetI:
         if (e->Iex.GetI.ix->tag == Iex_RdTmp) {
            ae = LibVEX_Alloc_inline(sizeof(AvailExpr));
            ae->tag           = GetIt;
            ae->u.GetIt.descr = e->Iex.GetI.descr;
            ae->u.GetIt.ix    = e->Iex.GetI.ix->Iex.RdTmp.tmp;
            ae->u.GetIt.bias  = e->Iex.GetI.bias;
            return ae;
         }
         break;

      case Iex_CCall:
         ae = LibVEX_Alloc_inline(sizeof(AvailExpr));
         ae->tag = CCall;
         /* Ok to share only the cee, since it is immutable. */
         ae->u.CCall.cee   = e->Iex.CCall.cee;
         ae->u.CCall.retty = e->Iex.CCall.retty;
         /* irExprVec_to_TmpOrConsts will assert if the args are
            neither tmps nor constants, but that's ok .. that's all they
            should be. */
         irExprVec_to_TmpOrConsts(
                                  &ae->u.CCall.args, &ae->u.CCall.nArgs,
                                  e->Iex.CCall.args
                                 );
         return ae;

      case Iex_Load:
         /* If the caller of do_cse_BB has requested that loads also
            be CSEd, convert them into AvailExprs.  If not, we'll just
            return NULL here, and the load never becomes considered
            "available", which effectively disables CSEing of them, as
            desired. */
         if (allowLoadsToBeCSEd) {
            ae = LibVEX_Alloc_inline(sizeof(AvailExpr));
            ae->tag        = Load;
            ae->u.Load.end = e->Iex.Load.end;
            ae->u.Load.ty  = e->Iex.Load.ty;
            irExpr_to_TmpOrConst(&ae->u.Load.addr, e->Iex.Load.addr);
            return ae;
         }
         break;

      default:
         break;
   }

   return NULL;
}


/* The BB is modified in-place.  Returns True if any changes were
   made.  The caller can choose whether or not loads should be CSEd.
   In the normal course of things we don't do that, since CSEing loads
   is something of a dodgy proposition if the guest program is doing
   some screwy stuff to do with races and spinloops. */

static Bool do_cse_BB ( IRSB* bb, Bool allowLoadsToBeCSEd )
{
   Int        i, j, paranoia;
   IRTemp     t, q;
   IRStmt*    st;
   AvailExpr* eprime;
   AvailExpr* ae;
   Bool       invalidate;
   Bool       anyDone = False;

   HashHW* tenv = newHHW(); /* :: IRTemp -> IRTemp */
   HashHW* aenv = newHHW(); /* :: AvailExpr* -> IRTemp */

   vassert(sizeof(IRTemp) <= sizeof(HWord));

   if (0) { ppIRSB(bb); vex_printf("\n\n"); }

   /* Iterate forwards over the stmts.  
      On seeing "t = E", where E is one of the AvailExpr forms:
         let E' = apply tenv substitution to E
         search aenv for E'
            if a mapping E' -> q is found, 
               replace this stmt by "t = q"
               and add binding t -> q to tenv
            else
               add binding E' -> t to aenv
               replace this stmt by "t = E'"

      Other statements are only interesting to the extent that they
      might invalidate some of the expressions in aenv.  So there is
      an invalidate-bindings check for each statement seen.
   */
   for (i = 0; i < bb->stmts_used; i++) {
      st = bb->stmts[i];

      /* ------ BEGIN invalidate aenv bindings ------ */
      /* This is critical: remove from aenv any E' -> .. bindings
         which might be invalidated by this statement.  The only
         vulnerable kind of bindings are the GetI and Load kinds.
            Dirty call - dump (paranoia level -> 2) 
            Store      - dump (ditto) 
            Put, PutI  - dump unless no-overlap is proven (.. -> 1)
         Uses getAliasingRelation_IC and getAliasingRelation_II
         to do the no-overlap assessments needed for Put/PutI.
      */
      switch (st->tag) {
         case Ist_Dirty: case Ist_Store: case Ist_MBE:
         case Ist_CAS: case Ist_LLSC:
         case Ist_StoreG:
            paranoia = 2; break;
         case Ist_Put: case Ist_PutI: 
            paranoia = 1; break;
         case Ist_NoOp: case Ist_IMark: case Ist_AbiHint: 
         case Ist_WrTmp: case Ist_Exit: case Ist_LoadG:
            paranoia = 0; break;
         default: 
            vpanic("do_cse_BB(1)");
      }

      if (paranoia > 0) {
         for (j = 0; j < aenv->used; j++) {
            if (!aenv->inuse[j])
               continue;
            ae = (AvailExpr*)aenv->key[j];
            if (ae->tag != GetIt && ae->tag != Load) 
               continue;
            invalidate = False;
            if (paranoia >= 2) {
               invalidate = True;
            } else {
               vassert(paranoia == 1);
               if (ae->tag == Load) {
                  /* Loads can be invalidated by anything that could
                     possibly touch memory.  But in that case we
                     should have |paranoia| == 2 and we won't get
                     here.  So there's nothing to do; we don't have to
                     invalidate the load. */
               }
               else
               if (st->tag == Ist_Put) {
                  if (getAliasingRelation_IC(
                         ae->u.GetIt.descr, 
                         IRExpr_RdTmp(ae->u.GetIt.ix), 
                         st->Ist.Put.offset, 
                         typeOfIRExpr(bb->tyenv,st->Ist.Put.data) 
                      ) != NoAlias) 
                     invalidate = True;
               }
               else 
               if (st->tag == Ist_PutI) {
                  IRPutI *puti = st->Ist.PutI.details;
                  if (getAliasingRelation_II(
                         ae->u.GetIt.descr, 
                         IRExpr_RdTmp(ae->u.GetIt.ix), 
                         ae->u.GetIt.bias,
                         puti->descr,
                         puti->ix,
                         puti->bias
                      ) != NoAlias)
                     invalidate = True;
               }
               else 
                  vpanic("do_cse_BB(2)");
            }

            if (invalidate) {
               aenv->inuse[j] = False;
               aenv->key[j]   = (HWord)NULL;  /* be sure */
            }
         } /* for j */
      } /* paranoia > 0 */

      /* ------ ENV invalidate aenv bindings ------ */

      /* ignore not-interestings */
      if (st->tag != Ist_WrTmp)
         continue;

      t = st->Ist.WrTmp.tmp;
      eprime = irExpr_to_AvailExpr(st->Ist.WrTmp.data, allowLoadsToBeCSEd);
      /* ignore if not of AvailExpr form */
      if (!eprime)
         continue;

      /* vex_printf("considering: " ); ppIRStmt(st); vex_printf("\n"); */

      /* apply tenv */
      subst_AvailExpr( tenv, eprime );

      /* search aenv for eprime, unfortunately the hard way */
      for (j = 0; j < aenv->used; j++)
         if (aenv->inuse[j] && eq_AvailExpr(eprime, (AvailExpr*)aenv->key[j]))
            break;

      if (j < aenv->used) {
         /* A binding E' -> q was found.  Replace stmt by "t = q" and
            note the t->q binding in tenv. */
         /* (this is the core of the CSE action) */
         q = (IRTemp)aenv->val[j];
         bb->stmts[i] = IRStmt_WrTmp( t, IRExpr_RdTmp(q) );
         addToHHW( tenv, (HWord)t, (HWord)q );
         anyDone = True;
      } else {
         /* No binding was found, so instead we add E' -> t to our
            collection of available expressions, replace this stmt
            with "t = E'", and move on. */
         bb->stmts[i] = IRStmt_WrTmp( t, availExpr_to_IRExpr(eprime) );
         addToHHW( aenv, (HWord)eprime, (HWord)t );
      }
   }

   /*
   ppIRSB(bb);
   sanityCheckIRSB(bb, Ity_I32);
   vex_printf("\n\n");
   */
   return anyDone;
}


/*---------------------------------------------------------------*/
/*--- Add32/Sub32 chain collapsing                            ---*/
/*---------------------------------------------------------------*/

/* ----- Helper functions for Add32/Sub32 chain collapsing ----- */

/* Is this expression "Add32(tmp,const)" or "Sub32(tmp,const)" ?  If
   yes, set *tmp and *i32 appropriately.  *i32 is set as if the
   root node is Add32, not Sub32. */

static Bool isAdd32OrSub32 ( IRExpr* e, IRTemp* tmp, Int* i32 )
{ 
   if (e->tag != Iex_Binop)
      return False;
   if (e->Iex.Binop.op != Iop_Add32 && e->Iex.Binop.op != Iop_Sub32)
      return False;
   if (e->Iex.Binop.arg1->tag != Iex_RdTmp)
      return False;
   if (e->Iex.Binop.arg2->tag != Iex_Const)
      return False;
   *tmp = e->Iex.Binop.arg1->Iex.RdTmp.tmp;
   *i32 = (Int)(e->Iex.Binop.arg2->Iex.Const.con->Ico.U32);
   if (e->Iex.Binop.op == Iop_Sub32)
      *i32 = -*i32;
   return True;
}


/* Figure out if tmp can be expressed as tmp2 +32 const, for some
   other tmp2.  Scan backwards from the specified start point -- an
   optimisation. */

static Bool collapseChain ( IRSB* bb, Int startHere,
                            IRTemp tmp,
                            IRTemp* tmp2, Int* i32 )
{
   Int     j, ii;
   IRTemp  vv;
   IRStmt* st;
   IRExpr* e;

   /* the (var, con) pair contain the current 'representation' for
      'tmp'.  We start with 'tmp + 0'.  */
   IRTemp var = tmp;
   Int    con = 0;

   /* Scan backwards to see if tmp can be replaced by some other tmp
     +/- a constant. */
   for (j = startHere; j >= 0; j--) {
      st = bb->stmts[j];
      if (st->tag != Ist_WrTmp) 
         continue;
      if (st->Ist.WrTmp.tmp != var)
         continue;
      e = st->Ist.WrTmp.data;
      if (!isAdd32OrSub32(e, &vv, &ii))
         break;
      var = vv;
      con += ii;
   }
   if (j == -1)
      /* no earlier binding for var .. ill-formed IR */
      vpanic("collapseChain");

   /* so, did we find anything interesting? */
   if (var == tmp)
      return False; /* no .. */
      
   *tmp2 = var;
   *i32  = con;
   return True;
}


/* ------- Main function for Add32/Sub32 chain collapsing ------ */

static void collapse_AddSub_chains_BB ( IRSB* bb )
{
   IRStmt *st;
   IRTemp var, var2;
   Int    i, con, con2;

   for (i = bb->stmts_used-1; i >= 0; i--) {
      st = bb->stmts[i];
      if (st->tag == Ist_NoOp)
         continue;

      /* Try to collapse 't1 = Add32/Sub32(t2, con)'. */

      if (st->tag == Ist_WrTmp
          && isAdd32OrSub32(st->Ist.WrTmp.data, &var, &con)) {

         /* So e1 is of the form Add32(var,con) or Sub32(var,-con).
            Find out if var can be expressed as var2 + con2. */
         if (collapseChain(bb, i-1, var, &var2, &con2)) {
            if (DEBUG_IROPT) {
               vex_printf("replacing1 ");
               ppIRStmt(st);
               vex_printf(" with ");
            }
            con2 += con;
            bb->stmts[i] 
               = IRStmt_WrTmp(
                    st->Ist.WrTmp.tmp,
                    (con2 >= 0) 
                      ? IRExpr_Binop(Iop_Add32, 
                                     IRExpr_RdTmp(var2),
                                     IRExpr_Const(IRConst_U32(con2)))
                      : IRExpr_Binop(Iop_Sub32, 
                                     IRExpr_RdTmp(var2),
                                     IRExpr_Const(IRConst_U32(-con2)))
                 );
            if (DEBUG_IROPT) {
               ppIRStmt(bb->stmts[i]);
               vex_printf("\n");
            }
         }

         continue;
      }

      /* Try to collapse 't1 = GetI[t2, con]'. */

      if (st->tag == Ist_WrTmp
          && st->Ist.WrTmp.data->tag == Iex_GetI
          && st->Ist.WrTmp.data->Iex.GetI.ix->tag == Iex_RdTmp
          && collapseChain(bb, i-1, st->Ist.WrTmp.data->Iex.GetI.ix
                                      ->Iex.RdTmp.tmp, &var2, &con2)) {
         if (DEBUG_IROPT) {
            vex_printf("replacing3 ");
            ppIRStmt(st);
            vex_printf(" with ");
         }
         con2 += st->Ist.WrTmp.data->Iex.GetI.bias;
         bb->stmts[i]
            = IRStmt_WrTmp(
                 st->Ist.WrTmp.tmp,
                 IRExpr_GetI(st->Ist.WrTmp.data->Iex.GetI.descr,
                             IRExpr_RdTmp(var2),
                             con2));
         if (DEBUG_IROPT) {
            ppIRStmt(bb->stmts[i]);
            vex_printf("\n");
         }
         continue;
      }

      /* Perhaps st is PutI[t, con] ? */
      IRPutI *puti = st->Ist.PutI.details;
      if (st->tag == Ist_PutI
          && puti->ix->tag == Iex_RdTmp
          && collapseChain(bb, i-1, puti->ix->Iex.RdTmp.tmp, 
                               &var2, &con2)) {
         if (DEBUG_IROPT) {
            vex_printf("replacing2 ");
            ppIRStmt(st);
            vex_printf(" with ");
         }
         con2 += puti->bias;
         bb->stmts[i]
            = IRStmt_PutI(mkIRPutI(puti->descr,
                                   IRExpr_RdTmp(var2),
                                   con2,
                                   puti->data));
         if (DEBUG_IROPT) {
            ppIRStmt(bb->stmts[i]);
            vex_printf("\n");
         }
         continue;
      }

   } /* for */
}


/*---------------------------------------------------------------*/
/*--- PutI/GetI transformations                               ---*/
/*---------------------------------------------------------------*/

/* Given the parts (descr, tmp, bias) for a GetI, scan backwards from
   the given starting point to find, if any, a PutI which writes
   exactly the same piece of guest state, and so return the expression
   that the PutI writes.  This is the core of PutI-GetI forwarding. */

static 
IRExpr* findPutI ( IRSB* bb, Int startHere,
                   IRRegArray* descrG, IRExpr* ixG, Int biasG )
{
   Int        j;
   IRStmt*    st;
   GSAliasing relation;

   if (0) {
      vex_printf("\nfindPutI ");
      ppIRRegArray(descrG);
      vex_printf(" ");
      ppIRExpr(ixG);
      vex_printf(" %d\n", biasG);
   }

   /* Scan backwards in bb from startHere to find a suitable PutI
      binding for (descrG, ixG, biasG), if any. */

   for (j = startHere; j >= 0; j--) {
      st = bb->stmts[j];
      if (st->tag == Ist_NoOp) 
         continue;

      if (st->tag == Ist_Put) {
         /* Non-indexed Put.  This can't give a binding, but we do
            need to check it doesn't invalidate the search by
            overlapping any part of the indexed guest state. */

         relation
            = getAliasingRelation_IC(
                 descrG, ixG,
                 st->Ist.Put.offset,
                 typeOfIRExpr(bb->tyenv,st->Ist.Put.data) );

         if (relation == NoAlias) {
            /* we're OK; keep going */
            continue;
         } else {
            /* relation == UnknownAlias || relation == ExactAlias */
            /* If this assertion fails, we've found a Put which writes
               an area of guest state which is read by a GetI.  Which
               is unlikely (although not per se wrong). */
            vassert(relation != ExactAlias);
            /* This Put potentially writes guest state that the GetI
               reads; we must fail. */
            return NULL;
         }
      }

      if (st->tag == Ist_PutI) {
         IRPutI *puti = st->Ist.PutI.details;

         relation = getAliasingRelation_II(
                       descrG, ixG, biasG,
                       puti->descr,
                       puti->ix,
                       puti->bias
                    );

         if (relation == NoAlias) {
            /* This PutI definitely doesn't overlap.  Ignore it and
               keep going. */
            continue; /* the for j loop */
         }

         if (relation == UnknownAlias) {
            /* We don't know if this PutI writes to the same guest
               state that the GetI, or not.  So we have to give up. */
            return NULL;
         }

         /* Otherwise, we've found what we're looking for.  */
         vassert(relation == ExactAlias);
         return puti->data;

      } /* if (st->tag == Ist_PutI) */

      if (st->tag == Ist_Dirty) {
         /* Be conservative.  If the dirty call has any guest effects at
            all, give up.  We could do better -- only give up if there
            are any guest writes/modifies. */
         if (st->Ist.Dirty.details->nFxState > 0)
            return NULL;
      }

   } /* for */

   /* No valid replacement was found. */
   return NULL;
}



/* Assuming pi is a PutI stmt, is s2 identical to it (in the sense
   that it writes exactly the same piece of guest state) ?  Safe
   answer: False. */

static Bool identicalPutIs ( IRStmt* pi, IRStmt* s2 )
{
   vassert(pi->tag == Ist_PutI);
   if (s2->tag != Ist_PutI)
      return False;

   IRPutI *p1 = pi->Ist.PutI.details;
   IRPutI *p2 = s2->Ist.PutI.details;

   return toBool(
          getAliasingRelation_II( 
             p1->descr, p1->ix, p1->bias, 
             p2->descr, p2->ix, p2->bias
          )
          == ExactAlias
          );
}


/* Assuming pi is a PutI stmt, is s2 a Get/GetI/Put/PutI which might
   overlap it?  Safe answer: True.  Note, we could do a lot better
   than this if needed. */

static 
Bool guestAccessWhichMightOverlapPutI ( 
        IRTypeEnv* tyenv, IRStmt* pi, IRStmt* s2 
     )
{
   GSAliasing relation;
   UInt       minoffP, maxoffP;

   vassert(pi->tag == Ist_PutI);

   IRPutI *p1 = pi->Ist.PutI.details;

   getArrayBounds(p1->descr, &minoffP, &maxoffP);
   switch (s2->tag) {

      case Ist_NoOp:
      case Ist_IMark:
         return False;

      case Ist_MBE:
      case Ist_AbiHint:
         /* just be paranoid ... these should be rare. */
         return True;

      case Ist_CAS:
         /* This is unbelievably lame, but it's probably not
            significant from a performance point of view.  Really, a
            CAS is a load-store op, so it should be safe to say False.
            However .. */
         return True;

      case Ist_Dirty:
         /* If the dirty call has any guest effects at all, give up.
            Probably could do better. */
         if (s2->Ist.Dirty.details->nFxState > 0)
            return True;
         return False;

      case Ist_Put:
         vassert(isIRAtom(s2->Ist.Put.data));
         relation 
            = getAliasingRelation_IC(
                 p1->descr, p1->ix,
                 s2->Ist.Put.offset, 
                 typeOfIRExpr(tyenv,s2->Ist.Put.data)
              );
         goto have_relation;

      case Ist_PutI: {
         IRPutI *p2 = s2->Ist.PutI.details;

         vassert(isIRAtom(p2->ix));
         vassert(isIRAtom(p2->data));
         relation
            = getAliasingRelation_II(
                 p1->descr, p1->ix, p1->bias, 
                 p2->descr, p2->ix, p2->bias
              );
         goto have_relation;
      }

      case Ist_WrTmp:
         if (s2->Ist.WrTmp.data->tag == Iex_GetI) {
            relation
               = getAliasingRelation_II(
                    p1->descr, p1->ix, p1->bias, 
                    s2->Ist.WrTmp.data->Iex.GetI.descr,
                    s2->Ist.WrTmp.data->Iex.GetI.ix,
                    s2->Ist.WrTmp.data->Iex.GetI.bias
                 );
            goto have_relation;
         }
         if (s2->Ist.WrTmp.data->tag == Iex_Get) {
            relation
               = getAliasingRelation_IC(
                    p1->descr, p1->ix,
                    s2->Ist.WrTmp.data->Iex.Get.offset,
                    s2->Ist.WrTmp.data->Iex.Get.ty
                 );
            goto have_relation;
         }
         return False;

      case Ist_Store:
         vassert(isIRAtom(s2->Ist.Store.addr));
         vassert(isIRAtom(s2->Ist.Store.data));
         return False;

      default:
         vex_printf("\n"); ppIRStmt(s2); vex_printf("\n");
         vpanic("guestAccessWhichMightOverlapPutI");
   }

  have_relation:
   if (relation == NoAlias)
      return False;
   else
      return True; /* ExactAlias or UnknownAlias */
}



/* ---------- PutI/GetI transformations main functions --------- */

/* Remove redundant GetIs, to the extent that they can be detected.
   bb is modified in-place. */

static
void do_redundant_GetI_elimination ( IRSB* bb )
{
   Int     i;
   IRStmt* st;

   for (i = bb->stmts_used-1; i >= 0; i--) {
      st = bb->stmts[i];
      if (st->tag == Ist_NoOp)
         continue;

      if (st->tag == Ist_WrTmp
          && st->Ist.WrTmp.data->tag == Iex_GetI
          && st->Ist.WrTmp.data->Iex.GetI.ix->tag == Iex_RdTmp) {
         IRRegArray* descr = st->Ist.WrTmp.data->Iex.GetI.descr;
         IRExpr*     ix    = st->Ist.WrTmp.data->Iex.GetI.ix;
         Int         bias  = st->Ist.WrTmp.data->Iex.GetI.bias;
         IRExpr*     replacement = findPutI(bb, i-1, descr, ix, bias);
         if (replacement 
             && isIRAtom(replacement)
             /* Make sure we're doing a type-safe transformation! */
             && typeOfIRExpr(bb->tyenv, replacement) == descr->elemTy) {
            if (DEBUG_IROPT) {
               vex_printf("rGI:  "); 
               ppIRExpr(st->Ist.WrTmp.data);
               vex_printf(" -> ");
               ppIRExpr(replacement);
               vex_printf("\n");
            }
            bb->stmts[i] = IRStmt_WrTmp(st->Ist.WrTmp.tmp, replacement);
         }
      }
   }

}


/* Remove redundant PutIs, to the extent which they can be detected.
   bb is modified in-place. */

static
void do_redundant_PutI_elimination ( IRSB* bb, VexRegisterUpdates pxControl )
{
   Int    i, j;
   Bool   delete;
   IRStmt *st, *stj;

   vassert(pxControl < VexRegUpdAllregsAtEachInsn);

   for (i = 0; i < bb->stmts_used; i++) {
      st = bb->stmts[i];
      if (st->tag != Ist_PutI)
         continue;
      /* Ok, search forwards from here to see if we can find another
         PutI which makes this one redundant, and dodging various 
         hazards.  Search forwards:
         * If conditional exit, give up (because anything after that 
           does not postdominate this put).
         * If a Get which might overlap, give up (because this PutI 
           not necessarily dead).
         * If a Put which is identical, stop with success.
         * If a Put which might overlap, but is not identical, give up.
         * If a dirty helper call which might write guest state, give up.
         * If a Put which definitely doesn't overlap, or any other 
           kind of stmt, continue.
      */
      delete = False;
      for (j = i+1; j < bb->stmts_used; j++) {
         stj = bb->stmts[j];
         if (stj->tag == Ist_NoOp) 
            continue;
         if (identicalPutIs(st, stj)) {
            /* success! */
            delete = True;
            break;
         }
         if (stj->tag == Ist_Exit)
            /* give up */
            break;
         if (st->tag == Ist_Dirty)
            /* give up; could do better here */
            break;
         if (guestAccessWhichMightOverlapPutI(bb->tyenv, st, stj))
            /* give up */
           break;
      }

      if (delete) {
         if (DEBUG_IROPT) {
            vex_printf("rPI:  "); 
            ppIRStmt(st); 
            vex_printf("\n");
         }
         bb->stmts[i] = IRStmt_NoOp();
      }

   }
}


/*---------------------------------------------------------------*/
/*--- Loop unrolling                                          ---*/
/*---------------------------------------------------------------*/

/* Adjust all tmp values (names) in e by delta.  e is destructively
   modified. */

static void deltaIRExpr ( IRExpr* e, Int delta )
{
   Int i;
   switch (e->tag) {
      case Iex_RdTmp:
         e->Iex.RdTmp.tmp += delta;
         break;
      case Iex_Get:
      case Iex_Const:
         break;
      case Iex_GetI:
         deltaIRExpr(e->Iex.GetI.ix, delta);
         break;
      case Iex_Qop:
         deltaIRExpr(e->Iex.Qop.details->arg1, delta);
         deltaIRExpr(e->Iex.Qop.details->arg2, delta);
         deltaIRExpr(e->Iex.Qop.details->arg3, delta);
         deltaIRExpr(e->Iex.Qop.details->arg4, delta);
         break;
      case Iex_Triop:
         deltaIRExpr(e->Iex.Triop.details->arg1, delta);
         deltaIRExpr(e->Iex.Triop.details->arg2, delta);
         deltaIRExpr(e->Iex.Triop.details->arg3, delta);
         break;
      case Iex_Binop:
         deltaIRExpr(e->Iex.Binop.arg1, delta);
         deltaIRExpr(e->Iex.Binop.arg2, delta);
         break;
      case Iex_Unop:
         deltaIRExpr(e->Iex.Unop.arg, delta);
         break;
      case Iex_Load:
         deltaIRExpr(e->Iex.Load.addr, delta);
         break;
      case Iex_CCall:
         for (i = 0; e->Iex.CCall.args[i]; i++)
            deltaIRExpr(e->Iex.CCall.args[i], delta);
         break;
      case Iex_ITE:
         deltaIRExpr(e->Iex.ITE.cond, delta);
         deltaIRExpr(e->Iex.ITE.iftrue, delta);
         deltaIRExpr(e->Iex.ITE.iffalse, delta);
         break;
      default: 
         vex_printf("\n"); ppIRExpr(e); vex_printf("\n");
         vpanic("deltaIRExpr");
   }
}

/* Adjust all tmp values (names) in st by delta.  st is destructively
   modified. */

static void deltaIRStmt ( IRStmt* st, Int delta )
{
   Int      i;
   IRDirty* d;
   switch (st->tag) {
      case Ist_NoOp:
      case Ist_IMark:
      case Ist_MBE:
         break;
      case Ist_AbiHint:
         deltaIRExpr(st->Ist.AbiHint.base, delta);
         deltaIRExpr(st->Ist.AbiHint.nia, delta);
         break;
      case Ist_Put:
         deltaIRExpr(st->Ist.Put.data, delta);
         break;
      case Ist_PutI:
         deltaIRExpr(st->Ist.PutI.details->ix, delta);
         deltaIRExpr(st->Ist.PutI.details->data, delta);
         break;
      case Ist_WrTmp: 
         st->Ist.WrTmp.tmp += delta;
         deltaIRExpr(st->Ist.WrTmp.data, delta);
         break;
      case Ist_Exit:
         deltaIRExpr(st->Ist.Exit.guard, delta);
         break;
      case Ist_Store:
         deltaIRExpr(st->Ist.Store.addr, delta);
         deltaIRExpr(st->Ist.Store.data, delta);
         break;
      case Ist_StoreG: {
         IRStoreG* sg = st->Ist.StoreG.details;
         deltaIRExpr(sg->addr, delta);
         deltaIRExpr(sg->data, delta);
         deltaIRExpr(sg->guard, delta);
         break;
      }
      case Ist_LoadG: {
         IRLoadG* lg = st->Ist.LoadG.details;
         lg->dst += delta;
         deltaIRExpr(lg->addr, delta);
         deltaIRExpr(lg->alt, delta);
         deltaIRExpr(lg->guard, delta);
         break;
      }
      case Ist_CAS:
         if (st->Ist.CAS.details->oldHi != IRTemp_INVALID)
            st->Ist.CAS.details->oldHi += delta;
         st->Ist.CAS.details->oldLo += delta;
         deltaIRExpr(st->Ist.CAS.details->addr, delta);
         if (st->Ist.CAS.details->expdHi)
            deltaIRExpr(st->Ist.CAS.details->expdHi, delta);
         deltaIRExpr(st->Ist.CAS.details->expdLo, delta);
         if (st->Ist.CAS.details->dataHi)
            deltaIRExpr(st->Ist.CAS.details->dataHi, delta);
         deltaIRExpr(st->Ist.CAS.details->dataLo, delta);
         break;
      case Ist_LLSC:
         st->Ist.LLSC.result += delta;
         deltaIRExpr(st->Ist.LLSC.addr, delta);
         if (st->Ist.LLSC.storedata)
            deltaIRExpr(st->Ist.LLSC.storedata, delta);
         break;
      case Ist_Dirty:
         d = st->Ist.Dirty.details;
         deltaIRExpr(d->guard, delta);
         for (i = 0; d->args[i]; i++) {
            IRExpr* arg = d->args[i];
            if (LIKELY(!is_IRExpr_VECRET_or_BBPTR(arg)))
               deltaIRExpr(arg, delta);
         }
         if (d->tmp != IRTemp_INVALID)
            d->tmp += delta;
         if (d->mAddr)
            deltaIRExpr(d->mAddr, delta);
         break;
      default: 
         vex_printf("\n"); ppIRStmt(st); vex_printf("\n");
         vpanic("deltaIRStmt");
   }
}


/* If possible, return a loop-unrolled version of bb0.  The original
   is changed.  If not possible, return NULL.  */

/* The two schemas considered are:

     X: BODY; goto X

     which unrolls to (eg)  X: BODY;BODY; goto X

   and

       X: BODY; if (c) goto X; goto Y
   which trivially transforms to
       X: BODY; if (!c) goto Y; goto X;
   so it falls in the scope of the first case.  

   X and Y must be literal (guest) addresses.
*/

static Int calc_unroll_factor( IRSB* bb )
{
   Int n_stmts, i;

   n_stmts = 0;
   for (i = 0; i < bb->stmts_used; i++) {
      if (bb->stmts[i]->tag != Ist_NoOp)
         n_stmts++;
   }

   if (n_stmts <= vex_control.iropt_unroll_thresh/8) {
      if (vex_control.iropt_verbosity > 0)
         vex_printf("vex iropt: 8 x unrolling (%d sts -> %d sts)\n",
                    n_stmts, 8* n_stmts);
      return 8;
   }
   if (n_stmts <= vex_control.iropt_unroll_thresh/4) {
      if (vex_control.iropt_verbosity > 0)
         vex_printf("vex iropt: 4 x unrolling (%d sts -> %d sts)\n",
                    n_stmts, 4* n_stmts);
      return 4;
   }

   if (n_stmts <= vex_control.iropt_unroll_thresh/2) {
      if (vex_control.iropt_verbosity > 0)
         vex_printf("vex iropt: 2 x unrolling (%d sts -> %d sts)\n",
                    n_stmts, 2* n_stmts);
      return 2;
   }

   if (vex_control.iropt_verbosity > 0)
      vex_printf("vex iropt: not unrolling (%d sts)\n", n_stmts);

   return 1;
}


static IRSB* maybe_loop_unroll_BB ( IRSB* bb0, Addr my_addr )
{
   Int      i, j, jmax, n_vars;
   Bool     xxx_known;
   Addr64   xxx_value, yyy_value;
   IRExpr*  udst;
   IRStmt*  st;
   IRConst* con;
   IRSB     *bb1, *bb2;
   Int      unroll_factor;

   if (vex_control.iropt_unroll_thresh <= 0)
      return NULL;

   /* First off, figure out if we can unroll this loop.  Do this
      without modifying bb0. */

   if (bb0->jumpkind != Ijk_Boring)
      return NULL;

   xxx_known = False;
   xxx_value = 0;

   /* Extract the next-guest address.  If it isn't a literal, we 
      have to give up. */

   udst = bb0->next;
   if (udst->tag == Iex_Const
       && (udst->Iex.Const.con->tag == Ico_U32
           || udst->Iex.Const.con->tag == Ico_U64)) {
      /* The BB ends in a jump to a literal location. */
      xxx_known = True;
      xxx_value = udst->Iex.Const.con->tag == Ico_U64
                    ?  udst->Iex.Const.con->Ico.U64
                    : (Addr64)(udst->Iex.Const.con->Ico.U32);
   }

   if (!xxx_known)
      return NULL;

   /* Now we know the BB ends to a jump to a literal location.  If
      it's a jump to itself (viz, idiom #1), move directly to the
      unrolling stage, first cloning the bb so the original isn't
      modified. */
   if (xxx_value == my_addr) {
      unroll_factor = calc_unroll_factor( bb0 );
      if (unroll_factor < 2)
         return NULL;
      bb1 = deepCopyIRSB( bb0 );
      bb0 = NULL;
      udst = NULL; /* is now invalid */
      goto do_unroll;
   }

   /* Search for the second idiomatic form:
        X: BODY; if (c) goto X; goto Y
      We know Y, but need to establish that the last stmt
      is 'if (c) goto X'.
   */
   yyy_value = xxx_value;
   for (i = bb0->stmts_used-1; i >= 0; i--)
      if (bb0->stmts[i])
         break;

   if (i < 0)
      return NULL; /* block with no stmts.  Strange. */

   st = bb0->stmts[i];
   if (st->tag != Ist_Exit)
      return NULL;
   if (st->Ist.Exit.jk != Ijk_Boring)
      return NULL;

   con = st->Ist.Exit.dst;
   vassert(con->tag == Ico_U32 || con->tag == Ico_U64);

   xxx_value = con->tag == Ico_U64 
                  ? st->Ist.Exit.dst->Ico.U64
                  : (Addr64)(st->Ist.Exit.dst->Ico.U32);

   /* If this assertion fails, we have some kind of type error. */
   vassert(con->tag == udst->Iex.Const.con->tag);

   if (xxx_value != my_addr)
      /* We didn't find either idiom.  Give up. */
      return NULL;

   /* Ok, we found idiom #2.  Copy the BB, switch around the xxx and
      yyy values (which makes it look like idiom #1), and go into
      unrolling proper.  This means finding (again) the last stmt, in
      the copied BB. */

   unroll_factor = calc_unroll_factor( bb0 );
   if (unroll_factor < 2)
      return NULL;

   bb1 = deepCopyIRSB( bb0 );
   bb0 = NULL;
   udst = NULL; /* is now invalid */
   for (i = bb1->stmts_used-1; i >= 0; i--)
      if (bb1->stmts[i])
         break;

   /* The next bunch of assertions should be true since we already
      found and checked the last stmt in the original bb. */

   vassert(i >= 0);

   st = bb1->stmts[i];
   vassert(st->tag == Ist_Exit);

   con = st->Ist.Exit.dst;
   vassert(con->tag == Ico_U32 || con->tag == Ico_U64);

   udst = bb1->next;
   vassert(udst->tag == Iex_Const);
   vassert(udst->Iex.Const.con->tag == Ico_U32
          || udst->Iex.Const.con->tag == Ico_U64);
   vassert(con->tag == udst->Iex.Const.con->tag);

   /* switch the xxx and yyy fields around */
   if (con->tag == Ico_U64) {
      udst->Iex.Const.con->Ico.U64 = xxx_value;
      con->Ico.U64 = yyy_value;
   } else {
      udst->Iex.Const.con->Ico.U32 = (UInt)xxx_value;
      con->Ico.U32 = (UInt)yyy_value;
   }

   /* negate the test condition */
   st->Ist.Exit.guard 
      = IRExpr_Unop(Iop_Not1,deepCopyIRExpr(st->Ist.Exit.guard));

   /* --- The unroller proper.  Both idioms are by now --- */
   /* --- now converted to idiom 1. --- */

  do_unroll:

   vassert(unroll_factor == 2 
           || unroll_factor == 4
           || unroll_factor == 8);

   jmax = unroll_factor==8 ? 3 : (unroll_factor==4 ? 2 : 1);
   for (j = 1; j <= jmax; j++) {

      n_vars = bb1->tyenv->types_used;

      bb2 = deepCopyIRSB(bb1);
      for (i = 0; i < n_vars; i++)
         (void)newIRTemp(bb1->tyenv, bb2->tyenv->types[i]);

      for (i = 0; i < bb2->stmts_used; i++) {
         /* deltaIRStmt destructively modifies the stmt, but 
            that's OK since bb2 is a complete fresh copy of bb1. */
         deltaIRStmt(bb2->stmts[i], n_vars);
         addStmtToIRSB(bb1, bb2->stmts[i]);
      }
   }

   if (DEBUG_IROPT) {
      vex_printf("\nUNROLLED (%lx)\n", my_addr);
      ppIRSB(bb1);
      vex_printf("\n");
   }

   /* Flattening; sigh.  The unroller succeeds in breaking flatness
      by negating the test condition.  This should be fixed properly.
      For the moment use this shotgun approach.  */
   return flatten_BB(bb1);
}


/*---------------------------------------------------------------*/
/*--- The tree builder                                        ---*/
/*---------------------------------------------------------------*/

/* This isn't part of IR optimisation.  Really it's a pass done prior
   to instruction selection, which improves the code that the
   instruction selector can produce. */

/* --- The 'tmp' environment is the central data structure here --- */

/* The number of outstanding bindings we're prepared to track.
   The number of times the env becomes full and we have to dump
   the oldest binding (hence reducing code quality) falls very
   rapidly as the env size increases.  8 gives reasonable performance 
   under most circumstances. */
#define A_NENV 10

/* An interval. Used to record the bytes in the guest state accessed
   by a Put[I] statement or by (one or more) Get[I] expression(s). In 
   case of several Get[I] expressions, the lower/upper bounds are recorded.
   This is conservative but cheap.
   E.g. a Put of 8 bytes at address 100 would be recorded as [100,107].
   E.g. an expression that reads 8 bytes at offset 100 and 4 bytes at
   offset 200 would be recorded as [100,203] */
typedef
   struct {
      Bool present;
      Int  low;
      Int  high;
   }
   Interval;

static inline Bool
intervals_overlap(Interval i1, Interval i2)
{
   return (i1.low >= i2.low && i1.low <= i2.high) ||
          (i2.low >= i1.low && i2.low <= i1.high);
}

static inline void
update_interval(Interval *i, Int low, Int high)
{
   vassert(low <= high);

   if (i->present) {
      if (low  < i->low)  i->low  = low;
      if (high > i->high) i->high = high;
   } else {
      i->present = True;
      i->low  = low;
      i->high = high;
   }
}


/* bindee == NULL   ===  slot is not in use
   bindee != NULL   ===  slot is in use
*/
typedef
   struct {
      IRTemp  binder;
      IRExpr* bindee;
      Bool    doesLoad;
      /* Record the bytes of the guest state BINDEE reads from. */
      Interval getInterval;
   }
   ATmpInfo;

__attribute__((unused))
static void ppAEnv ( ATmpInfo* env )
{
   Int i;
   for (i = 0; i < A_NENV; i++) {
      vex_printf("%d  tmp %d  val ", i, (Int)env[i].binder);
      if (env[i].bindee) 
         ppIRExpr(env[i].bindee);
      else 
         vex_printf("(null)");
      vex_printf("\n");
   }
}

/* --- Tree-traversal fns --- */

/* Traverse an expr, and detect if any part of it reads memory or does
   a Get.  Be careful ... this really controls how much the
   tree-builder can reorder the code, so getting it right is critical.
*/
static void setHints_Expr (Bool* doesLoad, Interval* getInterval, IRExpr* e )
{
   Int i;
   switch (e->tag) {
      case Iex_CCall:
         for (i = 0; e->Iex.CCall.args[i]; i++)
            setHints_Expr(doesLoad, getInterval, e->Iex.CCall.args[i]);
         return;
      case Iex_ITE:
         setHints_Expr(doesLoad, getInterval, e->Iex.ITE.cond);
         setHints_Expr(doesLoad, getInterval, e->Iex.ITE.iftrue);
         setHints_Expr(doesLoad, getInterval, e->Iex.ITE.iffalse);
         return;
      case Iex_Qop:
         setHints_Expr(doesLoad, getInterval, e->Iex.Qop.details->arg1);
         setHints_Expr(doesLoad, getInterval, e->Iex.Qop.details->arg2);
         setHints_Expr(doesLoad, getInterval, e->Iex.Qop.details->arg3);
         setHints_Expr(doesLoad, getInterval, e->Iex.Qop.details->arg4);
         return;
      case Iex_Triop:
         setHints_Expr(doesLoad, getInterval, e->Iex.Triop.details->arg1);
         setHints_Expr(doesLoad, getInterval, e->Iex.Triop.details->arg2);
         setHints_Expr(doesLoad, getInterval, e->Iex.Triop.details->arg3);
         return;
      case Iex_Binop:
         setHints_Expr(doesLoad, getInterval, e->Iex.Binop.arg1);
         setHints_Expr(doesLoad, getInterval, e->Iex.Binop.arg2);
         return;
      case Iex_Unop:
         setHints_Expr(doesLoad, getInterval, e->Iex.Unop.arg);
         return;
      case Iex_Load:
         *doesLoad = True;
         setHints_Expr(doesLoad, getInterval, e->Iex.Load.addr);
         return;
      case Iex_Get: {
         Int low = e->Iex.Get.offset;
         Int high = low + sizeofIRType(e->Iex.Get.ty) - 1;
         update_interval(getInterval, low, high);
         return;
      }
      case Iex_GetI: {
         IRRegArray *descr = e->Iex.GetI.descr;
         Int size = sizeofIRType(descr->elemTy);
         Int low  = descr->base;
         Int high = low + descr->nElems * size - 1;
         update_interval(getInterval, low, high);
         setHints_Expr(doesLoad, getInterval, e->Iex.GetI.ix);
         return;
      }
      case Iex_RdTmp:
      case Iex_Const:
         return;
      default: 
         vex_printf("\n"); ppIRExpr(e); vex_printf("\n");
         vpanic("setHints_Expr");
   }
}


/* Add a binding to the front of the env and slide all the rest
   backwards.  It should be the case that the last slot is free. */
static void addToEnvFront ( ATmpInfo* env, IRTemp binder, IRExpr* bindee )
{
   Int i;
   vassert(env[A_NENV-1].bindee == NULL);
   for (i = A_NENV-1; i >= 1; i--)
      env[i] = env[i-1];
   env[0].binder   = binder;
   env[0].bindee   = bindee;
   env[0].doesLoad = False; /* filled in later */
   env[0].getInterval.present  = False; /* filled in later */
   env[0].getInterval.low  = -1; /* filled in later */
   env[0].getInterval.high = -1; /* filled in later */
}

/* Given uses :: array of UShort, indexed by IRTemp
   Add the use-occurrences of temps in this expression 
   to the env. 
*/
static void aoccCount_Expr ( UShort* uses, IRExpr* e )
{
   Int i;

   switch (e->tag) {

      case Iex_RdTmp: /* the only interesting case */
         uses[e->Iex.RdTmp.tmp]++;
         return;

      case Iex_ITE:
         aoccCount_Expr(uses, e->Iex.ITE.cond);
         aoccCount_Expr(uses, e->Iex.ITE.iftrue);
         aoccCount_Expr(uses, e->Iex.ITE.iffalse);
         return;

      case Iex_Qop: 
         aoccCount_Expr(uses, e->Iex.Qop.details->arg1);
         aoccCount_Expr(uses, e->Iex.Qop.details->arg2);
         aoccCount_Expr(uses, e->Iex.Qop.details->arg3);
         aoccCount_Expr(uses, e->Iex.Qop.details->arg4);
         return;

      case Iex_Triop: 
         aoccCount_Expr(uses, e->Iex.Triop.details->arg1);
         aoccCount_Expr(uses, e->Iex.Triop.details->arg2);
         aoccCount_Expr(uses, e->Iex.Triop.details->arg3);
         return;

      case Iex_Binop: 
         aoccCount_Expr(uses, e->Iex.Binop.arg1);
         aoccCount_Expr(uses, e->Iex.Binop.arg2);
         return;

      case Iex_Unop: 
         aoccCount_Expr(uses, e->Iex.Unop.arg);
         return;

      case Iex_Load:
         aoccCount_Expr(uses, e->Iex.Load.addr);
         return;

      case Iex_CCall:
         for (i = 0; e->Iex.CCall.args[i]; i++)
            aoccCount_Expr(uses, e->Iex.CCall.args[i]);
         return;

      case Iex_GetI:
         aoccCount_Expr(uses, e->Iex.GetI.ix);
         return;

      case Iex_Const:
      case Iex_Get:
         return;

      default: 
         vex_printf("\n"); ppIRExpr(e); vex_printf("\n");
         vpanic("aoccCount_Expr");
    }
}


/* Given uses :: array of UShort, indexed by IRTemp
   Add the use-occurrences of temps in this statement 
   to the env. 
*/
static void aoccCount_Stmt ( UShort* uses, IRStmt* st )
{
   Int      i;
   IRDirty* d;
   IRCAS*   cas;
   switch (st->tag) {
      case Ist_AbiHint:
         aoccCount_Expr(uses, st->Ist.AbiHint.base);
         aoccCount_Expr(uses, st->Ist.AbiHint.nia);
         return;
      case Ist_WrTmp: 
         aoccCount_Expr(uses, st->Ist.WrTmp.data); 
         return; 
      case Ist_Put: 
         aoccCount_Expr(uses, st->Ist.Put.data);
         return;
      case Ist_PutI:
         aoccCount_Expr(uses, st->Ist.PutI.details->ix);
         aoccCount_Expr(uses, st->Ist.PutI.details->data);
         return;
      case Ist_Store:
         aoccCount_Expr(uses, st->Ist.Store.addr);
         aoccCount_Expr(uses, st->Ist.Store.data);
         return;
      case Ist_StoreG: {
         IRStoreG* sg = st->Ist.StoreG.details;
         aoccCount_Expr(uses, sg->addr);
         aoccCount_Expr(uses, sg->data);
         aoccCount_Expr(uses, sg->guard);
         return;
      }
      case Ist_LoadG: {
         IRLoadG* lg = st->Ist.LoadG.details;
         aoccCount_Expr(uses, lg->addr);
         aoccCount_Expr(uses, lg->alt);
         aoccCount_Expr(uses, lg->guard);
         return;
      }
      case Ist_CAS:
         cas = st->Ist.CAS.details;
         aoccCount_Expr(uses, cas->addr);
         if (cas->expdHi)
            aoccCount_Expr(uses, cas->expdHi);
         aoccCount_Expr(uses, cas->expdLo);
         if (cas->dataHi)
            aoccCount_Expr(uses, cas->dataHi);
         aoccCount_Expr(uses, cas->dataLo);
         return;
      case Ist_LLSC:
         aoccCount_Expr(uses, st->Ist.LLSC.addr);
         if (st->Ist.LLSC.storedata)
            aoccCount_Expr(uses, st->Ist.LLSC.storedata);
         return;
      case Ist_Dirty:
         d = st->Ist.Dirty.details;
         if (d->mFx != Ifx_None)
            aoccCount_Expr(uses, d->mAddr);
         aoccCount_Expr(uses, d->guard);
         for (i = 0; d->args[i]; i++) {
            IRExpr* arg = d->args[i];
            if (LIKELY(!is_IRExpr_VECRET_or_BBPTR(arg)))
               aoccCount_Expr(uses, arg);
         }
         return;
      case Ist_NoOp:
      case Ist_IMark:
      case Ist_MBE:
         return;
      case Ist_Exit:
         aoccCount_Expr(uses, st->Ist.Exit.guard);
         return;
      default: 
         vex_printf("\n"); ppIRStmt(st); vex_printf("\n");
         vpanic("aoccCount_Stmt");
   }
}

/* Look up a binding for tmp in the env.  If found, return the bound
   expression, and set the env's binding to NULL so it is marked as
   used.  If not found, return NULL. */

static IRExpr* atbSubst_Temp ( ATmpInfo* env, IRTemp tmp )
{
   Int i;
   for (i = 0; i < A_NENV; i++) {
      if (env[i].binder == tmp && env[i].bindee != NULL) {
         IRExpr* bindee = env[i].bindee;
         env[i].bindee = NULL;
         return bindee;
      }
   }
   return NULL;
}

/* Traverse e, looking for temps.  For each observed temp, see if env
   contains a binding for the temp, and if so return the bound value.
   The env has the property that any binding it holds is
   'single-shot', so once a binding is used, it is marked as no longer
   available, by setting its .bindee field to NULL. */

static inline Bool is_Unop ( IRExpr* e, IROp op ) {
   return e->tag == Iex_Unop && e->Iex.Unop.op == op;
}
static inline Bool is_Binop ( IRExpr* e, IROp op ) {
   return e->tag == Iex_Binop && e->Iex.Binop.op == op;
}

static IRExpr* fold_IRExpr_Binop ( IROp op, IRExpr* a1, IRExpr* a2 )
{
   switch (op) {
   case Iop_Or32:
      /* Or32( CmpwNEZ32(x), CmpwNEZ32(y) ) --> CmpwNEZ32( Or32( x, y ) )  */
      if (is_Unop(a1, Iop_CmpwNEZ32) && is_Unop(a2, Iop_CmpwNEZ32))
         return IRExpr_Unop( Iop_CmpwNEZ32,
                             IRExpr_Binop( Iop_Or32, a1->Iex.Unop.arg, 
                                                     a2->Iex.Unop.arg ) );
      break;

   case Iop_CmpNE32:
      /* Since X has type Ity_I1 we can simplify:
         CmpNE32(1Uto32(X),0)) ==> X */
      if (is_Unop(a1, Iop_1Uto32) && isZeroU32(a2))
         return a1->Iex.Unop.arg;
      break;

   default:
      break;
   }
   /* no reduction rule applies */
   return IRExpr_Binop( op, a1, a2 );
}

static IRExpr* fold_IRExpr_Unop ( IROp op, IRExpr* aa )
{
   switch (op) {
   case Iop_CmpwNEZ64:
      /* CmpwNEZ64( CmpwNEZ64 ( x ) ) --> CmpwNEZ64 ( x ) */
      if (is_Unop(aa, Iop_CmpwNEZ64))
         return IRExpr_Unop( Iop_CmpwNEZ64, aa->Iex.Unop.arg );
      /* CmpwNEZ64( Or64 ( CmpwNEZ64(x), y ) ) --> CmpwNEZ64( Or64( x, y ) ) */
      if (is_Binop(aa, Iop_Or64) 
          && is_Unop(aa->Iex.Binop.arg1, Iop_CmpwNEZ64))
         return fold_IRExpr_Unop(
                   Iop_CmpwNEZ64,
                   IRExpr_Binop(Iop_Or64, 
                                aa->Iex.Binop.arg1->Iex.Unop.arg, 
                                aa->Iex.Binop.arg2));
      /* CmpwNEZ64( Or64 ( x, CmpwNEZ64(y) ) ) --> CmpwNEZ64( Or64( x, y ) ) */
      if (is_Binop(aa, Iop_Or64)
          && is_Unop(aa->Iex.Binop.arg2, Iop_CmpwNEZ64))
         return fold_IRExpr_Unop(
                   Iop_CmpwNEZ64,
                   IRExpr_Binop(Iop_Or64, 
                                aa->Iex.Binop.arg1, 
                                aa->Iex.Binop.arg2->Iex.Unop.arg));
      break;
   case Iop_CmpNEZ64:
      /* CmpNEZ64( Left64(x) ) --> CmpNEZ64(x) */
      if (is_Unop(aa, Iop_Left64)) 
         return IRExpr_Unop(Iop_CmpNEZ64, aa->Iex.Unop.arg);
      /* CmpNEZ64( 1Uto64(X) ) --> X */
      if (is_Unop(aa, Iop_1Uto64))
         return aa->Iex.Unop.arg;
      break;
   case Iop_CmpwNEZ32:
      /* CmpwNEZ32( CmpwNEZ32 ( x ) ) --> CmpwNEZ32 ( x ) */
      if (is_Unop(aa, Iop_CmpwNEZ32))
         return IRExpr_Unop( Iop_CmpwNEZ32, aa->Iex.Unop.arg );
      break;
   case Iop_CmpNEZ32:
      /* CmpNEZ32( Left32(x) ) --> CmpNEZ32(x) */
      if (is_Unop(aa, Iop_Left32)) 
         return IRExpr_Unop(Iop_CmpNEZ32, aa->Iex.Unop.arg);
      /* CmpNEZ32( 1Uto32(X) ) --> X */
      if (is_Unop(aa, Iop_1Uto32))
         return aa->Iex.Unop.arg;
      /* CmpNEZ32( 64to32( CmpwNEZ64(X) ) ) --> CmpNEZ64(X) */
      if (is_Unop(aa, Iop_64to32) && is_Unop(aa->Iex.Unop.arg, Iop_CmpwNEZ64))
         return IRExpr_Unop(Iop_CmpNEZ64, aa->Iex.Unop.arg->Iex.Unop.arg);
      break;
   case Iop_CmpNEZ8:
      /* CmpNEZ8( 1Uto8(X) ) --> X */
      if (is_Unop(aa, Iop_1Uto8))
         return aa->Iex.Unop.arg;
      break;
   case Iop_Left32:
      /* Left32( Left32(x) ) --> Left32(x) */
      if (is_Unop(aa, Iop_Left32))
         return IRExpr_Unop( Iop_Left32, aa->Iex.Unop.arg );
      break;
   case Iop_Left64:
      /* Left64( Left64(x) ) --> Left64(x) */
      if (is_Unop(aa, Iop_Left64))
         return IRExpr_Unop( Iop_Left64, aa->Iex.Unop.arg );
      break;
   case Iop_ZeroHI64ofV128:
      /* ZeroHI64ofV128( ZeroHI64ofV128(x) ) --> ZeroHI64ofV128(x) */
      if (is_Unop(aa, Iop_ZeroHI64ofV128))
         return IRExpr_Unop( Iop_ZeroHI64ofV128, aa->Iex.Unop.arg );
      break;
   case Iop_32to1:
      /* 32to1( 1Uto32 ( x ) ) --> x */
      if (is_Unop(aa, Iop_1Uto32))
         return aa->Iex.Unop.arg;
      /* 32to1( CmpwNEZ32 ( x )) --> CmpNEZ32(x) */
      if (is_Unop(aa, Iop_CmpwNEZ32))
         return IRExpr_Unop( Iop_CmpNEZ32, aa->Iex.Unop.arg );
      break;
   case Iop_64to1:
      /* 64to1( 1Uto64 ( x ) ) --> x */
      if (is_Unop(aa, Iop_1Uto64))
         return aa->Iex.Unop.arg;
      /* 64to1( CmpwNEZ64 ( x )) --> CmpNEZ64(x) */
      if (is_Unop(aa, Iop_CmpwNEZ64))
         return IRExpr_Unop( Iop_CmpNEZ64, aa->Iex.Unop.arg );
      break;
   case Iop_64to32:
      /* 64to32( 32Uto64 ( x )) --> x */
      if (is_Unop(aa, Iop_32Uto64))
         return aa->Iex.Unop.arg;
      /* 64to32( 8Uto64 ( x )) --> 8Uto32(x) */
      if (is_Unop(aa, Iop_8Uto64))
         return IRExpr_Unop(Iop_8Uto32, aa->Iex.Unop.arg);
      break;

   case Iop_32Uto64:
      /* 32Uto64( 8Uto32( x )) --> 8Uto64(x) */
      if (is_Unop(aa, Iop_8Uto32))
         return IRExpr_Unop(Iop_8Uto64, aa->Iex.Unop.arg);
      /* 32Uto64( 16Uto32( x )) --> 16Uto64(x) */
      if (is_Unop(aa, Iop_16Uto32))
         return IRExpr_Unop(Iop_16Uto64, aa->Iex.Unop.arg);
      /* 32Uto64(64to32( Shr64( 32Uto64(64to32(x)), sh ))
                     --> Shr64( 32Uto64(64to32(x)), sh )) */
      if (is_Unop(aa, Iop_64to32)
          && is_Binop(aa->Iex.Unop.arg, Iop_Shr64)
          && is_Unop(aa->Iex.Unop.arg->Iex.Binop.arg1, Iop_32Uto64)
          && is_Unop(aa->Iex.Unop.arg->Iex.Binop.arg1->Iex.Unop.arg,
                     Iop_64to32)) {
         return aa->Iex.Unop.arg;
      }
      /*     32Uto64(64to32( Shl64( 32Uto64(64to32(x)), sh ))
         --> 32Uto64(64to32( Shl64(                x,   sh )) */
      if (is_Unop(aa, Iop_64to32)
          && is_Binop(aa->Iex.Unop.arg, Iop_Shl64)
          && is_Unop(aa->Iex.Unop.arg->Iex.Binop.arg1, Iop_32Uto64)
          && is_Unop(aa->Iex.Unop.arg->Iex.Binop.arg1->Iex.Unop.arg,
                     Iop_64to32)) {
         return
            IRExpr_Unop(
               Iop_32Uto64,
               IRExpr_Unop(
                  Iop_64to32,
                  IRExpr_Binop(
                     Iop_Shl64, 
                     aa->Iex.Unop.arg->Iex.Binop.arg1->Iex.Unop.arg->Iex.Unop.arg,
                     aa->Iex.Unop.arg->Iex.Binop.arg2
            )));
      }
      break;

   case Iop_1Sto32:
      /* 1Sto32( CmpNEZ8( 32to8( 1Uto32( CmpNEZ32( x ))))) -> CmpwNEZ32(x) */
      if (is_Unop(aa, Iop_CmpNEZ8)
          && is_Unop(aa->Iex.Unop.arg, Iop_32to8)
          && is_Unop(aa->Iex.Unop.arg->Iex.Unop.arg, Iop_1Uto32)
          && is_Unop(aa->Iex.Unop.arg->Iex.Unop.arg->Iex.Unop.arg,
                     Iop_CmpNEZ32)) {
         return IRExpr_Unop( Iop_CmpwNEZ32,
                             aa->Iex.Unop.arg->Iex.Unop.arg
                               ->Iex.Unop.arg->Iex.Unop.arg);
      }
      break;

   default:
      break;
   }
   /* no reduction rule applies */
   return IRExpr_Unop( op, aa );
}

static IRExpr* atbSubst_Expr ( ATmpInfo* env, IRExpr* e )
{
   IRExpr*  e2;
   IRExpr** args2;
   Int      i;

   switch (e->tag) {

      case Iex_CCall:
         args2 = shallowCopyIRExprVec(e->Iex.CCall.args);
         for (i = 0; args2[i]; i++)
            args2[i] = atbSubst_Expr(env,args2[i]);
         return IRExpr_CCall(
                   e->Iex.CCall.cee,
                   e->Iex.CCall.retty,
                   args2
                );
      case Iex_RdTmp:
         e2 = atbSubst_Temp(env, e->Iex.RdTmp.tmp);
         return e2 ? e2 : e;
      case Iex_ITE:
         return IRExpr_ITE(
                   atbSubst_Expr(env, e->Iex.ITE.cond),
                   atbSubst_Expr(env, e->Iex.ITE.iftrue),
                   atbSubst_Expr(env, e->Iex.ITE.iffalse)
                );
      case Iex_Qop:
         return IRExpr_Qop(
                   e->Iex.Qop.details->op,
                   atbSubst_Expr(env, e->Iex.Qop.details->arg1),
                   atbSubst_Expr(env, e->Iex.Qop.details->arg2),
                   atbSubst_Expr(env, e->Iex.Qop.details->arg3),
                   atbSubst_Expr(env, e->Iex.Qop.details->arg4)
                );
      case Iex_Triop:
         return IRExpr_Triop(
                   e->Iex.Triop.details->op,
                   atbSubst_Expr(env, e->Iex.Triop.details->arg1),
                   atbSubst_Expr(env, e->Iex.Triop.details->arg2),
                   atbSubst_Expr(env, e->Iex.Triop.details->arg3)
                );
      case Iex_Binop:
         return fold_IRExpr_Binop(
                   e->Iex.Binop.op,
                   atbSubst_Expr(env, e->Iex.Binop.arg1),
                   atbSubst_Expr(env, e->Iex.Binop.arg2)
                );
      case Iex_Unop:
         return fold_IRExpr_Unop(
                   e->Iex.Unop.op,
                   atbSubst_Expr(env, e->Iex.Unop.arg)
                );
      case Iex_Load:
         return IRExpr_Load(
                   e->Iex.Load.end,
                   e->Iex.Load.ty,
                   atbSubst_Expr(env, e->Iex.Load.addr)
                );
      case Iex_GetI:
         return IRExpr_GetI(
                   e->Iex.GetI.descr,
                   atbSubst_Expr(env, e->Iex.GetI.ix),
                   e->Iex.GetI.bias
                );
      case Iex_Const:
      case Iex_Get:
         return e;
      default: 
         vex_printf("\n"); ppIRExpr(e); vex_printf("\n");
         vpanic("atbSubst_Expr");
   }
}

/* Same deal as atbSubst_Expr, except for stmts. */

static IRStmt* atbSubst_Stmt ( ATmpInfo* env, IRStmt* st )
{
   Int     i;
   IRDirty *d, *d2;
   IRCAS   *cas, *cas2;
   IRPutI  *puti, *puti2;

   switch (st->tag) {
      case Ist_AbiHint:
         return IRStmt_AbiHint(
                   atbSubst_Expr(env, st->Ist.AbiHint.base),
                   st->Ist.AbiHint.len,
                   atbSubst_Expr(env, st->Ist.AbiHint.nia)
                );
      case Ist_Store:
         return IRStmt_Store(
                   st->Ist.Store.end,
                   atbSubst_Expr(env, st->Ist.Store.addr),
                   atbSubst_Expr(env, st->Ist.Store.data)
                );
      case Ist_StoreG: {
         IRStoreG* sg = st->Ist.StoreG.details;
         return IRStmt_StoreG(sg->end,
                              atbSubst_Expr(env, sg->addr),
                              atbSubst_Expr(env, sg->data),
                              atbSubst_Expr(env, sg->guard));
      }
      case Ist_LoadG: {
         IRLoadG* lg = st->Ist.LoadG.details;
         return IRStmt_LoadG(lg->end, lg->cvt, lg->dst,
                             atbSubst_Expr(env, lg->addr),
                             atbSubst_Expr(env, lg->alt),
                             atbSubst_Expr(env, lg->guard));
      }
      case Ist_WrTmp:
         return IRStmt_WrTmp(
                   st->Ist.WrTmp.tmp,
                   atbSubst_Expr(env, st->Ist.WrTmp.data)
                );
      case Ist_Put:
         return IRStmt_Put(
                   st->Ist.Put.offset,
                   atbSubst_Expr(env, st->Ist.Put.data)
                );
      case Ist_PutI:
         puti  = st->Ist.PutI.details;
         puti2 = mkIRPutI(puti->descr, 
                          atbSubst_Expr(env, puti->ix),
                          puti->bias,
                          atbSubst_Expr(env, puti->data));
         return IRStmt_PutI(puti2);

      case Ist_Exit:
         return IRStmt_Exit(
                   atbSubst_Expr(env, st->Ist.Exit.guard),
                   st->Ist.Exit.jk,
                   st->Ist.Exit.dst,
                   st->Ist.Exit.offsIP
                );
      case Ist_IMark:
         return IRStmt_IMark(st->Ist.IMark.addr,
                             st->Ist.IMark.len,
                             st->Ist.IMark.delta);
      case Ist_NoOp:
         return IRStmt_NoOp();
      case Ist_MBE:
         return IRStmt_MBE(st->Ist.MBE.event);
      case Ist_CAS:
         cas  = st->Ist.CAS.details;
         cas2 = mkIRCAS(
                   cas->oldHi, cas->oldLo, cas->end, 
                   atbSubst_Expr(env, cas->addr),
                   cas->expdHi ? atbSubst_Expr(env, cas->expdHi) : NULL,
                   atbSubst_Expr(env, cas->expdLo),
                   cas->dataHi ? atbSubst_Expr(env, cas->dataHi) : NULL,
                   atbSubst_Expr(env, cas->dataLo)
                );
         return IRStmt_CAS(cas2);
      case Ist_LLSC:
         return IRStmt_LLSC(
                   st->Ist.LLSC.end,
                   st->Ist.LLSC.result,
                   atbSubst_Expr(env, st->Ist.LLSC.addr),
                   st->Ist.LLSC.storedata
                      ? atbSubst_Expr(env, st->Ist.LLSC.storedata) : NULL
                );
      case Ist_Dirty:
         d  = st->Ist.Dirty.details;
         d2 = emptyIRDirty();
         *d2 = *d;
         if (d2->mFx != Ifx_None)
            d2->mAddr = atbSubst_Expr(env, d2->mAddr);
         d2->guard = atbSubst_Expr(env, d2->guard);
         for (i = 0; d2->args[i]; i++) {
            IRExpr* arg = d2->args[i];
            if (LIKELY(!is_IRExpr_VECRET_or_BBPTR(arg)))
               d2->args[i] = atbSubst_Expr(env, arg);
         }
         return IRStmt_Dirty(d2);
      default: 
         vex_printf("\n"); ppIRStmt(st); vex_printf("\n");
         vpanic("atbSubst_Stmt");
   }
}

inline
static Bool dirty_helper_stores ( const IRDirty *d )
{
   return d->mFx == Ifx_Write || d->mFx == Ifx_Modify;
}

inline
static Interval dirty_helper_puts (
                   const IRDirty *d,
                   Bool (*preciseMemExnsFn)(Int,Int,VexRegisterUpdates),
                   VexRegisterUpdates pxControl,
                   /*OUT*/Bool *requiresPreciseMemExns
                )
{
   Int i;
   Interval interval;

   /* Passing the guest state pointer opens the door to modifying the
      guest state under the covers.  It's not allowed, but let's be
      extra conservative and assume the worst. */
   for (i = 0; d->args[i]; i++) {
      if (UNLIKELY(d->args[i]->tag == Iex_BBPTR)) {
         *requiresPreciseMemExns = True;
         /* Assume all guest state is written. */
         interval.present = True;
         interval.low  = 0;
         interval.high = 0x7FFFFFFF;
         return interval;
      }
   }

   /* Check the side effects on the guest state */
   interval.present = False;
   interval.low = interval.high = -1;
   *requiresPreciseMemExns = False;

   for (i = 0; i < d->nFxState; ++i) {
      if (d->fxState[i].fx != Ifx_Read) {
         Int offset = d->fxState[i].offset;
         Int size = d->fxState[i].size;
         Int nRepeats = d->fxState[i].nRepeats;
         Int repeatLen = d->fxState[i].repeatLen;

         if (preciseMemExnsFn(offset,
                              offset + nRepeats * repeatLen + size - 1,
                              pxControl)) {
            *requiresPreciseMemExns = True;
         }
         update_interval(&interval, offset,
                         offset + nRepeats * repeatLen + size - 1);
      }
   }

   return interval;
}

/* Return an interval if st modifies the guest state.  Via
   requiresPreciseMemExns return whether or not that modification
   requires precise exceptions. */
static Interval stmt_modifies_guest_state ( 
                   IRSB *bb, const IRStmt *st,
                   Bool (*preciseMemExnsFn)(Int,Int,VexRegisterUpdates),
                   VexRegisterUpdates pxControl,
                   /*OUT*/Bool *requiresPreciseMemExns
                )
{
   Interval interval;

   switch (st->tag) {
   case Ist_Put: {
      Int offset = st->Ist.Put.offset;
      Int size = sizeofIRType(typeOfIRExpr(bb->tyenv, st->Ist.Put.data));

      *requiresPreciseMemExns
         = preciseMemExnsFn(offset, offset + size - 1, pxControl);
      interval.present = True;
      interval.low  = offset;
      interval.high = offset + size - 1;
      return interval;
   }

   case Ist_PutI: {
      IRRegArray *descr = st->Ist.PutI.details->descr;
      Int offset = descr->base;
      Int size = sizeofIRType(descr->elemTy);

      /* We quietly assume here that all segments are contiguous and there
         are no holes. This is to avoid a loop. The assumption is conservative
         in the sense that we might report that precise memory exceptions are
         needed when in fact they are not. */
      *requiresPreciseMemExns
         = preciseMemExnsFn(offset, offset + descr->nElems * size - 1,
                            pxControl);
      interval.present = True;
      interval.low  = offset;
      interval.high = offset + descr->nElems * size - 1;
      return interval;
   }

   case Ist_Dirty:
      return dirty_helper_puts(st->Ist.Dirty.details,
                               preciseMemExnsFn, pxControl,
                               requiresPreciseMemExns);

   default:
      *requiresPreciseMemExns = False;
      interval.present = False;
      interval.low  = -1;
      interval.high = -1;
      return interval;
   }
}

/* notstatic */ Addr ado_treebuild_BB (
                        IRSB* bb,
                        Bool (*preciseMemExnsFn)(Int,Int,VexRegisterUpdates),
                        VexRegisterUpdates pxControl
                     )
{
   Int      i, j, k, m;
   Bool     stmtStores, invalidateMe;
   Interval putInterval;
   IRStmt*  st;
   IRStmt*  st2;
   ATmpInfo env[A_NENV];

   Bool   max_ga_known = False;
   Addr   max_ga       = 0;

   Int       n_tmps = bb->tyenv->types_used;
   UShort*   uses   = LibVEX_Alloc_inline(n_tmps * sizeof(UShort));

   /* Phase 1.  Scan forwards in bb, counting use occurrences of each
      temp.  Also count occurrences in the bb->next field.  Take the
      opportunity to also find the maximum guest address in the block,
      since that will be needed later for deciding when we can safely
      elide event checks. */

   for (i = 0; i < n_tmps; i++)
      uses[i] = 0;

   for (i = 0; i < bb->stmts_used; i++) {
      st = bb->stmts[i];
      switch (st->tag) {
         case Ist_NoOp:
            continue;
         case Ist_IMark: {
            UInt len = st->Ist.IMark.len;
            Addr mga = st->Ist.IMark.addr + (len < 1 ? 1 : len) - 1;
            max_ga_known = True;
            if (mga > max_ga)
               max_ga = mga;
            break;
         }
         default:
            break;
      }
      aoccCount_Stmt( uses, st );
   }
   aoccCount_Expr(uses, bb->next );

#  if 0
   for (i = 0; i < n_tmps; i++) {
      if (uses[i] == 0)
        continue;
      ppIRTemp( (IRTemp)i );
      vex_printf("  used %d\n", (Int)uses[i] );
   }
#  endif

   /* Phase 2.  Scan forwards in bb.  For each statement in turn:

         If the env is full, emit the end element.  This guarantees
         there is at least one free slot in the following.

         On seeing 't = E', occ(t)==1,  
            let E'=env(E)
            delete this stmt
            add t -> E' to the front of the env
            Examine E' and set the hints for E' appropriately
              (doesLoad? doesGet?)

         On seeing any other stmt, 
            let stmt' = env(stmt)
            remove from env any 't=E' binds invalidated by stmt
                emit the invalidated stmts
            emit stmt'
            compact any holes in env 
              by sliding entries towards the front

      Finally, apply env to bb->next.  
   */

   for (i = 0; i < A_NENV; i++) {
      env[i].bindee = NULL;
      env[i].binder = IRTemp_INVALID;
   }

   /* The stmts in bb are being reordered, and we are guaranteed to
      end up with no more than the number we started with.  Use i to
      be the cursor of the current stmt examined and j <= i to be that
      for the current stmt being written. 
   */
   j = 0;
   for (i = 0; i < bb->stmts_used; i++) {

      st = bb->stmts[i];
      if (st->tag == Ist_NoOp)
         continue;
     
      /* Ensure there's at least one space in the env, by emitting
         the oldest binding if necessary. */
      if (env[A_NENV-1].bindee != NULL) {
         bb->stmts[j] = IRStmt_WrTmp( env[A_NENV-1].binder, 
                                      env[A_NENV-1].bindee );
         j++;
         vassert(j <= i);
         env[A_NENV-1].bindee = NULL;
      }

      /* Consider current stmt. */
      if (st->tag == Ist_WrTmp && uses[st->Ist.WrTmp.tmp] <= 1) {
         IRExpr *e, *e2;

         /* optional extra: dump dead bindings as we find them.
            Removes the need for a prior dead-code removal pass. */
         if (uses[st->Ist.WrTmp.tmp] == 0) {
	    if (0) vex_printf("DEAD binding\n");
            continue; /* for (i = 0; i < bb->stmts_used; i++) loop */
         }
         vassert(uses[st->Ist.WrTmp.tmp] == 1);

         /* ok, we have 't = E', occ(t)==1.  Do the abovementioned
            actions. */
         e  = st->Ist.WrTmp.data;
         e2 = atbSubst_Expr(env, e);
         addToEnvFront(env, st->Ist.WrTmp.tmp, e2);
         setHints_Expr(&env[0].doesLoad, &env[0].getInterval, e2);
         /* don't advance j, as we are deleting this stmt and instead
            holding it temporarily in the env. */
         continue; /* for (i = 0; i < bb->stmts_used; i++) loop */
      }

      /* we get here for any other kind of statement. */
      /* 'use up' any bindings required by the current statement. */
      st2 = atbSubst_Stmt(env, st);

      /* Now, before this stmt, dump any bindings in env that it
         invalidates.  These need to be dumped in the order in which
         they originally entered env -- that means from oldest to
         youngest. */

      /* putInterval/stmtStores characterise what the stmt under
         consideration does, or might do (sidely safe @ True). */

      Bool putRequiresPreciseMemExns;
      putInterval = stmt_modifies_guest_state(
                       bb, st, preciseMemExnsFn, pxControl,
                       &putRequiresPreciseMemExns
                    );

      /* be True if this stmt writes memory or might do (==> we don't
         want to reorder other loads or stores relative to it).  Also,
         both LL and SC fall under this classification, since we
         really ought to be conservative and not reorder any other
         memory transactions relative to them. */
      stmtStores
         = toBool( st->tag == Ist_Store
                   || (st->tag == Ist_Dirty
                       && dirty_helper_stores(st->Ist.Dirty.details))
                   || st->tag == Ist_LLSC
                   || st->tag == Ist_CAS );

      for (k = A_NENV-1; k >= 0; k--) {
         if (env[k].bindee == NULL)
            continue;
         /* Compare the actions of this stmt with the actions of
            binding 'k', to see if they invalidate the binding. */
         invalidateMe
            = toBool(
              /* a store invalidates loaded data */
              (env[k].doesLoad && stmtStores)
              /* a put invalidates get'd data, if they overlap */
              || ((env[k].getInterval.present && putInterval.present) &&
                  intervals_overlap(env[k].getInterval, putInterval))
              /* a put invalidates loaded data. That means, in essense, that
                 a load expression cannot be substituted into a statement
                 that follows the put. But there is nothing wrong doing so
                 except when the put statement requries precise exceptions.
                 Think of a load that is moved past a put where the put
                 updates the IP in the guest state. If the load generates
                 a segfault, the wrong address (line number) would be
                 reported. */
              || (env[k].doesLoad && putInterval.present &&
                  putRequiresPreciseMemExns)
              /* probably overly conservative: a memory bus event
                 invalidates absolutely everything, so that all
                 computation prior to it is forced to complete before
                 proceeding with the event (fence,lock,unlock). */
              || st->tag == Ist_MBE
              /* also be (probably overly) paranoid re AbiHints */
              || st->tag == Ist_AbiHint
              );
         if (invalidateMe) {
            bb->stmts[j] = IRStmt_WrTmp( env[k].binder, env[k].bindee );
            j++;
            vassert(j <= i);
            env[k].bindee = NULL;
         }
      }

      /* Slide in-use entries in env up to the front */
      m = 0;
      for (k = 0; k < A_NENV; k++) {
         if (env[k].bindee != NULL) {
            env[m] = env[k];
            m++;
	 }
      }
      for (m = m; m < A_NENV; m++) {
         env[m].bindee = NULL;
      }

      /* finally, emit the substituted statement */
      bb->stmts[j] = st2;
      /* vex_printf("**2  "); ppIRStmt(bb->stmts[j]); vex_printf("\n"); */
      j++;

      vassert(j <= i+1);
   } /* for each stmt in the original bb ... */

   /* Finally ... substitute the ->next field as much as possible, and
      dump any left-over bindings.  Hmm.  Perhaps there should be no
      left over bindings?  Or any left-over bindings are
      by definition dead? */
   bb->next = atbSubst_Expr(env, bb->next);
   bb->stmts_used = j;

   return max_ga_known ? max_ga : ~(Addr)0;
}


/*---------------------------------------------------------------*/
/*--- MSVC specific transformation hacks                      ---*/
/*---------------------------------------------------------------*/

/* The purpose of all this is to find MSVC's idiom for non-constant
   bitfield assignment, "a ^ ((a ^ b) & c)", and transform it into
   gcc's idiom "(a & ~c) | (b & c)".  Motivation is that Memcheck has
   generates a lot of false positives from the MSVC version because it
   doesn't understand that XORing an undefined bit with itself gives a
   defined result.

   This isn't a problem for the simple case "x ^ x", because iropt
   folds it to a constant zero before Memcheck ever sees it.  But in
   this case we have an intervening "& c" which defeats the simple
   case.  So we have to carefully inspect all expressions rooted at an
   XOR to see if any of them match "a ^ ((a ^ b) & c)", or any of the
   7 other variants resulting from swapping the order of arguments to
   the three binary operations.  If we get a match, we then replace
   the tree with "(a & ~c) | (b & c)", and Memcheck is happy.

   The key difficulty is to spot the two uses of "a".  To normalise
   the IR to maximise the chances of success, we first do a CSE pass,
   with CSEing of loads enabled, since the two "a" expressions may be
   loads, which need to be commoned up.  Then we do a constant folding
   pass, so as to remove any tmp-to-tmp assignment chains that would
   make matching the original expression more difficult.
*/


/* Helper function for debug printing */
__attribute__((unused))
static void print_flat_expr ( IRExpr** env, IRExpr* e )
{
   if (e == NULL) {
      vex_printf("?");
      return;
   }
   switch (e->tag) {
      case Iex_Binop: {
         ppIROp(e->Iex.Binop.op);
         vex_printf("(");
         print_flat_expr(env, e->Iex.Binop.arg1);
         vex_printf(",");
         print_flat_expr(env, e->Iex.Binop.arg2);
         vex_printf(")");
         break;
      }
      case Iex_Unop: {
         ppIROp(e->Iex.Unop.op);
         vex_printf("(");
         print_flat_expr(env, e->Iex.Unop.arg);
         vex_printf(")");
         break;
      }
      case Iex_RdTmp:
         ppIRTemp(e->Iex.RdTmp.tmp);
         vex_printf("=");
         print_flat_expr(env, chase(env, e));
         break;
      case Iex_Const:
      case Iex_CCall:
      case Iex_Load:
      case Iex_ITE:
      case Iex_Get:
         ppIRExpr(e);
         break;
      default:
         vex_printf("FAIL: "); ppIRExpr(e); vex_printf("\n");
         vassert(0);
   }
}

/* Spot   a ^ ((a ^ b) & c)   for a,b and c tmp-or-const (atoms)
   or any of the other 7 variants generated by switching the order
   of arguments to the outer ^, the inner ^ and the &.
*/
static UInt spotBitfieldAssignment ( /*OUT*/IRExpr** aa, /*OUT*/IRExpr** bb,
                                     /*OUT*/IRExpr** cc,
                                     IRExpr** env, IRExpr* e,
                                     IROp opAND, IROp opXOR)
{
#  define ISBIN(_e,_op) ((_e) && (_e)->tag == Iex_Binop \
                              && (_e)->Iex.Binop.op == (_op))
#  define ISATOM(_e)    isIRAtom(_e)
#  define STEP(_e)      chase1(env, (_e))
#  define LL(_e)        ((_e)->Iex.Binop.arg1)
#  define RR(_e)        ((_e)->Iex.Binop.arg2)

   IRExpr *a1, *and, *xor, *c, *a2bL, *a2bR;

   /* This is common to all 8 cases */
   if (!ISBIN(e, opXOR)) goto fail;

   /*                        -----and------ */
   /*                         --xor---      */
   /* find variant 1:   a1 ^ ((a2 ^ b) & c) */
   /* find variant 2:   a1 ^ ((b ^ a2) & c) */
   a1 = and = xor = c = a2bL = a2bR = NULL;

   a1   = LL(e);
   and  = STEP(RR(e));
   if (!ISBIN(and, opAND)) goto v34;
   xor  = STEP(LL(and));
   c    = RR(and);
   if (!ISBIN(xor, opXOR)) goto v34;
   a2bL = LL(xor);
   a2bR = RR(xor);

   if (eqIRAtom(a1, a2bL) && !eqIRAtom(a1, a2bR)) {
      *aa = a1;
      *bb = a2bR;
      *cc = c;
      return 1;
   }
   if (eqIRAtom(a1, a2bR) && !eqIRAtom(a1, a2bL)) {
      *aa = a1;
      *bb = a2bL;
      *cc = c;
      return 2;
   }

  v34:
   /*                   -----and------      */
   /*                    --xor---           */
   /* find variant 3:   ((a2 ^ b) & c) ^ a1 */
   /* find variant 4:   ((b ^ a2) & c) ^ a1 */
   a1 = and = xor = c = a2bL = a2bR = NULL;

   a1   = RR(e);
   and  = STEP(LL(e));
   if (!ISBIN(and, opAND)) goto v56;
   xor  = STEP(LL(and));
   c    = RR(and);
   if (!ISBIN(xor, opXOR)) goto v56;
   a2bL = LL(xor);
   a2bR = RR(xor);

   if (eqIRAtom(a1, a2bL) && !eqIRAtom(a1, a2bR)) {
      *aa = a1;
      *bb = a2bR;
      *cc = c;
      return 3;
   }
   if (eqIRAtom(a1, a2bR) && !eqIRAtom(a1, a2bL)) {
      *aa = a1;
      *bb = a2bL;
      *cc = c;
      return 4;
   }

  v56:
   /*                        -----and------ */
   /*                         --xor---      */
   /* find variant 5:   a1 ^ (c & (a2 ^ b)) */
   /* find variant 6:   a1 ^ (c & (b ^ a2)) */
   a1 = and = xor = c = a2bL = a2bR = NULL;

   a1   = LL(e);
   and  = STEP(RR(e));
   if (!ISBIN(and, opAND)) goto v78;
   xor  = STEP(RR(and));
   c    = LL(and);
   if (!ISBIN(xor, opXOR)) goto v78;
   a2bL = LL(xor);
   a2bR = RR(xor);

   if (eqIRAtom(a1, a2bL) && !eqIRAtom(a1, a2bR)) {
      *aa = a1;
      *bb = a2bR;
      *cc = c;
      vassert(5-5); // ATC
      return 5;
   }
   if (eqIRAtom(a1, a2bR) && !eqIRAtom(a1, a2bL)) {
      *aa = a1;
      *bb = a2bL;
      *cc = c;
      vassert(6-6); // ATC
      return 6;
   }

 v78:
   /*                   -----and------      */
   /*                    --xor---           */
   /* find variant 7:   (c & (a2 ^ b)) ^ a1 */
   /* find variant 8:   (c & (b ^ a2)) ^ a1 */
   a1 = and = xor = c = a2bL = a2bR = NULL;

   a1   = RR(e);
   and  = STEP(LL(e));
   if (!ISBIN(and, opAND)) goto fail;
   xor  = STEP(RR(and));
   c    = LL(and);
   if (!ISBIN(xor, opXOR)) goto fail;
   a2bL = LL(xor);
   a2bR = RR(xor);

   if (eqIRAtom(a1, a2bL) && !eqIRAtom(a1, a2bR)) {
      *aa = a1;
      *bb = a2bR;
      *cc = c;
      return 7;
   }
   if (eqIRAtom(a1, a2bR) && !eqIRAtom(a1, a2bL)) {
      *aa = a1;
      *bb = a2bL;
      *cc = c;
      return 8;
   }

 fail:
   return 0;

#  undef ISBIN
#  undef ISATOM
#  undef STEP
#  undef LL
#  undef RR
}

/* If |e| is of the form a ^ ((a ^ b) & c) (or any of the 7 other
   variants thereof generated by switching arguments around), return
   the IRExpr* for (a & ~c) | (b & c).  Else return NULL. */
static IRExpr* do_XOR_TRANSFORMS_IRExpr ( IRExpr** env, IRExpr* e )
{
   if (e->tag != Iex_Binop)
      return NULL;

   const HChar* tyNm = NULL;
   IROp   opOR  = Iop_INVALID;
   IROp   opAND = Iop_INVALID;
   IROp   opNOT = Iop_INVALID;
   IROp   opXOR = Iop_INVALID;
   switch (e->Iex.Binop.op) {
      case Iop_Xor32:
         tyNm  = "I32";
         opOR  = Iop_Or32;  opAND = Iop_And32;
         opNOT = Iop_Not32; opXOR = Iop_Xor32;
         break;
      case Iop_Xor16:
         tyNm  = "I16";
         opOR  = Iop_Or16;  opAND = Iop_And16;
         opNOT = Iop_Not16; opXOR = Iop_Xor16;
         break;
      case Iop_Xor8:
         tyNm  = "I8";
         opOR  = Iop_Or8;  opAND = Iop_And8;
         opNOT = Iop_Not8; opXOR = Iop_Xor8;
         break;
      default:
         return NULL;
   }

   IRExpr* aa = NULL;
   IRExpr* bb = NULL;
   IRExpr* cc = NULL;
   UInt variant = spotBitfieldAssignment(&aa, &bb, &cc, env, e, opAND, opXOR);
   if (variant > 0) {
      static UInt ctr = 0;
      if (0)
         vex_printf("XXXXXXXXXX Bitfield Assignment number %u, "
                    "type %s, variant %u\n",
                    ++ctr, tyNm, variant);
      /* It's vitally important that the returned aa, bb and cc are
         atoms -- either constants or tmps.  If it's anything else
         (eg, a GET) then incorporating them in a tree at this point
         in the SB may erroneously pull them forwards (eg of a PUT
         that originally was after the GET) and so transform the IR
         wrongly.  spotBitfieldAssignment should guarantee only to
         give us atoms, but we check here anyway. */
      vassert(aa && isIRAtom(aa));
      vassert(bb && isIRAtom(bb));
      vassert(cc && isIRAtom(cc));
      return IRExpr_Binop(
                opOR,
                IRExpr_Binop(opAND, aa, IRExpr_Unop(opNOT, cc)),
                IRExpr_Binop(opAND, bb, cc)
             );
   }
   return NULL;
}


/* SB is modified in-place.  Visit all the IRExprs and, for those
   which are allowed to be non-atomic, perform the XOR transform if
   possible.  This makes |sb| be non-flat, but that's ok, the caller
   can re-flatten it.  Returns True iff any changes were made. */
static Bool do_XOR_TRANSFORM_IRSB ( IRSB* sb )
{
   Int  i;
   Bool changed = False;

   /* Make the tmp->expr environment, so we can use it for
      chasing expressions. */
   Int      n_tmps = sb->tyenv->types_used;
   IRExpr** env = LibVEX_Alloc_inline(n_tmps * sizeof(IRExpr*));
   for (i = 0; i < n_tmps; i++)
      env[i] = NULL;

   for (i = 0; i < sb->stmts_used; i++) {
      IRStmt* st = sb->stmts[i];
      if (st->tag != Ist_WrTmp)
         continue;
      IRTemp t = st->Ist.WrTmp.tmp;
      vassert(t >= 0 && t < n_tmps);
      env[t] = st->Ist.WrTmp.data;
   }

   for (i = 0; i < sb->stmts_used; i++) {
      IRStmt* st = sb->stmts[i];

      switch (st->tag) {
         case Ist_AbiHint:
            vassert(isIRAtom(st->Ist.AbiHint.base));
            vassert(isIRAtom(st->Ist.AbiHint.nia));
            break;
         case Ist_Put:
            vassert(isIRAtom(st->Ist.Put.data));
            break;
         case Ist_PutI: {
            IRPutI* puti = st->Ist.PutI.details;
            vassert(isIRAtom(puti->ix));
            vassert(isIRAtom(puti->data));
            break;
         }
         case Ist_WrTmp: {
            /* This is the one place where an expr (st->Ist.WrTmp.data) is
               allowed to be more than just a constant or a tmp. */
            IRExpr* mb_new_data
               = do_XOR_TRANSFORMS_IRExpr(env, st->Ist.WrTmp.data);
            if (mb_new_data) {
               //ppIRSB(sb);
               st->Ist.WrTmp.data = mb_new_data;
               //ppIRSB(sb);
               changed = True;
            }
            break;
         }
         case Ist_Store:
            vassert(isIRAtom(st->Ist.Store.addr));
            vassert(isIRAtom(st->Ist.Store.data));
            break;
         case Ist_StoreG: {
            IRStoreG* sg = st->Ist.StoreG.details;
            vassert(isIRAtom(sg->addr));
            vassert(isIRAtom(sg->data));
            vassert(isIRAtom(sg->guard));
            break;
         }
         case Ist_LoadG: {
            IRLoadG* lg = st->Ist.LoadG.details;
            vassert(isIRAtom(lg->addr));
            vassert(isIRAtom(lg->alt));
            vassert(isIRAtom(lg->guard));
            break;
         }
         case Ist_CAS: {
            IRCAS* cas = st->Ist.CAS.details;
            vassert(isIRAtom(cas->addr));
            vassert(cas->expdHi == NULL || isIRAtom(cas->expdHi));
            vassert(isIRAtom(cas->expdLo));
            vassert(cas->dataHi == NULL || isIRAtom(cas->dataHi));
            vassert(isIRAtom(cas->dataLo));
            break;
         }
         case Ist_LLSC:
            vassert(isIRAtom(st->Ist.LLSC.addr));
            if (st->Ist.LLSC.storedata)
               vassert(isIRAtom(st->Ist.LLSC.storedata));
            break;
         case Ist_Dirty: {
            IRDirty* d = st->Ist.Dirty.details;
            if (d->mFx != Ifx_None) {
               vassert(isIRAtom(d->mAddr));
            }
            vassert(isIRAtom(d->guard));
            for (Int j = 0; d->args[j]; j++) {
               IRExpr* arg = d->args[j];
               if (LIKELY(!is_IRExpr_VECRET_or_BBPTR(arg))) {
                  vassert(isIRAtom(arg));
               }
            }
            break;
         }
         case Ist_IMark:
         case Ist_NoOp:
         case Ist_MBE:
            break;
         case Ist_Exit:
            vassert(isIRAtom(st->Ist.Exit.guard));
            break;
         default:
            vex_printf("\n"); ppIRStmt(st);
            vpanic("do_XOR_TRANSFORMS_IRSB");
      }
   }

   vassert(isIRAtom(sb->next));
   return changed;
}


static IRSB* do_MSVC_HACKS ( IRSB* sb )
{
   // Normalise as much as we can.  This is the one-and-only place
   // where we call do_cse_BB with allowLoadsToBeCSEd set to True.
   Bool any_cse_changes = do_cse_BB( sb, True/*allowLoadsToBeCSEd*/ );
   if (any_cse_changes) {
      // CSEing might have created dead code.  Remove it.
      sb = cprop_BB ( sb );
      do_deadcode_BB(sb);
   }

   // Visit all atoms, do the transformation proper.  bb is modified
   // in-place.
   Bool changed = do_XOR_TRANSFORM_IRSB(sb);

   if (changed) {
      // The transformation generates non-flat expressions, so we now
      // need to re-flatten the block.
      sb = flatten_BB(sb);
   }

   return sb;
}


/*---------------------------------------------------------------*/
/*--- iropt main                                              ---*/
/*---------------------------------------------------------------*/

static Bool iropt_verbose = False; /* True; */


/* Do a simple cleanup pass on bb.  This is: redundant Get removal,
   redundant Put removal, constant propagation, dead code removal,
   clean helper specialisation, and dead code removal (again).
*/


static 
IRSB* cheap_transformations ( 
         IRSB* bb,
         IRExpr* (*specHelper) (const HChar*, IRExpr**, IRStmt**, Int),
         Bool (*preciseMemExnsFn)(Int,Int,VexRegisterUpdates),
         VexRegisterUpdates pxControl
      )
{
   redundant_get_removal_BB ( bb );
   if (iropt_verbose) {
      vex_printf("\n========= REDUNDANT GET\n\n" );
      ppIRSB(bb);
   }

   if (pxControl < VexRegUpdAllregsAtEachInsn) {
      redundant_put_removal_BB ( bb, preciseMemExnsFn, pxControl );
   }
   if (iropt_verbose) {
      vex_printf("\n========= REDUNDANT PUT\n\n" );
      ppIRSB(bb);
   }

   bb = cprop_BB ( bb );
   if (iropt_verbose) {
      vex_printf("\n========= CPROPD\n\n" );
      ppIRSB(bb);
   }

   do_deadcode_BB ( bb );
   if (iropt_verbose) {
      vex_printf("\n========= DEAD\n\n" );
      ppIRSB(bb);
   }

   bb = spec_helpers_BB ( bb, specHelper );
   do_deadcode_BB ( bb );
   if (iropt_verbose) {
      vex_printf("\n========= SPECd \n\n" );
      ppIRSB(bb);
   }

   return bb;
}


/* Do some more expensive transformations on bb, which are aimed at
   optimising as much as possible in the presence of GetI and PutI.  */

static
IRSB* expensive_transformations( IRSB* bb, VexRegisterUpdates pxControl )
{
   (void)do_cse_BB( bb, False/*!allowLoadsToBeCSEd*/ );
   collapse_AddSub_chains_BB( bb );
   do_redundant_GetI_elimination( bb );
   if (pxControl < VexRegUpdAllregsAtEachInsn) {
      do_redundant_PutI_elimination( bb, pxControl );
   }
   do_deadcode_BB( bb );
   return bb;
}


/* Scan a flattened BB to look for signs that more expensive
   optimisations might be useful:
   - find out if there are any GetIs and PutIs
   - find out if there are any floating or vector-typed temporaries
*/

static void considerExpensives ( /*OUT*/Bool* hasGetIorPutI,
                                 /*OUT*/Bool* hasVorFtemps,
                                 IRSB* bb )
{
   Int      i, j;
   IRStmt*  st;
   IRDirty* d;
   IRCAS*   cas;

   *hasGetIorPutI = False;
   *hasVorFtemps  = False;

   for (i = 0; i < bb->stmts_used; i++) {
      st = bb->stmts[i];
      switch (st->tag) {
         case Ist_AbiHint:
            vassert(isIRAtom(st->Ist.AbiHint.base));
            vassert(isIRAtom(st->Ist.AbiHint.nia));
            break;
         case Ist_PutI: 
            *hasGetIorPutI = True;
            break;
         case Ist_WrTmp:  
            if (st->Ist.WrTmp.data->tag == Iex_GetI)
               *hasGetIorPutI = True;
            switch (typeOfIRTemp(bb->tyenv, st->Ist.WrTmp.tmp)) {
               case Ity_I1: case Ity_I8: case Ity_I16: 
               case Ity_I32: case Ity_I64: case Ity_I128: 
                  break;
               case Ity_F16: case Ity_F32: case Ity_F64: case Ity_F128:
               case Ity_V128: case Ity_V256:
                  *hasVorFtemps = True;
                  break;
               case Ity_D32: case Ity_D64: case Ity_D128:
                  *hasVorFtemps = True;
                  break;
               default: 
                  goto bad;
            }
            break;
         case Ist_Put:
            vassert(isIRAtom(st->Ist.Put.data));
            break;
         case Ist_Store:
            vassert(isIRAtom(st->Ist.Store.addr));
            vassert(isIRAtom(st->Ist.Store.data));
            break;
         case Ist_StoreG: {
            IRStoreG* sg = st->Ist.StoreG.details;
            vassert(isIRAtom(sg->addr));
            vassert(isIRAtom(sg->data));
            vassert(isIRAtom(sg->guard));
            break;
         }
         case Ist_LoadG: {
            IRLoadG* lg = st->Ist.LoadG.details;
            vassert(isIRAtom(lg->addr));
            vassert(isIRAtom(lg->alt));
            vassert(isIRAtom(lg->guard));
            break;
         }
         case Ist_CAS:
            cas = st->Ist.CAS.details;
            vassert(isIRAtom(cas->addr));
            vassert(cas->expdHi == NULL || isIRAtom(cas->expdHi));
            vassert(isIRAtom(cas->expdLo));
            vassert(cas->dataHi == NULL || isIRAtom(cas->dataHi));
            vassert(isIRAtom(cas->dataLo));
            break;
         case Ist_LLSC:
            vassert(isIRAtom(st->Ist.LLSC.addr));
            if (st->Ist.LLSC.storedata)
               vassert(isIRAtom(st->Ist.LLSC.storedata));
            break;
         case Ist_Dirty:
            d = st->Ist.Dirty.details;
            vassert(isIRAtom(d->guard));
            for (j = 0; d->args[j]; j++) {
               IRExpr* arg = d->args[j];
               if (LIKELY(!is_IRExpr_VECRET_or_BBPTR(arg)))
                  vassert(isIRAtom(arg));
            }
            if (d->mFx != Ifx_None)
               vassert(isIRAtom(d->mAddr));
            break;
         case Ist_NoOp:
         case Ist_IMark:
         case Ist_MBE:
            break;
         case Ist_Exit:
            vassert(isIRAtom(st->Ist.Exit.guard));
            break;
         default: 
         bad:
            ppIRStmt(st);
            vpanic("considerExpensives");
      }
   }
}


/* ---------------- The main iropt entry point. ---------------- */

/* exported from this file */
/* Rules of the game:

   - IRExpr/IRStmt trees should be treated as immutable, as they
     may get shared.  So never change a field of such a tree node;
     instead construct and return a new one if needed.
*/


IRSB* do_iropt_BB(
         IRSB* bb0,
         IRExpr* (*specHelper) (const HChar*, IRExpr**, IRStmt**, Int),
         Bool (*preciseMemExnsFn)(Int,Int,VexRegisterUpdates),
         VexRegisterUpdates pxControl,
         Addr    guest_addr,
         VexArch guest_arch
      )
{
   static Int n_total     = 0;
   static Int n_expensive = 0;

   Bool hasGetIorPutI, hasVorFtemps;
   IRSB *bb, *bb2;

   n_total++;

   /* First flatten the block out, since all other
      phases assume flat code. */

   bb = flatten_BB ( bb0 );

   if (iropt_verbose) {
      vex_printf("\n========= FLAT\n\n" );
      ppIRSB(bb);
   }

   /* If at level 0, stop now. */
   if (vex_control.iropt_level <= 0) return bb;

   /* Now do a preliminary cleanup pass, and figure out if we also
      need to do 'expensive' optimisations.  Expensive optimisations
      are deemed necessary if the block contains any GetIs or PutIs.
      If needed, do expensive transformations and then another cheap
      cleanup pass. */

   bb = cheap_transformations( bb, specHelper, preciseMemExnsFn, pxControl );

   if (guest_arch == VexArchARM) {
      /* Translating Thumb2 code produces a lot of chaff.  We have to
         work extra hard to get rid of it. */
      bb = cprop_BB(bb);
      bb = spec_helpers_BB ( bb, specHelper );
      if (pxControl < VexRegUpdAllregsAtEachInsn) {
         redundant_put_removal_BB ( bb, preciseMemExnsFn, pxControl );
      }
      do_cse_BB( bb, False/*!allowLoadsToBeCSEd*/ );
      do_deadcode_BB( bb );
   }

   if (vex_control.iropt_level > 1) {

      /* Peer at what we have, to decide how much more effort to throw
         at it. */
      considerExpensives( &hasGetIorPutI, &hasVorFtemps, bb );

      if (hasVorFtemps && !hasGetIorPutI) {
         /* If any evidence of FP or Vector activity, CSE, as that
            tends to mop up all manner of lardy code to do with
            rounding modes.  Don't bother if hasGetIorPutI since that
            case leads into the expensive transformations, which do
            CSE anyway. */
         (void)do_cse_BB( bb, False/*!allowLoadsToBeCSEd*/ );
         do_deadcode_BB( bb );
      }

      if (hasGetIorPutI) {
         Bool cses;
         n_expensive++;
         if (DEBUG_IROPT)
            vex_printf("***** EXPENSIVE %d %d\n", n_total, n_expensive);
         bb = expensive_transformations( bb, pxControl );
         bb = cheap_transformations( bb, specHelper,
                                     preciseMemExnsFn, pxControl );
         /* Potentially common up GetIs */
         cses = do_cse_BB( bb, False/*!allowLoadsToBeCSEd*/ );
         if (cses)
            bb = cheap_transformations( bb, specHelper,
                                        preciseMemExnsFn, pxControl );
      }

      ///////////////////////////////////////////////////////////
      // BEGIN MSVC optimised code transformation hacks
      if (0)
         bb = do_MSVC_HACKS(bb);
      // END   MSVC optimised code transformation hacks
      ///////////////////////////////////////////////////////////

      /* Now have a go at unrolling simple (single-BB) loops.  If
         successful, clean up the results as much as possible. */

      bb2 = maybe_loop_unroll_BB( bb, guest_addr );
      if (bb2) {
         bb = cheap_transformations( bb2, specHelper,
                                     preciseMemExnsFn, pxControl );
         if (hasGetIorPutI) {
            bb = expensive_transformations( bb, pxControl );
            bb = cheap_transformations( bb, specHelper,
                                        preciseMemExnsFn, pxControl );
         } else {
            /* at least do CSE and dead code removal */
            do_cse_BB( bb, False/*!allowLoadsToBeCSEd*/ );
            do_deadcode_BB( bb );
         }
         if (0) vex_printf("vex iropt: unrolled a loop\n");
      }

   }

   return bb;
}


/*---------------------------------------------------------------*/
/*--- end                                            ir_opt.c ---*/
/*---------------------------------------------------------------*/
