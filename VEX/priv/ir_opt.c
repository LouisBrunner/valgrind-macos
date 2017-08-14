
/*---------------------------------------------------------------*/
/*--- begin                                          ir_opt.c ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2004-2010 OpenWorks LLP
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

   * Immediately prior to any load or store, those parts of the guest
     state marked as requiring precise exceptions will be up to date.
     Also, guest memory will be up to date.  Parts of the guest state
     not marked as requiring precise exceptions cannot be assumed to
     be up-to-date at the point of the load/store.

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
   HashHW* h = LibVEX_Alloc(sizeof(HashHW));
   h->size   = 8;
   h->used   = 0;
   h->inuse  = LibVEX_Alloc(h->size * sizeof(Bool));
   h->key    = LibVEX_Alloc(h->size * sizeof(HWord));
   h->val    = LibVEX_Alloc(h->size * sizeof(HWord));
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
      Bool*  inuse2 = LibVEX_Alloc(2 * h->size * sizeof(Bool));
      HWord* key2   = LibVEX_Alloc(2 * h->size * sizeof(HWord));
      HWord* val2   = LibVEX_Alloc(2 * h->size * sizeof(HWord));
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

      case Iex_Qop:
         t1 = newIRTemp(bb->tyenv, ty);
         addStmtToIRSB(bb, IRStmt_WrTmp(t1, 
            IRExpr_Qop(ex->Iex.Qop.op,
                         flatten_Expr(bb, ex->Iex.Qop.arg1),
                         flatten_Expr(bb, ex->Iex.Qop.arg2),
                         flatten_Expr(bb, ex->Iex.Qop.arg3),
                         flatten_Expr(bb, ex->Iex.Qop.arg4))));
         return IRExpr_RdTmp(t1);

      case Iex_Triop:
         t1 = newIRTemp(bb->tyenv, ty);
         addStmtToIRSB(bb, IRStmt_WrTmp(t1, 
            IRExpr_Triop(ex->Iex.Triop.op,
                         flatten_Expr(bb, ex->Iex.Triop.arg1),
                         flatten_Expr(bb, ex->Iex.Triop.arg2),
                         flatten_Expr(bb, ex->Iex.Triop.arg3))));
         return IRExpr_RdTmp(t1);

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

      case Iex_Mux0X:
         t1 = newIRTemp(bb->tyenv, ty);
         addStmtToIRSB(bb, IRStmt_WrTmp(t1,
            IRExpr_Mux0X(flatten_Expr(bb, ex->Iex.Mux0X.cond),
                         flatten_Expr(bb, ex->Iex.Mux0X.expr0),
                         flatten_Expr(bb, ex->Iex.Mux0X.exprX))));
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
   IRExpr  *e1, *e2, *e3, *e4, *e5;
   IRDirty *d,  *d2;
   IRCAS   *cas, *cas2;
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
         e1 = flatten_Expr(bb, st->Ist.PutI.ix);
         e2 = flatten_Expr(bb, st->Ist.PutI.data);
         addStmtToIRSB(bb, IRStmt_PutI(st->Ist.PutI.descr,
                                       e1,
                                       st->Ist.PutI.bias,
                                       e2));
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
         for (i = 0; d2->args[i]; i++)
            d2->args[i] = flatten_Expr(bb, d2->args[i]);
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
                                           st->Ist.Exit.dst));
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
            key = mk_key_GetIPutI( st->Ist.PutI.descr );
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
               Bool (*preciseMemExnsFn)(Int,Int)
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

      case Ist_Exit:
         vassert(isIRAtom(st->Ist.Exit.guard));
         break;

      case Ist_PutI:
         vassert(isIRAtom(st->Ist.PutI.ix));
         vassert(isIRAtom(st->Ist.PutI.data));
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
      /* This statement accesses memory.  So we need to dump all parts
         of the environment corresponding to guest state that may not
         be reordered with respect to memory references.  That means
         at least the stack pointer. */
      for (j = 0; j < env->used; j++) {
         if (!env->inuse[j])
            continue;
         if (vex_control.iropt_precise_memory_exns) {
            /* Precise exceptions required.  Flush all guest state. */
            env->inuse[j] = False;
         } else {
            /* Just flush the minimal amount required, as computed by
               preciseMemExnsFn. */
            HWord k_lo = (env->key[j] >> 16) & 0xFFFF;
            HWord k_hi = env->key[j] & 0xFFFF;
            if (preciseMemExnsFn( k_lo, k_hi ))
               env->inuse[j] = False;
         }
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
               Bool (*preciseMemExnsFn)(Int,Int)
            )
{
   Int     i, j;
   Bool    isPut;
   IRStmt* st;
   UInt    key = 0; /* keep gcc -O happy */

   HashHW* env = newHHW();
   for (i = bb->stmts_used-1; i >= 0; i--) {
      st = bb->stmts[i];

      if (st->tag == Ist_NoOp)
         continue;

      /* Deal with conditional exits. */
      if (st->tag == Ist_Exit) {
         /* Since control may not get beyond this point, we must empty
            out the set, since we can no longer claim that the next
            event for any part of the guest state is definitely a
            write. */
         vassert(isIRAtom(st->Ist.Exit.guard));
         for (j = 0; j < env->used; j++)
            env->inuse[j] = False;
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
            key = mk_key_GetIPutI( st->Ist.PutI.descr );
            vassert(isIRAtom(st->Ist.PutI.ix));
            vassert(isIRAtom(st->Ist.PutI.data));
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
      handle_gets_Stmt( env, st, preciseMemExnsFn );
   }
}


/*---------------------------------------------------------------*/
/*--- Constant propagation and folding                        ---*/
/*---------------------------------------------------------------*/

/* The env in this section is a map from IRTemp to IRExpr*,
   that is, an array indexed by IRTemp. */

/* Are both expressions simply the same IRTemp ? */
static Bool sameIRTemps ( IRExpr* e1, IRExpr* e2 )
{
   return toBool( e1->tag == Iex_RdTmp
                  && e2->tag == Iex_RdTmp
                  && e1->Iex.RdTmp.tmp == e2->Iex.RdTmp.tmp );
}

static Bool sameIcoU32s ( IRExpr* e1, IRExpr* e2 )
{
   return toBool( e1->tag == Iex_Const
                  && e2->tag == Iex_Const
                  && e1->Iex.Const.con->tag == Ico_U32
                  && e2->Iex.Const.con->tag == Ico_U32
                  && e1->Iex.Const.con->Ico.U32
                     == e2->Iex.Const.con->Ico.U32 );
}

/* Are both expressions either the same IRTemp or IRConst-U32s ?  If
   in doubt, say No. */
static Bool sameIRTempsOrIcoU32s ( IRExpr* e1, IRExpr* e2 )
{
   switch (e1->tag) {
      case Iex_RdTmp:
         return sameIRTemps(e1, e2);
      case Iex_Const:
         return sameIcoU32s(e1, e2);
      default:
         return False;
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
      case Iop_Xor8:  return IRExpr_Const(IRConst_U8(0));
      case Iop_Xor16: return IRExpr_Const(IRConst_U16(0));
      case Iop_Sub32:
      case Iop_Xor32: return IRExpr_Const(IRConst_U32(0));
      case Iop_Sub64:
      case Iop_Xor64: return IRExpr_Const(IRConst_U64(0));
      case Iop_XorV128: return IRExpr_Const(IRConst_V128(0));
      default: vpanic("mkZeroOfPrimopResultType: bad primop");
   }
}

/* Make a value containing all 1-bits, which has the same type as the
   result of the given primop. */
static IRExpr* mkOnesOfPrimopResultType ( IROp op )
{
   switch (op) {
      case Iop_CmpEQ64:
         return IRExpr_Const(IRConst_U1(toBool(1)));
      case Iop_CmpEQ8x8:
         return IRExpr_Const(IRConst_U64(0xFFFFFFFFFFFFFFFFULL));
      case Iop_CmpEQ8x16:
         return IRExpr_Const(IRConst_V128(0xFFFF));
      default:
         vpanic("mkOnesOfPrimopResultType: bad primop");
   }
}


static IRExpr* fold_Expr ( IRExpr* e )
{
   Int     shift;
   IRExpr* e2 = e; /* e2 is the result of folding e, if possible */

   /* UNARY ops */
   if (e->tag == Iex_Unop
       && e->Iex.Unop.arg->tag == Iex_Const) {
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
            /* signed */ Int s32 = e->Iex.Unop.arg->Iex.Const.con->Ico.U8;
            s32 <<= 24;
            s32 >>= 24;
            e2 = IRExpr_Const(IRConst_U32((UInt)s32));
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
         case Iop_32Sto64: {
            /* signed */ Long s64 = e->Iex.Unop.arg->Iex.Const.con->Ico.U32;
            s64 <<= 32;
            s64 >>= 32;
            e2 = IRExpr_Const(IRConst_U64((ULong)s64));
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

         default: 
            goto unhandled;
      }
   }

   /* BINARY ops */
   if (e->tag == Iex_Binop) {
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

            /* -- And -- */
            case Iop_And8:
               e2 = IRExpr_Const(IRConst_U8(toUChar( 
                       (e->Iex.Binop.arg1->Iex.Const.con->Ico.U8
                        & e->Iex.Binop.arg2->Iex.Const.con->Ico.U8))));
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
               e2 = IRExpr_Const(IRConst_U1(toBool(
                       ((0xFF & e->Iex.Binop.arg1->Iex.Const.con->Ico.U8)
                        != (0xFF & e->Iex.Binop.arg2->Iex.Const.con->Ico.U8)))));
               break;
            case Iop_CmpNE32:
               e2 = IRExpr_Const(IRConst_U1(toBool(
                       (e->Iex.Binop.arg1->Iex.Const.con->Ico.U32
                        != e->Iex.Binop.arg2->Iex.Const.con->Ico.U32))));
               break;
            case Iop_CmpNE64:
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

            /* -- CmpLES -- */
            case Iop_CmpLE32S:
               e2 = IRExpr_Const(IRConst_U1(toBool(
                       ((Int)(e->Iex.Binop.arg1->Iex.Const.con->Ico.U32)
                        <= (Int)(e->Iex.Binop.arg2->Iex.Const.con->Ico.U32)))));
               break;

            /* -- CmpLTS -- */
            case Iop_CmpLT32S:
               e2 = IRExpr_Const(IRConst_U1(toBool(
                       ((Int)(e->Iex.Binop.arg1->Iex.Const.con->Ico.U32)
                        < (Int)(e->Iex.Binop.arg2->Iex.Const.con->Ico.U32)))));
               break;

            /* -- CmpLTU -- */
            case Iop_CmpLT32U:
               e2 = IRExpr_Const(IRConst_U1(toBool(
                       ((UInt)(e->Iex.Binop.arg1->Iex.Const.con->Ico.U32)
                        < (UInt)(e->Iex.Binop.arg2->Iex.Const.con->Ico.U32)))));
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
                       (((ULong)(e->Iex.Binop.arg1->Iex.Const.con->Ico.U32)) << 32)
                       | ((ULong)(e->Iex.Binop.arg2->Iex.Const.con->Ico.U32)) 
                    ));
               break;
            case Iop_64HLto128:
               /* We can't fold this, because there is no way to
                  express he result in IR, but at least pretend to
                  handle it, so as to stop getting blasted with
                  no-rule-for-this-primop messages. */
               break;

            default:
               goto unhandled;
         }

      } else {

         /* other cases (identities, etc) */

         /* Shl64/Shr64(x,0) ==> x */
         if ((e->Iex.Binop.op == Iop_Shl64 || e->Iex.Binop.op == Iop_Shr64)
             && e->Iex.Binop.arg2->tag == Iex_Const
             && e->Iex.Binop.arg2->Iex.Const.con->Ico.U8 == 0) {
            e2 = e->Iex.Binop.arg1;
         } else

         /* Shl32/Shr32(x,0) ==> x */
         if ((e->Iex.Binop.op == Iop_Shl32 || e->Iex.Binop.op == Iop_Shr32)
             && e->Iex.Binop.arg2->tag == Iex_Const
             && e->Iex.Binop.arg2->Iex.Const.con->Ico.U8 == 0) {
            e2 = e->Iex.Binop.arg1;
         } else

         /* Or8(x,0) ==> x */
         if ((e->Iex.Binop.op == Iop_Or8)
             && e->Iex.Binop.arg2->tag == Iex_Const
             && e->Iex.Binop.arg2->Iex.Const.con->Ico.U8 == 0) {
            e2 = e->Iex.Binop.arg1;
         } else

         /* Or16(x,0) ==> x */
         if ((e->Iex.Binop.op == Iop_Or16)
             && e->Iex.Binop.arg2->tag == Iex_Const
             && e->Iex.Binop.arg2->Iex.Const.con->Ico.U16 == 0) {
            e2 = e->Iex.Binop.arg1;
         } else

         /* Or32/Add32/Max32U(x,0) ==> x */
         if ((e->Iex.Binop.op == Iop_Add32 
              || e->Iex.Binop.op == Iop_Or32 || e->Iex.Binop.op == Iop_Max32U)
             && e->Iex.Binop.arg2->tag == Iex_Const
             && e->Iex.Binop.arg2->Iex.Const.con->Ico.U32 == 0) {
            e2 = e->Iex.Binop.arg1;
         } else

         /* Add32(t,t) ==> t << 1.  Memcheck doesn't understand that
            x+x produces a defined least significant bit, and it seems
            simplest just to get rid of the problem by rewriting it
            out, since the opportunity to do so exists. */
         if (e->Iex.Binop.op == Iop_Add32
             && e->Iex.Binop.arg1->tag == Iex_RdTmp
             && e->Iex.Binop.arg2->tag == Iex_RdTmp
             && e->Iex.Binop.arg1->Iex.RdTmp.tmp 
                == e->Iex.Binop.arg2->Iex.RdTmp.tmp) {
            e2 = IRExpr_Binop(Iop_Shl32,
                              e->Iex.Binop.arg1,
                              IRExpr_Const(IRConst_U8(1)));
         } else

         /* Add64(t,t) ==> t << 1;  rationale as for Add32(t,t) above. */
         if (e->Iex.Binop.op == Iop_Add64
             && e->Iex.Binop.arg1->tag == Iex_RdTmp
             && e->Iex.Binop.arg2->tag == Iex_RdTmp
             && e->Iex.Binop.arg1->Iex.RdTmp.tmp 
                == e->Iex.Binop.arg2->Iex.RdTmp.tmp) {
            e2 = IRExpr_Binop(Iop_Shl64,
                              e->Iex.Binop.arg1,
                              IRExpr_Const(IRConst_U8(1)));
         } else

         /* Add8(t,t) ==> t << 1;  rationale as for Add32(t,t) above. */
         if (e->Iex.Binop.op == Iop_Add8
             && e->Iex.Binop.arg1->tag == Iex_RdTmp
             && e->Iex.Binop.arg2->tag == Iex_RdTmp
             && e->Iex.Binop.arg1->Iex.RdTmp.tmp 
                == e->Iex.Binop.arg2->Iex.RdTmp.tmp) {
            e2 = IRExpr_Binop(Iop_Shl8,
                              e->Iex.Binop.arg1,
                              IRExpr_Const(IRConst_U8(1)));
         } else
         /* NB no Add16(t,t) case yet as no known test case exists */

         /* Or64/Add64(x,0) ==> x */
         if ((e->Iex.Binop.op == Iop_Add64 || e->Iex.Binop.op == Iop_Or64)
             && e->Iex.Binop.arg2->tag == Iex_Const
             && e->Iex.Binop.arg2->Iex.Const.con->Ico.U64 == 0) {
            e2 = e->Iex.Binop.arg1;
         } else

         /* And32(x,0xFFFFFFFF) ==> x */
         if (e->Iex.Binop.op == Iop_And32
             && e->Iex.Binop.arg2->tag == Iex_Const
             && e->Iex.Binop.arg2->Iex.Const.con->Ico.U32 == 0xFFFFFFFF) {
            e2 = e->Iex.Binop.arg1;
         } else

         /* And32(x,0) ==> 0 */
         if (e->Iex.Binop.op == Iop_And32
             && e->Iex.Binop.arg2->tag == Iex_Const
             && e->Iex.Binop.arg2->Iex.Const.con->Ico.U32 == 0) {
            e2 = IRExpr_Const(IRConst_U32(0));
         } else

         /* And32/Shl32(0,x) ==> 0 */
         if ((e->Iex.Binop.op == Iop_And32 || e->Iex.Binop.op == Iop_Shl32)
             && e->Iex.Binop.arg1->tag == Iex_Const
             && e->Iex.Binop.arg1->Iex.Const.con->Ico.U32 == 0) {
            e2 = IRExpr_Const(IRConst_U32(0));
         } else

         /* Or8(0,x) ==> x */
         if (e->Iex.Binop.op == Iop_Or8
             && e->Iex.Binop.arg1->tag == Iex_Const
             && e->Iex.Binop.arg1->Iex.Const.con->Ico.U8 == 0) {
            e2 = e->Iex.Binop.arg2;
         } else

         /* Or32/Max32U(0,x) ==> x */
         if ((e->Iex.Binop.op == Iop_Or32 || e->Iex.Binop.op == Iop_Max32U)
             && e->Iex.Binop.arg1->tag == Iex_Const
             && e->Iex.Binop.arg1->Iex.Const.con->Ico.U32 == 0) {
            e2 = e->Iex.Binop.arg2;
         } else

         /* Or64(0,x) ==> x */
         if (e->Iex.Binop.op == Iop_Or64
             && e->Iex.Binop.arg1->tag == Iex_Const
             && e->Iex.Binop.arg1->Iex.Const.con->Ico.U64 == 0) {
            e2 = e->Iex.Binop.arg2;
         } else

         /* Or8/16/32/64/V128(t,t) ==> t, for some IRTemp t */
         /* And8/16/32/64(t,t) ==> t, for some IRTemp t */
         /* Max32U(t,t) ==> t, for some IRTemp t */
         switch (e->Iex.Binop.op) {
            case Iop_And64: case Iop_And32:
            case Iop_And16: case Iop_And8:
            case Iop_Or64: case Iop_Or32:
            case Iop_Or16: case Iop_Or8: case Iop_OrV128:
            case Iop_Max32U:
               if (sameIRTemps(e->Iex.Binop.arg1, e->Iex.Binop.arg2))
                  e2 = e->Iex.Binop.arg1;
               break;
            default:
               break;
         }

         /* Xor8/16/32/64/V128(t,t) ==> 0, for some IRTemp t */
         /* Sub32/64(t,t) ==> 0, for some IRTemp t */
         switch (e->Iex.Binop.op) {
            case Iop_Xor64: case Iop_Xor32:
            case Iop_Xor16: case Iop_Xor8:
            case Iop_XorV128:
            case Iop_Sub64: case Iop_Sub32:
               if (sameIRTemps(e->Iex.Binop.arg1, e->Iex.Binop.arg2))
                  e2 = mkZeroOfPrimopResultType(e->Iex.Binop.op);
               break;
            default:
               break;
         }

         switch (e->Iex.Binop.op) {
            case Iop_CmpEQ64:
            case Iop_CmpEQ8x8:
            case Iop_CmpEQ8x16:
               if (sameIRTemps(e->Iex.Binop.arg1, e->Iex.Binop.arg2))
                  e2 = mkOnesOfPrimopResultType(e->Iex.Binop.op);
               break;
            default:
               break;
         }

      }
   }

   /* Mux0X */
   if (e->tag == Iex_Mux0X) {
      /* is the discriminant is a constant? */
      if (e->Iex.Mux0X.cond->tag == Iex_Const) {
         Bool zero;
         /* assured us by the IR type rules */
         vassert(e->Iex.Mux0X.cond->Iex.Const.con->tag == Ico_U8);
         zero = toBool(0 == (0xFF & e->Iex.Mux0X.cond
                                     ->Iex.Const.con->Ico.U8));
         e2 = zero ? e->Iex.Mux0X.expr0 : e->Iex.Mux0X.exprX;
      }
      else
      /* are the arms identical? (pretty weedy test) */
      if (sameIRTempsOrIcoU32s(e->Iex.Mux0X.expr0,
                               e->Iex.Mux0X.exprX)) {
         e2 = e->Iex.Mux0X.expr0;
      }
   }

   /* Show cases where we've found but not folded 'op(t,t)'. */
   if (0 && e == e2 && e->tag == Iex_Binop 
       && sameIRTemps(e->Iex.Binop.arg1, e->Iex.Binop.arg2)) {
      vex_printf("IDENT: ");
      ppIRExpr(e); vex_printf("\n");
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
      vex_printf("vex iropt: fold_Expr: no rule for: ");
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
            return env[(Int)ex->Iex.RdTmp.tmp];
         } else {
            /* not bound in env */
            return ex;
         }

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

      case Iex_Qop:
         vassert(isIRAtom(ex->Iex.Qop.arg1));
         vassert(isIRAtom(ex->Iex.Qop.arg2));
         vassert(isIRAtom(ex->Iex.Qop.arg3));
         vassert(isIRAtom(ex->Iex.Qop.arg4));
         return IRExpr_Qop(
                   ex->Iex.Qop.op,
                   subst_Expr(env, ex->Iex.Qop.arg1),
                   subst_Expr(env, ex->Iex.Qop.arg2),
                   subst_Expr(env, ex->Iex.Qop.arg3),
                   subst_Expr(env, ex->Iex.Qop.arg4)
                );

      case Iex_Triop:
         vassert(isIRAtom(ex->Iex.Triop.arg1));
         vassert(isIRAtom(ex->Iex.Triop.arg2));
         vassert(isIRAtom(ex->Iex.Triop.arg3));
         return IRExpr_Triop(
                   ex->Iex.Triop.op,
                   subst_Expr(env, ex->Iex.Triop.arg1),
                   subst_Expr(env, ex->Iex.Triop.arg2),
                   subst_Expr(env, ex->Iex.Triop.arg3)
                );

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

      case Iex_Mux0X:
         vassert(isIRAtom(ex->Iex.Mux0X.cond));
         vassert(isIRAtom(ex->Iex.Mux0X.expr0));
         vassert(isIRAtom(ex->Iex.Mux0X.exprX));
         return IRExpr_Mux0X(
                   subst_Expr(env, ex->Iex.Mux0X.cond),
                   subst_Expr(env, ex->Iex.Mux0X.expr0),
                   subst_Expr(env, ex->Iex.Mux0X.exprX)
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
                   fold_Expr(subst_Expr(env, st->Ist.AbiHint.base)),
                   st->Ist.AbiHint.len,
                   fold_Expr(subst_Expr(env, st->Ist.AbiHint.nia))
                );
      case Ist_Put:
         vassert(isIRAtom(st->Ist.Put.data));
         return IRStmt_Put(
                   st->Ist.Put.offset, 
                   fold_Expr(subst_Expr(env, st->Ist.Put.data)) 
                );

      case Ist_PutI:
         vassert(isIRAtom(st->Ist.PutI.ix));
         vassert(isIRAtom(st->Ist.PutI.data));
         return IRStmt_PutI(
                   st->Ist.PutI.descr,
                   fold_Expr(subst_Expr(env, st->Ist.PutI.ix)),
                   st->Ist.PutI.bias,
                   fold_Expr(subst_Expr(env, st->Ist.PutI.data))
                );

      case Ist_WrTmp:
         /* This is the one place where an expr (st->Ist.WrTmp.data) is
            allowed to be more than just a constant or a tmp. */
         return IRStmt_WrTmp(
                   st->Ist.WrTmp.tmp,
                   fold_Expr(subst_Expr(env, st->Ist.WrTmp.data))
                );

      case Ist_Store:
         vassert(isIRAtom(st->Ist.Store.addr));
         vassert(isIRAtom(st->Ist.Store.data));
         return IRStmt_Store(
                   st->Ist.Store.end,
                   fold_Expr(subst_Expr(env, st->Ist.Store.addr)),
                   fold_Expr(subst_Expr(env, st->Ist.Store.data))
                );

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
                   fold_Expr(subst_Expr(env, cas->addr)),
                   cas->expdHi ? fold_Expr(subst_Expr(env, cas->expdHi)) : NULL,
                   fold_Expr(subst_Expr(env, cas->expdLo)),
                   cas->dataHi ? fold_Expr(subst_Expr(env, cas->dataHi)) : NULL,
                   fold_Expr(subst_Expr(env, cas->dataLo))
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
                   fold_Expr(subst_Expr(env, st->Ist.LLSC.addr)),
                   st->Ist.LLSC.storedata
                      ? fold_Expr(subst_Expr(env, st->Ist.LLSC.storedata))
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
            d2->mAddr = fold_Expr(subst_Expr(env, d2->mAddr));
         }
         vassert(isIRAtom(d2->guard));
         d2->guard = fold_Expr(subst_Expr(env, d2->guard));
         for (i = 0; d2->args[i]; i++) {
            vassert(isIRAtom(d2->args[i]));
            d2->args[i] = fold_Expr(subst_Expr(env, d2->args[i]));
         }
         return IRStmt_Dirty(d2);
      }

      case Ist_IMark:
         return IRStmt_IMark(st->Ist.IMark.addr, st->Ist.IMark.len);

      case Ist_NoOp:
         return IRStmt_NoOp();

      case Ist_MBE:
         return IRStmt_MBE(st->Ist.MBE.event);

      case Ist_Exit: {
         IRExpr* fcond;
         vassert(isIRAtom(st->Ist.Exit.guard));
         fcond = fold_Expr(subst_Expr(env, st->Ist.Exit.guard));
         if (fcond->tag == Iex_Const) {
            /* Interesting.  The condition on this exit has folded down to
               a constant. */
            vassert(fcond->Iex.Const.con->tag == Ico_U1);
            vassert(fcond->Iex.Const.con->Ico.U1 == False
                    || fcond->Iex.Const.con->Ico.U1 == True);
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
         return IRStmt_Exit(fcond, st->Ist.Exit.jk, st->Ist.Exit.dst);
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
   IRExpr** env = LibVEX_Alloc(n_tmps * sizeof(IRExpr*));

   out = emptyIRSB();
   out->tyenv = deepCopyIRTypeEnv( in->tyenv );

   /* Set up the env with which travels forward.  This holds a
      substitution, mapping IRTemps to atoms, that is, IRExprs which
      are either IRTemps or IRConsts.  Thus, copy and constant
      propagation is done.  The environment is to be applied as we
      move along.  Keys are IRTemps.  Values are IRExpr*s.
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

      /* If the statement has been folded into a no-op, forget it. */
      if (st2->tag == Ist_NoOp) continue;

      /* Now consider what the stmt looks like.  If it's of the form
         't = const' or 't1 = t2', add it to the running environment
         and not to the output BB.  Otherwise, add it to the output
         BB.  Note, we choose not to propagate const when const is an
         F64i, so that F64i literals can be CSE'd later.  This helps
         x86 floating point code generation. */

      if (st2->tag == Ist_WrTmp 
          && st2->Ist.WrTmp.data->tag == Iex_Const
          && st2->Ist.WrTmp.data->Iex.Const.con->tag != Ico_F64i) {
         /* 't = const' -- add to env.  
             The pair (IRTemp, IRExpr*) is added. */
         env[(Int)(st2->Ist.WrTmp.tmp)] = st2->Ist.WrTmp.data;
      }
      else
      if (st2->tag == Ist_WrTmp && st2->Ist.WrTmp.data->tag == Iex_RdTmp) {
         /* 't1 = t2' -- add to env.  
             The pair (IRTemp, IRExpr*) is added. */
         env[(Int)(st2->Ist.WrTmp.tmp)] = st2->Ist.WrTmp.data;
      }
      else {
         /* Not interesting, copy st2 into the output block. */
         addStmtToIRSB( out, st2 );
      }
   }

   out->next     = subst_Expr( env, in->next );
   out->jumpkind = in->jumpkind;
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
      case Iex_Mux0X:
         addUses_Expr(set, e->Iex.Mux0X.cond);
         addUses_Expr(set, e->Iex.Mux0X.expr0);
         addUses_Expr(set, e->Iex.Mux0X.exprX);
         return;
      case Iex_CCall:
         for (i = 0; e->Iex.CCall.args[i]; i++)
            addUses_Expr(set, e->Iex.CCall.args[i]);
         return;
      case Iex_Load:
         addUses_Expr(set, e->Iex.Load.addr);
         return;
      case Iex_Qop:
         addUses_Expr(set, e->Iex.Qop.arg1);
         addUses_Expr(set, e->Iex.Qop.arg2);
         addUses_Expr(set, e->Iex.Qop.arg3);
         addUses_Expr(set, e->Iex.Qop.arg4);
         return;
      case Iex_Triop:
         addUses_Expr(set, e->Iex.Triop.arg1);
         addUses_Expr(set, e->Iex.Triop.arg2);
         addUses_Expr(set, e->Iex.Triop.arg3);
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
         addUses_Expr(set, st->Ist.PutI.ix);
         addUses_Expr(set, st->Ist.PutI.data);
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
         for (i = 0; d->args[i] != NULL; i++)
            addUses_Expr(set, d->args[i]);
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
   Bool*   set = LibVEX_Alloc(n_tmps * sizeof(Bool));
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
         IRExpr* (*specHelper) (HChar*, IRExpr**, IRStmt**, Int)
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
      enum { Ut, Btt, Btc, Bct, Cf64i, Mttt, GetIt } tag;
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
         /* Mux0X(tmp,tmp,tmp) */
         struct {
            IRTemp co;
            IRTemp e0;
            IRTemp eX;
         } Mttt;
         /* GetI(descr,tmp,bias)*/
         struct {
            IRRegArray* descr;
            IRTemp      ix;
            Int         bias;
         } GetIt;
      } u;
   }
   AvailExpr;

static Bool eq_AvailExpr ( AvailExpr* a1, AvailExpr* a2 )
{
   if (a1->tag != a2->tag)
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
      case Mttt:
         return toBool(a1->u.Mttt.co == a2->u.Mttt.co
                       && a1->u.Mttt.e0 == a2->u.Mttt.e0
                       && a1->u.Mttt.eX == a2->u.Mttt.eX);
      case GetIt:
         return toBool(eqIRRegArray(a1->u.GetIt.descr, a2->u.GetIt.descr) 
                       && a1->u.GetIt.ix == a2->u.GetIt.ix
                       && a1->u.GetIt.bias == a2->u.GetIt.bias);
      default: vpanic("eq_AvailExpr");
   }
}

static IRExpr* availExpr_to_IRExpr ( AvailExpr* ae ) 
{
   IRConst* con;
   switch (ae->tag) {
      case Ut:
         return IRExpr_Unop( ae->u.Ut.op, IRExpr_RdTmp(ae->u.Ut.arg) );
      case Btt:
         return IRExpr_Binop( ae->u.Btt.op,
                              IRExpr_RdTmp(ae->u.Btt.arg1),
                              IRExpr_RdTmp(ae->u.Btt.arg2) );
      case Btc:
         con = LibVEX_Alloc(sizeof(IRConst));
         *con = ae->u.Btc.con2;
         return IRExpr_Binop( ae->u.Btc.op,
                              IRExpr_RdTmp(ae->u.Btc.arg1), 
                              IRExpr_Const(con) );
      case Bct:
         con = LibVEX_Alloc(sizeof(IRConst));
         *con = ae->u.Bct.con1;
         return IRExpr_Binop( ae->u.Bct.op,
                              IRExpr_Const(con), 
                              IRExpr_RdTmp(ae->u.Bct.arg2) );
      case Cf64i:
         return IRExpr_Const(IRConst_F64i(ae->u.Cf64i.f64i));
      case Mttt:
         return IRExpr_Mux0X(IRExpr_RdTmp(ae->u.Mttt.co), 
                             IRExpr_RdTmp(ae->u.Mttt.e0), 
                             IRExpr_RdTmp(ae->u.Mttt.eX));
      case GetIt:
         return IRExpr_GetI(ae->u.GetIt.descr,
                            IRExpr_RdTmp(ae->u.GetIt.ix),
                            ae->u.GetIt.bias);
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
      case Mttt:
         ae->u.Mttt.co = subst_AvailExpr_Temp( env, ae->u.Mttt.co );
         ae->u.Mttt.e0 = subst_AvailExpr_Temp( env, ae->u.Mttt.e0 );
         ae->u.Mttt.eX = subst_AvailExpr_Temp( env, ae->u.Mttt.eX );
         break;
      case GetIt:
         ae->u.GetIt.ix = subst_AvailExpr_Temp( env, ae->u.GetIt.ix );
         break;
      default: 
         vpanic("subst_AvailExpr");
   }
}

static AvailExpr* irExpr_to_AvailExpr ( IRExpr* e )
{
   AvailExpr* ae;

   if (e->tag == Iex_Unop
       && e->Iex.Unop.arg->tag == Iex_RdTmp) {
      ae = LibVEX_Alloc(sizeof(AvailExpr));
      ae->tag      = Ut;
      ae->u.Ut.op  = e->Iex.Unop.op;
      ae->u.Ut.arg = e->Iex.Unop.arg->Iex.RdTmp.tmp;
      return ae;
   }

   if (e->tag == Iex_Binop
       && e->Iex.Binop.arg1->tag == Iex_RdTmp
       && e->Iex.Binop.arg2->tag == Iex_RdTmp) {
      ae = LibVEX_Alloc(sizeof(AvailExpr));
      ae->tag        = Btt;
      ae->u.Btt.op   = e->Iex.Binop.op;
      ae->u.Btt.arg1 = e->Iex.Binop.arg1->Iex.RdTmp.tmp;
      ae->u.Btt.arg2 = e->Iex.Binop.arg2->Iex.RdTmp.tmp;
      return ae;
   }

   if (e->tag == Iex_Binop
      && e->Iex.Binop.arg1->tag == Iex_RdTmp
      && e->Iex.Binop.arg2->tag == Iex_Const) {
      ae = LibVEX_Alloc(sizeof(AvailExpr));
      ae->tag        = Btc;
      ae->u.Btc.op   = e->Iex.Binop.op;
      ae->u.Btc.arg1 = e->Iex.Binop.arg1->Iex.RdTmp.tmp;
      ae->u.Btc.con2 = *(e->Iex.Binop.arg2->Iex.Const.con);
      return ae;
   }

   if (e->tag == Iex_Binop
      && e->Iex.Binop.arg1->tag == Iex_Const
      && e->Iex.Binop.arg2->tag == Iex_RdTmp) {
      ae = LibVEX_Alloc(sizeof(AvailExpr));
      ae->tag        = Bct;
      ae->u.Bct.op   = e->Iex.Binop.op;
      ae->u.Bct.arg2 = e->Iex.Binop.arg2->Iex.RdTmp.tmp;
      ae->u.Bct.con1 = *(e->Iex.Binop.arg1->Iex.Const.con);
      return ae;
   }

   if (e->tag == Iex_Const
       && e->Iex.Const.con->tag == Ico_F64i) {
      ae = LibVEX_Alloc(sizeof(AvailExpr));
      ae->tag          = Cf64i;
      ae->u.Cf64i.f64i = e->Iex.Const.con->Ico.F64i;
      return ae;
   }

   if (e->tag == Iex_Mux0X
       && e->Iex.Mux0X.cond->tag == Iex_RdTmp
       && e->Iex.Mux0X.expr0->tag == Iex_RdTmp
       && e->Iex.Mux0X.exprX->tag == Iex_RdTmp) {
      ae = LibVEX_Alloc(sizeof(AvailExpr));
      ae->tag       = Mttt;
      ae->u.Mttt.co = e->Iex.Mux0X.cond->Iex.RdTmp.tmp;
      ae->u.Mttt.e0 = e->Iex.Mux0X.expr0->Iex.RdTmp.tmp;
      ae->u.Mttt.eX = e->Iex.Mux0X.exprX->Iex.RdTmp.tmp;
      return ae;
   }

   if (e->tag == Iex_GetI
       && e->Iex.GetI.ix->tag == Iex_RdTmp) {
      ae = LibVEX_Alloc(sizeof(AvailExpr));
      ae->tag           = GetIt;
      ae->u.GetIt.descr = e->Iex.GetI.descr;
      ae->u.GetIt.ix    = e->Iex.GetI.ix->Iex.RdTmp.tmp;
      ae->u.GetIt.bias  = e->Iex.GetI.bias;
      return ae;
   }

   return NULL;
}


/* The BB is modified in-place.  Returns True if any changes were
   made. */

static Bool do_cse_BB ( IRSB* bb )
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
      On seeing "t = E", where E is one of the 5 AvailExpr forms:
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
         vulnerable kind of bindings are the GetI kind.
            Dirty call - dump (paranoia level -> 2) 
            Store      - dump (ditto) 
            Put, PutI  - dump unless no-overlap is proven (.. -> 1)
         Uses getAliasingRelation_IC and getAliasingRelation_II
         to do the no-overlap assessments needed for Put/PutI.
      */
      switch (st->tag) {
         case Ist_Dirty: case Ist_Store: case Ist_MBE:
         case Ist_CAS: case Ist_LLSC:
            paranoia = 2; break;
         case Ist_Put: case Ist_PutI: 
            paranoia = 1; break;
         case Ist_NoOp: case Ist_IMark: case Ist_AbiHint: 
         case Ist_WrTmp: case Ist_Exit: 
            paranoia = 0; break;
         default: 
            vpanic("do_cse_BB(1)");
      }

      if (paranoia > 0) {
         for (j = 0; j < aenv->used; j++) {
            if (!aenv->inuse[j])
               continue;
            ae = (AvailExpr*)aenv->key[j];
            if (ae->tag != GetIt) 
               continue;
            invalidate = False;
            if (paranoia >= 2) {
               invalidate = True;
            } else {
               vassert(paranoia == 1);
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
                  if (getAliasingRelation_II(
                         ae->u.GetIt.descr, 
                         IRExpr_RdTmp(ae->u.GetIt.ix), 
                         ae->u.GetIt.bias,
                         st->Ist.PutI.descr,
                         st->Ist.PutI.ix,
                         st->Ist.PutI.bias
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
      eprime = irExpr_to_AvailExpr(st->Ist.WrTmp.data);
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

      if (st->tag == Ist_PutI
          && st->Ist.PutI.ix->tag == Iex_RdTmp
          && collapseChain(bb, i-1, st->Ist.PutI.ix->Iex.RdTmp.tmp, 
                               &var2, &con2)) {
         if (DEBUG_IROPT) {
            vex_printf("replacing2 ");
            ppIRStmt(st);
            vex_printf(" with ");
         }
         con2 += st->Ist.PutI.bias;
         bb->stmts[i]
           = IRStmt_PutI(st->Ist.PutI.descr,
                         IRExpr_RdTmp(var2),
                         con2,
                         st->Ist.PutI.data);
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

         relation = getAliasingRelation_II(
                       descrG, ixG, biasG,
                       st->Ist.PutI.descr,
                       st->Ist.PutI.ix,
                       st->Ist.PutI.bias
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
         return st->Ist.PutI.data;

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

   return toBool(
          getAliasingRelation_II( 
             pi->Ist.PutI.descr, pi->Ist.PutI.ix, pi->Ist.PutI.bias, 
             s2->Ist.PutI.descr, s2->Ist.PutI.ix, s2->Ist.PutI.bias
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
   getArrayBounds(pi->Ist.PutI.descr, &minoffP, &maxoffP);
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
                 pi->Ist.PutI.descr, pi->Ist.PutI.ix,
                 s2->Ist.Put.offset, 
                 typeOfIRExpr(tyenv,s2->Ist.Put.data)
              );
         goto have_relation;

      case Ist_PutI:
         vassert(isIRAtom(s2->Ist.PutI.ix));
         vassert(isIRAtom(s2->Ist.PutI.data));
         relation
            = getAliasingRelation_II(
                 pi->Ist.PutI.descr, pi->Ist.PutI.ix, pi->Ist.PutI.bias, 
                 s2->Ist.PutI.descr, s2->Ist.PutI.ix, s2->Ist.PutI.bias
              );
         goto have_relation;

      case Ist_WrTmp:
         if (s2->Ist.WrTmp.data->tag == Iex_GetI) {
            relation
               = getAliasingRelation_II(
                    pi->Ist.PutI.descr, pi->Ist.PutI.ix, 
                                        pi->Ist.PutI.bias, 
                    s2->Ist.WrTmp.data->Iex.GetI.descr,
                    s2->Ist.WrTmp.data->Iex.GetI.ix,
                    s2->Ist.WrTmp.data->Iex.GetI.bias
                 );
            goto have_relation;
         }
         if (s2->Ist.WrTmp.data->tag == Iex_Get) {
            relation
               = getAliasingRelation_IC(
                    pi->Ist.PutI.descr, pi->Ist.PutI.ix,
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
void do_redundant_PutI_elimination ( IRSB* bb )
{
   Int    i, j;
   Bool   delete;
   IRStmt *st, *stj;

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
         deltaIRExpr(e->Iex.Qop.arg1, delta);
         deltaIRExpr(e->Iex.Qop.arg2, delta);
         deltaIRExpr(e->Iex.Qop.arg3, delta);
         deltaIRExpr(e->Iex.Qop.arg4, delta);
         break;
      case Iex_Triop:
         deltaIRExpr(e->Iex.Triop.arg1, delta);
         deltaIRExpr(e->Iex.Triop.arg2, delta);
         deltaIRExpr(e->Iex.Triop.arg3, delta);
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
      case Iex_Mux0X:
         deltaIRExpr(e->Iex.Mux0X.cond, delta);
         deltaIRExpr(e->Iex.Mux0X.expr0, delta);
         deltaIRExpr(e->Iex.Mux0X.exprX, delta);
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
         deltaIRExpr(st->Ist.PutI.ix, delta);
         deltaIRExpr(st->Ist.PutI.data, delta);
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
         for (i = 0; d->args[i]; i++)
            deltaIRExpr(d->args[i], delta);
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


static IRSB* maybe_loop_unroll_BB ( IRSB* bb0, Addr64 my_addr )
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
      vex_printf("\nUNROLLED (%llx)\n", my_addr);
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

/* bindee == NULL   ===  slot is not in use
   bindee != NULL   ===  slot is in use
*/
typedef
   struct {
      IRTemp  binder;
      IRExpr* bindee;
      Bool    doesLoad;
      Bool    doesGet;
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
static void setHints_Expr (Bool* doesLoad, Bool* doesGet, IRExpr* e )
{
   Int i;
   switch (e->tag) {
      case Iex_CCall:
         for (i = 0; e->Iex.CCall.args[i]; i++)
            setHints_Expr(doesLoad, doesGet, e->Iex.CCall.args[i]);
         return;
      case Iex_Mux0X:
         setHints_Expr(doesLoad, doesGet, e->Iex.Mux0X.cond);
         setHints_Expr(doesLoad, doesGet, e->Iex.Mux0X.expr0);
         setHints_Expr(doesLoad, doesGet, e->Iex.Mux0X.exprX);
         return;
      case Iex_Qop:
         setHints_Expr(doesLoad, doesGet, e->Iex.Qop.arg1);
         setHints_Expr(doesLoad, doesGet, e->Iex.Qop.arg2);
         setHints_Expr(doesLoad, doesGet, e->Iex.Qop.arg3);
         setHints_Expr(doesLoad, doesGet, e->Iex.Qop.arg4);
         return;
      case Iex_Triop:
         setHints_Expr(doesLoad, doesGet, e->Iex.Triop.arg1);
         setHints_Expr(doesLoad, doesGet, e->Iex.Triop.arg2);
         setHints_Expr(doesLoad, doesGet, e->Iex.Triop.arg3);
         return;
      case Iex_Binop:
         setHints_Expr(doesLoad, doesGet, e->Iex.Binop.arg1);
         setHints_Expr(doesLoad, doesGet, e->Iex.Binop.arg2);
         return;
      case Iex_Unop:
         setHints_Expr(doesLoad, doesGet, e->Iex.Unop.arg);
         return;
      case Iex_Load:
         *doesLoad = True;
         setHints_Expr(doesLoad, doesGet, e->Iex.Load.addr);
         return;
      case Iex_Get:
         *doesGet = True;
         return;
      case Iex_GetI:
         *doesGet = True;
         setHints_Expr(doesLoad, doesGet, e->Iex.GetI.ix);
         return;
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
   env[0].doesGet  = False; /* filled in later */
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

      case Iex_Mux0X:
         aoccCount_Expr(uses, e->Iex.Mux0X.cond);
         aoccCount_Expr(uses, e->Iex.Mux0X.expr0);
         aoccCount_Expr(uses, e->Iex.Mux0X.exprX);
         return;

      case Iex_Qop: 
         aoccCount_Expr(uses, e->Iex.Qop.arg1);
         aoccCount_Expr(uses, e->Iex.Qop.arg2);
         aoccCount_Expr(uses, e->Iex.Qop.arg3);
         aoccCount_Expr(uses, e->Iex.Qop.arg4);
         return;

      case Iex_Triop: 
         aoccCount_Expr(uses, e->Iex.Triop.arg1);
         aoccCount_Expr(uses, e->Iex.Triop.arg2);
         aoccCount_Expr(uses, e->Iex.Triop.arg3);
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
         aoccCount_Expr(uses, st->Ist.PutI.ix);
         aoccCount_Expr(uses, st->Ist.PutI.data);
         return;
      case Ist_Store:
         aoccCount_Expr(uses, st->Ist.Store.addr);
         aoccCount_Expr(uses, st->Ist.Store.data);
         return;
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
         for (i = 0; d->args[i]; i++)
            aoccCount_Expr(uses, d->args[i]);
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
      break;
   case Iop_Left32:
      /* Left32( Left32(x) ) --> Left32(x) */
      if (is_Unop(aa, Iop_Left32))
         return IRExpr_Unop( Iop_Left32, aa->Iex.Unop.arg );
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
      case Iex_Mux0X:
         return IRExpr_Mux0X(
                   atbSubst_Expr(env, e->Iex.Mux0X.cond),
                   atbSubst_Expr(env, e->Iex.Mux0X.expr0),
                   atbSubst_Expr(env, e->Iex.Mux0X.exprX)
                );
      case Iex_Qop:
         return IRExpr_Qop(
                   e->Iex.Qop.op,
                   atbSubst_Expr(env, e->Iex.Qop.arg1),
                   atbSubst_Expr(env, e->Iex.Qop.arg2),
                   atbSubst_Expr(env, e->Iex.Qop.arg3),
                   atbSubst_Expr(env, e->Iex.Qop.arg4)
                );
      case Iex_Triop:
         return IRExpr_Triop(
                   e->Iex.Triop.op,
                   atbSubst_Expr(env, e->Iex.Triop.arg1),
                   atbSubst_Expr(env, e->Iex.Triop.arg2),
                   atbSubst_Expr(env, e->Iex.Triop.arg3)
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
         return IRStmt_PutI(
                   st->Ist.PutI.descr,
                   atbSubst_Expr(env, st->Ist.PutI.ix),
                   st->Ist.PutI.bias,
                   atbSubst_Expr(env, st->Ist.PutI.data)
                );

      case Ist_Exit:
         return IRStmt_Exit(
                   atbSubst_Expr(env, st->Ist.Exit.guard),
                   st->Ist.Exit.jk,
                   st->Ist.Exit.dst
                );
      case Ist_IMark:
         return IRStmt_IMark(st->Ist.IMark.addr, st->Ist.IMark.len);
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
         for (i = 0; d2->args[i]; i++)
            d2->args[i] = atbSubst_Expr(env, d2->args[i]);
         return IRStmt_Dirty(d2);
      default: 
         vex_printf("\n"); ppIRStmt(st); vex_printf("\n");
         vpanic("atbSubst_Stmt");
   }
}

/* notstatic */ void ado_treebuild_BB ( IRSB* bb )
{
   Int      i, j, k, m;
   Bool     stmtPuts, stmtStores, invalidateMe;
   IRStmt*  st;
   IRStmt*  st2;
   ATmpInfo env[A_NENV];

   Int       n_tmps = bb->tyenv->types_used;
   UShort*   uses   = LibVEX_Alloc(n_tmps * sizeof(UShort));

   /* Phase 1.  Scan forwards in bb, counting use occurrences of each
      temp.  Also count occurrences in the bb->next field. */

   for (i = 0; i < n_tmps; i++)
      uses[i] = 0;

   for (i = 0; i < bb->stmts_used; i++) {
      st = bb->stmts[i];
      if (st->tag == Ist_NoOp)
         continue;
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
         setHints_Expr(&env[0].doesLoad, &env[0].doesGet, e2);
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

      /* stmtPuts/stmtStores characterise what the stmt under
         consideration does, or might do (sidely safe @ True). */
      stmtPuts
         = toBool( st->tag == Ist_Put
                   || st->tag == Ist_PutI 
                   || st->tag == Ist_Dirty );

      /* be True if this stmt writes memory or might do (==> we don't
         want to reorder other loads or stores relative to it).  Also,
         both LL and SC fall under this classification, since we
         really ought to be conservative and not reorder any other
         memory transactions relative to them. */
      stmtStores
         = toBool( st->tag == Ist_Store
                   || st->tag == Ist_Dirty
                   || st->tag == Ist_LLSC );

      for (k = A_NENV-1; k >= 0; k--) {
         if (env[k].bindee == NULL)
            continue;
         /* Compare the actions of this stmt with the actions of
            binding 'k', to see if they invalidate the binding. */
         invalidateMe
            = toBool(
              /* a store invalidates loaded data */
              (env[k].doesLoad && stmtStores)
              /* a put invalidates get'd data */
              || (env[k].doesGet && stmtPuts)
              /* a put invalidates loaded data.  Note, we could do
                 much better here in the sense that we only need to
                 invalidate trees containing loads if the Put in
                 question is marked as requiring precise
                 exceptions. */
              || (env[k].doesLoad && stmtPuts)
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
         IRExpr* (*specHelper) (HChar*, IRExpr**, IRStmt**, Int),
         Bool (*preciseMemExnsFn)(Int,Int)
      )
{
   redundant_get_removal_BB ( bb );
   if (iropt_verbose) {
      vex_printf("\n========= REDUNDANT GET\n\n" );
      ppIRSB(bb);
   }

   redundant_put_removal_BB ( bb, preciseMemExnsFn );
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
IRSB* expensive_transformations( IRSB* bb )
{
   (void)do_cse_BB( bb );
   collapse_AddSub_chains_BB( bb );
   do_redundant_GetI_elimination( bb );
   do_redundant_PutI_elimination( bb );
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
               case Ity_F32: case Ity_F64: case Ity_V128: 
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
            for (j = 0; d->args[j]; j++)
               vassert(isIRAtom(d->args[j]));
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
         IRExpr* (*specHelper) (HChar*, IRExpr**, IRStmt**, Int),
         Bool (*preciseMemExnsFn)(Int,Int),
         Addr64 guest_addr,
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

   bb = cheap_transformations( bb, specHelper, preciseMemExnsFn );

   if (guest_arch == VexArchARM) {
      /* Translating Thumb2 code produces a lot of chaff.  We have to
         work extra hard to get rid of it. */
      bb = cprop_BB(bb);
      bb = spec_helpers_BB ( bb, specHelper );
      redundant_put_removal_BB ( bb, preciseMemExnsFn );
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
         (void)do_cse_BB( bb );
         do_deadcode_BB( bb );
      }

      if (hasGetIorPutI) {
         Bool cses;
         n_expensive++;
         if (DEBUG_IROPT)
            vex_printf("***** EXPENSIVE %d %d\n", n_total, n_expensive);
         bb = expensive_transformations( bb );
         bb = cheap_transformations( bb, specHelper, preciseMemExnsFn );
         /* Potentially common up GetIs */
         cses = do_cse_BB( bb );
         if (cses)
            bb = cheap_transformations( bb, specHelper, preciseMemExnsFn );
      }

      /* Now have a go at unrolling simple (single-BB) loops.  If
         successful, clean up the results as much as possible. */

      bb2 = maybe_loop_unroll_BB( bb, guest_addr );
      if (bb2) {
         bb = cheap_transformations( bb2, specHelper, preciseMemExnsFn );
         if (hasGetIorPutI) {
            bb = expensive_transformations( bb );
            bb = cheap_transformations( bb, specHelper, preciseMemExnsFn );
         } else {
            /* at least do CSE and dead code removal */
            do_cse_BB( bb );
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
