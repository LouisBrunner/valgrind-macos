
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (ir/iropt.c) is                               ---*/
/*--- Copyright (c) 2004 OpenWorks LLP.  All rights reserved. ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

#include "libvex_basictypes.h"
#include "libvex_ir.h"
#include "libvex.h"

#include "main/vex_util.h"
#include "ir/iropt.h"

/* Set to 1 for lots of debugging output. */
#define DEBUG_IROPT 0


/*---------------------------------------------------------------*/
/*--- Finite mappery, of a sort                               ---*/
/*---------------------------------------------------------------*/

/* General map from 64-bit thing 64-bit thing.  Could be done faster
   by hashing. */

typedef
   struct {
      Bool*  inuse;
      ULong* key;
      ULong* val;
      Int    size;
      Int    used;
   }
   Hash64;

static Hash64* newH64 ( void )
{
   Hash64* h = LibVEX_Alloc(sizeof(Hash64));
   h->size   = 8;
   h->used   = 0;
   h->inuse  = LibVEX_Alloc(h->size * sizeof(Bool));
   h->key    = LibVEX_Alloc(h->size * sizeof(ULong));
   h->val    = LibVEX_Alloc(h->size * sizeof(ULong));
   return h;
}


/* Look up key in the map. */

static Bool lookupH64 ( Hash64* h, /*OUT*/ULong* val, ULong key )
{
   Int i;
   //vex_printf("lookupH64(%llx)\n", key );
   for (i = 0; i < h->used; i++) {
      if (h->inuse[i] && h->key[i] == key) {
         if (val)
            *val = h->val[i];
         return True;
      }
   }
   return False;
}


#if 0
/* Apparently unused */
/* Delete any binding for key in h. */

static void delFromH64 ( Hash64* h, ULong key )
{
   Int i;
   for (i = 0; i < h->used; i++) {
      if (h->inuse[i] && h->key[i] == key) {
         h->inuse[i] = False;
         return;
      }
   }
}
#endif


/* Add key->val to the map.  Replaces any existing binding for key. */

static void addToH64 ( Hash64* h, ULong key, ULong val )
{
   Int i, j;

   //vex_printf("addToH64(%llx, %llx)\n", key, val);
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
      ULong* key2   = LibVEX_Alloc(2 * h->size * sizeof(ULong));
      ULong* val2   = LibVEX_Alloc(2 * h->size * sizeof(ULong));
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
/*--- Flattening out a BB into pure SSA form                  ---*/
/*---------------------------------------------------------------*/

inline
static Bool isAtom ( IRExpr* e )
{
   return e->tag == Iex_Tmp || e->tag == Iex_Const;
}


/* Clone the NULL-terminated vector of IRExpr*s attached to a
   CCall. */

static IRExpr** copyIRExprCallArgs ( IRExpr** vec )
{
   Int      i;
   IRExpr** newvec;
   for (i = 0; vec[i]; i++)
      ;
   newvec = LibVEX_Alloc((i+1)*sizeof(IRExpr*));
   for (i = 0; vec[i]; i++)
      newvec[i] = vec[i];
   newvec[i] = NULL;
   return newvec;
}


/* Non-critical helper, heuristic for reducing the number of tmp-tmp
   copies made by flattening.  If in doubt return False. */

static Bool isFlat ( IRExpr* e )
{
   if (e->tag == Iex_Get) return True;
   if (e->tag == Iex_Binop)
      return isAtom(e->Iex.Binop.arg1) && isAtom(e->Iex.Binop.arg2);
   if (e->tag == Iex_LDle)
      return isAtom(e->Iex.LDle.addr);
   return False;
}

/* Flatten out 'ex' so it is atomic, returning a new expression with
   the same value, after having appended extra IRTemp assignments to
   the end of 'bb'. */

static IRExpr* flatten_Expr ( IRBB* bb, IRExpr* ex )
{
   Int i;
   IRExpr** newargs;
   IRType ty = typeOfIRExpr(bb->tyenv, ex);
   IRTemp t1;

   switch (ex->tag) {

      case Iex_GetI:
         t1 = newIRTemp(bb->tyenv, ty);
         addStmtToIRBB(bb, IRStmt_Tmp(t1,
            IRExpr_GetI(ex->Iex.GetI.descr,
                        flatten_Expr(bb, ex->Iex.GetI.off),
                        ex->Iex.GetI.bias)));
         return IRExpr_Tmp(t1);

      case Iex_Get:
         t1 = newIRTemp(bb->tyenv, ty);
         addStmtToIRBB(bb, 
            IRStmt_Tmp(t1, ex));
         return IRExpr_Tmp(t1);

      case Iex_Binop:
         t1 = newIRTemp(bb->tyenv, ty);
         addStmtToIRBB(bb, IRStmt_Tmp(t1, 
            IRExpr_Binop(ex->Iex.Binop.op,
                         flatten_Expr(bb, ex->Iex.Binop.arg1),
                         flatten_Expr(bb, ex->Iex.Binop.arg2))));
         return IRExpr_Tmp(t1);

      case Iex_Unop:
         t1 = newIRTemp(bb->tyenv, ty);
         addStmtToIRBB(bb, IRStmt_Tmp(t1, 
            IRExpr_Unop(ex->Iex.Unop.op,
                        flatten_Expr(bb, ex->Iex.Unop.arg))));
         return IRExpr_Tmp(t1);

      case Iex_LDle:
         t1 = newIRTemp(bb->tyenv, ty);
         addStmtToIRBB(bb, IRStmt_Tmp(t1,
            IRExpr_LDle(ex->Iex.LDle.ty, 
                        flatten_Expr(bb, ex->Iex.LDle.addr))));
         return IRExpr_Tmp(t1);

      case Iex_CCall:
         newargs = copyIRExprCallArgs(ex->Iex.CCall.args);
         for (i = 0; newargs[i]; i++)
            newargs[i] = flatten_Expr(bb, newargs[i]);
         t1 = newIRTemp(bb->tyenv, ty);
         addStmtToIRBB(bb, IRStmt_Tmp(t1,
            IRExpr_CCall(ex->Iex.CCall.name,
                         ex->Iex.CCall.retty,
                         newargs)));
         return IRExpr_Tmp(t1);

      case Iex_Mux0X:
         t1 = newIRTemp(bb->tyenv, ty);
         addStmtToIRBB(bb, IRStmt_Tmp(t1,
            IRExpr_Mux0X(flatten_Expr(bb, ex->Iex.Mux0X.cond),
                         flatten_Expr(bb, ex->Iex.Mux0X.expr0),
                         flatten_Expr(bb, ex->Iex.Mux0X.exprX))));
         return IRExpr_Tmp(t1);

      case Iex_Const:
      case Iex_Tmp:
         return ex;

      default:
         vex_printf("\n");
         ppIRExpr(ex); 
         vex_printf("\n");
         vpanic("flatten_Expr");
   }
}


/* Append a completely flattened form of 'st' to the end of 'bb'. */

static void flatten_Stmt ( IRBB* bb, IRStmt* st )
{
   Int i;
   IRExpr  *e1, *e2;
   IRDirty *d,  *d2;
   switch (st->tag) {
      case Ist_Put:
         e1 = flatten_Expr(bb, st->Ist.Put.data);
         addStmtToIRBB(bb, IRStmt_Put(st->Ist.Put.offset, e1));
         break;
      case Ist_PutI:
         e1 = flatten_Expr(bb, st->Ist.PutI.off);
         e2 = flatten_Expr(bb, st->Ist.PutI.data);
         addStmtToIRBB(bb, IRStmt_PutI(st->Ist.PutI.descr,
                                       e1,
                                       st->Ist.PutI.bias,
                                       e2));
         break;
      case Ist_Tmp:
         if (isFlat(st->Ist.Tmp.data)) {
            /* optimisation, to reduce the number of tmp-tmp
               copies generated */
            addStmtToIRBB(bb, st);
         } else {
            /* general case, always correct */
            e1 = flatten_Expr(bb, st->Ist.Tmp.data);
            addStmtToIRBB(bb, IRStmt_Tmp(st->Ist.Tmp.tmp, e1));
         }
         break;
      case Ist_STle:
         e1 = flatten_Expr(bb, st->Ist.STle.addr);
         e2 = flatten_Expr(bb, st->Ist.STle.data);
         addStmtToIRBB(bb, IRStmt_STle(e1,e2));
         break;
      case Ist_Dirty:
         d = st->Ist.Dirty.details;
         d2 = emptyIRDirty();
         *d2 = *d;
         d2->args = copyIRExprCallArgs(d2->args);
         if (d2->mFx != Ifx_None) {
            d2->mAddr = flatten_Expr(bb, d2->mAddr);
         } else {
            vassert(d2->mAddr == NULL);
         }
         for (i = 0; d2->args[i]; i++)
            d2->args[i] = flatten_Expr(bb, d2->args[i]);
         addStmtToIRBB(bb, IRStmt_Dirty(d2));
         break;
      case Ist_Exit:
         e1 = flatten_Expr(bb, st->Ist.Exit.cond);
         addStmtToIRBB(bb, IRStmt_Exit(e1, st->Ist.Exit.dst));
         break;
      default:
         vex_printf("\n");
         ppIRStmt(st); 
         vex_printf("\n");
         vpanic("flatten_Stmt");
   }
}

static IRBB* flatten_BB ( IRBB* in )
{
   Int   i;
   IRBB* out;
   out = emptyIRBB();
   out->tyenv = copyIRTypeEnv( in->tyenv );
   for (i = 0; i < in->stmts_used; i++)
      if (in->stmts[i])
         flatten_Stmt( out, in->stmts[i] );
   out->next     = flatten_Expr( out, in->next );
   out->jumpkind = in->jumpkind;
   return out;
}



/*---------------------------------------------------------------*/
/*--- Constant propagation and folding                        ---*/
/*---------------------------------------------------------------*/

/* The env in this section is a map from IRTemp to IRExpr*. */

/* Are both expressions simply the same IRTemp ? */
static Bool sameIRTemps ( IRExpr* e1, IRExpr* e2 )
{
   return e1->tag == Iex_Tmp
          && e2->tag == Iex_Tmp
          && e1->Iex.Tmp.tmp == e2->Iex.Tmp.tmp;
}

static IRExpr* fold_Expr ( IRExpr* e )
{
   Int     shift;
   IRExpr* e2 = e; /* e2 is the result of folding e, if possible */

   /* UNARY ops */
   if (e->tag == Iex_Unop
       && e->Iex.Unop.arg->tag == Iex_Const) {
      switch (e->Iex.Unop.op) {
         case Iop_1Uto32:
            e2 = IRExpr_Const(IRConst_U32(
                    e->Iex.Unop.arg->Iex.Const.con->Ico.Bit
                    ? 1 : 0));
            break;
         case Iop_8Sto32: {
            /* signed */ Int s32 = e->Iex.Unop.arg->Iex.Const.con->Ico.U8;
            s32 <<= 24;
            s32 >>= 24;
            e2 = IRExpr_Const(IRConst_U32((UInt)s32));
            break;
         }
         case Iop_8Uto32:
            e2 = IRExpr_Const(IRConst_U32(
                    0xFF & e->Iex.Unop.arg->Iex.Const.con->Ico.U8));
            break;
         case Iop_16Uto32:
            e2 = IRExpr_Const(IRConst_U32(
                    0xFFFF & e->Iex.Unop.arg->Iex.Const.con->Ico.U16));
            break;
         case Iop_32to8:
            e2 = IRExpr_Const(IRConst_U8(
                    0xFF & e->Iex.Unop.arg->Iex.Const.con->Ico.U32));
            break;
         case Iop_Not32:
            e2 = IRExpr_Const(IRConst_U32(
                    ~ (e->Iex.Unop.arg->Iex.Const.con->Ico.U32)));
            break;
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
            case Iop_Xor8:
               e2 = IRExpr_Const(IRConst_U8(0xFF & 
                       (e->Iex.Binop.arg1->Iex.Const.con->Ico.U8
                        ^ e->Iex.Binop.arg2->Iex.Const.con->Ico.U8)));
               break;
            case Iop_And8:
               e2 = IRExpr_Const(IRConst_U8(0xFF & 
                       (e->Iex.Binop.arg1->Iex.Const.con->Ico.U8
                        & e->Iex.Binop.arg2->Iex.Const.con->Ico.U8)));
               break;
            case Iop_Add8:
               e2 = IRExpr_Const(IRConst_U8(0xFF & 
                       (e->Iex.Binop.arg1->Iex.Const.con->Ico.U8
                        + e->Iex.Binop.arg2->Iex.Const.con->Ico.U8)));
               break;
            case Iop_Sub8:
               e2 = IRExpr_Const(IRConst_U8(0xFF & 
                       (e->Iex.Binop.arg1->Iex.Const.con->Ico.U8
                        - e->Iex.Binop.arg2->Iex.Const.con->Ico.U8)));
               break;
            case Iop_Sub32:
               e2 = IRExpr_Const(IRConst_U32(
                       (e->Iex.Binop.arg1->Iex.Const.con->Ico.U32
                        - e->Iex.Binop.arg2->Iex.Const.con->Ico.U32)));
               break;
            case Iop_Add32:
               e2 = IRExpr_Const(IRConst_U32(
                       (e->Iex.Binop.arg1->Iex.Const.con->Ico.U32
                        + e->Iex.Binop.arg2->Iex.Const.con->Ico.U32)));
               break;
            case Iop_Xor32:
               e2 = IRExpr_Const(IRConst_U32(
                       (e->Iex.Binop.arg1->Iex.Const.con->Ico.U32
                        ^ e->Iex.Binop.arg2->Iex.Const.con->Ico.U32)));
               break;
            case Iop_And32:
               e2 = IRExpr_Const(IRConst_U32(
                       (e->Iex.Binop.arg1->Iex.Const.con->Ico.U32
                        & e->Iex.Binop.arg2->Iex.Const.con->Ico.U32)));
               break;
            case Iop_Or32:
               e2 = IRExpr_Const(IRConst_U32(
                       (e->Iex.Binop.arg1->Iex.Const.con->Ico.U32
                        | e->Iex.Binop.arg2->Iex.Const.con->Ico.U32)));
               break;
            case Iop_Mul32:
               e2 = IRExpr_Const(IRConst_U32(
                       (e->Iex.Binop.arg1->Iex.Const.con->Ico.U32
                        * e->Iex.Binop.arg2->Iex.Const.con->Ico.U32)));
               break;
            case Iop_Shl32:
               vassert(e->Iex.Binop.arg2->Iex.Const.con->tag == Ico_U8);
               shift = (Int)(e->Iex.Binop.arg2->Iex.Const.con->Ico.U8);
               if (shift >= 0 && shift <= 31)
                  e2 = IRExpr_Const(IRConst_U32(
                          (e->Iex.Binop.arg1->Iex.Const.con->Ico.U32
                           << shift)));
               break;
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
            case Iop_Shr32: {
               /* paranoid ... */
               /*unsigned*/ UInt s32;
               vassert(e->Iex.Binop.arg2->Iex.Const.con->tag == Ico_U8);
               s32   = (Int)(e->Iex.Binop.arg1->Iex.Const.con->Ico.U32);
               shift = (Int)(e->Iex.Binop.arg2->Iex.Const.con->Ico.U8);
               if (shift >= 0 && shift <= 31) {
                  s32 >>=/*unsigned*/ shift;
                  e2 = IRExpr_Const(IRConst_U32((UInt)s32));
               }
               break;
            }
            case Iop_CmpEQ32:
               e2 = IRExpr_Const(IRConst_Bit(
                       (e->Iex.Binop.arg1->Iex.Const.con->Ico.U32
                        == e->Iex.Binop.arg2->Iex.Const.con->Ico.U32)));
               break;
            case Iop_32HLto64:
               e2 = IRExpr_Const(IRConst_U64(
                       (((ULong)(e->Iex.Binop.arg1->Iex.Const.con->Ico.U32)) << 32)
                       | ((ULong)(e->Iex.Binop.arg2->Iex.Const.con->Ico.U32)) 
                    ));
               break;
            default:
               goto unhandled;
         }

      } else {

         /* other cases (identities, etc) */
         /* Add32(x,0) ==> x */
         if (e->Iex.Binop.op == Iop_Add32
             && e->Iex.Binop.arg2->tag == Iex_Const
             && e->Iex.Binop.arg2->Iex.Const.con->Ico.U32 == 0) {
            e2 = e->Iex.Binop.arg1;
         } else

         /* And8/16/32(t,t) ==> t, for some IRTemp t */
         if ((e->Iex.Binop.op == Iop_And32
              || e->Iex.Binop.op == Iop_And16
              || e->Iex.Binop.op == Iop_And8)
             && sameIRTemps(e->Iex.Binop.arg1, e->Iex.Binop.arg2)) {
            e2 = e->Iex.Binop.arg1;
         }

      }
   }

   /* Mux0X */
   if (e->tag == Iex_Mux0X
       && e->Iex.Mux0X.cond->tag == Iex_Const) {
      Bool zero;
      /* assured us by the IR type rules */
      vassert(e->Iex.Mux0X.cond->Iex.Const.con->tag == Ico_U8);
      zero = 0 == e->Iex.Mux0X.cond->Iex.Const.con->Ico.U8;
      e2 = zero ? e->Iex.Mux0X.expr0 : e->Iex.Mux0X.exprX;
   }

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
   vex_printf("vex iropt: fold_Expr: no rule for: ");
   ppIRExpr(e);
   vex_printf("\n");
   return e2;
#  endif
}


/* Apply the subst to a simple 1-level expression -- guaranteed to be
   1-level due to previous flattening pass. */

static IRExpr* subst_Expr ( Hash64* env, IRExpr* ex )
{
   if (ex->tag == Iex_Tmp) {
      ULong res;
      if (lookupH64(env, &res, (ULong)ex->Iex.Tmp.tmp)) {
         return (IRExpr*)res;
      } else {
         /* not bound in env */
         return ex;
      }
   }

   if (ex->tag == Iex_Const)
      return ex;
   if (ex->tag == Iex_Get)
      return ex;

   if (ex->tag == Iex_GetI) {
      vassert(isAtom(ex->Iex.GetI.off));
      return IRExpr_GetI(
         ex->Iex.GetI.descr,
         subst_Expr(env, ex->Iex.GetI.off),
         ex->Iex.GetI.bias
      );
   }

   if (ex->tag == Iex_Binop) {
      vassert(isAtom(ex->Iex.Binop.arg1));
      vassert(isAtom(ex->Iex.Binop.arg2));
      return IRExpr_Binop(
                ex->Iex.Binop.op,
                subst_Expr(env, ex->Iex.Binop.arg1),
                subst_Expr(env, ex->Iex.Binop.arg2)
             );
   }

   if (ex->tag == Iex_Unop) {
      vassert(isAtom(ex->Iex.Unop.arg));
      return IRExpr_Unop(
                ex->Iex.Unop.op,
                subst_Expr(env, ex->Iex.Unop.arg)
             );
   }

   if (ex->tag == Iex_LDle) {
      vassert(isAtom(ex->Iex.LDle.addr));
      return IRExpr_LDle(
                ex->Iex.LDle.ty,
                subst_Expr(env, ex->Iex.LDle.addr)
             );
   }

   if (ex->tag == Iex_CCall) {
      Int      i;
      IRExpr** args2 = copyIRExprCallArgs ( ex->Iex.CCall.args );
      for (i = 0; args2[i]; i++) {
         vassert(isAtom(args2[i]));
         args2[i] = subst_Expr(env, args2[i]);
      }
      return IRExpr_CCall(
                ex->Iex.CCall.name,
                ex->Iex.CCall.retty,
                args2 
             );
   }

   if (ex->tag == Iex_Mux0X) {
      vassert(isAtom(ex->Iex.Mux0X.cond));
      vassert(isAtom(ex->Iex.Mux0X.expr0));
      vassert(isAtom(ex->Iex.Mux0X.exprX));
      return IRExpr_Mux0X(
                subst_Expr(env, ex->Iex.Mux0X.cond),
                subst_Expr(env, ex->Iex.Mux0X.expr0),
                subst_Expr(env, ex->Iex.Mux0X.exprX)
             );
   }

   vex_printf("\n\n");
   ppIRExpr(ex);
   vpanic("subst_Expr");
}


/* Apply the subst to stmt, then fold the result as much as possible.
   Much simplified due to stmt being previously flattened.  Returning
   NULL means the statement has been turned into a no-op. */

static IRStmt* subst_and_fold_Stmt ( Hash64* env, IRStmt* st )
{
#  if 0
   vex_printf("\nsubst and fold stmt\n");
   ppIRStmt(st);
   vex_printf("\n");
#  endif

   if (st->tag == Ist_Put) {
      vassert(isAtom(st->Ist.Put.data));
      return IRStmt_Put(
                st->Ist.Put.offset, 
                fold_Expr(subst_Expr(env, st->Ist.Put.data)) 
             );
   }

   if (st->tag == Ist_PutI) {
      vassert(isAtom(st->Ist.PutI.off));
      vassert(isAtom(st->Ist.PutI.data));
      return IRStmt_PutI(
                st->Ist.PutI.descr,
                fold_Expr(subst_Expr(env, st->Ist.PutI.off)),
                st->Ist.PutI.bias,
                fold_Expr(subst_Expr(env, st->Ist.PutI.data))
             );
   }

   if (st->tag == Ist_Tmp) {
      /* This is the one place where an expr (st->Ist.Tmp.data) is
         allowed to be more than just a constant or a tmp. */
      return IRStmt_Tmp(
                st->Ist.Tmp.tmp,
                fold_Expr(subst_Expr(env, st->Ist.Tmp.data))
             );
   }

   if (st->tag == Ist_STle) {
      vassert(isAtom(st->Ist.STle.addr));
      vassert(isAtom(st->Ist.STle.data));
      return IRStmt_STle(
                fold_Expr(subst_Expr(env, st->Ist.STle.addr)),
                fold_Expr(subst_Expr(env, st->Ist.STle.data))
             );
   }

   if (st->tag == Ist_Dirty) {
      Int     i;
      IRDirty *d, *d2;
      d = st->Ist.Dirty.details;
      d2 = emptyIRDirty();
      *d2 = *d;
      d2->args = copyIRExprCallArgs(d2->args);
      if (d2->mFx != Ifx_None) {
         vassert(isAtom(d2->mAddr));
         d2->mAddr = fold_Expr(subst_Expr(env, d2->mAddr));
      }
      for (i = 0; d2->args[i]; i++) {
         vassert(isAtom(d2->args[i]));
         d2->args[i] = fold_Expr(subst_Expr(env, d2->args[i]));
      }
      return IRStmt_Dirty(d2);
   }

   if (st->tag == Ist_Exit) {
      IRExpr* fcond;
      vassert(isAtom(st->Ist.Exit.cond));
      fcond = fold_Expr(subst_Expr(env, st->Ist.Exit.cond));
      if (fcond->tag == Iex_Const) {
         /* Interesting.  The condition on this exit has folded down to
            a constant. */
         vassert(fcond->Iex.Const.con->tag == Ico_Bit);
         if (fcond->Iex.Const.con->Ico.Bit == False) {
            /* exit is never going to happen, so dump the statement. */
            return NULL;
         } else {
            vassert(fcond->Iex.Const.con->Ico.Bit == True);
            /* Hmmm.  The exit has become unconditional.  Leave it as
               it is for now, since we'd have to truncate the BB at
               this point, which is tricky. */
            /* fall out into the reconstruct-the-exit code. */
            vex_printf("vex iropt: IRStmt_Exit became unconditional\n");
         }
      }
      return IRStmt_Exit(fcond,st->Ist.Exit.dst);
   }

   vex_printf("\n");
   ppIRStmt(st);
   vpanic("subst_and_fold_Stmt");
}


static IRBB* cprop_BB ( IRBB* in )
{
   Int     i;
   IRBB*   out;
   Hash64* env;
   IRStmt* st2;

   out = emptyIRBB();
   out->tyenv = copyIRTypeEnv( in->tyenv );

   /* Set up the env with which travels forward.  This holds a
      substitution, mapping IRTemps to atoms, that is, IRExprs which
      are either IRTemps or IRConsts.  Thus, copy and constant
      propagation is done.  The environment is to be applied as we
      move along.  Keys are IRTemps.  Values are IRExpr*s.
   */
   env = newH64();

   /* For each original SSA-form stmt ... */
   for (i = 0; i < in->stmts_used; i++) {

      /* First apply the substitution to the current stmt.  This
         propagates in any constants and tmp-tmp assignments
         accumulated prior to this point.  As part of the subst_Stmt
         call, also then fold any constant expressions resulting. */

      st2 = in->stmts[i];

      /* perhaps st2 is already a no-op? */
      if (!st2) continue;

      st2 = subst_and_fold_Stmt( env, st2 );

      /* If the statement has been folded into a no-op, forget it. */
      if (!st2) continue;

      /* Now consider what the stmt looks like.  If it's of the form
         't = const' or 't1 = t2', add it to the running environment
         and not to the output BB.  Otherwise, add it to the output
         BB. */

      if (st2->tag == Ist_Tmp && st2->Ist.Tmp.data->tag == Iex_Const) {
         /* 't = const' -- add to env.  
             The pair (IRTemp, IRExpr*) is added. */
         addToH64(env, (ULong)(st2->Ist.Tmp.tmp),
                       (ULong)(st2->Ist.Tmp.data) );
      }
      else
      if (st2->tag == Ist_Tmp && st2->Ist.Tmp.data->tag == Iex_Tmp) {
         /* 't1 = t2' -- add to env.  
             The pair (IRTemp, IRExpr*) is added. */
         addToH64(env, (ULong)(st2->Ist.Tmp.tmp),
                       (ULong)(st2->Ist.Tmp.data) );
      }
      else {
         /* Not interesting, copy st2 into the output block. */
         addStmtToIRBB( out, st2 );
      }
   }

   out->next     = subst_Expr( env, in->next );
   out->jumpkind = in->jumpkind;
   return out;
}



/*---------------------------------------------------------------*/
/*--- Dead code (t = E) removal                               ---*/
/*---------------------------------------------------------------*/

/* The type of the Hash64 map is: a map from IRTemp to nothing
   -- really just operating a set or IRTemps.
*/

static void addUses_Temp ( Hash64* set, IRTemp tmp )
{
   addToH64(set, (ULong)tmp, 0);
}

static void addUses_Expr ( Hash64* set, IRExpr* e )
{
   Int i;
   switch (e->tag) {
      case Iex_GetI:
         addUses_Expr(set, e->Iex.GetI.off);
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
      case Iex_LDle:
         addUses_Expr(set, e->Iex.LDle.addr);
         return;
      case Iex_Binop:
         addUses_Expr(set, e->Iex.Binop.arg1);
         addUses_Expr(set, e->Iex.Binop.arg2);
         return;
      case Iex_Unop:
         addUses_Expr(set, e->Iex.Unop.arg);
         return;
      case Iex_Tmp:
         addUses_Temp(set, e->Iex.Tmp.tmp);
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

static void addUses_Stmt ( Hash64* set, IRStmt* st )
{
   Int      i;
   IRDirty* d;
   switch (st->tag) {
      case Ist_PutI:
         addUses_Expr(set, st->Ist.PutI.off);
         addUses_Expr(set, st->Ist.PutI.data);
         return;
      case Ist_Tmp:
         addUses_Expr(set, st->Ist.Tmp.data);
         return;
      case Ist_Put:
         addUses_Expr(set, st->Ist.Put.data);
         return;
      case Ist_STle:
         addUses_Expr(set, st->Ist.STle.addr);
         addUses_Expr(set, st->Ist.STle.data);
         return;
      case Ist_Dirty:
         d = st->Ist.Dirty.details;
         if (d->mFx != Ifx_None)
            addUses_Expr(set, d->mAddr);
         for (i = 0; d->args[i] != NULL; i++)
            addUses_Expr(set, d->args[i]);
         return;
      case Ist_Exit:
         addUses_Expr(set, st->Ist.Exit.cond);
         return;
      default:
         vex_printf("\n");
         ppIRStmt(st);
         vpanic("addUses_Stmt");
      }
}



/* Note, this destructively modifies the given IRBB. */

/* Scan backwards through statements, carrying a set of IRTemps which
   are known to be used after the current point.  On encountering 't =
   E', delete the binding if it is not used.  Otherwise, add any temp
   uses to the set and keep on moving backwards. */

static void dead_BB ( IRBB* bb )
{
   Int     i;
   Hash64* set = newH64();
   IRStmt* st;

   /* start off by recording IRTemp uses in the next field. */
   addUses_Expr(set, bb->next);

   /* Work backwards through the stmts */
   for (i = bb->stmts_used-1; i >= 0; i--) {
      st = bb->stmts[i];
      if (!st)
         continue;
      if (st->tag == Ist_Tmp
          && !lookupH64(set, NULL, (ULong)(st->Ist.Tmp.tmp))) {
          /* it's an IRTemp which never got used.  Delete it. */
         if (DEBUG_IROPT) {
            vex_printf("DEAD: ");
            ppIRStmt(st);
            vex_printf("\n");
         }
         bb->stmts[i] = NULL;
      } else {
         /* Note any IRTemp uses made by the current statement. */
         addUses_Stmt(set, st);
      }
   }
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

static void getArrayBounds ( IRArray* descr, UInt* minoff, UInt* maxoff )
{
   *minoff = descr->base;
   *maxoff = *minoff + descr->nElems*sizeofIRType(descr->elemTy) - 1;
   vassert((*minoff & 0xFFFF0000) == 0);
   vassert((*maxoff & 0xFFFF0000) == 0);
   vassert(*minoff <= *maxoff);
}

/* Create keys, of the form ((minoffset << 16) | maxoffset). */

static UInt mk_key_GetPut ( Int offset, IRType ty )
{
   /* offset should fit in 16 bits. */
   UInt minoff = offset;
   UInt maxoff = minoff + sizeofIRType(ty) - 1;
   vassert((minoff & 0xFFFF0000) == 0);
   vassert((maxoff & 0xFFFF0000) == 0);
   return (minoff << 16) | maxoff;
}

static UInt mk_key_GetIPutI ( IRArray* descr )
{
   UInt minoff, maxoff;
   getArrayBounds( descr, &minoff, &maxoff );
   vassert((minoff & 0xFFFF0000) == 0);
   vassert((maxoff & 0xFFFF0000) == 0);
   return (minoff << 16) | maxoff;
}

/* Supposing h has keys of the form generated by mk_key_GetPut and
   mk_key_GetIPutI, invalidate any key which overlaps (k_lo
   .. k_hi). 
*/

static void invalidateOverlaps ( Hash64* h, UInt k_lo, UInt k_hi )
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


static void redundant_get_removal_BB ( IRBB* bb )
{
   Hash64* env = newH64();
   UInt    key = 0; /* keep gcc -O happy */
   Int     i;
   Bool    isPut;
   ULong   val;

   for (i = 0; i < bb->stmts_used; i++) {
      IRStmt* st = bb->stmts[i];

      if (!st)
         continue;

      /* Deal with Gets */
      if (st->tag == Ist_Tmp
          && st->Ist.Tmp.data->tag == Iex_Get) {
         /* st is 't = Get(...)'.  Look up in the environment and see
            if the Get can be replaced. */
         IRExpr* get = st->Ist.Tmp.data;
         key = (ULong)mk_key_GetPut( get->Iex.Get.offset, 
                                     get->Iex.Get.ty );
         if (lookupH64(env, &val, (ULong)key)) {
            /* found it */
            if (DEBUG_IROPT) {
               vex_printf("rGET: "); ppIRExpr(get);
               vex_printf("  ->  "); ppIRExpr((IRExpr*)val);
               vex_printf("\n");
            }
            bb->stmts[i] = IRStmt_Tmp(st->Ist.Tmp.tmp,
                                      (IRExpr*)val);
         } else {
            /* Not found, but at least we know that t and the Get(...)
               are now associated.  So add a binding to reflect that
               fact. */
            addToH64( env, (ULong)key, 
                           (ULong)(IRExpr_Tmp(st->Ist.Tmp.tmp)) );
         }
      }

      /* Deal with Puts */
      switch (st->tag) {
         case Ist_Put: 
            isPut = True;
            key = mk_key_GetPut( st->Ist.Put.offset, 
                                 typeOfIRExpr(bb->tyenv,st->Ist.Put.data) );
            break;
         case Ist_PutI:
            isPut = True;
            key = mk_key_GetIPutI( st->Ist.PutI.descr );
            break;
         default: 
            isPut = False;
      }

      /* invalidate any env entries overlapped by this Put */
      if (isPut) {
         UInt k_lo, k_hi;
         k_lo = (key >> 16) & 0xFFFF;
         k_hi = key & 0xFFFF;
         invalidateOverlaps(env, k_lo, k_hi);
      }

      /* add this one to the env, if appropriate */
      if (st->tag == Ist_Put) {
         vassert(isAtom(st->Ist.Put.data));
         addToH64( env, (ULong)key, (ULong)(st->Ist.Put.data));
      }

   } /* for (i = 0; i < bb->stmts_used; i++) */

}


/*---------------------------------------------------------------*/
/*--- In-place removal of redundant PUTs                      ---*/
/*---------------------------------------------------------------*/

/* Find any Get uses in st and invalidate any partially or fully
   overlapping ranges listed in env.  Due to the flattening phase, the
   only stmt kind we expect to find a Get on is IRStmt_Tmp. */

static void handle_gets_Stmt ( Hash64* env, IRStmt* st )
{
   UInt    key = 0; /* keep gcc -O happy */
   Bool    isGet;
   IRExpr* e;
   switch (st->tag) {

      /* This is the only interesting case.  Deal with Gets in the RHS
         expression. */
      case Ist_Tmp:
         e = st->Ist.Tmp.data;
         switch (e->tag) {
            case Iex_Get:
               isGet = True;
               key = mk_key_GetPut ( e->Iex.Get.offset, e->Iex.Get.ty );
               break;
            case Iex_GetI:
               isGet = True;
               key = mk_key_GetIPutI ( e->Iex.GetI.descr );
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

      /* all other cases are boring. */
      case Ist_STle:
         vassert(isAtom(st->Ist.STle.addr));
         vassert(isAtom(st->Ist.STle.data));
         return;

      case Ist_Dirty:
         return;

      case Ist_Exit:
         vassert(isAtom(st->Ist.Exit.cond));
         return;

      case Ist_PutI:
         vassert(isAtom(st->Ist.PutI.off));
         vassert(isAtom(st->Ist.PutI.data));
         return;

      default:
         vex_printf("\n");
         ppIRStmt(st);
         vex_printf("\n");
         vpanic("handle_gets_Stmt");
   }
}


/* Scan backwards, building up a set of (min offset, max
   offset) pairs, indicating those parts of the guest state
   for which the next event is a write.

   On seeing a conditional exit, empty the set.

   On seeing 'Put (minoff,maxoff) = t or c', if (minoff,maxoff) is
   completely within the set, remove the Put.  Otherwise, add
   (minoff,maxoff) to the set.

   On seeing 'Get (minoff,maxoff)', remove any part of the set
   overlapping (minoff,maxoff).
*/

static void redundant_put_removal_BB ( IRBB* bb )
{
   Int     i, j;
   Bool    isPut;
   IRStmt* st;
   UInt    key = 0; /* keep gcc -O happy */

   Hash64* env = newH64();
   for (i = bb->stmts_used-1; i >= 0; i--) {
      st = bb->stmts[i];

      /* Deal with conditional exits. */
      if (st->tag == Ist_Exit) {
         /* Since control may not get beyond this point, we must empty
            out the set, since we can no longer claim that the next
            event for any part of the guest state is definitely a
            write. */
         vassert(isAtom(st->Ist.Exit.cond));
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
            vassert(isAtom(st->Ist.Put.data));
            break;
         case Ist_PutI:
            isPut = True;
            key = mk_key_GetIPutI( st->Ist.PutI.descr );
            vassert(isAtom(st->Ist.PutI.off));
            vassert(isAtom(st->Ist.PutI.data));
            break;
         default: 
            isPut = False;
      }
      if (isPut && st->tag != Ist_PutI) {
         /* See if any single entry in env overlaps this Put.  This is
            simplistic in that the transformation is valid if, say, two
            or more entries in the env overlap this Put, but the use of
            lookupH64 will only find a single entry which exactly
            overlaps this Put.  This is suboptimal but safe. */
         if (lookupH64(env, NULL, (ULong)key)) {
            /* This Put is redundant because a later one will overwrite
               it.  So NULL (nop) it out. */
            if (DEBUG_IROPT) {
               vex_printf("rPUT: "); ppIRStmt(st);
               vex_printf("\n");
            }
            bb->stmts[i] = NULL;
         } else {
            /* We can't demonstrate that this Put is redundant, so add it
               to the running collection. */
            addToH64(env, (ULong)key, 0);
         }
         continue;
      }

      /* Deal with Gets.  These remove bits of the environment since
         appearance of a Get means that the next event for that slice
         of the guest state is no longer a write, but a read. */
      handle_gets_Stmt( env, st );
   }
}


/*---------------------------------------------------------------*/
/*--- Specialisation of helper function calls, in             ---*/
/*--- collaboration with the front end                        ---*/
/*---------------------------------------------------------------*/

static 
void spec_helpers_BB ( IRBB* bb,
                       IRExpr* (*specHelper) ( Char*, IRExpr**) )   
{
   Int i;
   IRStmt* st;
   IRExpr* ex;

   for (i = bb->stmts_used-1; i >= 0; i--) {
      st = bb->stmts[i];

      if (!st 
          || st->tag != Ist_Tmp
          || st->Ist.Tmp.data->tag != Iex_CCall)
        continue;

      ex = (*specHelper)( st->Ist.Tmp.data->Iex.CCall.name,
                          st->Ist.Tmp.data->Iex.CCall.args );
      if (!ex)
        /* the front end can't think of a suitable replacement */
        continue;

      /* We got something better.  Install it in the bb. */
      bb->stmts[i]
         = IRStmt_Tmp(st->Ist.Tmp.tmp, ex);

      if (0) {
         vex_printf("SPEC: ");
         ppIRExpr(st->Ist.Tmp.data);
         vex_printf("  -->  ");
         ppIRExpr(ex);
         vex_printf("\n");
      }
   }
}


/*---------------------------------------------------------------*/
/*--- The tree builder                                        ---*/
/*---------------------------------------------------------------*/

typedef
   struct { 
      Int     occ;          /* occurrence count for this tmp */
      IRExpr* expr;         /* expr it is bound to, 
                               or NULL if already 'used' */
      Bool    eDoesLoad;    /* True <=> expr reads mem */
      Bool    eDoesGet;     /* True <=> expr reads guest state */
      Bool    invalidateMe; /* used when dumping bindings */
      Int     origPos;      /* posn of the binder in the original bb */
   }
   TmpInfo;

/* Given env :: IRTemp -> TmpInfo*
   Add the use-occurrences of temps in this expression 
   to the environment. 
*/
static void occCount_Temp ( Hash64* env, IRTemp tmp )
{
   ULong    res;
   TmpInfo* ti;
   if (lookupH64(env, &res, (ULong)tmp)) {
      ti = (TmpInfo*)res;
      ti->occ++;
   } else {
      ti               = LibVEX_Alloc(sizeof(TmpInfo));
      ti->occ          = 1;
      ti->expr         = NULL;
      ti->eDoesLoad    = False;
      ti->eDoesGet     = False;
      ti->invalidateMe = False;
      ti->origPos      = -1; /* filed in properly later */
      addToH64(env, (ULong)tmp, (ULong)ti );
   }
}

static void occCount_Expr ( Hash64* env, IRExpr* e )
{
   Int i;

   switch (e->tag) {

      case Iex_Tmp: /* the only interesting case */
         occCount_Temp(env, e->Iex.Tmp.tmp);
         return;

      case Iex_Mux0X:
         occCount_Expr(env, e->Iex.Mux0X.cond);
         occCount_Expr(env, e->Iex.Mux0X.expr0);
         occCount_Expr(env, e->Iex.Mux0X.exprX);
         return;

      case Iex_Binop: 
         occCount_Expr(env, e->Iex.Binop.arg1);
         occCount_Expr(env, e->Iex.Binop.arg2);
         return;

      case Iex_Unop: 
         occCount_Expr(env, e->Iex.Unop.arg);
         return;

      case Iex_LDle: 
         occCount_Expr(env, e->Iex.LDle.addr);
         return;

      case Iex_CCall:
         for (i = 0; e->Iex.CCall.args[i]; i++)
            occCount_Expr(env, e->Iex.CCall.args[i]);
         return;

      case Iex_GetI:
         occCount_Expr(env, e->Iex.GetI.off);
         return;

      case Iex_Const:
      case Iex_Get:
         return;

      default: 
         vex_printf("\n"); ppIRExpr(e); vex_printf("\n");
         vpanic("occCount_Expr");
    }
}


/* Given env :: IRTemp -> TmpInfo*
   Add the use-occurrences of temps in this expression 
   to the environment. 
*/
static void occCount_Stmt ( Hash64* env, IRStmt* st )
{
   Int      i;
   IRDirty* d;
   switch (st->tag) {
      case Ist_Tmp: 
         occCount_Expr(env, st->Ist.Tmp.data); 
         return; 
      case Ist_Put: 
         occCount_Expr(env, st->Ist.Put.data);
         return;
      case Ist_PutI:
         occCount_Expr(env, st->Ist.PutI.off);
         occCount_Expr(env, st->Ist.PutI.data);
         return;
      case Ist_STle: 
         occCount_Expr(env, st->Ist.STle.addr);
         occCount_Expr(env, st->Ist.STle.data);
         return;
      case Ist_Dirty:
         d = st->Ist.Dirty.details;
         if (d->mFx != Ifx_None)
            occCount_Expr(env, d->mAddr);
         for (i = 0; d->args[i]; i++)
            occCount_Expr(env, d->args[i]);
         return;
      case Ist_Exit:
         occCount_Expr(env, st->Ist.Exit.cond);
         return;
      default: 
         vex_printf("\n"); ppIRStmt(st); vex_printf("\n");
         vpanic("occCount_Stmt");
   }
}

/* Look up a binding for tmp in the env.  If found, return the bound
   expression, and set the env's binding to NULL so it is marked as
   used.  If not found, return NULL. */

static IRExpr* tbSubst_Temp ( Hash64* env, IRTemp tmp )
{
   TmpInfo* ti;
   ULong    res;
   IRExpr*  e;
   if (lookupH64(env, &res, (ULong)tmp)) {
      ti = (TmpInfo*)res;
      e  = ti->expr;
      if (e) {
         ti->expr = NULL;
         return e;
      } else {
         return NULL;
      }
   } else {
      return NULL;
   }
}

/* Traverse e, looking for temps.  For each observed temp, see if env
   contains a binding for the temp, and if so return the bound value.
   The env has the property that any binding it holds is
   'single-shot', so once a binding is used, it is marked as no longer
   available, by setting its .expr field to NULL. */

static IRExpr* tbSubst_Expr ( Hash64* env, IRExpr* e )
{
   IRExpr*  e2;
   IRExpr** args2;
   Int      i;

   switch (e->tag) {

      case Iex_CCall:
         args2 = copyIRExprCallArgs(e->Iex.CCall.args);
         for (i = 0; args2[i]; i++)
            args2[i] = tbSubst_Expr(env,args2[i]);
         return IRExpr_CCall(e->Iex.CCall.name,
                   e->Iex.CCall.retty,
                   args2
                );
      case Iex_Tmp:
         e2 = tbSubst_Temp(env, e->Iex.Tmp.tmp);
         return e2 ? e2 : e;
      case Iex_Mux0X:
         return IRExpr_Mux0X(
                   tbSubst_Expr(env, e->Iex.Mux0X.cond),
                   tbSubst_Expr(env, e->Iex.Mux0X.expr0),
                   tbSubst_Expr(env, e->Iex.Mux0X.exprX)
                );
      case Iex_Binop:
         return IRExpr_Binop(
                   e->Iex.Binop.op,
                   tbSubst_Expr(env, e->Iex.Binop.arg1),
                   tbSubst_Expr(env, e->Iex.Binop.arg2)
                );
      case Iex_Unop:
         return IRExpr_Unop(
                   e->Iex.Unop.op,
                   tbSubst_Expr(env, e->Iex.Unop.arg)
                );
      case Iex_LDle:
         return IRExpr_LDle(
                   e->Iex.LDle.ty,
                  tbSubst_Expr(env, e->Iex.LDle.addr)
                );
      case Iex_GetI:
         return IRExpr_GetI(
                   e->Iex.GetI.descr,
                   tbSubst_Expr(env, e->Iex.GetI.off),
                   e->Iex.GetI.bias
                );
      case Iex_Const:
      case Iex_Get:
         return e;
      default: 
         vex_printf("\n"); ppIRExpr(e); vex_printf("\n");
         vpanic("tbSubst_Expr");
   }
}

/* Same deal as tbSubst_Expr, except for stmts. */

static IRStmt* tbSubst_Stmt ( Hash64* env, IRStmt* st )
{
   Int      i;
   IRDirty* d;
   IRDirty* d2;
   switch (st->tag) {
      case Ist_STle:
         return IRStmt_STle(
                   tbSubst_Expr(env, st->Ist.STle.addr),
                   tbSubst_Expr(env, st->Ist.STle.data)
                );
      case Ist_Tmp:
         return IRStmt_Tmp(
                   st->Ist.Tmp.tmp,
                   tbSubst_Expr(env, st->Ist.Tmp.data)
                );
      case Ist_Put:
         return IRStmt_Put(
                   st->Ist.Put.offset,
                   tbSubst_Expr(env, st->Ist.Put.data)
                );
      case Ist_PutI:
         return IRStmt_PutI(
                   st->Ist.PutI.descr,
                   tbSubst_Expr(env, st->Ist.PutI.off),
                   st->Ist.PutI.bias,
                   tbSubst_Expr(env, st->Ist.PutI.data)
                );

      case Ist_Exit:
         return IRStmt_Exit(
                   tbSubst_Expr(env, st->Ist.Exit.cond),
                   st->Ist.Exit.dst
                );
      case Ist_Dirty:
         d = st->Ist.Dirty.details;
         d2 = emptyIRDirty();
         *d2 = *d;
         if (d2->mFx != Ifx_None)
            d2->mAddr = tbSubst_Expr(env, d2->mAddr);
         for (i = 0; d2->args[i]; i++)
            d2->args[i] = tbSubst_Expr(env, d2->args[i]);
         return IRStmt_Dirty(d2);
      default: 
         vex_printf("\n"); ppIRStmt(st); vex_printf("\n");
         vpanic("tbSubst_Stmt");
   }
}


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
      case Iex_Binop:
         setHints_Expr(doesLoad, doesGet, e->Iex.Binop.arg1);
         setHints_Expr(doesLoad, doesGet, e->Iex.Binop.arg2);
         return;
      case Iex_Unop:
         setHints_Expr(doesLoad, doesGet, e->Iex.Unop.arg);
         return;
      case Iex_LDle:
         *doesLoad |= True;
         setHints_Expr(doesLoad, doesGet, e->Iex.LDle.addr);
         return;
      case Iex_Get:
         *doesGet |= True;
         return;
      case Iex_GetI:
         *doesGet |= True;
         setHints_Expr(doesLoad, doesGet, e->Iex.GetI.off);
         return;
      case Iex_Tmp:
      case Iex_Const:
         return;
      default: 
         vex_printf("\n"); ppIRExpr(e); vex_printf("\n");
         vpanic("setHints_Expr");
   }
}


static void dumpInvalidated ( Hash64* env, IRBB* bb, /*INOUT*/Int* j )
{
   Int k, oldest_op, oldest_k;
   TmpInfo* ti;

   /* Dump all the bindings to marked as invalidated, in order. */
   while (True) {
  
      /* find the oldest bind marked 'invalidateMe'. */
      oldest_op = 1<<30;
      oldest_k =  1<<30;
      for (k = 0;  k < env->used; k++) {
         if (!env->inuse[k])
            continue;
         ti = (TmpInfo*)(env->val[k]);
         if (!ti->expr)
            continue;
         if (!ti->invalidateMe)
           continue;
         /* vex_printf("FOUND INVAL %d %d\n", ti->origPos, oldest_op); */
         if (ti->origPos < oldest_op) {
            oldest_op = ti->origPos;
            oldest_k = k;
         }
      }

      /* No more binds to invalidate. */
      if (oldest_op == 1<<30)
        return;

      /* the oldest bind to invalidate has been identified */
      vassert(oldest_op != 1<<31 && oldest_k != 1<<31);
      ti = (TmpInfo*)(env->val[oldest_k]);
      vassert(ti->expr && ti->invalidateMe);

      /* and invalidate it ... */
      bb->stmts[*j] = IRStmt_Tmp( (IRTemp)(env->key[oldest_k]), ti->expr);
      /*  vex_printf("**1  "); ppIRStmt(bb->stmts[*j]); vex_printf("\n"); */
      (*j)++;
      ti->invalidateMe = False;
      ti->expr = NULL; /* no longer available for substitution */

   } /* loop which dumps the binds marked for invalidation */
}



static void treebuild_BB ( IRBB* bb )
{
   Int      i, j, k;
   ULong    res;
   Bool     invPut, invStore;
   IRStmt*  st;
   IRStmt*  st2;
   TmpInfo* ti;
   IRExpr*  next2;

   /* Mapping from IRTemp to TmpInfo*. */
   Hash64* env = newH64();

   /* Phase 1.  Scan forwards in bb, counting use occurrences of each
      temp.  Also count occurrences in the bb->next field. */

   for (i = 0; i < bb->stmts_used; i++) {
      st = bb->stmts[i];
      if (!st)
         continue;
      occCount_Stmt( env, st );
   }
   occCount_Expr(env, bb->next );

#if 0
   for (i = 0; i < env->used; i++) {
      if (!env->inuse[i])
        continue;
      ppIRTemp( (IRTemp)(env->key[i]) );
      vex_printf("  used %d\n", ((TmpInfo*)env->val[i])->occ );
   }
#endif

   /* Phase 2.  Fill in the origPos fields. */

   for (i = 0; i < bb->stmts_used; i++) {
      st = bb->stmts[i];
      if (!st)
         continue;
      if (st->tag != Ist_Tmp)
         continue;

      if (!lookupH64(env, &res, (ULong)(st->Ist.Tmp.tmp))) {
         vex_printf("\n");
         ppIRTemp(st->Ist.Tmp.tmp);
         vex_printf("\n");
         vpanic("treebuild_BB (phase 2): unmapped IRTemp");
      }
      ti = (TmpInfo*)res;
      ti->origPos = i;
   }

   /* Phase 3.  Scan forwards in bb.  

      On seeing 't = E', occ(t)==1,  
            let E'=env(E), set t's binding to be E', and
            delete this stmt.
            Also examine E' and set the hints for E' appropriately
              (doesLoad? doesGet?)

      On seeing any other stmt, 
            let stmt' = env(stmt)
            remove from env any 't=E' binds invalidated by stmt
                emit the invalidated stmts
            emit stmt'

      Apply env to bb->next.
   */

   /* The stmts in bb are being reordered, and we are guaranteed to
      end up with no more than the number we started with.  Use i to
      be the cursor of the current stmt examined and j <= i to be that
      for the current stmt being written. 
   */
   j = 0;
   for (i = 0; i < bb->stmts_used; i++) {
      st = bb->stmts[i];
      if (!st)
         continue;
     
      if (st->tag == Ist_Tmp) {
         /* vex_printf("acquire binding\n"); */
         if (!lookupH64(env, &res, (ULong)(st->Ist.Tmp.tmp))) {
            vpanic("treebuild_BB (phase 2): unmapped IRTemp");
         }
         ti = (TmpInfo*)res;
         if (ti->occ == 1) {
            /* ok, we have 't = E', occ(t)==1.  Do the abovementioned actions. */
            IRExpr* e = st->Ist.Tmp.data;
            IRExpr* e2 = tbSubst_Expr(env, e);
            ti->expr = e2;
            ti->eDoesLoad = ti->eDoesGet = False;
            setHints_Expr(&ti->eDoesLoad, &ti->eDoesGet, e2);
            /* don't advance j, as we are deleting this stmt and instead
               holding it temporarily in the env. */
            continue; /* the for (i = 0; i < bb->stmts_used; i++) loop */
         }
      }

      /* we get here for any other kind of statement. */
      /* 'use up' any bindings required by the current statement. */
      st2 = tbSubst_Stmt(env, st);

      /* Now, before this stmt, dump any bindings it invalidates.
         These need to be dumped in the order in which they originally
         appeared.  (Stupid algorithm): first, mark all bindings which
         need to be dumped.  Then, dump them in the order in which
         they were defined. */
      invPut = st->tag == Ist_Put 
               || st->tag == Ist_PutI || st->tag == Ist_Dirty;
      invStore = st->tag == Ist_STle
               || st->tag == Ist_Dirty;

      for (k = 0; k < env->used; k++) {
         if (!env->inuse[k])
            continue;
         ti = (TmpInfo*)(env->val[k]);
         if (!ti->expr)
            continue;

         /* We have to invalidate this binding. */
         ti->invalidateMe 
            = (ti->eDoesLoad && invStore) || (ti->eDoesGet && invPut);
         /*
         if (ti->invalidateMe)
           vex_printf("SET INVAL\n");
           */
      }

      dumpInvalidated ( env, bb, &j );

      /* finally, emit the substituted statement */
      bb->stmts[j] = st2;
      /*  vex_printf("**2  "); ppIRStmt(bb->stmts[j]); vex_printf("\n"); */
      j++;

      vassert(j <= i+1);
   } /* for each stmt in the original bb ... */

   /* Finally ... substitute the ->next field as much as possible, and
      dump any left-over bindings.  Hmm.  Perhaps there should be no
      left over bindings?  Or any left-over bindings are
      by definition dead? */
   next2 = tbSubst_Expr(env, bb->next);
   bb->next = next2;
   bb->stmts_used = j;
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
      enum { Ut, Btt, Btc } tag;
      union {
         /* unop(tmp) */
         struct {
            IROp op;
            IRTemp arg;
         } Ut;
         struct {
            IROp op;
            IRTemp arg1;
            IRTemp arg2;
         } Btt;
         struct {
            IROp op;
            IRTemp arg1;
            IRConst con2;
         } Btc;
      } u;
   }
   AvailExpr;

static Bool eq_AvailExpr ( AvailExpr* a1, AvailExpr* a2 )
{
   if (a1->tag != a2->tag)
      return False;
   switch (a1->tag) {
      case Ut: return a1->u.Ut.op == a2->u.Ut.op 
                      && a1->u.Ut.arg == a2->u.Ut.arg;
      case Btt: return a1->u.Btt.op == a2->u.Btt.op
                       && a1->u.Btt.arg1 == a2->u.Btt.arg1
                       && a1->u.Btt.arg2 == a2->u.Btt.arg2;
      case Btc: return a1->u.Btc.op == a2->u.Btc.op
                       && a1->u.Btc.arg1 == a2->u.Btc.arg1
                       && eqIRConst(&a1->u.Btc.con2, &a2->u.Btc.con2);
      default: vpanic("eq_AvailExpr");
   }
}

static IRExpr* availExpr_to_IRExpr ( AvailExpr* ae ) 
{
   IRConst* con;
   switch (ae->tag) {
      case Ut:
         return IRExpr_Unop( ae->u.Ut.op, IRExpr_Tmp(ae->u.Ut.arg) );
      case Btt:
         return IRExpr_Binop( ae->u.Btt.op,
                              IRExpr_Tmp(ae->u.Btt.arg1),
                              IRExpr_Tmp(ae->u.Btt.arg2) );
      case Btc:
         con = LibVEX_Alloc(sizeof(IRConst));
         *con = ae->u.Btc.con2;
         return IRExpr_Binop( ae->u.Btc.op,
                              IRExpr_Tmp(ae->u.Btc.arg1), IRExpr_Const(con) );
      default:
         vpanic("availExpr_to_IRExpr");
   }
}

inline
static IRTemp subst_AvailExpr_Temp ( Hash64* env, IRTemp tmp )
{
   ULong res;
   /* env :: IRTemp -> IRTemp */
   if (lookupH64( env, &res, (ULong)tmp ))
      return (IRTemp)res;
   else
      return tmp;
}

static void subst_AvailExpr ( Hash64* env, AvailExpr* ae )
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
      default: 
         vpanic("subst_AvailExpr");
   }
}

static AvailExpr* irExpr_to_AvailExpr ( IRExpr* e )
{
   AvailExpr* ae;

   if (e->tag == Iex_Unop
       && e->Iex.Unop.arg->tag == Iex_Tmp) {
      ae = LibVEX_Alloc(sizeof(AvailExpr));
      ae->tag      = Ut;
      ae->u.Ut.op  = e->Iex.Unop.op;
      ae->u.Ut.arg = e->Iex.Unop.arg->Iex.Tmp.tmp;
      return ae;
   }

   if (e->tag == Iex_Binop
       && e->Iex.Binop.arg1->tag == Iex_Tmp
       && e->Iex.Binop.arg2->tag == Iex_Tmp) {
      ae = LibVEX_Alloc(sizeof(AvailExpr));
      ae->tag        = Btt;
      ae->u.Btt.op   = e->Iex.Binop.op;
      ae->u.Btt.arg1 = e->Iex.Binop.arg1->Iex.Tmp.tmp;
      ae->u.Btt.arg2 = e->Iex.Binop.arg2->Iex.Tmp.tmp;
      return ae;
   }

   if (e->tag == Iex_Binop
      && e->Iex.Binop.arg1->tag == Iex_Tmp
      && e->Iex.Binop.arg2->tag == Iex_Const) {
      ae = LibVEX_Alloc(sizeof(AvailExpr));
      ae->tag        = Btc;
      ae->u.Btc.op   = e->Iex.Binop.op;
      ae->u.Btc.arg1 = e->Iex.Binop.arg1->Iex.Tmp.tmp;
      ae->u.Btc.con2 = *(e->Iex.Binop.arg2->Iex.Const.con);
      return ae;
   }

   return NULL;
}


/* The BB is modified in-place. */

static void cse_BB ( IRBB* bb )
{
   Int        i, j;
   IRTemp     t, q;
   IRStmt*    st;
   AvailExpr* eprime;

   Hash64* tenv = newH64(); /* :: IRTemp -> IRTemp */
   Hash64* aenv = newH64(); /* :: AvailExpr -> IRTemp */

   //ppIRBB(bb);
   //vex_printf("\n\n");

   /* Iterate forwards over the stmts.  
      On seeing "t = E", where E is one of the 3 AvailExpr forms:
         let E' = apply tenv substitution to E
         search aenv for E'
            if a mapping E' -> q is found, 
               replace this stmt by "t = q"
               and add binding t -> q to tenv
            else
               add binding E' -> t to aenv
               replace this stmt by "t = E'"
      Ignore any other kind of stmt.
   */
   for (i = 0; i < bb->stmts_used; i++) {
      st = bb->stmts[i];

      /* ignore not-interestings */
      if ((!st) || st->tag != Ist_Tmp)
         continue;

      t = st->Ist.Tmp.tmp;
      eprime = irExpr_to_AvailExpr(st->Ist.Tmp.data);
      /* ignore if not of AvailExpr form */
      if (!eprime)
         continue;

      //vex_printf("considering: " ); ppIRStmt(st); vex_printf("\n");

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
         bb->stmts[i] = IRStmt_Tmp( t, IRExpr_Tmp(q) );
         addToH64( tenv, (ULong)t, (ULong)q );
      } else {
         /* No binding was found, so instead we add E' -> t to our
            collection of available expressions, replace this stmt
            with "t = E'", and move on. */
         bb->stmts[i] = IRStmt_Tmp( t, availExpr_to_IRExpr(eprime) );
         addToH64( aenv, (ULong)eprime, (ULong)t );
      }
   }

   //ppIRBB(bb);
   //sanityCheckIRBB(bb, Ity_I32);
   //vex_printf("\n\n");
      
}


/*---------------------------------------------------------------*/
/*--- Add32/Sub32 chain collapsing                            ---*/
/*---------------------------------------------------------------*/

/* Is this expression "Add32(tmp,const)" or "Sub32(tmp,const)" ?  If
   yes, set *tmp and *i32 appropriately.  *i32 is set as if the
   root node is Add32, not Sub32. */

static Bool isAdd32OrSub32 ( IRExpr* e, IRTemp* tmp, Int* i32 )
{ 
   if (e->tag != Iex_Binop)
      return False;
   if (e->Iex.Binop.op != Iop_Add32 && e->Iex.Binop.op != Iop_Sub32)
      return False;
   if (e->Iex.Binop.arg1->tag != Iex_Tmp)
      return False;
   if (e->Iex.Binop.arg2->tag != Iex_Const)
      return False;
   *tmp = e->Iex.Binop.arg1->Iex.Tmp.tmp;
   *i32 = (Int)(e->Iex.Binop.arg2->Iex.Const.con->Ico.U32);
   if (e->Iex.Binop.op == Iop_Sub32)
      *i32 = -*i32;
   return True;
}


/* Figure out if tmp can be expressed as tmp3 +32 const, for some
   other tmp2.  Scan backwards from the specified start point -- an
   optimisation. */

static Bool collapseChain ( IRBB* bb, Int startHere,
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
      if (!st || st->tag != Ist_Tmp) 
         continue;
      if (st->Ist.Tmp.tmp != var)
         continue;
      e = st->Ist.Tmp.data;
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


/* The main function.  Needs renaming. */

static void track_deltas_BB ( IRBB* bb )
{
   IRStmt *st;
   IRTemp var, var2;
   Int    i, con, con2;

   for (i = bb->stmts_used-1; i >= 0; i--) {
      st = bb->stmts[i];
      if (!st)
         continue;

      /* Try to collapse 't1 = Add32/Sub32(t2, con)'. */

      if (st->tag == Ist_Tmp
          && isAdd32OrSub32(st->Ist.Tmp.data, &var, &con)) {

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
               = IRStmt_Tmp(
                    st->Ist.Tmp.tmp,
                    (con2 >= 0) 
                      ? IRExpr_Binop(Iop_Add32, 
                                     IRExpr_Tmp(var2),
                                     IRExpr_Const(IRConst_U32(con2)))
                      : IRExpr_Binop(Iop_Sub32, 
                                     IRExpr_Tmp(var2),
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

      if (st->tag == Ist_Tmp
          && st->Ist.Tmp.data->tag == Iex_GetI
          && st->Ist.Tmp.data->Iex.GetI.off->tag == Iex_Tmp
          && collapseChain(bb, i-1, st->Ist.Tmp.data->Iex.GetI.off
                                      ->Iex.Tmp.tmp, &var2, &con2)) {
         if (DEBUG_IROPT) {
            vex_printf("replacing3 ");
            ppIRStmt(st);
            vex_printf(" with ");
         }
         con2 += st->Ist.Tmp.data->Iex.GetI.bias;
         bb->stmts[i]
            = IRStmt_Tmp(
                 st->Ist.Tmp.tmp,
                 IRExpr_GetI(st->Ist.Tmp.data->Iex.GetI.descr,
                             IRExpr_Tmp(var2),
                             con2));
         if (DEBUG_IROPT) {
            ppIRStmt(bb->stmts[i]);
            vex_printf("\n");
         }
         continue;
      }

      /* Perhaps st is PutI[t, con] ? */

      if (st->tag == Ist_PutI
          && st->Ist.PutI.off->tag == Iex_Tmp
          && collapseChain(bb, i-1, st->Ist.PutI.off->Iex.Tmp.tmp, 
                               &var2, &con2)) {
         if (DEBUG_IROPT) {
            vex_printf("replacing2 ");
            ppIRStmt(st);
            vex_printf(" with ");
         }
         con2 += st->Ist.PutI.bias;
         bb->stmts[i]
           = IRStmt_PutI(st->Ist.PutI.descr,
                         IRExpr_Tmp(var2),
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

/* ------- Helper functions for PutI/GetI transformations ------ */


/* Determine, to the extent possible, the relationship between two
   guest state accesses.  The possible outcomes are:

   * Exact alias.  These two accesses denote precisely the same
     piece of the guest state.

   * Definely no alias.  These two accesses are guaranteed not to
     overlap any part of the guest state.

   * Unknown -- if neither of the above can be established.

   If in doubt, return Unknown.  */

typedef
   enum { ExactAlias, NoAlias, UnknownAlias }
   GSAliasing;


/* Produces the alias relation between an indexed guest
   state access and a non-indexed access. */

static
GSAliasing getAliasingRelation_IC ( IRArray* descr1, IRExpr* ix1,
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
              IRArray* descr1, IRExpr* ix1, Int bias1,
              IRArray* descr2, IRExpr* ix2, Int bias2
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
   if (!eqIRArray(descr1, descr2))
      return UnknownAlias;

   /* The descriptors are identical.  Now the only difference can be
      in the index expressions.  If they cannot be shown to be
      identical, we have to say we don't know what the aliasing
      relation will be.  Now, since the IR is flattened, the index
      expressions should be atoms -- either consts or tmps.  So that
      makes the comparison simple. */
   vassert(isAtom(ix1));
   vassert(isAtom(ix2));
   if (ix1->tag != Iex_Tmp || ix2->tag != Iex_Tmp)
      return UnknownAlias;

   /* Both index expressions are Tmps.  If they are not the same tmp,
      we're again hosed. */
   if (ix1->Iex.Tmp.tmp != ix2->Iex.Tmp.tmp)
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


static 
IRExpr* findPutI ( IRBB* bb, Int startHere,
                   IRArray* descrG, IRTemp tmpG, Int biasG )
{
   Int      j, iters, biasP;
   UInt     minoffP, maxoffP, minoffG, maxoffG;
   IRArray* descrP;
   IRStmt*  st;
   IRTemp   tmpP;

   if (0) {
      vex_printf("\nfindPutI ");
      ppIRArray(descrG);
      vex_printf(" ");
      ppIRTemp(tmpG);
      vex_printf(" %d\n", biasG);
   }

   /* Scan backwards in bb from startHere to find a suitable PutI
      binding for (descr, tmp, bias), if any. */
   getArrayBounds( descrG, &minoffG, &maxoffG );

   for (j = startHere; j >= 0; j--) {
      st = bb->stmts[j];
      if (!st) continue;

      if (st->tag == Ist_Put) {
         /* Non-indexed Put.  This can't give a binding, but we do
            need to check it doesn't invalidate the search by
            overlapping any part of the indexed guest state. */
         minoffP = st->Ist.Put.offset;
         maxoffP = minoffP + sizeofIRType(
                                typeOfIRExpr(bb->tyenv,st->Ist.Put.data)) 
                           - 1;
         vassert((minoffP & 0xFFFF0000) == 0);
         vassert((maxoffP & 0xFFFF0000) == 0);
         vassert(minoffP <= maxoffP);
         if (maxoffP < minoffG || maxoffG < minoffP) {
            /* we're OK; keep going */
            continue;
         } else {
            /* This Put potentially writes guest state that the GetI
               reads; we must fail. */
           return NULL;
         }
      }

      if (st->tag == Ist_PutI) {
         /* Indexed Put.  First off, do an invalidation check. */
         descrP = st->Ist.PutI.descr;
         getArrayBounds( descrP, &minoffP, &maxoffP );
         if (maxoffP < minoffG || maxoffG < minoffP) {
            /* This PutI definitely doesn't overlap.  Ignore it and
               keep going. */
            continue;
         }
         /* This PutI potentially writes the same array that the GetI
            reads.  It's safe to keep going provided we can show the
            PutI writes some *other* element in the same array -- not
            this one. */

         if (!eqIRArray(descrG, descrP))
            /* The written array has different base, type or size from
               the read array.  Better give up. */
            return NULL;

         if (st->Ist.PutI.off->tag != Iex_Tmp) {
            ppIRStmt(st);
            vpanic("vex iropt: findPutI: .off is not Tmp");
         }
         tmpP = st->Ist.PutI.off->Iex.Tmp.tmp;
         if (tmpP != tmpG) 
            /* We can't show that the base offsets are different.
               Prior CSE and Add/Sub-chain collapsing passes should
               have made them the same wherever possible.  Give up. */
            return NULL;

         biasP = st->Ist.PutI.bias;
         /* So now we know that the GetI and PutI index the same array
            with the same base.  Are the offsets the same, modulo the
            array size?  Do this paranoidly. */
         vassert(descrP->nElems == descrG->nElems);
         iters = 0;
         while (biasP < 0 || biasG < 0) {
            biasP += descrP->nElems;
            biasG += descrP->nElems;
            iters++;
            if (iters > 10)
               vpanic("findPutI: iters");
         }
         vassert(biasP >= 0 && biasG >= 0);
         biasP %= descrP->nElems;
         biasG %= descrP->nElems;

         /* Finally, biasP and biasG are normalised into the range 
            0 .. descrP/G->nElems - 1.  And so we can establish
            equality/non-equality. */

         /* Now we know the PutI doesn't invalidate the search.  But
            does it supply a binding for the GetI ? */
         if (0) {
            vex_printf("considering P ");
            ppIRStmt(st);
            vex_printf("\n");
         }
         if (biasP == biasG) {
            /* Yup, found a replacement. */
            return st->Ist.PutI.data;
         }

         /* else ... no, they don't match.  Keep going. */
      } /* if (st->tag == Ist_PutI) */

   } /* for */

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

   return getAliasingRelation_II( 
             pi->Ist.PutI.descr, pi->Ist.PutI.off, pi->Ist.PutI.bias, 
             s2->Ist.PutI.descr, s2->Ist.PutI.off, s2->Ist.PutI.bias
          )
          == ExactAlias;
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

      case Ist_Put:
         vassert(isAtom(s2->Ist.Put.data));
         relation 
            = getAliasingRelation_IC(
                 pi->Ist.PutI.descr, pi->Ist.PutI.off,
                 s2->Ist.Put.offset, 
                 typeOfIRExpr(tyenv,s2->Ist.Put.data)
              );
         goto have_relation;

      case Ist_PutI:
         vassert(isAtom(s2->Ist.PutI.off));
         vassert(isAtom(s2->Ist.PutI.data));
         relation
            = getAliasingRelation_II(
                 pi->Ist.PutI.descr, pi->Ist.PutI.off, pi->Ist.PutI.bias, 
                 s2->Ist.PutI.descr, s2->Ist.PutI.off, s2->Ist.PutI.bias
              );
         goto have_relation;

      case Ist_Tmp:
         if (s2->Ist.Tmp.data->tag == Iex_GetI) {
            relation
               = getAliasingRelation_II(
                    pi->Ist.PutI.descr, pi->Ist.PutI.off, 
                                        pi->Ist.PutI.bias, 
                    s2->Ist.Tmp.data->Iex.GetI.descr,
                    s2->Ist.Tmp.data->Iex.GetI.off,
                    s2->Ist.Tmp.data->Iex.GetI.bias
                 );
            goto have_relation;
         }
         if (s2->Ist.Tmp.data->tag == Iex_Get) {
            relation
               = getAliasingRelation_IC(
                    pi->Ist.PutI.descr, pi->Ist.PutI.off,
                    s2->Ist.Tmp.data->Iex.Get.offset,
                    s2->Ist.Tmp.data->Iex.Get.ty
                 );
            goto have_relation;
         }
         return False;

      case Ist_STle:
         vassert(isAtom(s2->Ist.STle.addr));
         vassert(isAtom(s2->Ist.STle.data));
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

/* Do PutI -> GetI forwarding.  bb is modified in-place. */

static
void do_PutI_GetI_forwarding_BB ( IRBB* bb )
{
   Int     i;
   IRStmt* st;

   for (i = bb->stmts_used-1; i >= 0; i--) {
      st = bb->stmts[i];
      if (!st)
         continue;

      if (st->tag == Ist_Tmp
          && st->Ist.Tmp.data->tag == Iex_GetI
          && st->Ist.Tmp.data->Iex.GetI.off->tag == Iex_Tmp) {
         IRArray* descr = st->Ist.Tmp.data->Iex.GetI.descr;
         IRTemp   tmp   = st->Ist.Tmp.data->Iex.GetI.off->Iex.Tmp.tmp;
         Int      bias  = st->Ist.Tmp.data->Iex.GetI.bias;
         IRExpr*  replacement = findPutI(bb, i-1, descr, tmp, bias);
         if (replacement && isAtom(replacement)) {
            if (DEBUG_IROPT) {
               vex_printf("PiGi: "); 
               ppIRExpr(st->Ist.Tmp.data);
               vex_printf(" -> ");
               ppIRExpr(replacement);
               vex_printf("\n");
            }
            bb->stmts[i] = IRStmt_Tmp(st->Ist.Tmp.tmp, replacement);
         }
      }
   }

}

/* Remove redundant PutIs, to the extent which they can be detected.
   bb is modified in-place. */

static
void do_redundant_PutI_elimination ( IRBB* bb )
{
   Int    i, j;
   Bool   delete;
   IRStmt *st, *stj;

   for (i = 0; i < bb->stmts_used; i++) {
      st = bb->stmts[i];
      if (!st || st->tag != Ist_PutI)
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
         if (!stj) 
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
         bb->stmts[i] = NULL;
      }

   }
}


/*---------------------------------------------------------------*/
/*--- iropt main                                              ---*/
/*---------------------------------------------------------------*/

static Bool iropt_verbose = False;


/* Rules of the game:

   - IRExpr/IRStmt trees should be treated as immutable, as they
     may get shared.  So never change a field of such a tree node;
     instead construct and return a new one if needed.
*/

/* Do a simple cleanup pass on bb.  This is: redundant Get removal,
   redundant Put removal, constant propagation, dead code removal,
   clean helper specialisation, and dead code removal (again).
*/

static 
IRBB* baseline_cleanup ( IRBB* bb,
                         IRExpr* (*specHelper) ( Char*, IRExpr**) )
{
   redundant_get_removal_BB ( bb );
   if (iropt_verbose) {
      vex_printf("\n========= REDUNDANT GET\n\n" );
      ppIRBB(bb);
   }

   redundant_put_removal_BB ( bb );
   if (iropt_verbose) {
      vex_printf("\n========= REDUNDANT PUT\n\n" );
      ppIRBB(bb);
   }

   bb = cprop_BB ( bb );
   if (iropt_verbose) {
      vex_printf("\n========= CPROPD\n\n" );
      ppIRBB(bb);
   }

   dead_BB ( bb );
   if (iropt_verbose) {
      vex_printf("\n========= DEAD\n\n" );
      ppIRBB(bb);
   }

   spec_helpers_BB ( bb, specHelper );
   dead_BB ( bb );
   if (iropt_verbose) {
      vex_printf("\n========= SPECd \n\n" );
      ppIRBB(bb);
   }

   return bb;
}

/* Scan a flattened BB to see if it has any GetI or PutIs in it.  Used
   as a heuristic hack to see if iropt needs to do expensive
   optimisations (CSE, PutI -> GetI forwarding) to improve code with
   those in. 
*/
static Bool hasGetIorPutI ( IRBB* bb )
{
   Int i, j;
   IRStmt* st;
   IRDirty* d;

   for (i = 0; i < bb->stmts_used; i++) {
      st = bb->stmts[i];
      if (!st)
         continue;

      switch (st->tag) {
         case Ist_PutI: 
            return True;
         case Ist_Tmp:  
            if (st->Ist.Tmp.data->tag == Iex_GetI)
               return True;
            break;
         case Ist_Put:
            vassert(isAtom(st->Ist.Put.data));
            break;
         case Ist_STle:
            vassert(isAtom(st->Ist.STle.addr));
            vassert(isAtom(st->Ist.STle.data));
            break;
         case Ist_Exit:
            vassert(isAtom(st->Ist.Exit.cond));
            break;
         case Ist_Dirty:
            d = st->Ist.Dirty.details;
            for (j = 0; d->args[j]; j++)
               vassert(isAtom(d->args[j]));
            if (d->mFx != Ifx_None)
               vassert(isAtom(d->mAddr));
            break;
         default: 
            ppIRStmt(st);
            vpanic("hasGetIorPutI");
      }

   }
   return False;

}


/* exported from this file */
/* The main iropt entry point. */

IRBB* do_iropt_BB ( IRBB* bb0,
                    IRExpr* (*specHelper) ( Char*, IRExpr**) )
{
   static UInt n_total     = 0;
   static UInt n_expensive = 0;

   Bool show_res = False;

   IRBB *bb;
 
   n_total++;

   /* First flatten the block out, since all other
      phases assume flat code. */

   bb = flatten_BB ( bb0 );

   if (iropt_verbose) {
      vex_printf("\n========= FLAT\n\n" );
      ppIRBB(bb);
   }

   /* Now do a preliminary cleanup pass. */

   bb = baseline_cleanup( bb, specHelper );

   /* If there are GetI/PutI in this block, do some expensive
      transformations:

    - CSE 
    - re-run of the baseline cleanup

   */

   if (hasGetIorPutI(bb)) {
      n_expensive++;
      vex_printf("***** EXPENSIVE %d %d\n", n_total, n_expensive);
      cse_BB( bb );
      //ppIRBB(bb); vex_printf("\n\n");
      track_deltas_BB( bb );
      do_PutI_GetI_forwarding_BB( bb );
      do_redundant_PutI_elimination( bb );
      /*
      ppIRBB(bb); vex_printf("\n\n");
      dead_BB( bb );
      bb = cprop_BB ( bb );
      dead_BB(bb);
      */
      //ppIRBB(bb); vex_printf("\n\nQQQQ\n");
      bb = baseline_cleanup( flatten_BB(bb), specHelper );
      //vassert(0);
      //      vex_printf("expensive done\n");
      //show_res = True;
   }

   /* Finally, rebuild trees, for the benefit of instruction
      selection. */

   treebuild_BB ( bb );
   if (show_res || iropt_verbose) {
      vex_printf("\n========= TREEd \n\n" );
      ppIRBB(bb);
   }

   return bb;
}


/*---------------------------------------------------------------*/
/*--- end                                          ir/iropt.c ---*/
/*---------------------------------------------------------------*/
