
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
   h->size   = 4;
   h->used   = 0;
   h->inuse  = LibVEX_Alloc(h->size * sizeof(Bool));
   h->key    = LibVEX_Alloc(h->size * sizeof(ULong));
   h->val    = LibVEX_Alloc(h->size * sizeof(ULong));
   return h;
}


/* Lookup key in the map. */

static Bool lookupH64 ( /*OUT*/ULong* val, Hash64* h, ULong key )
{
   Int i;
   for (i = 0; i < h->used; i++) {
      if (h->inuse[i] && h->key[i] == key) {
         *val = h->val[i];
         return True;
      }
   }
   return False;
}


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



/* Add key->val to the map.  Replaces any existing binding for key. */

static void addToH64 ( Hash64* h, ULong key, ULong val )
{
   Int i, j;

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
   h->key[h->used] = val;
   h->used++;
}

/*---------------------------------------------------------------*/
/*--- Flattening out a BB into pure SSA form                  ---*/
/*---------------------------------------------------------------*/

/* Clone the NULL-terminated vector of IRExpr*s attached to a
   CCall. */

static IRExpr** copyIexCCallArgs ( IRExpr** vec )
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
         newargs = copyIexCCallArgs(ex->Iex.CCall.args);
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
   IRExpr *e1, *e2;
   switch (st->tag) {
      case Ist_Put:
         e1 = flatten_Expr(bb, st->Ist.Put.expr);
         addStmtToIRBB(bb, IRStmt_Put(st->Ist.Put.offset, e1));
         break;
#if 0
      case Ist_PutI:
        e1 = flatten_Expr(bb, st->Ist.PutI.offset);
        e2 = flatten_Expr(bb, st->Ist.PutI.expr);
        addStmtToIRBB(bb, IRStmt_PutI(e1, e2, st->Ist.PutI.minoff,
                          st->Ist.PutI.maxoff));
        break;
#endif
      case Ist_Tmp:
         e1 = flatten_Expr(bb, st->Ist.Tmp.expr);
         addStmtToIRBB(bb, IRStmt_Tmp(st->Ist.Tmp.tmp, e1));
         break;
      case Ist_STle:
         e1 = flatten_Expr(bb, st->Ist.STle.addr);
         e2 = flatten_Expr(bb, st->Ist.STle.data);
         addStmtToIRBB(bb, IRStmt_STle(e1,e2));
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
      flatten_Stmt( out, in->stmts[i] );
   out->next     = flatten_Expr( out, in->next );
   out->jumpkind = in->jumpkind;
   return out;
}


/*---------------------------------------------------------------*/
/*--- iropt main                                              ---*/
/*---------------------------------------------------------------*/

/* Rules of the game:

   - IRExpr/IRStmt trees should be treated as immutable, as they
     may get shared.  So never change a field of such a tree node;
     instead construct and return a new one if needed.
*/

/* exported from this file */
IRBB* do_iropt_BB ( IRBB* bb0 )
{
   IRBB* flat = flatten_BB ( bb0 );
   return flat;
}


/*---------------------------------------------------------------*/
/*--- end                                          ir/iropt.c ---*/
/*---------------------------------------------------------------*/
