
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (ir_defs.c) is                                ---*/
/*--- Copyright (c) 2004 OpenWorks LLP.  All rights reserved. ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

#include "libjit_basictypes.h"
#include "ir_defs.h"


/*---------------------------------------------------------------*/
/*--- Printing the IR                                         ---*/
/*---------------------------------------------------------------*/

void ppIRType ( FILE* f, IRType ty )
{
  switch (ty) {
    case Ity_Bit: fprintf(f, "Bit"); break;
    case Ity_I8:  fprintf(f, "I8"); break;
    case Ity_I16: fprintf(f, "I16"); break;
    case Ity_I32: fprintf(f, "I32"); break;
    case Ity_I64: fprintf(f, "I64"); break;
    default: panic("ppIRType");
  }
}

void ppIRConst ( FILE* f, IRConst* con )
{
  switch (con->tag) {
    case Ico_U8:  fprintf(f, "0x%x",   (UInt)(con->Ico.U8)); break;
    case Ico_U16: fprintf(f, "0x%x",   (UInt)(con->Ico.U16)); break;
    case Ico_U32: fprintf(f, "0x%x",   (UInt)(con->Ico.U32)); break;
    case Ico_U64: fprintf(f, "0x%llx", (ULong)(con->Ico.U64)); break;
    default: panic("ppIRConst");
  }
}

void ppIRTemp ( FILE* f, IRTemp tmp )
{
  fprintf(f, "t%d", tmp);
}


void ppIROp ( FILE* f, IROp op )
{
  switch (op) {
    case Iop_Add32: fprintf(f, "Add32"); break;
    case Iop_Sub32: fprintf(f, "Sub32"); break;
    case Iop_Mul32: fprintf(f, "Mul32"); break;
    case Iop_Or32:  fprintf(f, "Or32");  break;
    case Iop_And32: fprintf(f, "And32"); break;
    case Iop_Xor32: fprintf(f, "Xor32"); break;
    case Iop_Shl32: fprintf(f, "Shl32"); break;
    case Iop_Shr32: fprintf(f, "Shr32"); break;
    case Iop_Sar32: fprintf(f, "Sar32"); break;
    case Iop_Not32: fprintf(f, "Not32"); break;
    case Iop_Neg32: fprintf(f, "Neg32"); break;
    default: panic("ppIROp");
  }
}

void ppIRExpr ( FILE* f, IRExpr* e )
{
  switch (e->tag) {
    case Iex_Get:
      fprintf(f, "GET(%d,%d)", e->Iex.Get.offset, e->Iex.Get.size);
      break;
    case Iex_Tmp:
      ppIRTemp(f, e->Iex.Tmp.tmp);
      break;
    case Iex_Binop:
      ppIROp(f, e->Iex.Binop.op);
      fprintf(f, "(");
      ppIRExpr(f, e->Iex.Binop.arg1);
      fprintf(f, ",");
      ppIRExpr(f, e->Iex.Binop.arg2);
      fprintf(f, ")");
      break;
    case Iex_Unop:
      ppIROp(f, e->Iex.Unop.op);
      fprintf(f, "(");
      ppIRExpr(f, e->Iex.Unop.arg);
      fprintf(f, ")");
      break;
    case Iex_LDle:
      fprintf(f, "LDle<");
      ppIRType(f, e->Iex.LDle.ty);
      fprintf(f,">(");
      ppIRExpr(f, e->Iex.LDle.addr);
      fprintf(f,")");
      break;
    case Iex_Const:
      ppIRConst(f, e->Iex.Const.con);
      break;
    default:
      panic("ppIExpr");
  }
}

void ppIRStmt ( FILE* f, IRStmt* s )
{
  switch (s->tag) {
    case Ist_Put:
      fprintf(f, "Put(%d,%d) = ", s->Ist.Put.offset, s->Ist.Put.size);
      ppIRExpr(f, s->Ist.Put.expr);
      break;
    case Ist_Tmp:
      ppIRTemp(f, s->Ist.Tmp.tmp);
      fprintf(f," = ");
      ppIRExpr(f, s->Ist.Tmp.expr);
      break;
    case Ist_STle:
      fprintf(f, "STle(");
      ppIRExpr(f, s->Ist.STle.addr);
      fprintf(f, ") = ");
      ppIRExpr(f, s->Ist.STle.data);
      break;
    default: 
      panic("ppIRStmt");
  }
}

void ppIRNext ( FILE* f, IRNext* nx )
{
  switch (nx->tag) {
    case Inx_UJump: 
      fprintf(f, "UJump ");
      ppIRConst(f, nx->Inx.UJump.dst);
      break;
    case Inx_CJump01:
      fprintf(f,"CJump01 (");
      ppIRExpr(f, nx->Inx.CJump01.cond);
      fprintf(f, ") ");
      ppIRConst(f, nx->Inx.CJump01.dst0);
      fprintf(f, " ");
      ppIRConst(f, nx->Inx.CJump01.dst1);
      break;
    case Inx_IJump:
      fprintf(f, "IJump ");
      ppIRExpr(f, nx->Inx.IJump.dst);
      break;
    default: 
      panic("ppIRNext");
  }
}

void ppIRTypeEnv ( FILE* f, IRTypeEnv* env ) {
   UInt i;
   for (i = 0; i < env->map_used; i++) {
      if (i % 8 == 0)
         fprintf(f, "   ");
      ppIRTemp(f, env->map[i].name);
      fprintf(f, ":");
      ppIRType(f, env->map[i].type);
      if (i % 8 == 7) 
         fprintf(f, "\n"); 
      else 
         fprintf(f, "   ");
   }
   if (env->map_used > 0 && env->map_used % 8 != 7) 
         fprintf(f, "\n"); 
}


void ppIRBB ( FILE* f, IRBB* bb )
{
   IRStmt* s;
   ppIRTypeEnv(f, bb->tyenv);
   for (s = bb->stmts; s; s = s->link) {
      fprintf(f, "   ");
      ppIRStmt(f, s);
      fprintf(f, "\n");
   }
   fprintf(f, "   ");
   ppIRNext(f, bb->next);
   fprintf(f, "\n");
}


/*---------------------------------------------------------------*/
/*--- Constructors                                            ---*/
/*---------------------------------------------------------------*/


/* Constructors -- IRConst */

IRConst* IRConst_U8 ( UChar u8 )
{
   IRConst* c = malloc(sizeof(IRConst));
   c->tag     = Ico_U8;
   c->Ico.U8  = u8;
   return c;
}
IRConst* IRConst_U16 ( UShort u16 )
{
   IRConst* c = malloc(sizeof(IRConst));
   c->tag     = Ico_U16;
   c->Ico.U16 = u16;
   return c;
}
IRConst* IRConst_U32 ( UInt u32 )
{
   IRConst* c = malloc(sizeof(IRConst));
   c->tag     = Ico_U32;
   c->Ico.U32 = u32;
   return c;
}
IRConst* IRConst_U64 ( ULong u64 )
{
   IRConst* c = malloc(sizeof(IRConst));
   c->tag     = Ico_U64;
   c->Ico.U64 = u64;
   return c;
}


/* Constructors -- IRExpr */

IRExpr* IRExpr_Get ( Int off, Int sz ) {
   IRExpr* e         = malloc(sizeof(IRExpr));
   e->tag            = Iex_Get;
   e->Iex.Get.offset = off;
   e->Iex.Get.size   = sz;
   return e;
}
IRExpr* IRExpr_Tmp ( IRTemp tmp ) {
   IRExpr* e      = malloc(sizeof(IRExpr));
   e->tag         = Iex_Tmp;
   e->Iex.Tmp.tmp = tmp;
   return e;
}
IRExpr* IRExpr_Binop ( IROp op, IRExpr* arg1, IRExpr* arg2 ) {
   IRExpr* e         = malloc(sizeof(IRExpr));
   e->tag            = Iex_Binop;
   e->Iex.Binop.op   = op;
   e->Iex.Binop.arg1 = arg1;
   e->Iex.Binop.arg2 = arg2;
   return e;
}
IRExpr* IRExpr_Unop ( IROp op, IRExpr* arg ) {
   IRExpr* e       = malloc(sizeof(IRExpr));
   e->tag          = Iex_Unop;
   e->Iex.Unop.op  = op;
   e->Iex.Unop.arg = arg;
   return e;
}
IRExpr* IRExpr_LDle  ( IRType ty, IRExpr* addr ) {
   IRExpr* e        = malloc(sizeof(IRExpr));
   e->tag           = Iex_LDle;
   e->Iex.LDle.ty   = ty;
   e->Iex.LDle.addr = addr;
   return e;
}
IRExpr* IRExpr_Const ( IRConst* con ) {
   IRExpr* e        = malloc(sizeof(IRExpr));
   e->tag           = Iex_Const;
   e->Iex.Const.con = con;
   return e;
}


/* Constructors -- IRStmt */

IRStmt* IRStmt_Put ( Int off, Int sz, IRExpr* value ) {
   IRStmt* s         = malloc(sizeof(IRStmt));
   s->tag            = Ist_Put;
   s->link           = NULL;
   s->Ist.Put.offset = off;
   s->Ist.Put.size   = sz;
   s->Ist.Put.expr   = value;
   return s;
}
IRStmt* IRStmt_Tmp ( IRTemp tmp, IRExpr* expr ) {
   IRStmt* s       = malloc(sizeof(IRStmt));
   s->tag          = Ist_Tmp;
   s->link         = NULL;
   s->Ist.Tmp.tmp  = tmp;
   s->Ist.Tmp.expr = expr;
   return s;
}
IRStmt* IRStmt_STle ( IRExpr* addr, IRExpr* value ) {
   IRStmt* s        = malloc(sizeof(IRStmt));
   s->tag           = Ist_STle;
   s->link          = NULL;
   s->Ist.STle.addr = addr;
   s->Ist.STle.data = value;
   return s;
}


/* Constructors -- IRNext */

IRNext* IRNext_UJump ( IRConst* dst ) {
   IRNext* nx        = malloc(sizeof(IRNext));
   nx->tag           = Inx_UJump;
   nx->Inx.UJump.dst = dst;
   return nx;
}


/* Constructors -- IRBB */

IRBB* mk_IRBB ( IRTypeEnv* env, IRStmt* stmts, IRNext* next ) {
   IRBB* bb  = malloc(sizeof(IRBB));
   bb->tyenv = env;
   bb->stmts = stmts;
   bb->next  = next;
   return bb;
}


/*---------------------------------------------------------------*/
/*--- Helper functions for the IR                             ---*/
/*---------------------------------------------------------------*/

IRTypeEnv* newIRTypeEnv ( void )
{
   IRTypeEnv* env = malloc(sizeof(IRTypeEnv));
   env->map       = NULL;
   env->map_size  = 0;
   env->map_used  = 0;
   return env;
}


void addToIRTypeEnv ( IRTypeEnv* env, IRTemp tmp, IRType ty )
{
   assert(env);
   assert(env->map_used >= 0);
   assert(env->map_size >= 0);
   assert(env->map_used <= env->map_size);
   if (env->map_used < env->map_size) {
      env->map[env->map_used].name = tmp;
      env->map[env->map_used].type = ty;
      env->map_used++;
   } else {
      Int i;
      Int new_size = env->map_size==0 ? 8 : 2*env->map_size;
      IRTypeEnvMaplet* new_map = malloc(new_size * sizeof(IRTypeEnvMaplet));
      for (i = 0; i < env->map_used; i++)
         new_map[i] = env->map[i];
      env->map      = new_map;
      env->map_size = new_size;
      return addToIRTypeEnv(env, tmp, ty);
   }
}


IRType lookupIRTypeEnv ( IRTypeEnv* env, IRTemp tmp )
{
   Int i;
   for (i = 0; i < env->map_used; i++)
      if (env->map[i].name == tmp)
         return env->map[i].type;
   panic("lookupIRTypeEnv");
}


IRType typeOfIRExpr ( IRTypeEnv* tyenv, IRExpr* e )
{
   switch (e->tag) {
      case Iex_Tmp:
         return lookupIRTypeEnv(tyenv, e->Iex.Tmp.tmp);
      case Iex_Const:
         switch (e->Iex.Const.con->tag) {
            case Ico_U8:  return Ity_I8;
            case Ico_U16: return Ity_I16;
            case Ico_U32: return Ity_I32;
            case Ico_U64: return Ity_I64;
            default: panic("typeOfIRExpr:Iex_Const");
         }
         break;
      case Iex_Binop:
         switch (e->Iex.Binop.op) {
            case Iop_Add32: case Iop_Sub32: case Iop_Mul32: 
            case Iop_Or32:  case Iop_And32: case Iop_Xor32:
            case Iop_Shl32: case Iop_Shr32: case Iop_Sar32:
               return Ity_I32;
            default: break;
         }
         break;
      default:
         break;
   }
   ppIRExpr(stderr,e);
   panic("typeOfIRExpr");
}
