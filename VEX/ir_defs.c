
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (ir_defs.c) is                                ---*/
/*--- Copyright (c) 2004 OpenWorks LLP.  All rights reserved. ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

#include "basictypes.h"
#include "ir_defs.h"

#include <malloc.h>

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
    case Ico_u8:  fprintf(f, "0x%x",   (UInt)(con->Ico.u8)); break;
    case Ico_u16: fprintf(f, "0x%x",   (UInt)(con->Ico.u16)); break;
    case Ico_u32: fprintf(f, "0x%x",   (UInt)(con->Ico.u32)); break;
    case Ico_u64: fprintf(f, "0x%llx", (ULong)(con->Ico.u64)); break;
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

void ppIRBB ( FILE* f, IRBB* bb )
{
   IRStmt* s;
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

/* Constructors -- IRExpr */

IRExpr* mkExGet ( Int off, Int sz ) {
   IRExpr* e         = malloc(sizeof(IRExpr));
   e->tag            = Iex_Get;
   e->Iex.Get.offset = off;
   e->Iex.Get.size   = sz;
   return e;
}
IRExpr* mkExTmp ( IRTemp tmp ) {
   IRExpr* e      = malloc(sizeof(IRExpr));
   e->tag         = Iex_Tmp;
   e->Iex.Tmp.tmp = tmp;
   return e;
}
IRExpr* mkExBinop ( IROp op, IRExpr* arg1, IRExpr* arg2 ) {
   IRExpr* e         = malloc(sizeof(IRExpr));
   e->tag            = Iex_Binop;
   e->Iex.Binop.op   = op;
   e->Iex.Binop.arg1 = arg1;
   e->Iex.Binop.arg2 = arg2;
   return e;
}
IRExpr* mkExUnop ( IROp op, IRExpr* arg ) {
   IRExpr* e       = malloc(sizeof(IRExpr));
   e->tag          = Iex_Unop;
   e->Iex.Unop.op  = op;
   e->Iex.Unop.arg = arg;
   return e;
}
IRExpr* mkExLDle  ( IRType ty, IRExpr* addr ) {
   IRExpr* e        = malloc(sizeof(IRExpr));
   e->tag           = Iex_LDle;
   e->Iex.LDle.ty   = ty;
   e->Iex.LDle.addr = addr;
   return e;
}
IRExpr* mkExConst ( IRConst* con ) {
   IRExpr* e        = malloc(sizeof(IRExpr));
   e->tag           = Iex_Const;
   e->Iex.Const.con = con;
   return e;
}


/* Constructors -- IRStmt */

IRStmt* mkStPut ( Int off, Int sz, IRExpr* value ) {
   IRStmt* s         = malloc(sizeof(IRStmt));
   s->tag            = Ist_Put;
   s->Ist.Put.offset = off;
   s->Ist.Put.size   = sz;
   s->Ist.Put.expr   = value;
   return s;
}
IRStmt* mkStTmp ( IRTemp tmp, IRExpr* expr ) {
   IRStmt* s       = malloc(sizeof(IRStmt));
   s->tag          = Ist_Tmp;
   s->Ist.Tmp.tmp  = tmp;
   s->Ist.Tmp.expr = expr;
   return s;
}
IRStmt* mkStSTle ( IRExpr* addr, IRExpr* value ) {
   IRStmt* s        = malloc(sizeof(IRStmt));
   s->tag           = Ist_STle;
   s->Ist.STle.addr = addr;
   s->Ist.STle.data = value;
   return s;
}


/* Constructors -- IRNext */

IRNext* mkNxUJump ( IRConst* dst ) {
   IRNext* nx        = malloc(sizeof(IRNext));
   nx->tag           = Inx_UJump;
   nx->Inx.UJump.dst = dst;
   return nx;
}


/* Constructors -- IRBB */

IRBB* mkIRBB ( IRStmt* stmts, IRNext* next ) {
   IRBB* bb  = malloc(sizeof(IRBB));
   bb->stmts = stmts;
   bb->next  = next;
   return bb;
}




