
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (ir_defs.c) is                                ---*/
/*--- Copyright (c) 2004 OpenWorks LLP.  All rights reserved. ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

#include <assert.h>

#include "ir_defs.h"


/*---------------------------------------------------------------*/
/*--- Registering the storage tags                            ---*/
/*---------------------------------------------------------------*/

void register_IR_tags ( void )
{
  storage_register_tag(IR_TMP, 1, "tmp");

  storage_register_tag(TY_Bit, 0, "Bit");
  storage_register_tag(TY_I8,  0, "I8");
  storage_register_tag(TY_I16, 0, "I16");
  storage_register_tag(TY_I32, 0, "I32");
  storage_register_tag(TY_I64, 0, "I64");

  storage_register_tag(OP_Add32, 2, "Add32");
  storage_register_tag(OP_Sub32, 2, "Sub32");
  storage_register_tag(OP_Mul32, 2, "Mul32");
  storage_register_tag(OP_Or32,  2, "Or32" );
  storage_register_tag(OP_And32, 2, "And32");
  storage_register_tag(OP_Xor32, 2, "Xor32");
  storage_register_tag(OP_Shl32, 2, "Shl32");
  storage_register_tag(OP_Shr32, 2, "Shr32");
  storage_register_tag(OP_Sar32, 2, "Sar32");

  storage_register_tag(OP_Not32, 1, "Not32");
  storage_register_tag(OP_Neg32, 1, "Neg32");

  storage_register_tag(ST_Put,  3, "ST_Put");
  storage_register_tag(ST_Tmp,  2, "ST_Tmp");
  storage_register_tag(ST_STle, 2, "ST_STle");

  storage_register_tag(NX_UJump,   1, "NX_UJump");
  storage_register_tag(NX_CJump01, 3, "NX_CJump01");
  storage_register_tag(NX_IJump,   1, "NX_IJump");

  storage_register_tag(EX_Get,   2, "EX_Get");
  storage_register_tag(EX_Tmp,   1, "EX_Tmp");
  storage_register_tag(EX_Binop, 3, "EX_Binop");
  storage_register_tag(EX_Unop,  2, "EX_Unop");
  storage_register_tag(EX_LDle,  2, "EX_LDle");
  storage_register_tag(EX_AtomI, 1, "EX_AtomI");

  storage_register_tag(IR_BB,    3, "IR_BB");
}


/*---------------------------------------------------------------*/
/*--- Printing the IR                                         ---*/
/*---------------------------------------------------------------*/

void ppGuestAddr ( FILE* f, GuestAddr ga )
{
  switch (getTag(ga)) {
    case TagWord32: fprintf(f, "0x%x",   getWord32(ga)); break;
    case TagWord64: fprintf(f, "0x%llx", getWord64(ga)); break;
    default: panic("ppGuestAddr");
  }
}

void ppIROp ( FILE* f, IROp op )
{
  switch (getTag(op)) {
    case OP_Add32: fprintf(f, "Add32"); break;
    case OP_Sub32: fprintf(f, "Sub32"); break;
    case OP_Mul32: fprintf(f, "Mul32"); break;
    case OP_Or32:  fprintf(f, "Or32");  break;
    case OP_And32: fprintf(f, "And32"); break;
    case OP_Xor32: fprintf(f, "Xor32"); break;
    case OP_Shl32: fprintf(f, "Shl32"); break;
    case OP_Shr32: fprintf(f, "Shr32"); break;
    case OP_Sar32: fprintf(f, "Sar32"); break;
    case OP_Not32: fprintf(f, "Not32"); break;
    case OP_Neg32: fprintf(f, "Neg32"); break;
    default: panic("ppIROp");
  }
}

void ppIRAtomI ( FILE* f, IRAtomI at )
{
  switch (getTag(at)) {
    case TagWord8:  fprintf(f, "0x%x",   getWord8(at)); break;
    case TagWord16: fprintf(f, "0x%x",   getWord16(at)); break;
    case TagWord32: fprintf(f, "0x%x",   getWord32(at)); break;
    case TagWord64: fprintf(f, "0x%llx", getWord64(at)); break;
    default: panic("ppIRAtom");
  }
}

void ppIRType ( FILE* f, IRType ty )
{
  switch (getTag(ty)) {
    case TY_Bit: fprintf(f, "Bit"); break;
    case TY_I8:  fprintf(f, "I8"); break;
    case TY_I16: fprintf(f, "I16"); break;
    case TY_I32: fprintf(f, "I32"); break;
    case TY_I64: fprintf(f, "I64"); break;
    default: panic("ppIRType");
  }
}

void ppIRExpr ( FILE* f, IRExpr expr )
{
  switch (getTag(expr)) {
    case EX_Get:
      fprintf(f, "GET(%d,%d)", sel21(expr), sel22(expr));
      break;
    case EX_Tmp:
      ppIRTemp(f,sel11(expr));
      break;
    case EX_Binop:
      ppIROp(f,sel31(expr));
      fprintf(f, "(");
      ppIRExpr(f,sel32(expr));
      fprintf(f, ",");
      ppIRExpr(f,sel33(expr));
      fprintf(f, ")");
      break;
    case EX_Unop:
      ppIROp(f,sel21(expr));
      fprintf(f, "(");
      ppIRExpr(f,sel22(expr));
      fprintf(f, ")");
      break;
    case EX_LDle:
      fprintf(f, "LDle<");
      ppIRType(f, sel21(expr));
      fprintf(f,">(");
      ppIRExpr(f, sel22(expr));
      fprintf(f,")");
      break;
    case EX_AtomI:
      ppIRAtomI(f, sel11(expr));
      break;
    default:
      panic("ppIExpr");
  }
}

void ppIRTemp ( FILE* f, IRTemp tmp )
{
  switch (getTag(tmp)) {
    case IR_TMP: fprintf(f, "t%d", sel11(tmp)); break;
    default:  panic("ppIRTemp");
  }
}

void ppIRStmt ( FILE* f, IRStmt stmt )
{
  switch (getTag(stmt)) {
    case ST_Put:
      fprintf(f, "Put(%d,%d) = ", sel31(stmt), sel32(stmt));
      ppIRExpr(f, sel33(stmt));
      break;
    case ST_Tmp:
      ppIRTemp(f, sel21(stmt));
      fprintf(f," = ");
      ppIRExpr(f, sel22(stmt));
      break;
    case ST_STle:
      fprintf(f, "STle(");
      ppIRExpr(f, sel21(stmt));
      fprintf(f, ") = ");
      ppIRExpr(f, sel22(stmt));
      break;
    default: 
      panic("ppIRStmt");
  }
}

void ppIRNext ( FILE* f, IRNext next )
{
  switch (getTag(next)) {
    case NX_UJump: 
      fprintf(f, "UJump ");
      ppGuestAddr(f,sel11(next));
      break;
    case NX_CJump01:
      fprintf(f,"CJump01 (");
      ppIRExpr(f,sel31(next));
      fprintf(f, ") ");
      ppGuestAddr(f,sel32(next));
      fprintf(f, " ");
      ppGuestAddr(f,sel33(next));
    case NX_IJump:
      fprintf(f, "IJump ");
      ppIRExpr(f,sel11(next));
      break;
    default: 
      panic("ppIRNext");
  }
}

void ppIRBB ( FILE* f, IRBB bb )
{
   Cell sts;
   assert(getTag(bb) == IR_BB);
   for (sts = sel32(bb); nonNil(sts); sts=tail(sts)) {
      fprintf(f, "   ");
      ppIRStmt(f,head(sts));
      fprintf(f, "\n");
   }
   fprintf(f, "   ");
   ppIRNext(f,sel33(bb));
   fprintf(f, "\n");
}


/*---------------------------------------------------------------*/
/*--- Constructors                                            ---*/
/*---------------------------------------------------------------*/

/* Constructors -- IRBB */
IRBB mkBB ( AList tys, List stmts, IRNext next ) {
  return tuple3(IR_BB, tys, stmts, next);
}

/* Constructors -- IRNext */
IRNext mkUJump  ( GuestAddr dst ) {
  return tuple1 ( NX_UJump, dst );
}

/* Constructors -- IRStmt */
IRStmt mkSTPut  ( Int off, Int sz, IRExpr value ) {
  return tuple3 ( ST_Put, off, sz, value);
}
IRStmt mkSTTmp  ( IRTemp tmp, IRExpr expr ) {
  return tuple2 ( ST_Tmp, tmp, expr );
}
IRStmt mkSTSTle ( IRExpr addr, IRExpr value ) {
  return tuple2 ( ST_STle, addr, value );
}

/* Constructors -- IRExpr */
IRExpr mkEXGet   ( Int off, Int sz ) {
  return tuple2 ( EX_Get, off, sz );
}
IRExpr mkEXTmp   ( IRTemp tmp ) {
  return tuple1 ( EX_Tmp, tmp );
}
IRExpr mkEXBinop ( IROp op, IRExpr arg1, IRExpr arg2 ) {
  return tuple3 ( EX_Binop, op, arg1, arg2 );
}
IRExpr mkEXUnop  ( IROp op, IRExpr arg ) {
  return tuple2 ( EX_Unop, op, arg );
}
IRExpr mkEXLDle  ( IRType ty, IRExpr addr ) {
  return tuple2 ( EX_LDle, ty, addr );
}
IRExpr mkEXAtomI ( IRAtomI atom ) {
  return tuple1 ( EX_AtomI, atom );
}

/* Constructors -- IRTemp */
IRTemp mkIRTemp  ( Int n ) {
  return tuple1 ( IR_TMP, n );
}


