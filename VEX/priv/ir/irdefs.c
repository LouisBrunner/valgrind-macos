
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (ir/irdefs.c) is                              ---*/
/*--- Copyright (c) 2004 OpenWorks LLP.  All rights reserved. ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

#include "libvex_basictypes.h"
#include "libvex_ir.h"
#include "libvex.h"

#include "main/vex_util.h"


/*---------------------------------------------------------------*/
/*--- Printing the IR                                         ---*/
/*---------------------------------------------------------------*/

void ppIRType ( IRType ty )
{
  switch (ty) {
    case Ity_INVALID: vex_printf("Ity_INVALID"); break;
    case Ity_Bit:     vex_printf( "Bit"); break;
    case Ity_I8:      vex_printf( "I8");  break;
    case Ity_I16:     vex_printf( "I16"); break;
    case Ity_I32:     vex_printf( "I32"); break;
    case Ity_I64:     vex_printf( "I64"); break;
    case Ity_F32:     vex_printf( "F32"); break;
    case Ity_F64:     vex_printf( "F64"); break;
    default: vex_printf("ty = 0x%x\n", (Int)ty);
             vpanic("ppIRType");
  }
}

void ppIRConst ( IRConst* con )
{
  switch (con->tag) {
    case Ico_Bit:   vex_printf( "%d:Bit",     con->Ico.Bit ? 1 : 0); break;
    case Ico_U8:    vex_printf( "0x%x:I8",    (UInt)(con->Ico.U8)); break;
    case Ico_U16:   vex_printf( "0x%x:I16",   (UInt)(con->Ico.U16)); break;
    case Ico_U32:   vex_printf( "0x%x:I32",   (UInt)(con->Ico.U32)); break;
    case Ico_U64:   vex_printf( "0x%llx:I64", (ULong)(con->Ico.U64)); break;
    case Ico_F64:   vex_printf( "(f64 value)"); break;
    case Ico_NaN64: vex_printf( "NaN:F64"); break;
    default: vpanic("ppIRConst");
  }
}

void ppIRTemp ( IRTemp tmp )
{
   if (tmp == INVALID_IRTEMP)
      vex_printf("INVALID_IRTEMP");
   else
      vex_printf( "t%d", (Int)tmp);
}

void ppIROp ( IROp op )
{
   Char* str; 
   IROp  base;
   switch (op) {
      case Iop_Add8 ... Iop_Add64:
         str = "Add"; base = Iop_Add8; break;
      case Iop_Sub8 ... Iop_Sub64:
         str = "Sub"; base = Iop_Sub8; break;
      case Iop_Mul8 ... Iop_Mul64:
         str = "Mul"; base = Iop_Mul8; break;
      case Iop_Or8 ... Iop_Or64:
         str = "Or"; base = Iop_Or8; break;
      case Iop_And8 ... Iop_And64:
         str = "And"; base = Iop_And8; break;
      case Iop_Xor8 ... Iop_Xor64:
         str = "Xor"; base = Iop_Xor8; break;
      case Iop_Shl8 ... Iop_Shl64:
         str = "Shl"; base = Iop_Shl8; break;
      case Iop_Shr8 ... Iop_Shr64:
         str = "Shr"; base = Iop_Shr8; break;
      case Iop_Sar8 ... Iop_Sar64:
         str = "Sar"; base = Iop_Sar8; break;
      case Iop_CmpEQ8 ... Iop_CmpEQ64:
         str = "CmpEQ"; base = Iop_CmpEQ8; break;
      case Iop_CmpNE8 ... Iop_CmpNE64:
         str = "CmpNE"; base = Iop_CmpNE8; break;
      case Iop_Neg8 ... Iop_Neg64:
         str = "Neg"; base = Iop_Neg8; break;
      case Iop_Not8 ... Iop_Not64:
         str = "Not"; base = Iop_Not8; break;
      /* other cases must explicitly "return;" */
      case Iop_8Uto16:   vex_printf("8Uto16");  return;
      case Iop_8Uto32:   vex_printf("8Uto32");  return;
      case Iop_16Uto32:  vex_printf("16Uto32"); return;
      case Iop_8Sto16:   vex_printf("8Sto16");  return;
      case Iop_8Sto32:   vex_printf("8Sto32");  return;
      case Iop_16Sto32:  vex_printf("16Sto32"); return;
      case Iop_32Sto64:  vex_printf("32Sto64"); return;
      case Iop_32to8:    vex_printf("32to8");   return;
      case Iop_32to1:    vex_printf("32to1");   return;
      case Iop_1Uto8:    vex_printf("1Uto8");   return;
      case Iop_1Uto32:   vex_printf("1Uto32");  return;

      case Iop_MullS8:   vex_printf("MullS8");  return;
      case Iop_MullS16:  vex_printf("MullS16"); return;
      case Iop_MullS32:  vex_printf("MullS32"); return;
      case Iop_MullU8:   vex_printf("MullU8");  return;
      case Iop_MullU16:  vex_printf("MullU16"); return;
      case Iop_MullU32:  vex_printf("MullU32"); return;

      case Iop_Clz32:    vex_printf("Clz32"); return;
      case Iop_Ctz32:    vex_printf("Ctz32"); return;

      case Iop_CmpLT32S: vex_printf("CmpLT32S"); return;
      case Iop_CmpLE32S: vex_printf("CmpLE32S"); return;
      case Iop_CmpLT32U: vex_printf("CmpLT32U"); return;
      case Iop_CmpLE32U: vex_printf("CmpLE32U"); return;

      case Iop_DivModU64to32: vex_printf("DivModU64to32"); return;
      case Iop_DivModS64to32: vex_printf("DivModS64to32"); return;

      case Iop_16HIto8:  vex_printf("16HIto8"); return;
      case Iop_16to8:    vex_printf("16to8");   return;
      case Iop_8HLto16:  vex_printf("8HLto16"); return;

      case Iop_32HIto16: vex_printf("32HIto16"); return;
      case Iop_32to16:   vex_printf("32to16");   return;
      case Iop_16HLto32: vex_printf("16HLto32"); return;

      case Iop_64HIto32: vex_printf("64HIto32"); return;
      case Iop_64to32:   vex_printf("64to32");   return;
      case Iop_32HLto64: vex_printf("32HLto64"); return;

      case Iop_AddF64:   vex_printf("AddF64"); return;
      case Iop_SubF64:   vex_printf("SubF64"); return;
      case Iop_MulF64:   vex_printf("MulF64"); return;
      case Iop_DivF64:   vex_printf("DivF64"); return;
      case Iop_I32toF64: vex_printf("I32toF64"); return;
      case Iop_F64toI32: vex_printf("F64toI32"); return;
      case Iop_F64toI16: vex_printf("F64toI16"); return;
      case Iop_F32toF64: vex_printf("F32toF64"); return;
      case Iop_F64toF32: vex_printf("F64toF32"); return;

      default:           vpanic("ppIROp(1)");
   }
  
   switch (op - base) {
      case 0: vex_printf(str); vex_printf("8"); break;
      case 1: vex_printf(str); vex_printf("16"); break;
      case 2: vex_printf(str); vex_printf("32"); break;
      case 3: vex_printf(str); vex_printf("64"); break;
      default: vpanic("ppIROp(2)");
   }
}

void ppIRExpr ( IRExpr* e )
{
  Int i;
  switch (e->tag) {
    case Iex_Binder:
      vex_printf("BIND-%d", e->Iex.Binder.binder);
      break;
    case Iex_Get:
      vex_printf( "GET(%d,", e->Iex.Get.offset);
      ppIRType(e->Iex.Get.ty);
      vex_printf(")");
      break;
    case Iex_GetI:
      vex_printf( "GETI[%d,%d](", e->Iex.GetI.minoff, e->Iex.GetI.maxoff);
      ppIRExpr(e->Iex.GetI.offset);
      vex_printf("):");
      ppIRType(e->Iex.GetI.ty);
      break;
    case Iex_Tmp:
      ppIRTemp(e->Iex.Tmp.tmp);
      break;
    case Iex_Binop:
      ppIROp(e->Iex.Binop.op);
      vex_printf( "(" );
      ppIRExpr(e->Iex.Binop.arg1);
      vex_printf( "," );
      ppIRExpr(e->Iex.Binop.arg2);
      vex_printf( ")" );
      break;
    case Iex_Unop:
      ppIROp(e->Iex.Unop.op);
      vex_printf( "(" );
      ppIRExpr(e->Iex.Unop.arg);
      vex_printf( ")" );
      break;
    case Iex_LDle:
      vex_printf( "LDle:" );
      ppIRType(e->Iex.LDle.ty);
      vex_printf( "(" );
      ppIRExpr(e->Iex.LDle.addr);
      vex_printf( ")" );
      break;
    case Iex_Const:
      ppIRConst(e->Iex.Const.con);
      break;
    case Iex_CCall:
      vex_printf("%s(", e->Iex.CCall.name);
      for (i = 0; e->Iex.CCall.args[i] != NULL; i++) {
        ppIRExpr(e->Iex.CCall.args[i]);
        if (e->Iex.CCall.args[i+1] != NULL)
          vex_printf(",");
      }
      vex_printf("):");
      ppIRType(e->Iex.CCall.retty);
      break;
    case Iex_Mux0X:
      vex_printf("Mux0X(");
      ppIRExpr(e->Iex.Mux0X.cond);
      vex_printf(",");
      ppIRExpr(e->Iex.Mux0X.expr0);
      vex_printf(",");
      ppIRExpr(e->Iex.Mux0X.exprX);
      vex_printf(")");
      break;
    default:
      vpanic("ppIExpr");
  }
}

void ppIRStmt ( IRStmt* s )
{
  switch (s->tag) {
    case Ist_Put:
      vex_printf( "PUT(%d) = ", s->Ist.Put.offset);
      ppIRExpr(s->Ist.Put.expr);
      break;
    case Ist_PutI:
      vex_printf( "PUTI[%d,%d](", s->Ist.PutI.minoff, s->Ist.PutI.maxoff);
      ppIRExpr(s->Ist.PutI.offset);
      vex_printf( ") = " );
      ppIRExpr(s->Ist.PutI.expr);
      break;
    case Ist_Tmp:
      ppIRTemp(s->Ist.Tmp.tmp);
      vex_printf( " = " );
      ppIRExpr(s->Ist.Tmp.expr);
      break;
    case Ist_STle:
      vex_printf( "STle(");
      ppIRExpr(s->Ist.STle.addr);
      vex_printf( ") = ");
      ppIRExpr(s->Ist.STle.data);
      break;
    case Ist_Exit:
      vex_printf( "if (" );
      ppIRExpr(s->Ist.Exit.cond);
      vex_printf( ") goto ");
      ppIRConst(s->Ist.Exit.dst);
      break;
    default: 
      vpanic("ppIRStmt");
  }
}

void ppIRJumpKind ( IRJumpKind kind )
{
   switch (kind) {
      case Ijk_Boring:    vex_printf("Boring"); break;
      case Ijk_Call:      vex_printf("Call"); break;
      case Ijk_Ret:       vex_printf("Return"); break;
      case Ijk_ClientReq: vex_printf("ClientReq"); break;
      case Ijk_Syscall:   vex_printf("Syscall"); break;
      case Ijk_Yield:     vex_printf("Yield"); break;
      default:            vpanic("ppIRJumpKind");
  }
}

void ppIRTypeEnv ( IRTypeEnv* env ) {
   UInt i;
   for (i = 0; i < env->types_used; i++) {
      if (i % 8 == 0)
         vex_printf( "   ");
      ppIRTemp(i);
      vex_printf( ":");
      ppIRType(env->types[i]);
      if (i % 8 == 7) 
         vex_printf( "\n"); 
      else 
         vex_printf( "   ");
   }
   if (env->types_used > 0 && env->types_used % 8 != 7) 
      vex_printf( "\n"); 
}




void ppIRBB ( IRBB* bb )
{
   Int i;
   vex_printf("IRBB {\n");
   ppIRTypeEnv(bb->tyenv);
   vex_printf("\n");
   for (i = 0; i < bb->stmts_used; i++) {
      if (bb->stmts[i]) {
         vex_printf( "   ");
         ppIRStmt(bb->stmts[i]);
      }
      vex_printf( "\n");
   }
   vex_printf( "   goto {");
   ppIRJumpKind(bb->jumpkind);
   vex_printf( "} ");
   ppIRExpr( bb->next );
   vex_printf( "\n}\n");
}


/*---------------------------------------------------------------*/
/*--- Constructors                                            ---*/
/*---------------------------------------------------------------*/


/* Constructors -- IRConst */

IRConst* IRConst_Bit ( Bool bit )
{
   IRConst* c = LibVEX_Alloc(sizeof(IRConst));
   c->tag     = Ico_Bit;
   c->Ico.Bit = bit;
   return c;
}
IRConst* IRConst_U8 ( UChar u8 )
{
   IRConst* c = LibVEX_Alloc(sizeof(IRConst));
   c->tag     = Ico_U8;
   c->Ico.U8  = u8;
   return c;
}
IRConst* IRConst_U16 ( UShort u16 )
{
   IRConst* c = LibVEX_Alloc(sizeof(IRConst));
   c->tag     = Ico_U16;
   c->Ico.U16 = u16;
   return c;
}
IRConst* IRConst_U32 ( UInt u32 )
{
   IRConst* c = LibVEX_Alloc(sizeof(IRConst));
   c->tag     = Ico_U32;
   c->Ico.U32 = u32;
   return c;
}
IRConst* IRConst_U64 ( ULong u64 )
{
   IRConst* c = LibVEX_Alloc(sizeof(IRConst));
   c->tag     = Ico_U64;
   c->Ico.U64 = u64;
   return c;
}
IRConst* IRConst_F64 ( Double f64 )
{
   IRConst* c = LibVEX_Alloc(sizeof(IRConst));
   c->tag     = Ico_F64;
   c->Ico.F64 = f64;
   return c;
}
IRConst* IRConst_NaN64 ( void )
{
   IRConst* c = LibVEX_Alloc(sizeof(IRConst));
   c->tag     = Ico_NaN64;
   return c;
}


/* Constructors -- IRExpr */

IRExpr* IRExpr_Binder ( Int binder ) {
   IRExpr* e            = LibVEX_Alloc(sizeof(IRExpr));
   e->tag               = Iex_Binder;
   e->Iex.Binder.binder = binder;
   return e;
}
IRExpr* IRExpr_Get ( Int off, IRType ty ) {
   IRExpr* e         = LibVEX_Alloc(sizeof(IRExpr));
   e->tag            = Iex_Get;
   e->Iex.Get.offset = off;
   e->Iex.Get.ty     = ty;
   return e;
}
IRExpr* IRExpr_GetI ( IRExpr* off, IRType ty, 
                      UShort minoff, UShort maxoff ) {
   IRExpr* e          = LibVEX_Alloc(sizeof(IRExpr));
   e->tag             = Iex_GetI;
   e->Iex.GetI.offset = off;
   e->Iex.GetI.ty     = ty;
   e->Iex.GetI.minoff = minoff;
   e->Iex.GetI.maxoff = maxoff;
   return e;
}
IRExpr* IRExpr_Tmp ( IRTemp tmp ) {
   IRExpr* e      = LibVEX_Alloc(sizeof(IRExpr));
   e->tag         = Iex_Tmp;
   e->Iex.Tmp.tmp = tmp;
   return e;
}
IRExpr* IRExpr_Binop ( IROp op, IRExpr* arg1, IRExpr* arg2 ) {
   IRExpr* e         = LibVEX_Alloc(sizeof(IRExpr));
   e->tag            = Iex_Binop;
   e->Iex.Binop.op   = op;
   e->Iex.Binop.arg1 = arg1;
   e->Iex.Binop.arg2 = arg2;
   return e;
}
IRExpr* IRExpr_Unop ( IROp op, IRExpr* arg ) {
   IRExpr* e       = LibVEX_Alloc(sizeof(IRExpr));
   e->tag          = Iex_Unop;
   e->Iex.Unop.op  = op;
   e->Iex.Unop.arg = arg;
   return e;
}
IRExpr* IRExpr_LDle  ( IRType ty, IRExpr* addr ) {
   IRExpr* e        = LibVEX_Alloc(sizeof(IRExpr));
   e->tag           = Iex_LDle;
   e->Iex.LDle.ty   = ty;
   e->Iex.LDle.addr = addr;
   return e;
}
IRExpr* IRExpr_Const ( IRConst* con ) {
   IRExpr* e        = LibVEX_Alloc(sizeof(IRExpr));
   e->tag           = Iex_Const;
   e->Iex.Const.con = con;
   return e;
}
IRExpr* IRExpr_CCall ( Char* name, IRType retty, IRExpr** args ) {
   IRExpr* e          = LibVEX_Alloc(sizeof(IRExpr));
   e->tag             = Iex_CCall;
   e->Iex.CCall.name  = name;
   e->Iex.CCall.retty = retty;
   e->Iex.CCall.args  = args;
   return e;
}
IRExpr* IRExpr_Mux0X ( IRExpr* cond, IRExpr* expr0, IRExpr* exprX ) {
   IRExpr* e          = LibVEX_Alloc(sizeof(IRExpr));
   e->tag             = Iex_Mux0X;
   e->Iex.Mux0X.cond  = cond;
   e->Iex.Mux0X.expr0 = expr0;
   e->Iex.Mux0X.exprX = exprX;
   return e;
}


/* Constructors -- IRStmt */

IRStmt* IRStmt_Put ( Int off, IRExpr* value ) {
   IRStmt* s         = LibVEX_Alloc(sizeof(IRStmt));
   s->tag            = Ist_Put;
   s->Ist.Put.offset = off;
   s->Ist.Put.expr   = value;
   return s;
}
IRStmt* IRStmt_PutI ( IRExpr* off, IRExpr* value, 
                      UShort minoff, UShort maxoff ) {
   IRStmt* s          = LibVEX_Alloc(sizeof(IRStmt));
   s->tag             = Ist_PutI;
   s->Ist.PutI.offset = off;
   s->Ist.PutI.expr   = value;
   s->Ist.PutI.minoff = minoff;
   s->Ist.PutI.maxoff = maxoff;
   return s;
}
IRStmt* IRStmt_Tmp ( IRTemp tmp, IRExpr* expr ) {
   IRStmt* s       = LibVEX_Alloc(sizeof(IRStmt));
   s->tag          = Ist_Tmp;
   s->Ist.Tmp.tmp  = tmp;
   s->Ist.Tmp.expr = expr;
   return s;
}
IRStmt* IRStmt_STle ( IRExpr* addr, IRExpr* value ) {
   IRStmt* s        = LibVEX_Alloc(sizeof(IRStmt));
   s->tag           = Ist_STle;
   s->Ist.STle.addr = addr;
   s->Ist.STle.data = value;
   return s;
}
IRStmt* IRStmt_Exit ( IRExpr* cond, IRConst* dst ) {
   IRStmt* s        = LibVEX_Alloc(sizeof(IRStmt));
   s->tag           = Ist_Exit;
   s->Ist.Exit.cond = cond;
   s->Ist.Exit.dst  = dst;
   return s;
}

/* Constructors -- IRBB (sort of) */

IRBB* emptyIRBB ( void )
{
   IRBB* bb       = LibVEX_Alloc(sizeof(IRBB));
   bb->tyenv      = emptyIRTypeEnv();
   bb->stmts_used = 0;
   bb->stmts_size = 8;
   bb->stmts      = LibVEX_Alloc(bb->stmts_size * sizeof(IRStmt*));
   bb->next       = NULL;
   bb->jumpkind   = Ijk_Boring;
   return bb;
}

void addStmtToIRBB ( IRBB* bb, IRStmt* st )
{
   Int i;
   if (bb->stmts_used == bb->stmts_size) {
      IRStmt** stmts2 = LibVEX_Alloc(2 * bb->stmts_size * sizeof(IRStmt*));
      for (i = 0; i < bb->stmts_size; i++)
         stmts2[i] = bb->stmts[i];
      bb->stmts = stmts2;
      bb->stmts_size *= 2;
   }
   vassert(bb->stmts_used < bb->stmts_size);
   bb->stmts[bb->stmts_used] = st;
   bb->stmts_used++;
}


/*---------------------------------------------------------------*/
/*--- Primop types                                            ---*/
/*---------------------------------------------------------------*/

static
void typeOfPrimop ( IROp op, IRType* t_dst, IRType* t_arg1, IRType* t_arg2 )
{
#  define UNARY(_td,_ta1)         \
      *t_dst = (_td); *t_arg1 = (_ta1); break
#  define BINARY(_td,_ta1,_ta2)   \
     *t_dst = (_td); *t_arg1 = (_ta1); *t_arg2 = (_ta2); break
#  define COMPARISON(_ta)         \
     *t_dst = Ity_Bit; *t_arg1 = *t_arg2 = (_ta); break;

   *t_dst  = Ity_INVALID;
   *t_arg1 = Ity_INVALID;
   *t_arg2 = Ity_INVALID;
   switch (op) {
      case Iop_Add8: case Iop_Sub8: //case Iop_Adc8: case Iop_Sbb8:
      case Iop_Mul8: case Iop_Or8:  case Iop_And8: case Iop_Xor8:
         BINARY(Ity_I8,Ity_I8,Ity_I8);

      case Iop_Add16: case Iop_Sub16: //case Iop_Adc16: case Iop_Sbb16:
      case Iop_Mul16: case Iop_Or16:  case Iop_And16: case Iop_Xor16:
         BINARY(Ity_I16,Ity_I16,Ity_I16);

      case Iop_Add32: case Iop_Sub32: //case Iop_Adc32: case Iop_Sbb32:
      case Iop_Mul32: case Iop_Or32:  case Iop_And32: case Iop_Xor32:
         BINARY(Ity_I32,Ity_I32,Ity_I32);

      case Iop_Add64: case Iop_Sub64: //case Iop_Adc64: case Iop_Sbb64:
      case Iop_Mul64: case Iop_Or64:  case Iop_And64: case Iop_Xor64:
         BINARY(Ity_I64,Ity_I64,Ity_I64);

      case Iop_Shl8: case Iop_Shr8: case Iop_Sar8:
         BINARY(Ity_I8,Ity_I8,Ity_I8);
      case Iop_Shl16: case Iop_Shr16: case Iop_Sar16:
         BINARY(Ity_I16,Ity_I16,Ity_I8);
      case Iop_Shl32: case Iop_Shr32: case Iop_Sar32:
         BINARY(Ity_I32,Ity_I32,Ity_I8);
      case Iop_Shl64: case Iop_Shr64: case Iop_Sar64:
         BINARY(Ity_I64,Ity_I64,Ity_I8);

      case Iop_Not8: case Iop_Neg8:
         UNARY(Ity_I8,Ity_I8);
      case Iop_Not16: case Iop_Neg16:
         UNARY(Ity_I16,Ity_I16);
      case Iop_Not32: case Iop_Neg32:
         UNARY(Ity_I32,Ity_I32);
      case Iop_Not64: case Iop_Neg64:
         UNARY(Ity_I64,Ity_I64);

      case Iop_CmpEQ8: case Iop_CmpNE8:
         COMPARISON(Ity_I8);
      case Iop_CmpEQ16: case Iop_CmpNE16:
         COMPARISON(Ity_I16);
      case Iop_CmpEQ32: case Iop_CmpNE32:
      case Iop_CmpLT32S:
      case Iop_CmpLE32S:
      case Iop_CmpLT32U:
      case Iop_CmpLE32U:
         COMPARISON(Ity_I32);
      case Iop_CmpEQ64: case Iop_CmpNE64:
         COMPARISON(Ity_I64);

      case Iop_MullU8: case Iop_MullS8:
         BINARY(Ity_I16,Ity_I8,Ity_I8);
      case Iop_MullU16: case Iop_MullS16:
         BINARY(Ity_I32,Ity_I16,Ity_I16);
      case Iop_MullU32: case Iop_MullS32:
         BINARY(Ity_I64,Ity_I32,Ity_I32);

      case Iop_Clz32:
      case Iop_Ctz32:
         UNARY(Ity_I32,Ity_I32);

      case Iop_DivModU64to32:
      case Iop_DivModS64to32:
         BINARY(Ity_I64,Ity_I64,Ity_I32);

      case Iop_16HIto8: case Iop_16to8:
         UNARY(Ity_I8,Ity_I16);
      case Iop_8HLto16:
         BINARY(Ity_I16,Ity_I8,Ity_I8);

      case Iop_32HIto16: case Iop_32to16:
         UNARY(Ity_I16,Ity_I32);
      case Iop_16HLto32:
         BINARY(Ity_I32,Ity_I16,Ity_I16);

      case Iop_64HIto32: case Iop_64to32:
         UNARY(Ity_I32, Ity_I64);
      case Iop_32HLto64:
         BINARY(Ity_I64,Ity_I32,Ity_I32);

      case Iop_1Uto8:   UNARY(Ity_I8,Ity_Bit);
      case Iop_1Uto32:  UNARY(Ity_I32,Ity_Bit);
      case Iop_32to1:   UNARY(Ity_Bit,Ity_I32);
      case Iop_8Uto32:  UNARY(Ity_I32,Ity_I8);
      case Iop_8Sto32:  UNARY(Ity_I32,Ity_I8);
      case Iop_16Uto32: UNARY(Ity_I32,Ity_I16);
      case Iop_16Sto32: UNARY(Ity_I32,Ity_I16);
      case Iop_32Sto64: UNARY(Ity_I64,Ity_I32);
      case Iop_32to8:   UNARY(Ity_I8,Ity_I32);

      case Iop_AddF64: case Iop_SubF64: 
      case Iop_MulF64: case Iop_DivF64:
         BINARY(Ity_F64,Ity_F64,Ity_F64);

      case Iop_I32toF64: UNARY(Ity_F64,Ity_I32);

      case Iop_F64toI32: BINARY(Ity_I32, Ity_I32,Ity_F64);
      case Iop_F64toI16: BINARY(Ity_I16, Ity_I32,Ity_F64);

      case Iop_F32toF64: UNARY(Ity_F64,Ity_F32);
      case Iop_F64toF32: UNARY(Ity_F32,Ity_F64);

      default:
         ppIROp(op);
         vpanic("typeOfPrimop");
   }
#  undef UNARY
#  undef BINARY
#  undef COMPARISON
}


/*---------------------------------------------------------------*/
/*--- Helper functions for the IR -- IR Type Environments     ---*/
/*---------------------------------------------------------------*/

/* Make a new, empty IRTypeEnv. */

IRTypeEnv* emptyIRTypeEnv ( void )
{
   IRTypeEnv* env   = LibVEX_Alloc(sizeof(IRTypeEnv));
   env->types       = LibVEX_Alloc(8 * sizeof(IRType));
   env->types_size  = 8;
   env->types_used  = 0;
   return env;
}

/* Make an exact copy of the given IRTypeEnv, usually so we can
   add new stuff to the copy without messing up the original. */

IRTypeEnv* copyIRTypeEnv ( IRTypeEnv* src )
{
   Int i;
   IRTypeEnv* dst = LibVEX_Alloc(sizeof(IRTypeEnv));
   dst->types_size = src->types_size;
   dst->types_used = src->types_used;
   dst->types = LibVEX_Alloc(dst->types_size * sizeof(IRType));
   for (i = 0; i < src->types_used; i++)
      dst->types[i] = src->types[i];
   return dst;
}

/* Allocate a new IRTemp, given its type. */

IRTemp newIRTemp ( IRTypeEnv* env, IRType ty )
{
   vassert(env);
   vassert(env->types_used >= 0);
   vassert(env->types_size >= 0);
   vassert(env->types_used <= env->types_size);
   if (env->types_used < env->types_size) {
      env->types[env->types_used] = ty;
      return env->types_used++;
   } else {
      Int i;
      Int new_size = env->types_size==0 ? 8 : 2*env->types_size;
      IRType* new_types 
         = LibVEX_Alloc(new_size * sizeof(IRType));
      for (i = 0; i < env->types_used; i++)
         new_types[i] = env->types[i];
      env->types      = new_types;
      env->types_size = new_size;
      return newIRTemp(env, ty);
   }
}

/* Find the type of a temporary previously allocated in an
   environment. */

IRType lookupIRTypeEnv ( IRTypeEnv* env, IRTemp tmp )
{
   vassert(tmp >= 0);
   vassert(tmp < env->types_used);
   return env->types[tmp];
}


/*---------------------------------------------------------------*/
/*--- Helper functions for the IR -- finding types of exprs   ---*/
/*---------------------------------------------------------------*/

IRType typeOfIRConst ( IRConst* con )
{
   switch (con->tag) {
      case Ico_Bit:   return Ity_Bit;
      case Ico_U8:    return Ity_I8;
      case Ico_U16:   return Ity_I16;
      case Ico_U32:   return Ity_I32;
      case Ico_U64:   return Ity_I64;
      case Ico_F64:   return Ity_F64;
      case Ico_NaN64: return Ity_F64;
      default: vpanic("typeOfIRConst");
   }
}

IRType typeOfIRExpr ( IRTypeEnv* tyenv, IRExpr* e )
{
   IRType t_dst, t_arg1, t_arg2;
   switch (e->tag) {
      case Iex_LDle:
         return e->Iex.LDle.ty;
      case Iex_Get:
         return e->Iex.Get.ty;
      case Iex_GetI:
         return e->Iex.GetI.ty;
      case Iex_Tmp:
         return lookupIRTypeEnv(tyenv, e->Iex.Tmp.tmp);
      case Iex_Const:
	 return typeOfIRConst(e->Iex.Const.con);
      case Iex_Binop:
         typeOfPrimop(e->Iex.Binop.op, &t_dst, &t_arg1, &t_arg2);
         return t_dst;
      case Iex_Unop:
         typeOfPrimop(e->Iex.Unop.op, &t_dst, &t_arg1, &t_arg2);
         return t_dst;
      case Iex_CCall:
         return e->Iex.CCall.retty;
      case Iex_Mux0X:
         return typeOfIRExpr(tyenv, e->Iex.Mux0X.expr0);
      case Iex_Binder:
         vpanic("typeOfIRExpr: Binder is not a valid expression");
      default:
         ppIRExpr(e);
         vpanic("typeOfIRExpr");
   }
}

/* Is this any value actually in the enumeration 'IRType' ? */
Bool isPlausibleType ( IRType ty )
{
   switch (ty) {
      case Ity_INVALID: case Ity_Bit:
      case Ity_I8: case Ity_I16: case Ity_I32: case Ity_I64: 
      case Ity_F32: case Ity_F64:
         return True;
      default: 
         return False;
   }
}


/*---------------------------------------------------------------*/
/*--- Sanity checking                                         ---*/
/*---------------------------------------------------------------*/

/* Checks:

   Everything is type-consistent.  No ill-typed anything.
   The target address at the end of the BB is a 32- or 64-
   bit expression, depending on the guest's word size.

   Each temp is assigned only once, before its uses.
 */

static
__attribute((noreturn))
void sanityCheckFail ( IRBB* bb, IRStmt* stmt, Char* what )
{
   vex_printf("\nIR SANITY CHECK FAILURE\n\n");
   ppIRBB(bb);
   if (stmt) {
      vex_printf("\nIN STATEMENT:\n\n");
      ppIRStmt(stmt);
   }
   vex_printf("\n\nERROR = %s\n\n", what );
   vpanic("sanityCheckFail: exiting due to bad IR");
}


/* Traverse a Stmt/Expr, inspecting IRTemp uses.  Report any out of
   range ones.  Report any which are read and for which the current
   def_count is zero. */

static
void useBeforeDef_Expr ( IRBB* bb, IRStmt* stmt, IRExpr* expr, Int* def_counts )
{
   Int i;
   switch (expr->tag) {
      case Iex_Get: 
         break;
      case Iex_GetI:
         useBeforeDef_Expr(bb,stmt,expr->Iex.GetI.offset,def_counts);
         break;
      case Iex_Tmp: 
         if (expr->Iex.Tmp.tmp < 0 || expr->Iex.Tmp.tmp >= bb->tyenv->types_used)
            sanityCheckFail(bb,stmt, "out of range Temp in IRExpr");
         if (def_counts[expr->Iex.Tmp.tmp] < 1)
            sanityCheckFail(bb,stmt, "IRTemp use before def in IRExpr");
         break;
      case Iex_Binop:
         useBeforeDef_Expr(bb,stmt,expr->Iex.Binop.arg1,def_counts);
         useBeforeDef_Expr(bb,stmt,expr->Iex.Binop.arg2,def_counts);
         break;
      case Iex_Unop:
         useBeforeDef_Expr(bb,stmt,expr->Iex.Unop.arg,def_counts);
         break;
      case Iex_LDle:
         useBeforeDef_Expr(bb,stmt,expr->Iex.LDle.addr,def_counts);
         break;
      case Iex_Const:
         break;
      case Iex_CCall:
         for (i = 0; expr->Iex.CCall.args[i]; i++)
            useBeforeDef_Expr(bb,stmt,expr->Iex.CCall.args[i],def_counts);
         break;
      case Iex_Mux0X:
         useBeforeDef_Expr(bb,stmt,expr->Iex.Mux0X.cond,def_counts);
         useBeforeDef_Expr(bb,stmt,expr->Iex.Mux0X.expr0,def_counts);
         useBeforeDef_Expr(bb,stmt,expr->Iex.Mux0X.exprX,def_counts);
         break;
      default:
         vpanic("useBeforeDef_Expr");
   }
}

static
void useBeforeDef_Stmt ( IRBB* bb, IRStmt* stmt, Int* def_counts )
{
   switch (stmt->tag) {
      case Ist_Put:
         useBeforeDef_Expr(bb,stmt,stmt->Ist.Put.expr,def_counts);
         break;
      case Ist_PutI:
         useBeforeDef_Expr(bb,stmt,stmt->Ist.PutI.offset,def_counts);
         useBeforeDef_Expr(bb,stmt,stmt->Ist.PutI.expr,def_counts);
         break;
      case Ist_Tmp:
         useBeforeDef_Expr(bb,stmt,stmt->Ist.Tmp.expr,def_counts);
         break;
      case Ist_STle:
         useBeforeDef_Expr(bb,stmt,stmt->Ist.STle.addr,def_counts);
         useBeforeDef_Expr(bb,stmt,stmt->Ist.STle.data,def_counts);
         break;
      case Ist_Exit:
         useBeforeDef_Expr(bb,stmt,stmt->Ist.Exit.cond,def_counts);
         break;
      default: 
         vpanic("useBeforeDef_Stmt");
   }
}

static
void tcExpr ( IRBB* bb, IRStmt* stmt, IRExpr* expr, IRType gWordTy )
{
   Int        i;
   IRType     t_dst, t_arg1, t_arg2;
   IRTypeEnv* tyenv = bb->tyenv;
   switch (expr->tag) {
      case Iex_Get:
      case Iex_Tmp:
         break;
      case Iex_GetI:
         tcExpr(bb,stmt, expr->Iex.GetI.offset, gWordTy );
         if (typeOfIRExpr(tyenv,expr->Iex.GetI.offset) != Ity_I32)
            sanityCheckFail(bb,stmt,"IRExpr.GetI.offset: not :: Ity_I32");
         break;
      case Iex_Binop: {
         IRType ttarg1, ttarg2;
         tcExpr(bb,stmt, expr->Iex.Binop.arg1, gWordTy );
         tcExpr(bb,stmt, expr->Iex.Binop.arg2, gWordTy );
         typeOfPrimop(expr->Iex.Binop.op, &t_dst, &t_arg1, &t_arg2);
         if (t_arg1 == Ity_INVALID || t_arg2 == Ity_INVALID) {
            vex_printf(" op name: " );
            ppIROp(expr->Iex.Binop.op);
            vex_printf("\n");
            sanityCheckFail(bb,stmt,
               "Iex.Binop: wrong arity op\n"
               "... name of op precedes BB printout\n");
         }
         ttarg1 = typeOfIRExpr(tyenv, expr->Iex.Binop.arg1);
         ttarg2 = typeOfIRExpr(tyenv, expr->Iex.Binop.arg2);
         if (t_arg1 != ttarg1 || t_arg2 != ttarg2) {
            vex_printf(" op name: ");
            ppIROp(expr->Iex.Binop.op);
            vex_printf("\n");
            vex_printf(" op type is (");
            ppIRType(t_arg1);
            vex_printf(",");
            ppIRType(t_arg2);
            vex_printf(") -> ");
            ppIRType (t_dst);
            vex_printf("\narg tys are (");
            ppIRType(ttarg1);
            vex_printf(",");
            ppIRType(ttarg2);
            vex_printf(")\n");
            sanityCheckFail(bb,stmt,
               "Iex.Binop: arg tys don't match op tys\n"
               "... additional details precede BB printout\n");
	 }
         break;
      }
      case Iex_Unop:
         tcExpr(bb,stmt, expr->Iex.Unop.arg, gWordTy );
         typeOfPrimop(expr->Iex.Binop.op, &t_dst, &t_arg1, &t_arg2);
         if (t_arg1 == Ity_INVALID || t_arg2 != Ity_INVALID)
            sanityCheckFail(bb,stmt,"Iex.Unop: wrong arity op");
         if (t_arg1 != typeOfIRExpr(tyenv, expr->Iex.Unop.arg))
            sanityCheckFail(bb,stmt,"Iex.Unop: arg ty doesn't match op ty");
         break;
      case Iex_LDle:
         tcExpr(bb,stmt, expr->Iex.LDle.addr, gWordTy);
         if (typeOfIRExpr(tyenv, expr->Iex.LDle.addr) != gWordTy)
            sanityCheckFail(bb,stmt,"Iex.LDle.addr: not :: guest word type");
         break;
      case Iex_CCall:
         for (i = 0; expr->Iex.CCall.args[i]; i++)
            tcExpr(bb,stmt, expr->Iex.CCall.args[i], gWordTy);
         if (expr->Iex.CCall.retty == Ity_Bit)
            sanityCheckFail(bb,stmt,"Iex.CCall.retty: cannot return :: Ity_Bit");
         for (i = 0; expr->Iex.CCall.args[i]; i++)
            if (typeOfIRExpr(tyenv, expr->Iex.CCall.args[i]) == Ity_Bit)
               sanityCheckFail(bb,stmt,"Iex.CCall.arg: arg :: Ity_Bit");
         break;
      case Iex_Const:
         break;
      case Iex_Mux0X:
         tcExpr(bb,stmt, expr->Iex.Mux0X.cond, gWordTy);
         tcExpr(bb,stmt, expr->Iex.Mux0X.expr0, gWordTy);
         tcExpr(bb,stmt, expr->Iex.Mux0X.exprX, gWordTy);
         if (typeOfIRExpr(tyenv, expr->Iex.Mux0X.cond) != Ity_I8)
            sanityCheckFail(bb,stmt,"Iex.Mux0X.cond: cond :: Ity_I8");
         if (typeOfIRExpr(tyenv, expr->Iex.Mux0X.expr0)
             != typeOfIRExpr(tyenv, expr->Iex.Mux0X.exprX))
            sanityCheckFail(bb,stmt,"Iex.Mux0X: expr0/exprX mismatch");
         break;
       default: 
         vpanic("tcExpr");
   }
}


static
void tcStmt ( IRBB* bb, IRStmt* stmt, IRType gWordTy )
{
   IRTypeEnv* tyenv = bb->tyenv;
   switch (stmt->tag) {
      case Ist_Put:
          tcExpr( bb, stmt, stmt->Ist.Put.expr, gWordTy );
          if (typeOfIRExpr(tyenv,stmt->Ist.Put.expr) == Ity_Bit)
             sanityCheckFail(bb,stmt,"IRStmt.Put.expr: cannot Put :: Ity_Bit");
          break;
      case Ist_PutI:
          tcExpr( bb, stmt, stmt->Ist.PutI.expr, gWordTy );
          tcExpr( bb, stmt, stmt->Ist.PutI.offset, gWordTy );
          if (typeOfIRExpr(tyenv,stmt->Ist.PutI.expr) == Ity_Bit)
             sanityCheckFail(bb,stmt,"IRStmt.PutI.expr: cannot PutI :: Ity_Bit");
          if (typeOfIRExpr(tyenv,stmt->Ist.PutI.offset) != Ity_I32)
             sanityCheckFail(bb,stmt,"IRStmt.PutI.offset: not :: Ity_I32");
          break;
      case Ist_Tmp:
         tcExpr( bb, stmt, stmt->Ist.Tmp.expr, gWordTy );
         if (lookupIRTypeEnv(tyenv, stmt->Ist.Tmp.tmp)
             != typeOfIRExpr(tyenv, stmt->Ist.Tmp.expr))
            sanityCheckFail(bb,stmt,"IRStmt.Put.Tmp: tmp and expr do not match");
         break;
      case Ist_STle:
         tcExpr( bb, stmt, stmt->Ist.STle.addr, gWordTy );
         tcExpr( bb, stmt, stmt->Ist.STle.data, gWordTy );
         if (typeOfIRExpr(tyenv, stmt->Ist.STle.addr) != gWordTy)
            sanityCheckFail(bb,stmt,"IRStmt.STle.addr: not :: guest word type");
         if (typeOfIRExpr(tyenv, stmt->Ist.STle.data) == Ity_Bit)
            sanityCheckFail(bb,stmt,"IRStmt.STle.data: cannot STle :: Ity_Bit");
         break;
      case Ist_Exit:
         tcExpr( bb, stmt, stmt->Ist.Exit.cond, gWordTy );
         if (typeOfIRExpr(tyenv,stmt->Ist.Exit.cond) != Ity_Bit)
            sanityCheckFail(bb,stmt,"IRStmt.Exit.cond: not :: Ity_Bit");
         if (typeOfIRConst(stmt->Ist.Exit.dst) != gWordTy)
            sanityCheckFail(bb,stmt,"IRStmt.Exit.dst: not :: guest word type");
         break;
      default:
         vpanic("tcStmt");
   }
}

void sanityCheckIRBB ( IRBB* bb, IRType guest_word_size )
{
   Int     i;
   IRStmt* stmt;
   Int     n_temps    = bb->tyenv->types_used;
   Int*    def_counts = LibVEX_Alloc(n_temps * sizeof(Int));

   vassert(guest_word_size == Ity_I32
	   || guest_word_size == Ity_I64);

   if (bb->stmts_used < 0 || bb->stmts_size < 8
       || bb->stmts_used > bb->stmts_size)
      /* this BB is so strange we can't even print it */
      vpanic("sanityCheckIRBB: stmts array limits wierd");

   /* Ensure each temp has a plausible type. */
   for (i = 0; i < n_temps; i++) {
      IRType ty = lookupIRTypeEnv(bb->tyenv,(IRTemp)i);
      if (!isPlausibleType(ty)) {
         vex_printf("Temp t%d declared with implausible type 0x%x\n",
                    i, (UInt)ty);
         sanityCheckFail(bb,NULL,"Temp declared with implausible type");
      }
   }

   /* Count the defs of each temp.  Only one def is allowed.
      Also, check that each used temp has already been defd. */

   for (i = 0; i < n_temps; i++)
      def_counts[i] = 0;

   for (i = 0; i < bb->stmts_used; i++) {
      stmt = bb->stmts[i];
      if (!stmt)
         continue;
      useBeforeDef_Stmt(bb,stmt,def_counts);
      if (stmt->tag == Ist_Tmp) {
         if (stmt->Ist.Tmp.tmp < 0 || stmt->Ist.Tmp.tmp >= n_temps)
            sanityCheckFail(bb, stmt, "Invalid temp in Tmp assignment");
         def_counts[stmt->Ist.Tmp.tmp]++;
         if (def_counts[stmt->Ist.Tmp.tmp] > 1)
            sanityCheckFail(bb, stmt, "Tmp assigned more than once");
      }
   }

   /* Typecheck everything. */
   for (i = 0; i < bb->stmts_used; i++)
      if (bb->stmts[i])
         tcStmt( bb, bb->stmts[i], guest_word_size );
   if (typeOfIRExpr(bb->tyenv,bb->next) != guest_word_size)
      sanityCheckFail(bb, NULL, "bb->next field has wrong type");
}


/*---------------------------------------------------------------*/
/*--- end                                         ir/irdefs.c ---*/
/*---------------------------------------------------------------*/
