
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (ir/irdefs.c) is                              ---*/
/*--- Copyright (c) 2004 OpenWorks LLP.  All rights reserved. ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of LibVEX, a library for dynamic binary
   instrumentation and translation.

   Copyright (C) 2004 OpenWorks, LLP.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; Version 2 dated June 1991 of the
   license.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE, or liability
   for damages.  See the GNU General Public License for more details.

   Neither the names of the U.S. Department of Energy nor the
   University of California nor the names of its contributors may be
   used to endorse or promote products derived from this software
   without prior written permission.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
   USA.
*/

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
      case Ico_Bit:  vex_printf( "%d:Bit",       con->Ico.Bit ? 1 : 0); break;
      case Ico_U8:   vex_printf( "0x%x:I8",      (UInt)(con->Ico.U8)); break;
      case Ico_U16:  vex_printf( "0x%x:I16",     (UInt)(con->Ico.U16)); break;
      case Ico_U32:  vex_printf( "0x%x:I32",     (UInt)(con->Ico.U32)); break;
      case Ico_U64:  vex_printf( "0x%llx:I64",   (ULong)(con->Ico.U64)); break;
      case Ico_F64:  vex_printf( "F64{0x%llx}",  *(ULong*)(&con->Ico.F64));
                     break;
      case Ico_F64i: vex_printf( "F64i{0x%llx}", con->Ico.F64i); break;
      default: vpanic("ppIRConst");
   }
}

void ppIRCallee ( IRCallee* ce )
{
   vex_printf("%s", ce->name);
   if (ce->regparms > 0)
      vex_printf("[rp=%d]", ce->regparms);
   if (ce->mcx_mask > 0)
      vex_printf("[mcx=0x%x]", ce->mcx_mask);
   vex_printf("{%p}", (void*)ce->addr);
}

void ppIRArray ( IRArray* arr )
{
   vex_printf("(%d:%dx", arr->base, arr->nElems);
   ppIRType(arr->elemTy);
   vex_printf(")");
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
      case Iop_32Uto64:  vex_printf("32Uto64"); return;
      case Iop_32to8:    vex_printf("32to8");   return;

      case Iop_Not1:     vex_printf("Not1");    return;
      case Iop_32to1:    vex_printf("32to1");   return;
      case Iop_1Uto8:    vex_printf("1Uto8");   return;
      case Iop_1Uto32:   vex_printf("1Uto32");  return;
      case Iop_1Sto8:    vex_printf("1Sto8");  return;
      case Iop_1Sto16:   vex_printf("1Sto16");  return;
      case Iop_1Sto32:   vex_printf("1Sto32");  return;
      case Iop_1Sto64:   vex_printf("1Sto64");  return;

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

      case Iop_AddF64:    vex_printf("AddF64"); return;
      case Iop_SubF64:    vex_printf("SubF64"); return;
      case Iop_MulF64:    vex_printf("MulF64"); return;
      case Iop_DivF64:    vex_printf("DivF64"); return;

      case Iop_ScaleF64:      vex_printf("ScaleF64"); return;
      case Iop_AtanF64:       vex_printf("AtanF64"); return;
      case Iop_Yl2xF64:       vex_printf("Yl2xF64"); return;
      case Iop_Yl2xp1F64:     vex_printf("Yl2xp1F64"); return;
      case Iop_PRemF64:       vex_printf("PRemF64"); return;
      case Iop_PRemC3210F64:  vex_printf("PRemC3210F64"); return;
      case Iop_PRem1F64:      vex_printf("PRem1F64"); return;
      case Iop_PRem1C3210F64: vex_printf("PRem1C3210F64"); return;
      case Iop_NegF64:        vex_printf("NegF64"); return;
      case Iop_SqrtF64:       vex_printf("SqrtF64"); return;

      case Iop_AbsF64:    vex_printf("AbsF64"); return;
      case Iop_SinF64:    vex_printf("SinF64"); return;
      case Iop_CosF64:    vex_printf("CosF64"); return;
      case Iop_TanF64:    vex_printf("TanF64"); return;
      case Iop_2xm1F64:   vex_printf("2xm1F64"); return;

      case Iop_CmpF64:    vex_printf("CmpF64"); return;

      case Iop_I32toF64: vex_printf("I32toF64"); return;
      case Iop_I64toF64: vex_printf("I64toF64"); return;

      case Iop_F64toI64: vex_printf("F64toI64"); return;
      case Iop_F64toI32: vex_printf("F64toI32"); return;
      case Iop_F64toI16: vex_printf("F64toI16"); return;
      case Iop_RoundF64: vex_printf("RoundF64"); return;

      case Iop_F32toF64: vex_printf("F32toF64"); return;
      case Iop_F64toF32: vex_printf("F64toF32"); return;

      case Iop_ReinterpF64asI64: vex_printf("ReinterpF64asI64"); return;
      case Iop_ReinterpI64asF64: vex_printf("ReinterpI64asF64"); return;

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
      vex_printf( "GETI" );
      ppIRArray(e->Iex.GetI.descr);
      vex_printf("[");
      ppIRExpr(e->Iex.GetI.ix);
      vex_printf(",%d]", e->Iex.GetI.bias);
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
      ppIRCallee(e->Iex.CCall.cee);
      vex_printf("(");
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

void ppIREffect ( IREffect fx )
{
   switch (fx) {
      case Ifx_None:   vex_printf("noFX"); return;
      case Ifx_Read:   vex_printf("RdFX"); return;
      case Ifx_Write:  vex_printf("WrFX"); return;
      case Ifx_Modify: vex_printf("MoFX"); return;
      default: vpanic("ppIREffect");
   }
}

void ppIRDirty ( IRDirty* d )
{
   Int i;
   if (d->tmp != INVALID_IRTEMP) {
      ppIRTemp(d->tmp);
      vex_printf(" = ");
   }
   vex_printf("DIRTY ");
   ppIRExpr(d->guard);
   if (d->needsBBP)
      vex_printf(" NeedsBBP");
   if (d->mFx != Ifx_None) {
      vex_printf(" ");
      ppIREffect(d->mFx);
      vex_printf("-mem(");
      ppIRExpr(d->mAddr);
      vex_printf(",%d)", d->mSize);
   }
   for (i = 0; i < d->nFxState; i++) {
      vex_printf(" ");
      ppIREffect(d->fxState[i].fx);
      vex_printf("-gst(%d,%d)", d->fxState[i].offset, d->fxState[i].size);
   }
   vex_printf(" ::: ");
   ppIRCallee(d->cee);
   vex_printf("(");
   for (i = 0; d->args[i] != NULL; i++) {
      ppIRExpr(d->args[i]);
      if (d->args[i+1] != NULL) {
         vex_printf(",");
      }
   }
   vex_printf(")");
}

void ppIRStmt ( IRStmt* s )
{
   switch (s->tag) {
      case Ist_Put:
         vex_printf( "PUT(%d) = ", s->Ist.Put.offset);
         ppIRExpr(s->Ist.Put.data);
         break;
      case Ist_PutI:
         vex_printf( "PUTI" );
         ppIRArray(s->Ist.PutI.descr);
         vex_printf("[");
         ppIRExpr(s->Ist.PutI.ix);
         vex_printf(",%d] = ", s->Ist.PutI.bias);
         ppIRExpr(s->Ist.PutI.data);
         break;
      case Ist_Tmp:
         ppIRTemp(s->Ist.Tmp.tmp);
         vex_printf( " = " );
         ppIRExpr(s->Ist.Tmp.data);
         break;
      case Ist_STle:
         vex_printf( "STle(");
         ppIRExpr(s->Ist.STle.addr);
         vex_printf( ") = ");
         ppIRExpr(s->Ist.STle.data);
         break;
      case Ist_Dirty:
         ppIRDirty(s->Ist.Dirty.details);
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
   /* call me paranoid; I don't care :-) */
   vassert(bit == False || bit == True);
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
IRConst* IRConst_F64i ( ULong f64i )
{
   IRConst* c  = LibVEX_Alloc(sizeof(IRConst));
   c->tag      = Ico_F64i;
   c->Ico.F64i = f64i;
   return c;
}


/* Constructors -- IRCallee */

IRCallee* mkIRCallee ( Int regparms, Char* name, void* addr )
{
   IRCallee* ce = LibVEX_Alloc(sizeof(IRCallee));
   ce->regparms = regparms;
   ce->name     = name;
   ce->addr     = addr;
   ce->mcx_mask = 0;
   vassert(regparms >= 0 && regparms <= 3);
   vassert(name != NULL);
   vassert(addr != 0);
   return ce;
}


/* Constructors -- IRArray */

IRArray* mkIRArray ( Int base, IRType elemTy, Int nElems )
{
   IRArray* arr = LibVEX_Alloc(sizeof(IRArray));
   arr->base    = base;
   arr->elemTy  = elemTy;
   arr->nElems  = nElems;
   vassert(!(arr->base < 0 || arr->base > 10000 /* somewhat arbitrary */));
   vassert(!(arr->elemTy == Ity_Bit));
   vassert(!(arr->nElems <= 0 || arr->nElems > 500 /* somewhat arbitrary */));
   return arr;
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
IRExpr* IRExpr_GetI ( IRArray* descr, IRExpr* ix, Int bias ) {
   IRExpr* e         = LibVEX_Alloc(sizeof(IRExpr));
   e->tag            = Iex_GetI;
   e->Iex.GetI.descr = descr;
   e->Iex.GetI.ix    = ix;
   e->Iex.GetI.bias  = bias;
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
IRExpr* IRExpr_CCall ( IRCallee* cee, IRType retty, IRExpr** args ) {
   IRExpr* e          = LibVEX_Alloc(sizeof(IRExpr));
   e->tag             = Iex_CCall;
   e->Iex.CCall.cee   = cee;
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


/* Constructors for NULL-terminated IRExpr expression vectors,
   suitable for use as arg lists in clean/dirty helper calls. */

IRExpr** mkIRExprVec_0 ( void ) {
   IRExpr** vec = LibVEX_Alloc(1 * sizeof(IRExpr*));
   vec[0] = NULL;
   return vec;
}
IRExpr** mkIRExprVec_1 ( IRExpr* arg1 ) {
   IRExpr** vec = LibVEX_Alloc(2 * sizeof(IRExpr*));
   vec[0] = arg1;
   vec[1] = NULL;
   return vec;
}
IRExpr** mkIRExprVec_2 ( IRExpr* arg1, IRExpr* arg2 ) {
   IRExpr** vec = LibVEX_Alloc(3 * sizeof(IRExpr*));
   vec[0] = arg1;
   vec[1] = arg2;
   vec[2] = NULL;
   return vec;
}
IRExpr** mkIRExprVec_3 ( IRExpr* arg1, IRExpr* arg2, IRExpr* arg3 ) {
   IRExpr** vec = LibVEX_Alloc(4 * sizeof(IRExpr*));
   vec[0] = arg1;
   vec[1] = arg2;
   vec[2] = arg3;
   vec[3] = NULL;
   return vec;
}
IRExpr** mkIRExprVec_4 ( IRExpr* arg1, IRExpr* arg2, 
                         IRExpr* arg3, IRExpr* arg4 ) {
   IRExpr** vec = LibVEX_Alloc(5 * sizeof(IRExpr*));
   vec[0] = arg1;
   vec[1] = arg2;
   vec[2] = arg3;
   vec[3] = arg4;
   vec[4] = NULL;
   return vec;
}
IRExpr** mkIRExprVec_5 ( IRExpr* arg1, IRExpr* arg2, 
                         IRExpr* arg3, IRExpr* arg4, IRExpr* arg5 ) {
   IRExpr** vec = LibVEX_Alloc(6 * sizeof(IRExpr*));
   vec[0] = arg1;
   vec[1] = arg2;
   vec[2] = arg3;
   vec[3] = arg4;
   vec[4] = arg5;
   vec[5] = NULL;
   return vec;
}


/* Constructors -- IRDirty */

IRDirty* emptyIRDirty ( void ) {
   IRDirty* d = LibVEX_Alloc(sizeof(IRDirty));
   d->cee      = NULL;
   d->guard    = NULL;
   d->args     = NULL;
   d->tmp      = INVALID_IRTEMP;
   d->mFx      = Ifx_None;
   d->mAddr    = NULL;
   d->mSize    = 0;
   d->needsBBP = False;
   d->nFxState = 0;
   return d;
}


/* Constructors -- IRStmt */

IRStmt* IRStmt_Put ( Int off, IRExpr* data ) {
   IRStmt* s         = LibVEX_Alloc(sizeof(IRStmt));
   s->tag            = Ist_Put;
   s->Ist.Put.offset = off;
   s->Ist.Put.data   = data;
   return s;
}
IRStmt* IRStmt_PutI ( IRArray* descr, IRExpr* ix,
                      Int bias, IRExpr* data ) {
   IRStmt* s         = LibVEX_Alloc(sizeof(IRStmt));
   s->tag            = Ist_PutI;
   s->Ist.PutI.descr = descr;
   s->Ist.PutI.ix    = ix;
   s->Ist.PutI.bias  = bias;
   s->Ist.PutI.data  = data;
   return s;
}
IRStmt* IRStmt_Tmp ( IRTemp tmp, IRExpr* data ) {
   IRStmt* s       = LibVEX_Alloc(sizeof(IRStmt));
   s->tag          = Ist_Tmp;
   s->Ist.Tmp.tmp  = tmp;
   s->Ist.Tmp.data = data;
   return s;
}
IRStmt* IRStmt_STle ( IRExpr* addr, IRExpr* data ) {
   IRStmt* s        = LibVEX_Alloc(sizeof(IRStmt));
   s->tag           = Ist_STle;
   s->Ist.STle.addr = addr;
   s->Ist.STle.data = data;
   return s;
}
IRStmt* IRStmt_Dirty ( IRDirty* d )
{
   IRStmt* s            = LibVEX_Alloc(sizeof(IRStmt));
   s->tag               = Ist_Dirty;
   s->Ist.Dirty.details = d;
   return s;
}
IRStmt* IRStmt_Exit ( IRExpr* cond, IRConst* dst ) {
   IRStmt* s        = LibVEX_Alloc(sizeof(IRStmt));
   s->tag           = Ist_Exit;
   s->Ist.Exit.cond = cond;
   s->Ist.Exit.dst  = dst;
   return s;
}


/* Constructors -- IRTypeEnv */

IRTypeEnv* emptyIRTypeEnv ( void )
{
   IRTypeEnv* env   = LibVEX_Alloc(sizeof(IRTypeEnv));
   env->types       = LibVEX_Alloc(8 * sizeof(IRType));
   env->types_size  = 8;
   env->types_used  = 0;
   return env;
}


/* Constructors -- IRBB */

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


/*---------------------------------------------------------------*/
/*--- (Deep) copy constructors.  These make complete copies   ---*/
/*--- the original, which can be modified without affecting   ---*/
/*--- the original.                                           ---*/
/*---------------------------------------------------------------*/

/* Copying IR Expr vectors (for call args). */

/* Shallow copy of an IRExpr vector */

IRExpr** sopyIRExprVec ( IRExpr** vec )
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

/* Deep copy of an IRExpr vector */

IRExpr** dopyIRExprVec ( IRExpr** vec )
{
   Int      i;
   IRExpr** newvec = sopyIRExprVec( vec );
   for (i = 0; newvec[i]; i++)
      newvec[i] = dopyIRExpr(newvec[i]);
   return newvec;
}

/* Deep copy constructors for all heap-allocated IR types follow. */

IRConst* dopyIRConst ( IRConst* c )
{
   switch (c->tag) {
      case Ico_Bit:  return IRConst_Bit(c->Ico.Bit);
      case Ico_U8:   return IRConst_U8(c->Ico.U8);
      case Ico_U16:  return IRConst_U16(c->Ico.U16);
      case Ico_U32:  return IRConst_U32(c->Ico.U32);
      case Ico_U64:  return IRConst_U64(c->Ico.U64);
      case Ico_F64:  return IRConst_F64(c->Ico.F64);
      case Ico_F64i: return IRConst_F64i(c->Ico.F64i);
      default: vpanic("dopyIRConst");
   }
}

IRCallee* dopyIRCallee ( IRCallee* ce )
{
   IRCallee* ce2 = mkIRCallee(ce->regparms, ce->name, ce->addr);
   ce2->mcx_mask = ce->mcx_mask;
   return ce2;
}

IRArray* dopyIRArray ( IRArray* d )
{
   return mkIRArray(d->base, d->elemTy, d->nElems);
}

IRExpr* dopyIRExpr ( IRExpr* e )
{
   switch (e->tag) {
      case Iex_Get: 
         return IRExpr_Get(e->Iex.Get.offset, e->Iex.Get.ty);
      case Iex_GetI: 
         return IRExpr_GetI(dopyIRArray(e->Iex.GetI.descr), 
                            dopyIRExpr(e->Iex.GetI.ix),
                            e->Iex.GetI.bias);
      case Iex_Tmp: 
         return IRExpr_Tmp(e->Iex.Tmp.tmp);
      case Iex_Binop: 
         return IRExpr_Binop(e->Iex.Binop.op,
                             dopyIRExpr(e->Iex.Binop.arg1),
                             dopyIRExpr(e->Iex.Binop.arg2));
      case Iex_Unop: 
         return IRExpr_Unop(e->Iex.Unop.op,
                            dopyIRExpr(e->Iex.Unop.arg));
      case Iex_LDle: 
         return IRExpr_LDle(e->Iex.LDle.ty,
                            dopyIRExpr(e->Iex.LDle.addr));
      case Iex_Const: 
         return IRExpr_Const(dopyIRConst(e->Iex.Const.con));
      case Iex_CCall:
         return IRExpr_CCall(dopyIRCallee(e->Iex.CCall.cee),
                             e->Iex.CCall.retty,
                             dopyIRExprVec(e->Iex.CCall.args));

      case Iex_Mux0X: 
         return IRExpr_Mux0X(dopyIRExpr(e->Iex.Mux0X.cond),
                             dopyIRExpr(e->Iex.Mux0X.expr0),
                             dopyIRExpr(e->Iex.Mux0X.exprX));
      default:
         vpanic("dopyIRExpr");
   }
}

IRDirty* dopyIRDirty ( IRDirty* d )
{
   Int      i;
   IRDirty* d2 = emptyIRDirty();
   d2->cee   = dopyIRCallee(d->cee);
   d2->guard = dopyIRExpr(d->guard);
   d2->args  = dopyIRExprVec(d->args);
   d2->tmp   = d->tmp;
   d2->mFx   = d->mFx;
   d2->mAddr = d->mAddr==NULL ? NULL : dopyIRExpr(d->mAddr);
   d2->mSize = d->mSize;
   d2->needsBBP = d->needsBBP;
   d2->nFxState = d->nFxState;
   for (i = 0; i < d2->nFxState; i++)
      d2->fxState[i] = d->fxState[i];
   return d2;
}

IRStmt* dopyIRStmt ( IRStmt* s )
{
   switch (s->tag) {
      case Ist_Put: 
         return IRStmt_Put(s->Ist.Put.offset, 
                           dopyIRExpr(s->Ist.Put.data));
      case Ist_PutI: 
         return IRStmt_PutI(dopyIRArray(s->Ist.PutI.descr),
                            dopyIRExpr(s->Ist.PutI.ix),
                            s->Ist.PutI.bias, 
                            dopyIRExpr(s->Ist.PutI.data));
      case Ist_Tmp:
         return IRStmt_Tmp(s->Ist.Tmp.tmp,
                           dopyIRExpr(s->Ist.Tmp.data));
      case Ist_STle: 
         return IRStmt_STle(dopyIRExpr(s->Ist.STle.addr),
                            dopyIRExpr(s->Ist.STle.data));
      case Ist_Dirty: 
         return IRStmt_Dirty(dopyIRDirty(s->Ist.Dirty.details));
      case Ist_Exit: 
         return IRStmt_Exit(dopyIRExpr(s->Ist.Exit.cond), 
                            dopyIRConst(s->Ist.Exit.dst));
      default: 
         vpanic("dopyIRStmt");
   }
}

IRTypeEnv* dopyIRTypeEnv ( IRTypeEnv* src )
{
   Int        i;
   IRTypeEnv* dst = LibVEX_Alloc(sizeof(IRTypeEnv));
   dst->types_size = src->types_size;
   dst->types_used = src->types_used;
   dst->types = LibVEX_Alloc(dst->types_size * sizeof(IRType));
   for (i = 0; i < src->types_used; i++)
      dst->types[i] = src->types[i];
   return dst;
}

IRBB* dopyIRBB ( IRBB* bb )
{
   Int      i;
   IRStmt** sts2;
   IRBB* bb2 = emptyIRBB();
   bb2->tyenv = dopyIRTypeEnv(bb->tyenv);
   bb2->stmts_used = bb2->stmts_size = bb->stmts_used;
   sts2 = LibVEX_Alloc(bb2->stmts_used * sizeof(IRStmt*));
   for (i = 0; i < bb2->stmts_used; i++)
      sts2[i] = bb->stmts[i]==NULL ? NULL : dopyIRStmt(bb->stmts[i]);
   bb2->stmts    = sts2;
   bb2->next     = dopyIRExpr(bb->next);
   bb2->jumpkind = bb->jumpkind;
   return bb2;
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
      case Iop_Add8: case Iop_Sub8: case Iop_Mul8: 
      case Iop_Or8:  case Iop_And8: case Iop_Xor8:
         BINARY(Ity_I8,Ity_I8,Ity_I8);

      case Iop_Add16: case Iop_Sub16: case Iop_Mul16:
      case Iop_Or16:  case Iop_And16: case Iop_Xor16:
         BINARY(Ity_I16,Ity_I16,Ity_I16);

      case Iop_Add32: case Iop_Sub32: case Iop_Mul32:
      case Iop_Or32:  case Iop_And32: case Iop_Xor32:
         BINARY(Ity_I32,Ity_I32,Ity_I32);

      case Iop_Add64: case Iop_Sub64: case Iop_Mul64:
      case Iop_Or64:  case Iop_And64: case Iop_Xor64:
         BINARY(Ity_I64,Ity_I64,Ity_I64);

      case Iop_Shl8: case Iop_Shr8: case Iop_Sar8:
         BINARY(Ity_I8,Ity_I8,Ity_I8);
      case Iop_Shl16: case Iop_Shr16: case Iop_Sar16:
         BINARY(Ity_I16,Ity_I16,Ity_I8);
      case Iop_Shl32: case Iop_Shr32: case Iop_Sar32:
         BINARY(Ity_I32,Ity_I32,Ity_I8);
      case Iop_Shl64: case Iop_Shr64: case Iop_Sar64:
         BINARY(Ity_I64,Ity_I64,Ity_I8);

      case Iop_Not8:   UNARY(Ity_I8,Ity_I8);
      case Iop_Not16:  UNARY(Ity_I16,Ity_I16);
      case Iop_Not32:  UNARY(Ity_I32,Ity_I32);
      case Iop_Not64:  UNARY(Ity_I64,Ity_I64);

      case Iop_CmpEQ8: case Iop_CmpNE8:
         COMPARISON(Ity_I8);
      case Iop_CmpEQ16: case Iop_CmpNE16:
         COMPARISON(Ity_I16);
      case Iop_CmpEQ32: case Iop_CmpNE32:
      case Iop_CmpLT32S: case Iop_CmpLE32S:
      case Iop_CmpLT32U: case Iop_CmpLE32U:
         COMPARISON(Ity_I32);
      case Iop_CmpEQ64: case Iop_CmpNE64:
         COMPARISON(Ity_I64);

      case Iop_MullU8: case Iop_MullS8:
         BINARY(Ity_I16,Ity_I8,Ity_I8);
      case Iop_MullU16: case Iop_MullS16:
         BINARY(Ity_I32,Ity_I16,Ity_I16);
      case Iop_MullU32: case Iop_MullS32:
         BINARY(Ity_I64,Ity_I32,Ity_I32);

      case Iop_Clz32: case Iop_Ctz32:
         UNARY(Ity_I32,Ity_I32);

      case Iop_DivModU64to32: case Iop_DivModS64to32:
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

      case Iop_Not1:   UNARY(Ity_Bit,Ity_Bit);
      case Iop_1Uto8:  UNARY(Ity_I8,Ity_Bit);
      case Iop_1Sto8:  UNARY(Ity_I8,Ity_Bit);
      case Iop_1Sto16: UNARY(Ity_I16,Ity_Bit);
      case Iop_1Uto32: case Iop_1Sto32: UNARY(Ity_I32,Ity_Bit);
      case Iop_1Sto64: UNARY(Ity_I64,Ity_Bit);
      case Iop_32to1:  UNARY(Ity_Bit,Ity_I32);

      case Iop_8Uto32: case Iop_8Sto32:
         UNARY(Ity_I32,Ity_I8);

      case Iop_8Uto16: case Iop_8Sto16:
         UNARY(Ity_I16,Ity_I8);

      case Iop_16Uto32: case Iop_16Sto32: 
         UNARY(Ity_I32,Ity_I16);

      case Iop_32Sto64: case Iop_32Uto64:
         UNARY(Ity_I64,Ity_I32);

      case Iop_32to8: UNARY(Ity_I8,Ity_I32);

      case Iop_ScaleF64: case Iop_PRemF64: case Iop_PRem1F64:
      case Iop_AtanF64: case Iop_Yl2xF64:  case Iop_Yl2xp1F64: 
      case Iop_AddF64: case Iop_SubF64: case Iop_MulF64: case Iop_DivF64:
         BINARY(Ity_F64,Ity_F64,Ity_F64);
      case Iop_PRemC3210F64: case Iop_PRem1C3210F64:
      case Iop_CmpF64:
         BINARY(Ity_I32,Ity_F64,Ity_F64);
      case Iop_NegF64: case Iop_AbsF64: case Iop_SqrtF64:
      case Iop_SinF64: case Iop_CosF64: case Iop_TanF64: case Iop_2xm1F64:
         UNARY(Ity_F64,Ity_F64);

      case Iop_I32toF64: UNARY(Ity_F64,Ity_I32);
      case Iop_I64toF64: case Iop_ReinterpI64asF64:
         UNARY(Ity_F64,Ity_I64);
      case Iop_ReinterpF64asI64: UNARY(Ity_I64, Ity_F64);

      case Iop_F64toI64: BINARY(Ity_I64, Ity_I32,Ity_F64);
      case Iop_F64toI32: BINARY(Ity_I32, Ity_I32,Ity_F64);
      case Iop_F64toI16: BINARY(Ity_I16, Ity_I32,Ity_F64);
      case Iop_RoundF64: BINARY(Ity_F64, Ity_I32,Ity_F64);

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
/*--- Helper functions for the IR -- IR Basic Blocks          ---*/
/*---------------------------------------------------------------*/

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
/*--- Helper functions for the IR -- IR Type Environments     ---*/
/*---------------------------------------------------------------*/

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


/*---------------------------------------------------------------*/
/*--- Helper functions for the IR -- finding types of exprs   ---*/
/*---------------------------------------------------------------*/

inline 
IRType typeOfIRTemp ( IRTypeEnv* env, IRTemp tmp )
{
   vassert(tmp >= 0);
   vassert(tmp < env->types_used);
   return env->types[tmp];
}


IRType typeOfIRConst ( IRConst* con )
{
   switch (con->tag) {
      case Ico_Bit:   return Ity_Bit;
      case Ico_U8:    return Ity_I8;
      case Ico_U16:   return Ity_I16;
      case Ico_U32:   return Ity_I32;
      case Ico_U64:   return Ity_I64;
      case Ico_F64:   return Ity_F64;
      case Ico_F64i:  return Ity_F64;
      default: vpanic("typeOfIRConst");
   }
}

IRType typeOfIRExpr ( IRTypeEnv* tyenv, IRExpr* e )
{
   IRType t_dst, t_arg1, t_arg2;
 start:
   switch (e->tag) {
      case Iex_LDle:
         return e->Iex.LDle.ty;
      case Iex_Get:
         return e->Iex.Get.ty;
      case Iex_GetI:
         return e->Iex.GetI.descr->elemTy;
      case Iex_Tmp:
         return typeOfIRTemp(tyenv, e->Iex.Tmp.tmp);
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
         e = e->Iex.Mux0X.expr0;
         goto start;
         /* return typeOfIRExpr(tyenv, e->Iex.Mux0X.expr0); */
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
/*--- Sanity checking -- FLATNESS                             ---*/
/*---------------------------------------------------------------*/

/* Check that the canonical flatness constraints hold on an
   IRStmt. The only place where any expression is allowed to be
   non-atomic is the RHS of IRStmt_Tmp. */

/* Relies on:
   inline static Bool isAtom ( IRExpr* e ) {
      return e->tag == Iex_Tmp || e->tag == Iex_Const;
   }
*/

Bool isFlatIRStmt ( IRStmt* st )
{
   Int      i;
   IRExpr*  e;
   IRDirty* di;

   switch (st->tag) {
      case Ist_Put:
         return isAtom(st->Ist.Put.data);
      case Ist_PutI:
         return isAtom(st->Ist.PutI.ix) && isAtom(st->Ist.PutI.data);
      case Ist_Tmp:
         /* This is the only interesting case.  The RHS can be any
            expression, *but* all its subexpressions *must* be
            atoms. */
         e = st->Ist.Tmp.data;
         switch (e->tag) {
            case Iex_Binder: return True;
            case Iex_Get:    return True;
            case Iex_GetI:   return isAtom(e->Iex.GetI.ix);
            case Iex_Tmp:    return True;
            case Iex_Binop:  return isAtom(e->Iex.Binop.arg1) 
                                    && isAtom(e->Iex.Binop.arg2);
            case Iex_Unop:   return isAtom(e->Iex.Unop.arg);
            case Iex_LDle:   return isAtom(e->Iex.LDle.addr);
            case Iex_Const:  return True;
            case Iex_CCall:  for (i = 0; e->Iex.CCall.args[i]; i++)
                                if (!isAtom(e->Iex.CCall.args[i])) 
                                   return False;
                             return True;
            case Iex_Mux0X:  return isAtom(e->Iex.Mux0X.cond) 
                                    && isAtom(e->Iex.Mux0X.expr0) 
                                    && isAtom(e->Iex.Mux0X.exprX);
            default:         vpanic("isFlatIRStmt(e)");
         }
         /*notreached*/
         vassert(0);
      case Ist_STle:
         return isAtom(st->Ist.STle.addr) && isAtom(st->Ist.STle.data);
      case Ist_Dirty:
         di = st->Ist.Dirty.details;
         if (!isAtom(di->guard)) 
            return False;
         for (i = 0; di->args[i]; i++)
            if (!isAtom(di->args[i])) 
               return False;
         if (di->mAddr && !isAtom(di->mAddr)) 
            return False;
         return True;
      case Ist_Exit:
         return isAtom(st->Ist.Exit.cond);
      default: 
         vpanic("isFlatIRStmt(st)");
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

static inline Int countArgs ( IRExpr** args )
{
   Int i;
   for (i = 0; args[i]; i++)
      ;
   return i;
}

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

static Bool saneIRArray ( IRArray* arr )
{
   if (arr->base < 0 || arr->base > 10000 /* somewhat arbitrary */)
      return False;
   if (arr->elemTy == Ity_Bit)
      return False;
   if (arr->nElems <= 0 || arr->nElems > 500 /* somewhat arbitrary */)
      return False;
   return True;
}

static Bool saneIRCallee ( IRCallee* cee )
{
   if (cee->name == NULL)
      return False;
   if (cee->addr == 0)
      return False;
   if (cee->regparms < 0 || cee->regparms > 3)
      return False;
   return True;
}


/* Traverse a Stmt/Expr, inspecting IRTemp uses.  Report any out of
   range ones.  Report any which are read and for which the current
   def_count is zero. */

static
void useBeforeDef_Temp ( IRBB* bb, IRStmt* stmt, IRTemp tmp, Int* def_counts )
{
   if (tmp < 0 || tmp >= bb->tyenv->types_used)
      sanityCheckFail(bb,stmt, "out of range Temp in IRExpr");
   if (def_counts[tmp] < 1)
      sanityCheckFail(bb,stmt, "IRTemp use before def in IRExpr");
}

static
void useBeforeDef_Expr ( IRBB* bb, IRStmt* stmt, IRExpr* expr, Int* def_counts )
{
   Int i;
   switch (expr->tag) {
      case Iex_Get: 
         break;
      case Iex_GetI:
         useBeforeDef_Expr(bb,stmt,expr->Iex.GetI.ix,def_counts);
         break;
      case Iex_Tmp:
         useBeforeDef_Temp(bb,stmt,expr->Iex.Tmp.tmp,def_counts);
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
   Int      i;
   IRDirty* d;
   switch (stmt->tag) {
      case Ist_Put:
         useBeforeDef_Expr(bb,stmt,stmt->Ist.Put.data,def_counts);
         break;
      case Ist_PutI:
         useBeforeDef_Expr(bb,stmt,stmt->Ist.PutI.ix,def_counts);
         useBeforeDef_Expr(bb,stmt,stmt->Ist.PutI.data,def_counts);
         break;
      case Ist_Tmp:
         useBeforeDef_Expr(bb,stmt,stmt->Ist.Tmp.data,def_counts);
         break;
      case Ist_STle:
         useBeforeDef_Expr(bb,stmt,stmt->Ist.STle.addr,def_counts);
         useBeforeDef_Expr(bb,stmt,stmt->Ist.STle.data,def_counts);
         break;
      case Ist_Dirty:
         d = stmt->Ist.Dirty.details;
         for (i = 0; d->args[i] != NULL; i++)
            useBeforeDef_Expr(bb,stmt,d->args[i],def_counts);
         if (d->mFx != Ifx_None)
            useBeforeDef_Expr(bb,stmt,d->mAddr,def_counts);
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
         tcExpr(bb,stmt, expr->Iex.GetI.ix, gWordTy );
         if (typeOfIRExpr(tyenv,expr->Iex.GetI.ix) != Ity_I32)
            sanityCheckFail(bb,stmt,"IRExpr.GetI.ix: not :: Ity_I32");
         if (!saneIRArray(expr->Iex.GetI.descr))
            sanityCheckFail(bb,stmt,"IRExpr.GetI.descr: invalid descr");
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
         if (!saneIRCallee(expr->Iex.CCall.cee))
            sanityCheckFail(bb,stmt,"Iex.CCall.cee: bad IRCallee");
         if (expr->Iex.CCall.cee->regparms > countArgs(expr->Iex.CCall.args)) 
            sanityCheckFail(bb,stmt,"Iex.CCall.cee: #regparms > #args");
         for (i = 0; expr->Iex.CCall.args[i]; i++) {
            if (i >= 32)
               sanityCheckFail(bb,stmt,"Iex.CCall: > 32 args");
            tcExpr(bb,stmt, expr->Iex.CCall.args[i], gWordTy);
         }
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
   Int        i;
   IRDirty*   d;
   IRTypeEnv* tyenv = bb->tyenv;
   switch (stmt->tag) {
      case Ist_Put:
         tcExpr( bb, stmt, stmt->Ist.Put.data, gWordTy );
         if (typeOfIRExpr(tyenv,stmt->Ist.Put.data) == Ity_Bit)
            sanityCheckFail(bb,stmt,"IRStmt.Put.data: cannot Put :: Ity_Bit");
         break;
      case Ist_PutI:
         tcExpr( bb, stmt, stmt->Ist.PutI.data, gWordTy );
         tcExpr( bb, stmt, stmt->Ist.PutI.ix, gWordTy );
         if (typeOfIRExpr(tyenv,stmt->Ist.PutI.data) == Ity_Bit)
            sanityCheckFail(bb,stmt,"IRStmt.PutI.data: cannot PutI :: Ity_Bit");
         if (typeOfIRExpr(tyenv,stmt->Ist.PutI.data) 
             != stmt->Ist.PutI.descr->elemTy)
            sanityCheckFail(bb,stmt,"IRStmt.PutI.data: data ty != elem ty");
         if (typeOfIRExpr(tyenv,stmt->Ist.PutI.ix) != Ity_I32)
            sanityCheckFail(bb,stmt,"IRStmt.PutI.ix: not :: Ity_I32");
         if (!saneIRArray(stmt->Ist.PutI.descr))
            sanityCheckFail(bb,stmt,"IRStmt.PutI.descr: invalid descr");
         break;
      case Ist_Tmp:
         tcExpr( bb, stmt, stmt->Ist.Tmp.data, gWordTy );
         if (typeOfIRTemp(tyenv, stmt->Ist.Tmp.tmp)
             != typeOfIRExpr(tyenv, stmt->Ist.Tmp.data))
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
      case Ist_Dirty:
         /* Mostly check for various kinds of ill-formed dirty calls. */
         d = stmt->Ist.Dirty.details;
         if (d->cee == NULL) goto bad_dirty;
         if (!saneIRCallee(d->cee)) goto bad_dirty;
         if (d->cee->regparms > countArgs(d->args)) goto bad_dirty;
         if (d->mFx == Ifx_None) {
            if (d->mAddr != NULL || d->mSize != 0)
               goto bad_dirty;
         } else {
            if (d->mAddr == NULL || d->mSize == 0)
               goto bad_dirty;
         }
         if (d->nFxState < 0 || d->nFxState > VEX_N_FXSTATE)
            goto bad_dirty;
         if (d->nFxState == 0 && d->needsBBP)
            goto bad_dirty;
         for (i = 0; i < d->nFxState; i++) {
            if (d->fxState[i].fx == Ifx_None) goto bad_dirty;
            if (d->fxState[i].size <= 0) goto bad_dirty;
         }
         /* check types, minimally */
         if (d->guard == NULL) goto bad_dirty;
         if (typeOfIRExpr(tyenv, d->guard) != Ity_Bit)
            sanityCheckFail(bb,stmt,"IRStmt.Dirty.guard not :: Ity_Bit");
         if (d->tmp != INVALID_IRTEMP
             && typeOfIRTemp(tyenv, d->tmp) == Ity_Bit)
            sanityCheckFail(bb,stmt,"IRStmt.Dirty.dst :: Ity_Bit");
         for (i = 0; d->args[i] != NULL; i++) {
            if (i >= 32)
               sanityCheckFail(bb,stmt,"IRStmt.Dirty: > 32 args");
            if (typeOfIRExpr(tyenv, d->args[i]) == Ity_Bit)
               sanityCheckFail(bb,stmt,"IRStmt.Dirty.arg[i] :: Ity_Bit");
         }
         break;
         bad_dirty:
         sanityCheckFail(bb,stmt,"IRStmt.Dirty: ill-formed");

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
      IRType ty = typeOfIRTemp(bb->tyenv,(IRTemp)i);
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
            sanityCheckFail(bb, stmt, 
               "IRStmt.Tmp: destination tmp is out of range");
         def_counts[stmt->Ist.Tmp.tmp]++;
         if (def_counts[stmt->Ist.Tmp.tmp] > 1)
            sanityCheckFail(bb, stmt, 
               "IRStmt.Tmp: destination tmp is assigned more than once");
      }
      else 
      if (stmt->tag == Ist_Dirty 
          && stmt->Ist.Dirty.details->tmp != INVALID_IRTEMP) {
         IRDirty* d = stmt->Ist.Dirty.details;
         if (d->tmp < 0 || d->tmp >= n_temps)
            sanityCheckFail(bb, stmt, 
               "IRStmt.Dirty: destination tmp is out of range");
         def_counts[d->tmp]++;
         if (def_counts[d->tmp] > 1)
            sanityCheckFail(bb, stmt, 
               "IRStmt.Dirty: destination tmp is assigned more than once");
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
/*--- Misc helper functions                                   ---*/
/*---------------------------------------------------------------*/

Bool eqIRConst ( IRConst* c1, IRConst* c2 )
{
   if (c1->tag != c2->tag)
      return False;

   switch (c1->tag) {
      case Ico_Bit: return (1 & c1->Ico.Bit) == (1 & c2->Ico.Bit);
      case Ico_U8:  return c1->Ico.U8  == c2->Ico.U8;
      case Ico_U16: return c1->Ico.U16 == c2->Ico.U16;
      case Ico_U32: return c1->Ico.U32 == c2->Ico.U32;
      case Ico_U64: return c1->Ico.U64 == c2->Ico.U64;
      case Ico_F64: return c1->Ico.F64 == c2->Ico.F64;
      default: vpanic("eqIRConst");
   }
}

Bool eqIRArray ( IRArray* descr1, IRArray* descr2 )
{
   return descr1->base == descr2->base 
          && descr1->elemTy == descr2->elemTy
          && descr1->nElems == descr2->nElems;
}

Int sizeofIRType ( IRType ty )
{
   switch (ty) {
      case Ity_I8:  return 1;
      case Ity_I16: return 2;
      case Ity_I32: return 4;
      case Ity_I64: return 8;
      case Ity_F32: return 4;
      case Ity_F64: return 8;
      default: vex_printf("\n"); ppIRType(ty); vex_printf("\n");
               vpanic("sizeofIRType");
   }
}

IRExpr* mkIRExpr_HWord ( HWord hw )
{
   vassert(sizeof(void*) == sizeof(HWord));
   if (sizeof(HWord) == 4)
      return IRExpr_Const(IRConst_U32((UInt)hw));
   if (sizeof(HWord) == 8)
      return IRExpr_Const(IRConst_U64((ULong)hw));
   vpanic("mkIRExpr_HWord");
}

IRDirty* unsafeIRDirty_0_N ( Int regparms, Char* name, void* addr, 
                             IRExpr** args ) 
{
   IRDirty* d = emptyIRDirty();
   d->cee   = mkIRCallee ( regparms, name, addr );
   d->guard = IRExpr_Const(IRConst_Bit(True));
   d->args  = args;
   return d;
}

IRDirty* unsafeIRDirty_1_N ( IRTemp dst, 
                             Int regparms, Char* name, void* addr, 
                             IRExpr** args ) 
{
   IRDirty* d = emptyIRDirty();
   d->cee   = mkIRCallee ( regparms, name, addr );
   d->guard = IRExpr_Const(IRConst_Bit(True));
   d->args  = args;
   d->tmp   = dst;
   return d;
}

IRExpr* mkIRExprCCall ( IRType retty,
                        Int regparms, Char* name, void* addr, 
                        IRExpr** args )
{
   return IRExpr_CCall ( mkIRCallee ( regparms, name, addr ), 
                         retty, args );
}

/*---------------------------------------------------------------*/
/*--- end                                         ir/irdefs.c ---*/
/*---------------------------------------------------------------*/
