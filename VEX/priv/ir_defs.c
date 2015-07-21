
/*---------------------------------------------------------------*/
/*--- begin                                         ir_defs.c ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2004-2013 OpenWorks LLP
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


/*---------------------------------------------------------------*/
/*--- Printing the IR                                         ---*/
/*---------------------------------------------------------------*/

void ppIRType ( IRType ty )
{
   switch (ty) {
      case Ity_INVALID: vex_printf("Ity_INVALID"); break;
      case Ity_I1:      vex_printf( "I1");   break;
      case Ity_I8:      vex_printf( "I8");   break;
      case Ity_I16:     vex_printf( "I16");  break;
      case Ity_I32:     vex_printf( "I32");  break;
      case Ity_I64:     vex_printf( "I64");  break;
      case Ity_I128:    vex_printf( "I128"); break;
      case Ity_F16:     vex_printf( "F16");  break;
      case Ity_F32:     vex_printf( "F32");  break;
      case Ity_F64:     vex_printf( "F64");  break;
      case Ity_F128:    vex_printf( "F128"); break;
      case Ity_D32:     vex_printf( "D32");  break;
      case Ity_D64:     vex_printf( "D64");  break;
      case Ity_D128:    vex_printf( "D128"); break;
      case Ity_V128:    vex_printf( "V128"); break;
      case Ity_V256:    vex_printf( "V256"); break;
      default: vex_printf("ty = 0x%x\n", (Int)ty);
               vpanic("ppIRType");
   }
}

void ppIRConst ( const IRConst* con )
{
   union { ULong i64; Double f64; UInt i32; Float f32; } u;
   vassert(sizeof(ULong) == sizeof(Double));
   switch (con->tag) {
      case Ico_U1:   vex_printf( "%d:I1",        con->Ico.U1 ? 1 : 0); break;
      case Ico_U8:   vex_printf( "0x%x:I8",      (UInt)(con->Ico.U8)); break;
      case Ico_U16:  vex_printf( "0x%x:I16",     (UInt)(con->Ico.U16)); break;
      case Ico_U32:  vex_printf( "0x%x:I32",     (UInt)(con->Ico.U32)); break;
      case Ico_U64:  vex_printf( "0x%llx:I64",   (ULong)(con->Ico.U64)); break;
      case Ico_F32:  u.f32 = con->Ico.F32;
                     vex_printf( "F32{0x%x}",   u.i32);
                     break;
      case Ico_F32i: vex_printf( "F32i{0x%x}",   con->Ico.F32i); break;
      case Ico_F64:  u.f64 = con->Ico.F64;
                     vex_printf( "F64{0x%llx}",  u.i64);
                     break;
      case Ico_F64i: vex_printf( "F64i{0x%llx}", con->Ico.F64i); break;
      case Ico_V128: vex_printf( "V128{0x%04x}", (UInt)(con->Ico.V128)); break;
      case Ico_V256: vex_printf( "V256{0x%08x}", con->Ico.V256); break;
      default: vpanic("ppIRConst");
   }
}

void ppIRCallee ( const IRCallee* ce )
{
   vex_printf("%s", ce->name);
   if (ce->regparms > 0)
      vex_printf("[rp=%d]", ce->regparms);
   if (ce->mcx_mask > 0)
      vex_printf("[mcx=0x%x]", ce->mcx_mask);
   vex_printf("{%p}", (void*)ce->addr);
}

void ppIRRegArray ( const IRRegArray* arr )
{
   vex_printf("(%d:%dx", arr->base, arr->nElems);
   ppIRType(arr->elemTy);
   vex_printf(")");
}

void ppIRTemp ( IRTemp tmp )
{
   if (tmp == IRTemp_INVALID)
      vex_printf("IRTemp_INVALID");
   else
      vex_printf( "t%d", (Int)tmp);
}

void ppIROp ( IROp op )
{
   const HChar* str = NULL; 
   IROp   base;
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
      case Iop_CasCmpEQ8 ... Iop_CasCmpEQ64:
         str = "CasCmpEQ"; base = Iop_CasCmpEQ8; break;
      case Iop_CasCmpNE8 ... Iop_CasCmpNE64:
         str = "CasCmpNE"; base = Iop_CasCmpNE8; break;
      case Iop_ExpCmpNE8 ... Iop_ExpCmpNE64:
         str = "ExpCmpNE"; base = Iop_ExpCmpNE8; break;
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
      case Iop_16Uto64:  vex_printf("16Uto64"); return;
      case Iop_16Sto64:  vex_printf("16Sto64"); return;
      case Iop_8Uto64:   vex_printf("8Uto64"); return;
      case Iop_8Sto64:   vex_printf("8Sto64"); return;
      case Iop_64to16:   vex_printf("64to16"); return;
      case Iop_64to8:    vex_printf("64to8");  return;

      case Iop_Not1:     vex_printf("Not1");    return;
      case Iop_32to1:    vex_printf("32to1");   return;
      case Iop_64to1:    vex_printf("64to1");   return;
      case Iop_1Uto8:    vex_printf("1Uto8");   return;
      case Iop_1Uto32:   vex_printf("1Uto32");  return;
      case Iop_1Uto64:   vex_printf("1Uto64");  return;
      case Iop_1Sto8:    vex_printf("1Sto8");  return;
      case Iop_1Sto16:   vex_printf("1Sto16");  return;
      case Iop_1Sto32:   vex_printf("1Sto32");  return;
      case Iop_1Sto64:   vex_printf("1Sto64");  return;

      case Iop_MullS8:   vex_printf("MullS8");  return;
      case Iop_MullS16:  vex_printf("MullS16"); return;
      case Iop_MullS32:  vex_printf("MullS32"); return;
      case Iop_MullS64:  vex_printf("MullS64"); return;
      case Iop_MullU8:   vex_printf("MullU8");  return;
      case Iop_MullU16:  vex_printf("MullU16"); return;
      case Iop_MullU32:  vex_printf("MullU32"); return;
      case Iop_MullU64:  vex_printf("MullU64"); return;

      case Iop_Clz64:    vex_printf("Clz64"); return;
      case Iop_Clz32:    vex_printf("Clz32"); return;
      case Iop_Ctz64:    vex_printf("Ctz64"); return;
      case Iop_Ctz32:    vex_printf("Ctz32"); return;

      case Iop_CmpLT32S: vex_printf("CmpLT32S"); return;
      case Iop_CmpLE32S: vex_printf("CmpLE32S"); return;
      case Iop_CmpLT32U: vex_printf("CmpLT32U"); return;
      case Iop_CmpLE32U: vex_printf("CmpLE32U"); return;

      case Iop_CmpLT64S: vex_printf("CmpLT64S"); return;
      case Iop_CmpLE64S: vex_printf("CmpLE64S"); return;
      case Iop_CmpLT64U: vex_printf("CmpLT64U"); return;
      case Iop_CmpLE64U: vex_printf("CmpLE64U"); return;

      case Iop_CmpNEZ8:  vex_printf("CmpNEZ8"); return;
      case Iop_CmpNEZ16: vex_printf("CmpNEZ16"); return;
      case Iop_CmpNEZ32: vex_printf("CmpNEZ32"); return;
      case Iop_CmpNEZ64: vex_printf("CmpNEZ64"); return;

      case Iop_CmpwNEZ32: vex_printf("CmpwNEZ32"); return;
      case Iop_CmpwNEZ64: vex_printf("CmpwNEZ64"); return;

      case Iop_Left8:  vex_printf("Left8"); return;
      case Iop_Left16: vex_printf("Left16"); return;
      case Iop_Left32: vex_printf("Left32"); return;
      case Iop_Left64: vex_printf("Left64"); return;
      case Iop_Max32U: vex_printf("Max32U"); return;

      case Iop_CmpORD32U: vex_printf("CmpORD32U"); return;
      case Iop_CmpORD32S: vex_printf("CmpORD32S"); return;

      case Iop_CmpORD64U: vex_printf("CmpORD64U"); return;
      case Iop_CmpORD64S: vex_printf("CmpORD64S"); return;

      case Iop_DivU32: vex_printf("DivU32"); return;
      case Iop_DivS32: vex_printf("DivS32"); return;
      case Iop_DivU64: vex_printf("DivU64"); return;
      case Iop_DivS64: vex_printf("DivS64"); return;
      case Iop_DivU64E: vex_printf("DivU64E"); return;
      case Iop_DivS64E: vex_printf("DivS64E"); return;
      case Iop_DivU32E: vex_printf("DivU32E"); return;
      case Iop_DivS32E: vex_printf("DivS32E"); return;

      case Iop_DivModU64to32: vex_printf("DivModU64to32"); return;
      case Iop_DivModS64to32: vex_printf("DivModS64to32"); return;

      case Iop_DivModU128to64: vex_printf("DivModU128to64"); return;
      case Iop_DivModS128to64: vex_printf("DivModS128to64"); return;

      case Iop_DivModS64to64: vex_printf("DivModS64to64"); return;

      case Iop_16HIto8:  vex_printf("16HIto8"); return;
      case Iop_16to8:    vex_printf("16to8");   return;
      case Iop_8HLto16:  vex_printf("8HLto16"); return;

      case Iop_32HIto16: vex_printf("32HIto16"); return;
      case Iop_32to16:   vex_printf("32to16");   return;
      case Iop_16HLto32: vex_printf("16HLto32"); return;

      case Iop_64HIto32: vex_printf("64HIto32"); return;
      case Iop_64to32:   vex_printf("64to32");   return;
      case Iop_32HLto64: vex_printf("32HLto64"); return;

      case Iop_128HIto64: vex_printf("128HIto64"); return;
      case Iop_128to64:   vex_printf("128to64");   return;
      case Iop_64HLto128: vex_printf("64HLto128"); return;

      case Iop_CmpF32:    vex_printf("CmpF32");    return;
      case Iop_F32toI32S: vex_printf("F32toI32S");  return;
      case Iop_F32toI64S: vex_printf("F32toI64S");  return;
      case Iop_I32StoF32: vex_printf("I32StoF32");  return;
      case Iop_I64StoF32: vex_printf("I64StoF32");  return;

      case Iop_AddF64:    vex_printf("AddF64"); return;
      case Iop_SubF64:    vex_printf("SubF64"); return;
      case Iop_MulF64:    vex_printf("MulF64"); return;
      case Iop_DivF64:    vex_printf("DivF64"); return;
      case Iop_AddF64r32: vex_printf("AddF64r32"); return;
      case Iop_SubF64r32: vex_printf("SubF64r32"); return;
      case Iop_MulF64r32: vex_printf("MulF64r32"); return;
      case Iop_DivF64r32: vex_printf("DivF64r32"); return;
      case Iop_AddF32:    vex_printf("AddF32"); return;
      case Iop_SubF32:    vex_printf("SubF32"); return;
      case Iop_MulF32:    vex_printf("MulF32"); return;
      case Iop_DivF32:    vex_printf("DivF32"); return;

        /* 128 bit floating point */
      case Iop_AddF128:   vex_printf("AddF128");  return;
      case Iop_SubF128:   vex_printf("SubF128");  return;
      case Iop_MulF128:   vex_printf("MulF128");  return;
      case Iop_DivF128:   vex_printf("DivF128");  return;
      case Iop_AbsF128:   vex_printf("AbsF128");  return;
      case Iop_NegF128:   vex_printf("NegF128");  return;
      case Iop_SqrtF128:  vex_printf("SqrtF128"); return;
      case Iop_CmpF128:   vex_printf("CmpF128");  return;

      case Iop_F64HLtoF128: vex_printf("F64HLtoF128"); return;
      case Iop_F128HItoF64: vex_printf("F128HItoF64"); return;
      case Iop_F128LOtoF64: vex_printf("F128LOtoF64"); return;
      case Iop_I32StoF128: vex_printf("I32StoF128"); return;
      case Iop_I64StoF128: vex_printf("I64StoF128"); return;
      case Iop_I32UtoF128: vex_printf("I32UtoF128"); return;
      case Iop_I64UtoF128: vex_printf("I64UtoF128"); return;
      case Iop_F128toI32S: vex_printf("F128toI32S"); return;
      case Iop_F128toI64S: vex_printf("F128toI64S"); return;
      case Iop_F128toI32U: vex_printf("F128toI32U"); return;
      case Iop_F128toI64U: vex_printf("F128toI64U"); return;
      case Iop_F32toF128:  vex_printf("F32toF128");  return;
      case Iop_F64toF128:  vex_printf("F64toF128");  return;
      case Iop_F128toF64:  vex_printf("F128toF64");  return;
      case Iop_F128toF32:  vex_printf("F128toF32");  return;

        /* s390 specific */
      case Iop_MAddF32:    vex_printf("s390_MAddF32"); return;
      case Iop_MSubF32:    vex_printf("s390_MSubF32"); return;

      case Iop_ScaleF64:      vex_printf("ScaleF64"); return;
      case Iop_AtanF64:       vex_printf("AtanF64"); return;
      case Iop_Yl2xF64:       vex_printf("Yl2xF64"); return;
      case Iop_Yl2xp1F64:     vex_printf("Yl2xp1F64"); return;
      case Iop_PRemF64:       vex_printf("PRemF64"); return;
      case Iop_PRemC3210F64:  vex_printf("PRemC3210F64"); return;
      case Iop_PRem1F64:      vex_printf("PRem1F64"); return;
      case Iop_PRem1C3210F64: vex_printf("PRem1C3210F64"); return;
      case Iop_NegF64:        vex_printf("NegF64"); return;
      case Iop_AbsF64:        vex_printf("AbsF64"); return;
      case Iop_NegF32:        vex_printf("NegF32"); return;
      case Iop_AbsF32:        vex_printf("AbsF32"); return;
      case Iop_SqrtF64:       vex_printf("SqrtF64"); return;
      case Iop_SqrtF32:       vex_printf("SqrtF32"); return;
      case Iop_SinF64:    vex_printf("SinF64"); return;
      case Iop_CosF64:    vex_printf("CosF64"); return;
      case Iop_TanF64:    vex_printf("TanF64"); return;
      case Iop_2xm1F64:   vex_printf("2xm1F64"); return;

      case Iop_MAddF64:    vex_printf("MAddF64"); return;
      case Iop_MSubF64:    vex_printf("MSubF64"); return;
      case Iop_MAddF64r32: vex_printf("MAddF64r32"); return;
      case Iop_MSubF64r32: vex_printf("MSubF64r32"); return;

      case Iop_RSqrtEst5GoodF64: vex_printf("RSqrtEst5GoodF64"); return;
      case Iop_RoundF64toF64_NEAREST: vex_printf("RoundF64toF64_NEAREST"); return;
      case Iop_RoundF64toF64_NegINF: vex_printf("RoundF64toF64_NegINF"); return;
      case Iop_RoundF64toF64_PosINF: vex_printf("RoundF64toF64_PosINF"); return;
      case Iop_RoundF64toF64_ZERO: vex_printf("RoundF64toF64_ZERO"); return;

      case Iop_TruncF64asF32: vex_printf("TruncF64asF32"); return;

      case Iop_RecpExpF64: vex_printf("RecpExpF64"); return;
      case Iop_RecpExpF32: vex_printf("RecpExpF32"); return;

      case Iop_F16toF64: vex_printf("F16toF64"); return;
      case Iop_F64toF16: vex_printf("F64toF16"); return;
      case Iop_F16toF32: vex_printf("F16toF32"); return;
      case Iop_F32toF16: vex_printf("F32toF16"); return;

      case Iop_QAdd32S: vex_printf("QAdd32S"); return;
      case Iop_QSub32S: vex_printf("QSub32S"); return; 
      case Iop_Add16x2:   vex_printf("Add16x2"); return;
      case Iop_Sub16x2:   vex_printf("Sub16x2"); return;
      case Iop_QAdd16Sx2: vex_printf("QAdd16Sx2"); return;
      case Iop_QAdd16Ux2: vex_printf("QAdd16Ux2"); return;
      case Iop_QSub16Sx2: vex_printf("QSub16Sx2"); return;
      case Iop_QSub16Ux2: vex_printf("QSub16Ux2"); return;
      case Iop_HAdd16Ux2: vex_printf("HAdd16Ux2"); return;
      case Iop_HAdd16Sx2: vex_printf("HAdd16Sx2"); return;
      case Iop_HSub16Ux2: vex_printf("HSub16Ux2"); return;
      case Iop_HSub16Sx2: vex_printf("HSub16Sx2"); return;

      case Iop_Add8x4:   vex_printf("Add8x4"); return;
      case Iop_Sub8x4:   vex_printf("Sub8x4"); return;
      case Iop_QAdd8Sx4: vex_printf("QAdd8Sx4"); return;
      case Iop_QAdd8Ux4: vex_printf("QAdd8Ux4"); return;
      case Iop_QSub8Sx4: vex_printf("QSub8Sx4"); return;
      case Iop_QSub8Ux4: vex_printf("QSub8Ux4"); return;
      case Iop_HAdd8Ux4: vex_printf("HAdd8Ux4"); return;
      case Iop_HAdd8Sx4: vex_printf("HAdd8Sx4"); return;
      case Iop_HSub8Ux4: vex_printf("HSub8Ux4"); return;
      case Iop_HSub8Sx4: vex_printf("HSub8Sx4"); return;
      case Iop_Sad8Ux4:  vex_printf("Sad8Ux4"); return;

      case Iop_CmpNEZ16x2: vex_printf("CmpNEZ16x2"); return;
      case Iop_CmpNEZ8x4:  vex_printf("CmpNEZ8x4"); return;

      case Iop_CmpF64:    vex_printf("CmpF64"); return;

      case Iop_F64toI16S: vex_printf("F64toI16S"); return;
      case Iop_F64toI32S: vex_printf("F64toI32S"); return;
      case Iop_F64toI64S: vex_printf("F64toI64S"); return;
      case Iop_F64toI64U: vex_printf("F64toI64U"); return;
      case Iop_F32toI32U: vex_printf("F32toI32U");  return;
      case Iop_F32toI64U: vex_printf("F32toI64U");  return;

      case Iop_F64toI32U: vex_printf("F64toI32U"); return;

      case Iop_I32StoF64: vex_printf("I32StoF64"); return;
      case Iop_I64StoF64: vex_printf("I64StoF64"); return;
      case Iop_I64UtoF64: vex_printf("I64UtoF64"); return;
      case Iop_I32UtoF32: vex_printf("I32UtoF32"); return;
      case Iop_I64UtoF32: vex_printf("I64UtoF32"); return;

      case Iop_I32UtoF64: vex_printf("I32UtoF64"); return;

      case Iop_F32toF64: vex_printf("F32toF64"); return;
      case Iop_F64toF32: vex_printf("F64toF32"); return;

      case Iop_RoundF64toInt: vex_printf("RoundF64toInt"); return;
      case Iop_RoundF32toInt: vex_printf("RoundF32toInt"); return;
      case Iop_RoundF64toF32: vex_printf("RoundF64toF32"); return;

      case Iop_ReinterpF64asI64: vex_printf("ReinterpF64asI64"); return;
      case Iop_ReinterpI64asF64: vex_printf("ReinterpI64asF64"); return;
      case Iop_ReinterpF32asI32: vex_printf("ReinterpF32asI32"); return;
      case Iop_ReinterpI32asF32: vex_printf("ReinterpI32asF32"); return;

      case Iop_I32UtoFx4: vex_printf("I32UtoFx4"); return;
      case Iop_I32StoFx4: vex_printf("I32StoFx4"); return;

      case Iop_F32toF16x4: vex_printf("F32toF16x4"); return;
      case Iop_F16toF32x4: vex_printf("F16toF32x4"); return;

      case Iop_RSqrtEst32Fx4: vex_printf("RSqrtEst32Fx4"); return;
      case Iop_RSqrtEst32Ux4: vex_printf("RSqrtEst32Ux4"); return;
      case Iop_RSqrtEst32Fx2: vex_printf("RSqrtEst32Fx2"); return;
      case Iop_RSqrtEst32Ux2: vex_printf("RSqrtEst32Ux2"); return;

      case Iop_QFtoI32Ux4_RZ: vex_printf("QFtoI32Ux4_RZ"); return;
      case Iop_QFtoI32Sx4_RZ: vex_printf("QFtoI32Sx4_RZ"); return;

      case Iop_FtoI32Ux4_RZ: vex_printf("FtoI32Ux4_RZ"); return;
      case Iop_FtoI32Sx4_RZ: vex_printf("FtoI32Sx4_RZ"); return;

      case Iop_I32UtoFx2: vex_printf("I32UtoFx2"); return;
      case Iop_I32StoFx2: vex_printf("I32StoFx2"); return;

      case Iop_FtoI32Ux2_RZ: vex_printf("FtoI32Ux2_RZ"); return;
      case Iop_FtoI32Sx2_RZ: vex_printf("FtoI32Sx2_RZ"); return;

      case Iop_RoundF32x4_RM: vex_printf("RoundF32x4_RM"); return;
      case Iop_RoundF32x4_RP: vex_printf("RoundF32x4_RP"); return;
      case Iop_RoundF32x4_RN: vex_printf("RoundF32x4_RN"); return;
      case Iop_RoundF32x4_RZ: vex_printf("RoundF32x4_RZ"); return;

      case Iop_Abs8x8: vex_printf("Abs8x8"); return;
      case Iop_Abs16x4: vex_printf("Abs16x4"); return;
      case Iop_Abs32x2: vex_printf("Abs32x2"); return;
      case Iop_Add8x8: vex_printf("Add8x8"); return;
      case Iop_Add16x4: vex_printf("Add16x4"); return;
      case Iop_Add32x2: vex_printf("Add32x2"); return;
      case Iop_QAdd8Ux8: vex_printf("QAdd8Ux8"); return;
      case Iop_QAdd16Ux4: vex_printf("QAdd16Ux4"); return;
      case Iop_QAdd32Ux2: vex_printf("QAdd32Ux2"); return;
      case Iop_QAdd64Ux1: vex_printf("QAdd64Ux1"); return;
      case Iop_QAdd8Sx8: vex_printf("QAdd8Sx8"); return;
      case Iop_QAdd16Sx4: vex_printf("QAdd16Sx4"); return;
      case Iop_QAdd32Sx2: vex_printf("QAdd32Sx2"); return;
      case Iop_QAdd64Sx1: vex_printf("QAdd64Sx1"); return;
      case Iop_PwAdd8x8: vex_printf("PwAdd8x8"); return;
      case Iop_PwAdd16x4: vex_printf("PwAdd16x4"); return;
      case Iop_PwAdd32x2: vex_printf("PwAdd32x2"); return;
      case Iop_PwAdd32Fx2: vex_printf("PwAdd32Fx2"); return;
      case Iop_PwAddL8Ux8: vex_printf("PwAddL8Ux8"); return;
      case Iop_PwAddL16Ux4: vex_printf("PwAddL16Ux4"); return;
      case Iop_PwAddL32Ux2: vex_printf("PwAddL32Ux2"); return;
      case Iop_PwAddL8Sx8: vex_printf("PwAddL8Sx8"); return;
      case Iop_PwAddL16Sx4: vex_printf("PwAddL16Sx4"); return;
      case Iop_PwAddL32Sx2: vex_printf("PwAddL32Sx2"); return;
      case Iop_Sub8x8: vex_printf("Sub8x8"); return;
      case Iop_Sub16x4: vex_printf("Sub16x4"); return;
      case Iop_Sub32x2: vex_printf("Sub32x2"); return;
      case Iop_QSub8Ux8: vex_printf("QSub8Ux8"); return;
      case Iop_QSub16Ux4: vex_printf("QSub16Ux4"); return;
      case Iop_QSub32Ux2: vex_printf("QSub32Ux2"); return;
      case Iop_QSub64Ux1: vex_printf("QSub64Ux1"); return;
      case Iop_QSub8Sx8: vex_printf("QSub8Sx8"); return;
      case Iop_QSub16Sx4: vex_printf("QSub16Sx4"); return;
      case Iop_QSub32Sx2: vex_printf("QSub32Sx2"); return;
      case Iop_QSub64Sx1: vex_printf("QSub64Sx1"); return;
      case Iop_Mul8x8: vex_printf("Mul8x8"); return;
      case Iop_Mul16x4: vex_printf("Mul16x4"); return;
      case Iop_Mul32x2: vex_printf("Mul32x2"); return;
      case Iop_Mul32Fx2: vex_printf("Mul32Fx2"); return;
      case Iop_PolynomialMul8x8: vex_printf("PolynomialMul8x8"); return;
      case Iop_MulHi16Ux4: vex_printf("MulHi16Ux4"); return;
      case Iop_MulHi16Sx4: vex_printf("MulHi16Sx4"); return;
      case Iop_QDMulHi16Sx4: vex_printf("QDMulHi16Sx4"); return;
      case Iop_QDMulHi32Sx2: vex_printf("QDMulHi32Sx2"); return;
      case Iop_QRDMulHi16Sx4: vex_printf("QRDMulHi16Sx4"); return;
      case Iop_QRDMulHi32Sx2: vex_printf("QRDMulHi32Sx2"); return;
      case Iop_QDMull16Sx4: vex_printf("QDMull16Sx4"); return;
      case Iop_QDMull32Sx2: vex_printf("QDMull32Sx2"); return;
      case Iop_Avg8Ux8: vex_printf("Avg8Ux8"); return;
      case Iop_Avg16Ux4: vex_printf("Avg16Ux4"); return;
      case Iop_Max8Sx8: vex_printf("Max8Sx8"); return;
      case Iop_Max16Sx4: vex_printf("Max16Sx4"); return;
      case Iop_Max32Sx2: vex_printf("Max32Sx2"); return;
      case Iop_Max8Ux8: vex_printf("Max8Ux8"); return;
      case Iop_Max16Ux4: vex_printf("Max16Ux4"); return;
      case Iop_Max32Ux2: vex_printf("Max32Ux2"); return;
      case Iop_Min8Sx8: vex_printf("Min8Sx8"); return;
      case Iop_Min16Sx4: vex_printf("Min16Sx4"); return;
      case Iop_Min32Sx2: vex_printf("Min32Sx2"); return;
      case Iop_Min8Ux8: vex_printf("Min8Ux8"); return;
      case Iop_Min16Ux4: vex_printf("Min16Ux4"); return;
      case Iop_Min32Ux2: vex_printf("Min32Ux2"); return;
      case Iop_PwMax8Sx8: vex_printf("PwMax8Sx8"); return;
      case Iop_PwMax16Sx4: vex_printf("PwMax16Sx4"); return;
      case Iop_PwMax32Sx2: vex_printf("PwMax32Sx2"); return;
      case Iop_PwMax8Ux8: vex_printf("PwMax8Ux8"); return;
      case Iop_PwMax16Ux4: vex_printf("PwMax16Ux4"); return;
      case Iop_PwMax32Ux2: vex_printf("PwMax32Ux2"); return;
      case Iop_PwMin8Sx8: vex_printf("PwMin8Sx8"); return;
      case Iop_PwMin16Sx4: vex_printf("PwMin16Sx4"); return;
      case Iop_PwMin32Sx2: vex_printf("PwMin32Sx2"); return;
      case Iop_PwMin8Ux8: vex_printf("PwMin8Ux8"); return;
      case Iop_PwMin16Ux4: vex_printf("PwMin16Ux4"); return;
      case Iop_PwMin32Ux2: vex_printf("PwMin32Ux2"); return;
      case Iop_CmpEQ8x8: vex_printf("CmpEQ8x8"); return;
      case Iop_CmpEQ16x4: vex_printf("CmpEQ16x4"); return;
      case Iop_CmpEQ32x2: vex_printf("CmpEQ32x2"); return;
      case Iop_CmpGT8Ux8: vex_printf("CmpGT8Ux8"); return;
      case Iop_CmpGT16Ux4: vex_printf("CmpGT16Ux4"); return;
      case Iop_CmpGT32Ux2: vex_printf("CmpGT32Ux2"); return;
      case Iop_CmpGT8Sx8: vex_printf("CmpGT8Sx8"); return;
      case Iop_CmpGT16Sx4: vex_printf("CmpGT16Sx4"); return;
      case Iop_CmpGT32Sx2: vex_printf("CmpGT32Sx2"); return;
      case Iop_Cnt8x8: vex_printf("Cnt8x8"); return;
      case Iop_Clz8x8: vex_printf("Clz8x8"); return;
      case Iop_Clz16x4: vex_printf("Clz16x4"); return;
      case Iop_Clz32x2: vex_printf("Clz32x2"); return;
      case Iop_Cls8x8: vex_printf("Cls8x8"); return;
      case Iop_Cls16x4: vex_printf("Cls16x4"); return;
      case Iop_Cls32x2: vex_printf("Cls32x2"); return;
      case Iop_ShlN8x8: vex_printf("ShlN8x8"); return;
      case Iop_ShlN16x4: vex_printf("ShlN16x4"); return;
      case Iop_ShlN32x2: vex_printf("ShlN32x2"); return;
      case Iop_ShrN8x8: vex_printf("ShrN8x8"); return;
      case Iop_ShrN16x4: vex_printf("ShrN16x4"); return;
      case Iop_ShrN32x2: vex_printf("ShrN32x2"); return;
      case Iop_SarN8x8: vex_printf("SarN8x8"); return;
      case Iop_SarN16x4: vex_printf("SarN16x4"); return;
      case Iop_SarN32x2: vex_printf("SarN32x2"); return;
      case Iop_QNarrowBin16Sto8Ux8: vex_printf("QNarrowBin16Sto8Ux8"); return;
      case Iop_QNarrowBin16Sto8Sx8: vex_printf("QNarrowBin16Sto8Sx8"); return;
      case Iop_QNarrowBin32Sto16Sx4: vex_printf("QNarrowBin32Sto16Sx4"); return;
      case Iop_QNarrowBin64Sto32Sx4: vex_printf("QNarrowBin64Sto32Sx4"); return;
      case Iop_QNarrowBin64Uto32Ux4: vex_printf("QNarrowBin64Uto32Ux4"); return;
      case Iop_NarrowBin16to8x8: vex_printf("NarrowBin16to8x8"); return;
      case Iop_NarrowBin32to16x4: vex_printf("NarrowBin32to16x4"); return;
      case Iop_NarrowBin64to32x4: vex_printf("NarrowBin64to32x4"); return;
      case Iop_InterleaveHI8x8: vex_printf("InterleaveHI8x8"); return;
      case Iop_InterleaveHI16x4: vex_printf("InterleaveHI16x4"); return;
      case Iop_InterleaveHI32x2: vex_printf("InterleaveHI32x2"); return;
      case Iop_InterleaveLO8x8: vex_printf("InterleaveLO8x8"); return;
      case Iop_InterleaveLO16x4: vex_printf("InterleaveLO16x4"); return;
      case Iop_InterleaveLO32x2: vex_printf("InterleaveLO32x2"); return;
      case Iop_CatOddLanes8x8: vex_printf("CatOddLanes8x8"); return;
      case Iop_CatOddLanes16x4: vex_printf("CatOddLanes16x4"); return;
      case Iop_CatEvenLanes8x8: vex_printf("CatEvenLanes8x8"); return;
      case Iop_CatEvenLanes16x4: vex_printf("CatEvenLanes16x4"); return;
      case Iop_InterleaveOddLanes8x8: vex_printf("InterleaveOddLanes8x8"); return;
      case Iop_InterleaveOddLanes16x4: vex_printf("InterleaveOddLanes16x4"); return;
      case Iop_InterleaveEvenLanes8x8: vex_printf("InterleaveEvenLanes8x8"); return;
      case Iop_InterleaveEvenLanes16x4: vex_printf("InterleaveEvenLanes16x4"); return;
      case Iop_Shl8x8: vex_printf("Shl8x8"); return;
      case Iop_Shl16x4: vex_printf("Shl16x4"); return;
      case Iop_Shl32x2: vex_printf("Shl32x2"); return;
      case Iop_Shr8x8: vex_printf("Shr8x8"); return;
      case Iop_Shr16x4: vex_printf("Shr16x4"); return;
      case Iop_Shr32x2: vex_printf("Shr32x2"); return;
      case Iop_QShl8x8: vex_printf("QShl8x8"); return;
      case Iop_QShl16x4: vex_printf("QShl16x4"); return;
      case Iop_QShl32x2: vex_printf("QShl32x2"); return;
      case Iop_QShl64x1: vex_printf("QShl64x1"); return;
      case Iop_QSal8x8: vex_printf("QSal8x8"); return;
      case Iop_QSal16x4: vex_printf("QSal16x4"); return;
      case Iop_QSal32x2: vex_printf("QSal32x2"); return;
      case Iop_QSal64x1: vex_printf("QSal64x1"); return;
      case Iop_QShlNsatUU8x8: vex_printf("QShlNsatUU8x8"); return;
      case Iop_QShlNsatUU16x4: vex_printf("QShlNsatUU16x4"); return;
      case Iop_QShlNsatUU32x2: vex_printf("QShlNsatUU32x2"); return;
      case Iop_QShlNsatUU64x1: vex_printf("QShlNsatUU64x1"); return;
      case Iop_QShlNsatSU8x8: vex_printf("QShlNsatSU8x8"); return;
      case Iop_QShlNsatSU16x4: vex_printf("QShlNsatSU16x4"); return;
      case Iop_QShlNsatSU32x2: vex_printf("QShlNsatSU32x2"); return;
      case Iop_QShlNsatSU64x1: vex_printf("QShlNsatSU64x1"); return;
      case Iop_QShlNsatSS8x8: vex_printf("QShlNsatSS8x8"); return;
      case Iop_QShlNsatSS16x4: vex_printf("QShlNsatSS16x4"); return;
      case Iop_QShlNsatSS32x2: vex_printf("QShlNsatSS32x2"); return;
      case Iop_QShlNsatSS64x1: vex_printf("QShlNsatSS64x1"); return;
      case Iop_Sar8x8: vex_printf("Sar8x8"); return;
      case Iop_Sar16x4: vex_printf("Sar16x4"); return;
      case Iop_Sar32x2: vex_printf("Sar32x2"); return;
      case Iop_Sal8x8: vex_printf("Sal8x8"); return;
      case Iop_Sal16x4: vex_printf("Sal16x4"); return;
      case Iop_Sal32x2: vex_printf("Sal32x2"); return;
      case Iop_Sal64x1: vex_printf("Sal64x1"); return;
      case Iop_Perm8x8: vex_printf("Perm8x8"); return;
      case Iop_Reverse8sIn16_x4: vex_printf("Reverse8sIn16_x4"); return;
      case Iop_Reverse8sIn32_x2: vex_printf("Reverse8sIn32_x2"); return;
      case Iop_Reverse16sIn32_x2: vex_printf("Reverse16sIn32_x2"); return;
      case Iop_Reverse8sIn64_x1: vex_printf("Reverse8sIn64_x1"); return;
      case Iop_Reverse16sIn64_x1: vex_printf("Reverse16sIn64_x1"); return;
      case Iop_Reverse32sIn64_x1: vex_printf("Reverse32sIn64_x1"); return;
      case Iop_Abs32Fx2: vex_printf("Abs32Fx2"); return;
      case Iop_GetMSBs8x8: vex_printf("GetMSBs8x8"); return;
      case Iop_GetMSBs8x16: vex_printf("GetMSBs8x16"); return;

      case Iop_CmpNEZ32x2: vex_printf("CmpNEZ32x2"); return;
      case Iop_CmpNEZ16x4: vex_printf("CmpNEZ16x4"); return;
      case Iop_CmpNEZ8x8:  vex_printf("CmpNEZ8x8"); return;

      case Iop_Add32Fx4:  vex_printf("Add32Fx4"); return;
      case Iop_Add32Fx2:  vex_printf("Add32Fx2"); return;
      case Iop_Add32F0x4: vex_printf("Add32F0x4"); return;
      case Iop_Add64Fx2:  vex_printf("Add64Fx2"); return;
      case Iop_Add64F0x2: vex_printf("Add64F0x2"); return;

      case Iop_Div32Fx4:  vex_printf("Div32Fx4"); return;
      case Iop_Div32F0x4: vex_printf("Div32F0x4"); return;
      case Iop_Div64Fx2:  vex_printf("Div64Fx2"); return;
      case Iop_Div64F0x2: vex_printf("Div64F0x2"); return;

      case Iop_Max32Fx8:  vex_printf("Max32Fx8"); return;
      case Iop_Max32Fx4:  vex_printf("Max32Fx4"); return;
      case Iop_Max32Fx2:  vex_printf("Max32Fx2"); return;
      case Iop_PwMax32Fx4:  vex_printf("PwMax32Fx4"); return;
      case Iop_PwMax32Fx2:  vex_printf("PwMax32Fx2"); return;
      case Iop_Max32F0x4: vex_printf("Max32F0x4"); return;
      case Iop_Max64Fx4:  vex_printf("Max64Fx4"); return;
      case Iop_Max64Fx2:  vex_printf("Max64Fx2"); return;
      case Iop_Max64F0x2: vex_printf("Max64F0x2"); return;

      case Iop_Min32Fx8:  vex_printf("Min32Fx8"); return;
      case Iop_Min32Fx4:  vex_printf("Min32Fx4"); return;
      case Iop_Min32Fx2:  vex_printf("Min32Fx2"); return;
      case Iop_PwMin32Fx4:  vex_printf("PwMin32Fx4"); return;
      case Iop_PwMin32Fx2:  vex_printf("PwMin32Fx2"); return;
      case Iop_Min32F0x4: vex_printf("Min32F0x4"); return;
      case Iop_Min64Fx4:  vex_printf("Min64Fx4"); return;
      case Iop_Min64Fx2:  vex_printf("Min64Fx2"); return;
      case Iop_Min64F0x2: vex_printf("Min64F0x2"); return;

      case Iop_Mul32Fx4:  vex_printf("Mul32Fx4"); return;
      case Iop_Mul32F0x4: vex_printf("Mul32F0x4"); return;
      case Iop_Mul64Fx2:  vex_printf("Mul64Fx2"); return;
      case Iop_Mul64F0x2: vex_printf("Mul64F0x2"); return;

      case Iop_RecipEst32Ux2: vex_printf("RecipEst32Ux2"); return;
      case Iop_RecipEst32Fx2: vex_printf("RecipEst32Fx2"); return;
      case Iop_RecipEst32Fx4: vex_printf("RecipEst32Fx4"); return;
      case Iop_RecipEst32Fx8: vex_printf("RecipEst32Fx8"); return;
      case Iop_RecipEst32Ux4: vex_printf("RecipEst32Ux4"); return;
      case Iop_RecipEst32F0x4: vex_printf("RecipEst32F0x4"); return;
      case Iop_RecipStep32Fx2: vex_printf("RecipStep32Fx2"); return;
      case Iop_RecipStep32Fx4: vex_printf("RecipStep32Fx4"); return;
      case Iop_RecipEst64Fx2: vex_printf("RecipEst64Fx2"); return;
      case Iop_RecipStep64Fx2: vex_printf("RecipStep64Fx2"); return;

      case Iop_Abs32Fx4:  vex_printf("Abs32Fx4"); return;
      case Iop_Abs64Fx2:  vex_printf("Abs64Fx2"); return;
      case Iop_RSqrtStep32Fx4:  vex_printf("RSqrtStep32Fx4"); return;
      case Iop_RSqrtStep64Fx2:  vex_printf("RSqrtStep64Fx2"); return;
      case Iop_RSqrtStep32Fx2:  vex_printf("RSqrtStep32Fx2"); return;
      case Iop_RSqrtEst64Fx2: vex_printf("RSqrtEst64Fx2"); return;

      case Iop_RSqrtEst32F0x4: vex_printf("RSqrtEst32F0x4"); return;
      case Iop_RSqrtEst32Fx8: vex_printf("RSqrtEst32Fx8"); return;

      case Iop_Sqrt32Fx4:  vex_printf("Sqrt32Fx4"); return;
      case Iop_Sqrt32F0x4: vex_printf("Sqrt32F0x4"); return;
      case Iop_Sqrt64Fx2:  vex_printf("Sqrt64Fx2"); return;
      case Iop_Sqrt64F0x2: vex_printf("Sqrt64F0x2"); return;
      case Iop_Sqrt32Fx8:  vex_printf("Sqrt32Fx8"); return;
      case Iop_Sqrt64Fx4:  vex_printf("Sqrt64Fx4"); return;
 
      case Iop_Sub32Fx4:  vex_printf("Sub32Fx4"); return;
      case Iop_Sub32Fx2:  vex_printf("Sub32Fx2"); return;
      case Iop_Sub32F0x4: vex_printf("Sub32F0x4"); return;
      case Iop_Sub64Fx2:  vex_printf("Sub64Fx2"); return;
      case Iop_Sub64F0x2: vex_printf("Sub64F0x2"); return;

      case Iop_CmpEQ32Fx4: vex_printf("CmpEQ32Fx4"); return;
      case Iop_CmpLT32Fx4: vex_printf("CmpLT32Fx4"); return;
      case Iop_CmpLE32Fx4: vex_printf("CmpLE32Fx4"); return;
      case Iop_CmpGT32Fx4: vex_printf("CmpGT32Fx4"); return;
      case Iop_CmpGE32Fx4: vex_printf("CmpGE32Fx4"); return;
      case Iop_CmpUN32Fx4: vex_printf("CmpUN32Fx4"); return;
      case Iop_CmpEQ64Fx2: vex_printf("CmpEQ64Fx2"); return;
      case Iop_CmpLT64Fx2: vex_printf("CmpLT64Fx2"); return;
      case Iop_CmpLE64Fx2: vex_printf("CmpLE64Fx2"); return;
      case Iop_CmpUN64Fx2: vex_printf("CmpUN64Fx2"); return;
      case Iop_CmpGT32Fx2: vex_printf("CmpGT32Fx2"); return;
      case Iop_CmpEQ32Fx2: vex_printf("CmpEQ32Fx2"); return;
      case Iop_CmpGE32Fx2: vex_printf("CmpGE32Fx2"); return;

      case Iop_CmpEQ32F0x4: vex_printf("CmpEQ32F0x4"); return;
      case Iop_CmpLT32F0x4: vex_printf("CmpLT32F0x4"); return;
      case Iop_CmpLE32F0x4: vex_printf("CmpLE32F0x4"); return;
      case Iop_CmpUN32F0x4: vex_printf("CmpUN32F0x4"); return;
      case Iop_CmpEQ64F0x2: vex_printf("CmpEQ64F0x2"); return;
      case Iop_CmpLT64F0x2: vex_printf("CmpLT64F0x2"); return;
      case Iop_CmpLE64F0x2: vex_printf("CmpLE64F0x2"); return;
      case Iop_CmpUN64F0x2: vex_printf("CmpUN64F0x2"); return;

      case Iop_Neg64Fx2: vex_printf("Neg64Fx2"); return;
      case Iop_Neg32Fx4: vex_printf("Neg32Fx4"); return;
      case Iop_Neg32Fx2: vex_printf("Neg32Fx2"); return;

      case Iop_V128to64:   vex_printf("V128to64");   return;
      case Iop_V128HIto64: vex_printf("V128HIto64"); return;
      case Iop_64HLtoV128: vex_printf("64HLtoV128"); return;

      case Iop_64UtoV128:   vex_printf("64UtoV128"); return;
      case Iop_SetV128lo64: vex_printf("SetV128lo64"); return;

      case Iop_ZeroHI64ofV128:  vex_printf("ZeroHI64ofV128"); return;
      case Iop_ZeroHI96ofV128:  vex_printf("ZeroHI96ofV128"); return;
      case Iop_ZeroHI112ofV128: vex_printf("ZeroHI112ofV128"); return;
      case Iop_ZeroHI120ofV128: vex_printf("ZeroHI120ofV128"); return;

      case Iop_32UtoV128:   vex_printf("32UtoV128"); return;
      case Iop_V128to32:    vex_printf("V128to32"); return;
      case Iop_SetV128lo32: vex_printf("SetV128lo32"); return;

      case Iop_Dup8x16: vex_printf("Dup8x16"); return;
      case Iop_Dup16x8: vex_printf("Dup16x8"); return;
      case Iop_Dup32x4: vex_printf("Dup32x4"); return;
      case Iop_Dup8x8: vex_printf("Dup8x8"); return;
      case Iop_Dup16x4: vex_printf("Dup16x4"); return;
      case Iop_Dup32x2: vex_printf("Dup32x2"); return;

      case Iop_NotV128:    vex_printf("NotV128"); return;
      case Iop_AndV128:    vex_printf("AndV128"); return;
      case Iop_OrV128:     vex_printf("OrV128");  return;
      case Iop_XorV128:    vex_printf("XorV128"); return;

      case Iop_CmpNEZ8x16: vex_printf("CmpNEZ8x16"); return;
      case Iop_CmpNEZ16x8: vex_printf("CmpNEZ16x8"); return;
      case Iop_CmpNEZ32x4: vex_printf("CmpNEZ32x4"); return;
      case Iop_CmpNEZ64x2: vex_printf("CmpNEZ64x2"); return;

      case Iop_Abs8x16: vex_printf("Abs8x16"); return;
      case Iop_Abs16x8: vex_printf("Abs16x8"); return;
      case Iop_Abs32x4: vex_printf("Abs32x4"); return;
      case Iop_Abs64x2: vex_printf("Abs64x2"); return;

      case Iop_Add8x16:   vex_printf("Add8x16"); return;
      case Iop_Add16x8:   vex_printf("Add16x8"); return;
      case Iop_Add32x4:   vex_printf("Add32x4"); return;
      case Iop_Add64x2:   vex_printf("Add64x2"); return;
      case Iop_QAdd8Ux16: vex_printf("QAdd8Ux16"); return;
      case Iop_QAdd16Ux8: vex_printf("QAdd16Ux8"); return;
      case Iop_QAdd32Ux4: vex_printf("QAdd32Ux4"); return;
      case Iop_QAdd8Sx16: vex_printf("QAdd8Sx16"); return;
      case Iop_QAdd16Sx8: vex_printf("QAdd16Sx8"); return;
      case Iop_QAdd32Sx4: vex_printf("QAdd32Sx4"); return;
      case Iop_QAdd64Ux2: vex_printf("QAdd64Ux2"); return;
      case Iop_QAdd64Sx2: vex_printf("QAdd64Sx2"); return;

      case Iop_QAddExtUSsatSS8x16: vex_printf("QAddExtUSsatSS8x16"); return;
      case Iop_QAddExtUSsatSS16x8: vex_printf("QAddExtUSsatSS16x8"); return;
      case Iop_QAddExtUSsatSS32x4: vex_printf("QAddExtUSsatSS32x4"); return;
      case Iop_QAddExtUSsatSS64x2: vex_printf("QAddExtUSsatSS64x2"); return;
      case Iop_QAddExtSUsatUU8x16: vex_printf("QAddExtSUsatUU8x16"); return;
      case Iop_QAddExtSUsatUU16x8: vex_printf("QAddExtSUsatUU16x8"); return;
      case Iop_QAddExtSUsatUU32x4: vex_printf("QAddExtSUsatUU32x4"); return;
      case Iop_QAddExtSUsatUU64x2: vex_printf("QAddExtSUsatUU64x2"); return;

      case Iop_PwAdd8x16: vex_printf("PwAdd8x16"); return;
      case Iop_PwAdd16x8: vex_printf("PwAdd16x8"); return;
      case Iop_PwAdd32x4: vex_printf("PwAdd32x4"); return;
      case Iop_PwAddL8Ux16: vex_printf("PwAddL8Ux16"); return;
      case Iop_PwAddL16Ux8: vex_printf("PwAddL16Ux8"); return;
      case Iop_PwAddL32Ux4: vex_printf("PwAddL32Ux4"); return;
      case Iop_PwAddL8Sx16: vex_printf("PwAddL8Sx16"); return;
      case Iop_PwAddL16Sx8: vex_printf("PwAddL16Sx8"); return;
      case Iop_PwAddL32Sx4: vex_printf("PwAddL32Sx4"); return;

      case Iop_Sub8x16:   vex_printf("Sub8x16"); return;
      case Iop_Sub16x8:   vex_printf("Sub16x8"); return;
      case Iop_Sub32x4:   vex_printf("Sub32x4"); return;
      case Iop_Sub64x2:   vex_printf("Sub64x2"); return;
      case Iop_QSub8Ux16: vex_printf("QSub8Ux16"); return;
      case Iop_QSub16Ux8: vex_printf("QSub16Ux8"); return;
      case Iop_QSub32Ux4: vex_printf("QSub32Ux4"); return;
      case Iop_QSub8Sx16: vex_printf("QSub8Sx16"); return;
      case Iop_QSub16Sx8: vex_printf("QSub16Sx8"); return;
      case Iop_QSub32Sx4: vex_printf("QSub32Sx4"); return;
      case Iop_QSub64Ux2: vex_printf("QSub64Ux2"); return;
      case Iop_QSub64Sx2: vex_printf("QSub64Sx2"); return;

      case Iop_Mul8x16:    vex_printf("Mul8x16"); return;
      case Iop_Mul16x8:    vex_printf("Mul16x8"); return;
      case Iop_Mul32x4:    vex_printf("Mul32x4"); return;
      case Iop_Mull8Ux8:    vex_printf("Mull8Ux8"); return;
      case Iop_Mull8Sx8:    vex_printf("Mull8Sx8"); return;
      case Iop_Mull16Ux4:    vex_printf("Mull16Ux4"); return;
      case Iop_Mull16Sx4:    vex_printf("Mull16Sx4"); return;
      case Iop_Mull32Ux2:    vex_printf("Mull32Ux2"); return;
      case Iop_Mull32Sx2:    vex_printf("Mull32Sx2"); return;
      case Iop_PolynomialMul8x16: vex_printf("PolynomialMul8x16"); return;
      case Iop_PolynomialMull8x8: vex_printf("PolynomialMull8x8"); return;
      case Iop_MulHi16Ux8: vex_printf("MulHi16Ux8"); return;
      case Iop_MulHi32Ux4: vex_printf("MulHi32Ux4"); return;
      case Iop_MulHi16Sx8: vex_printf("MulHi16Sx8"); return;
      case Iop_MulHi32Sx4: vex_printf("MulHi32Sx4"); return;
      case Iop_QDMulHi16Sx8: vex_printf("QDMulHi16Sx8"); return;
      case Iop_QDMulHi32Sx4: vex_printf("QDMulHi32Sx4"); return;
      case Iop_QRDMulHi16Sx8: vex_printf("QRDMulHi16Sx8"); return;
      case Iop_QRDMulHi32Sx4: vex_printf("QRDMulHi32Sx4"); return;

      case Iop_MullEven8Ux16: vex_printf("MullEven8Ux16"); return;
      case Iop_MullEven16Ux8: vex_printf("MullEven16Ux8"); return;
      case Iop_MullEven32Ux4: vex_printf("MullEven32Ux4"); return;
      case Iop_MullEven8Sx16: vex_printf("MullEven8Sx16"); return;
      case Iop_MullEven16Sx8: vex_printf("MullEven16Sx8"); return;
      case Iop_MullEven32Sx4: vex_printf("MullEven32Sx4"); return;

      case Iop_PolynomialMulAdd8x16:
         vex_printf("PolynomialMulAdd8x16"); return;
      case Iop_PolynomialMulAdd16x8:
         vex_printf("PolynomialMulAdd16x8"); return;
      case Iop_PolynomialMulAdd32x4:
         vex_printf("PolynomialMulAdd32x4"); return;
      case Iop_PolynomialMulAdd64x2:
         vex_printf("PolynomialMulAdd64x2"); return;

      case Iop_Avg8Ux16: vex_printf("Avg8Ux16"); return;
      case Iop_Avg16Ux8: vex_printf("Avg16Ux8"); return;
      case Iop_Avg32Ux4: vex_printf("Avg32Ux4"); return;
      case Iop_Avg8Sx16: vex_printf("Avg8Sx16"); return;
      case Iop_Avg16Sx8: vex_printf("Avg16Sx8"); return;
      case Iop_Avg32Sx4: vex_printf("Avg32Sx4"); return;

      case Iop_Max8Sx16: vex_printf("Max8Sx16"); return;
      case Iop_Max16Sx8: vex_printf("Max16Sx8"); return;
      case Iop_Max32Sx4: vex_printf("Max32Sx4"); return;
      case Iop_Max64Sx2: vex_printf("Max64Sx2"); return;
      case Iop_Max8Ux16: vex_printf("Max8Ux16"); return;
      case Iop_Max16Ux8: vex_printf("Max16Ux8"); return;
      case Iop_Max32Ux4: vex_printf("Max32Ux4"); return;
      case Iop_Max64Ux2: vex_printf("Max64Ux2"); return;

      case Iop_Min8Sx16: vex_printf("Min8Sx16"); return;
      case Iop_Min16Sx8: vex_printf("Min16Sx8"); return;
      case Iop_Min32Sx4: vex_printf("Min32Sx4"); return;
      case Iop_Min64Sx2: vex_printf("Min64Sx2"); return;
      case Iop_Min8Ux16: vex_printf("Min8Ux16"); return;
      case Iop_Min16Ux8: vex_printf("Min16Ux8"); return;
      case Iop_Min32Ux4: vex_printf("Min32Ux4"); return;
      case Iop_Min64Ux2: vex_printf("Min64Ux2"); return;

      case Iop_CmpEQ8x16:  vex_printf("CmpEQ8x16"); return;
      case Iop_CmpEQ16x8:  vex_printf("CmpEQ16x8"); return;
      case Iop_CmpEQ32x4:  vex_printf("CmpEQ32x4"); return;
      case Iop_CmpEQ64x2:  vex_printf("CmpEQ64x2"); return;
      case Iop_CmpGT8Sx16: vex_printf("CmpGT8Sx16"); return;
      case Iop_CmpGT16Sx8: vex_printf("CmpGT16Sx8"); return;
      case Iop_CmpGT32Sx4: vex_printf("CmpGT32Sx4"); return;
      case Iop_CmpGT64Sx2: vex_printf("CmpGT64Sx2"); return;
      case Iop_CmpGT8Ux16: vex_printf("CmpGT8Ux16"); return;
      case Iop_CmpGT16Ux8: vex_printf("CmpGT16Ux8"); return;
      case Iop_CmpGT32Ux4: vex_printf("CmpGT32Ux4"); return;
      case Iop_CmpGT64Ux2: vex_printf("CmpGT64Ux2"); return;

      case Iop_Cnt8x16: vex_printf("Cnt8x16"); return;
      case Iop_Clz8x16: vex_printf("Clz8x16"); return;
      case Iop_Clz16x8: vex_printf("Clz16x8"); return;
      case Iop_Clz32x4: vex_printf("Clz32x4"); return;
      case Iop_Clz64x2: vex_printf("Clz64x2"); return;
      case Iop_Cls8x16: vex_printf("Cls8x16"); return;
      case Iop_Cls16x8: vex_printf("Cls16x8"); return;
      case Iop_Cls32x4: vex_printf("Cls32x4"); return;

      case Iop_ShlV128: vex_printf("ShlV128"); return;
      case Iop_ShrV128: vex_printf("ShrV128"); return;

      case Iop_ShlN8x16: vex_printf("ShlN8x16"); return;
      case Iop_ShlN16x8: vex_printf("ShlN16x8"); return;
      case Iop_ShlN32x4: vex_printf("ShlN32x4"); return;
      case Iop_ShlN64x2: vex_printf("ShlN64x2"); return;
      case Iop_ShrN8x16: vex_printf("ShrN8x16"); return;
      case Iop_ShrN16x8: vex_printf("ShrN16x8"); return;
      case Iop_ShrN32x4: vex_printf("ShrN32x4"); return;
      case Iop_ShrN64x2: vex_printf("ShrN64x2"); return;
      case Iop_SarN8x16: vex_printf("SarN8x16"); return;
      case Iop_SarN16x8: vex_printf("SarN16x8"); return;
      case Iop_SarN32x4: vex_printf("SarN32x4"); return;
      case Iop_SarN64x2: vex_printf("SarN64x2"); return;

      case Iop_Shl8x16: vex_printf("Shl8x16"); return;
      case Iop_Shl16x8: vex_printf("Shl16x8"); return;
      case Iop_Shl32x4: vex_printf("Shl32x4"); return;
      case Iop_Shl64x2: vex_printf("Shl64x2"); return;
      case Iop_QSal8x16: vex_printf("QSal8x16"); return;
      case Iop_QSal16x8: vex_printf("QSal16x8"); return;
      case Iop_QSal32x4: vex_printf("QSal32x4"); return;
      case Iop_QSal64x2: vex_printf("QSal64x2"); return;
      case Iop_QShl8x16: vex_printf("QShl8x16"); return;
      case Iop_QShl16x8: vex_printf("QShl16x8"); return;
      case Iop_QShl32x4: vex_printf("QShl32x4"); return;
      case Iop_QShl64x2: vex_printf("QShl64x2"); return;
      case Iop_QShlNsatSS8x16: vex_printf("QShlNsatSS8x16"); return;
      case Iop_QShlNsatSS16x8: vex_printf("QShlNsatSS16x8"); return;
      case Iop_QShlNsatSS32x4: vex_printf("QShlNsatSS32x4"); return;
      case Iop_QShlNsatSS64x2: vex_printf("QShlNsatSS64x2"); return;
      case Iop_QShlNsatUU8x16: vex_printf("QShlNsatUU8x16"); return;
      case Iop_QShlNsatUU16x8: vex_printf("QShlNsatUU16x8"); return;
      case Iop_QShlNsatUU32x4: vex_printf("QShlNsatUU32x4"); return;
      case Iop_QShlNsatUU64x2: vex_printf("QShlNsatUU64x2"); return;
      case Iop_QShlNsatSU8x16: vex_printf("QShlNsatSU8x16"); return;
      case Iop_QShlNsatSU16x8: vex_printf("QShlNsatSU16x8"); return;
      case Iop_QShlNsatSU32x4: vex_printf("QShlNsatSU32x4"); return;
      case Iop_QShlNsatSU64x2: vex_printf("QShlNsatSU64x2"); return;
      case Iop_Shr8x16: vex_printf("Shr8x16"); return;
      case Iop_Shr16x8: vex_printf("Shr16x8"); return;
      case Iop_Shr32x4: vex_printf("Shr32x4"); return;
      case Iop_Shr64x2: vex_printf("Shr64x2"); return;
      case Iop_Sar8x16: vex_printf("Sar8x16"); return;
      case Iop_Sar16x8: vex_printf("Sar16x8"); return;
      case Iop_Sar32x4: vex_printf("Sar32x4"); return;
      case Iop_Sar64x2: vex_printf("Sar64x2"); return;
      case Iop_Sal8x16: vex_printf("Sal8x16"); return;
      case Iop_Sal16x8: vex_printf("Sal16x8"); return;
      case Iop_Sal32x4: vex_printf("Sal32x4"); return;
      case Iop_Sal64x2: vex_printf("Sal64x2"); return;
      case Iop_Rol8x16: vex_printf("Rol8x16"); return;
      case Iop_Rol16x8: vex_printf("Rol16x8"); return;
      case Iop_Rol32x4: vex_printf("Rol32x4"); return;
      case Iop_Rol64x2: vex_printf("Rol64x2"); return;

      case Iop_QandUQsh8x16: vex_printf("QandUQsh8x16"); return;
      case Iop_QandUQsh16x8: vex_printf("QandUQsh16x8"); return;
      case Iop_QandUQsh32x4: vex_printf("QandUQsh32x4"); return;
      case Iop_QandUQsh64x2: vex_printf("QandUQsh64x2"); return;
      case Iop_QandSQsh8x16: vex_printf("QandSQsh8x16"); return;
      case Iop_QandSQsh16x8: vex_printf("QandSQsh16x8"); return;
      case Iop_QandSQsh32x4: vex_printf("QandSQsh32x4"); return;
      case Iop_QandSQsh64x2: vex_printf("QandSQsh64x2"); return;
      case Iop_QandUQRsh8x16: vex_printf("QandUQRsh8x16"); return;
      case Iop_QandUQRsh16x8: vex_printf("QandUQRsh16x8"); return;
      case Iop_QandUQRsh32x4: vex_printf("QandUQRsh32x4"); return;
      case Iop_QandUQRsh64x2: vex_printf("QandUQRsh64x2"); return;
      case Iop_QandSQRsh8x16: vex_printf("QandSQRsh8x16"); return;
      case Iop_QandSQRsh16x8: vex_printf("QandSQRsh16x8"); return;
      case Iop_QandSQRsh32x4: vex_printf("QandSQRsh32x4"); return;
      case Iop_QandSQRsh64x2: vex_printf("QandSQRsh64x2"); return;

      case Iop_Sh8Sx16: vex_printf("Sh8Sx16"); return;
      case Iop_Sh16Sx8: vex_printf("Sh16Sx8"); return;
      case Iop_Sh32Sx4: vex_printf("Sh32Sx4"); return;
      case Iop_Sh64Sx2: vex_printf("Sh64Sx2"); return;
      case Iop_Sh8Ux16: vex_printf("Sh8Ux16"); return;
      case Iop_Sh16Ux8: vex_printf("Sh16Ux8"); return;
      case Iop_Sh32Ux4: vex_printf("Sh32Ux4"); return;
      case Iop_Sh64Ux2: vex_printf("Sh64Ux2"); return;
      case Iop_Rsh8Sx16: vex_printf("Rsh8Sx16"); return;
      case Iop_Rsh16Sx8: vex_printf("Rsh16Sx8"); return;
      case Iop_Rsh32Sx4: vex_printf("Rsh32Sx4"); return;
      case Iop_Rsh64Sx2: vex_printf("Rsh64Sx2"); return;
      case Iop_Rsh8Ux16: vex_printf("Rsh8Ux16"); return;
      case Iop_Rsh16Ux8: vex_printf("Rsh16Ux8"); return;
      case Iop_Rsh32Ux4: vex_printf("Rsh32Ux4"); return;
      case Iop_Rsh64Ux2: vex_printf("Rsh64Ux2"); return;

      case Iop_QandQShrNnarrow16Uto8Ux8:
         vex_printf("QandQShrNnarrow16Uto8Ux8"); return;
      case Iop_QandQShrNnarrow32Uto16Ux4:
         vex_printf("QandQShrNnarrow32Uto16Ux4"); return;
      case Iop_QandQShrNnarrow64Uto32Ux2:
         vex_printf("QandQShrNnarrow64Uto32Ux2"); return;
      case Iop_QandQSarNnarrow16Sto8Sx8:
         vex_printf("QandQSarNnarrow16Sto8Sx8"); return;
      case Iop_QandQSarNnarrow32Sto16Sx4:
         vex_printf("QandQSarNnarrow32Sto16Sx4"); return;
      case Iop_QandQSarNnarrow64Sto32Sx2:
         vex_printf("QandQSarNnarrow64Sto32Sx2"); return;
      case Iop_QandQSarNnarrow16Sto8Ux8:
         vex_printf("QandQSarNnarrow16Sto8Ux8"); return;
      case Iop_QandQSarNnarrow32Sto16Ux4:
         vex_printf("QandQSarNnarrow32Sto16Ux4"); return;
      case Iop_QandQSarNnarrow64Sto32Ux2:
         vex_printf("QandQSarNnarrow64Sto32Ux2"); return;
      case Iop_QandQRShrNnarrow16Uto8Ux8:
         vex_printf("QandQRShrNnarrow16Uto8Ux8"); return;
      case Iop_QandQRShrNnarrow32Uto16Ux4:
         vex_printf("QandQRShrNnarrow32Uto16Ux4"); return;
      case Iop_QandQRShrNnarrow64Uto32Ux2:
         vex_printf("QandQRShrNnarrow64Uto32Ux2"); return;
      case Iop_QandQRSarNnarrow16Sto8Sx8:
         vex_printf("QandQRSarNnarrow16Sto8Sx8"); return;
      case Iop_QandQRSarNnarrow32Sto16Sx4:
         vex_printf("QandQRSarNnarrow32Sto16Sx4"); return;
      case Iop_QandQRSarNnarrow64Sto32Sx2:
         vex_printf("QandQRSarNnarrow64Sto32Sx2"); return;
      case Iop_QandQRSarNnarrow16Sto8Ux8:
         vex_printf("QandQRSarNnarrow16Sto8Ux8"); return;
      case Iop_QandQRSarNnarrow32Sto16Ux4:
         vex_printf("QandQRSarNnarrow32Sto16Ux4"); return;
      case Iop_QandQRSarNnarrow64Sto32Ux2:
         vex_printf("QandQRSarNnarrow64Sto32Ux2"); return;

      case Iop_NarrowBin16to8x16:    vex_printf("NarrowBin16to8x16"); return;
      case Iop_NarrowBin32to16x8:    vex_printf("NarrowBin32to16x8"); return;
      case Iop_QNarrowBin16Uto8Ux16: vex_printf("QNarrowBin16Uto8Ux16"); return;
      case Iop_QNarrowBin32Sto16Ux8: vex_printf("QNarrowBin32Sto16Ux8"); return;
      case Iop_QNarrowBin16Sto8Ux16: vex_printf("QNarrowBin16Sto8Ux16"); return;
      case Iop_QNarrowBin32Uto16Ux8: vex_printf("QNarrowBin32Uto16Ux8"); return;
      case Iop_QNarrowBin16Sto8Sx16: vex_printf("QNarrowBin16Sto8Sx16"); return;
      case Iop_QNarrowBin32Sto16Sx8: vex_printf("QNarrowBin32Sto16Sx8"); return;
      case Iop_NarrowUn16to8x8:     vex_printf("NarrowUn16to8x8");  return;
      case Iop_NarrowUn32to16x4:    vex_printf("NarrowUn32to16x4"); return;
      case Iop_NarrowUn64to32x2:    vex_printf("NarrowUn64to32x2"); return;
      case Iop_QNarrowUn16Uto8Ux8:  vex_printf("QNarrowUn16Uto8Ux8");  return;
      case Iop_QNarrowUn32Uto16Ux4: vex_printf("QNarrowUn32Uto16Ux4"); return;
      case Iop_QNarrowUn64Uto32Ux2: vex_printf("QNarrowUn64Uto32Ux2"); return;
      case Iop_QNarrowUn16Sto8Sx8:  vex_printf("QNarrowUn16Sto8Sx8");  return;
      case Iop_QNarrowUn32Sto16Sx4: vex_printf("QNarrowUn32Sto16Sx4"); return;
      case Iop_QNarrowUn64Sto32Sx2: vex_printf("QNarrowUn64Sto32Sx2"); return;
      case Iop_QNarrowUn16Sto8Ux8:  vex_printf("QNarrowUn16Sto8Ux8");  return;
      case Iop_QNarrowUn32Sto16Ux4: vex_printf("QNarrowUn32Sto16Ux4"); return;
      case Iop_QNarrowUn64Sto32Ux2: vex_printf("QNarrowUn64Sto32Ux2"); return;
      case Iop_Widen8Uto16x8:  vex_printf("Widen8Uto16x8");  return;
      case Iop_Widen16Uto32x4: vex_printf("Widen16Uto32x4"); return;
      case Iop_Widen32Uto64x2: vex_printf("Widen32Uto64x2"); return;
      case Iop_Widen8Sto16x8:  vex_printf("Widen8Sto16x8");  return;
      case Iop_Widen16Sto32x4: vex_printf("Widen16Sto32x4"); return;
      case Iop_Widen32Sto64x2: vex_printf("Widen32Sto64x2"); return;

      case Iop_InterleaveHI8x16: vex_printf("InterleaveHI8x16"); return;
      case Iop_InterleaveHI16x8: vex_printf("InterleaveHI16x8"); return;
      case Iop_InterleaveHI32x4: vex_printf("InterleaveHI32x4"); return;
      case Iop_InterleaveHI64x2: vex_printf("InterleaveHI64x2"); return;
      case Iop_InterleaveLO8x16: vex_printf("InterleaveLO8x16"); return;
      case Iop_InterleaveLO16x8: vex_printf("InterleaveLO16x8"); return;
      case Iop_InterleaveLO32x4: vex_printf("InterleaveLO32x4"); return;
      case Iop_InterleaveLO64x2: vex_printf("InterleaveLO64x2"); return;

      case Iop_CatOddLanes8x16: vex_printf("CatOddLanes8x16"); return;
      case Iop_CatOddLanes16x8: vex_printf("CatOddLanes16x8"); return;
      case Iop_CatOddLanes32x4: vex_printf("CatOddLanes32x4"); return;
      case Iop_CatEvenLanes8x16: vex_printf("CatEvenLanes8x16"); return;
      case Iop_CatEvenLanes16x8: vex_printf("CatEvenLanes16x8"); return;
      case Iop_CatEvenLanes32x4: vex_printf("CatEvenLanes32x4"); return;

      case Iop_InterleaveOddLanes8x16: vex_printf("InterleaveOddLanes8x16"); return;
      case Iop_InterleaveOddLanes16x8: vex_printf("InterleaveOddLanes16x8"); return;
      case Iop_InterleaveOddLanes32x4: vex_printf("InterleaveOddLanes32x4"); return;
      case Iop_InterleaveEvenLanes8x16: vex_printf("InterleaveEvenLanes8x16"); return;
      case Iop_InterleaveEvenLanes16x8: vex_printf("InterleaveEvenLanes16x8"); return;
      case Iop_InterleaveEvenLanes32x4: vex_printf("InterleaveEvenLanes32x4"); return;

      case Iop_GetElem8x16: vex_printf("GetElem8x16"); return;
      case Iop_GetElem16x8: vex_printf("GetElem16x8"); return;
      case Iop_GetElem32x4: vex_printf("GetElem32x4"); return;
      case Iop_GetElem64x2: vex_printf("GetElem64x2"); return;

      case Iop_GetElem8x8: vex_printf("GetElem8x8"); return;
      case Iop_GetElem16x4: vex_printf("GetElem16x4"); return;
      case Iop_GetElem32x2: vex_printf("GetElem32x2"); return;
      case Iop_SetElem8x8: vex_printf("SetElem8x8"); return;
      case Iop_SetElem16x4: vex_printf("SetElem16x4"); return;
      case Iop_SetElem32x2: vex_printf("SetElem32x2"); return;

      case Iop_Slice64: vex_printf("Slice64"); return;
      case Iop_SliceV128: vex_printf("SliceV128"); return;

      case Iop_Perm8x16: vex_printf("Perm8x16"); return;
      case Iop_Perm32x4: vex_printf("Perm32x4"); return;
      case Iop_Reverse8sIn16_x8: vex_printf("Reverse8sIn16_x8"); return;
      case Iop_Reverse8sIn32_x4: vex_printf("Reverse8sIn32_x4"); return;
      case Iop_Reverse16sIn32_x4: vex_printf("Reverse16sIn32_x4"); return;
      case Iop_Reverse8sIn64_x2: vex_printf("Reverse8sIn64_x2"); return;
      case Iop_Reverse16sIn64_x2: vex_printf("Reverse16sIn64_x2"); return;
      case Iop_Reverse32sIn64_x2: vex_printf("Reverse32sIn64_x2"); return;
      case Iop_Reverse1sIn8_x16: vex_printf("Reverse1sIn8_x16"); return;

      case Iop_F32ToFixed32Ux4_RZ: vex_printf("F32ToFixed32Ux4_RZ"); return;
      case Iop_F32ToFixed32Sx4_RZ: vex_printf("F32ToFixed32Sx4_RZ"); return;
      case Iop_Fixed32UToF32x4_RN: vex_printf("Fixed32UToF32x4_RN"); return;
      case Iop_Fixed32SToF32x4_RN: vex_printf("Fixed32SToF32x4_RN"); return;
      case Iop_F32ToFixed32Ux2_RZ: vex_printf("F32ToFixed32Ux2_RZ"); return;
      case Iop_F32ToFixed32Sx2_RZ: vex_printf("F32ToFixed32Sx2_RZ"); return;
      case Iop_Fixed32UToF32x2_RN: vex_printf("Fixed32UToF32x2_RN"); return;
      case Iop_Fixed32SToF32x2_RN: vex_printf("Fixed32SToF32x2_RN"); return;

      case Iop_D32toD64:  vex_printf("D32toD64");   return;
      case Iop_D64toD32:  vex_printf("D64toD32");   return;
      case Iop_AddD64:  vex_printf("AddD64");   return;
      case Iop_SubD64:  vex_printf("SubD64");   return;
      case Iop_MulD64:  vex_printf("MulD64");   return;
      case Iop_DivD64:  vex_printf("DivD64");   return;
      case Iop_ShlD64:  vex_printf("ShlD64"); return;
      case Iop_ShrD64:  vex_printf("ShrD64"); return;
      case Iop_D64toI32S:  vex_printf("D64toI32S");  return;
      case Iop_D64toI32U:  vex_printf("D64toI32U");  return;
      case Iop_D64toI64S:  vex_printf("D64toI64S");  return;
      case Iop_D64toI64U:  vex_printf("D64toI64U");  return;
      case Iop_I32StoD64:  vex_printf("I32StoD64");  return;
      case Iop_I32UtoD64:  vex_printf("I32UtoD64");  return;
      case Iop_I64StoD64:  vex_printf("I64StoD64");  return;
      case Iop_I64UtoD64:  vex_printf("I64UtoD64");  return;
      case Iop_I32StoD128: vex_printf("I32StoD128"); return;
      case Iop_I32UtoD128: vex_printf("I32UtoD128"); return;
      case Iop_I64StoD128: vex_printf("I64StoD128"); return;
      case Iop_I64UtoD128: vex_printf("I64UtoD128"); return;
      case Iop_D64toD128:  vex_printf("D64toD128");  return;
      case Iop_D128toD64:  vex_printf("D128toD64");  return;
      case Iop_D128toI32S: vex_printf("D128toI32S"); return;
      case Iop_D128toI32U: vex_printf("D128toI32U"); return;
      case Iop_D128toI64S: vex_printf("D128toI64S"); return;
      case Iop_D128toI64U: vex_printf("D128toI64U"); return;
      case Iop_F32toD32:   vex_printf("F32toD32");   return;
      case Iop_F32toD64:   vex_printf("F32toD64");   return;
      case Iop_F32toD128:  vex_printf("F32toD128");  return;
      case Iop_F64toD32:   vex_printf("F64toD32");   return;
      case Iop_F64toD64:   vex_printf("F64toD64");   return;
      case Iop_F64toD128:  vex_printf("F64toD128");  return;
      case Iop_F128toD32:  vex_printf("F128toD32");  return;
      case Iop_F128toD64:  vex_printf("F128toD64");  return;
      case Iop_F128toD128: vex_printf("F128toD128"); return;
      case Iop_D32toF32:   vex_printf("D32toF32");   return;
      case Iop_D32toF64:   vex_printf("D32toF64");   return;
      case Iop_D32toF128:  vex_printf("D32toF128");  return;
      case Iop_D64toF32:   vex_printf("D64toF32");   return;
      case Iop_D64toF64:   vex_printf("D64toF64");   return;
      case Iop_D64toF128:  vex_printf("D64toF128");  return;
      case Iop_D128toF32:  vex_printf("D128toF32");  return;
      case Iop_D128toF64:  vex_printf("D128toF64");  return;
      case Iop_D128toF128: vex_printf("D128toF128"); return;
      case Iop_AddD128: vex_printf("AddD128");  return;
      case Iop_SubD128: vex_printf("SubD128");  return;
      case Iop_MulD128: vex_printf("MulD128");  return;
      case Iop_DivD128: vex_printf("DivD128");  return;
      case Iop_ShlD128: vex_printf("ShlD128");  return;
      case Iop_ShrD128: vex_printf("ShrD128");  return;
      case Iop_RoundD64toInt:  vex_printf("RoundD64toInt");  return;
      case Iop_RoundD128toInt: vex_printf("RoundD128toInt"); return;
      case Iop_QuantizeD64:    vex_printf("QuantizeD64");    return;
      case Iop_QuantizeD128:   vex_printf("QuantizeD128");   return;
      case Iop_ExtractExpD64:  vex_printf("ExtractExpD64");  return;
      case Iop_ExtractExpD128: vex_printf("ExtractExpD128"); return;
      case Iop_ExtractSigD64:  vex_printf("ExtractSigD64");  return;
      case Iop_ExtractSigD128: vex_printf("ExtractSigD128"); return;
      case Iop_InsertExpD64:   vex_printf("InsertExpD64");   return;
      case Iop_InsertExpD128:  vex_printf("InsertExpD128");  return;
      case Iop_CmpD64:         vex_printf("CmpD64");     return;
      case Iop_CmpD128:        vex_printf("CmpD128");    return;
      case Iop_CmpExpD64:      vex_printf("CmpExpD64");  return;
      case Iop_CmpExpD128:     vex_printf("CmpExpD128"); return;
      case Iop_D64HLtoD128: vex_printf("D64HLtoD128");   return;
      case Iop_D128HItoD64: vex_printf("D128HItoD64");   return;
      case Iop_D128LOtoD64: vex_printf("D128LOtoD64");   return;
      case Iop_SignificanceRoundD64: vex_printf("SignificanceRoundD64");
         return;
      case Iop_SignificanceRoundD128: vex_printf("SignificanceRoundD128");
         return;
      case Iop_ReinterpI64asD64: vex_printf("ReinterpI64asD64"); return;
      case Iop_ReinterpD64asI64: vex_printf("ReinterpD64asI64"); return;
      case Iop_V256to64_0: vex_printf("V256to64_0"); return;
      case Iop_V256to64_1: vex_printf("V256to64_1"); return;
      case Iop_V256to64_2: vex_printf("V256to64_2"); return;
      case Iop_V256to64_3: vex_printf("V256to64_3"); return;
      case Iop_64x4toV256: vex_printf("64x4toV256"); return;
      case Iop_V256toV128_0: vex_printf("V256toV128_0"); return;
      case Iop_V256toV128_1: vex_printf("V256toV128_1"); return;
      case Iop_V128HLtoV256: vex_printf("V128HLtoV256"); return;
      case Iop_DPBtoBCD: vex_printf("DPBtoBCD"); return;
      case Iop_BCDtoDPB: vex_printf("BCDtoDPB"); return;
      case Iop_Add64Fx4: vex_printf("Add64Fx4"); return;
      case Iop_Sub64Fx4: vex_printf("Sub64Fx4"); return;
      case Iop_Mul64Fx4: vex_printf("Mul64Fx4"); return;
      case Iop_Div64Fx4: vex_printf("Div64Fx4"); return;
      case Iop_Add32Fx8: vex_printf("Add32Fx8"); return;
      case Iop_Sub32Fx8: vex_printf("Sub32Fx8"); return;
      case Iop_Mul32Fx8: vex_printf("Mul32Fx8"); return;
      case Iop_Div32Fx8: vex_printf("Div32Fx8"); return;
      case Iop_AndV256: vex_printf("AndV256"); return;
      case Iop_OrV256:  vex_printf("OrV256"); return;
      case Iop_XorV256: vex_printf("XorV256"); return;
      case Iop_NotV256: vex_printf("NotV256"); return;
      case Iop_CmpNEZ64x4: vex_printf("CmpNEZ64x4"); return;
      case Iop_CmpNEZ32x8: vex_printf("CmpNEZ32x8"); return;
      case Iop_CmpNEZ16x16: vex_printf("CmpNEZ16x16"); return;
      case Iop_CmpNEZ8x32: vex_printf("CmpNEZ8x32"); return;

      case Iop_Add8x32:   vex_printf("Add8x32"); return;
      case Iop_Add16x16:  vex_printf("Add16x16"); return;
      case Iop_Add32x8:   vex_printf("Add32x8"); return;
      case Iop_Add64x4:   vex_printf("Add64x4"); return;
      case Iop_Sub8x32:   vex_printf("Sub8x32"); return;
      case Iop_Sub16x16:  vex_printf("Sub16x16"); return;
      case Iop_Sub32x8:   vex_printf("Sub32x8"); return;
      case Iop_Sub64x4:   vex_printf("Sub64x4"); return;
      case Iop_QAdd8Ux32: vex_printf("QAdd8Ux32"); return;
      case Iop_QAdd16Ux16: vex_printf("QAdd16Ux16"); return;
      case Iop_QAdd8Sx32: vex_printf("QAdd8Sx32"); return;
      case Iop_QAdd16Sx16: vex_printf("QAdd16Sx16"); return;
      case Iop_QSub8Ux32: vex_printf("QSub8Ux32"); return;
      case Iop_QSub16Ux16: vex_printf("QSub16Ux16"); return;
      case Iop_QSub8Sx32: vex_printf("QSub8Sx32"); return;
      case Iop_QSub16Sx16: vex_printf("QSub16Sx16"); return;

      case Iop_Mul16x16:    vex_printf("Mul16x16"); return;
      case Iop_Mul32x8:     vex_printf("Mul32x8"); return;
      case Iop_MulHi16Ux16: vex_printf("MulHi16Ux16"); return;
      case Iop_MulHi16Sx16: vex_printf("MulHi16Sx16"); return;

      case Iop_Avg8Ux32:  vex_printf("Avg8Ux32"); return;
      case Iop_Avg16Ux16: vex_printf("Avg16Ux16"); return;

      case Iop_Max8Sx32:  vex_printf("Max8Sx32"); return;
      case Iop_Max16Sx16: vex_printf("Max16Sx16"); return;
      case Iop_Max32Sx8:  vex_printf("Max32Sx8"); return;
      case Iop_Max8Ux32:  vex_printf("Max8Ux32"); return;
      case Iop_Max16Ux16: vex_printf("Max16Ux16"); return;
      case Iop_Max32Ux8:  vex_printf("Max32Ux8"); return;

      case Iop_Min8Sx32:  vex_printf("Min8Sx32"); return;
      case Iop_Min16Sx16: vex_printf("Min16Sx16"); return;
      case Iop_Min32Sx8:  vex_printf("Min32Sx8"); return;
      case Iop_Min8Ux32:  vex_printf("Min8Ux32"); return;
      case Iop_Min16Ux16: vex_printf("Min16Ux16"); return;
      case Iop_Min32Ux8:  vex_printf("Min32Ux8"); return;

      case Iop_CmpEQ8x32:   vex_printf("CmpEQ8x32"); return;
      case Iop_CmpEQ16x16:  vex_printf("CmpEQ16x16"); return;
      case Iop_CmpEQ32x8:   vex_printf("CmpEQ32x8"); return;
      case Iop_CmpEQ64x4:   vex_printf("CmpEQ64x4"); return;
      case Iop_CmpGT8Sx32:  vex_printf("CmpGT8Sx32"); return;
      case Iop_CmpGT16Sx16: vex_printf("CmpGT16Sx16"); return;
      case Iop_CmpGT32Sx8:  vex_printf("CmpGT32Sx8"); return;
      case Iop_CmpGT64Sx4:  vex_printf("CmpGT64Sx4"); return;

      case Iop_ShlN16x16:  vex_printf("ShlN16x16"); return;
      case Iop_ShlN32x8:   vex_printf("ShlN32x8"); return;
      case Iop_ShlN64x4:   vex_printf("ShlN64x4"); return;
      case Iop_ShrN16x16:  vex_printf("ShrN16x16"); return;
      case Iop_ShrN32x8:   vex_printf("ShrN32x8"); return;
      case Iop_ShrN64x4:   vex_printf("ShrN64x4"); return;
      case Iop_SarN16x16:  vex_printf("SarN16x16"); return;
      case Iop_SarN32x8:   vex_printf("SarN32x8"); return;

      case Iop_Perm32x8:   vex_printf("Perm32x8"); return;

      case Iop_CipherV128:   vex_printf("CipherV128"); return;
      case Iop_CipherLV128:  vex_printf("CipherLV128"); return;
      case Iop_NCipherV128:  vex_printf("NCipherV128"); return;
      case Iop_NCipherLV128: vex_printf("NCipherLV128"); return;
      case Iop_CipherSV128:  vex_printf("CipherSV128"); return;

      case Iop_SHA256:  vex_printf("SHA256"); return;
      case Iop_SHA512:  vex_printf("SHA512"); return;
      case Iop_BCDAdd:  vex_printf("BCDAdd"); return;
      case Iop_BCDSub:  vex_printf("BCDSub"); return;

      case Iop_PwBitMtxXpose64x2: vex_printf("BitMatrixTranspose64x2"); return;

      default: vpanic("ppIROp(1)");
   }

   vassert(str);  
   switch (op - base) {
      case 0: vex_printf("%s",str); vex_printf("8"); break;
      case 1: vex_printf("%s",str); vex_printf("16"); break;
      case 2: vex_printf("%s",str); vex_printf("32"); break;
      case 3: vex_printf("%s",str); vex_printf("64"); break;
      default: vpanic("ppIROp(2)");
   }
}

void ppIRExpr ( const IRExpr* e )
{
  Int i;
  switch (e->tag) {
    case Iex_Binder:
      vex_printf("BIND-%d", e->Iex.Binder.binder);
      break;
    case Iex_Get:
      vex_printf( "GET:" );
      ppIRType(e->Iex.Get.ty);
      vex_printf("(%d)", e->Iex.Get.offset);
      break;
    case Iex_GetI:
      vex_printf( "GETI" );
      ppIRRegArray(e->Iex.GetI.descr);
      vex_printf("[");
      ppIRExpr(e->Iex.GetI.ix);
      vex_printf(",%d]", e->Iex.GetI.bias);
      break;
    case Iex_RdTmp:
      ppIRTemp(e->Iex.RdTmp.tmp);
      break;
    case Iex_Qop: {
      const IRQop *qop = e->Iex.Qop.details;
      ppIROp(qop->op);
      vex_printf( "(" );
      ppIRExpr(qop->arg1);
      vex_printf( "," );
      ppIRExpr(qop->arg2);
      vex_printf( "," );
      ppIRExpr(qop->arg3);
      vex_printf( "," );
      ppIRExpr(qop->arg4);
      vex_printf( ")" );
      break;
    }
    case Iex_Triop: {
      const IRTriop *triop = e->Iex.Triop.details;
      ppIROp(triop->op);
      vex_printf( "(" );
      ppIRExpr(triop->arg1);
      vex_printf( "," );
      ppIRExpr(triop->arg2);
      vex_printf( "," );
      ppIRExpr(triop->arg3);
      vex_printf( ")" );
      break;
    }
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
    case Iex_Load:
      vex_printf( "LD%s:", e->Iex.Load.end==Iend_LE ? "le" : "be" );
      ppIRType(e->Iex.Load.ty);
      vex_printf( "(" );
      ppIRExpr(e->Iex.Load.addr);
      vex_printf( ")" );
      break;
    case Iex_Const:
      ppIRConst(e->Iex.Const.con);
      break;
    case Iex_CCall:
      ppIRCallee(e->Iex.CCall.cee);
      vex_printf("(");
      for (i = 0; e->Iex.CCall.args[i] != NULL; i++) {
        IRExpr* arg = e->Iex.CCall.args[i];
        ppIRExpr(arg);

        if (e->Iex.CCall.args[i+1] != NULL) {
          vex_printf(",");
        }
      }
      vex_printf("):");
      ppIRType(e->Iex.CCall.retty);
      break;
    case Iex_ITE:
      vex_printf("ITE(");
      ppIRExpr(e->Iex.ITE.cond);
      vex_printf(",");
      ppIRExpr(e->Iex.ITE.iftrue);
      vex_printf(",");
      ppIRExpr(e->Iex.ITE.iffalse);
      vex_printf(")");
      break;
    case Iex_VECRET:
      vex_printf("VECRET");
      break;
    case Iex_BBPTR:
      vex_printf("BBPTR");
      break;
    default:
      vpanic("ppIRExpr");
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

void ppIRDirty ( const IRDirty* d )
{
   Int i;
   if (d->tmp != IRTemp_INVALID) {
      ppIRTemp(d->tmp);
      vex_printf(" = ");
   }
   vex_printf("DIRTY ");
   ppIRExpr(d->guard);
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
      vex_printf("-gst(%u,%u", (UInt)d->fxState[i].offset,
                               (UInt)d->fxState[i].size);
      if (d->fxState[i].nRepeats > 0) {
         vex_printf(",reps%u,step%u", (UInt)d->fxState[i].nRepeats,
                                      (UInt)d->fxState[i].repeatLen);
      }
      vex_printf(")");
   }
   vex_printf(" ::: ");
   ppIRCallee(d->cee);
   vex_printf("(");
   for (i = 0; d->args[i] != NULL; i++) {
      IRExpr* arg = d->args[i];
      ppIRExpr(arg);

      if (d->args[i+1] != NULL) {
         vex_printf(",");
      }
   }
   vex_printf(")");
}

void ppIRCAS ( const IRCAS* cas )
{
   /* Print even structurally invalid constructions, as an aid to
      debugging. */
   if (cas->oldHi != IRTemp_INVALID) {
      ppIRTemp(cas->oldHi);
      vex_printf(",");
   }
   ppIRTemp(cas->oldLo);
   vex_printf(" = CAS%s(", cas->end==Iend_LE ? "le" : "be" );
   ppIRExpr(cas->addr);
   vex_printf("::");
   if (cas->expdHi) {
      ppIRExpr(cas->expdHi);
      vex_printf(",");
   }
   ppIRExpr(cas->expdLo);
   vex_printf("->");
   if (cas->dataHi) {
      ppIRExpr(cas->dataHi);
      vex_printf(",");
   }
   ppIRExpr(cas->dataLo);
   vex_printf(")");
}

void ppIRPutI ( const IRPutI* puti )
{
   vex_printf( "PUTI" );
   ppIRRegArray(puti->descr);
   vex_printf("[");
   ppIRExpr(puti->ix);
   vex_printf(",%d] = ", puti->bias);
   ppIRExpr(puti->data);
}

void ppIRStoreG ( const IRStoreG* sg )
{
   vex_printf("if (");
   ppIRExpr(sg->guard);
   vex_printf(") { ST%s(", sg->end==Iend_LE ? "le" : "be");
   ppIRExpr(sg->addr);
   vex_printf(") = ");
   ppIRExpr(sg->data);
   vex_printf(" }");
}

void ppIRLoadGOp ( IRLoadGOp cvt )
{
   switch (cvt) {
      case ILGop_INVALID: vex_printf("ILGop_INVALID"); break;      
      case ILGop_Ident64: vex_printf("Ident64"); break;      
      case ILGop_Ident32: vex_printf("Ident32"); break;      
      case ILGop_16Uto32: vex_printf("16Uto32"); break;      
      case ILGop_16Sto32: vex_printf("16Sto32"); break;      
      case ILGop_8Uto32:  vex_printf("8Uto32"); break;      
      case ILGop_8Sto32:  vex_printf("8Sto32"); break;      
      default: vpanic("ppIRLoadGOp");
   }
}

void ppIRLoadG ( const IRLoadG* lg )
{
   ppIRTemp(lg->dst);
   vex_printf(" = if-strict (");
   ppIRExpr(lg->guard);
   vex_printf(") ");
   ppIRLoadGOp(lg->cvt);
   vex_printf("(LD%s(", lg->end==Iend_LE ? "le" : "be");
   ppIRExpr(lg->addr);
   vex_printf(")) else ");
   ppIRExpr(lg->alt);
}

void ppIRJumpKind ( IRJumpKind kind )
{
   switch (kind) {
      case Ijk_Boring:        vex_printf("Boring"); break;
      case Ijk_Call:          vex_printf("Call"); break;
      case Ijk_Ret:           vex_printf("Return"); break;
      case Ijk_ClientReq:     vex_printf("ClientReq"); break;
      case Ijk_Yield:         vex_printf("Yield"); break;
      case Ijk_EmWarn:        vex_printf("EmWarn"); break;
      case Ijk_EmFail:        vex_printf("EmFail"); break;
      case Ijk_NoDecode:      vex_printf("NoDecode"); break;
      case Ijk_MapFail:       vex_printf("MapFail"); break;
      case Ijk_InvalICache:   vex_printf("InvalICache"); break;
      case Ijk_FlushDCache:   vex_printf("FlushDCache"); break;
      case Ijk_NoRedir:       vex_printf("NoRedir"); break;
      case Ijk_SigILL:        vex_printf("SigILL"); break;
      case Ijk_SigTRAP:       vex_printf("SigTRAP"); break;
      case Ijk_SigSEGV:       vex_printf("SigSEGV"); break;
      case Ijk_SigBUS:        vex_printf("SigBUS"); break;
      case Ijk_SigFPE_IntDiv: vex_printf("SigFPE_IntDiv"); break;
      case Ijk_SigFPE_IntOvf: vex_printf("SigFPE_IntOvf"); break;
      case Ijk_Sys_syscall:   vex_printf("Sys_syscall"); break;
      case Ijk_Sys_int32:     vex_printf("Sys_int32"); break;
      case Ijk_Sys_int128:    vex_printf("Sys_int128"); break;
      case Ijk_Sys_int129:    vex_printf("Sys_int129"); break;
      case Ijk_Sys_int130:    vex_printf("Sys_int130"); break;
      case Ijk_Sys_int145:    vex_printf("Sys_int145"); break;
      case Ijk_Sys_int210:    vex_printf("Sys_int210"); break;
      case Ijk_Sys_sysenter:  vex_printf("Sys_sysenter"); break;
      default:                vpanic("ppIRJumpKind");
   }
}

void ppIRMBusEvent ( IRMBusEvent event )
{
   switch (event) {
      case Imbe_Fence:
         vex_printf("Fence"); break;
      case Imbe_CancelReservation:
         vex_printf("CancelReservation"); break;
      default:
         vpanic("ppIRMBusEvent");
   }
}

void ppIRStmt ( const IRStmt* s )
{
   if (!s) {
      vex_printf("!!! IRStmt* which is NULL !!!");
      return;
   }
   switch (s->tag) {
      case Ist_NoOp:
         vex_printf("IR-NoOp");
         break;
      case Ist_IMark:
         vex_printf( "------ IMark(0x%lx, %u, %u) ------", 
                     s->Ist.IMark.addr, s->Ist.IMark.len,
                     (UInt)s->Ist.IMark.delta);
         break;
      case Ist_AbiHint:
         vex_printf("====== AbiHint(");
         ppIRExpr(s->Ist.AbiHint.base);
         vex_printf(", %d, ", s->Ist.AbiHint.len);
         ppIRExpr(s->Ist.AbiHint.nia);
         vex_printf(") ======");
         break;
      case Ist_Put:
         vex_printf( "PUT(%d) = ", s->Ist.Put.offset);
         ppIRExpr(s->Ist.Put.data);
         break;
      case Ist_PutI:
         ppIRPutI(s->Ist.PutI.details);
         break;
      case Ist_WrTmp:
         ppIRTemp(s->Ist.WrTmp.tmp);
         vex_printf( " = " );
         ppIRExpr(s->Ist.WrTmp.data);
         break;
      case Ist_Store:
         vex_printf( "ST%s(", s->Ist.Store.end==Iend_LE ? "le" : "be" );
         ppIRExpr(s->Ist.Store.addr);
         vex_printf( ") = ");
         ppIRExpr(s->Ist.Store.data);
         break;
      case Ist_StoreG:
         ppIRStoreG(s->Ist.StoreG.details);
         break;
      case Ist_LoadG:
         ppIRLoadG(s->Ist.LoadG.details);
         break;
      case Ist_CAS:
         ppIRCAS(s->Ist.CAS.details);
         break;
      case Ist_LLSC:
         if (s->Ist.LLSC.storedata == NULL) {
            ppIRTemp(s->Ist.LLSC.result);
            vex_printf(" = LD%s-Linked(",
                       s->Ist.LLSC.end==Iend_LE ? "le" : "be");
            ppIRExpr(s->Ist.LLSC.addr);
            vex_printf(")");
         } else {
            ppIRTemp(s->Ist.LLSC.result);
            vex_printf(" = ( ST%s-Cond(",
                       s->Ist.LLSC.end==Iend_LE ? "le" : "be");
            ppIRExpr(s->Ist.LLSC.addr);
            vex_printf(") = ");
            ppIRExpr(s->Ist.LLSC.storedata);
            vex_printf(" )");
         }
         break;
      case Ist_Dirty:
         ppIRDirty(s->Ist.Dirty.details);
         break;
      case Ist_MBE:
         vex_printf("IR-");
         ppIRMBusEvent(s->Ist.MBE.event);
         break;
      case Ist_Exit:
         vex_printf( "if (" );
         ppIRExpr(s->Ist.Exit.guard);
         vex_printf( ") { PUT(%d) = ", s->Ist.Exit.offsIP);
         ppIRConst(s->Ist.Exit.dst);
         vex_printf("; exit-");
         ppIRJumpKind(s->Ist.Exit.jk);
         vex_printf(" } ");
         break;
      default: 
         vpanic("ppIRStmt");
   }
}

void ppIRTypeEnv ( const IRTypeEnv* env )
{
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

void ppIRSB ( const IRSB* bb )
{
   Int i;
   vex_printf("IRSB {\n");
   ppIRTypeEnv(bb->tyenv);
   vex_printf("\n");
   for (i = 0; i < bb->stmts_used; i++) {
      vex_printf( "   ");
      ppIRStmt(bb->stmts[i]);
      vex_printf( "\n");
   }
   vex_printf( "   PUT(%d) = ", bb->offsIP );
   ppIRExpr( bb->next );
   vex_printf( "; exit-");
   ppIRJumpKind(bb->jumpkind);
   vex_printf( "\n}\n");
}


/*---------------------------------------------------------------*/
/*--- Constructors                                            ---*/
/*---------------------------------------------------------------*/


/* Constructors -- IRConst */

IRConst* IRConst_U1 ( Bool bit )
{
   IRConst* c = LibVEX_Alloc_inline(sizeof(IRConst));
   c->tag     = Ico_U1;
   c->Ico.U1  = bit;
   /* call me paranoid; I don't care :-) */
   vassert(bit == False || bit == True);
   return c;
}
IRConst* IRConst_U8 ( UChar u8 )
{
   IRConst* c = LibVEX_Alloc_inline(sizeof(IRConst));
   c->tag     = Ico_U8;
   c->Ico.U8  = u8;
   return c;
}
IRConst* IRConst_U16 ( UShort u16 )
{
   IRConst* c = LibVEX_Alloc_inline(sizeof(IRConst));
   c->tag     = Ico_U16;
   c->Ico.U16 = u16;
   return c;
}
IRConst* IRConst_U32 ( UInt u32 )
{
   IRConst* c = LibVEX_Alloc_inline(sizeof(IRConst));
   c->tag     = Ico_U32;
   c->Ico.U32 = u32;
   return c;
}
IRConst* IRConst_U64 ( ULong u64 )
{
   IRConst* c = LibVEX_Alloc_inline(sizeof(IRConst));
   c->tag     = Ico_U64;
   c->Ico.U64 = u64;
   return c;
}
IRConst* IRConst_F32 ( Float f32 )
{
   IRConst* c = LibVEX_Alloc_inline(sizeof(IRConst));
   c->tag     = Ico_F32;
   c->Ico.F32 = f32;
   return c;
}
IRConst* IRConst_F32i ( UInt f32i )
{
   IRConst* c  = LibVEX_Alloc_inline(sizeof(IRConst));
   c->tag      = Ico_F32i;
   c->Ico.F32i = f32i;
   return c;
}
IRConst* IRConst_F64 ( Double f64 )
{
   IRConst* c = LibVEX_Alloc_inline(sizeof(IRConst));
   c->tag     = Ico_F64;
   c->Ico.F64 = f64;
   return c;
}
IRConst* IRConst_F64i ( ULong f64i )
{
   IRConst* c  = LibVEX_Alloc_inline(sizeof(IRConst));
   c->tag      = Ico_F64i;
   c->Ico.F64i = f64i;
   return c;
}
IRConst* IRConst_V128 ( UShort con )
{
   IRConst* c  = LibVEX_Alloc_inline(sizeof(IRConst));
   c->tag      = Ico_V128;
   c->Ico.V128 = con;
   return c;
}
IRConst* IRConst_V256 ( UInt con )
{
   IRConst* c  = LibVEX_Alloc_inline(sizeof(IRConst));
   c->tag      = Ico_V256;
   c->Ico.V256 = con;
   return c;
}

/* Constructors -- IRCallee */

IRCallee* mkIRCallee ( Int regparms, const HChar* name, void* addr )
{
   IRCallee* ce = LibVEX_Alloc_inline(sizeof(IRCallee));
   ce->regparms = regparms;
   ce->name     = name;
   ce->addr     = addr;
   ce->mcx_mask = 0;
   vassert(regparms >= 0 && regparms <= 3);
   vassert(name != NULL);
   vassert(addr != 0);
   return ce;
}


/* Constructors -- IRRegArray */

IRRegArray* mkIRRegArray ( Int base, IRType elemTy, Int nElems )
{
   IRRegArray* arr = LibVEX_Alloc_inline(sizeof(IRRegArray));
   arr->base       = base;
   arr->elemTy     = elemTy;
   arr->nElems     = nElems;
   vassert(!(arr->base < 0 || arr->base > 10000 /* somewhat arbitrary */));
   vassert(!(arr->elemTy == Ity_I1));
   vassert(!(arr->nElems <= 0 || arr->nElems > 500 /* somewhat arbitrary */));
   return arr;
}


/* Constructors -- IRExpr */

IRExpr* IRExpr_Binder ( Int binder ) {
   IRExpr* e            = LibVEX_Alloc_inline(sizeof(IRExpr));
   e->tag               = Iex_Binder;
   e->Iex.Binder.binder = binder;
   return e;
}
IRExpr* IRExpr_Get ( Int off, IRType ty ) {
   IRExpr* e         = LibVEX_Alloc_inline(sizeof(IRExpr));
   e->tag            = Iex_Get;
   e->Iex.Get.offset = off;
   e->Iex.Get.ty     = ty;
   return e;
}
IRExpr* IRExpr_GetI ( IRRegArray* descr, IRExpr* ix, Int bias ) {
   IRExpr* e         = LibVEX_Alloc_inline(sizeof(IRExpr));
   e->tag            = Iex_GetI;
   e->Iex.GetI.descr = descr;
   e->Iex.GetI.ix    = ix;
   e->Iex.GetI.bias  = bias;
   return e;
}
IRExpr* IRExpr_RdTmp ( IRTemp tmp ) {
   IRExpr* e        = LibVEX_Alloc_inline(sizeof(IRExpr));
   e->tag           = Iex_RdTmp;
   e->Iex.RdTmp.tmp = tmp;
   return e;
}
IRExpr* IRExpr_Qop ( IROp op, IRExpr* arg1, IRExpr* arg2, 
                              IRExpr* arg3, IRExpr* arg4 ) {
   IRExpr* e       = LibVEX_Alloc_inline(sizeof(IRExpr));
   IRQop*  qop     = LibVEX_Alloc_inline(sizeof(IRQop));
   qop->op         = op;
   qop->arg1       = arg1;
   qop->arg2       = arg2;
   qop->arg3       = arg3;
   qop->arg4       = arg4;
   e->tag          = Iex_Qop;
   e->Iex.Qop.details = qop;
   return e;
}
IRExpr* IRExpr_Triop  ( IROp op, IRExpr* arg1, 
                                 IRExpr* arg2, IRExpr* arg3 ) {
   IRExpr*  e         = LibVEX_Alloc_inline(sizeof(IRExpr));
   IRTriop* triop     = LibVEX_Alloc_inline(sizeof(IRTriop));
   triop->op         = op;
   triop->arg1       = arg1;
   triop->arg2       = arg2;
   triop->arg3       = arg3;
   e->tag            = Iex_Triop;
   e->Iex.Triop.details = triop;
   return e;
}
IRExpr* IRExpr_Binop ( IROp op, IRExpr* arg1, IRExpr* arg2 ) {
   IRExpr* e         = LibVEX_Alloc_inline(sizeof(IRExpr));
   e->tag            = Iex_Binop;
   e->Iex.Binop.op   = op;
   e->Iex.Binop.arg1 = arg1;
   e->Iex.Binop.arg2 = arg2;
   return e;
}
IRExpr* IRExpr_Unop ( IROp op, IRExpr* arg ) {
   IRExpr* e       = LibVEX_Alloc_inline(sizeof(IRExpr));
   e->tag          = Iex_Unop;
   e->Iex.Unop.op  = op;
   e->Iex.Unop.arg = arg;
   return e;
}
IRExpr* IRExpr_Load ( IREndness end, IRType ty, IRExpr* addr ) {
   IRExpr* e        = LibVEX_Alloc_inline(sizeof(IRExpr));
   e->tag           = Iex_Load;
   e->Iex.Load.end  = end;
   e->Iex.Load.ty   = ty;
   e->Iex.Load.addr = addr;
   vassert(end == Iend_LE || end == Iend_BE);
   return e;
}
IRExpr* IRExpr_Const ( IRConst* con ) {
   IRExpr* e        = LibVEX_Alloc_inline(sizeof(IRExpr));
   e->tag           = Iex_Const;
   e->Iex.Const.con = con;
   return e;
}
IRExpr* IRExpr_CCall ( IRCallee* cee, IRType retty, IRExpr** args ) {
   IRExpr* e          = LibVEX_Alloc_inline(sizeof(IRExpr));
   e->tag             = Iex_CCall;
   e->Iex.CCall.cee   = cee;
   e->Iex.CCall.retty = retty;
   e->Iex.CCall.args  = args;
   return e;
}
IRExpr* IRExpr_ITE ( IRExpr* cond, IRExpr* iftrue, IRExpr* iffalse ) {
   IRExpr* e          = LibVEX_Alloc_inline(sizeof(IRExpr));
   e->tag             = Iex_ITE;
   e->Iex.ITE.cond    = cond;
   e->Iex.ITE.iftrue  = iftrue;
   e->Iex.ITE.iffalse = iffalse;
   return e;
}
IRExpr* IRExpr_VECRET ( void ) {
   IRExpr* e = LibVEX_Alloc_inline(sizeof(IRExpr));
   e->tag    = Iex_VECRET;
   return e;
}
IRExpr* IRExpr_BBPTR ( void ) {
   IRExpr* e = LibVEX_Alloc_inline(sizeof(IRExpr));
   e->tag    = Iex_BBPTR;
   return e;
}


/* Constructors for NULL-terminated IRExpr expression vectors,
   suitable for use as arg lists in clean/dirty helper calls. */

IRExpr** mkIRExprVec_0 ( void ) {
   IRExpr** vec = LibVEX_Alloc_inline(1 * sizeof(IRExpr*));
   vec[0] = NULL;
   return vec;
}
IRExpr** mkIRExprVec_1 ( IRExpr* arg1 ) {
   IRExpr** vec = LibVEX_Alloc_inline(2 * sizeof(IRExpr*));
   vec[0] = arg1;
   vec[1] = NULL;
   return vec;
}
IRExpr** mkIRExprVec_2 ( IRExpr* arg1, IRExpr* arg2 ) {
   IRExpr** vec = LibVEX_Alloc_inline(3 * sizeof(IRExpr*));
   vec[0] = arg1;
   vec[1] = arg2;
   vec[2] = NULL;
   return vec;
}
IRExpr** mkIRExprVec_3 ( IRExpr* arg1, IRExpr* arg2, IRExpr* arg3 ) {
   IRExpr** vec = LibVEX_Alloc_inline(4 * sizeof(IRExpr*));
   vec[0] = arg1;
   vec[1] = arg2;
   vec[2] = arg3;
   vec[3] = NULL;
   return vec;
}
IRExpr** mkIRExprVec_4 ( IRExpr* arg1, IRExpr* arg2, IRExpr* arg3,
                         IRExpr* arg4 ) {
   IRExpr** vec = LibVEX_Alloc_inline(5 * sizeof(IRExpr*));
   vec[0] = arg1;
   vec[1] = arg2;
   vec[2] = arg3;
   vec[3] = arg4;
   vec[4] = NULL;
   return vec;
}
IRExpr** mkIRExprVec_5 ( IRExpr* arg1, IRExpr* arg2, IRExpr* arg3,
                         IRExpr* arg4, IRExpr* arg5 ) {
   IRExpr** vec = LibVEX_Alloc_inline(6 * sizeof(IRExpr*));
   vec[0] = arg1;
   vec[1] = arg2;
   vec[2] = arg3;
   vec[3] = arg4;
   vec[4] = arg5;
   vec[5] = NULL;
   return vec;
}
IRExpr** mkIRExprVec_6 ( IRExpr* arg1, IRExpr* arg2, IRExpr* arg3,
                         IRExpr* arg4, IRExpr* arg5, IRExpr* arg6 ) {
   IRExpr** vec = LibVEX_Alloc_inline(7 * sizeof(IRExpr*));
   vec[0] = arg1;
   vec[1] = arg2;
   vec[2] = arg3;
   vec[3] = arg4;
   vec[4] = arg5;
   vec[5] = arg6;
   vec[6] = NULL;
   return vec;
}
IRExpr** mkIRExprVec_7 ( IRExpr* arg1, IRExpr* arg2, IRExpr* arg3,
                         IRExpr* arg4, IRExpr* arg5, IRExpr* arg6,
                         IRExpr* arg7 ) {
   IRExpr** vec = LibVEX_Alloc_inline(8 * sizeof(IRExpr*));
   vec[0] = arg1;
   vec[1] = arg2;
   vec[2] = arg3;
   vec[3] = arg4;
   vec[4] = arg5;
   vec[5] = arg6;
   vec[6] = arg7;
   vec[7] = NULL;
   return vec;
}
IRExpr** mkIRExprVec_8 ( IRExpr* arg1, IRExpr* arg2, IRExpr* arg3,
                         IRExpr* arg4, IRExpr* arg5, IRExpr* arg6,
                         IRExpr* arg7, IRExpr* arg8 ) {
   IRExpr** vec = LibVEX_Alloc_inline(9 * sizeof(IRExpr*));
   vec[0] = arg1;
   vec[1] = arg2;
   vec[2] = arg3;
   vec[3] = arg4;
   vec[4] = arg5;
   vec[5] = arg6;
   vec[6] = arg7;
   vec[7] = arg8;
   vec[8] = NULL;
   return vec;
}


/* Constructors -- IRDirty */

IRDirty* emptyIRDirty ( void ) {
   IRDirty* d = LibVEX_Alloc_inline(sizeof(IRDirty));
   d->cee      = NULL;
   d->guard    = NULL;
   d->args     = NULL;
   d->tmp      = IRTemp_INVALID;
   d->mFx      = Ifx_None;
   d->mAddr    = NULL;
   d->mSize    = 0;
   d->nFxState = 0;
   return d;
}


/* Constructors -- IRCAS */

IRCAS* mkIRCAS ( IRTemp oldHi, IRTemp oldLo,
                 IREndness end, IRExpr* addr, 
                 IRExpr* expdHi, IRExpr* expdLo,
                 IRExpr* dataHi, IRExpr* dataLo ) {
   IRCAS* cas = LibVEX_Alloc_inline(sizeof(IRCAS));
   cas->oldHi  = oldHi;
   cas->oldLo  = oldLo;
   cas->end    = end;
   cas->addr   = addr;
   cas->expdHi = expdHi;
   cas->expdLo = expdLo;
   cas->dataHi = dataHi;
   cas->dataLo = dataLo;
   return cas;
}


/* Constructors -- IRPutI */

IRPutI* mkIRPutI ( IRRegArray* descr, IRExpr* ix,
                   Int bias, IRExpr* data )
{
   IRPutI* puti = LibVEX_Alloc_inline(sizeof(IRPutI));
   puti->descr  = descr;
   puti->ix     = ix;
   puti->bias   = bias;
   puti->data   = data;
   return puti;
}


/* Constructors -- IRStoreG and IRLoadG */

IRStoreG* mkIRStoreG ( IREndness end,
                       IRExpr* addr, IRExpr* data, IRExpr* guard )
{
   IRStoreG* sg = LibVEX_Alloc_inline(sizeof(IRStoreG));
   sg->end      = end;
   sg->addr     = addr;
   sg->data     = data;
   sg->guard    = guard;
   return sg;
}

IRLoadG* mkIRLoadG ( IREndness end, IRLoadGOp cvt,
                     IRTemp dst, IRExpr* addr, IRExpr* alt, IRExpr* guard )
{
   IRLoadG* lg = LibVEX_Alloc_inline(sizeof(IRLoadG));
   lg->end     = end;
   lg->cvt     = cvt;
   lg->dst     = dst;
   lg->addr    = addr;
   lg->alt     = alt;
   lg->guard   = guard;
   return lg;
}


/* Constructors -- IRStmt */

IRStmt* IRStmt_NoOp ( void )
{
   /* Just use a single static closure. */
   static IRStmt static_closure;
   static_closure.tag = Ist_NoOp;
   return &static_closure;
}
IRStmt* IRStmt_IMark ( Addr addr, UInt len, UChar delta ) {
   IRStmt* s          = LibVEX_Alloc_inline(sizeof(IRStmt));
   s->tag             = Ist_IMark;
   s->Ist.IMark.addr  = addr;
   s->Ist.IMark.len   = len;
   s->Ist.IMark.delta = delta;
   return s;
}
IRStmt* IRStmt_AbiHint ( IRExpr* base, Int len, IRExpr* nia ) {
   IRStmt* s           = LibVEX_Alloc_inline(sizeof(IRStmt));
   s->tag              = Ist_AbiHint;
   s->Ist.AbiHint.base = base;
   s->Ist.AbiHint.len  = len;
   s->Ist.AbiHint.nia  = nia;
   return s;
}
IRStmt* IRStmt_Put ( Int off, IRExpr* data ) {
   IRStmt* s         = LibVEX_Alloc_inline(sizeof(IRStmt));
   s->tag            = Ist_Put;
   s->Ist.Put.offset = off;
   s->Ist.Put.data   = data;
   return s;
}
IRStmt* IRStmt_PutI ( IRPutI* details ) {
   IRStmt* s          = LibVEX_Alloc_inline(sizeof(IRStmt));
   s->tag             = Ist_PutI;
   s->Ist.PutI.details = details;
   return s;
}
IRStmt* IRStmt_WrTmp ( IRTemp tmp, IRExpr* data ) {
   IRStmt* s         = LibVEX_Alloc_inline(sizeof(IRStmt));
   s->tag            = Ist_WrTmp;
   s->Ist.WrTmp.tmp  = tmp;
   s->Ist.WrTmp.data = data;
   return s;
}
IRStmt* IRStmt_Store ( IREndness end, IRExpr* addr, IRExpr* data ) {
   IRStmt* s         = LibVEX_Alloc_inline(sizeof(IRStmt));
   s->tag            = Ist_Store;
   s->Ist.Store.end  = end;
   s->Ist.Store.addr = addr;
   s->Ist.Store.data = data;
   vassert(end == Iend_LE || end == Iend_BE);
   return s;
}
IRStmt* IRStmt_StoreG ( IREndness end, IRExpr* addr, IRExpr* data,
                        IRExpr* guard ) {
   IRStmt* s             = LibVEX_Alloc_inline(sizeof(IRStmt));
   s->tag                = Ist_StoreG;
   s->Ist.StoreG.details = mkIRStoreG(end, addr, data, guard);
   vassert(end == Iend_LE || end == Iend_BE);
   return s;
}
IRStmt* IRStmt_LoadG ( IREndness end, IRLoadGOp cvt, IRTemp dst,
                       IRExpr* addr, IRExpr* alt, IRExpr* guard ) {
   IRStmt* s            = LibVEX_Alloc_inline(sizeof(IRStmt));
   s->tag               = Ist_LoadG;
   s->Ist.LoadG.details = mkIRLoadG(end, cvt, dst, addr, alt, guard);
   return s;
}
IRStmt* IRStmt_CAS ( IRCAS* cas ) {
   IRStmt* s          = LibVEX_Alloc_inline(sizeof(IRStmt));
   s->tag             = Ist_CAS;
   s->Ist.CAS.details = cas;
   return s;
}
IRStmt* IRStmt_LLSC ( IREndness end,
                      IRTemp result, IRExpr* addr, IRExpr* storedata ) {
   IRStmt* s = LibVEX_Alloc_inline(sizeof(IRStmt));
   s->tag                = Ist_LLSC;
   s->Ist.LLSC.end       = end;
   s->Ist.LLSC.result    = result;
   s->Ist.LLSC.addr      = addr;
   s->Ist.LLSC.storedata = storedata;
   return s;
}
IRStmt* IRStmt_Dirty ( IRDirty* d )
{
   IRStmt* s            = LibVEX_Alloc_inline(sizeof(IRStmt));
   s->tag               = Ist_Dirty;
   s->Ist.Dirty.details = d;
   return s;
}
IRStmt* IRStmt_MBE ( IRMBusEvent event )
{
   IRStmt* s        = LibVEX_Alloc_inline(sizeof(IRStmt));
   s->tag           = Ist_MBE;
   s->Ist.MBE.event = event;
   return s;
}
IRStmt* IRStmt_Exit ( IRExpr* guard, IRJumpKind jk, IRConst* dst,
                      Int offsIP ) {
   IRStmt* s          = LibVEX_Alloc_inline(sizeof(IRStmt));
   s->tag             = Ist_Exit;
   s->Ist.Exit.guard  = guard;
   s->Ist.Exit.jk     = jk;
   s->Ist.Exit.dst    = dst;
   s->Ist.Exit.offsIP = offsIP;
   return s;
}


/* Constructors -- IRTypeEnv */

IRTypeEnv* emptyIRTypeEnv ( void )
{
   IRTypeEnv* env   = LibVEX_Alloc_inline(sizeof(IRTypeEnv));
   env->types       = LibVEX_Alloc_inline(8 * sizeof(IRType));
   env->types_size  = 8;
   env->types_used  = 0;
   return env;
}


/* Constructors -- IRSB */

IRSB* emptyIRSB ( void )
{
   IRSB* bb       = LibVEX_Alloc_inline(sizeof(IRSB));
   bb->tyenv      = emptyIRTypeEnv();
   bb->stmts_used = 0;
   bb->stmts_size = 8;
   bb->stmts      = LibVEX_Alloc_inline(bb->stmts_size * sizeof(IRStmt*));
   bb->next       = NULL;
   bb->jumpkind   = Ijk_Boring;
   bb->offsIP     = 0;
   return bb;
}


/*---------------------------------------------------------------*/
/*--- (Deep) copy constructors.  These make complete copies   ---*/
/*--- the original, which can be modified without affecting   ---*/
/*--- the original.                                           ---*/
/*---------------------------------------------------------------*/

/* Copying IR Expr vectors (for call args). */

/* Shallow copy of an IRExpr vector */

IRExpr** shallowCopyIRExprVec ( IRExpr** vec )
{
   Int      i;
   IRExpr** newvec;
   for (i = 0; vec[i]; i++)
      ;
   newvec = LibVEX_Alloc_inline((i+1)*sizeof(IRExpr*));
   for (i = 0; vec[i]; i++)
      newvec[i] = vec[i];
   newvec[i] = NULL;
   return newvec;
}

/* Deep copy of an IRExpr vector */

IRExpr** deepCopyIRExprVec ( IRExpr *const * vec )
{
   Int      i;
   IRExpr** newvec;
   for (i = 0; vec[i]; i++)
      ;
   newvec = LibVEX_Alloc_inline((i+1)*sizeof(IRExpr*));
   for (i = 0; vec[i]; i++)
      newvec[i] = deepCopyIRExpr(vec[i]);
   newvec[i] = NULL;
   return newvec;
}

/* Deep copy constructors for all heap-allocated IR types follow. */

IRConst* deepCopyIRConst ( const IRConst* c )
{
   switch (c->tag) {
      case Ico_U1:   return IRConst_U1(c->Ico.U1);
      case Ico_U8:   return IRConst_U8(c->Ico.U8);
      case Ico_U16:  return IRConst_U16(c->Ico.U16);
      case Ico_U32:  return IRConst_U32(c->Ico.U32);
      case Ico_U64:  return IRConst_U64(c->Ico.U64);
      case Ico_F32:  return IRConst_F32(c->Ico.F32);
      case Ico_F32i: return IRConst_F32i(c->Ico.F32i);
      case Ico_F64:  return IRConst_F64(c->Ico.F64);
      case Ico_F64i: return IRConst_F64i(c->Ico.F64i);
      case Ico_V128: return IRConst_V128(c->Ico.V128);
      case Ico_V256: return IRConst_V256(c->Ico.V256);
      default: vpanic("deepCopyIRConst");
   }
}

IRCallee* deepCopyIRCallee ( const IRCallee* ce )
{
   IRCallee* ce2 = mkIRCallee(ce->regparms, ce->name, ce->addr);
   ce2->mcx_mask = ce->mcx_mask;
   return ce2;
}

IRRegArray* deepCopyIRRegArray ( const IRRegArray* d )
{
   return mkIRRegArray(d->base, d->elemTy, d->nElems);
}

IRExpr* deepCopyIRExpr ( const IRExpr* e )
{
   switch (e->tag) {
      case Iex_Get: 
         return IRExpr_Get(e->Iex.Get.offset, e->Iex.Get.ty);
      case Iex_GetI: 
         return IRExpr_GetI(deepCopyIRRegArray(e->Iex.GetI.descr), 
                            deepCopyIRExpr(e->Iex.GetI.ix),
                            e->Iex.GetI.bias);
      case Iex_RdTmp: 
         return IRExpr_RdTmp(e->Iex.RdTmp.tmp);
      case Iex_Qop: {
         const IRQop* qop = e->Iex.Qop.details;

         return IRExpr_Qop(qop->op,
                           deepCopyIRExpr(qop->arg1),
                           deepCopyIRExpr(qop->arg2),
                           deepCopyIRExpr(qop->arg3),
                           deepCopyIRExpr(qop->arg4));
      }
      case Iex_Triop:  {
         const IRTriop *triop = e->Iex.Triop.details;

         return IRExpr_Triop(triop->op,
                             deepCopyIRExpr(triop->arg1),
                             deepCopyIRExpr(triop->arg2),
                             deepCopyIRExpr(triop->arg3));
      }
      case Iex_Binop: 
         return IRExpr_Binop(e->Iex.Binop.op,
                             deepCopyIRExpr(e->Iex.Binop.arg1),
                             deepCopyIRExpr(e->Iex.Binop.arg2));
      case Iex_Unop: 
         return IRExpr_Unop(e->Iex.Unop.op,
                            deepCopyIRExpr(e->Iex.Unop.arg));
      case Iex_Load: 
         return IRExpr_Load(e->Iex.Load.end,
                            e->Iex.Load.ty,
                            deepCopyIRExpr(e->Iex.Load.addr));
      case Iex_Const: 
         return IRExpr_Const(deepCopyIRConst(e->Iex.Const.con));
      case Iex_CCall:
         return IRExpr_CCall(deepCopyIRCallee(e->Iex.CCall.cee),
                             e->Iex.CCall.retty,
                             deepCopyIRExprVec(e->Iex.CCall.args));

      case Iex_ITE: 
         return IRExpr_ITE(deepCopyIRExpr(e->Iex.ITE.cond),
                           deepCopyIRExpr(e->Iex.ITE.iftrue),
                           deepCopyIRExpr(e->Iex.ITE.iffalse));
      case Iex_VECRET:
         return IRExpr_VECRET();

      case Iex_BBPTR:
         return IRExpr_BBPTR();

      case Iex_Binder:
         return IRExpr_Binder(e->Iex.Binder.binder);

      default:
         vpanic("deepCopyIRExpr");
   }
}

IRDirty* deepCopyIRDirty ( const IRDirty* d )
{
   Int      i;
   IRDirty* d2 = emptyIRDirty();
   d2->cee   = deepCopyIRCallee(d->cee);
   d2->guard = deepCopyIRExpr(d->guard);
   d2->args  = deepCopyIRExprVec(d->args);
   d2->tmp   = d->tmp;
   d2->mFx   = d->mFx;
   d2->mAddr = d->mAddr==NULL ? NULL : deepCopyIRExpr(d->mAddr);
   d2->mSize = d->mSize;
   d2->nFxState = d->nFxState;
   for (i = 0; i < d2->nFxState; i++)
      d2->fxState[i] = d->fxState[i];
   return d2;
}

IRCAS* deepCopyIRCAS ( const IRCAS* cas )
{
   return mkIRCAS( cas->oldHi, cas->oldLo, cas->end,
                   deepCopyIRExpr(cas->addr),
                   cas->expdHi==NULL ? NULL : deepCopyIRExpr(cas->expdHi),
                   deepCopyIRExpr(cas->expdLo),
                   cas->dataHi==NULL ? NULL : deepCopyIRExpr(cas->dataHi),
                   deepCopyIRExpr(cas->dataLo) );
}

IRPutI* deepCopyIRPutI ( const IRPutI * puti )
{
  return mkIRPutI( deepCopyIRRegArray(puti->descr),
                   deepCopyIRExpr(puti->ix),
                   puti->bias, 
                   deepCopyIRExpr(puti->data));
}

IRStmt* deepCopyIRStmt ( const IRStmt* s )
{
   switch (s->tag) {
      case Ist_NoOp:
         return IRStmt_NoOp();
      case Ist_AbiHint:
         return IRStmt_AbiHint(deepCopyIRExpr(s->Ist.AbiHint.base),
                               s->Ist.AbiHint.len,
                               deepCopyIRExpr(s->Ist.AbiHint.nia));
      case Ist_IMark:
         return IRStmt_IMark(s->Ist.IMark.addr,
                             s->Ist.IMark.len,
                             s->Ist.IMark.delta);
      case Ist_Put: 
         return IRStmt_Put(s->Ist.Put.offset, 
                           deepCopyIRExpr(s->Ist.Put.data));
      case Ist_PutI: 
         return IRStmt_PutI(deepCopyIRPutI(s->Ist.PutI.details));
      case Ist_WrTmp:
         return IRStmt_WrTmp(s->Ist.WrTmp.tmp,
                             deepCopyIRExpr(s->Ist.WrTmp.data));
      case Ist_Store: 
         return IRStmt_Store(s->Ist.Store.end,
                             deepCopyIRExpr(s->Ist.Store.addr),
                             deepCopyIRExpr(s->Ist.Store.data));
      case Ist_StoreG: {
         const IRStoreG* sg = s->Ist.StoreG.details;
         return IRStmt_StoreG(sg->end,
                              deepCopyIRExpr(sg->addr),
                              deepCopyIRExpr(sg->data),
                              deepCopyIRExpr(sg->guard));
      }
      case Ist_LoadG: {
         const IRLoadG* lg = s->Ist.LoadG.details;
         return IRStmt_LoadG(lg->end, lg->cvt, lg->dst,
                             deepCopyIRExpr(lg->addr),
                             deepCopyIRExpr(lg->alt),
                             deepCopyIRExpr(lg->guard));
      }
      case Ist_CAS:
         return IRStmt_CAS(deepCopyIRCAS(s->Ist.CAS.details));
      case Ist_LLSC:
         return IRStmt_LLSC(s->Ist.LLSC.end,
                            s->Ist.LLSC.result,
                            deepCopyIRExpr(s->Ist.LLSC.addr),
                            s->Ist.LLSC.storedata
                               ? deepCopyIRExpr(s->Ist.LLSC.storedata)
                               : NULL);
      case Ist_Dirty: 
         return IRStmt_Dirty(deepCopyIRDirty(s->Ist.Dirty.details));
      case Ist_MBE:
         return IRStmt_MBE(s->Ist.MBE.event);
      case Ist_Exit: 
         return IRStmt_Exit(deepCopyIRExpr(s->Ist.Exit.guard),
                            s->Ist.Exit.jk,
                            deepCopyIRConst(s->Ist.Exit.dst),
                            s->Ist.Exit.offsIP);
      default: 
         vpanic("deepCopyIRStmt");
   }
}

IRTypeEnv* deepCopyIRTypeEnv ( const IRTypeEnv* src )
{
   Int        i;
   IRTypeEnv* dst = LibVEX_Alloc_inline(sizeof(IRTypeEnv));
   dst->types_size = src->types_size;
   dst->types_used = src->types_used;
   dst->types = LibVEX_Alloc_inline(dst->types_size * sizeof(IRType));
   for (i = 0; i < src->types_used; i++)
      dst->types[i] = src->types[i];
   return dst;
}

IRSB* deepCopyIRSB ( const IRSB* bb )
{
   Int      i;
   IRStmt** sts2;
   IRSB* bb2 = deepCopyIRSBExceptStmts(bb);
   bb2->stmts_used = bb2->stmts_size = bb->stmts_used;
   sts2 = LibVEX_Alloc_inline(bb2->stmts_used * sizeof(IRStmt*));
   for (i = 0; i < bb2->stmts_used; i++)
      sts2[i] = deepCopyIRStmt(bb->stmts[i]);
   bb2->stmts = sts2;
   return bb2;
}

IRSB* deepCopyIRSBExceptStmts ( const IRSB* bb )
{
   IRSB* bb2     = emptyIRSB();
   bb2->tyenv    = deepCopyIRTypeEnv(bb->tyenv);
   bb2->next     = deepCopyIRExpr(bb->next);
   bb2->jumpkind = bb->jumpkind;
   bb2->offsIP   = bb->offsIP;
   return bb2;
}


/*---------------------------------------------------------------*/
/*--- Primop types                                            ---*/
/*---------------------------------------------------------------*/

static
void typeOfPrimop ( IROp op, 
                    /*OUTs*/
                    IRType* t_dst, 
                    IRType* t_arg1, IRType* t_arg2, 
                    IRType* t_arg3, IRType* t_arg4 )
{
#  define UNARY(_ta1,_td)                                      \
      *t_dst = (_td); *t_arg1 = (_ta1); break
#  define BINARY(_ta1,_ta2,_td)                                \
     *t_dst = (_td); *t_arg1 = (_ta1); *t_arg2 = (_ta2); break
#  define TERNARY(_ta1,_ta2,_ta3,_td)                          \
     *t_dst = (_td); *t_arg1 = (_ta1);                         \
     *t_arg2 = (_ta2); *t_arg3 = (_ta3); break
#  define QUATERNARY(_ta1,_ta2,_ta3,_ta4,_td)                  \
     *t_dst = (_td); *t_arg1 = (_ta1);                         \
     *t_arg2 = (_ta2); *t_arg3 = (_ta3);                       \
     *t_arg4 = (_ta4); break
#  define COMPARISON(_ta)                                      \
     *t_dst = Ity_I1; *t_arg1 = *t_arg2 = (_ta); break;
#  define UNARY_COMPARISON(_ta)                                \
     *t_dst = Ity_I1; *t_arg1 = (_ta); break;

   /* Rounding mode values are always Ity_I32, encoded as per
      IRRoundingMode */
   const IRType ity_RMode = Ity_I32;

   *t_dst  = Ity_INVALID;
   *t_arg1 = Ity_INVALID;
   *t_arg2 = Ity_INVALID;
   *t_arg3 = Ity_INVALID;
   *t_arg4 = Ity_INVALID;
   switch (op) {
      case Iop_Add8: case Iop_Sub8: case Iop_Mul8: 
      case Iop_Or8:  case Iop_And8: case Iop_Xor8:
         BINARY(Ity_I8,Ity_I8, Ity_I8);

      case Iop_Add16: case Iop_Sub16: case Iop_Mul16:
      case Iop_Or16:  case Iop_And16: case Iop_Xor16:
         BINARY(Ity_I16,Ity_I16, Ity_I16);

      case Iop_CmpORD32U:
      case Iop_CmpORD32S:
      case Iop_Add32: case Iop_Sub32: case Iop_Mul32:
      case Iop_Or32:  case Iop_And32: case Iop_Xor32:
      case Iop_Max32U:
      case Iop_QAdd32S: case Iop_QSub32S:
      case Iop_Add16x2: case Iop_Sub16x2:
      case Iop_QAdd16Sx2: case Iop_QAdd16Ux2:
      case Iop_QSub16Sx2: case Iop_QSub16Ux2:
      case Iop_HAdd16Ux2: case Iop_HAdd16Sx2:
      case Iop_HSub16Ux2: case Iop_HSub16Sx2:
      case Iop_Add8x4: case Iop_Sub8x4:
      case Iop_QAdd8Sx4: case Iop_QAdd8Ux4:
      case Iop_QSub8Sx4: case Iop_QSub8Ux4:
      case Iop_HAdd8Ux4: case Iop_HAdd8Sx4:
      case Iop_HSub8Ux4: case Iop_HSub8Sx4:
      case Iop_Sad8Ux4:
         BINARY(Ity_I32,Ity_I32, Ity_I32);

      case Iop_Add64: case Iop_Sub64: case Iop_Mul64:
      case Iop_Or64:  case Iop_And64: case Iop_Xor64:
      case Iop_CmpORD64U:
      case Iop_CmpORD64S:
      case Iop_Avg8Ux8: case Iop_Avg16Ux4:
      case Iop_Add8x8: case Iop_Add16x4: case Iop_Add32x2:
      case Iop_Add32Fx2: case Iop_Sub32Fx2:
      case Iop_CmpEQ8x8: case Iop_CmpEQ16x4: case Iop_CmpEQ32x2:
      case Iop_CmpGT8Sx8: case Iop_CmpGT16Sx4: case Iop_CmpGT32Sx2:
      case Iop_CmpGT8Ux8: case Iop_CmpGT16Ux4: case Iop_CmpGT32Ux2:
      case Iop_CmpGT32Fx2: case Iop_CmpEQ32Fx2: case Iop_CmpGE32Fx2:
      case Iop_InterleaveHI8x8: case Iop_InterleaveLO8x8:
      case Iop_InterleaveHI16x4: case Iop_InterleaveLO16x4:
      case Iop_InterleaveHI32x2: case Iop_InterleaveLO32x2:
      case Iop_CatOddLanes8x8: case Iop_CatEvenLanes8x8:
      case Iop_CatOddLanes16x4: case Iop_CatEvenLanes16x4:
      case Iop_InterleaveOddLanes8x8: case Iop_InterleaveEvenLanes8x8:
      case Iop_InterleaveOddLanes16x4: case Iop_InterleaveEvenLanes16x4:
      case Iop_Perm8x8:
      case Iop_Max8Ux8: case Iop_Max16Ux4: case Iop_Max32Ux2:
      case Iop_Max8Sx8: case Iop_Max16Sx4: case Iop_Max32Sx2:
      case Iop_Max32Fx2: case Iop_Min32Fx2:
      case Iop_PwMax32Fx2: case Iop_PwMin32Fx2:
      case Iop_Min8Ux8: case Iop_Min16Ux4: case Iop_Min32Ux2:
      case Iop_Min8Sx8: case Iop_Min16Sx4: case Iop_Min32Sx2:
      case Iop_PwMax8Ux8: case Iop_PwMax16Ux4: case Iop_PwMax32Ux2:
      case Iop_PwMax8Sx8: case Iop_PwMax16Sx4: case Iop_PwMax32Sx2:
      case Iop_PwMin8Ux8: case Iop_PwMin16Ux4: case Iop_PwMin32Ux2:
      case Iop_PwMin8Sx8: case Iop_PwMin16Sx4: case Iop_PwMin32Sx2:
      case Iop_Mul8x8: case Iop_Mul16x4: case Iop_Mul32x2:
      case Iop_Mul32Fx2:
      case Iop_PolynomialMul8x8:
      case Iop_MulHi16Sx4: case Iop_MulHi16Ux4:
      case Iop_QDMulHi16Sx4: case Iop_QDMulHi32Sx2:
      case Iop_QRDMulHi16Sx4: case Iop_QRDMulHi32Sx2:
      case Iop_QAdd8Sx8: case Iop_QAdd16Sx4:
      case Iop_QAdd32Sx2: case Iop_QAdd64Sx1:
      case Iop_QAdd8Ux8: case Iop_QAdd16Ux4:
      case Iop_QAdd32Ux2: case Iop_QAdd64Ux1:
      case Iop_PwAdd8x8: case Iop_PwAdd16x4: case Iop_PwAdd32x2:
      case Iop_PwAdd32Fx2:
      case Iop_QNarrowBin32Sto16Sx4:
      case Iop_QNarrowBin16Sto8Sx8: case Iop_QNarrowBin16Sto8Ux8:
      case Iop_NarrowBin16to8x8: case Iop_NarrowBin32to16x4:
      case Iop_Sub8x8: case Iop_Sub16x4: case Iop_Sub32x2:
      case Iop_QSub8Sx8: case Iop_QSub16Sx4:
      case Iop_QSub32Sx2: case Iop_QSub64Sx1:
      case Iop_QSub8Ux8: case Iop_QSub16Ux4:
      case Iop_QSub32Ux2: case Iop_QSub64Ux1:
      case Iop_Shl8x8: case Iop_Shl16x4: case Iop_Shl32x2:
      case Iop_Shr8x8: case Iop_Shr16x4: case Iop_Shr32x2:
      case Iop_Sar8x8: case Iop_Sar16x4: case Iop_Sar32x2:
      case Iop_Sal8x8: case Iop_Sal16x4: case Iop_Sal32x2: case Iop_Sal64x1:
      case Iop_QShl8x8: case Iop_QShl16x4: case Iop_QShl32x2: case Iop_QShl64x1:
      case Iop_QSal8x8: case Iop_QSal16x4: case Iop_QSal32x2: case Iop_QSal64x1:
      case Iop_RecipStep32Fx2:
      case Iop_RSqrtStep32Fx2:
         BINARY(Ity_I64,Ity_I64, Ity_I64);

      case Iop_ShlN32x2: case Iop_ShlN16x4: case Iop_ShlN8x8:
      case Iop_ShrN32x2: case Iop_ShrN16x4: case Iop_ShrN8x8:
      case Iop_SarN32x2: case Iop_SarN16x4: case Iop_SarN8x8:
      case Iop_QShlNsatUU8x8:  case Iop_QShlNsatUU16x4:
      case Iop_QShlNsatUU32x2: case Iop_QShlNsatUU64x1:
      case Iop_QShlNsatSU8x8:  case Iop_QShlNsatSU16x4:
      case Iop_QShlNsatSU32x2: case Iop_QShlNsatSU64x1:
      case Iop_QShlNsatSS8x8:  case Iop_QShlNsatSS16x4:
      case Iop_QShlNsatSS32x2: case Iop_QShlNsatSS64x1:
         BINARY(Ity_I64,Ity_I8, Ity_I64);

      case Iop_Shl8: case Iop_Shr8: case Iop_Sar8:
         BINARY(Ity_I8,Ity_I8, Ity_I8);
      case Iop_Shl16: case Iop_Shr16: case Iop_Sar16:
         BINARY(Ity_I16,Ity_I8, Ity_I16);
      case Iop_Shl32: case Iop_Shr32: case Iop_Sar32:
         BINARY(Ity_I32,Ity_I8, Ity_I32);
      case Iop_Shl64: case Iop_Shr64: case Iop_Sar64:
         BINARY(Ity_I64,Ity_I8, Ity_I64);

      case Iop_Not8:
         UNARY(Ity_I8, Ity_I8);
      case Iop_Not16:
         UNARY(Ity_I16, Ity_I16);
      case Iop_Not32:
      case Iop_CmpNEZ16x2: case Iop_CmpNEZ8x4:
         UNARY(Ity_I32, Ity_I32);

      case Iop_Not64:
      case Iop_CmpNEZ32x2: case Iop_CmpNEZ16x4: case Iop_CmpNEZ8x8:
      case Iop_Cnt8x8:
      case Iop_Clz8x8: case Iop_Clz16x4: case Iop_Clz32x2:
      case Iop_Cls8x8: case Iop_Cls16x4: case Iop_Cls32x2:
      case Iop_PwAddL8Ux8: case Iop_PwAddL16Ux4: case Iop_PwAddL32Ux2:
      case Iop_PwAddL8Sx8: case Iop_PwAddL16Sx4: case Iop_PwAddL32Sx2:
      case Iop_Reverse8sIn64_x1: case Iop_Reverse16sIn64_x1:
      case Iop_Reverse32sIn64_x1:
      case Iop_Reverse8sIn32_x2: case Iop_Reverse16sIn32_x2:
      case Iop_Reverse8sIn16_x4:
      case Iop_FtoI32Sx2_RZ: case Iop_FtoI32Ux2_RZ:
      case Iop_I32StoFx2: case Iop_I32UtoFx2:
      case Iop_RecipEst32Ux2: case Iop_RecipEst32Fx2:
      case Iop_Abs32Fx2:
      case Iop_RSqrtEst32Fx2:
      case Iop_RSqrtEst32Ux2:
      case Iop_Neg32Fx2:
      case Iop_Abs8x8: case Iop_Abs16x4: case Iop_Abs32x2:
         UNARY(Ity_I64, Ity_I64);

      case Iop_CmpEQ8: case Iop_CmpNE8:
      case Iop_CasCmpEQ8: case Iop_CasCmpNE8: case Iop_ExpCmpNE8:
         COMPARISON(Ity_I8);
      case Iop_CmpEQ16: case Iop_CmpNE16:
      case Iop_CasCmpEQ16: case Iop_CasCmpNE16: case Iop_ExpCmpNE16:
         COMPARISON(Ity_I16);
      case Iop_CmpEQ32: case Iop_CmpNE32:
      case Iop_CasCmpEQ32: case Iop_CasCmpNE32: case Iop_ExpCmpNE32:
      case Iop_CmpLT32S: case Iop_CmpLE32S:
      case Iop_CmpLT32U: case Iop_CmpLE32U:
         COMPARISON(Ity_I32);
      case Iop_CmpEQ64: case Iop_CmpNE64:
      case Iop_CasCmpEQ64: case Iop_CasCmpNE64: case Iop_ExpCmpNE64:
      case Iop_CmpLT64S: case Iop_CmpLE64S:
      case Iop_CmpLT64U: case Iop_CmpLE64U:
         COMPARISON(Ity_I64);

      case Iop_CmpNEZ8:  UNARY_COMPARISON(Ity_I8);
      case Iop_CmpNEZ16: UNARY_COMPARISON(Ity_I16);
      case Iop_CmpNEZ32: UNARY_COMPARISON(Ity_I32);
      case Iop_CmpNEZ64: UNARY_COMPARISON(Ity_I64);

      case Iop_Left8:  UNARY(Ity_I8, Ity_I8);
      case Iop_Left16: UNARY(Ity_I16,Ity_I16);
      case Iop_CmpwNEZ32: case Iop_Left32: UNARY(Ity_I32,Ity_I32);
      case Iop_CmpwNEZ64: case Iop_Left64: UNARY(Ity_I64,Ity_I64);

      case Iop_GetMSBs8x8:  UNARY(Ity_I64, Ity_I8);
      case Iop_GetMSBs8x16: UNARY(Ity_V128, Ity_I16);

      case Iop_MullU8: case Iop_MullS8:
         BINARY(Ity_I8,Ity_I8, Ity_I16);
      case Iop_MullU16: case Iop_MullS16:
         BINARY(Ity_I16,Ity_I16, Ity_I32);
      case Iop_MullU32: case Iop_MullS32:
         BINARY(Ity_I32,Ity_I32, Ity_I64);
      case Iop_MullU64: case Iop_MullS64:
         BINARY(Ity_I64,Ity_I64, Ity_I128);

      case Iop_Clz32: case Iop_Ctz32:
         UNARY(Ity_I32, Ity_I32);

      case Iop_Clz64: case Iop_Ctz64:
         UNARY(Ity_I64, Ity_I64);

      case Iop_DivU32: case Iop_DivS32: case Iop_DivU32E: case Iop_DivS32E:
         BINARY(Ity_I32,Ity_I32, Ity_I32);

      case Iop_DivU64: case Iop_DivS64: case Iop_DivS64E: case Iop_DivU64E:
         BINARY(Ity_I64,Ity_I64, Ity_I64);

      case Iop_DivModU64to32: case Iop_DivModS64to32:
         BINARY(Ity_I64,Ity_I32, Ity_I64);

      case Iop_DivModU128to64: case Iop_DivModS128to64:
         BINARY(Ity_I128,Ity_I64, Ity_I128);

      case Iop_DivModS64to64:
         BINARY(Ity_I64,Ity_I64, Ity_I128);

      case Iop_16HIto8: case Iop_16to8:
         UNARY(Ity_I16, Ity_I8);
      case Iop_8HLto16:
         BINARY(Ity_I8,Ity_I8, Ity_I16);

      case Iop_32HIto16: case Iop_32to16:
         UNARY(Ity_I32, Ity_I16);
      case Iop_16HLto32:
         BINARY(Ity_I16,Ity_I16, Ity_I32);

      case Iop_64HIto32: case Iop_64to32:
         UNARY(Ity_I64, Ity_I32);
      case Iop_32HLto64:
         BINARY(Ity_I32,Ity_I32, Ity_I64);

      case Iop_128HIto64: case Iop_128to64:
         UNARY(Ity_I128, Ity_I64);
      case Iop_64HLto128:
         BINARY(Ity_I64,Ity_I64, Ity_I128);

      case Iop_Not1:   UNARY(Ity_I1, Ity_I1);
      case Iop_1Uto8:  UNARY(Ity_I1, Ity_I8);
      case Iop_1Sto8:  UNARY(Ity_I1, Ity_I8);
      case Iop_1Sto16: UNARY(Ity_I1, Ity_I16);
      case Iop_1Uto32: case Iop_1Sto32: UNARY(Ity_I1, Ity_I32);
      case Iop_1Sto64: case Iop_1Uto64: UNARY(Ity_I1, Ity_I64);
      case Iop_32to1:  UNARY(Ity_I32, Ity_I1);
      case Iop_64to1:  UNARY(Ity_I64, Ity_I1);

      case Iop_8Uto32: case Iop_8Sto32:
         UNARY(Ity_I8, Ity_I32);

      case Iop_8Uto16: case Iop_8Sto16:
         UNARY(Ity_I8, Ity_I16);

      case Iop_16Uto32: case Iop_16Sto32: 
         UNARY(Ity_I16, Ity_I32);

      case Iop_32Sto64: case Iop_32Uto64:
         UNARY(Ity_I32, Ity_I64);

      case Iop_8Uto64: case Iop_8Sto64:
         UNARY(Ity_I8, Ity_I64);

      case Iop_16Uto64: case Iop_16Sto64:
         UNARY(Ity_I16, Ity_I64);
      case Iop_64to16:
         UNARY(Ity_I64, Ity_I16);

      case Iop_32to8: UNARY(Ity_I32, Ity_I8);
      case Iop_64to8: UNARY(Ity_I64, Ity_I8);

      case Iop_AddF64:    case Iop_SubF64: 
      case Iop_MulF64:    case Iop_DivF64:
      case Iop_AddF64r32: case Iop_SubF64r32: 
      case Iop_MulF64r32: case Iop_DivF64r32:
         TERNARY(ity_RMode,Ity_F64,Ity_F64, Ity_F64);

      case Iop_AddF32: case Iop_SubF32:
      case Iop_MulF32: case Iop_DivF32:
         TERNARY(ity_RMode,Ity_F32,Ity_F32, Ity_F32);

      case Iop_NegF64: case Iop_AbsF64: 
         UNARY(Ity_F64, Ity_F64);

      case Iop_NegF32: case Iop_AbsF32:
         UNARY(Ity_F32, Ity_F32);

      case Iop_SqrtF64:
      case Iop_RecpExpF64:
         BINARY(ity_RMode,Ity_F64, Ity_F64);

      case Iop_SqrtF32:
      case Iop_RoundF32toInt:
      case Iop_RecpExpF32:
         BINARY(ity_RMode,Ity_F32, Ity_F32);

      case Iop_CmpF32:
         BINARY(Ity_F32,Ity_F32, Ity_I32);

      case Iop_CmpF64:
         BINARY(Ity_F64,Ity_F64, Ity_I32);

      case Iop_CmpF128:
         BINARY(Ity_F128,Ity_F128, Ity_I32);

      case Iop_F64toI16S: BINARY(ity_RMode,Ity_F64, Ity_I16);
      case Iop_F64toI32S: BINARY(ity_RMode,Ity_F64, Ity_I32);
      case Iop_F64toI64S: case Iop_F64toI64U:
         BINARY(ity_RMode,Ity_F64, Ity_I64);

      case Iop_F64toI32U: BINARY(ity_RMode,Ity_F64, Ity_I32);

      case Iop_I32StoF64: UNARY(Ity_I32, Ity_F64);
      case Iop_I64StoF64: BINARY(ity_RMode,Ity_I64, Ity_F64);
      case Iop_I64UtoF64: BINARY(ity_RMode,Ity_I64, Ity_F64);
      case Iop_I64UtoF32: BINARY(ity_RMode,Ity_I64, Ity_F32);

      case Iop_I32UtoF64: UNARY(Ity_I32, Ity_F64);

      case Iop_F32toI32S: BINARY(ity_RMode,Ity_F32, Ity_I32);
      case Iop_F32toI64S: BINARY(ity_RMode,Ity_F32, Ity_I64);
      case Iop_F32toI32U: BINARY(ity_RMode,Ity_F32, Ity_I32);
      case Iop_F32toI64U: BINARY(ity_RMode,Ity_F32, Ity_I64);

      case Iop_I32UtoF32: BINARY(ity_RMode,Ity_I32, Ity_F32);
      case Iop_I32StoF32: BINARY(ity_RMode,Ity_I32, Ity_F32);
      case Iop_I64StoF32: BINARY(ity_RMode,Ity_I64, Ity_F32);

      case Iop_F32toF64: UNARY(Ity_F32, Ity_F64);
      case Iop_F16toF64: UNARY(Ity_F16, Ity_F64);
      case Iop_F16toF32: UNARY(Ity_F16, Ity_F32);

      case Iop_F64toF32: BINARY(ity_RMode,Ity_F64, Ity_F32);
      case Iop_F64toF16: BINARY(ity_RMode,Ity_F64, Ity_F16);
      case Iop_F32toF16: BINARY(ity_RMode,Ity_F32, Ity_F16);

      case Iop_ReinterpI64asF64: UNARY(Ity_I64, Ity_F64);
      case Iop_ReinterpF64asI64: UNARY(Ity_F64, Ity_I64);
      case Iop_ReinterpI32asF32: UNARY(Ity_I32, Ity_F32);
      case Iop_ReinterpF32asI32: UNARY(Ity_F32, Ity_I32);

      case Iop_AtanF64: case Iop_Yl2xF64:  case Iop_Yl2xp1F64: 
      case Iop_ScaleF64: case Iop_PRemF64: case Iop_PRem1F64:
         TERNARY(ity_RMode,Ity_F64,Ity_F64, Ity_F64);

      case Iop_PRemC3210F64: case Iop_PRem1C3210F64:
         TERNARY(ity_RMode,Ity_F64,Ity_F64, Ity_I32);

      case Iop_SinF64: case Iop_CosF64: case Iop_TanF64: 
      case Iop_2xm1F64:
      case Iop_RoundF64toInt: BINARY(ity_RMode,Ity_F64, Ity_F64);

      case Iop_MAddF64: case Iop_MSubF64:
      case Iop_MAddF64r32: case Iop_MSubF64r32:
         QUATERNARY(ity_RMode,Ity_F64,Ity_F64,Ity_F64, Ity_F64);

      case Iop_RSqrtEst5GoodF64:
      case Iop_RoundF64toF64_NEAREST: case Iop_RoundF64toF64_NegINF:
      case Iop_RoundF64toF64_PosINF: case Iop_RoundF64toF64_ZERO:
         UNARY(Ity_F64, Ity_F64);
      case Iop_RoundF64toF32:
         BINARY(ity_RMode,Ity_F64, Ity_F64);
      case Iop_TruncF64asF32:
         UNARY(Ity_F64, Ity_F32);

      case Iop_I32UtoFx4:
      case Iop_I32StoFx4:
      case Iop_QFtoI32Ux4_RZ:
      case Iop_QFtoI32Sx4_RZ:
      case Iop_FtoI32Ux4_RZ:
      case Iop_FtoI32Sx4_RZ:
      case Iop_RoundF32x4_RM:
      case Iop_RoundF32x4_RP:
      case Iop_RoundF32x4_RN:
      case Iop_RoundF32x4_RZ:
      case Iop_Abs64Fx2: case Iop_Abs32Fx4:
      case Iop_RSqrtEst32Fx4:
      case Iop_RSqrtEst32Ux4:
         UNARY(Ity_V128, Ity_V128);

      case Iop_Sqrt64Fx2:
      case Iop_Sqrt32Fx4:
         BINARY(ity_RMode,Ity_V128, Ity_V128);

      case Iop_64HLtoV128:
         BINARY(Ity_I64,Ity_I64, Ity_V128);

      case Iop_V128to64: case Iop_V128HIto64:
      case Iop_NarrowUn16to8x8:
      case Iop_NarrowUn32to16x4:
      case Iop_NarrowUn64to32x2:
      case Iop_QNarrowUn16Uto8Ux8:
      case Iop_QNarrowUn32Uto16Ux4:
      case Iop_QNarrowUn64Uto32Ux2:
      case Iop_QNarrowUn16Sto8Sx8:
      case Iop_QNarrowUn32Sto16Sx4:
      case Iop_QNarrowUn64Sto32Sx2:
      case Iop_QNarrowUn16Sto8Ux8:
      case Iop_QNarrowUn32Sto16Ux4:
      case Iop_QNarrowUn64Sto32Ux2:
      case Iop_F32toF16x4:
         UNARY(Ity_V128, Ity_I64);

      case Iop_Widen8Uto16x8:
      case Iop_Widen16Uto32x4:
      case Iop_Widen32Uto64x2:
      case Iop_Widen8Sto16x8:
      case Iop_Widen16Sto32x4:
      case Iop_Widen32Sto64x2:
      case Iop_F16toF32x4:
         UNARY(Ity_I64, Ity_V128);

      case Iop_V128to32:    UNARY(Ity_V128, Ity_I32);
      case Iop_32UtoV128:   UNARY(Ity_I32, Ity_V128);
      case Iop_64UtoV128:   UNARY(Ity_I64, Ity_V128);
      case Iop_SetV128lo32: BINARY(Ity_V128,Ity_I32, Ity_V128);
      case Iop_SetV128lo64: BINARY(Ity_V128,Ity_I64, Ity_V128);

      case Iop_Dup8x16: UNARY(Ity_I8, Ity_V128);
      case Iop_Dup16x8: UNARY(Ity_I16, Ity_V128);
      case Iop_Dup32x4: UNARY(Ity_I32, Ity_V128);
      case Iop_Dup8x8:  UNARY(Ity_I8, Ity_I64);
      case Iop_Dup16x4: UNARY(Ity_I16, Ity_I64);
      case Iop_Dup32x2: UNARY(Ity_I32, Ity_I64);

      case Iop_CmpEQ32Fx4: case Iop_CmpLT32Fx4:
      case Iop_CmpEQ64Fx2: case Iop_CmpLT64Fx2:
      case Iop_CmpLE32Fx4: case Iop_CmpUN32Fx4:
      case Iop_CmpLE64Fx2: case Iop_CmpUN64Fx2:
      case Iop_CmpGT32Fx4: case Iop_CmpGE32Fx4:
      case Iop_CmpEQ32F0x4: case Iop_CmpLT32F0x4:
      case Iop_CmpEQ64F0x2: case Iop_CmpLT64F0x2:
      case Iop_CmpLE32F0x4: case Iop_CmpUN32F0x4:
      case Iop_CmpLE64F0x2: case Iop_CmpUN64F0x2:
      case Iop_Add32F0x4:
      case Iop_Add64F0x2:
      case Iop_Div32F0x4:
      case Iop_Div64F0x2:
      case Iop_Max32Fx4: case Iop_Max32F0x4:
      case Iop_PwMax32Fx4: case Iop_PwMin32Fx4:
      case Iop_Max64Fx2: case Iop_Max64F0x2:
      case Iop_Min32Fx4: case Iop_Min32F0x4:
      case Iop_Min64Fx2: case Iop_Min64F0x2:
      case Iop_Mul32F0x4:
      case Iop_Mul64F0x2:
      case Iop_Sub32F0x4:
      case Iop_Sub64F0x2:
      case Iop_AndV128: case Iop_OrV128: case Iop_XorV128:
      case Iop_Add8x16:   case Iop_Add16x8:   
      case Iop_Add32x4:   case Iop_Add64x2:
      case Iop_QAdd8Ux16: case Iop_QAdd16Ux8:
      case Iop_QAdd32Ux4: case Iop_QAdd64Ux2:
      case Iop_QAdd8Sx16: case Iop_QAdd16Sx8:
      case Iop_QAdd32Sx4: case Iop_QAdd64Sx2:
      case Iop_QAddExtUSsatSS8x16: case Iop_QAddExtUSsatSS16x8:
      case Iop_QAddExtUSsatSS32x4: case Iop_QAddExtUSsatSS64x2:
      case Iop_QAddExtSUsatUU8x16: case Iop_QAddExtSUsatUU16x8:
      case Iop_QAddExtSUsatUU32x4: case Iop_QAddExtSUsatUU64x2:
      case Iop_PwAdd8x16: case Iop_PwAdd16x8: case Iop_PwAdd32x4:
      case Iop_Sub8x16:   case Iop_Sub16x8:
      case Iop_Sub32x4:   case Iop_Sub64x2:
      case Iop_QSub8Ux16: case Iop_QSub16Ux8:
      case Iop_QSub32Ux4: case Iop_QSub64Ux2:
      case Iop_QSub8Sx16: case Iop_QSub16Sx8:
      case Iop_QSub32Sx4: case Iop_QSub64Sx2:
      case Iop_Mul8x16: case Iop_Mul16x8: case Iop_Mul32x4:
      case Iop_PolynomialMul8x16:
      case Iop_PolynomialMulAdd8x16: case Iop_PolynomialMulAdd16x8:
      case Iop_PolynomialMulAdd32x4: case Iop_PolynomialMulAdd64x2:
      case Iop_MulHi16Ux8: case Iop_MulHi32Ux4: 
      case Iop_MulHi16Sx8: case Iop_MulHi32Sx4: 
      case Iop_QDMulHi16Sx8: case Iop_QDMulHi32Sx4:
      case Iop_QRDMulHi16Sx8: case Iop_QRDMulHi32Sx4:
      case Iop_MullEven8Ux16: case Iop_MullEven16Ux8: case Iop_MullEven32Ux4:
      case Iop_MullEven8Sx16: case Iop_MullEven16Sx8: case Iop_MullEven32Sx4:
      case Iop_Avg8Ux16: case Iop_Avg16Ux8: case Iop_Avg32Ux4:
      case Iop_Avg8Sx16: case Iop_Avg16Sx8: case Iop_Avg32Sx4:
      case Iop_Max8Sx16: case Iop_Max16Sx8: case Iop_Max32Sx4:
      case Iop_Max64Sx2:
      case Iop_Max8Ux16: case Iop_Max16Ux8: case Iop_Max32Ux4:
      case Iop_Max64Ux2:
      case Iop_Min8Sx16: case Iop_Min16Sx8: case Iop_Min32Sx4:
      case Iop_Min64Sx2:
      case Iop_Min8Ux16: case Iop_Min16Ux8: case Iop_Min32Ux4:
      case Iop_Min64Ux2:
      case Iop_CmpEQ8x16:  case Iop_CmpEQ16x8:  case Iop_CmpEQ32x4:
      case Iop_CmpEQ64x2:
      case Iop_CmpGT8Sx16: case Iop_CmpGT16Sx8: case Iop_CmpGT32Sx4:
      case Iop_CmpGT64Sx2:
      case Iop_CmpGT8Ux16: case Iop_CmpGT16Ux8: case Iop_CmpGT32Ux4:
      case Iop_CmpGT64Ux2:
      case Iop_Shl8x16: case Iop_Shl16x8: case Iop_Shl32x4: case Iop_Shl64x2:
      case Iop_QShl8x16: case Iop_QShl16x8:
      case Iop_QShl32x4: case Iop_QShl64x2:
      case Iop_QSal8x16: case Iop_QSal16x8:
      case Iop_QSal32x4: case Iop_QSal64x2:
      case Iop_Shr8x16: case Iop_Shr16x8: case Iop_Shr32x4: case Iop_Shr64x2:
      case Iop_Sar8x16: case Iop_Sar16x8: case Iop_Sar32x4: case Iop_Sar64x2:
      case Iop_Sal8x16: case Iop_Sal16x8: case Iop_Sal32x4: case Iop_Sal64x2:
      case Iop_Rol8x16: case Iop_Rol16x8: case Iop_Rol32x4:case Iop_Rol64x2:
      case Iop_QNarrowBin16Sto8Ux16: case Iop_QNarrowBin32Sto16Ux8:
      case Iop_QNarrowBin16Sto8Sx16: case Iop_QNarrowBin32Sto16Sx8:
      case Iop_QNarrowBin16Uto8Ux16: case Iop_QNarrowBin32Uto16Ux8:
      case Iop_QNarrowBin64Sto32Sx4: case Iop_QNarrowBin64Uto32Ux4:
      case Iop_NarrowBin16to8x16:   case Iop_NarrowBin32to16x8:
      case Iop_NarrowBin64to32x4:
      case Iop_InterleaveHI8x16: case Iop_InterleaveHI16x8:
      case Iop_InterleaveHI32x4: case Iop_InterleaveHI64x2:
      case Iop_InterleaveLO8x16: case Iop_InterleaveLO16x8:
      case Iop_InterleaveLO32x4: case Iop_InterleaveLO64x2:
      case Iop_CatOddLanes8x16: case Iop_CatEvenLanes8x16:
      case Iop_CatOddLanes16x8: case Iop_CatEvenLanes16x8:
      case Iop_CatOddLanes32x4: case Iop_CatEvenLanes32x4:
      case Iop_InterleaveOddLanes8x16: case Iop_InterleaveEvenLanes8x16:
      case Iop_InterleaveOddLanes16x8: case Iop_InterleaveEvenLanes16x8:
      case Iop_InterleaveOddLanes32x4: case Iop_InterleaveEvenLanes32x4:
      case Iop_Perm8x16: case Iop_Perm32x4:
      case Iop_RecipStep32Fx4: case Iop_RecipStep64Fx2:
      case Iop_RSqrtStep32Fx4: case Iop_RSqrtStep64Fx2:
      case Iop_CipherV128:
      case Iop_CipherLV128:
      case Iop_NCipherV128:
      case Iop_NCipherLV128:
      case Iop_Sh8Sx16: case Iop_Sh16Sx8:
      case Iop_Sh32Sx4: case Iop_Sh64Sx2:
      case Iop_Sh8Ux16: case Iop_Sh16Ux8:
      case Iop_Sh32Ux4: case Iop_Sh64Ux2:
      case Iop_Rsh8Sx16: case Iop_Rsh16Sx8:
      case Iop_Rsh32Sx4: case Iop_Rsh64Sx2:
      case Iop_Rsh8Ux16: case Iop_Rsh16Ux8:
      case Iop_Rsh32Ux4: case Iop_Rsh64Ux2:
         BINARY(Ity_V128,Ity_V128, Ity_V128);

      case Iop_PolynomialMull8x8:
      case Iop_Mull8Ux8: case Iop_Mull8Sx8:
      case Iop_Mull16Ux4: case Iop_Mull16Sx4:
      case Iop_Mull32Ux2: case Iop_Mull32Sx2:
         BINARY(Ity_I64, Ity_I64, Ity_V128);

      case Iop_NotV128:
      case Iop_RecipEst32Fx4: case Iop_RecipEst32F0x4:
      case Iop_RecipEst64Fx2: case Iop_RSqrtEst64Fx2:
      case Iop_RecipEst32Ux4:
      case Iop_RSqrtEst32F0x4:
      case Iop_Sqrt32F0x4:
      case Iop_Sqrt64F0x2:
      case Iop_CmpNEZ8x16: case Iop_CmpNEZ16x8:
      case Iop_CmpNEZ32x4: case Iop_CmpNEZ64x2:
      case Iop_Cnt8x16:
      case Iop_Clz8x16: case Iop_Clz16x8: case Iop_Clz32x4: case Iop_Clz64x2:
      case Iop_Cls8x16: case Iop_Cls16x8: case Iop_Cls32x4:
      case Iop_PwAddL8Ux16: case Iop_PwAddL16Ux8: case Iop_PwAddL32Ux4:
      case Iop_PwAddL8Sx16: case Iop_PwAddL16Sx8: case Iop_PwAddL32Sx4:
      case Iop_Reverse8sIn64_x2: case Iop_Reverse16sIn64_x2:
      case Iop_Reverse32sIn64_x2:
      case Iop_Reverse8sIn32_x4: case Iop_Reverse16sIn32_x4:
      case Iop_Reverse8sIn16_x8:
      case Iop_Reverse1sIn8_x16:
      case Iop_Neg64Fx2: case Iop_Neg32Fx4:
      case Iop_Abs8x16: case Iop_Abs16x8: case Iop_Abs32x4: case Iop_Abs64x2:
      case Iop_CipherSV128:
      case Iop_PwBitMtxXpose64x2:
      case Iop_ZeroHI64ofV128:  case Iop_ZeroHI96ofV128:
      case Iop_ZeroHI112ofV128: case Iop_ZeroHI120ofV128:
         UNARY(Ity_V128, Ity_V128);

      case Iop_ShlV128: case Iop_ShrV128:
      case Iop_ShlN8x16: case Iop_ShlN16x8: 
      case Iop_ShlN32x4: case Iop_ShlN64x2:
      case Iop_ShrN8x16: case Iop_ShrN16x8: 
      case Iop_ShrN32x4: case Iop_ShrN64x2:
      case Iop_SarN8x16: case Iop_SarN16x8:
      case Iop_SarN32x4: case Iop_SarN64x2:
      case Iop_QShlNsatUU8x16: case Iop_QShlNsatUU16x8:
      case Iop_QShlNsatUU32x4: case Iop_QShlNsatUU64x2:
      case Iop_QShlNsatSU8x16: case Iop_QShlNsatSU16x8:
      case Iop_QShlNsatSU32x4: case Iop_QShlNsatSU64x2:
      case Iop_QShlNsatSS8x16: case Iop_QShlNsatSS16x8:
      case Iop_QShlNsatSS32x4: case Iop_QShlNsatSS64x2:
      case Iop_SHA256:    case Iop_SHA512:
      case Iop_QandQShrNnarrow16Uto8Ux8:
      case Iop_QandQShrNnarrow32Uto16Ux4:
      case Iop_QandQShrNnarrow64Uto32Ux2:
      case Iop_QandQSarNnarrow16Sto8Sx8:
      case Iop_QandQSarNnarrow32Sto16Sx4:
      case Iop_QandQSarNnarrow64Sto32Sx2:
      case Iop_QandQSarNnarrow16Sto8Ux8:
      case Iop_QandQSarNnarrow32Sto16Ux4:
      case Iop_QandQSarNnarrow64Sto32Ux2:
      case Iop_QandQRShrNnarrow16Uto8Ux8:
      case Iop_QandQRShrNnarrow32Uto16Ux4:
      case Iop_QandQRShrNnarrow64Uto32Ux2:
      case Iop_QandQRSarNnarrow16Sto8Sx8:
      case Iop_QandQRSarNnarrow32Sto16Sx4:
      case Iop_QandQRSarNnarrow64Sto32Sx2:
      case Iop_QandQRSarNnarrow16Sto8Ux8:
      case Iop_QandQRSarNnarrow32Sto16Ux4:
      case Iop_QandQRSarNnarrow64Sto32Ux2:
         BINARY(Ity_V128,Ity_I8, Ity_V128);

      case Iop_F32ToFixed32Ux4_RZ:
      case Iop_F32ToFixed32Sx4_RZ:
      case Iop_Fixed32UToF32x4_RN:
      case Iop_Fixed32SToF32x4_RN:
         BINARY(Ity_V128, Ity_I8, Ity_V128);

      case Iop_F32ToFixed32Ux2_RZ:
      case Iop_F32ToFixed32Sx2_RZ:
      case Iop_Fixed32UToF32x2_RN:
      case Iop_Fixed32SToF32x2_RN:
         BINARY(Ity_I64, Ity_I8, Ity_I64);

      case Iop_GetElem8x16:
         BINARY(Ity_V128, Ity_I8, Ity_I8);
      case Iop_GetElem16x8:
         BINARY(Ity_V128, Ity_I8, Ity_I16);
      case Iop_GetElem32x4:
         BINARY(Ity_V128, Ity_I8, Ity_I32);
      case Iop_GetElem64x2:
         BINARY(Ity_V128, Ity_I8, Ity_I64);
      case Iop_GetElem8x8:
         BINARY(Ity_I64, Ity_I8, Ity_I8);
      case Iop_GetElem16x4:
         BINARY(Ity_I64, Ity_I8, Ity_I16);
      case Iop_GetElem32x2:
         BINARY(Ity_I64, Ity_I8, Ity_I32);
      case Iop_SetElem8x8:
         TERNARY(Ity_I64, Ity_I8, Ity_I8, Ity_I64);
      case Iop_SetElem16x4:
         TERNARY(Ity_I64, Ity_I8, Ity_I16, Ity_I64);
      case Iop_SetElem32x2:
         TERNARY(Ity_I64, Ity_I8, Ity_I32, Ity_I64);

      case Iop_Slice64:
         TERNARY(Ity_I64, Ity_I64, Ity_I8, Ity_I64);
      case Iop_SliceV128:
         TERNARY(Ity_V128, Ity_V128, Ity_I8, Ity_V128);

      case Iop_BCDAdd:
      case Iop_BCDSub:
         TERNARY(Ity_V128,Ity_V128, Ity_I8, Ity_V128);
      case Iop_QDMull16Sx4: case Iop_QDMull32Sx2:
         BINARY(Ity_I64, Ity_I64, Ity_V128);

      /* s390 specific */
      case Iop_MAddF32:
      case Iop_MSubF32:
         QUATERNARY(ity_RMode,Ity_F32,Ity_F32,Ity_F32, Ity_F32);

      case Iop_F64HLtoF128:
        BINARY(Ity_F64,Ity_F64, Ity_F128);

      case Iop_F128HItoF64:
      case Iop_F128LOtoF64:
        UNARY(Ity_F128, Ity_F64);

      case Iop_AddF128:
      case Iop_SubF128:
      case Iop_MulF128:
      case Iop_DivF128:
         TERNARY(ity_RMode,Ity_F128,Ity_F128, Ity_F128);

      case Iop_Add64Fx2: case Iop_Sub64Fx2:
      case Iop_Mul64Fx2: case Iop_Div64Fx2: 
      case Iop_Add32Fx4: case Iop_Sub32Fx4:
      case Iop_Mul32Fx4: case Iop_Div32Fx4: 
         TERNARY(ity_RMode,Ity_V128,Ity_V128, Ity_V128);

      case Iop_Add64Fx4: case Iop_Sub64Fx4:
      case Iop_Mul64Fx4: case Iop_Div64Fx4:
      case Iop_Add32Fx8: case Iop_Sub32Fx8:
      case Iop_Mul32Fx8: case Iop_Div32Fx8:
         TERNARY(ity_RMode,Ity_V256,Ity_V256, Ity_V256);

      case Iop_NegF128:
      case Iop_AbsF128:
         UNARY(Ity_F128, Ity_F128);

      case Iop_SqrtF128:
         BINARY(ity_RMode,Ity_F128, Ity_F128);

      case Iop_I32StoF128: UNARY(Ity_I32, Ity_F128);
      case Iop_I64StoF128: UNARY(Ity_I64, Ity_F128);

      case Iop_I32UtoF128: UNARY(Ity_I32, Ity_F128);
      case Iop_I64UtoF128: UNARY(Ity_I64, Ity_F128);

      case Iop_F128toI32S: BINARY(ity_RMode,Ity_F128, Ity_I32);
      case Iop_F128toI64S: BINARY(ity_RMode,Ity_F128, Ity_I64);

      case Iop_F128toI32U: BINARY(ity_RMode,Ity_F128, Ity_I32);
      case Iop_F128toI64U: BINARY(ity_RMode,Ity_F128, Ity_I64);

      case Iop_F32toF128: UNARY(Ity_F32, Ity_F128);
      case Iop_F64toF128: UNARY(Ity_F64, Ity_F128);

      case Iop_F128toF32: BINARY(ity_RMode,Ity_F128, Ity_F32);
      case Iop_F128toF64: BINARY(ity_RMode,Ity_F128, Ity_F64);

      case Iop_D32toD64:
         UNARY(Ity_D32, Ity_D64);

      case Iop_ExtractExpD64:
         UNARY(Ity_D64, Ity_I64);

      case Iop_ExtractSigD64:
         UNARY(Ity_D64, Ity_I64);

      case Iop_InsertExpD64:
         BINARY(Ity_I64,Ity_D64, Ity_D64);

      case Iop_ExtractExpD128:
         UNARY(Ity_D128, Ity_I64);

      case Iop_ExtractSigD128:
        UNARY(Ity_D128, Ity_I64);

      case Iop_InsertExpD128:
         BINARY(Ity_I64,Ity_D128, Ity_D128);

      case Iop_D64toD128:
         UNARY(Ity_D64, Ity_D128);

      case Iop_ReinterpD64asI64:
	UNARY(Ity_D64, Ity_I64);

      case Iop_ReinterpI64asD64:
         UNARY(Ity_I64, Ity_D64);

      case Iop_RoundD64toInt:
         BINARY(ity_RMode,Ity_D64, Ity_D64);

      case Iop_RoundD128toInt:
         BINARY(ity_RMode,Ity_D128, Ity_D128);

      case Iop_I32StoD128:
      case Iop_I32UtoD128:
         UNARY(Ity_I32, Ity_D128);

      case Iop_I64StoD128:
         UNARY(Ity_I64, Ity_D128);

      case Iop_I64UtoD128:
         UNARY(Ity_I64, Ity_D128);

      case Iop_DPBtoBCD:
      case Iop_BCDtoDPB:
         UNARY(Ity_I64, Ity_I64);

      case Iop_D128HItoD64:
      case Iop_D128LOtoD64:
         UNARY(Ity_D128, Ity_D64);

      case Iop_D128toI64S:
         BINARY(ity_RMode, Ity_D128, Ity_I64);

      case Iop_D128toI64U:
         BINARY(ity_RMode, Ity_D128, Ity_I64);

      case Iop_D128toI32S:
      case Iop_D128toI32U:
         BINARY(ity_RMode, Ity_D128, Ity_I32);

      case Iop_D64HLtoD128:
         BINARY(Ity_D64, Ity_D64, Ity_D128);

      case Iop_ShlD64:
      case Iop_ShrD64:
         BINARY(Ity_D64, Ity_I8, Ity_D64 );

      case Iop_D64toD32:
         BINARY(ity_RMode, Ity_D64, Ity_D32);

      case Iop_D64toI32S:
      case Iop_D64toI32U:
         BINARY(ity_RMode, Ity_D64, Ity_I32);

      case Iop_D64toI64S:
         BINARY(ity_RMode, Ity_D64, Ity_I64);

      case Iop_D64toI64U:
         BINARY(ity_RMode, Ity_D64, Ity_I64);

      case Iop_I32StoD64:
      case Iop_I32UtoD64:
         UNARY(Ity_I32, Ity_D64);

      case Iop_I64StoD64:
         BINARY(ity_RMode, Ity_I64, Ity_D64);

      case Iop_I64UtoD64:
         BINARY(ity_RMode, Ity_I64, Ity_D64);

      case Iop_F32toD32:
         BINARY(ity_RMode, Ity_F32, Ity_D32);

      case Iop_F32toD64:
         BINARY(ity_RMode, Ity_F32, Ity_D64);

      case Iop_F32toD128:
         BINARY(ity_RMode, Ity_F32, Ity_D128);

      case Iop_F64toD32:
         BINARY(ity_RMode, Ity_F64, Ity_D32);

      case Iop_F64toD64:
         BINARY(ity_RMode, Ity_F64, Ity_D64);

      case Iop_F64toD128:
         BINARY(ity_RMode, Ity_F64, Ity_D128);

      case Iop_F128toD32:
         BINARY(ity_RMode, Ity_F128, Ity_D32);

      case Iop_F128toD64:
         BINARY(ity_RMode, Ity_F128, Ity_D64);

      case Iop_F128toD128:
         BINARY(ity_RMode, Ity_F128, Ity_D128);

      case Iop_D32toF32:
         BINARY(ity_RMode, Ity_D32, Ity_F32);

      case Iop_D32toF64:
         BINARY(ity_RMode, Ity_D32, Ity_F64);

      case Iop_D32toF128:
         BINARY(ity_RMode, Ity_D32, Ity_F128);

      case Iop_D64toF32:
         BINARY(ity_RMode, Ity_D64, Ity_F32);

      case Iop_D64toF64:
         BINARY(ity_RMode, Ity_D64, Ity_F64);

      case Iop_D64toF128:
         BINARY(ity_RMode, Ity_D64, Ity_F128);

      case Iop_D128toF32:
         BINARY(ity_RMode, Ity_D128, Ity_F32);

      case Iop_D128toF64:
         BINARY(ity_RMode, Ity_D128, Ity_F64);

      case Iop_D128toF128:
         BINARY(ity_RMode, Ity_D128, Ity_F128);

      case Iop_CmpD64:
      case Iop_CmpExpD64:
         BINARY(Ity_D64,Ity_D64, Ity_I32);

      case Iop_CmpD128:
      case Iop_CmpExpD128:
         BINARY(Ity_D128,Ity_D128, Ity_I32);

      case Iop_QuantizeD64:
         TERNARY(ity_RMode,Ity_D64,Ity_D64, Ity_D64);

      case Iop_SignificanceRoundD64:
         TERNARY(ity_RMode, Ity_I8,Ity_D64, Ity_D64);

      case Iop_QuantizeD128:
         TERNARY(ity_RMode,Ity_D128,Ity_D128, Ity_D128);

      case Iop_SignificanceRoundD128:
         TERNARY(ity_RMode, Ity_I8,Ity_D128, Ity_D128);

      case Iop_ShlD128:
      case Iop_ShrD128:
         BINARY(Ity_D128, Ity_I8, Ity_D128 );

      case Iop_AddD64:
      case Iop_SubD64:
      case Iop_MulD64:
      case Iop_DivD64:
         TERNARY( ity_RMode, Ity_D64, Ity_D64, Ity_D64 );

      case Iop_D128toD64:
         BINARY( ity_RMode, Ity_D128, Ity_D64 );

      case Iop_AddD128:
      case Iop_SubD128:
      case Iop_MulD128:
      case Iop_DivD128:
         TERNARY(ity_RMode,Ity_D128,Ity_D128, Ity_D128);

      case Iop_V256to64_0: case Iop_V256to64_1:
      case Iop_V256to64_2: case Iop_V256to64_3:
         UNARY(Ity_V256, Ity_I64);

      case Iop_64x4toV256:
         QUATERNARY(Ity_I64, Ity_I64, Ity_I64, Ity_I64, Ity_V256);

      case Iop_AndV256:  case Iop_OrV256:
      case Iop_XorV256:
      case Iop_Max32Fx8: case Iop_Min32Fx8:
      case Iop_Max64Fx4: case Iop_Min64Fx4:
      case Iop_Add8x32:  case Iop_Add16x16:
      case Iop_Add32x8:  case Iop_Add64x4:
      case Iop_Sub8x32:  case Iop_Sub16x16:
      case Iop_Sub32x8:  case Iop_Sub64x4:
      case Iop_Mul16x16: case Iop_Mul32x8:
      case Iop_MulHi16Ux16: case Iop_MulHi16Sx16:
      case Iop_Avg8Ux32: case Iop_Avg16Ux16:
      case Iop_Max8Sx32: case Iop_Max16Sx16: case Iop_Max32Sx8:
      case Iop_Max8Ux32: case Iop_Max16Ux16: case Iop_Max32Ux8:
      case Iop_Min8Sx32: case Iop_Min16Sx16: case Iop_Min32Sx8:
      case Iop_Min8Ux32: case Iop_Min16Ux16: case Iop_Min32Ux8:
      case Iop_CmpEQ8x32:  case Iop_CmpEQ16x16:
      case Iop_CmpEQ32x8:  case Iop_CmpEQ64x4:
      case Iop_CmpGT8Sx32: case Iop_CmpGT16Sx16:
      case Iop_CmpGT32Sx8: case Iop_CmpGT64Sx4:
      case Iop_QAdd8Ux32: case Iop_QAdd16Ux16:
      case Iop_QAdd8Sx32: case Iop_QAdd16Sx16:
      case Iop_QSub8Ux32: case Iop_QSub16Ux16:
      case Iop_QSub8Sx32: case Iop_QSub16Sx16:
      case Iop_Perm32x8:
         BINARY(Ity_V256,Ity_V256, Ity_V256);

      case Iop_V256toV128_1: case Iop_V256toV128_0:
         UNARY(Ity_V256, Ity_V128);

      case Iop_QandUQsh8x16:  case Iop_QandUQsh16x8:
      case Iop_QandUQsh32x4:  case Iop_QandUQsh64x2:
      case Iop_QandSQsh8x16:  case Iop_QandSQsh16x8:
      case Iop_QandSQsh32x4:  case Iop_QandSQsh64x2:
      case Iop_QandUQRsh8x16: case Iop_QandUQRsh16x8:
      case Iop_QandUQRsh32x4: case Iop_QandUQRsh64x2:
      case Iop_QandSQRsh8x16: case Iop_QandSQRsh16x8:
      case Iop_QandSQRsh32x4: case Iop_QandSQRsh64x2:
      case Iop_V128HLtoV256:
         BINARY(Ity_V128,Ity_V128, Ity_V256);

      case Iop_NotV256:
      case Iop_RSqrtEst32Fx8:
      case Iop_Sqrt32Fx8:
      case Iop_Sqrt64Fx4:
      case Iop_RecipEst32Fx8:
      case Iop_CmpNEZ8x32: case Iop_CmpNEZ16x16:
      case Iop_CmpNEZ64x4: case Iop_CmpNEZ32x8:
         UNARY(Ity_V256, Ity_V256);

      case Iop_ShlN16x16: case Iop_ShlN32x8:
      case Iop_ShlN64x4:
      case Iop_ShrN16x16: case Iop_ShrN32x8:
      case Iop_ShrN64x4:
      case Iop_SarN16x16: case Iop_SarN32x8:
         BINARY(Ity_V256,Ity_I8, Ity_V256);

      default:
         ppIROp(op);
         vpanic("typeOfPrimop");
   }
#  undef UNARY
#  undef BINARY
#  undef TERNARY
#  undef COMPARISON
#  undef UNARY_COMPARISON
}


/*---------------------------------------------------------------*/
/*--- Helper functions for the IR -- IR Basic Blocks          ---*/
/*---------------------------------------------------------------*/

void addStmtToIRSB ( IRSB* bb, IRStmt* st )
{
   Int i;
   if (bb->stmts_used == bb->stmts_size) {
      IRStmt** stmts2 = LibVEX_Alloc_inline(2 * bb->stmts_size * sizeof(IRStmt*));
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
         = LibVEX_Alloc_inline(new_size * sizeof(IRType));
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
IRType typeOfIRTemp ( const IRTypeEnv* env, IRTemp tmp )
{
   vassert(tmp >= 0);
   vassert(tmp < env->types_used);
   return env->types[tmp];
}

IRType typeOfIRConst ( const IRConst* con )
{
   switch (con->tag) {
      case Ico_U1:    return Ity_I1;
      case Ico_U8:    return Ity_I8;
      case Ico_U16:   return Ity_I16;
      case Ico_U32:   return Ity_I32;
      case Ico_U64:   return Ity_I64;
      case Ico_F32:   return Ity_F32;
      case Ico_F32i:  return Ity_F32;
      case Ico_F64:   return Ity_F64;
      case Ico_F64i:  return Ity_F64;
      case Ico_V128:  return Ity_V128;
      case Ico_V256:  return Ity_V256;
      default: vpanic("typeOfIRConst");
   }
}

void typeOfIRLoadGOp ( IRLoadGOp cvt,
                       /*OUT*/IRType* t_res, /*OUT*/IRType* t_arg )
{
   switch (cvt) {
      case ILGop_Ident64:
         *t_res = Ity_I64; *t_arg = Ity_I64; break;
      case ILGop_Ident32:
         *t_res = Ity_I32; *t_arg = Ity_I32; break;
      case ILGop_16Uto32: case ILGop_16Sto32:
         *t_res = Ity_I32; *t_arg = Ity_I16; break;
      case ILGop_8Uto32: case ILGop_8Sto32:
         *t_res = Ity_I32; *t_arg = Ity_I8; break;
      default:
         vpanic("typeOfIRLoadGOp");
   }
}

IRType typeOfIRExpr ( const IRTypeEnv* tyenv, const IRExpr* e )
{
   IRType t_dst, t_arg1, t_arg2, t_arg3, t_arg4;
 start:
   switch (e->tag) {
      case Iex_Load:
         return e->Iex.Load.ty;
      case Iex_Get:
         return e->Iex.Get.ty;
      case Iex_GetI:
         return e->Iex.GetI.descr->elemTy;
      case Iex_RdTmp:
         return typeOfIRTemp(tyenv, e->Iex.RdTmp.tmp);
      case Iex_Const:
         return typeOfIRConst(e->Iex.Const.con);
      case Iex_Qop:
         typeOfPrimop(e->Iex.Qop.details->op, 
                      &t_dst, &t_arg1, &t_arg2, &t_arg3, &t_arg4);
         return t_dst;
      case Iex_Triop:
         typeOfPrimop(e->Iex.Triop.details->op,
                      &t_dst, &t_arg1, &t_arg2, &t_arg3, &t_arg4);
         return t_dst;
      case Iex_Binop:
         typeOfPrimop(e->Iex.Binop.op, 
                      &t_dst, &t_arg1, &t_arg2, &t_arg3, &t_arg4);
         return t_dst;
      case Iex_Unop:
         typeOfPrimop(e->Iex.Unop.op, 
                      &t_dst, &t_arg1, &t_arg2, &t_arg3, &t_arg4);
         return t_dst;
      case Iex_CCall:
         return e->Iex.CCall.retty;
      case Iex_ITE:
         e = e->Iex.ITE.iffalse;
         goto start;
         /* return typeOfIRExpr(tyenv, e->Iex.ITE.iffalse); */
      case Iex_Binder:
         vpanic("typeOfIRExpr: Binder is not a valid expression");
      case Iex_VECRET:
         vpanic("typeOfIRExpr: VECRET is not a valid expression");
      case Iex_BBPTR:
         vpanic("typeOfIRExpr: BBPTR is not a valid expression");
      default:
         ppIRExpr(e);
         vpanic("typeOfIRExpr");
   }
}

/* Is this any value actually in the enumeration 'IRType' ? */
Bool isPlausibleIRType ( IRType ty )
{
   switch (ty) {
      case Ity_INVALID: case Ity_I1:
      case Ity_I8: case Ity_I16: case Ity_I32: 
      case Ity_I64: case Ity_I128:
      case Ity_F16: case Ity_F32: case Ity_F64: case Ity_F128:
      case Ity_D32: case Ity_D64: case Ity_D128:
      case Ity_V128: case Ity_V256:
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
      return e->tag == Iex_RdTmp || e->tag == Iex_Const;
   }
*/

static inline Bool isIRAtom_or_VECRET_or_BBPTR ( const IRExpr* e )
{
  if (isIRAtom(e)) {
    return True;
  }

  return UNLIKELY(is_IRExpr_VECRET_or_BBPTR(e));
}

Bool isFlatIRStmt ( const IRStmt* st )
{
   Int      i;
   const IRExpr*  e;
   const IRQop*   qop;
   const IRTriop* triop;

   switch (st->tag) {
      case Ist_AbiHint:
         return isIRAtom(st->Ist.AbiHint.base)
                && isIRAtom(st->Ist.AbiHint.nia);
      case Ist_Put:
         return isIRAtom(st->Ist.Put.data);
      case Ist_PutI: {
         const IRPutI *puti = st->Ist.PutI.details;
         return toBool( isIRAtom(puti->ix) 
                        && isIRAtom(puti->data) );
      }
      case Ist_WrTmp:
         /* This is the only interesting case.  The RHS can be any
            expression, *but* all its subexpressions *must* be
            atoms. */
         e = st->Ist.WrTmp.data;
         switch (e->tag) {
            case Iex_Binder: return True;
            case Iex_Get:    return True;
            case Iex_GetI:   return isIRAtom(e->Iex.GetI.ix);
            case Iex_RdTmp:  return True;
            case Iex_Qop:    qop = e->Iex.Qop.details;
                             return toBool(
                                    isIRAtom(qop->arg1) 
                                    && isIRAtom(qop->arg2)
                                    && isIRAtom(qop->arg3)
                                    && isIRAtom(qop->arg4));
            case Iex_Triop:  triop = e->Iex.Triop.details;
                             return toBool(
                                    isIRAtom(triop->arg1) 
                                    && isIRAtom(triop->arg2)
                                    && isIRAtom(triop->arg3));
            case Iex_Binop:  return toBool(
                                    isIRAtom(e->Iex.Binop.arg1) 
                                    && isIRAtom(e->Iex.Binop.arg2));
            case Iex_Unop:   return isIRAtom(e->Iex.Unop.arg);
            case Iex_Load:   return isIRAtom(e->Iex.Load.addr);
            case Iex_Const:  return True;
            case Iex_CCall:  for (i = 0; e->Iex.CCall.args[i]; i++)
                                if (!isIRAtom(e->Iex.CCall.args[i])) 
                                   return False;
                             return True;
            case Iex_ITE:    return toBool (
                                    isIRAtom(e->Iex.ITE.cond) 
                                    && isIRAtom(e->Iex.ITE.iftrue) 
                                    && isIRAtom(e->Iex.ITE.iffalse));
            default:         vpanic("isFlatIRStmt(e)");
         }
         /*notreached*/
         vassert(0);
      case Ist_Store:
         return toBool( isIRAtom(st->Ist.Store.addr) 
                        && isIRAtom(st->Ist.Store.data) );
      case Ist_StoreG: {
         const IRStoreG* sg = st->Ist.StoreG.details;
         return toBool( isIRAtom(sg->addr)
                        && isIRAtom(sg->data) && isIRAtom(sg->guard) );
      }
      case Ist_LoadG: {
         const IRLoadG* lg = st->Ist.LoadG.details;
         return toBool( isIRAtom(lg->addr)
                        && isIRAtom(lg->alt) && isIRAtom(lg->guard) );
      }
      case Ist_CAS: {
        const IRCAS* cas = st->Ist.CAS.details;
         return toBool( isIRAtom(cas->addr)
                        && (cas->expdHi ? isIRAtom(cas->expdHi) : True)
                        && isIRAtom(cas->expdLo)
                        && (cas->dataHi ? isIRAtom(cas->dataHi) : True)
                        && isIRAtom(cas->dataLo) );
      }
      case Ist_LLSC:
         return toBool( isIRAtom(st->Ist.LLSC.addr)
                        && (st->Ist.LLSC.storedata
                               ? isIRAtom(st->Ist.LLSC.storedata) : True) );
      case Ist_Dirty: {
         const IRDirty* di = st->Ist.Dirty.details;
         if (!isIRAtom(di->guard)) 
            return False;
         for (i = 0; di->args[i]; i++)
            if (!isIRAtom_or_VECRET_or_BBPTR(di->args[i])) 
               return False;
         if (di->mAddr && !isIRAtom(di->mAddr)) 
            return False;
         return True;
      }
      case Ist_NoOp:
      case Ist_IMark:
      case Ist_MBE:
         return True;
      case Ist_Exit:
         return isIRAtom(st->Ist.Exit.guard);
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
void sanityCheckFail ( const IRSB* bb, const IRStmt* stmt, const HChar* what )
{
   vex_printf("\nIR SANITY CHECK FAILURE\n\n");
   ppIRSB(bb);
   if (stmt) {
      vex_printf("\nIN STATEMENT:\n\n");
      ppIRStmt(stmt);
   }
   vex_printf("\n\nERROR = %s\n\n", what );
   vpanic("sanityCheckFail: exiting due to bad IR");
}

static Bool saneIRRegArray ( const IRRegArray* arr )
{
   if (arr->base < 0 || arr->base > 10000 /* somewhat arbitrary */)
      return False;
   if (arr->elemTy == Ity_I1)
      return False;
   if (arr->nElems <= 0 || arr->nElems > 500 /* somewhat arbitrary */)
      return False;
   return True;
}

static Bool saneIRCallee ( const IRCallee* cee )
{
   if (cee->name == NULL)
      return False;
   if (cee->addr == 0)
      return False;
   if (cee->regparms < 0 || cee->regparms > 3)
      return False;
   return True;
}

static Bool saneIRConst ( const IRConst* con )
{
   switch (con->tag) {
      case Ico_U1: 
         return toBool( con->Ico.U1 == True || con->Ico.U1 == False );
      default: 
         /* Is there anything we can meaningfully check?  I don't
            think so. */
         return True;
   }
}

/* Traverse a Stmt/Expr, inspecting IRTemp uses.  Report any out of
   range ones.  Report any which are read and for which the current
   def_count is zero. */

static
void useBeforeDef_Temp ( const IRSB* bb, const IRStmt* stmt, IRTemp tmp,
                         Int* def_counts )
{
   if (tmp < 0 || tmp >= bb->tyenv->types_used)
      sanityCheckFail(bb,stmt, "out of range Temp in IRExpr");
   if (def_counts[tmp] < 1)
      sanityCheckFail(bb,stmt, "IRTemp use before def in IRExpr");
}

static
void useBeforeDef_Expr ( const IRSB* bb, const IRStmt* stmt,
                         const IRExpr* expr, Int* def_counts )
{
   Int i;
   switch (expr->tag) {
      case Iex_Get: 
         break;
      case Iex_GetI:
         useBeforeDef_Expr(bb,stmt,expr->Iex.GetI.ix,def_counts);
         break;
      case Iex_RdTmp:
         useBeforeDef_Temp(bb,stmt,expr->Iex.RdTmp.tmp,def_counts);
         break;
      case Iex_Qop: {
         const IRQop* qop = expr->Iex.Qop.details;
         useBeforeDef_Expr(bb,stmt,qop->arg1,def_counts);
         useBeforeDef_Expr(bb,stmt,qop->arg2,def_counts);
         useBeforeDef_Expr(bb,stmt,qop->arg3,def_counts);
         useBeforeDef_Expr(bb,stmt,qop->arg4,def_counts);
         break;
      }
      case Iex_Triop: {
         const IRTriop* triop = expr->Iex.Triop.details;
         useBeforeDef_Expr(bb,stmt,triop->arg1,def_counts);
         useBeforeDef_Expr(bb,stmt,triop->arg2,def_counts);
         useBeforeDef_Expr(bb,stmt,triop->arg3,def_counts);
         break;
      }
      case Iex_Binop:
         useBeforeDef_Expr(bb,stmt,expr->Iex.Binop.arg1,def_counts);
         useBeforeDef_Expr(bb,stmt,expr->Iex.Binop.arg2,def_counts);
         break;
      case Iex_Unop:
         useBeforeDef_Expr(bb,stmt,expr->Iex.Unop.arg,def_counts);
         break;
      case Iex_Load:
         useBeforeDef_Expr(bb,stmt,expr->Iex.Load.addr,def_counts);
         break;
      case Iex_Const:
         break;
      case Iex_CCall:
         for (i = 0; expr->Iex.CCall.args[i]; i++) {
            const IRExpr* arg = expr->Iex.CCall.args[i];
            if (UNLIKELY(is_IRExpr_VECRET_or_BBPTR(arg))) {
               /* These aren't allowed in CCall lists.  Let's detect
                  and throw them out here, though, rather than
                  segfaulting a bit later on. */
               sanityCheckFail(bb,stmt, "IRExprP__* value in CCall arg list");
            } else {
               useBeforeDef_Expr(bb,stmt,arg,def_counts);
            }
         }
         break;
      case Iex_ITE:
         useBeforeDef_Expr(bb,stmt,expr->Iex.ITE.cond,def_counts);
         useBeforeDef_Expr(bb,stmt,expr->Iex.ITE.iftrue,def_counts);
         useBeforeDef_Expr(bb,stmt,expr->Iex.ITE.iffalse,def_counts);
         break;
      default:
         vpanic("useBeforeDef_Expr");
   }
}

static
void useBeforeDef_Stmt ( const IRSB* bb, const IRStmt* stmt, Int* def_counts )
{
   Int       i;
   const IRDirty*  d;
   const IRCAS*    cas;
   const IRPutI*   puti;
   const IRLoadG*  lg;
   const IRStoreG* sg;
   switch (stmt->tag) {
      case Ist_IMark:
         break;
      case Ist_AbiHint:
         useBeforeDef_Expr(bb,stmt,stmt->Ist.AbiHint.base,def_counts);
         useBeforeDef_Expr(bb,stmt,stmt->Ist.AbiHint.nia,def_counts);
         break;
      case Ist_Put:
         useBeforeDef_Expr(bb,stmt,stmt->Ist.Put.data,def_counts);
         break;
      case Ist_PutI:
         puti = stmt->Ist.PutI.details;
         useBeforeDef_Expr(bb,stmt,puti->ix,def_counts);
         useBeforeDef_Expr(bb,stmt,puti->data,def_counts);
         break;
      case Ist_WrTmp:
         useBeforeDef_Expr(bb,stmt,stmt->Ist.WrTmp.data,def_counts);
         break;
      case Ist_Store:
         useBeforeDef_Expr(bb,stmt,stmt->Ist.Store.addr,def_counts);
         useBeforeDef_Expr(bb,stmt,stmt->Ist.Store.data,def_counts);
         break;
      case Ist_StoreG:
         sg = stmt->Ist.StoreG.details;
         useBeforeDef_Expr(bb,stmt,sg->addr,def_counts);
         useBeforeDef_Expr(bb,stmt,sg->data,def_counts);
         useBeforeDef_Expr(bb,stmt,sg->guard,def_counts);
         break;
      case Ist_LoadG:
         lg = stmt->Ist.LoadG.details;
         useBeforeDef_Expr(bb,stmt,lg->addr,def_counts);
         useBeforeDef_Expr(bb,stmt,lg->alt,def_counts);
         useBeforeDef_Expr(bb,stmt,lg->guard,def_counts);
         break;
      case Ist_CAS:
         cas = stmt->Ist.CAS.details;
         useBeforeDef_Expr(bb,stmt,cas->addr,def_counts);
         if (cas->expdHi)
            useBeforeDef_Expr(bb,stmt,cas->expdHi,def_counts);
         useBeforeDef_Expr(bb,stmt,cas->expdLo,def_counts);
         if (cas->dataHi)
            useBeforeDef_Expr(bb,stmt,cas->dataHi,def_counts);
         useBeforeDef_Expr(bb,stmt,cas->dataLo,def_counts);
         break;
      case Ist_LLSC:
         useBeforeDef_Expr(bb,stmt,stmt->Ist.LLSC.addr,def_counts);
         if (stmt->Ist.LLSC.storedata != NULL)
            useBeforeDef_Expr(bb,stmt,stmt->Ist.LLSC.storedata,def_counts);
         break;
      case Ist_Dirty:
         d = stmt->Ist.Dirty.details;
         for (i = 0; d->args[i] != NULL; i++) {
            IRExpr* arg = d->args[i];
            if (UNLIKELY(is_IRExpr_VECRET_or_BBPTR(arg))) {
               /* This is ensured by isFlatIRStmt */
              ;
            } else {
               useBeforeDef_Expr(bb,stmt,arg,def_counts);
            }
         }
         if (d->mFx != Ifx_None)
            useBeforeDef_Expr(bb,stmt,d->mAddr,def_counts);
         break;
      case Ist_NoOp:
      case Ist_MBE:
         break;
      case Ist_Exit:
         useBeforeDef_Expr(bb,stmt,stmt->Ist.Exit.guard,def_counts);
         break;
      default: 
         vpanic("useBeforeDef_Stmt");
   }
}

static
void tcExpr ( const IRSB* bb, const IRStmt* stmt, const IRExpr* expr,
              IRType gWordTy )
{
   Int        i;
   IRType     t_dst, t_arg1, t_arg2, t_arg3, t_arg4;
   const IRTypeEnv* tyenv = bb->tyenv;
   switch (expr->tag) {
      case Iex_Get:
      case Iex_RdTmp:
         break;
      case Iex_GetI:
         tcExpr(bb,stmt, expr->Iex.GetI.ix, gWordTy );
         if (typeOfIRExpr(tyenv,expr->Iex.GetI.ix) != Ity_I32)
            sanityCheckFail(bb,stmt,"IRExpr.GetI.ix: not :: Ity_I32");
         if (!saneIRRegArray(expr->Iex.GetI.descr))
            sanityCheckFail(bb,stmt,"IRExpr.GetI.descr: invalid descr");
         break;
      case Iex_Qop: {
         IRType ttarg1, ttarg2, ttarg3, ttarg4;
         const IRQop* qop = expr->Iex.Qop.details;
         tcExpr(bb,stmt, qop->arg1, gWordTy );
         tcExpr(bb,stmt, qop->arg2, gWordTy );
         tcExpr(bb,stmt, qop->arg3, gWordTy );
         tcExpr(bb,stmt, qop->arg4, gWordTy );
         typeOfPrimop(qop->op, 
                      &t_dst, &t_arg1, &t_arg2, &t_arg3, &t_arg4);
         if (t_arg1 == Ity_INVALID || t_arg2 == Ity_INVALID 
             || t_arg3 == Ity_INVALID || t_arg4 == Ity_INVALID) {
            vex_printf(" op name: " );
            ppIROp(qop->op);
            vex_printf("\n");
            sanityCheckFail(bb,stmt,
               "Iex.Qop: wrong arity op\n"
               "... name of op precedes BB printout\n");
         }
         ttarg1 = typeOfIRExpr(tyenv, qop->arg1);
         ttarg2 = typeOfIRExpr(tyenv, qop->arg2);
         ttarg3 = typeOfIRExpr(tyenv, qop->arg3);
         ttarg4 = typeOfIRExpr(tyenv, qop->arg4);
         if (t_arg1 != ttarg1 || t_arg2 != ttarg2 
             || t_arg3 != ttarg3 || t_arg4 != ttarg4) {
            vex_printf(" op name: ");
            ppIROp(qop->op);
            vex_printf("\n");
            vex_printf(" op type is (");
            ppIRType(t_arg1);
            vex_printf(",");
            ppIRType(t_arg2);
            vex_printf(",");
            ppIRType(t_arg3);
            vex_printf(",");
            ppIRType(t_arg4);
            vex_printf(") -> ");
            ppIRType (t_dst);
            vex_printf("\narg tys are (");
            ppIRType(ttarg1);
            vex_printf(",");
            ppIRType(ttarg2);
            vex_printf(",");
            ppIRType(ttarg3);
            vex_printf(",");
            ppIRType(ttarg4);
            vex_printf(")\n");
            sanityCheckFail(bb,stmt,
               "Iex.Qop: arg tys don't match op tys\n"
               "... additional details precede BB printout\n");
         }
         break;
      }
      case Iex_Triop: {
         IRType ttarg1, ttarg2, ttarg3;
         const IRTriop *triop = expr->Iex.Triop.details;
         tcExpr(bb,stmt, triop->arg1, gWordTy );
         tcExpr(bb,stmt, triop->arg2, gWordTy );
         tcExpr(bb,stmt, triop->arg3, gWordTy );
         typeOfPrimop(triop->op, 
                      &t_dst, &t_arg1, &t_arg2, &t_arg3, &t_arg4);
         if (t_arg1 == Ity_INVALID || t_arg2 == Ity_INVALID 
             || t_arg3 == Ity_INVALID || t_arg4 != Ity_INVALID) {
            vex_printf(" op name: " );
            ppIROp(triop->op);
            vex_printf("\n");
            sanityCheckFail(bb,stmt,
               "Iex.Triop: wrong arity op\n"
               "... name of op precedes BB printout\n");
         }
         ttarg1 = typeOfIRExpr(tyenv, triop->arg1);
         ttarg2 = typeOfIRExpr(tyenv, triop->arg2);
         ttarg3 = typeOfIRExpr(tyenv, triop->arg3);
         if (t_arg1 != ttarg1 || t_arg2 != ttarg2 || t_arg3 != ttarg3) {
            vex_printf(" op name: ");
            ppIROp(triop->op);
            vex_printf("\n");
            vex_printf(" op type is (");
            ppIRType(t_arg1);
            vex_printf(",");
            ppIRType(t_arg2);
            vex_printf(",");
            ppIRType(t_arg3);
            vex_printf(") -> ");
            ppIRType (t_dst);
            vex_printf("\narg tys are (");
            ppIRType(ttarg1);
            vex_printf(",");
            ppIRType(ttarg2);
            vex_printf(",");
            ppIRType(ttarg3);
            vex_printf(")\n");
            sanityCheckFail(bb,stmt,
               "Iex.Triop: arg tys don't match op tys\n"
               "... additional details precede BB printout\n");
         }
         break;
      }
      case Iex_Binop: {
         IRType ttarg1, ttarg2;
         tcExpr(bb,stmt, expr->Iex.Binop.arg1, gWordTy );
         tcExpr(bb,stmt, expr->Iex.Binop.arg2, gWordTy );
         typeOfPrimop(expr->Iex.Binop.op, 
                      &t_dst, &t_arg1, &t_arg2, &t_arg3, &t_arg4);
         if (t_arg1 == Ity_INVALID || t_arg2 == Ity_INVALID 
             || t_arg3 != Ity_INVALID || t_arg4 != Ity_INVALID) {
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
         typeOfPrimop(expr->Iex.Unop.op, 
                      &t_dst, &t_arg1, &t_arg2, &t_arg3, &t_arg4);
         if (t_arg1 == Ity_INVALID || t_arg2 != Ity_INVALID
             || t_arg3 != Ity_INVALID || t_arg4 != Ity_INVALID)
            sanityCheckFail(bb,stmt,"Iex.Unop: wrong arity op");
         if (t_arg1 != typeOfIRExpr(tyenv, expr->Iex.Unop.arg))
            sanityCheckFail(bb,stmt,"Iex.Unop: arg ty doesn't match op ty");
         break;
      case Iex_Load:
         tcExpr(bb,stmt, expr->Iex.Load.addr, gWordTy);
         if (typeOfIRExpr(tyenv, expr->Iex.Load.addr) != gWordTy)
            sanityCheckFail(bb,stmt,"Iex.Load.addr: not :: guest word type");
         if (expr->Iex.Load.end != Iend_LE && expr->Iex.Load.end != Iend_BE)
            sanityCheckFail(bb,stmt,"Iex.Load.end: bogus endianness");
         break;
      case Iex_CCall:
         if (!saneIRCallee(expr->Iex.CCall.cee))
            sanityCheckFail(bb,stmt,"Iex.CCall.cee: bad IRCallee");
         if (expr->Iex.CCall.cee->regparms > countArgs(expr->Iex.CCall.args)) 
            sanityCheckFail(bb,stmt,"Iex.CCall.cee: #regparms > #args");
         for (i = 0; expr->Iex.CCall.args[i]; i++) {
            if (i >= 32)
               sanityCheckFail(bb,stmt,"Iex.CCall: > 32 args");
            IRExpr* arg = expr->Iex.CCall.args[i];
            if (UNLIKELY(is_IRExpr_VECRET_or_BBPTR(arg)))
               sanityCheckFail(bb,stmt,"Iex.CCall.args: is VECRET/BBPTR");
            tcExpr(bb,stmt, arg, gWordTy);
         }
         if (expr->Iex.CCall.retty == Ity_I1)
            sanityCheckFail(bb,stmt,"Iex.CCall.retty: cannot return :: Ity_I1");
         for (i = 0; expr->Iex.CCall.args[i]; i++)
            if (typeOfIRExpr(tyenv, expr->Iex.CCall.args[i]) == Ity_I1)
               sanityCheckFail(bb,stmt,"Iex.CCall.arg: arg :: Ity_I1");
         break;
      case Iex_Const:
         if (!saneIRConst(expr->Iex.Const.con))
            sanityCheckFail(bb,stmt,"Iex.Const.con: invalid const");
         break;
      case Iex_ITE:
         tcExpr(bb,stmt, expr->Iex.ITE.cond, gWordTy);
         tcExpr(bb,stmt, expr->Iex.ITE.iftrue, gWordTy);
         tcExpr(bb,stmt, expr->Iex.ITE.iffalse, gWordTy);
         if (typeOfIRExpr(tyenv, expr->Iex.ITE.cond) != Ity_I1)
            sanityCheckFail(bb,stmt,"Iex.ITE.cond: cond :: Ity_I1");
         if (typeOfIRExpr(tyenv, expr->Iex.ITE.iftrue)
             != typeOfIRExpr(tyenv, expr->Iex.ITE.iffalse))
            sanityCheckFail(bb,stmt,"Iex.ITE: iftrue/iffalse mismatch");
         break;
      default: 
         vpanic("tcExpr");
   }
}


static
void tcStmt ( const IRSB* bb, const IRStmt* stmt, IRType gWordTy )
{
   Int        i;
   IRType     tyExpd, tyData;
   const IRTypeEnv* tyenv = bb->tyenv;
   switch (stmt->tag) {
      case Ist_IMark:
         /* Somewhat heuristic, but rule out totally implausible
            instruction sizes and deltas. */
         if (stmt->Ist.IMark.len > 24)
            sanityCheckFail(bb,stmt,"IRStmt.IMark.len: implausible");
         if (stmt->Ist.IMark.delta > 1)
            sanityCheckFail(bb,stmt,"IRStmt.IMark.delta: implausible");
         break;
      case Ist_AbiHint:
         if (typeOfIRExpr(tyenv, stmt->Ist.AbiHint.base) != gWordTy)
            sanityCheckFail(bb,stmt,"IRStmt.AbiHint.base: "
                                    "not :: guest word type");
         if (typeOfIRExpr(tyenv, stmt->Ist.AbiHint.nia) != gWordTy)
            sanityCheckFail(bb,stmt,"IRStmt.AbiHint.nia: "
                                    "not :: guest word type");
         break;
      case Ist_Put:
         tcExpr( bb, stmt, stmt->Ist.Put.data, gWordTy );
         if (typeOfIRExpr(tyenv,stmt->Ist.Put.data) == Ity_I1)
            sanityCheckFail(bb,stmt,"IRStmt.Put.data: cannot Put :: Ity_I1");
         break;
      case Ist_PutI:{
         const IRPutI* puti = stmt->Ist.PutI.details;
         tcExpr( bb, stmt, puti->data, gWordTy );
         tcExpr( bb, stmt, puti->ix, gWordTy );
         if (typeOfIRExpr(tyenv,puti->data) == Ity_I1)
            sanityCheckFail(bb,stmt,"IRStmt.PutI.data: cannot PutI :: Ity_I1");
         if (typeOfIRExpr(tyenv,puti->data) 
             != puti->descr->elemTy)
            sanityCheckFail(bb,stmt,"IRStmt.PutI.data: data ty != elem ty");
         if (typeOfIRExpr(tyenv,puti->ix) != Ity_I32)
            sanityCheckFail(bb,stmt,"IRStmt.PutI.ix: not :: Ity_I32");
         if (!saneIRRegArray(puti->descr))
            sanityCheckFail(bb,stmt,"IRStmt.PutI.descr: invalid descr");
         break;
      }
      case Ist_WrTmp:
         tcExpr( bb, stmt, stmt->Ist.WrTmp.data, gWordTy );
         if (typeOfIRTemp(tyenv, stmt->Ist.WrTmp.tmp)
             != typeOfIRExpr(tyenv, stmt->Ist.WrTmp.data))
            sanityCheckFail(bb,stmt,
                            "IRStmt.Put.Tmp: tmp and expr do not match");
         break;
      case Ist_Store:
         tcExpr( bb, stmt, stmt->Ist.Store.addr, gWordTy );
         tcExpr( bb, stmt, stmt->Ist.Store.data, gWordTy );
         if (typeOfIRExpr(tyenv, stmt->Ist.Store.addr) != gWordTy)
            sanityCheckFail(bb,stmt,
                            "IRStmt.Store.addr: not :: guest word type");
         if (typeOfIRExpr(tyenv, stmt->Ist.Store.data) == Ity_I1)
            sanityCheckFail(bb,stmt,
                            "IRStmt.Store.data: cannot Store :: Ity_I1");
         if (stmt->Ist.Store.end != Iend_LE && stmt->Ist.Store.end != Iend_BE)
            sanityCheckFail(bb,stmt,"Ist.Store.end: bogus endianness");
         break;
      case Ist_StoreG: {
         const IRStoreG* sg = stmt->Ist.StoreG.details;
         tcExpr( bb, stmt, sg->addr, gWordTy );
         tcExpr( bb, stmt, sg->data, gWordTy );
         tcExpr( bb, stmt, sg->guard, gWordTy );
         if (typeOfIRExpr(tyenv, sg->addr) != gWordTy)
            sanityCheckFail(bb,stmt,"IRStmtG...addr: not :: guest word type");
         if (typeOfIRExpr(tyenv, sg->data) == Ity_I1)
            sanityCheckFail(bb,stmt,"IRStmtG...data: cannot Store :: Ity_I1");
         if (typeOfIRExpr(tyenv, sg->guard) != Ity_I1)
            sanityCheckFail(bb,stmt,"IRStmtG...guard: not :: Ity_I1");
         if (sg->end != Iend_LE && sg->end != Iend_BE)
            sanityCheckFail(bb,stmt,"IRStmtG...end: bogus endianness");
         break;
      }
      case Ist_LoadG: {
         const IRLoadG* lg = stmt->Ist.LoadG.details;
         tcExpr( bb, stmt, lg->addr, gWordTy );
         tcExpr( bb, stmt, lg->alt, gWordTy );
         tcExpr( bb, stmt, lg->guard, gWordTy );
         if (typeOfIRExpr(tyenv, lg->guard) != Ity_I1)
            sanityCheckFail(bb,stmt,"IRStmt.LoadG.guard: not :: Ity_I1");
         if (typeOfIRExpr(tyenv, lg->addr) != gWordTy)
              sanityCheckFail(bb,stmt,"IRStmt.LoadG.addr: not "
                                      ":: guest word type");
         if (typeOfIRExpr(tyenv, lg->alt) != typeOfIRTemp(tyenv, lg->dst))
             sanityCheckFail(bb,stmt,"IRStmt.LoadG: dst/alt type mismatch");
         IRTemp cvtRes = Ity_INVALID, cvtArg = Ity_INVALID;
         typeOfIRLoadGOp(lg->cvt, &cvtRes, &cvtArg);
         if (cvtRes != typeOfIRTemp(tyenv, lg->dst))
            sanityCheckFail(bb,stmt,"IRStmt.LoadG: dst/loaded type mismatch");
         break;
      }
      case Ist_CAS: {
         const IRCAS* cas = stmt->Ist.CAS.details;
         /* make sure it's definitely either a CAS or a DCAS */
         if (cas->oldHi == IRTemp_INVALID 
             && cas->expdHi == NULL && cas->dataHi == NULL) {
            /* fine; it's a single cas */
         }
         else
         if (cas->oldHi != IRTemp_INVALID 
             && cas->expdHi != NULL && cas->dataHi != NULL) {
            /* fine; it's a double cas */
         }
         else {
            /* it's some el-mutanto hybrid */
            goto bad_cas;
         }
         /* check the address type */
         tcExpr( bb, stmt, cas->addr, gWordTy );
         if (typeOfIRExpr(tyenv, cas->addr) != gWordTy) goto bad_cas;
         /* check types on the {old,expd,data}Lo components agree */
         tyExpd = typeOfIRExpr(tyenv, cas->expdLo);
         tyData = typeOfIRExpr(tyenv, cas->dataLo);
         if (tyExpd != tyData) goto bad_cas;
         if (tyExpd != typeOfIRTemp(tyenv, cas->oldLo))
            goto bad_cas;
         /* check the base element type is sane */
         if (tyExpd == Ity_I8 || tyExpd == Ity_I16 || tyExpd == Ity_I32
             || (gWordTy == Ity_I64 && tyExpd == Ity_I64)) {
            /* fine */
         } else {
            goto bad_cas;
         }
         /* If it's a DCAS, check types on the {old,expd,data}Hi
            components too */
         if (cas->oldHi != IRTemp_INVALID) {
            tyExpd = typeOfIRExpr(tyenv, cas->expdHi);
            tyData = typeOfIRExpr(tyenv, cas->dataHi);
            if (tyExpd != tyData) goto bad_cas;
            if (tyExpd != typeOfIRTemp(tyenv, cas->oldHi))
               goto bad_cas;
            /* and finally check that oldLo and oldHi have the same
               type.  This forces equivalence amongst all 6 types. */
            if (typeOfIRTemp(tyenv, cas->oldHi)
                != typeOfIRTemp(tyenv, cas->oldLo))
               goto bad_cas;
         }
         break;
         bad_cas:
         sanityCheckFail(bb,stmt,"IRStmt.CAS: ill-formed");
         break;
      }
      case Ist_LLSC: {
         IRType tyRes;
         if (typeOfIRExpr(tyenv, stmt->Ist.LLSC.addr) != gWordTy)
            sanityCheckFail(bb,stmt,"IRStmt.LLSC.addr: not :: guest word type");
         if (stmt->Ist.LLSC.end != Iend_LE && stmt->Ist.LLSC.end != Iend_BE)
            sanityCheckFail(bb,stmt,"Ist.LLSC.end: bogus endianness");
         tyRes = typeOfIRTemp(tyenv, stmt->Ist.LLSC.result);
         if (stmt->Ist.LLSC.storedata == NULL) {
            /* it's a LL */
            if (tyRes != Ity_I64 && tyRes != Ity_I32
                && tyRes != Ity_I16 && tyRes != Ity_I8)
               sanityCheckFail(bb,stmt,"Ist.LLSC(LL).result :: bogus");
         } else {
            /* it's a SC */
            if (tyRes != Ity_I1)
               sanityCheckFail(bb,stmt,"Ist.LLSC(SC).result: not :: Ity_I1");
            tyData = typeOfIRExpr(tyenv, stmt->Ist.LLSC.storedata);
            if (tyData != Ity_I64 && tyData != Ity_I32
                && tyData != Ity_I16 && tyData != Ity_I8)
               sanityCheckFail(bb,stmt,
                               "Ist.LLSC(SC).result :: storedata bogus");
         }
         break;
      }
      case Ist_Dirty: {
         /* Mostly check for various kinds of ill-formed dirty calls. */
         const IRDirty* d = stmt->Ist.Dirty.details;
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
         for (i = 0; i < d->nFxState; i++) {
            if (d->fxState[i].fx == Ifx_None) goto bad_dirty;
            if (d->fxState[i].size <= 0) goto bad_dirty;
            if (d->fxState[i].nRepeats == 0) {
               if (d->fxState[i].repeatLen != 0) goto bad_dirty;
            } else {
               if (d->fxState[i].repeatLen <= d->fxState[i].size)
                  goto bad_dirty;
               /* the % is safe because of the .size check above */
               if ((d->fxState[i].repeatLen % d->fxState[i].size) != 0)
                  goto bad_dirty;
            }
         }
         /* check guard */
         if (d->guard == NULL) goto bad_dirty;
         tcExpr( bb, stmt, d->guard, gWordTy );
         if (typeOfIRExpr(tyenv, d->guard) != Ity_I1)
            sanityCheckFail(bb,stmt,"IRStmt.Dirty.guard not :: Ity_I1");
         /* check types, minimally */
         IRType retTy = Ity_INVALID;
         if (d->tmp != IRTemp_INVALID) {
            retTy = typeOfIRTemp(tyenv, d->tmp);
            if (retTy == Ity_I1)
               sanityCheckFail(bb,stmt,"IRStmt.Dirty.dst :: Ity_I1");
         }
         UInt nVECRETs = 0, nBBPTRs = 0;
         for (i = 0; d->args[i] != NULL; i++) {
            if (i >= 32)
               sanityCheckFail(bb,stmt,"IRStmt.Dirty: > 32 args");
            const IRExpr* arg = d->args[i];
            if (UNLIKELY(arg->tag == Iex_VECRET)) {
               nVECRETs++;
            } else if (UNLIKELY(arg->tag == Iex_BBPTR)) {
               nBBPTRs++;
            } else {
               if (typeOfIRExpr(tyenv, arg) == Ity_I1)
                  sanityCheckFail(bb,stmt,"IRStmt.Dirty.arg[i] :: Ity_I1");
            }
            if (nBBPTRs > 1) {
               sanityCheckFail(bb,stmt,"IRStmt.Dirty.args: > 1 BBPTR arg");
            }
            if (nVECRETs == 1) {
               /* Fn must return V128 or V256. */
               if (retTy != Ity_V128 && retTy != Ity_V256)
                  sanityCheckFail(bb,stmt,
                                  "IRStmt.Dirty.args: VECRET present, "
                                  "but fn does not return V128 or V256");
            } else if (nVECRETs == 0) {
               /* Fn must not return V128 or V256 */
               if (retTy == Ity_V128 || retTy == Ity_V256)
                  sanityCheckFail(bb,stmt,
                                  "IRStmt.Dirty.args: VECRET not present, "
                                  "but fn returns V128 or V256");
            } else {
               sanityCheckFail(bb,stmt,
                               "IRStmt.Dirty.args: > 1 VECRET present");
            }
         }
         if (nBBPTRs > 1) {
            sanityCheckFail(bb,stmt,
                            "IRStmt.Dirty.args: > 1 BBPTR present");
         }
         /* If you ask for the baseblock pointer, you have to make
            some declaration about access to the guest state too. */
         if (d->nFxState == 0 && nBBPTRs != 0) {
            sanityCheckFail(bb,stmt,
                            "IRStmt.Dirty.args: BBPTR requested, "
                            "but no fxState declared");
         }
        break;
         bad_dirty:
         sanityCheckFail(bb,stmt,"IRStmt.Dirty: ill-formed");
         break;
      }
      case Ist_NoOp:
         break;
      case Ist_MBE:
         switch (stmt->Ist.MBE.event) {
            case Imbe_Fence: case Imbe_CancelReservation:
               break;
            default: sanityCheckFail(bb,stmt,"IRStmt.MBE.event: unknown");
               break;
         }
         break;
      case Ist_Exit:
         tcExpr( bb, stmt, stmt->Ist.Exit.guard, gWordTy );
         if (typeOfIRExpr(tyenv,stmt->Ist.Exit.guard) != Ity_I1)
            sanityCheckFail(bb,stmt,"IRStmt.Exit.guard: not :: Ity_I1");
         if (!saneIRConst(stmt->Ist.Exit.dst))
            sanityCheckFail(bb,stmt,"IRStmt.Exit.dst: bad dst");
         if (typeOfIRConst(stmt->Ist.Exit.dst) != gWordTy)
            sanityCheckFail(bb,stmt,"IRStmt.Exit.dst: not :: guest word type");
         /* because it would intersect with host_EvC_* */
         if (stmt->Ist.Exit.offsIP < 16)
            sanityCheckFail(bb,stmt,"IRStmt.Exit.offsIP: too low");
         break;
      default:
         vpanic("tcStmt");
   }
}

void sanityCheckIRSB ( const IRSB* bb, const HChar* caller,
                       Bool require_flat, IRType guest_word_size )
{
   Int     i;
   Int     n_temps    = bb->tyenv->types_used;
   Int*    def_counts = LibVEX_Alloc_inline(n_temps * sizeof(Int));

   if (0)
      vex_printf("sanityCheck: %s\n", caller);

   vassert(guest_word_size == Ity_I32
           || guest_word_size == Ity_I64);

   if (bb->stmts_used < 0 || bb->stmts_size < 8
       || bb->stmts_used > bb->stmts_size)
      /* this BB is so strange we can't even print it */
      vpanic("sanityCheckIRSB: stmts array limits wierd");

   /* Ensure each temp has a plausible type. */
   for (i = 0; i < n_temps; i++) {
      IRType ty = typeOfIRTemp(bb->tyenv,(IRTemp)i);
      if (!isPlausibleIRType(ty)) {
         vex_printf("Temp t%d declared with implausible type 0x%x\n",
                    i, (UInt)ty);
         sanityCheckFail(bb,NULL,"Temp declared with implausible type");
      }
   }

   const IRStmt* stmt;

   /* Check for flatness, if required. */
   if (require_flat) {
      for (i = 0; i < bb->stmts_used; i++) {
         stmt = bb->stmts[i];
         if (!stmt)
            sanityCheckFail(bb, stmt, "IRStmt: is NULL");
         if (!isFlatIRStmt(stmt))
            sanityCheckFail(bb, stmt, "IRStmt: is not flat");
      }
      if (!isIRAtom(bb->next))
         sanityCheckFail(bb, NULL, "bb->next is not an atom");
   }

   /* Count the defs of each temp.  Only one def is allowed.
      Also, check that each used temp has already been defd. */

   for (i = 0; i < n_temps; i++)
      def_counts[i] = 0;

   for (i = 0; i < bb->stmts_used; i++) {
      stmt = bb->stmts[i];
      /* Check any temps used by this statement. */
      useBeforeDef_Stmt(bb,stmt,def_counts);

      /* Now make note of any temps defd by this statement. */
      switch (stmt->tag) {
      case Ist_WrTmp:
         if (stmt->Ist.WrTmp.tmp < 0 || stmt->Ist.WrTmp.tmp >= n_temps)
            sanityCheckFail(bb, stmt, 
               "IRStmt.Tmp: destination tmp is out of range");
         def_counts[stmt->Ist.WrTmp.tmp]++;
         if (def_counts[stmt->Ist.WrTmp.tmp] > 1)
            sanityCheckFail(bb, stmt, 
               "IRStmt.Tmp: destination tmp is assigned more than once");
         break;
      case Ist_LoadG: {
         const IRLoadG* lg = stmt->Ist.LoadG.details;
         if (lg->dst < 0 || lg->dst >= n_temps)
             sanityCheckFail(bb, stmt, 
                "IRStmt.LoadG: destination tmp is out of range");
         def_counts[lg->dst]++;
         if (def_counts[lg->dst] > 1)
             sanityCheckFail(bb, stmt, 
                "IRStmt.LoadG: destination tmp is assigned more than once");
         break;
      }
      case Ist_Dirty: {
         const IRDirty* d = stmt->Ist.Dirty.details;
         if (d->tmp != IRTemp_INVALID) {
            if (d->tmp < 0 || d->tmp >= n_temps)
               sanityCheckFail(bb, stmt, 
                  "IRStmt.Dirty: destination tmp is out of range");
            def_counts[d->tmp]++;
            if (def_counts[d->tmp] > 1)
               sanityCheckFail(bb, stmt, 
                  "IRStmt.Dirty: destination tmp is assigned more than once");
         }
         break;
      }
      case Ist_CAS: {
         const IRCAS* cas = stmt->Ist.CAS.details;
         if (cas->oldHi != IRTemp_INVALID) {
            if (cas->oldHi < 0 || cas->oldHi >= n_temps)
                sanityCheckFail(bb, stmt, 
                   "IRStmt.CAS: destination tmpHi is out of range");
             def_counts[cas->oldHi]++;
             if (def_counts[cas->oldHi] > 1)
                sanityCheckFail(bb, stmt, 
                   "IRStmt.CAS: destination tmpHi is assigned more than once");
         }
         if (cas->oldLo < 0 || cas->oldLo >= n_temps)
            sanityCheckFail(bb, stmt, 
               "IRStmt.CAS: destination tmpLo is out of range");
         def_counts[cas->oldLo]++;
         if (def_counts[cas->oldLo] > 1)
            sanityCheckFail(bb, stmt, 
               "IRStmt.CAS: destination tmpLo is assigned more than once");
         break;
      }
      case Ist_LLSC:
         if (stmt->Ist.LLSC.result < 0 || stmt->Ist.LLSC.result >= n_temps)
            sanityCheckFail(bb, stmt,
               "IRStmt.LLSC: destination tmp is out of range");
         def_counts[stmt->Ist.LLSC.result]++;
         if (def_counts[stmt->Ist.LLSC.result] > 1)
            sanityCheckFail(bb, stmt,
               "IRStmt.LLSC: destination tmp is assigned more than once");
         break;
      default:
         /* explicitly handle the rest, so as to keep gcc quiet */
         break;
      }
   }

   /* Typecheck everything. */
   for (i = 0; i < bb->stmts_used; i++)
      if (bb->stmts[i])
         tcStmt( bb, bb->stmts[i], guest_word_size );
   if (typeOfIRExpr(bb->tyenv,bb->next) != guest_word_size)
      sanityCheckFail(bb, NULL, "bb->next field has wrong type");
   /* because it would intersect with host_EvC_* */
   if (bb->offsIP < 16)
      sanityCheckFail(bb, NULL, "bb->offsIP: too low");
}

/*---------------------------------------------------------------*/
/*--- Misc helper functions                                   ---*/
/*---------------------------------------------------------------*/

Bool eqIRConst ( const IRConst* c1, const IRConst* c2 )
{
   if (c1->tag != c2->tag)
      return False;

   switch (c1->tag) {
      case Ico_U1:  return toBool( (1 & c1->Ico.U1) == (1 & c2->Ico.U1) );
      case Ico_U8:  return toBool( c1->Ico.U8  == c2->Ico.U8 );
      case Ico_U16: return toBool( c1->Ico.U16 == c2->Ico.U16 );
      case Ico_U32: return toBool( c1->Ico.U32 == c2->Ico.U32 );
      case Ico_U64: return toBool( c1->Ico.U64 == c2->Ico.U64 );
      case Ico_F32: return toBool( c1->Ico.F32 == c2->Ico.F32 );
      case Ico_F32i: return toBool( c1->Ico.F32i == c2->Ico.F32i );
      case Ico_F64: return toBool( c1->Ico.F64 == c2->Ico.F64 );
      case Ico_F64i: return toBool( c1->Ico.F64i == c2->Ico.F64i );
      case Ico_V128: return toBool( c1->Ico.V128 == c2->Ico.V128 );
      case Ico_V256: return toBool( c1->Ico.V256 == c2->Ico.V256 );
      default: vpanic("eqIRConst");
   }
}

Bool eqIRRegArray ( const IRRegArray* descr1, const IRRegArray* descr2 )
{
   return toBool( descr1->base == descr2->base 
                  && descr1->elemTy == descr2->elemTy
                  && descr1->nElems == descr2->nElems );
}

Int sizeofIRType ( IRType ty )
{
   switch (ty) {
      case Ity_I8:   return 1;
      case Ity_I16:  return 2;
      case Ity_I32:  return 4;
      case Ity_I64:  return 8;
      case Ity_I128: return 16;
      case Ity_F16:  return 2;
      case Ity_F32:  return 4;
      case Ity_F64:  return 8;
      case Ity_F128: return 16;
      case Ity_D32:  return 4;
      case Ity_D64:  return 8;
      case Ity_D128: return 16;
      case Ity_V128: return 16;
      case Ity_V256: return 32;
      default: vex_printf("\n"); ppIRType(ty); vex_printf("\n");
               vpanic("sizeofIRType");
   }
}

IRType integerIRTypeOfSize ( Int szB )
{
   switch (szB) {
      case 8: return Ity_I64;
      case 4: return Ity_I32;
      case 2: return Ity_I16;
      case 1: return Ity_I8;
      default: vpanic("integerIRTypeOfSize");
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

IRDirty* unsafeIRDirty_0_N ( Int regparms, const HChar* name, void* addr, 
                             IRExpr** args ) 
{
   IRDirty* d = emptyIRDirty();
   d->cee   = mkIRCallee ( regparms, name, addr );
   d->guard = IRExpr_Const(IRConst_U1(True));
   d->args  = args;
   return d;
}

IRDirty* unsafeIRDirty_1_N ( IRTemp dst, 
                             Int regparms, const HChar* name, void* addr, 
                             IRExpr** args ) 
{
   IRDirty* d = emptyIRDirty();
   d->cee   = mkIRCallee ( regparms, name, addr );
   d->guard = IRExpr_Const(IRConst_U1(True));
   d->args  = args;
   d->tmp   = dst;
   return d;
}

IRExpr* mkIRExprCCall ( IRType retty,
                        Int regparms, const HChar* name, void* addr, 
                        IRExpr** args )
{
   return IRExpr_CCall ( mkIRCallee ( regparms, name, addr ), 
                         retty, args );
}

Bool eqIRAtom ( const IRExpr* a1, const IRExpr* a2 )
{
   vassert(isIRAtom(a1));
   vassert(isIRAtom(a2));
   if (a1->tag == Iex_RdTmp && a2->tag == Iex_RdTmp)
      return toBool(a1->Iex.RdTmp.tmp == a2->Iex.RdTmp.tmp);
   if (a1->tag == Iex_Const && a2->tag == Iex_Const)
      return eqIRConst(a1->Iex.Const.con, a2->Iex.Const.con);
   return False;
}

/*---------------------------------------------------------------*/
/*--- end                                           ir_defs.c ---*/
/*---------------------------------------------------------------*/
