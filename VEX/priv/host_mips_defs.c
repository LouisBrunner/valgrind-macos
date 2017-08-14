
/*---------------------------------------------------------------*/
/*--- begin                                  host_mips_defs.c ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2010-2012 RT-RK
      mips-valgrind@rt-rk.com

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
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307, USA.

   The GNU General Public License is contained in the file COPYING.
*/

#include "libvex_basictypes.h"
#include "libvex.h"
#include "libvex_trc_values.h"

#include "main_util.h"
#include "host_generic_regs.h"
#include "host_mips_defs.h"

/*---------------- Registers ----------------*/

void ppHRegMIPS(HReg reg, Bool mode64)
{
   Int r;
   static HChar *ireg32_names[35]
       = { "$0", "$1", "$2", "$3", "$4", "$5", "$6", "$7",
      "$8", "$9", "$10", "$11", "$12", "$13", "$14", "$15",
      "$16", "$17", "$18", "$19", "$20", "$21", "$22", "$23",
      "$24", "$25", "$26", "$27", "$28", "$29", "$30", "$31",
      "%32", "%33", "%34",
   };

   static HChar *freg32_names[32]
       = { "$f0", "$f1", "$f2", "$f3", "$f4", "$f5", "$f6", "$f7",
      "$f8", "$f9", "$f10", "$f11", "$f12", "$f13", "$f14", "$f15",
      "$f16", "$f17", "$f18", "$f19", "$f20", "$f21", "$f22", "$f23",
      "$f24", "$f25", "$f26", "$f27", "$f28", "$f29", "f30", "$f31"
   };

   static HChar *freg64_names[32]
       = { "$d0", "$d1", "$d2", "$d3", "$d4", "$d5", "$d6", "$d7",
      "$d8", "$d9", "$d10", "$d11", "$d12", "$d13", "$d14", "$d15",
   };

   /* Be generic for all virtual regs. */
   if (hregIsVirtual(reg)) {
      ppHReg(reg);
      return;
   }

   /* But specific for real regs. */
   vassert(hregClass(reg) == HRcInt32 || hregClass(reg) == HRcInt64 ||
      hregClass(reg) == HRcFlt32 || hregClass(reg) == HRcFlt64);

   /* But specific for real regs. */
   {
      switch (hregClass(reg)) {
         case HRcInt32:
            r = hregNumber(reg);
            vassert(r >= 0 && r < 32);
            vex_printf("%s", ireg32_names[r]);
            return;
         case HRcFlt32:
            r = hregNumber(reg);
            vassert(r >= 0 && r < 32);
            vex_printf("%s", freg32_names[r]);
            return;
         case HRcFlt64:
            r = hregNumber(reg);
            vassert(r >= 0 && r < 32);
            vex_printf("%s", freg64_names[r]);
            return;
         default:
            vpanic("ppHRegMIPS");
            break;
      }
   }

   return;
}

#define MkHRegGPR(_n, _mode64) \
   mkHReg(_n, _mode64 ? HRcInt64 : HRcInt32, False)

HReg hregMIPS_GPR0(Bool mode64)
{
   return MkHRegGPR(0, mode64);
}

HReg hregMIPS_GPR1(Bool mode64)
{
   return MkHRegGPR(1, mode64);
}

HReg hregMIPS_GPR2(Bool mode64)
{
   return MkHRegGPR(2, mode64);
}

HReg hregMIPS_GPR3(Bool mode64)
{
   return MkHRegGPR(3, mode64);
}

HReg hregMIPS_GPR4(Bool mode64)
{
   return MkHRegGPR(4, mode64);
}

HReg hregMIPS_GPR5(Bool mode64)
{
   return MkHRegGPR(5, mode64);
}

HReg hregMIPS_GPR6(Bool mode64)
{
   return MkHRegGPR(6, mode64);
}

HReg hregMIPS_GPR7(Bool mode64)
{
   return MkHRegGPR(7, mode64);
}

HReg hregMIPS_GPR8(Bool mode64)
{
   return MkHRegGPR(8, mode64);
}

HReg hregMIPS_GPR9(Bool mode64)
{
   return MkHRegGPR(9, mode64);
}

HReg hregMIPS_GPR10(Bool mode64)
{
   return MkHRegGPR(10, mode64);
}

HReg hregMIPS_GPR11(Bool mode64)
{
   return MkHRegGPR(11, mode64);
}

HReg hregMIPS_GPR12(Bool mode64)
{
   return MkHRegGPR(12, mode64);
}

HReg hregMIPS_GPR13(Bool mode64)
{
   return MkHRegGPR(13, mode64);
}

HReg hregMIPS_GPR14(Bool mode64)
{
   return MkHRegGPR(14, mode64);
}

HReg hregMIPS_GPR15(Bool mode64)
{
   return MkHRegGPR(15, mode64);
}

HReg hregMIPS_GPR16(Bool mode64)
{
   return MkHRegGPR(16, mode64);
}

HReg hregMIPS_GPR17(Bool mode64)
{
   return MkHRegGPR(17, mode64);
}

HReg hregMIPS_GPR18(Bool mode64)
{
   return MkHRegGPR(18, mode64);
}

HReg hregMIPS_GPR19(Bool mode64)
{
   return MkHRegGPR(19, mode64);
}

HReg hregMIPS_GPR20(Bool mode64)
{
   return MkHRegGPR(20, mode64);
}

HReg hregMIPS_GPR21(Bool mode64)
{
   return MkHRegGPR(21, mode64);
}

HReg hregMIPS_GPR22(Bool mode64)
{
   return MkHRegGPR(22, mode64);
}

HReg hregMIPS_GPR23(Bool mode64)
{
   return MkHRegGPR(23, mode64);
}

HReg hregMIPS_GPR24(Bool mode64)
{
   return MkHRegGPR(24, mode64);
}

HReg hregMIPS_GPR25(Bool mode64)
{
   return MkHRegGPR(25, mode64);
}

HReg hregMIPS_GPR26(Bool mode64)
{
   return MkHRegGPR(26, mode64);
}

HReg hregMIPS_GPR27(Bool mode64)
{
   return MkHRegGPR(27, mode64);
}

HReg hregMIPS_GPR28(Bool mode64)
{
   return MkHRegGPR(28, mode64);
}

HReg hregMIPS_GPR29(Bool mode64)
{
   return MkHRegGPR(29, mode64);
}

HReg hregMIPS_GPR30(Bool mode64)
{
   return MkHRegGPR(30, mode64);
}

HReg hregMIPS_GPR31(Bool mode64)
{
   return MkHRegGPR(31, mode64);
}

#define MkHRegFPR(_n, _mode64) \
   mkHReg(_n, _mode64 ? HRcFlt64 : HRcFlt32, False)

HReg hregMIPS_F0(Bool mode64)
{
   return MkHRegFPR(0, mode64);
}

HReg hregMIPS_F1(Bool mode64)
{
   return MkHRegFPR(1, mode64);
}

HReg hregMIPS_F2(Bool mode64)
{
   return MkHRegFPR(2, mode64);
}

HReg hregMIPS_F3(Bool mode64)
{
   return MkHRegFPR(3, mode64);
}

HReg hregMIPS_F4(Bool mode64)
{
   return MkHRegFPR(4, mode64);
}

HReg hregMIPS_F5(Bool mode64)
{
   return MkHRegFPR(5, mode64);
}

HReg hregMIPS_F6(Bool mode64)
{
   return MkHRegFPR(6, mode64);
}

HReg hregMIPS_F7(Bool mode64)
{
   return MkHRegFPR(7, mode64);
}

HReg hregMIPS_F8(Bool mode64)
{
   return MkHRegFPR(8, mode64);
}

HReg hregMIPS_F9(Bool mode64)
{
   return MkHRegFPR(9, mode64);
}

HReg hregMIPS_F10(Bool mode64)
{
   return MkHRegFPR(10, mode64);
}

HReg hregMIPS_F11(Bool mode64)
{
   return MkHRegFPR(11, mode64);
}

HReg hregMIPS_F12(Bool mode64)
{
   return MkHRegFPR(12, mode64);
}

HReg hregMIPS_F13(Bool mode64)
{
   return MkHRegFPR(13, mode64);
}

HReg hregMIPS_F14(Bool mode64)
{
   return MkHRegFPR(14, mode64);
}

HReg hregMIPS_F15(Bool mode64)
{
   return MkHRegFPR(15, mode64);
}

HReg hregMIPS_F16(Bool mode64)
{
   return MkHRegFPR(16, mode64);
}

HReg hregMIPS_F17(Bool mode64)
{
   return MkHRegFPR(17, mode64);
}

HReg hregMIPS_F18(Bool mode64)
{
   return MkHRegFPR(18, mode64);
}

HReg hregMIPS_F19(Bool mode64)
{
   return MkHRegFPR(19, mode64);
}

HReg hregMIPS_F20(Bool mode64)
{
   return MkHRegFPR(20, mode64);
}

HReg hregMIPS_F21(Bool mode64)
{
   return MkHRegFPR(21, mode64);
}

HReg hregMIPS_F22(Bool mode64)
{
   return MkHRegFPR(22, mode64);
}

HReg hregMIPS_F23(Bool mode64)
{
   return MkHRegFPR(23, mode64);
}

HReg hregMIPS_F24(Bool mode64)
{
   return MkHRegFPR(24, mode64);
}

HReg hregMIPS_F25(Bool mode64)
{
   return MkHRegFPR(25, mode64);
}

HReg hregMIPS_F26(Bool mode64)
{
   return MkHRegFPR(26, mode64);
}

HReg hregMIPS_F27(Bool mode64)
{
   return MkHRegFPR(27, mode64);
}

HReg hregMIPS_F28(Bool mode64)
{
   return MkHRegFPR(28, mode64);
}

HReg hregMIPS_F29(Bool mode64)
{
   return MkHRegFPR(29, mode64);
}

HReg hregMIPS_F30(Bool mode64)
{
   return MkHRegFPR(30, mode64);
}

HReg hregMIPS_F31(Bool mode64)
{
   return MkHRegFPR(31, mode64);
}

HReg hregMIPS_PC(Bool mode64)
{
   return mkHReg(32, mode64 ? HRcFlt64 : HRcFlt32, False);
}

HReg hregMIPS_HI(Bool mode64)
{
   return mkHReg(33, mode64 ? HRcFlt64 : HRcFlt32, False);
}

HReg hregMIPS_LO(Bool mode64)
{
   return mkHReg(34, mode64 ? HRcFlt64 : HRcFlt32, False);
}

HReg hregMIPS_D0(void)
{
   return mkHReg(0, HRcFlt64, False);
}

HReg hregMIPS_D1(void)
{
   return mkHReg(2, HRcFlt64, False);
}

HReg hregMIPS_D2(void)
{
   return mkHReg(4, HRcFlt64, False);
}

HReg hregMIPS_D3(void)
{
   return mkHReg(6, HRcFlt64, False);
}

HReg hregMIPS_D4(void)
{
   return mkHReg(8, HRcFlt64, False);
}

HReg hregMIPS_D5(void)
{
   return mkHReg(10, HRcFlt64, False);
}

HReg hregMIPS_D6(void)
{
   return mkHReg(12, HRcFlt64, False);
}

HReg hregMIPS_D7(void)
{
   return mkHReg(14, HRcFlt64, False);
}

HReg hregMIPS_D8(void)
{
   return mkHReg(16, HRcFlt64, False);
}

HReg hregMIPS_D9(void)
{
   return mkHReg(18, HRcFlt64, False);
}

HReg hregMIPS_D10(void)
{
   return mkHReg(20, HRcFlt64, False);
}

HReg hregMIPS_D11(void)
{
   return mkHReg(22, HRcFlt64, False);
}

HReg hregMIPS_D12(void)
{
   return mkHReg(24, HRcFlt64, False);
}

HReg hregMIPS_D13(void)
{
   return mkHReg(26, HRcFlt64, False);
}

HReg hregMIPS_D14(void)
{
   return mkHReg(28, HRcFlt64, False);
}

HReg hregMIPS_D15(void)
{
   return mkHReg(30, HRcFlt64, False);
}

HReg hregMIPS_FIR(void)
{
   return mkHReg(35, HRcInt32, False);
}

HReg hregMIPS_FCCR(void)
{
   return mkHReg(36, HRcInt32, False);
}

HReg hregMIPS_FEXR(void)
{
   return mkHReg(37, HRcInt32, False);
}

HReg hregMIPS_FENR(void)
{
   return mkHReg(38, HRcInt32, False);
}

HReg hregMIPS_FCSR(void)
{
   return mkHReg(39, HRcInt32, False);
}

HReg hregMIPS_COND(void)
{
   return mkHReg(47, HRcInt32, False);
}

void getAllocableRegs_MIPS(Int * nregs, HReg ** arr, Bool mode64)
{
   if (mode64)
      *nregs = 27;
   else
      *nregs = 34;
   UInt i = 0;
   *arr = LibVEX_Alloc(*nregs * sizeof(HReg));

   //ZERO = constant 0
   //AT = assembler temporary
   // callee saves ones are listed first, since we prefer them
   // if they're available
   (*arr)[i++] = hregMIPS_GPR16(mode64);
   (*arr)[i++] = hregMIPS_GPR17(mode64);
   (*arr)[i++] = hregMIPS_GPR18(mode64);
   (*arr)[i++] = hregMIPS_GPR19(mode64);
   (*arr)[i++] = hregMIPS_GPR20(mode64);
   (*arr)[i++] = hregMIPS_GPR21(mode64);
   (*arr)[i++] = hregMIPS_GPR22(mode64);
   if (!mode64)
      (*arr)[i++] = hregMIPS_GPR23(mode64);

   // otherwise we'll have to slum it out with caller-saves ones
   if (mode64) {
      (*arr)[i++] = hregMIPS_GPR8(mode64);
      (*arr)[i++] = hregMIPS_GPR9(mode64);
      (*arr)[i++] = hregMIPS_GPR10(mode64);
      (*arr)[i++] = hregMIPS_GPR11(mode64);
   }
   (*arr)[i++] = hregMIPS_GPR12(mode64);
   (*arr)[i++] = hregMIPS_GPR13(mode64);
   (*arr)[i++] = hregMIPS_GPR14(mode64);
   (*arr)[i++] = hregMIPS_GPR15(mode64);
   (*arr)[i++] = hregMIPS_GPR24(mode64);
    /***********mips32********************/
   // t0  (=dispatch_ctr)
   // t1  spill reg temp
   // t2  (=guest_state)
   // t3  (=PC = next guest address)
   // K0 and K1 are reserved for OS kernel
   // GP = global pointer
   // SP = stack pointer
   // FP = frame pointer
   // RA = link register
   // + PC, HI and LO
   (*arr)[i++] = hregMIPS_F20(mode64);
   (*arr)[i++] = hregMIPS_F21(mode64);
   (*arr)[i++] = hregMIPS_F22(mode64);
   (*arr)[i++] = hregMIPS_F23(mode64);
   (*arr)[i++] = hregMIPS_F24(mode64);
   (*arr)[i++] = hregMIPS_F25(mode64);
   (*arr)[i++] = hregMIPS_F26(mode64);
   (*arr)[i++] = hregMIPS_F27(mode64);
   (*arr)[i++] = hregMIPS_F28(mode64);
   (*arr)[i++] = hregMIPS_F29(mode64);
   (*arr)[i++] = hregMIPS_F30(mode64);
   if (!mode64) {
      /* Fake double floating point */
      (*arr)[i++] = hregMIPS_D0();
      (*arr)[i++] = hregMIPS_D1();
      (*arr)[i++] = hregMIPS_D2();
      (*arr)[i++] = hregMIPS_D3();
      (*arr)[i++] = hregMIPS_D4();
      (*arr)[i++] = hregMIPS_D5();
      (*arr)[i++] = hregMIPS_D6();
      (*arr)[i++] = hregMIPS_D7();
      (*arr)[i++] = hregMIPS_D8();
      (*arr)[i++] = hregMIPS_D9();
   }
   vassert(i == *nregs);

}

/*----------------- Condition Codes ----------------------*/

HChar *showMIPSCondCode(MIPSCondCode cond)
{
   HChar* ret;
   switch (cond) {
      case MIPScc_EQ:
         ret = "EQ"; /* equal */
         break;
      case MIPScc_NE:
         ret = "NEQ";   /* not equal */
         break;
      case MIPScc_HS:
         ret = "GE";   /* >=u (Greater Than or Equal) */
         break;
      case MIPScc_LO:
         ret = "LT";   /* <u  (lower) */
         break;
      case MIPScc_MI:
         ret = "mi";   /* minus (negative) */
         break;
      case MIPScc_PL:
         ret = "pl";   /* plus (zero or +ve) */
         break;
      case MIPScc_VS:
         ret = "vs";   /* overflow */
         break;
      case MIPScc_VC:
         ret = "vc";   /* no overflow */
         break;
      case MIPScc_HI:
         ret = "hi";   /* >u   (higher) */
         break;
      case MIPScc_LS:
         ret = "ls";   /* <=u  (lower or same) */
         break;
      case MIPScc_GE:
         ret = "ge";   /* >=s (signed greater or equal) */
         break;
      case MIPScc_LT:
         ret = "lt";   /* <s  (signed less than) */
         break;
      case MIPScc_GT:
         ret = "gt";   /* >s  (signed greater) */
         break;
      case MIPScc_LE:
         ret = "le";   /* <=s (signed less or equal) */
         break;
      case MIPScc_AL:
         ret = "al";   /* always (unconditional) */
         break;
      case MIPScc_NV:
         ret = "nv";   /* never (unconditional): */
         break;
      default:
         vpanic("showMIPSCondCode");
         break;
   }
   return ret;
}

HChar *showMIPSFpOp(MIPSFpOp op)
{
   HChar *ret;
   switch (op) {
      case Mfp_ADDD:
         ret = "ADD.D";
         break;
      case Mfp_SUBD:
         ret = "SUB.D";
         break;
      case Mfp_MULD:
         ret = "MUL.D";
         break;
      case Mfp_DIVD:
         ret = "DIV.D";
         break;
      case Mfp_MADDD:
         ret = "MADD.D";
         break;
      case Mfp_MSUBD:
         ret = "MSUB.D";
         break;
      case Mfp_MADDS:
         ret = "MADD.S";
         break;
      case Mfp_MSUBS:
         ret = "MSUB.S";
         break;
      case Mfp_ADDS:
         ret = "ADD.S";
         break;
      case Mfp_SUBS:
         ret = "SUB.S";
         break;
      case Mfp_MULS:
         ret = "MUL.S";
         break;
      case Mfp_DIVS:
         ret = "DIV.S";
         break;
      case Mfp_SQRTS:
         ret = "SQRT.S";
         break;
      case Mfp_SQRTD:
         ret = "SQRT.D";
         break;
      case Mfp_RSQRTS:
         ret = "RSQRT.S";
         break;
      case Mfp_RSQRTD:
         ret = "RSQRT.D";
         break;
      case Mfp_RECIPS:
         ret = "RECIP.S";
         break;
      case Mfp_RECIPD:
         ret = "RECIP.D";
         break;
      case Mfp_ABSS:
         ret = "ABS.S";
         break;
      case Mfp_ABSD:
         ret = "ABS.D";
         break;
      case Mfp_NEGS:
         ret = "NEG.S";
         break;
      case Mfp_NEGD:
         ret = "NEG.D";
         break;
      case Mfp_MOVS:
         ret = "MOV.S";
         break;
      case Mfp_MOVD:
         ret = "MOV.D";
         break;
      case Mfp_RES:
         ret = "RES";
         break;
      case Mfp_ROUNDWS:
         ret = "ROUND.W.S";
         break;
      case Mfp_ROUNDWD:
         ret = "ROUND.W.D";
         break;
      case Mfp_FLOORWS:
         ret = "FLOOR.W.S";
         break;
      case Mfp_FLOORWD:
         ret = "FLOOR.W.D";
         break;
      case Mfp_RSQRTE:
         ret = "frsqrte";
         break;
      case Mfp_CVTDW:
      case Mfp_CVTD:
         ret = "CVT.D";
         break;
      case Mfp_CVTSD:
      case Mfp_CVTSW:
         ret = "CVT.S";
         break;
      case Mfp_CVTWS:
      case Mfp_CVTWD:
         ret = "CVT.W";
         break;
      case Mfp_TRUWD:
      case Mfp_TRUWS:
         ret = "TRUNC.W";
         break;
      case Mfp_TRULD:
      case Mfp_TRULS:
         ret = "TRUNC.L";
         break;
      case Mfp_CEILWS:
      case Mfp_CEILWD:
         ret = "CEIL.W";
         break;
      case Mfp_CEILLS:
      case Mfp_CEILLD:
         ret = "CEIL.L";
         break;
      case Mfp_CMP:
         ret = "C.cond.d";
         break;
      default:
         vpanic("showMIPSFpOp");
         break;
   }
   return ret;
}

/* --------- MIPSAMode: memory address expressions. --------- */

MIPSAMode *MIPSAMode_IR(Int idx, HReg base)
{
   MIPSAMode *am = LibVEX_Alloc(sizeof(MIPSAMode));
   am->tag = Mam_IR;
   am->Mam.IR.base = base;
   am->Mam.IR.index = idx;

   return am;
}

MIPSAMode *MIPSAMode_RR(HReg idx, HReg base)
{
   MIPSAMode *am = LibVEX_Alloc(sizeof(MIPSAMode));
   am->tag = Mam_RR;
   am->Mam.RR.base = base;
   am->Mam.RR.index = idx;

   return am;
}

MIPSAMode *dopyMIPSAMode(MIPSAMode * am)
{
   MIPSAMode* ret;
   switch (am->tag) {
      case Mam_IR:
         ret = MIPSAMode_IR(am->Mam.IR.index, am->Mam.IR.base);
         break;
      case Mam_RR:
         ret = MIPSAMode_RR(am->Mam.RR.index, am->Mam.RR.base);
         break;
      default:
         vpanic("dopyMIPSAMode");
         break;
   }
   return ret;
}

MIPSAMode *nextMIPSAModeFloat(MIPSAMode * am)
{
   MIPSAMode* ret;
   switch (am->tag) {
      case Mam_IR:
         ret = MIPSAMode_IR(am->Mam.IR.index + 8, am->Mam.IR.base);
         break;
      case Mam_RR:
         ret = MIPSAMode_RR(am->Mam.RR.index + 1, am->Mam.RR.base);
         break;
      default:
         vpanic("dopyMIPSAMode");
         break;
   }
   return ret;
}

MIPSAMode *nextMIPSAModeInt(MIPSAMode * am)
{
   MIPSAMode* ret;
   switch (am->tag) {
      case Mam_IR:
         ret = MIPSAMode_IR(am->Mam.IR.index + 4, am->Mam.IR.base);
         break;
      case Mam_RR:
         ret = MIPSAMode_RR(am->Mam.RR.index + 1, am->Mam.RR.base);
         break;
      default:
         vpanic("dopyMIPSAMode");
         break;
   }
   return ret;
}

void ppMIPSAMode(MIPSAMode * am, Bool mode64)
{
   switch (am->tag) {
      case Mam_IR:
         if (am->Mam.IR.index == 0)
            vex_printf("0(");
         else
            vex_printf("%d(", (Int) am->Mam.IR.index);
         ppHRegMIPS(am->Mam.IR.base, mode64);
         vex_printf(")");
         return;
      case Mam_RR:
         ppHRegMIPS(am->Mam.RR.base, mode64);
         vex_printf(", ");
         ppHRegMIPS(am->Mam.RR.index, mode64);
         return;
      default:
         vpanic("ppMIPSAMode");
         break;
   }
}

static void addRegUsage_MIPSAMode(HRegUsage * u, MIPSAMode * am)
{
   switch (am->tag) {
      case Mam_IR:
         addHRegUse(u, HRmRead, am->Mam.IR.base);
         return;
      case Mam_RR:
         addHRegUse(u, HRmRead, am->Mam.RR.base);
         addHRegUse(u, HRmRead, am->Mam.RR.index);
         return;
      default:
         vpanic("addRegUsage_MIPSAMode");
         break;
   }
}

static void mapRegs_MIPSAMode(HRegRemap * m, MIPSAMode * am)
{
   switch (am->tag) {
      case Mam_IR:
         am->Mam.IR.base = lookupHRegRemap(m, am->Mam.IR.base);
         return;
      case Mam_RR:
         am->Mam.RR.base = lookupHRegRemap(m, am->Mam.RR.base);
         am->Mam.RR.index = lookupHRegRemap(m, am->Mam.RR.index);
         return;
      default:
         vpanic("mapRegs_MIPSAMode");
         break;
   }
}

/* --------- Operand, which can be a reg or a u16/s16. --------- */

MIPSRH *MIPSRH_Imm(Bool syned, UShort imm16)
{
   MIPSRH *op = LibVEX_Alloc(sizeof(MIPSRH));
   op->tag = Mrh_Imm;
   op->Mrh.Imm.syned = syned;
   op->Mrh.Imm.imm16 = imm16;
   /* If this is a signed value, ensure it's not -32768, so that we
      are guaranteed always to be able to negate if needed. */
   if (syned)
      vassert(imm16 != 0x8000);
   vassert(syned == True || syned == False);
   return op;
}

MIPSRH *MIPSRH_Reg(HReg reg)
{
   MIPSRH *op = LibVEX_Alloc(sizeof(MIPSRH));
   op->tag = Mrh_Reg;
   op->Mrh.Reg.reg = reg;
   return op;
}

void ppMIPSRH(MIPSRH * op, Bool mode64)
{
   MIPSRHTag tag = op->tag;
   switch (tag) {
      case Mrh_Imm:
         if (op->Mrh.Imm.syned)
            vex_printf("%d", (Int) (Short) op->Mrh.Imm.imm16);
         else
            vex_printf("%u", (UInt) (UShort) op->Mrh.Imm.imm16);
         return;
      case Mrh_Reg:
         ppHRegMIPS(op->Mrh.Reg.reg, mode64);
         return;
      default:
         vpanic("ppMIPSRH");
         break;
   }
}

/* An MIPSRH can only be used in a "read" context (what would it mean
   to write or modify a literal?) and so we enumerate its registers
   accordingly. */
static void addRegUsage_MIPSRH(HRegUsage * u, MIPSRH * op)
{
   switch (op->tag) {
      case Mrh_Imm:
         return;
      case Mrh_Reg:
         addHRegUse(u, HRmRead, op->Mrh.Reg.reg);
         return;
      default:
         vpanic("addRegUsage_MIPSRH");
         break;
   }
}

static void mapRegs_MIPSRH(HRegRemap * m, MIPSRH * op)
{
   switch (op->tag) {
      case Mrh_Imm:
         return;
      case Mrh_Reg:
         op->Mrh.Reg.reg = lookupHRegRemap(m, op->Mrh.Reg.reg);
         return;
      default:
         vpanic("mapRegs_MIPSRH");
         break;
   }
}

/* --------- Instructions. --------- */

HChar *showMIPSUnaryOp(MIPSUnaryOp op)
{
   HChar* ret;
   switch (op) {
      case Mun_CLO:
         ret = "clo";
         break;
      case Mun_CLZ:
         ret = "clz";
         break;
      case Mun_NOP:
         ret = "nop";
         break;
      default:
         vpanic("showMIPSUnaryOp");
         break;
   }
   return ret;
}

HChar *showMIPSAluOp(MIPSAluOp op, Bool immR)
{
   HChar* ret;
   switch (op) {
      case Malu_ADD:
         ret = immR ? "addiu" : "addu";
         break;
      case Malu_SUB:
         ret = "subu";
         break;
      case Malu_AND:
         ret = immR ? "andi" : "and";
         break;
      case Malu_OR:
         ret = immR ? "ori" : "or";
         break;
      case Malu_NOR:
         vassert(immR == False); /*there's no nor with an immediate operand!? */
         ret = "nor";
         break;
      case Malu_XOR:
         ret = immR ? "xori" : "xor";
         break;
      default:
         vpanic("showMIPSAluOp");
         break;
   }
   return ret;
}

HChar *showMIPSShftOp(MIPSShftOp op, Bool immR, Bool sz32)
{
   HChar *ret;
   switch (op) {
      case Mshft_SRA:
         ret = immR ? (sz32 ? "sar" : "dsar") : (sz32 ? "sarv" : "dsrav");
         break;
      case Mshft_SLL:
         ret = immR ? (sz32 ? "sll" : "dsll") : (sz32 ? "sllv" : "dsllv");
         break;
      case Mshft_SRL:
         ret = immR ? (sz32 ? "srl" : "dsrl") : (sz32 ? "srlv" : "dsrlv");
         break;
      default:
         vpanic("showMIPSShftOp");
         break;
   }
   return ret;
}

HChar *showMIPSMaccOp(MIPSMaccOp op, Bool variable)
{
   HChar *ret;
   switch (op) {
      case Macc_ADD:
         ret = variable ? "madd" : "maddu";
         break;
      case Macc_SUB:
         ret = variable ? "msub" : "msubu";
         break;
      default:
         vpanic("showMIPSAccOp");
         break;
   }
   return ret;
}

MIPSInstr *MIPSInstr_LI(HReg dst, ULong imm)
{
   MIPSInstr *i = LibVEX_Alloc(sizeof(MIPSInstr));
   i->tag = Min_LI;
   i->Min.LI.dst = dst;
   i->Min.LI.imm = imm;
   return i;
}

MIPSInstr *MIPSInstr_Alu(MIPSAluOp op, HReg dst, HReg srcL, MIPSRH * srcR)
{
   MIPSInstr *i = LibVEX_Alloc(sizeof(MIPSInstr));
   i->tag = Min_Alu;
   i->Min.Alu.op = op;
   i->Min.Alu.dst = dst;
   i->Min.Alu.srcL = srcL;
   i->Min.Alu.srcR = srcR;
   return i;
}

MIPSInstr *MIPSInstr_Shft(MIPSShftOp op, Bool sz32, HReg dst, HReg srcL,
                          MIPSRH * srcR)
{
   MIPSInstr *i = LibVEX_Alloc(sizeof(MIPSInstr));
   i->tag = Min_Shft;
   i->Min.Shft.op = op;
   i->Min.Shft.sz32 = sz32;
   i->Min.Shft.dst = dst;
   i->Min.Shft.srcL = srcL;
   i->Min.Shft.srcR = srcR;
   return i;
}

MIPSInstr *MIPSInstr_Unary(MIPSUnaryOp op, HReg dst, HReg src)
{
   MIPSInstr *i = LibVEX_Alloc(sizeof(MIPSInstr));
   i->tag = Min_Unary;
   i->Min.Unary.op = op;
   i->Min.Unary.dst = dst;
   i->Min.Unary.src = src;
   return i;
}

MIPSInstr *MIPSInstr_Cmp(Bool syned, Bool sz32, HReg dst, HReg srcL, HReg srcR,
                         MIPSCondCode cond)
{
   MIPSInstr *i = LibVEX_Alloc(sizeof(MIPSInstr));
   i->tag = Min_Cmp;
   i->Min.Cmp.syned = syned;
   i->Min.Cmp.sz32 = sz32;
   i->Min.Cmp.dst = dst;
   i->Min.Cmp.srcL = srcL;
   i->Min.Cmp.srcR = srcR;
   i->Min.Cmp.cond = cond;
   return i;
}

/* multiply */
MIPSInstr *MIPSInstr_Mul(Bool syned, Bool wid, Bool sz32, HReg dst, HReg srcL,
                         HReg srcR)
{
   MIPSInstr *i = LibVEX_Alloc(sizeof(MIPSInstr));
   i->tag = Min_Mul;
   i->Min.Mul.syned = syned;
   i->Min.Mul.widening = wid; /* widen=True else False */
   i->Min.Mul.sz32 = sz32; /* True = 32 bits */
   i->Min.Mul.dst = dst;
   i->Min.Mul.srcL = srcL;
   i->Min.Mul.srcR = srcR;
   return i;
}

/* msub */
MIPSInstr *MIPSInstr_Msub(Bool syned, HReg srcL, HReg srcR)
{
   MIPSInstr *i = LibVEX_Alloc(sizeof(MIPSInstr));
   i->tag = Min_Macc;

   i->Min.Macc.op = Macc_SUB;
   i->Min.Macc.syned = syned;
   i->Min.Macc.srcL = srcL;
   i->Min.Macc.srcR = srcR;
   return i;
}

/* madd */
MIPSInstr *MIPSInstr_Madd(Bool syned, HReg srcL, HReg srcR)
{
   MIPSInstr *i = LibVEX_Alloc(sizeof(MIPSInstr));
   i->tag = Min_Macc;

   i->Min.Macc.op = Macc_ADD;
   i->Min.Macc.syned = syned;
   i->Min.Macc.srcL = srcL;
   i->Min.Macc.srcR = srcR;
   return i;
}

/* div */
MIPSInstr *MIPSInstr_Div(Bool syned, Bool sz32, HReg srcL, HReg srcR)
{
   MIPSInstr *i = LibVEX_Alloc(sizeof(MIPSInstr));
   i->tag = Min_Div;
   i->Min.Div.syned = syned;
   i->Min.Div.sz32 = sz32; /* True = 32 bits */
   i->Min.Div.srcL = srcL;
   i->Min.Div.srcR = srcR;
   return i;
}

MIPSInstr *MIPSInstr_Call(MIPSCondCode cond, Addr32 target, UInt argiregs,
                          HReg src)
{
   UInt mask;
   MIPSInstr *i = LibVEX_Alloc(sizeof(MIPSInstr));
   i->tag = Min_Call;
   i->Min.Call.cond = cond;
   i->Min.Call.target = target;
   i->Min.Call.argiregs = argiregs;
   i->Min.Call.src = src;
   /* Only r4 .. r7 inclusive may be used as arg regs. Hence: */
   mask = (1 << 4) | (1 << 5) | (1 << 6) | (1 << 7);
   vassert(0 == (argiregs & ~mask));
   return i;
}

MIPSInstr *MIPSInstr_CallAlways(MIPSCondCode cond, Addr32 target, UInt argiregs)
{
   UInt mask;
   MIPSInstr *i = LibVEX_Alloc(sizeof(MIPSInstr));
   i->tag = Min_Call;
   i->Min.Call.cond = cond;
   i->Min.Call.target = target;
   i->Min.Call.argiregs = argiregs;
   /* Only r4 .. r7 inclusive may be used as arg regs. Hence: */
   mask = (1 << 4) | (1 << 5) | (1 << 6) | (1 << 7);
   vassert(0 == (argiregs & ~mask));
   return i;
}

MIPSInstr *MIPSInstr_XDirect ( Addr32 dstGA, MIPSAMode* amPC,
                               MIPSCondCode cond, Bool toFastEP ) {
   MIPSInstr* i               = LibVEX_Alloc(sizeof(MIPSInstr));
   i->tag                     = Min_XDirect;
   i->Min.XDirect.dstGA       = dstGA;
   i->Min.XDirect.amPC        = amPC;
   i->Min.XDirect.cond        = cond;
   i->Min.XDirect.toFastEP    = toFastEP;
   return i;
}

MIPSInstr *MIPSInstr_XIndir ( HReg dstGA, MIPSAMode* amPC,
                              MIPSCondCode cond ) {
   MIPSInstr* i            = LibVEX_Alloc(sizeof(MIPSInstr));
   i->tag                  = Min_XIndir;
   i->Min.XIndir.dstGA     = dstGA;
   i->Min.XIndir.amPC      = amPC;
   i->Min.XIndir.cond      = cond;
   return i;
}

MIPSInstr *MIPSInstr_XAssisted ( HReg dstGA, MIPSAMode* amPC,
                                 MIPSCondCode cond, IRJumpKind jk ) {
   MIPSInstr* i               = LibVEX_Alloc(sizeof(MIPSInstr));
   i->tag                     = Min_XAssisted;
   i->Min.XAssisted.dstGA     = dstGA;
   i->Min.XAssisted.amPC      = amPC;
   i->Min.XAssisted.cond      = cond;
   i->Min.XAssisted.jk        = jk;
   return i;
}

MIPSInstr *MIPSInstr_Load(UChar sz, HReg dst, MIPSAMode * src, Bool mode64)
{
   MIPSInstr *i = LibVEX_Alloc(sizeof(MIPSInstr));
   i->tag = Min_Load;
   i->Min.Load.sz = sz;
   i->Min.Load.src = src;
   i->Min.Load.dst = dst;
   vassert(sz == 1 || sz == 2 || sz == 4 || sz == 8);

   if (sz == 8)
      vassert(mode64);
   return i;
}

MIPSInstr *MIPSInstr_Store(UChar sz, MIPSAMode * dst, HReg src, Bool mode64)
{
   MIPSInstr *i = LibVEX_Alloc(sizeof(MIPSInstr));
   i->tag = Min_Store;
   i->Min.Store.sz = sz;
   i->Min.Store.src = src;
   i->Min.Store.dst = dst;
   vassert(sz == 1 || sz == 2 || sz == 4 || sz == 8);

   if (sz == 8)
      vassert(mode64);
   return i;
}

MIPSInstr *MIPSInstr_LoadL(UChar sz, HReg dst, MIPSAMode * src, Bool mode64)
{
   MIPSInstr *i = LibVEX_Alloc(sizeof(MIPSInstr));
   i->tag = Min_LoadL;
   i->Min.LoadL.sz  = sz;
   i->Min.LoadL.src = src;
   i->Min.LoadL.dst = dst;
   vassert(sz == 4 || sz == 8);

   if (sz == 8)
      vassert(mode64);
   return i;
}

MIPSInstr *MIPSInstr_StoreC(UChar sz, MIPSAMode * dst, HReg src, Bool mode64)
{
   MIPSInstr *i = LibVEX_Alloc(sizeof(MIPSInstr));
   i->tag = Min_StoreC;
   i->Min.StoreC.sz  = sz;
   i->Min.StoreC.src = src;
   i->Min.StoreC.dst = dst;
   vassert(sz == 4 || sz == 8);

   if (sz == 8)
      vassert(mode64);
   return i;
}

MIPSInstr *MIPSInstr_Mthi(HReg src)
{
   MIPSInstr *i = LibVEX_Alloc(sizeof(MIPSInstr));
   i->tag = Min_Mthi;
   i->Min.MtHL.src = src;
   return i;
}

MIPSInstr *MIPSInstr_Mtlo(HReg src)
{
   MIPSInstr *i = LibVEX_Alloc(sizeof(MIPSInstr));
   i->tag = Min_Mtlo;
   i->Min.MtHL.src = src;
   return i;
}

MIPSInstr *MIPSInstr_Mfhi(HReg dst)
{
   MIPSInstr *i = LibVEX_Alloc(sizeof(MIPSInstr));
   i->tag = Min_Mfhi;
   i->Min.MfHL.dst = dst;
   return i;
}

MIPSInstr *MIPSInstr_Mflo(HReg dst)
{
   MIPSInstr *i = LibVEX_Alloc(sizeof(MIPSInstr));
   i->tag = Min_Mflo;
   i->Min.MfHL.dst = dst;
   return i;
}

/* Read/Write Link Register */
MIPSInstr *MIPSInstr_RdWrLR(Bool wrLR, HReg gpr)
{
   MIPSInstr *i = LibVEX_Alloc(sizeof(MIPSInstr));
   i->tag = Min_RdWrLR;
   i->Min.RdWrLR.wrLR = wrLR;
   i->Min.RdWrLR.gpr = gpr;
   return i;
}

MIPSInstr *MIPSInstr_FpLdSt(Bool isLoad, UChar sz, HReg reg, MIPSAMode * addr)
{
   MIPSInstr *i = LibVEX_Alloc(sizeof(MIPSInstr));
   i->tag = Min_FpLdSt;
   i->Min.FpLdSt.isLoad = isLoad;
   i->Min.FpLdSt.sz = sz;
   i->Min.FpLdSt.reg = reg;
   i->Min.FpLdSt.addr = addr;
   vassert(sz == 4 || sz == 8);
   return i;
}

MIPSInstr *MIPSInstr_FpUnary(MIPSFpOp op, HReg dst, HReg src)
{
   MIPSInstr *i = LibVEX_Alloc(sizeof(MIPSInstr));
   i->tag = Min_FpUnary;
   i->Min.FpUnary.op = op;
   i->Min.FpUnary.dst = dst;
   i->Min.FpUnary.src = src;
   return i;
}

MIPSInstr *MIPSInstr_FpBinary(MIPSFpOp op, HReg dst, HReg srcL, HReg srcR)
{
   MIPSInstr *i = LibVEX_Alloc(sizeof(MIPSInstr));
   i->tag = Min_FpBinary;
   i->Min.FpBinary.op = op;
   i->Min.FpBinary.dst = dst;
   i->Min.FpBinary.srcL = srcL;
   i->Min.FpBinary.srcR = srcR;
   return i;
}

MIPSInstr *MIPSInstr_FpConvert(MIPSFpOp op, HReg dst, HReg src)
{
   MIPSInstr *i = LibVEX_Alloc(sizeof(MIPSInstr));
   i->tag = Min_FpConvert;
   i->Min.FpConvert.op = op;
   i->Min.FpConvert.dst = dst;
   i->Min.FpConvert.src = src;
   return i;

}

MIPSInstr *MIPSInstr_FpCompare(MIPSFpOp op, HReg dst, HReg srcL, HReg srcR,
                               UChar cond1)
{
   MIPSInstr *i = LibVEX_Alloc(sizeof(MIPSInstr));
   i->tag = Min_FpCompare;
   i->Min.FpCompare.op = op;
   i->Min.FpCompare.dst = dst;
   i->Min.FpCompare.srcL = srcL;
   i->Min.FpCompare.srcR = srcR;
   i->Min.FpCompare.cond1 = cond1;
   return i;
}

MIPSInstr *MIPSInstr_MovCond(HReg dst, HReg argL, MIPSRH * argR, HReg condR,
                              MIPSCondCode cond)
{
   MIPSInstr *i = LibVEX_Alloc(sizeof(MIPSInstr));
   i->tag = Min_MovCond;
   i->Min.MovCond.dst = dst;
   i->Min.MovCond.srcL = argL;
   i->Min.MovCond.srcR = argR;
   i->Min.MovCond.condR = condR;
   i->Min.MovCond.cond = cond;
   return i;
}

MIPSInstr *MIPSInstr_MtFCSR(HReg src)
{
   MIPSInstr *i = LibVEX_Alloc(sizeof(MIPSInstr));
   i->tag = Min_MtFCSR;
   i->Min.MtFCSR.src = src;
   return i;
}

MIPSInstr *MIPSInstr_MfFCSR(HReg dst)
{
   MIPSInstr *i = LibVEX_Alloc(sizeof(MIPSInstr));
   i->tag = Min_MfFCSR;
   i->Min.MfFCSR.dst = dst;
   return i;
}

MIPSInstr *MIPSInstr_EvCheck ( MIPSAMode* amCounter,
                            MIPSAMode* amFailAddr ) {
   MIPSInstr* i                 = LibVEX_Alloc(sizeof(MIPSInstr));
   i->tag                       = Min_EvCheck;
   i->Min.EvCheck.amCounter     = amCounter;
   i->Min.EvCheck.amFailAddr    = amFailAddr;
   return i;
}

MIPSInstr* MIPSInstr_ProfInc ( void ) {
   MIPSInstr* i = LibVEX_Alloc(sizeof(MIPSInstr));
   i->tag       = Min_ProfInc;
   return i;
}

/* -------- Pretty Print instructions ------------- */
static void ppLoadImm(HReg dst, ULong imm, Bool mode64)
{
   vex_printf("li ");
   ppHRegMIPS(dst, mode64);
   vex_printf(",0x%016llx", imm);
}

void ppMIPSInstr(MIPSInstr * i, Bool mode64)
{
   switch (i->tag) {
      case Min_LI:
         ppLoadImm(i->Min.LI.dst, i->Min.LI.imm, mode64);
         break;
      case Min_Alu: {
         HReg r_srcL = i->Min.Alu.srcL;
         MIPSRH *rh_srcR = i->Min.Alu.srcR;
         /* generic */
         vex_printf("%s ", showMIPSAluOp(i->Min.Alu.op,
                                         toBool(rh_srcR->tag == Mrh_Imm)));
         ppHRegMIPS(i->Min.Alu.dst, mode64);
         vex_printf(",");
         ppHRegMIPS(r_srcL, mode64);
         vex_printf(",");
         ppMIPSRH(rh_srcR, mode64);
         return;
      }
      case Min_Shft: {
         HReg r_srcL = i->Min.Shft.srcL;
         MIPSRH *rh_srcR = i->Min.Shft.srcR;
         vex_printf("%s ", showMIPSShftOp(i->Min.Shft.op,
                                          toBool(rh_srcR->tag == Mrh_Imm),
                                          i->Min.Shft.sz32));
         ppHRegMIPS(i->Min.Shft.dst, mode64);
         vex_printf(",");
         ppHRegMIPS(r_srcL, mode64);
         vex_printf(",");
         ppMIPSRH(rh_srcR, mode64);
         return;
      }
      case Min_Unary: {
         vex_printf("%s ", showMIPSUnaryOp(i->Min.Unary.op));
         ppHRegMIPS(i->Min.Unary.dst, mode64);
         vex_printf(",");
         ppHRegMIPS(i->Min.Unary.src, mode64);
         return;
      }
      case Min_Cmp: {
         vex_printf("word_compare ");
         ppHRegMIPS(i->Min.Cmp.dst, mode64);
         vex_printf(" = %s ( ", showMIPSCondCode(i->Min.Cmp.cond));
         ppHRegMIPS(i->Min.Cmp.srcL, mode64);
         vex_printf(", ");
         ppHRegMIPS(i->Min.Cmp.srcR, mode64);
         vex_printf(" )");

         return;
      }
      case Min_Mul: {
         switch (i->Min.Mul.widening) {
            case False:
               vex_printf("mul ");
               ppHRegMIPS(i->Min.Mul.dst, mode64);
               vex_printf(", ");
               ppHRegMIPS(i->Min.Mul.srcL, mode64);
               vex_printf(", ");
               ppHRegMIPS(i->Min.Mul.srcR, mode64);
               return;
            case True:
               vex_printf("%s%s ", i->Min.Mul.sz32 ? "mult" : "dmult",
                                   i->Min.Mul.syned ? "" : "u");
               ppHRegMIPS(i->Min.Mul.dst, mode64);
               vex_printf(", ");
               ppHRegMIPS(i->Min.Mul.srcL, mode64);
               vex_printf(", ");
               ppHRegMIPS(i->Min.Mul.srcR, mode64);
               return;
            }
         break;
      }
      case Min_Mthi: {
         vex_printf("mthi ");
         ppHRegMIPS(i->Min.MtHL.src, mode64);
         return;
      }
      case Min_Mtlo: {
         vex_printf("mtlo ");
         ppHRegMIPS(i->Min.MtHL.src, mode64);
         return;
      }
      case Min_Mfhi: {
         vex_printf("mfhi ");
         ppHRegMIPS(i->Min.MfHL.dst, mode64);
         return;
      }
      case Min_Mflo: {
         vex_printf("mflo ");
         ppHRegMIPS(i->Min.MfHL.dst, mode64);
         return;
      }
      case Min_Macc: {
         vex_printf("%s ", showMIPSMaccOp(i->Min.Macc.op, i->Min.Macc.syned));
         ppHRegMIPS(i->Min.Macc.srcL, mode64);
         vex_printf(", ");
         ppHRegMIPS(i->Min.Macc.srcR, mode64);
         return;
      }
      case Min_Div: {
         if (!i->Min.Div.sz32)
            vex_printf("d");
         vex_printf("div");
         vex_printf("%s ", i->Min.Div.syned ? "s" : "u");
         ppHRegMIPS(i->Min.Div.srcL, mode64);
         vex_printf(", ");
         ppHRegMIPS(i->Min.Div.srcR, mode64);
         return;
      }
      case Min_Call: {
         Int n;
         vex_printf("call: ");
         if (i->Min.Call.cond != MIPScc_AL) {
            vex_printf("if (%s) ", showMIPSCondCode(i->Min.Call.cond));
         }
         vex_printf("{ ");
         ppLoadImm(hregMIPS_GPR11(mode64), i->Min.Call.target, mode64);

         vex_printf(" ; mtctr r10 ; bctrl [");
         for (n = 0; n < 32; n++) {
            if (i->Min.Call.argiregs & (1 << n)) {
               vex_printf("r%d", n);
               if ((i->Min.Call.argiregs >> n) > 1)
                  vex_printf(",");
            }
         }
         vex_printf("] }");
         break;
      }
      case Min_XDirect:
         vex_printf("(xDirect) ");
         vex_printf("if (guest_COND.%s) { ",
                    showMIPSCondCode(i->Min.XDirect.cond));
         vex_printf("move $9, 0x%x,", i->Min.XDirect.dstGA);
         vex_printf("; sw $9, ");
         ppMIPSAMode(i->Min.XDirect.amPC, mode64);
         vex_printf("; move $9, $disp_cp_chain_me_to_%sEP; jalr $9; nop}",
                    i->Min.XDirect.toFastEP ? "fast" : "slow");
         return;
      case Min_XIndir:
         vex_printf("(xIndir) ");
         vex_printf("if (guest_COND.%s) { sw ",
        	        showMIPSCondCode(i->Min.XIndir.cond));
         ppHRegMIPS(i->Min.XIndir.dstGA, mode64);
         vex_printf(", ");
         ppMIPSAMode(i->Min.XIndir.amPC, mode64);
         vex_printf("; move $9, $disp_indir; jalr $9; nop}");
         return;
      case Min_XAssisted:
         vex_printf("(xAssisted) ");
         vex_printf("if (guest_COND.%s) { ",
                    showMIPSCondCode(i->Min.XAssisted.cond));
         vex_printf("sw ");
         ppHRegMIPS(i->Min.XAssisted.dstGA, mode64);
         vex_printf(", ");
         ppMIPSAMode(i->Min.XAssisted.amPC, mode64);
         vex_printf("; move $9, $IRJumpKind_to_TRCVAL(%d)",
                    (Int)i->Min.XAssisted.jk);
         vex_printf("; move $9, $disp_assisted; jalr $9; nop; }");
         return;
      case Min_Load: {
         Bool idxd = toBool(i->Min.Load.src->tag == Mam_RR);
         UChar sz = i->Min.Load.sz;
         UChar c_sz = sz == 1 ? 'b' : sz == 2 ? 'h' : sz == 4 ? 'w' : 'd';
         vex_printf("l%c%s ", c_sz, idxd ? "x" : "");
         ppHRegMIPS(i->Min.Load.dst, mode64);
         vex_printf(",");
         ppMIPSAMode(i->Min.Load.src, mode64);
         return;
      }
      case Min_Store: {
         UChar sz = i->Min.Store.sz;
         Bool idxd = toBool(i->Min.Store.dst->tag == Mam_RR);
         UChar c_sz = sz == 1 ? 'b' : sz == 2 ? 'h' : sz == 4 ? 'w' : 'd';
         vex_printf("s%c%s ", c_sz, idxd ? "x" : "");
         ppHRegMIPS(i->Min.Store.src, mode64);
         vex_printf(",");
         ppMIPSAMode(i->Min.Store.dst, mode64);
         return;
      }
      case Min_LoadL: {
         vex_printf("ll ");
         ppHRegMIPS(i->Min.LoadL.dst, mode64);
         vex_printf(",");
         ppMIPSAMode(i->Min.LoadL.src, mode64);
         return;
      }
      case Min_StoreC: {
         vex_printf("sc ");
         ppHRegMIPS(i->Min.StoreC.src, mode64);
         vex_printf(",");
         ppMIPSAMode(i->Min.StoreC.dst, mode64);
         return;
      }
      case Min_RdWrLR: {
         vex_printf("%s ", i->Min.RdWrLR.wrLR ? "mtlr" : "mflr");
         ppHRegMIPS(i->Min.RdWrLR.gpr, mode64);
         return;
      }
      case Min_FpUnary:
         vex_printf("%s ", showMIPSFpOp(i->Min.FpUnary.op));
         ppHRegMIPS(i->Min.FpUnary.dst, mode64);
         vex_printf(",");
         ppHRegMIPS(i->Min.FpUnary.src, mode64);
         return;
      case Min_FpBinary:
         vex_printf("%s", showMIPSFpOp(i->Min.FpBinary.op));
         ppHRegMIPS(i->Min.FpBinary.dst, mode64);
         vex_printf(",");
         ppHRegMIPS(i->Min.FpBinary.srcL, mode64);
         vex_printf(",");
         ppHRegMIPS(i->Min.FpBinary.srcR, mode64);
         return;
      case Min_FpConvert:
         vex_printf("%s", showMIPSFpOp(i->Min.FpConvert.op));
         ppHRegMIPS(i->Min.FpConvert.dst, mode64);
         vex_printf(",");
         ppHRegMIPS(i->Min.FpConvert.src, mode64);
         return;
      case Min_FpCompare:
         vex_printf("%s ", showMIPSFpOp(i->Min.FpCompare.op));
         ppHRegMIPS(i->Min.FpCompare.srcL, mode64);
         vex_printf(",");
         ppHRegMIPS(i->Min.FpCompare.srcR, mode64);
         vex_printf(" cond: %c", i->Min.FpCompare.cond1);
         return;
      case Min_FpMulAcc:
         vex_printf("%s ", showMIPSFpOp(i->Min.FpMulAcc.op));
         ppHRegMIPS(i->Min.FpMulAcc.dst, mode64);
         vex_printf(",");
         ppHRegMIPS(i->Min.FpMulAcc.srcML, mode64);
         vex_printf(",");
         ppHRegMIPS(i->Min.FpMulAcc.srcMR, mode64);
         vex_printf(",");
         ppHRegMIPS(i->Min.FpMulAcc.srcAcc, mode64);
         return;
      case Min_FpLdSt: {
         if (i->Min.FpLdSt.sz == 4) {
            if (i->Min.FpLdSt.isLoad) {
               vex_printf("lwc1 ");
               ppHRegMIPS(i->Min.FpLdSt.reg, mode64);
               vex_printf(",");
               ppMIPSAMode(i->Min.FpLdSt.addr, mode64);
            } else {
               vex_printf("swc1 ");
               ppHRegMIPS(i->Min.FpLdSt.reg, mode64);
               vex_printf(",");
               ppMIPSAMode(i->Min.FpLdSt.addr, mode64);
            }
         } else if (i->Min.FpLdSt.sz == 8) {
            if (i->Min.FpLdSt.isLoad) {
               if (mode64)
                  vex_printf("ldc1 ");
               else
                  vex_printf("lwc1 ");
               ppHRegMIPS(i->Min.FpLdSt.reg, mode64);
               vex_printf(",");
               ppMIPSAMode(i->Min.FpLdSt.addr, mode64);
            } else {
               if (mode64)
                  vex_printf("sdc1 ");
               else
                  vex_printf("swc1 ");
               ppHRegMIPS(i->Min.FpLdSt.reg, mode64);
               vex_printf(",");
               ppMIPSAMode(i->Min.FpLdSt.addr, mode64);
            }
         }
         return;
      }
      case Min_MovCond: {
         if (i->Min.MovCond.cond == MIPScc_MI) {
            vex_printf("\ncond move\n");
            return;

         }
         break;
      }
      case Min_MtFCSR: {
         vex_printf("ctc1  ");
         ppHRegMIPS(i->Min.MtFCSR.src, mode64);
         vex_printf(", $31");
         return;
      }
   
      case Min_MfFCSR: {
         vex_printf("ctc1  ");
         ppHRegMIPS(i->Min.MfFCSR.dst, mode64);
         vex_printf(", $31");
         return;
      }
      case Min_EvCheck:
         vex_printf("(evCheck) lw $9, ");
         ppMIPSAMode(i->Min.EvCheck.amCounter, mode64);
         vex_printf("; addiu $9, $9, -1");
         vex_printf("; sw $9, ");
         ppMIPSAMode(i->Min.EvCheck.amCounter, mode64);
         vex_printf("; bgez $t9, nofail; jalr *");
         ppMIPSAMode(i->Min.EvCheck.amFailAddr, mode64);
         vex_printf("; nofail:");
         return;
      case Min_ProfInc:
         vex_printf("(profInc) move $9, ($NotKnownYet); "
                    "lw $8, 0($9); "
                    "addiu $8, $8, 1; "
                    "sw $8, 0($9); "
                    "sltiu $1, $8, 1; "
                    "lw $8, 4($9); "
                    "addu $8, $8, $1; "
                    "sw $8, 4($9); " );
         return;
      default:
         vpanic("ppMIPSInstr");
         break;
   }
}

/* --------- Helpers for register allocation. --------- */

void getRegUsage_MIPSInstr(HRegUsage * u, MIPSInstr * i, Bool mode64)
{
   initHRegUsage(u);
   switch (i->tag) {
      case Min_LI:
         addHRegUse(u, HRmWrite, i->Min.LI.dst);
         break;
      case Min_Alu:
         addHRegUse(u, HRmRead, i->Min.Alu.srcL);
         addRegUsage_MIPSRH(u, i->Min.Alu.srcR);
         addHRegUse(u, HRmWrite, i->Min.Alu.dst);
         return;
      case Min_Shft:
         addHRegUse(u, HRmRead, i->Min.Shft.srcL);
         addRegUsage_MIPSRH(u, i->Min.Shft.srcR);
         addHRegUse(u, HRmWrite, i->Min.Shft.dst);
         return;
      case Min_Cmp:
         addHRegUse(u, HRmRead, i->Min.Cmp.srcL);
         addHRegUse(u, HRmRead, i->Min.Cmp.srcR);
         addHRegUse(u, HRmWrite, i->Min.Cmp.dst);
         return;
      case Min_Unary:
         addHRegUse(u, HRmRead, i->Min.Unary.src);
         addHRegUse(u, HRmWrite, i->Min.Unary.dst);
         return;
      case Min_Mul:
         addHRegUse(u, HRmWrite, i->Min.Mul.dst);
         addHRegUse(u, HRmRead, i->Min.Mul.srcL);
         addHRegUse(u, HRmRead, i->Min.Mul.srcR);
         return;
      case Min_Mthi:
      case Min_Mtlo:
         addHRegUse(u, HRmWrite, hregMIPS_HI(mode64));
         addHRegUse(u, HRmWrite, hregMIPS_LO(mode64));
         addHRegUse(u, HRmRead, i->Min.MtHL.src);
         return;
      case Min_Mfhi:
      case Min_Mflo:
         addHRegUse(u, HRmRead, hregMIPS_HI(mode64));
         addHRegUse(u, HRmRead, hregMIPS_LO(mode64));
         addHRegUse(u, HRmWrite, i->Min.MfHL.dst);
         return;
      case Min_MtFCSR:
         addHRegUse(u, HRmRead, i->Min.MtFCSR.src);
         return;
      case Min_MfFCSR:
         addHRegUse(u, HRmWrite, i->Min.MfFCSR.dst);
         return;
      case Min_Macc:
         addHRegUse(u, HRmModify, hregMIPS_HI(mode64));
         addHRegUse(u, HRmModify, hregMIPS_LO(mode64));
         addHRegUse(u, HRmRead, i->Min.Macc.srcL);
         addHRegUse(u, HRmRead, i->Min.Macc.srcR);
         return;
      case Min_Div:
         addHRegUse(u, HRmWrite, hregMIPS_HI(mode64));
         addHRegUse(u, HRmWrite, hregMIPS_LO(mode64));
         addHRegUse(u, HRmRead, i->Min.Div.srcL);
         addHRegUse(u, HRmRead, i->Min.Div.srcR);
         return;
      case Min_Call: {
         if (i->Min.Call.cond != MIPScc_AL)
            addHRegUse(u, HRmRead, i->Min.Call.src);
         UInt argir;
         addHRegUse(u, HRmWrite, hregMIPS_GPR1(mode64));

         addHRegUse(u, HRmWrite, hregMIPS_GPR2(mode64));
         addHRegUse(u, HRmWrite, hregMIPS_GPR3(mode64));

         addHRegUse(u, HRmWrite, hregMIPS_GPR4(mode64));
         addHRegUse(u, HRmWrite, hregMIPS_GPR5(mode64));
         addHRegUse(u, HRmWrite, hregMIPS_GPR6(mode64));
         addHRegUse(u, HRmWrite, hregMIPS_GPR7(mode64));

         addHRegUse(u, HRmWrite, hregMIPS_GPR8(mode64));
         addHRegUse(u, HRmWrite, hregMIPS_GPR9(mode64));
         addHRegUse(u, HRmWrite, hregMIPS_GPR10(mode64));
         addHRegUse(u, HRmWrite, hregMIPS_GPR11(mode64));
         addHRegUse(u, HRmWrite, hregMIPS_GPR12(mode64));
         addHRegUse(u, HRmWrite, hregMIPS_GPR13(mode64));
         addHRegUse(u, HRmWrite, hregMIPS_GPR14(mode64));
         addHRegUse(u, HRmWrite, hregMIPS_GPR15(mode64));

         addHRegUse(u, HRmWrite, hregMIPS_GPR24(mode64));
         addHRegUse(u, HRmWrite, hregMIPS_GPR25(mode64));
         addHRegUse(u, HRmWrite, hregMIPS_GPR26(mode64));
         addHRegUse(u, HRmWrite, hregMIPS_GPR27(mode64));

         /* Now we have to state any parameter-carrying registers
            which might be read.  This depends on the argiregs field. */
         argir = i->Min.Call.argiregs;
         if (argir & (1 << 7))
            addHRegUse(u, HRmRead, hregMIPS_GPR7(mode64));
         if (argir & (1 << 6))
            addHRegUse(u, HRmRead, hregMIPS_GPR6(mode64));
         if (argir & (1 << 5))
            addHRegUse(u, HRmRead, hregMIPS_GPR5(mode64));
         if (argir & (1 << 4))
            addHRegUse(u, HRmRead, hregMIPS_GPR4(mode64));

         vassert(0 == (argir & ~((1 << 4) | (1 << 5) | (1 << 6) | (1 << 7))));
         return;
      }
      /* XDirect/XIndir/XAssisted are also a bit subtle.  They
         conditionally exit the block.  Hence we only need to list (1)
         the registers that they read, and (2) the registers that they
         write in the case where the block is not exited.  (2) is
         empty, hence only (1) is relevant here. */
      case Min_XDirect:
         addRegUsage_MIPSAMode(u, i->Min.XDirect.amPC);
         return;
      case Min_XIndir:
         addHRegUse(u, HRmRead, i->Min.XIndir.dstGA);
         addRegUsage_MIPSAMode(u, i->Min.XIndir.amPC);
         return;
      case Min_XAssisted:
         addHRegUse(u, HRmRead, i->Min.XAssisted.dstGA);
         addRegUsage_MIPSAMode(u, i->Min.XAssisted.amPC);
         return;
      case Min_Load:
         addRegUsage_MIPSAMode(u, i->Min.Load.src);
         addHRegUse(u, HRmWrite, i->Min.Load.dst);
         return;
      case Min_Store:
         addHRegUse(u, HRmRead, i->Min.Store.src);
         addRegUsage_MIPSAMode(u, i->Min.Store.dst);
         return;
      case Min_LoadL:
         addRegUsage_MIPSAMode(u, i->Min.LoadL.src);
         addHRegUse(u, HRmWrite, i->Min.LoadL.dst);
         return;
      case Min_StoreC:
         addHRegUse(u, HRmWrite, i->Min.StoreC.src);
         addHRegUse(u, HRmRead, i->Min.StoreC.src);
         addRegUsage_MIPSAMode(u, i->Min.StoreC.dst);
         return;
      case Min_RdWrLR:
         addHRegUse(u, (i->Min.RdWrLR.wrLR ? HRmRead : HRmWrite),
                        i->Min.RdWrLR.gpr);
         return;
      case Min_FpLdSt:
         if (i->Min.FpLdSt.sz == 4) {
            addHRegUse(u, (i->Min.FpLdSt.isLoad ? HRmWrite : HRmRead),
                           i->Min.FpLdSt.reg);
            addRegUsage_MIPSAMode(u, i->Min.FpLdSt.addr);
            return;
         } else if (i->Min.FpLdSt.sz == 8) {
            if (mode64) {
               addHRegUse(u, (i->Min.FpLdSt.isLoad ? HRmWrite : HRmRead),
                              i->Min.FpLdSt.reg);
               addRegUsage_MIPSAMode(u, i->Min.FpLdSt.addr);
            } else {
               addHRegUse(u, (i->Min.FpLdSt.isLoad ? HRmWrite : HRmRead),
                              i->Min.FpLdSt.reg);
               addRegUsage_MIPSAMode(u, i->Min.FpLdSt.addr);
               addRegUsage_MIPSAMode(u, nextMIPSAModeFloat(i->Min.FpLdSt.addr));
            }
            return;
         }
         break;
      case Min_FpUnary:
         if (i->Min.FpUnary.op == Mfp_CVTD) {
            addHRegUse(u, HRmWrite, i->Min.FpUnary.dst);
            addHRegUse(u, HRmRead, i->Min.FpUnary.src);
            return;
         } else {
            addHRegUse(u, HRmWrite, i->Min.FpUnary.dst);
            addHRegUse(u, HRmRead, i->Min.FpUnary.src);
            return;
         }
      case Min_FpBinary:
         addHRegUse(u, HRmWrite, i->Min.FpBinary.dst);
         addHRegUse(u, HRmRead, i->Min.FpBinary.srcL);
         addHRegUse(u, HRmRead, i->Min.FpBinary.srcR);
         return;
      case Min_FpConvert:
         addHRegUse(u, HRmWrite, i->Min.FpConvert.dst);
         addHRegUse(u, HRmRead, i->Min.FpConvert.src);
         return;
      case Min_FpCompare:
         addHRegUse(u, HRmWrite, i->Min.FpCompare.dst);
         addHRegUse(u, HRmRead, i->Min.FpCompare.srcL);
         addHRegUse(u, HRmRead, i->Min.FpCompare.srcR);
         return;
      case Min_MovCond:
         if (i->Min.MovCond.srcR->tag == Mrh_Reg) {
            addHRegUse(u, HRmRead, i->Min.MovCond.srcR->Mrh.Reg.reg);
         }
         addHRegUse(u, HRmRead, i->Min.MovCond.srcL);
         addHRegUse(u, HRmRead, i->Min.MovCond.condR);
         addHRegUse(u, HRmWrite, i->Min.MovCond.dst);
         return;
      case Min_EvCheck:
         /* We expect both amodes only to mention %ebp, so this is in
            fact pointless, since %ebp isn't allocatable, but anyway.. */
         addRegUsage_MIPSAMode(u, i->Min.EvCheck.amCounter);
         addRegUsage_MIPSAMode(u, i->Min.EvCheck.amFailAddr);
         return;
      case Min_ProfInc:
         /* does not use any registers. */
         return;
      default:
         ppMIPSInstr(i, mode64);
         vpanic("getRegUsage_MIPSInstr");
         break;
   }
}

/* local helper */
static void mapReg(HRegRemap * m, HReg * r)
{
   *r = lookupHRegRemap(m, *r);
}

void mapRegs_MIPSInstr(HRegRemap * m, MIPSInstr * i, Bool mode64)
{
   switch (i->tag) {
      case Min_LI:
         mapReg(m, &i->Min.LI.dst);
         break;
      case Min_Alu:
         mapReg(m, &i->Min.Alu.srcL);
         mapRegs_MIPSRH(m, i->Min.Alu.srcR);
         mapReg(m, &i->Min.Alu.dst);
         return;
      case Min_Shft:
         mapReg(m, &i->Min.Shft.srcL);
         mapRegs_MIPSRH(m, i->Min.Shft.srcR);
         mapReg(m, &i->Min.Shft.dst);
         return;
      case Min_Cmp:
         mapReg(m, &i->Min.Cmp.srcL);
         mapReg(m, &i->Min.Cmp.srcR);
         mapReg(m, &i->Min.Cmp.dst);
         return;
      case Min_Unary:
         mapReg(m, &i->Min.Unary.src);
         mapReg(m, &i->Min.Unary.dst);
         return;
      case Min_Mul:
         mapReg(m, &i->Min.Mul.dst);
         mapReg(m, &i->Min.Mul.srcL);
         mapReg(m, &i->Min.Mul.srcR);
         return;
      case Min_Mthi:
      case Min_Mtlo:
         mapReg(m, &i->Min.MtHL.src);
         return;
      case Min_Mfhi:
      case Min_Mflo:
         mapReg(m, &i->Min.MfHL.dst);
         return;
      case Min_Macc:
         mapReg(m, &i->Min.Macc.srcL);
         mapReg(m, &i->Min.Macc.srcR);
         return;
      case Min_Div:
         mapReg(m, &i->Min.Div.srcL);
         mapReg(m, &i->Min.Div.srcR);
         return;
      case Min_Call:
         {
            if (i->Min.Call.cond != MIPScc_AL)
               mapReg(m, &i->Min.Call.src);
            return;
         }
      case Min_XDirect:
         mapRegs_MIPSAMode(m, i->Min.XDirect.amPC);
         return;
      case Min_XIndir:
         mapReg(m, &i->Min.XIndir.dstGA);
         mapRegs_MIPSAMode(m, i->Min.XIndir.amPC);
         return;
      case Min_XAssisted:
         mapReg(m, &i->Min.XAssisted.dstGA);
         mapRegs_MIPSAMode(m, i->Min.XAssisted.amPC);
         return;
      case Min_Load:
         mapRegs_MIPSAMode(m, i->Min.Load.src);
         mapReg(m, &i->Min.Load.dst);
         return;
      case Min_Store:
         mapReg(m, &i->Min.Store.src);
         mapRegs_MIPSAMode(m, i->Min.Store.dst);
         return;
      case Min_LoadL:
         mapRegs_MIPSAMode(m, i->Min.LoadL.src);
         mapReg(m, &i->Min.LoadL.dst);
         return;
      case Min_StoreC:
         mapReg(m, &i->Min.StoreC.src);
         mapRegs_MIPSAMode(m, i->Min.StoreC.dst);
         return;
      case Min_RdWrLR:
         mapReg(m, &i->Min.RdWrLR.gpr);
         return;
      case Min_FpLdSt:
         if (i->Min.FpLdSt.sz == 4) {
            mapReg(m, &i->Min.FpLdSt.reg);
            mapRegs_MIPSAMode(m, i->Min.FpLdSt.addr);
            return;
         } else if (i->Min.FpLdSt.sz == 8) {
            if (mode64) {
               mapReg(m, &i->Min.FpLdSt.reg);
               mapRegs_MIPSAMode(m, i->Min.FpLdSt.addr);
            } else {
               mapReg(m, &i->Min.FpLdSt.reg);
               mapRegs_MIPSAMode(m, i->Min.FpLdSt.addr);
               mapRegs_MIPSAMode(m, nextMIPSAModeFloat(i->Min.FpLdSt.addr));
            }
            return;
         }
         break;
      case Min_FpUnary:
         if (i->Min.FpUnary.op == Mfp_CVTD) {
            mapReg(m, &i->Min.FpUnary.dst);
            mapReg(m, &i->Min.FpUnary.src);
            return;
         } else {
            mapReg(m, &i->Min.FpUnary.dst);
            mapReg(m, &i->Min.FpUnary.src);
            return;
         }
      case Min_FpBinary:
         mapReg(m, &i->Min.FpBinary.dst);
         mapReg(m, &i->Min.FpBinary.srcL);
         mapReg(m, &i->Min.FpBinary.srcR);
         return;
      case Min_FpConvert:
         mapReg(m, &i->Min.FpConvert.dst);
         mapReg(m, &i->Min.FpConvert.src);
         return;
      case Min_FpCompare:
         mapReg(m, &i->Min.FpCompare.dst);
         mapReg(m, &i->Min.FpCompare.srcL);
         mapReg(m, &i->Min.FpCompare.srcR);
         return;
      case Min_MtFCSR:
         mapReg(m, &i->Min.MtFCSR.src);
         return;
      case Min_MfFCSR:
         mapReg(m, &i->Min.MfFCSR.dst);
         return;
      case Min_MovCond:
         if (i->Min.MovCond.srcR->tag == Mrh_Reg) {
            mapReg(m, &(i->Min.MovCond.srcR->Mrh.Reg.reg));
         }
         mapReg(m, &i->Min.MovCond.srcL);
         mapReg(m, &i->Min.MovCond.condR);
         mapReg(m, &i->Min.MovCond.dst);

         return;
      case Min_EvCheck:
         /* We expect both amodes only to mention %ebp, so this is in
            fact pointless, since %ebp isn't allocatable, but anyway.. */
         mapRegs_MIPSAMode(m, i->Min.EvCheck.amCounter);
         mapRegs_MIPSAMode(m, i->Min.EvCheck.amFailAddr);
         return;
      case Min_ProfInc:
         /* does not use any registers. */
         return;
      default:
         ppMIPSInstr(i, mode64);
         vpanic("mapRegs_MIPSInstr");
         break;
   }

}

/* Figure out if i represents a reg-reg move, and if so assign the
   source and destination to *src and *dst.  If in doubt say No.  Used
   by the register allocator to do move coalescing. 
*/
Bool isMove_MIPSInstr(MIPSInstr * i, HReg * src, HReg * dst)
{
   /* Moves between integer regs */
   if (i->tag == Min_Alu) {
      // or Rd,Rs,Rs == mr Rd,Rs
      if (i->Min.Alu.op != Malu_OR)
         return False;
      if (i->Min.Alu.srcR->tag != Mrh_Reg)
         return False;
      if (i->Min.Alu.srcR->Mrh.Reg.reg != i->Min.Alu.srcL)
         return False;
      *src = i->Min.Alu.srcL;
      *dst = i->Min.Alu.dst;
      return True;
   }
   return False;
}

/* Generate mips spill/reload instructions under the direction of the
   register allocator.
*/
void genSpill_MIPS( /*OUT*/ HInstr ** i1, /*OUT*/ HInstr ** i2, HReg rreg,
                    Int offsetB, Bool mode64)
{
   MIPSAMode *am;
   vassert(offsetB >= 0);
   vassert(!hregIsVirtual(rreg));
   *i1 = *i2 = NULL;
   am = MIPSAMode_IR(offsetB, GuestStatePointer(mode64));

   switch (hregClass(rreg)) {
      case HRcInt64:
         vassert(mode64);
         *i1 = MIPSInstr_Store(8, am, rreg, mode64);
         break;
      case HRcInt32:
         vassert(!mode64);
         *i1 = MIPSInstr_Store(4, am, rreg, mode64);
         break;
      case HRcFlt32:
         vassert(!mode64);
         *i1 = MIPSInstr_FpLdSt(False /*Store */ , 4, rreg, am);
         break;
      case HRcFlt64:
         *i1 = MIPSInstr_FpLdSt(False /*Store */ , 8, rreg, am);
         break;
      default:
         ppHRegClass(hregClass(rreg));
         vpanic("genSpill_MIPS: unimplemented regclass");
         break;
   }
}

void genReload_MIPS( /*OUT*/ HInstr ** i1, /*OUT*/ HInstr ** i2, HReg rreg,
                     Int offsetB, Bool mode64)
{
   MIPSAMode *am;
   vassert(!hregIsVirtual(rreg));
   am = MIPSAMode_IR(offsetB, GuestStatePointer(mode64));

   switch (hregClass(rreg)) {
      case HRcInt64:
         vassert(mode64);
         *i1 = MIPSInstr_Load(8, rreg, am, mode64);
         break;
      case HRcInt32:
         vassert(!mode64);
         *i1 = MIPSInstr_Load(4, rreg, am, mode64);
         break;
      case HRcFlt32:
         if (mode64)
            *i1 = MIPSInstr_FpLdSt(True /*Load */ , 8, rreg, am);
         else
            *i1 = MIPSInstr_FpLdSt(True /*Load */ , 4, rreg, am);
         break;
      case HRcFlt64:
         *i1 = MIPSInstr_FpLdSt(True /*Load */ , 8, rreg, am);
         break;
      default:
         ppHRegClass(hregClass(rreg));
         vpanic("genReload_MIPS: unimplemented regclass");
         break;
   }
}

/* --------- The mips assembler --------- */

static UInt iregNo(HReg r, Bool mode64)
{
   UInt n;
   vassert(hregClass(r) == mode64 ? HRcInt64 : HRcInt32);
   vassert(!hregIsVirtual(r));
   n = hregNumber(r);
   vassert(n <= 32);
   return n;
}

static UChar fregNo(HReg r, Bool mode64)
{
   UInt n;
   vassert(hregClass(r) == mode64 ? HRcFlt64 : HRcFlt32);
   vassert(!hregIsVirtual(r));
   n = hregNumber(r);
   vassert(n <= 31);
   return n;
}

static UChar dregNo(HReg r)
{
   UInt n;
   vassert(hregClass(r) == HRcFlt64);
   vassert(!hregIsVirtual(r));
   n = hregNumber(r);
   vassert(n <= 31);
   return n;
}

/* Emit 32bit instruction */
static UChar *emit32(UChar * p, UInt w32)
{
#if defined (_MIPSEL)
   *p++ = toUChar(w32 & 0x000000FF);
   *p++ = toUChar((w32 >> 8) & 0x000000FF);
   *p++ = toUChar((w32 >> 16) & 0x000000FF);
   *p++ = toUChar((w32 >> 24) & 0x000000FF);
#elif defined (_MIPSEB)
   *p++ = toUChar((w32 >> 24) & 0x000000FF);
   *p++ = toUChar((w32 >> 16) & 0x000000FF);
   *p++ = toUChar((w32 >> 8) & 0x000000FF);
   *p++ = toUChar(w32 & 0x000000FF);
#endif
   return p;
}
/* Fetch an instruction */
static UInt fetch32 ( UChar* p )
{
   UInt w32 = 0;
#if defined (_MIPSEL)
   w32 |= ((0xFF & (UInt)p[0]) << 0);
   w32 |= ((0xFF & (UInt)p[1]) << 8);
   w32 |= ((0xFF & (UInt)p[2]) << 16);
   w32 |= ((0xFF & (UInt)p[3]) << 24);
#elif defined (_MIPSEB)
   w32 |= ((0xFF & (UInt)p[0]) << 24);
   w32 |= ((0xFF & (UInt)p[1]) << 16);
   w32 |= ((0xFF & (UInt)p[2]) <<  8);
   w32 |= ((0xFF & (UInt)p[3]) <<  0);
#endif
   return w32;
}

/* physical structure of mips instructions */
/* type I : opcode    - 6 bits 
         rs         - 5 bits
         rt         - 5 bits
         immediate - 16 bits
*/
static UChar *mkFormI(UChar * p, UInt opc, UInt rs, UInt rt, UInt imm)
{
   UInt theInstr;
   vassert(opc < 0x40);
   vassert(rs < 0x20);
   vassert(rt < 0x20);
   imm = imm & 0xFFFF;
   theInstr = ((opc << 26) | (rs << 21) | (rt << 16) | (imm));
   return emit32(p, theInstr);
}

/* type R: opcode    - 6 bits
         rs    - 5 bits
         rt    - 5 bits
         rd    - 5 bits
         sa    - 5 bits
         func  - 6 bits
*/
static UChar *mkFormR(UChar * p, UInt opc, UInt rs, UInt rt, UInt rd, UInt sa,
            UInt func)
{
   if (rs >= 0x20)
      vex_printf("rs = %d\n", rs);
   UInt theInstr;
   vassert(opc < 0x40);
   vassert(rs < 0x20);
   vassert(rt < 0x20);
   vassert(rd < 0x20);
   vassert(sa < 0x20);
   func = func & 0xFFFF;
   theInstr = ((opc << 26) | (rs << 21) | (rt << 16) | (rd << 11) | (sa << 6) |
               (func));

   return emit32(p, theInstr);
}

static UChar *mkFormS(UChar * p, UInt opc1, UInt rRD, UInt rRS, UInt rRT,
                      UInt sa, UInt opc2)
{
   UInt theInstr;
   vassert(opc1 <= 0x3F);
   vassert(rRD < 0x20);
   vassert(rRS < 0x20);
   vassert(rRT < 0x20);
   vassert(opc2 <= 0x3F);
   vassert(sa >= 0 && sa <= 0x3F);

   theInstr = ((opc1 << 26) | (rRS << 21) | (rRT << 16) | (rRD << 11) |
              ((sa & 0x1F) << 6) | (opc2));

   return emit32(p, theInstr);
}

static UChar *doAMode_IR(UChar * p, UInt opc1, UInt rSD, MIPSAMode * am,
                         Bool mode64)
{
   UInt rA, idx, r_dst;
   vassert(am->tag == Mam_IR);
   vassert(am->Mam.IR.index < 0x10000);

   rA = iregNo(am->Mam.IR.base, mode64);
   idx = am->Mam.IR.index;

   if (rSD == 33 || rSD == 34)
      r_dst = 24;
   else
      r_dst = rSD;

   if (opc1 < 40) {
      //load
      if (rSD == 33)
         /* mfhi */
         p = mkFormR(p, 0, 0, 0, r_dst, 0, 16);
      else if (rSD == 34)
         /* mflo */
         p = mkFormR(p, 0, 0, 0, r_dst, 0, 18);
   }

   p = mkFormI(p, opc1, rA, r_dst, idx);

   if (opc1 >= 40) {
      //store
      if (rSD == 33)
         /* mthi */
         p = mkFormR(p, 0, r_dst, 0, 0, 0, 17);
      else if (rSD == 34)
         /* mtlo */
         p = mkFormR(p, 0, r_dst, 0, 0, 0, 19);
   }

   return p;
}

static UChar *doAMode_RR(UChar * p, UInt opc1, UInt rSD, MIPSAMode * am,
                         Bool mode64)
{
   UInt rA, rB, r_dst;
   vassert(am->tag == Mam_RR);

   rA = iregNo(am->Mam.RR.base, mode64);
   rB = iregNo(am->Mam.RR.index, mode64);

   if (rSD == 33 || rSD == 34)
      r_dst = 24;
   else
      r_dst = rSD;

   if (opc1 < 40) {
      //load
      if (rSD == 33)
         /* mfhi */
         p = mkFormR(p, 0, 0, 0, r_dst, 0, 16);
      else if (rSD == 34)
         /* mflo */
         p = mkFormR(p, 0, 0, 0, r_dst, 0, 18);
   }
   /* addiu sp, sp, -4
    * sw rA, 0(sp)
    * addu rA, rA, rB 
    * sw/lw r_dst, 0(rA)
    * lw rA, 0(sp) 
    * addiu sp, sp, 4 */
   if (mode64) {
      p = mkFormI(p, 25, 29, 29, 0xFFFC);
      p = mkFormI(p, 63, 29, rA, 0);
      p = mkFormR(p, 0, rA, rB, rA, 0, 45);
      p = mkFormI(p, opc1, rA, r_dst, 0);
      p = mkFormI(p, 55, 29, rA, 0);
      p = mkFormI(p, 25, 29, 29, 4);
   } else {
      p = mkFormI(p, 9, 29, 29, 0xFFFC);
      p = mkFormI(p, 43, 29, rA, 0);
      p = mkFormR(p, 0, rA, rB, rA, 0, 33);
      p = mkFormI(p, opc1, rA, r_dst, 0);
      p = mkFormI(p, 35, 29, rA, 0);
      p = mkFormI(p, 9, 29, 29, 4);
   }
   if (opc1 >= 40) {
      //store
      if (rSD == 33)
         /* mthi */
         p = mkFormR(p, 0, r_dst, 0, 0, 0, 17);
      else if (rSD == 34)
         /* mtlo */
         p = mkFormR(p, 0, r_dst, 0, 0, 0, 19);
   }

   return p;
}

/* Load imm to r_dst */
static UChar *mkLoadImm(UChar * p, UInt r_dst, ULong imm, Bool mode64)
{
   if (!mode64) {
      vassert(r_dst < 0x20);
      UInt u32 = (UInt) imm;
      Int s32 = (Int) u32;
      Long s64 = (Long) s32;
      imm = (ULong) s64;
   }

   if (imm >= 0xFFFFFFFFFFFF8000ULL || imm < 0x8000) {
      // sign-extendable from 16 bits
      // addiu r_dst,0,imm  => li r_dst,imm
      p = mkFormI(p, 9, 0, r_dst, imm & 0xFFFF);
   } else {
      if (imm >= 0xFFFFFFFF80000000ULL || imm < 0x80000000ULL) {
         // sign-extendable from 32 bits
         // addiu r_dst,r0,(imm>>16) => lis r_dst, (imm>>16)
         // lui r_dst, (imm>>16)
         p = mkFormI(p, 15, 0, r_dst, (imm >> 16) & 0xFFFF);
         // ori r_dst, r_dst, (imm & 0xFFFF)
         p = mkFormI(p, 13, r_dst, r_dst, imm & 0xFFFF);
      } else {
         vassert(mode64);
         // lui load in upper half of low word
         p = mkFormI(p, 15, 0, r_dst, (imm >> 48) & 0xFFFF);
         // ori
         p = mkFormI(p, 13, r_dst, r_dst, (imm >> 32) & 0xFFFF);
         //shift
         p = mkFormS(p, 0, r_dst, 0, r_dst, 16, 56);
         // ori
         p = mkFormI(p, 13, r_dst, r_dst, (imm >> 16) & 0xFFFF);
         //shift
         p = mkFormS(p, 0, r_dst, 0, r_dst, 16, 56);
         // ori
         p = mkFormI(p, 13, r_dst, r_dst, imm & 0xFFFF);
      }
   }
   return p;
}

/* A simplified version of mkLoadImm that always generates 2 or 5
   instructions (32 or 64 bits respectively) even if it could generate
   fewer.  This is needed for generating fixed sized patchable
   sequences. */
static UChar* mkLoadImm_EXACTLY2or5 ( UChar* p,
                                      UInt r_dst, ULong imm, Bool mode64 )
{
   vassert(r_dst < 0x20);

   if (!mode64) {
      /* In 32-bit mode, make sure the top 32 bits of imm are a sign
         extension of the bottom 32 bits.  (Probably unnecessary.) */
      UInt u32 = (UInt)imm;
      Int  s32 = (Int)u32;
      Long s64 = (Long)s32;
      imm = (ULong)s64;
   }

   if (!mode64) {
      // sign-extendable from 32 bits
      // addiu r_dst,r0,(imm>>16) => lis r_dst, (imm>>16)
      // lui r_dst, (imm>>16)
      p = mkFormI(p, 15, 0, r_dst, (imm >> 16) & 0xFFFF);
      // ori r_dst, r_dst, (imm & 0xFFFF)
      p = mkFormI(p, 13, r_dst, r_dst, imm & 0xFFFF);
   } else {
      vassert(0);
   }
   return p;
}

/* Checks whether the sequence of bytes at p was indeed created
   by mkLoadImm_EXACTLY2or5 with the given parameters. */
static Bool isLoadImm_EXACTLY2or5 ( UChar* p_to_check,
                                    UInt r_dst, ULong imm, Bool mode64 )
{
   vassert(r_dst < 0x20);
   Bool ret;
   if (!mode64) {
      /* In 32-bit mode, make sure the top 32 bits of imm are a sign
         extension of the bottom 32 bits.  (Probably unnecessary.) */
      UInt u32 = (UInt)imm;
      Int  s32 = (Int)u32;
      Long s64 = (Long)s32;
      imm = (ULong)s64;
   }

   if (!mode64) {
      UInt   expect[2] = { 0, 0 };
      UChar* p         = (UChar*)&expect[0];
      // lui r_dst, (imm>>16)
      p = mkFormI(p, 15, 0, r_dst, (imm >> 16) & 0xFFFF);
      // ori r_dst, r_dst, (imm & 0xFFFF)
      p = mkFormI(p, 13, r_dst, r_dst, imm & 0xFFFF);
      vassert(p == (UChar*)&expect[2]);

      ret = fetch32(p_to_check + 0) == expect[0]
             && fetch32(p_to_check + 4) == expect[1];

   } else {
      vassert(0);
   }
   return ret;
}

/* Generate a machine-word sized load or store.  Simplified version of
   the Min_Load and Min_Store cases below. */
static UChar* do_load_or_store_machine_word ( 
                 UChar* p, Bool isLoad,
                 UInt reg, MIPSAMode* am, Bool mode64 )
{
   if (isLoad) { /* load */
      UInt opc1, sz = mode64 ? 8 : 4;
      switch (am->tag) {
         case Mam_IR:
            if (mode64) {
               vassert(0 == (am->Mam.IR.index & 3));
            }
            switch (sz) {
               case 1:
                  opc1 = 32;
                  break;
               case 2:
                  opc1 = 33;
                  break;
               case 4:
                  opc1 = 35;
                  break;
               case 8:
                  opc1 = 55;
                  vassert(mode64);
                  break;
               default:
                  vassert(0);
                  break;
            }
            p = doAMode_IR(p, opc1, reg, am, mode64);
            break;
         case Mam_RR:
            /* we could handle this case, but we don't expect to ever
               need to. */
            vassert(0);
            break;
         default:
            vassert(0);
            break;
      }
   } else /* store */ {
      UInt opc1, sz = mode64 ? 8 : 4;
      switch (am->tag) {
         case Mam_IR:
            if (mode64) {
               vassert(0 == (am->Mam.IR.index & 3));
            }
            switch (sz) {
               case 1:
                  opc1 = 40;
                  break;
               case 2:
                  opc1 = 41;
                  break;
               case 4:
                  opc1 = 43;
                  break;
               case 8:
                  vassert(mode64);
                  opc1 = 63;
                  break;
               default:
                  vassert(0);
                  break;
            }
            p = doAMode_IR(p, opc1, reg, am, mode64);
            break;
         case Mam_RR:
            /* we could handle this case, but we don't expect to ever
               need to. */
            vassert(0);
            break;
         default:
            vassert(0);
            break;
      }
   }
   return p;
}

/* Move r_dst to r_src */
static UChar *mkMoveReg(UChar * p, UInt r_dst, UInt r_src)
{
   vassert(r_dst < 0x20);
   vassert(r_src < 0x20);

   if (r_dst != r_src) {
      /* or r_dst, r_src, r_src */
      p = mkFormR(p, 0, r_src, r_src, r_dst, 0, 37);
   }
   return p;
}

/* Emit an instruction into buf and return the number of bytes used.
   Note that buf is not the insn's final place, and therefore it is
   imperative to emit position-independent code.  If the emitted
   instruction was a profiler inc, set *is_profInc to True, else
   leave it unchanged. */
Int emit_MIPSInstr ( /*MB_MOD*/Bool* is_profInc,
                     UChar* buf, Int nbuf, MIPSInstr* i, 
                     Bool mode64,
                     void* disp_cp_chain_me_to_slowEP,
                     void* disp_cp_chain_me_to_fastEP,
                     void* disp_cp_xindir,
                     void* disp_cp_xassisted )
{
   UChar *p = &buf[0];
   UChar *ptmp = p;
   vassert(nbuf >= 32);

   switch (i->tag) {
      case Min_MovCond: {
         MIPSRH *srcR = i->Min.MovCond.srcR;
         UInt condR = iregNo(i->Min.MovCond.condR, mode64);
         UInt dst = iregNo(i->Min.MovCond.dst, mode64);

         UInt srcL = iregNo(i->Min.MovCond.srcL, mode64);

         p = mkMoveReg(p, dst, srcL);
         if (i->Min.MovCond.cond == MIPScc_MI) {
            p = mkFormI(p, 7, condR, 0, 2);  //bgtz cond,2
         }

         p = mkFormR(p, 0, 0, 0, 0, 0, 0);   //nop
   
         if (srcR->tag == Mrh_Reg) {
            //or dst,src,src
            p = mkMoveReg(p, dst, iregNo(srcR->Mrh.Reg.reg, mode64));
            /*p = mkFormR(p, 0, dst, iregNo(src->Mrh.Reg.reg, mode64),
                        iregNo(src->Mrh.Reg.reg, mode64), 0, 37);*/
         } else {
            p = mkLoadImm(p, dst, srcR->Mrh.Imm.imm16, mode64);
         }
      }
         goto done;
   
      case Min_LI:
         p = mkLoadImm(p, iregNo(i->Min.LI.dst, mode64), i->Min.LI.imm, mode64);
         goto done;
   
      case Min_Alu: {
         MIPSRH *srcR = i->Min.Alu.srcR;
         Bool immR = toBool(srcR->tag == Mrh_Imm);
         UInt r_dst = iregNo(i->Min.Alu.dst, mode64);
         UInt r_srcL = iregNo(i->Min.Alu.srcL, mode64);
         UInt r_srcR = immR ? (-1) /*bogus */ : iregNo(srcR->Mrh.Reg.reg, mode64);
   
         switch (i->Min.Alu.op) {
            /*Malu_ADD, Malu_SUB, Malu_AND, Malu_OR, Malu_NOR, Malu_XOR */
            case Malu_ADD:
               if (immR) {
                  vassert(srcR->Mrh.Imm.imm16 != 0x8000);
                  if (srcR->Mrh.Imm.syned)
                     /* addi */
                     p = mkFormI(p, 9, r_srcL, r_dst, srcR->Mrh.Imm.imm16);
                  else
                     /* addiu */
                     p = mkFormI(p, 9, r_srcL, r_dst, srcR->Mrh.Imm.imm16);
               } else {
                  /* addu */
                  p = mkFormR(p, 0, r_srcL, r_srcR, r_dst, 0, 33);
               }
               break;
            case Malu_SUB:
               if (immR) {
                  /* addi , but with negated imm */
                  vassert(srcR->Mrh.Imm.syned);
                  vassert(srcR->Mrh.Imm.imm16 != 0x8000);
                  p = mkFormI(p, 8, r_srcL, r_dst, (-srcR->Mrh.Imm.imm16));
               } else {
                  /* subu */
                  p = mkFormR(p, 0, r_srcL, r_srcR, r_dst, 0, 35);
               }
               break;
            case Malu_AND:
               if (immR) {
                  /* andi */
                  vassert(!srcR->Mrh.Imm.syned);
                  p = mkFormI(p, 12, r_srcL, r_dst, srcR->Mrh.Imm.imm16);
               } else {
                  /* and */
                  p = mkFormR(p, 0, r_srcL, r_srcR, r_dst, 0, 36);
               }
               break;
            case Malu_OR:
               if (immR) {
                  /* ori */
                  vassert(!srcR->Mrh.Imm.syned);
                  p = mkFormI(p, 13, r_srcL, r_dst, srcR->Mrh.Imm.imm16);
               } else {
                  /* or */
                  if (r_srcL == 33)
                     //MFHI
                     p = mkFormR(p, 0, 0, 0, r_dst, 0, 16);
                  else if (r_srcL == 34)
                     //MFLO
                     p = mkFormR(p, 0, 0, 0, r_dst, 0, 18);
                  else if (r_dst == 33)
                     //MTHI
                     p = mkFormR(p, 0, r_srcL, 0, 0, 0, 17);
                  else if (r_dst == 34)
                     //MTLO
                     p = mkFormR(p, 0, r_srcL, 0, 0, 0, 19);
                  else
                     p = mkFormR(p, 0, r_srcL, r_srcR, r_dst, 0, 37);
               }
               break;
            case Malu_NOR:
               /* nor */
               vassert(!immR);
               p = mkFormR(p, 0, r_srcL, r_srcR, r_dst, 0, 39);
               break;
            case Malu_XOR:
               if (immR) {
                  /* xori */
                  vassert(!srcR->Mrh.Imm.syned);
                  p = mkFormI(p, 14, r_srcL, r_dst, srcR->Mrh.Imm.imm16);
               } else {
                  /* xor */
                  p = mkFormR(p, 0, r_srcL, r_srcR, r_dst, 0, 38);
               }
               break;
      
            default:
               goto bad;
         }
         goto done;
      }

      case Min_Shft: {
         MIPSRH *srcR = i->Min.Shft.srcR;
         Bool sz32 = i->Min.Shft.sz32;
         Bool immR = toBool(srcR->tag == Mrh_Imm);
         UInt r_dst = iregNo(i->Min.Shft.dst, mode64);
         UInt r_srcL = iregNo(i->Min.Shft.srcL, mode64);
         UInt r_srcR = immR ? (-1) /*bogus */ : iregNo(srcR->Mrh.Reg.reg,
                                                       mode64);
         if (!mode64)
            vassert(sz32);
         switch (i->Min.Shft.op) {
            case Mshft_SLL:
               if (sz32) {
                  if (immR) {
                     UInt n = srcR->Mrh.Imm.imm16;
                     vassert(n >= 0 && n < 32);
                     p = mkFormS(p, 0, r_dst, 0, r_srcL, n, 0);
                  } else {
                     /* shift variable */
                     p = mkFormS(p, 0, r_dst, r_srcR, r_srcL, 0, 4);
                  }
               } else {
                  if (immR) {
                     UInt n = srcR->Mrh.Imm.imm16;
                     vassert((n >= 0 && n < 32) || (n > 31 && n < 64));
                     if (n >= 0 && n < 32) {
                        p = mkFormS(p, 0, r_dst, 0, r_srcL, n, 56);
                     } else {
                        p = mkFormS(p, 0, r_dst, 0, r_srcL, n - 32, 60);
                     }
                  } else {
                     p = mkFormS(p, 0, r_dst, r_srcR, r_srcL, 0, 20);
                  }
               }
               break;
   
            case Mshft_SRL:
               if (sz32) {
                  // SRL, SRLV
                  if (immR) {
                     UInt n = srcR->Mrh.Imm.imm16;
                     vassert(n >= 0 && n < 32);
                     p = mkFormS(p, 0, r_dst, 0, r_srcL, n, 2);
                  } else {
                     /* shift variable */
                     p = mkFormS(p, 0, r_dst, r_srcR, r_srcL, 0, 6);
                  }
               } else {
                  // DSRL, DSRL32, DSRLV
                  if (immR) {
                     UInt n = srcR->Mrh.Imm.imm16;
                     vassert((n >= 0 && n < 32) || (n > 31 && n < 64));
                     if (n >= 0 && n < 32) {
                        p = mkFormS(p, 0, r_dst, 0, r_srcL, n, 58);
                     } else {
                        p = mkFormS(p, 0, r_dst, 0, r_srcL, n - 32, 62);
                     }
                  } else {
                     p = mkFormS(p, 0, r_dst, r_srcR, r_srcL, 0, 22);
                  }
               }
               break;
   
            case Mshft_SRA:
               if (sz32) {
                  // SRA, SRAV
                  if (immR) {
                     UInt n = srcR->Mrh.Imm.imm16;
                     vassert(n >= 0 && n < 32);
                     p = mkFormS(p, 0, r_dst, 0, r_srcL, n, 3);
                  } else {
                     /* shift variable */
                     p = mkFormS(p, 0, r_dst, r_srcR, r_srcL, 0, 7);
                  }
               } else {
                  // DSRA, DSRA32, DSRAV
                  if (immR) {
                     UInt n = srcR->Mrh.Imm.imm16;
                     vassert((n >= 0 && n < 32) || (n > 31 && n < 64));
                     if (n >= 0 && n < 32) {
                        p = mkFormS(p, 0, r_dst, 0, r_srcL, n, 59);
                     } else {
                        p = mkFormS(p, 0, r_dst, 0, r_srcL, n - 32, 63);
                     }
                  } else {
                     p = mkFormS(p, 0, r_dst, r_srcR, r_srcL, 0, 23);
                  }
               }
               break;
   
            default:
               goto bad;
         }

         goto done;
      }
   
      case Min_Unary: {
         UInt r_dst = iregNo(i->Min.Unary.dst, mode64);
         UInt r_src = iregNo(i->Min.Unary.src, mode64);

         switch (i->Min.Unary.op) {
            /*Mun_CLO, Mun_CLZ, Mun_NOP */
            case Mun_CLO:  //clo
               p = mkFormR(p, 28, r_src, 0 /*whatever */ , r_dst, 0, 33);
               break;
            case Mun_CLZ:  //clz
               p = mkFormR(p, 28, r_src, 0 /*whatever */ , r_dst, 0, 32);
               break;
            case Mun_NOP:  //nop (sll r0,r0,0)
               p = mkFormR(p, 0, 0, 0, 0, 0, 0);
               break;
         }
         goto done;
      }
   
      case Min_Cmp: {
         UInt r_srcL = iregNo(i->Min.Cmp.srcL, mode64);
         UInt r_srcR = iregNo(i->Min.Cmp.srcR, mode64);
         UInt r_dst = iregNo(i->Min.Cmp.dst, mode64);

         switch (i->Min.Cmp.cond) {
            case MIPScc_EQ:
               /*  addiu r_dst, r0, 1
                  beq r_srcL, r_srcR, 2
                  nop
                  addiu r_dst, r0, 0
                */
               p = mkFormI(p, 9, 0, r_dst, 1);
               p = mkFormI(p, 4, r_srcL, r_srcR, 2);
               p = mkFormR(p, 0, 0, 0, 0, 0, 0);
               p = mkFormI(p, 9, 0, r_dst, 0);
               break;
            case MIPScc_NE:
               /*  addiu r_dst, r0, 1
                  bne r_srcL, r_srcR, 2
                  nop
                  addiu r_dst, r0, 0
                */
               p = mkFormI(p, 9, 0, r_dst, 1);
               p = mkFormI(p, 5, r_srcL, r_srcR, 2);
               p = mkFormR(p, 0, 0, 0, 0, 0, 0);
               p = mkFormI(p, 9, 0, r_dst, 0);
               break;
            case MIPScc_LT:
               /*  slt r_dst, r_srcL, r_srcR */
               p = mkFormR(p, 0, r_srcL, r_srcR, r_dst, 0, 42);
               break;
            case MIPScc_LO:
               /*  sltu r_dst, r_srcL, r_srcR */
               p = mkFormR(p, 0, r_srcL, r_srcR, r_dst, 0, 43);
               break;
            case MIPScc_LE:
               /*  addiu r_dst, r0, 1
                  beq r_srcL, r_srcR, 2
                  nop
                  slt r_dst, r_srcL, r_srcR */
               p = mkFormI(p, 9, 0, r_dst, 1);
               p = mkFormI(p, 4, r_srcL, r_srcR, 2);
               p = mkFormR(p, 0, 0, 0, 0, 0, 0);
               p = mkFormR(p, 0, r_srcL, r_srcR, r_dst, 0, 42);
               break;
            case MIPScc_LS:
               /*  addiu r_dst, r0, 1
                  beq r_srcL, r_srcR, 2
                  nop
                  sltu r_dst, r_srcL, r_srcR */
               p = mkFormI(p, 9, 0, r_dst, 1);
               p = mkFormI(p, 4, r_srcL, r_srcR, 2);
               p = mkFormR(p, 0, 0, 0, 0, 0, 0);
               p = mkFormR(p, 0, r_srcL, r_srcR, r_dst, 0, 43);
               break;
            default:
               goto bad;
         }
         goto done;
      }
   
      case Min_Mul: {
         Bool syned = i->Min.Mul.syned;
         Bool widening = i->Min.Mul.widening;
         Bool sz32 = i->Min.Mul.sz32;
         UInt r_srcL = iregNo(i->Min.Mul.srcL, mode64);
         UInt r_srcR = iregNo(i->Min.Mul.srcR, mode64);
         UInt r_dst = iregNo(i->Min.Mul.dst, mode64);

         if (widening) {
            if (sz32) {
               if (syned)
                  /* mult */
                  p = mkFormR(p, 0, r_srcL, r_srcR, 0, 0, 24);
               else
                  /* multu */
                  p = mkFormR(p, 0, r_srcL, r_srcR, 0, 0, 25);
            } else {
               if (syned)  /* DMULT  r_dst,r_srcL,r_srcR */
                  p = mkFormR(p, 0, r_srcL, r_srcR, 0, 0, 28);
               else  /* DMULTU r_dst,r_srcL,r_srcR */
                  p = mkFormR(p, 0, r_srcL, r_srcR, 0, 0, 29);
            }
         } else {
            if (sz32)
               /* mul */
               p = mkFormR(p, 28, r_srcL, r_srcR, r_dst, 0, 2);
            else if (mode64 && !sz32)
               p = mkFormR(p, 28, r_srcL, r_srcR, r_dst, 0, 2);
            else
               goto bad;
         }
         goto done;
      }
   
      case Min_Macc: {
         Bool syned = i->Min.Macc.syned;
         UInt r_srcL = iregNo(i->Min.Macc.srcL, mode64);
         UInt r_srcR = iregNo(i->Min.Macc.srcR, mode64);

         if (syned) {
            switch (i->Min.Macc.op) {
               case Macc_ADD:
                  //madd
                  p = mkFormR(p, 28, r_srcL, r_srcR, 0, 0, 0);
                  break;
               case Macc_SUB:
                  //msub
                  p = mkFormR(p, 28, r_srcL, r_srcR, 0, 0,
                         4);
                  break;
               default:
                  goto bad;
            }
         } else {
            switch (i->Min.Macc.op) {
               case Macc_ADD:
                  //maddu
                  p = mkFormR(p, 28, r_srcL, r_srcR, 0, 0,
                         1);
                  break;
               case Macc_SUB:
                  //msubu
                  p = mkFormR(p, 28, r_srcL, r_srcR, 0, 0,
                         5);
                  break;
               default:
                  goto bad;
            }
         }

         goto done;
      }

      case Min_Div: {
         Bool syned = i->Min.Div.syned;
         Bool sz32 = i->Min.Div.sz32;
         UInt r_srcL = iregNo(i->Min.Div.srcL, mode64);
         UInt r_srcR = iregNo(i->Min.Div.srcR, mode64);
         if (sz32) {
            if (syned) {
               /* div */
               p = mkFormR(p, 0, r_srcL, r_srcR, 0, 0, 26);
            } else
               /* divu */
               p = mkFormR(p, 0, r_srcL, r_srcR, 0, 0, 27);
            goto done;
         } else {
            if (syned) {
               /* ddiv */
               p = mkFormR(p, 0, r_srcL, r_srcR, 0, 0, 30);
            } else
               /* ddivu */
               p = mkFormR(p, 0, r_srcL, r_srcR, 0, 0, 31);
            goto done;
         }
      }
   
      case Min_Mthi: {
         UInt r_src = iregNo(i->Min.MtHL.src, mode64);
         p = mkFormR(p, 0, r_src, 0, 0, 0, 17);
         goto done;
      }
   
      case Min_Mtlo: {
         UInt r_src = iregNo(i->Min.MtHL.src, mode64);
         p = mkFormR(p, 0, r_src, 0, 0, 0, 19);
         goto done;
      }
   
      case Min_Mfhi: {
         UInt r_dst = iregNo(i->Min.MfHL.dst, mode64);
         p = mkFormR(p, 0, 0, 0, r_dst, 0, 16);
         goto done;
      }
   
      case Min_Mflo: {
         UInt r_dst = iregNo(i->Min.MfHL.dst, mode64);
         p = mkFormR(p, 0, 0, 0, r_dst, 0, 18);
         goto done;
      }
   
      case Min_MtFCSR: {
         UInt r_src = iregNo(i->Min.MtFCSR.src, mode64);
         /* ctc1 */
         p = mkFormR(p, 17, 6, r_src, 31, 0, 0);
         goto done;
      }
   
      case Min_MfFCSR: {
         UInt r_dst = iregNo(i->Min.MfFCSR.dst, mode64);
         /* cfc1 */
         p = mkFormR(p, 17, 2, r_dst, 31, 0, 0);
         goto done;
      }
   
      case Min_Call: {
         MIPSCondCode cond = i->Min.Call.cond;
         UInt r_dst = 25;  /* using %r25 as address temporary - 
                     see getRegUsage_MIPSInstr */

         /* jump over the following insns if condition does not hold */
         if (cond != MIPScc_AL) {
            /* jmp fwds if !condition */
            /* don't know how many bytes to jump over yet...
               make space for a jump instruction + nop!!! and fill in later. */
            ptmp = p;   /* fill in this bit later */
            p += 8;  // p += 8
         }

         /* load target to r_dst */// p += 4|8
         p = mkLoadImm(p, r_dst, i->Min.Call.target, mode64);

         /* jalr %r_dst */
         p = mkFormR(p, 0, r_dst, 0, 31, 0, 9); // p += 4
         p = mkFormR(p, 0, 0, 0, 0, 0, 0);   // p += 4

         /* Fix up the conditional jump, if there was one. */
         if (cond != MIPScc_AL) {
            UInt r_src = iregNo(i->Min.Call.src, mode64);
            Int delta = p - ptmp;

            vassert(delta >= 20 && delta <= 32);
            /* bc !ct,cf,delta/4 */
            /* blez r_src, delta/4-1 */
            vassert(cond == MIPScc_EQ);
            ptmp = mkFormI(ptmp, 6, r_src, 0, delta / 4 - 1);
            ptmp = mkFormR(ptmp, 0, 0, 0, 0, 0, 0);
         }
         goto done;
      }

      case Min_XDirect: {
         /* NB: what goes on here has to be very closely coordinated
            with the chainXDirect_MIPS and unchainXDirect_MIPS below. */
         /* We're generating chain-me requests here, so we need to be
            sure this is actually allowed -- no-redir translations
            can't use chain-me's.  Hence: */
         vassert(disp_cp_chain_me_to_slowEP != NULL);
         vassert(disp_cp_chain_me_to_fastEP != NULL);

         /* Use ptmp for backpatching conditional jumps. */
         ptmp = NULL;

         /* First off, if this is conditional, create a conditional
            jump over the rest of it.  Or at least, leave a space for
            it that we will shortly fill in. */
         if (i->Min.XDirect.cond != MIPScc_AL) {
            vassert(i->Min.XDirect.cond != MIPScc_NV);
            ptmp = p;
            p += 12;
         }

         /* Update the guest PC. */
         /* move r9, dstGA */
         /* sw r9, amPC */
         p = mkLoadImm_EXACTLY2or5(p, /*r*/9,
                                  (ULong)i->Min.XDirect.dstGA, mode64);
         p = do_load_or_store_machine_word(p, False/*!isLoad*/,
                                /*r*/9, i->Min.XDirect.amPC, mode64);

         /* --- FIRST PATCHABLE BYTE follows --- */
         /* VG_(disp_cp_chain_me_to_{slowEP,fastEP}) (where we're
            calling to) backs up the return address, so as to find the
            address of the first patchable byte.  So: don't change the
            number of instructions (3) below. */
         /* move r9, VG_(disp_cp_chain_me_to_{slowEP,fastEP}) */
         /* jr  r9  */
         void* disp_cp_chain_me
                  = i->Min.XDirect.toFastEP ? disp_cp_chain_me_to_fastEP 
                                              : disp_cp_chain_me_to_slowEP;
         p = mkLoadImm_EXACTLY2or5(p, /*r*/9,
                                     Ptr_to_ULong(disp_cp_chain_me), mode64);
         /* jalr $9 */
         /* nop */
         p = mkFormR(p, 0, 9, 0, 31, 0, 9); // p += 4
         p = mkFormR(p, 0, 0, 0, 0, 0, 0);   // p += 4
         /* --- END of PATCHABLE BYTES --- */

         /* Fix up the conditional jump, if there was one. */
         if (i->Min.XDirect.cond != MIPScc_AL) {
            Int delta = p - ptmp;
            delta = delta / 4 - 3;
            vassert(delta > 0 && delta < 40);
            /* lw $9, 316($10)  // guest_COND
               beq $9, $0, 2
               nop*/
            ptmp = mkFormI(ptmp, 35, 10, 9, 316);
            ptmp = mkFormI(ptmp, 4, 0, 9, (delta));
            ptmp = mkFormR(ptmp, 0, 0, 0, 0, 0, 0);
         }
         goto done;
      }

      case Min_XIndir: {
         /* We're generating transfers that could lead indirectly to a
            chain-me, so we need to be sure this is actually allowed --
            no-redir translations are not allowed to reach normal
            translations without going through the scheduler.  That means
            no XDirects or XIndirs out from no-redir translations.
            Hence: */
         vassert(disp_cp_xindir != NULL);

         /* Use ptmp for backpatching conditional jumps. */
         ptmp = NULL;

         /* First off, if this is conditional, create a conditional
            jump over the rest of it. */
         if (i->Min.XIndir.cond != MIPScc_AL) {
            vassert(i->Min.XIndir.cond != MIPScc_NV);
            ptmp = p;
            p += 12;
         }

         /* Update the guest PC. */
         /* sw r-dstGA, amPC */
         p = do_load_or_store_machine_word(p, False/*!isLoad*/,
                                           iregNo(i->Min.XIndir.dstGA, mode64),
                                           i->Min.XIndir.amPC, mode64);

         /* move r9, VG_(disp_cp_xindir) */
         /* jalr   r9 */
         /* nop */
         p = mkLoadImm_EXACTLY2or5 ( p, /*r*/9,
                                     Ptr_to_ULong(disp_cp_xindir), mode64);
         p = mkFormR(p, 0, 9, 0, 31, 0, 9); // p += 4
         p = mkFormR(p, 0, 0, 0, 0, 0, 0);   // p += 4

         /* Fix up the conditional jump, if there was one. */
         if (i->Min.XIndir.cond != MIPScc_AL) {
            Int delta = p - ptmp;
            delta = delta / 4 - 3;
            vassert(delta > 0 && delta < 40);
            /* lw $9, 316($10)  // guest_COND
               beq $9, $0, 2
               nop*/
            ptmp = mkFormI(ptmp, 35, 10, 9, 316);
            ptmp = mkFormI(ptmp, 4, 0, 9, (delta));
            ptmp = mkFormR(ptmp, 0, 0, 0, 0, 0, 0);
         }
         goto done;
      }

      case Min_XAssisted: {
         /* First off, if this is conditional, create a conditional jump
            over the rest of it.  Or at least, leave a space for it that
            we will shortly fill in. */
         ptmp = NULL;
         if (i->Min.XAssisted.cond != MIPScc_AL) {
            vassert(i->Min.XAssisted.cond != MIPScc_NV);
            ptmp = p;
            p += 12;
         }

         /* Update the guest PC. */
         /* sw r-dstGA, amPC */
         p = do_load_or_store_machine_word(p, False/*!isLoad*/,
                                           iregNo(i->Min.XIndir.dstGA, mode64),
                                           i->Min.XIndir.amPC, mode64);

         /* imm32/64 r31, $magic_number */
         UInt trcval = 0;
         switch (i->Min.XAssisted.jk) {
            case Ijk_ClientReq:   trcval = VEX_TRC_JMP_CLIENTREQ;   break;
            case Ijk_Sys_syscall: trcval = VEX_TRC_JMP_SYS_SYSCALL; break;
            //case Ijk_Sys_int128:  trcval = VEX_TRC_JMP_SYS_INT128;  break;
            //case Ijk_Yield:       trcval = VEX_TRC_JMP_YIELD;       break;
            case Ijk_EmWarn:      trcval = VEX_TRC_JMP_EMWARN;      break;
            case Ijk_EmFail:      trcval = VEX_TRC_JMP_EMFAIL;      break;
            //case Ijk_MapFail:     trcval = VEX_TRC_JMP_MAPFAIL;     break;
            case Ijk_NoDecode:    trcval = VEX_TRC_JMP_NODECODE;    break;
            case Ijk_TInval:      trcval = VEX_TRC_JMP_TINVAL;      break;
            case Ijk_NoRedir:     trcval = VEX_TRC_JMP_NOREDIR;     break;
            case Ijk_SigTRAP:     trcval = VEX_TRC_JMP_SIGTRAP;     break;
            //case Ijk_SigSEGV:     trcval = VEX_TRC_JMP_SIGSEGV;     break;
            case Ijk_SigBUS:        trcval = VEX_TRC_JMP_SIGBUS;    break;
            case Ijk_Boring:      trcval = VEX_TRC_JMP_BORING;      break;
            /* We don't expect to see the following being assisted. */
            //case Ijk_Ret:
            //case Ijk_Call:
            /* fallthrough */
            default: 
               ppIRJumpKind(i->Min.XAssisted.jk);
               vpanic("emit_MIPSInstr.Min_XAssisted: unexpected jump kind");
         }
         vassert(trcval != 0);
         p = mkLoadImm_EXACTLY2or5(p, /*r*/10, trcval, mode64);

         /* move r9, VG_(disp_cp_xassisted) */
         p = mkLoadImm_EXACTLY2or5(p, /*r*/9,
                          (ULong)Ptr_to_ULong(disp_cp_xassisted), mode64);
         /* jalr $9
             nop */
         p = mkFormR(p, 0, 9, 0, 31, 0, 9); // p += 4
         p = mkFormR(p, 0, 0, 0, 0, 0, 0);   // p += 4

         /* Fix up the conditional jump, if there was one. */
         if (i->Min.XAssisted.cond != MIPScc_AL) {
            Int delta = p - ptmp;
            delta = delta / 4 - 3;
            vassert(delta > 0 && delta < 40);
            /* lw $9, 316($10)  // guest_COND
               beq $9, $0, 2
               nop*/
            ptmp = mkFormI(ptmp, 35, 10, 9, 316);
            ptmp = mkFormI(ptmp, 4, 0, 9, (delta));
            ptmp = mkFormR(ptmp, 0, 0, 0, 0, 0, 0);
         }
         goto done;
      }

      case Min_Load: {
         MIPSAMode *am_addr = i->Min.Load.src;
         if (am_addr->tag == Mam_IR) {
            UInt r_dst = iregNo(i->Min.Load.dst, mode64);
            UInt opc, sz = i->Min.Load.sz;
            if (mode64 && (sz == 4 || sz == 8)) {
               /* should be guaranteed to us by iselWordExpr_AMode */
               vassert(0 == (am_addr->Mam.IR.index & 3));
            }
            switch (sz) {
               case 1:
                  opc = 32;
                  break;
               case 2:
                  opc = 33;
                  break;
               case 4:
                  opc = 35;
                  break;
               case 8:
                  opc = 55;
                  vassert(mode64);
                  break;
               default:
                  goto bad;
            }

            p = doAMode_IR(p, opc, r_dst, am_addr, mode64);
            goto done;
         } else if (am_addr->tag == Mam_RR) {
            UInt r_dst = iregNo(i->Min.Load.dst, mode64);
            UInt opc, sz = i->Min.Load.sz;

            switch (sz) {
               case 1:
                  opc = 32;
                  break;
               case 2:
                  opc = 33;
                  break;
               case 4:
                  opc = 35;
                  break;
               case 8:
                  opc = 55;
                  vassert(mode64);
                  break;
               default:
                  goto bad;
            }

            p = doAMode_RR(p, opc, r_dst, am_addr, mode64);
            goto done;
         }
         break;
      }
   
      case Min_Store: {
         MIPSAMode *am_addr = i->Min.Store.dst;
         if (am_addr->tag == Mam_IR) {
            UInt r_src = iregNo(i->Min.Store.src, mode64);
            UInt opc, sz = i->Min.Store.sz;
            if (mode64 && (sz == 4 || sz == 8)) {
               /* should be guaranteed to us by iselWordExpr_AMode */
               vassert(0 == (am_addr->Mam.IR.index & 3));
            }
            switch (sz) {
               case 1:
                  opc = 40;
                  break;
               case 2:
                  opc = 41;
                  break;
               case 4:
                  opc = 43;
                  break;
               case 8:
                  vassert(mode64);
                  opc = 63;
                  break;
               default:
                  goto bad;
            }

            p = doAMode_IR(p, opc, r_src, am_addr, mode64);
            goto done;
         } else if (am_addr->tag == Mam_RR) {
            UInt r_src = iregNo(i->Min.Store.src, mode64);
            UInt opc, sz = i->Min.Store.sz;

            switch (sz) {
               case 1:
                  opc = 40;
                  break;
               case 2:
                  opc = 41;
                  break;
               case 4:
                  opc = 43;
                  break;
               case 8:
                  vassert(mode64);
                  opc = 63;
                  break;
               default:
                  goto bad;
            }

            p = doAMode_RR(p, opc, r_src, am_addr, mode64);
            goto done;
         }
         break;
      }
      case Min_LoadL: {
         MIPSAMode *am_addr = i->Min.LoadL.src;
         UInt r_src = iregNo(am_addr->Mam.IR.base, mode64);
         UInt idx = am_addr->Mam.IR.index;
         UInt r_dst = iregNo(i->Min.LoadL.dst, mode64);

         p = mkFormI(p, 0x30, r_src, r_dst, idx);
         goto done;
      }
      case Min_StoreC: {
         MIPSAMode *am_addr = i->Min.StoreC.dst;
         UInt r_src = iregNo(i->Min.StoreC.src, mode64);
         UInt idx = am_addr->Mam.IR.index;
         UInt r_dst = iregNo(am_addr->Mam.IR.base, mode64);

         p = mkFormI(p, 0x38, r_dst, r_src, idx);
         goto done;
      }
      case Min_RdWrLR: {
         UInt reg = iregNo(i->Min.RdWrLR.gpr, mode64);
         Bool wrLR = i->Min.RdWrLR.wrLR;
         if (wrLR)
            p = mkMoveReg(p, 31, reg);
         else
            p = mkMoveReg(p, reg, 31);
         goto done;
      }
   
         // Floating point
   
      case Min_FpLdSt: {
         MIPSAMode *am_addr = i->Min.FpLdSt.addr;
         UChar sz = i->Min.FpLdSt.sz;
         vassert(sz == 4 || sz == 8);
         if (sz == 4) {
            UInt f_reg = fregNo(i->Min.FpLdSt.reg, mode64);
            if (i->Min.FpLdSt.isLoad) {
               if (am_addr->tag == Mam_IR)
                  p = doAMode_IR(p, 0x31, f_reg, am_addr, mode64);
               else if (am_addr->tag == Mam_RR)
                  p = doAMode_RR(p, 0x31, f_reg, am_addr, mode64);
            } else {
               if (am_addr->tag == Mam_IR)
                  p = doAMode_IR(p, 0x39, f_reg, am_addr, mode64);
               else if (am_addr->tag == Mam_RR)
                  p = doAMode_RR(p, 0x39, f_reg, am_addr, mode64);
            }
         } else if (sz == 8) {
            UInt f_reg = dregNo(i->Min.FpLdSt.reg);
            if (i->Min.FpLdSt.isLoad) {
               if (am_addr->tag == Mam_IR) {
                  if (mode64) {
                     p = doAMode_IR(p, 0x35, f_reg, am_addr, mode64);
                  } else {
                     p = doAMode_IR(p, 0x31, f_reg, am_addr, mode64);
                     p = doAMode_IR(p, 0x31, f_reg + 1,
                                   nextMIPSAModeFloat(am_addr), mode64);
                  }
               } else if (am_addr->tag == Mam_RR) {
                  if (mode64) {
                     p = doAMode_RR(p, 0x35, f_reg, am_addr, mode64);
                  } else {
                     p = doAMode_RR(p, 0x31, f_reg, am_addr, mode64);
                     p = doAMode_RR(p, 0x31, f_reg + 1,
                                    nextMIPSAModeFloat(am_addr), mode64);
                  }
               }
            } else {
               if (am_addr->tag == Mam_IR) {
                  if (mode64) {
                     p = doAMode_IR(p, 0x3d, f_reg, am_addr, mode64);
                  } else {
                     p = doAMode_IR(p, 0x39, f_reg, am_addr, mode64);
                     p = doAMode_IR(p, 0x39, f_reg + 1,
                                    nextMIPSAModeFloat(am_addr), mode64);
                  }
               } else if (am_addr->tag == Mam_RR) {
                  if (mode64) {
                     p = doAMode_RR(p, 0x3d, f_reg, am_addr, mode64);
                  } else {
                     p = doAMode_RR(p, 0x39, f_reg, am_addr, mode64);
                     p = doAMode_RR(p, 0x39, f_reg + 1,
                                    nextMIPSAModeFloat(am_addr), mode64);
                  }
               }
            }
         }
         goto done;
      }

      case Min_FpUnary: {
         switch (i->Min.FpUnary.op) {
            case Mfp_MOVS: { // FP move
               UInt fr_dst = fregNo(i->Min.FpUnary.dst, mode64);
               UInt fr_src = fregNo(i->Min.FpUnary.src, mode64);
               p = mkFormR(p, 0x11, 0x10, 0, fr_src, fr_dst, 0x6);
               break;
            }
            case Mfp_MOVD: { // FP move
                UInt fr_dst = dregNo(i->Min.FpUnary.dst);
                UInt fr_src = dregNo(i->Min.FpUnary.src);
                p = mkFormR(p, 0x11, 0x11, 0, fr_src, fr_dst, 0x6);
                break;
             }
            case Mfp_ABSS: { // ABSS
               UInt fr_dst = fregNo(i->Min.FpUnary.dst, mode64);
               UInt fr_src = fregNo(i->Min.FpUnary.src, mode64);
               p = mkFormR(p, 0x11, 0x10, 0, fr_src, fr_dst, 0x5);
               break;
            }
            case Mfp_ABSD: { // ABSD
               UInt fr_dst = dregNo(i->Min.FpUnary.dst);
               UInt fr_src = dregNo(i->Min.FpUnary.src);
               p = mkFormR(p, 0x11, 0x11, 0, fr_src, fr_dst, 0x5);
               break;
            }
            case Mfp_NEGS: { // ABSS
               UInt fr_dst = fregNo(i->Min.FpUnary.dst, mode64);
               UInt fr_src = fregNo(i->Min.FpUnary.src, mode64);
               p = mkFormR(p, 0x11, 0x10, 0, fr_src, fr_dst, 0x7);
               break;
            }
            case Mfp_NEGD: { // ABSD
               UInt fr_dst = dregNo(i->Min.FpUnary.dst);
               UInt fr_src = dregNo(i->Min.FpUnary.src);
               p = mkFormR(p, 0x11, 0x11, 0, fr_src, fr_dst, 0x7);
               break;
            }
            case Mfp_CVTD: { //CVT.D
               UInt fr_dst = dregNo(i->Min.FpUnary.dst);
               UInt fr_src = fregNo(i->Min.FpUnary.src, mode64);
               p = mkFormR(p, 0x11, 0x10, 0, fr_src, fr_dst, 0x21);
               break;
            }
            case Mfp_SQRTS: { //SQRT.S
               UInt fr_dst = fregNo(i->Min.FpUnary.dst, mode64);
               UInt fr_src = fregNo(i->Min.FpUnary.src, mode64);
               p = mkFormR(p, 0x11, 0x10, 0, fr_src, fr_dst, 0x04);
               break;
            }
            case Mfp_SQRTD: { //SQRT.D
               UInt fr_dst = dregNo(i->Min.FpUnary.dst);
               UInt fr_src = dregNo(i->Min.FpUnary.src);
               p = mkFormR(p, 0x11, 0x11, 0, fr_src, fr_dst, 0x04);
               break;
            }
            case Mfp_RSQRTS: { //RSQRT.S
                UInt fr_dst = fregNo(i->Min.FpUnary.dst, mode64);
                UInt fr_src = fregNo(i->Min.FpUnary.src, mode64);
                p = mkFormR(p, 0x11, 0x10, 0, fr_src, fr_dst, 0x16);
                break;
             }
            case Mfp_RSQRTD: { //RSQRT.D
               UInt fr_dst = dregNo(i->Min.FpUnary.dst);
               UInt fr_src = dregNo(i->Min.FpUnary.src);
               p = mkFormR(p, 0x11, 0x11, 0, fr_src, fr_dst, 0x16);
               break;
            }
            case Mfp_RECIPS: { //RECIP.S
               UInt fr_dst = fregNo(i->Min.FpUnary.dst, mode64);
               UInt fr_src = fregNo(i->Min.FpUnary.src, mode64);
               p = mkFormR(p, 0x11, 0x10, 0, fr_src, fr_dst, 0x15);
               break;
            }
            case Mfp_RECIPD: { //RECIP.D
               UInt fr_dst = dregNo(i->Min.FpUnary.dst);
               UInt fr_src = dregNo(i->Min.FpUnary.src);
               p = mkFormR(p, 0x11, 0x11, 0, fr_src, fr_dst, 0x15);
               break;
            }
            default:
               goto bad;
         }
         goto done;
      }

      case Min_FpBinary: {
         switch (i->Min.FpBinary.op) {
            case Mfp_ADDS: {
               UInt fr_dst = fregNo(i->Min.FpBinary.dst, mode64);
               UInt fr_srcL = fregNo(i->Min.FpBinary.srcL, mode64);
               UInt fr_srcR = fregNo(i->Min.FpBinary.srcR, mode64);
               p = mkFormR(p, 0x11, 0x10, fr_srcR, fr_srcL, fr_dst, 0);
               break;
            }
            case Mfp_SUBS: {
               UInt fr_dst = fregNo(i->Min.FpBinary.dst, mode64);
               UInt fr_srcL = fregNo(i->Min.FpBinary.srcL, mode64);
               UInt fr_srcR = fregNo(i->Min.FpBinary.srcR, mode64);
               p = mkFormR(p, 0x11, 0x10, fr_srcR, fr_srcL, fr_dst, 1);
               break;
            }
            case Mfp_MULS: {
               UInt fr_dst = fregNo(i->Min.FpBinary.dst, mode64);
               UInt fr_srcL = fregNo(i->Min.FpBinary.srcL, mode64);
               UInt fr_srcR = fregNo(i->Min.FpBinary.srcR, mode64);
               p = mkFormR(p, 0x11, 0x10, fr_srcR, fr_srcL, fr_dst, 2);
               break;
            }
            case Mfp_DIVS: {
               UInt fr_dst = fregNo(i->Min.FpBinary.dst, mode64);
               UInt fr_srcL = fregNo(i->Min.FpBinary.srcL, mode64);
               UInt fr_srcR = fregNo(i->Min.FpBinary.srcR, mode64);
               p = mkFormR(p, 0x11, 0x10, fr_srcR, fr_srcL, fr_dst, 3);
               break;
            }
            case Mfp_ADDD: {
               UInt fr_dst = dregNo(i->Min.FpBinary.dst);
               UInt fr_srcL = dregNo(i->Min.FpBinary.srcL);
               UInt fr_srcR = dregNo(i->Min.FpBinary.srcR);
               p = mkFormR(p, 0x11, 0x11, fr_srcR, fr_srcL, fr_dst, 0);
               break;
            }
            case Mfp_SUBD: {
               UInt fr_dst = dregNo(i->Min.FpBinary.dst);
               UInt fr_srcL = dregNo(i->Min.FpBinary.srcL);
               UInt fr_srcR = dregNo(i->Min.FpBinary.srcR);
               p = mkFormR(p, 0x11, 0x11, fr_srcR, fr_srcL, fr_dst, 1);
               break;
            }
            case Mfp_MULD: {
               UInt fr_dst = dregNo(i->Min.FpBinary.dst);
               UInt fr_srcL = dregNo(i->Min.FpBinary.srcL);
               UInt fr_srcR = dregNo(i->Min.FpBinary.srcR);
               p = mkFormR(p, 0x11, 0x11, fr_srcR, fr_srcL, fr_dst, 2);
               break;
            }
            case Mfp_DIVD: {
               UInt fr_dst = dregNo(i->Min.FpBinary.dst);
               UInt fr_srcL = dregNo(i->Min.FpBinary.srcL);
               UInt fr_srcR = dregNo(i->Min.FpBinary.srcR);
               p = mkFormR(p, 0x11, 0x11, fr_srcR, fr_srcL, fr_dst, 3);
               break;
            }
            default:
               goto bad;
         }
         goto done;
      }

      case Min_FpConvert: {
         switch (i->Min.FpConvert.op) {
            UInt fr_dst, fr_src;
            case Mfp_CVTSD:
               fr_dst = fregNo(i->Min.FpConvert.dst, mode64);
               fr_src = dregNo(i->Min.FpConvert.src);
               p = mkFormR(p, 0x11, 0x11, 0, fr_src, fr_dst, 0x20);
               break;
            case Mfp_CVTSW:
               fr_dst = fregNo(i->Min.FpConvert.dst, mode64);
               fr_src = fregNo(i->Min.FpConvert.src, mode64);
               p = mkFormR(p, 0x11, 0x14, 0, fr_src, fr_dst, 0x20);
               break;
            case Mfp_CVTWD:
               fr_dst = fregNo(i->Min.FpConvert.dst, mode64);
               fr_src = dregNo(i->Min.FpConvert.src);
               p = mkFormR(p, 0x11, 0x11, 0, fr_src, fr_dst, 0x24);
               break;
            case Mfp_CVTWS:
               fr_dst = fregNo(i->Min.FpConvert.dst, mode64);
               fr_src = fregNo(i->Min.FpConvert.src, mode64);
               p = mkFormR(p, 0x11, 0x10, 0, fr_src, fr_dst, 0x24);
               break;
            case Mfp_CVTDW:
               fr_dst = dregNo(i->Min.FpConvert.dst);
               fr_src = fregNo(i->Min.FpConvert.src, mode64);
               p = mkFormR(p, 0x11, 0x14, 0, fr_src, fr_dst, 0x21);
               break;
            case Mfp_TRUWS:
               fr_dst = fregNo(i->Min.FpConvert.dst, mode64);
               fr_src = fregNo(i->Min.FpConvert.src, mode64);
               p = mkFormR(p, 0x11, 0x10, 0, fr_src, fr_dst, 0x0D);
               break;
            case Mfp_TRUWD:
               fr_dst = fregNo(i->Min.FpConvert.dst, mode64);
               fr_src = dregNo(i->Min.FpConvert.src);
               p = mkFormR(p, 0x11, 0x11, 0, fr_src, fr_dst, 0x0D);
               break;
            case Mfp_TRULS:
               fr_dst = fregNo(i->Min.FpConvert.dst, mode64);
               fr_src = dregNo(i->Min.FpConvert.src);
               p = mkFormR(p, 0x11, 0x10, 0, fr_src, fr_dst, 0x09);
               break;
            case Mfp_TRULD:
               fr_dst = dregNo(i->Min.FpConvert.dst);
               fr_src = dregNo(i->Min.FpConvert.src);
               p = mkFormR(p, 0x11, 0x11, 0, fr_src, fr_dst, 0x09);
               break;
            case Mfp_CEILWS:
               fr_dst = fregNo(i->Min.FpConvert.dst, mode64);
               fr_src = fregNo(i->Min.FpConvert.src, mode64);
               p = mkFormR(p, 0x11, 0x10, 0, fr_src, fr_dst, 0x0E);
               break;
            case Mfp_CEILWD:
               fr_dst = fregNo(i->Min.FpConvert.dst, mode64);
               fr_src = dregNo(i->Min.FpConvert.src);
               p = mkFormR(p, 0x11, 0x11, 0, fr_src, fr_dst, 0x0E);
               break;
            case Mfp_CEILLS:
               fr_dst = dregNo(i->Min.FpConvert.dst);
               fr_src = fregNo(i->Min.FpConvert.src, mode64);
               p = mkFormR(p, 0x11, 0x10, 0, fr_src, fr_dst, 0x0A);
               break;
            case Mfp_CEILLD:
               fr_dst = dregNo(i->Min.FpConvert.dst);
               fr_src = dregNo(i->Min.FpConvert.src);
               p = mkFormR(p, 0x11, 0x11, 0, fr_src, fr_dst, 0x0A);
               break;
            case Mfp_ROUNDWS:
               fr_dst = fregNo(i->Min.FpConvert.dst, mode64);
               fr_src = fregNo(i->Min.FpConvert.src, mode64);
               p = mkFormR(p, 0x11, 0x10, 0, fr_src, fr_dst, 0x0C);
               break;
            case Mfp_ROUNDWD:
               fr_dst = fregNo(i->Min.FpConvert.dst, mode64);
               fr_src = dregNo(i->Min.FpConvert.src);
               p = mkFormR(p, 0x11, 0x11, 0, fr_src, fr_dst, 0x0C);
               break;
            case Mfp_FLOORWS:
               fr_dst = fregNo(i->Min.FpConvert.dst, mode64);
               fr_src = fregNo(i->Min.FpConvert.src, mode64);
               p = mkFormR(p, 0x11, 0x10, 0, fr_src, fr_dst, 0x0F);
               break;
            case Mfp_FLOORWD:
               fr_dst = fregNo(i->Min.FpConvert.dst, mode64);
               fr_src = dregNo(i->Min.FpConvert.src);
               p = mkFormR(p, 0x11, 0x11, 0, fr_src, fr_dst, 0x0F);
               break;

            default:
               goto bad;
         }
         goto done;
      }

      case Min_FpCompare: {
         UInt r_dst = iregNo(i->Min.FpCompare.dst, mode64);
         UInt fr_srcL = dregNo(i->Min.FpCompare.srcL);
         UInt fr_srcR = dregNo(i->Min.FpCompare.srcR);

         switch (i->Min.FpConvert.op) {
            case Mfp_CMP:
               p = mkFormR(p, 0x11, 0x11, fr_srcL, fr_srcR, 0,
                          (i->Min.FpCompare.cond1 + 48));
               p = mkFormR(p, 0x11, 0x2, r_dst, 31, 0, 0);
               break;
            default:
               goto bad;
         }
         goto done;
      }
      case Min_EvCheck: {
         /* This requires a 32-bit dec/test in 32 mode. */
         /* We generate:
               lw      r9, amCounter
               addiu   r9, r9, -1
               sw      r9, amCounter
               bgez    r9, nofail
               lw      r9, amFailAddr
               jalr    r9
               nop
              nofail:
         */
         UChar* p0 = p;
         /* lw  r9, amCounter */
         p = do_load_or_store_machine_word(p, True/*isLoad*/, /*r*/9,
                                     i->Min.EvCheck.amCounter, mode64);
         /* addiu r9,r9,-1 */
         p = mkFormI(p, 9, 9, 9, 0xFFFF);
         /* sw r30, amCounter */
         p = do_load_or_store_machine_word(p, False/*!isLoad*/, /*r*/9,
                                     i->Min.EvCheck.amCounter, mode64);
         /* bgez t9, nofail */
         p = mkFormI(p, 1, 9, 1, 3);
         /* lw r9, amFailAddr */
         p = do_load_or_store_machine_word(p, True/*isLoad*/, /*r*/9,
                                           i->Min.EvCheck.amFailAddr, mode64);
         /* jalr $9 */
         p = mkFormR(p, 0, 9, 0, 31, 0, 9); // p += 4
         p = mkFormR(p, 0, 0, 0, 0, 0, 0);   // p += 4
         /* nofail: */
   
         /* Crosscheck */
         vassert(evCheckSzB_MIPS() == (UChar*)p - (UChar*)p0);
         goto done;
      }

      case Min_ProfInc: {
         /* Generate a code template to increment a memory location whose
            address will be known later as an immediate value. This code
            template will be patched once the memory location is known.
            For now we do this with address == 0x65556555. 
               32-bit:

                 move r9, 0x65556555
                 lw r8, 0(r9)
                 addiu r8, r8, 1         # add least significant word
                 sw r8, 0(r9)
                 sltiu r1, r8, 1         # set carry-in bit
                 lw r8, 4(r9)
                 addu r8, r8, r1
                 sw r8, 4(r9) */

         if (mode64) {
            vassert(0);
         } else {
            // move r9, 0x65556555
            p = mkLoadImm_EXACTLY2or5(p, /*r*/9, 0x65556555ULL,
                                      False/*!mode64*/);
            // lw r8, 0(r9)
            p = mkFormI(p, 35, 9, 8, 0);

            // addiu r8, r8, 1         # add least significant word
            p = mkFormI(p, 9, 8, 8, 1);

            // sw r8, 0(r9)
            p = mkFormI(p, 43, 9, 8, 0);

            // sltiu r1, r8, 1         # set carry-in bit
            p = mkFormI(p, 11, 8, 1, 1);

            // lw r8, 4(r9)
            p = mkFormI(p, 35, 9, 8, 4);

            // addu r8, r8, r1
            p = mkFormR(p, 0, 8, 1, 8, 0, 33);

            // sw r8, 4(r9)
            p = mkFormI(p, 43, 9, 8, 4);

         }
         /* Tell the caller .. */
         vassert(!(*is_profInc));
         *is_profInc = True;
         goto done;
      }
   
      default:
         goto bad;

   }

   bad:
      vex_printf("\n=> ");
      ppMIPSInstr(i, mode64);
      vpanic("emit_MIPSInstr");
      /*NOTREACHED*/ done:
      //vassert(p - &buf[0] <= 32);
      return p - &buf[0];
}

/* How big is an event check?  See case for Min_EvCheck in
   emit_MIPSInstr just above.  That crosschecks what this returns, so
   we can tell if we're inconsistent. */
Int evCheckSzB_MIPS ( void )
{
  UInt kInstrSize = 4;
  return 7*kInstrSize;
}

/* NB: what goes on here has to be very closely coordinated with the
   emitInstr case for XDirect, above. */
VexInvalRange chainXDirect_MIPS ( void* place_to_chain,
                                  void* disp_cp_chain_me_EXPECTED,
                                  void* place_to_jump_to,
                                  Bool  mode64 )
{
   /* What we're expecting to see is:
        move r9, disp_cp_chain_me_to_EXPECTED
        jalr r9
        nop
      viz
        <8 or 20 bytes generated by mkLoadImm_EXACTLY2or5>
        0x120F809  // jalr r9
        0x00000000 // nop
   */
   UChar* p = (UChar*)place_to_chain;
   vassert(0 == (3 & (HWord)p));
   vassert(isLoadImm_EXACTLY2or5(p, /*r*/9,
                                 (UInt)Ptr_to_ULong(disp_cp_chain_me_EXPECTED),
                                 mode64));
   vassert(fetch32(p + (mode64 ? 20 : 8) + 0) == 0x120F809);
   vassert(fetch32(p + (mode64 ? 20 : 8) + 4) == 0x00000000);
   /* And what we want to change it to is either:
          move r9, place_to_jump_to
          jalr r9
          nop
        viz
          <8 bytes generated by mkLoadImm_EXACTLY2or5>
          0x120F809  // jalr r9
          0x00000000 // nop

      The replacement has the same length as the original.
   */

   p = mkLoadImm_EXACTLY2or5(p, /*r*/9,
                             Ptr_to_ULong(place_to_jump_to), mode64);
   p = emit32(p, 0x120F809);
   p = emit32(p, 0x00000000);

   Int len = p - (UChar*)place_to_chain;
   vassert(len == (mode64 ? 28 : 16)); /* stay sane */
   VexInvalRange vir = {(HWord)place_to_chain, len};
   return vir;
}

/* NB: what goes on here has to be very closely coordinated with the
   emitInstr case for XDirect, above. */
VexInvalRange unchainXDirect_MIPS ( void* place_to_unchain,
                                    void* place_to_jump_to_EXPECTED,
                                    void* disp_cp_chain_me,
                                    Bool  mode64 )
{
   /* What we're expecting to see is:
        move r9, place_to_jump_to_EXPECTED
        jalr r9
        nop
      viz
        <8 or 20 bytes generated by mkLoadImm_EXACTLY2or5>
        0x120F809  // jalr r9
        0x00000000 // nop
   */
   UChar* p = (UChar*)place_to_unchain;
   vassert(0 == (3 & (HWord)p));
   vassert(isLoadImm_EXACTLY2or5(p, /*r*/9,
                                 Ptr_to_ULong(place_to_jump_to_EXPECTED),
                                 mode64));
   vassert(fetch32(p + (mode64 ? 20 : 8) + 0) == 0x120F809);
   vassert(fetch32(p + (mode64 ? 20 : 8) + 4) == 0x00000000);
   /* And what we want to change it to is:
        move r9, disp_cp_chain_me
        jalr r9
        nop
      viz
        <8 or 20 bytes generated by mkLoadImm_EXACTLY2or5>
        0x120F809  // jalr r9
        0x00000000 // nop
      The replacement has the same length as the original.
   */
   p = mkLoadImm_EXACTLY2or5(p, /*r*/9,
                             Ptr_to_ULong(disp_cp_chain_me), mode64);
   p = emit32(p, 0x120F809);
   p = emit32(p, 0x00000000);

   Int len = p - (UChar*)place_to_unchain;
   vassert(len == (mode64 ? 28 : 16)); /* stay sane */
   VexInvalRange vir = {(HWord)place_to_unchain, len};
   return vir;
}

/* Patch the counter address into a profile inc point, as previously
   created by the Min_ProfInc case for emit_MIPSInstr. */
VexInvalRange patchProfInc_MIPS ( void*  place_to_patch,
                                  ULong* location_of_counter, Bool mode64 )
{
   vassert(sizeof(ULong*) == 4);
   UChar* p = (UChar*)place_to_patch;
   vassert(0 == (3 & (HWord)p));
   vassert(isLoadImm_EXACTLY2or5((UChar *)p, /*r*/9, 0x65556555, mode64));

   vassert(fetch32(p + (mode64 ? 20 : 8) + 0) == 0x8D280000);
   vassert(fetch32(p + (mode64 ? 20 : 8) + 4) == 0x25080001);
   vassert(fetch32(p + (mode64 ? 20 : 8) + 8) == 0xAD280000);
   vassert(fetch32(p + (mode64 ? 20 : 8) + 12) == 0x2d010001);
   vassert(fetch32(p + (mode64 ? 20 : 8) + 16) == 0x8d280004);
   vassert(fetch32(p + (mode64 ? 20 : 8) + 20) == 0x01014021);
   vassert(fetch32(p + (mode64 ? 20 : 8) + 24) == 0xad280004);

   p = mkLoadImm_EXACTLY2or5(p, /*r*/9,
                             Ptr_to_ULong(location_of_counter), mode64);

   VexInvalRange vir = {(HWord)p, 8};
   return vir;
}


/*---------------------------------------------------------------*/
/*--- end                                    host_mips_defs.c ---*/
/*---------------------------------------------------------------*/
