
/*---------------------------------------------------------------*/
/*--- begin                                 host_arm64_defs.c ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2013-2017 OpenWorks
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
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.
*/

#include "libvex_basictypes.h"
#include "libvex.h"
#include "libvex_trc_values.h"

#include "main_util.h"
#include "host_generic_regs.h"
#include "host_arm64_defs.h"


/* --------- Registers. --------- */

/* The usual HReg abstraction.  We use the following classes only:
     X regs (64 bit int)
     D regs (64 bit float, also used for 32 bit float)
     Q regs (128 bit vector)
*/

const RRegUniverse* getRRegUniverse_ARM64 ( void )
{
   /* The real-register universe is a big constant, so we just want to
      initialise it once. */
   static RRegUniverse rRegUniverse_ARM64;
   static Bool         rRegUniverse_ARM64_initted = False;

   /* Handy shorthand, nothing more */
   RRegUniverse* ru = &rRegUniverse_ARM64;

   /* This isn't thread-safe.  Sigh. */
   if (LIKELY(rRegUniverse_ARM64_initted))
      return ru;

   RRegUniverse__init(ru);

   /* Add the registers.  The initial segment of this array must be
      those available for allocation by reg-alloc, and those that
      follow are not available for allocation. */
   ru->allocable_start[HRcInt64] = ru->size;
   ru->regs[ru->size++] = hregARM64_X22();
   ru->regs[ru->size++] = hregARM64_X23();
   ru->regs[ru->size++] = hregARM64_X24();
   ru->regs[ru->size++] = hregARM64_X25();
   ru->regs[ru->size++] = hregARM64_X26();
   ru->regs[ru->size++] = hregARM64_X27();
   ru->regs[ru->size++] = hregARM64_X28();

   ru->regs[ru->size++] = hregARM64_X0();
   ru->regs[ru->size++] = hregARM64_X1();
   ru->regs[ru->size++] = hregARM64_X2();
   ru->regs[ru->size++] = hregARM64_X3();
   ru->regs[ru->size++] = hregARM64_X4();
   ru->regs[ru->size++] = hregARM64_X5();
   ru->regs[ru->size++] = hregARM64_X6();
   ru->regs[ru->size++] = hregARM64_X7();
   ru->allocable_end[HRcInt64] = ru->size - 1;
   // X8 is used as a ProfInc temporary, not available to regalloc.
   // X9 is a chaining/spill temporary, not available to regalloc.

   // Do we really need all these?
   //ru->regs[ru->size++] = hregARM64_X10();
   //ru->regs[ru->size++] = hregARM64_X11();
   //ru->regs[ru->size++] = hregARM64_X12();
   //ru->regs[ru->size++] = hregARM64_X13();
   //ru->regs[ru->size++] = hregARM64_X14();
   //ru->regs[ru->size++] = hregARM64_X15();
   // X21 is the guest state pointer, not available to regalloc.

   // vector regs.  Unfortunately not callee-saved.
   ru->allocable_start[HRcVec128] = ru->size;
   ru->regs[ru->size++] = hregARM64_Q16();
   ru->regs[ru->size++] = hregARM64_Q17();
   ru->regs[ru->size++] = hregARM64_Q18();
   ru->regs[ru->size++] = hregARM64_Q19();
   ru->regs[ru->size++] = hregARM64_Q20();
   ru->allocable_end[HRcVec128] = ru->size - 1;

   // F64 regs, all of which are callee-saved
   ru->allocable_start[HRcFlt64] = ru->size;
   ru->regs[ru->size++] = hregARM64_D8();
   ru->regs[ru->size++] = hregARM64_D9();
   ru->regs[ru->size++] = hregARM64_D10();
   ru->regs[ru->size++] = hregARM64_D11();
   ru->regs[ru->size++] = hregARM64_D12();
   ru->regs[ru->size++] = hregARM64_D13();
   ru->allocable_end[HRcFlt64] = ru->size - 1;

   ru->allocable = ru->size;
   /* And other regs, not available to the allocator. */

   // unavail: x21 as GSP
   // x8 is used as a ProfInc temporary
   // x9 is used as a spill/reload/chaining/call temporary
   // x30 as LR
   //
   // x31 is mentionable, but not allocatable, and is dangerous to use
   // because of SP-vs-ZR overloading.  Here, we call it `XZR_XSP`.  Whether
   // it denotes the zero register or the stack pointer depends both on what
   // kind of instruction it appears in and even on the position within an
   // instruction that it appears.  So be careful.  There's absolutely
   // nothing to prevent shooting oneself in the foot.
   //
   // Currently, we have 15 allocatable integer registers:
   // 0 1 2 3 4 5 6 7 22 23 24 25 26 27 28
   //
   // Hence for the allocatable integer registers we have:
   //
   // callee-saved: 22 23 24 25 26 27 28
   // caller-saved: 0 1 2 3 4 5 6 7
   //
   // If the set of available registers changes or if the e/r status
   // changes, be sure to re-check/sync the definition of
   // getRegUsage for ARM64Instr_Call too.

   ru->regs[ru->size++] = hregARM64_X8();
   ru->regs[ru->size++] = hregARM64_X9();
   ru->regs[ru->size++] = hregARM64_X21();
   ru->regs[ru->size++] = hregARM64_XZR_XSP();

   rRegUniverse_ARM64_initted = True;

   RRegUniverse__check_is_sane(ru);
   return ru;
}


UInt ppHRegARM64 ( HReg reg )  {
   Int r;
   /* Be generic for all virtual regs. */
   if (hregIsVirtual(reg)) {
      return ppHReg(reg);
   }
   /* But specific for real regs. */
   switch (hregClass(reg)) {
      case HRcInt64:
         r = hregEncoding(reg);
         vassert(r >= 0 && r <= 31);
         return r ==31 ? vex_printf("xzr/xsp") : vex_printf("x%d", r);
      case HRcFlt64:
         r = hregEncoding(reg);
         vassert(r >= 0 && r < 32);
         return vex_printf("d%d", r);
      case HRcVec128:
         r = hregEncoding(reg);
         vassert(r >= 0 && r < 32);
         return vex_printf("q%d", r);
      default:
         vpanic("ppHRegARM64");
   }
}

static UInt ppHRegARM64asSreg ( HReg reg ) {
   UInt written = ppHRegARM64(reg);
   written += vex_printf("(S-reg)");
   return written;
}

static UInt ppHRegARM64asHreg ( HReg reg ) {
   UInt written = ppHRegARM64(reg);
   written += vex_printf("(H-reg)");
   return written;
}


/* --------- Condition codes, ARM64 encoding. --------- */

static const HChar* showARM64CondCode ( ARM64CondCode cond ) {
   switch (cond) {
       case ARM64cc_EQ:  return "eq";
       case ARM64cc_NE:  return "ne";
       case ARM64cc_CS:  return "cs";
       case ARM64cc_CC:  return "cc";
       case ARM64cc_MI:  return "mi";
       case ARM64cc_PL:  return "pl";
       case ARM64cc_VS:  return "vs";
       case ARM64cc_VC:  return "vc";
       case ARM64cc_HI:  return "hi";
       case ARM64cc_LS:  return "ls";
       case ARM64cc_GE:  return "ge";
       case ARM64cc_LT:  return "lt";
       case ARM64cc_GT:  return "gt";
       case ARM64cc_LE:  return "le";
       case ARM64cc_AL:  return "al"; // default
       case ARM64cc_NV:  return "nv";
       default: vpanic("showARM64CondCode");
   }
}


/* --------- Memory address expressions (amodes). --------- */

ARM64AMode* ARM64AMode_RI9  ( HReg reg, Int simm9 ) {
   ARM64AMode* am        = LibVEX_Alloc_inline(sizeof(ARM64AMode));
   am->tag               = ARM64am_RI9;
   am->ARM64am.RI9.reg   = reg;
   am->ARM64am.RI9.simm9 = simm9;
   vassert(-256 <= simm9 && simm9 <= 255);
   return am;
}

ARM64AMode* ARM64AMode_RI12 ( HReg reg, Int uimm12, UChar szB ) {
   ARM64AMode* am          = LibVEX_Alloc_inline(sizeof(ARM64AMode));
   am->tag                 = ARM64am_RI12;
   am->ARM64am.RI12.reg    = reg;
   am->ARM64am.RI12.uimm12 = uimm12;
   am->ARM64am.RI12.szB    = szB;
   vassert(uimm12 >= 0 && uimm12 <= 4095);
   switch (szB) {
      case 1: case 2: case 4: case 8: break;
      default: vassert(0);
   }
   return am;
}

ARM64AMode* ARM64AMode_RR ( HReg base, HReg index ) {
   ARM64AMode* am       = LibVEX_Alloc_inline(sizeof(ARM64AMode));
   am->tag              = ARM64am_RR;
   am->ARM64am.RR.base  = base;
   am->ARM64am.RR.index = index;
   return am;
}

static void ppARM64AMode ( ARM64AMode* am ) {
   switch (am->tag) {
      case ARM64am_RI9:
         vex_printf("%d(", am->ARM64am.RI9.simm9);
         ppHRegARM64(am->ARM64am.RI9.reg);
         vex_printf(")");
         break;
      case ARM64am_RI12:
         vex_printf("%u(", (UInt)am->ARM64am.RI12.szB
                           * (UInt)am->ARM64am.RI12.uimm12);
         ppHRegARM64(am->ARM64am.RI12.reg);
         vex_printf(")");
         break;
      case ARM64am_RR:
         vex_printf("(");
         ppHRegARM64(am->ARM64am.RR.base);
         vex_printf(",");
         ppHRegARM64(am->ARM64am.RR.index);
         vex_printf(")");
         break;
      default:
         vassert(0);
   }
}

static void addRegUsage_ARM64AMode ( HRegUsage* u, ARM64AMode* am ) {
   switch (am->tag) {
      case ARM64am_RI9:
         addHRegUse(u, HRmRead, am->ARM64am.RI9.reg);
         return;
      case ARM64am_RI12:
         addHRegUse(u, HRmRead, am->ARM64am.RI12.reg);
         return;
      case ARM64am_RR:
         addHRegUse(u, HRmRead, am->ARM64am.RR.base);
         addHRegUse(u, HRmRead, am->ARM64am.RR.index);
         return;
      default:
         vpanic("addRegUsage_ARM64Amode");
   }
}

static void mapRegs_ARM64AMode ( HRegRemap* m, ARM64AMode* am ) {
   switch (am->tag) {
      case ARM64am_RI9:
         am->ARM64am.RI9.reg = lookupHRegRemap(m, am->ARM64am.RI9.reg);
         return;
      case ARM64am_RI12:
         am->ARM64am.RI12.reg = lookupHRegRemap(m, am->ARM64am.RI12.reg);
         return;
      case ARM64am_RR:
         am->ARM64am.RR.base  = lookupHRegRemap(m, am->ARM64am.RR.base);
         am->ARM64am.RR.index = lookupHRegRemap(m, am->ARM64am.RR.index);
         return;
      default:
         vpanic("mapRegs_ARM64Amode");
   }
}


/* --------- Reg or uimm12<<{0,12} operands --------- */

ARM64RIA* ARM64RIA_I12 ( UShort imm12, UChar shift ) {
   ARM64RIA* riA           = LibVEX_Alloc_inline(sizeof(ARM64RIA));
   riA->tag                = ARM64riA_I12;
   riA->ARM64riA.I12.imm12 = imm12;
   riA->ARM64riA.I12.shift = shift;
   vassert(imm12 < 4096);
   vassert(shift == 0 || shift == 12);
   return riA;
}
ARM64RIA* ARM64RIA_R ( HReg reg ) {
   ARM64RIA* riA       = LibVEX_Alloc_inline(sizeof(ARM64RIA));
   riA->tag            = ARM64riA_R;
   riA->ARM64riA.R.reg = reg;
   return riA;
}

static void ppARM64RIA ( ARM64RIA* riA ) {
   switch (riA->tag) {
      case ARM64riA_I12:
         vex_printf("#%u",(UInt)(riA->ARM64riA.I12.imm12
                                 << riA->ARM64riA.I12.shift));
         break;
      case ARM64riA_R:
         ppHRegARM64(riA->ARM64riA.R.reg);
         break;
      default:
         vassert(0);
   }
}

static void addRegUsage_ARM64RIA ( HRegUsage* u, ARM64RIA* riA ) {
   switch (riA->tag) {
      case ARM64riA_I12:
         return;
      case ARM64riA_R:
         addHRegUse(u, HRmRead, riA->ARM64riA.R.reg);
         return;
      default:
         vpanic("addRegUsage_ARM64RIA");
   }
}

static void mapRegs_ARM64RIA ( HRegRemap* m, ARM64RIA* riA ) {
   switch (riA->tag) {
      case ARM64riA_I12:
         return;
      case ARM64riA_R:
         riA->ARM64riA.R.reg = lookupHRegRemap(m, riA->ARM64riA.R.reg);
         return;
      default:
         vpanic("mapRegs_ARM64RIA");
   }
}


/* --------- Reg or "bitfield" (logic immediate) operands --------- */

ARM64RIL* ARM64RIL_I13 ( UChar bitN, UChar immR, UChar immS ) {
   ARM64RIL* riL          = LibVEX_Alloc_inline(sizeof(ARM64RIL));
   riL->tag               = ARM64riL_I13;
   riL->ARM64riL.I13.bitN = bitN;
   riL->ARM64riL.I13.immR = immR;
   riL->ARM64riL.I13.immS = immS;
   vassert(bitN < 2);
   vassert(immR < 64);
   vassert(immS < 64);
   return riL;
}
ARM64RIL* ARM64RIL_R ( HReg reg ) {
   ARM64RIL* riL       = LibVEX_Alloc_inline(sizeof(ARM64RIL));
   riL->tag            = ARM64riL_R;
   riL->ARM64riL.R.reg = reg;
   return riL;
}

static void ppARM64RIL ( ARM64RIL* riL ) {
   switch (riL->tag) {
      case ARM64riL_I13:
         vex_printf("#nrs(%u,%u,%u)",
                     (UInt)riL->ARM64riL.I13.bitN,
                     (UInt)riL->ARM64riL.I13.immR,
                     (UInt)riL->ARM64riL.I13.immS);
         break;
      case ARM64riL_R:
         ppHRegARM64(riL->ARM64riL.R.reg);
         break;
      default:
         vassert(0);
   }
}

static void addRegUsage_ARM64RIL ( HRegUsage* u, ARM64RIL* riL ) {
   switch (riL->tag) {
      case ARM64riL_I13:
         return;
      case ARM64riL_R:
         addHRegUse(u, HRmRead, riL->ARM64riL.R.reg);
         return;
      default:
         vpanic("addRegUsage_ARM64RIL");
   }
}

static void mapRegs_ARM64RIL ( HRegRemap* m, ARM64RIL* riL ) {
   switch (riL->tag) {
      case ARM64riL_I13:
         return;
      case ARM64riL_R:
         riL->ARM64riL.R.reg = lookupHRegRemap(m, riL->ARM64riL.R.reg);
         return;
      default:
         vpanic("mapRegs_ARM64RIL");
   }
}


/* --------------- Reg or uimm6 operands --------------- */

ARM64RI6* ARM64RI6_I6 ( UInt imm6 ) {
   ARM64RI6* ri6         = LibVEX_Alloc_inline(sizeof(ARM64RI6));
   ri6->tag              = ARM64ri6_I6;
   ri6->ARM64ri6.I6.imm6 = imm6;
   vassert(imm6 > 0 && imm6 < 64);
   return ri6;
}
ARM64RI6* ARM64RI6_R ( HReg reg ) {
   ARM64RI6* ri6       = LibVEX_Alloc_inline(sizeof(ARM64RI6));
   ri6->tag            = ARM64ri6_R;
   ri6->ARM64ri6.R.reg = reg;
   return ri6;
}

static void ppARM64RI6 ( ARM64RI6* ri6 ) {
   switch (ri6->tag) {
      case ARM64ri6_I6:
         vex_printf("#%u", ri6->ARM64ri6.I6.imm6);
         break;
      case ARM64ri6_R:
         ppHRegARM64(ri6->ARM64ri6.R.reg);
         break;
      default:
         vassert(0);
   }
}

static void addRegUsage_ARM64RI6 ( HRegUsage* u, ARM64RI6* ri6 ) {
   switch (ri6->tag) {
      case ARM64ri6_I6:
         return;
      case ARM64ri6_R:
         addHRegUse(u, HRmRead, ri6->ARM64ri6.R.reg);
         return;
      default:
         vpanic("addRegUsage_ARM64RI6");
   }
}

static void mapRegs_ARM64RI6 ( HRegRemap* m, ARM64RI6* ri6 ) {
   switch (ri6->tag) {
      case ARM64ri6_I6:
         return;
      case ARM64ri6_R:
         ri6->ARM64ri6.R.reg = lookupHRegRemap(m, ri6->ARM64ri6.R.reg);
         return;
      default:
         vpanic("mapRegs_ARM64RI6");
   }
}


/* --------- Instructions. --------- */

static const HChar* showARM64LogicOp ( ARM64LogicOp op ) {
   switch (op) {
      case ARM64lo_AND: return "and";
      case ARM64lo_OR:  return "orr";
      case ARM64lo_XOR: return "eor";
      default: vpanic("showARM64LogicOp");
   }
}

static const HChar* showARM64ShiftOp ( ARM64ShiftOp op ) {
   switch (op) {
      case ARM64sh_SHL: return "lsl";
      case ARM64sh_SHR: return "lsr";
      case ARM64sh_SAR: return "asr";
      default: vpanic("showARM64ShiftOp");
   }
}

static const HChar* showARM64RRSOp ( ARM64RRSOp op ) {
   switch (op) {
      case ARM64rrs_ADD: return "add";
      case ARM64rrs_SUB: return "sub";
      case ARM64rrs_AND: return "and";
      case ARM64rrs_OR:  return "orr";
      case ARM64rrs_XOR: return "eor";
      default: vpanic("showARM64RRSOp");
   }
}

static const HChar* showARM64UnaryOp ( ARM64UnaryOp op ) {
   switch (op) {
      case ARM64un_NEG: return "neg";
      case ARM64un_NOT: return "not";
      case ARM64un_CLZ: return "clz";
      default: vpanic("showARM64UnaryOp");
   }
}

static const HChar* showARM64MulOp ( ARM64MulOp op ) {
   switch (op) {
      case ARM64mul_PLAIN: return "mul  ";
      case ARM64mul_ZX:    return "umulh";
      case ARM64mul_SX:    return "smulh";
      default: vpanic("showARM64MulOp");
   }
}

static void characteriseARM64CvtOp ( /*OUT*/HChar* syn,
                                     /*OUT*/UInt* fszB, /*OUT*/UInt* iszB, 
                                     ARM64CvtOp op ) {
   switch (op) {
      case ARM64cvt_F32_I32S:
         *syn = 's'; *fszB = 4; *iszB = 4; break;
      case ARM64cvt_F64_I32S:
         *syn = 's'; *fszB = 8; *iszB = 4; break;
      case ARM64cvt_F32_I64S:
         *syn = 's'; *fszB = 4; *iszB = 8; break;
      case ARM64cvt_F64_I64S:
         *syn = 's'; *fszB = 8; *iszB = 8; break;
      case ARM64cvt_F32_I32U:
         *syn = 'u'; *fszB = 4; *iszB = 4; break;
      case ARM64cvt_F64_I32U:
         *syn = 'u'; *fszB = 8; *iszB = 4; break;
      case ARM64cvt_F32_I64U:
         *syn = 'u'; *fszB = 4; *iszB = 8; break;
      case ARM64cvt_F64_I64U:
         *syn = 'u'; *fszB = 8; *iszB = 8; break;
      default:
         vpanic("characteriseARM64CvtOp");
  }
}

static const HChar* showARM64FpBinOp ( ARM64FpBinOp op ) {
   switch (op) {
      case ARM64fpb_ADD: return "add";
      case ARM64fpb_SUB: return "sub";
      case ARM64fpb_MUL: return "mul";
      case ARM64fpb_DIV: return "div";
      default: vpanic("showARM64FpBinOp");
   }
}

static const HChar* showARM64FpTriOp ( ARM64FpTriOp op ) {
   switch (op) {
      case ARM64fpt_FMADD: return "fmadd";
      case ARM64fpt_FMSUB: return "fmsub";
      default: vpanic("showARM64FpTriOp");
   }
}

static const HChar* showARM64FpUnaryOp ( ARM64FpUnaryOp op ) {
   switch (op) {
      case ARM64fpu_NEG:    return "neg  ";
      case ARM64fpu_ABS:    return "abs  ";
      case ARM64fpu_SQRT:   return "sqrt ";
      case ARM64fpu_RINT:   return "rinti";
      case ARM64fpu_RINTA0: return "rinta";
      case ARM64fpu_RINTE:  return "rintn";
      case ARM64fpu_RECPX:  return "recpx";
      default: vpanic("showARM64FpUnaryOp");
   }
}

static void showARM64VecBinOp(/*OUT*/const HChar** nm,
                              /*OUT*/const HChar** ar, ARM64VecBinOp op ) {
   switch (op) {
      case ARM64vecb_ADD64x2:      *nm = "add   ";    *ar = "2d";   return;
      case ARM64vecb_ADD32x4:      *nm = "add   ";    *ar = "4s";   return;
      case ARM64vecb_ADD16x8:      *nm = "add   ";    *ar = "8h";   return;
      case ARM64vecb_ADD8x16:      *nm = "add   ";    *ar = "16b";  return;
      case ARM64vecb_SUB64x2:      *nm = "sub   ";    *ar = "2d";   return;
      case ARM64vecb_SUB32x4:      *nm = "sub   ";    *ar = "4s";   return;
      case ARM64vecb_SUB16x8:      *nm = "sub   ";    *ar = "8h";   return;
      case ARM64vecb_SUB8x16:      *nm = "sub   ";    *ar = "16b";  return;
      case ARM64vecb_MUL32x4:      *nm = "mul   ";    *ar = "4s";   return;
      case ARM64vecb_MUL16x8:      *nm = "mul   ";    *ar = "8h";   return;
      case ARM64vecb_MUL8x16:      *nm = "mul   ";    *ar = "16b";  return;
      case ARM64vecb_FADD64x2:     *nm = "fadd  ";    *ar = "2d";   return;
      case ARM64vecb_FSUB64x2:     *nm = "fsub  ";    *ar = "2d";   return;
      case ARM64vecb_FMUL64x2:     *nm = "fmul  ";    *ar = "2d";   return;
      case ARM64vecb_FDIV64x2:     *nm = "fdiv  ";    *ar = "2d";   return;
      case ARM64vecb_FADD32x4:     *nm = "fadd  ";    *ar = "4s";   return;
      case ARM64vecb_FADD16x8:     *nm = "fadd  ";    *ar = "8h";   return;
      case ARM64vecb_FSUB32x4:     *nm = "fsub  ";    *ar = "4s";   return;
      case ARM64vecb_FSUB16x8:     *nm = "fsub  ";    *ar = "8h";   return;
      case ARM64vecb_FMUL32x4:     *nm = "fmul  ";    *ar = "4s";   return;
      case ARM64vecb_FDIV32x4:     *nm = "fdiv  ";    *ar = "4s";   return;
      case ARM64vecb_FMAX64x2:     *nm = "fmax  ";    *ar = "2d";   return;
      case ARM64vecb_FMAX32x4:     *nm = "fmax  ";    *ar = "4s";   return;
      case ARM64vecb_FMIN64x2:     *nm = "fmin  ";    *ar = "2d";   return;
      case ARM64vecb_FMIN32x4:     *nm = "fmin  ";    *ar = "4s";   return;
      case ARM64vecb_UMAX32x4:     *nm = "umax  ";    *ar = "4s";   return;
      case ARM64vecb_UMAX16x8:     *nm = "umax  ";    *ar = "8h";   return;
      case ARM64vecb_UMAX8x16:     *nm = "umax  ";    *ar = "16b";  return;
      case ARM64vecb_UMIN32x4:     *nm = "umin  ";    *ar = "4s";   return;
      case ARM64vecb_UMIN16x8:     *nm = "umin  ";    *ar = "8h";   return;
      case ARM64vecb_UMIN8x16:     *nm = "umin  ";    *ar = "16b";  return;
      case ARM64vecb_SMAX32x4:     *nm = "smax  ";    *ar = "4s";   return;
      case ARM64vecb_SMAX16x8:     *nm = "smax  ";    *ar = "8h";   return;
      case ARM64vecb_SMAX8x16:     *nm = "smax  ";    *ar = "16b";  return;
      case ARM64vecb_SMIN32x4:     *nm = "smin  ";    *ar = "4s";   return;
      case ARM64vecb_SMIN16x8:     *nm = "smin  ";    *ar = "8h";   return;
      case ARM64vecb_SMIN8x16:     *nm = "smin  ";    *ar = "16b";  return;
      case ARM64vecb_AND:          *nm = "and   ";    *ar = "16b";  return;
      case ARM64vecb_ORR:          *nm = "orr   ";    *ar = "16b";  return;
      case ARM64vecb_XOR:          *nm = "eor   ";    *ar = "16b";  return;
      case ARM64vecb_CMEQ64x2:     *nm = "cmeq  ";    *ar = "2d";   return;
      case ARM64vecb_CMEQ32x4:     *nm = "cmeq  ";    *ar = "4s";   return;
      case ARM64vecb_CMEQ16x8:     *nm = "cmeq  ";    *ar = "8h";   return;
      case ARM64vecb_CMEQ8x16:     *nm = "cmeq  ";    *ar = "16b";  return;
      case ARM64vecb_CMHI64x2:     *nm = "cmhi  ";    *ar = "2d";   return;
      case ARM64vecb_CMHI32x4:     *nm = "cmhi  ";    *ar = "4s";   return;
      case ARM64vecb_CMHI16x8:     *nm = "cmhi  ";    *ar = "8h";   return;
      case ARM64vecb_CMHI8x16:     *nm = "cmhi  ";    *ar = "16b";  return;
      case ARM64vecb_CMGT64x2:     *nm = "cmgt  ";    *ar = "2d";   return;
      case ARM64vecb_CMGT32x4:     *nm = "cmgt  ";    *ar = "4s";   return;
      case ARM64vecb_CMGT16x8:     *nm = "cmgt  ";    *ar = "8h";   return;
      case ARM64vecb_CMGT8x16:     *nm = "cmgt  ";    *ar = "16b";  return;
      case ARM64vecb_FCMEQ64x2:    *nm = "fcmeq ";    *ar = "2d";   return;
      case ARM64vecb_FCMEQ32x4:    *nm = "fcmeq ";    *ar = "4s";   return;
      case ARM64vecb_FCMGE64x2:    *nm = "fcmge ";    *ar = "2d";   return;
      case ARM64vecb_FCMGE32x4:    *nm = "fcmge ";    *ar = "4s";   return;
      case ARM64vecb_FCMGE16x8:    *nm = "fcmge ";    *ar = "8h";   return;
      case ARM64vecb_FCMGT64x2:    *nm = "fcmgt ";    *ar = "2d";   return;
      case ARM64vecb_FCMGT32x4:    *nm = "fcmgt ";    *ar = "4s";   return;
      case ARM64vecb_FCMGT16x8:    *nm = "fcmgt ";    *ar = "8h";   return;
      case ARM64vecb_FCMEQ16x8:    *nm = "fcmeq ";    *ar = "8h";   return;
      case ARM64vecb_TBL1:         *nm = "tbl   ";    *ar = "16b";  return;
      case ARM64vecb_UZP164x2:     *nm = "uzp1  ";    *ar = "2d";   return;
      case ARM64vecb_UZP132x4:     *nm = "uzp1  ";    *ar = "4s";   return;
      case ARM64vecb_UZP116x8:     *nm = "uzp1  ";    *ar = "8h";   return;
      case ARM64vecb_UZP18x16:     *nm = "uzp1  ";    *ar = "16b";  return;
      case ARM64vecb_UZP264x2:     *nm = "uzp2  ";    *ar = "2d";   return;
      case ARM64vecb_UZP232x4:     *nm = "uzp2  ";    *ar = "4s";   return;
      case ARM64vecb_UZP216x8:     *nm = "uzp2  ";    *ar = "8h";   return;
      case ARM64vecb_UZP28x16:     *nm = "uzp2  ";    *ar = "16b";  return;
      case ARM64vecb_ZIP132x4:     *nm = "zip1  ";    *ar = "4s";   return;
      case ARM64vecb_ZIP116x8:     *nm = "zip1  ";    *ar = "8h";   return;
      case ARM64vecb_ZIP18x16:     *nm = "zip1  ";    *ar = "16b";  return;
      case ARM64vecb_ZIP232x4:     *nm = "zip2  ";    *ar = "4s";   return;
      case ARM64vecb_ZIP216x8:     *nm = "zip2  ";    *ar = "8h";   return;
      case ARM64vecb_ZIP28x16:     *nm = "zip2  ";    *ar = "16b";  return;
      case ARM64vecb_PMUL8x16:     *nm = "pmul  ";    *ar = "16b";  return;
      case ARM64vecb_PMULL8x8:     *nm = "pmull ";    *ar = "8hbb"; return;
      case ARM64vecb_UMULL2DSS:    *nm = "umull ";    *ar = "2dss"; return;
      case ARM64vecb_UMULL4SHH:    *nm = "umull ";    *ar = "4shh"; return;
      case ARM64vecb_UMULL8HBB:    *nm = "umull ";    *ar = "8hbb"; return;
      case ARM64vecb_SMULL2DSS:    *nm = "smull ";    *ar = "2dss"; return;
      case ARM64vecb_SMULL4SHH:    *nm = "smull ";    *ar = "4shh"; return;
      case ARM64vecb_SMULL8HBB:    *nm = "smull ";    *ar = "8hbb"; return;
      case ARM64vecb_SQADD64x2:    *nm = "sqadd ";    *ar = "2d";   return;
      case ARM64vecb_SQADD32x4:    *nm = "sqadd ";    *ar = "4s";   return;
      case ARM64vecb_SQADD16x8:    *nm = "sqadd ";    *ar = "8h";   return;
      case ARM64vecb_SQADD8x16:    *nm = "sqadd ";    *ar = "16b";  return;
      case ARM64vecb_UQADD64x2:    *nm = "uqadd ";    *ar = "2d";   return;
      case ARM64vecb_UQADD32x4:    *nm = "uqadd ";    *ar = "4s";   return;
      case ARM64vecb_UQADD16x8:    *nm = "uqadd ";    *ar = "8h";   return;
      case ARM64vecb_UQADD8x16:    *nm = "uqadd ";    *ar = "16b";  return;
      case ARM64vecb_SQSUB64x2:    *nm = "sqsub ";    *ar = "2d";   return;
      case ARM64vecb_SQSUB32x4:    *nm = "sqsub ";    *ar = "4s";   return;
      case ARM64vecb_SQSUB16x8:    *nm = "sqsub ";    *ar = "8h";   return;
      case ARM64vecb_SQSUB8x16:    *nm = "sqsub ";    *ar = "16b";  return;
      case ARM64vecb_UQSUB64x2:    *nm = "uqsub ";    *ar = "2d";   return;
      case ARM64vecb_UQSUB32x4:    *nm = "uqsub ";    *ar = "4s";   return;
      case ARM64vecb_UQSUB16x8:    *nm = "uqsub ";    *ar = "8h";   return;
      case ARM64vecb_UQSUB8x16:    *nm = "uqsub ";    *ar = "16b";  return;
      case ARM64vecb_SQDMULL2DSS:  *nm = "sqdmull";   *ar = "2dss"; return;
      case ARM64vecb_SQDMULL4SHH:  *nm = "sqdmull";   *ar = "4shh"; return;
      case ARM64vecb_SQDMULH32x4:  *nm = "sqdmulh";   *ar = "4s";   return;
      case ARM64vecb_SQDMULH16x8:  *nm = "sqdmulh";   *ar = "8h";   return;
      case ARM64vecb_SQRDMULH32x4: *nm = "sqrdmulh";  *ar = "4s";   return;
      case ARM64vecb_SQRDMULH16x8: *nm = "sqrdmulh";  *ar = "8h";   return;
      case ARM64vecb_SQSHL64x2:    *nm = "sqshl ";    *ar = "2d";   return;
      case ARM64vecb_SQSHL32x4:    *nm = "sqshl ";    *ar = "4s";   return;
      case ARM64vecb_SQSHL16x8:    *nm = "sqshl ";    *ar = "8h";   return;
      case ARM64vecb_SQSHL8x16:    *nm = "sqshl ";    *ar = "16b";  return;
      case ARM64vecb_UQSHL64x2:    *nm = "uqshl ";    *ar = "2d";   return;
      case ARM64vecb_UQSHL32x4:    *nm = "uqshl ";    *ar = "4s";   return;
      case ARM64vecb_UQSHL16x8:    *nm = "uqshl ";    *ar = "8h";   return;
      case ARM64vecb_UQSHL8x16:    *nm = "uqshl ";    *ar = "16b";  return;
      case ARM64vecb_SQRSHL64x2:   *nm = "sqrshl";    *ar = "2d";   return;
      case ARM64vecb_SQRSHL32x4:   *nm = "sqrshl";    *ar = "4s";   return;
      case ARM64vecb_SQRSHL16x8:   *nm = "sqrshl";    *ar = "8h";   return;
      case ARM64vecb_SQRSHL8x16:   *nm = "sqrshl";    *ar = "16b";  return;
      case ARM64vecb_UQRSHL64x2:   *nm = "uqrshl";    *ar = "2d";   return;
      case ARM64vecb_UQRSHL32x4:   *nm = "uqrshl";    *ar = "4s";   return;
      case ARM64vecb_UQRSHL16x8:   *nm = "uqrshl";    *ar = "8h";   return;
      case ARM64vecb_UQRSHL8x16:   *nm = "uqrshl";    *ar = "16b";  return;
      case ARM64vecb_SSHL64x2:     *nm = "sshl  ";    *ar = "2d";   return;
      case ARM64vecb_SSHL32x4:     *nm = "sshl  ";    *ar = "4s";   return;
      case ARM64vecb_SSHL16x8:     *nm = "sshl  ";    *ar = "8h";   return;
      case ARM64vecb_SSHL8x16:     *nm = "sshl  ";    *ar = "16b";  return;
      case ARM64vecb_USHL64x2:     *nm = "ushl  ";    *ar = "2d";   return;
      case ARM64vecb_USHL32x4:     *nm = "ushl  ";    *ar = "4s";   return;
      case ARM64vecb_USHL16x8:     *nm = "ushl  ";    *ar = "8h";   return;
      case ARM64vecb_USHL8x16:     *nm = "ushl  ";    *ar = "16b";  return;
      case ARM64vecb_SRSHL64x2:    *nm = "srshl ";    *ar = "2d";   return;
      case ARM64vecb_SRSHL32x4:    *nm = "srshl ";    *ar = "4s";   return;
      case ARM64vecb_SRSHL16x8:    *nm = "srshl ";    *ar = "8h";   return;
      case ARM64vecb_SRSHL8x16:    *nm = "srshl ";    *ar = "16b";  return;
      case ARM64vecb_URSHL64x2:    *nm = "urshl ";    *ar = "2d";   return;
      case ARM64vecb_URSHL32x4:    *nm = "urshl ";    *ar = "4s";   return;
      case ARM64vecb_URSHL16x8:    *nm = "urshl ";    *ar = "8h";   return;
      case ARM64vecb_URSHL8x16:    *nm = "urshl ";    *ar = "16b";  return;
      case ARM64vecb_FRECPS64x2:   *nm = "frecps";    *ar = "2d";   return;
      case ARM64vecb_FRECPS32x4:   *nm = "frecps";    *ar = "4s";   return;
      case ARM64vecb_FRSQRTS64x2:  *nm = "frsqrts";   *ar = "2d";   return;
      case ARM64vecb_FRSQRTS32x4:  *nm = "frsqrts";   *ar = "4s";   return;
      default: vpanic("showARM64VecBinOp");
   }
}

static void showARM64VecModifyOp(/*OUT*/const HChar** nm,
                                 /*OUT*/const HChar** ar,
                                 ARM64VecModifyOp op ) {
   switch (op) {
      case ARM64vecmo_SUQADD64x2:   *nm = "suqadd";    *ar = "2d";   return;
      case ARM64vecmo_SUQADD32x4:   *nm = "suqadd";    *ar = "4s";   return;
      case ARM64vecmo_SUQADD16x8:   *nm = "suqadd";    *ar = "8h";   return;
      case ARM64vecmo_SUQADD8x16:   *nm = "suqadd";    *ar = "16b";  return;
      case ARM64vecmo_USQADD64x2:   *nm = "usqadd";    *ar = "2d";   return;
      case ARM64vecmo_USQADD32x4:   *nm = "usqadd";    *ar = "4s";   return;
      case ARM64vecmo_USQADD16x8:   *nm = "usqadd";    *ar = "8h";   return;
      case ARM64vecmo_USQADD8x16:   *nm = "usqadd";    *ar = "16b";  return;
      default: vpanic("showARM64VecModifyOp");
   }
}

static void showARM64VecUnaryOp(/*OUT*/const HChar** nm,
                                /*OUT*/const HChar** ar, ARM64VecUnaryOp op )
{
   switch (op) {
      case ARM64vecu_FNEG64x2:    *nm = "fneg ";   *ar = "2d";  return;
      case ARM64vecu_FNEG32x4:    *nm = "fneg ";   *ar = "4s";  return;
      case ARM64vecu_FNEG16x8:    *nm = "fneg ";   *ar = "8h";  return;
      case ARM64vecu_FABS64x2:    *nm = "fabs ";   *ar = "2d";  return;
      case ARM64vecu_FABS32x4:    *nm = "fabs ";   *ar = "4s";  return;
      case ARM64vecu_FABS16x8:    *nm = "fabs ";   *ar = "8h";  return;
      case ARM64vecu_NOT:         *nm = "not  ";   *ar = "all"; return;
      case ARM64vecu_ABS64x2:     *nm = "abs  ";   *ar = "2d";  return;
      case ARM64vecu_ABS32x4:     *nm = "abs  ";   *ar = "4s";  return;
      case ARM64vecu_ABS16x8:     *nm = "abs  ";   *ar = "8h";  return;
      case ARM64vecu_ABS8x16:     *nm = "abs  ";   *ar = "16b"; return;
      case ARM64vecu_CLS32x4:     *nm = "cls  ";   *ar = "4s";  return;
      case ARM64vecu_CLS16x8:     *nm = "cls  ";   *ar = "8h";  return;
      case ARM64vecu_CLS8x16:     *nm = "cls  ";   *ar = "16b"; return;
      case ARM64vecu_CLZ32x4:     *nm = "clz  ";   *ar = "4s";  return;
      case ARM64vecu_CLZ16x8:     *nm = "clz  ";   *ar = "8h";  return;
      case ARM64vecu_CLZ8x16:     *nm = "clz  ";   *ar = "16b"; return;
      case ARM64vecu_CNT8x16:     *nm = "cnt  ";   *ar = "16b"; return;
      case ARM64vecu_RBIT:        *nm = "rbit ";   *ar = "16b"; return;
      case ARM64vecu_REV1616B:    *nm = "rev16";   *ar = "16b"; return;
      case ARM64vecu_REV3216B:    *nm = "rev32";   *ar = "16b"; return;
      case ARM64vecu_REV328H:     *nm = "rev32";   *ar = "8h";  return;
      case ARM64vecu_REV6416B:    *nm = "rev64";   *ar = "16b"; return;
      case ARM64vecu_REV648H:     *nm = "rev64";   *ar = "8h";  return;
      case ARM64vecu_REV644S:     *nm = "rev64";   *ar = "4s";  return;
      case ARM64vecu_URECPE32x4:  *nm = "urecpe";  *ar = "4s";  return;
      case ARM64vecu_URSQRTE32x4: *nm = "ursqrte"; *ar = "4s";  return;
      case ARM64vecu_FRECPE64x2:  *nm = "frecpe";  *ar = "2d";  return;
      case ARM64vecu_FRECPE32x4:  *nm = "frecpe";  *ar = "4s";  return;
      case ARM64vecu_FRSQRTE64x2: *nm = "frsqrte"; *ar = "2d";  return;
      case ARM64vecu_FRSQRTE32x4: *nm = "frsqrte"; *ar = "4s";  return;
      case ARM64vecu_FSQRT64x2:   *nm = "fsqrt";   *ar = "2d";  return;
      case ARM64vecu_FSQRT32x4:   *nm = "fsqrt";   *ar = "4s";  return;
      case ARM64vecu_FSQRT16x8:   *nm = "fsqrt";   *ar = "8h";  return;
      default: vpanic("showARM64VecUnaryOp");
   }
}

static void showARM64VecShiftImmOp(/*OUT*/const HChar** nm,
                                   /*OUT*/const HChar** ar,
                                   ARM64VecShiftImmOp op )
{
   switch (op) {
      case ARM64vecshi_USHR64x2:    *nm = "ushr  ";   *ar = "2d";  return;
      case ARM64vecshi_USHR32x4:    *nm = "ushr  ";   *ar = "4s";  return;
      case ARM64vecshi_USHR16x8:    *nm = "ushr  ";   *ar = "8h";  return;
      case ARM64vecshi_USHR8x16:    *nm = "ushr  ";   *ar = "16b"; return;
      case ARM64vecshi_SSHR64x2:    *nm = "sshr  ";   *ar = "2d";  return;
      case ARM64vecshi_SSHR32x4:    *nm = "sshr  ";   *ar = "4s";  return;
      case ARM64vecshi_SSHR16x8:    *nm = "sshr  ";   *ar = "8h";  return;
      case ARM64vecshi_SSHR8x16:    *nm = "sshr  ";   *ar = "16b"; return;
      case ARM64vecshi_SHL64x2:     *nm = "shl   ";   *ar = "2d";  return;
      case ARM64vecshi_SHL32x4:     *nm = "shl   ";   *ar = "4s";  return;
      case ARM64vecshi_SHL16x8:     *nm = "shl   ";   *ar = "8h";  return;
      case ARM64vecshi_SHL8x16:     *nm = "shl   ";   *ar = "16b"; return;
      case ARM64vecshi_SQSHRN2SD:   *nm = "sqshrn";   *ar = "2sd"; return;
      case ARM64vecshi_SQSHRN4HS:   *nm = "sqshrn";   *ar = "4hs"; return;
      case ARM64vecshi_SQSHRN8BH:   *nm = "sqshrn";   *ar = "8bh"; return;
      case ARM64vecshi_UQSHRN2SD:   *nm = "uqshrn";   *ar = "2sd"; return;
      case ARM64vecshi_UQSHRN4HS:   *nm = "uqshrn";   *ar = "4hs"; return;
      case ARM64vecshi_UQSHRN8BH:   *nm = "uqshrn";   *ar = "8bh"; return;
      case ARM64vecshi_SQSHRUN2SD:  *nm = "sqshrun";  *ar = "2sd"; return;
      case ARM64vecshi_SQSHRUN4HS:  *nm = "sqshrun";  *ar = "4hs"; return;
      case ARM64vecshi_SQSHRUN8BH:  *nm = "sqshrun";  *ar = "8bh"; return;
      case ARM64vecshi_SQRSHRN2SD:  *nm = "sqrshrn";  *ar = "2sd"; return;
      case ARM64vecshi_SQRSHRN4HS:  *nm = "sqrshrn";  *ar = "4hs"; return;
      case ARM64vecshi_SQRSHRN8BH:  *nm = "sqrshrn";  *ar = "8bh"; return;
      case ARM64vecshi_UQRSHRN2SD:  *nm = "uqrshrn";  *ar = "2sd"; return;
      case ARM64vecshi_UQRSHRN4HS:  *nm = "uqrshrn";  *ar = "4hs"; return;
      case ARM64vecshi_UQRSHRN8BH:  *nm = "uqrshrn";  *ar = "8bh"; return;
      case ARM64vecshi_SQRSHRUN2SD: *nm = "sqrshrun"; *ar = "2sd"; return;
      case ARM64vecshi_SQRSHRUN4HS: *nm = "sqrshrun"; *ar = "4hs"; return;
      case ARM64vecshi_SQRSHRUN8BH: *nm = "sqrshrun"; *ar = "8bh"; return;
      case ARM64vecshi_UQSHL64x2:   *nm = "uqshl ";   *ar = "2d";  return;
      case ARM64vecshi_UQSHL32x4:   *nm = "uqshl ";   *ar = "4s";  return;
      case ARM64vecshi_UQSHL16x8:   *nm = "uqshl ";   *ar = "8h";  return;
      case ARM64vecshi_UQSHL8x16:   *nm = "uqshl ";   *ar = "16b"; return;
      case ARM64vecshi_SQSHL64x2:   *nm = "sqshl ";   *ar = "2d";  return;
      case ARM64vecshi_SQSHL32x4:   *nm = "sqshl ";   *ar = "4s";  return;
      case ARM64vecshi_SQSHL16x8:   *nm = "sqshl ";   *ar = "8h";  return;
      case ARM64vecshi_SQSHL8x16:   *nm = "sqshl ";   *ar = "16b"; return;
      case ARM64vecshi_SQSHLU64x2:  *nm = "sqshlu";   *ar = "2d";  return;
      case ARM64vecshi_SQSHLU32x4:  *nm = "sqshlu";   *ar = "4s";  return;
      case ARM64vecshi_SQSHLU16x8:  *nm = "sqshlu";   *ar = "8h";  return;
      case ARM64vecshi_SQSHLU8x16:  *nm = "sqshlu";   *ar = "16b"; return;
      default: vpanic("showARM64VecShiftImmOp");
   }
}

static const HChar* showARM64VecNarrowOp(ARM64VecNarrowOp op) {
   switch (op) {
      case ARM64vecna_XTN:    return "xtn   ";
      case ARM64vecna_SQXTN:  return "sqxtn ";
      case ARM64vecna_UQXTN:  return "uqxtn ";
      case ARM64vecna_SQXTUN: return "sqxtun";
      default: vpanic("showARM64VecNarrowOp");
   }
}

ARM64Instr* ARM64Instr_Arith ( HReg dst,
                               HReg argL, ARM64RIA* argR, Bool isAdd ) {
   ARM64Instr* i = LibVEX_Alloc_inline(sizeof(ARM64Instr));
   i->tag                 = ARM64in_Arith;
   i->ARM64in.Arith.dst   = dst;
   i->ARM64in.Arith.argL  = argL;
   i->ARM64in.Arith.argR  = argR;
   i->ARM64in.Arith.isAdd = isAdd;
   return i;
}
ARM64Instr* ARM64Instr_Cmp ( HReg argL, ARM64RIA* argR, Bool is64 ) {
   ARM64Instr* i = LibVEX_Alloc_inline(sizeof(ARM64Instr));
   i->tag              = ARM64in_Cmp;
   i->ARM64in.Cmp.argL = argL;
   i->ARM64in.Cmp.argR = argR;
   i->ARM64in.Cmp.is64 = is64;
   return i;
}
ARM64Instr* ARM64Instr_Logic ( HReg dst,
                               HReg argL, ARM64RIL* argR, ARM64LogicOp op ) {
   ARM64Instr* i = LibVEX_Alloc_inline(sizeof(ARM64Instr));
   i->tag                 = ARM64in_Logic;
   i->ARM64in.Logic.dst   = dst;
   i->ARM64in.Logic.argL  = argL;
   i->ARM64in.Logic.argR  = argR;
   i->ARM64in.Logic.op    = op;
   return i;
}
ARM64Instr* ARM64Instr_RRS ( HReg dst, HReg argL, HReg argR,
                             ARM64ShiftOp shiftOp, UChar amt,
                             ARM64RRSOp mainOp ) {
   ARM64Instr* i = LibVEX_Alloc_inline(sizeof(ARM64Instr));
   i->tag                 = ARM64in_RRS;
   i->ARM64in.RRS.dst     = dst;
   i->ARM64in.RRS.argL    = argL;
   i->ARM64in.RRS.argR    = argR;
   i->ARM64in.RRS.shiftOp = shiftOp;
   i->ARM64in.RRS.amt     = amt;
   i->ARM64in.RRS.mainOp  = mainOp;
   vassert(amt >= 1 && amt <= 63);
   return i;
}
ARM64Instr* ARM64Instr_Test ( HReg argL, ARM64RIL* argR ) {
   ARM64Instr* i = LibVEX_Alloc_inline(sizeof(ARM64Instr));
   i->tag               = ARM64in_Test;
   i->ARM64in.Test.argL = argL;
   i->ARM64in.Test.argR = argR;
   return i;
}
ARM64Instr* ARM64Instr_Shift ( HReg dst,
                               HReg argL, ARM64RI6* argR, ARM64ShiftOp op ) {
   ARM64Instr* i = LibVEX_Alloc_inline(sizeof(ARM64Instr));
   i->tag                = ARM64in_Shift;
   i->ARM64in.Shift.dst  = dst;
   i->ARM64in.Shift.argL = argL;
   i->ARM64in.Shift.argR = argR;
   i->ARM64in.Shift.op   = op;
   return i;
}
ARM64Instr* ARM64Instr_Unary ( HReg dst, HReg src, ARM64UnaryOp op ) {
   ARM64Instr* i = LibVEX_Alloc_inline(sizeof(ARM64Instr));
   i->tag               = ARM64in_Unary;
   i->ARM64in.Unary.dst = dst;
   i->ARM64in.Unary.src = src;
   i->ARM64in.Unary.op  = op;
   return i;
}
ARM64Instr* ARM64Instr_Set64 ( HReg dst, ARM64CondCode cond ) {
   ARM64Instr* i = LibVEX_Alloc_inline(sizeof(ARM64Instr));
   i->tag                = ARM64in_Set64;
   i->ARM64in.Set64.dst  = dst;
   i->ARM64in.Set64.cond = cond;
   return i;
}
ARM64Instr* ARM64Instr_MovI ( HReg dst, HReg src ) {
   ARM64Instr* i      = LibVEX_Alloc_inline(sizeof(ARM64Instr));
   i->tag             = ARM64in_MovI;
   i->ARM64in.MovI.dst = dst;
   i->ARM64in.MovI.src = src;
   vassert(hregClass(src) == HRcInt64);
   vassert(hregClass(dst) == HRcInt64);
   return i;
}
ARM64Instr* ARM64Instr_Imm64 ( HReg dst, ULong imm64 ) {
   ARM64Instr* i = LibVEX_Alloc_inline(sizeof(ARM64Instr));
   i->tag                 = ARM64in_Imm64;
   i->ARM64in.Imm64.dst   = dst;
   i->ARM64in.Imm64.imm64 = imm64;
   return i;
}
ARM64Instr* ARM64Instr_LdSt64 ( Bool isLoad, HReg rD, ARM64AMode* amode ) {
   ARM64Instr* i = LibVEX_Alloc_inline(sizeof(ARM64Instr));
   i->tag                   = ARM64in_LdSt64;
   i->ARM64in.LdSt64.isLoad = isLoad;
   i->ARM64in.LdSt64.rD     = rD;
   i->ARM64in.LdSt64.amode  = amode;
   return i;
}
ARM64Instr* ARM64Instr_LdSt32 ( Bool isLoad, HReg rD, ARM64AMode* amode ) {
   ARM64Instr* i = LibVEX_Alloc_inline(sizeof(ARM64Instr));
   i->tag                   = ARM64in_LdSt32;
   i->ARM64in.LdSt32.isLoad = isLoad;
   i->ARM64in.LdSt32.rD     = rD;
   i->ARM64in.LdSt32.amode  = amode;
   return i;
}
ARM64Instr* ARM64Instr_LdSt16 ( Bool isLoad, HReg rD, ARM64AMode* amode ) {
   ARM64Instr* i = LibVEX_Alloc_inline(sizeof(ARM64Instr));
   i->tag                   = ARM64in_LdSt16;
   i->ARM64in.LdSt16.isLoad = isLoad;
   i->ARM64in.LdSt16.rD     = rD;
   i->ARM64in.LdSt16.amode  = amode;
   return i;
}
ARM64Instr* ARM64Instr_LdSt8 ( Bool isLoad, HReg rD, ARM64AMode* amode ) {
   ARM64Instr* i = LibVEX_Alloc_inline(sizeof(ARM64Instr));
   i->tag                  = ARM64in_LdSt8;
   i->ARM64in.LdSt8.isLoad = isLoad;
   i->ARM64in.LdSt8.rD     = rD;
   i->ARM64in.LdSt8.amode  = amode;
   return i;
}
ARM64Instr* ARM64Instr_XDirect ( Addr64 dstGA, ARM64AMode* amPC,
                                 ARM64CondCode cond, Bool toFastEP ) {
   ARM64Instr* i               = LibVEX_Alloc_inline(sizeof(ARM64Instr));
   i->tag                      = ARM64in_XDirect;
   i->ARM64in.XDirect.dstGA    = dstGA;
   i->ARM64in.XDirect.amPC     = amPC;
   i->ARM64in.XDirect.cond     = cond;
   i->ARM64in.XDirect.toFastEP = toFastEP;
   return i;
}
ARM64Instr* ARM64Instr_XIndir ( HReg dstGA, ARM64AMode* amPC,
                                ARM64CondCode cond ) {
   ARM64Instr* i           = LibVEX_Alloc_inline(sizeof(ARM64Instr));
   i->tag                  = ARM64in_XIndir;
   i->ARM64in.XIndir.dstGA = dstGA;
   i->ARM64in.XIndir.amPC  = amPC;
   i->ARM64in.XIndir.cond  = cond;
   return i;
}
ARM64Instr* ARM64Instr_XAssisted ( HReg dstGA, ARM64AMode* amPC,
                                   ARM64CondCode cond, IRJumpKind jk ) {
   ARM64Instr* i              = LibVEX_Alloc_inline(sizeof(ARM64Instr));
   i->tag                     = ARM64in_XAssisted;
   i->ARM64in.XAssisted.dstGA = dstGA;
   i->ARM64in.XAssisted.amPC  = amPC;
   i->ARM64in.XAssisted.cond  = cond;
   i->ARM64in.XAssisted.jk    = jk;
   return i;
}
ARM64Instr* ARM64Instr_CSel ( HReg dst, HReg argL, HReg argR,
                              ARM64CondCode cond ) {
   ARM64Instr* i        = LibVEX_Alloc_inline(sizeof(ARM64Instr));
   i->tag               = ARM64in_CSel;
   i->ARM64in.CSel.dst  = dst;
   i->ARM64in.CSel.argL = argL;
   i->ARM64in.CSel.argR = argR;
   i->ARM64in.CSel.cond = cond;
   return i;
}
ARM64Instr* ARM64Instr_Call ( ARM64CondCode cond, Addr64 target, Int nArgRegs,
                              RetLoc rloc ) {
   ARM64Instr* i = LibVEX_Alloc_inline(sizeof(ARM64Instr));
   i->tag                   = ARM64in_Call;
   i->ARM64in.Call.cond     = cond;
   i->ARM64in.Call.target   = target;
   i->ARM64in.Call.nArgRegs = nArgRegs;
   i->ARM64in.Call.rloc     = rloc;
   vassert(is_sane_RetLoc(rloc));
   return i;
}
extern ARM64Instr* ARM64Instr_AddToSP ( Int simm ) {
   ARM64Instr* i = LibVEX_Alloc_inline(sizeof(ARM64Instr));
   i->tag                  = ARM64in_AddToSP;
   i->ARM64in.AddToSP.simm = simm;
   vassert(-4096 < simm && simm < 4096);
   vassert(0 == (simm & 0xF));
   return i;
}
extern ARM64Instr* ARM64Instr_FromSP  ( HReg dst ) {
   ARM64Instr* i = LibVEX_Alloc_inline(sizeof(ARM64Instr));
   i->tag                = ARM64in_FromSP;
   i->ARM64in.FromSP.dst = dst;
   return i;
}
ARM64Instr* ARM64Instr_Mul ( HReg dst, HReg argL, HReg argR,
                             ARM64MulOp op ) {
   ARM64Instr* i = LibVEX_Alloc_inline(sizeof(ARM64Instr));
   i->tag              = ARM64in_Mul;
   i->ARM64in.Mul.dst  = dst;
   i->ARM64in.Mul.argL = argL;
   i->ARM64in.Mul.argR = argR;
   i->ARM64in.Mul.op   = op;
   return i;
}
ARM64Instr* ARM64Instr_LdrEX ( Int szB ) {
   ARM64Instr* i = LibVEX_Alloc_inline(sizeof(ARM64Instr));
   i->tag               = ARM64in_LdrEX;
   i->ARM64in.LdrEX.szB = szB;
   vassert(szB == 8 || szB == 4 || szB == 2 || szB == 1);
   return i;
}
ARM64Instr* ARM64Instr_StrEX ( Int szB ) {
   ARM64Instr* i = LibVEX_Alloc_inline(sizeof(ARM64Instr));
   i->tag               = ARM64in_StrEX;
   i->ARM64in.StrEX.szB = szB;
   vassert(szB == 8 || szB == 4 || szB == 2 || szB == 1);
   return i;
}
ARM64Instr* ARM64Instr_LdrEXP ( void ) {
   ARM64Instr* i = LibVEX_Alloc_inline(sizeof(ARM64Instr));
   i->tag        = ARM64in_LdrEXP;
   return i;
}
ARM64Instr* ARM64Instr_StrEXP ( void ) {
   ARM64Instr* i = LibVEX_Alloc_inline(sizeof(ARM64Instr));
   i->tag        = ARM64in_StrEXP;
   return i;
}
ARM64Instr* ARM64Instr_CAS ( Int szB ) {
   ARM64Instr* i = LibVEX_Alloc_inline(sizeof(ARM64Instr));
   i->tag             = ARM64in_CAS;
   i->ARM64in.CAS.szB = szB;
   vassert(szB == 8 || szB == 4 || szB == 2 || szB == 1);
   return i;
}
ARM64Instr* ARM64Instr_CASP ( Int szB ) {
   ARM64Instr* i = LibVEX_Alloc_inline(sizeof(ARM64Instr));
   i->tag              = ARM64in_CASP;
   i->ARM64in.CASP.szB = szB;
   vassert(szB == 8 || szB == 4);
   return i;
}
ARM64Instr* ARM64Instr_MFence ( void ) {
   ARM64Instr* i = LibVEX_Alloc_inline(sizeof(ARM64Instr));
   i->tag        = ARM64in_MFence;
   return i;
}
ARM64Instr* ARM64Instr_ClrEX ( void ) {
   ARM64Instr* i = LibVEX_Alloc_inline(sizeof(ARM64Instr));
   i->tag        = ARM64in_ClrEX;
   return i;
}
ARM64Instr* ARM64Instr_VLdStH ( Bool isLoad, HReg sD, HReg rN, UInt uimm12 ) {
   ARM64Instr* i = LibVEX_Alloc_inline(sizeof(ARM64Instr));
   i->tag                   = ARM64in_VLdStH;
   i->ARM64in.VLdStH.isLoad = isLoad;
   i->ARM64in.VLdStH.hD     = sD;
   i->ARM64in.VLdStH.rN     = rN;
   i->ARM64in.VLdStH.uimm12 = uimm12;
   vassert(uimm12 < 8192 && 0 == (uimm12 & 1));
   return i;
}
ARM64Instr* ARM64Instr_VLdStS ( Bool isLoad, HReg sD, HReg rN, UInt uimm12 ) {
   ARM64Instr* i = LibVEX_Alloc_inline(sizeof(ARM64Instr));
   i->tag                   = ARM64in_VLdStS;
   i->ARM64in.VLdStS.isLoad = isLoad;
   i->ARM64in.VLdStS.sD     = sD;
   i->ARM64in.VLdStS.rN     = rN;
   i->ARM64in.VLdStS.uimm12 = uimm12;
   vassert(uimm12 < 16384 && 0 == (uimm12 & 3));
   return i;
}
ARM64Instr* ARM64Instr_VLdStD ( Bool isLoad, HReg dD, HReg rN, UInt uimm12 ) {
   ARM64Instr* i = LibVEX_Alloc_inline(sizeof(ARM64Instr));
   i->tag                   = ARM64in_VLdStD;
   i->ARM64in.VLdStD.isLoad = isLoad;
   i->ARM64in.VLdStD.dD     = dD;
   i->ARM64in.VLdStD.rN     = rN;
   i->ARM64in.VLdStD.uimm12 = uimm12;
   vassert(uimm12 < 32768 && 0 == (uimm12 & 7));
   return i;
}
ARM64Instr* ARM64Instr_VLdStQ ( Bool isLoad, HReg rQ, HReg rN ) {
   ARM64Instr* i = LibVEX_Alloc_inline(sizeof(ARM64Instr));
   i->tag                   = ARM64in_VLdStQ;
   i->ARM64in.VLdStQ.isLoad = isLoad;
   i->ARM64in.VLdStQ.rQ     = rQ;
   i->ARM64in.VLdStQ.rN     = rN;
   return i;
}
ARM64Instr* ARM64Instr_VCvtI2F ( ARM64CvtOp how, HReg rD, HReg rS ) {
   ARM64Instr* i = LibVEX_Alloc_inline(sizeof(ARM64Instr));
   i->tag                 = ARM64in_VCvtI2F;
   i->ARM64in.VCvtI2F.how = how;
   i->ARM64in.VCvtI2F.rD  = rD;
   i->ARM64in.VCvtI2F.rS  = rS;
   return i;
}
ARM64Instr* ARM64Instr_VCvtF2I ( ARM64CvtOp how, HReg rD, HReg rS,
                                 UChar armRM, Bool tiesToAway ) {
   ARM64Instr* i = LibVEX_Alloc_inline(sizeof(ARM64Instr));
   i->tag                   = ARM64in_VCvtF2I;
   i->ARM64in.VCvtF2I.how   = how;
   i->ARM64in.VCvtF2I.rD    = rD;
   i->ARM64in.VCvtF2I.rS    = rS;
   i->ARM64in.VCvtF2I.armRM = armRM;
   i->ARM64in.VCvtF2I.tiesToAway = tiesToAway;
   vassert(armRM <= 3);
   return i;
}
ARM64Instr* ARM64Instr_VCvtSD ( Bool sToD, HReg dst, HReg src ) {
   ARM64Instr* i = LibVEX_Alloc_inline(sizeof(ARM64Instr));
   i->tag                 = ARM64in_VCvtSD;
   i->ARM64in.VCvtSD.sToD = sToD;
   i->ARM64in.VCvtSD.dst  = dst;
   i->ARM64in.VCvtSD.src  = src;
   return i;
}
ARM64Instr* ARM64Instr_VCvtHS ( Bool hToS, HReg dst, HReg src ) {
   ARM64Instr* i = LibVEX_Alloc_inline(sizeof(ARM64Instr));
   i->tag                 = ARM64in_VCvtHS;
   i->ARM64in.VCvtHS.hToS = hToS;
   i->ARM64in.VCvtHS.dst  = dst;
   i->ARM64in.VCvtHS.src  = src;
   return i;
}
ARM64Instr* ARM64Instr_VCvtHD ( Bool hToD, HReg dst, HReg src ) {
   ARM64Instr* i = LibVEX_Alloc_inline(sizeof(ARM64Instr));
   i->tag                 = ARM64in_VCvtHD;
   i->ARM64in.VCvtHD.hToD = hToD;
   i->ARM64in.VCvtHD.dst  = dst;
   i->ARM64in.VCvtHD.src  = src;
   return i;
}
ARM64Instr* ARM64Instr_VUnaryD ( ARM64FpUnaryOp op, HReg dst, HReg src ) {
   ARM64Instr* i = LibVEX_Alloc_inline(sizeof(ARM64Instr));
   i->tag                 = ARM64in_VUnaryD;
   i->ARM64in.VUnaryD.op  = op;
   i->ARM64in.VUnaryD.dst = dst;
   i->ARM64in.VUnaryD.src = src;
   return i;
}
ARM64Instr* ARM64Instr_VUnaryS ( ARM64FpUnaryOp op, HReg dst, HReg src ) {
   ARM64Instr* i = LibVEX_Alloc_inline(sizeof(ARM64Instr));
   i->tag                 = ARM64in_VUnaryS;
   i->ARM64in.VUnaryS.op  = op;
   i->ARM64in.VUnaryS.dst = dst;
   i->ARM64in.VUnaryS.src = src;
   return i;
}
ARM64Instr* ARM64Instr_VUnaryH ( ARM64FpUnaryOp op, HReg dst, HReg src ) {
   ARM64Instr* i = LibVEX_Alloc_inline(sizeof(ARM64Instr));
   i->tag                 = ARM64in_VUnaryH;
   i->ARM64in.VUnaryH.op  = op;
   i->ARM64in.VUnaryH.dst = dst;
   i->ARM64in.VUnaryH.src = src;
   return i;
}
ARM64Instr* ARM64Instr_VBinD ( ARM64FpBinOp op,
                               HReg dst, HReg argL, HReg argR ) {
   ARM64Instr* i = LibVEX_Alloc_inline(sizeof(ARM64Instr));
   i->tag                = ARM64in_VBinD;
   i->ARM64in.VBinD.op   = op;
   i->ARM64in.VBinD.dst  = dst;
   i->ARM64in.VBinD.argL = argL;
   i->ARM64in.VBinD.argR = argR;
   return i;
}
ARM64Instr* ARM64Instr_VBinS ( ARM64FpBinOp op,
                               HReg dst, HReg argL, HReg argR ) {
   ARM64Instr* i = LibVEX_Alloc_inline(sizeof(ARM64Instr));
   i->tag                = ARM64in_VBinS;
   i->ARM64in.VBinS.op   = op;
   i->ARM64in.VBinS.dst  = dst;
   i->ARM64in.VBinS.argL = argL;
   i->ARM64in.VBinS.argR = argR;
   return i;
}
ARM64Instr* ARM64Instr_VBinH ( ARM64FpBinOp op,
                               HReg dst, HReg argL, HReg argR ) {
   ARM64Instr* i = LibVEX_Alloc_inline(sizeof(ARM64Instr));
   i->tag                = ARM64in_VBinH;
   i->ARM64in.VBinH.op   = op;
   i->ARM64in.VBinH.dst  = dst;
   i->ARM64in.VBinH.argL = argL;
   i->ARM64in.VBinH.argR = argR;
   return i;
}
ARM64Instr* ARM64Instr_VTriD ( ARM64FpTriOp op,
                               HReg dst, HReg arg1, HReg arg2, HReg arg3 ) {
   ARM64Instr* i = LibVEX_Alloc_inline(sizeof(ARM64Instr));
   i->tag                = ARM64in_VTriD;
   i->ARM64in.VTriD.op   = op;
   i->ARM64in.VTriD.dst  = dst;
   i->ARM64in.VTriD.arg1 = arg1;
   i->ARM64in.VTriD.arg2 = arg2;
   i->ARM64in.VTriD.arg3 = arg3;
   return i;
}
ARM64Instr* ARM64Instr_VTriS ( ARM64FpTriOp op,
                               HReg dst, HReg arg1, HReg arg2, HReg arg3 ) {
   ARM64Instr* i = LibVEX_Alloc_inline(sizeof(ARM64Instr));
   i->tag                = ARM64in_VTriS;
   i->ARM64in.VTriS.op   = op;
   i->ARM64in.VTriS.dst  = dst;
   i->ARM64in.VTriS.arg1 = arg1;
   i->ARM64in.VTriS.arg2 = arg2;
   i->ARM64in.VTriS.arg3 = arg3;
   return i;
}
ARM64Instr* ARM64Instr_VCmpD ( HReg argL, HReg argR ) {
   ARM64Instr* i = LibVEX_Alloc_inline(sizeof(ARM64Instr));
   i->tag                = ARM64in_VCmpD;
   i->ARM64in.VCmpD.argL = argL;
   i->ARM64in.VCmpD.argR = argR;
   return i;
}
ARM64Instr* ARM64Instr_VCmpS ( HReg argL, HReg argR ) {
   ARM64Instr* i = LibVEX_Alloc_inline(sizeof(ARM64Instr));
   i->tag                = ARM64in_VCmpS;
   i->ARM64in.VCmpS.argL = argL;
   i->ARM64in.VCmpS.argR = argR;
   return i;
}
ARM64Instr* ARM64Instr_VCmpH ( HReg argL, HReg argR ) {
   ARM64Instr* i = LibVEX_Alloc_inline(sizeof(ARM64Instr));
   i->tag                = ARM64in_VCmpH;
   i->ARM64in.VCmpH.argL = argL;
   i->ARM64in.VCmpH.argR = argR;
   return i;
}
ARM64Instr* ARM64Instr_VFCSel ( HReg dst, HReg argL, HReg argR,
                                ARM64CondCode cond, Bool isD ) {
   ARM64Instr* i          = LibVEX_Alloc_inline(sizeof(ARM64Instr));
   i->tag                 = ARM64in_VFCSel;
   i->ARM64in.VFCSel.dst  = dst;
   i->ARM64in.VFCSel.argL = argL;
   i->ARM64in.VFCSel.argR = argR;
   i->ARM64in.VFCSel.cond = cond;
   i->ARM64in.VFCSel.isD  = isD;
   return i;
}
ARM64Instr* ARM64Instr_FPCR ( Bool toFPCR, HReg iReg ) {
   ARM64Instr* i = LibVEX_Alloc_inline(sizeof(ARM64Instr));
   i->tag                 = ARM64in_FPCR;
   i->ARM64in.FPCR.toFPCR = toFPCR;
   i->ARM64in.FPCR.iReg   = iReg;
   return i;
}
ARM64Instr* ARM64Instr_FPSR ( Bool toFPSR, HReg iReg ) {
   ARM64Instr* i = LibVEX_Alloc_inline(sizeof(ARM64Instr));
   i->tag                 = ARM64in_FPSR;
   i->ARM64in.FPSR.toFPSR = toFPSR;
   i->ARM64in.FPSR.iReg   = iReg;
   return i;
}
ARM64Instr* ARM64Instr_VBinV ( ARM64VecBinOp op,
                               HReg dst, HReg argL, HReg argR ) {
   ARM64Instr* i = LibVEX_Alloc_inline(sizeof(ARM64Instr));
   i->tag                = ARM64in_VBinV;
   i->ARM64in.VBinV.op   = op;
   i->ARM64in.VBinV.dst  = dst;
   i->ARM64in.VBinV.argL = argL;
   i->ARM64in.VBinV.argR = argR;
   return i;
}
ARM64Instr* ARM64Instr_VModifyV ( ARM64VecModifyOp op, HReg mod, HReg arg ) {
   ARM64Instr* i = LibVEX_Alloc_inline(sizeof(ARM64Instr));
   i->tag                  = ARM64in_VModifyV;
   i->ARM64in.VModifyV.op  = op;
   i->ARM64in.VModifyV.mod = mod;
   i->ARM64in.VModifyV.arg = arg;
   return i;
}
ARM64Instr* ARM64Instr_VUnaryV ( ARM64VecUnaryOp op, HReg dst, HReg arg ) {
   ARM64Instr* i = LibVEX_Alloc_inline(sizeof(ARM64Instr));
   i->tag                 = ARM64in_VUnaryV;
   i->ARM64in.VUnaryV.op  = op;
   i->ARM64in.VUnaryV.dst = dst;
   i->ARM64in.VUnaryV.arg = arg;
   return i;
}
ARM64Instr* ARM64Instr_VNarrowV ( ARM64VecNarrowOp op,
                                  UInt dszBlg2, HReg dst, HReg src ) {
   ARM64Instr* i = LibVEX_Alloc_inline(sizeof(ARM64Instr));
   i->tag                      = ARM64in_VNarrowV;
   i->ARM64in.VNarrowV.op      = op;
   i->ARM64in.VNarrowV.dszBlg2 = dszBlg2;
   i->ARM64in.VNarrowV.dst     = dst;
   i->ARM64in.VNarrowV.src     = src;
   vassert(dszBlg2 == 0 || dszBlg2 == 1 || dszBlg2 == 2);
   return i;
}
ARM64Instr* ARM64Instr_VShiftImmV ( ARM64VecShiftImmOp op,
                                    HReg dst, HReg src, UInt amt ) {
   ARM64Instr* i = LibVEX_Alloc_inline(sizeof(ARM64Instr));
   i->tag                    = ARM64in_VShiftImmV;
   i->ARM64in.VShiftImmV.op  = op;
   i->ARM64in.VShiftImmV.dst = dst;
   i->ARM64in.VShiftImmV.src = src;
   i->ARM64in.VShiftImmV.amt = amt;
   UInt minSh = 0;
   UInt maxSh = 0;
   switch (op) {
      /* For right shifts, the allowed shift amounts are 1 .. lane_size.
         For left shifts,  the allowed shift amounts are 0 .. lane_size-1. 
      */
      case ARM64vecshi_USHR64x2: case ARM64vecshi_SSHR64x2:
      case ARM64vecshi_UQSHRN2SD: case ARM64vecshi_SQSHRN2SD:
      case ARM64vecshi_SQSHRUN2SD:
      case ARM64vecshi_UQRSHRN2SD: case ARM64vecshi_SQRSHRN2SD:
      case ARM64vecshi_SQRSHRUN2SD:
         minSh = 1; maxSh = 64; break;
      case ARM64vecshi_SHL64x2:
      case ARM64vecshi_UQSHL64x2: case ARM64vecshi_SQSHL64x2:
      case ARM64vecshi_SQSHLU64x2:
         minSh = 0; maxSh = 63; break;
      case ARM64vecshi_USHR32x4: case ARM64vecshi_SSHR32x4:
      case ARM64vecshi_UQSHRN4HS: case ARM64vecshi_SQSHRN4HS:
      case ARM64vecshi_SQSHRUN4HS:
      case ARM64vecshi_UQRSHRN4HS: case ARM64vecshi_SQRSHRN4HS:
      case ARM64vecshi_SQRSHRUN4HS:
         minSh = 1; maxSh = 32; break;
      case ARM64vecshi_SHL32x4:
      case ARM64vecshi_UQSHL32x4: case ARM64vecshi_SQSHL32x4:
      case ARM64vecshi_SQSHLU32x4:
         minSh = 0; maxSh = 31; break;
      case ARM64vecshi_USHR16x8: case ARM64vecshi_SSHR16x8:
      case ARM64vecshi_UQSHRN8BH: case ARM64vecshi_SQSHRN8BH:
      case ARM64vecshi_SQSHRUN8BH:
      case ARM64vecshi_UQRSHRN8BH: case ARM64vecshi_SQRSHRN8BH:
      case ARM64vecshi_SQRSHRUN8BH:
         minSh = 1; maxSh = 16; break;
      case ARM64vecshi_SHL16x8:
      case ARM64vecshi_UQSHL16x8: case ARM64vecshi_SQSHL16x8:
      case ARM64vecshi_SQSHLU16x8:
         minSh = 0; maxSh = 15; break;
      case ARM64vecshi_USHR8x16: case ARM64vecshi_SSHR8x16:
         minSh = 1; maxSh = 8; break;
      case ARM64vecshi_SHL8x16:
      case ARM64vecshi_UQSHL8x16: case ARM64vecshi_SQSHL8x16:
      case ARM64vecshi_SQSHLU8x16:
         minSh = 0; maxSh = 7; break;
      default:
         vassert(0);
   }
   vassert(maxSh > 0);
   vassert(amt >= minSh && amt <= maxSh);
   return i;
}
ARM64Instr* ARM64Instr_VExtV ( HReg dst, HReg srcLo, HReg srcHi, UInt amtB ) {
   ARM64Instr* i = LibVEX_Alloc_inline(sizeof(ARM64Instr));
   i->tag                 = ARM64in_VExtV;
   i->ARM64in.VExtV.dst   = dst;
   i->ARM64in.VExtV.srcLo = srcLo;
   i->ARM64in.VExtV.srcHi = srcHi;
   i->ARM64in.VExtV.amtB  = amtB;
   vassert(amtB >= 1 && amtB <= 15);
   return i;
}
ARM64Instr* ARM64Instr_VImmQ (HReg rQ, UShort imm) {
   ARM64Instr* i = LibVEX_Alloc_inline(sizeof(ARM64Instr));
   i->tag               = ARM64in_VImmQ;
   i->ARM64in.VImmQ.rQ  = rQ;
   i->ARM64in.VImmQ.imm = imm;
   /* Check that this is something that can actually be emitted. */
   switch (imm) {
      case 0x0000: case 0x0001: case 0x0003:
      case 0x000F: case 0x003F: case 0x00FF: case 0xFFFF:
         break;
      default:
         vassert(0);
   }
   return i;
}
ARM64Instr* ARM64Instr_VDfromX ( HReg rD, HReg rX ) {
   ARM64Instr* i = LibVEX_Alloc_inline(sizeof(ARM64Instr));
   i->tag                = ARM64in_VDfromX;
   i->ARM64in.VDfromX.rD = rD;
   i->ARM64in.VDfromX.rX = rX;
   return i;
}
ARM64Instr* ARM64Instr_VQfromX ( HReg rQ, HReg rXlo ) {
   ARM64Instr* i = LibVEX_Alloc_inline(sizeof(ARM64Instr));
   i->tag                  = ARM64in_VQfromX;
   i->ARM64in.VQfromX.rQ   = rQ;
   i->ARM64in.VQfromX.rXlo = rXlo;
   return i;
}
ARM64Instr* ARM64Instr_VQfromXX ( HReg rQ, HReg rXhi, HReg rXlo ) {
   ARM64Instr* i = LibVEX_Alloc_inline(sizeof(ARM64Instr));
   i->tag                   = ARM64in_VQfromXX;
   i->ARM64in.VQfromXX.rQ   = rQ;
   i->ARM64in.VQfromXX.rXhi = rXhi;
   i->ARM64in.VQfromXX.rXlo = rXlo;
   return i;
}
ARM64Instr* ARM64Instr_VXfromQ ( HReg rX, HReg rQ, UInt laneNo ) {
   ARM64Instr* i = LibVEX_Alloc_inline(sizeof(ARM64Instr));
   i->tag                    = ARM64in_VXfromQ;
   i->ARM64in.VXfromQ.rX     = rX;
   i->ARM64in.VXfromQ.rQ     = rQ;
   i->ARM64in.VXfromQ.laneNo = laneNo;
   vassert(laneNo <= 1);
   return i;
}
ARM64Instr* ARM64Instr_VXfromDorS ( HReg rX, HReg rDorS, Bool fromD ) {
   ARM64Instr* i = LibVEX_Alloc_inline(sizeof(ARM64Instr));
   i->tag                      = ARM64in_VXfromDorS;
   i->ARM64in.VXfromDorS.rX    = rX;
   i->ARM64in.VXfromDorS.rDorS = rDorS;
   i->ARM64in.VXfromDorS.fromD = fromD;
   return i;
}
ARM64Instr* ARM64Instr_VMov ( UInt szB, HReg dst, HReg src ) {
   ARM64Instr* i       = LibVEX_Alloc_inline(sizeof(ARM64Instr));
   i->tag              = ARM64in_VMov;
   i->ARM64in.VMov.szB = szB;
   i->ARM64in.VMov.dst = dst;
   i->ARM64in.VMov.src = src;
   switch (szB) {
      case 16:
        vassert(hregClass(src) == HRcVec128);
        vassert(hregClass(dst) == HRcVec128);
        break;
      case 8:
        vassert(hregClass(src) == HRcFlt64);
        vassert(hregClass(dst) == HRcFlt64);
        break;
      default:
        vpanic("ARM64Instr_VMov");
   }
   return i;
}
ARM64Instr* ARM64Instr_EvCheck ( ARM64AMode* amCounter,
                                 ARM64AMode* amFailAddr ) {
   ARM64Instr* i                 = LibVEX_Alloc_inline(sizeof(ARM64Instr));
   i->tag                        = ARM64in_EvCheck;
   i->ARM64in.EvCheck.amCounter  = amCounter;
   i->ARM64in.EvCheck.amFailAddr = amFailAddr;
   return i;
}
ARM64Instr* ARM64Instr_ProfInc ( void ) {
   ARM64Instr* i = LibVEX_Alloc_inline(sizeof(ARM64Instr));
   i->tag        = ARM64in_ProfInc;
   return i;
}

/* ... */

void ppARM64Instr ( const ARM64Instr* i ) {
   switch (i->tag) {
      case ARM64in_Arith:
         vex_printf("%s    ", i->ARM64in.Arith.isAdd ? "add" : "sub");
         ppHRegARM64(i->ARM64in.Arith.dst);
         vex_printf(", ");
         ppHRegARM64(i->ARM64in.Arith.argL);
         vex_printf(", ");
         ppARM64RIA(i->ARM64in.Arith.argR);
         return;
      case ARM64in_Cmp:
         vex_printf("cmp%s ", i->ARM64in.Cmp.is64 ? "   " : "(w)" );
         ppHRegARM64(i->ARM64in.Cmp.argL);
         vex_printf(", ");
         ppARM64RIA(i->ARM64in.Cmp.argR);
         return;
      case ARM64in_Logic:
         vex_printf("%s    ", showARM64LogicOp(i->ARM64in.Logic.op));
         ppHRegARM64(i->ARM64in.Logic.dst);
         vex_printf(", ");
         ppHRegARM64(i->ARM64in.Logic.argL);
         vex_printf(", ");
         ppARM64RIL(i->ARM64in.Logic.argR);
         return;
      case ARM64in_RRS:
         vex_printf("%s    ", showARM64RRSOp(i->ARM64in.RRS.mainOp));
         ppHRegARM64(i->ARM64in.RRS.dst);
         vex_printf(", ");
         ppHRegARM64(i->ARM64in.RRS.argL);
         vex_printf(", ");
         ppHRegARM64(i->ARM64in.RRS.argR);
         vex_printf(", %s #%u", showARM64ShiftOp(i->ARM64in.RRS.shiftOp),
                    i->ARM64in.RRS.amt);
         return;
      case ARM64in_Test:
         vex_printf("tst    ");
         ppHRegARM64(i->ARM64in.Test.argL);
         vex_printf(", ");
         ppARM64RIL(i->ARM64in.Test.argR);
         return;
      case ARM64in_Shift:
         vex_printf("%s    ", showARM64ShiftOp(i->ARM64in.Shift.op));
         ppHRegARM64(i->ARM64in.Shift.dst);
         vex_printf(", ");
         ppHRegARM64(i->ARM64in.Shift.argL);
         vex_printf(", ");
         ppARM64RI6(i->ARM64in.Shift.argR);
         return;
      case ARM64in_Unary:
         vex_printf("%s    ", showARM64UnaryOp(i->ARM64in.Unary.op));
         ppHRegARM64(i->ARM64in.Unary.dst);
         vex_printf(", ");
         ppHRegARM64(i->ARM64in.Unary.src);
         return;
      case ARM64in_Set64:
         vex_printf("cset   ");
         ppHRegARM64(i->ARM64in.Set64.dst);
         vex_printf(", %s", showARM64CondCode(i->ARM64in.Set64.cond));
         return;
      case ARM64in_MovI:
         vex_printf("mov    ");
         ppHRegARM64(i->ARM64in.MovI.dst);
         vex_printf(", ");
         ppHRegARM64(i->ARM64in.MovI.src);
         return;
      case ARM64in_Imm64:
         vex_printf("imm64  ");
         ppHRegARM64(i->ARM64in.Imm64.dst);
         vex_printf(", 0x%llx", i->ARM64in.Imm64.imm64);
         return;
      case ARM64in_LdSt64:
         if (i->ARM64in.LdSt64.isLoad) {
            vex_printf("ldr    ");
            ppHRegARM64(i->ARM64in.LdSt64.rD);
            vex_printf(", ");
            ppARM64AMode(i->ARM64in.LdSt64.amode);
         } else {
            vex_printf("str    ");
            ppARM64AMode(i->ARM64in.LdSt64.amode);
            vex_printf(", ");
            ppHRegARM64(i->ARM64in.LdSt64.rD);
         }
         return;
      case ARM64in_LdSt32:
         if (i->ARM64in.LdSt32.isLoad) {
            vex_printf("ldruw  ");
            ppHRegARM64(i->ARM64in.LdSt32.rD);
            vex_printf(", ");
            ppARM64AMode(i->ARM64in.LdSt32.amode);
         } else {
            vex_printf("strw   ");
            ppARM64AMode(i->ARM64in.LdSt32.amode);
            vex_printf(", ");
            ppHRegARM64(i->ARM64in.LdSt32.rD);
         }
         return;
      case ARM64in_LdSt16:
         if (i->ARM64in.LdSt16.isLoad) {
            vex_printf("ldruh  ");
            ppHRegARM64(i->ARM64in.LdSt16.rD);
            vex_printf(", ");
            ppARM64AMode(i->ARM64in.LdSt16.amode);
         } else {
            vex_printf("strh   ");
            ppARM64AMode(i->ARM64in.LdSt16.amode);
            vex_printf(", ");
            ppHRegARM64(i->ARM64in.LdSt16.rD);
         }
         return;
      case ARM64in_LdSt8:
         if (i->ARM64in.LdSt8.isLoad) {
            vex_printf("ldrub  ");
            ppHRegARM64(i->ARM64in.LdSt8.rD);
            vex_printf(", ");
            ppARM64AMode(i->ARM64in.LdSt8.amode);
         } else {
            vex_printf("strb   ");
            ppARM64AMode(i->ARM64in.LdSt8.amode);
            vex_printf(", ");
            ppHRegARM64(i->ARM64in.LdSt8.rD);
         }
         return;
      case ARM64in_XDirect:
         vex_printf("(xDirect) ");
         vex_printf("if (%%pstate.%s) { ",
                    showARM64CondCode(i->ARM64in.XDirect.cond));
         vex_printf("imm64 x9,0x%llx; ", i->ARM64in.XDirect.dstGA);
         vex_printf("str x9,");
         ppARM64AMode(i->ARM64in.XDirect.amPC);
         vex_printf("; imm64-exactly4 x9,$disp_cp_chain_me_to_%sEP; ",
                    i->ARM64in.XDirect.toFastEP ? "fast" : "slow");
         vex_printf("blr x9 }");
         return;
      case ARM64in_XIndir:
         vex_printf("(xIndir) ");
         vex_printf("if (%%pstate.%s) { ",
                    showARM64CondCode(i->ARM64in.XIndir.cond));
         vex_printf("str ");
         ppHRegARM64(i->ARM64in.XIndir.dstGA);
         vex_printf(",");
         ppARM64AMode(i->ARM64in.XIndir.amPC);
         vex_printf("; imm64 x9,$disp_cp_xindir; ");
         vex_printf("br x9 }");
         return;
      case ARM64in_XAssisted:
         vex_printf("(xAssisted) ");
         vex_printf("if (%%pstate.%s) { ",
                    showARM64CondCode(i->ARM64in.XAssisted.cond));
         vex_printf("str ");
         ppHRegARM64(i->ARM64in.XAssisted.dstGA);
         vex_printf(",");
         ppARM64AMode(i->ARM64in.XAssisted.amPC);
         vex_printf("; movw x21,$IRJumpKind_to_TRCVAL(%d); ",
                    (Int)i->ARM64in.XAssisted.jk);
         vex_printf("imm64 x9,$disp_cp_xassisted; ");
         vex_printf("br x9 }");
         return;
      case ARM64in_CSel:
         vex_printf("csel   ");
         ppHRegARM64(i->ARM64in.CSel.dst);
         vex_printf(", ");
         ppHRegARM64(i->ARM64in.CSel.argL);
         vex_printf(", ");
         ppHRegARM64(i->ARM64in.CSel.argR);
         vex_printf(", %s", showARM64CondCode(i->ARM64in.CSel.cond));
         return;
      case ARM64in_Call:
         vex_printf("call%s ",
                    i->ARM64in.Call.cond==ARM64cc_AL
                       ? "  " : showARM64CondCode(i->ARM64in.Call.cond));
         vex_printf("0x%llx [nArgRegs=%d, ",
                    i->ARM64in.Call.target, i->ARM64in.Call.nArgRegs);
         ppRetLoc(i->ARM64in.Call.rloc);
         vex_printf("]");
         return;
      case ARM64in_AddToSP: {
         Int simm = i->ARM64in.AddToSP.simm;
         vex_printf("%s    xsp, xsp, #%d", simm < 0 ? "sub" : "add", 
                                           simm < 0 ? -simm : simm);
         return;
      }
      case ARM64in_FromSP:
         vex_printf("mov    ");
         ppHRegARM64(i->ARM64in.FromSP.dst);
         vex_printf(", xsp");
         return;
      case ARM64in_Mul:
         vex_printf("%s  ", showARM64MulOp(i->ARM64in.Mul.op));
         ppHRegARM64(i->ARM64in.Mul.dst);
         vex_printf(", ");
         ppHRegARM64(i->ARM64in.Mul.argL);
         vex_printf(", ");
         ppHRegARM64(i->ARM64in.Mul.argR);
         return;

      case ARM64in_LdrEX: {
         const HChar* sz = " ";
         switch (i->ARM64in.LdrEX.szB) {
            case 1: sz = "b"; break;
            case 2: sz = "h"; break;
            case 4: case 8: break;
            default: vassert(0);
         }
         vex_printf("ldxr%s  %c2, [x4]",
                    sz, i->ARM64in.LdrEX.szB == 8 ? 'x' : 'w');
         return;
      }
      case ARM64in_StrEX: {
         const HChar* sz = " ";
         switch (i->ARM64in.StrEX.szB) {
            case 1: sz = "b"; break;
            case 2: sz = "h"; break;
            case 4: case 8: break;
            default: vassert(0);
         }
         vex_printf("stxr%s  w0, %c2, [x4]",
                    sz, i->ARM64in.StrEX.szB == 8 ? 'x' : 'w');
         return;
      }
      case ARM64in_LdrEXP:
         vex_printf("ldxp   x2, x3, [x4]");
         return;
      case ARM64in_StrEXP:
         vex_printf("stxp   w0, x2, x3, [x4]");
         return;
      case ARM64in_CAS: {
         vex_printf("x1 = cas(%dbit)(x3, x5 -> x7)", 8 * i->ARM64in.CAS.szB);
         return;
      }
      case ARM64in_CASP: {
         vex_printf("x0,x1 = casp(2x%dbit)(x2, x4,x5 -> x6,x7)",
                    8 * i->ARM64in.CASP.szB);
         return;
      }
      case ARM64in_MFence:
         vex_printf("(mfence) dsb sy; dmb sy; isb");
         return;
      case ARM64in_ClrEX:
         vex_printf("clrex #15");
         return;
      case ARM64in_VLdStH:
         if (i->ARM64in.VLdStH.isLoad) {
            vex_printf("ldr    ");
            ppHRegARM64asHreg(i->ARM64in.VLdStH.hD);
            vex_printf(", %u(", i->ARM64in.VLdStH.uimm12);
            ppHRegARM64(i->ARM64in.VLdStH.rN);
            vex_printf(")");
         } else {
            vex_printf("str    ");
            vex_printf("%u(", i->ARM64in.VLdStH.uimm12);
            ppHRegARM64(i->ARM64in.VLdStH.rN);
            vex_printf("), ");
            ppHRegARM64asHreg(i->ARM64in.VLdStH.hD);
         }
         return;
      case ARM64in_VLdStS:
         if (i->ARM64in.VLdStS.isLoad) {
            vex_printf("ldr    ");
            ppHRegARM64asSreg(i->ARM64in.VLdStS.sD);
            vex_printf(", %u(", i->ARM64in.VLdStS.uimm12);
            ppHRegARM64(i->ARM64in.VLdStS.rN);
            vex_printf(")");
         } else {
            vex_printf("str    ");
            vex_printf("%u(", i->ARM64in.VLdStS.uimm12);
            ppHRegARM64(i->ARM64in.VLdStS.rN);
            vex_printf("), ");
            ppHRegARM64asSreg(i->ARM64in.VLdStS.sD);
         }
         return;
      case ARM64in_VLdStD:
         if (i->ARM64in.VLdStD.isLoad) {
            vex_printf("ldr    ");
            ppHRegARM64(i->ARM64in.VLdStD.dD);
            vex_printf(", %u(", i->ARM64in.VLdStD.uimm12);
            ppHRegARM64(i->ARM64in.VLdStD.rN);
            vex_printf(")");
         } else {
            vex_printf("str    ");
            vex_printf("%u(", i->ARM64in.VLdStD.uimm12);
            ppHRegARM64(i->ARM64in.VLdStD.rN);
            vex_printf("), ");
            ppHRegARM64(i->ARM64in.VLdStD.dD);
         }
         return;
      case ARM64in_VLdStQ:
         if (i->ARM64in.VLdStQ.isLoad)
            vex_printf("ld1.2d {");
         else
            vex_printf("st1.2d {");
         ppHRegARM64(i->ARM64in.VLdStQ.rQ);
         vex_printf("}, [");
         ppHRegARM64(i->ARM64in.VLdStQ.rN);
         vex_printf("]");
         return;
      case ARM64in_VCvtI2F: {
         HChar syn  = '?';
         UInt  fszB = 0;
         UInt  iszB = 0;
         characteriseARM64CvtOp(&syn, &fszB, &iszB, i->ARM64in.VCvtI2F.how);
         vex_printf("%ccvtf  ", syn);
         ppHRegARM64(i->ARM64in.VCvtI2F.rD);
         vex_printf("(%c-reg), ", fszB == 4 ? 'S' : 'D');
         ppHRegARM64(i->ARM64in.VCvtI2F.rS);
         vex_printf("(%c-reg)", iszB == 4 ? 'W' : 'X');
         return;
      }
      case ARM64in_VCvtF2I: {
         HChar syn  = '?';
         UInt  fszB = 0;
         UInt  iszB = 0;
         HChar rmo  = '?';
         characteriseARM64CvtOp(&syn, &fszB, &iszB, i->ARM64in.VCvtF2I.how);
         UChar armRM = i->ARM64in.VCvtF2I.armRM;
         if (armRM < 4) rmo = "npmz"[armRM];
         vex_printf("fcvt%c%c ", rmo, syn);
         ppHRegARM64(i->ARM64in.VCvtF2I.rD);
         vex_printf("(%c-reg), ", iszB == 4 ? 'W' : 'X');
         ppHRegARM64(i->ARM64in.VCvtF2I.rS);
         vex_printf("(%c-reg)", fszB == 4 ? 'S' : 'D');
         return;
      }
      case ARM64in_VCvtSD:
         vex_printf("fcvt%s ", i->ARM64in.VCvtSD.sToD ? "s2d" : "d2s");
         if (i->ARM64in.VCvtSD.sToD) {
            ppHRegARM64(i->ARM64in.VCvtSD.dst);
            vex_printf(", ");
            ppHRegARM64asSreg(i->ARM64in.VCvtSD.src);
         } else {
            ppHRegARM64asSreg(i->ARM64in.VCvtSD.dst);
            vex_printf(", ");
            ppHRegARM64(i->ARM64in.VCvtSD.src);
         }
         return;
      case ARM64in_VCvtHS:
         vex_printf("fcvt%s ", i->ARM64in.VCvtHS.hToS ? "h2s" : "s2h");
         if (i->ARM64in.VCvtHS.hToS) {
            ppHRegARM64asSreg(i->ARM64in.VCvtHS.dst);
            vex_printf(", ");
            ppHRegARM64asHreg(i->ARM64in.VCvtHS.src);
         } else {
            ppHRegARM64asHreg(i->ARM64in.VCvtHS.dst);
            vex_printf(", ");
            ppHRegARM64asSreg(i->ARM64in.VCvtHS.src);
         }
         return;
      case ARM64in_VCvtHD:
         vex_printf("fcvt%s ", i->ARM64in.VCvtHD.hToD ? "h2d" : "d2h");
         if (i->ARM64in.VCvtHD.hToD) {
            ppHRegARM64(i->ARM64in.VCvtHD.dst);
            vex_printf(", ");
            ppHRegARM64asHreg(i->ARM64in.VCvtHD.src);
         } else {
            ppHRegARM64asHreg(i->ARM64in.VCvtHD.dst);
            vex_printf(", ");
            ppHRegARM64(i->ARM64in.VCvtHD.src);
         }
         return;
      case ARM64in_VUnaryD:
         vex_printf("f%s ", showARM64FpUnaryOp(i->ARM64in.VUnaryD.op));
         ppHRegARM64(i->ARM64in.VUnaryD.dst);
         vex_printf(", ");
         ppHRegARM64(i->ARM64in.VUnaryD.src);
         return;
      case ARM64in_VUnaryS:
         vex_printf("f%s ", showARM64FpUnaryOp(i->ARM64in.VUnaryS.op));
         ppHRegARM64asSreg(i->ARM64in.VUnaryS.dst);
         vex_printf(", ");
         ppHRegARM64asSreg(i->ARM64in.VUnaryS.src);
         return;
      case ARM64in_VUnaryH:
         vex_printf("f%s ", showARM64FpUnaryOp(i->ARM64in.VUnaryH.op));
         ppHRegARM64asHreg(i->ARM64in.VUnaryH.dst);
         vex_printf(", ");
         ppHRegARM64asHreg(i->ARM64in.VUnaryH.src);
         return;
      case ARM64in_VBinD:
         vex_printf("f%s   ", showARM64FpBinOp(i->ARM64in.VBinD.op));
         ppHRegARM64(i->ARM64in.VBinD.dst);
         vex_printf(", ");
         ppHRegARM64(i->ARM64in.VBinD.argL);
         vex_printf(", ");
         ppHRegARM64(i->ARM64in.VBinD.argR);
         return;
      case ARM64in_VBinS:
         vex_printf("f%s   ", showARM64FpBinOp(i->ARM64in.VBinS.op));
         ppHRegARM64asSreg(i->ARM64in.VBinS.dst);
         vex_printf(", ");
         ppHRegARM64asSreg(i->ARM64in.VBinS.argL);
         vex_printf(", ");
         ppHRegARM64asSreg(i->ARM64in.VBinS.argR);
         return;
      case ARM64in_VBinH:
         vex_printf("f%s   ", showARM64FpBinOp(i->ARM64in.VBinH.op));
         ppHRegARM64asHreg(i->ARM64in.VBinH.dst);
         vex_printf(", ");
         ppHRegARM64asHreg(i->ARM64in.VBinH.argL);
         vex_printf(", ");
         ppHRegARM64asHreg(i->ARM64in.VBinH.argR);
         return;
      case ARM64in_VTriD:
         vex_printf("f%s   ", showARM64FpTriOp(i->ARM64in.VTriD.op));
         ppHRegARM64(i->ARM64in.VTriD.dst);
         vex_printf(", ");
         ppHRegARM64(i->ARM64in.VTriD.arg1);
         vex_printf(", ");
         ppHRegARM64(i->ARM64in.VTriD.arg2);
         vex_printf(", ");
         ppHRegARM64(i->ARM64in.VTriD.arg3);
         return;
      case ARM64in_VTriS:
         vex_printf("f%s   ", showARM64FpTriOp(i->ARM64in.VTriS.op));
         ppHRegARM64asSreg(i->ARM64in.VTriS.dst);
         vex_printf(", ");
         ppHRegARM64asSreg(i->ARM64in.VTriS.arg1);
         vex_printf(", ");
         ppHRegARM64asSreg(i->ARM64in.VTriS.arg2);
         vex_printf(", ");
         ppHRegARM64asSreg(i->ARM64in.VTriS.arg3);
         return;
      case ARM64in_VCmpD:
         vex_printf("fcmp   ");
         ppHRegARM64(i->ARM64in.VCmpD.argL);
         vex_printf(", ");
         ppHRegARM64(i->ARM64in.VCmpD.argR);
         return;
      case ARM64in_VCmpS:
         vex_printf("fcmp   ");
         ppHRegARM64asSreg(i->ARM64in.VCmpS.argL);
         vex_printf(", ");
         ppHRegARM64asSreg(i->ARM64in.VCmpS.argR);
         return;
      case ARM64in_VCmpH:
         vex_printf("fcmp   ");
         ppHRegARM64asHreg(i->ARM64in.VCmpH.argL);
         vex_printf(", ");
         ppHRegARM64asHreg(i->ARM64in.VCmpH.argR);
         return;
      case ARM64in_VFCSel: {
         UInt (*ppHRegARM64fp)(HReg)
            = (i->ARM64in.VFCSel.isD ? ppHRegARM64 : ppHRegARM64asSreg);
         vex_printf("fcsel  ");
         ppHRegARM64fp(i->ARM64in.VFCSel.dst);
         vex_printf(", ");
         ppHRegARM64fp(i->ARM64in.VFCSel.argL);
         vex_printf(", ");
         ppHRegARM64fp(i->ARM64in.VFCSel.argR);
         vex_printf(", %s", showARM64CondCode(i->ARM64in.VFCSel.cond));
         return;
      }
      case ARM64in_FPCR:
         if (i->ARM64in.FPCR.toFPCR) {
            vex_printf("msr    fpcr, ");
            ppHRegARM64(i->ARM64in.FPCR.iReg);
         } else {
            vex_printf("mrs    ");
            ppHRegARM64(i->ARM64in.FPCR.iReg);
            vex_printf(", fpcr");
         }
         return;
      case ARM64in_FPSR:
         if (i->ARM64in.FPSR.toFPSR) {
            vex_printf("msr    fpsr, ");
            ppHRegARM64(i->ARM64in.FPSR.iReg);
         } else {
            vex_printf("mrs    ");
            ppHRegARM64(i->ARM64in.FPSR.iReg);
            vex_printf(", fpsr");
         }
         return;
      case ARM64in_VBinV: {
         const HChar* nm = "??";
         const HChar* ar = "??";
         showARM64VecBinOp(&nm, &ar, i->ARM64in.VBinV.op);
         vex_printf("%s ", nm);
         ppHRegARM64(i->ARM64in.VBinV.dst);
         vex_printf(".%s, ", ar);
         ppHRegARM64(i->ARM64in.VBinV.argL);
         vex_printf(".%s, ", ar);
         ppHRegARM64(i->ARM64in.VBinV.argR);
         vex_printf(".%s", ar);
         return;
      }
      case ARM64in_VModifyV: {
         const HChar* nm = "??";
         const HChar* ar = "??";
         showARM64VecModifyOp(&nm, &ar, i->ARM64in.VModifyV.op);
         vex_printf("%s ", nm);
         ppHRegARM64(i->ARM64in.VModifyV.mod);
         vex_printf(".%s, ", ar);
         ppHRegARM64(i->ARM64in.VModifyV.arg);
         vex_printf(".%s", ar);
         return;
      }
      case ARM64in_VUnaryV: {
         const HChar* nm = "??";
         const HChar* ar = "??";
         showARM64VecUnaryOp(&nm, &ar, i->ARM64in.VUnaryV.op);
         vex_printf("%s  ", nm);
         ppHRegARM64(i->ARM64in.VUnaryV.dst);
         vex_printf(".%s, ", ar);
         ppHRegARM64(i->ARM64in.VUnaryV.arg);
         vex_printf(".%s", ar);
         return;
      }
      case ARM64in_VNarrowV: {
         UInt dszBlg2 = i->ARM64in.VNarrowV.dszBlg2;
         const HChar* darr[3] = { "8b", "4h", "2s" };
         const HChar* sarr[3] = { "8h", "4s", "2d" };
         const HChar* nm = showARM64VecNarrowOp(i->ARM64in.VNarrowV.op);
         vex_printf("%s ", nm);
         ppHRegARM64(i->ARM64in.VNarrowV.dst);
         vex_printf(".%s, ", dszBlg2 < 3 ? darr[dszBlg2] : "??");
         ppHRegARM64(i->ARM64in.VNarrowV.src);
         vex_printf(".%s", dszBlg2 < 3 ? sarr[dszBlg2] : "??");
         return;
      }
      case ARM64in_VShiftImmV: {
         const HChar* nm = "??";
         const HChar* ar = "??";
         showARM64VecShiftImmOp(&nm, &ar, i->ARM64in.VShiftImmV.op);
         vex_printf("%s ", nm);
         ppHRegARM64(i->ARM64in.VShiftImmV.dst);
         vex_printf(".%s, ", ar);
         ppHRegARM64(i->ARM64in.VShiftImmV.src);
         vex_printf(".%s, #%u", ar, i->ARM64in.VShiftImmV.amt);
         return;
      }
      case ARM64in_VExtV: {
         vex_printf("ext    ");
         ppHRegARM64(i->ARM64in.VExtV.dst);
         vex_printf(".16b, ");
         ppHRegARM64(i->ARM64in.VExtV.srcLo);
         vex_printf(".16b, ");
         ppHRegARM64(i->ARM64in.VExtV.srcHi);
         vex_printf(".16b, #%u", i->ARM64in.VExtV.amtB);
         return;
      }
      case ARM64in_VImmQ:
         vex_printf("qimm   ");
         ppHRegARM64(i->ARM64in.VImmQ.rQ);
         vex_printf(", Bits16toBytes16(0x%x)", (UInt)i->ARM64in.VImmQ.imm);
         return;
      case ARM64in_VDfromX:
         vex_printf("fmov   ");
         ppHRegARM64(i->ARM64in.VDfromX.rD);
         vex_printf(", ");
         ppHRegARM64(i->ARM64in.VDfromX.rX);
         return;
      case ARM64in_VQfromX:
         vex_printf("fmov   ");
         ppHRegARM64(i->ARM64in.VQfromX.rQ);
         vex_printf(".d[0], ");
         ppHRegARM64(i->ARM64in.VQfromX.rXlo);
         return;
      case ARM64in_VQfromXX:
         vex_printf("qFromXX ");
         ppHRegARM64(i->ARM64in.VQfromXX.rQ);
         vex_printf(", ");
         ppHRegARM64(i->ARM64in.VQfromXX.rXhi);
         vex_printf(", ");
         ppHRegARM64(i->ARM64in.VQfromXX.rXlo);
         return;
      case ARM64in_VXfromQ:
         vex_printf("fmov   ");
         ppHRegARM64(i->ARM64in.VXfromQ.rX);
         vex_printf(", ");
         ppHRegARM64(i->ARM64in.VXfromQ.rQ);
         vex_printf(".d[%u]", i->ARM64in.VXfromQ.laneNo);
         return;
      case ARM64in_VXfromDorS:
         vex_printf("fmov   ");
         ppHRegARM64(i->ARM64in.VXfromDorS.rX);
         vex_printf("(%c-reg), ", i->ARM64in.VXfromDorS.fromD ? 'X':'W');
         ppHRegARM64(i->ARM64in.VXfromDorS.rDorS);
         vex_printf("(%c-reg)", i->ARM64in.VXfromDorS.fromD ? 'D' : 'S');
         return;
      case ARM64in_VMov: {
         UChar aux = '?';
         switch (i->ARM64in.VMov.szB) {
            case 16: aux = 'q'; break;
            case 8:  aux = 'd'; break;
            case 4:  aux = 's'; break;
            default: break;
         }
         vex_printf("mov(%c) ", aux);
         ppHRegARM64(i->ARM64in.VMov.dst);
         vex_printf(", ");
         ppHRegARM64(i->ARM64in.VMov.src);
         return;
      }
      case ARM64in_EvCheck:
         vex_printf("(evCheck) ldr w9,");
         ppARM64AMode(i->ARM64in.EvCheck.amCounter);
         vex_printf("; subs w9,w9,$1; str w9,");
         ppARM64AMode(i->ARM64in.EvCheck.amCounter);
         vex_printf("; bpl nofail; ldr x9,");
         ppARM64AMode(i->ARM64in.EvCheck.amFailAddr);
         vex_printf("; br x9; nofail:");
         return;
      case ARM64in_ProfInc:
         vex_printf("(profInc) imm64-fixed4 x9,$NotKnownYet; "
                    "ldr x8,[x9]; add x8,x8,#1, str x8,[x9]");
         return;
      default:
         vex_printf("ppARM64Instr: unhandled case (tag %d)", (Int)i->tag);
         vpanic("ppARM64Instr(1)");
         return;
   }
}


/* --------- Helpers for register allocation. --------- */

void getRegUsage_ARM64Instr ( HRegUsage* u, const ARM64Instr* i, Bool mode64 )
{
   vassert(mode64 == True);
   initHRegUsage(u);
   switch (i->tag) {
      case ARM64in_Arith:
         addHRegUse(u, HRmWrite, i->ARM64in.Arith.dst);
         addHRegUse(u, HRmRead, i->ARM64in.Arith.argL);
         addRegUsage_ARM64RIA(u, i->ARM64in.Arith.argR);
         return;
      case ARM64in_Cmp:
         addHRegUse(u, HRmRead, i->ARM64in.Cmp.argL);
         addRegUsage_ARM64RIA(u, i->ARM64in.Cmp.argR);
         return;
      case ARM64in_Logic:
         addHRegUse(u, HRmWrite, i->ARM64in.Logic.dst);
         addHRegUse(u, HRmRead, i->ARM64in.Logic.argL);
         addRegUsage_ARM64RIL(u, i->ARM64in.Logic.argR);
         return;
      case ARM64in_RRS:
         addHRegUse(u, HRmWrite, i->ARM64in.RRS.dst);
         addHRegUse(u, HRmRead, i->ARM64in.RRS.argL);
         addHRegUse(u, HRmRead, i->ARM64in.RRS.argR);
         return;
      case ARM64in_Test:
         addHRegUse(u, HRmRead, i->ARM64in.Test.argL);
         addRegUsage_ARM64RIL(u, i->ARM64in.Test.argR);
         return;
      case ARM64in_Shift:
         addHRegUse(u, HRmWrite, i->ARM64in.Shift.dst);
         addHRegUse(u, HRmRead, i->ARM64in.Shift.argL);
         addRegUsage_ARM64RI6(u, i->ARM64in.Shift.argR);
         return;
      case ARM64in_Unary:
         addHRegUse(u, HRmWrite, i->ARM64in.Unary.dst);
         addHRegUse(u, HRmRead, i->ARM64in.Unary.src);
         return;
      case ARM64in_Set64:
         addHRegUse(u, HRmWrite, i->ARM64in.Set64.dst);
         return;
      case ARM64in_MovI:
         addHRegUse(u, HRmWrite, i->ARM64in.MovI.dst);
         addHRegUse(u, HRmRead,  i->ARM64in.MovI.src);
         u->isRegRegMove = True;
         u->regMoveSrc   = i->ARM64in.MovI.src;
         u->regMoveDst   = i->ARM64in.MovI.dst;
         return;
      case ARM64in_Imm64:
         addHRegUse(u, HRmWrite, i->ARM64in.Imm64.dst);
         return;
      case ARM64in_LdSt64:
         addRegUsage_ARM64AMode(u, i->ARM64in.LdSt64.amode);
         if (i->ARM64in.LdSt64.isLoad) {
            addHRegUse(u, HRmWrite, i->ARM64in.LdSt64.rD);
         } else {
            addHRegUse(u, HRmRead, i->ARM64in.LdSt64.rD);
         }
         return;
      case ARM64in_LdSt32:
         addRegUsage_ARM64AMode(u, i->ARM64in.LdSt32.amode);
         if (i->ARM64in.LdSt32.isLoad) {
            addHRegUse(u, HRmWrite, i->ARM64in.LdSt32.rD);
         } else {
            addHRegUse(u, HRmRead, i->ARM64in.LdSt32.rD);
         }
         return;
      case ARM64in_LdSt16:
         addRegUsage_ARM64AMode(u, i->ARM64in.LdSt16.amode);
         if (i->ARM64in.LdSt16.isLoad) {
            addHRegUse(u, HRmWrite, i->ARM64in.LdSt16.rD);
         } else {
            addHRegUse(u, HRmRead, i->ARM64in.LdSt16.rD);
         }
         return;
      case ARM64in_LdSt8:
         addRegUsage_ARM64AMode(u, i->ARM64in.LdSt8.amode);
         if (i->ARM64in.LdSt8.isLoad) {
            addHRegUse(u, HRmWrite, i->ARM64in.LdSt8.rD);
         } else {
            addHRegUse(u, HRmRead, i->ARM64in.LdSt8.rD);
         }
         return;
      /* XDirect/XIndir/XAssisted are also a bit subtle.  They
         conditionally exit the block.  Hence we only need to list (1)
         the registers that they read, and (2) the registers that they
         write in the case where the block is not exited.  (2) is
         empty, hence only (1) is relevant here. */
      case ARM64in_XDirect:
         addRegUsage_ARM64AMode(u, i->ARM64in.XDirect.amPC);
         return;
      case ARM64in_XIndir:
         addHRegUse(u, HRmRead, i->ARM64in.XIndir.dstGA);
         addRegUsage_ARM64AMode(u, i->ARM64in.XIndir.amPC);
         return;
      case ARM64in_XAssisted:
         addHRegUse(u, HRmRead, i->ARM64in.XAssisted.dstGA);
         addRegUsage_ARM64AMode(u, i->ARM64in.XAssisted.amPC);
         return;
      case ARM64in_CSel:
         addHRegUse(u, HRmWrite, i->ARM64in.CSel.dst);
         addHRegUse(u, HRmRead,  i->ARM64in.CSel.argL);
         addHRegUse(u, HRmRead,  i->ARM64in.CSel.argR);
         return;
      case ARM64in_Call:
         /* logic and comments copied/modified from x86 back end */
         /* This is a bit subtle. */
         /* First off, claim it trashes all the caller-saved regs
            which fall within the register allocator's jurisdiction.
            These I believe to be x0 to x7 and the 128-bit vector
            registers in use, q16 .. q20. */
         addHRegUse(u, HRmWrite, hregARM64_X0());
         addHRegUse(u, HRmWrite, hregARM64_X1());
         addHRegUse(u, HRmWrite, hregARM64_X2());
         addHRegUse(u, HRmWrite, hregARM64_X3());
         addHRegUse(u, HRmWrite, hregARM64_X4());
         addHRegUse(u, HRmWrite, hregARM64_X5());
         addHRegUse(u, HRmWrite, hregARM64_X6());
         addHRegUse(u, HRmWrite, hregARM64_X7());
         addHRegUse(u, HRmWrite, hregARM64_Q16());
         addHRegUse(u, HRmWrite, hregARM64_Q17());
         addHRegUse(u, HRmWrite, hregARM64_Q18());
         addHRegUse(u, HRmWrite, hregARM64_Q19());
         addHRegUse(u, HRmWrite, hregARM64_Q20());
         /* Now we have to state any parameter-carrying registers
            which might be read.  This depends on nArgRegs. */
            switch (i->ARM64in.Call.nArgRegs) {
            case 8: addHRegUse(u, HRmRead, hregARM64_X7()); /*fallthru*/
            case 7: addHRegUse(u, HRmRead, hregARM64_X6()); /*fallthru*/
            case 6: addHRegUse(u, HRmRead, hregARM64_X5()); /*fallthru*/
            case 5: addHRegUse(u, HRmRead, hregARM64_X4()); /*fallthru*/
            case 4: addHRegUse(u, HRmRead, hregARM64_X3()); /*fallthru*/
            case 3: addHRegUse(u, HRmRead, hregARM64_X2()); /*fallthru*/
            case 2: addHRegUse(u, HRmRead, hregARM64_X1()); /*fallthru*/
            case 1: addHRegUse(u, HRmRead, hregARM64_X0()); break;
            case 0: break;
            default: vpanic("getRegUsage_ARM64:Call:regparms");
         }
         /* Finally, there is the issue that the insn trashes a
            register because the literal target address has to be
            loaded into a register.  However, we reserve x9 for that
            purpose so there's no further complexity here.  Stating x9
            as trashed is pointless since it's not under the control
            of the allocator, but what the hell. */
         addHRegUse(u, HRmWrite, hregARM64_X9());
         return;
      case ARM64in_AddToSP:
         /* Only changes SP, but regalloc doesn't control that, hence
            we don't care. */
         return;
      case ARM64in_FromSP:
         addHRegUse(u, HRmWrite, i->ARM64in.FromSP.dst);
         return;
      case ARM64in_Mul:
         addHRegUse(u, HRmWrite, i->ARM64in.Mul.dst);
         addHRegUse(u, HRmRead,  i->ARM64in.Mul.argL);
         addHRegUse(u, HRmRead,  i->ARM64in.Mul.argR);
         return;
      case ARM64in_LdrEX:
         addHRegUse(u, HRmRead, hregARM64_X4());
         addHRegUse(u, HRmWrite, hregARM64_X2());
         return;
      case ARM64in_StrEX:
         addHRegUse(u, HRmRead, hregARM64_X4());
         addHRegUse(u, HRmWrite, hregARM64_X0());
         addHRegUse(u, HRmRead, hregARM64_X2());
         return;
      case ARM64in_LdrEXP:
         addHRegUse(u, HRmRead, hregARM64_X4());
         addHRegUse(u, HRmWrite, hregARM64_X2());
         addHRegUse(u, HRmWrite, hregARM64_X3());
         return;
      case ARM64in_StrEXP:
         addHRegUse(u, HRmRead, hregARM64_X4());
         addHRegUse(u, HRmWrite, hregARM64_X0());
         addHRegUse(u, HRmRead, hregARM64_X2());
         addHRegUse(u, HRmRead, hregARM64_X3());
         return;
      case ARM64in_CAS:
         addHRegUse(u, HRmRead, hregARM64_X3());
         addHRegUse(u, HRmRead, hregARM64_X5());
         addHRegUse(u, HRmRead, hregARM64_X7());
         addHRegUse(u, HRmWrite, hregARM64_X1());
         /* Pointless to state this since X8 is not available to RA. */
         addHRegUse(u, HRmWrite, hregARM64_X8());
         break;
      case ARM64in_CASP:
         addHRegUse(u, HRmRead, hregARM64_X2());
         addHRegUse(u, HRmRead, hregARM64_X4());
         addHRegUse(u, HRmRead, hregARM64_X5());
         addHRegUse(u, HRmRead, hregARM64_X6());
         addHRegUse(u, HRmRead, hregARM64_X7());
         addHRegUse(u, HRmWrite, hregARM64_X0());
         addHRegUse(u, HRmWrite, hregARM64_X1());
         addHRegUse(u, HRmWrite, hregARM64_X9());
         addHRegUse(u, HRmWrite, hregARM64_X8());
         addHRegUse(u, HRmWrite, hregARM64_X3());
         break;
      case ARM64in_MFence:
         return;
      case ARM64in_ClrEX:
         return;
      case ARM64in_VLdStH:
         addHRegUse(u, HRmRead, i->ARM64in.VLdStH.rN);
         if (i->ARM64in.VLdStH.isLoad) {
            addHRegUse(u, HRmWrite, i->ARM64in.VLdStH.hD);
         } else {
            addHRegUse(u, HRmRead, i->ARM64in.VLdStH.hD);
         }
         return;
      case ARM64in_VLdStS:
         addHRegUse(u, HRmRead, i->ARM64in.VLdStS.rN);
         if (i->ARM64in.VLdStS.isLoad) {
            addHRegUse(u, HRmWrite, i->ARM64in.VLdStS.sD);
         } else {
            addHRegUse(u, HRmRead, i->ARM64in.VLdStS.sD);
         }
         return;
      case ARM64in_VLdStD:
         addHRegUse(u, HRmRead, i->ARM64in.VLdStD.rN);
         if (i->ARM64in.VLdStD.isLoad) {
            addHRegUse(u, HRmWrite, i->ARM64in.VLdStD.dD);
         } else {
            addHRegUse(u, HRmRead, i->ARM64in.VLdStD.dD);
         }
         return;
      case ARM64in_VLdStQ:
         addHRegUse(u, HRmRead, i->ARM64in.VLdStQ.rN);
         if (i->ARM64in.VLdStQ.isLoad)
            addHRegUse(u, HRmWrite, i->ARM64in.VLdStQ.rQ);
         else
            addHRegUse(u, HRmRead, i->ARM64in.VLdStQ.rQ);
         return;
      case ARM64in_VCvtI2F:
         addHRegUse(u, HRmRead, i->ARM64in.VCvtI2F.rS);
         addHRegUse(u, HRmWrite, i->ARM64in.VCvtI2F.rD);
         return;
      case ARM64in_VCvtF2I:
         addHRegUse(u, HRmRead, i->ARM64in.VCvtF2I.rS);
         addHRegUse(u, HRmWrite, i->ARM64in.VCvtF2I.rD);
         return;
      case ARM64in_VCvtSD:
         addHRegUse(u, HRmWrite, i->ARM64in.VCvtSD.dst);
         addHRegUse(u, HRmRead,  i->ARM64in.VCvtSD.src);
         return;
      case ARM64in_VCvtHS:
         addHRegUse(u, HRmWrite, i->ARM64in.VCvtHS.dst);
         addHRegUse(u, HRmRead,  i->ARM64in.VCvtHS.src);
         return;
      case ARM64in_VCvtHD:
         addHRegUse(u, HRmWrite, i->ARM64in.VCvtHD.dst);
         addHRegUse(u, HRmRead,  i->ARM64in.VCvtHD.src);
         return;
      case ARM64in_VUnaryD:
         addHRegUse(u, HRmWrite, i->ARM64in.VUnaryD.dst);
         addHRegUse(u, HRmRead, i->ARM64in.VUnaryD.src);
         return;
      case ARM64in_VUnaryS:
         addHRegUse(u, HRmWrite, i->ARM64in.VUnaryS.dst);
         addHRegUse(u, HRmRead, i->ARM64in.VUnaryS.src);
         return;
      case ARM64in_VUnaryH:
         addHRegUse(u, HRmWrite, i->ARM64in.VUnaryH.dst);
         addHRegUse(u, HRmRead, i->ARM64in.VUnaryH.src);
         return;
      case ARM64in_VBinD:
         addHRegUse(u, HRmWrite, i->ARM64in.VBinD.dst);
         addHRegUse(u, HRmRead, i->ARM64in.VBinD.argL);
         addHRegUse(u, HRmRead, i->ARM64in.VBinD.argR);
         return;
      case ARM64in_VBinS:
         addHRegUse(u, HRmWrite, i->ARM64in.VBinS.dst);
         addHRegUse(u, HRmRead, i->ARM64in.VBinS.argL);
         addHRegUse(u, HRmRead, i->ARM64in.VBinS.argR);
         return;
      case ARM64in_VBinH:
         addHRegUse(u, HRmWrite, i->ARM64in.VBinH.dst);
         addHRegUse(u, HRmRead, i->ARM64in.VBinH.argL);
         addHRegUse(u, HRmRead, i->ARM64in.VBinH.argR);
         return;
      case ARM64in_VTriD:
         addHRegUse(u, HRmWrite, i->ARM64in.VTriD.dst);
         addHRegUse(u, HRmRead, i->ARM64in.VTriD.arg1);
         addHRegUse(u, HRmRead, i->ARM64in.VTriD.arg2);
         addHRegUse(u, HRmRead, i->ARM64in.VTriD.arg3);
         return;
      case ARM64in_VTriS:
         addHRegUse(u, HRmWrite, i->ARM64in.VTriS.dst);
         addHRegUse(u, HRmRead, i->ARM64in.VTriS.arg1);
         addHRegUse(u, HRmRead, i->ARM64in.VTriS.arg2);
         addHRegUse(u, HRmRead, i->ARM64in.VTriS.arg3);
         return;
      case ARM64in_VCmpD:
         addHRegUse(u, HRmRead, i->ARM64in.VCmpD.argL);
         addHRegUse(u, HRmRead, i->ARM64in.VCmpD.argR);
         return;
      case ARM64in_VCmpS:
         addHRegUse(u, HRmRead, i->ARM64in.VCmpS.argL);
         addHRegUse(u, HRmRead, i->ARM64in.VCmpS.argR);
         return;
      case ARM64in_VCmpH:
         addHRegUse(u, HRmRead, i->ARM64in.VCmpH.argL);
         addHRegUse(u, HRmRead, i->ARM64in.VCmpH.argR);
         return;
      case ARM64in_VFCSel:
         addHRegUse(u, HRmRead, i->ARM64in.VFCSel.argL);
         addHRegUse(u, HRmRead, i->ARM64in.VFCSel.argR);
         addHRegUse(u, HRmWrite, i->ARM64in.VFCSel.dst);
         return;
      case ARM64in_FPCR:
         if (i->ARM64in.FPCR.toFPCR)
            addHRegUse(u, HRmRead, i->ARM64in.FPCR.iReg);
         else
            addHRegUse(u, HRmWrite, i->ARM64in.FPCR.iReg);
         return;
      case ARM64in_FPSR:
         if (i->ARM64in.FPSR.toFPSR)
            addHRegUse(u, HRmRead, i->ARM64in.FPSR.iReg);
         else
            addHRegUse(u, HRmWrite, i->ARM64in.FPSR.iReg);
         return;
      case ARM64in_VBinV:
         addHRegUse(u, HRmWrite, i->ARM64in.VBinV.dst);
         addHRegUse(u, HRmRead, i->ARM64in.VBinV.argL);
         addHRegUse(u, HRmRead, i->ARM64in.VBinV.argR);
         return;
      case ARM64in_VModifyV:
         addHRegUse(u, HRmWrite, i->ARM64in.VModifyV.mod);
         addHRegUse(u, HRmRead, i->ARM64in.VModifyV.mod);
         addHRegUse(u, HRmRead, i->ARM64in.VModifyV.arg);
         return;
      case ARM64in_VUnaryV:
         addHRegUse(u, HRmWrite, i->ARM64in.VUnaryV.dst);
         addHRegUse(u, HRmRead, i->ARM64in.VUnaryV.arg);
         return;
      case ARM64in_VNarrowV:
         addHRegUse(u, HRmWrite, i->ARM64in.VNarrowV.dst);
         addHRegUse(u, HRmRead, i->ARM64in.VNarrowV.src);
         return;
      case ARM64in_VShiftImmV:
         addHRegUse(u, HRmWrite, i->ARM64in.VShiftImmV.dst);
         addHRegUse(u, HRmRead, i->ARM64in.VShiftImmV.src);
         return;
      case ARM64in_VExtV:
         addHRegUse(u, HRmWrite, i->ARM64in.VExtV.dst);
         addHRegUse(u, HRmRead, i->ARM64in.VExtV.srcLo);
         addHRegUse(u, HRmRead, i->ARM64in.VExtV.srcHi);
         return;
      case ARM64in_VImmQ:
         addHRegUse(u, HRmWrite, i->ARM64in.VImmQ.rQ);
         return;
      case ARM64in_VDfromX:
         addHRegUse(u, HRmWrite, i->ARM64in.VDfromX.rD);
         addHRegUse(u, HRmRead,  i->ARM64in.VDfromX.rX);
         return;
      case ARM64in_VQfromX:
         addHRegUse(u, HRmWrite, i->ARM64in.VQfromX.rQ);
         addHRegUse(u, HRmRead,  i->ARM64in.VQfromX.rXlo);
         return;
      case ARM64in_VQfromXX:
         addHRegUse(u, HRmWrite, i->ARM64in.VQfromXX.rQ);
         addHRegUse(u, HRmRead,  i->ARM64in.VQfromXX.rXhi);
         addHRegUse(u, HRmRead,  i->ARM64in.VQfromXX.rXlo);
         return;
      case ARM64in_VXfromQ:
         addHRegUse(u, HRmWrite, i->ARM64in.VXfromQ.rX);
         addHRegUse(u, HRmRead,  i->ARM64in.VXfromQ.rQ);
         return;
      case ARM64in_VXfromDorS:
         addHRegUse(u, HRmWrite, i->ARM64in.VXfromDorS.rX);
         addHRegUse(u, HRmRead,  i->ARM64in.VXfromDorS.rDorS);
         return;
      case ARM64in_VMov:
         addHRegUse(u, HRmWrite, i->ARM64in.VMov.dst);
         addHRegUse(u, HRmRead,  i->ARM64in.VMov.src);
         u->isRegRegMove = True;
         u->regMoveSrc   = i->ARM64in.VMov.src;
         u->regMoveDst   = i->ARM64in.VMov.dst;
         return;
      case ARM64in_EvCheck:
         /* We expect both amodes only to mention x21, so this is in
            fact pointless, since x21 isn't allocatable, but
            anyway.. */
         addRegUsage_ARM64AMode(u, i->ARM64in.EvCheck.amCounter);
         addRegUsage_ARM64AMode(u, i->ARM64in.EvCheck.amFailAddr);
         addHRegUse(u, HRmWrite, hregARM64_X9()); /* also unavail to RA */
         return;
      case ARM64in_ProfInc:
         /* Again, pointless to actually state these since neither
            is available to RA. */
         addHRegUse(u, HRmWrite, hregARM64_X9()); /* unavail to RA */
         addHRegUse(u, HRmWrite, hregARM64_X8()); /* unavail to RA */
         return;
      default:
         ppARM64Instr(i);
         vpanic("getRegUsage_ARM64Instr");
   }
}


void mapRegs_ARM64Instr ( HRegRemap* m, ARM64Instr* i, Bool mode64 )
{
   vassert(mode64 == True);
   switch (i->tag) {
      case ARM64in_Arith:
         i->ARM64in.Arith.dst = lookupHRegRemap(m, i->ARM64in.Arith.dst);
         i->ARM64in.Arith.argL = lookupHRegRemap(m, i->ARM64in.Arith.argL);
         mapRegs_ARM64RIA(m, i->ARM64in.Arith.argR);
         return;
      case ARM64in_Cmp:
         i->ARM64in.Cmp.argL = lookupHRegRemap(m, i->ARM64in.Cmp.argL);
         mapRegs_ARM64RIA(m, i->ARM64in.Cmp.argR);
         return;
      case ARM64in_Logic:
         i->ARM64in.Logic.dst = lookupHRegRemap(m, i->ARM64in.Logic.dst);
         i->ARM64in.Logic.argL = lookupHRegRemap(m, i->ARM64in.Logic.argL);
         mapRegs_ARM64RIL(m, i->ARM64in.Logic.argR);
         return;
      case ARM64in_RRS:
         i->ARM64in.RRS.dst = lookupHRegRemap(m, i->ARM64in.RRS.dst);
         i->ARM64in.RRS.argL = lookupHRegRemap(m, i->ARM64in.RRS.argL);
         i->ARM64in.RRS.argR = lookupHRegRemap(m, i->ARM64in.RRS.argR);
         return;
      case ARM64in_Test:
         i->ARM64in.Test.argL = lookupHRegRemap(m, i->ARM64in.Test.argL);
         mapRegs_ARM64RIL(m, i->ARM64in.Logic.argR);
         return;
      case ARM64in_Shift:
         i->ARM64in.Shift.dst = lookupHRegRemap(m, i->ARM64in.Shift.dst);
         i->ARM64in.Shift.argL = lookupHRegRemap(m, i->ARM64in.Shift.argL);
         mapRegs_ARM64RI6(m, i->ARM64in.Shift.argR);
         return;
      case ARM64in_Unary:
         i->ARM64in.Unary.dst = lookupHRegRemap(m, i->ARM64in.Unary.dst);
         i->ARM64in.Unary.src = lookupHRegRemap(m, i->ARM64in.Unary.src);
         return;
      case ARM64in_Set64:
         i->ARM64in.Set64.dst = lookupHRegRemap(m, i->ARM64in.Set64.dst);
         return;
      case ARM64in_MovI:
         i->ARM64in.MovI.dst = lookupHRegRemap(m, i->ARM64in.MovI.dst);
         i->ARM64in.MovI.src = lookupHRegRemap(m, i->ARM64in.MovI.src);
         return;
      case ARM64in_Imm64:
         i->ARM64in.Imm64.dst = lookupHRegRemap(m, i->ARM64in.Imm64.dst);
         return;
      case ARM64in_LdSt64:
         i->ARM64in.LdSt64.rD = lookupHRegRemap(m, i->ARM64in.LdSt64.rD);
         mapRegs_ARM64AMode(m, i->ARM64in.LdSt64.amode);
         return;
      case ARM64in_LdSt32:
         i->ARM64in.LdSt32.rD = lookupHRegRemap(m, i->ARM64in.LdSt32.rD);
         mapRegs_ARM64AMode(m, i->ARM64in.LdSt32.amode);
         return;
      case ARM64in_LdSt16:
         i->ARM64in.LdSt16.rD = lookupHRegRemap(m, i->ARM64in.LdSt16.rD);
         mapRegs_ARM64AMode(m, i->ARM64in.LdSt16.amode);
         return;
      case ARM64in_LdSt8:
         i->ARM64in.LdSt8.rD = lookupHRegRemap(m, i->ARM64in.LdSt8.rD);
         mapRegs_ARM64AMode(m, i->ARM64in.LdSt8.amode);
         return;
      case ARM64in_XDirect:
         mapRegs_ARM64AMode(m, i->ARM64in.XDirect.amPC);
         return;
      case ARM64in_XIndir:
         i->ARM64in.XIndir.dstGA
            = lookupHRegRemap(m, i->ARM64in.XIndir.dstGA);
         mapRegs_ARM64AMode(m, i->ARM64in.XIndir.amPC);
         return;
      case ARM64in_XAssisted:
         i->ARM64in.XAssisted.dstGA
            = lookupHRegRemap(m, i->ARM64in.XAssisted.dstGA);
         mapRegs_ARM64AMode(m, i->ARM64in.XAssisted.amPC);
         return;
      case ARM64in_CSel:
         i->ARM64in.CSel.dst  = lookupHRegRemap(m, i->ARM64in.CSel.dst);
         i->ARM64in.CSel.argL = lookupHRegRemap(m, i->ARM64in.CSel.argL);
         i->ARM64in.CSel.argR = lookupHRegRemap(m, i->ARM64in.CSel.argR);
         return;
      case ARM64in_Call:
         return;
      case ARM64in_AddToSP:
         return;
      case ARM64in_FromSP:
         i->ARM64in.FromSP.dst = lookupHRegRemap(m, i->ARM64in.FromSP.dst);
         return;
      case ARM64in_Mul:
         i->ARM64in.Mul.dst  = lookupHRegRemap(m, i->ARM64in.Mul.dst);
         i->ARM64in.Mul.argL = lookupHRegRemap(m, i->ARM64in.Mul.argL);
         i->ARM64in.Mul.argR = lookupHRegRemap(m, i->ARM64in.Mul.argR);
         break;
      case ARM64in_LdrEX:
         return;
      case ARM64in_StrEX:
         return;
      case ARM64in_LdrEXP:
         return;
      case ARM64in_StrEXP:
         return;
      case ARM64in_CAS:
         return;
      case ARM64in_CASP:
         return;
      case ARM64in_MFence:
         return;
      case ARM64in_ClrEX:
         return;
      case ARM64in_VLdStH:
         i->ARM64in.VLdStH.hD = lookupHRegRemap(m, i->ARM64in.VLdStH.hD);
         i->ARM64in.VLdStH.rN = lookupHRegRemap(m, i->ARM64in.VLdStH.rN);
         return;
      case ARM64in_VLdStS:
         i->ARM64in.VLdStS.sD = lookupHRegRemap(m, i->ARM64in.VLdStS.sD);
         i->ARM64in.VLdStS.rN = lookupHRegRemap(m, i->ARM64in.VLdStS.rN);
         return;
      case ARM64in_VLdStD:
         i->ARM64in.VLdStD.dD = lookupHRegRemap(m, i->ARM64in.VLdStD.dD);
         i->ARM64in.VLdStD.rN = lookupHRegRemap(m, i->ARM64in.VLdStD.rN);
         return;
      case ARM64in_VLdStQ:
         i->ARM64in.VLdStQ.rQ = lookupHRegRemap(m, i->ARM64in.VLdStQ.rQ);
         i->ARM64in.VLdStQ.rN = lookupHRegRemap(m, i->ARM64in.VLdStQ.rN);
         return;
      case ARM64in_VCvtI2F:
         i->ARM64in.VCvtI2F.rS = lookupHRegRemap(m, i->ARM64in.VCvtI2F.rS);
         i->ARM64in.VCvtI2F.rD = lookupHRegRemap(m, i->ARM64in.VCvtI2F.rD);
         return;
      case ARM64in_VCvtF2I:
         i->ARM64in.VCvtF2I.rS = lookupHRegRemap(m, i->ARM64in.VCvtF2I.rS);
         i->ARM64in.VCvtF2I.rD = lookupHRegRemap(m, i->ARM64in.VCvtF2I.rD);
         return;
      case ARM64in_VCvtSD:
         i->ARM64in.VCvtSD.dst = lookupHRegRemap(m, i->ARM64in.VCvtSD.dst);
         i->ARM64in.VCvtSD.src = lookupHRegRemap(m, i->ARM64in.VCvtSD.src);
         return;
      case ARM64in_VCvtHS:
         i->ARM64in.VCvtHS.dst = lookupHRegRemap(m, i->ARM64in.VCvtHS.dst);
         i->ARM64in.VCvtHS.src = lookupHRegRemap(m, i->ARM64in.VCvtHS.src);
         return;
      case ARM64in_VCvtHD:
         i->ARM64in.VCvtHD.dst = lookupHRegRemap(m, i->ARM64in.VCvtHD.dst);
         i->ARM64in.VCvtHD.src = lookupHRegRemap(m, i->ARM64in.VCvtHD.src);
         return;
      case ARM64in_VUnaryD:
         i->ARM64in.VUnaryD.dst = lookupHRegRemap(m, i->ARM64in.VUnaryD.dst);
         i->ARM64in.VUnaryD.src = lookupHRegRemap(m, i->ARM64in.VUnaryD.src);
         return;
      case ARM64in_VUnaryS:
         i->ARM64in.VUnaryS.dst = lookupHRegRemap(m, i->ARM64in.VUnaryS.dst);
         i->ARM64in.VUnaryS.src = lookupHRegRemap(m, i->ARM64in.VUnaryS.src);
         return;
      case ARM64in_VUnaryH:
         i->ARM64in.VUnaryH.dst = lookupHRegRemap(m, i->ARM64in.VUnaryH.dst);
         i->ARM64in.VUnaryH.src = lookupHRegRemap(m, i->ARM64in.VUnaryH.src);
         return;
      case ARM64in_VBinD:
         i->ARM64in.VBinD.dst  = lookupHRegRemap(m, i->ARM64in.VBinD.dst);
         i->ARM64in.VBinD.argL = lookupHRegRemap(m, i->ARM64in.VBinD.argL);
         i->ARM64in.VBinD.argR = lookupHRegRemap(m, i->ARM64in.VBinD.argR);
         return;
      case ARM64in_VBinS:
         i->ARM64in.VBinS.dst  = lookupHRegRemap(m, i->ARM64in.VBinS.dst);
         i->ARM64in.VBinS.argL = lookupHRegRemap(m, i->ARM64in.VBinS.argL);
         i->ARM64in.VBinS.argR = lookupHRegRemap(m, i->ARM64in.VBinS.argR);
         return;
      case ARM64in_VBinH:
         i->ARM64in.VBinH.dst  = lookupHRegRemap(m, i->ARM64in.VBinH.dst);
         i->ARM64in.VBinH.argL = lookupHRegRemap(m, i->ARM64in.VBinH.argL);
         i->ARM64in.VBinH.argR = lookupHRegRemap(m, i->ARM64in.VBinH.argR);
         return;
      case ARM64in_VTriD:
         i->ARM64in.VTriD.dst  = lookupHRegRemap(m, i->ARM64in.VTriD.dst);
         i->ARM64in.VTriD.arg1 = lookupHRegRemap(m, i->ARM64in.VTriD.arg1);
         i->ARM64in.VTriD.arg2 = lookupHRegRemap(m, i->ARM64in.VTriD.arg2);
         i->ARM64in.VTriD.arg3 = lookupHRegRemap(m, i->ARM64in.VTriD.arg3);
         return;
      case ARM64in_VTriS:
         i->ARM64in.VTriS.dst  = lookupHRegRemap(m, i->ARM64in.VTriS.dst);
         i->ARM64in.VTriS.arg1 = lookupHRegRemap(m, i->ARM64in.VTriS.arg1);
         i->ARM64in.VTriS.arg2 = lookupHRegRemap(m, i->ARM64in.VTriS.arg2);
         i->ARM64in.VTriS.arg3 = lookupHRegRemap(m, i->ARM64in.VTriS.arg3);
         return;
      case ARM64in_VCmpD:
         i->ARM64in.VCmpD.argL = lookupHRegRemap(m, i->ARM64in.VCmpD.argL);
         i->ARM64in.VCmpD.argR = lookupHRegRemap(m, i->ARM64in.VCmpD.argR);
         return;
      case ARM64in_VCmpS:
         i->ARM64in.VCmpS.argL = lookupHRegRemap(m, i->ARM64in.VCmpS.argL);
         i->ARM64in.VCmpS.argR = lookupHRegRemap(m, i->ARM64in.VCmpS.argR);
         return;
      case ARM64in_VCmpH:
         i->ARM64in.VCmpH.argL = lookupHRegRemap(m, i->ARM64in.VCmpH.argL);
         i->ARM64in.VCmpH.argR = lookupHRegRemap(m, i->ARM64in.VCmpH.argR);
         return;
      case ARM64in_VFCSel:
         i->ARM64in.VFCSel.argL = lookupHRegRemap(m, i->ARM64in.VFCSel.argL);
         i->ARM64in.VFCSel.argR = lookupHRegRemap(m, i->ARM64in.VFCSel.argR);
         i->ARM64in.VFCSel.dst  = lookupHRegRemap(m, i->ARM64in.VFCSel.dst);
         return;
      case ARM64in_FPCR:
         i->ARM64in.FPCR.iReg = lookupHRegRemap(m, i->ARM64in.FPCR.iReg);
         return;
      case ARM64in_FPSR:
         i->ARM64in.FPSR.iReg = lookupHRegRemap(m, i->ARM64in.FPSR.iReg);
         return;
      case ARM64in_VBinV:
         i->ARM64in.VBinV.dst  = lookupHRegRemap(m, i->ARM64in.VBinV.dst);
         i->ARM64in.VBinV.argL = lookupHRegRemap(m, i->ARM64in.VBinV.argL);
         i->ARM64in.VBinV.argR = lookupHRegRemap(m, i->ARM64in.VBinV.argR);
         return;
      case ARM64in_VModifyV:
         i->ARM64in.VModifyV.mod = lookupHRegRemap(m, i->ARM64in.VModifyV.mod);
         i->ARM64in.VModifyV.arg = lookupHRegRemap(m, i->ARM64in.VModifyV.arg);
         return;
      case ARM64in_VUnaryV:
         i->ARM64in.VUnaryV.dst = lookupHRegRemap(m, i->ARM64in.VUnaryV.dst);
         i->ARM64in.VUnaryV.arg = lookupHRegRemap(m, i->ARM64in.VUnaryV.arg);
         return;
      case ARM64in_VNarrowV:
         i->ARM64in.VNarrowV.dst = lookupHRegRemap(m, i->ARM64in.VNarrowV.dst);
         i->ARM64in.VNarrowV.src = lookupHRegRemap(m, i->ARM64in.VNarrowV.src);
         return;
      case ARM64in_VShiftImmV:
         i->ARM64in.VShiftImmV.dst
            = lookupHRegRemap(m, i->ARM64in.VShiftImmV.dst);
         i->ARM64in.VShiftImmV.src
            = lookupHRegRemap(m, i->ARM64in.VShiftImmV.src);
         return;
      case ARM64in_VExtV:
         i->ARM64in.VExtV.dst = lookupHRegRemap(m, i->ARM64in.VExtV.dst);
         i->ARM64in.VExtV.srcLo = lookupHRegRemap(m, i->ARM64in.VExtV.srcLo);
         i->ARM64in.VExtV.srcHi = lookupHRegRemap(m, i->ARM64in.VExtV.srcHi);
         return;
      case ARM64in_VImmQ:
         i->ARM64in.VImmQ.rQ = lookupHRegRemap(m, i->ARM64in.VImmQ.rQ);
         return;
      case ARM64in_VDfromX:
         i->ARM64in.VDfromX.rD
            = lookupHRegRemap(m, i->ARM64in.VDfromX.rD);
         i->ARM64in.VDfromX.rX
            = lookupHRegRemap(m, i->ARM64in.VDfromX.rX);
         return;
      case ARM64in_VQfromX:
         i->ARM64in.VQfromX.rQ
            = lookupHRegRemap(m, i->ARM64in.VQfromX.rQ);
         i->ARM64in.VQfromX.rXlo
            = lookupHRegRemap(m, i->ARM64in.VQfromX.rXlo);
         return;
      case ARM64in_VQfromXX:
         i->ARM64in.VQfromXX.rQ
            = lookupHRegRemap(m, i->ARM64in.VQfromXX.rQ);
         i->ARM64in.VQfromXX.rXhi
            = lookupHRegRemap(m, i->ARM64in.VQfromXX.rXhi);
         i->ARM64in.VQfromXX.rXlo
            = lookupHRegRemap(m, i->ARM64in.VQfromXX.rXlo);
         return;
      case ARM64in_VXfromQ:
         i->ARM64in.VXfromQ.rX
            = lookupHRegRemap(m, i->ARM64in.VXfromQ.rX);
         i->ARM64in.VXfromQ.rQ
            = lookupHRegRemap(m, i->ARM64in.VXfromQ.rQ);
         return;
      case ARM64in_VXfromDorS:
         i->ARM64in.VXfromDorS.rX
            = lookupHRegRemap(m, i->ARM64in.VXfromDorS.rX);
         i->ARM64in.VXfromDorS.rDorS
            = lookupHRegRemap(m, i->ARM64in.VXfromDorS.rDorS);
         return;
      case ARM64in_VMov:
         i->ARM64in.VMov.dst = lookupHRegRemap(m, i->ARM64in.VMov.dst);
         i->ARM64in.VMov.src = lookupHRegRemap(m, i->ARM64in.VMov.src);
         return;
      case ARM64in_EvCheck:
         /* We expect both amodes only to mention x21, so this is in
            fact pointless, since x21 isn't allocatable, but
            anyway.. */
         mapRegs_ARM64AMode(m, i->ARM64in.EvCheck.amCounter);
         mapRegs_ARM64AMode(m, i->ARM64in.EvCheck.amFailAddr);
         return;
      case ARM64in_ProfInc:
         /* hardwires x8 and x9 -- nothing to modify. */
         return;
      default:
         ppARM64Instr(i);
         vpanic("mapRegs_ARM64Instr");
   }
}

/* Generate arm spill/reload instructions under the direction of the
   register allocator.  Note it's critical these don't write the
   condition codes. */

void genSpill_ARM64 ( /*OUT*/HInstr** i1, /*OUT*/HInstr** i2,
                      HReg rreg, Int offsetB, Bool mode64 )
{
   HRegClass rclass;
   vassert(offsetB >= 0);
   vassert(!hregIsVirtual(rreg));
   vassert(mode64 == True);
   *i1 = *i2 = NULL;
   rclass = hregClass(rreg);
   switch (rclass) {
      case HRcInt64:
         vassert(0 == (offsetB & 7));
         offsetB >>= 3;
         vassert(offsetB < 4096);
         *i1 = ARM64Instr_LdSt64(
                  False/*!isLoad*/, 
                  rreg, 
                  ARM64AMode_RI12(hregARM64_X21(), offsetB, 8)
               );
         return;
      case HRcFlt64:
         vassert(0 == (offsetB & 7));
         vassert(offsetB >= 0 && offsetB < 32768);
         *i1 = ARM64Instr_VLdStD(False/*!isLoad*/,
                                 rreg, hregARM64_X21(), offsetB);
         return;
      case HRcVec128: {
         HReg x21  = hregARM64_X21();  // baseblock
         HReg x9   = hregARM64_X9();   // spill temporary
         vassert(0 == (offsetB & 15)); // check sane alignment
         vassert(offsetB < 4096);
         *i1 = ARM64Instr_Arith(x9, x21, ARM64RIA_I12(offsetB, 0), True);
         *i2 = ARM64Instr_VLdStQ(False/*!isLoad*/, rreg, x9);
         return;
      }
      default:
         ppHRegClass(rclass);
         vpanic("genSpill_ARM: unimplemented regclass");
   }
}

void genReload_ARM64 ( /*OUT*/HInstr** i1, /*OUT*/HInstr** i2,
                       HReg rreg, Int offsetB, Bool mode64 )
{
   HRegClass rclass;
   vassert(offsetB >= 0);
   vassert(!hregIsVirtual(rreg));
   vassert(mode64 == True);
   *i1 = *i2 = NULL;
   rclass = hregClass(rreg);
   switch (rclass) {
      case HRcInt64:
         vassert(0 == (offsetB & 7));
         offsetB >>= 3;
         vassert(offsetB < 4096);
         *i1 = ARM64Instr_LdSt64(
                  True/*isLoad*/, 
                  rreg, 
                  ARM64AMode_RI12(hregARM64_X21(), offsetB, 8)
               );
         return;
      case HRcFlt64:
         vassert(0 == (offsetB & 7));
         vassert(offsetB >= 0 && offsetB < 32768);
         *i1 = ARM64Instr_VLdStD(True/*isLoad*/,
                                 rreg, hregARM64_X21(), offsetB);
         return;
      case HRcVec128: {
         HReg x21  = hregARM64_X21();  // baseblock
         HReg x9   = hregARM64_X9();   // spill temporary
         vassert(0 == (offsetB & 15)); // check sane alignment
         vassert(offsetB < 4096);
         *i1 = ARM64Instr_Arith(x9, x21, ARM64RIA_I12(offsetB, 0), True);
         *i2 = ARM64Instr_VLdStQ(True/*isLoad*/, rreg, x9);
         return;
      }
      default:
         ppHRegClass(rclass);
         vpanic("genReload_ARM: unimplemented regclass");
   }
}

ARM64Instr* genMove_ARM64(HReg from, HReg to, Bool mode64)
{
   switch (hregClass(from)) {
   case HRcInt64:
      return ARM64Instr_MovI(to, from);
   case HRcFlt64:
      return ARM64Instr_VMov(8, to, from);
   case HRcVec128:
      return ARM64Instr_VMov(16, to, from);
   default:
      ppHRegClass(hregClass(from));
      vpanic("genMove_ARM64: unimplemented regclass");
   }
}


/* Emit an instruction into buf and return the number of bytes used.
   Note that buf is not the insn's final place, and therefore it is
   imperative to emit position-independent code. */

static inline UInt iregEnc ( HReg r )
{
   UInt n;
   vassert(hregClass(r) == HRcInt64);
   vassert(!hregIsVirtual(r));
   n = hregEncoding(r);
   vassert(n <= 30);
   return n;
}

static inline UInt iregEncOr31 ( HReg r )
{
   // This is the same as iregEnc() except that we're allowed to use the
   // "special" encoding number 31, which means, depending on the context,
   // either XZR/WZR or SP.
   UInt n;
   vassert(hregClass(r) == HRcInt64);
   vassert(!hregIsVirtual(r));
   n = hregEncoding(r);
   vassert(n <= 31);
   return n;
}

static inline UInt dregEnc ( HReg r )
{
   UInt n;
   vassert(hregClass(r) == HRcFlt64);
   vassert(!hregIsVirtual(r));
   n = hregEncoding(r);
   vassert(n <= 31);
   return n;
}

static inline UInt qregEnc ( HReg r )
{
   UInt n;
   vassert(hregClass(r) == HRcVec128);
   vassert(!hregIsVirtual(r));
   n = hregEncoding(r);
   vassert(n <= 31);
   return n;
}

#define BITS4(zzb3,zzb2,zzb1,zzb0) \
   (((zzb3) << 3) | ((zzb2) << 2) | ((zzb1) << 1) | (zzb0))

#define X00  BITS4(0,0, 0,0)
#define X01  BITS4(0,0, 0,1)
#define X10  BITS4(0,0, 1,0)
#define X11  BITS4(0,0, 1,1)

#define X000 BITS4(0, 0,0,0)
#define X001 BITS4(0, 0,0,1)
#define X010 BITS4(0, 0,1,0)
#define X011 BITS4(0, 0,1,1)
#define X100 BITS4(0, 1,0,0)
#define X101 BITS4(0, 1,0,1)
#define X110 BITS4(0, 1,1,0)
#define X111 BITS4(0, 1,1,1)

#define X0000 BITS4(0,0,0,0)
#define X0001 BITS4(0,0,0,1)
#define X0010 BITS4(0,0,1,0)
#define X0011 BITS4(0,0,1,1)

#define BITS8(zzb7,zzb6,zzb5,zzb4,zzb3,zzb2,zzb1,zzb0) \
  ((BITS4(zzb7,zzb6,zzb5,zzb4) << 4) | BITS4(zzb3,zzb2,zzb1,zzb0))

#define X00000   BITS8(0,0,0, 0,0,0,0,0)
#define X00001   BITS8(0,0,0, 0,0,0,0,1)
#define X00100   BITS8(0,0,0, 0,0,1,0,0)
#define X00110   BITS8(0,0,0, 0,0,1,1,0)
#define X00111   BITS8(0,0,0, 0,0,1,1,1)
#define X01000   BITS8(0,0,0, 0,1,0,0,0)
#define X10000   BITS8(0,0,0, 1,0,0,0,0)
#define X11000   BITS8(0,0,0, 1,1,0,0,0)
#define X11001   BITS8(0,0,0, 1,1,0,0,1)
#define X11110   BITS8(0,0,0, 1,1,1,1,0)
#define X11111   BITS8(0,0,0, 1,1,1,1,1)

#define X000000  BITS8(0,0, 0,0,0,0,0,0)
#define X000001  BITS8(0,0, 0,0,0,0,0,1)
#define X000010  BITS8(0,0, 0,0,0,0,1,0)
#define X000011  BITS8(0,0, 0,0,0,0,1,1)
#define X000100  BITS8(0,0, 0,0,0,1,0,0)
#define X000101  BITS8(0,0, 0,0,0,1,0,1)
#define X000110  BITS8(0,0, 0,0,0,1,1,0)
#define X000111  BITS8(0,0, 0,0,0,1,1,1)
#define X001000  BITS8(0,0, 0,0,1,0,0,0)
#define X001001  BITS8(0,0, 0,0,1,0,0,1)
#define X001010  BITS8(0,0, 0,0,1,0,1,0)
#define X001011  BITS8(0,0, 0,0,1,0,1,1)
#define X001101  BITS8(0,0, 0,0,1,1,0,1)
#define X001110  BITS8(0,0, 0,0,1,1,1,0)
#define X001111  BITS8(0,0, 0,0,1,1,1,1)
#define X010000  BITS8(0,0, 0,1,0,0,0,0)
#define X010001  BITS8(0,0, 0,1,0,0,0,1)
#define X010010  BITS8(0,0, 0,1,0,0,1,0)
#define X010011  BITS8(0,0, 0,1,0,0,1,1)
#define X010101  BITS8(0,0, 0,1,0,1,0,1)
#define X010110  BITS8(0,0, 0,1,0,1,1,0)
#define X010111  BITS8(0,0, 0,1,0,1,1,1)
#define X011001  BITS8(0,0, 0,1,1,0,0,1)
#define X011010  BITS8(0,0, 0,1,1,0,1,0)
#define X011011  BITS8(0,0, 0,1,1,0,1,1)
#define X011101  BITS8(0,0, 0,1,1,1,0,1)
#define X011110  BITS8(0,0, 0,1,1,1,1,0)
#define X011111  BITS8(0,0, 0,1,1,1,1,1)
#define X100001  BITS8(0,0, 1,0,0,0,0,1)
#define X100011  BITS8(0,0, 1,0,0,0,1,1)
#define X100100  BITS8(0,0, 1,0,0,1,0,0)
#define X100101  BITS8(0,0, 1,0,0,1,0,1)
#define X100110  BITS8(0,0, 1,0,0,1,1,0)
#define X100111  BITS8(0,0, 1,0,0,1,1,1)
#define X101101  BITS8(0,0, 1,0,1,1,0,1)
#define X101110  BITS8(0,0, 1,0,1,1,1,0)
#define X110000  BITS8(0,0, 1,1,0,0,0,0)
#define X110001  BITS8(0,0, 1,1,0,0,0,1)
#define X110010  BITS8(0,0, 1,1,0,0,1,0)
#define X110100  BITS8(0,0, 1,1,0,1,0,0)
#define X110101  BITS8(0,0, 1,1,0,1,0,1)
#define X110110  BITS8(0,0, 1,1,0,1,1,0)
#define X110111  BITS8(0,0, 1,1,0,1,1,1)
#define X111000  BITS8(0,0, 1,1,1,0,0,0)
#define X111001  BITS8(0,0, 1,1,1,0,0,1)
#define X111101  BITS8(0,0, 1,1,1,1,0,1)
#define X111110  BITS8(0,0, 1,1,1,1,1,0)
#define X111111  BITS8(0,0, 1,1,1,1,1,1)

#define X0001000  BITS8(0, 0,0,0,1,0,0,0)
#define X0010000  BITS8(0, 0,0,1,0,0,0,0)
#define X0100000  BITS8(0, 0,1,0,0,0,0,0)
#define X1000000  BITS8(0, 1,0,0,0,0,0,0)

#define X00100000  BITS8(0,0,1,0,0,0,0,0)
#define X00100001  BITS8(0,0,1,0,0,0,0,1)
#define X00100010  BITS8(0,0,1,0,0,0,1,0)
#define X00100011  BITS8(0,0,1,0,0,0,1,1)
#define X01010000  BITS8(0,1,0,1,0,0,0,0)
#define X01010001  BITS8(0,1,0,1,0,0,0,1)
#define X01010100  BITS8(0,1,0,1,0,1,0,0)
#define X01011000  BITS8(0,1,0,1,1,0,0,0)
#define X01100000  BITS8(0,1,1,0,0,0,0,0)
#define X01100001  BITS8(0,1,1,0,0,0,0,1)
#define X01100010  BITS8(0,1,1,0,0,0,1,0)
#define X01100011  BITS8(0,1,1,0,0,0,1,1)
#define X01110000  BITS8(0,1,1,1,0,0,0,0)
#define X01110001  BITS8(0,1,1,1,0,0,0,1)
#define X01110010  BITS8(0,1,1,1,0,0,1,0)
#define X01110011  BITS8(0,1,1,1,0,0,1,1)
#define X01110100  BITS8(0,1,1,1,0,1,0,0)
#define X01110101  BITS8(0,1,1,1,0,1,0,1)
#define X01110110  BITS8(0,1,1,1,0,1,1,0)
#define X01110111  BITS8(0,1,1,1,0,1,1,1)
#define X10001010  BITS8(1,0,0,0,1,0,1,0)
#define X10001011  BITS8(1,0,0,0,1,0,1,1)
#define X10101010  BITS8(1,0,1,0,1,0,1,0)
#define X11000001  BITS8(1,1,0,0,0,0,0,1)
#define X11000011  BITS8(1,1,0,0,0,0,1,1)
#define X11001010  BITS8(1,1,0,0,1,0,1,0)
#define X11001011  BITS8(1,1,0,0,1,0,1,1)
#define X11010100  BITS8(1,1,0,1,0,1,0,0)
#define X11010110  BITS8(1,1,0,1,0,1,1,0)
#define X11011000  BITS8(1,1,0,1,1,0,0,0)
#define X11011010  BITS8(1,1,0,1,1,0,1,0)
#define X11011110  BITS8(1,1,0,1,1,1,1,0)
#define X11100010  BITS8(1,1,1,0,0,0,1,0)
#define X11110001  BITS8(1,1,1,1,0,0,0,1)
#define X11110010  BITS8(1,1,1,1,0,0,1,0)
#define X11110011  BITS8(1,1,1,1,0,0,1,1)
#define X11110101  BITS8(1,1,1,1,0,1,0,1)
#define X11110111  BITS8(1,1,1,1,0,1,1,1)
#define X11111000  BITS8(1,1,1,1,1,0,0,0)
#define X11111010  BITS8(1,1,1,1,1,0,1,0)

/* --- 4 fields --- */

static inline UInt X_8_19_1_4 ( UInt f1, UInt f2, UInt f3, UInt f4 ) {
   vassert(8+19+1+4 == 32);
   vassert(f1 < (1<<8));
   vassert(f2 < (1<<19));
   vassert(f3 < (1<<1));
   vassert(f4 < (1<<4));
   UInt w = 0;
   w = (w <<  8) | f1;
   w = (w << 19) | f2;
   w = (w <<  1) | f3;
   w = (w <<  4) | f4;
   return w;
}

/* --- 5 fields --- */

static inline UInt X_3_6_2_16_5 ( UInt f1, UInt f2,
                                  UInt f3, UInt f4, UInt f5 ) {
   vassert(3+6+2+16+5 == 32);
   vassert(f1 < (1<<3));
   vassert(f2 < (1<<6));
   vassert(f3 < (1<<2));
   vassert(f4 < (1<<16));
   vassert(f5 < (1<<5));
   UInt w = 0;
   w = (w <<  3) | f1;
   w = (w <<  6) | f2;
   w = (w <<  2) | f3;
   w = (w << 16) | f4;
   w = (w <<  5) | f5;
   return w;
}

/* --- 6 fields --- */

static inline UInt X_2_6_2_12_5_5 ( UInt f1, UInt f2, UInt f3,
                                    UInt f4, UInt f5, UInt f6 ) {
   vassert(2+6+2+12+5+5 == 32);
   vassert(f1 < (1<<2));
   vassert(f2 < (1<<6));
   vassert(f3 < (1<<2));
   vassert(f4 < (1<<12));
   vassert(f5 < (1<<5));
   vassert(f6 < (1<<5));
   UInt w = 0;
   w = (w <<  2) | f1;
   w = (w <<  6) | f2;
   w = (w <<  2) | f3;
   w = (w << 12) | f4;
   w = (w <<  5) | f5;
   w = (w <<  5) | f6;
   return w;
}

static inline UInt X_3_8_5_6_5_5 ( UInt f1, UInt f2, UInt f3,
                                   UInt f4, UInt f5, UInt f6 ) {
   vassert(3+8+5+6+5+5 == 32);
   vassert(f1 < (1<<3));
   vassert(f2 < (1<<8));
   vassert(f3 < (1<<5));
   vassert(f4 < (1<<6));
   vassert(f5 < (1<<5));
   vassert(f6 < (1<<5));
   UInt w = 0;
   w = (w <<  3) | f1;
   w = (w <<  8) | f2;
   w = (w <<  5) | f3;
   w = (w <<  6) | f4;
   w = (w <<  5) | f5;
   w = (w <<  5) | f6;
   return w;
}

static inline UInt X_3_5_8_6_5_5 ( UInt f1, UInt f2, UInt f3,
                                   UInt f4, UInt f5, UInt f6 ) {
   vassert(3+8+5+6+5+5 == 32);
   vassert(f1 < (1<<3));
   vassert(f2 < (1<<5));
   vassert(f3 < (1<<8));
   vassert(f4 < (1<<6));
   vassert(f5 < (1<<5));
   vassert(f6 < (1<<5));
   UInt w = 0;
   w = (w <<  3) | f1;
   w = (w <<  5) | f2;
   w = (w <<  8) | f3;
   w = (w <<  6) | f4;
   w = (w <<  5) | f5;
   w = (w <<  5) | f6;
   return w;
}

static inline UInt X_3_6_7_6_5_5 ( UInt f1, UInt f2, UInt f3,
                                   UInt f4, UInt f5, UInt f6 ) {
   vassert(3+6+7+6+5+5 == 32);
   vassert(f1 < (1<<3));
   vassert(f2 < (1<<6));
   vassert(f3 < (1<<7));
   vassert(f4 < (1<<6));
   vassert(f5 < (1<<5));
   vassert(f6 < (1<<5));
   UInt w = 0;
   w = (w <<  3) | f1;
   w = (w <<  6) | f2;
   w = (w <<  7) | f3;
   w = (w <<  6) | f4;
   w = (w <<  5) | f5;
   w = (w <<  5) | f6;
   return w;
}

/* --- 7 fields --- */

static inline UInt X_2_6_3_9_2_5_5 ( UInt f1, UInt f2, UInt f3,
                                     UInt f4, UInt f5, UInt f6, UInt f7 ) {
   vassert(2+6+3+9+2+5+5 == 32);
   vassert(f1 < (1<<2));
   vassert(f2 < (1<<6));
   vassert(f3 < (1<<3));
   vassert(f4 < (1<<9));
   vassert(f5 < (1<<2));
   vassert(f6 < (1<<5));
   vassert(f7 < (1<<5));
   UInt w = 0;
   w = (w << 2) | f1;
   w = (w << 6) | f2;
   w = (w << 3) | f3;
   w = (w << 9) | f4;
   w = (w << 2) | f5;
   w = (w << 5) | f6;
   w = (w << 5) | f7;
   return w;
}

static inline UInt X_3_6_1_6_6_5_5 ( UInt f1, UInt f2, UInt f3,
                                     UInt f4, UInt f5, UInt f6, UInt f7 ) {
   vassert(3+6+1+6+6+5+5 == 32);
   vassert(f1 < (1<<3));
   vassert(f2 < (1<<6));
   vassert(f3 < (1<<1));
   vassert(f4 < (1<<6));
   vassert(f5 < (1<<6));
   vassert(f6 < (1<<5));
   vassert(f7 < (1<<5));
   UInt w = 0;
   w = (w << 3) | f1;
   w = (w << 6) | f2;
   w = (w << 1) | f3;
   w = (w << 6) | f4;
   w = (w << 6) | f5;
   w = (w << 5) | f6;
   w = (w << 5) | f7;
   return w;
}

static inline UInt X_3_8_5_1_5_5_5 ( UInt f1, UInt f2, UInt f3, UInt f4,
                                     UInt f5, UInt f6, UInt f7 ) {
   vassert(3+8+5+1+5+5+5 == 32);
   vassert(f1 < (1<<3));
   vassert(f2 < (1<<8));
   vassert(f3 < (1<<5));
   vassert(f4 < (1<<1));
   vassert(f5 < (1<<5));
   vassert(f6 < (1<<5));
   vassert(f7 < (1<<5));
   UInt w = 0;
   w = (w << 3) | f1;
   w = (w << 8) | f2;
   w = (w << 5) | f3;
   w = (w << 1) | f4;
   w = (w << 5) | f5;
   w = (w << 5) | f6;
   w = (w << 5) | f7;
   return w;
}

static inline UInt X_8_2_1_5_6_5_5 ( UInt f1, UInt f2, UInt f3, UInt f4,
                                     UInt f5, UInt f6, UInt f7 ) {
   vassert(8+2+1+5+6+5+5 == 32);
   vassert(f1 < (1<<8));
   vassert(f2 < (1<<2));
   vassert(f3 < (1<<1));
   vassert(f4 < (1<<5));
   vassert(f5 < (1<<6));
   vassert(f6 < (1<<5));
   vassert(f7 < (1<<5));
   UInt w = 0;
   w = (w << 8) | f1;
   w = (w << 2) | f2;
   w = (w << 1) | f3;
   w = (w << 5) | f4;
   w = (w << 6) | f5;
   w = (w << 5) | f6;
   w = (w << 5) | f7;
   return w;
}

//ZZ #define X0000  BITS4(0,0,0,0)
//ZZ #define X0001  BITS4(0,0,0,1)
//ZZ #define X0010  BITS4(0,0,1,0)
//ZZ #define X0011  BITS4(0,0,1,1)
//ZZ #define X0100  BITS4(0,1,0,0)
//ZZ #define X0101  BITS4(0,1,0,1)
//ZZ #define X0110  BITS4(0,1,1,0)
//ZZ #define X0111  BITS4(0,1,1,1)
//ZZ #define X1000  BITS4(1,0,0,0)
//ZZ #define X1001  BITS4(1,0,0,1)
//ZZ #define X1010  BITS4(1,0,1,0)
//ZZ #define X1011  BITS4(1,0,1,1)
//ZZ #define X1100  BITS4(1,1,0,0)
//ZZ #define X1101  BITS4(1,1,0,1)
//ZZ #define X1110  BITS4(1,1,1,0)
//ZZ #define X1111  BITS4(1,1,1,1)
/*
#define XXXXX___(zzx7,zzx6,zzx5,zzx4,zzx3) \
   ((((zzx7) & 0xF) << 28) | (((zzx6) & 0xF) << 24) |  \
    (((zzx5) & 0xF) << 20) | (((zzx4) & 0xF) << 16) |  \
    (((zzx3) & 0xF) << 12))

#define XXXXXX__(zzx7,zzx6,zzx5,zzx4,zzx3,zzx2)        \
   ((((zzx7) & 0xF) << 28) | (((zzx6) & 0xF) << 24) |  \
    (((zzx5) & 0xF) << 20) | (((zzx4) & 0xF) << 16) |  \
    (((zzx3) & 0xF) << 12) | (((zzx2) & 0xF) <<  8))

#define XXXXX__X(zzx7,zzx6,zzx5,zzx4,zzx3,zzx0)        \
   ((((zzx7) & 0xF) << 28) | (((zzx6) & 0xF) << 24) |  \
    (((zzx5) & 0xF) << 20) | (((zzx4) & 0xF) << 16) |  \
    (((zzx3) & 0xF) << 12) | (((zzx0) & 0xF) <<  0))

#define XXX___XX(zzx7,zzx6,zzx5,zzx1,zzx0) \
  ((((zzx7) & 0xF) << 28) | (((zzx6) & 0xF) << 24) | \
   (((zzx5) & 0xF) << 20) | (((zzx1) & 0xF) << 4) | \
   (((zzx0) & 0xF) << 0))

#define XXXXXXXX(zzx7,zzx6,zzx5,zzx4,zzx3,zzx2,zzx1,zzx0)  \
   ((((zzx7) & 0xF) << 28) | (((zzx6) & 0xF) << 24) |  \
    (((zzx5) & 0xF) << 20) | (((zzx4) & 0xF) << 16) |  \
    (((zzx3) & 0xF) << 12) | (((zzx2) & 0xF) <<  8) |  \
    (((zzx1) & 0xF) <<  4) | (((zzx0) & 0xF) <<  0))

#define XX______(zzx7,zzx6) \
   ((((zzx7) & 0xF) << 28) | (((zzx6) & 0xF) << 24))
*/


/* Get an immediate into a register, using only that register. */
static UInt* imm64_to_ireg ( UInt* p, Int xD, ULong imm64 )
{
   if (imm64 == 0) {
      // This has to be special-cased, since the logic below
      // will leave the register unchanged in this case.
      // MOVZ xD, #0, LSL #0
      *p++ = X_3_6_2_16_5(X110, X100101, X00, 0/*imm16*/, xD);
      return p;
   }

   // There must be at least one non-zero halfword.  Find the
   // lowest nonzero such, and use MOVZ to install it and zero
   // out the rest of the register.
   UShort h[4];
   h[3] = (UShort)((imm64 >> 48) & 0xFFFF);
   h[2] = (UShort)((imm64 >> 32) & 0xFFFF);
   h[1] = (UShort)((imm64 >> 16) & 0xFFFF);
   h[0] = (UShort)((imm64 >>  0) & 0xFFFF);

   UInt i;
   for (i = 0; i < 4; i++) {
      if (h[i] != 0)
         break;
   }
   vassert(i < 4);

   // MOVZ xD, h[i], LSL (16*i)
   *p++ = X_3_6_2_16_5(X110, X100101, i, h[i], xD);

   // Work on upwards through h[i], using MOVK to stuff in any
   // remaining nonzero elements.
   i++;
   for (; i < 4; i++) {
      if (h[i] == 0)
         continue;
      // MOVK xD, h[i], LSL (16*i)
      *p++ = X_3_6_2_16_5(X111, X100101, i, h[i], xD);
   }

   return p;
}

/* Get an immediate into a register, using only that register, and
   generating exactly 4 instructions, regardless of the value of the
   immediate. This is used when generating sections of code that need
   to be patched later, so as to guarantee a specific size. */
static UInt* imm64_to_ireg_EXACTLY4 ( UInt* p, Int xD, ULong imm64 )
{
   UShort h[4];
   h[3] = (UShort)((imm64 >> 48) & 0xFFFF);
   h[2] = (UShort)((imm64 >> 32) & 0xFFFF);
   h[1] = (UShort)((imm64 >> 16) & 0xFFFF);
   h[0] = (UShort)((imm64 >>  0) & 0xFFFF);
   // Work on upwards through h[i], using MOVK to stuff in the
   // remaining elements.
   UInt i;
   for (i = 0; i < 4; i++) {
      if (i == 0) {
         // MOVZ xD, h[0], LSL (16*0)
         *p++ = X_3_6_2_16_5(X110, X100101, i, h[i], xD);
      } else {
         // MOVK xD, h[i], LSL (16*i)
         *p++ = X_3_6_2_16_5(X111, X100101, i, h[i], xD);
      }
   }
   return p;
}

/* Check whether p points at a 4-insn sequence cooked up by
   imm64_to_ireg_EXACTLY4(). */
static Bool is_imm64_to_ireg_EXACTLY4 ( UInt* p, Int xD, ULong imm64 )
{
   UShort h[4];
   h[3] = (UShort)((imm64 >> 48) & 0xFFFF);
   h[2] = (UShort)((imm64 >> 32) & 0xFFFF);
   h[1] = (UShort)((imm64 >> 16) & 0xFFFF);
   h[0] = (UShort)((imm64 >>  0) & 0xFFFF);
   // Work on upwards through h[i], using MOVK to stuff in the
   // remaining elements.
   UInt i;
   for (i = 0; i < 4; i++) {
      UInt expected;
      if (i == 0) {
         // MOVZ xD, h[0], LSL (16*0)
         expected = X_3_6_2_16_5(X110, X100101, i, h[i], xD);
      } else {
         // MOVK xD, h[i], LSL (16*i)
         expected = X_3_6_2_16_5(X111, X100101, i, h[i], xD);
      }
      if (p[i] != expected)
         return False;
   }
   return True;
}


/* Generate a 8 bit store or 8-to-64 unsigned widening load from/to
   rD, using the given amode for the address. */
static UInt* do_load_or_store8 ( UInt* p,
                                 Bool isLoad, UInt wD, ARM64AMode* am )
{
   vassert(wD <= 30);
   if (am->tag == ARM64am_RI9) {
      /* STURB Wd, [Xn|SP + simm9]:  00 111000 000 simm9 00 n d
         LDURB Wd, [Xn|SP + simm9]:  00 111000 010 simm9 00 n d
      */
      Int simm9 = am->ARM64am.RI9.simm9;
      vassert(-256 <= simm9 && simm9 <= 255);
      UInt instr = X_2_6_3_9_2_5_5(X00, X111000, isLoad ? X010 : X000,
                                   simm9 & 0x1FF, X00,
                                   iregEnc(am->ARM64am.RI9.reg), wD);
      *p++ = instr;
      return p;
   }
   if (am->tag == ARM64am_RI12) {
      /* STRB Wd, [Xn|SP + uimm12 * 1]:  00 111 001 00 imm12 n d
         LDRB Wd, [Xn|SP + uimm12 * 1]:  00 111 001 01 imm12 n d
      */
      UInt uimm12 = am->ARM64am.RI12.uimm12;
      UInt scale  = am->ARM64am.RI12.szB;
      vassert(scale == 1); /* failure of this is serious.  Do not ignore. */
      UInt xN    = iregEnc(am->ARM64am.RI12.reg);
      vassert(xN <= 30);
      UInt instr = X_2_6_2_12_5_5(X00, X111001, isLoad ? X01 : X00,
                                  uimm12, xN, wD);
      *p++ = instr;
      return p;
   }
   if (am->tag == ARM64am_RR) {
      /* STRB Xd, [Xn|SP, Xm]: 00 111 000 001 m 011 0 10 n d
         LDRB Xd, [Xn|SP, Xm]: 00 111 000 011 m 011 0 10 n d
      */
      UInt xN = iregEnc(am->ARM64am.RR.base);
      UInt xM = iregEnc(am->ARM64am.RR.index);
      vassert(xN <= 30);
      UInt instr = X_3_8_5_6_5_5(X001, isLoad ? X11000011 : X11000001, 
                                 xM, X011010, xN, wD);
      *p++ = instr;
      return p;
   }
   vpanic("do_load_or_store8");
   vassert(0);
}


/* Generate a 16 bit store or 16-to-64 unsigned widening load from/to
   rD, using the given amode for the address. */
static UInt* do_load_or_store16 ( UInt* p,
                                  Bool isLoad, UInt wD, ARM64AMode* am )
{
   vassert(wD <= 30);
   if (am->tag == ARM64am_RI9) {
      /* STURH Wd, [Xn|SP + simm9]:  01 111000 000 simm9 00 n d
         LDURH Wd, [Xn|SP + simm9]:  01 111000 010 simm9 00 n d
      */
      Int simm9 = am->ARM64am.RI9.simm9;
      vassert(-256 <= simm9 && simm9 <= 255);
      UInt instr = X_2_6_3_9_2_5_5(X01, X111000, isLoad ? X010 : X000,
                                   simm9 & 0x1FF, X00,
                                   iregEnc(am->ARM64am.RI9.reg), wD);
      *p++ = instr;
      return p;
   }
   if (am->tag == ARM64am_RI12) {
      /* STRH Wd, [Xn|SP + uimm12 * 2]:  01 111 001 00 imm12 n d
         LDRH Wd, [Xn|SP + uimm12 * 2]:  01 111 001 01 imm12 n d
      */
      UInt uimm12 = am->ARM64am.RI12.uimm12;
      UInt scale  = am->ARM64am.RI12.szB;
      vassert(scale == 2); /* failure of this is serious.  Do not ignore. */
      UInt xN    = iregEnc(am->ARM64am.RI12.reg);
      vassert(xN <= 30);
      UInt instr = X_2_6_2_12_5_5(X01, X111001, isLoad ? X01 : X00,
                                  uimm12, xN, wD);
      *p++ = instr;
      return p;
   }
   if (am->tag == ARM64am_RR) {
      /* STRH Xd, [Xn|SP, Xm]: 01 111 000 001 m 011 0 10 n d
         LDRH Xd, [Xn|SP, Xm]: 01 111 000 011 m 011 0 10 n d
      */
      UInt xN = iregEnc(am->ARM64am.RR.base);
      UInt xM = iregEnc(am->ARM64am.RR.index);
      vassert(xN <= 30);
      UInt instr = X_3_8_5_6_5_5(X011, isLoad ? X11000011 : X11000001, 
                                 xM, X011010, xN, wD);
      *p++ = instr;
      return p;
   }
   vpanic("do_load_or_store16");
   vassert(0);
}


/* Generate a 32 bit store or 32-to-64 unsigned widening load from/to
   rD, using the given amode for the address. */
static UInt* do_load_or_store32 ( UInt* p,
                                  Bool isLoad, UInt wD, ARM64AMode* am )
{
   vassert(wD <= 30);
   if (am->tag == ARM64am_RI9) {
      /* STUR Wd, [Xn|SP + simm9]:  10 111000 000 simm9 00 n d
         LDUR Wd, [Xn|SP + simm9]:  10 111000 010 simm9 00 n d
      */
      Int simm9 = am->ARM64am.RI9.simm9;
      vassert(-256 <= simm9 && simm9 <= 255);
      UInt instr = X_2_6_3_9_2_5_5(X10, X111000, isLoad ? X010 : X000,
                                   simm9 & 0x1FF, X00,
                                   iregEnc(am->ARM64am.RI9.reg), wD);
      *p++ = instr;
      return p;
   }
   if (am->tag == ARM64am_RI12) {
      /* STR Wd, [Xn|SP + uimm12 * 4]:  10 111 001 00 imm12 n d
         LDR Wd, [Xn|SP + uimm12 * 4]:  10 111 001 01 imm12 n d
      */
      UInt uimm12 = am->ARM64am.RI12.uimm12;
      UInt scale  = am->ARM64am.RI12.szB;
      vassert(scale == 4); /* failure of this is serious.  Do not ignore. */
      UInt xN    = iregEnc(am->ARM64am.RI12.reg);
      vassert(xN <= 30);
      UInt instr = X_2_6_2_12_5_5(X10, X111001, isLoad ? X01 : X00,
                                  uimm12, xN, wD);
      *p++ = instr;
      return p;
   }
   if (am->tag == ARM64am_RR) {
      /* STR Wd, [Xn|SP, Xm]: 10 111 000 001 m 011 0 10 n d
         LDR Wd, [Xn|SP, Xm]: 10 111 000 011 m 011 0 10 n d
      */
      UInt xN = iregEnc(am->ARM64am.RR.base);
      UInt xM = iregEnc(am->ARM64am.RR.index);
      vassert(xN <= 30);
      UInt instr = X_3_8_5_6_5_5(X101, isLoad ? X11000011 : X11000001, 
                                 xM, X011010, xN, wD);
      *p++ = instr;
      return p;
   }
   vpanic("do_load_or_store32");
   vassert(0);
}


/* Generate a 64 bit integer load or store to/from xD, using the given amode
   for the address. */
static UInt* do_load_or_store64 ( UInt* p,
                                  Bool isLoad, UInt xD, ARM64AMode* am )
{
   /* In all these cases, Rn can't be 31 since that means SP.  But Rd can be
      31, meaning XZR/WZR. */
   vassert(xD <= 31);
   if (am->tag == ARM64am_RI9) {
      /* STUR Xd, [Xn|SP + simm9]:  11 111000 000 simm9 00 n d
         LDUR Xd, [Xn|SP + simm9]:  11 111000 010 simm9 00 n d
      */
      Int simm9 = am->ARM64am.RI9.simm9;
      vassert(-256 <= simm9 && simm9 <= 255);
      UInt xN = iregEnc(am->ARM64am.RI9.reg);
      vassert(xN <= 30);
      UInt instr = X_2_6_3_9_2_5_5(X11, X111000, isLoad ? X010 : X000,
                                   simm9 & 0x1FF, X00, xN, xD);
      *p++ = instr;
      return p;
   }
   if (am->tag == ARM64am_RI12) {
      /* STR Xd, [Xn|SP + uimm12 * 8]:  11 111 001 00 imm12 n d
         LDR Xd, [Xn|SP + uimm12 * 8]:  11 111 001 01 imm12 n d
      */
      UInt uimm12 = am->ARM64am.RI12.uimm12;
      UInt scale  = am->ARM64am.RI12.szB;
      vassert(scale == 8); /* failure of this is serious.  Do not ignore. */
      UInt xN    = iregEnc(am->ARM64am.RI12.reg);
      vassert(xN <= 30);
      UInt instr = X_2_6_2_12_5_5(X11, X111001, isLoad ? X01 : X00,
                                  uimm12, xN, xD);
      *p++ = instr;
      return p;
   }
   if (am->tag == ARM64am_RR) {
      /* STR Xd, [Xn|SP, Xm]: 11 111 000 001 m 011 0 10 n d
         LDR Xd, [Xn|SP, Xm]: 11 111 000 011 m 011 0 10 n d
      */
      UInt xN = iregEnc(am->ARM64am.RR.base);
      UInt xM = iregEnc(am->ARM64am.RR.index);
      vassert(xN <= 30);
      UInt instr = X_3_8_5_6_5_5(X111, isLoad ? X11000011 : X11000001, 
                                 xM, X011010, xN, xD);
      *p++ = instr;
      return p;
   }
   vpanic("do_load_or_store64");
   vassert(0);
}


/* Emit an instruction into buf and return the number of bytes used.
   Note that buf is not the insn's final place, and therefore it is
   imperative to emit position-independent code.  If the emitted
   instruction was a profiler inc, set *is_profInc to True, else
   leave it unchanged. */

Int emit_ARM64Instr ( /*MB_MOD*/Bool* is_profInc,
                      UChar* buf, Int nbuf, const ARM64Instr* i,
                      Bool mode64, VexEndness endness_host,
                      const void* disp_cp_chain_me_to_slowEP,
                      const void* disp_cp_chain_me_to_fastEP,
                      const void* disp_cp_xindir,
                      const void* disp_cp_xassisted )
{
   UInt* p = (UInt*)buf;
   vassert(nbuf >= 32);
   vassert(mode64 == True);
   vassert(0 == (((HWord)buf) & 3));

   switch (i->tag) {
      case ARM64in_Arith: {
         UInt      rD   = iregEnc(i->ARM64in.Arith.dst);
         UInt      rN   = iregEnc(i->ARM64in.Arith.argL);
         ARM64RIA* argR = i->ARM64in.Arith.argR;
         switch (argR->tag) {
            case ARM64riA_I12:
               *p++ = X_2_6_2_12_5_5(
                         i->ARM64in.Arith.isAdd ? X10 : X11,
                         X010001,
                         argR->ARM64riA.I12.shift == 12 ? X01 : X00,
                         argR->ARM64riA.I12.imm12, rN, rD
                      );
               break;
            case ARM64riA_R: {
               UInt rM = iregEnc(i->ARM64in.Arith.argR->ARM64riA.R.reg);
               *p++ = X_3_8_5_6_5_5(
                         i->ARM64in.Arith.isAdd ? X100 : X110,
                         X01011000, rM, X000000, rN, rD
                      );
               break;
            }
            default:
               goto bad;
         }
         goto done;
      }
      case ARM64in_Cmp: {
         UInt      rD   = 31; /* XZR, we are going to dump the result */
         UInt      rN   = iregEnc(i->ARM64in.Cmp.argL);
         ARM64RIA* argR = i->ARM64in.Cmp.argR;
         Bool      is64 = i->ARM64in.Cmp.is64;
         switch (argR->tag) {
            case ARM64riA_I12:
               /* 1 11 10001 sh imm12 Rn Rd = SUBS Xd, Xn, #imm */
               /* 0 11 10001 sh imm12 Rn Rd = SUBS Wd, Wn, #imm */
               *p++ = X_2_6_2_12_5_5(
                         is64 ? X11 : X01, X110001,
                         argR->ARM64riA.I12.shift == 12 ? X01 : X00,
                         argR->ARM64riA.I12.imm12, rN, rD);
               break;
            case ARM64riA_R: {
               /* 1 11 01011 00 0 Rm 000000 Rn Rd = SUBS Xd, Xn, Xm */
               /* 0 11 01011 00 0 Rm 000000 Rn Rd = SUBS Wd, Wn, Wm */
               UInt rM = iregEnc(i->ARM64in.Cmp.argR->ARM64riA.R.reg);
               *p++ = X_3_8_5_6_5_5(is64 ? X111 : X011,
                                    X01011000, rM, X000000, rN, rD);
               break;
            }
            default:
               goto bad;
         }
         goto done;
      }
      case ARM64in_Logic: {
         UInt      rD   = iregEnc(i->ARM64in.Logic.dst);
         UInt      rN   = iregEnc(i->ARM64in.Logic.argL);
         ARM64RIL* argR = i->ARM64in.Logic.argR;
         UInt      opc  = 0; /* invalid */
         vassert(rD < 31);
         vassert(rN < 31);
         switch (i->ARM64in.Logic.op) {
            case ARM64lo_OR:  opc = X101; break;
            case ARM64lo_AND: opc = X100; break;
            case ARM64lo_XOR: opc = X110; break;
            default: break;
         }
         vassert(opc != 0);
         switch (argR->tag) {
            case ARM64riL_I13: {
               /* 1 01 100100 N immR immS Rn Rd = ORR <Xd|Sp>, Xn, #imm */
               /* 1 00 100100 N immR immS Rn Rd = AND <Xd|Sp>, Xn, #imm */
               /* 1 10 100100 N immR immS Rn Rd = EOR <Xd|Sp>, Xn, #imm */
               *p++ = X_3_6_1_6_6_5_5(
                         opc, X100100, argR->ARM64riL.I13.bitN,
                         argR->ARM64riL.I13.immR, argR->ARM64riL.I13.immS,
                         rN, rD
                      );
               break;
            }
            case ARM64riL_R: {
               /* 1 01 01010 00 0 m 000000 n d = ORR Xd, Xn, Xm */
               /* 1 00 01010 00 0 m 000000 n d = AND Xd, Xn, Xm */
               /* 1 10 01010 00 0 m 000000 n d = EOR Xd, Xn, Xm */
               UInt rM = iregEnc(argR->ARM64riL.R.reg);
               vassert(rM < 31);
               *p++ = X_3_8_5_6_5_5(opc, X01010000, rM, X000000, rN, rD);
               break;
            }
            default:
               goto bad;
         }
         goto done;
      }
      case ARM64in_RRS: {
         UInt top8 = 0;
         switch (i->ARM64in.RRS.mainOp) {
            case ARM64rrs_ADD: top8 = X10001011; break;
            case ARM64rrs_SUB: top8 = X11001011; break;
            case ARM64rrs_AND: top8 = X10001010; break;
            case ARM64rrs_XOR: top8 = X11001010; break;
            case ARM64rrs_OR:  top8 = X10101010; break;
            default: vassert(0); /*NOTREACHED*/
         }
         UInt sh = 0;
         switch (i->ARM64in.RRS.shiftOp) {
            case ARM64sh_SHL: sh = X00; break;
            case ARM64sh_SHR: sh = X01; break;
            case ARM64sh_SAR: sh = X10; break;
            default: vassert(0); /*NOTREACHED*/
         }
         UInt amt = i->ARM64in.RRS.amt;
         vassert(amt >= 1 && amt <= 63);
         *p++ = X_8_2_1_5_6_5_5(top8, sh, 0,
                                iregEnc(i->ARM64in.RRS.argR), amt,
                                iregEnc(i->ARM64in.RRS.argL),
                                iregEnc(i->ARM64in.RRS.dst));
         goto done;
      }
      case ARM64in_Test: {
         UInt      rD   = 31; /* XZR, we are going to dump the result */
         UInt      rN   = iregEnc(i->ARM64in.Test.argL);
         ARM64RIL* argR = i->ARM64in.Test.argR;
         switch (argR->tag) {
            case ARM64riL_I13: {
               /* 1 11 100100 N immR immS Rn Rd = ANDS Xd, Xn, #imm */
               *p++ = X_3_6_1_6_6_5_5(
                         X111, X100100, argR->ARM64riL.I13.bitN,
                         argR->ARM64riL.I13.immR, argR->ARM64riL.I13.immS,
                         rN, rD
                      );
               break;
            }
            default:
               goto bad;
         }
         goto done;
      }
      case ARM64in_Shift: {
         UInt      rD   = iregEnc(i->ARM64in.Shift.dst);
         UInt      rN   = iregEnc(i->ARM64in.Shift.argL);
         ARM64RI6* argR = i->ARM64in.Shift.argR;
         vassert(rD < 31);
         vassert(rN < 31);
         switch (argR->tag) {
            case ARM64ri6_I6: {
               /* 110 1001101 (63-sh) (64-sh) nn dd   LSL Xd, Xn, sh */
               /* 110 1001101 sh      63      nn dd   LSR Xd, Xn, sh */
               /* 100 1001101 sh      63      nn dd   ASR Xd, Xn, sh */
               UInt sh = argR->ARM64ri6.I6.imm6;
               vassert(sh > 0 && sh < 64);
               switch (i->ARM64in.Shift.op) {
                  case ARM64sh_SHL:
                     *p++ = X_3_6_1_6_6_5_5(X110, X100110,
                                            1, 64-sh, 63-sh, rN, rD);
                     break;
                  case ARM64sh_SHR:
                     *p++ = X_3_6_1_6_6_5_5(X110, X100110, 1, sh, 63, rN, rD);
                     break;
                  case ARM64sh_SAR:
                     *p++ = X_3_6_1_6_6_5_5(X100, X100110, 1, sh, 63, rN, rD);
                     break;
                  default:
                     vassert(0);
               }
               break;
            }
            case ARM64ri6_R: {
               /* 100 1101 0110 mm 001000 nn dd   LSL Xd, Xn, Xm */
               /* 100 1101 0110 mm 001001 nn dd   LSR Xd, Xn, Xm */
               /* 100 1101 0110 mm 001010 nn dd   ASR Xd, Xn, Xm */
               UInt rM = iregEnc(argR->ARM64ri6.R.reg);
               vassert(rM < 31);
               UInt subOpc = 0;
               switch (i->ARM64in.Shift.op) {
                  case ARM64sh_SHL: subOpc = X001000; break;
                  case ARM64sh_SHR: subOpc = X001001; break;
                  case ARM64sh_SAR: subOpc = X001010; break;
                  default: vassert(0);
               }
               *p++ = X_3_8_5_6_5_5(X100, X11010110, rM, subOpc, rN, rD);
               break;
            }
            default:
               vassert(0);
         }
         goto done;
      }
      case ARM64in_Unary: {
         UInt rDst = iregEnc(i->ARM64in.Unary.dst);
         UInt rSrc = iregEnc(i->ARM64in.Unary.src);
         switch (i->ARM64in.Unary.op) {
            case ARM64un_CLZ:
               /* 1 10 1101 0110 00000 00010 0 nn dd   CLZ Xd, Xn */
               /* 1 10 1101 0110 00000 00010 1 nn dd   CLS Xd, Xn (unimp) */
               *p++ = X_3_8_5_6_5_5(X110,
                                    X11010110, X00000, X000100, rSrc, rDst);
               goto done;
            case ARM64un_NEG:
               /* 1 10 01011 000 m 000000 11111 d  NEG Xd,Xm */
               /* 0 10 01011 000 m 000000 11111 d  NEG Wd,Wm (unimp) */
               *p++ = X_3_8_5_6_5_5(X110,
                                    X01011000, rSrc, X000000, X11111, rDst);
               goto done;
            case ARM64un_NOT: {
               /* 1 01 01010 00 1 m 000000 11111 d   MVN Xd,Xm */
               *p++ = X_3_8_5_6_5_5(X101,
                                    X01010001, rSrc, X000000, X11111, rDst);
               goto done;
            }
            default:
               break;
         }
         goto bad;
      }
      case ARM64in_Set64: {
         /* 1 00 1101 0100 11111 invert(cond) 01 11111 Rd   CSET Rd, Cond */
         UInt rDst = iregEnc(i->ARM64in.Set64.dst);
         UInt cc = (UInt)i->ARM64in.Set64.cond;
         vassert(cc < 14);
         *p++ = X_3_8_5_6_5_5(X100, X11010100, X11111,
                              ((cc ^ 1) << 2) | X01, X11111, rDst);
         goto done;
      }
      case ARM64in_MovI: {
         /* We generate the "preferred form", ORR Xd, XZR, Xm
            101 01010 00 0 m 000000 11111 d
         */
         UInt instr = 0xAA0003E0;
         UInt d     = iregEnc(i->ARM64in.MovI.dst);
         UInt m     = iregEnc(i->ARM64in.MovI.src);
         *p++ = instr | ((m & 31) << 16) | ((d & 31) << 0);
         goto done;
      }
      case ARM64in_Imm64: {
         p = imm64_to_ireg( p, iregEnc(i->ARM64in.Imm64.dst),
                               i->ARM64in.Imm64.imm64 );
         goto done;
      }
      case ARM64in_LdSt64: {
         p = do_load_or_store64( p, i->ARM64in.LdSt64.isLoad,
                                 iregEncOr31(i->ARM64in.LdSt64.rD),
                                 i->ARM64in.LdSt64.amode );
         goto done;
      }
      case ARM64in_LdSt32: {
         p = do_load_or_store32( p, i->ARM64in.LdSt32.isLoad,
                                 iregEnc(i->ARM64in.LdSt32.rD),
                                 i->ARM64in.LdSt32.amode );
         goto done;
      }
      case ARM64in_LdSt16: {
         p = do_load_or_store16( p, i->ARM64in.LdSt16.isLoad,
                                 iregEnc(i->ARM64in.LdSt16.rD),
                                 i->ARM64in.LdSt16.amode );
         goto done;
      }
      case ARM64in_LdSt8: {
         p = do_load_or_store8( p, i->ARM64in.LdSt8.isLoad,
                                iregEnc(i->ARM64in.LdSt8.rD),
                                i->ARM64in.LdSt8.amode );
         goto done;
      }

      case ARM64in_XDirect: {
         /* NB: what goes on here has to be very closely coordinated
            with chainXDirect_ARM64 and unchainXDirect_ARM64 below. */
         /* We're generating chain-me requests here, so we need to be
            sure this is actually allowed -- no-redir translations
            can't use chain-me's.  Hence: */
         vassert(disp_cp_chain_me_to_slowEP != NULL);
         vassert(disp_cp_chain_me_to_fastEP != NULL);

         /* Use ptmp for backpatching conditional jumps. */
         UInt* ptmp = NULL;

         /* First off, if this is conditional, create a conditional
            jump over the rest of it.  Or at least, leave a space for
            it that we will shortly fill in. */
         if (i->ARM64in.XDirect.cond != ARM64cc_AL) {
            vassert(i->ARM64in.XDirect.cond != ARM64cc_NV);
            ptmp = p;
            *p++ = 0;
         }

         /* Update the guest PC. */
         /* imm64 x9, dstGA */
         /* str   x9, amPC */
         p = imm64_to_ireg(p, /*x*/9, i->ARM64in.XDirect.dstGA);
         p = do_load_or_store64(p, False/*!isLoad*/,
                                /*x*/9, i->ARM64in.XDirect.amPC);

         /* --- FIRST PATCHABLE BYTE follows --- */
         /* VG_(disp_cp_chain_me_to_{slowEP,fastEP}) (where we're
            calling to) backs up the return address, so as to find the
            address of the first patchable byte.  So: don't change the
            number of instructions (5) below. */
         /* movw x9, VG_(disp_cp_chain_me_to_{slowEP,fastEP})[15:0] */
         /* movk x9, VG_(disp_cp_chain_me_to_{slowEP,fastEP})[31:15], lsl 16 */
         /* movk x9, VG_(disp_cp_chain_me_to_{slowEP,fastEP})[47:32], lsl 32 */
         /* movk x9, VG_(disp_cp_chain_me_to_{slowEP,fastEP})[63:48], lsl 48 */
         /* blr  x9 */
         const void* disp_cp_chain_me
                  = i->ARM64in.XDirect.toFastEP ? disp_cp_chain_me_to_fastEP 
                                                : disp_cp_chain_me_to_slowEP;
         p = imm64_to_ireg_EXACTLY4(p, /*x*/9, (Addr)disp_cp_chain_me);
         *p++ = 0xD63F0120;
         /* --- END of PATCHABLE BYTES --- */

         /* Fix up the conditional jump, if there was one. */
         if (i->ARM64in.XDirect.cond != ARM64cc_AL) {
            Int delta = (UChar*)p - (UChar*)ptmp; /* must be signed */
            vassert(delta > 0 && delta <= 40);
            vassert((delta & 3) == 0);
            UInt notCond = 1 ^ (UInt)i->ARM64in.XDirect.cond;
            vassert(notCond <= 13); /* Neither AL nor NV */
            vassert(ptmp != NULL);
            delta = delta >> 2;
            *ptmp = X_8_19_1_4(X01010100, delta & ((1<<19)-1), 0, notCond);
         }
         goto done;
      }

      case ARM64in_XIndir: {
         // XIndir is more or less the same as XAssisted, except
         // we don't have a trc value to hand back, so there's no
         // write to r21
         /* Use ptmp for backpatching conditional jumps. */
         //UInt* ptmp = NULL;

         /* First off, if this is conditional, create a conditional
            jump over the rest of it.  Or at least, leave a space for
            it that we will shortly fill in. */
         if (i->ARM64in.XIndir.cond != ARM64cc_AL) {
            vassert(0); //ATC
//ZZ             vassert(i->ARMin.XIndir.cond != ARMcc_NV);
//ZZ             ptmp = p;
//ZZ             *p++ = 0;
         }

         /* Update the guest PC. */
         /* str r-dstGA, amPC */
         p = do_load_or_store64(p, False/*!isLoad*/,
                                iregEnc(i->ARM64in.XIndir.dstGA),
                                i->ARM64in.XIndir.amPC);

         /* imm64 x9, VG_(disp_cp_xindir) */
         /* br    x9 */
         p = imm64_to_ireg(p, /*x*/9, (Addr)disp_cp_xindir);
         *p++ = 0xD61F0120; /* br x9 */

         /* Fix up the conditional jump, if there was one. */
         if (i->ARM64in.XIndir.cond != ARM64cc_AL) {
            vassert(0); //ATC
//ZZ             Int delta = (UChar*)p - (UChar*)ptmp; /* must be signed */
//ZZ             vassert(delta > 0 && delta < 40);
//ZZ             vassert((delta & 3) == 0);
//ZZ             UInt notCond = 1 ^ (UInt)i->ARMin.XIndir.cond;
//ZZ             vassert(notCond <= 13); /* Neither AL nor NV */
//ZZ             delta = (delta >> 2) - 2;
//ZZ             *ptmp = XX______(notCond, X1010) | (delta & 0xFFFFFF);
         }
         goto done;
      }

      case ARM64in_XAssisted: {
         /* Use ptmp for backpatching conditional jumps. */
         UInt* ptmp = NULL;

         /* First off, if this is conditional, create a conditional
            jump over the rest of it.  Or at least, leave a space for
            it that we will shortly fill in.  I think this can only
            ever happen when VEX is driven by the switchbacker. */
         if (i->ARM64in.XAssisted.cond != ARM64cc_AL) {
            vassert(i->ARM64in.XDirect.cond != ARM64cc_NV);
            ptmp = p;
            *p++ = 0;
         }

         /* Update the guest PC. */
         /* str r-dstGA, amPC */
         p = do_load_or_store64(p, False/*!isLoad*/,
                                iregEnc(i->ARM64in.XAssisted.dstGA),
                                i->ARM64in.XAssisted.amPC);

         /* movw r21,  $magic_number */
         UInt trcval = 0;
         switch (i->ARM64in.XAssisted.jk) {
            case Ijk_ClientReq:   trcval = VEX_TRC_JMP_CLIENTREQ;   break;
            case Ijk_Sys_syscall: trcval = VEX_TRC_JMP_SYS_SYSCALL; break;
            //case Ijk_Sys_int128:  trcval = VEX_TRC_JMP_SYS_INT128;  break;
            case Ijk_Yield:       trcval = VEX_TRC_JMP_YIELD;       break;
            //case Ijk_EmWarn:      trcval = VEX_TRC_JMP_EMWARN;      break;
            //case Ijk_MapFail:     trcval = VEX_TRC_JMP_MAPFAIL;     break;
            case Ijk_NoDecode:    trcval = VEX_TRC_JMP_NODECODE;    break;
            case Ijk_InvalICache: trcval = VEX_TRC_JMP_INVALICACHE; break;
            case Ijk_FlushDCache: trcval = VEX_TRC_JMP_FLUSHDCACHE; break;
            case Ijk_NoRedir:     trcval = VEX_TRC_JMP_NOREDIR;     break;
            case Ijk_SigTRAP:     trcval = VEX_TRC_JMP_SIGTRAP;     break;
            case Ijk_SigBUS:      trcval = VEX_TRC_JMP_SIGBUS;      break;
            //case Ijk_SigSEGV:     trcval = VEX_TRC_JMP_SIGSEGV;     break;
            case Ijk_Boring:      trcval = VEX_TRC_JMP_BORING;      break;
            /* We don't expect to see the following being assisted. */
            //case Ijk_Ret:
            //case Ijk_Call:
            /* fallthrough */
            default: 
               ppIRJumpKind(i->ARM64in.XAssisted.jk);
               vpanic("emit_ARM64Instr.ARM64in_XAssisted: "
                      "unexpected jump kind");
         }
         vassert(trcval != 0);
         p = imm64_to_ireg(p, /*x*/21, (ULong)trcval);

         /* imm64 x9, VG_(disp_cp_xassisted) */
         /* br    x9 */
         p = imm64_to_ireg(p, /*x*/9, (Addr)disp_cp_xassisted);
         *p++ = 0xD61F0120; /* br x9 */

         /* Fix up the conditional jump, if there was one. */
         if (i->ARM64in.XAssisted.cond != ARM64cc_AL) {
            Int delta = (UChar*)p - (UChar*)ptmp; /* must be signed */
            vassert(delta > 0 && delta < 40);
            vassert((delta & 3) == 0);
            UInt notCond = 1 ^ (UInt)i->ARM64in.XDirect.cond;
            vassert(notCond <= 13); /* Neither AL nor NV */
            vassert(ptmp != NULL);
            delta = delta >> 2;
            *ptmp = X_8_19_1_4(X01010100, delta & ((1<<19)-1), 0, notCond);
         }
         goto done;
      }

      case ARM64in_CSel: {
         /* 100 1101 0100 mm cond 00 nn dd = CSEL Xd, Xn, Xm, cond */
         UInt dd   = iregEnc(i->ARM64in.CSel.dst);
         UInt nn   = iregEnc(i->ARM64in.CSel.argL);
         UInt mm   = iregEncOr31(i->ARM64in.CSel.argR); // Can be XZR
         UInt cond = (UInt)i->ARM64in.CSel.cond;
         vassert(dd < 31 && nn < 31 && mm <= 31 && cond < 16);
         *p++ = X_3_8_5_6_5_5(X100, X11010100, mm, cond << 2, nn, dd);
         goto done;
      }

      case ARM64in_Call: {
         /* We'll use x9 as a scratch register to put the target
            address in. */
         if (i->ARM64in.Call.cond != ARM64cc_AL
             && i->ARM64in.Call.rloc.pri != RLPri_None) {
            /* The call might not happen (it isn't unconditional) and
               it returns a result.  In this case we will need to
               generate a control flow diamond to put 0x555..555 in
               the return register(s) in the case where the call
               doesn't happen.  If this ever becomes necessary, maybe
               copy code from the 32-bit ARM equivalent.  Until that
               day, just give up. */
            goto bad;
         }

         UInt* ptmp = NULL;
         if (i->ARM64in.Call.cond != ARM64cc_AL) {
            /* Create a hole to put a conditional branch in.  We'll
               patch it once we know the branch length. */
            ptmp = p;
            *p++ = 0;
         }

         // x9 = &target
         p = imm64_to_ireg( (UInt*)p, /*x*/9, (ULong)i->ARM64in.Call.target );
         // blr x9
         *p++ = 0xD63F0120;

         // Patch the hole if necessary
         if (i->ARM64in.Call.cond != ARM64cc_AL) {
            ULong dist = (ULong)(p - ptmp);
            /* imm64_to_ireg produces between 1 and 4 insns, and
               then there's the BLR itself.  Hence: */
            vassert(dist >= 2 && dist <= 5);
            vassert(ptmp != NULL);
            // 01010100 simm19 0 cond = B.cond (here + simm19 << 2)
            *ptmp = X_8_19_1_4(X01010100, dist, 0,
                               1 ^ (UInt)i->ARM64in.Call.cond);
         } else {
            vassert(ptmp == NULL);
         }

         goto done;
      }

      case ARM64in_AddToSP: {
         /* 10,0 10001 00 imm12 11111 11111  ADD xsp, xsp, #imm12
            11,0 10001 00 imm12 11111 11111  SUB xsp, xsp, #imm12
         */
         Int simm12 = i->ARM64in.AddToSP.simm;
         vassert(-4096 < simm12 && simm12 < 4096);
         vassert(0 == (simm12 & 0xF));
         if (simm12 >= 0) {
            *p++ = X_2_6_2_12_5_5(X10, X010001, X00, simm12, X11111, X11111);
         } else {
            *p++ = X_2_6_2_12_5_5(X11, X010001, X00, -simm12, X11111, X11111);
         }
         goto done;
      }

      case ARM64in_FromSP: {
         /* 10,0 10001 00 0..(12)..0 11111 dd  MOV Xd, xsp */
         UInt dd = iregEnc(i->ARM64in.FromSP.dst);
         vassert(dd < 31);
         *p++ = X_2_6_2_12_5_5(X10, X010001, X00, 0, X11111, dd);
         goto done;
      }

      case ARM64in_Mul: {
         /* 100 11011 110 mm 011111 nn dd   UMULH Xd, Xn,Xm
            100 11011 010 mm 011111 nn dd   SMULH Xd, Xn,Xm
            100 11011 000 mm 011111 nn dd   MUL   Xd, Xn,Xm
         */
         UInt dd = iregEnc(i->ARM64in.Mul.dst);
         UInt nn = iregEnc(i->ARM64in.Mul.argL);
         UInt mm = iregEnc(i->ARM64in.Mul.argR);
         vassert(dd < 31 && nn < 31 && mm < 31);
         switch (i->ARM64in.Mul.op) {
            case ARM64mul_ZX:
               *p++ = X_3_8_5_6_5_5(X100, X11011110, mm, X011111, nn, dd);
               goto done;
            case ARM64mul_SX:
               *p++ = X_3_8_5_6_5_5(X100, X11011010, mm, X011111, nn, dd);
               goto done;
            case ARM64mul_PLAIN:
               *p++ = X_3_8_5_6_5_5(X100, X11011000, mm, X011111, nn, dd);
               goto done;
            default:
               vassert(0);
         }
         goto bad;
      }
      case ARM64in_LdrEX: {
         /* 085F7C82   ldxrb w2, [x4]
            485F7C82   ldxrh w2, [x4]
            885F7C82   ldxr  w2, [x4]
            C85F7C82   ldxr  x2, [x4]
         */
         switch (i->ARM64in.LdrEX.szB) {
            case 1: *p++ = 0x085F7C82; goto done;
            case 2: *p++ = 0x485F7C82; goto done;
            case 4: *p++ = 0x885F7C82; goto done;
            case 8: *p++ = 0xC85F7C82; goto done;
            default: break;
         }
         goto bad;
      }
      case ARM64in_StrEX: {
         /* 08007C82   stxrb w0, w2, [x4]
            48007C82   stxrh w0, w2, [x4]
            88007C82   stxr  w0, w2, [x4]
            C8007C82   stxr  w0, x2, [x4]
         */
         switch (i->ARM64in.StrEX.szB) {
            case 1: *p++ = 0x08007C82; goto done;
            case 2: *p++ = 0x48007C82; goto done;
            case 4: *p++ = 0x88007C82; goto done;
            case 8: *p++ = 0xC8007C82; goto done;
            default: break;
         }
         goto bad;
      }
      case ARM64in_LdrEXP: {
         // 820C7FC8   ldxp x2, x3, [x4]
         *p++ = 0xC87F0C82;
         goto done;
      }
      case ARM64in_StrEXP: {
         // 820C20C8   stxp w0, x2, x3, [x4]
         *p++ = 0xC8200C82;
         goto done;
      }
      case ARM64in_CAS: {
         /* This isn't simple.  For an explanation see the comment in
            host_arm64_defs.h on the definition of ARM64Instr case CAS.

            NOTE: We could place "loop:" after mov/and but then we need
                  an additional scratch register.
         */
         /* Generate:

            loop:
              -- one of:
              mov     x8, x5                 // AA0503E8
              and     x8, x5, #0xFFFFFFFF    // 92407CA8
              and     x8, x5, #0xFFFF        // 92403CA8
              and     x8, x5, #0xFF          // 92401CA8

              -- one of:
              ldxr    x1, [x3]               // C85F7C61
              ldxr    w1, [x3]               // 885F7C61
              ldxrh   w1, [x3]               // 485F7C61 
              ldxrb   w1, [x3]               // 085F7C61

              -- always:
              cmp     x1, x8                 // EB08003F
              bne     out                    // 54000061

              -- one of:
              stxr    w8, x7, [x3]           // C8087C67
              stxr    w8, w7, [x3]           // 88087C67
              stxrh   w8, w7, [x3]           // 48087C67
              stxrb   w8, w7, [x3]           // 08087C67

              -- always:
              cbne    w8, loop               // 35FFFF68
            out:
         */
         switch (i->ARM64in.CAS.szB) {
            case 8:  *p++ = 0xAA0503E8; break;
            case 4:  *p++ = 0x92407CA8; break;
            case 2:  *p++ = 0x92403CA8; break;
            case 1:  *p++ = 0x92401CA8; break;
            default: vassert(0);
         }
         switch (i->ARM64in.CAS.szB) {
            case 8:  *p++ = 0xC85F7C61; break;
            case 4:  *p++ = 0x885F7C61; break;
            case 2:  *p++ = 0x485F7C61; break;
            case 1:  *p++ = 0x085F7C61; break;
         }
         *p++ = 0xEB08003F;
         *p++ = 0x54000061;
         switch (i->ARM64in.CAS.szB) {
            case 8:  *p++ = 0xC8087C67; break;
            case 4:  *p++ = 0x88087C67; break;
            case 2:  *p++ = 0x48087C67; break;
            case 1:  *p++ = 0x08087C67; break;
         }
         *p++ = 0x35FFFF68;
         goto done;
      }
      case ARM64in_CASP: {
         /* Generate:
            CASP <Xs>, <X(s+1)>, <Xt>, <X(t+1)>, [<Xn|SP>{,#0}]

            Register allocation (see ARM64in_CASP in getRegUsage_ARM64Instr):
            Xn:         memory address
                        -> X2 (INPUT)
            Xs, X(s+1): values to be compared with value read from address
                        -> X4,X5 (INPUTS)
                        -> X0,X1 (OUTPUTS) loaded from memory and compared with
                           scratch registers X8,X9 (CLOBBERED) which contain
                           contents of X4,X5
            Xt, X(t+1): values to be stored to memory if X0,X1==X8,X9
                        -> X6,X7 (INPUT)

            loop:
              -- two of:
              mov     x8, x4                 // AA0403E8
              mov     x9, x5                 // AA0503E9
              and     x8, x4, #0xFFFFFFFF    // 92407C88
              and     x9, x5, #0xFFFFFFFF    // 92407CA9

              -- one of:
              ldxp    x0,x1, [x2]            // C87F0440
              ldxp    w0,w1, [x2]            // 887F0440

              -- always:
              cmp     x0, x8                 // EB08001F
              bne     out                    // 540000A1
              cmp     x1, x9                 // EB09003F
              bne     out                    // 54000061

              -- one of:
              stxp    w3, x6, x7, [x2]       // C8231C46
              stxp    w3, w6, w7, [x2]       // 88231C46

              -- always:
              cbnz    w3, loop               // 35FFFF03
            out:
         */
         switch (i->ARM64in.CASP.szB) {
            case 8:  *p++ = 0xAA0403E8; *p++ = 0xAA0503E9; break;
            case 4:  *p++ = 0x92407C88; *p++ = 0x92407CA9; break;
            default: vassert(0);
         }
         switch (i->ARM64in.CASP.szB) {
            case 8:  *p++ = 0xC87F0440; break;
            case 4:  *p++ = 0x887F0440; break;
            default: vassert(0);
         }
         *p++ = 0xEB08001F;
         *p++ = 0x540000A1;
         *p++ = 0xEB09003F;
         *p++ = 0x54000061;
         switch (i->ARM64in.CASP.szB) {
            case 8:  *p++ = 0xC8231C46; break;
            case 4:  *p++ = 0x88231C46; break;
            default: vassert(0);
         }
         *p++ = 0x35FFFF03;
         goto done;
      }
      case ARM64in_MFence: {
         *p++ = 0xD5033F9F; /* DSB sy */
         *p++ = 0xD5033FBF; /* DMB sy */
         *p++ = 0xD5033FDF; /* ISB */
         goto done;
      }
      case ARM64in_ClrEX: {
         *p++ = 0xD5033F5F; /* clrex #15 */
         goto done;
      }
      case ARM64in_VLdStH: {
         /* 01 111101 01 imm12 n t   LDR Ht, [Xn|SP, #imm12 * 2]
            01 111101 00 imm12 n t   STR Ht, [Xn|SP, #imm12 * 2]
         */
         UInt hD     = dregEnc(i->ARM64in.VLdStH.hD);
         UInt rN     = iregEnc(i->ARM64in.VLdStH.rN);
         UInt uimm12 = i->ARM64in.VLdStH.uimm12;
         Bool isLD   = i->ARM64in.VLdStH.isLoad;
         vassert(uimm12 < 8192 && 0 == (uimm12 & 1));
         uimm12 >>= 1;
         vassert(uimm12 < (1<<12));
         vassert(hD < 32);
         vassert(rN < 31);
         *p++ = X_2_6_2_12_5_5(X01, X111101, isLD ? X01 : X00,
                               uimm12, rN, hD);
         goto done;
      }
      case ARM64in_VLdStS: {
         /* 10 111101 01 imm12 n t   LDR St, [Xn|SP, #imm12 * 4]
            10 111101 00 imm12 n t   STR St, [Xn|SP, #imm12 * 4]
         */
         UInt sD     = dregEnc(i->ARM64in.VLdStS.sD);
         UInt rN     = iregEnc(i->ARM64in.VLdStS.rN);
         UInt uimm12 = i->ARM64in.VLdStS.uimm12;
         Bool isLD   = i->ARM64in.VLdStS.isLoad;
         vassert(uimm12 < 16384 && 0 == (uimm12 & 3));
         uimm12 >>= 2;
         vassert(uimm12 < (1<<12));
         vassert(sD < 32);
         vassert(rN < 31);
         *p++ = X_2_6_2_12_5_5(X10, X111101, isLD ? X01 : X00,
                               uimm12, rN, sD);
         goto done;
      }
      case ARM64in_VLdStD: {
         /* 11 111101 01 imm12 n t   LDR Dt, [Xn|SP, #imm12 * 8]
            11 111101 00 imm12 n t   STR Dt, [Xn|SP, #imm12 * 8]
         */
         UInt dD     = dregEnc(i->ARM64in.VLdStD.dD);
         UInt rN     = iregEnc(i->ARM64in.VLdStD.rN);
         UInt uimm12 = i->ARM64in.VLdStD.uimm12;
         Bool isLD   = i->ARM64in.VLdStD.isLoad;
         vassert(uimm12 < 32768 && 0 == (uimm12 & 7));
         uimm12 >>= 3;
         vassert(uimm12 < (1<<12));
         vassert(dD < 32);
         vassert(rN < 31);
         *p++ = X_2_6_2_12_5_5(X11, X111101, isLD ? X01 : X00,
                               uimm12, rN, dD);
         goto done;
      }
      case ARM64in_VLdStQ: {
         /* 0100 1100 0000 0000 0111 11 rN rQ   st1 {vQ.2d}, [<rN|SP>]
            0100 1100 0100 0000 0111 11 rN rQ   ld1 {vQ.2d}, [<rN|SP>]
         */
         UInt rQ = qregEnc(i->ARM64in.VLdStQ.rQ);
         UInt rN = iregEnc(i->ARM64in.VLdStQ.rN);
         vassert(rQ < 32);
         vassert(rN < 31);
         if (i->ARM64in.VLdStQ.isLoad) {
            *p++ = 0x4C407C00 | (rN << 5) | rQ;
         } else {
            *p++ = 0x4C007C00 | (rN << 5) | rQ;
         }
         goto done;
      }
      case ARM64in_VCvtI2F: {
         /* 31  28    23 21 20 18  15     9 4
            000 11110 00 1  00 010 000000 n d  SCVTF Sd, Wn
            000 11110 01 1  00 010 000000 n d  SCVTF Dd, Wn
            100 11110 00 1  00 010 000000 n d  SCVTF Sd, Xn
            100 11110 01 1  00 010 000000 n d  SCVTF Dd, Xn
            000 11110 00 1  00 011 000000 n d  UCVTF Sd, Wn
            000 11110 01 1  00 011 000000 n d  UCVTF Dd, Wn
            100 11110 00 1  00 011 000000 n d  UCVTF Sd, Xn
            100 11110 01 1  00 011 000000 n d  UCVTF Dd, Xn
         */
         UInt       rN = iregEnc(i->ARM64in.VCvtI2F.rS);
         UInt       rD = dregEnc(i->ARM64in.VCvtI2F.rD);
         ARM64CvtOp how = i->ARM64in.VCvtI2F.how;
         /* Just handle cases as they show up. */
         switch (how) {
            case ARM64cvt_F32_I32S: /* SCVTF Sd, Wn */
               *p++ = X_3_5_8_6_5_5(X000, X11110, X00100010, X000000, rN, rD);
               break;
            case ARM64cvt_F64_I32S: /* SCVTF Dd, Wn */
               *p++ = X_3_5_8_6_5_5(X000, X11110, X01100010, X000000, rN, rD);
               break;
            case ARM64cvt_F32_I64S: /* SCVTF Sd, Xn */
               *p++ = X_3_5_8_6_5_5(X100, X11110, X00100010, X000000, rN, rD);
               break;
            case ARM64cvt_F64_I64S: /* SCVTF Dd, Xn */
               *p++ = X_3_5_8_6_5_5(X100, X11110, X01100010, X000000, rN, rD);
               break;
            case ARM64cvt_F32_I32U: /* UCVTF Sd, Wn */
               *p++ = X_3_5_8_6_5_5(X000, X11110, X00100011, X000000, rN, rD);
               break;
            case ARM64cvt_F64_I32U: /* UCVTF Dd, Wn */
               *p++ = X_3_5_8_6_5_5(X000, X11110, X01100011, X000000, rN, rD);
               break;
            case ARM64cvt_F32_I64U: /* UCVTF Sd, Xn */
               *p++ = X_3_5_8_6_5_5(X100, X11110, X00100011, X000000, rN, rD);
               break;
            case ARM64cvt_F64_I64U: /* UCVTF Dd, Xn  */
               *p++ = X_3_5_8_6_5_5(X100, X11110, X01100011, X000000, rN, rD);
               break;
            default:
               goto bad; //ATC
         }
         goto done;
      }
      case ARM64in_VCvtF2I: {
         /*    30       23   20 18  15     9 4
            sf 00,11110,0x 1 00 000,000000 n d  FCVTNS Rd, Fn (round to
            sf 00,11110,0x 1 00 001,000000 n d  FCVTNU Rd, Fn  nearest)
            ---------------- 01 --------------  FCVTP-------- (round to +inf)
            ---------------- 10 --------------  FCVTM-------- (round to -inf)
            ---------------- 11 --------------  FCVTZ-------- (round to zero)
            ---------------- 00 100 ----------  FCVTAS------- (nearest, ties away)
            ---------------- 00 101 ----------  FCVTAU------- (nearest, ties away)

            Rd is Xd when sf==1, Wd when sf==0
            Fn is Dn when x==1, Sn when x==0
            20:19 carry the rounding mode, using the same encoding as FPCR
            18 enable translation to FCVTA{S,U}
         */
         UInt       rD    = iregEnc(i->ARM64in.VCvtF2I.rD);
         UInt       rN    = dregEnc(i->ARM64in.VCvtF2I.rS);
         ARM64CvtOp how   = i->ARM64in.VCvtF2I.how;
         UChar      armRM = i->ARM64in.VCvtF2I.armRM;
         UChar      bit18 = i->ARM64in.VCvtF2I.tiesToAway ? 4 : 0;
         /* Just handle cases as they show up. */
         switch (how) {
            case ARM64cvt_F64_I32S: /* FCVTxS Wd, Dn */
               *p++ = X_3_5_8_6_5_5(X000, X11110, X01100000 | (armRM << 3) | bit18,
                                    X000000, rN, rD);
               break;
            case ARM64cvt_F64_I32U: /* FCVTxU Wd, Dn */
               *p++ = X_3_5_8_6_5_5(X000, X11110, X01100001 | (armRM << 3) | bit18,
                                    X000000, rN, rD);
               break;
            case ARM64cvt_F64_I64S: /* FCVTxS Xd, Dn */
               *p++ = X_3_5_8_6_5_5(X100, X11110, X01100000 | (armRM << 3) | bit18,
                                    X000000, rN, rD);
               break;
            case ARM64cvt_F64_I64U: /* FCVTxU Xd, Dn */
               *p++ = X_3_5_8_6_5_5(X100, X11110, X01100001 | (armRM << 3) | bit18,
                                    X000000, rN, rD);
               break;
            case ARM64cvt_F32_I32S: /* FCVTxS Wd, Sn */
               *p++ = X_3_5_8_6_5_5(X000, X11110, X00100000 | (armRM << 3) | bit18,
                                    X000000, rN, rD);
               break;
            case ARM64cvt_F32_I32U: /* FCVTxU Wd, Sn */
               *p++ = X_3_5_8_6_5_5(X000, X11110, X00100001 | (armRM << 3) | bit18,
                                    X000000, rN, rD);
               break;
            case ARM64cvt_F32_I64S: /* FCVTxS Xd, Sn */
               *p++ = X_3_5_8_6_5_5(X100, X11110, X00100000 | (armRM << 3) | bit18,
                                    X000000, rN, rD);
               break;
            case ARM64cvt_F32_I64U: /* FCVTxU Xd, Sn */
               *p++ = X_3_5_8_6_5_5(X100, X11110, X00100001 | (armRM << 3) | bit18,
                                    X000000, rN, rD);
               break;
            default:
               goto bad; //ATC
         }
         goto done;
      }
      case ARM64in_VCvtSD: {
         /* 31         23 21    16  14    9 4
            000,11110, 00 10001 0,1 10000 n d   FCVT Dd, Sn (S->D)
            ---------- 01 ----- 0,0 ---------   FCVT Sd, Dn (D->S)
            Rounding, when dst is smaller than src, is per the FPCR.
         */
         UInt dd = dregEnc(i->ARM64in.VCvtSD.dst);
         UInt nn = dregEnc(i->ARM64in.VCvtSD.src);
         if (i->ARM64in.VCvtSD.sToD) {
            *p++ = X_3_5_8_6_5_5(X000, X11110, X00100010, X110000, nn, dd);
         } else {
            *p++ = X_3_5_8_6_5_5(X000, X11110, X01100010, X010000, nn, dd);
         }
         goto done;
      }
      case ARM64in_VCvtHS: {
         /* 31         23 21    16  14    9 4
            000,11110, 11 10001 0,0 10000 n d   FCVT Sd, Hn (H->S)
            ---------- 00 ----- 1,1 ---------   FCVT Hd, Sn (S->H)
            Rounding, when dst is smaller than src, is per the FPCR.
         */
         UInt dd = dregEnc(i->ARM64in.VCvtHS.dst);
         UInt nn = dregEnc(i->ARM64in.VCvtHS.src);
         if (i->ARM64in.VCvtHS.hToS) {
            *p++ = X_3_5_8_6_5_5(X000, X11110, X11100010, X010000, nn, dd);
         } else {
            *p++ = X_3_5_8_6_5_5(X000, X11110, X00100011, X110000, nn, dd);
         }
         goto done;
      }
      case ARM64in_VCvtHD: {
         /* 31         23 21    16  14    9 4
            000,11110, 11 10001 0,1 10000 n d   FCVT Dd, Hn (H->D)
            ---------- 01 ----- 1,1 ---------   FCVT Hd, Dn (D->H)
            Rounding, when dst is smaller than src, is per the FPCR.
         */
         UInt dd = dregEnc(i->ARM64in.VCvtHD.dst);
         UInt nn = dregEnc(i->ARM64in.VCvtHD.src);
         if (i->ARM64in.VCvtHD.hToD) {
            *p++ = X_3_5_8_6_5_5(X000, X11110, X11100010, X110000, nn, dd);
         } else {
            *p++ = X_3_5_8_6_5_5(X000, X11110, X01100011, X110000, nn, dd);
         }
         goto done;
      }
      case ARM64in_VUnaryD: {
         /* 31        23 21     16 14    9 4
            000,11110 01 1,0000 0,0 10000 n d  FMOV Dd, Dn (not handled)
            ------------------- 0,1 ---------  FABS ------
            ------------------- 1,0 ---------  FNEG ------
            ------------------- 1,1 ---------  FSQRT -----
         */
         UInt dD  = dregEnc(i->ARM64in.VUnaryD.dst);
         UInt dN  = dregEnc(i->ARM64in.VUnaryD.src);
         UInt b16 = 2; /* impossible */
         UInt b15 = 2; /* impossible */
         switch (i->ARM64in.VUnaryD.op) {
            case ARM64fpu_NEG:  b16 = 1; b15 = 0; break;
            case ARM64fpu_SQRT: b16 = 1; b15 = 1; break;
            case ARM64fpu_ABS:  b16 = 0; b15 = 1; break;
            default: break;
         }
         if (b16 < 2 && b15 < 2) {
            *p++ = X_3_8_5_6_5_5(X000, X11110011, (X0000 << 1) | b16,
                                 (b15 << 5) | X10000, dN, dD);
            goto done;
         }
         /* 
            000, 11110 01 1,001 11,1 10000 n d  FRINTI Dd, Dm (round per FPCR)
         */
         if (i->ARM64in.VUnaryD.op == ARM64fpu_RINT) {
           *p++ = X_3_8_5_6_5_5(X000, X11110011, X00111, X110000, dN, dD);
           goto done;
         }
         /*
            000, 11110 01 1,001 10,0 10000 n d  FRINTA Dd, Dm (round away from zero)
         */
         if (i->ARM64in.VUnaryD.op == ARM64fpu_RINTA0) {
           *p++ = X_3_8_5_6_5_5(X000, X11110011, X00110, X010000, dN, dD);
           goto done;
         }
         /*
            000, 11110 01 1,001 10,0 10000 n d  FRINTN Dd, Dm (round to even)
         */
         if (i->ARM64in.VUnaryD.op == ARM64fpu_RINTE) {
           *p++ = X_3_8_5_6_5_5(X000, X11110011, X00100, X010000, dN, dD);
           goto done;
         }
         /*
            010, 11110 11 1,0000 1,1111 10 n d  FRECPX Dd, Dm
         */
         if (i->ARM64in.VUnaryD.op == ARM64fpu_RECPX) {
           *p++ = X_3_8_5_6_5_5(X010, X11110111, X00001, X111110, dN, dD);
           goto done;
         }
         goto bad;
      }
      case ARM64in_VUnaryS: {
         /* 31        23 21     16 14    9 4
            000,11110 00 1,0000 0,0 10000 n d  FMOV Sd, Sn (not handled)
            ------------------- 0,1 ---------  FABS ------
            ------------------- 1,0 ---------  FNEG ------
            ------------------- 1,1 ---------  FSQRT -----
         */
         UInt sD  = dregEnc(i->ARM64in.VUnaryS.dst);
         UInt sN  = dregEnc(i->ARM64in.VUnaryS.src);
         UInt b16 = 2; /* impossible */
         UInt b15 = 2; /* impossible */
         switch (i->ARM64in.VUnaryS.op) {
            case ARM64fpu_NEG:  b16 = 1; b15 = 0; break;
            case ARM64fpu_SQRT: b16 = 1; b15 = 1; break;
            case ARM64fpu_ABS:  b16 = 0; b15 = 1; break;
            default: break;
         }
         if (b16 < 2 && b15 < 2) {
            *p++ = X_3_8_5_6_5_5(X000, X11110001, (X0000 << 1) | b16,
                                 (b15 << 5) | X10000, sN, sD);
            goto done;
         }
         /* 
            000, 11110 00 1,001 11,1 10000 n d  FRINTI Sd, Sm (round per FPCR)
         */
         if (i->ARM64in.VUnaryS.op == ARM64fpu_RINT) {
           *p++ = X_3_8_5_6_5_5(X000, X11110001, X00111, X110000, sN, sD);
           goto done;
         }
         /*
            000, 11110 00 1,001 11,1 10000 n d  FRINTA Sd, Sm (round away from zero)
         */
         if (i->ARM64in.VUnaryS.op == ARM64fpu_RINTA0) {
           *p++ = X_3_8_5_6_5_5(X000, X11110001, X00110, X010000, sN, sD);
           goto done;
         }
         /*
            000, 11110 00 1,001 11,1 10000 n d  FRINTN Sd, Sm (round to even)
         */
         if (i->ARM64in.VUnaryS.op == ARM64fpu_RINTE) {
           *p++ = X_3_8_5_6_5_5(X000, X11110001, X00100, X010000, sN, sD);
           goto done;
         }
         /*
            010, 11110 10 1,0000 1,1111 10 n d  FRECPX Sd, Sm
         */
         if (i->ARM64in.VUnaryS.op == ARM64fpu_RECPX) {
           *p++ = X_3_8_5_6_5_5(X010, X11110101, X00001, X111110, sN, sD);
           goto done;
         }
         goto bad;
      }
      case ARM64in_VUnaryH: {
         /* 31        23 21     16  14    9 4
            000 11110 11 1 0000 0,1 10000 n d  FABS Hd, Hn
            ------------------- 1,0 ---------  FNEG Hd, Hn
            ------------------- 1,1 ---------  FSQRT Hd, Hn
         */
         UInt hD  = dregEnc(i->ARM64in.VUnaryH.dst);
         UInt hN  = dregEnc(i->ARM64in.VUnaryH.src);
         /* opc field (bits 15 and 16) */
         UInt b16 = 2; /* impossible */
         UInt b15 = 2; /* impossible */
         switch (i->ARM64in.VUnaryH.op) {
            case ARM64fpu_NEG:  b16 = 1; b15 = 0; break;
            case ARM64fpu_SQRT: b16 = 1; b15 = 1; break;
            case ARM64fpu_ABS:  b16 = 0; b15 = 1; break;
            default: break;
         }
         /*
            000, 11110 11 1,0000 01,10000 n d   FABS Hd, Hn
            ---, ----- -- -,---- 10,----- n d   FNEG Hd, Hn
            ---, ----- -- -,---- 11,----- n d   FSQRT Hd, Hn
         */
         if (b16 < 2 && b15 < 2) {
            *p++ = X_3_8_5_6_5_5(X000, X11110111, (X0000 << 1) | b16,
                                 (b15 << 5) | X10000, hN, hD);
            goto done;
         }
         goto bad;
      }
      case ARM64in_VBinD: {
         /* 31        23  20 15   11 9 4
            ---------------- 0000 ------   FMUL  --------
            000 11110 011 m  0001 10 n d   FDIV  Dd,Dn,Dm
            ---------------- 0010 ------   FADD  --------
            ---------------- 0011 ------   FSUB  --------
         */
         UInt dD = dregEnc(i->ARM64in.VBinD.dst);
         UInt dN = dregEnc(i->ARM64in.VBinD.argL);
         UInt dM = dregEnc(i->ARM64in.VBinD.argR);
         UInt b1512 = 16; /* impossible */
         switch (i->ARM64in.VBinD.op) {
            case ARM64fpb_DIV: b1512 = X0001; break;
            case ARM64fpb_MUL: b1512 = X0000; break;
            case ARM64fpb_SUB: b1512 = X0011; break;
            case ARM64fpb_ADD: b1512 = X0010; break;
            default: goto bad;
         }
         vassert(b1512 < 16);
         *p++
            = X_3_8_5_6_5_5(X000, X11110011, dM, (b1512 << 2) | X10, dN, dD);
         goto done;
      }
      case ARM64in_VBinS: {
         /* 31        23  20 15   11 9 4
            ---------------- 0000 ------   FMUL  --------
            000 11110 001 m  0001 10 n d   FDIV  Dd,Dn,Dm
            ---------------- 0010 ------   FADD  --------
            ---------------- 0011 ------   FSUB  --------
         */
         UInt sD = dregEnc(i->ARM64in.VBinS.dst);
         UInt sN = dregEnc(i->ARM64in.VBinS.argL);
         UInt sM = dregEnc(i->ARM64in.VBinS.argR);
         UInt b1512 = 16; /* impossible */
         switch (i->ARM64in.VBinS.op) {
            case ARM64fpb_DIV: b1512 = X0001; break;
            case ARM64fpb_MUL: b1512 = X0000; break;
            case ARM64fpb_SUB: b1512 = X0011; break;
            case ARM64fpb_ADD: b1512 = X0010; break;
            default: goto bad;
         }
         vassert(b1512 < 16);
         *p++
            = X_3_8_5_6_5_5(X000, X11110001, sM, (b1512 << 2) | X10, sN, sD);
         goto done;
      }
      case ARM64in_VBinH: {
         /* 31        23  20 15   11 9 4
            000 11110 111 m  0010 10 n d   FADD  Hd,Hn,Hm
            000 11110 111 m  0011 10 n d   FSUB  Hd,Hn,Hm
         */
         UInt hD = dregEnc(i->ARM64in.VBinH.dst);
         UInt hN = dregEnc(i->ARM64in.VBinH.argL);
         UInt hM = dregEnc(i->ARM64in.VBinH.argR);
         UInt b1512 = 16; /* impossible */
         switch (i->ARM64in.VBinH.op) {
            case ARM64fpb_ADD: b1512 = X0010; break;
            case ARM64fpb_SUB: b1512 = X0011; break;
            default: goto bad;
         }
         vassert(b1512 < 16);
         *p++
            = X_3_8_5_6_5_5(X000, X11110111, hM, (b1512 << 2) | X10, hN, hD);
         goto done;
      }
      case ARM64in_VTriD: {
         /* 31            20 15 14 9 4
            000 11111 010 m  0  a  n d FMADD  Dd,Dn,Dm,Da
            ---------------- 1  ------ FMSUB  -----------
         */
         UInt dD = dregEnc(i->ARM64in.VTriD.dst);
         UInt dN = dregEnc(i->ARM64in.VTriD.arg1);
         UInt dM = dregEnc(i->ARM64in.VTriD.arg2);
         UInt dA = dregEnc(i->ARM64in.VTriD.arg3);
         UInt b15 = 2; /* impossible */
         switch (i->ARM64in.VTriD.op) {
            case ARM64fpt_FMADD: b15 = 0; break;
            case ARM64fpt_FMSUB: b15 = 1; break;
            default: goto bad;
         }
         vassert(b15 < 2);
         *p++ = X_3_8_5_1_5_5_5(X000, X11111010, dM, b15, dA, dN, dD);
         goto done;
      }
      case ARM64in_VTriS: {
         /* 31            20 15 14 9 4
            000 11111 000 m  0  a  n d FMADD  Dd,Dn,Dm,Da
            ---------------- 1  ------ FMSUB  -----------
         */
         UInt dD = dregEnc(i->ARM64in.VTriD.dst);
         UInt dN = dregEnc(i->ARM64in.VTriD.arg1);
         UInt dM = dregEnc(i->ARM64in.VTriD.arg2);
         UInt dA = dregEnc(i->ARM64in.VTriD.arg3);
         UInt b15 = 2; /* impossible */
         switch (i->ARM64in.VTriD.op) {
            case ARM64fpt_FMADD: b15 = 0; break;
            case ARM64fpt_FMSUB: b15 = 1; break;
            default: goto bad;
         }
         vassert(b15 < 2);
         *p++ = X_3_8_5_1_5_5_5(X000, X11111000, dM, b15, dA, dN, dD);
         goto done;
      }
      case ARM64in_VCmpD: {
         /* 000 11110 01 1 m 00 1000 n 00 000  FCMP Dn, Dm */
         UInt dN = dregEnc(i->ARM64in.VCmpD.argL);
         UInt dM = dregEnc(i->ARM64in.VCmpD.argR);
         *p++ = X_3_8_5_6_5_5(X000, X11110011, dM, X001000, dN, X00000);
         goto done;
      }
      case ARM64in_VCmpS: {
         /* 000 11110 00 1 m 00 1000 n 00 000  FCMP Sn, Sm */
         UInt sN = dregEnc(i->ARM64in.VCmpS.argL);
         UInt sM = dregEnc(i->ARM64in.VCmpS.argR);
         *p++ = X_3_8_5_6_5_5(X000, X11110001, sM, X001000, sN, X00000);
         goto done;
      }
      case ARM64in_VCmpH: {
         /* 000 11110 11 1 m 00 1000 n 00 000  FCMP Hn, Hm */
         UInt hN = dregEnc(i->ARM64in.VCmpH.argL);
         UInt hM = dregEnc(i->ARM64in.VCmpH.argR);
         *p++ = X_3_8_5_6_5_5(X000, X11110111, hM, X001000, hN, X00000);
         goto done;
      }
      case ARM64in_VFCSel: {
         /* 31        23 21 20 15   11 9 5
            000 11110 00 1  m  cond 11 n d  FCSEL Sd,Sn,Sm,cond
            000 11110 01 1  m  cond 11 n d  FCSEL Dd,Dn,Dm,cond
         */
         Bool isD  = i->ARM64in.VFCSel.isD;
         UInt dd   = dregEnc(i->ARM64in.VFCSel.dst);
         UInt nn   = dregEnc(i->ARM64in.VFCSel.argL);
         UInt mm   = dregEnc(i->ARM64in.VFCSel.argR);
         UInt cond = (UInt)i->ARM64in.VFCSel.cond;
         vassert(cond < 16);
         *p++ = X_3_8_5_6_5_5(X000, isD ? X11110011 : X11110001,
                              mm, (cond << 2) | X000011, nn, dd);
         goto done; 
      }
      case ARM64in_FPCR: {
         Bool toFPCR = i->ARM64in.FPCR.toFPCR;
         UInt iReg   = iregEnc(i->ARM64in.FPCR.iReg);
         if (toFPCR) {
            /* 0xD51B44 000 Rt  MSR fpcr, rT */
            *p++ = 0xD51B4400 | (iReg & 0x1F);
            goto done;
         }
         goto bad; // FPCR -> iReg case currently ATC
      }
      case ARM64in_FPSR: {
         Bool toFPSR = i->ARM64in.FPSR.toFPSR;
         UInt iReg   = iregEnc(i->ARM64in.FPSR.iReg);
         if (toFPSR) {
            /* 0xD51B44 001 Rt  MSR fpsr, rT */
            *p++ = 0xD51B4420 | (iReg & 0x1F);
         } else {
            /* 0xD53B44 001 Rt  MRS rT, fpsr */
            *p++ = 0xD53B4420 | (iReg & 0x1F);
         }
         goto done;
      }
      case ARM64in_VBinV: {
         /* 31        23   20 15     9 4
            010 01110 11 1 m  100001 n d   ADD Vd.2d,  Vn.2d,  Vm.2d
            010 01110 10 1 m  100001 n d   ADD Vd.4s,  Vn.4s,  Vm.4s
            010 01110 01 1 m  100001 n d   ADD Vd.8h,  Vn.8h,  Vm.8h
            010 01110 00 1 m  100001 n d   ADD Vd.16b, Vn.16b, Vm.16b

            011 01110 11 1 m  100001 n d   SUB Vd.2d,  Vn.2d,  Vm.2d
            011 01110 10 1 m  100001 n d   SUB Vd.4s,  Vn.4s,  Vm.4s
            011 01110 01 1 m  100001 n d   SUB Vd.8h,  Vn.8h,  Vm.8h
            011 01110 00 1 m  100001 n d   SUB Vd.16b, Vn.16b, Vm.16b

            010 01110 10 1 m  100111 n d   MUL Vd.4s,  Vn.4s,  Vm.4s
            010 01110 01 1 m  100111 n d   MUL Vd.8h,  Vn.8h,  Vm.8h
            010 01110 00 1 m  100111 n d   MUL Vd.16b, Vn.16b, Vm.16b

            010 01110 01 1 m  110101 n d   FADD Vd.2d, Vn.2d, Vm.2d
            010 01110 00 1 m  110101 n d   FADD Vd.4s, Vn.4s, Vm.4s
            010 01110 11 1 m  110101 n d   FSUB Vd.2d, Vn.2d, Vm.2d
            010 01110 10 1 m  110101 n d   FSUB Vd.4s, Vn.4s, Vm.4s

            011 01110 01 1 m  110111 n d   FMUL Vd.2d, Vn.2d, Vm.2d
            011 01110 00 1 m  110111 n d   FMUL Vd.4s, Vn.4s, Vm.4s
            011 01110 01 1 m  111111 n d   FDIV Vd.2d, Vn.2d, Vm.2d
            011 01110 00 1 m  111111 n d   FDIV Vd.4s, Vn.4s, Vm.4s

            010 01110 01 1 m  111101 n d   FMAX Vd.2d, Vn.2d, Vm.2d
            010 01110 00 1 m  111101 n d   FMAX Vd.4s, Vn.4s, Vm.4s
            010 01110 11 1 m  111101 n d   FMIN Vd.2d, Vn.2d, Vm.2d
            010 01110 10 1 m  111101 n d   FMIN Vd.4s, Vn.4s, Vm.4s

            011 01110 10 1 m  011001 n d   UMAX Vd.4s,  Vn.4s,  Vm.4s
            011 01110 01 1 m  011001 n d   UMAX Vd.8h,  Vn.8h,  Vm.8h
            011 01110 00 1 m  011001 n d   UMAX Vd.16b, Vn.16b, Vm.16b

            011 01110 10 1 m  011011 n d   UMIN Vd.4s,  Vn.4s,  Vm.4s
            011 01110 01 1 m  011011 n d   UMIN Vd.8h,  Vn.8h,  Vm.8h
            011 01110 00 1 m  011011 n d   UMIN Vd.16b, Vn.16b, Vm.16b

            010 01110 10 1 m  011001 n d   SMAX Vd.4s,  Vn.4s,  Vm.4s
            010 01110 01 1 m  011001 n d   SMAX Vd.8h,  Vn.8h,  Vm.8h
            010 01110 00 1 m  011001 n d   SMAX Vd.16b, Vn.16b, Vm.16b

            010 01110 10 1 m  011011 n d   SMIN Vd.4s,  Vn.4s,  Vm.4s
            010 01110 01 1 m  011011 n d   SMIN Vd.8h,  Vn.8h,  Vm.8h
            010 01110 00 1 m  011011 n d   SMIN Vd.16b, Vn.16b, Vm.16b

            010 01110 00 1 m  000111 n d   AND Vd, Vn, Vm
            010 01110 10 1 m  000111 n d   ORR Vd, Vn, Vm
            011 01110 00 1 m  000111 n d   EOR Vd, Vn, Vm

            011 01110 11 1 m  100011 n d   CMEQ Vd.2d,  Vn.2d,  Vm.2d
            011 01110 10 1 m  100011 n d   CMEQ Vd.4s,  Vn.4s,  Vm.4s
            011 01110 01 1 m  100011 n d   CMEQ Vd.8h,  Vn.8h,  Vm.8h
            011 01110 00 1 m  100011 n d   CMEQ Vd.16b, Vn.16b, Vm.16b

            011 01110 11 1 m  001101 n d   CMHI Vd.2d,  Vn.2d,  Vm.2d
            011 01110 10 1 m  001101 n d   CMHI Vd.4s,  Vn.4s,  Vm.4s
            011 01110 01 1 m  001101 n d   CMHI Vd.8h,  Vn.8h,  Vm.8h
            011 01110 00 1 m  001101 n d   CMHI Vd.16b, Vn.16b, Vm.16b

            010 01110 11 1 m  001101 n d   CMGT Vd.2d,  Vn.2d,  Vm.2d
            010 01110 10 1 m  001101 n d   CMGT Vd.4s,  Vn.4s,  Vm.4s
            010 01110 01 1 m  001101 n d   CMGT Vd.8h,  Vn.8h,  Vm.8h
            010 01110 00 1 m  001101 n d   CMGT Vd.16b, Vn.16b, Vm.16b

            010 01110 01 1 m  111001 n d   FCMEQ Vd.2d, Vn.2d, Vm.2d
            010 01110 00 1 m  111001 n d   FCMEQ Vd.4s, Vn.4s, Vm.4s

            011 01110 01 1 m  111001 n d   FCMGE Vd.2d, Vn.2d, Vm.2d
            011 01110 00 1 m  111001 n d   FCMGE Vd.4s, Vn.4s, Vm.4s

            011 01110 11 1 m  111001 n d   FCMGT Vd.2d, Vn.2d, Vm.2d
            011 01110 10 1 m  111001 n d   FCMGT Vd.4s, Vn.4s, Vm.4s

            010 01110 00 0 m  000000 n d   TBL Vd.16b, {Vn.16b}, Vm.16b

            010 01110 11 0 m  000110 n d   UZP1 Vd.2d,  Vn.2d,  Vm.2d
            010 01110 10 0 m  000110 n d   UZP1 Vd.4s,  Vn.4s,  Vm.4s
            010 01110 01 0 m  000110 n d   UZP1 Vd.8h,  Vn.8h,  Vm.8h
            010 01110 00 0 m  000110 n d   UZP1 Vd.16b, Vn.16b, Vm.16b

            010 01110 11 0 m  010110 n d   UZP2 Vd.2d,  Vn.2d,  Vm.2d
            010 01110 10 0 m  010110 n d   UZP2 Vd.4s,  Vn.4s,  Vm.4s
            010 01110 01 0 m  010110 n d   UZP2 Vd.8h,  Vn.8h,  Vm.8h
            010 01110 00 0 m  010110 n d   UZP2 Vd.16b, Vn.16b, Vm.16b

            010 01110 10 0 m  001110 n d   ZIP1 Vd.4s,  Vn.4s,  Vm.4s
            010 01110 01 0 m  001110 n d   ZIP1 Vd.8h,  Vn.8h,  Vm.8h
            010 01110 10 0 m  001110 n d   ZIP1 Vd.16b, Vn.16b, Vm.16b

            010 01110 10 0 m  011110 n d   ZIP2 Vd.4s,  Vn.4s,  Vm.4s
            010 01110 01 0 m  011110 n d   ZIP2 Vd.8h,  Vn.8h,  Vm.8h
            010 01110 10 0 m  011110 n d   ZIP2 Vd.16b, Vn.16b, Vm.16b

            011 01110 00 1 m  100111 n d   PMUL Vd.16b, Vn.16b, Vm.16b

            000 01110 00 1 m  111000 n d   PMULL Vd.8h, Vn.8b, Vm.8b

            001 01110 10 1 m  110000 n d   UMULL Vd.2d, Vn.2s, Vm.2s
            001 01110 01 1 m  110000 n d   UMULL Vd.4s, Vn.4h, Vm.4h
            001 01110 00 1 m  110000 n d   UMULL Vd.8h, Vn.8b, Vm.8b

            000 01110 10 1 m  110000 n d   SMULL Vd.2d, Vn.2s, Vm.2s
            000 01110 01 1 m  110000 n d   SMULL Vd.4s, Vn.4h, Vm.4h
            000 01110 00 1 m  110000 n d   SMULL Vd.8h, Vn.8b, Vm.8b

            010 01110 11 1 m  000011 n d   SQADD Vd.2d,  Vn.2d,  Vm.2d
            010 01110 10 1 m  000011 n d   SQADD Vd.4s,  Vn.4s,  Vm.4s
            010 01110 01 1 m  000011 n d   SQADD Vd.8h,  Vn.8h,  Vm.8h
            010 01110 00 1 m  000011 n d   SQADD Vd.16b, Vn.16b, Vm.16b

            011 01110 11 1 m  000011 n d   UQADD Vd.2d,  Vn.2d,  Vm.2d
            011 01110 10 1 m  000011 n d   UQADD Vd.4s,  Vn.4s,  Vm.4s
            011 01110 01 1 m  000011 n d   UQADD Vd.8h,  Vn.8h,  Vm.8h
            011 01110 00 1 m  000011 n d   UQADD Vd.16b, Vn.16b, Vm.16b

            010 01110 11 1 m  001011 n d   SQSUB Vd.2d,  Vn.2d,  Vm.2d
            010 01110 10 1 m  001011 n d   SQSUB Vd.4s,  Vn.4s,  Vm.4s
            010 01110 01 1 m  001011 n d   SQSUB Vd.8h,  Vn.8h,  Vm.8h
            010 01110 00 1 m  001011 n d   SQSUB Vd.16b, Vn.16b, Vm.16b

            011 01110 11 1 m  001011 n d   UQSUB Vd.2d,  Vn.2d,  Vm.2d
            011 01110 10 1 m  001011 n d   UQSUB Vd.4s,  Vn.4s,  Vm.4s
            011 01110 01 1 m  001011 n d   UQSUB Vd.8h,  Vn.8h,  Vm.8h
            011 01110 00 1 m  001011 n d   UQSUB Vd.16b, Vn.16b, Vm.16b

            000 01110 10 1 m  110100 n d   SQDMULL Vd.2d, Vn.2s, Vm.2s
            000 01110 01 1 m  110100 n d   SQDMULL Vd.4s, Vn.4h, Vm.4h

            010 01110 10 1 m  101101 n d   SQDMULH   Vd.4s,  Vn.4s,  Vm.4s
            010 01110 01 1 m  101101 n d   SQDMULH   Vd.8h,  Vn.8h,  Vm.8h
            011 01110 10 1 m  101101 n d   SQRDMULH  Vd.4s,  Vn.4s,  Vm.4s
            011 01110 10 1 m  101101 n d   SQRDMULH  Vd.8h,  Vn.8h,  Vm.8h

            010 01110 sz 1 m  010011 n d   SQSHL@sz   Vd, Vn, Vm
            010 01110 sz 1 m  010111 n d   SQRSHL@sz  Vd, Vn, Vm
            011 01110 sz 1 m  010011 n d   UQSHL@sz   Vd, Vn, Vm
            011 01110 sz 1 m  010111 n d   URQSHL@sz  Vd, Vn, Vm

            010 01110 sz 1 m  010001 n d   SSHL@sz   Vd, Vn, Vm
            010 01110 sz 1 m  010101 n d   SRSHL@sz  Vd, Vn, Vm
            011 01110 sz 1 m  010001 n d   USHL@sz   Vd, Vn, Vm
            011 01110 sz 1 m  010101 n d   URSHL@sz  Vd, Vn, Vm

            010 01110 01 1 m  111111 n d   FRECPS  Vd.2d, Vn.2d, Vm.2d
            010 01110 00 1 m  111111 n d   FRECPS  Vd.4s, Vn.4s, Vm.4s
            010 01110 11 1 m  111111 n d   FRSQRTS Vd.2d, Vn.2d, Vm.2d
            010 01110 10 1 m  111111 n d   FRSQRTS Vd.4s, Vn.4s, Vm.4s
         */
         UInt vD = qregEnc(i->ARM64in.VBinV.dst);
         UInt vN = qregEnc(i->ARM64in.VBinV.argL);
         UInt vM = qregEnc(i->ARM64in.VBinV.argR);
         switch (i->ARM64in.VBinV.op) {
            case ARM64vecb_ADD64x2:
               *p++ = X_3_8_5_6_5_5(X010, X01110111, vM, X100001, vN, vD);
               break;
            case ARM64vecb_ADD32x4:
               *p++ = X_3_8_5_6_5_5(X010, X01110101, vM, X100001, vN, vD);
               break;
            case ARM64vecb_ADD16x8:
               *p++ = X_3_8_5_6_5_5(X010, X01110011, vM, X100001, vN, vD);
               break;
            case ARM64vecb_ADD8x16:
               *p++ = X_3_8_5_6_5_5(X010, X01110001, vM, X100001, vN, vD);
               break;
            case ARM64vecb_SUB64x2:
               *p++ = X_3_8_5_6_5_5(X011, X01110111, vM, X100001, vN, vD);
               break;
            case ARM64vecb_SUB32x4:
               *p++ = X_3_8_5_6_5_5(X011, X01110101, vM, X100001, vN, vD);
               break;
            case ARM64vecb_SUB16x8:
               *p++ = X_3_8_5_6_5_5(X011, X01110011, vM, X100001, vN, vD);
               break;
            case ARM64vecb_SUB8x16:
               *p++ = X_3_8_5_6_5_5(X011, X01110001, vM, X100001, vN, vD);
               break;
            case ARM64vecb_MUL32x4:
               *p++ = X_3_8_5_6_5_5(X010, X01110101, vM, X100111, vN, vD);
               break;
            case ARM64vecb_MUL16x8:
               *p++ = X_3_8_5_6_5_5(X010, X01110011, vM, X100111, vN, vD);
               break;
            case ARM64vecb_MUL8x16:
               *p++ = X_3_8_5_6_5_5(X010, X01110001, vM, X100111, vN, vD);
               break;
            case ARM64vecb_FADD64x2:
               *p++ = X_3_8_5_6_5_5(X010, X01110011, vM, X110101, vN, vD);
               break;
            case ARM64vecb_FADD32x4:
               *p++ = X_3_8_5_6_5_5(X010, X01110001, vM, X110101, vN, vD);
               break;
            case ARM64vecb_FADD16x8:
               *p++ = X_3_8_5_6_5_5(X010, X01110010, vM, X000101, vN, vD);
               break;
            case ARM64vecb_FSUB64x2:
               *p++ = X_3_8_5_6_5_5(X010, X01110111, vM, X110101, vN, vD);
               break;
            case ARM64vecb_FSUB32x4:
               *p++ = X_3_8_5_6_5_5(X010, X01110101, vM, X110101, vN, vD);
               break;
            case ARM64vecb_FSUB16x8:
               *p++ = X_3_8_5_6_5_5(X010, X01110110, vM, X000101, vN, vD);
               break;
            case ARM64vecb_FMUL64x2:
               *p++ = X_3_8_5_6_5_5(X011, X01110011, vM, X110111, vN, vD);
               break;
            case ARM64vecb_FMUL32x4:
               *p++ = X_3_8_5_6_5_5(X011, X01110001, vM, X110111, vN, vD);
               break;
            case ARM64vecb_FDIV64x2:
               *p++ = X_3_8_5_6_5_5(X011, X01110011, vM, X111111, vN, vD);
               break;
            case ARM64vecb_FDIV32x4:
               *p++ = X_3_8_5_6_5_5(X011, X01110001, vM, X111111, vN, vD);
               break;

            case ARM64vecb_FMAX64x2:
               *p++ = X_3_8_5_6_5_5(X010, X01110011, vM, X111101, vN, vD);
               break;
            case ARM64vecb_FMAX32x4:
               *p++ = X_3_8_5_6_5_5(X010, X01110001, vM, X111101, vN, vD);
               break;
            case ARM64vecb_FMIN64x2:
               *p++ = X_3_8_5_6_5_5(X010, X01110111, vM, X111101, vN, vD);
               break;
            case ARM64vecb_FMIN32x4:
               *p++ = X_3_8_5_6_5_5(X010, X01110101, vM, X111101, vN, vD);
               break;

            case ARM64vecb_UMAX32x4:
               *p++ = X_3_8_5_6_5_5(X011, X01110101, vM, X011001, vN, vD);
               break;
            case ARM64vecb_UMAX16x8:
               *p++ = X_3_8_5_6_5_5(X011, X01110011, vM, X011001, vN, vD);
               break;
            case ARM64vecb_UMAX8x16:
               *p++ = X_3_8_5_6_5_5(X011, X01110001, vM, X011001, vN, vD);
               break;

            case ARM64vecb_UMIN32x4:
               *p++ = X_3_8_5_6_5_5(X011, X01110101, vM, X011011, vN, vD);
               break;
            case ARM64vecb_UMIN16x8:
               *p++ = X_3_8_5_6_5_5(X011, X01110011, vM, X011011, vN, vD);
               break;
            case ARM64vecb_UMIN8x16:
               *p++ = X_3_8_5_6_5_5(X011, X01110001, vM, X011011, vN, vD);
               break;

            case ARM64vecb_SMAX32x4:
               *p++ = X_3_8_5_6_5_5(X010, X01110101, vM, X011001, vN, vD);
               break;
            case ARM64vecb_SMAX16x8:
               *p++ = X_3_8_5_6_5_5(X010, X01110011, vM, X011001, vN, vD);
               break;
            case ARM64vecb_SMAX8x16:
               *p++ = X_3_8_5_6_5_5(X010, X01110001, vM, X011001, vN, vD);
               break;

            case ARM64vecb_SMIN32x4:
               *p++ = X_3_8_5_6_5_5(X010, X01110101, vM, X011011, vN, vD);
               break;
            case ARM64vecb_SMIN16x8:
               *p++ = X_3_8_5_6_5_5(X010, X01110011, vM, X011011, vN, vD);
               break;
            case ARM64vecb_SMIN8x16:
               *p++ = X_3_8_5_6_5_5(X010, X01110001, vM, X011011, vN, vD);
               break;

            case ARM64vecb_AND:
               *p++ = X_3_8_5_6_5_5(X010, X01110001, vM, X000111, vN, vD);
               break;
            case ARM64vecb_ORR:
               *p++ = X_3_8_5_6_5_5(X010, X01110101, vM, X000111, vN, vD);
               break;
            case ARM64vecb_XOR:
               *p++ = X_3_8_5_6_5_5(X011, X01110001, vM, X000111, vN, vD);
               break;

            case ARM64vecb_CMEQ64x2:
               *p++ = X_3_8_5_6_5_5(X011, X01110111, vM, X100011, vN, vD);
               break;
            case ARM64vecb_CMEQ32x4:
               *p++ = X_3_8_5_6_5_5(X011, X01110101, vM, X100011, vN, vD);
               break;
            case ARM64vecb_CMEQ16x8:
               *p++ = X_3_8_5_6_5_5(X011, X01110011, vM, X100011, vN, vD);
               break;
            case ARM64vecb_CMEQ8x16:
               *p++ = X_3_8_5_6_5_5(X011, X01110001, vM, X100011, vN, vD);
               break;

            case ARM64vecb_CMHI64x2:
               *p++ = X_3_8_5_6_5_5(X011, X01110111, vM,  X001101, vN, vD);
               break;
            case ARM64vecb_CMHI32x4:
               *p++ = X_3_8_5_6_5_5(X011, X01110101, vM,  X001101, vN, vD);
               break;
            case ARM64vecb_CMHI16x8:
               *p++ = X_3_8_5_6_5_5(X011, X01110011, vM,  X001101, vN, vD);
               break;
            case ARM64vecb_CMHI8x16:
               *p++ = X_3_8_5_6_5_5(X011, X01110001, vM,  X001101, vN, vD);
               break;

            case ARM64vecb_CMGT64x2:
               *p++ = X_3_8_5_6_5_5(X010, X01110111, vM,  X001101, vN, vD);
               break;
            case ARM64vecb_CMGT32x4:
               *p++ = X_3_8_5_6_5_5(X010, X01110101, vM,  X001101, vN, vD);
               break;
            case ARM64vecb_CMGT16x8:
               *p++ = X_3_8_5_6_5_5(X010, X01110011, vM,  X001101, vN, vD);
               break;
            case ARM64vecb_CMGT8x16:
               *p++ = X_3_8_5_6_5_5(X010, X01110001, vM,  X001101, vN, vD);
               break;

            case ARM64vecb_FCMEQ64x2:
               *p++ = X_3_8_5_6_5_5(X010, X01110011, vM, X111001, vN, vD);
               break;
            case ARM64vecb_FCMEQ32x4:
               *p++ = X_3_8_5_6_5_5(X010, X01110001, vM, X111001, vN, vD);
               break;
            case ARM64vecb_FCMEQ16x8:
               *p++ = X_3_8_5_6_5_5(X010, X11110010, vM, X001001, vN, vD);
               break;

            case ARM64vecb_FCMGE64x2:
               *p++ = X_3_8_5_6_5_5(X011, X01110011, vM, X111001, vN, vD);
               break;
            case ARM64vecb_FCMGE32x4:
               *p++ = X_3_8_5_6_5_5(X011, X01110001, vM, X111001, vN, vD);
               break;
            case ARM64vecb_FCMGE16x8:
               *p++ = X_3_8_5_6_5_5(X011, X01110010, vM, X001001, vN, vD);
               break;

            case ARM64vecb_FCMGT64x2:
               *p++ = X_3_8_5_6_5_5(X011, X01110111, vM, X111001, vN, vD);
               break;
            case ARM64vecb_FCMGT32x4:
               *p++ = X_3_8_5_6_5_5(X011, X01110101, vM, X111001, vN, vD);
               break;
            case ARM64vecb_FCMGT16x8:
               *p++ = X_3_8_5_6_5_5(X011, X01110110, vM, X001001, vN, vD);
               break;

            case ARM64vecb_TBL1:
               *p++ = X_3_8_5_6_5_5(X010, X01110000, vM, X000000, vN, vD);
               break;

            case ARM64vecb_UZP164x2:
               *p++ = X_3_8_5_6_5_5(X010, X01110110, vM, X000110, vN, vD);
               break;
            case ARM64vecb_UZP132x4:
               *p++ = X_3_8_5_6_5_5(X010, X01110100, vM, X000110, vN, vD);
               break;
            case ARM64vecb_UZP116x8:
               *p++ = X_3_8_5_6_5_5(X010, X01110010, vM, X000110, vN, vD);
               break;
            case ARM64vecb_UZP18x16:
               *p++ = X_3_8_5_6_5_5(X010, X01110000, vM, X000110, vN, vD);
               break;

            case ARM64vecb_UZP264x2:
               *p++ = X_3_8_5_6_5_5(X010, X01110110, vM, X010110, vN, vD);
               break;
            case ARM64vecb_UZP232x4:
               *p++ = X_3_8_5_6_5_5(X010, X01110100, vM, X010110, vN, vD);
               break;
            case ARM64vecb_UZP216x8:
               *p++ = X_3_8_5_6_5_5(X010, X01110010, vM, X010110, vN, vD);
               break;
            case ARM64vecb_UZP28x16:
               *p++ = X_3_8_5_6_5_5(X010, X01110000, vM, X010110, vN, vD);
               break;

            case ARM64vecb_ZIP132x4:
               *p++ = X_3_8_5_6_5_5(X010, X01110100, vM, X001110, vN, vD);
               break;
            case ARM64vecb_ZIP116x8:
               *p++ = X_3_8_5_6_5_5(X010, X01110010, vM, X001110, vN, vD);
               break;
            case ARM64vecb_ZIP18x16:
               *p++ = X_3_8_5_6_5_5(X010, X01110000, vM, X001110, vN, vD);
               break;

            case ARM64vecb_ZIP232x4:
               *p++ = X_3_8_5_6_5_5(X010, X01110100, vM, X011110, vN, vD);
               break;
            case ARM64vecb_ZIP216x8:
               *p++ = X_3_8_5_6_5_5(X010, X01110010, vM, X011110, vN, vD);
               break;
            case ARM64vecb_ZIP28x16:
               *p++ = X_3_8_5_6_5_5(X010, X01110000, vM, X011110, vN, vD);
               break;

            case ARM64vecb_PMUL8x16:
               *p++ = X_3_8_5_6_5_5(X011, X01110001, vM, X100111, vN, vD);
               break;

            case ARM64vecb_PMULL8x8:
               *p++ = X_3_8_5_6_5_5(X000, X01110001, vM, X111000, vN, vD);
               break;

            case ARM64vecb_UMULL2DSS:
               *p++ = X_3_8_5_6_5_5(X001, X01110101, vM, X110000, vN, vD);
               break;
            case ARM64vecb_UMULL4SHH:
               *p++ = X_3_8_5_6_5_5(X001, X01110011, vM, X110000, vN, vD);
               break;
            case ARM64vecb_UMULL8HBB:
               *p++ = X_3_8_5_6_5_5(X001, X01110001, vM, X110000, vN, vD);
               break;

            case ARM64vecb_SMULL2DSS:
               *p++ = X_3_8_5_6_5_5(X000, X01110101, vM, X110000, vN, vD);
               break;
            case ARM64vecb_SMULL4SHH:
               *p++ = X_3_8_5_6_5_5(X000, X01110011, vM, X110000, vN, vD);
               break;
            case ARM64vecb_SMULL8HBB:
               *p++ = X_3_8_5_6_5_5(X000, X01110001, vM, X110000, vN, vD);
               break;

            case ARM64vecb_SQADD64x2:
               *p++ = X_3_8_5_6_5_5(X010, X01110111, vM, X000011, vN, vD);
               break;
            case ARM64vecb_SQADD32x4:
               *p++ = X_3_8_5_6_5_5(X010, X01110101, vM, X000011, vN, vD);
               break;
            case ARM64vecb_SQADD16x8:
               *p++ = X_3_8_5_6_5_5(X010, X01110011, vM, X000011, vN, vD);
               break;
            case ARM64vecb_SQADD8x16:
               *p++ = X_3_8_5_6_5_5(X010, X01110001, vM, X000011, vN, vD);
               break;

            case ARM64vecb_UQADD64x2:
               *p++ = X_3_8_5_6_5_5(X011, X01110111, vM, X000011, vN, vD);
               break;
            case ARM64vecb_UQADD32x4:
               *p++ = X_3_8_5_6_5_5(X011, X01110101, vM, X000011, vN, vD);
               break;
            case ARM64vecb_UQADD16x8:
               *p++ = X_3_8_5_6_5_5(X011, X01110011, vM, X000011, vN, vD);
               break;
            case ARM64vecb_UQADD8x16:
               *p++ = X_3_8_5_6_5_5(X011, X01110001, vM, X000011, vN, vD);
               break;

            case ARM64vecb_SQSUB64x2:
               *p++ = X_3_8_5_6_5_5(X010, X01110111, vM, X001011, vN, vD);
               break;
            case ARM64vecb_SQSUB32x4:
               *p++ = X_3_8_5_6_5_5(X010, X01110101, vM, X001011, vN, vD);
               break;
            case ARM64vecb_SQSUB16x8:
               *p++ = X_3_8_5_6_5_5(X010, X01110011, vM, X001011, vN, vD);
               break;
            case ARM64vecb_SQSUB8x16:
               *p++ = X_3_8_5_6_5_5(X010, X01110001, vM, X001011, vN, vD);
               break;

            case ARM64vecb_UQSUB64x2:
               *p++ = X_3_8_5_6_5_5(X011, X01110111, vM, X001011, vN, vD);
               break;
            case ARM64vecb_UQSUB32x4:
               *p++ = X_3_8_5_6_5_5(X011, X01110101, vM, X001011, vN, vD);
               break;
            case ARM64vecb_UQSUB16x8:
               *p++ = X_3_8_5_6_5_5(X011, X01110011, vM, X001011, vN, vD);
               break;
            case ARM64vecb_UQSUB8x16:
               *p++ = X_3_8_5_6_5_5(X011, X01110001, vM, X001011, vN, vD);
               break;

            case ARM64vecb_SQDMULL2DSS:
               *p++ = X_3_8_5_6_5_5(X000, X01110101, vM, X110100, vN, vD);
               break;
            case ARM64vecb_SQDMULL4SHH:
               *p++ = X_3_8_5_6_5_5(X000, X01110011, vM, X110100, vN, vD);
               break;

            case ARM64vecb_SQDMULH32x4:
               *p++ = X_3_8_5_6_5_5(X010, X01110101, vM, X101101, vN, vD);
               break;
            case ARM64vecb_SQDMULH16x8:
               *p++ = X_3_8_5_6_5_5(X010, X01110011, vM, X101101, vN, vD);
               break;
            case ARM64vecb_SQRDMULH32x4:
               *p++ = X_3_8_5_6_5_5(X011, X01110101, vM, X101101, vN, vD);
               break;
            case ARM64vecb_SQRDMULH16x8:
               *p++ = X_3_8_5_6_5_5(X011, X01110011, vM, X101101, vN, vD);
               break;

            case ARM64vecb_SQSHL64x2:
               *p++ = X_3_8_5_6_5_5(X010, X01110111, vM, X010011, vN, vD);
               break;
            case ARM64vecb_SQSHL32x4:
               *p++ = X_3_8_5_6_5_5(X010, X01110101, vM, X010011, vN, vD);
               break;
            case ARM64vecb_SQSHL16x8:
               *p++ = X_3_8_5_6_5_5(X010, X01110011, vM, X010011, vN, vD);
               break;
            case ARM64vecb_SQSHL8x16:
               *p++ = X_3_8_5_6_5_5(X010, X01110001, vM, X010011, vN, vD);
               break;

            case ARM64vecb_SQRSHL64x2:
               *p++ = X_3_8_5_6_5_5(X010, X01110111, vM, X010111, vN, vD);
               break;
            case ARM64vecb_SQRSHL32x4:
               *p++ = X_3_8_5_6_5_5(X010, X01110101, vM, X010111, vN, vD);
               break;
            case ARM64vecb_SQRSHL16x8:
               *p++ = X_3_8_5_6_5_5(X010, X01110011, vM, X010111, vN, vD);
               break;
            case ARM64vecb_SQRSHL8x16:
               *p++ = X_3_8_5_6_5_5(X010, X01110001, vM, X010111, vN, vD);
               break;

            case ARM64vecb_UQSHL64x2:
               *p++ = X_3_8_5_6_5_5(X011, X01110111, vM, X010011, vN, vD);
               break;
            case ARM64vecb_UQSHL32x4:
               *p++ = X_3_8_5_6_5_5(X011, X01110101, vM, X010011, vN, vD);
               break;
            case ARM64vecb_UQSHL16x8:
               *p++ = X_3_8_5_6_5_5(X011, X01110011, vM, X010011, vN, vD);
               break;
            case ARM64vecb_UQSHL8x16:
               *p++ = X_3_8_5_6_5_5(X011, X01110001, vM, X010011, vN, vD);
               break;

            case ARM64vecb_UQRSHL64x2:
               *p++ = X_3_8_5_6_5_5(X011, X01110111, vM, X010111, vN, vD);
               break;
            case ARM64vecb_UQRSHL32x4:
               *p++ = X_3_8_5_6_5_5(X011, X01110101, vM, X010111, vN, vD);
               break;
            case ARM64vecb_UQRSHL16x8:
               *p++ = X_3_8_5_6_5_5(X011, X01110011, vM, X010111, vN, vD);
               break;
            case ARM64vecb_UQRSHL8x16:
               *p++ = X_3_8_5_6_5_5(X011, X01110001, vM, X010111, vN, vD);
               break;

            case ARM64vecb_SSHL64x2:
               *p++ = X_3_8_5_6_5_5(X010, X01110111, vM, X010001, vN, vD);
               break;
            case ARM64vecb_SSHL32x4:
               *p++ = X_3_8_5_6_5_5(X010, X01110101, vM, X010001, vN, vD);
               break;
            case ARM64vecb_SSHL16x8:
               *p++ = X_3_8_5_6_5_5(X010, X01110011, vM, X010001, vN, vD);
               break;
            case ARM64vecb_SSHL8x16:
               *p++ = X_3_8_5_6_5_5(X010, X01110001, vM, X010001, vN, vD);
               break;

            case ARM64vecb_SRSHL64x2:
               *p++ = X_3_8_5_6_5_5(X010, X01110111, vM, X010101, vN, vD);
               break;
            case ARM64vecb_SRSHL32x4:
               *p++ = X_3_8_5_6_5_5(X010, X01110101, vM, X010101, vN, vD);
               break;
            case ARM64vecb_SRSHL16x8:
               *p++ = X_3_8_5_6_5_5(X010, X01110011, vM, X010101, vN, vD);
               break;
            case ARM64vecb_SRSHL8x16:
               *p++ = X_3_8_5_6_5_5(X010, X01110001, vM, X010101, vN, vD);
               break;

            case ARM64vecb_USHL64x2:
               *p++ = X_3_8_5_6_5_5(X011, X01110111, vM, X010001, vN, vD);
               break;
            case ARM64vecb_USHL32x4:
               *p++ = X_3_8_5_6_5_5(X011, X01110101, vM, X010001, vN, vD);
               break;
            case ARM64vecb_USHL16x8:
               *p++ = X_3_8_5_6_5_5(X011, X01110011, vM, X010001, vN, vD);
               break;
            case ARM64vecb_USHL8x16:
               *p++ = X_3_8_5_6_5_5(X011, X01110001, vM, X010001, vN, vD);
               break;

            case ARM64vecb_URSHL64x2:
               *p++ = X_3_8_5_6_5_5(X011, X01110111, vM, X010101, vN, vD);
               break;
            case ARM64vecb_URSHL32x4:
               *p++ = X_3_8_5_6_5_5(X011, X01110101, vM, X010101, vN, vD);
               break;
            case ARM64vecb_URSHL16x8:
               *p++ = X_3_8_5_6_5_5(X011, X01110011, vM, X010101, vN, vD);
               break;
            case ARM64vecb_URSHL8x16:
               *p++ = X_3_8_5_6_5_5(X011, X01110001, vM, X010101, vN, vD);
               break;

            case ARM64vecb_FRECPS64x2:
               *p++ = X_3_8_5_6_5_5(X010, X01110011, vM, X111111, vN, vD);
               break;
            case ARM64vecb_FRECPS32x4:
               *p++ = X_3_8_5_6_5_5(X010, X01110001, vM, X111111, vN, vD);
               break;
            case ARM64vecb_FRSQRTS64x2:
               *p++ = X_3_8_5_6_5_5(X010, X01110111, vM, X111111, vN, vD);
               break;
            case ARM64vecb_FRSQRTS32x4:
               *p++ = X_3_8_5_6_5_5(X010, X01110101, vM, X111111, vN, vD);
               break;

            default:
               goto bad;
         }
         goto done;
      }
      case ARM64in_VModifyV: {
         /* 31        23   20    15     9 4
            010 01110 sz 1 00000 001110 n d   SUQADD@sz  Vd, Vn
            011 01110 sz 1 00000 001110 n d   USQADD@sz  Vd, Vn
         */
         UInt vD = qregEnc(i->ARM64in.VModifyV.mod);
         UInt vN = qregEnc(i->ARM64in.VModifyV.arg);
         switch (i->ARM64in.VModifyV.op) {
            case ARM64vecmo_SUQADD64x2:
               *p++ = X_3_8_5_6_5_5(X010, X01110111, X00000, X001110, vN, vD);
               break;
            case ARM64vecmo_SUQADD32x4:
               *p++ = X_3_8_5_6_5_5(X010, X01110101, X00000, X001110, vN, vD);
               break;
            case ARM64vecmo_SUQADD16x8:
               *p++ = X_3_8_5_6_5_5(X010, X01110011, X00000, X001110, vN, vD);
               break;
            case ARM64vecmo_SUQADD8x16:
               *p++ = X_3_8_5_6_5_5(X010, X01110001, X00000, X001110, vN, vD);
               break;
            case ARM64vecmo_USQADD64x2:
               *p++ = X_3_8_5_6_5_5(X011, X01110111, X00000, X001110, vN, vD);
               break;
            case ARM64vecmo_USQADD32x4:
               *p++ = X_3_8_5_6_5_5(X011, X01110101, X00000, X001110, vN, vD);
               break;
            case ARM64vecmo_USQADD16x8:
               *p++ = X_3_8_5_6_5_5(X011, X01110011, X00000, X001110, vN, vD);
               break;
            case ARM64vecmo_USQADD8x16:
               *p++ = X_3_8_5_6_5_5(X011, X01110001, X00000, X001110, vN, vD);
               break;
            default:
               goto bad;
         }
         goto done;
      }
      case ARM64in_VUnaryV: {
         /* 31        23   20    15     9 4
            010 01110 11 1 00000 111110 n d  FABS Vd.2d,  Vn.2d
            010 01110 10 1 00000 111110 n d  FABS Vd.4s,  Vn.4s
            010 01110 11 1 11000 111110 n d  FABS Vd.8h,  Vn.8h
            011 01110 11 1 00000 111110 n d  FNEG Vd.2d,  Vn.2d
            011 01110 10 1 00000 111110 n d  FNEG Vd.4s,  Vn.4s
            011 01110 11 1 11000 111110 n d  FNEG Vd.8h,  Vn.8h
            011 01110 00 1 00000 010110 n d  NOT  Vd.16b, Vn.16b

            010 01110 11 1 00000 101110 n d  ABS  Vd.2d,  Vn.2d
            010 01110 10 1 00000 101110 n d  ABS  Vd.4s,  Vn.4s
            010 01110 01 1 00000 101110 n d  ABS  Vd.8h,  Vn.8h
            010 01110 00 1 00000 101110 n d  ABS  Vd.16b, Vn.16b

            010 01110 10 1 00000 010010 n d  CLS  Vd.4s,  Vn.4s
            010 01110 01 1 00000 010010 n d  CLS  Vd.8h,  Vn.8h
            010 01110 00 1 00000 010010 n d  CLS  Vd.16b, Vn.16b

            011 01110 10 1 00000 010010 n d  CLZ  Vd.4s,  Vn.4s
            011 01110 01 1 00000 010010 n d  CLZ  Vd.8h,  Vn.8h
            011 01110 00 1 00000 010010 n d  CLZ  Vd.16b, Vn.16b

            010 01110 00 1 00000 010110 n d  CNT  Vd.16b, Vn.16b

            011 01110 01 1 00000 010110 n d  RBIT  Vd.16b, Vn.16b
            010 01110 00 1 00000 000110 n d  REV16 Vd.16b, Vn.16b
            011 01110 00 1 00000 000010 n d  REV32 Vd.16b, Vn.16b
            011 01110 01 1 00000 000010 n d  REV32 Vd.8h, Vn.8h

            010 01110 00 1 00000 000010 n d  REV64 Vd.16b, Vn.16b
            010 01110 01 1 00000 000010 n d  REV64 Vd.8h, Vn.8h
            010 01110 10 1 00000 000010 n d  REV64 Vd.4s, Vn.4s

            010 01110 10 1 00001 110010 n d  URECPE Vd.4s, Vn.4s
            011 01110 10 1 00001 110010 n d  URSQRTE Vd.4s, Vn.4s

            010 01110 11 1 00001 110110 n d  FRECPE Vd.2d, Vn.2d
            010 01110 10 1 00001 110110 n d  FRECPE Vd.4s, Vn.4s

            011 01110 11 1 00001 110110 n d  FRECPE Vd.2d, Vn.2d
            011 01110 10 1 00001 110110 n d  FRECPE Vd.4s, Vn.4s

            011 01110 11 1 00001 111110 n d  FSQRT Vd.2d, Vn.2d
            011 01110 10 1 00001 111110 n d  FSQRT Vd.4s, Vn.4s
            011 01110 11 1 11001 111110 n d  FSQRT Vd.8h, Vn.8h
         */
         UInt vD = qregEnc(i->ARM64in.VUnaryV.dst);
         UInt vN = qregEnc(i->ARM64in.VUnaryV.arg);
         switch (i->ARM64in.VUnaryV.op) {
            case ARM64vecu_FABS16x8:
               *p++ = X_3_8_5_6_5_5(X010, X01110111, X11000, X111110, vN, vD);
               break;
            case ARM64vecu_FABS64x2:
               *p++ = X_3_8_5_6_5_5(X010, X01110111, X00000, X111110, vN, vD);
               break;
            case ARM64vecu_FABS32x4:
               *p++ = X_3_8_5_6_5_5(X010, X01110101, X00000, X111110, vN, vD);
               break;
            case ARM64vecu_FNEG16x8:
               *p++ = X_3_8_5_6_5_5(X011, X01110111, X11000, X111110, vN, vD);
               break;
            case ARM64vecu_FNEG64x2:
               *p++ = X_3_8_5_6_5_5(X011, X01110111, X00000, X111110, vN, vD);
               break;
            case ARM64vecu_FNEG32x4:
               *p++ = X_3_8_5_6_5_5(X011, X01110101, X00000, X111110, vN, vD);
               break;
            case ARM64vecu_NOT:
               *p++ = X_3_8_5_6_5_5(X011, X01110001, X00000, X010110, vN, vD);
               break;
            case ARM64vecu_ABS64x2:
               *p++ = X_3_8_5_6_5_5(X010, X01110111, X00000, X101110, vN, vD);
               break;
            case ARM64vecu_ABS32x4:
               *p++ = X_3_8_5_6_5_5(X010, X01110101, X00000, X101110, vN, vD);
               break;
            case ARM64vecu_ABS16x8:
               *p++ = X_3_8_5_6_5_5(X010, X01110011, X00000, X101110, vN, vD);
               break;
            case ARM64vecu_ABS8x16:
               *p++ = X_3_8_5_6_5_5(X010, X01110001, X00000, X101110, vN, vD);
               break;
            case ARM64vecu_CLS32x4:
               *p++ = X_3_8_5_6_5_5(X010, X01110101, X00000, X010010, vN, vD);
               break;
            case ARM64vecu_CLS16x8:
               *p++ = X_3_8_5_6_5_5(X010, X01110011, X00000, X010010, vN, vD);
               break;
            case ARM64vecu_CLS8x16:
               *p++ = X_3_8_5_6_5_5(X010, X01110001, X00000, X010010, vN, vD);
               break;
            case ARM64vecu_CLZ32x4:
               *p++ = X_3_8_5_6_5_5(X011, X01110101, X00000, X010010, vN, vD);
               break;
            case ARM64vecu_CLZ16x8:
               *p++ = X_3_8_5_6_5_5(X011, X01110011, X00000, X010010, vN, vD);
               break;
            case ARM64vecu_CLZ8x16:
               *p++ = X_3_8_5_6_5_5(X011, X01110001, X00000, X010010, vN, vD);
               break;
            case ARM64vecu_CNT8x16:
               *p++ = X_3_8_5_6_5_5(X010, X01110001, X00000, X010110, vN, vD);
               break;
            case ARM64vecu_RBIT:
               *p++ = X_3_8_5_6_5_5(X011, X01110011, X00000, X010110, vN, vD);
               break;
            case ARM64vecu_REV1616B:
               *p++ = X_3_8_5_6_5_5(X010, X01110001, X00000, X000110, vN, vD);
               break;
            case ARM64vecu_REV3216B:
               *p++ = X_3_8_5_6_5_5(X011, X01110001, X00000, X000010, vN, vD);
               break;
            case ARM64vecu_REV328H:
               *p++ = X_3_8_5_6_5_5(X011, X01110011, X00000, X000010, vN, vD);
               break;
            case ARM64vecu_REV6416B:
               *p++ = X_3_8_5_6_5_5(X010, X01110001, X00000, X000010, vN, vD);
               break;
            case ARM64vecu_REV648H:
               *p++ = X_3_8_5_6_5_5(X010, X01110011, X00000, X000010, vN, vD);
               break;
            case ARM64vecu_REV644S:
               *p++ = X_3_8_5_6_5_5(X010, X01110101, X00000, X000010, vN, vD);
               break;
            case ARM64vecu_URECPE32x4:
               *p++ = X_3_8_5_6_5_5(X010, X01110101, X00001, X110010, vN, vD);
               break;
            case ARM64vecu_URSQRTE32x4:
               *p++ = X_3_8_5_6_5_5(X011, X01110101, X00001, X110010, vN, vD);
               break;
            case ARM64vecu_FRECPE64x2:
               *p++ = X_3_8_5_6_5_5(X010, X01110111, X00001, X110110, vN, vD);
               break;
            case ARM64vecu_FRECPE32x4:
               *p++ = X_3_8_5_6_5_5(X010, X01110101, X00001, X110110, vN, vD);
               break;
            case ARM64vecu_FRSQRTE64x2:
               *p++ = X_3_8_5_6_5_5(X011, X01110111, X00001, X110110, vN, vD);
               break;
            case ARM64vecu_FRSQRTE32x4:
               *p++ = X_3_8_5_6_5_5(X011, X01110101, X00001, X110110, vN, vD);
               break;
            case ARM64vecu_FSQRT64x2:
               *p++ = X_3_8_5_6_5_5(X011, X01110111, X00001, X111110, vN, vD);
               break;
            case ARM64vecu_FSQRT32x4:
               *p++ = X_3_8_5_6_5_5(X011, X01110101, X00001, X111110, vN, vD);
               break;
            case ARM64vecu_FSQRT16x8:
               *p++ = X_3_8_5_6_5_5(X011, X01110111, X11001, X111110, vN, vD);
               break;
            default:
               goto bad;
         }
         goto done;
      }
      case ARM64in_VNarrowV: {
         /* 31        23 21      15     9 4
            000 01110 00 1,00001 001010 n d  XTN Vd.8b, Vn.8h
            000 01110 01 1,00001 001010 n d  XTN Vd.4h, Vn.4s
            000 01110 10 1,00001 001010 n d  XTN Vd.2s, Vn.2d

            001 01110 00 1,00001 001010 n d  SQXTUN Vd.8b, Vn.8h
            001 01110 01 1,00001 001010 n d  SQXTUN Vd.4h, Vn.4s
            001 01110 10 1,00001 001010 n d  SQXTUN Vd.2s, Vn.2d

            000 01110 00 1,00001 010010 n d  SQXTN Vd.8b, Vn.8h
            000 01110 01 1,00001 010010 n d  SQXTN Vd.4h, Vn.4s
            000 01110 10 1,00001 010010 n d  SQXTN Vd.2s, Vn.2d

            001 01110 00 1,00001 010010 n d  UQXTN Vd.8b, Vn.8h
            001 01110 01 1,00001 010010 n d  UQXTN Vd.4h, Vn.4s
            001 01110 10 1,00001 010010 n d  UQXTN Vd.2s, Vn.2d
         */
         UInt vD = qregEnc(i->ARM64in.VNarrowV.dst);
         UInt vN = qregEnc(i->ARM64in.VNarrowV.src);
         UInt dszBlg2 = i->ARM64in.VNarrowV.dszBlg2;
         vassert(dszBlg2 >= 0 && dszBlg2 <= 2);
         switch (i->ARM64in.VNarrowV.op) {
            case ARM64vecna_XTN:
               *p++ = X_3_8_5_6_5_5(X000, X01110001 | (dszBlg2 << 1),
                                    X00001, X001010, vN, vD);
               goto done;
            case ARM64vecna_SQXTUN:
               *p++ = X_3_8_5_6_5_5(X001, X01110001 | (dszBlg2 << 1),
                                    X00001, X001010, vN, vD);
               goto done;
            case ARM64vecna_SQXTN:
               *p++ = X_3_8_5_6_5_5(X000, X01110001 | (dszBlg2 << 1),
                                    X00001, X010010, vN, vD);
               goto done;
            case ARM64vecna_UQXTN:
               *p++ = X_3_8_5_6_5_5(X001, X01110001 | (dszBlg2 << 1),
                                    X00001, X010010, vN, vD);
               goto done;
            default:
               break;
         }
         goto bad;
      }
      case ARM64in_VShiftImmV: {
         /*
            011 011110 immh immb 000001 n d  USHR     Vd.T, Vn.T, #sh
            010 011110 immh immb 000001 n d  SSHR     Vd.T, Vn.T, #sh

            001 011110 immh immb 100101 n d  UQSHRN   ,,#sh
            000 011110 immh immb 100101 n d  SQSHRN   ,,#sh
            001 011110 immh immb 100001 n d  SQSHRUN  ,,#sh

            001 011110 immh immb 100111 n d  UQRSHRN  ,,#sh
            000 011110 immh immb 100111 n d  SQRSHRN  ,,#sh
            001 011110 immh immb 100011 n d  SQRSHRUN ,,#sh

            where immh:immb
               = case T of 
                    2d  | sh in 1..64 -> let xxxxxx = 64-sh in 1xxx:xxx
                    4s  | sh in 1..32 -> let  xxxxx = 32-sh in 01xx:xxx
                    8h  | sh in 1..16 -> let   xxxx = 16-sh in 001x:xxx
                    16b | sh in 1..8  -> let    xxx =  8-sh in 0001:xxx

            010 011110 immh immb 010101 n d  SHL    Vd.T, Vn.T, #sh

            011 011110 immh immb 011101 n d  UQSHL  Vd.T, Vn.T, #sh
            010 011110 immh immb 011101 n d  SQSHL  Vd.T, Vn.T, #sh
            011 011110 immh immb 011001 n d  SQSHLU Vd.T, Vn.T, #sh

            where immh:immb
               = case T of 
                    2d  | sh in 0..63 -> let xxxxxx = sh in 1xxx:xxx
                    4s  | sh in 0..31 -> let  xxxxx = sh in 01xx:xxx
                    8h  | sh in 0..15 -> let   xxxx = sh in 001x:xxx
                    16b | sh in 0..7  -> let    xxx = sh in 0001:xxx
         */
         UInt vD   = qregEnc(i->ARM64in.VShiftImmV.dst);
         UInt vN   = qregEnc(i->ARM64in.VShiftImmV.src);
         UInt sh   = i->ARM64in.VShiftImmV.amt;
         UInt tmpl = 0; /* invalid */

         const UInt tmpl_USHR
            = X_3_6_7_6_5_5(X011, X011110, 0, X000001, vN, vD);
         const UInt tmpl_SSHR
            = X_3_6_7_6_5_5(X010, X011110, 0, X000001, vN, vD);

         const UInt tmpl_UQSHRN
            = X_3_6_7_6_5_5(X001, X011110, 0, X100101, vN, vD);
         const UInt tmpl_SQSHRN
            = X_3_6_7_6_5_5(X000, X011110, 0, X100101, vN, vD);
         const UInt tmpl_SQSHRUN
            = X_3_6_7_6_5_5(X001, X011110, 0, X100001, vN, vD);

         const UInt tmpl_UQRSHRN
            = X_3_6_7_6_5_5(X001, X011110, 0, X100111, vN, vD);
         const UInt tmpl_SQRSHRN
            = X_3_6_7_6_5_5(X000, X011110, 0, X100111, vN, vD);
         const UInt tmpl_SQRSHRUN
            = X_3_6_7_6_5_5(X001, X011110, 0, X100011, vN, vD);

         const UInt tmpl_SHL
            = X_3_6_7_6_5_5(X010, X011110, 0, X010101, vN, vD);

         const UInt tmpl_UQSHL
            = X_3_6_7_6_5_5(X011, X011110, 0, X011101, vN, vD);
         const UInt tmpl_SQSHL
            = X_3_6_7_6_5_5(X010, X011110, 0, X011101, vN, vD);
         const UInt tmpl_SQSHLU
            = X_3_6_7_6_5_5(X011, X011110, 0, X011001, vN, vD);

         switch (i->ARM64in.VShiftImmV.op) {
            case ARM64vecshi_SSHR64x2:    tmpl = tmpl_SSHR;     goto right64x2;
            case ARM64vecshi_USHR64x2:    tmpl = tmpl_USHR;     goto right64x2;
            case ARM64vecshi_SHL64x2:     tmpl = tmpl_SHL;      goto left64x2;
            case ARM64vecshi_UQSHL64x2:   tmpl = tmpl_UQSHL;    goto left64x2;
            case ARM64vecshi_SQSHL64x2:   tmpl = tmpl_SQSHL;    goto left64x2;
            case ARM64vecshi_SQSHLU64x2:  tmpl = tmpl_SQSHLU;   goto left64x2;
            case ARM64vecshi_SSHR32x4:    tmpl = tmpl_SSHR;     goto right32x4;
            case ARM64vecshi_USHR32x4:    tmpl = tmpl_USHR;     goto right32x4;
            case ARM64vecshi_UQSHRN2SD:   tmpl = tmpl_UQSHRN;   goto right32x4;
            case ARM64vecshi_SQSHRN2SD:   tmpl = tmpl_SQSHRN;   goto right32x4;
            case ARM64vecshi_SQSHRUN2SD:  tmpl = tmpl_SQSHRUN;  goto right32x4;
            case ARM64vecshi_UQRSHRN2SD:  tmpl = tmpl_UQRSHRN;  goto right32x4;
            case ARM64vecshi_SQRSHRN2SD:  tmpl = tmpl_SQRSHRN;  goto right32x4;
            case ARM64vecshi_SQRSHRUN2SD: tmpl = tmpl_SQRSHRUN; goto right32x4;
            case ARM64vecshi_SHL32x4:     tmpl = tmpl_SHL;      goto left32x4;
            case ARM64vecshi_UQSHL32x4:   tmpl = tmpl_UQSHL;    goto left32x4;
            case ARM64vecshi_SQSHL32x4:   tmpl = tmpl_SQSHL;    goto left32x4;
            case ARM64vecshi_SQSHLU32x4:  tmpl = tmpl_SQSHLU;   goto left32x4;
            case ARM64vecshi_SSHR16x8:    tmpl = tmpl_SSHR;     goto right16x8;
            case ARM64vecshi_USHR16x8:    tmpl = tmpl_USHR;     goto right16x8;
            case ARM64vecshi_UQSHRN4HS:   tmpl = tmpl_UQSHRN;   goto right16x8;
            case ARM64vecshi_SQSHRN4HS:   tmpl = tmpl_SQSHRN;   goto right16x8;
            case ARM64vecshi_SQSHRUN4HS:  tmpl = tmpl_SQSHRUN;  goto right16x8;
            case ARM64vecshi_UQRSHRN4HS:  tmpl = tmpl_UQRSHRN;  goto right16x8;
            case ARM64vecshi_SQRSHRN4HS:  tmpl = tmpl_SQRSHRN;  goto right16x8;
            case ARM64vecshi_SQRSHRUN4HS: tmpl = tmpl_SQRSHRUN; goto right16x8;
            case ARM64vecshi_SHL16x8:     tmpl = tmpl_SHL;      goto left16x8;
            case ARM64vecshi_UQSHL16x8:   tmpl = tmpl_UQSHL;    goto left16x8;
            case ARM64vecshi_SQSHL16x8:   tmpl = tmpl_SQSHL;    goto left16x8;
            case ARM64vecshi_SQSHLU16x8:  tmpl = tmpl_SQSHLU;   goto left16x8;
            case ARM64vecshi_SSHR8x16:    tmpl = tmpl_SSHR;     goto right8x16;
            case ARM64vecshi_USHR8x16:    tmpl = tmpl_USHR;     goto right8x16;
            case ARM64vecshi_UQSHRN8BH:   tmpl = tmpl_UQSHRN;   goto right8x16;
            case ARM64vecshi_SQSHRN8BH:   tmpl = tmpl_SQSHRN;   goto right8x16;
            case ARM64vecshi_SQSHRUN8BH:  tmpl = tmpl_SQSHRUN;  goto right8x16;
            case ARM64vecshi_UQRSHRN8BH:  tmpl = tmpl_UQRSHRN;  goto right8x16;
            case ARM64vecshi_SQRSHRN8BH:  tmpl = tmpl_SQRSHRN;  goto right8x16;
            case ARM64vecshi_SQRSHRUN8BH: tmpl = tmpl_SQRSHRUN; goto right8x16;
            case ARM64vecshi_SHL8x16:     tmpl = tmpl_SHL;      goto left8x16;
            case ARM64vecshi_UQSHL8x16:   tmpl = tmpl_UQSHL;    goto left8x16;
            case ARM64vecshi_SQSHL8x16:   tmpl = tmpl_SQSHL;    goto left8x16;
            case ARM64vecshi_SQSHLU8x16:  tmpl = tmpl_SQSHLU;   goto left8x16;

            default: break;

            right64x2:
               if (sh >= 1 && sh <= 63) {
                  *p++ = tmpl | X_3_6_7_6_5_5(0,0, X1000000 | (64-sh), 0,0,0);
                  goto done;
               }
               break;
            right32x4:
               if (sh >= 1 && sh <= 32) {
                  *p++ = tmpl | X_3_6_7_6_5_5(0,0, X0100000 | (32-sh), 0,0,0);
                  goto done;
               }
               break;
            right16x8:
               if (sh >= 1 && sh <= 16) {
                  *p++ = tmpl | X_3_6_7_6_5_5(0,0, X0010000 | (16-sh), 0,0,0);
                  goto done;
               }
               break;
            right8x16:
               if (sh >= 1 && sh <= 8) {
                  *p++ = tmpl | X_3_6_7_6_5_5(0,0, X0001000 | (8-sh), 0,0,0);
                  goto done;
               }
               break;

            left64x2:
               if (sh >= 0 && sh <= 63) {
                  *p++ = tmpl | X_3_6_7_6_5_5(0,0, X1000000 | sh, 0,0,0);
                  goto done;
               }
               break;
            left32x4:
               if (sh >= 0 && sh <= 31) {
                  *p++ = tmpl | X_3_6_7_6_5_5(0,0, X0100000 | sh, 0,0,0);
                  goto done;
               }
               break;
            left16x8:
               if (sh >= 0 && sh <= 15) {
                  *p++ = tmpl | X_3_6_7_6_5_5(0,0, X0010000 | sh, 0,0,0);
                  goto done;
               }
               break;
            left8x16:
               if (sh >= 0 && sh <= 7) {
                  *p++ = tmpl | X_3_6_7_6_5_5(0,0, X0001000 | sh, 0,0,0);
                  goto done;
               }
               break;
         }
         goto bad;
      }
      case ARM64in_VExtV: {
         /*
            011 01110 000 m 0 imm4 0 n d  EXT Vd.16b, Vn.16b, Vm.16b, #imm4
            where imm4 = the shift amount, in bytes,
                  Vn is low operand, Vm is high operand
         */
         UInt vD   = qregEnc(i->ARM64in.VExtV.dst);
         UInt vN   = qregEnc(i->ARM64in.VExtV.srcLo);
         UInt vM   = qregEnc(i->ARM64in.VExtV.srcHi);
         UInt imm4 = i->ARM64in.VExtV.amtB;
         vassert(imm4 >= 1 && imm4 <= 15);
         *p++ = X_3_8_5_6_5_5(X011, X01110000, vM,
                              X000000 | (imm4 << 1), vN, vD);
         goto done;
      }
      case ARM64in_VImmQ: {
         UInt   rQ  = qregEnc(i->ARM64in.VImmQ.rQ);
         UShort imm = i->ARM64in.VImmQ.imm;
         vassert(rQ < 32);
         switch (imm) {
            case 0x0000:
               // movi rQ.4s, #0x0 == 0x4F 0x00 0x04 000 rQ
               *p++ = 0x4F000400 | rQ;
               goto done;
            case 0x0001:
               // movi rQ, #0xFF == 0x2F 0x00 0xE4 001 rQ
               *p++ = 0x2F00E420 | rQ;
               goto done;
            case 0x0003:
               // movi rQ, #0xFFFF == 0x2F 0x00 0xE4 011 rQ
               *p++ = 0x2F00E460 | rQ;
               goto done;
            case 0x000F:
               // movi rQ, #0xFFFFFFFF == 0x2F 0x00 0xE5 111 rQ
               *p++ = 0x2F00E5E0 | rQ;
               goto done;
            case 0x003F:
               // movi rQ, #0xFFFFFFFFFFFF == 0x2F 0x01 0xE7 111 rQ
               *p++ = 0x2F01E7E0 | rQ;
               goto done;
            case 0x00FF:
               // movi rQ, #0xFFFFFFFFFFFFFFFF == 0x2F 0x07 0xE7 111 rQ
               *p++ = 0x2F07E7E0 | rQ;
               goto done;
            case 0xFFFF:
               // mvni rQ.4s, #0x0 == 0x6F 0x00 0x04 000 rQ
               *p++ = 0x6F000400 | rQ;
               goto done;
            default:
               break;
         }
         goto bad; /* no other handled cases right now */
      }

      case ARM64in_VDfromX: {
         /* INS Vd.D[0], rX
            0100 1110 0000 1000 0001 11 nn dd   INS Vd.D[0], Xn
            This isn't wonderful, in the sense that the upper half of
            the vector register stays unchanged and thus the insn is
            data dependent on its output register. */
         UInt dd = dregEnc(i->ARM64in.VDfromX.rD);
         UInt xx = iregEnc(i->ARM64in.VDfromX.rX);
         vassert(xx < 31);
         *p++ = 0x4E081C00 | X_2_6_2_12_5_5(0,0,0,0,xx,dd);
         goto done;
      }

      case ARM64in_VQfromX: {
         /* FMOV D, X
            1001 1110 0110 0111 0000 00 nn dd   FMOV Vd.D[0], Xn
            I think this zeroes out the top half of the destination, which
            is what we need.  TODO: can we do VDfromX and VQfromXX better? */
         UInt dd = qregEnc(i->ARM64in.VQfromX.rQ);
         UInt xx = iregEnc(i->ARM64in.VQfromX.rXlo);
         vassert(xx < 31);
         *p++ = 0x9E670000 | X_2_6_2_12_5_5(0,0,0,0,xx,dd);
         goto done;
      }

      case ARM64in_VQfromXX: {
         /* What we really generate is a two insn sequence:
               INS Vd.D[0], Xlo; INS Vd.D[1], Xhi
            0100 1110 0000 1000 0001 11 nn dd   INS Vd.D[0], Xn
            0100 1110 0001 1000 0001 11 nn dd   INS Vd.D[1], Xn
         */
         UInt qq  = qregEnc(i->ARM64in.VQfromXX.rQ);
         UInt xhi = iregEnc(i->ARM64in.VQfromXX.rXhi);
         UInt xlo = iregEnc(i->ARM64in.VQfromXX.rXlo);
         vassert(xhi < 31 && xlo < 31);
         *p++ = 0x4E081C00 | X_2_6_2_12_5_5(0,0,0,0,xlo,qq);
         *p++ = 0x4E181C00 | X_2_6_2_12_5_5(0,0,0,0,xhi,qq);
         goto done;
      }

      case ARM64in_VXfromQ: {
         /* 010 0111 0000 01000 001111 nn dd  UMOV Xd, Vn.D[0]
            010 0111 0000 11000 001111 nn dd  UMOV Xd, Vn.D[1]
         */
         UInt dd     = iregEnc(i->ARM64in.VXfromQ.rX);
         UInt nn     = qregEnc(i->ARM64in.VXfromQ.rQ);
         UInt laneNo = i->ARM64in.VXfromQ.laneNo;
         vassert(dd < 31);
         vassert(laneNo < 2);
         *p++ = X_3_8_5_6_5_5(X010, X01110000,
                              laneNo == 1 ? X11000 : X01000, X001111, nn, dd);
         goto done;
      }

      case ARM64in_VXfromDorS: {
         /* 000 11110001 00110 000000 n d     FMOV Wd, Sn
            100 11110011 00110 000000 n d     FMOV Xd, Dn
         */
         UInt dd    = iregEnc(i->ARM64in.VXfromDorS.rX);
         UInt nn    = dregEnc(i->ARM64in.VXfromDorS.rDorS);
         Bool fromD = i->ARM64in.VXfromDorS.fromD;
         vassert(dd < 31);
         *p++ = X_3_8_5_6_5_5(fromD ? X100 : X000,
                              fromD ? X11110011 : X11110001,
                              X00110, X000000, nn, dd);
         goto done;
      }

      case ARM64in_VMov: {
         /* 000 11110 00 10000 00 10000 n d   FMOV Sd, Sn
            000 11110 01 10000 00 10000 n d   FMOV Dd, Dn
            010 01110 10 1 n    0 00111 n d   MOV Vd.16b, Vn.16b
         */
        HReg rD = i->ARM64in.VMov.dst;
        HReg rN = i->ARM64in.VMov.src;
        switch (i->ARM64in.VMov.szB) {
           case 16: {
              UInt dd = qregEnc(rD);
              UInt nn = qregEnc(rN);
              *p++ = X_3_8_5_6_5_5(X010, X01110101, nn, X000111, nn, dd);
              goto done;
           }
           case 8: {
              UInt dd = dregEnc(rD);
              UInt nn = dregEnc(rN);
              *p++ = X_3_8_5_6_5_5(X000, X11110011, X00000, X010000, nn, dd);
              goto done;
           }
           default: 
              break;
        }
        goto bad;
      }

      case ARM64in_EvCheck: {
         /* The sequence is fixed (canned) except for the two amodes
            supplied by the insn.  These don't change the length, though.
            We generate:
               ldr  w9, [x21 + #8]   8 == offsetof(host_EvC_COUNTER)
               subs w9, w9, #1
               str  w9, [x21 + #8]   8 == offsetof(host_EvC_COUNTER)
               bpl  nofail
               ldr  x9, [x21 + #0]   0 == offsetof(host_EvC_FAILADDR)
               br   x9
              nofail:
         */
         UInt* p0 = p;
         p = do_load_or_store32(p, True/*isLoad*/, /*w*/9,
                                i->ARM64in.EvCheck.amCounter);
         *p++ = 0x71000529; /* subs w9, w9, #1 */
         p = do_load_or_store32(p, False/*!isLoad*/, /*w*/9,
                                i->ARM64in.EvCheck.amCounter);
         *p++ = 0x54000065; /* bpl nofail */
         p = do_load_or_store64(p, True/*isLoad*/, /*x*/9,
                                i->ARM64in.EvCheck.amFailAddr);
         *p++ = 0xD61F0120; /* br x9 */
         /* nofail: */

         /* Crosscheck */
         vassert(evCheckSzB_ARM64() == (UChar*)p - (UChar*)p0);
         goto done;
      }

      case ARM64in_ProfInc: {
         /* We generate:
              (ctrP is unknown now, so use 0x6555'7555'8555'9566 in the
              expectation that a later call to LibVEX_patchProfCtr
              will be used to fill in the immediate fields once the
              right value is known.)
            imm64-exactly4 x9, 0x6555'7555'8555'9566
            ldr  x8, [x9]
            add  x8, x8, #1
            str  x8, [x9]
         */
         p = imm64_to_ireg_EXACTLY4(p, /*x*/9, 0x6555755585559566ULL);
         *p++ = 0xF9400128;
         *p++ = 0x91000508;
         *p++ = 0xF9000128;
         /* Tell the caller .. */
         vassert(!(*is_profInc));
         *is_profInc = True;
         goto done;
      }

      /* ... */
      default: 
         goto bad;
    }

  bad:
   ppARM64Instr(i);
   vpanic("emit_ARM64Instr");
   /*NOTREACHED*/

  done:
   vassert(((UChar*)p) - &buf[0] <= 40);
   return ((UChar*)p) - &buf[0];
}


/* How big is an event check?  See case for ARM64in_EvCheck in
   emit_ARM64Instr just above.  That crosschecks what this returns, so
   we can tell if we're inconsistent. */
Int evCheckSzB_ARM64 (void)
{
   return 24;
}


/* NB: what goes on here has to be very closely coordinated with the
   emitInstr case for XDirect, above. */
VexInvalRange chainXDirect_ARM64 ( VexEndness endness_host,
                                   void* place_to_chain,
                                   const void* disp_cp_chain_me_EXPECTED,
                                   const void* place_to_jump_to )
{
   vassert(endness_host == VexEndnessLE);

   /* What we're expecting to see is:
        movw x9, disp_cp_chain_me_to_EXPECTED[15:0]
        movk x9, disp_cp_chain_me_to_EXPECTED[31:15], lsl 16
        movk x9, disp_cp_chain_me_to_EXPECTED[47:32], lsl 32
        movk x9, disp_cp_chain_me_to_EXPECTED[63:48], lsl 48
        blr  x9
      viz
        <16 bytes generated by imm64_to_ireg_EXACTLY4>
        D6 3F 01 20
   */
   UInt* p = (UInt*)place_to_chain;
   vassert(0 == (3 & (HWord)p));
   vassert(is_imm64_to_ireg_EXACTLY4(
              p, /*x*/9, (Addr)disp_cp_chain_me_EXPECTED));
   vassert(p[4] == 0xD63F0120);

   /* And what we want to change it to is:
        movw x9, place_to_jump_to[15:0]
        movk x9, place_to_jump_to[31:15], lsl 16
        movk x9, place_to_jump_to[47:32], lsl 32
        movk x9, place_to_jump_to[63:48], lsl 48
        br   x9
      viz
        <16 bytes generated by imm64_to_ireg_EXACTLY4>
        D6 1F 01 20

      The replacement has the same length as the original.
   */
   (void)imm64_to_ireg_EXACTLY4(p, /*x*/9, (Addr)place_to_jump_to);
   p[4] = 0xD61F0120;

   VexInvalRange vir = {(HWord)p, 20};
   return vir;
}


/* NB: what goes on here has to be very closely coordinated with the
   emitInstr case for XDirect, above. */
VexInvalRange unchainXDirect_ARM64 ( VexEndness endness_host,
                                     void* place_to_unchain,
                                     const void* place_to_jump_to_EXPECTED,
                                     const void* disp_cp_chain_me )
{
   vassert(endness_host == VexEndnessLE);

   /* What we're expecting to see is:
        movw x9, place_to_jump_to_EXPECTED[15:0]
        movk x9, place_to_jump_to_EXPECTED[31:15], lsl 16
        movk x9, place_to_jump_to_EXPECTED[47:32], lsl 32
        movk x9, place_to_jump_to_EXPECTED[63:48], lsl 48
        br   x9
      viz
        <16 bytes generated by imm64_to_ireg_EXACTLY4>
        D6 1F 01 20
   */
   UInt* p = (UInt*)place_to_unchain;
   vassert(0 == (3 & (HWord)p));
   vassert(is_imm64_to_ireg_EXACTLY4(
              p, /*x*/9, (Addr)place_to_jump_to_EXPECTED));
   vassert(p[4] == 0xD61F0120);

   /* And what we want to change it to is:
        movw x9, disp_cp_chain_me_to[15:0]
        movk x9, disp_cp_chain_me_to[31:15], lsl 16
        movk x9, disp_cp_chain_me_to[47:32], lsl 32
        movk x9, disp_cp_chain_me_to[63:48], lsl 48
        blr  x9
      viz
        <16 bytes generated by imm64_to_ireg_EXACTLY4>
        D6 3F 01 20
   */
   (void)imm64_to_ireg_EXACTLY4(p, /*x*/9, (Addr)disp_cp_chain_me);
   p[4] = 0xD63F0120;

   VexInvalRange vir = {(HWord)p, 20};
   return vir;
}


/* Patch the counter address into a profile inc point, as previously
   created by the ARM64in_ProfInc case for emit_ARM64Instr. */
VexInvalRange patchProfInc_ARM64 ( VexEndness endness_host,
                                   void*  place_to_patch,
                                   const ULong* location_of_counter )
{
   vassert(sizeof(ULong*) == 8);
   vassert(endness_host == VexEndnessLE);
   UInt* p = (UInt*)place_to_patch;
   vassert(0 == (3 & (HWord)p));
   vassert(is_imm64_to_ireg_EXACTLY4(p, /*x*/9, 0x6555755585559566ULL));
   vassert(p[4] == 0xF9400128);
   vassert(p[5] == 0x91000508);
   vassert(p[6] == 0xF9000128);
   imm64_to_ireg_EXACTLY4(p, /*x*/9, (Addr)location_of_counter);
   VexInvalRange vir = {(HWord)p, 4*4};
   return vir;
}

/*---------------------------------------------------------------*/
/*--- end                                   host_arm64_defs.c ---*/
/*---------------------------------------------------------------*/
