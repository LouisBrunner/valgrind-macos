
/*---------------------------------------------------------------*/
/*--- begin                                 host_arm64_defs.c ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2013-2013 OpenWorks
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
*/

#include "libvex_basictypes.h"
#include "libvex.h"
#include "libvex_trc_values.h"

#include "main_util.h"
#include "host_generic_regs.h"
#include "host_arm64_defs.h"

//ZZ UInt arm_hwcaps = 0;


/* --------- Registers. --------- */

/* The usual HReg abstraction.  We use the following classes only:
     X regs (64 bit int)
     D regs (64 bit float, also used for 32 bit float)
     Q regs (128 bit vector)
*/

void ppHRegARM64 ( HReg reg )  {
   Int r;
   /* Be generic for all virtual regs. */
   if (hregIsVirtual(reg)) {
      ppHReg(reg);
      return;
   }
   /* But specific for real regs. */
   switch (hregClass(reg)) {
      case HRcInt64:
         r = hregNumber(reg);
         vassert(r >= 0 && r < 31);
         vex_printf("x%d", r);
         return;
      case HRcFlt64:
         r = hregNumber(reg);
         vassert(r >= 0 && r < 32);
         vex_printf("d%d", r);
         return;
      case HRcVec128:
         r = hregNumber(reg);
         vassert(r >= 0 && r < 32);
         vex_printf("q%d", r);
         return;
      default:
         vpanic("ppHRegARM64");
   }
}

static void ppHRegARM64asSreg ( HReg reg ) {
   ppHRegARM64(reg);
   vex_printf("(S-reg)");
}

HReg hregARM64_X0  ( void ) { return mkHReg(0,  HRcInt64, False); }
HReg hregARM64_X1  ( void ) { return mkHReg(1,  HRcInt64, False); }
HReg hregARM64_X2  ( void ) { return mkHReg(2,  HRcInt64, False); }
HReg hregARM64_X3  ( void ) { return mkHReg(3,  HRcInt64, False); }
HReg hregARM64_X4  ( void ) { return mkHReg(4,  HRcInt64, False); }
HReg hregARM64_X5  ( void ) { return mkHReg(5,  HRcInt64, False); }
HReg hregARM64_X6  ( void ) { return mkHReg(6,  HRcInt64, False); }
HReg hregARM64_X7  ( void ) { return mkHReg(7,  HRcInt64, False); }
//ZZ HReg hregARM_R8  ( void ) { return mkHReg(8,  HRcInt32, False); }
HReg hregARM64_X9  ( void ) { return mkHReg(9,  HRcInt64, False); }
HReg hregARM64_X10 ( void ) { return mkHReg(10, HRcInt64, False); }
HReg hregARM64_X11 ( void ) { return mkHReg(11, HRcInt64, False); }
HReg hregARM64_X12 ( void ) { return mkHReg(12, HRcInt64, False); }
HReg hregARM64_X13 ( void ) { return mkHReg(13, HRcInt64, False); }
HReg hregARM64_X14 ( void ) { return mkHReg(14, HRcInt64, False); }
HReg hregARM64_X15 ( void ) { return mkHReg(15, HRcInt64, False); }
HReg hregARM64_X21 ( void ) { return mkHReg(21, HRcInt64, False); }
HReg hregARM64_X22 ( void ) { return mkHReg(22, HRcInt64, False); }
HReg hregARM64_X23 ( void ) { return mkHReg(23, HRcInt64, False); }
HReg hregARM64_X24 ( void ) { return mkHReg(24, HRcInt64, False); }
HReg hregARM64_X25 ( void ) { return mkHReg(25, HRcInt64, False); }
HReg hregARM64_X26 ( void ) { return mkHReg(26, HRcInt64, False); }
HReg hregARM64_X27 ( void ) { return mkHReg(27, HRcInt64, False); }
HReg hregARM64_X28 ( void ) { return mkHReg(28, HRcInt64, False); }

// Should really use D8 .. D15 for class F64, since they are callee
// save
HReg hregARM64_D8  ( void ) { return mkHReg(8,  HRcFlt64, False); }
HReg hregARM64_D9  ( void ) { return mkHReg(9,  HRcFlt64, False); }
HReg hregARM64_D10 ( void ) { return mkHReg(10, HRcFlt64, False); }
HReg hregARM64_D11 ( void ) { return mkHReg(11, HRcFlt64, False); }
HReg hregARM64_D12 ( void ) { return mkHReg(12, HRcFlt64, False); }
HReg hregARM64_D13 ( void ) { return mkHReg(13, HRcFlt64, False); }
//ZZ HReg hregARM_S26 ( void ) { return mkHReg(26, HRcFlt32, False); }
//ZZ HReg hregARM_S27 ( void ) { return mkHReg(27, HRcFlt32, False); }
//ZZ HReg hregARM_S28 ( void ) { return mkHReg(28, HRcFlt32, False); }
//ZZ HReg hregARM_S29 ( void ) { return mkHReg(29, HRcFlt32, False); }
//ZZ HReg hregARM_S30 ( void ) { return mkHReg(30, HRcFlt32, False); }
HReg hregARM64_Q16 ( void ) { return mkHReg(16, HRcVec128, False); }
HReg hregARM64_Q17 ( void ) { return mkHReg(17, HRcVec128, False); }
HReg hregARM64_Q18 ( void ) { return mkHReg(18, HRcVec128, False); }
HReg hregARM64_Q19 ( void ) { return mkHReg(19, HRcVec128, False); }
HReg hregARM64_Q20 ( void ) { return mkHReg(20, HRcVec128, False); }
//ZZ HReg hregARM_Q11 ( void ) { return mkHReg(11, HRcVec128, False); }
//ZZ HReg hregARM_Q12 ( void ) { return mkHReg(12, HRcVec128, False); }
//ZZ HReg hregARM_Q13 ( void ) { return mkHReg(13, HRcVec128, False); }
//ZZ HReg hregARM_Q14 ( void ) { return mkHReg(14, HRcVec128, False); }
//ZZ HReg hregARM_Q15 ( void ) { return mkHReg(15, HRcVec128, False); }

void getAllocableRegs_ARM64 ( Int* nregs, HReg** arr )
{
   Int i = 0;
   *nregs = 26;
   *arr = LibVEX_Alloc(*nregs * sizeof(HReg));

   // callee saves ones (22 to 28) are listed first, since we prefer
   // them if they're available
   (*arr)[i++] = hregARM64_X22();
   (*arr)[i++] = hregARM64_X23();
   (*arr)[i++] = hregARM64_X24();
   (*arr)[i++] = hregARM64_X25();
   (*arr)[i++] = hregARM64_X26();
   (*arr)[i++] = hregARM64_X27();
   (*arr)[i++] = hregARM64_X28();

   (*arr)[i++] = hregARM64_X0();
   (*arr)[i++] = hregARM64_X1();
   (*arr)[i++] = hregARM64_X2();
   (*arr)[i++] = hregARM64_X3();
   (*arr)[i++] = hregARM64_X4();
   (*arr)[i++] = hregARM64_X5();
   (*arr)[i++] = hregARM64_X6();
   (*arr)[i++] = hregARM64_X7();
   // X8 .. who knows.
   // X9 is a chaining/spill temporary, not available to regalloc.

   // Do we really need all these?
   //(*arr)[i++] = hregARM64_X10();
   //(*arr)[i++] = hregARM64_X11();
   //(*arr)[i++] = hregARM64_X12();
   //(*arr)[i++] = hregARM64_X13();
   //(*arr)[i++] = hregARM64_X14();
   //(*arr)[i++] = hregARM64_X15();
   // X21 is the guest state pointer, not available to regalloc.

   // vector regs.  Unfortunately not callee-saved.
   (*arr)[i++] = hregARM64_Q16();
   (*arr)[i++] = hregARM64_Q17();
   (*arr)[i++] = hregARM64_Q18();
   (*arr)[i++] = hregARM64_Q19();
   (*arr)[i++] = hregARM64_Q20();

   // F64 regs, all of which are callee-saved
   (*arr)[i++] = hregARM64_D8();
   (*arr)[i++] = hregARM64_D9();
   (*arr)[i++] = hregARM64_D10();
   (*arr)[i++] = hregARM64_D11();
   (*arr)[i++] = hregARM64_D12();
   (*arr)[i++] = hregARM64_D13();

   // unavail: x21 as GSP
   // x9 is used as a spill/reload/chaining/call temporary
   // x8 is unassigned
   // x30 as LR
   // x31 because dealing with the SP-vs-ZR overloading is too
   // confusing, and we don't need to do so, so let's just avoid
   // the problem
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
   // getHRegUsage for ARMInstr_Call too.
   vassert(i == *nregs);
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
   ARM64AMode* am        = LibVEX_Alloc(sizeof(ARM64AMode));
   am->tag               = ARM64am_RI9;
   am->ARM64am.RI9.reg   = reg;
   am->ARM64am.RI9.simm9 = simm9;
   vassert(-256 <= simm9 && simm9 <= 255);
   return am;
}

ARM64AMode* ARM64AMode_RI12 ( HReg reg, Int uimm12, UChar szB ) {
   ARM64AMode* am          = LibVEX_Alloc(sizeof(ARM64AMode));
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
   ARM64AMode* am       = LibVEX_Alloc(sizeof(ARM64AMode));
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


//ZZ /* --------- Mem AModes: Addressing Mode 2 --------- */
//ZZ 
//ZZ ARMAMode2* ARMAMode2_RI ( HReg reg, Int simm9 ) {
//ZZ    ARMAMode2* am       = LibVEX_Alloc(sizeof(ARMAMode2));
//ZZ    am->tag             = ARMam2_RI;
//ZZ    am->ARMam2.RI.reg   = reg;
//ZZ    am->ARMam2.RI.simm9 = simm9;
//ZZ    vassert(-255 <= simm9 && simm9 <= 255);
//ZZ    return am;
//ZZ }
//ZZ ARMAMode2* ARMAMode2_RR ( HReg base, HReg index ) {
//ZZ    ARMAMode2* am       = LibVEX_Alloc(sizeof(ARMAMode2));
//ZZ    am->tag             = ARMam2_RR;
//ZZ    am->ARMam2.RR.base  = base;
//ZZ    am->ARMam2.RR.index = index;
//ZZ    return am;
//ZZ }
//ZZ 
//ZZ void ppARMAMode2 ( ARMAMode2* am ) {
//ZZ    switch (am->tag) {
//ZZ       case ARMam2_RI:
//ZZ          vex_printf("%d(", am->ARMam2.RI.simm9);
//ZZ          ppHRegARM(am->ARMam2.RI.reg);
//ZZ          vex_printf(")");
//ZZ          break;
//ZZ       case ARMam2_RR:
//ZZ          vex_printf("(");
//ZZ          ppHRegARM(am->ARMam2.RR.base);
//ZZ          vex_printf(",");
//ZZ          ppHRegARM(am->ARMam2.RR.index);
//ZZ          vex_printf(")");
//ZZ          break;
//ZZ       default:
//ZZ          vassert(0);
//ZZ    }
//ZZ }
//ZZ 
//ZZ static void addRegUsage_ARMAMode2 ( HRegUsage* u, ARMAMode2* am ) {
//ZZ    switch (am->tag) {
//ZZ       case ARMam2_RI:
//ZZ          addHRegUse(u, HRmRead, am->ARMam2.RI.reg);
//ZZ          return;
//ZZ       case ARMam2_RR:
//ZZ          //    addHRegUse(u, HRmRead, am->ARMam2.RR.base);
//ZZ          //    addHRegUse(u, HRmRead, am->ARMam2.RR.index);
//ZZ          //   return;
//ZZ       default:
//ZZ          vpanic("addRegUsage_ARMAmode2");
//ZZ    }
//ZZ }
//ZZ 
//ZZ static void mapRegs_ARMAMode2 ( HRegRemap* m, ARMAMode2* am ) {
//ZZ    switch (am->tag) {
//ZZ       case ARMam2_RI:
//ZZ          am->ARMam2.RI.reg = lookupHRegRemap(m, am->ARMam2.RI.reg);
//ZZ          return;
//ZZ       case ARMam2_RR:
//ZZ          //am->ARMam2.RR.base =lookupHRegRemap(m, am->ARMam2.RR.base);
//ZZ          //am->ARMam2.RR.index = lookupHRegRemap(m, am->ARMam2.RR.index);
//ZZ          //return;
//ZZ       default:
//ZZ          vpanic("mapRegs_ARMAmode2");
//ZZ    }
//ZZ }
//ZZ 
//ZZ 
//ZZ /* --------- Mem AModes: Addressing Mode VFP --------- */
//ZZ 
//ZZ ARMAModeV* mkARMAModeV ( HReg reg, Int simm11 ) {
//ZZ    ARMAModeV* am = LibVEX_Alloc(sizeof(ARMAModeV));
//ZZ    vassert(simm11 >= -1020 && simm11 <= 1020);
//ZZ    vassert(0 == (simm11 & 3));
//ZZ    am->reg    = reg;
//ZZ    am->simm11 = simm11;
//ZZ    return am;
//ZZ }
//ZZ 
//ZZ void ppARMAModeV ( ARMAModeV* am ) {
//ZZ    vex_printf("%d(", am->simm11);
//ZZ    ppHRegARM(am->reg);
//ZZ    vex_printf(")");
//ZZ }
//ZZ 
//ZZ static void addRegUsage_ARMAModeV ( HRegUsage* u, ARMAModeV* am ) {
//ZZ    addHRegUse(u, HRmRead, am->reg);
//ZZ }
//ZZ 
//ZZ static void mapRegs_ARMAModeV ( HRegRemap* m, ARMAModeV* am ) {
//ZZ    am->reg = lookupHRegRemap(m, am->reg);
//ZZ }
//ZZ 
//ZZ 
//ZZ /* --------- Mem AModes: Addressing Mode Neon ------- */
//ZZ 
//ZZ ARMAModeN *mkARMAModeN_RR ( HReg rN, HReg rM ) {
//ZZ    ARMAModeN* am = LibVEX_Alloc(sizeof(ARMAModeN));
//ZZ    am->tag = ARMamN_RR;
//ZZ    am->ARMamN.RR.rN = rN;
//ZZ    am->ARMamN.RR.rM = rM;
//ZZ    return am;
//ZZ }
//ZZ 
//ZZ ARMAModeN *mkARMAModeN_R ( HReg rN ) {
//ZZ    ARMAModeN* am = LibVEX_Alloc(sizeof(ARMAModeN));
//ZZ    am->tag = ARMamN_R;
//ZZ    am->ARMamN.R.rN = rN;
//ZZ    return am;
//ZZ }
//ZZ 
//ZZ static void addRegUsage_ARMAModeN ( HRegUsage* u, ARMAModeN* am ) {
//ZZ    if (am->tag == ARMamN_R) {
//ZZ       addHRegUse(u, HRmRead, am->ARMamN.R.rN);
//ZZ    } else {
//ZZ       addHRegUse(u, HRmRead, am->ARMamN.RR.rN);
//ZZ       addHRegUse(u, HRmRead, am->ARMamN.RR.rM);
//ZZ    }
//ZZ }
//ZZ 
//ZZ static void mapRegs_ARMAModeN ( HRegRemap* m, ARMAModeN* am ) {
//ZZ    if (am->tag == ARMamN_R) {
//ZZ       am->ARMamN.R.rN = lookupHRegRemap(m, am->ARMamN.R.rN);
//ZZ    } else {
//ZZ       am->ARMamN.RR.rN = lookupHRegRemap(m, am->ARMamN.RR.rN);
//ZZ       am->ARMamN.RR.rM = lookupHRegRemap(m, am->ARMamN.RR.rM);
//ZZ    }
//ZZ }
//ZZ 
//ZZ void ppARMAModeN ( ARMAModeN* am ) {
//ZZ    vex_printf("[");
//ZZ    if (am->tag == ARMamN_R) {
//ZZ       ppHRegARM(am->ARMamN.R.rN);
//ZZ    } else {
//ZZ       ppHRegARM(am->ARMamN.RR.rN);
//ZZ    }
//ZZ    vex_printf("]");
//ZZ    if (am->tag == ARMamN_RR) {
//ZZ       vex_printf(", ");
//ZZ       ppHRegARM(am->ARMamN.RR.rM);
//ZZ    }
//ZZ }


/* --------- Reg or uimm12<<{0,12} operands --------- */

ARM64RIA* ARM64RIA_I12 ( UShort imm12, UChar shift ) {
   ARM64RIA* riA           = LibVEX_Alloc(sizeof(ARM64RIA));
   riA->tag                = ARM64riA_I12;
   riA->ARM64riA.I12.imm12 = imm12;
   riA->ARM64riA.I12.shift = shift;
   vassert(imm12 < 4096);
   vassert(shift == 0 || shift == 12);
   return riA;
}
ARM64RIA* ARM64RIA_R ( HReg reg ) {
   ARM64RIA* riA       = LibVEX_Alloc(sizeof(ARM64RIA));
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
   ARM64RIL* riL          = LibVEX_Alloc(sizeof(ARM64RIL));
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
   ARM64RIL* riL       = LibVEX_Alloc(sizeof(ARM64RIL));
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
   ARM64RI6* ri6         = LibVEX_Alloc(sizeof(ARM64RI6));
   ri6->tag              = ARM64ri6_I6;
   ri6->ARM64ri6.I6.imm6 = imm6;
   vassert(imm6 > 0 && imm6 < 64);
   return ri6;
}
ARM64RI6* ARM64RI6_R ( HReg reg ) {
   ARM64RI6* ri6       = LibVEX_Alloc(sizeof(ARM64RI6));
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


//ZZ /* -------- Neon Immediate operatnd --------- */
//ZZ 
//ZZ ARMNImm* ARMNImm_TI ( UInt type, UInt imm8 ) {
//ZZ    ARMNImm* i = LibVEX_Alloc(sizeof(ARMNImm));
//ZZ    i->type = type;
//ZZ    i->imm8 = imm8;
//ZZ    return i;
//ZZ }
//ZZ 
//ZZ ULong ARMNImm_to_Imm64 ( ARMNImm* imm ) {
//ZZ    int i, j;
//ZZ    ULong y, x = imm->imm8;
//ZZ    switch (imm->type) {
//ZZ       case 3:
//ZZ          x = x << 8; /* fallthrough */
//ZZ       case 2:
//ZZ          x = x << 8; /* fallthrough */
//ZZ       case 1:
//ZZ          x = x << 8; /* fallthrough */
//ZZ       case 0:
//ZZ          return (x << 32) | x;
//ZZ       case 5:
//ZZ       case 6:
//ZZ          if (imm->type == 5)
//ZZ             x = x << 8;
//ZZ          else
//ZZ             x = (x << 8) | x;
//ZZ          /* fallthrough */
//ZZ       case 4:
//ZZ          x = (x << 16) | x;
//ZZ          return (x << 32) | x;
//ZZ       case 8:
//ZZ          x = (x << 8) | 0xFF;
//ZZ          /* fallthrough */
//ZZ       case 7:
//ZZ          x = (x << 8) | 0xFF;
//ZZ          return (x << 32) | x;
//ZZ       case 9:
//ZZ          x = 0;
//ZZ          for (i = 7; i >= 0; i--) {
//ZZ             y = ((ULong)imm->imm8 >> i) & 1;
//ZZ             for (j = 0; j < 8; j++) {
//ZZ                x = (x << 1) | y;
//ZZ             }
//ZZ          }
//ZZ          return x;
//ZZ       case 10:
//ZZ          x |= (x & 0x80) << 5;
//ZZ          x |= (~x & 0x40) << 5;
//ZZ          x &= 0x187F; /* 0001 1000 0111 1111 */
//ZZ          x |= (x & 0x40) << 4;
//ZZ          x |= (x & 0x40) << 3;
//ZZ          x |= (x & 0x40) << 2;
//ZZ          x |= (x & 0x40) << 1;
//ZZ          x = x << 19;
//ZZ          x = (x << 32) | x;
//ZZ          return x;
//ZZ       default:
//ZZ          vpanic("ARMNImm_to_Imm64");
//ZZ    }
//ZZ }
//ZZ 
//ZZ ARMNImm* Imm64_to_ARMNImm ( ULong x ) {
//ZZ    ARMNImm tmp;
//ZZ    if ((x & 0xFFFFFFFF) == (x >> 32)) {
//ZZ       if ((x & 0xFFFFFF00) == 0)
//ZZ          return ARMNImm_TI(0, x & 0xFF);
//ZZ       if ((x & 0xFFFF00FF) == 0)
//ZZ          return ARMNImm_TI(1, (x >> 8) & 0xFF);
//ZZ       if ((x & 0xFF00FFFF) == 0)
//ZZ          return ARMNImm_TI(2, (x >> 16) & 0xFF);
//ZZ       if ((x & 0x00FFFFFF) == 0)
//ZZ          return ARMNImm_TI(3, (x >> 24) & 0xFF);
//ZZ       if ((x & 0xFFFF00FF) == 0xFF)
//ZZ          return ARMNImm_TI(7, (x >> 8) & 0xFF);
//ZZ       if ((x & 0xFF00FFFF) == 0xFFFF)
//ZZ          return ARMNImm_TI(8, (x >> 16) & 0xFF);
//ZZ       if ((x & 0xFFFF) == ((x >> 16) & 0xFFFF)) {
//ZZ          if ((x & 0xFF00) == 0)
//ZZ             return ARMNImm_TI(4, x & 0xFF);
//ZZ          if ((x & 0x00FF) == 0)
//ZZ             return ARMNImm_TI(5, (x >> 8) & 0xFF);
//ZZ          if ((x & 0xFF) == ((x >> 8) & 0xFF))
//ZZ             return ARMNImm_TI(6, x & 0xFF);
//ZZ       }
//ZZ       if ((x & 0x7FFFF) == 0) {
//ZZ          tmp.type = 10;
//ZZ          tmp.imm8 = ((x >> 19) & 0x7F) | ((x >> 24) & 0x80);
//ZZ          if (ARMNImm_to_Imm64(&tmp) == x)
//ZZ             return ARMNImm_TI(tmp.type, tmp.imm8);
//ZZ       }
//ZZ    } else {
//ZZ       /* This can only be type 9. */
//ZZ       tmp.imm8 = (((x >> 56) & 1) << 7)
//ZZ                | (((x >> 48) & 1) << 6)
//ZZ                | (((x >> 40) & 1) << 5)
//ZZ                | (((x >> 32) & 1) << 4)
//ZZ                | (((x >> 24) & 1) << 3)
//ZZ                | (((x >> 16) & 1) << 2)
//ZZ                | (((x >>  8) & 1) << 1)
//ZZ                | (((x >>  0) & 1) << 0);
//ZZ       tmp.type = 9;
//ZZ       if (ARMNImm_to_Imm64 (&tmp) == x)
//ZZ          return ARMNImm_TI(tmp.type, tmp.imm8);
//ZZ    }
//ZZ    return NULL;
//ZZ }
//ZZ 
//ZZ void ppARMNImm (ARMNImm* i) {
//ZZ    ULong x = ARMNImm_to_Imm64(i);
//ZZ    vex_printf("0x%llX%llX", x, x);
//ZZ }
//ZZ 
//ZZ /* -- Register or scalar operand --- */
//ZZ 
//ZZ ARMNRS* mkARMNRS(ARMNRS_tag tag, HReg reg, UInt index)
//ZZ {
//ZZ    ARMNRS *p = LibVEX_Alloc(sizeof(ARMNRS));
//ZZ    p->tag = tag;
//ZZ    p->reg = reg;
//ZZ    p->index = index;
//ZZ    return p;
//ZZ }
//ZZ 
//ZZ void ppARMNRS(ARMNRS *p)
//ZZ {
//ZZ    ppHRegARM(p->reg);
//ZZ    if (p->tag == ARMNRS_Scalar) {
//ZZ       vex_printf("[%d]", p->index);
//ZZ    }
//ZZ }

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

static const HChar* showARM64FpUnaryOp ( ARM64FpUnaryOp op ) {
   switch (op) {
      case ARM64fpu_NEG:  return "neg  ";
      case ARM64fpu_ABS:  return "abs  ";
      case ARM64fpu_SQRT: return "sqrt ";
      case ARM64fpu_RINT: return "rinti";
      default: vpanic("showARM64FpUnaryOp");
   }
}

static void showARM64VecBinOp(/*OUT*/const HChar** nm,
                              /*OUT*/const HChar** ar, ARM64VecBinOp op ) {
   switch (op) {
      case ARM64vecb_ADD64x2:    *nm = "add  ";  *ar = "2d";   return;
      case ARM64vecb_ADD32x4:    *nm = "add  ";  *ar = "4s";   return;
      case ARM64vecb_ADD16x8:    *nm = "add  ";  *ar = "8h";   return;
      case ARM64vecb_ADD8x16:    *nm = "add  ";  *ar = "16b";  return;
      case ARM64vecb_SUB64x2:    *nm = "sub  ";  *ar = "2d";   return;
      case ARM64vecb_SUB32x4:    *nm = "sub  ";  *ar = "4s";   return;
      case ARM64vecb_SUB16x8:    *nm = "sub  ";  *ar = "8h";   return;
      case ARM64vecb_SUB8x16:    *nm = "sub  ";  *ar = "16b";  return;
      case ARM64vecb_MUL32x4:    *nm = "mul  ";  *ar = "4s";   return;
      case ARM64vecb_MUL16x8:    *nm = "mul  ";  *ar = "8h";   return;
      case ARM64vecb_MUL8x16:    *nm = "mul  ";  *ar = "16b";  return;
      case ARM64vecb_FADD64x2:   *nm = "fadd ";  *ar = "2d";   return;
      case ARM64vecb_FSUB64x2:   *nm = "fsub ";  *ar = "2d";   return;
      case ARM64vecb_FMUL64x2:   *nm = "fmul ";  *ar = "2d";   return;
      case ARM64vecb_FDIV64x2:   *nm = "fdiv ";  *ar = "2d";   return;
      case ARM64vecb_FADD32x4:   *nm = "fadd ";  *ar = "4s";   return;
      case ARM64vecb_FSUB32x4:   *nm = "fsub ";  *ar = "4s";   return;
      case ARM64vecb_FMUL32x4:   *nm = "fmul ";  *ar = "4s";   return;
      case ARM64vecb_FDIV32x4:   *nm = "fdiv ";  *ar = "4s";   return;
      case ARM64vecb_UMAX32x4:   *nm = "umax ";  *ar = "4s";   return;
      case ARM64vecb_UMAX16x8:   *nm = "umax ";  *ar = "8h";   return;
      case ARM64vecb_UMAX8x16:   *nm = "umax ";  *ar = "16b";  return;
      case ARM64vecb_UMIN32x4:   *nm = "umin ";  *ar = "4s";   return;
      case ARM64vecb_UMIN16x8:   *nm = "umin ";  *ar = "8h";   return;
      case ARM64vecb_UMIN8x16:   *nm = "umin ";  *ar = "16b";  return;
      case ARM64vecb_SMAX32x4:   *nm = "smax ";  *ar = "4s";   return;
      case ARM64vecb_SMAX16x8:   *nm = "smax ";  *ar = "8h";   return;
      case ARM64vecb_SMAX8x16:   *nm = "smax ";  *ar = "16b";  return;
      case ARM64vecb_SMIN32x4:   *nm = "smin ";  *ar = "4s";   return;
      case ARM64vecb_SMIN16x8:   *nm = "smin ";  *ar = "8h";   return;
      case ARM64vecb_SMIN8x16:   *nm = "smin ";  *ar = "16b";  return;
      case ARM64vecb_AND:        *nm = "and  ";  *ar = "16b";  return;
      case ARM64vecb_ORR:        *nm = "orr  ";  *ar = "16b";  return;
      case ARM64vecb_XOR:        *nm = "eor  ";  *ar = "16b";  return;
      case ARM64vecb_CMEQ64x2:   *nm = "cmeq ";  *ar = "2d";   return;
      case ARM64vecb_CMEQ32x4:   *nm = "cmeq ";  *ar = "4s";   return;
      case ARM64vecb_CMEQ16x8:   *nm = "cmeq ";  *ar = "8h";   return;
      case ARM64vecb_CMEQ8x16:   *nm = "cmeq ";  *ar = "16b";  return;
      case ARM64vecb_CMHI64x2:   *nm = "cmhi ";  *ar = "2d";   return;
      case ARM64vecb_CMHI32x4:   *nm = "cmhi ";  *ar = "4s";   return;
      case ARM64vecb_CMHI16x8:   *nm = "cmhi ";  *ar = "8h";   return;
      case ARM64vecb_CMHI8x16:   *nm = "cmhi ";  *ar = "16b";  return;
      case ARM64vecb_CMGT64x2:   *nm = "cmgt ";  *ar = "2d";   return;
      case ARM64vecb_CMGT32x4:   *nm = "cmgt ";  *ar = "4s";   return;
      case ARM64vecb_CMGT16x8:   *nm = "cmgt ";  *ar = "8h";   return;
      case ARM64vecb_CMGT8x16:   *nm = "cmgt ";  *ar = "16b";  return;
      case ARM64vecb_FCMEQ64x2:  *nm = "fcmeq";  *ar = "2d";   return;
      case ARM64vecb_FCMEQ32x4:  *nm = "fcmeq";  *ar = "4s";   return;
      case ARM64vecb_FCMGE64x2:  *nm = "fcmge";  *ar = "2d";   return;
      case ARM64vecb_FCMGE32x4:  *nm = "fcmge";  *ar = "4s";   return;
      case ARM64vecb_FCMGT64x2:  *nm = "fcmgt";  *ar = "2d";   return;
      case ARM64vecb_FCMGT32x4:  *nm = "fcmgt";  *ar = "4s";   return;
      case ARM64vecb_TBL1:       *nm = "tbl  ";  *ar = "16b";  return;
      case ARM64vecb_UZP164x2:   *nm = "uzp1 ";  *ar = "2d";   return;
      case ARM64vecb_UZP132x4:   *nm = "uzp1 ";  *ar = "4s";   return;
      case ARM64vecb_UZP116x8:   *nm = "uzp1 ";  *ar = "8h";   return;
      case ARM64vecb_UZP18x16:   *nm = "uzp1 ";  *ar = "16b";  return;
      case ARM64vecb_UZP264x2:   *nm = "uzp2 ";  *ar = "2d";   return;
      case ARM64vecb_UZP232x4:   *nm = "uzp2 ";  *ar = "4s";   return;
      case ARM64vecb_UZP216x8:   *nm = "uzp2 ";  *ar = "8h";   return;
      case ARM64vecb_UZP28x16:   *nm = "uzp2 ";  *ar = "16b";  return;
      case ARM64vecb_ZIP132x4:   *nm = "zip1 ";  *ar = "4s";   return;
      case ARM64vecb_ZIP116x8:   *nm = "zip1 ";  *ar = "8h";   return;
      case ARM64vecb_ZIP18x16:   *nm = "zip1 ";  *ar = "16b";  return;
      case ARM64vecb_ZIP232x4:   *nm = "zip2 ";  *ar = "4s";   return;
      case ARM64vecb_ZIP216x8:   *nm = "zip2 ";  *ar = "8h";   return;
      case ARM64vecb_ZIP28x16:   *nm = "zip2 ";  *ar = "16b";  return;
      case ARM64vecb_PMUL8x16:   *nm = "pmul ";  *ar = "16b";  return;
      case ARM64vecb_PMULL8x8:   *nm = "pmull";  *ar = "8hbb"; return;
      case ARM64vecb_UMULL2DSS:  *nm = "umull";  *ar = "2dss"; return;
      case ARM64vecb_UMULL4SHH:  *nm = "umull";  *ar = "4shh"; return;
      case ARM64vecb_UMULL8HBB:  *nm = "umull";  *ar = "8hbb"; return;
      case ARM64vecb_SMULL2DSS:  *nm = "smull";  *ar = "2dss"; return;
      case ARM64vecb_SMULL4SHH:  *nm = "smull";  *ar = "4shh"; return;
      case ARM64vecb_SMULL8HBB:  *nm = "smull";  *ar = "8hbb"; return;
      case ARM64vecb_SQADD64x2:  *nm = "sqadd";  *ar = "2d";   return;
      case ARM64vecb_SQADD32x4:  *nm = "sqadd";  *ar = "4s";   return;
      case ARM64vecb_SQADD16x8:  *nm = "sqadd";  *ar = "8h";   return;
      case ARM64vecb_SQADD8x16:  *nm = "sqadd";  *ar = "16b";  return;
      case ARM64vecb_UQADD64x2:  *nm = "uqadd";  *ar = "2d";   return;
      case ARM64vecb_UQADD32x4:  *nm = "uqadd";  *ar = "4s";   return;
      case ARM64vecb_UQADD16x8:  *nm = "uqadd";  *ar = "8h";   return;
      case ARM64vecb_UQADD8x16:  *nm = "uqadd";  *ar = "16b";  return;
      case ARM64vecb_SQSUB64x2:  *nm = "sqsub";  *ar = "2d";   return;
      case ARM64vecb_SQSUB32x4:  *nm = "sqsub";  *ar = "4s";   return;
      case ARM64vecb_SQSUB16x8:  *nm = "sqsub";  *ar = "8h";   return;
      case ARM64vecb_SQSUB8x16:  *nm = "sqsub";  *ar = "16b";  return;
      case ARM64vecb_UQSUB64x2:  *nm = "uqsub";  *ar = "2d";   return;
      case ARM64vecb_UQSUB32x4:  *nm = "uqsub";  *ar = "4s";   return;
      case ARM64vecb_UQSUB16x8:  *nm = "uqsub";  *ar = "8h";   return;
      case ARM64vecb_UQSUB8x16:  *nm = "uqsub";  *ar = "16b";  return;
      default: vpanic("showARM64VecBinOp");
   }
}

static void showARM64VecUnaryOp(/*OUT*/const HChar** nm,
                                /*OUT*/const HChar** ar, ARM64VecUnaryOp op )
{
   switch (op) {
      case ARM64vecu_FNEG64x2: *nm = "fneg "; *ar = "2d";  return;
      case ARM64vecu_FNEG32x4: *nm = "fneg "; *ar = "4s";  return;
      case ARM64vecu_FABS64x2: *nm = "fabs "; *ar = "2d";  return;
      case ARM64vecu_FABS32x4: *nm = "fabs "; *ar = "4s";  return;
      case ARM64vecu_NOT:      *nm = "not  "; *ar = "all"; return;
      case ARM64vecu_ABS64x2:  *nm = "abs  "; *ar = "2d";  return;
      case ARM64vecu_ABS32x4:  *nm = "abs  "; *ar = "4s";  return;
      case ARM64vecu_ABS16x8:  *nm = "abs  "; *ar = "8h";  return;
      case ARM64vecu_ABS8x16:  *nm = "abs  "; *ar = "16b"; return;
      case ARM64vecu_CLS32x4:  *nm = "cls  "; *ar = "4s";  return;
      case ARM64vecu_CLS16x8:  *nm = "cls  "; *ar = "8h";  return;
      case ARM64vecu_CLS8x16:  *nm = "cls  "; *ar = "16b"; return;
      case ARM64vecu_CLZ32x4:  *nm = "clz  "; *ar = "4s";  return;
      case ARM64vecu_CLZ16x8:  *nm = "clz  "; *ar = "8h";  return;
      case ARM64vecu_CLZ8x16:  *nm = "clz  "; *ar = "16b"; return;
      case ARM64vecu_CNT8x16:  *nm = "cnt  "; *ar = "16b"; return;
      case ARM64vecu_RBIT:     *nm = "rbit "; *ar = "16b"; return;
      case ARM64vecu_REV1616B: *nm = "rev16"; *ar = "16b"; return;
      case ARM64vecu_REV3216B: *nm = "rev32"; *ar = "16b"; return;
      case ARM64vecu_REV328H:  *nm = "rev32"; *ar = "8h";  return;
      case ARM64vecu_REV6416B: *nm = "rev64"; *ar = "16b"; return;
      case ARM64vecu_REV648H:  *nm = "rev64"; *ar = "8h";  return;
      case ARM64vecu_REV644S:  *nm = "rev64"; *ar = "4s";  return;
      default: vpanic("showARM64VecUnaryOp");
   }
}

static void showARM64VecShiftOp(/*OUT*/const HChar** nm,
                                /*OUT*/const HChar** ar,
                                ARM64VecShiftOp op )
{
   switch (op) {
      case ARM64vecsh_USHR64x2: *nm = "ushr  "; *ar = "2d";  return;
      case ARM64vecsh_USHR32x4: *nm = "ushr  "; *ar = "4s";  return;
      case ARM64vecsh_USHR16x8: *nm = "ushr  "; *ar = "8h";  return;
      case ARM64vecsh_USHR8x16: *nm = "ushr  "; *ar = "16b"; return;
      case ARM64vecsh_SSHR64x2: *nm = "sshr  "; *ar = "2d";  return;
      case ARM64vecsh_SSHR32x4: *nm = "sshr  "; *ar = "4s";  return;
      case ARM64vecsh_SSHR16x8: *nm = "sshr  "; *ar = "8h";  return;
      case ARM64vecsh_SSHR8x16: *nm = "sshr  "; *ar = "16b"; return;
      case ARM64vecsh_SHL64x2:  *nm = "shl   "; *ar = "2d";  return;
      case ARM64vecsh_SHL32x4:  *nm = "shl   "; *ar = "4s";  return;
      case ARM64vecsh_SHL16x8:  *nm = "shl   "; *ar = "8h";  return;
      case ARM64vecsh_SHL8x16:  *nm = "shl   "; *ar = "16b"; return;
      default: vpanic("showARM64VecShiftImmOp");
   }
}

//ZZ const HChar* showARMNeonBinOp ( ARMNeonBinOp op ) {
//ZZ    switch (op) {
//ZZ       case ARMneon_VAND: return "vand";
//ZZ       case ARMneon_VORR: return "vorr";
//ZZ       case ARMneon_VXOR: return "veor";
//ZZ       case ARMneon_VADD: return "vadd";
//ZZ       case ARMneon_VRHADDS: return "vrhadd";
//ZZ       case ARMneon_VRHADDU: return "vrhadd";
//ZZ       case ARMneon_VADDFP: return "vadd";
//ZZ       case ARMneon_VPADDFP: return "vpadd";
//ZZ       case ARMneon_VABDFP: return "vabd";
//ZZ       case ARMneon_VSUB: return "vsub";
//ZZ       case ARMneon_VSUBFP: return "vsub";
//ZZ       case ARMneon_VMINU: return "vmin";
//ZZ       case ARMneon_VMINS: return "vmin";
//ZZ       case ARMneon_VMINF: return "vmin";
//ZZ       case ARMneon_VMAXU: return "vmax";
//ZZ       case ARMneon_VMAXS: return "vmax";
//ZZ       case ARMneon_VMAXF: return "vmax";
//ZZ       case ARMneon_VQADDU: return "vqadd";
//ZZ       case ARMneon_VQADDS: return "vqadd";
//ZZ       case ARMneon_VQSUBU: return "vqsub";
//ZZ       case ARMneon_VQSUBS: return "vqsub";
//ZZ       case ARMneon_VCGTU:  return "vcgt";
//ZZ       case ARMneon_VCGTS:  return "vcgt";
//ZZ       case ARMneon_VCGTF:  return "vcgt";
//ZZ       case ARMneon_VCGEF:  return "vcgt";
//ZZ       case ARMneon_VCGEU:  return "vcge";
//ZZ       case ARMneon_VCGES:  return "vcge";
//ZZ       case ARMneon_VCEQ:  return "vceq";
//ZZ       case ARMneon_VCEQF:  return "vceq";
//ZZ       case ARMneon_VPADD:   return "vpadd";
//ZZ       case ARMneon_VPMINU:   return "vpmin";
//ZZ       case ARMneon_VPMINS:   return "vpmin";
//ZZ       case ARMneon_VPMINF:   return "vpmin";
//ZZ       case ARMneon_VPMAXU:   return "vpmax";
//ZZ       case ARMneon_VPMAXS:   return "vpmax";
//ZZ       case ARMneon_VPMAXF:   return "vpmax";
//ZZ       case ARMneon_VEXT:   return "vext";
//ZZ       case ARMneon_VMUL:   return "vmuli";
//ZZ       case ARMneon_VMULLU:   return "vmull";
//ZZ       case ARMneon_VMULLS:   return "vmull";
//ZZ       case ARMneon_VMULP:  return "vmul";
//ZZ       case ARMneon_VMULFP:  return "vmul";
//ZZ       case ARMneon_VMULLP:  return "vmul";
//ZZ       case ARMneon_VQDMULH: return "vqdmulh";
//ZZ       case ARMneon_VQRDMULH: return "vqrdmulh";
//ZZ       case ARMneon_VQDMULL: return "vqdmull";
//ZZ       case ARMneon_VTBL: return "vtbl";
//ZZ       case ARMneon_VRECPS: return "vrecps";
//ZZ       case ARMneon_VRSQRTS: return "vrecps";
//ZZ       /* ... */
//ZZ       default: vpanic("showARMNeonBinOp");
//ZZ    }
//ZZ }
//ZZ 
//ZZ const HChar* showARMNeonBinOpDataType ( ARMNeonBinOp op ) {
//ZZ    switch (op) {
//ZZ       case ARMneon_VAND:
//ZZ       case ARMneon_VORR:
//ZZ       case ARMneon_VXOR:
//ZZ          return "";
//ZZ       case ARMneon_VADD:
//ZZ       case ARMneon_VSUB:
//ZZ       case ARMneon_VEXT:
//ZZ       case ARMneon_VMUL:
//ZZ       case ARMneon_VPADD:
//ZZ       case ARMneon_VTBL:
//ZZ       case ARMneon_VCEQ:
//ZZ          return ".i";
//ZZ       case ARMneon_VRHADDU:
//ZZ       case ARMneon_VMINU:
//ZZ       case ARMneon_VMAXU:
//ZZ       case ARMneon_VQADDU:
//ZZ       case ARMneon_VQSUBU:
//ZZ       case ARMneon_VCGTU:
//ZZ       case ARMneon_VCGEU:
//ZZ       case ARMneon_VMULLU:
//ZZ       case ARMneon_VPMINU:
//ZZ       case ARMneon_VPMAXU:
//ZZ          return ".u";
//ZZ       case ARMneon_VRHADDS:
//ZZ       case ARMneon_VMINS:
//ZZ       case ARMneon_VMAXS:
//ZZ       case ARMneon_VQADDS:
//ZZ       case ARMneon_VQSUBS:
//ZZ       case ARMneon_VCGTS:
//ZZ       case ARMneon_VCGES:
//ZZ       case ARMneon_VQDMULL:
//ZZ       case ARMneon_VMULLS:
//ZZ       case ARMneon_VPMINS:
//ZZ       case ARMneon_VPMAXS:
//ZZ       case ARMneon_VQDMULH:
//ZZ       case ARMneon_VQRDMULH:
//ZZ          return ".s";
//ZZ       case ARMneon_VMULP:
//ZZ       case ARMneon_VMULLP:
//ZZ          return ".p";
//ZZ       case ARMneon_VADDFP:
//ZZ       case ARMneon_VABDFP:
//ZZ       case ARMneon_VPADDFP:
//ZZ       case ARMneon_VSUBFP:
//ZZ       case ARMneon_VMULFP:
//ZZ       case ARMneon_VMINF:
//ZZ       case ARMneon_VMAXF:
//ZZ       case ARMneon_VPMINF:
//ZZ       case ARMneon_VPMAXF:
//ZZ       case ARMneon_VCGTF:
//ZZ       case ARMneon_VCGEF:
//ZZ       case ARMneon_VCEQF:
//ZZ       case ARMneon_VRECPS:
//ZZ       case ARMneon_VRSQRTS:
//ZZ          return ".f";
//ZZ       /* ... */
//ZZ       default: vpanic("showARMNeonBinOpDataType");
//ZZ    }
//ZZ }
//ZZ 
//ZZ const HChar* showARMNeonUnOp ( ARMNeonUnOp op ) {
//ZZ    switch (op) {
//ZZ       case ARMneon_COPY: return "vmov";
//ZZ       case ARMneon_COPYLS: return "vmov";
//ZZ       case ARMneon_COPYLU: return "vmov";
//ZZ       case ARMneon_COPYN: return "vmov";
//ZZ       case ARMneon_COPYQNSS: return "vqmovn";
//ZZ       case ARMneon_COPYQNUS: return "vqmovun";
//ZZ       case ARMneon_COPYQNUU: return "vqmovn";
//ZZ       case ARMneon_NOT: return "vmvn";
//ZZ       case ARMneon_EQZ: return "vceq";
//ZZ       case ARMneon_CNT: return "vcnt";
//ZZ       case ARMneon_CLS: return "vcls";
//ZZ       case ARMneon_CLZ: return "vclz";
//ZZ       case ARMneon_DUP: return "vdup";
//ZZ       case ARMneon_PADDLS: return "vpaddl";
//ZZ       case ARMneon_PADDLU: return "vpaddl";
//ZZ       case ARMneon_VQSHLNSS: return "vqshl";
//ZZ       case ARMneon_VQSHLNUU: return "vqshl";
//ZZ       case ARMneon_VQSHLNUS: return "vqshlu";
//ZZ       case ARMneon_REV16: return "vrev16";
//ZZ       case ARMneon_REV32: return "vrev32";
//ZZ       case ARMneon_REV64: return "vrev64";
//ZZ       case ARMneon_VCVTFtoU: return "vcvt";
//ZZ       case ARMneon_VCVTFtoS: return "vcvt";
//ZZ       case ARMneon_VCVTUtoF: return "vcvt";
//ZZ       case ARMneon_VCVTStoF: return "vcvt";
//ZZ       case ARMneon_VCVTFtoFixedU: return "vcvt";
//ZZ       case ARMneon_VCVTFtoFixedS: return "vcvt";
//ZZ       case ARMneon_VCVTFixedUtoF: return "vcvt";
//ZZ       case ARMneon_VCVTFixedStoF: return "vcvt";
//ZZ       case ARMneon_VCVTF32toF16: return "vcvt";
//ZZ       case ARMneon_VCVTF16toF32: return "vcvt";
//ZZ       case ARMneon_VRECIP: return "vrecip";
//ZZ       case ARMneon_VRECIPF: return "vrecipf";
//ZZ       case ARMneon_VNEGF: return "vneg";
//ZZ       case ARMneon_ABS: return "vabs";
//ZZ       case ARMneon_VABSFP: return "vabsfp";
//ZZ       case ARMneon_VRSQRTEFP: return "vrsqrtefp";
//ZZ       case ARMneon_VRSQRTE: return "vrsqrte";
//ZZ       /* ... */
//ZZ       default: vpanic("showARMNeonUnOp");
//ZZ    }
//ZZ }
//ZZ 
//ZZ const HChar* showARMNeonUnOpDataType ( ARMNeonUnOp op ) {
//ZZ    switch (op) {
//ZZ       case ARMneon_COPY:
//ZZ       case ARMneon_NOT:
//ZZ          return "";
//ZZ       case ARMneon_COPYN:
//ZZ       case ARMneon_EQZ:
//ZZ       case ARMneon_CNT:
//ZZ       case ARMneon_DUP:
//ZZ       case ARMneon_REV16:
//ZZ       case ARMneon_REV32:
//ZZ       case ARMneon_REV64:
//ZZ          return ".i";
//ZZ       case ARMneon_COPYLU:
//ZZ       case ARMneon_PADDLU:
//ZZ       case ARMneon_COPYQNUU:
//ZZ       case ARMneon_VQSHLNUU:
//ZZ       case ARMneon_VRECIP:
//ZZ       case ARMneon_VRSQRTE:
//ZZ          return ".u";
//ZZ       case ARMneon_CLS:
//ZZ       case ARMneon_CLZ:
//ZZ       case ARMneon_COPYLS:
//ZZ       case ARMneon_PADDLS:
//ZZ       case ARMneon_COPYQNSS:
//ZZ       case ARMneon_COPYQNUS:
//ZZ       case ARMneon_VQSHLNSS:
//ZZ       case ARMneon_VQSHLNUS:
//ZZ       case ARMneon_ABS:
//ZZ          return ".s";
//ZZ       case ARMneon_VRECIPF:
//ZZ       case ARMneon_VNEGF:
//ZZ       case ARMneon_VABSFP:
//ZZ       case ARMneon_VRSQRTEFP:
//ZZ          return ".f";
//ZZ       case ARMneon_VCVTFtoU: return ".u32.f32";
//ZZ       case ARMneon_VCVTFtoS: return ".s32.f32";
//ZZ       case ARMneon_VCVTUtoF: return ".f32.u32";
//ZZ       case ARMneon_VCVTStoF: return ".f32.s32";
//ZZ       case ARMneon_VCVTF16toF32: return ".f32.f16";
//ZZ       case ARMneon_VCVTF32toF16: return ".f16.f32";
//ZZ       case ARMneon_VCVTFtoFixedU: return ".u32.f32";
//ZZ       case ARMneon_VCVTFtoFixedS: return ".s32.f32";
//ZZ       case ARMneon_VCVTFixedUtoF: return ".f32.u32";
//ZZ       case ARMneon_VCVTFixedStoF: return ".f32.s32";
//ZZ       /* ... */
//ZZ       default: vpanic("showARMNeonUnOpDataType");
//ZZ    }
//ZZ }
//ZZ 
//ZZ const HChar* showARMNeonUnOpS ( ARMNeonUnOpS op ) {
//ZZ    switch (op) {
//ZZ       case ARMneon_SETELEM: return "vmov";
//ZZ       case ARMneon_GETELEMU: return "vmov";
//ZZ       case ARMneon_GETELEMS: return "vmov";
//ZZ       case ARMneon_VDUP: return "vdup";
//ZZ       /* ... */
//ZZ       default: vpanic("showARMNeonUnarySOp");
//ZZ    }
//ZZ }
//ZZ 
//ZZ const HChar* showARMNeonUnOpSDataType ( ARMNeonUnOpS op ) {
//ZZ    switch (op) {
//ZZ       case ARMneon_SETELEM:
//ZZ       case ARMneon_VDUP:
//ZZ          return ".i";
//ZZ       case ARMneon_GETELEMS:
//ZZ          return ".s";
//ZZ       case ARMneon_GETELEMU:
//ZZ          return ".u";
//ZZ       /* ... */
//ZZ       default: vpanic("showARMNeonUnarySOp");
//ZZ    }
//ZZ }
//ZZ 
//ZZ const HChar* showARMNeonShiftOp ( ARMNeonShiftOp op ) {
//ZZ    switch (op) {
//ZZ       case ARMneon_VSHL: return "vshl";
//ZZ       case ARMneon_VSAL: return "vshl";
//ZZ       case ARMneon_VQSHL: return "vqshl";
//ZZ       case ARMneon_VQSAL: return "vqshl";
//ZZ       /* ... */
//ZZ       default: vpanic("showARMNeonShiftOp");
//ZZ    }
//ZZ }
//ZZ 
//ZZ const HChar* showARMNeonShiftOpDataType ( ARMNeonShiftOp op ) {
//ZZ    switch (op) {
//ZZ       case ARMneon_VSHL:
//ZZ       case ARMneon_VQSHL:
//ZZ          return ".u";
//ZZ       case ARMneon_VSAL:
//ZZ       case ARMneon_VQSAL:
//ZZ          return ".s";
//ZZ       /* ... */
//ZZ       default: vpanic("showARMNeonShiftOpDataType");
//ZZ    }
//ZZ }
//ZZ 
//ZZ const HChar* showARMNeonDualOp ( ARMNeonDualOp op ) {
//ZZ    switch (op) {
//ZZ       case ARMneon_TRN: return "vtrn";
//ZZ       case ARMneon_ZIP: return "vzip";
//ZZ       case ARMneon_UZP: return "vuzp";
//ZZ       /* ... */
//ZZ       default: vpanic("showARMNeonDualOp");
//ZZ    }
//ZZ }
//ZZ 
//ZZ const HChar* showARMNeonDualOpDataType ( ARMNeonDualOp op ) {
//ZZ    switch (op) {
//ZZ       case ARMneon_TRN:
//ZZ       case ARMneon_ZIP:
//ZZ       case ARMneon_UZP:
//ZZ          return "i";
//ZZ       /* ... */
//ZZ       default: vpanic("showARMNeonDualOp");
//ZZ    }
//ZZ }
//ZZ 
//ZZ static const HChar* showARMNeonDataSize_wrk ( UInt size )
//ZZ {
//ZZ    switch (size) {
//ZZ       case 0: return "8";
//ZZ       case 1: return "16";
//ZZ       case 2: return "32";
//ZZ       case 3: return "64";
//ZZ       default: vpanic("showARMNeonDataSize");
//ZZ    }
//ZZ }
//ZZ 
//ZZ static const HChar* showARMNeonDataSize ( ARMInstr* i )
//ZZ {
//ZZ    switch (i->tag) {
//ZZ       case ARMin_NBinary:
//ZZ          if (i->ARMin.NBinary.op == ARMneon_VEXT)
//ZZ             return "8";
//ZZ          if (i->ARMin.NBinary.op == ARMneon_VAND ||
//ZZ              i->ARMin.NBinary.op == ARMneon_VORR ||
//ZZ              i->ARMin.NBinary.op == ARMneon_VXOR)
//ZZ             return "";
//ZZ          return showARMNeonDataSize_wrk(i->ARMin.NBinary.size);
//ZZ       case ARMin_NUnary:
//ZZ          if (i->ARMin.NUnary.op == ARMneon_COPY ||
//ZZ              i->ARMin.NUnary.op == ARMneon_NOT ||
//ZZ              i->ARMin.NUnary.op == ARMneon_VCVTF32toF16||
//ZZ              i->ARMin.NUnary.op == ARMneon_VCVTF16toF32||
//ZZ              i->ARMin.NUnary.op == ARMneon_VCVTFtoFixedS ||
//ZZ              i->ARMin.NUnary.op == ARMneon_VCVTFtoFixedU ||
//ZZ              i->ARMin.NUnary.op == ARMneon_VCVTFixedStoF ||
//ZZ              i->ARMin.NUnary.op == ARMneon_VCVTFixedUtoF ||
//ZZ              i->ARMin.NUnary.op == ARMneon_VCVTFtoS ||
//ZZ              i->ARMin.NUnary.op == ARMneon_VCVTFtoU ||
//ZZ              i->ARMin.NUnary.op == ARMneon_VCVTStoF ||
//ZZ              i->ARMin.NUnary.op == ARMneon_VCVTUtoF)
//ZZ             return "";
//ZZ          if (i->ARMin.NUnary.op == ARMneon_VQSHLNSS ||
//ZZ              i->ARMin.NUnary.op == ARMneon_VQSHLNUU ||
//ZZ              i->ARMin.NUnary.op == ARMneon_VQSHLNUS) {
//ZZ             UInt size;
//ZZ             size = i->ARMin.NUnary.size;
//ZZ             if (size & 0x40)
//ZZ                return "64";
//ZZ             if (size & 0x20)
//ZZ                return "32";
//ZZ             if (size & 0x10)
//ZZ                return "16";
//ZZ             if (size & 0x08)
//ZZ                return "8";
//ZZ             vpanic("showARMNeonDataSize");
//ZZ          }
//ZZ          return showARMNeonDataSize_wrk(i->ARMin.NUnary.size);
//ZZ       case ARMin_NUnaryS:
//ZZ          if (i->ARMin.NUnaryS.op == ARMneon_VDUP) {
//ZZ             int size;
//ZZ             size = i->ARMin.NUnaryS.size;
//ZZ             if ((size & 1) == 1)
//ZZ                return "8";
//ZZ             if ((size & 3) == 2)
//ZZ                return "16";
//ZZ             if ((size & 7) == 4)
//ZZ                return "32";
//ZZ             vpanic("showARMNeonDataSize");
//ZZ          }
//ZZ          return showARMNeonDataSize_wrk(i->ARMin.NUnaryS.size);
//ZZ       case ARMin_NShift:
//ZZ          return showARMNeonDataSize_wrk(i->ARMin.NShift.size);
//ZZ       case ARMin_NDual:
//ZZ          return showARMNeonDataSize_wrk(i->ARMin.NDual.size);
//ZZ       default:
//ZZ          vpanic("showARMNeonDataSize");
//ZZ    }
//ZZ }

ARM64Instr* ARM64Instr_Arith ( HReg dst,
                               HReg argL, ARM64RIA* argR, Bool isAdd ) {
   ARM64Instr* i = LibVEX_Alloc(sizeof(ARM64Instr));
   i->tag                 = ARM64in_Arith;
   i->ARM64in.Arith.dst   = dst;
   i->ARM64in.Arith.argL  = argL;
   i->ARM64in.Arith.argR  = argR;
   i->ARM64in.Arith.isAdd = isAdd;
   return i;
}
ARM64Instr* ARM64Instr_Cmp ( HReg argL, ARM64RIA* argR, Bool is64 ) {
   ARM64Instr* i = LibVEX_Alloc(sizeof(ARM64Instr));
   i->tag              = ARM64in_Cmp;
   i->ARM64in.Cmp.argL = argL;
   i->ARM64in.Cmp.argR = argR;
   i->ARM64in.Cmp.is64 = is64;
   return i;
}
ARM64Instr* ARM64Instr_Logic ( HReg dst,
                               HReg argL, ARM64RIL* argR, ARM64LogicOp op ) {
   ARM64Instr* i = LibVEX_Alloc(sizeof(ARM64Instr));
   i->tag                 = ARM64in_Logic;
   i->ARM64in.Logic.dst   = dst;
   i->ARM64in.Logic.argL  = argL;
   i->ARM64in.Logic.argR  = argR;
   i->ARM64in.Logic.op    = op;
   return i;
}
ARM64Instr* ARM64Instr_Test ( HReg argL, ARM64RIL* argR ) {
   ARM64Instr* i = LibVEX_Alloc(sizeof(ARM64Instr));
   i->tag               = ARM64in_Test;
   i->ARM64in.Test.argL = argL;
   i->ARM64in.Test.argR = argR;
   return i;
}
ARM64Instr* ARM64Instr_Shift ( HReg dst,
                               HReg argL, ARM64RI6* argR, ARM64ShiftOp op ) {
   ARM64Instr* i = LibVEX_Alloc(sizeof(ARM64Instr));
   i->tag                = ARM64in_Shift;
   i->ARM64in.Shift.dst  = dst;
   i->ARM64in.Shift.argL = argL;
   i->ARM64in.Shift.argR = argR;
   i->ARM64in.Shift.op   = op;
   return i;
}
ARM64Instr* ARM64Instr_Unary ( HReg dst, HReg src, ARM64UnaryOp op ) {
   ARM64Instr* i = LibVEX_Alloc(sizeof(ARM64Instr));
   i->tag               = ARM64in_Unary;
   i->ARM64in.Unary.dst = dst;
   i->ARM64in.Unary.src = src;
   i->ARM64in.Unary.op  = op;
   return i;
}
ARM64Instr* ARM64Instr_MovI ( HReg dst, HReg src ) {
   ARM64Instr* i      = LibVEX_Alloc(sizeof(ARM64Instr));
   i->tag             = ARM64in_MovI;
   i->ARM64in.MovI.dst = dst;
   i->ARM64in.MovI.src = src;
   vassert(hregClass(src) == HRcInt64);
   vassert(hregClass(dst) == HRcInt64);
   return i;
}
ARM64Instr* ARM64Instr_Imm64 ( HReg dst, ULong imm64 ) {
   ARM64Instr* i = LibVEX_Alloc(sizeof(ARM64Instr));
   i->tag                 = ARM64in_Imm64;
   i->ARM64in.Imm64.dst   = dst;
   i->ARM64in.Imm64.imm64 = imm64;
   return i;
}
ARM64Instr* ARM64Instr_LdSt64 ( Bool isLoad, HReg rD, ARM64AMode* amode ) {
   ARM64Instr* i = LibVEX_Alloc(sizeof(ARM64Instr));
   i->tag                   = ARM64in_LdSt64;
   i->ARM64in.LdSt64.isLoad = isLoad;
   i->ARM64in.LdSt64.rD     = rD;
   i->ARM64in.LdSt64.amode  = amode;
   return i;
}
ARM64Instr* ARM64Instr_LdSt32 ( Bool isLoad, HReg rD, ARM64AMode* amode ) {
   ARM64Instr* i = LibVEX_Alloc(sizeof(ARM64Instr));
   i->tag                   = ARM64in_LdSt32;
   i->ARM64in.LdSt32.isLoad = isLoad;
   i->ARM64in.LdSt32.rD     = rD;
   i->ARM64in.LdSt32.amode  = amode;
   return i;
}
ARM64Instr* ARM64Instr_LdSt16 ( Bool isLoad, HReg rD, ARM64AMode* amode ) {
   ARM64Instr* i = LibVEX_Alloc(sizeof(ARM64Instr));
   i->tag                   = ARM64in_LdSt16;
   i->ARM64in.LdSt16.isLoad = isLoad;
   i->ARM64in.LdSt16.rD     = rD;
   i->ARM64in.LdSt16.amode  = amode;
   return i;
}
ARM64Instr* ARM64Instr_LdSt8 ( Bool isLoad, HReg rD, ARM64AMode* amode ) {
   ARM64Instr* i = LibVEX_Alloc(sizeof(ARM64Instr));
   i->tag                  = ARM64in_LdSt8;
   i->ARM64in.LdSt8.isLoad = isLoad;
   i->ARM64in.LdSt8.rD     = rD;
   i->ARM64in.LdSt8.amode  = amode;
   return i;
}
ARM64Instr* ARM64Instr_XDirect ( Addr64 dstGA, ARM64AMode* amPC,
                                 ARM64CondCode cond, Bool toFastEP ) {
   ARM64Instr* i               = LibVEX_Alloc(sizeof(ARM64Instr));
   i->tag                      = ARM64in_XDirect;
   i->ARM64in.XDirect.dstGA    = dstGA;
   i->ARM64in.XDirect.amPC     = amPC;
   i->ARM64in.XDirect.cond     = cond;
   i->ARM64in.XDirect.toFastEP = toFastEP;
   return i;
}
ARM64Instr* ARM64Instr_XIndir ( HReg dstGA, ARM64AMode* amPC,
                                ARM64CondCode cond ) {
   ARM64Instr* i           = LibVEX_Alloc(sizeof(ARM64Instr));
   i->tag                  = ARM64in_XIndir;
   i->ARM64in.XIndir.dstGA = dstGA;
   i->ARM64in.XIndir.amPC  = amPC;
   i->ARM64in.XIndir.cond  = cond;
   return i;
}
ARM64Instr* ARM64Instr_XAssisted ( HReg dstGA, ARM64AMode* amPC,
                                   ARM64CondCode cond, IRJumpKind jk ) {
   ARM64Instr* i              = LibVEX_Alloc(sizeof(ARM64Instr));
   i->tag                     = ARM64in_XAssisted;
   i->ARM64in.XAssisted.dstGA = dstGA;
   i->ARM64in.XAssisted.amPC  = amPC;
   i->ARM64in.XAssisted.cond  = cond;
   i->ARM64in.XAssisted.jk    = jk;
   return i;
}
ARM64Instr* ARM64Instr_CSel ( HReg dst, HReg argL, HReg argR,
                              ARM64CondCode cond ) {
   ARM64Instr* i        = LibVEX_Alloc(sizeof(ARM64Instr));
   i->tag               = ARM64in_CSel;
   i->ARM64in.CSel.dst  = dst;
   i->ARM64in.CSel.argL = argL;
   i->ARM64in.CSel.argR = argR;
   i->ARM64in.CSel.cond = cond;
   return i;
}
ARM64Instr* ARM64Instr_Call ( ARM64CondCode cond, HWord target, Int nArgRegs,
                              RetLoc rloc ) {
   ARM64Instr* i = LibVEX_Alloc(sizeof(ARM64Instr));
   i->tag                   = ARM64in_Call;
   i->ARM64in.Call.cond     = cond;
   i->ARM64in.Call.target   = target;
   i->ARM64in.Call.nArgRegs = nArgRegs;
   i->ARM64in.Call.rloc     = rloc;
   vassert(is_sane_RetLoc(rloc));
   return i;
}
extern ARM64Instr* ARM64Instr_AddToSP ( Int simm ) {
   ARM64Instr* i = LibVEX_Alloc(sizeof(ARM64Instr));
   i->tag                  = ARM64in_AddToSP;
   i->ARM64in.AddToSP.simm = simm;
   vassert(-4096 < simm && simm < 4096);
   vassert(0 == (simm & 0xF));
   return i;
}
extern ARM64Instr* ARM64Instr_FromSP  ( HReg dst ) {
   ARM64Instr* i = LibVEX_Alloc(sizeof(ARM64Instr));
   i->tag                = ARM64in_FromSP;
   i->ARM64in.FromSP.dst = dst;
   return i;
}
ARM64Instr* ARM64Instr_Mul ( HReg dst, HReg argL, HReg argR,
                             ARM64MulOp op ) {
   ARM64Instr* i = LibVEX_Alloc(sizeof(ARM64Instr));
   i->tag              = ARM64in_Mul;
   i->ARM64in.Mul.dst  = dst;
   i->ARM64in.Mul.argL = argL;
   i->ARM64in.Mul.argR = argR;
   i->ARM64in.Mul.op   = op;
   return i;
}
ARM64Instr* ARM64Instr_LdrEX ( Int szB ) {
   ARM64Instr* i = LibVEX_Alloc(sizeof(ARM64Instr));
   i->tag               = ARM64in_LdrEX;
   i->ARM64in.LdrEX.szB = szB;
   vassert(szB == 8 || szB == 4 || szB == 2 || szB == 1);
   return i;
}
ARM64Instr* ARM64Instr_StrEX ( Int szB ) {
   ARM64Instr* i = LibVEX_Alloc(sizeof(ARM64Instr));
   i->tag               = ARM64in_StrEX;
   i->ARM64in.StrEX.szB = szB;
   vassert(szB == 8 || szB == 4 || szB == 2 || szB == 1);
   return i;
}
ARM64Instr* ARM64Instr_MFence ( void ) {
   ARM64Instr* i = LibVEX_Alloc(sizeof(ARM64Instr));
   i->tag        = ARM64in_MFence;
   return i;
}
//ZZ ARM64Instr* ARM64Instr_CLREX( void ) {
//ZZ    ARM64Instr* i = LibVEX_Alloc(sizeof(ARM64Instr));
//ZZ    i->tag        = ARM64in_CLREX;
//ZZ    return i;
//ZZ }
ARM64Instr* ARM64Instr_VLdStS ( Bool isLoad, HReg sD, HReg rN, UInt uimm12 ) {
   ARM64Instr* i = LibVEX_Alloc(sizeof(ARM64Instr));
   i->tag                  = ARM64in_VLdStS;
   i->ARM64in.VLdStS.isLoad = isLoad;
   i->ARM64in.VLdStS.sD     = sD;
   i->ARM64in.VLdStS.rN     = rN;
   i->ARM64in.VLdStS.uimm12 = uimm12;
   vassert(uimm12 < 16384 && 0 == (uimm12 & 3));
   return i;
}
ARM64Instr* ARM64Instr_VLdStD ( Bool isLoad, HReg dD, HReg rN, UInt uimm12 ) {
   ARM64Instr* i = LibVEX_Alloc(sizeof(ARM64Instr));
   i->tag                  = ARM64in_VLdStD;
   i->ARM64in.VLdStD.isLoad = isLoad;
   i->ARM64in.VLdStD.dD     = dD;
   i->ARM64in.VLdStD.rN     = rN;
   i->ARM64in.VLdStD.uimm12 = uimm12;
   vassert(uimm12 < 32768 && 0 == (uimm12 & 7));
   return i;
}
ARM64Instr* ARM64Instr_VLdStQ ( Bool isLoad, HReg rQ, HReg rN ) {
   ARM64Instr* i = LibVEX_Alloc(sizeof(ARM64Instr));
   i->tag                   = ARM64in_VLdStQ;
   i->ARM64in.VLdStQ.isLoad = isLoad;
   i->ARM64in.VLdStQ.rQ     = rQ;
   i->ARM64in.VLdStQ.rN     = rN;
   return i;
}
ARM64Instr* ARM64Instr_VCvtI2F ( ARM64CvtOp how, HReg rD, HReg rS ) {
   ARM64Instr* i = LibVEX_Alloc(sizeof(ARM64Instr));
   i->tag                 = ARM64in_VCvtI2F;
   i->ARM64in.VCvtI2F.how = how;
   i->ARM64in.VCvtI2F.rD  = rD;
   i->ARM64in.VCvtI2F.rS  = rS;
   return i;
}
ARM64Instr* ARM64Instr_VCvtF2I ( ARM64CvtOp how, HReg rD, HReg rS,
                                 UChar armRM ) {
   ARM64Instr* i = LibVEX_Alloc(sizeof(ARM64Instr));
   i->tag                   = ARM64in_VCvtF2I;
   i->ARM64in.VCvtF2I.how   = how;
   i->ARM64in.VCvtF2I.rD    = rD;
   i->ARM64in.VCvtF2I.rS    = rS;
   i->ARM64in.VCvtF2I.armRM = armRM;
   vassert(armRM <= 3);
   return i;
}
ARM64Instr* ARM64Instr_VCvtSD ( Bool sToD, HReg dst, HReg src ) {
   ARM64Instr* i = LibVEX_Alloc(sizeof(ARM64Instr));
   i->tag               = ARM64in_VCvtSD;
   i->ARM64in.VCvtSD.sToD = sToD;
   i->ARM64in.VCvtSD.dst  = dst;
   i->ARM64in.VCvtSD.src  = src;
   return i;
}
ARM64Instr* ARM64Instr_VUnaryD ( ARM64FpUnaryOp op, HReg dst, HReg src ) {
   ARM64Instr* i = LibVEX_Alloc(sizeof(ARM64Instr));
   i->tag                 = ARM64in_VUnaryD;
   i->ARM64in.VUnaryD.op  = op;
   i->ARM64in.VUnaryD.dst = dst;
   i->ARM64in.VUnaryD.src = src;
   return i;
}
ARM64Instr* ARM64Instr_VUnaryS ( ARM64FpUnaryOp op, HReg dst, HReg src ) {
   ARM64Instr* i = LibVEX_Alloc(sizeof(ARM64Instr));
   i->tag                 = ARM64in_VUnaryS;
   i->ARM64in.VUnaryS.op  = op;
   i->ARM64in.VUnaryS.dst = dst;
   i->ARM64in.VUnaryS.src = src;
   return i;
}
ARM64Instr* ARM64Instr_VBinD ( ARM64FpBinOp op,
                               HReg dst, HReg argL, HReg argR ) {
   ARM64Instr* i = LibVEX_Alloc(sizeof(ARM64Instr));
   i->tag                = ARM64in_VBinD;
   i->ARM64in.VBinD.op   = op;
   i->ARM64in.VBinD.dst  = dst;
   i->ARM64in.VBinD.argL = argL;
   i->ARM64in.VBinD.argR = argR;
   return i;
}
ARM64Instr* ARM64Instr_VBinS ( ARM64FpBinOp op,
                               HReg dst, HReg argL, HReg argR ) {
   ARM64Instr* i = LibVEX_Alloc(sizeof(ARM64Instr));
   i->tag                = ARM64in_VBinS;
   i->ARM64in.VBinS.op   = op;
   i->ARM64in.VBinS.dst  = dst;
   i->ARM64in.VBinS.argL = argL;
   i->ARM64in.VBinS.argR = argR;
   return i;
}
ARM64Instr* ARM64Instr_VCmpD ( HReg argL, HReg argR ) {
   ARM64Instr* i = LibVEX_Alloc(sizeof(ARM64Instr));
   i->tag                = ARM64in_VCmpD;
   i->ARM64in.VCmpD.argL = argL;
   i->ARM64in.VCmpD.argR = argR;
   return i;
}
ARM64Instr* ARM64Instr_VCmpS ( HReg argL, HReg argR ) {
   ARM64Instr* i = LibVEX_Alloc(sizeof(ARM64Instr));
   i->tag                = ARM64in_VCmpS;
   i->ARM64in.VCmpS.argL = argL;
   i->ARM64in.VCmpS.argR = argR;
   return i;
}
ARM64Instr* ARM64Instr_FPCR ( Bool toFPCR, HReg iReg ) {
   ARM64Instr* i = LibVEX_Alloc(sizeof(ARM64Instr));
   i->tag                 = ARM64in_FPCR;
   i->ARM64in.FPCR.toFPCR = toFPCR;
   i->ARM64in.FPCR.iReg   = iReg;
   return i;
}
ARM64Instr* ARM64Instr_VBinV ( ARM64VecBinOp op,
                               HReg dst, HReg argL, HReg argR ) {
   ARM64Instr* i = LibVEX_Alloc(sizeof(ARM64Instr));
   i->tag                = ARM64in_VBinV;
   i->ARM64in.VBinV.op   = op;
   i->ARM64in.VBinV.dst  = dst;
   i->ARM64in.VBinV.argL = argL;
   i->ARM64in.VBinV.argR = argR;
   return i;
}
ARM64Instr* ARM64Instr_VUnaryV ( ARM64VecUnaryOp op, HReg dst, HReg arg ) {
   ARM64Instr* i = LibVEX_Alloc(sizeof(ARM64Instr));
   i->tag                 = ARM64in_VUnaryV;
   i->ARM64in.VUnaryV.op  = op;
   i->ARM64in.VUnaryV.dst = dst;
   i->ARM64in.VUnaryV.arg = arg;
   return i;
}
ARM64Instr* ARM64Instr_VNarrowV ( UInt dszBlg2, HReg dst, HReg src ) {
   ARM64Instr* i = LibVEX_Alloc(sizeof(ARM64Instr));
   i->tag                      = ARM64in_VNarrowV;
   i->ARM64in.VNarrowV.dszBlg2 = dszBlg2;
   i->ARM64in.VNarrowV.dst     = dst;
   i->ARM64in.VNarrowV.src     = src;
   vassert(dszBlg2 == 0 || dszBlg2 == 1 || dszBlg2 == 2);
   return i;
}
ARM64Instr* ARM64Instr_VShiftImmV ( ARM64VecShiftOp op,
                                    HReg dst, HReg src, UInt amt ) {
   ARM64Instr* i = LibVEX_Alloc(sizeof(ARM64Instr));
   i->tag                    = ARM64in_VShiftImmV;
   i->ARM64in.VShiftImmV.op  = op;
   i->ARM64in.VShiftImmV.dst = dst;
   i->ARM64in.VShiftImmV.src = src;
   i->ARM64in.VShiftImmV.amt = amt;
   UInt maxSh = 0;
   switch (op) {
      case ARM64vecsh_USHR64x2: case ARM64vecsh_SSHR64x2:
      case ARM64vecsh_SHL64x2:
         maxSh = 63; break;
      case ARM64vecsh_USHR32x4: case ARM64vecsh_SSHR32x4:
      case ARM64vecsh_SHL32x4:
         maxSh = 31; break;
      case ARM64vecsh_USHR16x8: case ARM64vecsh_SSHR16x8:
      case ARM64vecsh_SHL16x8:
         maxSh = 15; break;
      case ARM64vecsh_USHR8x16: case ARM64vecsh_SSHR8x16:
      case ARM64vecsh_SHL8x16:
         maxSh = 7; break;
      default:
         vassert(0);
   }
   vassert(maxSh > 0);
   vassert(amt > 0 && amt <= maxSh);
   return i;
}
ARM64Instr* ARM64Instr_VExtV ( HReg dst, HReg srcLo, HReg srcHi, UInt amtB ) {
   ARM64Instr* i = LibVEX_Alloc(sizeof(ARM64Instr));
   i->tag                 = ARM64in_VExtV;
   i->ARM64in.VExtV.dst   = dst;
   i->ARM64in.VExtV.srcLo = srcLo;
   i->ARM64in.VExtV.srcHi = srcHi;
   i->ARM64in.VExtV.amtB  = amtB;
   vassert(amtB >= 1 && amtB <= 15);
   return i;
}
//ZZ ARMInstr* ARMInstr_VAluS ( ARMVfpOp op, HReg dst, HReg argL, HReg argR ) {
//ZZ    ARMInstr* i = LibVEX_Alloc(sizeof(ARMInstr));
//ZZ    i->tag              = ARMin_VAluS;
//ZZ    i->ARMin.VAluS.op   = op;
//ZZ    i->ARMin.VAluS.dst  = dst;
//ZZ    i->ARMin.VAluS.argL = argL;
//ZZ    i->ARMin.VAluS.argR = argR;
//ZZ    return i;
//ZZ }
//ZZ ARMInstr* ARMInstr_VCMovD ( ARMCondCode cond, HReg dst, HReg src ) {
//ZZ    ARMInstr* i = LibVEX_Alloc(sizeof(ARMInstr));
//ZZ    i->tag               = ARMin_VCMovD;
//ZZ    i->ARMin.VCMovD.cond = cond;
//ZZ    i->ARMin.VCMovD.dst  = dst;
//ZZ    i->ARMin.VCMovD.src  = src;
//ZZ    vassert(cond != ARMcc_AL);
//ZZ    return i;
//ZZ }
//ZZ ARMInstr* ARMInstr_VCMovS ( ARMCondCode cond, HReg dst, HReg src ) {
//ZZ    ARMInstr* i = LibVEX_Alloc(sizeof(ARMInstr));
//ZZ    i->tag               = ARMin_VCMovS;
//ZZ    i->ARMin.VCMovS.cond = cond;
//ZZ    i->ARMin.VCMovS.dst  = dst;
//ZZ    i->ARMin.VCMovS.src  = src;
//ZZ    vassert(cond != ARMcc_AL);
//ZZ    return i;
//ZZ }
//ZZ ARMInstr* ARMInstr_VXferD ( Bool toD, HReg dD, HReg rHi, HReg rLo ) {
//ZZ    ARMInstr* i = LibVEX_Alloc(sizeof(ARMInstr));
//ZZ    i->tag              = ARMin_VXferD;
//ZZ    i->ARMin.VXferD.toD = toD;
//ZZ    i->ARMin.VXferD.dD  = dD;
//ZZ    i->ARMin.VXferD.rHi = rHi;
//ZZ    i->ARMin.VXferD.rLo = rLo;
//ZZ    return i;
//ZZ }
//ZZ ARMInstr* ARMInstr_VXferS ( Bool toS, HReg fD, HReg rLo ) {
//ZZ    ARMInstr* i = LibVEX_Alloc(sizeof(ARMInstr));
//ZZ    i->tag              = ARMin_VXferS;
//ZZ    i->ARMin.VXferS.toS = toS;
//ZZ    i->ARMin.VXferS.fD  = fD;
//ZZ    i->ARMin.VXferS.rLo = rLo;
//ZZ    return i;
//ZZ }
//ZZ ARMInstr* ARMInstr_VCvtID ( Bool iToD, Bool syned,
//ZZ                             HReg dst, HReg src ) {
//ZZ    ARMInstr* i = LibVEX_Alloc(sizeof(ARMInstr));
//ZZ    i->tag                = ARMin_VCvtID;
//ZZ    i->ARMin.VCvtID.iToD  = iToD;
//ZZ    i->ARMin.VCvtID.syned = syned;
//ZZ    i->ARMin.VCvtID.dst   = dst;
//ZZ    i->ARMin.VCvtID.src   = src;
//ZZ    return i;
//ZZ }
//ZZ ARMInstr* ARMInstr_NLdStD ( Bool isLoad, HReg dD, ARMAModeN *amode ) {
//ZZ    ARMInstr* i = LibVEX_Alloc(sizeof(ARMInstr));
//ZZ    i->tag                  = ARMin_NLdStD;
//ZZ    i->ARMin.NLdStD.isLoad  = isLoad;
//ZZ    i->ARMin.NLdStD.dD      = dD;
//ZZ    i->ARMin.NLdStD.amode   = amode;
//ZZ    return i;
//ZZ }
//ZZ 
//ZZ ARMInstr* ARMInstr_NUnary ( ARMNeonUnOp op, HReg dQ, HReg nQ,
//ZZ                             UInt size, Bool Q ) {
//ZZ    ARMInstr* i = LibVEX_Alloc(sizeof(ARMInstr));
//ZZ    i->tag                = ARMin_NUnary;
//ZZ    i->ARMin.NUnary.op   = op;
//ZZ    i->ARMin.NUnary.src  = nQ;
//ZZ    i->ARMin.NUnary.dst  = dQ;
//ZZ    i->ARMin.NUnary.size = size;
//ZZ    i->ARMin.NUnary.Q    = Q;
//ZZ    return i;
//ZZ }
//ZZ 
//ZZ ARMInstr* ARMInstr_NUnaryS ( ARMNeonUnOpS op, ARMNRS* dst, ARMNRS* src,
//ZZ                              UInt size, Bool Q ) {
//ZZ    ARMInstr* i = LibVEX_Alloc(sizeof(ARMInstr));
//ZZ    i->tag                = ARMin_NUnaryS;
//ZZ    i->ARMin.NUnaryS.op   = op;
//ZZ    i->ARMin.NUnaryS.src  = src;
//ZZ    i->ARMin.NUnaryS.dst  = dst;
//ZZ    i->ARMin.NUnaryS.size = size;
//ZZ    i->ARMin.NUnaryS.Q    = Q;
//ZZ    return i;
//ZZ }
//ZZ 
//ZZ ARMInstr* ARMInstr_NDual ( ARMNeonDualOp op, HReg nQ, HReg mQ,
//ZZ                            UInt size, Bool Q ) {
//ZZ    ARMInstr* i = LibVEX_Alloc(sizeof(ARMInstr));
//ZZ    i->tag                = ARMin_NDual;
//ZZ    i->ARMin.NDual.op   = op;
//ZZ    i->ARMin.NDual.arg1 = nQ;
//ZZ    i->ARMin.NDual.arg2 = mQ;
//ZZ    i->ARMin.NDual.size = size;
//ZZ    i->ARMin.NDual.Q    = Q;
//ZZ    return i;
//ZZ }
//ZZ 
//ZZ ARMInstr* ARMInstr_NBinary ( ARMNeonBinOp op,
//ZZ                              HReg dst, HReg argL, HReg argR,
//ZZ                              UInt size, Bool Q ) {
//ZZ    ARMInstr* i = LibVEX_Alloc(sizeof(ARMInstr));
//ZZ    i->tag                = ARMin_NBinary;
//ZZ    i->ARMin.NBinary.op   = op;
//ZZ    i->ARMin.NBinary.argL = argL;
//ZZ    i->ARMin.NBinary.argR = argR;
//ZZ    i->ARMin.NBinary.dst  = dst;
//ZZ    i->ARMin.NBinary.size = size;
//ZZ    i->ARMin.NBinary.Q    = Q;
//ZZ    return i;
//ZZ }

ARM64Instr* ARM64Instr_VImmQ (HReg rQ, UShort imm) {
   ARM64Instr* i = LibVEX_Alloc(sizeof(ARM64Instr));
   i->tag               = ARM64in_VImmQ;
   i->ARM64in.VImmQ.rQ  = rQ;
   i->ARM64in.VImmQ.imm = imm;
   return i;
}
ARM64Instr* ARM64Instr_VDfromX ( HReg rD, HReg rX ) {
   ARM64Instr* i = LibVEX_Alloc(sizeof(ARM64Instr));
   i->tag                = ARM64in_VDfromX;
   i->ARM64in.VDfromX.rD = rD;
   i->ARM64in.VDfromX.rX = rX;
   return i;
}
ARM64Instr* ARM64Instr_VQfromXX ( HReg rQ, HReg rXhi, HReg rXlo ) {
   ARM64Instr* i = LibVEX_Alloc(sizeof(ARM64Instr));
   i->tag                   = ARM64in_VQfromXX;
   i->ARM64in.VQfromXX.rQ   = rQ;
   i->ARM64in.VQfromXX.rXhi = rXhi;
   i->ARM64in.VQfromXX.rXlo = rXlo;
   return i;
}
ARM64Instr* ARM64Instr_VXfromQ ( HReg rX, HReg rQ, UInt laneNo ) {
   ARM64Instr* i = LibVEX_Alloc(sizeof(ARM64Instr));
   i->tag                    = ARM64in_VXfromQ;
   i->ARM64in.VXfromQ.rX     = rX;
   i->ARM64in.VXfromQ.rQ     = rQ;
   i->ARM64in.VXfromQ.laneNo = laneNo;
   vassert(laneNo <= 1);
   return i;
}
ARM64Instr* ARM64Instr_VXfromDorS ( HReg rX, HReg rDorS, Bool fromD ) {
   ARM64Instr* i = LibVEX_Alloc(sizeof(ARM64Instr));
   i->tag                      = ARM64in_VXfromDorS;
   i->ARM64in.VXfromDorS.rX    = rX;
   i->ARM64in.VXfromDorS.rDorS = rDorS;
   i->ARM64in.VXfromDorS.fromD = fromD;
   return i;
}
ARM64Instr* ARM64Instr_VMov ( UInt szB, HReg dst, HReg src ) {
   ARM64Instr* i       = LibVEX_Alloc(sizeof(ARM64Instr));
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

//ZZ ARMInstr* ARMInstr_NCMovQ ( ARMCondCode cond, HReg dst, HReg src ) {
//ZZ    ARMInstr* i = LibVEX_Alloc(sizeof(ARMInstr));
//ZZ    i->tag               = ARMin_NCMovQ;
//ZZ    i->ARMin.NCMovQ.cond = cond;
//ZZ    i->ARMin.NCMovQ.dst  = dst;
//ZZ    i->ARMin.NCMovQ.src  = src;
//ZZ    vassert(cond != ARMcc_AL);
//ZZ    return i;
//ZZ }
//ZZ 
//ZZ ARMInstr* ARMInstr_NShift ( ARMNeonShiftOp op,
//ZZ                             HReg dst, HReg argL, HReg argR,
//ZZ                             UInt size, Bool Q ) {
//ZZ    ARMInstr* i = LibVEX_Alloc(sizeof(ARMInstr));
//ZZ    i->tag                = ARMin_NShift;
//ZZ    i->ARMin.NShift.op   = op;
//ZZ    i->ARMin.NShift.argL = argL;
//ZZ    i->ARMin.NShift.argR = argR;
//ZZ    i->ARMin.NShift.dst  = dst;
//ZZ    i->ARMin.NShift.size = size;
//ZZ    i->ARMin.NShift.Q    = Q;
//ZZ    return i;
//ZZ }
//ZZ 
//ZZ ARMInstr* ARMInstr_NShl64 ( HReg dst, HReg src, UInt amt )
//ZZ {
//ZZ    ARMInstr* i = LibVEX_Alloc(sizeof(ARMInstr));
//ZZ    i->tag              = ARMin_NShl64;
//ZZ    i->ARMin.NShl64.dst = dst;
//ZZ    i->ARMin.NShl64.src = src;
//ZZ    i->ARMin.NShl64.amt = amt;
//ZZ    vassert(amt >= 1 && amt <= 63);
//ZZ    return i;
//ZZ }
//ZZ 
//ZZ /* Helper copy-pasted from isel.c */
//ZZ static Bool fitsIn8x4 ( UInt* u8, UInt* u4, UInt u )
//ZZ {
//ZZ    UInt i;
//ZZ    for (i = 0; i < 16; i++) {
//ZZ       if (0 == (u & 0xFFFFFF00)) {
//ZZ          *u8 = u;
//ZZ          *u4 = i;
//ZZ          return True;
//ZZ       }
//ZZ       u = ROR32(u, 30);
//ZZ    }
//ZZ    vassert(i == 16);
//ZZ    return False;
//ZZ }
//ZZ 
//ZZ ARMInstr* ARMInstr_Add32 ( HReg rD, HReg rN, UInt imm32 ) {
//ZZ    UInt u8, u4;
//ZZ    ARMInstr *i = LibVEX_Alloc(sizeof(ARMInstr));
//ZZ    /* Try to generate single ADD if possible */
//ZZ    if (fitsIn8x4(&u8, &u4, imm32)) {
//ZZ       i->tag            = ARMin_Alu;
//ZZ       i->ARMin.Alu.op   = ARMalu_ADD;
//ZZ       i->ARMin.Alu.dst  = rD;
//ZZ       i->ARMin.Alu.argL = rN;
//ZZ       i->ARMin.Alu.argR = ARMRI84_I84(u8, u4);
//ZZ    } else {
//ZZ       i->tag               = ARMin_Add32;
//ZZ       i->ARMin.Add32.rD    = rD;
//ZZ       i->ARMin.Add32.rN    = rN;
//ZZ       i->ARMin.Add32.imm32 = imm32;
//ZZ    }
//ZZ    return i;
//ZZ }

ARM64Instr* ARM64Instr_EvCheck ( ARM64AMode* amCounter,
                                 ARM64AMode* amFailAddr ) {
   ARM64Instr* i                 = LibVEX_Alloc(sizeof(ARM64Instr));
   i->tag                        = ARM64in_EvCheck;
   i->ARM64in.EvCheck.amCounter  = amCounter;
   i->ARM64in.EvCheck.amFailAddr = amFailAddr;
   return i;
}

//ZZ ARMInstr* ARMInstr_ProfInc ( void ) {
//ZZ    ARMInstr* i = LibVEX_Alloc(sizeof(ARMInstr));
//ZZ    i->tag      = ARMin_ProfInc;
//ZZ    return i;
//ZZ }

/* ... */

void ppARM64Instr ( ARM64Instr* i ) {
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
         vex_printf("0x%lx [nArgRegs=%d, ",
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
      case ARM64in_MFence:
         vex_printf("(mfence) dsb sy; dmb sy; isb");
         return;
//ZZ       case ARM64in_CLREX:
//ZZ          vex_printf("clrex");
//ZZ          return;
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
      case ARM64in_VBinV: {
         const HChar* nm = "??";
         const HChar* ar = "??";
         showARM64VecBinOp(&nm, &ar, i->ARM64in.VBinV.op);
         vex_printf("%s   ", nm);
         ppHRegARM64(i->ARM64in.VBinV.dst);
         vex_printf(".%s, ", ar);
         ppHRegARM64(i->ARM64in.VBinV.argL);
         vex_printf(".%s, ", ar);
         ppHRegARM64(i->ARM64in.VBinV.argR);
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
         vex_printf("xtn    ");
         ppHRegARM64(i->ARM64in.VNarrowV.dst);
         vex_printf(".%s, ", dszBlg2 < 3 ? darr[dszBlg2] : "??");
         ppHRegARM64(i->ARM64in.VNarrowV.src);
         vex_printf(".%s", dszBlg2 < 3 ? sarr[dszBlg2] : "??");
         return;
      }
      case ARM64in_VShiftImmV: {
         const HChar* nm = "??";
         const HChar* ar = "??";
         showARM64VecShiftOp(&nm, &ar, i->ARM64in.VShiftImmV.op);
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
//ZZ       case ARMin_VAluS:
//ZZ          vex_printf("f%-3ss ", showARMVfpOp(i->ARMin.VAluS.op));
//ZZ          ppHRegARM(i->ARMin.VAluS.dst);
//ZZ          vex_printf(", ");
//ZZ          ppHRegARM(i->ARMin.VAluS.argL);
//ZZ          vex_printf(", ");
//ZZ          ppHRegARM(i->ARMin.VAluS.argR);
//ZZ          return;
//ZZ       case ARMin_VCMovD:
//ZZ          vex_printf("fcpyd%s ", showARMCondCode(i->ARMin.VCMovD.cond));
//ZZ          ppHRegARM(i->ARMin.VCMovD.dst);
//ZZ          vex_printf(", ");
//ZZ          ppHRegARM(i->ARMin.VCMovD.src);
//ZZ          return;
//ZZ       case ARMin_VCMovS:
//ZZ          vex_printf("fcpys%s ", showARMCondCode(i->ARMin.VCMovS.cond));
//ZZ          ppHRegARM(i->ARMin.VCMovS.dst);
//ZZ          vex_printf(", ");
//ZZ          ppHRegARM(i->ARMin.VCMovS.src);
//ZZ          return;
//ZZ       case ARMin_VXferD:
//ZZ          vex_printf("vmov  ");
//ZZ          if (i->ARMin.VXferD.toD) {
//ZZ             ppHRegARM(i->ARMin.VXferD.dD);
//ZZ             vex_printf(", ");
//ZZ             ppHRegARM(i->ARMin.VXferD.rLo);
//ZZ             vex_printf(", ");
//ZZ             ppHRegARM(i->ARMin.VXferD.rHi);
//ZZ          } else {
//ZZ             ppHRegARM(i->ARMin.VXferD.rLo);
//ZZ             vex_printf(", ");
//ZZ             ppHRegARM(i->ARMin.VXferD.rHi);
//ZZ             vex_printf(", ");
//ZZ             ppHRegARM(i->ARMin.VXferD.dD);
//ZZ          }
//ZZ          return;
//ZZ       case ARMin_VXferS:
//ZZ          vex_printf("vmov  ");
//ZZ          if (i->ARMin.VXferS.toS) {
//ZZ             ppHRegARM(i->ARMin.VXferS.fD);
//ZZ             vex_printf(", ");
//ZZ             ppHRegARM(i->ARMin.VXferS.rLo);
//ZZ          } else {
//ZZ             ppHRegARM(i->ARMin.VXferS.rLo);
//ZZ             vex_printf(", ");
//ZZ             ppHRegARM(i->ARMin.VXferS.fD);
//ZZ          }
//ZZ          return;
//ZZ       case ARMin_VCvtID: {
//ZZ          const HChar* nm = "?";
//ZZ          if (i->ARMin.VCvtID.iToD) {
//ZZ             nm = i->ARMin.VCvtID.syned ? "fsitod" : "fuitod";
//ZZ          } else {
//ZZ             nm = i->ARMin.VCvtID.syned ? "ftosid" : "ftouid";
//ZZ          }
//ZZ          vex_printf("%s ", nm);
//ZZ          ppHRegARM(i->ARMin.VCvtID.dst);
//ZZ          vex_printf(", ");
//ZZ          ppHRegARM(i->ARMin.VCvtID.src);
//ZZ          return;
//ZZ       }
//ZZ       case ARMin_NLdStD:
//ZZ          if (i->ARMin.NLdStD.isLoad)
//ZZ             vex_printf("vld1.32 {");
//ZZ          else
//ZZ             vex_printf("vst1.32 {");
//ZZ          ppHRegARM(i->ARMin.NLdStD.dD);
//ZZ          vex_printf("} ");
//ZZ          ppARMAModeN(i->ARMin.NLdStD.amode);
//ZZ          return;
//ZZ       case ARMin_NUnary:
//ZZ          vex_printf("%s%s%s  ",
//ZZ                     showARMNeonUnOp(i->ARMin.NUnary.op),
//ZZ                     showARMNeonUnOpDataType(i->ARMin.NUnary.op),
//ZZ                     showARMNeonDataSize(i));
//ZZ          ppHRegARM(i->ARMin.NUnary.dst);
//ZZ          vex_printf(", ");
//ZZ          ppHRegARM(i->ARMin.NUnary.src);
//ZZ          if (i->ARMin.NUnary.op == ARMneon_EQZ)
//ZZ             vex_printf(", #0");
//ZZ          if (i->ARMin.NUnary.op == ARMneon_VCVTFtoFixedS ||
//ZZ              i->ARMin.NUnary.op == ARMneon_VCVTFtoFixedU ||
//ZZ              i->ARMin.NUnary.op == ARMneon_VCVTFixedStoF ||
//ZZ              i->ARMin.NUnary.op == ARMneon_VCVTFixedUtoF) {
//ZZ             vex_printf(", #%d", i->ARMin.NUnary.size);
//ZZ          }
//ZZ          if (i->ARMin.NUnary.op == ARMneon_VQSHLNSS ||
//ZZ              i->ARMin.NUnary.op == ARMneon_VQSHLNUU ||
//ZZ              i->ARMin.NUnary.op == ARMneon_VQSHLNUS) {
//ZZ             UInt size;
//ZZ             size = i->ARMin.NUnary.size;
//ZZ             if (size & 0x40) {
//ZZ                vex_printf(", #%d", size - 64);
//ZZ             } else if (size & 0x20) {
//ZZ                vex_printf(", #%d", size - 32);
//ZZ             } else if (size & 0x10) {
//ZZ                vex_printf(", #%d", size - 16);
//ZZ             } else if (size & 0x08) {
//ZZ                vex_printf(", #%d", size - 8);
//ZZ             }
//ZZ          }
//ZZ          return;
//ZZ       case ARMin_NUnaryS:
//ZZ          vex_printf("%s%s%s  ",
//ZZ                     showARMNeonUnOpS(i->ARMin.NUnaryS.op),
//ZZ                     showARMNeonUnOpSDataType(i->ARMin.NUnaryS.op),
//ZZ                     showARMNeonDataSize(i));
//ZZ          ppARMNRS(i->ARMin.NUnaryS.dst);
//ZZ          vex_printf(", ");
//ZZ          ppARMNRS(i->ARMin.NUnaryS.src);
//ZZ          return;
//ZZ       case ARMin_NShift:
//ZZ          vex_printf("%s%s%s  ",
//ZZ                     showARMNeonShiftOp(i->ARMin.NShift.op),
//ZZ                     showARMNeonShiftOpDataType(i->ARMin.NShift.op),
//ZZ                     showARMNeonDataSize(i));
//ZZ          ppHRegARM(i->ARMin.NShift.dst);
//ZZ          vex_printf(", ");
//ZZ          ppHRegARM(i->ARMin.NShift.argL);
//ZZ          vex_printf(", ");
//ZZ          ppHRegARM(i->ARMin.NShift.argR);
//ZZ          return;
//ZZ       case ARMin_NShl64:
//ZZ          vex_printf("vshl.i64 ");
//ZZ          ppHRegARM(i->ARMin.NShl64.dst);
//ZZ          vex_printf(", ");
//ZZ          ppHRegARM(i->ARMin.NShl64.src);
//ZZ          vex_printf(", #%u", i->ARMin.NShl64.amt);
//ZZ          return;
//ZZ       case ARMin_NDual:
//ZZ          vex_printf("%s%s%s  ",
//ZZ                     showARMNeonDualOp(i->ARMin.NDual.op),
//ZZ                     showARMNeonDualOpDataType(i->ARMin.NDual.op),
//ZZ                     showARMNeonDataSize(i));
//ZZ          ppHRegARM(i->ARMin.NDual.arg1);
//ZZ          vex_printf(", ");
//ZZ          ppHRegARM(i->ARMin.NDual.arg2);
//ZZ          return;
//ZZ       case ARMin_NBinary:
//ZZ          vex_printf("%s%s%s",
//ZZ                     showARMNeonBinOp(i->ARMin.NBinary.op),
//ZZ                     showARMNeonBinOpDataType(i->ARMin.NBinary.op),
//ZZ                     showARMNeonDataSize(i));
//ZZ          vex_printf("  ");
//ZZ          ppHRegARM(i->ARMin.NBinary.dst);
//ZZ          vex_printf(", ");
//ZZ          ppHRegARM(i->ARMin.NBinary.argL);
//ZZ          vex_printf(", ");
//ZZ          ppHRegARM(i->ARMin.NBinary.argR);
//ZZ          return;
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
//ZZ        case ARMin_NCMovQ:
//ZZ          vex_printf("vmov%s ", showARMCondCode(i->ARMin.NCMovQ.cond));
//ZZ          ppHRegARM(i->ARMin.NCMovQ.dst);
//ZZ          vex_printf(", ");
//ZZ          ppHRegARM(i->ARMin.NCMovQ.src);
//ZZ          return;
//ZZ       case ARMin_Add32:
//ZZ          vex_printf("add32 ");
//ZZ          ppHRegARM(i->ARMin.Add32.rD);
//ZZ          vex_printf(", ");
//ZZ          ppHRegARM(i->ARMin.Add32.rN);
//ZZ          vex_printf(", ");
//ZZ          vex_printf("%d", i->ARMin.Add32.imm32);
//ZZ          return;
      case ARM64in_EvCheck:
         vex_printf("(evCheck) ldr w9,");
         ppARM64AMode(i->ARM64in.EvCheck.amCounter);
         vex_printf("; subs w9,w9,$1; str w9,");
         ppARM64AMode(i->ARM64in.EvCheck.amCounter);
         vex_printf("; bpl nofail; ldr x9,");
         ppARM64AMode(i->ARM64in.EvCheck.amFailAddr);
         vex_printf("; br x9; nofail:");
         return;
//ZZ       case ARMin_ProfInc:
//ZZ          vex_printf("(profInc) movw r12,LO16($NotKnownYet); "
//ZZ                     "movw r12,HI16($NotKnownYet); "
//ZZ                     "ldr r11,[r12]; "
//ZZ                     "adds r11,r11,$1; "
//ZZ                     "str r11,[r12]; "
//ZZ                     "ldr r11,[r12+4]; "
//ZZ                     "adc r11,r11,$0; "
//ZZ                     "str r11,[r12+4]");
//ZZ          return;
      default:
         vex_printf("ppARM64Instr: unhandled case (tag %d)", (Int)i->tag);
         vpanic("ppARM64Instr(1)");
         return;
   }
}


/* --------- Helpers for register allocation. --------- */

void getRegUsage_ARM64Instr ( HRegUsage* u, ARM64Instr* i, Bool mode64 )
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
      case ARM64in_MovI:
         addHRegUse(u, HRmWrite, i->ARM64in.MovI.dst);
         addHRegUse(u, HRmRead,  i->ARM64in.MovI.src);
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
      case ARM64in_MFence:
         return;
//ZZ       case ARMin_CLREX:
//ZZ          return;
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
      case ARM64in_VUnaryD:
         addHRegUse(u, HRmWrite, i->ARM64in.VUnaryD.dst);
         addHRegUse(u, HRmRead, i->ARM64in.VUnaryD.src);
         return;
      case ARM64in_VUnaryS:
         addHRegUse(u, HRmWrite, i->ARM64in.VUnaryS.dst);
         addHRegUse(u, HRmRead, i->ARM64in.VUnaryS.src);
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
      case ARM64in_VCmpD:
         addHRegUse(u, HRmRead, i->ARM64in.VCmpD.argL);
         addHRegUse(u, HRmRead, i->ARM64in.VCmpD.argR);
         return;
      case ARM64in_VCmpS:
         addHRegUse(u, HRmRead, i->ARM64in.VCmpS.argL);
         addHRegUse(u, HRmRead, i->ARM64in.VCmpS.argR);
         return;
      case ARM64in_FPCR:
         if (i->ARM64in.FPCR.toFPCR)
            addHRegUse(u, HRmRead, i->ARM64in.FPCR.iReg);
         else
            addHRegUse(u, HRmWrite, i->ARM64in.FPCR.iReg);
         return;
      case ARM64in_VBinV:
         addHRegUse(u, HRmWrite, i->ARM64in.VBinV.dst);
         addHRegUse(u, HRmRead, i->ARM64in.VBinV.argL);
         addHRegUse(u, HRmRead, i->ARM64in.VBinV.argR);
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
//ZZ       case ARMin_VAluS:
//ZZ          addHRegUse(u, HRmWrite, i->ARMin.VAluS.dst);
//ZZ          addHRegUse(u, HRmRead, i->ARMin.VAluS.argL);
//ZZ          addHRegUse(u, HRmRead, i->ARMin.VAluS.argR);
//ZZ          return;
//ZZ       case ARMin_VUnaryS:
//ZZ          addHRegUse(u, HRmWrite, i->ARMin.VUnaryS.dst);
//ZZ          addHRegUse(u, HRmRead, i->ARMin.VUnaryS.src);
//ZZ          return;
//ZZ       case ARMin_VCMovD:
//ZZ          addHRegUse(u, HRmWrite, i->ARMin.VCMovD.dst);
//ZZ          addHRegUse(u, HRmRead,  i->ARMin.VCMovD.dst);
//ZZ          addHRegUse(u, HRmRead,  i->ARMin.VCMovD.src);
//ZZ          return;
//ZZ       case ARMin_VCMovS:
//ZZ          addHRegUse(u, HRmWrite, i->ARMin.VCMovS.dst);
//ZZ          addHRegUse(u, HRmRead,  i->ARMin.VCMovS.dst);
//ZZ          addHRegUse(u, HRmRead,  i->ARMin.VCMovS.src);
//ZZ          return;
//ZZ       case ARMin_VXferD:
//ZZ          if (i->ARMin.VXferD.toD) {
//ZZ             addHRegUse(u, HRmWrite, i->ARMin.VXferD.dD);
//ZZ             addHRegUse(u, HRmRead,  i->ARMin.VXferD.rHi);
//ZZ             addHRegUse(u, HRmRead,  i->ARMin.VXferD.rLo);
//ZZ          } else {
//ZZ             addHRegUse(u, HRmRead,  i->ARMin.VXferD.dD);
//ZZ             addHRegUse(u, HRmWrite, i->ARMin.VXferD.rHi);
//ZZ             addHRegUse(u, HRmWrite, i->ARMin.VXferD.rLo);
//ZZ          }
//ZZ          return;
//ZZ       case ARMin_VXferS:
//ZZ          if (i->ARMin.VXferS.toS) {
//ZZ             addHRegUse(u, HRmWrite, i->ARMin.VXferS.fD);
//ZZ             addHRegUse(u, HRmRead,  i->ARMin.VXferS.rLo);
//ZZ          } else {
//ZZ             addHRegUse(u, HRmRead,  i->ARMin.VXferS.fD);
//ZZ             addHRegUse(u, HRmWrite, i->ARMin.VXferS.rLo);
//ZZ          }
//ZZ          return;
//ZZ       case ARMin_VCvtID:
//ZZ          addHRegUse(u, HRmWrite, i->ARMin.VCvtID.dst);
//ZZ          addHRegUse(u, HRmRead,  i->ARMin.VCvtID.src);
//ZZ          return;
//ZZ       case ARMin_NLdStD:
//ZZ          if (i->ARMin.NLdStD.isLoad)
//ZZ             addHRegUse(u, HRmWrite, i->ARMin.NLdStD.dD);
//ZZ          else
//ZZ             addHRegUse(u, HRmRead, i->ARMin.NLdStD.dD);
//ZZ          addRegUsage_ARMAModeN(u, i->ARMin.NLdStD.amode);
//ZZ          return;
//ZZ       case ARMin_NUnary:
//ZZ          addHRegUse(u, HRmWrite, i->ARMin.NUnary.dst);
//ZZ          addHRegUse(u, HRmRead, i->ARMin.NUnary.src);
//ZZ          return;
//ZZ       case ARMin_NUnaryS:
//ZZ          addHRegUse(u, HRmWrite, i->ARMin.NUnaryS.dst->reg);
//ZZ          addHRegUse(u, HRmRead, i->ARMin.NUnaryS.src->reg);
//ZZ          return;
//ZZ       case ARMin_NShift:
//ZZ          addHRegUse(u, HRmWrite, i->ARMin.NShift.dst);
//ZZ          addHRegUse(u, HRmRead, i->ARMin.NShift.argL);
//ZZ          addHRegUse(u, HRmRead, i->ARMin.NShift.argR);
//ZZ          return;
//ZZ       case ARMin_NShl64:
//ZZ          addHRegUse(u, HRmWrite, i->ARMin.NShl64.dst);
//ZZ          addHRegUse(u, HRmRead, i->ARMin.NShl64.src);
//ZZ          return;
//ZZ       case ARMin_NDual:
//ZZ          addHRegUse(u, HRmWrite, i->ARMin.NDual.arg1);
//ZZ          addHRegUse(u, HRmWrite, i->ARMin.NDual.arg2);
//ZZ          addHRegUse(u, HRmRead, i->ARMin.NDual.arg1);
//ZZ          addHRegUse(u, HRmRead, i->ARMin.NDual.arg2);
//ZZ          return;
      case ARM64in_VImmQ:
         addHRegUse(u, HRmWrite, i->ARM64in.VImmQ.rQ);
         return;
      case ARM64in_VDfromX:
         addHRegUse(u, HRmWrite, i->ARM64in.VDfromX.rD);
         addHRegUse(u, HRmRead,  i->ARM64in.VDfromX.rX);
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
         return;
//ZZ       case ARMin_NBinary:
//ZZ          addHRegUse(u, HRmWrite, i->ARMin.NBinary.dst);
//ZZ          /* TODO: sometimes dst is also being read! */
//ZZ          // XXX fix this
//ZZ          addHRegUse(u, HRmRead, i->ARMin.NBinary.argL);
//ZZ          addHRegUse(u, HRmRead, i->ARMin.NBinary.argR);
//ZZ          return;
//ZZ       case ARMin_NCMovQ:
//ZZ          addHRegUse(u, HRmWrite, i->ARMin.NCMovQ.dst);
//ZZ          addHRegUse(u, HRmRead,  i->ARMin.NCMovQ.dst);
//ZZ          addHRegUse(u, HRmRead,  i->ARMin.NCMovQ.src);
//ZZ          return;
//ZZ       case ARMin_Add32:
//ZZ          addHRegUse(u, HRmWrite, i->ARMin.Add32.rD);
//ZZ          addHRegUse(u, HRmRead, i->ARMin.Add32.rN);
//ZZ          return;
      case ARM64in_EvCheck:
         /* We expect both amodes only to mention x21, so this is in
            fact pointless, since x21 isn't allocatable, but
            anyway.. */
         addRegUsage_ARM64AMode(u, i->ARM64in.EvCheck.amCounter);
         addRegUsage_ARM64AMode(u, i->ARM64in.EvCheck.amFailAddr);
         addHRegUse(u, HRmWrite, hregARM64_X9()); /* also unavail to RA */
         return;
//ZZ       case ARMin_ProfInc:
//ZZ          addHRegUse(u, HRmWrite, hregARM_R12());
//ZZ          addHRegUse(u, HRmWrite, hregARM_R11());
//ZZ          return;
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
      case ARM64in_MFence:
         return;
//ZZ       case ARMin_CLREX:
//ZZ          return;
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
      case ARM64in_VUnaryD:
         i->ARM64in.VUnaryD.dst = lookupHRegRemap(m, i->ARM64in.VUnaryD.dst);
         i->ARM64in.VUnaryD.src = lookupHRegRemap(m, i->ARM64in.VUnaryD.src);
         return;
      case ARM64in_VUnaryS:
         i->ARM64in.VUnaryS.dst = lookupHRegRemap(m, i->ARM64in.VUnaryS.dst);
         i->ARM64in.VUnaryS.src = lookupHRegRemap(m, i->ARM64in.VUnaryS.src);
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
      case ARM64in_VCmpD:
         i->ARM64in.VCmpD.argL = lookupHRegRemap(m, i->ARM64in.VCmpD.argL);
         i->ARM64in.VCmpD.argR = lookupHRegRemap(m, i->ARM64in.VCmpD.argR);
         return;
      case ARM64in_VCmpS:
         i->ARM64in.VCmpS.argL = lookupHRegRemap(m, i->ARM64in.VCmpS.argL);
         i->ARM64in.VCmpS.argR = lookupHRegRemap(m, i->ARM64in.VCmpS.argR);
         return;
      case ARM64in_FPCR:
         i->ARM64in.FPCR.iReg = lookupHRegRemap(m, i->ARM64in.FPCR.iReg);
         return;
      case ARM64in_VBinV:
         i->ARM64in.VBinV.dst  = lookupHRegRemap(m, i->ARM64in.VBinV.dst);
         i->ARM64in.VBinV.argL = lookupHRegRemap(m, i->ARM64in.VBinV.argL);
         i->ARM64in.VBinV.argR = lookupHRegRemap(m, i->ARM64in.VBinV.argR);
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

//ZZ       case ARMin_VAluS:
//ZZ          i->ARMin.VAluS.dst  = lookupHRegRemap(m, i->ARMin.VAluS.dst);
//ZZ          i->ARMin.VAluS.argL = lookupHRegRemap(m, i->ARMin.VAluS.argL);
//ZZ          i->ARMin.VAluS.argR = lookupHRegRemap(m, i->ARMin.VAluS.argR);
//ZZ          return;
//ZZ       case ARMin_VCMovD:
//ZZ          i->ARMin.VCMovD.dst = lookupHRegRemap(m, i->ARMin.VCMovD.dst);
//ZZ          i->ARMin.VCMovD.src = lookupHRegRemap(m, i->ARMin.VCMovD.src);
//ZZ          return;
//ZZ       case ARMin_VCMovS:
//ZZ          i->ARMin.VCMovS.dst = lookupHRegRemap(m, i->ARMin.VCMovS.dst);
//ZZ          i->ARMin.VCMovS.src = lookupHRegRemap(m, i->ARMin.VCMovS.src);
//ZZ          return;
//ZZ       case ARMin_VXferD:
//ZZ          i->ARMin.VXferD.dD  = lookupHRegRemap(m, i->ARMin.VXferD.dD);
//ZZ          i->ARMin.VXferD.rHi = lookupHRegRemap(m, i->ARMin.VXferD.rHi);
//ZZ          i->ARMin.VXferD.rLo = lookupHRegRemap(m, i->ARMin.VXferD.rLo);
//ZZ          return;
//ZZ       case ARMin_VXferS:
//ZZ          i->ARMin.VXferS.fD  = lookupHRegRemap(m, i->ARMin.VXferS.fD);
//ZZ          i->ARMin.VXferS.rLo = lookupHRegRemap(m, i->ARMin.VXferS.rLo);
//ZZ          return;
//ZZ       case ARMin_VCvtID:
//ZZ          i->ARMin.VCvtID.dst = lookupHRegRemap(m, i->ARMin.VCvtID.dst);
//ZZ          i->ARMin.VCvtID.src = lookupHRegRemap(m, i->ARMin.VCvtID.src);
//ZZ          return;
//ZZ       case ARMin_NLdStD:
//ZZ          i->ARMin.NLdStD.dD = lookupHRegRemap(m, i->ARMin.NLdStD.dD);
//ZZ          mapRegs_ARMAModeN(m, i->ARMin.NLdStD.amode);
//ZZ          return;
//ZZ       case ARMin_NUnary:
//ZZ          i->ARMin.NUnary.src = lookupHRegRemap(m, i->ARMin.NUnary.src);
//ZZ          i->ARMin.NUnary.dst = lookupHRegRemap(m, i->ARMin.NUnary.dst);
//ZZ          return;
//ZZ       case ARMin_NUnaryS:
//ZZ          i->ARMin.NUnaryS.src->reg
//ZZ             = lookupHRegRemap(m, i->ARMin.NUnaryS.src->reg);
//ZZ          i->ARMin.NUnaryS.dst->reg
//ZZ             = lookupHRegRemap(m, i->ARMin.NUnaryS.dst->reg);
//ZZ          return;
//ZZ       case ARMin_NShift:
//ZZ          i->ARMin.NShift.dst = lookupHRegRemap(m, i->ARMin.NShift.dst);
//ZZ          i->ARMin.NShift.argL = lookupHRegRemap(m, i->ARMin.NShift.argL);
//ZZ          i->ARMin.NShift.argR = lookupHRegRemap(m, i->ARMin.NShift.argR);
//ZZ          return;
//ZZ       case ARMin_NShl64:
//ZZ          i->ARMin.NShl64.dst = lookupHRegRemap(m, i->ARMin.NShl64.dst);
//ZZ          i->ARMin.NShl64.src = lookupHRegRemap(m, i->ARMin.NShl64.src);
//ZZ          return;
//ZZ       case ARMin_NDual:
//ZZ          i->ARMin.NDual.arg1 = lookupHRegRemap(m, i->ARMin.NDual.arg1);
//ZZ          i->ARMin.NDual.arg2 = lookupHRegRemap(m, i->ARMin.NDual.arg2);
//ZZ          return;
      case ARM64in_VImmQ:
         i->ARM64in.VImmQ.rQ = lookupHRegRemap(m, i->ARM64in.VImmQ.rQ);
         return;
      case ARM64in_VDfromX:
         i->ARM64in.VDfromX.rD
            = lookupHRegRemap(m, i->ARM64in.VDfromX.rD);
         i->ARM64in.VDfromX.rX
            = lookupHRegRemap(m, i->ARM64in.VDfromX.rX);
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

//ZZ       case ARMin_NBinary:
//ZZ          i->ARMin.NBinary.argL = lookupHRegRemap(m, i->ARMin.NBinary.argL);
//ZZ          i->ARMin.NBinary.argR = lookupHRegRemap(m, i->ARMin.NBinary.argR);
//ZZ          i->ARMin.NBinary.dst  = lookupHRegRemap(m, i->ARMin.NBinary.dst);
//ZZ          return;
//ZZ       case ARMin_NCMovQ:
//ZZ          i->ARMin.NCMovQ.dst = lookupHRegRemap(m, i->ARMin.NCMovQ.dst);
//ZZ          i->ARMin.NCMovQ.src = lookupHRegRemap(m, i->ARMin.NCMovQ.src);
//ZZ          return;
//ZZ       case ARMin_Add32:
//ZZ          i->ARMin.Add32.rD = lookupHRegRemap(m, i->ARMin.Add32.rD);
//ZZ          i->ARMin.Add32.rN = lookupHRegRemap(m, i->ARMin.Add32.rN);
//ZZ          return;
      case ARM64in_EvCheck:
         /* We expect both amodes only to mention x21, so this is in
            fact pointless, since x21 isn't allocatable, but
            anyway.. */
         mapRegs_ARM64AMode(m, i->ARM64in.EvCheck.amCounter);
         mapRegs_ARM64AMode(m, i->ARM64in.EvCheck.amFailAddr);
         return;
//ZZ       case ARMin_ProfInc:
//ZZ          /* hardwires r11 and r12 -- nothing to modify. */
//ZZ          return;
      default:
         ppARM64Instr(i);
         vpanic("mapRegs_ARM64Instr");
   }
}

/* Figure out if i represents a reg-reg move, and if so assign the
   source and destination to *src and *dst.  If in doubt say No.  Used
   by the register allocator to do move coalescing. 
*/
Bool isMove_ARM64Instr ( ARM64Instr* i, HReg* src, HReg* dst )
{
   switch (i->tag) {
      case ARM64in_MovI:
         *src = i->ARM64in.MovI.src;
         *dst = i->ARM64in.MovI.dst;
         return True;
      case ARM64in_VMov:
         *src = i->ARM64in.VMov.src;
         *dst = i->ARM64in.VMov.dst;
         return True;
      default:
         break;
   }

   return False;
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


//ZZ /* Emit an instruction into buf and return the number of bytes used.
//ZZ    Note that buf is not the insn's final place, and therefore it is
//ZZ    imperative to emit position-independent code. */

static inline UChar iregNo ( HReg r )
{
   UInt n;
   vassert(hregClass(r) == HRcInt64);
   vassert(!hregIsVirtual(r));
   n = hregNumber(r);
   vassert(n <= 30);
   return toUChar(n);
}

static inline UChar dregNo ( HReg r )
{
   UInt n;
   vassert(hregClass(r) == HRcFlt64);
   vassert(!hregIsVirtual(r));
   n = hregNumber(r);
   vassert(n <= 31);
   return toUChar(n);
}

static inline UChar qregNo ( HReg r )
{
   UInt n;
   vassert(hregClass(r) == HRcVec128);
   vassert(!hregIsVirtual(r));
   n = hregNumber(r);
   vassert(n <= 31);
   return toUChar(n);
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
#define X00110   BITS8(0,0,0, 0,0,1,1,0)
#define X00111   BITS8(0,0,0, 0,0,1,1,1)
#define X01000   BITS8(0,0,0, 0,1,0,0,0)
#define X10000   BITS8(0,0,0, 1,0,0,0,0)
#define X11000   BITS8(0,0,0, 1,1,0,0,0)
#define X11110   BITS8(0,0,0, 1,1,1,1,0)
#define X11111   BITS8(0,0,0, 1,1,1,1,1)

#define X000000  BITS8(0,0, 0,0,0,0,0,0)
#define X000001  BITS8(0,0, 0,0,0,0,0,1)
#define X000010  BITS8(0,0, 0,0,0,0,1,0)
#define X000011  BITS8(0,0, 0,0,0,0,1,1)
#define X000100  BITS8(0,0, 0,0,0,1,0,0)
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
#define X010101  BITS8(0,0, 0,1,0,1,0,1)
#define X010110  BITS8(0,0, 0,1,0,1,1,0)
#define X011001  BITS8(0,0, 0,1,1,0,0,1)
#define X011010  BITS8(0,0, 0,1,1,0,1,0)
#define X011011  BITS8(0,0, 0,1,1,0,1,1)
#define X011110  BITS8(0,0, 0,1,1,1,1,0)
#define X011111  BITS8(0,0, 0,1,1,1,1,1)
#define X100001  BITS8(0,0, 1,0,0,0,0,1)
#define X100011  BITS8(0,0, 1,0,0,0,1,1)
#define X100100  BITS8(0,0, 1,0,0,1,0,0)
#define X100101  BITS8(0,0, 1,0,0,1,0,1)
#define X100110  BITS8(0,0, 1,0,0,1,1,0)
#define X100111  BITS8(0,0, 1,0,0,1,1,1)
#define X101110  BITS8(0,0, 1,0,1,1,1,0)
#define X110000  BITS8(0,0, 1,1,0,0,0,0)
#define X110001  BITS8(0,0, 1,1,0,0,0,1)
#define X110101  BITS8(0,0, 1,1,0,1,0,1)
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
#define X11000001  BITS8(1,1,0,0,0,0,0,1)
#define X11000011  BITS8(1,1,0,0,0,0,1,1)
#define X11010100  BITS8(1,1,0,1,0,1,0,0)
#define X11010110  BITS8(1,1,0,1,0,1,1,0)
#define X11011000  BITS8(1,1,0,1,1,0,0,0)
#define X11011010  BITS8(1,1,0,1,1,0,1,0)
#define X11011110  BITS8(1,1,0,1,1,1,1,0)
#define X11110001  BITS8(1,1,1,1,0,0,0,1)
#define X11110011  BITS8(1,1,1,1,0,0,1,1)


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
//ZZ /* Generate a skeletal insn that involves an a RI84 shifter operand.
//ZZ    Returns a word which is all zeroes apart from bits 25 and 11..0,
//ZZ    since it is those that encode the shifter operand (at least to the
//ZZ    extent that we care about it.) */
//ZZ static UInt skeletal_RI84 ( ARMRI84* ri )
//ZZ {
//ZZ    UInt instr;
//ZZ    if (ri->tag == ARMri84_I84) {
//ZZ       vassert(0 == (ri->ARMri84.I84.imm4 & ~0x0F));
//ZZ       vassert(0 == (ri->ARMri84.I84.imm8 & ~0xFF));
//ZZ       instr = 1 << 25;
//ZZ       instr |= (ri->ARMri84.I84.imm4 << 8);
//ZZ       instr |= ri->ARMri84.I84.imm8;
//ZZ    } else {
//ZZ       instr = 0 << 25;
//ZZ       instr |= iregNo(ri->ARMri84.R.reg);
//ZZ    }
//ZZ    return instr;
//ZZ }
//ZZ 
//ZZ /* Ditto for RI5.  Resulting word is zeroes apart from bit 4 and bits
//ZZ    11..7. */
//ZZ static UInt skeletal_RI5 ( ARMRI5* ri )
//ZZ {
//ZZ    UInt instr;
//ZZ    if (ri->tag == ARMri5_I5) {
//ZZ       UInt imm5 = ri->ARMri5.I5.imm5;
//ZZ       vassert(imm5 >= 1 && imm5 <= 31);
//ZZ       instr = 0 << 4;
//ZZ       instr |= imm5 << 7;
//ZZ    } else {
//ZZ       instr = 1 << 4;
//ZZ       instr |= iregNo(ri->ARMri5.R.reg) << 8;
//ZZ    }
//ZZ    return instr;
//ZZ }


/* Get an immediate into a register, using only that register. */
static UInt* imm64_to_iregNo ( UInt* p, Int xD, ULong imm64 )
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
static UInt* imm64_to_iregNo_EXACTLY4 ( UInt* p, Int xD, ULong imm64 )
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
   imm64_to_iregNo_EXACTLY4(). */
static Bool is_imm64_to_iregNo_EXACTLY4 ( UInt* p, Int xD, ULong imm64 )
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
                                   iregNo(am->ARM64am.RI9.reg), wD);
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
      UInt xN    = iregNo(am->ARM64am.RI12.reg);
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
      UInt xN = iregNo(am->ARM64am.RR.base);
      UInt xM = iregNo(am->ARM64am.RR.index);
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
                                   iregNo(am->ARM64am.RI9.reg), wD);
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
      UInt xN    = iregNo(am->ARM64am.RI12.reg);
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
      UInt xN = iregNo(am->ARM64am.RR.base);
      UInt xM = iregNo(am->ARM64am.RR.index);
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
                                   iregNo(am->ARM64am.RI9.reg), wD);
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
      UInt xN    = iregNo(am->ARM64am.RI12.reg);
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
      UInt xN = iregNo(am->ARM64am.RR.base);
      UInt xM = iregNo(am->ARM64am.RR.index);
      vassert(xN <= 30);
      UInt instr = X_3_8_5_6_5_5(X101, isLoad ? X11000011 : X11000001, 
                                 xM, X011010, xN, wD);
      *p++ = instr;
      return p;
   }
   vpanic("do_load_or_store32");
   vassert(0);
}


/* Generate a 64 bit load or store to/from xD, using the given amode
   for the address. */
static UInt* do_load_or_store64 ( UInt* p,
                                  Bool isLoad, UInt xD, ARM64AMode* am )
{
   /* In all these cases, Rn can't be 31 since that means SP. */
   vassert(xD <= 30);
   if (am->tag == ARM64am_RI9) {
      /* STUR Xd, [Xn|SP + simm9]:  11 111000 000 simm9 00 n d
         LDUR Xd, [Xn|SP + simm9]:  11 111000 010 simm9 00 n d
      */
      Int simm9 = am->ARM64am.RI9.simm9;
      vassert(-256 <= simm9 && simm9 <= 255);
      UInt xN = iregNo(am->ARM64am.RI9.reg);
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
      UInt xN    = iregNo(am->ARM64am.RI12.reg);
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
      UInt xN = iregNo(am->ARM64am.RR.base);
      UInt xM = iregNo(am->ARM64am.RR.index);
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
                      UChar* buf, Int nbuf, ARM64Instr* i,
                      Bool mode64,
                      void* disp_cp_chain_me_to_slowEP,
                      void* disp_cp_chain_me_to_fastEP,
                      void* disp_cp_xindir,
                      void* disp_cp_xassisted )
{
   UInt* p = (UInt*)buf;
   vassert(nbuf >= 32);
   vassert(mode64 == True);
   vassert(0 == (((HWord)buf) & 3));

   switch (i->tag) {
      case ARM64in_Arith: {
         UInt      rD   = iregNo(i->ARM64in.Arith.dst);
         UInt      rN   = iregNo(i->ARM64in.Arith.argL);
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
               UInt rM = iregNo(i->ARM64in.Arith.argR->ARM64riA.R.reg);
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
         UInt      rN   = iregNo(i->ARM64in.Cmp.argL);
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
               UInt rM = iregNo(i->ARM64in.Cmp.argR->ARM64riA.R.reg);
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
         UInt      rD   = iregNo(i->ARM64in.Logic.dst);
         UInt      rN   = iregNo(i->ARM64in.Logic.argL);
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
               UInt rM = iregNo(argR->ARM64riL.R.reg);
               vassert(rM < 31);
               *p++ = X_3_8_5_6_5_5(opc, X01010000, rM, X000000, rN, rD);
               break;
            }
            default:
               goto bad;
         }
         goto done;
      }
      case ARM64in_Test: {
         UInt      rD   = 31; /* XZR, we are going to dump the result */
         UInt      rN   = iregNo(i->ARM64in.Test.argL);
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
         UInt      rD   = iregNo(i->ARM64in.Shift.dst);
         UInt      rN   = iregNo(i->ARM64in.Shift.argL);
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
               UInt rM = iregNo(argR->ARM64ri6.R.reg);
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
         UInt rDst = iregNo(i->ARM64in.Unary.dst);
         UInt rSrc = iregNo(i->ARM64in.Unary.src);
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
      case ARM64in_MovI: {
         /* We generate the "preferred form", ORR Xd, XZR, Xm
            101 01010 00 0 m 000000 11111 d
         */
         UInt instr = 0xAA0003E0;
         UInt d     = iregNo(i->ARM64in.MovI.dst);
         UInt m     = iregNo(i->ARM64in.MovI.src);
         *p++ = instr | ((m & 31) << 16) | ((d & 31) << 0);
         goto done;
      }
      case ARM64in_Imm64: {
         p = imm64_to_iregNo( p, iregNo(i->ARM64in.Imm64.dst),
                              i->ARM64in.Imm64.imm64 );
         goto done;
      }
      case ARM64in_LdSt64: {
         p = do_load_or_store64( p, i->ARM64in.LdSt64.isLoad,
                                 iregNo(i->ARM64in.LdSt64.rD),
                                 i->ARM64in.LdSt64.amode );
         goto done;
      }
      case ARM64in_LdSt32: {
         p = do_load_or_store32( p, i->ARM64in.LdSt32.isLoad,
                                 iregNo(i->ARM64in.LdSt32.rD),
                                 i->ARM64in.LdSt32.amode );
         goto done;
      }
      case ARM64in_LdSt16: {
         p = do_load_or_store16( p, i->ARM64in.LdSt16.isLoad,
                                 iregNo(i->ARM64in.LdSt16.rD),
                                 i->ARM64in.LdSt16.amode );
         goto done;
      }
      case ARM64in_LdSt8: {
         p = do_load_or_store8( p, i->ARM64in.LdSt8.isLoad,
                                iregNo(i->ARM64in.LdSt8.rD),
                                i->ARM64in.LdSt8.amode );
         goto done;
      }
//ZZ       case ARMin_LdSt32:
//ZZ       case ARMin_LdSt8U: {
//ZZ          UInt        bL, bB;
//ZZ          HReg        rD;
//ZZ          ARMAMode1*  am;
//ZZ          ARMCondCode cc;
//ZZ          if (i->tag == ARMin_LdSt32) {
//ZZ             bB = 0;
//ZZ             bL = i->ARMin.LdSt32.isLoad ? 1 : 0;
//ZZ             am = i->ARMin.LdSt32.amode;
//ZZ             rD = i->ARMin.LdSt32.rD;
//ZZ             cc = i->ARMin.LdSt32.cc;
//ZZ          } else {
//ZZ             bB = 1;
//ZZ             bL = i->ARMin.LdSt8U.isLoad ? 1 : 0;
//ZZ             am = i->ARMin.LdSt8U.amode;
//ZZ             rD = i->ARMin.LdSt8U.rD;
//ZZ             cc = i->ARMin.LdSt8U.cc;
//ZZ          }
//ZZ          vassert(cc != ARMcc_NV);
//ZZ          if (am->tag == ARMam1_RI) {
//ZZ             Int  simm12;
//ZZ             UInt instr, bP;
//ZZ             if (am->ARMam1.RI.simm13 < 0) {
//ZZ                bP = 0;
//ZZ                simm12 = -am->ARMam1.RI.simm13;
//ZZ             } else {
//ZZ                bP = 1;
//ZZ                simm12 = am->ARMam1.RI.simm13;
//ZZ             }
//ZZ             vassert(simm12 >= 0 && simm12 <= 4095);
//ZZ             instr = XXXXX___(cc,X0101,BITS4(bP,bB,0,bL),
//ZZ                              iregNo(am->ARMam1.RI.reg),
//ZZ                              iregNo(rD));
//ZZ             instr |= simm12;
//ZZ             *p++ = instr;
//ZZ             goto done;
//ZZ          } else {
//ZZ             // RR case
//ZZ             goto bad;
//ZZ          }
//ZZ       }
//ZZ       case ARMin_LdSt16: {
//ZZ          HReg        rD = i->ARMin.LdSt16.rD;
//ZZ          UInt        bS = i->ARMin.LdSt16.signedLoad ? 1 : 0;
//ZZ          UInt        bL = i->ARMin.LdSt16.isLoad ? 1 : 0;
//ZZ          ARMAMode2*  am = i->ARMin.LdSt16.amode;
//ZZ          ARMCondCode cc = i->ARMin.LdSt16.cc;
//ZZ          vassert(cc != ARMcc_NV);
//ZZ          if (am->tag == ARMam2_RI) {
//ZZ             HReg rN = am->ARMam2.RI.reg;
//ZZ             Int  simm8;
//ZZ             UInt bP, imm8hi, imm8lo, instr;
//ZZ             if (am->ARMam2.RI.simm9 < 0) {
//ZZ                bP = 0;
//ZZ                simm8 = -am->ARMam2.RI.simm9;
//ZZ             } else {
//ZZ                bP = 1;
//ZZ                simm8 = am->ARMam2.RI.simm9;
//ZZ             }
//ZZ             vassert(simm8 >= 0 && simm8 <= 255);
//ZZ             imm8hi = (simm8 >> 4) & 0xF;
//ZZ             imm8lo = simm8 & 0xF;
//ZZ             vassert(!(bL == 0 && bS == 1)); // "! signed store"
//ZZ             /**/ if (bL == 0 && bS == 0) {
//ZZ                // strh
//ZZ                instr = XXXXXXXX(cc,X0001, BITS4(bP,1,0,0), iregNo(rN),
//ZZ                                 iregNo(rD), imm8hi, X1011, imm8lo);
//ZZ                *p++ = instr;
//ZZ                goto done;
//ZZ             }
//ZZ             else if (bL == 1 && bS == 0) {
//ZZ                // ldrh
//ZZ                instr = XXXXXXXX(cc,X0001, BITS4(bP,1,0,1), iregNo(rN),
//ZZ                                 iregNo(rD), imm8hi, X1011, imm8lo);
//ZZ                *p++ = instr;
//ZZ                goto done;
//ZZ             }
//ZZ             else if (bL == 1 && bS == 1) {
//ZZ                // ldrsh
//ZZ                instr = XXXXXXXX(cc,X0001, BITS4(bP,1,0,1), iregNo(rN),
//ZZ                                 iregNo(rD), imm8hi, X1111, imm8lo);
//ZZ                *p++ = instr;
//ZZ                goto done;
//ZZ             }
//ZZ             else vassert(0); // ill-constructed insn
//ZZ          } else {
//ZZ             // RR case
//ZZ             goto bad;
//ZZ          }
//ZZ       }
//ZZ       case ARMin_Ld8S: {
//ZZ          HReg        rD = i->ARMin.Ld8S.rD;
//ZZ          ARMAMode2*  am = i->ARMin.Ld8S.amode;
//ZZ          ARMCondCode cc = i->ARMin.Ld8S.cc;
//ZZ          vassert(cc != ARMcc_NV);
//ZZ          if (am->tag == ARMam2_RI) {
//ZZ             HReg rN = am->ARMam2.RI.reg;
//ZZ             Int  simm8;
//ZZ             UInt bP, imm8hi, imm8lo, instr;
//ZZ             if (am->ARMam2.RI.simm9 < 0) {
//ZZ                bP = 0;
//ZZ                simm8 = -am->ARMam2.RI.simm9;
//ZZ             } else {
//ZZ                bP = 1;
//ZZ                simm8 = am->ARMam2.RI.simm9;
//ZZ             }
//ZZ             vassert(simm8 >= 0 && simm8 <= 255);
//ZZ             imm8hi = (simm8 >> 4) & 0xF;
//ZZ             imm8lo = simm8 & 0xF;
//ZZ             // ldrsb
//ZZ             instr = XXXXXXXX(cc,X0001, BITS4(bP,1,0,1), iregNo(rN),
//ZZ                              iregNo(rD), imm8hi, X1101, imm8lo);
//ZZ             *p++ = instr;
//ZZ             goto done;
//ZZ          } else {
//ZZ             // RR case
//ZZ             goto bad;
//ZZ          }
//ZZ       }

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
         p = imm64_to_iregNo(p, /*x*/9, i->ARM64in.XDirect.dstGA);
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
         void* disp_cp_chain_me
                  = i->ARM64in.XDirect.toFastEP ? disp_cp_chain_me_to_fastEP 
                                                : disp_cp_chain_me_to_slowEP;
         p = imm64_to_iregNo_EXACTLY4(p, /*x*/9,
                                      Ptr_to_ULong(disp_cp_chain_me));
         *p++ = 0xD63F0120;
         /* --- END of PATCHABLE BYTES --- */

         /* Fix up the conditional jump, if there was one. */
         if (i->ARM64in.XDirect.cond != ARM64cc_AL) {
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
                                iregNo(i->ARM64in.XIndir.dstGA),
                                i->ARM64in.XIndir.amPC);

         /* imm64 x9, VG_(disp_cp_xindir) */
         /* br    x9 */
         p = imm64_to_iregNo(p, /*x*/9, Ptr_to_ULong(disp_cp_xindir));
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
                                iregNo(i->ARM64in.XAssisted.dstGA),
                                i->ARM64in.XAssisted.amPC);

         /* movw r21,  $magic_number */
         UInt trcval = 0;
         switch (i->ARM64in.XAssisted.jk) {
            case Ijk_ClientReq:   trcval = VEX_TRC_JMP_CLIENTREQ;   break;
            case Ijk_Sys_syscall: trcval = VEX_TRC_JMP_SYS_SYSCALL; break;
            //case Ijk_Sys_int128:  trcval = VEX_TRC_JMP_SYS_INT128;  break;
            //case Ijk_Yield:       trcval = VEX_TRC_JMP_YIELD;       break;
            //case Ijk_EmWarn:      trcval = VEX_TRC_JMP_EMWARN;      break;
            //case Ijk_MapFail:     trcval = VEX_TRC_JMP_MAPFAIL;     break;
            case Ijk_NoDecode:    trcval = VEX_TRC_JMP_NODECODE;    break;
            case Ijk_InvalICache: trcval = VEX_TRC_JMP_INVALICACHE; break;
            case Ijk_FlushDCache: trcval = VEX_TRC_JMP_FLUSHDCACHE; break;
            case Ijk_NoRedir:     trcval = VEX_TRC_JMP_NOREDIR;     break;
            //case Ijk_SigTRAP:     trcval = VEX_TRC_JMP_SIGTRAP;     break;
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
         p = imm64_to_iregNo(p, /*x*/21, (ULong)trcval);

         /* imm64 x9, VG_(disp_cp_xassisted) */
         /* br    x9 */
         p = imm64_to_iregNo(p, /*x*/9, Ptr_to_ULong(disp_cp_xassisted));
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
         UInt dd   = iregNo(i->ARM64in.CSel.dst);
         UInt nn   = iregNo(i->ARM64in.CSel.argL);
         UInt mm   = iregNo(i->ARM64in.CSel.argR);
         UInt cond = (UInt)i->ARM64in.CSel.cond;
         vassert(dd < 31 && nn < 31 && mm < 31 && cond < 16);
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
         p = imm64_to_iregNo( (UInt*)p,
                              /*x*/9, (ULong)i->ARM64in.Call.target );
         // blr x9
         *p++ = 0xD63F0120;

         // Patch the hole if necessary
         if (i->ARM64in.Call.cond != ARM64cc_AL) {
            ULong dist = (ULong)(p - ptmp);
            /* imm64_to_iregNo produces between 1 and 4 insns, and
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
         UInt dd = iregNo(i->ARM64in.FromSP.dst);
         vassert(dd < 31);
         *p++ = X_2_6_2_12_5_5(X10, X010001, X00, 0, X11111, dd);
         goto done;
      }

      case ARM64in_Mul: {
         /* 100 11011 110 mm 011111 nn dd   UMULH Xd, Xn,Xm
            100 11011 010 mm 011111 nn dd   SMULH Xd, Xn,Xm
            100 11011 000 mm 011111 nn dd   MUL   Xd, Xn,Xm
         */
         UInt dd = iregNo(i->ARM64in.Mul.dst);
         UInt nn = iregNo(i->ARM64in.Mul.argL);
         UInt mm = iregNo(i->ARM64in.Mul.argR);
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
      case ARM64in_MFence: {
         *p++ = 0xD5033F9F; /* DSB sy */
         *p++ = 0xD5033FBF; /* DMB sy */
         *p++ = 0xD5033FDF; /* ISB */
         goto done;
      }
      //case ARM64in_CLREX: {
      //   //ATC, but believed to be correct
      //   goto bad;
      //   *p++ = 0xD5033F5F; /* clrex */
      //   goto done;
      //}
      case ARM64in_VLdStS: {
         /* 10 111101 01 imm12 n t   LDR St, [Xn|SP, #imm12 * 4]
            10 111101 00 imm12 n t   STR St, [Xn|SP, #imm12 * 4]
         */
         UInt sD     = dregNo(i->ARM64in.VLdStS.sD);
         UInt rN     = iregNo(i->ARM64in.VLdStS.rN);
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
         UInt dD     = dregNo(i->ARM64in.VLdStD.dD);
         UInt rN     = iregNo(i->ARM64in.VLdStD.rN);
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
         UInt rQ = qregNo(i->ARM64in.VLdStQ.rQ);
         UInt rN = iregNo(i->ARM64in.VLdStQ.rN);
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
         UInt       rN = iregNo(i->ARM64in.VCvtI2F.rS);
         UInt       rD = dregNo(i->ARM64in.VCvtI2F.rD);
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

            Rd is Xd when sf==1, Wd when sf==0
            Fn is Dn when x==1, Sn when x==0
            20:19 carry the rounding mode, using the same encoding as FPCR
         */
         UInt       rD    = iregNo(i->ARM64in.VCvtF2I.rD);
         UInt       rN    = dregNo(i->ARM64in.VCvtF2I.rS);
         ARM64CvtOp how   = i->ARM64in.VCvtF2I.how;
         UChar      armRM = i->ARM64in.VCvtF2I.armRM;
         /* Just handle cases as they show up. */
         switch (how) {
            case ARM64cvt_F64_I32S: /* FCVTxS Wd, Dn */
               *p++ = X_3_5_8_6_5_5(X000, X11110, X01100000 | (armRM << 3),
                                    X000000, rN, rD);
               break;
            case ARM64cvt_F64_I32U: /* FCVTxU Wd, Dn */
               *p++ = X_3_5_8_6_5_5(X000, X11110, X01100001 | (armRM << 3),
                                    X000000, rN, rD);
               break;
            case ARM64cvt_F64_I64S: /* FCVTxS Xd, Dn */
               *p++ = X_3_5_8_6_5_5(X100, X11110, X01100000 | (armRM << 3),
                                    X000000, rN, rD);
               break;
            case ARM64cvt_F64_I64U: /* FCVTxU Xd, Dn */
               *p++ = X_3_5_8_6_5_5(X100, X11110, X01100001 | (armRM << 3),
                                    X000000, rN, rD);
               break;
            case ARM64cvt_F32_I32S: /* FCVTxS Wd, Sn */
               *p++ = X_3_5_8_6_5_5(X000, X11110, X00100000 | (armRM << 3),
                                    X000000, rN, rD);
               break;
            case ARM64cvt_F32_I32U: /* FCVTxU Wd, Sn */
               *p++ = X_3_5_8_6_5_5(X000, X11110, X00100001 | (armRM << 3),
                                    X000000, rN, rD);
               break;
            case ARM64cvt_F32_I64S: /* FCVTxS Xd, Sn */
               *p++ = X_3_5_8_6_5_5(X100, X11110, X00100000 | (armRM << 3),
                                    X000000, rN, rD);
               break;
            case ARM64cvt_F32_I64U: /* FCVTxU Xd, Sn */
               *p++ = X_3_5_8_6_5_5(X100, X11110, X00100001 | (armRM << 3),
                                    X000000, rN, rD);
               break;
            default:
               goto bad; //ATC
         }
         goto done;
      }
      case ARM64in_VCvtSD: {
         /* 31        23 21     16  14    9 4
            000,11110, 00 10001 0,1 10000 n d   FCVT Dd, Sn (S->D)
            ---------- 01 ----- 0,0 ---------   FCVT Sd, Dn (D->S)
            Rounding, when dst is smaller than src, is per the FPCR.
         */
         UInt dd = dregNo(i->ARM64in.VCvtSD.dst);
         UInt nn = dregNo(i->ARM64in.VCvtSD.src);
         if (i->ARM64in.VCvtSD.sToD) {
            *p++ = X_3_5_8_6_5_5(X000, X11110, X00100010, X110000, nn, dd);
         } else {
            *p++ = X_3_5_8_6_5_5(X000, X11110, X01100010, X010000, nn, dd);
         }
         goto done;
      }
      case ARM64in_VUnaryD: {
         /* 31        23 21     16 14    9 4
            000,11110 01 1,0000 0,0 10000 n d  FMOV Dd, Dn (not handled)
            ------------------- 0,1 ---------  FABS ------
            ------------------- 1,0 ---------  FNEG ------
            ------------------- 1,1 ---------  FQSRT -----
         */
         UInt dD  = dregNo(i->ARM64in.VUnaryD.dst);
         UInt dN  = dregNo(i->ARM64in.VUnaryD.src);
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
         goto bad;
      }
      case ARM64in_VUnaryS: {
         /* 31        23 21     16 14    9 4
            000,11110 00 1,0000 0,0 10000 n d  FMOV Sd, Sn (not handled)
            ------------------- 0,1 ---------  FABS ------
            ------------------- 1,0 ---------  FNEG ------
            ------------------- 1,1 ---------  FQSRT -----
         */
         UInt sD  = dregNo(i->ARM64in.VUnaryS.dst);
         UInt sN  = dregNo(i->ARM64in.VUnaryS.src);
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
         goto bad;
      }
      case ARM64in_VBinD: {
         /* 31        23  20 15   11 9 4
            ---------------- 0000 ------   FMUL  --------
            000 11110 011 m  0001 10 n d   FDIV  Dd,Dn,Dm
            ---------------- 0010 ------   FADD  --------
            ---------------- 0011 ------   FSUB  --------
         */
         UInt dD = dregNo(i->ARM64in.VBinD.dst);
         UInt dN = dregNo(i->ARM64in.VBinD.argL);
         UInt dM = dregNo(i->ARM64in.VBinD.argR);
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
         UInt sD = dregNo(i->ARM64in.VBinS.dst);
         UInt sN = dregNo(i->ARM64in.VBinS.argL);
         UInt sM = dregNo(i->ARM64in.VBinS.argR);
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
      case ARM64in_VCmpD: {
         /* 000 11110 01 1 m 00 1000 n 00 000  FCMP Dn, Dm */
         UInt dN = dregNo(i->ARM64in.VCmpD.argL);
         UInt dM = dregNo(i->ARM64in.VCmpD.argR);
         *p++ = X_3_8_5_6_5_5(X000, X11110011, dM, X001000, dN, X00000);
         goto done;
      }
      case ARM64in_VCmpS: {
         /* 000 11110 00 1 m 00 1000 n 00 000  FCMP Sn, Sm */
         UInt sN = dregNo(i->ARM64in.VCmpS.argL);
         UInt sM = dregNo(i->ARM64in.VCmpS.argR);
         *p++ = X_3_8_5_6_5_5(X000, X11110001, sM, X001000, sN, X00000);
         goto done;
      }
      case ARM64in_FPCR: {
         Bool toFPCR = i->ARM64in.FPCR.toFPCR;
         UInt iReg   = iregNo(i->ARM64in.FPCR.iReg);
         if (toFPCR) {
            /* 0xD51B44 000 Rt  MSR fpcr, rT */
            *p++ = 0xD51B4400 | (iReg & 0x1F);
            goto done;
         }
         goto bad; // FPCR -> iReg case currently ATC
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
         */
         UInt vD = qregNo(i->ARM64in.VBinV.dst);
         UInt vN = qregNo(i->ARM64in.VBinV.argL);
         UInt vM = qregNo(i->ARM64in.VBinV.argR);
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
            case ARM64vecb_FSUB64x2:
               *p++ = X_3_8_5_6_5_5(X010, X01110111, vM, X110101, vN, vD);
               break;
            case ARM64vecb_FSUB32x4:
               *p++ = X_3_8_5_6_5_5(X010, X01110101, vM, X110101, vN, vD);
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

            case ARM64vecb_FCMGE64x2:
               *p++ = X_3_8_5_6_5_5(X011, X01110011, vM, X111001, vN, vD);
               break;
            case ARM64vecb_FCMGE32x4:
               *p++ = X_3_8_5_6_5_5(X011, X01110001, vM, X111001, vN, vD);
               break;

            case ARM64vecb_FCMGT64x2:
               *p++ = X_3_8_5_6_5_5(X011, X01110111, vM, X111001, vN, vD);
               break;
            case ARM64vecb_FCMGT32x4:
               *p++ = X_3_8_5_6_5_5(X011, X01110101, vM, X111001, vN, vD);
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

            default:
               goto bad;
         }
         goto done;
      }
      case ARM64in_VUnaryV: {
         /* 31        23   20    15     9 4
            010 01110 11 1 00000 111110 n d  FABS Vd.2d,  Vn.2d
            010 01110 10 1 00000 111110 n d  FABS Vd.4s,  Vn.4s
            011 01110 11 1 00000 111110 n d  FNEG Vd.2d,  Vn.2d
            011 01110 10 1 00000 111110 n d  FNEG Vd.4s,  Vn.4s
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
         */
         UInt vD = qregNo(i->ARM64in.VUnaryV.dst);
         UInt vN = qregNo(i->ARM64in.VUnaryV.arg);
         switch (i->ARM64in.VUnaryV.op) {
            case ARM64vecu_FABS64x2:
               *p++ = X_3_8_5_6_5_5(X010, X01110111, X00000, X111110, vN, vD);
               break;
            case ARM64vecu_FABS32x4:
               *p++ = X_3_8_5_6_5_5(X010, X01110101, X00000, X111110, vN, vD);
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
         */
         UInt vD = qregNo(i->ARM64in.VNarrowV.dst);
         UInt vN = qregNo(i->ARM64in.VNarrowV.src);
         UInt dszBlg2 = i->ARM64in.VNarrowV.dszBlg2;
         vassert(dszBlg2 >= 0 && dszBlg2 <= 2);
         *p++ = X_3_8_5_6_5_5(X000, X01110001 | (dszBlg2 << 1),
                              X00001, X001010, vN, vD);
         goto done;
      }
      case ARM64in_VShiftImmV: {
         /*
            011 011110 immh immb 000001 n d  USHR Vd.T, Vn.T, #sh
            010 011110 immh immb 000001 n d  SSHR Vd.T, Vn.T, #sh
            where immh:immb
               = case T of 
                    2d  | sh in 1..63 -> let xxxxxx = 64-sh in 1xxx:xxx
                    4s  | sh in 1..31 -> let  xxxxx = 32-sh in 01xx:xxx
                    8h  | sh in 1..15 -> let   xxxx = 16-sh in 001x:xxx
                    16b | sh in 1..7  -> let    xxx =  8-sh in 0001:xxx

            010 011110 immh immb 010101 n d  SHL Vd.T, Vn.T, #sh
            where immh:immb
               = case T of 
                    2d  | sh in 1..63 -> let xxxxxx = sh in 1xxx:xxx
                    4s  | sh in 1..31 -> let  xxxxx = sh in 01xx:xxx
                    8h  | sh in 1..15 -> let   xxxx = sh in 001x:xxx
                    16b | sh in 1..7  -> let    xxx = sh in 0001:xxx
         */
         UInt vD = qregNo(i->ARM64in.VShiftImmV.dst);
         UInt vN = qregNo(i->ARM64in.VShiftImmV.src);
         UInt sh = i->ARM64in.VShiftImmV.amt;
         ARM64VecShiftOp op = i->ARM64in.VShiftImmV.op;
         Bool syned = False;
         switch (op) {
            /* 64x2 cases */
            case ARM64vecsh_SSHR64x2: syned = True;
            case ARM64vecsh_USHR64x2: /* fallthrough */
               if (sh >= 1 && sh <= 63) {
                  UInt xxxxxx = 64-sh;
                  *p++ = X_3_6_7_6_5_5(syned ? X010 : X011, X011110,
                                       X1000000 | xxxxxx, X000001, vN, vD);
                  goto done;
               }
               break;
            case ARM64vecsh_SHL64x2:
               if (sh >= 1 && sh <= 63) {
                  UInt xxxxxx = sh;
                  *p++ = X_3_6_7_6_5_5(X010, X011110,
                                       X1000000 | xxxxxx, X010101, vN, vD);
                  goto done;
               }
               break;
            /* 32x4 cases */
            case ARM64vecsh_SSHR32x4: syned = True;
            case ARM64vecsh_USHR32x4: /* fallthrough */
               if (sh >= 1 && sh <= 31) {
                  UInt xxxxx = 32-sh;
                  *p++ = X_3_6_7_6_5_5(syned ? X010 : X011, X011110,
                                       X0100000 | xxxxx, X000001, vN, vD);
                  goto done;
               }
               break;
            case ARM64vecsh_SHL32x4:
               if (sh >= 1 && sh <= 31) {
                  UInt xxxxx = sh;
                  *p++ = X_3_6_7_6_5_5(X010, X011110,
                                       X0100000 | xxxxx, X010101, vN, vD);
                  goto done;
               }
               break;
            /* 16x8 cases */
            case ARM64vecsh_SSHR16x8: syned = True;
            case ARM64vecsh_USHR16x8: /* fallthrough */
               if (sh >= 1 && sh <= 15) {
                  UInt xxxx = 16-sh;
                  *p++ = X_3_6_7_6_5_5(syned ? X010 : X011, X011110,
                                       X0010000 | xxxx, X000001, vN, vD);
                  goto done;
               }
               break;
            case ARM64vecsh_SHL16x8:
               if (sh >= 1 && sh <= 15) {
                  UInt xxxx = sh;
                  *p++ = X_3_6_7_6_5_5(X010, X011110,
                                       X0010000 | xxxx, X010101, vN, vD);
                  goto done;
               }
               break;
            /* 8x16 cases */
            case ARM64vecsh_SSHR8x16: syned = True;
            case ARM64vecsh_USHR8x16: /* fallthrough */
               if (sh >= 1 && sh <= 7) {
                  UInt xxx = 8-sh;
                  *p++ = X_3_6_7_6_5_5(syned ? X010 : X011, X011110,
                                       X0001000 | xxx, X000001, vN, vD);
                  goto done;
               }
               break;
            case ARM64vecsh_SHL8x16:
               if (sh >= 1 && sh <= 7) {
                  UInt xxx = sh;
                  *p++ = X_3_6_7_6_5_5(X010, X011110,
                                       X0001000 | xxx, X010101, vN, vD);
                  goto done;
               }
               break;
            default:
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
         UInt vD   = qregNo(i->ARM64in.VExtV.dst);
         UInt vN   = qregNo(i->ARM64in.VExtV.srcLo);
         UInt vM   = qregNo(i->ARM64in.VExtV.srcHi);
         UInt imm4 = i->ARM64in.VExtV.amtB;
         vassert(imm4 >= 1 && imm4 <= 15);
         *p++ = X_3_8_5_6_5_5(X011, X01110000, vM,
                              X000000 | (imm4 << 1), vN, vD);
         goto done;
      }
//ZZ       case ARMin_VAluS: {
//ZZ          UInt dN = fregNo(i->ARMin.VAluS.argL);
//ZZ          UInt dD = fregNo(i->ARMin.VAluS.dst);
//ZZ          UInt dM = fregNo(i->ARMin.VAluS.argR);
//ZZ          UInt bN = dN & 1;
//ZZ          UInt bD = dD & 1;
//ZZ          UInt bM = dM & 1;
//ZZ          UInt pqrs = X1111; /* undefined */
//ZZ          switch (i->ARMin.VAluS.op) {
//ZZ             case ARMvfp_ADD: pqrs = X0110; break;
//ZZ             case ARMvfp_SUB: pqrs = X0111; break;
//ZZ             case ARMvfp_MUL: pqrs = X0100; break;
//ZZ             case ARMvfp_DIV: pqrs = X1000; break;
//ZZ             default: goto bad;
//ZZ          }
//ZZ          vassert(pqrs != X1111);
//ZZ          UInt bP  = (pqrs >> 3) & 1;
//ZZ          UInt bQ  = (pqrs >> 2) & 1;
//ZZ          UInt bR  = (pqrs >> 1) & 1;
//ZZ          UInt bS  = (pqrs >> 0) & 1;
//ZZ          UInt insn = XXXXXXXX(0xE, X1110, BITS4(bP,bD,bQ,bR),
//ZZ                               (dN >> 1), (dD >> 1),
//ZZ                               X1010, BITS4(bN,bS,bM,0), (dM >> 1));
//ZZ          *p++ = insn;
//ZZ          goto done;
//ZZ       }
//ZZ       case ARMin_VUnaryS: {
//ZZ          UInt fD   = fregNo(i->ARMin.VUnaryS.dst);
//ZZ          UInt fM   = fregNo(i->ARMin.VUnaryS.src);
//ZZ          UInt insn = 0;
//ZZ          switch (i->ARMin.VUnaryS.op) {
//ZZ             case ARMvfpu_COPY:
//ZZ                insn = XXXXXXXX(0xE, X1110, BITS4(1,(fD & 1),1,1), X0000,
//ZZ                                (fD >> 1), X1010, BITS4(0,1,(fM & 1),0),
//ZZ                                (fM >> 1));
//ZZ                break;
//ZZ             case ARMvfpu_ABS:
//ZZ                insn = XXXXXXXX(0xE, X1110, BITS4(1,(fD & 1),1,1), X0000,
//ZZ                                (fD >> 1), X1010, BITS4(1,1,(fM & 1),0),
//ZZ                                (fM >> 1));
//ZZ                break;
//ZZ             case ARMvfpu_NEG:
//ZZ                insn = XXXXXXXX(0xE, X1110, BITS4(1,(fD & 1),1,1), X0001,
//ZZ                                (fD >> 1), X1010, BITS4(0,1,(fM & 1),0),
//ZZ                                (fM >> 1));
//ZZ                break;
//ZZ             case ARMvfpu_SQRT:
//ZZ                insn = XXXXXXXX(0xE, X1110, BITS4(1,(fD & 1),1,1), X0001,
//ZZ                                (fD >> 1), X1010, BITS4(1,1,(fM & 1),0),
//ZZ                                (fM >> 1));
//ZZ                break;
//ZZ             default:
//ZZ                goto bad;
//ZZ          }
//ZZ          *p++ = insn;
//ZZ          goto done;
//ZZ       }
//ZZ       case ARMin_VCMovD: {
//ZZ          UInt cc = (UInt)i->ARMin.VCMovD.cond;
//ZZ          UInt dD = dregNo(i->ARMin.VCMovD.dst);
//ZZ          UInt dM = dregNo(i->ARMin.VCMovD.src);
//ZZ          vassert(cc < 16 && cc != ARMcc_AL);
//ZZ          UInt insn = XXXXXXXX(cc, X1110,X1011,X0000,dD,X1011,X0100,dM);
//ZZ          *p++ = insn;
//ZZ          goto done;
//ZZ       }
//ZZ       case ARMin_VCMovS: {
//ZZ          UInt cc = (UInt)i->ARMin.VCMovS.cond;
//ZZ          UInt fD = fregNo(i->ARMin.VCMovS.dst);
//ZZ          UInt fM = fregNo(i->ARMin.VCMovS.src);
//ZZ          vassert(cc < 16 && cc != ARMcc_AL);
//ZZ          UInt insn = XXXXXXXX(cc, X1110, BITS4(1,(fD & 1),1,1),
//ZZ                               X0000,(fD >> 1),X1010,
//ZZ                               BITS4(0,1,(fM & 1),0), (fM >> 1));
//ZZ          *p++ = insn;
//ZZ          goto done;
//ZZ       }
//ZZ       case ARMin_VXferD: {
//ZZ          UInt dD  = dregNo(i->ARMin.VXferD.dD);
//ZZ          UInt rHi = iregNo(i->ARMin.VXferD.rHi);
//ZZ          UInt rLo = iregNo(i->ARMin.VXferD.rLo);
//ZZ          /* vmov dD, rLo, rHi is
//ZZ             E C 4 rHi rLo B (0,0,dD[4],1) dD[3:0]
//ZZ             vmov rLo, rHi, dD is
//ZZ             E C 5 rHi rLo B (0,0,dD[4],1) dD[3:0]
//ZZ          */
//ZZ          UInt insn
//ZZ             = XXXXXXXX(0xE, 0xC, i->ARMin.VXferD.toD ? 4 : 5,
//ZZ                        rHi, rLo, 0xB,
//ZZ                        BITS4(0,0, ((dD >> 4) & 1), 1), (dD & 0xF));
//ZZ          *p++ = insn;
//ZZ          goto done;
//ZZ       }
//ZZ       case ARMin_VXferS: {
//ZZ          UInt fD  = fregNo(i->ARMin.VXferS.fD);
//ZZ          UInt rLo = iregNo(i->ARMin.VXferS.rLo);
//ZZ          /* vmov fD, rLo is
//ZZ             E E 0 fD[4:1] rLo A (fD[0],0,0,1) 0
//ZZ             vmov rLo, fD is
//ZZ             E E 1 fD[4:1] rLo A (fD[0],0,0,1) 0
//ZZ          */
//ZZ          UInt insn
//ZZ             = XXXXXXXX(0xE, 0xE, i->ARMin.VXferS.toS ? 0 : 1,
//ZZ                        (fD >> 1) & 0xF, rLo, 0xA, 
//ZZ                        BITS4((fD & 1),0,0,1), 0);
//ZZ          *p++ = insn;
//ZZ          goto done;
//ZZ       }
//ZZ       case ARMin_VCvtID: {
//ZZ          Bool iToD = i->ARMin.VCvtID.iToD;
//ZZ          Bool syned = i->ARMin.VCvtID.syned;
//ZZ          if (iToD && syned) {
//ZZ             // FSITOD: I32S-in-freg to F64-in-dreg
//ZZ             UInt regF = fregNo(i->ARMin.VCvtID.src);
//ZZ             UInt regD = dregNo(i->ARMin.VCvtID.dst);
//ZZ             UInt insn = XXXXXXXX(0xE, X1110, X1011, X1000, regD,
//ZZ                                  X1011, BITS4(1,1,(regF & 1),0),
//ZZ                                  (regF >> 1) & 0xF);
//ZZ             *p++ = insn;
//ZZ             goto done;
//ZZ          }
//ZZ          if (iToD && (!syned)) {
//ZZ             // FUITOD: I32U-in-freg to F64-in-dreg
//ZZ             UInt regF = fregNo(i->ARMin.VCvtID.src);
//ZZ             UInt regD = dregNo(i->ARMin.VCvtID.dst);
//ZZ             UInt insn = XXXXXXXX(0xE, X1110, X1011, X1000, regD,
//ZZ                                  X1011, BITS4(0,1,(regF & 1),0),
//ZZ                                  (regF >> 1) & 0xF);
//ZZ             *p++ = insn;
//ZZ             goto done;
//ZZ          }
//ZZ          if ((!iToD) && syned) {
//ZZ             // FTOSID: F64-in-dreg to I32S-in-freg
//ZZ             UInt regD = dregNo(i->ARMin.VCvtID.src);
//ZZ             UInt regF = fregNo(i->ARMin.VCvtID.dst);
//ZZ             UInt insn = XXXXXXXX(0xE, X1110, BITS4(1,(regF & 1),1,1),
//ZZ                                  X1101, (regF >> 1) & 0xF,
//ZZ                                  X1011, X0100, regD);
//ZZ             *p++ = insn;
//ZZ             goto done;
//ZZ          }
//ZZ          if ((!iToD) && (!syned)) {
//ZZ             // FTOUID: F64-in-dreg to I32U-in-freg
//ZZ             UInt regD = dregNo(i->ARMin.VCvtID.src);
//ZZ             UInt regF = fregNo(i->ARMin.VCvtID.dst);
//ZZ             UInt insn = XXXXXXXX(0xE, X1110, BITS4(1,(regF & 1),1,1),
//ZZ                                  X1100, (regF >> 1) & 0xF,
//ZZ                                  X1011, X0100, regD);
//ZZ             *p++ = insn;
//ZZ             goto done;
//ZZ          }
//ZZ          /*UNREACHED*/
//ZZ          vassert(0);
//ZZ       }
//ZZ       case ARMin_NLdStD: {
//ZZ          UInt regD = dregNo(i->ARMin.NLdStD.dD);
//ZZ          UInt regN, regM;
//ZZ          UInt D = regD >> 4;
//ZZ          UInt bL = i->ARMin.NLdStD.isLoad ? 1 : 0;
//ZZ          UInt insn;
//ZZ          vassert(hregClass(i->ARMin.NLdStD.dD) == HRcFlt64);
//ZZ          regD &= 0xF;
//ZZ          if (i->ARMin.NLdStD.amode->tag == ARMamN_RR) {
//ZZ             regN = iregNo(i->ARMin.NLdStD.amode->ARMamN.RR.rN);
//ZZ             regM = iregNo(i->ARMin.NLdStD.amode->ARMamN.RR.rM);
//ZZ          } else {
//ZZ             regN = iregNo(i->ARMin.NLdStD.amode->ARMamN.R.rN);
//ZZ             regM = 15;
//ZZ          }
//ZZ          insn = XXXXXXXX(0xF, X0100, BITS4(0, D, bL, 0),
//ZZ                               regN, regD, X0111, X1000, regM);
//ZZ          *p++ = insn;
//ZZ          goto done;
//ZZ       }
//ZZ       case ARMin_NUnaryS: {
//ZZ          UInt Q = i->ARMin.NUnaryS.Q ? 1 : 0;
//ZZ          UInt regD, D;
//ZZ          UInt regM, M;
//ZZ          UInt size = i->ARMin.NUnaryS.size;
//ZZ          UInt insn;
//ZZ          UInt opc, opc1, opc2;
//ZZ          switch (i->ARMin.NUnaryS.op) {
//ZZ 	    case ARMneon_VDUP:
//ZZ                if (i->ARMin.NUnaryS.size >= 16)
//ZZ                   goto bad;
//ZZ                if (i->ARMin.NUnaryS.dst->tag != ARMNRS_Reg)
//ZZ                   goto bad;
//ZZ                if (i->ARMin.NUnaryS.src->tag != ARMNRS_Scalar)
//ZZ                   goto bad;
//ZZ                regD = (hregClass(i->ARMin.NUnaryS.dst->reg) == HRcVec128)
//ZZ                         ? (qregNo(i->ARMin.NUnaryS.dst->reg) << 1)
//ZZ                         : dregNo(i->ARMin.NUnaryS.dst->reg);
//ZZ                regM = (hregClass(i->ARMin.NUnaryS.src->reg) == HRcVec128)
//ZZ                         ? (qregNo(i->ARMin.NUnaryS.src->reg) << 1)
//ZZ                         : dregNo(i->ARMin.NUnaryS.src->reg);
//ZZ                D = regD >> 4;
//ZZ                M = regM >> 4;
//ZZ                regD &= 0xf;
//ZZ                regM &= 0xf;
//ZZ                insn = XXXXXXXX(0xF, X0011, BITS4(1,D,1,1),
//ZZ                                (i->ARMin.NUnaryS.size & 0xf), regD,
//ZZ                                X1100, BITS4(0,Q,M,0), regM);
//ZZ                *p++ = insn;
//ZZ                goto done; 
//ZZ             case ARMneon_SETELEM:
//ZZ                regD = Q ? (qregNo(i->ARMin.NUnaryS.dst->reg) << 1) :
//ZZ                                 dregNo(i->ARMin.NUnaryS.dst->reg);
//ZZ                regM = iregNo(i->ARMin.NUnaryS.src->reg);
//ZZ                M = regM >> 4;
//ZZ                D = regD >> 4;
//ZZ                regM &= 0xF;
//ZZ                regD &= 0xF;
//ZZ                if (i->ARMin.NUnaryS.dst->tag != ARMNRS_Scalar)
//ZZ                   goto bad;
//ZZ                switch (size) {
//ZZ                   case 0:
//ZZ                      if (i->ARMin.NUnaryS.dst->index > 7)
//ZZ                         goto bad;
//ZZ                      opc = X1000 | i->ARMin.NUnaryS.dst->index;
//ZZ                      break;
//ZZ                   case 1:
//ZZ                      if (i->ARMin.NUnaryS.dst->index > 3)
//ZZ                         goto bad;
//ZZ                      opc = X0001 | (i->ARMin.NUnaryS.dst->index << 1);
//ZZ                      break;
//ZZ                   case 2:
//ZZ                      if (i->ARMin.NUnaryS.dst->index > 1)
//ZZ                         goto bad;
//ZZ                      opc = X0000 | (i->ARMin.NUnaryS.dst->index << 2);
//ZZ                      break;
//ZZ                   default:
//ZZ                      goto bad;
//ZZ                }
//ZZ                opc1 = (opc >> 2) & 3;
//ZZ                opc2 = opc & 3;
//ZZ                insn = XXXXXXXX(0xE, X1110, BITS4(0,(opc1 >> 1),(opc1 & 1),0),
//ZZ                                regD, regM, X1011,
//ZZ                                BITS4(D,(opc2 >> 1),(opc2 & 1),1), X0000);
//ZZ                *p++ = insn;
//ZZ                goto done;
//ZZ             case ARMneon_GETELEMU:
//ZZ                regM = Q ? (qregNo(i->ARMin.NUnaryS.src->reg) << 1) :
//ZZ                                 dregNo(i->ARMin.NUnaryS.src->reg);
//ZZ                regD = iregNo(i->ARMin.NUnaryS.dst->reg);
//ZZ                M = regM >> 4;
//ZZ                D = regD >> 4;
//ZZ                regM &= 0xF;
//ZZ                regD &= 0xF;
//ZZ                if (i->ARMin.NUnaryS.src->tag != ARMNRS_Scalar)
//ZZ                   goto bad;
//ZZ                switch (size) {
//ZZ                   case 0:
//ZZ                      if (Q && i->ARMin.NUnaryS.src->index > 7) {
//ZZ                         regM++;
//ZZ                         i->ARMin.NUnaryS.src->index -= 8;
//ZZ                      }
//ZZ                      if (i->ARMin.NUnaryS.src->index > 7)
//ZZ                         goto bad;
//ZZ                      opc = X1000 | i->ARMin.NUnaryS.src->index;
//ZZ                      break;
//ZZ                   case 1:
//ZZ                      if (Q && i->ARMin.NUnaryS.src->index > 3) {
//ZZ                         regM++;
//ZZ                         i->ARMin.NUnaryS.src->index -= 4;
//ZZ                      }
//ZZ                      if (i->ARMin.NUnaryS.src->index > 3)
//ZZ                         goto bad;
//ZZ                      opc = X0001 | (i->ARMin.NUnaryS.src->index << 1);
//ZZ                      break;
//ZZ                   case 2:
//ZZ                      goto bad;
//ZZ                   default:
//ZZ                      goto bad;
//ZZ                }
//ZZ                opc1 = (opc >> 2) & 3;
//ZZ                opc2 = opc & 3;
//ZZ                insn = XXXXXXXX(0xE, X1110, BITS4(1,(opc1 >> 1),(opc1 & 1),1),
//ZZ                                regM, regD, X1011,
//ZZ                                BITS4(M,(opc2 >> 1),(opc2 & 1),1), X0000);
//ZZ                *p++ = insn;
//ZZ                goto done;
//ZZ             case ARMneon_GETELEMS:
//ZZ                regM = Q ? (qregNo(i->ARMin.NUnaryS.src->reg) << 1) :
//ZZ                                 dregNo(i->ARMin.NUnaryS.src->reg);
//ZZ                regD = iregNo(i->ARMin.NUnaryS.dst->reg);
//ZZ                M = regM >> 4;
//ZZ                D = regD >> 4;
//ZZ                regM &= 0xF;
//ZZ                regD &= 0xF;
//ZZ                if (i->ARMin.NUnaryS.src->tag != ARMNRS_Scalar)
//ZZ                   goto bad;
//ZZ                switch (size) {
//ZZ                   case 0:
//ZZ                      if (Q && i->ARMin.NUnaryS.src->index > 7) {
//ZZ                         regM++;
//ZZ                         i->ARMin.NUnaryS.src->index -= 8;
//ZZ                      }
//ZZ                      if (i->ARMin.NUnaryS.src->index > 7)
//ZZ                         goto bad;
//ZZ                      opc = X1000 | i->ARMin.NUnaryS.src->index;
//ZZ                      break;
//ZZ                   case 1:
//ZZ                      if (Q && i->ARMin.NUnaryS.src->index > 3) {
//ZZ                         regM++;
//ZZ                         i->ARMin.NUnaryS.src->index -= 4;
//ZZ                      }
//ZZ                      if (i->ARMin.NUnaryS.src->index > 3)
//ZZ                         goto bad;
//ZZ                      opc = X0001 | (i->ARMin.NUnaryS.src->index << 1);
//ZZ                      break;
//ZZ                   case 2:
//ZZ                      if (Q && i->ARMin.NUnaryS.src->index > 1) {
//ZZ                         regM++;
//ZZ                         i->ARMin.NUnaryS.src->index -= 2;
//ZZ                      }
//ZZ                      if (i->ARMin.NUnaryS.src->index > 1)
//ZZ                         goto bad;
//ZZ                      opc = X0000 | (i->ARMin.NUnaryS.src->index << 2);
//ZZ                      break;
//ZZ                   default:
//ZZ                      goto bad;
//ZZ                }
//ZZ                opc1 = (opc >> 2) & 3;
//ZZ                opc2 = opc & 3;
//ZZ                insn = XXXXXXXX(0xE, X1110, BITS4(0,(opc1 >> 1),(opc1 & 1),1),
//ZZ                                regM, regD, X1011,
//ZZ                                BITS4(M,(opc2 >> 1),(opc2 & 1),1), X0000);
//ZZ                *p++ = insn;
//ZZ                goto done;
//ZZ             default:
//ZZ                goto bad;
//ZZ          }
//ZZ       }
//ZZ       case ARMin_NUnary: {
//ZZ          UInt Q = i->ARMin.NUnary.Q ? 1 : 0;
//ZZ          UInt regD = (hregClass(i->ARMin.NUnary.dst) == HRcVec128)
//ZZ                        ? (qregNo(i->ARMin.NUnary.dst) << 1)
//ZZ                        : dregNo(i->ARMin.NUnary.dst);
//ZZ          UInt regM, M;
//ZZ          UInt D = regD >> 4;
//ZZ          UInt sz1 = i->ARMin.NUnary.size >> 1;
//ZZ          UInt sz2 = i->ARMin.NUnary.size & 1;
//ZZ          UInt sz = i->ARMin.NUnary.size;
//ZZ          UInt insn;
//ZZ          UInt F = 0; /* TODO: floating point EQZ ??? */
//ZZ          if (i->ARMin.NUnary.op != ARMneon_DUP) {
//ZZ             regM = (hregClass(i->ARMin.NUnary.src) == HRcVec128) 
//ZZ                      ? (qregNo(i->ARMin.NUnary.src) << 1)
//ZZ                      : dregNo(i->ARMin.NUnary.src);
//ZZ             M = regM >> 4;
//ZZ          } else {
//ZZ             regM = iregNo(i->ARMin.NUnary.src);
//ZZ             M = regM >> 4;
//ZZ          }
//ZZ          regD &= 0xF;
//ZZ          regM &= 0xF;
//ZZ          switch (i->ARMin.NUnary.op) {
//ZZ             case ARMneon_COPY: /* VMOV reg, reg */
//ZZ                insn = XXXXXXXX(0xF, X0010, BITS4(0,D,1,0), regM, regD, X0001,
//ZZ                                BITS4(M,Q,M,1), regM);
//ZZ                break;
//ZZ             case ARMneon_COPYN: /* VMOVN regD, regQ */
//ZZ                insn = XXXXXXXX(0xF, X0011, BITS4(1,D,1,1), BITS4(sz1,sz2,1,0),
//ZZ                                regD, X0010, BITS4(0,0,M,0), regM);
//ZZ                break;
//ZZ             case ARMneon_COPYQNSS: /* VQMOVN regD, regQ */
//ZZ                insn = XXXXXXXX(0xF, X0011, BITS4(1,D,1,1), BITS4(sz1,sz2,1,0),
//ZZ                                regD, X0010, BITS4(1,0,M,0), regM);
//ZZ                break;
//ZZ             case ARMneon_COPYQNUS: /* VQMOVUN regD, regQ */
//ZZ                insn = XXXXXXXX(0xF, X0011, BITS4(1,D,1,1), BITS4(sz1,sz2,1,0),
//ZZ                                regD, X0010, BITS4(0,1,M,0), regM);
//ZZ                break;
//ZZ             case ARMneon_COPYQNUU: /* VQMOVN regD, regQ */
//ZZ                insn = XXXXXXXX(0xF, X0011, BITS4(1,D,1,1), BITS4(sz1,sz2,1,0),
//ZZ                                regD, X0010, BITS4(1,1,M,0), regM);
//ZZ                break;
//ZZ             case ARMneon_COPYLS: /* VMOVL regQ, regD */
//ZZ                if (sz >= 3)
//ZZ                   goto bad;
//ZZ                insn = XXXXXXXX(0xF, X0010,
//ZZ                                BITS4(1,D,(sz == 2) ? 1 : 0,(sz == 1) ? 1 : 0),
//ZZ                                BITS4((sz == 0) ? 1 : 0,0,0,0),
//ZZ                                regD, X1010, BITS4(0,0,M,1), regM);
//ZZ                break;
//ZZ             case ARMneon_COPYLU: /* VMOVL regQ, regD */
//ZZ                if (sz >= 3)
//ZZ                   goto bad;
//ZZ                insn = XXXXXXXX(0xF, X0011,
//ZZ                                BITS4(1,D,(sz == 2) ? 1 : 0,(sz == 1) ? 1 : 0),
//ZZ                                BITS4((sz == 0) ? 1 : 0,0,0,0),
//ZZ                                regD, X1010, BITS4(0,0,M,1), regM);
//ZZ                break;
//ZZ             case ARMneon_NOT: /* VMVN reg, reg*/
//ZZ                insn = XXXXXXXX(0xF, X0011, BITS4(1,D,1,1), X0000, regD, X0101,
//ZZ                                BITS4(1,Q,M,0), regM);
//ZZ                break;
//ZZ             case ARMneon_EQZ:
//ZZ                insn = XXXXXXXX(0xF, X0011, BITS4(1,D,1,1), BITS4(sz1,sz2,0,1),
//ZZ                                regD, BITS4(0,F,0,1), BITS4(0,Q,M,0), regM);
//ZZ                break;
//ZZ             case ARMneon_CNT:
//ZZ                insn = XXXXXXXX(0xF, X0011, BITS4(1,D,1,1), X0000, regD, X0101,
//ZZ                                BITS4(0,Q,M,0), regM);
//ZZ                break;
//ZZ             case ARMneon_CLZ:
//ZZ                insn = XXXXXXXX(0xF, X0011, BITS4(1,D,1,1), BITS4(sz1,sz2,0,0),
//ZZ                                regD, X0100, BITS4(1,Q,M,0), regM);
//ZZ                break;
//ZZ             case ARMneon_CLS:
//ZZ                insn = XXXXXXXX(0xF, X0011, BITS4(1,D,1,1), BITS4(sz1,sz2,0,0),
//ZZ                                regD, X0100, BITS4(0,Q,M,0), regM);
//ZZ                break;
//ZZ             case ARMneon_ABS:
//ZZ                insn = XXXXXXXX(0xF, X0011, BITS4(1,D,1,1), BITS4(sz1,sz2,0,1),
//ZZ                                regD, X0011, BITS4(0,Q,M,0), regM);
//ZZ                break;
//ZZ             case ARMneon_DUP:
//ZZ                sz1 = i->ARMin.NUnary.size == 0 ? 1 : 0;
//ZZ                sz2 = i->ARMin.NUnary.size == 1 ? 1 : 0;
//ZZ                vassert(sz1 + sz2 < 2);
//ZZ                insn = XXXXXXXX(0xE, X1110, BITS4(1, sz1, Q, 0), regD, regM,
//ZZ                                X1011, BITS4(D,0,sz2,1), X0000);
//ZZ                break;
//ZZ             case ARMneon_REV16:
//ZZ                insn = XXXXXXXX(0xF, X0011, BITS4(1,D,1,1), BITS4(sz1,sz2,0,0),
//ZZ                                regD, BITS4(0,0,0,1), BITS4(0,Q,M,0), regM);
//ZZ                break;
//ZZ             case ARMneon_REV32:
//ZZ                insn = XXXXXXXX(0xF, X0011, BITS4(1,D,1,1), BITS4(sz1,sz2,0,0),
//ZZ                                regD, BITS4(0,0,0,0), BITS4(1,Q,M,0), regM);
//ZZ                break;
//ZZ             case ARMneon_REV64:
//ZZ                insn = XXXXXXXX(0xF, X0011, BITS4(1,D,1,1), BITS4(sz1,sz2,0,0),
//ZZ                                regD, BITS4(0,0,0,0), BITS4(0,Q,M,0), regM);
//ZZ                break;
//ZZ             case ARMneon_PADDLU:
//ZZ                insn = XXXXXXXX(0xF, X0011, BITS4(1,D,1,1), BITS4(sz1,sz2,0,0),
//ZZ                                regD, X0010, BITS4(1,Q,M,0), regM);
//ZZ                break;
//ZZ             case ARMneon_PADDLS:
//ZZ                insn = XXXXXXXX(0xF, X0011, BITS4(1,D,1,1), BITS4(sz1,sz2,0,0),
//ZZ                                regD, X0010, BITS4(0,Q,M,0), regM);
//ZZ                break;
//ZZ             case ARMneon_VQSHLNUU:
//ZZ                insn = XXXXXXXX(0xF, X0011,
//ZZ                                (1 << 3) | (D << 2) | ((sz >> 4) & 3),
//ZZ                                sz & 0xf, regD, X0111,
//ZZ                                BITS4(sz >> 6,Q,M,1), regM);
//ZZ                break;
//ZZ             case ARMneon_VQSHLNSS:
//ZZ                insn = XXXXXXXX(0xF, X0010,
//ZZ                                (1 << 3) | (D << 2) | ((sz >> 4) & 3),
//ZZ                                sz & 0xf, regD, X0111,
//ZZ                                BITS4(sz >> 6,Q,M,1), regM);
//ZZ                break;
//ZZ             case ARMneon_VQSHLNUS:
//ZZ                insn = XXXXXXXX(0xF, X0011,
//ZZ                                (1 << 3) | (D << 2) | ((sz >> 4) & 3),
//ZZ                                sz & 0xf, regD, X0110,
//ZZ                                BITS4(sz >> 6,Q,M,1), regM);
//ZZ                break;
//ZZ             case ARMneon_VCVTFtoS:
//ZZ                insn = XXXXXXXX(0xF, X0011, BITS4(1,D,1,1), X1011, regD, X0111,
//ZZ                                BITS4(0,Q,M,0), regM);
//ZZ                break;
//ZZ             case ARMneon_VCVTFtoU:
//ZZ                insn = XXXXXXXX(0xF, X0011, BITS4(1,D,1,1), X1011, regD, X0111,
//ZZ                                BITS4(1,Q,M,0), regM);
//ZZ                break;
//ZZ             case ARMneon_VCVTStoF:
//ZZ                insn = XXXXXXXX(0xF, X0011, BITS4(1,D,1,1), X1011, regD, X0110,
//ZZ                                BITS4(0,Q,M,0), regM);
//ZZ                break;
//ZZ             case ARMneon_VCVTUtoF:
//ZZ                insn = XXXXXXXX(0xF, X0011, BITS4(1,D,1,1), X1011, regD, X0110,
//ZZ                                BITS4(1,Q,M,0), regM);
//ZZ                break;
//ZZ             case ARMneon_VCVTFtoFixedU:
//ZZ                sz1 = (sz >> 5) & 1;
//ZZ                sz2 = (sz >> 4) & 1;
//ZZ                sz &= 0xf;
//ZZ                insn = XXXXXXXX(0xF, X0011,
//ZZ                                BITS4(1,D,sz1,sz2), sz, regD, X1111,
//ZZ                                BITS4(0,Q,M,1), regM);
//ZZ                break;
//ZZ             case ARMneon_VCVTFtoFixedS:
//ZZ                sz1 = (sz >> 5) & 1;
//ZZ                sz2 = (sz >> 4) & 1;
//ZZ                sz &= 0xf;
//ZZ                insn = XXXXXXXX(0xF, X0010,
//ZZ                                BITS4(1,D,sz1,sz2), sz, regD, X1111,
//ZZ                                BITS4(0,Q,M,1), regM);
//ZZ                break;
//ZZ             case ARMneon_VCVTFixedUtoF:
//ZZ                sz1 = (sz >> 5) & 1;
//ZZ                sz2 = (sz >> 4) & 1;
//ZZ                sz &= 0xf;
//ZZ                insn = XXXXXXXX(0xF, X0011,
//ZZ                                BITS4(1,D,sz1,sz2), sz, regD, X1110,
//ZZ                                BITS4(0,Q,M,1), regM);
//ZZ                break;
//ZZ             case ARMneon_VCVTFixedStoF:
//ZZ                sz1 = (sz >> 5) & 1;
//ZZ                sz2 = (sz >> 4) & 1;
//ZZ                sz &= 0xf;
//ZZ                insn = XXXXXXXX(0xF, X0010,
//ZZ                                BITS4(1,D,sz1,sz2), sz, regD, X1110,
//ZZ                                BITS4(0,Q,M,1), regM);
//ZZ                break;
//ZZ             case ARMneon_VCVTF32toF16:
//ZZ                insn = XXXXXXXX(0xF, X0011, BITS4(1,D,1,1), X0110, regD, X0110,
//ZZ                                BITS4(0,0,M,0), regM);
//ZZ                break;
//ZZ             case ARMneon_VCVTF16toF32:
//ZZ                insn = XXXXXXXX(0xF, X0011, BITS4(1,D,1,1), X0110, regD, X0111,
//ZZ                                BITS4(0,0,M,0), regM);
//ZZ                break;
//ZZ             case ARMneon_VRECIP:
//ZZ                insn = XXXXXXXX(0xF, X0011, BITS4(1,D,1,1), X1011, regD, X0100,
//ZZ                                BITS4(0,Q,M,0), regM);
//ZZ                break;
//ZZ             case ARMneon_VRECIPF:
//ZZ                insn = XXXXXXXX(0xF, X0011, BITS4(1,D,1,1), X1011, regD, X0101,
//ZZ                                BITS4(0,Q,M,0), regM);
//ZZ                break;
//ZZ             case ARMneon_VABSFP:
//ZZ                insn = XXXXXXXX(0xF, X0011, BITS4(1,D,1,1), X1001, regD, X0111,
//ZZ                                BITS4(0,Q,M,0), regM);
//ZZ                break;
//ZZ             case ARMneon_VRSQRTEFP:
//ZZ                insn = XXXXXXXX(0xF, X0011, BITS4(1,D,1,1), X1011, regD, X0101,
//ZZ                                BITS4(1,Q,M,0), regM);
//ZZ                break;
//ZZ             case ARMneon_VRSQRTE:
//ZZ                insn = XXXXXXXX(0xF, X0011, BITS4(1,D,1,1), X1011, regD, X0100,
//ZZ                                BITS4(1,Q,M,0), regM);
//ZZ                break;
//ZZ             case ARMneon_VNEGF:
//ZZ                insn = XXXXXXXX(0xF, X0011, BITS4(1,D,1,1), X1001, regD, X0111,
//ZZ                                BITS4(1,Q,M,0), regM);
//ZZ                break;
//ZZ 
//ZZ             default:
//ZZ                goto bad;
//ZZ          }
//ZZ          *p++ = insn;
//ZZ          goto done;
//ZZ       }
//ZZ       case ARMin_NDual: {
//ZZ          UInt Q = i->ARMin.NDual.Q ? 1 : 0;
//ZZ          UInt regD = (hregClass(i->ARMin.NDual.arg1) == HRcVec128)
//ZZ                        ? (qregNo(i->ARMin.NDual.arg1) << 1)
//ZZ                        : dregNo(i->ARMin.NDual.arg1);
//ZZ          UInt regM = (hregClass(i->ARMin.NDual.arg2) == HRcVec128)
//ZZ                        ? (qregNo(i->ARMin.NDual.arg2) << 1)
//ZZ                        : dregNo(i->ARMin.NDual.arg2);
//ZZ          UInt D = regD >> 4;
//ZZ          UInt M = regM >> 4;
//ZZ          UInt sz1 = i->ARMin.NDual.size >> 1;
//ZZ          UInt sz2 = i->ARMin.NDual.size & 1;
//ZZ          UInt insn;
//ZZ          regD &= 0xF;
//ZZ          regM &= 0xF;
//ZZ          switch (i->ARMin.NDual.op) {
//ZZ             case ARMneon_TRN: /* VTRN reg, reg */
//ZZ                insn = XXXXXXXX(0xF, X0011, BITS4(1,D,1,1), BITS4(sz1,sz2,1,0),
//ZZ                                regD, X0000, BITS4(1,Q,M,0), regM);
//ZZ                break;
//ZZ             case ARMneon_ZIP: /* VZIP reg, reg */
//ZZ                insn = XXXXXXXX(0xF, X0011, BITS4(1,D,1,1), BITS4(sz1,sz2,1,0),
//ZZ                                regD, X0001, BITS4(1,Q,M,0), regM);
//ZZ                break;
//ZZ             case ARMneon_UZP: /* VUZP reg, reg */
//ZZ                insn = XXXXXXXX(0xF, X0011, BITS4(1,D,1,1), BITS4(sz1,sz2,1,0),
//ZZ                                regD, X0001, BITS4(0,Q,M,0), regM);
//ZZ                break;
//ZZ             default:
//ZZ                goto bad;
//ZZ          }
//ZZ          *p++ = insn;
//ZZ          goto done;
//ZZ       }
//ZZ       case ARMin_NBinary: {
//ZZ          UInt Q = i->ARMin.NBinary.Q ? 1 : 0;
//ZZ          UInt regD = (hregClass(i->ARMin.NBinary.dst) == HRcVec128)
//ZZ                        ? (qregNo(i->ARMin.NBinary.dst) << 1)
//ZZ                        : dregNo(i->ARMin.NBinary.dst);
//ZZ          UInt regN = (hregClass(i->ARMin.NBinary.argL) == HRcVec128)
//ZZ                        ? (qregNo(i->ARMin.NBinary.argL) << 1)
//ZZ                        : dregNo(i->ARMin.NBinary.argL);
//ZZ          UInt regM = (hregClass(i->ARMin.NBinary.argR) == HRcVec128)
//ZZ                        ? (qregNo(i->ARMin.NBinary.argR) << 1)
//ZZ                        : dregNo(i->ARMin.NBinary.argR);
//ZZ          UInt sz1 = i->ARMin.NBinary.size >> 1;
//ZZ          UInt sz2 = i->ARMin.NBinary.size & 1;
//ZZ          UInt D = regD >> 4;
//ZZ          UInt N = regN >> 4;
//ZZ          UInt M = regM >> 4;
//ZZ          UInt insn;
//ZZ          regD &= 0xF;
//ZZ          regM &= 0xF;
//ZZ          regN &= 0xF;
//ZZ          switch (i->ARMin.NBinary.op) {
//ZZ             case ARMneon_VAND: /* VAND reg, reg, reg */
//ZZ                insn = XXXXXXXX(0xF, X0010, BITS4(0,D,0,0), regN, regD, X0001,
//ZZ                                BITS4(N,Q,M,1), regM);
//ZZ                break;
//ZZ             case ARMneon_VORR: /* VORR reg, reg, reg*/
//ZZ                insn = XXXXXXXX(0xF, X0010, BITS4(0,D,1,0), regN, regD, X0001,
//ZZ                                BITS4(N,Q,M,1), regM);
//ZZ                break;
//ZZ             case ARMneon_VXOR: /* VEOR reg, reg, reg */
//ZZ                insn = XXXXXXXX(0xF, X0011, BITS4(0,D,0,0), regN, regD, X0001,
//ZZ                                BITS4(N,Q,M,1), regM);
//ZZ                break;
//ZZ             case ARMneon_VADD: /* VADD reg, reg, reg */
//ZZ                insn = XXXXXXXX(0xF, X0010, BITS4(0,D,sz1,sz2), regN, regD,
//ZZ                                X1000, BITS4(N,Q,M,0), regM);
//ZZ                break;
//ZZ             case ARMneon_VSUB: /* VSUB reg, reg, reg */
//ZZ                insn = XXXXXXXX(0xF, X0011, BITS4(0,D,sz1,sz2), regN, regD,
//ZZ                                X1000, BITS4(N,Q,M,0), regM);
//ZZ                break;
//ZZ             case ARMneon_VMINU: /* VMIN.Uxx reg, reg, reg */
//ZZ                insn = XXXXXXXX(0xF, X0011, BITS4(0,D,sz1,sz2), regN, regD,
//ZZ                                X0110, BITS4(N,Q,M,1), regM);
//ZZ                break;
//ZZ             case ARMneon_VMINS: /* VMIN.Sxx reg, reg, reg */
//ZZ                insn = XXXXXXXX(0xF, X0010, BITS4(0,D,sz1,sz2), regN, regD,
//ZZ                                X0110, BITS4(N,Q,M,1), regM);
//ZZ                break;
//ZZ             case ARMneon_VMAXU: /* VMAX.Uxx reg, reg, reg */
//ZZ                insn = XXXXXXXX(0xF, X0011, BITS4(0,D,sz1,sz2), regN, regD,
//ZZ                                X0110, BITS4(N,Q,M,0), regM);
//ZZ                break;
//ZZ             case ARMneon_VMAXS: /* VMAX.Sxx reg, reg, reg */
//ZZ                insn = XXXXXXXX(0xF, X0010, BITS4(0,D,sz1,sz2), regN, regD,
//ZZ                                X0110, BITS4(N,Q,M,0), regM);
//ZZ                break;
//ZZ             case ARMneon_VRHADDS: /* VRHADD.Sxx reg, reg, reg */
//ZZ                insn = XXXXXXXX(0xF, X0010, BITS4(0,D,sz1,sz2), regN, regD,
//ZZ                                X0001, BITS4(N,Q,M,0), regM);
//ZZ                break;
//ZZ             case ARMneon_VRHADDU: /* VRHADD.Uxx reg, reg, reg */
//ZZ                insn = XXXXXXXX(0xF, X0011, BITS4(0,D,sz1,sz2), regN, regD,
//ZZ                                X0001, BITS4(N,Q,M,0), regM);
//ZZ                break;
//ZZ             case ARMneon_VQADDU: /* VQADD unsigned reg, reg, reg */
//ZZ                insn = XXXXXXXX(0xF, X0011, BITS4(0,D,sz1,sz2), regN, regD,
//ZZ                                X0000, BITS4(N,Q,M,1), regM);
//ZZ                break;
//ZZ             case ARMneon_VQADDS: /* VQADD signed reg, reg, reg */
//ZZ                insn = XXXXXXXX(0xF, X0010, BITS4(0,D,sz1,sz2), regN, regD,
//ZZ                                X0000, BITS4(N,Q,M,1), regM);
//ZZ                break;
//ZZ             case ARMneon_VQSUBU: /* VQSUB unsigned reg, reg, reg */
//ZZ                insn = XXXXXXXX(0xF, X0011, BITS4(0,D,sz1,sz2), regN, regD,
//ZZ                                X0010, BITS4(N,Q,M,1), regM);
//ZZ                break;
//ZZ             case ARMneon_VQSUBS: /* VQSUB signed reg, reg, reg */
//ZZ                insn = XXXXXXXX(0xF, X0010, BITS4(0,D,sz1,sz2), regN, regD,
//ZZ                                X0010, BITS4(N,Q,M,1), regM);
//ZZ                break;
//ZZ             case ARMneon_VCGTU: /* VCGT unsigned reg, reg, reg */
//ZZ                insn = XXXXXXXX(0xF, X0011, BITS4(0,D,sz1,sz2), regN, regD,
//ZZ                                X0011, BITS4(N,Q,M,0), regM);
//ZZ                break;
//ZZ             case ARMneon_VCGTS: /* VCGT signed reg, reg, reg */
//ZZ                insn = XXXXXXXX(0xF, X0010, BITS4(0,D,sz1,sz2), regN, regD,
//ZZ                                X0011, BITS4(N,Q,M,0), regM);
//ZZ                break;
//ZZ             case ARMneon_VCGEU: /* VCGE unsigned reg, reg, reg */
//ZZ                insn = XXXXXXXX(0xF, X0011, BITS4(0,D,sz1,sz2), regN, regD,
//ZZ                                X0011, BITS4(N,Q,M,1), regM);
//ZZ                break;
//ZZ             case ARMneon_VCGES: /* VCGE signed reg, reg, reg */
//ZZ                insn = XXXXXXXX(0xF, X0010, BITS4(0,D,sz1,sz2), regN, regD,
//ZZ                                X0011, BITS4(N,Q,M,1), regM);
//ZZ                break;
//ZZ             case ARMneon_VCEQ: /* VCEQ reg, reg, reg */
//ZZ                insn = XXXXXXXX(0xF, X0011, BITS4(0,D,sz1,sz2), regN, regD,
//ZZ                                X1000, BITS4(N,Q,M,1), regM);
//ZZ                break;
//ZZ             case ARMneon_VEXT: /* VEXT.8 reg, reg, #imm4*/
//ZZ                if (i->ARMin.NBinary.size >= 16)
//ZZ                   goto bad;
//ZZ                insn = XXXXXXXX(0xF, X0010, BITS4(1,D,1,1), regN, regD,
//ZZ                                i->ARMin.NBinary.size & 0xf, BITS4(N,Q,M,0),
//ZZ                                regM);
//ZZ                break;
//ZZ             case ARMneon_VMUL:
//ZZ                insn = XXXXXXXX(0xF, X0010, BITS4(0,D,sz1,sz2), regN, regD,
//ZZ                                X1001, BITS4(N,Q,M,1), regM);
//ZZ                break;
//ZZ             case ARMneon_VMULLU:
//ZZ                insn = XXXXXXXX(0xF, X0011, BITS4(1,D,sz1,sz2), regN, regD,
//ZZ                                X1100, BITS4(N,0,M,0), regM);
//ZZ                break;
//ZZ             case ARMneon_VMULLS:
//ZZ                insn = XXXXXXXX(0xF, X0010, BITS4(1,D,sz1,sz2), regN, regD,
//ZZ                                X1100, BITS4(N,0,M,0), regM);
//ZZ                break;
//ZZ             case ARMneon_VMULP:
//ZZ                insn = XXXXXXXX(0xF, X0011, BITS4(0,D,sz1,sz2), regN, regD,
//ZZ                                X1001, BITS4(N,Q,M,1), regM);
//ZZ                break;
//ZZ             case ARMneon_VMULFP:
//ZZ                insn = XXXXXXXX(0xF, X0011, BITS4(0,D,0,0), regN, regD,
//ZZ                                X1101, BITS4(N,Q,M,1), regM);
//ZZ                break;
//ZZ             case ARMneon_VMULLP:
//ZZ                insn = XXXXXXXX(0xF, X0010, BITS4(1,D,sz1,sz2), regN, regD,
//ZZ                                X1110, BITS4(N,0,M,0), regM);
//ZZ                break;
//ZZ             case ARMneon_VQDMULH:
//ZZ                insn = XXXXXXXX(0xF, X0010, BITS4(0,D,sz1,sz2), regN, regD,
//ZZ                                X1011, BITS4(N,Q,M,0), regM);
//ZZ                break;
//ZZ             case ARMneon_VQRDMULH:
//ZZ                insn = XXXXXXXX(0xF, X0011, BITS4(0,D,sz1,sz2), regN, regD,
//ZZ                                X1011, BITS4(N,Q,M,0), regM);
//ZZ                break;
//ZZ             case ARMneon_VQDMULL:
//ZZ                insn = XXXXXXXX(0xF, X0010, BITS4(1,D,sz1,sz2), regN, regD,
//ZZ                                X1101, BITS4(N,0,M,0), regM);
//ZZ                break;
//ZZ             case ARMneon_VTBL:
//ZZ                insn = XXXXXXXX(0xF, X0011, BITS4(1,D,1,1), regN, regD,
//ZZ                                X1000, BITS4(N,0,M,0), regM);
//ZZ                break;
//ZZ             case ARMneon_VPADD:
//ZZ                insn = XXXXXXXX(0xF, X0010, BITS4(0,D,sz1,sz2), regN, regD,
//ZZ                                X1011, BITS4(N,Q,M,1), regM);
//ZZ                break;
//ZZ             case ARMneon_VPADDFP:
//ZZ                insn = XXXXXXXX(0xF, X0011, BITS4(0,D,0,0), regN, regD,
//ZZ                                X1101, BITS4(N,Q,M,0), regM);
//ZZ                break;
//ZZ             case ARMneon_VPMINU:
//ZZ                insn = XXXXXXXX(0xF, X0011, BITS4(0,D,sz1,sz2), regN, regD,
//ZZ                                X1010, BITS4(N,Q,M,1), regM);
//ZZ                break;
//ZZ             case ARMneon_VPMINS:
//ZZ                insn = XXXXXXXX(0xF, X0010, BITS4(0,D,sz1,sz2), regN, regD,
//ZZ                                X1010, BITS4(N,Q,M,1), regM);
//ZZ                break;
//ZZ             case ARMneon_VPMAXU:
//ZZ                insn = XXXXXXXX(0xF, X0011, BITS4(0,D,sz1,sz2), regN, regD,
//ZZ                                X1010, BITS4(N,Q,M,0), regM);
//ZZ                break;
//ZZ             case ARMneon_VPMAXS:
//ZZ                insn = XXXXXXXX(0xF, X0010, BITS4(0,D,sz1,sz2), regN, regD,
//ZZ                                X1010, BITS4(N,Q,M,0), regM);
//ZZ                break;
//ZZ             case ARMneon_VADDFP: /* VADD reg, reg, reg */
//ZZ                insn = XXXXXXXX(0xF, X0010, BITS4(0,D,0,0), regN, regD,
//ZZ                                X1101, BITS4(N,Q,M,0), regM);
//ZZ                break;
//ZZ             case ARMneon_VSUBFP: /* VADD reg, reg, reg */
//ZZ                insn = XXXXXXXX(0xF, X0010, BITS4(0,D,1,0), regN, regD,
//ZZ                                X1101, BITS4(N,Q,M,0), regM);
//ZZ                break;
//ZZ             case ARMneon_VABDFP: /* VABD reg, reg, reg */
//ZZ                insn = XXXXXXXX(0xF, X0011, BITS4(0,D,1,0), regN, regD,
//ZZ                                X1101, BITS4(N,Q,M,0), regM);
//ZZ                break;
//ZZ             case ARMneon_VMINF:
//ZZ                insn = XXXXXXXX(0xF, X0010, BITS4(0,D,1,0), regN, regD,
//ZZ                                X1111, BITS4(N,Q,M,0), regM);
//ZZ                break;
//ZZ             case ARMneon_VMAXF:
//ZZ                insn = XXXXXXXX(0xF, X0010, BITS4(0,D,0,0), regN, regD,
//ZZ                                X1111, BITS4(N,Q,M,0), regM);
//ZZ                break;
//ZZ             case ARMneon_VPMINF:
//ZZ                insn = XXXXXXXX(0xF, X0011, BITS4(0,D,1,0), regN, regD,
//ZZ                                X1111, BITS4(N,Q,M,0), regM);
//ZZ                break;
//ZZ             case ARMneon_VPMAXF:
//ZZ                insn = XXXXXXXX(0xF, X0011, BITS4(0,D,0,0), regN, regD,
//ZZ                                X1111, BITS4(N,Q,M,0), regM);
//ZZ                break;
//ZZ             case ARMneon_VRECPS:
//ZZ                insn = XXXXXXXX(0xF, X0010, BITS4(0,D,0,0), regN, regD, X1111,
//ZZ                                BITS4(N,Q,M,1), regM);
//ZZ                break;
//ZZ             case ARMneon_VCGTF:
//ZZ                insn = XXXXXXXX(0xF, X0011, BITS4(0,D,1,0), regN, regD, X1110,
//ZZ                                BITS4(N,Q,M,0), regM);
//ZZ                break;
//ZZ             case ARMneon_VCGEF:
//ZZ                insn = XXXXXXXX(0xF, X0011, BITS4(0,D,0,0), regN, regD, X1110,
//ZZ                                BITS4(N,Q,M,0), regM);
//ZZ                break;
//ZZ             case ARMneon_VCEQF:
//ZZ                insn = XXXXXXXX(0xF, X0010, BITS4(0,D,0,0), regN, regD, X1110,
//ZZ                                BITS4(N,Q,M,0), regM);
//ZZ                break;
//ZZ             case ARMneon_VRSQRTS:
//ZZ                insn = XXXXXXXX(0xF, X0010, BITS4(0,D,1,0), regN, regD, X1111,
//ZZ                                BITS4(N,Q,M,1), regM);
//ZZ                break;
//ZZ             default:
//ZZ                goto bad;
//ZZ          }
//ZZ          *p++ = insn;
//ZZ          goto done;
//ZZ       }
//ZZ       case ARMin_NShift: {
//ZZ          UInt Q = i->ARMin.NShift.Q ? 1 : 0;
//ZZ          UInt regD = (hregClass(i->ARMin.NShift.dst) == HRcVec128)
//ZZ                        ? (qregNo(i->ARMin.NShift.dst) << 1)
//ZZ                        : dregNo(i->ARMin.NShift.dst);
//ZZ          UInt regM = (hregClass(i->ARMin.NShift.argL) == HRcVec128)
//ZZ                        ? (qregNo(i->ARMin.NShift.argL) << 1)
//ZZ                        : dregNo(i->ARMin.NShift.argL);
//ZZ          UInt regN = (hregClass(i->ARMin.NShift.argR) == HRcVec128)
//ZZ                        ? (qregNo(i->ARMin.NShift.argR) << 1)
//ZZ                        : dregNo(i->ARMin.NShift.argR);
//ZZ          UInt sz1 = i->ARMin.NShift.size >> 1;
//ZZ          UInt sz2 = i->ARMin.NShift.size & 1;
//ZZ          UInt D = regD >> 4;
//ZZ          UInt N = regN >> 4;
//ZZ          UInt M = regM >> 4;
//ZZ          UInt insn;
//ZZ          regD &= 0xF;
//ZZ          regM &= 0xF;
//ZZ          regN &= 0xF;
//ZZ          switch (i->ARMin.NShift.op) {
//ZZ             case ARMneon_VSHL:
//ZZ                insn = XXXXXXXX(0xF, X0011, BITS4(0,D,sz1,sz2), regN, regD,
//ZZ                                X0100, BITS4(N,Q,M,0), regM);
//ZZ                break;
//ZZ             case ARMneon_VSAL:
//ZZ                insn = XXXXXXXX(0xF, X0010, BITS4(0,D,sz1,sz2), regN, regD,
//ZZ                                X0100, BITS4(N,Q,M,0), regM);
//ZZ                break;
//ZZ             case ARMneon_VQSHL:
//ZZ                insn = XXXXXXXX(0xF, X0011, BITS4(0,D,sz1,sz2), regN, regD,
//ZZ                                X0100, BITS4(N,Q,M,1), regM);
//ZZ                break;
//ZZ             case ARMneon_VQSAL:
//ZZ                insn = XXXXXXXX(0xF, X0010, BITS4(0,D,sz1,sz2), regN, regD,
//ZZ                                X0100, BITS4(N,Q,M,1), regM);
//ZZ                break;
//ZZ             default:
//ZZ                goto bad;
//ZZ          }
//ZZ          *p++ = insn;
//ZZ          goto done;
//ZZ       }
//ZZ       case ARMin_NShl64: {
//ZZ          HReg regDreg = i->ARMin.NShl64.dst;
//ZZ          HReg regMreg = i->ARMin.NShl64.src;
//ZZ          UInt amt     = i->ARMin.NShl64.amt;
//ZZ          vassert(amt >= 1 && amt <= 63);
//ZZ          vassert(hregClass(regDreg) == HRcFlt64);
//ZZ          vassert(hregClass(regMreg) == HRcFlt64);
//ZZ          UInt regD = dregNo(regDreg);
//ZZ          UInt regM = dregNo(regMreg);
//ZZ          UInt D    = (regD >> 4) & 1;
//ZZ          UInt Vd   = regD & 0xF;
//ZZ          UInt L    = 1;
//ZZ          UInt Q    = 0; /* always 64-bit */
//ZZ          UInt M    = (regM >> 4) & 1;
//ZZ          UInt Vm   = regM & 0xF;
//ZZ          UInt insn = XXXXXXXX(X1111,X0010, BITS4(1,D,(amt>>5)&1,(amt>>4)&1),
//ZZ                               amt & 0xF, Vd, X0101, BITS4(L,Q,M,1), Vm);
//ZZ          *p++ = insn;
//ZZ          goto done;
//ZZ       }
      case ARM64in_VImmQ: {
         UInt   rQ  = qregNo(i->ARM64in.VImmQ.rQ);
         UShort imm = i->ARM64in.VImmQ.imm;
         if (imm == 0x0000) {
            /* movi rQ.4s, #0x0 == 0x4F 0x00 0x04 000 rQ */
            vassert(rQ < 32);
            *p++ = 0x4F000400 | rQ;
            goto done;
         }
         if (imm == 0x0001) {
            /* movi rD, #0xFF == 0x2F 0x00 0xE4 001 rD */
            vassert(rQ < 32);
            *p++ = 0x2F00E420 | rQ;
            goto done;
         }
         if (imm == 0x0003) {
            /* movi rD, #0xFFFF == 0x2F 0x00 0xE4 011 rD */
            vassert(rQ < 32);
            *p++ = 0x2F00E460 | rQ;
            goto done;
         }
         if (imm == 0x000F) {
            /* movi rD, #0xFFFFFFFF == 0x2F 0x00 0xE5 111 rD */
            vassert(rQ < 32);
            *p++ = 0x2F00E5E0 | rQ;
            goto done;
         }
         if (imm == 0x00FF) {
            /* movi rD, #0xFFFFFFFFFFFFFFFF == 0x2F 0x07 0xE7 111 rD */
            vassert(rQ < 32);
            *p++ = 0x2F07E7E0 | rQ;
            goto done;
         }
         goto bad; /* no other handled cases right now */
      }

      case ARM64in_VDfromX: {
         /* INS Vd.D[0], rX
            0100 1110 0000 1000 0001 11 nn dd   INS Vd.D[0], Xn
            This isn't wonderful, in the sense that the upper half of
            the vector register stays unchanged and thus the insn is
            data dependent on its output register. */
         UInt dd = dregNo(i->ARM64in.VDfromX.rD);
         UInt xx = iregNo(i->ARM64in.VDfromX.rX);
         vassert(xx < 31);
         *p++ = 0x4E081C00 | X_2_6_2_12_5_5(0,0,0,0,xx,dd);
         goto done;
      }

      case ARM64in_VQfromXX: {
         /* What we really generate is a two insn sequence:
               INS Vd.D[0], Xlo; INS Vd.D[1], Xhi
            0100 1110 0000 1000 0001 11 nn dd   INS Vd.D[0], Xn
            0100 1110 0001 1000 0001 11 nn dd   INS Vd.D[1], Xn
         */
         UInt qq  = qregNo(i->ARM64in.VQfromXX.rQ);
         UInt xhi = iregNo(i->ARM64in.VQfromXX.rXhi);
         UInt xlo = iregNo(i->ARM64in.VQfromXX.rXlo);
         vassert(xhi < 31 && xlo < 31);
         *p++ = 0x4E081C00 | X_2_6_2_12_5_5(0,0,0,0,xlo,qq);
         *p++ = 0x4E181C00 | X_2_6_2_12_5_5(0,0,0,0,xhi,qq);
         goto done;
      }

      case ARM64in_VXfromQ: {
         /* 010 0111 0000 01000 001111 nn dd  UMOV Xd, Vn.D[0]
            010 0111 0000 11000 001111 nn dd  UMOV Xd, Vn.D[1]
         */
         UInt dd     = iregNo(i->ARM64in.VXfromQ.rX);
         UInt nn     = qregNo(i->ARM64in.VXfromQ.rQ);
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
         UInt dd    = iregNo(i->ARM64in.VXfromDorS.rX);
         UInt nn    = dregNo(i->ARM64in.VXfromDorS.rDorS);
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
           case 8: {
              UInt dd = dregNo(rD);
              UInt nn = dregNo(rN);
              *p++ = X_3_8_5_6_5_5(X000, X11110011, X00000, X010000, nn, dd);
              goto done;
           }
           default: 
              break;
        }
        goto bad;
      }
//ZZ       case ARMin_NeonImm: {
//ZZ          UInt Q = (hregClass(i->ARMin.NeonImm.dst) == HRcVec128) ? 1 : 0;
//ZZ          UInt regD = Q ? (qregNo(i->ARMin.NeonImm.dst) << 1) :
//ZZ                           dregNo(i->ARMin.NeonImm.dst);
//ZZ          UInt D = regD >> 4;
//ZZ          UInt imm = i->ARMin.NeonImm.imm->imm8;
//ZZ          UInt tp = i->ARMin.NeonImm.imm->type;
//ZZ          UInt j = imm >> 7;
//ZZ          UInt imm3 = (imm >> 4) & 0x7;
//ZZ          UInt imm4 = imm & 0xF;
//ZZ          UInt cmode, op;
//ZZ          UInt insn;
//ZZ          regD &= 0xF;
//ZZ          if (tp == 9)
//ZZ             op = 1;
//ZZ          else
//ZZ             op = 0;
//ZZ          switch (tp) {
//ZZ             case 0:
//ZZ             case 1:
//ZZ             case 2:
//ZZ             case 3:
//ZZ             case 4:
//ZZ             case 5:
//ZZ                cmode = tp << 1;
//ZZ                break;
//ZZ             case 9:
//ZZ             case 6:
//ZZ                cmode = 14;
//ZZ                break;
//ZZ             case 7:
//ZZ                cmode = 12;
//ZZ                break;
//ZZ             case 8:
//ZZ                cmode = 13;
//ZZ                break;
//ZZ             case 10:
//ZZ                cmode = 15;
//ZZ                break;
//ZZ             default:
//ZZ                vpanic("ARMin_NeonImm");
//ZZ 
//ZZ          }
//ZZ          insn = XXXXXXXX(0xF, BITS4(0,0,1,j), BITS4(1,D,0,0), imm3, regD,
//ZZ                          cmode, BITS4(0,Q,op,1), imm4);
//ZZ          *p++ = insn;
//ZZ          goto done;
//ZZ       }
//ZZ       case ARMin_NCMovQ: {
//ZZ          UInt cc = (UInt)i->ARMin.NCMovQ.cond;
//ZZ          UInt qM = qregNo(i->ARMin.NCMovQ.src) << 1;
//ZZ          UInt qD = qregNo(i->ARMin.NCMovQ.dst) << 1;
//ZZ          UInt vM = qM & 0xF;
//ZZ          UInt vD = qD & 0xF;
//ZZ          UInt M  = (qM >> 4) & 1;
//ZZ          UInt D  = (qD >> 4) & 1;
//ZZ          vassert(cc < 16 && cc != ARMcc_AL && cc != ARMcc_NV);
//ZZ          /* b!cc here+8: !cc A00 0000 */
//ZZ          UInt insn = XXXXXXXX(cc ^ 1, 0xA, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0);
//ZZ          *p++ = insn;
//ZZ          /* vmov qD, qM */
//ZZ          insn = XXXXXXXX(0xF, 0x2, BITS4(0,D,1,0),
//ZZ                          vM, vD, BITS4(0,0,0,1), BITS4(M,1,M,1), vM);
//ZZ          *p++ = insn;
//ZZ          goto done;
//ZZ       }
//ZZ       case ARMin_Add32: {
//ZZ          UInt regD = iregNo(i->ARMin.Add32.rD);
//ZZ          UInt regN = iregNo(i->ARMin.Add32.rN);
//ZZ          UInt imm32 = i->ARMin.Add32.imm32;
//ZZ          vassert(regD != regN);
//ZZ          /* MOV regD, imm32 */
//ZZ          p = imm32_to_iregNo((UInt *)p, regD, imm32);
//ZZ          /* ADD regD, regN, regD */
//ZZ          UInt insn = XXXXXXXX(0xE, 0, X1000, regN, regD, 0, 0, regD);
//ZZ          *p++ = insn;
//ZZ          goto done;
//ZZ       }

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

//ZZ       case ARMin_ProfInc: {
//ZZ          /* We generate:
//ZZ               (ctrP is unknown now, so use 0x65556555 in the
//ZZ               expectation that a later call to LibVEX_patchProfCtr
//ZZ               will be used to fill in the immediate fields once the
//ZZ               right value is known.)
//ZZ             movw r12, lo16(0x65556555)
//ZZ             movt r12, lo16(0x65556555)
//ZZ             ldr  r11, [r12]
//ZZ             adds r11, r11, #1
//ZZ             str  r11, [r12]
//ZZ             ldr  r11, [r12+4]
//ZZ             adc  r11, r11, #0
//ZZ             str  r11, [r12+4]
//ZZ          */
//ZZ          p = imm32_to_iregNo_EXACTLY2(p, /*r*/12, 0x65556555);
//ZZ          *p++ = 0xE59CB000;
//ZZ          *p++ = 0xE29BB001;
//ZZ          *p++ = 0xE58CB000;
//ZZ          *p++ = 0xE59CB004;
//ZZ          *p++ = 0xE2ABB000;
//ZZ          *p++ = 0xE58CB004;
//ZZ          /* Tell the caller .. */
//ZZ          vassert(!(*is_profInc));
//ZZ          *is_profInc = True;
//ZZ          goto done;
//ZZ       }

      /* ... */
      default: 
         goto bad;
    }

  bad:
   ppARM64Instr(i);
   vpanic("emit_ARM64Instr");
   /*NOTREACHED*/

  done:
   vassert(((UChar*)p) - &buf[0] <= 36);
   return ((UChar*)p) - &buf[0];
}


/* How big is an event check?  See case for ARM64in_EvCheck in
   emit_ARM64Instr just above.  That crosschecks what this returns, so
   we can tell if we're inconsistent. */
Int evCheckSzB_ARM64 ( void )
{
   return 24;
}


/* NB: what goes on here has to be very closely coordinated with the
   emitInstr case for XDirect, above. */
VexInvalRange chainXDirect_ARM64 ( void* place_to_chain,
                                   void* disp_cp_chain_me_EXPECTED,
                                   void* place_to_jump_to )
{
   /* What we're expecting to see is:
        movw x9, disp_cp_chain_me_to_EXPECTED[15:0]
        movk x9, disp_cp_chain_me_to_EXPECTED[31:15], lsl 16
        movk x9, disp_cp_chain_me_to_EXPECTED[47:32], lsl 32
        movk x9, disp_cp_chain_me_to_EXPECTED[63:48], lsl 48
        blr  x9
      viz
        <16 bytes generated by imm64_to_iregNo_EXACTLY4>
        D6 3F 01 20
   */
   UInt* p = (UInt*)place_to_chain;
   vassert(0 == (3 & (HWord)p));
   vassert(is_imm64_to_iregNo_EXACTLY4(
              p, /*x*/9, Ptr_to_ULong(disp_cp_chain_me_EXPECTED)));
   vassert(p[4] == 0xD63F0120);

   /* And what we want to change it to is:
        movw x9, place_to_jump_to[15:0]
        movk x9, place_to_jump_to[31:15], lsl 16
        movk x9, place_to_jump_to[47:32], lsl 32
        movk x9, place_to_jump_to[63:48], lsl 48
        br   x9
      viz
        <16 bytes generated by imm64_to_iregNo_EXACTLY4>
        D6 1F 01 20

      The replacement has the same length as the original.
   */
   (void)imm64_to_iregNo_EXACTLY4(
            p, /*x*/9, Ptr_to_ULong(place_to_jump_to));
   p[4] = 0xD61F0120;

   VexInvalRange vir = {(HWord)p, 20};
   return vir;
}


/* NB: what goes on here has to be very closely coordinated with the
   emitInstr case for XDirect, above. */
VexInvalRange unchainXDirect_ARM64 ( void* place_to_unchain,
                                     void* place_to_jump_to_EXPECTED,
                                     void* disp_cp_chain_me )
{
   /* What we're expecting to see is:
        movw x9, place_to_jump_to_EXPECTED[15:0]
        movk x9, place_to_jump_to_EXPECTED[31:15], lsl 16
        movk x9, place_to_jump_to_EXPECTED[47:32], lsl 32
        movk x9, place_to_jump_to_EXPECTED[63:48], lsl 48
        br   x9
      viz
        <16 bytes generated by imm64_to_iregNo_EXACTLY4>
        D6 1F 01 20
   */
   UInt* p = (UInt*)place_to_unchain;
   vassert(0 == (3 & (HWord)p));
   vassert(is_imm64_to_iregNo_EXACTLY4(
              p, /*x*/9, Ptr_to_ULong(place_to_jump_to_EXPECTED)));
   vassert(p[4] == 0xD61F0120);

   /* And what we want to change it to is:
        movw x9, disp_cp_chain_me_to[15:0]
        movk x9, disp_cp_chain_me_to[31:15], lsl 16
        movk x9, disp_cp_chain_me_to[47:32], lsl 32
        movk x9, disp_cp_chain_me_to[63:48], lsl 48
        blr  x9
      viz
        <16 bytes generated by imm64_to_iregNo_EXACTLY4>
        D6 3F 01 20
   */
   (void)imm64_to_iregNo_EXACTLY4(
            p, /*x*/9, Ptr_to_ULong(disp_cp_chain_me));
   p[4] = 0xD63F0120;

   VexInvalRange vir = {(HWord)p, 20};
   return vir;
}


//ZZ /* Patch the counter address into a profile inc point, as previously
//ZZ    created by the ARMin_ProfInc case for emit_ARMInstr. */
//ZZ VexInvalRange patchProfInc_ARM ( void*  place_to_patch,
//ZZ                                  ULong* location_of_counter )
//ZZ {
//ZZ    vassert(sizeof(ULong*) == 4);
//ZZ    UInt* p = (UInt*)place_to_patch;
//ZZ    vassert(0 == (3 & (HWord)p));
//ZZ    vassert(is_imm32_to_iregNo_EXACTLY2(p, /*r*/12, 0x65556555));
//ZZ    vassert(p[2] == 0xE59CB000);
//ZZ    vassert(p[3] == 0xE29BB001);
//ZZ    vassert(p[4] == 0xE58CB000);
//ZZ    vassert(p[5] == 0xE59CB004);
//ZZ    vassert(p[6] == 0xE2ABB000);
//ZZ    vassert(p[7] == 0xE58CB004);
//ZZ    imm32_to_iregNo_EXACTLY2(p, /*r*/12, 
//ZZ                             (UInt)Ptr_to_ULong(location_of_counter));
//ZZ    VexInvalRange vir = {(HWord)p, 8};
//ZZ    return vir;
//ZZ }
//ZZ 
//ZZ 
//ZZ #undef BITS4
//ZZ #undef X0000
//ZZ #undef X0001
//ZZ #undef X0010
//ZZ #undef X0011
//ZZ #undef X0100
//ZZ #undef X0101
//ZZ #undef X0110
//ZZ #undef X0111
//ZZ #undef X1000
//ZZ #undef X1001
//ZZ #undef X1010
//ZZ #undef X1011
//ZZ #undef X1100
//ZZ #undef X1101
//ZZ #undef X1110
//ZZ #undef X1111
//ZZ #undef XXXXX___
//ZZ #undef XXXXXX__
//ZZ #undef XXX___XX
//ZZ #undef XXXXX__X
//ZZ #undef XXXXXXXX
//ZZ #undef XX______

/*---------------------------------------------------------------*/
/*--- end                                   host_arm64_defs.c ---*/
/*---------------------------------------------------------------*/
