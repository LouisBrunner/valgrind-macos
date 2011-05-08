
/*---------------------------------------------------------------*/
/*--- begin                                   host_ppc_defs.c ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2004-2010 OpenWorks LLP
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
#include "libvex.h"
#include "libvex_trc_values.h"

#include "main_util.h"
#include "host_generic_regs.h"
#include "host_ppc_defs.h"


/* --------- Registers. --------- */

void ppHRegPPC ( HReg reg ) 
{
   Int r;
   static HChar* ireg32_names[32] 
      = { "%r0",  "%r1",  "%r2",  "%r3",
          "%r4",  "%r5",  "%r6",  "%r7",
          "%r8",  "%r9",  "%r10", "%r11",
          "%r12", "%r13", "%r14", "%r15",
          "%r16", "%r17", "%r18", "%r19",
          "%r20", "%r21", "%r22", "%r23",
          "%r24", "%r25", "%r26", "%r27",
          "%r28", "%r29", "%r30", "%r31" };
   /* Be generic for all virtual regs. */
   if (hregIsVirtual(reg)) {
      ppHReg(reg);
      return;
   }
   /* But specific for real regs. */
   switch (hregClass(reg)) {
   case HRcInt64:
      r = hregNumber(reg);
      vassert(r >= 0 && r < 32);
      vex_printf("%s", ireg32_names[r]);
      return;
   case HRcInt32:
      r = hregNumber(reg);
      vassert(r >= 0 && r < 32);
      vex_printf("%s", ireg32_names[r]);
      return;
   case HRcFlt64:
      r = hregNumber(reg);
      vassert(r >= 0 && r < 32);
      vex_printf("%%fr%d", r);
      return;
   case HRcVec128:
      r = hregNumber(reg);
      vassert(r >= 0 && r < 32);
      vex_printf("%%v%d", r);
      return;
   default:
      vpanic("ppHRegPPC");
   }
}


#define MkHRegGPR(_n, _mode64) \
   mkHReg(_n, _mode64 ? HRcInt64 : HRcInt32, False)

HReg hregPPC_GPR0  ( Bool mode64 ) { return MkHRegGPR( 0, mode64); }
HReg hregPPC_GPR1  ( Bool mode64 ) { return MkHRegGPR( 1, mode64); }
HReg hregPPC_GPR2  ( Bool mode64 ) { return MkHRegGPR( 2, mode64); }
HReg hregPPC_GPR3  ( Bool mode64 ) { return MkHRegGPR( 3, mode64); }
HReg hregPPC_GPR4  ( Bool mode64 ) { return MkHRegGPR( 4, mode64); }
HReg hregPPC_GPR5  ( Bool mode64 ) { return MkHRegGPR( 5, mode64); }
HReg hregPPC_GPR6  ( Bool mode64 ) { return MkHRegGPR( 6, mode64); }
HReg hregPPC_GPR7  ( Bool mode64 ) { return MkHRegGPR( 7, mode64); }
HReg hregPPC_GPR8  ( Bool mode64 ) { return MkHRegGPR( 8, mode64); }
HReg hregPPC_GPR9  ( Bool mode64 ) { return MkHRegGPR( 9, mode64); }
HReg hregPPC_GPR10 ( Bool mode64 ) { return MkHRegGPR(10, mode64); }
HReg hregPPC_GPR11 ( Bool mode64 ) { return MkHRegGPR(11, mode64); }
HReg hregPPC_GPR12 ( Bool mode64 ) { return MkHRegGPR(12, mode64); }
HReg hregPPC_GPR13 ( Bool mode64 ) { return MkHRegGPR(13, mode64); }
HReg hregPPC_GPR14 ( Bool mode64 ) { return MkHRegGPR(14, mode64); }
HReg hregPPC_GPR15 ( Bool mode64 ) { return MkHRegGPR(15, mode64); }
HReg hregPPC_GPR16 ( Bool mode64 ) { return MkHRegGPR(16, mode64); }
HReg hregPPC_GPR17 ( Bool mode64 ) { return MkHRegGPR(17, mode64); }
HReg hregPPC_GPR18 ( Bool mode64 ) { return MkHRegGPR(18, mode64); }
HReg hregPPC_GPR19 ( Bool mode64 ) { return MkHRegGPR(19, mode64); }
HReg hregPPC_GPR20 ( Bool mode64 ) { return MkHRegGPR(20, mode64); }
HReg hregPPC_GPR21 ( Bool mode64 ) { return MkHRegGPR(21, mode64); }
HReg hregPPC_GPR22 ( Bool mode64 ) { return MkHRegGPR(22, mode64); }
HReg hregPPC_GPR23 ( Bool mode64 ) { return MkHRegGPR(23, mode64); }
HReg hregPPC_GPR24 ( Bool mode64 ) { return MkHRegGPR(24, mode64); }
HReg hregPPC_GPR25 ( Bool mode64 ) { return MkHRegGPR(25, mode64); }
HReg hregPPC_GPR26 ( Bool mode64 ) { return MkHRegGPR(26, mode64); }
HReg hregPPC_GPR27 ( Bool mode64 ) { return MkHRegGPR(27, mode64); }
HReg hregPPC_GPR28 ( Bool mode64 ) { return MkHRegGPR(28, mode64); }
HReg hregPPC_GPR29 ( Bool mode64 ) { return MkHRegGPR(29, mode64); }
HReg hregPPC_GPR30 ( Bool mode64 ) { return MkHRegGPR(30, mode64); }
HReg hregPPC_GPR31 ( Bool mode64 ) { return MkHRegGPR(31, mode64); }

#undef MK_INT_HREG

HReg hregPPC_FPR0  ( void ) { return mkHReg( 0, HRcFlt64, False); }
HReg hregPPC_FPR1  ( void ) { return mkHReg( 1, HRcFlt64, False); }
HReg hregPPC_FPR2  ( void ) { return mkHReg( 2, HRcFlt64, False); }
HReg hregPPC_FPR3  ( void ) { return mkHReg( 3, HRcFlt64, False); }
HReg hregPPC_FPR4  ( void ) { return mkHReg( 4, HRcFlt64, False); }
HReg hregPPC_FPR5  ( void ) { return mkHReg( 5, HRcFlt64, False); }
HReg hregPPC_FPR6  ( void ) { return mkHReg( 6, HRcFlt64, False); }
HReg hregPPC_FPR7  ( void ) { return mkHReg( 7, HRcFlt64, False); }
HReg hregPPC_FPR8  ( void ) { return mkHReg( 8, HRcFlt64, False); }
HReg hregPPC_FPR9  ( void ) { return mkHReg( 9, HRcFlt64, False); }
HReg hregPPC_FPR10 ( void ) { return mkHReg(10, HRcFlt64, False); }
HReg hregPPC_FPR11 ( void ) { return mkHReg(11, HRcFlt64, False); }
HReg hregPPC_FPR12 ( void ) { return mkHReg(12, HRcFlt64, False); }
HReg hregPPC_FPR13 ( void ) { return mkHReg(13, HRcFlt64, False); }
HReg hregPPC_FPR14 ( void ) { return mkHReg(14, HRcFlt64, False); }
HReg hregPPC_FPR15 ( void ) { return mkHReg(15, HRcFlt64, False); }
HReg hregPPC_FPR16 ( void ) { return mkHReg(16, HRcFlt64, False); }
HReg hregPPC_FPR17 ( void ) { return mkHReg(17, HRcFlt64, False); }
HReg hregPPC_FPR18 ( void ) { return mkHReg(18, HRcFlt64, False); }
HReg hregPPC_FPR19 ( void ) { return mkHReg(19, HRcFlt64, False); }
HReg hregPPC_FPR20 ( void ) { return mkHReg(20, HRcFlt64, False); }
HReg hregPPC_FPR21 ( void ) { return mkHReg(21, HRcFlt64, False); }
HReg hregPPC_FPR22 ( void ) { return mkHReg(22, HRcFlt64, False); }
HReg hregPPC_FPR23 ( void ) { return mkHReg(23, HRcFlt64, False); }
HReg hregPPC_FPR24 ( void ) { return mkHReg(24, HRcFlt64, False); }
HReg hregPPC_FPR25 ( void ) { return mkHReg(25, HRcFlt64, False); }
HReg hregPPC_FPR26 ( void ) { return mkHReg(26, HRcFlt64, False); }
HReg hregPPC_FPR27 ( void ) { return mkHReg(27, HRcFlt64, False); }
HReg hregPPC_FPR28 ( void ) { return mkHReg(28, HRcFlt64, False); }
HReg hregPPC_FPR29 ( void ) { return mkHReg(29, HRcFlt64, False); }
HReg hregPPC_FPR30 ( void ) { return mkHReg(30, HRcFlt64, False); }
HReg hregPPC_FPR31 ( void ) { return mkHReg(31, HRcFlt64, False); }

HReg hregPPC_VR0  ( void ) { return mkHReg( 0, HRcVec128, False); }
HReg hregPPC_VR1  ( void ) { return mkHReg( 1, HRcVec128, False); }
HReg hregPPC_VR2  ( void ) { return mkHReg( 2, HRcVec128, False); }
HReg hregPPC_VR3  ( void ) { return mkHReg( 3, HRcVec128, False); }
HReg hregPPC_VR4  ( void ) { return mkHReg( 4, HRcVec128, False); }
HReg hregPPC_VR5  ( void ) { return mkHReg( 5, HRcVec128, False); }
HReg hregPPC_VR6  ( void ) { return mkHReg( 6, HRcVec128, False); }
HReg hregPPC_VR7  ( void ) { return mkHReg( 7, HRcVec128, False); }
HReg hregPPC_VR8  ( void ) { return mkHReg( 8, HRcVec128, False); }
HReg hregPPC_VR9  ( void ) { return mkHReg( 9, HRcVec128, False); }
HReg hregPPC_VR10 ( void ) { return mkHReg(10, HRcVec128, False); }
HReg hregPPC_VR11 ( void ) { return mkHReg(11, HRcVec128, False); }
HReg hregPPC_VR12 ( void ) { return mkHReg(12, HRcVec128, False); }
HReg hregPPC_VR13 ( void ) { return mkHReg(13, HRcVec128, False); }
HReg hregPPC_VR14 ( void ) { return mkHReg(14, HRcVec128, False); }
HReg hregPPC_VR15 ( void ) { return mkHReg(15, HRcVec128, False); }
HReg hregPPC_VR16 ( void ) { return mkHReg(16, HRcVec128, False); }
HReg hregPPC_VR17 ( void ) { return mkHReg(17, HRcVec128, False); }
HReg hregPPC_VR18 ( void ) { return mkHReg(18, HRcVec128, False); }
HReg hregPPC_VR19 ( void ) { return mkHReg(19, HRcVec128, False); }
HReg hregPPC_VR20 ( void ) { return mkHReg(20, HRcVec128, False); }
HReg hregPPC_VR21 ( void ) { return mkHReg(21, HRcVec128, False); }
HReg hregPPC_VR22 ( void ) { return mkHReg(22, HRcVec128, False); }
HReg hregPPC_VR23 ( void ) { return mkHReg(23, HRcVec128, False); }
HReg hregPPC_VR24 ( void ) { return mkHReg(24, HRcVec128, False); }
HReg hregPPC_VR25 ( void ) { return mkHReg(25, HRcVec128, False); }
HReg hregPPC_VR26 ( void ) { return mkHReg(26, HRcVec128, False); }
HReg hregPPC_VR27 ( void ) { return mkHReg(27, HRcVec128, False); }
HReg hregPPC_VR28 ( void ) { return mkHReg(28, HRcVec128, False); }
HReg hregPPC_VR29 ( void ) { return mkHReg(29, HRcVec128, False); }
HReg hregPPC_VR30 ( void ) { return mkHReg(30, HRcVec128, False); }
HReg hregPPC_VR31 ( void ) { return mkHReg(31, HRcVec128, False); }

void getAllocableRegs_PPC ( Int* nregs, HReg** arr, Bool mode64 )
{
   UInt i=0;
   if (mode64)
      *nregs = (32-9) + (32-24) + (32-24);
   else
      *nregs = (32-7) + (32-24) + (32-24);
   *arr = LibVEX_Alloc(*nregs * sizeof(HReg));
   // GPR0 = scratch reg where poss. - some ops interpret as value zero
   // GPR1 = stack pointer
   // GPR2 = TOC pointer
   (*arr)[i++] = hregPPC_GPR3(mode64);
   (*arr)[i++] = hregPPC_GPR4(mode64);
   (*arr)[i++] = hregPPC_GPR5(mode64);
   (*arr)[i++] = hregPPC_GPR6(mode64);
   (*arr)[i++] = hregPPC_GPR7(mode64);
   (*arr)[i++] = hregPPC_GPR8(mode64);
   (*arr)[i++] = hregPPC_GPR9(mode64);
   (*arr)[i++] = hregPPC_GPR10(mode64);
   if (!mode64) {
      /* in mode64: 
         r11 used for calls by ptr / env ptr for some langs
         r12 used for exception handling and global linkage code */
      (*arr)[i++] = hregPPC_GPR11(mode64);
      (*arr)[i++] = hregPPC_GPR12(mode64);
   }
   // GPR13 = thread specific pointer
   // GPR14 and above are callee save.  Yay.
   (*arr)[i++] = hregPPC_GPR14(mode64);
   (*arr)[i++] = hregPPC_GPR15(mode64);
   (*arr)[i++] = hregPPC_GPR16(mode64);
   (*arr)[i++] = hregPPC_GPR17(mode64);
   (*arr)[i++] = hregPPC_GPR18(mode64);
   (*arr)[i++] = hregPPC_GPR19(mode64);
   (*arr)[i++] = hregPPC_GPR20(mode64);
   (*arr)[i++] = hregPPC_GPR21(mode64);
   (*arr)[i++] = hregPPC_GPR22(mode64);
   (*arr)[i++] = hregPPC_GPR23(mode64);
   (*arr)[i++] = hregPPC_GPR24(mode64);
   (*arr)[i++] = hregPPC_GPR25(mode64);
   (*arr)[i++] = hregPPC_GPR26(mode64);
   (*arr)[i++] = hregPPC_GPR27(mode64);
   (*arr)[i++] = hregPPC_GPR28(mode64);
   // GPR29 is reserved for the dispatcher
   // GPR30 is reserved as AltiVec spill reg temporary
   // GPR31 is reserved for the GuestStatePtr

   /* Don't waste the reg-allocs's time trawling through zillions of
      FP registers - they mostly will never be used.  We'll tolerate
      the occasional extra spill instead. */
   /* For both ppc32-linux and ppc64-linux, f14-f31 are callee save.
      So use them. */
   (*arr)[i++] = hregPPC_FPR14();
   (*arr)[i++] = hregPPC_FPR15();
   (*arr)[i++] = hregPPC_FPR16();
   (*arr)[i++] = hregPPC_FPR17();
   (*arr)[i++] = hregPPC_FPR18();
   (*arr)[i++] = hregPPC_FPR19();
   (*arr)[i++] = hregPPC_FPR20();
   (*arr)[i++] = hregPPC_FPR21();

   /* Same deal re Altivec */
   /* For both ppc32-linux and ppc64-linux, v20-v31 are callee save.
      So use them. */
   /* NB, vr29 is used as a scratch temporary -- do not allocate */
   (*arr)[i++] = hregPPC_VR20();
   (*arr)[i++] = hregPPC_VR21();
   (*arr)[i++] = hregPPC_VR22();
   (*arr)[i++] = hregPPC_VR23();
   (*arr)[i++] = hregPPC_VR24();
   (*arr)[i++] = hregPPC_VR25();
   (*arr)[i++] = hregPPC_VR26();
   (*arr)[i++] = hregPPC_VR27();

   vassert(i == *nregs);
}


/* --------- Condition codes, Intel encoding. --------- */

HChar* showPPCCondCode ( PPCCondCode cond )
{
   if (cond.test == Pct_ALWAYS) return "always";

   switch (cond.flag) {
   case Pcf_7SO:
      return (cond.test == Pct_TRUE) ? "cr7.so=1" : "cr7.so=0";
   case Pcf_7EQ:
      return (cond.test == Pct_TRUE) ? "cr7.eq=1" : "cr7.eq=0";
   case Pcf_7GT:
      return (cond.test == Pct_TRUE) ? "cr7.gt=1" : "cr7.gt=0";
   case Pcf_7LT:
      return (cond.test == Pct_TRUE) ? "cr7.lt=1" : "cr7.lt=0";
   case Pcf_NONE:
      return "no-flag";
   default: vpanic("ppPPCCondCode");
   }
}

/* construct condition code */
PPCCondCode mk_PPCCondCode ( PPCCondTest test, PPCCondFlag flag )
{
   PPCCondCode cc;
   cc.flag = flag;
   cc.test = test;
   if (test == Pct_ALWAYS) { 
      vassert(flag == Pcf_NONE);
   } else {
      vassert(flag != Pcf_NONE);
   }
   return cc;
}

/* false->true, true->false */
PPCCondTest invertCondTest ( PPCCondTest ct )
{
   vassert(ct != Pct_ALWAYS);
   return (ct == Pct_TRUE) ? Pct_FALSE : Pct_TRUE;
}


/* --------- PPCAMode: memory address expressions. --------- */

PPCAMode* PPCAMode_IR ( Int idx, HReg base ) {
   PPCAMode* am = LibVEX_Alloc(sizeof(PPCAMode));
   vassert(idx >= -0x8000 && idx < 0x8000);
   am->tag = Pam_IR;
   am->Pam.IR.base = base;
   am->Pam.IR.index = idx;
   return am;
}
PPCAMode* PPCAMode_RR ( HReg idx, HReg base ) {
   PPCAMode* am = LibVEX_Alloc(sizeof(PPCAMode));
   am->tag = Pam_RR;
   am->Pam.RR.base = base;
   am->Pam.RR.index = idx;
   return am;
}

PPCAMode* dopyPPCAMode ( PPCAMode* am ) {
   switch (am->tag) {
   case Pam_IR: 
      return PPCAMode_IR( am->Pam.IR.index, am->Pam.IR.base );
   case Pam_RR: 
      return PPCAMode_RR( am->Pam.RR.index, am->Pam.RR.base );
   default:
      vpanic("dopyPPCAMode");
   }
}

void ppPPCAMode ( PPCAMode* am ) {
   switch (am->tag) {
   case Pam_IR: 
      if (am->Pam.IR.index == 0)
         vex_printf("0(");
      else
         vex_printf("%d(", (Int)am->Pam.IR.index);
      ppHRegPPC(am->Pam.IR.base);
      vex_printf(")");
      return;
   case Pam_RR:
      ppHRegPPC(am->Pam.RR.base);
      vex_printf(",");
      ppHRegPPC(am->Pam.RR.index);
      return;
   default:
      vpanic("ppPPCAMode");
   }
}

static void addRegUsage_PPCAMode ( HRegUsage* u, PPCAMode* am ) {
   switch (am->tag) {
   case Pam_IR: 
      addHRegUse(u, HRmRead, am->Pam.IR.base);
      return;
   case Pam_RR:
      addHRegUse(u, HRmRead, am->Pam.RR.base);
      addHRegUse(u, HRmRead, am->Pam.RR.index);
      return;
   default:
      vpanic("addRegUsage_PPCAMode");
   }
}

static void mapRegs_PPCAMode ( HRegRemap* m, PPCAMode* am ) {
   switch (am->tag) {
   case Pam_IR: 
      am->Pam.IR.base = lookupHRegRemap(m, am->Pam.IR.base);
      return;
   case Pam_RR:
      am->Pam.RR.base = lookupHRegRemap(m, am->Pam.RR.base);
      am->Pam.RR.index = lookupHRegRemap(m, am->Pam.RR.index);
      return;
   default:
      vpanic("mapRegs_PPCAMode");
   }
}

/* --------- Operand, which can be a reg or a u16/s16. --------- */

PPCRH* PPCRH_Imm ( Bool syned, UShort imm16 ) {
   PPCRH* op         = LibVEX_Alloc(sizeof(PPCRH));
   op->tag           = Prh_Imm;
   op->Prh.Imm.syned = syned;
   op->Prh.Imm.imm16 = imm16;
   /* If this is a signed value, ensure it's not -32768, so that we
      are guaranteed always to be able to negate if needed. */
   if (syned)
      vassert(imm16 != 0x8000);
   vassert(syned == True || syned == False);
   return op;
}
PPCRH* PPCRH_Reg ( HReg reg ) {
   PPCRH* op       = LibVEX_Alloc(sizeof(PPCRH));
   op->tag         = Prh_Reg;
   op->Prh.Reg.reg = reg;
   return op;
}

void ppPPCRH ( PPCRH* op ) {
   switch (op->tag) {
   case Prh_Imm: 
      if (op->Prh.Imm.syned)
         vex_printf("%d", (Int)(Short)op->Prh.Imm.imm16);
      else
         vex_printf("%u", (UInt)(UShort)op->Prh.Imm.imm16);
      return;
   case Prh_Reg: 
      ppHRegPPC(op->Prh.Reg.reg);
      return;
   default: 
      vpanic("ppPPCRH");
   }
}

/* An PPCRH can only be used in a "read" context (what would it mean
   to write or modify a literal?) and so we enumerate its registers
   accordingly. */
static void addRegUsage_PPCRH ( HRegUsage* u, PPCRH* op ) {
   switch (op->tag) {
   case Prh_Imm: 
      return;
   case Prh_Reg: 
      addHRegUse(u, HRmRead, op->Prh.Reg.reg);
      return;
   default: 
      vpanic("addRegUsage_PPCRH");
   }
}

static void mapRegs_PPCRH ( HRegRemap* m, PPCRH* op ) {
   switch (op->tag) {
   case Prh_Imm: 
      return;
   case Prh_Reg: 
      op->Prh.Reg.reg = lookupHRegRemap(m, op->Prh.Reg.reg);
      return;
   default: 
      vpanic("mapRegs_PPCRH");
   }
}


/* --------- Operand, which can be a reg or a u32/64. --------- */

PPCRI* PPCRI_Imm ( ULong imm64 ) {
   PPCRI* op   = LibVEX_Alloc(sizeof(PPCRI));
   op->tag     = Pri_Imm;
   op->Pri.Imm = imm64;
   return op;
}
PPCRI* PPCRI_Reg ( HReg reg ) {
   PPCRI* op   = LibVEX_Alloc(sizeof(PPCRI));
   op->tag     = Pri_Reg;
   op->Pri.Reg = reg;
   return op;
}

void ppPPCRI ( PPCRI* dst ) {
   switch (dst->tag) {
      case Pri_Imm: 
         vex_printf("0x%llx", dst->Pri.Imm);
         break;
      case Pri_Reg: 
         ppHRegPPC(dst->Pri.Reg);
         break;
      default: 
         vpanic("ppPPCRI");
   }
}

/* An PPCRI can only be used in a "read" context (what would it
   mean to write or modify a literal?) and so we enumerate its
   registers accordingly. */
static void addRegUsage_PPCRI ( HRegUsage* u, PPCRI* dst ) {
   switch (dst->tag) {
      case Pri_Imm: 
         return;
      case Pri_Reg: 
         addHRegUse(u, HRmRead, dst->Pri.Reg);
         return;
      default: 
         vpanic("addRegUsage_PPCRI");
   }
}

static void mapRegs_PPCRI ( HRegRemap* m, PPCRI* dst ) {
   switch (dst->tag) {
      case Pri_Imm: 
         return;
      case Pri_Reg: 
         dst->Pri.Reg = lookupHRegRemap(m, dst->Pri.Reg);
         return;
      default: 
         vpanic("mapRegs_PPCRI");
   }
}


/* --------- Operand, which can be a vector reg or a simm5. --------- */

PPCVI5s* PPCVI5s_Imm ( Char simm5 ) {
   PPCVI5s* op   = LibVEX_Alloc(sizeof(PPCVI5s));
   op->tag       = Pvi_Imm;
   op->Pvi.Imm5s = simm5;
   vassert(simm5 >= -16 && simm5 <= 15);
   return op;
}
PPCVI5s* PPCVI5s_Reg ( HReg reg ) {
   PPCVI5s* op = LibVEX_Alloc(sizeof(PPCVI5s));
   op->tag     = Pvi_Reg;
   op->Pvi.Reg = reg;
   vassert(hregClass(reg) == HRcVec128);
   return op;
}

void ppPPCVI5s ( PPCVI5s* src ) {
   switch (src->tag) {
      case Pvi_Imm: 
         vex_printf("%d", (Int)src->Pvi.Imm5s);
         break;
      case Pvi_Reg: 
         ppHRegPPC(src->Pvi.Reg);
         break;
      default: 
         vpanic("ppPPCVI5s");
   }
}

/* An PPCVI5s can only be used in a "read" context (what would it
   mean to write or modify a literal?) and so we enumerate its
   registers accordingly. */
static void addRegUsage_PPCVI5s ( HRegUsage* u, PPCVI5s* dst ) {
   switch (dst->tag) {
      case Pvi_Imm: 
         return;
      case Pvi_Reg: 
         addHRegUse(u, HRmRead, dst->Pvi.Reg);
         return;
      default: 
         vpanic("addRegUsage_PPCVI5s");
   }
}

static void mapRegs_PPCVI5s ( HRegRemap* m, PPCVI5s* dst ) {
   switch (dst->tag) {
      case Pvi_Imm: 
         return;
      case Pvi_Reg: 
         dst->Pvi.Reg = lookupHRegRemap(m, dst->Pvi.Reg);
         return;
      default: 
         vpanic("mapRegs_PPCVI5s");
   }
}


/* --------- Instructions. --------- */

HChar* showPPCUnaryOp ( PPCUnaryOp op ) {
   switch (op) {
   case Pun_NOT:   return "not";
   case Pun_NEG:   return "neg";
   case Pun_CLZ32: return "cntlzw";
   case Pun_CLZ64: return "cntlzd";
   case Pun_EXTSW: return "extsw";
   default: vpanic("showPPCUnaryOp");
   }
}

HChar* showPPCAluOp ( PPCAluOp op, Bool immR ) {
   switch (op) {
      case Palu_ADD: return immR ? "addi"  : "add";
      case Palu_SUB: return immR ? "subi"  : "sub";
      case Palu_AND: return immR ? "andi." : "and";
      case Palu_OR:  return immR ? "ori"   : "or";
      case Palu_XOR: return immR ? "xori"  : "xor";
      default: vpanic("showPPCAluOp");
   }
}

HChar* showPPCShftOp ( PPCShftOp op, Bool immR, Bool sz32 ) {
   switch (op) {
      case Pshft_SHL: return sz32 ? (immR ? "slwi"  : "slw") : 
                                    (immR ? "sldi"  : "sld");
      case Pshft_SHR: return sz32 ? (immR ? "srwi"  : "srw") :
                                    (immR ? "srdi"  : "srd");
      case Pshft_SAR: return sz32 ? (immR ? "srawi" : "sraw") :
                                    (immR ? "sradi" : "srad");
      default: vpanic("showPPCShftOp");
   }
}

HChar* showPPCFpOp ( PPCFpOp op ) {
   switch (op) {
      case Pfp_ADDD:   return "fadd";
      case Pfp_SUBD:   return "fsub";
      case Pfp_MULD:   return "fmul";
      case Pfp_DIVD:   return "fdiv";
      case Pfp_MADDD:  return "fmadd";
      case Pfp_MSUBD:  return "fmsub";
      case Pfp_MADDS:  return "fmadds";
      case Pfp_MSUBS:  return "fmsubs";
      case Pfp_ADDS:   return "fadds";
      case Pfp_SUBS:   return "fsubs";
      case Pfp_MULS:   return "fmuls";
      case Pfp_DIVS:   return "fdivs";
      case Pfp_SQRT:   return "fsqrt";
      case Pfp_ABS:    return "fabs";
      case Pfp_NEG:    return "fneg";
      case Pfp_MOV:    return "fmr";
      case Pfp_RES:    return "fres";
      case Pfp_RSQRTE: return "frsqrte";
      case Pfp_FRIM:   return "frim";
      case Pfp_FRIN:   return "frin";
      case Pfp_FRIP:   return "frip";
      case Pfp_FRIZ:   return "friz";
      default: vpanic("showPPCFpOp");
   }
}

HChar* showPPCAvOp ( PPCAvOp op ) {
   switch (op) {

   /* Unary */
   case Pav_MOV:       return "vmr";      /* Mov */
     
   case Pav_AND:       return "vand";     /* Bitwise */
   case Pav_OR:        return "vor";
   case Pav_XOR:       return "vxor";
   case Pav_NOT:       return "vnot";

   case Pav_UNPCKH8S:  return "vupkhsb";  /* Unpack */
   case Pav_UNPCKH16S: return "vupkhsh";
   case Pav_UNPCKL8S:  return "vupklsb";
   case Pav_UNPCKL16S: return "vupklsh";
   case Pav_UNPCKHPIX: return "vupkhpx";
   case Pav_UNPCKLPIX: return "vupklpx";

   /* Integer binary */
   case Pav_ADDU:      return "vaddu_m";  // b,h,w
   case Pav_QADDU:     return "vaddu_s";  // b,h,w
   case Pav_QADDS:     return "vadds_s";  // b,h,w
     
   case Pav_SUBU:      return "vsubu_m";  // b,h,w
   case Pav_QSUBU:     return "vsubu_s";  // b,h,w
   case Pav_QSUBS:     return "vsubs_s";  // b,h,w
     
   case Pav_OMULU:     return "vmulou";   // b,h
   case Pav_OMULS:     return "vmulos";   // b,h
   case Pav_EMULU:     return "vmuleu";   // b,h
   case Pav_EMULS:     return "vmules";   // b,h
  
   case Pav_AVGU:      return "vavgu";    // b,h,w
   case Pav_AVGS:      return "vavgs";    // b,h,w
     
   case Pav_MAXU:      return "vmaxu";    // b,h,w
   case Pav_MAXS:      return "vmaxs";    // b,h,w
     
   case Pav_MINU:      return "vminu";    // b,h,w
   case Pav_MINS:      return "vmins";    // b,h,w
     
   /* Compare (always affects CR field 6) */
   case Pav_CMPEQU:    return "vcmpequ";  // b,h,w
   case Pav_CMPGTU:    return "vcmpgtu";  // b,h,w
   case Pav_CMPGTS:    return "vcmpgts";  // b,h,w

   /* Shift */
   case Pav_SHL:       return "vsl";      // ' ',b,h,w
   case Pav_SHR:       return "vsr";      // ' ',b,h,w
   case Pav_SAR:       return "vsra";     // b,h,w
   case Pav_ROTL:      return "vrl";      // b,h,w

   /* Pack */
   case Pav_PACKUU:    return "vpku_um";  // h,w
   case Pav_QPACKUU:   return "vpku_us";  // h,w
   case Pav_QPACKSU:   return "vpks_us";  // h,w
   case Pav_QPACKSS:   return "vpks_ss";  // h,w
   case Pav_PACKPXL:   return "vpkpx";

   /* Merge */
   case Pav_MRGHI:     return "vmrgh";    // b,h,w
   case Pav_MRGLO:     return "vmrgl";    // b,h,w

   default: vpanic("showPPCAvOp");
   }
}

HChar* showPPCAvFpOp ( PPCAvFpOp op ) {
   switch (op) {
   /* Floating Point Binary */
   case Pavfp_ADDF:      return "vaddfp";
   case Pavfp_SUBF:      return "vsubfp";
   case Pavfp_MULF:      return "vmaddfp";
   case Pavfp_MAXF:      return "vmaxfp";
   case Pavfp_MINF:      return "vminfp";
   case Pavfp_CMPEQF:    return "vcmpeqfp";
   case Pavfp_CMPGTF:    return "vcmpgtfp";
   case Pavfp_CMPGEF:    return "vcmpgefp";
     
   /* Floating Point Unary */
   case Pavfp_RCPF:      return "vrefp";
   case Pavfp_RSQRTF:    return "vrsqrtefp";
   case Pavfp_CVTU2F:    return "vcfux";
   case Pavfp_CVTS2F:    return "vcfsx";
   case Pavfp_QCVTF2U:   return "vctuxs";
   case Pavfp_QCVTF2S:   return "vctsxs";
   case Pavfp_ROUNDM:    return "vrfim";
   case Pavfp_ROUNDP:    return "vrfip";
   case Pavfp_ROUNDN:    return "vrfin";
   case Pavfp_ROUNDZ:    return "vrfiz";

   default: vpanic("showPPCAvFpOp");
   }
}

PPCInstr* PPCInstr_LI ( HReg dst, ULong imm64, Bool mode64 )
{
   PPCInstr* i     = LibVEX_Alloc(sizeof(PPCInstr));
   i->tag          = Pin_LI;
   i->Pin.LI.dst   = dst;
   i->Pin.LI.imm64 = imm64;
   if (!mode64)
      vassert( (Long)imm64 == (Long)(Int)(UInt)imm64 );
   return i;
}
PPCInstr* PPCInstr_Alu ( PPCAluOp op, HReg dst, 
                         HReg srcL, PPCRH* srcR ) {
   PPCInstr* i     = LibVEX_Alloc(sizeof(PPCInstr));
   i->tag          = Pin_Alu;
   i->Pin.Alu.op   = op;
   i->Pin.Alu.dst  = dst;
   i->Pin.Alu.srcL = srcL;
   i->Pin.Alu.srcR = srcR;
   return i;
}
PPCInstr* PPCInstr_Shft ( PPCShftOp op, Bool sz32, 
                          HReg dst, HReg srcL, PPCRH* srcR ) {
   PPCInstr* i      = LibVEX_Alloc(sizeof(PPCInstr));
   i->tag           = Pin_Shft;
   i->Pin.Shft.op   = op;
   i->Pin.Shft.sz32 = sz32;
   i->Pin.Shft.dst  = dst;
   i->Pin.Shft.srcL = srcL;
   i->Pin.Shft.srcR = srcR;
   return i;
}
PPCInstr* PPCInstr_AddSubC ( Bool isAdd, Bool setC,
                             HReg dst, HReg srcL, HReg srcR ) {
   PPCInstr* i          = LibVEX_Alloc(sizeof(PPCInstr));
   i->tag               = Pin_AddSubC;
   i->Pin.AddSubC.isAdd = isAdd;
   i->Pin.AddSubC.setC  = setC;
   i->Pin.AddSubC.dst   = dst;
   i->Pin.AddSubC.srcL  = srcL;
   i->Pin.AddSubC.srcR  = srcR;
   return i;
}
PPCInstr* PPCInstr_Cmp ( Bool syned, Bool sz32, 
                         UInt crfD, HReg srcL, PPCRH* srcR ) {
   PPCInstr* i      = LibVEX_Alloc(sizeof(PPCInstr));
   i->tag           = Pin_Cmp;
   i->Pin.Cmp.syned = syned;
   i->Pin.Cmp.sz32  = sz32;
   i->Pin.Cmp.crfD  = crfD;
   i->Pin.Cmp.srcL  = srcL;
   i->Pin.Cmp.srcR  = srcR;
   return i;
}
PPCInstr* PPCInstr_Unary ( PPCUnaryOp op, HReg dst, HReg src ) {
   PPCInstr* i      = LibVEX_Alloc(sizeof(PPCInstr));
   i->tag           = Pin_Unary;
   i->Pin.Unary.op  = op;
   i->Pin.Unary.dst = dst;
   i->Pin.Unary.src = src;
   return i;
}
PPCInstr* PPCInstr_MulL ( Bool syned, Bool hi, Bool sz32, 
                          HReg dst, HReg srcL, HReg srcR ) {
   PPCInstr* i       = LibVEX_Alloc(sizeof(PPCInstr));
   i->tag            = Pin_MulL;
   i->Pin.MulL.syned = syned;
   i->Pin.MulL.hi    = hi;
   i->Pin.MulL.sz32  = sz32;
   i->Pin.MulL.dst   = dst;
   i->Pin.MulL.srcL  = srcL;
   i->Pin.MulL.srcR  = srcR;
   /* if doing the low word, the signedness is irrelevant, but tie it
      down anyway. */
   if (!hi) vassert(!syned);
   return i;
}
PPCInstr* PPCInstr_Div ( Bool syned, Bool sz32,
                         HReg dst, HReg srcL, HReg srcR ) {
   PPCInstr* i      = LibVEX_Alloc(sizeof(PPCInstr));
   i->tag           = Pin_Div;
   i->Pin.Div.syned = syned;
   i->Pin.Div.sz32  = sz32;
   i->Pin.Div.dst   = dst;
   i->Pin.Div.srcL  = srcL;
   i->Pin.Div.srcR  = srcR;
   return i;
}
PPCInstr* PPCInstr_Call ( PPCCondCode cond, 
                          Addr64 target, UInt argiregs ) {
   UInt mask;
   PPCInstr* i          = LibVEX_Alloc(sizeof(PPCInstr));
   i->tag               = Pin_Call;
   i->Pin.Call.cond     = cond;
   i->Pin.Call.target   = target;
   i->Pin.Call.argiregs = argiregs;
   /* Only r3 .. r10 inclusive may be used as arg regs. Hence: */
   mask = (1<<3)|(1<<4)|(1<<5)|(1<<6)|(1<<7)|(1<<8)|(1<<9)|(1<<10);
   vassert(0 == (argiregs & ~mask));
   return i;
}
PPCInstr* PPCInstr_Goto ( IRJumpKind jk, 
                          PPCCondCode cond, PPCRI* dst ) {
   PPCInstr* i      = LibVEX_Alloc(sizeof(PPCInstr));
   i->tag           = Pin_Goto;
   i->Pin.Goto.cond = cond;
   i->Pin.Goto.dst  = dst;
   i->Pin.Goto.jk   = jk;
   return i;
}
PPCInstr* PPCInstr_CMov  ( PPCCondCode cond, 
                           HReg dst, PPCRI* src ) {
   PPCInstr* i      = LibVEX_Alloc(sizeof(PPCInstr));
   i->tag           = Pin_CMov;
   i->Pin.CMov.cond = cond;
   i->Pin.CMov.src  = src;
   i->Pin.CMov.dst  = dst;
   vassert(cond.test != Pct_ALWAYS);
   return i;
}
PPCInstr* PPCInstr_Load ( UChar sz,
                          HReg dst, PPCAMode* src, Bool mode64 ) {
   PPCInstr* i       = LibVEX_Alloc(sizeof(PPCInstr));
   i->tag            = Pin_Load;
   i->Pin.Load.sz    = sz;
   i->Pin.Load.src   = src;
   i->Pin.Load.dst   = dst;
   vassert(sz == 1 || sz == 2 || sz == 4 || sz == 8);
   if (sz == 8) vassert(mode64);
   return i;
}
PPCInstr* PPCInstr_LoadL ( UChar sz,
                           HReg dst, HReg src, Bool mode64 )
{
   PPCInstr* i       = LibVEX_Alloc(sizeof(PPCInstr));
   i->tag            = Pin_LoadL;
   i->Pin.LoadL.sz   = sz;
   i->Pin.LoadL.src  = src;
   i->Pin.LoadL.dst  = dst;
   vassert(sz == 4 || sz == 8);
   if (sz == 8) vassert(mode64);
   return i;
}
PPCInstr* PPCInstr_Store ( UChar sz, PPCAMode* dst, HReg src,
                           Bool mode64 ) {
   PPCInstr* i      = LibVEX_Alloc(sizeof(PPCInstr));
   i->tag           = Pin_Store;
   i->Pin.Store.sz  = sz;
   i->Pin.Store.src = src;
   i->Pin.Store.dst = dst;
   vassert(sz == 1 || sz == 2 || sz == 4 || sz == 8);
   if (sz == 8) vassert(mode64);
   return i;
}
PPCInstr* PPCInstr_StoreC ( UChar sz, HReg dst, HReg src, Bool mode64 ) {
   PPCInstr* i       = LibVEX_Alloc(sizeof(PPCInstr));
   i->tag            = Pin_StoreC;
   i->Pin.StoreC.sz  = sz;
   i->Pin.StoreC.src = src;
   i->Pin.StoreC.dst = dst;
   vassert(sz == 4 || sz == 8);
   if (sz == 8) vassert(mode64);
   return i;
}
PPCInstr* PPCInstr_Set ( PPCCondCode cond, HReg dst ) {
   PPCInstr* i     = LibVEX_Alloc(sizeof(PPCInstr));
   i->tag          = Pin_Set;
   i->Pin.Set.cond = cond;
   i->Pin.Set.dst  = dst;
   return i;
}
PPCInstr* PPCInstr_MfCR ( HReg dst )
{
   PPCInstr* i     = LibVEX_Alloc(sizeof(PPCInstr));
   i->tag          = Pin_MfCR;
   i->Pin.MfCR.dst = dst;
   return i;
}
PPCInstr* PPCInstr_MFence ( void )
{
   PPCInstr* i = LibVEX_Alloc(sizeof(PPCInstr));
   i->tag      = Pin_MFence;
   return i;
}

PPCInstr* PPCInstr_FpUnary ( PPCFpOp op, HReg dst, HReg src ) {
   PPCInstr* i        = LibVEX_Alloc(sizeof(PPCInstr));
   i->tag             = Pin_FpUnary;
   i->Pin.FpUnary.op  = op;
   i->Pin.FpUnary.dst = dst;
   i->Pin.FpUnary.src = src;
   return i;
}
PPCInstr* PPCInstr_FpBinary ( PPCFpOp op, HReg dst,
                              HReg srcL, HReg srcR ) {
   PPCInstr* i          = LibVEX_Alloc(sizeof(PPCInstr));
   i->tag               = Pin_FpBinary;
   i->Pin.FpBinary.op   = op;
   i->Pin.FpBinary.dst  = dst;
   i->Pin.FpBinary.srcL = srcL;
   i->Pin.FpBinary.srcR = srcR;
   return i;
}
PPCInstr* PPCInstr_FpMulAcc ( PPCFpOp op, HReg dst, HReg srcML, 
                                          HReg srcMR, HReg srcAcc )
{
   PPCInstr* i            = LibVEX_Alloc(sizeof(PPCInstr));
   i->tag                 = Pin_FpMulAcc;
   i->Pin.FpMulAcc.op     = op;
   i->Pin.FpMulAcc.dst    = dst;
   i->Pin.FpMulAcc.srcML  = srcML;
   i->Pin.FpMulAcc.srcMR  = srcMR;
   i->Pin.FpMulAcc.srcAcc = srcAcc;
   return i;
}
PPCInstr* PPCInstr_FpLdSt ( Bool isLoad, UChar sz,
                            HReg reg, PPCAMode* addr ) {
   PPCInstr* i          = LibVEX_Alloc(sizeof(PPCInstr));
   i->tag               = Pin_FpLdSt;
   i->Pin.FpLdSt.isLoad = isLoad;
   i->Pin.FpLdSt.sz     = sz;
   i->Pin.FpLdSt.reg    = reg;
   i->Pin.FpLdSt.addr   = addr;
   vassert(sz == 4 || sz == 8);
   return i;
}
PPCInstr* PPCInstr_FpSTFIW ( HReg addr, HReg data )
{
   PPCInstr* i         = LibVEX_Alloc(sizeof(PPCInstr));
   i->tag              = Pin_FpSTFIW;
   i->Pin.FpSTFIW.addr = addr;
   i->Pin.FpSTFIW.data = data;
   return i;
}
PPCInstr* PPCInstr_FpRSP ( HReg dst, HReg src ) {
   PPCInstr* i      = LibVEX_Alloc(sizeof(PPCInstr));
   i->tag           = Pin_FpRSP;
   i->Pin.FpRSP.dst = dst;
   i->Pin.FpRSP.src = src;
   return i;
}

/*
Valid combo | fromI | int32 | syned | flt64 |
--------------------------------------------
            |  n       n       n       n    |
--------------------------------------------
 F64->I64U  |  n       n       n       y    |
--------------------------------------------
            |  n       n       y       n    |
--------------------------------------------
 F64->I64S  |  n       n       y       y    |
--------------------------------------------
            |  n       y       n       n    |
--------------------------------------------
 F64->I32U  |  n       y       n       y    |
--------------------------------------------
            |  n       y       y       n    |
--------------------------------------------
 F64->I32S  |  n       y       y       y    |
--------------------------------------------
 I64U->F32  |  y       n       n       n    |
--------------------------------------------
 I64U->F64  |  y       n       n       y    |
--------------------------------------------
            |  y       n       y       n    |
--------------------------------------------
 I64S->F64  |  y       n       y       y    |
--------------------------------------------
            |  y       y       n       n    |
--------------------------------------------
            |  y       y       n       y    |
--------------------------------------------
            |  y       y       y       n    |
--------------------------------------------
            |  y       y       y       y    |
--------------------------------------------
*/
PPCInstr* PPCInstr_FpCftI ( Bool fromI, Bool int32, Bool syned,
                            Bool flt64, HReg dst, HReg src ) {
   Bool tmp = fromI | int32 | syned | flt64;
   vassert(tmp == True || tmp == False); // iow, no high bits set
   UShort conversion = 0;
   conversion = (fromI << 3) | (int32 << 2) | (syned << 1) | flt64;
   switch (conversion) {
      // Supported conversion operations
      case 1: case 3: case 5: case 7:
      case 8: case 9: case 11:
         break;
      default:
         vpanic("PPCInstr_FpCftI(ppc_host)");
   }
   PPCInstr* i         = LibVEX_Alloc(sizeof(PPCInstr));
   i->tag              = Pin_FpCftI;
   i->Pin.FpCftI.fromI = fromI;
   i->Pin.FpCftI.int32 = int32;
   i->Pin.FpCftI.syned = syned;
   i->Pin.FpCftI.flt64 = flt64;
   i->Pin.FpCftI.dst   = dst;
   i->Pin.FpCftI.src   = src;
   return i;
}
PPCInstr* PPCInstr_FpCMov ( PPCCondCode cond, HReg dst, HReg src ) {
   PPCInstr* i        = LibVEX_Alloc(sizeof(PPCInstr));
   i->tag             = Pin_FpCMov;
   i->Pin.FpCMov.cond = cond;
   i->Pin.FpCMov.dst  = dst;
   i->Pin.FpCMov.src  = src;
   vassert(cond.test != Pct_ALWAYS);
   return i;
}
PPCInstr* PPCInstr_FpLdFPSCR ( HReg src ) {
   PPCInstr* i          = LibVEX_Alloc(sizeof(PPCInstr));
   i->tag               = Pin_FpLdFPSCR;
   i->Pin.FpLdFPSCR.src = src;
   return i;
}
PPCInstr* PPCInstr_FpCmp ( HReg dst, HReg srcL, HReg srcR ) {
   PPCInstr* i       = LibVEX_Alloc(sizeof(PPCInstr));
   i->tag            = Pin_FpCmp;
   i->Pin.FpCmp.dst  = dst;
   i->Pin.FpCmp.srcL = srcL;
   i->Pin.FpCmp.srcR = srcR;
   return i;
}

/* Read/Write Link Register */
PPCInstr* PPCInstr_RdWrLR ( Bool wrLR, HReg gpr ) {
   PPCInstr* i        = LibVEX_Alloc(sizeof(PPCInstr));
   i->tag             = Pin_RdWrLR;
   i->Pin.RdWrLR.wrLR = wrLR;
   i->Pin.RdWrLR.gpr  = gpr;
   return i;
}

/* AltiVec */
PPCInstr* PPCInstr_AvLdSt ( Bool isLoad, UChar sz,
                            HReg reg, PPCAMode* addr ) {
   PPCInstr* i          = LibVEX_Alloc(sizeof(PPCInstr));
   i->tag               = Pin_AvLdSt;
   i->Pin.AvLdSt.isLoad = isLoad;
   i->Pin.AvLdSt.sz     = sz;
   i->Pin.AvLdSt.reg    = reg;
   i->Pin.AvLdSt.addr   = addr;
   return i;
}
PPCInstr* PPCInstr_AvUnary ( PPCAvOp op, HReg dst, HReg src ) {
   PPCInstr* i        = LibVEX_Alloc(sizeof(PPCInstr));
   i->tag             = Pin_AvUnary;
   i->Pin.AvUnary.op  = op;
   i->Pin.AvUnary.dst = dst;
   i->Pin.AvUnary.src = src;
   return i;
}
PPCInstr* PPCInstr_AvBinary ( PPCAvOp op, HReg dst,
                              HReg srcL, HReg srcR ) {
   PPCInstr* i          = LibVEX_Alloc(sizeof(PPCInstr));
   i->tag               = Pin_AvBinary;
   i->Pin.AvBinary.op   = op;
   i->Pin.AvBinary.dst  = dst;
   i->Pin.AvBinary.srcL = srcL;
   i->Pin.AvBinary.srcR = srcR;
   return i;
}
PPCInstr* PPCInstr_AvBin8x16 ( PPCAvOp op, HReg dst,
                               HReg srcL, HReg srcR ) {
   PPCInstr* i           = LibVEX_Alloc(sizeof(PPCInstr));
   i->tag                = Pin_AvBin8x16;
   i->Pin.AvBin8x16.op   = op;
   i->Pin.AvBin8x16.dst  = dst;
   i->Pin.AvBin8x16.srcL = srcL;
   i->Pin.AvBin8x16.srcR = srcR;
   return i;
}
PPCInstr* PPCInstr_AvBin16x8 ( PPCAvOp op, HReg dst,
                               HReg srcL, HReg srcR ) {
   PPCInstr* i           = LibVEX_Alloc(sizeof(PPCInstr));
   i->tag                = Pin_AvBin16x8;
   i->Pin.AvBin16x8.op   = op;
   i->Pin.AvBin16x8.dst  = dst;
   i->Pin.AvBin16x8.srcL = srcL;
   i->Pin.AvBin16x8.srcR = srcR;
   return i;
}
PPCInstr* PPCInstr_AvBin32x4 ( PPCAvOp op, HReg dst,
                               HReg srcL, HReg srcR ) {
   PPCInstr* i           = LibVEX_Alloc(sizeof(PPCInstr));
   i->tag                = Pin_AvBin32x4;
   i->Pin.AvBin32x4.op   = op;
   i->Pin.AvBin32x4.dst  = dst;
   i->Pin.AvBin32x4.srcL = srcL;
   i->Pin.AvBin32x4.srcR = srcR;
   return i;
}
PPCInstr* PPCInstr_AvBin32Fx4 ( PPCAvFpOp op, HReg dst,
                                HReg srcL, HReg srcR ) {
   PPCInstr* i            = LibVEX_Alloc(sizeof(PPCInstr));
   i->tag                 = Pin_AvBin32Fx4;
   i->Pin.AvBin32Fx4.op   = op;
   i->Pin.AvBin32Fx4.dst  = dst;
   i->Pin.AvBin32Fx4.srcL = srcL;
   i->Pin.AvBin32Fx4.srcR = srcR;
   return i;
}
PPCInstr* PPCInstr_AvUn32Fx4 ( PPCAvFpOp op, HReg dst, HReg src ) {
   PPCInstr* i          = LibVEX_Alloc(sizeof(PPCInstr));
   i->tag               = Pin_AvUn32Fx4;
   i->Pin.AvUn32Fx4.op  = op;
   i->Pin.AvUn32Fx4.dst = dst;
   i->Pin.AvUn32Fx4.src = src;
   return i;
}
PPCInstr* PPCInstr_AvPerm ( HReg dst, HReg srcL, HReg srcR, HReg ctl ) {
   PPCInstr* i        = LibVEX_Alloc(sizeof(PPCInstr));
   i->tag             = Pin_AvPerm;
   i->Pin.AvPerm.dst  = dst;
   i->Pin.AvPerm.srcL = srcL;
   i->Pin.AvPerm.srcR = srcR;
   i->Pin.AvPerm.ctl  = ctl;
   return i;
}
PPCInstr* PPCInstr_AvSel ( HReg ctl, HReg dst, HReg srcL, HReg srcR ) {
   PPCInstr* i       = LibVEX_Alloc(sizeof(PPCInstr));
   i->tag            = Pin_AvSel;
   i->Pin.AvSel.ctl  = ctl;
   i->Pin.AvSel.dst  = dst;
   i->Pin.AvSel.srcL = srcL;
   i->Pin.AvSel.srcR = srcR;
   return i;
}
PPCInstr* PPCInstr_AvShlDbl ( UChar shift, HReg dst,
                              HReg srcL, HReg srcR ) {
   PPCInstr* i           = LibVEX_Alloc(sizeof(PPCInstr));
   i->tag                = Pin_AvShlDbl;
   i->Pin.AvShlDbl.shift = shift;
   i->Pin.AvShlDbl.dst   = dst;
   i->Pin.AvShlDbl.srcL  = srcL;
   i->Pin.AvShlDbl.srcR  = srcR;
   return i;
}
PPCInstr* PPCInstr_AvSplat ( UChar sz, HReg dst, PPCVI5s* src ) {
   PPCInstr* i        = LibVEX_Alloc(sizeof(PPCInstr));
   i->tag             = Pin_AvSplat;
   i->Pin.AvSplat.sz  = sz;
   i->Pin.AvSplat.dst = dst;
   i->Pin.AvSplat.src = src;
   return i;
}
PPCInstr* PPCInstr_AvCMov ( PPCCondCode cond, HReg dst, HReg src ) {
   PPCInstr* i        = LibVEX_Alloc(sizeof(PPCInstr));
   i->tag             = Pin_AvCMov;
   i->Pin.AvCMov.cond = cond;
   i->Pin.AvCMov.dst  = dst;
   i->Pin.AvCMov.src  = src;
   vassert(cond.test != Pct_ALWAYS);
   return i;
}
PPCInstr* PPCInstr_AvLdVSCR ( HReg src ) {
   PPCInstr* i         = LibVEX_Alloc(sizeof(PPCInstr));
   i->tag              = Pin_AvLdVSCR;
   i->Pin.AvLdVSCR.src = src;
   return i;
}


/* Pretty Print instructions */
static void ppLoadImm ( HReg dst, ULong imm, Bool mode64 ) {
   vex_printf("li_word ");
   ppHRegPPC(dst);
   if (!mode64) {
      vex_printf(",0x%08x", (UInt)imm);
   } else {
      vex_printf(",0x%016llx", imm);
   }
}

static void ppMovReg ( HReg dst, HReg src ) {
   if (hregNumber(dst) != hregNumber(src)) {
      vex_printf("mr ");
      ppHRegPPC(dst);
      vex_printf(",");
      ppHRegPPC(src);
   }
}

void ppPPCInstr ( PPCInstr* i, Bool mode64 )
{
   switch (i->tag) {
   case Pin_LI:
      ppLoadImm(i->Pin.LI.dst, i->Pin.LI.imm64, mode64);
      break;
   case Pin_Alu: {
      HReg   r_srcL  = i->Pin.Alu.srcL;
      PPCRH* rh_srcR = i->Pin.Alu.srcR;
      /* special-case "mr" */
      if (i->Pin.Alu.op == Palu_OR &&   // or Rd,Rs,Rs == mr Rd,Rs
          rh_srcR->tag == Prh_Reg &&
          rh_srcR->Prh.Reg.reg == r_srcL) {
         vex_printf("mr ");
         ppHRegPPC(i->Pin.Alu.dst);
         vex_printf(",");
         ppHRegPPC(r_srcL);
         return;
      }
      /* special-case "li" */
      if (i->Pin.Alu.op == Palu_ADD &&   // addi Rd,0,imm == li Rd,imm
          rh_srcR->tag == Prh_Imm &&
          hregNumber(r_srcL) == 0) {
         vex_printf("li ");
         ppHRegPPC(i->Pin.Alu.dst);
         vex_printf(",");
         ppPPCRH(rh_srcR);
         return;
      }
      /* generic */
      vex_printf("%s ", showPPCAluOp(i->Pin.Alu.op,
                                     toBool(rh_srcR->tag == Prh_Imm)));
      ppHRegPPC(i->Pin.Alu.dst);
      vex_printf(",");
      ppHRegPPC(r_srcL);
      vex_printf(",");
      ppPPCRH(rh_srcR);
      return;
   }
   case Pin_Shft: {
      HReg   r_srcL  = i->Pin.Shft.srcL;
      PPCRH* rh_srcR = i->Pin.Shft.srcR;
      vex_printf("%s ", showPPCShftOp(i->Pin.Shft.op,
                                      toBool(rh_srcR->tag == Prh_Imm),
                                      i->Pin.Shft.sz32));
      ppHRegPPC(i->Pin.Shft.dst);
      vex_printf(",");
      ppHRegPPC(r_srcL);
      vex_printf(",");
      ppPPCRH(rh_srcR);
      return;
   }
   case Pin_AddSubC:
      vex_printf("%s%s ",
                 i->Pin.AddSubC.isAdd ? "add" : "sub",
                 i->Pin.AddSubC.setC ? "c" : "e");
      ppHRegPPC(i->Pin.AddSubC.dst);
      vex_printf(",");
      ppHRegPPC(i->Pin.AddSubC.srcL);
      vex_printf(",");
      ppHRegPPC(i->Pin.AddSubC.srcR);
      return;
   case Pin_Cmp:
      vex_printf("%s%c%s %%cr%u,",
                 i->Pin.Cmp.syned ? "cmp" : "cmpl",
                 i->Pin.Cmp.sz32 ? 'w' : 'd',
                 i->Pin.Cmp.srcR->tag == Prh_Imm ? "i" : "",
                 i->Pin.Cmp.crfD);
      ppHRegPPC(i->Pin.Cmp.srcL);
      vex_printf(",");
      ppPPCRH(i->Pin.Cmp.srcR);
      return;
   case Pin_Unary:
      vex_printf("%s ", showPPCUnaryOp(i->Pin.Unary.op));
      ppHRegPPC(i->Pin.Unary.dst);
      vex_printf(",");
      ppHRegPPC(i->Pin.Unary.src);
      return;
   case Pin_MulL:
      vex_printf("mul%c%c%s ",
                 i->Pin.MulL.hi ? 'h' : 'l',
                 i->Pin.MulL.sz32 ? 'w' : 'd',
                 i->Pin.MulL.hi ? (i->Pin.MulL.syned ? "s" : "u") : "");
      ppHRegPPC(i->Pin.MulL.dst);
      vex_printf(",");
      ppHRegPPC(i->Pin.MulL.srcL);
      vex_printf(",");
      ppHRegPPC(i->Pin.MulL.srcR);
      return;
   case Pin_Div:
      vex_printf("div%c%s ",
                 i->Pin.Div.sz32 ? 'w' : 'd',
                 i->Pin.Div.syned ? "" : "u");
      ppHRegPPC(i->Pin.Div.dst);
      vex_printf(",");
      ppHRegPPC(i->Pin.Div.srcL);
      vex_printf(",");
      ppHRegPPC(i->Pin.Div.srcR);
      return;
   case Pin_Call: {
      Int n;
      vex_printf("call: ");
      if (i->Pin.Call.cond.test != Pct_ALWAYS) {
         vex_printf("if (%s) ", showPPCCondCode(i->Pin.Call.cond));
      }
      vex_printf("{ ");
      ppLoadImm(hregPPC_GPR10(mode64), i->Pin.Call.target, mode64);
      vex_printf(" ; mtctr r10 ; bctrl [");
      for (n = 0; n < 32; n++) {
         if (i->Pin.Call.argiregs & (1<<n)) {
            vex_printf("r%d", n);
            if ((i->Pin.Call.argiregs >> n) > 1)
               vex_printf(",");
         }
      }
      vex_printf("] }");
      break;
   }
   case Pin_Goto:
      vex_printf("goto: ");
      if (i->Pin.Goto.cond.test != Pct_ALWAYS) {
         vex_printf("if (%s) ", showPPCCondCode(i->Pin.Goto.cond));
      }
      vex_printf("{ ");
      if (i->Pin.Goto.jk != Ijk_Boring
          && i->Pin.Goto.jk != Ijk_Call
          && i->Pin.Goto.jk != Ijk_Ret) {
         vex_printf("li %%r31,$");
         ppIRJumpKind(i->Pin.Goto.jk);
         vex_printf(" ; ");
      }
      if (i->Pin.Goto.dst->tag == Pri_Imm) {
         ppLoadImm(hregPPC_GPR3(mode64), i->Pin.Goto.dst->Pri.Imm,
                   mode64);
      } else {
         ppMovReg(hregPPC_GPR3(mode64), i->Pin.Goto.dst->Pri.Reg);
      }
      vex_printf(" ; blr }");
      return;
   case Pin_CMov:
      vex_printf("cmov (%s) ", showPPCCondCode(i->Pin.CMov.cond));
      ppHRegPPC(i->Pin.CMov.dst);
      vex_printf(",");
      ppPPCRI(i->Pin.CMov.src);
      vex_printf(": ");
      if (i->Pin.CMov.cond.test != Pct_ALWAYS) {
         vex_printf("if (%s) ", showPPCCondCode(i->Pin.CMov.cond));
      }
      vex_printf("{ ");
      if (i->Pin.CMov.src->tag == Pri_Imm) {
         ppLoadImm(i->Pin.CMov.dst, i->Pin.CMov.src->Pri.Imm, mode64);
      } else {
         ppMovReg(i->Pin.CMov.dst, i->Pin.CMov.src->Pri.Reg);
      }
      vex_printf(" }");
      return;
   case Pin_Load: {
      Bool idxd = toBool(i->Pin.Load.src->tag == Pam_RR);
      UChar sz = i->Pin.Load.sz;
      UChar c_sz = sz==1 ? 'b' : sz==2 ? 'h' : sz==4 ? 'w' : 'd';
      vex_printf("l%c%s%s ", c_sz, sz==8 ? "" : "z", idxd ? "x" : "" );
      ppHRegPPC(i->Pin.Load.dst);
      vex_printf(",");
      ppPPCAMode(i->Pin.Load.src);
      return;
   }
   case Pin_LoadL:
      vex_printf("l%carx ", i->Pin.LoadL.sz==4 ? 'w' : 'd');
      ppHRegPPC(i->Pin.LoadL.dst);
      vex_printf(",%%r0,");
      ppHRegPPC(i->Pin.LoadL.src);
      return;
   case Pin_Store: {
      UChar sz = i->Pin.Store.sz;
      Bool idxd = toBool(i->Pin.Store.dst->tag == Pam_RR);
      UChar c_sz = sz==1 ? 'b' : sz==2 ? 'h' : sz==4 ? 'w' : /*8*/ 'd';
      vex_printf("st%c%s ", c_sz, idxd ? "x" : "" );
      ppHRegPPC(i->Pin.Store.src);
      vex_printf(",");
      ppPPCAMode(i->Pin.Store.dst);
      return;
   }
   case Pin_StoreC:
      vex_printf("st%ccx. ", i->Pin.StoreC.sz==4 ? 'w' : 'd');
      ppHRegPPC(i->Pin.StoreC.src);
      vex_printf(",%%r0,");
      ppHRegPPC(i->Pin.StoreC.dst);
      return;
   case Pin_Set: {
      PPCCondCode cc = i->Pin.Set.cond;
      vex_printf("set (%s),", showPPCCondCode(cc));
      ppHRegPPC(i->Pin.Set.dst);
      if (cc.test == Pct_ALWAYS) {
         vex_printf(": { li ");
         ppHRegPPC(i->Pin.Set.dst);
         vex_printf(",1 }");
      } else {
         vex_printf(": { mfcr r0 ; rlwinm ");
         ppHRegPPC(i->Pin.Set.dst);
         vex_printf(",r0,%u,31,31", cc.flag+1);
         if (cc.test == Pct_FALSE) {
            vex_printf("; xori ");
            ppHRegPPC(i->Pin.Set.dst);
            vex_printf(",");
            ppHRegPPC(i->Pin.Set.dst);
            vex_printf(",1");
         }
         vex_printf(" }");
      }
      return;
   }
   case Pin_MfCR:
      vex_printf("mfcr ");
      ppHRegPPC(i->Pin.MfCR.dst);
      break;
   case Pin_MFence:
      vex_printf("mfence (=sync)");
      return;

   case Pin_FpUnary:
      vex_printf("%s ", showPPCFpOp(i->Pin.FpUnary.op));
      ppHRegPPC(i->Pin.FpUnary.dst);
      vex_printf(",");
      ppHRegPPC(i->Pin.FpUnary.src);
      return;
   case Pin_FpBinary:
      vex_printf("%s ", showPPCFpOp(i->Pin.FpBinary.op));
      ppHRegPPC(i->Pin.FpBinary.dst);
      vex_printf(",");
      ppHRegPPC(i->Pin.FpBinary.srcL);
      vex_printf(",");
      ppHRegPPC(i->Pin.FpBinary.srcR);
      return;
   case Pin_FpMulAcc:
      vex_printf("%s ", showPPCFpOp(i->Pin.FpMulAcc.op));
      ppHRegPPC(i->Pin.FpMulAcc.dst);
      vex_printf(",");
      ppHRegPPC(i->Pin.FpMulAcc.srcML);
      vex_printf(",");
      ppHRegPPC(i->Pin.FpMulAcc.srcMR);
      vex_printf(",");
      ppHRegPPC(i->Pin.FpMulAcc.srcAcc);
      return;
   case Pin_FpLdSt: {
      UChar sz = i->Pin.FpLdSt.sz;
      Bool idxd = toBool(i->Pin.FpLdSt.addr->tag == Pam_RR);
      if (i->Pin.FpLdSt.isLoad) {
         vex_printf("lf%c%s ",
                    (sz==4 ? 's' : 'd'),
                    idxd ? "x" : "" );
         ppHRegPPC(i->Pin.FpLdSt.reg);
         vex_printf(",");
         ppPPCAMode(i->Pin.FpLdSt.addr);
      } else {
         vex_printf("stf%c%s ",
                    (sz==4 ? 's' : 'd'),
                    idxd ? "x" : "" );
         ppHRegPPC(i->Pin.FpLdSt.reg);
         vex_printf(",");
         ppPPCAMode(i->Pin.FpLdSt.addr);
      }
      return;
   }
   case Pin_FpSTFIW:
      vex_printf("stfiwz ");
      ppHRegPPC(i->Pin.FpSTFIW.data);
      vex_printf(",0(");
      ppHRegPPC(i->Pin.FpSTFIW.addr);
      vex_printf(")");
      return;
   case Pin_FpRSP:
      vex_printf("frsp ");
      ppHRegPPC(i->Pin.FpRSP.dst);
      vex_printf(",");
      ppHRegPPC(i->Pin.FpRSP.src);
      return;
   case Pin_FpCftI: {
      HChar* str = "fc?????";
      if (i->Pin.FpCftI.fromI == False && i->Pin.FpCftI.int32 == False)
         str = "fctid";
      else
      if (i->Pin.FpCftI.fromI == False && i->Pin.FpCftI.int32 == True)
         str = "fctiw";
      else
      if (i->Pin.FpCftI.fromI == True && i->Pin.FpCftI.int32 == False) {
         if (i->Pin.FpCftI.syned == True)
            str = "fcfid";
         else if (i->Pin.FpCftI.flt64 == True)
            str = "fcfidu";
         else
            str = "fcfidus";
      }
      vex_printf("%s ", str);
      ppHRegPPC(i->Pin.FpCftI.dst);
      vex_printf(",");
      ppHRegPPC(i->Pin.FpCftI.src);
      return;
   }
   case Pin_FpCMov:
      vex_printf("fpcmov (%s) ", showPPCCondCode(i->Pin.FpCMov.cond));
      ppHRegPPC(i->Pin.FpCMov.dst);
      vex_printf(",");
      ppHRegPPC(i->Pin.FpCMov.src);
      vex_printf(": ");
      vex_printf("if (fr_dst != fr_src) { ");
      if (i->Pin.FpCMov.cond.test != Pct_ALWAYS) {
         vex_printf("if (%s) { ", showPPCCondCode(i->Pin.FpCMov.cond));
      }
      vex_printf("fmr ");
      ppHRegPPC(i->Pin.FpCMov.dst);
      vex_printf(",");
      ppHRegPPC(i->Pin.FpCMov.src);
      if (i->Pin.FpCMov.cond.test != Pct_ALWAYS)
         vex_printf(" }");
      vex_printf(" }");
      return;
   case Pin_FpLdFPSCR:
      vex_printf("mtfsf 0xFF,");
      ppHRegPPC(i->Pin.FpLdFPSCR.src);
      return;
   case Pin_FpCmp:
      vex_printf("fcmpo %%cr1,");
      ppHRegPPC(i->Pin.FpCmp.srcL);
      vex_printf(",");
      ppHRegPPC(i->Pin.FpCmp.srcR);
      vex_printf("; mfcr ");
      ppHRegPPC(i->Pin.FpCmp.dst);
      vex_printf("; rlwinm ");
      ppHRegPPC(i->Pin.FpCmp.dst);
      vex_printf(",");
      ppHRegPPC(i->Pin.FpCmp.dst);
      vex_printf(",8,28,31");
      return;

   case Pin_RdWrLR:
      vex_printf("%s ", i->Pin.RdWrLR.wrLR ? "mtlr" : "mflr");
      ppHRegPPC(i->Pin.RdWrLR.gpr);
      return;

   case Pin_AvLdSt: {
      UChar  sz = i->Pin.AvLdSt.sz;
      HChar* str_size;
      if (i->Pin.AvLdSt.addr->tag == Pam_IR) {
         ppLoadImm(hregPPC_GPR30(mode64),
                   i->Pin.AvLdSt.addr->Pam.RR.index, mode64);
         vex_printf(" ; ");
      }
      str_size = sz==1 ? "eb" : sz==2 ? "eh" : sz==4 ? "ew" : "";
      if (i->Pin.AvLdSt.isLoad)
         vex_printf("lv%sx ", str_size);
      else
         vex_printf("stv%sx ", str_size);
      ppHRegPPC(i->Pin.AvLdSt.reg);
      vex_printf(",");
      if (i->Pin.AvLdSt.addr->tag == Pam_IR)
         vex_printf("%%r30");
      else 
         ppHRegPPC(i->Pin.AvLdSt.addr->Pam.RR.index);
      vex_printf(",");
      ppHRegPPC(i->Pin.AvLdSt.addr->Pam.RR.base);
      return;
   }
   case Pin_AvUnary:
      vex_printf("%s ", showPPCAvOp(i->Pin.AvUnary.op));
      ppHRegPPC(i->Pin.AvUnary.dst);
      vex_printf(",");
      ppHRegPPC(i->Pin.AvUnary.src);
      return;
   case Pin_AvBinary:
      vex_printf("%s ", showPPCAvOp(i->Pin.AvBinary.op));
      ppHRegPPC(i->Pin.AvBinary.dst);
      vex_printf(",");
      ppHRegPPC(i->Pin.AvBinary.srcL);
      vex_printf(",");
      ppHRegPPC(i->Pin.AvBinary.srcR);
      return;
   case Pin_AvBin8x16:
      vex_printf("%s(b) ", showPPCAvOp(i->Pin.AvBin8x16.op));
      ppHRegPPC(i->Pin.AvBin8x16.dst);
      vex_printf(",");
      ppHRegPPC(i->Pin.AvBin8x16.srcL);
      vex_printf(",");
      ppHRegPPC(i->Pin.AvBin8x16.srcR);
      return;
   case Pin_AvBin16x8:
      vex_printf("%s(h) ", showPPCAvOp(i->Pin.AvBin16x8.op));
      ppHRegPPC(i->Pin.AvBin16x8.dst);
      vex_printf(",");
      ppHRegPPC(i->Pin.AvBin16x8.srcL);
      vex_printf(",");
      ppHRegPPC(i->Pin.AvBin16x8.srcR);
      return;
   case Pin_AvBin32x4:
      vex_printf("%s(w) ", showPPCAvOp(i->Pin.AvBin32x4.op));
      ppHRegPPC(i->Pin.AvBin32x4.dst);
      vex_printf(",");
      ppHRegPPC(i->Pin.AvBin32x4.srcL);
      vex_printf(",");
      ppHRegPPC(i->Pin.AvBin32x4.srcR);
      return;
   case Pin_AvBin32Fx4:
      vex_printf("%s ", showPPCAvFpOp(i->Pin.AvBin32Fx4.op));
      ppHRegPPC(i->Pin.AvBin32Fx4.dst);
      vex_printf(",");
      ppHRegPPC(i->Pin.AvBin32Fx4.srcL);
      vex_printf(",");
      ppHRegPPC(i->Pin.AvBin32Fx4.srcR);
      return;
   case Pin_AvUn32Fx4:
      vex_printf("%s ", showPPCAvFpOp(i->Pin.AvUn32Fx4.op));
      ppHRegPPC(i->Pin.AvUn32Fx4.dst);
      vex_printf(",");
      ppHRegPPC(i->Pin.AvUn32Fx4.src);
      return;
   case Pin_AvPerm:
      vex_printf("vperm ");
      ppHRegPPC(i->Pin.AvPerm.dst);
      vex_printf(",");
      ppHRegPPC(i->Pin.AvPerm.srcL);
      vex_printf(",");
      ppHRegPPC(i->Pin.AvPerm.srcR);
      vex_printf(",");
      ppHRegPPC(i->Pin.AvPerm.ctl);
      return;

   case Pin_AvSel:
      vex_printf("vsel ");
      ppHRegPPC(i->Pin.AvSel.dst);
      vex_printf(",");
      ppHRegPPC(i->Pin.AvSel.srcL);
      vex_printf(",");
      ppHRegPPC(i->Pin.AvSel.srcR);
      vex_printf(",");
      ppHRegPPC(i->Pin.AvSel.ctl);
      return;

   case Pin_AvShlDbl:
      vex_printf("vsldoi ");
      ppHRegPPC(i->Pin.AvShlDbl.dst);
      vex_printf(",");
      ppHRegPPC(i->Pin.AvShlDbl.srcL);
      vex_printf(",");
      ppHRegPPC(i->Pin.AvShlDbl.srcR);
      vex_printf(",%d", i->Pin.AvShlDbl.shift);
      return;

   case Pin_AvSplat: {
      UChar sz = i->Pin.AvSplat.sz;
      UChar ch_sz = toUChar( (sz == 8) ? 'b' : (sz == 16) ? 'h' : 'w' );
      vex_printf("vsplt%s%c ",
                 i->Pin.AvSplat.src->tag == Pvi_Imm ? "is" : "", ch_sz);
      ppHRegPPC(i->Pin.AvSplat.dst);
      vex_printf(",");
      ppPPCVI5s(i->Pin.AvSplat.src);
      if (i->Pin.AvSplat.src->tag == Pvi_Reg)
         vex_printf(", %d", (128/sz)-1);   /* louis lane */
      return;
   }

   case Pin_AvCMov:
      vex_printf("avcmov (%s) ", showPPCCondCode(i->Pin.AvCMov.cond));
      ppHRegPPC(i->Pin.AvCMov.dst);
      vex_printf(",");
      ppHRegPPC(i->Pin.AvCMov.src);
      vex_printf(": ");
      vex_printf("if (v_dst != v_src) { ");
      if (i->Pin.AvCMov.cond.test != Pct_ALWAYS) {
         vex_printf("if (%s) { ", showPPCCondCode(i->Pin.AvCMov.cond));
      }
      vex_printf("vmr ");
      ppHRegPPC(i->Pin.AvCMov.dst);
      vex_printf(",");
      ppHRegPPC(i->Pin.AvCMov.src);
      if (i->Pin.FpCMov.cond.test != Pct_ALWAYS)
         vex_printf(" }");
      vex_printf(" }");
      return;

   case Pin_AvLdVSCR:
      vex_printf("mtvscr ");
      ppHRegPPC(i->Pin.AvLdVSCR.src);
      return;

   default:
      vex_printf("\nppPPCInstr: No such tag(%d)\n", (Int)i->tag);
      vpanic("ppPPCInstr");
   }
}

/* --------- Helpers for register allocation. --------- */

void getRegUsage_PPCInstr ( HRegUsage* u, PPCInstr* i, Bool mode64 )
{
   initHRegUsage(u);
   switch (i->tag) {
   case Pin_LI:
      addHRegUse(u, HRmWrite, i->Pin.LI.dst);
      break;
   case Pin_Alu:
      addHRegUse(u, HRmRead,  i->Pin.Alu.srcL);
      addRegUsage_PPCRH(u,    i->Pin.Alu.srcR);
      addHRegUse(u, HRmWrite, i->Pin.Alu.dst);
      return;
   case Pin_Shft:
      addHRegUse(u, HRmRead,  i->Pin.Shft.srcL);
      addRegUsage_PPCRH(u,    i->Pin.Shft.srcR);
      addHRegUse(u, HRmWrite, i->Pin.Shft.dst);
      return;
   case Pin_AddSubC:
      addHRegUse(u, HRmWrite, i->Pin.AddSubC.dst);
      addHRegUse(u, HRmRead,  i->Pin.AddSubC.srcL);
      addHRegUse(u, HRmRead,  i->Pin.AddSubC.srcR);
      return;
   case Pin_Cmp:
      addHRegUse(u, HRmRead, i->Pin.Cmp.srcL);
      addRegUsage_PPCRH(u,   i->Pin.Cmp.srcR);
      return;
   case Pin_Unary:
      addHRegUse(u, HRmWrite, i->Pin.Unary.dst);
      addHRegUse(u, HRmRead,  i->Pin.Unary.src);
      return;
   case Pin_MulL:
      addHRegUse(u, HRmWrite, i->Pin.MulL.dst);
      addHRegUse(u, HRmRead,  i->Pin.MulL.srcL);
      addHRegUse(u, HRmRead,  i->Pin.MulL.srcR);
      return;
   case Pin_Div:
      addHRegUse(u, HRmWrite, i->Pin.Div.dst);
      addHRegUse(u, HRmRead,  i->Pin.Div.srcL);
      addHRegUse(u, HRmRead,  i->Pin.Div.srcR);
      return;
   case Pin_Call: {
      UInt argir;
      /* This is a bit subtle. */
      /* First off, claim it trashes all the caller-saved regs
         which fall within the register allocator's jurisdiction.
         These I believe to be:
         mode32: r3 to r12
         mode64: r3 to r10
      */
      /* XXXXXXXXXXXXXXXXX BUG! This doesn't say anything about the FP
         or Altivec registers.  We get away with this ONLY because
         getAllocatableRegs_PPC gives the allocator callee-saved fp
         and Altivec regs, and no caller-save ones. */
      addHRegUse(u, HRmWrite, hregPPC_GPR3(mode64));
      addHRegUse(u, HRmWrite, hregPPC_GPR4(mode64));
      addHRegUse(u, HRmWrite, hregPPC_GPR5(mode64));
      addHRegUse(u, HRmWrite, hregPPC_GPR6(mode64));
      addHRegUse(u, HRmWrite, hregPPC_GPR7(mode64));
      addHRegUse(u, HRmWrite, hregPPC_GPR8(mode64));
      addHRegUse(u, HRmWrite, hregPPC_GPR9(mode64));
      addHRegUse(u, HRmWrite, hregPPC_GPR10(mode64));
      if (!mode64) {
         addHRegUse(u, HRmWrite, hregPPC_GPR11(mode64));
         addHRegUse(u, HRmWrite, hregPPC_GPR12(mode64));
      }

      /* Now we have to state any parameter-carrying registers
         which might be read.  This depends on the argiregs field. */
      argir = i->Pin.Call.argiregs;
      if (argir &(1<<10)) addHRegUse(u, HRmRead, hregPPC_GPR10(mode64));
      if (argir & (1<<9)) addHRegUse(u, HRmRead, hregPPC_GPR9(mode64));
      if (argir & (1<<8)) addHRegUse(u, HRmRead, hregPPC_GPR8(mode64));
      if (argir & (1<<7)) addHRegUse(u, HRmRead, hregPPC_GPR7(mode64));
      if (argir & (1<<6)) addHRegUse(u, HRmRead, hregPPC_GPR6(mode64));
      if (argir & (1<<5)) addHRegUse(u, HRmRead, hregPPC_GPR5(mode64));
      if (argir & (1<<4)) addHRegUse(u, HRmRead, hregPPC_GPR4(mode64));
      if (argir & (1<<3)) addHRegUse(u, HRmRead, hregPPC_GPR3(mode64));

      vassert(0 == (argir & ~((1<<3)|(1<<4)|(1<<5)|(1<<6)
                              |(1<<7)|(1<<8)|(1<<9)|(1<<10))));

      /* Finally, there is the issue that the insn trashes a
         register because the literal target address has to be
         loaded into a register.  %r10 seems a suitable victim.
         (Can't use %r0, as some insns interpret it as value zero). */
      addHRegUse(u, HRmWrite, hregPPC_GPR10(mode64));
      /* Upshot of this is that the assembler really must use %r10,
         and no other, as a destination temporary. */
      return;
   }
   case Pin_Goto:
      addRegUsage_PPCRI(u, i->Pin.Goto.dst);
      /* GPR3 holds destination address from Pin_Goto */
      addHRegUse(u, HRmWrite, hregPPC_GPR3(mode64));
      if (i->Pin.Goto.jk != Ijk_Boring
          && i->Pin.Goto.jk != Ijk_Call
          && i->Pin.Goto.jk != Ijk_Ret)
            /* note, this is irrelevant since the guest state pointer
               register is not actually available to the allocator.
               But still .. */
         addHRegUse(u, HRmWrite, GuestStatePtr(mode64));
      return;
   case Pin_CMov:
      addRegUsage_PPCRI(u,  i->Pin.CMov.src);
      addHRegUse(u, HRmWrite, i->Pin.CMov.dst);
      return;
   case Pin_Load:
      addRegUsage_PPCAMode(u, i->Pin.Load.src);
      addHRegUse(u, HRmWrite, i->Pin.Load.dst);
      return;
   case Pin_LoadL:
      addHRegUse(u, HRmRead,  i->Pin.LoadL.src);
      addHRegUse(u, HRmWrite, i->Pin.LoadL.dst);
      return;
   case Pin_Store:
      addHRegUse(u, HRmRead,  i->Pin.Store.src);
      addRegUsage_PPCAMode(u, i->Pin.Store.dst);
      return;
   case Pin_StoreC:
      addHRegUse(u, HRmRead, i->Pin.StoreC.src);
      addHRegUse(u, HRmRead, i->Pin.StoreC.dst);
      return;
   case Pin_Set:
      addHRegUse(u, HRmWrite, i->Pin.Set.dst);
      return;
   case Pin_MfCR:
      addHRegUse(u, HRmWrite, i->Pin.MfCR.dst);
      return;
   case Pin_MFence:
      return;

   case Pin_FpUnary:
      addHRegUse(u, HRmWrite, i->Pin.FpUnary.dst);
      addHRegUse(u, HRmRead,  i->Pin.FpUnary.src);
      return;
   case Pin_FpBinary:
      addHRegUse(u, HRmWrite, i->Pin.FpBinary.dst);
      addHRegUse(u, HRmRead,  i->Pin.FpBinary.srcL);
      addHRegUse(u, HRmRead,  i->Pin.FpBinary.srcR);
      return;
   case Pin_FpMulAcc:
      addHRegUse(u, HRmWrite, i->Pin.FpMulAcc.dst);
      addHRegUse(u, HRmRead,  i->Pin.FpMulAcc.srcML);
      addHRegUse(u, HRmRead,  i->Pin.FpMulAcc.srcMR);
      addHRegUse(u, HRmRead,  i->Pin.FpMulAcc.srcAcc);
      return;
   case Pin_FpLdSt:
      addHRegUse(u, (i->Pin.FpLdSt.isLoad ? HRmWrite : HRmRead),
                 i->Pin.FpLdSt.reg);
      addRegUsage_PPCAMode(u, i->Pin.FpLdSt.addr);
      return;
   case Pin_FpSTFIW:
      addHRegUse(u, HRmRead, i->Pin.FpSTFIW.addr);
      addHRegUse(u, HRmRead, i->Pin.FpSTFIW.data);
      return;
   case Pin_FpRSP:
      addHRegUse(u, HRmWrite, i->Pin.FpRSP.dst);
      addHRegUse(u, HRmRead,  i->Pin.FpRSP.src);
      return;
   case Pin_FpCftI:
      addHRegUse(u, HRmWrite, i->Pin.FpCftI.dst);
      addHRegUse(u, HRmRead,  i->Pin.FpCftI.src);
      return;
   case Pin_FpCMov:
      addHRegUse(u, HRmModify, i->Pin.FpCMov.dst);
      addHRegUse(u, HRmRead,   i->Pin.FpCMov.src);
      return;
   case Pin_FpLdFPSCR:
      addHRegUse(u, HRmRead, i->Pin.FpLdFPSCR.src);
      return;
   case Pin_FpCmp:
      addHRegUse(u, HRmWrite, i->Pin.FpCmp.dst);
      addHRegUse(u, HRmRead,  i->Pin.FpCmp.srcL);
      addHRegUse(u, HRmRead,  i->Pin.FpCmp.srcR);
      return;

   case Pin_RdWrLR:
      addHRegUse(u, (i->Pin.RdWrLR.wrLR ? HRmRead : HRmWrite),
                 i->Pin.RdWrLR.gpr);
      return;

   case Pin_AvLdSt:
      addHRegUse(u, (i->Pin.AvLdSt.isLoad ? HRmWrite : HRmRead),
                 i->Pin.AvLdSt.reg);
      if (i->Pin.AvLdSt.addr->tag == Pam_IR)
         addHRegUse(u, HRmWrite, hregPPC_GPR30(mode64));
      addRegUsage_PPCAMode(u, i->Pin.AvLdSt.addr);
      return;
   case Pin_AvUnary:
      addHRegUse(u, HRmWrite, i->Pin.AvUnary.dst);
      addHRegUse(u, HRmRead,  i->Pin.AvUnary.src);
      return;
   case Pin_AvBinary:
      if (i->Pin.AvBinary.op == Pav_XOR
          && i->Pin.AvBinary.dst == i->Pin.AvBinary.srcL
          && i->Pin.AvBinary.dst == i->Pin.AvBinary.srcR) {
         /* reg-alloc needs to understand 'xor r,r,r' as a write of r */
         /* (as opposed to a rite of passage :-) */
         addHRegUse(u, HRmWrite, i->Pin.AvBinary.dst);
      } else {
         addHRegUse(u, HRmWrite, i->Pin.AvBinary.dst);
         addHRegUse(u, HRmRead,  i->Pin.AvBinary.srcL);
         addHRegUse(u, HRmRead,  i->Pin.AvBinary.srcR);
      }
      return;
   case Pin_AvBin8x16:
      addHRegUse(u, HRmWrite, i->Pin.AvBin8x16.dst);
      addHRegUse(u, HRmRead,  i->Pin.AvBin8x16.srcL);
      addHRegUse(u, HRmRead,  i->Pin.AvBin8x16.srcR);
      return;
   case Pin_AvBin16x8:
      addHRegUse(u, HRmWrite, i->Pin.AvBin16x8.dst);
      addHRegUse(u, HRmRead,  i->Pin.AvBin16x8.srcL);
      addHRegUse(u, HRmRead,  i->Pin.AvBin16x8.srcR);
      return;
   case Pin_AvBin32x4:
      addHRegUse(u, HRmWrite, i->Pin.AvBin32x4.dst);
      addHRegUse(u, HRmRead,  i->Pin.AvBin32x4.srcL);
      addHRegUse(u, HRmRead,  i->Pin.AvBin32x4.srcR);
      return;
   case Pin_AvBin32Fx4:
      addHRegUse(u, HRmWrite, i->Pin.AvBin32Fx4.dst);
      addHRegUse(u, HRmRead,  i->Pin.AvBin32Fx4.srcL);
      addHRegUse(u, HRmRead,  i->Pin.AvBin32Fx4.srcR);
      if (i->Pin.AvBin32Fx4.op == Pavfp_MULF)
         addHRegUse(u, HRmWrite, hregPPC_VR29());
      return;
   case Pin_AvUn32Fx4:
      addHRegUse(u, HRmWrite, i->Pin.AvUn32Fx4.dst);
      addHRegUse(u, HRmRead,  i->Pin.AvUn32Fx4.src);
      return;
   case Pin_AvPerm:
      addHRegUse(u, HRmWrite, i->Pin.AvPerm.dst);
      addHRegUse(u, HRmRead,  i->Pin.AvPerm.srcL);
      addHRegUse(u, HRmRead,  i->Pin.AvPerm.srcR);
      addHRegUse(u, HRmRead,  i->Pin.AvPerm.ctl);
      return;
   case Pin_AvSel:
      addHRegUse(u, HRmWrite, i->Pin.AvSel.dst);
      addHRegUse(u, HRmRead,  i->Pin.AvSel.ctl);
      addHRegUse(u, HRmRead,  i->Pin.AvSel.srcL);
      addHRegUse(u, HRmRead,  i->Pin.AvSel.srcR);
      return;
   case Pin_AvShlDbl:
      addHRegUse(u, HRmWrite, i->Pin.AvShlDbl.dst);
      addHRegUse(u, HRmRead,  i->Pin.AvShlDbl.srcL);
      addHRegUse(u, HRmRead,  i->Pin.AvShlDbl.srcR);
      return;
   case Pin_AvSplat:
      addHRegUse(u, HRmWrite, i->Pin.AvSplat.dst);
      addRegUsage_PPCVI5s(u,  i->Pin.AvSplat.src);
      return;
   case Pin_AvCMov:
      addHRegUse(u, HRmModify, i->Pin.AvCMov.dst);
      addHRegUse(u, HRmRead,   i->Pin.AvCMov.src);
      return;
   case Pin_AvLdVSCR:
      addHRegUse(u, HRmRead, i->Pin.AvLdVSCR.src);
      return;

   default:
      ppPPCInstr(i, mode64);
      vpanic("getRegUsage_PPCInstr");
   }
}

/* local helper */
static void mapReg( HRegRemap* m, HReg* r )
{
   *r = lookupHRegRemap(m, *r);
}

void mapRegs_PPCInstr ( HRegRemap* m, PPCInstr* i, Bool mode64 )
{
   switch (i->tag) {
   case Pin_LI:
      mapReg(m, &i->Pin.LI.dst);
      return;
   case Pin_Alu:
      mapReg(m, &i->Pin.Alu.dst);
      mapReg(m, &i->Pin.Alu.srcL);
      mapRegs_PPCRH(m, i->Pin.Alu.srcR);
      return;
   case Pin_Shft:
      mapReg(m, &i->Pin.Shft.dst);
      mapReg(m, &i->Pin.Shft.srcL);
      mapRegs_PPCRH(m, i->Pin.Shft.srcR);
      return;
   case Pin_AddSubC:
      mapReg(m, &i->Pin.AddSubC.dst);
      mapReg(m, &i->Pin.AddSubC.srcL);
      mapReg(m, &i->Pin.AddSubC.srcR);
      return;
   case Pin_Cmp:
      mapReg(m, &i->Pin.Cmp.srcL);
      mapRegs_PPCRH(m, i->Pin.Cmp.srcR);
      return;
   case Pin_Unary:
      mapReg(m, &i->Pin.Unary.dst);
      mapReg(m, &i->Pin.Unary.src);
      return;
   case Pin_MulL:
      mapReg(m, &i->Pin.MulL.dst);
      mapReg(m, &i->Pin.MulL.srcL);
      mapReg(m, &i->Pin.MulL.srcR);
      return;
   case Pin_Div:
      mapReg(m, &i->Pin.Div.dst);
      mapReg(m, &i->Pin.Div.srcL);
      mapReg(m, &i->Pin.Div.srcR);
      return;
   case Pin_Call:
      return;
   case Pin_Goto:
      mapRegs_PPCRI(m, i->Pin.Goto.dst);
      return;
   case Pin_CMov:
      mapRegs_PPCRI(m, i->Pin.CMov.src);
      mapReg(m, &i->Pin.CMov.dst);
      return;
   case Pin_Load:
      mapRegs_PPCAMode(m, i->Pin.Load.src);
      mapReg(m, &i->Pin.Load.dst);
      return;
   case Pin_LoadL:
      mapReg(m, &i->Pin.LoadL.src);
      mapReg(m, &i->Pin.LoadL.dst);
      return;
   case Pin_Store:
      mapReg(m, &i->Pin.Store.src);
      mapRegs_PPCAMode(m, i->Pin.Store.dst);
      return;
   case Pin_StoreC:
      mapReg(m, &i->Pin.StoreC.src);
      mapReg(m, &i->Pin.StoreC.dst);
      return;
   case Pin_Set:
      mapReg(m, &i->Pin.Set.dst);
      return;
   case Pin_MfCR:
      mapReg(m, &i->Pin.MfCR.dst);
      return;
   case Pin_MFence:
      return;
   case Pin_FpUnary:
      mapReg(m, &i->Pin.FpUnary.dst);
      mapReg(m, &i->Pin.FpUnary.src);
      return;
   case Pin_FpBinary:
      mapReg(m, &i->Pin.FpBinary.dst);
      mapReg(m, &i->Pin.FpBinary.srcL);
      mapReg(m, &i->Pin.FpBinary.srcR);
      return;
   case Pin_FpMulAcc:
      mapReg(m, &i->Pin.FpMulAcc.dst);
      mapReg(m, &i->Pin.FpMulAcc.srcML);
      mapReg(m, &i->Pin.FpMulAcc.srcMR);
      mapReg(m, &i->Pin.FpMulAcc.srcAcc);
      return;
   case Pin_FpLdSt:
      mapReg(m, &i->Pin.FpLdSt.reg);
      mapRegs_PPCAMode(m, i->Pin.FpLdSt.addr);
      return;
   case Pin_FpSTFIW:
      mapReg(m, &i->Pin.FpSTFIW.addr);
      mapReg(m, &i->Pin.FpSTFIW.data);
      return;
   case Pin_FpRSP:
      mapReg(m, &i->Pin.FpRSP.dst);
      mapReg(m, &i->Pin.FpRSP.src);
      return;
   case Pin_FpCftI:
      mapReg(m, &i->Pin.FpCftI.dst);
      mapReg(m, &i->Pin.FpCftI.src);
      return;
   case Pin_FpCMov:
      mapReg(m, &i->Pin.FpCMov.dst);
      mapReg(m, &i->Pin.FpCMov.src);
      return;
   case Pin_FpLdFPSCR:
      mapReg(m, &i->Pin.FpLdFPSCR.src);
      return;
   case Pin_FpCmp:
      mapReg(m, &i->Pin.FpCmp.dst);
      mapReg(m, &i->Pin.FpCmp.srcL);
      mapReg(m, &i->Pin.FpCmp.srcR);
      return;
   case Pin_RdWrLR:
      mapReg(m, &i->Pin.RdWrLR.gpr);
      return;
   case Pin_AvLdSt:
      mapReg(m, &i->Pin.AvLdSt.reg);
      mapRegs_PPCAMode(m, i->Pin.AvLdSt.addr);
      return;
   case Pin_AvUnary:
      mapReg(m, &i->Pin.AvUnary.dst);
      mapReg(m, &i->Pin.AvUnary.src);
      return;
   case Pin_AvBinary:
      mapReg(m, &i->Pin.AvBinary.dst);
      mapReg(m, &i->Pin.AvBinary.srcL);
      mapReg(m, &i->Pin.AvBinary.srcR);
      return;
   case Pin_AvBin8x16:
      mapReg(m, &i->Pin.AvBin8x16.dst);
      mapReg(m, &i->Pin.AvBin8x16.srcL);
      mapReg(m, &i->Pin.AvBin8x16.srcR);
      return;
   case Pin_AvBin16x8:
      mapReg(m, &i->Pin.AvBin16x8.dst);
      mapReg(m, &i->Pin.AvBin16x8.srcL);
      mapReg(m, &i->Pin.AvBin16x8.srcR);
      return;
   case Pin_AvBin32x4:
      mapReg(m, &i->Pin.AvBin32x4.dst);
      mapReg(m, &i->Pin.AvBin32x4.srcL);
      mapReg(m, &i->Pin.AvBin32x4.srcR);
      return;
   case Pin_AvBin32Fx4:
      mapReg(m, &i->Pin.AvBin32Fx4.dst);
      mapReg(m, &i->Pin.AvBin32Fx4.srcL);
      mapReg(m, &i->Pin.AvBin32Fx4.srcR);
      return;
   case Pin_AvUn32Fx4:
      mapReg(m, &i->Pin.AvUn32Fx4.dst);
      mapReg(m, &i->Pin.AvUn32Fx4.src);
      return;
   case Pin_AvPerm:
      mapReg(m, &i->Pin.AvPerm.dst);
      mapReg(m, &i->Pin.AvPerm.srcL);
      mapReg(m, &i->Pin.AvPerm.srcR);
      mapReg(m, &i->Pin.AvPerm.ctl);
      return;
   case Pin_AvSel:
      mapReg(m, &i->Pin.AvSel.dst);
      mapReg(m, &i->Pin.AvSel.srcL);
      mapReg(m, &i->Pin.AvSel.srcR);
      mapReg(m, &i->Pin.AvSel.ctl);
      return;
   case Pin_AvShlDbl:
      mapReg(m, &i->Pin.AvShlDbl.dst);
      mapReg(m, &i->Pin.AvShlDbl.srcL);
      mapReg(m, &i->Pin.AvShlDbl.srcR);
      return;
   case Pin_AvSplat:
      mapReg(m, &i->Pin.AvSplat.dst);
      mapRegs_PPCVI5s(m, i->Pin.AvSplat.src);
      return;
   case Pin_AvCMov:
     mapReg(m, &i->Pin.AvCMov.dst);
     mapReg(m, &i->Pin.AvCMov.src);
     return;
   case Pin_AvLdVSCR:
      mapReg(m, &i->Pin.AvLdVSCR.src);
      return;

   default:
      ppPPCInstr(i, mode64);
      vpanic("mapRegs_PPCInstr");
   }
}

/* Figure out if i represents a reg-reg move, and if so assign the
   source and destination to *src and *dst.  If in doubt say No.  Used
   by the register allocator to do move coalescing. 
*/
Bool isMove_PPCInstr ( PPCInstr* i, HReg* src, HReg* dst )
{
   /* Moves between integer regs */
   if (i->tag == Pin_Alu) {
      // or Rd,Rs,Rs == mr Rd,Rs
      if (i->Pin.Alu.op != Palu_OR)
         return False;
      if (i->Pin.Alu.srcR->tag != Prh_Reg)
         return False;
      if (i->Pin.Alu.srcR->Prh.Reg.reg != i->Pin.Alu.srcL)
         return False;
      *src = i->Pin.Alu.srcL;
      *dst = i->Pin.Alu.dst;
      return True;
   }
   /* Moves between FP regs */
   if (i->tag == Pin_FpUnary) {
      if (i->Pin.FpUnary.op != Pfp_MOV)
         return False;
      *src = i->Pin.FpUnary.src;
      *dst = i->Pin.FpUnary.dst;
      return True;
   }
   return False;
}


/* Generate ppc spill/reload instructions under the direction of the
   register allocator.  Note it's critical these don't write the
   condition codes. */

void genSpill_PPC ( /*OUT*/HInstr** i1, /*OUT*/HInstr** i2,
                    HReg rreg, Int offsetB, Bool mode64 )
{
   PPCAMode* am;
   vassert(!hregIsVirtual(rreg));
   *i1 = *i2 = NULL;
   am = PPCAMode_IR( offsetB, GuestStatePtr(mode64) );
   switch (hregClass(rreg)) {
      case HRcInt64:
         vassert(mode64);
         *i1 = PPCInstr_Store( 8, am, rreg, mode64 );
         return;
      case HRcInt32:
         vassert(!mode64);
         *i1 = PPCInstr_Store( 4, am, rreg, mode64 );
         return;
      case HRcFlt64:
         *i1 = PPCInstr_FpLdSt ( False/*store*/, 8, rreg, am );
         return;
      case HRcVec128:
         // XXX: GPR30 used as spill register to kludge AltiVec
         // AMode_IR
         *i1 = PPCInstr_AvLdSt ( False/*store*/, 16, rreg, am );
         return;
      default: 
         ppHRegClass(hregClass(rreg));
         vpanic("genSpill_PPC: unimplemented regclass");
   }
}

void genReload_PPC ( /*OUT*/HInstr** i1, /*OUT*/HInstr** i2,
                     HReg rreg, Int offsetB, Bool mode64 )
{
   PPCAMode* am;
   vassert(!hregIsVirtual(rreg));
   *i1 = *i2 = NULL;
   am = PPCAMode_IR( offsetB, GuestStatePtr(mode64) );
   switch (hregClass(rreg)) {
      case HRcInt64:
         vassert(mode64);
         *i1 = PPCInstr_Load( 8, rreg, am, mode64 );
         return;
      case HRcInt32:
         vassert(!mode64);
         *i1 = PPCInstr_Load( 4, rreg, am, mode64 );
         return;
      case HRcFlt64:
         *i1 = PPCInstr_FpLdSt ( True/*load*/, 8, rreg, am );
         return;
      case HRcVec128:
         // XXX: GPR30 used as spill register to kludge AltiVec AMode_IR
         *i1 = PPCInstr_AvLdSt ( True/*load*/, 16, rreg, am );
         return;
      default: 
         ppHRegClass(hregClass(rreg));
         vpanic("genReload_PPC: unimplemented regclass");
   }
}


/* --------- The ppc assembler (bleh.) --------- */

static UInt iregNo ( HReg r, Bool mode64 )
{
   UInt n;
   vassert(hregClass(r) == mode64 ? HRcInt64 : HRcInt32);
   vassert(!hregIsVirtual(r));
   n = hregNumber(r);
   vassert(n <= 32);
   return n;
}

static UInt fregNo ( HReg fr )
{
   UInt n;
   vassert(hregClass(fr) == HRcFlt64);
   vassert(!hregIsVirtual(fr));
   n = hregNumber(fr);
   vassert(n <= 32);
   return n;
}

static UInt vregNo ( HReg v )
{
   UInt n;
   vassert(hregClass(v) == HRcVec128);
   vassert(!hregIsVirtual(v));
   n = hregNumber(v);
   vassert(n <= 32);
   return n;
}

/* Emit 32bit instruction big-endianly */
static UChar* emit32 ( UChar* p, UInt w32 )
{
   *p++ = toUChar((w32 >> 24) & 0x000000FF);
   *p++ = toUChar((w32 >> 16) & 0x000000FF);
   *p++ = toUChar((w32 >>  8) & 0x000000FF);
   *p++ = toUChar((w32)       & 0x000000FF);
   return p;
}

/* The following mkForm[...] functions refer to ppc instruction forms
   as per PPC32 p576
 */

static UChar* mkFormD ( UChar* p, UInt opc1,
                        UInt r1, UInt r2, UInt imm )
{
   UInt theInstr;
   vassert(opc1 < 0x40);
   vassert(r1   < 0x20);
   vassert(r2   < 0x20);
   imm = imm & 0xFFFF;
   theInstr = ((opc1<<26) | (r1<<21) | (r2<<16) | (imm));
   return emit32(p, theInstr);
}

static UChar* mkFormMD ( UChar* p, UInt opc1, UInt r1, UInt r2,
                         UInt imm1, UInt imm2, UInt opc2 )
{
   UInt theInstr;
   vassert(opc1 < 0x40);
   vassert(r1   < 0x20);
   vassert(r2   < 0x20);
   vassert(imm1 < 0x40);
   vassert(imm2 < 0x40);
   vassert(opc2 < 0x08);
   imm2 = ((imm2 & 0x1F) << 1) | (imm2 >> 5);
   theInstr = ((opc1<<26) | (r1<<21) | (r2<<16) |
               ((imm1 & 0x1F)<<11) | (imm2<<5) |
               (opc2<<2) | ((imm1 >> 5)<<1));
   return emit32(p, theInstr);
}

static UChar* mkFormX ( UChar* p, UInt opc1, UInt r1, UInt r2,
                        UInt r3, UInt opc2, UInt b0 )
{
   UInt theInstr;
   vassert(opc1 < 0x40);
   vassert(r1   < 0x20);
   vassert(r2   < 0x20);
   vassert(r3   < 0x20);
   vassert(opc2 < 0x400);
   vassert(b0   < 0x2);
   theInstr = ((opc1<<26) | (r1<<21) | (r2<<16) |
               (r3<<11) | (opc2<<1) | (b0));
   return emit32(p, theInstr);
}

static UChar* mkFormXO ( UChar* p, UInt opc1, UInt r1, UInt r2,
                         UInt r3, UInt b10, UInt opc2, UInt b0 )
{
   UInt theInstr;
   vassert(opc1 < 0x40);
   vassert(r1   < 0x20);
   vassert(r2   < 0x20);
   vassert(r3   < 0x20);
   vassert(b10  < 0x2);
   vassert(opc2 < 0x200);
   vassert(b0   < 0x2);
   theInstr = ((opc1<<26) | (r1<<21) | (r2<<16) |
               (r3<<11) | (b10 << 10) | (opc2<<1) | (b0));
   return emit32(p, theInstr);
}

static UChar* mkFormXL ( UChar* p, UInt opc1, UInt f1, UInt f2,
                         UInt f3, UInt opc2, UInt b0 )
{
   UInt theInstr;
   vassert(opc1 < 0x40);
   vassert(f1   < 0x20);
   vassert(f2   < 0x20);
   vassert(f3   < 0x20);
   vassert(opc2 < 0x400);
   vassert(b0   < 0x2);
   theInstr = ((opc1<<26) | (f1<<21) | (f2<<16) |
               (f3<<11) | (opc2<<1) | (b0));
   return emit32(p, theInstr);
}

// Note: for split field ops, give mnemonic arg
static UChar* mkFormXFX ( UChar* p, UInt r1, UInt f2, UInt opc2 )
{
   UInt theInstr;
   vassert(r1   < 0x20);
   vassert(f2   < 0x20);
   vassert(opc2 < 0x400);
   switch (opc2) {
   case 144:  // mtcrf
      vassert(f2 < 0x100);
      f2 = f2 << 1;
      break;
   case 339:  // mfspr
   case 371:  // mftb
   case 467:  // mtspr
      vassert(f2 < 0x400);
      // re-arrange split field
      f2 = ((f2>>5) & 0x1F) | ((f2 & 0x1F)<<5);
      break;
   default: vpanic("mkFormXFX(ppch)");
   }
   theInstr = ((31<<26) | (r1<<21) | (f2<<11) | (opc2<<1));
   return emit32(p, theInstr);
}

// Only used by mtfsf
static UChar* mkFormXFL ( UChar* p, UInt FM, UInt freg )
{
   UInt theInstr;
   vassert(FM   < 0x100);
   vassert(freg < 0x20);
   theInstr = ((63<<26) | (FM<<17) | (freg<<11) | (711<<1));
   return emit32(p, theInstr);
}

static UChar* mkFormXS ( UChar* p, UInt opc1, UInt r1, UInt r2,
                         UInt imm, UInt opc2, UInt b0 )
{
   UInt theInstr;
   vassert(opc1 < 0x40);
   vassert(r1   < 0x20);
   vassert(r2   < 0x20);
   vassert(imm  < 0x40);
   vassert(opc2 < 0x400);
   vassert(b0   < 0x2);
   theInstr = ((opc1<<26) | (r1<<21) | (r2<<16) |
               ((imm & 0x1F)<<11) | (opc2<<2) | ((imm>>5)<<1) | (b0));
   return emit32(p, theInstr);
}


#if 0
// 'b'
static UChar* mkFormI ( UChar* p, UInt LI, UInt AA, UInt LK )
{
   UInt theInstr;
   vassert(LI  < 0x1000000);
   vassert(AA  < 0x2);
   vassert(LK  < 0x2);
   theInstr = ((18<<26) | (LI<<2) | (AA<<1) | (LK));
   return emit32(p, theInstr);
}
#endif

// 'bc'
static UChar* mkFormB ( UChar* p, UInt BO, UInt BI,
                        UInt BD, UInt AA, UInt LK )
{
   UInt theInstr;
   vassert(BO  < 0x20);
   vassert(BI  < 0x20);
   vassert(BD  < 0x4000);
   vassert(AA  < 0x2);
   vassert(LK  < 0x2);
   theInstr = ((16<<26) | (BO<<21) | (BI<<16) |
               (BD<<2) | (AA<<1) | (LK));
   return emit32(p, theInstr);
}

// rotates
static UChar* mkFormM ( UChar* p, UInt opc1, UInt r1, UInt r2,
                        UInt f3, UInt MB, UInt ME, UInt Rc )
{
   UInt theInstr;
   vassert(opc1 < 0x40);
   vassert(r1   < 0x20);
   vassert(r2   < 0x20);
   vassert(f3   < 0x20);
   vassert(MB   < 0x20);
   vassert(ME   < 0x20);
   vassert(Rc   < 0x2);
   theInstr = ((opc1<<26) | (r1<<21) | (r2<<16) |
               (f3<<11) | (MB<<6) | (ME<<1) | (Rc));
   return emit32(p, theInstr);
}

static UChar* mkFormA ( UChar* p, UInt opc1, UInt r1, UInt r2,
                        UInt r3, UInt r4, UInt opc2, UInt b0 )
{
   UInt theInstr;
   vassert(opc1 < 0x40);
   vassert(r1   < 0x20);
   vassert(r2   < 0x20);
   vassert(r3   < 0x20);
   vassert(r4   < 0x20);
   vassert(opc2 < 0x20);
   vassert(b0   < 0x2 );
   theInstr = ((opc1<<26) | (r1<<21) | (r2<<16) | (r3<<11) |
               (r4<<6) | (opc2<<1) | (b0));
   return emit32(p, theInstr);
}

static UChar* doAMode_IR ( UChar* p, UInt opc1, UInt rSD,
                           PPCAMode* am, Bool mode64 )
{
   UInt rA, idx;
   vassert(am->tag == Pam_IR);
   vassert(am->Pam.IR.index < 0x10000);

   rA  = iregNo(am->Pam.IR.base, mode64);
   idx = am->Pam.IR.index;

   if (opc1 == 58 || opc1 == 62) { // ld/std: mode64 only
      vassert(mode64);
      /* stay sane with DS form: lowest 2 bits must be 00.  This
         should be guaranteed to us by iselWordExpr_AMode. */
      vassert(0 == (idx & 3));
   }
   p = mkFormD(p, opc1, rSD, rA, idx);
   return p;
}

static UChar* doAMode_RR ( UChar* p, UInt opc1, UInt opc2,
                           UInt rSD, PPCAMode* am, Bool mode64 )
{
   UInt rA, rB;
   vassert(am->tag == Pam_RR);

   rA  = iregNo(am->Pam.RR.base, mode64);
   rB  = iregNo(am->Pam.RR.index, mode64);
   
   p = mkFormX(p, opc1, rSD, rA, rB, opc2, 0);
   return p;
}


/* Load imm to r_dst */
static UChar* mkLoadImm ( UChar* p, UInt r_dst, ULong imm, Bool mode64 )
{
   vassert(r_dst < 0x20);

   if (!mode64) {
      /* In 32-bit mode, make sure the top 32 bits of imm are a sign
         extension of the bottom 32 bits, so that the range tests
         below work correctly. */
      UInt u32 = (UInt)imm;
      Int  s32 = (Int)u32;
      Long s64 = (Long)s32;
      imm = (ULong)s64;
   }

   if (imm >= 0xFFFFFFFFFFFF8000ULL || imm < 0x8000) {
      // sign-extendable from 16 bits

      // addi r_dst,0,imm  => li r_dst,imm
      p = mkFormD(p, 14, r_dst, 0, imm & 0xFFFF);
   } else {
      if (imm >= 0xFFFFFFFF80000000ULL || imm < 0x80000000ULL) {
         // sign-extendable from 32 bits

         // addis r_dst,r0,(imm>>16) => lis r_dst, (imm>>16)
         p = mkFormD(p, 15, r_dst, 0, (imm>>16) & 0xFFFF);
         // ori r_dst, r_dst, (imm & 0xFFFF)
         p = mkFormD(p, 24, r_dst, r_dst, imm & 0xFFFF);
      } else {
         // full 64bit immediate load: 5 (five!) insns.
         vassert(mode64);

         // load high word

         // lis r_dst, (imm>>48) & 0xFFFF
         p = mkFormD(p, 15, r_dst, 0, (imm>>48) & 0xFFFF);

         // ori r_dst, r_dst, (imm>>32) & 0xFFFF
         if ((imm>>32) & 0xFFFF)
            p = mkFormD(p, 24, r_dst, r_dst, (imm>>32) & 0xFFFF);
         
         // shift r_dst low word to high word => rldicr
         p = mkFormMD(p, 30, r_dst, r_dst, 32, 31, 1);

         // load low word

         // oris r_dst, r_dst, (imm>>16) & 0xFFFF
         if ((imm>>16) & 0xFFFF)
            p = mkFormD(p, 25, r_dst, r_dst, (imm>>16) & 0xFFFF);

         // ori r_dst, r_dst, (imm) & 0xFFFF
         if (imm & 0xFFFF)
            p = mkFormD(p, 24, r_dst, r_dst, imm & 0xFFFF);
      }
   }
   return p;
}

/* Move r_dst to r_src */
static UChar* mkMoveReg ( UChar* p, UInt r_dst, UInt r_src )
{
   vassert(r_dst < 0x20);
   vassert(r_src < 0x20);

   if (r_dst != r_src) {
      /* or r_dst, r_src, r_src */
      p = mkFormX(p, 31, r_src, r_dst, r_src, 444, 0 );
   }
   return p;
}

static UChar* mkFormVX ( UChar* p, UInt opc1, UInt r1, UInt r2,
                         UInt r3, UInt opc2 )
{
   UInt theInstr;
   vassert(opc1 < 0x40);
   vassert(r1   < 0x20);
   vassert(r2   < 0x20);
   vassert(r3   < 0x20);
   vassert(opc2 < 0x800);
   theInstr = ((opc1<<26) | (r1<<21) | (r2<<16) | (r3<<11) | opc2);
   return emit32(p, theInstr);
}

static UChar* mkFormVXR ( UChar* p, UInt opc1, UInt r1, UInt r2,
                          UInt r3, UInt Rc, UInt opc2 )
{
   UInt theInstr;
   vassert(opc1 < 0x40);
   vassert(r1   < 0x20);
   vassert(r2   < 0x20);
   vassert(r3   < 0x20);
   vassert(Rc   < 0x2);
   vassert(opc2 < 0x400);
   theInstr = ((opc1<<26) | (r1<<21) | (r2<<16) |
               (r3<<11) | (Rc<<10) | opc2);
   return emit32(p, theInstr);
}

static UChar* mkFormVA ( UChar* p, UInt opc1, UInt r1, UInt r2,
                         UInt r3, UInt r4, UInt opc2 )
{
   UInt theInstr;
   vassert(opc1 < 0x40);
   vassert(r1   < 0x20);
   vassert(r2   < 0x20);
   vassert(r3   < 0x20);
   vassert(r4   < 0x20);
   vassert(opc2 < 0x40);
   theInstr = ((opc1<<26) | (r1<<21) | (r2<<16) |
               (r3<<11) | (r4<<6) | opc2);
   return emit32(p, theInstr);
}



/* Emit an instruction into buf and return the number of bytes used.
   Note that buf is not the insn's final place, and therefore it is
   imperative to emit position-independent code. 

   Note, dispatch should always be NULL since ppc32/64 backends
   use a call-return scheme to get from the dispatcher to generated
   code and back.
*/
Int emit_PPCInstr ( UChar* buf, Int nbuf, PPCInstr* i, 
                    Bool mode64, void* dispatch )
{
   UChar* p = &buf[0];
   UChar* ptmp = p;
   vassert(nbuf >= 32);

   if (0) {
      vex_printf("asm  ");ppPPCInstr(i, mode64); vex_printf("\n");
   }

   switch (i->tag) {

   case Pin_LI:
      p = mkLoadImm(p, iregNo(i->Pin.LI.dst, mode64),
                    i->Pin.LI.imm64, mode64);
      goto done;

   case Pin_Alu: {
      PPCRH* srcR   = i->Pin.Alu.srcR;
      Bool   immR   = toBool(srcR->tag == Prh_Imm);
      UInt   r_dst  = iregNo(i->Pin.Alu.dst, mode64);
      UInt   r_srcL = iregNo(i->Pin.Alu.srcL, mode64);
      UInt   r_srcR = immR ? (-1)/*bogus*/ :
                             iregNo(srcR->Prh.Reg.reg, mode64);

      switch (i->Pin.Alu.op) {
      case Palu_ADD:
         if (immR) {
            /* addi (PPC32 p350) */
            vassert(srcR->Prh.Imm.syned);
            vassert(srcR->Prh.Imm.imm16 != 0x8000);
            p = mkFormD(p, 14, r_dst, r_srcL, srcR->Prh.Imm.imm16);
         } else {
            /* add (PPC32 p347) */
            p = mkFormXO(p, 31, r_dst, r_srcL, r_srcR, 0, 266, 0);
         }
         break;

      case Palu_SUB:
         if (immR) {
            /* addi (PPC32 p350), but with negated imm */
            vassert(srcR->Prh.Imm.syned);
            vassert(srcR->Prh.Imm.imm16 != 0x8000);
            p = mkFormD(p, 14, r_dst, r_srcL, (- srcR->Prh.Imm.imm16));
         } else {
            /* subf (PPC32 p537), with args the "wrong" way round */
            p = mkFormXO(p, 31, r_dst, r_srcR, r_srcL, 0, 40, 0);
         }
         break;

      case Palu_AND:
         if (immR) {
            /* andi. (PPC32 p358) */
            vassert(!srcR->Prh.Imm.syned);
            p = mkFormD(p, 28, r_srcL, r_dst, srcR->Prh.Imm.imm16);
         } else {
            /* and (PPC32 p356) */
            p = mkFormX(p, 31, r_srcL, r_dst, r_srcR, 28, 0);
         }
         break;

      case Palu_OR:
         if (immR) {
            /* ori (PPC32 p497) */
            vassert(!srcR->Prh.Imm.syned);
            p = mkFormD(p, 24, r_srcL, r_dst, srcR->Prh.Imm.imm16);
         } else {
            /* or (PPC32 p495) */
            p = mkFormX(p, 31, r_srcL, r_dst, r_srcR, 444, 0);
         }
         break;

      case Palu_XOR:
         if (immR) {
            /* xori (PPC32 p550) */
            vassert(!srcR->Prh.Imm.syned);
            p = mkFormD(p, 26, r_srcL, r_dst, srcR->Prh.Imm.imm16);
         } else {
            /* xor (PPC32 p549) */
            p = mkFormX(p, 31, r_srcL, r_dst, r_srcR, 316, 0);
         }
         break;

      default:
         goto bad;
      }
      goto done;
   }

   case Pin_Shft: {
      PPCRH* srcR   = i->Pin.Shft.srcR;
      Bool   sz32   = i->Pin.Shft.sz32;
      Bool   immR   = toBool(srcR->tag == Prh_Imm);
      UInt   r_dst  = iregNo(i->Pin.Shft.dst, mode64);
      UInt   r_srcL = iregNo(i->Pin.Shft.srcL, mode64);
      UInt   r_srcR = immR ? (-1)/*bogus*/ :
                             iregNo(srcR->Prh.Reg.reg, mode64);
      if (!mode64)
         vassert(sz32);

      switch (i->Pin.Shft.op) {
      case Pshft_SHL:
         if (sz32) {
            if (immR) {
               /* rd = rs << n, 1 <= n <= 31
                  is
                  rlwinm rd,rs,n,0,31-n  (PPC32 p501)
               */
               UInt n = srcR->Prh.Imm.imm16;
               vassert(!srcR->Prh.Imm.syned);
               vassert(n > 0 && n < 32);
               p = mkFormM(p, 21, r_srcL, r_dst, n, 0, 31-n, 0);
            } else {
               /* slw (PPC32 p505) */
               p = mkFormX(p, 31, r_srcL, r_dst, r_srcR, 24, 0);
            }
         } else {
            if (immR) {
               /* rd = rs << n, 1 <= n <= 63
                  is
                  rldicr rd,rs,n,63-n  (PPC64 p559)
               */
               UInt n = srcR->Prh.Imm.imm16;
               vassert(!srcR->Prh.Imm.syned);
               vassert(n > 0 && n < 64);
               p = mkFormMD(p, 30, r_srcL, r_dst, n, 63-n, 1);
            } else {
               /* sld (PPC64 p568) */
               p = mkFormX(p, 31, r_srcL, r_dst, r_srcR, 27, 0);
            }
         }
         break;

      case Pshft_SHR:
         if (sz32) {
             if (immR) {
               /* rd = rs >>u n, 1 <= n <= 31
                  is
                  rlwinm rd,rs,32-n,n,31  (PPC32 p501)
               */
               UInt n = srcR->Prh.Imm.imm16;
               vassert(!srcR->Prh.Imm.syned);
               vassert(n > 0 && n < 32);
               p = mkFormM(p, 21, r_srcL, r_dst, 32-n, n, 31, 0);
            } else {
               /* srw (PPC32 p508) */
               p = mkFormX(p, 31, r_srcL, r_dst, r_srcR, 536, 0);
            }
         } else {
            if (immR) {
               /* rd = rs >>u n, 1 <= n <= 63
                  is
                  rldicl rd,rs,64-n,n  (PPC64 p558)
               */
               UInt n = srcR->Prh.Imm.imm16;
               vassert(!srcR->Prh.Imm.syned);
               vassert(n > 0 && n < 64);
               p = mkFormMD(p, 30, r_srcL, r_dst, 64-n, n, 0);
            } else {
               /* srd (PPC64 p574) */
               p = mkFormX(p, 31, r_srcL, r_dst, r_srcR, 539, 0);
            }
         }
         break;

      case Pshft_SAR:
         if (sz32) {
            if (immR) {
               /* srawi (PPC32 p507) */
               UInt n = srcR->Prh.Imm.imm16;
               vassert(!srcR->Prh.Imm.syned);
               /* In 64-bit mode, we allow right shifts by zero bits
                  as that is a handy way to sign extend the lower 32
                  bits into the upper 32 bits. */
               if (mode64)
                  vassert(n >= 0 && n < 32);
               else 
                  vassert(n > 0 && n < 32);
               p = mkFormX(p, 31, r_srcL, r_dst, n, 824, 0);
            } else {
               /* sraw (PPC32 p506) */
               p = mkFormX(p, 31, r_srcL, r_dst, r_srcR, 792, 0);
            }
         } else {
            if (immR) {
               /* sradi (PPC64 p571) */
               UInt n = srcR->Prh.Imm.imm16;
               vassert(!srcR->Prh.Imm.syned);
               vassert(n > 0 && n < 64);
               p = mkFormXS(p, 31, r_srcL, r_dst, n, 413, 0);
            } else {
               /* srad (PPC32 p570) */
               p = mkFormX(p, 31, r_srcL, r_dst, r_srcR, 794, 0);
            }
         }
         break;

      default:
         goto bad;
      }
      goto done;
   }

   case Pin_AddSubC: {
      Bool isAdd  = i->Pin.AddSubC.isAdd;
      Bool setC   = i->Pin.AddSubC.setC;
      UInt r_srcL = iregNo(i->Pin.AddSubC.srcL, mode64);
      UInt r_srcR = iregNo(i->Pin.AddSubC.srcR, mode64);
      UInt r_dst  = iregNo(i->Pin.AddSubC.dst, mode64);
      
      if (isAdd) {
         if (setC) /* addc (PPC32 p348) */
            p = mkFormXO(p, 31, r_dst, r_srcL, r_srcR, 0, 10, 0);
         else          /* adde (PPC32 p349) */
            p = mkFormXO(p, 31, r_dst, r_srcL, r_srcR, 0, 138, 0);
      } else {
         /* subfX, with args the "wrong" way round */
         if (setC) /* subfc (PPC32 p538) */
            p = mkFormXO(p, 31, r_dst, r_srcR, r_srcL, 0, 8, 0);
         else          /* subfe (PPC32 p539) */
            p = mkFormXO(p, 31, r_dst, r_srcR, r_srcL, 0, 136, 0);
      }
      goto done;
   }

   case Pin_Cmp: {
      Bool syned  = i->Pin.Cmp.syned;
      Bool sz32   = i->Pin.Cmp.sz32;
      UInt fld1   = i->Pin.Cmp.crfD << 2;
      UInt r_srcL = iregNo(i->Pin.Cmp.srcL, mode64);
      UInt r_srcR, imm_srcR;
      PPCRH* srcR = i->Pin.Cmp.srcR;

      if (!mode64)        // cmp double word invalid for mode32
         vassert(sz32);      
      else if (!sz32)     // mode64 && cmp64: set L=1
         fld1 |= 1;
 
      switch (srcR->tag) {
      case Prh_Imm:
         vassert(syned == srcR->Prh.Imm.syned);
         imm_srcR = srcR->Prh.Imm.imm16;
         if (syned) {  // cmpw/di  (signed)   (PPC32 p368)
            vassert(imm_srcR != 0x8000);
            p = mkFormD(p, 11, fld1, r_srcL, imm_srcR);
         } else {      // cmplw/di (unsigned) (PPC32 p370)
            p = mkFormD(p, 10, fld1, r_srcL, imm_srcR);
         }
         break;
      case Prh_Reg:
         r_srcR = iregNo(srcR->Prh.Reg.reg, mode64);
         if (syned)  // cmpwi  (signed)   (PPC32 p367)
            p = mkFormX(p, 31, fld1, r_srcL, r_srcR, 0, 0);
         else        // cmplwi (unsigned) (PPC32 p379)
            p = mkFormX(p, 31, fld1, r_srcL, r_srcR, 32, 0);
         break;
      default: 
         goto bad;
      }        
      goto done;
   }

   case Pin_Unary: {
      UInt r_dst = iregNo(i->Pin.Unary.dst, mode64);
      UInt r_src = iregNo(i->Pin.Unary.src, mode64);

      switch (i->Pin.Unary.op) {
      case Pun_NOT:  // nor r_dst,r_src,r_src
         p = mkFormX(p, 31, r_src, r_dst, r_src, 124, 0);
         break;
      case Pun_NEG:  // neg r_dst,r_src
         p = mkFormXO(p, 31, r_dst, r_src, 0, 0, 104, 0);
         break;
      case Pun_CLZ32:  // cntlzw r_dst, r_src
         p = mkFormX(p, 31, r_src, r_dst, 0, 26, 0);
         break;
      case Pun_CLZ64:  // cntlzd r_dst, r_src
         vassert(mode64);
         p = mkFormX(p, 31, r_src, r_dst, 0, 58, 0);
         break;
      case Pun_EXTSW:  // extsw r_dst, r_src
         vassert(mode64);
         p = mkFormX(p, 31, r_src, r_dst, 0, 986, 0);
         break;
      default: goto bad;
      }
      goto done;
   }

   case Pin_MulL: {
      Bool syned  = i->Pin.MulL.syned;
      Bool sz32   = i->Pin.MulL.sz32;
      UInt r_dst  = iregNo(i->Pin.MulL.dst, mode64);
      UInt r_srcL = iregNo(i->Pin.MulL.srcL, mode64);
      UInt r_srcR = iregNo(i->Pin.MulL.srcR, mode64);

      if (!mode64)
         vassert(sz32);

      if (i->Pin.MulL.hi) {
         // mul hi words, must consider sign
         if (sz32) {
            if (syned)  // mulhw r_dst,r_srcL,r_srcR
               p = mkFormXO(p, 31, r_dst, r_srcL, r_srcR, 0, 75, 0);
            else        // mulhwu r_dst,r_srcL,r_srcR
               p = mkFormXO(p, 31, r_dst, r_srcL, r_srcR, 0, 11, 0);
         } else {
            if (syned)  // mulhd r_dst,r_srcL,r_srcR
               p = mkFormXO(p, 31, r_dst, r_srcL, r_srcR, 0, 73, 0);
            else        // mulhdu r_dst,r_srcL,r_srcR
               p = mkFormXO(p, 31, r_dst, r_srcL, r_srcR, 0, 9, 0);
         }
      } else {
         // mul low word, sign is irrelevant
         vassert(!i->Pin.MulL.syned);
         if (sz32)      // mullw r_dst,r_srcL,r_srcR
            p = mkFormXO(p, 31, r_dst, r_srcL, r_srcR, 0, 235, 0);
         else           // mulld r_dst,r_srcL,r_srcR
            p = mkFormXO(p, 31, r_dst, r_srcL, r_srcR, 0, 233, 0);
      }
      goto done;
   }

   case Pin_Div: {
      Bool syned  = i->Pin.Div.syned;
      Bool sz32   = i->Pin.Div.sz32;
      UInt r_dst  = iregNo(i->Pin.Div.dst, mode64);
      UInt r_srcL = iregNo(i->Pin.Div.srcL, mode64);
      UInt r_srcR = iregNo(i->Pin.Div.srcR, mode64);

      if (!mode64)
         vassert(sz32);

      if (sz32) {
         if (syned)  // divw r_dst,r_srcL,r_srcR
            p = mkFormXO(p, 31, r_dst, r_srcL, r_srcR, 0, 491, 0);
         else        // divwu r_dst,r_srcL,r_srcR
            p = mkFormXO(p, 31, r_dst, r_srcL, r_srcR, 0, 459, 0);
      } else {
         if (syned)  // divd r_dst,r_srcL,r_srcR
            p = mkFormXO(p, 31, r_dst, r_srcL, r_srcR, 0, 489, 0);
         else        // divdu r_dst,r_srcL,r_srcR
            p = mkFormXO(p, 31, r_dst, r_srcL, r_srcR, 0, 457, 0);
      }
      goto done;
   }

   case Pin_Call: {
      PPCCondCode cond  = i->Pin.Call.cond;
      UInt        r_dst = 10;
      /* As per detailed comment for Pin_Call in
         getRegUsage_PPCInstr above, %r10 is used as an address temp */

      /* jump over the following insns if condition does not hold */
      if (cond.test != Pct_ALWAYS) {
         /* jmp fwds if !condition */
         /* don't know how many bytes to jump over yet...
            make space for a jump instruction and fill in later. */
         ptmp = p; /* fill in this bit later */
         p += 4;                                          // p += 4
      }

      /* load target to r_dst */                          // p += 4|8|20
      p = mkLoadImm(p, r_dst, i->Pin.Call.target, mode64);

      /* mtspr 9,r_dst => move r_dst to count register */
      p = mkFormXFX(p, r_dst, 9, 467);                    // p += 4
      
      /* bctrl => branch to count register (and save to lr) */
      p = mkFormXL(p, 19, Pct_ALWAYS, 0, 0, 528, 1);      // p += 4

      /* Fix up the conditional jump, if there was one. */
      if (cond.test != Pct_ALWAYS) {
         Int delta = p - ptmp;
         vassert(delta >= 16 && delta <= 32);
         /* bc !ct,cf,delta */
         mkFormB(ptmp, invertCondTest(cond.test),
                 cond.flag, (delta>>2), 0, 0);
      }
      goto done;
   }

   case Pin_Goto: {
      UInt        trc   = 0;
      UChar       r_ret = 3;        /* Put target addr into %r3 */
      PPCCondCode cond  = i->Pin.Goto.cond;
      UInt r_dst;
      ULong imm_dst;

      vassert(dispatch == NULL);
      
      /* First off, if this is conditional, create a conditional
         jump over the rest of it. */
      if (cond.test != Pct_ALWAYS) {
         /* jmp fwds if !condition */
         /* don't know how many bytes to jump over yet...
            make space for a jump instruction and fill in later. */
         ptmp = p; /* fill in this bit later */
         p += 4;
      }

      // cond succeeds...
      
      /* If a non-boring, set GuestStatePtr appropriately. */
      switch (i->Pin.Goto.jk) {
         case Ijk_ClientReq:   trc = VEX_TRC_JMP_CLIENTREQ;   break;
         case Ijk_Sys_syscall: trc = VEX_TRC_JMP_SYS_SYSCALL; break;
         case Ijk_Yield:       trc = VEX_TRC_JMP_YIELD;       break;
         case Ijk_EmWarn:      trc = VEX_TRC_JMP_EMWARN;      break;
         case Ijk_EmFail:      trc = VEX_TRC_JMP_EMFAIL;      break;
         case Ijk_MapFail:     trc = VEX_TRC_JMP_MAPFAIL;     break;
         case Ijk_NoDecode:    trc = VEX_TRC_JMP_NODECODE;    break;
         case Ijk_TInval:      trc = VEX_TRC_JMP_TINVAL;      break;
         case Ijk_NoRedir:     trc = VEX_TRC_JMP_NOREDIR;     break;
         case Ijk_SigTRAP:     trc = VEX_TRC_JMP_SIGTRAP;     break;
         case Ijk_SigBUS:      trc = VEX_TRC_JMP_SIGBUS;      break;
         case Ijk_Ret:
         case Ijk_Call:
         case Ijk_Boring:
            break;
         default: 
            ppIRJumpKind(i->Pin.Goto.jk);
            vpanic("emit_PPCInstr.Pin_Goto: unknown jump kind");
      }
      if (trc !=0) {
         vassert(trc < 0x10000);
         /* addi r31,0,trc */
         p = mkFormD(p, 14, 31, 0, trc);               // p += 4
      }

      /* Get the destination address into %r_ret */
      if (i->Pin.Goto.dst->tag == Pri_Imm) {
         imm_dst = i->Pin.Goto.dst->Pri.Imm;
         p = mkLoadImm(p, r_ret, imm_dst, mode64);     // p += 4|8|20
      } else {
         vassert(i->Pin.Goto.dst->tag == Pri_Reg);
         r_dst = iregNo(i->Pin.Goto.dst->Pri.Reg, mode64);
         p = mkMoveReg(p, r_ret, r_dst);               // p += 4
      }
      
      /* blr */
      p = mkFormXL(p, 19, Pct_ALWAYS, 0, 0, 16, 0);    // p += 4

      /* Fix up the conditional jump, if there was one. */
      if (cond.test != Pct_ALWAYS) {
         Int delta = p - ptmp;
         vassert(delta >= 12 && delta <= 32);
         /* bc !ct,cf,delta */
         mkFormB(ptmp, invertCondTest(cond.test),
                 cond.flag, delta>>2, 0, 0);
      }
      goto done;
   }

   case Pin_CMov: {
      UInt  r_dst, r_src;
      ULong imm_src;
      PPCCondCode cond;
      vassert(i->Pin.CMov.cond.test != Pct_ALWAYS);

      r_dst = iregNo(i->Pin.CMov.dst, mode64);
      cond = i->Pin.CMov.cond;

      /* branch (if cond fails) over move instrs */
      if (cond.test != Pct_ALWAYS) {
         /* don't know how many bytes to jump over yet...
            make space for a jump instruction and fill in later. */
         ptmp = p; /* fill in this bit later */
         p += 4;
      }

      // cond true: move src => dst
      switch (i->Pin.CMov.src->tag) {
      case Pri_Imm:
         imm_src = i->Pin.CMov.src->Pri.Imm;
         p = mkLoadImm(p, r_dst, imm_src, mode64);  // p += 4|8|20
         break;
      case Pri_Reg:
         r_src = iregNo(i->Pin.CMov.src->Pri.Reg, mode64);
         p = mkMoveReg(p, r_dst, r_src);            // p += 4
         break;
      default: goto bad;
      }

      /* Fix up the conditional jump, if there was one. */
      if (cond.test != Pct_ALWAYS) {
         Int delta = p - ptmp;
         vassert(delta >= 8 && delta <= 24);
         /* bc !ct,cf,delta */
         mkFormB(ptmp, invertCondTest(cond.test),
                 cond.flag, (delta>>2), 0, 0);
      }
      goto done;
   }

   case Pin_Load: {
      PPCAMode* am_addr = i->Pin.Load.src;
      UInt r_dst = iregNo(i->Pin.Load.dst, mode64);
      UInt opc1, opc2, sz = i->Pin.Load.sz;
      switch (am_addr->tag) {
      case Pam_IR:
         if (mode64 && (sz == 4 || sz == 8)) {
            /* should be guaranteed to us by iselWordExpr_AMode */
            vassert(0 == (am_addr->Pam.IR.index & 3));
         }
         switch(sz) {
            case 1:  opc1 = 34; break;
            case 2:  opc1 = 40; break;
            case 4:  opc1 = 32; break;
            case 8:  opc1 = 58; vassert(mode64); break;
            default: goto bad;
         }
         p = doAMode_IR(p, opc1, r_dst, am_addr, mode64);
         goto done;
      case Pam_RR:
         switch(sz) {
            case 1:  opc2 = 87;  break;
            case 2:  opc2 = 279; break;
            case 4:  opc2 = 23;  break;
            case 8:  opc2 = 21; vassert(mode64); break;
            default: goto bad;
         }
         p = doAMode_RR(p, 31, opc2, r_dst, am_addr, mode64);
         goto done;
      default:
         goto bad;
      }
   }

   case Pin_LoadL: {
      if (i->Pin.LoadL.sz == 4) {
         p = mkFormX(p, 31, iregNo(i->Pin.LoadL.dst, mode64),
                     0, iregNo(i->Pin.LoadL.src, mode64), 20, 0);
         goto done;
      }
      if (i->Pin.LoadL.sz == 8 && mode64) {
         p = mkFormX(p, 31, iregNo(i->Pin.LoadL.dst, mode64),
                     0, iregNo(i->Pin.LoadL.src, mode64), 84, 0);
         goto done;
      }
      goto bad;
   }

   case Pin_Set: {
      /* Make the destination register be 1 or 0, depending on whether
         the relevant condition holds. */
      UInt        r_dst = iregNo(i->Pin.Set.dst, mode64);
      PPCCondCode cond  = i->Pin.Set.cond;
      UInt rot_imm, r_tmp;

      if (cond.test == Pct_ALWAYS) {
         // Just load 1 to dst => li dst,1
         p = mkFormD(p, 14, r_dst, 0, 1);
      } else {
         vassert(cond.flag != Pcf_NONE);
         rot_imm = 1 + cond.flag;
         r_tmp = 0;  // Not set in getAllocable, so no need to declare.

         // r_tmp = CR  => mfcr r_tmp
         p = mkFormX(p, 31, r_tmp, 0, 0, 19, 0);

         // r_dst = flag (rotate left and mask)
         //  => rlwinm r_dst,r_tmp,rot_imm,31,31
         p = mkFormM(p, 21, r_tmp, r_dst, rot_imm, 31, 31, 0);

         if (cond.test == Pct_FALSE) {
            // flip bit  => xori r_dst,r_dst,1
            p = mkFormD(p, 26, r_dst, r_dst, 1);
         }
      }
      goto done;
   }

   case Pin_MfCR:
      // mfcr dst
      p = mkFormX(p, 31, iregNo(i->Pin.MfCR.dst, mode64), 0, 0, 19, 0);
      goto done;

   case Pin_MFence: {
      p = mkFormX(p, 31, 0, 0, 0, 598, 0);   // sync, PPC32 p616
      // CAB: Should this be isync?
      //    p = mkFormXL(p, 19, 0, 0, 0, 150, 0);  // isync, PPC32 p467
      goto done;
   }

   case Pin_Store: {
      PPCAMode* am_addr = i->Pin.Store.dst;
      UInt r_src = iregNo(i->Pin.Store.src, mode64);
      UInt opc1, opc2, sz = i->Pin.Store.sz;
      switch (i->Pin.Store.dst->tag) {
      case Pam_IR:
         if (mode64 && (sz == 4 || sz == 8)) {
            /* should be guaranteed to us by iselWordExpr_AMode */
            vassert(0 == (am_addr->Pam.IR.index & 3));
         }
         switch(sz) {
         case 1: opc1 = 38; break;
         case 2: opc1 = 44; break;
         case 4: opc1 = 36; break;
         case 8: vassert(mode64);
                 opc1 = 62; break;
         default:
            goto bad;
         }
         p = doAMode_IR(p, opc1, r_src, am_addr, mode64);
         goto done;
      case Pam_RR:
         switch(sz) {
         case 1: opc2 = 215; break;
         case 2: opc2 = 407; break;
         case 4: opc2 = 151; break;
         case 8: vassert(mode64);
                 opc2 = 149; break;
         default:
            goto bad;
         }
         p = doAMode_RR(p, 31, opc2, r_src, am_addr, mode64);
         goto done;
      default:
         goto bad;
      }
      goto done;
   }

   case Pin_StoreC: {
      if (i->Pin.StoreC.sz == 4) {
         p = mkFormX(p, 31, iregNo(i->Pin.StoreC.src, mode64),
                     0, iregNo(i->Pin.StoreC.dst, mode64), 150, 1);
         goto done;
      }
      if (i->Pin.StoreC.sz == 8 && mode64) {
         p = mkFormX(p, 31, iregNo(i->Pin.StoreC.src, mode64),
                     0, iregNo(i->Pin.StoreC.dst, mode64), 214, 1);
         goto done;
      }
      goto bad;
   }

   case Pin_FpUnary: {
      UInt fr_dst = fregNo(i->Pin.FpUnary.dst);
      UInt fr_src = fregNo(i->Pin.FpUnary.src);
      switch (i->Pin.FpUnary.op) {
      case Pfp_RSQRTE: // frsqrtre, PPC32 p424
         p = mkFormA( p, 63, fr_dst, 0, fr_src, 0, 26, 0 );
         break;
      case Pfp_RES:   // fres, PPC32 p421
         p = mkFormA( p, 59, fr_dst, 0, fr_src, 0, 24, 0 );
         break;
      case Pfp_SQRT:  // fsqrt, PPC32 p427
         p = mkFormA( p, 63, fr_dst, 0, fr_src, 0, 22, 0 );
         break;
      case Pfp_ABS:   // fabs, PPC32 p399
         p = mkFormX(p, 63, fr_dst, 0, fr_src, 264, 0);
         break;
      case Pfp_NEG:   // fneg, PPC32 p416
         p = mkFormX(p, 63, fr_dst, 0, fr_src, 40, 0);
         break;
      case Pfp_MOV:   // fmr, PPC32 p410
         p = mkFormX(p, 63, fr_dst, 0, fr_src, 72, 0);
         break;
      case Pfp_FRIM:  // frim, PPC ISA 2.05 p137
         p = mkFormX(p, 63, fr_dst, 0, fr_src, 488, 0);
         break;
      case Pfp_FRIP:  // frip, PPC ISA 2.05 p137
         p = mkFormX(p, 63, fr_dst, 0, fr_src, 456, 0);
         break;
      case Pfp_FRIN:  // frin, PPC ISA 2.05 p137
         p = mkFormX(p, 63, fr_dst, 0, fr_src, 392, 0);
         break;
      case Pfp_FRIZ:  // friz, PPC ISA 2.05 p137
         p = mkFormX(p, 63, fr_dst, 0, fr_src, 424, 0);
         break;
      default:
         goto bad;
      }
      goto done;
   }

   case Pin_FpBinary: {
      UInt fr_dst  = fregNo(i->Pin.FpBinary.dst);
      UInt fr_srcL = fregNo(i->Pin.FpBinary.srcL);
      UInt fr_srcR = fregNo(i->Pin.FpBinary.srcR);
      switch (i->Pin.FpBinary.op) {
      case Pfp_ADDD:   // fadd, PPC32 p400
         p = mkFormA( p, 63, fr_dst, fr_srcL, fr_srcR, 0, 21, 0 );
         break;
      case Pfp_ADDS:   // fadds, PPC32 p401
         p = mkFormA( p, 59, fr_dst, fr_srcL, fr_srcR, 0, 21, 0 );
         break;
      case Pfp_SUBD:   // fsub, PPC32 p429
         p = mkFormA( p, 63, fr_dst, fr_srcL, fr_srcR, 0, 20, 0 );
         break;
      case Pfp_SUBS:   // fsubs, PPC32 p430
         p = mkFormA( p, 59, fr_dst, fr_srcL, fr_srcR, 0, 20, 0 );
         break;
      case Pfp_MULD:   // fmul, PPC32 p413
         p = mkFormA( p, 63, fr_dst, fr_srcL, 0, fr_srcR, 25, 0 );
         break;
      case Pfp_MULS:   // fmuls, PPC32 p414
         p = mkFormA( p, 59, fr_dst, fr_srcL, 0, fr_srcR, 25, 0 );
         break;
      case Pfp_DIVD:   // fdiv, PPC32 p406
         p = mkFormA( p, 63, fr_dst, fr_srcL, fr_srcR, 0, 18, 0 );
         break;
      case Pfp_DIVS:   // fdivs, PPC32 p407
         p = mkFormA( p, 59, fr_dst, fr_srcL, fr_srcR, 0, 18, 0 );
         break;
      default:
         goto bad;
      }
      goto done;
   }

   case Pin_FpMulAcc: {
      UInt fr_dst    = fregNo(i->Pin.FpMulAcc.dst);
      UInt fr_srcML  = fregNo(i->Pin.FpMulAcc.srcML);
      UInt fr_srcMR  = fregNo(i->Pin.FpMulAcc.srcMR);
      UInt fr_srcAcc = fregNo(i->Pin.FpMulAcc.srcAcc);
      switch (i->Pin.FpMulAcc.op) {
      case Pfp_MADDD:   // fmadd, PPC32 p408
         p = mkFormA( p, 63, fr_dst, fr_srcML, fr_srcAcc, fr_srcMR, 29, 0 );
         break;
      case Pfp_MADDS:   // fmadds, PPC32 p409
         p = mkFormA( p, 59, fr_dst, fr_srcML, fr_srcAcc, fr_srcMR, 29, 0 );
         break;
      case Pfp_MSUBD:   // fmsub, PPC32 p411
         p = mkFormA( p, 63, fr_dst, fr_srcML, fr_srcAcc, fr_srcMR, 28, 0 );
         break;
      case Pfp_MSUBS:   // fmsubs, PPC32 p412
         p = mkFormA( p, 59, fr_dst, fr_srcML, fr_srcAcc, fr_srcMR, 28, 0 );
         break;
      default:
         goto bad;
      }
      goto done;
   }

   case Pin_FpLdSt: {
      PPCAMode* am_addr = i->Pin.FpLdSt.addr;
      UInt f_reg = fregNo(i->Pin.FpLdSt.reg);
      Bool idxd = toBool(i->Pin.FpLdSt.addr->tag == Pam_RR);
      UChar sz = i->Pin.FpLdSt.sz;
      UInt opc;
      vassert(sz == 4 || sz == 8);

      if (i->Pin.FpLdSt.isLoad) {   // Load from memory
         if (idxd) {  // lf[s|d]x, PPC32 p444|440
            opc = (sz == 4) ? 535 : 599;
            p = doAMode_RR(p, 31, opc, f_reg, am_addr, mode64);
         } else {     // lf[s|d], PPC32 p441|437
            opc = (sz == 4) ? 48 : 50;
            p = doAMode_IR(p, opc, f_reg, am_addr, mode64);
         }
      } else {                      // Store to memory
         if (idxd) { // stf[s|d]x, PPC32 p521|516
            opc = (sz == 4) ? 663 : 727;
            p = doAMode_RR(p, 31, opc, f_reg, am_addr, mode64);
         } else {    // stf[s|d], PPC32 p518|513
            opc = (sz == 4) ? 52 : 54;
            p = doAMode_IR(p, opc, f_reg, am_addr, mode64);
         }
      }
      goto done;
   }

   case Pin_FpSTFIW: {
      UInt ir_addr = iregNo(i->Pin.FpSTFIW.addr, mode64);
      UInt fr_data = fregNo(i->Pin.FpSTFIW.data);
      // stfiwx (store fp64[lo32] as int32), PPC32 p517
      // Use rA==0, so that EA == rB == ir_addr
      p = mkFormX(p, 31, fr_data, 0/*rA=0*/, ir_addr, 983, 0);
      goto done;
   }

   case Pin_FpRSP: {
      UInt fr_dst = fregNo(i->Pin.FpRSP.dst);
      UInt fr_src = fregNo(i->Pin.FpRSP.src);
      // frsp, PPC32 p423
      p = mkFormX(p, 63, fr_dst, 0, fr_src, 12, 0);
      goto done;
   }

   case Pin_FpCftI: {
      UInt fr_dst = fregNo(i->Pin.FpCftI.dst);
      UInt fr_src = fregNo(i->Pin.FpCftI.src);
      if (i->Pin.FpCftI.fromI == False && i->Pin.FpCftI.int32 == True) {
         // fctiw (conv f64 to i32), PPC32 p404
         p = mkFormX(p, 63, fr_dst, 0, fr_src, 14, 0);
         goto done;
      }
      if (i->Pin.FpCftI.fromI == False && i->Pin.FpCftI.int32 == False) {
         // fctid (conv f64 to i64), PPC64 p437
         p = mkFormX(p, 63, fr_dst, 0, fr_src, 814, 0);
         goto done;
      }
      if (i->Pin.FpCftI.fromI == True && i->Pin.FpCftI.int32 == False) {
         if (i->Pin.FpCftI.syned == True) {
            // fcfid (conv i64 to f64), PPC64 p434
            p = mkFormX(p, 63, fr_dst, 0, fr_src, 846, 0);
            goto done;
         } else if (i->Pin.FpCftI.flt64 == True) {
            // fcfidu (conv u64 to f64)
            p = mkFormX(p, 63, fr_dst, 0, fr_src, 974, 0);
            goto done;
         } else {
            // fcfidus (conv u64 to f32)
            p = mkFormX(p, 59, fr_dst, 0, fr_src, 974, 0);
            goto done;
         }
      }
      goto bad;
   }

   case Pin_FpCMov: {
      UInt        fr_dst = fregNo(i->Pin.FpCMov.dst);
      UInt        fr_src = fregNo(i->Pin.FpCMov.src);
      PPCCondCode cc     = i->Pin.FpCMov.cond;

      if (fr_dst == fr_src) goto done;
      
      vassert(cc.test != Pct_ALWAYS);

      /* jmp fwds if !condition */
      if (cc.test != Pct_ALWAYS) {
         /* bc !ct,cf,n_bytes>>2 */
         p = mkFormB(p, invertCondTest(cc.test), cc.flag, 8>>2, 0, 0);
      }

      // fmr, PPC32 p410
      p = mkFormX(p, 63, fr_dst, 0, fr_src, 72, 0);
      goto done;
   }

   case Pin_FpLdFPSCR: {
      UInt fr_src = fregNo(i->Pin.FpLdFPSCR.src);
      p = mkFormXFL(p, 0xFF, fr_src);     // mtfsf, PPC32 p480
      goto done;
   }

   case Pin_FpCmp: {
      UChar crfD    = 1;
      UInt  r_dst   = iregNo(i->Pin.FpCmp.dst, mode64);
      UInt  fr_srcL = fregNo(i->Pin.FpCmp.srcL);
      UInt  fr_srcR = fregNo(i->Pin.FpCmp.srcR);
      vassert(crfD < 8);
      // fcmpo, PPC32 p402
      p = mkFormX(p, 63, crfD<<2, fr_srcL, fr_srcR, 32, 0);

      // mfcr (mv CR to r_dst), PPC32 p467
      p = mkFormX(p, 31, r_dst, 0, 0, 19, 0);
      
      // rlwinm r_dst,r_dst,8,28,31, PPC32 p501
      //  => rotate field 1 to bottomw of word, masking out upper 28
      p = mkFormM(p, 21, r_dst, r_dst, 8, 28, 31, 0);
      goto done;
   }

   case Pin_RdWrLR: {
      UInt reg = iregNo(i->Pin.RdWrLR.gpr, mode64);
      /* wrLR==True ? mtlr r4 : mflr r4 */
      p = mkFormXFX(p, reg, 8, (i->Pin.RdWrLR.wrLR==True) ? 467 : 339);
      goto done;
   }


   /* AltiVec */
   case Pin_AvLdSt: {
      UInt opc2, v_reg, r_idx, r_base;
      UChar sz   = i->Pin.AvLdSt.sz;
      Bool  idxd = toBool(i->Pin.AvLdSt.addr->tag == Pam_RR);
      vassert(sz == 1 || sz == 2 || sz == 4 || sz == 16);

      v_reg  = vregNo(i->Pin.AvLdSt.reg);
      r_base = iregNo(i->Pin.AvLdSt.addr->Pam.RR.base, mode64);

      // Only have AltiVec AMode_RR: kludge AMode_IR
      if (!idxd) {
         r_idx = 30;                       // XXX: Using r30 as temp
         p = mkLoadImm(p, r_idx,
                       i->Pin.AvLdSt.addr->Pam.IR.index, mode64);
      } else {
         r_idx  = iregNo(i->Pin.AvLdSt.addr->Pam.RR.index, mode64);
      }

      if (i->Pin.FpLdSt.isLoad) {  // Load from memory (1,2,4,16)
         opc2 = (sz==1) ?   7 : (sz==2) ?  39 : (sz==4) ?  71 : 103;
         p = mkFormX(p, 31, v_reg, r_idx, r_base, opc2, 0);
      } else {                      // Store to memory (1,2,4,16)
         opc2 = (sz==1) ? 135 : (sz==2) ? 167 : (sz==4) ? 199 : 231;
         p = mkFormX(p, 31, v_reg, r_idx, r_base, opc2, 0);
      }
      goto done;
   }

   case Pin_AvUnary: {
      UInt v_dst = vregNo(i->Pin.AvUnary.dst);
      UInt v_src = vregNo(i->Pin.AvUnary.src);
      UInt opc2;
      switch (i->Pin.AvUnary.op) {
      case Pav_MOV:       opc2 = 1156; break; // vor vD,vS,vS
      case Pav_NOT:       opc2 = 1284; break; // vnor vD,vS,vS
      case Pav_UNPCKH8S:  opc2 =  526; break; // vupkhsb
      case Pav_UNPCKH16S: opc2 =  590; break; // vupkhsh
      case Pav_UNPCKL8S:  opc2 =  654; break; // vupklsb
      case Pav_UNPCKL16S: opc2 =  718; break; // vupklsh
      case Pav_UNPCKHPIX: opc2 =  846; break; // vupkhpx
      case Pav_UNPCKLPIX: opc2 =  974; break; // vupklpx
      default:
         goto bad;
      }
      switch (i->Pin.AvUnary.op) {
      case Pav_MOV:
      case Pav_NOT:
         p = mkFormVX( p, 4, v_dst, v_src, v_src, opc2 );
         break;
      default:
         p = mkFormVX( p, 4, v_dst, 0, v_src, opc2 );
         break;
      }
      goto done;
   }

   case Pin_AvBinary: {
      UInt v_dst  = vregNo(i->Pin.AvBinary.dst);
      UInt v_srcL = vregNo(i->Pin.AvBinary.srcL);
      UInt v_srcR = vregNo(i->Pin.AvBinary.srcR);
      UInt opc2;
      if (i->Pin.AvBinary.op == Pav_SHL) {
         p = mkFormVX( p, 4, v_dst, v_srcL, v_srcR, 1036 ); // vslo
         p = mkFormVX( p, 4, v_dst, v_dst,  v_srcR, 452 );  // vsl
         goto done;
      }
      if (i->Pin.AvBinary.op == Pav_SHR) {
         p = mkFormVX( p, 4, v_dst, v_srcL, v_srcR, 1100 ); // vsro
         p = mkFormVX( p, 4, v_dst, v_dst,  v_srcR, 708 );  // vsr
         goto done;
      }
      switch (i->Pin.AvBinary.op) {
      /* Bitwise */
      case Pav_AND:       opc2 = 1028; break; // vand
      case Pav_OR:        opc2 = 1156; break; // vor
      case Pav_XOR:       opc2 = 1220; break; // vxor
      default:
         goto bad;
      }
      p = mkFormVX( p, 4, v_dst, v_srcL, v_srcR, opc2 );
      goto done;
   }

   case Pin_AvBin8x16: {
      UInt v_dst  = vregNo(i->Pin.AvBin8x16.dst);
      UInt v_srcL = vregNo(i->Pin.AvBin8x16.srcL);
      UInt v_srcR = vregNo(i->Pin.AvBin8x16.srcR);
      UInt opc2;
      switch (i->Pin.AvBin8x16.op) {

      case Pav_ADDU:     opc2 =    0; break; // vaddubm
      case Pav_QADDU:    opc2 =  512; break; // vaddubs
      case Pav_QADDS:    opc2 =  768; break; // vaddsbs

      case Pav_SUBU:     opc2 = 1024; break; // vsububm
      case Pav_QSUBU:    opc2 = 1536; break; // vsububs
      case Pav_QSUBS:    opc2 = 1792; break; // vsubsbs

      case Pav_OMULU:   opc2 =    8; break; // vmuloub
      case Pav_OMULS:   opc2 =  264; break; // vmulosb
      case Pav_EMULU:   opc2 =  520; break; // vmuleub
      case Pav_EMULS:   opc2 =  776; break; // vmulesb

      case Pav_AVGU:     opc2 = 1026; break; // vavgub
      case Pav_AVGS:     opc2 = 1282; break; // vavgsb
      case Pav_MAXU:     opc2 =    2; break; // vmaxub
      case Pav_MAXS:     opc2 =  258; break; // vmaxsb
      case Pav_MINU:     opc2 =  514; break; // vminub
      case Pav_MINS:     opc2 =  770; break; // vminsb

      case Pav_CMPEQU:   opc2 =    6; break; // vcmpequb
      case Pav_CMPGTU:   opc2 =  518; break; // vcmpgtub
      case Pav_CMPGTS:   opc2 =  774; break; // vcmpgtsb

      case Pav_SHL:      opc2 =  260; break; // vslb
      case Pav_SHR:      opc2 =  516; break; // vsrb
      case Pav_SAR:      opc2 =  772; break; // vsrab
      case Pav_ROTL:     opc2 =    4; break; // vrlb

      case Pav_MRGHI:    opc2 =   12; break; // vmrghb
      case Pav_MRGLO:    opc2 =  268; break; // vmrglb

      default:
         goto bad;
      }
      p = mkFormVX( p, 4, v_dst, v_srcL, v_srcR, opc2 );
      goto done;
   }

   case Pin_AvBin16x8: {
      UInt v_dst  = vregNo(i->Pin.AvBin16x8.dst);
      UInt v_srcL = vregNo(i->Pin.AvBin16x8.srcL);
      UInt v_srcR = vregNo(i->Pin.AvBin16x8.srcR);
      UInt opc2;
      switch (i->Pin.AvBin16x8.op) {

      case Pav_ADDU:    opc2 =   64; break; // vadduhm
      case Pav_QADDU:   opc2 =  576; break; // vadduhs
      case Pav_QADDS:   opc2 =  832; break; // vaddshs

      case Pav_SUBU:    opc2 = 1088; break; // vsubuhm
      case Pav_QSUBU:   opc2 = 1600; break; // vsubuhs
      case Pav_QSUBS:   opc2 = 1856; break; // vsubshs

      case Pav_OMULU:   opc2 =   72; break; // vmulouh
      case Pav_OMULS:   opc2 =  328; break; // vmulosh
      case Pav_EMULU:   opc2 =  584; break; // vmuleuh
      case Pav_EMULS:   opc2 =  840; break; // vmulesh

      case Pav_AVGU:    opc2 = 1090; break; // vavguh
      case Pav_AVGS:    opc2 = 1346; break; // vavgsh
      case Pav_MAXU:    opc2 =   66; break; // vmaxuh
      case Pav_MAXS:    opc2 =  322; break; // vmaxsh
      case Pav_MINS:    opc2 =  834; break; // vminsh
      case Pav_MINU:    opc2 =  578; break; // vminuh

      case Pav_CMPEQU:  opc2 =   70; break; // vcmpequh
      case Pav_CMPGTU:  opc2 =  582; break; // vcmpgtuh
      case Pav_CMPGTS:  opc2 =  838; break; // vcmpgtsh

      case Pav_SHL:     opc2 =  324; break; // vslh
      case Pav_SHR:     opc2 =  580; break; // vsrh
      case Pav_SAR:     opc2 =  836; break; // vsrah
      case Pav_ROTL:    opc2 =   68; break; // vrlh

      case Pav_PACKUU:  opc2 =   14; break; // vpkuhum
      case Pav_QPACKUU: opc2 =  142; break; // vpkuhus
      case Pav_QPACKSU: opc2 =  270; break; // vpkshus
      case Pav_QPACKSS: opc2 =  398; break; // vpkshss
      case Pav_PACKPXL: opc2 =  782; break; // vpkpx

      case Pav_MRGHI:   opc2 =   76; break; // vmrghh
      case Pav_MRGLO:   opc2 =  332; break; // vmrglh

      default:
         goto bad;
      }
      p = mkFormVX( p, 4, v_dst, v_srcL, v_srcR, opc2 );
      goto done;
   }

   case Pin_AvBin32x4: {
      UInt v_dst  = vregNo(i->Pin.AvBin32x4.dst);
      UInt v_srcL = vregNo(i->Pin.AvBin32x4.srcL);
      UInt v_srcR = vregNo(i->Pin.AvBin32x4.srcR);
      UInt opc2;
      switch (i->Pin.AvBin32x4.op) {

      case Pav_ADDU:    opc2 =  128; break; // vadduwm
      case Pav_QADDU:   opc2 =  640; break; // vadduws
      case Pav_QADDS:   opc2 =  896; break; // vaddsws

      case Pav_SUBU:    opc2 = 1152; break; // vsubuwm
      case Pav_QSUBU:   opc2 = 1664; break; // vsubuws
      case Pav_QSUBS:   opc2 = 1920; break; // vsubsws

      case Pav_AVGU:    opc2 = 1154; break; // vavguw
      case Pav_AVGS:    opc2 = 1410; break; // vavgsw

      case Pav_MAXU:    opc2 =  130; break; // vmaxuw
      case Pav_MAXS:    opc2 =  386; break; // vmaxsw

      case Pav_MINS:    opc2 =  898; break; // vminsw
      case Pav_MINU:    opc2 =  642; break; // vminuw

      case Pav_CMPEQU:  opc2 =  134; break; // vcmpequw
      case Pav_CMPGTS:  opc2 =  902; break; // vcmpgtsw
      case Pav_CMPGTU:  opc2 =  646; break; // vcmpgtuw

      case Pav_SHL:     opc2 =  388; break; // vslw
      case Pav_SHR:     opc2 =  644; break; // vsrw
      case Pav_SAR:     opc2 =  900; break; // vsraw
      case Pav_ROTL:    opc2 =  132; break; // vrlw

      case Pav_PACKUU:  opc2 =   78; break; // vpkuwum
      case Pav_QPACKUU: opc2 =  206; break; // vpkuwus
      case Pav_QPACKSU: opc2 =  334; break; // vpkswus
      case Pav_QPACKSS: opc2 =  462; break; // vpkswss

      case Pav_MRGHI:   opc2 =  140; break; // vmrghw
      case Pav_MRGLO:   opc2 =  396; break; // vmrglw

      default:
         goto bad;
      }
      p = mkFormVX( p, 4, v_dst, v_srcL, v_srcR, opc2 );
      goto done;
   }

   case Pin_AvBin32Fx4: {
      UInt v_dst  = vregNo(i->Pin.AvBin32Fx4.dst);
      UInt v_srcL = vregNo(i->Pin.AvBin32Fx4.srcL);
      UInt v_srcR = vregNo(i->Pin.AvBin32Fx4.srcR);
      switch (i->Pin.AvBin32Fx4.op) {

      case Pavfp_ADDF:
         p = mkFormVX( p, 4, v_dst, v_srcL, v_srcR, 10 );   // vaddfp
         break;
      case Pavfp_SUBF:
         p = mkFormVX( p, 4, v_dst, v_srcL, v_srcR, 74 );   // vsubfp
         break;
      case Pavfp_MAXF:
         p = mkFormVX( p, 4, v_dst, v_srcL, v_srcR, 1034 ); // vmaxfp
         break;
      case Pavfp_MINF:
         p = mkFormVX( p, 4, v_dst, v_srcL, v_srcR, 1098 ); // vminfp
         break;

      case Pavfp_MULF: {
         /* Make a vmulfp from a vmaddfp:
            load -0.0 (0x8000_0000) to each 32-bit word of vB
            this makes the add a noop.
         */
         UInt vB = 29;  // XXX: Using v29 for temp do not change
                        // without also changing
                        // getRegUsage_PPCInstr
         UInt konst = 0x1F;

         // Better way to load -0.0 (0x80000000) ?
         // vspltisw vB,0x1F   (0x1F => each word of vB)
         p = mkFormVX( p, 4, vB, konst, 0, 908 );

         // vslw vB,vB,vB (each word of vB = (0x1F << 0x1F) = 0x80000000
         p = mkFormVX( p, 4, vB, vB, vB, 388 );

         // Finally, do the multiply:
         p = mkFormVA( p, 4, v_dst, v_srcL, vB, v_srcR, 46 );
         break;
      }
      case Pavfp_CMPEQF:  // vcmpeqfp
         p = mkFormVXR( p, 4, v_dst, v_srcL, v_srcR, 0, 198 );
         break;
      case Pavfp_CMPGTF:  // vcmpgtfp
         p = mkFormVXR( p, 4, v_dst, v_srcL, v_srcR, 0, 710 );
         break;
      case Pavfp_CMPGEF:  // vcmpgefp
         p = mkFormVXR( p, 4, v_dst, v_srcL, v_srcR, 0, 454 );
         break;

      default:
         goto bad;
      }
      goto done;
   }

   case Pin_AvUn32Fx4: {
      UInt v_dst = vregNo(i->Pin.AvUn32Fx4.dst);
      UInt v_src = vregNo(i->Pin.AvUn32Fx4.src);
      UInt opc2;
      switch (i->Pin.AvUn32Fx4.op) {
      case Pavfp_RCPF:    opc2 =  266; break; // vrefp
      case Pavfp_RSQRTF:  opc2 =  330; break; // vrsqrtefp
      case Pavfp_CVTU2F:  opc2 =  778; break; // vcfux
      case Pavfp_CVTS2F:  opc2 =  842; break; // vcfsx
      case Pavfp_QCVTF2U: opc2 =  906; break; // vctuxs
      case Pavfp_QCVTF2S: opc2 =  970; break; // vctsxs
      case Pavfp_ROUNDM:  opc2 =  714; break; // vrfim
      case Pavfp_ROUNDP:  opc2 =  650; break; // vrfip
      case Pavfp_ROUNDN:  opc2 =  522; break; // vrfin
      case Pavfp_ROUNDZ:  opc2 =  586; break; // vrfiz
      default:
         goto bad;
      }
      p = mkFormVX( p, 4, v_dst, 0, v_src, opc2 );
      goto done;
   }

   case Pin_AvPerm: {  // vperm
      UInt v_dst  = vregNo(i->Pin.AvPerm.dst);
      UInt v_srcL = vregNo(i->Pin.AvPerm.srcL);
      UInt v_srcR = vregNo(i->Pin.AvPerm.srcR);
      UInt v_ctl  = vregNo(i->Pin.AvPerm.ctl);
      p = mkFormVA( p, 4, v_dst, v_srcL, v_srcR, v_ctl, 43 );
      goto done;
   }

   case Pin_AvSel: {  // vsel
      UInt v_ctl  = vregNo(i->Pin.AvSel.ctl);
      UInt v_dst  = vregNo(i->Pin.AvSel.dst);
      UInt v_srcL = vregNo(i->Pin.AvSel.srcL);
      UInt v_srcR = vregNo(i->Pin.AvSel.srcR);
      p = mkFormVA( p, 4, v_dst, v_srcL, v_srcR, v_ctl, 42 );
      goto done;
   }

   case Pin_AvShlDbl: {  // vsldoi
      UInt shift  = i->Pin.AvShlDbl.shift;
      UInt v_dst  = vregNo(i->Pin.AvShlDbl.dst);
      UInt v_srcL = vregNo(i->Pin.AvShlDbl.srcL);
      UInt v_srcR = vregNo(i->Pin.AvShlDbl.srcR);
      vassert(shift <= 0xF);
      p = mkFormVA( p, 4, v_dst, v_srcL, v_srcR, shift, 44 );
      goto done;
   }

   case Pin_AvSplat: { // vsplt(is)(b,h,w)
      UInt v_dst = vregNo(i->Pin.AvShlDbl.dst);
      UChar sz   = i->Pin.AvSplat.sz;
      UInt v_src, opc2;
      vassert(sz == 8 || sz == 16 || sz == 32);

      if (i->Pin.AvSplat.src->tag == Pvi_Imm) {
         Char simm5;
         opc2 = (sz == 8) ? 780 : (sz == 16) ? 844 : 908;   // 8,16,32
         /* expects 5-bit-signed-imm */
         simm5 = i->Pin.AvSplat.src->Pvi.Imm5s;
         vassert(simm5 >= -16 && simm5 <= 15);
         simm5 = simm5 & 0x1F;
         p = mkFormVX( p, 4, v_dst, (UInt)simm5, 0, opc2 );
      }
      else {  // Pri_Reg
         UInt lowest_lane;
         opc2 = (sz == 8) ? 524 : (sz == 16) ? 588 : 652;  // 8,16,32
         vassert(hregClass(i->Pin.AvSplat.src->Pvi.Reg) == HRcVec128);
         v_src = vregNo(i->Pin.AvSplat.src->Pvi.Reg);
         lowest_lane = (128/sz)-1;
         p = mkFormVX( p, 4, v_dst, lowest_lane, v_src, opc2 );
      }
      goto done;
   }

   case Pin_AvCMov: {
      UInt v_dst     = vregNo(i->Pin.AvCMov.dst);
      UInt v_src     = vregNo(i->Pin.AvCMov.src);
      PPCCondCode cc = i->Pin.AvCMov.cond;

      if (v_dst == v_src) goto done;
      
      vassert(cc.test != Pct_ALWAYS);

      /* jmp fwds 2 insns if !condition */
      if (cc.test != Pct_ALWAYS) {
         /* bc !ct,cf,n_bytes>>2 */
         p = mkFormB(p, invertCondTest(cc.test), cc.flag, 8>>2, 0, 0);
      }
      /* vmr */
      p = mkFormVX( p, 4, v_dst, v_src, v_src, 1156 );
      goto done;
   }

   case Pin_AvLdVSCR: {  // mtvscr
      UInt v_src = vregNo(i->Pin.AvLdVSCR.src);
      p = mkFormVX( p, 4, 0, 0, v_src, 1604 );
      goto done;
   }

   default: 
      goto bad;
   }

  bad:
   vex_printf("\n=> ");
   ppPPCInstr(i, mode64);
   vpanic("emit_PPCInstr");
   /*NOTREACHED*/
   
  done:
   vassert(p - &buf[0] <= 32);
   return p - &buf[0];
}

/*---------------------------------------------------------------*/
/*--- end                                     host_ppc_defs.c ---*/
/*---------------------------------------------------------------*/
