
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (host-ppc32/hdefs.c) is                       ---*/
/*--- Copyright (c) 2005 OpenWorks LLP.  All rights reserved. ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of LibVEX, a library for dynamic binary
   instrumentation and translation.

   Copyright (C) 2004-2005 OpenWorks, LLP.

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
#include "libvex.h"
#include "libvex_trc_values.h"

#include "main/vex_util.h"
#include "host-generic/h_generic_regs.h"
#include "host-ppc32/hdefs.h"


/* --------- Registers. --------- */

void ppHRegPPC32 ( HReg reg ) 
{
   Int r;
   static HChar* ireg32_names[32] 
     = { "%r0",  "%r1",  "%r2", "%r3", "%r4", "%r5", "%r6", "%r7",
	 "%r8",  "%r9",  "%r10", "%r11", "%r12", "%r13", "%r14", "%r15",
	 "%r16",  "%r17",  "%r18", "%r19", "%r20", "%r21", "%r22", "%r23",
	 "%r24",  "%r25",  "%r26", "%r27", "%r28", "%r29", "%r30", "%r31" };
   /* Be generic for all virtual regs. */
   if (hregIsVirtual(reg)) {
      ppHReg(reg);
      return;
   }
   /* But specific for real regs. */
   switch (hregClass(reg)) {
      case HRcInt32:
         r = hregNumber(reg);
         vassert(r >= 0 && r < 32);
         vex_printf("%s", ireg32_names[r]);
         return;
      case HRcFlt64:
         r = hregNumber(reg);
         vassert(r >= 0 && r < 6);
         vex_printf("%%fpr%d", r);
         return;
      default:
         vpanic("ppHRegPPC32");
   }
}

HReg hregPPC32_GPR0  ( void ) { return mkHReg( 0, HRcInt32, False); }
HReg hregPPC32_GPR1  ( void ) { return mkHReg( 1, HRcInt32, False); }
HReg hregPPC32_GPR2  ( void ) { return mkHReg( 2, HRcInt32, False); }
HReg hregPPC32_GPR3  ( void ) { return mkHReg( 3, HRcInt32, False); }
HReg hregPPC32_GPR4  ( void ) { return mkHReg( 4, HRcInt32, False); }
HReg hregPPC32_GPR5  ( void ) { return mkHReg( 5, HRcInt32, False); }
HReg hregPPC32_GPR6  ( void ) { return mkHReg( 6, HRcInt32, False); }
HReg hregPPC32_GPR7  ( void ) { return mkHReg( 7, HRcInt32, False); }
HReg hregPPC32_GPR8  ( void ) { return mkHReg( 8, HRcInt32, False); }
HReg hregPPC32_GPR9  ( void ) { return mkHReg( 9, HRcInt32, False); }
HReg hregPPC32_GPR10 ( void ) { return mkHReg(10, HRcInt32, False); }
HReg hregPPC32_GPR11 ( void ) { return mkHReg(11, HRcInt32, False); }
HReg hregPPC32_GPR12 ( void ) { return mkHReg(12, HRcInt32, False); }
HReg hregPPC32_GPR13 ( void ) { return mkHReg(13, HRcInt32, False); }
HReg hregPPC32_GPR14 ( void ) { return mkHReg(14, HRcInt32, False); }
HReg hregPPC32_GPR15 ( void ) { return mkHReg(15, HRcInt32, False); }
HReg hregPPC32_GPR16 ( void ) { return mkHReg(16, HRcInt32, False); }
HReg hregPPC32_GPR17 ( void ) { return mkHReg(17, HRcInt32, False); }
HReg hregPPC32_GPR18 ( void ) { return mkHReg(18, HRcInt32, False); }
HReg hregPPC32_GPR19 ( void ) { return mkHReg(19, HRcInt32, False); }
HReg hregPPC32_GPR20 ( void ) { return mkHReg(20, HRcInt32, False); }
HReg hregPPC32_GPR21 ( void ) { return mkHReg(21, HRcInt32, False); }
HReg hregPPC32_GPR22 ( void ) { return mkHReg(22, HRcInt32, False); }
HReg hregPPC32_GPR23 ( void ) { return mkHReg(23, HRcInt32, False); }
HReg hregPPC32_GPR24 ( void ) { return mkHReg(24, HRcInt32, False); }
HReg hregPPC32_GPR25 ( void ) { return mkHReg(25, HRcInt32, False); }
HReg hregPPC32_GPR26 ( void ) { return mkHReg(26, HRcInt32, False); }
HReg hregPPC32_GPR27 ( void ) { return mkHReg(27, HRcInt32, False); }
HReg hregPPC32_GPR28 ( void ) { return mkHReg(28, HRcInt32, False); }
HReg hregPPC32_GPR29 ( void ) { return mkHReg(29, HRcInt32, False); }
HReg hregPPC32_GPR30 ( void ) { return mkHReg(30, HRcInt32, False); }
HReg hregPPC32_GPR31 ( void ) { return mkHReg(31, HRcInt32, False); }

HReg hregPPC32_FPR0  ( void ) { return mkHReg( 0, HRcFlt64, False); }
HReg hregPPC32_FPR1  ( void ) { return mkHReg( 1, HRcFlt64, False); }
HReg hregPPC32_FPR2  ( void ) { return mkHReg( 2, HRcFlt64, False); }
HReg hregPPC32_FPR3  ( void ) { return mkHReg( 3, HRcFlt64, False); }
HReg hregPPC32_FPR4  ( void ) { return mkHReg( 4, HRcFlt64, False); }
HReg hregPPC32_FPR5  ( void ) { return mkHReg( 5, HRcFlt64, False); }
HReg hregPPC32_FPR6  ( void ) { return mkHReg( 6, HRcFlt64, False); }
HReg hregPPC32_FPR7  ( void ) { return mkHReg( 7, HRcFlt64, False); }
HReg hregPPC32_FPR8  ( void ) { return mkHReg( 8, HRcFlt64, False); }
HReg hregPPC32_FPR9  ( void ) { return mkHReg( 9, HRcFlt64, False); }
HReg hregPPC32_FPR10 ( void ) { return mkHReg(10, HRcFlt64, False); }
HReg hregPPC32_FPR11 ( void ) { return mkHReg(11, HRcFlt64, False); }
HReg hregPPC32_FPR12 ( void ) { return mkHReg(12, HRcFlt64, False); }
HReg hregPPC32_FPR13 ( void ) { return mkHReg(13, HRcFlt64, False); }
HReg hregPPC32_FPR14 ( void ) { return mkHReg(14, HRcFlt64, False); }
HReg hregPPC32_FPR15 ( void ) { return mkHReg(15, HRcFlt64, False); }
HReg hregPPC32_FPR16 ( void ) { return mkHReg(16, HRcFlt64, False); }
HReg hregPPC32_FPR17 ( void ) { return mkHReg(17, HRcFlt64, False); }
HReg hregPPC32_FPR18 ( void ) { return mkHReg(18, HRcFlt64, False); }
HReg hregPPC32_FPR19 ( void ) { return mkHReg(19, HRcFlt64, False); }
HReg hregPPC32_FPR20 ( void ) { return mkHReg(20, HRcFlt64, False); }
HReg hregPPC32_FPR21 ( void ) { return mkHReg(21, HRcFlt64, False); }
HReg hregPPC32_FPR22 ( void ) { return mkHReg(22, HRcFlt64, False); }
HReg hregPPC32_FPR23 ( void ) { return mkHReg(23, HRcFlt64, False); }
HReg hregPPC32_FPR24 ( void ) { return mkHReg(24, HRcFlt64, False); }
HReg hregPPC32_FPR25 ( void ) { return mkHReg(25, HRcFlt64, False); }
HReg hregPPC32_FPR26 ( void ) { return mkHReg(26, HRcFlt64, False); }
HReg hregPPC32_FPR27 ( void ) { return mkHReg(27, HRcFlt64, False); }
HReg hregPPC32_FPR28 ( void ) { return mkHReg(28, HRcFlt64, False); }
HReg hregPPC32_FPR29 ( void ) { return mkHReg(29, HRcFlt64, False); }
HReg hregPPC32_FPR30 ( void ) { return mkHReg(30, HRcFlt64, False); }
HReg hregPPC32_FPR31 ( void ) { return mkHReg(31, HRcFlt64, False); }


void getAllocableRegs_PPC32 ( Int* nregs, HReg** arr )
{
   *nregs = 64;
   *arr = LibVEX_Alloc(*nregs * sizeof(HReg));
   (*arr)[ 0] = hregPPC32_GPR0();
   (*arr)[ 1] = hregPPC32_GPR1();
   (*arr)[ 2] = hregPPC32_GPR2();
   (*arr)[ 3] = hregPPC32_GPR3();
   (*arr)[ 4] = hregPPC32_GPR4();
   (*arr)[ 5] = hregPPC32_GPR5();
   (*arr)[ 6] = hregPPC32_GPR6();
   (*arr)[ 7] = hregPPC32_GPR7();
   (*arr)[ 8] = hregPPC32_GPR8();
   (*arr)[ 9] = hregPPC32_GPR9();
   (*arr)[10] = hregPPC32_GPR10();
   (*arr)[11] = hregPPC32_GPR11();
   (*arr)[12] = hregPPC32_GPR12();
   (*arr)[13] = hregPPC32_GPR13();
   (*arr)[14] = hregPPC32_GPR14();
   (*arr)[15] = hregPPC32_GPR15();
   (*arr)[16] = hregPPC32_GPR16();
   (*arr)[17] = hregPPC32_GPR17();
   (*arr)[18] = hregPPC32_GPR18();
   (*arr)[19] = hregPPC32_GPR19();
   (*arr)[20] = hregPPC32_GPR20();
   (*arr)[21] = hregPPC32_GPR21();
   (*arr)[22] = hregPPC32_GPR22();
   (*arr)[23] = hregPPC32_GPR23();
   (*arr)[24] = hregPPC32_GPR24();
   (*arr)[25] = hregPPC32_GPR25();
   (*arr)[26] = hregPPC32_GPR26();
   (*arr)[27] = hregPPC32_GPR27();
   (*arr)[28] = hregPPC32_GPR28();
   (*arr)[29] = hregPPC32_GPR29();
   (*arr)[30] = hregPPC32_GPR30();
   (*arr)[31] = hregPPC32_GPR31();

   (*arr)[32] = hregPPC32_FPR0();
   (*arr)[33] = hregPPC32_FPR1();
   (*arr)[34] = hregPPC32_FPR2();
   (*arr)[35] = hregPPC32_FPR3();
   (*arr)[36] = hregPPC32_FPR4();
   (*arr)[37] = hregPPC32_FPR5();
   (*arr)[38] = hregPPC32_FPR6();
   (*arr)[39] = hregPPC32_FPR7();
   (*arr)[40] = hregPPC32_FPR8();
   (*arr)[41] = hregPPC32_FPR9();
   (*arr)[42] = hregPPC32_FPR10();
   (*arr)[43] = hregPPC32_FPR11();
   (*arr)[44] = hregPPC32_FPR12();
   (*arr)[45] = hregPPC32_FPR13();
   (*arr)[46] = hregPPC32_FPR14();
   (*arr)[47] = hregPPC32_FPR15();
   (*arr)[48] = hregPPC32_FPR16();
   (*arr)[49] = hregPPC32_FPR17();
   (*arr)[50] = hregPPC32_FPR18();
   (*arr)[51] = hregPPC32_FPR19();
   (*arr)[52] = hregPPC32_FPR20();
   (*arr)[53] = hregPPC32_FPR21();
   (*arr)[54] = hregPPC32_FPR22();
   (*arr)[55] = hregPPC32_FPR23();
   (*arr)[56] = hregPPC32_FPR24();
   (*arr)[57] = hregPPC32_FPR25();
   (*arr)[58] = hregPPC32_FPR26();
   (*arr)[59] = hregPPC32_FPR27();
   (*arr)[60] = hregPPC32_FPR28();
   (*arr)[61] = hregPPC32_FPR29();
   (*arr)[62] = hregPPC32_FPR30();
   (*arr)[63] = hregPPC32_FPR31();
}


/* --------- PPCAMode: memory address expressions. --------- */

PPC32AMode* PPC32AMode_IR ( UInt idx, HReg base ) {
   PPC32AMode* am = LibVEX_Alloc(sizeof(PPC32AMode));
   am->tag = Pam_IR;
   am->Pam.IR.base = base;
   am->Pam.IR.index = idx;
   return am;
}
PPC32AMode* PPC32AMode_RR ( HReg idx, HReg base ) {
   PPC32AMode* am = LibVEX_Alloc(sizeof(PPC32AMode));
   am->tag = Pam_RR;
   am->Pam.RR.base = base;
   am->Pam.RR.index = idx;
   return am;
}

PPC32AMode* dopyPPC32AMode ( PPC32AMode* am ) {
   switch (am->tag) {
      case Pam_IR: 
         return PPC32AMode_IR( am->Pam.IR.index, am->Pam.IR.base );
      case Pam_RR: 
         return PPC32AMode_RR( am->Pam.RR.index, am->Pam.RR.base );
      default:
         vpanic("dopyPPC32AMode");
   }
}

void ppPPC32AMode ( PPC32AMode* am ) {
   switch (am->tag) {
      case Pam_IR: 
         if (am->Pam.IR.index == 0)
            vex_printf("(");
         else
            vex_printf("0x%x(", am->Pam.IR.index);
         ppHRegPPC32(am->Pam.IR.base);
         vex_printf(")");
         return;
      case Pam_RR:
         ppHRegPPC32(am->Pam.RR.base);
         vex_printf(",");
         ppHRegPPC32(am->Pam.RR.index);
         return;
      default:
         vpanic("ppPPC32AMode");
   }
}

#if 0
static void addRegUsage_PPC32AMode ( HRegUsage* u, PPC32AMode* am ) {
   switch (am->tag) {
      case Pam_IR: 
         addHRegUse(u, HRmRead, am->Pam.IR.base);
         return;
      case Pam_RR:
         addHRegUse(u, HRmRead, am->Pam.RR.base);
         addHRegUse(u, HRmRead, am->Pam.RR.index);
         return;
      default:
         vpanic("addRegUsage_PPC32AMode");
   }
}
#endif

#if 0
static void mapRegs_PPC32AMode ( HRegRemap* m, PPC32AMode* am ) {
   switch (am->tag) {
      case Pam_IR: 
         am->Pam.IR.base = lookupHRegRemap(m, am->Pam.IR.base);
         return;
      case Pam_RR:
         am->Pam.RR.base = lookupHRegRemap(m, am->Pam.RR.base);
         am->Pam.RR.index = lookupHRegRemap(m, am->Pam.RR.index);
         return;
      default:
         vpanic("mapRegs_PPC32AMode");
   }
}
#endif

/* --------- Operand, which can be reg or immediate only. --------- */

PPC32RI* PPC32RI_Imm ( UInt imm32 ) {
   PPC32RI* op       = LibVEX_Alloc(sizeof(PPC32RI));
   op->tag           = Pri_Imm;
   op->Pri.Imm.imm32 = imm32;
   return op;
}
PPC32RI* PPC32RI_Reg ( HReg reg ) {
   PPC32RI* op     = LibVEX_Alloc(sizeof(PPC32RI));
   op->tag         = Pri_Reg;
   op->Pri.Reg.reg = reg;
   return op;
}

void ppPPC32RI ( PPC32RI* op ) {
   switch (op->tag) {
      case Pri_Imm: 
         vex_printf("$0x%x", op->Pri.Imm.imm32);
         return;
      case Pri_Reg: 
         ppHRegPPC32(op->Pri.Reg.reg);
         return;
     default: 
         vpanic("ppPPC32RI");
   }
}

/* An PPC32RI can only be used in a "read" context (what would it mean
   to write or modify a literal?) and so we enumerate its registers
   accordingly. */
static void addRegUsage_PPC32RI ( HRegUsage* u, PPC32RI* op ) {
   switch (op->tag) {
      case Pri_Imm: 
         return;
      case Pri_Reg: 
         addHRegUse(u, HRmRead, op->Pri.Reg.reg);
         return;
      default: 
         vpanic("addRegUsage_PPC32RI");
   }
}

static void mapRegs_PPC32RI ( HRegRemap* m, PPC32RI* op ) {
   switch (op->tag) {
      case Pri_Imm: 
         return;
      case Pri_Reg: 
         op->Pri.Reg.reg = lookupHRegRemap(m, op->Pri.Reg.reg);
         return;
      default: 
         vpanic("mapRegs_PPC32RI");
   }
}

/* --------- Instructions. --------- */

//.. HChar* showX86ScalarSz ( X86ScalarSz sz ) {
//..    switch (sz) {
//..       case Xss_16: return "w";
//..       case Xss_32: return "l";
//..       default: vpanic("showX86ScalarSz");
//..    }
//.. }

//.. HChar* showX86UnaryOp ( X86UnaryOp op ) {
//..    switch (op) {
//..       case Xun_NOT: return "not";
//..       case Xun_NEG: return "neg";
//..       default: vpanic("showX86UnaryOp");
//..    }
//.. }

HChar* showPPC32AluOp ( PPC32AluOp op ) {
   switch (op) {
      case Palu_CMP:  return "cmp";
      case Palu_ADD:  return "add";
      case Palu_SUB:  return "sub";
      case Palu_ADC:  return "adc";
      case Palu_SBB:  return "sbb";
      case Palu_AND:  return "and";
      case Palu_OR:   return "or";
      case Palu_XOR:  return "xor";
      case Palu_MUL:  return "mul";
      default: vpanic("showPPC32AluOp");
   }
}

HChar* showPPC32ShiftOp ( PPC32ShiftOp op ) {
   switch (op) {
      case Psh_SHL: return "shl";
      case Psh_SHR: return "shr";
      case Psh_SAR: return "sar";
      case Psh_ROL: return "rol";
      default: vpanic("showPPC32ShiftOp");
   }
}

//.. HChar* showX86FpOp ( X86FpOp op ) {
//..    switch (op) {
//..       case Xfp_ADD:    return "add";
//..       case Xfp_SUB:    return "sub";
//..       case Xfp_MUL:    return "mul";
//..       case Xfp_DIV:    return "div";
//..       case Xfp_SCALE:  return "scale";
//..       case Xfp_ATAN:   return "atan";
//..       case Xfp_YL2X:   return "yl2x";
//..       case Xfp_YL2XP1: return "yl2xp1";
//..       case Xfp_PREM:   return "prem";
//..       case Xfp_PREM1:  return "prem1";
//..       case Xfp_SQRT:   return "sqrt";
//..       case Xfp_ABS:    return "abs";
//..       case Xfp_NEG:    return "chs";
//..       case Xfp_MOV:    return "mov";
//..       case Xfp_SIN:    return "sin";
//..       case Xfp_COS:    return "cos";
//..       case Xfp_TAN:    return "tan";
//..       case Xfp_ROUND:  return "round";
//..       case Xfp_2XM1:   return "2xm1";
//..       default: vpanic("showX86FpOp");
//..    }
//.. }

PPC32Instr* PPC32Instr_Alu32 ( PPC32AluOp op, HReg dst, HReg src1, PPC32RI* src2 ) {
   PPC32Instr* i     = LibVEX_Alloc(sizeof(PPC32Instr));
   i->tag            = Pin_Alu32;
   i->Pin.Alu32.op   = op;
   i->Pin.Alu32.dst  = dst;
   i->Pin.Alu32.src1 = src1;
   i->Pin.Alu32.src2 = src2;
   return i;
}
PPC32Instr* PPC32Instr_Sh32 ( PPC32ShiftOp op, HReg dst, HReg src, PPC32RI* shft ) {
   PPC32Instr* i    = LibVEX_Alloc(sizeof(PPC32Instr));
   i->tag           = Pin_Sh32;
   i->Pin.Sh32.op   = op;
   i->Pin.Sh32.dst  = dst;
   i->Pin.Sh32.src  = src;
   i->Pin.Sh32.shft = shft;
   return i;
}
//.. X86Instr* X86Instr_Test32  ( X86RI* src, X86RM* dst ) {
//..    X86Instr* i       = LibVEX_Alloc(sizeof(X86Instr));
//..    i->tag            = Xin_Test32;
//..    i->Xin.Test32.src = src;
//..    i->Xin.Test32.dst = dst;
//..    return i;
//.. }
//.. X86Instr* X86Instr_Unary32  ( X86UnaryOp op, X86RM* dst ) {
//..    X86Instr* i        = LibVEX_Alloc(sizeof(X86Instr));
//..    i->tag             = Xin_Unary32;
//..    i->Xin.Unary32.op  = op;
//..    i->Xin.Unary32.dst = dst;
//..    return i;
//.. }
//.. X86Instr* X86Instr_MulL ( Bool syned, X86ScalarSz ssz , X86RM* src ) {
//..    X86Instr* i        = LibVEX_Alloc(sizeof(X86Instr));
//..    i->tag             = Xin_MulL;
//..    i->Xin.MulL.syned  = syned;
//..    i->Xin.MulL.ssz    = ssz;
//..    i->Xin.MulL.src    = src;
//..    return i;
//.. }
//.. X86Instr* X86Instr_Div ( Bool syned, X86ScalarSz ssz, X86RM* src ) {
//..    X86Instr* i        = LibVEX_Alloc(sizeof(X86Instr));
//..    i->tag             = Xin_Div;
//..    i->Xin.Div.syned   = syned;
//..    i->Xin.Div.ssz     = ssz;
//..    i->Xin.Div.src     = src;
//..    return i;
//.. }
//.. X86Instr* X86Instr_Sh3232  ( X86ShiftOp op, UInt amt, HReg src, HReg dst ) {
//..    X86Instr* i       = LibVEX_Alloc(sizeof(X86Instr));
//..    i->tag            = Xin_Sh3232;
//..    i->Xin.Sh3232.op  = op;
//..    i->Xin.Sh3232.amt = amt;
//..    i->Xin.Sh3232.src = src;
//..    i->Xin.Sh3232.dst = dst;
//..    vassert(op == Xsh_SHL || op == Xsh_SHR);
//..    return i;
//.. }
//.. X86Instr* X86Instr_Push( X86RMI* src ) {
//..    X86Instr* i     = LibVEX_Alloc(sizeof(X86Instr));
//..    i->tag          = Xin_Push;
//..    i->Xin.Push.src = src;
//..    return i;
//.. }
//.. X86Instr* X86Instr_Call ( X86CondCode cond, Addr32 target, Int regparms ) {
//..    X86Instr* i          = LibVEX_Alloc(sizeof(X86Instr));
//..    i->tag               = Xin_Call;
//..    i->Xin.Call.cond     = cond;
//..    i->Xin.Call.target   = target;
//..    i->Xin.Call.regparms = regparms;
//..    vassert(regparms >= 0 && regparms <= 3);
//..    return i;
//.. }
//.. X86Instr* X86Instr_Goto ( IRJumpKind jk, X86CondCode cond, X86RI* dst ) {
//..    X86Instr* i      = LibVEX_Alloc(sizeof(X86Instr));
//..    i->tag           = Xin_Goto;
//..    i->Xin.Goto.cond = cond;
//..    i->Xin.Goto.dst  = dst;
//..    i->Xin.Goto.jk   = jk;
//..    return i;
//.. }
//.. X86Instr* X86Instr_CMov32  ( X86CondCode cond, X86RM* src, HReg dst ) {
//..    X86Instr* i        = LibVEX_Alloc(sizeof(X86Instr));
//..    i->tag             = Xin_CMov32;
//..    i->Xin.CMov32.cond = cond;
//..    i->Xin.CMov32.src  = src;
//..    i->Xin.CMov32.dst  = dst;
//..    vassert(cond != Xcc_ALWAYS);
//..    return i;
//.. }
PPC32Instr* PPC32Instr_LoadEX ( UChar szSmall, Bool syned,
				HReg dst, PPC32AMode* src ) {
   PPC32Instr* i         = LibVEX_Alloc(sizeof(PPC32Instr));
   i->tag                = Pin_LoadEX;
   i->Pin.LoadEX.szSmall = szSmall;
   i->Pin.LoadEX.syned   = syned;
   i->Pin.LoadEX.src     = src;
   i->Pin.LoadEX.dst     = dst;
   vassert(szSmall == 1 || szSmall == 2 || szSmall == 4);
   return i;
}
PPC32Instr* PPC32Instr_Store ( UChar sz, PPC32AMode* dst, HReg src ) {
   PPC32Instr* i    = LibVEX_Alloc(sizeof(PPC32Instr));
   i->tag           = Pin_Store;
   i->Pin.Store.sz  = sz;
   i->Pin.Store.src = src;
   i->Pin.Store.dst = dst;
   vassert(sz == 1 || sz == 2 || sz == 4);
   return i;
}
//.. X86Instr* X86Instr_Set32 ( X86CondCode cond, HReg dst ) {
//..    X86Instr* i       = LibVEX_Alloc(sizeof(X86Instr));
//..    i->tag            = Xin_Set32;
//..    i->Xin.Set32.cond = cond;
//..    i->Xin.Set32.dst  = dst;
//..    return i;
//.. }
//.. X86Instr* X86Instr_Bsfr32 ( Bool isFwds, HReg src, HReg dst ) {
//..    X86Instr* i          = LibVEX_Alloc(sizeof(X86Instr));
//..    i->tag               = Xin_Bsfr32;
//..    i->Xin.Bsfr32.isFwds = isFwds;
//..    i->Xin.Bsfr32.src    = src;
//..    i->Xin.Bsfr32.dst    = dst;
//..    return i;
//.. }
//.. X86Instr* X86Instr_MFence ( VexSubArch subarch )
//.. {
//..    X86Instr* i           = LibVEX_Alloc(sizeof(X86Instr));
//..    i->tag                = Xin_MFence;
//..    i->Xin.MFence.subarch = subarch;
//..    vassert(subarch == VexSubArchX86_sse0
//..            || subarch == VexSubArchX86_sse1
//..            || subarch == VexSubArchX86_sse2);
//..    return i;
//.. }

//.. X86Instr* X86Instr_FpUnary ( X86FpOp op, HReg src, HReg dst ) {
//..    X86Instr* i        = LibVEX_Alloc(sizeof(X86Instr));
//..    i->tag             = Xin_FpUnary;
//..    i->Xin.FpUnary.op  = op;
//..    i->Xin.FpUnary.src = src;
//..    i->Xin.FpUnary.dst = dst;
//..    return i;
//.. }
//.. X86Instr* X86Instr_FpBinary ( X86FpOp op, HReg srcL, HReg srcR, HReg dst ) {
//..    X86Instr* i          = LibVEX_Alloc(sizeof(X86Instr));
//..    i->tag               = Xin_FpBinary;
//..    i->Xin.FpBinary.op   = op;
//..    i->Xin.FpBinary.srcL = srcL;
//..    i->Xin.FpBinary.srcR = srcR;
//..    i->Xin.FpBinary.dst  = dst;
//..    return i;
//.. }
//.. X86Instr* X86Instr_FpLdSt ( Bool isLoad, UChar sz, HReg reg, X86AMode* addr ) {
//..    X86Instr* i          = LibVEX_Alloc(sizeof(X86Instr));
//..    i->tag               = Xin_FpLdSt;
//..    i->Xin.FpLdSt.isLoad = isLoad;
//..    i->Xin.FpLdSt.sz     = sz;
//..    i->Xin.FpLdSt.reg    = reg;
//..    i->Xin.FpLdSt.addr   = addr;
//..    vassert(sz == 4 || sz == 8);
//..    return i;
//.. }
//.. X86Instr* X86Instr_FpLdStI ( Bool isLoad, UChar sz,  
//..                              HReg reg, X86AMode* addr ) {
//..    X86Instr* i           = LibVEX_Alloc(sizeof(X86Instr));
//..    i->tag                = Xin_FpLdStI;
//..    i->Xin.FpLdStI.isLoad = isLoad;
//..    i->Xin.FpLdStI.sz     = sz;
//..    i->Xin.FpLdStI.reg    = reg;
//..    i->Xin.FpLdStI.addr   = addr;
//..    vassert(sz == 2 || sz == 4 || sz == 8);
//..    return i;
//.. }
//.. X86Instr* X86Instr_Fp64to32 ( HReg src, HReg dst ) {
//..    X86Instr* i         = LibVEX_Alloc(sizeof(X86Instr));
//..    i->tag              = Xin_Fp64to32;
//..    i->Xin.Fp64to32.src = src;
//..    i->Xin.Fp64to32.dst = dst;
//..    return i;
//.. }
//.. X86Instr* X86Instr_FpCMov ( X86CondCode cond, HReg src, HReg dst ) {
//..    X86Instr* i        = LibVEX_Alloc(sizeof(X86Instr));
//..    i->tag             = Xin_FpCMov;
//..    i->Xin.FpCMov.cond = cond;
//..    i->Xin.FpCMov.src  = src;
//..    i->Xin.FpCMov.dst  = dst;
//..    vassert(cond != Xcc_ALWAYS);
//..    return i;
//.. }
//.. X86Instr* X86Instr_FpLdStCW ( Bool isLoad, X86AMode* addr ) {
//..    X86Instr* i            = LibVEX_Alloc(sizeof(X86Instr));
//..    i->tag                 = Xin_FpLdStCW;
//..    i->Xin.FpLdStCW.isLoad = isLoad;
//..    i->Xin.FpLdStCW.addr   = addr;
//..    return i;
//.. }
//.. X86Instr* X86Instr_FpStSW_AX ( void ) {
//..    X86Instr* i = LibVEX_Alloc(sizeof(X86Instr));
//..    i->tag      = Xin_FpStSW_AX;
//..    return i;
//.. }
//.. X86Instr* X86Instr_FpCmp ( HReg srcL, HReg srcR, HReg dst ) {
//..    X86Instr* i       = LibVEX_Alloc(sizeof(X86Instr));
//..    i->tag            = Xin_FpCmp;
//..    i->Xin.FpCmp.srcL = srcL;
//..    i->Xin.FpCmp.srcR = srcR;
//..    i->Xin.FpCmp.dst  = dst;
//..    return i;
//.. }

void ppPPC32Instr ( PPC32Instr* i )
{
   switch (i->tag) {
      case Pin_Alu32:
         vex_printf("%sl ", showPPC32AluOp(i->Pin.Alu32.op));
         ppHRegPPC32(i->Pin.Alu32.dst);
         vex_printf(",");
         ppHRegPPC32(i->Pin.Alu32.src1);
         vex_printf(",");
         ppPPC32RI(i->Pin.Alu32.src2);
         return;
      case Pin_Sh32:
         vex_printf("%sl ", showPPC32ShiftOp(i->Pin.Sh32.op));
         ppHRegPPC32(i->Pin.Sh32.dst);
         vex_printf(",");
         ppHRegPPC32(i->Pin.Sh32.src);
         vex_printf(",");
         ppPPC32RI(i->Pin.Sh32.shft);
         return;
//..       case Xin_Test32:
//..          vex_printf("testl ");
//..          ppX86RI(i->Xin.Test32.src);
//..          vex_printf(",");
//..          ppX86RM(i->Xin.Test32.dst);
//..          return;
//..       case Xin_Unary32:
//..          vex_printf("%sl ", showX86UnaryOp(i->Xin.Unary32.op));
//..          ppX86RM(i->Xin.Unary32.dst);
//..          return;
//..       case Xin_MulL:
//..          vex_printf("%cmul%s ",
//..                     i->Xin.MulL.syned ? 's' : 'u',
//..                     showX86ScalarSz(i->Xin.MulL.ssz));
//..          ppX86RM(i->Xin.MulL.src);
//..          return;
//..       case Xin_Div:
//..          vex_printf("%cdiv%s ",
//..                     i->Xin.Div.syned ? 's' : 'u',
//..                     showX86ScalarSz(i->Xin.Div.ssz));
//..          ppX86RM(i->Xin.Div.src);
//..          return;
//..       case Xin_Sh3232:
//..          vex_printf("%sdl ", showX86ShiftOp(i->Xin.Sh3232.op));
//..          if (i->Xin.Sh3232.amt == 0)
//..            vex_printf(" %%cl,"); 
//..          else 
//..             vex_printf(" $%d,", i->Xin.Sh3232.amt);
//..          ppHRegX86(i->Xin.Sh3232.src);
//..          vex_printf(",");
//..          ppHRegX86(i->Xin.Sh3232.dst);
//..          return;
//..       case Xin_Push:
//..          vex_printf("pushl ");
//..          ppX86RMI(i->Xin.Push.src);
//..          return;
//..       case Xin_Call:
//..          vex_printf("call%s[%d] ", 
//..                     i->Xin.Call.cond==Xcc_ALWAYS 
//..                        ? "" : showX86CondCode(i->Xin.Call.cond), 
//..                     i->Xin.Call.regparms);
//..          vex_printf("0x%x", i->Xin.Call.target);
//..          break;
//..       case Xin_Goto:
//..          if (i->Xin.Goto.cond != Xcc_ALWAYS) {
//..             vex_printf("if (%%eflags.%s) { ", 
//..                        showX86CondCode(i->Xin.Goto.cond));
//.. 	 }
//..          if (i->Xin.Goto.jk != Ijk_Boring) {
//..             vex_printf("movl $");
//..             ppIRJumpKind(i->Xin.Goto.jk);
//..             vex_printf(",%%ebp ; ");
//..          }
//..          vex_printf("movl ");
//..          ppX86RI(i->Xin.Goto.dst);
//..          vex_printf(",%%eax ; ret");
//..          if (i->Xin.Goto.cond != Xcc_ALWAYS) {
//..             vex_printf(" }");
//.. 	 }
//..          return;
//..       case Xin_CMov32:
//..          vex_printf("cmov%s ", showX86CondCode(i->Xin.CMov32.cond));
//..          ppX86RM(i->Xin.CMov32.src);
//..          vex_printf(",");
//..          ppHRegX86(i->Xin.CMov32.dst);
//..          return;
       case Pin_LoadEX: {
	 UChar sz = i->Pin.LoadEX.szSmall;
	 Bool syned = i->Pin.LoadEX.syned;
	 Bool update = False;                     // CAB: ?
	 Bool idxd = (i->Pin.LoadEX.src->tag == Pam_IR) ? True : False;

         vex_printf("l%c%c%s%s ",
                    (sz==1) ? 'b' : sz==1 ? 'h' : 'w',
                    syned ? 'a' : 'z',
		    update ? "u" : "",
		    idxd ? "x" : "" );

         ppHRegPPC32(i->Pin.LoadEX.dst);
         vex_printf(",");
         ppPPC32AMode(i->Pin.LoadEX.src);
         return;
       }
       case Pin_Store: {
         UChar sz = i->Pin.Store.sz;
	 Bool update = False;                     // CAB: ?
	 Bool idxd = (i->Pin.Store.dst->tag == Pam_IR) ? True : False;

         vex_printf("st%c%s%s ",
                    (sz==1) ? 'b' : sz==1 ? 'h' : 'w',
		    update ? "u" : "",
		    idxd ? "x" : "" );

         ppHRegPPC32(i->Pin.Store.src);
         vex_printf(",");
         ppPPC32AMode(i->Pin.Store.dst);
         return;
       }
#if 0
         vex_printf("mov%c ", i->Xin.Store.sz==1 ? 'b' : 'w');
         ppHRegX86(i->Xin.Store.src);
         vex_printf(",");
         ppX86AMode(i->Xin.Store.dst);
#endif
         return;
//..       case Xin_Set32:
//..          vex_printf("setl%s ", showX86CondCode(i->Xin.Set32.cond));
//..          ppHRegX86(i->Xin.Set32.dst);
//..          return;
//..       case Xin_Bsfr32:
//..          vex_printf("bs%cl ", i->Xin.Bsfr32.isFwds ? 'f' : 'r');
//..          ppHRegX86(i->Xin.Bsfr32.src);
//..          vex_printf(",");
//..          ppHRegX86(i->Xin.Bsfr32.dst);
//..          return;
//..       case Xin_MFence:
//..          vex_printf("mfence(%s)",
//..                     LibVEX_ppVexSubArch(i->Xin.MFence.subarch));
//..          return;
//..       case Xin_FpUnary:
//..          vex_printf("g%sD ", showX86FpOp(i->Xin.FpUnary.op));
//..          ppHRegX86(i->Xin.FpUnary.src);
//..          vex_printf(",");
//..          ppHRegX86(i->Xin.FpUnary.dst);
//..          break;
//..       case Xin_FpBinary:
//..          vex_printf("g%sD ", showX86FpOp(i->Xin.FpBinary.op));
//..          ppHRegX86(i->Xin.FpBinary.srcL);
//..          vex_printf(",");
//..          ppHRegX86(i->Xin.FpBinary.srcR);
//..          vex_printf(",");
//..          ppHRegX86(i->Xin.FpBinary.dst);
//..          break;
//..       case Xin_FpLdSt:
//..          if (i->Xin.FpLdSt.isLoad) {
//..             vex_printf("gld%c " , i->Xin.FpLdSt.sz==8 ? 'D' : 'F');
//..             ppX86AMode(i->Xin.FpLdSt.addr);
//..             vex_printf(", ");
//..             ppHRegX86(i->Xin.FpLdSt.reg);
//..          } else {
//..             vex_printf("gst%c " , i->Xin.FpLdSt.sz==8 ? 'D' : 'F');
//..             ppHRegX86(i->Xin.FpLdSt.reg);
//..             vex_printf(", ");
//..             ppX86AMode(i->Xin.FpLdSt.addr);
//..          }
//..          return;
//..       case Xin_FpLdStI:
//..          if (i->Xin.FpLdStI.isLoad) {
//..             vex_printf("gild%s ", i->Xin.FpLdStI.sz==8 ? "ll" : 
//..                                   i->Xin.FpLdStI.sz==4 ? "l" : "w");
//..             ppX86AMode(i->Xin.FpLdStI.addr);
//..             vex_printf(", ");
//..             ppHRegX86(i->Xin.FpLdStI.reg);
//..          } else {
//..             vex_printf("gist%s ", i->Xin.FpLdStI.sz==8 ? "ll" : 
//..                                   i->Xin.FpLdStI.sz==4 ? "l" : "w");
//..             ppHRegX86(i->Xin.FpLdStI.reg);
//..             vex_printf(", ");
//..             ppX86AMode(i->Xin.FpLdStI.addr);
//..          }
//..          return;
//..       case Xin_Fp64to32:
//..          vex_printf("gdtof ");
//..          ppHRegX86(i->Xin.Fp64to32.src);
//..          vex_printf(",");
//..          ppHRegX86(i->Xin.Fp64to32.dst);
//..          return;
//..       case Xin_FpCMov:
//..          vex_printf("gcmov%s ", showX86CondCode(i->Xin.FpCMov.cond));
//..          ppHRegX86(i->Xin.FpCMov.src);
//..          vex_printf(",");
//..          ppHRegX86(i->Xin.FpCMov.dst);
//..          return;
//..       case Xin_FpLdStCW:
//..          vex_printf(i->Xin.FpLdStCW.isLoad ? "fldcw " : "fstcw ");
//..          ppX86AMode(i->Xin.FpLdStCW.addr);
//..          return;
//..       case Xin_FpStSW_AX:
//..          vex_printf("fstsw %%ax");
//..          return;
//..       case Xin_FpCmp:
//..          vex_printf("gcmp ");
//..          ppHRegX86(i->Xin.FpCmp.srcL);
//..          vex_printf(",");
//..          ppHRegX86(i->Xin.FpCmp.srcR);
//..          vex_printf(",");
//..          ppHRegX86(i->Xin.FpCmp.dst);
//..          break;
//..       case Xin_SseConst:
//..          vex_printf("const $0x%04x,", (Int)i->Xin.SseConst.con);
//..          ppHRegX86(i->Xin.SseConst.dst);
//..          break;
//..       case Xin_SseLdSt:
//..          vex_printf("movups ");
//..          if (i->Xin.SseLdSt.isLoad) {
//..             ppX86AMode(i->Xin.SseLdSt.addr);
//..             vex_printf(",");
//..             ppHRegX86(i->Xin.SseLdSt.reg);
//..          } else {
//..             ppHRegX86(i->Xin.SseLdSt.reg);
//..             vex_printf(",");
//..             ppX86AMode(i->Xin.SseLdSt.addr);
//..          }
//..          return;
//..       case Xin_SseLdzLO:
//..          vex_printf("movs%s ", i->Xin.SseLdzLO.sz==4 ? "s" : "d");
//..          ppX86AMode(i->Xin.SseLdzLO.addr);
//..          vex_printf(",");
//..          ppHRegX86(i->Xin.SseLdzLO.reg);
//..          return;
//..       case Xin_Sse32Fx4:
//..          vex_printf("%sps ", showX86SseOp(i->Xin.Sse32Fx4.op));
//..          ppHRegX86(i->Xin.Sse32Fx4.src);
//..          vex_printf(",");
//..          ppHRegX86(i->Xin.Sse32Fx4.dst);
//..          return;
//..       case Xin_Sse32FLo:
//..          vex_printf("%sss ", showX86SseOp(i->Xin.Sse32FLo.op));
//..          ppHRegX86(i->Xin.Sse32FLo.src);
//..          vex_printf(",");
//..          ppHRegX86(i->Xin.Sse32FLo.dst);
//..          return;
//..       case Xin_Sse64Fx2:
//..          vex_printf("%spd ", showX86SseOp(i->Xin.Sse64Fx2.op));
//..          ppHRegX86(i->Xin.Sse64Fx2.src);
//..          vex_printf(",");
//..          ppHRegX86(i->Xin.Sse64Fx2.dst);
//..          return;
//..       case Xin_Sse64FLo:
//..          vex_printf("%ssd ", showX86SseOp(i->Xin.Sse64FLo.op));
//..          ppHRegX86(i->Xin.Sse64FLo.src);
//..          vex_printf(",");
//..          ppHRegX86(i->Xin.Sse64FLo.dst);
//..          return;
//..       case Xin_SseReRg:
//..          vex_printf("%s ", showX86SseOp(i->Xin.SseReRg.op));
//..          ppHRegX86(i->Xin.SseReRg.src);
//..          vex_printf(",");
//..          ppHRegX86(i->Xin.SseReRg.dst);
//..          return;
//..       case Xin_SseCMov:
//..          vex_printf("cmov%s ", showX86CondCode(i->Xin.SseCMov.cond));
//..          ppHRegX86(i->Xin.SseCMov.src);
//..          vex_printf(",");
//..          ppHRegX86(i->Xin.SseCMov.dst);
//..          return;
//..       case Xin_SseShuf:
//..          vex_printf("pshufd $0x%x,", i->Xin.SseShuf.order);
//..          ppHRegX86(i->Xin.SseShuf.src);
//..          vex_printf(",");
//..          ppHRegX86(i->Xin.SseShuf.dst);
//..          return;

      default:
         vpanic("ppPPC32Instr");
   }
}

/* --------- Helpers for register allocation. --------- */

void getRegUsage_PPC32Instr ( HRegUsage* u, PPC32Instr* i )
{
//   Bool unary;
   initHRegUsage(u);
   switch (i->tag) {
      case Pin_Alu32:
	 addHRegUse(u, HRmRead, i->Pin.Alu32.src1);
         addRegUsage_PPC32RI(u, i->Pin.Alu32.src2);
         if (i->Pin.Alu32.op == Palu_CMP) { 
            addHRegUse(u, HRmRead, i->Pin.Alu32.dst);
            return;
         }
	 addHRegUse(u, HRmWrite, i->Pin.Alu32.dst);

	 // CAB: Any circumstance where dst is read & written?
         return;

      case Pin_Sh32:
	 addHRegUse(u, HRmWrite, i->Pin.Sh32.dst);
 	 addHRegUse(u, HRmRead, i->Pin.Sh32.src);
	 addRegUsage_PPC32RI(u, i->Pin.Sh32.shft);

	 // CAB: Any circumstance where dst is read & written?
         return;

//..       case Xin_Test32:
//..          addRegUsage_X86RI(u, i->Xin.Test32.src);
//..          addRegUsage_X86RM(u, i->Xin.Test32.dst, HRmRead);
//..          return;
//..       case Xin_Unary32:
//..          addRegUsage_X86RM(u, i->Xin.Unary32.dst, HRmModify);
//..          return;
//..       case Xin_MulL:
//..          addRegUsage_X86RM(u, i->Xin.MulL.src, HRmRead);
//..          addHRegUse(u, HRmModify, hregX86_EAX());
//..          addHRegUse(u, HRmWrite, hregX86_EDX());
//..          return;
//..       case Xin_Div:
//..          addRegUsage_X86RM(u, i->Xin.Div.src, HRmRead);
//..          addHRegUse(u, HRmModify, hregX86_EAX());
//..          addHRegUse(u, HRmModify, hregX86_EDX());
//..          return;
//..       case Xin_Sh3232:
//..          addHRegUse(u, HRmRead, i->Xin.Sh3232.src);
//..          addHRegUse(u, HRmModify, i->Xin.Sh3232.dst);
//..          if (i->Xin.Sh3232.amt == 0)
//..             addHRegUse(u, HRmRead, hregX86_ECX());
//..          return;
//..       case Xin_Push:
//..          addRegUsage_X86RMI(u, i->Xin.Push.src);
//..          addHRegUse(u, HRmModify, hregX86_ESP());
//..          return;
//..       case Xin_Call:
//..          /* This is a bit subtle. */
//..          /* First off, claim it trashes all the callee-saved regs */
//..          /* which I believe to be %eax,%ecx,%edx. */
//..          addHRegUse(u, HRmWrite, hregX86_EAX());
//..          addHRegUse(u, HRmWrite, hregX86_ECX());
//..          addHRegUse(u, HRmWrite, hregX86_EDX());
//..          /* Now we have to state any parameter-carrying registers
//..             which might be read.  This depends on the regparmness. */
//..          switch (i->Xin.Call.regparms) {
//..             case 3: addHRegUse(u, HRmRead, hregX86_ECX()); /*fallthru*/
//..             case 2: addHRegUse(u, HRmRead, hregX86_EDX()); /*fallthru*/
//..             case 1: addHRegUse(u, HRmRead, hregX86_EAX()); break;
//..             case 0: break;
//..             default: vpanic("getRegUsage_X86Instr:Call:regparms");
//..          }
//..          /* Finally, there is the issue that the insn trashes a
//..             register because the literal target address has to be
//..             loaded into a register.  Fortunately, for the 0/1/2
//..             regparm case, we can use EAX, EDX and ECX respectively, so
//..             this does not cause any further damage.  For the 3-regparm
//..             case, we'll have to choose another register arbitrarily --
//..             since A, D and C are used for parameters -- and so we might
//..             as well choose EDI. */
//..          if (i->Xin.Call.regparms == 3)
//..             addHRegUse(u, HRmWrite, hregX86_EDI());
//..          /* Upshot of this is that the assembler really must observe
//..             the here-stated convention of which register to use as an
//..             address temporary, depending on the regparmness: 0==EAX,
//..             1==EDX, 2==ECX, 3==EDI. */
//..          return;
//..       case Xin_Goto:
//..          addRegUsage_X86RI(u, i->Xin.Goto.dst);
//..          addHRegUse(u, HRmWrite, hregX86_EAX());
//..          if (i->Xin.Goto.jk != Ijk_Boring)
//..             addHRegUse(u, HRmWrite, hregX86_EBP());
//..          return;
//..       case Xin_CMov32:
//..          addRegUsage_X86RM(u, i->Xin.CMov32.src, HRmRead);
//..          addHRegUse(u, HRmModify, i->Xin.CMov32.dst);
//..          return;
//..       case Xin_LoadEX:
//..          addRegUsage_X86AMode(u, i->Xin.LoadEX.src);
//..          addHRegUse(u, HRmWrite, i->Xin.LoadEX.dst);
//..          return;
//..       case Xin_Store:
//..          addHRegUse(u, HRmRead, i->Xin.Store.src);
//..          addRegUsage_X86AMode(u, i->Xin.Store.dst);
//..          return;
//..       case Xin_Set32:
//..          addHRegUse(u, HRmWrite, i->Xin.Set32.dst);
//..          return;
//..       case Xin_Bsfr32:
//..          addHRegUse(u, HRmRead, i->Xin.Bsfr32.src);
//..          addHRegUse(u, HRmWrite, i->Xin.Bsfr32.dst);
//..          return;
//..       case Xin_MFence:
//..          return;
//..       case Xin_FpUnary:
//..          addHRegUse(u, HRmRead, i->Xin.FpUnary.src);
//..          addHRegUse(u, HRmWrite, i->Xin.FpUnary.dst);
//..          return;
//..       case Xin_FpBinary:
//..          addHRegUse(u, HRmRead, i->Xin.FpBinary.srcL);
//..          addHRegUse(u, HRmRead, i->Xin.FpBinary.srcR);
//..          addHRegUse(u, HRmWrite, i->Xin.FpBinary.dst);
//..          return;
//..       case Xin_FpLdSt:
//..          addRegUsage_X86AMode(u, i->Xin.FpLdSt.addr);
//..          addHRegUse(u, i->Xin.FpLdSt.isLoad ? HRmWrite : HRmRead,
//..                        i->Xin.FpLdSt.reg);
//..          return;
//..       case Xin_FpLdStI:
//..          addRegUsage_X86AMode(u, i->Xin.FpLdStI.addr);
//..          addHRegUse(u, i->Xin.FpLdStI.isLoad ? HRmWrite : HRmRead,
//..                        i->Xin.FpLdStI.reg);
//..          return;
//..       case Xin_Fp64to32:
//..          addHRegUse(u, HRmRead,  i->Xin.Fp64to32.src);
//..          addHRegUse(u, HRmWrite, i->Xin.Fp64to32.dst);
//..          return;
//..       case Xin_FpCMov:
//..          addHRegUse(u, HRmRead,   i->Xin.FpCMov.src);
//..          addHRegUse(u, HRmModify, i->Xin.FpCMov.dst);
//..          return;
//..       case Xin_FpLdStCW:
//..          addRegUsage_X86AMode(u, i->Xin.FpLdStCW.addr);
//..          return;
//..       case Xin_FpStSW_AX:
//..          addHRegUse(u, HRmWrite, hregX86_EAX());
//..          return;
//..       case Xin_FpCmp:
//..          addHRegUse(u, HRmRead, i->Xin.FpCmp.srcL);
//..          addHRegUse(u, HRmRead, i->Xin.FpCmp.srcR);
//..          addHRegUse(u, HRmWrite, i->Xin.FpCmp.dst);
//..          addHRegUse(u, HRmWrite, hregX86_EAX());
//..          return;
//..       case Xin_SseLdSt:
//..          addRegUsage_X86AMode(u, i->Xin.SseLdSt.addr);
//..          addHRegUse(u, i->Xin.SseLdSt.isLoad ? HRmWrite : HRmRead,
//..                        i->Xin.SseLdSt.reg);
//..          return;
//..       case Xin_SseLdzLO:
//..          addRegUsage_X86AMode(u, i->Xin.SseLdzLO.addr);
//..          addHRegUse(u, HRmWrite, i->Xin.SseLdzLO.reg);
//..          return;
//..       case Xin_SseConst:
//..          addHRegUse(u, HRmWrite, i->Xin.SseConst.dst);
//..          return;
//..       case Xin_Sse32Fx4:
//..          vassert(i->Xin.Sse32Fx4.op != Xsse_MOV);
//..          unary = i->Xin.Sse32Fx4.op == Xsse_RCPF
//..                  || i->Xin.Sse32Fx4.op == Xsse_RSQRTF
//..                  || i->Xin.Sse32Fx4.op == Xsse_SQRTF;
//..          addHRegUse(u, HRmRead, i->Xin.Sse32Fx4.src);
//..          addHRegUse(u, unary ? HRmWrite : HRmModify, 
//..                        i->Xin.Sse32Fx4.dst);
//..          return;
//..       case Xin_Sse32FLo:
//..          vassert(i->Xin.Sse32FLo.op != Xsse_MOV);
//..          unary = i->Xin.Sse32FLo.op == Xsse_RCPF
//..                  || i->Xin.Sse32FLo.op == Xsse_RSQRTF
//..                  || i->Xin.Sse32FLo.op == Xsse_SQRTF;
//..          addHRegUse(u, HRmRead, i->Xin.Sse32FLo.src);
//..          addHRegUse(u, unary ? HRmWrite : HRmModify, 
//..                        i->Xin.Sse32FLo.dst);
//..          return;
//..       case Xin_Sse64Fx2:
//..          vassert(i->Xin.Sse64Fx2.op != Xsse_MOV);
//..          unary = i->Xin.Sse64Fx2.op == Xsse_RCPF
//..                  || i->Xin.Sse64Fx2.op == Xsse_RSQRTF
//..                  || i->Xin.Sse64Fx2.op == Xsse_SQRTF;
//..          addHRegUse(u, HRmRead, i->Xin.Sse64Fx2.src);
//..          addHRegUse(u, unary ? HRmWrite : HRmModify, 
//..                        i->Xin.Sse64Fx2.dst);
//..          return;
//..       case Xin_Sse64FLo:
//..          vassert(i->Xin.Sse64FLo.op != Xsse_MOV);
//..          unary = i->Xin.Sse64FLo.op == Xsse_RCPF
//..                  || i->Xin.Sse64FLo.op == Xsse_RSQRTF
//..                  || i->Xin.Sse64FLo.op == Xsse_SQRTF;
//..          addHRegUse(u, HRmRead, i->Xin.Sse64FLo.src);
//..          addHRegUse(u, unary ? HRmWrite : HRmModify, 
//..                        i->Xin.Sse64FLo.dst);
//..          return;
//..       case Xin_SseReRg:
//..          if (i->Xin.SseReRg.op == Xsse_XOR
//..              && i->Xin.SseReRg.src == i->Xin.SseReRg.dst) {
//..             /* reg-alloc needs to understand 'xor r,r' as a write of r */
//..             /* (as opposed to a rite of passage :-) */
//..             addHRegUse(u, HRmWrite, i->Xin.SseReRg.dst);
//..          } else {
//..             addHRegUse(u, HRmRead, i->Xin.SseReRg.src);
//..             addHRegUse(u, i->Xin.SseReRg.op == Xsse_MOV 
//..                              ? HRmWrite : HRmModify, 
//..                           i->Xin.SseReRg.dst);
//..          }
//..          return;
//..       case Xin_SseCMov:
//..          addHRegUse(u, HRmRead,   i->Xin.SseCMov.src);
//..          addHRegUse(u, HRmModify, i->Xin.SseCMov.dst);
//..          return;
//..       case Xin_SseShuf:
//..          addHRegUse(u, HRmRead,  i->Xin.SseShuf.src);
//..          addHRegUse(u, HRmWrite, i->Xin.SseShuf.dst);
//..          return;
      default:
         ppPPC32Instr(i);
         vpanic("getRegUsage_PPC32Instr");
   }
}

/* local helper */
static void mapReg(HRegRemap* m, HReg* r)
{
   *r = lookupHRegRemap(m, *r);
}

void mapRegs_PPC32Instr (HRegRemap* m, PPC32Instr* i)
{
   switch (i->tag) {
      case Pin_Alu32:
         mapReg(m, &i->Pin.Alu32.dst);
         mapReg(m, &i->Pin.Alu32.src1);
         mapRegs_PPC32RI(m, i->Pin.Alu32.src2);
         return;
      case Pin_Sh32:
         mapReg(m, &i->Pin.Sh32.dst);
         mapReg(m, &i->Pin.Sh32.src);
         mapRegs_PPC32RI(m, i->Pin.Sh32.shft);
         return;
//..       case Xin_Test32:
//..          mapRegs_X86RI(m, i->Xin.Test32.src);
//..          mapRegs_X86RM(m, i->Xin.Test32.dst);
//..          return;
//..       case Xin_Unary32:
//..          mapRegs_X86RM(m, i->Xin.Unary32.dst);
//..          return;
//..       case Xin_MulL:
//..          mapRegs_X86RM(m, i->Xin.MulL.src);
//..          return;
//..       case Xin_Div:
//..          mapRegs_X86RM(m, i->Xin.Div.src);
//..          return;
//..       case Xin_Sh3232:
//..          mapReg(m, &i->Xin.Sh3232.src);
//..          mapReg(m, &i->Xin.Sh3232.dst);
//..          return;
//..       case Xin_Push:
//..          mapRegs_X86RMI(m, i->Xin.Push.src);
//..          return;
//..       case Xin_Call:
//..          return;
//..       case Xin_Goto:
//..          mapRegs_X86RI(m, i->Xin.Goto.dst);
//..          return;
//..       case Xin_CMov32:
//..          mapRegs_X86RM(m, i->Xin.CMov32.src);
//..          mapReg(m, &i->Xin.CMov32.dst);
//..          return;
//..       case Xin_LoadEX:
//..          mapRegs_X86AMode(m, i->Xin.LoadEX.src);
//..          mapReg(m, &i->Xin.LoadEX.dst);
//..          return;
//..       case Xin_Store:
//..          mapReg(m, &i->Xin.Store.src);
//..          mapRegs_X86AMode(m, i->Xin.Store.dst);
//..          return;
//..       case Xin_Set32:
//..          mapReg(m, &i->Xin.Set32.dst);
//..          return;
//..       case Xin_Bsfr32:
//..          mapReg(m, &i->Xin.Bsfr32.src);
//..          mapReg(m, &i->Xin.Bsfr32.dst);
//..          return;
//..       case Xin_MFence:
//..          return;
//..       case Xin_FpUnary:
//..          mapReg(m, &i->Xin.FpUnary.src);
//..          mapReg(m, &i->Xin.FpUnary.dst);
//..          return;
//..       case Xin_FpBinary:
//..          mapReg(m, &i->Xin.FpBinary.srcL);
//..          mapReg(m, &i->Xin.FpBinary.srcR);
//..          mapReg(m, &i->Xin.FpBinary.dst);
//..          return;
//..       case Xin_FpLdSt:
//..          mapRegs_X86AMode(m, i->Xin.FpLdSt.addr);
//..          mapReg(m, &i->Xin.FpLdSt.reg);
//..          return;
//..       case Xin_FpLdStI:
//..          mapRegs_X86AMode(m, i->Xin.FpLdStI.addr);
//..          mapReg(m, &i->Xin.FpLdStI.reg);
//..          return;
//..       case Xin_Fp64to32:
//..          mapReg(m, &i->Xin.Fp64to32.src);
//..          mapReg(m, &i->Xin.Fp64to32.dst);
//..          return;
//..       case Xin_FpCMov:
//..          mapReg(m, &i->Xin.FpCMov.src);
//..          mapReg(m, &i->Xin.FpCMov.dst);
//..          return;
//..       case Xin_FpLdStCW:
//..          mapRegs_X86AMode(m, i->Xin.FpLdStCW.addr);
//..          return;
//..       case Xin_FpStSW_AX:
//..          return;
//..       case Xin_FpCmp:
//..          mapReg(m, &i->Xin.FpCmp.srcL);
//..          mapReg(m, &i->Xin.FpCmp.srcR);
//..          mapReg(m, &i->Xin.FpCmp.dst);
//..          return;
//..       case Xin_SseConst:
//..          mapReg(m, &i->Xin.SseConst.dst);
//..          return;
//..       case Xin_SseLdSt:
//..          mapReg(m, &i->Xin.SseLdSt.reg);
//..          mapRegs_X86AMode(m, i->Xin.SseLdSt.addr);
//..          break;
//..       case Xin_SseLdzLO:
//..          mapReg(m, &i->Xin.SseLdzLO.reg);
//..          mapRegs_X86AMode(m, i->Xin.SseLdzLO.addr);
//..          break;
//..       case Xin_Sse32Fx4:
//..          mapReg(m, &i->Xin.Sse32Fx4.src);
//..          mapReg(m, &i->Xin.Sse32Fx4.dst);
//..          return;
//..       case Xin_Sse32FLo:
//..          mapReg(m, &i->Xin.Sse32FLo.src);
//..          mapReg(m, &i->Xin.Sse32FLo.dst);
//..          return;
//..       case Xin_Sse64Fx2:
//..          mapReg(m, &i->Xin.Sse64Fx2.src);
//..          mapReg(m, &i->Xin.Sse64Fx2.dst);
//..          return;
//..       case Xin_Sse64FLo:
//..          mapReg(m, &i->Xin.Sse64FLo.src);
//..          mapReg(m, &i->Xin.Sse64FLo.dst);
//..          return;
//..       case Xin_SseReRg:
//..          mapReg(m, &i->Xin.SseReRg.src);
//..          mapReg(m, &i->Xin.SseReRg.dst);
//..          return;
//..       case Xin_SseCMov:
//..          mapReg(m, &i->Xin.SseCMov.src);
//..          mapReg(m, &i->Xin.SseCMov.dst);
//..          return;
//..       case Xin_SseShuf:
//..          mapReg(m, &i->Xin.SseShuf.src);
//..          mapReg(m, &i->Xin.SseShuf.dst);
//..          return;
      default:
         ppPPC32Instr(i);
         vpanic("mapRegs_PPC32Instr");
   }
}

/* Figure out if i represents a reg-reg move, and if so assign the
   source and destination to *src and *dst.  If in doubt say No.  Used
   by the register allocator to do move coalescing. 
*/
Bool isMove_PPC32Instr ( PPC32Instr* i, HReg* src, HReg* dst )
{
//..    /* Moves between integer regs */
//..    if (i->tag == Xin_Alu32R) {
//..       if (i->Xin.Alu32R.op != Xalu_MOV)
//..          return False;
//..       if (i->Xin.Alu32R.src->tag != Xrmi_Reg)
//..          return False;
//..       *src = i->Xin.Alu32R.src->Xrmi.Reg.reg;
//..       *dst = i->Xin.Alu32R.dst;
//..       return True;
//..    }
//..    /* Moves between FP regs */
//..    if (i->tag == Xin_FpUnary) {
//..       if (i->Xin.FpUnary.op != Xfp_MOV)
//..          return False;
//..       *src = i->Xin.FpUnary.src;
//..       *dst = i->Xin.FpUnary.dst;
//..       return True;
//..    }
//..    if (i->tag == Xin_SseReRg) {
//..       if (i->Xin.SseReRg.op != Xsse_MOV)
//..          return False;
//..       *src = i->Xin.SseReRg.src;
//..       *dst = i->Xin.SseReRg.dst;
//..       return True;
//..    }
   return False;
}


/* Generate x86 spill/reload instructions under the direction of the
   register allocator.  Note it's critical these don't write the
   condition codes. */

PPC32Instr* genSpill_PPC32 ( HReg rreg, Int offsetB )
{ vassert(0);
//..    X86AMode* am;
//..    vassert(offsetB >= 0);
//..    vassert(!hregIsVirtual(rreg));
//..    am = X86AMode_IR(offsetB, hregX86_EBP());
//.. 
//..    switch (hregClass(rreg)) {
//..       case HRcInt32:
//..          return X86Instr_Alu32M ( Xalu_MOV, X86RI_Reg(rreg), am );
//..       case HRcFlt64:
//..          return X86Instr_FpLdSt ( False/*store*/, 8, rreg, am );
//..       case HRcVec128:
//..          return X86Instr_SseLdSt ( False/*store*/, rreg, am );
//..       default: 
//..          ppHRegClass(hregClass(rreg));
//..          vpanic("genSpill_X86: unimplemented regclass");
//..    }
}

PPC32Instr* genReload_PPC32 ( HReg rreg, Int offsetB )
{ vassert(0);
//..    X86AMode* am;
//..    vassert(offsetB >= 0);
//..    vassert(!hregIsVirtual(rreg));
//..    am = X86AMode_IR(offsetB, hregX86_EBP());
//..    switch (hregClass(rreg)) {
//..       case HRcInt32:
//..          return X86Instr_Alu32R ( Xalu_MOV, X86RMI_Mem(am), rreg );
//..       case HRcFlt64:
//..          return X86Instr_FpLdSt ( True/*load*/, 8, rreg, am );
//..       case HRcVec128:
//..          return X86Instr_SseLdSt ( True/*load*/, rreg, am );
//..       default: 
//..          ppHRegClass(hregClass(rreg));
//..          vpanic("genReload_X86: unimplemented regclass");
//..    }
}


/* --------- The x86 assembler (bleh.) --------- */

#if 0
static UInt iregNo ( HReg r )
{
   UInt n;
   vassert(hregClass(r) == HRcInt32);
   vassert(!hregIsVirtual(r));
   n = hregNumber(r);
   vassert(n <= 32);
   return n;
}
#endif

//.. static UInt fregNo ( HReg r )
//.. {
//..    UInt n;
//..    vassert(hregClass(r) == HRcFlt64);
//..    vassert(!hregIsVirtual(r));
//..    n = hregNumber(r);
//..    vassert(n <= 5);
//..    return n;
//.. }

//.. static UInt vregNo ( HReg r )
//.. {
//..    UInt n;
//..    vassert(hregClass(r) == HRcVec128);
//..    vassert(!hregIsVirtual(r));
//..    n = hregNumber(r);
//..    vassert(n <= 7);
//..    return n;
//.. }

//.. static UChar mkModRegRM ( UChar mod, UChar reg, UChar regmem )
//.. {
//..    return ((mod & 3) << 6) | ((reg & 7) << 3) | (regmem & 7);
//.. }

//.. static UChar mkSIB ( Int shift, Int regindex, Int regbase )
//.. {
//..    return ((shift & 3) << 6) | ((regindex & 7) << 3) | (regbase & 7);
//.. }

//.. static UChar* emit32 ( UChar* p, UInt w32 )
//.. {
//..    *p++ = (w32)       & 0x000000FF;
//..    *p++ = (w32 >>  8) & 0x000000FF;
//..    *p++ = (w32 >> 16) & 0x000000FF;
//..    *p++ = (w32 >> 24) & 0x000000FF;
//..    return p;
//.. }

//.. /* Does a sign-extend of the lowest 8 bits give 
//..    the original number? */
//.. static Bool fits8bits ( UInt w32 )
//.. {
//..    Int i32 = (Int)w32;
//..    return i32 == ((i32 << 24) >> 24);
//.. }


//.. /* Forming mod-reg-rm bytes and scale-index-base bytes.
//.. 
//..      greg,  0(ereg)    |  ereg != ESP && ereg != EBP
//..                        =  00 greg ereg
//.. 
//..      greg,  d8(ereg)   |  ereg != ESP
//..                        =  01 greg ereg, d8
//.. 
//..      greg,  d32(ereg)  |  ereg != ESP
//..                        =  10 greg ereg, d32
//.. 
//..      greg,  d8(%esp)   =  01 greg 100, 0x24, d8
//.. 
//..      -----------------------------------------------
//.. 
//..      greg,  d8(base,index,scale)  
//..                |  index != ESP
//..                =  01 greg 100, scale index base, d8
//.. 
//..      greg,  d32(base,index,scale)
//..                |  index != ESP
//..                =  10 greg 100, scale index base, d32
//.. */
//.. static UChar* doAMode_M ( UChar* p, HReg greg, X86AMode* am ) 
//.. {
//..    if (am->tag == Xam_IR) {
//..       if (am->Xam.IR.imm == 0 
//..           && am->Xam.IR.reg != hregX86_ESP()
//..           && am->Xam.IR.reg != hregX86_EBP() ) {
//..          *p++ = mkModRegRM(0, iregNo(greg), iregNo(am->Xam.IR.reg));
//..          return p;
//..       }
//..       if (fits8bits(am->Xam.IR.imm)
//..           && am->Xam.IR.reg != hregX86_ESP()) {
//..          *p++ = mkModRegRM(1, iregNo(greg), iregNo(am->Xam.IR.reg));
//..          *p++ = am->Xam.IR.imm & 0xFF;
//..          return p;
//..       }
//..       if (am->Xam.IR.reg != hregX86_ESP()) {
//..          *p++ = mkModRegRM(2, iregNo(greg), iregNo(am->Xam.IR.reg));
//..          p = emit32(p, am->Xam.IR.imm);
//..          return p;
//..       }
//..       if (am->Xam.IR.reg == hregX86_ESP()
//..           && fits8bits(am->Xam.IR.imm)) {
//..  	 *p++ = mkModRegRM(1, iregNo(greg), 4);
//..          *p++ = 0x24;
//..          *p++ = am->Xam.IR.imm & 0xFF;
//..          return p;
//..       }
//..       ppX86AMode(am);
//..       vpanic("doAMode_M: can't emit amode IR");
//..       /*NOTREACHED*/
//..    }
//..    if (am->tag == Xam_IRRS) {
//..       if (fits8bits(am->Xam.IRRS.imm)
//..           && am->Xam.IRRS.index != hregX86_ESP()) {
//..          *p++ = mkModRegRM(1, iregNo(greg), 4);
//..          *p++ = mkSIB(am->Xam.IRRS.shift, am->Xam.IRRS.index, 
//..                                           am->Xam.IRRS.base);
//..          *p++ = am->Xam.IRRS.imm & 0xFF;
//..          return p;
//..       }
//..       if (am->Xam.IRRS.index != hregX86_ESP()) {
//..          *p++ = mkModRegRM(2, iregNo(greg), 4);
//..          *p++ = mkSIB(am->Xam.IRRS.shift, am->Xam.IRRS.index,
//..                                           am->Xam.IRRS.base);
//..          p = emit32(p, am->Xam.IRRS.imm);
//..          return p;
//..       }
//..       ppX86AMode(am);
//..       vpanic("doAMode_M: can't emit amode IRRS");
//..       /*NOTREACHED*/
//..    }
//..    vpanic("doAMode_M: unknown amode");
//..    /*NOTREACHED*/
//.. }


//.. /* Emit a mod-reg-rm byte when the rm bit denotes a reg. */
//.. static UChar* doAMode_R ( UChar* p, HReg greg, HReg ereg ) 
//.. {
//..    *p++ = mkModRegRM(3, iregNo(greg), iregNo(ereg));
//..    return p;
//.. }


//.. /* Emit ffree %st(7) */
//.. static UChar* do_ffree_st7 ( UChar* p )
//.. {
//..    *p++ = 0xDD;
//..    *p++ = 0xC7;
//..    return p;
//.. }

//.. /* Emit fstp %st(i), 1 <= i <= 7 */
//.. static UChar* do_fstp_st ( UChar* p, Int i )
//.. {
//..    vassert(1 <= i && i <= 7);
//..    *p++ = 0xDD;
//..    *p++ = 0xD8+i;
//..    return p;
//.. }

//.. /* Emit fld %st(i), 0 <= i <= 6 */
//.. static UChar* do_fld_st ( UChar* p, Int i )
//.. {
//..    vassert(0 <= i && i <= 6);
//..    *p++ = 0xD9;
//..    *p++ = 0xC0+i;
//..    return p;
//.. }

//.. /* Emit f<op> %st(0) */
//.. static UChar* do_fop1_st ( UChar* p, X86FpOp op )
//.. {
//..    switch (op) {
//..       case Xfp_NEG:    *p++ = 0xD9; *p++ = 0xE0; break;
//..       case Xfp_ABS:    *p++ = 0xD9; *p++ = 0xE1; break;
//..       case Xfp_SQRT:   *p++ = 0xD9; *p++ = 0xFA; break;
//..       case Xfp_ROUND:  *p++ = 0xD9; *p++ = 0xFC; break;
//..       case Xfp_SIN:    *p++ = 0xD9; *p++ = 0xFE; break;
//..       case Xfp_COS:    *p++ = 0xD9; *p++ = 0xFF; break;
//..       case Xfp_2XM1:   *p++ = 0xD9; *p++ = 0xF0; break;
//..       case Xfp_MOV:    break;
//..       case Xfp_TAN:    p = do_ffree_st7(p); /* since fptan pushes 1.0 */
//..                        *p++ = 0xD9; *p++ = 0xF2; /* fptan */
//..                        *p++ = 0xD9; *p++ = 0xF7; /* fincstp */
//..                        break;
//..       default: vpanic("do_fop1_st: unknown op");
//..    }
//..    return p;
//.. }

//.. /* Emit f<op> %st(i), 1 <= i <= 5 */
//.. static UChar* do_fop2_st ( UChar* p, X86FpOp op, Int i )
//.. {
//.. #  define fake(_n) mkHReg((_n), HRcInt32, False)
//..    Int subopc;
//..    switch (op) {
//..       case Xfp_ADD: subopc = 0; break;
//..       case Xfp_SUB: subopc = 4; break;
//..       case Xfp_MUL: subopc = 1; break;
//..       case Xfp_DIV: subopc = 6; break;
//..       default: vpanic("do_fop2_st: unknown op");
//..    }
//..    *p++ = 0xD8;
//..    p    = doAMode_R(p, fake(subopc), fake(i));
//..    return p;
//.. #  undef fake
//.. }

//.. /* Push a 32-bit word on the stack.  The word depends on tags[3:0];
//.. each byte is either 0x00 or 0xFF depending on the corresponding bit in tags[].
//.. */
//.. static UChar* push_word_from_tags ( UChar* p, UShort tags )
//.. {
//..    UInt w;
//..    vassert(0 == (tags & ~0xF));
//..    if (tags == 0) {
//..       /* pushl $0x00000000 */
//..       *p++ = 0x6A;
//..       *p++ = 0x00;
//..    }
//..    else 
//..    /* pushl $0xFFFFFFFF */
//..    if (tags == 0xF) {
//..       *p++ = 0x6A;
//..       *p++ = 0xFF;
//..    } else {
//..       vassert(0); /* awaiting test case */
//..       w = 0;
//..       if (tags & 1) w |= 0x000000FF;
//..       if (tags & 2) w |= 0x0000FF00;
//..       if (tags & 4) w |= 0x00FF0000;
//..       if (tags & 8) w |= 0xFF000000;
//..       *p++ = 0x68;
//..       p = emit32(p, w);
//..    }
//..    return p;
//.. }

/* Emit an instruction into buf and return the number of bytes used.
   Note that buf is not the insn's final place, and therefore it is
   imperative to emit position-independent code. */

Int emit_PPC32Instr ( UChar* buf, Int nbuf, PPC32Instr* i )
{
//..    UInt irno, opc, opc_rr, subopc_imm, opc_imma, opc_cl, opc_imm, subopc;
//.. 
//..    UInt   xtra;
   UChar* p = &buf[0];
//..    UChar* ptmp;
//..    vassert(nbuf >= 32);
//.. 
//..    /* Wrap an integer as a int register, for use assembling
//..       GrpN insns, in which the greg field is used as a sub-opcode
//..       and does not really contain a register. */
//.. #  define fake(_n) mkHReg((_n), HRcInt32, False)
//.. 
//..    /* vex_printf("asm  ");ppX86Instr(i); vex_printf("\n"); */

   switch (i->tag) {

//..    case Xin_Alu32R:
//..       /* Deal specially with MOV */
//..       if (i->Xin.Alu32R.op == Xalu_MOV) {
//..          switch (i->Xin.Alu32R.src->tag) {
//..             case Xrmi_Imm:
//..                *p++ = 0xB8 + iregNo(i->Xin.Alu32R.dst);
//..                p = emit32(p, i->Xin.Alu32R.src->Xrmi.Imm.imm32);
//..                goto done;
//..             case Xrmi_Reg:
//..                *p++ = 0x89;
//..                p = doAMode_R(p, i->Xin.Alu32R.src->Xrmi.Reg.reg,
//..                                 i->Xin.Alu32R.dst);
//..                goto done;
//..             case Xrmi_Mem:
//..                *p++ = 0x8B;
//..                p = doAMode_M(p, i->Xin.Alu32R.dst, 
//..                                 i->Xin.Alu32R.src->Xrmi.Mem.am);
//..                goto done;
//..             default:
//..                goto bad;
//..          }
//..       }
//..       /* MUL */
//..       if (i->Xin.Alu32R.op == Xalu_MUL) {
//..          switch (i->Xin.Alu32R.src->tag) {
//..             case Xrmi_Reg:
//..                *p++ = 0x0F;
//..                *p++ = 0xAF;
//..                p = doAMode_R(p, i->Xin.Alu32R.dst,
//..                                 i->Xin.Alu32R.src->Xrmi.Reg.reg);
//..                goto done;
//..             case Xrmi_Mem:
//..                *p++ = 0x0F;
//..                *p++ = 0xAF;
//..                p = doAMode_M(p, i->Xin.Alu32R.dst,
//..                                 i->Xin.Alu32R.src->Xrmi.Mem.am);
//..                goto done;
//..             case Xrmi_Imm:
//..                if (fits8bits(i->Xin.Alu32R.src->Xrmi.Imm.imm32)) {
//..                   *p++ = 0x6B;
//..                   p = doAMode_R(p, i->Xin.Alu32R.dst, i->Xin.Alu32R.dst);
//..                   *p++ = 0xFF & i->Xin.Alu32R.src->Xrmi.Imm.imm32;
//..                } else {
//..                   *p++ = 0x69;
//..                   p = doAMode_R(p, i->Xin.Alu32R.dst, i->Xin.Alu32R.dst);
//..                   p = emit32(p, i->Xin.Alu32R.src->Xrmi.Imm.imm32);
//..                }
//..                goto done;
//..             default:
//..                goto bad;
//..          }
//..       }
//..       /* ADD/SUB/ADC/SBB/AND/OR/XOR/CMP */
//..       opc = opc_rr = subopc_imm = opc_imma = 0;
//..       switch (i->Xin.Alu32R.op) {
//..          case Xalu_ADC: opc = 0x13; opc_rr = 0x11; 
//..                         subopc_imm = 2; opc_imma = 0x15; break;
//..          case Xalu_ADD: opc = 0x03; opc_rr = 0x01; 
//..                         subopc_imm = 0; opc_imma = 0x05; break;
//..          case Xalu_SUB: opc = 0x2B; opc_rr = 0x29; 
//..                         subopc_imm = 5; opc_imma = 0x2D; break;
//..          case Xalu_SBB: opc = 0x1B; opc_rr = 0x19; 
//..                         subopc_imm = 3; opc_imma = 0x1D; break;
//..          case Xalu_AND: opc = 0x23; opc_rr = 0x21; 
//..                         subopc_imm = 4; opc_imma = 0x25; break;
//..          case Xalu_XOR: opc = 0x33; opc_rr = 0x31; 
//..                         subopc_imm = 6; opc_imma = 0x35; break;
//..          case Xalu_OR:  opc = 0x0B; opc_rr = 0x09; 
//..                         subopc_imm = 1; opc_imma = 0x0D; break;
//..          case Xalu_CMP: opc = 0x3B; opc_rr = 0x39; 
//..                         subopc_imm = 7; opc_imma = 0x3D; break;
//..          default: goto bad;
//..       }
//..       switch (i->Xin.Alu32R.src->tag) {
//..          case Xrmi_Imm:
//..             if (i->Xin.Alu32R.dst == hregX86_EAX()
//..                 && !fits8bits(i->Xin.Alu32R.src->Xrmi.Imm.imm32)) {
//..                *p++ = opc_imma;
//..                p = emit32(p, i->Xin.Alu32R.src->Xrmi.Imm.imm32);
//..             } else
//..             if (fits8bits(i->Xin.Alu32R.src->Xrmi.Imm.imm32)) {
//..                *p++ = 0x83; 
//..                p    = doAMode_R(p, fake(subopc_imm), i->Xin.Alu32R.dst);
//..                *p++ = 0xFF & i->Xin.Alu32R.src->Xrmi.Imm.imm32;
//..             } else {
//..                *p++ = 0x81; 
//..                p    = doAMode_R(p, fake(subopc_imm), i->Xin.Alu32R.dst);
//..                p    = emit32(p, i->Xin.Alu32R.src->Xrmi.Imm.imm32);
//..             }
//..             goto done;
//..          case Xrmi_Reg:
//..             *p++ = opc_rr;
//..             p = doAMode_R(p, i->Xin.Alu32R.src->Xrmi.Reg.reg,
//..                              i->Xin.Alu32R.dst);
//..             goto done;
//..          case Xrmi_Mem:
//..             *p++ = opc;
//..             p = doAMode_M(p, i->Xin.Alu32R.dst,
//..                              i->Xin.Alu32R.src->Xrmi.Mem.am);
//..             goto done;
//..          default: 
//..             goto bad;
//..       }
//..       break;
//.. 
//..    case Xin_Alu32M:
//..       /* Deal specially with MOV */
//..       if (i->Xin.Alu32M.op == Xalu_MOV) {
//..          switch (i->Xin.Alu32M.src->tag) {
//..             case Xri_Reg:
//..                *p++ = 0x89;
//..                p = doAMode_M(p, i->Xin.Alu32M.src->Xri.Reg.reg,
//..                                 i->Xin.Alu32M.dst);
//..                goto done;
//..             case Xri_Imm:
//..                *p++ = 0xC7;
//..                p = doAMode_M(p, fake(0), i->Xin.Alu32M.dst);
//..                p = emit32(p, i->Xin.Alu32M.src->Xri.Imm.imm32);
//..                goto done;
//..             default: 
//..                goto bad;
//..          }
//..       }
//..       /* ADD/SUB/ADC/SBB/AND/OR/XOR/CMP.  MUL is not
//..          allowed here. */
//..       opc = subopc_imm = opc_imma = 0;
//..       switch (i->Xin.Alu32M.op) {
//..          case Xalu_ADD: opc = 0x01; subopc_imm = 0; break;
//..          case Xalu_SUB: opc = 0x29; subopc_imm = 5; break;
//..          default: goto bad;
//..       }
//..       switch (i->Xin.Alu32M.src->tag) {
//..          case Xri_Reg:
//..             *p++ = opc;
//..             p = doAMode_M(p, i->Xin.Alu32M.src->Xri.Reg.reg,
//..                              i->Xin.Alu32M.dst);
//..             goto done;
//..          case Xri_Imm:
//..             if (fits8bits(i->Xin.Alu32M.src->Xri.Imm.imm32)) {
//..                *p++ = 0x83;
//..                p    = doAMode_M(p, fake(subopc_imm), i->Xin.Alu32M.dst);
//..                *p++ = 0xFF & i->Xin.Alu32M.src->Xri.Imm.imm32;
//..                goto done;
//..             } else {
//..                *p++ = 0x81;
//..                p    = doAMode_M(p, fake(subopc_imm), i->Xin.Alu32M.dst);
//..                p    = emit32(p, i->Xin.Alu32M.src->Xri.Imm.imm32);
//..                goto done;
//..             }
//..          default: 
//..             goto bad;
//..       }
//..       break;
//.. 
//..    case Xin_Sh32:
//..       opc_cl = opc_imm = subopc = 0;
//..       switch (i->Xin.Sh32.op) {
//..          case Xsh_SHR: opc_cl = 0xD3; opc_imm = 0xC1; subopc = 5; break;
//..          case Xsh_SAR: opc_cl = 0xD3; opc_imm = 0xC1; subopc = 7; break;
//..          case Xsh_SHL: opc_cl = 0xD3; opc_imm = 0xC1; subopc = 4; break;
//..          default: goto bad;
//..       }
//..       if (i->Xin.Sh32.src == 0) {
//..          *p++ = opc_cl;
//..          switch (i->Xin.Sh32.dst->tag) {
//..             case Xrm_Reg:
//..                p = doAMode_R(p, fake(subopc), 
//..                                 i->Xin.Sh32.dst->Xrm.Reg.reg);
//..                goto done;
//..             default:
//..                goto bad;
//..          }
//..       } else {
//..          *p++ = opc_imm;
//..          switch (i->Xin.Sh32.dst->tag) {
//..             case Xrm_Reg:
//..                p = doAMode_R(p, fake(subopc), 
//..                                 i->Xin.Sh32.dst->Xrm.Reg.reg);
//..                *p++ = (UChar)(i->Xin.Sh32.src);
//..                goto done;
//..             default:
//..                goto bad;
//..          }
//..       }
//..       break;
//.. 
//..    case Xin_Test32:
//..       if (i->Xin.Test32.src->tag == Xri_Imm
//..           && i->Xin.Test32.dst->tag == Xrm_Reg) {
//..          /* testl $imm32, %reg */
//..          *p++ = 0xF7;
//..          p = doAMode_R(p, fake(0), i->Xin.Test32.dst->Xrm.Reg.reg);
//..          p = emit32(p, i->Xin.Test32.src->Xri.Imm.imm32);
//..          goto done;
//..       }
//..       break;
//.. 
//..    case Xin_Unary32:
//..       if (i->Xin.Unary32.op == Xun_NOT) {
//..          *p++ = 0xF7;
//..          if (i->Xin.Unary32.dst->tag == Xrm_Reg) {
//..             p = doAMode_R(p, fake(2), i->Xin.Unary32.dst->Xrm.Reg.reg);
//..             goto done;
//..          } else {
//..             goto bad;
//..          }
//..       }
//..       if (i->Xin.Unary32.op == Xun_NEG) {
//..          *p++ = 0xF7;
//..          if (i->Xin.Unary32.dst->tag == Xrm_Reg) {
//..             p = doAMode_R(p, fake(3), i->Xin.Unary32.dst->Xrm.Reg.reg);
//..             goto done;
//..          } else {
//..             goto bad;
//..          }
//..       }
//..       break;
//.. 
//..    case Xin_MulL:
//..       subopc = i->Xin.MulL.syned ? 5 : 4;
//..       if (i->Xin.MulL.ssz == Xss_32) {
//..          *p++ = 0xF7;
//..          switch (i->Xin.MulL.src->tag)  {
//..             case Xrm_Mem:
//..                p = doAMode_M(p, fake(subopc),
//..                                 i->Xin.MulL.src->Xrm.Mem.am);
//..                goto done;
//..             case Xrm_Reg:
//..                p = doAMode_R(p, fake(subopc), 
//..                                 i->Xin.MulL.src->Xrm.Reg.reg);
//..                goto done;
//..             default:
//..                goto bad;
//..          }
//..       }
//..       break;
//.. 
//..    case Xin_Div:
//..       subopc = i->Xin.Div.syned ? 7 : 6;
//..       if (i->Xin.Div.ssz == Xss_32) {
//..          *p++ = 0xF7;
//..          switch (i->Xin.Div.src->tag)  {
//..             case Xrm_Mem:
//..                p = doAMode_M(p, fake(subopc),
//..                                 i->Xin.Div.src->Xrm.Mem.am);
//..                goto done;
//..             case Xrm_Reg:
//..                p = doAMode_R(p, fake(subopc), 
//..                                 i->Xin.Div.src->Xrm.Reg.reg);
//..                goto done;
//..             default:
//..                goto bad;
//..          }
//..       }
//..       break;
//.. 
//..    case Xin_Sh3232:
//..       vassert(i->Xin.Sh3232.op == Xsh_SHL || i->Xin.Sh3232.op == Xsh_SHR);
//..       if (i->Xin.Sh3232.amt == 0) {
//..          /* shldl/shrdl by %cl */
//..          *p++ = 0x0F;
//..          if (i->Xin.Sh3232.op == Xsh_SHL) {
//..             *p++ = 0xA5;
//..          } else {
//..             *p++ = 0xAD;
//..          }
//..          p = doAMode_R(p, i->Xin.Sh3232.src, i->Xin.Sh3232.dst);
//..          goto done;
//..       }
//..       break;
//.. 
//..    case Xin_Push:
//..       switch (i->Xin.Push.src->tag) {
//..          case Xrmi_Mem: 
//..             *p++ = 0xFF;
//..             p = doAMode_M(p, fake(6), i->Xin.Push.src->Xrmi.Mem.am);
//..             goto done;
//..          case Xrmi_Imm:
//..             *p++ = 0x68;
//..             p = emit32(p, i->Xin.Push.src->Xrmi.Imm.imm32);
//..             goto done;
//..          case Xrmi_Reg:
//..             *p++ = 0x50 + iregNo(i->Xin.Push.src->Xrmi.Reg.reg);
//..             goto done;
//..         default: 
//..             goto bad;
//..       }
//.. 
//..    case Xin_Call:
//..       /* See detailed comment for Xin_Call in getRegUsage_X86Instr above
//..          for explanation of this. */
//..       switch (i->Xin.Call.regparms) {
//..          case 0: irno = iregNo(hregX86_EAX()); break;
//..          case 1: irno = iregNo(hregX86_EDX()); break;
//..          case 2: irno = iregNo(hregX86_ECX()); break;
//..          case 3: irno = iregNo(hregX86_EDI()); break;
//..          default: vpanic(" emit_X86Instr:call:regparms");
//..       }
//..       /* jump over the following two insns if the condition does not
//..          hold */
//..       if (i->Xin.Call.cond != Xcc_ALWAYS) {
//..          *p++ = 0x70 + (0xF & (i->Xin.Call.cond ^ 1));
//..          *p++ = 0x07; /* 7 bytes in the next two insns */
//..       }
//..       /* movl $target, %tmp */
//..       *p++ = 0xB8 + irno; 
//..       p = emit32(p, i->Xin.Call.target);
//..       /* call *%tmp */
//..       *p++ = 0xFF;
//..       *p++ = 0xD0 + irno;
//..       goto done;
//.. 
//..    case Xin_Goto:
//..       /* Use ptmp for backpatching conditional jumps. */
//..       ptmp = NULL;
//.. 
//..       /* First off, if this is conditional, create a conditional
//.. 	 jump over the rest of it. */
//..       if (i->Xin.Goto.cond != Xcc_ALWAYS) {
//..          /* jmp fwds if !condition */
//..          *p++ = 0x70 + (i->Xin.Goto.cond ^ 1);
//..          ptmp = p; /* fill in this bit later */
//..          *p++ = 0; /* # of bytes to jump over; don't know how many yet. */
//..       }
//.. 
//..       /* If a non-boring, set %ebp (the guest state pointer)
//..          appropriately. */
//..       /* movl $magic_number, %ebp */
//..       switch (i->Xin.Goto.jk) {
//..          case Ijk_ClientReq: 
//..             *p++ = 0xBD;
//..             p = emit32(p, VEX_TRC_JMP_CLIENTREQ); break;
//..          case Ijk_Syscall: 
//..             *p++ = 0xBD;
//..             p = emit32(p, VEX_TRC_JMP_SYSCALL); break;
//..          case Ijk_Yield: 
//..             *p++ = 0xBD;
//..             p = emit32(p, VEX_TRC_JMP_YIELD); break;
//..          case Ijk_EmWarn:
//..             *p++ = 0xBD;
//..             p = emit32(p, VEX_TRC_JMP_EMWARN); break;
//..          case Ijk_MapFail:
//..             *p++ = 0xBD;
//..             p = emit32(p, VEX_TRC_JMP_MAPFAIL); break;
//..          case Ijk_NoDecode:
//..             *p++ = 0xBD;
//..             p = emit32(p, VEX_TRC_JMP_NODECODE); break;
//..          case Ijk_Ret:
//.. 	 case Ijk_Call:
//..          case Ijk_Boring:
//..             break;
//..          default: 
//..             ppIRJumpKind(i->Xin.Goto.jk);
//..             vpanic("emit_X86Instr.Xin_Goto: unknown jump kind");
//..       }
//.. 
//..       /* Get the destination address into %eax */
//..       if (i->Xin.Goto.dst->tag == Xri_Imm) {
//..          /* movl $immediate, %eax ; ret */
//..          *p++ = 0xB8;
//..          p = emit32(p, i->Xin.Goto.dst->Xri.Imm.imm32);
//..       } else {
//..          vassert(i->Xin.Goto.dst->tag == Xri_Reg);
//..          /* movl %reg, %eax ; ret */
//..          if (i->Xin.Goto.dst->Xri.Reg.reg != hregX86_EAX()) {
//..             *p++ = 0x89;
//..             p = doAMode_R(p, i->Xin.Goto.dst->Xri.Reg.reg, hregX86_EAX());
//..          }
//..       }
//.. 
//..       /* ret */
//..       *p++ = 0xC3;
//.. 
//..       /* Fix up the conditional jump, if there was one. */
//..       if (i->Xin.Goto.cond != Xcc_ALWAYS) {
//..          Int delta = p - ptmp;
//.. 	 vassert(delta > 0 && delta < 20);
//..          *ptmp = (UChar)(delta-1);
//..       }
//..       goto done;
//.. 
//..    case Xin_CMov32:
//..       vassert(i->Xin.CMov32.cond != Xcc_ALWAYS);
//.. #if 0
//..       /* This generates cmov, which is illegal on P54/P55. */
//..       *p++ = 0x0F;
//..       *p++ = 0x40 + i->Xin.CMov32.cond;
//..       if (i->Xin.CMov32.src->tag == Xrm_Reg) {
//..          p = doAMode_R(p, i->Xin.CMov32.dst, i->Xin.CMov32.src->Xrm.Reg.reg);
//..          goto done;
//..       }
//..       if (i->Xin.CMov32.src->tag == Xrm_Mem) {
//..          p = doAMode_M(p, i->Xin.CMov32.dst, i->Xin.CMov32.src->Xrm.Mem.am);
//..          goto done;
//..       }
//.. #else
//..       /* P5 friendly version: conditional jump over an unconditional
//..          move. */
//..       /* jmp fwds if !condition */
//..       *p++ = 0x70 + (i->Xin.CMov32.cond ^ 1);
//..       *p++ = 0; /* # of bytes in the next bit, which we don't know yet */
//..       ptmp = p;
//.. 
//..       switch (i->Xin.CMov32.src->tag) {
//..          case Xrm_Reg:
//..             /* Big sigh.  This is movl E -> G ... */
//..             *p++ = 0x89;
//..             p = doAMode_R(p, i->Xin.CMov32.src->Xrm.Reg.reg,
//..                              i->Xin.CMov32.dst);
//..                              
//..             break;
//..          case Xrm_Mem:
//..             /* ... whereas this is movl G -> E.  That's why the args
//..                to doAMode_R appear to be the wrong way round in the
//..                Xrm_Reg case. */
//..             *p++ = 0x8B;
//..             p = doAMode_M(p, i->Xin.CMov32.dst, 
//..                              i->Xin.CMov32.src->Xrm.Mem.am);
//..             break;
//..          default:
//..             goto bad;
//..       }
//..       /* Fill in the jump offset. */
//..       *(ptmp-1) = p - ptmp;
//..       goto done;
//.. #endif
//..       break;
//.. 
//..    case Xin_LoadEX:
//..       if (i->Xin.LoadEX.szSmall == 1 && !i->Xin.LoadEX.syned) {
//..          /* movzbl */
//..          *p++ = 0x0F;
//..          *p++ = 0xB6;
//..          p = doAMode_M(p, i->Xin.LoadEX.dst, i->Xin.LoadEX.src); 
//..          goto done;
//..       }
//..       if (i->Xin.LoadEX.szSmall == 2 && !i->Xin.LoadEX.syned) {
//..          /* movzwl */
//..          *p++ = 0x0F;
//..          *p++ = 0xB7;
//..          p = doAMode_M(p, i->Xin.LoadEX.dst, i->Xin.LoadEX.src); 
//..          goto done;
//..       }
//..       break;
//.. 
//..    case Xin_Set32:
//..       /* Make the destination register be 1 or 0, depending on whether
//..          the relevant condition holds.  We have to dodge and weave
//..          when the destination is %esi or %edi as we cannot directly
//..          emit the native 'setb %reg' for those.  Further complication:
//..          the top 24 bits of the destination should be forced to zero,
//..          but doing 'xor %r,%r' kills the flag(s) we are about to read.
//..          Sigh.  So start off my moving $0 into the dest. */
//.. 
//..       /* Do we need to swap in %eax? */
//..       if (iregNo(i->Xin.Set32.dst) >= 4) {
//..          /* xchg %eax, %dst */
//..          *p++ = 0x90 + iregNo(i->Xin.Set32.dst);
//..          /* movl $0, %eax */
//..          *p++ = 0xB8 + iregNo(hregX86_EAX());
//..          p = emit32(p, 0);
//..          /* setb lo8(%eax) */
//..          *p++ = 0x0F; 
//..          *p++ = 0x90 + (UChar)(i->Xin.Set32.cond);
//..          p = doAMode_R(p, fake(0), hregX86_EAX());
//..          /* xchg %eax, %dst */
//..          *p++ = 0x90 + iregNo(i->Xin.Set32.dst);
//..       } else {
//..          /* movl $0, %dst */
//..          *p++ = 0xB8 + iregNo(i->Xin.Set32.dst);
//..          p = emit32(p, 0);
//..          /* setb lo8(%dst) */
//..          *p++ = 0x0F; 
//..          *p++ = 0x90 + (UChar)(i->Xin.Set32.cond);
//..          p = doAMode_R(p, fake(0), i->Xin.Set32.dst);
//..       }
//..       goto done;
//.. 
//..    case Xin_Bsfr32:
//..       *p++ = 0x0F;
//..       if (i->Xin.Bsfr32.isFwds) {
//..          *p++ = 0xBC;
//..       } else {
//..          *p++ = 0xBD;
//..       }
//..       p = doAMode_R(p, i->Xin.Bsfr32.dst, i->Xin.Bsfr32.src);
//..       goto done;
//.. 
//..    case Xin_MFence:
//..       /* see comment in hdefs.h re this insn */
//..       if (0) vex_printf("EMIT FENCE\n");
//..       switch (i->Xin.MFence.subarch) {
//..          case VexSubArchX86_sse0:
//..             vassert(0); /* awaiting test case */
//..             /* lock addl $0,0(%esp) */
//..             *p++ = 0xF0; *p++ = 0x83; *p++ = 0x44; 
//..             *p++ = 0x24; *p++ = 0x00; *p++ = 0x00;
//..             goto done;
//..          case VexSubArchX86_sse1:
//..             /* sfence */
//..             *p++ = 0x0F; *p++ = 0xAE; *p++ = 0xF8;
//..             /* lock addl $0,0(%esp) */
//..             *p++ = 0xF0; *p++ = 0x83; *p++ = 0x44; 
//..             *p++ = 0x24; *p++ = 0x00; *p++ = 0x00;
//..             goto done;
//..          case VexSubArchX86_sse2:
//..             /* mfence */
//..             *p++ = 0x0F; *p++ = 0xAE; *p++ = 0xF0;
//..             goto done;
//..          default: 
//..             vpanic("emit_X86Instr:mfence:subarch");
//..       }
//..       break;
//.. 
//..    case Xin_Store:
//..       if (i->Xin.Store.sz == 2) {
//..          /* This case, at least, is simple, given that we can
//..             reference the low 16 bits of any integer register. */
//..          *p++ = 0x66;
//..          *p++ = 0x89;
//..          p = doAMode_M(p, i->Xin.Store.src, i->Xin.Store.dst);
//..          goto done;
//..       }
//.. 
//..       if (i->Xin.Store.sz == 1) {
//..          /* We have to do complex dodging and weaving if src is not
//..             the low 8 bits of %eax/%ebx/%ecx/%edx. */
//..          if (iregNo(i->Xin.Store.src) < 4) {
//..             /* we're OK, can do it directly */
//..             *p++ = 0x88;
//..             p = doAMode_M(p, i->Xin.Store.src, i->Xin.Store.dst);
//..            goto done;
//..          } else {
//..             /* Bleh.  This means the source is %edi or %esi.  Since
//..                the address mode can only mention three registers, at
//..                least one of %eax/%ebx/%ecx/%edx must be available to
//..                temporarily swap the source into, so the store can
//..                happen.  So we have to look at the regs mentioned
//..                in the amode. */
//..             HReg swap = INVALID_HREG;
//..             HReg  eax = hregX86_EAX(), ebx = hregX86_EBX(), 
//..                   ecx = hregX86_ECX(), edx = hregX86_EDX();
//..             Bool a_ok = True, b_ok = True, c_ok = True, d_ok = True;
//..             HRegUsage u;
//..             Int j;
//..             initHRegUsage(&u);
//..             addRegUsage_X86AMode(&u,  i->Xin.Store.dst);
//..             for (j = 0; j < u.n_used; j++) {
//..                HReg r = u.hreg[j];
//..                if (r == eax) a_ok = False;
//..                if (r == ebx) b_ok = False;
//..                if (r == ecx) c_ok = False;
//..                if (r == edx) d_ok = False;
//..             }
//..             if (a_ok) swap = eax;
//..             if (b_ok) swap = ebx;
//..             if (c_ok) swap = ecx;
//..             if (d_ok) swap = edx;
//..             vassert(swap != INVALID_HREG);
//..             /* xchgl %source, %swap. Could do better if swap is %eax. */
//..             *p++ = 0x87;
//..             p = doAMode_R(p, i->Xin.Store.src, swap);
//..             /* movb lo8{%swap}, (dst) */
//..             *p++ = 0x88;
//..             p = doAMode_M(p, swap, i->Xin.Store.dst);
//..             /* xchgl %source, %swap. Could do better if swap is %eax. */
//..             *p++ = 0x87;
//..             p = doAMode_R(p, i->Xin.Store.src, swap);
//..             goto done;
//..          }
//..       } /* if (i->Xin.Store.sz == 1) */
//..       break;
//.. 
//..    case Xin_FpUnary:
//..       /* gop %src, %dst
//..          --> ffree %st7 ; fld %st(src) ; fop %st(0) ; fstp %st(1+dst)
//..       */
//..       p = do_ffree_st7(p);
//..       p = do_fld_st(p, 0+hregNumber(i->Xin.FpUnary.src));
//..       p = do_fop1_st(p, i->Xin.FpUnary.op);
//..       p = do_fstp_st(p, 1+hregNumber(i->Xin.FpUnary.dst));
//..       goto done;
//.. 
//..    case Xin_FpBinary:
//..       if (i->Xin.FpBinary.op == Xfp_YL2X
//..           || i->Xin.FpBinary.op == Xfp_YL2XP1) {
//..          /* Have to do this specially. */
//..          /* ffree %st7 ; fld %st(srcL) ; 
//..             ffree %st7 ; fld %st(srcR+1) ; fyl2x{p1} ; fstp(1+dst) */
//..          p = do_ffree_st7(p);
//..          p = do_fld_st(p, 0+hregNumber(i->Xin.FpBinary.srcL));
//..          p = do_ffree_st7(p);
//..          p = do_fld_st(p, 1+hregNumber(i->Xin.FpBinary.srcR));
//..          *p++ = 0xD9; 
//..          *p++ = i->Xin.FpBinary.op==Xfp_YL2X ? 0xF1 : 0xF9;
//..          p = do_fstp_st(p, 1+hregNumber(i->Xin.FpBinary.dst));
//..          goto done;
//..       }
//..       if (i->Xin.FpBinary.op == Xfp_ATAN) {
//..          /* Have to do this specially. */
//..          /* ffree %st7 ; fld %st(srcL) ; 
//..             ffree %st7 ; fld %st(srcR+1) ; fpatan ; fstp(1+dst) */
//..          p = do_ffree_st7(p);
//..          p = do_fld_st(p, 0+hregNumber(i->Xin.FpBinary.srcL));
//..          p = do_ffree_st7(p);
//..          p = do_fld_st(p, 1+hregNumber(i->Xin.FpBinary.srcR));
//..          *p++ = 0xD9; *p++ = 0xF3;
//..          p = do_fstp_st(p, 1+hregNumber(i->Xin.FpBinary.dst));
//..          goto done;
//..       }
//..       if (i->Xin.FpBinary.op == Xfp_PREM
//..           || i->Xin.FpBinary.op == Xfp_PREM1
//..           || i->Xin.FpBinary.op == Xfp_SCALE) {
//..          /* Have to do this specially. */
//..          /* ffree %st7 ; fld %st(srcR) ; 
//..             ffree %st7 ; fld %st(srcL+1) ; fprem/fprem1/fscale ; fstp(2+dst) ; 
//..             fincstp ; ffree %st7 */
//..          p = do_ffree_st7(p);
//..          p = do_fld_st(p, 0+hregNumber(i->Xin.FpBinary.srcR));
//..          p = do_ffree_st7(p);
//..          p = do_fld_st(p, 1+hregNumber(i->Xin.FpBinary.srcL));
//..          *p++ = 0xD9;
//..          switch (i->Xin.FpBinary.op) {
//..             case Xfp_PREM: *p++ = 0xF8; break;
//..             case Xfp_PREM1: *p++ = 0xF5; break;
//..             case Xfp_SCALE: *p++ =  0xFD; break;
//..             default: vpanic("emitX86Instr(FpBinary,PREM/PREM1/SCALE)");
//..          }
//..          p = do_fstp_st(p, 2+hregNumber(i->Xin.FpBinary.dst));
//..          *p++ = 0xD9; *p++ = 0xF7;
//..          p = do_ffree_st7(p);
//..          goto done;
//..       }
//..       /* General case */
//..       /* gop %srcL, %srcR, %dst
//..          --> ffree %st7 ; fld %st(srcL) ; fop %st(1+srcR) ; fstp %st(1+dst)
//..       */
//..       p = do_ffree_st7(p);
//..       p = do_fld_st(p, 0+hregNumber(i->Xin.FpBinary.srcL));
//..       p = do_fop2_st(p, i->Xin.FpBinary.op, 
//..                         1+hregNumber(i->Xin.FpBinary.srcR));
//..       p = do_fstp_st(p, 1+hregNumber(i->Xin.FpBinary.dst));
//..       goto done;
//.. 
//..    case Xin_FpLdSt:
//..       vassert(i->Xin.FpLdSt.sz == 4 || i->Xin.FpLdSt.sz == 8);
//..       if (i->Xin.FpLdSt.isLoad) {
//..          /* Load from memory into %fakeN.  
//..             --> ffree %st(7) ; fld{s/l} amode ; fstp st(N+1) 
//..          */
//..          p = do_ffree_st7(p);
//..          *p++ = i->Xin.FpLdSt.sz==4 ? 0xD9 : 0xDD;
//.. 	 p = doAMode_M(p, fake(0)/*subopcode*/, i->Xin.FpLdSt.addr);
//..          p = do_fstp_st(p, 1+hregNumber(i->Xin.FpLdSt.reg));
//..          goto done;
//..       } else {
//..          /* Store from %fakeN into memory.
//..             --> ffree %st(7) ; fld st(N) ; fstp{l|s} amode
//.. 	 */
//..          p = do_ffree_st7(p);
//..          p = do_fld_st(p, 0+hregNumber(i->Xin.FpLdSt.reg));
//..          *p++ = i->Xin.FpLdSt.sz==4 ? 0xD9 : 0xDD;
//..          p = doAMode_M(p, fake(3)/*subopcode*/, i->Xin.FpLdSt.addr);
//..          goto done;
//..       }
//..       break;
//.. 
//..    case Xin_FpLdStI:
//..       if (i->Xin.FpLdStI.isLoad) {
//..          /* Load from memory into %fakeN, converting from an int.  
//..             --> ffree %st(7) ; fild{w/l/ll} amode ; fstp st(N+1) 
//..          */
//..          switch (i->Xin.FpLdStI.sz) {
//..             case 8:  opc = 0xDF; subopc_imm = 5; break;
//..             case 4:  opc = 0xDB; subopc_imm = 0; break;
//..             case 2:  vassert(0); opc = 0xDF; subopc_imm = 0; break;
//..             default: vpanic("emitX86Instr(Xin_FpLdStI-load)");
//..          }
//..          p = do_ffree_st7(p);
//..          *p++ = opc;
//..          p = doAMode_M(p, fake(subopc_imm)/*subopcode*/, i->Xin.FpLdStI.addr);
//..          p = do_fstp_st(p, 1+hregNumber(i->Xin.FpLdStI.reg));
//..          goto done;
//..       } else {
//..          /* Store from %fakeN into memory, converting to an int.
//..             --> ffree %st(7) ; fld st(N) ; fistp{w/l/ll} amode
//.. 	 */
//..          switch (i->Xin.FpLdStI.sz) {
//..             case 8:  opc = 0xDF; subopc_imm = 7; break;
//..             case 4:  opc = 0xDB; subopc_imm = 3; break;
//..             case 2:  opc = 0xDF; subopc_imm = 3; break;
//..             default: vpanic("emitX86Instr(Xin_FpLdStI-store)");
//..          }
//..          p = do_ffree_st7(p);
//..          p = do_fld_st(p, 0+hregNumber(i->Xin.FpLdStI.reg));
//..          *p++ = opc;
//..          p = doAMode_M(p, fake(subopc_imm)/*subopcode*/, i->Xin.FpLdStI.addr);
//..          goto done;
//..       }
//..       break;
//.. 
//..    case Xin_Fp64to32:
//..       /* ffree %st7 ; fld %st(src) */
//..       p = do_ffree_st7(p);
//..       p = do_fld_st(p, 0+fregNo(i->Xin.Fp64to32.src));
//..       /* subl $4, %esp */
//..       *p++ = 0x83; *p++ = 0xEC; *p++ = 0x04;
//..       /* fstps (%esp) */
//..       *p++ = 0xD9; *p++ = 0x1C; *p++ = 0x24;
//..       /* flds (%esp) */
//..       *p++ = 0xD9; *p++ = 0x04; *p++ = 0x24;
//..       /* addl $4, %esp */
//..       *p++ = 0x83; *p++ = 0xC4; *p++ = 0x04;
//..       /* fstp %st(1+dst) */
//..       p = do_fstp_st(p, 1+fregNo(i->Xin.Fp64to32.dst));
//..       goto done;
//.. 
//..    case Xin_FpCMov:
//..       /* jmp fwds if !condition */
//..       *p++ = 0x70 + (i->Xin.FpCMov.cond ^ 1);
//..       *p++ = 0; /* # of bytes in the next bit, which we don't know yet */
//..       ptmp = p;
//.. 
//..       /* ffree %st7 ; fld %st(src) ; fstp %st(1+dst) */
//..       p = do_ffree_st7(p);
//..       p = do_fld_st(p, 0+fregNo(i->Xin.FpCMov.src));
//..       p = do_fstp_st(p, 1+fregNo(i->Xin.FpCMov.dst));
//.. 
//..       /* Fill in the jump offset. */
//..       *(ptmp-1) = p - ptmp;
//..       goto done;
//.. 
//..    case Xin_FpLdStCW:
//..       if (i->Xin.FpLdStCW.isLoad) {
//..          *p++ = 0xD9;
//..          p = doAMode_M(p, fake(5)/*subopcode*/, i->Xin.FpLdStCW.addr);
//..       } else {
//..          vassert(0);
//..       }
//..       goto done;
//.. 
//..    case Xin_FpStSW_AX:
//..       /* note, this emits fnstsw %ax, not fstsw %ax */
//..       *p++ = 0xDF;
//..       *p++ = 0xE0;
//..       goto done;
//.. 
//..    case Xin_FpCmp:
//..       /* gcmp %fL, %fR, %dst
//..          -> ffree %st7; fpush %fL ; fucomp %(fR+1) ; 
//..             fnstsw %ax ; movl %eax, %dst 
//..       */
//..       /* ffree %st7 */
//..       p = do_ffree_st7(p);
//..       /* fpush %fL */
//..       p = do_fld_st(p, 0+fregNo(i->Xin.FpCmp.srcL));
//..       /* fucomp %(fR+1) */
//..       *p++ = 0xDD;
//..       *p++ = 0xE8 + (7 & (1+fregNo(i->Xin.FpCmp.srcR)));
//..       /* fnstsw %ax */
//..       *p++ = 0xDF;
//..       *p++ = 0xE0;
//..       /*  movl %eax, %dst */
//..       *p++ = 0x89;
//..       p = doAMode_R(p, hregX86_EAX(), i->Xin.FpCmp.dst);
//..       goto done;
//.. 
//..    case Xin_SseConst: {
//..       UShort con = i->Xin.SseConst.con;
//..       p = push_word_from_tags(p, (con >> 12) & 0xF);
//..       p = push_word_from_tags(p, (con >> 8) & 0xF);
//..       p = push_word_from_tags(p, (con >> 4) & 0xF);
//..       p = push_word_from_tags(p, con & 0xF);
//..       /* movl (%esp), %xmm-dst */
//..       *p++ = 0x0F;
//..       *p++ = 0x10;
//..       *p++ = 0x04 + 8 * (7 & vregNo(i->Xin.SseConst.dst));
//..       *p++ = 0x24;
//..       /* addl $16, %esp */
//..       *p++ = 0x83;
//..       *p++ = 0xC4;
//..       *p++ = 0x10;
//..       goto done;
//..    }
//.. 
//..    case Xin_SseLdSt:
//..       *p++ = 0x0F; 
//..       *p++ = i->Xin.SseLdSt.isLoad ? 0x10 : 0x11;
//..       p = doAMode_M(p, fake(vregNo(i->Xin.SseLdSt.reg)), i->Xin.SseLdSt.addr);
//..       goto done;
//.. 
//..    case Xin_SseLdzLO:
//..       vassert(i->Xin.SseLdzLO.sz == 4 || i->Xin.SseLdzLO.sz == 8);
//..       /* movs[sd] amode, %xmm-dst */
//..       *p++ = i->Xin.SseLdzLO.sz==4 ? 0xF3 : 0xF2;
//..       *p++ = 0x0F; 
//..       *p++ = 0x10; 
//..       p = doAMode_M(p, fake(vregNo(i->Xin.SseLdzLO.reg)), 
//..                        i->Xin.SseLdzLO.addr);
//..       goto done;
//.. 
//..    case Xin_Sse32Fx4:
//..       xtra = 0;
//..       *p++ = 0x0F;
//..       switch (i->Xin.Sse32Fx4.op) {
//..          case Xsse_ADDF:   *p++ = 0x58; break;
//..          case Xsse_DIVF:   *p++ = 0x5E; break;
//..          case Xsse_MAXF:   *p++ = 0x5F; break;
//..          case Xsse_MINF:   *p++ = 0x5D; break;
//..          case Xsse_MULF:   *p++ = 0x59; break;
//..          case Xsse_RCPF:   *p++ = 0x53; break;
//..          case Xsse_RSQRTF: *p++ = 0x52; break;
//..          case Xsse_SQRTF:  *p++ = 0x51; break;
//..          case Xsse_SUBF:   *p++ = 0x5C; break;
//..          case Xsse_CMPEQF: *p++ = 0xC2; xtra = 0x100; break;
//..          case Xsse_CMPLTF: *p++ = 0xC2; xtra = 0x101; break;
//..          case Xsse_CMPLEF: *p++ = 0xC2; xtra = 0x102; break;
//..          default: goto bad;
//..       }
//..       p = doAMode_R(p, fake(vregNo(i->Xin.Sse32Fx4.dst)),
//..                        fake(vregNo(i->Xin.Sse32Fx4.src)) );
//..       if (xtra & 0x100)
//..          *p++ = (UChar)(xtra & 0xFF);
//..       goto done;
//.. 
//..    case Xin_Sse64Fx2:
//..       xtra = 0;
//..       *p++ = 0x66;
//..       *p++ = 0x0F;
//..       switch (i->Xin.Sse64Fx2.op) {
//..          case Xsse_ADDF:   *p++ = 0x58; break;
//..          case Xsse_DIVF:   *p++ = 0x5E; break;
//..          case Xsse_MAXF:   *p++ = 0x5F; break;
//..          case Xsse_MINF:   *p++ = 0x5D; break;
//..          case Xsse_MULF:   *p++ = 0x59; break;
//..          case Xsse_RCPF:   *p++ = 0x53; break;
//..          case Xsse_RSQRTF: *p++ = 0x52; break;
//..          case Xsse_SQRTF:  *p++ = 0x51; break;
//..          case Xsse_SUBF:   *p++ = 0x5C; break;
//..          case Xsse_CMPEQF: *p++ = 0xC2; xtra = 0x100; break;
//..          case Xsse_CMPLTF: *p++ = 0xC2; xtra = 0x101; break;
//..          case Xsse_CMPLEF: *p++ = 0xC2; xtra = 0x102; break;
//..          default: goto bad;
//..       }
//..       p = doAMode_R(p, fake(vregNo(i->Xin.Sse64Fx2.dst)),
//..                        fake(vregNo(i->Xin.Sse64Fx2.src)) );
//..       if (xtra & 0x100)
//..          *p++ = (UChar)(xtra & 0xFF);
//..       goto done;
//.. 
//..    case Xin_Sse32FLo:
//..       xtra = 0;
//..       *p++ = 0xF3;
//..       *p++ = 0x0F;
//..       switch (i->Xin.Sse32FLo.op) {
//..          case Xsse_ADDF:   *p++ = 0x58; break;
//..          case Xsse_DIVF:   *p++ = 0x5E; break;
//..          case Xsse_MAXF:   *p++ = 0x5F; break;
//..          case Xsse_MINF:   *p++ = 0x5D; break;
//..          case Xsse_MULF:   *p++ = 0x59; break;
//..          case Xsse_RCPF:   *p++ = 0x53; break;
//..          case Xsse_RSQRTF: *p++ = 0x52; break;
//..          case Xsse_SQRTF:  *p++ = 0x51; break;
//..          case Xsse_SUBF:   *p++ = 0x5C; break;
//..          case Xsse_CMPEQF: *p++ = 0xC2; xtra = 0x100; break;
//..          case Xsse_CMPLTF: *p++ = 0xC2; xtra = 0x101; break;
//..          case Xsse_CMPLEF: *p++ = 0xC2; xtra = 0x102; break;
//..          default: goto bad;
//..       }
//..       p = doAMode_R(p, fake(vregNo(i->Xin.Sse32FLo.dst)),
//..                        fake(vregNo(i->Xin.Sse32FLo.src)) );
//..       if (xtra & 0x100)
//..          *p++ = (UChar)(xtra & 0xFF);
//..       goto done;
//.. 
//..    case Xin_Sse64FLo:
//..       xtra = 0;
//..       *p++ = 0xF2;
//..       *p++ = 0x0F;
//..       switch (i->Xin.Sse64FLo.op) {
//..          case Xsse_ADDF:   *p++ = 0x58; break;
//..          case Xsse_DIVF:   *p++ = 0x5E; break;
//..          case Xsse_MAXF:   *p++ = 0x5F; break;
//..          case Xsse_MINF:   *p++ = 0x5D; break;
//..          case Xsse_MULF:   *p++ = 0x59; break;
//..          case Xsse_RCPF:   *p++ = 0x53; break;
//..          case Xsse_RSQRTF: *p++ = 0x52; break;
//..          case Xsse_SQRTF:  *p++ = 0x51; break;
//..          case Xsse_SUBF:   *p++ = 0x5C; break;
//..          case Xsse_CMPEQF: *p++ = 0xC2; xtra = 0x100; break;
//..          case Xsse_CMPLTF: *p++ = 0xC2; xtra = 0x101; break;
//..          case Xsse_CMPLEF: *p++ = 0xC2; xtra = 0x102; break;
//..          default: goto bad;
//..       }
//..       p = doAMode_R(p, fake(vregNo(i->Xin.Sse64FLo.dst)),
//..                        fake(vregNo(i->Xin.Sse64FLo.src)) );
//..       if (xtra & 0x100)
//..          *p++ = (UChar)(xtra & 0xFF);
//..       goto done;
//.. 
//..    case Xin_SseReRg:
//.. #     define XX(_n) *p++ = (_n)
//..       switch (i->Xin.SseReRg.op) {
//..          case Xsse_MOV:     /*movups*/ XX(0x0F); XX(0x10); break;
//..          case Xsse_OR:                 XX(0x0F); XX(0x56); break;
//..          case Xsse_XOR:                XX(0x0F); XX(0x57); break;
//..          case Xsse_AND:                XX(0x0F); XX(0x54); break;
//..          case Xsse_PACKSSD:  XX(0x66); XX(0x0F); XX(0x6B); break;
//..          case Xsse_PACKSSW:  XX(0x66); XX(0x0F); XX(0x63); break;
//..          case Xsse_PACKUSW:  XX(0x66); XX(0x0F); XX(0x67); break;
//..          case Xsse_ADD8:     XX(0x66); XX(0x0F); XX(0xFC); break;
//..          case Xsse_ADD16:    XX(0x66); XX(0x0F); XX(0xFD); break;
//..          case Xsse_ADD32:    XX(0x66); XX(0x0F); XX(0xFE); break;
//..          case Xsse_ADD64:    XX(0x66); XX(0x0F); XX(0xD4); break;
//..          case Xsse_QADD8S:   XX(0x66); XX(0x0F); XX(0xEC); break;
//..          case Xsse_QADD16S:  XX(0x66); XX(0x0F); XX(0xED); break;
//..          case Xsse_QADD8U:   XX(0x66); XX(0x0F); XX(0xDC); break;
//..          case Xsse_QADD16U:  XX(0x66); XX(0x0F); XX(0xDD); break;
//..          case Xsse_AVG8U:    XX(0x66); XX(0x0F); XX(0xE0); break;
//..          case Xsse_AVG16U:   XX(0x66); XX(0x0F); XX(0xE3); break;
//..          case Xsse_CMPEQ8:   XX(0x66); XX(0x0F); XX(0x74); break;
//..          case Xsse_CMPEQ16:  XX(0x66); XX(0x0F); XX(0x75); break;
//..          case Xsse_CMPEQ32:  XX(0x66); XX(0x0F); XX(0x76); break;
//..          case Xsse_CMPGT8S:  XX(0x66); XX(0x0F); XX(0x64); break;
//..          case Xsse_CMPGT16S: XX(0x66); XX(0x0F); XX(0x65); break;
//..          case Xsse_CMPGT32S: XX(0x66); XX(0x0F); XX(0x66); break;
//..          case Xsse_MAX16S:   XX(0x66); XX(0x0F); XX(0xEE); break;
//..          case Xsse_MAX8U:    XX(0x66); XX(0x0F); XX(0xDE); break;
//..          case Xsse_MIN16S:   XX(0x66); XX(0x0F); XX(0xEA); break;
//..          case Xsse_MIN8U:    XX(0x66); XX(0x0F); XX(0xDA); break;
//..          case Xsse_MULHI16U: XX(0x66); XX(0x0F); XX(0xE4); break;
//..          case Xsse_MULHI16S: XX(0x66); XX(0x0F); XX(0xE5); break;
//..          case Xsse_MUL16:    XX(0x66); XX(0x0F); XX(0xD5); break;
//..          case Xsse_SHL16:    XX(0x66); XX(0x0F); XX(0xF1); break;
//..          case Xsse_SHL32:    XX(0x66); XX(0x0F); XX(0xF2); break;
//..          case Xsse_SHL64:    XX(0x66); XX(0x0F); XX(0xF3); break;
//..          case Xsse_SAR16:    XX(0x66); XX(0x0F); XX(0xE1); break;
//..          case Xsse_SAR32:    XX(0x66); XX(0x0F); XX(0xE2); break;
//..          case Xsse_SHR16:    XX(0x66); XX(0x0F); XX(0xD1); break;
//..          case Xsse_SHR32:    XX(0x66); XX(0x0F); XX(0xD2); break;
//..          case Xsse_SHR64:    XX(0x66); XX(0x0F); XX(0xD3); break;
//..          case Xsse_SUB8:     XX(0x66); XX(0x0F); XX(0xF8); break;
//..          case Xsse_SUB16:    XX(0x66); XX(0x0F); XX(0xF9); break;
//..          case Xsse_SUB32:    XX(0x66); XX(0x0F); XX(0xFA); break;
//..          case Xsse_SUB64:    XX(0x66); XX(0x0F); XX(0xFB); break;
//..          case Xsse_QSUB8S:   XX(0x66); XX(0x0F); XX(0xE8); break;
//..          case Xsse_QSUB16S:  XX(0x66); XX(0x0F); XX(0xE9); break;
//..          case Xsse_QSUB8U:   XX(0x66); XX(0x0F); XX(0xD8); break;
//..          case Xsse_QSUB16U:  XX(0x66); XX(0x0F); XX(0xD9); break;
//..          case Xsse_UNPCKHB:  XX(0x66); XX(0x0F); XX(0x68); break;
//..          case Xsse_UNPCKHW:  XX(0x66); XX(0x0F); XX(0x69); break;
//..          case Xsse_UNPCKHD:  XX(0x66); XX(0x0F); XX(0x6A); break;
//..          case Xsse_UNPCKHQ:  XX(0x66); XX(0x0F); XX(0x6D); break;
//..          case Xsse_UNPCKLB:  XX(0x66); XX(0x0F); XX(0x60); break;
//..          case Xsse_UNPCKLW:  XX(0x66); XX(0x0F); XX(0x61); break;
//..          case Xsse_UNPCKLD:  XX(0x66); XX(0x0F); XX(0x62); break;
//..          case Xsse_UNPCKLQ:  XX(0x66); XX(0x0F); XX(0x6C); break;
//..          default: goto bad;
//..       }
//..       p = doAMode_R(p, fake(vregNo(i->Xin.SseReRg.dst)),
//..                        fake(vregNo(i->Xin.SseReRg.src)) );
//.. #     undef XX
//..       goto done;
//.. 
//..    case Xin_SseCMov:
//..       /* jmp fwds if !condition */
//..       *p++ = 0x70 + (i->Xin.SseCMov.cond ^ 1);
//..       *p++ = 0; /* # of bytes in the next bit, which we don't know yet */
//..       ptmp = p;
//.. 
//..       /* movaps %src, %dst */
//..       *p++ = 0x0F; 
//..       *p++ = 0x28; 
//..       p = doAMode_R(p, fake(vregNo(i->Xin.SseCMov.dst)),
//..                        fake(vregNo(i->Xin.SseCMov.src)) );
//.. 
//..       /* Fill in the jump offset. */
//..       *(ptmp-1) = p - ptmp;
//..       goto done;
//.. 
//..    case Xin_SseShuf:
//..       *p++ = 0x66; 
//..       *p++ = 0x0F; 
//..       *p++ = 0x70; 
//..       p = doAMode_R(p, fake(vregNo(i->Xin.SseShuf.dst)),
//..                        fake(vregNo(i->Xin.SseShuf.src)) );
//..       *p++ = (UChar)(i->Xin.SseShuf.order);
//..       goto done;

   default: 
      goto bad;
   }

  bad:
   ppPPC32Instr(i);
   vpanic("emit_PPC32Instr");
   /*NOTREACHED*/
   
  goto done;  // CAB: Rem to remove - Just reducing compiler warnings.
  done:
   vassert(p - &buf[0] <= 32);
   return p - &buf[0];

#  undef fake
}

/*---------------------------------------------------------------*/
/*--- end                                  host-ppc32/hdefs.c ---*/
/*---------------------------------------------------------------*/
