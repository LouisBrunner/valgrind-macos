
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (host-amd64/hdefs.c) is                       ---*/
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
#include "host-amd64/hdefs.h"


/* --------- Registers. --------- */

void ppHRegAMD64 ( HReg reg ) 
{
   Int r;
   static HChar* ireg64_names[16] 
     = { "%rax", "%rcx", "%rdx", "%rbx", "%rsp", "%rbp", "%rsi", "%rdi",
         "%r8",  "%r9",  "%r10", "%r11", "%r12", "%r13", "%r14", "%r15" };
   /* Be generic for all virtual regs. */
   if (hregIsVirtual(reg)) {
      ppHReg(reg);
      return;
   }
   /* But specific for real regs. */
   switch (hregClass(reg)) {
      case HRcInt64:
         r = hregNumber(reg);
         vassert(r >= 0 && r < 16);
         vex_printf("%s", ireg64_names[r]);
         return;
      case HRcFlt64:
         r = hregNumber(reg);
         vassert(r >= 0 && r < 6);
         vex_printf("%%fake%d", r);
         return;
      case HRcVec128:
         r = hregNumber(reg);
         vassert(r >= 0 && r < 16);
         vex_printf("%%xmm%d", r);
         return;
      default:
         vpanic("ppHRegAMD64");
   }
}

HReg hregAMD64_RAX ( void ) { return mkHReg( 0, HRcInt64, False); }
HReg hregAMD64_RCX ( void ) { return mkHReg( 1, HRcInt64, False); }
HReg hregAMD64_RDX ( void ) { return mkHReg( 2, HRcInt64, False); }
HReg hregAMD64_RBX ( void ) { return mkHReg( 3, HRcInt64, False); }
HReg hregAMD64_RSP ( void ) { return mkHReg( 4, HRcInt64, False); }
HReg hregAMD64_RBP ( void ) { return mkHReg( 5, HRcInt64, False); }
HReg hregAMD64_RSI ( void ) { return mkHReg( 6, HRcInt64, False); }
HReg hregAMD64_RDI ( void ) { return mkHReg( 7, HRcInt64, False); }
HReg hregAMD64_R8  ( void ) { return mkHReg( 8, HRcInt64, False); }
HReg hregAMD64_R9  ( void ) { return mkHReg( 9, HRcInt64, False); }
HReg hregAMD64_R10 ( void ) { return mkHReg(10, HRcInt64, False); }
HReg hregAMD64_R11 ( void ) { return mkHReg(11, HRcInt64, False); }
HReg hregAMD64_R12 ( void ) { return mkHReg(12, HRcInt64, False); }
HReg hregAMD64_R13 ( void ) { return mkHReg(13, HRcInt64, False); }
HReg hregAMD64_R14 ( void ) { return mkHReg(14, HRcInt64, False); }
HReg hregAMD64_R15 ( void ) { return mkHReg(15, HRcInt64, False); }

//.. HReg hregAMD64_FAKE0 ( void ) { return mkHReg(0, HRcFlt64, False); }
//.. HReg hregAMD64_FAKE1 ( void ) { return mkHReg(1, HRcFlt64, False); }
//.. HReg hregAMD64_FAKE2 ( void ) { return mkHReg(2, HRcFlt64, False); }
//.. HReg hregAMD64_FAKE3 ( void ) { return mkHReg(3, HRcFlt64, False); }
//.. HReg hregAMD64_FAKE4 ( void ) { return mkHReg(4, HRcFlt64, False); }
//.. HReg hregAMD64_FAKE5 ( void ) { return mkHReg(5, HRcFlt64, False); }
//.. 
HReg hregAMD64_XMM0  ( void ) { return mkHReg( 0, HRcVec128, False); }
HReg hregAMD64_XMM1  ( void ) { return mkHReg( 1, HRcVec128, False); }
HReg hregAMD64_XMM2  ( void ) { return mkHReg( 2, HRcVec128, False); }
HReg hregAMD64_XMM3  ( void ) { return mkHReg( 3, HRcVec128, False); }
HReg hregAMD64_XMM4  ( void ) { return mkHReg( 4, HRcVec128, False); }
HReg hregAMD64_XMM5  ( void ) { return mkHReg( 5, HRcVec128, False); }
HReg hregAMD64_XMM6  ( void ) { return mkHReg( 6, HRcVec128, False); }
HReg hregAMD64_XMM7  ( void ) { return mkHReg( 7, HRcVec128, False); }
HReg hregAMD64_XMM8  ( void ) { return mkHReg( 8, HRcVec128, False); }
HReg hregAMD64_XMM9  ( void ) { return mkHReg( 9, HRcVec128, False); }
HReg hregAMD64_XMM10 ( void ) { return mkHReg(10, HRcVec128, False); }
HReg hregAMD64_XMM11 ( void ) { return mkHReg(11, HRcVec128, False); }
HReg hregAMD64_XMM12 ( void ) { return mkHReg(12, HRcVec128, False); }
HReg hregAMD64_XMM13 ( void ) { return mkHReg(13, HRcVec128, False); }
HReg hregAMD64_XMM14 ( void ) { return mkHReg(14, HRcVec128, False); }
HReg hregAMD64_XMM15 ( void ) { return mkHReg(15, HRcVec128, False); }


void getAllocableRegs_AMD64 ( Int* nregs, HReg** arr )
{
   *nregs = 30;
   *arr = LibVEX_Alloc(*nregs * sizeof(HReg));

   (*arr)[ 0] = hregAMD64_RAX();
   (*arr)[ 1] = hregAMD64_RBX();
   (*arr)[ 2] = hregAMD64_RCX();
   (*arr)[ 3] = hregAMD64_RDX();
   (*arr)[ 4] = hregAMD64_RSI();
   (*arr)[ 5] = hregAMD64_RDI();
   (*arr)[ 6] = hregAMD64_R8();
   (*arr)[ 7] = hregAMD64_R9();
   (*arr)[ 8] = hregAMD64_R10();
   (*arr)[ 9] = hregAMD64_R11();
   (*arr)[10] = hregAMD64_R12();
   (*arr)[11] = hregAMD64_R13();
   (*arr)[12] = hregAMD64_R14();
   (*arr)[13] = hregAMD64_R15();

   //   (*arr)[6] = hregAMD64_FAKE0();
   //(*arr)[7] = hregAMD64_FAKE1();
   //(*arr)[8] = hregAMD64_FAKE2();
   //(*arr)[9] = hregAMD64_FAKE3();
   //(*arr)[10] = hregAMD64_FAKE4();
   //(*arr)[11] = hregAMD64_FAKE5();
   (*arr)[14] = hregAMD64_XMM0();
   (*arr)[15] = hregAMD64_XMM1();
   (*arr)[16] = hregAMD64_XMM2();
   (*arr)[17] = hregAMD64_XMM3();
   (*arr)[18] = hregAMD64_XMM4();
   (*arr)[19] = hregAMD64_XMM5();
   (*arr)[20] = hregAMD64_XMM6();
   (*arr)[21] = hregAMD64_XMM7();
   (*arr)[22] = hregAMD64_XMM8();
   (*arr)[23] = hregAMD64_XMM9();
   (*arr)[24] = hregAMD64_XMM10();
   (*arr)[25] = hregAMD64_XMM11();
   (*arr)[26] = hregAMD64_XMM12();
   (*arr)[27] = hregAMD64_XMM13();
   (*arr)[28] = hregAMD64_XMM14();
   (*arr)[29] = hregAMD64_XMM15();
}


/* --------- Condition codes, Intel encoding. --------- */

HChar* showAMD64CondCode ( AMD64CondCode cond )
{
   switch (cond) {
      case Acc_O:      return "o";
      case Acc_NO:     return "no";
      case Acc_B:      return "b";
      case Acc_NB:     return "nb";
      case Acc_Z:      return "z";
      case Acc_NZ:     return "nz";
      case Acc_BE:     return "be";
      case Acc_NBE:    return "nbe";
      case Acc_S:      return "s";
      case Acc_NS:     return "ns";
      case Acc_P:      return "p";
      case Acc_NP:     return "np";
      case Acc_L:      return "l";
      case Acc_NL:     return "nl";
      case Acc_LE:     return "le";
      case Acc_NLE:    return "nle";
      case Acc_ALWAYS: return "ALWAYS";
      default: vpanic("ppAMD64CondCode");
   }
}


/* --------- AMD64AMode: memory address expressions. --------- */

AMD64AMode* AMD64AMode_IR ( UInt imm32, HReg reg ) {
   AMD64AMode* am = LibVEX_Alloc(sizeof(AMD64AMode));
   am->tag        = Aam_IR;
   am->Aam.IR.imm = imm32;
   am->Aam.IR.reg = reg;
   return am;
}
AMD64AMode* AMD64AMode_IRRS ( UInt imm32, HReg base, HReg indEx, Int shift ) {
   AMD64AMode* am = LibVEX_Alloc(sizeof(AMD64AMode));
   am->tag = Aam_IRRS;
   am->Aam.IRRS.imm   = imm32;
   am->Aam.IRRS.base  = base;
   am->Aam.IRRS.index = indEx;
   am->Aam.IRRS.shift = shift;
   vassert(shift >= 0 && shift <= 3);
   return am;
}

//.. AMD64AMode* dopyAMD64AMode ( AMD64AMode* am ) {
//..    switch (am->tag) {
//..       case Xam_IR: 
//..          return AMD64AMode_IR( am->Xam.IR.imm, am->Xam.IR.reg );
//..       case Xam_IRRS: 
//..          return AMD64AMode_IRRS( am->Xam.IRRS.imm, am->Xam.IRRS.base, 
//..                                am->Xam.IRRS.index, am->Xam.IRRS.shift );
//..       default:
//..          vpanic("dopyAMD64AMode");
//..    }
//.. }

void ppAMD64AMode ( AMD64AMode* am ) {
   switch (am->tag) {
      case Aam_IR: 
         if (am->Aam.IR.imm == 0)
            vex_printf("(");
         else
            vex_printf("0x%x(", am->Aam.IR.imm);
         ppHRegAMD64(am->Aam.IR.reg);
         vex_printf(")");
         return;
      case Aam_IRRS:
         vex_printf("0x%x(", am->Aam.IRRS.imm);
         ppHRegAMD64(am->Aam.IRRS.base);
         vex_printf(",");
         ppHRegAMD64(am->Aam.IRRS.index);
         vex_printf(",%d)", 1 << am->Aam.IRRS.shift);
         return;
      default:
         vpanic("ppAMD64AMode");
   }
}

static void addRegUsage_AMD64AMode ( HRegUsage* u, AMD64AMode* am ) {
   switch (am->tag) {
      case Aam_IR: 
         addHRegUse(u, HRmRead, am->Aam.IR.reg);
         return;
      case Aam_IRRS:
         addHRegUse(u, HRmRead, am->Aam.IRRS.base);
         addHRegUse(u, HRmRead, am->Aam.IRRS.index);
         return;
      default:
         vpanic("addRegUsage_AMD64AMode");
   }
}

static void mapRegs_AMD64AMode ( HRegRemap* m, AMD64AMode* am ) {
   switch (am->tag) {
      case Aam_IR: 
         am->Aam.IR.reg = lookupHRegRemap(m, am->Aam.IR.reg);
         return;
      case Aam_IRRS:
         am->Aam.IRRS.base = lookupHRegRemap(m, am->Aam.IRRS.base);
         am->Aam.IRRS.index = lookupHRegRemap(m, am->Aam.IRRS.index);
         return;
      default:
         vpanic("mapRegs_AMD64AMode");
   }
}

/* --------- Operand, which can be reg, immediate or memory. --------- */

AMD64RMI* AMD64RMI_Imm ( UInt imm32 ) {
   AMD64RMI* op       = LibVEX_Alloc(sizeof(AMD64RMI));
   op->tag            = Armi_Imm;
   op->Armi.Imm.imm32 = imm32;
   return op;
}
AMD64RMI* AMD64RMI_Reg ( HReg reg ) {
   AMD64RMI* op     = LibVEX_Alloc(sizeof(AMD64RMI));
   op->tag          = Armi_Reg;
   op->Armi.Reg.reg = reg;
   return op;
}
AMD64RMI* AMD64RMI_Mem ( AMD64AMode* am ) {
   AMD64RMI* op    = LibVEX_Alloc(sizeof(AMD64RMI));
   op->tag         = Armi_Mem;
   op->Armi.Mem.am = am;
   return op;
}

void ppAMD64RMI ( AMD64RMI* op ) {
   switch (op->tag) {
      case Armi_Imm: 
         vex_printf("$0x%x", op->Armi.Imm.imm32);
         return;
      case Armi_Reg: 
         ppHRegAMD64(op->Armi.Reg.reg);
         return;
      case Armi_Mem: 
         ppAMD64AMode(op->Armi.Mem.am);
         return;
     default: 
         vpanic("ppAMD64RMI");
   }
}

/* An AMD64RMI can only be used in a "read" context (what would it mean
   to write or modify a literal?) and so we enumerate its registers
   accordingly. */
static void addRegUsage_AMD64RMI ( HRegUsage* u, AMD64RMI* op ) {
   switch (op->tag) {
      case Armi_Imm: 
         return;
      case Armi_Reg: 
         addHRegUse(u, HRmRead, op->Armi.Reg.reg);
         return;
      case Armi_Mem: 
         addRegUsage_AMD64AMode(u, op->Armi.Mem.am);
         return;
      default: 
         vpanic("addRegUsage_AMD64RMI");
   }
}

static void mapRegs_AMD64RMI ( HRegRemap* m, AMD64RMI* op ) {
   switch (op->tag) {
      case Armi_Imm: 
         return;
      case Armi_Reg: 
         op->Armi.Reg.reg = lookupHRegRemap(m, op->Armi.Reg.reg);
         return;
      case Armi_Mem: 
         mapRegs_AMD64AMode(m, op->Armi.Mem.am);
         return;
      default: 
         vpanic("mapRegs_AMD64RMI");
   }
}


/* --------- Operand, which can be reg or immediate only. --------- */

AMD64RI* AMD64RI_Imm ( UInt imm32 ) {
   AMD64RI* op       = LibVEX_Alloc(sizeof(AMD64RI));
   op->tag           = Ari_Imm;
   op->Ari.Imm.imm32 = imm32;
   return op;
}
AMD64RI* AMD64RI_Reg ( HReg reg ) {
   AMD64RI* op     = LibVEX_Alloc(sizeof(AMD64RI));
   op->tag         = Ari_Reg;
   op->Ari.Reg.reg = reg;
   return op;
}

void ppAMD64RI ( AMD64RI* op ) {
   switch (op->tag) {
      case Ari_Imm: 
         vex_printf("$0x%x", op->Ari.Imm.imm32);
         return;
      case Ari_Reg: 
         ppHRegAMD64(op->Ari.Reg.reg);
         return;
     default: 
         vpanic("ppAMD64RI");
   }
}

/* An AMD64RI can only be used in a "read" context (what would it mean
   to write or modify a literal?) and so we enumerate its registers
   accordingly. */
static void addRegUsage_AMD64RI ( HRegUsage* u, AMD64RI* op ) {
   switch (op->tag) {
      case Ari_Imm: 
         return;
      case Ari_Reg: 
         addHRegUse(u, HRmRead, op->Ari.Reg.reg);
         return;
      default: 
         vpanic("addRegUsage_AMD64RI");
   }
}

static void mapRegs_AMD64RI ( HRegRemap* m, AMD64RI* op ) {
   switch (op->tag) {
      case Ari_Imm: 
         return;
      case Ari_Reg: 
         op->Ari.Reg.reg = lookupHRegRemap(m, op->Ari.Reg.reg);
         return;
      default: 
         vpanic("mapRegs_AMD64RI");
   }
}


/* --------- Operand, which can be reg or memory only. --------- */

AMD64RM* AMD64RM_Reg ( HReg reg ) {
   AMD64RM* op       = LibVEX_Alloc(sizeof(AMD64RM));
   op->tag         = Arm_Reg;
   op->Arm.Reg.reg = reg;
   return op;
}
//.. AMD64RM* AMD64RM_Mem ( AMD64AMode* am ) {
//..    AMD64RM* op      = LibVEX_Alloc(sizeof(AMD64RM));
//..    op->tag        = Xrm_Mem;
//..    op->Xrm.Mem.am = am;
//..    return op;
//.. }

void ppAMD64RM ( AMD64RM* op ) {
   switch (op->tag) {
      case Arm_Mem: 
         ppAMD64AMode(op->Arm.Mem.am);
         return;
      case Arm_Reg: 
         ppHRegAMD64(op->Arm.Reg.reg);
         return;
     default: 
         vpanic("ppAMD64RM");
   }
}

/* Because an AMD64RM can be both a source or destination operand, we
   have to supply a mode -- pertaining to the operand as a whole --
   indicating how it's being used. */
static void addRegUsage_AMD64RM ( HRegUsage* u, AMD64RM* op, HRegMode mode ) {
   switch (op->tag) {
      case Arm_Mem: 
         /* Memory is read, written or modified.  So we just want to
            know the regs read by the amode. */
         addRegUsage_AMD64AMode(u, op->Arm.Mem.am);
         return;
      case Arm_Reg: 
         /* reg is read, written or modified.  Add it in the
            appropriate way. */
         addHRegUse(u, mode, op->Arm.Reg.reg);
         return;
     default: 
         vpanic("addRegUsage_AMD64RM");
   }
}

static void mapRegs_AMD64RM ( HRegRemap* m, AMD64RM* op )
{
   switch (op->tag) {
      case Arm_Mem: 
         mapRegs_AMD64AMode(m, op->Arm.Mem.am);
         return;
      case Arm_Reg: 
         op->Arm.Reg.reg = lookupHRegRemap(m, op->Arm.Reg.reg);
         return;
     default: 
         vpanic("mapRegs_AMD64RM");
   }
}


//.. /* --------- Instructions. --------- */
//.. 
//.. HChar* showAMD64ScalarSz ( AMD64ScalarSz sz ) {
//..    switch (sz) {
//..       case Xss_16: return "w";
//..       case Xss_32: return "l";
//..       default: vpanic("showAMD64ScalarSz");
//..    }
//.. }
//.. 
//.. HChar* showAMD64UnaryOp ( AMD64UnaryOp op ) {
//..    switch (op) {
//..       case Xun_NOT: return "not";
//..       case Xun_NEG: return "neg";
//..       default: vpanic("showAMD64UnaryOp");
//..    }
//.. }

HChar* showAMD64AluOp ( AMD64AluOp op ) {
   switch (op) {
      case Aalu_MOV:  return "mov";
      case Aalu_CMP:  return "cmp";
      case Aalu_ADD:  return "add";
      case Aalu_SUB:  return "sub";
      case Aalu_ADC:  return "adc";
      case Aalu_SBB:  return "sbb";
      case Aalu_AND:  return "and";
      case Aalu_OR:   return "or";
      case Aalu_XOR:  return "xor";
      case Aalu_MUL:  return "mul";
      default: vpanic("showAMD64AluOp");
   }
}

HChar* showAMD64ShiftOp ( AMD64ShiftOp op ) {
   switch (op) {
      case Ash_SHL: return "shl";
      case Ash_SHR: return "shr";
      case Ash_SAR: return "sar";
      default: vpanic("showAMD64ShiftOp");
   }
}

//.. HChar* showAMD64FpOp ( AMD64FpOp op ) {
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
//..       default: vpanic("showAMD64FpOp");
//..    }
//.. }
//.. 
//.. HChar* showAMD64SseOp ( AMD64SseOp op ) {
//..    switch (op) {
//..       case Xsse_MOV:      return "mov(?!)";
//..       case Xsse_ADDF:     return "add";
//..       case Xsse_SUBF:     return "sub";
//..       case Xsse_MULF:     return "mul";
//..       case Xsse_DIVF:     return "div";
//..       case Xsse_MAXF:     return "max";
//..       case Xsse_MINF:     return "min";
//..       case Xsse_CMPEQF:   return "cmpFeq";
//..       case Xsse_CMPLTF:   return "cmpFlt";
//..       case Xsse_CMPLEF:   return "cmpFle";
//..       case Xsse_CMPUNF:   return "cmpFun";
//..       case Xsse_RCPF:     return "rcp";
//..       case Xsse_RSQRTF:   return "rsqrt";
//..       case Xsse_SQRTF:    return "sqrt";
//..       case Xsse_AND:      return "and";
//..       case Xsse_OR:       return "or";
//..       case Xsse_XOR:      return "xor";
//..       case Xsse_ANDN:     return "andn";
//..       case Xsse_ADD8:     return "paddb";
//..       case Xsse_ADD16:    return "paddw";
//..       case Xsse_ADD32:    return "paddd";
//..       case Xsse_ADD64:    return "paddq";
//..       case Xsse_QADD8U:   return "paddusb";
//..       case Xsse_QADD16U:  return "paddusw";
//..       case Xsse_QADD8S:   return "paddsb";
//..       case Xsse_QADD16S:  return "paddsw";
//..       case Xsse_SUB8:     return "psubb";
//..       case Xsse_SUB16:    return "psubw";
//..       case Xsse_SUB32:    return "psubd";
//..       case Xsse_SUB64:    return "psubq";
//..       case Xsse_QSUB8U:   return "psubusb";
//..       case Xsse_QSUB16U:  return "psubusw";
//..       case Xsse_QSUB8S:   return "psubsb";
//..       case Xsse_QSUB16S:  return "psubsw";
//..       case Xsse_MUL16:    return "pmullw";
//..       case Xsse_MULHI16U: return "pmulhuw";
//..       case Xsse_MULHI16S: return "pmulhw";
//..       case Xsse_AVG8U:    return "pavgb";
//..       case Xsse_AVG16U:   return "pavgw";
//..       case Xsse_MAX16S:   return "pmaxw";
//..       case Xsse_MAX8U:    return "pmaxub";
//..       case Xsse_MIN16S:   return "pminw";
//..       case Xsse_MIN8U:    return "pminub";
//..       case Xsse_CMPEQ8:   return "pcmpeqb";
//..       case Xsse_CMPEQ16:  return "pcmpeqw";
//..       case Xsse_CMPEQ32:  return "pcmpeqd";
//..       case Xsse_CMPGT8S:  return "pcmpgtb";
//..       case Xsse_CMPGT16S: return "pcmpgtw";
//..       case Xsse_CMPGT32S: return "pcmpgtd";
//..       case Xsse_SHL16:    return "psllw";
//..       case Xsse_SHL32:    return "pslld";
//..       case Xsse_SHL64:    return "psllq";
//..       case Xsse_SHR16:    return "psrlw";
//..       case Xsse_SHR32:    return "psrld";
//..       case Xsse_SHR64:    return "psrlq";
//..       case Xsse_SAR16:    return "psraw";
//..       case Xsse_SAR32:    return "psrad";
//..       case Xsse_PACKSSD:  return "packssdw";
//..       case Xsse_PACKSSW:  return "packsswb";
//..       case Xsse_PACKUSW:  return "packuswb";
//..       case Xsse_UNPCKHB:  return "punpckhb";
//..       case Xsse_UNPCKHW:  return "punpckhw";
//..       case Xsse_UNPCKHD:  return "punpckhd";
//..       case Xsse_UNPCKHQ:  return "punpckhq";
//..       case Xsse_UNPCKLB:  return "punpcklb";
//..       case Xsse_UNPCKLW:  return "punpcklw";
//..       case Xsse_UNPCKLD:  return "punpckld";
//..       case Xsse_UNPCKLQ:  return "punpcklq";
//..       default: vpanic("showAMD64SseOp");
//..    }
//.. }

AMD64Instr* AMD64Instr_Alu64R ( AMD64AluOp op, AMD64RMI* src, HReg dst ) {
   AMD64Instr* i     = LibVEX_Alloc(sizeof(AMD64Instr));
   i->tag            = Ain_Alu64R;
   i->Ain.Alu64R.op  = op;
   i->Ain.Alu64R.src = src;
   i->Ain.Alu64R.dst = dst;
   return i;
}
AMD64Instr* AMD64Instr_Alu64M ( AMD64AluOp op, AMD64RI* src, AMD64AMode* dst ) {
   AMD64Instr* i     = LibVEX_Alloc(sizeof(AMD64Instr));
   i->tag            = Ain_Alu64M;
   i->Ain.Alu64M.op  = op;
   i->Ain.Alu64M.src = src;
   i->Ain.Alu64M.dst = dst;
   vassert(op != Aalu_MUL);
   return i;
}
AMD64Instr* AMD64Instr_Sh64 ( AMD64ShiftOp op, UInt src, AMD64RM* dst ) {
   AMD64Instr* i   = LibVEX_Alloc(sizeof(AMD64Instr));
   i->tag          = Ain_Sh64;
   i->Ain.Sh64.op  = op;
   i->Ain.Sh64.src = src;
   i->Ain.Sh64.dst = dst;
   return i;
}
//.. AMD64Instr* AMD64Instr_Test32  ( AMD64RI* src, AMD64RM* dst ) {
//..    AMD64Instr* i       = LibVEX_Alloc(sizeof(AMD64Instr));
//..    i->tag            = Xin_Test32;
//..    i->Xin.Test32.src = src;
//..    i->Xin.Test32.dst = dst;
//..    return i;
//.. }
//.. AMD64Instr* AMD64Instr_Unary32  ( AMD64UnaryOp op, AMD64RM* dst ) {
//..    AMD64Instr* i        = LibVEX_Alloc(sizeof(AMD64Instr));
//..    i->tag             = Xin_Unary32;
//..    i->Xin.Unary32.op  = op;
//..    i->Xin.Unary32.dst = dst;
//..    return i;
//.. }
//.. AMD64Instr* AMD64Instr_MulL ( Bool syned, AMD64ScalarSz ssz , AMD64RM* src ) {
//..    AMD64Instr* i        = LibVEX_Alloc(sizeof(AMD64Instr));
//..    i->tag             = Xin_MulL;
//..    i->Xin.MulL.syned  = syned;
//..    i->Xin.MulL.ssz    = ssz;
//..    i->Xin.MulL.src    = src;
//..    return i;
//.. }
//.. AMD64Instr* AMD64Instr_Div ( Bool syned, AMD64ScalarSz ssz, AMD64RM* src ) {
//..    AMD64Instr* i        = LibVEX_Alloc(sizeof(AMD64Instr));
//..    i->tag             = Xin_Div;
//..    i->Xin.Div.syned   = syned;
//..    i->Xin.Div.ssz     = ssz;
//..    i->Xin.Div.src     = src;
//..    return i;
//.. }
//.. AMD64Instr* AMD64Instr_Sh3232  ( AMD64ShiftOp op, UInt amt, HReg src, HReg dst ) {
//..    AMD64Instr* i       = LibVEX_Alloc(sizeof(AMD64Instr));
//..    i->tag            = Xin_Sh3232;
//..    i->Xin.Sh3232.op  = op;
//..    i->Xin.Sh3232.amt = amt;
//..    i->Xin.Sh3232.src = src;
//..    i->Xin.Sh3232.dst = dst;
//..    vassert(op == Xsh_SHL || op == Xsh_SHR);
//..    return i;
//.. }
//.. AMD64Instr* AMD64Instr_Push( AMD64RMI* src ) {
//..    AMD64Instr* i     = LibVEX_Alloc(sizeof(AMD64Instr));
//..    i->tag          = Xin_Push;
//..    i->Xin.Push.src = src;
//..    return i;
//.. }
//.. AMD64Instr* AMD64Instr_Call ( AMD64CondCode cond, Addr32 target, Int regparms ) {
//..    AMD64Instr* i          = LibVEX_Alloc(sizeof(AMD64Instr));
//..    i->tag               = Xin_Call;
//..    i->Xin.Call.cond     = cond;
//..    i->Xin.Call.target   = target;
//..    i->Xin.Call.regparms = regparms;
//..    vassert(regparms >= 0 && regparms <= 3);
//..    return i;
//.. }
AMD64Instr* AMD64Instr_Goto ( IRJumpKind jk, AMD64CondCode cond, AMD64RI* dst ) {
   AMD64Instr* i    = LibVEX_Alloc(sizeof(AMD64Instr));
   i->tag           = Ain_Goto;
   i->Ain.Goto.cond = cond;
   i->Ain.Goto.dst  = dst;
   i->Ain.Goto.jk   = jk;
   return i;
}
//.. AMD64Instr* AMD64Instr_CMov32  ( AMD64CondCode cond, AMD64RM* src, HReg dst ) {
//..    AMD64Instr* i        = LibVEX_Alloc(sizeof(AMD64Instr));
//..    i->tag             = Xin_CMov32;
//..    i->Xin.CMov32.cond = cond;
//..    i->Xin.CMov32.src  = src;
//..    i->Xin.CMov32.dst  = dst;
//..    vassert(cond != Xcc_ALWAYS);
//..    return i;
//.. }
AMD64Instr* AMD64Instr_MovZLQ ( HReg src, HReg dst ) {
   AMD64Instr* i     = LibVEX_Alloc(sizeof(AMD64Instr));
   i->tag            = Ain_MovZLQ;
   i->Ain.MovZLQ.src = src;
   i->Ain.MovZLQ.dst = dst;
   return i;
}
AMD64Instr* AMD64Instr_LoadEX ( UChar szSmall, Bool syned,
                                AMD64AMode* src, HReg dst ) {
   AMD64Instr* i         = LibVEX_Alloc(sizeof(AMD64Instr));
   i->tag                = Ain_LoadEX;
   i->Ain.LoadEX.szSmall = szSmall;
   i->Ain.LoadEX.syned   = syned;
   i->Ain.LoadEX.src     = src;
   i->Ain.LoadEX.dst     = dst;
   vassert(szSmall == 1 || szSmall == 2 || szSmall == 4);
   return i;
}
AMD64Instr* AMD64Instr_Store ( UChar sz, HReg src, AMD64AMode* dst ) {
   AMD64Instr* i    = LibVEX_Alloc(sizeof(AMD64Instr));
   i->tag           = Ain_Store;
   i->Ain.Store.sz  = sz;
   i->Ain.Store.src = src;
   i->Ain.Store.dst = dst;
   vassert(sz == 1 || sz == 2 || sz == 4);
   return i;
}
//.. AMD64Instr* AMD64Instr_Set32 ( AMD64CondCode cond, HReg dst ) {
//..    AMD64Instr* i       = LibVEX_Alloc(sizeof(AMD64Instr));
//..    i->tag            = Xin_Set32;
//..    i->Xin.Set32.cond = cond;
//..    i->Xin.Set32.dst  = dst;
//..    return i;
//.. }
//.. AMD64Instr* AMD64Instr_Bsfr32 ( Bool isFwds, HReg src, HReg dst ) {
//..    AMD64Instr* i          = LibVEX_Alloc(sizeof(AMD64Instr));
//..    i->tag               = Xin_Bsfr32;
//..    i->Xin.Bsfr32.isFwds = isFwds;
//..    i->Xin.Bsfr32.src    = src;
//..    i->Xin.Bsfr32.dst    = dst;
//..    return i;
//.. }
//.. AMD64Instr* AMD64Instr_MFence ( VexSubArch subarch )
//.. {
//..    AMD64Instr* i           = LibVEX_Alloc(sizeof(AMD64Instr));
//..    i->tag                = Xin_MFence;
//..    i->Xin.MFence.subarch = subarch;
//..    vassert(subarch == VexSubArchAMD64_sse0
//..            || subarch == VexSubArchAMD64_sse1
//..            || subarch == VexSubArchAMD64_sse2);
//..    return i;
//.. }
//.. 
//.. AMD64Instr* AMD64Instr_FpUnary ( AMD64FpOp op, HReg src, HReg dst ) {
//..    AMD64Instr* i        = LibVEX_Alloc(sizeof(AMD64Instr));
//..    i->tag             = Xin_FpUnary;
//..    i->Xin.FpUnary.op  = op;
//..    i->Xin.FpUnary.src = src;
//..    i->Xin.FpUnary.dst = dst;
//..    return i;
//.. }
//.. AMD64Instr* AMD64Instr_FpBinary ( AMD64FpOp op, HReg srcL, HReg srcR, HReg dst ) {
//..    AMD64Instr* i          = LibVEX_Alloc(sizeof(AMD64Instr));
//..    i->tag               = Xin_FpBinary;
//..    i->Xin.FpBinary.op   = op;
//..    i->Xin.FpBinary.srcL = srcL;
//..    i->Xin.FpBinary.srcR = srcR;
//..    i->Xin.FpBinary.dst  = dst;
//..    return i;
//.. }
//.. AMD64Instr* AMD64Instr_FpLdSt ( Bool isLoad, UChar sz, HReg reg, AMD64AMode* addr ) {
//..    AMD64Instr* i          = LibVEX_Alloc(sizeof(AMD64Instr));
//..    i->tag               = Xin_FpLdSt;
//..    i->Xin.FpLdSt.isLoad = isLoad;
//..    i->Xin.FpLdSt.sz     = sz;
//..    i->Xin.FpLdSt.reg    = reg;
//..    i->Xin.FpLdSt.addr   = addr;
//..    vassert(sz == 4 || sz == 8);
//..    return i;
//.. }
//.. AMD64Instr* AMD64Instr_FpLdStI ( Bool isLoad, UChar sz,  
//..                              HReg reg, AMD64AMode* addr ) {
//..    AMD64Instr* i           = LibVEX_Alloc(sizeof(AMD64Instr));
//..    i->tag                = Xin_FpLdStI;
//..    i->Xin.FpLdStI.isLoad = isLoad;
//..    i->Xin.FpLdStI.sz     = sz;
//..    i->Xin.FpLdStI.reg    = reg;
//..    i->Xin.FpLdStI.addr   = addr;
//..    vassert(sz == 2 || sz == 4 || sz == 8);
//..    return i;
//.. }
//.. AMD64Instr* AMD64Instr_Fp64to32 ( HReg src, HReg dst ) {
//..    AMD64Instr* i         = LibVEX_Alloc(sizeof(AMD64Instr));
//..    i->tag              = Xin_Fp64to32;
//..    i->Xin.Fp64to32.src = src;
//..    i->Xin.Fp64to32.dst = dst;
//..    return i;
//.. }
//.. AMD64Instr* AMD64Instr_FpCMov ( AMD64CondCode cond, HReg src, HReg dst ) {
//..    AMD64Instr* i        = LibVEX_Alloc(sizeof(AMD64Instr));
//..    i->tag             = Xin_FpCMov;
//..    i->Xin.FpCMov.cond = cond;
//..    i->Xin.FpCMov.src  = src;
//..    i->Xin.FpCMov.dst  = dst;
//..    vassert(cond != Xcc_ALWAYS);
//..    return i;
//.. }
//.. AMD64Instr* AMD64Instr_FpLdStCW ( Bool isLoad, AMD64AMode* addr ) {
//..    AMD64Instr* i            = LibVEX_Alloc(sizeof(AMD64Instr));
//..    i->tag                 = Xin_FpLdStCW;
//..    i->Xin.FpLdStCW.isLoad = isLoad;
//..    i->Xin.FpLdStCW.addr   = addr;
//..    return i;
//.. }
//.. AMD64Instr* AMD64Instr_FpStSW_AX ( void ) {
//..    AMD64Instr* i = LibVEX_Alloc(sizeof(AMD64Instr));
//..    i->tag      = Xin_FpStSW_AX;
//..    return i;
//.. }
//.. AMD64Instr* AMD64Instr_FpCmp ( HReg srcL, HReg srcR, HReg dst ) {
//..    AMD64Instr* i       = LibVEX_Alloc(sizeof(AMD64Instr));
//..    i->tag            = Xin_FpCmp;
//..    i->Xin.FpCmp.srcL = srcL;
//..    i->Xin.FpCmp.srcR = srcR;
//..    i->Xin.FpCmp.dst  = dst;
//..    return i;
//.. }
//.. 
//.. AMD64Instr* AMD64Instr_SseConst ( UShort con, HReg dst ) {
//..    AMD64Instr* i            = LibVEX_Alloc(sizeof(AMD64Instr));
//..    i->tag                 = Xin_SseConst;
//..    i->Xin.SseConst.con    = con;
//..    i->Xin.SseConst.dst    = dst;
//..    vassert(hregClass(dst) == HRcVec128);
//..    return i;
//.. }
//.. AMD64Instr* AMD64Instr_SseLdSt ( Bool isLoad, HReg reg, AMD64AMode* addr ) {
//..    AMD64Instr* i           = LibVEX_Alloc(sizeof(AMD64Instr));
//..    i->tag                = Xin_SseLdSt;
//..    i->Xin.SseLdSt.isLoad = isLoad;
//..    i->Xin.SseLdSt.reg    = reg;
//..    i->Xin.SseLdSt.addr   = addr;
//..    return i;
//.. }
//.. AMD64Instr* AMD64Instr_SseLdzLO  ( Int sz, HReg reg, AMD64AMode* addr )
//.. {
//..    AMD64Instr* i           = LibVEX_Alloc(sizeof(AMD64Instr));
//..    i->tag                = Xin_SseLdzLO;
//..    i->Xin.SseLdzLO.sz    = sz;
//..    i->Xin.SseLdzLO.reg   = reg;
//..    i->Xin.SseLdzLO.addr  = addr;
//..    vassert(sz == 4 || sz == 8);
//..    return i;
//.. }
//.. AMD64Instr* AMD64Instr_Sse32Fx4 ( AMD64SseOp op, HReg src, HReg dst ) {
//..    AMD64Instr* i         = LibVEX_Alloc(sizeof(AMD64Instr));
//..    i->tag              = Xin_Sse32Fx4;
//..    i->Xin.Sse32Fx4.op  = op;
//..    i->Xin.Sse32Fx4.src = src;
//..    i->Xin.Sse32Fx4.dst = dst;
//..    vassert(op != Xsse_MOV);
//..    return i;
//.. }
//.. AMD64Instr* AMD64Instr_Sse32FLo ( AMD64SseOp op, HReg src, HReg dst ) {
//..    AMD64Instr* i         = LibVEX_Alloc(sizeof(AMD64Instr));
//..    i->tag              = Xin_Sse32FLo;
//..    i->Xin.Sse32FLo.op  = op;
//..    i->Xin.Sse32FLo.src = src;
//..    i->Xin.Sse32FLo.dst = dst;
//..    vassert(op != Xsse_MOV);
//..    return i;
//.. }
//.. AMD64Instr* AMD64Instr_Sse64Fx2 ( AMD64SseOp op, HReg src, HReg dst ) {
//..    AMD64Instr* i         = LibVEX_Alloc(sizeof(AMD64Instr));
//..    i->tag              = Xin_Sse64Fx2;
//..    i->Xin.Sse64Fx2.op  = op;
//..    i->Xin.Sse64Fx2.src = src;
//..    i->Xin.Sse64Fx2.dst = dst;
//..    vassert(op != Xsse_MOV);
//..    return i;
//.. }
//.. AMD64Instr* AMD64Instr_Sse64FLo ( AMD64SseOp op, HReg src, HReg dst ) {
//..    AMD64Instr* i         = LibVEX_Alloc(sizeof(AMD64Instr));
//..    i->tag              = Xin_Sse64FLo;
//..    i->Xin.Sse64FLo.op  = op;
//..    i->Xin.Sse64FLo.src = src;
//..    i->Xin.Sse64FLo.dst = dst;
//..    vassert(op != Xsse_MOV);
//..    return i;
//.. }
//.. AMD64Instr* AMD64Instr_SseReRg ( AMD64SseOp op, HReg re, HReg rg ) {
//..    AMD64Instr* i        = LibVEX_Alloc(sizeof(AMD64Instr));
//..    i->tag             = Xin_SseReRg;
//..    i->Xin.SseReRg.op  = op;
//..    i->Xin.SseReRg.src = re;
//..    i->Xin.SseReRg.dst = rg;
//..    return i;
//.. }
//.. AMD64Instr* AMD64Instr_SseCMov ( AMD64CondCode cond, HReg src, HReg dst ) {
//..    AMD64Instr* i         = LibVEX_Alloc(sizeof(AMD64Instr));
//..    i->tag              = Xin_SseCMov;
//..    i->Xin.SseCMov.cond = cond;
//..    i->Xin.SseCMov.src  = src;
//..    i->Xin.SseCMov.dst  = dst;
//..    vassert(cond != Xcc_ALWAYS);
//..    return i;
//.. }
//.. AMD64Instr* AMD64Instr_SseShuf ( Int order, HReg src, HReg dst ) {
//..    AMD64Instr* i          = LibVEX_Alloc(sizeof(AMD64Instr));
//..    i->tag               = Xin_SseShuf;
//..    i->Xin.SseShuf.order = order;
//..    i->Xin.SseShuf.src   = src;
//..    i->Xin.SseShuf.dst   = dst;
//..    vassert(order >= 0 && order <= 0xFF);
//..    return i;
//.. }

void ppAMD64Instr ( AMD64Instr* i ) 
{
   switch (i->tag) {
      case Ain_Alu64R:
         vex_printf("%sq ", showAMD64AluOp(i->Ain.Alu64R.op));
         ppAMD64RMI(i->Ain.Alu64R.src);
         vex_printf(",");
         ppHRegAMD64(i->Ain.Alu64R.dst);
         return;
      case Ain_Alu64M:
         vex_printf("%sq ", showAMD64AluOp(i->Ain.Alu64M.op));
         ppAMD64RI(i->Ain.Alu64M.src);
         vex_printf(",");
         ppAMD64AMode(i->Ain.Alu64M.dst);
         return;
      case Ain_Sh64:
         vex_printf("%sl ", showAMD64ShiftOp(i->Ain.Sh64.op));
         if (i->Ain.Sh64.src == 0)
            vex_printf("%%cl,"); 
         else 
            vex_printf("$%d,", i->Ain.Sh64.src);
         ppAMD64RM(i->Ain.Sh64.dst);
         return;
//..       case Xin_Test32:
//..          vex_printf("testl ");
//..          ppAMD64RI(i->Xin.Test32.src);
//..          vex_printf(",");
//..          ppAMD64RM(i->Xin.Test32.dst);
//..          return;
//..       case Xin_Unary32:
//..          vex_printf("%sl ", showAMD64UnaryOp(i->Xin.Unary32.op));
//..          ppAMD64RM(i->Xin.Unary32.dst);
//..          return;
//..       case Xin_MulL:
//..          vex_printf("%cmul%s ",
//..                     i->Xin.MulL.syned ? 's' : 'u',
//..                     showAMD64ScalarSz(i->Xin.MulL.ssz));
//..          ppAMD64RM(i->Xin.MulL.src);
//..          return;
//..       case Xin_Div:
//..          vex_printf("%cdiv%s ",
//..                     i->Xin.Div.syned ? 's' : 'u',
//..                     showAMD64ScalarSz(i->Xin.Div.ssz));
//..          ppAMD64RM(i->Xin.Div.src);
//..          return;
//..       case Xin_Sh3232:
//..          vex_printf("%sdl ", showAMD64ShiftOp(i->Xin.Sh3232.op));
//..          if (i->Xin.Sh3232.amt == 0)
//..            vex_printf(" %%cl,"); 
//..          else 
//..             vex_printf(" $%d,", i->Xin.Sh3232.amt);
//..          ppHRegAMD64(i->Xin.Sh3232.src);
//..          vex_printf(",");
//..          ppHRegAMD64(i->Xin.Sh3232.dst);
//..          return;
//..       case Xin_Push:
//..          vex_printf("pushl ");
//..          ppAMD64RMI(i->Xin.Push.src);
//..          return;
//..       case Xin_Call:
//..          vex_printf("call%s[%d] ", 
//..                     i->Xin.Call.cond==Xcc_ALWAYS 
//..                        ? "" : showAMD64CondCode(i->Xin.Call.cond), 
//..                     i->Xin.Call.regparms);
//..          vex_printf("0x%x", i->Xin.Call.target);
//..          break;
      case Ain_Goto:
         if (i->Ain.Goto.cond != Acc_ALWAYS) {
            vex_printf("if (%%rflags.%s) { ", 
                       showAMD64CondCode(i->Ain.Goto.cond));
         }
         if (i->Ain.Goto.jk != Ijk_Boring) {
            vex_printf("movl $");
            ppIRJumpKind(i->Ain.Goto.jk);
            vex_printf(",%%rbp ; ");
         }
         vex_printf("movl ");
         ppAMD64RI(i->Ain.Goto.dst);
         vex_printf(",%%rax ; ret");
         if (i->Ain.Goto.cond != Acc_ALWAYS) {
            vex_printf(" }");
         }
         return;
//..       case Xin_CMov32:
//..          vex_printf("cmov%s ", showAMD64CondCode(i->Xin.CMov32.cond));
//..          ppAMD64RM(i->Xin.CMov32.src);
//..          vex_printf(",");
//..          ppHRegAMD64(i->Xin.CMov32.dst);
//..          return;
      case Ain_MovZLQ:
         vex_printf("movzlq ");
         ppHRegAMD64(i->Ain.MovZLQ.src);
         vex_printf(",");
         ppHRegAMD64(i->Ain.MovZLQ.dst);
         return;
      case Ain_LoadEX:
         vex_printf("mov%c%cq ",
                    i->Ain.LoadEX.syned ? 's' : 'z',
                    i->Ain.LoadEX.szSmall==1 
                       ? 'b' 
                       : (i->Ain.LoadEX.szSmall==2 ? 'w' : 'l'));
         ppAMD64AMode(i->Ain.LoadEX.src);
         vex_printf(",");
         ppHRegAMD64(i->Ain.LoadEX.dst);
         return;
//..       case Xin_Store:
//..          vex_printf("mov%c ", i->Xin.Store.sz==1 ? 'b' : 'w');
//..          ppHRegAMD64(i->Xin.Store.src);
//..          vex_printf(",");
//..          ppAMD64AMode(i->Xin.Store.dst);
//..          return;
//..       case Xin_Set32:
//..          vex_printf("setl%s ", showAMD64CondCode(i->Xin.Set32.cond));
//..          ppHRegAMD64(i->Xin.Set32.dst);
//..          return;
//..       case Xin_Bsfr32:
//..          vex_printf("bs%cl ", i->Xin.Bsfr32.isFwds ? 'f' : 'r');
//..          ppHRegAMD64(i->Xin.Bsfr32.src);
//..          vex_printf(",");
//..          ppHRegAMD64(i->Xin.Bsfr32.dst);
//..          return;
//..       case Xin_MFence:
//..          vex_printf("mfence(%s)",
//..                     LibVEX_ppVexSubArch(i->Xin.MFence.subarch));
//..          return;
//..       case Xin_FpUnary:
//..          vex_printf("g%sD ", showAMD64FpOp(i->Xin.FpUnary.op));
//..          ppHRegAMD64(i->Xin.FpUnary.src);
//..          vex_printf(",");
//..          ppHRegAMD64(i->Xin.FpUnary.dst);
//..          break;
//..       case Xin_FpBinary:
//..          vex_printf("g%sD ", showAMD64FpOp(i->Xin.FpBinary.op));
//..          ppHRegAMD64(i->Xin.FpBinary.srcL);
//..          vex_printf(",");
//..          ppHRegAMD64(i->Xin.FpBinary.srcR);
//..          vex_printf(",");
//..          ppHRegAMD64(i->Xin.FpBinary.dst);
//..          break;
//..       case Xin_FpLdSt:
//..          if (i->Xin.FpLdSt.isLoad) {
//..             vex_printf("gld%c " , i->Xin.FpLdSt.sz==8 ? 'D' : 'F');
//..             ppAMD64AMode(i->Xin.FpLdSt.addr);
//..             vex_printf(", ");
//..             ppHRegAMD64(i->Xin.FpLdSt.reg);
//..          } else {
//..             vex_printf("gst%c " , i->Xin.FpLdSt.sz==8 ? 'D' : 'F');
//..             ppHRegAMD64(i->Xin.FpLdSt.reg);
//..             vex_printf(", ");
//..             ppAMD64AMode(i->Xin.FpLdSt.addr);
//..          }
//..          return;
//..       case Xin_FpLdStI:
//..          if (i->Xin.FpLdStI.isLoad) {
//..             vex_printf("gild%s ", i->Xin.FpLdStI.sz==8 ? "ll" : 
//..                                   i->Xin.FpLdStI.sz==4 ? "l" : "w");
//..             ppAMD64AMode(i->Xin.FpLdStI.addr);
//..             vex_printf(", ");
//..             ppHRegAMD64(i->Xin.FpLdStI.reg);
//..          } else {
//..             vex_printf("gist%s ", i->Xin.FpLdStI.sz==8 ? "ll" : 
//..                                   i->Xin.FpLdStI.sz==4 ? "l" : "w");
//..             ppHRegAMD64(i->Xin.FpLdStI.reg);
//..             vex_printf(", ");
//..             ppAMD64AMode(i->Xin.FpLdStI.addr);
//..          }
//..          return;
//..       case Xin_Fp64to32:
//..          vex_printf("gdtof ");
//..          ppHRegAMD64(i->Xin.Fp64to32.src);
//..          vex_printf(",");
//..          ppHRegAMD64(i->Xin.Fp64to32.dst);
//..          return;
//..       case Xin_FpCMov:
//..          vex_printf("gcmov%s ", showAMD64CondCode(i->Xin.FpCMov.cond));
//..          ppHRegAMD64(i->Xin.FpCMov.src);
//..          vex_printf(",");
//..          ppHRegAMD64(i->Xin.FpCMov.dst);
//..          return;
//..       case Xin_FpLdStCW:
//..          vex_printf(i->Xin.FpLdStCW.isLoad ? "fldcw " : "fstcw ");
//..          ppAMD64AMode(i->Xin.FpLdStCW.addr);
//..          return;
//..       case Xin_FpStSW_AX:
//..          vex_printf("fstsw %%ax");
//..          return;
//..       case Xin_FpCmp:
//..          vex_printf("gcmp ");
//..          ppHRegAMD64(i->Xin.FpCmp.srcL);
//..          vex_printf(",");
//..          ppHRegAMD64(i->Xin.FpCmp.srcR);
//..          vex_printf(",");
//..          ppHRegAMD64(i->Xin.FpCmp.dst);
//..          break;
//..       case Xin_SseConst:
//..          vex_printf("const $0x%04x,", (Int)i->Xin.SseConst.con);
//..          ppHRegAMD64(i->Xin.SseConst.dst);
//..          break;
//..       case Xin_SseLdSt:
//..          vex_printf("movups ");
//..          if (i->Xin.SseLdSt.isLoad) {
//..             ppAMD64AMode(i->Xin.SseLdSt.addr);
//..             vex_printf(",");
//..             ppHRegAMD64(i->Xin.SseLdSt.reg);
//..          } else {
//..             ppHRegAMD64(i->Xin.SseLdSt.reg);
//..             vex_printf(",");
//..             ppAMD64AMode(i->Xin.SseLdSt.addr);
//..          }
//..          return;
//..       case Xin_SseLdzLO:
//..          vex_printf("movs%s ", i->Xin.SseLdzLO.sz==4 ? "s" : "d");
//..          ppAMD64AMode(i->Xin.SseLdzLO.addr);
//..          vex_printf(",");
//..          ppHRegAMD64(i->Xin.SseLdzLO.reg);
//..          return;
//..       case Xin_Sse32Fx4:
//..          vex_printf("%sps ", showAMD64SseOp(i->Xin.Sse32Fx4.op));
//..          ppHRegAMD64(i->Xin.Sse32Fx4.src);
//..          vex_printf(",");
//..          ppHRegAMD64(i->Xin.Sse32Fx4.dst);
//..          return;
//..       case Xin_Sse32FLo:
//..          vex_printf("%sss ", showAMD64SseOp(i->Xin.Sse32FLo.op));
//..          ppHRegAMD64(i->Xin.Sse32FLo.src);
//..          vex_printf(",");
//..          ppHRegAMD64(i->Xin.Sse32FLo.dst);
//..          return;
//..       case Xin_Sse64Fx2:
//..          vex_printf("%spd ", showAMD64SseOp(i->Xin.Sse64Fx2.op));
//..          ppHRegAMD64(i->Xin.Sse64Fx2.src);
//..          vex_printf(",");
//..          ppHRegAMD64(i->Xin.Sse64Fx2.dst);
//..          return;
//..       case Xin_Sse64FLo:
//..          vex_printf("%ssd ", showAMD64SseOp(i->Xin.Sse64FLo.op));
//..          ppHRegAMD64(i->Xin.Sse64FLo.src);
//..          vex_printf(",");
//..          ppHRegAMD64(i->Xin.Sse64FLo.dst);
//..          return;
//..       case Xin_SseReRg:
//..          vex_printf("%s ", showAMD64SseOp(i->Xin.SseReRg.op));
//..          ppHRegAMD64(i->Xin.SseReRg.src);
//..          vex_printf(",");
//..          ppHRegAMD64(i->Xin.SseReRg.dst);
//..          return;
//..       case Xin_SseCMov:
//..          vex_printf("cmov%s ", showAMD64CondCode(i->Xin.SseCMov.cond));
//..          ppHRegAMD64(i->Xin.SseCMov.src);
//..          vex_printf(",");
//..          ppHRegAMD64(i->Xin.SseCMov.dst);
//..          return;
//..       case Xin_SseShuf:
//..          vex_printf("pshufd $0x%x,", i->Xin.SseShuf.order);
//..          ppHRegAMD64(i->Xin.SseShuf.src);
//..          vex_printf(",");
//..          ppHRegAMD64(i->Xin.SseShuf.dst);
//..          return;

      default:
         vpanic("ppAMD64Instr");
   }
}

/* --------- Helpers for register allocation. --------- */

void getRegUsage_AMD64Instr ( HRegUsage* u, AMD64Instr* i )
{
  //   Bool unary;
   initHRegUsage(u);
   switch (i->tag) {
      case Ain_Alu64R:
         addRegUsage_AMD64RMI(u, i->Ain.Alu64R.src);
         if (i->Ain.Alu64R.op == Aalu_MOV) {
            addHRegUse(u, HRmWrite, i->Ain.Alu64R.dst);
            return;
         }
         if (i->Ain.Alu64R.op == Aalu_CMP) { 
            addHRegUse(u, HRmRead, i->Ain.Alu64R.dst);
            return;
         }
         addHRegUse(u, HRmModify, i->Ain.Alu64R.dst);
         return;
      case Ain_Alu64M:
         addRegUsage_AMD64RI(u, i->Ain.Alu64M.src);
         addRegUsage_AMD64AMode(u, i->Ain.Alu64M.dst);
         return;
      case Ain_Sh64:
         addRegUsage_AMD64RM(u, i->Ain.Sh64.dst, HRmModify);
         if (i->Ain.Sh64.src == 0)
            addHRegUse(u, HRmRead, hregAMD64_RCX());
         return;
//..       case Xin_Test32:
//..          addRegUsage_AMD64RI(u, i->Xin.Test32.src);
//..          addRegUsage_AMD64RM(u, i->Xin.Test32.dst, HRmRead);
//..          return;
//..       case Xin_Unary32:
//..          addRegUsage_AMD64RM(u, i->Xin.Unary32.dst, HRmModify);
//..          return;
//..       case Xin_MulL:
//..          addRegUsage_AMD64RM(u, i->Xin.MulL.src, HRmRead);
//..          addHRegUse(u, HRmModify, hregAMD64_EAX());
//..          addHRegUse(u, HRmWrite, hregAMD64_EDX());
//..          return;
//..       case Xin_Div:
//..          addRegUsage_AMD64RM(u, i->Xin.Div.src, HRmRead);
//..          addHRegUse(u, HRmModify, hregAMD64_EAX());
//..          addHRegUse(u, HRmModify, hregAMD64_EDX());
//..          return;
//..       case Xin_Sh3232:
//..          addHRegUse(u, HRmRead, i->Xin.Sh3232.src);
//..          addHRegUse(u, HRmModify, i->Xin.Sh3232.dst);
//..          if (i->Xin.Sh3232.amt == 0)
//..             addHRegUse(u, HRmRead, hregAMD64_ECX());
//..          return;
//..       case Xin_Push:
//..          addRegUsage_AMD64RMI(u, i->Xin.Push.src);
//..          addHRegUse(u, HRmModify, hregAMD64_ESP());
//..          return;
//..       case Xin_Call:
//..          /* This is a bit subtle. */
//..          /* First off, claim it trashes all the callee-saved regs */
//..          /* which I believe to be %eax,%ecx,%edx. */
//..          addHRegUse(u, HRmWrite, hregAMD64_EAX());
//..          addHRegUse(u, HRmWrite, hregAMD64_ECX());
//..          addHRegUse(u, HRmWrite, hregAMD64_EDX());
//..          /* Now we have to state any parameter-carrying registers
//..             which might be read.  This depends on the regparmness. */
//..          switch (i->Xin.Call.regparms) {
//..             case 3: addHRegUse(u, HRmRead, hregAMD64_ECX()); /*fallthru*/
//..             case 2: addHRegUse(u, HRmRead, hregAMD64_EDX()); /*fallthru*/
//..             case 1: addHRegUse(u, HRmRead, hregAMD64_EAX()); break;
//..             case 0: break;
//..             default: vpanic("getRegUsage_AMD64Instr:Call:regparms");
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
//..             addHRegUse(u, HRmWrite, hregAMD64_EDI());
//..          /* Upshot of this is that the assembler really must observe
//..             the here-stated convention of which register to use as an
//..             address temporary, depending on the regparmness: 0==EAX,
//..             1==EDX, 2==ECX, 3==EDI. */
//..          return;
      case Ain_Goto:
         addRegUsage_AMD64RI(u, i->Ain.Goto.dst);
         addHRegUse(u, HRmWrite, hregAMD64_RAX());
         if (i->Ain.Goto.jk != Ijk_Boring)
            addHRegUse(u, HRmWrite, hregAMD64_RBP());
         return;
//..       case Xin_CMov32:
//..          addRegUsage_AMD64RM(u, i->Xin.CMov32.src, HRmRead);
//..          addHRegUse(u, HRmModify, i->Xin.CMov32.dst);
//..          return;
      case Ain_MovZLQ:
         addHRegUse(u, HRmRead,  i->Ain.MovZLQ.src);
         addHRegUse(u, HRmWrite, i->Ain.MovZLQ.dst);
         return;
      case Ain_LoadEX:
         addRegUsage_AMD64AMode(u, i->Ain.LoadEX.src);
         addHRegUse(u, HRmWrite, i->Ain.LoadEX.dst);
         return;
//..       case Xin_Store:
//..          addHRegUse(u, HRmRead, i->Xin.Store.src);
//..          addRegUsage_AMD64AMode(u, i->Xin.Store.dst);
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
//..          addRegUsage_AMD64AMode(u, i->Xin.FpLdSt.addr);
//..          addHRegUse(u, i->Xin.FpLdSt.isLoad ? HRmWrite : HRmRead,
//..                        i->Xin.FpLdSt.reg);
//..          return;
//..       case Xin_FpLdStI:
//..          addRegUsage_AMD64AMode(u, i->Xin.FpLdStI.addr);
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
//..          addRegUsage_AMD64AMode(u, i->Xin.FpLdStCW.addr);
//..          return;
//..       case Xin_FpStSW_AX:
//..          addHRegUse(u, HRmWrite, hregAMD64_EAX());
//..          return;
//..       case Xin_FpCmp:
//..          addHRegUse(u, HRmRead, i->Xin.FpCmp.srcL);
//..          addHRegUse(u, HRmRead, i->Xin.FpCmp.srcR);
//..          addHRegUse(u, HRmWrite, i->Xin.FpCmp.dst);
//..          addHRegUse(u, HRmWrite, hregAMD64_EAX());
//..          return;
//..       case Xin_SseLdSt:
//..          addRegUsage_AMD64AMode(u, i->Xin.SseLdSt.addr);
//..          addHRegUse(u, i->Xin.SseLdSt.isLoad ? HRmWrite : HRmRead,
//..                        i->Xin.SseLdSt.reg);
//..          return;
//..       case Xin_SseLdzLO:
//..          addRegUsage_AMD64AMode(u, i->Xin.SseLdzLO.addr);
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
         ppAMD64Instr(i);
         vpanic("getRegUsage_AMD64Instr");
   }
}

/* local helper */
static void mapReg(HRegRemap* m, HReg* r)
{
   *r = lookupHRegRemap(m, *r);
}

void mapRegs_AMD64Instr ( HRegRemap* m, AMD64Instr* i )
{
   switch (i->tag) {
      case Ain_Alu64R:
         mapRegs_AMD64RMI(m, i->Ain.Alu64R.src);
         mapReg(m, &i->Ain.Alu64R.dst);
         return;
      case Ain_Alu64M:
         mapRegs_AMD64RI(m, i->Ain.Alu64M.src);
         mapRegs_AMD64AMode(m, i->Ain.Alu64M.dst);
         return;
      case Ain_Sh64:
         mapRegs_AMD64RM(m, i->Ain.Sh64.dst);
         return;
//..       case Xin_Test32:
//..          mapRegs_AMD64RI(m, i->Xin.Test32.src);
//..          mapRegs_AMD64RM(m, i->Xin.Test32.dst);
//..          return;
//..       case Xin_Unary32:
//..          mapRegs_AMD64RM(m, i->Xin.Unary32.dst);
//..          return;
//..       case Xin_MulL:
//..          mapRegs_AMD64RM(m, i->Xin.MulL.src);
//..          return;
//..       case Xin_Div:
//..          mapRegs_AMD64RM(m, i->Xin.Div.src);
//..          return;
//..       case Xin_Sh3232:
//..          mapReg(m, &i->Xin.Sh3232.src);
//..          mapReg(m, &i->Xin.Sh3232.dst);
//..          return;
//..       case Xin_Push:
//..          mapRegs_AMD64RMI(m, i->Xin.Push.src);
//..          return;
//..       case Xin_Call:
//..          return;
      case Ain_Goto:
         mapRegs_AMD64RI(m, i->Ain.Goto.dst);
         return;
//..       case Xin_CMov32:
//..          mapRegs_AMD64RM(m, i->Xin.CMov32.src);
//..          mapReg(m, &i->Xin.CMov32.dst);
//..          return;
      case Ain_MovZLQ:
         mapReg(m, &i->Ain.MovZLQ.src);
         mapReg(m, &i->Ain.MovZLQ.dst);
         return;
      case Ain_LoadEX:
         mapRegs_AMD64AMode(m, i->Ain.LoadEX.src);
         mapReg(m, &i->Ain.LoadEX.dst);
         return;
//..       case Xin_Store:
//..          mapReg(m, &i->Xin.Store.src);
//..          mapRegs_AMD64AMode(m, i->Xin.Store.dst);
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
//..          mapRegs_AMD64AMode(m, i->Xin.FpLdSt.addr);
//..          mapReg(m, &i->Xin.FpLdSt.reg);
//..          return;
//..       case Xin_FpLdStI:
//..          mapRegs_AMD64AMode(m, i->Xin.FpLdStI.addr);
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
//..          mapRegs_AMD64AMode(m, i->Xin.FpLdStCW.addr);
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
//..          mapRegs_AMD64AMode(m, i->Xin.SseLdSt.addr);
//..          break;
//..       case Xin_SseLdzLO:
//..          mapReg(m, &i->Xin.SseLdzLO.reg);
//..          mapRegs_AMD64AMode(m, i->Xin.SseLdzLO.addr);
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
         ppAMD64Instr(i);
         vpanic("mapRegs_AMD64Instr");
   }
}

/* Figure out if i represents a reg-reg move, and if so assign the
   source and destination to *src and *dst.  If in doubt say No.  Used
   by the register allocator to do move coalescing. 
*/
Bool isMove_AMD64Instr ( AMD64Instr* i, HReg* src, HReg* dst )
{
   /* Moves between integer regs */
   if (i->tag == Ain_Alu64R) {
      if (i->Ain.Alu64R.op != Aalu_MOV)
         return False;
      if (i->Ain.Alu64R.src->tag != Armi_Reg)
         return False;
      *src = i->Ain.Alu64R.src->Armi.Reg.reg;
      *dst = i->Ain.Alu64R.dst;
      return True;
   }
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


/* Generate amd64 spill/reload instructions under the direction of the
   register allocator.  Note it's critical these don't write the
   condition codes. */

AMD64Instr* genSpill_AMD64 ( HReg rreg, Int offsetB )
{vassert(0);
//..    AMD64AMode* am;
//..    vassert(offsetB >= 0);
//..    vassert(!hregIsVirtual(rreg));
//..    am = AMD64AMode_IR(offsetB, hregAMD64_EBP());
//.. 
//..    switch (hregClass(rreg)) {
//..       case HRcInt32:
//..          return AMD64Instr_Alu32M ( Xalu_MOV, AMD64RI_Reg(rreg), am );
//..       case HRcFlt64:
//..          return AMD64Instr_FpLdSt ( False/*store*/, 8, rreg, am );
//..       case HRcVec128:
//..          return AMD64Instr_SseLdSt ( False/*store*/, rreg, am );
//..       default: 
//..          ppHRegClass(hregClass(rreg));
//..          vpanic("genSpill_AMD64: unimplemented regclass");
//..    }
}

AMD64Instr* genReload_AMD64 ( HReg rreg, Int offsetB )
{vassert(0);
//..    AMD64AMode* am;
//..    vassert(offsetB >= 0);
//..    vassert(!hregIsVirtual(rreg));
//..    am = AMD64AMode_IR(offsetB, hregAMD64_EBP());
//..    switch (hregClass(rreg)) {
//..       case HRcInt32:
//..          return AMD64Instr_Alu32R ( Xalu_MOV, AMD64RMI_Mem(am), rreg );
//..       case HRcFlt64:
//..          return AMD64Instr_FpLdSt ( True/*load*/, 8, rreg, am );
//..       case HRcVec128:
//..          return AMD64Instr_SseLdSt ( True/*load*/, rreg, am );
//..       default: 
//..          ppHRegClass(hregClass(rreg));
//..          vpanic("genReload_AMD64: unimplemented regclass");
//..    }
}


//.. /* --------- The x86 assembler (bleh.) --------- */
//.. 
//.. static UInt iregNo ( HReg r )
//.. {
//..    UInt n;
//..    vassert(hregClass(r) == HRcInt32);
//..    vassert(!hregIsVirtual(r));
//..    n = hregNumber(r);
//..    vassert(n <= 7);
//..    return n;
//.. }
//.. 
//.. static UInt fregNo ( HReg r )
//.. {
//..    UInt n;
//..    vassert(hregClass(r) == HRcFlt64);
//..    vassert(!hregIsVirtual(r));
//..    n = hregNumber(r);
//..    vassert(n <= 5);
//..    return n;
//.. }
//.. 
//.. static UInt vregNo ( HReg r )
//.. {
//..    UInt n;
//..    vassert(hregClass(r) == HRcVec128);
//..    vassert(!hregIsVirtual(r));
//..    n = hregNumber(r);
//..    vassert(n <= 7);
//..    return n;
//.. }
//.. 
//.. static UChar mkModRegRM ( UChar mod, UChar reg, UChar regmem )
//.. {
//..    return ((mod & 3) << 6) | ((reg & 7) << 3) | (regmem & 7);
//.. }
//.. 
//.. static UChar mkSIB ( Int shift, Int regindex, Int regbase )
//.. {
//..    return ((shift & 3) << 6) | ((regindex & 7) << 3) | (regbase & 7);
//.. }
//.. 
//.. static UChar* emit32 ( UChar* p, UInt w32 )
//.. {
//..    *p++ = (w32)       & 0x000000FF;
//..    *p++ = (w32 >>  8) & 0x000000FF;
//..    *p++ = (w32 >> 16) & 0x000000FF;
//..    *p++ = (w32 >> 24) & 0x000000FF;
//..    return p;
//.. }
//.. 
//.. /* Does a sign-extend of the lowest 8 bits give 
//..    the original number? */
//.. static Bool fits8bits ( UInt w32 )
//.. {
//..    Int i32 = (Int)w32;
//..    return i32 == ((i32 << 24) >> 24);
//.. }
//.. 
//.. 
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
//.. static UChar* doAMode_M ( UChar* p, HReg greg, AMD64AMode* am ) 
//.. {
//..    if (am->tag == Xam_IR) {
//..       if (am->Xam.IR.imm == 0 
//..           && am->Xam.IR.reg != hregAMD64_ESP()
//..           && am->Xam.IR.reg != hregAMD64_EBP() ) {
//..          *p++ = mkModRegRM(0, iregNo(greg), iregNo(am->Xam.IR.reg));
//..          return p;
//..       }
//..       if (fits8bits(am->Xam.IR.imm)
//..           && am->Xam.IR.reg != hregAMD64_ESP()) {
//..          *p++ = mkModRegRM(1, iregNo(greg), iregNo(am->Xam.IR.reg));
//..          *p++ = am->Xam.IR.imm & 0xFF;
//..          return p;
//..       }
//..       if (am->Xam.IR.reg != hregAMD64_ESP()) {
//..          *p++ = mkModRegRM(2, iregNo(greg), iregNo(am->Xam.IR.reg));
//..          p = emit32(p, am->Xam.IR.imm);
//..          return p;
//..       }
//..       if (am->Xam.IR.reg == hregAMD64_ESP()
//..           && fits8bits(am->Xam.IR.imm)) {
//..  	 *p++ = mkModRegRM(1, iregNo(greg), 4);
//..          *p++ = 0x24;
//..          *p++ = am->Xam.IR.imm & 0xFF;
//..          return p;
//..       }
//..       ppAMD64AMode(am);
//..       vpanic("doAMode_M: can't emit amode IR");
//..       /*NOTREACHED*/
//..    }
//..    if (am->tag == Xam_IRRS) {
//..       if (fits8bits(am->Xam.IRRS.imm)
//..           && am->Xam.IRRS.index != hregAMD64_ESP()) {
//..          *p++ = mkModRegRM(1, iregNo(greg), 4);
//..          *p++ = mkSIB(am->Xam.IRRS.shift, am->Xam.IRRS.index, 
//..                                           am->Xam.IRRS.base);
//..          *p++ = am->Xam.IRRS.imm & 0xFF;
//..          return p;
//..       }
//..       if (am->Xam.IRRS.index != hregAMD64_ESP()) {
//..          *p++ = mkModRegRM(2, iregNo(greg), 4);
//..          *p++ = mkSIB(am->Xam.IRRS.shift, am->Xam.IRRS.index,
//..                                           am->Xam.IRRS.base);
//..          p = emit32(p, am->Xam.IRRS.imm);
//..          return p;
//..       }
//..       ppAMD64AMode(am);
//..       vpanic("doAMode_M: can't emit amode IRRS");
//..       /*NOTREACHED*/
//..    }
//..    vpanic("doAMode_M: unknown amode");
//..    /*NOTREACHED*/
//.. }
//.. 
//.. 
//.. /* Emit a mod-reg-rm byte when the rm bit denotes a reg. */
//.. static UChar* doAMode_R ( UChar* p, HReg greg, HReg ereg ) 
//.. {
//..    *p++ = mkModRegRM(3, iregNo(greg), iregNo(ereg));
//..    return p;
//.. }
//.. 
//.. 
//.. /* Emit ffree %st(7) */
//.. static UChar* do_ffree_st7 ( UChar* p )
//.. {
//..    *p++ = 0xDD;
//..    *p++ = 0xC7;
//..    return p;
//.. }
//.. 
//.. /* Emit fstp %st(i), 1 <= i <= 7 */
//.. static UChar* do_fstp_st ( UChar* p, Int i )
//.. {
//..    vassert(1 <= i && i <= 7);
//..    *p++ = 0xDD;
//..    *p++ = 0xD8+i;
//..    return p;
//.. }
//.. 
//.. /* Emit fld %st(i), 0 <= i <= 6 */
//.. static UChar* do_fld_st ( UChar* p, Int i )
//.. {
//..    vassert(0 <= i && i <= 6);
//..    *p++ = 0xD9;
//..    *p++ = 0xC0+i;
//..    return p;
//.. }
//.. 
//.. /* Emit f<op> %st(0) */
//.. static UChar* do_fop1_st ( UChar* p, AMD64FpOp op )
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
//.. 
//.. /* Emit f<op> %st(i), 1 <= i <= 5 */
//.. static UChar* do_fop2_st ( UChar* p, AMD64FpOp op, Int i )
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
//.. 
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

Int emit_AMD64Instr ( UChar* buf, Int nbuf, AMD64Instr* i )
{
  //   UInt irno, opc, opc_rr, subopc_imm, opc_imma, opc_cl, opc_imm, subopc;
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
//..    /* vex_printf("asm  ");ppAMD64Instr(i); vex_printf("\n"); */

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
//..             if (i->Xin.Alu32R.dst == hregAMD64_EAX()
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
//..       /* See detailed comment for Xin_Call in getRegUsage_AMD64Instr above
//..          for explanation of this. */
//..       switch (i->Xin.Call.regparms) {
//..          case 0: irno = iregNo(hregAMD64_EAX()); break;
//..          case 1: irno = iregNo(hregAMD64_EDX()); break;
//..          case 2: irno = iregNo(hregAMD64_ECX()); break;
//..          case 3: irno = iregNo(hregAMD64_EDI()); break;
//..          default: vpanic(" emit_AMD64Instr:call:regparms");
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
//..             vpanic("emit_AMD64Instr.Xin_Goto: unknown jump kind");
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
//..          if (i->Xin.Goto.dst->Xri.Reg.reg != hregAMD64_EAX()) {
//..             *p++ = 0x89;
//..             p = doAMode_R(p, i->Xin.Goto.dst->Xri.Reg.reg, hregAMD64_EAX());
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
//..          *p++ = 0xB8 + iregNo(hregAMD64_EAX());
//..          p = emit32(p, 0);
//..          /* setb lo8(%eax) */
//..          *p++ = 0x0F; 
//..          *p++ = 0x90 + (UChar)(i->Xin.Set32.cond);
//..          p = doAMode_R(p, fake(0), hregAMD64_EAX());
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
//..          case VexSubArchAMD64_sse0:
//..             vassert(0); /* awaiting test case */
//..             /* lock addl $0,0(%esp) */
//..             *p++ = 0xF0; *p++ = 0x83; *p++ = 0x44; 
//..             *p++ = 0x24; *p++ = 0x00; *p++ = 0x00;
//..             goto done;
//..          case VexSubArchAMD64_sse1:
//..             /* sfence */
//..             *p++ = 0x0F; *p++ = 0xAE; *p++ = 0xF8;
//..             /* lock addl $0,0(%esp) */
//..             *p++ = 0xF0; *p++ = 0x83; *p++ = 0x44; 
//..             *p++ = 0x24; *p++ = 0x00; *p++ = 0x00;
//..             goto done;
//..          case VexSubArchAMD64_sse2:
//..             /* mfence */
//..             *p++ = 0x0F; *p++ = 0xAE; *p++ = 0xF0;
//..             goto done;
//..          default: 
//..             vpanic("emit_AMD64Instr:mfence:subarch");
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
//..             HReg  eax = hregAMD64_EAX(), ebx = hregAMD64_EBX(), 
//..                   ecx = hregAMD64_ECX(), edx = hregAMD64_EDX();
//..             Bool a_ok = True, b_ok = True, c_ok = True, d_ok = True;
//..             HRegUsage u;
//..             Int j;
//..             initHRegUsage(&u);
//..             addRegUsage_AMD64AMode(&u,  i->Xin.Store.dst);
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
//..             default: vpanic("emitAMD64Instr(FpBinary,PREM/PREM1/SCALE)");
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
//..             default: vpanic("emitAMD64Instr(Xin_FpLdStI-load)");
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
//..             default: vpanic("emitAMD64Instr(Xin_FpLdStI-store)");
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
//..       p = doAMode_R(p, hregAMD64_EAX(), i->Xin.FpCmp.dst);
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
//.. 
   default: 
      goto bad;
   }

  bad:
   ppAMD64Instr(i);
   vpanic("emit_AMD64Instr");
   /*NOTREACHED*/
   
   //  done:
   vassert(p - &buf[0] <= 32);
   return p - &buf[0];

#  undef fake
}

/*---------------------------------------------------------------*/
/*--- end                                  host-amd64/hdefs.c ---*/
/*---------------------------------------------------------------*/
