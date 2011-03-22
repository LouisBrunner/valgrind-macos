
/*---------------------------------------------------------------*/
/*--- begin                                 host_amd64_defs.c ---*/
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
#include "host_amd64_defs.h"


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

static void ppHRegAMD64_lo32 ( HReg reg ) 
{
   Int r;
   static HChar* ireg32_names[16] 
     = { "%eax",  "%ecx",  "%edx",  "%ebx",  "%esp",  "%ebp",  "%esi",  "%edi",
         "%r8d",  "%r9d",  "%r10d", "%r11d", "%r12d", "%r13d", "%r14d", "%r15d" };
   /* Be generic for all virtual regs. */
   if (hregIsVirtual(reg)) {
      ppHReg(reg);
      vex_printf("d");
      return;
   }
   /* But specific for real regs. */
   switch (hregClass(reg)) {
      case HRcInt64:
         r = hregNumber(reg);
         vassert(r >= 0 && r < 16);
         vex_printf("%s", ireg32_names[r]);
         return;
      default:
         vpanic("ppHRegAMD64_lo32: invalid regclass");
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
#if 0
   *nregs = 6;
   *arr = LibVEX_Alloc(*nregs * sizeof(HReg));
   (*arr)[ 0] = hregAMD64_RSI();
   (*arr)[ 1] = hregAMD64_RDI();
   (*arr)[ 2] = hregAMD64_RBX();

   (*arr)[ 3] = hregAMD64_XMM7();
   (*arr)[ 4] = hregAMD64_XMM8();
   (*arr)[ 5] = hregAMD64_XMM9();
#endif
#if 1
   *nregs = 20;
   *arr = LibVEX_Alloc(*nregs * sizeof(HReg));
   (*arr)[ 0] = hregAMD64_RSI();
   (*arr)[ 1] = hregAMD64_RDI();
   (*arr)[ 2] = hregAMD64_R8();
   (*arr)[ 3] = hregAMD64_R9();
   (*arr)[ 4] = hregAMD64_R12();
   (*arr)[ 5] = hregAMD64_R13();
   (*arr)[ 6] = hregAMD64_R14();
   (*arr)[ 7] = hregAMD64_R15();
   (*arr)[ 8] = hregAMD64_RBX();

   (*arr)[ 9] = hregAMD64_XMM3();
   (*arr)[10] = hregAMD64_XMM4();
   (*arr)[11] = hregAMD64_XMM5();
   (*arr)[12] = hregAMD64_XMM6();
   (*arr)[13] = hregAMD64_XMM7();
   (*arr)[14] = hregAMD64_XMM8();
   (*arr)[15] = hregAMD64_XMM9();
   (*arr)[16] = hregAMD64_XMM10();
   (*arr)[17] = hregAMD64_XMM11();
   (*arr)[18] = hregAMD64_XMM12();
   (*arr)[19] = hregAMD64_R10();
#endif
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
AMD64RM* AMD64RM_Mem ( AMD64AMode* am ) {
   AMD64RM* op    = LibVEX_Alloc(sizeof(AMD64RM));
   op->tag        = Arm_Mem;
   op->Arm.Mem.am = am;
   return op;
}

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


/* --------- Instructions. --------- */

static HChar* showAMD64ScalarSz ( Int sz ) {
   switch (sz) {
      case 2: return "w";
      case 4: return "l";
      case 8: return "q";
      default: vpanic("showAMD64ScalarSz");
   }
}
 
HChar* showAMD64UnaryOp ( AMD64UnaryOp op ) {
   switch (op) {
      case Aun_NOT: return "not";
      case Aun_NEG: return "neg";
      default: vpanic("showAMD64UnaryOp");
   }
}

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
      case Aalu_MUL:  return "imul";
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

HChar* showA87FpOp ( A87FpOp op ) {
   switch (op) {
//..       case Xfp_ADD:    return "add";
//..       case Xfp_SUB:    return "sub";
//..       case Xfp_MUL:    return "mul";
//..       case Xfp_DIV:    return "div";
      case Afp_SCALE:  return "scale";
      case Afp_ATAN:   return "atan";
      case Afp_YL2X:   return "yl2x";
      case Afp_YL2XP1: return "yl2xp1";
      case Afp_PREM:   return "prem";
      case Afp_PREM1:  return "prem1";
      case Afp_SQRT:   return "sqrt";
//..       case Xfp_ABS:    return "abs";
//..       case Xfp_NEG:    return "chs";
//..       case Xfp_MOV:    return "mov";
      case Afp_SIN:    return "sin";
      case Afp_COS:    return "cos";
      case Afp_TAN:    return "tan";
      case Afp_ROUND:  return "round";
      case Afp_2XM1:   return "2xm1";
      default: vpanic("showA87FpOp");
   }
}

HChar* showAMD64SseOp ( AMD64SseOp op ) {
   switch (op) {
      case Asse_MOV:      return "movups";
      case Asse_ADDF:     return "add";
      case Asse_SUBF:     return "sub";
      case Asse_MULF:     return "mul";
      case Asse_DIVF:     return "div";
      case Asse_MAXF:     return "max";
      case Asse_MINF:     return "min";
      case Asse_CMPEQF:   return "cmpFeq";
      case Asse_CMPLTF:   return "cmpFlt";
      case Asse_CMPLEF:   return "cmpFle";
      case Asse_CMPUNF:   return "cmpFun";
      case Asse_RCPF:     return "rcp";
      case Asse_RSQRTF:   return "rsqrt";
      case Asse_SQRTF:    return "sqrt";
      case Asse_AND:      return "and";
      case Asse_OR:       return "or";
      case Asse_XOR:      return "xor";
      case Asse_ANDN:     return "andn";
      case Asse_ADD8:     return "paddb";
      case Asse_ADD16:    return "paddw";
      case Asse_ADD32:    return "paddd";
      case Asse_ADD64:    return "paddq";
      case Asse_QADD8U:   return "paddusb";
      case Asse_QADD16U:  return "paddusw";
      case Asse_QADD8S:   return "paddsb";
      case Asse_QADD16S:  return "paddsw";
      case Asse_SUB8:     return "psubb";
      case Asse_SUB16:    return "psubw";
      case Asse_SUB32:    return "psubd";
      case Asse_SUB64:    return "psubq";
      case Asse_QSUB8U:   return "psubusb";
      case Asse_QSUB16U:  return "psubusw";
      case Asse_QSUB8S:   return "psubsb";
      case Asse_QSUB16S:  return "psubsw";
      case Asse_MUL16:    return "pmullw";
      case Asse_MULHI16U: return "pmulhuw";
      case Asse_MULHI16S: return "pmulhw";
      case Asse_AVG8U:    return "pavgb";
      case Asse_AVG16U:   return "pavgw";
      case Asse_MAX16S:   return "pmaxw";
      case Asse_MAX8U:    return "pmaxub";
      case Asse_MIN16S:   return "pminw";
      case Asse_MIN8U:    return "pminub";
      case Asse_CMPEQ8:   return "pcmpeqb";
      case Asse_CMPEQ16:  return "pcmpeqw";
      case Asse_CMPEQ32:  return "pcmpeqd";
      case Asse_CMPGT8S:  return "pcmpgtb";
      case Asse_CMPGT16S: return "pcmpgtw";
      case Asse_CMPGT32S: return "pcmpgtd";
      case Asse_SHL16:    return "psllw";
      case Asse_SHL32:    return "pslld";
      case Asse_SHL64:    return "psllq";
      case Asse_SHR16:    return "psrlw";
      case Asse_SHR32:    return "psrld";
      case Asse_SHR64:    return "psrlq";
      case Asse_SAR16:    return "psraw";
      case Asse_SAR32:    return "psrad";
      case Asse_PACKSSD:  return "packssdw";
      case Asse_PACKSSW:  return "packsswb";
      case Asse_PACKUSW:  return "packuswb";
      case Asse_UNPCKHB:  return "punpckhb";
      case Asse_UNPCKHW:  return "punpckhw";
      case Asse_UNPCKHD:  return "punpckhd";
      case Asse_UNPCKHQ:  return "punpckhq";
      case Asse_UNPCKLB:  return "punpcklb";
      case Asse_UNPCKLW:  return "punpcklw";
      case Asse_UNPCKLD:  return "punpckld";
      case Asse_UNPCKLQ:  return "punpcklq";
      default: vpanic("showAMD64SseOp");
   }
}

AMD64Instr* AMD64Instr_Imm64 ( ULong imm64, HReg dst ) {
   AMD64Instr* i      = LibVEX_Alloc(sizeof(AMD64Instr));
   i->tag             = Ain_Imm64;
   i->Ain.Imm64.imm64 = imm64;
   i->Ain.Imm64.dst   = dst;
   return i;
}
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
AMD64Instr* AMD64Instr_Sh64 ( AMD64ShiftOp op, UInt src, HReg dst ) {
   AMD64Instr* i   = LibVEX_Alloc(sizeof(AMD64Instr));
   i->tag          = Ain_Sh64;
   i->Ain.Sh64.op  = op;
   i->Ain.Sh64.src = src;
   i->Ain.Sh64.dst = dst;
   return i;
}
AMD64Instr* AMD64Instr_Test64 ( UInt imm32, HReg dst ) {
   AMD64Instr* i       = LibVEX_Alloc(sizeof(AMD64Instr));
   i->tag              = Ain_Test64;
   i->Ain.Test64.imm32 = imm32;
   i->Ain.Test64.dst   = dst;
   return i;
}
AMD64Instr* AMD64Instr_Unary64 ( AMD64UnaryOp op, HReg dst ) {
   AMD64Instr* i      = LibVEX_Alloc(sizeof(AMD64Instr));
   i->tag             = Ain_Unary64;
   i->Ain.Unary64.op  = op;
   i->Ain.Unary64.dst = dst;
   return i;
}
AMD64Instr* AMD64Instr_Lea64 ( AMD64AMode* am, HReg dst ) {
   AMD64Instr* i      = LibVEX_Alloc(sizeof(AMD64Instr));
   i->tag             = Ain_Lea64;
   i->Ain.Lea64.am    = am;
   i->Ain.Lea64.dst   = dst;
   return i;
}
AMD64Instr* AMD64Instr_MulL ( Bool syned, AMD64RM* src ) {
   AMD64Instr* i     = LibVEX_Alloc(sizeof(AMD64Instr));
   i->tag            = Ain_MulL;
   i->Ain.MulL.syned = syned;
   i->Ain.MulL.src   = src;
   return i;
}
AMD64Instr* AMD64Instr_Div ( Bool syned, Int sz, AMD64RM* src ) {
   AMD64Instr* i     = LibVEX_Alloc(sizeof(AMD64Instr));
   i->tag            = Ain_Div;
   i->Ain.Div.syned  = syned;
   i->Ain.Div.sz     = sz;
   i->Ain.Div.src    = src;
   vassert(sz == 4 || sz == 8);
   return i;
}
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
AMD64Instr* AMD64Instr_Push( AMD64RMI* src ) {
   AMD64Instr* i   = LibVEX_Alloc(sizeof(AMD64Instr));
   i->tag          = Ain_Push;
   i->Ain.Push.src = src;
   return i;
}
AMD64Instr* AMD64Instr_Call ( AMD64CondCode cond, Addr64 target, Int regparms ) {
   AMD64Instr* i        = LibVEX_Alloc(sizeof(AMD64Instr));
   i->tag               = Ain_Call;
   i->Ain.Call.cond     = cond;
   i->Ain.Call.target   = target;
   i->Ain.Call.regparms = regparms;
   vassert(regparms >= 0 && regparms <= 6);
   return i;
}
AMD64Instr* AMD64Instr_Goto ( IRJumpKind jk, AMD64CondCode cond, AMD64RI* dst ) {
   AMD64Instr* i    = LibVEX_Alloc(sizeof(AMD64Instr));
   i->tag           = Ain_Goto;
   i->Ain.Goto.cond = cond;
   i->Ain.Goto.dst  = dst;
   i->Ain.Goto.jk   = jk;
   return i;
}
AMD64Instr* AMD64Instr_CMov64 ( AMD64CondCode cond, AMD64RM* src, HReg dst ) {
   AMD64Instr* i      = LibVEX_Alloc(sizeof(AMD64Instr));
   i->tag             = Ain_CMov64;
   i->Ain.CMov64.cond = cond;
   i->Ain.CMov64.src  = src;
   i->Ain.CMov64.dst  = dst;
   vassert(cond != Acc_ALWAYS);
   return i;
}
AMD64Instr* AMD64Instr_MovxLQ ( Bool syned, HReg src, HReg dst ) {
   AMD64Instr* i       = LibVEX_Alloc(sizeof(AMD64Instr));
   i->tag              = Ain_MovxLQ;
   i->Ain.MovxLQ.syned = syned;
   i->Ain.MovxLQ.src   = src;
   i->Ain.MovxLQ.dst   = dst;
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
AMD64Instr* AMD64Instr_Set64 ( AMD64CondCode cond, HReg dst ) {
   AMD64Instr* i     = LibVEX_Alloc(sizeof(AMD64Instr));
   i->tag            = Ain_Set64;
   i->Ain.Set64.cond = cond;
   i->Ain.Set64.dst  = dst;
   return i;
}
AMD64Instr* AMD64Instr_Bsfr64 ( Bool isFwds, HReg src, HReg dst ) {
   AMD64Instr* i        = LibVEX_Alloc(sizeof(AMD64Instr));
   i->tag               = Ain_Bsfr64;
   i->Ain.Bsfr64.isFwds = isFwds;
   i->Ain.Bsfr64.src    = src;
   i->Ain.Bsfr64.dst    = dst;
   return i;
}
AMD64Instr* AMD64Instr_MFence ( void ) {
   AMD64Instr* i = LibVEX_Alloc(sizeof(AMD64Instr));
   i->tag        = Ain_MFence;
   return i;
}
AMD64Instr* AMD64Instr_ACAS ( AMD64AMode* addr, UChar sz ) {
   AMD64Instr* i    = LibVEX_Alloc(sizeof(AMD64Instr));
   i->tag           = Ain_ACAS;
   i->Ain.ACAS.addr = addr;
   i->Ain.ACAS.sz   = sz;
   vassert(sz == 8 || sz == 4 || sz == 2 || sz == 1);
   return i;
}
AMD64Instr* AMD64Instr_DACAS ( AMD64AMode* addr, UChar sz ) {
   AMD64Instr* i     = LibVEX_Alloc(sizeof(AMD64Instr));
   i->tag            = Ain_DACAS;
   i->Ain.DACAS.addr = addr;
   i->Ain.DACAS.sz   = sz;
   vassert(sz == 8 || sz == 4);
   return i;
}

AMD64Instr* AMD64Instr_A87Free ( Int nregs )
{
   AMD64Instr* i        = LibVEX_Alloc(sizeof(AMD64Instr));
   i->tag               = Ain_A87Free;
   i->Ain.A87Free.nregs = nregs;
   vassert(nregs >= 1 && nregs <= 7);
   return i;
}
AMD64Instr* AMD64Instr_A87PushPop ( AMD64AMode* addr, Bool isPush, UChar szB )
{
   AMD64Instr* i            = LibVEX_Alloc(sizeof(AMD64Instr));
   i->tag                   = Ain_A87PushPop;
   i->Ain.A87PushPop.addr   = addr;
   i->Ain.A87PushPop.isPush = isPush;
   i->Ain.A87PushPop.szB    = szB;
   vassert(szB == 8 || szB == 4);
   return i;
}
AMD64Instr* AMD64Instr_A87FpOp ( A87FpOp op )
{
   AMD64Instr* i     = LibVEX_Alloc(sizeof(AMD64Instr));
   i->tag            = Ain_A87FpOp;
   i->Ain.A87FpOp.op = op;
   return i;
}
AMD64Instr* AMD64Instr_A87LdCW ( AMD64AMode* addr )
{
   AMD64Instr* i       = LibVEX_Alloc(sizeof(AMD64Instr));
   i->tag              = Ain_A87LdCW;
   i->Ain.A87LdCW.addr = addr;
   return i;
}
AMD64Instr* AMD64Instr_A87StSW ( AMD64AMode* addr )
{
   AMD64Instr* i       = LibVEX_Alloc(sizeof(AMD64Instr));
   i->tag              = Ain_A87StSW;
   i->Ain.A87StSW.addr = addr;
   return i;
}

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
AMD64Instr* AMD64Instr_LdMXCSR ( AMD64AMode* addr ) {
   AMD64Instr* i         = LibVEX_Alloc(sizeof(AMD64Instr));
   i->tag                = Ain_LdMXCSR;
   i->Ain.LdMXCSR.addr   = addr;
   return i;
}
//.. AMD64Instr* AMD64Instr_FpStSW_AX ( void ) {
//..    AMD64Instr* i = LibVEX_Alloc(sizeof(AMD64Instr));
//..    i->tag      = Xin_FpStSW_AX;
//..    return i;
//.. }
AMD64Instr* AMD64Instr_SseUComIS ( Int sz, HReg srcL, HReg srcR, HReg dst ) {
   AMD64Instr* i         = LibVEX_Alloc(sizeof(AMD64Instr));
   i->tag                = Ain_SseUComIS;
   i->Ain.SseUComIS.sz   = toUChar(sz);
   i->Ain.SseUComIS.srcL = srcL;
   i->Ain.SseUComIS.srcR = srcR;
   i->Ain.SseUComIS.dst  = dst;
   vassert(sz == 4 || sz == 8);
   return i;
}
AMD64Instr* AMD64Instr_SseSI2SF ( Int szS, Int szD, HReg src, HReg dst ) {
   AMD64Instr* i       = LibVEX_Alloc(sizeof(AMD64Instr));
   i->tag              = Ain_SseSI2SF;
   i->Ain.SseSI2SF.szS = toUChar(szS);
   i->Ain.SseSI2SF.szD = toUChar(szD);
   i->Ain.SseSI2SF.src = src;
   i->Ain.SseSI2SF.dst = dst;
   vassert(szS == 4 || szS == 8);
   vassert(szD == 4 || szD == 8);
   return i;
}
AMD64Instr* AMD64Instr_SseSF2SI ( Int szS, Int szD, HReg src, HReg dst ) {
   AMD64Instr* i       = LibVEX_Alloc(sizeof(AMD64Instr));
   i->tag              = Ain_SseSF2SI;
   i->Ain.SseSF2SI.szS = toUChar(szS);
   i->Ain.SseSF2SI.szD = toUChar(szD);
   i->Ain.SseSF2SI.src = src;
   i->Ain.SseSF2SI.dst = dst;
   vassert(szS == 4 || szS == 8);
   vassert(szD == 4 || szD == 8);
   return i;
}
AMD64Instr* AMD64Instr_SseSDSS   ( Bool from64, HReg src, HReg dst )
{
   AMD64Instr* i         = LibVEX_Alloc(sizeof(AMD64Instr));
   i->tag                = Ain_SseSDSS;
   i->Ain.SseSDSS.from64 = from64;
   i->Ain.SseSDSS.src    = src;
   i->Ain.SseSDSS.dst    = dst;
   return i;
}

//.. AMD64Instr* AMD64Instr_SseConst ( UShort con, HReg dst ) {
//..    AMD64Instr* i            = LibVEX_Alloc(sizeof(AMD64Instr));
//..    i->tag                 = Xin_SseConst;
//..    i->Xin.SseConst.con    = con;
//..    i->Xin.SseConst.dst    = dst;
//..    vassert(hregClass(dst) == HRcVec128);
//..    return i;
//.. }
AMD64Instr* AMD64Instr_SseLdSt ( Bool isLoad, Int sz, 
                                 HReg reg, AMD64AMode* addr ) {
   AMD64Instr* i         = LibVEX_Alloc(sizeof(AMD64Instr));
   i->tag                = Ain_SseLdSt;
   i->Ain.SseLdSt.isLoad = isLoad;
   i->Ain.SseLdSt.sz     = toUChar(sz);
   i->Ain.SseLdSt.reg    = reg;
   i->Ain.SseLdSt.addr   = addr;
   vassert(sz == 4 || sz == 8 || sz == 16);
   return i;
}
AMD64Instr* AMD64Instr_SseLdzLO  ( Int sz, HReg reg, AMD64AMode* addr )
{
   AMD64Instr* i         = LibVEX_Alloc(sizeof(AMD64Instr));
   i->tag                = Ain_SseLdzLO;
   i->Ain.SseLdzLO.sz    = sz;
   i->Ain.SseLdzLO.reg   = reg;
   i->Ain.SseLdzLO.addr  = addr;
   vassert(sz == 4 || sz == 8);
   return i;
}
AMD64Instr* AMD64Instr_Sse32Fx4 ( AMD64SseOp op, HReg src, HReg dst ) {
   AMD64Instr* i       = LibVEX_Alloc(sizeof(AMD64Instr));
   i->tag              = Ain_Sse32Fx4;
   i->Ain.Sse32Fx4.op  = op;
   i->Ain.Sse32Fx4.src = src;
   i->Ain.Sse32Fx4.dst = dst;
   vassert(op != Asse_MOV);
   return i;
}
AMD64Instr* AMD64Instr_Sse32FLo ( AMD64SseOp op, HReg src, HReg dst ) {
   AMD64Instr* i       = LibVEX_Alloc(sizeof(AMD64Instr));
   i->tag              = Ain_Sse32FLo;
   i->Ain.Sse32FLo.op  = op;
   i->Ain.Sse32FLo.src = src;
   i->Ain.Sse32FLo.dst = dst;
   vassert(op != Asse_MOV);
   return i;
}
AMD64Instr* AMD64Instr_Sse64Fx2 ( AMD64SseOp op, HReg src, HReg dst ) {
   AMD64Instr* i       = LibVEX_Alloc(sizeof(AMD64Instr));
   i->tag              = Ain_Sse64Fx2;
   i->Ain.Sse64Fx2.op  = op;
   i->Ain.Sse64Fx2.src = src;
   i->Ain.Sse64Fx2.dst = dst;
   vassert(op != Asse_MOV);
   return i;
}
AMD64Instr* AMD64Instr_Sse64FLo ( AMD64SseOp op, HReg src, HReg dst ) {
   AMD64Instr* i       = LibVEX_Alloc(sizeof(AMD64Instr));
   i->tag              = Ain_Sse64FLo;
   i->Ain.Sse64FLo.op  = op;
   i->Ain.Sse64FLo.src = src;
   i->Ain.Sse64FLo.dst = dst;
   vassert(op != Asse_MOV);
   return i;
}
AMD64Instr* AMD64Instr_SseReRg ( AMD64SseOp op, HReg re, HReg rg ) {
   AMD64Instr* i      = LibVEX_Alloc(sizeof(AMD64Instr));
   i->tag             = Ain_SseReRg;
   i->Ain.SseReRg.op  = op;
   i->Ain.SseReRg.src = re;
   i->Ain.SseReRg.dst = rg;
   return i;
}
AMD64Instr* AMD64Instr_SseCMov ( AMD64CondCode cond, HReg src, HReg dst ) {
   AMD64Instr* i       = LibVEX_Alloc(sizeof(AMD64Instr));
   i->tag              = Ain_SseCMov;
   i->Ain.SseCMov.cond = cond;
   i->Ain.SseCMov.src  = src;
   i->Ain.SseCMov.dst  = dst;
   vassert(cond != Acc_ALWAYS);
   return i;
}
AMD64Instr* AMD64Instr_SseShuf ( Int order, HReg src, HReg dst ) {
   AMD64Instr* i        = LibVEX_Alloc(sizeof(AMD64Instr));
   i->tag               = Ain_SseShuf;
   i->Ain.SseShuf.order = order;
   i->Ain.SseShuf.src   = src;
   i->Ain.SseShuf.dst   = dst;
   vassert(order >= 0 && order <= 0xFF);
   return i;
}

void ppAMD64Instr ( AMD64Instr* i, Bool mode64 ) 
{
   vassert(mode64 == True);
   switch (i->tag) {
      case Ain_Imm64: 
         vex_printf("movabsq $0x%llx,", i->Ain.Imm64.imm64);
         ppHRegAMD64(i->Ain.Imm64.dst);
         return;
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
         vex_printf("%sq ", showAMD64ShiftOp(i->Ain.Sh64.op));
         if (i->Ain.Sh64.src == 0)
            vex_printf("%%cl,"); 
         else 
            vex_printf("$%d,", (Int)i->Ain.Sh64.src);
         ppHRegAMD64(i->Ain.Sh64.dst);
         return;
      case Ain_Test64:
         vex_printf("testq $%d,", (Int)i->Ain.Test64.imm32);
         ppHRegAMD64(i->Ain.Test64.dst);
         return;
      case Ain_Unary64:
         vex_printf("%sq ", showAMD64UnaryOp(i->Ain.Unary64.op));
         ppHRegAMD64(i->Ain.Unary64.dst);
         return;
      case Ain_Lea64:
         vex_printf("leaq ");
         ppAMD64AMode(i->Ain.Lea64.am);
         vex_printf(",");
         ppHRegAMD64(i->Ain.Lea64.dst);
         return;
      case Ain_MulL:
         vex_printf("%cmulq ", i->Ain.MulL.syned ? 's' : 'u');
         ppAMD64RM(i->Ain.MulL.src);
         return;
      case Ain_Div:
         vex_printf("%cdiv%s ",
                    i->Ain.Div.syned ? 's' : 'u',
                    showAMD64ScalarSz(i->Ain.Div.sz));
         ppAMD64RM(i->Ain.Div.src);
         return;
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
      case Ain_Push:
         vex_printf("pushq ");
         ppAMD64RMI(i->Ain.Push.src);
         return;
      case Ain_Call:
         vex_printf("call%s[%d] ", 
                    i->Ain.Call.cond==Acc_ALWAYS 
                       ? "" : showAMD64CondCode(i->Ain.Call.cond),
                    i->Ain.Call.regparms );
         vex_printf("0x%llx", i->Ain.Call.target);
         break;
      case Ain_Goto:
         if (i->Ain.Goto.cond != Acc_ALWAYS) {
            vex_printf("if (%%rflags.%s) { ", 
                       showAMD64CondCode(i->Ain.Goto.cond));
         }
         if (i->Ain.Goto.jk != Ijk_Boring
             && i->Ain.Goto.jk != Ijk_Call
             && i->Ain.Goto.jk != Ijk_Ret) {
            vex_printf("movl $");
            ppIRJumpKind(i->Ain.Goto.jk);
            vex_printf(",%%ebp ; ");
         }
         vex_printf("movq ");
         ppAMD64RI(i->Ain.Goto.dst);
         vex_printf(",%%rax ; movabsq $dispatcher_addr,%%rdx ; jmp *%%rdx");
         if (i->Ain.Goto.cond != Acc_ALWAYS) {
            vex_printf(" }");
         }
         return;
      case Ain_CMov64:
         vex_printf("cmov%s ", showAMD64CondCode(i->Ain.CMov64.cond));
         ppAMD64RM(i->Ain.CMov64.src);
         vex_printf(",");
         ppHRegAMD64(i->Ain.CMov64.dst);
         return;
      case Ain_MovxLQ:
         vex_printf("mov%clq ", i->Ain.MovxLQ.syned ? 's' : 'z');
         ppHRegAMD64_lo32(i->Ain.MovxLQ.src);
         vex_printf(",");
         ppHRegAMD64(i->Ain.MovxLQ.dst);
         return;
      case Ain_LoadEX:
         if (i->Ain.LoadEX.szSmall==4 && !i->Ain.LoadEX.syned) {
            vex_printf("movl ");
            ppAMD64AMode(i->Ain.LoadEX.src);
            vex_printf(",");
            ppHRegAMD64_lo32(i->Ain.LoadEX.dst);
         } else {
            vex_printf("mov%c%cq ",
                       i->Ain.LoadEX.syned ? 's' : 'z',
                       i->Ain.LoadEX.szSmall==1 
                          ? 'b' 
                          : (i->Ain.LoadEX.szSmall==2 ? 'w' : 'l'));
            ppAMD64AMode(i->Ain.LoadEX.src);
            vex_printf(",");
            ppHRegAMD64(i->Ain.LoadEX.dst);
         }
         return;
      case Ain_Store:
         vex_printf("mov%c ", i->Ain.Store.sz==1 ? 'b' 
                              : (i->Ain.Store.sz==2 ? 'w' : 'l'));
         ppHRegAMD64(i->Ain.Store.src);
         vex_printf(",");
         ppAMD64AMode(i->Ain.Store.dst);
         return;
      case Ain_Set64:
         vex_printf("setq%s ", showAMD64CondCode(i->Ain.Set64.cond));
         ppHRegAMD64(i->Ain.Set64.dst);
         return;
      case Ain_Bsfr64:
         vex_printf("bs%cq ", i->Ain.Bsfr64.isFwds ? 'f' : 'r');
         ppHRegAMD64(i->Ain.Bsfr64.src);
         vex_printf(",");
         ppHRegAMD64(i->Ain.Bsfr64.dst);
         return;
      case Ain_MFence:
         vex_printf("mfence" );
         return;
      case Ain_ACAS:
         vex_printf("lock cmpxchg%c ",
                     i->Ain.ACAS.sz==1 ? 'b' : i->Ain.ACAS.sz==2 ? 'w' 
                     : i->Ain.ACAS.sz==4 ? 'l' : 'q' );
         vex_printf("{%%rax->%%rbx},");
         ppAMD64AMode(i->Ain.ACAS.addr);
         return;
      case Ain_DACAS:
         vex_printf("lock cmpxchg%db {%%rdx:%%rax->%%rcx:%%rbx},",
                    (Int)(2 * i->Ain.DACAS.sz));
         ppAMD64AMode(i->Ain.DACAS.addr);
         return;
      case Ain_A87Free:
         vex_printf("ffree %%st(7..%d)", 8 - i->Ain.A87Free.nregs );
         break;
      case Ain_A87PushPop:
         vex_printf(i->Ain.A87PushPop.isPush ? "fld%c " : "fstp%c ",
                    i->Ain.A87PushPop.szB == 4 ? 's' : 'l');
         ppAMD64AMode(i->Ain.A87PushPop.addr);
         break;
      case Ain_A87FpOp:
         vex_printf("f%s", showA87FpOp(i->Ain.A87FpOp.op));
         break;
      case Ain_A87LdCW:
         vex_printf("fldcw ");
         ppAMD64AMode(i->Ain.A87LdCW.addr);
         break;
      case Ain_A87StSW:
         vex_printf("fstsw ");
         ppAMD64AMode(i->Ain.A87StSW.addr);
         break;
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
      case Ain_LdMXCSR:
         vex_printf("ldmxcsr ");
         ppAMD64AMode(i->Ain.LdMXCSR.addr);
         break;
      case Ain_SseUComIS:
         vex_printf("ucomis%s ", i->Ain.SseUComIS.sz==4 ? "s" : "d");
         ppHRegAMD64(i->Ain.SseUComIS.srcL);
         vex_printf(",");
         ppHRegAMD64(i->Ain.SseUComIS.srcR);
         vex_printf(" ; pushfq ; popq ");
         ppHRegAMD64(i->Ain.SseUComIS.dst);
         break;
      case Ain_SseSI2SF:
         vex_printf("cvtsi2s%s ", i->Ain.SseSI2SF.szD==4 ? "s" : "d");
         (i->Ain.SseSI2SF.szS==4 ? ppHRegAMD64_lo32 : ppHRegAMD64)
            (i->Ain.SseSI2SF.src);
         vex_printf(",");
         ppHRegAMD64(i->Ain.SseSI2SF.dst);
         break;
      case Ain_SseSF2SI:
         vex_printf("cvts%s2si ", i->Ain.SseSF2SI.szS==4 ? "s" : "d");
         ppHRegAMD64(i->Ain.SseSF2SI.src);
         vex_printf(",");
         (i->Ain.SseSF2SI.szD==4 ? ppHRegAMD64_lo32 : ppHRegAMD64)
            (i->Ain.SseSF2SI.dst);
         break;
      case Ain_SseSDSS:
         vex_printf(i->Ain.SseSDSS.from64 ? "cvtsd2ss " : "cvtss2sd ");
         ppHRegAMD64(i->Ain.SseSDSS.src);
         vex_printf(",");
         ppHRegAMD64(i->Ain.SseSDSS.dst);
         break;
//..       case Xin_SseConst:
//..          vex_printf("const $0x%04x,", (Int)i->Xin.SseConst.con);
//..          ppHRegAMD64(i->Xin.SseConst.dst);
//..          break;
      case Ain_SseLdSt:
         switch (i->Ain.SseLdSt.sz) {
            case 4:  vex_printf("movss "); break;
            case 8:  vex_printf("movsd "); break;
            case 16: vex_printf("movups "); break;
            default: vassert(0);
         }
         if (i->Ain.SseLdSt.isLoad) {
            ppAMD64AMode(i->Ain.SseLdSt.addr);
            vex_printf(",");
            ppHRegAMD64(i->Ain.SseLdSt.reg);
         } else {
            ppHRegAMD64(i->Ain.SseLdSt.reg);
            vex_printf(",");
            ppAMD64AMode(i->Ain.SseLdSt.addr);
         }
         return;
      case Ain_SseLdzLO:
         vex_printf("movs%s ", i->Ain.SseLdzLO.sz==4 ? "s" : "d");
         ppAMD64AMode(i->Ain.SseLdzLO.addr);
         vex_printf(",");
         ppHRegAMD64(i->Ain.SseLdzLO.reg);
         return;
      case Ain_Sse32Fx4:
         vex_printf("%sps ", showAMD64SseOp(i->Ain.Sse32Fx4.op));
         ppHRegAMD64(i->Ain.Sse32Fx4.src);
         vex_printf(",");
         ppHRegAMD64(i->Ain.Sse32Fx4.dst);
         return;
      case Ain_Sse32FLo:
         vex_printf("%sss ", showAMD64SseOp(i->Ain.Sse32FLo.op));
         ppHRegAMD64(i->Ain.Sse32FLo.src);
         vex_printf(",");
         ppHRegAMD64(i->Ain.Sse32FLo.dst);
         return;
      case Ain_Sse64Fx2:
         vex_printf("%spd ", showAMD64SseOp(i->Ain.Sse64Fx2.op));
         ppHRegAMD64(i->Ain.Sse64Fx2.src);
         vex_printf(",");
         ppHRegAMD64(i->Ain.Sse64Fx2.dst);
         return;
      case Ain_Sse64FLo:
         vex_printf("%ssd ", showAMD64SseOp(i->Ain.Sse64FLo.op));
         ppHRegAMD64(i->Ain.Sse64FLo.src);
         vex_printf(",");
         ppHRegAMD64(i->Ain.Sse64FLo.dst);
         return;
      case Ain_SseReRg:
         vex_printf("%s ", showAMD64SseOp(i->Ain.SseReRg.op));
         ppHRegAMD64(i->Ain.SseReRg.src);
         vex_printf(",");
         ppHRegAMD64(i->Ain.SseReRg.dst);
         return;
      case Ain_SseCMov:
         vex_printf("cmov%s ", showAMD64CondCode(i->Ain.SseCMov.cond));
         ppHRegAMD64(i->Ain.SseCMov.src);
         vex_printf(",");
         ppHRegAMD64(i->Ain.SseCMov.dst);
         return;
      case Ain_SseShuf:
         vex_printf("pshufd $0x%x,", i->Ain.SseShuf.order);
         ppHRegAMD64(i->Ain.SseShuf.src);
         vex_printf(",");
         ppHRegAMD64(i->Ain.SseShuf.dst);
         return;

      default:
         vpanic("ppAMD64Instr");
   }
}

/* --------- Helpers for register allocation. --------- */

void getRegUsage_AMD64Instr ( HRegUsage* u, AMD64Instr* i, Bool mode64 )
{
   Bool unary;
   vassert(mode64 == True);
   initHRegUsage(u);
   switch (i->tag) {
      case Ain_Imm64:
         addHRegUse(u, HRmWrite, i->Ain.Imm64.dst);
         return;
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
         addHRegUse(u, HRmModify, i->Ain.Sh64.dst);
         if (i->Ain.Sh64.src == 0)
            addHRegUse(u, HRmRead, hregAMD64_RCX());
         return;
      case Ain_Test64:
         addHRegUse(u, HRmRead, i->Ain.Test64.dst);
         return;
      case Ain_Unary64:
         addHRegUse(u, HRmModify, i->Ain.Unary64.dst);
         return;
      case Ain_Lea64:
         addRegUsage_AMD64AMode(u, i->Ain.Lea64.am);
         addHRegUse(u, HRmWrite, i->Ain.Lea64.dst);
         return;
      case Ain_MulL:
         addRegUsage_AMD64RM(u, i->Ain.MulL.src, HRmRead);
         addHRegUse(u, HRmModify, hregAMD64_RAX());
         addHRegUse(u, HRmWrite, hregAMD64_RDX());
         return;
      case Ain_Div:
         addRegUsage_AMD64RM(u, i->Ain.Div.src, HRmRead);
         addHRegUse(u, HRmModify, hregAMD64_RAX());
         addHRegUse(u, HRmModify, hregAMD64_RDX());
         return;
//..       case Xin_Sh3232:
//..          addHRegUse(u, HRmRead, i->Xin.Sh3232.src);
//..          addHRegUse(u, HRmModify, i->Xin.Sh3232.dst);
//..          if (i->Xin.Sh3232.amt == 0)
//..             addHRegUse(u, HRmRead, hregAMD64_ECX());
//..          return;
      case Ain_Push:
         addRegUsage_AMD64RMI(u, i->Ain.Push.src);
         addHRegUse(u, HRmModify, hregAMD64_RSP());
         return;
      case Ain_Call:
         /* This is a bit subtle. */
         /* First off, claim it trashes all the caller-saved regs
            which fall within the register allocator's jurisdiction.
            These I believe to be: rax rcx rdx rsi rdi r8 r9 r10 r11 
            and all the xmm registers.
         */
         addHRegUse(u, HRmWrite, hregAMD64_RAX());
         addHRegUse(u, HRmWrite, hregAMD64_RCX());
         addHRegUse(u, HRmWrite, hregAMD64_RDX());
         addHRegUse(u, HRmWrite, hregAMD64_RSI());
         addHRegUse(u, HRmWrite, hregAMD64_RDI());
         addHRegUse(u, HRmWrite, hregAMD64_R8());
         addHRegUse(u, HRmWrite, hregAMD64_R9());
         addHRegUse(u, HRmWrite, hregAMD64_R10());
         addHRegUse(u, HRmWrite, hregAMD64_R11());
         addHRegUse(u, HRmWrite, hregAMD64_XMM0());
         addHRegUse(u, HRmWrite, hregAMD64_XMM1());
         addHRegUse(u, HRmWrite, hregAMD64_XMM2());
         addHRegUse(u, HRmWrite, hregAMD64_XMM3());
         addHRegUse(u, HRmWrite, hregAMD64_XMM4());
         addHRegUse(u, HRmWrite, hregAMD64_XMM5());
         addHRegUse(u, HRmWrite, hregAMD64_XMM6());
         addHRegUse(u, HRmWrite, hregAMD64_XMM7());
         addHRegUse(u, HRmWrite, hregAMD64_XMM8());
         addHRegUse(u, HRmWrite, hregAMD64_XMM9());
         addHRegUse(u, HRmWrite, hregAMD64_XMM10());
         addHRegUse(u, HRmWrite, hregAMD64_XMM11());
         addHRegUse(u, HRmWrite, hregAMD64_XMM12());
         addHRegUse(u, HRmWrite, hregAMD64_XMM13());
         addHRegUse(u, HRmWrite, hregAMD64_XMM14());
         addHRegUse(u, HRmWrite, hregAMD64_XMM15());

         /* Now we have to state any parameter-carrying registers
            which might be read.  This depends on the regparmness. */
         switch (i->Ain.Call.regparms) {
            case 6: addHRegUse(u, HRmRead, hregAMD64_R9());  /*fallthru*/
            case 5: addHRegUse(u, HRmRead, hregAMD64_R8());  /*fallthru*/
            case 4: addHRegUse(u, HRmRead, hregAMD64_RCX()); /*fallthru*/
            case 3: addHRegUse(u, HRmRead, hregAMD64_RDX()); /*fallthru*/
            case 2: addHRegUse(u, HRmRead, hregAMD64_RSI()); /*fallthru*/
            case 1: addHRegUse(u, HRmRead, hregAMD64_RDI()); break;
            case 0: break;
            default: vpanic("getRegUsage_AMD64Instr:Call:regparms");
         }
         /* Finally, there is the issue that the insn trashes a
            register because the literal target address has to be
            loaded into a register.  Fortunately, r11 is stated in the
            ABI as a scratch register, and so seems a suitable victim.  */
         addHRegUse(u, HRmWrite, hregAMD64_R11());
         /* Upshot of this is that the assembler really must use r11,
            and no other, as a destination temporary. */
         return;
      case Ain_Goto:
         addRegUsage_AMD64RI(u, i->Ain.Goto.dst);
         addHRegUse(u, HRmWrite, hregAMD64_RAX()); /* used for next guest addr */
         addHRegUse(u, HRmWrite, hregAMD64_RDX()); /* used for dispatcher addr */
         if (i->Ain.Goto.jk != Ijk_Boring
             && i->Ain.Goto.jk != Ijk_Call
             && i->Ain.Goto.jk != Ijk_Ret)
            /* note, this is irrelevant since rbp is not actually
               available to the allocator.  But still .. */
            addHRegUse(u, HRmWrite, hregAMD64_RBP());
         return;
      case Ain_CMov64:
         addRegUsage_AMD64RM(u, i->Ain.CMov64.src, HRmRead);
         addHRegUse(u, HRmModify, i->Ain.CMov64.dst);
         return;
      case Ain_MovxLQ:
         addHRegUse(u, HRmRead,  i->Ain.MovxLQ.src);
         addHRegUse(u, HRmWrite, i->Ain.MovxLQ.dst);
         return;
      case Ain_LoadEX:
         addRegUsage_AMD64AMode(u, i->Ain.LoadEX.src);
         addHRegUse(u, HRmWrite, i->Ain.LoadEX.dst);
         return;
      case Ain_Store:
         addHRegUse(u, HRmRead, i->Ain.Store.src);
         addRegUsage_AMD64AMode(u, i->Ain.Store.dst);
         return;
      case Ain_Set64:
         addHRegUse(u, HRmWrite, i->Ain.Set64.dst);
         return;
      case Ain_Bsfr64:
         addHRegUse(u, HRmRead, i->Ain.Bsfr64.src);
         addHRegUse(u, HRmWrite, i->Ain.Bsfr64.dst);
         return;
      case Ain_MFence:
         return;
      case Ain_ACAS:
         addRegUsage_AMD64AMode(u, i->Ain.ACAS.addr);
         addHRegUse(u, HRmRead, hregAMD64_RBX());
         addHRegUse(u, HRmModify, hregAMD64_RAX());
         return;
      case Ain_DACAS:
         addRegUsage_AMD64AMode(u, i->Ain.DACAS.addr);
         addHRegUse(u, HRmRead, hregAMD64_RCX());
         addHRegUse(u, HRmRead, hregAMD64_RBX());
         addHRegUse(u, HRmModify, hregAMD64_RDX());
         addHRegUse(u, HRmModify, hregAMD64_RAX());
         return;
      case Ain_A87Free:
         return;
      case Ain_A87PushPop:
         addRegUsage_AMD64AMode(u, i->Ain.A87PushPop.addr);
         return;
      case Ain_A87FpOp:
         return;
      case Ain_A87LdCW:
         addRegUsage_AMD64AMode(u, i->Ain.A87LdCW.addr);
         return;
      case Ain_A87StSW:
         addRegUsage_AMD64AMode(u, i->Ain.A87StSW.addr);
         return;
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
      case Ain_LdMXCSR:
         addRegUsage_AMD64AMode(u, i->Ain.LdMXCSR.addr);
         return;
//..       case Xin_FpStSW_AX:
//..          addHRegUse(u, HRmWrite, hregAMD64_EAX());
//..          return;
      case Ain_SseUComIS:
         addHRegUse(u, HRmRead,  i->Ain.SseUComIS.srcL);
         addHRegUse(u, HRmRead,  i->Ain.SseUComIS.srcR);
         addHRegUse(u, HRmWrite, i->Ain.SseUComIS.dst);
         return;
      case Ain_SseSI2SF:
         addHRegUse(u, HRmRead,  i->Ain.SseSI2SF.src);
         addHRegUse(u, HRmWrite, i->Ain.SseSI2SF.dst);
         return;
      case Ain_SseSF2SI:
         addHRegUse(u, HRmRead,  i->Ain.SseSF2SI.src);
         addHRegUse(u, HRmWrite, i->Ain.SseSF2SI.dst);
         return;
      case Ain_SseSDSS:
         addHRegUse(u, HRmRead,  i->Ain.SseSDSS.src);
         addHRegUse(u, HRmWrite, i->Ain.SseSDSS.dst);
         return;
      case Ain_SseLdSt:
         addRegUsage_AMD64AMode(u, i->Ain.SseLdSt.addr);
         addHRegUse(u, i->Ain.SseLdSt.isLoad ? HRmWrite : HRmRead,
                       i->Ain.SseLdSt.reg);
         return;
      case Ain_SseLdzLO:
         addRegUsage_AMD64AMode(u, i->Ain.SseLdzLO.addr);
         addHRegUse(u, HRmWrite, i->Ain.SseLdzLO.reg);
         return;
//..       case Xin_SseConst:
//..          addHRegUse(u, HRmWrite, i->Xin.SseConst.dst);
//..          return;
      case Ain_Sse32Fx4:
         vassert(i->Ain.Sse32Fx4.op != Asse_MOV);
         unary = toBool( i->Ain.Sse32Fx4.op == Asse_RCPF
                         || i->Ain.Sse32Fx4.op == Asse_RSQRTF
                         || i->Ain.Sse32Fx4.op == Asse_SQRTF );
         addHRegUse(u, HRmRead, i->Ain.Sse32Fx4.src);
         addHRegUse(u, unary ? HRmWrite : HRmModify, 
                       i->Ain.Sse32Fx4.dst);
         return;
      case Ain_Sse32FLo:
         vassert(i->Ain.Sse32FLo.op != Asse_MOV);
         unary = toBool( i->Ain.Sse32FLo.op == Asse_RCPF
                         || i->Ain.Sse32FLo.op == Asse_RSQRTF
                         || i->Ain.Sse32FLo.op == Asse_SQRTF );
         addHRegUse(u, HRmRead, i->Ain.Sse32FLo.src);
         addHRegUse(u, unary ? HRmWrite : HRmModify, 
                       i->Ain.Sse32FLo.dst);
         return;
      case Ain_Sse64Fx2:
         vassert(i->Ain.Sse64Fx2.op != Asse_MOV);
         unary = toBool( i->Ain.Sse64Fx2.op == Asse_RCPF
                         || i->Ain.Sse64Fx2.op == Asse_RSQRTF
                         || i->Ain.Sse64Fx2.op == Asse_SQRTF );
         addHRegUse(u, HRmRead, i->Ain.Sse64Fx2.src);
         addHRegUse(u, unary ? HRmWrite : HRmModify, 
                       i->Ain.Sse64Fx2.dst);
         return;
      case Ain_Sse64FLo:
         vassert(i->Ain.Sse64FLo.op != Asse_MOV);
         unary = toBool( i->Ain.Sse64FLo.op == Asse_RCPF
                         || i->Ain.Sse64FLo.op == Asse_RSQRTF
                         || i->Ain.Sse64FLo.op == Asse_SQRTF );
         addHRegUse(u, HRmRead, i->Ain.Sse64FLo.src);
         addHRegUse(u, unary ? HRmWrite : HRmModify, 
                       i->Ain.Sse64FLo.dst);
         return;
      case Ain_SseReRg:
         if ( (i->Ain.SseReRg.op == Asse_XOR
               || i->Ain.SseReRg.op == Asse_CMPEQ32)
              && i->Ain.SseReRg.src == i->Ain.SseReRg.dst) {
            /* reg-alloc needs to understand 'xor r,r' and 'cmpeqd
               r,r' as a write of a value to r, and independent of any
               previous value in r */
            /* (as opposed to a rite of passage :-) */
            addHRegUse(u, HRmWrite, i->Ain.SseReRg.dst);
         } else {
            addHRegUse(u, HRmRead, i->Ain.SseReRg.src);
            addHRegUse(u, i->Ain.SseReRg.op == Asse_MOV 
                             ? HRmWrite : HRmModify, 
                          i->Ain.SseReRg.dst);
         }
         return;
      case Ain_SseCMov:
         addHRegUse(u, HRmRead,   i->Ain.SseCMov.src);
         addHRegUse(u, HRmModify, i->Ain.SseCMov.dst);
         return;
      case Ain_SseShuf:
         addHRegUse(u, HRmRead,  i->Ain.SseShuf.src);
         addHRegUse(u, HRmWrite, i->Ain.SseShuf.dst);
         return;
      default:
         ppAMD64Instr(i, mode64);
         vpanic("getRegUsage_AMD64Instr");
   }
}

/* local helper */
static inline void mapReg(HRegRemap* m, HReg* r)
{
   *r = lookupHRegRemap(m, *r);
}

void mapRegs_AMD64Instr ( HRegRemap* m, AMD64Instr* i, Bool mode64 )
{
   vassert(mode64 == True);
   switch (i->tag) {
      case Ain_Imm64:
         mapReg(m, &i->Ain.Imm64.dst);
         return;
      case Ain_Alu64R:
         mapRegs_AMD64RMI(m, i->Ain.Alu64R.src);
         mapReg(m, &i->Ain.Alu64R.dst);
         return;
      case Ain_Alu64M:
         mapRegs_AMD64RI(m, i->Ain.Alu64M.src);
         mapRegs_AMD64AMode(m, i->Ain.Alu64M.dst);
         return;
      case Ain_Sh64:
         mapReg(m, &i->Ain.Sh64.dst);
         return;
      case Ain_Test64:
         mapReg(m, &i->Ain.Test64.dst);
         return;
      case Ain_Unary64:
         mapReg(m, &i->Ain.Unary64.dst);
         return;
      case Ain_Lea64:
         mapRegs_AMD64AMode(m, i->Ain.Lea64.am);
         mapReg(m, &i->Ain.Lea64.dst);
         return;
      case Ain_MulL:
         mapRegs_AMD64RM(m, i->Ain.MulL.src);
         return;
      case Ain_Div:
         mapRegs_AMD64RM(m, i->Ain.Div.src);
         return;
//..       case Xin_Sh3232:
//..          mapReg(m, &i->Xin.Sh3232.src);
//..          mapReg(m, &i->Xin.Sh3232.dst);
//..          return;
      case Ain_Push:
         mapRegs_AMD64RMI(m, i->Ain.Push.src);
         return;
      case Ain_Call:
         return;
      case Ain_Goto:
         mapRegs_AMD64RI(m, i->Ain.Goto.dst);
         return;
      case Ain_CMov64:
         mapRegs_AMD64RM(m, i->Ain.CMov64.src);
         mapReg(m, &i->Ain.CMov64.dst);
         return;
      case Ain_MovxLQ:
         mapReg(m, &i->Ain.MovxLQ.src);
         mapReg(m, &i->Ain.MovxLQ.dst);
         return;
      case Ain_LoadEX:
         mapRegs_AMD64AMode(m, i->Ain.LoadEX.src);
         mapReg(m, &i->Ain.LoadEX.dst);
         return;
      case Ain_Store:
         mapReg(m, &i->Ain.Store.src);
         mapRegs_AMD64AMode(m, i->Ain.Store.dst);
         return;
      case Ain_Set64:
         mapReg(m, &i->Ain.Set64.dst);
         return;
      case Ain_Bsfr64:
         mapReg(m, &i->Ain.Bsfr64.src);
         mapReg(m, &i->Ain.Bsfr64.dst);
         return;
      case Ain_MFence:
         return;
      case Ain_ACAS:
         mapRegs_AMD64AMode(m, i->Ain.ACAS.addr);
         return;
      case Ain_DACAS:
         mapRegs_AMD64AMode(m, i->Ain.DACAS.addr);
         return;
      case Ain_A87Free:
         return;
      case Ain_A87PushPop:
         mapRegs_AMD64AMode(m, i->Ain.A87PushPop.addr);
         return;
      case Ain_A87FpOp:
         return;
      case Ain_A87LdCW:
         mapRegs_AMD64AMode(m, i->Ain.A87LdCW.addr);
         return;
      case Ain_A87StSW:
         mapRegs_AMD64AMode(m, i->Ain.A87StSW.addr);
         return;
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
      case Ain_LdMXCSR:
         mapRegs_AMD64AMode(m, i->Ain.LdMXCSR.addr);
         return;
//..       case Xin_FpStSW_AX:
//..          return;
      case Ain_SseUComIS:
         mapReg(m, &i->Ain.SseUComIS.srcL);
         mapReg(m, &i->Ain.SseUComIS.srcR);
         mapReg(m, &i->Ain.SseUComIS.dst);
         return;
      case Ain_SseSI2SF:
         mapReg(m, &i->Ain.SseSI2SF.src);
         mapReg(m, &i->Ain.SseSI2SF.dst);
         return;
      case Ain_SseSF2SI:
         mapReg(m, &i->Ain.SseSF2SI.src);
         mapReg(m, &i->Ain.SseSF2SI.dst);
         return;
      case Ain_SseSDSS:
         mapReg(m, &i->Ain.SseSDSS.src);
         mapReg(m, &i->Ain.SseSDSS.dst);
         return;
//..       case Xin_SseConst:
//..          mapReg(m, &i->Xin.SseConst.dst);
//..          return;
      case Ain_SseLdSt:
         mapReg(m, &i->Ain.SseLdSt.reg);
         mapRegs_AMD64AMode(m, i->Ain.SseLdSt.addr);
         break;
      case Ain_SseLdzLO:
         mapReg(m, &i->Ain.SseLdzLO.reg);
         mapRegs_AMD64AMode(m, i->Ain.SseLdzLO.addr);
         break;
      case Ain_Sse32Fx4:
         mapReg(m, &i->Ain.Sse32Fx4.src);
         mapReg(m, &i->Ain.Sse32Fx4.dst);
         return;
      case Ain_Sse32FLo:
         mapReg(m, &i->Ain.Sse32FLo.src);
         mapReg(m, &i->Ain.Sse32FLo.dst);
         return;
      case Ain_Sse64Fx2:
         mapReg(m, &i->Ain.Sse64Fx2.src);
         mapReg(m, &i->Ain.Sse64Fx2.dst);
         return;
      case Ain_Sse64FLo:
         mapReg(m, &i->Ain.Sse64FLo.src);
         mapReg(m, &i->Ain.Sse64FLo.dst);
         return;
      case Ain_SseReRg:
         mapReg(m, &i->Ain.SseReRg.src);
         mapReg(m, &i->Ain.SseReRg.dst);
         return;
      case Ain_SseCMov:
         mapReg(m, &i->Ain.SseCMov.src);
         mapReg(m, &i->Ain.SseCMov.dst);
         return;
      case Ain_SseShuf:
         mapReg(m, &i->Ain.SseShuf.src);
         mapReg(m, &i->Ain.SseShuf.dst);
         return;
      default:
         ppAMD64Instr(i, mode64);
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
   /* Moves between vector regs */
   if (i->tag == Ain_SseReRg) {
      if (i->Ain.SseReRg.op != Asse_MOV)
         return False;
      *src = i->Ain.SseReRg.src;
      *dst = i->Ain.SseReRg.dst;
      return True;
   }
   return False;
}


/* Generate amd64 spill/reload instructions under the direction of the
   register allocator.  Note it's critical these don't write the
   condition codes. */

void genSpill_AMD64 ( /*OUT*/HInstr** i1, /*OUT*/HInstr** i2,
                      HReg rreg, Int offsetB, Bool mode64 )
{
   AMD64AMode* am;
   vassert(offsetB >= 0);
   vassert(!hregIsVirtual(rreg));
   vassert(mode64 == True);
   *i1 = *i2 = NULL;
   am = AMD64AMode_IR(offsetB, hregAMD64_RBP());
   switch (hregClass(rreg)) {
      case HRcInt64:
         *i1 = AMD64Instr_Alu64M ( Aalu_MOV, AMD64RI_Reg(rreg), am );
         return;
      case HRcVec128:
         *i1 = AMD64Instr_SseLdSt ( False/*store*/, 16, rreg, am );
         return;
      default: 
         ppHRegClass(hregClass(rreg));
         vpanic("genSpill_AMD64: unimplemented regclass");
   }
}

void genReload_AMD64 ( /*OUT*/HInstr** i1, /*OUT*/HInstr** i2,
                       HReg rreg, Int offsetB, Bool mode64 )
{
   AMD64AMode* am;
   vassert(offsetB >= 0);
   vassert(!hregIsVirtual(rreg));
   vassert(mode64 == True);
   *i1 = *i2 = NULL;
   am = AMD64AMode_IR(offsetB, hregAMD64_RBP());
   switch (hregClass(rreg)) {
      case HRcInt64:
         *i1 = AMD64Instr_Alu64R ( Aalu_MOV, AMD64RMI_Mem(am), rreg );
         return;
      case HRcVec128:
         *i1 = AMD64Instr_SseLdSt ( True/*load*/, 16, rreg, am );
         return;
      default: 
         ppHRegClass(hregClass(rreg));
         vpanic("genReload_AMD64: unimplemented regclass");
   }
}


/* --------- The amd64 assembler (bleh.) --------- */

/* Produce the low three bits of an integer register number. */
static UChar iregBits210 ( HReg r )
{
   UInt n;
   vassert(hregClass(r) == HRcInt64);
   vassert(!hregIsVirtual(r));
   n = hregNumber(r);
   vassert(n <= 15);
   return toUChar(n & 7);
}

/* Produce bit 3 of an integer register number. */
static UChar iregBit3 ( HReg r )
{
   UInt n;
   vassert(hregClass(r) == HRcInt64);
   vassert(!hregIsVirtual(r));
   n = hregNumber(r);
   vassert(n <= 15);
   return toUChar((n >> 3) & 1);
}

/* Produce a complete 4-bit integer register number. */
static UChar iregBits3210 ( HReg r )
{
   UInt n;
   vassert(hregClass(r) == HRcInt64);
   vassert(!hregIsVirtual(r));
   n = hregNumber(r);
   vassert(n <= 15);
   return toUChar(n);
}

/* Given an xmm (128bit V-class) register number, produce the
   equivalent numbered register in 64-bit I-class.  This is a bit of
   fakery which facilitates using functions that work on integer
   register numbers to be used when assembling SSE instructions
   too. */
static UInt vreg2ireg ( HReg r )
{
   UInt n;
   vassert(hregClass(r) == HRcVec128);
   vassert(!hregIsVirtual(r));
   n = hregNumber(r);
   vassert(n <= 15);
   return mkHReg(n, HRcInt64, False);
}

static UChar mkModRegRM ( UChar mod, UChar reg, UChar regmem )
{
   return toUChar( ((mod & 3) << 6) 
                   | ((reg & 7) << 3) 
                   | (regmem & 7) );
}

static UChar mkSIB ( Int shift, Int regindex, Int regbase )
{
   return toUChar( ((shift & 3) << 6) 
                   | ((regindex & 7) << 3) 
                   | (regbase & 7) );
}

static UChar* emit32 ( UChar* p, UInt w32 )
{
   *p++ = toUChar((w32)       & 0x000000FF);
   *p++ = toUChar((w32 >>  8) & 0x000000FF);
   *p++ = toUChar((w32 >> 16) & 0x000000FF);
   *p++ = toUChar((w32 >> 24) & 0x000000FF);
   return p;
}

static UChar* emit64 ( UChar* p, ULong w64 )
{
   p = emit32(p, toUInt(w64         & 0xFFFFFFFF));
   p = emit32(p, toUInt((w64 >> 32) & 0xFFFFFFFF));
   return p;
}

/* Does a sign-extend of the lowest 8 bits give 
   the original number? */
static Bool fits8bits ( UInt w32 )
{
   Int i32 = (Int)w32;
   return toBool(i32 == ((i32 << 24) >> 24));
}
/* Can the lower 32 bits be signedly widened to produce the whole
   64-bit value?  In other words, are the top 33 bits either all 0 or
   all 1 ? */
static Bool fitsIn32Bits ( ULong x )
{
   Long y0 = (Long)x;
   Long y1 = y0;
   y1 <<= 32;
   y1 >>=/*s*/ 32;
   return toBool(x == y1);
}


/* Forming mod-reg-rm bytes and scale-index-base bytes.

     greg,  0(ereg)    |  ereg is not any of: RSP RBP R12 R13
                       =  00 greg ereg

     greg,  d8(ereg)   |  ereg is neither of: RSP R12
                       =  01 greg ereg, d8

     greg,  d32(ereg)  |  ereg is neither of: RSP R12
                       =  10 greg ereg, d32

     greg,  d8(ereg)   |  ereg is either: RSP R12
                       =  01 greg 100, 0x24, d8
                       (lowest bit of rex distinguishes R12/RSP)

     greg,  d32(ereg)  |  ereg is either: RSP R12
                       =  10 greg 100, 0x24, d32
                       (lowest bit of rex distinguishes R12/RSP)

     -----------------------------------------------

     greg,  d8(base,index,scale)  
               |  index != RSP
               =  01 greg 100, scale index base, d8

     greg,  d32(base,index,scale)
               |  index != RSP
               =  10 greg 100, scale index base, d32
*/
static UChar* doAMode_M ( UChar* p, HReg greg, AMD64AMode* am ) 
{
   if (am->tag == Aam_IR) {
      if (am->Aam.IR.imm == 0 
          && am->Aam.IR.reg != hregAMD64_RSP()
          && am->Aam.IR.reg != hregAMD64_RBP() 
          && am->Aam.IR.reg != hregAMD64_R12() 
          && am->Aam.IR.reg != hregAMD64_R13() 
         ) {
         *p++ = mkModRegRM(0, iregBits210(greg), 
                              iregBits210(am->Aam.IR.reg));
         return p;
      }
      if (fits8bits(am->Aam.IR.imm)
          && am->Aam.IR.reg != hregAMD64_RSP()
          && am->Aam.IR.reg != hregAMD64_R12()
         ) {
         *p++ = mkModRegRM(1, iregBits210(greg), 
                              iregBits210(am->Aam.IR.reg));
         *p++ = toUChar(am->Aam.IR.imm & 0xFF);
         return p;
      }
      if (am->Aam.IR.reg != hregAMD64_RSP()
          && am->Aam.IR.reg != hregAMD64_R12()
         ) {
         *p++ = mkModRegRM(2, iregBits210(greg), 
                              iregBits210(am->Aam.IR.reg));
         p = emit32(p, am->Aam.IR.imm);
         return p;
      }
      if ((am->Aam.IR.reg == hregAMD64_RSP()
           || am->Aam.IR.reg == hregAMD64_R12())
          && fits8bits(am->Aam.IR.imm)) {
 	 *p++ = mkModRegRM(1, iregBits210(greg), 4);
         *p++ = 0x24;
         *p++ = toUChar(am->Aam.IR.imm & 0xFF);
         return p;
      }
      if (/* (am->Aam.IR.reg == hregAMD64_RSP()
	     || wait for test case for RSP case */
          am->Aam.IR.reg == hregAMD64_R12()) {
 	 *p++ = mkModRegRM(2, iregBits210(greg), 4);
         *p++ = 0x24;
         p = emit32(p, am->Aam.IR.imm);
         return p;
      }
      ppAMD64AMode(am);
      vpanic("doAMode_M: can't emit amode IR");
      /*NOTREACHED*/
   }
   if (am->tag == Aam_IRRS) {
      if (fits8bits(am->Aam.IRRS.imm)
          && am->Aam.IRRS.index != hregAMD64_RSP()) {
         *p++ = mkModRegRM(1, iregBits210(greg), 4);
         *p++ = mkSIB(am->Aam.IRRS.shift, am->Aam.IRRS.index, 
                                          am->Aam.IRRS.base);
         *p++ = toUChar(am->Aam.IRRS.imm & 0xFF);
         return p;
      }
      if (am->Aam.IRRS.index != hregAMD64_RSP()) {
         *p++ = mkModRegRM(2, iregBits210(greg), 4);
         *p++ = mkSIB(am->Aam.IRRS.shift, am->Aam.IRRS.index,
                                          am->Aam.IRRS.base);
         p = emit32(p, am->Aam.IRRS.imm);
         return p;
      }
      ppAMD64AMode(am);
      vpanic("doAMode_M: can't emit amode IRRS");
      /*NOTREACHED*/
   }
   vpanic("doAMode_M: unknown amode");
   /*NOTREACHED*/
}


/* Emit a mod-reg-rm byte when the rm bit denotes a reg. */
static UChar* doAMode_R ( UChar* p, HReg greg, HReg ereg ) 
{
   *p++ = mkModRegRM(3, iregBits210(greg), iregBits210(ereg));
   return p;
}


/* Clear the W bit on a REX byte, thereby changing the operand size
   back to whatever that instruction's default operand size is. */
static inline UChar clearWBit ( UChar rex )
{
   return toUChar(rex & ~(1<<3));
}


/* Make up a REX byte, with W=1 (size=64), for a (greg,amode) pair. */
static UChar rexAMode_M ( HReg greg, AMD64AMode* am )
{
   if (am->tag == Aam_IR) {
      UChar W = 1;  /* we want 64-bit mode */
      UChar R = iregBit3(greg);
      UChar X = 0; /* not relevant */
      UChar B = iregBit3(am->Aam.IR.reg);
      return toUChar(0x40 + ((W << 3) | (R << 2) | (X << 1) | (B << 0)));
   }
   if (am->tag == Aam_IRRS) {
      UChar W = 1;  /* we want 64-bit mode */
      UChar R = iregBit3(greg);
      UChar X = iregBit3(am->Aam.IRRS.index);
      UChar B = iregBit3(am->Aam.IRRS.base);
      return toUChar(0x40 + ((W << 3) | (R << 2) | (X << 1) | (B << 0)));
   }
   vassert(0);
   return 0; /*NOTREACHED*/
}

/* Make up a REX byte, with W=1 (size=64), for a (greg,ereg) pair. */
static UChar rexAMode_R ( HReg greg, HReg ereg )
{
   UChar W = 1;  /* we want 64-bit mode */
   UChar R = iregBit3(greg);
   UChar X = 0; /* not relevant */
   UChar B = iregBit3(ereg);
   return toUChar(0x40 + ((W << 3) | (R << 2) | (X << 1) | (B << 0)));
}


/* Emit ffree %st(N) */
static UChar* do_ffree_st ( UChar* p, Int n )
{
   vassert(n >= 0 && n <= 7);
   *p++ = 0xDD;
   *p++ = toUChar(0xC0 + n);
   return p;
}

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

Int emit_AMD64Instr ( UChar* buf, Int nbuf, AMD64Instr* i, 
                      Bool mode64, void* dispatch )
{
   UInt /*irno,*/ opc, opc_rr, subopc_imm, opc_imma, opc_cl, opc_imm, subopc;
   UInt   xtra;
   UInt   reg;
   UChar  rex;
   UChar* p = &buf[0];
   UChar* ptmp;
   Int    j;
   vassert(nbuf >= 32);
   vassert(mode64 == True);

   /* Wrap an integer as a int register, for use assembling
      GrpN insns, in which the greg field is used as a sub-opcode
      and does not really contain a register. */
#  define fake(_n) mkHReg((_n), HRcInt64, False)

   /* vex_printf("asm  "); ppAMD64Instr(i, mode64); vex_printf("\n"); */

   switch (i->tag) {

   case Ain_Imm64:
      if (i->Ain.Imm64.imm64 <= 0xFFFFFULL) {
         /* Use the short form (load into 32 bit reg, + default
            widening rule) for constants under 1 million.  We could
            use this form for the range 0 to 0x7FFFFFFF inclusive, but
            limit it to a smaller range for verifiability purposes. */
         if (1 & iregBit3(i->Ain.Imm64.dst))
            *p++ = 0x41;
         *p++ = 0xB8 + iregBits210(i->Ain.Imm64.dst);
         p = emit32(p, (UInt)i->Ain.Imm64.imm64);
      } else {
         *p++ = toUChar(0x48 + (1 & iregBit3(i->Ain.Imm64.dst)));
         *p++ = toUChar(0xB8 + iregBits210(i->Ain.Imm64.dst));
         p = emit64(p, i->Ain.Imm64.imm64);
      }
      goto done;

   case Ain_Alu64R:
      /* Deal specially with MOV */
      if (i->Ain.Alu64R.op == Aalu_MOV) {
         switch (i->Ain.Alu64R.src->tag) {
            case Armi_Imm:
               if (0 == (i->Ain.Alu64R.src->Armi.Imm.imm32 & ~0xFFFFF)) {
                  /* Actually we could use this form for constants in
                     the range 0 through 0x7FFFFFFF inclusive, but
                     limit it to a small range for verifiability
                     purposes. */
                  /* Generate "movl $imm32, 32-bit-register" and let
                     the default zero-extend rule cause the upper half
                     of the dst to be zeroed out too.  This saves 1
                     and sometimes 2 bytes compared to the more
                     obvious encoding in the 'else' branch. */
                  if (1 & iregBit3(i->Ain.Alu64R.dst))
                     *p++ = 0x41;
                  *p++ = 0xB8 + iregBits210(i->Ain.Alu64R.dst);
                  p = emit32(p, i->Ain.Alu64R.src->Armi.Imm.imm32);
               } else {
                  *p++ = toUChar(0x48 + (1 & iregBit3(i->Ain.Alu64R.dst)));
                  *p++ = 0xC7;
                  *p++ = toUChar(0xC0 + iregBits210(i->Ain.Alu64R.dst));
                  p = emit32(p, i->Ain.Alu64R.src->Armi.Imm.imm32);
               }
               goto done;
            case Armi_Reg:
               *p++ = rexAMode_R( i->Ain.Alu64R.src->Armi.Reg.reg,
                                  i->Ain.Alu64R.dst );
               *p++ = 0x89;
               p = doAMode_R(p, i->Ain.Alu64R.src->Armi.Reg.reg,
                                i->Ain.Alu64R.dst);
               goto done;
            case Armi_Mem:
               *p++ = rexAMode_M(i->Ain.Alu64R.dst,
                                 i->Ain.Alu64R.src->Armi.Mem.am);
               *p++ = 0x8B;
               p = doAMode_M(p, i->Ain.Alu64R.dst, 
                                i->Ain.Alu64R.src->Armi.Mem.am);
               goto done;
            default:
               goto bad;
         }
      }
      /* MUL */
      if (i->Ain.Alu64R.op == Aalu_MUL) {
         switch (i->Ain.Alu64R.src->tag) {
            case Armi_Reg:
               *p++ = rexAMode_R( i->Ain.Alu64R.dst,
                                  i->Ain.Alu64R.src->Armi.Reg.reg);
               *p++ = 0x0F;
               *p++ = 0xAF;
               p = doAMode_R(p, i->Ain.Alu64R.dst,
                                i->Ain.Alu64R.src->Armi.Reg.reg);
               goto done;
            case Armi_Mem:
               *p++ = rexAMode_M(i->Ain.Alu64R.dst,
                                 i->Ain.Alu64R.src->Armi.Mem.am);
               *p++ = 0x0F;
               *p++ = 0xAF;
               p = doAMode_M(p, i->Ain.Alu64R.dst,
                                i->Ain.Alu64R.src->Armi.Mem.am);
               goto done;
            case Armi_Imm:
               if (fits8bits(i->Ain.Alu64R.src->Armi.Imm.imm32)) {
                  *p++ = rexAMode_R(i->Ain.Alu64R.dst, i->Ain.Alu64R.dst);
                  *p++ = 0x6B;
                  p = doAMode_R(p, i->Ain.Alu64R.dst, i->Ain.Alu64R.dst);
                  *p++ = toUChar(0xFF & i->Ain.Alu64R.src->Armi.Imm.imm32);
               } else {
                  *p++ = rexAMode_R(i->Ain.Alu64R.dst, i->Ain.Alu64R.dst);
                  *p++ = 0x69;
                  p = doAMode_R(p, i->Ain.Alu64R.dst, i->Ain.Alu64R.dst);
                  p = emit32(p, i->Ain.Alu64R.src->Armi.Imm.imm32);
               }
               goto done;
            default:
               goto bad;
         }
      }
      /* ADD/SUB/ADC/SBB/AND/OR/XOR/CMP */
      opc = opc_rr = subopc_imm = opc_imma = 0;
      switch (i->Ain.Alu64R.op) {
         case Aalu_ADC: opc = 0x13; opc_rr = 0x11; 
                        subopc_imm = 2; opc_imma = 0x15; break;
         case Aalu_ADD: opc = 0x03; opc_rr = 0x01; 
                        subopc_imm = 0; opc_imma = 0x05; break;
         case Aalu_SUB: opc = 0x2B; opc_rr = 0x29; 
                        subopc_imm = 5; opc_imma = 0x2D; break;
         case Aalu_SBB: opc = 0x1B; opc_rr = 0x19; 
                        subopc_imm = 3; opc_imma = 0x1D; break;
         case Aalu_AND: opc = 0x23; opc_rr = 0x21; 
                        subopc_imm = 4; opc_imma = 0x25; break;
         case Aalu_XOR: opc = 0x33; opc_rr = 0x31; 
                        subopc_imm = 6; opc_imma = 0x35; break;
         case Aalu_OR:  opc = 0x0B; opc_rr = 0x09; 
                        subopc_imm = 1; opc_imma = 0x0D; break;
         case Aalu_CMP: opc = 0x3B; opc_rr = 0x39; 
                        subopc_imm = 7; opc_imma = 0x3D; break;
         default: goto bad;
      }
      switch (i->Ain.Alu64R.src->tag) {
         case Armi_Imm:
            if (i->Ain.Alu64R.dst == hregAMD64_RAX()
                && !fits8bits(i->Ain.Alu64R.src->Armi.Imm.imm32)) {
               goto bad; /* FIXME: awaiting test case */
               *p++ = toUChar(opc_imma);
               p = emit32(p, i->Ain.Alu64R.src->Armi.Imm.imm32);
            } else
            if (fits8bits(i->Ain.Alu64R.src->Armi.Imm.imm32)) {
               *p++ = rexAMode_R( fake(0), i->Ain.Alu64R.dst );
               *p++ = 0x83; 
               p    = doAMode_R(p, fake(subopc_imm), i->Ain.Alu64R.dst);
               *p++ = toUChar(0xFF & i->Ain.Alu64R.src->Armi.Imm.imm32);
            } else {
               *p++ = rexAMode_R( fake(0), i->Ain.Alu64R.dst);
               *p++ = 0x81; 
               p    = doAMode_R(p, fake(subopc_imm), i->Ain.Alu64R.dst);
               p    = emit32(p, i->Ain.Alu64R.src->Armi.Imm.imm32);
            }
            goto done;
         case Armi_Reg:
            *p++ = rexAMode_R( i->Ain.Alu64R.src->Armi.Reg.reg,
                               i->Ain.Alu64R.dst);
            *p++ = toUChar(opc_rr);
            p = doAMode_R(p, i->Ain.Alu64R.src->Armi.Reg.reg,
                             i->Ain.Alu64R.dst);
            goto done;
         case Armi_Mem:
            *p++ = rexAMode_M( i->Ain.Alu64R.dst,
                               i->Ain.Alu64R.src->Armi.Mem.am);
            *p++ = toUChar(opc);
            p = doAMode_M(p, i->Ain.Alu64R.dst,
                             i->Ain.Alu64R.src->Armi.Mem.am);
            goto done;
         default: 
            goto bad;
      }
      break;

   case Ain_Alu64M:
      /* Deal specially with MOV */
      if (i->Ain.Alu64M.op == Aalu_MOV) {
         switch (i->Ain.Alu64M.src->tag) {
            case Ari_Reg:
               *p++ = rexAMode_M(i->Ain.Alu64M.src->Ari.Reg.reg,
                                 i->Ain.Alu64M.dst);
               *p++ = 0x89;
               p = doAMode_M(p, i->Ain.Alu64M.src->Ari.Reg.reg,
                                i->Ain.Alu64M.dst);
               goto done;
            case Ari_Imm:
               *p++ = rexAMode_M(fake(0), i->Ain.Alu64M.dst);
               *p++ = 0xC7;
               p = doAMode_M(p, fake(0), i->Ain.Alu64M.dst);
               p = emit32(p, i->Ain.Alu64M.src->Ari.Imm.imm32);
               goto done;
            default: 
               goto bad;
         }
      }
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
      break;

   case Ain_Sh64:
      opc_cl = opc_imm = subopc = 0;
      switch (i->Ain.Sh64.op) {
         case Ash_SHR: opc_cl = 0xD3; opc_imm = 0xC1; subopc = 5; break;
         case Ash_SAR: opc_cl = 0xD3; opc_imm = 0xC1; subopc = 7; break;
         case Ash_SHL: opc_cl = 0xD3; opc_imm = 0xC1; subopc = 4; break;
         default: goto bad;
      }
      if (i->Ain.Sh64.src == 0) {
         *p++ = rexAMode_R(fake(0), i->Ain.Sh64.dst);
         *p++ = toUChar(opc_cl);
         p = doAMode_R(p, fake(subopc), i->Ain.Sh64.dst);
         goto done;
      } else {
         *p++ = rexAMode_R(fake(0), i->Ain.Sh64.dst);
         *p++ = toUChar(opc_imm);
         p = doAMode_R(p, fake(subopc), i->Ain.Sh64.dst);
         *p++ = (UChar)(i->Ain.Sh64.src);
         goto done;
      }
      break;

   case Ain_Test64:
      /* testq sign-extend($imm32), %reg */
      *p++ = rexAMode_R(fake(0), i->Ain.Test64.dst);
      *p++ = 0xF7;
      p = doAMode_R(p, fake(0), i->Ain.Test64.dst);
      p = emit32(p, i->Ain.Test64.imm32);
      goto done;

   case Ain_Unary64:
      if (i->Ain.Unary64.op == Aun_NOT) {
         *p++ = rexAMode_R(fake(0), i->Ain.Unary64.dst);
         *p++ = 0xF7;
         p = doAMode_R(p, fake(2), i->Ain.Unary64.dst);
         goto done;
      }
      if (i->Ain.Unary64.op == Aun_NEG) {
         *p++ = rexAMode_R(fake(0), i->Ain.Unary64.dst);
         *p++ = 0xF7;
         p = doAMode_R(p, fake(3), i->Ain.Unary64.dst);
         goto done;
      }
      break;

   case Ain_Lea64:
      *p++ = rexAMode_M(i->Ain.Lea64.dst, i->Ain.Lea64.am);
      *p++ = 0x8D;
      p = doAMode_M(p, i->Ain.Lea64.dst, i->Ain.Lea64.am);
      goto done;

   case Ain_MulL:
      subopc = i->Ain.MulL.syned ? 5 : 4;
      switch (i->Ain.MulL.src->tag)  {
         case Arm_Mem:
            *p++ = rexAMode_M( fake(0),
                               i->Ain.MulL.src->Arm.Mem.am);
            *p++ = 0xF7;
            p = doAMode_M(p, fake(subopc),
                             i->Ain.MulL.src->Arm.Mem.am);
            goto done;
         case Arm_Reg:
            *p++ = rexAMode_R(fake(0), 
                              i->Ain.MulL.src->Arm.Reg.reg);
            *p++ = 0xF7;
            p = doAMode_R(p, fake(subopc), 
                             i->Ain.MulL.src->Arm.Reg.reg);
            goto done;
         default:
            goto bad;
      }
      break;

   case Ain_Div:
      subopc = i->Ain.Div.syned ? 7 : 6;
      if (i->Ain.Div.sz == 4) {
         switch (i->Ain.Div.src->tag)  {
            case Arm_Mem:
               goto bad;
               /*FIXME*/
               *p++ = 0xF7;
               p = doAMode_M(p, fake(subopc),
                                i->Ain.Div.src->Arm.Mem.am);
               goto done;
            case Arm_Reg:
               *p++ = clearWBit(
                      rexAMode_R( fake(0), i->Ain.Div.src->Arm.Reg.reg));
               *p++ = 0xF7;
               p = doAMode_R(p, fake(subopc), 
                                i->Ain.Div.src->Arm.Reg.reg);
               goto done;
            default:
               goto bad;
         }
      }
      if (i->Ain.Div.sz == 8) {
         switch (i->Ain.Div.src->tag)  {
            case Arm_Mem:
               *p++ = rexAMode_M( fake(0),
                                  i->Ain.Div.src->Arm.Mem.am);
               *p++ = 0xF7;
               p = doAMode_M(p, fake(subopc),
                                i->Ain.Div.src->Arm.Mem.am);
               goto done;
            case Arm_Reg:
               *p++ = rexAMode_R( fake(0), 
                                  i->Ain.Div.src->Arm.Reg.reg);
               *p++ = 0xF7;
               p = doAMode_R(p, fake(subopc), 
                                i->Ain.Div.src->Arm.Reg.reg);
               goto done;
            default:
               goto bad;
         }
      }
      break;

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

   case Ain_Push:
      switch (i->Ain.Push.src->tag) {
         case Armi_Mem: 
            *p++ = clearWBit(
                   rexAMode_M(fake(0), i->Ain.Push.src->Armi.Mem.am));
            *p++ = 0xFF;
            p = doAMode_M(p, fake(6), i->Ain.Push.src->Armi.Mem.am);
            goto done;
         case Armi_Imm:
            *p++ = 0x68;
            p = emit32(p, i->Ain.Push.src->Armi.Imm.imm32);
            goto done;
         case Armi_Reg:
            *p++ = toUChar(0x40 + (1 & iregBit3(i->Ain.Push.src->Armi.Reg.reg)));
            *p++ = toUChar(0x50 + iregBits210(i->Ain.Push.src->Armi.Reg.reg));
            goto done;
        default: 
            goto bad;
      }

   case Ain_Call: {
      /* As per detailed comment for Ain_Call in
         getRegUsage_AMD64Instr above, %r11 is used as an address
         temporary. */
      /* jump over the following two insns if the condition does not
         hold */
      Bool shortImm = fitsIn32Bits(i->Ain.Call.target);
      if (i->Ain.Call.cond != Acc_ALWAYS) {
         *p++ = toUChar(0x70 + (0xF & (i->Ain.Call.cond ^ 1)));
         *p++ = shortImm ? 10 : 13;
         /* 10 or 13 bytes in the next two insns */
      }
      if (shortImm) {
         /* 7 bytes: movl sign-extend(imm32), %r11 */
         *p++ = 0x49;
         *p++ = 0xC7;
         *p++ = 0xC3;
         p = emit32(p, (UInt)i->Ain.Call.target);
      } else {
         /* 10 bytes: movabsq $target, %r11 */
         *p++ = 0x49;
         *p++ = 0xBB;
         p = emit64(p, i->Ain.Call.target);
      }
      /* 3 bytes: call *%r11 */
      *p++ = 0x41;
      *p++ = 0xFF;
      *p++ = 0xD3;
      goto done;
   }

   case Ain_Goto:
      /* Use ptmp for backpatching conditional jumps. */
      ptmp = NULL;

      /* First off, if this is conditional, create a conditional
         jump over the rest of it. */
      if (i->Ain.Goto.cond != Acc_ALWAYS) {
         /* jmp fwds if !condition */
         *p++ = toUChar(0x70 + (i->Ain.Goto.cond ^ 1));
         ptmp = p; /* fill in this bit later */
         *p++ = 0; /* # of bytes to jump over; don't know how many yet. */
      }

      /* If a non-boring, set %rbp (the guest state pointer)
         appropriately.  Since these numbers are all small positive
         integers, we can get away with "movl $N, %ebp" rather than
         the longer "movq $N, %rbp". */
      /* movl $magic_number, %ebp */
      switch (i->Ain.Goto.jk) {
         case Ijk_ClientReq: 
            *p++ = 0xBD;
            p = emit32(p, VEX_TRC_JMP_CLIENTREQ); break;
         case Ijk_Sys_syscall: 
            *p++ = 0xBD;
            p = emit32(p, VEX_TRC_JMP_SYS_SYSCALL); break;
         case Ijk_Sys_int32: 
            *p++ = 0xBD;
            p = emit32(p, VEX_TRC_JMP_SYS_INT32); break;
         case Ijk_Yield: 
            *p++ = 0xBD;
            p = emit32(p, VEX_TRC_JMP_YIELD); break;
         case Ijk_EmWarn:
            *p++ = 0xBD;
            p = emit32(p, VEX_TRC_JMP_EMWARN); break;
         case Ijk_MapFail:
            *p++ = 0xBD;
            p = emit32(p, VEX_TRC_JMP_MAPFAIL); break;
         case Ijk_NoDecode:
            *p++ = 0xBD;
            p = emit32(p, VEX_TRC_JMP_NODECODE); break;
         case Ijk_TInval:
            *p++ = 0xBD;
            p = emit32(p, VEX_TRC_JMP_TINVAL); break;
         case Ijk_NoRedir:
            *p++ = 0xBD;
            p = emit32(p, VEX_TRC_JMP_NOREDIR); break;
         case Ijk_SigTRAP:
            *p++ = 0xBD;
            p = emit32(p, VEX_TRC_JMP_SIGTRAP); break;
         case Ijk_SigSEGV:
            *p++ = 0xBD;
            p = emit32(p, VEX_TRC_JMP_SIGSEGV); break;
         case Ijk_Ret:
         case Ijk_Call:
         case Ijk_Boring:
            break;
         default: 
            ppIRJumpKind(i->Ain.Goto.jk);
            vpanic("emit_AMD64Instr.Ain_Goto: unknown jump kind");
      }

      /* Get the destination address into %rax */
      if (i->Ain.Goto.dst->tag == Ari_Imm) {
         /* movl sign-ext($immediate), %rax ; ret */
         *p++ = 0x48;
         *p++ = 0xC7;
         *p++ = 0xC0;
         p = emit32(p, i->Ain.Goto.dst->Ari.Imm.imm32);
      } else {
         vassert(i->Ain.Goto.dst->tag == Ari_Reg);
         /* movq %reg, %rax ; ret */
         if (i->Ain.Goto.dst->Ari.Reg.reg != hregAMD64_RAX()) {
            *p++ = rexAMode_R(i->Ain.Goto.dst->Ari.Reg.reg, hregAMD64_RAX());
            *p++ = 0x89;
            p = doAMode_R(p, i->Ain.Goto.dst->Ari.Reg.reg, hregAMD64_RAX());
         }
      }

      /* Get the dispatcher address into %rdx.  This has to happen
         after the load of %rax since %rdx might be carrying the value
         destined for %rax immediately prior to this Ain_Goto. */
      vassert(sizeof(ULong) == sizeof(void*));
      vassert(dispatch != NULL);

      if (fitsIn32Bits(Ptr_to_ULong(dispatch))) {
         /* movl sign-extend(imm32), %rdx */
         *p++ = 0x48;
         *p++ = 0xC7;
         *p++ = 0xC2;
         p = emit32(p, (UInt)Ptr_to_ULong(dispatch));
      } else {
         /* movabsq $imm64, %rdx */
         *p++ = 0x48;
         *p++ = 0xBA;
         p = emit64(p, Ptr_to_ULong(dispatch));
      }
      /* jmp *%rdx */
      *p++ = 0xFF;
      *p++ = 0xE2;

      /* Fix up the conditional jump, if there was one. */
      if (i->Ain.Goto.cond != Acc_ALWAYS) {
         Int delta = p - ptmp;
         vassert(delta > 0 && delta < 30);
         *ptmp = toUChar(delta-1);
      }
      goto done;

   case Ain_CMov64:
      vassert(i->Ain.CMov64.cond != Acc_ALWAYS);
      if (i->Ain.CMov64.src->tag == Arm_Reg) {
         *p++ = rexAMode_R(i->Ain.CMov64.dst, i->Ain.CMov64.src->Arm.Reg.reg);
         *p++ = 0x0F;
         *p++ = toUChar(0x40 + (0xF & i->Ain.CMov64.cond));
         p = doAMode_R(p, i->Ain.CMov64.dst, i->Ain.CMov64.src->Arm.Reg.reg);
         goto done;
      }
      if (i->Ain.CMov64.src->tag == Arm_Mem) {
         *p++ = rexAMode_M(i->Ain.CMov64.dst, i->Ain.CMov64.src->Arm.Mem.am);
         *p++ = 0x0F;
         *p++ = toUChar(0x40 + (0xF & i->Ain.CMov64.cond));
         p = doAMode_M(p, i->Ain.CMov64.dst, i->Ain.CMov64.src->Arm.Mem.am);
         goto done;
      }
      break;

   case Ain_MovxLQ:
      /* No, _don't_ ask me why the sense of the args has to be
         different in the S vs Z case.  I don't know. */
      if (i->Ain.MovxLQ.syned) {
         /* Need REX.W = 1 here, but rexAMode_R does that for us. */
         *p++ = rexAMode_R(i->Ain.MovxLQ.dst, i->Ain.MovxLQ.src);
         *p++ = 0x63;
         p = doAMode_R(p, i->Ain.MovxLQ.dst, i->Ain.MovxLQ.src);
      } else {
         /* Produce a 32-bit reg-reg move, since the implicit
            zero-extend does what we want. */
         *p++ = clearWBit (
                   rexAMode_R(i->Ain.MovxLQ.src, i->Ain.MovxLQ.dst));
         *p++ = 0x89;
         p = doAMode_R(p, i->Ain.MovxLQ.src, i->Ain.MovxLQ.dst);
      }
      goto done;

   case Ain_LoadEX:
      if (i->Ain.LoadEX.szSmall == 1 && !i->Ain.LoadEX.syned) {
         /* movzbq */
         *p++ = rexAMode_M(i->Ain.LoadEX.dst, i->Ain.LoadEX.src); 
         *p++ = 0x0F;
         *p++ = 0xB6;
         p = doAMode_M(p, i->Ain.LoadEX.dst, i->Ain.LoadEX.src); 
         goto done;
      }
      if (i->Ain.LoadEX.szSmall == 2 && !i->Ain.LoadEX.syned) {
         /* movzwq */
         *p++ = rexAMode_M(i->Ain.LoadEX.dst, i->Ain.LoadEX.src); 
         *p++ = 0x0F;
         *p++ = 0xB7;
         p = doAMode_M(p, i->Ain.LoadEX.dst, i->Ain.LoadEX.src); 
         goto done;
      }
      if (i->Ain.LoadEX.szSmall == 4 && !i->Ain.LoadEX.syned) {
         /* movzlq */
         /* This isn't really an existing AMD64 instruction per se.
            Rather, we have to do a 32-bit load.  Because a 32-bit
            write implicitly clears the upper 32 bits of the target
            register, we get what we want. */
         *p++ = clearWBit(
                rexAMode_M(i->Ain.LoadEX.dst, i->Ain.LoadEX.src));
         *p++ = 0x8B;
         p = doAMode_M(p, i->Ain.LoadEX.dst, i->Ain.LoadEX.src);
         goto done;
      }
      break;

   case Ain_Set64:
      /* Make the destination register be 1 or 0, depending on whether
         the relevant condition holds.  Complication: the top 56 bits
         of the destination should be forced to zero, but doing 'xorq
         %r,%r' kills the flag(s) we are about to read.  Sigh.  So
         start off my moving $0 into the dest. */
      reg = iregBits3210(i->Ain.Set64.dst);
      vassert(reg < 16);

      /* movq $0, %dst */
      *p++ = toUChar(reg >= 8 ? 0x49 : 0x48);
      *p++ = 0xC7;
      *p++ = toUChar(0xC0 + (reg & 7));
      p = emit32(p, 0);

      /* setb lo8(%dst) */
      /* note, 8-bit register rex trickyness.  Be careful here. */
      *p++ = toUChar(reg >= 8 ? 0x41 : 0x40);
      *p++ = 0x0F; 
      *p++ = toUChar(0x90 + (0x0F & i->Ain.Set64.cond));
      *p++ = toUChar(0xC0 + (reg & 7));
      goto done;

   case Ain_Bsfr64:
      *p++ = rexAMode_R(i->Ain.Bsfr64.dst, i->Ain.Bsfr64.src);
      *p++ = 0x0F;
      if (i->Ain.Bsfr64.isFwds) {
         *p++ = 0xBC;
      } else {
         *p++ = 0xBD;
      }
      p = doAMode_R(p, i->Ain.Bsfr64.dst, i->Ain.Bsfr64.src);
      goto done;

   case Ain_MFence:
      /* mfence */
      *p++ = 0x0F; *p++ = 0xAE; *p++ = 0xF0;
      goto done;

   case Ain_ACAS:
      /* lock */
      *p++ = 0xF0;
      if (i->Ain.ACAS.sz == 2) *p++ = 0x66; 
      /* cmpxchg{b,w,l,q} %rbx,mem.  Expected-value in %rax, new value
         in %rbx.  The new-value register is hardwired to be %rbx
         since dealing with byte integer registers is too much hassle,
         so we force the register operand to %rbx (could equally be
         %rcx or %rdx). */
      rex = rexAMode_M( hregAMD64_RBX(), i->Ain.ACAS.addr );
      if (i->Ain.ACAS.sz != 8)
         rex = clearWBit(rex);

      *p++ = rex; /* this can emit 0x40, which is pointless. oh well. */
      *p++ = 0x0F;
      if (i->Ain.ACAS.sz == 1) *p++ = 0xB0; else *p++ = 0xB1;
      p = doAMode_M(p, hregAMD64_RBX(), i->Ain.ACAS.addr);
      goto done;

   case Ain_DACAS:
      /* lock */
      *p++ = 0xF0;
      /* cmpxchg{8,16}b m{64,128}.  Expected-value in %rdx:%rax, new
         value in %rcx:%rbx.  All 4 regs are hardwired in the ISA, so
         aren't encoded in the insn. */
      rex = rexAMode_M( fake(1), i->Ain.ACAS.addr );
      if (i->Ain.ACAS.sz != 8)
         rex = clearWBit(rex);
      *p++ = rex;
      *p++ = 0x0F;
      *p++ = 0xC7;
      p = doAMode_M(p, fake(1), i->Ain.DACAS.addr);
      goto done;

   case Ain_A87Free:
      vassert(i->Ain.A87Free.nregs > 0 && i->Ain.A87Free.nregs <= 7);
      for (j = 0; j < i->Ain.A87Free.nregs; j++) {
         p = do_ffree_st(p, 7-j);
      }
      goto done;

   case Ain_A87PushPop:
      vassert(i->Ain.A87PushPop.szB == 8 || i->Ain.A87PushPop.szB == 4);
      if (i->Ain.A87PushPop.isPush) {
         /* Load from memory into %st(0): flds/fldl amode */
         *p++ = clearWBit(
                   rexAMode_M(fake(0), i->Ain.A87PushPop.addr) );
         *p++ = i->Ain.A87PushPop.szB == 4 ? 0xD9 : 0xDD;
	 p = doAMode_M(p, fake(0)/*subopcode*/, i->Ain.A87PushPop.addr);
      } else {
         /* Dump %st(0) to memory: fstps/fstpl amode */
         *p++ = clearWBit(
                   rexAMode_M(fake(3), i->Ain.A87PushPop.addr) );
         *p++ = i->Ain.A87PushPop.szB == 4 ? 0xD9 : 0xDD;
         p = doAMode_M(p, fake(3)/*subopcode*/, i->Ain.A87PushPop.addr);
         goto done;
      }
      goto done;

   case Ain_A87FpOp:
      switch (i->Ain.A87FpOp.op) {
         case Afp_SQRT:   *p++ = 0xD9; *p++ = 0xFA; break;
         case Afp_SIN:    *p++ = 0xD9; *p++ = 0xFE; break;
         case Afp_COS:    *p++ = 0xD9; *p++ = 0xFF; break;
         case Afp_TAN:    *p++ = 0xD9; *p++ = 0xF2; break;
         case Afp_ROUND:  *p++ = 0xD9; *p++ = 0xFC; break;
         case Afp_2XM1:   *p++ = 0xD9; *p++ = 0xF0; break;
         case Afp_SCALE:  *p++ = 0xD9; *p++ = 0xFD; break;
         case Afp_ATAN:   *p++ = 0xD9; *p++ = 0xF3; break;
         case Afp_YL2X:   *p++ = 0xD9; *p++ = 0xF1; break;
         case Afp_YL2XP1: *p++ = 0xD9; *p++ = 0xF9; break;
         case Afp_PREM:   *p++ = 0xD9; *p++ = 0xF8; break;
         case Afp_PREM1:  *p++ = 0xD9; *p++ = 0xF5; break;
         default: goto bad;
      }
      goto done;

   case Ain_A87LdCW:
      *p++ = clearWBit(
                rexAMode_M(fake(5), i->Ain.A87LdCW.addr) );
      *p++ = 0xD9;
      p = doAMode_M(p, fake(5)/*subopcode*/, i->Ain.A87LdCW.addr);
      goto done;

   case Ain_A87StSW:
      *p++ = clearWBit(
                rexAMode_M(fake(7), i->Ain.A87StSW.addr) );
      *p++ = 0xDD;
      p = doAMode_M(p, fake(7)/*subopcode*/, i->Ain.A87StSW.addr);
      goto done;

   case Ain_Store:
      if (i->Ain.Store.sz == 2) {
         /* This just goes to show the crazyness of the instruction
            set encoding.  We have to insert two prefix bytes, but be
            careful to avoid a conflict in what the size should be, by
            ensuring that REX.W = 0. */
         *p++ = 0x66; /* override to 16-bits */
	 *p++ = clearWBit( rexAMode_M( i->Ain.Store.src, i->Ain.Store.dst) );
         *p++ = 0x89;
         p = doAMode_M(p, i->Ain.Store.src, i->Ain.Store.dst);
         goto done;
      }
      if (i->Ain.Store.sz == 4) {
	 *p++ = clearWBit( rexAMode_M( i->Ain.Store.src, i->Ain.Store.dst) );
         *p++ = 0x89;
         p = doAMode_M(p, i->Ain.Store.src, i->Ain.Store.dst);
         goto done;
      }
      if (i->Ain.Store.sz == 1) {
         /* This is one place where it would be wrong to skip emitting
            a rex byte of 0x40, since the mere presence of rex changes
            the meaning of the byte register access.  Be careful. */
	 *p++ = clearWBit( rexAMode_M( i->Ain.Store.src, i->Ain.Store.dst) );
         *p++ = 0x88;
         p = doAMode_M(p, i->Ain.Store.src, i->Ain.Store.dst);
         goto done;
      }
      break;

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

   case Ain_LdMXCSR:
      *p++ = clearWBit(rexAMode_M( fake(0), i->Ain.LdMXCSR.addr));
      *p++ = 0x0F;
      *p++ = 0xAE;
      p = doAMode_M(p, fake(2)/*subopcode*/, i->Ain.LdMXCSR.addr);
      goto done;

//..    case Xin_FpStSW_AX:
//..       /* note, this emits fnstsw %ax, not fstsw %ax */
//..       *p++ = 0xDF;
//..       *p++ = 0xE0;
//..       goto done;

   case Ain_SseUComIS:
      /* ucomi[sd] %srcL, %srcR ;  pushfq ; popq %dst */
      /* ucomi[sd] %srcL, %srcR */
      if (i->Ain.SseUComIS.sz == 8) {
         *p++ = 0x66;
      } else {
         goto bad;
         vassert(i->Ain.SseUComIS.sz == 4);
      }
      *p++ = clearWBit (
             rexAMode_R( vreg2ireg(i->Ain.SseUComIS.srcL),
                         vreg2ireg(i->Ain.SseUComIS.srcR) ));
      *p++ = 0x0F;
      *p++ = 0x2E;
      p = doAMode_R(p, vreg2ireg(i->Ain.SseUComIS.srcL),
                       vreg2ireg(i->Ain.SseUComIS.srcR) );
      /* pushfq */
      *p++ = 0x9C;
      /* popq %dst */
      *p++ = toUChar(0x40 + (1 & iregBit3(i->Ain.SseUComIS.dst)));
      *p++ = toUChar(0x58 + iregBits210(i->Ain.SseUComIS.dst));
      goto done;

   case Ain_SseSI2SF:
      /* cvssi2s[sd] %src, %dst */
      rex = rexAMode_R( vreg2ireg(i->Ain.SseSI2SF.dst),
                        i->Ain.SseSI2SF.src );
      *p++ = toUChar(i->Ain.SseSI2SF.szD==4 ? 0xF3 : 0xF2);
      *p++ = toUChar(i->Ain.SseSI2SF.szS==4 ? clearWBit(rex) : rex);
      *p++ = 0x0F;
      *p++ = 0x2A;
      p = doAMode_R( p, vreg2ireg(i->Ain.SseSI2SF.dst),
                        i->Ain.SseSI2SF.src );
      goto done;

   case Ain_SseSF2SI:
      /* cvss[sd]2si %src, %dst */
      rex = rexAMode_R( i->Ain.SseSF2SI.dst,
                        vreg2ireg(i->Ain.SseSF2SI.src) );
      *p++ = toUChar(i->Ain.SseSF2SI.szS==4 ? 0xF3 : 0xF2);
      *p++ = toUChar(i->Ain.SseSF2SI.szD==4 ? clearWBit(rex) : rex);
      *p++ = 0x0F;
      *p++ = 0x2D;
      p = doAMode_R( p, i->Ain.SseSF2SI.dst,
                        vreg2ireg(i->Ain.SseSF2SI.src) );
      goto done;

   case Ain_SseSDSS:
      /* cvtsd2ss/cvtss2sd %src, %dst */
      *p++ = toUChar(i->Ain.SseSDSS.from64 ? 0xF2 : 0xF3);
      *p++ = clearWBit(
              rexAMode_R( vreg2ireg(i->Ain.SseSDSS.dst),
                          vreg2ireg(i->Ain.SseSDSS.src) ));
      *p++ = 0x0F;
      *p++ = 0x5A;
      p = doAMode_R( p, vreg2ireg(i->Ain.SseSDSS.dst),
                        vreg2ireg(i->Ain.SseSDSS.src) );
      goto done;

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

   case Ain_SseLdSt:
      if (i->Ain.SseLdSt.sz == 8) {
         *p++ = 0xF2;
      } else
      if (i->Ain.SseLdSt.sz == 4) {
         *p++ = 0xF3;
      } else
      if (i->Ain.SseLdSt.sz != 16) {
         vassert(0);
      }
      *p++ = clearWBit(
             rexAMode_M( vreg2ireg(i->Ain.SseLdSt.reg), i->Ain.SseLdSt.addr));
      *p++ = 0x0F; 
      *p++ = toUChar(i->Ain.SseLdSt.isLoad ? 0x10 : 0x11);
      p = doAMode_M(p, vreg2ireg(i->Ain.SseLdSt.reg), i->Ain.SseLdSt.addr);
      goto done;

   case Ain_SseLdzLO:
      vassert(i->Ain.SseLdzLO.sz == 4 || i->Ain.SseLdzLO.sz == 8);
      /* movs[sd] amode, %xmm-dst */
      *p++ = toUChar(i->Ain.SseLdzLO.sz==4 ? 0xF3 : 0xF2);
      *p++ = clearWBit(
             rexAMode_M(vreg2ireg(i->Ain.SseLdzLO.reg), 
                        i->Ain.SseLdzLO.addr));
      *p++ = 0x0F; 
      *p++ = 0x10; 
      p = doAMode_M(p, vreg2ireg(i->Ain.SseLdzLO.reg), 
                       i->Ain.SseLdzLO.addr);
      goto done;

   case Ain_Sse32Fx4:
      xtra = 0;
      *p++ = clearWBit(
             rexAMode_R( vreg2ireg(i->Ain.Sse32Fx4.dst),
                         vreg2ireg(i->Ain.Sse32Fx4.src) ));
      *p++ = 0x0F;
      switch (i->Ain.Sse32Fx4.op) {
         case Asse_ADDF:   *p++ = 0x58; break;
         case Asse_DIVF:   *p++ = 0x5E; break;
         case Asse_MAXF:   *p++ = 0x5F; break;
         case Asse_MINF:   *p++ = 0x5D; break;
         case Asse_MULF:   *p++ = 0x59; break;
         case Asse_RCPF:   *p++ = 0x53; break;
         case Asse_RSQRTF: *p++ = 0x52; break;
         case Asse_SQRTF:  *p++ = 0x51; break;
         case Asse_SUBF:   *p++ = 0x5C; break;
         case Asse_CMPEQF: *p++ = 0xC2; xtra = 0x100; break;
         case Asse_CMPLTF: *p++ = 0xC2; xtra = 0x101; break;
         case Asse_CMPLEF: *p++ = 0xC2; xtra = 0x102; break;
         case Asse_CMPUNF: *p++ = 0xC2; xtra = 0x103; break;
         default: goto bad;
      }
      p = doAMode_R(p, vreg2ireg(i->Ain.Sse32Fx4.dst),
                       vreg2ireg(i->Ain.Sse32Fx4.src) );
      if (xtra & 0x100)
         *p++ = toUChar(xtra & 0xFF);
      goto done;

   case Ain_Sse64Fx2:
      xtra = 0;
      *p++ = 0x66;
      *p++ = clearWBit(
             rexAMode_R( vreg2ireg(i->Ain.Sse64Fx2.dst),
                         vreg2ireg(i->Ain.Sse64Fx2.src) ));
      *p++ = 0x0F;
      switch (i->Ain.Sse64Fx2.op) {
         case Asse_ADDF:   *p++ = 0x58; break;
         case Asse_DIVF:   *p++ = 0x5E; break;
         case Asse_MAXF:   *p++ = 0x5F; break;
         case Asse_MINF:   *p++ = 0x5D; break;
         case Asse_MULF:   *p++ = 0x59; break;
//..          case Xsse_RCPF:   *p++ = 0x53; break;
//..          case Xsse_RSQRTF: *p++ = 0x52; break;
         case Asse_SQRTF:  *p++ = 0x51; break;
         case Asse_SUBF:   *p++ = 0x5C; break;
         case Asse_CMPEQF: *p++ = 0xC2; xtra = 0x100; break;
         case Asse_CMPLTF: *p++ = 0xC2; xtra = 0x101; break;
         case Asse_CMPLEF: *p++ = 0xC2; xtra = 0x102; break;
         case Asse_CMPUNF: *p++ = 0xC2; xtra = 0x103; break;
         default: goto bad;
      }
      p = doAMode_R(p, vreg2ireg(i->Ain.Sse64Fx2.dst),
                       vreg2ireg(i->Ain.Sse64Fx2.src) );
      if (xtra & 0x100)
         *p++ = toUChar(xtra & 0xFF);
      goto done;

   case Ain_Sse32FLo:
      xtra = 0;
      *p++ = 0xF3;
      *p++ = clearWBit(
             rexAMode_R( vreg2ireg(i->Ain.Sse32FLo.dst),
                         vreg2ireg(i->Ain.Sse32FLo.src) ));
      *p++ = 0x0F;
      switch (i->Ain.Sse32FLo.op) {
         case Asse_ADDF:   *p++ = 0x58; break;
         case Asse_DIVF:   *p++ = 0x5E; break;
         case Asse_MAXF:   *p++ = 0x5F; break;
         case Asse_MINF:   *p++ = 0x5D; break;
         case Asse_MULF:   *p++ = 0x59; break;
         case Asse_RCPF:   *p++ = 0x53; break;
         case Asse_RSQRTF: *p++ = 0x52; break;
         case Asse_SQRTF:  *p++ = 0x51; break;
         case Asse_SUBF:   *p++ = 0x5C; break;
         case Asse_CMPEQF: *p++ = 0xC2; xtra = 0x100; break;
         case Asse_CMPLTF: *p++ = 0xC2; xtra = 0x101; break;
         case Asse_CMPLEF: *p++ = 0xC2; xtra = 0x102; break;
         case Asse_CMPUNF: *p++ = 0xC2; xtra = 0x103; break;
         default: goto bad;
      }
      p = doAMode_R(p, vreg2ireg(i->Ain.Sse32FLo.dst),
                       vreg2ireg(i->Ain.Sse32FLo.src) );
      if (xtra & 0x100)
         *p++ = toUChar(xtra & 0xFF);
      goto done;

   case Ain_Sse64FLo:
      xtra = 0;
      *p++ = 0xF2;
      *p++ = clearWBit(
             rexAMode_R( vreg2ireg(i->Ain.Sse64FLo.dst),
                         vreg2ireg(i->Ain.Sse64FLo.src) ));
      *p++ = 0x0F;
      switch (i->Ain.Sse64FLo.op) {
         case Asse_ADDF:   *p++ = 0x58; break;
         case Asse_DIVF:   *p++ = 0x5E; break;
         case Asse_MAXF:   *p++ = 0x5F; break;
         case Asse_MINF:   *p++ = 0x5D; break;
         case Asse_MULF:   *p++ = 0x59; break;
//..          case Xsse_RCPF:   *p++ = 0x53; break;
//..          case Xsse_RSQRTF: *p++ = 0x52; break;
         case Asse_SQRTF:  *p++ = 0x51; break;
         case Asse_SUBF:   *p++ = 0x5C; break;
         case Asse_CMPEQF: *p++ = 0xC2; xtra = 0x100; break;
         case Asse_CMPLTF: *p++ = 0xC2; xtra = 0x101; break;
         case Asse_CMPLEF: *p++ = 0xC2; xtra = 0x102; break;
         case Asse_CMPUNF: *p++ = 0xC2; xtra = 0x103; break;
         default: goto bad;
      }
      p = doAMode_R(p, vreg2ireg(i->Ain.Sse64FLo.dst),
                       vreg2ireg(i->Ain.Sse64FLo.src) );
      if (xtra & 0x100)
         *p++ = toUChar(xtra & 0xFF);
      goto done;

   case Ain_SseReRg:
#     define XX(_n) *p++ = (_n)

      rex = clearWBit(
            rexAMode_R( vreg2ireg(i->Ain.SseReRg.dst),
                        vreg2ireg(i->Ain.SseReRg.src) ));

      switch (i->Ain.SseReRg.op) {
         case Asse_MOV:     /*movups*/ XX(rex); XX(0x0F); XX(0x10); break;
         case Asse_OR:                 XX(rex); XX(0x0F); XX(0x56); break;
         case Asse_XOR:                XX(rex); XX(0x0F); XX(0x57); break;
         case Asse_AND:                XX(rex); XX(0x0F); XX(0x54); break;
         case Asse_ANDN:               XX(rex); XX(0x0F); XX(0x55); break;
         case Asse_PACKSSD:  XX(0x66); XX(rex); XX(0x0F); XX(0x6B); break;
         case Asse_PACKSSW:  XX(0x66); XX(rex); XX(0x0F); XX(0x63); break;
         case Asse_PACKUSW:  XX(0x66); XX(rex); XX(0x0F); XX(0x67); break;
         case Asse_ADD8:     XX(0x66); XX(rex); XX(0x0F); XX(0xFC); break;
         case Asse_ADD16:    XX(0x66); XX(rex); XX(0x0F); XX(0xFD); break;
         case Asse_ADD32:    XX(0x66); XX(rex); XX(0x0F); XX(0xFE); break;
         case Asse_ADD64:    XX(0x66); XX(rex); XX(0x0F); XX(0xD4); break;
         case Asse_QADD8S:   XX(0x66); XX(rex); XX(0x0F); XX(0xEC); break;
         case Asse_QADD16S:  XX(0x66); XX(rex); XX(0x0F); XX(0xED); break;
         case Asse_QADD8U:   XX(0x66); XX(rex); XX(0x0F); XX(0xDC); break;
         case Asse_QADD16U:  XX(0x66); XX(rex); XX(0x0F); XX(0xDD); break;
         case Asse_AVG8U:    XX(0x66); XX(rex); XX(0x0F); XX(0xE0); break;
         case Asse_AVG16U:   XX(0x66); XX(rex); XX(0x0F); XX(0xE3); break;
         case Asse_CMPEQ8:   XX(0x66); XX(rex); XX(0x0F); XX(0x74); break;
         case Asse_CMPEQ16:  XX(0x66); XX(rex); XX(0x0F); XX(0x75); break;
         case Asse_CMPEQ32:  XX(0x66); XX(rex); XX(0x0F); XX(0x76); break;
         case Asse_CMPGT8S:  XX(0x66); XX(rex); XX(0x0F); XX(0x64); break;
         case Asse_CMPGT16S: XX(0x66); XX(rex); XX(0x0F); XX(0x65); break;
         case Asse_CMPGT32S: XX(0x66); XX(rex); XX(0x0F); XX(0x66); break;
         case Asse_MAX16S:   XX(0x66); XX(rex); XX(0x0F); XX(0xEE); break;
         case Asse_MAX8U:    XX(0x66); XX(rex); XX(0x0F); XX(0xDE); break;
         case Asse_MIN16S:   XX(0x66); XX(rex); XX(0x0F); XX(0xEA); break;
         case Asse_MIN8U:    XX(0x66); XX(rex); XX(0x0F); XX(0xDA); break;
         case Asse_MULHI16U: XX(0x66); XX(rex); XX(0x0F); XX(0xE4); break;
         case Asse_MULHI16S: XX(0x66); XX(rex); XX(0x0F); XX(0xE5); break;
         case Asse_MUL16:    XX(0x66); XX(rex); XX(0x0F); XX(0xD5); break;
         case Asse_SHL16:    XX(0x66); XX(rex); XX(0x0F); XX(0xF1); break;
         case Asse_SHL32:    XX(0x66); XX(rex); XX(0x0F); XX(0xF2); break;
         case Asse_SHL64:    XX(0x66); XX(rex); XX(0x0F); XX(0xF3); break;
         case Asse_SAR16:    XX(0x66); XX(rex); XX(0x0F); XX(0xE1); break;
         case Asse_SAR32:    XX(0x66); XX(rex); XX(0x0F); XX(0xE2); break;
         case Asse_SHR16:    XX(0x66); XX(rex); XX(0x0F); XX(0xD1); break;
         case Asse_SHR32:    XX(0x66); XX(rex); XX(0x0F); XX(0xD2); break;
         case Asse_SHR64:    XX(0x66); XX(rex); XX(0x0F); XX(0xD3); break;
         case Asse_SUB8:     XX(0x66); XX(rex); XX(0x0F); XX(0xF8); break;
         case Asse_SUB16:    XX(0x66); XX(rex); XX(0x0F); XX(0xF9); break;
         case Asse_SUB32:    XX(0x66); XX(rex); XX(0x0F); XX(0xFA); break;
         case Asse_SUB64:    XX(0x66); XX(rex); XX(0x0F); XX(0xFB); break;
         case Asse_QSUB8S:   XX(0x66); XX(rex); XX(0x0F); XX(0xE8); break;
         case Asse_QSUB16S:  XX(0x66); XX(rex); XX(0x0F); XX(0xE9); break;
         case Asse_QSUB8U:   XX(0x66); XX(rex); XX(0x0F); XX(0xD8); break;
         case Asse_QSUB16U:  XX(0x66); XX(rex); XX(0x0F); XX(0xD9); break;
         case Asse_UNPCKHB:  XX(0x66); XX(rex); XX(0x0F); XX(0x68); break;
         case Asse_UNPCKHW:  XX(0x66); XX(rex); XX(0x0F); XX(0x69); break;
         case Asse_UNPCKHD:  XX(0x66); XX(rex); XX(0x0F); XX(0x6A); break;
         case Asse_UNPCKHQ:  XX(0x66); XX(rex); XX(0x0F); XX(0x6D); break;
         case Asse_UNPCKLB:  XX(0x66); XX(rex); XX(0x0F); XX(0x60); break;
         case Asse_UNPCKLW:  XX(0x66); XX(rex); XX(0x0F); XX(0x61); break;
         case Asse_UNPCKLD:  XX(0x66); XX(rex); XX(0x0F); XX(0x62); break;
         case Asse_UNPCKLQ:  XX(0x66); XX(rex); XX(0x0F); XX(0x6C); break;
         default: goto bad;
      }
      p = doAMode_R(p, vreg2ireg(i->Ain.SseReRg.dst),
                       vreg2ireg(i->Ain.SseReRg.src) );
#     undef XX
      goto done;

   case Ain_SseCMov:
      /* jmp fwds if !condition */
      *p++ = toUChar(0x70 + (i->Ain.SseCMov.cond ^ 1));
      *p++ = 0; /* # of bytes in the next bit, which we don't know yet */
      ptmp = p;

      /* movaps %src, %dst */
      *p++ = clearWBit(
             rexAMode_R( vreg2ireg(i->Ain.SseCMov.dst),
                         vreg2ireg(i->Ain.SseCMov.src) ));
      *p++ = 0x0F; 
      *p++ = 0x28; 
      p = doAMode_R(p, vreg2ireg(i->Ain.SseCMov.dst),
                       vreg2ireg(i->Ain.SseCMov.src) );

      /* Fill in the jump offset. */
      *(ptmp-1) = toUChar(p - ptmp);
      goto done;

   case Ain_SseShuf:
      *p++ = 0x66; 
      *p++ = clearWBit(
             rexAMode_R( vreg2ireg(i->Ain.SseShuf.dst),
                         vreg2ireg(i->Ain.SseShuf.src) ));
      *p++ = 0x0F; 
      *p++ = 0x70; 
      p = doAMode_R(p, vreg2ireg(i->Ain.SseShuf.dst),
                       vreg2ireg(i->Ain.SseShuf.src) );
      *p++ = (UChar)(i->Ain.SseShuf.order);
      goto done;

   default: 
      goto bad;
   }

  bad:
   ppAMD64Instr(i, mode64);
   vpanic("emit_AMD64Instr");
   /*NOTREACHED*/
   
  done:
   vassert(p - &buf[0] <= 32);
   return p - &buf[0];

#  undef fake
}

/*---------------------------------------------------------------*/
/*--- end                                   host_amd64_defs.c ---*/
/*---------------------------------------------------------------*/
