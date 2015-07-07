
/*---------------------------------------------------------------*/
/*--- begin                                 host_amd64_defs.c ---*/
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
#include "libvex.h"
#include "libvex_trc_values.h"

#include "main_util.h"
#include "host_generic_regs.h"
#include "host_amd64_defs.h"


/* --------- Registers. --------- */

const RRegUniverse* getRRegUniverse_AMD64 ( void )
{
   /* The real-register universe is a big constant, so we just want to
      initialise it once. */
   static RRegUniverse rRegUniverse_AMD64;
   static Bool         rRegUniverse_AMD64_initted = False;

   /* Handy shorthand, nothing more */
   RRegUniverse* ru = &rRegUniverse_AMD64;

   /* This isn't thread-safe.  Sigh. */
   if (LIKELY(rRegUniverse_AMD64_initted))
      return ru;

   RRegUniverse__init(ru);

   /* Add the registers.  The initial segment of this array must be
      those available for allocation by reg-alloc, and those that
      follow are not available for allocation. */
   ru->regs[ru->size++] = hregAMD64_RSI();
   ru->regs[ru->size++] = hregAMD64_RDI();
   ru->regs[ru->size++] = hregAMD64_R8();
   ru->regs[ru->size++] = hregAMD64_R9();
   ru->regs[ru->size++] = hregAMD64_R12();
   ru->regs[ru->size++] = hregAMD64_R13();
   ru->regs[ru->size++] = hregAMD64_R14();
   ru->regs[ru->size++] = hregAMD64_R15();
   ru->regs[ru->size++] = hregAMD64_RBX();
   ru->regs[ru->size++] = hregAMD64_XMM3();
   ru->regs[ru->size++] = hregAMD64_XMM4();
   ru->regs[ru->size++] = hregAMD64_XMM5();
   ru->regs[ru->size++] = hregAMD64_XMM6();
   ru->regs[ru->size++] = hregAMD64_XMM7();
   ru->regs[ru->size++] = hregAMD64_XMM8();
   ru->regs[ru->size++] = hregAMD64_XMM9();
   ru->regs[ru->size++] = hregAMD64_XMM10();
   ru->regs[ru->size++] = hregAMD64_XMM11();
   ru->regs[ru->size++] = hregAMD64_XMM12();
   ru->regs[ru->size++] = hregAMD64_R10();
   ru->allocable = ru->size;
   /* And other regs, not available to the allocator. */
   ru->regs[ru->size++] = hregAMD64_RAX();
   ru->regs[ru->size++] = hregAMD64_RCX();
   ru->regs[ru->size++] = hregAMD64_RDX();
   ru->regs[ru->size++] = hregAMD64_RSP();
   ru->regs[ru->size++] = hregAMD64_RBP();
   ru->regs[ru->size++] = hregAMD64_R11();
   ru->regs[ru->size++] = hregAMD64_XMM0();
   ru->regs[ru->size++] = hregAMD64_XMM1();

   rRegUniverse_AMD64_initted = True;

   RRegUniverse__check_is_sane(ru);
   return ru;
}


void ppHRegAMD64 ( HReg reg ) 
{
   Int r;
   static const HChar* ireg64_names[16] 
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
         r = hregEncoding(reg);
         vassert(r >= 0 && r < 16);
         vex_printf("%s", ireg64_names[r]);
         return;
      case HRcVec128:
         r = hregEncoding(reg);
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
   static const HChar* ireg32_names[16] 
     = { "%eax", "%ecx", "%edx",  "%ebx",  "%esp",  "%ebp",  "%esi",  "%edi",
         "%r8d", "%r9d", "%r10d", "%r11d", "%r12d", "%r13d", "%r14d", "%r15d" };
   /* Be generic for all virtual regs. */
   if (hregIsVirtual(reg)) {
      ppHReg(reg);
      vex_printf("d");
      return;
   }
   /* But specific for real regs. */
   switch (hregClass(reg)) {
      case HRcInt64:
         r = hregEncoding(reg);
         vassert(r >= 0 && r < 16);
         vex_printf("%s", ireg32_names[r]);
         return;
      default:
         vpanic("ppHRegAMD64_lo32: invalid regclass");
   }
}


/* --------- Condition codes, Intel encoding. --------- */

const HChar* showAMD64CondCode ( AMD64CondCode cond )
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
   AMD64AMode* am = LibVEX_Alloc_inline(sizeof(AMD64AMode));
   am->tag        = Aam_IR;
   am->Aam.IR.imm = imm32;
   am->Aam.IR.reg = reg;
   return am;
}
AMD64AMode* AMD64AMode_IRRS ( UInt imm32, HReg base, HReg indEx, Int shift ) {
   AMD64AMode* am = LibVEX_Alloc_inline(sizeof(AMD64AMode));
   am->tag = Aam_IRRS;
   am->Aam.IRRS.imm   = imm32;
   am->Aam.IRRS.base  = base;
   am->Aam.IRRS.index = indEx;
   am->Aam.IRRS.shift = shift;
   vassert(shift >= 0 && shift <= 3);
   return am;
}

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
   AMD64RMI* op       = LibVEX_Alloc_inline(sizeof(AMD64RMI));
   op->tag            = Armi_Imm;
   op->Armi.Imm.imm32 = imm32;
   return op;
}
AMD64RMI* AMD64RMI_Reg ( HReg reg ) {
   AMD64RMI* op     = LibVEX_Alloc_inline(sizeof(AMD64RMI));
   op->tag          = Armi_Reg;
   op->Armi.Reg.reg = reg;
   return op;
}
AMD64RMI* AMD64RMI_Mem ( AMD64AMode* am ) {
   AMD64RMI* op    = LibVEX_Alloc_inline(sizeof(AMD64RMI));
   op->tag         = Armi_Mem;
   op->Armi.Mem.am = am;
   return op;
}

static void ppAMD64RMI_wrk ( AMD64RMI* op, Bool lo32 ) {
   switch (op->tag) {
      case Armi_Imm: 
         vex_printf("$0x%x", op->Armi.Imm.imm32);
         return;
      case Armi_Reg:
         if (lo32)
            ppHRegAMD64_lo32(op->Armi.Reg.reg);
         else
            ppHRegAMD64(op->Armi.Reg.reg);
         return;
      case Armi_Mem: 
         ppAMD64AMode(op->Armi.Mem.am);
         return;
     default: 
         vpanic("ppAMD64RMI");
   }
}
void ppAMD64RMI ( AMD64RMI* op ) {
   ppAMD64RMI_wrk(op, False/*!lo32*/);
}
void ppAMD64RMI_lo32 ( AMD64RMI* op ) {
   ppAMD64RMI_wrk(op, True/*lo32*/);
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
   AMD64RI* op       = LibVEX_Alloc_inline(sizeof(AMD64RI));
   op->tag           = Ari_Imm;
   op->Ari.Imm.imm32 = imm32;
   return op;
}
AMD64RI* AMD64RI_Reg ( HReg reg ) {
   AMD64RI* op     = LibVEX_Alloc_inline(sizeof(AMD64RI));
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
   AMD64RM* op       = LibVEX_Alloc_inline(sizeof(AMD64RM));
   op->tag         = Arm_Reg;
   op->Arm.Reg.reg = reg;
   return op;
}
AMD64RM* AMD64RM_Mem ( AMD64AMode* am ) {
   AMD64RM* op    = LibVEX_Alloc_inline(sizeof(AMD64RM));
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

static const HChar* showAMD64ScalarSz ( Int sz ) {
   switch (sz) {
      case 2: return "w";
      case 4: return "l";
      case 8: return "q";
      default: vpanic("showAMD64ScalarSz");
   }
}
 
const HChar* showAMD64UnaryOp ( AMD64UnaryOp op ) {
   switch (op) {
      case Aun_NOT: return "not";
      case Aun_NEG: return "neg";
      default: vpanic("showAMD64UnaryOp");
   }
}

const HChar* showAMD64AluOp ( AMD64AluOp op ) {
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

const HChar* showAMD64ShiftOp ( AMD64ShiftOp op ) {
   switch (op) {
      case Ash_SHL: return "shl";
      case Ash_SHR: return "shr";
      case Ash_SAR: return "sar";
      default: vpanic("showAMD64ShiftOp");
   }
}

const HChar* showA87FpOp ( A87FpOp op ) {
   switch (op) {
      case Afp_SCALE:  return "scale";
      case Afp_ATAN:   return "atan";
      case Afp_YL2X:   return "yl2x";
      case Afp_YL2XP1: return "yl2xp1";
      case Afp_PREM:   return "prem";
      case Afp_PREM1:  return "prem1";
      case Afp_SQRT:   return "sqrt";
      case Afp_SIN:    return "sin";
      case Afp_COS:    return "cos";
      case Afp_TAN:    return "tan";
      case Afp_ROUND:  return "round";
      case Afp_2XM1:   return "2xm1";
      default: vpanic("showA87FpOp");
   }
}

const HChar* showAMD64SseOp ( AMD64SseOp op ) {
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
   AMD64Instr* i      = LibVEX_Alloc_inline(sizeof(AMD64Instr));
   i->tag             = Ain_Imm64;
   i->Ain.Imm64.imm64 = imm64;
   i->Ain.Imm64.dst   = dst;
   return i;
}
AMD64Instr* AMD64Instr_Alu64R ( AMD64AluOp op, AMD64RMI* src, HReg dst ) {
   AMD64Instr* i     = LibVEX_Alloc_inline(sizeof(AMD64Instr));
   i->tag            = Ain_Alu64R;
   i->Ain.Alu64R.op  = op;
   i->Ain.Alu64R.src = src;
   i->Ain.Alu64R.dst = dst;
   return i;
}
AMD64Instr* AMD64Instr_Alu64M ( AMD64AluOp op, AMD64RI* src, AMD64AMode* dst ) {
   AMD64Instr* i     = LibVEX_Alloc_inline(sizeof(AMD64Instr));
   i->tag            = Ain_Alu64M;
   i->Ain.Alu64M.op  = op;
   i->Ain.Alu64M.src = src;
   i->Ain.Alu64M.dst = dst;
   vassert(op != Aalu_MUL);
   return i;
}
AMD64Instr* AMD64Instr_Sh64 ( AMD64ShiftOp op, UInt src, HReg dst ) {
   AMD64Instr* i   = LibVEX_Alloc_inline(sizeof(AMD64Instr));
   i->tag          = Ain_Sh64;
   i->Ain.Sh64.op  = op;
   i->Ain.Sh64.src = src;
   i->Ain.Sh64.dst = dst;
   return i;
}
AMD64Instr* AMD64Instr_Test64 ( UInt imm32, HReg dst ) {
   AMD64Instr* i       = LibVEX_Alloc_inline(sizeof(AMD64Instr));
   i->tag              = Ain_Test64;
   i->Ain.Test64.imm32 = imm32;
   i->Ain.Test64.dst   = dst;
   return i;
}
AMD64Instr* AMD64Instr_Unary64 ( AMD64UnaryOp op, HReg dst ) {
   AMD64Instr* i      = LibVEX_Alloc_inline(sizeof(AMD64Instr));
   i->tag             = Ain_Unary64;
   i->Ain.Unary64.op  = op;
   i->Ain.Unary64.dst = dst;
   return i;
}
AMD64Instr* AMD64Instr_Lea64 ( AMD64AMode* am, HReg dst ) {
   AMD64Instr* i      = LibVEX_Alloc_inline(sizeof(AMD64Instr));
   i->tag             = Ain_Lea64;
   i->Ain.Lea64.am    = am;
   i->Ain.Lea64.dst   = dst;
   return i;
}
AMD64Instr* AMD64Instr_Alu32R ( AMD64AluOp op, AMD64RMI* src, HReg dst ) {
   AMD64Instr* i     = LibVEX_Alloc_inline(sizeof(AMD64Instr));
   i->tag            = Ain_Alu32R;
   i->Ain.Alu32R.op  = op;
   i->Ain.Alu32R.src = src;
   i->Ain.Alu32R.dst = dst;
   switch (op) {
      case Aalu_ADD: case Aalu_SUB: case Aalu_CMP:
      case Aalu_AND: case Aalu_OR:  case Aalu_XOR: break;
      default: vassert(0);
   }
   return i;
}
AMD64Instr* AMD64Instr_MulL ( Bool syned, AMD64RM* src ) {
   AMD64Instr* i     = LibVEX_Alloc_inline(sizeof(AMD64Instr));
   i->tag            = Ain_MulL;
   i->Ain.MulL.syned = syned;
   i->Ain.MulL.src   = src;
   return i;
}
AMD64Instr* AMD64Instr_Div ( Bool syned, Int sz, AMD64RM* src ) {
   AMD64Instr* i     = LibVEX_Alloc_inline(sizeof(AMD64Instr));
   i->tag            = Ain_Div;
   i->Ain.Div.syned  = syned;
   i->Ain.Div.sz     = sz;
   i->Ain.Div.src    = src;
   vassert(sz == 4 || sz == 8);
   return i;
}
AMD64Instr* AMD64Instr_Push( AMD64RMI* src ) {
   AMD64Instr* i   = LibVEX_Alloc_inline(sizeof(AMD64Instr));
   i->tag          = Ain_Push;
   i->Ain.Push.src = src;
   return i;
}
AMD64Instr* AMD64Instr_Call ( AMD64CondCode cond, Addr64 target, Int regparms,
                              RetLoc rloc ) {
   AMD64Instr* i        = LibVEX_Alloc_inline(sizeof(AMD64Instr));
   i->tag               = Ain_Call;
   i->Ain.Call.cond     = cond;
   i->Ain.Call.target   = target;
   i->Ain.Call.regparms = regparms;
   i->Ain.Call.rloc     = rloc;
   vassert(regparms >= 0 && regparms <= 6);
   vassert(is_sane_RetLoc(rloc));
   return i;
}

AMD64Instr* AMD64Instr_XDirect ( Addr64 dstGA, AMD64AMode* amRIP,
                                 AMD64CondCode cond, Bool toFastEP ) {
   AMD64Instr* i           = LibVEX_Alloc_inline(sizeof(AMD64Instr));
   i->tag                  = Ain_XDirect;
   i->Ain.XDirect.dstGA    = dstGA;
   i->Ain.XDirect.amRIP    = amRIP;
   i->Ain.XDirect.cond     = cond;
   i->Ain.XDirect.toFastEP = toFastEP;
   return i;
}
AMD64Instr* AMD64Instr_XIndir ( HReg dstGA, AMD64AMode* amRIP,
                                AMD64CondCode cond ) {
   AMD64Instr* i       = LibVEX_Alloc_inline(sizeof(AMD64Instr));
   i->tag              = Ain_XIndir;
   i->Ain.XIndir.dstGA = dstGA;
   i->Ain.XIndir.amRIP = amRIP;
   i->Ain.XIndir.cond  = cond;
   return i;
}
AMD64Instr* AMD64Instr_XAssisted ( HReg dstGA, AMD64AMode* amRIP,
                                   AMD64CondCode cond, IRJumpKind jk ) {
   AMD64Instr* i          = LibVEX_Alloc_inline(sizeof(AMD64Instr));
   i->tag                 = Ain_XAssisted;
   i->Ain.XAssisted.dstGA = dstGA;
   i->Ain.XAssisted.amRIP = amRIP;
   i->Ain.XAssisted.cond  = cond;
   i->Ain.XAssisted.jk    = jk;
   return i;
}

AMD64Instr* AMD64Instr_CMov64 ( AMD64CondCode cond, HReg src, HReg dst ) {
   AMD64Instr* i      = LibVEX_Alloc_inline(sizeof(AMD64Instr));
   i->tag             = Ain_CMov64;
   i->Ain.CMov64.cond = cond;
   i->Ain.CMov64.src  = src;
   i->Ain.CMov64.dst  = dst;
   vassert(cond != Acc_ALWAYS);
   return i;
}
AMD64Instr* AMD64Instr_CLoad ( AMD64CondCode cond, UChar szB,
                               AMD64AMode* addr, HReg dst ) {
   AMD64Instr* i     = LibVEX_Alloc_inline(sizeof(AMD64Instr));
   i->tag            = Ain_CLoad;
   i->Ain.CLoad.cond = cond;
   i->Ain.CLoad.szB  = szB;
   i->Ain.CLoad.addr = addr;
   i->Ain.CLoad.dst  = dst;
   vassert(cond != Acc_ALWAYS && (szB == 4 || szB == 8));
   return i;
}
AMD64Instr* AMD64Instr_CStore ( AMD64CondCode cond, UChar szB,
                                HReg src, AMD64AMode* addr ) {
   AMD64Instr* i      = LibVEX_Alloc_inline(sizeof(AMD64Instr));
   i->tag             = Ain_CStore;
   i->Ain.CStore.cond = cond;
   i->Ain.CStore.szB  = szB;
   i->Ain.CStore.src  = src;
   i->Ain.CStore.addr = addr;
   vassert(cond != Acc_ALWAYS && (szB == 4 || szB == 8));
   return i;
}
AMD64Instr* AMD64Instr_MovxLQ ( Bool syned, HReg src, HReg dst ) {
   AMD64Instr* i       = LibVEX_Alloc_inline(sizeof(AMD64Instr));
   i->tag              = Ain_MovxLQ;
   i->Ain.MovxLQ.syned = syned;
   i->Ain.MovxLQ.src   = src;
   i->Ain.MovxLQ.dst   = dst;
   return i;
}
AMD64Instr* AMD64Instr_LoadEX ( UChar szSmall, Bool syned,
                                AMD64AMode* src, HReg dst ) {
   AMD64Instr* i         = LibVEX_Alloc_inline(sizeof(AMD64Instr));
   i->tag                = Ain_LoadEX;
   i->Ain.LoadEX.szSmall = szSmall;
   i->Ain.LoadEX.syned   = syned;
   i->Ain.LoadEX.src     = src;
   i->Ain.LoadEX.dst     = dst;
   vassert(szSmall == 1 || szSmall == 2 || szSmall == 4);
   return i;
}
AMD64Instr* AMD64Instr_Store ( UChar sz, HReg src, AMD64AMode* dst ) {
   AMD64Instr* i    = LibVEX_Alloc_inline(sizeof(AMD64Instr));
   i->tag           = Ain_Store;
   i->Ain.Store.sz  = sz;
   i->Ain.Store.src = src;
   i->Ain.Store.dst = dst;
   vassert(sz == 1 || sz == 2 || sz == 4);
   return i;
}
AMD64Instr* AMD64Instr_Set64 ( AMD64CondCode cond, HReg dst ) {
   AMD64Instr* i     = LibVEX_Alloc_inline(sizeof(AMD64Instr));
   i->tag            = Ain_Set64;
   i->Ain.Set64.cond = cond;
   i->Ain.Set64.dst  = dst;
   return i;
}
AMD64Instr* AMD64Instr_Bsfr64 ( Bool isFwds, HReg src, HReg dst ) {
   AMD64Instr* i        = LibVEX_Alloc_inline(sizeof(AMD64Instr));
   i->tag               = Ain_Bsfr64;
   i->Ain.Bsfr64.isFwds = isFwds;
   i->Ain.Bsfr64.src    = src;
   i->Ain.Bsfr64.dst    = dst;
   return i;
}
AMD64Instr* AMD64Instr_MFence ( void ) {
   AMD64Instr* i = LibVEX_Alloc_inline(sizeof(AMD64Instr));
   i->tag        = Ain_MFence;
   return i;
}
AMD64Instr* AMD64Instr_ACAS ( AMD64AMode* addr, UChar sz ) {
   AMD64Instr* i    = LibVEX_Alloc_inline(sizeof(AMD64Instr));
   i->tag           = Ain_ACAS;
   i->Ain.ACAS.addr = addr;
   i->Ain.ACAS.sz   = sz;
   vassert(sz == 8 || sz == 4 || sz == 2 || sz == 1);
   return i;
}
AMD64Instr* AMD64Instr_DACAS ( AMD64AMode* addr, UChar sz ) {
   AMD64Instr* i     = LibVEX_Alloc_inline(sizeof(AMD64Instr));
   i->tag            = Ain_DACAS;
   i->Ain.DACAS.addr = addr;
   i->Ain.DACAS.sz   = sz;
   vassert(sz == 8 || sz == 4);
   return i;
}

AMD64Instr* AMD64Instr_A87Free ( Int nregs )
{
   AMD64Instr* i        = LibVEX_Alloc_inline(sizeof(AMD64Instr));
   i->tag               = Ain_A87Free;
   i->Ain.A87Free.nregs = nregs;
   vassert(nregs >= 1 && nregs <= 7);
   return i;
}
AMD64Instr* AMD64Instr_A87PushPop ( AMD64AMode* addr, Bool isPush, UChar szB )
{
   AMD64Instr* i            = LibVEX_Alloc_inline(sizeof(AMD64Instr));
   i->tag                   = Ain_A87PushPop;
   i->Ain.A87PushPop.addr   = addr;
   i->Ain.A87PushPop.isPush = isPush;
   i->Ain.A87PushPop.szB    = szB;
   vassert(szB == 8 || szB == 4);
   return i;
}
AMD64Instr* AMD64Instr_A87FpOp ( A87FpOp op )
{
   AMD64Instr* i     = LibVEX_Alloc_inline(sizeof(AMD64Instr));
   i->tag            = Ain_A87FpOp;
   i->Ain.A87FpOp.op = op;
   return i;
}
AMD64Instr* AMD64Instr_A87LdCW ( AMD64AMode* addr )
{
   AMD64Instr* i       = LibVEX_Alloc_inline(sizeof(AMD64Instr));
   i->tag              = Ain_A87LdCW;
   i->Ain.A87LdCW.addr = addr;
   return i;
}
AMD64Instr* AMD64Instr_A87StSW ( AMD64AMode* addr )
{
   AMD64Instr* i       = LibVEX_Alloc_inline(sizeof(AMD64Instr));
   i->tag              = Ain_A87StSW;
   i->Ain.A87StSW.addr = addr;
   return i;
}
AMD64Instr* AMD64Instr_LdMXCSR ( AMD64AMode* addr ) {
   AMD64Instr* i         = LibVEX_Alloc_inline(sizeof(AMD64Instr));
   i->tag                = Ain_LdMXCSR;
   i->Ain.LdMXCSR.addr   = addr;
   return i;
}
AMD64Instr* AMD64Instr_SseUComIS ( Int sz, HReg srcL, HReg srcR, HReg dst ) {
   AMD64Instr* i         = LibVEX_Alloc_inline(sizeof(AMD64Instr));
   i->tag                = Ain_SseUComIS;
   i->Ain.SseUComIS.sz   = toUChar(sz);
   i->Ain.SseUComIS.srcL = srcL;
   i->Ain.SseUComIS.srcR = srcR;
   i->Ain.SseUComIS.dst  = dst;
   vassert(sz == 4 || sz == 8);
   return i;
}
AMD64Instr* AMD64Instr_SseSI2SF ( Int szS, Int szD, HReg src, HReg dst ) {
   AMD64Instr* i       = LibVEX_Alloc_inline(sizeof(AMD64Instr));
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
   AMD64Instr* i       = LibVEX_Alloc_inline(sizeof(AMD64Instr));
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
   AMD64Instr* i         = LibVEX_Alloc_inline(sizeof(AMD64Instr));
   i->tag                = Ain_SseSDSS;
   i->Ain.SseSDSS.from64 = from64;
   i->Ain.SseSDSS.src    = src;
   i->Ain.SseSDSS.dst    = dst;
   return i;
}
AMD64Instr* AMD64Instr_SseLdSt ( Bool isLoad, Int sz, 
                                 HReg reg, AMD64AMode* addr ) {
   AMD64Instr* i         = LibVEX_Alloc_inline(sizeof(AMD64Instr));
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
   AMD64Instr* i         = LibVEX_Alloc_inline(sizeof(AMD64Instr));
   i->tag                = Ain_SseLdzLO;
   i->Ain.SseLdzLO.sz    = sz;
   i->Ain.SseLdzLO.reg   = reg;
   i->Ain.SseLdzLO.addr  = addr;
   vassert(sz == 4 || sz == 8);
   return i;
}
AMD64Instr* AMD64Instr_Sse32Fx4 ( AMD64SseOp op, HReg src, HReg dst ) {
   AMD64Instr* i       = LibVEX_Alloc_inline(sizeof(AMD64Instr));
   i->tag              = Ain_Sse32Fx4;
   i->Ain.Sse32Fx4.op  = op;
   i->Ain.Sse32Fx4.src = src;
   i->Ain.Sse32Fx4.dst = dst;
   vassert(op != Asse_MOV);
   return i;
}
AMD64Instr* AMD64Instr_Sse32FLo ( AMD64SseOp op, HReg src, HReg dst ) {
   AMD64Instr* i       = LibVEX_Alloc_inline(sizeof(AMD64Instr));
   i->tag              = Ain_Sse32FLo;
   i->Ain.Sse32FLo.op  = op;
   i->Ain.Sse32FLo.src = src;
   i->Ain.Sse32FLo.dst = dst;
   vassert(op != Asse_MOV);
   return i;
}
AMD64Instr* AMD64Instr_Sse64Fx2 ( AMD64SseOp op, HReg src, HReg dst ) {
   AMD64Instr* i       = LibVEX_Alloc_inline(sizeof(AMD64Instr));
   i->tag              = Ain_Sse64Fx2;
   i->Ain.Sse64Fx2.op  = op;
   i->Ain.Sse64Fx2.src = src;
   i->Ain.Sse64Fx2.dst = dst;
   vassert(op != Asse_MOV);
   return i;
}
AMD64Instr* AMD64Instr_Sse64FLo ( AMD64SseOp op, HReg src, HReg dst ) {
   AMD64Instr* i       = LibVEX_Alloc_inline(sizeof(AMD64Instr));
   i->tag              = Ain_Sse64FLo;
   i->Ain.Sse64FLo.op  = op;
   i->Ain.Sse64FLo.src = src;
   i->Ain.Sse64FLo.dst = dst;
   vassert(op != Asse_MOV);
   return i;
}
AMD64Instr* AMD64Instr_SseReRg ( AMD64SseOp op, HReg re, HReg rg ) {
   AMD64Instr* i      = LibVEX_Alloc_inline(sizeof(AMD64Instr));
   i->tag             = Ain_SseReRg;
   i->Ain.SseReRg.op  = op;
   i->Ain.SseReRg.src = re;
   i->Ain.SseReRg.dst = rg;
   return i;
}
AMD64Instr* AMD64Instr_SseCMov ( AMD64CondCode cond, HReg src, HReg dst ) {
   AMD64Instr* i       = LibVEX_Alloc_inline(sizeof(AMD64Instr));
   i->tag              = Ain_SseCMov;
   i->Ain.SseCMov.cond = cond;
   i->Ain.SseCMov.src  = src;
   i->Ain.SseCMov.dst  = dst;
   vassert(cond != Acc_ALWAYS);
   return i;
}
AMD64Instr* AMD64Instr_SseShuf ( Int order, HReg src, HReg dst ) {
   AMD64Instr* i        = LibVEX_Alloc_inline(sizeof(AMD64Instr));
   i->tag               = Ain_SseShuf;
   i->Ain.SseShuf.order = order;
   i->Ain.SseShuf.src   = src;
   i->Ain.SseShuf.dst   = dst;
   vassert(order >= 0 && order <= 0xFF);
   return i;
}
//uu AMD64Instr* AMD64Instr_AvxLdSt ( Bool isLoad,
//uu                                  HReg reg, AMD64AMode* addr ) {
//uu    AMD64Instr* i         = LibVEX_Alloc_inline(sizeof(AMD64Instr));
//uu    i->tag                = Ain_AvxLdSt;
//uu    i->Ain.AvxLdSt.isLoad = isLoad;
//uu    i->Ain.AvxLdSt.reg    = reg;
//uu    i->Ain.AvxLdSt.addr   = addr;
//uu    return i;
//uu }
//uu AMD64Instr* AMD64Instr_AvxReRg ( AMD64SseOp op, HReg re, HReg rg ) {
//uu    AMD64Instr* i      = LibVEX_Alloc_inline(sizeof(AMD64Instr));
//uu    i->tag             = Ain_AvxReRg;
//uu    i->Ain.AvxReRg.op  = op;
//uu    i->Ain.AvxReRg.src = re;
//uu    i->Ain.AvxReRg.dst = rg;
//uu    return i;
//uu }
AMD64Instr* AMD64Instr_EvCheck ( AMD64AMode* amCounter,
                                 AMD64AMode* amFailAddr ) {
   AMD64Instr* i             = LibVEX_Alloc_inline(sizeof(AMD64Instr));
   i->tag                    = Ain_EvCheck;
   i->Ain.EvCheck.amCounter  = amCounter;
   i->Ain.EvCheck.amFailAddr = amFailAddr;
   return i;
}
AMD64Instr* AMD64Instr_ProfInc ( void ) {
   AMD64Instr* i = LibVEX_Alloc_inline(sizeof(AMD64Instr));
   i->tag        = Ain_ProfInc;
   return i;
}

void ppAMD64Instr ( const AMD64Instr* i, Bool mode64 ) 
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
      case Ain_Alu32R:
         vex_printf("%sl ", showAMD64AluOp(i->Ain.Alu32R.op));
         ppAMD64RMI_lo32(i->Ain.Alu32R.src);
         vex_printf(",");
         ppHRegAMD64_lo32(i->Ain.Alu32R.dst);
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
      case Ain_Push:
         vex_printf("pushq ");
         ppAMD64RMI(i->Ain.Push.src);
         return;
      case Ain_Call:
         vex_printf("call%s[%d,", 
                    i->Ain.Call.cond==Acc_ALWAYS 
                       ? "" : showAMD64CondCode(i->Ain.Call.cond),
                    i->Ain.Call.regparms );
         ppRetLoc(i->Ain.Call.rloc);
         vex_printf("] 0x%llx", i->Ain.Call.target);
         break;

      case Ain_XDirect:
         vex_printf("(xDirect) ");
         vex_printf("if (%%rflags.%s) { ",
                    showAMD64CondCode(i->Ain.XDirect.cond));
         vex_printf("movabsq $0x%llx,%%r11; ", i->Ain.XDirect.dstGA);
         vex_printf("movq %%r11,");
         ppAMD64AMode(i->Ain.XDirect.amRIP);
         vex_printf("; ");
         vex_printf("movabsq $disp_cp_chain_me_to_%sEP,%%r11; call *%%r11 }",
                    i->Ain.XDirect.toFastEP ? "fast" : "slow");
         return;
      case Ain_XIndir:
         vex_printf("(xIndir) ");
         vex_printf("if (%%rflags.%s) { ",
                    showAMD64CondCode(i->Ain.XIndir.cond));
         vex_printf("movq ");
         ppHRegAMD64(i->Ain.XIndir.dstGA);
         vex_printf(",");
         ppAMD64AMode(i->Ain.XIndir.amRIP);
         vex_printf("; movabsq $disp_indir,%%r11; jmp *%%r11 }");
         return;
      case Ain_XAssisted:
         vex_printf("(xAssisted) ");
         vex_printf("if (%%rflags.%s) { ",
                    showAMD64CondCode(i->Ain.XAssisted.cond));
         vex_printf("movq ");
         ppHRegAMD64(i->Ain.XAssisted.dstGA);
         vex_printf(",");
         ppAMD64AMode(i->Ain.XAssisted.amRIP);
         vex_printf("; movl $IRJumpKind_to_TRCVAL(%d),%%rbp",
                    (Int)i->Ain.XAssisted.jk);
         vex_printf("; movabsq $disp_assisted,%%r11; jmp *%%r11 }");
         return;

      case Ain_CMov64:
         vex_printf("cmov%s ", showAMD64CondCode(i->Ain.CMov64.cond));
         ppHRegAMD64(i->Ain.CMov64.src);
         vex_printf(",");
         ppHRegAMD64(i->Ain.CMov64.dst);
         return;
      case Ain_CLoad:
         vex_printf("if (%%rflags.%s) { ",
                    showAMD64CondCode(i->Ain.CLoad.cond));
         vex_printf("mov%c ", i->Ain.CLoad.szB == 4 ? 'l' : 'q');
         ppAMD64AMode(i->Ain.CLoad.addr);
         vex_printf(", ");
         (i->Ain.CLoad.szB == 4 ? ppHRegAMD64_lo32 : ppHRegAMD64)
            (i->Ain.CLoad.dst);
         vex_printf(" }");
         return;
      case Ain_CStore:
         vex_printf("if (%%rflags.%s) { ",
                    showAMD64CondCode(i->Ain.CStore.cond));
         vex_printf("mov%c ", i->Ain.CStore.szB == 4 ? 'l' : 'q');
         (i->Ain.CStore.szB == 4 ? ppHRegAMD64_lo32 : ppHRegAMD64)
            (i->Ain.CStore.src);
         vex_printf(", ");
         ppAMD64AMode(i->Ain.CStore.addr);
         vex_printf(" }");
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
      //uu case Ain_AvxLdSt:
      //uu    vex_printf("vmovups ");
      //uu    if (i->Ain.AvxLdSt.isLoad) {
      //uu       ppAMD64AMode(i->Ain.AvxLdSt.addr);
      //uu       vex_printf(",");
      //uu       ppHRegAMD64(i->Ain.AvxLdSt.reg);
      //uu    } else {
      //uu       ppHRegAMD64(i->Ain.AvxLdSt.reg);
      //uu       vex_printf(",");
      //uu       ppAMD64AMode(i->Ain.AvxLdSt.addr);
      //uu    }
      //uu    return;
      //uu case Ain_AvxReRg:
      //uu    vex_printf("v%s ", showAMD64SseOp(i->Ain.SseReRg.op));
      //uu    ppHRegAMD64(i->Ain.AvxReRg.src);
      //uu    vex_printf(",");
      //uu    ppHRegAMD64(i->Ain.AvxReRg.dst);
      //uu    return;
      case Ain_EvCheck:
         vex_printf("(evCheck) decl ");
         ppAMD64AMode(i->Ain.EvCheck.amCounter);
         vex_printf("; jns nofail; jmp *");
         ppAMD64AMode(i->Ain.EvCheck.amFailAddr);
         vex_printf("; nofail:");
         return;
      case Ain_ProfInc:
         vex_printf("(profInc) movabsq $NotKnownYet, %%r11; incq (%%r11)");
         return;
      default:
         vpanic("ppAMD64Instr");
   }
}

/* --------- Helpers for register allocation. --------- */

void getRegUsage_AMD64Instr ( HRegUsage* u, const AMD64Instr* i, Bool mode64 )
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
      case Ain_Alu32R:
         vassert(i->Ain.Alu32R.op != Aalu_MOV);
         addRegUsage_AMD64RMI(u, i->Ain.Alu32R.src);
         if (i->Ain.Alu32R.op == Aalu_CMP) { 
            addHRegUse(u, HRmRead, i->Ain.Alu32R.dst);
            return;
         }
         addHRegUse(u, HRmModify, i->Ain.Alu32R.dst);
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
      /* XDirect/XIndir/XAssisted are also a bit subtle.  They
         conditionally exit the block.  Hence we only need to list (1)
         the registers that they read, and (2) the registers that they
         write in the case where the block is not exited.  (2) is
         empty, hence only (1) is relevant here. */
      case Ain_XDirect:
         /* Don't bother to mention the write to %r11, since it is not
            available to the allocator. */
         addRegUsage_AMD64AMode(u, i->Ain.XDirect.amRIP);
         return;
      case Ain_XIndir:
         /* Ditto re %r11 */
         addHRegUse(u, HRmRead, i->Ain.XIndir.dstGA);
         addRegUsage_AMD64AMode(u, i->Ain.XIndir.amRIP);
         return;
      case Ain_XAssisted:
         /* Ditto re %r11 and %rbp (the baseblock ptr) */
         addHRegUse(u, HRmRead, i->Ain.XAssisted.dstGA);
         addRegUsage_AMD64AMode(u, i->Ain.XAssisted.amRIP);
         return;
      case Ain_CMov64:
         addHRegUse(u, HRmRead,   i->Ain.CMov64.src);
         addHRegUse(u, HRmModify, i->Ain.CMov64.dst);
         return;
      case Ain_CLoad:
         addRegUsage_AMD64AMode(u, i->Ain.CLoad.addr);
         addHRegUse(u, HRmModify, i->Ain.CLoad.dst);
         return;
      case Ain_CStore:
         addRegUsage_AMD64AMode(u, i->Ain.CStore.addr);
         addHRegUse(u, HRmRead, i->Ain.CStore.src);
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
      case Ain_LdMXCSR:
         addRegUsage_AMD64AMode(u, i->Ain.LdMXCSR.addr);
         return;
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
              && sameHReg(i->Ain.SseReRg.src, i->Ain.SseReRg.dst)) {
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
      //uu case Ain_AvxLdSt:
      //uu addRegUsage_AMD64AMode(u, i->Ain.AvxLdSt.addr);
      //uu addHRegUse(u, i->Ain.AvxLdSt.isLoad ? HRmWrite : HRmRead,
      //uu               i->Ain.AvxLdSt.reg);
      //uu return;
      //uu case Ain_AvxReRg:
      //uu    if ( (i->Ain.AvxReRg.op == Asse_XOR
      //uu          || i->Ain.AvxReRg.op == Asse_CMPEQ32)
      //uu         && i->Ain.AvxReRg.src == i->Ain.AvxReRg.dst) {
      //uu       /* See comments on the case for Ain_SseReRg. */
      //uu       addHRegUse(u, HRmWrite, i->Ain.AvxReRg.dst);
      //uu    } else {
      //uu       addHRegUse(u, HRmRead, i->Ain.AvxReRg.src);
      //uu       addHRegUse(u, i->Ain.AvxReRg.op == Asse_MOV 
      //uu                        ? HRmWrite : HRmModify, 
      //uu                     i->Ain.AvxReRg.dst);
      //uu    }
      //uu    return;
      case Ain_EvCheck:
         /* We expect both amodes only to mention %rbp, so this is in
            fact pointless, since %rbp isn't allocatable, but anyway.. */
         addRegUsage_AMD64AMode(u, i->Ain.EvCheck.amCounter);
         addRegUsage_AMD64AMode(u, i->Ain.EvCheck.amFailAddr);
         return;
      case Ain_ProfInc:
         addHRegUse(u, HRmWrite, hregAMD64_R11());
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
      case Ain_Alu32R:
         mapRegs_AMD64RMI(m, i->Ain.Alu32R.src);
         mapReg(m, &i->Ain.Alu32R.dst);
         return;
      case Ain_MulL:
         mapRegs_AMD64RM(m, i->Ain.MulL.src);
         return;
      case Ain_Div:
         mapRegs_AMD64RM(m, i->Ain.Div.src);
         return;
      case Ain_Push:
         mapRegs_AMD64RMI(m, i->Ain.Push.src);
         return;
      case Ain_Call:
         return;
      case Ain_XDirect:
         mapRegs_AMD64AMode(m, i->Ain.XDirect.amRIP);
         return;
      case Ain_XIndir:
         mapReg(m, &i->Ain.XIndir.dstGA);
         mapRegs_AMD64AMode(m, i->Ain.XIndir.amRIP);
         return;
      case Ain_XAssisted:
         mapReg(m, &i->Ain.XAssisted.dstGA);
         mapRegs_AMD64AMode(m, i->Ain.XAssisted.amRIP);
         return;
      case Ain_CMov64:
         mapReg(m, &i->Ain.CMov64.src);
         mapReg(m, &i->Ain.CMov64.dst);
         return;
      case Ain_CLoad:
         mapRegs_AMD64AMode(m, i->Ain.CLoad.addr);
         mapReg(m, &i->Ain.CLoad.dst);
         return;
      case Ain_CStore:
         mapRegs_AMD64AMode(m, i->Ain.CStore.addr);
         mapReg(m, &i->Ain.CStore.src);
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
      case Ain_LdMXCSR:
         mapRegs_AMD64AMode(m, i->Ain.LdMXCSR.addr);
         return;
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
      //uu case Ain_AvxLdSt:
      //uu    mapReg(m, &i->Ain.AvxLdSt.reg);
      //uu    mapRegs_AMD64AMode(m, i->Ain.AvxLdSt.addr);
      //uu    break;
      //uu case Ain_AvxReRg:
      //uu    mapReg(m, &i->Ain.AvxReRg.src);
      //uu    mapReg(m, &i->Ain.AvxReRg.dst);
      //uu    return;
      case Ain_EvCheck:
         /* We expect both amodes only to mention %rbp, so this is in
            fact pointless, since %rbp isn't allocatable, but anyway.. */
         mapRegs_AMD64AMode(m, i->Ain.EvCheck.amCounter);
         mapRegs_AMD64AMode(m, i->Ain.EvCheck.amFailAddr);
         return;
      case Ain_ProfInc:
         /* hardwires r11 -- nothing to modify. */
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
Bool isMove_AMD64Instr ( const AMD64Instr* i, HReg* src, HReg* dst )
{
   switch (i->tag) {
      case Ain_Alu64R:
         /* Moves between integer regs */
         if (i->Ain.Alu64R.op != Aalu_MOV)
            return False;
         if (i->Ain.Alu64R.src->tag != Armi_Reg)
            return False;
         *src = i->Ain.Alu64R.src->Armi.Reg.reg;
         *dst = i->Ain.Alu64R.dst;
         return True;
      case Ain_SseReRg:
         /* Moves between SSE regs */
         if (i->Ain.SseReRg.op != Asse_MOV)
            return False;
         *src = i->Ain.SseReRg.src;
         *dst = i->Ain.SseReRg.dst;
         return True;
      //uu case Ain_AvxReRg:
      //uu    /* Moves between AVX regs */
      //uu    if (i->Ain.AvxReRg.op != Asse_MOV)
      //uu       return False;
      //uu    *src = i->Ain.AvxReRg.src;
      //uu    *dst = i->Ain.AvxReRg.dst;
      //uu    return True;
      default:
         return False;
   }
   /*NOTREACHED*/
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
inline static UInt iregEnc210 ( HReg r )
{
   UInt n;
   vassert(hregClass(r) == HRcInt64);
   vassert(!hregIsVirtual(r));
   n = hregEncoding(r);
   vassert(n <= 15);
   return n & 7;
}

/* Produce bit 3 of an integer register number. */
inline static UInt iregEnc3 ( HReg r )
{
   UInt n;
   vassert(hregClass(r) == HRcInt64);
   vassert(!hregIsVirtual(r));
   n = hregEncoding(r);
   vassert(n <= 15);
   return (n >> 3) & 1;
}

/* Produce a complete 4-bit integer register number. */
inline static UInt iregEnc3210 ( HReg r )
{
   UInt n;
   vassert(hregClass(r) == HRcInt64);
   vassert(!hregIsVirtual(r));
   n = hregEncoding(r);
   vassert(n <= 15);
   return n;
}

/* Produce a complete 4-bit integer register number. */
inline static UInt vregEnc3210 ( HReg r )
{
   UInt n;
   vassert(hregClass(r) == HRcVec128);
   vassert(!hregIsVirtual(r));
   n = hregEncoding(r);
   vassert(n <= 15);
   return n;
}

inline static UChar mkModRegRM ( UInt mod, UInt reg, UInt regmem )
{
   vassert(mod < 4);
   vassert((reg|regmem) < 8);
   return (UChar)( ((mod & 3) << 6) | ((reg & 7) << 3) | (regmem & 7) );
}

inline static UChar mkSIB ( UInt shift, UInt regindex, UInt regbase )
{
   vassert(shift < 4);
   vassert((regindex|regbase) < 8);
   return (UChar)( ((shift & 3) << 6) | ((regindex & 7) << 3) | (regbase & 7) );
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
   return toBool(i32 == ((Int)(w32 << 24) >> 24));
}
/* Can the lower 32 bits be signedly widened to produce the whole
   64-bit value?  In other words, are the top 33 bits either all 0 or
   all 1 ? */
static Bool fitsIn32Bits ( ULong x )
{
   Long y1;
   y1 = x << 32;
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
static UChar* doAMode_M__wrk ( UChar* p, UInt gregEnc3210, AMD64AMode* am ) 
{
   UInt gregEnc210 = gregEnc3210 & 7;
   if (am->tag == Aam_IR) {
      if (am->Aam.IR.imm == 0 
          && ! sameHReg(am->Aam.IR.reg, hregAMD64_RSP())
          && ! sameHReg(am->Aam.IR.reg, hregAMD64_RBP())
          && ! sameHReg(am->Aam.IR.reg, hregAMD64_R12())
          && ! sameHReg(am->Aam.IR.reg, hregAMD64_R13())
         ) {
         *p++ = mkModRegRM(0, gregEnc210, iregEnc210(am->Aam.IR.reg));
         return p;
      }
      if (fits8bits(am->Aam.IR.imm)
          && ! sameHReg(am->Aam.IR.reg, hregAMD64_RSP())
          && ! sameHReg(am->Aam.IR.reg, hregAMD64_R12())
         ) {
         *p++ = mkModRegRM(1, gregEnc210, iregEnc210(am->Aam.IR.reg));
         *p++ = toUChar(am->Aam.IR.imm & 0xFF);
         return p;
      }
      if (! sameHReg(am->Aam.IR.reg, hregAMD64_RSP())
          && ! sameHReg(am->Aam.IR.reg, hregAMD64_R12())
         ) {
         *p++ = mkModRegRM(2, gregEnc210, iregEnc210(am->Aam.IR.reg));
         p = emit32(p, am->Aam.IR.imm);
         return p;
      }
      if ((sameHReg(am->Aam.IR.reg, hregAMD64_RSP())
           || sameHReg(am->Aam.IR.reg, hregAMD64_R12()))
          && fits8bits(am->Aam.IR.imm)) {
 	 *p++ = mkModRegRM(1, gregEnc210, 4);
         *p++ = 0x24;
         *p++ = toUChar(am->Aam.IR.imm & 0xFF);
         return p;
      }
      if (/* (sameHReg(am->Aam.IR.reg, hregAMD64_RSP())
	      || wait for test case for RSP case */
          sameHReg(am->Aam.IR.reg, hregAMD64_R12())) {
 	 *p++ = mkModRegRM(2, gregEnc210, 4);
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
          && ! sameHReg(am->Aam.IRRS.index, hregAMD64_RSP())) {
         *p++ = mkModRegRM(1, gregEnc210, 4);
         *p++ = mkSIB(am->Aam.IRRS.shift, iregEnc210(am->Aam.IRRS.index),
                                          iregEnc210(am->Aam.IRRS.base));
         *p++ = toUChar(am->Aam.IRRS.imm & 0xFF);
         return p;
      }
      if (! sameHReg(am->Aam.IRRS.index, hregAMD64_RSP())) {
         *p++ = mkModRegRM(2, gregEnc210, 4);
         *p++ = mkSIB(am->Aam.IRRS.shift, iregEnc210(am->Aam.IRRS.index),
                                          iregEnc210(am->Aam.IRRS.base));
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

static UChar* doAMode_M ( UChar* p, HReg greg, AMD64AMode* am )
{
   return doAMode_M__wrk(p, iregEnc3210(greg), am);
}

static UChar* doAMode_M_enc ( UChar* p, UInt gregEnc3210, AMD64AMode* am )
{
   vassert(gregEnc3210 < 16);
   return doAMode_M__wrk(p, gregEnc3210, am);
}


/* Emit a mod-reg-rm byte when the rm bit denotes a reg. */
inline
static UChar* doAMode_R__wrk ( UChar* p, UInt gregEnc3210, UInt eregEnc3210 ) 
{
   *p++ = mkModRegRM(3, gregEnc3210 & 7, eregEnc3210 & 7);
   return p;
}

static UChar* doAMode_R ( UChar* p, HReg greg, HReg ereg )
{
   return doAMode_R__wrk(p, iregEnc3210(greg), iregEnc3210(ereg));
}

static UChar* doAMode_R_enc_reg ( UChar* p, UInt gregEnc3210, HReg ereg )
{
   vassert(gregEnc3210 < 16);
   return doAMode_R__wrk(p, gregEnc3210, iregEnc3210(ereg));
}

static UChar* doAMode_R_reg_enc ( UChar* p, HReg greg, UInt eregEnc3210 )
{
   vassert(eregEnc3210 < 16);
   return doAMode_R__wrk(p, iregEnc3210(greg), eregEnc3210);
}

static UChar* doAMode_R_enc_enc ( UChar* p, UInt gregEnc3210, UInt eregEnc3210 )
{
   vassert( (gregEnc3210|eregEnc3210) < 16);
   return doAMode_R__wrk(p, gregEnc3210, eregEnc3210);
}


/* Clear the W bit on a REX byte, thereby changing the operand size
   back to whatever that instruction's default operand size is. */
static inline UChar clearWBit ( UChar rex )
{
   return rex & ~(1<<3);
}


/* Make up a REX byte, with W=1 (size=64), for a (greg,amode) pair. */
inline static UChar rexAMode_M__wrk ( UInt gregEnc3210, AMD64AMode* am )
{
   if (am->tag == Aam_IR) {
      UChar W = 1;  /* we want 64-bit mode */
      UChar R = (gregEnc3210 >> 3) & 1;
      UChar X = 0; /* not relevant */
      UChar B = iregEnc3(am->Aam.IR.reg);
      return 0x40 + ((W << 3) | (R << 2) | (X << 1) | (B << 0));
   }
   if (am->tag == Aam_IRRS) {
      UChar W = 1;  /* we want 64-bit mode */
      UChar R = (gregEnc3210 >> 3) & 1;
      UChar X = iregEnc3(am->Aam.IRRS.index);
      UChar B = iregEnc3(am->Aam.IRRS.base);
      return 0x40 + ((W << 3) | (R << 2) | (X << 1) | (B << 0));
   }
   vassert(0);
   return 0; /*NOTREACHED*/
}

static UChar rexAMode_M ( HReg greg, AMD64AMode* am )
{
   return rexAMode_M__wrk(iregEnc3210(greg), am);
}

static UChar rexAMode_M_enc ( UInt gregEnc3210, AMD64AMode* am )
{
   vassert(gregEnc3210 < 16);
   return rexAMode_M__wrk(gregEnc3210, am);
}


/* Make up a REX byte, with W=1 (size=64), for a (greg,ereg) pair. */
inline static UChar rexAMode_R__wrk ( UInt gregEnc3210, UInt eregEnc3210 )
{
   UChar W = 1;  /* we want 64-bit mode */
   UChar R = (gregEnc3210 >> 3) & 1;
   UChar X = 0; /* not relevant */
   UChar B = (eregEnc3210 >> 3) & 1;
   return 0x40 + ((W << 3) | (R << 2) | (X << 1) | (B << 0));
}

static UChar rexAMode_R ( HReg greg, HReg ereg )
{
   return rexAMode_R__wrk(iregEnc3210(greg), iregEnc3210(ereg));
}

static UChar rexAMode_R_enc_reg ( UInt gregEnc3210, HReg ereg )
{
   vassert(gregEnc3210 < 16);
   return rexAMode_R__wrk(gregEnc3210, iregEnc3210(ereg));
}

static UChar rexAMode_R_reg_enc ( HReg greg, UInt eregEnc3210 )
{
   vassert(eregEnc3210 < 16);
   return rexAMode_R__wrk(iregEnc3210(greg), eregEnc3210);
}

static UChar rexAMode_R_enc_enc ( UInt gregEnc3210, UInt eregEnc3210 )
{
   vassert((gregEnc3210|eregEnc3210) < 16);
   return rexAMode_R__wrk(gregEnc3210, eregEnc3210);
}


//uu /* May 2012: this VEX prefix stuff is currently unused, but has
//uu    verified correct (I reckon).  Certainly it has been known to
//uu    produce correct VEX prefixes during testing. */
//uu 
//uu /* Assemble a 2 or 3 byte VEX prefix from parts.  rexR, rexX, rexB and
//uu    notVvvvv need to be not-ed before packing.  mmmmm, rexW, L and pp go
//uu    in verbatim.  There's no range checking on the bits. */
//uu static UInt packVexPrefix ( UInt rexR, UInt rexX, UInt rexB,
//uu                             UInt mmmmm, UInt rexW, UInt notVvvv,
//uu                             UInt L, UInt pp )
//uu {
//uu    UChar byte0 = 0;
//uu    UChar byte1 = 0;
//uu    UChar byte2 = 0;
//uu    if (rexX == 0 && rexB == 0 && mmmmm == 1 && rexW == 0) {
//uu       /* 2 byte encoding is possible. */
//uu       byte0 = 0xC5;
//uu       byte1 = ((rexR ^ 1) << 7) | ((notVvvv ^ 0xF) << 3) 
//uu               | (L << 2) | pp;
//uu    } else {
//uu       /* 3 byte encoding is needed. */
//uu       byte0 = 0xC4;
//uu       byte1 = ((rexR ^ 1) << 7) | ((rexX ^ 1) << 6)
//uu               | ((rexB ^ 1) << 5) | mmmmm;
//uu       byte2 = (rexW << 7) | ((notVvvv ^ 0xF) << 3) | (L << 2) | pp;
//uu    }
//uu    return (((UInt)byte2) << 16) | (((UInt)byte1) << 8) | ((UInt)byte0);
//uu }
//uu 
//uu /* Make up a VEX prefix for a (greg,amode) pair.  First byte in bits
//uu    7:0 of result, second in 15:8, third (for a 3 byte prefix) in
//uu    23:16.  Has m-mmmm set to indicate a prefix of 0F, pp set to
//uu    indicate no SIMD prefix, W=0 (ignore), L=1 (size=256), and
//uu    vvvv=1111 (unused 3rd reg). */
//uu static UInt vexAMode_M ( HReg greg, AMD64AMode* am )
//uu {
//uu    UChar L       = 1; /* size = 256 */
//uu    UChar pp      = 0; /* no SIMD prefix */
//uu    UChar mmmmm   = 1; /* 0F */
//uu    UChar notVvvv = 0; /* unused */
//uu    UChar rexW    = 0;
//uu    UChar rexR    = 0;
//uu    UChar rexX    = 0;
//uu    UChar rexB    = 0;
//uu    /* Same logic as in rexAMode_M. */
//uu    if (am->tag == Aam_IR) {
//uu       rexR = iregEnc3(greg);
//uu       rexX = 0; /* not relevant */
//uu       rexB = iregEnc3(am->Aam.IR.reg);
//uu    }
//uu    else if (am->tag == Aam_IRRS) {
//uu       rexR = iregEnc3(greg);
//uu       rexX = iregEnc3(am->Aam.IRRS.index);
//uu       rexB = iregEnc3(am->Aam.IRRS.base);
//uu    } else {
//uu       vassert(0);
//uu    }
//uu    return packVexPrefix( rexR, rexX, rexB, mmmmm, rexW, notVvvv, L, pp );
//uu }
//uu 
//uu static UChar* emitVexPrefix ( UChar* p, UInt vex )
//uu {
//uu    switch (vex & 0xFF) {
//uu       case 0xC5:
//uu          *p++ = 0xC5;
//uu          *p++ = (vex >> 8) & 0xFF;
//uu          vassert(0 == (vex >> 16));
//uu          break;
//uu       case 0xC4:
//uu          *p++ = 0xC4;
//uu          *p++ = (vex >> 8) & 0xFF;
//uu          *p++ = (vex >> 16) & 0xFF;
//uu          vassert(0 == (vex >> 24));
//uu          break;
//uu       default:
//uu          vassert(0);
//uu    }
//uu    return p;
//uu }


/* Emit ffree %st(N) */
static UChar* do_ffree_st ( UChar* p, Int n )
{
   vassert(n >= 0 && n <= 7);
   *p++ = 0xDD;
   *p++ = toUChar(0xC0 + n);
   return p;
}

/* Emit an instruction into buf and return the number of bytes used.
   Note that buf is not the insn's final place, and therefore it is
   imperative to emit position-independent code.  If the emitted
   instruction was a profiler inc, set *is_profInc to True, else
   leave it unchanged. */

Int emit_AMD64Instr ( /*MB_MOD*/Bool* is_profInc,
                      UChar* buf, Int nbuf, const AMD64Instr* i, 
                      Bool mode64, VexEndness endness_host,
                      const void* disp_cp_chain_me_to_slowEP,
                      const void* disp_cp_chain_me_to_fastEP,
                      const void* disp_cp_xindir,
                      const void* disp_cp_xassisted )
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

   /* vex_printf("asm  "); ppAMD64Instr(i, mode64); vex_printf("\n"); */

   switch (i->tag) {

   case Ain_Imm64:
      if (i->Ain.Imm64.imm64 <= 0xFFFFFULL) {
         /* Use the short form (load into 32 bit reg, + default
            widening rule) for constants under 1 million.  We could
            use this form for the range 0 to 0x7FFFFFFF inclusive, but
            limit it to a smaller range for verifiability purposes. */
         if (1 & iregEnc3(i->Ain.Imm64.dst))
            *p++ = 0x41;
         *p++ = 0xB8 + iregEnc210(i->Ain.Imm64.dst);
         p = emit32(p, (UInt)i->Ain.Imm64.imm64);
      } else {
         *p++ = toUChar(0x48 + (1 & iregEnc3(i->Ain.Imm64.dst)));
         *p++ = toUChar(0xB8 + iregEnc210(i->Ain.Imm64.dst));
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
                  if (1 & iregEnc3(i->Ain.Alu64R.dst))
                     *p++ = 0x41;
                  *p++ = 0xB8 + iregEnc210(i->Ain.Alu64R.dst);
                  p = emit32(p, i->Ain.Alu64R.src->Armi.Imm.imm32);
               } else {
                  *p++ = toUChar(0x48 + (1 & iregEnc3(i->Ain.Alu64R.dst)));
                  *p++ = 0xC7;
                  *p++ = toUChar(0xC0 + iregEnc210(i->Ain.Alu64R.dst));
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
            if (sameHReg(i->Ain.Alu64R.dst, hregAMD64_RAX())
                && !fits8bits(i->Ain.Alu64R.src->Armi.Imm.imm32)) {
               goto bad; /* FIXME: awaiting test case */
               *p++ = toUChar(opc_imma);
               p = emit32(p, i->Ain.Alu64R.src->Armi.Imm.imm32);
            } else
            if (fits8bits(i->Ain.Alu64R.src->Armi.Imm.imm32)) {
               *p++ = rexAMode_R_enc_reg( 0, i->Ain.Alu64R.dst );
               *p++ = 0x83; 
               p    = doAMode_R_enc_reg(p, subopc_imm, i->Ain.Alu64R.dst);
               *p++ = toUChar(0xFF & i->Ain.Alu64R.src->Armi.Imm.imm32);
            } else {
               *p++ = rexAMode_R_enc_reg( 0, i->Ain.Alu64R.dst);
               *p++ = 0x81; 
               p    = doAMode_R_enc_reg(p, subopc_imm, i->Ain.Alu64R.dst);
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
               *p++ = rexAMode_M_enc(0, i->Ain.Alu64M.dst);
               *p++ = 0xC7;
               p = doAMode_M_enc(p, 0, i->Ain.Alu64M.dst);
               p = emit32(p, i->Ain.Alu64M.src->Ari.Imm.imm32);
               goto done;
            default: 
               goto bad;
         }
      }
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
         *p++ = rexAMode_R_enc_reg(0, i->Ain.Sh64.dst);
         *p++ = toUChar(opc_cl);
         p = doAMode_R_enc_reg(p, subopc, i->Ain.Sh64.dst);
         goto done;
      } else {
         *p++ = rexAMode_R_enc_reg(0, i->Ain.Sh64.dst);
         *p++ = toUChar(opc_imm);
         p = doAMode_R_enc_reg(p, subopc, i->Ain.Sh64.dst);
         *p++ = (UChar)(i->Ain.Sh64.src);
         goto done;
      }
      break;

   case Ain_Test64:
      /* testq sign-extend($imm32), %reg */
      *p++ = rexAMode_R_enc_reg(0, i->Ain.Test64.dst);
      *p++ = 0xF7;
      p = doAMode_R_enc_reg(p, 0, i->Ain.Test64.dst);
      p = emit32(p, i->Ain.Test64.imm32);
      goto done;

   case Ain_Unary64:
      if (i->Ain.Unary64.op == Aun_NOT) {
         *p++ = rexAMode_R_enc_reg(0, i->Ain.Unary64.dst);
         *p++ = 0xF7;
         p = doAMode_R_enc_reg(p, 2, i->Ain.Unary64.dst);
         goto done;
      }
      if (i->Ain.Unary64.op == Aun_NEG) {
         *p++ = rexAMode_R_enc_reg(0, i->Ain.Unary64.dst);
         *p++ = 0xF7;
         p = doAMode_R_enc_reg(p, 3, i->Ain.Unary64.dst);
         goto done;
      }
      break;

   case Ain_Lea64:
      *p++ = rexAMode_M(i->Ain.Lea64.dst, i->Ain.Lea64.am);
      *p++ = 0x8D;
      p = doAMode_M(p, i->Ain.Lea64.dst, i->Ain.Lea64.am);
      goto done;

   case Ain_Alu32R:
      /* ADD/SUB/AND/OR/XOR/CMP */
      opc = opc_rr = subopc_imm = opc_imma = 0;
      switch (i->Ain.Alu32R.op) {
         case Aalu_ADD: opc = 0x03; opc_rr = 0x01; 
                        subopc_imm = 0; opc_imma = 0x05; break;
         case Aalu_SUB: opc = 0x2B; opc_rr = 0x29; 
                        subopc_imm = 5; opc_imma = 0x2D; break;
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
      switch (i->Ain.Alu32R.src->tag) {
         case Armi_Imm:
            if (sameHReg(i->Ain.Alu32R.dst, hregAMD64_RAX())
                && !fits8bits(i->Ain.Alu32R.src->Armi.Imm.imm32)) {
               goto bad; /* FIXME: awaiting test case */
               *p++ = toUChar(opc_imma);
               p = emit32(p, i->Ain.Alu32R.src->Armi.Imm.imm32);
            } else
            if (fits8bits(i->Ain.Alu32R.src->Armi.Imm.imm32)) {
               rex  = clearWBit( rexAMode_R_enc_reg( 0, i->Ain.Alu32R.dst ) );
               if (rex != 0x40) *p++ = rex;
               *p++ = 0x83; 
               p    = doAMode_R_enc_reg(p, subopc_imm, i->Ain.Alu32R.dst);
               *p++ = toUChar(0xFF & i->Ain.Alu32R.src->Armi.Imm.imm32);
            } else {
               rex  = clearWBit( rexAMode_R_enc_reg( 0, i->Ain.Alu32R.dst) );
               if (rex != 0x40) *p++ = rex;
               *p++ = 0x81; 
               p    = doAMode_R_enc_reg(p, subopc_imm, i->Ain.Alu32R.dst);
               p    = emit32(p, i->Ain.Alu32R.src->Armi.Imm.imm32);
            }
            goto done;
         case Armi_Reg:
            rex  = clearWBit( 
                   rexAMode_R( i->Ain.Alu32R.src->Armi.Reg.reg,
                               i->Ain.Alu32R.dst) );
            if (rex != 0x40) *p++ = rex;
            *p++ = toUChar(opc_rr);
            p = doAMode_R(p, i->Ain.Alu32R.src->Armi.Reg.reg,
                             i->Ain.Alu32R.dst);
            goto done;
         case Armi_Mem:
            rex  = clearWBit(
                   rexAMode_M( i->Ain.Alu32R.dst,
                               i->Ain.Alu32R.src->Armi.Mem.am) );
            if (rex != 0x40) *p++ = rex;
            *p++ = toUChar(opc);
            p = doAMode_M(p, i->Ain.Alu32R.dst,
                             i->Ain.Alu32R.src->Armi.Mem.am);
            goto done;
         default: 
            goto bad;
      }
      break;

   case Ain_MulL:
      subopc = i->Ain.MulL.syned ? 5 : 4;
      switch (i->Ain.MulL.src->tag)  {
         case Arm_Mem:
            *p++ = rexAMode_M_enc(0, i->Ain.MulL.src->Arm.Mem.am);
            *p++ = 0xF7;
            p = doAMode_M_enc(p, subopc, i->Ain.MulL.src->Arm.Mem.am);
            goto done;
         case Arm_Reg:
            *p++ = rexAMode_R_enc_reg(0, i->Ain.MulL.src->Arm.Reg.reg);
            *p++ = 0xF7;
            p = doAMode_R_enc_reg(p, subopc, i->Ain.MulL.src->Arm.Reg.reg);
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
               p = doAMode_M_enc(p, subopc, i->Ain.Div.src->Arm.Mem.am);
               goto done;
            case Arm_Reg:
               *p++ = clearWBit(
                      rexAMode_R_enc_reg(0, i->Ain.Div.src->Arm.Reg.reg));
               *p++ = 0xF7;
               p = doAMode_R_enc_reg(p, subopc, i->Ain.Div.src->Arm.Reg.reg);
               goto done;
            default:
               goto bad;
         }
      }
      if (i->Ain.Div.sz == 8) {
         switch (i->Ain.Div.src->tag)  {
            case Arm_Mem:
               *p++ = rexAMode_M_enc(0, i->Ain.Div.src->Arm.Mem.am);
               *p++ = 0xF7;
               p = doAMode_M_enc(p, subopc, i->Ain.Div.src->Arm.Mem.am);
               goto done;
            case Arm_Reg:
               *p++ = rexAMode_R_enc_reg(0, i->Ain.Div.src->Arm.Reg.reg);
               *p++ = 0xF7;
               p = doAMode_R_enc_reg(p, subopc, i->Ain.Div.src->Arm.Reg.reg);
               goto done;
            default:
               goto bad;
         }
      }
      break;

   case Ain_Push:
      switch (i->Ain.Push.src->tag) {
         case Armi_Mem: 
            *p++ = clearWBit(
                   rexAMode_M_enc(0, i->Ain.Push.src->Armi.Mem.am));
            *p++ = 0xFF;
            p = doAMode_M_enc(p, 6, i->Ain.Push.src->Armi.Mem.am);
            goto done;
         case Armi_Imm:
            *p++ = 0x68;
            p = emit32(p, i->Ain.Push.src->Armi.Imm.imm32);
            goto done;
         case Armi_Reg:
            *p++ = toUChar(0x40 + (1 & iregEnc3(i->Ain.Push.src->Armi.Reg.reg)));
            *p++ = toUChar(0x50 + iregEnc210(i->Ain.Push.src->Armi.Reg.reg));
            goto done;
        default: 
            goto bad;
      }

   case Ain_Call: {
      /* As per detailed comment for Ain_Call in getRegUsage_AMD64Instr
         above, %r11 is used as an address temporary. */
      /* If we don't need to do any fixup actions in the case that the
         call doesn't happen, just do the simple thing and emit
         straight-line code.  This is usually the case. */
      if (i->Ain.Call.cond == Acc_ALWAYS/*call always happens*/
          || i->Ain.Call.rloc.pri == RLPri_None/*no fixup action*/) {
         /* jump over the following two insns if the condition does
            not hold */
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
      } else {
         Int delta;
         /* Complex case.  We have to generate an if-then-else diamond. */
         // before:
         //   j{!cond} else:
         //   movabsq $target, %r11
         //   call* %r11
         // preElse:
         //   jmp after:
         // else:
         //   movabsq $0x5555555555555555, %rax  // possibly
         //   movq %rax, %rdx                    // possibly
         // after:

         // before:
         UChar* pBefore = p;

         //   j{!cond} else:
         *p++ = toUChar(0x70 + (0xF & (i->Ain.Call.cond ^ 1)));
         *p++ = 0; /* # of bytes to jump over; don't know how many yet. */

         //   movabsq $target, %r11
         *p++ = 0x49;
         *p++ = 0xBB;
         p = emit64(p, i->Ain.Call.target);

         //   call* %r11
         *p++ = 0x41;
         *p++ = 0xFF;
         *p++ = 0xD3;

         // preElse:
         UChar* pPreElse = p;

         //   jmp after:
         *p++ = 0xEB;
         *p++ = 0; /* # of bytes to jump over; don't know how many yet. */

         // else:
         UChar* pElse = p;

         /* Do the 'else' actions */
         switch (i->Ain.Call.rloc.pri) {
            case RLPri_Int:
               // movabsq $0x5555555555555555, %rax
               *p++ = 0x48; *p++ = 0xB8; p = emit64(p, 0x5555555555555555ULL);
               break;
            case RLPri_2Int:
               vassert(0); //ATC
               // movabsq $0x5555555555555555, %rax
               *p++ = 0x48; *p++ = 0xB8; p = emit64(p, 0x5555555555555555ULL);
               // movq %rax, %rdx
               *p++ = 0x48; *p++ = 0x89; *p++ = 0xC2;
            case RLPri_None: case RLPri_INVALID: default:
               vassert(0);
         }

         // after:
         UChar* pAfter = p;

         // Fix up the branch offsets.  The +2s in the offset
         // calculations are there because x86 requires conditional
         // branches to have their offset stated relative to the
         // instruction immediately following the branch insn.  And in
         // both cases the branch insns are 2 bytes long.

         // First, the "j{!cond} else:" at pBefore.
         delta = (Int)(Long)(pElse - (pBefore + 2));
         vassert(delta >= 0 && delta < 100/*arbitrary*/);
         *(pBefore+1) = (UChar)delta;

         // And secondly, the "jmp after:" at pPreElse.
         delta = (Int)(Long)(pAfter - (pPreElse + 2));
         vassert(delta >= 0 && delta < 100/*arbitrary*/);
         *(pPreElse+1) = (UChar)delta;
      }
      goto done;
   }

   case Ain_XDirect: {
      /* NB: what goes on here has to be very closely coordinated with the
         chainXDirect_AMD64 and unchainXDirect_AMD64 below. */
      /* We're generating chain-me requests here, so we need to be
         sure this is actually allowed -- no-redir translations can't
         use chain-me's.  Hence: */
      vassert(disp_cp_chain_me_to_slowEP != NULL);
      vassert(disp_cp_chain_me_to_fastEP != NULL);

      HReg r11 = hregAMD64_R11();

      /* Use ptmp for backpatching conditional jumps. */
      ptmp = NULL;

      /* First off, if this is conditional, create a conditional
         jump over the rest of it. */
      if (i->Ain.XDirect.cond != Acc_ALWAYS) {
         /* jmp fwds if !condition */
         *p++ = toUChar(0x70 + (0xF & (i->Ain.XDirect.cond ^ 1)));
         ptmp = p; /* fill in this bit later */
         *p++ = 0; /* # of bytes to jump over; don't know how many yet. */
      }

      /* Update the guest RIP. */
      if (fitsIn32Bits(i->Ain.XDirect.dstGA)) {
         /* use a shorter encoding */
         /* movl sign-extend(dstGA), %r11 */
         *p++ = 0x49;
         *p++ = 0xC7;
         *p++ = 0xC3;
         p = emit32(p, (UInt)i->Ain.XDirect.dstGA);
      } else {
         /* movabsq $dstGA, %r11 */
         *p++ = 0x49;
         *p++ = 0xBB;
         p = emit64(p, i->Ain.XDirect.dstGA);
      }

      /* movq %r11, amRIP */
      *p++ = rexAMode_M(r11, i->Ain.XDirect.amRIP);
      *p++ = 0x89;
      p = doAMode_M(p, r11, i->Ain.XDirect.amRIP);

      /* --- FIRST PATCHABLE BYTE follows --- */
      /* VG_(disp_cp_chain_me_to_{slowEP,fastEP}) (where we're calling
         to) backs up the return address, so as to find the address of
         the first patchable byte.  So: don't change the length of the
         two instructions below. */
      /* movabsq $disp_cp_chain_me_to_{slow,fast}EP,%r11; */
      *p++ = 0x49;
      *p++ = 0xBB;
      const void* disp_cp_chain_me
               = i->Ain.XDirect.toFastEP ? disp_cp_chain_me_to_fastEP 
                                         : disp_cp_chain_me_to_slowEP;
      p = emit64(p, (Addr)disp_cp_chain_me);
      /* call *%r11 */
      *p++ = 0x41;
      *p++ = 0xFF;
      *p++ = 0xD3;
      /* --- END of PATCHABLE BYTES --- */

      /* Fix up the conditional jump, if there was one. */
      if (i->Ain.XDirect.cond != Acc_ALWAYS) {
         Int delta = p - ptmp;
         vassert(delta > 0 && delta < 40);
         *ptmp = toUChar(delta-1);
      }
      goto done;
   }

   case Ain_XIndir: {
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
      if (i->Ain.XIndir.cond != Acc_ALWAYS) {
         /* jmp fwds if !condition */
         *p++ = toUChar(0x70 + (0xF & (i->Ain.XIndir.cond ^ 1)));
         ptmp = p; /* fill in this bit later */
         *p++ = 0; /* # of bytes to jump over; don't know how many yet. */
      }

      /* movq dstGA(a reg), amRIP -- copied from Alu64M MOV case */
      *p++ = rexAMode_M(i->Ain.XIndir.dstGA, i->Ain.XIndir.amRIP);
      *p++ = 0x89;
      p = doAMode_M(p, i->Ain.XIndir.dstGA, i->Ain.XIndir.amRIP);

      /* get $disp_cp_xindir into %r11 */
      if (fitsIn32Bits((Addr)disp_cp_xindir)) {
         /* use a shorter encoding */
         /* movl sign-extend(disp_cp_xindir), %r11 */
         *p++ = 0x49;
         *p++ = 0xC7;
         *p++ = 0xC3;
         p = emit32(p, (UInt)(Addr)disp_cp_xindir);
      } else {
         /* movabsq $disp_cp_xindir, %r11 */
         *p++ = 0x49;
         *p++ = 0xBB;
         p = emit64(p, (Addr)disp_cp_xindir);
      }

      /* jmp *%r11 */
      *p++ = 0x41;
      *p++ = 0xFF;
      *p++ = 0xE3;

      /* Fix up the conditional jump, if there was one. */
      if (i->Ain.XIndir.cond != Acc_ALWAYS) {
         Int delta = p - ptmp;
         vassert(delta > 0 && delta < 40);
         *ptmp = toUChar(delta-1);
      }
      goto done;
   }

   case Ain_XAssisted: {
      /* Use ptmp for backpatching conditional jumps. */
      ptmp = NULL;

      /* First off, if this is conditional, create a conditional
         jump over the rest of it. */
      if (i->Ain.XAssisted.cond != Acc_ALWAYS) {
         /* jmp fwds if !condition */
         *p++ = toUChar(0x70 + (0xF & (i->Ain.XAssisted.cond ^ 1)));
         ptmp = p; /* fill in this bit later */
         *p++ = 0; /* # of bytes to jump over; don't know how many yet. */
      }

      /* movq dstGA(a reg), amRIP -- copied from Alu64M MOV case */
      *p++ = rexAMode_M(i->Ain.XAssisted.dstGA, i->Ain.XAssisted.amRIP);
      *p++ = 0x89;
      p = doAMode_M(p, i->Ain.XAssisted.dstGA, i->Ain.XAssisted.amRIP);
      /* movl $magic_number, %ebp.  Since these numbers are all small positive
         integers, we can get away with "movl $N, %ebp" rather than
         the longer "movq $N, %rbp". */
      UInt trcval = 0;
      switch (i->Ain.XAssisted.jk) {
         case Ijk_ClientReq:   trcval = VEX_TRC_JMP_CLIENTREQ;   break;
         case Ijk_Sys_syscall: trcval = VEX_TRC_JMP_SYS_SYSCALL; break;
         case Ijk_Sys_int32:   trcval = VEX_TRC_JMP_SYS_INT32;   break;
         case Ijk_Yield:       trcval = VEX_TRC_JMP_YIELD;       break;
         case Ijk_EmWarn:      trcval = VEX_TRC_JMP_EMWARN;      break;
         case Ijk_MapFail:     trcval = VEX_TRC_JMP_MAPFAIL;     break;
         case Ijk_NoDecode:    trcval = VEX_TRC_JMP_NODECODE;    break;
         case Ijk_InvalICache: trcval = VEX_TRC_JMP_INVALICACHE; break;
         case Ijk_NoRedir:     trcval = VEX_TRC_JMP_NOREDIR;     break;
         case Ijk_SigTRAP:     trcval = VEX_TRC_JMP_SIGTRAP;     break;
         case Ijk_SigSEGV:     trcval = VEX_TRC_JMP_SIGSEGV;     break;
         case Ijk_Boring:      trcval = VEX_TRC_JMP_BORING;      break;
         /* We don't expect to see the following being assisted. */
         case Ijk_Ret:
         case Ijk_Call:
         /* fallthrough */
         default: 
            ppIRJumpKind(i->Ain.XAssisted.jk);
            vpanic("emit_AMD64Instr.Ain_XAssisted: unexpected jump kind");
      }
      vassert(trcval != 0);
      *p++ = 0xBD;
      p = emit32(p, trcval);
      /* movabsq $disp_assisted, %r11 */
      *p++ = 0x49;
      *p++ = 0xBB;
      p = emit64(p, (Addr)disp_cp_xassisted);
      /* jmp *%r11 */
      *p++ = 0x41;
      *p++ = 0xFF;
      *p++ = 0xE3;

      /* Fix up the conditional jump, if there was one. */
      if (i->Ain.XAssisted.cond != Acc_ALWAYS) {
         Int delta = p - ptmp;
         vassert(delta > 0 && delta < 40);
         *ptmp = toUChar(delta-1);
      }
      goto done;
   }

   case Ain_CMov64:
      vassert(i->Ain.CMov64.cond != Acc_ALWAYS);
      *p++ = rexAMode_R(i->Ain.CMov64.dst, i->Ain.CMov64.src);
      *p++ = 0x0F;
      *p++ = toUChar(0x40 + (0xF & i->Ain.CMov64.cond));
      p = doAMode_R(p, i->Ain.CMov64.dst, i->Ain.CMov64.src);
      goto done;

   case Ain_CLoad: {
      vassert(i->Ain.CLoad.cond != Acc_ALWAYS);

      /* Only 32- or 64-bit variants are allowed. */
      vassert(i->Ain.CLoad.szB == 4 || i->Ain.CLoad.szB == 8);

      /* Use ptmp for backpatching conditional jumps. */
      ptmp = NULL;

      /* jmp fwds if !condition */
      *p++ = toUChar(0x70 + (0xF & (i->Ain.CLoad.cond ^ 1)));
      ptmp = p; /* fill in this bit later */
      *p++ = 0; /* # of bytes to jump over; don't know how many yet. */

      /* Now the load.  Either a normal 64 bit load or a normal 32 bit
         load, which, by the default zero-extension rule, zeroes out
         the upper half of the destination, as required. */
      rex = rexAMode_M(i->Ain.CLoad.dst, i->Ain.CLoad.addr);
      *p++ = i->Ain.CLoad.szB == 4 ? clearWBit(rex) : rex;
      *p++ = 0x8B;
      p = doAMode_M(p, i->Ain.CLoad.dst, i->Ain.CLoad.addr);

      /* Fix up the conditional branch */
      Int delta = p - ptmp;
      vassert(delta > 0 && delta < 40);
      *ptmp = toUChar(delta-1);
      goto done;
   }

   case Ain_CStore: {
      /* AFAICS this is identical to Ain_CStore except that the opcode
         is 0x89 instead of 0x8B. */
      vassert(i->Ain.CStore.cond != Acc_ALWAYS);

      /* Only 32- or 64-bit variants are allowed. */
      vassert(i->Ain.CStore.szB == 4 || i->Ain.CStore.szB == 8);

      /* Use ptmp for backpatching conditional jumps. */
      ptmp = NULL;

      /* jmp fwds if !condition */
      *p++ = toUChar(0x70 + (0xF & (i->Ain.CStore.cond ^ 1)));
      ptmp = p; /* fill in this bit later */
      *p++ = 0; /* # of bytes to jump over; don't know how many yet. */

      /* Now the store. */
      rex = rexAMode_M(i->Ain.CStore.src, i->Ain.CStore.addr);
      *p++ = i->Ain.CStore.szB == 4 ? clearWBit(rex) : rex;
      *p++ = 0x89;
      p = doAMode_M(p, i->Ain.CStore.src, i->Ain.CStore.addr);

      /* Fix up the conditional branch */
      Int delta = p - ptmp;
      vassert(delta > 0 && delta < 40);
      *ptmp = toUChar(delta-1);
      goto done;
   }

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
      reg = iregEnc3210(i->Ain.Set64.dst);
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
      rex = rexAMode_M_enc(1, i->Ain.ACAS.addr );
      if (i->Ain.ACAS.sz != 8)
         rex = clearWBit(rex);
      *p++ = rex;
      *p++ = 0x0F;
      *p++ = 0xC7;
      p = doAMode_M_enc(p, 1, i->Ain.DACAS.addr);
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
                   rexAMode_M_enc(0, i->Ain.A87PushPop.addr) );
         *p++ = i->Ain.A87PushPop.szB == 4 ? 0xD9 : 0xDD;
	 p = doAMode_M_enc(p, 0/*subopcode*/, i->Ain.A87PushPop.addr);
      } else {
         /* Dump %st(0) to memory: fstps/fstpl amode */
         *p++ = clearWBit(
                   rexAMode_M_enc(3, i->Ain.A87PushPop.addr) );
         *p++ = i->Ain.A87PushPop.szB == 4 ? 0xD9 : 0xDD;
         p = doAMode_M_enc(p, 3/*subopcode*/, i->Ain.A87PushPop.addr);
         goto done;
      }
      goto done;

   case Ain_A87FpOp:
      switch (i->Ain.A87FpOp.op) {
         case Afp_SQRT:   *p++ = 0xD9; *p++ = 0xFA; break;
         case Afp_SIN:    *p++ = 0xD9; *p++ = 0xFE; break;
         case Afp_COS:    *p++ = 0xD9; *p++ = 0xFF; break;
         case Afp_ROUND:  *p++ = 0xD9; *p++ = 0xFC; break;
         case Afp_2XM1:   *p++ = 0xD9; *p++ = 0xF0; break;
         case Afp_SCALE:  *p++ = 0xD9; *p++ = 0xFD; break;
         case Afp_ATAN:   *p++ = 0xD9; *p++ = 0xF3; break;
         case Afp_YL2X:   *p++ = 0xD9; *p++ = 0xF1; break;
         case Afp_YL2XP1: *p++ = 0xD9; *p++ = 0xF9; break;
         case Afp_PREM:   *p++ = 0xD9; *p++ = 0xF8; break;
         case Afp_PREM1:  *p++ = 0xD9; *p++ = 0xF5; break;
         case Afp_TAN:
            /* fptan pushes 1.0 on the FP stack, except when the
               argument is out of range.  Hence we have to do the
               instruction, then inspect C2 to see if there is an out
               of range condition.  If there is, we skip the fincstp
               that is used by the in-range case to get rid of this
               extra 1.0 value. */
            *p++ = 0xD9; *p++ = 0xF2; // fptan
            *p++ = 0x50;              // pushq %rax
            *p++ = 0xDF; *p++ = 0xE0; // fnstsw %ax
            *p++ = 0x66; *p++ = 0xA9; 
            *p++ = 0x00; *p++ = 0x04; // testw $0x400,%ax
            *p++ = 0x75; *p++ = 0x02; // jnz after_fincstp
            *p++ = 0xD9; *p++ = 0xF7; // fincstp
            *p++ = 0x58;              // after_fincstp: popq %rax
            break;
         default:
            goto bad;
      }
      goto done;

   case Ain_A87LdCW:
      *p++ = clearWBit(
                rexAMode_M_enc(5, i->Ain.A87LdCW.addr) );
      *p++ = 0xD9;
      p = doAMode_M_enc(p, 5/*subopcode*/, i->Ain.A87LdCW.addr);
      goto done;

   case Ain_A87StSW:
      *p++ = clearWBit(
                rexAMode_M_enc(7, i->Ain.A87StSW.addr) );
      *p++ = 0xDD;
      p = doAMode_M_enc(p, 7/*subopcode*/, i->Ain.A87StSW.addr);
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

   case Ain_LdMXCSR:
      *p++ = clearWBit(rexAMode_M_enc(0, i->Ain.LdMXCSR.addr));
      *p++ = 0x0F;
      *p++ = 0xAE;
      p = doAMode_M_enc(p, 2/*subopcode*/, i->Ain.LdMXCSR.addr);
      goto done;

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
             rexAMode_R_enc_enc( vregEnc3210(i->Ain.SseUComIS.srcL),
                                 vregEnc3210(i->Ain.SseUComIS.srcR) ));
      *p++ = 0x0F;
      *p++ = 0x2E;
      p = doAMode_R_enc_enc(p, vregEnc3210(i->Ain.SseUComIS.srcL),
                               vregEnc3210(i->Ain.SseUComIS.srcR) );
      /* pushfq */
      *p++ = 0x9C;
      /* popq %dst */
      *p++ = toUChar(0x40 + (1 & iregEnc3(i->Ain.SseUComIS.dst)));
      *p++ = toUChar(0x58 + iregEnc210(i->Ain.SseUComIS.dst));
      goto done;

   case Ain_SseSI2SF:
      /* cvssi2s[sd] %src, %dst */
      rex = rexAMode_R_enc_reg( vregEnc3210(i->Ain.SseSI2SF.dst),
                                i->Ain.SseSI2SF.src );
      *p++ = toUChar(i->Ain.SseSI2SF.szD==4 ? 0xF3 : 0xF2);
      *p++ = toUChar(i->Ain.SseSI2SF.szS==4 ? clearWBit(rex) : rex);
      *p++ = 0x0F;
      *p++ = 0x2A;
      p = doAMode_R_enc_reg( p, vregEnc3210(i->Ain.SseSI2SF.dst),
                                i->Ain.SseSI2SF.src );
      goto done;

   case Ain_SseSF2SI:
      /* cvss[sd]2si %src, %dst */
      rex = rexAMode_R_reg_enc( i->Ain.SseSF2SI.dst,
                                vregEnc3210(i->Ain.SseSF2SI.src) );
      *p++ = toUChar(i->Ain.SseSF2SI.szS==4 ? 0xF3 : 0xF2);
      *p++ = toUChar(i->Ain.SseSF2SI.szD==4 ? clearWBit(rex) : rex);
      *p++ = 0x0F;
      *p++ = 0x2D;
      p = doAMode_R_reg_enc( p, i->Ain.SseSF2SI.dst,
                                vregEnc3210(i->Ain.SseSF2SI.src) );
      goto done;

   case Ain_SseSDSS:
      /* cvtsd2ss/cvtss2sd %src, %dst */
      *p++ = toUChar(i->Ain.SseSDSS.from64 ? 0xF2 : 0xF3);
      *p++ = clearWBit(
              rexAMode_R_enc_enc( vregEnc3210(i->Ain.SseSDSS.dst),
                                  vregEnc3210(i->Ain.SseSDSS.src) ));
      *p++ = 0x0F;
      *p++ = 0x5A;
      p = doAMode_R_enc_enc( p, vregEnc3210(i->Ain.SseSDSS.dst),
                                vregEnc3210(i->Ain.SseSDSS.src) );
      goto done;

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
             rexAMode_M_enc(vregEnc3210(i->Ain.SseLdSt.reg),
                            i->Ain.SseLdSt.addr));
      *p++ = 0x0F; 
      *p++ = toUChar(i->Ain.SseLdSt.isLoad ? 0x10 : 0x11);
      p = doAMode_M_enc(p, vregEnc3210(i->Ain.SseLdSt.reg),
                           i->Ain.SseLdSt.addr);
      goto done;

   case Ain_SseLdzLO:
      vassert(i->Ain.SseLdzLO.sz == 4 || i->Ain.SseLdzLO.sz == 8);
      /* movs[sd] amode, %xmm-dst */
      *p++ = toUChar(i->Ain.SseLdzLO.sz==4 ? 0xF3 : 0xF2);
      *p++ = clearWBit(
             rexAMode_M_enc(vregEnc3210(i->Ain.SseLdzLO.reg), 
                            i->Ain.SseLdzLO.addr));
      *p++ = 0x0F; 
      *p++ = 0x10; 
      p = doAMode_M_enc(p, vregEnc3210(i->Ain.SseLdzLO.reg), 
                           i->Ain.SseLdzLO.addr);
      goto done;

   case Ain_Sse32Fx4:
      xtra = 0;
      *p++ = clearWBit(
             rexAMode_R_enc_enc( vregEnc3210(i->Ain.Sse32Fx4.dst),
                                 vregEnc3210(i->Ain.Sse32Fx4.src) ));
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
      p = doAMode_R_enc_enc(p, vregEnc3210(i->Ain.Sse32Fx4.dst),
                               vregEnc3210(i->Ain.Sse32Fx4.src) );
      if (xtra & 0x100)
         *p++ = toUChar(xtra & 0xFF);
      goto done;

   case Ain_Sse64Fx2:
      xtra = 0;
      *p++ = 0x66;
      *p++ = clearWBit(
             rexAMode_R_enc_enc( vregEnc3210(i->Ain.Sse64Fx2.dst),
                                 vregEnc3210(i->Ain.Sse64Fx2.src) ));
      *p++ = 0x0F;
      switch (i->Ain.Sse64Fx2.op) {
         case Asse_ADDF:   *p++ = 0x58; break;
         case Asse_DIVF:   *p++ = 0x5E; break;
         case Asse_MAXF:   *p++ = 0x5F; break;
         case Asse_MINF:   *p++ = 0x5D; break;
         case Asse_MULF:   *p++ = 0x59; break;
         case Asse_SQRTF:  *p++ = 0x51; break;
         case Asse_SUBF:   *p++ = 0x5C; break;
         case Asse_CMPEQF: *p++ = 0xC2; xtra = 0x100; break;
         case Asse_CMPLTF: *p++ = 0xC2; xtra = 0x101; break;
         case Asse_CMPLEF: *p++ = 0xC2; xtra = 0x102; break;
         case Asse_CMPUNF: *p++ = 0xC2; xtra = 0x103; break;
         default: goto bad;
      }
      p = doAMode_R_enc_enc(p, vregEnc3210(i->Ain.Sse64Fx2.dst),
                               vregEnc3210(i->Ain.Sse64Fx2.src) );
      if (xtra & 0x100)
         *p++ = toUChar(xtra & 0xFF);
      goto done;

   case Ain_Sse32FLo:
      xtra = 0;
      *p++ = 0xF3;
      *p++ = clearWBit(
             rexAMode_R_enc_enc( vregEnc3210(i->Ain.Sse32FLo.dst),
                                 vregEnc3210(i->Ain.Sse32FLo.src) ));
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
      p = doAMode_R_enc_enc(p, vregEnc3210(i->Ain.Sse32FLo.dst),
                               vregEnc3210(i->Ain.Sse32FLo.src) );
      if (xtra & 0x100)
         *p++ = toUChar(xtra & 0xFF);
      goto done;

   case Ain_Sse64FLo:
      xtra = 0;
      *p++ = 0xF2;
      *p++ = clearWBit(
             rexAMode_R_enc_enc( vregEnc3210(i->Ain.Sse64FLo.dst),
                                 vregEnc3210(i->Ain.Sse64FLo.src) ));
      *p++ = 0x0F;
      switch (i->Ain.Sse64FLo.op) {
         case Asse_ADDF:   *p++ = 0x58; break;
         case Asse_DIVF:   *p++ = 0x5E; break;
         case Asse_MAXF:   *p++ = 0x5F; break;
         case Asse_MINF:   *p++ = 0x5D; break;
         case Asse_MULF:   *p++ = 0x59; break;
         case Asse_SQRTF:  *p++ = 0x51; break;
         case Asse_SUBF:   *p++ = 0x5C; break;
         case Asse_CMPEQF: *p++ = 0xC2; xtra = 0x100; break;
         case Asse_CMPLTF: *p++ = 0xC2; xtra = 0x101; break;
         case Asse_CMPLEF: *p++ = 0xC2; xtra = 0x102; break;
         case Asse_CMPUNF: *p++ = 0xC2; xtra = 0x103; break;
         default: goto bad;
      }
      p = doAMode_R_enc_enc(p, vregEnc3210(i->Ain.Sse64FLo.dst),
                               vregEnc3210(i->Ain.Sse64FLo.src) );
      if (xtra & 0x100)
         *p++ = toUChar(xtra & 0xFF);
      goto done;

   case Ain_SseReRg:
#     define XX(_n) *p++ = (_n)

      rex = clearWBit(
            rexAMode_R_enc_enc( vregEnc3210(i->Ain.SseReRg.dst),
                                vregEnc3210(i->Ain.SseReRg.src) ));

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
      p = doAMode_R_enc_enc(p, vregEnc3210(i->Ain.SseReRg.dst),
                               vregEnc3210(i->Ain.SseReRg.src) );
#     undef XX
      goto done;

   case Ain_SseCMov:
      /* jmp fwds if !condition */
      *p++ = toUChar(0x70 + (i->Ain.SseCMov.cond ^ 1));
      *p++ = 0; /* # of bytes in the next bit, which we don't know yet */
      ptmp = p;

      /* movaps %src, %dst */
      *p++ = clearWBit(
             rexAMode_R_enc_enc( vregEnc3210(i->Ain.SseCMov.dst),
                                 vregEnc3210(i->Ain.SseCMov.src) ));
      *p++ = 0x0F; 
      *p++ = 0x28; 
      p = doAMode_R_enc_enc(p, vregEnc3210(i->Ain.SseCMov.dst),
                               vregEnc3210(i->Ain.SseCMov.src) );

      /* Fill in the jump offset. */
      *(ptmp-1) = toUChar(p - ptmp);
      goto done;

   case Ain_SseShuf:
      *p++ = 0x66; 
      *p++ = clearWBit(
             rexAMode_R_enc_enc( vregEnc3210(i->Ain.SseShuf.dst),
                                 vregEnc3210(i->Ain.SseShuf.src) ));
      *p++ = 0x0F; 
      *p++ = 0x70; 
      p = doAMode_R_enc_enc(p, vregEnc3210(i->Ain.SseShuf.dst),
                               vregEnc3210(i->Ain.SseShuf.src) );
      *p++ = (UChar)(i->Ain.SseShuf.order);
      goto done;

   //uu case Ain_AvxLdSt: {
   //uu    UInt vex = vexAMode_M( dvreg2ireg(i->Ain.AvxLdSt.reg),
   //uu                           i->Ain.AvxLdSt.addr );
   //uu    p = emitVexPrefix(p, vex);
   //uu    *p++ = toUChar(i->Ain.AvxLdSt.isLoad ? 0x10 : 0x11);
   //uu    p = doAMode_M(p, dvreg2ireg(i->Ain.AvxLdSt.reg), i->Ain.AvxLdSt.addr);
   //uu      goto done;
   //uu }

   case Ain_EvCheck: {
      /* We generate:
            (3 bytes)  decl 8(%rbp)    8 == offsetof(host_EvC_COUNTER)
            (2 bytes)  jns  nofail     expected taken
            (3 bytes)  jmp* 0(%rbp)    0 == offsetof(host_EvC_FAILADDR)
            nofail:
      */
      /* This is heavily asserted re instruction lengths.  It needs to
         be.  If we get given unexpected forms of .amCounter or
         .amFailAddr -- basically, anything that's not of the form
         uimm7(%rbp) -- they are likely to fail. */
      /* Note also that after the decl we must be very careful not to
         read the carry flag, else we get a partial flags stall.
         js/jns avoids that, though. */
      UChar* p0 = p;
      /* ---  decl 8(%rbp) --- */
      /* Need to compute the REX byte for the decl in order to prove
         that we don't need it, since this is a 32-bit inc and all
         registers involved in the amode are < r8.  "1" because
         there's no register in this encoding; instead the register
         field is used as a sub opcode.  The encoding for "decl r/m32"
         is FF /1, hence the "1". */
      rex = clearWBit(rexAMode_M_enc(1, i->Ain.EvCheck.amCounter));
      if (rex != 0x40) goto bad; /* We don't expect to need the REX byte. */
      *p++ = 0xFF;
      p = doAMode_M_enc(p, 1, i->Ain.EvCheck.amCounter);
      vassert(p - p0 == 3);
      /* --- jns nofail --- */
      *p++ = 0x79;
      *p++ = 0x03; /* need to check this 0x03 after the next insn */
      vassert(p - p0 == 5);
      /* --- jmp* 0(%rbp) --- */
      /* Once again, verify we don't need REX.  The encoding is FF /4.
         We don't need REX.W since by default FF /4 in 64-bit mode
         implies a 64 bit load. */
      rex = clearWBit(rexAMode_M_enc(4, i->Ain.EvCheck.amFailAddr));
      if (rex != 0x40) goto bad;
      *p++ = 0xFF;
      p = doAMode_M_enc(p, 4, i->Ain.EvCheck.amFailAddr);
      vassert(p - p0 == 8); /* also ensures that 0x03 offset above is ok */
      /* And crosscheck .. */
      vassert(evCheckSzB_AMD64() == 8);
      goto done;
   }

   case Ain_ProfInc: {
      /* We generate   movabsq $0, %r11
                       incq (%r11)
         in the expectation that a later call to LibVEX_patchProfCtr
         will be used to fill in the immediate field once the right
         value is known.
         49 BB 00 00 00 00 00 00 00 00
         49 FF 03
      */
      *p++ = 0x49; *p++ = 0xBB;
      *p++ = 0x00; *p++ = 0x00; *p++ = 0x00; *p++ = 0x00;
      *p++ = 0x00; *p++ = 0x00; *p++ = 0x00; *p++ = 0x00;
      *p++ = 0x49; *p++ = 0xFF; *p++ = 0x03;
      /* Tell the caller .. */
      vassert(!(*is_profInc));
      *is_profInc = True;
      goto done;
   }

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
}


/* How big is an event check?  See case for Ain_EvCheck in
   emit_AMD64Instr just above.  That crosschecks what this returns, so
   we can tell if we're inconsistent. */
Int evCheckSzB_AMD64 (void)
{
   return 8;
}


/* NB: what goes on here has to be very closely coordinated with the
   emitInstr case for XDirect, above. */
VexInvalRange chainXDirect_AMD64 ( VexEndness endness_host,
                                   void* place_to_chain,
                                   const void* disp_cp_chain_me_EXPECTED,
                                   const void* place_to_jump_to )
{
   vassert(endness_host == VexEndnessLE);

   /* What we're expecting to see is:
        movabsq $disp_cp_chain_me_EXPECTED, %r11
        call *%r11
      viz
        49 BB <8 bytes value == disp_cp_chain_me_EXPECTED>
        41 FF D3
   */
   UChar* p = (UChar*)place_to_chain;
   vassert(p[0] == 0x49);
   vassert(p[1] == 0xBB);
   vassert(read_misaligned_ULong_LE(&p[2]) == (Addr)disp_cp_chain_me_EXPECTED);
   vassert(p[10] == 0x41);
   vassert(p[11] == 0xFF);
   vassert(p[12] == 0xD3);
   /* And what we want to change it to is either:
        (general case):
          movabsq $place_to_jump_to, %r11
          jmpq *%r11
        viz
          49 BB <8 bytes value == place_to_jump_to>
          41 FF E3
        So it's the same length (convenient, huh) and we don't
        need to change all the bits.
      ---OR---
        in the case where the displacement falls within 32 bits
          jmpq disp32   where disp32 is relative to the next insn
          ud2; ud2; ud2; ud2
        viz
          E9 <4 bytes == disp32>
          0F 0B 0F 0B 0F 0B 0F 0B 

      In both cases the replacement has the same length as the original.
      To remain sane & verifiable,
      (1) limit the displacement for the short form to 
          (say) +/- one billion, so as to avoid wraparound
          off-by-ones
      (2) even if the short form is applicable, once every (say)
          1024 times use the long form anyway, so as to maintain
          verifiability
   */
   /* This is the delta we need to put into a JMP d32 insn.  It's
      relative to the start of the next insn, hence the -5.  */
   Long delta   = (Long)((const UChar *)place_to_jump_to - (const UChar*)p) - 5;
   Bool shortOK = delta >= -1000*1000*1000 && delta < 1000*1000*1000;

   static UInt shortCTR = 0; /* DO NOT MAKE NON-STATIC */
   if (shortOK) {
      shortCTR++; // thread safety bleh
      if (0 == (shortCTR & 0x3FF)) {
         shortOK = False;
         if (0)
            vex_printf("QQQ chainXDirect_AMD64: shortCTR = %u, "
                       "using long jmp\n", shortCTR);
      }
   }

   /* And make the modifications. */
   if (shortOK) {
      p[0]  = 0xE9;
      write_misaligned_UInt_LE(&p[1], (UInt)(Int)delta);
      p[5]  = 0x0F; p[6]  = 0x0B;
      p[7]  = 0x0F; p[8]  = 0x0B;
      p[9]  = 0x0F; p[10] = 0x0B;
      p[11] = 0x0F; p[12] = 0x0B;
      /* sanity check on the delta -- top 32 are all 0 or all 1 */
      delta >>= 32;
      vassert(delta == 0LL || delta == -1LL);
   } else {
      /* Minimal modifications from the starting sequence. */   
      write_misaligned_ULong_LE(&p[2], (ULong)(Addr)place_to_jump_to);
      p[12] = 0xE3;
   }
   VexInvalRange vir = { (HWord)place_to_chain, 13 };
   return vir;
}


/* NB: what goes on here has to be very closely coordinated with the
   emitInstr case for XDirect, above. */
VexInvalRange unchainXDirect_AMD64 ( VexEndness endness_host,
                                     void* place_to_unchain,
                                     const void* place_to_jump_to_EXPECTED,
                                     const void* disp_cp_chain_me )
{
   vassert(endness_host == VexEndnessLE);

   /* What we're expecting to see is either:
        (general case)
          movabsq $place_to_jump_to_EXPECTED, %r11
          jmpq *%r11
        viz
          49 BB <8 bytes value == place_to_jump_to_EXPECTED>
          41 FF E3
      ---OR---
        in the case where the displacement falls within 32 bits
          jmpq d32
          ud2; ud2; ud2; ud2
        viz
          E9 <4 bytes == disp32>
          0F 0B 0F 0B 0F 0B 0F 0B
   */
   UChar* p     = (UChar*)place_to_unchain;
   Bool   valid = False;
   if (p[0] == 0x49 && p[1] == 0xBB
       && read_misaligned_ULong_LE(&p[2])
          == (ULong)(Addr)place_to_jump_to_EXPECTED
       && p[10] == 0x41 && p[11] == 0xFF && p[12] == 0xE3) {
      /* it's the long form */
      valid = True;
   }
   else
   if (p[0] == 0xE9 
       && p[5]  == 0x0F && p[6]  == 0x0B
       && p[7]  == 0x0F && p[8]  == 0x0B
       && p[9]  == 0x0F && p[10] == 0x0B
       && p[11] == 0x0F && p[12] == 0x0B) {
      /* It's the short form.  Check the offset is right. */
      Int  s32 = (Int)read_misaligned_UInt_LE(&p[1]);
      Long s64 = (Long)s32;
      if ((UChar*)p + 5 + s64 == place_to_jump_to_EXPECTED) {
         valid = True;
         if (0)
            vex_printf("QQQ unchainXDirect_AMD64: found short form\n");
      }
   }
   vassert(valid);
   /* And what we want to change it to is:
        movabsq $disp_cp_chain_me, %r11
        call *%r11
      viz
        49 BB <8 bytes value == disp_cp_chain_me>
        41 FF D3
      So it's the same length (convenient, huh).
   */
   p[0] = 0x49;
   p[1] = 0xBB;
   write_misaligned_ULong_LE(&p[2], (ULong)(Addr)disp_cp_chain_me);
   p[10] = 0x41;
   p[11] = 0xFF;
   p[12] = 0xD3;
   VexInvalRange vir = { (HWord)place_to_unchain, 13 };
   return vir;
}


/* Patch the counter address into a profile inc point, as previously
   created by the Ain_ProfInc case for emit_AMD64Instr. */
VexInvalRange patchProfInc_AMD64 ( VexEndness endness_host,
                                   void*  place_to_patch,
                                   const ULong* location_of_counter )
{
   vassert(endness_host == VexEndnessLE);
   vassert(sizeof(ULong*) == 8);
   UChar* p = (UChar*)place_to_patch;
   vassert(p[0] == 0x49);
   vassert(p[1] == 0xBB);
   vassert(p[2] == 0x00);
   vassert(p[3] == 0x00);
   vassert(p[4] == 0x00);
   vassert(p[5] == 0x00);
   vassert(p[6] == 0x00);
   vassert(p[7] == 0x00);
   vassert(p[8] == 0x00);
   vassert(p[9] == 0x00);
   vassert(p[10] == 0x49);
   vassert(p[11] == 0xFF);
   vassert(p[12] == 0x03);
   ULong imm64 = (ULong)(Addr)location_of_counter;
   p[2] = imm64 & 0xFF; imm64 >>= 8;
   p[3] = imm64 & 0xFF; imm64 >>= 8;
   p[4] = imm64 & 0xFF; imm64 >>= 8;
   p[5] = imm64 & 0xFF; imm64 >>= 8;
   p[6] = imm64 & 0xFF; imm64 >>= 8;
   p[7] = imm64 & 0xFF; imm64 >>= 8;
   p[8] = imm64 & 0xFF; imm64 >>= 8;
   p[9] = imm64 & 0xFF; imm64 >>= 8;
   VexInvalRange vir = { (HWord)place_to_patch, 13 };
   return vir;
}


/*---------------------------------------------------------------*/
/*--- end                                   host_amd64_defs.c ---*/
/*---------------------------------------------------------------*/
