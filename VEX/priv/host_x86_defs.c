
/*---------------------------------------------------------------*/
/*--- begin                                   host_x86_defs.c ---*/
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
#include "host_x86_defs.h"


/* --------- Registers. --------- */

const RRegUniverse* getRRegUniverse_X86 ( void )
{
   /* The real-register universe is a big constant, so we just want to
      initialise it once. */
   static RRegUniverse rRegUniverse_X86;
   static Bool         rRegUniverse_X86_initted = False;

   /* Handy shorthand, nothing more */
   RRegUniverse* ru = &rRegUniverse_X86;

   /* This isn't thread-safe.  Sigh. */
   if (LIKELY(rRegUniverse_X86_initted))
      return ru;

   RRegUniverse__init(ru);

   /* Add the registers.  The initial segment of this array must be
      those available for allocation by reg-alloc, and those that
      follow are not available for allocation. */
   ru->regs[ru->size++] = hregX86_EAX();
   ru->regs[ru->size++] = hregX86_EBX();
   ru->regs[ru->size++] = hregX86_ECX();
   ru->regs[ru->size++] = hregX86_EDX();
   ru->regs[ru->size++] = hregX86_ESI();
   ru->regs[ru->size++] = hregX86_EDI();
   ru->regs[ru->size++] = hregX86_FAKE0();
   ru->regs[ru->size++] = hregX86_FAKE1();
   ru->regs[ru->size++] = hregX86_FAKE2();
   ru->regs[ru->size++] = hregX86_FAKE3();
   ru->regs[ru->size++] = hregX86_FAKE4();
   ru->regs[ru->size++] = hregX86_FAKE5();
   ru->regs[ru->size++] = hregX86_XMM0();
   ru->regs[ru->size++] = hregX86_XMM1();
   ru->regs[ru->size++] = hregX86_XMM2();
   ru->regs[ru->size++] = hregX86_XMM3();
   ru->regs[ru->size++] = hregX86_XMM4();
   ru->regs[ru->size++] = hregX86_XMM5();
   ru->regs[ru->size++] = hregX86_XMM6();
   ru->regs[ru->size++] = hregX86_XMM7();
   ru->allocable = ru->size;
   /* And other regs, not available to the allocator. */
   ru->regs[ru->size++] = hregX86_ESP();
   ru->regs[ru->size++] = hregX86_EBP();

   rRegUniverse_X86_initted = True;

   RRegUniverse__check_is_sane(ru);
   return ru;
}


void ppHRegX86 ( HReg reg ) 
{
   Int r;
   static const HChar* ireg32_names[8] 
     = { "%eax", "%ecx", "%edx", "%ebx", "%esp", "%ebp", "%esi", "%edi" };
   /* Be generic for all virtual regs. */
   if (hregIsVirtual(reg)) {
      ppHReg(reg);
      return;
   }
   /* But specific for real regs. */
   switch (hregClass(reg)) {
      case HRcInt32:
         r = hregEncoding(reg);
         vassert(r >= 0 && r < 8);
         vex_printf("%s", ireg32_names[r]);
         return;
      case HRcFlt64:
         r = hregEncoding(reg);
         vassert(r >= 0 && r < 6);
         vex_printf("%%fake%d", r);
         return;
      case HRcVec128:
         r = hregEncoding(reg);
         vassert(r >= 0 && r < 8);
         vex_printf("%%xmm%d", r);
         return;
      default:
         vpanic("ppHRegX86");
   }
}


/* --------- Condition codes, Intel encoding. --------- */

const HChar* showX86CondCode ( X86CondCode cond )
{
   switch (cond) {
      case Xcc_O:      return "o";
      case Xcc_NO:     return "no";
      case Xcc_B:      return "b";
      case Xcc_NB:     return "nb";
      case Xcc_Z:      return "z";
      case Xcc_NZ:     return "nz";
      case Xcc_BE:     return "be";
      case Xcc_NBE:    return "nbe";
      case Xcc_S:      return "s";
      case Xcc_NS:     return "ns";
      case Xcc_P:      return "p";
      case Xcc_NP:     return "np";
      case Xcc_L:      return "l";
      case Xcc_NL:     return "nl";
      case Xcc_LE:     return "le";
      case Xcc_NLE:    return "nle";
      case Xcc_ALWAYS: return "ALWAYS";
      default: vpanic("ppX86CondCode");
   }
}


/* --------- X86AMode: memory address expressions. --------- */

X86AMode* X86AMode_IR ( UInt imm32, HReg reg ) {
   X86AMode* am = LibVEX_Alloc_inline(sizeof(X86AMode));
   am->tag = Xam_IR;
   am->Xam.IR.imm = imm32;
   am->Xam.IR.reg = reg;
   return am;
}
X86AMode* X86AMode_IRRS ( UInt imm32, HReg base, HReg indEx, Int shift ) {
   X86AMode* am = LibVEX_Alloc_inline(sizeof(X86AMode));
   am->tag = Xam_IRRS;
   am->Xam.IRRS.imm = imm32;
   am->Xam.IRRS.base = base;
   am->Xam.IRRS.index = indEx;
   am->Xam.IRRS.shift = shift;
   vassert(shift >= 0 && shift <= 3);
   return am;
}

X86AMode* dopyX86AMode ( X86AMode* am ) {
   switch (am->tag) {
      case Xam_IR: 
         return X86AMode_IR( am->Xam.IR.imm, am->Xam.IR.reg );
      case Xam_IRRS: 
         return X86AMode_IRRS( am->Xam.IRRS.imm, am->Xam.IRRS.base, 
                               am->Xam.IRRS.index, am->Xam.IRRS.shift );
      default:
         vpanic("dopyX86AMode");
   }
}

void ppX86AMode ( X86AMode* am ) {
   switch (am->tag) {
      case Xam_IR: 
         if (am->Xam.IR.imm == 0)
            vex_printf("(");
         else
            vex_printf("0x%x(", am->Xam.IR.imm);
         ppHRegX86(am->Xam.IR.reg);
         vex_printf(")");
         return;
      case Xam_IRRS:
         vex_printf("0x%x(", am->Xam.IRRS.imm);
         ppHRegX86(am->Xam.IRRS.base);
         vex_printf(",");
         ppHRegX86(am->Xam.IRRS.index);
         vex_printf(",%d)", 1 << am->Xam.IRRS.shift);
         return;
      default:
         vpanic("ppX86AMode");
   }
}

static void addRegUsage_X86AMode ( HRegUsage* u, X86AMode* am ) {
   switch (am->tag) {
      case Xam_IR: 
         addHRegUse(u, HRmRead, am->Xam.IR.reg);
         return;
      case Xam_IRRS:
         addHRegUse(u, HRmRead, am->Xam.IRRS.base);
         addHRegUse(u, HRmRead, am->Xam.IRRS.index);
         return;
      default:
         vpanic("addRegUsage_X86AMode");
   }
}

static void mapRegs_X86AMode ( HRegRemap* m, X86AMode* am ) {
   switch (am->tag) {
      case Xam_IR: 
         am->Xam.IR.reg = lookupHRegRemap(m, am->Xam.IR.reg);
         return;
      case Xam_IRRS:
         am->Xam.IRRS.base = lookupHRegRemap(m, am->Xam.IRRS.base);
         am->Xam.IRRS.index = lookupHRegRemap(m, am->Xam.IRRS.index);
         return;
      default:
         vpanic("mapRegs_X86AMode");
   }
}

/* --------- Operand, which can be reg, immediate or memory. --------- */

X86RMI* X86RMI_Imm ( UInt imm32 ) {
   X86RMI* op         = LibVEX_Alloc_inline(sizeof(X86RMI));
   op->tag            = Xrmi_Imm;
   op->Xrmi.Imm.imm32 = imm32;
   return op;
}
X86RMI* X86RMI_Reg ( HReg reg ) {
   X86RMI* op       = LibVEX_Alloc_inline(sizeof(X86RMI));
   op->tag          = Xrmi_Reg;
   op->Xrmi.Reg.reg = reg;
   return op;
}
X86RMI* X86RMI_Mem ( X86AMode* am ) {
   X86RMI* op      = LibVEX_Alloc_inline(sizeof(X86RMI));
   op->tag         = Xrmi_Mem;
   op->Xrmi.Mem.am = am;
   return op;
}

void ppX86RMI ( X86RMI* op ) {
   switch (op->tag) {
      case Xrmi_Imm: 
         vex_printf("$0x%x", op->Xrmi.Imm.imm32);
         return;
      case Xrmi_Reg: 
         ppHRegX86(op->Xrmi.Reg.reg);
         return;
      case Xrmi_Mem: 
         ppX86AMode(op->Xrmi.Mem.am);
         return;
     default: 
         vpanic("ppX86RMI");
   }
}

/* An X86RMI can only be used in a "read" context (what would it mean
   to write or modify a literal?) and so we enumerate its registers
   accordingly. */
static void addRegUsage_X86RMI ( HRegUsage* u, X86RMI* op ) {
   switch (op->tag) {
      case Xrmi_Imm: 
         return;
      case Xrmi_Reg: 
         addHRegUse(u, HRmRead, op->Xrmi.Reg.reg);
         return;
      case Xrmi_Mem: 
         addRegUsage_X86AMode(u, op->Xrmi.Mem.am);
         return;
      default: 
         vpanic("addRegUsage_X86RMI");
   }
}

static void mapRegs_X86RMI ( HRegRemap* m, X86RMI* op ) {
   switch (op->tag) {
      case Xrmi_Imm: 
         return;
      case Xrmi_Reg: 
         op->Xrmi.Reg.reg = lookupHRegRemap(m, op->Xrmi.Reg.reg);
         return;
      case Xrmi_Mem: 
         mapRegs_X86AMode(m, op->Xrmi.Mem.am);
         return;
      default: 
         vpanic("mapRegs_X86RMI");
   }
}


/* --------- Operand, which can be reg or immediate only. --------- */

X86RI* X86RI_Imm ( UInt imm32 ) {
   X86RI* op         = LibVEX_Alloc_inline(sizeof(X86RI));
   op->tag           = Xri_Imm;
   op->Xri.Imm.imm32 = imm32;
   return op;
}
X86RI* X86RI_Reg ( HReg reg ) {
   X86RI* op       = LibVEX_Alloc_inline(sizeof(X86RI));
   op->tag         = Xri_Reg;
   op->Xri.Reg.reg = reg;
   return op;
}

void ppX86RI ( X86RI* op ) {
   switch (op->tag) {
      case Xri_Imm: 
         vex_printf("$0x%x", op->Xri.Imm.imm32);
         return;
      case Xri_Reg: 
         ppHRegX86(op->Xri.Reg.reg);
         return;
     default: 
         vpanic("ppX86RI");
   }
}

/* An X86RI can only be used in a "read" context (what would it mean
   to write or modify a literal?) and so we enumerate its registers
   accordingly. */
static void addRegUsage_X86RI ( HRegUsage* u, X86RI* op ) {
   switch (op->tag) {
      case Xri_Imm: 
         return;
      case Xri_Reg: 
         addHRegUse(u, HRmRead, op->Xri.Reg.reg);
         return;
      default: 
         vpanic("addRegUsage_X86RI");
   }
}

static void mapRegs_X86RI ( HRegRemap* m, X86RI* op ) {
   switch (op->tag) {
      case Xri_Imm: 
         return;
      case Xri_Reg: 
         op->Xri.Reg.reg = lookupHRegRemap(m, op->Xri.Reg.reg);
         return;
      default: 
         vpanic("mapRegs_X86RI");
   }
}


/* --------- Operand, which can be reg or memory only. --------- */

X86RM* X86RM_Reg ( HReg reg ) {
   X86RM* op       = LibVEX_Alloc_inline(sizeof(X86RM));
   op->tag         = Xrm_Reg;
   op->Xrm.Reg.reg = reg;
   return op;
}
X86RM* X86RM_Mem ( X86AMode* am ) {
   X86RM* op      = LibVEX_Alloc_inline(sizeof(X86RM));
   op->tag        = Xrm_Mem;
   op->Xrm.Mem.am = am;
   return op;
}

void ppX86RM ( X86RM* op ) {
   switch (op->tag) {
      case Xrm_Mem: 
         ppX86AMode(op->Xrm.Mem.am);
         return;
      case Xrm_Reg: 
         ppHRegX86(op->Xrm.Reg.reg);
         return;
     default: 
         vpanic("ppX86RM");
   }
}

/* Because an X86RM can be both a source or destination operand, we
   have to supply a mode -- pertaining to the operand as a whole --
   indicating how it's being used. */
static void addRegUsage_X86RM ( HRegUsage* u, X86RM* op, HRegMode mode ) {
   switch (op->tag) {
      case Xrm_Mem: 
         /* Memory is read, written or modified.  So we just want to
            know the regs read by the amode. */
         addRegUsage_X86AMode(u, op->Xrm.Mem.am);
         return;
      case Xrm_Reg: 
         /* reg is read, written or modified.  Add it in the
            appropriate way. */
         addHRegUse(u, mode, op->Xrm.Reg.reg);
         return;
     default: 
         vpanic("addRegUsage_X86RM");
   }
}

static void mapRegs_X86RM ( HRegRemap* m, X86RM* op )
{
   switch (op->tag) {
      case Xrm_Mem: 
         mapRegs_X86AMode(m, op->Xrm.Mem.am);
         return;
      case Xrm_Reg: 
         op->Xrm.Reg.reg = lookupHRegRemap(m, op->Xrm.Reg.reg);
         return;
     default: 
         vpanic("mapRegs_X86RM");
   }
}


/* --------- Instructions. --------- */

const HChar* showX86UnaryOp ( X86UnaryOp op ) {
   switch (op) {
      case Xun_NOT: return "not";
      case Xun_NEG: return "neg";
      default: vpanic("showX86UnaryOp");
   }
}

const HChar* showX86AluOp ( X86AluOp op ) {
   switch (op) {
      case Xalu_MOV:  return "mov";
      case Xalu_CMP:  return "cmp";
      case Xalu_ADD:  return "add";
      case Xalu_SUB:  return "sub";
      case Xalu_ADC:  return "adc";
      case Xalu_SBB:  return "sbb";
      case Xalu_AND:  return "and";
      case Xalu_OR:   return "or";
      case Xalu_XOR:  return "xor";
      case Xalu_MUL:  return "mul";
      default: vpanic("showX86AluOp");
   }
}

const HChar* showX86ShiftOp ( X86ShiftOp op ) {
   switch (op) {
      case Xsh_SHL: return "shl";
      case Xsh_SHR: return "shr";
      case Xsh_SAR: return "sar";
      default: vpanic("showX86ShiftOp");
   }
}

const HChar* showX86FpOp ( X86FpOp op ) {
   switch (op) {
      case Xfp_ADD:    return "add";
      case Xfp_SUB:    return "sub";
      case Xfp_MUL:    return "mul";
      case Xfp_DIV:    return "div";
      case Xfp_SCALE:  return "scale";
      case Xfp_ATAN:   return "atan";
      case Xfp_YL2X:   return "yl2x";
      case Xfp_YL2XP1: return "yl2xp1";
      case Xfp_PREM:   return "prem";
      case Xfp_PREM1:  return "prem1";
      case Xfp_SQRT:   return "sqrt";
      case Xfp_ABS:    return "abs";
      case Xfp_NEG:    return "chs";
      case Xfp_MOV:    return "mov";
      case Xfp_SIN:    return "sin";
      case Xfp_COS:    return "cos";
      case Xfp_TAN:    return "tan";
      case Xfp_ROUND:  return "round";
      case Xfp_2XM1:   return "2xm1";
      default: vpanic("showX86FpOp");
   }
}

const HChar* showX86SseOp ( X86SseOp op ) {
   switch (op) {
      case Xsse_MOV:      return "mov(?!)";
      case Xsse_ADDF:     return "add";
      case Xsse_SUBF:     return "sub";
      case Xsse_MULF:     return "mul";
      case Xsse_DIVF:     return "div";
      case Xsse_MAXF:     return "max";
      case Xsse_MINF:     return "min";
      case Xsse_CMPEQF:   return "cmpFeq";
      case Xsse_CMPLTF:   return "cmpFlt";
      case Xsse_CMPLEF:   return "cmpFle";
      case Xsse_CMPUNF:   return "cmpFun";
      case Xsse_RCPF:     return "rcp";
      case Xsse_RSQRTF:   return "rsqrt";
      case Xsse_SQRTF:    return "sqrt";
      case Xsse_AND:      return "and";
      case Xsse_OR:       return "or";
      case Xsse_XOR:      return "xor";
      case Xsse_ANDN:     return "andn";
      case Xsse_ADD8:     return "paddb";
      case Xsse_ADD16:    return "paddw";
      case Xsse_ADD32:    return "paddd";
      case Xsse_ADD64:    return "paddq";
      case Xsse_QADD8U:   return "paddusb";
      case Xsse_QADD16U:  return "paddusw";
      case Xsse_QADD8S:   return "paddsb";
      case Xsse_QADD16S:  return "paddsw";
      case Xsse_SUB8:     return "psubb";
      case Xsse_SUB16:    return "psubw";
      case Xsse_SUB32:    return "psubd";
      case Xsse_SUB64:    return "psubq";
      case Xsse_QSUB8U:   return "psubusb";
      case Xsse_QSUB16U:  return "psubusw";
      case Xsse_QSUB8S:   return "psubsb";
      case Xsse_QSUB16S:  return "psubsw";
      case Xsse_MUL16:    return "pmullw";
      case Xsse_MULHI16U: return "pmulhuw";
      case Xsse_MULHI16S: return "pmulhw";
      case Xsse_AVG8U:    return "pavgb";
      case Xsse_AVG16U:   return "pavgw";
      case Xsse_MAX16S:   return "pmaxw";
      case Xsse_MAX8U:    return "pmaxub";
      case Xsse_MIN16S:   return "pminw";
      case Xsse_MIN8U:    return "pminub";
      case Xsse_CMPEQ8:   return "pcmpeqb";
      case Xsse_CMPEQ16:  return "pcmpeqw";
      case Xsse_CMPEQ32:  return "pcmpeqd";
      case Xsse_CMPGT8S:  return "pcmpgtb";
      case Xsse_CMPGT16S: return "pcmpgtw";
      case Xsse_CMPGT32S: return "pcmpgtd";
      case Xsse_SHL16:    return "psllw";
      case Xsse_SHL32:    return "pslld";
      case Xsse_SHL64:    return "psllq";
      case Xsse_SHR16:    return "psrlw";
      case Xsse_SHR32:    return "psrld";
      case Xsse_SHR64:    return "psrlq";
      case Xsse_SAR16:    return "psraw";
      case Xsse_SAR32:    return "psrad";
      case Xsse_PACKSSD:  return "packssdw";
      case Xsse_PACKSSW:  return "packsswb";
      case Xsse_PACKUSW:  return "packuswb";
      case Xsse_UNPCKHB:  return "punpckhb";
      case Xsse_UNPCKHW:  return "punpckhw";
      case Xsse_UNPCKHD:  return "punpckhd";
      case Xsse_UNPCKHQ:  return "punpckhq";
      case Xsse_UNPCKLB:  return "punpcklb";
      case Xsse_UNPCKLW:  return "punpcklw";
      case Xsse_UNPCKLD:  return "punpckld";
      case Xsse_UNPCKLQ:  return "punpcklq";
      default: vpanic("showX86SseOp");
   }
}

X86Instr* X86Instr_Alu32R ( X86AluOp op, X86RMI* src, HReg dst ) {
   X86Instr* i       = LibVEX_Alloc_inline(sizeof(X86Instr));
   i->tag            = Xin_Alu32R;
   i->Xin.Alu32R.op  = op;
   i->Xin.Alu32R.src = src;
   i->Xin.Alu32R.dst = dst;
   return i;
}
X86Instr* X86Instr_Alu32M ( X86AluOp op, X86RI* src, X86AMode* dst ) {
   X86Instr* i       = LibVEX_Alloc_inline(sizeof(X86Instr));
   i->tag            = Xin_Alu32M;
   i->Xin.Alu32M.op  = op;
   i->Xin.Alu32M.src = src;
   i->Xin.Alu32M.dst = dst;
   vassert(op != Xalu_MUL);
   return i;
}
X86Instr* X86Instr_Sh32 ( X86ShiftOp op, UInt src, HReg dst ) {
   X86Instr* i     = LibVEX_Alloc_inline(sizeof(X86Instr));
   i->tag          = Xin_Sh32;
   i->Xin.Sh32.op  = op;
   i->Xin.Sh32.src = src;
   i->Xin.Sh32.dst = dst;
   return i;
}
X86Instr* X86Instr_Test32 ( UInt imm32, X86RM* dst ) {
   X86Instr* i         = LibVEX_Alloc_inline(sizeof(X86Instr));
   i->tag              = Xin_Test32;
   i->Xin.Test32.imm32 = imm32;
   i->Xin.Test32.dst   = dst;
   return i;
}
X86Instr* X86Instr_Unary32 ( X86UnaryOp op, HReg dst ) {
   X86Instr* i        = LibVEX_Alloc_inline(sizeof(X86Instr));
   i->tag             = Xin_Unary32;
   i->Xin.Unary32.op  = op;
   i->Xin.Unary32.dst = dst;
   return i;
}
X86Instr* X86Instr_Lea32 ( X86AMode* am, HReg dst ) {
   X86Instr* i        = LibVEX_Alloc_inline(sizeof(X86Instr));
   i->tag             = Xin_Lea32;
   i->Xin.Lea32.am    = am;
   i->Xin.Lea32.dst   = dst;
   return i;
}
X86Instr* X86Instr_MulL ( Bool syned, X86RM* src ) {
   X86Instr* i        = LibVEX_Alloc_inline(sizeof(X86Instr));
   i->tag             = Xin_MulL;
   i->Xin.MulL.syned  = syned;
   i->Xin.MulL.src    = src;
   return i;
}
X86Instr* X86Instr_Div ( Bool syned, X86RM* src ) {
   X86Instr* i      = LibVEX_Alloc_inline(sizeof(X86Instr));
   i->tag           = Xin_Div;
   i->Xin.Div.syned = syned;
   i->Xin.Div.src   = src;
   return i;
}
X86Instr* X86Instr_Sh3232  ( X86ShiftOp op, UInt amt, HReg src, HReg dst ) {
   X86Instr* i       = LibVEX_Alloc_inline(sizeof(X86Instr));
   i->tag            = Xin_Sh3232;
   i->Xin.Sh3232.op  = op;
   i->Xin.Sh3232.amt = amt;
   i->Xin.Sh3232.src = src;
   i->Xin.Sh3232.dst = dst;
   vassert(op == Xsh_SHL || op == Xsh_SHR);
   return i;
}
X86Instr* X86Instr_Push( X86RMI* src ) {
   X86Instr* i     = LibVEX_Alloc_inline(sizeof(X86Instr));
   i->tag          = Xin_Push;
   i->Xin.Push.src = src;
   return i;
}
X86Instr* X86Instr_Call ( X86CondCode cond, Addr32 target, Int regparms,
                          RetLoc rloc ) {
   X86Instr* i          = LibVEX_Alloc_inline(sizeof(X86Instr));
   i->tag               = Xin_Call;
   i->Xin.Call.cond     = cond;
   i->Xin.Call.target   = target;
   i->Xin.Call.regparms = regparms;
   i->Xin.Call.rloc     = rloc;
   vassert(regparms >= 0 && regparms <= 3);
   vassert(is_sane_RetLoc(rloc));
   return i;
}
X86Instr* X86Instr_XDirect ( Addr32 dstGA, X86AMode* amEIP,
                             X86CondCode cond, Bool toFastEP ) {
   X86Instr* i             = LibVEX_Alloc_inline(sizeof(X86Instr));
   i->tag                  = Xin_XDirect;
   i->Xin.XDirect.dstGA    = dstGA;
   i->Xin.XDirect.amEIP    = amEIP;
   i->Xin.XDirect.cond     = cond;
   i->Xin.XDirect.toFastEP = toFastEP;
   return i;
}
X86Instr* X86Instr_XIndir ( HReg dstGA, X86AMode* amEIP,
                            X86CondCode cond ) {
   X86Instr* i         = LibVEX_Alloc_inline(sizeof(X86Instr));
   i->tag              = Xin_XIndir;
   i->Xin.XIndir.dstGA = dstGA;
   i->Xin.XIndir.amEIP = amEIP;
   i->Xin.XIndir.cond  = cond;
   return i;
}
X86Instr* X86Instr_XAssisted ( HReg dstGA, X86AMode* amEIP,
                               X86CondCode cond, IRJumpKind jk ) {
   X86Instr* i            = LibVEX_Alloc_inline(sizeof(X86Instr));
   i->tag                 = Xin_XAssisted;
   i->Xin.XAssisted.dstGA = dstGA;
   i->Xin.XAssisted.amEIP = amEIP;
   i->Xin.XAssisted.cond  = cond;
   i->Xin.XAssisted.jk    = jk;
   return i;
}
X86Instr* X86Instr_CMov32  ( X86CondCode cond, X86RM* src, HReg dst ) {
   X86Instr* i        = LibVEX_Alloc_inline(sizeof(X86Instr));
   i->tag             = Xin_CMov32;
   i->Xin.CMov32.cond = cond;
   i->Xin.CMov32.src  = src;
   i->Xin.CMov32.dst  = dst;
   vassert(cond != Xcc_ALWAYS);
   return i;
}
X86Instr* X86Instr_LoadEX ( UChar szSmall, Bool syned,
                            X86AMode* src, HReg dst ) {
   X86Instr* i           = LibVEX_Alloc_inline(sizeof(X86Instr));
   i->tag                = Xin_LoadEX;
   i->Xin.LoadEX.szSmall = szSmall;
   i->Xin.LoadEX.syned   = syned;
   i->Xin.LoadEX.src     = src;
   i->Xin.LoadEX.dst     = dst;
   vassert(szSmall == 1 || szSmall == 2);
   return i;
}
X86Instr* X86Instr_Store ( UChar sz, HReg src, X86AMode* dst ) {
   X86Instr* i      = LibVEX_Alloc_inline(sizeof(X86Instr));
   i->tag           = Xin_Store;
   i->Xin.Store.sz  = sz;
   i->Xin.Store.src = src;
   i->Xin.Store.dst = dst;
   vassert(sz == 1 || sz == 2);
   return i;
}
X86Instr* X86Instr_Set32 ( X86CondCode cond, HReg dst ) {
   X86Instr* i       = LibVEX_Alloc_inline(sizeof(X86Instr));
   i->tag            = Xin_Set32;
   i->Xin.Set32.cond = cond;
   i->Xin.Set32.dst  = dst;
   return i;
}
X86Instr* X86Instr_Bsfr32 ( Bool isFwds, HReg src, HReg dst ) {
   X86Instr* i          = LibVEX_Alloc_inline(sizeof(X86Instr));
   i->tag               = Xin_Bsfr32;
   i->Xin.Bsfr32.isFwds = isFwds;
   i->Xin.Bsfr32.src    = src;
   i->Xin.Bsfr32.dst    = dst;
   return i;
}
X86Instr* X86Instr_MFence ( UInt hwcaps ) {
   X86Instr* i          = LibVEX_Alloc_inline(sizeof(X86Instr));
   i->tag               = Xin_MFence;
   i->Xin.MFence.hwcaps = hwcaps;
   vassert(0 == (hwcaps & ~(VEX_HWCAPS_X86_MMXEXT
                            |VEX_HWCAPS_X86_SSE1
                            |VEX_HWCAPS_X86_SSE2
                            |VEX_HWCAPS_X86_SSE3
                            |VEX_HWCAPS_X86_LZCNT)));
   return i;
}
X86Instr* X86Instr_ACAS ( X86AMode* addr, UChar sz ) {
   X86Instr* i      = LibVEX_Alloc_inline(sizeof(X86Instr));
   i->tag           = Xin_ACAS;
   i->Xin.ACAS.addr = addr;
   i->Xin.ACAS.sz   = sz;
   vassert(sz == 4 || sz == 2 || sz == 1);
   return i;
}
X86Instr* X86Instr_DACAS ( X86AMode* addr ) {
   X86Instr* i       = LibVEX_Alloc_inline(sizeof(X86Instr));
   i->tag            = Xin_DACAS;
   i->Xin.DACAS.addr = addr;
   return i;
}

X86Instr* X86Instr_FpUnary ( X86FpOp op, HReg src, HReg dst ) {
   X86Instr* i        = LibVEX_Alloc_inline(sizeof(X86Instr));
   i->tag             = Xin_FpUnary;
   i->Xin.FpUnary.op  = op;
   i->Xin.FpUnary.src = src;
   i->Xin.FpUnary.dst = dst;
   return i;
}
X86Instr* X86Instr_FpBinary ( X86FpOp op, HReg srcL, HReg srcR, HReg dst ) {
   X86Instr* i          = LibVEX_Alloc_inline(sizeof(X86Instr));
   i->tag               = Xin_FpBinary;
   i->Xin.FpBinary.op   = op;
   i->Xin.FpBinary.srcL = srcL;
   i->Xin.FpBinary.srcR = srcR;
   i->Xin.FpBinary.dst  = dst;
   return i;
}
X86Instr* X86Instr_FpLdSt ( Bool isLoad, UChar sz, HReg reg, X86AMode* addr ) {
   X86Instr* i          = LibVEX_Alloc_inline(sizeof(X86Instr));
   i->tag               = Xin_FpLdSt;
   i->Xin.FpLdSt.isLoad = isLoad;
   i->Xin.FpLdSt.sz     = sz;
   i->Xin.FpLdSt.reg    = reg;
   i->Xin.FpLdSt.addr   = addr;
   vassert(sz == 4 || sz == 8 || sz == 10);
   return i;
}
X86Instr* X86Instr_FpLdStI ( Bool isLoad, UChar sz,  
                             HReg reg, X86AMode* addr ) {
   X86Instr* i           = LibVEX_Alloc_inline(sizeof(X86Instr));
   i->tag                = Xin_FpLdStI;
   i->Xin.FpLdStI.isLoad = isLoad;
   i->Xin.FpLdStI.sz     = sz;
   i->Xin.FpLdStI.reg    = reg;
   i->Xin.FpLdStI.addr   = addr;
   vassert(sz == 2 || sz == 4 || sz == 8);
   return i;
}
X86Instr* X86Instr_Fp64to32 ( HReg src, HReg dst ) {
   X86Instr* i         = LibVEX_Alloc_inline(sizeof(X86Instr));
   i->tag              = Xin_Fp64to32;
   i->Xin.Fp64to32.src = src;
   i->Xin.Fp64to32.dst = dst;
   return i;
}
X86Instr* X86Instr_FpCMov ( X86CondCode cond, HReg src, HReg dst ) {
   X86Instr* i        = LibVEX_Alloc_inline(sizeof(X86Instr));
   i->tag             = Xin_FpCMov;
   i->Xin.FpCMov.cond = cond;
   i->Xin.FpCMov.src  = src;
   i->Xin.FpCMov.dst  = dst;
   vassert(cond != Xcc_ALWAYS);
   return i;
}
X86Instr* X86Instr_FpLdCW ( X86AMode* addr ) {
   X86Instr* i          = LibVEX_Alloc_inline(sizeof(X86Instr));
   i->tag               = Xin_FpLdCW;
   i->Xin.FpLdCW.addr   = addr;
   return i;
}
X86Instr* X86Instr_FpStSW_AX ( void ) {
   X86Instr* i = LibVEX_Alloc_inline(sizeof(X86Instr));
   i->tag      = Xin_FpStSW_AX;
   return i;
}
X86Instr* X86Instr_FpCmp ( HReg srcL, HReg srcR, HReg dst ) {
   X86Instr* i       = LibVEX_Alloc_inline(sizeof(X86Instr));
   i->tag            = Xin_FpCmp;
   i->Xin.FpCmp.srcL = srcL;
   i->Xin.FpCmp.srcR = srcR;
   i->Xin.FpCmp.dst  = dst;
   return i;
}
X86Instr* X86Instr_SseConst ( UShort con, HReg dst ) {
   X86Instr* i            = LibVEX_Alloc_inline(sizeof(X86Instr));
   i->tag                 = Xin_SseConst;
   i->Xin.SseConst.con    = con;
   i->Xin.SseConst.dst    = dst;
   vassert(hregClass(dst) == HRcVec128);
   return i;
}
X86Instr* X86Instr_SseLdSt ( Bool isLoad, HReg reg, X86AMode* addr ) {
   X86Instr* i           = LibVEX_Alloc_inline(sizeof(X86Instr));
   i->tag                = Xin_SseLdSt;
   i->Xin.SseLdSt.isLoad = isLoad;
   i->Xin.SseLdSt.reg    = reg;
   i->Xin.SseLdSt.addr   = addr;
   return i;
}
X86Instr* X86Instr_SseLdzLO  ( Int sz, HReg reg, X86AMode* addr )
{
   X86Instr* i           = LibVEX_Alloc_inline(sizeof(X86Instr));
   i->tag                = Xin_SseLdzLO;
   i->Xin.SseLdzLO.sz    = toUChar(sz);
   i->Xin.SseLdzLO.reg   = reg;
   i->Xin.SseLdzLO.addr  = addr;
   vassert(sz == 4 || sz == 8);
   return i;
}
X86Instr* X86Instr_Sse32Fx4 ( X86SseOp op, HReg src, HReg dst ) {
   X86Instr* i         = LibVEX_Alloc_inline(sizeof(X86Instr));
   i->tag              = Xin_Sse32Fx4;
   i->Xin.Sse32Fx4.op  = op;
   i->Xin.Sse32Fx4.src = src;
   i->Xin.Sse32Fx4.dst = dst;
   vassert(op != Xsse_MOV);
   return i;
}
X86Instr* X86Instr_Sse32FLo ( X86SseOp op, HReg src, HReg dst ) {
   X86Instr* i         = LibVEX_Alloc_inline(sizeof(X86Instr));
   i->tag              = Xin_Sse32FLo;
   i->Xin.Sse32FLo.op  = op;
   i->Xin.Sse32FLo.src = src;
   i->Xin.Sse32FLo.dst = dst;
   vassert(op != Xsse_MOV);
   return i;
}
X86Instr* X86Instr_Sse64Fx2 ( X86SseOp op, HReg src, HReg dst ) {
   X86Instr* i         = LibVEX_Alloc_inline(sizeof(X86Instr));
   i->tag              = Xin_Sse64Fx2;
   i->Xin.Sse64Fx2.op  = op;
   i->Xin.Sse64Fx2.src = src;
   i->Xin.Sse64Fx2.dst = dst;
   vassert(op != Xsse_MOV);
   return i;
}
X86Instr* X86Instr_Sse64FLo ( X86SseOp op, HReg src, HReg dst ) {
   X86Instr* i         = LibVEX_Alloc_inline(sizeof(X86Instr));
   i->tag              = Xin_Sse64FLo;
   i->Xin.Sse64FLo.op  = op;
   i->Xin.Sse64FLo.src = src;
   i->Xin.Sse64FLo.dst = dst;
   vassert(op != Xsse_MOV);
   return i;
}
X86Instr* X86Instr_SseReRg ( X86SseOp op, HReg re, HReg rg ) {
   X86Instr* i        = LibVEX_Alloc_inline(sizeof(X86Instr));
   i->tag             = Xin_SseReRg;
   i->Xin.SseReRg.op  = op;
   i->Xin.SseReRg.src = re;
   i->Xin.SseReRg.dst = rg;
   return i;
}
X86Instr* X86Instr_SseCMov ( X86CondCode cond, HReg src, HReg dst ) {
   X86Instr* i         = LibVEX_Alloc_inline(sizeof(X86Instr));
   i->tag              = Xin_SseCMov;
   i->Xin.SseCMov.cond = cond;
   i->Xin.SseCMov.src  = src;
   i->Xin.SseCMov.dst  = dst;
   vassert(cond != Xcc_ALWAYS);
   return i;
}
X86Instr* X86Instr_SseShuf ( Int order, HReg src, HReg dst ) {
   X86Instr* i          = LibVEX_Alloc_inline(sizeof(X86Instr));
   i->tag               = Xin_SseShuf;
   i->Xin.SseShuf.order = order;
   i->Xin.SseShuf.src   = src;
   i->Xin.SseShuf.dst   = dst;
   vassert(order >= 0 && order <= 0xFF);
   return i;
}
X86Instr* X86Instr_EvCheck ( X86AMode* amCounter,
                             X86AMode* amFailAddr ) {
   X86Instr* i               = LibVEX_Alloc_inline(sizeof(X86Instr));
   i->tag                    = Xin_EvCheck;
   i->Xin.EvCheck.amCounter  = amCounter;
   i->Xin.EvCheck.amFailAddr = amFailAddr;
   return i;
}
X86Instr* X86Instr_ProfInc ( void ) {
   X86Instr* i = LibVEX_Alloc_inline(sizeof(X86Instr));
   i->tag      = Xin_ProfInc;
   return i;
}

void ppX86Instr ( const X86Instr* i, Bool mode64 ) {
   vassert(mode64 == False);
   switch (i->tag) {
      case Xin_Alu32R:
         vex_printf("%sl ", showX86AluOp(i->Xin.Alu32R.op));
         ppX86RMI(i->Xin.Alu32R.src);
         vex_printf(",");
         ppHRegX86(i->Xin.Alu32R.dst);
         return;
      case Xin_Alu32M:
         vex_printf("%sl ", showX86AluOp(i->Xin.Alu32M.op));
         ppX86RI(i->Xin.Alu32M.src);
         vex_printf(",");
         ppX86AMode(i->Xin.Alu32M.dst);
         return;
      case Xin_Sh32:
         vex_printf("%sl ", showX86ShiftOp(i->Xin.Sh32.op));
         if (i->Xin.Sh32.src == 0)
           vex_printf("%%cl,"); 
         else 
            vex_printf("$%d,", (Int)i->Xin.Sh32.src);
         ppHRegX86(i->Xin.Sh32.dst);
         return;
      case Xin_Test32:
         vex_printf("testl $%d,", (Int)i->Xin.Test32.imm32);
         ppX86RM(i->Xin.Test32.dst);
         return;
      case Xin_Unary32:
         vex_printf("%sl ", showX86UnaryOp(i->Xin.Unary32.op));
         ppHRegX86(i->Xin.Unary32.dst);
         return;
      case Xin_Lea32:
         vex_printf("leal ");
         ppX86AMode(i->Xin.Lea32.am);
         vex_printf(",");
         ppHRegX86(i->Xin.Lea32.dst);
         return;
      case Xin_MulL:
         vex_printf("%cmull ", i->Xin.MulL.syned ? 's' : 'u');
         ppX86RM(i->Xin.MulL.src);
         return;
      case Xin_Div:
         vex_printf("%cdivl ", i->Xin.Div.syned ? 's' : 'u');
         ppX86RM(i->Xin.Div.src);
         return;
      case Xin_Sh3232:
         vex_printf("%sdl ", showX86ShiftOp(i->Xin.Sh3232.op));
         if (i->Xin.Sh3232.amt == 0)
           vex_printf(" %%cl,"); 
         else 
            vex_printf(" $%d,", (Int)i->Xin.Sh3232.amt);
         ppHRegX86(i->Xin.Sh3232.src);
         vex_printf(",");
         ppHRegX86(i->Xin.Sh3232.dst);
         return;
      case Xin_Push:
         vex_printf("pushl ");
         ppX86RMI(i->Xin.Push.src);
         return;
      case Xin_Call:
         vex_printf("call%s[%d,", 
                    i->Xin.Call.cond==Xcc_ALWAYS 
                       ? "" : showX86CondCode(i->Xin.Call.cond), 
                    i->Xin.Call.regparms);
         ppRetLoc(i->Xin.Call.rloc);
         vex_printf("] 0x%x", i->Xin.Call.target);
         break;
      case Xin_XDirect:
         vex_printf("(xDirect) ");
         vex_printf("if (%%eflags.%s) { ",
                    showX86CondCode(i->Xin.XDirect.cond));
         vex_printf("movl $0x%x,", i->Xin.XDirect.dstGA);
         ppX86AMode(i->Xin.XDirect.amEIP);
         vex_printf("; ");
         vex_printf("movl $disp_cp_chain_me_to_%sEP,%%edx; call *%%edx }",
                    i->Xin.XDirect.toFastEP ? "fast" : "slow");
         return;
      case Xin_XIndir:
         vex_printf("(xIndir) ");
         vex_printf("if (%%eflags.%s) { movl ",
                    showX86CondCode(i->Xin.XIndir.cond));
         ppHRegX86(i->Xin.XIndir.dstGA);
         vex_printf(",");
         ppX86AMode(i->Xin.XIndir.amEIP);
         vex_printf("; movl $disp_indir,%%edx; jmp *%%edx }");
         return;
      case Xin_XAssisted:
         vex_printf("(xAssisted) ");
         vex_printf("if (%%eflags.%s) { ",
                    showX86CondCode(i->Xin.XAssisted.cond));
         vex_printf("movl ");
         ppHRegX86(i->Xin.XAssisted.dstGA);
         vex_printf(",");
         ppX86AMode(i->Xin.XAssisted.amEIP);
         vex_printf("; movl $IRJumpKind_to_TRCVAL(%d),%%ebp",
                    (Int)i->Xin.XAssisted.jk);
         vex_printf("; movl $disp_assisted,%%edx; jmp *%%edx }");
         return;
      case Xin_CMov32:
         vex_printf("cmov%s ", showX86CondCode(i->Xin.CMov32.cond));
         ppX86RM(i->Xin.CMov32.src);
         vex_printf(",");
         ppHRegX86(i->Xin.CMov32.dst);
         return;
      case Xin_LoadEX:
         vex_printf("mov%c%cl ",
                    i->Xin.LoadEX.syned ? 's' : 'z',
                    i->Xin.LoadEX.szSmall==1 ? 'b' : 'w');
         ppX86AMode(i->Xin.LoadEX.src);
         vex_printf(",");
         ppHRegX86(i->Xin.LoadEX.dst);
         return;
      case Xin_Store:
         vex_printf("mov%c ", i->Xin.Store.sz==1 ? 'b' : 'w');
         ppHRegX86(i->Xin.Store.src);
         vex_printf(",");
         ppX86AMode(i->Xin.Store.dst);
         return;
      case Xin_Set32:
         vex_printf("setl%s ", showX86CondCode(i->Xin.Set32.cond));
         ppHRegX86(i->Xin.Set32.dst);
         return;
      case Xin_Bsfr32:
         vex_printf("bs%cl ", i->Xin.Bsfr32.isFwds ? 'f' : 'r');
         ppHRegX86(i->Xin.Bsfr32.src);
         vex_printf(",");
         ppHRegX86(i->Xin.Bsfr32.dst);
         return;
      case Xin_MFence:
         vex_printf("mfence(%s)",
                    LibVEX_ppVexHwCaps(VexArchX86,i->Xin.MFence.hwcaps));
         return;
      case Xin_ACAS:
         vex_printf("lock cmpxchg%c ",
                     i->Xin.ACAS.sz==1 ? 'b' 
                                       : i->Xin.ACAS.sz==2 ? 'w' : 'l');
         vex_printf("{%%eax->%%ebx},");
         ppX86AMode(i->Xin.ACAS.addr);
         return;
      case Xin_DACAS:
         vex_printf("lock cmpxchg8b {%%edx:%%eax->%%ecx:%%ebx},");
         ppX86AMode(i->Xin.DACAS.addr);
         return;
      case Xin_FpUnary:
         vex_printf("g%sD ", showX86FpOp(i->Xin.FpUnary.op));
         ppHRegX86(i->Xin.FpUnary.src);
         vex_printf(",");
         ppHRegX86(i->Xin.FpUnary.dst);
         break;
      case Xin_FpBinary:
         vex_printf("g%sD ", showX86FpOp(i->Xin.FpBinary.op));
         ppHRegX86(i->Xin.FpBinary.srcL);
         vex_printf(",");
         ppHRegX86(i->Xin.FpBinary.srcR);
         vex_printf(",");
         ppHRegX86(i->Xin.FpBinary.dst);
         break;
      case Xin_FpLdSt:
         if (i->Xin.FpLdSt.isLoad) {
            vex_printf("gld%c " ,  i->Xin.FpLdSt.sz==10 ? 'T'
                                   : (i->Xin.FpLdSt.sz==8 ? 'D' : 'F'));
            ppX86AMode(i->Xin.FpLdSt.addr);
            vex_printf(", ");
            ppHRegX86(i->Xin.FpLdSt.reg);
         } else {
            vex_printf("gst%c " , i->Xin.FpLdSt.sz==10 ? 'T'
                                  : (i->Xin.FpLdSt.sz==8 ? 'D' : 'F'));
            ppHRegX86(i->Xin.FpLdSt.reg);
            vex_printf(", ");
            ppX86AMode(i->Xin.FpLdSt.addr);
         }
         return;
      case Xin_FpLdStI:
         if (i->Xin.FpLdStI.isLoad) {
            vex_printf("gild%s ", i->Xin.FpLdStI.sz==8 ? "ll" : 
                                  i->Xin.FpLdStI.sz==4 ? "l" : "w");
            ppX86AMode(i->Xin.FpLdStI.addr);
            vex_printf(", ");
            ppHRegX86(i->Xin.FpLdStI.reg);
         } else {
            vex_printf("gist%s ", i->Xin.FpLdStI.sz==8 ? "ll" : 
                                  i->Xin.FpLdStI.sz==4 ? "l" : "w");
            ppHRegX86(i->Xin.FpLdStI.reg);
            vex_printf(", ");
            ppX86AMode(i->Xin.FpLdStI.addr);
         }
         return;
      case Xin_Fp64to32:
         vex_printf("gdtof ");
         ppHRegX86(i->Xin.Fp64to32.src);
         vex_printf(",");
         ppHRegX86(i->Xin.Fp64to32.dst);
         return;
      case Xin_FpCMov:
         vex_printf("gcmov%s ", showX86CondCode(i->Xin.FpCMov.cond));
         ppHRegX86(i->Xin.FpCMov.src);
         vex_printf(",");
         ppHRegX86(i->Xin.FpCMov.dst);
         return;
      case Xin_FpLdCW:
         vex_printf("fldcw ");
         ppX86AMode(i->Xin.FpLdCW.addr);
         return;
      case Xin_FpStSW_AX:
         vex_printf("fstsw %%ax");
         return;
      case Xin_FpCmp:
         vex_printf("gcmp ");
         ppHRegX86(i->Xin.FpCmp.srcL);
         vex_printf(",");
         ppHRegX86(i->Xin.FpCmp.srcR);
         vex_printf(",");
         ppHRegX86(i->Xin.FpCmp.dst);
         break;
      case Xin_SseConst:
         vex_printf("const $0x%04x,", (Int)i->Xin.SseConst.con);
         ppHRegX86(i->Xin.SseConst.dst);
         break;
      case Xin_SseLdSt:
         vex_printf("movups ");
         if (i->Xin.SseLdSt.isLoad) {
            ppX86AMode(i->Xin.SseLdSt.addr);
            vex_printf(",");
            ppHRegX86(i->Xin.SseLdSt.reg);
         } else {
            ppHRegX86(i->Xin.SseLdSt.reg);
            vex_printf(",");
            ppX86AMode(i->Xin.SseLdSt.addr);
         }
         return;
      case Xin_SseLdzLO:
         vex_printf("movs%s ", i->Xin.SseLdzLO.sz==4 ? "s" : "d");
         ppX86AMode(i->Xin.SseLdzLO.addr);
         vex_printf(",");
         ppHRegX86(i->Xin.SseLdzLO.reg);
         return;
      case Xin_Sse32Fx4:
         vex_printf("%sps ", showX86SseOp(i->Xin.Sse32Fx4.op));
         ppHRegX86(i->Xin.Sse32Fx4.src);
         vex_printf(",");
         ppHRegX86(i->Xin.Sse32Fx4.dst);
         return;
      case Xin_Sse32FLo:
         vex_printf("%sss ", showX86SseOp(i->Xin.Sse32FLo.op));
         ppHRegX86(i->Xin.Sse32FLo.src);
         vex_printf(",");
         ppHRegX86(i->Xin.Sse32FLo.dst);
         return;
      case Xin_Sse64Fx2:
         vex_printf("%spd ", showX86SseOp(i->Xin.Sse64Fx2.op));
         ppHRegX86(i->Xin.Sse64Fx2.src);
         vex_printf(",");
         ppHRegX86(i->Xin.Sse64Fx2.dst);
         return;
      case Xin_Sse64FLo:
         vex_printf("%ssd ", showX86SseOp(i->Xin.Sse64FLo.op));
         ppHRegX86(i->Xin.Sse64FLo.src);
         vex_printf(",");
         ppHRegX86(i->Xin.Sse64FLo.dst);
         return;
      case Xin_SseReRg:
         vex_printf("%s ", showX86SseOp(i->Xin.SseReRg.op));
         ppHRegX86(i->Xin.SseReRg.src);
         vex_printf(",");
         ppHRegX86(i->Xin.SseReRg.dst);
         return;
      case Xin_SseCMov:
         vex_printf("cmov%s ", showX86CondCode(i->Xin.SseCMov.cond));
         ppHRegX86(i->Xin.SseCMov.src);
         vex_printf(",");
         ppHRegX86(i->Xin.SseCMov.dst);
         return;
      case Xin_SseShuf:
         vex_printf("pshufd $0x%x,", (UInt)i->Xin.SseShuf.order);
         ppHRegX86(i->Xin.SseShuf.src);
         vex_printf(",");
         ppHRegX86(i->Xin.SseShuf.dst);
         return;
      case Xin_EvCheck:
         vex_printf("(evCheck) decl ");
         ppX86AMode(i->Xin.EvCheck.amCounter);
         vex_printf("; jns nofail; jmp *");
         ppX86AMode(i->Xin.EvCheck.amFailAddr);
         vex_printf("; nofail:");
         return;
      case Xin_ProfInc:
         vex_printf("(profInc) addl $1,NotKnownYet; "
                    "adcl $0,NotKnownYet+4");
         return;
      default:
         vpanic("ppX86Instr");
   }
}

/* --------- Helpers for register allocation. --------- */

void getRegUsage_X86Instr (HRegUsage* u, const X86Instr* i, Bool mode64)
{
   Bool unary;
   vassert(mode64 == False);
   initHRegUsage(u);
   switch (i->tag) {
      case Xin_Alu32R:
         addRegUsage_X86RMI(u, i->Xin.Alu32R.src);
         if (i->Xin.Alu32R.op == Xalu_MOV) {
            addHRegUse(u, HRmWrite, i->Xin.Alu32R.dst);
            return;
         }
         if (i->Xin.Alu32R.op == Xalu_CMP) { 
            addHRegUse(u, HRmRead, i->Xin.Alu32R.dst);
            return;
         }
         addHRegUse(u, HRmModify, i->Xin.Alu32R.dst);
         return;
      case Xin_Alu32M:
         addRegUsage_X86RI(u, i->Xin.Alu32M.src);
         addRegUsage_X86AMode(u, i->Xin.Alu32M.dst);
         return;
      case Xin_Sh32:
         addHRegUse(u, HRmModify, i->Xin.Sh32.dst);
         if (i->Xin.Sh32.src == 0)
            addHRegUse(u, HRmRead, hregX86_ECX());
         return;
      case Xin_Test32:
         addRegUsage_X86RM(u, i->Xin.Test32.dst, HRmRead);
         return;
      case Xin_Unary32:
         addHRegUse(u, HRmModify, i->Xin.Unary32.dst);
         return;
      case Xin_Lea32:
         addRegUsage_X86AMode(u, i->Xin.Lea32.am);
         addHRegUse(u, HRmWrite, i->Xin.Lea32.dst);
         return;
      case Xin_MulL:
         addRegUsage_X86RM(u, i->Xin.MulL.src, HRmRead);
         addHRegUse(u, HRmModify, hregX86_EAX());
         addHRegUse(u, HRmWrite, hregX86_EDX());
         return;
      case Xin_Div:
         addRegUsage_X86RM(u, i->Xin.Div.src, HRmRead);
         addHRegUse(u, HRmModify, hregX86_EAX());
         addHRegUse(u, HRmModify, hregX86_EDX());
         return;
      case Xin_Sh3232:
         addHRegUse(u, HRmRead, i->Xin.Sh3232.src);
         addHRegUse(u, HRmModify, i->Xin.Sh3232.dst);
         if (i->Xin.Sh3232.amt == 0)
            addHRegUse(u, HRmRead, hregX86_ECX());
         return;
      case Xin_Push:
         addRegUsage_X86RMI(u, i->Xin.Push.src);
         addHRegUse(u, HRmModify, hregX86_ESP());
         return;
      case Xin_Call:
         /* This is a bit subtle. */
         /* First off, claim it trashes all the caller-saved regs
            which fall within the register allocator's jurisdiction.
            These I believe to be %eax %ecx %edx and all the xmm
            registers. */
         addHRegUse(u, HRmWrite, hregX86_EAX());
         addHRegUse(u, HRmWrite, hregX86_ECX());
         addHRegUse(u, HRmWrite, hregX86_EDX());
         addHRegUse(u, HRmWrite, hregX86_XMM0());
         addHRegUse(u, HRmWrite, hregX86_XMM1());
         addHRegUse(u, HRmWrite, hregX86_XMM2());
         addHRegUse(u, HRmWrite, hregX86_XMM3());
         addHRegUse(u, HRmWrite, hregX86_XMM4());
         addHRegUse(u, HRmWrite, hregX86_XMM5());
         addHRegUse(u, HRmWrite, hregX86_XMM6());
         addHRegUse(u, HRmWrite, hregX86_XMM7());
         /* Now we have to state any parameter-carrying registers
            which might be read.  This depends on the regparmness. */
         switch (i->Xin.Call.regparms) {
            case 3: addHRegUse(u, HRmRead, hregX86_ECX()); /*fallthru*/
            case 2: addHRegUse(u, HRmRead, hregX86_EDX()); /*fallthru*/
            case 1: addHRegUse(u, HRmRead, hregX86_EAX()); break;
            case 0: break;
            default: vpanic("getRegUsage_X86Instr:Call:regparms");
         }
         /* Finally, there is the issue that the insn trashes a
            register because the literal target address has to be
            loaded into a register.  Fortunately, for the 0/1/2
            regparm case, we can use EAX, EDX and ECX respectively, so
            this does not cause any further damage.  For the 3-regparm
            case, we'll have to choose another register arbitrarily --
            since A, D and C are used for parameters -- and so we might
            as well choose EDI. */
         if (i->Xin.Call.regparms == 3)
            addHRegUse(u, HRmWrite, hregX86_EDI());
         /* Upshot of this is that the assembler really must observe
            the here-stated convention of which register to use as an
            address temporary, depending on the regparmness: 0==EAX,
            1==EDX, 2==ECX, 3==EDI. */
         return;
      /* XDirect/XIndir/XAssisted are also a bit subtle.  They
         conditionally exit the block.  Hence we only need to list (1)
         the registers that they read, and (2) the registers that they
         write in the case where the block is not exited.  (2) is
         empty, hence only (1) is relevant here. */
      case Xin_XDirect:
         addRegUsage_X86AMode(u, i->Xin.XDirect.amEIP);
         return;
      case Xin_XIndir:
         addHRegUse(u, HRmRead, i->Xin.XIndir.dstGA);
         addRegUsage_X86AMode(u, i->Xin.XIndir.amEIP);
         return;
      case Xin_XAssisted:
         addHRegUse(u, HRmRead, i->Xin.XAssisted.dstGA);
         addRegUsage_X86AMode(u, i->Xin.XAssisted.amEIP);
         return;
      case Xin_CMov32:
         addRegUsage_X86RM(u, i->Xin.CMov32.src, HRmRead);
         addHRegUse(u, HRmModify, i->Xin.CMov32.dst);
         return;
      case Xin_LoadEX:
         addRegUsage_X86AMode(u, i->Xin.LoadEX.src);
         addHRegUse(u, HRmWrite, i->Xin.LoadEX.dst);
         return;
      case Xin_Store:
         addHRegUse(u, HRmRead, i->Xin.Store.src);
         addRegUsage_X86AMode(u, i->Xin.Store.dst);
         return;
      case Xin_Set32:
         addHRegUse(u, HRmWrite, i->Xin.Set32.dst);
         return;
      case Xin_Bsfr32:
         addHRegUse(u, HRmRead, i->Xin.Bsfr32.src);
         addHRegUse(u, HRmWrite, i->Xin.Bsfr32.dst);
         return;
      case Xin_MFence:
         return;
      case Xin_ACAS:
         addRegUsage_X86AMode(u, i->Xin.ACAS.addr);
         addHRegUse(u, HRmRead, hregX86_EBX());
         addHRegUse(u, HRmModify, hregX86_EAX());
         return;
      case Xin_DACAS:
         addRegUsage_X86AMode(u, i->Xin.DACAS.addr);
         addHRegUse(u, HRmRead, hregX86_ECX());
         addHRegUse(u, HRmRead, hregX86_EBX());
         addHRegUse(u, HRmModify, hregX86_EDX());
         addHRegUse(u, HRmModify, hregX86_EAX());
         return;
      case Xin_FpUnary:
         addHRegUse(u, HRmRead, i->Xin.FpUnary.src);
         addHRegUse(u, HRmWrite, i->Xin.FpUnary.dst);
         return;
      case Xin_FpBinary:
         addHRegUse(u, HRmRead, i->Xin.FpBinary.srcL);
         addHRegUse(u, HRmRead, i->Xin.FpBinary.srcR);
         addHRegUse(u, HRmWrite, i->Xin.FpBinary.dst);
         return;
      case Xin_FpLdSt:
         addRegUsage_X86AMode(u, i->Xin.FpLdSt.addr);
         addHRegUse(u, i->Xin.FpLdSt.isLoad ? HRmWrite : HRmRead,
                       i->Xin.FpLdSt.reg);
         return;
      case Xin_FpLdStI:
         addRegUsage_X86AMode(u, i->Xin.FpLdStI.addr);
         addHRegUse(u, i->Xin.FpLdStI.isLoad ? HRmWrite : HRmRead,
                       i->Xin.FpLdStI.reg);
         return;
      case Xin_Fp64to32:
         addHRegUse(u, HRmRead,  i->Xin.Fp64to32.src);
         addHRegUse(u, HRmWrite, i->Xin.Fp64to32.dst);
         return;
      case Xin_FpCMov:
         addHRegUse(u, HRmRead,   i->Xin.FpCMov.src);
         addHRegUse(u, HRmModify, i->Xin.FpCMov.dst);
         return;
      case Xin_FpLdCW:
         addRegUsage_X86AMode(u, i->Xin.FpLdCW.addr);
         return;
      case Xin_FpStSW_AX:
         addHRegUse(u, HRmWrite, hregX86_EAX());
         return;
      case Xin_FpCmp:
         addHRegUse(u, HRmRead, i->Xin.FpCmp.srcL);
         addHRegUse(u, HRmRead, i->Xin.FpCmp.srcR);
         addHRegUse(u, HRmWrite, i->Xin.FpCmp.dst);
         addHRegUse(u, HRmWrite, hregX86_EAX());
         return;
      case Xin_SseLdSt:
         addRegUsage_X86AMode(u, i->Xin.SseLdSt.addr);
         addHRegUse(u, i->Xin.SseLdSt.isLoad ? HRmWrite : HRmRead,
                       i->Xin.SseLdSt.reg);
         return;
      case Xin_SseLdzLO:
         addRegUsage_X86AMode(u, i->Xin.SseLdzLO.addr);
         addHRegUse(u, HRmWrite, i->Xin.SseLdzLO.reg);
         return;
      case Xin_SseConst:
         addHRegUse(u, HRmWrite, i->Xin.SseConst.dst);
         return;
      case Xin_Sse32Fx4:
         vassert(i->Xin.Sse32Fx4.op != Xsse_MOV);
         unary = toBool( i->Xin.Sse32Fx4.op == Xsse_RCPF
                         || i->Xin.Sse32Fx4.op == Xsse_RSQRTF
                         || i->Xin.Sse32Fx4.op == Xsse_SQRTF );
         addHRegUse(u, HRmRead, i->Xin.Sse32Fx4.src);
         addHRegUse(u, unary ? HRmWrite : HRmModify, 
                       i->Xin.Sse32Fx4.dst);
         return;
      case Xin_Sse32FLo:
         vassert(i->Xin.Sse32FLo.op != Xsse_MOV);
         unary = toBool( i->Xin.Sse32FLo.op == Xsse_RCPF
                         || i->Xin.Sse32FLo.op == Xsse_RSQRTF
                         || i->Xin.Sse32FLo.op == Xsse_SQRTF );
         addHRegUse(u, HRmRead, i->Xin.Sse32FLo.src);
         addHRegUse(u, unary ? HRmWrite : HRmModify, 
                       i->Xin.Sse32FLo.dst);
         return;
      case Xin_Sse64Fx2:
         vassert(i->Xin.Sse64Fx2.op != Xsse_MOV);
         unary = toBool( i->Xin.Sse64Fx2.op == Xsse_RCPF
                         || i->Xin.Sse64Fx2.op == Xsse_RSQRTF
                         || i->Xin.Sse64Fx2.op == Xsse_SQRTF );
         addHRegUse(u, HRmRead, i->Xin.Sse64Fx2.src);
         addHRegUse(u, unary ? HRmWrite : HRmModify, 
                       i->Xin.Sse64Fx2.dst);
         return;
      case Xin_Sse64FLo:
         vassert(i->Xin.Sse64FLo.op != Xsse_MOV);
         unary = toBool( i->Xin.Sse64FLo.op == Xsse_RCPF
                         || i->Xin.Sse64FLo.op == Xsse_RSQRTF
                         || i->Xin.Sse64FLo.op == Xsse_SQRTF );
         addHRegUse(u, HRmRead, i->Xin.Sse64FLo.src);
         addHRegUse(u, unary ? HRmWrite : HRmModify, 
                       i->Xin.Sse64FLo.dst);
         return;
      case Xin_SseReRg:
         if (i->Xin.SseReRg.op == Xsse_XOR
             && sameHReg(i->Xin.SseReRg.src, i->Xin.SseReRg.dst)) {
            /* reg-alloc needs to understand 'xor r,r' as a write of r */
            /* (as opposed to a rite of passage :-) */
            addHRegUse(u, HRmWrite, i->Xin.SseReRg.dst);
         } else {
            addHRegUse(u, HRmRead, i->Xin.SseReRg.src);
            addHRegUse(u, i->Xin.SseReRg.op == Xsse_MOV 
                             ? HRmWrite : HRmModify, 
                          i->Xin.SseReRg.dst);
         }
         return;
      case Xin_SseCMov:
         addHRegUse(u, HRmRead,   i->Xin.SseCMov.src);
         addHRegUse(u, HRmModify, i->Xin.SseCMov.dst);
         return;
      case Xin_SseShuf:
         addHRegUse(u, HRmRead,  i->Xin.SseShuf.src);
         addHRegUse(u, HRmWrite, i->Xin.SseShuf.dst);
         return;
      case Xin_EvCheck:
         /* We expect both amodes only to mention %ebp, so this is in
            fact pointless, since %ebp isn't allocatable, but anyway.. */
         addRegUsage_X86AMode(u, i->Xin.EvCheck.amCounter);
         addRegUsage_X86AMode(u, i->Xin.EvCheck.amFailAddr);
         return;
      case Xin_ProfInc:
         /* does not use any registers. */
         return;
      default:
         ppX86Instr(i, False);
         vpanic("getRegUsage_X86Instr");
   }
}

/* local helper */
static void mapReg( HRegRemap* m, HReg* r )
{
   *r = lookupHRegRemap(m, *r);
}

void mapRegs_X86Instr ( HRegRemap* m, X86Instr* i, Bool mode64 )
{
   vassert(mode64 == False);
   switch (i->tag) {
      case Xin_Alu32R:
         mapRegs_X86RMI(m, i->Xin.Alu32R.src);
         mapReg(m, &i->Xin.Alu32R.dst);
         return;
      case Xin_Alu32M:
         mapRegs_X86RI(m, i->Xin.Alu32M.src);
         mapRegs_X86AMode(m, i->Xin.Alu32M.dst);
         return;
      case Xin_Sh32:
         mapReg(m, &i->Xin.Sh32.dst);
         return;
      case Xin_Test32:
         mapRegs_X86RM(m, i->Xin.Test32.dst);
         return;
      case Xin_Unary32:
         mapReg(m, &i->Xin.Unary32.dst);
         return;
      case Xin_Lea32:
         mapRegs_X86AMode(m, i->Xin.Lea32.am);
         mapReg(m, &i->Xin.Lea32.dst);
         return;
      case Xin_MulL:
         mapRegs_X86RM(m, i->Xin.MulL.src);
         return;
      case Xin_Div:
         mapRegs_X86RM(m, i->Xin.Div.src);
         return;
      case Xin_Sh3232:
         mapReg(m, &i->Xin.Sh3232.src);
         mapReg(m, &i->Xin.Sh3232.dst);
         return;
      case Xin_Push:
         mapRegs_X86RMI(m, i->Xin.Push.src);
         return;
      case Xin_Call:
         return;
      case Xin_XDirect:
         mapRegs_X86AMode(m, i->Xin.XDirect.amEIP);
         return;
      case Xin_XIndir:
         mapReg(m, &i->Xin.XIndir.dstGA);
         mapRegs_X86AMode(m, i->Xin.XIndir.amEIP);
         return;
      case Xin_XAssisted:
         mapReg(m, &i->Xin.XAssisted.dstGA);
         mapRegs_X86AMode(m, i->Xin.XAssisted.amEIP);
         return;
      case Xin_CMov32:
         mapRegs_X86RM(m, i->Xin.CMov32.src);
         mapReg(m, &i->Xin.CMov32.dst);
         return;
      case Xin_LoadEX:
         mapRegs_X86AMode(m, i->Xin.LoadEX.src);
         mapReg(m, &i->Xin.LoadEX.dst);
         return;
      case Xin_Store:
         mapReg(m, &i->Xin.Store.src);
         mapRegs_X86AMode(m, i->Xin.Store.dst);
         return;
      case Xin_Set32:
         mapReg(m, &i->Xin.Set32.dst);
         return;
      case Xin_Bsfr32:
         mapReg(m, &i->Xin.Bsfr32.src);
         mapReg(m, &i->Xin.Bsfr32.dst);
         return;
      case Xin_MFence:
         return;
      case Xin_ACAS:
         mapRegs_X86AMode(m, i->Xin.ACAS.addr);
         return;
      case Xin_DACAS:
         mapRegs_X86AMode(m, i->Xin.DACAS.addr);
         return;
      case Xin_FpUnary:
         mapReg(m, &i->Xin.FpUnary.src);
         mapReg(m, &i->Xin.FpUnary.dst);
         return;
      case Xin_FpBinary:
         mapReg(m, &i->Xin.FpBinary.srcL);
         mapReg(m, &i->Xin.FpBinary.srcR);
         mapReg(m, &i->Xin.FpBinary.dst);
         return;
      case Xin_FpLdSt:
         mapRegs_X86AMode(m, i->Xin.FpLdSt.addr);
         mapReg(m, &i->Xin.FpLdSt.reg);
         return;
      case Xin_FpLdStI:
         mapRegs_X86AMode(m, i->Xin.FpLdStI.addr);
         mapReg(m, &i->Xin.FpLdStI.reg);
         return;
      case Xin_Fp64to32:
         mapReg(m, &i->Xin.Fp64to32.src);
         mapReg(m, &i->Xin.Fp64to32.dst);
         return;
      case Xin_FpCMov:
         mapReg(m, &i->Xin.FpCMov.src);
         mapReg(m, &i->Xin.FpCMov.dst);
         return;
      case Xin_FpLdCW:
         mapRegs_X86AMode(m, i->Xin.FpLdCW.addr);
         return;
      case Xin_FpStSW_AX:
         return;
      case Xin_FpCmp:
         mapReg(m, &i->Xin.FpCmp.srcL);
         mapReg(m, &i->Xin.FpCmp.srcR);
         mapReg(m, &i->Xin.FpCmp.dst);
         return;
      case Xin_SseConst:
         mapReg(m, &i->Xin.SseConst.dst);
         return;
      case Xin_SseLdSt:
         mapReg(m, &i->Xin.SseLdSt.reg);
         mapRegs_X86AMode(m, i->Xin.SseLdSt.addr);
         break;
      case Xin_SseLdzLO:
         mapReg(m, &i->Xin.SseLdzLO.reg);
         mapRegs_X86AMode(m, i->Xin.SseLdzLO.addr);
         break;
      case Xin_Sse32Fx4:
         mapReg(m, &i->Xin.Sse32Fx4.src);
         mapReg(m, &i->Xin.Sse32Fx4.dst);
         return;
      case Xin_Sse32FLo:
         mapReg(m, &i->Xin.Sse32FLo.src);
         mapReg(m, &i->Xin.Sse32FLo.dst);
         return;
      case Xin_Sse64Fx2:
         mapReg(m, &i->Xin.Sse64Fx2.src);
         mapReg(m, &i->Xin.Sse64Fx2.dst);
         return;
      case Xin_Sse64FLo:
         mapReg(m, &i->Xin.Sse64FLo.src);
         mapReg(m, &i->Xin.Sse64FLo.dst);
         return;
      case Xin_SseReRg:
         mapReg(m, &i->Xin.SseReRg.src);
         mapReg(m, &i->Xin.SseReRg.dst);
         return;
      case Xin_SseCMov:
         mapReg(m, &i->Xin.SseCMov.src);
         mapReg(m, &i->Xin.SseCMov.dst);
         return;
      case Xin_SseShuf:
         mapReg(m, &i->Xin.SseShuf.src);
         mapReg(m, &i->Xin.SseShuf.dst);
         return;
      case Xin_EvCheck:
         /* We expect both amodes only to mention %ebp, so this is in
            fact pointless, since %ebp isn't allocatable, but anyway.. */
         mapRegs_X86AMode(m, i->Xin.EvCheck.amCounter);
         mapRegs_X86AMode(m, i->Xin.EvCheck.amFailAddr);
         return;
      case Xin_ProfInc:
         /* does not use any registers. */
         return;

      default:
         ppX86Instr(i, mode64);
         vpanic("mapRegs_X86Instr");
   }
}

/* Figure out if i represents a reg-reg move, and if so assign the
   source and destination to *src and *dst.  If in doubt say No.  Used
   by the register allocator to do move coalescing. 
*/
Bool isMove_X86Instr ( const X86Instr* i, HReg* src, HReg* dst )
{
   /* Moves between integer regs */
   if (i->tag == Xin_Alu32R) {
      if (i->Xin.Alu32R.op != Xalu_MOV)
         return False;
      if (i->Xin.Alu32R.src->tag != Xrmi_Reg)
         return False;
      *src = i->Xin.Alu32R.src->Xrmi.Reg.reg;
      *dst = i->Xin.Alu32R.dst;
      return True;
   }
   /* Moves between FP regs */
   if (i->tag == Xin_FpUnary) {
      if (i->Xin.FpUnary.op != Xfp_MOV)
         return False;
      *src = i->Xin.FpUnary.src;
      *dst = i->Xin.FpUnary.dst;
      return True;
   }
   if (i->tag == Xin_SseReRg) {
      if (i->Xin.SseReRg.op != Xsse_MOV)
         return False;
      *src = i->Xin.SseReRg.src;
      *dst = i->Xin.SseReRg.dst;
      return True;
   }
   return False;
}


/* Generate x86 spill/reload instructions under the direction of the
   register allocator.  Note it's critical these don't write the
   condition codes. */

void genSpill_X86 ( /*OUT*/HInstr** i1, /*OUT*/HInstr** i2,
                    HReg rreg, Int offsetB, Bool mode64 )
{
   X86AMode* am;
   vassert(offsetB >= 0);
   vassert(!hregIsVirtual(rreg));
   vassert(mode64 == False);
   *i1 = *i2 = NULL;
   am = X86AMode_IR(offsetB, hregX86_EBP());
   switch (hregClass(rreg)) {
      case HRcInt32:
         *i1 = X86Instr_Alu32M ( Xalu_MOV, X86RI_Reg(rreg), am );
         return;
      case HRcFlt64:
         *i1 = X86Instr_FpLdSt ( False/*store*/, 10, rreg, am );
         return;
      case HRcVec128:
         *i1 = X86Instr_SseLdSt ( False/*store*/, rreg, am );
         return;
      default: 
         ppHRegClass(hregClass(rreg));
         vpanic("genSpill_X86: unimplemented regclass");
   }
}

void genReload_X86 ( /*OUT*/HInstr** i1, /*OUT*/HInstr** i2,
                     HReg rreg, Int offsetB, Bool mode64 )
{
   X86AMode* am;
   vassert(offsetB >= 0);
   vassert(!hregIsVirtual(rreg));
   vassert(mode64 == False);
   *i1 = *i2 = NULL;
   am = X86AMode_IR(offsetB, hregX86_EBP());
   switch (hregClass(rreg)) {
      case HRcInt32:
         *i1 = X86Instr_Alu32R ( Xalu_MOV, X86RMI_Mem(am), rreg );
         return;
      case HRcFlt64:
         *i1 = X86Instr_FpLdSt ( True/*load*/, 10, rreg, am );
         return;
      case HRcVec128:
         *i1 = X86Instr_SseLdSt ( True/*load*/, rreg, am );
         return;
      default: 
         ppHRegClass(hregClass(rreg));
         vpanic("genReload_X86: unimplemented regclass");
   }
}

/* The given instruction reads the specified vreg exactly once, and
   that vreg is currently located at the given spill offset.  If
   possible, return a variant of the instruction to one which instead
   references the spill slot directly. */

X86Instr* directReload_X86( X86Instr* i, HReg vreg, Short spill_off )
{
   vassert(spill_off >= 0 && spill_off < 10000); /* let's say */

   /* Deal with form: src=RMI_Reg, dst=Reg where src == vreg 
      Convert to: src=RMI_Mem, dst=Reg 
   */
   if (i->tag == Xin_Alu32R
       && (i->Xin.Alu32R.op == Xalu_MOV || i->Xin.Alu32R.op == Xalu_OR
           || i->Xin.Alu32R.op == Xalu_XOR)
       && i->Xin.Alu32R.src->tag == Xrmi_Reg
       && sameHReg(i->Xin.Alu32R.src->Xrmi.Reg.reg, vreg)) {
      vassert(! sameHReg(i->Xin.Alu32R.dst, vreg));
      return X86Instr_Alu32R( 
                i->Xin.Alu32R.op, 
                X86RMI_Mem( X86AMode_IR( spill_off, hregX86_EBP())),
                i->Xin.Alu32R.dst
             );
   }

   /* Deal with form: src=RMI_Imm, dst=Reg where dst == vreg 
      Convert to: src=RI_Imm, dst=Mem
   */
   if (i->tag == Xin_Alu32R
       && (i->Xin.Alu32R.op == Xalu_CMP)
       && i->Xin.Alu32R.src->tag == Xrmi_Imm
       && sameHReg(i->Xin.Alu32R.dst, vreg)) {
      return X86Instr_Alu32M( 
                i->Xin.Alu32R.op,
		X86RI_Imm( i->Xin.Alu32R.src->Xrmi.Imm.imm32 ),
                X86AMode_IR( spill_off, hregX86_EBP())
             );
   }

   /* Deal with form: Push(RMI_Reg)
      Convert to: Push(RMI_Mem) 
   */
   if (i->tag == Xin_Push
       && i->Xin.Push.src->tag == Xrmi_Reg
       && sameHReg(i->Xin.Push.src->Xrmi.Reg.reg, vreg)) {
      return X86Instr_Push(
                X86RMI_Mem( X86AMode_IR( spill_off, hregX86_EBP()))
             );
   }

   /* Deal with form: CMov32(src=RM_Reg, dst) where vreg == src
      Convert to CMov32(RM_Mem, dst) */
   if (i->tag == Xin_CMov32
       && i->Xin.CMov32.src->tag == Xrm_Reg
       && sameHReg(i->Xin.CMov32.src->Xrm.Reg.reg, vreg)) {
      vassert(! sameHReg(i->Xin.CMov32.dst, vreg));
      return X86Instr_CMov32( 
                i->Xin.CMov32.cond,
                X86RM_Mem( X86AMode_IR( spill_off, hregX86_EBP() )),
                i->Xin.CMov32.dst
             );
   }

   /* Deal with form: Test32(imm,RM_Reg vreg) -> Test32(imm,amode) */
   if (i->tag == Xin_Test32
       && i->Xin.Test32.dst->tag == Xrm_Reg
       && sameHReg(i->Xin.Test32.dst->Xrm.Reg.reg, vreg)) {
      return X86Instr_Test32(
                i->Xin.Test32.imm32,
                X86RM_Mem( X86AMode_IR( spill_off, hregX86_EBP() ) )
             );
   }

   return NULL;
}


/* --------- The x86 assembler (bleh.) --------- */

inline static UInt iregEnc ( HReg r )
{
   UInt n;
   vassert(hregClass(r) == HRcInt32);
   vassert(!hregIsVirtual(r));
   n = hregEncoding(r);
   vassert(n <= 7);
   return n;
}

inline static UInt fregEnc ( HReg r )
{
   UInt n;
   vassert(hregClass(r) == HRcFlt64);
   vassert(!hregIsVirtual(r));
   n = hregEncoding(r);
   vassert(n <= 5);
   return n;
}

inline static UInt vregEnc ( HReg r )
{
   UInt n;
   vassert(hregClass(r) == HRcVec128);
   vassert(!hregIsVirtual(r));
   n = hregEncoding(r);
   vassert(n <= 7);
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
   *p++ = toUChar( w32        & 0x000000FF);
   *p++ = toUChar((w32 >>  8) & 0x000000FF);
   *p++ = toUChar((w32 >> 16) & 0x000000FF);
   *p++ = toUChar((w32 >> 24) & 0x000000FF);
   return p;
}

/* Does a sign-extend of the lowest 8 bits give 
   the original number? */
static Bool fits8bits ( UInt w32 )
{
   Int i32 = (Int)w32;
   return toBool(i32 == ((Int)(w32 << 24) >> 24));
}


/* Forming mod-reg-rm bytes and scale-index-base bytes.

     greg,  0(ereg)    |  ereg != ESP && ereg != EBP
                       =  00 greg ereg

     greg,  d8(ereg)   |  ereg != ESP
                       =  01 greg ereg, d8

     greg,  d32(ereg)  |  ereg != ESP
                       =  10 greg ereg, d32

     greg,  d8(%esp)   =  01 greg 100, 0x24, d8

     -----------------------------------------------

     greg,  d8(base,index,scale)  
               |  index != ESP
               =  01 greg 100, scale index base, d8

     greg,  d32(base,index,scale)
               |  index != ESP
               =  10 greg 100, scale index base, d32
*/
static UChar* doAMode_M__wrk ( UChar* p, UInt gregEnc, X86AMode* am )
{
   if (am->tag == Xam_IR) {
      if (am->Xam.IR.imm == 0 
          && ! sameHReg(am->Xam.IR.reg, hregX86_ESP())
          && ! sameHReg(am->Xam.IR.reg, hregX86_EBP()) ) {
         *p++ = mkModRegRM(0, gregEnc, iregEnc(am->Xam.IR.reg));
         return p;
      }
      if (fits8bits(am->Xam.IR.imm)
          && ! sameHReg(am->Xam.IR.reg, hregX86_ESP())) {
         *p++ = mkModRegRM(1, gregEnc, iregEnc(am->Xam.IR.reg));
         *p++ = toUChar(am->Xam.IR.imm & 0xFF);
         return p;
      }
      if (! sameHReg(am->Xam.IR.reg, hregX86_ESP())) {
         *p++ = mkModRegRM(2, gregEnc, iregEnc(am->Xam.IR.reg));
         p = emit32(p, am->Xam.IR.imm);
         return p;
      }
      if (sameHReg(am->Xam.IR.reg, hregX86_ESP())
          && fits8bits(am->Xam.IR.imm)) {
 	 *p++ = mkModRegRM(1, gregEnc, 4);
         *p++ = 0x24;
         *p++ = toUChar(am->Xam.IR.imm & 0xFF);
         return p;
      }
      ppX86AMode(am);
      vpanic("doAMode_M: can't emit amode IR");
      /*NOTREACHED*/
   }
   if (am->tag == Xam_IRRS) {
      if (fits8bits(am->Xam.IRRS.imm)
          && ! sameHReg(am->Xam.IRRS.index, hregX86_ESP())) {
         *p++ = mkModRegRM(1, gregEnc, 4);
         *p++ = mkSIB(am->Xam.IRRS.shift, iregEnc(am->Xam.IRRS.index),
                                          iregEnc(am->Xam.IRRS.base));
         *p++ = toUChar(am->Xam.IRRS.imm & 0xFF);
         return p;
      }
      if (! sameHReg(am->Xam.IRRS.index, hregX86_ESP())) {
         *p++ = mkModRegRM(2, gregEnc, 4);
         *p++ = mkSIB(am->Xam.IRRS.shift, iregEnc(am->Xam.IRRS.index),
                                          iregEnc(am->Xam.IRRS.base));
         p = emit32(p, am->Xam.IRRS.imm);
         return p;
      }
      ppX86AMode(am);
      vpanic("doAMode_M: can't emit amode IRRS");
      /*NOTREACHED*/
   }
   vpanic("doAMode_M: unknown amode");
   /*NOTREACHED*/
}

static UChar* doAMode_M ( UChar* p, HReg greg, X86AMode* am )
{
   return doAMode_M__wrk(p, iregEnc(greg), am);
}

static UChar* doAMode_M_enc ( UChar* p, UInt gregEnc, X86AMode* am )
{
   vassert(gregEnc < 8);
   return doAMode_M__wrk(p, gregEnc, am);
}


/* Emit a mod-reg-rm byte when the rm bit denotes a reg. */
inline static UChar* doAMode_R__wrk ( UChar* p, UInt gregEnc, UInt eregEnc ) 
{
   *p++ = mkModRegRM(3, gregEnc, eregEnc);
   return p;
}

static UChar* doAMode_R ( UChar* p, HReg greg, HReg ereg )
{
   return doAMode_R__wrk(p, iregEnc(greg), iregEnc(ereg));
}

static UChar* doAMode_R_enc_reg ( UChar* p, UInt gregEnc, HReg ereg )
{
   vassert(gregEnc < 8);
   return doAMode_R__wrk(p, gregEnc, iregEnc(ereg));
}

static UChar* doAMode_R_enc_enc ( UChar* p, UInt gregEnc, UInt eregEnc )
{
   vassert( (gregEnc|eregEnc) < 8);
   return doAMode_R__wrk(p, gregEnc, eregEnc);
}


/* Emit ffree %st(7) */
static UChar* do_ffree_st7 ( UChar* p )
{
   *p++ = 0xDD;
   *p++ = 0xC7;
   return p;
}

/* Emit fstp %st(i), 1 <= i <= 7 */
static UChar* do_fstp_st ( UChar* p, Int i )
{
   vassert(1 <= i && i <= 7);
   *p++ = 0xDD;
   *p++ = toUChar(0xD8+i);
   return p;
}

/* Emit fld %st(i), 0 <= i <= 6 */
static UChar* do_fld_st ( UChar* p, Int i )
{
   vassert(0 <= i && i <= 6);
   *p++ = 0xD9;
   *p++ = toUChar(0xC0+i);
   return p;
}

/* Emit f<op> %st(0) */
static UChar* do_fop1_st ( UChar* p, X86FpOp op )
{
   switch (op) {
      case Xfp_NEG:    *p++ = 0xD9; *p++ = 0xE0; break;
      case Xfp_ABS:    *p++ = 0xD9; *p++ = 0xE1; break;
      case Xfp_SQRT:   *p++ = 0xD9; *p++ = 0xFA; break;
      case Xfp_ROUND:  *p++ = 0xD9; *p++ = 0xFC; break;
      case Xfp_SIN:    *p++ = 0xD9; *p++ = 0xFE; break;
      case Xfp_COS:    *p++ = 0xD9; *p++ = 0xFF; break;
      case Xfp_2XM1:   *p++ = 0xD9; *p++ = 0xF0; break;
      case Xfp_MOV:    break;
      case Xfp_TAN:
         /* fptan pushes 1.0 on the FP stack, except when the argument
            is out of range.  Hence we have to do the instruction,
            then inspect C2 to see if there is an out of range
            condition.  If there is, we skip the fincstp that is used
            by the in-range case to get rid of this extra 1.0
            value. */
         p = do_ffree_st7(p); /* since fptan sometimes pushes 1.0 */
         *p++ = 0xD9; *p++ = 0xF2; // fptan
         *p++ = 0x50;              // pushl %eax
         *p++ = 0xDF; *p++ = 0xE0; // fnstsw %ax
         *p++ = 0x66; *p++ = 0xA9; 
         *p++ = 0x00; *p++ = 0x04; // testw $0x400,%ax
         *p++ = 0x75; *p++ = 0x02; // jnz after_fincstp
         *p++ = 0xD9; *p++ = 0xF7; // fincstp
         *p++ = 0x58;              // after_fincstp: popl %eax
         break;
      default:
         vpanic("do_fop1_st: unknown op");
   }
   return p;
}

/* Emit f<op> %st(i), 1 <= i <= 5 */
static UChar* do_fop2_st ( UChar* p, X86FpOp op, Int i )
{
   Int subopc;
   switch (op) {
      case Xfp_ADD: subopc = 0; break;
      case Xfp_SUB: subopc = 4; break;
      case Xfp_MUL: subopc = 1; break;
      case Xfp_DIV: subopc = 6; break;
      default: vpanic("do_fop2_st: unknown op");
   }
   *p++ = 0xD8;
   p    = doAMode_R_enc_enc(p, subopc, i);
   return p;
}

/* Push a 32-bit word on the stack.  The word depends on tags[3:0];
each byte is either 0x00 or 0xFF depending on the corresponding bit in tags[].
*/
static UChar* push_word_from_tags ( UChar* p, UShort tags )
{
   UInt w;
   vassert(0 == (tags & ~0xF));
   if (tags == 0) {
      /* pushl $0x00000000 */
      *p++ = 0x6A;
      *p++ = 0x00;
   }
   else 
   /* pushl $0xFFFFFFFF */
   if (tags == 0xF) {
      *p++ = 0x6A;
      *p++ = 0xFF;
   } else {
      vassert(0); /* awaiting test case */
      w = 0;
      if (tags & 1) w |= 0x000000FF;
      if (tags & 2) w |= 0x0000FF00;
      if (tags & 4) w |= 0x00FF0000;
      if (tags & 8) w |= 0xFF000000;
      *p++ = 0x68;
      p = emit32(p, w);
   }
   return p;
}

/* Emit an instruction into buf and return the number of bytes used.
   Note that buf is not the insn's final place, and therefore it is
   imperative to emit position-independent code.  If the emitted
   instruction was a profiler inc, set *is_profInc to True, else
   leave it unchanged. */

Int emit_X86Instr ( /*MB_MOD*/Bool* is_profInc,
                    UChar* buf, Int nbuf, const X86Instr* i, 
                    Bool mode64, VexEndness endness_host,
                    const void* disp_cp_chain_me_to_slowEP,
                    const void* disp_cp_chain_me_to_fastEP,
                    const void* disp_cp_xindir,
                    const void* disp_cp_xassisted )
{
   UInt irno, opc, opc_rr, subopc_imm, opc_imma, opc_cl, opc_imm, subopc;

   UInt   xtra;
   UChar* p = &buf[0];
   UChar* ptmp;
   vassert(nbuf >= 32);
   vassert(mode64 == False);

   /* vex_printf("asm  ");ppX86Instr(i, mode64); vex_printf("\n"); */

   switch (i->tag) {

   case Xin_Alu32R:
      /* Deal specially with MOV */
      if (i->Xin.Alu32R.op == Xalu_MOV) {
         switch (i->Xin.Alu32R.src->tag) {
            case Xrmi_Imm:
               *p++ = toUChar(0xB8 + iregEnc(i->Xin.Alu32R.dst));
               p = emit32(p, i->Xin.Alu32R.src->Xrmi.Imm.imm32);
               goto done;
            case Xrmi_Reg:
               *p++ = 0x89;
               p = doAMode_R(p, i->Xin.Alu32R.src->Xrmi.Reg.reg,
                                i->Xin.Alu32R.dst);
               goto done;
            case Xrmi_Mem:
               *p++ = 0x8B;
               p = doAMode_M(p, i->Xin.Alu32R.dst, 
                                i->Xin.Alu32R.src->Xrmi.Mem.am);
               goto done;
            default:
               goto bad;
         }
      }
      /* MUL */
      if (i->Xin.Alu32R.op == Xalu_MUL) {
         switch (i->Xin.Alu32R.src->tag) {
            case Xrmi_Reg:
               *p++ = 0x0F;
               *p++ = 0xAF;
               p = doAMode_R(p, i->Xin.Alu32R.dst,
                                i->Xin.Alu32R.src->Xrmi.Reg.reg);
               goto done;
            case Xrmi_Mem:
               *p++ = 0x0F;
               *p++ = 0xAF;
               p = doAMode_M(p, i->Xin.Alu32R.dst,
                                i->Xin.Alu32R.src->Xrmi.Mem.am);
               goto done;
            case Xrmi_Imm:
               if (fits8bits(i->Xin.Alu32R.src->Xrmi.Imm.imm32)) {
                  *p++ = 0x6B;
                  p = doAMode_R(p, i->Xin.Alu32R.dst, i->Xin.Alu32R.dst);
                  *p++ = toUChar(0xFF & i->Xin.Alu32R.src->Xrmi.Imm.imm32);
               } else {
                  *p++ = 0x69;
                  p = doAMode_R(p, i->Xin.Alu32R.dst, i->Xin.Alu32R.dst);
                  p = emit32(p, i->Xin.Alu32R.src->Xrmi.Imm.imm32);
               }
               goto done;
            default:
               goto bad;
         }
      }
      /* ADD/SUB/ADC/SBB/AND/OR/XOR/CMP */
      opc = opc_rr = subopc_imm = opc_imma = 0;
      switch (i->Xin.Alu32R.op) {
         case Xalu_ADC: opc = 0x13; opc_rr = 0x11; 
                        subopc_imm = 2; opc_imma = 0x15; break;
         case Xalu_ADD: opc = 0x03; opc_rr = 0x01; 
                        subopc_imm = 0; opc_imma = 0x05; break;
         case Xalu_SUB: opc = 0x2B; opc_rr = 0x29; 
                        subopc_imm = 5; opc_imma = 0x2D; break;
         case Xalu_SBB: opc = 0x1B; opc_rr = 0x19; 
                        subopc_imm = 3; opc_imma = 0x1D; break;
         case Xalu_AND: opc = 0x23; opc_rr = 0x21; 
                        subopc_imm = 4; opc_imma = 0x25; break;
         case Xalu_XOR: opc = 0x33; opc_rr = 0x31; 
                        subopc_imm = 6; opc_imma = 0x35; break;
         case Xalu_OR:  opc = 0x0B; opc_rr = 0x09; 
                        subopc_imm = 1; opc_imma = 0x0D; break;
         case Xalu_CMP: opc = 0x3B; opc_rr = 0x39; 
                        subopc_imm = 7; opc_imma = 0x3D; break;
         default: goto bad;
      }
      switch (i->Xin.Alu32R.src->tag) {
         case Xrmi_Imm:
            if (sameHReg(i->Xin.Alu32R.dst, hregX86_EAX())
                && !fits8bits(i->Xin.Alu32R.src->Xrmi.Imm.imm32)) {
               *p++ = toUChar(opc_imma);
               p = emit32(p, i->Xin.Alu32R.src->Xrmi.Imm.imm32);
            } else
            if (fits8bits(i->Xin.Alu32R.src->Xrmi.Imm.imm32)) {
               *p++ = 0x83; 
               p    = doAMode_R_enc_reg(p, subopc_imm, i->Xin.Alu32R.dst);
               *p++ = toUChar(0xFF & i->Xin.Alu32R.src->Xrmi.Imm.imm32);
            } else {
               *p++ = 0x81; 
               p    = doAMode_R_enc_reg(p, subopc_imm, i->Xin.Alu32R.dst);
               p    = emit32(p, i->Xin.Alu32R.src->Xrmi.Imm.imm32);
            }
            goto done;
         case Xrmi_Reg:
            *p++ = toUChar(opc_rr);
            p = doAMode_R(p, i->Xin.Alu32R.src->Xrmi.Reg.reg,
                             i->Xin.Alu32R.dst);
            goto done;
         case Xrmi_Mem:
            *p++ = toUChar(opc);
            p = doAMode_M(p, i->Xin.Alu32R.dst,
                             i->Xin.Alu32R.src->Xrmi.Mem.am);
            goto done;
         default: 
            goto bad;
      }
      break;

   case Xin_Alu32M:
      /* Deal specially with MOV */
      if (i->Xin.Alu32M.op == Xalu_MOV) {
         switch (i->Xin.Alu32M.src->tag) {
            case Xri_Reg:
               *p++ = 0x89;
               p = doAMode_M(p, i->Xin.Alu32M.src->Xri.Reg.reg,
                                i->Xin.Alu32M.dst);
               goto done;
            case Xri_Imm:
               *p++ = 0xC7;
               p = doAMode_M_enc(p, 0, i->Xin.Alu32M.dst);
               p = emit32(p, i->Xin.Alu32M.src->Xri.Imm.imm32);
               goto done;
            default: 
               goto bad;
         }
      }
      /* ADD/SUB/ADC/SBB/AND/OR/XOR/CMP.  MUL is not
         allowed here. */
      opc = subopc_imm = opc_imma = 0;
      switch (i->Xin.Alu32M.op) {
         case Xalu_ADD: opc = 0x01; subopc_imm = 0; break;
         case Xalu_SUB: opc = 0x29; subopc_imm = 5; break;
         case Xalu_CMP: opc = 0x39; subopc_imm = 7; break;
         default: goto bad;
      }
      switch (i->Xin.Alu32M.src->tag) {
         case Xri_Reg:
            *p++ = toUChar(opc);
            p = doAMode_M(p, i->Xin.Alu32M.src->Xri.Reg.reg,
                             i->Xin.Alu32M.dst);
            goto done;
         case Xri_Imm:
            if (fits8bits(i->Xin.Alu32M.src->Xri.Imm.imm32)) {
               *p++ = 0x83;
               p    = doAMode_M_enc(p, subopc_imm, i->Xin.Alu32M.dst);
               *p++ = toUChar(0xFF & i->Xin.Alu32M.src->Xri.Imm.imm32);
               goto done;
            } else {
               *p++ = 0x81;
               p    = doAMode_M_enc(p, subopc_imm, i->Xin.Alu32M.dst);
               p    = emit32(p, i->Xin.Alu32M.src->Xri.Imm.imm32);
               goto done;
            }
         default: 
            goto bad;
      }
      break;

   case Xin_Sh32:
      opc_cl = opc_imm = subopc = 0;
      switch (i->Xin.Sh32.op) {
         case Xsh_SHR: opc_cl = 0xD3; opc_imm = 0xC1; subopc = 5; break;
         case Xsh_SAR: opc_cl = 0xD3; opc_imm = 0xC1; subopc = 7; break;
         case Xsh_SHL: opc_cl = 0xD3; opc_imm = 0xC1; subopc = 4; break;
         default: goto bad;
      }
      if (i->Xin.Sh32.src == 0) {
         *p++ = toUChar(opc_cl);
         p = doAMode_R_enc_reg(p, subopc, i->Xin.Sh32.dst);
      } else {
         *p++ = toUChar(opc_imm);
         p = doAMode_R_enc_reg(p, subopc, i->Xin.Sh32.dst);
         *p++ = (UChar)(i->Xin.Sh32.src);
      }
      goto done;

   case Xin_Test32:
      if (i->Xin.Test32.dst->tag == Xrm_Reg) {
         /* testl $imm32, %reg */
         *p++ = 0xF7;
         p = doAMode_R_enc_reg(p, 0, i->Xin.Test32.dst->Xrm.Reg.reg);
         p = emit32(p, i->Xin.Test32.imm32);
         goto done;
      } else {
         /* testl $imm32, amode */
         *p++ = 0xF7;
         p = doAMode_M_enc(p, 0, i->Xin.Test32.dst->Xrm.Mem.am);
         p = emit32(p, i->Xin.Test32.imm32);
         goto done;
      }

   case Xin_Unary32:
      if (i->Xin.Unary32.op == Xun_NOT) {
         *p++ = 0xF7;
         p = doAMode_R_enc_reg(p, 2, i->Xin.Unary32.dst);
         goto done;
      }
      if (i->Xin.Unary32.op == Xun_NEG) {
         *p++ = 0xF7;
         p = doAMode_R_enc_reg(p, 3, i->Xin.Unary32.dst);
         goto done;
      }
      break;

   case Xin_Lea32:
      *p++ = 0x8D;
      p = doAMode_M(p, i->Xin.Lea32.dst, i->Xin.Lea32.am);
      goto done;

   case Xin_MulL:
      subopc = i->Xin.MulL.syned ? 5 : 4;
      *p++ = 0xF7;
      switch (i->Xin.MulL.src->tag)  {
         case Xrm_Mem:
            p = doAMode_M_enc(p, subopc, i->Xin.MulL.src->Xrm.Mem.am);
            goto done;
         case Xrm_Reg:
            p = doAMode_R_enc_reg(p, subopc, i->Xin.MulL.src->Xrm.Reg.reg);
            goto done;
         default:
            goto bad;
      }
      break;

   case Xin_Div:
      subopc = i->Xin.Div.syned ? 7 : 6;
      *p++ = 0xF7;
      switch (i->Xin.Div.src->tag)  {
         case Xrm_Mem:
            p = doAMode_M_enc(p, subopc, i->Xin.Div.src->Xrm.Mem.am);
            goto done;
         case Xrm_Reg:
            p = doAMode_R_enc_reg(p, subopc, i->Xin.Div.src->Xrm.Reg.reg);
            goto done;
         default:
            goto bad;
      }
      break;

   case Xin_Sh3232:
      vassert(i->Xin.Sh3232.op == Xsh_SHL || i->Xin.Sh3232.op == Xsh_SHR);
      if (i->Xin.Sh3232.amt == 0) {
         /* shldl/shrdl by %cl */
         *p++ = 0x0F;
         if (i->Xin.Sh3232.op == Xsh_SHL) {
            *p++ = 0xA5;
         } else {
            *p++ = 0xAD;
         }
         p = doAMode_R(p, i->Xin.Sh3232.src, i->Xin.Sh3232.dst);
         goto done;
      }
      break;

   case Xin_Push:
      switch (i->Xin.Push.src->tag) {
         case Xrmi_Mem: 
            *p++ = 0xFF;
            p = doAMode_M_enc(p, 6, i->Xin.Push.src->Xrmi.Mem.am);
            goto done;
         case Xrmi_Imm:
            *p++ = 0x68;
            p = emit32(p, i->Xin.Push.src->Xrmi.Imm.imm32);
            goto done;
         case Xrmi_Reg:
            *p++ = toUChar(0x50 + iregEnc(i->Xin.Push.src->Xrmi.Reg.reg));
            goto done;
        default: 
            goto bad;
      }

   case Xin_Call:
      if (i->Xin.Call.cond != Xcc_ALWAYS
          && i->Xin.Call.rloc.pri != RLPri_None) {
         /* The call might not happen (it isn't unconditional) and it
            returns a result.  In this case we will need to generate a
            control flow diamond to put 0x555..555 in the return
            register(s) in the case where the call doesn't happen.  If
            this ever becomes necessary, maybe copy code from the ARM
            equivalent.  Until that day, just give up. */
         goto bad;
      }
      /* See detailed comment for Xin_Call in getRegUsage_X86Instr above
         for explanation of this. */
      switch (i->Xin.Call.regparms) {
         case 0: irno = iregEnc(hregX86_EAX()); break;
         case 1: irno = iregEnc(hregX86_EDX()); break;
         case 2: irno = iregEnc(hregX86_ECX()); break;
         case 3: irno = iregEnc(hregX86_EDI()); break;
         default: vpanic(" emit_X86Instr:call:regparms");
      }
      /* jump over the following two insns if the condition does not
         hold */
      if (i->Xin.Call.cond != Xcc_ALWAYS) {
         *p++ = toUChar(0x70 + (0xF & (i->Xin.Call.cond ^ 1)));
         *p++ = 0x07; /* 7 bytes in the next two insns */
      }
      /* movl $target, %tmp */
      *p++ = toUChar(0xB8 + irno);
      p = emit32(p, i->Xin.Call.target);
      /* call *%tmp */
      *p++ = 0xFF;
      *p++ = toUChar(0xD0 + irno);
      goto done;

   case Xin_XDirect: {
      /* NB: what goes on here has to be very closely coordinated with the
         chainXDirect_X86 and unchainXDirect_X86 below. */
      /* We're generating chain-me requests here, so we need to be
         sure this is actually allowed -- no-redir translations can't
         use chain-me's.  Hence: */
      vassert(disp_cp_chain_me_to_slowEP != NULL);
      vassert(disp_cp_chain_me_to_fastEP != NULL);

      /* Use ptmp for backpatching conditional jumps. */
      ptmp = NULL;

      /* First off, if this is conditional, create a conditional
         jump over the rest of it. */
      if (i->Xin.XDirect.cond != Xcc_ALWAYS) {
         /* jmp fwds if !condition */
         *p++ = toUChar(0x70 + (0xF & (i->Xin.XDirect.cond ^ 1)));
         ptmp = p; /* fill in this bit later */
         *p++ = 0; /* # of bytes to jump over; don't know how many yet. */
      }

      /* Update the guest EIP. */
      /* movl $dstGA, amEIP */
      *p++ = 0xC7;
      p    = doAMode_M_enc(p, 0, i->Xin.XDirect.amEIP);
      p    = emit32(p, i->Xin.XDirect.dstGA);

      /* --- FIRST PATCHABLE BYTE follows --- */
      /* VG_(disp_cp_chain_me_to_{slowEP,fastEP}) (where we're calling
         to) backs up the return address, so as to find the address of
         the first patchable byte.  So: don't change the length of the
         two instructions below. */
      /* movl $disp_cp_chain_me_to_{slow,fast}EP,%edx; */
      *p++ = 0xBA;
      const void* disp_cp_chain_me
               = i->Xin.XDirect.toFastEP ? disp_cp_chain_me_to_fastEP 
                                         : disp_cp_chain_me_to_slowEP;
      p = emit32(p, (UInt)(Addr)disp_cp_chain_me);
      /* call *%edx */
      *p++ = 0xFF;
      *p++ = 0xD2;
      /* --- END of PATCHABLE BYTES --- */

      /* Fix up the conditional jump, if there was one. */
      if (i->Xin.XDirect.cond != Xcc_ALWAYS) {
         Int delta = p - ptmp;
         vassert(delta > 0 && delta < 40);
         *ptmp = toUChar(delta-1);
      }
      goto done;
   }

   case Xin_XIndir: {
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
      if (i->Xin.XIndir.cond != Xcc_ALWAYS) {
         /* jmp fwds if !condition */
         *p++ = toUChar(0x70 + (0xF & (i->Xin.XIndir.cond ^ 1)));
         ptmp = p; /* fill in this bit later */
         *p++ = 0; /* # of bytes to jump over; don't know how many yet. */
      }

      /* movl dstGA(a reg), amEIP -- copied from Alu32M MOV case */
      *p++ = 0x89;
      p = doAMode_M(p, i->Xin.XIndir.dstGA, i->Xin.XIndir.amEIP);

      /* movl $disp_indir, %edx */
      *p++ = 0xBA;
      p = emit32(p, (UInt)(Addr)disp_cp_xindir);
      /* jmp *%edx */
      *p++ = 0xFF;
      *p++ = 0xE2;

      /* Fix up the conditional jump, if there was one. */
      if (i->Xin.XIndir.cond != Xcc_ALWAYS) {
         Int delta = p - ptmp;
         vassert(delta > 0 && delta < 40);
         *ptmp = toUChar(delta-1);
      }
      goto done;
   }

   case Xin_XAssisted: {
      /* Use ptmp for backpatching conditional jumps. */
      ptmp = NULL;

      /* First off, if this is conditional, create a conditional
         jump over the rest of it. */
      if (i->Xin.XAssisted.cond != Xcc_ALWAYS) {
         /* jmp fwds if !condition */
         *p++ = toUChar(0x70 + (0xF & (i->Xin.XAssisted.cond ^ 1)));
         ptmp = p; /* fill in this bit later */
         *p++ = 0; /* # of bytes to jump over; don't know how many yet. */
      }

      /* movl dstGA(a reg), amEIP -- copied from Alu32M MOV case */
      *p++ = 0x89;
      p = doAMode_M(p, i->Xin.XIndir.dstGA, i->Xin.XIndir.amEIP);
      /* movl $magic_number, %ebp. */
      UInt trcval = 0;
      switch (i->Xin.XAssisted.jk) {
         case Ijk_ClientReq:    trcval = VEX_TRC_JMP_CLIENTREQ;    break;
         case Ijk_Sys_syscall:  trcval = VEX_TRC_JMP_SYS_SYSCALL;  break;
         case Ijk_Sys_int128:   trcval = VEX_TRC_JMP_SYS_INT128;   break;
         case Ijk_Sys_int129:   trcval = VEX_TRC_JMP_SYS_INT129;   break;
         case Ijk_Sys_int130:   trcval = VEX_TRC_JMP_SYS_INT130;   break;
         case Ijk_Sys_int145:   trcval = VEX_TRC_JMP_SYS_INT145;   break;
         case Ijk_Sys_int210:   trcval = VEX_TRC_JMP_SYS_INT210;   break;
         case Ijk_Sys_sysenter: trcval = VEX_TRC_JMP_SYS_SYSENTER; break;
         case Ijk_Yield:        trcval = VEX_TRC_JMP_YIELD;        break;
         case Ijk_EmWarn:       trcval = VEX_TRC_JMP_EMWARN;       break;
         case Ijk_MapFail:      trcval = VEX_TRC_JMP_MAPFAIL;      break;
         case Ijk_NoDecode:     trcval = VEX_TRC_JMP_NODECODE;     break;
         case Ijk_InvalICache:  trcval = VEX_TRC_JMP_INVALICACHE;  break;
         case Ijk_NoRedir:      trcval = VEX_TRC_JMP_NOREDIR;      break;
         case Ijk_SigTRAP:      trcval = VEX_TRC_JMP_SIGTRAP;      break;
         case Ijk_SigSEGV:      trcval = VEX_TRC_JMP_SIGSEGV;      break;
         case Ijk_Boring:       trcval = VEX_TRC_JMP_BORING;       break;
         /* We don't expect to see the following being assisted. */
         case Ijk_Ret:
         case Ijk_Call:
         /* fallthrough */
         default: 
            ppIRJumpKind(i->Xin.XAssisted.jk);
            vpanic("emit_X86Instr.Xin_XAssisted: unexpected jump kind");
      }
      vassert(trcval != 0);
      *p++ = 0xBD;
      p = emit32(p, trcval);

      /* movl $disp_indir, %edx */
      *p++ = 0xBA;
      p = emit32(p, (UInt)(Addr)disp_cp_xassisted);
      /* jmp *%edx */
      *p++ = 0xFF;
      *p++ = 0xE2;

      /* Fix up the conditional jump, if there was one. */
      if (i->Xin.XAssisted.cond != Xcc_ALWAYS) {
         Int delta = p - ptmp;
         vassert(delta > 0 && delta < 40);
         *ptmp = toUChar(delta-1);
      }
      goto done;
   }

   case Xin_CMov32:
      vassert(i->Xin.CMov32.cond != Xcc_ALWAYS);

      /* This generates cmov, which is illegal on P54/P55. */
      /*
      *p++ = 0x0F;
      *p++ = toUChar(0x40 + (0xF & i->Xin.CMov32.cond));
      if (i->Xin.CMov32.src->tag == Xrm_Reg) {
         p = doAMode_R(p, i->Xin.CMov32.dst, i->Xin.CMov32.src->Xrm.Reg.reg);
         goto done;
      }
      if (i->Xin.CMov32.src->tag == Xrm_Mem) {
         p = doAMode_M(p, i->Xin.CMov32.dst, i->Xin.CMov32.src->Xrm.Mem.am);
         goto done;
      }
      */

      /* Alternative version which works on any x86 variant. */
      /* jmp fwds if !condition */
      *p++ = toUChar(0x70 + (i->Xin.CMov32.cond ^ 1));
      *p++ = 0; /* # of bytes in the next bit, which we don't know yet */
      ptmp = p;

      switch (i->Xin.CMov32.src->tag) {
         case Xrm_Reg:
            /* Big sigh.  This is movl E -> G ... */
            *p++ = 0x89;
            p = doAMode_R(p, i->Xin.CMov32.src->Xrm.Reg.reg,
                             i->Xin.CMov32.dst);

            break;
         case Xrm_Mem:
            /* ... whereas this is movl G -> E.  That's why the args
               to doAMode_R appear to be the wrong way round in the
               Xrm_Reg case. */
            *p++ = 0x8B;
            p = doAMode_M(p, i->Xin.CMov32.dst,
                             i->Xin.CMov32.src->Xrm.Mem.am);
            break;
         default:
            goto bad;
      }
      /* Fill in the jump offset. */
      *(ptmp-1) = toUChar(p - ptmp);
      goto done;

      break;

   case Xin_LoadEX:
      if (i->Xin.LoadEX.szSmall == 1 && !i->Xin.LoadEX.syned) {
         /* movzbl */
         *p++ = 0x0F;
         *p++ = 0xB6;
         p = doAMode_M(p, i->Xin.LoadEX.dst, i->Xin.LoadEX.src); 
         goto done;
      }
      if (i->Xin.LoadEX.szSmall == 2 && !i->Xin.LoadEX.syned) {
         /* movzwl */
         *p++ = 0x0F;
         *p++ = 0xB7;
         p = doAMode_M(p, i->Xin.LoadEX.dst, i->Xin.LoadEX.src); 
         goto done;
      }
      if (i->Xin.LoadEX.szSmall == 1 && i->Xin.LoadEX.syned) {
         /* movsbl */
         *p++ = 0x0F;
         *p++ = 0xBE;
         p = doAMode_M(p, i->Xin.LoadEX.dst, i->Xin.LoadEX.src); 
         goto done;
      }
      break;

   case Xin_Set32:
      /* Make the destination register be 1 or 0, depending on whether
         the relevant condition holds.  We have to dodge and weave
         when the destination is %esi or %edi as we cannot directly
         emit the native 'setb %reg' for those.  Further complication:
         the top 24 bits of the destination should be forced to zero,
         but doing 'xor %r,%r' kills the flag(s) we are about to read.
         Sigh.  So start off my moving $0 into the dest. */

      /* Do we need to swap in %eax? */
      if (iregEnc(i->Xin.Set32.dst) >= 4) {
         /* xchg %eax, %dst */
         *p++ = toUChar(0x90 + iregEnc(i->Xin.Set32.dst));
         /* movl $0, %eax */
         *p++ =toUChar(0xB8 + iregEnc(hregX86_EAX()));
         p = emit32(p, 0);
         /* setb lo8(%eax) */
         *p++ = 0x0F; 
         *p++ = toUChar(0x90 + (0xF & i->Xin.Set32.cond));
         p = doAMode_R_enc_reg(p, 0, hregX86_EAX());
         /* xchg %eax, %dst */
         *p++ = toUChar(0x90 + iregEnc(i->Xin.Set32.dst));
      } else {
         /* movl $0, %dst */
         *p++ = toUChar(0xB8 + iregEnc(i->Xin.Set32.dst));
         p = emit32(p, 0);
         /* setb lo8(%dst) */
         *p++ = 0x0F; 
         *p++ = toUChar(0x90 + (0xF & i->Xin.Set32.cond));
         p = doAMode_R_enc_reg(p, 0, i->Xin.Set32.dst);
      }
      goto done;

   case Xin_Bsfr32:
      *p++ = 0x0F;
      if (i->Xin.Bsfr32.isFwds) {
         *p++ = 0xBC;
      } else {
         *p++ = 0xBD;
      }
      p = doAMode_R(p, i->Xin.Bsfr32.dst, i->Xin.Bsfr32.src);
      goto done;

   case Xin_MFence:
      /* see comment in hdefs.h re this insn */
      if (0) vex_printf("EMIT FENCE\n");
      if (i->Xin.MFence.hwcaps & (VEX_HWCAPS_X86_SSE3
                                  |VEX_HWCAPS_X86_SSE2)) {
         /* mfence */
         *p++ = 0x0F; *p++ = 0xAE; *p++ = 0xF0;
         goto done;
      }
      if (i->Xin.MFence.hwcaps & VEX_HWCAPS_X86_MMXEXT) {
         /* sfence */
         *p++ = 0x0F; *p++ = 0xAE; *p++ = 0xF8;
         /* lock addl $0,0(%esp) */
         *p++ = 0xF0; *p++ = 0x83; *p++ = 0x44; 
         *p++ = 0x24; *p++ = 0x00; *p++ = 0x00;
         goto done;
      }
      if (i->Xin.MFence.hwcaps == 0/*baseline, no SSE*/) {
         /* lock addl $0,0(%esp) */
         *p++ = 0xF0; *p++ = 0x83; *p++ = 0x44; 
         *p++ = 0x24; *p++ = 0x00; *p++ = 0x00;
         goto done;
      }
      vpanic("emit_X86Instr:mfence:hwcaps");
      /*NOTREACHED*/
      break;

   case Xin_ACAS:
      /* lock */
      *p++ = 0xF0;
      /* cmpxchg{b,w,l} %ebx,mem.  Expected-value in %eax, new value
         in %ebx.  The new-value register is hardwired to be %ebx
         since letting it be any integer register gives the problem
         that %sil and %dil are unaddressible on x86 and hence we
         would have to resort to the same kind of trickery as with
         byte-sized Xin.Store, just below.  Given that this isn't
         performance critical, it is simpler just to force the
         register operand to %ebx (could equally be %ecx or %edx).
         (Although %ebx is more consistent with cmpxchg8b.) */
      if (i->Xin.ACAS.sz == 2) *p++ = 0x66; 
      *p++ = 0x0F;
      if (i->Xin.ACAS.sz == 1) *p++ = 0xB0; else *p++ = 0xB1;
      p = doAMode_M(p, hregX86_EBX(), i->Xin.ACAS.addr);
      goto done;

   case Xin_DACAS:
      /* lock */
      *p++ = 0xF0;
      /* cmpxchg8b m64.  Expected-value in %edx:%eax, new value
         in %ecx:%ebx.  All 4 regs are hardwired in the ISA, so
         aren't encoded in the insn. */
      *p++ = 0x0F;
      *p++ = 0xC7;
      p = doAMode_M_enc(p, 1, i->Xin.DACAS.addr);
      goto done;

   case Xin_Store:
      if (i->Xin.Store.sz == 2) {
         /* This case, at least, is simple, given that we can
            reference the low 16 bits of any integer register. */
         *p++ = 0x66;
         *p++ = 0x89;
         p = doAMode_M(p, i->Xin.Store.src, i->Xin.Store.dst);
         goto done;
      }

      if (i->Xin.Store.sz == 1) {
         /* We have to do complex dodging and weaving if src is not
            the low 8 bits of %eax/%ebx/%ecx/%edx. */
         if (iregEnc(i->Xin.Store.src) < 4) {
            /* we're OK, can do it directly */
            *p++ = 0x88;
            p = doAMode_M(p, i->Xin.Store.src, i->Xin.Store.dst);
           goto done;
         } else {
            /* Bleh.  This means the source is %edi or %esi.  Since
               the address mode can only mention three registers, at
               least one of %eax/%ebx/%ecx/%edx must be available to
               temporarily swap the source into, so the store can
               happen.  So we have to look at the regs mentioned
               in the amode. */
            HReg swap = INVALID_HREG;
            HReg  eax = hregX86_EAX(), ebx = hregX86_EBX(), 
                  ecx = hregX86_ECX(), edx = hregX86_EDX();
            HRegUsage u;
            initHRegUsage(&u);
            addRegUsage_X86AMode(&u, i->Xin.Store.dst);
            /**/ if (! HRegUsage__contains(&u, eax)) { swap = eax; }
            else if (! HRegUsage__contains(&u, ebx)) { swap = ebx; }
            else if (! HRegUsage__contains(&u, ecx)) { swap = ecx; }
            else if (! HRegUsage__contains(&u, edx)) { swap = edx; }
            vassert(! hregIsInvalid(swap));
            /* xchgl %source, %swap. Could do better if swap is %eax. */
            *p++ = 0x87;
            p = doAMode_R(p, i->Xin.Store.src, swap);
            /* movb lo8{%swap}, (dst) */
            *p++ = 0x88;
            p = doAMode_M(p, swap, i->Xin.Store.dst);
            /* xchgl %source, %swap. Could do better if swap is %eax. */
            *p++ = 0x87;
            p = doAMode_R(p, i->Xin.Store.src, swap);
            goto done;
         }
      } /* if (i->Xin.Store.sz == 1) */
      break;

   case Xin_FpUnary:
      /* gop %src, %dst
         --> ffree %st7 ; fld %st(src) ; fop %st(0) ; fstp %st(1+dst)
      */
      p = do_ffree_st7(p);
      p = do_fld_st(p, 0+fregEnc(i->Xin.FpUnary.src));
      p = do_fop1_st(p, i->Xin.FpUnary.op);
      p = do_fstp_st(p, 1+fregEnc(i->Xin.FpUnary.dst));
      goto done;

   case Xin_FpBinary:
      if (i->Xin.FpBinary.op == Xfp_YL2X
          || i->Xin.FpBinary.op == Xfp_YL2XP1) {
         /* Have to do this specially. */
         /* ffree %st7 ; fld %st(srcL) ; 
            ffree %st7 ; fld %st(srcR+1) ; fyl2x{p1} ; fstp(1+dst) */
         p = do_ffree_st7(p);
         p = do_fld_st(p, 0+fregEnc(i->Xin.FpBinary.srcL));
         p = do_ffree_st7(p);
         p = do_fld_st(p, 1+fregEnc(i->Xin.FpBinary.srcR));
         *p++ = 0xD9; 
         *p++ = toUChar(i->Xin.FpBinary.op==Xfp_YL2X ? 0xF1 : 0xF9);
         p = do_fstp_st(p, 1+fregEnc(i->Xin.FpBinary.dst));
         goto done;
      }
      if (i->Xin.FpBinary.op == Xfp_ATAN) {
         /* Have to do this specially. */
         /* ffree %st7 ; fld %st(srcL) ; 
            ffree %st7 ; fld %st(srcR+1) ; fpatan ; fstp(1+dst) */
         p = do_ffree_st7(p);
         p = do_fld_st(p, 0+fregEnc(i->Xin.FpBinary.srcL));
         p = do_ffree_st7(p);
         p = do_fld_st(p, 1+fregEnc(i->Xin.FpBinary.srcR));
         *p++ = 0xD9; *p++ = 0xF3;
         p = do_fstp_st(p, 1+fregEnc(i->Xin.FpBinary.dst));
         goto done;
      }
      if (i->Xin.FpBinary.op == Xfp_PREM
          || i->Xin.FpBinary.op == Xfp_PREM1
          || i->Xin.FpBinary.op == Xfp_SCALE) {
         /* Have to do this specially. */
         /* ffree %st7 ; fld %st(srcR) ; 
            ffree %st7 ; fld %st(srcL+1) ; fprem/fprem1/fscale ; fstp(2+dst) ; 
            fincstp ; ffree %st7 */
         p = do_ffree_st7(p);
         p = do_fld_st(p, 0+fregEnc(i->Xin.FpBinary.srcR));
         p = do_ffree_st7(p);
         p = do_fld_st(p, 1+fregEnc(i->Xin.FpBinary.srcL));
         *p++ = 0xD9;
         switch (i->Xin.FpBinary.op) {
            case Xfp_PREM: *p++ = 0xF8; break;
            case Xfp_PREM1: *p++ = 0xF5; break;
            case Xfp_SCALE: *p++ =  0xFD; break;
            default: vpanic("emitX86Instr(FpBinary,PREM/PREM1/SCALE)");
         }
         p = do_fstp_st(p, 2+fregEnc(i->Xin.FpBinary.dst));
         *p++ = 0xD9; *p++ = 0xF7;
         p = do_ffree_st7(p);
         goto done;
      }
      /* General case */
      /* gop %srcL, %srcR, %dst
         --> ffree %st7 ; fld %st(srcL) ; fop %st(1+srcR) ; fstp %st(1+dst)
      */
      p = do_ffree_st7(p);
      p = do_fld_st(p, 0+fregEnc(i->Xin.FpBinary.srcL));
      p = do_fop2_st(p, i->Xin.FpBinary.op, 
                        1+fregEnc(i->Xin.FpBinary.srcR));
      p = do_fstp_st(p, 1+fregEnc(i->Xin.FpBinary.dst));
      goto done;

   case Xin_FpLdSt:
      if (i->Xin.FpLdSt.isLoad) {
         /* Load from memory into %fakeN.  
            --> ffree %st(7) ; fld{s/l/t} amode ; fstp st(N+1) 
         */
         p = do_ffree_st7(p);
         switch (i->Xin.FpLdSt.sz) {
            case 4:
               *p++ = 0xD9;
               p = doAMode_M_enc(p, 0/*subopcode*/, i->Xin.FpLdSt.addr);
               break;
            case 8:
               *p++ = 0xDD;
               p = doAMode_M_enc(p, 0/*subopcode*/, i->Xin.FpLdSt.addr);
               break;
            case 10:
               *p++ = 0xDB;
               p = doAMode_M_enc(p, 5/*subopcode*/, i->Xin.FpLdSt.addr);
               break;
            default:
               vpanic("emitX86Instr(FpLdSt,load)");
         }
         p = do_fstp_st(p, 1+fregEnc(i->Xin.FpLdSt.reg));
         goto done;
      } else {
         /* Store from %fakeN into memory.
            --> ffree %st(7) ; fld st(N) ; fstp{l|s} amode
	 */
         p = do_ffree_st7(p);
         p = do_fld_st(p, 0+fregEnc(i->Xin.FpLdSt.reg));
         switch (i->Xin.FpLdSt.sz) {
            case 4:
               *p++ = 0xD9;
               p = doAMode_M_enc(p, 3/*subopcode*/, i->Xin.FpLdSt.addr);
               break;
            case 8:
               *p++ = 0xDD;
               p = doAMode_M_enc(p, 3/*subopcode*/, i->Xin.FpLdSt.addr);
               break;
            case 10:
               *p++ = 0xDB;
               p = doAMode_M_enc(p, 7/*subopcode*/, i->Xin.FpLdSt.addr);
               break;
            default:
               vpanic("emitX86Instr(FpLdSt,store)");
         }
         goto done;
      }
      break;

   case Xin_FpLdStI:
      if (i->Xin.FpLdStI.isLoad) {
         /* Load from memory into %fakeN, converting from an int.  
            --> ffree %st(7) ; fild{w/l/ll} amode ; fstp st(N+1) 
         */
         switch (i->Xin.FpLdStI.sz) {
            case 8:  opc = 0xDF; subopc_imm = 5; break;
            case 4:  opc = 0xDB; subopc_imm = 0; break;
            case 2:  vassert(0); opc = 0xDF; subopc_imm = 0; break;
            default: vpanic("emitX86Instr(Xin_FpLdStI-load)");
         }
         p = do_ffree_st7(p);
         *p++ = toUChar(opc);
         p = doAMode_M_enc(p, subopc_imm/*subopcode*/, i->Xin.FpLdStI.addr);
         p = do_fstp_st(p, 1+fregEnc(i->Xin.FpLdStI.reg));
         goto done;
      } else {
         /* Store from %fakeN into memory, converting to an int.
            --> ffree %st(7) ; fld st(N) ; fistp{w/l/ll} amode
	 */
         switch (i->Xin.FpLdStI.sz) {
            case 8:  opc = 0xDF; subopc_imm = 7; break;
            case 4:  opc = 0xDB; subopc_imm = 3; break;
            case 2:  opc = 0xDF; subopc_imm = 3; break;
            default: vpanic("emitX86Instr(Xin_FpLdStI-store)");
         }
         p = do_ffree_st7(p);
         p = do_fld_st(p, 0+fregEnc(i->Xin.FpLdStI.reg));
         *p++ = toUChar(opc);
         p = doAMode_M_enc(p, subopc_imm/*subopcode*/, i->Xin.FpLdStI.addr);
         goto done;
      }
      break;

   case Xin_Fp64to32:
      /* ffree %st7 ; fld %st(src) */
      p = do_ffree_st7(p);
      p = do_fld_st(p, 0+fregEnc(i->Xin.Fp64to32.src));
      /* subl $4, %esp */
      *p++ = 0x83; *p++ = 0xEC; *p++ = 0x04;
      /* fstps (%esp) */
      *p++ = 0xD9; *p++ = 0x1C; *p++ = 0x24;
      /* flds (%esp) */
      *p++ = 0xD9; *p++ = 0x04; *p++ = 0x24;
      /* addl $4, %esp */
      *p++ = 0x83; *p++ = 0xC4; *p++ = 0x04;
      /* fstp %st(1+dst) */
      p = do_fstp_st(p, 1+fregEnc(i->Xin.Fp64to32.dst));
      goto done;

   case Xin_FpCMov:
      /* jmp fwds if !condition */
      *p++ = toUChar(0x70 + (i->Xin.FpCMov.cond ^ 1));
      *p++ = 0; /* # of bytes in the next bit, which we don't know yet */
      ptmp = p;

      /* ffree %st7 ; fld %st(src) ; fstp %st(1+dst) */
      p = do_ffree_st7(p);
      p = do_fld_st(p, 0+fregEnc(i->Xin.FpCMov.src));
      p = do_fstp_st(p, 1+fregEnc(i->Xin.FpCMov.dst));

      /* Fill in the jump offset. */
      *(ptmp-1) = toUChar(p - ptmp);
      goto done;

   case Xin_FpLdCW:
      *p++ = 0xD9;
      p = doAMode_M_enc(p, 5/*subopcode*/, i->Xin.FpLdCW.addr);
      goto done;

   case Xin_FpStSW_AX:
      /* note, this emits fnstsw %ax, not fstsw %ax */
      *p++ = 0xDF;
      *p++ = 0xE0;
      goto done;

   case Xin_FpCmp:
      /* gcmp %fL, %fR, %dst
         -> ffree %st7; fpush %fL ; fucomp %(fR+1) ; 
            fnstsw %ax ; movl %eax, %dst 
      */
      /* ffree %st7 */
      p = do_ffree_st7(p);
      /* fpush %fL */
      p = do_fld_st(p, 0+fregEnc(i->Xin.FpCmp.srcL));
      /* fucomp %(fR+1) */
      *p++ = 0xDD;
      *p++ = toUChar(0xE8 + (7 & (1+fregEnc(i->Xin.FpCmp.srcR))));
      /* fnstsw %ax */
      *p++ = 0xDF;
      *p++ = 0xE0;
      /*  movl %eax, %dst */
      *p++ = 0x89;
      p = doAMode_R(p, hregX86_EAX(), i->Xin.FpCmp.dst);
      goto done;

   case Xin_SseConst: {
      UShort con = i->Xin.SseConst.con;
      p = push_word_from_tags(p, toUShort((con >> 12) & 0xF));
      p = push_word_from_tags(p, toUShort((con >> 8) & 0xF));
      p = push_word_from_tags(p, toUShort((con >> 4) & 0xF));
      p = push_word_from_tags(p, toUShort(con & 0xF));
      /* movl (%esp), %xmm-dst */
      *p++ = 0x0F;
      *p++ = 0x10;
      *p++ = toUChar(0x04 + 8 * (7 & vregEnc(i->Xin.SseConst.dst)));
      *p++ = 0x24;
      /* addl $16, %esp */
      *p++ = 0x83;
      *p++ = 0xC4;
      *p++ = 0x10;
      goto done;
   }

   case Xin_SseLdSt:
      *p++ = 0x0F; 
      *p++ = toUChar(i->Xin.SseLdSt.isLoad ? 0x10 : 0x11);
      p = doAMode_M_enc(p, vregEnc(i->Xin.SseLdSt.reg), i->Xin.SseLdSt.addr);
      goto done;

   case Xin_SseLdzLO:
      vassert(i->Xin.SseLdzLO.sz == 4 || i->Xin.SseLdzLO.sz == 8);
      /* movs[sd] amode, %xmm-dst */
      *p++ = toUChar(i->Xin.SseLdzLO.sz==4 ? 0xF3 : 0xF2);
      *p++ = 0x0F; 
      *p++ = 0x10; 
      p = doAMode_M_enc(p, vregEnc(i->Xin.SseLdzLO.reg), i->Xin.SseLdzLO.addr);
      goto done;

   case Xin_Sse32Fx4:
      xtra = 0;
      *p++ = 0x0F;
      switch (i->Xin.Sse32Fx4.op) {
         case Xsse_ADDF:   *p++ = 0x58; break;
         case Xsse_DIVF:   *p++ = 0x5E; break;
         case Xsse_MAXF:   *p++ = 0x5F; break;
         case Xsse_MINF:   *p++ = 0x5D; break;
         case Xsse_MULF:   *p++ = 0x59; break;
         case Xsse_RCPF:   *p++ = 0x53; break;
         case Xsse_RSQRTF: *p++ = 0x52; break;
         case Xsse_SQRTF:  *p++ = 0x51; break;
         case Xsse_SUBF:   *p++ = 0x5C; break;
         case Xsse_CMPEQF: *p++ = 0xC2; xtra = 0x100; break;
         case Xsse_CMPLTF: *p++ = 0xC2; xtra = 0x101; break;
         case Xsse_CMPLEF: *p++ = 0xC2; xtra = 0x102; break;
         case Xsse_CMPUNF: *p++ = 0xC2; xtra = 0x103; break;
         default: goto bad;
      }
      p = doAMode_R_enc_enc(p, vregEnc(i->Xin.Sse32Fx4.dst),
                               vregEnc(i->Xin.Sse32Fx4.src) );
      if (xtra & 0x100)
         *p++ = toUChar(xtra & 0xFF);
      goto done;

   case Xin_Sse64Fx2:
      xtra = 0;
      *p++ = 0x66;
      *p++ = 0x0F;
      switch (i->Xin.Sse64Fx2.op) {
         case Xsse_ADDF:   *p++ = 0x58; break;
         case Xsse_DIVF:   *p++ = 0x5E; break;
         case Xsse_MAXF:   *p++ = 0x5F; break;
         case Xsse_MINF:   *p++ = 0x5D; break;
         case Xsse_MULF:   *p++ = 0x59; break;
         case Xsse_RCPF:   *p++ = 0x53; break;
         case Xsse_RSQRTF: *p++ = 0x52; break;
         case Xsse_SQRTF:  *p++ = 0x51; break;
         case Xsse_SUBF:   *p++ = 0x5C; break;
         case Xsse_CMPEQF: *p++ = 0xC2; xtra = 0x100; break;
         case Xsse_CMPLTF: *p++ = 0xC2; xtra = 0x101; break;
         case Xsse_CMPLEF: *p++ = 0xC2; xtra = 0x102; break;
         case Xsse_CMPUNF: *p++ = 0xC2; xtra = 0x103; break;
         default: goto bad;
      }
      p = doAMode_R_enc_enc(p, vregEnc(i->Xin.Sse64Fx2.dst),
                               vregEnc(i->Xin.Sse64Fx2.src) );
      if (xtra & 0x100)
         *p++ = toUChar(xtra & 0xFF);
      goto done;

   case Xin_Sse32FLo:
      xtra = 0;
      *p++ = 0xF3;
      *p++ = 0x0F;
      switch (i->Xin.Sse32FLo.op) {
         case Xsse_ADDF:   *p++ = 0x58; break;
         case Xsse_DIVF:   *p++ = 0x5E; break;
         case Xsse_MAXF:   *p++ = 0x5F; break;
         case Xsse_MINF:   *p++ = 0x5D; break;
         case Xsse_MULF:   *p++ = 0x59; break;
         case Xsse_RCPF:   *p++ = 0x53; break;
         case Xsse_RSQRTF: *p++ = 0x52; break;
         case Xsse_SQRTF:  *p++ = 0x51; break;
         case Xsse_SUBF:   *p++ = 0x5C; break;
         case Xsse_CMPEQF: *p++ = 0xC2; xtra = 0x100; break;
         case Xsse_CMPLTF: *p++ = 0xC2; xtra = 0x101; break;
         case Xsse_CMPLEF: *p++ = 0xC2; xtra = 0x102; break;
         case Xsse_CMPUNF: *p++ = 0xC2; xtra = 0x103; break;
         default: goto bad;
      }
      p = doAMode_R_enc_enc(p, vregEnc(i->Xin.Sse32FLo.dst),
                               vregEnc(i->Xin.Sse32FLo.src) );
      if (xtra & 0x100)
         *p++ = toUChar(xtra & 0xFF);
      goto done;

   case Xin_Sse64FLo:
      xtra = 0;
      *p++ = 0xF2;
      *p++ = 0x0F;
      switch (i->Xin.Sse64FLo.op) {
         case Xsse_ADDF:   *p++ = 0x58; break;
         case Xsse_DIVF:   *p++ = 0x5E; break;
         case Xsse_MAXF:   *p++ = 0x5F; break;
         case Xsse_MINF:   *p++ = 0x5D; break;
         case Xsse_MULF:   *p++ = 0x59; break;
         case Xsse_RCPF:   *p++ = 0x53; break;
         case Xsse_RSQRTF: *p++ = 0x52; break;
         case Xsse_SQRTF:  *p++ = 0x51; break;
         case Xsse_SUBF:   *p++ = 0x5C; break;
         case Xsse_CMPEQF: *p++ = 0xC2; xtra = 0x100; break;
         case Xsse_CMPLTF: *p++ = 0xC2; xtra = 0x101; break;
         case Xsse_CMPLEF: *p++ = 0xC2; xtra = 0x102; break;
         case Xsse_CMPUNF: *p++ = 0xC2; xtra = 0x103; break;
         default: goto bad;
      }
      p = doAMode_R_enc_enc(p, vregEnc(i->Xin.Sse64FLo.dst),
                               vregEnc(i->Xin.Sse64FLo.src) );
      if (xtra & 0x100)
         *p++ = toUChar(xtra & 0xFF);
      goto done;

   case Xin_SseReRg:
#     define XX(_n) *p++ = (_n)
      switch (i->Xin.SseReRg.op) {
         case Xsse_MOV:     /*movups*/ XX(0x0F); XX(0x10); break;
         case Xsse_OR:                 XX(0x0F); XX(0x56); break;
         case Xsse_XOR:                XX(0x0F); XX(0x57); break;
         case Xsse_AND:                XX(0x0F); XX(0x54); break;
         case Xsse_PACKSSD:  XX(0x66); XX(0x0F); XX(0x6B); break;
         case Xsse_PACKSSW:  XX(0x66); XX(0x0F); XX(0x63); break;
         case Xsse_PACKUSW:  XX(0x66); XX(0x0F); XX(0x67); break;
         case Xsse_ADD8:     XX(0x66); XX(0x0F); XX(0xFC); break;
         case Xsse_ADD16:    XX(0x66); XX(0x0F); XX(0xFD); break;
         case Xsse_ADD32:    XX(0x66); XX(0x0F); XX(0xFE); break;
         case Xsse_ADD64:    XX(0x66); XX(0x0F); XX(0xD4); break;
         case Xsse_QADD8S:   XX(0x66); XX(0x0F); XX(0xEC); break;
         case Xsse_QADD16S:  XX(0x66); XX(0x0F); XX(0xED); break;
         case Xsse_QADD8U:   XX(0x66); XX(0x0F); XX(0xDC); break;
         case Xsse_QADD16U:  XX(0x66); XX(0x0F); XX(0xDD); break;
         case Xsse_AVG8U:    XX(0x66); XX(0x0F); XX(0xE0); break;
         case Xsse_AVG16U:   XX(0x66); XX(0x0F); XX(0xE3); break;
         case Xsse_CMPEQ8:   XX(0x66); XX(0x0F); XX(0x74); break;
         case Xsse_CMPEQ16:  XX(0x66); XX(0x0F); XX(0x75); break;
         case Xsse_CMPEQ32:  XX(0x66); XX(0x0F); XX(0x76); break;
         case Xsse_CMPGT8S:  XX(0x66); XX(0x0F); XX(0x64); break;
         case Xsse_CMPGT16S: XX(0x66); XX(0x0F); XX(0x65); break;
         case Xsse_CMPGT32S: XX(0x66); XX(0x0F); XX(0x66); break;
         case Xsse_MAX16S:   XX(0x66); XX(0x0F); XX(0xEE); break;
         case Xsse_MAX8U:    XX(0x66); XX(0x0F); XX(0xDE); break;
         case Xsse_MIN16S:   XX(0x66); XX(0x0F); XX(0xEA); break;
         case Xsse_MIN8U:    XX(0x66); XX(0x0F); XX(0xDA); break;
         case Xsse_MULHI16U: XX(0x66); XX(0x0F); XX(0xE4); break;
         case Xsse_MULHI16S: XX(0x66); XX(0x0F); XX(0xE5); break;
         case Xsse_MUL16:    XX(0x66); XX(0x0F); XX(0xD5); break;
         case Xsse_SHL16:    XX(0x66); XX(0x0F); XX(0xF1); break;
         case Xsse_SHL32:    XX(0x66); XX(0x0F); XX(0xF2); break;
         case Xsse_SHL64:    XX(0x66); XX(0x0F); XX(0xF3); break;
         case Xsse_SAR16:    XX(0x66); XX(0x0F); XX(0xE1); break;
         case Xsse_SAR32:    XX(0x66); XX(0x0F); XX(0xE2); break;
         case Xsse_SHR16:    XX(0x66); XX(0x0F); XX(0xD1); break;
         case Xsse_SHR32:    XX(0x66); XX(0x0F); XX(0xD2); break;
         case Xsse_SHR64:    XX(0x66); XX(0x0F); XX(0xD3); break;
         case Xsse_SUB8:     XX(0x66); XX(0x0F); XX(0xF8); break;
         case Xsse_SUB16:    XX(0x66); XX(0x0F); XX(0xF9); break;
         case Xsse_SUB32:    XX(0x66); XX(0x0F); XX(0xFA); break;
         case Xsse_SUB64:    XX(0x66); XX(0x0F); XX(0xFB); break;
         case Xsse_QSUB8S:   XX(0x66); XX(0x0F); XX(0xE8); break;
         case Xsse_QSUB16S:  XX(0x66); XX(0x0F); XX(0xE9); break;
         case Xsse_QSUB8U:   XX(0x66); XX(0x0F); XX(0xD8); break;
         case Xsse_QSUB16U:  XX(0x66); XX(0x0F); XX(0xD9); break;
         case Xsse_UNPCKHB:  XX(0x66); XX(0x0F); XX(0x68); break;
         case Xsse_UNPCKHW:  XX(0x66); XX(0x0F); XX(0x69); break;
         case Xsse_UNPCKHD:  XX(0x66); XX(0x0F); XX(0x6A); break;
         case Xsse_UNPCKHQ:  XX(0x66); XX(0x0F); XX(0x6D); break;
         case Xsse_UNPCKLB:  XX(0x66); XX(0x0F); XX(0x60); break;
         case Xsse_UNPCKLW:  XX(0x66); XX(0x0F); XX(0x61); break;
         case Xsse_UNPCKLD:  XX(0x66); XX(0x0F); XX(0x62); break;
         case Xsse_UNPCKLQ:  XX(0x66); XX(0x0F); XX(0x6C); break;
         default: goto bad;
      }
      p = doAMode_R_enc_enc(p, vregEnc(i->Xin.SseReRg.dst),
                               vregEnc(i->Xin.SseReRg.src) );
#     undef XX
      goto done;

   case Xin_SseCMov:
      /* jmp fwds if !condition */
      *p++ = toUChar(0x70 + (i->Xin.SseCMov.cond ^ 1));
      *p++ = 0; /* # of bytes in the next bit, which we don't know yet */
      ptmp = p;

      /* movaps %src, %dst */
      *p++ = 0x0F; 
      *p++ = 0x28; 
      p = doAMode_R_enc_enc(p, vregEnc(i->Xin.SseCMov.dst),
                               vregEnc(i->Xin.SseCMov.src) );

      /* Fill in the jump offset. */
      *(ptmp-1) = toUChar(p - ptmp);
      goto done;

   case Xin_SseShuf:
      *p++ = 0x66; 
      *p++ = 0x0F; 
      *p++ = 0x70; 
      p = doAMode_R_enc_enc(p, vregEnc(i->Xin.SseShuf.dst),
                               vregEnc(i->Xin.SseShuf.src) );
      *p++ = (UChar)(i->Xin.SseShuf.order);
      goto done;

   case Xin_EvCheck: {
      /* We generate:
            (3 bytes)  decl 4(%ebp)    4 == offsetof(host_EvC_COUNTER)
            (2 bytes)  jns  nofail     expected taken
            (3 bytes)  jmp* 0(%ebp)    0 == offsetof(host_EvC_FAILADDR)
            nofail:
      */
      /* This is heavily asserted re instruction lengths.  It needs to
         be.  If we get given unexpected forms of .amCounter or
         .amFailAddr -- basically, anything that's not of the form
         uimm7(%ebp) -- they are likely to fail. */
      /* Note also that after the decl we must be very careful not to
         read the carry flag, else we get a partial flags stall.
         js/jns avoids that, though. */
      UChar* p0 = p;
      /* ---  decl 8(%ebp) --- */
      /* "1" because + there's no register in this encoding;
         instead the register + field is used as a sub opcode.  The
         encoding for "decl r/m32" + is FF /1, hence the "1". */
      *p++ = 0xFF;
      p = doAMode_M_enc(p, 1, i->Xin.EvCheck.amCounter);
      vassert(p - p0 == 3);
      /* --- jns nofail --- */
      *p++ = 0x79;
      *p++ = 0x03; /* need to check this 0x03 after the next insn */
      vassert(p - p0 == 5);
      /* --- jmp* 0(%ebp) --- */
      /* The encoding is FF /4. */
      *p++ = 0xFF;
      p = doAMode_M_enc(p, 4, i->Xin.EvCheck.amFailAddr);
      vassert(p - p0 == 8); /* also ensures that 0x03 offset above is ok */
      /* And crosscheck .. */
      vassert(evCheckSzB_X86() == 8);
      goto done;
   }

   case Xin_ProfInc: {
      /* We generate   addl $1,NotKnownYet
                       adcl $0,NotKnownYet+4
         in the expectation that a later call to LibVEX_patchProfCtr
         will be used to fill in the immediate fields once the right
         value is known.
           83 05  00 00 00 00  01
           83 15  00 00 00 00  00
      */
      *p++ = 0x83; *p++ = 0x05;
      *p++ = 0x00; *p++ = 0x00; *p++ = 0x00; *p++ = 0x00;
      *p++ = 0x01;
      *p++ = 0x83; *p++ = 0x15;
      *p++ = 0x00; *p++ = 0x00; *p++ = 0x00; *p++ = 0x00;
      *p++ = 0x00;
      /* Tell the caller .. */
      vassert(!(*is_profInc));
      *is_profInc = True;
      goto done;
   }

   default: 
      goto bad;
   }

  bad:
   ppX86Instr(i, mode64);
   vpanic("emit_X86Instr");
   /*NOTREACHED*/
   
  done:
   vassert(p - &buf[0] <= 32);
   return p - &buf[0];
}


/* How big is an event check?  See case for Xin_EvCheck in
   emit_X86Instr just above.  That crosschecks what this returns, so
   we can tell if we're inconsistent. */
Int evCheckSzB_X86 (void)
{
   return 8;
}


/* NB: what goes on here has to be very closely coordinated with the
   emitInstr case for XDirect, above. */
VexInvalRange chainXDirect_X86 ( VexEndness endness_host,
                                 void* place_to_chain,
                                 const void* disp_cp_chain_me_EXPECTED,
                                 const void* place_to_jump_to )
{
   vassert(endness_host == VexEndnessLE);

   /* What we're expecting to see is:
        movl $disp_cp_chain_me_EXPECTED, %edx
        call *%edx
      viz
        BA <4 bytes value == disp_cp_chain_me_EXPECTED>
        FF D2
   */
   UChar* p = (UChar*)place_to_chain;
   vassert(p[0] == 0xBA);
   vassert(read_misaligned_UInt_LE(&p[1])
           == (UInt)(Addr)disp_cp_chain_me_EXPECTED);
   vassert(p[5] == 0xFF);
   vassert(p[6] == 0xD2);
   /* And what we want to change it to is:
          jmp disp32   where disp32 is relative to the next insn
          ud2;
        viz
          E9 <4 bytes == disp32>
          0F 0B
      The replacement has the same length as the original.
   */
   /* This is the delta we need to put into a JMP d32 insn.  It's
      relative to the start of the next insn, hence the -5.  */
   Long delta = (Long)((const UChar *)place_to_jump_to - p) - 5;

   /* And make the modifications. */
   p[0] = 0xE9;
   write_misaligned_UInt_LE(&p[1], (UInt)(ULong)delta);
   p[5] = 0x0F; p[6] = 0x0B;
   /* sanity check on the delta -- top 32 are all 0 or all 1 */
   delta >>= 32;
   vassert(delta == 0LL || delta == -1LL);
   VexInvalRange vir = { (HWord)place_to_chain, 7 };
   return vir;
}


/* NB: what goes on here has to be very closely coordinated with the
   emitInstr case for XDirect, above. */
VexInvalRange unchainXDirect_X86 ( VexEndness endness_host,
                                   void* place_to_unchain,
                                   const void* place_to_jump_to_EXPECTED,
                                   const void* disp_cp_chain_me )
{
   vassert(endness_host == VexEndnessLE);

   /* What we're expecting to see is:
          jmp d32
          ud2;
       viz
          E9 <4 bytes == disp32>
          0F 0B
   */
   UChar* p     = (UChar*)place_to_unchain;
   Bool   valid = False;
   if (p[0] == 0xE9 
       && p[5] == 0x0F && p[6]  == 0x0B) {
      /* Check the offset is right. */
      Int s32 = (Int)read_misaligned_UInt_LE(&p[1]);
      if ((UChar*)p + 5 + s32 == place_to_jump_to_EXPECTED) {
         valid = True;
         if (0)
            vex_printf("QQQ unchainXDirect_X86: found valid\n");
      }
   }
   vassert(valid);
   /* And what we want to change it to is:
         movl $disp_cp_chain_me, %edx
         call *%edx
      viz
         BA <4 bytes value == disp_cp_chain_me_EXPECTED>
         FF D2
      So it's the same length (convenient, huh).
   */
   p[0] = 0xBA;
   write_misaligned_UInt_LE(&p[1], (UInt)(Addr)disp_cp_chain_me);
   p[5] = 0xFF;
   p[6] = 0xD2;
   VexInvalRange vir = { (HWord)place_to_unchain, 7 };
   return vir;
}


/* Patch the counter address into a profile inc point, as previously
   created by the Xin_ProfInc case for emit_X86Instr. */
VexInvalRange patchProfInc_X86 ( VexEndness endness_host,
                                 void*  place_to_patch,
                                 const ULong* location_of_counter )
{
   vassert(endness_host == VexEndnessLE);
   vassert(sizeof(ULong*) == 4);
   UChar* p = (UChar*)place_to_patch;
   vassert(p[0] == 0x83);
   vassert(p[1] == 0x05);
   vassert(p[2] == 0x00);
   vassert(p[3] == 0x00);
   vassert(p[4] == 0x00);
   vassert(p[5] == 0x00);
   vassert(p[6] == 0x01);
   vassert(p[7] == 0x83);
   vassert(p[8] == 0x15);
   vassert(p[9] == 0x00);
   vassert(p[10] == 0x00);
   vassert(p[11] == 0x00);
   vassert(p[12] == 0x00);
   vassert(p[13] == 0x00);
   UInt imm32 = (UInt)(Addr)location_of_counter;
   p[2] = imm32 & 0xFF; imm32 >>= 8;
   p[3] = imm32 & 0xFF; imm32 >>= 8;
   p[4] = imm32 & 0xFF; imm32 >>= 8;
   p[5] = imm32 & 0xFF;
   imm32 = 4 + (UInt)(Addr)location_of_counter;
   p[9]  = imm32 & 0xFF; imm32 >>= 8;
   p[10] = imm32 & 0xFF; imm32 >>= 8;
   p[11] = imm32 & 0xFF; imm32 >>= 8;
   p[12] = imm32 & 0xFF;
   VexInvalRange vir = { (HWord)place_to_patch, 14 };
   return vir;
}


/*---------------------------------------------------------------*/
/*--- end                                     host_x86_defs.c ---*/
/*---------------------------------------------------------------*/
