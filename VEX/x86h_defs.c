
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (x86h_defs.c) is                              ---*/
/*--- Copyright (c) 2004 OpenWorks LLP.  All rights reserved. ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

#include <stdio.h>
#include <malloc.h>

#include "basictypes.h"
#include "host_regs.h"
#include "x86h_defs.h"


/* --------- Registers. --------- */

void ppHRegX86 ( FILE* f, HReg reg ) 
{
   Int r;
   static Char* ireg32_names[8] 
     = { "%eax", "%ecx", "%edx", "%ebx", "%esp", "%ebp", "%esi", "%edi" };
   /* Be generic for all virtual regs. */
   if (hregIsVirtual(reg)) {
      ppHReg(f, reg);
      return;
   }
   /* But specific for real regs. */
   switch (hregClass(reg)) {
      case HRcInt:
         r = hregNumber(reg);
         assert(r >= 0 && r < 8);
         fprintf(f, "%s", ireg32_names[r]);
         return;
      case HRcFloat:
         r = hregNumber(reg);
         assert(r >= 0 && r < 6);
         fprintf(f, "%%fake%d", r);
         return;
      case HRcVector:
         panic("ppHRegX86: real vector reg");
     default:
         panic("ppHRegX86");
   }
}

HReg hregX86_EAX ( void ) { return mkHReg(0, HRcInt, False); }
HReg hregX86_EBX ( void ) { return mkHReg(3, HRcInt, False); }
HReg hregX86_ECX ( void ) { return mkHReg(1, HRcInt, False); }
HReg hregX86_EDX ( void ) { return mkHReg(2, HRcInt, False); }
HReg hregX86_EBP ( void ) { return mkHReg(5, HRcInt, False); }


/* --------- X86AMode: memory address expressions. --------- */

X86AMode* X86AMode_IR ( UInt imm32, HReg reg ) {
   X86AMode* am = malloc(sizeof(X86AMode));
   am->tag = Xam_IR;
   am->Xam.IR.imm = imm32;
   am->Xam.IR.reg = reg;
   return am;
}

X86AMode* X86AMode_IRRS ( UInt imm32, HReg base, HReg index, Int shift ) {
   X86AMode* am = malloc(sizeof(X86AMode));
   am->tag = Xam_IRRS;
   am->Xam.IRRS.imm = imm32;
   am->Xam.IRRS.base = base;
   am->Xam.IRRS.index = index;
   am->Xam.IRRS.shift = shift;
   assert(shift >= 0 && shift <= 3);
   return am;
}

void ppX86AMode ( FILE* f, X86AMode* am ) {
   switch (am->tag) {
      case Xam_IR: 
         fprintf(f, "0x%x(", am->Xam.IR.imm);
         ppHRegX86(f, am->Xam.IR.reg);
         fprintf(f, ")");
         return;
      case Xam_IRRS:
         fprintf(f, "0x%x(", am->Xam.IRRS.imm);
         ppHRegX86(f, am->Xam.IRRS.base);
         fprintf(f, ",");
         ppHRegX86(f, am->Xam.IRRS.index);
         fprintf(f, ",%d)", am->Xam.IRRS.shift);
         return;
      default:
         panic("ppX86AMode");
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
         panic("addRegUsage_X86AMode");
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
         panic("mapRegs_X86AMode");
   }
}

/* --------- Operand, which can be reg, immediate or memory. --------- */

X86RMI* X86RMI_Imm ( UInt imm32 ) {
   X86RMI* op         = malloc(sizeof(X86RMI));
   op->tag            = Xrmi_Imm;
   op->Xrmi.Imm.imm32 = imm32;
   return op;
}

X86RMI* X86RMI_Reg ( HReg reg ) {
   X86RMI* op       = malloc(sizeof(X86RMI));
   op->tag          = Xrmi_Reg;
   op->Xrmi.Reg.reg = reg;
   return op;
}

X86RMI* X86RMI_Mem ( X86AMode* am ) {
   X86RMI* op      = malloc(sizeof(X86RMI));
   op->tag         = Xrmi_Mem;
   op->Xrmi.Mem.am = am;
   return op;
}

void ppX86RMI ( FILE* f, X86RMI* op ) {
   switch (op->tag) {
      case Xrmi_Imm: 
         fprintf(f, "$0x%x", op->Xrmi.Imm.imm32);
         return;
      case Xrmi_Reg: 
         ppHRegX86(f, op->Xrmi.Reg.reg);
         return;
      case Xrmi_Mem: 
         ppX86AMode(f, op->Xrmi.Mem.am);
         return;
     default: 
         panic("ppX86RMI");
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
         panic("addRegUsage_X86RMI");
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
         panic("mapRegs_X86RMI");
   }
}


/* --------- Operand, which can be reg or immediate only. --------- */

X86RI* X86RI_Imm ( UInt imm32 ) {
   X86RI* op         = malloc(sizeof(X86RI));
   op->tag           = Xri_Imm;
   op->Xri.Imm.imm32 = imm32;
   return op;
}

X86RI* X86RI_Reg ( HReg reg ) {
   X86RI* op       = malloc(sizeof(X86RI));
   op->tag         = Xri_Reg;
   op->Xri.Reg.reg = reg;
   return op;
}

void ppX86RI ( FILE* f, X86RI* op ) {
   switch (op->tag) {
      case Xri_Imm: 
         fprintf(f, "$0x%x", op->Xri.Imm.imm32);
         return;
      case Xri_Reg: 
         ppHRegX86(f, op->Xri.Reg.reg);
         return;
     default: 
         panic("ppX86RI");
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
         panic("addRegUsage_X86RI");
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
         panic("mapRegs_X86RI");
   }
}


/* --------- Operand, which can be reg or memory only. --------- */

X86RM* X86RM_Reg ( HReg reg ) {
   X86RM* op       = malloc(sizeof(X86RM));
   op->tag         = Xrm_Reg;
   op->Xrm.Reg.reg = reg;
   return op;
}

X86RM* X86RM_Mem ( X86AMode* am ) {
   X86RM* op      = malloc(sizeof(X86RM));
   op->tag        = Xrm_Mem;
   op->Xrm.Mem.am = am;
   return op;
}

void ppX86RM ( FILE* f, X86RM* op ) {
   switch (op->tag) {
      case Xrm_Mem: 
         ppX86AMode(f, op->Xrm.Mem.am);
         return;
      case Xrm_Reg: 
         ppHRegX86(f, op->Xrm.Reg.reg);
         return;
     default: 
         panic("ppX86RM");
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
         panic("addRegUsage_X86RM");
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
         panic("mapRegs_X86RM");
   }
}


/* --------- Instructions. --------- */

void ppX86AluOp ( FILE* f, X86AluOp op ) {
   Char* name;
   switch (op) {
      case Xalu_MOV: name = "mov"; break;
      case Xalu_ADD: name = "add"; break;
      case Xalu_SUB: name = "sub"; break;
      case Xalu_ADC: name = "adc"; break;
      case Xalu_SBB: name = "sbb"; break;
      case Xalu_AND: name = "and"; break;
      case Xalu_OR:  name = "or";  break;
      case Xalu_XOR: name = "xor"; break;
      default: panic("ppX86AluOp");
   }
   fprintf(f, "%s", name);
}

void ppX86ShiftOp ( FILE* f, X86ShiftOp op ) {
   Char* name;
   switch (op) {
      case Xsh_SHL: name = "shl"; break;
      case Xsh_SHR: name = "shr"; break;
      case Xsh_SAR: name = "sar"; break;
      case Xsh_ROL: name = "rol"; break;
      case Xsh_ROR: name = "ror"; break;
      default: panic("ppX86ShiftOp");
   }
   fprintf(f, "%s", name);
}

X86Instr* X86Instr_Alu32R ( X86AluOp op, X86RMI* src, HReg dst ) {
   X86Instr* i       = malloc(sizeof(X86Instr));
   i->tag            = Xin_Alu32R;
   i->Xin.Alu32R.op  = op;
   i->Xin.Alu32R.src = src;
   i->Xin.Alu32R.dst = dst;
   return i;
}

X86Instr* X86Instr_Alu32M ( X86AluOp op, X86RI* src, X86AMode* dst ) {
   X86Instr* i       = malloc(sizeof(X86Instr));
   i->tag            = Xin_Alu32M;
   i->Xin.Alu32M.op  = op;
   i->Xin.Alu32M.src = src;
   i->Xin.Alu32M.dst = dst;
   return i;
}

X86Instr* X86Instr_Sh32 ( X86ShiftOp op, UInt src, X86RM* dst ) {
   X86Instr* i     = malloc(sizeof(X86Instr));
   i->tag          = Xin_Sh32;
   i->Xin.Sh32.op  = op;
   i->Xin.Sh32.src = src;
   i->Xin.Sh32.dst = dst;
   return i;
}

X86Instr* X86Instr_RET ( void ) {
   X86Instr* i     = malloc(sizeof(X86Instr));
   i->tag          = Xin_RET;
   return i;
}

void ppX86Instr ( FILE* f, X86Instr* i ) {
   switch (i->tag) {
      case Xin_Alu32R:
         ppX86AluOp(f, i->Xin.Alu32R.op);
         fprintf(f, "l ");
         ppX86RMI(f, i->Xin.Alu32R.src);
         fprintf(f, ",");
         ppHRegX86(f, i->Xin.Alu32R.dst);
         return;
      case Xin_Alu32M:
         ppX86AluOp(f, i->Xin.Alu32M.op);
         fprintf(f, "l ");
         ppX86RI(f, i->Xin.Alu32M.src);
         fprintf(f, ",");
         ppX86AMode(f, i->Xin.Alu32M.dst);
         return;
      case Xin_Sh32:
         ppX86ShiftOp(f, i->Xin.Sh32.op);
         fprintf(f, "l ");
         if (i->Xin.Sh32.src == 0)
	   fprintf(f, " %%cl,"); 
         else 
            fprintf(f, " $%d,", i->Xin.Sh32.src);
         ppX86RM(f, i->Xin.Sh32.dst);
         return;
      case Xin_RET:
         fprintf(f, "ret");
         return;
      default:
         panic("ppX86Instr");
   }
}

void getRegUsage_X86Instr (HRegUsage* u, X86Instr* i)
{
   initHRegUsage(u);
   switch (i->tag) {
      case Xin_Alu32R:
         addRegUsage_X86RMI(u, i->Xin.Alu32R.src);
         if (i->Xin.Alu32R.op == Xalu_MOV)
            addHRegUse(u, HRmWrite,  i->Xin.Alu32R.dst);
         else
            addHRegUse(u, HRmModify, i->Xin.Alu32R.dst);
         return;
      case Xin_Alu32M:
         addRegUsage_X86RI(u, i->Xin.Alu32M.src);
         addRegUsage_X86AMode(u, i->Xin.Alu32M.dst);
         return;
      case Xin_Sh32:
         addRegUsage_X86RM(u, i->Xin.Sh32.dst, HRmModify);
         if (i->Xin.Sh32.src == 0)
            addHRegUse(u, HRmRead, hregX86_ECX());
         return;
      case Xin_RET:
         return;
      default:
         ppX86Instr(stderr, i);
         panic("getRegUsage_X86Instr");
   }
}

void mapRegs_X86Instr (HRegRemap* m, X86Instr* i)
{
   switch (i->tag) {
      case Xin_Alu32R:
         mapRegs_X86RMI(m, i->Xin.Alu32R.src);
         i->Xin.Alu32R.dst = lookupHRegRemap(m, i->Xin.Alu32R.dst);
         return;
      case Xin_Alu32M:
         mapRegs_X86RI(m, i->Xin.Alu32M.src);
         mapRegs_X86AMode(m, i->Xin.Alu32M.dst);
         return;
      case Xin_Sh32:
         mapRegs_X86RM(m, i->Xin.Sh32.dst);
         return;
      case Xin_RET:
         return;
      default:
         ppX86Instr(stderr, i);
         panic("mapRegs_X86Instr");
   }
}

