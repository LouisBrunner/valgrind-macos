
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


/* --------- X86Operand.  Not all are valid for all insns. --------- */

X86Operand* X86Operand_Imm ( UInt imm32 ) {
   X86Operand* op    = malloc(sizeof(X86Operand));
   op->tag           = Xop_Imm;
   op->Xop.Imm.imm32 = imm32;
   return op;
}

X86Operand* X86Operand_Reg ( HReg reg ) {
   X86Operand* op  = malloc(sizeof(X86Operand));
   op->tag         = Xop_Reg;
   op->Xop.Reg.reg = reg;
   return op;
}

X86Operand* X86Operand_Mem ( X86AMode* am ) {
   X86Operand* op = malloc(sizeof(X86Operand));
   op->tag        = Xop_Mem;
   op->Xop.Mem.am = am;
   return op;
}

void ppX86Operand ( FILE* f, X86Operand* op ) {
   switch (op->tag) {
      case Xop_Imm: 
         fprintf(f, "$0x%x", op->Xop.Imm.imm32);
         return;
      case Xop_Reg: 
         ppHRegX86(f, op->Xop.Reg.reg);
         return;
      case Xop_Mem: 
         ppX86AMode(f, op->Xop.Mem.am);
         return;
     default: 
         panic("ppX86Operand");
   }
}


/* --------- Instructions. --------- */

void ppX86AluOp ( FILE* f, X86AluOp op ) {
   Char* name;
   switch (op) {
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

X86Instr* X86Instr_ST32 ( HReg src, X86AMode* dst ) {
   X86Instr* i     = malloc(sizeof(X86Instr));
   i->tag          = Xin_ST32;
   i->Xin.ST32.src = src;
   i->Xin.ST32.dst = dst;
   return i;
}

X86Instr* X86Instr_LD32 ( X86AMode* src, HReg dst ) {
   X86Instr* i     = malloc(sizeof(X86Instr));
   i->tag          = Xin_LD32;
   i->Xin.LD32.src = src;
   i->Xin.LD32.dst = dst;
   return i;
}

X86Instr* X86Instr_Alu32 ( X86AluOp op, X86Operand* src, X86Operand* dst ) {
   X86Instr* i      = malloc(sizeof(X86Instr));
   i->tag           = Xin_Alu32;
   i->Xin.Alu32.op  = op;
   i->Xin.Alu32.src = src;
   i->Xin.Alu32.dst = dst;
   return i;
}

X86Instr* X86Instr_Mov32 ( X86Operand* src, X86Operand* dst ) {
   X86Instr* i      = malloc(sizeof(X86Instr));
   i->tag           = Xin_Mov32;
   i->Xin.Mov32.src = src;
   i->Xin.Mov32.dst = dst;
   return i;
}

X86Instr* X86Instr_Alu16 ( X86AluOp op, X86Operand* src, X86Operand* dst ) {
   X86Instr* i      = malloc(sizeof(X86Instr));
   i->tag           = Xin_Alu16;
   i->Xin.Alu16.op  = op;
   i->Xin.Alu16.src = src;
   i->Xin.Alu16.dst = dst;
   return i;
}

void ppX86Instr ( FILE* f, X86Instr* i ) {
   switch (i->tag) {
      case Xin_ST32:
         fprintf(f, "movl ");
         ppHRegX86(f, i->Xin.ST32.src);
         fprintf(f, ",");
         ppX86AMode(f, i->Xin.ST32.dst);
         return;
      case Xin_LD32:
         fprintf(f, "movl ");
         ppX86AMode(f, i->Xin.LD32.src);
         fprintf(f, ",");
         ppHRegX86(f, i->Xin.LD32.dst);
         return;
      case Xin_Alu32:
         ppX86AluOp(f, i->Xin.Alu32.op);
         fprintf(f, "l ");
         ppX86Operand(f, i->Xin.Alu32.src);
         fprintf(f, ",");
         ppX86Operand(f, i->Xin.Alu32.dst);
         return;
      case Xin_Mov32:
         fprintf(f, "movl ");
         ppX86Operand(f, i->Xin.Mov32.src);
         fprintf(f, ",");
         ppX86Operand(f, i->Xin.Mov32.dst);
         return;
      case Xin_Alu16:
         ppX86AluOp(f, i->Xin.Alu16.op);
         fprintf(f, "w ");
         ppX86Operand(f, i->Xin.Alu16.src);
         fprintf(f, ",");
         ppX86Operand(f, i->Xin.Alu16.dst);
         return;
      default:
         panic("ppX86Instr");
   }
}
