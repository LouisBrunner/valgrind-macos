
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (x86h_defs.h) is                              ---*/
/*--- Copyright (c) 2004 OpenWorks LLP.  All rights reserved. ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

#ifndef __X86H_DEFS_H
#define __X86H_DEFS_H


/* --------- Registers. --------- */

/* The usual HReg abstraction.  There are 8 real int regs,
   6 real float regs, and 0 real vector regs. 
*/

extern void ppHRegX86 ( FILE*, HReg );


/* --------- Memory address expressions (amodes). --------- */

typedef
   enum {
     Xam_IR,
     Xam_IRRS
   }
   X86AModeTag;

typedef
   struct {
      X86AModeTag tag;
      union {
         struct {
            UInt imm;
            HReg reg;
         } IR;
         struct {
            UInt imm;
            HReg base;
            HReg index;
            Int  shift; /* 0, 1, 2 or 3 only */
         } IRRS;
      } Xam;
   }
   X86AMode;

extern X86AMode* X86AMode_IR   ( UInt, HReg );
extern X86AMode* X86AMode_IRRS ( UInt, HReg, HReg, Int );

extern void ppX86AMode ( FILE*, X86AMode* );


/* --------- Operands.  Not all are valid for all insns. --------- */

typedef 
   enum {
      Xop_Imm,
      Xop_Reg,
      Xop_Mem
   }
   X86OperandTag;

typedef
   struct {
      X86OperandTag tag;
      union {
         struct {
            UInt imm32;
         } Imm;
         struct {
            HReg reg;
         } Reg;
         struct {
            X86AMode* am;
         } Mem;
      }
      Xop;
   }
   X86Operand;

extern X86Operand* X86Operand_Imm ( UInt );
extern X86Operand* X86Operand_Reg ( HReg );
extern X86Operand* X86Operand_Mem ( X86AMode* );

extern void ppX86Operand ( FILE*, X86Operand* );


/* --------- Instructions. --------- */

typedef 
   enum { Xalu_ADD, Xalu_SUB, Xalu_ADC, Xalu_SBB, Xalu_AND, Xalu_OR, Xalu_XOR }
   X86AluOp;

extern void ppX86AluOp ( FILE*, X86AluOp );


typedef
   enum {
      Xin_LD32,     /* 32-bit integer load */
      Xin_ST32,     /* 32-bit integer store */
      Xin_Alu32,    /* 32-bit arith/logical, R-R, R-M, M-R */
      Xin_Mov32,    /* 32-bit move R-R R-M M-R I-R I-M */
      Xin_Alu16     /* 16-bit arith/logical, R-R, R-M, M-R */
   }
   X86InstrTag;

/* Destinations are on the RIGHT (second operand). */

typedef
   struct {
      X86InstrTag tag;
      union {
	 struct {
	    HReg src;
	    X86AMode* dst;
	 } ST32;
	 struct {
	    X86AMode* src;
	    HReg dst;
	 } LD32;
         struct {
            X86AluOp    op;
            X86Operand* src;
            X86Operand* dst;
         } Alu32;
         struct {
            X86Operand* src;
            X86Operand* dst;
         } Mov32;
         struct {
            X86AluOp    op;
            X86Operand* src;
            X86Operand* dst;
         } Alu16;
      } Xin;
   }
   X86Instr;

extern X86Instr* X86Instr_ST32  ( HReg, X86AMode* );
extern X86Instr* X86Instr_LD32  ( X86AMode*, HReg );
extern X86Instr* X86Instr_Alu32 ( X86AluOp, X86Operand*, X86Operand* );
extern X86Instr* X86Instr_Mov32 ( X86Operand*, X86Operand* );
extern X86Instr* X86Instr_Alu16 ( X86AluOp, X86Operand*, X86Operand* );

extern void ppX86Instr ( FILE*, X86Instr* );


#endif /* ndef __X86H_DEFS_H */
