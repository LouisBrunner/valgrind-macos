
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

extern HReg hregX86_EAX ( void );
extern HReg hregX86_EBX ( void );
extern HReg hregX86_ECX ( void );
extern HReg hregX86_EDX ( void );
extern HReg hregX86_EBP ( void );


/* --------- Memory address expressions (amodes). --------- */

typedef
   enum {
     Xam_IR,        /* Immediate + Reg */
     Xam_IRRS       /* Immediate + Reg1 + (Reg2 << Shift) */
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


/* --------- Operand, which can be reg, immediate or memory. --------- */

typedef 
   enum {
      Xrmi_Imm,
      Xrmi_Reg,
      Xrmi_Mem
   }
   X86RMITag;

typedef
   struct {
      X86RMITag tag;
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
      Xrmi;
   }
   X86RMI;

extern X86RMI* X86RMI_Imm ( UInt );
extern X86RMI* X86RMI_Reg ( HReg );
extern X86RMI* X86RMI_Mem ( X86AMode* );

extern void ppX86RMI ( FILE*, X86RMI* );


/* --------- Operand, which can be reg or immediate only. --------- */

typedef 
   enum {
      Xri_Imm,
      Xri_Reg
   }
   X86RITag;

typedef
   struct {
      X86RITag tag;
      union {
         struct {
            UInt imm32;
         } Imm;
         struct {
            HReg reg;
         } Reg;
      }
      Xri;
   }
   X86RI;

extern X86RI* X86RI_Imm ( UInt );
extern X86RI* X86RI_Reg ( HReg );

extern void ppX86RI ( FILE*, X86RI* );


/* --------- Operand, which can be reg or memory only. --------- */

typedef 
   enum {
      Xrm_Reg,
      Xrm_Mem
   }
   X86RMTag;

typedef
   struct {
      X86RMTag tag;
      union {
         struct {
            HReg reg;
         } Reg;
         struct {
            X86AMode* am;
         } Mem;
      }
      Xrm;
   }
   X86RM;

extern X86RM* X86RM_Reg ( HReg );
extern X86RM* X86RM_Mem ( X86AMode* );

extern void ppX86RM ( FILE*, X86RM* );


/* --------- Instructions. --------- */

/* --------- */
typedef 
   enum { 
      Xalu_MOV,
      Xalu_ADD, Xalu_SUB, Xalu_ADC, Xalu_SBB, 
      Xalu_AND, Xalu_OR, Xalu_XOR 
   }
   X86AluOp;

extern void ppX86AluOp ( FILE*, X86AluOp );


/* --------- */
typedef
   enum {
      Xsh_SHL, Xsh_SHR, Xsh_SAR, 
      Xsh_ROL, Xsh_ROR
   }
   X86ShiftOp;

extern void ppX86ShiftOp ( FILE*, X86ShiftOp );


/* --------- */
typedef
   enum {
      Xin_Alu32R,    /* 32-bit mov/arith/logical, dst=REG */
      Xin_Alu32M,    /* 32-bit mov/arith/logical, dst=MEM */
      Xin_Sh32,      /* 32-bit shift/rotate, dst=REG or MEM */
      Xin_RET
   }
   X86InstrTag;

/* Destinations are on the RIGHT (second operand). */

typedef
   struct {
      X86InstrTag tag;
      union {
         struct {
            X86AluOp op;
            X86RMI*  src;
            HReg     dst;
         } Alu32R;
         struct {
            X86AluOp  op;
            X86RI*    src;
            X86AMode* dst;
         } Alu32M;
         struct {
            X86ShiftOp op;
            UInt       src;  /* shift amount, or 0 means %cl */
            X86RM*     dst;
         } Sh32;
 	 struct {
	 } RET;
      } Xin;
   }
   X86Instr;

extern X86Instr* X86Instr_Alu32R ( X86AluOp, X86RMI*, HReg );
extern X86Instr* X86Instr_Alu32M ( X86AluOp, X86RI*,  X86AMode* );
extern X86Instr* X86Instr_Sh32   ( X86ShiftOp, UInt, X86RM* );
extern X86Instr* X86Instr_RET    ( void );

extern void ppX86Instr ( FILE*, X86Instr* );

extern void getRegUsage_X86Instr ( HRegUsage*, X86Instr* );

extern void mapRegs_X86Instr ( HRegRemap*, X86Instr* );

extern Bool isMove_X86Instr ( X86Instr*, HReg*, HReg* );


#endif /* ndef __X86H_DEFS_H */
