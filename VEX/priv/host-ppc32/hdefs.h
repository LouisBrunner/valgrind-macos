
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (host-ppc32/hdefs.h) is                       ---*/
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

#ifndef __LIBVEX_HOST_PPC32_HDEFS_H
#define __LIBVEX_HOST_PPC32_HDEFS_H

/* Num registers used for function calls */
#define PPC32_N_REGPARMS 8


/* --------- Registers. --------- */

/* The usual HReg abstraction.  There are 32 real int regs,
   32 real float regs, and 0 real vector regs. 
*/

extern void ppHRegPPC32 ( HReg );

extern HReg hregPPC32_GPR0  ( void );   // reserved
extern HReg hregPPC32_GPR1  ( void );   // Stack Frame Pointer
extern HReg hregPPC32_GPR2  ( void );   // TOC pointer - not used
extern HReg hregPPC32_GPR3  ( void );
extern HReg hregPPC32_GPR4  ( void );
extern HReg hregPPC32_GPR5  ( void );
extern HReg hregPPC32_GPR6  ( void );
extern HReg hregPPC32_GPR7  ( void );
extern HReg hregPPC32_GPR8  ( void );
extern HReg hregPPC32_GPR9  ( void );
extern HReg hregPPC32_GPR10 ( void );
extern HReg hregPPC32_GPR11 ( void );
extern HReg hregPPC32_GPR12 ( void );
extern HReg hregPPC32_GPR13 ( void );   // thread specific pointer - not used
extern HReg hregPPC32_GPR14 ( void );
extern HReg hregPPC32_GPR15 ( void );
extern HReg hregPPC32_GPR16 ( void );
extern HReg hregPPC32_GPR17 ( void );
extern HReg hregPPC32_GPR18 ( void );
extern HReg hregPPC32_GPR19 ( void );
extern HReg hregPPC32_GPR20 ( void );
extern HReg hregPPC32_GPR21 ( void );
extern HReg hregPPC32_GPR22 ( void );
extern HReg hregPPC32_GPR23 ( void );
extern HReg hregPPC32_GPR24 ( void );
extern HReg hregPPC32_GPR25 ( void );
extern HReg hregPPC32_GPR26 ( void );
extern HReg hregPPC32_GPR27 ( void );
extern HReg hregPPC32_GPR28 ( void );
extern HReg hregPPC32_GPR29 ( void );
extern HReg hregPPC32_GPR30 ( void );
extern HReg hregPPC32_GPR31 ( void );    // GuestStatePtr

extern HReg hregPPC32_FPR0  ( void );
extern HReg hregPPC32_FPR1  ( void );
extern HReg hregPPC32_FPR2  ( void );
extern HReg hregPPC32_FPR3  ( void );
extern HReg hregPPC32_FPR4  ( void );
extern HReg hregPPC32_FPR5  ( void );
extern HReg hregPPC32_FPR6  ( void );
extern HReg hregPPC32_FPR7  ( void );
extern HReg hregPPC32_FPR8  ( void );
extern HReg hregPPC32_FPR9  ( void );
extern HReg hregPPC32_FPR10 ( void );
extern HReg hregPPC32_FPR11 ( void );
extern HReg hregPPC32_FPR12 ( void );
extern HReg hregPPC32_FPR13 ( void );
extern HReg hregPPC32_FPR14 ( void );
extern HReg hregPPC32_FPR15 ( void );
extern HReg hregPPC32_FPR16 ( void );
extern HReg hregPPC32_FPR17 ( void );
extern HReg hregPPC32_FPR18 ( void );
extern HReg hregPPC32_FPR19 ( void );
extern HReg hregPPC32_FPR20 ( void );
extern HReg hregPPC32_FPR21 ( void );
extern HReg hregPPC32_FPR22 ( void );
extern HReg hregPPC32_FPR23 ( void );
extern HReg hregPPC32_FPR24 ( void );
extern HReg hregPPC32_FPR25 ( void );
extern HReg hregPPC32_FPR26 ( void );
extern HReg hregPPC32_FPR27 ( void );
extern HReg hregPPC32_FPR28 ( void );
extern HReg hregPPC32_FPR29 ( void );
extern HReg hregPPC32_FPR30 ( void );
extern HReg hregPPC32_FPR31 ( void );


#define StackFramePtr hregPPC32_GPR1()
#define GuestStatePtr hregPPC32_GPR31()



/* --------- Condition codes, Intel encoding. --------- */

typedef
   enum {
      Pcc_O      = 0,  /* overflow           */
      Pcc_NO     = 1,  /* no overflow        */

      Pcc_B      = 2,  /* below              */
      Pcc_NB     = 3,  /* not below          */

      Pcc_Z      = 4,  /* zero               */
      Pcc_NZ     = 5,  /* not zero           */

      Pcc_BE     = 6,  /* below or equal     */
      Pcc_NBE    = 7,  /* not below or equal */

      Pcc_S      = 8,  /* negative           */
      Pcc_NS     = 9,  /* not negative       */

      Pcc_P      = 10, /* parity even        */
      Pcc_NP     = 11, /* not parity even    */

      Pcc_L      = 12, /* jump less          */
      Pcc_NL     = 13, /* not less           */

      Pcc_LE     = 14, /* less or equal      */
      Pcc_NLE    = 15, /* not less or equal  */

      Pcc_ALWAYS = 16  /* the usual hack     */
   }
   PPC32CondCode;

extern HChar* showPPC32CondCode ( PPC32CondCode );


/* --------- Memory address expressions (amodes). --------- */

typedef
   enum {
     Pam_IR,        /* Immediate + Reg */
     Pam_RR         /* Reg1 + Reg2     */
   }
   PPC32AModeTag;

typedef
   struct {
      PPC32AModeTag tag;
      union {
         struct {
            HReg base;
            UInt index;
         } IR;
         struct {
            HReg base;
            HReg index;
         } RR;
      } Pam;
   }
   PPC32AMode;

extern PPC32AMode* PPC32AMode_IR ( UInt, HReg );
extern PPC32AMode* PPC32AMode_RR ( HReg, HReg );

extern PPC32AMode* dopyPPC32AMode ( PPC32AMode* );

extern void ppPPC32AMode ( PPC32AMode* );


/* --------- Operand, which can be reg or immediate only. --------- */

typedef 
   enum {
      Pri_Imm,
      Pri_Reg
   }
   PPC32RITag;

typedef
   struct {
      PPC32RITag tag;
      union {
         struct {
            UInt imm32;
         } Imm;
         struct {
            HReg reg;
         } Reg;
      }
      Pri;
   }
   PPC32RI;

extern PPC32RI* PPC32RI_Imm ( UInt );
extern PPC32RI* PPC32RI_Reg ( HReg );

extern void ppPPC32RI ( PPC32RI* );


/* --------- Instructions. --------- */

//.. /* --------- */
//.. typedef
//..    enum {
//..       Xss_16,
//..       Xss_32
//..    }
//..    X86ScalarSz;
//.. 
//.. extern HChar* showX86ScalarSz ( X86ScalarSz );

/* --------- */
typedef
   enum {
      Pun_NEG,
      Pun_NOT
   }
   PPC32UnaryOp;

extern HChar* showPPC32UnaryOp ( PPC32UnaryOp );


/* --------- */
typedef 
   enum {
      Palu_INVALID,
      Palu_CMP,
      Palu_ADD, Palu_SUB, Palu_ADC, Palu_SBB,
      Palu_AND, Palu_OR, Palu_XOR,
      Palu_MUL
   }
   PPC32AluOp;

extern HChar* showPPC32AluOp ( PPC32AluOp );


/* --------- */
typedef
   enum {
      Psh_INVALID,
      Psh_SHL, Psh_SHR, Psh_SAR, 
      Psh_ROL
   }
   PPC32ShiftOp;

extern HChar* showPPC32ShiftOp ( PPC32ShiftOp );


//.. /* --------- */
//.. typedef
//..    enum {
//..       Xfp_INVALID,
//..       /* Binary */
//..       Xfp_ADD, Xfp_SUB, Xfp_MUL, Xfp_DIV, 
//..       Xfp_SCALE, Xfp_ATAN, Xfp_YL2X, Xfp_YL2XP1, Xfp_PREM, Xfp_PREM1,
//..       /* Unary */
//..       Xfp_SQRT, Xfp_ABS, Xfp_NEG, Xfp_MOV, Xfp_SIN, Xfp_COS, Xfp_TAN,
//..       Xfp_ROUND, Xfp_2XM1
//..    }
//..    X86FpOp;
//.. 
//.. extern HChar* showX86FpOp ( X86FpOp );


/* --------- */
typedef
   enum {
      Pin_Alu32,     /* 32-bit mov/arith/logical */
      Pin_Sh32,      /* 32-bit shift/rotate */
      Pin_Test32,    /* 32-bit test (AND, set flags, discard result) */
      Pin_Unary32,   /* 32-bit not and neg */
//..       Xin_MulL,      /* widening multiply */
//..       Xin_Div,       /* div and mod */
//..       Xin_Sh3232,    /* shldl or shrdl */
//..       Xin_Push,      /* push (32-bit?) value on stack */
      Pin_Call,      /* call to address in register */
      Pin_Goto,      /* conditional/unconditional jmp to dst */
//..       Xin_CMov32,    /* conditional move */
      Pin_LoadEX,    /* load a 8|16|32 bit value from mem */
      Pin_Store,     /* store a 8|16|32 bit value to mem */
//..       Xin_Set32,     /* convert condition code to 32-bit value */
//..       Xin_Bsfr32,    /* 32-bit bsf/bsr */
//..       Xin_MFence,    /* mem fence (not just sse2, but sse0 and 1 too) */

//..       Xin_FpUnary,   /* FP fake unary op */
//..       Xin_FpBinary,  /* FP fake binary op */
//..       Xin_FpLdSt,    /* FP fake load/store */
//..       Xin_FpLdStI,   /* FP fake load/store, converting to/from Int */
//..       Xin_Fp64to32,  /* FP round IEEE754 double to IEEE754 single */
//..       Xin_FpCMov,    /* FP fake floating point conditional move */
//..       Xin_FpLdStCW,  /* fldcw / fstcw */
//..       Xin_FpStSW_AX, /* fstsw %ax */
//..       Xin_FpCmp,     /* FP compare, generating a C320 value into int reg */
      Pin_Nada // Nix
   }
   PPC32InstrTag;

/* Destinations are on the LEFT (first operand) */

typedef
   struct {
      PPC32InstrTag tag;
      union {
         struct {
            PPC32AluOp op;
            HReg       dst;
            HReg       src1;
            PPC32RI*   src2;
         } Alu32;
         struct {
            PPC32ShiftOp op;
            HReg         dst;
            HReg         src;
            PPC32RI*     shft;
         } Sh32;
         struct {
            HReg     dst;
            PPC32RI* src;
         } Test32;
         /* Not and Neg */
         struct {
            PPC32UnaryOp op;
            PPC32RI*     dst;
         } Unary32;
//..          /* DX:AX = AX *s/u r/m16,  or EDX:EAX = EAX *s/u r/m32 */
//..          struct {
//..             Bool        syned;
//..             X86ScalarSz ssz;
//..             X86RM*      src;
//..          } MulL;
//..          /* x86 div/idiv instruction.  Modifies EDX and EAX and reads src. */
//..          struct {
//..             Bool        syned;
//..             X86ScalarSz ssz;
//..             X86RM*      src;
//..          } Div;
//..          /* shld/shrd.  op may only be Xsh_SHL or Xsh_SHR */
//..          struct {
//..             X86ShiftOp op;
//..             UInt       amt;   /* shift amount, or 0 means %cl */
//..             HReg       src;
//..             HReg       dst;
//..          } Sh3232;
//..          struct {
//..             X86RMI* src;
//..          } Push;
         /* Pseudo-insn.  Call target (an absolute address), on given
            condition (which could be Pcc_ALWAYS). */
         struct {
            PPC32CondCode cond;
            Addr32        target;
            Int           regparms; /* 0 .. 9 */
         } Call;
         /* Pseudo-insn.  Goto dst, on given condition (which could be
            Pcc_ALWAYS).  Note importantly that if the jump is 
            conditional (not Pcc_ALWAYS) the jump kind *must* be
            Ijk_Boring.  Ie non-Boring conditional jumps are
            not allowed. */
         struct {
            IRJumpKind    jk;
            PPC32CondCode cond;
            PPC32RI*      dst;
         } Goto;
//..          /* Mov src to dst on the given condition, which may not
//..             be the bogus Xcc_ALWAYS. */
//..          struct {
//..             X86CondCode cond;
//..             X86RM*      src;
//..             HReg        dst;
//..          } CMov32;
         /* Sign/Zero extending loads.  Dst size is always 32 bits. */
         struct {
            UChar       sz; /* 1|2|4 */
            Bool        syned;
            HReg        dst;
            PPC32AMode* src;
         } LoadEX;
         /* 16/8 bit stores, which are troublesome (particularly 8-bit) */
         struct {
            UChar       sz; /* 1|2|4 */
            PPC32AMode* dst;
            HReg        src;
         } Store;
//..          /* Convert a x86 condition code to a 32-bit value (0 or 1). */
//..          struct {
//..             X86CondCode cond;
//..             HReg        dst;
//..          } Set32;
//..          /* 32-bit bsf or bsr. */
//..          struct {
//..             Bool isFwds;
//..             HReg src;
//..             HReg dst;
//..          } Bsfr32;
//..          /* Mem fence (not just sse2, but sse0 and 1 too).  In short,
//..             an insn which flushes all preceding loads and stores as
//..             much as possible before continuing.  On SSE2 we emit a
//..             real "mfence", on SSE1 "sfence ; lock addl $0,0(%esp)" and
//..             on SSE0 "lock addl $0,0(%esp)".  This insn therefore
//..             carries the subarch so the assembler knows what to
//..             emit. */
//..          struct {
//..             VexSubArch subarch;
//..          } MFence;

//..          /* X86 Floating point (fake 3-operand, "flat reg file" insns) */
//..          struct {
//..             X86FpOp op;
//..             HReg    src;
//..             HReg    dst;
//..          } FpUnary;
//..          struct {
//..             X86FpOp op;
//..             HReg    srcL;
//..             HReg    srcR;
//..             HReg    dst;
//..          } FpBinary;
//..          struct {
//..             Bool      isLoad;
//..             UChar     sz; /* only 4 (IEEE single) or 8 (IEEE double) */
//..             HReg      reg;
//..             X86AMode* addr;
//..          } FpLdSt;
//..          /* Move 64-bit float to/from memory, converting to/from
//..             signed int on the way.  Note the conversions will observe
//..             the host FPU rounding mode currently in force. */
//..          struct {
//..             Bool      isLoad;
//..             UChar     sz; /* only 2, 4 or 8 */
//..             HReg      reg;
//..             X86AMode* addr;
//..          } FpLdStI;
//..          /* By observing the current FPU rounding mode, round (etc)
//..             src into dst given that dst should be interpreted as an
//..             IEEE754 32-bit (float) type. */
//..          struct {
//..             HReg src;
//..             HReg dst;
//..          } Fp64to32;
//..          /* Mov src to dst on the given condition, which may not
//..             be the bogus Xcc_ALWAYS. */
//..          struct {
//..             X86CondCode cond;
//..             HReg        src;
//..             HReg        dst;
//..          } FpCMov;
//..          /* Load/store the FPU's 16-bit control word (fldcw/fstcw) */
//..          struct {
//..             Bool      isLoad;
//..             X86AMode* addr;
//..          }
//..          FpLdStCW;
//..          /* fstsw %ax */
//..          struct {
//..             /* no fields */
//..          }
//..          FpStSW_AX;
//..          /* Do a compare, generating the C320 bits into the dst. */
//..          struct {
//..             HReg    srcL;
//..             HReg    srcR;
//..             HReg    dst;
//..          } FpCmp;

       } Pin;
   }
   PPC32Instr;


extern PPC32Instr* PPC32Instr_Alu32     ( PPC32AluOp, HReg, HReg, PPC32RI* );
extern PPC32Instr* PPC32Instr_Sh32      ( PPC32ShiftOp, HReg, HReg, PPC32RI* );
extern PPC32Instr* PPC32Instr_Test32    ( HReg dst, PPC32RI* src );
extern PPC32Instr* PPC32Instr_Unary32   ( PPC32UnaryOp op, PPC32RI* dst );
//.. extern X86Instr* X86Instr_MulL      ( Bool syned, X86ScalarSz, X86RM* );
//.. extern X86Instr* X86Instr_Div       ( Bool syned, X86ScalarSz, X86RM* );
//.. extern X86Instr* X86Instr_Sh3232    ( X86ShiftOp, UInt amt, HReg src, HReg dst );
//.. extern X86Instr* X86Instr_Push      ( X86RMI* );
extern PPC32Instr* PPC32Instr_Call      ( PPC32CondCode, Addr32, Int );
extern PPC32Instr* PPC32Instr_Goto      ( IRJumpKind, PPC32CondCode cond, PPC32RI* dst );
//.. extern X86Instr* X86Instr_CMov32    ( X86CondCode, X86RM* src, HReg dst );
extern PPC32Instr* PPC32Instr_LoadEX    ( UChar sz, Bool syned,
					  HReg dst, PPC32AMode* src );
extern PPC32Instr* PPC32Instr_Store     ( UChar sz, PPC32AMode* dst, HReg src );
//.. extern X86Instr* X86Instr_Set32     ( X86CondCode cond, HReg dst );
//.. extern X86Instr* X86Instr_Bsfr32    ( Bool isFwds, HReg src, HReg dst );
//.. extern X86Instr* X86Instr_MFence    ( VexSubArch );
//.. 
//.. extern X86Instr* X86Instr_FpUnary   ( X86FpOp op, HReg src, HReg dst );
//.. extern X86Instr* X86Instr_FpBinary  ( X86FpOp op, HReg srcL, HReg srcR, HReg dst );
//.. extern X86Instr* X86Instr_FpLdSt    ( Bool isLoad, UChar sz, HReg reg, X86AMode* );
//.. extern X86Instr* X86Instr_FpLdStI   ( Bool isLoad, UChar sz, HReg reg, X86AMode* );
//.. extern X86Instr* X86Instr_Fp64to32  ( HReg src, HReg dst );
//.. extern X86Instr* X86Instr_FpCMov    ( X86CondCode, HReg src, HReg dst );
//.. extern X86Instr* X86Instr_FpLdStCW  ( Bool isLoad, X86AMode* );
//.. extern X86Instr* X86Instr_FpStSW_AX ( void );
//.. extern X86Instr* X86Instr_FpCmp     ( HReg srcL, HReg srcR, HReg dst );


extern void ppPPC32Instr ( PPC32Instr* );

/* Some functions that insulate the register allocator from details
   of the underlying instruction set. */
extern void         getRegUsage_PPC32Instr ( HRegUsage*, PPC32Instr* );
extern void         mapRegs_PPC32Instr     ( HRegRemap*, PPC32Instr* );
extern Bool         isMove_PPC32Instr      ( PPC32Instr*, HReg*, HReg* );
extern Int          emit_PPC32Instr        ( UChar* buf, Int nbuf, PPC32Instr* );
extern PPC32Instr*  genSpill_PPC32         ( HReg rreg, Int offset );
extern PPC32Instr*  genReload_PPC32        ( HReg rreg, Int offset );
extern void         getAllocableRegs_PPC32 ( Int*, HReg** );
extern HInstrArray* iselBB_PPC32           ( IRBB*, VexSubArch );

#endif /* ndef __LIBVEX_HOST_PPC32_HDEFS_H */

/*---------------------------------------------------------------*/
/*--- end                                  host-ppc32/hdefs.h ---*/
/*---------------------------------------------------------------*/
