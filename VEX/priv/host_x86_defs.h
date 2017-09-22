
/*---------------------------------------------------------------*/
/*--- begin                                   host_x86_defs.h ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2004-2017 OpenWorks LLP
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

#ifndef __VEX_HOST_X86_DEFS_H
#define __VEX_HOST_X86_DEFS_H

#include "libvex_basictypes.h"
#include "libvex.h"                      // VexArch
#include "host_generic_regs.h"           // HReg

/* --------- Registers. --------- */

/* The usual HReg abstraction.  There are 8 real int regs,
   6 real float regs, and 8 real vector regs. 
*/

#define ST_IN static inline
ST_IN HReg hregX86_EBX   ( void ) { return mkHReg(False, HRcInt32,  3,  0); }
ST_IN HReg hregX86_ESI   ( void ) { return mkHReg(False, HRcInt32,  6,  1); }
ST_IN HReg hregX86_EDI   ( void ) { return mkHReg(False, HRcInt32,  7,  2); }
ST_IN HReg hregX86_EAX   ( void ) { return mkHReg(False, HRcInt32,  0,  3); }
ST_IN HReg hregX86_ECX   ( void ) { return mkHReg(False, HRcInt32,  1,  4); }
ST_IN HReg hregX86_EDX   ( void ) { return mkHReg(False, HRcInt32,  2,  5); }

ST_IN HReg hregX86_FAKE0 ( void ) { return mkHReg(False, HRcFlt64,  0,  6); }
ST_IN HReg hregX86_FAKE1 ( void ) { return mkHReg(False, HRcFlt64,  1,  7); }
ST_IN HReg hregX86_FAKE2 ( void ) { return mkHReg(False, HRcFlt64,  2,  8); }
ST_IN HReg hregX86_FAKE3 ( void ) { return mkHReg(False, HRcFlt64,  3,  9); }
ST_IN HReg hregX86_FAKE4 ( void ) { return mkHReg(False, HRcFlt64,  4, 10); }
ST_IN HReg hregX86_FAKE5 ( void ) { return mkHReg(False, HRcFlt64,  5, 11); }

ST_IN HReg hregX86_XMM0  ( void ) { return mkHReg(False, HRcVec128, 0, 12); }
ST_IN HReg hregX86_XMM1  ( void ) { return mkHReg(False, HRcVec128, 1, 13); }
ST_IN HReg hregX86_XMM2  ( void ) { return mkHReg(False, HRcVec128, 2, 14); }
ST_IN HReg hregX86_XMM3  ( void ) { return mkHReg(False, HRcVec128, 3, 15); }
ST_IN HReg hregX86_XMM4  ( void ) { return mkHReg(False, HRcVec128, 4, 16); }
ST_IN HReg hregX86_XMM5  ( void ) { return mkHReg(False, HRcVec128, 5, 17); }
ST_IN HReg hregX86_XMM6  ( void ) { return mkHReg(False, HRcVec128, 6, 18); }
ST_IN HReg hregX86_XMM7  ( void ) { return mkHReg(False, HRcVec128, 7, 19); }

ST_IN HReg hregX86_ESP   ( void ) { return mkHReg(False, HRcInt32,  4, 20); }
ST_IN HReg hregX86_EBP   ( void ) { return mkHReg(False, HRcInt32,  5, 21); }
#undef ST_IN

extern UInt ppHRegX86 ( HReg );


/* --------- Condition codes, Intel encoding. --------- */

typedef
   enum {
      Xcc_O      = 0,  /* overflow           */
      Xcc_NO     = 1,  /* no overflow        */

      Xcc_B      = 2,  /* below              */
      Xcc_NB     = 3,  /* not below          */

      Xcc_Z      = 4,  /* zero               */
      Xcc_NZ     = 5,  /* not zero           */

      Xcc_BE     = 6,  /* below or equal     */
      Xcc_NBE    = 7,  /* not below or equal */

      Xcc_S      = 8,  /* negative           */
      Xcc_NS     = 9,  /* not negative       */

      Xcc_P      = 10, /* parity even        */
      Xcc_NP     = 11, /* not parity even    */

      Xcc_L      = 12, /* jump less          */
      Xcc_NL     = 13, /* not less           */

      Xcc_LE     = 14, /* less or equal      */
      Xcc_NLE    = 15, /* not less or equal  */

      Xcc_ALWAYS = 16  /* the usual hack     */
   }
   X86CondCode;

extern const HChar* showX86CondCode ( X86CondCode );


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

extern X86AMode* dopyX86AMode ( X86AMode* );

extern void ppX86AMode ( X86AMode* );


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

extern void ppX86RMI ( X86RMI* );


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

extern void ppX86RI ( X86RI* );


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

extern void ppX86RM ( X86RM* );


/* --------- Instructions. --------- */

/* --------- */
typedef
   enum {
      Xun_NEG,
      Xun_NOT
   }
   X86UnaryOp;

extern const HChar* showX86UnaryOp ( X86UnaryOp );


/* --------- */
typedef 
   enum {
      Xalu_INVALID,
      Xalu_MOV,
      Xalu_CMP,
      Xalu_ADD, Xalu_SUB, Xalu_ADC, Xalu_SBB, 
      Xalu_AND, Xalu_OR, Xalu_XOR,
      Xalu_MUL
   }
   X86AluOp;

extern const HChar* showX86AluOp ( X86AluOp );


/* --------- */
typedef
   enum {
      Xsh_INVALID,
      Xsh_SHL, Xsh_SHR, Xsh_SAR
   }
   X86ShiftOp;

extern const HChar* showX86ShiftOp ( X86ShiftOp );


/* --------- */
typedef
   enum {
      Xfp_INVALID,
      /* Binary */
      Xfp_ADD, Xfp_SUB, Xfp_MUL, Xfp_DIV, 
      Xfp_SCALE, Xfp_ATAN, Xfp_YL2X, Xfp_YL2XP1, Xfp_PREM, Xfp_PREM1,
      /* Unary */
      Xfp_SQRT, Xfp_ABS, Xfp_NEG, Xfp_MOV, Xfp_SIN, Xfp_COS, Xfp_TAN,
      Xfp_ROUND, Xfp_2XM1
   }
   X86FpOp;

extern const HChar* showX86FpOp ( X86FpOp );


/* --------- */
typedef
   enum {
      Xsse_INVALID,
      /* mov */
      Xsse_MOV,
      /* Floating point binary */
      Xsse_ADDF, Xsse_SUBF, Xsse_MULF, Xsse_DIVF,
      Xsse_MAXF, Xsse_MINF,
      Xsse_CMPEQF, Xsse_CMPLTF, Xsse_CMPLEF, Xsse_CMPUNF,
      /* Floating point unary */
      Xsse_RCPF, Xsse_RSQRTF, Xsse_SQRTF, 
      /* Bitwise */
      Xsse_AND, Xsse_OR, Xsse_XOR, Xsse_ANDN,
      /* Integer binary */
      Xsse_ADD8,   Xsse_ADD16,   Xsse_ADD32,   Xsse_ADD64,
      Xsse_QADD8U, Xsse_QADD16U,
      Xsse_QADD8S, Xsse_QADD16S,
      Xsse_SUB8,   Xsse_SUB16,   Xsse_SUB32,   Xsse_SUB64,
      Xsse_QSUB8U, Xsse_QSUB16U,
      Xsse_QSUB8S, Xsse_QSUB16S,
      Xsse_MUL16,
      Xsse_MULHI16U,
      Xsse_MULHI16S,
      Xsse_AVG8U, Xsse_AVG16U,
      Xsse_MAX16S,
      Xsse_MAX8U,
      Xsse_MIN16S,
      Xsse_MIN8U,
      Xsse_CMPEQ8,  Xsse_CMPEQ16,  Xsse_CMPEQ32,
      Xsse_CMPGT8S, Xsse_CMPGT16S, Xsse_CMPGT32S,
      Xsse_SHL16, Xsse_SHL32, Xsse_SHL64,
      Xsse_SHR16, Xsse_SHR32, Xsse_SHR64,
      Xsse_SAR16, Xsse_SAR32, 
      Xsse_PACKSSD, Xsse_PACKSSW, Xsse_PACKUSW,
      Xsse_UNPCKHB, Xsse_UNPCKHW, Xsse_UNPCKHD, Xsse_UNPCKHQ,
      Xsse_UNPCKLB, Xsse_UNPCKLW, Xsse_UNPCKLD, Xsse_UNPCKLQ
   }
   X86SseOp;

extern const HChar* showX86SseOp ( X86SseOp );


/* --------- */
typedef
   enum {
      Xin_Alu32R,    /* 32-bit mov/arith/logical, dst=REG */
      Xin_Alu32M,    /* 32-bit mov/arith/logical, dst=MEM */
      Xin_Sh32,      /* 32-bit shift/rotate, dst=REG */
      Xin_Test32,    /* 32-bit test of REG or MEM against imm32 (AND, set
                        flags, discard result) */
      Xin_Unary32,   /* 32-bit not and neg */
      Xin_Lea32,     /* 32-bit compute EA into a reg */
      Xin_MulL,      /* 32 x 32 -> 64 multiply */
      Xin_Div,       /* 64/32 -> (32,32) div and mod */
      Xin_Sh3232,    /* shldl or shrdl */
      Xin_Push,      /* push (32-bit?) value on stack */
      Xin_Call,      /* call to address in register */
      Xin_XDirect,   /* direct transfer to GA */
      Xin_XIndir,    /* indirect transfer to GA */
      Xin_XAssisted, /* assisted transfer to GA */
      Xin_CMov32,    /* conditional move */
      Xin_LoadEX,    /* mov{s,z}{b,w}l from mem to reg */
      Xin_Store,     /* store 16/8 bit value in memory */
      Xin_Set32,     /* convert condition code to 32-bit value */
      Xin_Bsfr32,    /* 32-bit bsf/bsr */
      Xin_MFence,    /* mem fence (not just sse2, but sse0 and 1/mmxext too) */
      Xin_ACAS,      /* 8/16/32-bit lock;cmpxchg */
      Xin_DACAS,     /* lock;cmpxchg8b (doubleword ACAS, 2 x 32-bit only) */

      Xin_FpUnary,   /* FP fake unary op */
      Xin_FpBinary,  /* FP fake binary op */
      Xin_FpLdSt,    /* FP fake load/store */
      Xin_FpLdStI,   /* FP fake load/store, converting to/from Int */
      Xin_Fp64to32,  /* FP round IEEE754 double to IEEE754 single */
      Xin_FpCMov,    /* FP fake floating point conditional move */
      Xin_FpLdCW,    /* fldcw */
      Xin_FpStSW_AX, /* fstsw %ax */
      Xin_FpCmp,     /* FP compare, generating a C320 value into int reg */

      Xin_SseConst,  /* Generate restricted SSE literal */
      Xin_SseLdSt,   /* SSE load/store, no alignment constraints */
      Xin_SseLdzLO,  /* SSE load low 32/64 bits, zero remainder of reg */
      Xin_Sse32Fx4,  /* SSE binary, 32Fx4 */
      Xin_Sse32FLo,  /* SSE binary, 32F in lowest lane only */
      Xin_Sse64Fx2,  /* SSE binary, 64Fx2 */
      Xin_Sse64FLo,  /* SSE binary, 64F in lowest lane only */
      Xin_SseReRg,   /* SSE binary general reg-reg, Re, Rg */
      Xin_SseCMov,   /* SSE conditional move */
      Xin_SseShuf,   /* SSE2 shuffle (pshufd) */
      Xin_EvCheck,   /* Event check */
      Xin_ProfInc    /* 64-bit profile counter increment */
   }
   X86InstrTag;

/* Destinations are on the RIGHT (second operand) */

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
            UInt  src;  /* shift amount, or 0 means %cl */
            HReg  dst;
         } Sh32;
         struct {
            UInt   imm32;
            X86RM* dst; /* not written, only read */
         } Test32;
         /* Not and Neg */
         struct {
            X86UnaryOp op;
            HReg       dst;
         } Unary32;
         /* 32-bit compute EA into a reg */
         struct {
            X86AMode* am;
            HReg      dst;
         } Lea32;
         /* EDX:EAX = EAX *s/u r/m32 */
         struct {
            Bool   syned;
            X86RM* src;
         } MulL;
         /* x86 div/idiv instruction.  Modifies EDX and EAX and reads src. */
         struct {
            Bool   syned;
            X86RM* src;
         } Div;
         /* shld/shrd.  op may only be Xsh_SHL or Xsh_SHR */
         struct {
            X86ShiftOp op;
            UInt       amt;   /* shift amount, or 0 means %cl */
            HReg       src;
            HReg       dst;
         } Sh3232;
         struct {
            X86RMI* src;
         } Push;
         /* Pseudo-insn.  Call target (an absolute address), on given
            condition (which could be Xcc_ALWAYS). */
         struct {
            X86CondCode cond;
            Addr32      target;
            Int         regparms; /* 0 .. 3 */
            RetLoc      rloc;     /* where the return value will be */
         } Call;
         /* Update the guest EIP value, then exit requesting to chain
            to it.  May be conditional.  Urr, use of Addr32 implicitly
            assumes that wordsize(guest) == wordsize(host). */
         struct {
            Addr32      dstGA;    /* next guest address */
            X86AMode*   amEIP;    /* amode in guest state for EIP */
            X86CondCode cond;     /* can be Xcc_ALWAYS */
            Bool        toFastEP; /* chain to the slow or fast point? */
         } XDirect;
         /* Boring transfer to a guest address not known at JIT time.
            Not chainable.  May be conditional. */
         struct {
            HReg        dstGA;
            X86AMode*   amEIP;
            X86CondCode cond; /* can be Xcc_ALWAYS */
         } XIndir;
         /* Assisted transfer to a guest address, most general case.
            Not chainable.  May be conditional. */
         struct {
            HReg        dstGA;
            X86AMode*   amEIP;
            X86CondCode cond; /* can be Xcc_ALWAYS */
            IRJumpKind  jk;
         } XAssisted;
         /* Mov src to dst on the given condition, which may not
            be the bogus Xcc_ALWAYS. */
         struct {
            X86CondCode cond;
            X86RM*      src;
            HReg        dst;
         } CMov32;
         /* Sign/Zero extending loads.  Dst size is always 32 bits. */
         struct {
            UChar     szSmall;
            Bool      syned;
            X86AMode* src;
            HReg      dst;
         } LoadEX;
         /* 16/8 bit stores, which are troublesome (particularly
            8-bit) */
         struct {
            UChar     sz; /* only 1 or 2 */
            HReg      src;
            X86AMode* dst;
         } Store;
         /* Convert a x86 condition code to a 32-bit value (0 or 1). */
         struct {
            X86CondCode cond;
            HReg        dst;
         } Set32;
         /* 32-bit bsf or bsr. */
         struct {
            Bool isFwds;
            HReg src;
            HReg dst;
         } Bsfr32;
         /* Mem fence (not just sse2, but sse0 and sse1/mmxext too).
            In short, an insn which flushes all preceding loads and
            stores as much as possible before continuing.  On SSE2
            we emit a real "mfence", on SSE1 or the MMXEXT subset
            "sfence ; lock addl $0,0(%esp)" and on SSE0
            "lock addl $0,0(%esp)".  This insn therefore carries the
            host's hwcaps so the assembler knows what to emit. */
         struct {
            UInt hwcaps;
         } MFence;
         /* "lock;cmpxchg": mem address in .addr,
             expected value in %eax, new value in %ebx */
         struct {
            X86AMode* addr;
            UChar     sz; /* 1, 2 or 4 */
         } ACAS;
         /* "lock;cmpxchg8b": mem address in .addr, expected value in
            %edx:%eax, new value in %ecx:%ebx */
         struct {
            X86AMode* addr;
         } DACAS;

         /* X86 Floating point (fake 3-operand, "flat reg file" insns) */
         struct {
            X86FpOp op;
            HReg    src;
            HReg    dst;
         } FpUnary;
         struct {
            X86FpOp op;
            HReg    srcL;
            HReg    srcR;
            HReg    dst;
         } FpBinary;
         struct {
            Bool      isLoad;
            UChar     sz; /* only 4 (IEEE single) or 8 (IEEE double) */
            HReg      reg;
            X86AMode* addr;
         } FpLdSt;
         /* Move 64-bit float to/from memory, converting to/from
            signed int on the way.  Note the conversions will observe
            the host FPU rounding mode currently in force. */
         struct {
            Bool      isLoad;
            UChar     sz; /* only 2, 4 or 8 */
            HReg      reg;
            X86AMode* addr;
         } FpLdStI;
         /* By observing the current FPU rounding mode, round (etc)
            src into dst given that dst should be interpreted as an
            IEEE754 32-bit (float) type. */
         struct {
            HReg src;
            HReg dst;
         } Fp64to32;
         /* Mov src to dst on the given condition, which may not
            be the bogus Xcc_ALWAYS. */
         struct {
            X86CondCode cond;
            HReg        src;
            HReg        dst;
         } FpCMov;
         /* Load the FPU's 16-bit control word (fldcw) */
         struct {
            X86AMode* addr;
         }
         FpLdCW;
         /* fstsw %ax */
         struct {
            /* no fields */
         }
         FpStSW_AX;
         /* Do a compare, generating the C320 bits into the dst. */
         struct {
            HReg    srcL;
            HReg    srcR;
            HReg    dst;
         } FpCmp;

         /* Simplistic SSE[123] */
         struct {
            UShort  con;
            HReg    dst;
         } SseConst;
         struct {
            Bool      isLoad;
            HReg      reg;
            X86AMode* addr;
         } SseLdSt;
         struct {
            UChar     sz; /* 4 or 8 only */
            HReg      reg;
            X86AMode* addr;
         } SseLdzLO;
         struct {
            X86SseOp op;
            HReg     src;
            HReg     dst;
         } Sse32Fx4;
         struct {
            X86SseOp op;
            HReg     src;
            HReg     dst;
         } Sse32FLo;
         struct {
            X86SseOp op;
            HReg     src;
            HReg     dst;
         } Sse64Fx2;
         struct {
            X86SseOp op;
            HReg     src;
            HReg     dst;
         } Sse64FLo;
         struct {
            X86SseOp op;
            HReg     src;
            HReg     dst;
         } SseReRg;
         /* Mov src to dst on the given condition, which may not
            be the bogus Xcc_ALWAYS. */
         struct {
            X86CondCode cond;
            HReg        src;
            HReg        dst;
         } SseCMov;
         struct {
            Int    order; /* 0 <= order <= 0xFF */
            HReg   src;
            HReg   dst;
         } SseShuf;
         struct {
            X86AMode* amCounter;
            X86AMode* amFailAddr;
         } EvCheck;
         struct {
            /* No fields.  The address of the counter to inc is
               installed later, post-translation, by patching it in,
               as it is not known at translation time. */
         } ProfInc;

      } Xin;
   }
   X86Instr;

extern X86Instr* X86Instr_Alu32R    ( X86AluOp, X86RMI*, HReg );
extern X86Instr* X86Instr_Alu32M    ( X86AluOp, X86RI*,  X86AMode* );
extern X86Instr* X86Instr_Unary32   ( X86UnaryOp op, HReg dst );
extern X86Instr* X86Instr_Lea32     ( X86AMode* am, HReg dst );

extern X86Instr* X86Instr_Sh32      ( X86ShiftOp, UInt, HReg );
extern X86Instr* X86Instr_Test32    ( UInt imm32, X86RM* dst );
extern X86Instr* X86Instr_MulL      ( Bool syned, X86RM* );
extern X86Instr* X86Instr_Div       ( Bool syned, X86RM* );
extern X86Instr* X86Instr_Sh3232    ( X86ShiftOp, UInt amt, HReg src, HReg dst );
extern X86Instr* X86Instr_Push      ( X86RMI* );
extern X86Instr* X86Instr_Call      ( X86CondCode, Addr32, Int, RetLoc );
extern X86Instr* X86Instr_XDirect   ( Addr32 dstGA, X86AMode* amEIP,
                                      X86CondCode cond, Bool toFastEP );
extern X86Instr* X86Instr_XIndir    ( HReg dstGA, X86AMode* amEIP,
                                      X86CondCode cond );
extern X86Instr* X86Instr_XAssisted ( HReg dstGA, X86AMode* amEIP,
                                      X86CondCode cond, IRJumpKind jk );
extern X86Instr* X86Instr_CMov32    ( X86CondCode, X86RM* src, HReg dst );
extern X86Instr* X86Instr_LoadEX    ( UChar szSmall, Bool syned,
                                      X86AMode* src, HReg dst );
extern X86Instr* X86Instr_Store     ( UChar sz, HReg src, X86AMode* dst );
extern X86Instr* X86Instr_Set32     ( X86CondCode cond, HReg dst );
extern X86Instr* X86Instr_Bsfr32    ( Bool isFwds, HReg src, HReg dst );
extern X86Instr* X86Instr_MFence    ( UInt hwcaps );
extern X86Instr* X86Instr_ACAS      ( X86AMode* addr, UChar sz );
extern X86Instr* X86Instr_DACAS     ( X86AMode* addr );

extern X86Instr* X86Instr_FpUnary   ( X86FpOp op, HReg src, HReg dst );
extern X86Instr* X86Instr_FpBinary  ( X86FpOp op, HReg srcL, HReg srcR, HReg dst );
extern X86Instr* X86Instr_FpLdSt    ( Bool isLoad, UChar sz, HReg reg, X86AMode* );
extern X86Instr* X86Instr_FpLdStI   ( Bool isLoad, UChar sz, HReg reg, X86AMode* );
extern X86Instr* X86Instr_Fp64to32  ( HReg src, HReg dst );
extern X86Instr* X86Instr_FpCMov    ( X86CondCode, HReg src, HReg dst );
extern X86Instr* X86Instr_FpLdCW    ( X86AMode* );
extern X86Instr* X86Instr_FpStSW_AX ( void );
extern X86Instr* X86Instr_FpCmp     ( HReg srcL, HReg srcR, HReg dst );

extern X86Instr* X86Instr_SseConst  ( UShort con, HReg dst );
extern X86Instr* X86Instr_SseLdSt   ( Bool isLoad, HReg, X86AMode* );
extern X86Instr* X86Instr_SseLdzLO  ( Int sz, HReg, X86AMode* );
extern X86Instr* X86Instr_Sse32Fx4  ( X86SseOp, HReg, HReg );
extern X86Instr* X86Instr_Sse32FLo  ( X86SseOp, HReg, HReg );
extern X86Instr* X86Instr_Sse64Fx2  ( X86SseOp, HReg, HReg );
extern X86Instr* X86Instr_Sse64FLo  ( X86SseOp, HReg, HReg );
extern X86Instr* X86Instr_SseReRg   ( X86SseOp, HReg, HReg );
extern X86Instr* X86Instr_SseCMov   ( X86CondCode, HReg src, HReg dst );
extern X86Instr* X86Instr_SseShuf   ( Int order, HReg src, HReg dst );
extern X86Instr* X86Instr_EvCheck   ( X86AMode* amCounter,
                                      X86AMode* amFailAddr );
extern X86Instr* X86Instr_ProfInc   ( void );


extern void ppX86Instr ( const X86Instr*, Bool );

/* Some functions that insulate the register allocator from details
   of the underlying instruction set. */
extern void         getRegUsage_X86Instr ( HRegUsage*, const X86Instr*, Bool );
extern void         mapRegs_X86Instr     ( HRegRemap*, X86Instr*, Bool );
extern Int          emit_X86Instr   ( /*MB_MOD*/Bool* is_profInc,
                                      UChar* buf, Int nbuf, const X86Instr* i, 
                                      Bool mode64,
                                      VexEndness endness_host,
                                      const void* disp_cp_chain_me_to_slowEP,
                                      const void* disp_cp_chain_me_to_fastEP,
                                      const void* disp_cp_xindir,
                                      const void* disp_cp_xassisted );

extern void genSpill_X86  ( /*OUT*/HInstr** i1, /*OUT*/HInstr** i2,
                            HReg rreg, Int offset, Bool );
extern void genReload_X86 ( /*OUT*/HInstr** i1, /*OUT*/HInstr** i2,
                            HReg rreg, Int offset, Bool );
extern X86Instr* genMove_X86(HReg from, HReg to, Bool);
extern X86Instr* directReload_X86 ( X86Instr* i, HReg vreg, Short spill_off );

extern const RRegUniverse* getRRegUniverse_X86 ( void );

extern HInstrArray* iselSB_X86           ( const IRSB*,
                                           VexArch,
                                           const VexArchInfo*,
                                           const VexAbiInfo*,
                                           Int offs_Host_EvC_Counter,
                                           Int offs_Host_EvC_FailAddr,
                                           Bool chainingAllowed,
                                           Bool addProfInc,
                                           Addr max_ga );

/* How big is an event check?  This is kind of a kludge because it
   depends on the offsets of host_EvC_FAILADDR and host_EvC_COUNTER,
   and so assumes that they are both <= 128, and so can use the short
   offset encoding.  This is all checked with assertions, so in the
   worst case we will merely assert at startup. */
extern Int evCheckSzB_X86 (void);

/* Perform a chaining and unchaining of an XDirect jump. */
extern VexInvalRange chainXDirect_X86 ( VexEndness endness_host,
                                        void* place_to_chain,
                                        const void* disp_cp_chain_me_EXPECTED,
                                        const void* place_to_jump_to );

extern VexInvalRange unchainXDirect_X86 ( VexEndness endness_host,
                                          void* place_to_unchain,
                                          const void* place_to_jump_to_EXPECTED,
                                          const void* disp_cp_chain_me );

/* Patch the counter location into an existing ProfInc point. */
extern VexInvalRange patchProfInc_X86 ( VexEndness endness_host,
                                        void*  place_to_patch,
                                        const ULong* location_of_counter );


#endif /* ndef __VEX_HOST_X86_DEFS_H */

/*---------------------------------------------------------------*/
/*--- end                                     host_x86_defs.h ---*/
/*---------------------------------------------------------------*/
