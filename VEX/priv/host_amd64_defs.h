
/*---------------------------------------------------------------*/
/*--- begin                                 host_amd64_defs.h ---*/
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
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.

   Neither the names of the U.S. Department of Energy nor the
   University of California nor the names of its contributors may be
   used to endorse or promote products derived from this software
   without prior written permission.
*/

#ifndef __VEX_HOST_AMD64_DEFS_H
#define __VEX_HOST_AMD64_DEFS_H

#include "libvex_basictypes.h"
#include "libvex.h"                      // VexArch
#include "host_generic_regs.h"           // HReg

/* --------- Registers. --------- */

/* The usual HReg abstraction.  There are 16 real int regs, 6 real
   float regs, and 16 real vector regs.
*/

#define ST_IN static inline
ST_IN HReg hregAMD64_R12   ( void ) { return mkHReg(False, HRcInt64,  12,  0); }
ST_IN HReg hregAMD64_R13   ( void ) { return mkHReg(False, HRcInt64,  13,  1); }
ST_IN HReg hregAMD64_R14   ( void ) { return mkHReg(False, HRcInt64,  14,  2); }
ST_IN HReg hregAMD64_R15   ( void ) { return mkHReg(False, HRcInt64,  15,  3); }
ST_IN HReg hregAMD64_RBX   ( void ) { return mkHReg(False, HRcInt64,   3,  4); }
ST_IN HReg hregAMD64_RSI   ( void ) { return mkHReg(False, HRcInt64,   6,  5); }
ST_IN HReg hregAMD64_RDI   ( void ) { return mkHReg(False, HRcInt64,   7,  6); }
ST_IN HReg hregAMD64_R8    ( void ) { return mkHReg(False, HRcInt64,   8,  7); }
ST_IN HReg hregAMD64_R9    ( void ) { return mkHReg(False, HRcInt64,   9,  8); }
ST_IN HReg hregAMD64_R10   ( void ) { return mkHReg(False, HRcInt64,  10,  9); }

ST_IN HReg hregAMD64_XMM3  ( void ) { return mkHReg(False, HRcVec128,  3, 10); }
ST_IN HReg hregAMD64_XMM4  ( void ) { return mkHReg(False, HRcVec128,  4, 11); }
ST_IN HReg hregAMD64_XMM5  ( void ) { return mkHReg(False, HRcVec128,  5, 12); }
ST_IN HReg hregAMD64_XMM6  ( void ) { return mkHReg(False, HRcVec128,  6, 13); }
ST_IN HReg hregAMD64_XMM7  ( void ) { return mkHReg(False, HRcVec128,  7, 14); }
ST_IN HReg hregAMD64_XMM8  ( void ) { return mkHReg(False, HRcVec128,  8, 15); }
ST_IN HReg hregAMD64_XMM9  ( void ) { return mkHReg(False, HRcVec128,  9, 16); }
ST_IN HReg hregAMD64_XMM10 ( void ) { return mkHReg(False, HRcVec128, 10, 17); }
ST_IN HReg hregAMD64_XMM11 ( void ) { return mkHReg(False, HRcVec128, 11, 18); }
ST_IN HReg hregAMD64_XMM12 ( void ) { return mkHReg(False, HRcVec128, 12, 19); }

ST_IN HReg hregAMD64_RAX   ( void ) { return mkHReg(False, HRcInt64,   0, 20); }
ST_IN HReg hregAMD64_RCX   ( void ) { return mkHReg(False, HRcInt64,   1, 21); }
ST_IN HReg hregAMD64_RDX   ( void ) { return mkHReg(False, HRcInt64,   2, 22); }
ST_IN HReg hregAMD64_RSP   ( void ) { return mkHReg(False, HRcInt64,   4, 23); }
ST_IN HReg hregAMD64_RBP   ( void ) { return mkHReg(False, HRcInt64,   5, 24); }
ST_IN HReg hregAMD64_R11   ( void ) { return mkHReg(False, HRcInt64,  11, 25); }

ST_IN HReg hregAMD64_XMM0  ( void ) { return mkHReg(False, HRcVec128,  0, 26); }
ST_IN HReg hregAMD64_XMM1  ( void ) { return mkHReg(False, HRcVec128,  1, 27); }
#undef ST_IN

extern UInt ppHRegAMD64 ( HReg );


/* --------- Condition codes, AMD encoding. --------- */

typedef
   enum {
      Acc_O      = 0,  /* overflow           */
      Acc_NO     = 1,  /* no overflow        */

      Acc_B      = 2,  /* below              */
      Acc_NB     = 3,  /* not below          */

      Acc_Z      = 4,  /* zero               */
      Acc_NZ     = 5,  /* not zero           */

      Acc_BE     = 6,  /* below or equal     */
      Acc_NBE    = 7,  /* not below or equal */

      Acc_S      = 8,  /* negative           */
      Acc_NS     = 9,  /* not negative       */

      Acc_P      = 10, /* parity even        */
      Acc_NP     = 11, /* not parity even    */

      Acc_L      = 12, /* jump less          */
      Acc_NL     = 13, /* not less           */

      Acc_LE     = 14, /* less or equal      */
      Acc_NLE    = 15, /* not less or equal  */

      Acc_ALWAYS = 16  /* the usual hack     */
   }
   AMD64CondCode;

extern const HChar* showAMD64CondCode ( AMD64CondCode );


/* --------- Memory address expressions (amodes). --------- */

typedef
   enum {
     Aam_IR,        /* Immediate + Reg */
     Aam_IRRS       /* Immediate + Reg1 + (Reg2 << Shift) */
   }
   AMD64AModeTag;

typedef
   struct {
      AMD64AModeTag tag;
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
      } Aam;
   }
   AMD64AMode;

extern AMD64AMode* AMD64AMode_IR   ( UInt, HReg );
extern AMD64AMode* AMD64AMode_IRRS ( UInt, HReg, HReg, Int );

extern AMD64AMode* dopyAMD64AMode ( AMD64AMode* );

extern void ppAMD64AMode ( AMD64AMode* );


/* --------- Operand, which can be reg, immediate or memory. --------- */

typedef 
   enum {
      Armi_Imm,
      Armi_Reg,
      Armi_Mem
   }
   AMD64RMITag;

typedef
   struct {
      AMD64RMITag tag;
      union {
         struct {
            UInt imm32;
         } Imm;
         struct {
            HReg reg;
         } Reg;
         struct {
            AMD64AMode* am;
         } Mem;
      }
      Armi;
   }
   AMD64RMI;

extern AMD64RMI* AMD64RMI_Imm ( UInt );
extern AMD64RMI* AMD64RMI_Reg ( HReg );
extern AMD64RMI* AMD64RMI_Mem ( AMD64AMode* );

extern void ppAMD64RMI      ( AMD64RMI* );
extern void ppAMD64RMI_lo32 ( AMD64RMI* );


/* --------- Operand, which can be reg or immediate only. --------- */

typedef 
   enum {
      Ari_Imm,
      Ari_Reg
   }
   AMD64RITag;

typedef
   struct {
      AMD64RITag tag;
      union {
         struct {
            UInt imm32;
         } Imm;
         struct {
            HReg reg;
         } Reg;
      }
      Ari;
   }
   AMD64RI;

extern AMD64RI* AMD64RI_Imm ( UInt );
extern AMD64RI* AMD64RI_Reg ( HReg );

extern void ppAMD64RI ( AMD64RI* );


/* --------- Operand, which can be reg or memory only. --------- */

typedef 
   enum {
      Arm_Reg,
      Arm_Mem
   }
   AMD64RMTag;

typedef
   struct {
      AMD64RMTag tag;
      union {
         struct {
            HReg reg;
         } Reg;
         struct {
            AMD64AMode* am;
         } Mem;
      }
      Arm;
   }
   AMD64RM;

extern AMD64RM* AMD64RM_Reg ( HReg );
extern AMD64RM* AMD64RM_Mem ( AMD64AMode* );

extern void ppAMD64RM ( AMD64RM* );


/* --------- Instructions. --------- */

/* --------- */
typedef
   enum {
      Aun_NEG,
      Aun_NOT
   }
   AMD64UnaryOp;

extern const HChar* showAMD64UnaryOp ( AMD64UnaryOp );


/* --------- */
typedef 
   enum {
      Aalu_INVALID,
      Aalu_MOV,
      Aalu_CMP,
      Aalu_ADD, Aalu_SUB, Aalu_ADC, Aalu_SBB, 
      Aalu_AND, Aalu_OR, Aalu_XOR,
      Aalu_MUL
   }
   AMD64AluOp;

extern const HChar* showAMD64AluOp ( AMD64AluOp );


/* --------- */
typedef
   enum {
      Ash_INVALID,
      Ash_SHL, Ash_SHR, Ash_SAR
   }
   AMD64ShiftOp;

extern const HChar* showAMD64ShiftOp ( AMD64ShiftOp );


/* --------- */
typedef
   enum {
      Afp_INVALID,
      /* Binary */
      Afp_SCALE, Afp_ATAN, Afp_YL2X, Afp_YL2XP1, Afp_PREM, Afp_PREM1,
      /* Unary */
      Afp_SQRT,
      Afp_SIN, Afp_COS, Afp_TAN,
      Afp_ROUND, Afp_2XM1
   }
   A87FpOp;

extern const HChar* showA87FpOp ( A87FpOp );


/* --------- */
typedef
   enum {
      Asse_INVALID,
      /* mov */
      Asse_MOV,
      /* Floating point binary */
      Asse_ADDF, Asse_SUBF, Asse_MULF, Asse_DIVF,
      Asse_MAXF, Asse_MINF,
      Asse_CMPEQF, Asse_CMPLTF, Asse_CMPLEF, Asse_CMPUNF,
      /* Floating point unary */
      Asse_RCPF, Asse_RSQRTF, Asse_SQRTF, 
      /* Floating point conversion */
      Asse_I2F, // i32-signed to float conversion, aka cvtdq2ps in vec form
      Asse_F2I, // float to i32-signed conversion, aka cvtps2dq in vec form
      /* Bitwise */
      Asse_AND, Asse_OR, Asse_XOR, Asse_ANDN,
      Asse_ADD8, Asse_ADD16, Asse_ADD32, Asse_ADD64,
      Asse_QADD8U, Asse_QADD16U,
      Asse_QADD8S, Asse_QADD16S,
      Asse_SUB8, Asse_SUB16, Asse_SUB32, Asse_SUB64,
      Asse_QSUB8U, Asse_QSUB16U,
      Asse_QSUB8S, Asse_QSUB16S,
      Asse_MUL16,
      Asse_MULHI16U,
      Asse_MULHI16S,
      Asse_AVG8U, Asse_AVG16U,
      Asse_MAX16S,
      Asse_MAX8U,
      Asse_MIN16S,
      Asse_MIN8U,
      Asse_CMPEQ8, Asse_CMPEQ16, Asse_CMPEQ32,
      Asse_CMPGT8S, Asse_CMPGT16S, Asse_CMPGT32S,
      Asse_SHL16, Asse_SHL32, Asse_SHL64, Asse_SHL128,
      Asse_SHR16, Asse_SHR32, Asse_SHR64, Asse_SHR128,
      Asse_SAR16, Asse_SAR32, 
      Asse_PACKSSD, Asse_PACKSSW, Asse_PACKUSW,
      Asse_UNPCKHB, Asse_UNPCKHW, Asse_UNPCKHD, Asse_UNPCKHQ,
      Asse_UNPCKLB, Asse_UNPCKLW, Asse_UNPCKLD, Asse_UNPCKLQ,
      // Only for SSSE3 capable hosts:
      Asse_PSHUFB,
      Asse_PMADDUBSW,
      // Only for F16C capable hosts:
      Asse_F32toF16, // F32 to F16 conversion, aka vcvtps2ph
      Asse_F16toF32, // F16 to F32 conversion, aka vcvtph2ps
      // Only for FMA (FMA3) capable hosts:
      Asse_VFMADD213, // Fused Multiply-Add, aka vfmadd213ss
   }
   AMD64SseOp;

extern const HChar* showAMD64SseOp ( AMD64SseOp );


/* --------- */
typedef
   enum {
      Ain_Imm64,       /* Generate 64-bit literal to register */
      Ain_Alu64R,      /* 64-bit mov/arith/logical, dst=REG */
      Ain_Alu64M,      /* 64-bit mov/arith/logical, dst=MEM */
      Ain_Sh64,        /* 64-bit shift, dst=REG */
      Ain_Sh32,        /* 32-bit shift, dst=REG */
      Ain_Test64,      /* 64-bit test (AND, set flags, discard result) */
      Ain_Unary64,     /* 64-bit not and neg */
      Ain_Lea64,       /* 64-bit compute EA into a reg */
      Ain_Alu32R,      /* 32-bit add/sub/and/or/xor/cmp, dst=REG (a la Alu64R) */
      Ain_MulL,        /* widening multiply */
      Ain_Div,         /* div and mod */
      Ain_Push,        /* push 64-bit value on stack */
      Ain_Call,        /* call to address in register */
      Ain_XDirect,     /* direct transfer to GA */
      Ain_XIndir,      /* indirect transfer to GA */
      Ain_XAssisted,   /* assisted transfer to GA */
      Ain_CMov64,      /* conditional move, 64-bit reg-reg only */
      Ain_CLoad,       /* cond. load to int reg, 32 bit ZX or 64 bit only */
      Ain_CStore,      /* cond. store from int reg, 32 or 64 bit only */
      Ain_MovxLQ,      /* reg-reg move, zx-ing/sx-ing top half */
      Ain_LoadEX,      /* mov{s,z}{b,w,l}q from mem to reg */
      Ain_Store,       /* store 32/16/8 bit value in memory */
      Ain_Set64,       /* convert condition code to 64-bit value */
      Ain_Bsfr64,      /* 64-bit bsf/bsr */
      Ain_MFence,      /* mem fence */
      Ain_ACAS,        /* 8/16/32/64-bit lock;cmpxchg */
      Ain_DACAS,       /* lock;cmpxchg8b/16b (doubleword ACAS, 2 x
                          32-bit or 2 x 64-bit only) */
      Ain_A87Free,     /* free up x87 registers */
      Ain_A87PushPop,  /* x87 loads/stores */
      Ain_A87FpOp,     /* x87 operations */
      Ain_A87LdCW,     /* load x87 control word */
      Ain_A87StSW,     /* store x87 status word */
      Ain_LdMXCSR,     /* load %mxcsr */
      Ain_SseUComIS,   /* ucomisd/ucomiss, then get %rflags into int
                          register */
      Ain_SseSI2SF,    /* scalar 32/64 int to 32/64 float conversion */
      Ain_SseSF2SI,    /* scalar 32/64 float to 32/64 int conversion */
      Ain_SseSDSS,     /* scalar float32 to/from float64 */
      Ain_SseLdSt,     /* SSE load/store 32/64/128 bits, no alignment
                          constraints, upper 96/64/0 bits arbitrary */
      Ain_SseCStore,   /* SSE conditional store, 128 bit only, any alignment */
      Ain_SseCLoad,    /* SSE conditional load, 128 bit only, any alignment */
      Ain_SseLdzLO,    /* SSE load low 32/64 bits, zero remainder of reg */
      Ain_Sse32Fx4,    /* SSE binary, 32Fx4 */
      Ain_Sse32FLo,    /* SSE binary, 32F in lowest lane only */
      Ain_Sse64Fx2,    /* SSE binary, 64Fx2 */
      Ain_Sse64FLo,    /* SSE binary, 64F in lowest lane only */
      Ain_SseReRg,     /* SSE binary general reg-reg, Re, Rg */
      Ain_SseCMov,     /* SSE conditional move */
      Ain_SseShuf,     /* SSE2 shuffle (pshufd) */
      Ain_SseShiftN,   /* SSE2 shift by immediate */
      Ain_SseMOVQ,     /* SSE2 moves of xmm[63:0] to/from GPR */
      //uu Ain_AvxLdSt,     /* AVX load/store 256 bits,
      //uu                     no alignment constraints */
      //uu Ain_AvxReRg,     /* AVX binary general reg-reg, Re, Rg */
      Ain_Avx32FLo,    /* AVX binary 3 operand, 32F in lowest lane only */
      Ain_Avx64FLo,    /* AVX binary 3 operand, 64F in lowest lane only */
      Ain_EvCheck,     /* Event check */
      Ain_ProfInc      /* 64-bit profile counter increment */
   }
   AMD64InstrTag;

/* Destinations are on the RIGHT (second operand) */

typedef
   struct {
      AMD64InstrTag tag;
      union {
         struct {
            ULong imm64;
            HReg  dst;
         } Imm64;
         struct {
            AMD64AluOp op;
            AMD64RMI*  src;
            HReg       dst;
         } Alu64R;
         struct {
            AMD64AluOp  op;
            AMD64RI*    src;
            AMD64AMode* dst;
         } Alu64M;
         struct {
            AMD64ShiftOp op;
            UInt         src;  /* shift amount, or 0 means %cl */
            HReg         dst;
         } Sh64;
         struct {
            AMD64ShiftOp op;
            UInt         src;  /* shift amount, or 0 means %cl */
            HReg         dst;
         } Sh32;
         struct {
            UInt   imm32;
            HReg   dst;
         } Test64;
         /* Not and Neg */
         struct {
            AMD64UnaryOp op;
            HReg         dst;
         } Unary64;
         /* 64-bit compute EA into a reg */
         struct {
            AMD64AMode* am;
            HReg        dst;
         } Lea64;
         /* 32-bit add/sub/and/or/xor/cmp, dst=REG (a la Alu64R) */
         struct {
            AMD64AluOp op;
            AMD64RMI*  src;
            HReg       dst;
         } Alu32R;
         /* 64 x 64 -> 128 bit widening multiply: RDX:RAX = RAX *s/u
            r/m64 */
         struct {
            Bool     syned;
            AMD64RM* src;
         } MulL;
          /* amd64 div/idiv instruction.  Modifies RDX and RAX and
	     reads src. */
         struct {
            Bool     syned;
            Int      sz; /* 4 or 8 only */
            AMD64RM* src;
         } Div;
         struct {
            AMD64RMI* src;
         } Push;
         /* Pseudo-insn.  Call target (an absolute address), on given
            condition (which could be Xcc_ALWAYS). */
         struct {
            AMD64CondCode cond;
            Addr64        target;
            Int           regparms; /* 0 .. 6 */
            RetLoc        rloc;     /* where the return value will be */
         } Call;
         /* Update the guest RIP value, then exit requesting to chain
            to it.  May be conditional. */
         struct {
            Addr64        dstGA;    /* next guest address */
            AMD64AMode*   amRIP;    /* amode in guest state for RIP */
            AMD64CondCode cond;     /* can be Acc_ALWAYS */
            Bool          toFastEP; /* chain to the slow or fast point? */
         } XDirect;
         /* Boring transfer to a guest address not known at JIT time.
            Not chainable.  May be conditional. */
         struct {
            HReg          dstGA;
            AMD64AMode*   amRIP;
            AMD64CondCode cond; /* can be Acc_ALWAYS */
         } XIndir;
         /* Assisted transfer to a guest address, most general case.
            Not chainable.  May be conditional. */
         struct {
            HReg          dstGA;
            AMD64AMode*   amRIP;
            AMD64CondCode cond; /* can be Acc_ALWAYS */
            IRJumpKind    jk;
         } XAssisted;
         /* Mov src to dst on the given condition, which may not
            be the bogus Acc_ALWAYS. */
         struct {
            AMD64CondCode cond;
            HReg          src;
            HReg          dst;
         } CMov64;
         /* conditional load to int reg, 32 bit ZX or 64 bit only.
            cond may not be Acc_ALWAYS. */
         struct {
            AMD64CondCode cond;
            UChar         szB; /* 4 or 8 only */
            AMD64AMode*   addr;
            HReg          dst;
         } CLoad;
         /* cond. store from int reg, 32 or 64 bit only.
            cond may not be Acc_ALWAYS. */
         struct {
            AMD64CondCode cond;
            UChar         szB; /* 4 or 8 only */
            HReg          src;
            AMD64AMode*   addr;
         } CStore;
         /* reg-reg move, sx-ing/zx-ing top half */
         struct {
            Bool syned;
            HReg src;
            HReg dst;
         } MovxLQ;
         /* Sign/Zero extending loads.  Dst size is always 64 bits. */
         struct {
            UChar       szSmall; /* only 1, 2 or 4 */
            Bool        syned;
            AMD64AMode* src;
            HReg        dst;
         } LoadEX;
         /* 32/16/8 bit stores. */
         struct {
            UChar       sz; /* only 1, 2 or 4 */
            HReg        src;
            AMD64AMode* dst;
         } Store;
         /* Convert an amd64 condition code to a 64-bit value (0 or 1). */
         struct {
            AMD64CondCode cond;
            HReg          dst;
         } Set64;
         /* 64-bit bsf or bsr. */
         struct {
            Bool isFwds;
            HReg src;
            HReg dst;
         } Bsfr64;
         /* Mem fence.  In short, an insn which flushes all preceding
            loads and stores as much as possible before continuing.
            On AMD64 we emit a real "mfence". */
         struct {
         } MFence;
         struct {
            AMD64AMode* addr;
            UChar       sz; /* 1, 2, 4 or 8 */
         } ACAS;
         struct {
            AMD64AMode* addr;
            UChar       sz; /* 4 or 8 only */
         } DACAS;

         /* --- X87 --- */

         /* A very minimal set of x87 insns, that operate exactly in a
            stack-like way so no need to think about x87 registers. */

         /* Do 'ffree' on %st(7) .. %st(7-nregs) */
         struct {
            Int nregs; /* 1 <= nregs <= 7 */
         } A87Free;

         /* Push a 32- or 64-bit FP value from memory onto the stack,
            or move a value from the stack to memory and remove it
            from the stack. */
         struct {
            AMD64AMode* addr;
            Bool        isPush;
            UChar       szB; /* 4 or 8 */
         } A87PushPop;

         /* Do an operation on the top-of-stack.  This can be unary, in
            which case it is %st0 = OP( %st0 ), or binary: %st0 = OP(
            %st0, %st1 ). */
         struct {
            A87FpOp op;
         } A87FpOp;

         /* Load the FPU control word. */
         struct {
            AMD64AMode* addr;
         } A87LdCW;

         /* Store the FPU status word (fstsw m16) */
         struct {
            AMD64AMode* addr;
         } A87StSW;

         /* --- SSE --- */

         /* Load 32 bits into %mxcsr. */
         struct {
            AMD64AMode* addr;
         }
         LdMXCSR;
         /* ucomisd/ucomiss, then get %rflags into int register */
         struct {
            UChar   sz;   /* 4 or 8 only */
            HReg    srcL; /* xmm */
            HReg    srcR; /* xmm */
            HReg    dst;  /* int */
         } SseUComIS;
         /* scalar 32/64 int to 32/64 float conversion */
         struct {
            UChar szS; /* 4 or 8 */
            UChar szD; /* 4 or 8 */
            HReg  src; /* i class */
            HReg  dst; /* v class */
         } SseSI2SF;
         /* scalar 32/64 float to 32/64 int conversion */
         struct {
            UChar szS; /* 4 or 8 */
            UChar szD; /* 4 or 8 */
            HReg  src; /* v class */
            HReg  dst; /* i class */
         } SseSF2SI;
         /* scalar float32 to/from float64 */
         struct {
            Bool from64; /* True: 64->32; False: 32->64 */
            HReg src;
            HReg dst;
         } SseSDSS;
         struct {
            Bool        isLoad;
            UChar       sz; /* 4, 8 or 16 only */
            HReg        reg;
            AMD64AMode* addr;
         } SseLdSt;
         struct {
            AMD64CondCode cond; /* may not be Acc_ALWAYS */
            HReg          src;
            AMD64AMode*   addr;
         } SseCStore;
         struct {
            AMD64CondCode cond; /* may not be Acc_ALWAYS */
            AMD64AMode*   addr;
            HReg          dst;
         } SseCLoad;
         struct {
            Int         sz; /* 4 or 8 only */
            HReg        reg;
            AMD64AMode* addr;
         } SseLdzLO;
         struct {
            AMD64SseOp op;
            HReg       src;
            HReg       dst;
         } Sse32Fx4;
         struct {
            AMD64SseOp op;
            HReg       src;
            HReg       dst;
         } Sse32FLo;
         struct {
            AMD64SseOp op;
            HReg       src;
            HReg       dst;
         } Sse64Fx2;
         struct {
            AMD64SseOp op;
            HReg       src;
            HReg       dst;
         } Sse64FLo;
         struct {
            AMD64SseOp op;
            HReg       src;
            HReg       dst;
         } SseReRg;
         /* Mov src to dst on the given condition, which may not
            be the bogus Xcc_ALWAYS. */
         struct {
            AMD64CondCode cond;
            HReg          src;
            HReg          dst;
         } SseCMov;
         struct {
            Int    order; /* 0 <= order <= 0xFF */
            HReg   src;
            HReg   dst;
         } SseShuf;
         struct {
            AMD64SseOp op;
            UInt       shiftBits;
            HReg       dst;
         } SseShiftN;
         struct {
            HReg gpr;
            HReg xmm;
            Bool toXMM; // when moving to xmm, xmm[127:64] is zeroed out
         } SseMOVQ;
         //uu struct {
         //uu    Bool        isLoad;
         //uu    HReg        reg;
         //uu    AMD64AMode* addr;
         //uu } AvxLdSt;
         //uu struct {
         //uu    AMD64SseOp op;
         //uu    HReg       src;
         //uu    HReg       dst;
         //uu } AvxReRg;
         struct {
            AMD64SseOp op;
            HReg       src1;
            HReg       src2;
            HReg       dst;
         } Avx32FLo;
         struct {
            AMD64SseOp op;
            HReg       src1;
            HReg       src2;
            HReg       dst;
         } Avx64FLo;
         struct {
            AMD64AMode* amCounter;
            AMD64AMode* amFailAddr;
         } EvCheck;
         struct {
            /* No fields.  The address of the counter to inc is
               installed later, post-translation, by patching it in,
               as it is not known at translation time. */
         } ProfInc;

      } Ain;
   }
   AMD64Instr;

extern AMD64Instr* AMD64Instr_Imm64      ( ULong imm64, HReg dst );
extern AMD64Instr* AMD64Instr_Alu64R     ( AMD64AluOp, AMD64RMI*, HReg );
extern AMD64Instr* AMD64Instr_Alu64M     ( AMD64AluOp, AMD64RI*,  AMD64AMode* );
extern AMD64Instr* AMD64Instr_Unary64    ( AMD64UnaryOp op, HReg dst );
extern AMD64Instr* AMD64Instr_Lea64      ( AMD64AMode* am, HReg dst );
extern AMD64Instr* AMD64Instr_Alu32R     ( AMD64AluOp, AMD64RMI*, HReg );
extern AMD64Instr* AMD64Instr_Sh64       ( AMD64ShiftOp, UInt, HReg );
extern AMD64Instr* AMD64Instr_Sh32       ( AMD64ShiftOp, UInt, HReg );
extern AMD64Instr* AMD64Instr_Test64     ( UInt imm32, HReg dst );
extern AMD64Instr* AMD64Instr_MulL       ( Bool syned, AMD64RM* );
extern AMD64Instr* AMD64Instr_Div        ( Bool syned, Int sz, AMD64RM* );
extern AMD64Instr* AMD64Instr_Push       ( AMD64RMI* );
extern AMD64Instr* AMD64Instr_Call       ( AMD64CondCode, Addr64, Int, RetLoc );
extern AMD64Instr* AMD64Instr_XDirect    ( Addr64 dstGA, AMD64AMode* amRIP,
                                           AMD64CondCode cond, Bool toFastEP );
extern AMD64Instr* AMD64Instr_XIndir     ( HReg dstGA, AMD64AMode* amRIP,
                                           AMD64CondCode cond );
extern AMD64Instr* AMD64Instr_XAssisted  ( HReg dstGA, AMD64AMode* amRIP,
                                           AMD64CondCode cond, IRJumpKind jk );
extern AMD64Instr* AMD64Instr_CMov64     ( AMD64CondCode, HReg src, HReg dst );
extern AMD64Instr* AMD64Instr_CLoad      ( AMD64CondCode cond, UChar szB,
                                           AMD64AMode* addr, HReg dst );
extern AMD64Instr* AMD64Instr_CStore     ( AMD64CondCode cond, UChar szB,
                                           HReg src, AMD64AMode* addr );
extern AMD64Instr* AMD64Instr_MovxLQ     ( Bool syned, HReg src, HReg dst );
extern AMD64Instr* AMD64Instr_LoadEX     ( UChar szSmall, Bool syned,
                                           AMD64AMode* src, HReg dst );
extern AMD64Instr* AMD64Instr_Store      ( UChar sz, HReg src, AMD64AMode* dst );
extern AMD64Instr* AMD64Instr_Set64      ( AMD64CondCode cond, HReg dst );
extern AMD64Instr* AMD64Instr_Bsfr64     ( Bool isFwds, HReg src, HReg dst );
extern AMD64Instr* AMD64Instr_MFence     ( void );
extern AMD64Instr* AMD64Instr_ACAS       ( AMD64AMode* addr, UChar sz );
extern AMD64Instr* AMD64Instr_DACAS      ( AMD64AMode* addr, UChar sz );

extern AMD64Instr* AMD64Instr_A87Free    ( Int nregs );
extern AMD64Instr* AMD64Instr_A87PushPop ( AMD64AMode* addr, Bool isPush, UChar szB );
extern AMD64Instr* AMD64Instr_A87FpOp    ( A87FpOp op );
extern AMD64Instr* AMD64Instr_A87LdCW    ( AMD64AMode* addr );
extern AMD64Instr* AMD64Instr_A87StSW    ( AMD64AMode* addr );
extern AMD64Instr* AMD64Instr_LdMXCSR    ( AMD64AMode* );
extern AMD64Instr* AMD64Instr_SseUComIS  ( Int sz, HReg srcL, HReg srcR, HReg dst );
extern AMD64Instr* AMD64Instr_SseSI2SF   ( Int szS, Int szD, HReg src, HReg dst );
extern AMD64Instr* AMD64Instr_SseSF2SI   ( Int szS, Int szD, HReg src, HReg dst );
extern AMD64Instr* AMD64Instr_SseSDSS    ( Bool from64, HReg src, HReg dst );
extern AMD64Instr* AMD64Instr_SseLdSt    ( Bool isLoad, Int sz, HReg, AMD64AMode* );
extern AMD64Instr* AMD64Instr_SseCStore  ( AMD64CondCode, HReg, AMD64AMode* );
extern AMD64Instr* AMD64Instr_SseCLoad   ( AMD64CondCode, AMD64AMode*, HReg );
extern AMD64Instr* AMD64Instr_SseLdzLO   ( Int sz, HReg, AMD64AMode* );
extern AMD64Instr* AMD64Instr_Sse32Fx4   ( AMD64SseOp, HReg, HReg );
extern AMD64Instr* AMD64Instr_Sse32FLo   ( AMD64SseOp, HReg, HReg );
extern AMD64Instr* AMD64Instr_Sse64Fx2   ( AMD64SseOp, HReg, HReg );
extern AMD64Instr* AMD64Instr_Sse64FLo   ( AMD64SseOp, HReg, HReg );
extern AMD64Instr* AMD64Instr_SseReRg    ( AMD64SseOp, HReg, HReg );
extern AMD64Instr* AMD64Instr_SseCMov    ( AMD64CondCode, HReg src, HReg dst );
extern AMD64Instr* AMD64Instr_SseShuf    ( Int order, HReg src, HReg dst );
extern AMD64Instr* AMD64Instr_SseShiftN  ( AMD64SseOp,
                                           UInt shiftBits, HReg dst );
extern AMD64Instr* AMD64Instr_SseMOVQ    ( HReg gpr, HReg xmm, Bool toXMM );
//uu extern AMD64Instr* AMD64Instr_AvxLdSt    ( Bool isLoad, HReg, AMD64AMode* );
//uu extern AMD64Instr* AMD64Instr_AvxReRg    ( AMD64SseOp, HReg, HReg );
extern AMD64Instr* AMD64Instr_Avx32FLo   ( AMD64SseOp, HReg, HReg, HReg );
extern AMD64Instr* AMD64Instr_Avx64FLo   ( AMD64SseOp, HReg, HReg, HReg );
extern AMD64Instr* AMD64Instr_EvCheck    ( AMD64AMode* amCounter,
                                           AMD64AMode* amFailAddr );
extern AMD64Instr* AMD64Instr_ProfInc    ( void );


extern void ppAMD64Instr ( const AMD64Instr*, Bool );

/* Some functions that insulate the register allocator from details
   of the underlying instruction set. */
extern void getRegUsage_AMD64Instr ( HRegUsage*, const AMD64Instr*, Bool );
extern void mapRegs_AMD64Instr     ( HRegRemap*, AMD64Instr*, Bool );
extern Int          emit_AMD64Instr   ( /*MB_MOD*/Bool* is_profInc,
                                        UChar* buf, Int nbuf,
                                        const AMD64Instr* i, 
                                        Bool mode64,
                                        VexEndness endness_host,
                                        const void* disp_cp_chain_me_to_slowEP,
                                        const void* disp_cp_chain_me_to_fastEP,
                                        const void* disp_cp_xindir,
                                        const void* disp_cp_xassisted );

extern void genSpill_AMD64  ( /*OUT*/HInstr** i1, /*OUT*/HInstr** i2,
                              HReg rreg, Int offset, Bool );
extern void genReload_AMD64 ( /*OUT*/HInstr** i1, /*OUT*/HInstr** i2,
                              HReg rreg, Int offset, Bool );
extern AMD64Instr* genMove_AMD64(HReg from, HReg to, Bool);
extern AMD64Instr* directReload_AMD64 ( AMD64Instr* i,
                                        HReg vreg, Short spill_off );

extern const RRegUniverse* getRRegUniverse_AMD64 ( void );

extern HInstrArray* iselSB_AMD64           ( const IRSB*, 
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
extern Int evCheckSzB_AMD64 (void);

/* Perform a chaining and unchaining of an XDirect jump. */
extern VexInvalRange chainXDirect_AMD64 ( VexEndness endness_host,
                                          void* place_to_chain,
                                          const void* disp_cp_chain_me_EXPECTED,
                                          const void* place_to_jump_to );

extern VexInvalRange unchainXDirect_AMD64 ( VexEndness endness_host,
                                            void* place_to_unchain,
                                            const void* place_to_jump_to_EXPECTED,
                                            const void* disp_cp_chain_me );

/* Patch the counter location into an existing ProfInc point. */
extern VexInvalRange patchProfInc_AMD64 ( VexEndness endness_host,
                                          void*  place_to_patch,
                                          const ULong* location_of_counter );


#endif /* ndef __VEX_HOST_AMD64_DEFS_H */

/*---------------------------------------------------------------*/
/*--- end                                   host_amd64_defs.h ---*/
/*---------------------------------------------------------------*/
