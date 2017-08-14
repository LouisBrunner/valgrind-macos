
/*---------------------------------------------------------------*/
/*--- begin                                 host_amd64_defs.h ---*/
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

#ifndef __VEX_HOST_AMD64_DEFS_H
#define __VEX_HOST_AMD64_DEFS_H


/* --------- Registers. --------- */

/* The usual HReg abstraction.  There are 16 real int regs, 6 real
   float regs, and 16 real vector regs.
*/

extern void ppHRegAMD64 ( HReg );

extern HReg hregAMD64_RAX ( void );
extern HReg hregAMD64_RBX ( void );
extern HReg hregAMD64_RCX ( void );
extern HReg hregAMD64_RDX ( void );
extern HReg hregAMD64_RSP ( void );
extern HReg hregAMD64_RBP ( void );
extern HReg hregAMD64_RSI ( void );
extern HReg hregAMD64_RDI ( void );
extern HReg hregAMD64_R8  ( void );
extern HReg hregAMD64_R9  ( void );
extern HReg hregAMD64_R10 ( void );
extern HReg hregAMD64_R11 ( void );
extern HReg hregAMD64_R12 ( void );
extern HReg hregAMD64_R13 ( void );
extern HReg hregAMD64_R14 ( void );
extern HReg hregAMD64_R15 ( void );

extern HReg hregAMD64_FAKE0 ( void );
extern HReg hregAMD64_FAKE1 ( void );
extern HReg hregAMD64_FAKE2 ( void );
extern HReg hregAMD64_FAKE3 ( void );
extern HReg hregAMD64_FAKE4 ( void );
extern HReg hregAMD64_FAKE5 ( void );

extern HReg hregAMD64_XMM0  ( void );
extern HReg hregAMD64_XMM1  ( void );
extern HReg hregAMD64_XMM2  ( void );
extern HReg hregAMD64_XMM3  ( void );
extern HReg hregAMD64_XMM4  ( void );
extern HReg hregAMD64_XMM5  ( void );
extern HReg hregAMD64_XMM6  ( void );
extern HReg hregAMD64_XMM7  ( void );
extern HReg hregAMD64_XMM8  ( void );
extern HReg hregAMD64_XMM9  ( void );
extern HReg hregAMD64_XMM10 ( void );
extern HReg hregAMD64_XMM11 ( void );
extern HReg hregAMD64_XMM12 ( void );
extern HReg hregAMD64_XMM13 ( void );
extern HReg hregAMD64_XMM14 ( void );
extern HReg hregAMD64_XMM15 ( void );


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

extern HChar* showAMD64CondCode ( AMD64CondCode );


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

extern void ppAMD64RMI ( AMD64RMI* );


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

extern HChar* showAMD64UnaryOp ( AMD64UnaryOp );


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

extern HChar* showAMD64AluOp ( AMD64AluOp );


/* --------- */
typedef
   enum {
      Ash_INVALID,
      Ash_SHL, Ash_SHR, Ash_SAR
   }
   AMD64ShiftOp;

extern HChar* showAMD64ShiftOp ( AMD64ShiftOp );


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

extern HChar* showA87FpOp ( A87FpOp );


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
      Asse_SHL16, Asse_SHL32, Asse_SHL64,
      Asse_SHR16, Asse_SHR32, Asse_SHR64,
      Asse_SAR16, Asse_SAR32, 
      Asse_PACKSSD, Asse_PACKSSW, Asse_PACKUSW,
      Asse_UNPCKHB, Asse_UNPCKHW, Asse_UNPCKHD, Asse_UNPCKHQ,
      Asse_UNPCKLB, Asse_UNPCKLW, Asse_UNPCKLD, Asse_UNPCKLQ
   }
   AMD64SseOp;

extern HChar* showAMD64SseOp ( AMD64SseOp );


/* --------- */
typedef
   enum {
      Ain_Imm64,       /* Generate 64-bit literal to register */
      Ain_Alu64R,      /* 64-bit mov/arith/logical, dst=REG */
      Ain_Alu64M,      /* 64-bit mov/arith/logical, dst=MEM */
      Ain_Sh64,        /* 64-bit shift/rotate, dst=REG or MEM */
      Ain_Test64,      /* 64-bit test (AND, set flags, discard result) */
      Ain_Unary64,     /* 64-bit not and neg */
      Ain_Lea64,       /* 64-bit compute EA into a reg */
      Ain_MulL,        /* widening multiply */
      Ain_Div,         /* div and mod */
//..       Xin_Sh3232,    /* shldl or shrdl */
      Ain_Push,        /* push 64-bit value on stack */
      Ain_Call,        /* call to address in register */
      Ain_Goto,        /* conditional/unconditional jmp to dst */
      Ain_CMov64,      /* conditional move */
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
//.. 
//..       Xin_FpUnary,   /* FP fake unary op */
//..       Xin_FpBinary,  /* FP fake binary op */
//..       Xin_FpLdSt,    /* FP fake load/store */
//..       Xin_FpLdStI,   /* FP fake load/store, converting to/from Int */
//..       Xin_Fp64to32,  /* FP round IEEE754 double to IEEE754 single */
//..       Xin_FpCMov,    /* FP fake floating point conditional move */
      Ain_LdMXCSR,     /* load %mxcsr */
//..       Xin_FpStSW_AX, /* fstsw %ax */
      Ain_SseUComIS,   /* ucomisd/ucomiss, then get %rflags into int
                          register */
      Ain_SseSI2SF,    /* scalar 32/64 int to 32/64 float conversion */
      Ain_SseSF2SI,    /* scalar 32/64 float to 32/64 int conversion */
      Ain_SseSDSS,     /* scalar float32 to/from float64 */
//.. 
//..       Xin_SseConst,  /* Generate restricted SSE literal */
      Ain_SseLdSt,     /* SSE load/store 32/64/128 bits, no alignment
                          constraints, upper 96/64/0 bits arbitrary */
      Ain_SseLdzLO,    /* SSE load low 32/64 bits, zero remainder of reg */
      Ain_Sse32Fx4,    /* SSE binary, 32Fx4 */
      Ain_Sse32FLo,    /* SSE binary, 32F in lowest lane only */
      Ain_Sse64Fx2,    /* SSE binary, 64Fx2 */
      Ain_Sse64FLo,    /* SSE binary, 64F in lowest lane only */
      Ain_SseReRg,     /* SSE binary general reg-reg, Re, Rg */
      Ain_SseCMov,     /* SSE conditional move */
      Ain_SseShuf      /* SSE2 shuffle (pshufd) */
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
//..          /* shld/shrd.  op may only be Xsh_SHL or Xsh_SHR */
//..          struct {
//..             X86ShiftOp op;
//..             UInt       amt;   /* shift amount, or 0 means %cl */
//..             HReg       src;
//..             HReg       dst;
//..          } Sh3232;
         struct {
            AMD64RMI* src;
         } Push;
         /* Pseudo-insn.  Call target (an absolute address), on given
            condition (which could be Xcc_ALWAYS). */
         struct {
            AMD64CondCode cond;
            Addr64        target;
            Int           regparms; /* 0 .. 6 */
         } Call;
         /* Pseudo-insn.  Goto dst, on given condition (which could be
            Acc_ALWAYS). */
         struct {
            IRJumpKind    jk;
            AMD64CondCode cond;
            AMD64RI*      dst;
         } Goto;
         /* Mov src to dst on the given condition, which may not
            be the bogus Acc_ALWAYS. */
         struct {
            AMD64CondCode cond;
            AMD64RM*      src;
            HReg          dst;
         } CMov64;
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
//..          /* fstsw %ax */
//..          struct {
//..             /* no fields */
//..          }
//..          FpStSW_AX;
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
//.. 
//..          /* Simplistic SSE[123] */
//..          struct {
//..             UShort  con;
//..             HReg    dst;
//..          } SseConst;
         struct {
            Bool        isLoad;
            UChar       sz; /* 4, 8 or 16 only */
            HReg        reg;
            AMD64AMode* addr;
         } SseLdSt;
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

      } Ain;
   }
   AMD64Instr;

extern AMD64Instr* AMD64Instr_Imm64      ( ULong imm64, HReg dst );
extern AMD64Instr* AMD64Instr_Alu64R     ( AMD64AluOp, AMD64RMI*, HReg );
extern AMD64Instr* AMD64Instr_Alu64M     ( AMD64AluOp, AMD64RI*,  AMD64AMode* );
extern AMD64Instr* AMD64Instr_Unary64    ( AMD64UnaryOp op, HReg dst );
extern AMD64Instr* AMD64Instr_Lea64      ( AMD64AMode* am, HReg dst );
extern AMD64Instr* AMD64Instr_Sh64       ( AMD64ShiftOp, UInt, HReg );
extern AMD64Instr* AMD64Instr_Test64     ( UInt imm32, HReg dst );
extern AMD64Instr* AMD64Instr_MulL       ( Bool syned, AMD64RM* );
extern AMD64Instr* AMD64Instr_Div        ( Bool syned, Int sz, AMD64RM* );
//.. extern AMD64Instr* AMD64Instr_Sh3232    ( AMD64ShiftOp, UInt amt, HReg src, HReg dst );
extern AMD64Instr* AMD64Instr_Push       ( AMD64RMI* );
extern AMD64Instr* AMD64Instr_Call       ( AMD64CondCode, Addr64, Int );
extern AMD64Instr* AMD64Instr_Goto       ( IRJumpKind, AMD64CondCode cond, AMD64RI* dst );
extern AMD64Instr* AMD64Instr_CMov64     ( AMD64CondCode, AMD64RM* src, HReg dst );
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
//.. 
//.. extern AMD64Instr* AMD64Instr_FpUnary   ( AMD64FpOp op, HReg src, HReg dst );
//.. extern AMD64Instr* AMD64Instr_FpBinary  ( AMD64FpOp op, HReg srcL, HReg srcR, HReg dst );
//.. extern AMD64Instr* AMD64Instr_FpLdSt    ( Bool isLoad, UChar sz, HReg reg, AMD64AMode* );
//.. extern AMD64Instr* AMD64Instr_FpLdStI   ( Bool isLoad, UChar sz, HReg reg, AMD64AMode* );
//.. extern AMD64Instr* AMD64Instr_Fp64to32  ( HReg src, HReg dst );
//.. extern AMD64Instr* AMD64Instr_FpCMov    ( AMD64CondCode, HReg src, HReg dst );
extern AMD64Instr* AMD64Instr_LdMXCSR    ( AMD64AMode* );
//.. extern AMD64Instr* AMD64Instr_FpStSW_AX ( void );
extern AMD64Instr* AMD64Instr_SseUComIS  ( Int sz, HReg srcL, HReg srcR, HReg dst );
extern AMD64Instr* AMD64Instr_SseSI2SF   ( Int szS, Int szD, HReg src, HReg dst );
extern AMD64Instr* AMD64Instr_SseSF2SI   ( Int szS, Int szD, HReg src, HReg dst );
extern AMD64Instr* AMD64Instr_SseSDSS    ( Bool from64, HReg src, HReg dst );
//.. 
//.. extern AMD64Instr* AMD64Instr_SseConst  ( UShort con, HReg dst );
extern AMD64Instr* AMD64Instr_SseLdSt    ( Bool isLoad, Int sz, HReg, AMD64AMode* );
extern AMD64Instr* AMD64Instr_SseLdzLO   ( Int sz, HReg, AMD64AMode* );
extern AMD64Instr* AMD64Instr_Sse32Fx4   ( AMD64SseOp, HReg, HReg );
extern AMD64Instr* AMD64Instr_Sse32FLo   ( AMD64SseOp, HReg, HReg );
extern AMD64Instr* AMD64Instr_Sse64Fx2   ( AMD64SseOp, HReg, HReg );
extern AMD64Instr* AMD64Instr_Sse64FLo   ( AMD64SseOp, HReg, HReg );
extern AMD64Instr* AMD64Instr_SseReRg    ( AMD64SseOp, HReg, HReg );
extern AMD64Instr* AMD64Instr_SseCMov    ( AMD64CondCode, HReg src, HReg dst );
extern AMD64Instr* AMD64Instr_SseShuf    ( Int order, HReg src, HReg dst );


extern void ppAMD64Instr ( AMD64Instr*, Bool );

/* Some functions that insulate the register allocator from details
   of the underlying instruction set. */
extern void         getRegUsage_AMD64Instr ( HRegUsage*, AMD64Instr*, Bool );
extern void         mapRegs_AMD64Instr     ( HRegRemap*, AMD64Instr*, Bool );
extern Bool         isMove_AMD64Instr      ( AMD64Instr*, HReg*, HReg* );
extern Int          emit_AMD64Instr        ( UChar* buf, Int nbuf, AMD64Instr*, 
                                             Bool, void* dispatch );

extern void genSpill_AMD64  ( /*OUT*/HInstr** i1, /*OUT*/HInstr** i2,
                              HReg rreg, Int offset, Bool );
extern void genReload_AMD64 ( /*OUT*/HInstr** i1, /*OUT*/HInstr** i2,
                              HReg rreg, Int offset, Bool );

extern void         getAllocableRegs_AMD64 ( Int*, HReg** );
extern HInstrArray* iselSB_AMD64           ( IRSB*, VexArch,
                                                    VexArchInfo*,
                                                    VexAbiInfo* );

#endif /* ndef __VEX_HOST_AMD64_DEFS_H */

/*---------------------------------------------------------------*/
/*--- end                                   host_amd64_defs.h ---*/
/*---------------------------------------------------------------*/
