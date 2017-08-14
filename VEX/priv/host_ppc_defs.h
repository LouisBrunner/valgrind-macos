
/*---------------------------------------------------------------*/
/*--- begin                                   host_ppc_defs.h ---*/
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

#ifndef __VEX_HOST_PPC_DEFS_H
#define __VEX_HOST_PPC_DEFS_H

/* Num registers used for function calls */
#define PPC_N_REGPARMS 8


/* --------- Registers. --------- */

/* The usual HReg abstraction.  There are 32 real int regs,
   32 real float regs, and 32 real vector regs. 
*/

extern void ppHRegPPC ( HReg );

extern HReg hregPPC_GPR0  ( Bool mode64 ); // scratch reg / zero reg
extern HReg hregPPC_GPR1  ( Bool mode64 ); // Stack Frame Pointer
extern HReg hregPPC_GPR2  ( Bool mode64 ); // not used: TOC pointer
extern HReg hregPPC_GPR3  ( Bool mode64 );
extern HReg hregPPC_GPR4  ( Bool mode64 );
extern HReg hregPPC_GPR5  ( Bool mode64 );
extern HReg hregPPC_GPR6  ( Bool mode64 );
extern HReg hregPPC_GPR7  ( Bool mode64 );
extern HReg hregPPC_GPR8  ( Bool mode64 );
extern HReg hregPPC_GPR9  ( Bool mode64 );
extern HReg hregPPC_GPR10 ( Bool mode64 );
extern HReg hregPPC_GPR11 ( Bool mode64 );
extern HReg hregPPC_GPR12 ( Bool mode64 );
extern HReg hregPPC_GPR13 ( Bool mode64 );
extern HReg hregPPC_GPR14 ( Bool mode64 );
extern HReg hregPPC_GPR15 ( Bool mode64 );
extern HReg hregPPC_GPR16 ( Bool mode64 );
extern HReg hregPPC_GPR17 ( Bool mode64 );
extern HReg hregPPC_GPR18 ( Bool mode64 );
extern HReg hregPPC_GPR19 ( Bool mode64 );
extern HReg hregPPC_GPR20 ( Bool mode64 );
extern HReg hregPPC_GPR21 ( Bool mode64 );
extern HReg hregPPC_GPR22 ( Bool mode64 );
extern HReg hregPPC_GPR23 ( Bool mode64 );
extern HReg hregPPC_GPR24 ( Bool mode64 );
extern HReg hregPPC_GPR25 ( Bool mode64 );
extern HReg hregPPC_GPR26 ( Bool mode64 );
extern HReg hregPPC_GPR27 ( Bool mode64 );
extern HReg hregPPC_GPR28 ( Bool mode64 );
extern HReg hregPPC_GPR29 ( Bool mode64 ); // reserved for dispatcher
extern HReg hregPPC_GPR30 ( Bool mode64 ); // used as VMX spill temp
extern HReg hregPPC_GPR31 ( Bool mode64 ); // GuestStatePtr (callee-saved)

extern HReg hregPPC_FPR0  ( void );
extern HReg hregPPC_FPR1  ( void );
extern HReg hregPPC_FPR2  ( void );
extern HReg hregPPC_FPR3  ( void );
extern HReg hregPPC_FPR4  ( void );
extern HReg hregPPC_FPR5  ( void );
extern HReg hregPPC_FPR6  ( void );
extern HReg hregPPC_FPR7  ( void );
extern HReg hregPPC_FPR8  ( void );
extern HReg hregPPC_FPR9  ( void );
extern HReg hregPPC_FPR10 ( void );
extern HReg hregPPC_FPR11 ( void );
extern HReg hregPPC_FPR12 ( void );
extern HReg hregPPC_FPR13 ( void );
extern HReg hregPPC_FPR14 ( void );
extern HReg hregPPC_FPR15 ( void );
extern HReg hregPPC_FPR16 ( void );
extern HReg hregPPC_FPR17 ( void );
extern HReg hregPPC_FPR18 ( void );
extern HReg hregPPC_FPR19 ( void );
extern HReg hregPPC_FPR20 ( void );
extern HReg hregPPC_FPR21 ( void );
extern HReg hregPPC_FPR22 ( void );
extern HReg hregPPC_FPR23 ( void );
extern HReg hregPPC_FPR24 ( void );
extern HReg hregPPC_FPR25 ( void );
extern HReg hregPPC_FPR26 ( void );
extern HReg hregPPC_FPR27 ( void );
extern HReg hregPPC_FPR28 ( void );
extern HReg hregPPC_FPR29 ( void );
extern HReg hregPPC_FPR30 ( void );
extern HReg hregPPC_FPR31 ( void );

extern HReg hregPPC_VR0  ( void );
extern HReg hregPPC_VR1  ( void );
extern HReg hregPPC_VR2  ( void );
extern HReg hregPPC_VR3  ( void );
extern HReg hregPPC_VR4  ( void );
extern HReg hregPPC_VR5  ( void );
extern HReg hregPPC_VR6  ( void );
extern HReg hregPPC_VR7  ( void );
extern HReg hregPPC_VR8  ( void );
extern HReg hregPPC_VR9  ( void );
extern HReg hregPPC_VR10 ( void );
extern HReg hregPPC_VR11 ( void );
extern HReg hregPPC_VR12 ( void );
extern HReg hregPPC_VR13 ( void );
extern HReg hregPPC_VR14 ( void );
extern HReg hregPPC_VR15 ( void );
extern HReg hregPPC_VR16 ( void );
extern HReg hregPPC_VR17 ( void );
extern HReg hregPPC_VR18 ( void );
extern HReg hregPPC_VR19 ( void );
extern HReg hregPPC_VR20 ( void );
extern HReg hregPPC_VR21 ( void );
extern HReg hregPPC_VR22 ( void );
extern HReg hregPPC_VR23 ( void );
extern HReg hregPPC_VR24 ( void );
extern HReg hregPPC_VR25 ( void );
extern HReg hregPPC_VR26 ( void );
extern HReg hregPPC_VR27 ( void );
extern HReg hregPPC_VR28 ( void );
extern HReg hregPPC_VR29 ( void );
extern HReg hregPPC_VR30 ( void );
extern HReg hregPPC_VR31 ( void );

#define StackFramePtr(_mode64) hregPPC_GPR1(_mode64)
#define GuestStatePtr(_mode64) hregPPC_GPR31(_mode64)



/* --------- Condition codes --------- */

/* This gives names from bitfields in CR; hence it names BI numbers */
/* Using IBM/hardware indexing convention */
typedef
   enum {
      // CR7, which we use for integer compares
      Pcf_7LT  = 28,  /* neg  | lt          */
      Pcf_7GT  = 29,  /* pos  | gt          */
      Pcf_7EQ  = 30,  /* zero | equal       */
      Pcf_7SO  = 31   /* summary overflow   */
   }
   PPCCondFlag;

typedef
   enum {   /* Maps bc bitfield BO */
      Pct_FALSE  = 0x4,
      Pct_TRUE   = 0xC,
      Pct_ALWAYS = 0x14
   }
   PPCCondTest;

typedef
   struct {
      PPCCondFlag flag;
      PPCCondTest test;
   }
   PPCCondCode;

extern HChar* showPPCCondCode ( PPCCondCode );

/* constructor */
extern PPCCondCode mk_PPCCondCode ( PPCCondTest, PPCCondFlag );

/* false->true, true->false */
extern PPCCondTest invertCondTest ( PPCCondTest );




/* --------- Memory address expressions (amodes). --------- */

typedef
   enum {
     Pam_IR=1,      /* Immediate (signed 16-bit) + Reg */
     Pam_RR=2       /* Reg1 + Reg2     */
   }
   PPCAModeTag;

typedef
   struct {
      PPCAModeTag tag;
      union {
         struct {
            HReg base;
            Int  index;
         } IR;
         struct {
            HReg base;
            HReg index;
         } RR;
      } Pam;
   }
   PPCAMode;

extern PPCAMode* PPCAMode_IR ( Int,  HReg );
extern PPCAMode* PPCAMode_RR ( HReg, HReg );

extern PPCAMode* dopyPPCAMode ( PPCAMode* );

extern void ppPPCAMode ( PPCAMode* );


/* --------- Operand, which can be a reg or a u16/s16. --------- */
/* ("RH" == "Register or Halfword immediate") */
typedef 
   enum {
      Prh_Imm=3,
      Prh_Reg=4
   }
   PPCRHTag;

typedef
   struct {
      PPCRHTag tag;
      union {
         struct {
            Bool   syned;
            UShort imm16;
         } Imm;
         struct {
            HReg reg;
         } Reg;
      }
      Prh;
   }
   PPCRH;

extern PPCRH* PPCRH_Imm ( Bool, UShort );
extern PPCRH* PPCRH_Reg ( HReg );

extern void ppPPCRH ( PPCRH* );


/* --------- Operand, which can be a reg or a u32/64. --------- */

typedef
   enum {
      Pri_Imm=5,
      Pri_Reg=6
   } 
   PPCRITag;

typedef
   struct {
      PPCRITag tag;
      union {
         ULong Imm;
         HReg  Reg;
      }
      Pri;
   }
   PPCRI;

extern PPCRI* PPCRI_Imm ( ULong );
extern PPCRI* PPCRI_Reg( HReg );

extern void ppPPCRI ( PPCRI* );


/* --------- Operand, which can be a vector reg or a s6. --------- */
/* ("VI" == "Vector Register or Immediate") */
typedef
   enum {
      Pvi_Imm=7,
      Pvi_Reg=8
   } 
   PPCVI5sTag;

typedef
   struct {
      PPCVI5sTag tag;
      union {
         Char Imm5s;
         HReg Reg;
      }
      Pvi;
   }
   PPCVI5s;

extern PPCVI5s* PPCVI5s_Imm ( Char );
extern PPCVI5s* PPCVI5s_Reg ( HReg );

extern void ppPPCVI5s ( PPCVI5s* );


/* --------- Instructions. --------- */

/* --------- */
typedef
   enum {
      Pun_NEG,
      Pun_NOT,
      Pun_CLZ32,
      Pun_CLZ64,
      Pun_EXTSW
   }
   PPCUnaryOp;

extern HChar* showPPCUnaryOp ( PPCUnaryOp );


/* --------- */
typedef 
   enum {
      Palu_INVALID,
      Palu_ADD, Palu_SUB,
      Palu_AND, Palu_OR, Palu_XOR,
   }
   PPCAluOp;

extern 
HChar* showPPCAluOp ( PPCAluOp, 
                      Bool /* is the 2nd operand an immediate? */);


/* --------- */
typedef 
   enum {
      Pshft_INVALID,
      Pshft_SHL, Pshft_SHR, Pshft_SAR, 
   }
   PPCShftOp;

extern 
HChar* showPPCShftOp ( PPCShftOp, 
                       Bool /* is the 2nd operand an immediate? */,
                       Bool /* is this a 32bit or 64bit op? */ );


/* --------- */
typedef
   enum {
      Pfp_INVALID,

      /* Ternary */
      Pfp_MADDD, Pfp_MSUBD, 
      Pfp_MADDS, Pfp_MSUBS,

      /* Binary */
      Pfp_ADDD, Pfp_SUBD, Pfp_MULD, Pfp_DIVD, 
      Pfp_ADDS, Pfp_SUBS, Pfp_MULS, Pfp_DIVS, 

      /* Unary */
      Pfp_SQRT, Pfp_ABS, Pfp_NEG, Pfp_MOV, Pfp_RES, Pfp_RSQRTE,
      Pfp_FRIN, Pfp_FRIM, Pfp_FRIP, Pfp_FRIZ
   }
   PPCFpOp;

extern HChar* showPPCFpOp ( PPCFpOp );


/* --------- */
typedef
   enum {
      Pav_INVALID,

      /* Integer Unary */
      Pav_MOV,                             /* Mov */
      Pav_NOT,                             /* Bitwise */
      Pav_UNPCKH8S,  Pav_UNPCKH16S,        /* Unpack */
      Pav_UNPCKL8S,  Pav_UNPCKL16S,
      Pav_UNPCKHPIX, Pav_UNPCKLPIX,

      /* Integer Binary */
      Pav_AND, Pav_OR, Pav_XOR,            /* Bitwise */
      Pav_ADDU, Pav_QADDU, Pav_QADDS,
      Pav_SUBU, Pav_QSUBU, Pav_QSUBS,
      Pav_OMULU, Pav_OMULS, Pav_EMULU, Pav_EMULS,
      Pav_AVGU, Pav_AVGS,
      Pav_MAXU, Pav_MAXS,
      Pav_MINU, Pav_MINS,

      /* Compare (always affects CR field 6) */
      Pav_CMPEQU, Pav_CMPGTU, Pav_CMPGTS,

      /* Shift */
      Pav_SHL, Pav_SHR, Pav_SAR, Pav_ROTL,

      /* Pack */
      Pav_PACKUU, Pav_QPACKUU, Pav_QPACKSU, Pav_QPACKSS,
      Pav_PACKPXL,

      /* Merge */
      Pav_MRGHI, Pav_MRGLO,
   }
   PPCAvOp;

extern HChar* showPPCAvOp ( PPCAvOp );


/* --------- */
typedef
   enum {
      Pavfp_INVALID,

      /* Floating point binary */
      Pavfp_ADDF, Pavfp_SUBF, Pavfp_MULF,
      Pavfp_MAXF, Pavfp_MINF,
      Pavfp_CMPEQF, Pavfp_CMPGTF, Pavfp_CMPGEF,

      /* Floating point unary */
      Pavfp_RCPF, Pavfp_RSQRTF,
      Pavfp_CVTU2F, Pavfp_CVTS2F, Pavfp_QCVTF2U, Pavfp_QCVTF2S,
      Pavfp_ROUNDM, Pavfp_ROUNDP, Pavfp_ROUNDN, Pavfp_ROUNDZ,
   }
   PPCAvFpOp;

extern HChar* showPPCAvFpOp ( PPCAvFpOp );


/* --------- */
typedef
   enum {
      Pin_LI,         /* load word (32/64-bit) immediate (fake insn) */
      Pin_Alu,        /* word add/sub/and/or/xor */
      Pin_Shft,       /* word shl/shr/sar */
      Pin_AddSubC,    /* add/sub with read/write carry */
      Pin_Cmp,        /* word compare */
      Pin_Unary,      /* not, neg, clz */
      Pin_MulL,       /* widening multiply */
      Pin_Div,        /* div */
      Pin_Call,       /* call to address in register */
      Pin_Goto,       /* conditional/unconditional jmp to dst */
      Pin_CMov,       /* conditional move */
      Pin_Load,       /* zero-extending load a 8|16|32|64 bit value from mem */
      Pin_LoadL,      /* load-linked (lwarx/ldarx) 32|64 bit value from mem */
      Pin_Store,      /* store a 8|16|32|64 bit value to mem */
      Pin_StoreC,     /* store-conditional (stwcx./stdcx.) 32|64 bit val */
      Pin_Set,        /* convert condition code to value 0 or 1 */
      Pin_MfCR,       /* move from condition register to GPR */
      Pin_MFence,     /* mem fence */

      Pin_FpUnary,    /* FP unary op */
      Pin_FpBinary,   /* FP binary op */
      Pin_FpMulAcc,   /* FP multipy-accumulate style op */
      Pin_FpLdSt,     /* FP load/store */
      Pin_FpSTFIW,    /* stfiwx */
      Pin_FpRSP,      /* FP round IEEE754 double to IEEE754 single */
      Pin_FpCftI,     /* fcfid/fctid/fctiw */
      Pin_FpCMov,     /* FP floating point conditional move */
      Pin_FpLdFPSCR,  /* mtfsf */
      Pin_FpCmp,      /* FP compare, generating value into int reg */

      Pin_RdWrLR,     /* Read/Write Link Register */

      Pin_AvLdSt,     /* AV load/store (kludging for AMode_IR) */
      Pin_AvUnary,    /* AV unary general reg=>reg */

      Pin_AvBinary,   /* AV binary general reg,reg=>reg */
      Pin_AvBin8x16,  /* AV binary, 8x4 */
      Pin_AvBin16x8,  /* AV binary, 16x4 */
      Pin_AvBin32x4,  /* AV binary, 32x4 */

      Pin_AvBin32Fx4, /* AV FP binary, 32Fx4 */
      Pin_AvUn32Fx4,  /* AV FP unary,  32Fx4 */

      Pin_AvPerm,     /* AV permute (shuffle) */
      Pin_AvSel,      /* AV select */
      Pin_AvShlDbl,   /* AV shift-left double by imm */
      Pin_AvSplat,    /* One elem repeated throughout dst */
      Pin_AvLdVSCR,   /* mtvscr */
      Pin_AvCMov      /* AV conditional move */
   }
   PPCInstrTag;

/* Destinations are on the LEFT (first operand) */

typedef
   struct {
      PPCInstrTag tag;
      union {
         /* Get a 32/64-bit literal into a register.
            May turn into a number of real insns. */
         struct {
            HReg dst;
            ULong imm64;
         } LI;
         /* Integer add/sub/and/or/xor.  Limitations:
            - For add, the immediate, if it exists, is a signed 16.
            - For sub, the immediate, if it exists, is a signed 16
              which may not be -32768, since no such instruction 
              exists, and so we have to emit addi with +32768, but 
              that is not possible.
            - For and/or/xor,  the immediate, if it exists, 
              is an unsigned 16.
         */
         struct {
            PPCAluOp op;
            HReg     dst;
            HReg     srcL;
            PPCRH*   srcR;
         } Alu;
         /* Integer shl/shr/sar.
            Limitations: the immediate, if it exists,
            is a signed 5-bit value between 1 and 31 inclusive.
         */
         struct {
            PPCShftOp op;
            Bool      sz32;   /* mode64 has both 32 and 64bit shft */
            HReg      dst;
            HReg      srcL;
            PPCRH*    srcR;
         } Shft;
         /*  */
         struct {
            Bool isAdd;  /* else sub */
            Bool setC;   /* else read carry */
            HReg dst;
            HReg srcL;
            HReg srcR;
         } AddSubC;
         /* If signed, the immediate, if it exists, is a signed 16,
            else it is an unsigned 16. */
         struct {
            Bool   syned;
            Bool   sz32;    /* mode64 has both 32 and 64bit cmp */
            UInt   crfD;
            HReg   srcL;
            PPCRH* srcR;
         } Cmp;
         /* Not, Neg, Clz32/64, Extsw */
         struct {
            PPCUnaryOp op;
            HReg       dst;
            HReg       src;
         } Unary;
         struct {
            Bool syned;  /* meaningless if hi32==False */
            Bool hi;     /* False=>low, True=>high */
            Bool sz32;   /* mode64 has both 32 & 64bit mull */
            HReg dst;
            HReg srcL;
            HReg srcR;
         } MulL;
         /* ppc32 div/divu instruction. */
         struct {
            Bool syned;
            Bool sz32;   /* mode64 has both 32 & 64bit div */
            HReg dst;
            HReg srcL;
            HReg srcR;
         } Div;
         /* Pseudo-insn.  Call target (an absolute address), on given
            condition (which could be Pct_ALWAYS).  argiregs indicates
            which of r3 .. r10 carries argument values for this call,
            using a bit mask (1<<N is set if rN holds an arg, for N in
            3 .. 10 inclusive). */
         struct {
            PPCCondCode cond;
            Addr64      target;
            UInt        argiregs;
         } Call;
         /* Pseudo-insn.  Goto dst, on given condition (which could be
            Pct_ALWAYS). */
         struct {
            IRJumpKind  jk;
            PPCCondCode cond;
            PPCRI*      dst;
         } Goto;
         /* Mov src to dst on the given condition, which may not
            be the bogus Pct_ALWAYS. */
         struct {
            PPCCondCode cond;
            HReg        dst;
            PPCRI*      src;
         } CMov;
         /* Zero extending loads.  Dst size is host word size */
         struct {
            UChar     sz; /* 1|2|4|8 */
            HReg      dst;
            PPCAMode* src;
         } Load;
         /* Load-and-reserve (lwarx, ldarx) */
         struct {
            UChar sz; /* 4|8 */
            HReg  dst;
            HReg  src;
         } LoadL;
         /* 64/32/16/8 bit stores */
         struct {
            UChar     sz; /* 1|2|4|8 */
            PPCAMode* dst;
            HReg      src;
         } Store;
         /* Store-conditional (stwcx., stdcx.) */
         struct {
            UChar sz; /* 4|8 */
            HReg  dst;
            HReg  src;
         } StoreC;
         /* Convert a ppc condition code to value 0 or 1. */
         struct {
            PPCCondCode cond;
            HReg        dst;
         } Set;
         /* Move the entire CR to a GPR */
         struct {
            HReg dst;
         } MfCR;
         /* Mem fence.  In short, an insn which flushes all preceding
            loads and stores as much as possible before continuing.
            On PPC we emit a "sync". */
         struct {
         } MFence;

         /* PPC Floating point */
         struct {
            PPCFpOp op;
            HReg    dst;
            HReg    src;
         } FpUnary;
         struct {
            PPCFpOp op;
            HReg    dst;
            HReg    srcL;
            HReg    srcR;
         } FpBinary;
         struct {
            PPCFpOp op;
            HReg    dst;
            HReg    srcML;
            HReg    srcMR;
            HReg    srcAcc;
         } FpMulAcc;
         struct {
            Bool      isLoad;
            UChar     sz; /* only 4 (IEEE single) or 8 (IEEE double) */
            HReg      reg;
            PPCAMode* addr;
         } FpLdSt;
         struct {
            HReg addr; /* int reg */
            HReg data; /* float reg */
         } FpSTFIW;
         /* Round 64-bit FP value to 32-bit FP value in an FP reg. */
         struct {
            HReg src;
            HReg dst;
         } FpRSP;
         /* fcfid/fctid/fctiw.  Note there's no fcfiw so fromI==True
            && int32==True is not allowed. */
         struct {
            Bool fromI; /* False==F->I, True==I->F */
            Bool int32; /* True== I is 32, False==I is 64 */
            HReg src;
            HReg dst;
         } FpCftI;
         /* FP mov src to dst on the given condition. */
         struct {
            PPCCondCode cond;
            HReg        dst;
            HReg        src;
         } FpCMov;
         /* Load FP Status & Control Register */
         struct {
            HReg src;
         } FpLdFPSCR;
         /* Do a compare, generating result into an int register. */
         struct {
            UChar crfD;
            HReg  dst;
            HReg  srcL;
            HReg  srcR;
         } FpCmp;

         /* Read/Write Link Register */
         struct {
            Bool wrLR;
            HReg gpr;
         } RdWrLR;

         /* Simplistic AltiVec */
         struct {
            Bool      isLoad;
            UChar     sz;      /* 8|16|32|128 */
            HReg      reg;
            PPCAMode* addr;
         } AvLdSt;
         struct {
            PPCAvOp op;
            HReg    dst;
            HReg    src;
         } AvUnary;
         struct {
            PPCAvOp op;
            HReg    dst;
            HReg    srcL;
            HReg    srcR;
         } AvBinary;
         struct {
            PPCAvOp op;
            HReg    dst;
            HReg    srcL;
            HReg    srcR;
         } AvBin8x16;
         struct {
            PPCAvOp op;
            HReg    dst;
            HReg    srcL;
            HReg    srcR;
         } AvBin16x8;
         struct {
            PPCAvOp op;
            HReg    dst;
            HReg    srcL;
            HReg    srcR;
         } AvBin32x4;
         struct {
            PPCAvFpOp op;
            HReg      dst;
            HReg      srcL;
            HReg      srcR;
         } AvBin32Fx4;
         struct {
            PPCAvFpOp op;
            HReg      dst;
            HReg      src;
         } AvUn32Fx4;
         /* Perm,Sel,SlDbl,Splat are all weird AV permutations */
         struct {
            HReg dst;
            HReg srcL;
            HReg srcR;
            HReg ctl;
         } AvPerm;
         struct {
            HReg dst;
            HReg srcL;
            HReg srcR;
            HReg ctl;
         } AvSel;
         struct {
            UChar shift;
            HReg  dst;
            HReg  srcL;
            HReg  srcR;
         } AvShlDbl;
         struct {
            UChar    sz;   /* 8,16,32 */
            HReg     dst;
            PPCVI5s* src; 
         } AvSplat;
         /* Mov src to dst on the given condition, which may not
            be the bogus Xcc_ALWAYS. */
         struct {
            PPCCondCode cond;
            HReg        dst;
            HReg        src;
         } AvCMov;
         /* Load AltiVec Status & Control Register */
         struct {
            HReg src;
         } AvLdVSCR;
       } Pin;
   }
   PPCInstr;


extern PPCInstr* PPCInstr_LI         ( HReg, ULong, Bool );
extern PPCInstr* PPCInstr_Alu        ( PPCAluOp, HReg, HReg, PPCRH* );
extern PPCInstr* PPCInstr_Shft       ( PPCShftOp, Bool sz32, HReg, HReg, PPCRH* );
extern PPCInstr* PPCInstr_AddSubC    ( Bool, Bool, HReg, HReg, HReg );
extern PPCInstr* PPCInstr_Cmp        ( Bool, Bool, UInt, HReg, PPCRH* );
extern PPCInstr* PPCInstr_Unary      ( PPCUnaryOp op, HReg dst, HReg src );
extern PPCInstr* PPCInstr_MulL       ( Bool syned, Bool hi32, Bool sz32, HReg, HReg, HReg );
extern PPCInstr* PPCInstr_Div        ( Bool syned, Bool sz32, HReg dst, HReg srcL, HReg srcR );
extern PPCInstr* PPCInstr_Call       ( PPCCondCode, Addr64, UInt );
extern PPCInstr* PPCInstr_Goto       ( IRJumpKind, PPCCondCode cond, PPCRI* dst );
extern PPCInstr* PPCInstr_CMov       ( PPCCondCode, HReg dst, PPCRI* src );
extern PPCInstr* PPCInstr_Load       ( UChar sz,
                                       HReg dst, PPCAMode* src, Bool mode64 );
extern PPCInstr* PPCInstr_LoadL      ( UChar sz,
                                       HReg dst, HReg src, Bool mode64 );
extern PPCInstr* PPCInstr_Store      ( UChar sz, PPCAMode* dst,
                                       HReg src, Bool mode64 );
extern PPCInstr* PPCInstr_StoreC     ( UChar sz, HReg dst, HReg src,
                                       Bool mode64 );
extern PPCInstr* PPCInstr_Set        ( PPCCondCode cond, HReg dst );
extern PPCInstr* PPCInstr_MfCR       ( HReg dst );
extern PPCInstr* PPCInstr_MFence     ( void );

extern PPCInstr* PPCInstr_FpUnary    ( PPCFpOp op, HReg dst, HReg src );
extern PPCInstr* PPCInstr_FpBinary   ( PPCFpOp op, HReg dst, HReg srcL, HReg srcR );
extern PPCInstr* PPCInstr_FpMulAcc   ( PPCFpOp op, HReg dst, HReg srcML, 
                                                   HReg srcMR, HReg srcAcc );
extern PPCInstr* PPCInstr_FpLdSt     ( Bool isLoad, UChar sz, HReg, PPCAMode* );
extern PPCInstr* PPCInstr_FpSTFIW    ( HReg addr, HReg data );
extern PPCInstr* PPCInstr_FpRSP      ( HReg dst, HReg src );
extern PPCInstr* PPCInstr_FpCftI     ( Bool fromI, Bool int32, 
                                       HReg dst, HReg src );
extern PPCInstr* PPCInstr_FpCMov     ( PPCCondCode, HReg dst, HReg src );
extern PPCInstr* PPCInstr_FpLdFPSCR  ( HReg src );
extern PPCInstr* PPCInstr_FpCmp      ( HReg dst, HReg srcL, HReg srcR );

extern PPCInstr* PPCInstr_RdWrLR     ( Bool wrLR, HReg gpr );

extern PPCInstr* PPCInstr_AvLdSt     ( Bool isLoad, UChar sz, HReg, PPCAMode* );
extern PPCInstr* PPCInstr_AvUnary    ( PPCAvOp op, HReg dst, HReg src );
extern PPCInstr* PPCInstr_AvBinary   ( PPCAvOp op, HReg dst, HReg srcL, HReg srcR );
extern PPCInstr* PPCInstr_AvBin8x16  ( PPCAvOp op, HReg dst, HReg srcL, HReg srcR );
extern PPCInstr* PPCInstr_AvBin16x8  ( PPCAvOp op, HReg dst, HReg srcL, HReg srcR );
extern PPCInstr* PPCInstr_AvBin32x4  ( PPCAvOp op, HReg dst, HReg srcL, HReg srcR );
extern PPCInstr* PPCInstr_AvBin32Fx4 ( PPCAvOp op, HReg dst, HReg srcL, HReg srcR );
extern PPCInstr* PPCInstr_AvUn32Fx4  ( PPCAvOp op, HReg dst, HReg src );
extern PPCInstr* PPCInstr_AvPerm     ( HReg dst, HReg srcL, HReg srcR, HReg ctl );
extern PPCInstr* PPCInstr_AvSel      ( HReg ctl, HReg dst, HReg srcL, HReg srcR );
extern PPCInstr* PPCInstr_AvShlDbl   ( UChar shift, HReg dst, HReg srcL, HReg srcR );
extern PPCInstr* PPCInstr_AvSplat    ( UChar sz, HReg dst, PPCVI5s* src );
extern PPCInstr* PPCInstr_AvCMov     ( PPCCondCode, HReg dst, HReg src );
extern PPCInstr* PPCInstr_AvLdVSCR   ( HReg src );

extern void ppPPCInstr ( PPCInstr*, Bool mode64 );

/* Some functions that insulate the register allocator from details
   of the underlying instruction set. */
extern void         getRegUsage_PPCInstr ( HRegUsage*, PPCInstr*, Bool mode64 );
extern void         mapRegs_PPCInstr     ( HRegRemap*, PPCInstr* , Bool mode64);
extern Bool         isMove_PPCInstr      ( PPCInstr*, HReg*, HReg* );
extern Int          emit_PPCInstr        ( UChar* buf, Int nbuf, PPCInstr*, 
                                           Bool mode64, void* dispatch );

extern void genSpill_PPC  ( /*OUT*/HInstr** i1, /*OUT*/HInstr** i2,
                            HReg rreg, Int offsetB, Bool mode64 );
extern void genReload_PPC ( /*OUT*/HInstr** i1, /*OUT*/HInstr** i2,
                            HReg rreg, Int offsetB, Bool mode64 );

extern void         getAllocableRegs_PPC ( Int*, HReg**, Bool mode64 );
extern HInstrArray* iselSB_PPC           ( IRSB*, VexArch,
                                                  VexArchInfo*,
                                                  VexAbiInfo* );

#endif /* ndef __VEX_HOST_PPC_DEFS_H */

/*---------------------------------------------------------------*/
/*--- end                                     host_ppc_defs.h ---*/
/*---------------------------------------------------------------*/
