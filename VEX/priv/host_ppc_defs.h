
/*---------------------------------------------------------------*/
/*--- begin                                   host_ppc_defs.h ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2004-2015 OpenWorks LLP
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

#include "libvex_basictypes.h"
#include "libvex.h"                      // VexArch
#include "host_generic_regs.h"           // HReg


/* --------- Registers. --------- */

#define ST_IN static inline

#define GPR(_mode64, _enc, _ix64, _ix32) \
  mkHReg(False,  (_mode64) ? HRcInt64 : HRcInt32, \
         (_enc), (_mode64) ? (_ix64) : (_ix32))

#define FPR(_mode64, _enc, _ix64, _ix32) \
  mkHReg(False,  HRcFlt64, \
         (_enc), (_mode64) ? (_ix64) : (_ix32))

#define VR(_mode64, _enc, _ix64, _ix32) \
  mkHReg(False,  HRcVec128, \
         (_enc), (_mode64) ? (_ix64) : (_ix32))

ST_IN HReg hregPPC_GPR3  ( Bool mode64 ) { return GPR(mode64,  3,   0,  0); }
ST_IN HReg hregPPC_GPR4  ( Bool mode64 ) { return GPR(mode64,  4,   1,  1); }
ST_IN HReg hregPPC_GPR5  ( Bool mode64 ) { return GPR(mode64,  5,   2,  2); }
ST_IN HReg hregPPC_GPR6  ( Bool mode64 ) { return GPR(mode64,  6,   3,  3); }
ST_IN HReg hregPPC_GPR7  ( Bool mode64 ) { return GPR(mode64,  7,   4,  4); }
ST_IN HReg hregPPC_GPR8  ( Bool mode64 ) { return GPR(mode64,  8,   5,  5); }
ST_IN HReg hregPPC_GPR9  ( Bool mode64 ) { return GPR(mode64,  9,   6,  6); }
ST_IN HReg hregPPC_GPR10 ( Bool mode64 ) { return GPR(mode64, 10,   7,  7); }

// r11 and r12 are only allocatable in 32-bit mode.  Hence the 64-bit
// index numbering doesn't advance for these two.
ST_IN HReg hregPPC_GPR11 ( Bool mode64 ) { return GPR(mode64, 11,   0,  8); }
ST_IN HReg hregPPC_GPR12 ( Bool mode64 ) { return GPR(mode64, 12,   0,  9); }

ST_IN HReg hregPPC_GPR14 ( Bool mode64 ) { return GPR(mode64, 14,   8, 10); }
ST_IN HReg hregPPC_GPR15 ( Bool mode64 ) { return GPR(mode64, 15,   9, 11); }
ST_IN HReg hregPPC_GPR16 ( Bool mode64 ) { return GPR(mode64, 16,  10, 12); }
ST_IN HReg hregPPC_GPR17 ( Bool mode64 ) { return GPR(mode64, 17,  11, 13); }
ST_IN HReg hregPPC_GPR18 ( Bool mode64 ) { return GPR(mode64, 18,  12, 14); }
ST_IN HReg hregPPC_GPR19 ( Bool mode64 ) { return GPR(mode64, 19,  13, 15); }
ST_IN HReg hregPPC_GPR20 ( Bool mode64 ) { return GPR(mode64, 20,  14, 16); }
ST_IN HReg hregPPC_GPR21 ( Bool mode64 ) { return GPR(mode64, 21,  15, 17); }
ST_IN HReg hregPPC_GPR22 ( Bool mode64 ) { return GPR(mode64, 22,  16, 18); }
ST_IN HReg hregPPC_GPR23 ( Bool mode64 ) { return GPR(mode64, 23,  17, 19); }
ST_IN HReg hregPPC_GPR24 ( Bool mode64 ) { return GPR(mode64, 24,  18, 20); }
ST_IN HReg hregPPC_GPR25 ( Bool mode64 ) { return GPR(mode64, 25,  19, 21); }
ST_IN HReg hregPPC_GPR26 ( Bool mode64 ) { return GPR(mode64, 26,  20, 22); }
ST_IN HReg hregPPC_GPR27 ( Bool mode64 ) { return GPR(mode64, 27,  21, 23); }
ST_IN HReg hregPPC_GPR28 ( Bool mode64 ) { return GPR(mode64, 28,  22, 24); }

ST_IN HReg hregPPC_FPR14 ( Bool mode64 ) { return FPR(mode64, 14,  23, 25); }
ST_IN HReg hregPPC_FPR15 ( Bool mode64 ) { return FPR(mode64, 15,  24, 26); }
ST_IN HReg hregPPC_FPR16 ( Bool mode64 ) { return FPR(mode64, 16,  25, 27); }
ST_IN HReg hregPPC_FPR17 ( Bool mode64 ) { return FPR(mode64, 17,  26, 28); }
ST_IN HReg hregPPC_FPR18 ( Bool mode64 ) { return FPR(mode64, 18,  27, 29); }
ST_IN HReg hregPPC_FPR19 ( Bool mode64 ) { return FPR(mode64, 19,  28, 30); }
ST_IN HReg hregPPC_FPR20 ( Bool mode64 ) { return FPR(mode64, 20,  29, 31); }
ST_IN HReg hregPPC_FPR21 ( Bool mode64 ) { return FPR(mode64, 21,  30, 32); }

ST_IN HReg hregPPC_VR20  ( Bool mode64 ) { return VR (mode64, 20,  31, 33); }
ST_IN HReg hregPPC_VR21  ( Bool mode64 ) { return VR (mode64, 21,  32, 34); }
ST_IN HReg hregPPC_VR22  ( Bool mode64 ) { return VR (mode64, 22,  33, 35); }
ST_IN HReg hregPPC_VR23  ( Bool mode64 ) { return VR (mode64, 23,  34, 36); }
ST_IN HReg hregPPC_VR24  ( Bool mode64 ) { return VR (mode64, 24,  35, 37); }
ST_IN HReg hregPPC_VR25  ( Bool mode64 ) { return VR (mode64, 25,  36, 38); }
ST_IN HReg hregPPC_VR26  ( Bool mode64 ) { return VR (mode64, 26,  37, 39); }
ST_IN HReg hregPPC_VR27  ( Bool mode64 ) { return VR (mode64, 27,  38, 40); }

ST_IN HReg hregPPC_GPR1  ( Bool mode64 ) { return GPR(mode64,  1,  39, 41); }
ST_IN HReg hregPPC_GPR29 ( Bool mode64 ) { return GPR(mode64, 29,  40, 42); }
ST_IN HReg hregPPC_GPR30 ( Bool mode64 ) { return GPR(mode64, 30,  41, 43); }
ST_IN HReg hregPPC_GPR31 ( Bool mode64 ) { return GPR(mode64, 31,  42, 44); }
ST_IN HReg hregPPC_VR29  ( Bool mode64 ) { return VR (mode64, 29,  43, 45); }

#undef ST_IN
#undef GPR
#undef FPR
#undef VR

#define StackFramePtr(_mode64) hregPPC_GPR1(_mode64)
#define GuestStatePtr(_mode64) hregPPC_GPR31(_mode64)

/* Num registers used for function calls */
#define PPC_N_REGPARMS 8

extern void ppHRegPPC ( HReg );


/* --------- Condition codes --------- */

/* This gives names from bitfields in CR; hence it names BI numbers */
/* Using IBM/hardware indexing convention */
typedef
   enum {
      // CR7, which we use for integer compares
      Pcf_7LT  = 28,  /* neg  | lt          */
      Pcf_7GT  = 29,  /* pos  | gt          */
      Pcf_7EQ  = 30,  /* zero | equal       */
      Pcf_7SO  = 31,  /* summary overflow   */
      Pcf_NONE = 32   /* no condition; used with Pct_ALWAYS */
   }
   PPCCondFlag;

typedef
   enum {   /* Maps bc bitfield BO */
      Pct_FALSE  = 0x4, /* associated PPCCondFlag must not be Pcf_NONE */
      Pct_TRUE   = 0xC, /* associated PPCCondFlag must not be Pcf_NONE */
      Pct_ALWAYS = 0x14 /* associated PPCCondFlag must be Pcf_NONE */
   }
   PPCCondTest;

typedef
   struct {
      PPCCondFlag flag;
      PPCCondTest test;
   }
   PPCCondCode;

extern const HChar* showPPCCondCode ( PPCCondCode );

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
      Pun_CTZ32,
      Pun_CTZ64,
      Pun_EXTSW
   }
   PPCUnaryOp;

extern const HChar* showPPCUnaryOp ( PPCUnaryOp );


/* --------- */
typedef 
   enum {
      Palu_INVALID,
      Palu_ADD, Palu_SUB,
      Palu_AND, Palu_OR, Palu_XOR,
   }
   PPCAluOp;

extern 
const HChar* showPPCAluOp ( PPCAluOp, 
                            Bool /* is the 2nd operand an immediate? */);


/* --------- */
typedef 
   enum {
      Pshft_INVALID,
      Pshft_SHL, Pshft_SHR, Pshft_SAR, 
   }
   PPCShftOp;

extern 
const HChar* showPPCShftOp ( PPCShftOp, 
                             Bool /* is the 2nd operand an immediate? */,
                             Bool /* is this a 32bit or 64bit op? */ );


/* --------- */
typedef
   enum {
      Pfp_INVALID,

      /* Ternary */
      Pfp_MADDD,  Pfp_MSUBD,
      Pfp_MADDS,  Pfp_MSUBS,
      Pfp_FPADDQ, Pfp_FPADDQRNDODD,
      Pfp_FPSUBQ, Pfp_FPSUBQRNDODD,
      Pfp_FPMULQ, Pfp_FPMULQRNDODD,
      Pfp_FPDIVQ, Pfp_FPDIVQRNDODD,
      Pfp_FPMULADDQ, Pfp_FPMULADDQRNDODD,
      Pfp_FPMULSUBQ, Pfp_FPMULSUBQRNDODD,
      Pfp_FPNEGMULADDQ, Pfp_FPNEGMULADDQRNDODD,
      Pfp_FPNEGMULSUBQ, Pfp_FPNEGMULSUBQRNDODD,
      Pfp_FPSQRTQ, Pfp_FPSQRTQRNDODD,
      Pfp_FPQTOD, Pfp_FPQTODRNDODD,
      Pfp_FPQTOW, Pfp_FPQTOWRNDODD,
      Pfp_FPDTOQ,
      Pfp_IDSTOQ,
      Pfp_IDUTOQ,
      Pfp_TRUNCFPQTOISD,
      Pfp_TRUNCFPQTOISW,
      Pfp_TRUNCFPQTOIUD,
      Pfp_TRUNCFPQTOIUW,
      Pfp_DFPADD, Pfp_DFPADDQ,
      Pfp_DFPSUB, Pfp_DFPSUBQ,
      Pfp_DFPMUL, Pfp_DFPMULQ,
      Pfp_DFPDIV, Pfp_DFPDIVQ,
      Pfp_DQUAQ,  Pfp_DRRNDQ,

      /* Binary */
      Pfp_ADDD, Pfp_SUBD, Pfp_MULD, Pfp_DIVD,
      Pfp_ADDS, Pfp_SUBS, Pfp_MULS, Pfp_DIVS,
      Pfp_DRSP, Pfp_DRDPQ, Pfp_DCTFIX, Pfp_DCTFIXQ, Pfp_DCFFIX, 
      Pfp_DQUA, Pfp_RRDTR, Pfp_DIEX, Pfp_DIEXQ, Pfp_DRINTN,

      /* Unary */
      Pfp_SQRT, Pfp_ABS, Pfp_NEG, Pfp_MOV, Pfp_RES, Pfp_RSQRTE,
      Pfp_FRIN, Pfp_FRIM, Pfp_FRIP, Pfp_FRIZ, 
      Pfp_DSCLI, Pfp_DSCRI, Pfp_DSCLIQ, Pfp_DSCRIQ, Pfp_DCTDP,
      Pfp_DCTQPQ, Pfp_DCFFIXQ, Pfp_DXEX, Pfp_DXEXQ, 

   }
   PPCFpOp;

extern const HChar* showPPCFpOp ( PPCFpOp );


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
      Pav_MULU,
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

      /* Concatenation */
      Pav_CATODD, Pav_CATEVEN,

      /* Polynomial Multipy-Add */
      Pav_POLYMULADD,

      /* Cipher */
      Pav_CIPHERV128, Pav_CIPHERLV128, Pav_NCIPHERV128, Pav_NCIPHERLV128,
      Pav_CIPHERSUBV128,

      /* Hash */
      Pav_SHA256, Pav_SHA512,

      /* BCD Arithmetic */
      Pav_BCDAdd, Pav_BCDSub,

      /* Conversion signed 128-bit value to signed BCD 128-bit */
      Pav_I128StoBCD128,

      /* Conversion signed BCD 128-bit to signed 128-bit value */
      Pav_BCD128toI128S,

      /* zero count */
      Pav_ZEROCNTBYTE, Pav_ZEROCNTWORD, Pav_ZEROCNTHALF, Pav_ZEROCNTDBL,

      /* trailing zero count */
      Pav_TRAILINGZEROCNTBYTE, Pav_TRAILINGZEROCNTWORD,
      Pav_TRAILINGZEROCNTHALF, Pav_TRAILINGZEROCNTDBL,

      /* Vector bit matrix transpose by byte */
      Pav_BITMTXXPOSE,

      /* Vector Half-precision format to single precision conversion */
      Pav_F16toF32x4,

      /* Vector Single precision format to Half-precision conversion */
      Pav_F32toF16x4,

      /* Vector Half-precision format to Double precision conversion */
      Pav_F16toF64x2,

      /* Vector Double precision format to Half-precision conversion */
      Pav_F64toF16x2,

      /* 128 bit mult by 10 */
      Pav_MulI128by10,

      /* 128 bit mult by 10, carry out */
      Pav_MulI128by10Carry,

      /* 128 bit mult by 10 plus carry in */
      Pav_MulI128by10E,

      /* 128 bit mult by 10 plus carry in, carry out */
      Pav_MulI128by10ECarry,

      /* F128 to I128 */
      Pav_F128toI128S,

      /* Round F128 to F128 */
      Pav_ROUNDFPQ,
   }
   PPCAvOp;

extern const HChar* showPPCAvOp ( PPCAvOp );


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

extern const HChar* showPPCAvFpOp ( PPCAvFpOp );


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
      Pin_XDirect,    /* direct transfer to GA */
      Pin_XIndir,     /* indirect transfer to GA */
      Pin_XAssisted,  /* assisted transfer to GA */
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
      Pin_Fp128Unary,   /* FP unary op for 128-bit floating point */
      Pin_Fp128Binary,  /* FP binary op for 128-bit floating point */
      Pin_Fp128Trinary, /* FP trinary op for 128-bit floating point */
      Pin_FpMulAcc,   /* FP multipy-accumulate style op */
      Pin_FpLdSt,     /* FP load/store */
      Pin_FpSTFIW,    /* stfiwx */
      Pin_FpRSP,      /* FP round IEEE754 double to IEEE754 single */
      Pin_FpCftI,     /* fcfid[u,s,us]/fctid[u]/fctiw[u] */
      Pin_FpCMov,     /* FP floating point conditional move */
      Pin_FpLdFPSCR,  /* mtfsf */
      Pin_FpCmp,      /* FP compare, generating value into int reg */

      Pin_RdWrLR,     /* Read/Write Link Register */

      Pin_AvLdSt,     /* AV load/store (kludging for AMode_IR) */
      Pin_AvUnary,    /* AV unary general reg=>reg */

      Pin_AvBinary,   /* AV binary general reg,reg=>reg */
      Pin_AvBinaryInt,/* AV binary  reg,int=>reg */
      Pin_AvBin8x16,  /* AV binary, 8x4 */
      Pin_AvBin16x8,  /* AV binary, 16x4 */
      Pin_AvBin32x4,  /* AV binary, 32x4 */
      Pin_AvBin64x2,  /* AV binary, 64x2 */

      Pin_AvBin32Fx4, /* AV FP binary, 32Fx4 */
      Pin_AvUn32Fx4,  /* AV FP unary,  32Fx4 */

      Pin_AvPerm,     /* AV permute (shuffle) */
      Pin_AvSel,      /* AV select */
      Pin_AvSh,       /* AV shift left or right */
      Pin_AvShlDbl,   /* AV shift-left double by imm */
      Pin_AvSplat,    /* One elem repeated throughout dst */
      Pin_AvLdVSCR,   /* mtvscr */
      Pin_AvCMov,     /* AV conditional move */
      Pin_AvCipherV128Unary,  /* AV Vector unary Cipher */
      Pin_AvCipherV128Binary, /* AV Vector binary Cipher */
      Pin_AvHashV128Binary, /* AV Vector binary Hash */
      Pin_AvBCDV128Binary,  /* BCD Arithmetic */
      Pin_Dfp64Unary,   /* DFP64  unary op */
      Pin_Dfp128Unary,  /* DFP128 unary op */
      Pin_DfpShift,     /* Decimal floating point shift by immediate value */
      Pin_Dfp64Binary,  /* DFP64  binary op */
      Pin_Dfp128Binary, /* DFP128 binary op */
      Pin_DfpShift128,  /* 128-bit Decimal floating point shift by 
                         * immediate value */
      Pin_DfpD128toD64, /* DFP 128 to DFP 64 op */
      Pin_DfpI64StoD128, /* DFP signed integer to DFP 128 */
      Pin_DfpRound,       /* D64 round to D64 */
      Pin_DfpRound128,    /* D128 round to D128 */
      Pin_ExtractExpD128, /* DFP, extract 64 bit exponent */
      Pin_InsertExpD128,  /* DFP, insert 64 bit exponent and 128 bit binary 
                           * significand into a DFP 128-bit value*/
      Pin_Dfp64Cmp,       /* DFP 64-bit compare, generating value into
                           * int reg */
      Pin_Dfp128Cmp,      /* DFP 128-bit  compare, generating value into
                           * int reg */
      Pin_DfpQuantize,    /* D64 quantize using register value, significance 
                           * round */
      Pin_DfpQuantize128, /* D128 quantize using register value, significance
                           * round */
      Pin_EvCheck,    /* Event check */
      Pin_ProfInc     /* 64-bit profile counter increment */
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
            Bool extended;
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
            RetLoc      rloc;     /* where the return value will be */
         } Call;
         /* Update the guest CIA value, then exit requesting to chain
            to it.  May be conditional.  Use of Addr64 in order to cope
            with 64-bit hosts. */
         struct {
            Addr64      dstGA;    /* next guest address */
            PPCAMode*   amCIA;    /* amode in guest state for CIA */
            PPCCondCode cond;     /* can be ALWAYS */
            Bool        toFastEP; /* chain to the slow or fast point? */
         } XDirect;
         /* Boring transfer to a guest address not known at JIT time.
            Not chainable.  May be conditional. */
         struct {
            HReg        dstGA;
            PPCAMode*   amCIA;
            PPCCondCode cond; /* can be ALWAYS */
         } XIndir;
         /* Assisted transfer to a guest address, most general case.
            Not chainable.  May be conditional. */
         struct {
            HReg        dstGA;
            PPCAMode*   amCIA;
            PPCCondCode cond; /* can be ALWAYS */
            IRJumpKind  jk;
         } XAssisted;
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
            HReg dst;
            HReg src;
         } Fp128Unary;
         struct {
            PPCFpOp op;
            HReg dst;
            HReg srcL;
            HReg srcR;
         } Fp128Binary;
         struct {
            PPCFpOp op;
            HReg dst;
            HReg srcL;
            HReg srcR;
         } Fp128Trinary;
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
         /* fcfid[u,s,us]/fctid[u]/fctiw[u].  Only some combinations
            of the various fields are allowed.  This is asserted for
            and documented in the code for the constructor,
            PPCInstr_FpCftI, in host_ppc_defs.c.  */
         struct {
            Bool fromI; /* True== I->F,    False== F->I */
            Bool int32; /* True== I is 32, False== I is 64 */
            Bool syned;
            Bool flt64; /* True== F is 64, False== F is 32 */
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
            UInt dfp_rm;
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
            HReg    src;
            PPCRI*  val;
         } AvBinaryInt;
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
         /* Can only be generated for CPUs capable of ISA 2.07 or above */
         struct {
            PPCAvOp op;
            HReg    dst;
            HReg    srcL;
            HReg    srcR;
         } AvBin64x2;
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
            Bool  shLeft;
            HReg  dst;
            PPCAMode* addr;
         } AvSh;
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
         struct {
            PPCAvOp   op;
            HReg      dst;
            HReg      src;
         } AvCipherV128Unary;
         struct {
            PPCAvOp     op;
            HReg       dst;
            HReg       src;
            PPCRI* s_field;
         } AvHashV128Binary;
         struct {
            PPCAvOp     op;
            HReg       dst;
            HReg      src1;
            HReg      src2;
         } AvBCDV128Binary;
         struct {
            PPCAvOp   op;
            HReg      dst;
            HReg      srcL;
            HReg      srcR;
         } AvCipherV128Binary;
         struct {
            PPCFpOp op;
            HReg dst;
            HReg src;
         } Dfp64Unary;
         struct {
            PPCFpOp op;
            HReg dst;
            HReg srcL;
            HReg srcR;
         } Dfp64Binary;
         struct {
            PPCFpOp op;
            HReg   dst;
            HReg   src;
            PPCRI* shift;
         } DfpShift;
         struct {
            PPCFpOp op;
            HReg dst_hi;
            HReg dst_lo;
            HReg src_hi;
            HReg src_lo;
         } Dfp128Unary;
         struct {
            /* The dst is used to pass the left source operand in and return
             * the result.
             */
            PPCFpOp op;
            HReg dst_hi;
            HReg dst_lo;
            HReg srcR_hi;
            HReg srcR_lo;
         } Dfp128Binary;
         struct {
            PPCFpOp op;
            HReg   dst_hi;
            HReg   dst_lo;
            HReg   src_hi;
            HReg   src_lo;
            PPCRI* shift;
         } DfpShift128;
         struct {
            HReg dst;
            HReg src;
            PPCRI* r_rmc;
         } DfpRound;
         struct {
            HReg dst_hi;
            HReg dst_lo;
            HReg src_hi;
            HReg src_lo;
            PPCRI* r_rmc;
         } DfpRound128;
         struct {
	    PPCFpOp op;
            HReg dst;
            HReg srcL;
            HReg srcR;
            PPCRI* rmc;
         } DfpQuantize;
         struct {
	    PPCFpOp op;
            HReg dst_hi;
            HReg dst_lo;
            HReg src_hi;
            HReg src_lo;
  	    PPCRI* rmc;
         } DfpQuantize128;
         struct {
            PPCFpOp op;
            HReg dst;
            HReg src_hi;
            HReg src_lo;
         } ExtractExpD128;
         struct {
	    PPCFpOp op;
            HReg dst_hi;
            HReg dst_lo;
            HReg srcL;
            HReg srcR_hi;
            HReg srcR_lo;
         } InsertExpD128;
         struct {
            PPCFpOp op;
            HReg   dst;
            HReg   src_hi;
            HReg   src_lo;
         } DfpD128toD64;
         struct {
            PPCFpOp op;
            HReg   dst_hi;
            HReg   dst_lo;
            HReg   src;
         } DfpI64StoD128;
         struct {
            UChar crfD;
            HReg  dst;
            HReg  srcL;
            HReg  srcR;
         } Dfp64Cmp;
         struct {         
            UChar crfD;   
            HReg  dst;    
            HReg  srcL_hi;
            HReg  srcL_lo;
            HReg  srcR_hi;
            HReg  srcR_lo;
         } Dfp128Cmp;     
         struct {
            PPCAMode* amCounter;
            PPCAMode* amFailAddr;
         } EvCheck;
         struct {
            /* No fields.  The address of the counter to inc is
               installed later, post-translation, by patching it in,
               as it is not known at translation time. */
         } ProfInc;
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
extern PPCInstr* PPCInstr_Div        ( Bool extended, Bool syned, Bool sz32, HReg dst, HReg srcL, HReg srcR );
extern PPCInstr* PPCInstr_Call       ( PPCCondCode, Addr64, UInt, RetLoc );
extern PPCInstr* PPCInstr_XDirect    ( Addr64 dstGA, PPCAMode* amCIA,
                                       PPCCondCode cond, Bool toFastEP );
extern PPCInstr* PPCInstr_XIndir     ( HReg dstGA, PPCAMode* amCIA,
                                       PPCCondCode cond );
extern PPCInstr* PPCInstr_XAssisted  ( HReg dstGA, PPCAMode* amCIA,
                                       PPCCondCode cond, IRJumpKind jk );
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

extern PPCInstr* PPCInstr_Fp128Unary    ( PPCFpOp op, HReg dst, HReg src );
extern PPCInstr* PPCInstr_Fp128Binary   ( PPCFpOp op, HReg dst, HReg srcL, HReg srcR );
extern PPCInstr* PPCInstr_Fp128Trinary  ( PPCFpOp op, HReg dst, HReg srcL,
                                          HReg srcR);

extern PPCInstr* PPCInstr_FpUnary    ( PPCFpOp op, HReg dst, HReg src );
extern PPCInstr* PPCInstr_FpBinary   ( PPCFpOp op, HReg dst, HReg srcL, HReg srcR );
extern PPCInstr* PPCInstr_FpMulAcc   ( PPCFpOp op, HReg dst, HReg srcML, 
                                                   HReg srcMR, HReg srcAcc );
extern PPCInstr* PPCInstr_FpLdSt     ( Bool isLoad, UChar sz, HReg, PPCAMode* );
extern PPCInstr* PPCInstr_FpSTFIW    ( HReg addr, HReg data );
extern PPCInstr* PPCInstr_FpRSP      ( HReg dst, HReg src );
extern PPCInstr* PPCInstr_FpCftI     ( Bool fromI, Bool int32, Bool syned,
                                       Bool dst64, HReg dst, HReg src );
extern PPCInstr* PPCInstr_FpCMov     ( PPCCondCode, HReg dst, HReg src );
extern PPCInstr* PPCInstr_FpLdFPSCR  ( HReg src, Bool dfp_rm );
extern PPCInstr* PPCInstr_FpCmp      ( HReg dst, HReg srcL, HReg srcR );

extern PPCInstr* PPCInstr_RdWrLR     ( Bool wrLR, HReg gpr );

extern PPCInstr* PPCInstr_AvLdSt     ( Bool isLoad, UChar sz, HReg, PPCAMode* );
extern PPCInstr* PPCInstr_AvUnary    ( PPCAvOp op, HReg dst, HReg src );
extern PPCInstr* PPCInstr_AvBinary   ( PPCAvOp op, HReg dst, HReg srcL, HReg srcR );
extern PPCInstr* PPCInstr_AvBinaryInt( PPCAvOp op, HReg dst, HReg src, PPCRI* val );
extern PPCInstr* PPCInstr_AvBin8x16  ( PPCAvOp op, HReg dst, HReg srcL, HReg srcR );
extern PPCInstr* PPCInstr_AvBin16x8  ( PPCAvOp op, HReg dst, HReg srcL, HReg srcR );
extern PPCInstr* PPCInstr_AvBin32x4  ( PPCAvOp op, HReg dst, HReg srcL, HReg srcR );
extern PPCInstr* PPCInstr_AvBin64x2  ( PPCAvOp op, HReg dst, HReg srcL, HReg srcR );
extern PPCInstr* PPCInstr_AvBin32Fx4 ( PPCAvFpOp op, HReg dst, HReg srcL, HReg srcR );
extern PPCInstr* PPCInstr_AvUn32Fx4  ( PPCAvFpOp op, HReg dst, HReg src );
extern PPCInstr* PPCInstr_AvPerm     ( HReg dst, HReg srcL, HReg srcR, HReg ctl );
extern PPCInstr* PPCInstr_AvSel      ( HReg ctl, HReg dst, HReg srcL, HReg srcR );
extern PPCInstr* PPCInstr_AvSh       ( Bool shLeft, HReg dst, PPCAMode* am_addr );
extern PPCInstr* PPCInstr_AvShlDbl   ( UChar shift, HReg dst, HReg srcL, HReg srcR );
extern PPCInstr* PPCInstr_AvSplat    ( UChar sz, HReg dst, PPCVI5s* src );
extern PPCInstr* PPCInstr_AvCMov     ( PPCCondCode, HReg dst, HReg src );
extern PPCInstr* PPCInstr_AvLdVSCR   ( HReg src );
extern PPCInstr* PPCInstr_AvCipherV128Unary  ( PPCAvOp op, HReg dst,
                                               HReg srcR );
extern PPCInstr* PPCInstr_AvCipherV128Binary ( PPCAvOp op, HReg dst,
                                               HReg srcL, HReg srcR );
extern PPCInstr* PPCInstr_AvHashV128Binary ( PPCAvOp op, HReg dst,
                                             HReg src, PPCRI* s_field );
extern PPCInstr* PPCInstr_AvBCDV128Binary ( PPCAvOp op, HReg dst,
                                            HReg src1, HReg src2 );
extern PPCInstr* PPCInstr_Dfp64Unary  ( PPCFpOp op, HReg dst, HReg src );
extern PPCInstr* PPCInstr_Dfp64Binary ( PPCFpOp op, HReg dst, HReg srcL,
                                        HReg srcR );
extern PPCInstr* PPCInstr_DfpShift    ( PPCFpOp op, HReg dst, HReg src,
                                        PPCRI* shift );
extern PPCInstr* PPCInstr_Dfp128Unary  ( PPCFpOp op, HReg dst_hi, HReg dst_lo,
                                         HReg srcR_hi, HReg srcR_lo );
extern PPCInstr* PPCInstr_Dfp128Binary ( PPCFpOp op, HReg dst_hi, HReg dst_lo,
                                         HReg srcR_hi, HReg srcR_lo );
extern PPCInstr* PPCInstr_DfpShift128  ( PPCFpOp op, HReg dst_hi, HReg src_hi,
                                         HReg dst_lo, HReg src_lo,
                                         PPCRI* shift );
extern PPCInstr* PPCInstr_DfpD128toD64 ( PPCFpOp op, HReg dst,
                                         HReg dst_lo, HReg src_lo);
extern PPCInstr* PPCInstr_DfpI64StoD128  ( PPCFpOp op, HReg dst_hi,
                                           HReg dst_lo, HReg src);
extern PPCInstr* PPCInstr_DfpRound       ( HReg dst, HReg src, PPCRI* r_rmc );
extern PPCInstr* PPCInstr_DfpRound128    ( HReg dst_hi, HReg dst_lo, HReg src_hi,
                                           HReg src_lo, PPCRI* r_rmc );
extern PPCInstr* PPCInstr_DfpQuantize    ( PPCFpOp op, HReg dst, HReg srcL,
                                           HReg srcR, PPCRI* rmc );
extern PPCInstr* PPCInstr_DfpQuantize128 ( PPCFpOp op, HReg dst_hi,
                                           HReg dst_lo,
                                           HReg src_hi,
                                           HReg src_lo, PPCRI* rmc );
extern PPCInstr* PPCInstr_ExtractExpD128 ( PPCFpOp op,   HReg dst, 
                                           HReg src_hi, HReg src_lo );
extern PPCInstr* PPCInstr_InsertExpD128  ( PPCFpOp op,   HReg dst_hi, 
                                           HReg dst_lo,  HReg srcL,
                                           HReg srcR_hi, HReg srcR_lo );
extern PPCInstr* PPCInstr_Dfp64Cmp       ( HReg dst, HReg srcL, HReg srcR );
extern PPCInstr* PPCInstr_Dfp128Cmp      ( HReg dst, HReg srcL_hi, HReg srcL_lo,
                                           HReg srcR_hi, HReg srcR_lo );
extern PPCInstr* PPCInstr_EvCheck     ( PPCAMode* amCounter,
                                        PPCAMode* amFailAddr );
extern PPCInstr* PPCInstr_ProfInc     ( void );

extern void ppPPCInstr(const PPCInstr*, Bool mode64);


/* Some functions that insulate the register allocator from details
   of the underlying instruction set. */
extern void getRegUsage_PPCInstr ( HRegUsage*, const PPCInstr*, Bool mode64 );
extern void mapRegs_PPCInstr     ( HRegRemap*, PPCInstr* , Bool mode64);
extern Bool isMove_PPCInstr      ( const PPCInstr*, HReg*, HReg* );
extern Int          emit_PPCInstr   ( /*MB_MOD*/Bool* is_profInc,
                                      UChar* buf, Int nbuf, const PPCInstr* i, 
                                      Bool mode64,
                                      VexEndness endness_host,
                                      const void* disp_cp_chain_me_to_slowEP,
                                      const void* disp_cp_chain_me_to_fastEP,
                                      const void* disp_cp_xindir,
                                      const void* disp_cp_xassisted );

extern void genSpill_PPC  ( /*OUT*/HInstr** i1, /*OUT*/HInstr** i2,
                            HReg rreg, Int offsetB, Bool mode64 );
extern void genReload_PPC ( /*OUT*/HInstr** i1, /*OUT*/HInstr** i2,
                            HReg rreg, Int offsetB, Bool mode64 );

extern const RRegUniverse* getRRegUniverse_PPC ( Bool mode64 );

extern HInstrArray* iselSB_PPC           ( const IRSB*,
                                           VexArch,
                                           const VexArchInfo*,
                                           const VexAbiInfo*,
                                           Int offs_Host_EvC_Counter,
                                           Int offs_Host_EvC_FailAddr,
                                           Bool chainingAllowed,
                                           Bool addProfInc,
                                           Addr max_ga );

/* How big is an event check?  This is kind of a kludge because it
   depends on the offsets of host_EvC_FAILADDR and
   host_EvC_COUNTER. */
extern Int evCheckSzB_PPC (void);

/* Perform a chaining and unchaining of an XDirect jump. */
extern VexInvalRange chainXDirect_PPC ( VexEndness endness_host,
                                        void* place_to_chain,
                                        const void* disp_cp_chain_me_EXPECTED,
                                        const void* place_to_jump_to,
                                        Bool  mode64 );

extern VexInvalRange unchainXDirect_PPC ( VexEndness endness_host,
                                          void* place_to_unchain,
                                          const void* place_to_jump_to_EXPECTED,
                                          const void* disp_cp_chain_me,
                                          Bool  mode64 );

/* Patch the counter location into an existing ProfInc point. */
extern VexInvalRange patchProfInc_PPC ( VexEndness endness_host,
                                        void*  place_to_patch,
                                        const ULong* location_of_counter,
                                        Bool   mode64 );


#endif /* ndef __VEX_HOST_PPC_DEFS_H */

/*---------------------------------------------------------------*/
/*--- end                                     host_ppc_defs.h ---*/
/*---------------------------------------------------------------*/
