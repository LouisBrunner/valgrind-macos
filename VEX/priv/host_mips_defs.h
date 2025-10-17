
/*---------------------------------------------------------------*/
/*--- begin                                  host_mips_defs.h ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2010-2017 RT-RK

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.
*/

#ifndef __VEX_HOST_MIPS_DEFS_H
#define __VEX_HOST_MIPS_DEFS_H

#include "libvex_basictypes.h"
#include "libvex.h"             /* VexArch */
#include "host_generic_regs.h"  /* HReg */


/* --------- Registers. --------- */

#define ST_IN static inline

#define GPR(_mode64, _enc, _ix64, _ix32) \
  mkHReg(False,  (_mode64) ? HRcInt64 : HRcInt32, \
         (_enc), (_mode64) ? (_ix64) : (_ix32))

#define FR(_mode64, _enc, _ix64, _ix32) \
  mkHReg(False,  (_mode64) ? HRcFlt64 : HRcFlt32, \
         (_enc), (_mode64) ? (_ix64) : (_ix32))

#define DR(_mode64, _enc, _ix64, _ix32) \
  mkHReg(False,  HRcFlt64, \
         (_enc), (_mode64) ? (_ix64) : (_ix32))

#define VEC(_mode64, _enc, _ix64, _ix32) \
  mkHReg(False,  HRcVec128, \
         (_enc), (_mode64) ? (_ix64) : (_ix32))

ST_IN HReg hregMIPS_GPR16 ( Bool mode64 ) { return GPR(mode64, 16,  0,  0); }
ST_IN HReg hregMIPS_GPR17 ( Bool mode64 ) { return GPR(mode64, 17,  1,  1); }
ST_IN HReg hregMIPS_GPR18 ( Bool mode64 ) { return GPR(mode64, 18,  2,  2); }
ST_IN HReg hregMIPS_GPR19 ( Bool mode64 ) { return GPR(mode64, 19,  3,  3); }
ST_IN HReg hregMIPS_GPR20 ( Bool mode64 ) { return GPR(mode64, 20,  4,  4); }
ST_IN HReg hregMIPS_GPR21 ( Bool mode64 ) { return GPR(mode64, 21,  5,  5); }
ST_IN HReg hregMIPS_GPR22 ( Bool mode64 ) { return GPR(mode64, 22,  6,  6); }

ST_IN HReg hregMIPS_GPR12 ( Bool mode64 ) { return GPR(mode64, 12,  7,  7); }
ST_IN HReg hregMIPS_GPR13 ( Bool mode64 ) { return GPR(mode64, 13,  8,  8); }
ST_IN HReg hregMIPS_GPR14 ( Bool mode64 ) { return GPR(mode64, 14,  9,  9); }
ST_IN HReg hregMIPS_GPR15 ( Bool mode64 ) { return GPR(mode64, 15, 10, 10); }
ST_IN HReg hregMIPS_GPR24 ( Bool mode64 ) { return GPR(mode64, 24, 11, 11); }

ST_IN HReg hregMIPS_F16   ( Bool mode64 ) { return FR (mode64, 16, 12, 12); }
ST_IN HReg hregMIPS_F18   ( Bool mode64 ) { return FR (mode64, 18, 13, 13); }
ST_IN HReg hregMIPS_F20   ( Bool mode64 ) { return FR (mode64, 20, 14, 14); }
ST_IN HReg hregMIPS_F22   ( Bool mode64 ) { return FR (mode64, 22, 15, 15); }
ST_IN HReg hregMIPS_F24   ( Bool mode64 ) { return FR (mode64, 24, 16, 16); }
ST_IN HReg hregMIPS_F26   ( Bool mode64 ) { return FR (mode64, 26, 17, 17); }
ST_IN HReg hregMIPS_F28   ( Bool mode64 ) { return FR (mode64, 28, 18, 18); }
ST_IN HReg hregMIPS_F30   ( Bool mode64 ) { return FR (mode64, 30, 19, 19); }

ST_IN HReg hregMIPS_W16    ( Bool mode64 ) { return VEC(mode64, 1, 20, 20); }
ST_IN HReg hregMIPS_W17    ( Bool mode64 ) { return VEC(mode64, 3, 21, 21); }
ST_IN HReg hregMIPS_W18    ( Bool mode64 ) { return VEC(mode64, 5, 22, 22); }
ST_IN HReg hregMIPS_W19    ( Bool mode64 ) { return VEC(mode64, 7, 23, 23); }
ST_IN HReg hregMIPS_W20    ( Bool mode64 ) { return VEC(mode64, 9, 24, 24); }
ST_IN HReg hregMIPS_W21    ( Bool mode64 ) { return VEC(mode64, 11, 25, 25); }
ST_IN HReg hregMIPS_W22    ( Bool mode64 ) { return VEC(mode64, 13, 26, 26); }
ST_IN HReg hregMIPS_W23    ( Bool mode64 ) { return VEC(mode64, 15, 27, 27); }
ST_IN HReg hregMIPS_W24    ( Bool mode64 ) { return VEC(mode64, 17, 28, 28); }
ST_IN HReg hregMIPS_W25    ( Bool mode64 ) { return VEC(mode64, 19, 29, 29); }
ST_IN HReg hregMIPS_W26    ( Bool mode64 ) { return VEC(mode64, 21, 30, 30); }
ST_IN HReg hregMIPS_W27    ( Bool mode64 ) { return VEC(mode64, 23, 31, 31); }
ST_IN HReg hregMIPS_W28    ( Bool mode64 ) { return VEC(mode64, 25, 32, 32); }
ST_IN HReg hregMIPS_W29    ( Bool mode64 ) { return VEC(mode64, 27, 33, 33); }
ST_IN HReg hregMIPS_W30    ( Bool mode64 ) { return VEC(mode64, 29, 34, 34); }
ST_IN HReg hregMIPS_W31    ( Bool mode64 ) { return VEC(mode64, 31, 35, 35); }

// DRs are only allocatable in 32-bit mode, so the 64-bit index numbering
// doesn't advance here.
ST_IN HReg hregMIPS_D0    ( Bool mode64 ) { vassert(!mode64);
                                            return DR (mode64,  0,  0, 36); }
ST_IN HReg hregMIPS_D1    ( Bool mode64 ) { vassert(!mode64);
                                            return DR (mode64,  2,  0, 37); }
ST_IN HReg hregMIPS_D2    ( Bool mode64 ) { vassert(!mode64);
                                            return DR (mode64,  4,  0, 38); }
ST_IN HReg hregMIPS_D3    ( Bool mode64 ) { vassert(!mode64);
                                            return DR (mode64,  6,  0, 39); }
ST_IN HReg hregMIPS_D4    ( Bool mode64 ) { vassert(!mode64);
                                            return DR (mode64,  8,  0, 40); }
ST_IN HReg hregMIPS_D5    ( Bool mode64 ) { vassert(!mode64);
                                            return DR (mode64, 10,  0, 41); }
ST_IN HReg hregMIPS_D6    ( Bool mode64 ) { vassert(!mode64);
                                            return DR (mode64, 12,  0, 42); }
ST_IN HReg hregMIPS_D7    ( Bool mode64 ) { vassert(!mode64);
                                            return DR (mode64, 14,  0, 43); }

ST_IN HReg hregMIPS_HI    ( Bool mode64 ) { return FR (mode64, 33, 36, 44); }
ST_IN HReg hregMIPS_LO    ( Bool mode64 ) { return FR (mode64, 34, 37, 45); }

ST_IN HReg hregMIPS_GPR0  ( Bool mode64 ) { return GPR(mode64,  0, 38, 46); }
ST_IN HReg hregMIPS_GPR1  ( Bool mode64 ) { return GPR(mode64,  1, 39, 47); }
ST_IN HReg hregMIPS_GPR2  ( Bool mode64 ) { return GPR(mode64,  2, 40, 48); }
ST_IN HReg hregMIPS_GPR3  ( Bool mode64 ) { return GPR(mode64,  3, 41, 49); }
ST_IN HReg hregMIPS_GPR4  ( Bool mode64 ) { return GPR(mode64,  4, 42, 50); }
ST_IN HReg hregMIPS_GPR5  ( Bool mode64 ) { return GPR(mode64,  5, 43, 51); }
ST_IN HReg hregMIPS_GPR6  ( Bool mode64 ) { return GPR(mode64,  6, 44, 52); }
ST_IN HReg hregMIPS_GPR7  ( Bool mode64 ) { return GPR(mode64,  7, 45, 53); }
ST_IN HReg hregMIPS_GPR8  ( Bool mode64 ) { return GPR(mode64,  8, 46, 54); }
ST_IN HReg hregMIPS_GPR9  ( Bool mode64 ) { return GPR(mode64,  9, 47, 55); }
ST_IN HReg hregMIPS_GPR10 ( Bool mode64 ) { return GPR(mode64, 10, 48, 56); }
ST_IN HReg hregMIPS_GPR11 ( Bool mode64 ) { return GPR(mode64, 11, 49, 57); }
ST_IN HReg hregMIPS_GPR23 ( Bool mode64 ) { return GPR(mode64, 23, 50, 58); }
ST_IN HReg hregMIPS_GPR25 ( Bool mode64 ) { return GPR(mode64, 25, 51, 59); }
ST_IN HReg hregMIPS_GPR29 ( Bool mode64 ) { return GPR(mode64, 29, 52, 60); }
ST_IN HReg hregMIPS_GPR31 ( Bool mode64 ) { return GPR(mode64, 31, 53, 61); }

#undef ST_IN
#undef GPR
#undef FR
#undef DR
#undef VEC

#define GuestStatePointer(_mode64)     hregMIPS_GPR23(_mode64)
#define StackFramePointer(_mode64)     hregMIPS_GPR30(_mode64)
#define StackPointer(_mode64)          hregMIPS_GPR29(_mode64)
#define Zero(_mode64)                  hregMIPS_GPR0(_mode64)

/* guest_COND offset */
#define COND_OFFSET(_mode64) ((_mode64) ? 588 : 448)

/* guest_MSACSR offset */
#define MSACSR_OFFSET(_mode64) ((_mode64) ? 1144 : 1016)

/* Num registers used for function calls */
#if defined(VGP_mips32_linux)
  /* a0, a1, a2, a3 */
# define MIPS_N_REGPARMS 4
#else
  /* a0, a1, a2, a3, a4, a5, a6, a7 */
# define MIPS_N_REGPARMS 8
#endif

extern UInt ppHRegMIPS ( HReg, Bool );

#define OPC_MSA        0x78000000

/* --------- Condition codes, Intel encoding. --------- */
typedef enum {
   MIPScc_EQ = 0,   /* equal */
   MIPScc_NE = 1,   /* not equal */

   MIPScc_HS = 2,   /* >=u (higher or same) */
   MIPScc_LO = 3,   /* <u  (lower) */

   MIPScc_MI = 4,   /* minus (negative) */
   MIPScc_PL = 5,   /* plus (zero or +ve) */

   MIPScc_VS = 6,   /* overflow */
   MIPScc_VC = 7,   /* no overflow */

   MIPScc_HI = 8,   /* >u   (higher) */
   MIPScc_LS = 9,   /* <=u  (lower or same) */

   MIPScc_GE = 10,  /* >=s (signed greater or equal) */
   MIPScc_LT = 11,  /* <s  (signed less than) */

   MIPScc_GT = 12,  /* >s  (signed greater) */
   MIPScc_LE = 13,  /* <=s (signed less or equal) */

   MIPScc_AL = 14,  /* always (unconditional) */
   MIPScc_NV = 15   /* never (unconditional): */
} MIPSCondCode;

extern const HChar *showMIPSCondCode(MIPSCondCode);

/* --------- Memory address expressions (amodes). --------- */
typedef enum {
   Mam_IR,        /* Immediate (signed 16-bit) + Reg */
   Mam_RR         /* Reg1 + Reg2 */
} MIPSAModeTag;

typedef struct {
   MIPSAModeTag tag;
   union {
      struct {
         HReg base;
         Int index;
      } IR;
      struct {
         HReg base;
         HReg index;
      } RR;
   } Mam;
} MIPSAMode;

extern MIPSAMode *MIPSAMode_IR(Int, HReg);
extern MIPSAMode *MIPSAMode_RR(HReg, HReg);

extern MIPSAMode *dopyMIPSAMode(MIPSAMode *);
extern MIPSAMode *nextMIPSAModeFloat(MIPSAMode *);
extern MIPSAMode *nextMIPSAModeInt(MIPSAMode *);

extern void ppMIPSAMode(MIPSAMode *, Bool);

/* --------- Operand, which can be a reg or a u16/s16. --------- */
/* ("RH" == "Register or Halfword immediate") */
typedef enum {
   Mrh_Imm,
   Mrh_Reg
} MIPSRHTag;

typedef struct {
   MIPSRHTag tag;
   union {
      struct {
         Bool syned;
         UShort imm16;
      } Imm;
      struct {
         HReg reg;
      } Reg;
   } Mrh;
} MIPSRH;

extern void ppMIPSRH(MIPSRH *, Bool);

extern MIPSRH *MIPSRH_Imm(Bool, UShort);
extern MIPSRH *MIPSRH_Reg(HReg);

/* --------- Instructions. --------- */

/*Tags for operations*/

/* --------- */
typedef enum {
   Mun_CLO,
   Mun_CLZ,
   Mun_DCLO,
   Mun_DCLZ,
   Mun_NOP,
} MIPSUnaryOp;

extern const HChar *showMIPSUnaryOp(MIPSUnaryOp);
/* --------- */

/* --------- */

typedef enum {
   Malu_INVALID,
   Malu_ADD, Malu_SUB,
   Malu_AND, Malu_OR, Malu_NOR, Malu_XOR,
   Malu_DADD, Malu_DSUB,
   Malu_SLT
} MIPSAluOp;

extern const HChar *showMIPSAluOp(MIPSAluOp,
                            Bool /* is the 2nd operand an immediate? */ );

/* --------- */
typedef enum {
   Mshft_INVALID,
   Mshft_SLL, Mshft_SRL,
   Mshft_SRA
} MIPSShftOp;

extern const HChar *showMIPSShftOp(MIPSShftOp,
                             Bool /* is the 2nd operand an immediate? */ ,
                             Bool /* is this a 32bit or 64bit op? */ );

/* --------- */
typedef enum {
   Macc_ADD,
   Macc_SUB
} MIPSMaccOp;

extern const HChar *showMIPSMaccOp(MIPSMaccOp, Bool);
/* --------- */

typedef enum {
   MSA_LD = 8,
   MSA_ST = 9
} MSAMI10Op;

extern const HChar *showMsaMI10op(MSAMI10Op);

typedef enum {
   MSA_SLDI   = 0,
   MSA_COPY_S = 2,
   MSA_COPY_U = 3,
   MSA_INSERT = 4,
   MSA_INSVE  = 5,
   MSA_MOVE   = 0xBE,
   MSA_CFCMSA = 0x7E,
   MSA_CTCMSA = 0x3E
} MSAELMOp;

extern const HChar *showMsaElmOp(MSAELMOp);

typedef enum {
   MSA_FILL = 0xC0,
   MSA_PCNT = 0xC1,
   MSA_NLOC = 0xC2,
   MSA_NLZC = 0xC3
} MSA2ROp;

extern const HChar *showMsa2ROp(MSA2ROp);

typedef enum {
   MSA_FTRUNC_S = 0x191,
   MSA_FTRUNC_U = 0x192,
   MSA_FFINT_S  = 0x19E,
   MSA_FFINT_U  = 0x19F,
   MSA_FSQRT    = 0x193,
   MSA_FRSQRT   = 0x194,
   MSA_FRCP     = 0x195,
   MSA_FLOG2    = 0x197,
   MSA_FEXUPR   = 0x199,
   MSA_FTINT_U  = 0x19D,
   MSA_FTINT_S  = 0x19C,
} MSA2RFOp;

extern const HChar *showMsa2RFOp(MSA2RFOp);

typedef enum {
   MSA_SLL = 0xD,
   MSA_ADDV,
   MSA_CEQ,
   MSA_ADD_A,
   MSA_SUBS_S,
   MSA_SLD = 0x14,
   MSA_SRA = 0x80000D,
   MSA_SUBV,
   MSA_SUBS_U = 0x800011,
   MSA_SRL = 0x100000D,
   MSA_MAX_S,
   MSA_CLT_S,
   MSA_ADDS_S,
   MSA_PCKEV = 0x1000014,
   MSA_MAX_U = 0x180000E,
   MSA_CLT_U,
   MSA_ADDS_U,
   MSA_PCKOD = 0x1800014,
   MSA_MIN_S = 0x200000E,
   MSA_ILVL = 0x2000014,
   MSA_MIN_U = 0x280000E,
   MSA_ILVR = 0x2800014,
   MSA_AVER_S = 0x3000010,
   MSA_ILVEV = 0x3000014,
   MSA_AVER_U = 0x3800010,
   MSA_ILVOD = 0x3800014,
   MSA_MULV = 0x0000012,
   MSA_SPLAT = 0x0800014,
   MSA_DIVS = 0x2000012,
   MSA_DIVU = 0x2800012,
   MSA_VSHF = 0x0000015,
} MSA3ROp;

extern const HChar *showMsa3ROp(MSA3ROp);

typedef enum {
   MSA_FADD   = 0x000001B,
   MSA_FCUN   = 0x040001A,
   MSA_FSUB   = 0x040001B,
   MSA_FCEQ   = 0x080001A,
   MSA_FMUL   = 0x080001B,
   MSA_FDIV   = 0x0C0001B,
   MSA_FMADD  = 0x100001B,
   MSA_FCLT   = 0x100001A,
   MSA_FMSUB  = 0x140001B,
   MSA_FEXP2  = 0x1C0001B,
   MSA_FMIN   = 0x300001B,
   MSA_FMIN_A = 0x340001B,
   MSA_FMAX   = 0x380001B,
   MSA_MUL_Q  = 0x100001C,
   MSA_FCLE   = 0x180001A,
   MSA_FTQ    = 0x280001B,
   MSA_FEXDO  = 0x200001B,
   MSA_MULR_Q = 0x300001C,
} MSA3RFOp;

extern const HChar *showMsa3RFOp(MSA3RFOp);

typedef enum {
   MSA_ANDV,
   MSA_ORV,
   MSA_NORV,
   MSA_XORV
} MSAVECOp;

extern const HChar *showMsaVecOp(MSAVECOp);

typedef enum {
   MSA_SLLI = 9,
   MSA_SAT_S,
   MSA_SRAI = 0x800009,
   MSA_SRLI = 0x1000009,
   MSA_SRARI = 0x100000A
} MSABITOp;

extern const HChar *showMsaBitOp(MSABITOp);

typedef enum {
   MSA_B = 0,
   MSA_H = 1,
   MSA_W = 2,
   MSA_D = 3,
} MSADF;

extern HChar showMsaDF(MSADF df);

typedef enum {
   MSA_DFN_B    = 0x00,
   MSA_DFN_H    = 0x20,
   MSA_DFN_W    = 0x30,
   MSA_DFN_D    = 0x38,
} MSADFNMask;

typedef enum {
   MSA_F_WH = 0,
   MSA_F_DW = 1,
} MSADFFlx;

extern HChar showMsaDFF(MSADFFlx df, int op);


/* ----- Instruction tags ----- */
typedef enum {
   Min_LI,         /* load word (32/64-bit) immediate (fake insn) */
   Min_Alu,        /* word add/sub/and/or/xor/nor/others? */
   Min_Shft,       /* word sll/srl/sra */
   Min_Unary,      /* clo, clz, nop, neg */
   Min_Ext,        /* ext / dext, dextm, dextu */
   Min_Rotx,

   Min_Cmp,        /* word compare (fake insn) */

   Min_Mul,        /* non-widening, 32-bit, signed multiply */
   Min_Mult,       /* widening multiply */
   Min_Mulr6,
   Min_Div,        /* div */
   Min_Divr6,

   Min_Call,       /* call to address in register */

   /* The following 5 insns are mandated by translation chaining */
   Min_XDirect,    /* direct transfer to GA */
   Min_XIndir,     /* indirect transfer to GA */
   Min_XAssisted,  /* assisted transfer to GA */
   Min_EvCheck,    /* Event check */
   Min_ProfInc,    /* 64-bit profile counter increment */

   Min_RdWrLR,     /* Read/Write Link Register */
   Min_Mthi,       /* Move to HI from GP register */
   Min_Mtlo,       /* Move to LO from GP register */
   Min_Mfhi,       /* Move from HI to GP register */
   Min_Mflo,       /* Move from LO to GP register */
   Min_Macc,       /* Multiply and accumulate */

   Min_Load,       /* zero-extending load a 8|16|32 bit value from mem */
   Min_Store,      /* store a 8|16|32 bit value to mem */
   Min_Cas,        /* compare and swap */
   Min_LoadL,      /* mips Load Linked Word - LL */
   Min_StoreC,     /* mips Store Conditional Word - SC */

   Min_FpUnary,    /* FP unary op */
   Min_FpBinary,   /* FP binary op */
   Min_FpTernary,  /* FP ternary op */
   Min_FpConvert,  /* FP conversion op */
   Min_FpMulAcc,   /* FP multipy-accumulate style op */
   Min_FpLdSt,     /* FP load/store */
   Min_FpSTFIW,    /* stfiwx */
   Min_FpRSP,      /* FP round IEEE754 double to IEEE754 single */
   Min_FpCftI,     /* fcfid/fctid/fctiw */
   Min_FpCMov,     /* FP floating point conditional move */
   Min_MtFCSR,     /* set FCSR register */
   Min_MfFCSR,     /* get FCSR register */
   Min_FpCompare,  /* FP compare, generating value into int reg */
   Min_FpMinMax,    /* FP r6 min and max*/

   Min_FpGpMove,   /* Move from/to fpr to/from gpr */
   Min_MoveCond,   /* Move Conditional */

   Msa_MI10,
   Msa_ELM,
   Msa_3R,
   Msa_2R,
   Msa_VEC,
   Msa_BIT,
   Msa_3RF,
   Msa_2RF,
} MIPSInstrTag;

/* --------- */
typedef enum {
   Mfp_INVALID,

   /* Ternary */
   Mfp_MADDD, Mfp_MSUBD,
   Mfp_MADDS, Mfp_MSUBS,

   /* Binary */
   Mfp_ADDD, Mfp_SUBD, Mfp_MULD, Mfp_DIVD,
   Mfp_ADDS, Mfp_SUBS, Mfp_MULS, Mfp_DIVS,

   /* Unary */
   Mfp_SQRTS, Mfp_SQRTD,
   Mfp_ABSS, Mfp_ABSD, Mfp_NEGS, Mfp_NEGD, Mfp_MOVS, Mfp_MOVD,

   /* FP convert */
   Mfp_CVTSD, Mfp_CVTSW, Mfp_CVTWD,
   Mfp_CVTWS, Mfp_CVTDL, Mfp_CVTSL, Mfp_CVTLS, Mfp_CVTLD, Mfp_TRULS, Mfp_TRULD,
   Mfp_TRUWS, Mfp_TRUWD, Mfp_FLOORWS, Mfp_FLOORWD, Mfp_ROUNDWS, Mfp_ROUNDWD,
   Mfp_CVTDW, Mfp_CEILWS, Mfp_CEILWD, Mfp_CEILLS, Mfp_CEILLD, Mfp_CVTDS,
   Mfp_ROUNDLD, Mfp_FLOORLD, Mfp_RINTS, Mfp_RINTD,

   /* FP compare */
   Mfp_CMP_UN, Mfp_CMP_EQ, Mfp_CMP_LT, Mfp_CMP_NGT,

   Mfp_CMP_UN_S, Mfp_CMP_EQ_S, Mfp_CMP_LT_S, Mfp_CMP_NGT_S,

   /*MAX and MIN*/
   Mfp_MAXS, Mfp_MAXD, Mfp_MINS, Mfp_MIND

} MIPSFpOp;

extern const HChar *showMIPSFpOp(MIPSFpOp);

typedef enum {
   Rotx32,
   Rotx64
} MIPSRotxOp;

extern const HChar *showRotxOp(MIPSRotxOp);

/* Move from/to fpr to/from gpr */
typedef enum {
   MFpGpMove_mfc1,   /* Move Word From Floating Point - MIPS32 */
   MFpGpMove_dmfc1,  /* Doubleword Move from Floating Point - MIPS64 */
   MFpGpMove_mtc1,   /* Move Word to Floating Point - MIPS32 */
   MFpGpMove_dmtc1   /* Doubleword Move to Floating Point - MIPS64 */
} MIPSFpGpMoveOp;

extern const HChar *showMIPSFpGpMoveOp ( MIPSFpGpMoveOp );

/* Move Conditional */
typedef enum {
   MFpMoveCond_movns,  /* FP Move Conditional on Not Zero - MIPS32 */
   MFpMoveCond_movnd,
   MMoveCond_movn,      /* Move Conditional on Not Zero */
   MSeleqz, /* r6 instructions */
   MSelnez,
   MFpSels,
   MFpSeld
} MIPSMoveCondOp;

extern const HChar *showMIPSMoveCondOp ( MIPSMoveCondOp );

/*--------- Structure for instructions ----------*/
/* Destinations are on the LEFT (first operand) */

typedef struct {
   MIPSInstrTag tag;
   union {
      /* Get a 32/64-bit literal into a register.
         May turn into a number of real insns. */
      struct {
         HReg dst;
         ULong imm;
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
         MIPSAluOp op;
         HReg dst;
         HReg srcL;
         MIPSRH *srcR;
      } Alu;
      /* Integer shl/shr/sar.
         Limitations: the immediate, if it exists,
         is a signed 5-bit value between 1 and 31 inclusive.
       */
      struct {
         MIPSShftOp op;
         Bool sz32;  /* mode64 has both 32 and 64bit shft */
         HReg dst;
         HReg srcL;
         MIPSRH *srcR;
      } Shft;
      struct {
         MIPSRotxOp op;
         HReg rd;
         HReg rt;
         HReg shift;
         HReg shiftx;
         HReg stripe;
      } Rotx;
      /* Clz, Clo, nop */
      struct {
         MIPSUnaryOp op;
         HReg dst;
         HReg src;
      } Unary;
      /* Bit extract */
      struct {
         HReg dst;
         HReg src;
         UInt pos;
         UInt size;
      } Ext;
      /* Word compare. Fake instruction, used for basic block ending */
      struct {
         Bool syned;
         Bool sz32;
         HReg dst;
         HReg srcL;
         HReg srcR;

         MIPSCondCode cond;
      } Cmp;
      struct {
         Bool widening;  /* True => widening, False => non-widening */
         Bool syned;     /* signed/unsigned - meaningless if widenind = False */
         Bool sz32;
         HReg dst;
         HReg srcL;
         HReg srcR;
      } Mul;
      struct {
         Bool syned;     /* signed/unsigned */
         HReg srcL;
         HReg srcR;
      } Mult;
      struct {
         Bool syned;     /* signed/unsigned - meaningless if widenind = False */
         Bool sz32;
         Bool low;
         HReg dst;
         HReg srcL;
         HReg srcR;
      } Mulr6;
      struct {
         Bool syned;  /* signed/unsigned - meaningless if widenind = False */
         Bool sz32;
         HReg srcL;
         HReg srcR;
      } Div;
      struct {
         Bool syned;     /* signed/unsigned - meaningless if widenind = False */
         Bool sz32;
         Bool mod;
         HReg dst;
         HReg srcL;
         HReg srcR;
      } Divr6;
      /* Pseudo-insn.  Call target (an absolute address), on given
         condition (which could be Mcc_ALWAYS).  argiregs indicates
         which of $4 .. $7 (mips32) or $4 .. $11 (mips64)
         carries argument values for this call,
         using a bit mask (1<<N is set if $N holds an arg, for N in
         $4 .. $7 or $4 .. $11 inclusive). 
         If cond is != Mcc_ALWAYS, src is checked.
         Otherwise, unconditional call */
      struct {
         MIPSCondCode cond;
         Addr64 target;
         UInt argiregs;
         HReg src;
         RetLoc rloc;     /* where the return value will be */
      } Call;
      /* Update the guest EIP value, then exit requesting to chain
         to it.  May be conditional.  Urr, use of Addr32 implicitly
         assumes that wordsize(guest) == wordsize(host). */
      struct {
         Addr64       dstGA;     /* next guest address */
         MIPSAMode*   amPC;      /* amode in guest state for PC */
         MIPSCondCode cond;      /* can be MIPScc_AL */
         Bool         toFastEP;  /* chain to the slow or fast point? */
      } XDirect;
      /* Boring transfer to a guest address not known at JIT time.
         Not chainable.  May be conditional. */
      struct {
         HReg        dstGA;
         MIPSAMode*   amPC;
         MIPSCondCode cond; /* can be MIPScc_AL */
      } XIndir;
      /* Assisted transfer to a guest address, most general case.
         Not chainable.  May be conditional. */
      struct {
         HReg        dstGA;
         MIPSAMode*   amPC;
         MIPSCondCode cond; /* can be MIPScc_AL */
         IRJumpKind  jk;
      } XAssisted;
      /* Zero extending loads.  Dst size is host word size */
      struct {
         UChar sz;   /* 1|2|4|8 */
         HReg dst;
         MIPSAMode *src;
      } Load;
      struct {
         HReg data;
         HReg addr;
      } MsaLoad;
      /* 64/32/16/8 bit stores */
      struct {
         UChar sz;   /* 1|2|4|8 */
         MIPSAMode *dst;
         HReg src;
      } Store;
      struct {
         UChar sz;   /* 4|8 */
         HReg dst;
         MIPSAMode *src;
      } LoadL;
      struct {
         UChar sz;   /* 4|8 */
         HReg  old;
         HReg  addr;
         HReg  expd;
         HReg  data;
      } Cas;
      struct {
         UChar sz;   /* 4|8 */
         MIPSAMode *dst;
         HReg src;
      } StoreC;
      /* Move from HI/LO register to GP register. */
      struct {
         HReg dst;
      } MfHL;

      /* Move to HI/LO register from GP register. */
      struct {
         HReg src;
      } MtHL;

      /* Read/Write Link Register */
      struct {
         Bool wrLR;
         HReg gpr;
      } RdWrLR;

      /* MIPS Multiply and accumulate instructions. */
      struct {
         MIPSMaccOp op;
         Bool syned;

         HReg srcL;
         HReg srcR;
      } Macc;

      /* MIPS Floating point */
      struct {
         MIPSFpOp op;
         HReg dst;
         HReg src;
      } FpUnary;
      struct {
         MIPSFpOp op;
         HReg dst;
         HReg srcL;
         HReg srcR;
      } FpBinary;
      struct {
         MIPSFpOp op;
         HReg dst;
         HReg src1;
         HReg src2;
         HReg src3;
      } FpTernary;
      struct {
         MIPSFpOp op;
         HReg dst;
         HReg srcML;
         HReg srcMR;
         HReg srcAcc;
      } FpMulAcc;
      struct {
         Bool isLoad;
         UChar sz;   /* only 4 (IEEE single) or 8 (IEEE double) */
         HReg reg;
         MIPSAMode *addr;
      } FpLdSt;

      struct {
         MIPSFpOp op;
         HReg dst;
         HReg src;
      } FpConvert;
      struct {
         MIPSFpOp op;
         HReg dst;
         HReg srcL;
         HReg srcR;
         UChar cond1;
      } FpCompare;
      struct {
         MIPSFpOp op;
         HReg dst;
         HReg srcL;
         HReg srcR;
      } FpMinMax;
      /* Move from GP register to FCSR register. */
      struct {
         HReg src;
      } MtFCSR;
      /* Move from FCSR register to GP register. */
      struct {
         HReg dst;
      } MfFCSR;
      struct {
         MIPSAMode* amCounter;
         MIPSAMode* amFailAddr;
      } EvCheck;
      struct {
         /* No fields.  The address of the counter to inc is
            installed later, post-translation, by patching it in,
            as it is not known at translation time. */
      } ProfInc;

      /* Move from/to fpr to/from gpr */
      struct {
         MIPSFpGpMoveOp op;
         HReg dst;
         HReg src;
      } FpGpMove;
      struct {
         MIPSMoveCondOp op;
         HReg dst;
         HReg src;
         HReg cond;
      } MoveCond;
      struct {
         MSAMI10Op op;
         UInt s10;
         HReg rs;
         HReg wd;
         MSADF df;
      } MsaMi10;
      struct {
         MSAELMOp op;
         HReg ws;
         HReg wd;
         UInt dfn;
      } MsaElm;
      struct {
         MSA2ROp op;
         MSADF df;
         HReg ws;
         HReg wd;
      } Msa2R;
      struct {
         MSA3ROp op;
         MSADF df;
         HReg wt;
         HReg ws;
         HReg wd;
      } Msa3R;
      struct {
         MSAVECOp op;
         HReg wt;
         HReg ws;
         HReg wd;
      } MsaVec;
      struct {
         MSABITOp op;
         MSADF df;
         UChar ms;
         HReg ws;
         HReg wd;
      }MsaBit;
      struct {
         MSA3RFOp op;
         MSADFFlx df;
         HReg wt;
         HReg ws;
         HReg wd;
      } Msa3RF;
      struct {
         MSA2RFOp op;
         MSADFFlx df;
         HReg ws;
         HReg wd;
      } Msa2RF;

   } Min;
} MIPSInstr;

extern MIPSInstr *MIPSInstr_LI(HReg, ULong);
extern MIPSInstr *MIPSInstr_Alu(MIPSAluOp, HReg, HReg, MIPSRH *);
extern MIPSInstr *MIPSInstr_Shft(MIPSShftOp, Bool sz32, HReg, HReg, MIPSRH *);
extern MIPSInstr *MIPSInstr_Unary(MIPSUnaryOp op, HReg dst, HReg src);
extern MIPSInstr *MIPSInstr_Ext(HReg, HReg, UInt, UInt);
extern MIPSInstr *MIPSInstr_Cmp(Bool, Bool, HReg, HReg, HReg, MIPSCondCode);
extern MIPSInstr *MIPSInstr_Mul(HReg, HReg, HReg);
extern MIPSInstr *MIPSInstr_Mult(Bool, HReg, HReg);
extern MIPSInstr *MIPSInstr_Mulr6(Bool syned, Bool sz32, Bool low,
                                  HReg, HReg, HReg);
extern MIPSInstr *MIPSInstr_Div(Bool syned, Bool sz32, HReg, HReg);
extern MIPSInstr *MIPSInstr_Divr6(Bool syned, Bool sz32, Bool mod,
                                  HReg, HReg, HReg);
extern MIPSInstr *MIPSInstr_Madd(Bool, HReg, HReg);
extern MIPSInstr *MIPSInstr_Msub(Bool, HReg, HReg);

extern MIPSInstr *MIPSInstr_Load(UChar sz, HReg dst, MIPSAMode * src,
                                 Bool mode64);
extern MIPSInstr *MIPSInstr_Store(UChar sz, MIPSAMode * dst, HReg src,
                                  Bool mode64);

extern MIPSInstr *MIPSInstr_LoadL(UChar sz, HReg dst, MIPSAMode * src,
                                  Bool mode64);
extern MIPSInstr *MIPSInstr_StoreC(UChar sz, MIPSAMode * dst, HReg src,
                                   Bool mode64);
extern MIPSInstr *MIPSInstr_Cas(UChar sz, HReg old, HReg addr,
                                HReg expd, HReg data, Bool mode64);

extern MIPSInstr *MIPSInstr_Call ( MIPSCondCode, Addr64, UInt, HReg, RetLoc );
extern MIPSInstr *MIPSInstr_CallAlways ( MIPSCondCode, Addr64, UInt, RetLoc );

extern MIPSInstr *MIPSInstr_XDirect ( Addr64 dstGA, MIPSAMode* amPC,
                                      MIPSCondCode cond, Bool toFastEP );
extern MIPSInstr *MIPSInstr_XIndir(HReg dstGA, MIPSAMode* amPC,
                                     MIPSCondCode cond);
extern MIPSInstr *MIPSInstr_XAssisted(HReg dstGA, MIPSAMode* amPC,
                                      MIPSCondCode cond, IRJumpKind jk);

extern MIPSInstr *MIPSInstr_FpUnary(MIPSFpOp op, HReg dst, HReg src);
extern MIPSInstr *MIPSInstr_FpBinary(MIPSFpOp op, HReg dst, HReg srcL,
                                     HReg srcR);
extern MIPSInstr *MIPSInstr_FpTernary ( MIPSFpOp op, HReg dst, HReg src1,
                                        HReg src2, HReg src3 );
extern MIPSInstr *MIPSInstr_FpConvert(MIPSFpOp op, HReg dst, HReg src);
extern MIPSInstr *MIPSInstr_FpCompare(MIPSFpOp op, HReg dst, HReg srcL,
                                      HReg srcR);
extern MIPSInstr *MIPSInstr_FpMinMax(MIPSFpOp op, HReg dst, HReg srcL,
                                      HReg srcR);
extern MIPSInstr *MIPSInstr_FpMulAcc(MIPSFpOp op, HReg dst, HReg srcML,
                                     HReg srcMR, HReg srcAcc);
extern MIPSInstr *MIPSInstr_FpLdSt(Bool isLoad, UChar sz, HReg, MIPSAMode *);
extern MIPSInstr *MIPSInstr_FpSTFIW(HReg addr, HReg data);
extern MIPSInstr *MIPSInstr_FpRSP(HReg dst, HReg src);
extern MIPSInstr *MIPSInstr_FpCftI(Bool fromI, Bool int32, HReg dst, HReg src);
extern MIPSInstr *MIPSInstr_FpCMov(MIPSCondCode, HReg dst, HReg src);
extern MIPSInstr *MIPSInstr_MtFCSR(HReg src);
extern MIPSInstr *MIPSInstr_MfFCSR(HReg dst);
extern MIPSInstr *MIPSInstr_FpCmp(HReg dst, HReg srcL, HReg srcR);

extern MIPSInstr *MIPSInstr_Mfhi(HReg dst);
extern MIPSInstr *MIPSInstr_Mflo(HReg dst);
extern MIPSInstr *MIPSInstr_Mthi(HReg src);
extern MIPSInstr *MIPSInstr_Mtlo(HReg src);

extern MIPSInstr *MIPSInstr_RdWrLR(Bool wrLR, HReg gpr);

extern MIPSInstr *MIPSInstr_MoveCond ( MIPSMoveCondOp op, HReg dst,
                                       HReg src, HReg cond );

extern MIPSInstr *MIPSInstr_FpGpMove ( MIPSFpGpMoveOp op, HReg dst, HReg src );

extern MIPSInstr *MIPSInstr_EvCheck(MIPSAMode* amCounter,
                                    MIPSAMode* amFailAddr );
extern MIPSInstr *MIPSInstr_ProfInc( void );

extern MIPSInstr* MIPSInstr_MsaMi10(MSAMI10Op op, UInt s10, HReg rs, HReg wd, MSADF df);
extern MIPSInstr* MIPSInstr_MsaElm(MSAELMOp op, HReg ws, HReg wd, UInt dfn);
extern MIPSInstr* MIPSInstr_Msa3R(MSA3ROp op, MSADF df, HReg wd, HReg ws, HReg wt);
extern MIPSInstr* MIPSInstr_Msa2R(MSA2ROp op, MSADF df, HReg ws, HReg wd);
extern MIPSInstr* MIPSInstr_MsaVec(MSAVECOp op, HReg wt, HReg ws, HReg wd);
extern MIPSInstr* MIPSInstr_MsaBit(MSABITOp op, MSADF df, UChar ms, HReg ws, HReg wd);
extern MIPSInstr* MIPSInstr_Msa3RF(MSA3RFOp op, MSADFFlx df, HReg wd, HReg ws, HReg wt);
extern MIPSInstr* MIPSInstr_Msa2RF(MSA2RFOp op, MSADFFlx df, HReg wd, HReg ws);

extern MIPSInstr* MIPSInstr_Bitswap(MIPSRotxOp, HReg, HReg, HReg, HReg, HReg);

extern void ppMIPSInstr(const MIPSInstr *, Bool mode64);

/* Some functions that insulate the register allocator from details
   of the underlying instruction set. */
extern void getRegUsage_MIPSInstr (HRegUsage *, const MIPSInstr *, Bool);
extern void mapRegs_MIPSInstr     (HRegRemap *, MIPSInstr *, Bool mode64);
extern Int        emit_MIPSInstr (/*MB_MOD*/Bool* is_profInc,
                                  UChar* buf, Int nbuf, const MIPSInstr* i,
                                  Bool mode64,
                                  VexEndness endness_host,
                                  const void* disp_cp_chain_me_to_slowEP,
                                  const void* disp_cp_chain_me_to_fastEP,
                                  const void* disp_cp_xindir,
                                  const void* disp_cp_xassisted );

extern void genSpill_MIPS ( /*OUT*/ HInstr ** i1, /*OUT*/ HInstr ** i2,
                            HReg rreg, Int offset, Bool);
extern void genReload_MIPS( /*OUT*/ HInstr ** i1, /*OUT*/ HInstr ** i2,
                            HReg rreg, Int offset, Bool);
extern MIPSInstr* genMove_MIPS(HReg from, HReg to, Bool mode64);

extern const RRegUniverse* getRRegUniverse_MIPS ( Bool mode64 );

extern HInstrArray *iselSB_MIPS          ( const IRSB*,
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
extern Int evCheckSzB_MIPS (void);

/* Perform a chaining and unchaining of an XDirect jump. */
extern VexInvalRange chainXDirect_MIPS ( VexEndness endness_host,
                                         void* place_to_chain,
                                         const void* disp_cp_chain_me_EXPECTED,
                                         const void* place_to_jump_to,
                                         Bool  mode64 );

extern VexInvalRange unchainXDirect_MIPS ( VexEndness endness_host,
                                           void* place_to_unchain,
                                           const void* place_to_jump_to_EXPECTED,
                                           const void* disp_cp_chain_me,
                                           Bool  mode64 );

/* Patch the counter location into an existing ProfInc point. */
extern VexInvalRange patchProfInc_MIPS ( VexEndness endness_host,
                                         void*  place_to_patch,
                                         const ULong* location_of_counter,
                                         Bool  mode64 );


#endif /* ndef __VEX_HOST_MIPS_DEFS_H */

/*---------------------------------------------------------------*/
/*--- end                                    host-mips_defs.h ---*/
/*---------------------------------------------------------------*/
