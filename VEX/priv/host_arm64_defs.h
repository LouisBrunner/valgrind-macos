
/*---------------------------------------------------------------*/
/*--- begin                                 host_arm64_defs.h ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2013-2015 OpenWorks
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
*/

#ifndef __VEX_HOST_ARM64_DEFS_H
#define __VEX_HOST_ARM64_DEFS_H

#include "libvex_basictypes.h"
#include "libvex.h"                      // VexArch
#include "host_generic_regs.h"           // HReg


/* --------- Registers. --------- */

#define ST_IN static inline
ST_IN HReg hregARM64_X22 ( void ) { return mkHReg(False, HRcInt64,  22,  0); }
ST_IN HReg hregARM64_X23 ( void ) { return mkHReg(False, HRcInt64,  23,  1); }
ST_IN HReg hregARM64_X24 ( void ) { return mkHReg(False, HRcInt64,  24,  2); }
ST_IN HReg hregARM64_X25 ( void ) { return mkHReg(False, HRcInt64,  25,  3); }
ST_IN HReg hregARM64_X26 ( void ) { return mkHReg(False, HRcInt64,  26,  4); }
ST_IN HReg hregARM64_X27 ( void ) { return mkHReg(False, HRcInt64,  27,  5); }
ST_IN HReg hregARM64_X28 ( void ) { return mkHReg(False, HRcInt64,  28,  6); }

ST_IN HReg hregARM64_X0  ( void ) { return mkHReg(False, HRcInt64,  0,   7); }
ST_IN HReg hregARM64_X1  ( void ) { return mkHReg(False, HRcInt64,  1,   8); }
ST_IN HReg hregARM64_X2  ( void ) { return mkHReg(False, HRcInt64,  2,   9); }
ST_IN HReg hregARM64_X3  ( void ) { return mkHReg(False, HRcInt64,  3,  10); }
ST_IN HReg hregARM64_X4  ( void ) { return mkHReg(False, HRcInt64,  4,  11); }
ST_IN HReg hregARM64_X5  ( void ) { return mkHReg(False, HRcInt64,  5,  12); }
ST_IN HReg hregARM64_X6  ( void ) { return mkHReg(False, HRcInt64,  6,  13); }
ST_IN HReg hregARM64_X7  ( void ) { return mkHReg(False, HRcInt64,  7,  14); }

ST_IN HReg hregARM64_Q16 ( void ) { return mkHReg(False, HRcVec128, 16, 15); }
ST_IN HReg hregARM64_Q17 ( void ) { return mkHReg(False, HRcVec128, 17, 16); }
ST_IN HReg hregARM64_Q18 ( void ) { return mkHReg(False, HRcVec128, 18, 17); }
ST_IN HReg hregARM64_Q19 ( void ) { return mkHReg(False, HRcVec128, 19, 18); }
ST_IN HReg hregARM64_Q20 ( void ) { return mkHReg(False, HRcVec128, 20, 19); }

ST_IN HReg hregARM64_D8  ( void ) { return mkHReg(False, HRcFlt64,  8,  20); }
ST_IN HReg hregARM64_D9  ( void ) { return mkHReg(False, HRcFlt64,  9,  21); }
ST_IN HReg hregARM64_D10 ( void ) { return mkHReg(False, HRcFlt64,  10, 22); }
ST_IN HReg hregARM64_D11 ( void ) { return mkHReg(False, HRcFlt64,  11, 23); }
ST_IN HReg hregARM64_D12 ( void ) { return mkHReg(False, HRcFlt64,  12, 24); }
ST_IN HReg hregARM64_D13 ( void ) { return mkHReg(False, HRcFlt64,  13, 25); }

ST_IN HReg hregARM64_X8  ( void ) { return mkHReg(False, HRcInt64,  8,  26); }
ST_IN HReg hregARM64_X9  ( void ) { return mkHReg(False, HRcInt64,  9,  27); }
ST_IN HReg hregARM64_X21 ( void ) { return mkHReg(False, HRcInt64, 21,  28); }
#undef ST_IN

extern void ppHRegARM64 ( HReg );

/* Number of registers used arg passing in function calls */
#define ARM64_N_ARGREGS 8   /* x0 .. x7 */


/* --------- Condition codes. --------- */

typedef
   enum {
      ARM64cc_EQ  = 0,  /* equal                         : Z=1 */
      ARM64cc_NE  = 1,  /* not equal                     : Z=0 */

      ARM64cc_CS  = 2,  /* >=u (higher or same)          : C=1 */
      ARM64cc_CC  = 3,  /* <u  (lower)                   : C=0 */

      ARM64cc_MI  = 4,  /* minus (negative)              : N=1 */
      ARM64cc_PL  = 5,  /* plus (zero or +ve)            : N=0 */

      ARM64cc_VS  = 6,  /* overflow                      : V=1 */
      ARM64cc_VC  = 7,  /* no overflow                   : V=0 */

      ARM64cc_HI  = 8,  /* >u   (higher)                 :   C=1 && Z=0 */
      ARM64cc_LS  = 9,  /* <=u  (lower or same)          : !(C=1 && Z=0) */

      ARM64cc_GE  = 10, /* >=s (signed greater or equal) :   N=V */
      ARM64cc_LT  = 11, /* <s  (signed less than)        : !(N=V) */

      ARM64cc_GT  = 12, /* >s  (signed greater)          :   Z=0 && N=V */
      ARM64cc_LE  = 13, /* <=s (signed less or equal)    : !(Z=0 && N=V) */

      ARM64cc_AL  = 14, /* always (unconditional) */
      ARM64cc_NV  = 15  /* in 64-bit mode also means "always" */
   }
   ARM64CondCode;


/* --------- Memory address expressions (amodes). --------- */

typedef
   enum {
      ARM64am_RI9=10, /* reg + simm9 */
      ARM64am_RI12,   /* reg + uimm12 * szB (iow, scaled by access size) */
      ARM64am_RR      /* reg1 + reg2 */
   }
   ARM64AModeTag;

typedef
   struct {
      ARM64AModeTag tag;
      union {
         struct {
            HReg reg;
            Int  simm9; /* -256 .. +255 */
         } RI9;
         struct {
            HReg  reg;
            UInt  uimm12; /* 0 .. 4095 */
            UChar szB;    /* 1, 2, 4, 8 (16 ?) */
         } RI12;
         struct {
            HReg base;
            HReg index;
         } RR;
      } ARM64am;
   }
   ARM64AMode;

extern ARM64AMode* ARM64AMode_RI9  ( HReg reg, Int simm9 );
extern ARM64AMode* ARM64AMode_RI12 ( HReg reg, Int uimm12, UChar szB );
extern ARM64AMode* ARM64AMode_RR   ( HReg base, HReg index );


/* --------- Reg or uimm12 or (uimm12 << 12) operands --------- */

typedef
   enum {
      ARM64riA_I12=20, /* uimm12 << 0 or 12 only */
      ARM64riA_R       /* reg */
   }
   ARM64RIATag;

typedef
   struct {
      ARM64RIATag tag;
      union {
         struct {
            UShort imm12;  /* 0 .. 4095 */
            UChar  shift;  /* 0 or 12 only */
         } I12;
         struct {
            HReg reg;
         } R;
      } ARM64riA;
   }
   ARM64RIA;

extern ARM64RIA* ARM64RIA_I12 ( UShort imm12, UChar shift );
extern ARM64RIA* ARM64RIA_R   ( HReg );


/* --------- Reg or "bitfield" (logic immediate) operands --------- */

typedef
   enum {
      ARM64riL_I13=6, /* wierd-o bitfield immediate, 13 bits in total */
      ARM64riL_R      /* reg */
   }
   ARM64RILTag;

typedef
   struct {
      ARM64RILTag tag;
      union {
         struct {
            UChar bitN; /* 0 .. 1 */
            UChar immR; /* 0 .. 63 */
            UChar immS; /* 0 .. 63 */
         } I13;
         struct {
            HReg reg;
         } R;
      } ARM64riL;
   }
   ARM64RIL;

extern ARM64RIL* ARM64RIL_I13 ( UChar bitN, UChar immR, UChar immS );
extern ARM64RIL* ARM64RIL_R   ( HReg );


/* --------------- Reg or uimm6 operands --------------- */

typedef
   enum {
      ARM64ri6_I6=30, /* uimm6, 1 .. 63 only */
      ARM64ri6_R      /* reg */
   }
   ARM64RI6Tag;

typedef
   struct {
      ARM64RI6Tag tag;
      union {
         struct {
            UInt imm6;   /* 1 .. 63 */
         } I6;
         struct {
            HReg reg;
         } R;
      } ARM64ri6;
   }
   ARM64RI6;

extern ARM64RI6* ARM64RI6_I6 ( UInt imm6 );
extern ARM64RI6* ARM64RI6_R  ( HReg );


/* --------------------- Instructions --------------------- */

typedef
   enum {
      ARM64lo_AND=40,
      ARM64lo_OR,
      ARM64lo_XOR
   }
   ARM64LogicOp;

typedef
   enum {
      ARM64sh_SHL=50,
      ARM64sh_SHR,
      ARM64sh_SAR
   }
   ARM64ShiftOp;

typedef
   enum {
      ARM64un_NEG=60,
      ARM64un_NOT,
      ARM64un_CLZ,
   }
   ARM64UnaryOp;

typedef
   enum {
      ARM64mul_PLAIN=70, /* lo64(64 * 64)  */
      ARM64mul_ZX,       /* hi64(64 *u 64) */
      ARM64mul_SX        /* hi64(64 *s 64) */
   }
   ARM64MulOp;

typedef
   /* These characterise an integer-FP conversion, but don't imply any
      particular direction. */
   enum {
      ARM64cvt_F32_I32S=80,
      ARM64cvt_F64_I32S,
      ARM64cvt_F32_I64S,
      ARM64cvt_F64_I64S,
      ARM64cvt_F32_I32U,
      ARM64cvt_F64_I32U,
      ARM64cvt_F32_I64U,
      ARM64cvt_F64_I64U,
      ARM64cvt_INVALID
   }
   ARM64CvtOp;

typedef
   enum {
      ARM64fpb_ADD=100,
      ARM64fpb_SUB,
      ARM64fpb_MUL,
      ARM64fpb_DIV,
      ARM64fpb_INVALID
   }
   ARM64FpBinOp;

typedef
   enum {
      ARM64fpu_NEG=110,
      ARM64fpu_ABS,
      ARM64fpu_SQRT,
      ARM64fpu_RINT,
      ARM64fpu_RECPX,
      ARM64fpu_INVALID
   }
   ARM64FpUnaryOp;

typedef
   enum {
      ARM64vecb_ADD64x2=120, ARM64vecb_ADD32x4,
      ARM64vecb_ADD16x8,     ARM64vecb_ADD8x16,
      ARM64vecb_SUB64x2,     ARM64vecb_SUB32x4,
      ARM64vecb_SUB16x8,     ARM64vecb_SUB8x16,
                             ARM64vecb_MUL32x4,
      ARM64vecb_MUL16x8,     ARM64vecb_MUL8x16,
      ARM64vecb_FADD64x2,    ARM64vecb_FADD32x4,
      ARM64vecb_FSUB64x2,    ARM64vecb_FSUB32x4,
      ARM64vecb_FMUL64x2,    ARM64vecb_FMUL32x4,
      ARM64vecb_FDIV64x2,    ARM64vecb_FDIV32x4,
      ARM64vecb_FMAX64x2,    ARM64vecb_FMAX32x4,
      ARM64vecb_FMIN64x2,    ARM64vecb_FMIN32x4,
                             ARM64vecb_UMAX32x4,
      ARM64vecb_UMAX16x8,    ARM64vecb_UMAX8x16,
                             ARM64vecb_UMIN32x4,
      ARM64vecb_UMIN16x8,    ARM64vecb_UMIN8x16,
                             ARM64vecb_SMAX32x4,
      ARM64vecb_SMAX16x8,    ARM64vecb_SMAX8x16,
                             ARM64vecb_SMIN32x4,
      ARM64vecb_SMIN16x8,    ARM64vecb_SMIN8x16,
      ARM64vecb_AND,
      ARM64vecb_ORR,
      ARM64vecb_XOR,
      ARM64vecb_CMEQ64x2,    ARM64vecb_CMEQ32x4,
      ARM64vecb_CMEQ16x8,    ARM64vecb_CMEQ8x16,
      ARM64vecb_CMHI64x2,    ARM64vecb_CMHI32x4, /* >u */
      ARM64vecb_CMHI16x8,    ARM64vecb_CMHI8x16,
      ARM64vecb_CMGT64x2,    ARM64vecb_CMGT32x4, /* >s */
      ARM64vecb_CMGT16x8,    ARM64vecb_CMGT8x16,
      ARM64vecb_FCMEQ64x2,   ARM64vecb_FCMEQ32x4,
      ARM64vecb_FCMGE64x2,   ARM64vecb_FCMGE32x4,
      ARM64vecb_FCMGT64x2,   ARM64vecb_FCMGT32x4,
      ARM64vecb_TBL1,
      ARM64vecb_UZP164x2,    ARM64vecb_UZP132x4,
      ARM64vecb_UZP116x8,    ARM64vecb_UZP18x16,
      ARM64vecb_UZP264x2,    ARM64vecb_UZP232x4,
      ARM64vecb_UZP216x8,    ARM64vecb_UZP28x16,
      ARM64vecb_ZIP132x4,    ARM64vecb_ZIP116x8,
      ARM64vecb_ZIP18x16,    ARM64vecb_ZIP232x4,
      ARM64vecb_ZIP216x8,    ARM64vecb_ZIP28x16,
                             ARM64vecb_PMUL8x16,
                             ARM64vecb_PMULL8x8,
                             ARM64vecb_UMULL2DSS,
      ARM64vecb_UMULL4SHH,   ARM64vecb_UMULL8HBB,
                             ARM64vecb_SMULL2DSS,
      ARM64vecb_SMULL4SHH,   ARM64vecb_SMULL8HBB,
      ARM64vecb_SQADD64x2,   ARM64vecb_SQADD32x4,
      ARM64vecb_SQADD16x8,   ARM64vecb_SQADD8x16,
      ARM64vecb_UQADD64x2,   ARM64vecb_UQADD32x4,
      ARM64vecb_UQADD16x8,   ARM64vecb_UQADD8x16,
      ARM64vecb_SQSUB64x2,   ARM64vecb_SQSUB32x4,
      ARM64vecb_SQSUB16x8,   ARM64vecb_SQSUB8x16,
      ARM64vecb_UQSUB64x2,   ARM64vecb_UQSUB32x4,
      ARM64vecb_UQSUB16x8,   ARM64vecb_UQSUB8x16,
                             ARM64vecb_SQDMULL2DSS,
      ARM64vecb_SQDMULL4SHH,
                             ARM64vecb_SQDMULH32x4,
      ARM64vecb_SQDMULH16x8,
                             ARM64vecb_SQRDMULH32x4,
      ARM64vecb_SQRDMULH16x8,
      ARM64vecb_SQSHL64x2,   ARM64vecb_SQSHL32x4,
      ARM64vecb_SQSHL16x8,   ARM64vecb_SQSHL8x16,
      ARM64vecb_UQSHL64x2,   ARM64vecb_UQSHL32x4,
      ARM64vecb_UQSHL16x8,   ARM64vecb_UQSHL8x16,
      ARM64vecb_SQRSHL64x2,  ARM64vecb_SQRSHL32x4,
      ARM64vecb_SQRSHL16x8,  ARM64vecb_SQRSHL8x16,
      ARM64vecb_UQRSHL64x2,  ARM64vecb_UQRSHL32x4,
      ARM64vecb_UQRSHL16x8,  ARM64vecb_UQRSHL8x16,
      ARM64vecb_SSHL64x2,    ARM64vecb_SSHL32x4,
      ARM64vecb_SSHL16x8,    ARM64vecb_SSHL8x16, 
      ARM64vecb_USHL64x2,    ARM64vecb_USHL32x4,
      ARM64vecb_USHL16x8,    ARM64vecb_USHL8x16, 
      ARM64vecb_SRSHL64x2,   ARM64vecb_SRSHL32x4,
      ARM64vecb_SRSHL16x8,   ARM64vecb_SRSHL8x16, 
      ARM64vecb_URSHL64x2,   ARM64vecb_URSHL32x4,
      ARM64vecb_URSHL16x8,   ARM64vecb_URSHL8x16, 
      ARM64vecb_FRECPS64x2,  ARM64vecb_FRECPS32x4,
      ARM64vecb_FRSQRTS64x2, ARM64vecb_FRSQRTS32x4,
      ARM64vecb_INVALID
   }
   ARM64VecBinOp;

typedef
   enum {
      ARM64vecmo_SUQADD64x2=300, ARM64vecmo_SUQADD32x4,
      ARM64vecmo_SUQADD16x8,     ARM64vecmo_SUQADD8x16,
      ARM64vecmo_USQADD64x2,     ARM64vecmo_USQADD32x4,
      ARM64vecmo_USQADD16x8,     ARM64vecmo_USQADD8x16,
      ARM64vecmo_INVALID
   }
   ARM64VecModifyOp;

typedef
   enum {
      ARM64vecu_FNEG64x2=350, ARM64vecu_FNEG32x4,
      ARM64vecu_FABS64x2,     ARM64vecu_FABS32x4,
      ARM64vecu_NOT,
      ARM64vecu_ABS64x2,      ARM64vecu_ABS32x4,
      ARM64vecu_ABS16x8,      ARM64vecu_ABS8x16,
      ARM64vecu_CLS32x4,      ARM64vecu_CLS16x8,      ARM64vecu_CLS8x16, 
      ARM64vecu_CLZ32x4,      ARM64vecu_CLZ16x8,      ARM64vecu_CLZ8x16, 
      ARM64vecu_CNT8x16,
      ARM64vecu_RBIT,
      ARM64vecu_REV1616B,
      ARM64vecu_REV3216B,     ARM64vecu_REV328H,
      ARM64vecu_REV6416B,     ARM64vecu_REV648H,      ARM64vecu_REV644S,
      ARM64vecu_URECPE32x4,
      ARM64vecu_URSQRTE32x4,
      ARM64vecu_FRECPE64x2,   ARM64vecu_FRECPE32x4,
      ARM64vecu_FRSQRTE64x2,  ARM64vecu_FRSQRTE32x4,
      ARM64vecu_FSQRT64x2,    ARM64vecu_FSQRT32x4,
      ARM64vecu_INVALID
   }
   ARM64VecUnaryOp;

typedef
   enum {
      ARM64vecshi_USHR64x2=400, ARM64vecshi_USHR32x4,
      ARM64vecshi_USHR16x8,     ARM64vecshi_USHR8x16,
      ARM64vecshi_SSHR64x2,     ARM64vecshi_SSHR32x4,
      ARM64vecshi_SSHR16x8,     ARM64vecshi_SSHR8x16,
      ARM64vecshi_SHL64x2,      ARM64vecshi_SHL32x4,
      ARM64vecshi_SHL16x8,      ARM64vecshi_SHL8x16,
      /* These narrowing shifts zero out the top half of the destination
         register. */
      ARM64vecshi_SQSHRN2SD,    ARM64vecshi_SQSHRN4HS,   ARM64vecshi_SQSHRN8BH,
      ARM64vecshi_UQSHRN2SD,    ARM64vecshi_UQSHRN4HS,   ARM64vecshi_UQSHRN8BH,
      ARM64vecshi_SQSHRUN2SD,   ARM64vecshi_SQSHRUN4HS,  ARM64vecshi_SQSHRUN8BH,
      ARM64vecshi_SQRSHRN2SD,   ARM64vecshi_SQRSHRN4HS,  ARM64vecshi_SQRSHRN8BH,
      ARM64vecshi_UQRSHRN2SD,   ARM64vecshi_UQRSHRN4HS,  ARM64vecshi_UQRSHRN8BH,
      ARM64vecshi_SQRSHRUN2SD,  ARM64vecshi_SQRSHRUN4HS, ARM64vecshi_SQRSHRUN8BH,
      /* Saturating left shifts, of various flavours. */
      ARM64vecshi_UQSHL64x2,    ARM64vecshi_UQSHL32x4,
      ARM64vecshi_UQSHL16x8,    ARM64vecshi_UQSHL8x16, 
      ARM64vecshi_SQSHL64x2,    ARM64vecshi_SQSHL32x4,
      ARM64vecshi_SQSHL16x8,    ARM64vecshi_SQSHL8x16, 
      ARM64vecshi_SQSHLU64x2,   ARM64vecshi_SQSHLU32x4,
      ARM64vecshi_SQSHLU16x8,   ARM64vecshi_SQSHLU8x16, 
      ARM64vecshi_INVALID
   }
   ARM64VecShiftImmOp;

typedef
   enum {
      ARM64vecna_XTN=450,
      ARM64vecna_SQXTN,
      ARM64vecna_UQXTN,
      ARM64vecna_SQXTUN,
      ARM64vecna_INVALID
   }
   ARM64VecNarrowOp;

typedef
   enum {
      /* baseline */
      ARM64in_Arith=1220,
      ARM64in_Cmp,
      ARM64in_Logic,
      ARM64in_Test,
      ARM64in_Shift,
      ARM64in_Unary,
      ARM64in_MovI,        /* int reg-reg move */
      ARM64in_Imm64,
      ARM64in_LdSt64,
      ARM64in_LdSt32,      /* w/ ZX loads */
      ARM64in_LdSt16,      /* w/ ZX loads */
      ARM64in_LdSt8,       /* w/ ZX loads */
      ARM64in_XDirect,     /* direct transfer to GA */
      ARM64in_XIndir,      /* indirect transfer to GA */
      ARM64in_XAssisted,   /* assisted transfer to GA */
      ARM64in_CSel,
      ARM64in_Call,
      ARM64in_AddToSP,     /* move SP by small, signed constant */
      ARM64in_FromSP,      /* move SP to integer register */
      ARM64in_Mul,
      ARM64in_LdrEX,
      ARM64in_StrEX,
      ARM64in_MFence,
      /* ARM64in_V*: scalar ops involving vector registers */
      ARM64in_VLdStH,   /* ld/st to/from low 16 bits of vec reg, imm offset */
      ARM64in_VLdStS,   /* ld/st to/from low 32 bits of vec reg, imm offset */
      ARM64in_VLdStD,   /* ld/st to/from low 64 bits of vec reg, imm offset */
      ARM64in_VLdStQ,   /* ld/st to/from all 128 bits of vec reg, no offset */
      ARM64in_VCvtI2F,
      ARM64in_VCvtF2I,
      ARM64in_VCvtSD,   /* scalar 32 bit FP <--> 64 bit FP */
      ARM64in_VCvtHS,   /* scalar 16 bit FP <--> 32 bit FP */
      ARM64in_VCvtHD,   /* scalar 16 bit FP <--> 64 bit FP */
      ARM64in_VUnaryD,
      ARM64in_VUnaryS,
      ARM64in_VBinD,
      ARM64in_VBinS,
      ARM64in_VCmpD,
      ARM64in_VCmpS,
      ARM64in_VFCSel,
      ARM64in_FPCR,
      ARM64in_FPSR,
      /* ARM64in_V*V: vector ops on vector registers */
      ARM64in_VBinV,
      ARM64in_VModifyV,
      ARM64in_VUnaryV,
      ARM64in_VNarrowV,
      ARM64in_VShiftImmV,
      ARM64in_VExtV,
      ARM64in_VImmQ,
      ARM64in_VDfromX,    /* Move an Xreg to a Dreg */
      ARM64in_VQfromX,    /* Move an Xreg to a Qreg lo64, and zero hi64 */
      ARM64in_VQfromXX,   /* Move 2 Xregs to a Qreg */
      ARM64in_VXfromQ,    /* Move half a Qreg to an Xreg */
      ARM64in_VXfromDorS, /* Move Dreg or Sreg(ZX) to an Xreg */
      ARM64in_VMov,       /* vector reg-reg move, 16, 8 or 4 bytes */
      /* infrastructure */
      ARM64in_EvCheck,    /* Event check */
      ARM64in_ProfInc     /* 64-bit profile counter increment */
   }
   ARM64InstrTag;

/* Destinations are on the LEFT (first operand) */

typedef
   struct {
      ARM64InstrTag tag;
      union {
         /* --- INTEGER INSTRUCTIONS --- */
         /* 64 bit ADD/SUB reg, reg or uimm12<<{0,12} */
         struct {
            HReg      dst;
            HReg      argL;
            ARM64RIA* argR;
            Bool      isAdd;
         } Arith;
         /* 64 or 32 bit CMP reg, reg or aimm (SUB and set flags) */
         struct {
            HReg      argL;
            ARM64RIA* argR;
            Bool      is64;
         } Cmp;
         /* 64 bit AND/OR/XOR reg, reg or bitfield-immediate */
         struct {
            HReg         dst;
            HReg         argL;
            ARM64RIL*    argR;
            ARM64LogicOp op;
         } Logic;
         /* 64 bit TST reg, reg or bimm (AND and set flags) */
         struct {
            HReg      argL;
            ARM64RIL* argR;
         } Test;
         /* 64 bit SHL/SHR/SAR, 2nd arg is reg or imm */
         struct {
            HReg         dst;
            HReg         argL;
            ARM64RI6*    argR;
            ARM64ShiftOp op;
         } Shift;
         /* NOT/NEG/CLZ, 64 bit only */
         struct {
            HReg         dst;
            HReg         src;
            ARM64UnaryOp op;
         } Unary;
         /* MOV dst, src -- reg-reg move for integer registers */
         struct {
            HReg dst;
            HReg src;
         } MovI;
         /* Pseudo-insn; make a 64-bit immediate */
         struct {
            HReg  dst;
            ULong imm64;
         } Imm64;
         /* 64-bit load or store */
         struct {
            Bool        isLoad;
            HReg        rD;
            ARM64AMode* amode;
         } LdSt64;
         /* zx-32-to-64-bit load, or 32-bit store */
         struct {
            Bool        isLoad;
            HReg        rD;
            ARM64AMode* amode;
         } LdSt32;
         /* zx-16-to-64-bit load, or 16-bit store */
         struct {
            Bool        isLoad;
            HReg        rD;
            ARM64AMode* amode;
         } LdSt16;
         /* zx-8-to-64-bit load, or 8-bit store */
         struct {
            Bool        isLoad;
            HReg        rD;
            ARM64AMode* amode;
         } LdSt8;
         /* Update the guest PC value, then exit requesting to chain
            to it.  May be conditional.  Urr, use of Addr64 implicitly
            assumes that wordsize(guest) == wordsize(host). */
         struct {
            Addr64        dstGA;    /* next guest address */
            ARM64AMode*   amPC;     /* amode in guest state for PC */
            ARM64CondCode cond;     /* can be ARM64cc_AL */
            Bool          toFastEP; /* chain to the slow or fast point? */
         } XDirect;
         /* Boring transfer to a guest address not known at JIT time.
            Not chainable.  May be conditional. */
         struct {
            HReg          dstGA;
            ARM64AMode*   amPC;
            ARM64CondCode cond; /* can be ARM64cc_AL */
         } XIndir;
         /* Assisted transfer to a guest address, most general case.
            Not chainable.  May be conditional. */
         struct {
            HReg          dstGA;
            ARM64AMode*   amPC;
            ARM64CondCode cond; /* can be ARM64cc_AL */
            IRJumpKind    jk;
         } XAssisted;
         /* CSEL: dst = if cond then argL else argR.  cond may be anything. */
          struct {
            HReg          dst;
            HReg          argL;
            HReg          argR;
            ARM64CondCode cond;
         } CSel;
         /* Pseudo-insn.  Call target (an absolute address), on given
            condition (which could be ARM64cc_AL). */
         struct {
            RetLoc        rloc;     /* where the return value will be */
            Addr64        target;
            ARM64CondCode cond;
            Int           nArgRegs; /* # regs carrying args: 0 .. 8 */
         } Call;
         /* move SP by small, signed constant */
         struct {
            Int simm; /* needs to be 0 % 16 and in the range -4095
                         .. 4095 inclusive */
         } AddToSP;
         /* move SP to integer register */
         struct {
            HReg dst;
         } FromSP;
         /* Integer multiply, with 3 variants:
              (PLAIN) lo64(64 *  64)
              (ZX)    hi64(64 *u 64)
              (SX)    hi64(64 *s 64)
         */
         struct {
            HReg       dst;
            HReg       argL;
            HReg       argR;
            ARM64MulOp op;
         } Mul;
         /* LDXR{,H,B} x2, [x4] */
         struct {
            Int  szB; /* 1, 2, 4 or 8 */
         } LdrEX;
         /* STXR{,H,B} w0, x2, [x4] */
         struct {
            Int  szB; /* 1, 2, 4 or 8 */
         } StrEX;
         /* Mem fence.  An insn which fences all loads and stores as
            much as possible before continuing.  On ARM64 we emit the
            sequence "dsb sy ; dmb sy ; isb sy", which is probably
            total nuclear overkill, but better safe than sorry. */
         struct {
         } MFence;
         /* --- INSTRUCTIONS INVOLVING VECTOR REGISTERS --- */
         /* ld/st to/from low 16 bits of vec reg, imm offset */
         struct {
            Bool isLoad;
            HReg hD;
            HReg rN;
            UInt uimm12;  /* 0 .. 8190 inclusive, 0 % 2 */
         } VLdStH;
         /* ld/st to/from low 32 bits of vec reg, imm offset */
         struct {
            Bool isLoad;
            HReg sD;
            HReg rN;
            UInt uimm12;  /* 0 .. 16380 inclusive, 0 % 4 */
         } VLdStS;
         /* ld/st to/from low 64 bits of vec reg, imm offset */
         struct {
            Bool isLoad;
            HReg dD;
            HReg rN;
            UInt uimm12;  /* 0 .. 32760 inclusive, 0 % 8 */
         } VLdStD;
         /* ld/st to/from all 128 bits of vec reg, no offset */
         struct {
            Bool isLoad;
            HReg rQ; // data
            HReg rN; // address
         } VLdStQ;
         /* Scalar conversion of int to float. */
         struct {
            ARM64CvtOp how;
            HReg       rD; // dst, a D or S register
            HReg       rS; // src, a W or X register
         } VCvtI2F;
         /* Scalar conversion of float to int, w/ specified RM. */
         struct {
            ARM64CvtOp how;
            HReg       rD; // dst, a W or X register
            HReg       rS; // src, a D or S register
            UChar      armRM; // ARM encoded RM:
                              // 00=nearest, 01=+inf, 10=-inf, 11=zero
         } VCvtF2I;
         /* Convert between 32-bit and 64-bit FP values (both ways). (FCVT) */
         struct {
            Bool sToD; /* True: F32->F64.  False: F64->F32 */
            HReg dst;
            HReg src;
         } VCvtSD;
         /* Convert between 16-bit and 32-bit FP values (both ways). (FCVT) */
         struct {
            Bool hToS; /* True: F16->F32.  False: F32->F16 */
            HReg dst;
            HReg src;
         } VCvtHS;
         /* Convert between 16-bit and 64-bit FP values (both ways). (FCVT) */
         struct {
            Bool hToD; /* True: F16->F64.  False: F64->F16 */
            HReg dst;
            HReg src;
         } VCvtHD;
         /* 64-bit FP unary */
         struct {
            ARM64FpUnaryOp op;
            HReg           dst;
            HReg           src;
         } VUnaryD;
         /* 32-bit FP unary */
         struct {
            ARM64FpUnaryOp op;
            HReg           dst;
            HReg           src;
         } VUnaryS;
         /* 64-bit FP binary arithmetic */
         struct {
            ARM64FpBinOp op;
            HReg         dst;
            HReg         argL;
            HReg         argR;
         } VBinD;
         /* 32-bit FP binary arithmetic */
         struct {
            ARM64FpBinOp op;
            HReg         dst;
            HReg         argL;
            HReg         argR;
         } VBinS;
         /* 64-bit FP compare */
         struct {
            HReg argL;
            HReg argR;
         } VCmpD;
         /* 32-bit FP compare */
         struct {
            HReg argL;
            HReg argR;
         } VCmpS;
         /* 32- or 64-bit FP conditional select */
         struct {
            HReg          dst;
            HReg          argL;
            HReg          argR;
            ARM64CondCode cond;
            Bool          isD;
         }
         VFCSel;
         /* Move a 32-bit value to/from the FPCR */
         struct {
            Bool toFPCR;
            HReg iReg;
         } FPCR;
         /* Move a 32-bit value to/from the FPSR */
         struct {
            Bool toFPSR;
            HReg iReg;
         } FPSR;
         /* binary vector operation on vector registers */
         struct {
            ARM64VecBinOp op;
            HReg          dst;
            HReg          argL;
            HReg          argR;
         } VBinV;
         /* binary vector operation on vector registers.
            Dst reg is also a src. */
         struct {
            ARM64VecModifyOp op;
            HReg             mod;
            HReg             arg;
         } VModifyV;
         /* unary vector operation on vector registers */
         struct {
            ARM64VecUnaryOp op;
            HReg            dst;
            HReg            arg;
         } VUnaryV;
         /* vector narrowing, Q -> Q.  Result goes in the bottom half
            of dst and the top half is zeroed out.  Iow one of the
            XTN family. */
        struct {
           ARM64VecNarrowOp op;
           UInt             dszBlg2; // 0: 16to8_x8  1: 32to16_x4  2: 64to32_x2
           HReg             dst;     // Q reg
           HReg             src;     // Q reg
        } VNarrowV;
        /* Vector shift by immediate.  For left shifts, |amt| must be
           >= 0 and < implied lane size of |op|.  For right shifts,
           |amt| must be > 0 and <= implied lane size of |op|.  Shifts
           beyond these ranges are not allowed. */
        struct {
           ARM64VecShiftImmOp op;
           HReg               dst;
           HReg               src;
           UInt               amt;
        } VShiftImmV;
        struct {
           HReg dst;
           HReg srcLo;
           HReg srcHi;
           UInt amtB;
        } VExtV;
         struct {
            HReg   rQ;
            UShort imm; /* Same 1-bit-per-byte encoding as IR */
         } VImmQ;
         struct {
            HReg rD;
            HReg rX;
         } VDfromX;
         struct {
            HReg rQ;
            HReg rXlo;
         } VQfromX;
         struct {
            HReg rQ;
            HReg rXhi;
            HReg rXlo;
         } VQfromXX;
         struct {
            HReg rX;
            HReg rQ;
            UInt laneNo; /* either 0 or 1 */
         } VXfromQ;
         struct {
            HReg rX;
            HReg rDorS;
            Bool fromD;
         } VXfromDorS;
         /* MOV dst, src -- reg-reg move for vector registers */
         struct {
            UInt szB; // 16=mov qD,qS;  8=mov dD,dS;  4=mov sD,sS
            HReg dst;
            HReg src;
         } VMov;
         struct {
            ARM64AMode* amCounter;
            ARM64AMode* amFailAddr;
         } EvCheck;
         struct {
            /* No fields.  The address of the counter to inc is
               installed later, post-translation, by patching it in,
               as it is not known at translation time. */
         } ProfInc;
      } ARM64in;
   }
   ARM64Instr;


extern ARM64Instr* ARM64Instr_Arith   ( HReg, HReg, ARM64RIA*, Bool isAdd );
extern ARM64Instr* ARM64Instr_Cmp     ( HReg, ARM64RIA*, Bool is64 );
extern ARM64Instr* ARM64Instr_Logic   ( HReg, HReg, ARM64RIL*, ARM64LogicOp );
extern ARM64Instr* ARM64Instr_Test    ( HReg, ARM64RIL* );
extern ARM64Instr* ARM64Instr_Shift   ( HReg, HReg, ARM64RI6*, ARM64ShiftOp );
extern ARM64Instr* ARM64Instr_Unary   ( HReg, HReg, ARM64UnaryOp );
extern ARM64Instr* ARM64Instr_MovI    ( HReg, HReg );
extern ARM64Instr* ARM64Instr_Imm64   ( HReg, ULong );
extern ARM64Instr* ARM64Instr_LdSt64  ( Bool isLoad, HReg, ARM64AMode* );
extern ARM64Instr* ARM64Instr_LdSt32  ( Bool isLoad, HReg, ARM64AMode* );
extern ARM64Instr* ARM64Instr_LdSt16  ( Bool isLoad, HReg, ARM64AMode* );
extern ARM64Instr* ARM64Instr_LdSt8   ( Bool isLoad, HReg, ARM64AMode* );
extern ARM64Instr* ARM64Instr_XDirect ( Addr64 dstGA, ARM64AMode* amPC,
                                        ARM64CondCode cond, Bool toFastEP );
extern ARM64Instr* ARM64Instr_XIndir  ( HReg dstGA, ARM64AMode* amPC,
                                        ARM64CondCode cond );
extern ARM64Instr* ARM64Instr_XAssisted ( HReg dstGA, ARM64AMode* amPC,
                                          ARM64CondCode cond, IRJumpKind jk );
extern ARM64Instr* ARM64Instr_CSel    ( HReg dst, HReg argL, HReg argR,
                                        ARM64CondCode cond );
extern ARM64Instr* ARM64Instr_Call    ( ARM64CondCode, Addr64, Int nArgRegs,
                                        RetLoc rloc );
extern ARM64Instr* ARM64Instr_AddToSP ( Int simm );
extern ARM64Instr* ARM64Instr_FromSP  ( HReg dst );
extern ARM64Instr* ARM64Instr_Mul     ( HReg dst, HReg argL, HReg argR,
                                        ARM64MulOp op );
extern ARM64Instr* ARM64Instr_LdrEX   ( Int szB );
extern ARM64Instr* ARM64Instr_StrEX   ( Int szB );
extern ARM64Instr* ARM64Instr_MFence  ( void );
extern ARM64Instr* ARM64Instr_VLdStH  ( Bool isLoad, HReg sD, HReg rN,
                                        UInt uimm12 /* 0 .. 8190, 0 % 2 */ );
extern ARM64Instr* ARM64Instr_VLdStS  ( Bool isLoad, HReg sD, HReg rN,
                                        UInt uimm12 /* 0 .. 16380, 0 % 4 */ );
extern ARM64Instr* ARM64Instr_VLdStD  ( Bool isLoad, HReg dD, HReg rN,
                                        UInt uimm12 /* 0 .. 32760, 0 % 8 */ );
extern ARM64Instr* ARM64Instr_VLdStQ  ( Bool isLoad, HReg rQ, HReg rN );
extern ARM64Instr* ARM64Instr_VCvtI2F ( ARM64CvtOp how, HReg rD, HReg rS );
extern ARM64Instr* ARM64Instr_VCvtF2I ( ARM64CvtOp how, HReg rD, HReg rS,
                                        UChar armRM );
extern ARM64Instr* ARM64Instr_VCvtSD  ( Bool sToD, HReg dst, HReg src );
extern ARM64Instr* ARM64Instr_VCvtHS  ( Bool hToS, HReg dst, HReg src );
extern ARM64Instr* ARM64Instr_VCvtHD  ( Bool hToD, HReg dst, HReg src );
extern ARM64Instr* ARM64Instr_VUnaryD ( ARM64FpUnaryOp op, HReg dst, HReg src );
extern ARM64Instr* ARM64Instr_VUnaryS ( ARM64FpUnaryOp op, HReg dst, HReg src );
extern ARM64Instr* ARM64Instr_VBinD   ( ARM64FpBinOp op, HReg, HReg, HReg );
extern ARM64Instr* ARM64Instr_VBinS   ( ARM64FpBinOp op, HReg, HReg, HReg );
extern ARM64Instr* ARM64Instr_VCmpD   ( HReg argL, HReg argR );
extern ARM64Instr* ARM64Instr_VCmpS   ( HReg argL, HReg argR );
extern ARM64Instr* ARM64Instr_VFCSel  ( HReg dst, HReg argL, HReg argR,
                                        ARM64CondCode cond, Bool isD );
extern ARM64Instr* ARM64Instr_FPCR    ( Bool toFPCR, HReg iReg );
extern ARM64Instr* ARM64Instr_FPSR    ( Bool toFPSR, HReg iReg );
extern ARM64Instr* ARM64Instr_VBinV   ( ARM64VecBinOp op, HReg, HReg, HReg );
extern ARM64Instr* ARM64Instr_VModifyV ( ARM64VecModifyOp, HReg, HReg );
extern ARM64Instr* ARM64Instr_VUnaryV ( ARM64VecUnaryOp op, HReg, HReg );
extern ARM64Instr* ARM64Instr_VNarrowV ( ARM64VecNarrowOp op, UInt dszBlg2,
                                         HReg dst, HReg src );
extern ARM64Instr* ARM64Instr_VShiftImmV ( ARM64VecShiftImmOp op,
                                           HReg dst, HReg src, UInt amt );
extern ARM64Instr* ARM64Instr_VExtV   ( HReg dst,
                                        HReg srcLo, HReg srcHi, UInt amtB );
extern ARM64Instr* ARM64Instr_VImmQ   ( HReg, UShort );
extern ARM64Instr* ARM64Instr_VDfromX ( HReg rD, HReg rX );
extern ARM64Instr* ARM64Instr_VQfromX ( HReg rQ, HReg rXlo );
extern ARM64Instr* ARM64Instr_VQfromXX( HReg rQ, HReg rXhi, HReg rXlo );
extern ARM64Instr* ARM64Instr_VXfromQ ( HReg rX, HReg rQ, UInt laneNo );
extern ARM64Instr* ARM64Instr_VXfromDorS ( HReg rX, HReg rDorS, Bool fromD );
extern ARM64Instr* ARM64Instr_VMov    ( UInt szB, HReg dst, HReg src );

extern ARM64Instr* ARM64Instr_EvCheck ( ARM64AMode* amCounter,
                                        ARM64AMode* amFailAddr );
extern ARM64Instr* ARM64Instr_ProfInc ( void );

extern void ppARM64Instr ( const ARM64Instr* );


/* Some functions that insulate the register allocator from details
   of the underlying instruction set. */
extern void getRegUsage_ARM64Instr ( HRegUsage*, const ARM64Instr*, Bool );
extern void mapRegs_ARM64Instr     ( HRegRemap*, ARM64Instr*, Bool );
extern Bool isMove_ARM64Instr      ( const ARM64Instr*, HReg*, HReg* );
extern Int  emit_ARM64Instr        ( /*MB_MOD*/Bool* is_profInc,
                                     UChar* buf, Int nbuf, const ARM64Instr* i,
                                     Bool mode64,
                                     VexEndness endness_host,
                                     const void* disp_cp_chain_me_to_slowEP,
                                     const void* disp_cp_chain_me_to_fastEP,
                                     const void* disp_cp_xindir,
                                     const void* disp_cp_xassisted );

extern void genSpill_ARM64  ( /*OUT*/HInstr** i1, /*OUT*/HInstr** i2,
                              HReg rreg, Int offset, Bool );
extern void genReload_ARM64 ( /*OUT*/HInstr** i1, /*OUT*/HInstr** i2,
                              HReg rreg, Int offset, Bool );

extern const RRegUniverse* getRRegUniverse_ARM64 ( void );

extern HInstrArray* iselSB_ARM64 ( const IRSB*, 
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
extern Int evCheckSzB_ARM64 (void);

/* Perform a chaining and unchaining of an XDirect jump. */
extern VexInvalRange chainXDirect_ARM64 ( VexEndness endness_host,
                                          void* place_to_chain,
                                          const void* disp_cp_chain_me_EXPECTED,
                                          const void* place_to_jump_to );

extern VexInvalRange unchainXDirect_ARM64 ( VexEndness endness_host,
                                            void* place_to_unchain,
                                            const void* place_to_jump_to_EXPECTED,
                                            const void* disp_cp_chain_me );

/* Patch the counter location into an existing ProfInc point. */
extern VexInvalRange patchProfInc_ARM64 ( VexEndness endness_host,
                                          void*  place_to_patch,
                                          const ULong* location_of_counter );


#endif /* ndef __VEX_HOST_ARM64_DEFS_H */

/*---------------------------------------------------------------*/
/*--- end                                   host_arm64_defs.h ---*/
/*---------------------------------------------------------------*/
