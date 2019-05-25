/*---------------------------------------------------------------*/
/*--- begin                                   host_arm_defs.h ---*/
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
*/

#ifndef __VEX_HOST_ARM_DEFS_H
#define __VEX_HOST_ARM_DEFS_H

#include "libvex_basictypes.h"
#include "libvex.h"                      // VexArch
#include "host_generic_regs.h"           // HReg

extern UInt arm_hwcaps;


/* --------- Registers. --------- */

#define ST_IN static inline
ST_IN HReg hregARM_R4  ( void ) { return mkHReg(False, HRcInt32,  4,  0);  }
ST_IN HReg hregARM_R5  ( void ) { return mkHReg(False, HRcInt32,  5,  1);  }
ST_IN HReg hregARM_R6  ( void ) { return mkHReg(False, HRcInt32,  6,  2);  }
ST_IN HReg hregARM_R7  ( void ) { return mkHReg(False, HRcInt32,  7,  3);  }
ST_IN HReg hregARM_R10 ( void ) { return mkHReg(False, HRcInt32,  10, 4);  }
ST_IN HReg hregARM_R11 ( void ) { return mkHReg(False, HRcInt32,  11, 5);  }

ST_IN HReg hregARM_R0  ( void ) { return mkHReg(False, HRcInt32,  0,  6);  }
ST_IN HReg hregARM_R1  ( void ) { return mkHReg(False, HRcInt32,  1,  7);  }
ST_IN HReg hregARM_R2  ( void ) { return mkHReg(False, HRcInt32,  2,  8);  }
ST_IN HReg hregARM_R3  ( void ) { return mkHReg(False, HRcInt32,  3,  9);  }
ST_IN HReg hregARM_R9  ( void ) { return mkHReg(False, HRcInt32,  9,  10); }

ST_IN HReg hregARM_D8  ( void ) { return mkHReg(False, HRcFlt64,  8,  11); }
ST_IN HReg hregARM_D9  ( void ) { return mkHReg(False, HRcFlt64,  9,  12); }
ST_IN HReg hregARM_D10 ( void ) { return mkHReg(False, HRcFlt64,  10, 13); }
ST_IN HReg hregARM_D11 ( void ) { return mkHReg(False, HRcFlt64,  11, 14); }
ST_IN HReg hregARM_D12 ( void ) { return mkHReg(False, HRcFlt64,  12, 15); }

ST_IN HReg hregARM_S26 ( void ) { return mkHReg(False, HRcFlt32,  26, 16); }
ST_IN HReg hregARM_S27 ( void ) { return mkHReg(False, HRcFlt32,  27, 17); }
ST_IN HReg hregARM_S28 ( void ) { return mkHReg(False, HRcFlt32,  28, 18); }
ST_IN HReg hregARM_S29 ( void ) { return mkHReg(False, HRcFlt32,  29, 19); }
ST_IN HReg hregARM_S30 ( void ) { return mkHReg(False, HRcFlt32,  30, 20); }

ST_IN HReg hregARM_Q8  ( void ) { return mkHReg(False, HRcVec128, 8,  21); }
ST_IN HReg hregARM_Q9  ( void ) { return mkHReg(False, HRcVec128, 9,  22); }
ST_IN HReg hregARM_Q10 ( void ) { return mkHReg(False, HRcVec128, 10, 23); }
ST_IN HReg hregARM_Q11 ( void ) { return mkHReg(False, HRcVec128, 11, 24); }
ST_IN HReg hregARM_Q12 ( void ) { return mkHReg(False, HRcVec128, 12, 25); }

ST_IN HReg hregARM_R8  ( void ) { return mkHReg(False, HRcInt32,  8,  26); }
ST_IN HReg hregARM_R12 ( void ) { return mkHReg(False, HRcInt32,  12, 27); }
ST_IN HReg hregARM_R13 ( void ) { return mkHReg(False, HRcInt32,  13, 28); }
ST_IN HReg hregARM_R14 ( void ) { return mkHReg(False, HRcInt32,  14, 29); }
ST_IN HReg hregARM_R15 ( void ) { return mkHReg(False, HRcInt32,  15, 30); }
ST_IN HReg hregARM_Q13 ( void ) { return mkHReg(False, HRcVec128, 13, 31); }
ST_IN HReg hregARM_Q14 ( void ) { return mkHReg(False, HRcVec128, 14, 32); }
ST_IN HReg hregARM_Q15 ( void ) { return mkHReg(False, HRcVec128, 15, 33); }
#undef ST_IN

extern UInt ppHRegARM ( HReg );

/* Number of registers used arg passing in function calls */
#define ARM_N_ARGREGS 4   /* r0, r1, r2, r3 */


/* --------- Condition codes. --------- */

typedef
   enum {
      ARMcc_EQ  = 0,  /* equal                          : Z=1 */
      ARMcc_NE  = 1,  /* not equal                      : Z=0 */

      ARMcc_HS  = 2,  /* >=u (higher or same)           : C=1 */
      ARMcc_LO  = 3,  /* <u  (lower)                    : C=0 */

      ARMcc_MI  = 4,  /* minus (negative)               : N=1 */
      ARMcc_PL  = 5,  /* plus (zero or +ve)             : N=0 */

      ARMcc_VS  = 6,  /* overflow                       : V=1 */
      ARMcc_VC  = 7,  /* no overflow                    : V=0 */

      ARMcc_HI  = 8,  /* >u   (higher)                  : C=1 && Z=0 */
      ARMcc_LS  = 9,  /* <=u  (lower or same)           : C=0 || Z=1 */

      ARMcc_GE  = 10, /* >=s (signed greater or equal)  : N=V */
      ARMcc_LT  = 11, /* <s  (signed less than)         : N!=V */

      ARMcc_GT  = 12, /* >s  (signed greater)           : Z=0 && N=V */
      ARMcc_LE  = 13, /* <=s (signed less or equal)     : Z=1 || N!=V */

      ARMcc_AL  = 14, /* always (unconditional) */
      ARMcc_NV  = 15  /* never (basically undefined meaning), deprecated */
   }
   ARMCondCode;

extern const HChar* showARMCondCode ( ARMCondCode );



/* --------- Memory address expressions (amodes). --------- */

/* --- Addressing Mode 1 --- */
typedef
   enum {
      ARMam1_RI=1,   /* reg +/- imm12 */
      ARMam1_RRS     /* reg1 + (reg2 << 0, 1 2 or 3) */
   }
   ARMAMode1Tag;

typedef
   struct {
      ARMAMode1Tag tag;
      union {
         struct {
            HReg reg;
            Int  simm13; /* -4095 .. +4095 */
         } RI;
         struct {
            HReg base;
            HReg index;
            UInt shift; /* 0, 1 2 or 3 */
         } RRS;
      } ARMam1;
   }
   ARMAMode1;

extern ARMAMode1* ARMAMode1_RI  ( HReg reg, Int simm13 );
extern ARMAMode1* ARMAMode1_RRS ( HReg base, HReg index, UInt shift );

extern void ppARMAMode1 ( ARMAMode1* );


/* --- Addressing Mode 2 --- */
typedef
   enum {
      ARMam2_RI=3,   /* reg +/- imm8 */
      ARMam2_RR      /* reg1 + reg2 */
   }
   ARMAMode2Tag;

typedef
   struct {
      ARMAMode2Tag tag;
      union {
         struct {
            HReg reg;
            Int  simm9; /* -255 .. 255 */
         } RI;
         struct {
            HReg base;
            HReg index;
         } RR;
      } ARMam2;
   }
   ARMAMode2;

extern ARMAMode2* ARMAMode2_RI ( HReg reg, Int simm9 );
extern ARMAMode2* ARMAMode2_RR ( HReg base, HReg index );

extern void ppARMAMode2 ( ARMAMode2* );


/* --- Addressing Mode suitable for VFP --- */
/* The simm11 is encoded as 8 bits + 1 sign bit,
   so can only be 0 % 4. */
typedef
   struct {
      HReg reg;
      Int  simm11; /* -1020, -1016 .. 1016, 1020 */
   }
   ARMAModeV;

extern ARMAModeV* mkARMAModeV ( HReg reg, Int simm11 );

extern void ppARMAModeV ( ARMAModeV* );

/* --- Addressing Mode suitable for Neon --- */
typedef
   enum {
      ARMamN_R=5,
      ARMamN_RR
      /* ... */
   }
   ARMAModeNTag;

typedef
   struct {
      ARMAModeNTag tag;
      union {
         struct {
            HReg rN;
            HReg rM;
         } RR;
         struct {
            HReg rN;
         } R;
         /* ... */
      } ARMamN;
   }
   ARMAModeN;

extern ARMAModeN* mkARMAModeN_RR ( HReg, HReg );
extern ARMAModeN* mkARMAModeN_R ( HReg );
extern void ppARMAModeN ( ARMAModeN* );

/* --------- Reg or imm-8x4 operands --------- */
/* a.k.a (a very restricted form of) Shifter Operand,
   in the ARM parlance. */

typedef
   enum {
      ARMri84_I84=7,   /* imm8 `ror` (2 * imm4) */
      ARMri84_R        /* reg */
   }
   ARMRI84Tag;

typedef
   struct {
      ARMRI84Tag tag;
      union {
         struct {
            UShort imm8;
            UShort imm4;
         } I84;
         struct {
            HReg reg;
         } R;
      } ARMri84;
   }
   ARMRI84;

extern ARMRI84* ARMRI84_I84 ( UShort imm8, UShort imm4 );
extern ARMRI84* ARMRI84_R   ( HReg );

extern void ppARMRI84 ( ARMRI84* );


/* --------- Reg or imm5 operands --------- */
typedef
   enum {
      ARMri5_I5=9,   /* imm5, 1 .. 31 only (no zero!) */
      ARMri5_R       /* reg */
   }
   ARMRI5Tag;

typedef
   struct {
      ARMRI5Tag tag;
      union {
         struct {
            UInt imm5;
         } I5;
         struct {
            HReg reg;
         } R;
      } ARMri5;
   }
   ARMRI5;

extern ARMRI5* ARMRI5_I5 ( UInt imm5 );
extern ARMRI5* ARMRI5_R  ( HReg );

extern void ppARMRI5 ( ARMRI5* );

/* -------- Neon Immediate operand -------- */

/* imm8 = abcdefgh, B = NOT(b);

type | value (64bit binary)
-----+-------------------------------------------------------------------------
   0 | 00000000 00000000 00000000 abcdefgh 00000000 00000000 00000000 abcdefgh
   1 | 00000000 00000000 abcdefgh 00000000 00000000 00000000 abcdefgh 00000000
   2 | 00000000 abcdefgh 00000000 00000000 00000000 abcdefgh 00000000 00000000
   3 | abcdefgh 00000000 00000000 00000000 abcdefgh 00000000 00000000 00000000
   4 | 00000000 abcdefgh 00000000 abcdefgh 00000000 abcdefgh 00000000 abcdefgh
   5 | abcdefgh 00000000 abcdefgh 00000000 abcdefgh 00000000 abcdefgh 00000000
   6 | abcdefgh abcdefgh abcdefgh abcdefgh abcdefgh abcdefgh abcdefgh abcdefgh
   7 | 00000000 00000000 abcdefgh 11111111 00000000 00000000 abcdefgh 11111111
   8 | 00000000 abcdefgh 11111111 11111111 00000000 abcdefgh 11111111 11111111
   9 | aaaaaaaa bbbbbbbb cccccccc dddddddd eeeeeeee ffffffff gggggggg hhhhhhhh
  10 | aBbbbbbc defgh000 00000000 00000000 aBbbbbbc defgh000 00000000 00000000
-----+-------------------------------------------------------------------------

Type 10 is:
   (-1)^S * 2^exp * mantissa
where S = a, exp = UInt(B:c:d) - 3, mantissa = (16 + UInt(e:f:g:h)) / 16
*/

typedef
   struct {
      UInt type;
      UInt imm8;
   }
   ARMNImm;

extern ARMNImm* ARMNImm_TI ( UInt type, UInt imm8 );
extern ULong ARMNImm_to_Imm64 ( ARMNImm* );
extern ARMNImm* Imm64_to_ARMNImm ( ULong );

extern void ppARMNImm ( ARMNImm* );

/* ------ Neon Register or Scalar Operand ------ */

typedef
   enum {
      ARMNRS_Reg=11,
      ARMNRS_Scalar
   }
   ARMNRS_tag;

typedef
   struct {
      ARMNRS_tag tag;
      HReg reg;
      UInt index;
   }
   ARMNRS;

extern ARMNRS* mkARMNRS(ARMNRS_tag, HReg reg, UInt index);
extern void ppARMNRS ( ARMNRS* );

/* --------- Instructions. --------- */

/* --------- */
typedef
   enum {
      ARMalu_ADD=20,   /* plain 32-bit add */
      ARMalu_ADDS,     /* 32-bit add, and set the flags */
      ARMalu_ADC,      /* 32-bit add with carry */
      ARMalu_SUB,      /* plain 32-bit subtract */
      ARMalu_SUBS,     /* 32-bit subtract, and set the flags */
      ARMalu_SBC,      /* 32-bit subtract with carry */
      ARMalu_AND,
      ARMalu_BIC,
      ARMalu_OR,
      ARMalu_XOR
   }
   ARMAluOp;

extern const HChar* showARMAluOp ( ARMAluOp op );


typedef
   enum {
      ARMsh_SHL=40,
      ARMsh_SHR,
      ARMsh_SAR
   }
   ARMShiftOp;

extern const HChar* showARMShiftOp ( ARMShiftOp op );


typedef
   enum {
      ARMun_NEG=50,
      ARMun_NOT,
      ARMun_CLZ
   }
   ARMUnaryOp;

extern const HChar* showARMUnaryOp ( ARMUnaryOp op );


typedef
   enum {
      ARMmul_PLAIN=60,
      ARMmul_ZX,
      ARMmul_SX
   }
   ARMMulOp;

extern const HChar* showARMMulOp ( ARMMulOp op );


typedef
   enum {
      ARMvfp_ADD=70,
      ARMvfp_SUB,
      ARMvfp_MUL,
      ARMvfp_DIV
   }
   ARMVfpOp;

extern const HChar* showARMVfpOp ( ARMVfpOp op );


typedef
   enum {
      ARMvfpu_COPY=80,
      ARMvfpu_NEG,
      ARMvfpu_ABS,
      ARMvfpu_SQRT
   }
   ARMVfpUnaryOp;

extern const HChar* showARMVfpUnaryOp ( ARMVfpUnaryOp op );

typedef
   enum {
      ARMneon_VAND=90,
      ARMneon_VORR,
      ARMneon_VXOR,
      ARMneon_VADD,
      ARMneon_VADDFP,
      ARMneon_VRHADDS,
      ARMneon_VRHADDU,
      ARMneon_VPADDFP,
      ARMneon_VABDFP,
      ARMneon_VSUB,
      ARMneon_VSUBFP,
      ARMneon_VMAXU,
      ARMneon_VMAXS,
      ARMneon_VMAXF,
      ARMneon_VMINU,
      ARMneon_VMINS,
      ARMneon_VMINF,
      ARMneon_VQADDU,
      ARMneon_VQADDS,
      ARMneon_VQSUBU,
      ARMneon_VQSUBS,
      ARMneon_VCGTU,
      ARMneon_VCGTS,
      ARMneon_VCGEU,
      ARMneon_VCGES,
      ARMneon_VCGTF,
      ARMneon_VCGEF,
      ARMneon_VCEQ,
      ARMneon_VCEQF,
      ARMneon_VEXT,
      ARMneon_VMUL,
      ARMneon_VMULFP,
      ARMneon_VMULLU,
      ARMneon_VMULLS,
      ARMneon_VMULP,
      ARMneon_VMULLP,
      ARMneon_VQDMULH,
      ARMneon_VQRDMULH,
      ARMneon_VPADD,
      ARMneon_VPMINU,
      ARMneon_VPMINS,
      ARMneon_VPMINF,
      ARMneon_VPMAXU,
      ARMneon_VPMAXS,
      ARMneon_VPMAXF,
      ARMneon_VTBL,
      ARMneon_VQDMULL,
      ARMneon_VRECPS,
      ARMneon_VRSQRTS,
      ARMneon_INVALID
      /* ... */
   }
   ARMNeonBinOp;

typedef
   enum {
      ARMneon_VSHL=150,
      ARMneon_VSAL, /* Yah, not SAR but SAL */
      ARMneon_VQSHL,
      ARMneon_VQSAL
   }
   ARMNeonShiftOp;

typedef
   enum {
      ARMneon_COPY=160,
      ARMneon_COPYLU,
      ARMneon_COPYLS,
      ARMneon_COPYN,
      ARMneon_COPYQNSS,
      ARMneon_COPYQNUS,
      ARMneon_COPYQNUU,
      ARMneon_NOT,
      ARMneon_EQZ,
      ARMneon_DUP,
      ARMneon_PADDLS,
      ARMneon_PADDLU,
      ARMneon_CNT,
      ARMneon_CLZ,
      ARMneon_CLS,
      ARMneon_VCVTxFPxINT,
      ARMneon_VQSHLNSS,
      ARMneon_VQSHLNUU,
      ARMneon_VQSHLNUS,
      ARMneon_VCVTFtoU,
      ARMneon_VCVTFtoS,
      ARMneon_VCVTUtoF,
      ARMneon_VCVTStoF,
      ARMneon_VCVTFtoFixedU,
      ARMneon_VCVTFtoFixedS,
      ARMneon_VCVTFixedUtoF,
      ARMneon_VCVTFixedStoF,
      ARMneon_VCVTF16toF32,
      ARMneon_VCVTF32toF16,
      ARMneon_REV16,
      ARMneon_REV32,
      ARMneon_REV64,
      ARMneon_ABS,
      ARMneon_VNEGF,
      ARMneon_VRECIP,
      ARMneon_VRECIPF,
      ARMneon_VABSFP,
      ARMneon_VRSQRTEFP,
      ARMneon_VRSQRTE
      /* ... */
   }
   ARMNeonUnOp;

typedef
   enum {
      ARMneon_SETELEM=200,
      ARMneon_GETELEMU,
      ARMneon_GETELEMS,
      ARMneon_VDUP,
   }
   ARMNeonUnOpS;

typedef
   enum {
      ARMneon_TRN=210,
      ARMneon_ZIP,
      ARMneon_UZP
      /* ... */
   }
   ARMNeonDualOp;

extern const HChar* showARMNeonBinOp ( ARMNeonBinOp op );
extern const HChar* showARMNeonUnOp ( ARMNeonUnOp op );
extern const HChar* showARMNeonUnOpS ( ARMNeonUnOpS op );
extern const HChar* showARMNeonShiftOp ( ARMNeonShiftOp op );
extern const HChar* showARMNeonDualOp ( ARMNeonDualOp op );
extern const HChar* showARMNeonBinOpDataType ( ARMNeonBinOp op );
extern const HChar* showARMNeonUnOpDataType ( ARMNeonUnOp op );
extern const HChar* showARMNeonUnOpSDataType ( ARMNeonUnOpS op );
extern const HChar* showARMNeonShiftOpDataType ( ARMNeonShiftOp op );
extern const HChar* showARMNeonDualOpDataType ( ARMNeonDualOp op );

typedef
   enum {
      /* baseline */
      ARMin_Alu=220,
      ARMin_Shift,
      ARMin_Unary,
      ARMin_CmpOrTst,
      ARMin_Mov,
      ARMin_Imm32,
      ARMin_LdSt32,
      ARMin_LdSt16,
      ARMin_LdSt8U,
      ARMin_Ld8S,
      ARMin_XDirect,     /* direct transfer to GA */
      ARMin_XIndir,      /* indirect transfer to GA */
      ARMin_XAssisted,   /* assisted transfer to GA */
      ARMin_CMov,
      ARMin_Call,
      ARMin_Mul,
      ARMin_LdrEX,
      ARMin_StrEX,
      /* vfp */
      ARMin_VLdStD,
      ARMin_VLdStS,
      ARMin_VAluD,
      ARMin_VAluS,
      ARMin_VUnaryD,
      ARMin_VUnaryS,
      ARMin_VCmpD,
      ARMin_VCMovD,
      ARMin_VCMovS,
      ARMin_VCvtSD,
      ARMin_VXferQ,
      ARMin_VXferD,
      ARMin_VXferS,
      ARMin_VCvtID,
      ARMin_VRIntR,
      ARMin_VMinMaxNum,
      ARMin_FPSCR,
      ARMin_MFence,
      ARMin_CLREX,
      /* Neon */
      ARMin_NLdStQ,
      ARMin_NLdStD,
      ARMin_NUnary,
      ARMin_NUnaryS,
      ARMin_NDual,
      ARMin_NBinary,
      ARMin_NBinaryS,
      ARMin_NShift,
      ARMin_NShl64, // special case 64-bit shift of Dreg by immediate
      ARMin_NeonImm,
      ARMin_NCMovQ,
      /* This is not a NEON instruction. Actually there is no corresponding
         instruction in ARM instruction set at all. We need this one to
         generate spill/reload of 128-bit registers since current register
         allocator demands them to consist of no more than two instructions.
         We will split this instruction into 2 or 3 ARM instructions on the
         emiting phase.
         NOTE: source and destination registers should be different! */
      ARMin_Add32,
      ARMin_EvCheck,     /* Event check */
      ARMin_ProfInc      /* 64-bit profile counter increment */
   }
   ARMInstrTag;

/* Destinations are on the LEFT (first operand) */

typedef
   struct {
      ARMInstrTag tag;
      union {
         /* ADD/SUB/AND/OR/XOR, vanilla ALU op */
         struct {
            ARMAluOp op;
            HReg     dst;
            HReg     argL;
            ARMRI84* argR;
         } Alu;
         /* SHL/SHR/SAR, 2nd arg is reg or imm */
         struct {
            ARMShiftOp op;
            HReg       dst;
            HReg       argL;
            ARMRI5*    argR;
         } Shift;
         /* NOT/NEG/CLZ */
         struct {
            ARMUnaryOp op;
            HReg       dst;
            HReg       src;
         } Unary;
         /* CMP/TST; subtract/and, discard result, set NZCV */
         struct {
            Bool     isCmp;
            HReg     argL;
            ARMRI84* argR;
         } CmpOrTst;
         /* MOV dst, src -- reg-reg (or reg-imm8x4) move */
         struct {
            HReg     dst;
            ARMRI84* src;
         } Mov;
         /* Pseudo-insn; make a 32-bit immediate */
         struct {
            HReg dst;
            UInt imm32;
         } Imm32;
         /* 32-bit load or store, may be conditional */
         struct {
            ARMCondCode cc; /* ARMcc_NV is not allowed */
            Bool        isLoad;
            HReg        rD;
            ARMAMode1*  amode;
         } LdSt32;
         /* 16-bit load or store, may be conditional */
         struct {
            ARMCondCode cc; /* ARMcc_NV is not allowed */
            Bool        isLoad;
            Bool        signedLoad;
            HReg        rD;
            ARMAMode2*  amode;
         } LdSt16;
         /* 8-bit (unsigned) load or store, may be conditional */
         struct {
            ARMCondCode cc; /* ARMcc_NV is not allowed */
            Bool        isLoad;
            HReg        rD;
            ARMAMode1*  amode;
         } LdSt8U;
         /* 8-bit signed load, may be conditional */
         struct {
            ARMCondCode cc; /* ARMcc_NV is not allowed */
            HReg        rD;
            ARMAMode2*  amode;
         } Ld8S;
         /* Update the guest R15T value, then exit requesting to chain
            to it.  May be conditional.  Urr, use of Addr32 implicitly
            assumes that wordsize(guest) == wordsize(host). */
         struct {
            Addr32      dstGA;    /* next guest address */
            ARMAMode1*  amR15T;   /* amode in guest state for R15T */
            ARMCondCode cond;     /* can be ARMcc_AL */
            Bool        toFastEP; /* chain to the slow or fast point? */
         } XDirect;
         /* Boring transfer to a guest address not known at JIT time.
            Not chainable.  May be conditional. */
         struct {
            HReg        dstGA;
            ARMAMode1*  amR15T;
            ARMCondCode cond; /* can be ARMcc_AL */
         } XIndir;
         /* Assisted transfer to a guest address, most general case.
            Not chainable.  May be conditional. */
         struct {
            HReg        dstGA;
            ARMAMode1*  amR15T;
            ARMCondCode cond; /* can be ARMcc_AL */
            IRJumpKind  jk;
         } XAssisted;
         /* Mov src to dst on the given condition, which may not
            be ARMcc_AL. */
         struct {
            ARMCondCode cond;
            HReg        dst;
            ARMRI84*    src;
         } CMov;
         /* Pseudo-insn.  Call target (an absolute address), on given
            condition (which could be ARMcc_AL). */
         struct {
            ARMCondCode cond;
            Addr32      target;
            Int         nArgRegs; /* # regs carrying args: 0 .. 4 */
            RetLoc      rloc;     /* where the return value will be */
         } Call;
         /* (PLAIN) 32 *  32 -> 32:  r0    = r2 * r3
            (ZX)    32 *u 32 -> 64:  r1:r0 = r2 *u r3
            (SX)    32 *s 32 -> 64:  r1:r0 = r2 *s r3
            Why hardwired registers?  Because the ARM ARM specifies
            (eg for straight MUL) the result (Rd) and the left arg (Rm)
            may not be the same register.  That's not a constraint we
            can enforce in the register allocator (without mucho extra
            complexity).  Hence hardwire it.  At least using caller-saves
            registers, which are less likely to be in use. */
         struct {
            ARMMulOp op;
         } Mul;
         /* LDREX{,H,B} r2, [r4]  and
            LDREXD r2, r3, [r4]   (on LE hosts, transferred value is r3:r2)
            Again, hardwired registers since this is not performance
            critical, and there are possibly constraints on the
            registers that we can't express in the register allocator.*/
         struct {
            Int  szB; /* 1, 2, 4 or 8 */
         } LdrEX;
         /* STREX{,H,B} r0, r2, [r4]  and  
            STREXD r0, r2, r3, [r4]   (on LE hosts, transferred value is r3:r2)
            r0 = SC( [r4] = r2 )      (8, 16, 32 bit transfers)
            r0 = SC( [r4] = r3:r2)    (64 bit transfers)
            Ditto comment re fixed registers. */
         struct {
            Int  szB; /* 1, 2, 4 or 8 */
         } StrEX;
         /* VFP INSTRUCTIONS */
         /* 64-bit Fp load/store */
         struct {
            Bool       isLoad;
            HReg       dD;
            ARMAModeV* amode;
         } VLdStD;
         /* 32-bit Fp load/store */
         struct {
            Bool       isLoad;
            HReg       fD;
            ARMAModeV* amode;
         } VLdStS;
         /* 64-bit FP binary arithmetic */
         struct {
            ARMVfpOp op;
            HReg     dst;
            HReg     argL;
            HReg     argR;
         } VAluD;
         /* 32-bit FP binary arithmetic */
         struct {
            ARMVfpOp op;
            HReg     dst;
            HReg     argL;
            HReg     argR;
         } VAluS;
         /* 64-bit FP unary, also reg-reg move */
         struct {
            ARMVfpUnaryOp op;
            HReg          dst;
            HReg          src;
         } VUnaryD;
         /* 32-bit FP unary, also reg-reg move */
         struct {
            ARMVfpUnaryOp op;
            HReg          dst;
            HReg          src;
         } VUnaryS;
         /* 64-bit FP compare and move results to CPSR (FCMPD;FMSTAT) */
         struct {
            HReg argL;
            HReg argR;
         } VCmpD;
         /* 64-bit FP mov src to dst on the given condition, which may
            not be ARMcc_AL. */
         struct {
            ARMCondCode cond;
            HReg        dst;
            HReg        src;
         } VCMovD;
         /* 32-bit FP mov src to dst on the given condition, which may
            not be ARMcc_AL. */
         struct {
            ARMCondCode cond;
            HReg        dst;
            HReg        src;
         } VCMovS;
         /* Convert between 32-bit and 64-bit FP values (both ways).
            (FCVTSD, FCVTDS) */
         struct {
            Bool sToD; /* True: F32->F64.  False: F64->F32 */
            HReg dst;
            HReg src;
         } VCvtSD;
         /* Transfer a NEON Q reg to/from two D registers (VMOV x 2) */
         struct {
            Bool toQ;
            HReg qD;
            HReg dHi;
            HReg dLo;
         } VXferQ;
         /* Transfer a VFP D reg to/from two integer registers (VMOV) */
         struct {
            Bool toD;
            HReg dD;
            HReg rHi;
            HReg rLo;
         } VXferD;
         /* Transfer a VFP S reg to/from an integer register (VMOV) */
         struct {
            Bool toS;
            HReg fD;
            HReg rLo;
         } VXferS;
         /* Convert between 32-bit ints and 64-bit FP values (both ways
            and both signednesses). (FSITOD, FUITOD, FTOSID, FTOUID) */
         struct {
            Bool iToD; /* True: I32->F64.  False: F64->I32 */
            Bool syned; /* True: I32 is signed.  False: I32 is unsigned */
            HReg dst;
            HReg src;
         } VCvtID;
         /* Round a F32 or F64 value to the nearest integral value,
            according to the FPSCR.RM.  For ARM >= V8 hosts only. */
         struct {
            Bool isF64;
            HReg dst;
            HReg src;
         } VRIntR;
         /* Do Min/Max of F32 or F64 values, propagating the numerical arg
            if the other is a qNaN.  For ARM >= V8 hosts only. */
         struct {
            Bool isF64;
            Bool isMax;
            HReg dst;
            HReg srcL;
            HReg srcR;
         } VMinMaxNum;
         /* Move a 32-bit value to/from the FPSCR (FMXR, FMRX) */
         struct {
            Bool toFPSCR;
            HReg iReg;
         } FPSCR;
         /* Mem fence.  An insn which fences all loads and stores as
            much as possible before continuing.  On ARM we emit the
            sequence
               mcr 15,0,r0,c7,c10,4 (DSB)
               mcr 15,0,r0,c7,c10,5 (DMB)
               mcr 15,0,r0,c7,c5,4 (ISB)
            which is probably total overkill, but better safe than
            sorry.
         */
         struct {
         } MFence;
         /* A CLREX instruction. */
         struct {
         } CLREX;
         /* Neon data processing instruction: 3 registers of the same
            length */
         struct {
            ARMNeonBinOp op;
            HReg dst;
            HReg argL;
            HReg argR;
            UInt size;
            Bool Q;
         } NBinary;
         struct {
            ARMNeonBinOp op;
            ARMNRS* dst;
            ARMNRS* argL;
            ARMNRS* argR;
            UInt size;
            Bool Q;
         } NBinaryS;
         struct {
            ARMNeonShiftOp op;
            HReg dst;
            HReg argL;
            HReg argR;
            UInt size;
            Bool Q;
         } NShift;
         struct {
            HReg dst;
            HReg src;
            UInt amt; /* 1..63 only */
         } NShl64;
         struct {
            Bool isLoad;
            HReg dQ;
            ARMAModeN *amode;
         } NLdStQ;
         struct {
            Bool isLoad;
            HReg dD;
            ARMAModeN *amode;
         } NLdStD;
         struct {
            ARMNeonUnOpS op;
            ARMNRS*  dst;
            ARMNRS*  src;
            UInt size;
            Bool Q;
         } NUnaryS;
         struct {
            ARMNeonUnOp op;
            HReg  dst;
            HReg  src;
            UInt size;
            Bool Q;
         } NUnary;
         /* Takes two arguments and modifies them both. */
         struct {
            ARMNeonDualOp op;
            HReg  arg1;
            HReg  arg2;
            UInt size;
            Bool Q;
         } NDual;
         struct {
            HReg dst;
            ARMNImm* imm;
         } NeonImm;
         /* 128-bit Neon move src to dst on the given condition, which
            may not be ARMcc_AL. */
         struct {
            ARMCondCode cond;
            HReg        dst;
            HReg        src;
         } NCMovQ;
         struct {
            /* Note: rD != rN */
            HReg rD;
            HReg rN;
            UInt imm32;
         } Add32;
         struct {
            ARMAMode1* amCounter;
            ARMAMode1* amFailAddr;
         } EvCheck;
         struct {
            /* No fields.  The address of the counter to inc is
               installed later, post-translation, by patching it in,
               as it is not known at translation time. */
         } ProfInc;
      } ARMin;
   }
   ARMInstr;


extern ARMInstr* ARMInstr_Alu      ( ARMAluOp, HReg, HReg, ARMRI84* );
extern ARMInstr* ARMInstr_Shift    ( ARMShiftOp, HReg, HReg, ARMRI5* );
extern ARMInstr* ARMInstr_Unary    ( ARMUnaryOp, HReg, HReg );
extern ARMInstr* ARMInstr_CmpOrTst ( Bool isCmp, HReg, ARMRI84* );
extern ARMInstr* ARMInstr_Mov      ( HReg, ARMRI84* );
extern ARMInstr* ARMInstr_Imm32    ( HReg, UInt );
extern ARMInstr* ARMInstr_LdSt32   ( ARMCondCode,
                                     Bool isLoad, HReg, ARMAMode1* );
extern ARMInstr* ARMInstr_LdSt16   ( ARMCondCode,
                                     Bool isLoad, Bool signedLoad,
                                     HReg, ARMAMode2* );
extern ARMInstr* ARMInstr_LdSt8U   ( ARMCondCode,
                                     Bool isLoad, HReg, ARMAMode1* );
extern ARMInstr* ARMInstr_Ld8S     ( ARMCondCode, HReg, ARMAMode2* );
extern ARMInstr* ARMInstr_XDirect  ( Addr32 dstGA, ARMAMode1* amR15T,
                                     ARMCondCode cond, Bool toFastEP );
extern ARMInstr* ARMInstr_XIndir   ( HReg dstGA, ARMAMode1* amR15T,
                                     ARMCondCode cond );
extern ARMInstr* ARMInstr_XAssisted ( HReg dstGA, ARMAMode1* amR15T,
                                      ARMCondCode cond, IRJumpKind jk );
extern ARMInstr* ARMInstr_CMov     ( ARMCondCode, HReg dst, ARMRI84* src );
extern ARMInstr* ARMInstr_Call     ( ARMCondCode, Addr32, Int nArgRegs,
                                     RetLoc rloc );
extern ARMInstr* ARMInstr_Mul      ( ARMMulOp op );
extern ARMInstr* ARMInstr_LdrEX    ( Int szB );
extern ARMInstr* ARMInstr_StrEX    ( Int szB );
extern ARMInstr* ARMInstr_VLdStD   ( Bool isLoad, HReg, ARMAModeV* );
extern ARMInstr* ARMInstr_VLdStS   ( Bool isLoad, HReg, ARMAModeV* );
extern ARMInstr* ARMInstr_VAluD    ( ARMVfpOp op, HReg, HReg, HReg );
extern ARMInstr* ARMInstr_VAluS    ( ARMVfpOp op, HReg, HReg, HReg );
extern ARMInstr* ARMInstr_VUnaryD  ( ARMVfpUnaryOp, HReg dst, HReg src );
extern ARMInstr* ARMInstr_VUnaryS  ( ARMVfpUnaryOp, HReg dst, HReg src );
extern ARMInstr* ARMInstr_VCmpD    ( HReg argL, HReg argR );
extern ARMInstr* ARMInstr_VCMovD   ( ARMCondCode, HReg dst, HReg src );
extern ARMInstr* ARMInstr_VCMovS   ( ARMCondCode, HReg dst, HReg src );
extern ARMInstr* ARMInstr_VCvtSD   ( Bool sToD, HReg dst, HReg src );
extern ARMInstr* ARMInstr_VXferQ   ( Bool toQ, HReg qD, HReg dHi, HReg dLo );
extern ARMInstr* ARMInstr_VXferD   ( Bool toD, HReg dD, HReg rHi, HReg rLo );
extern ARMInstr* ARMInstr_VXferS   ( Bool toS, HReg fD, HReg rLo );
extern ARMInstr* ARMInstr_VCvtID   ( Bool iToD, Bool syned,
                                     HReg dst, HReg src );
extern ARMInstr* ARMInstr_VRIntR   ( Bool isF64, HReg dst, HReg src );
extern ARMInstr* ARMInstr_VMinMaxNum ( Bool isF64, Bool isMax,
                                       HReg dst, HReg srcL, HReg srcR );
extern ARMInstr* ARMInstr_FPSCR    ( Bool toFPSCR, HReg iReg );
extern ARMInstr* ARMInstr_MFence   ( void );
extern ARMInstr* ARMInstr_CLREX    ( void );
extern ARMInstr* ARMInstr_NLdStQ   ( Bool isLoad, HReg, ARMAModeN* );
extern ARMInstr* ARMInstr_NLdStD   ( Bool isLoad, HReg, ARMAModeN* );
extern ARMInstr* ARMInstr_NUnary   ( ARMNeonUnOp, HReg, HReg, UInt, Bool );
extern ARMInstr* ARMInstr_NUnaryS  ( ARMNeonUnOpS, ARMNRS*, ARMNRS*,
                                     UInt, Bool );
extern ARMInstr* ARMInstr_NDual    ( ARMNeonDualOp, HReg, HReg, UInt, Bool );
extern ARMInstr* ARMInstr_NBinary  ( ARMNeonBinOp, HReg, HReg, HReg,
                                     UInt, Bool );
extern ARMInstr* ARMInstr_NShift   ( ARMNeonShiftOp, HReg, HReg, HReg,
                                     UInt, Bool );
extern ARMInstr* ARMInstr_NShl64   ( HReg, HReg, UInt );
extern ARMInstr* ARMInstr_NeonImm  ( HReg, ARMNImm* );
extern ARMInstr* ARMInstr_NCMovQ   ( ARMCondCode, HReg, HReg );
extern ARMInstr* ARMInstr_Add32    ( HReg rD, HReg rN, UInt imm32 );
extern ARMInstr* ARMInstr_EvCheck  ( ARMAMode1* amCounter,
                                     ARMAMode1* amFailAddr );
extern ARMInstr* ARMInstr_ProfInc  ( void );

extern void ppARMInstr ( const ARMInstr* );


/* Some functions that insulate the register allocator from details
   of the underlying instruction set. */
extern void getRegUsage_ARMInstr ( HRegUsage*, const ARMInstr*, Bool );
extern void mapRegs_ARMInstr     ( HRegRemap*, ARMInstr*, Bool );
extern Int  emit_ARMInstr        ( /*MB_MOD*/Bool* is_profInc,
                                   UChar* buf, Int nbuf, const ARMInstr* i, 
                                   Bool mode64,
                                   VexEndness endness_host,
                                   const void* disp_cp_chain_me_to_slowEP,
                                   const void* disp_cp_chain_me_to_fastEP,
                                   const void* disp_cp_xindir,
                                   const void* disp_cp_xassisted );

extern void genSpill_ARM  ( /*OUT*/HInstr** i1, /*OUT*/HInstr** i2,
                            HReg rreg, Int offset, Bool );
extern void genReload_ARM ( /*OUT*/HInstr** i1, /*OUT*/HInstr** i2,
                            HReg rreg, Int offset, Bool );
extern ARMInstr* genMove_ARM(HReg from, HReg to, Bool);

extern const RRegUniverse* getRRegUniverse_ARM ( void );

extern HInstrArray* iselSB_ARM   ( const IRSB*, 
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
extern Int evCheckSzB_ARM (void);

/* Perform a chaining and unchaining of an XDirect jump. */
extern VexInvalRange chainXDirect_ARM ( VexEndness endness_host,
                                        void* place_to_chain,
                                        const void* disp_cp_chain_me_EXPECTED,
                                        const void* place_to_jump_to );

extern VexInvalRange unchainXDirect_ARM ( VexEndness endness_host,
                                          void* place_to_unchain,
                                          const void* place_to_jump_to_EXPECTED,
                                          const void* disp_cp_chain_me );

/* Patch the counter location into an existing ProfInc point. */
extern VexInvalRange patchProfInc_ARM ( VexEndness endness_host,
                                        void*  place_to_patch,
                                        const ULong* location_of_counter );


#endif /* ndef __VEX_HOST_ARM_DEFS_H */

/*---------------------------------------------------------------*/
/*--- end                                     host_arm_defs.h ---*/
/*---------------------------------------------------------------*/
