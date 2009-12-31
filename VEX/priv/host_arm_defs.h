
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (host_arm_defs.h) is                          ---*/
/*--- Copyright (C) OpenWorks LLP.  All rights reserved.      ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of LibVEX, a library for dynamic binary
   instrumentation and translation.

   Copyright (C) 2004-2009 OpenWorks LLP.  All rights reserved.

   This library is made available under a dual licensing scheme.

   If you link LibVEX against other code all of which is itself
   licensed under the GNU General Public License, version 2 dated June
   1991 ("GPL v2"), then you may use LibVEX under the terms of the GPL
   v2, as appearing in the file LICENSE.GPL.  If the file LICENSE.GPL
   is missing, you can obtain a copy of the GPL v2 from the Free
   Software Foundation Inc., 51 Franklin St, Fifth Floor, Boston, MA
   02110-1301, USA.

   For any other uses of LibVEX, you must first obtain a commercial
   license from OpenWorks LLP.  Please contact info@open-works.co.uk
   for information about commercial licensing.

   This software is provided by OpenWorks LLP "as is" and any express
   or implied warranties, including, but not limited to, the implied
   warranties of merchantability and fitness for a particular purpose
   are disclaimed.  In no event shall OpenWorks LLP be liable for any
   direct, indirect, incidental, special, exemplary, or consequential
   damages (including, but not limited to, procurement of substitute
   goods or services; loss of use, data, or profits; or business
   interruption) however caused and on any theory of liability,
   whether in contract, strict liability, or tort (including
   negligence or otherwise) arising in any way out of the use of this
   software, even if advised of the possibility of such damage.
*/

#ifndef __VEX_HOST_ARM_DEFS_H
#define __VEX_HOST_ARM_DEFS_H


/* --------- Registers. --------- */

/* The usual HReg abstraction.
   There are 16 general purpose regs.
*/

extern void ppHRegARM ( HReg );

extern HReg hregARM_R0  ( void );
extern HReg hregARM_R1  ( void );
extern HReg hregARM_R2  ( void );
extern HReg hregARM_R3  ( void );
extern HReg hregARM_R4  ( void );
extern HReg hregARM_R5  ( void );
extern HReg hregARM_R6  ( void );
extern HReg hregARM_R7  ( void );
extern HReg hregARM_R8  ( void );
extern HReg hregARM_R9  ( void );
extern HReg hregARM_R10 ( void );
extern HReg hregARM_R11 ( void );
extern HReg hregARM_R12 ( void );
extern HReg hregARM_R13 ( void );
extern HReg hregARM_R14 ( void );
extern HReg hregARM_R15 ( void );
extern HReg hregARM_D8  ( void );
extern HReg hregARM_D9  ( void );
extern HReg hregARM_D10 ( void );
extern HReg hregARM_D11 ( void );
extern HReg hregARM_D12 ( void );
extern HReg hregARM_S26 ( void );
extern HReg hregARM_S27 ( void );
extern HReg hregARM_S28 ( void );
extern HReg hregARM_S29 ( void );
extern HReg hregARM_S30 ( void );

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

extern HChar* showARMCondCode ( ARMCondCode );



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


/* --------- Reg or imm-8x4 operands --------- */
/* a.k.a (a very restricted form of) Shifter Operand,
   in the ARM parlance. */

typedef
   enum {
      ARMri84_I84=5,   /* imm8 `ror` (2 * imm4) */
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
      ARMri5_I5=7,   /* imm5, 1 .. 31 only (no zero!) */
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


/* --------- Instructions. --------- */

/* --------- */
typedef
   enum {
      ARMalu_ADD=10,   /* plain 32-bit add */
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

extern HChar* showARMAluOp ( ARMAluOp op );


typedef
   enum {
      ARMsh_SHL=20,
      ARMsh_SHR,
      ARMsh_SAR
   }
   ARMShiftOp;

extern HChar* showARMShiftOp ( ARMShiftOp op );


typedef
   enum {
      ARMun_NEG=30,
      ARMun_NOT,
      ARMun_CLZ
   }
   ARMUnaryOp;

extern HChar* showARMUnaryOp ( ARMUnaryOp op );


typedef
   enum {
      ARMmul_PLAIN=40,
      ARMmul_ZX,
      ARMmul_SX
   }
   ARMMulOp;

extern HChar* showARMMulOp ( ARMMulOp op );


typedef
   enum {
      ARMvfp_ADD=50,
      ARMvfp_SUB,
      ARMvfp_MUL,
      ARMvfp_DIV
   }
   ARMVfpOp;

extern HChar* showARMVfpOp ( ARMVfpOp op );


typedef
   enum {
      ARMvfpu_COPY=60,
      ARMvfpu_NEG,
      ARMvfpu_ABS,
      ARMvfpu_SQRT
   }
   ARMVfpUnaryOp;

extern HChar* showARMVfpUnaryOp ( ARMVfpUnaryOp op );


typedef
   enum {
      /* baseline */
      ARMin_Alu=70,
      ARMin_Shift,
      ARMin_Unary,
      ARMin_CmpOrTst,
      ARMin_Mov,
      ARMin_Imm32,
      ARMin_LdSt32,
      ARMin_LdSt16,
      ARMin_LdSt8U,
      ARMin_Ld8S,
      ARMin_Goto,
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
      ARMin_VXferD,
      ARMin_VXferS,
      ARMin_VCvtID,
      ARMin_FPSCR
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
         /* 32-bit load or store */
         struct {
            Bool       isLoad;
            HReg       rD;
            ARMAMode1* amode;
         } LdSt32;
         /* 16-bit load or store */
         struct {
            Bool       isLoad;
            Bool       signedLoad;
            HReg       rD;
            ARMAMode2* amode;
         } LdSt16;
         /* 8-bit (unsigned) load or store */
         struct {
            Bool       isLoad;
            HReg       rD;
            ARMAMode1* amode;
         } LdSt8U;
         /* 8-bit signed load */
         struct {
            HReg       rD;
            ARMAMode2* amode;
         } Ld8S;
         /* Pseudo-insn.  Go to guest address gnext, on given
            condition, which could be ARMcc_AL. */
         struct {
            IRJumpKind  jk;
            ARMCondCode cond;
            HReg        gnext;
         } Goto;
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
            HWord       target;
            Int         nArgRegs; /* # regs carrying args: 0 .. 4 */
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
         /* LDREX{,H,B} r0, [r1]
            Again, hardwired registers since this is not performance
            critical, and there are possibly constraints on the
            registers that we can't express in the register allocator.*/
         struct {
            Int  szB; /* currently only 4 is allowed */
         } LdrEX;
         /* STREX{,H,B} r0, r1, [r2]
            r0 = SC( [r2] = r1 )
            Ditto comment re fixed registers. */
         struct {
            Int  szB; /* currently only 4 is allowed */
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
         /* Move a 32-bit value to/from the FPSCR (FMXR, FMRX) */
         struct {
            Bool toFPSCR;
            HReg iReg;
         } FPSCR;
      } ARMin;
   }
   ARMInstr;


extern ARMInstr* ARMInstr_Alu      ( ARMAluOp, HReg, HReg, ARMRI84* );
extern ARMInstr* ARMInstr_Shift    ( ARMShiftOp, HReg, HReg, ARMRI5* );
extern ARMInstr* ARMInstr_Unary    ( ARMUnaryOp, HReg, HReg );
extern ARMInstr* ARMInstr_CmpOrTst ( Bool isCmp, HReg, ARMRI84* );
extern ARMInstr* ARMInstr_Mov      ( HReg, ARMRI84* );
extern ARMInstr* ARMInstr_Imm32    ( HReg, UInt );
extern ARMInstr* ARMInstr_LdSt32   ( Bool isLoad, HReg, ARMAMode1* );
extern ARMInstr* ARMInstr_LdSt16   ( Bool isLoad, Bool signedLoad,
                                     HReg, ARMAMode2* );
extern ARMInstr* ARMInstr_LdSt8U   ( Bool isLoad, HReg, ARMAMode1* );
extern ARMInstr* ARMInstr_Ld8S     ( HReg, ARMAMode2* );
extern ARMInstr* ARMInstr_Goto     ( IRJumpKind, ARMCondCode, HReg gnext );
extern ARMInstr* ARMInstr_CMov     ( ARMCondCode, HReg dst, ARMRI84* src );
extern ARMInstr* ARMInstr_Call     ( ARMCondCode, HWord, Int nArgRegs );
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
extern ARMInstr* ARMInstr_VXferD   ( Bool toD, HReg dD, HReg rHi, HReg rLo );
extern ARMInstr* ARMInstr_VXferS   ( Bool toS, HReg fD, HReg rLo );
extern ARMInstr* ARMInstr_VCvtID   ( Bool iToD, Bool syned,
                                     HReg dst, HReg src );
extern ARMInstr* ARMInstr_FPSCR    ( Bool toFPSCR, HReg iReg );

extern void ppARMInstr ( ARMInstr* );


/* Some functions that insulate the register allocator from details
   of the underlying instruction set. */
extern void getRegUsage_ARMInstr ( HRegUsage*, ARMInstr*, Bool );
extern void mapRegs_ARMInstr     ( HRegRemap*, ARMInstr*, Bool );
extern Bool isMove_ARMInstr      ( ARMInstr*, HReg*, HReg* );
extern Int  emit_ARMInstr        ( UChar* buf, Int nbuf, ARMInstr*, 
                                   Bool, void* dispatch );

extern void genSpill_ARM  ( /*OUT*/HInstr** i1, /*OUT*/HInstr** i2,
                            HReg rreg, Int offset, Bool );
extern void genReload_ARM ( /*OUT*/HInstr** i1, /*OUT*/HInstr** i2,
                            HReg rreg, Int offset, Bool );

extern void getAllocableRegs_ARM ( Int*, HReg** );
extern HInstrArray* iselSB_ARM   ( IRSB*, VexArch,
                                   VexArchInfo*, VexAbiInfo* );

#endif /* ndef __VEX_HOST_ARM_DEFS_H */

/*---------------------------------------------------------------*/
/*--- end                                     host_arm_defs.h ---*/
/*---------------------------------------------------------------*/
