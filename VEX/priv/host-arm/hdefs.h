
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (hdefs.h) is                                  ---*/
/*--- Copyright (C) OpenWorks LLP.  All rights reserved.      ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of LibVEX, a library for dynamic binary
   instrumentation and translation.

   Copyright (C) 2004-2008 OpenWorks LLP.  All rights reserved.

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

   Neither the names of the U.S. Department of Energy nor the
   University of California nor the names of its contributors may be
   used to endorse or promote products derived from this software
   without prior written permission.
*/

#ifndef __LIBVEX_ARMH_DEFS_H
#define __LIBVEX_ARMH_DEFS_H


/* --------- Registers. --------- */

/* The usual HReg abstraction.
   There are 16 general purpose regs.
*/

extern void ppHRegARM ( HReg );

extern HReg hregARM_R0 ( void );
extern HReg hregARM_R1 ( void );
extern HReg hregARM_R2 ( void );
extern HReg hregARM_R3 ( void );
extern HReg hregARM_R4 ( void );
extern HReg hregARM_R5 ( void );
extern HReg hregARM_R6 ( void );
extern HReg hregARM_R7 ( void );
extern HReg hregARM_R8 ( void );
extern HReg hregARM_R9 ( void );
extern HReg hregARM_R10 ( void );
extern HReg hregARM_R11 ( void );
extern HReg hregARM_R12 ( void );
extern HReg hregARM_R13 ( void );
extern HReg hregARM_R14 ( void );
extern HReg hregARM_R15 ( void );


#define GET_BP_REG() hregARM_R12()  // x86 ebp
#define GET_SP_REG() hregARM_R13()  // x86 esp
#define GET_LN_REG() hregARM_R14()
#define GET_PC_REG() hregARM_R15()




/* --------- Condition codes, Intel encoding. --------- */

typedef
   enum {
      ARMccEQ     = 0,  /* equal                               : Z=1 */
      ARMccNE     = 1,  /* not equal                           : Z=0 */

      ARMccHS     = 2,  /* >=u (higher or same)                : C=1 */
      ARMccLO     = 3,  /* <u  (lower)                         : C=0 */

      ARMccMI     = 4,  /* minus (negative)                    : N=1 */
      ARMccPL     = 5,  /* plus (zero or +ve)                  : N=0 */

      ARMccVS     = 6,  /* overflow                            : V=1 */
      ARMccVC     = 7,  /* no overflow                         : V=0 */

      ARMccHI     = 8,  /* >u   (higher)                       : C=1 && Z=0 */
      ARMccLS     = 9,  /* <=u  (lower or same)                : C=0 || Z=1 */

      ARMccGE     = 10, /* >=s (signed greater or equal)       : N=V */
      ARMccLT     = 11, /* <s  (signed less than)              : N!=V */

      ARMccGT     = 12, /* >s  (signed greater)                : Z=0 && N=V */
      ARMccLE     = 13, /* <=s (signed less or equal)          : Z=1 || N!=V */

      ARMccAL     = 14, /* always (unconditional) */
      ARMccNV     = 15  /* never (basically undefined meaning), deprecated */
   }
   ARMCondCode;

extern HChar* showARMCondCode ( ARMCondCode );




/* --------- Immediate types. --------- */

typedef UInt ARMImm4;
typedef UInt ARMImm5;
typedef UInt ARMImm8;
typedef UInt ARMImm12;
typedef UInt ARMImm24; // Branch imm
typedef
  struct {
      ARMImm8 imm;
      ARMImm4 rot;
  }
  ARMImm12A;   /* extended (rotated) immedate */

extern Bool mk_ARMImm12A ( UInt, ARMImm12A*);



/* --------- Memory address expressions (amodes). --------- */

/* --- Addressing Mode 1 --- */
typedef
   enum {
       ARMam1_I12A,    /* Imm12A: extended (rotated) immedate */
       ARMam1_ShlI,    /* ShlI  reg  Imm5 */
       ARMam1_ShrI,    /* ShrI  reg  Imm5 */
       ARMam1_SarI,    /* SarI  reg  Imm5 */
       ARMam1_ShlR,    /* ShlR  reg  reg */
       ARMam1_ShrR,    /* ShrR  reg  reg */
       ARMam1_SarR,    /* SarR  reg  reg */
   }
   ARMAMode1Tag;

typedef
   struct {
       ARMAMode1Tag tag;
       union {
	   struct {
	       ARMImm12A imm;
	   } I12A;
	   struct {
	       HReg Rm;
	       ARMImm5 imm;
	   } ShlI;
	   struct {
	       HReg Rm;
	       ARMImm5 imm;
	   } ShrI;
	   struct {
	       HReg Rm;
	       ARMImm5 imm;
	   } SarI;
	   struct {
	       HReg Rm;
	       HReg Rs;
	   } ShlR;
	   struct {
	       HReg Rm;
	       HReg Rs;
	   } ShrR;
	   struct {
	       HReg Rm;
	       HReg Rs;
	   } SarR;
       } ARMam1;
   }
   ARMAMode1;

extern ARMAMode1* ARMAMode1_I12A ( ARMImm12A );
extern ARMAMode1* ARMAMode1_ShlI ( HReg, ARMImm5 );
extern ARMAMode1* ARMAMode1_ShrI ( HReg, ARMImm5 );
extern ARMAMode1* ARMAMode1_SarI ( HReg, ARMImm5 );
extern ARMAMode1* ARMAMode1_ShlR ( HReg, HReg );
extern ARMAMode1* ARMAMode1_ShrR ( HReg, HReg );
extern ARMAMode1* ARMAMode1_SarR ( HReg, HReg );

extern ARMAMode1* dopyARMAMode1 ( ARMAMode1* );

extern void ppARMAMode1 ( ARMAMode1* );



/* --- Addressing Mode 2 --- */
typedef
   enum {
     ARMam2_RI,      /* Reg +/- Imm12 */
     ARMam2_RR,       /* Reg +/- Reg */
     ARMam2_RRS,       /* Reg +/- (Reg << Imm5) */
   }
   ARMAMode2Tag;

typedef
   struct {
      ARMAMode2Tag tag;
      union {
         struct {
            HReg Rn;
            ARMImm12 imm;
         } RI;
         struct {
            HReg Rn;
            HReg Rm;
	 } RR;
         struct {
            HReg Rn;
            HReg Rm;
            ARMImm5 shift;
	 } RRS;
      } ARMam2;
   }
   ARMAMode2;

extern ARMAMode2* ARMAMode2_RI ( HReg, ARMImm12 );
extern ARMAMode2* ARMAMode2_RR ( HReg, HReg );
extern ARMAMode2* ARMAMode2_RRS ( HReg, HReg, ARMImm5 );

extern ARMAMode2* dopyARMAMode2 ( ARMAMode2* );

extern void ppARMAMode2 ( ARMAMode2* );


/* --- Addressing Mode 3 --- */
typedef
   enum {
     ARMam3_RI,       /* Reg +/- Imm8 */
     ARMam3_RR,       /* Reg +/- Reg */
   }
   ARMAMode3Tag;

typedef
   struct {
      ARMAMode3Tag tag;
      union {
         struct {
            HReg Rn;
            ARMImm8 imm;
         } RI;
         struct {
            HReg Rn;
            HReg Rm;
	 } RR;
      } ARMam3;
   }
   ARMAMode3;


extern ARMAMode3* ARMAMode3_RI ( HReg, ARMImm8 );
extern ARMAMode3* ARMAMode3_RR ( HReg, HReg );

extern ARMAMode3* dopyARMAMode3 ( ARMAMode3* );

extern void ppARMAMode3 ( ARMAMode3* );




/* ------ Branch destination ------ */
typedef
   enum {
       ARMbdImm,
       ARMbdReg
   }
   ARMBranchTag;

typedef
  struct {
      ARMBranchTag tag;
      union {
	  struct {
	      ARMImm24 imm24;
	  } Imm;
	  struct {
	      HReg reg;
	  } Reg;
      } ARMbd;
  }
  ARMBranchDest;

extern ARMBranchDest* ARMBranchDest_Imm ( ARMImm24 );
extern ARMBranchDest* ARMBranchDest_Reg ( HReg );

extern void ppARMBranchDest ( ARMBranchDest* );


/* --------- Instructions. --------- */


/* --- DPI's --- */
typedef
   enum {
       ARMalu_AND, ARMalu_ORR, ARMalu_EOR, ARMalu_BIC, // Logic
       ARMalu_SUB, ARMalu_RSB, ARMalu_ADD, ARMalu_ADC, ARMalu_SBC, ARMalu_RSC, // Arith
       ARMalu_TST, ARMalu_TEQ, ARMalu_CMP, ARMalu_CMN, // Test
       ARMalu_MOV, ARMalu_MVN  // Move
   }
   ARMAluOp;

extern HChar* showARMAluOp ( ARMAluOp );



/* --------- */
typedef
   enum {
       ARMin_DPCmp,
       ARMin_DPInstr1,
       ARMin_DPInstr2,

       ARMin_LoadUB,
       ARMin_StoreB,
       ARMin_LoadW,
       ARMin_StoreW,
       ARMin_LoadSB,
       ARMin_LoadUH,
       ARMin_LoadSH,
       ARMin_StoreH,

       ARMin_Branch,
       ARMin_BranchL,
       ARMin_Literal
   }
   ARMInstrTag;


typedef
   struct {
      ARMInstrTag tag;
      union {
	  /* Addressing Mode 1 */
	  struct {
	      ARMAluOp op;
	      HReg Rn;
	      ARMAMode1* shifter_op;
	  } DPCmp;        // test instrs - compare Rd with RIS and set flags
	  struct {
	      ARMAluOp op;
	      HReg Rd;
	      ARMAMode1* shifter_op;
	  } DPInstr1;      // 1 reg: Mov
	  struct {
	      ARMAluOp op;
	      HReg Rd;
	      HReg Rn;
	      ARMAMode1* shifter_op;
	  } DPInstr2;      // 2 regs: Logic, Arith, Bic

	  /* Addressing Mode 2 */
	  struct {
	      HReg Rd;
	      ARMAMode2* addr_mode;
	  } LoadUB;
	  struct {
	      HReg Rd;
	      ARMAMode2* addr_mode;
	  } StoreB;
	  struct {
	      HReg Rd;
	      ARMAMode2* addr_mode;
	  } LoadW;
	  struct {
	      HReg Rd;
	      ARMAMode2* addr_mode;
	  } StoreW;

	  /* Addressing Mode 3 */
	  struct {
	      HReg Rd;
	      ARMAMode3* addr_mode;
	  } LoadSB;
	  struct {
	      HReg Rd;
	      ARMAMode3* addr_mode;
	  } LoadUH;
	  struct {
	      HReg Rd;
	      ARMAMode3* addr_mode;
	  } LoadSH;
	  struct {
	      HReg Rd;
	      ARMAMode3* addr_mode;
	  } StoreH;


	  /* Branch */
	  struct {
	      ARMCondCode cond;
	      ARMBranchDest* dest;
	  } Branch;
	  struct {
	      ARMCondCode cond;
	      ARMBranchDest* dest;
	  } BranchL;

	  /* Literal */
	  struct {
	      HReg reg;
	      UInt imm;
	  } Literal;       // -- reg = imm

      } ARMin;
   }
   ARMInstr;

extern ARMInstr* ARMInstr_DPCmp     ( ARMAluOp, HReg, ARMAMode1* );
extern ARMInstr* ARMInstr_DPInstr1  ( ARMAluOp, HReg, ARMAMode1* );
extern ARMInstr* ARMInstr_DPInstr2  ( ARMAluOp, HReg, HReg, ARMAMode1* );

extern ARMInstr* ARMInstr_LoadUB    ( HReg, ARMAMode2* );
extern ARMInstr* ARMInstr_StoreB    ( HReg, ARMAMode2* );
extern ARMInstr* ARMInstr_LoadW     ( HReg, ARMAMode2* );
extern ARMInstr* ARMInstr_StoreW    ( HReg, ARMAMode2* );
extern ARMInstr* ARMInstr_LoadSB    ( HReg, ARMAMode3* );
extern ARMInstr* ARMInstr_LoadUH    ( HReg, ARMAMode3* );
extern ARMInstr* ARMInstr_LoadSH    ( HReg, ARMAMode3* );
extern ARMInstr* ARMInstr_StoreH    ( HReg, ARMAMode3* );

extern ARMInstr* ARMInstr_Branch    ( ARMCondCode, ARMBranchDest* );
extern ARMInstr* ARMInstr_BranchL   ( ARMCondCode, ARMBranchDest* );

extern ARMInstr* ARMInstr_Literal   ( HReg, UInt );

extern void ppARMInstr ( ARMInstr* );


/* Some functions that insulate the register allocator from details
   of the underlying instruction set. */
extern void         getAllocableRegs_ARM ( Int*, HReg** );
extern void         getRegUsage_ARMInstr ( HRegUsage*, ARMInstr* );
extern void         mapRegs_ARMInstr     ( HRegRemap*, ARMInstr* );
extern Bool         isMove_ARMInstr      ( ARMInstr*, HReg*, HReg* );
extern Int          emit_ARMInstr        ( UChar* buf, Int nbuf, ARMInstr* );
extern ARMInstr*    genSpill_ARM         ( HReg rreg, Int offset );
extern ARMInstr*    genReload_ARM        ( HReg rreg, Int offset );
extern void         getAllocableRegs_ARM ( Int*, HReg** );
extern HInstrArray* iselSB_ARM           ( IRSB* );

#endif /* ndef __LIBVEX_ARMH_DEFS_H */

/*---------------------------------------------------------------*/
/*--- end                                         arm hdefs.h ---*/
/*---------------------------------------------------------------*/
