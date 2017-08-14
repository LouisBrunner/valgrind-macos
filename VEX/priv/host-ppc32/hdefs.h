
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (host-ppc32/hdefs.h) is                       ---*/
/*--- Copyright (C) OpenWorks LLP.  All rights reserved.      ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of LibVEX, a library for dynamic binary
   instrumentation and translation.

   Copyright (C) 2004-2005 OpenWorks LLP.  All rights reserved.

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

#ifndef __LIBVEX_HOST_PPC32_HDEFS_H
#define __LIBVEX_HOST_PPC32_HDEFS_H

/* Num registers used for function calls */
#define PPC32_N_REGPARMS 8


/* --------- Registers. --------- */

/* The usual HReg abstraction.  There are 32 real int regs,
   32 real float regs, and 0 real vector regs. 
*/

extern void ppHRegPPC32 ( HReg );

extern HReg hregPPC32_GPR0  ( void );   // scratch reg / zero reg
extern HReg hregPPC32_GPR1  ( void );   // Stack Frame Pointer
extern HReg hregPPC32_GPR2  ( void );   // TOC pointer - not used
extern HReg hregPPC32_GPR3  ( void );
extern HReg hregPPC32_GPR4  ( void );
extern HReg hregPPC32_GPR5  ( void );
extern HReg hregPPC32_GPR6  ( void );
extern HReg hregPPC32_GPR7  ( void );
extern HReg hregPPC32_GPR8  ( void );
extern HReg hregPPC32_GPR9  ( void );
extern HReg hregPPC32_GPR10 ( void );
extern HReg hregPPC32_GPR11 ( void );
extern HReg hregPPC32_GPR12 ( void );
extern HReg hregPPC32_GPR13 ( void );   // thread specific pointer - not used
extern HReg hregPPC32_GPR14 ( void );
extern HReg hregPPC32_GPR15 ( void );
extern HReg hregPPC32_GPR16 ( void );
extern HReg hregPPC32_GPR17 ( void );
extern HReg hregPPC32_GPR18 ( void );
extern HReg hregPPC32_GPR19 ( void );
extern HReg hregPPC32_GPR20 ( void );
extern HReg hregPPC32_GPR21 ( void );
extern HReg hregPPC32_GPR22 ( void );
extern HReg hregPPC32_GPR23 ( void );
extern HReg hregPPC32_GPR24 ( void );
extern HReg hregPPC32_GPR25 ( void );
extern HReg hregPPC32_GPR26 ( void );
extern HReg hregPPC32_GPR27 ( void );
extern HReg hregPPC32_GPR28 ( void );
extern HReg hregPPC32_GPR29 ( void );
extern HReg hregPPC32_GPR30 ( void );
extern HReg hregPPC32_GPR31 ( void );    // GuestStatePtr

extern HReg hregPPC32_FPR0  ( void );
extern HReg hregPPC32_FPR1  ( void );
extern HReg hregPPC32_FPR2  ( void );
extern HReg hregPPC32_FPR3  ( void );
extern HReg hregPPC32_FPR4  ( void );
extern HReg hregPPC32_FPR5  ( void );
extern HReg hregPPC32_FPR6  ( void );
extern HReg hregPPC32_FPR7  ( void );
extern HReg hregPPC32_FPR8  ( void );
extern HReg hregPPC32_FPR9  ( void );
extern HReg hregPPC32_FPR10 ( void );
extern HReg hregPPC32_FPR11 ( void );
extern HReg hregPPC32_FPR12 ( void );
extern HReg hregPPC32_FPR13 ( void );
extern HReg hregPPC32_FPR14 ( void );
extern HReg hregPPC32_FPR15 ( void );
extern HReg hregPPC32_FPR16 ( void );
extern HReg hregPPC32_FPR17 ( void );
extern HReg hregPPC32_FPR18 ( void );
extern HReg hregPPC32_FPR19 ( void );
extern HReg hregPPC32_FPR20 ( void );
extern HReg hregPPC32_FPR21 ( void );
extern HReg hregPPC32_FPR22 ( void );
extern HReg hregPPC32_FPR23 ( void );
extern HReg hregPPC32_FPR24 ( void );
extern HReg hregPPC32_FPR25 ( void );
extern HReg hregPPC32_FPR26 ( void );
extern HReg hregPPC32_FPR27 ( void );
extern HReg hregPPC32_FPR28 ( void );
extern HReg hregPPC32_FPR29 ( void );
extern HReg hregPPC32_FPR30 ( void );
extern HReg hregPPC32_FPR31 ( void );

extern HReg hregPPC32_VR0  ( void );
extern HReg hregPPC32_VR1  ( void );
extern HReg hregPPC32_VR2  ( void );
extern HReg hregPPC32_VR3  ( void );
extern HReg hregPPC32_VR4  ( void );
extern HReg hregPPC32_VR5  ( void );
extern HReg hregPPC32_VR6  ( void );
extern HReg hregPPC32_VR7  ( void );
extern HReg hregPPC32_VR8  ( void );
extern HReg hregPPC32_VR9  ( void );
extern HReg hregPPC32_VR10 ( void );
extern HReg hregPPC32_VR11 ( void );
extern HReg hregPPC32_VR12 ( void );
extern HReg hregPPC32_VR13 ( void );
extern HReg hregPPC32_VR14 ( void );
extern HReg hregPPC32_VR15 ( void );
extern HReg hregPPC32_VR16 ( void );
extern HReg hregPPC32_VR17 ( void );
extern HReg hregPPC32_VR18 ( void );
extern HReg hregPPC32_VR19 ( void );
extern HReg hregPPC32_VR20 ( void );
extern HReg hregPPC32_VR21 ( void );
extern HReg hregPPC32_VR22 ( void );
extern HReg hregPPC32_VR23 ( void );
extern HReg hregPPC32_VR24 ( void );
extern HReg hregPPC32_VR25 ( void );
extern HReg hregPPC32_VR26 ( void );
extern HReg hregPPC32_VR27 ( void );
extern HReg hregPPC32_VR28 ( void );
extern HReg hregPPC32_VR29 ( void );
extern HReg hregPPC32_VR30 ( void );
extern HReg hregPPC32_VR31 ( void );

#define StackFramePtr hregPPC32_GPR1()
#define GuestStatePtr hregPPC32_GPR31()



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
   PPC32CondFlag;

typedef
   enum {   /* Maps bc bitfield BO */
      Pct_FALSE  = 0x4,
      Pct_TRUE   = 0xC,
      Pct_ALWAYS = 0x14
   }
   PPC32CondTest;

typedef
   struct {
      PPC32CondFlag flag;
      PPC32CondTest test;
   }
   PPC32CondCode;

extern HChar* showPPC32CondCode ( PPC32CondCode );

/* constructor */
extern PPC32CondCode mk_PPCCondCode ( PPC32CondTest, PPC32CondFlag );

/* false->true, true->false */
extern PPC32CondTest invertCondTest ( PPC32CondTest );




/* --------- Memory address expressions (amodes). --------- */

typedef
   enum {
     Pam_IR,        /* Immediate (signed 16-bit) + Reg */
     Pam_RR         /* Reg1 + Reg2     */
   }
   PPC32AModeTag;

typedef
   struct {
      PPC32AModeTag tag;
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
   PPC32AMode;

extern PPC32AMode* PPC32AMode_IR ( Int,  HReg );
extern PPC32AMode* PPC32AMode_RR ( HReg, HReg );

extern PPC32AMode* dopyPPC32AMode ( PPC32AMode* );

extern void ppPPC32AMode ( PPC32AMode* );


/* --------- Operand, which can be a reg or a u16/s16. --------- */
/* ("RH" == "Register or Halfword immediate") */
typedef 
   enum {
      Prh_Imm=1,
      Prh_Reg=2
   }
   PPC32RHTag;

typedef
   struct {
      PPC32RHTag tag;
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
   PPC32RH;

extern PPC32RH* PPC32RH_Imm ( Bool, UShort );
extern PPC32RH* PPC32RH_Reg ( HReg );

extern void ppPPC32RH ( PPC32RH* );


/* --------- Operand, which can be a reg or a u32. --------- */

typedef
   enum {
      Pri_Imm=3,
      Pri_Reg=4
   } 
   PPC32RITag;

typedef
   struct {
      PPC32RITag tag;
      union {
         UInt Imm;
         HReg Reg;
      }
      Pri;
   }
   PPC32RI;

extern PPC32RI* PPC32RI_Imm ( UInt );
extern PPC32RI* PPC32RI_Reg ( HReg );

extern void ppPPC32RI ( PPC32RI* );


/* --------- Instructions. --------- */

/* --------- */
typedef
   enum {
      Pun_NEG,
      Pun_NOT,
      Pun_CLZ
   }
   PPC32UnaryOp;

extern HChar* showPPC32UnaryOp ( PPC32UnaryOp );


/* --------- */
typedef 
   enum {
      Palu_INVALID,
      Palu_ADD, Palu_SUB,
      Palu_AND, Palu_OR, Palu_XOR,
      Palu_SHL, Palu_SHR, Palu_SAR, 
   }
   PPC32AluOp;

extern 
HChar* showPPC32AluOp ( PPC32AluOp, 
                        Bool /* is the 2nd operand an immediate? */ );


/* --------- */
typedef
   enum {
      Pfp_INVALID,
      /* Binary */
      Pfp_ADD, Pfp_SUB, Pfp_MUL, Pfp_DIV, 

      /* Unary */
      Pfp_SQRT, Pfp_ABS, Pfp_NEG, Pfp_MOV
   }
   PPC32FpOp;

extern HChar* showPPC32FpOp ( PPC32FpOp );


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
      Pav_AND, Pav_OR, Pav_XOR,   /* Bitwise */

      Pav_ADDUM, Pav_ADDUS,Pav_ADDSS,

      Pav_SUBUM, Pav_SUBUS, Pav_SUBSS,

      Pav_OMULU, Pav_OMULS, Pav_EMULU, Pav_EMULS,

      Pav_AVGU, Pav_AVGS,
      Pav_MAXU, Pav_MAXS,
      Pav_MINU, Pav_MINS,

      /* Compare (always affects CR field 6) */
      Pav_CMPEQU, Pav_CMPGTU, Pav_CMPGTS,

      /* Shift */
      Pav_SHL, Pav_SHR, Pav_SAR, Pav_ROTL,

      /* Pack */
      Pav_PACKUUM, Pav_PACKUUS, Pav_PACKSUS, Pav_PACKSSS,
      Pav_PACKPXL,

      /* Merge */
      Pav_MRGHI, Pav_MRGLO,

      /* Floating point binary */
      Pav_ADDF, Pav_SUBF, Pav_MULF,
      Pav_MAXF, Pav_MINF,
      Pav_CMPEQF, Pav_CMPGTF, Pav_CMPGEF,

//..       /* Floating point unary */
//..       Xsse_RCPF, Xsse_RSQRTF, Xsse_SQRTF,
   }
   PPC32AvOp;

extern HChar* showPPC32AvOp ( PPC32AvOp );


/* --------- */
typedef
   enum {
      Pin_LI32,       /* load 32-bit immediate (fake insn) */
      Pin_Alu32,      /* 32-bit add/sub/and/or/xor/shl/shr/sar */
      Pin_Cmp32,      /* 32-bit compare */
      Pin_Unary32,    /* 32-bit not, neg, clz */
      Pin_MulL,       /* widening multiply */
      Pin_Div,        /* div */
      Pin_Call,       /* call to address in register */
      Pin_Goto,       /* conditional/unconditional jmp to dst */
      Pin_CMov32,     /* conditional move */
      Pin_Load,       /* load a 8|16|32 bit value from mem */
      Pin_Store,      /* store a 8|16|32 bit value to mem */
      Pin_Set32,      /* convert condition code to 32-bit value */
      Pin_MfCR,       /* move from condition register to GPR */
      Pin_MFence,     /* mem fence */

      Pin_FpUnary,    /* FP unary op */
      Pin_FpBinary,   /* FP binary op */
      Pin_FpLdSt,     /* FP load/store */
      Pin_FpF64toF32, /* FP round IEEE754 double to IEEE754 single */
      Pin_FpF64toI32, /* FP round IEEE754 double to 32-bit integer */
      Pin_FpCMov,     /* FP floating point conditional move */
      Pin_FpLdFPSCR,  /* mtfsf */
      Pin_FpCmp,      /* FP compare, generating value into int reg */
      Pin_RdWrLR,     /* Read/Write Link Register */

//    Pin_AvConst,    /* Generate restricted AV literal */
      Pin_AvLdSt,     /* AV load/store (kludging for AMode_IR) */
      Pin_AvUnary,    /* AV unary general reg=>reg */

      Pin_AvBinary,   /* AV binary general reg,reg=>reg */
      Pin_AvBin8x16,  /* AV binary, 8x4 */
      Pin_AvBin16x8,  /* AV binary, 16x4 */
      Pin_AvBin32x4,  /* AV binary, 32x4 */

      Pin_AvBin32Fx4, /* AV FP binary, 32Fx4 */

      Pin_AvPerm,     /* AV permute (shuffle) */
      Pin_AvSel,      /* AV select */
      Pin_AvShlDbl,   /* AV shift-left double by imm */
      Pin_AvSplat,    /* One elem repeated throughout dst */
      Pin_AvLdVSCR,   /* mtvscr */
      Pin_AvCMov      /* AV conditional move */
   }
   PPC32InstrTag;

/* Destinations are on the LEFT (first operand) */

typedef
   struct {
      PPC32InstrTag tag;
      union {
         /* Get a 32-bit literal into a register.  May turn into one or
	    two real insns. */
         struct {
            HReg dst;
            UInt imm32;
         } LI32;
         /* Integer add/sub/and/or/xor/shl/shr/sar.  Limitations:
            - For add, the immediate, if it exists, is a signed 16.
            - For sub, the immediate, if it exists, is a signed 16
              which may not be -32768, since no such instruction 
              exists, and so we have to emit addi with +32768, but 
              that is not possible.
            - For and/or/xor,  the immediate, if it exists, 
              is an unsigned 16.
            - For shr/shr/sar, the immediate, if it exists,
              is a signed 5-bit value between 1 and 31 inclusive.
         */
         struct {
            PPC32AluOp op;
            HReg       dst;
            HReg       srcL;
            PPC32RH*   srcR;
         } Alu32;
         /* If signed, the immediate, if it exists, is a signed 16,
            else it is an unsigned 16. */
         struct {
            Bool     syned;
            UInt     crfD;
            HReg     srcL;
            PPC32RH* srcR;
         } Cmp32;
         /* Not and Neg */
         struct {
            PPC32UnaryOp op;
            HReg         dst;
            HReg         src;
         } Unary32;
         struct {
            Bool syned;  /* meaningless if hi32==False */
            Bool hi32;   /* False=>low, True=>high */
            HReg dst;
            HReg srcL;
            HReg srcR;
         } MulL;
         /* ppc32 div/divu instruction. */
         struct {
            Bool syned;
            HReg dst;
            HReg srcL;
            HReg srcR;
         } Div;
         /* Pseudo-insn.  Call target (an absolute address), on given
            condition (which could be Pct_ALWAYS). */
         struct {
            PPC32CondCode cond;
            Addr32        target;
            Int           regparms; /* 0 .. 9 */
         } Call;
         /* Pseudo-insn.  Goto dst, on given condition (which could be
            Pct_ALWAYS). */
         struct {
            IRJumpKind    jk;
            PPC32CondCode cond;
            PPC32RI*      dst;
         } Goto;
         /* Mov src to dst on the given condition, which may not
            be the bogus Pct_ALWAYS. */
         struct {
            PPC32CondCode cond;
            HReg          dst;
            PPC32RI*      src;
         } CMov32;
         /* Sign/Zero extending loads.  Dst size is always 32 bits. */
         struct {
            UChar       sz; /* 1|2|4 */
            Bool        syned;
            HReg        dst;
            PPC32AMode* src;
         } Load;
         /* 32/16/8 bit stores */
         struct {
            UChar       sz; /* 1|2|4 */
            PPC32AMode* dst;
            HReg        src;
         } Store;
         /* Convert a ppc32 condition code to a 32-bit value (0 or 1). */
         struct {
            PPC32CondCode cond;
            HReg          dst;
         } Set32;
         /* Move the entire CR to a GPR */
         struct {
            HReg dst;
         } MfCR;
         /* Mem fence.  In short, an insn which flushes all preceding
            loads and stores as much as possible before continuing.
            On PPC32 we emit a "sync". */
         struct {
         } MFence;

         /* PPC32 Floating point */
         struct {
            PPC32FpOp op;
            HReg      dst;
            HReg      src;
         } FpUnary;
         struct {
            PPC32FpOp op;
            HReg      dst;
            HReg      srcL;
            HReg      srcR;
         } FpBinary;
         struct {
            Bool        isLoad;
            UChar       sz; /* only 4 (IEEE single) or 8 (IEEE double) */
            HReg        reg;
            PPC32AMode* addr;
         } FpLdSt;
         /* By observing the current FPU rounding mode, round src into dst,
            re-interpreting dst to an IEEE754 32-bit (float) type. */
         struct {
            HReg src;
            HReg dst;
         } FpF64toF32;
         /* By observing the current FPU rounding mode, round src into dst,
            re-interpreting dst to an 32-bit integer type. */
         struct {
            HReg src;
            HReg dst;
         } FpF64toI32;
         /* Mov src to dst on the given condition, which may not
            be the bogus Xcc_ALWAYS. */
         struct {
            PPC32CondCode cond;
            HReg          dst;
            HReg          src;
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
            Bool        isLoad;
            UChar       sz;      /* 8|16|32|128 */
            HReg        reg;
            PPC32AMode* addr;
         } AvLdSt;
         struct {
            PPC32AvOp op;
            HReg      dst;
            HReg      src;
         } AvUnary;
         struct {
            PPC32AvOp op;
            HReg      dst;
            HReg      srcL;
            HReg      srcR;
         } AvBinary;
         struct {
            PPC32AvOp op;
            HReg      dst;
            HReg      srcL;
            HReg      srcR;
         } AvBin8x16;
         struct {
            PPC32AvOp op;
            HReg      dst;
            HReg      srcL;
            HReg      srcR;
         } AvBin16x8;
         struct {
            PPC32AvOp op;
            HReg      dst;
            HReg      srcL;
            HReg      srcR;
         } AvBin32x4;
         struct {
            PPC32AvOp op;
            HReg      dst;
            HReg      srcL;
            HReg      srcR;
         } AvBin32Fx4;
         /* Perm,Sel,SlDbl,Splat are all weird AV permutations */
         struct {
            HReg ctl;
            HReg dst;
            HReg srcL;
            HReg srcR;
         } AvPerm;
         struct {
            HReg ctl;
            HReg dst;
            HReg srcL;
            HReg srcR;
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
            PPC32RI* src;
         } AvSplat;
         /* Mov src to dst on the given condition, which may not
            be the bogus Xcc_ALWAYS. */
         struct {
            PPC32CondCode cond;
            HReg          dst;
            HReg          src;
         } AvCMov;
         /* Load AltiVec Status & Control Register */
         struct {
            HReg src;
         } AvLdVSCR;
       } Pin;
   }
   PPC32Instr;


extern PPC32Instr* PPC32Instr_LI32       ( HReg, UInt );
extern PPC32Instr* PPC32Instr_Alu32      ( PPC32AluOp, HReg, HReg, PPC32RH* );
extern PPC32Instr* PPC32Instr_Cmp32      ( Bool,       UInt, HReg, PPC32RH* );
extern PPC32Instr* PPC32Instr_Unary32    ( PPC32UnaryOp op, HReg dst, HReg src );
extern PPC32Instr* PPC32Instr_MulL       ( Bool syned, Bool hi32, HReg, HReg, HReg );
extern PPC32Instr* PPC32Instr_Div        ( Bool syned, HReg dst, HReg srcL, HReg srcR );
extern PPC32Instr* PPC32Instr_Call       ( PPC32CondCode, Addr32, Int );
extern PPC32Instr* PPC32Instr_Goto       ( IRJumpKind, PPC32CondCode cond, PPC32RI* dst );
extern PPC32Instr* PPC32Instr_CMov32     ( PPC32CondCode, HReg dst, PPC32RI* src );
extern PPC32Instr* PPC32Instr_Load       ( UChar sz, Bool syned,
                                           HReg dst, PPC32AMode* src );
extern PPC32Instr* PPC32Instr_Store      ( UChar sz, PPC32AMode* dst, HReg src );
extern PPC32Instr* PPC32Instr_Set32      ( PPC32CondCode cond, HReg dst );
extern PPC32Instr* PPC32Instr_MfCR       ( HReg dst );
extern PPC32Instr* PPC32Instr_MFence     ( void );

extern PPC32Instr* PPC32Instr_FpUnary    ( PPC32FpOp op, HReg dst, HReg src );
extern PPC32Instr* PPC32Instr_FpBinary   ( PPC32FpOp op, HReg dst, HReg srcL, HReg srcR );
extern PPC32Instr* PPC32Instr_FpLdSt     ( Bool isLoad, UChar sz, HReg, PPC32AMode* );
extern PPC32Instr* PPC32Instr_FpF64toF32 ( HReg dst, HReg src );
extern PPC32Instr* PPC32Instr_FpF64toI32 ( HReg dst, HReg src );
extern PPC32Instr* PPC32Instr_FpCMov     ( PPC32CondCode, HReg dst, HReg src );
extern PPC32Instr* PPC32Instr_FpLdFPSCR  ( HReg src );
extern PPC32Instr* PPC32Instr_FpCmp      ( HReg dst, HReg srcL, HReg srcR );

extern PPC32Instr* PPC32Instr_RdWrLR     ( Bool wrLR, HReg gpr );

extern PPC32Instr* PPC32Instr_AvLdSt     ( Bool isLoad, UChar sz, HReg, PPC32AMode* );
extern PPC32Instr* PPC32Instr_AvUnary    ( PPC32AvOp op, HReg dst, HReg src );
extern PPC32Instr* PPC32Instr_AvBinary   ( PPC32AvOp op, HReg dst, HReg srcL, HReg srcR );
extern PPC32Instr* PPC32Instr_AvBin8x16  ( PPC32AvOp op, HReg dst, HReg srcL, HReg srcR );
extern PPC32Instr* PPC32Instr_AvBin16x8  ( PPC32AvOp op, HReg dst, HReg srcL, HReg srcR );
extern PPC32Instr* PPC32Instr_AvBin32x4  ( PPC32AvOp op, HReg dst, HReg srcL, HReg srcR );
extern PPC32Instr* PPC32Instr_AvBin32Fx4 ( PPC32AvOp op, HReg dst, HReg srcL, HReg srcR );
extern PPC32Instr* PPC32Instr_AvPerm     ( HReg ctl, HReg dst, HReg srcL, HReg srcR );
extern PPC32Instr* PPC32Instr_AvSel      ( HReg ctl, HReg dst, HReg srcL, HReg srcR );
extern PPC32Instr* PPC32Instr_AvShlDbl   ( UChar shift, HReg dst, HReg srcL, HReg srcR );
extern PPC32Instr* PPC32Instr_AvSplat    ( UChar sz, HReg dst, PPC32RI* src );
extern PPC32Instr* PPC32Instr_AvCMov     ( PPC32CondCode, HReg dst, HReg src );
extern PPC32Instr* PPC32Instr_AvLdVSCR   ( HReg src );

extern void ppPPC32Instr ( PPC32Instr* );

/* Some functions that insulate the register allocator from details
   of the underlying instruction set. */
extern void         getRegUsage_PPC32Instr ( HRegUsage*, PPC32Instr* );
extern void         mapRegs_PPC32Instr     ( HRegRemap*, PPC32Instr* );
extern Bool         isMove_PPC32Instr      ( PPC32Instr*, HReg*, HReg* );
extern Int          emit_PPC32Instr        ( UChar* buf, Int nbuf, PPC32Instr* );
extern PPC32Instr*  genSpill_PPC32         ( HReg rreg, UShort offsetB );
extern PPC32Instr*  genReload_PPC32        ( HReg rreg, UShort offsetB );
extern void         getAllocableRegs_PPC32 ( Int*, HReg** );
extern HInstrArray* iselBB_PPC32           ( IRBB*, VexArchInfo* );

#endif /* ndef __LIBVEX_HOST_PPC32_HDEFS_H */

/*---------------------------------------------------------------*/
/*--- end                                  host-ppc32/hdefs.h ---*/
/*---------------------------------------------------------------*/
