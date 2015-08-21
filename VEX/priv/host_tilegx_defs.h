
/*---------------------------------------------------------------*/
/*--- begin                                host_tilegx_defs.h ---*/
/*---------------------------------------------------------------*/

/*
  This file is part of Valgrind, a dynamic binary instrumentation
  framework.

  Copyright (C) 2010-2015 Tilera Corp.

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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
  02111-1307, USA.

  The GNU General Public License is contained in the file COPYING.
*/

 /* Contributed by Zhi-Gang Liu <zliu at tilera dot com> */

#ifndef __VEX_HOST_TILEGX_DEFS_H
#define __VEX_HOST_TILEGX_DEFS_H

#include "tilegx_disasm.h"

/* Num registers used for function calls */
#define TILEGX_N_REGPARMS 10

/* --------- Registers. --------- */

/* The usual HReg abstraction.
   There are 56 general purpose regs.
*/

#define ST_IN static inline

ST_IN HReg hregTILEGX_R30 ( void ) { return mkHReg(False, HRcInt64,  30,  0); }
ST_IN HReg hregTILEGX_R31 ( void ) { return mkHReg(False, HRcInt64,  31,  1); }
ST_IN HReg hregTILEGX_R32 ( void ) { return mkHReg(False, HRcInt64,  32,  2); }
ST_IN HReg hregTILEGX_R33 ( void ) { return mkHReg(False, HRcInt64,  33,  3); }
ST_IN HReg hregTILEGX_R34 ( void ) { return mkHReg(False, HRcInt64,  34,  4); }
ST_IN HReg hregTILEGX_R35 ( void ) { return mkHReg(False, HRcInt64,  35,  5); }
ST_IN HReg hregTILEGX_R36 ( void ) { return mkHReg(False, HRcInt64,  36,  6); }
ST_IN HReg hregTILEGX_R37 ( void ) { return mkHReg(False, HRcInt64,  37,  7); }
ST_IN HReg hregTILEGX_R38 ( void ) { return mkHReg(False, HRcInt64,  38,  8); }
ST_IN HReg hregTILEGX_R39 ( void ) { return mkHReg(False, HRcInt64,  39,  9); }

ST_IN HReg hregTILEGX_R40 ( void ) { return mkHReg(False, HRcInt64,  40,  10); }
ST_IN HReg hregTILEGX_R41 ( void ) { return mkHReg(False, HRcInt64,  41,  11); }
ST_IN HReg hregTILEGX_R42 ( void ) { return mkHReg(False, HRcInt64,  42,  12); }
ST_IN HReg hregTILEGX_R43 ( void ) { return mkHReg(False, HRcInt64,  43,  13); }
ST_IN HReg hregTILEGX_R44 ( void ) { return mkHReg(False, HRcInt64,  44,  14); }
ST_IN HReg hregTILEGX_R45 ( void ) { return mkHReg(False, HRcInt64,  45,  15); }
ST_IN HReg hregTILEGX_R46 ( void ) { return mkHReg(False, HRcInt64,  46,  16); }
ST_IN HReg hregTILEGX_R47 ( void ) { return mkHReg(False, HRcInt64,  47,  17); }
ST_IN HReg hregTILEGX_R48 ( void ) { return mkHReg(False, HRcInt64,  48,  18); }
ST_IN HReg hregTILEGX_R49 ( void ) { return mkHReg(False, HRcInt64,  49,  19); }

ST_IN HReg hregTILEGX_R10 ( void ) { return mkHReg(False, HRcInt64,  10,  20); }
ST_IN HReg hregTILEGX_R13 ( void ) { return mkHReg(False, HRcInt64,  13,  21); }
ST_IN HReg hregTILEGX_R14 ( void ) { return mkHReg(False, HRcInt64,  14,  22); }
ST_IN HReg hregTILEGX_R15 ( void ) { return mkHReg(False, HRcInt64,  15,  23); }
ST_IN HReg hregTILEGX_R16 ( void ) { return mkHReg(False, HRcInt64,  16,  24); }
ST_IN HReg hregTILEGX_R17 ( void ) { return mkHReg(False, HRcInt64,  17,  25); }
ST_IN HReg hregTILEGX_R18 ( void ) { return mkHReg(False, HRcInt64,  18,  26); }
ST_IN HReg hregTILEGX_R19 ( void ) { return mkHReg(False, HRcInt64,  19,  27); }
ST_IN HReg hregTILEGX_R20 ( void ) { return mkHReg(False, HRcInt64,  20,  28); }

ST_IN HReg hregTILEGX_R21 ( void ) { return mkHReg(False, HRcInt64,  21,  29); }
ST_IN HReg hregTILEGX_R22 ( void ) { return mkHReg(False, HRcInt64,  22,  30); }
ST_IN HReg hregTILEGX_R23 ( void ) { return mkHReg(False, HRcInt64,  23,  31); }
ST_IN HReg hregTILEGX_R24 ( void ) { return mkHReg(False, HRcInt64,  24,  32); }
ST_IN HReg hregTILEGX_R25 ( void ) { return mkHReg(False, HRcInt64,  25,  33); }
ST_IN HReg hregTILEGX_R26 ( void ) { return mkHReg(False, HRcInt64,  26,  34); }
ST_IN HReg hregTILEGX_R27 ( void ) { return mkHReg(False, HRcInt64,  27,  35); }
ST_IN HReg hregTILEGX_R28 ( void ) { return mkHReg(False, HRcInt64,  28,  36); }
ST_IN HReg hregTILEGX_R29 ( void ) { return mkHReg(False, HRcInt64,  29,  37); }

ST_IN HReg hregTILEGX_R0 ( void ) { return mkHReg(False, HRcInt64,  0,  38); }
ST_IN HReg hregTILEGX_R1 ( void ) { return mkHReg(False, HRcInt64,  1,  39); }
ST_IN HReg hregTILEGX_R2 ( void ) { return mkHReg(False, HRcInt64,  2,  40); }
ST_IN HReg hregTILEGX_R3 ( void ) { return mkHReg(False, HRcInt64,  3,  41); }
ST_IN HReg hregTILEGX_R4 ( void ) { return mkHReg(False, HRcInt64,  4,  42); }
ST_IN HReg hregTILEGX_R5 ( void ) { return mkHReg(False, HRcInt64,  5,  43); }
ST_IN HReg hregTILEGX_R6 ( void ) { return mkHReg(False, HRcInt64,  6,  44); }
ST_IN HReg hregTILEGX_R7 ( void ) { return mkHReg(False, HRcInt64,  7,  45); }
ST_IN HReg hregTILEGX_R8 ( void ) { return mkHReg(False, HRcInt64,  8,  46); }
ST_IN HReg hregTILEGX_R9 ( void ) { return mkHReg(False, HRcInt64,  9,  47); }

ST_IN HReg hregTILEGX_R11 ( void ) { return mkHReg(False, HRcInt64,  11,  48); }
ST_IN HReg hregTILEGX_R12 ( void ) { return mkHReg(False, HRcInt64,  12,  49); }
ST_IN HReg hregTILEGX_R50 ( void ) { return mkHReg(False, HRcInt64,  50,  50); }
ST_IN HReg hregTILEGX_R51 ( void ) { return mkHReg(False, HRcInt64,  51,  51); }
ST_IN HReg hregTILEGX_R52 ( void ) { return mkHReg(False, HRcInt64,  52,  52); }
ST_IN HReg hregTILEGX_R53 ( void ) { return mkHReg(False, HRcInt64,  53,  53); }
ST_IN HReg hregTILEGX_R54 ( void ) { return mkHReg(False, HRcInt64,  54,  54); }
ST_IN HReg hregTILEGX_R55 ( void ) { return mkHReg(False, HRcInt64,  55,  55); }
ST_IN HReg hregTILEGX_R63 ( void ) { return mkHReg(False, HRcInt64,  63,  56); }

extern void ppHRegTILEGX ( HReg );

#define TILEGXGuestStatePointer()     hregTILEGX_R50()
#define TILEGXStackFramePointer()     hregTILEGX_R52()
#define TILEGXLinkRegister()          hregTILEGX_R55()
#define TILEGXStackPointer()          hregTILEGX_R54()

/* r0, r1, r2, r3 ... r9 */
#define TILEGX_N_ARGREGS 10

/* --------- Condition codes, Tilegx encoding. --------- */
typedef enum {
  TILEGXcc_EQ = 0,    /* equal */
  TILEGXcc_NE = 1,    /* not equal */
  TILEGXcc_HS = 2,    /* >=u (higher or same) */
  TILEGXcc_LO = 3,    /* <u  (lower) */
  TILEGXcc_MI = 4,    /* minus (negative) */
  TILEGXcc_PL = 5,    /* plus (zero or +ve) */
  TILEGXcc_VS = 6,    /* overflow */
  TILEGXcc_VC = 7,    /* no overflow */
  TILEGXcc_HI = 8,    /* >u   (higher) */
  TILEGXcc_LS = 9,    /* <=u  (lower or same) */
  TILEGXcc_GE = 10,   /* >=s (signed greater or equal) */
  TILEGXcc_LT = 11,   /* <s  (signed less than) */
  TILEGXcc_GT = 12,   /* >s  (signed greater) */
  TILEGXcc_LE = 13,   /* <=s (signed less or equal) */
  TILEGXcc_AL = 14,   /* always (unconditional) */
  TILEGXcc_NV = 15,   /* never (unconditional): */
  TILEGXcc_EQ8x8 = 16,/* V1 equal */
  TILEGXcc_NE8x8 = 17,/* V1 not equal */
  TILEGXcc_EZ = 18,   /* equal 0 */
  TILEGXcc_NZ = 19,   /* not equal */

} TILEGXCondCode;

/* --------- Memory address expressions (amodes). --------- */
typedef enum {
  GXam_IR,        /* Immediate (signed 16-bit) + Reg */
} TILEGXAModeTag;

typedef struct {
  TILEGXAModeTag tag;
  union {
    struct {
      HReg base;
      Int index;
    } IR;
    struct {
      HReg base;
      HReg index;
    } RR;
  } GXam;
} TILEGXAMode;

extern TILEGXAMode *TILEGXAMode_IR ( Int, HReg );
extern TILEGXAMode *TILEGXAMode_RR ( HReg, HReg );
extern TILEGXAMode *dopyTILEGXAMode ( TILEGXAMode * );
extern TILEGXAMode *nextTILEGXAModeFloat ( TILEGXAMode * );
extern TILEGXAMode *nextTILEGXAModeInt ( TILEGXAMode * );
extern void ppTILEGXAMode ( const TILEGXAMode * );

/* --------- Operand, which can be a reg or a u16/s16. --------- */
/* ("RH" == "Register or Halfword immediate") */
typedef enum {
  GXrh_Imm,
  GXrh_Reg
} TILEGXRHTag;

typedef struct {
  TILEGXRHTag tag;
  union {
    struct {
      Bool syned;
      UShort imm16;
    } Imm;
    struct {
      HReg reg;
    } Reg;
  } GXrh;
} TILEGXRH;

extern void ppTILEGXRH ( const TILEGXRH * );
extern TILEGXRH *TILEGXRH_Imm ( Bool, UShort );
extern TILEGXRH *TILEGXRH_Reg ( HReg );

/* --------- Reg or imm5 operands --------- */
typedef enum {
  TILEGXri5_I5 = 7,      /* imm5, 1 .. 31 only (no zero!) */
  TILEGXri5_R      /* reg */
} TILEGXRI5Tag;

typedef struct {
  TILEGXRI5Tag tag;
  union {
    struct {
      UInt imm5;
    } I5;
    struct {
      HReg reg;
    } R;
  } TILEGXri5;
} TILEGXRI5;

extern TILEGXRI5 *TILEGXRI5_I5 ( UInt imm5 );
extern TILEGXRI5 *TILEGXRI5_R ( HReg );

extern void ppTILEGXRI5 ( const TILEGXRI5 * );

/* --------- Instructions. --------- */

/*Tags for operations*/

/* --------- */
typedef enum {
  GXun_CLZ,
  GXun_CTZ,
  GXun_NOP,
} TILEGXUnaryOp;

/* --------- */

typedef enum {
  GXalu_INVALID,
  GXalu_ADD,
  GXalu_SUB,
  GXalu_AND,
  GXalu_OR,
  GXalu_NOR,
  GXalu_XOR,
} TILEGXAluOp;

/* --------- */

typedef enum {
  GXshft_INVALID,
  GXshft_SLL,
  GXshft_SRL,
  GXshft_SRA,
  GXshft_SLL8x8,
  GXshft_SRL8x8,

} TILEGXShftOp;


/* --------- */
typedef enum {
  GXbf_EXTS,
  GXbf_EXTU,
  GXbf_INS
} TILEGXBfOp;

/* --------- */


/* --------- */
typedef enum {
  GXacas_CMPEXCH,
  GXacas_EXCH,
  GXacas_FetchAnd,
  GXacas_FetchAdd,
  GXacas_FetchAddgez,
  GXacas_FetchOr,
} TILEGXAcasOp;

/* --------- */

/* ----- Instruction tags ----- */
typedef enum {
  GXin_LI,        /* load word (32/64-bit) immediate (fake insn) */
  GXin_Alu,    /* word add/sub/and/or/xor/nor/others? */
  GXin_Shft,      /* word sll/srl/sra */
  GXin_Unary,     /* clo, clz, nop, neg */

  GXin_Cmp,    /* word compare (fake insn) */
  GXin_CmpI,

  GXin_Mul,    /* widening/non-widening multiply */

  GXin_Call,      /* call to address in register */

  GXin_XDirect,    /* direct transfer to GA */
  GXin_XIndir,     /* indirect transfer to GA */
  GXin_XAssisted,  /* assisted transfer to GA */
  GXin_EvCheck,    /* Event check */
  GXin_ProfInc,    /* 64-bit profile counter increment */ 

  GXin_RdWrLR,    /* Read/Write Link Register */

  GXin_Load,      /* zero-extending load a 8|16|32|64 bit value from mem */
  GXin_Store,     /* store a 8|16|32|64 bit value to mem */

  GXin_MovCond,
  GXin_Bf,           /* Bitfield operations */
  GXin_Acas,          /* Atomic Campare and swap. */

} TILEGXInstrTag;

/*--------- Structure for instructions ----------*/
/* Destinations are on the LEFT (first operand) */

typedef struct {
  TILEGXInstrTag tag;
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
      TILEGXAluOp op;
      HReg dst;
      HReg srcL;
      TILEGXRH *srcR;
    } Alu;

    struct {
      TILEGXBfOp op;
      HReg dst;
      HReg src;
      UInt Start;
      UInt End;
    } Bf;

    struct {
      TILEGXAcasOp op;
      HReg addr;
      HReg exp;
      HReg new;
      HReg old;
      UInt sz;
    } Acas;

    /* Integer shl/shr/sar.
       Limitations: the immediate, if it exists,
       is a signed 5-bit value between 1 and 31 inclusive.
    */
    struct {
      TILEGXShftOp op;
      Bool sz32;
      HReg dst;
      HReg srcL;
      TILEGXRH *srcR;
    } Shft;
    /* Clz, Ctz, Clo, nop */
    struct {
      TILEGXUnaryOp op;
      HReg dst;
      HReg src;
    } Unary;
    /* Word compare. Fake instruction, used for basic block ending */
    struct {
      Bool syned;
      Bool sz32;
      HReg dst;
      HReg srcL;
      HReg srcR;
      TILEGXCondCode cond;
    } Cmp;
    struct {
      Bool syned;
      Bool sz32;
      HReg dst;
      HReg srcL;
      TILEGXRH *srcR;
      TILEGXCondCode cond;
    } CmpI;
    struct {
      Bool widening; //True => widening, False => non-widening
      Bool syned; //signed/unsigned - meaningless if widenind = False
      Bool sz32;
      HReg dst;
      HReg srcL;
      HReg srcR;
    } Mul;
    /* Pseudo-insn.  Call target (an absolute address), on given
       condition (which could be Mcc_ALWAYS).  argiregs indicates
       which of r0 .. r9
       carries argument values for this call,
       using a bit mask (1<<N is set if rN holds an arg, for N in
       0 .. 9 inclusive).
       If cond is != Mcc_ALWAYS, src is checked.
       Otherwise, unconditional call */
    struct {
      TILEGXCondCode cond;
      Addr64 target;
      ULong argiregs;
      HReg src;
      RetLoc rloc; /* where the return value saved. */
    } Call;

    /* Update the guest IP value, then exit requesting to chain
       to it.  May be conditional.  Urr, use of Addr32 implicitly
       assumes that wordsize(guest) == wordsize(host). */
    struct {
      Addr64         dstGA;     /* next guest address */
      TILEGXAMode*   amPC;      /* amode in guest state for PC */
      TILEGXCondCode cond;      /* can be TILEGXcc_AL */
      Bool           toFastEP;  /* chain to the slow or fast point? */
    } XDirect;

    /* Boring transfer to a guest address not known at JIT time.
       Not chainable.  May be conditional. */
    struct {
      HReg           dstGA;
      TILEGXAMode*   amPC;
      TILEGXCondCode cond; /* can be TILEGXcc_AL */
    } XIndir;

    /* Assisted transfer to a guest address, most general case.
       Not chainable.  May be conditional. */
    struct {
      HReg           dstGA;
      TILEGXAMode*   amPC;
      TILEGXCondCode cond; /* can be TILEGXcc_AL */
      IRJumpKind     jk;
    } XAssisted;

    struct {
      TILEGXAMode* amCounter;
      TILEGXAMode* amFailAddr;
    } EvCheck;

    struct {
      /* No fields.  The address of the counter to inc is
         installed later, post-translation, by patching it in,
         as it is not known at translation time. */
    } ProfInc;
    /* Zero extending loads.  Dst size is host word size */
    struct {
      UChar sz;   /* 1|2|4|8 */
      HReg dst;
      TILEGXAMode *src;
    } Load;
    /* 64/32/16/8 bit stores */
    struct {
      UChar sz;   /* 1|2|4|8 */
      TILEGXAMode *dst;
      HReg src;
    } Store;
    /* Read/Write Link Register */
    struct {
      Bool wrLR;
      HReg gpr;
    } RdWrLR;
    struct {
      HReg dst;
      HReg srcL;
      TILEGXRH *srcR;
      HReg condR;
      TILEGXCondCode cond;
    } MovCond;
  } GXin;
} TILEGXInstr;
extern TILEGXInstr *TILEGXInstr_LI ( HReg, ULong );
extern TILEGXInstr *TILEGXInstr_Alu ( TILEGXAluOp, HReg, HReg, TILEGXRH * );
extern TILEGXInstr *TILEGXInstr_Shft ( TILEGXShftOp, Bool sz32, HReg, HReg,
                                       TILEGXRH * );
extern TILEGXInstr *TILEGXInstr_Unary ( TILEGXUnaryOp op, HReg dst, HReg src );
extern TILEGXInstr *TILEGXInstr_Cmp ( Bool, Bool, HReg, HReg, HReg,
                                      TILEGXCondCode );
extern TILEGXInstr *TILEGXInstr_CmpI ( Bool, Bool, HReg, HReg, TILEGXRH *,
                                       TILEGXCondCode );
extern TILEGXInstr *TILEGXInstr_Bf ( TILEGXBfOp op, HReg dst, HReg src,
                                     UInt Start, UInt End );
extern TILEGXInstr *TILEGXInstr_Acas ( TILEGXAcasOp op, HReg old, HReg addr,
                                       HReg exp, HReg new, UInt sz );
extern TILEGXInstr *TILEGXInstr_Mul ( Bool syned, Bool hi32, Bool sz32, HReg,
                                      HReg, HReg );
extern TILEGXInstr *TILEGXInstr_Div ( Bool syned, Bool sz32, HReg, HReg );
extern TILEGXInstr *TILEGXInstr_Madd ( Bool, HReg, HReg );
extern TILEGXInstr *TILEGXInstr_Msub ( Bool, HReg, HReg );

extern TILEGXInstr *TILEGXInstr_Load ( UChar sz, HReg dst, TILEGXAMode * src );

extern TILEGXInstr *TILEGXInstr_Store ( UChar sz, TILEGXAMode * dst, HReg src );

extern TILEGXInstr *TILEGXInstr_LoadL ( UChar sz, HReg dst, TILEGXAMode * src );

extern TILEGXInstr *TILEGXInstr_StoreC ( UChar sz, TILEGXAMode * dst, HReg src );

extern TILEGXInstr *TILEGXInstr_Call ( TILEGXCondCode, Addr64, ULong, HReg );
extern TILEGXInstr *TILEGXInstr_CallAlways ( TILEGXCondCode, Addr64, ULong );
extern TILEGXInstr *TILEGXInstr_XDirect ( Addr64 dstGA, TILEGXAMode* amPC,
                                          TILEGXCondCode cond, Bool toFastEP );
extern TILEGXInstr *TILEGXInstr_XIndir ( HReg dstGA, TILEGXAMode* amPC,
                                         TILEGXCondCode cond );
extern TILEGXInstr *TILEGXInstr_XAssisted ( HReg dstGA, TILEGXAMode* amPC,
                                            TILEGXCondCode cond, IRJumpKind jk );
extern TILEGXInstr *TILEGXInstr_EvCheck ( TILEGXAMode* amCounter,
                                          TILEGXAMode* amFailAddr );
extern TILEGXInstr* TILEGXInstr_ProfInc (void);

extern TILEGXInstr *TILEGXInstr_Goto ( IRJumpKind, TILEGXCondCode,
                                       TILEGXRH * dst, HReg );
extern TILEGXInstr *TILEGXInstr_GotoAlways ( IRJumpKind, TILEGXRH * );
extern TILEGXInstr *TILEGXInstr_RdWrLR ( Bool wrLR, HReg gpr );
extern TILEGXInstr *TILEGXInstr_MovCond ( HReg dst, HReg srcL, TILEGXRH * src,
                                          HReg condR, TILEGXCondCode cond );
extern void ppTILEGXInstr ( const TILEGXInstr * );

/* Some functions that insulate the register allocator from details
   of the underlying instruction set. */
extern void getRegUsage_TILEGXInstr ( HRegUsage *, TILEGXInstr *);
extern void mapRegs_TILEGXInstr ( HRegRemap *, TILEGXInstr *);
extern Bool isMove_TILEGXInstr ( TILEGXInstr *, HReg *, HReg * );
extern Int  emit_TILEGXInstr ( Bool*, UChar*, Int, TILEGXInstr*, Bool, VexEndness,
                               void*, void*, void*, void* );
extern void genSpill_TILEGX ( /*OUT*/ HInstr ** i1, /*OUT*/ HInstr ** i2,
                              HReg rreg, Int offset );
extern void genReload_TILEGX ( /*OUT*/ HInstr ** i1, /*OUT*/ HInstr ** i2,
                               HReg rreg, Int offset );

extern const RRegUniverse* getRRegUniverse_TILEGX ( void );

extern HInstrArray *iselSB_TILEGX ( const IRSB*, VexArch,
                                    const VexArchInfo*,
                                    const VexAbiInfo*,
                                    Int, Int, Bool, Bool, Addr);
extern const HChar *showTILEGXCondCode ( TILEGXCondCode cond );
extern Int evCheckSzB_TILEGX (void);
extern VexInvalRange chainXDirect_TILEGX ( VexEndness endness_host,
                                           void* place_to_chain,
                                           const void* disp_cp_chain_me_EXPECTED,
                                           const void* place_to_jump_to,
                                           Bool  mode64 );
extern VexInvalRange unchainXDirect_TILEGX ( VexEndness endness_host,
                                             void* place_to_unchain,
                                             const void* place_to_jump_to_EXPECTED,
                                             const void* disp_cp_chain_me,
                                             Bool  mode64 );
extern VexInvalRange patchProfInc_TILEGX ( VexEndness endness_host,
                                           void*  place_to_patch,
                                           const ULong* location_of_counter,
                                           Bool  mode64 );

extern Int decode_and_display ( tilegx_bundle_bits *p, Int count, ULong pc );

#endif  /* __LIBVEX_HOST_TILEGX_HDEFS_H */

/*---------------------------------------------------------------*/
/*--- end                                  host-tilegx_defs.h ---*/
/*---------------------------------------------------------------*/
