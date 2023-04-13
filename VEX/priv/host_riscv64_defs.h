
/*--------------------------------------------------------------------*/
/*--- begin                                    host_riscv64_defs.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2020-2023 Petr Pavlu
      petr.pavlu@dagobah.cz

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

#ifndef __VEX_HOST_RISCV64_DEFS_H
#define __VEX_HOST_RISCV64_DEFS_H

#include "libvex.h"
#include "libvex_basictypes.h"

#include "host_generic_regs.h"

/*------------------------------------------------------------*/
/*--- Registers                                            ---*/
/*------------------------------------------------------------*/

#define ST_IN static inline
ST_IN HReg hregRISCV64_x18(void) { return mkHReg(False, HRcInt64, 18, 0); }
ST_IN HReg hregRISCV64_x19(void) { return mkHReg(False, HRcInt64, 19, 1); }
ST_IN HReg hregRISCV64_x20(void) { return mkHReg(False, HRcInt64, 20, 2); }
ST_IN HReg hregRISCV64_x21(void) { return mkHReg(False, HRcInt64, 21, 3); }
ST_IN HReg hregRISCV64_x22(void) { return mkHReg(False, HRcInt64, 22, 4); }
ST_IN HReg hregRISCV64_x23(void) { return mkHReg(False, HRcInt64, 23, 5); }
ST_IN HReg hregRISCV64_x24(void) { return mkHReg(False, HRcInt64, 24, 6); }
ST_IN HReg hregRISCV64_x25(void) { return mkHReg(False, HRcInt64, 25, 7); }
ST_IN HReg hregRISCV64_x26(void) { return mkHReg(False, HRcInt64, 26, 8); }
ST_IN HReg hregRISCV64_x27(void) { return mkHReg(False, HRcInt64, 27, 9); }

ST_IN HReg hregRISCV64_x10(void) { return mkHReg(False, HRcInt64, 10, 10); }
ST_IN HReg hregRISCV64_x11(void) { return mkHReg(False, HRcInt64, 11, 11); }
ST_IN HReg hregRISCV64_x12(void) { return mkHReg(False, HRcInt64, 12, 12); }
ST_IN HReg hregRISCV64_x13(void) { return mkHReg(False, HRcInt64, 13, 13); }
ST_IN HReg hregRISCV64_x14(void) { return mkHReg(False, HRcInt64, 14, 14); }
ST_IN HReg hregRISCV64_x15(void) { return mkHReg(False, HRcInt64, 15, 15); }
ST_IN HReg hregRISCV64_x16(void) { return mkHReg(False, HRcInt64, 16, 16); }
ST_IN HReg hregRISCV64_x17(void) { return mkHReg(False, HRcInt64, 17, 17); }

ST_IN HReg hregRISCV64_f0(void) { return mkHReg(False, HRcFlt64, 0, 18); }
ST_IN HReg hregRISCV64_f1(void) { return mkHReg(False, HRcFlt64, 1, 19); }
ST_IN HReg hregRISCV64_f2(void) { return mkHReg(False, HRcFlt64, 2, 20); }
ST_IN HReg hregRISCV64_f3(void) { return mkHReg(False, HRcFlt64, 3, 21); }
ST_IN HReg hregRISCV64_f4(void) { return mkHReg(False, HRcFlt64, 4, 22); }
ST_IN HReg hregRISCV64_f5(void) { return mkHReg(False, HRcFlt64, 5, 23); }
ST_IN HReg hregRISCV64_f6(void) { return mkHReg(False, HRcFlt64, 6, 24); }
ST_IN HReg hregRISCV64_f7(void) { return mkHReg(False, HRcFlt64, 7, 25); }

ST_IN HReg hregRISCV64_f10(void) { return mkHReg(False, HRcFlt64, 10, 26); }
ST_IN HReg hregRISCV64_f11(void) { return mkHReg(False, HRcFlt64, 11, 27); }
ST_IN HReg hregRISCV64_f12(void) { return mkHReg(False, HRcFlt64, 12, 28); }
ST_IN HReg hregRISCV64_f13(void) { return mkHReg(False, HRcFlt64, 13, 29); }
ST_IN HReg hregRISCV64_f14(void) { return mkHReg(False, HRcFlt64, 14, 30); }
ST_IN HReg hregRISCV64_f15(void) { return mkHReg(False, HRcFlt64, 15, 31); }
ST_IN HReg hregRISCV64_f16(void) { return mkHReg(False, HRcFlt64, 16, 32); }
ST_IN HReg hregRISCV64_f17(void) { return mkHReg(False, HRcFlt64, 17, 33); }

ST_IN HReg hregRISCV64_f28(void) { return mkHReg(False, HRcFlt64, 28, 34); }
ST_IN HReg hregRISCV64_f29(void) { return mkHReg(False, HRcFlt64, 29, 35); }
ST_IN HReg hregRISCV64_f30(void) { return mkHReg(False, HRcFlt64, 30, 36); }
ST_IN HReg hregRISCV64_f31(void) { return mkHReg(False, HRcFlt64, 31, 37); }

ST_IN HReg hregRISCV64_x0(void) { return mkHReg(False, HRcInt64, 0, 38); }
ST_IN HReg hregRISCV64_x2(void) { return mkHReg(False, HRcInt64, 2, 39); }
ST_IN HReg hregRISCV64_x8(void) { return mkHReg(False, HRcInt64, 8, 40); }
#undef ST_IN

/* Number of registers used for argument passing in function calls. */
#define RISCV64_N_ARGREGS  8 /* x10/a0 .. x17/a7 */
#define RISCV64_N_FARGREGS 8 /* f10/fa0 .. f17/fa7 */

/*------------------------------------------------------------*/
/*--- Instructions                                         ---*/
/*------------------------------------------------------------*/

/* RISCV64in_ALU sub-types. */
typedef enum {
   RISCV64op_ADD = 0x100, /* Addition of two registers. */
   RISCV64op_SUB,         /* Subtraction of one register from another. */
   RISCV64op_ADDW,        /* 32-bit addition of two registers. */
   RISCV64op_SUBW,        /* 32-bit subtraction of one register from another. */
   RISCV64op_XOR,         /* Bitwise XOR of two registers. */
   RISCV64op_OR,          /* Bitwise OR of two registers. */
   RISCV64op_AND,         /* Bitwise AND of two registers. */
   RISCV64op_SLL,         /* Logical left shift on a register. */
   RISCV64op_SRL,         /* Logical right shift on a register. */
   RISCV64op_SRA,         /* Arithmetic right shift on a register. */
   RISCV64op_SLLW,        /* 32-bit logical left shift on a register. */
   RISCV64op_SRLW,        /* 32-bit logical right shift on a register. */
   RISCV64op_SRAW,        /* 32-bit arithmetic right shift on a register. */
   RISCV64op_SLT,         /* Signed comparison of two registers. */
   RISCV64op_SLTU,        /* Unsigned comparison of two registers. */
   RISCV64op_MUL,         /* Multiplication of two registers, producing the
                             lower 64 bits. */
   RISCV64op_MULH,        /* Signed multiplication of two registers, producing
                             the upper 64 bits. */
   RISCV64op_MULHU,       /* Unsigned multiplication of two registers, producing
                             the upper 64 bits. */
   RISCV64op_DIV,         /* Signed division of one register by another. */
   RISCV64op_DIVU,        /* Unsigned division of one register by another. */
   RISCV64op_REM,         /* Remainder from signed division of one register by
                             another. */
   RISCV64op_REMU,        /* Remainder from unsigned division of one register by
                             another. */
   RISCV64op_MULW,        /* 32-bit multiplication of two registers, producing
                             the lower 32 bits. */
   RISCV64op_DIVW,        /* 32-bit signed division of one register by
                             another. */
   RISCV64op_DIVUW,       /* 32-bit unsigned division of one register by
                             another. */
   RISCV64op_REMW,        /* Remainder from 32-bit signed division of one
                             register by another. */
   RISCV64op_REMUW,       /* Remainder from 32-bit unsigned division of one
                             register by another. */
} RISCV64ALUOp;

/* RISCV64in_ALUImm sub-types. */
typedef enum {
   RISCV64op_ADDI = 0x200, /* Addition of a register and a sx-12-bit
                              immediate. */
   RISCV64op_ADDIW,        /* 32-bit addition of a register and a sx-12-bit
                              immediate. */
   RISCV64op_XORI,         /* Bitwise XOR of a register and a sx-12-bit
                              immediate. */
   RISCV64op_ANDI,         /* Bitwise AND of a register and a sx-12-bit
                              immediate. */
   RISCV64op_SLLI,         /* Logical left shift on a register by a 6-bit
                              immediate. */
   RISCV64op_SRLI,         /* Logical right shift on a register by a 6-bit
                              immediate. */
   RISCV64op_SRAI,         /* Arithmetic right shift on a register by a 6-bit
                              immediate. */
   RISCV64op_SLTIU,        /* Unsigned comparison of a register and a sx-12-bit
                              immediate. */
} RISCV64ALUImmOp;

/* RISCV64in_Load sub-types. */
typedef enum {
   RISCV64op_LD = 0x300, /* 64-bit load. */
   RISCV64op_LW,         /* sx-32-to-64-bit load. */
   RISCV64op_LH,         /* sx-16-to-64-bit load. */
   RISCV64op_LB,         /* sx-8-to-64-bit load. */
} RISCV64LoadOp;

/* RISCV64in_Store sub-types. */
typedef enum {
   RISCV64op_SD = 0x400, /* 64-bit store. */
   RISCV64op_SW,         /* 32-bit store. */
   RISCV64op_SH,         /* 16-bit store. */
   RISCV64op_SB,         /* 8-bit store. */
} RISCV64StoreOp;

/* RISCV64in_LoadR sub-types. */
typedef enum {
   RISCV64op_LR_W = 0x500, /* sx-32-to-64-bit load-reserved. */
} RISCV64LoadROp;

/* RISCV64in_StoreC sub-types. */
typedef enum {
   RISCV64op_SC_W = 0x600, /* 32-bit store-conditional. */
} RISCV64StoreCOp;

/* RISCV64in_FpUnary sub-types. */
typedef enum {
   RISCV64op_FSQRT_S = 0x700, /* Square root of a 32-bit floating-point
                                 register. */
   RISCV64op_FSQRT_D,         /* Square root of a 64-bit floating-point
                                 register. */
} RISCV64FpUnaryOp;

/* RISCV64in_FpBinary sub-types. */
typedef enum {
   RISCV64op_FADD_S = 0x800, /* Addition of two 32-bit floating-point
                                registers. */
   RISCV64op_FMUL_S,         /* Multiplication of two 32-bit floating-point
                                registers. */
   RISCV64op_FDIV_S,         /* Division of a 32-bit floating-point register by
                                another. */
   RISCV64op_FSGNJN_S,       /* Copy of a 32-bit floating-point register to
                                another with the sign bit taken from the second
                                input and negated. */
   RISCV64op_FSGNJX_S,       /* Copy of a 32-bit floating-point register to
                                another with the sign bit XOR'ed from the second
                                input. */
   RISCV64op_FMIN_S,         /* Select minimum-number of two 32-bit
                                floating-point registers. */
   RISCV64op_FMAX_S,         /* Select maximum-number of two 32-bit
                                floating-point registers. */
   RISCV64op_FADD_D,         /* Addition of two 64-bit floating-point
                                registers. */
   RISCV64op_FSUB_D,         /* Subtraction of one 64-bit floating-point
                                register from another. */
   RISCV64op_FMUL_D,         /* Multiplication of two 64-bit floating-point
                                registers. */
   RISCV64op_FDIV_D,         /* Division of a 64-bit floating-point register by
                                another. */
   RISCV64op_FSGNJN_D,       /* Copy of a 64-bit floating-point register to
                                another with the sign bit taken from the second
                                input and negated. */
   RISCV64op_FSGNJX_D,       /* Copy of a 64-bit floating-point register to
                                another with the sign bit XOR'ed from the second
                                input. */
   RISCV64op_FMIN_D,         /* Select minimum-number of two 64-bit
                                floating-point registers. */
   RISCV64op_FMAX_D,         /* Select maximum-number of two 64-bit
                                floating-point registers. */
} RISCV64FpBinaryOp;

/* RISCV64in_FpTernary sub-types. */
typedef enum {
   RISCV64op_FMADD_S = 0x900, /* Fused multiply-add of 32-bit floating-point
                                 registers. */
   RISCV64op_FMADD_D,         /* Fused multiply-add of 64-bit floating-point
                                 registers. */
} RISCV64FpTernaryOp;

/* RISCV64in_FpMove sub-types. */
typedef enum {
   RISCV64op_FMV_X_W = 0xa00, /* Move as-is a 32-bit value from a floating-point
                                 register to an integer register. */
   RISCV64op_FMV_W_X,         /* Move as-is a 32-bit value from an integer
                                 register to a floating-point register. */
   RISCV64op_FMV_D,           /* Copy one 64-bit floating-point register to
                                 another. */
   RISCV64op_FMV_X_D,         /* Move as-is a 64-bit value from a floating-point
                                 register to an integer register. */
   RISCV64op_FMV_D_X,         /* Move as-is a 64-bit value from an integer
                                 register to a floating-point register. */
} RISCV64FpMoveOp;

/* RISCV64in_FpConvert sub-types. */
typedef enum {
   RISCV64op_FCVT_W_S = 0xb00, /* Convert a 32-bit floating-point number to
                                  a 32-bit signed integer. */
   RISCV64op_FCVT_WU_S,        /* Convert a 32-bit floating-point number to
                                  a 32-bit unsigned integer. */
   RISCV64op_FCVT_S_W,         /* Convert a 32-bit signed integer to a 32-bit
                                  floating-point number. */
   RISCV64op_FCVT_S_WU,        /* Convert a 32-bit unsigned integer to a 32-bit
                                  floating-point number. */
   RISCV64op_FCVT_L_S,         /* Convert a 32-bit floating-point number to
                                  a 64-bit signed integer. */
   RISCV64op_FCVT_LU_S,        /* Convert a 32-bit floating-point number to
                                  a 64-bit unsigned integer. */
   RISCV64op_FCVT_S_L,         /* Convert a 64-bit signed integer to a 32-bit
                                  floating-point number. */
   RISCV64op_FCVT_S_LU,        /* Convert a 64-bit unsigned integer to a 32-bit
                                  floating-point number. */
   RISCV64op_FCVT_S_D,         /* Convert a 64-bit floating-point number to
                                  a 32-bit floating-point number. */
   RISCV64op_FCVT_D_S,         /* Convert a 32-bit floating-point number to
                                  a 64-bit floating-point number. */
   RISCV64op_FCVT_W_D,         /* Convert a 64-bit floating-point number to
                                  a 32-bit signed integer. */
   RISCV64op_FCVT_WU_D,        /* Convert a 64-bit floating-point number to
                                  a 32-bit unsigned integer. */
   RISCV64op_FCVT_D_W,         /* Convert a 32-bit signed integer to a 64-bit
                                  floating-point number. */
   RISCV64op_FCVT_D_WU,        /* Convert a 32-bit unsigned integer to a 64-bit
                                  floating-point number. */
   RISCV64op_FCVT_L_D,         /* Convert a 64-bit floating-point number to
                                  a 64-bit signed integer. */
   RISCV64op_FCVT_LU_D,        /* Convert a 64-bit floating-point number to
                                  a 64-bit unsigned integer. */
   RISCV64op_FCVT_D_L,         /* Convert a 64-bit signed integer to a 64-bit
                                  floating-point number. */
   RISCV64op_FCVT_D_LU,        /* Convert a 64-bit unsigned integer to a 64-bit
                                  floating-point number. */
} RISCV64FpConvertOp;

/* RISCV64in_FpCompare sub-types. */
typedef enum {
   RISCV64op_FEQ_S = 0xc00, /* Equality comparison of two 32-bit floating-point
                               registers. */
   RISCV64op_FLT_S,         /* Less-than comparison of two 32-bit floating-point
                               registers. */
   RISCV64op_FEQ_D,         /* Equality comparison of two 64-bit floating-point
                               registers. */
   RISCV64op_FLT_D,         /* Less-than comparison of two 64-bit floating-point
                               registers. */
} RISCV64FpCompareOp;

/* RISCV64in_FpLdSt sub-types. */
typedef enum {
   RISCV64op_FLW = 0xd00, /* 32-bit floating-point load. */
   RISCV64op_FLD,         /* 64-bit floating-point load. */
   RISCV64op_FSW,         /* 32-bit floating-point store. */
   RISCV64op_FSD,         /* 64-bit floating-point store. */
} RISCV64FpLdStOp;

/* RISCV64in_CAS sub-types. */
typedef enum {
   RISCV64op_CAS_D = 0xe00, /* 64-bit compare-and-swap pseudoinstruction. */
   RISCV64op_CAS_W,         /* 32-bit compare-and-swap pseudoinstruction. */
} RISCV64CASOp;

/* The kind of instructions. */
typedef enum {
   RISCV64in_LI = 0x52640000, /* Load immediate pseudoinstruction. */
   RISCV64in_MV,              /* Copy one register to another. */
   RISCV64in_ALU,             /* Computational binary instruction. */
   RISCV64in_ALUImm,          /* Computational binary instruction, with
                                 an immediate as the second input. */
   RISCV64in_Load,            /* Load from memory (sign-extended). */
   RISCV64in_Store,           /* Store to memory. */
   RISCV64in_LoadR,           /* Load-reserved from memory (sign-extended). */
   RISCV64in_StoreC,          /* Store-conditional to memory. */
   RISCV64in_CSRRW,           /* Atomic swap of values in a CSR and an integer
                                 register. */
   RISCV64in_FpUnary,         /* Floating-point unary instruction. */
   RISCV64in_FpBinary,        /* Floating-point binary instruction. */
   RISCV64in_FpTernary,       /* Floating-point ternary instruction. */
   RISCV64in_FpMove,          /* Floating-point move instruction. */
   RISCV64in_FpConvert,       /* Floating-point convert instruction. */
   RISCV64in_FpCompare,       /* Floating-point compare instruction. */
   RISCV64in_FpLdSt,          /* Floating-point load/store instruction. */
   RISCV64in_FpCSEL,          /* Floating-point conditional-select pseudoinstruction.*/
   RISCV64in_CAS,             /* Compare-and-swap pseudoinstruction. */
   RISCV64in_FENCE,           /* Device I/O and memory fence. */
   RISCV64in_CSEL,            /* Conditional-select pseudoinstruction. */
   RISCV64in_Call,            /* Call pseudoinstruction. */
   RISCV64in_XDirect,         /* Direct transfer to guest address. */
   RISCV64in_XIndir,          /* Indirect transfer to guest address. */
   RISCV64in_XAssisted,       /* Assisted transfer to guest address. */
   RISCV64in_EvCheck,         /* Event check. */
   RISCV64in_ProfInc          /* 64-bit profile counter increment. */
} RISCV64InstrTag;

typedef struct {
   RISCV64InstrTag tag;
   union {
      /* Load immediate pseudoinstruction. */
      struct {
         HReg  dst;
         ULong imm64;
      } LI;
      /* Copy one register to another. */
      struct {
         HReg dst;
         HReg src;
      } MV;
      /* Computational binary instruction. */
      struct {
         RISCV64ALUOp op;
         HReg         dst;
         HReg         src1;
         HReg         src2;
      } ALU;
      /* Computational binary instruction, with an immediate as the second
         input. */
      struct {
         RISCV64ALUImmOp op;
         HReg            dst;
         HReg            src;
         Int             imm12; /* simm12 or uimm6 */
      } ALUImm;
      /* Load from memory (sign-extended). */
      struct {
         RISCV64LoadOp op;
         HReg          dst;
         HReg          base;
         Int           soff12; /* -2048 .. +2047 */
      } Load;
      /* Store to memory. */
      struct {
         RISCV64StoreOp op;
         HReg           src;
         HReg           base;
         Int            soff12; /* -2048 .. +2047 */
      } Store;
      /* Load-reserved from memory (sign-extended). */
      struct {
         RISCV64LoadROp op;
         HReg           dst;
         HReg           addr;
      } LoadR;
      /* Store-conditional to memory. */
      struct {
         RISCV64StoreCOp op;
         HReg            res;
         HReg            src;
         HReg            addr;
      } StoreC;
      /* Atomic swap of values in a CSR and an integer register. */
      struct {
         HReg dst;
         HReg src;
         UInt csr;
      } CSRRW;
      /* Floating-point unary instruction. */
      struct {
         RISCV64FpUnaryOp op;
         HReg             dst;
         HReg             src;
      } FpUnary;
      /* Floating-point binary instruction. */
      struct {
         RISCV64FpBinaryOp op;
         HReg              dst;
         HReg              src1;
         HReg              src2;
      } FpBinary;
      /* Floating-point ternary instruction. */
      struct {
         RISCV64FpTernaryOp op;
         HReg               dst;
         HReg               src1;
         HReg               src2;
         HReg               src3;
      } FpTernary;
      /* Floating-point move instruction. */
      struct {
         RISCV64FpMoveOp op;
         HReg            dst;
         HReg            src;
      } FpMove;
      /* Floating-point convert instruction. */
      struct {
         RISCV64FpConvertOp op;
         HReg               dst;
         HReg               src;
      } FpConvert;
      /* Floating-point compare instruction. */
      struct {
         RISCV64FpCompareOp op;
         HReg               dst;
         HReg               src1;
         HReg               src2;
      } FpCompare;
      /* Floating-point load/store instruction. */
      struct {
         RISCV64FpLdStOp op;
         HReg            reg; /* dst for load, src for store */
         HReg            base;
         Int             soff12; /* -2048 .. +2047 */
      } FpLdSt;
      /* Floating-point conditional-select pseudoinstruction. */
      struct {
         IRType ty;
         HReg   dst;
         HReg   iftrue;
         HReg   iffalse;
         HReg   cond;
      } FpCSEL;
      /* Compare-and-swap pseudoinstruction. */
      struct {
         RISCV64CASOp op;
         HReg         old;
         HReg         addr;
         HReg         expd;
         HReg         data;
      } CAS;
      /* Device I/O and memory fence. */
      struct {
      } FENCE;
      /* Conditional-select pseudoinstruction. */
      struct {
         HReg dst;
         HReg iftrue;
         HReg iffalse;
         HReg cond;
      } CSEL;
      /* Call pseudoinstruction. Call a target (an absolute address), on a given
         condition register. */
      struct {
         RetLoc rloc;      /* Where the return value will be. */
         Addr64 target;    /* Target address of the call. */
         HReg   cond;      /* Condition, can be INVALID_HREG for "always". */
         UChar  nArgRegs;  /* # regs carrying integer args: 0 .. 8 */
         UChar  nFArgRegs; /* # regs carrying floating-point args: 0 .. 8 */
      } Call;
      /* Update the guest pc value, then exit requesting to chain to it. May be
         conditional. */
      struct {
         Addr64 dstGA;    /* Next guest address. */
         HReg   base;     /* Base to access the guest state. */
         Int    soff12;   /* Offset from the base register to access pc. */
         HReg   cond;     /* Condition, can be INVALID_HREG for "always". */
         Bool   toFastEP; /* Chain to the slow or fast point? */
      } XDirect;
      /* Boring transfer to a guest address not known at JIT time. Not
         chainable. May be conditional. */
      struct {
         HReg dstGA;  /* Next guest address. */
         HReg base;   /* Base to access the guest state. */
         Int  soff12; /* Offset from the base register to access pc. */
         HReg cond;   /* Condition, can be INVALID_HREG for "always". */
      } XIndir;
      /* Assisted transfer to a guest address, most general case. Not chainable.
         May be conditional. */
      struct {
         HReg       dstGA;  /* Next guest address. */
         HReg       base;   /* Base to access the guest state. */
         Int        soff12; /* Offset from the base register to access pc. */
         HReg       cond;   /* Condition, can be INVALID_HREG for "always". */
         IRJumpKind jk;
      } XAssisted;
      /* Event check. */
      struct {
         HReg base_amCounter;   /* Base to access the guest state for
                                   host_EvC_Counter. */
         Int soff12_amCounter;  /* Offset from the base register to access
                                   host_EvC_COUNTER. */
         HReg base_amFailAddr;  /* Base to access the guest state for for
                                   host_EvC_FAILADDR. */
         Int soff12_amFailAddr; /* Offset from the base register to access
                                   host_EvC_FAILADDR. */
      } EvCheck;
      /* 64-bit profile counter increment. */
      struct {
         /* No fields. The address of the counter to inc is installed later,
            post-translation, by patching it in, as it is not known at
            translation time. */
      } ProfInc;
   } RISCV64in;
} RISCV64Instr;

RISCV64Instr* RISCV64Instr_LI(HReg dst, ULong imm64);
RISCV64Instr* RISCV64Instr_MV(HReg dst, HReg src);
RISCV64Instr* RISCV64Instr_ALU(RISCV64ALUOp op, HReg dst, HReg src1, HReg src2);
RISCV64Instr*
RISCV64Instr_ALUImm(RISCV64ALUImmOp op, HReg dst, HReg src, Int imm12);
RISCV64Instr*
RISCV64Instr_Load(RISCV64LoadOp op, HReg dst, HReg base, Int soff12);
RISCV64Instr*
RISCV64Instr_Store(RISCV64StoreOp op, HReg src, HReg base, Int soff12);
RISCV64Instr* RISCV64Instr_LoadR(RISCV64LoadROp op, HReg dst, HReg addr);
RISCV64Instr*
RISCV64Instr_StoreC(RISCV64StoreCOp op, HReg res, HReg src, HReg addr);
RISCV64Instr* RISCV64Instr_CSRRW(HReg dst, HReg src, UInt csr);
RISCV64Instr* RISCV64Instr_FpUnary(RISCV64FpUnaryOp op, HReg dst, HReg src);
RISCV64Instr*
RISCV64Instr_FpBinary(RISCV64FpBinaryOp op, HReg dst, HReg src1, HReg src2);
RISCV64Instr* RISCV64Instr_FpTernary(
   RISCV64FpTernaryOp op, HReg dst, HReg src1, HReg src2, HReg src3);
RISCV64Instr* RISCV64Instr_FpMove(RISCV64FpMoveOp op, HReg dst, HReg src);
RISCV64Instr* RISCV64Instr_FpConvert(RISCV64FpConvertOp op, HReg dst, HReg src);
RISCV64Instr*
RISCV64Instr_FpCompare(RISCV64FpCompareOp op, HReg dst, HReg src1, HReg src2);
RISCV64Instr*
RISCV64Instr_FpLdSt(RISCV64FpLdStOp op, HReg reg, HReg base, Int soff12);
RISCV64Instr*
RISCV64Instr_FpCSEL(IRType ty, HReg dst, HReg iftrue, HReg iffalse, HReg cond);
RISCV64Instr*
RISCV64Instr_CAS(RISCV64CASOp op, HReg old, HReg addr, HReg expd, HReg data);
RISCV64Instr* RISCV64Instr_FENCE(void);
RISCV64Instr* RISCV64Instr_CSEL(HReg dst, HReg iftrue, HReg iffalse, HReg cond);
RISCV64Instr* RISCV64Instr_Call(
   RetLoc rloc, Addr64 target, HReg cond, UChar nArgRegs, UChar nFArgRegs);
RISCV64Instr* RISCV64Instr_XDirect(
   Addr64 dstGA, HReg base, Int soff12, HReg cond, Bool toFastEP);
RISCV64Instr* RISCV64Instr_XIndir(HReg dstGA, HReg base, Int soff12, HReg cond);
RISCV64Instr* RISCV64Instr_XAssisted(
   HReg dstGA, HReg base, Int soff12, HReg cond, IRJumpKind jk);
RISCV64Instr* RISCV64Instr_EvCheck(HReg base_amCounter,
                                   Int  soff12_amCounter,
                                   HReg base_amFailAddr,
                                   Int  soff12_amFailAddr);
RISCV64Instr* RISCV64Instr_ProfInc(void);

/*------------------------------------------------------------*/
/*--- Misc helpers                                         ---*/
/*------------------------------------------------------------*/

static inline HReg get_baseblock_register(void) { return hregRISCV64_x8(); }
#define BASEBLOCK_OFFSET_ADJUSTMENT 2048

/*------------------------------------------------------------*/
/* --- Interface exposed to VEX                           --- */
/*------------------------------------------------------------*/

UInt ppHRegRISCV64(HReg reg);

void ppRISCV64Instr(const RISCV64Instr* i, Bool mode64);

const RRegUniverse* getRRegUniverse_RISCV64(void);

/* Some functions that insulate the register allocator from details of the
   underlying instruction set. */
void getRegUsage_RISCV64Instr(HRegUsage* u, const RISCV64Instr* i, Bool mode64);
void mapRegs_RISCV64Instr(HRegRemap* m, RISCV64Instr* i, Bool mode64);

void genSpill_RISCV64(
   /*OUT*/ HInstr** i1, /*OUT*/ HInstr** i2, HReg rreg, Int offset, Bool);
void genReload_RISCV64(
   /*OUT*/ HInstr** i1, /*OUT*/ HInstr** i2, HReg rreg, Int offset, Bool);
RISCV64Instr* genMove_RISCV64(HReg from, HReg to, Bool);

Int emit_RISCV64Instr(/*MB_MOD*/ Bool*    is_profInc,
                      UChar*              buf,
                      Int                 nbuf,
                      const RISCV64Instr* i,
                      Bool                mode64,
                      VexEndness          endness_host,
                      const void*         disp_cp_chain_me_to_slowEP,
                      const void*         disp_cp_chain_me_to_fastEP,
                      const void*         disp_cp_xindir,
                      const void*         disp_cp_xassisted);

/* Return the number of bytes of code needed for an event check. */
Int evCheckSzB_RISCV64(void);

/* Perform a chaining and unchaining of an XDirect jump. */
VexInvalRange chainXDirect_RISCV64(VexEndness  endness_host,
                                   void*       place_to_chain,
                                   const void* disp_cp_chain_me_EXPECTED,
                                   const void* place_to_jump_to);

VexInvalRange unchainXDirect_RISCV64(VexEndness  endness_host,
                                     void*       place_to_unchain,
                                     const void* place_to_jump_to_EXPECTED,
                                     const void* disp_cp_chain_me);

/* Patch the counter location into an existing ProfInc point. */
VexInvalRange patchProfInc_RISCV64(VexEndness   endness_host,
                                   void*        place_to_patch,
                                   const ULong* location_of_counter);

HInstrArray* iselSB_RISCV64(const IRSB*        bb,
                            VexArch            arch_host,
                            const VexArchInfo* archinfo_host,
                            const VexAbiInfo*  vbi,
                            Int                offs_Host_EvC_Counter,
                            Int                offs_Host_EvC_FailAddr,
                            Bool               chainingAllowed,
                            Bool               addProfInc,
                            Addr               max_ga);

#endif /* ndef __VEX_HOST_RISCV64_DEFS_H */

/*--------------------------------------------------------------------*/
/*--- end                                      host_riscv64_defs.h ---*/
/*--------------------------------------------------------------------*/
