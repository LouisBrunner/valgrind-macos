/* -*- mode: C; c-basic-offset: 3; -*- */

/*---------------------------------------------------------------*/
/*--- begin                                  host_s390_defs.h ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright IBM Corp. 2010-2015

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

/* Contributed by Florian Krohm */

#ifndef __VEX_HOST_S390_DEFS_H
#define __VEX_HOST_S390_DEFS_H

#include "libvex_basictypes.h"            /* Bool */
#include "libvex.h"                       /* VexArchInfo */
#include "host_generic_regs.h"            /* HReg */
#include "s390_defs.h"                    /* s390_cc_t */

/* --------- Registers --------- */
const HChar *s390_hreg_as_string(HReg);
HReg s390_hreg_gpr(UInt regno);
HReg s390_hreg_fpr(UInt regno);

/* Dedicated registers */
HReg s390_hreg_guest_state_pointer(void);


/* Given the index of a function argument, return the number of the
   general purpose register in which it is being passed. Arguments are
   counted 0, 1, 2, ... and they are being passed in r2, r3, r4, ... */
static __inline__ UInt
s390_gprno_from_arg_index(UInt ix)
{
   return ix + 2;
}

/* --------- Memory address expressions (amodes). --------- */

/* These are the address modes:
   (1) b12:  base register + 12-bit unsigned offset   (e.g. RS)
   (2) b20:  base register + 20-bit signed offset     (e.g. RSY)
   (3) bx12: base register + index register + 12-bit unsigned offset (e.g. RX)
   (4) bx20: base register + index register + 20-bit signed offset   (e.g. RXY)
   fixs390: There is also pc-relative stuff.. e.g. LARL
*/

typedef enum {
   S390_AMODE_B12,
   S390_AMODE_B20,
   S390_AMODE_BX12,
   S390_AMODE_BX20
} s390_amode_t;

typedef struct {
   s390_amode_t tag;
   HReg b;
   HReg x;       /* hregNumber(x) == 0  for S390_AMODE_B12/B20 kinds */
   Int  d;       /* 12 bit unsigned or 20 bit signed */
} s390_amode;


s390_amode *s390_amode_b12(Int d, HReg b);
s390_amode *s390_amode_b20(Int d, HReg b);
s390_amode *s390_amode_bx12(Int d, HReg b, HReg x);
s390_amode *s390_amode_bx20(Int d, HReg b, HReg x);
s390_amode *s390_amode_for_guest_state(Int d);
Bool        s390_amode_is_sane(const s390_amode *);

const HChar *s390_amode_as_string(const s390_amode *);

/* ------------- 2nd (right) operand of binary operation ---------------- */

typedef enum {
   S390_OPND_REG,
   S390_OPND_IMMEDIATE,
   S390_OPND_AMODE
} s390_opnd_t;


/* Naming convention for operand locations:
   R    - GPR
   I    - immediate value
   M    - memory (any Amode may be used)
*/

/* An operand that is either in a GPR or is addressable via a BX20 amode */
typedef struct {
   s390_opnd_t tag;
   union {
      HReg        reg;
      s390_amode *am;
      ULong       imm;
   } variant;
} s390_opnd_RMI;


/* The kind of instructions */
typedef enum {
   S390_INSN_LOAD,   /* load register from memory */
   S390_INSN_STORE,  /* store register to memory */
   S390_INSN_MOVE,   /* from register to register */
   S390_INSN_MEMCPY, /* from memory to memory */
   S390_INSN_COND_MOVE, /* conditonal "move" to register */
   S390_INSN_LOAD_IMMEDIATE,
   S390_INSN_ALU,
   S390_INSN_SMUL,   /*   signed multiply; n-bit operands; 2n-bit result */
   S390_INSN_UMUL,   /* unsigned multiply; n-bit operands; 2n-bit result */
   S390_INSN_SDIV,   /*   signed division; 2n-bit / n-bit -> n-bit quot/rem */
   S390_INSN_UDIV,   /* unsigned division; 2n-bit / n-bit -> n-bit quot/rem */
   S390_INSN_DIVS,   /* n-bit dividend; n-bit divisor; n-bit quot/rem */
   S390_INSN_CLZ,    /* count left-most zeroes */
   S390_INSN_UNOP,
   S390_INSN_TEST,   /* test operand and set cc */
   S390_INSN_CC2BOOL,/* convert condition code to 0/1 */
   S390_INSN_COMPARE,
   S390_INSN_HELPER_CALL,
   S390_INSN_CAS,    /* compare and swap */
   S390_INSN_CDAS,   /* compare double and swap */
   S390_INSN_BFP_BINOP, /* Binary floating point */
   S390_INSN_BFP_UNOP,
   S390_INSN_BFP_TRIOP,
   S390_INSN_BFP_COMPARE,
   S390_INSN_BFP_CONVERT,
   S390_INSN_DFP_BINOP, /* Decimal floating point */
   S390_INSN_DFP_UNOP,
   S390_INSN_DFP_INTOP,
   S390_INSN_DFP_COMPARE,
   S390_INSN_DFP_CONVERT,
   S390_INSN_DFP_REROUND,
   S390_INSN_FP_CONVERT,
   S390_INSN_MFENCE,
   S390_INSN_MIMM,    /* Assign an immediate constant to a memory location */
   S390_INSN_MADD,    /* Add a value to a memory location */
   S390_INSN_SET_FPC_BFPRM, /* Set the bfp rounding mode in the FPC */
   S390_INSN_SET_FPC_DFPRM, /* Set the dfp rounding mode in the FPC */
   /* The following 5 insns are mandated by translation chaining */
   S390_INSN_XDIRECT,     /* direct transfer to guest address */
   S390_INSN_XINDIR,      /* indirect transfer to guest address */
   S390_INSN_XASSISTED,   /* assisted transfer to guest address */
   S390_INSN_EVCHECK,     /* Event check */
   S390_INSN_PROFINC      /* 64-bit profile counter increment */
} s390_insn_tag;


/* The kind of ALU instructions */
typedef enum {
   S390_ALU_ADD,
   S390_ALU_SUB,
   S390_ALU_MUL,   /* n-bit operands; result is lower n-bit of product */
   S390_ALU_AND,
   S390_ALU_OR,
   S390_ALU_XOR,
   S390_ALU_LSH,
   S390_ALU_RSH,
   S390_ALU_RSHA   /* arithmetic */
} s390_alu_t;


/* The kind of unary integer operations */
typedef enum {
   S390_ZERO_EXTEND_8,
   S390_ZERO_EXTEND_16,
   S390_ZERO_EXTEND_32,
   S390_SIGN_EXTEND_8,
   S390_SIGN_EXTEND_16,
   S390_SIGN_EXTEND_32,
   S390_NEGATE
} s390_unop_t;

/* The kind of ternary BFP operations */
typedef enum {
   S390_BFP_MADD,
   S390_BFP_MSUB,
} s390_bfp_triop_t;

/* The kind of binary BFP operations */
typedef enum {
   S390_BFP_ADD,
   S390_BFP_SUB,
   S390_BFP_MUL,
   S390_BFP_DIV
} s390_bfp_binop_t;

/* The kind of unary BFP operations */
typedef enum {
   S390_BFP_ABS,
   S390_BFP_NABS,
   S390_BFP_NEG,
   S390_BFP_SQRT
} s390_bfp_unop_t;

/* Type conversion operations: to and/or from binary floating point */
typedef enum {
   S390_BFP_I32_TO_F32,
   S390_BFP_I32_TO_F64,
   S390_BFP_I32_TO_F128,
   S390_BFP_I64_TO_F32,
   S390_BFP_I64_TO_F64,
   S390_BFP_I64_TO_F128,
   S390_BFP_U32_TO_F32,
   S390_BFP_U32_TO_F64,
   S390_BFP_U32_TO_F128,
   S390_BFP_U64_TO_F32,
   S390_BFP_U64_TO_F64,
   S390_BFP_U64_TO_F128,
   S390_BFP_F32_TO_I32,
   S390_BFP_F32_TO_I64,
   S390_BFP_F32_TO_U32,
   S390_BFP_F32_TO_U64,
   S390_BFP_F32_TO_F64,
   S390_BFP_F32_TO_F128,
   S390_BFP_F64_TO_I32,
   S390_BFP_F64_TO_I64,
   S390_BFP_F64_TO_U32,
   S390_BFP_F64_TO_U64,
   S390_BFP_F64_TO_F32,
   S390_BFP_F64_TO_F128,
   S390_BFP_F128_TO_I32,
   S390_BFP_F128_TO_I64,
   S390_BFP_F128_TO_U32,
   S390_BFP_F128_TO_U64,
   S390_BFP_F128_TO_F32,
   S390_BFP_F128_TO_F64,
   S390_BFP_F32_TO_F32I,
   S390_BFP_F64_TO_F64I
} s390_bfp_conv_t;

/* Type conversion operations: to and/or from decimal floating point */
typedef enum {
   S390_DFP_D32_TO_D64,
   S390_DFP_D64_TO_D32,
   S390_DFP_D64_TO_D128,
   S390_DFP_D128_TO_D64,
   S390_DFP_I32_TO_D64,
   S390_DFP_I32_TO_D128,
   S390_DFP_I64_TO_D64,
   S390_DFP_I64_TO_D128,
   S390_DFP_U32_TO_D64,
   S390_DFP_U32_TO_D128,
   S390_DFP_U64_TO_D64,
   S390_DFP_U64_TO_D128,
   S390_DFP_D64_TO_I32,
   S390_DFP_D64_TO_I64,
   S390_DFP_D64_TO_U32,
   S390_DFP_D64_TO_U64,
   S390_DFP_D128_TO_I32,
   S390_DFP_D128_TO_I64,
   S390_DFP_D128_TO_U32,
   S390_DFP_D128_TO_U64
} s390_dfp_conv_t;

typedef enum {
   S390_FP_F32_TO_D32,
   S390_FP_F32_TO_D64,
   S390_FP_F32_TO_D128,
   S390_FP_F64_TO_D32,
   S390_FP_F64_TO_D64,
   S390_FP_F64_TO_D128,
   S390_FP_F128_TO_D32,
   S390_FP_F128_TO_D64,
   S390_FP_F128_TO_D128,
   S390_FP_D32_TO_F32,
   S390_FP_D32_TO_F64,
   S390_FP_D32_TO_F128,
   S390_FP_D64_TO_F32,
   S390_FP_D64_TO_F64,
   S390_FP_D64_TO_F128,
   S390_FP_D128_TO_F32,
   S390_FP_D128_TO_F64,
   S390_FP_D128_TO_F128
} s390_fp_conv_t;

/* The kind of binary DFP operations */
typedef enum {
   S390_DFP_ADD,
   S390_DFP_SUB,
   S390_DFP_MUL,
   S390_DFP_DIV,
   S390_DFP_QUANTIZE
} s390_dfp_binop_t;

/* The kind of unary DFP operations */
typedef enum {
   S390_DFP_EXTRACT_EXP_D64,
   S390_DFP_EXTRACT_EXP_D128,
   S390_DFP_EXTRACT_SIG_D64,
   S390_DFP_EXTRACT_SIG_D128,
} s390_dfp_unop_t;

/* The DFP operations with 2 operands one of them being integer */
typedef enum {
   S390_DFP_SHIFT_LEFT,
   S390_DFP_SHIFT_RIGHT,
   S390_DFP_INSERT_EXP
} s390_dfp_intop_t;

/* The kind of DFP compare operations */
typedef enum {
   S390_DFP_COMPARE,
   S390_DFP_COMPARE_EXP,
} s390_dfp_cmp_t;

/* The details of a CDAS insn. Carved out to keep the size of
   s390_insn low */
typedef struct {
   HReg        op1_high;
   HReg        op1_low;
   s390_amode *op2;
   HReg        op3_high;
   HReg        op3_low;
   HReg        old_mem_high;
   HReg        old_mem_low;
   HReg        scratch;
} s390_cdas;

/* The details of a binary DFP insn. Carved out to keep the size of
   s390_insn low */
typedef struct {
   s390_dfp_binop_t tag;
   s390_dfp_round_t rounding_mode;
   HReg         dst_hi; /* 128-bit result high part; 64-bit result */
   HReg         dst_lo; /* 128-bit result low part */
   HReg         op2_hi; /* 128-bit operand high part; 64-bit opnd 1 */
   HReg         op2_lo; /* 128-bit operand low part */
   HReg         op3_hi; /* 128-bit operand high part; 64-bit opnd 2 */
   HReg         op3_lo; /* 128-bit operand low part */
} s390_dfp_binop;

typedef struct {
   s390_fp_conv_t  tag;
   s390_dfp_round_t rounding_mode;
   HReg         dst_hi; /* 128-bit result high part; 32/64-bit result */
   HReg         dst_lo; /* 128-bit result low part */
   HReg         op_hi;  /* 128-bit operand high part; 32/64-bit opnd */
   HReg         op_lo;  /* 128-bit operand low part */
   HReg         r1;     /* clobbered register GPR #1 */
} s390_fp_convert;

/* Pseudo-insn for representing a helper call.
   TARGET is the absolute address of the helper function
   NUM_ARGS says how many arguments are being passed.
   All arguments have integer type and are being passed according to ABI,
   i.e. in registers r2, r3, r4, r5, and r6, with argument #0 being
   passed in r2 and so forth. */
typedef struct {
   s390_cc_t    cond     : 16;
   UInt         num_args : 16;
   RetLoc       rloc;     /* where the return value will be */
   Addr64       target;
   const HChar *name;      /* callee's name (for debugging) */
} s390_helper_call;

typedef struct {
   s390_insn_tag tag;
   /* Usually, this is the size of the result of an operation.
      Exceptions are:
      - for comparisons it is the size of the operand
   */
   UChar size;
   union {
      struct {
         HReg        dst;
         s390_amode *src;
      } load;
      struct {
         s390_amode *dst;
         HReg        src;
      } store;
      struct {
         HReg        dst;
         HReg        src;
      } move;
      struct {
         s390_amode *dst;
         s390_amode *src;
      } memcpy;
      struct {
         s390_cc_t     cond;
         HReg          dst;
         s390_opnd_RMI src;
      } cond_move;
      struct {
         HReg        dst;
         ULong       value;  /* not sign extended */
      } load_immediate;
      /* add, and, or, xor */
      struct {
         s390_alu_t    tag;
         HReg          dst; /* op1 */
         s390_opnd_RMI op2;
      } alu;
      struct {
         HReg          dst_hi;  /*           r10 */
         HReg          dst_lo;  /* also op1  r11 */
         s390_opnd_RMI op2;
      } mul;
      struct {
         HReg          op1_hi;  /* also remainder   r10 */
         HReg          op1_lo;  /* also quotient    r11 */
         s390_opnd_RMI op2;
      } div;
      struct {
         HReg          rem; /* remainder      r10 */
         HReg          op1; /* also quotient  r11 */
         s390_opnd_RMI op2;
      } divs;
      struct {
         HReg          num_bits; /* number of leftmost '0' bits  r10 */
         HReg          clobber;  /* unspecified                  r11 */
         s390_opnd_RMI src;
      } clz;
      struct {
         s390_unop_t   tag;
         HReg          dst;
         s390_opnd_RMI src;
      } unop;
      struct {
         Bool          signed_comparison;
         HReg          src1;
         s390_opnd_RMI src2;
      } compare;
      struct {
         s390_opnd_RMI src;
      } test;
      /* Convert the condition code to a boolean value. */
      struct {
         s390_cc_t cond;
         HReg      dst;
      } cc2bool;
      struct {
         HReg        op1;
         s390_amode *op2;
         HReg        op3;
         HReg        old_mem;
      } cas;
      struct {
         s390_cdas *details;
      } cdas;
      struct {
         s390_helper_call *details;
      } helper_call;

      /* Floating point instructions (including conversion to/from floating
         point

         128-bit floating point requires register pairs. As the registers
         in a register pair cannot be chosen independently it would suffice
         to store only one register of the pair in order to represent it.
         We chose not to do that as being explicit about all registers
         helps with debugging and does not require special handling in 
         e.g. s390_insn_get_reg_usage, It'd be all too easy to forget about
         the "other" register in a pair if it is implicit.

         The convention for all fp s390_insn is that the _hi register will
         be used to store the result / operand of a 32/64-bit operation.
         The _hi register holds the  8 bytes of HIgher significance of a
         128-bit value (hence the suffix). However, it is the lower numbered
         register of a register pair. POP says that the lower numbered
         register is used to identify the pair in an insn encoding. So,
         when an insn is emitted, only the _hi registers need to be looked
         at. Nothing special is needed for 128-bit BFP which is nice.
      */

      /* There are currently no ternary 128-bit BFP operations. */
      struct {
         s390_bfp_triop_t tag;
         HReg         dst;
         HReg         op2;
         HReg         op3;
      } bfp_triop;
      struct {
         s390_bfp_binop_t tag;
         HReg         dst_hi; /* 128-bit result high part; 32/64-bit result */
         HReg         dst_lo; /* 128-bit result low part */
         HReg         op2_hi; /* 128-bit operand high part; 32/64-bit opnd */
         HReg         op2_lo; /* 128-bit operand low part */
      } bfp_binop;
      struct {
         s390_bfp_unop_t  tag;
         HReg         dst_hi; /* 128-bit result high part; 32/64-bit result */
         HReg         dst_lo; /* 128-bit result low part */
         HReg         op_hi;  /* 128-bit operand high part; 32/64-bit opnd */
         HReg         op_lo;  /* 128-bit operand low part */
      } bfp_unop;
      struct {
         s390_bfp_conv_t  tag;
         s390_bfp_round_t rounding_mode;
         HReg         dst_hi; /* 128-bit result high part; 32/64-bit result */
         HReg         dst_lo; /* 128-bit result low part */
         HReg         op_hi;  /* 128-bit operand high part; 32/64-bit opnd */
         HReg         op_lo;  /* 128-bit operand low part */
      } bfp_convert;
      struct {
         HReg         dst;     /* condition code in s390 encoding */
         HReg         op1_hi;  /* 128-bit operand high part; 32/64-bit opnd */
         HReg         op1_lo;  /* 128-bit operand low part */
         HReg         op2_hi;  /* 128-bit operand high part; 32/64-bit opnd */
         HReg         op2_lo;  /* 128-bit operand low part */
      } bfp_compare;
      struct {
         s390_dfp_binop *details;
      } dfp_binop;
      struct {
         s390_dfp_unop_t tag;
         HReg         dst_hi; /* 128-bit result high part; 64-bit result */
         HReg         dst_lo; /* 128-bit result low part */
         HReg         op_hi;  /* 128-bit operand high part; 64-bit opnd */
         HReg         op_lo;  /* 128-bit operand low part */
      } dfp_unop;
      struct {
         s390_dfp_intop_t tag;
         HReg         dst_hi; /* 128-bit result high part; 64-bit result */
         HReg         dst_lo; /* 128-bit result low part */
         HReg         op2;    /* integer operand */
         HReg         op3_hi; /* 128-bit operand high part; 64-bit opnd */
         HReg         op3_lo; /* 128-bit operand low part */
      } dfp_intop;
      struct {
         s390_dfp_conv_t  tag;
         s390_dfp_round_t rounding_mode;
         HReg         dst_hi; /* 128-bit result high part; 64-bit result */
         HReg         dst_lo; /* 128-bit result low part */
         HReg         op_hi;  /* 128-bit operand high part; 64-bit opnd */
         HReg         op_lo;  /* 128-bit operand low part */
      } dfp_convert;
      struct {
         s390_fp_convert *details;
      } fp_convert;
      struct {
         s390_dfp_cmp_t tag;
         HReg         dst;     /* condition code in s390 encoding */
         HReg         op1_hi;  /* 128-bit operand high part; 64-bit opnd 1 */
         HReg         op1_lo;  /* 128-bit operand low part */
         HReg         op2_hi;  /* 128-bit operand high part; 64-bit opnd 2 */
         HReg         op2_lo;  /* 128-bit operand low part */
      } dfp_compare;
      struct {
         s390_dfp_round_t rounding_mode;
         HReg         dst_hi; /* 128-bit result high part; 64-bit result */
         HReg         dst_lo; /* 128-bit result low part */
         HReg         op2;    /* integer operand */
         HReg         op3_hi; /* 128-bit operand high part; 64-bit opnd */
         HReg         op3_lo; /* 128-bit operand low part */
      } dfp_reround;

      /* Miscellaneous */
      struct {
         s390_amode      *dst;
         ULong            value;  /* sign extended */
      } mimm;
      struct {
         s390_amode      *dst;
         UChar            delta;
         ULong            value;  /* for debugging only */
      } madd;
      struct {
         HReg             mode;
      } set_fpc_bfprm;
      struct {
         HReg             mode;
      } set_fpc_dfprm;

      /* The next 5 entries are generic to support translation chaining */

      /* Update the guest IA value, then exit requesting to chain
         to it.  May be conditional. */
      struct {
         s390_cc_t     cond;
         Bool          to_fast_entry;  /* chain to the what entry point? */
         Addr64        dst;            /* next guest address */
         s390_amode   *guest_IA;
      } xdirect;
      /* Boring transfer to a guest address not known at JIT time.
         Not chainable.  May be conditional. */
      struct {
         s390_cc_t     cond;
         HReg          dst;
         s390_amode   *guest_IA;
      } xindir;
      /* Assisted transfer to a guest address, most general case.
         Not chainable.  May be conditional. */
      struct {
         s390_cc_t     cond;
         IRJumpKind    kind;
         HReg          dst;
         s390_amode   *guest_IA;
      } xassisted;
      struct {
         /* fixs390: I don't think these are really needed
            as the gsp and the offset are fixed  no ? */
         s390_amode   *counter;    /* dispatch counter */
         s390_amode   *fail_addr;
      } evcheck;
      struct {
         /* No fields.  The address of the counter to increment is
            installed later, post-translation, by patching it in,
            as it is not known at translation time. */
      } profinc;

   } variant;
} s390_insn;

s390_insn *s390_insn_load(UChar size, HReg dst, s390_amode *src);
s390_insn *s390_insn_store(UChar size, s390_amode *dst, HReg src);
s390_insn *s390_insn_move(UChar size, HReg dst, HReg src);
s390_insn *s390_insn_memcpy(UChar size, s390_amode *dst, s390_amode *src);
s390_insn *s390_insn_cond_move(UChar size, s390_cc_t cond, HReg dst,
                               s390_opnd_RMI src);
s390_insn *s390_insn_load_immediate(UChar size, HReg dst, ULong val);
s390_insn *s390_insn_alu(UChar size, s390_alu_t, HReg dst,
                         s390_opnd_RMI op2);
s390_insn *s390_insn_mul(UChar size, HReg dst_hi, HReg dst_lo,
                         s390_opnd_RMI op2, Bool signed_multiply);
s390_insn *s390_insn_div(UChar size, HReg op1_hi, HReg op1_lo,
                         s390_opnd_RMI op2, Bool signed_divide);
s390_insn *s390_insn_divs(UChar size, HReg rem, HReg op1, s390_opnd_RMI op2);
s390_insn *s390_insn_clz(UChar size, HReg num_bits, HReg clobber,
                         s390_opnd_RMI op);
s390_insn *s390_insn_cas(UChar size, HReg op1, s390_amode *op2, HReg op3,
                         HReg old);
s390_insn *s390_insn_cdas(UChar size, HReg op1_high, HReg op1_low,
                          s390_amode *op2, HReg op3_high, HReg op3_low,
                          HReg old_high, HReg old_low, HReg scratch);
s390_insn *s390_insn_unop(UChar size, s390_unop_t tag, HReg dst,
                          s390_opnd_RMI opnd);
s390_insn *s390_insn_cc2bool(HReg dst, s390_cc_t src);
s390_insn *s390_insn_test(UChar size, s390_opnd_RMI src);
s390_insn *s390_insn_compare(UChar size, HReg dst, s390_opnd_RMI opnd,
                             Bool signed_comparison);
s390_insn *s390_insn_helper_call(s390_cc_t cond, Addr64 target, UInt num_args,
                                 const HChar *name, RetLoc rloc);
s390_insn *s390_insn_bfp_triop(UChar size, s390_bfp_triop_t, HReg dst,
                               HReg op2, HReg op3);
s390_insn *s390_insn_bfp_binop(UChar size, s390_bfp_binop_t, HReg dst,
                               HReg op2);
s390_insn *s390_insn_bfp_unop(UChar size, s390_bfp_unop_t tag, HReg dst,
                              HReg op);
s390_insn *s390_insn_bfp_compare(UChar size, HReg dst, HReg op1, HReg op2);
s390_insn *s390_insn_bfp_convert(UChar size, s390_bfp_conv_t tag, HReg dst,
                                 HReg op, s390_bfp_round_t);
s390_insn *s390_insn_bfp128_binop(UChar size, s390_bfp_binop_t, HReg dst_hi,
                                  HReg dst_lo, HReg op2_hi, HReg op2_lo);
s390_insn *s390_insn_bfp128_unop(UChar size, s390_bfp_unop_t, HReg dst_hi,
                                 HReg dst_lo, HReg op_hi, HReg op_lo);
s390_insn *s390_insn_bfp128_compare(UChar size, HReg dst, HReg op1_hi,
                                    HReg op1_lo, HReg op2_hi, HReg op2_lo);
s390_insn *s390_insn_bfp128_convert_to(UChar size, s390_bfp_conv_t,
                                       HReg dst_hi, HReg dst_lo, HReg op);
s390_insn *s390_insn_bfp128_convert_from(UChar size, s390_bfp_conv_t,
                                         HReg dst_hi, HReg dst_lo, HReg op_hi,
                                         HReg op_lo, s390_bfp_round_t);
s390_insn *s390_insn_dfp_binop(UChar size, s390_dfp_binop_t, HReg dst,
                               HReg op2, HReg op3,
                               s390_dfp_round_t rounding_mode);
s390_insn *s390_insn_dfp_unop(UChar size, s390_dfp_unop_t, HReg dst, HReg op);
s390_insn *s390_insn_dfp_intop(UChar size, s390_dfp_intop_t, HReg dst,
                               HReg op2, HReg op3);
s390_insn *s390_insn_dfp_compare(UChar size, s390_dfp_cmp_t, HReg dst,
                                 HReg op1, HReg op2);
s390_insn *s390_insn_dfp_convert(UChar size, s390_dfp_conv_t tag, HReg dst,
                                 HReg op, s390_dfp_round_t);
s390_insn *s390_insn_dfp_reround(UChar size, HReg dst, HReg op2, HReg op3,
                                 s390_dfp_round_t);
s390_insn *s390_insn_fp_convert(UChar size, s390_fp_conv_t tag,
                                HReg dst, HReg op, HReg r1, s390_dfp_round_t);
s390_insn *s390_insn_fp128_convert(UChar size, s390_fp_conv_t tag,
                                   HReg dst_hi, HReg dst_lo, HReg op_hi,
                                   HReg op_lo, HReg r1, s390_dfp_round_t);
s390_insn *s390_insn_dfp128_binop(UChar size, s390_dfp_binop_t, HReg dst_hi,
                                  HReg dst_lo, HReg op2_hi, HReg op2_lo,
                                  HReg op3_hi, HReg op3_lo,
                                  s390_dfp_round_t rounding_mode);
s390_insn *s390_insn_dfp128_unop(UChar size, s390_dfp_unop_t, HReg dst,
                                 HReg op_hi, HReg op_lo);
s390_insn *s390_insn_dfp128_intop(UChar size, s390_dfp_intop_t, HReg dst_hi,
                                  HReg dst_lo, HReg op2,
                                  HReg op3_hi, HReg op3_lo);
s390_insn *s390_insn_dfp128_compare(UChar size, s390_dfp_cmp_t, HReg dst,
                                    HReg op1_hi, HReg op1_lo, HReg op2_hi,
                                    HReg op2_lo);
s390_insn *s390_insn_dfp128_convert_to(UChar size, s390_dfp_conv_t,
                                       HReg dst_hi, HReg dst_lo, HReg op);
s390_insn *s390_insn_dfp128_convert_from(UChar size, s390_dfp_conv_t,
                                         HReg dst_hi, HReg dst_lo, HReg op_hi,
                                         HReg op_lo, s390_dfp_round_t);
s390_insn *s390_insn_dfp128_reround(UChar size, HReg dst_hi, HReg dst_lo,
                                    HReg op2, HReg op3_hi, HReg op3_lo,
                                    s390_dfp_round_t);
s390_insn *s390_insn_mfence(void);
s390_insn *s390_insn_mimm(UChar size, s390_amode *dst, ULong value);
s390_insn *s390_insn_madd(UChar size, s390_amode *dst, UChar delta,
                          ULong value);
s390_insn *s390_insn_set_fpc_bfprm(UChar size, HReg mode);
s390_insn *s390_insn_set_fpc_dfprm(UChar size, HReg mode);

/* Five for translation chaining */
s390_insn *s390_insn_xdirect(s390_cc_t cond, Addr64 dst, s390_amode *guest_IA,
                             Bool to_fast_entry);
s390_insn *s390_insn_xindir(s390_cc_t cond, HReg dst, s390_amode *guest_IA);
s390_insn *s390_insn_xassisted(s390_cc_t cond, HReg dst, s390_amode *guest_IA,
                               IRJumpKind kind);
s390_insn *s390_insn_evcheck(s390_amode *counter, s390_amode *fail_addr);
s390_insn *s390_insn_profinc(void);

const HChar *s390_insn_as_string(const s390_insn *);

/*--------------------------------------------------------*/
/* --- Interface exposed to VEX                       --- */
/*--------------------------------------------------------*/

void ppS390AMode(const s390_amode *);
void ppS390Instr(const s390_insn *, Bool mode64);
void ppHRegS390(HReg);

/* Some functions that insulate the register allocator from details
   of the underlying instruction set. */
void  getRegUsage_S390Instr( HRegUsage *, const s390_insn *, Bool );
void  mapRegs_S390Instr    ( HRegRemap *, s390_insn *, Bool );
Bool  isMove_S390Instr     ( const s390_insn *, HReg *, HReg * );
Int   emit_S390Instr       ( Bool *, UChar *, Int, const s390_insn *, Bool,
                             VexEndness, const void *, const void *,
                             const void *, const void *);
const RRegUniverse *getRRegUniverse_S390( void );
void  genSpill_S390        ( HInstr **, HInstr **, HReg , Int , Bool );
void  genReload_S390       ( HInstr **, HInstr **, HReg , Int , Bool );
HInstrArray *iselSB_S390   ( const IRSB *, VexArch, const VexArchInfo *,
                             const VexAbiInfo *, Int, Int, Bool, Bool, Addr);

/* Return the number of bytes of code needed for an event check */
Int evCheckSzB_S390(void);

/* Perform a chaining and unchaining of an XDirect jump. */
VexInvalRange chainXDirect_S390(VexEndness endness_host,
                                void *place_to_chain,
                                const void *disp_cp_chain_me_EXPECTED,
                                const void *place_to_jump_to);

VexInvalRange unchainXDirect_S390(VexEndness endness_host,
                                  void *place_to_unchain,
                                  const void *place_to_jump_to_EXPECTED,
                                  const void *disp_cp_chain_me);

/* Patch the counter location into an existing ProfInc point. */
VexInvalRange patchProfInc_S390(VexEndness endness_host,
                                void  *code_to_patch,
                                const ULong *location_of_counter);

/* KLUDGE: See detailled comment in host_s390_defs.c. */
extern UInt s390_host_hwcaps;

/* Convenience macros to test installed facilities */
#define s390_host_has_ldisp \
                      (s390_host_hwcaps & (VEX_HWCAPS_S390X_LDISP))
#define s390_host_has_eimm \
                      (s390_host_hwcaps & (VEX_HWCAPS_S390X_EIMM))
#define s390_host_has_gie \
                      (s390_host_hwcaps & (VEX_HWCAPS_S390X_GIE))
#define s390_host_has_dfp \
                      (s390_host_hwcaps & (VEX_HWCAPS_S390X_DFP))
#define s390_host_has_fgx \
                      (s390_host_hwcaps & (VEX_HWCAPS_S390X_FGX))
#define s390_host_has_etf2 \
                      (s390_host_hwcaps & (VEX_HWCAPS_S390X_ETF2))
#define s390_host_has_stfle \
                      (s390_host_hwcaps & (VEX_HWCAPS_S390X_STFLE))
#define s390_host_has_etf3 \
                      (s390_host_hwcaps & (VEX_HWCAPS_S390X_ETF3))
#define s390_host_has_stckf \
                      (s390_host_hwcaps & (VEX_HWCAPS_S390X_STCKF))
#define s390_host_has_fpext \
                      (s390_host_hwcaps & (VEX_HWCAPS_S390X_FPEXT))
#define s390_host_has_lsc \
                      (s390_host_hwcaps & (VEX_HWCAPS_S390X_LSC))
#define s390_host_has_pfpo \
                      (s390_host_hwcaps & (VEX_HWCAPS_S390X_PFPO))

#endif /* ndef __VEX_HOST_S390_DEFS_H */

/*---------------------------------------------------------------*/
/*--- end                                    host_s390_defs.h ---*/
/*---------------------------------------------------------------*/
