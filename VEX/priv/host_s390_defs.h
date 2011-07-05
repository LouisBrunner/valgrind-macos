/* -*- mode: C; c-basic-offset: 3; -*- */

/*---------------------------------------------------------------*/
/*--- begin                                  host_s390_defs.h ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright IBM Corp. 2010-2011

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
#include "main_util.h"                    /* needed for host_generic_regs.h */
#include "host_generic_regs.h"            /* HReg */

/* --------- Registers --------- */
const HChar *s390_hreg_as_string(HReg);

/* Dedicated registers */
HReg s390_hreg_guest_state_pointer(void);


/* Given the index of a function argument, return the number of the
   general purpose register in which it is being passed. Arguments are
   counted 0, 1, 2, ... and they are being passed in r2, r3, r4, ... */
static __inline__ unsigned
s390_gprno_from_arg_index(unsigned ix)
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
   S390_INSN_COND_MOVE, /* conditonal "move" to register */
   S390_INSN_LOAD_IMMEDIATE,
   S390_INSN_ALU,
   S390_INSN_MUL,    /* n-bit operands; 2n-bit result */
   S390_INSN_DIV,    /* 2n-bit dividend; n-bit divisor; n-bit quot/rem */
   S390_INSN_DIVS,   /* n-bit dividend; n-bit divisor; n-bit quot/rem */
   S390_INSN_CLZ,    /* count left-most zeroes */
   S390_INSN_UNOP,
   S390_INSN_TEST,   /* test operand and set cc */
   S390_INSN_CC2BOOL,/* convert condition code to 0/1 */
   S390_INSN_COMPARE,
   S390_INSN_BRANCH, /* un/conditional goto */
   S390_INSN_HELPER_CALL,
   S390_INSN_CAS,    /* compare and swap */
   S390_INSN_BFP_BINOP, /* Binary floating point 32-bit / 64-bit */
   S390_INSN_BFP_UNOP,
   S390_INSN_BFP_TRIOP,
   S390_INSN_BFP_COMPARE,
   S390_INSN_BFP128_BINOP, /* Binary floating point 128-bit */
   S390_INSN_BFP128_UNOP,
   S390_INSN_BFP128_COMPARE,
   S390_INSN_BFP128_CONVERT_TO,
   S390_INSN_BFP128_CONVERT_FROM,
   S390_INSN_MFENCE
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
   S390_BFP_SQRT,
   S390_BFP_I32_TO_F32,
   S390_BFP_I32_TO_F64,
   S390_BFP_I32_TO_F128,
   S390_BFP_I64_TO_F32,
   S390_BFP_I64_TO_F64,
   S390_BFP_I64_TO_F128,
   S390_BFP_F32_TO_I32,
   S390_BFP_F32_TO_I64,
   S390_BFP_F32_TO_F64,
   S390_BFP_F32_TO_F128,
   S390_BFP_F64_TO_I32,
   S390_BFP_F64_TO_I64,
   S390_BFP_F64_TO_F32,
   S390_BFP_F64_TO_F128,
   S390_BFP_F128_TO_I32,
   S390_BFP_F128_TO_I64,
   S390_BFP_F128_TO_F32,
   S390_BFP_F128_TO_F64
} s390_bfp_unop_t;


/* Condition code. The encoding of the enumerators matches the value of
   the mask field in the various branch opcodes. */
typedef enum {
   S390_CC_NEVER=  0,
   S390_CC_OVFL =  1,   /* overflow */
   S390_CC_H    =  2,   /* A > B ; high */
   S390_CC_NLE  =  3,   /* not low or equal */
   S390_CC_L    =  4,   /* A < B ; low */
   S390_CC_NHE  =  5,   /* not high or equal */
   S390_CC_LH   =  6,   /* low or high */
   S390_CC_NE   =  7,   /* A != B ; not zero */
   S390_CC_E    =  8,   /* A == B ; zero */
   S390_CC_NLH  =  9,   /* not low or high */
   S390_CC_HE   = 10,   /* A >= B ; high or equal*/
   S390_CC_NL   = 11,   /* not low */
   S390_CC_LE   = 12,   /* A <= B ; low or equal */
   S390_CC_NH   = 13,   /* not high */
   S390_CC_NO   = 14,   /* not overflow */
   S390_CC_ALWAYS = 15
} s390_cc_t;


/* Rounding mode as it is encoded in the m3/m4 fields of certain
   instructions (e.g. CFEBR) */
typedef enum {
/* S390_ROUND_NEAREST_AWAY = 1, not supported */
   S390_ROUND_NEAREST_EVEN = 4,
   S390_ROUND_ZERO         = 5,
   S390_ROUND_POSINF       = 6,
   S390_ROUND_NEGINF       = 7
} s390_round_t;


/* Invert the condition code */
static __inline__ s390_cc_t
s390_cc_invert(s390_cc_t cond)
{
   return S390_CC_ALWAYS - cond;
}


typedef struct {
   s390_insn_tag tag;
   UChar size;            /* size of the result in bytes */
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
         Bool          signed_multiply;
         HReg          dst_hi;  /*           r10 */
         HReg          dst_lo;  /* also op1  r11 */
         s390_opnd_RMI op2;
      } mul;
      struct {
         Bool          signed_divide;
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
         HReg          dst;  /* condition code in s390 encoding */
         HReg          op1;
         HReg          op2;
      } bfp_compare;
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
         IRJumpKind    kind;
         s390_cc_t     cond;
         s390_opnd_RMI dst;
      } branch;
      /* Pseudo-insn for representing a helper call.
         TARGET is the absolute address of the helper function
         NUM_ARGS says how many arguments are being passed.
         All arguments have integer type and are being passed according to ABI,
         i.e. in registers r2, r3, r4, r5, and r6, with argument #0 being
         passed in r2 and so forth. */
      struct {
         s390_cc_t cond;
         Addr64    target;
         UInt      num_args;
         HChar    *name;      /* callee's name (for debugging) */
      } helper_call;
      struct {
         s390_bfp_triop_t tag;
         s390_round_t     rounding_mode;
         HReg             dst; /* first operand */
         HReg             op2; /* second operand */
         HReg             op3; /* third operand */
      } bfp_triop;
      struct {
         s390_bfp_binop_t tag;
         s390_round_t     rounding_mode;
         HReg             dst; /* left operand */
         HReg             op2; /* right operand */
      } bfp_binop;
      struct {
         s390_bfp_unop_t tag;
         s390_round_t    rounding_mode;
         HReg            dst;  /* result */
         HReg            op;   /* operand */
      } bfp_unop;
      struct {
         s390_bfp_binop_t tag;
         s390_round_t     rounding_mode;
         HReg             dst_hi; /* left operand; high part */
         HReg             dst_lo; /* left operand; low part */
         HReg             op2_hi; /* right operand; high part */
         HReg             op2_lo; /* right operand; low part */
      } bfp128_binop;
      /* This variant is also used by the BFP128_CONVERT_TO and
         BFP128_CONVERT_FROM insns. */
      struct {
         s390_bfp_unop_t  tag;
         s390_round_t     rounding_mode;
         HReg             dst_hi; /* result; high part */
         HReg             dst_lo; /* result; low part */
         HReg             op_hi;  /* operand; high part */
         HReg             op_lo;  /* operand; low part */
      } bfp128_unop;
      struct {
         HReg             dst;    /* condition code in s390 encoding */
         HReg             op1_hi; /* left operand; high part */
         HReg             op1_lo; /* left operand; low part */
         HReg             op2_hi; /* right operand; high part */
         HReg             op2_lo; /* right operand; low part */
      } bfp128_compare;
   } variant;
} s390_insn;

s390_insn *s390_insn_load(UChar size, HReg dst, s390_amode *src);
s390_insn *s390_insn_store(UChar size, s390_amode *dst, HReg src);
s390_insn *s390_insn_move(UChar size, HReg dst, HReg src);
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
s390_insn *s390_insn_unop(UChar size, s390_unop_t tag, HReg dst,
                          s390_opnd_RMI opnd);
s390_insn *s390_insn_cc2bool(HReg dst, s390_cc_t src);
s390_insn *s390_insn_test(UChar size, s390_opnd_RMI src);
s390_insn *s390_insn_compare(UChar size, HReg dst, s390_opnd_RMI opnd,
                             Bool signed_comparison);
s390_insn *s390_insn_branch(IRJumpKind jk, s390_cc_t cond, s390_opnd_RMI dst);
s390_insn *s390_insn_helper_call(s390_cc_t cond, Addr64 target, UInt num_args,
                                 HChar *name);
s390_insn *s390_insn_bfp_triop(UChar size, s390_bfp_triop_t, HReg dst, HReg op2,
                               HReg op3, s390_round_t);
s390_insn *s390_insn_bfp_binop(UChar size, s390_bfp_binop_t, HReg dst, HReg op2,
                               s390_round_t);
s390_insn *s390_insn_bfp_unop(UChar size, s390_bfp_unop_t tag, HReg dst,
                              HReg op, s390_round_t);
s390_insn *s390_insn_bfp_compare(UChar size, HReg dst, HReg op1, HReg op2);
s390_insn *s390_insn_bfp128_binop(UChar size, s390_bfp_binop_t, HReg dst_hi,
                                  HReg dst_lo, HReg op2_hi, HReg op2_lo,
                                  s390_round_t);
s390_insn *s390_insn_bfp128_unop(UChar size, s390_bfp_unop_t, HReg dst_hi,
                                 HReg dst_lo, HReg op_hi, HReg op_lo,
                                 s390_round_t);
s390_insn *s390_insn_bfp128_compare(UChar size, HReg dst, HReg op1_hi,
                                    HReg op1_lo, HReg op2_hi, HReg op2_lo);
s390_insn *s390_insn_bfp128_convert_to(UChar size, s390_bfp_unop_t,
                                       HReg dst_hi, HReg dst_lo, HReg op);
s390_insn *s390_insn_bfp128_convert_from(UChar size, s390_bfp_unop_t,
                                         HReg dst, HReg op_hi, HReg op_lo,
                                         s390_round_t);
s390_insn *s390_insn_mfence(void);
UInt       s390_insn_emit(UChar *buf, Int nbuf, const s390_insn *insn,
                          void *dispatch);

const HChar *s390_insn_as_string(const s390_insn *);

/*--------------------------------------------------------*/
/* --- Interface exposed to VEX                       --- */
/*--------------------------------------------------------*/

void ppS390AMode(s390_amode *);
void ppS390Instr(s390_insn *, Bool mode64);
void ppHRegS390(HReg);

/* Some functions that insulate the register allocator from details
   of the underlying instruction set. */
void  getRegUsage_S390Instr( HRegUsage *, s390_insn *, Bool );
void  mapRegs_S390Instr    ( HRegRemap *, s390_insn *, Bool );
Bool  isMove_S390Instr     ( s390_insn *, HReg *, HReg * );
Int   emit_S390Instr       ( UChar *, Int, s390_insn *, Bool,
                             void *, void * );
void  getAllocableRegs_S390( Int *, HReg **, Bool );
void  genSpill_S390        ( HInstr **, HInstr **, HReg , Int , Bool );
void  genReload_S390       ( HInstr **, HInstr **, HReg , Int , Bool );
s390_insn *directReload_S390 ( s390_insn *, HReg, Short );
HInstrArray *iselSB_S390   ( IRSB *, VexArch, VexArchInfo *, VexAbiInfo * );

/* KLUDGE: See detailled comment in host_s390_defs.c. */
extern const VexArchInfo *s390_archinfo_host;

/* Convenience macros to test installed facilities */
#define s390_host_has_ldisp \
                      (s390_archinfo_host->hwcaps & (VEX_HWCAPS_S390X_LDISP))
#define s390_host_has_eimm \
                      (s390_archinfo_host->hwcaps & (VEX_HWCAPS_S390X_EIMM))
#define s390_host_has_gie \
                      (s390_archinfo_host->hwcaps & (VEX_HWCAPS_S390X_GIE))
#define s390_host_has_dfp \
                      (s390_archinfo_host->hwcaps & (VEX_HWCAPS_S390X_DFP))
#define s390_host_has_fgx \
                      (s390_archinfo_host->hwcaps & (VEX_HWCAPS_S390X_FGX))

#endif /* ndef __VEX_HOST_S390_DEFS_H */

/*---------------------------------------------------------------*/
/*--- end                                    host_s390_defs.h ---*/
/*---------------------------------------------------------------*/
