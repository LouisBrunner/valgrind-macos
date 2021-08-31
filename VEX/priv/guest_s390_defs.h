/* -*- mode: C; c-basic-offset: 3; -*- */

/*---------------------------------------------------------------*/
/*--- begin                                 guest_s390_defs.h ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright IBM Corp. 2010-2020

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

/* Contributed by Florian Krohm */

#ifndef __VEX_GUEST_S390_DEFS_H
#define __VEX_GUEST_S390_DEFS_H

#include "libvex_basictypes.h"        // offsetof
#include "guest_generic_bb_to_IR.h"   // DisResult
#include "libvex_guest_s390x.h"       // VexGuestS390XState


/* Convert one s390 insn to IR.  See the type DisOneInstrFn in
   guest_generic_bb_to_IR.h. */
DisResult disInstr_S390 ( IRSB*        irbb,
                          const UChar* guest_code,
                          Long         delta,
                          Addr         guest_IP,
                          VexArch      guest_arch,
                          const VexArchInfo* archinfo,
                          const VexAbiInfo*  abiinfo,
                          VexEndness   host_endness,
                          Bool         sigill_diag );

/* Used by the optimiser to specialise calls to helpers. */
IRExpr* guest_s390x_spechelper ( const HChar *function_name,
                                 IRExpr **args,
                                 IRStmt **precedingStmts,
                                 Int n_precedingStmts);


/* Describes to the optimiser which part of the guest state require
   precise memory exceptions.  This is logically part of the guest
   state description. */
Bool guest_s390x_state_requires_precise_mem_exns ( Int, Int,
                                                   VexRegisterUpdates );

extern VexGuestLayout s390xGuest_layout;


#define S390X_GUEST_OFFSET(x)  offsetof(VexGuestS390XState, x)

/*------------------------------------------------------------*/
/*--- Helper functions.                                    ---*/
/*------------------------------------------------------------*/
void s390x_dirtyhelper_EX(ULong torun);
ULong s390x_dirtyhelper_STCK(ULong *addr);
ULong s390x_dirtyhelper_STCKF(ULong *addr);
ULong s390x_dirtyhelper_STCKE(ULong *addr);
ULong s390x_dirtyhelper_STFLE(VexGuestS390XState *guest_state, ULong *addr);
void  s390x_dirtyhelper_CUxy(UChar *addr, ULong data, ULong num_bytes);
ULong s390x_dirtyhelper_vec_op(VexGuestS390XState *guest_state,
                               ULong details);
ULong s390_do_cu12_cu14_helper1(UInt byte1, UInt etf3_and_m3_is_1);
ULong s390_do_cu12_helper2(UInt byte1, UInt byte2, UInt byte3, UInt byte4,
                           ULong stuff);
ULong s390_do_cu14_helper2(UInt byte1, UInt byte2, UInt byte3, UInt byte4,
                           ULong stuff);
ULong s390_do_cu21(UInt srcvalue, UInt low_surrogate);
ULong s390_do_cu24(UInt srcvalue, UInt low_surrogate);
ULong s390_do_cu41(UInt srcvalue);
ULong s390_do_cu42(UInt srcvalue);
UInt  s390_do_cvb(ULong decimal);
ULong s390_do_cvd(ULong binary);
ULong s390_do_ecag(ULong op2addr);
UInt  s390_do_pfpo(UInt gpr0);
void  s390x_dirtyhelper_PPNO_query(VexGuestS390XState *guest_state, ULong r1, ULong r2);
ULong  s390x_dirtyhelper_PPNO_sha512(VexGuestS390XState *guest_state, ULong r1, ULong r2);
void  s390x_dirtyhelper_PPNO_sha512_load_param_block( void );
/* The various ways to compute the condition code. */
enum {
   S390_CC_OP_BITWISE = 0,
   S390_CC_OP_SIGNED_COMPARE = 1,
   S390_CC_OP_UNSIGNED_COMPARE = 2,
   S390_CC_OP_SIGNED_ADD_32 = 3,
   S390_CC_OP_SIGNED_ADD_64 = 4,
   S390_CC_OP_UNSIGNED_ADD_32 = 5,
   S390_CC_OP_UNSIGNED_ADD_64 = 6,
   S390_CC_OP_UNSIGNED_ADDC_32 = 7,
   S390_CC_OP_UNSIGNED_ADDC_64 = 8,
   S390_CC_OP_SIGNED_SUB_32 = 9,
   S390_CC_OP_SIGNED_SUB_64 = 10,
   S390_CC_OP_UNSIGNED_SUB_32 = 11,
   S390_CC_OP_UNSIGNED_SUB_64 = 12,
   S390_CC_OP_UNSIGNED_SUBB_32 = 13,
   S390_CC_OP_UNSIGNED_SUBB_64 = 14,
   S390_CC_OP_LOAD_AND_TEST = 15,
   S390_CC_OP_LOAD_POSITIVE_32 = 16,
   S390_CC_OP_LOAD_POSITIVE_64 = 17,
   S390_CC_OP_TEST_UNDER_MASK_8 = 18,
   S390_CC_OP_TEST_UNDER_MASK_16 = 19,
   S390_CC_OP_SHIFT_LEFT_32 = 20,
   S390_CC_OP_SHIFT_LEFT_64 = 21,
   S390_CC_OP_INSERT_CHAR_MASK_32 = 22,
   S390_CC_OP_BFP_RESULT_32 = 23,
   S390_CC_OP_BFP_RESULT_64 = 24,
   S390_CC_OP_BFP_RESULT_128 = 25,
   S390_CC_OP_BFP_32_TO_INT_32 = 26,
   S390_CC_OP_BFP_64_TO_INT_32 = 27,
   S390_CC_OP_BFP_128_TO_INT_32 = 28,
   S390_CC_OP_BFP_32_TO_INT_64 = 29,
   S390_CC_OP_BFP_64_TO_INT_64 = 30,
   S390_CC_OP_BFP_128_TO_INT_64 = 31,
   S390_CC_OP_BFP_TDC_32 = 32,
   S390_CC_OP_BFP_TDC_64 = 33,
   S390_CC_OP_BFP_TDC_128 = 34,
   S390_CC_OP_SET = 35,
   S390_CC_OP_BFP_32_TO_UINT_32 = 36,
   S390_CC_OP_BFP_64_TO_UINT_32 = 37,
   S390_CC_OP_BFP_128_TO_UINT_32 = 38,
   S390_CC_OP_BFP_32_TO_UINT_64 = 39,
   S390_CC_OP_BFP_64_TO_UINT_64 = 40,
   S390_CC_OP_BFP_128_TO_UINT_64 = 41,
   S390_CC_OP_DFP_RESULT_64 = 42,
   S390_CC_OP_DFP_RESULT_128 = 43,
   S390_CC_OP_DFP_TDC_32 = 44,
   S390_CC_OP_DFP_TDC_64 = 45,
   S390_CC_OP_DFP_TDC_128 = 46,
   S390_CC_OP_DFP_TDG_32 = 47,
   S390_CC_OP_DFP_TDG_64 = 48,
   S390_CC_OP_DFP_TDG_128 = 49,
   S390_CC_OP_DFP_64_TO_UINT_32 = 50,
   S390_CC_OP_DFP_128_TO_UINT_32 = 51,
   S390_CC_OP_DFP_64_TO_UINT_64 = 52,
   S390_CC_OP_DFP_128_TO_UINT_64 = 53,
   S390_CC_OP_DFP_64_TO_INT_32 = 54,
   S390_CC_OP_DFP_128_TO_INT_32 = 55,
   S390_CC_OP_DFP_64_TO_INT_64 = 56,
   S390_CC_OP_DFP_128_TO_INT_64 = 57,
   S390_CC_OP_PFPO_32 = 58,
   S390_CC_OP_PFPO_64 = 59,
   S390_CC_OP_PFPO_128 = 60,
   S390_CC_OP_MUL_32 = 61,
   S390_CC_OP_MUL_64 = 62
};

/*------------------------------------------------------------*/
/*--- Thunk layout                                         ---*/
/*------------------------------------------------------------*/

/*
   Z -- value is zero extended to 32 / 64 bit
   S -- value is sign extended to 32 / 64 bit
   F -- a binary floating point value
   D -- a decimal floating point value

   +--------------------------------+-----------------------+----------------------+-----------------+
   | op                             |   cc_dep1             |   cc_dep2            |   cc_ndep       |
   +--------------------------------+-----------------------+----------------------+-----------------+
   | S390_CC_OP_BITWISE             | Z result              |                      |                 |
   | S390_CC_OP_SIGNED_COMPARE      | S 1st operand         | S 2nd operand        |                 |
   | S390_CC_OP_UNSIGNED_COMPARE    | Z 1st operand         | Z 2nd operand        |                 |
   | S390_CC_OP_SIGNED_ADD_32       | S 1st operand         | S 2nd operand        |                 |
   | S390_CC_OP_SIGNED_ADD_64       | S 1st operand         | S 2nd operand        |                 |
   | S390_CC_OP_UNSIGNED_ADD_32     | Z 1st operand         | Z 2nd operand        |                 |
   | S390_CC_OP_UNSIGNED_ADD_64     | Z 1st operand         | Z 2nd operand        |                 |
   | S390_CC_OP_UNSIGNED_ADDC_32    | Z 1st operand         | Z 2nd operand        | Z carry in      |
   | S390_CC_OP_UNSIGNED_ADDC_64    | Z 1st operand         | Z 2nd operand        | Z carry in      |
   | S390_CC_OP_SIGNED_SUB_32       | S left operand        | S right operand      |                 |
   | S390_CC_OP_SIGNED_SUB_64       | S left operand        | S right operand      |                 |
   | S390_CC_OP_UNSIGNED_SUB_32     | Z left operand        | Z right operand      |                 |
   | S390_CC_OP_UNSIGNED_SUB_64     | Z left operand        | Z right operand      |                 |
   | S390_CC_OP_UNSIGNED_SUBB_32    | Z left operand        | Z right operand      | Z borrow in     |
   | S390_CC_OP_UNSIGNED_SUBB_64    | Z left operand        | Z right operand      | Z borrow in     |
   | S390_CC_OP_LOAD_AND_TEST       | S loaded value        |                      |                 |
   | S390_CC_OP_LOAD_POSITIVE_32    | S loaded value        |                      |                 |
   | S390_CC_OP_LOAD_POSITIVE_64    | S loaded value        |                      |                 |
   | S390_CC_OP_TEST_UNDER_MASK_8   | Z tested value        | Z mask               |                 |
   | S390_CC_OP_TEST_UNDER_MASK_16  | Z tested value        | Z mask               |                 |
   | S390_CC_OP_SHIFT_LEFT_32       | Z value to be shifted | Z shift amount       |                 |
   | S390_CC_OP_SHIFT_LEFT_64       | Z value to be shifted | Z shift amount       |                 |
   | S390_CC_OP_INSERT_CHAR_MASK_32 | Z result              | Z mask               |                 |
   | S390_CC_OP_BFP_RESULT_32       | F result              |                      |                 |
   | S390_CC_OP_BFP_RESULT_64       | F result              |                      |                 |
   | S390_CC_OP_BFP_RESULT_128      | F result hi 64 bits   | F result low 64 bits |                 |
   | S390_CC_OP_BFP_32_TO_INT_32    | F source              | Z rounding mode      |                 |
   | S390_CC_OP_BFP_64_TO_INT_32    | F source              | Z rounding mode      |                 |
   | S390_CC_OP_BFP_128_TO_INT_32   | F source hi 64 bits   | F source low 64 bits | Z rounding mode |
   | S390_CC_OP_BFP_32_TO_INT_64    | F source              | Z rounding mode      |                 |
   | S390_CC_OP_BFP_64_TO_INT_64    | F source              | Z rounding mode      |                 |
   | S390_CC_OP_BFP_128_TO_INT_64   | F source hi 64 bits   | F source low 64 bits | Z rounding mode |
   | S390_CC_OP_BFP_TDC_32          | F value               | Z class              |                 |
   | S390_CC_OP_BFP_TDC_64          | F value               | Z class              |                 |
   | S390_CC_OP_BFP_TDC_128         | F value hi 64 bits    | F value low 64 bits  | Z class         |
   | S390_CC_OP_SET                 | Z condition code      |                      |                 |
   | S390_CC_OP_BFP_32_TO_UINT_32   | F source              | Z rounding mode      |                 |
   | S390_CC_OP_BFP_64_TO_UINT_32   | F source              | Z rounding mode      |                 |
   | S390_CC_OP_BFP_128_TO_UINT_32  | F source hi 64 bits   | F source low 64 bits | Z rounding mode |
   | S390_CC_OP_BFP_32_TO_UINT_64   | F source              | Z rounding mode      |                 |
   | S390_CC_OP_BFP_64_TO_UINT_64   | F source              | Z rounding mode      |                 |
   | S390_CC_OP_BFP_128_TO_UINT_64  | F source hi 64 bits   | F source low 64 bits | Z rounding mode |
   | S390_CC_OP_DFP_RESULT_64       | D result              |                      |                 |
   | S390_CC_OP_DFP_RESULT_128      | D result hi 64 bits   | D result low 64 bits |                 |
   | S390_CC_OP_DFP_TDC_32          | D value               | Z class              |                 |
   | S390_CC_OP_DFP_TDC_64          | D value               | Z class              |                 |
   | S390_CC_OP_DFP_TDC_128         | D value hi 64 bits    | D value low 64 bits  | Z class         |
   | S390_CC_OP_DFP_TDG_32          | D value               | Z group              |                 |
   | S390_CC_OP_DFP_TDG_64          | D value               | Z group              |                 |
   | S390_CC_OP_DFP_TDG_128         | D value hi 64 bits    | D value low 64 bits  | Z group         |
   | S390_CC_OP_DFP_64_TO_UINT_32   | D source              | Z rounding mode      |                 |
   | S390_CC_OP_DFP_128_TO_UINT_32  | D source hi 64 bits   | D source low 64 bits | Z rounding mode |
   | S390_CC_OP_DFP_64_TO_UINT_64   | D source              | Z rounding mode      |                 |
   | S390_CC_OP_DFP_128_TO_UINT_64  | D source hi 64 bits   | D source low 64 bits | Z rounding mode |
   | S390_CC_OP_DFP_64_TO_INT_32    | D source              | Z rounding mode      |                 |
   | S390_CC_OP_DFP_128_TO_INT_32   | D source hi 64 bits   | D source low 64 bits | Z rounding mode |
   | S390_CC_OP_DFP_64_TO_INT_64    | D source              | Z rounding mode      |                 |
   | S390_CC_OP_DFP_128_TO_INT_64   | D source hi 64 bits   | D source low 64 bits | Z rounding mode |
   | S390_CC_OP_PFPO_32             | F|D source            | Z GR0 low 32 bits    |                 |
   | S390_CC_OP_PFPO_64             | F|D source            | Z GR0 low 32 bits    |                 |
   | S390_CC_OP_PFPO_128            | F|D source hi 64 bits | F|D src low 64 bits  | Z GR0 low 32 bits |
   +--------------------------------+-----------------------+----------------------+-----------------+
*/

/*------------------------------------------------------------*/
/*--- Condition code helpers.                             ---*/
/*------------------------------------------------------------*/
UInt s390_calculate_cc(ULong cc_op, ULong cc_dep1, ULong cc_dep2,
                       ULong cc_ndep);
UInt s390_calculate_cond(ULong mask, ULong op, ULong dep1, ULong dep2,
                         ULong ndep);

/* Size of special instruction preamble */
#define S390_SPECIAL_OP_PREAMBLE_SIZE 8

/* Size of special instructions */
#define S390_SPECIAL_OP_SIZE 2

/* Last target instruction for the EX helper */
extern ULong last_execute_target;

/*------------------------------------------------------------*/
/*--- Vector helpers.                                      ---*/
/*------------------------------------------------------------*/

/* Vector operatons passed to s390x_dirtyhelper_vec_op(...) helper.
   Please don't change ordering of elements and append new items
   before  S390_VEC_OP_LAST. */
typedef enum {
   S390_VEC_OP_INVALID = 0,
   S390_VEC_OP_VPKS,
   S390_VEC_OP_VPKLS,
   S390_VEC_OP_VCEQ,
   S390_VEC_OP_VTM,
   S390_VEC_OP_VGFM,
   S390_VEC_OP_VGFMA,
   S390_VEC_OP_VMAH,
   S390_VEC_OP_VMALH,
   S390_VEC_OP_VCH,
   S390_VEC_OP_VCHL,
   S390_VEC_OP_VFTCI,
   S390_VEC_OP_VFMIN,
   S390_VEC_OP_VFMAX,
   S390_VEC_OP_VBPERM,
   S390_VEC_OP_VMSL,
   S390_VEC_OP_LAST             // supposed to be the last element in enum
} s390x_vec_op_t;

/* Arguments of s390x_dirtyhelper_vec_op(...) which are packed into one
   ULong variable.
 */
typedef union {
   struct {
      unsigned int op : 8;        // should be an element of s390x_vec_op_t
      unsigned int v1 : 5;        // result of operation
      unsigned int v2 : 5;        // argument one of operation
      unsigned int v3 : 5;        // argument two of operation or
                                  // zero for unary operations

      unsigned int v4 : 5;        // argument two of operation or
                                  // zero for unary and binary operations

      unsigned int m4 : 4;        // field m4 of insn or zero if it's missing
      unsigned int m5 : 4;        // field m5 of insn or zero if it's missing
      unsigned int m6 : 4;        // field m6 of insn or zero if it's missing
      unsigned int i3 : 12;       // field i3 of insn or zero if it's missing
      unsigned int read_only: 1;  // don't write result to Guest State
      unsigned int reserved : 11; // reserved for future
   };
   ULong serialized;
} s390x_vec_op_details_t;

STATIC_ASSERT(sizeof(s390x_vec_op_details_t) == sizeof(ULong));

/* Macro definitions for opcodes that are not generally available.

   The values to be encoded in those fields must be integer values in
   hexadecimal notation without a leading 0x.
   E.g. VRX_VXBD(e7, 1, 0, 3, 0000, 0, 06) is equal to "vl %%v1, 0(%%r3)\n\t"
*/
#define VRX_VXBD(op1, v1, x2, b2, d2, rxb, op2)  \
            ".short 0x" #op1 #v1 #x2 "\n\t .int  0x" #b2 #d2 "0" #rxb #op2 "\n\t"
#define VRR_VVVMM(op1, v1, v2, v3, m5, m4, rxb, op2) \
            ".short 0x" #op1 #v1 #v2 "\n\t .int  0x" #v3 "0" #m5 "0" #m4 #rxb #op2 "\n\t"

#define VL(v1, x2, b2, d2, rxb)                VRX_VXBD(e7, v1, x2, b2, d2, rxb, 06)
#define VST(v1, x2, b2, d2, rxb)               VRX_VXBD(e7, v1, x2, b2, d2, rxb, 0e)
#define VPKS(v1, v2, v3, m4, m5, rxb)          VRR_VVVMM(e7, v1, v2, v3, m5, m4, rxb, 97)
#define VPKLS(v1, v2, v3, m4, m5, rxb)         VRR_VVVMM(e7, v1, v2, v3, m5, m4, rxb, 95)


/*---------------------------------------------------------------*/
/*--- end                                   guest_s390_defs.h ---*/
/*---------------------------------------------------------------*/

#endif /* __VEX_GUEST_S390_DEFS_H */
