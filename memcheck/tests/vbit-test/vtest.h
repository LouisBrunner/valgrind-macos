/* -*- mode: C; c-basic-offset: 3; -*- */

/*
   This file is part of MemCheck, a heavyweight Valgrind tool for
   detecting memory errors.

   Copyright (C) 2012-2017  Florian Krohm

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

#ifndef VTEST_H
#define VTEST_H

/* Main header file for the V-bit tester */

#include <stdint.h>   // uint64_t
#include "libvex.h"   // IROp
#include "vbits.h"    // vbits_t


/* How undefinedness propagates from input to output */

typedef enum {
   // For any undefined input bit, all output bits are defined.
   UNDEF_NONE,

   // For any undefined input bit, all output bits are undefined.
   UNDEF_ALL,

   // For each undefined input bit, the corresponding output bit
   // in the same position is undefined. No other bit is undefined.
   UNDEF_SAME,

   // For each undefined input bit, the corresponding output bit
   // in the same position is undefined. No other bit is undefined.
   // If the corresponding output bit does not exist, the input bit
   // does not cause any output bits to be undefined.
   UNDEF_TRUNC,

   // For each undefined input bit, the corresponding output bit
   // in the same position is undefined. No other bit is undefined.
   // Output bits that do no not have a corresponding input bit are
   // defined.
   UNDEF_ZEXT,

   // For each undefined input bit, the corresponding output bit
   // in the same position is undefined. If the MSB of the input value
   // is undefined, so are all output bits with higher significance
   // than the MSB input bit.
   UNDEF_SEXT,

   // For each undefined input bit, the corresponding output bit
   // and all output bits with higher significance are undefined.
   UNDEF_LEFT,    

   UNDEF_CONCAT,  // nHLto2n ops e.g. Iop_32HLto64
   UNDEF_UPPER,   // 2nHIton ops e.g. Iop_64HIto32
   UNDEF_SHL,     // shift-left
   UNDEF_SHR,     // logical shift-right
   UNDEF_SAR,     // arithmetic shift-right
   UNDEF_OR,      // bitwise OR operation
   UNDEF_AND,     // bitwise AND operation

   UNDEF_ORD,     // Iop_CmpORD compare 

   // Expensive (exact) integer EQ and NE
   UNDEF_CMP_EQ_NE,

   // Expensive (exact) integer addition and subtraction
   UNDEF_INT_ADD,
   UNDEF_INT_SUB,

   /* For each of the following UNDEF_ALL_BxE, E is the number of
    * elements and B is the number of bits in the element.
    *
    * If any bits in one of the E elements is not defined, then the
    * return value has all bits in the corresponding element set to 1.
    */
   UNDEF_ALL_64x2,         // 128-bit vector, two 64-bit elements
   UNDEF_ALL_32x4,         // 128-bit vector, four 32-bit elements
   UNDEF_ALL_16x8,         // 128-bit vector, eight 16-bit elements
   UNDEF_ALL_8x16,         // 128-bit vector, sixteen 8-bit elements

   /* For each of the following UNDEF_ALL_BxE_EVEN, E is the number of
    * elements and B is the number of bits in the element. Elements are
    * numbered from right to left starting with element number 0.
    *
    * If any bits in one of the even numbered elements is not defined, then
    * the return value has all bits in the corresponding element set to 1.
    * The bits in the odd numbered elements are not checked
    */
   UNDEF_ALL_32x4_EVEN,    // 128-bit vector, four 32-bit elements
   UNDEF_ALL_16x8_EVEN,    // 128-bit vector, eight 16-bit elements
   UNDEF_ALL_8x16_EVEN,    // 128-bit vector, sixteen 8-bit elements

   /* For each of the following UNDEF_BxE_TRANSPOSE, E is the number of
    * elements and B is the number of bits in the element.
    *
    * Concatenate bit i from each byte j.  Place concatenated 8 bit value
    * into byte i of the result.  Do for each bit i from 0 to 7 and
    * byte j from 0 to 7 of each 64-bit element.
    */
   UNDEF_64x2_TRANSPOSE,

   /* For each of the following UNDEF_BxE_ROTATE, E is the number of
    * elements and B is the number of bits in the element.
    *
    * The result is the undefined bits in each element rotated by the
    * specified amount.  Bits rotated out of the element are discarded.
    * No additional bits are set to undefined.
    */
   UNDEF_64x2_ROTATE, /* 128-bit vector, two 64-bit elements, rotate
                       * elements left.
                       */
   UNDEF_32x4_ROTATE, /* 128-bit vector, four 32-bit elements, rotate
                       * elements left.
                       */
   UNDEF_16x8_ROTATE, /* 128-bit vector, eight 16-bit elements, rotate
                       * elements left.
                       */
   UNDEF_8x16_ROTATE, /* 128-bit vector, sixteen 8-bit elements, rotate
                       * elements left.
                       */

   /* If the input had some vbits set, the result will have one or more
    * vbits set. Minimal test when the vbit propagation can not be easily
    * calculated.
    */
   UNDEF_SOME,

   /* For UNDEF_NARROW256_AtoB, narrow the elements of size A-bits in
    * the 256-bit source (stored in two 128-bit values) to a 128-bit
    * result with elements of size B-bits.
    *
    * If the source element will fit into the corresponding destination
    * element, then only the undefined bits in the source element are
    * undefined in the corresponding bit position of the destination element.
    *
    * If the source element will not fit into the destination element, then
    * only the lower B undefined bits of the source element will be
    * undefined in the corresponding result element unless the saturate
    * flag is true.  If the saturate flag is true and the element in the
    * source will not fit into the corresponding destination element, then
    * all of the bits in the corresponding destination element are set to one.
    */
   UNDEF_NARROW256_AtoB,

   // For IROps I don't know anything about
   UNDEF_UNKNOWN
} undef_t;


// Everything we want to know about an IROp
typedef struct {
   IROp op;
   const char *name;
   undef_t     undef_kind;
   /* The following two members describe if this operand has immediate
    *  operands. There are a few restrictions:
    *    (1) An operator can have at most one immediate operand.
    *    (2) If there is an immediate operand, it is the right-most operand.
    *  An immediate_index of 0 means there is no immediate operand.
    */
   unsigned    immediate_index;
   unsigned    immediate_type;

   // Indicate whether IROp can be tested on a particular architecture
   unsigned    s390x  : 1;
   unsigned    amd64  : 1;
   unsigned    ppc32  : 1;
   unsigned    ppc64  : 1;
   unsigned    arm    : 1;
   unsigned    arm64  : 1;
   unsigned    x86    : 1;
   unsigned    mips32 : 1;
   unsigned    mips64 : 1;
} irop_t;


/* The maximum number of input operands */
#define MAX_OPERANDS 4

/* An operand of an IROp (also used for the result) */
typedef struct {
   IRType  type;
   vbits_t vbits;
   value_t value;
} opnd_t;


/* Carries the data needed to execute and evaluate a test. I.e.
   inputs and results (V-bits and actual value). */
typedef struct {
   opnd_t result;
   opnd_t opnds[MAX_OPERANDS];
   unsigned rounding_mode;
} test_data_t;


/* Function prototypes */
irop_t *get_irop(IROp);
int  is_floating_point_op_with_rounding_mode(IROp);
int  get_num_operands(IROp);

void print_opnd(FILE *, const opnd_t *);

int test_unary_op(const irop_t *, test_data_t *);
int test_binary_op(const irop_t *, test_data_t *);
int test_ternary_op(const irop_t *, test_data_t *);
int test_qernary_op(const irop_t *, test_data_t *);

void valgrind_vex_init_for_iri(IRICB *);
void valgrind_execute_test(const irop_t *, test_data_t *);

IRICB new_iricb(const irop_t *, test_data_t *);

void panic(const char *) __attribute__((noreturn));
void complain(const irop_t *, const test_data_t *, vbits_t expected);

/* Imported from VEX */
unsigned sizeof_irtype(IRType);
void typeof_primop(IROp, IRType *t_dst, IRType *t_arg1, IRType *t_arg2, 
                   IRType *t_arg3, IRType *t_arg4);

static __inline__ unsigned bitsof_irtype(IRType type)
{
   return type == Ity_I1 ? 1 : sizeof_irtype(type) * 8;
}


/* Exported variables */
extern int verbose;

#endif // VTEST_H
