/* -*- mode: C; c-basic-offset: 3; -*- */

/*
   This file is part of MemCheck, a heavyweight Valgrind tool for
   detecting memory errors.

   Copyright (C) 2012-2017  Florian Krohm

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.
*/

#ifndef VBITS_H
#define VBITS_H

#include <stdint.h>
#include <stdio.h>
#include <stdbool.h>

typedef uint64_t uint128_t[2];
typedef uint64_t uint256_t[4];

/* A type to represent V-bits */
typedef struct {
   unsigned num_bits;
   union {
      uint8_t   u8;
      uint16_t  u16;
      uint32_t  u32;
      uint64_t  u64;
      uint128_t u128;
      uint256_t u256;
   } bits;
} vbits_t;


/* A type large enough to hold any IRType'd value. At this point
   we do not expect to test with specific floating point values.
   So we don't need to represent them.

   NOTE: Values of type Ity_I1 are stored in the u32 variant. This is
   inconsistent with the way such values are stored elsewhere in VEX,
   namely, in an 8-bit container. Why is that?
   The reason is that libvex_ir.h does not provide an Iop_8to1 operator.
   However, that would be needed in ir_inject.c when loading a 1-bit value
   from memory (see function load_aux there). Instead of today's
      return unop(Iop_32to1, IRExpr_Load(endian, Ity_I32, addr));
   we'd like to write
      return unop(Iop_8to1, IRExpr_Load(endian, Ity_I8, addr));
   But cannot do. Grrrrr...
 */
typedef union {
   uint8_t   u8;
   uint16_t  u16;
   uint32_t  u32;
   uint64_t  u64;
   uint128_t u128;
   uint256_t u256;
} value_t;


void    print_vbits(FILE *, vbits_t);
vbits_t undefined_vbits(unsigned num_bits);
vbits_t undefined_vbits_BxE(unsigned int bits, unsigned int elements,
                            vbits_t v);
vbits_t undefined_vbits_BxE_rotate(unsigned int bits, unsigned int elements,
                                   vbits_t vbits,
                                   value_t value);
vbits_t undefined_vbits_128_even_element(unsigned int bits,
                                         unsigned int elements, vbits_t v);
vbits_t undefined_vbits_64x2_transpose(vbits_t v);
vbits_t undefined_vbits_Narrow256_AtoB(unsigned int src_num_bits,
                                       unsigned int result_num_bits,
                                       vbits_t src1_v, value_t src1_value,
                                       vbits_t src2_v, value_t src2_value,
                                       bool sataurate);
vbits_t defined_vbits(unsigned num_bits);
int     equal_vbits(vbits_t, vbits_t);
vbits_t truncate_vbits(vbits_t, unsigned num_bits);
vbits_t left_vbits(vbits_t, unsigned num_bits);
vbits_t or_vbits(vbits_t, vbits_t);
vbits_t and_vbits(vbits_t, vbits_t);
vbits_t concat_vbits(vbits_t, vbits_t);
vbits_t upper_vbits(vbits_t);
vbits_t sextend_vbits(vbits_t, unsigned num_bits);
vbits_t zextend_vbits(vbits_t, unsigned num_bits);
vbits_t onehot_vbits(unsigned bitno, unsigned num_bits);
vbits_t shl_vbits(vbits_t, unsigned amount);
vbits_t shr_vbits(vbits_t, unsigned amount);
vbits_t sar_vbits(vbits_t, unsigned amount);
int     completely_defined_vbits(vbits_t);
vbits_t cmpord_vbits(unsigned v1_num_bits, unsigned v2_num_bits);
vbits_t cmp_eq_ne_vbits(vbits_t vbits1, vbits_t vbits2,
                        value_t val1, value_t val2);
uint64_t min_vbits(uint64_t vbits, uint64_t value);
uint64_t max_vbits(uint64_t vbits, uint64_t value);
vbits_t int_add_or_sub_vbits(int isAdd,
                             vbits_t vbits1, vbits_t vbits2,
                             value_t val1, value_t val2);
vbits_t cmp_gt_vbits(int is_signed, int bits_per_element, int element_count,
                     vbits_t vbits1, vbits_t vbits2, value_t val1, value_t val2);

#endif // VBITS_H
