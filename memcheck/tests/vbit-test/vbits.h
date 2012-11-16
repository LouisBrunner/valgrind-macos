/* -*- mode: C; c-basic-offset: 3; -*- */

#ifndef VBITS_H
#define VBITS_H

#include <stdint.h>
#include <stdio.h>

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


/* A type large enough to hold any IRtype'd value. At this point
   we do not expect to test with specific floating point values.
   So we don't need to represent them. */
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

#endif // VBITS_H
