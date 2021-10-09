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

#include <stdio.h>   // fprintf
#include <assert.h>  // assert
#if defined(__APPLE__)
#include <machine/endian.h>
#define __BYTE_ORDER    BYTE_ORDER
#define __LITTLE_ENDIAN LITTLE_ENDIAN
#elif defined(__sun)
#define __LITTLE_ENDIAN 1234
#define __BIG_ENDIAN    4321
#  if defined(_LITTLE_ENDIAN)
#  define __BYTE_ORDER    __LITTLE_ENDIAN
#  else
#  define __BYTE_ORDER    __BIG_ENDIAN
#  endif
#elif defined(__linux__)
#include <endian.h>
#else
#include <sys/endian.h>
#define __BYTE_ORDER    BYTE_ORDER
#define __LITTLE_ENDIAN LITTLE_ENDIAN
#endif
#include <inttypes.h>
#include "vbits.h"
#include "vtest.h"

#include "memcheck.h"  // VALGRIND_MAKE_MEM_DEFINED


/* Return the bits of V if they fit into 64-bit. If V has fewer than
   64 bits, the bit pattern is zero-extended to the left. */
static uint64_t
get_bits64(vbits_t v)
{
   switch (v.num_bits) {
   case 1:  return v.bits.u32;
   case 8:  return v.bits.u8;
   case 16: return v.bits.u16;
   case 32: return v.bits.u32;
   case 64: return v.bits.u64;
   case 128:
   case 256:
      /* fall through */
   default:
      panic(__func__);
   }
}

void
print_vbits(FILE *fp, vbits_t v)
{
   switch (v.num_bits) {
   case 1:   fprintf(fp, "%08x",   v.bits.u32); break;
   case 8:   fprintf(fp, "%02x",   v.bits.u8);  break;
   case 16:  fprintf(fp, "%04x",   v.bits.u16); break;
   case 32:  fprintf(fp, "%08x",   v.bits.u32); break;
   case 64:  fprintf(fp, "%016"PRIx64, v.bits.u64); break;
   case 128:
      if (__BYTE_ORDER == __LITTLE_ENDIAN) {
         fprintf(fp, "%016"PRIx64, v.bits.u128[1]);
         fprintf(fp, "%016"PRIx64, v.bits.u128[0]);
      } else {
         fprintf(fp, "%016"PRIx64, v.bits.u128[0]);
         fprintf(fp, "%016"PRIx64, v.bits.u128[1]);
      }
      break;
   case 256:
      if (__BYTE_ORDER == __LITTLE_ENDIAN) {
         fprintf(fp, "%016"PRIx64, v.bits.u256[3]);
         fprintf(fp, "%016"PRIx64, v.bits.u256[2]);
         fprintf(fp, "%016"PRIx64, v.bits.u256[1]);
         fprintf(fp, "%016"PRIx64, v.bits.u256[0]);
      } else {
         fprintf(fp, "%016"PRIx64, v.bits.u256[0]);
         fprintf(fp, "%016"PRIx64, v.bits.u256[1]);
         fprintf(fp, "%016"PRIx64, v.bits.u256[2]);
         fprintf(fp, "%016"PRIx64, v.bits.u256[3]);
      }
      break;
   default:
      panic(__func__);
   }
}


/* Return a value where all bits are set to undefined. */
vbits_t
undefined_vbits(unsigned num_bits)
{
   vbits_t new = { .num_bits = num_bits };

   switch (num_bits) {
   case   1: new.bits.u32 = 0x01;   break;
   case   8: new.bits.u8  = 0xff;   break;
   case  16: new.bits.u16 = 0xffff; break;
   case  32: new.bits.u32 = ~0;     break;
   case  64: new.bits.u64 = ~0ull;  break;
   case 128: new.bits.u128[0] = ~0ull;
             new.bits.u128[1] = ~0ull;
             break;
   case 256: new.bits.u256[0] = ~0ull;
             new.bits.u256[1] = ~0ull;
             new.bits.u256[2] = ~0ull;
             new.bits.u256[3] = ~0ull;
             break;
   default:
      panic(__func__);
   }
   return new;
}

/* The following routines named undefined_vbits_BxE() return a 128-bit
 * vector with E elements each of size bits.  If any of the bits in an
 * element is undefined, then return a value where all bits in that
 * element are undefined.
 */
vbits_t
undefined_vbits_BxE(unsigned int bits, unsigned int elements, vbits_t v)
{
   vbits_t new = { .num_bits = v.num_bits };
   uint64_t mask = ~0ull >> (64 - bits);
   int i, j;

   assert ((elements % 2) == 0);
   assert (bits <= 64);

   for (i = 0; i<2; i++) {
      new.bits.u128[i] = 0ull;

      for (j = 0; j<elements/2; j++) {
         if ((v.bits.u128[i] & (mask << (j*bits))) != 0)
            new.bits.u128[i] |= (mask << (j*bits));
      }
   }
   return new;
}

/* The following routines named undefined_vbits_BxE_rotate() return a 128-bit
 * vector with E elements each of size bits.  The bits in v are rotated
 * left by the amounts in the corresponding element of val. Specified rotate
 * amount field is assumed to be at most 8-bits wide.
 */
vbits_t
undefined_vbits_BxE_rotate(unsigned int bits, unsigned int elements,
                           vbits_t v, value_t val)
{
   vbits_t new = { .num_bits = v.num_bits };
   uint64_t mask = ~0ull >> (64 - bits);
   uint64_t const shift_mask = 0xFF;
   uint64_t element;
   int i, j;
   signed char shift;
   assert ((elements % 2) == 0);
   assert (bits <= 64);

   for (i = 0; i<2; i++) {
      new.bits.u128[i] = 0ull;

      for (j = 0; j<elements/2; j++) {
         element = (v.bits.u128[i] >> (j*bits)) & mask;
         shift = (int)((val.u128[i] >> (j*bits)) & shift_mask);

         if (shift < 0) {
            /* right shift */
            new.bits.u128[i] = element >> -shift;

            /* OR in the bits shifted out into the top of the element */
            new.bits.u128[i] |= element << (bits + shift);
         } else {
            /* left shift */
            /* upper bits from shift */
            new.bits.u128[i] = element << shift;

            /* OR in the bits shifted out into the bottom of the element */
            new.bits.u128[i] |= element >> (bits - shift);
         }
      }
   }
   return new;
}

/* Only the even elements of the input are used by the Iop*/
vbits_t
undefined_vbits_128_even_element(unsigned int bits, unsigned int elements,
                                 vbits_t v)
{
   int i;
   uint64_t mask;
   unsigned int const element_width = 128/elements;
   vbits_t new = { .num_bits = v.num_bits };

   assert ((elements % 2) == 0);
   assert (bits <= 64);

   /* Create a 128-bit mask with the bits in the even numbered
    * elements are all ones.
    */
   mask = ~0ull >> (64 - bits);

   for (i = 2; i < elements/2; i=i+2) {
      mask |= mask << (i * element_width);
   }

   new.bits.u128[0] = mask & v.bits.u128[0];
   new.bits.u128[1] = mask & v.bits.u128[1];

   return new;
}

/* Concatenate bit i from each byte j.  Place concatenated 8 bit value into
 * byte i of the result.  Do for all i from 0 to 7 and j from 0 to 7 of each
 * 64-bit element.
 */
vbits_t
undefined_vbits_64x2_transpose(vbits_t v)
{
   vbits_t new = { .num_bits = v.num_bits };
   unsigned int bit, byte, element;
   uint64_t value, new_value, select_bit;

   for (element = 0; element < 2; element++) {
      value = v.bits.u128[element];
      new_value = 0;
      for (byte = 0; byte < 8; byte++) {
         for (bit = 0; bit < 8; bit++) {
            select_bit = 1ULL & (value >> (bit + 8*byte));
            new_value |= select_bit << (bit*8 + byte);
         }
      }
      new.bits.u128[element] = new_value;
   }
   return new;
}

/* The routine takes a 256-bit vector value stored across the two 128-bit
 * source operands src1 and src2.  The size of each element in the input is
 * src_num_bits.  The elements are narrowed to result_num_bits and packed
 * into the result.  If saturate is True, then the all the result bits are
 * set to 1 if the source element can not be represented in result_num_bits.
 */
vbits_t
undefined_vbits_Narrow256_AtoB(unsigned int src_num_bits,
                               unsigned int result_num_bits,
                               vbits_t src1_v, value_t src1_value,
                               vbits_t src2_v, value_t src2_value,
                               bool saturate)
{

   vbits_t new = { .num_bits = src1_v.num_bits };
   unsigned int i;
   uint64_t vbits, new_value;
   uint64_t const src_mask = ~0x0ULL >> (64 - src_num_bits);
   uint64_t const result_mask = ~0x0ULL >> (64 - result_num_bits);
   unsigned int num_elements_per_64_bits = src_num_bits/64;
   unsigned int shift;

   /*
    * NOTE:  POWER PPC
    *   the saturated value is 0xFFFF for the vbit is in one of the lower
    *   32-bits of the source.  The saturated result is 0xFFFF0000 if the
    *   vbit is in the upper 32-bits of the source.  Not sure what
    *   the saturated result is in general for a B-bit result.
    *
    *  ONLY TESTED FOR 64 bit input, 32 bit result
    */
   uint64_t const saturated_result = 0xFFFFULL;

   /* Source elements are split between the two source operands */

   assert(src_num_bits <= 64);
   assert(result_num_bits < 64);
   assert(result_num_bits < src_num_bits);

   /* Narrow the elements from src1 to the upper 64-bits of result.
    * Do each of the 64 bit values that make up a u128
    */
   new_value = 0;
   for (i = 0; i < num_elements_per_64_bits; i++) {
      vbits = src1_v.bits.u128[0] >> (i * src_num_bits);
      vbits &= src_mask;

      shift = result_num_bits * i;
      if (vbits) {
         if (saturate) {
            /* Value will not fit in B-bits, saturate the result as needed. */
            if (vbits >> (src_num_bits/2))
               /* vbit is upper half of the source */
               new_value |= saturated_result << ( shift + result_num_bits/2);
            else
               new_value |= saturated_result << shift;
         } else {
            new_value |= (vbits & result_mask) << shift;
         }
      }
   }

   for (i = 0; i < num_elements_per_64_bits; i++) {
      vbits = src1_v.bits.u128[1] >> (i * src_num_bits);
      vbits &= src_mask;

      shift = result_num_bits * i + (num_elements_per_64_bits
                                     * result_num_bits);
      if (vbits) {
         if (saturate) {
            /* Value will not fit in result_num_bits, saturate the result
             * as needed.
             */
            if (vbits >> (src_num_bits/2))
               /* vbit is upper half of the source */
               new_value |= saturated_result << (shift + result_num_bits/2);

            else
               new_value |= saturated_result << shift;

         } else {
            new_value |= (vbits & result_mask) << shift;
         }
      }
   }
   if (__BYTE_ORDER == __LITTLE_ENDIAN)
      new.bits.u128[1] = new_value;
   else
      /* Big endian, swap the upper and lower 32-bits of new_value */
      new.bits.u128[0] = (new_value << 32) | (new_value >> 32);

   new_value = 0;
   /* Narrow the elements from src2 to the lower 64-bits of result.
    * Do each of the 64 bit values that make up a u128
    */
   for (i = 0; i < num_elements_per_64_bits; i++) {
      vbits =  src2_v.bits.u128[0] >> (i * src_num_bits);
      vbits &= src_mask;

      shift = result_num_bits * i;
      if (vbits) {
         if (saturate) {
            /* Value will not fit in result, saturate the result as needed. */
            if (vbits >> (src_num_bits/2))
               /* vbit is upper half of the source */
               new_value |= saturated_result << (shift + result_num_bits/2);
            else
               new_value |= saturated_result << shift;
         } else {
            new_value |= (vbits & result_mask) << shift;
         }
      }
   }

   for (i = 0; i < num_elements_per_64_bits; i++) {
      vbits = src2_v.bits.u128[1] >> (i * src_num_bits);
      vbits &= src_mask;

      if (vbits) {
         if (saturate) {
            /* Value will not fit in result_num_bits, saturate the result
             * as needed.
             */
            if (vbits >> (src_num_bits/2))
               /* vbit is upper half of the source */
               new_value |= saturated_result << (result_num_bits * i
                                                 + result_num_bits/2
                                                   + (num_elements_per_64_bits
                                                      * result_num_bits));
            else
               new_value |= saturated_result << (result_num_bits * i
                                                   + (num_elements_per_64_bits
                                                      * result_num_bits));

         } else {
            new_value |= (vbits & result_mask) << (result_num_bits * i
                                                   + (num_elements_per_64_bits
                                                      * result_num_bits));
         }
      }
   }
   if (__BYTE_ORDER == __LITTLE_ENDIAN)
      new.bits.u128[0] = new_value;
   else
      /* Big endian, swap the upper and lower 32-bits of new_value */
      new.bits.u128[1] = (new_value << 32) | (new_value >> 32);

   return new;
}

/* Return a value where all bits are set to defined. */
vbits_t
defined_vbits(unsigned num_bits)
{
   vbits_t new = { .num_bits = num_bits };

   switch (num_bits) {
   case   1: new.bits.u32 = 0x0; break;
   case   8: new.bits.u8  = 0x0; break;
   case  16: new.bits.u16 = 0x0; break;
   case  32: new.bits.u32 = 0x0; break;
   case  64: new.bits.u64 = 0x0; break;
   case 128: new.bits.u128[0] = 0x0;
             new.bits.u128[1] = 0x0;
             break;
   case 256: new.bits.u256[0] = 0x0;
             new.bits.u256[1] = 0x0;
             new.bits.u256[2] = 0x0;
             new.bits.u256[3] = 0x0;
             break;
   default:
      panic(__func__);
   }
   return new;
}


/* Return 1, if equal. */
int
equal_vbits(vbits_t v1, vbits_t v2)
{
   assert(v1.num_bits == v2.num_bits);

   switch (v1.num_bits) {
   case 1:   return v1.bits.u32 == v2.bits.u32;
   case 8:   return v1.bits.u8  == v2.bits.u8;
   case 16:  return v1.bits.u16 == v2.bits.u16;
   case 32:  return v1.bits.u32 == v2.bits.u32;
   case 64:  return v1.bits.u64 == v2.bits.u64;
   case 128: return v1.bits.u128[0] == v2.bits.u128[0] &&
                    v1.bits.u128[1] == v2.bits.u128[1];
   case 256: return v1.bits.u256[0] == v2.bits.u256[0] &&
                    v1.bits.u256[1] == v2.bits.u256[1] &&
                    v1.bits.u256[2] == v2.bits.u256[2] &&
                    v1.bits.u256[3] == v2.bits.u256[3];
   default:
      panic(__func__);
   }
}


/* Truncate the bit pattern in V1 to NUM_BITS bits */
vbits_t
truncate_vbits(vbits_t v, unsigned num_bits)
{
   assert(num_bits <= v.num_bits);

   if (num_bits == v.num_bits) return v;

   vbits_t new = { .num_bits = num_bits };

   if (num_bits <= 64) {
      uint64_t bits;

      if (v.num_bits <= 64)
         bits = get_bits64(v);
      else if (v.num_bits == 128)
         if (__BYTE_ORDER == __LITTLE_ENDIAN)
            bits = v.bits.u128[0];
         else
            bits = v.bits.u128[1];
      else if (v.num_bits == 256)
         if (__BYTE_ORDER == __LITTLE_ENDIAN)
            bits = v.bits.u256[0];
         else
            bits = v.bits.u256[3];
      else
         panic(__func__);

      switch (num_bits) {
      case 1:   new.bits.u32 = bits & 0x01;   break;
      case 8:   new.bits.u8  = bits & 0xff;   break;
      case 16:  new.bits.u16 = bits & 0xffff; break;
      case 32:  new.bits.u32 = bits & ~0u;    break;
      case 64:  new.bits.u64 = bits & ~0ll;   break;
      default:
         panic(__func__);
      }
      return new;
   }

   if (num_bits == 128) {
      assert(v.num_bits == 256);
      /* From 256 bits to 128 */
      if (__BYTE_ORDER == __LITTLE_ENDIAN) {
         new.bits.u128[0] = v.bits.u256[0];
         new.bits.u128[1] = v.bits.u256[1];
      } else {
         new.bits.u128[0] = v.bits.u256[2];
         new.bits.u128[1] = v.bits.u256[3];
      }
      return new;
   }

   /* Cannot truncate to 256 bits from something larger */
   panic(__func__);
}


/* Helper function to compute left_vbits */
static uint64_t
left64(uint64_t x)
{
   // left(x) = x | -x
   return x | (~x + 1);
}


vbits_t
left_vbits(vbits_t v, unsigned num_bits)
{
   assert(num_bits >= v.num_bits);

   vbits_t new = { .num_bits = num_bits };

   if (v.num_bits <= 64) {
      uint64_t bits = left64(get_bits64(v));

      switch (num_bits) {
      case 8:   new.bits.u8  = bits & 0xff;   break;
      case 16:  new.bits.u16 = bits & 0xffff; break;
      case 32:  new.bits.u32 = bits & ~0u;    break;
      case 64:  new.bits.u64 = bits & ~0ll;   break;
      case 128:
         if (__BYTE_ORDER == __LITTLE_ENDIAN) {
            new.bits.u128[0] = bits;
            if (bits & (1ull << 63)) {  // MSB is set
               new.bits.u128[1] = ~0ull;
            } else {
               new.bits.u128[1] = 0;
            }
         } else {
            new.bits.u128[1] = bits;
            if (bits & (1ull << 63)) {  // MSB is set
               new.bits.u128[0] = ~0ull;
            } else {
               new.bits.u128[0] = 0;
            }
         }
         break;
      case 256:
         if (__BYTE_ORDER == __LITTLE_ENDIAN) {
            new.bits.u256[0] = bits;
            if (bits & (1ull << 63)) {  // MSB is set
               new.bits.u256[1] = ~0ull;
               new.bits.u256[2] = ~0ull;
               new.bits.u256[3] = ~0ull;
            } else {
               new.bits.u256[1] = 0;
               new.bits.u256[2] = 0;
               new.bits.u256[3] = 0;
            }
         } else {
            new.bits.u256[3] = bits;
            if (bits & (1ull << 63)) {  // MSB is set
               new.bits.u256[0] = ~0ull;
               new.bits.u256[1] = ~0ull;
               new.bits.u256[2] = ~0ull;
            } else {
               new.bits.u256[0] = 0;
               new.bits.u256[1] = 0;
               new.bits.u256[2] = 0;
            }
         }
         break;
      default:
         panic(__func__);
      }
      return new;
   }

   if (v.num_bits == 128) {
      if (__BYTE_ORDER == __LITTLE_ENDIAN) {
         if (v.bits.u128[1] != 0) {
            new.bits.u128[0] = v.bits.u128[0];
            new.bits.u128[1] = left64(v.bits.u128[1]);
         } else {
            new.bits.u128[0] = left64(v.bits.u128[0]);
            if (new.bits.u128[0] & (1ull << 63)) {  // MSB is set
               new.bits.u128[1] = ~0ull;
            } else {
               new.bits.u128[1] = 0;
            }
         }
      } else {
         if (v.bits.u128[0] != 0) {
            new.bits.u128[0] = left64(v.bits.u128[0]);
            new.bits.u128[1] = v.bits.u128[1];
         } else {
            new.bits.u128[1] = left64(v.bits.u128[1]);
            if (new.bits.u128[1] & (1ull << 63)) {  // MSB is set
               new.bits.u128[0] = ~0ull;
            } else {
               new.bits.u128[0] = 0;
            }
         }
      }
      if (num_bits == 128) return new;

      assert(num_bits == 256);

      if (__BYTE_ORDER == __LITTLE_ENDIAN) {
         uint64_t b1 = new.bits.u128[1];
         uint64_t b0 = new.bits.u128[0];

         new.bits.u256[0] = b0;
         new.bits.u256[1] = b1;

         if (new.bits.u256[1] & (1ull << 63)) {  // MSB is set
            new.bits.u256[2] = ~0ull;
            new.bits.u256[3] = ~0ull;
         } else {
            new.bits.u256[2] = 0;
            new.bits.u256[3] = 0;
         }
      } else {
         uint64_t b1 = new.bits.u128[0];
         uint64_t b0 = new.bits.u128[1];

         new.bits.u256[2] = b0;
         new.bits.u256[3] = b1;

         if (new.bits.u256[2] & (1ull << 63)) {  // MSB is set
            new.bits.u256[0] = ~0ull;
            new.bits.u256[1] = ~0ull;
         } else {
            new.bits.u256[0] = 0;
            new.bits.u256[1] = 0;
         }
      }
      return new;
   }

   panic(__func__);
}


vbits_t
or_vbits(vbits_t v1, vbits_t v2)
{
   assert(v1.num_bits == v2.num_bits);

   vbits_t new = { .num_bits = v1.num_bits };

   switch (v1.num_bits) {
   case 1:   new.bits.u32 = (v1.bits.u32 | v2.bits.u32) & 1; break;
   case 8:   new.bits.u8  = v1.bits.u8  | v2.bits.u8;  break;
   case 16:  new.bits.u16 = v1.bits.u16 | v2.bits.u16; break;
   case 32:  new.bits.u32 = v1.bits.u32 | v2.bits.u32; break;
   case 64:  new.bits.u64 = v1.bits.u64 | v2.bits.u64; break;
   case 128: new.bits.u128[0] = v1.bits.u128[0] | v2.bits.u128[0];
             new.bits.u128[1] = v1.bits.u128[1] | v2.bits.u128[1];
             break;
   case 256: new.bits.u256[0] = v1.bits.u256[0] | v2.bits.u256[0];
             new.bits.u256[1] = v1.bits.u256[1] | v2.bits.u256[1];
             new.bits.u256[2] = v1.bits.u256[2] | v2.bits.u256[2];
             new.bits.u256[3] = v1.bits.u256[3] | v2.bits.u256[3];
             break;
   default:
      panic(__func__);
   }

   return new;
}


vbits_t
and_vbits(vbits_t v1, vbits_t v2)
{
   assert(v1.num_bits == v2.num_bits);

   vbits_t new = { .num_bits = v1.num_bits };

   switch (v1.num_bits) {
   case 1:   new.bits.u32 = (v1.bits.u32 & v2.bits.u32) & 1; break;
   case 8:   new.bits.u8  = v1.bits.u8  & v2.bits.u8;  break;
   case 16:  new.bits.u16 = v1.bits.u16 & v2.bits.u16; break;
   case 32:  new.bits.u32 = v1.bits.u32 & v2.bits.u32; break;
   case 64:  new.bits.u64 = v1.bits.u64 & v2.bits.u64; break;
   case 128: new.bits.u128[0] = v1.bits.u128[0] & v2.bits.u128[0];
             new.bits.u128[1] = v1.bits.u128[1] & v2.bits.u128[1];
             break;
   case 256: new.bits.u256[0] = v1.bits.u256[0] & v2.bits.u256[0];
             new.bits.u256[1] = v1.bits.u256[1] & v2.bits.u256[1];
             new.bits.u256[2] = v1.bits.u256[2] & v2.bits.u256[2];
             new.bits.u256[3] = v1.bits.u256[3] & v2.bits.u256[3];
             break;
   default:
      panic(__func__);
   }

   return new;
}


vbits_t
concat_vbits(vbits_t v1, vbits_t v2)
{
   assert(v1.num_bits == v2.num_bits);

   vbits_t new = { .num_bits = v1.num_bits * 2 };

   switch (v1.num_bits) {
   case 8:   new.bits.u16 = v1.bits.u8;
             new.bits.u16 = (new.bits.u16 << 8)  | v2.bits.u8;  break;
   case 16:  new.bits.u32 = v1.bits.u16;
             new.bits.u32 = (new.bits.u32 << 16) | v2.bits.u16; break;
   case 32:  new.bits.u64 = v1.bits.u32;
             new.bits.u64 = (new.bits.u64 << 32) | v2.bits.u32; break;
   case 64:
      if (__BYTE_ORDER == __LITTLE_ENDIAN) {
         new.bits.u128[0] = v2.bits.u64;
         new.bits.u128[1] = v1.bits.u64;
      } else {
         new.bits.u128[0] = v1.bits.u64;
         new.bits.u128[1] = v2.bits.u64;
      }
      break;
   case 128:
      if (__BYTE_ORDER == __LITTLE_ENDIAN) {
         new.bits.u256[0] = v2.bits.u128[0];
         new.bits.u256[1] = v2.bits.u128[1];
         new.bits.u256[2] = v1.bits.u128[0];
         new.bits.u256[3] = v1.bits.u128[1];
      } else {
         new.bits.u256[0] = v1.bits.u128[0];
         new.bits.u256[1] = v1.bits.u128[1];
         new.bits.u256[2] = v2.bits.u128[0];
         new.bits.u256[3] = v2.bits.u128[1];
      }
      break;
   case 256: /* Fall through */
   default:
      panic(__func__);
   }

   return new;
}


vbits_t
upper_vbits(vbits_t v)
{
   vbits_t new = { .num_bits = v.num_bits / 2 };

   switch (v.num_bits) {
   case 16:  new.bits.u8  = v.bits.u16 >> 8;  break;
   case 32:  new.bits.u16 = v.bits.u32 >> 16; break;
   case 64:  new.bits.u32 = v.bits.u64 >> 32; break;
   case 128: 
      if (__BYTE_ORDER == __LITTLE_ENDIAN)
         new.bits.u64 = v.bits.u128[1];
      else
         new.bits.u64 = v.bits.u128[0];
      break;
   case 256:
      if (__BYTE_ORDER == __LITTLE_ENDIAN) {
         new.bits.u128[0] = v.bits.u256[2];
         new.bits.u128[1] = v.bits.u256[3];
      } else {
         new.bits.u128[0] = v.bits.u256[0];
         new.bits.u128[1] = v.bits.u256[1];
      }
      break;
   case 8:
   default:
      panic(__func__);
   }

   return new;
}


vbits_t
zextend_vbits(vbits_t v, unsigned num_bits)
{
   assert(num_bits >= v.num_bits);

   if (num_bits == v.num_bits) return v;

   vbits_t new = { .num_bits = num_bits };

   if (v.num_bits <= 64) {
      uint64_t bits = get_bits64(v);

      switch (num_bits) {
      case 8:   new.bits.u8  = bits; break;
      case 16:  new.bits.u16 = bits; break;
      case 32:  new.bits.u32 = bits; break;
      case 64:  new.bits.u64 = bits; break;
      case 128:
         if (__BYTE_ORDER == __LITTLE_ENDIAN) {
            new.bits.u128[0] = bits;
            new.bits.u128[1] = 0;
         } else {
            new.bits.u128[0] = 0;
            new.bits.u128[1] = bits;
         }
         break;
      case 256:
         if (__BYTE_ORDER == __LITTLE_ENDIAN) {
            new.bits.u256[0] = bits;
            new.bits.u256[1] = 0;
            new.bits.u256[2] = 0;
            new.bits.u256[3] = 0;
         } else {
            new.bits.u256[0] = 0;
            new.bits.u256[1] = 0;
            new.bits.u256[2] = 0;
            new.bits.u256[3] = bits;
         }
         break;
      default:
         panic(__func__);
      }
      return new;
   }

   if (v.num_bits == 128) {
      assert(num_bits == 256);

      if (__BYTE_ORDER == __LITTLE_ENDIAN) {
         new.bits.u256[0] = v.bits.u128[0];
         new.bits.u256[1] = v.bits.u128[1];
         new.bits.u256[2] = 0;
         new.bits.u256[3] = 0;
      } else {
         new.bits.u256[0] = 0;
         new.bits.u256[1] = 0;
         new.bits.u256[2] = v.bits.u128[1];
         new.bits.u256[3] = v.bits.u128[0];
      }
      return new;
   }

   /* Cannot zero-extend a 256-bit value to something larger */
   panic(__func__);
}


vbits_t
sextend_vbits(vbits_t v, unsigned num_bits)
{
   assert(num_bits >= v.num_bits);

   int sextend = 0;

   switch (v.num_bits) {
   case 8:   if (v.bits.u8  == 0x80)             sextend = 1; break;
   case 16:  if (v.bits.u16 == 0x8000)           sextend = 1; break;
   case 32:  if (v.bits.u32 == 0x80000000)       sextend = 1; break;
   case 64:  if (v.bits.u64 == (1ull << 63))     sextend = 1; break;
   case 128: if (v.bits.u128[1] == (1ull << 63)) sextend = 1; break;
   case 256: if (v.bits.u256[3] == (1ull << 63)) sextend = 1; break;

   default:
      panic(__func__);
   }

   return sextend ? left_vbits(v, num_bits) : zextend_vbits(v, num_bits);
}


vbits_t
onehot_vbits(unsigned bitno, unsigned num_bits)
{
   assert(bitno < num_bits);

   vbits_t new = { .num_bits = num_bits };

   switch (num_bits) {
   case 1:   new.bits.u32 = 1    << bitno; break;
   case 8:   new.bits.u8  = 1    << bitno; break;
   case 16:  new.bits.u16 = 1    << bitno; break;
   case 32:  new.bits.u32 = 1u   << bitno; break;
   case 64:  new.bits.u64 = 1ull << bitno; break;
   case 128:
      if (__BYTE_ORDER == __LITTLE_ENDIAN) {
         if (bitno < 64) {
            new.bits.u128[0] = 1ull << bitno;
            new.bits.u128[1] = 0;
         } else {
            new.bits.u128[0] = 0;
            new.bits.u128[1] = 1ull << (bitno - 64);
         }
      } else {
         if (bitno < 64) {
            new.bits.u128[0] = 0;
            new.bits.u128[1] = 1ull << bitno;
         } else {
            new.bits.u128[0] = 1ull << (bitno - 64);
            new.bits.u128[1] = 0;
         }
      }
      break;
   case 256:
      if (__BYTE_ORDER == __LITTLE_ENDIAN) {
         if (bitno < 64) {
            new.bits.u256[0] = 1ull << bitno;
            new.bits.u256[1] = 0;
            new.bits.u256[2] = 0;
            new.bits.u256[3] = 0;
         } else if (bitno < 128) {
            new.bits.u256[0] = 0;
            new.bits.u256[1] = 1ull << (bitno - 64);
            new.bits.u256[2] = 0;
            new.bits.u256[3] = 0;
         } else if (bitno < 192) {
            new.bits.u256[0] = 0;
            new.bits.u256[1] = 0;
            new.bits.u256[2] = 1ull << (bitno - 128);
            new.bits.u256[3] = 0;
         } else {
            new.bits.u256[0] = 0;
            new.bits.u256[1] = 0;
            new.bits.u256[2] = 0;
            new.bits.u256[3] = 1ull << (bitno - 192);
         }
      } else {
         if (bitno < 64) {
            new.bits.u256[0] = 0;
            new.bits.u256[1] = 0;
            new.bits.u256[2] = 0;
            new.bits.u256[3] = 1ull << bitno;
         } else if (bitno < 128) {
            new.bits.u256[0] = 0;
            new.bits.u256[1] = 0;
            new.bits.u256[2] = 1ull << (bitno - 64);
            new.bits.u256[3] = 0;
         } else if (bitno < 192) {
            new.bits.u256[0] = 0;
            new.bits.u256[1] = 1ull << (bitno - 128);
            new.bits.u256[2] = 0;
            new.bits.u256[3] = 0;
         } else {
            new.bits.u256[0] = 1ull << (bitno - 192);
            new.bits.u256[1] = 0;
            new.bits.u256[2] = 0;
            new.bits.u256[3] = 0;
         }
      }
      break;
   default:
      panic(__func__);
   }
   return new;
}


int
completely_defined_vbits(vbits_t v)
{
   return equal_vbits(v, defined_vbits(v.num_bits));
}


vbits_t
shl_vbits(vbits_t v, unsigned shift_amount)
{
   assert(shift_amount < v.num_bits);

   vbits_t new = v;

   switch (v.num_bits) {
   case 8:  new.bits.u8  <<= shift_amount; break;
   case 16: new.bits.u16 <<= shift_amount; break;
   case 32: new.bits.u32 <<= shift_amount; break;
   case 64: new.bits.u64 <<= shift_amount; break;
   case 128: /* fall through */
   case 256: /* fall through */
   default:
      panic(__func__);
   }
   
   return new;
}


vbits_t
shr_vbits(vbits_t v, unsigned shift_amount)
{
   assert(shift_amount < v.num_bits);

   vbits_t new = v;

   switch (v.num_bits) {
   case 8:  new.bits.u8  >>= shift_amount; break;
   case 16: new.bits.u16 >>= shift_amount; break;
   case 32: new.bits.u32 >>= shift_amount; break;
   case 64: new.bits.u64 >>= shift_amount; break;
   case 128: /* fall through */
   case 256: /* fall through */
   default:
      panic(__func__);
   }
   
   return new;
}


vbits_t
sar_vbits(vbits_t v, unsigned shift_amount)
{
   assert(shift_amount < v.num_bits);

   vbits_t new = v;
   int msb;

   switch (v.num_bits) {
   case 8: 
      new.bits.u8  >>= shift_amount;
      msb = (v.bits.u8 & 0x80) != 0;
      break;
   case 16:
      new.bits.u16 >>= shift_amount;
      msb = (v.bits.u16 & 0x8000) != 0;
      break;
   case 32:
      new.bits.u32 >>= shift_amount;
      msb = (v.bits.u32 & (1u << 31)) != 0;
      break;
   case 64:
      new.bits.u64 >>= shift_amount;
      msb = (v.bits.u64 & (1ull << 63)) != 0;
      break;
   case 128: /* fall through */
   case 256: /* fall through */
   default:
      panic(__func__);
   }

   if (msb)
      new = left_vbits(new, new.num_bits);
   return new;
}

/* Return a value for the POWER Iop_CmpORD class iops */
vbits_t
cmpord_vbits(unsigned v1_num_bits, unsigned v2_num_bits)
{
   vbits_t new = { .num_bits = v1_num_bits };

   /* Size of values being compared must be the same */
   assert( v1_num_bits == v2_num_bits);

   /* Comparison only produces 32-bit or 64-bit value where
    * the lower 3 bits are set to indicate, less than, equal and greater than.
    */
   switch (v1_num_bits) {
   case 32:
      new.bits.u32 = 0xE;
      break;

   case 64:
      new.bits.u64 = 0xE;
      break;

   default:
      panic(__func__);
   }

   return new;
}


/* Deal with precise integer EQ and NE.  Needs some helpers.  The helpers
   compute the result for 64-bit inputs, but can also be used for the
   32/16/8 bit cases, because we can zero extend both the vbits and values
   out to 64 bits and still get the correct result. */


/* Get both vbits and values for a binary operation, that has args of the
   same size (type?), namely 8, 16, 32 or 64 bit.  Unused bits are set to
   zero in both vbit_ and val_ cases. */
static
void get_binary_vbits_and_vals64 ( /*OUT*/uint64_t* varg1,
                                   /*OUT*/uint64_t* arg1,
                                   /*OUT*/uint64_t* varg2,
                                   /*OUT*/uint64_t* arg2,
                                   vbits_t vbits1, vbits_t vbits2,
                                   value_t val1, value_t val2)
{
   assert(vbits1.num_bits == vbits2.num_bits);

   *varg1 = *arg1 = *varg2 = *arg2 = 0;

   switch (vbits1.num_bits) {
      case 8:  *arg1 = (uint64_t)val1.u8;  *arg2 = (uint64_t)val2.u8;  break;
      case 16: *arg1 = (uint64_t)val1.u16; *arg2 = (uint64_t)val2.u16; break;
      case 32: *arg1 = (uint64_t)val1.u32; *arg2 = (uint64_t)val2.u32; break;
      case 64: *arg1 = val1.u64;           *arg2 = val2.u64;           break;
      default: panic(__func__);
   }

   *varg1 = get_bits64(vbits1);
   *varg2 = get_bits64(vbits2);
}

static uint64_t uifu64 ( uint64_t x, uint64_t y ) { return x | y; }

/* Returns 0 (defined) or 1 (undefined). */
static uint32_t ref_CmpEQ64_with_vbits ( uint64_t arg1, uint64_t varg1,
                                         uint64_t arg2, uint64_t varg2 )
{
   uint64_t naive = uifu64(varg1, varg2);
   if (naive == 0) {
      return 0; /* defined */
   }

   // Mark the two actual arguments as fully defined, else Memcheck will
   // complain about undefinedness in them, which is correct but confusing
   // (and pollutes the output of this test program.)
   VALGRIND_MAKE_MEM_DEFINED(&arg1, sizeof(arg1));
   VALGRIND_MAKE_MEM_DEFINED(&arg2, sizeof(arg2));

   // if any bit in naive is 1, then the result is undefined.  Except,
   // if we can find two corresponding bits in arg1 and arg2 such that they
   // are different but both defined, then the overall result is defined
   // (because the two bits guarantee that the bit vectors arg1 and arg2
   // are different.)
   UInt i;
   for (i = 0; i < 64; i++) {
      if ((arg1 & 1) != (arg2 & 1) && (varg1 & 1) == 0 && (varg2 & 1) == 0) {
         return 0; /* defined */
      }
      arg1 >>= 1; arg2 >>= 1; varg1 >>= 1; varg2 >>= 1;
   }

   return 1; /* undefined */
}


vbits_t
cmp_eq_ne_vbits(vbits_t vbits1, vbits_t vbits2, value_t val1, value_t val2)
{
   uint64_t varg1 = 0, arg1 = 0, varg2 = 0, arg2 = 0;
   get_binary_vbits_and_vals64(&varg1, &arg1, &varg2, &arg2,
                               vbits1, vbits2, val1, val2);

   vbits_t res = { .num_bits = 1 };
   res.bits.u32 = ref_CmpEQ64_with_vbits(arg1, varg1, arg2, varg2);

   return res;
}


/* Deal with precise integer ADD and SUB. */
vbits_t
int_add_or_sub_vbits(int isAdd,
                     vbits_t vbits1, vbits_t vbits2, value_t val1, value_t val2)
{
   uint64_t vaa = 0, aa = 0, vbb = 0, bb = 0;
   get_binary_vbits_and_vals64(&vaa, &aa, &vbb, &bb,
                               vbits1, vbits2, val1, val2);

   // This is derived from expensiveAddSub() in mc_translate.c.
   uint64_t a_min = aa & ~vaa;
   uint64_t b_min = bb & ~vbb;
   uint64_t a_max = aa | vaa;
   uint64_t b_max = bb | vbb;

   uint64_t result;
   if (isAdd) {
      result = (vaa | vbb) | ((a_min + b_min) ^ (a_max + b_max));
   } else {
      result = (vaa | vbb) | ((a_min - b_max) ^ (a_max - b_min));
   }

   vbits_t res = { .num_bits = vbits1.num_bits };
   switch (res.num_bits) {
      case 8:  res.bits.u8  = (uint8_t)result;  break;
      case 16: res.bits.u16 = (uint16_t)result; break;
      case 32: res.bits.u32 = (uint32_t)result; break;
      case 64: res.bits.u64 = (uint64_t)result; break;
      default: panic(__func__);
   }

   return res;
}
