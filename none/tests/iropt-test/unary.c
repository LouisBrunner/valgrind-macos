/* -*- mode: C; c-basic-offset: 3; -*- */

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2025  Florian Krohm

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

#include <stdio.h>      // printf
#include <stdlib.h>     // rand
#include <stdint.h>     // UINT64_MAX
#include "vtest.h"

static void run_selected_tests(const irop_t *, test_data_t *);
static void run_random_tests(const irop_t *, test_data_t *);
static uint64_t get_expected_value(const irop_t *, const test_data_t *);
static uint64_t left(uint64_t, unsigned);
static uint32_t popcount(uint64_t);
static uint32_t clz(uint64_t, unsigned);
static uint32_t ctz(uint64_t, unsigned);


void
test_unary_op(const irop_t *op, test_data_t *data)
{
   opnd_t *opnd = &data->opnds[0];

   switch (opnd->type) {
   case Ity_I1:
   case Ity_I8: {
      /* Exhaustive */
      unsigned max = (1 << bitsof_irtype(opnd->type)) - 1;
      for (unsigned i = 0; i <= max; ++i) {
         opnd->value = i;

         valgrind_execute_test(op, data, get_expected_value(op, data));
      }
      break;
   }

   case Ity_I16:
   case Ity_I32:
   case Ity_I64:
      run_selected_tests(op, data);
      run_random_tests(op, data);
      break;

   default:
      panic(__func__);
   }
}


static void
run_selected_tests(const irop_t *op, test_data_t *data)
{
   opnd_t *opnd = &data->opnds[0];
   unsigned num_val;
   const uint64_t *values = get_selected_values(opnd->type, &num_val);

   for (unsigned i = 0; i < num_val; ++i) {
      opnd->value = values[i];

      valgrind_execute_test(op, data, get_expected_value(op, data));
   }
}


/* Test with pseudo-random numbers */
static void
run_random_tests(const irop_t *op, test_data_t *data)
{
   opnd_t *opnd = &data->opnds[0];

   for (unsigned i = 0; i < num_random_tests; ++i) {
      opnd->value = get_random_value(opnd->type);

      valgrind_execute_test(op, data, get_expected_value(op, data));
   }
}


/* Compute the expected result of a unary operation. */
static uint64_t
get_expected_value(const irop_t *op, const test_data_t *data)
{
   uint64_t opnd   = data->opnds[0].value;
   uint64_t expected;

   switch (op->op) {
   case Iop_Not1:   expected = ~opnd & 0x1;        break;
   case Iop_Not8:   expected = ~opnd & UINT8_MAX;  break;
   case Iop_Not16:  expected = ~opnd & UINT16_MAX; break;
   case Iop_Not32:  expected = ~opnd & UINT32_MAX; break;
   case Iop_Not64:  expected = ~opnd & UINT64_MAX; break;

   case Iop_1Uto8:  expected = opnd; break;
// case Iop_1Uto16: expected = opnd; break;
   case Iop_1Uto32: expected = opnd; break;
   case Iop_1Uto64: expected = opnd; break;

   case Iop_1Sto8:
      expected = sign_extend(opnd, 1) & UINT8_MAX;
      break;
   case Iop_1Sto16:
      expected = sign_extend(opnd, 1) & UINT16_MAX;
      break;
   case Iop_1Sto32:
      expected = sign_extend(opnd, 1) & UINT32_MAX;
      break;
   case Iop_1Sto64:
      expected = sign_extend(opnd, 1) & UINT64_MAX;
      break;

   case Iop_8Uto16: expected = opnd; break;
   case Iop_8Uto32: expected = opnd; break;
   case Iop_8Uto64: expected = opnd; break;

   case Iop_8Sto16:
      expected = sign_extend(opnd, 8) & UINT16_MAX;
      break;
   case Iop_8Sto32:
      expected = sign_extend(opnd, 8) & UINT32_MAX;
      break;
   case Iop_8Sto64:
      expected = sign_extend(opnd, 8) & UINT64_MAX;
      break;

   case Iop_16Uto32: expected = opnd; break;
   case Iop_16Uto64: expected = opnd; break;

   case Iop_16Sto32:
      expected = sign_extend(opnd, 16) & UINT32_MAX;
      break;
   case Iop_16Sto64:
      expected = sign_extend(opnd, 16) & UINT64_MAX;
      break;

   case Iop_32Uto64: expected = opnd; break;

   case Iop_32Sto64:
      expected = sign_extend(opnd, 32) & UINT64_MAX;
      break;

// case Iop_8to1:    expected = opnd & 0x1;       break;
// case Iop_16to1:   expected = opnd & 0x1;       break;
   case Iop_16to8:   expected = opnd & UINT8_MAX; break;
   case Iop_16HIto8: expected = opnd >> 8;        break;

   case Iop_32to1:    expected = opnd & 0x1;        break;
   case Iop_32to8:    expected = opnd & UINT8_MAX;  break;
   case Iop_32to16:   expected = opnd & UINT16_MAX; break;
   case Iop_32HIto16: expected = opnd >> 16;        break;

   case Iop_64to1:    expected = opnd & 0x1;        break;
   case Iop_64to8:    expected = opnd & UINT8_MAX;  break;
   case Iop_64to16:   expected = opnd & UINT16_MAX; break;
   case Iop_64to32:   expected = opnd & UINT32_MAX; break;
   case Iop_64HIto32: expected = opnd >> 32;        break;

   case Iop_CmpNEZ8:
   case Iop_CmpNEZ16:
   case Iop_CmpNEZ32:
   case Iop_CmpNEZ64:
      expected = opnd != 0;
      break;

   case Iop_CmpwNEZ32: expected = opnd == 0 ? 0 : UINT32_MAX; break;
   case Iop_CmpwNEZ64: expected = opnd == 0 ? 0 : UINT64_MAX; break;

   case Iop_Left8:  expected = left(opnd, 8);  break;
   case Iop_Left16: expected = left(opnd, 16); break;
   case Iop_Left32: expected = left(opnd, 32); break;
   case Iop_Left64: expected = left(opnd, 64); break;

   case Iop_PopCount32:
   case Iop_PopCount64:
      expected = popcount(opnd);
      break;

   case Iop_ClzNat32: expected = clz(opnd, 32); break;
   case Iop_ClzNat64: expected = clz(opnd, 64); break;

   case Iop_CtzNat32: expected = ctz(opnd, 32); break;
   case Iop_CtzNat64: expected = ctz(opnd, 64); break;

   default:
      panic("%s: operator %s not handled\n", __func__, op->name);
   }

   return expected;
}


/* An implementation for Iop_Left/8/16/32/64.
   The semantics of those operators are defined in Section 2.5 of
   https://valgrind.org/docs/memcheck2005.pdf as follows:

   Iop_Left(v) is the same as v, except that all bits to the left of the
   rightmost 1-bit in v are set. */
static uint64_t
left(uint64_t val, unsigned width)
{
  uint64_t ret = 0;

  /* Find the rightmost 1-bit, then sign-extend. */
  for (unsigned bit = 0; bit < width; ++bit) {
    if (val & ((uint64_t)1 << bit)) {
      ret = (int64_t)((uint64_t)val << (63 - bit)) >> (63 - bit);
      break;
    }
  }

  /* Truncate to desired width */
  switch (width) {
  case 8:  return ret & UINT8_MAX;
  case 16: return ret & UINT16_MAX;
  case 32: return ret & UINT32_MAX;
  case 64: return ret & UINT64_MAX;
  default:
     panic(__func__);
  }
}


/* Naive implementation of counting 1-bits */
static uint32_t
popcount(uint64_t value)
{
   uint32_t count;

   for (count = 0; value != 0; value >>= 1) {
      count += value & 1;
   }
   return count;
}


static uint32_t
clz(uint64_t value, unsigned num_bits)
{
   unsigned last_seen_1bit = 0;

   for (int i = 1; i <= num_bits; ++i) {
      if (value & 0x1)
         last_seen_1bit = i;
      value >>= 1;
   }
   return num_bits - last_seen_1bit;
}


static uint32_t
ctz(uint64_t value, unsigned num_bits )
{
  unsigned count = 0;
  unsigned num_nibbles = num_bits / 4;

  for (unsigned i = 0; i < num_nibbles; ++i) {
    UInt nibble = value & 0xF;
    if ((nibble & 0x1) == 0x1) return count;
    if ((nibble & 0x2) == 0x2) return count + 1;
    if ((nibble & 0x4) == 0x4) return count + 2;
    if ((nibble & 0x8) == 0x8) return count + 3;
    count  += 4;
    value >>= 4;
  }
  return count;
}
