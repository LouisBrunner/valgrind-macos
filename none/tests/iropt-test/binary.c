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
#include <stdint.h>     // UINT64_MAX
#include "vtest.h"

static uint64_t get_expected_value(const irop_t *, const test_data_t *);
static void run_tests(const irop_t *, test_data_t *);
static void run_shift_tests(const irop_t *, test_data_t *);
static int  is_shift_op(IROp);
static int  ok_to_run(IROp op, uint64_t, uint64_t);


void
test_binary_op(const irop_t *op, test_data_t *data)
{
   if (is_shift_op(op->op))
      run_shift_tests(op, data);
   else
      run_tests(op, data);
}


static void
run_selected_tests(const irop_t *op, test_data_t *data)
{
   opnd_t *opnd_l = &data->opnds[0];
   opnd_t *opnd_r = &data->opnds[1];
   unsigned num_val_l, num_val_r;
   const uint64_t *values_l = get_selected_values(opnd_l->type, &num_val_l);
   const uint64_t *values_r = get_selected_values(opnd_r->type, &num_val_r);

   for (unsigned i = 0; i < num_val_l; ++i) {
      opnd_l->value = values_l[i];
      for (unsigned j = 0; j < num_val_r; ++j) {
         opnd_r->value = values_r[j];

         if (ok_to_run(op->op, opnd_l->value, opnd_r->value))
            valgrind_execute_test(op, data, get_expected_value(op, data));
      }
   }
}


/* Test with pseudo-random numbers */
static void
run_random_tests(const irop_t *op, test_data_t *data)
{
   opnd_t *opnd_l = &data->opnds[0];
   opnd_t *opnd_r = &data->opnds[1];

   /* 1-bit wide operands are tested exhaustively. Skip random tests. */
   if (opnd_l->type == Ity_I1 && opnd_r->type == Ity_I1) return;

   unsigned num_tests = 0;
   while (num_tests < num_random_tests) {
      opnd_l->value = get_random_value(opnd_l->type);
      opnd_r->value = get_random_value(opnd_r->type);

      if (ok_to_run(op->op, opnd_l->value, opnd_r->value)) {
         valgrind_execute_test(op, data, get_expected_value(op, data));
         ++num_tests;
      }
   }
}


/* OP is a shift operator. */
static void
run_shift_tests(const irop_t *op, test_data_t *data)
{
   opnd_t *opnd_l = &data->opnds[0];
   opnd_t *opnd_r = &data->opnds[1];
   unsigned num_shiftee;
   const uint64_t *shiftee = get_selected_values(opnd_l->type, &num_shiftee);
   unsigned  max_shift_amount = bitsof_irtype(opnd_r->type) - 1;

   /* Shift selected values with all possible shift amounts */
   for (unsigned i = 0; i < num_shiftee; ++i) {
      opnd_l->value = shiftee[i];
      for (unsigned j = 0; j < max_shift_amount; ++j) {
         opnd_r->value = j;

         valgrind_execute_test(op, data, get_expected_value(op, data));
      }
   }

   /* Shift random values with random shift amounts */
   for (unsigned i = 0; i < num_random_tests; ++i) {
      opnd_l->value = get_random_value(opnd_l->type);
      opnd_r->value = get_random_value(opnd_r->type) & max_shift_amount;

      valgrind_execute_test(op, data, get_expected_value(op, data));
   }
}


static void
run_tests(const irop_t *op, test_data_t *data)
{
   run_selected_tests(op, data);
   run_random_tests(op, data);
}


/* Compute the expected result of a binary operation. */
static uint64_t
get_expected_value(const irop_t *op, const test_data_t *data)
{
   uint64_t opnd_l = data->opnds[0].value;
   uint64_t opnd_r = data->opnds[1].value;
   uint64_t expected;

   switch (op->op) {
   case Iop_Add8:
   case Iop_Add16:
   case Iop_Add32:
   case Iop_Add64:
      expected = opnd_l + opnd_r;
      break;

   case Iop_Sub8:
   case Iop_Sub16:
   case Iop_Sub32:
   case Iop_Sub64:
      expected = opnd_l - opnd_r;
      break;

   case Iop_Mul8:
   case Iop_MullU8: {
      uint8_t u8l = opnd_l;
      uint8_t u8r = opnd_r;
      expected = u8l * u8r;
      break;
   }
   case Iop_Mul16:
   case Iop_MullU16: {
      uint16_t u16l = opnd_l;
      uint16_t u16r = opnd_r;
      expected = u16l * u16r;
      break;
   }
   case Iop_Mul32:
   case Iop_MullU32: {
      uint32_t u32l = opnd_l;
      uint32_t u32r = opnd_r;
      expected = (uint64_t)u32l * (uint64_t)u32r;
      break;
   }

   case Iop_Mul64:
      expected = opnd_l * opnd_r;
      break;

   case Iop_MullS8: {
      uint8_t u8l = opnd_l;
      uint8_t u8r = opnd_r;
      int8_t  s8l = (int8_t)u8l;
      int8_t  s8r = (int8_t)u8r;
      expected = (int16_t)s8l * (int16_t)s8r;
      break;
   }
   case Iop_MullS16: {
      uint16_t u16l = opnd_l;
      uint16_t u16r = opnd_r;
      int16_t  s16l = (int16_t)u16l;
      int16_t  s16r = (int16_t)u16r;
      expected = (int32_t)s16l * (int32_t)s16r;
      break;
   }
   case Iop_MullS32:
      expected = (int64_t)(int32_t)opnd_l * (int64_t)(int32_t)opnd_r;
      break;

   case Iop_DivU32:
   case Iop_DivU64:
      expected = opnd_l / opnd_r;
      break;

   case Iop_DivS32:
      expected = (int32_t)opnd_l / (int32_t)opnd_r;
      break;

   case Iop_DivS64:
      expected = (int64_t)opnd_l / (int64_t)opnd_r;
      break;

   case Iop_DivU32E:
      expected = (opnd_l << 32) / opnd_r;
      break;

   case Iop_DivS32E:
      expected = (int64_t)(opnd_l << 32) / (int32_t)opnd_r;
      break;

   case Iop_DivModU32to32: {
      uint32_t q = opnd_l / opnd_r;
      uint32_t r = opnd_l % opnd_r;
      expected = ((uint64_t)r << 32) | q;
      break;
   }

   case Iop_DivModS32to32: {
      int32_t q = (int32_t)opnd_l / (int32_t)opnd_r;
      int32_t r = (int32_t)opnd_l % (int32_t)opnd_r;
      expected  = ((uint64_t)r << 32) | (uint32_t)q;
      break;
   }

   case Iop_DivModU64to32: {
      uint64_t q = opnd_l / opnd_r;
      uint64_t r = opnd_l % opnd_r;
      expected = (r << 32) | q;
      break;
   }

   case Iop_DivModS64to32: {
      int64_t q = (int64_t)opnd_l / (int32_t)opnd_r;
      int32_t r = (int64_t)opnd_l % (int32_t)opnd_r;
      expected = ((uint64_t)r << 32) | (uint32_t)q;
      break;
   }

   case Iop_Shl8:
   case Iop_Shl16:
   case Iop_Shl32:
   case Iop_Shl64:
      expected = opnd_l << opnd_r;
      break;

   case Iop_Shr8:
   case Iop_Shr16:
   case Iop_Shr32:
   case Iop_Shr64:
      expected = opnd_l >> opnd_r;
      break;

   case Iop_Sar8:
      expected = ((int64_t)(opnd_l << 56) >> 56) >> opnd_r;
      break;

   case Iop_Sar16:
      expected = ((int64_t)(opnd_l << 48) >> 48) >> opnd_r;
      break;

   case Iop_Sar32:
      expected = ((int64_t)(opnd_l << 32) >> 32) >> opnd_r;
      break;

   case Iop_Sar64:
      expected = (int64_t)opnd_l >> opnd_r;
      break;

   case Iop_Or1:
   case Iop_Or8:
   case Iop_Or16:
   case Iop_Or32:
   case Iop_Or64:
      expected = opnd_l | opnd_r;
      break;

   case Iop_And1:
   case Iop_And8:
   case Iop_And16:
   case Iop_And32:
   case Iop_And64:
      expected = opnd_l & opnd_r;
      break;

   case Iop_Xor8:
   case Iop_Xor16:
   case Iop_Xor32:
   case Iop_Xor64:
      expected = opnd_l ^ opnd_r;
      break;

   case Iop_CmpEQ8:
   case Iop_CmpEQ16:
   case Iop_CmpEQ32:
   case Iop_CmpEQ64:
   case Iop_CasCmpEQ8:
   case Iop_CasCmpEQ16:
   case Iop_CasCmpEQ32:
   case Iop_CasCmpEQ64:
      expected = opnd_l == opnd_r;
      break;

   case Iop_CmpNE8:
   case Iop_CmpNE16:
   case Iop_CmpNE32:
   case Iop_CmpNE64:
   case Iop_CasCmpNE8:
   case Iop_CasCmpNE16:
   case Iop_CasCmpNE32:
   case Iop_CasCmpNE64:
   case Iop_ExpCmpNE8:
   case Iop_ExpCmpNE16:
   case Iop_ExpCmpNE32:
   case Iop_ExpCmpNE64:
      expected = opnd_l != opnd_r;
      break;

   case Iop_CmpLT32U:
   case Iop_CmpLT64U:
      expected = opnd_l < opnd_r;
      break;

   case Iop_CmpLT32S: {
      uint32_t u32l = opnd_l & UINT32_MAX;
      uint32_t u32r = opnd_r & UINT32_MAX;
      int32_t  s32l = (int32_t)u32l;
      int32_t  s32r = (int32_t)u32r;
      expected = s32l < s32r;
      break;
   }

   case Iop_CmpLT64S:
      expected = (int64_t)opnd_l < (int64_t)opnd_r;
      break;

   case Iop_CmpLE32U:
   case Iop_CmpLE64U:
      expected = opnd_l <= opnd_r;
      break;

   case Iop_CmpLE32S: {
      uint32_t u32l = opnd_l & UINT32_MAX;
      uint32_t u32r = opnd_r & UINT32_MAX;
      int32_t  s32l = (int32_t)u32l;
      int32_t  s32r = (int32_t)u32r;
      expected = s32l <= s32r;
      break;
   }

   case Iop_CmpLE64S:
      expected = (int64_t)opnd_l <= (int64_t)opnd_r;
      break;

   case Iop_CmpORD32U:
   case Iop_CmpORD64U:
      expected = (opnd_l < opnd_r) ? 8 : (opnd_l > opnd_r) ? 4 : 2;
      break;

   case Iop_CmpORD32S: {
      uint32_t u32l = opnd_l & UINT32_MAX;
      uint32_t u32r = opnd_r & UINT32_MAX;
      int32_t  s32l = (int32_t)u32l;
      int32_t  s32r = (int32_t)u32r;
      expected = (s32l < s32r) ? 8 : (s32l > s32r) ? 4 : 2;
      break;
   }
   case Iop_CmpORD64S: {
      int64_t opnd_ls = (int64_t)opnd_l;
      int64_t opnd_rs = (int64_t)opnd_r;
      expected = (opnd_ls < opnd_rs) ? 8 : (opnd_ls > opnd_rs) ? 4 : 2;
      break;
   }

   case Iop_Max32U:
      opnd_l &= UINT32_MAX;
      opnd_r &= UINT32_MAX;
      expected = opnd_l > opnd_r ? opnd_l : opnd_r;
      break;

   case Iop_8HLto16:
      expected = (opnd_l << 8) | opnd_r;
      break;

   case Iop_16HLto32:
      expected = (opnd_l << 16) | opnd_r;
      break;

   case Iop_32HLto64:
      expected = (opnd_l << 32) | opnd_r;
      break;

   default:
      panic("%s: operator %s not handled\n", __func__, op->name);
   }

   /* Truncate to width of result type */
   switch (bitsof_irtype(op->result_type)) {
   case 1:  expected &= 0x1;        break;
   case 8:  expected &= UINT8_MAX;  break;
   case 16: expected &= UINT16_MAX; break;
   case 32: expected &= UINT32_MAX; break;
   case 64: expected &= UINT64_MAX; break;
   default:
      panic(__func__);
   }

   return expected;
}


static int
is_shift_op(IROp op)
{
   switch (op) {
   case Iop_Shl8: case Iop_Shl16: case Iop_Shl32: case Iop_Shl64:
   case Iop_Shr8: case Iop_Shr16: case Iop_Shr32: case Iop_Shr64:
   case Iop_Sar8: case Iop_Sar16: case Iop_Sar32: case Iop_Sar64:
      return 1;
   default:
      return 0;
   }
}


static int
ok_to_run(IROp op, uint64_t o1, uint64_t o2)
{
   switch (op) {
      /* Division by zero -- not good */
   case Iop_DivU32: case Iop_DivU64:
   case Iop_DivS32: case Iop_DivS64:
   case Iop_DivU32E:
   case Iop_DivS32E:
   case Iop_DivModU32to32:
      return o2 != 0;

   /* Check that result can be represented */
   case Iop_DivModU64to32: {
      uint64_t dividend = o1;
      uint32_t divisor  = o2;

      if (divisor == 0) return 0;
      uint64_t q = dividend / divisor;  // always safe
      return q <= UINT32_MAX;
   }

   case Iop_DivModS64to32: {
      int64_t dividend = o1;
      int32_t divisor  = o2;

      if (divisor == 0) return 0;
      /* Division may trap on overflow */
      if (divisor == -1 && o1 == (0x1ULL << 63))  // INT64_MIN
         return 0;
      int64_t q = dividend / divisor;
      return q <= INT32_MAX && q >= INT32_MIN;
   }

   case Iop_DivModS32to32: {
      int32_t divisor  = o2;

      if (divisor == 0) return 0;
      /* Division may trap on overflow */
      if (divisor == -1 && o1 == (0x1UL << 31))  // INT32_MIN
         return 0;
      return 1;
   }

   default:
      return 1;
   }
}
