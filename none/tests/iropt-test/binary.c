/* -*- mode: C; c-basic-offset: 3; -*- */

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2025  Florian Krohm

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

#include <stdio.h>      // printf
#include <stdint.h>     // UINT64_MAX
#include "vtest.h"

static void check_result(const irop_t *, const test_data_t *);
static void run_tests(const irop_t *, test_data_t *, unsigned, uint64_t *,
                      unsigned, uint64_t *);
static int  is_shift_op(IROp);


void
test_binary_op(const irop_t *op, test_data_t *data)
{
   opnd_t *opnd_l = &data->opnds[0];

   switch (opnd_l->type) {
   case Ity_I1: {
      uint64_t values[] = { 0, 1 };

      run_tests(op, data, NUM_EL(values), values, NUM_EL(values), values);
      break;
   }

   case Ity_I8: {
      uint64_t values[] = { 0, 1, 2, UINT8_MAX - 1, UINT8_MAX };
      uint64_t shifts[] = { 0, 1, 2, 6, 7 };

      if (is_shift_op(op->op))
         run_tests(op, data, NUM_EL(values), values, NUM_EL(shifts), shifts);
      else
         run_tests(op, data, NUM_EL(values), values, NUM_EL(values), values);
      break;
   }

   case Ity_I16: {
      uint64_t values[] = { 0, 1, 2, UINT16_MAX - 1, UINT16_MAX };
      uint64_t shifts[] = { 0, 1, 2, 14, 15 };

      if (is_shift_op(op->op))
         run_tests(op, data, NUM_EL(values), values, NUM_EL(shifts), shifts);
      else
         run_tests(op, data, NUM_EL(values), values, NUM_EL(values), values);
      break;
   }

   case Ity_I32: {
      uint64_t values[] = { 0, 1, 2, UINT32_MAX - 1, UINT32_MAX };
      uint64_t shifts[] = { 0, 1, 2, 30, 31 };

      if (is_shift_op(op->op))
         run_tests(op, data, NUM_EL(values), values, NUM_EL(shifts), shifts);
      else
         run_tests(op, data, NUM_EL(values), values, NUM_EL(values), values);
      break;
   }

   case Ity_I64: {
      uint64_t values[] = { 0, 1, 2, UINT64_MAX - 1, UINT64_MAX };
      uint64_t shifts[] = { 0, 1, 2, 62, 63 };

      if (is_shift_op(op->op))
         run_tests(op, data, NUM_EL(values), values, NUM_EL(shifts), shifts);
      else
         run_tests(op, data, NUM_EL(values), values, NUM_EL(values), values);
      break;
   }

   default:
      panic(__func__);
   }
}


static void
run_tests(const irop_t *op, test_data_t *data, unsigned num_val_l,
          uint64_t *values_l, unsigned num_val_r, uint64_t *values_r)
{
   opnd_t *opnd_l = &data->opnds[0];
   opnd_t *opnd_r = &data->opnds[1];

   for (unsigned i = 0; i < num_val_l; ++i) {
      opnd_l->value = values_l[i];
      for (unsigned j = 0; j < num_val_r; ++j) {
         opnd_r->value = values_r[j];

         valgrind_execute_test(op, data);
         check_result(op, data);
      }
   }
}


/* Check the result of a binary operation. */
static void
check_result(const irop_t *op, const test_data_t *data)
{
   uint64_t result = data->result.value;
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

   case Iop_MullS32:
      expected = (int64_t)(int32_t)opnd_l * (int64_t)(int32_t)opnd_r;
      break;

   case Iop_Shl32:
      expected = opnd_l << opnd_r;
      break;

   case Iop_Shl64:
      expected = opnd_l << opnd_r;
      break;

   case Iop_Shr32:
      expected = opnd_l >> opnd_r;
      break;

   case Iop_Shr64:
      expected = opnd_l >> opnd_r;
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
//   case Iop_CasCmpEQ8:
//   case Iop_CasCmpEQ16:
//   case Iop_CasCmpEQ32:
//   case Iop_CasCmpEQ64:
      expected = opnd_l == opnd_r;
      break;

   case Iop_CmpNE8:
//   case Iop_CmpNE16:
   case Iop_CmpNE32:
   case Iop_CmpNE64:
   case Iop_CasCmpNE8:
//   case Iop_CasCmpNE16:
   case Iop_CasCmpNE32:
   case Iop_CasCmpNE64:
   case Iop_ExpCmpNE8:
//   case Iop_ExpCmpNE16:
   case Iop_ExpCmpNE32:
   case Iop_ExpCmpNE64:
      expected = opnd_l != opnd_r;
      break;

   case Iop_CmpLT32U:
   case Iop_CmpLT64U:
      expected = opnd_l < opnd_r;
      break;

   case Iop_CmpLT32S: {
      int32_t opnd_ls = (int32_t)(opnd_l & UINT32_MAX);
      int32_t opnd_rs = (int32_t)(opnd_r & UINT32_MAX);
      expected = opnd_ls < opnd_rs;
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
      int32_t opnd_ls = (int32_t)(opnd_l & UINT32_MAX);
      int32_t opnd_rs = (int32_t)(opnd_r & UINT32_MAX);
      expected = opnd_ls <= opnd_rs;
      break;
   }

   case Iop_CmpLE64S:
      expected = (int64_t)opnd_l <= (int64_t)opnd_r;
      break;

   case Iop_CmpORD32S: {
      int32_t opnd_ls = (int32_t)(opnd_l & UINT32_MAX);
      int32_t opnd_rs = (int32_t)(opnd_r & UINT32_MAX);
      expected = (opnd_ls < opnd_rs) ? 8 : (opnd_ls > opnd_rs) ? 4 : 2;
      break;
   }

   case Iop_Max32U:
      opnd_l &= UINT32_MAX;
      opnd_r &= UINT32_MAX;
      expected = opnd_l > opnd_r ? opnd_l : opnd_r;
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

   if (verbose > 1) {
      printf("expected:  value = ");
      print_value(stdout, expected, bitsof_irtype(data->result.type));
      printf("\n");
   }

   int ok = 1;
   switch (data->result.type) {
   case Ity_I1:  ok = result == expected; break;
   case Ity_I8:  ok = result == expected; break;
   case Ity_I16: ok = result == expected; break;
   case Ity_I32: ok = result == expected; break;
   case Ity_I64: ok = result == expected; break;
   default:
      panic(__func__);
   }

   if (! ok)
      complain(op, data, expected);
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
