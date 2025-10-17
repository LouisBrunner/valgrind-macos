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

#include <assert.h>
#include "vtest.h"


/* Check the result of a ternary operation. */
static void
check_result_for_ternary(const irop_t *op, const test_data_t *data)
{
   const opnd_t *result = &data->result;
   const opnd_t *opnd1  = &data->opnds[0];
   const opnd_t *opnd2  = &data->opnds[1];
   const opnd_t *opnd3  = &data->opnds[2];
   vbits_t expected_vbits;

   /* Only handle those undef-kinds that actually occur. */
   switch (op->undef_kind) {
   case UNDEF_ALL:
      expected_vbits = undefined_vbits(result->vbits.num_bits);
      break;

   case UNDEF_SAME:
      // SAME with respect to the 1-bits in all operands
      expected_vbits = or_vbits(or_vbits(opnd1->vbits, opnd2->vbits),
                                opnd3->vbits);
      break;

   case UNDEF_SOME:
      expected_vbits.num_bits = result->vbits.num_bits;

      if ((result->vbits.bits.u128[0] != 0) ||
          (result->vbits.bits.u128[1] != 0)) {
         expected_vbits.bits.u128[0] = result->vbits.bits.u128[0];
         expected_vbits.bits.u128[1] = result->vbits.bits.u128[1];

      } else {
         /* The input had at least one vbit set but the result doesn't have any
          * bit set.  Set them all so we will trigger the error on the call
          * to complain().
          */
         expected_vbits.bits.u128[0] = ~0x0ULL;
         expected_vbits.bits.u128[1] = ~0x0ULL;
      }

      break;

   default:
      panic(__func__);
   }

   if (! equal_vbits(result->vbits, expected_vbits))
      complain(op, data, expected_vbits);
}


int
test_ternary_op(const irop_t *op, test_data_t *data)
{
   unsigned num_input_bits, i, bitpos;
   opnd_t *opnds = data->opnds;
   int tests_done = 0;

   /* For each operand, set a single bit to undefined and observe how
      that propagates to the output. Do this for all bits in each
      operand. */
   for (i = 0; i < 3; ++i) {
      num_input_bits = bitsof_irtype(opnds[i].type);

      opnds[0].vbits = defined_vbits(bitsof_irtype(opnds[0].type));
      opnds[1].vbits = defined_vbits(bitsof_irtype(opnds[1].type));
      opnds[2].vbits = defined_vbits(bitsof_irtype(opnds[2].type));

      for (bitpos = 0; bitpos < num_input_bits; ++bitpos) {
         opnds[i].vbits = onehot_vbits(bitpos, bitsof_irtype(opnds[i].type));

         valgrind_execute_test(op, data);

         check_result_for_ternary(op, data);

	 tests_done++;
      }
   }
   return tests_done;
}
