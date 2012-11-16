/* -*- mode: C; c-basic-offset: 3; -*- */

#include <assert.h>
#include "vtest.h"


/* Check the result of a unary operation. */
static void
check_result_for_unary(const irop_t *op, const test_data_t *data)
{
   const opnd_t *result = &data->result;
   const opnd_t *opnd   = &data->opnds[0];
   unsigned num_bits = result->vbits.num_bits;
   vbits_t expected_vbits;

   /* Only handle those undef-kinds that actually occur. */
   switch (op->undef_kind) {
   case UNDEF_ALL:
      expected_vbits = undefined_vbits(num_bits);
      break;

   case UNDEF_SAME:
      expected_vbits = opnd->vbits;
      break;

   case UNDEF_TRUNC:
      expected_vbits = truncate_vbits(opnd->vbits, num_bits);
      break;

   case UNDEF_LEFT:
      expected_vbits = left_vbits(opnd->vbits, num_bits);
      break;

   case UNDEF_UPPER:
      assert(num_bits * 2 == opnd->vbits.num_bits);
      expected_vbits = upper_vbits(opnd->vbits);
      break;

   case UNDEF_SEXT:
      expected_vbits = sextend_vbits(opnd->vbits, num_bits);
      break;

   case UNDEF_ZEXT:
      expected_vbits = zextend_vbits(opnd->vbits, num_bits);
      break;

   default:
      panic(__func__);
   }

   if (! equal_vbits(result->vbits, expected_vbits))
      complain(op, data, expected_vbits);
}


int
test_unary_op(const irop_t *op, test_data_t *data)
{
   unsigned num_input_bits, bitpos;
   int tests_done = 0;

   num_input_bits = bitsof_irtype(data->opnds[0].type);

   for (bitpos = 0; bitpos < num_input_bits; ++bitpos) {
      data->opnds[0].vbits = onehot_vbits(bitpos, num_input_bits);

      valgrind_execute_test(op, data);

      check_result_for_unary(op, data);
      tests_done++;
   }
   return tests_done;
}
