/* -*- mode: C; c-basic-offset: 3; -*- */

#include <assert.h>
#include <string.h>  // memset
#include "vtest.h"


/* A convenience function to compute either v1 & ~v2 & val2  or
   v1 & ~v2 & ~val2  depending on INVERT_VAL2. */
static vbits_t
and_combine(vbits_t v1, vbits_t v2, value_t val2, int invert_val2)
{
   assert(v1.num_bits == v2.num_bits);

   vbits_t new = { .num_bits = v2.num_bits };

   if (invert_val2) {
      switch (v2.num_bits) {
      case 8:  val2.u8  = ~val2.u8  & 0xff;   break;
      case 16: val2.u16 = ~val2.u16 & 0xffff; break;
      case 32: val2.u32 = ~val2.u32;          break;
      case 64: val2.u64 = ~val2.u64;          break;
      default:
         panic(__func__);
      }
   }

   switch (v2.num_bits) {
   case 8:
      new.bits.u8  = (v1.bits.u8 & ~v2.bits.u8  & val2.u8)  & 0xff;
      break;
   case 16:
      new.bits.u16 = (v1.bits.u16 & ~v2.bits.u16 & val2.u16) & 0xffff;
      break;
   case 32:
      new.bits.u32 = (v1.bits.u32 & ~v2.bits.u32 & val2.u32);
      break;
   case 64:
      new.bits.u64 = (v1.bits.u64 & ~v2.bits.u64 & val2.u64);
      break;
   default:
      panic(__func__);
   }
   return new;
}

/* Check the result of a binary operation. */
static void
check_result_for_binary(const irop_t *op, const test_data_t *data)
{
   const opnd_t *result = &data->result;
   const opnd_t *opnd1  = &data->opnds[0];
   const opnd_t *opnd2  = &data->opnds[1];
   vbits_t expected_vbits;

   /* Only handle those undef-kinds that actually occur. */
   switch (op->undef_kind) {
   case UNDEF_NONE:
      expected_vbits = defined_vbits(result->vbits.num_bits);
      break;

   case UNDEF_ALL:
      expected_vbits = undefined_vbits(result->vbits.num_bits);
      break;

   case UNDEF_LEFT:
      // LEFT with respect to the leftmost 1-bit in both operands
      expected_vbits = left_vbits(or_vbits(opnd1->vbits, opnd2->vbits),
                                  result->vbits.num_bits);
      break;

   case UNDEF_SAME:
      assert(opnd1->vbits.num_bits == opnd2->vbits.num_bits);
      assert(opnd1->vbits.num_bits == result->vbits.num_bits);

      // SAME with respect to the 1-bits in both operands
      expected_vbits = or_vbits(opnd1->vbits, opnd2->vbits);
      break;

   case UNDEF_CONCAT:
      assert(opnd1->vbits.num_bits == opnd2->vbits.num_bits);
      assert(result->vbits.num_bits == 2 * opnd1->vbits.num_bits);
      expected_vbits = concat_vbits(opnd1->vbits, opnd2->vbits);
      break;

   case UNDEF_SHL:
      /* If any bit in the 2nd operand is undefined, so are all bits
         of the result. */
      if (! completely_defined_vbits(opnd2->vbits)) {
         expected_vbits = undefined_vbits(result->vbits.num_bits);
      } else {
         assert(opnd2->vbits.num_bits == 8);
         unsigned shift_amount = opnd2->value.u8;
      
         expected_vbits = shl_vbits(opnd1->vbits, shift_amount);
      }
      break;

   case UNDEF_SHR:
      /* If any bit in the 2nd operand is undefined, so are all bits
         of the result. */
      if (! completely_defined_vbits(opnd2->vbits)) {
         expected_vbits = undefined_vbits(result->vbits.num_bits);
      } else {
         assert(opnd2->vbits.num_bits == 8);
         unsigned shift_amount = opnd2->value.u8;
      
         expected_vbits = shr_vbits(opnd1->vbits, shift_amount);
      }
      break;

   case UNDEF_SAR:
      /* If any bit in the 2nd operand is undefined, so are all bits
         of the result. */
      if (! completely_defined_vbits(opnd2->vbits)) {
         expected_vbits = undefined_vbits(result->vbits.num_bits);
      } else {
         assert(opnd2->vbits.num_bits == 8);
         unsigned shift_amount = opnd2->value.u8;
      
         expected_vbits = sar_vbits(opnd1->vbits, shift_amount);
      }
      break;

   case UNDEF_AND: {
      /* Let v1, v2 be the V-bits of the 1st and 2nd operand, respectively
         Let b1, b2 be the actual value of the 1st and 2nd operand, respect.
         And output bit is undefined (i.e. its V-bit == 1), iff
         (1) (v1 == 1) && (v2 == 1)   OR
         (2) (v1 == 1) && (v2 == 0 && b2 == 1) OR
         (3) (v2 == 1) && (v1 == 0 && b1 == 1)
      */
      vbits_t term1, term2, term3;
      term1 = and_vbits(opnd1->vbits, opnd2->vbits);
      term2 = and_combine(opnd1->vbits, opnd2->vbits, opnd2->value, 0);
      term3 = and_combine(opnd2->vbits, opnd1->vbits, opnd1->value, 0);
      expected_vbits = or_vbits(term1, or_vbits(term2, term3));
      break;
   }

   case UNDEF_OR: {
      /* Let v1, v2 be the V-bits of the 1st and 2nd operand, respectively
         Let b1, b2 be the actual value of the 1st and 2nd operand, respect.
         And output bit is undefined (i.e. its V-bit == 1), iff
         (1) (v1 == 1) && (v2 == 1)   OR
         (2) (v1 == 1) && (v2 == 0 && b2 == 0) OR
         (3) (v2 == 1) && (v1 == 0 && b1 == 0)
      */
      vbits_t term1, term2, term3;
      term1 = and_vbits(opnd1->vbits, opnd2->vbits);
      term2 = and_combine(opnd1->vbits, opnd2->vbits, opnd2->value, 1);
      term3 = and_combine(opnd2->vbits, opnd1->vbits, opnd1->value, 1);
      expected_vbits = or_vbits(term1, or_vbits(term2, term3));
      break;
   }

   case UNDEF_ORD:
      /* Set expected_vbits for the Iop_CmpORD category of iops.
       * If any of the input bits is undefined the least significant
       * three bits in the result will be set, i.e. 0xe.
       */
      expected_vbits = cmpord_vbits(opnd1->vbits.num_bits,
                                    opnd2->vbits.num_bits);
      break;

   default:
      panic(__func__);
   }

   if (! equal_vbits(result->vbits, expected_vbits))
      complain(op, data, expected_vbits);
}


static int 
test_shift(const irop_t *op, test_data_t *data)
{
   unsigned num_input_bits, i;
   opnd_t *opnds = data->opnds;
   int tests_done = 0;

   /* When testing the 1st operand's undefinedness propagation,
      do so with all possible shift amnounts */
   for (unsigned amount = 0; amount < bitsof_irtype(opnds[0].type); ++amount) {
      opnds[1].value.u8 = amount;

      // 1st (left) operand
      num_input_bits = bitsof_irtype(opnds[0].type);

      for (i = 0; i < num_input_bits; ++i) {
         opnds[0].vbits = onehot_vbits(i, bitsof_irtype(opnds[0].type));
         opnds[1].vbits = defined_vbits(bitsof_irtype(opnds[1].type));
         
         valgrind_execute_test(op, data);
         
         check_result_for_binary(op, data);
         tests_done++;
      }
   }

   // 2nd (right) operand

   /* If the operand is an immediate value, there are no v-bits to set. */
   if (op->shift_amount_is_immediate) return tests_done;

   num_input_bits = bitsof_irtype(opnds[1].type);

   for (i = 0; i < num_input_bits; ++i) {
      opnds[0].vbits = defined_vbits(bitsof_irtype(opnds[0].type));
      opnds[1].vbits = onehot_vbits(i, bitsof_irtype(opnds[1].type));

      valgrind_execute_test(op, data);

      check_result_for_binary(op, data);

      tests_done++;
   }
   return tests_done;
}


static value_t
all_bits_zero_value(unsigned num_bits)
{
   value_t val;

   switch (num_bits) {
   case 8:  val.u8  = 0; break;
   case 16: val.u16 = 0; break;
   case 32: val.u32 = 0; break;
   case 64: val.u64 = 0; break;
   default:
      panic(__func__);
   }
   return val;
}


static value_t
all_bits_one_value(unsigned num_bits)
{
   value_t val;

   switch (num_bits) {
   case 8:  val.u8  = 0xff;   break;
   case 16: val.u16 = 0xffff; break;
   case 32: val.u32 = ~0u;    break;
   case 64: val.u64 = ~0ull;  break;
   default:
      panic(__func__);
   }
   return val;
}


static int
test_and(const irop_t *op, test_data_t *data)
{
   unsigned num_input_bits, bitpos;
   opnd_t *opnds = data->opnds;
   int tests_done = 0;

   /* Undefinedness does not propagate if the other operand is 0.
      Use an all-bits-zero operand and test the other operand in
      the usual way (one bit undefined at a time). */

   // 1st (left) operand variable, 2nd operand all-bits-zero
   num_input_bits = bitsof_irtype(opnds[0].type);

   for (bitpos = 0; bitpos < num_input_bits; ++bitpos) {
      opnds[0].vbits = onehot_vbits(bitpos, bitsof_irtype(opnds[0].type));
      opnds[1].vbits = defined_vbits(bitsof_irtype(opnds[1].type));
      opnds[1].value = all_bits_zero_value(bitsof_irtype(opnds[1].type));

      valgrind_execute_test(op, data);
         
      check_result_for_binary(op, data);
      tests_done++;
   }
   
   // 2nd (right) operand variable, 1st operand all-bits-zero
   num_input_bits = bitsof_irtype(opnds[1].type);

   for (bitpos = 0; bitpos < num_input_bits; ++bitpos) {
      opnds[1].vbits = onehot_vbits(bitpos, bitsof_irtype(opnds[1].type));
      opnds[0].vbits = defined_vbits(bitsof_irtype(opnds[0].type));
      opnds[0].value = all_bits_zero_value(bitsof_irtype(opnds[0].type));

      valgrind_execute_test(op, data);
         
      check_result_for_binary(op, data);
      tests_done++;
   }

   /* Undefinedness propagates if the other operand is 1.
      Use an all-bits-one operand and test the other operand in
      the usual way (one bit undefined at a time). */

   // 1st (left) operand variable, 2nd operand all-bits-one
   num_input_bits = bitsof_irtype(opnds[0].type);

   for (bitpos = 0; bitpos < num_input_bits; ++bitpos) {
      opnds[0].vbits = onehot_vbits(bitpos, bitsof_irtype(opnds[0].type));
      opnds[1].vbits = defined_vbits(bitsof_irtype(opnds[1].type));
      opnds[1].value = all_bits_one_value(bitsof_irtype(opnds[1].type));

      valgrind_execute_test(op, data);
         
      check_result_for_binary(op, data);
      tests_done++;
   }
   
   // 2nd (right) operand variable, 1st operand all-bits-one
   num_input_bits = bitsof_irtype(opnds[1].type);

   for (bitpos = 0; bitpos < num_input_bits; ++bitpos) {
      opnds[1].vbits = onehot_vbits(bitpos, bitsof_irtype(opnds[1].type));
      opnds[0].vbits = defined_vbits(bitsof_irtype(opnds[0].type));
      opnds[0].value = all_bits_one_value(bitsof_irtype(opnds[0].type));

      valgrind_execute_test(op, data);
         
      check_result_for_binary(op, data);
      tests_done++;
   }
   return tests_done;
}


static int
test_or(const irop_t *op, test_data_t *data)
{
   unsigned num_input_bits, bitpos;
   opnd_t *opnds = data->opnds;
   int tests_done = 0;

   /* Undefinedness does not propagate if the other operand is 1.
      Use an all-bits-one operand and test the other operand in
      the usual way (one bit undefined at a time). */

   // 1st (left) operand variable, 2nd operand all-bits-one
   num_input_bits = bitsof_irtype(opnds[0].type);

   opnds[0].vbits = defined_vbits(bitsof_irtype(opnds[0].type));
   opnds[1].vbits = defined_vbits(bitsof_irtype(opnds[1].type));
   opnds[1].value = all_bits_one_value(bitsof_irtype(opnds[1].type));

   for (bitpos = 0; bitpos < num_input_bits; ++bitpos) {
      opnds[0].vbits = onehot_vbits(bitpos, bitsof_irtype(opnds[0].type));

      valgrind_execute_test(op, data);
         
      check_result_for_binary(op, data);
      tests_done++;
   }
   
   // 2nd (right) operand variable, 1st operand all-bits-one
   num_input_bits = bitsof_irtype(opnds[1].type);

   opnds[0].vbits = defined_vbits(bitsof_irtype(opnds[0].type));
   opnds[1].vbits = defined_vbits(bitsof_irtype(opnds[1].type));
   opnds[0].value = all_bits_one_value(bitsof_irtype(opnds[0].type));

   for (bitpos = 0; bitpos < num_input_bits; ++bitpos) {
      opnds[1].vbits = onehot_vbits(bitpos, bitsof_irtype(opnds[1].type));

      valgrind_execute_test(op, data);
         
      check_result_for_binary(op, data);
      tests_done++;
   }

   /* Undefinedness propagates if the other operand is 0.
      Use an all-bits-zero operand and test the other operand in
      the usual way (one bit undefined at a time). */

   // 1st (left) operand variable, 2nd operand all-bits-zero
   num_input_bits = bitsof_irtype(opnds[0].type);

   opnds[0].vbits = defined_vbits(bitsof_irtype(opnds[0].type));
   opnds[1].vbits = defined_vbits(bitsof_irtype(opnds[1].type));
   opnds[1].value = all_bits_zero_value(bitsof_irtype(opnds[1].type));

   for (bitpos = 0; bitpos < num_input_bits; ++bitpos) {
      opnds[0].vbits = onehot_vbits(bitpos, bitsof_irtype(opnds[0].type));

      valgrind_execute_test(op, data);
         
      check_result_for_binary(op, data);
      tests_done++;
   }
   
   // 2nd (right) operand variable, 1st operand all-bits-zero
   num_input_bits = bitsof_irtype(opnds[1].type);

   opnds[0].vbits = defined_vbits(bitsof_irtype(opnds[0].type));
   opnds[1].vbits = defined_vbits(bitsof_irtype(opnds[1].type));
   opnds[0].value = all_bits_zero_value(bitsof_irtype(opnds[0].type));

   for (bitpos = 0; bitpos < num_input_bits; ++bitpos) {
      opnds[1].vbits = onehot_vbits(bitpos, bitsof_irtype(opnds[1].type));

      valgrind_execute_test(op, data);
         
      check_result_for_binary(op, data);
      tests_done++;
   }
   return tests_done;
}


int
test_binary_op(const irop_t *op, test_data_t *data)
{
   unsigned num_input_bits, i, bitpos;
   opnd_t *opnds = data->opnds;
   int tests_done = 0;

   /* Handle special cases upfront */
   switch (op->undef_kind) {
   case UNDEF_SHL:
   case UNDEF_SHR:
   case UNDEF_SAR:
      return test_shift(op, data);

   case UNDEF_AND:
      return test_and(op, data);

   case UNDEF_OR:
      return test_or(op, data);

   default:
      break;
   }

   /* For each operand, set a single bit to undefined and observe how
      that propagates to the output. Do this for all bits in each
      operand. */
   for (i = 0; i < 2; ++i) {

      /* If this is a shift op that requires an immediate shift amount,
         do not iterate the v-bits of the 2nd operand */
      if (i == 1 && op->shift_amount_is_immediate) break;

      num_input_bits = bitsof_irtype(opnds[i].type);
      opnds[0].vbits = defined_vbits(bitsof_irtype(opnds[0].type));
      opnds[1].vbits = defined_vbits(bitsof_irtype(opnds[1].type));

      /* Set the value of the 2nd operand to something != 0. So division
         won't crash. */
      memset(&opnds[1].value, 0xff, sizeof opnds[1].value);

      /* For immediate shift amounts choose a value of '1'. That should
         not cause a problem. */
      if (op->shift_amount_is_immediate)
         opnds[1].value.u8 = 1;

      for (bitpos = 0; bitpos < num_input_bits; ++bitpos) {
         opnds[i].vbits = onehot_vbits(bitpos, bitsof_irtype(opnds[i].type));

         valgrind_execute_test(op, data);

         check_result_for_binary(op, data);

         tests_done++;
      }
   }
   return tests_done;
}
