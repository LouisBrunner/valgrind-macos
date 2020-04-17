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
      case 1:  val2.u32 = ~val2.u32 & 1;      break;
      case 8:  val2.u8  = ~val2.u8  & 0xff;   break;
      case 16: val2.u16 = ~val2.u16 & 0xffff; break;
      case 32: val2.u32 = ~val2.u32;          break;
      case 64: val2.u64 = ~val2.u64;          break;
      default:
         panic(__func__);
      }
   }

   switch (v2.num_bits) {
   case 1:
      new.bits.u32 = (v1.bits.u32 & ~v2.bits.u32 & val2.u32) & 1;
      break;
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
   opnd_t tmp;
   vbits_t expected_vbits;

   /* Only handle those undef-kinds that actually occur. */
   switch (op->undef_kind) {
   case UNDEF_NONE:
      expected_vbits = defined_vbits(result->vbits.num_bits);
      break;

   case UNDEF_ALL:
      /* Iop_ShlD64, Iop_ShrD64, Iop_ShlD128, Iop_ShrD128 have
       * one immediate operand in operand 2.
       */
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

   case UNDEF_CMP_EQ_NE:
      expected_vbits = cmp_eq_ne_vbits(opnd1->vbits, opnd2->vbits,
                                       opnd1->value, opnd2->value);
      break;

   case UNDEF_INT_ADD:
      expected_vbits = int_add_or_sub_vbits(1/*isAdd*/,
                                            opnd1->vbits, opnd2->vbits,
                                            opnd1->value, opnd2->value);
      break;

   case UNDEF_INT_SUB:
      expected_vbits = int_add_or_sub_vbits(0/*!isAdd*/,
                                            opnd1->vbits, opnd2->vbits,
                                            opnd1->value, opnd2->value);
      break;

   case UNDEF_ALL_64x2:
      assert(opnd1->vbits.num_bits == opnd2->vbits.num_bits);
      expected_vbits =
         undefined_vbits_BxE(64, 2,
                             or_vbits(opnd1->vbits, opnd2->vbits));
      break;

   case UNDEF_ALL_32x4:
      assert(opnd1->vbits.num_bits == opnd2->vbits.num_bits);
      expected_vbits =
         undefined_vbits_BxE(32, 4,
                             or_vbits(opnd1->vbits, opnd2->vbits));
      break;

   case UNDEF_ALL_16x8:
      assert(opnd1->vbits.num_bits == opnd2->vbits.num_bits);
      expected_vbits =
         undefined_vbits_BxE(16, 8,
                             or_vbits(opnd1->vbits, opnd2->vbits));
      break;

   case UNDEF_ALL_8x16:
      assert(opnd1->vbits.num_bits == opnd2->vbits.num_bits);
      expected_vbits =
         undefined_vbits_BxE(8, 16,
                             or_vbits(opnd1->vbits, opnd2->vbits));
      break;

   case UNDEF_ALL_32x4_EVEN:
      /* Only even input bytes are used, result can be twice as wide */
      assert(opnd1->vbits.num_bits == opnd2->vbits.num_bits);
      expected_vbits =
         undefined_vbits_BxE(64, 2,
                             undefined_vbits_128_even_element(32, 4,
                                        or_vbits(opnd1->vbits, opnd2->vbits)));
      break;

   case UNDEF_ALL_16x8_EVEN:
      /* Only even input bytes are used, result can be twice as wide */
      assert(opnd1->vbits.num_bits == opnd2->vbits.num_bits);
      expected_vbits =
         undefined_vbits_BxE(32, 4,
                             undefined_vbits_128_even_element(16, 8,
                                        or_vbits(opnd1->vbits, opnd2->vbits)));
      break;

   case UNDEF_ALL_8x16_EVEN:
      /* Only even input bytes are used, result can be twice as wide */
      assert(opnd1->vbits.num_bits == opnd2->vbits.num_bits);
      expected_vbits =
         undefined_vbits_BxE(16, 8,
                             undefined_vbits_128_even_element(8, 16,
                                        or_vbits(opnd1->vbits, opnd2->vbits)));
      break;

   case UNDEF_64x2_ROTATE:
      /* Rotate left each element in opnd1 by the amount in the corresponding
       * element of opnd2.
       */
      assert(opnd1->vbits.num_bits == opnd2->vbits.num_bits);
      /* Setup the tmp to match what the vbit tester seems to use.  I can't
       * use opnd2-value since valgrind doesn't think it has been set.
       */
      tmp.value.u128[0] = -1;
      tmp.value.u128[1] = -1;
      /* Calculate expected for the first operand when it is shifted.
       * If any of the vbits are set for the shift field of the second operand
       * then the result of the expected result for that element is all 1's.
       */
      expected_vbits = or_vbits(undefined_vbits_BxE_rotate(64, 2, opnd1->vbits,
                                                           tmp.value),
                                undefined_vbits_BxE(64, 2, opnd2->vbits));
      break;

   case UNDEF_32x4_ROTATE:
      /* Rotate left each element in opnd1 by the amount in the corresponding
       * element of opnd2.
       */
      assert(opnd1->vbits.num_bits == opnd2->vbits.num_bits);
      expected_vbits = undefined_vbits_BxE_rotate(32, 4, opnd1->vbits,
                                                  opnd2->value);
      break;

   case UNDEF_16x8_ROTATE:
      /* Rotate left each element in opnd1 by the amount in the corresponding
       * element of opnd2.
       */
      assert(opnd1->vbits.num_bits == opnd2->vbits.num_bits);
      expected_vbits = undefined_vbits_BxE_rotate(16, 8, opnd1->vbits,
                                                  opnd2->value);
      break;

   case UNDEF_8x16_ROTATE:
      /* Rotate left each element in opnd1 by the amount in the corresponding
       * element of opnd2.
       */
      assert(opnd1->vbits.num_bits == opnd2->vbits.num_bits);
      expected_vbits = undefined_vbits_BxE_rotate(16, 8, opnd1->vbits,
                                                  opnd2->value);
      break;

   case UNDEF_SOME:
      /* The result for the Iop_SHA256 and Iop_SHA256 is a secure hash. If
       * one of the input bits is not defined there must be atleast one
       * undefined bit in the output.  Which bit and how many depends on
       * which bit is undefined.  Don't know the secure hash algorithm so
       * we can only make sure at least one of the result bits is set.
       *
       * The Iop_SHA256, Iop_SHA512 iops have one immediate value in the
       * second operand.
       */
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

   case UNDEF_NARROW256_AtoB:
      assert(opnd1->vbits.num_bits == opnd2->vbits.num_bits);
      switch(op->op) {
      case Iop_NarrowBin64to32x4:
         expected_vbits =
            undefined_vbits_Narrow256_AtoB(64, 32, opnd1->vbits, opnd1->value,
                                           opnd2->vbits, opnd2->value,
                                           False);
         break;
      case Iop_QNarrowBin64Sto32Sx4:
         expected_vbits =
            undefined_vbits_Narrow256_AtoB(64, 32, opnd1->vbits, opnd1->value,
                                           opnd2->vbits, opnd2->value,
                                           True);
         break;
      case Iop_QNarrowBin64Uto32Ux4:
         expected_vbits =
            undefined_vbits_Narrow256_AtoB(64, 32, opnd1->vbits, opnd1->value,
                                           opnd2->vbits, opnd2->value,
                                           True);
         break;
      default:
         fprintf(stderr, "ERROR, unknown Iop for UNDEF_NARROW256_AtoB\n");
         panic(__func__);
      }
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
   if (!op->immediate_index) return tests_done;

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
   case 1:
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
   case 1:  val.u32 = 1;      break;
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

      /* If this is a Iop that requires an immediate amount,
         do not iterate the v-bits of the operand */
      if (((i+1) == op->immediate_index)
          && (op->immediate_index)) break;

      num_input_bits = bitsof_irtype(opnds[i].type);
      opnds[0].vbits = defined_vbits(bitsof_irtype(opnds[0].type));
      opnds[1].vbits = defined_vbits(bitsof_irtype(opnds[1].type));

      /* Set the value of the 2nd operand to something != 0. So division
         won't crash. */
      memset(&opnds[1].value, 0xff, sizeof opnds[1].value);

      /* For immediate shift amounts choose a value of '1'. That value should
         not cause a problem. Note: we always assign to the u64 member here.
         The reason is that in ir_inject.c the value_t type is not visible.
         The value is picked up there by interpreting the memory as an
         ULong value. So, we rely on 
         union {
           ULong   v1;   // value picked up in ir_inject.c
           value_t v2;   // value assigned here
         } xx;
         assert(sizeof xx.v1 == sizeof xx.v2.u64);
         assert(xx.v1 == xx.v2.u64);
      */

      if (op->immediate_index > 0) {
         assert((op->immediate_type == Ity_I8)
                || (op->immediate_type == Ity_I16)
                || (op->immediate_type == Ity_I32));
         opnds[1].value.u64 = 1;
      }

      for (bitpos = 0; bitpos < num_input_bits; ++bitpos) {
         opnds[i].vbits = onehot_vbits(bitpos, bitsof_irtype(opnds[i].type));

         valgrind_execute_test(op, data);

         check_result_for_binary(op, data);

         tests_done++;
      }
   }
   return tests_done;
}
