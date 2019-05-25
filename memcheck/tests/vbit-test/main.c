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

#include <assert.h>    // assert
#include <stdio.h>     // printf
#include <stdlib.h>    // malloc
#include <string.h>    // memset
#include "valgrind.h"  // RUNNING_ON_VALGRIND
#include "vtest.h"


static test_data_t *
new_test_data(const irop_t *op)
{
   test_data_t *data = malloc(sizeof *data);

   memset(data, 0x0, sizeof *data);  // initialise

   /* Obtain the operand types and set them */
   IRType t_dst, t1, t2, t3, t4;

   typeof_primop(op->op, &t_dst, &t1, &t2, &t3, &t4);
   assert(t_dst != Ity_INVALID);
   assert(t1    != Ity_INVALID);

   data->result.type = t_dst;
   if (is_floating_point_op_with_rounding_mode(op->op)) {
      data->opnds[0].type = t2;
      data->opnds[1].type = t3;
      data->opnds[2].type = t4;
      data->opnds[3].type = Ity_INVALID;
   } else {
      data->opnds[0].type = t1;
      data->opnds[1].type = t2;
      data->opnds[2].type = t3;
      data->opnds[3].type = t4;
   }

   /* Set the rounding mode if the operation requires one. 
      FIXME: We should iterate over all rounding modes. For that need
      FIXME: to distinguish between binary and decimal floating point */
   if (is_floating_point_op_with_rounding_mode(op->op)) {
      // for now just pick one
      data->rounding_mode = Irrm_NEAREST;  // same as Irrm_DFP_NEAREST
   } else {
      data->rounding_mode = NO_ROUNDING_MODE;
   }

   return data;
}

int verbose = 0;


/* Certain IROps require special handling. */
static void
fixup_irops(void)
{
#ifdef __powerpc__
   irop_t* tmp;

   /* Iops with immediate shift value */
   tmp = get_irop(Iop_ShlD64);
   if (tmp) {
      tmp->immediate_index = 2;
      tmp->immediate_type = Ity_I8;
   }

   tmp = get_irop(Iop_ShrD64);
   if (tmp) {
      tmp->immediate_index = 2;
      tmp->immediate_type = Ity_I8;
   }

   tmp = get_irop(Iop_ShlD128);
   if (tmp) {
      tmp->immediate_index = 2;
      tmp->immediate_type = Ity_I8;
   }

   tmp = get_irop(Iop_ShrD128);
   if (tmp) {
      tmp->immediate_index = 2;
      tmp->immediate_type = Ity_I8;
   }

   /* Iops with immediate value that controls PPC instruction behavior */
   tmp = get_irop(Iop_SHA256);
   if (tmp) {
      tmp->immediate_index = 2;
      tmp->immediate_type = Ity_I8;
   }

   tmp = get_irop(Iop_SHA512);
   if (tmp) {
      tmp->immediate_index = 2;
      tmp->immediate_type = Ity_I8;
   }
#endif
}


int 
main(int argc, char *argv[])
{
   assert(sizeof(long long) == 8);
   int num_unary_tests = 0, num_binary_tests = 0;
   int num_ternary_tests = 0, num_qernary_tests = 0;

   for (int i = 1; i < argc; ++i) {
      if (strcmp(argv[i], "-v") == 0) ++verbose;
      else if (strcmp(argv[i], "--help") == 0) {
        printf("\nvbit-test [ -v | --help ]\n");
        printf("\n\t-v       verbose mode; show number of tests\n");
        printf("\n\t-v -v    verbose mode; shows IROps being tested\n");
        printf("\n\t-v -v -v verbose mode, extreme edition\n\n");
        return 0;
      } else {
        printf("%s ?  Nothing happens.\n", argv[i]);
        return 1;
      }
   }

   if (! RUNNING_ON_VALGRIND) {
     fprintf(stderr, "*** This program needs to run under memcheck.\n");
     return 1;
   }

   setbuf(stdout, NULL);  // make stdout unbuffered

   fixup_irops();         // determine need for special handling

   // Iterate over all primops
   IROp first = Iop_INVALID + 1;
   IROp last  = Iop_LAST;
   IROp opkind;

   if (0) {   // overwrite for debugging
      first = Iop_CasCmpEQ8; last = first + 1;
   }

   // Iterate over all IROps in the enum type. That is the only way to
   // make sure the operator is tested on at least one platform.

   // Loop assumes no holes in the enumerator values
   for (opkind = first; opkind < last; ++opkind) {

      const irop_t *op = get_irop(opkind);
      if (op == NULL) continue;

      if (op->undef_kind == UNDEF_UNKNOWN) {
         fprintf(stderr, "...skipping %s; unknown undef propagation\n",
                 op->name);
         continue;
      }

      test_data_t *data = new_test_data(op);

      if (verbose > 1) printf("Testing operator %s\n", op->name);

      IRICB iricb = new_iricb(op, data);

      valgrind_vex_init_for_iri(&iricb);

      switch (iricb.num_operands) {
      case 1:
         num_unary_tests += test_unary_op(op, data);
         break;

      case 2:
         num_binary_tests += test_binary_op(op, data);
         break;

      case 3:
         num_ternary_tests += test_ternary_op(op, data);
         break;

      case 4:
         num_qernary_tests += test_qernary_op(op, data);
         break;

      default:
         panic("operator not handled");
      }

      free(data);
   }

   if (verbose) 
      printf("\nvbit-test ran  %d unary, %d binary, %d ternary and"
             " %d qernary tests.\n\n",
             num_unary_tests, num_binary_tests, num_ternary_tests,
             num_qernary_tests);
   return 0;
}
