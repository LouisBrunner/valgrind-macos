/* -*- mode: C; c-basic-offset: 3; -*- */

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

int 
main(int argc, char *argv[])
{
   assert(sizeof(long long) == 8);

   for (int i = 1; i < argc; ++i) {
      if (strcmp(argv[i], "-v") == 0) ++verbose;
      else if (strcmp(argv[i], "--help") == 0) {
        printf("\nvbit-test [ -v | --help ]\n");
        printf("\n\t-v     verbose mode; shows IROps being tested\n");
        printf("\n\t-v -v  verbose mode, extreme edition\n\n");
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

      test_data_t *data = new_test_data(op);

      if (op->undef_kind == UNDEF_UNKNOWN) {
         fprintf(stderr, "...skipping %s; unknown undef propagation\n",
                 op->name);
         continue;
      }

      if (verbose) printf("Testing operator %s\n", op->name);

      IRICB iricb = new_iricb(op, data);

      valgrind_vex_init_for_iri(&iricb);

      switch (iricb.num_operands) {
      case 1:
         test_unary_op(op, data);
         break;

      case 2:
         test_binary_op(op, data);
         break;

      case 3:
         test_ternary_op(op, data);
         break;

      case 4:
         test_qernary_op(op, data);
         break;

      default:
         panic("operator not handled");
      }
   }

   return 0;
}
