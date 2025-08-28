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

#include <assert.h>    // assert
#include <stdio.h>     // printf
#include <stdlib.h>    // malloc
#include <string.h>    // memset
#include "valgrind.h"  // RUNNING_ON_VALGRIND
#include "vtest.h"

/* Table of IROps. */
static irop_t irops[] = {
   #include "irops.tab"
};

static void check_irops_table(void);
static test_data_t *new_test_data(const irop_t *);
static int is_enabled(const irop_t *);

int verbose = 0;
unsigned num_random_tests;


int
main(int argc, char *argv[])
{
// FIXME: temporarily until ppc has been fixed
#if !defined(__s390x__) && !defined(__i386__) && !defined(__x86_64__)
   return 0;
#endif
   assert(sizeof(long long) == 8);
   srand48(42L);

   for (int i = 1; i < argc; ++i) {
      if (strcmp(argv[i], "-v") == 0)
         ++verbose;
      else if (strncmp(argv[i], "-r", 2) == 0) {
         num_random_tests = atoi(argv[i] + 2);
      } else if (strcmp(argv[i], "--help") == 0) {
        printf("\niropt-test [ -v | --help ]\n");
        printf("\n\t -rNUM number of random tests per IRop\n");
        printf("\n\t -v    verbose mode; shows IROps being tested\n");
        printf("\n\t -v -v verbose mode, extreme edition\n\n");
        return 0;
      } else {
        printf("%s ?  Nothing happens.\n", argv[i]);
        return 1;
      }
   }

   if (! RUNNING_ON_VALGRIND) {
     fprintf(stderr, "*** This program needs to run under valgrind.\n");
     return 1;
   }

   check_irops_table();

   setbuf(stdout, NULL);  // make stdout unbuffered

   for (unsigned i = 0; i < NUM_EL(irops); ++i) {
      const irop_t *op = irops +i;

      if (! is_enabled(op)) continue;

      if (verbose)
         printf("Testing operator %s\n", op->name);

      test_data_t *data = new_test_data(op);

      IRICB *iricb = new_iricb(op, data);

      valgrind_vex_init_for_iri(iricb);

      switch (op->num_opnds) {
      case 1:
         test_unary_op(op, data);
         break;

      case 2:
         test_binary_op(op, data);
         break;

      default:
         panic("operator %s not handled", op->name);
      }

      free(data);
   }

   return 0;
}


static void
check_irops_table(void)
{
   for (unsigned i = 0; i < sizeof irops / sizeof *irops; ++i) {
      const irop_t *op = irops +i;

      IRType t_res, t_opnd1, t_opnd2, t_opnd3, t_opnd4;

      typeOfPrimop(op->op, &t_res, &t_opnd1, &t_opnd2, &t_opnd3, &t_opnd4);

      if (op->result_type != t_res   ||
          op->opnd1_type  != t_opnd1 ||
          (op->num_opnds == 2 && op->opnd2_type  != t_opnd2))
         panic("%s: type mismatch\n", op->name);
   }
}


static test_data_t *
new_test_data(const irop_t *op)
{
   test_data_t *data = malloc(sizeof *data);

   memset(data, 0x0, sizeof *data);  // initialise

   data->result_fold.type   = op->result_type;
   data->result_nofold.type = op->result_type;

   data->opnds[0].type = op->opnd1_type;
   if (op->num_opnds > 1)
      data->opnds[1].type = op->opnd2_type;

   return data;
}


static int
is_enabled(const irop_t *op)
{
   /* For convenience of specification in irops.tab a zero value
      means that the IROp is implemented. */
   if (op->enabled_arch == 0) return 1;

#ifdef __x86_64__
   return op->enabled_arch & ARCH_amd64;
#endif
#ifdef __i386__
   return op->enabled_arch & ARCH_x86;
#endif
#ifdef __s390x__
   return op->enabled_arch & ARCH_s390;
#endif
#ifdef __powerpc__    /* defined for both 32-bit and 64-bit */
#define  MIN_POWER_ISA  "../../../tests/min_power_isa"
   int rc;

   switch (op->op) {
   case Iop_DivS64E:
   case Iop_DivU64E:
   case Iop_DivU32E:
   case Iop_DivS32E:
      /* IROps require a processor that supports ISA 2.06 (Power 7)
         or newer */
      rc = system(MIN_POWER_ISA " 2.06 ") / 256;
      break;

   case Iop_DivU128:
   case Iop_DivS128:
   case Iop_DivU128E:
   case Iop_DivS128E:
   case Iop_ModU128:
   case Iop_ModS128:
      /* IROps require a processor that supports ISA 3.10 (Power 10)
         or newer */
      rc = system(MIN_POWER_ISA " 3.1 ") / 256;
      break;

   default:
      rc = 0;
      break;
   }

   /* MIN_POWER_ISA returns 0 if the underlying HW supports the specified
      ISA or newer. Returns 1 if the HW does not support the specified ISA.
      Returns 2 on error. */
   switch (rc) {
   case 0:
#ifdef __powerpc64__
      return op->enabled_arch & ARCH_ppc64;
#endif
      return op->enabled_arch & ARCH_ppc32;
   case 1:
      return 0;
   default:
      panic("min_power_isa() return code is invalid.\n");
   }
#endif

   return 0;
}
