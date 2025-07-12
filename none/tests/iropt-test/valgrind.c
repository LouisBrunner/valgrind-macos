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

#include <string.h>    // memset
#include "valgrind.h"  // VALGRIND_VEX_INJECT_IR
#include "vtest.h"


/* Return a completely initialised control block */
IRICB
new_iricb(const irop_t *op, test_data_t *data)
{
   IRICB_iropt_payload cb;

   memset(&cb, 0x0, sizeof cb);

   cb.op = op->op;
   cb.result = (HWord)&data->result.value;
   cb.opnd1  = (HWord)&data->opnds[0].value;
   cb.opnd2  = (HWord)&data->opnds[1].value;
   cb.t_result = data->result.type;
   cb.t_opnd1  = data->opnds[0].type;
   cb.t_opnd2  = data->opnds[1].type;

   cb.num_operands = op->num_opnds;

   return (IRICB) { .kind = IRICB_iropt, .iropt = cb };
}


/* Insert a client request that will initialize VEX for IR injection */
void
valgrind_vex_init_for_iri(IRICB *cb)
{
   VALGRIND_DO_CLIENT_REQUEST_STMT(VG_USERREQ__VEX_INIT_FOR_IRI, cb, 0,0,0,0);
}


/* Insert a special opcode that will cause VEX to inject an IR stmt based
   on the information passed in the IRICB (in valgrind_vex_init_for_iri). */
static void
valgrind_vex_inject_ir(void)
{
   VALGRIND_VEX_INJECT_IR();
}


/* Execute the test under valgrind. Well, yes, we're not really executing
   it here, just preparing for it... */
void
valgrind_execute_test(const irop_t *op, test_data_t *data)
{
   if (verbose > 1)
      printf("---------- Running a test\n");

   for (unsigned i = 0; i < op->num_opnds; ++i) {
      if (verbose > 1) {
         printf("opnd #%u:   ", i);
         print_opnd(stdout, &data->opnds[i]);
         printf("\n");
      }
   }

   valgrind_vex_inject_ir();

   if (verbose > 1) {
      printf("result:    ");
      print_opnd(stdout, &data->result);
      printf("\n");
   }
}
