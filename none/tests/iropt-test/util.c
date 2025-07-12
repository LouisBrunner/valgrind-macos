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

#include <stdio.h>     // fprintf
#include <stdlib.h>    // exit
#include <stdarg.h>    // va_list
#include <inttypes.h>  // PRIx...
#include "vtest.h"


/* Something bad happened. Cannot continue. */
void __attribute__((noreturn))
panic(const char *fmt, ...)
{
   va_list args;

   va_start(args, fmt);
   fprintf(stderr, "*** OOPS: ");
   vfprintf(stderr, fmt, args);
   fputc('\n', stderr);
   va_end(args);
   exit(1);
}


/* Issue a complaint because the result of an operation differs from what
   was expected. */
void
complain(const irop_t *op, const test_data_t *data, uint64_t expected)
{
   fprintf(stderr, "*** Incorrect result for operator %s\n", op->name);

   for (unsigned i = 0; i < op->num_opnds; ++i) {
      fprintf(stderr, "    opnd %u:  ", i);
      print_opnd(stderr, &data->opnds[i]);
      fprintf(stderr, "\n");
   }
   fprintf(stderr, "    result:  ");
   print_opnd(stderr, &data->result);
   fprintf(stderr, "\n");
   fprintf(stderr, "    expect:  ");
   print_value(stderr, expected, bitsof_irtype(op->result_type));
   fprintf(stderr, "\n");
}


void
print_value(FILE *fp, uint64_t val, unsigned num_bits)
{
   switch (num_bits) {
   case 1:  fprintf(fp, "%01"  PRIx64, val); break;
   case 8:  fprintf(fp, "%02"  PRIx64, val); break;
   case 16: fprintf(fp, "%04"  PRIx64, val); break;
   case 32: fprintf(fp, "%08"  PRIx64, val); break;
   case 64: fprintf(fp, "%016" PRIx64, val); break;
   case 128:
   case 256:
      /* fall through */
   default:
      panic("%s: num_bits = %u", __func__, num_bits);
   }
}


void
print_opnd(FILE *fp, const opnd_t *opnd)
{
   fprintf(fp, "value = ");
   print_value(fp, opnd->value, bitsof_irtype(opnd->type));
}


unsigned
bitsof_irtype(IRType ty)
{
   switch (ty) {
   case Ity_I1:  return 1;
   case Ity_I8:  return 8;
   case Ity_I16: return 16;
   case Ity_I32: return 32;
   case Ity_I64: return 64;
   default:
      panic(__func__);
   }
}
