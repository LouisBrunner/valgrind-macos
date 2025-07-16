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


uint64_t
get_random_value(IRType type)
{
   uint64_t val = rand();

   switch (type) {
   case Ity_I1:  return val & 0x1;
   case Ity_I8:  return val & UINT8_MAX;
   case Ity_I16: return val & UINT16_MAX;
   case Ity_I32: return val & UINT32_MAX;
   case Ity_I64:
      /* Note, that RAND_MAX == INT32_MAX. Therefore, simply concatenating
         two rand() values would never produce a value with MSB == 1 */
      val <<= (32 + 1);
      val |= rand() << 1;
      val |= rand() & 0x1;
      return val;

   default:
      panic(__func__);
   }
}


const uint64_t *
get_selected_values(IRType type, unsigned *num_val)
{
   static const uint64_t values_1bit[]  = { 0, 1 };
   static const uint64_t values_8bit[]  = { 0, 1, 2,
      UINT8_MAX  - 1, UINT8_MAX };
   static const uint64_t values_16bit[] = { 0, 1, 2,
      UINT8_MAX  - 1, UINT8_MAX, UINT8_MAX + 1,
      UINT16_MAX - 1, UINT16_MAX };
   static const uint64_t values_32bit[] = { 0, 1, 2,
      UINT8_MAX  - 1, UINT8_MAX,  UINT8_MAX  + 1,
      UINT16_MAX - 1, UINT16_MAX, UINT16_MAX + 1,
      UINT32_MAX - 1, UINT32_MAX };
   static const uint64_t values_64bit[] = { 0, 1, 2,
      UINT8_MAX  - 1, UINT8_MAX,  UINT8_MAX  + 1,
      UINT16_MAX - 1, UINT16_MAX, UINT16_MAX + 1,
      UINT32_MAX - 1, UINT32_MAX, UINT32_MAX + 1,
      UINT64_MAX - 1, UINT64_MAX };

   switch (type) {
   case Ity_I1:  *num_val = NUM_EL(values_1bit);  return values_1bit;
   case Ity_I8:  *num_val = NUM_EL(values_8bit);  return values_8bit;
   case Ity_I16: *num_val = NUM_EL(values_16bit); return values_16bit;
   case Ity_I32: *num_val = NUM_EL(values_32bit); return values_32bit;
   case Ity_I64: *num_val = NUM_EL(values_64bit); return values_64bit;
   default:
      panic(__func__);
   }
}
