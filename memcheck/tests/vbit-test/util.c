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

#include <stdio.h>     // fprintf
#include <stdlib.h>    // exit
#include <assert.h>    // assert
#if defined(__APPLE__)
#include <machine/endian.h>
#define __BYTE_ORDER    BYTE_ORDER
#define __LITTLE_ENDIAN LITTLE_ENDIAN
#elif defined(__sun)
#define __LITTLE_ENDIAN 1234
#define __BIG_ENDIAN    4321
#  if defined(_LITTLE_ENDIAN)
#  define __BYTE_ORDER    __LITTLE_ENDIAN
#  else
#  define __BYTE_ORDER    __BIG_ENDIAN
#  endif
#elif defined(__linux__)
#include <endian.h>
#else
#define __BYTE_ORDER    BYTE_ORDER
#define __LITTLE_ENDIAN LITTLE_ENDIAN
#include <sys/endian.h>
#endif
#include <inttypes.h>
#include "vtest.h"

#include "memcheck.h" // VALGRIND_MAKE_MEM_DEFINED


/* Something bad happened. Cannot continue. */
void __attribute__((noreturn))
panic(const char *string)
{
   fprintf(stderr, "*** OOPS: %s\n", string);
   exit(1);
}


/* Issue a complaint because the V-bits of the result of an operation
   differ from what was expected. */
void
complain(const irop_t *op, const test_data_t *data, vbits_t expected)
{
   fprintf(stderr, "*** Incorrect result for operator %s\n", op->name);
   
   int num_operands = get_num_operands(op->op);

   for (unsigned i = 0; i < num_operands; ++i) {
      fprintf(stderr, "    opnd %u:  ", i);
      print_opnd(stderr, &data->opnds[i]);
      fprintf(stderr, "\n");
   }
   fprintf(stderr, "    result:  ");
   print_opnd(stderr, &data->result);
   fprintf(stderr, "\n");
   fprintf(stderr, "    expect:  vbits = ");
   print_vbits(stderr, expected);
   fprintf(stderr, "\n");
}


static void
print_value(FILE *fp, value_t val, unsigned num_bits)
{
   switch (num_bits) {
   case 1:  fprintf(fp, "%02x",   val.u8);  break;
   case 8:  fprintf(fp, "%02x",   val.u8);  break;
   case 16: fprintf(fp, "%04x",   val.u16); break;
   case 32: fprintf(fp, "%08x",   val.u32); break;
   case 64: fprintf(fp, "%016"PRIx64, val.u64); break;
   case 128:
      if (__BYTE_ORDER == __LITTLE_ENDIAN) {
         fprintf(fp, "%016"PRIx64, val.u128[1]);
         fprintf(fp, "%016"PRIx64, val.u128[0]);
      } else {
         fprintf(fp, "%016"PRIx64, val.u128[0]);
         fprintf(fp, "%016"PRIx64, val.u128[1]);
      }
      break;
   case 256:
      if (__BYTE_ORDER == __LITTLE_ENDIAN) {
         fprintf(fp, "%016"PRIx64, val.u256[3]);
         fprintf(fp, "%016"PRIx64, val.u256[2]);
         fprintf(fp, "%016"PRIx64, val.u256[1]);
         fprintf(fp, "%016"PRIx64, val.u256[0]);
      } else {
         fprintf(fp, "%016"PRIx64, val.u256[0]);
         fprintf(fp, "%016"PRIx64, val.u256[1]);
         fprintf(fp, "%016"PRIx64, val.u256[2]);
         fprintf(fp, "%016"PRIx64, val.u256[3]);
      }
      break;
  default:
      panic(__func__);
   }
}


void
print_opnd(FILE *fp, const opnd_t *opnd)
{
   fprintf(fp, "vbits = ");
   print_vbits(fp, opnd->vbits);
   /* The value itself might be partially or fully undefined, so take a
      copy, paint the copy as defined, and print the copied value.  This is
      so as to avoid error messages from Memcheck, which are correct, but
      confusing. */
   volatile value_t value_copy = opnd->value;
   VALGRIND_MAKE_MEM_DEFINED(&value_copy, sizeof(value_copy));
   fprintf(fp, "   value = ");
   print_value(fp, value_copy, opnd->vbits.num_bits);
}


static int
is_floating_point_type(IRType type)
{
   switch (type) {
   case Ity_F32:
   case Ity_F64:
   case Ity_F128:
   case Ity_D32:
   case Ity_D64:
   case Ity_D128:
      return 1;

   default:
      return 0;
   }
}


int
is_floating_point_op_with_rounding_mode(IROp op)
{
   IRType t_dst, t_arg1, t_arg2, t_arg3, t_arg4;

   typeof_primop(op, &t_dst, &t_arg1, &t_arg2, &t_arg3, &t_arg4);

   // A unary operator cannot have a rounding mode
   if (t_arg2 == Ity_INVALID) return 0;

   if (is_floating_point_type(t_dst)  ||
       is_floating_point_type(t_arg1) || 
       is_floating_point_type(t_arg2) || 
       is_floating_point_type(t_arg3) || 
       is_floating_point_type(t_arg4)) {
      // Rounding mode, if present, is the 1st operand
      return t_arg1 == Ity_I32;
   }
   return 0;
}


/* Return the number of operands for which input values can
   be freely chosen. For floating point ops, the rounding mode
   is not counted here, as it is restricted. */
int
get_num_operands(IROp op)
{
   IRType unused, t1, t2, t3, t4;

   typeof_primop(op, &unused, &t1, &t2, &t3, &t4);

   int num_operands = 4;
   if (t4 == Ity_INVALID) num_operands = 3;
   if (t3 == Ity_INVALID) num_operands = 2;
   if (t2 == Ity_INVALID) num_operands = 1;

   if (is_floating_point_op_with_rounding_mode(op))
      -- num_operands;

   return num_operands;
}


unsigned
sizeof_irtype(IRType ty)
{
   return sizeofIRType(ty);
}


void
typeof_primop(IROp op, IRType *t_dst, IRType *t_arg1, IRType *t_arg2, 
              IRType *t_arg3, IRType *t_arg4)
{
   return typeOfPrimop(op, t_dst, t_arg1, t_arg2, t_arg3, t_arg4);
}
