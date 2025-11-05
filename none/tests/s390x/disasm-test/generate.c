/* -*- mode: C; c-basic-offset: 3; -*- */

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2024-2025  Florian Krohm

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 3 of the
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
#include <stdlib.h>    // system
#include <string.h>    // strlen
#include <stdarg.h>    // va_list
#include <ctype.h>     // isdigit
#include <assert.h>    // assert
#include "main.h"      // error
#include "objdump.h"   // MARK

// FIXME: if more than one VR or GPR (non-base, non-index) are used in
// an opcode use different register! So we can recognise a mixup in
// register order. E.g. vctz  %v2,%v2,3 will not allow to detect whether
// the two registers was mixed up.
static unsigned num_tests;   // # generated tests

static void run_cmd(const char *, ...);


static const char *
gpr_operand(unsigned regno)
{
   static const char *gprs[] = {
      "%r0", "%r1", "%r2",  "%r3",  "%r4",  "%r5",  "%r6",  "%r7",
      "%r8", "%r9", "%r10", "%r11", "%r12", "%r13", "%r14", "%r15"
   };

   return gprs[regno];
}


static const char *
ar_operand(unsigned regno)
{
   static const char *ars[] = {
      "%a0", "%a1", "%a2",  "%a3",  "%a4",  "%a5",  "%a6",  "%a7",
      "%a8", "%a9", "%a10", "%a11", "%a12", "%a13", "%a14", "%a15"
   };

   return ars[regno];
}


static const char *
fpr_operand(unsigned regno)
{
   static const char *fprs[] = {
      "%f0", "%f1", "%f2",  "%f3",  "%f4",  "%f5",  "%f6",  "%f7",
      "%f8", "%f9", "%f10", "%f11", "%f12", "%f13", "%f14", "%f15"
   };

   return fprs[regno];
}


static const char *
vr_operand(unsigned regno)
{
   static const char *vrs[] = {
      "%v0",  "%v1",  "%v2",  "%v3",  "%v4",  "%v5",  "%v6",  "%v7",
      "%v8",  "%v9",  "%v10", "%v11", "%v12", "%v13", "%v14", "%v15",
      "%v16", "%v17", "%v18", "%v19", "%v20", "%v21", "%v22", "%v23",
      "%v24", "%v25", "%v26", "%v27", "%v28", "%v29", "%v30", "%v31"
   };

   return vrs[regno];
}


static unsigned
random_reg(opnd_t reg_kind, int r0_allowed)
{
   unsigned num_regs = reg_kind == OPND_VR ? 32 : 16;
   unsigned regno;

   if (r0_allowed) {
      regno = rand() % num_regs;
   } else {
      do {
         regno = rand() % num_regs;
      } while (regno == 0);
   }

   return regno;
}


#if 0
/* These functions are currently unused. But may become useful in
   alternate test generation strategies that use random values instead
   of hardwired interesting ones. */

static unsigned
random_uint(unsigned num_bits)
{
   assert(num_bits <= 32);

   long long val = rand();
   return val % (1LL << num_bits);
}


static int
random_sint(unsigned num_bits)
{
   assert(num_bits <= 32);

   static int sign = 1;

   long long val = rand();   // positive value
   int value = val % (1LL << (num_bits - 1));

   /* alternate */
   if (sign == -1)
      value = -value;
   sign = -sign;
   return value;
}


static unsigned
d12_value(void)
{
   return random_uint(12);
}


static int
d20_value(void)
{
   return random_sint(20);
}


static unsigned
uint_value(unsigned num_bits)
{
   if (num_bits > 32)
      fatal("integer operand > 32 bits not supported\n");
   return random_uint(num_bits);
}


static int
sint_value(unsigned num_bits)
{
   if (num_bits > 32)
      fatal("integer operand > 32 bits not supported\n");
   return random_sint(num_bits);
}
#endif

/* MASK is a bitvector. For an e.g. GPR rk the k'th bit will be set. The
   function returns a register number which has not been used and
   adjusts the bitvector. */
static unsigned
unique_reg(opnd_t reg_kind, unsigned regno, unsigned *mask)
{
   unsigned num_regs = reg_kind == OPND_VR ? 32 : 16;
   assert(regno < num_regs);
   assert(*mask != ~0U);   // Paranoia: avoid infinite loop

   unsigned bit = 1 << regno;
   while (*mask & bit) {
      regno = random_reg(reg_kind, /* r0_allowed */1);
      bit = 1 << regno;
   }
   *mask |= bit;
   return regno;
}


/* Field */
typedef struct {
   const opnd *operand;  // the operand to which this field belongs
   int is_displacement;  // only relevant for OPND_D12/20[X]B operands
   int is_length;        // only relevant for OPND_D12LB operands
   int is_vr;            // only relevant for OPND_D12VB operands
   long long assigned_value;
} field;

static void choose_reg_and_iterate(FILE *, const opcode *, const opnd *,
                                   field [], unsigned);

/* Write out a single ASM statement for OPC. */
static void
write_asm_stmt(FILE *fp, const opcode *opc, const field fields[])
{
   fprintf(fp, "  asm volatile(\"%s ", opc->name);

   unsigned gpr_mask, vr_mask, ar_mask, fpr_mask, regno;
   int inc;
   int needs_comma = 0;

   gpr_mask = vr_mask = ar_mask = fpr_mask = 0;
   for (int i = 0; i < opc->num_fields; i += inc) {
      const opnd *operand = fields[i].operand;

      inc = 1; // for most operand kinds

      if (needs_comma++)
         fputc(',', fp);
      switch (operand->kind) {
      case OPND_GPR:
         regno = fields[i].assigned_value;
         if (! operand->allowed_values)
            regno = unique_reg(operand->kind, regno, &gpr_mask);
         fprintf(fp, "%s", gpr_operand(regno));
         break;
      case OPND_VR:
         regno = fields[i].assigned_value;
         if (! operand->allowed_values)
            regno = unique_reg(operand->kind, regno, &vr_mask);
         fprintf(fp, "%s", vr_operand(regno));
         break;
      case OPND_AR:
         regno = fields[i].assigned_value;
         if (! operand->allowed_values)
            regno = unique_reg(operand->kind, regno, &ar_mask);
         fprintf(fp, "%s", ar_operand(regno));
         break;
      case OPND_FPR:
         regno = fields[i].assigned_value;
         if (! operand->allowed_values)
            regno = unique_reg(operand->kind, regno, &fpr_mask);
         fprintf(fp, "%s", fpr_operand(regno));
         break;
      case OPND_D12XB:
      case OPND_D20XB: {
         long long d = fields[i].assigned_value;
         const char *x = gpr_operand(fields[i + 1].assigned_value);
         const char *b = gpr_operand(fields[i + 2].assigned_value);
         fprintf(fp, "%lld(%s,%s)", d, x, b);
         inc = 3;
         break;
      }
      case OPND_D12VB: {
         long long d = fields[i].assigned_value;
         const char *v = vr_operand(fields[i + 1].assigned_value);
         const char *b = gpr_operand(fields[i + 2].assigned_value);
         fprintf(fp, "%lld(%s,%s)", d, v, b);
         inc = 3;
         break;
      }
      case OPND_D12LB: {
         long long d = fields[i].assigned_value;
         unsigned  l = fields[i + 1].assigned_value;
         const char *b = gpr_operand(fields[i + 2].assigned_value);
         fprintf(fp, "%lld(%u,%s)", d, l + 1, b);
         inc = 3;
         break;
      }
      case OPND_D12B:
      case OPND_D20B: {
         long long d = fields[i].assigned_value;
         const char *b = gpr_operand(fields[i + 1].assigned_value);
         fprintf(fp, "%lld(%s)", d, b);
         inc = 2;
         break;
      }
      case OPND_MASK:
      case OPND_SINT:
      case OPND_UINT:
         fprintf(fp, "%lld", fields[i].assigned_value);
         break;
      case OPND_PCREL:
         fprintf(fp, "%lld*2", fields[i].assigned_value);
         break;
      default:
         assert(0);
      }
   }
   fprintf(fp, "\");\n");

   ++num_tests;
}


/* IX identifies the element of the FIELDS array to which a value
   will be assigned in this iteration. */
static void
iterate(FILE *fp, const opcode *opc, field fields[], unsigned ix)
{
   /* All fields are assigned. Write out the asm stmt */
   if (ix == opc->num_fields) {
      write_asm_stmt(fp, opc, fields);
      return;
   }

   field *f = fields + ix;
   const opnd *operand = f->operand;

   switch (operand->kind) {
   case OPND_GPR:
      if (operand->name[0] == 'b' || operand->name[0] == 'x') {
         /* Choose r0 */
         f->assigned_value = 0;
         iterate(fp, opc, fields, ix + 1);
         /* Choose any GPR other than r0 */
         f->assigned_value = random_reg(operand->kind, /* r0_allowed */ 0);
         iterate(fp, opc, fields, ix + 1);
      } else {
         choose_reg_and_iterate(fp, opc, operand, fields, ix);
      }
      break;

   case OPND_AR:
   case OPND_FPR:
   case OPND_VR:
      choose_reg_and_iterate(fp, opc, operand, fields, ix);
      break;

   case OPND_D12B:
   case OPND_D12XB:
   case OPND_D12LB:
   case OPND_D12VB:
      if (f->is_displacement) {
         /* Choose these interesting values */
         static const long long values[] = { 0, 1, 2, 0xfff };

         for (int i = 0; i < sizeof values / sizeof *values; ++i) {
            f->assigned_value = values[i];
            iterate(fp, opc, fields, ix + 1);
         }
      } else if (f->is_length) {
         /* Choose these interesting values */
         static const long long values[] = { 0, 1, 2, 255 };

         for (int i = 0; i < sizeof values / sizeof *values; ++i) {
            f->assigned_value = values[i];
            iterate(fp, opc, fields, ix + 1);
         }
      } else if (f->is_vr) {
         /* v0 is not special AFAICT */
         f->assigned_value = random_reg(OPND_VR, /* r0_allowed */ 11);
         iterate(fp, opc, fields, ix + 1);
      } else {
         /* Base or index register */
         /* Choose r0 */
         f->assigned_value = 0;
         iterate(fp, opc, fields, ix + 1);
         /* Choose any GPR other than r0 */
         f->assigned_value = random_reg(OPND_GPR, /* r0_allowed */ 0);
         iterate(fp, opc, fields, ix + 1);
      }
      break;

   case OPND_D20B:
   case OPND_D20XB:
      if (f->is_displacement) {
         /* Choose these interesting values */
         static const long long values[] = {
            0, 1, 2, -1, -2, 0x7ffff, -0x80000
         };

         for (int i = 0; i < sizeof values / sizeof *values; ++i) {
            f->assigned_value = values[i];
            iterate(fp, opc, fields, ix + 1);
         }
      } else {
         /* base or index register */
         f->assigned_value = 0;
         iterate(fp, opc, fields, ix + 1);
         f->assigned_value = random_reg(OPND_GPR, /* r0_allowed */ 0);
         iterate(fp, opc, fields, ix + 1);
      }
      break;

   case OPND_SINT:
   case OPND_PCREL:
      if (operand->allowed_values == NULL) {
         /* No constraint: Choose these interesting values */
         const long long values[] = {
            0, 1, 2, -1, -2, (1LL << (operand->num_bits - 1)) - 1,
            -(1LL << (operand->num_bits - 1))
         };

         for (int i = 0; i < sizeof values / sizeof *values; ++i) {
            f->assigned_value = values[i];
            iterate(fp, opc, fields, ix + 1);
         }
      } else {
         /* Constraint. Choose only allowed values */
         unsigned num_val = operand->allowed_values[0];
         for (int i = 1; i <= num_val; ++i) {
            f->assigned_value = operand->allowed_values[i];
            iterate(fp, opc, fields, ix + 1);
         }
      }
      break;

   case OPND_UINT:
      if (operand->allowed_values == NULL) {
         /* No constraint: Choose these interesting values */
         const long long values[] = {
            0, 1, 2, (1LL << operand->num_bits) - 1
         };

         for (int i = 0; i < sizeof values / sizeof *values; ++i) {
            f->assigned_value = values[i];
            iterate(fp, opc, fields, ix + 1);
         }
      } else {
         /* Constraint. Choose only allowed values */
         unsigned num_val = operand->allowed_values[0];
         for (int i = 1; i <= num_val; ++i) {
            f->assigned_value = operand->allowed_values[i];
            iterate(fp, opc, fields, ix + 1);
         }
      }
      break;

   case OPND_MASK:
      if (operand->allowed_values == NULL) {
         /* No constraint. Choose all possible values */
         unsigned maxval = (1u << operand->num_bits) - 1;
         for (int val = 0; val <= maxval; ++val) {
            f->assigned_value = val;
            iterate(fp, opc, fields, ix + 1);
         }
      } else {
         /* Constraint. Choose only allowed values */
         unsigned num_val = operand->allowed_values[0];
         for (int i = 1; i <= num_val; ++i) {
            f->assigned_value = operand->allowed_values[i];
            iterate(fp, opc, fields, ix + 1);
         }
      }
      break;

      case OPND_INVALID:
      default:
         assert(0);
   }
}


static void
generate(FILE *fp, const opcode *opc)
{
   /* Array of opcode fields to which we need to assign values. */
   field fields[opc->num_fields];
   field *f;

   int ix = 0;
   for (int i = 0; i < opc->num_opnds; ++i) {
      const opnd *operand = opc->opnds + i;

      switch (operand->kind) {
      case OPND_GPR:
      case OPND_VR:
      case OPND_AR:
      case OPND_FPR:
      case OPND_SINT:
      case OPND_UINT:
      case OPND_PCREL:
      case OPND_MASK:
         f = fields + ix++;
         f->operand = operand;
         break;

      case OPND_D12XB:
      case OPND_D20XB:
         for (int j = 1; j <= 3; ++j) {
            f = fields + ix++;
            f->operand = operand;
            f->is_displacement = j == 1;
            f->is_length = 0;
            f->is_vr = 0;
         }
         break;

      case OPND_D12B:
      case OPND_D20B:
         for (int j = 1; j <= 2; ++j) {
            f = fields + ix++;
            f->operand = operand;
            f->is_displacement = j == 1;
            f->is_length = 0;
            f->is_vr = 0;
         }
         break;

      case OPND_D12LB:
         for (int j = 1; j <= 3; ++j) {
            f = fields + ix++;
            f->operand = operand;
            f->is_displacement = j == 1;
            f->is_length = j == 2;
            f->is_vr = 0;
         }
         break;

      case OPND_D12VB:
         for (int j = 1; j <= 3; ++j) {
            f = fields + ix++;
            f->operand = operand;
            f->is_displacement = j == 1;
            f->is_length = 0;
            f->is_vr = j == 2;
         }
         break;

      case OPND_INVALID:
      default:
         assert(0);
      }
   }
   assert(ix == opc->num_fields);

   iterate(fp, opc, fields, 0);
}


unsigned
generate_tests(const opcode *opc)
{
   srand(42);

   if (verbose)
      printf("...generating testcases for '%s'\n", opc->name);

   num_tests = 0;

   char file[strlen(opc->name) + 3];
   sprintf(file, "%s.c", opc->name);

   FILE *fp = fopen(file, "w");
   if (fp == NULL) {
      error("%s: fopen failed\n", file);
      return 0;
   }

   fprintf(fp, "void\n");
   fprintf(fp, "main(void)\n");
   fprintf(fp, "{\n");
   fprintf(fp, "  asm volatile(\"%s\");\n", MARK);
   generate(fp, opc);
   fprintf(fp, "  asm volatile(\"%s\");\n", MARK);
   fprintf(fp, "}\n");
   fclose(fp);

   if (verbose)
      printf("...%u testcases generated for '%s'\n", num_tests,
             opc->name);

   run_cmd("%s -c %s %s.c", gcc, gcc_flags, opc->name);
   run_cmd("%s --disassemble=%s %s.o > %s.dump", objdump, FUNCTION,
           opc->name, opc->name);

   return num_tests;
}


static void
choose_reg_and_iterate(FILE *fp, const opcode *opc, const opnd *operand,
                       field fields[], unsigned ix)
{
   field *f = fields + ix;

   if (operand->allowed_values == NULL) {
      /* No constraint. Pick register at random. */
      f->assigned_value = random_reg(operand->kind, /* r0_allowed */ 1);
      iterate(fp, opc, fields, ix + 1);
   } else {
      /* Constraint. Choose only allowed values */
      unsigned num_val = operand->allowed_values[0];
      for (int i = 1; i <= num_val; ++i) {
         f->assigned_value = operand->allowed_values[i];
         iterate(fp, opc, fields, ix + 1);
      }
   }
}


static void
run_cmd(const char *fmt, ...)
{
   va_list args;
   va_start(args, fmt);
   int need = vsnprintf((char []){ 0 }, 1, fmt, args);
   va_end(args);

   char cmd[need + 1];
   va_list args2;
   va_start(args2, fmt);
   vsnprintf(cmd, sizeof cmd, fmt, args2);
   va_end(args2);

   if (debug)
      printf("Running command: %s\n", cmd);

   int rc = system(cmd);

   if (rc != 0)
      error("Command '%s' failed\n", cmd);
}
