/* -*- mode: C; c-basic-offset: 3; -*- */

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2024-2025  Florian Krohm

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

#ifndef MAIN_H
#define MAIN_H

/* The various kinds of operands. */
typedef enum {
   OPND_GPR,
   OPND_VR,
   OPND_AR,
   OPND_FPR,
   OPND_D12LB,
   OPND_D12XB,
   OPND_D12VB,
   OPND_D20XB,
   OPND_D12B,
   OPND_D20B,
   OPND_SINT,
   OPND_UINT,
   OPND_MASK,
   OPND_PCREL,
   OPND_INVALID
} opnd_t;

/* An operand */
typedef struct {
   char    *name;
   opnd_t   kind;
   unsigned num_bits;
   int      is_unsigned;
   // NULL = no values specified. Otherwise, values[0] == #values
   // and values[1..#values] are the values.
   long long *allowed_values;
} opnd;

/* An opcode */
typedef struct {
   char *name;
   opnd *opnds;
   unsigned num_opnds;
   /* When generating a testcase this is the number of fields we
      need to assign a value to */
   unsigned num_fields;
} opcode;

typedef struct {
   unsigned num_verified;
   unsigned num_mismatch;
   unsigned num_spec_exc;
} verify_stats;

__attribute__((format(printf, 1, 2)))
void error(const char *, ...);
__attribute__((noreturn)) __attribute__((format(printf, 1, 2)))
void fatal(const char *, ...);

verify_stats verify_disassembly(const char *);
unsigned generate_tests(const opcode *);
opcode  *get_opcode_by_name(const char *);
opcode  *get_opcode_by_index(unsigned);
void     release_opcode(opcode *);
void     run_unit_tests(void);

void *mallock(unsigned);
char *strsave(const char *);
char *strnsave(const char *, unsigned);

extern int verbose;
extern int debug;
extern int show_spec_exc;
extern int show_miscompares;
extern unsigned num_opcodes;
extern const char *gcc;
extern const char *gcc_flags;
extern const char *objdump;

#endif // MAIN_H
