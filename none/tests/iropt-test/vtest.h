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

#ifndef VTEST_H
#define VTEST_H

/* Main header file for the iropt tester */

#include <stdint.h>   // uint64_t
#include <stdio.h>    // FILE
#include "libvex.h"   // IROp

/* Everything we want to know about an IROp */
typedef struct {
   const char *name;
   IROp op;
   IRType result_type;
   unsigned num_opnds;
   IRType opnd1_type;
   IRType opnd2_type;
   unsigned enabled_arch;
} irop_t;


/* The maximum number of input operands */
#define MAX_OPERANDS 2

/* An operand of an IROp (also used for the result) */
typedef struct {
   IRType   type;
   uint64_t value;
} opnd_t;


/* Carries the data needed to execute and evaluate a test. I.e.
   inputs and result. */
typedef struct {
   opnd_t result_fold;
   opnd_t result_nofold;
   opnd_t opnds[MAX_OPERANDS];
} test_data_t;


/* Convenience macros */
#define NUM_EL(x) (sizeof x / sizeof *(x))

/* Sign-extend VAL which is NUM_BITS wide to 64 bit */
#define sign_extend(val, num_bits) \
        ((int64_t)((val) << (64 - (num_bits))) >> (64 - (num_bits)))


/* Function prototypes */
void print_value(FILE *, uint64_t, unsigned);

void test_unary_op(const irop_t *, test_data_t *);
void test_binary_op(const irop_t *, test_data_t *);

void valgrind_vex_init_for_iri(IRICB *);
void valgrind_execute_test(const irop_t *, test_data_t *, uint64_t);

IRICB *new_iricb(const irop_t *, test_data_t *);

void panic(const char *, ...) __attribute__((noreturn));

unsigned bitsof_irtype(IRType);
uint64_t get_random_value(IRType);
const uint64_t *get_selected_values(IRType, unsigned *);

/* Exported variables */
extern int verbose;
extern unsigned num_random_tests;

#endif // VTEST_H
