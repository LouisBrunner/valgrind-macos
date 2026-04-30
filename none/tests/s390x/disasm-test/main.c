/* -*- mode: C; c-basic-offset: 3; -*- */

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2024-2026  Florian Krohm

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

#include <stddef.h>           // NULL
#include <stdlib.h>           // exit, malloc
#include <stdio.h>            // vfprintf
#include <ctype.h>            // isalpha
#include <stdarg.h>           // va_list
#include <string.h>           // strchr
#include <assert.h>           // assert
#include <unistd.h>           // unlink
#include "vex.h"              // vex_init
#include "main.h"

int verbose, debug;

const char *gcc = "gcc";          // path to GCC
const char *objdump = "objdump";  // path to objdump
const char *gcc_flags = "-march=arch14";

#define CHECK_CLO(x, s) (strncmp(x, s, sizeof s - 1) == 0)

static const char usage[] =
   "Usage:\n\n"
   "disasm-test --run OPCODES\n"
   "    Generate testcases for the given opcodes and check for missed and unexpected specification exceptions.\n\n"
   "disasm-test --all\n"
   "    For all opcodes generate testcases and check for missed and unexpected specification exceptions.\n\n"
   "disasm-test --unit-test\n"
   "    Run unit tests. All other command line options are ignored.\n\n"
   "Additional options:\n"
   "    --verbose\n"
   "    --debug\n"
   "    --spec-exc        - Only generate testcases causing spec. exceptions\n"
   "    --no-spec-exc     - Do not generate testcases causing spec. exceptions\n"
   "    --exclude OPCODES - Do not generate testcases for named opcodes\n"
   "    --gcc=/path/to/gcc\n"
   "    --gcc-flags=FLAGS\n"
   "    --objdump=/path/to/objdump\n"
   "    --keep-temp - Do not remove temporary files\n"
   "    --summary   - Output test generation summary (with --all)\n"
   "    --unit-test - Run unit tests\n"
   "    --check-march=ARCH - Check whether GCC supports -march=ARCH\n"
   ;

static int  check_march_level(const char *);
static void remove_temp_files(const char *, int);
static int  opcode_has_errors(const opcode *);
static int  is_excluded_opcode(const opcode *);
static int  skip_opcode(const opcode *, int);
static test_stats run_opcode(opcode *, int);

static int keep_temp = 0;
static int summary = 0;
static const char **excluded_opcodes;
static unsigned num_excluded_opcodes;

#define GEN_SPEC_EXC    1
#define GEN_NO_SPEC_EXC 2


/* Return code: 0  No missed or unexpected specification exceptions
   Return code: 1  False missed and/or unexpected specification exceptions found */
int
main(int argc, char *argv[])
{
   int all = 0, unit_test = 0, run = 0;
   int num_to_run = 0, check_march = 0;
   int mode = 0;
   const char *to_run[argc];
   const char *arch;

   assert(sizeof(long long) == 8);

   /* Change to line buffering */
   setlinebuf(stdout);
   setlinebuf(stderr);

   /* Collect options and arguments */
   for (int i = 1; i < argc; ++i) {
      const char *clo = argv[i];

      if (CHECK_CLO(clo, "--all")) {
         all = 1;
      } else if (CHECK_CLO(clo, "--verbose")) {
         verbose = 1;
      } else if (CHECK_CLO(clo, "--debug")) {
         debug = 1;
      } else if (CHECK_CLO(clo, "--spec-exc")) {
         mode |= GEN_SPEC_EXC;
      } else if (CHECK_CLO(clo, "--no-spec-exc")) {
         mode |= GEN_NO_SPEC_EXC;
      } else if (CHECK_CLO(clo, "--summary")) {
         summary = 1;
      } else if (CHECK_CLO(clo, "--unit-test")) {
         unit_test = 1;
      } else if (CHECK_CLO(clo, "--keep-temp")) {
         keep_temp = 1;
      } else if (CHECK_CLO(clo, "--check-march=")) {
         check_march = 1;
         arch = strchr(clo, '=') + 1;
      } else if (CHECK_CLO(clo, "--help")) {
         printf("%s\n", usage);
         return 0;
      } else if (CHECK_CLO(clo, "--gcc=")) {
         gcc = strchr(clo, '=') + 1;
      } else if (CHECK_CLO(clo, "--gcc-flags=")) {
         gcc_flags = strchr(clo, '=') + 1;
      } else if (CHECK_CLO(clo, "--objdump=")) {
         objdump = strchr(clo, '=') + 1;
      } else if (CHECK_CLO(clo, "--run")) {
         run = 1;
         num_to_run = 0;
         int j;
         for (j = i + 1; j < argc; ++j) {
            if (! isalpha(argv[j][0]))
               break;
            if (get_opcode_by_name(argv[j]) == NULL)
               error("'%s' is not a recognised opcode\n", argv[j]);
            else
               to_run[num_to_run++] = argv[j];
         }
         if (num_to_run == 0)
            error("Missing opcode(s) for --run\n");
         i = j - 1;
      } else if (CHECK_CLO(clo, "--exclude")) {
         num_excluded_opcodes = 0;
         excluded_opcodes = mallock(argc * sizeof(char *));
         int j;
         for (j = i + 1; j < argc; ++j) {
            if (argv[j][0] == '-')
               break;
            excluded_opcodes[num_excluded_opcodes++] = argv[j];
         }
         if (num_excluded_opcodes == 0)
            error("Missing opcode(s) for --exclude\n");
         i = j - 1;
      } else {
         if (strncmp(clo, "--", 2) == 0)
            fatal("Invalid command line option '%s'\n", clo);
         else
            fatal("Excess arguments on command line\n");
      }
   }

   if (check_march)
      return check_march_level(arch);

   /* Check consistency of command line options */
   if (run + all + unit_test == 0)
      fatal("One of --run, --all, or --unit-test is required\n");
   if (run + all + unit_test != 1)
      fatal("At most one of --run, --all, or  --unit-test can be given\n");

   /* Nothing specified: look for both false positives and false negatives */
   if (mode == 0)
      mode = GEN_SPEC_EXC | GEN_NO_SPEC_EXC;

   vex_init();

   if (run) {
      int rc = 0;

      if (mode & GEN_SPEC_EXC) {
         unsigned num_tests = 0, num_spec_exc = 0;
         if (verbose)
            printf("Looking for missed specification exceptions\n");

         for (int i = 0; i < num_to_run; ++i) {
            const char *name = to_run[i];

            opcode *opc = get_opcode_by_name(name);  // never NULL

            test_stats stats = run_opcode(opc, /* gen-spec-exc-tests */ 1);
            num_tests    += stats.num_generated;
            num_spec_exc += stats.num_spec_exc;
         }
         rc += num_tests != num_spec_exc;
      }
      if (mode & GEN_NO_SPEC_EXC) {
         unsigned num_spec_exc = 0;
         if (verbose)
            printf("Looking for unexpected specification exceptions\n");

         for (int i = 0; i < num_to_run; ++i) {
            const char *name = to_run[i];

            opcode *opc = get_opcode_by_name(name);  // never NULL

            test_stats stats = run_opcode(opc, /* gen-spec-exc-tests */ 0);
            num_spec_exc += stats.num_spec_exc;
         }
         rc += num_spec_exc != 0;
      }
      return rc;
   }

   if (all) {
      int rc = 0;

      if (mode & GEN_SPEC_EXC) {
         unsigned num_tests = 0, num_spec_exc = 0;
         if (verbose || summary)
            printf("Looking for missed specification exceptions\n");
         for (int i = 0; i < num_opcodes; ++i) {
            opcode *opc = get_opcode_by_index(i); // never NULL

            test_stats stats = run_opcode(opc, /* gen-spec-exc-tests */ 1);
            num_tests    += stats.num_generated;
            num_spec_exc += stats.num_spec_exc;
         }
         if (verbose || summary) {
            printf("Total: %6u tests generated\n", num_tests);
            printf("Total: %6u specification exceptions\n", num_spec_exc);
         }
         rc += num_tests != num_spec_exc;
      }
      if (mode & GEN_NO_SPEC_EXC) {
         unsigned num_tests = 0, num_spec_exc = 0;
         if (verbose || summary)
            printf("Looking for unexpected specification exceptions\n");
         for (int i = 0; i < num_opcodes; ++i) {
            opcode *opc = get_opcode_by_index(i); // never NULL

            test_stats stats = run_opcode(opc, /* gen-spec-exc-tests */ 0);
            num_tests    += stats.num_generated;
            num_spec_exc += stats.num_spec_exc;
         }
         if (verbose || summary) {
            printf("Total: %6u tests generated\n", num_tests);
            printf("Total: %6u specification exceptions\n", num_spec_exc);
         }
         rc += num_spec_exc != 0;
      }
      return rc != 0;
   }

   if (unit_test)
      run_unit_tests();

   return 0;
}


static test_stats
run_opcode(opcode *opc, int gen_spec_exc_tests)
{
   test_stats stats = { 0, 0 };  // return value

   if (opcode_has_errors(opc)) {
      error("Opcode '%s' ignored due to syntax errors\n", opc->name);
   } else if (skip_opcode(opc, gen_spec_exc_tests)) {
      if (verbose)
         printf("Opcode '%s' skipped\n", opc->name);
   } else if (is_excluded_opcode(opc)) {
      if (verbose)
         printf("Opcode '%s' excluded via command line\n", opc->name);
   } else {
      unsigned num_generated = generate_tests(opc, gen_spec_exc_tests);

      stats = verify_spec_exceptions(opc, gen_spec_exc_tests);
      stats.num_generated = num_generated;

      if (! keep_temp)
         remove_temp_files(opc->name, gen_spec_exc_tests);
      release_opcode(opc);
   }
   return stats;
}


/* The GNU assembler detects certain insns that would cause a
   specification exception, namely:
   - an invalid register identifying a GPR pair is used
   - an invalid register identifying a FPR pair is used
   We do not want the assembler to complain and therefore need
   to construct testcases accordingly. */
int
asm_detects_spec_exc(const opnd *operand)
{
   assert(operand->allowed_values);

   if (operand->kind == OPND_GPR) {
      /* Check for constraint on a GPR pair. */
      int expected[8] = { 0,2,4,6,8,10,12,14 };
      if (operand->allowed_values[0] != 8) return 0;

      for (int i = 1; i <= 8; ++i) {
         if (operand->allowed_values[i] != expected[i - 1])
            return 0;
      }
      return 1;
   }

   if (operand->kind == OPND_FPR) {
      /* Check for constraint on an FPR pair. */
      int expected[8] = { 0,1,4,5,8,9,12,13 };
      if (operand->allowed_values[0] != 8) return 0;

      for (int i = 1; i <= 8; ++i) {
         if (operand->allowed_values[i] != expected[i - 1])
            return 0;
      }
      return 1;
   }

   return 0;
}


static int
skip_opcode(const opcode *opc, int gen_spec_exc_tests)
{
   if (gen_spec_exc_tests) {
      /* Looking for false negatives. I.e. insns that cause spec. exception.
         That means:
         a) opcode must have at least one constraint (necessary condition)
         b) at least one of those constraints is such that a constraint
            violation is not detected by the assembler (sufficient condition)
      */
      for (int i = 0; i < opc->num_opnds; ++i) {
         const opnd *operand = opc->opnds + i;

         if (operand->allowed_values)
            if (! asm_detects_spec_exc(operand))
               return 0;
      }
      return 1;
   } else {
      /* Looking for false positives. I.e. opcodes that should not cause
         a spec. exception. That is:
         a) opcodes without constraints
         b) opcodes with satisfied constraints */
      return 0;
   }
}


static int
check_march_level(const char *arch)
{
   const char *cmd = "%s -c -march=%s /dev/null 2> /dev/null";
   char buf[strlen(gcc) + strlen(cmd) + strlen(arch) + 10];
   sprintf(buf, cmd, gcc, arch);

   int rc = system(buf);
   return rc == 0 ? 0 : 1;
}


static void
remove_temp_files(const char *op, int gen_spec_exc_tests)
{
   char file[strlen(op) + 20];    // large enough
   static const char *suffix[] = { ".c", ".o", ".dump", ".spec-exc" };

   for (int i = 0; i < sizeof suffix / sizeof *suffix; ++i) {
      sprintf(file, "%s-%s%s", op, gen_spec_exc_tests ? "se" : "no-se",
              suffix[i]);
      unlink(file);
   }
}


static int
is_excluded_opcode(const opcode *opc)
{
   if (excluded_opcodes) {
      for (int i = 0; i < num_excluded_opcodes; ++i)
         if (strcmp(opc->name, excluded_opcodes[i]) == 0)
            return 1;
   }
   return 0;
}


/* A few convenience utilities */
void
error(const char *fmt, ...)
{
   va_list args;
   va_start(args, fmt);
   fprintf(stderr, "error: ");
   vfprintf(stderr, fmt, args);
   va_end(args);
}


void
fatal(const char *fmt, ...)
{
   va_list args;
   va_start(args, fmt);
   vfprintf(stderr, fmt, args);
   va_end(args);
   exit(EXIT_FAILURE);
}


void *
mallock(unsigned n)
{
   void *p = malloc(n);

   if (p == NULL)
      fatal("malloc failed\n");
   return p;
}


char *
strsave(const char *s)
{
   return strcpy(mallock(strlen(s) + 1), s);
}


char *
strnsave(const char *s, unsigned len)
{
   char *p = memcpy(mallock(len + 1), s, len);

   p[len] = '\0';
   return p;
}


/* Return 1, if the given opcode has at least one invalid operand.
   This indicates that there were parse errors earlier. */
static int
opcode_has_errors(const opcode *opc)
{
   const opnd *opnds = opc->opnds;

   for (int i = 0; i < opc->num_opnds; ++i) {
      if (opnds[i].kind == OPND_INVALID)
         return 1;
   }
   return 0;
}
