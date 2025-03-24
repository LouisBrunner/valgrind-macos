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

#include <stddef.h>           // NULL
#include <stdlib.h>           // exit, malloc
#include <stdio.h>            // vfprintf
#include <stdarg.h>           // va_list
#include <string.h>           // strchr
#include <assert.h>           // assert
#include <unistd.h>           // unlink
#include "vex.h"              // vex_init
#include "main.h"

int verbose, debug, show_spec_exc, show_miscompares;
int d12_val, d20_val;
long long uint_val, sint_val;
int d12_val_specified, d20_val_specified;
int uint_val_specified, sint_val_specified;

const char *gcc = "gcc";          // path to GCC
const char *objdump = "objdump";  // path to objdump
const char *gcc_flags = "-c -march=arch14";

#define CHECK_CLO(x, s) (strncmp(x, s, sizeof s - 1) == 0)

static const char usage[] =
   "Usage:\n\n"
   "disasm-test --generate OPCODES\n"
   "    Generate testcases for the given opcodes and prepare objdump files.\n\n"
   "disasm-test --verify FILES\n"
   "    Read specified objdump files and compare with VEX disassembly.\n\n"
   "disasm-test --run OPCODES\n"
   "    Generate testcases for the given opcodes and compare the disassembly.\n\n"
   "disasm-test --all\n"
   "    For all opcodes generate testcases and compare the disassembly.\n\n"
   "disasm-test --unit-test\n"
   "    Run unit tests. All other command line options are ignored.\n\n"
   "Additional options:\n"
   "    --verbose\n"
   "    --debug\n"
   "    --gcc=/path/to/gcc\n"
   "    --gcc-flags=FLAGS\n"
   "    --objdump=/path/to/objdump\n"
   "    --d12=INT   - Use INT as value for d12 offsets\n"
   "    --d20=INT   - Use INT as value for d20 offsets\n"
   "    --sint=INT  - Use INT as value for signed integer fields\n"
   "    --uint=INT  - Use INT as value for unsigned integer fields\n"
   "    --keep-temp - Do not remove temporary files\n"
   "    --summary   - Output test generation summary (with --all)\n"
   "    --unit-test - Run unit tests\n"
   "    --show-spec-exc - Show insns causing specification exceptions\n"
   "    --no-show-miscompares - Do not show disassembly miscompares\n"
   ;

static long long get_clo_value(const char *, const char *, long long,
                               long long);
static void remove_temp_files(const char *);
static int  opcode_has_errors(const opcode *);

static int keep_temp = 0;
static int summary = 0;


/* Return code: 0  no disassembly mismatches
   Return code: 1  at least one disassembly mismatch

   Specification exceptions do not influence the return code. */
int
main(int argc, char *argv[])
{
   int all = 0, verify = 0, generate = 0, unit_test = 0;
   int num_clargs = 0;
   int run = 0;
   const char *clargs[argc];

   assert(sizeof(long long) == 8);

   /* Change to line buffering */
   setlinebuf(stdout);
   setlinebuf(stderr);

   show_miscompares = 1;

   /* Collect options and arguments */
   for (int i = 1; i < argc; ++i) {
      const char *clo = argv[i];

      if (CHECK_CLO(clo, "--verify")) {
         verify = 1;
      } else if (CHECK_CLO(clo, "--generate")) {
         generate = 1;
      } else if (CHECK_CLO(clo, "--all")) {
         all = 1;
      } else if (CHECK_CLO(clo, "--verbose")) {
         verbose = 1;
      } else if (CHECK_CLO(clo, "--debug")) {
         debug = 1;
      } else if (CHECK_CLO(clo, "--summary")) {
         summary = 1;
      } else if (CHECK_CLO(clo, "--unit-test")) {
         unit_test = 1;
      } else if (CHECK_CLO(clo, "--show-spec-exc")) {
         show_spec_exc = 1;
      } else if (CHECK_CLO(clo, "--no-show-miscompares")) {
         show_miscompares = 0;
      } else if (CHECK_CLO(clo, "--keep-temp")) {
         keep_temp = 1;
      } else if (CHECK_CLO(clo, "--run")) {
         run = 1;
      } else if (CHECK_CLO(clo, "--help")) {
         printf("%s\n", usage);
         return 0;
      } else if (CHECK_CLO(clo, "--gcc=")) {
         gcc = strchr(clo, '=') + 1;
      } else if (CHECK_CLO(clo, "--gcc-flags=")) {
         gcc_flags = strchr(clo, '=') + 1;
      } else if (CHECK_CLO(clo, "--objdump=")) {
         objdump = strchr(clo, '=') + 1;
      } else if (CHECK_CLO(clo, "--d12=")) {
         d12_val = get_clo_value(clo, "d12", 0, 0xfff);
      } else if (CHECK_CLO(clo, "--d20=")) {
         d20_val = get_clo_value(clo, "d20", -0x80000, 0x7ffff);
      } else if (CHECK_CLO(clo, "--sint=")) {
         /* Integer field is restricted to 32-bit */
         long long max = 0x7fffffff;
         sint_val = get_clo_value(clo, "sint", -max - 1, max);
      } else if (CHECK_CLO(clo, "--uint=")) {
         /* Integer field is restricted to 32-bit */
         uint_val = get_clo_value(clo, "uint", 0, 0xffffffffU);
      } else {
         if (strncmp(clo, "--", 2) == 0)
            fatal("Invalid command line option '%s'\n", clo);
         clargs[num_clargs++] = clo;
      }
   }

   /* Check consistency of command line options */
   if (verify + generate + run + all + unit_test == 0)
      fatal("One of --verify, --generate, --run, --all, or --unit-test "
            "is required\n");
   if (verify + generate + run + all + unit_test != 1)
      fatal("At most one of --verify, --generate, --run, --all, or "
            " --unit-test can be given\n");

   vex_init();

   if (generate) {
      if (num_clargs == 0)
         fatal("Missing opcode name[s]\n");

      for (int i = 0; i < num_clargs; ++i) {
         const char *name = clargs[i];

         opcode *opc = get_opcode_by_name(name);

         if (opc == NULL) {
            error("'%s' is not a recognised opcode\n", name);
         } else if (opcode_has_errors(opc)) {
            error("Opcode '%s' ignored due to syntax errors\n", name);
         } else {
            generate_tests(opc);
            release_opcode(opc);
         }
      }
      return 0;
   }

   if (verify) {
      if (num_clargs == 0)
         fatal("Missing file name[s]\n");

      int num_mismatch = 0;

      for (int i = 0; i < num_clargs; ++i) {
         verify_stats stats = verify_disassembly(clargs[i]);
         num_mismatch += stats.num_mismatch;
      }
      return num_mismatch != 0;
   }

   if (run) {
      if (num_clargs == 0)
         fatal("Missing opcode name[s]\n");

      unsigned num_mismatch = 0;

      for (int i = 0; i < num_clargs; ++i) {
         const char *name = clargs[i];

         opcode *opc = get_opcode_by_name(name);

         if (opc == NULL) {
            error("'%s' is not a recognised opcode\n", name);
         } else if (opcode_has_errors(opc)) {
            error("Opcode '%s' ignored due to syntax errors\n", name);
         } else {
            generate_tests(opc);

            char file[strlen(name) + 10];    // large enough
            sprintf(file, "%s.dump", name);

            verify_stats stats = verify_disassembly(file);
            num_mismatch += stats.num_mismatch;

            if (! keep_temp)
               remove_temp_files(name);
            release_opcode(opc);
         }
      }
      return num_mismatch != 0;
   }

   if (all) {
      if (num_clargs != 0)
         fatal("Excess arguments on command line\n");

      unsigned num_tests, num_verified, num_mismatch, num_spec_exc;
      num_tests = num_verified = num_mismatch = num_spec_exc = 0;

      for (int i = 0; i < num_opcodes; ++i) {
         opcode *opc = get_opcode_by_index(i); // never NULL

         if (opcode_has_errors(opc)) {
            error("Opcode '%s' ignored due to syntax errors\n",
                  opc->name);
            continue;
         }
         num_tests += generate_tests(opc);

         char file[strlen(opc->name) + 10];
         sprintf(file, "%s.dump", opc->name);

         verify_stats stats = verify_disassembly(file);

         num_verified += stats.num_verified;
         num_mismatch += stats.num_mismatch;
         num_spec_exc += stats.num_spec_exc;

         if (! keep_temp)
            remove_temp_files(opc->name);
         release_opcode(opc);
      }
      if (verbose || summary) {
         printf("Total: %6u tests generated\n", num_tests);
         printf("Total: %6u insns verified\n", num_verified);
         printf("Total: %6u disassembly mismatches\n", num_mismatch);
         printf("Total: %6u specification exceptions\n", num_spec_exc);
      }
      return num_mismatch != 0;
   }

   if (unit_test)
      run_unit_tests();

   return 0;
}


static void
remove_temp_files(const char *op)
{
   char file[strlen(op) + 10];    // large enough
   static const char *suffix[] = { ".c", ".o", ".dump", ".vex" };

   for (int i = 0; i < sizeof suffix / sizeof *suffix; ++i) {
      sprintf(file, "%s%s", op, suffix[i]);
      unlink(file);
   }
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


static long long
get_clo_value(const char *clo, const char *field_name, long long min,
              long long max)
{
   long long value;

   const char *p = strchr(clo, '=') + 1;   // succeeds

   if  (sscanf(p, "%lld", &value) != 1)
      fatal("%s value '%s' is not an integer\n", field_name, p);
   if (value < min || value > max)
      fatal("%s value '%lld' is out of range\n", field_name, value);
   return value;
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
