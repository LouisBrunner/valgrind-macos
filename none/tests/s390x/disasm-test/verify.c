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

#include <stdio.h>          // printf
#include <string.h>         // strcpy
#include <ctype.h>          // isspace
#include "objdump.h"        // read_objdump
#include "main.h"           // verbose
#include "vex.h"            // vex_disasm


/* Watch out: returned string is allocated in a static buffer which will
   be overwritten in the next invocation. */
static const char *
insn_bytes_as_string(const unsigned char *bytes, unsigned num_bytes)
{
   static char buf[4 + 1 + 4 + 1 + 4 + 1];

   char *p = buf;
   for (int j = 0; j < num_bytes; j += 2)
      p += sprintf(p, "%02X%02X ", bytes[j], bytes[j + 1]);
   *--p = '\0';

   return buf;
}


test_stats
verify_spec_exceptions(const opcode *opc, int gen_spec_exc_tests)
{
   test_stats stats = { 0, 0 };  // return value

   char file[strlen(opc->name) + 15];    // large enough
   sprintf(file, "%s-%s.dump", opc->name, gen_spec_exc_tests ? "se" : "no-se");

   objdump_file *ofile = read_objdump(file);
   if (ofile == NULL)
      return stats;

   if (verbose)
      printf("...verifying %u insns in '%s'\n", ofile->num_lines, file);

   char se_file[strlen(opc->name) + 15];  // large enough
   sprintf(se_file, "%s-%s.spec-exc", opc->name,
           gen_spec_exc_tests ? "se" : "no-se");

   FILE *fpse = fopen(se_file, "w");
   if (fpse == NULL)
      error("%s: fopen failed\n", se_file);

   for (int i = 0; i < ofile->num_lines; ++i) {
      const objdump_line *oline = ofile->lines + i;
      int spec_exc = 0;
      const char *disassembly_from_vex =
         vex_disasm(oline->insn_bytes, &spec_exc);
      const char *insn_bytes =
         insn_bytes_as_string(oline->insn_bytes, oline->insn_len);

      if (disassembly_from_vex == NULL) {
         error("Disasm failed for %s\n", insn_bytes);
         continue;
      }

      if (spec_exc) {
         ++stats.num_spec_exc;

         if (fpse)
            fprintf(fpse, "%s   %s\n", insn_bytes, disassembly_from_vex);
         if (! gen_spec_exc_tests)
            error("Unexpected spec. exc. detected for %s   %s\n", insn_bytes,
                  disassembly_from_vex);
      } else {
         if (gen_spec_exc_tests)
            error("Spec. exc. not detected for %s   %s\n", insn_bytes,
                  disassembly_from_vex);
      }
   }

   if (fpse)
      fclose(fpse);
   release_objdump(ofile);

   return stats;
}
