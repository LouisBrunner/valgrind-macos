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

#include <stdio.h>          // printf
#include <string.h>         // strcpy
#include <ctype.h>          // isspace
#include "objdump.h"        // read_objdump
#include "main.h"           // verbose
#include "vex.h"            // vex_disasm

static int disasm_same(const char *, const char *, unsigned);


/* Return number of disassembly mismatches. */
verify_stats
verify_disassembly(const char *file)
{
   verify_stats stats = { 0, 0, 0 };  // return value

   objdump_file *ofile = read_objdump(file);
   if (ofile == NULL)
      return stats;

   if (verbose)
      printf("...verifying %u insns in '%s'\n", ofile->num_lines, file);

   const char *p = strchr(file, '.');
   char vex_file[strlen(file) + 5];

   if (p == NULL) {
      sprintf(vex_file, "%s.vex", file);
   } else {
      int len = p - file;
      strncpy(vex_file, file, len);
      strcpy(vex_file + len, ".vex");
   }
   FILE *fpvex = fopen(vex_file, "w");
   if (fpvex == NULL)
      error("%s: fopen failed\n", vex_file);

   for (int i = 0; i < ofile->num_lines; ++i) {
      const objdump_line *oline = ofile->lines + i;
      int spec_exc = 0;
      const char *disassembly_from_vex =
         vex_disasm(oline->insn_bytes, &spec_exc);

      if (spec_exc) {
         ++stats.num_spec_exc;

         if (show_spec_exc) {
            fprintf(stderr, "*** specification exception for insn ");
            for (int j = 0; j < oline->insn_len; ++j)
               fprintf(stderr, "%02X", oline->insn_bytes[j]);
            fprintf(stderr, " in %s\n", file);
         }
         /* Instructions causing specification exceptions are not
            compared */
         continue;
      }

      if (disassembly_from_vex == NULL)
         disassembly_from_vex = "MISSING disassembly from VEX";
      if (fpvex)
         fprintf(fpvex, "%s\n", disassembly_from_vex);

      /* Compare disassembled insns */
      ++stats.num_verified;
      if (! disasm_same(oline->disassembled_insn, disassembly_from_vex,
                        oline->address)) {
         ++stats.num_mismatch;
         if (show_miscompares) {
            int n = fprintf(stderr, "*** mismatch VEX: |%s|",
                            disassembly_from_vex);
            fprintf(stderr, "%*c", 50 - n, ' ');
            fprintf(stderr, "objdump: |%s|\n", oline->disassembled_insn);
         }
      }
   }
   if (fpvex)
      fclose(fpvex);
   release_objdump(ofile);

   if (verbose) {
      printf("...%u insns verified\n", stats.num_verified);
      printf("...%u disassembly mismatches\n", stats.num_mismatch);
      printf("...%u specification exceptions\n", stats.num_spec_exc);
   }

   return stats;
}


/* Compare two disassembled insns ignoring white space. Return 1 if
   equal. */
static int
disasm_same(const char *from_objdump, const char *from_vex,
            unsigned address)
{
   const char *p1 = from_objdump;
   const char *p2 = from_vex;

   while (42) {
      if (*p1 == '\0' && *p2 == '\0')
         return 1;
      if (*p1 == '\0' || *p2 == '\0')
         return 0;
      while (isspace(*p1))
         ++p1;
      while (isspace(*p2))
         ++p2;
      if (*p1 != *p2) {
         long long offset_in_bytes;
         unsigned long long target_address;

         /* Consider the case where the VEX disassembly has ".+integer"
            or ".-integer" and the objdump disassembly has an
            address. */
         if (*p2++ != '.') return 0;
         if (sscanf(p2, "%lld", &offset_in_bytes) != 1) return 0;
         if (sscanf(p1, "%llx", &target_address)  != 1) return 0;
         return address + offset_in_bytes == target_address;
      }
      ++p1;
      ++p2;
   }
}
