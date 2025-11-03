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

#include <stddef.h>           // NULL
#include <stdio.h>            // sprintf
#include <stdlib.h>           // free
#include <string.h>           // strchr
#include <ctype.h>            // isdigit
#include "main.h"             // error
#include "objdump.h"

static int get_nibble(const char *);
static int get_byte(const char *);
static void io_error(FILE *, const char *, const char *);


objdump_file *
read_objdump(const char *file)
{
   const char *function = FUNCTION;
   const char *mark = MARK;

   /* Slurp file into memory */
   FILE *fp = fopen(file, "rb");
   if (fp == NULL) {
      io_error(fp, file, "fopen failed\n");
      return NULL;
   }

   /* Determine file size */
   int rc = fseek(fp, 0, SEEK_END);
   if (rc < 0) {
      io_error(fp, file, "fseek failed\n");
      return NULL;
   }

   long size = ftell(fp);
   if (size < 0) {
      io_error(fp, file, "ftell failed\n");
      return NULL;
   }
   if (size == 0) {
      io_error(fp, file, "file is empty\n");
      return NULL;
   }
   rewind(fp);

   char *const buf = mallock(size + 1);
   size_t num_read = fread(buf, 1, size, fp);
   if (num_read != size) {
      io_error(fp, file, "fread failed\n");
      free(buf);
      return NULL;
   }
   buf[size] = '\0';

   fclose(fp);

   /* Determine the number of lines in the file. This number
      exceeds the number of lines containing insns. */
   unsigned num_lines = 0;

   for (char *p = buf; *p; ++p) {
      if (*p == '\n') {
         *p = '\0';
         ++num_lines;
      }
   }

   /* Allocate an objdump_file. */
   objdump_file *ofile = mallock(sizeof (objdump_file));

   ofile->filebuf = buf;
   ofile->lines   = mallock(num_lines * sizeof(objdump_line));

   /* Locate the line containing <FUNCTION>: */
   char string[strlen(function) + 3 + 1];
   sprintf(string, "<%s>:", function);

   char *cur, *next = 0;   // shut up, GCC
   const char *end = buf + num_read;

   for (cur = buf; cur != end; cur = next) {
      const char *line = cur;
      next = strchr(line, '\0') + 1;
      if (strstr(line, string))
         break;
   }

   /* Process the lines containing insns. These are the lines between
      the 1st and 2nd MARK. */
   unsigned linecnt = 0;
   int marker_seen = 0;
   for (cur = next; cur != end; cur = next) {
      char *line = cur;

      next = strchr(line, '\0') + 1;

      char *p;
      for (p = line; isspace(*p); ++p)
         ;

      if (*p == '\0') continue;   // blank line allowed

      unsigned address = 0;
      while (*p != ':') {
         address = (address << 4) + get_nibble(p);
         ++p;
      }

      ++p;    // skip ':'

      while (isspace(*p))
         ++p;

      /* The leftmost two bits (0:1) encode the length of the insn
         in bytes:
         00 -> 2 bytes, 01 -> 4 bytes, 10 -> 4 bytes, 11 -> 6 bytes. */
      unsigned char byte = get_byte(p);
      unsigned insn_len = ((((byte >> 6) + 1) >> 1) + 1) << 1;

      /* Temporary buffer. */
      char insn_bytes[6] = { 0 };

      for (int i = 0; i < insn_len; ++i) {
         insn_bytes[i] = get_byte(p);
         p += 3;
      }

      while (isspace(*p))   // skip white space to disassembled text
         ++p;

      char *dis_insn = p;

      if (strncmp(dis_insn, mark, strlen(mark)) == 0) {
         if (marker_seen)
            break;          // we're done
         marker_seen = 1;
      } else {
         if (marker_seen == 1) {
            /* Add the line */
            objdump_line *oline = ofile->lines + linecnt++;
            oline->address = address;
            oline->insn_len = insn_len;
            oline->disassembled_insn = dis_insn;
            memcpy(oline->insn_bytes, insn_bytes, sizeof insn_bytes);

            /* Extra byte to allow the decoder to peek past the end of
               the current insn */
            // FIXME: introduce global variable that is observed in
            // FIXME: the decoder which disables peeking ahead ?
            oline->insn_bytes[insn_len] = 0x00;
         }
      }
   }

   if (marker_seen == 0) {
      error("%s is not a valid objdump -d file\n", file);
      release_objdump(ofile);
      return NULL;
   }

   ofile->num_lines = linecnt;

   return ofile;
}


/* Free all memory allocated for the objdump file */
void
release_objdump(objdump_file *ofile)
{
   free(ofile->filebuf);
   free(ofile->lines);
   free(ofile);
}


static int
get_nibble(const char *p)
{
   int c = *p;

   if (isdigit(c))
      return c - '0';

   switch (tolower(c)) {
   case 'a': return 10;
   case 'b': return 11;
   case 'c': return 12;
   case 'd': return 13;
   case 'e': return 14;
   case 'f': return 15;
   default:
      break;
   }

   error("%s: get_nibble failed; continuing with fingers crossed\n", p);
   return 0;
}


static int
get_byte(const char *p)
{
   int n1 = get_nibble(p);
   int n2 = get_nibble(p + 1);

   return (n1 << 4) + n2;
}

static void
io_error(FILE *fp, const char *file, const char *msg)
{
   if (fp)
      fclose(fp);
   error("%s: %s", file, msg);
}
