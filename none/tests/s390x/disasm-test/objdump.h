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

#ifndef OBJDUMP_H
#define OBJDUMP_H

/* An opcode which marks the begin and end of a sequence of insns
   in a testcase whose disassembly should be verified. */
#define MARK "csch"

/* Name of the C function containing the generated insns. */
#define FUNCTION "main"

/* INSN_BYTES needs an extra byte because s390_decode_and_irgen peeks
   at the next instruction to handle some special case. And in case
   of INSN_BYTES having only 6 elements that would be an out-of-bounds
   memory access. insn_bytes[insn_len] will always be 0x00. */
typedef struct {
   unsigned address;
   unsigned insn_len;
   unsigned char insn_bytes[6 + 1];
   const char *disassembled_insn;   // points into objdump_file::filebuf
} objdump_line;

typedef struct {
   char *filebuf;       // contents of objdump file; will be modified !
   unsigned num_lines;  // #lines containing insns
   objdump_line *lines;
} objdump_file;

objdump_file *read_objdump(const char *);
void release_objdump(objdump_file *);

#endif // OBJDUMP_H
