
/*--------------------------------------------------------------------*/
/*--- ARM-specific stuff for the core.             arm/core_arch.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, an extensible x86 protected-mode
   emulator for monitoring program execution on x86-Unixes.

   Copyright (C) 2000-2004 Nicholas Nethercote
      njn25@cam.ac.uk

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307, USA.

   The GNU General Public License is contained in the file COPYING.
*/

#ifndef __ARM_CORE_ARCH_H
#define __ARM_CORE_ARCH_H

#include "core_arch_asm.h"    // arch-specific asm  stuff
#include "tool_arch.h"        // arch-specific tool stuff

#include "libvex_guest_arm.h"


/* ---------------------------------------------------------------------
   Interesting registers
   ------------------------------------------------------------------ */
// Vex field names
// XXX: Not sure, but I think:
//   r11 = frame pointer
//   r12 = "implicit parameter" (neither caller-save, nor callee-save)
//   r13 = stack pointer
//   r14 = link register
//   r15 = program counter
#define ARCH_INSTR_PTR        guest_R15
#define ARCH_STACK_PTR        guest_R13
#define ARCH_FRAME_PTR        guest_R11

#define ARCH_CLREQ_ARGS       guest_R0
#define ARCH_CLREQ_RET        guest_R0
#define ARCH_PTHREQ_RET       guest_R0

// Register numbers, for vg_symtab2.c
#define R_STACK_PTR           13
#define R_FRAME_PTR           11

// Stack frame layout and linkage
// XXX ToDo: ???
#define FIRST_STACK_FRAME(ebp)         (ebp)
#define STACK_FRAME_RET(ebp)           (((UInt*)ebp)[1])
#define STACK_FRAME_NEXT(ebp)          (((UInt*)ebp)[0])

// Get stack pointer and frame pointer
#define ARCH_GET_REAL_STACK_PTR(esp) do {   \
   I_die_here; \
} while (0)

#define ARCH_GET_REAL_FRAME_PTR(ebp) do {   \
   I_die_here; \
} while (0)

/* ---------------------------------------------------------------------
   Elf stuff
   ------------------------------------------------------------------ */

#define VG_ELF_ENDIANNESS     ELFDATA2LSB
#define VG_ELF_MACHINE        EM_ARM
#define VG_ELF_CLASS          ELFCLASS32

/* ---------------------------------------------------------------------
   Architecture-specific part of a ThreadState
   ------------------------------------------------------------------ */

// Architecture-specific part of a ThreadState
// XXX: eventually this should be made abstract, ie. the fields not visible
//      to the core... ?? 
typedef struct {
   /* Saved machine context. */
   VexGuestARMState vex;

   /* Saved shadow context. */
   VexGuestARMState vex_shadow;

   /* Spill area. */
   UChar vex_spill[LibVEX_N_SPILL_BYTES];
} 
ThreadArchState;

typedef VexGuestARMState VexGuestArchState;

/* ---------------------------------------------------------------------
   libpthread stuff
   ------------------------------------------------------------------ */

struct _ThreadArchAux {
   // XXX: nothing?
};

/* ---------------------------------------------------------------------
   Miscellaneous constants
   ------------------------------------------------------------------ */

// Valgrind's signal stack size, in words.
#define VG_SIGSTACK_SIZE_W    10000

// Base address of client address space.
#define CLIENT_BASE	0x00000000ul

#endif   // __ARM_CORE_ARCH_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
