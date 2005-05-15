
/*--------------------------------------------------------------------*/
/*--- Arch-specific stuff for the core.            arm/core_arch.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2005 Nicholas Nethercote
      njn@valgrind.org

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
   Basic properties
   ------------------------------------------------------------------ */

#define VGA_ELF_ENDIANNESS     ELFDATA2LSB
#define VGA_ELF_MACHINE        EM_ARM
#define VGA_ELF_CLASS          ELFCLASS32

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
#define VGA_INSTR_PTR         guest_R15
#define VGA_STACK_PTR         guest_R13
#define VGA_FRAME_PTR         guest_R11

#define VGA_CLREQ_ARGS        guest_R0
#define VGA_CLREQ_RET         guest_R0

// Register numbers, for vg_symtab2.c
#define VGA_R_STACK_PTR       13
#define VGA_R_FRAME_PTR       11

// Stack frame layout and linkage
// XXX ToDo: ???
#define VGA_FIRST_STACK_FRAME(ebp)     (ebp)
#define VGA_STACK_FRAME_RET(ebp)       (((UInt*)ebp)[1])
#define VGA_STACK_FRAME_NEXT(ebp)      (((UInt*)ebp)[0])

// Get stack pointer and frame pointer
#define VGA_GET_REAL_STACK_PTR(esp) do {   \
   I_die_here; \
} while (0)

#define VGA_GET_REAL_FRAME_PTR(ebp) do {   \
   I_die_here; \
} while (0)

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
   Miscellaneous constants
   ------------------------------------------------------------------ */

// Base address of client address space.
#define VGA_CLIENT_BASE       0x0ul

#endif   // __ARM_CORE_ARCH_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
