
/*--------------------------------------------------------------------*/
/*--- Arch-specific stuff for the core.            x86/core_arch.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2005 Nicholas Nethercote
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

#ifndef __X86_CORE_ARCH_H
#define __X86_CORE_ARCH_H

#include "core_arch_asm.h"    // arch-specific asm  stuff
#include "tool_arch.h"        // arch-specific tool stuff

#include "libvex_guest_x86.h"

/* ---------------------------------------------------------------------
   Basic properties
   ------------------------------------------------------------------ */

#define VGA_ELF_ENDIANNESS     ELFDATA2LSB
#define VGA_ELF_MACHINE        EM_386
#define VGA_ELF_CLASS          ELFCLASS32

#define VGA_WORD_SIZE         4

/* ---------------------------------------------------------------------
   Interesting registers
   ------------------------------------------------------------------ */

// Vex field names
#define VGA_INSTR_PTR         guest_EIP
#define VGA_STACK_PTR         guest_ESP
#define VGA_FRAME_PTR         guest_EBP

#define VGA_CLREQ_ARGS        guest_EAX
#define VGA_CLREQ_RET         guest_EDX

// Register numbers, for vg_symtab2.c
#define VGA_R_STACK_PTR       4
#define VGA_R_FRAME_PTR       5

// Stack frame layout and linkage
#define VGA_FIRST_STACK_FRAME(ebp)     (ebp)
#define VGA_STACK_FRAME_RET(ebp)       (((UInt*)ebp)[1])
#define VGA_STACK_FRAME_NEXT(ebp)      (((UInt*)ebp)[0])

// Get stack pointer and frame pointer
#define VGA_GET_REAL_STACK_PTR(esp) do {   \
   asm("movl %%esp, %0" : "=r" (esp));       \
} while (0)

#define VGA_GET_REAL_FRAME_PTR(ebp) do {   \
   asm("movl %%ebp, %0" : "=r" (ebp));       \
} while (0)

//extern const Char VG_(helper_wrapper_before)[];	/* in dispatch.S */
//extern const Char VG_(helper_wrapper_return)[];	/* in dispatch.S */

//extern const Char VG_(helper_undefined_instruction)[];
//extern const Char VG_(helper_INT)[];
//extern const Char VG_(helper_breakpoint)[];


/* ---------------------------------------------------------------------
   Architecture-specific part of a ThreadState
   ------------------------------------------------------------------ */

typedef 
   struct {
      /* --- BEGIN vex-mandated guest state --- */

      /* Saved machine context. */
      VexGuestX86State vex;

      /* Saved shadow context. */
      VexGuestX86State vex_shadow;

      /* Spill area. */
      UChar vex_spill[LibVEX_N_SPILL_BYTES];

      /* --- END vex-mandated guest state --- */
   } 
   ThreadArchState;

typedef VexGuestX86State VexGuestArchState;

/* ---------------------------------------------------------------------
   Miscellaneous constants
   ------------------------------------------------------------------ */

// Valgrind's signal stack size, in words.
#define VGA_SIGSTACK_SIZE_W   10000

// Valgrind's stack size, in words.
#define VGA_STACK_SIZE_W      16384

// Base address of client address space.
#define VGA_CLIENT_BASE       0x0ul

/* ---------------------------------------------------------------------
   Signal stuff (should be plat)
   ------------------------------------------------------------------ */

void VGA_(signal_return)(ThreadId tid, Bool isRT);

#endif   // __X86_CORE_ARCH_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
