
/*--------------------------------------------------------------------*/
/*--- Arch-specific stuff for the core.          amd64/core_arch.h ---*/
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

#ifndef __AMD64_CORE_ARCH_H
#define __AMD64_CORE_ARCH_H

#include "core_arch_asm.h"    // arch-specific asm  stuff
#include "tool_arch.h"        // arch-specific tool stuff

#include "libvex_guest_amd64.h"

/* ---------------------------------------------------------------------
   Basic properties
   ------------------------------------------------------------------ */

#define VGA_ELF_ENDIANNESS     ELFDATA2LSB
#define VGA_ELF_MACHINE        EM_X86_64
#define VGA_ELF_CLASS          ELFCLASS64

/* ---------------------------------------------------------------------
   Interesting registers
   ------------------------------------------------------------------ */

// Vex field names
#define VGA_INSTR_PTR         guest_RIP
#define VGA_STACK_PTR         guest_RSP
#define VGA_FRAME_PTR         guest_RBP

#define VGA_CLREQ_ARGS        guest_RAX
#define VGA_CLREQ_RET         guest_RDX

/* ---------------------------------------------------------------------
   Architecture-specific part of a ThreadState
   ------------------------------------------------------------------ */

// Architecture-specific part of a ThreadState
// XXX: eventually this should be made abstract, ie. the fields not visible
//      to the core...
typedef 
   struct {
      /* --- BEGIN vex-mandated guest state --- */

      /* Saved machine context. */
      VexGuestAMD64State vex;

      /* Saved shadow context. */
      VexGuestAMD64State vex_shadow;

      /* Spill area. */
      UChar vex_spill[LibVEX_N_SPILL_BYTES];

      /* --- END vex-mandated guest state --- */
   } 
   ThreadArchState;

typedef VexGuestAMD64State VexGuestArchState;

/* ---------------------------------------------------------------------
   Miscellaneous constants
   ------------------------------------------------------------------ */

// Valgrind's stack size, in words.
#define VGA_STACK_SIZE_W      16384

#endif   // __AMD64_CORE_ARCH_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/

