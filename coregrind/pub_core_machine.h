
/*--------------------------------------------------------------------*/
/*--- Machine-related things.                   pub_core_machine.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2005 Julian Seward
      jseward@acm.org

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

#ifndef __PUB_CORE_MACHINE_H
#define __PUB_CORE_MACHINE_H

//--------------------------------------------------------------------
// PURPOSE: This module contains code related to the particular
// architecture, things like accessing guest state, endianness, word size,
// etc.
//--------------------------------------------------------------------

#include "pub_tool_machine.h"

#if defined(VGA_x86)
#  define VG_ELF_DATA2XXX     ELFDATA2LSB
#  define VG_ELF_MACHINE      EM_386
#  define VG_ELF_CLASS        ELFCLASS32
#elif defined(VGA_amd64)
#  define VG_ELF_DATA2XXX     ELFDATA2LSB
#  define VG_ELF_MACHINE      EM_X86_64
#  define VG_ELF_CLASS        ELFCLASS64
#elif defined(VGA_ppc32)
#  define VG_ELF_DATA2XXX     ELFDATA2MSB
#  define VG_ELF_MACHINE      EM_PPC
#  define VG_ELF_CLASS        ELFCLASS32
#else
#  error Unknown arch
#endif

#if defined(VGA_x86)
#  define VG_INSTR_PTR        guest_EIP
#  define VG_STACK_PTR        guest_ESP
#  define VG_FRAME_PTR        guest_EBP
#elif defined(VGA_amd64)
#  define VG_INSTR_PTR        guest_RIP
#  define VG_STACK_PTR        guest_RSP
#  define VG_FRAME_PTR        guest_RBP
#elif defined(VGA_ppc32)
#  define VG_INSTR_PTR        guest_CIA
#  define VG_STACK_PTR        guest_GPR1
#  define VG_FRAME_PTR        guest_GPR1   // No frame ptr for PPC
#else
#  error Unknown arch
#endif


// Offsets for the Vex state
#define VG_O_STACK_PTR        (offsetof(VexGuestArchState, VG_STACK_PTR))


// Architecture specifics

// PPC: what is the cache line size (for dcbz etc) ?
// This info is harvested on Linux at startup from the AT_SYSINFO
// entries.
#if defined(VGA_ppc32)
extern Int VG_(cache_line_size_ppc32);
#endif

// X86: set to 1 if the host is able to do {ld,st}mxcsr (load/store
// the SSE control/status register. 
#if defined(VGA_x86)
extern Int VG_(have_mxcsr_x86);
#endif


#endif   // __PUB_CORE_MACHINE_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
