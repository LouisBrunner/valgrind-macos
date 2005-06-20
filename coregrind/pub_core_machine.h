
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
#  define VGA_ELF_ENDIANNESS  ELFDATA2LSB
#  define VGA_ELF_MACHINE     EM_386
#  define VGA_ELF_CLASS       ELFCLASS32
#elif defined(VGA_amd64)
#  define VGA_ELF_ENDIANNESS  ELFDATA2LSB
#  define VGA_ELF_MACHINE     EM_X86_64
#  define VGA_ELF_CLASS       ELFCLASS64
#elif defined(VGA_arm)
#  define VGA_ELF_ENDIANNESS  ELFDATA2LSB
#  define VGA_ELF_MACHINE     EM_ARM
#  define VGA_ELF_CLASS       ELFCLASS32
#elif defined(VGA_ppc32)
#  define VGA_ELF_ENDIANNESS  ELFDATA2MSB
#  define VGA_ELF_MACHINE     EM_PPC
#  define VGA_ELF_CLASS       ELFCLASS32
#else
#  error Unknown arch
#endif

#if defined(VGA_x86)
#  define VGA_INSTR_PTR       guest_EIP
#  define VGA_STACK_PTR       guest_ESP
#  define VGA_FRAME_PTR       guest_EBP
#elif defined(VGA_amd64)
#  define VGA_INSTR_PTR       guest_RIP
#  define VGA_STACK_PTR       guest_RSP
#  define VGA_FRAME_PTR       guest_RBP
#elif defined(VGA_arm)
   // XXX: Not sure, but I think:
   //   r11 = frame pointer
   //   r12 = "implicit parameter" (neither caller-save, nor callee-save)
   //   r13 = stack pointer
   //   r14 = link register
   //   r15 = program counter
#  define VGA_INSTR_PTR       guest_R15
#  define VGA_STACK_PTR       guest_R13
#  define VGA_FRAME_PTR       guest_R11
#elif defined(VGA_ppc32)
#  define VGA_INSTR_PTR       guest_CIA
#  define VGA_STACK_PTR       guest_GPR1
#  define VGA_FRAME_PTR       guest_GPR1   // No frame ptr for PPC
#else
#  error Unknown arch
#endif

// Offsets for the Vex state
#define O_STACK_PTR        (offsetof(VexGuestArchState, VGA_STACK_PTR))

#endif   // __PUB_CORE_MACHINE_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
