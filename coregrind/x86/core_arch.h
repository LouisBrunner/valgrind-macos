
/*--------------------------------------------------------------------*/
/*--- Arch-specific stuff for the core.            x86/core_arch.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

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

#ifndef __X86_CORE_ARCH_H
#define __X86_CORE_ARCH_H

#include "core_arch_asm.h"    // arch-specific asm  stuff
#include "tool_arch.h"        // arch-specific tool stuff

#include "libvex_guest_x86.h"

/* ---------------------------------------------------------------------
   Basic properties
   ------------------------------------------------------------------ */

#define VG_ELF_ENDIANNESS     ELFDATA2LSB
#define VG_ELF_MACHINE        EM_386
#define VG_ELF_CLASS          ELFCLASS32

#define InsnSetArch           InsnSetX86

#define VGA_WORD_SIZE         4

/* ---------------------------------------------------------------------
   Interesting registers
   ------------------------------------------------------------------ */

// Vex field names
#define ARCH_INSTR_PTR        guest_EIP
#define ARCH_STACK_PTR        guest_ESP
#define ARCH_FRAME_PTR        guest_EBP

#define ARCH_CLREQ_ARGS       guest_EAX
#define ARCH_CLREQ_RET        guest_EDX
#define ARCH_PTHREQ_RET       guest_EDX

// Register numbers, for vg_symtab2.c
#define R_STACK_PTR           4
#define R_FRAME_PTR           5

// Stack frame layout and linkage
#define FIRST_STACK_FRAME(ebp)         (ebp)
#define STACK_FRAME_RET(ebp)           (((UInt*)ebp)[1])
#define STACK_FRAME_NEXT(ebp)          (((UInt*)ebp)[0])

// Get stack pointer and frame pointer
#define ARCH_GET_REAL_STACK_PTR(esp) do {   \
   asm("movl %%esp, %0" : "=r" (esp));       \
} while (0)

#define ARCH_GET_REAL_FRAME_PTR(ebp) do {   \
   asm("movl %%ebp, %0" : "=r" (ebp));       \
} while (0)

/* ---------------------------------------------------------------------
   LDT type             
   ------------------------------------------------------------------ */

// XXX: eventually this will be x86-private, not seen by the core(?)

/* This is the hardware-format for a segment descriptor, ie what the
   x86 actually deals with.  It is 8 bytes long.  It's ugly.  */

typedef struct _LDT_ENTRY {
    union {
       struct {
          UShort      LimitLow;
          UShort      BaseLow;
          unsigned    BaseMid         : 8;
          unsigned    Type            : 5;
          unsigned    Dpl             : 2;
          unsigned    Pres            : 1;
          unsigned    LimitHi         : 4;
          unsigned    Sys             : 1;
          unsigned    Reserved_0      : 1;
          unsigned    Default_Big     : 1;
          unsigned    Granularity     : 1;
          unsigned    BaseHi          : 8;
       } Bits;
       struct {
          UInt word1;
          UInt word2;
       } Words;
    } 
    LdtEnt;
} VgLdtEntry;


/* ---------------------------------------------------------------------
   Architecture-specific part of a ThreadState
   ------------------------------------------------------------------ */

// Architecture-specific part of a ThreadState
// XXX: eventually this should be made abstract, ie. the fields not visible
//      to the core...  then VgLdtEntry can be made non-visible to the core
//      also.
typedef 
   struct {
      /* Pointer to this thread's Local (Segment) Descriptor Table.
         Starts out as NULL, indicating there is no table, and we hope
         to keep it that way.  If the thread does __NR_modify_ldt to
         create entries, we allocate a 8192-entry table at that point.
         This is a straight copy of the Linux kernel's scheme.  Don't
         forget to deallocate this at thread exit. */
      VgLdtEntry* ldt;

      /* TLS table. This consists of a small number (currently 3) of
         entries from the Global Descriptor Table. */
      VgLdtEntry tls[VKI_GDT_ENTRY_TLS_ENTRIES];

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
   libpthread stuff
   ------------------------------------------------------------------ */

struct _ThreadArchAux {
   void*         tls_data;
   int           tls_segment;
   unsigned long sysinfo;
};

/* ---------------------------------------------------------------------
   Miscellaneous constants
   ------------------------------------------------------------------ */

// Valgrind's signal stack size, in words.
#define VG_SIGSTACK_SIZE_W    10000

// Base address of client address space.
#define CLIENT_BASE	0x00000000ul

#endif   // __X86_CORE_ARCH_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
