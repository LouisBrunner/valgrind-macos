
/*--------------------------------------------------------------------*/
/*--- x86-specific stuff for the core.             x86/core_arch.h ---*/
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

#ifndef __X86_CORE_ARCH_H
#define __X86_CORE_ARCH_H

#include "core_arch_asm.h"    // arch-specific asm  stuff
#include "tool_arch.h"        // arch-specific tool stuff

#include "libvex_guest_x86.h"


/* ---------------------------------------------------------------------
   Interesting registers
   ------------------------------------------------------------------ */

// Accessors for the arch_thread_t
#define ARCH_INSTR_PTR(regs)           ((regs).vex.guest_EIP)
#define ARCH_STACK_PTR(regs)           ((regs).vex.guest_ESP)
#define ARCH_FRAME_PTR(regs)           ((regs).vex.guest_EBP)

#define ARCH_CLREQ_ARGS(regs)          ((regs).vex.guest_EAX)
#define ARCH_PTHREQ_RET(regs)          ((regs).vex.guest_EDX)
#define ARCH_CLREQ_RET(regs)           ((regs).vex.guest_EDX)

// Accessors for the baseBlock
#define R_STACK_PTR                    R_ESP
#define R_FRAME_PTR                    R_EBP

#define R_CLREQ_RET                    R_EDX
#define R_PTHREQ_RET                   R_EDX

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

// So the dispatch loop can find %EIP
extern Int VGOFF_(m_eip);


/* ---------------------------------------------------------------------
   Elf stuff
   ------------------------------------------------------------------ */

#define VG_ELF_ENDIANNESS     ELFDATA2LSB
#define VG_ELF_MACHINE        EM_386       
#define VG_ELF_CLASS          ELFCLASS32


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


/* ---------------------------------------------------------------------
   libpthread stuff
   ------------------------------------------------------------------ */

struct arch_thread_aux {
   void*         tls_data;
   int           tls_segment;
   unsigned long sysinfo;
};

/* ---------------------------------------------------------------------
   Miscellaneous constants
   ------------------------------------------------------------------ */

// Total number of spill slots available for register allocation.
#define VG_MAX_SPILLSLOTS     100

// Valgrind's signal stack size, in words.
#define VG_SIGSTACK_SIZE_W    10000

// Base address of client address space.
#define CLIENT_BASE	0x00000000ul

#endif   // __X86_CORE_ARCH_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
