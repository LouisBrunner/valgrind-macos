
/*--------------------------------------------------------------------*/
/*--- Arch-specific stuff for the core.            x86/core_arch.h ---*/
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

/* ---------------------------------------------------------------------
   Exports of vg_ldt.c
   ------------------------------------------------------------------ */

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
   Constants pertaining to the simulated CPU state, VG_(baseBlock),
   which need to go here to avoid ugly circularities.
   ------------------------------------------------------------------ */

/* How big is the saved SSE/SSE2 state?  Note that this subsumes the
   FPU state.  On machines without SSE, we just save/restore the FPU
   state into the first part of this area. */
/* A general comment about SSE save/restore: It appears that the 7th
   word (which is the MXCSR) has to be &ed with 0x0000FFBF in order
   that restoring from it later does not cause a GP fault (which is
   delivered as a segfault).  I guess this will have to be done
   any time we do fxsave :-(  7th word means word offset 6 or byte
   offset 24 from the start address of the save area.
 */
#define VG_SIZE_OF_SSESTATE 512
/* ... and in words ... */
#define VG_SIZE_OF_SSESTATE_W ((VG_SIZE_OF_SSESTATE+3)/4)


// Architecture-specific part of a ThreadState
// XXX: eventually this should be made abstract, ie. the fields not visible
//      to the core...
typedef struct {
   /* Pointer to this thread's Local (Segment) Descriptor Table.
      Starts out as NULL, indicating there is no table, and we hope to
      keep it that way.  If the thread does __NR_modify_ldt to create
      entries, we allocate a 8192-entry table at that point.  This is
      a straight copy of the Linux kernel's scheme.  Don't forget to
      deallocate this at thread exit. */
   VgLdtEntry* ldt;


   /* TLS table. This consists of a small number (currently 3) of
      entries from the Global Descriptor Table. */
   VgLdtEntry tls[VKI_GDT_TLS_ENTRIES];

   /* Saved machine context.  Note the FPU state, %EIP and segment
      registers are not shadowed.

      Although the segment registers are 16 bits long, storage
      management here and in VG_(baseBlock) is
      simplified if we pretend they are 32 bits. */
   UInt m_cs;
   UInt m_ss;
   UInt m_ds;
   UInt m_es;
   UInt m_fs;
   UInt m_gs;

   UInt m_eax;
   UInt m_ebx;
   UInt m_ecx;
   UInt m_edx;
   UInt m_esi;
   UInt m_edi;
   UInt m_ebp;
   UInt m_esp;
   UInt m_eflags;
   UInt m_eip;

   /* The SSE/FPU state.  This array does not (necessarily) have the
      required 16-byte alignment required to get stuff in/out by
      fxsave/fxrestore.  So we have to do it "by hand".
   */
   UInt m_sse[VG_SIZE_OF_SSESTATE_W];

   UInt sh_eax;
   UInt sh_ebx;
   UInt sh_ecx;
   UInt sh_edx;
   UInt sh_esi;
   UInt sh_edi;
   UInt sh_ebp;
   UInt sh_esp;
   UInt sh_eflags;
} 
arch_thread_t;


/* ---------------------------------------------------------------------
   Constants involving memory layout
   ------------------------------------------------------------------ */

// base address of client address space
#define CLIENT_BASE	0x00000000ul

#endif   // __X86_CORE_ARCH_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
