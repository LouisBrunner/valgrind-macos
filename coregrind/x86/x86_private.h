
/*--------------------------------------------------------------------*/
/*--- Private arch-specific header.              x86/x86_private.h ---*/
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

#ifndef __X86_PRIVATE_H
#define __X86_PRIVATE_H

#include "core_arch_asm.h"    // arch-specific asm  stuff
#include "x86_private_asm.h"  // private arch-specific asm stuff
#include "tool_arch.h"        // arch-specific tool stuff

/* ---------------------------------------------------------------------
   Exports of state.c that are not core-visible
   ------------------------------------------------------------------ */

/* Is this a SSE/SSE2-capable CPU?  If so, we had better save/restore
   the SSE state all over the place.  This is set up very early, since we
   can't even correctly snapshot the startup machine state without it. */
extern Bool VG_(have_ssestate);

/* ---------------------------------------------------------------------
   Exports of vg_ldt.c
   ------------------------------------------------------------------ */

/* Alloc & copy, and dealloc. */
extern VgLdtEntry* VG_(allocate_LDT_for_thread)   ( VgLdtEntry* parent_ldt );
extern void        VG_(deallocate_LDT_for_thread) ( VgLdtEntry* ldt );
extern void        VG_(clear_TLS_for_thread)      ( VgLdtEntry* tls );


#endif   // __X86_PRIVATE_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
