
/*--------------------------------------------------------------------*/
/*--- Private arch-specific header.              x86/x86_private.h ---*/
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

#ifndef __X86_PRIVATE_H
#define __X86_PRIVATE_H

#include "core_arch_asm.h"    // arch-specific asm  stuff
#include "tool_arch.h"        // arch-specific tool stuff

#include "libvex_guest_x86.h" // for VexGuestX86SegDescr

/* ---------------------------------------------------------------------
   Exports of state.c that are not core-visible
   ------------------------------------------------------------------ */

/* Create LDT/GDT arrays, as specified in libvex_guest_x86.h. */
extern VexGuestX86SegDescr* VG_(alloc_zeroed_x86_GDT) ( void );
extern VexGuestX86SegDescr* VG_(alloc_zeroed_x86_LDT) ( void );


#endif   // __X86_PRIVATE_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
