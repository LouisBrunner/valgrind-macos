
/*--------------------------------------------------------------------*/
/*--- Asm-specific core stuff.                          core_asm.h ---*/
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

#ifndef __CORE_ASM_H
#define __CORE_ASM_H

#include "tool_asm.h"            // tool asm stuff
#include "core_arch_asm.h"       // arch-specific asm stuff

/* This file is included in all Valgrind source files, including
   assembly ones. */


/* Constants for the fast translation lookup cache. */
#define VG_TT_FAST_BITS 16
#define VG_TT_FAST_SIZE (1 << VG_TT_FAST_BITS)
#define VG_TT_FAST_MASK ((VG_TT_FAST_SIZE) - 1)

#endif /* __CORE_ASM_H */

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
