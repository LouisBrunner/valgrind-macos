
/*--------------------------------------------------------------------*/
/*--- Syscall numbers and related operations. pub_core_vkiscnums.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2010 Julian Seward
      jseward@acm.org
   Copyright (C) 2005-2010 Nicholas Nethercote
      njn@valgrind.org
   Copyright (C) 2006-2010 OpenWorks LLP
      info@open-works.co.uk

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

#ifndef __PUB_CORE_VKISCNUMS_H
#define __PUB_CORE_VKISCNUMS_H

//--------------------------------------------------------------------
// PURPOSE: This module holds all the syscall numbers, and a handful of
// operations relating to them.
//
// On Linux they are a bunch of #define'd constants of the form
// __NR_name, and this file must contain nothing else, since it will
// be included in assembly code (m_trampoline.S).
//
// On AIX the __NR_name consts are renamings of global variables which
// tell us the number for each syscall.  This elaboration is necessary
// because on AIX the syscall numbers are not constant; they can be
// different for each process (in principle; in practice they rarely
// change).  32- and 64-bit AIX5 share a common "implementation".
//
// On Darwin the __NR_name consts are #define'd constants which are
// encoded using various macros.  32- and 64-bit Darwin share a common
// "implementation" also.
//
// Note that most of the actual code for this module is in include/vki/, but
// you should not directly #include any file in include/vki; instead #include
// only one of pub_{core,tool}_vkiscnums{,_asm}.h.
//--------------------------------------------------------------------

/* Most unfortunately, all the kernel decls are visible to tools.  Not
   really necessary, but to avoid this would require some tedious
   refactoring of the sources.  Anyway, we live with this kludge, and
   that means the only thing to be done here is ... */

#include "pub_core_vkiscnums_asm.h"
#include "pub_tool_vkiscnums.h"


#if defined(VGO_linux)
   // Nothing

#elif defined(VGO_aix5)
/* Bind the given syscall name to the given number.  Returns True if
   successful, False if the name is unknown. */
extern Bool VG_(aix5_register_syscall)( Int, UChar* );

#elif defined(VGO_darwin)
   // Nothing

#else
#  error Unknown OS
#endif

#endif // __PUB_CORE_VKISCNUMS_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
