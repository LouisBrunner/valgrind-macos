
/*--------------------------------------------------------------------*/
/*--- Top level for kernel interface declarations.                 ---*/
/*---                                         pub_core_vkiscnums.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2007 Julian Seward
      jseward@acm.org
   Copyright (C) 2005-2007 Nicholas Nethercote
      njn@valgrind.org
   Copyright (C) 2006-2007 OpenWorks LLP
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

/* Most unfortunately, all the kernel decls are visible to tools.  Not
   really necessary, but to avoid this would require some tedious
   refactoring of the sources.  Anyway, we live with this kludge, and
   that means the only thing to be done here is ... */

#include "pub_tool_vkiscnums.h"


/* Make it possible to include this file in assembly sources. */
#if !defined(VG_IN_ASSEMBLY_SOURCE)

#if defined(VGO_aix5)
/* Bind the given syscall name to the given number.  Returns True if
   successful, False if the name is unknown. */
extern Bool VG_(aix5_register_syscall)( Int, UChar* );
#endif

#endif /* !defined(VG_IN_ASSEMBLY_SOURCE) */

#endif // __PUB_CORE_VKISCNUMS_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
