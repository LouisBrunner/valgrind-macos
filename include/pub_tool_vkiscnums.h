
/*--------------------------------------------------------------------*/
/*--- Syscall numbers and related operations. pub_tool_vkiscnums.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2005-2011 Nicholas Nethercote
      njn@valgrind.org
   Copyright (C) 2006-2011 OpenWorks LLP
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

#ifndef __PUB_TOOL_VKISCNUMS_H
#define __PUB_TOOL_VKISCNUMS_H

#include "pub_tool_vkiscnums_asm.h"


// This converts a syscall number into a string, suitable for printing.  It is
// needed because some platforms (Darwin) munge sysnums in various ways.
// It is used in places where the sycall name will be printed alongside.
extern Char* VG_(sysnum_string)      (Word sysnum, SizeT n_buf, Char* buf);

// This is like VG_(sysnum_string), but prints extra info if needed.  It is
// called in places where the syscall name will *not* be printed alongside.
extern Char* VG_(sysnum_string_extra)(Word sysnum, SizeT n_buf, Char* buf);

// Macros that make the above functions easier to use by declaring a local
// buffer.
#define VG_SYSNUM_STRING(sysnum) \
   ({ Char qq_zz_buf[32]; VG_(sysnum_string)(sysnum, 32, qq_zz_buf); })
#define VG_SYSNUM_STRING_EXTRA(sysnum) \
   ({ Char qq_zz_buf[64]; VG_(sysnum_string_extra)(sysnum, 64, qq_zz_buf); })


#endif   // __PUB_TOOL_VKISCNUMS_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
