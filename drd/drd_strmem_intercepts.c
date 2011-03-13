/* -*- mode: C; c-basic-offset: 3; indent-tabs-mode: nil; -*- */

/*--------------------------------------------------------------------*/
/*--- Replacements for strlen() and strnlen(), which run on the    ---*/
/*--- simulated CPU.                                               ---*/
/*--------------------------------------------------------------------*/

/*
  This file is part of DRD, a heavyweight Valgrind tool for
  detecting threading errors. The code below has been extracted
  from memchec/mc_replace_strmem.c, which has the following copyright
  notice:

  Copyright (C) 2000-2010 Julian Seward
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

#include "pub_tool_basics.h"
#include "pub_tool_hashtable.h"
#include "pub_tool_redir.h"
#include "pub_tool_tooliface.h"
#include "valgrind.h"


#define STRNLEN(soname, fnname)                                         \
   SizeT VG_REPLACE_FUNCTION_ZU(soname,fnname) ( const char* str, SizeT n ); \
   SizeT VG_REPLACE_FUNCTION_ZU(soname,fnname) ( const char* str, SizeT n ) \
   {                                                                    \
      SizeT i = 0;                                                      \
      while (i < n && str[i] != 0) i++;                                 \
      return i;                                                         \
   }

STRNLEN(VG_Z_LIBC_SONAME, strnlen)


// Note that this replacement often doesn't get used because gcc inlines
// calls to strlen() with its own built-in version.  This can be very
// confusing if you aren't expecting it.  Other small functions in this file
// may also be inline by gcc.
#define STRLEN(soname, fnname)                                          \
   SizeT VG_REPLACE_FUNCTION_ZU(soname,fnname)( const char* str );      \
   SizeT VG_REPLACE_FUNCTION_ZU(soname,fnname)( const char* str )       \
   {                                                                    \
      SizeT i = 0;                                                      \
      while (str[i] != 0) i++;                                          \
      return i;                                                         \
   }

STRLEN(VG_Z_LIBC_SONAME,          strlen)
#if defined(VGO_linux)
STRLEN(VG_Z_LD_LINUX_SO_2,        strlen)
STRLEN(VG_Z_LD_LINUX_X86_64_SO_2, strlen)
#endif

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
