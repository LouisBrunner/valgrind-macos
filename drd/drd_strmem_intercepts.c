
/*--------------------------------------------------------------------*/
/*--- Replacements for strlen() and strnlen(), which run on the    ---*/
/*--- simulated CPU.                                               ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of DRD, a heavyweight Valgrind tool for
   detecting threading errors. The code below has been extracted
   from memchec/mc_replace_strmem.c, which has the following copyright
   notice:

   Copyright (C) 2000-2008 Julian Seward 
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


/* --------- Some handy Z-encoded names. --------- */

/* --- Soname of the standard C library. --- */

#if defined(VGO_linux)
#  define  m_libc_soname     libcZdsoZa              // libc.so*
#elif defined(VGP_ppc32_aix5)
   /* AIX has both /usr/lib/libc.a and /usr/lib/libc_r.a. */
#  define  m_libc_soname     libcZaZdaZLshrZdoZR     // libc*.a(shr.o)
#elif defined(VGP_ppc64_aix5)
#  define  m_libc_soname     libcZaZdaZLshrZu64ZdoZR // libc*.a(shr_64.o)
#else
#  error "Unknown platform"
#endif

/* --- Sonames for Linux ELF linkers. --- */

#define  m_ld_linux_so_2         ldZhlinuxZdsoZd2           // ld-linux.so.2
#define  m_ld_linux_x86_64_so_2  ldZhlinuxZhx86Zh64ZdsoZd2  // ld-linux-x86-64.so.2
#define  m_ld64_so_1             ld64ZdsoZd1                // ld64.so.1
#define  m_ld_so_1               ldZdsoZd1                  // ld.so.1


#define STRNLEN(soname, fnname) \
   SizeT VG_REPLACE_FUNCTION_ZU(soname,fnname) ( const char* str, SizeT n ); \
   SizeT VG_REPLACE_FUNCTION_ZU(soname,fnname) ( const char* str, SizeT n ) \
   { \
      SizeT i = 0; \
      while (i < n && str[i] != 0) i++; \
      return i; \
   }

STRNLEN(m_libc_soname, strnlen)
   

// Note that this replacement often doesn't get used because gcc inlines
// calls to strlen() with its own built-in version.  This can be very
// confusing if you aren't expecting it.  Other small functions in this file
// may also be inline by gcc.
#define STRLEN(soname, fnname) \
   SizeT VG_REPLACE_FUNCTION_ZU(soname,fnname)( const char* str ); \
   SizeT VG_REPLACE_FUNCTION_ZU(soname,fnname)( const char* str ) \
   { \
      SizeT i = 0; \
      while (str[i] != 0) i++; \
      return i; \
   }

STRLEN(m_libc_soname,          strlen)
STRLEN(m_ld_linux_so_2,        strlen)
STRLEN(m_ld_linux_x86_64_so_2, strlen)

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
