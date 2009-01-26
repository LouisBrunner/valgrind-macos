
/*--------------------------------------------------------------------*/
/*--- Ptrcheck: a pointer-use checker.             pc_intercepts.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Ptrcheck, a Valgrind tool for checking pointer
   use in programs.

   Copyright (C) 2003-2008 Nicholas Nethercote
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

/* Nothing actually in here.  However it appears this file is needed
   to make malloc intercepting work. (jrs, 2 july 08 -- not sure about
   that).
*/

#include "pub_tool_basics.h"
#include "pub_tool_hashtable.h"
#include "pub_tool_redir.h"
#include "pub_tool_tooliface.h"
#include "valgrind.h"


/* The following intercepts are copied verbatim from
   memcheck/mc_replace_strmem.c.  If you copy more in, please keep
   them in the same order as in mc_replace_strmem.c. */

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
STRLEN(m_ld_so_1,              strlen)


#define STRCMP(soname, fnname) \
   int VG_REPLACE_FUNCTION_ZU(soname,fnname) \
          ( const char* s1, const char* s2 ); \
   int VG_REPLACE_FUNCTION_ZU(soname,fnname) \
          ( const char* s1, const char* s2 ) \
   { \
      register unsigned char c1; \
      register unsigned char c2; \
      while (True) { \
         c1 = *(unsigned char *)s1; \
         c2 = *(unsigned char *)s2; \
         if (c1 != c2) break; \
         if (c1 == 0) break; \
         s1++; s2++; \
      } \
      if ((unsigned char)c1 < (unsigned char)c2) return -1; \
      if ((unsigned char)c1 > (unsigned char)c2) return 1; \
      return 0; \
   }

STRCMP(m_libc_soname,          strcmp)
STRCMP(m_ld_linux_x86_64_so_2, strcmp)
STRCMP(m_ld64_so_1,            strcmp)


#define MEMCPY(soname, fnname) \
   void* VG_REPLACE_FUNCTION_ZU(soname,fnname) \
            ( void *dst, const void *src, SizeT sz ); \
   void* VG_REPLACE_FUNCTION_ZU(soname,fnname) \
            ( void *dest, const void *src, SizeT sz ) \
   { \
   const UChar*  s  = (const UChar*)src; \
         UChar*  d  =       (UChar*)dest; \
   const UWord*  sW = (const UWord*)src; \
         UWord*  dW =       (UWord*)dest; \
   const UWord   al = sizeof(UWord)-1; \
   \
   if (0 == (((UWord)dW) & al) && 0 == (((UWord)sW) & al)) { \
      while (sz >= 4 * sizeof(UWord)) { \
         dW[0] = sW[0]; \
         dW[1] = sW[1]; \
         dW[2] = sW[2]; \
         dW[3] = sW[3]; \
         sz -= 4 * sizeof(UWord); \
         dW += 4; \
         sW += 4; \
      } \
      if (sz == 0) \
         return dest; \
      while (sz >= 1 * sizeof(UWord)) { \
         dW[0] = sW[0]; \
         sz -= 1 * sizeof(UWord); \
         dW += 1; \
         sW += 1; \
      } \
      if (sz == 0) \
         return dest; \
      s = (const UChar*)sW; \
      d = (UChar*)dW; \
   } \
   \
   while (sz--) \
      *d++ = *s++; \
   \
   return dest; \
   }

MEMCPY(m_libc_soname, memcpy)
MEMCPY(m_ld_so_1,     memcpy) /* ld.so.1 */
MEMCPY(m_ld64_so_1,   memcpy) /* ld64.so.1 */


/* Copy SRC to DEST, returning the address of the terminating '\0' in
   DEST. (minor variant of strcpy) */
#define STPCPY(soname, fnname) \
   char* VG_REPLACE_FUNCTION_ZU(soname,fnname) ( char* dst, const char* src ); \
   char* VG_REPLACE_FUNCTION_ZU(soname,fnname) ( char* dst, const char* src ) \
   { \
      while (*src) *dst++ = *src++; \
      *dst = 0; \
      \
      return dst; \
   }

STPCPY(m_libc_soname,         stpcpy)
STPCPY(m_ld_linux_so_2,        stpcpy)
STPCPY(m_ld_linux_x86_64_so_2, stpcpy)


/*--------------------------------------------------------------------*/
/*--- end                                          pc_intercepts.c ---*/
/*--------------------------------------------------------------------*/
