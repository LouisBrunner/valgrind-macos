/*--------------------------------------------------------------------*/
/*--- Replacements for strlen() and strnlen(), which run on the    ---*/
/*--- simulated CPU.                                               ---*/
/*--------------------------------------------------------------------*/

/*
  This file is part of DRD, a heavyweight Valgrind tool for
  detecting threading errors. The code below has been extracted
  from memchec/mc_replace_strmem.c, which has the following copyright
  notice:

  Copyright (C) 2000-2013 Julian Seward
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
#include "pub_tool_clreq.h"


#define STRCHR(soname, fnname) \
   char* VG_REPLACE_FUNCTION_ZU(soname,fnname)(const char* s, int c); \
   char* VG_REPLACE_FUNCTION_ZU(soname,fnname)(const char* s, int c) \
   {                                                                 \
      HChar ch = (HChar)c;                                           \
      const HChar* p = s;                                            \
      while (True) {                                                 \
         if (*p == ch) return (HChar *)p;                            \
         if (*p == 0) return NULL;                                   \
         p++;                                                        \
      }                                                              \
   }

// Apparently index() is the same thing as strchr()
#if defined(VGO_linux)
 STRCHR(VG_Z_LIBC_SONAME,          strchr)
 STRCHR(VG_Z_LIBC_SONAME,          index)
 STRCHR(VG_Z_LD_LINUX_SO_2,        strchr)
 STRCHR(VG_Z_LD_LINUX_SO_2,        index)
 STRCHR(VG_Z_LD_LINUX_X86_64_SO_2, strchr)
 STRCHR(VG_Z_LD_LINUX_X86_64_SO_2, index)
#elif defined(VGO_darwin)
 STRCHR(VG_Z_LIBC_SONAME,          strchr)
 STRCHR(VG_Z_LIBC_SONAME,          index)
#endif


#define STRNLEN(soname, fnname)                                         \
   SizeT VG_REPLACE_FUNCTION_ZU(soname,fnname) ( const char* str, SizeT n ); \
   SizeT VG_REPLACE_FUNCTION_ZU(soname,fnname) ( const char* str, SizeT n ) \
   {                                                                    \
      SizeT i = 0;                                                      \
      while (i < n && str[i] != 0) i++;                                 \
      return i;                                                         \
   }

#if defined(VGO_linux)
 STRNLEN(VG_Z_LIBC_SONAME, strnlen)
#elif defined(VGO_darwin)
 STRNLEN(VG_Z_LIBC_SONAME, strnlen)
#endif


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

#if defined(VGO_linux)
 STRLEN(VG_Z_LIBC_SONAME,          strlen)
 STRLEN(VG_Z_LD_LINUX_SO_2,        strlen)
 STRLEN(VG_Z_LD_LINUX_X86_64_SO_2, strlen)
 STRLEN(VG_Z_LIBC_SONAME,          __GI_strlen)
#elif defined(VGO_darwin)
 STRLEN(VG_Z_LIBC_SONAME,          strlen)
#endif


#define STRCPY(soname, fnname)                                          \
 char* VG_REPLACE_FUNCTION_ZU(soname, fnname)(char* dst, const char* src); \
 char* VG_REPLACE_FUNCTION_ZU(soname, fnname)(char* dst, const char* src) \
 {                                                                      \
    HChar* dst_orig = dst;                                              \
                                                                        \
    while (*src) *dst++ = *src++;                                       \
    *dst = 0;                                                           \
                                                                        \
    return dst_orig;                                                    \
 }

#if defined(VGO_linux)
 STRCPY(VG_Z_LIBC_SONAME, strcpy)
#elif defined(VGO_darwin)
 STRCPY(VG_Z_LIBC_SONAME, strcpy)
#endif


#define STRCMP(soname, fnname)                                          \
 int VG_REPLACE_FUNCTION_ZU(soname,fnname)(const char* s1, const char* s2); \
 int VG_REPLACE_FUNCTION_ZU(soname,fnname)(const char* s1, const char* s2) \
 {                                                                      \
    register UChar c1;                                                  \
    register UChar c2;                                                  \
    while (True) {                                                      \
       c1 = *(UChar *)s1;                                               \
       c2 = *(UChar *)s2;                                               \
       if (c1 != c2) break;                                             \
       if (c1 == 0) break;                                              \
       s1++; s2++;                                                      \
    }                                                                   \
    if ((UChar)c1 < (UChar)c2) return -1;                               \
    if ((UChar)c1 > (UChar)c2) return 1;                                \
    return 0;                                                           \
 }

#if defined(VGO_linux)
 STRCMP(VG_Z_LIBC_SONAME,          strcmp)
 STRCMP(VG_Z_LD_LINUX_X86_64_SO_2, strcmp)
 STRCMP(VG_Z_LD64_SO_1,            strcmp)
#elif defined(VGO_darwin)
 STRCMP(VG_Z_LIBC_SONAME,          strcmp)
#endif


#define MEMCPY(soname, fnname)                                          \
   void* VG_REPLACE_FUNCTION_ZU(soname,fnname)                          \
        (void *dst, const void *src, SizeT len);                        \
   void* VG_REPLACE_FUNCTION_ZU(soname,fnname)                          \
        (void *dst, const void *src, SizeT len)                         \
   {                                                                    \
      const Addr WS = sizeof(UWord); /* 8 or 4 */                       \
      const Addr WM = WS - 1;        /* 7 or 3 */                       \
                                                                        \
      if (len > 0) {                                                    \
         if (dst < src) {                                               \
                                                                        \
            /* Copying backwards. */                                    \
            SizeT n = len;                                              \
            Addr  d = (Addr)dst;                                        \
            Addr  s = (Addr)src;                                        \
                                                                        \
            if (((s^d) & WM) == 0) {                                    \
               /* s and d have same UWord alignment. */                 \
               /* Pull up to a UWord boundary. */                       \
               while ((s & WM) != 0 && n >= 1)                          \
                  { *(UChar*)d = *(UChar*)s; s += 1; d += 1; n -= 1; }  \
               /* Copy UWords. */                                       \
               while (n >= WS)                                          \
                  { *(UWord*)d = *(UWord*)s; s += WS; d += WS; n -= WS; } \
               if (n == 0)                                              \
                  return dst;                                           \
            }                                                           \
            if (((s|d) & 1) == 0) {                                     \
               /* Both are 16-aligned; copy what we can thusly. */      \
               while (n >= 2)                                           \
                  { *(UShort*)d = *(UShort*)s; s += 2; d += 2; n -= 2; } \
            }                                                           \
            /* Copy leftovers, or everything if misaligned. */          \
            while (n >= 1)                                              \
               { *(UChar*)d = *(UChar*)s; s += 1; d += 1; n -= 1; }     \
                                                                        \
         } else if (dst > src) {                                        \
                                                                        \
            SizeT n = len;                                              \
            Addr  d = ((Addr)dst) + n;                                  \
            Addr  s = ((Addr)src) + n;                                  \
                                                                        \
            /* Copying forwards. */                                     \
            if (((s^d) & WM) == 0) {                                    \
               /* s and d have same UWord alignment. */                 \
               /* Back down to a UWord boundary. */                     \
               while ((s & WM) != 0 && n >= 1)                          \
                  { s -= 1; d -= 1; *(UChar*)d = *(UChar*)s; n -= 1; }  \
               /* Copy UWords. */                                       \
               while (n >= WS)                                          \
                  { s -= WS; d -= WS; *(UWord*)d = *(UWord*)s; n -= WS; } \
               if (n == 0)                                              \
                  return dst;                                           \
            }                                                           \
            if (((s|d) & 1) == 0) {                                     \
               /* Both are 16-aligned; copy what we can thusly. */      \
               while (n >= 2)                                           \
                  { s -= 2; d -= 2; *(UShort*)d = *(UShort*)s; n -= 2; } \
            }                                                           \
            /* Copy leftovers, or everything if misaligned. */          \
            while (n >= 1)                                              \
               { s -= 1; d -= 1; *(UChar*)d = *(UChar*)s; n -= 1; }     \
                                                                        \
         }                                                              \
      }                                                                 \
                                                                        \
      return dst;                                                       \
   }

#if defined(VGO_linux)
 MEMCPY(VG_Z_LIBC_SONAME,    memcpy)
 MEMCPY(VG_Z_LD_SO_1,        memcpy) /* ld.so.1 */
 MEMCPY(VG_Z_LD64_SO_1,      memcpy) /* ld64.so.1 */
 /* icc9 blats these around all over the place.  Not only in the main
    executable but various .so's.  They are highly tuned and read
    memory beyond the source boundary (although work correctly and
    never go across page boundaries), so give errors when run
    natively, at least for misaligned source arg.  Just intercepting
    in the exe only until we understand more about the problem.  See
    http://bugs.kde.org/show_bug.cgi?id=139776
 */
 MEMCPY(NONE, _intel_fast_memcpy)

#elif defined(VGO_darwin)
# if DARWIN_VERS <= DARWIN_10_6
  MEMCPY(VG_Z_LIBC_SONAME,  memcpy)
# endif
 MEMCPY(VG_Z_LIBC_SONAME,  memcpyZDVARIANTZDsse3x) /* memcpy$VARIANT$sse3x */
 MEMCPY(VG_Z_LIBC_SONAME,  memcpyZDVARIANTZDsse42) /* memcpy$VARIANT$sse42 */

#endif


/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
