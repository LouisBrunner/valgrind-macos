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


/*---------------------- strrchr ----------------------*/

#define STRRCHR(soname, fnname) \
   char* VG_REPLACE_FUNCTION_EZU(20010,soname,fnname)( const char* s, int c ); \
   char* VG_REPLACE_FUNCTION_EZU(20010,soname,fnname)( const char* s, int c ) \
   {                                                                    \
      HChar ch = (HChar)c;                                              \
      const HChar* p = s;                                               \
      const HChar* last = NULL;                                         \
      while (True) {                                                    \
         if (*p == ch) last = p;                                        \
         if (*p == 0) return (HChar *)last;                             \
         p++;                                                           \
      }                                                                 \
   }

// Apparently rindex() is the same thing as strrchr()
#if defined(VGO_linux)
 STRRCHR(VG_Z_LIBC_SONAME,   strrchr)
 STRRCHR(VG_Z_LIBC_SONAME,   rindex)
 STRRCHR(VG_Z_LIBC_SONAME,   __GI_strrchr)
 STRRCHR(VG_Z_LD_LINUX_SO_2, rindex)
#if defined(VGPV_arm_linux_android) || defined(VGPV_x86_linux_android)
  STRRCHR(NONE, __dl_strrchr); /* in /system/bin/linker */
#endif
#elif defined(VGO_darwin)
 //STRRCHR(VG_Z_LIBC_SONAME,   strrchr)
 //STRRCHR(VG_Z_LIBC_SONAME,   rindex)
 //STRRCHR(VG_Z_DYLD,          strrchr)
 //STRRCHR(VG_Z_DYLD,          rindex)
 STRRCHR(VG_Z_LIBC_SONAME, strrchr)
#endif
   

/*---------------------- strchr ----------------------*/

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
 STRCHR(VG_Z_LIBC_SONAME,          __GI_strchr)
 STRCHR(VG_Z_LIBC_SONAME,          index)
 STRCHR(VG_Z_LD_LINUX_SO_2,        strchr)
 STRCHR(VG_Z_LD_LINUX_SO_2,        index)
 STRCHR(VG_Z_LD_LINUX_X86_64_SO_2, strchr)
 STRCHR(VG_Z_LD_LINUX_X86_64_SO_2, index)
#elif defined(VGO_darwin)
 STRCHR(VG_Z_LIBC_SONAME,          strchr)
 STRCHR(VG_Z_LIBC_SONAME,          index)
#endif


/*---------------------- strnlen ----------------------*/

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


/*---------------------- strlen ----------------------*/

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


/*---------------------- strcpy ----------------------*/

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
 STRCPY(VG_Z_LIBC_SONAME, __GI_strcpy)
#elif defined(VGO_darwin)
 STRCPY(VG_Z_LIBC_SONAME, strcpy)
#endif


/*---------------------- strcmp ----------------------*/

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
 STRCMP(VG_Z_LIBC_SONAME,          __GI_strcmp)
 STRCMP(VG_Z_LD_LINUX_X86_64_SO_2, strcmp)
 STRCMP(VG_Z_LD64_SO_1,            strcmp)
#elif defined(VGO_darwin)
 STRCMP(VG_Z_LIBC_SONAME,          strcmp)
#endif


/*---------------------- memchr ----------------------*/

#define MEMCHR(soname, fnname) \
 void* VG_REPLACE_FUNCTION_EZU(20170,soname,fnname)     \
      (const void *s, int c, SizeT n);                  \
 void* VG_REPLACE_FUNCTION_EZU(20170,soname,fnname)     \
      (const void *s, int c, SizeT n)                   \
 {                                                      \
    SizeT i;                                            \
    UChar c0 = (UChar)c;                                \
    UChar* p = (UChar*)s;                               \
    for (i = 0; i < n; i++)                             \
       if (p[i] == c0) return (void*)(&p[i]);           \
    return NULL;                                        \
 }

#if defined(VGO_linux)
 MEMCHR(VG_Z_LIBC_SONAME, memchr)
 MEMCHR(VG_Z_LIBC_SONAME, __GI_memchr)
#elif defined(VGO_darwin)
 //MEMCHR(VG_Z_LIBC_SONAME, memchr)
 //MEMCHR(VG_Z_DYLD,        memchr)
#endif


/*---------------------- memrchr ----------------------*/

#define MEMRCHR(soname, fnname) \
 void* VG_REPLACE_FUNCTION_EZU(20360,soname,fnname)     \
      (const void *s, int c, SizeT n);                  \
 void* VG_REPLACE_FUNCTION_EZU(20360,soname,fnname)     \
      (const void *s, int c, SizeT n)                   \
 {                                                      \
    SizeT i;                                            \
    UChar c0 = (UChar)c;                                \
    UChar* p = (UChar*)s;                               \
    for (i = 0; i < n; i++)                             \
       if (p[n-1-i] == c0) return (void*)(&p[n-1-i]);   \
    return NULL;                                        \
 }

#if defined(VGO_linux)
 MEMRCHR(VG_Z_LIBC_SONAME, memrchr)
#elif defined(VGO_darwin)
 //MEMRCHR(VG_Z_LIBC_SONAME, memrchr)
 //MEMRCHR(VG_Z_DYLD,        memrchr)
#endif


/*---------------------- memcpy ----------------------*/

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
 MEMCPY(VG_Z_LIBC_SONAME,    __GI_memcpy)
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


/*---------------------- memcmp ----------------------*/

#define MEMCMP(soname, fnname) \
 int VG_REPLACE_FUNCTION_EZU(20190,soname,fnname)         \
      (const void *s1V, const void *s2V, SizeT n);        \
 int VG_REPLACE_FUNCTION_EZU(20190,soname,fnname)         \
      (const void *s1V, const void *s2V, SizeT n)       \
 {                                                      \
    int res;                                            \
    UChar a0;                                           \
    UChar b0;                                           \
    const UChar* s1 = s1V;                              \
    const UChar* s2 = s2V;                              \
                                                        \
    while (n != 0) {                                    \
       a0 = s1[0];                                      \
       b0 = s2[0];                                      \
       s1 += 1;                                         \
       s2 += 1;                                         \
       res = ((int)a0) - ((int)b0);                     \
       if (res != 0)                                    \
          return res;                                   \
       n -= 1;                                          \
    }                                                   \
    return 0;                                           \
 }

#if defined(VGO_linux)
 MEMCMP(VG_Z_LIBC_SONAME, memcmp)
 MEMCMP(VG_Z_LIBC_SONAME, __GI_memcmp)
 MEMCMP(VG_Z_LIBC_SONAME, bcmp)
 MEMCMP(VG_Z_LD_SO_1,     bcmp)
#elif defined(VGO_darwin)
 //MEMCMP(VG_Z_LIBC_SONAME, memcmp)
 //MEMCMP(VG_Z_LIBC_SONAME, bcmp)
 //MEMCMP(VG_Z_DYLD,        memcmp)
 //MEMCMP(VG_Z_DYLD,        bcmp)
#endif


/*---------------------- stpcpy ----------------------*/

/* Copy SRC to DEST, returning the address of the terminating '\0' in
   DEST. (minor variant of strcpy) */
#define STPCPY(soname, fnname)                                          \
 char* VG_REPLACE_FUNCTION_EZU(20200,soname,fnname)                     \
      (char* dst, const char* src);                                     \
 char* VG_REPLACE_FUNCTION_EZU(20200,soname,fnname)                     \
      (char* dst, const char* src)                                      \
 {                                                                      \
    while (*src) *dst++ = *src++;                                       \
    *dst = 0;                                                           \
                                                                        \
    return dst;                                                         \
 }

#if defined(VGO_linux)
 STPCPY(VG_Z_LIBC_SONAME,          stpcpy)
 STPCPY(VG_Z_LIBC_SONAME,          __GI_stpcpy)
 STPCPY(VG_Z_LD_LINUX_SO_2,        stpcpy)
 STPCPY(VG_Z_LD_LINUX_X86_64_SO_2, stpcpy)
#elif defined(VGO_darwin)
 //STPCPY(VG_Z_LIBC_SONAME,          stpcpy)
 //STPCPY(VG_Z_DYLD,                 stpcpy)
#endif


/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
