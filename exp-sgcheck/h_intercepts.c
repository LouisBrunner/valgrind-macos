
/*--------------------------------------------------------------------*/
/*--- Ptrcheck: a pointer-use checker.             pc_intercepts.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Ptrcheck, a Valgrind tool for checking pointer
   use in programs.

   Copyright (C) 2003-2013 Nicholas Nethercote
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
#include "pub_tool_clreq.h"


/* The following intercepts are copied verbatim from
   memcheck/mc_replace_strmem.c.  If you copy more in, please keep
   them in the same order as in mc_replace_strmem.c. */


#define STRRCHR(soname, fnname) \
   char* VG_REPLACE_FUNCTION_ZU(soname,fnname)( const char* s, int c ); \
   char* VG_REPLACE_FUNCTION_ZU(soname,fnname)( const char* s, int c ) \
   { \
      HChar ch = (HChar)c;   \
      const HChar* p = s;       \
      const HChar* last = NULL; \
      while (True) { \
         if (*p == ch) last = p; \
         if (*p == 0) return (HChar *)last;     \
         p++; \
      } \
   }

// Apparently rindex() is the same thing as strrchr()
STRRCHR(VG_Z_LIBC_SONAME,   strrchr)
STRRCHR(VG_Z_LIBC_SONAME,   rindex)
#if defined(VGO_linux)
STRRCHR(VG_Z_LIBC_SONAME,   __GI_strrchr)
STRRCHR(VG_Z_LD_LINUX_SO_2, rindex)
#elif defined(VGO_darwin)
STRRCHR(VG_Z_DYLD,          strrchr)
STRRCHR(VG_Z_DYLD,          rindex)
#endif


#define STRCHR(soname, fnname) \
   char* VG_REPLACE_FUNCTION_ZU(soname,fnname) ( const char* s, int c ); \
   char* VG_REPLACE_FUNCTION_ZU(soname,fnname) ( const char* s, int c ) \
   { \
      HChar  ch = (HChar)c ; \
      const HChar* p  = s;   \
      while (True) { \
         if (*p == ch) return (HChar *)p; \
         if (*p == 0) return NULL; \
         p++; \
      } \
   }

// Apparently index() is the same thing as strchr()
STRCHR(VG_Z_LIBC_SONAME,          strchr)
STRCHR(VG_Z_LIBC_SONAME,          index)
#if defined(VGO_linux)
STRCHR(VG_Z_LIBC_SONAME,          __GI_strchr)
STRCHR(VG_Z_LD_LINUX_SO_2,        strchr)
STRCHR(VG_Z_LD_LINUX_SO_2,        index)
STRCHR(VG_Z_LD_LINUX_X86_64_SO_2, strchr)
STRCHR(VG_Z_LD_LINUX_X86_64_SO_2, index)
#elif defined(VGO_darwin)
STRCHR(VG_Z_DYLD,                 strchr)
STRCHR(VG_Z_DYLD,                 index)
#endif


#define STRNLEN(soname, fnname) \
   SizeT VG_REPLACE_FUNCTION_ZU(soname,fnname) ( const char* str, SizeT n ); \
   SizeT VG_REPLACE_FUNCTION_ZU(soname,fnname) ( const char* str, SizeT n ) \
   { \
      SizeT i = 0; \
      while (i < n && str[i] != 0) i++; \
      return i; \
   }

STRNLEN(VG_Z_LIBC_SONAME, strnlen)


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

STRLEN(VG_Z_LIBC_SONAME,          strlen)
#if defined(VGO_linux)
STRLEN(VG_Z_LIBC_SONAME,          __GI_strlen)
STRLEN(VG_Z_LD_LINUX_SO_2,        strlen)
STRLEN(VG_Z_LD_LINUX_X86_64_SO_2, strlen)
STRLEN(VG_Z_LD_SO_1,              strlen)
#endif


#define STRCPY(soname, fnname) \
   char* VG_REPLACE_FUNCTION_ZU(soname, fnname) ( char* dst, const char* src ); \
   char* VG_REPLACE_FUNCTION_ZU(soname, fnname) ( char* dst, const char* src ) \
   { \
      HChar* dst_orig = dst; \
      \
      while (*src) *dst++ = *src++; \
      *dst = 0; \
      \
      return dst_orig; \
   }

STRCPY(VG_Z_LIBC_SONAME, strcpy)
#if defined(VGO_linux)
STRCPY(VG_Z_LIBC_SONAME, __GI_strcpy)
#elif defined(VGO_darwin)
STRCPY(VG_Z_DYLD,        strcpy)
#endif


#define STRNCMP(soname, fnname) \
   int VG_REPLACE_FUNCTION_ZU(soname,fnname) \
          ( const char* s1, const char* s2, SizeT nmax ); \
   int VG_REPLACE_FUNCTION_ZU(soname,fnname) \
          ( const char* s1, const char* s2, SizeT nmax ) \
   { \
      SizeT n = 0; \
      while (True) { \
         if (n >= nmax) return 0; \
         if (*s1 == 0 && *s2 == 0) return 0; \
         if (*s1 == 0) return -1; \
         if (*s2 == 0) return 1; \
         \
         if (*(const unsigned char*)s1 < *(const unsigned char*)s2) return -1; \
         if (*(const unsigned char*)s1 > *(const unsigned char*)s2) return 1; \
         \
         s1++; s2++; n++; \
      } \
   }

STRNCMP(VG_Z_LIBC_SONAME, strncmp)
#if defined(VGO_linux)
STRNCMP(VG_Z_LIBC_SONAME, __GI_strncmp)
#elif defined(VGO_darwin)
STRNCMP(VG_Z_DYLD,        strncmp)
#endif


#define STRCMP(soname, fnname) \
   int VG_REPLACE_FUNCTION_ZU(soname,fnname) \
          ( const char* s1, const char* s2 ); \
   int VG_REPLACE_FUNCTION_ZU(soname,fnname) \
          ( const char* s1, const char* s2 ) \
   { \
      register UChar c1; \
      register UChar c2; \
      while (True) { \
         c1 = *(const UChar *)s1; \
         c2 = *(const UChar *)s2; \
         if (c1 != c2) break; \
         if (c1 == 0) break; \
         s1++; s2++; \
      } \
      if ((UChar)c1 < (UChar)c2) return -1; \
      if ((UChar)c1 > (UChar)c2) return 1; \
      return 0; \
   }

STRCMP(VG_Z_LIBC_SONAME,          strcmp)
#if defined(VGO_linux)
STRCMP(VG_Z_LIBC_SONAME,          __GI_strcmp)
STRCMP(VG_Z_LD_LINUX_X86_64_SO_2, strcmp)
STRCMP(VG_Z_LD64_SO_1,            strcmp)
#endif


#define MEMCHR(soname, fnname) \
   void* VG_REPLACE_FUNCTION_ZU(soname,fnname) (const void *s, int c, SizeT n); \
   void* VG_REPLACE_FUNCTION_ZU(soname,fnname) (const void *s, int c, SizeT n) \
   { \
      SizeT i; \
      UChar c0 = (UChar)c; \
      const UChar* p = (const UChar*)s; \
      for (i = 0; i < n; i++) \
         if (p[i] == c0) return (void*)(&p[i]); \
      return NULL; \
   }

MEMCHR(VG_Z_LIBC_SONAME, memchr)
#if defined(VGO_darwin)
MEMCHR(VG_Z_DYLD,        memchr)
#endif


#define MEMCPY(soname, fnname) \
   void* VG_REPLACE_FUNCTION_ZU(soname,fnname) \
            ( void *dst, const void *src, SizeT len ); \
   void* VG_REPLACE_FUNCTION_ZU(soname,fnname) \
            ( void *dst, const void *src, SizeT len ) \
   { \
      const Addr WS = sizeof(UWord); /* 8 or 4 */ \
      const Addr WM = WS - 1;        /* 7 or 3 */ \
      \
      if (len > 0) { \
         if (dst < src) { \
         \
            /* Copying backwards. */ \
            SizeT n = len; \
            Addr  d = (Addr)dst; \
            Addr  s = (Addr)src; \
            \
            if (((s^d) & WM) == 0) { \
               /* s and d have same UWord alignment. */ \
               /* Pull up to a UWord boundary. */ \
               while ((s & WM) != 0 && n >= 1) \
                  { *(UChar*)d = *(UChar*)s; s += 1; d += 1; n -= 1; } \
               /* Copy UWords. */ \
               while (n >= WS) \
                  { *(UWord*)d = *(UWord*)s; s += WS; d += WS; n -= WS; } \
               if (n == 0) \
                  return dst; \
            } \
            if (((s|d) & 1) == 0) { \
               /* Both are 16-aligned; copy what we can thusly. */ \
               while (n >= 2) \
                  { *(UShort*)d = *(UShort*)s; s += 2; d += 2; n -= 2; } \
            } \
            /* Copy leftovers, or everything if misaligned. */ \
            while (n >= 1) \
               { *(UChar*)d = *(UChar*)s; s += 1; d += 1; n -= 1; } \
         \
         } else if (dst > src) { \
         \
            SizeT n = len; \
            Addr  d = ((Addr)dst) + n; \
            Addr  s = ((Addr)src) + n; \
            \
            /* Copying forwards. */ \
            if (((s^d) & WM) == 0) { \
               /* s and d have same UWord alignment. */ \
               /* Back down to a UWord boundary. */ \
               while ((s & WM) != 0 && n >= 1) \
                  { s -= 1; d -= 1; *(UChar*)d = *(UChar*)s; n -= 1; } \
               /* Copy UWords. */ \
               while (n >= WS) \
                  { s -= WS; d -= WS; *(UWord*)d = *(UWord*)s; n -= WS; } \
               if (n == 0) \
                  return dst; \
            } \
            if (((s|d) & 1) == 0) { \
               /* Both are 16-aligned; copy what we can thusly. */ \
               while (n >= 2) \
                  { s -= 2; d -= 2; *(UShort*)d = *(UShort*)s; n -= 2; } \
            } \
            /* Copy leftovers, or everything if misaligned. */ \
            while (n >= 1) \
               { s -= 1; d -= 1; *(UChar*)d = *(UChar*)s; n -= 1; } \
            \
         } \
      } \
      \
      return dst; \
   }

MEMCPY(VG_Z_LIBC_SONAME, memcpy)
#if defined(VGO_linux)
MEMCPY(VG_Z_LD_SO_1,     memcpy) /* ld.so.1 */
MEMCPY(VG_Z_LD64_SO_1,   memcpy) /* ld64.so.1 */
#endif


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

STPCPY(VG_Z_LIBC_SONAME,          stpcpy)
#if defined(VGO_linux)
STPCPY(VG_Z_LD_LINUX_SO_2,        stpcpy)
STPCPY(VG_Z_LD_LINUX_X86_64_SO_2, stpcpy)
#endif


/* Find the first occurrence of C in S.  */
#define GLIBC232_RAWMEMCHR(soname, fnname) \
   void* VG_REPLACE_FUNCTION_ZU(soname,fnname) (const void* s, int c_in); \
   void* VG_REPLACE_FUNCTION_ZU(soname,fnname) (const void* s, int c_in) \
   { \
      UChar c = (UChar)c_in; \
      const UChar* char_ptr = s; \
      while (1) { \
        if (*char_ptr == c) return (void *)char_ptr;    \
         char_ptr++; \
      } \
   }

GLIBC232_RAWMEMCHR(VG_Z_LIBC_SONAME, rawmemchr)
#if defined (VGO_linux)
GLIBC232_RAWMEMCHR(VG_Z_LIBC_SONAME, __GI___rawmemchr)
#endif


#define STRSTR(soname, fnname) \
   char* VG_REPLACE_FUNCTION_ZU(soname,fnname) \
         (const char* haystack, const char* needle); \
   char* VG_REPLACE_FUNCTION_ZU(soname,fnname) \
         (const char* haystack, const char* needle) \
   { \
      const HChar* h = haystack; \
      const HChar* n = needle; \
      \
      /* find the length of n, not including terminating zero */ \
      UWord nlen = 0; \
      while (n[nlen]) nlen++; \
      \
      /* if n is the empty string, match immediately. */ \
      if (nlen == 0) return (HChar *)h;                  \
      \
      /* assert(nlen >= 1); */ \
      HChar n0 = n[0]; \
      \
      while (1) { \
         const HChar hh = *h; \
         if (hh == 0) return NULL; \
         if (hh != n0) { h++; continue; } \
         \
         UWord i; \
         for (i = 0; i < nlen; i++) { \
            if (n[i] != h[i]) \
               break; \
         } \
         /* assert(i >= 0 && i <= nlen); */ \
         if (i == nlen) \
           return (HChar *)h;                   \
         \
         h++; \
      } \
   }

#if defined(VGO_linux)
STRSTR(VG_Z_LIBC_SONAME,          strstr)
#endif


#define STRPBRK(soname, fnname) \
   char* VG_REPLACE_FUNCTION_ZU(soname,fnname) \
         (const char* sV, const char* acceptV); \
   char* VG_REPLACE_FUNCTION_ZU(soname,fnname) \
         (const char* sV, const char* acceptV) \
   { \
      const HChar* s = sV; \
      const HChar* accept = acceptV; \
      \
      /*  find the length of 'accept', not including terminating zero */ \
      UWord nacc = 0; \
      while (accept[nacc]) nacc++; \
      \
      /* if n is the empty string, fail immediately. */ \
      if (nacc == 0) return NULL; \
      \
      /* assert(nacc >= 1); */ \
      while (1) { \
         UWord i; \
         HChar sc = *s; \
         if (sc == 0) \
            break; \
         for (i = 0; i < nacc; i++) { \
            if (sc == accept[i]) \
              return (HChar *)s; \
         } \
         s++; \
      } \
      \
      return NULL; \
   }

#if defined(VGO_linux)
STRPBRK(VG_Z_LIBC_SONAME,          strpbrk)
#endif


/*--------------------------------------------------------------------*/
/*--- end                                          pc_intercepts.c ---*/
/*--------------------------------------------------------------------*/
