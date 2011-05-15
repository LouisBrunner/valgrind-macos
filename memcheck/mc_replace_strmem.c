
/*--------------------------------------------------------------------*/
/*--- Replacements for strcpy(), memcpy() et al, which run on the  ---*/
/*--- simulated CPU.                                               ---*/
/*---                                          mc_replace_strmem.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of MemCheck, a heavyweight Valgrind tool for
   detecting memory errors.

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

#include "mc_include.h"
#include "memcheck.h"

/* ---------------------------------------------------------------------
   We have our own versions of these functions for two reasons:
   (a) it allows us to do overlap checking
   (b) some of the normal versions are hyper-optimised, which fools
       Memcheck and cause spurious value warnings.  Our versions are
       simpler.

   Note that overenthusiastic use of PLT bypassing by the glibc people also
   means that we need to patch multiple versions of some of the functions to
   our own implementations.

   THEY RUN ON THE SIMD CPU!
   ------------------------------------------------------------------ */

/* Figure out if [dst .. dst+dstlen-1] overlaps with 
                 [src .. src+srclen-1].
   We assume that the address ranges do not wrap around
   (which is safe since on Linux addresses >= 0xC0000000
   are not accessible and the program will segfault in this
   circumstance, presumably).
*/
static __inline__
Bool is_overlap ( void* dst, const void* src, SizeT dstlen, SizeT srclen )
{
   Addr loS, hiS, loD, hiD;

   if (dstlen == 0 || srclen == 0)
      return False;

   loS = (Addr)src;
   loD = (Addr)dst;
   hiS = loS + srclen - 1;
   hiD = loD + dstlen - 1;

   /* So figure out if [loS .. hiS] overlaps with [loD .. hiD]. */
   if (loS < loD) {
      return !(hiS < loD);
   }
   else if (loD < loS) {
      return !(hiD < loS);
   }
   else { 
      /* They start at same place.  Since we know neither of them has
         zero length, they must overlap. */
      return True;
   }
}

// This is a macro rather than a function because we don't want to have an
// extra function in the stack trace.
#define RECORD_OVERLAP_ERROR(s, src, dst, len)                  \
  VALGRIND_DO_CLIENT_REQUEST_EXPR(0,                            \
                  _VG_USERREQ__MEMCHECK_RECORD_OVERLAP_ERROR,   \
                  s, src, dst, len, 0)


#define STRRCHR(soname, fnname) \
   char* VG_REPLACE_FUNCTION_ZU(soname,fnname)( const char* s, int c ); \
   char* VG_REPLACE_FUNCTION_ZU(soname,fnname)( const char* s, int c ) \
   { \
      UChar  ch   = (UChar)((UInt)c); \
      UChar* p    = (UChar*)s; \
      UChar* last = NULL; \
      while (True) { \
         if (*p == ch) last = p; \
         if (*p == 0) return last; \
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
      UChar  ch = (UChar)((UInt)c); \
      UChar* p  = (UChar*)s; \
      while (True) { \
         if (*p == ch) return p; \
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


#define STRCAT(soname, fnname) \
   char* VG_REPLACE_FUNCTION_ZU(soname,fnname) ( char* dst, const char* src ); \
   char* VG_REPLACE_FUNCTION_ZU(soname,fnname) ( char* dst, const char* src ) \
   { \
      const Char* src_orig = src; \
            Char* dst_orig = dst; \
      while (*dst) dst++; \
      while (*src) *dst++ = *src++; \
      *dst = 0; \
      \
      /* This is a bit redundant, I think;  any overlap and the strcat will */ \
      /* go forever... or until a seg fault occurs. */ \
      if (is_overlap(dst_orig,  \
                     src_orig,  \
                     (Addr)dst-(Addr)dst_orig+1,  \
                     (Addr)src-(Addr)src_orig+1)) \
         RECORD_OVERLAP_ERROR("strcat", dst_orig, src_orig, 0); \
      \
      return dst_orig; \
   }

STRCAT(VG_Z_LIBC_SONAME, strcat)
#if defined(VGO_linux)
STRCAT(VG_Z_LIBC_SONAME, __GI_strcat)
#endif

#define STRNCAT(soname, fnname) \
   char* VG_REPLACE_FUNCTION_ZU(soname,fnname) \
            ( char* dst, const char* src, SizeT n ); \
   char* VG_REPLACE_FUNCTION_ZU(soname,fnname) \
            ( char* dst, const char* src, SizeT n ) \
   { \
      const Char* src_orig = src; \
            Char* dst_orig = dst; \
      SizeT m = 0; \
      \
      while (*dst) dst++; \
      while (m < n && *src) { m++; *dst++ = *src++; } /* concat <= n chars */ \
      *dst = 0;                                       /* always add null   */ \
      \
      /* This checks for overlap after copying, unavoidable without */ \
      /* pre-counting lengths... should be ok */ \
      if (is_overlap(dst_orig,  \
                     src_orig,  \
                     (Addr)dst-(Addr)dst_orig+1,  \
                     (Addr)src-(Addr)src_orig+1)) \
         RECORD_OVERLAP_ERROR("strncat", dst_orig, src_orig, n); \
      \
      return dst_orig; \
   }

STRNCAT(VG_Z_LIBC_SONAME, strncat)
#if defined(VGO_darwin)
STRNCAT(VG_Z_DYLD,        strncat)
#endif


/* Append src to dst. n is the size of dst's buffer. dst is guaranteed 
   to be nul-terminated after the copy, unless n <= strlen(dst_orig). 
   Returns min(n, strlen(dst_orig)) + strlen(src_orig). 
   Truncation occurred if retval >= n. 
*/
#define STRLCAT(soname, fnname) \
    SizeT VG_REPLACE_FUNCTION_ZU(soname,fnname) \
        ( char* dst, const char* src, SizeT n ); \
    SizeT VG_REPLACE_FUNCTION_ZU(soname,fnname) \
        ( char* dst, const char* src, SizeT n ) \
   { \
      const Char* src_orig = src; \
      Char* dst_orig = dst; \
      SizeT m = 0; \
\
      while (m < n && *dst) { m++; dst++; } \
      if (m < n) { \
         /* Fill as far as dst_orig[n-2], then nul-terminate. */ \
         while (m < n-1 && *src) { m++; *dst++ = *src++; } \
         *dst = 0; \
      } else { \
         /* No space to copy anything to dst. m == n */ \
      } \
      /* Finish counting min(n, strlen(dst_orig)) + strlen(src_orig) */ \
      while (*src) { m++; src++; } \
      /* This checks for overlap after copying, unavoidable without */ \
      /* pre-counting lengths... should be ok */ \
      if (is_overlap(dst_orig,  \
                     src_orig,  \
                     (Addr)dst-(Addr)dst_orig+1,  \
                     (Addr)src-(Addr)src_orig+1)) \
         RECORD_OVERLAP_ERROR("strlcat", dst_orig, src_orig, n); \
\
      return m; \
   }

#if defined(VGO_darwin)
STRLCAT(VG_Z_LIBC_SONAME, strlcat)
STRLCAT(VG_Z_DYLD,        strlcat)
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
#if defined(VGO_linux)
STRNLEN(VG_Z_LIBC_SONAME, __GI_strnlen)
#endif
   

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
#endif


#define STRCPY(soname, fnname) \
   char* VG_REPLACE_FUNCTION_ZU(soname, fnname) ( char* dst, const char* src ); \
   char* VG_REPLACE_FUNCTION_ZU(soname, fnname) ( char* dst, const char* src ) \
   { \
      const Char* src_orig = src; \
            Char* dst_orig = dst; \
      \
      while (*src) *dst++ = *src++; \
      *dst = 0; \
      \
      /* This checks for overlap after copying, unavoidable without */ \
      /* pre-counting length... should be ok */ \
      if (is_overlap(dst_orig,  \
                     src_orig,  \
                     (Addr)dst-(Addr)dst_orig+1,  \
                     (Addr)src-(Addr)src_orig+1)) \
         RECORD_OVERLAP_ERROR("strcpy", dst_orig, src_orig, 0); \
      \
      return dst_orig; \
   }

STRCPY(VG_Z_LIBC_SONAME, strcpy)
#if defined(VGO_linux)
STRCPY(VG_Z_LIBC_SONAME, __GI_strcpy)
#elif defined(VGO_darwin)
STRCPY(VG_Z_DYLD,        strcpy)
#endif


#define STRNCPY(soname, fnname) \
   char* VG_REPLACE_FUNCTION_ZU(soname, fnname) \
            ( char* dst, const char* src, SizeT n ); \
   char* VG_REPLACE_FUNCTION_ZU(soname, fnname) \
            ( char* dst, const char* src, SizeT n ) \
   { \
      const Char* src_orig = src; \
            Char* dst_orig = dst; \
      SizeT m = 0; \
      \
      while (m   < n && *src) { m++; *dst++ = *src++; } \
      /* Check for overlap after copying; all n bytes of dst are relevant, */ \
      /* but only m+1 bytes of src if terminator was found */ \
      if (is_overlap(dst_orig, src_orig, n, (m < n) ? m+1 : n)) \
         RECORD_OVERLAP_ERROR("strncpy", dst, src, n); \
      while (m++ < n) *dst++ = 0;         /* must pad remainder with nulls */ \
 \
      return dst_orig; \
   }

STRNCPY(VG_Z_LIBC_SONAME, strncpy)
#if defined(VGO_linux)
STRNCPY(VG_Z_LIBC_SONAME, __GI_strncpy)
#elif defined(VGO_darwin)
STRNCPY(VG_Z_DYLD,        strncpy)
#endif


/* Copy up to n-1 bytes from src to dst. Then nul-terminate dst if n > 0. 
   Returns strlen(src). Does not zero-fill the remainder of dst. */
#define STRLCPY(soname, fnname) \
   SizeT VG_REPLACE_FUNCTION_ZU(soname, fnname) \
       ( char* dst, const char* src, SizeT n ); \
   SizeT VG_REPLACE_FUNCTION_ZU(soname, fnname) \
       ( char* dst, const char* src, SizeT n ) \
   { \
      const char* src_orig = src; \
      char* dst_orig = dst; \
      SizeT m = 0; \
\
      while (m < n-1 && *src) { m++; *dst++ = *src++; } \
      /* m non-nul bytes have now been copied, and m <= n-1. */ \
      /* Check for overlap after copying; all n bytes of dst are relevant, */ \
      /* but only m+1 bytes of src if terminator was found */ \
      if (is_overlap(dst_orig, src_orig, n, (m < n) ? m+1 : n)) \
          RECORD_OVERLAP_ERROR("strlcpy", dst, src, n); \
      /* Nul-terminate dst. */ \
      if (n > 0) *dst = 0; \
      /* Finish counting strlen(src). */ \
      while (*src) src++; \
      return src - src_orig; \
   }

#if defined(VGO_darwin)
STRLCPY(VG_Z_LIBC_SONAME, strlcpy)
STRLCPY(VG_Z_DYLD,        strlcpy)
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
         if (*(unsigned char*)s1 < *(unsigned char*)s2) return -1; \
         if (*(unsigned char*)s1 > *(unsigned char*)s2) return 1; \
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


#define STRCASECMP(soname, fnname) \
   int VG_REPLACE_FUNCTION_ZU(soname,fnname) \
          ( const char* s1, const char* s2 ); \
   int VG_REPLACE_FUNCTION_ZU(soname,fnname) \
          ( const char* s1, const char* s2 ) \
   { \
      extern int tolower(int); \
      register unsigned char c1; \
      register unsigned char c2; \
      while (True) { \
         c1 = tolower(*(unsigned char *)s1); \
         c2 = tolower(*(unsigned char *)s2); \
         if (c1 != c2) break; \
         if (c1 == 0) break; \
         s1++; s2++; \
      } \
      if ((unsigned char)c1 < (unsigned char)c2) return -1; \
      if ((unsigned char)c1 > (unsigned char)c2) return 1; \
      return 0; \
   }

STRCASECMP(VG_Z_LIBC_SONAME, strcasecmp)
#if defined(VGO_linux)
STRCASECMP(VG_Z_LIBC_SONAME, __GI_strcasecmp)
#endif


#define STRNCASECMP(soname, fnname) \
   int VG_REPLACE_FUNCTION_ZU(soname,fnname) \
          ( const char* s1, const char* s2, SizeT nmax ); \
   int VG_REPLACE_FUNCTION_ZU(soname,fnname) \
          ( const char* s1, const char* s2, SizeT nmax ) \
   { \
      extern int tolower(int); \
      SizeT n = 0; \
      while (True) { \
         if (n >= nmax) return 0; \
         if (*s1 == 0 && *s2 == 0) return 0; \
         if (*s1 == 0) return -1; \
         if (*s2 == 0) return 1; \
         \
         if (tolower(*(unsigned char*)s1) < tolower(*(unsigned char*)s2)) return -1; \
         if (tolower(*(unsigned char*)s1) > tolower(*(unsigned char*)s2)) return 1; \
         \
         s1++; s2++; n++; \
      } \
   }

STRNCASECMP(VG_Z_LIBC_SONAME, strncasecmp)
#if defined(VGO_linux)
STRNCASECMP(VG_Z_LIBC_SONAME, __GI_strncasecmp)
#elif defined(VGO_darwin)
STRNCASECMP(VG_Z_DYLD,        strncasecmp)
#endif


#define STRCASECMP_L(soname, fnname) \
   int VG_REPLACE_FUNCTION_ZU(soname,fnname) \
          ( const char* s1, const char* s2, void* locale ); \
   int VG_REPLACE_FUNCTION_ZU(soname,fnname) \
          ( const char* s1, const char* s2, void* locale ) \
   { \
      extern int tolower_l(int, void*) __attribute__((weak));    \
      register unsigned char c1; \
      register unsigned char c2; \
      while (True) { \
         c1 = tolower_l(*(unsigned char *)s1, locale); \
         c2 = tolower_l(*(unsigned char *)s2, locale); \
         if (c1 != c2) break; \
         if (c1 == 0) break; \
         s1++; s2++; \
      } \
      if ((unsigned char)c1 < (unsigned char)c2) return -1; \
      if ((unsigned char)c1 > (unsigned char)c2) return 1; \
      return 0; \
   }

STRCASECMP_L(VG_Z_LIBC_SONAME, strcasecmp_l)
#if defined(VGO_linux)
STRCASECMP_L(VG_Z_LIBC_SONAME, __GI_strcasecmp_l)
#endif


#define STRNCASECMP_L(soname, fnname) \
   int VG_REPLACE_FUNCTION_ZU(soname,fnname) \
          ( const char* s1, const char* s2, SizeT nmax, void* locale ); \
   int VG_REPLACE_FUNCTION_ZU(soname,fnname) \
          ( const char* s1, const char* s2, SizeT nmax, void* locale ) \
   { \
      extern int tolower_l(int, void*) __attribute__((weak));    \
      SizeT n = 0; \
      while (True) { \
         if (n >= nmax) return 0; \
         if (*s1 == 0 && *s2 == 0) return 0; \
         if (*s1 == 0) return -1; \
         if (*s2 == 0) return 1; \
         \
         if (tolower_l(*(unsigned char*)s1, locale) < tolower_l(*(unsigned char*)s2, locale)) return -1; \
         if (tolower_l(*(unsigned char*)s1, locale) > tolower_l(*(unsigned char*)s2, locale)) return 1; \
         \
         s1++; s2++; n++; \
      } \
   }

STRNCASECMP_L(VG_Z_LIBC_SONAME, strncasecmp_l)
#if defined(VGO_linux)
STRNCASECMP_L(VG_Z_LIBC_SONAME, __GI_strncasecmp_l)
#elif defined(VGO_darwin)
STRNCASECMP_L(VG_Z_DYLD,        strncasecmp_l)
#endif


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
      UChar* p = (UChar*)s; \
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
      if (is_overlap(dst, src, len, len)) \
         RECORD_OVERLAP_ERROR("memcpy", dst, src, len); \
      \
      const Addr WS = sizeof(UWord); /* 8 or 4 */ \
      const Addr WM = WS - 1;        /* 7 or 3 */ \
      \
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
      \
      return dst; \
   }

MEMCPY(VG_Z_LIBC_SONAME, memcpy)
#if defined(VGO_linux)
MEMCPY(VG_Z_LD_SO_1,     memcpy) /* ld.so.1 */
MEMCPY(VG_Z_LD64_SO_1,   memcpy) /* ld64.so.1 */
#elif defined(VGO_darwin)
MEMCPY(VG_Z_DYLD,        memcpy)
#endif
/* icc9 blats these around all over the place.  Not only in the main
   executable but various .so's.  They are highly tuned and read
   memory beyond the source boundary (although work correctly and
   never go across page boundaries), so give errors when run natively,
   at least for misaligned source arg.  Just intercepting in the exe
   only until we understand more about the problem.  See
   http://bugs.kde.org/show_bug.cgi?id=139776
 */
MEMCPY(NONE, _intel_fast_memcpy)


#define MEMCMP(soname, fnname) \
   int VG_REPLACE_FUNCTION_ZU(soname,fnname) \
          ( const void *s1V, const void *s2V, SizeT n ); \
   int VG_REPLACE_FUNCTION_ZU(soname,fnname) \
          ( const void *s1V, const void *s2V, SizeT n ) \
   { \
      int res; \
      unsigned char a0; \
      unsigned char b0; \
      unsigned char* s1 = (unsigned char*)s1V; \
      unsigned char* s2 = (unsigned char*)s2V; \
      \
      while (n != 0) { \
         a0 = s1[0]; \
         b0 = s2[0]; \
         s1 += 1; \
         s2 += 1; \
         res = ((int)a0) - ((int)b0); \
         if (res != 0) \
            return res; \
         n -= 1; \
      } \
      return 0; \
   }

MEMCMP(VG_Z_LIBC_SONAME, memcmp)
MEMCMP(VG_Z_LIBC_SONAME, bcmp)
#if defined(VGO_linux)
MEMCMP(VG_Z_LD_SO_1,     bcmp)
#elif defined(VGO_darwin)
MEMCMP(VG_Z_DYLD,        memcmp)
MEMCMP(VG_Z_DYLD,        bcmp)
#endif


/* Copy SRC to DEST, returning the address of the terminating '\0' in
   DEST. (minor variant of strcpy) */
#define STPCPY(soname, fnname) \
   char* VG_REPLACE_FUNCTION_ZU(soname,fnname) ( char* dst, const char* src ); \
   char* VG_REPLACE_FUNCTION_ZU(soname,fnname) ( char* dst, const char* src ) \
   { \
      const Char* src_orig = src; \
            Char* dst_orig = dst; \
      \
      while (*src) *dst++ = *src++; \
      *dst = 0; \
      \
      /* This checks for overlap after copying, unavoidable without */ \
      /* pre-counting length... should be ok */ \
      if (is_overlap(dst_orig,  \
                     src_orig,  \
                     (Addr)dst-(Addr)dst_orig+1,  \
                     (Addr)src-(Addr)src_orig+1)) \
         RECORD_OVERLAP_ERROR("stpcpy", dst_orig, src_orig, 0); \
      \
      return dst; \
   }

STPCPY(VG_Z_LIBC_SONAME,          stpcpy)
#if defined(VGO_linux)
STPCPY(VG_Z_LIBC_SONAME,          __GI_stpcpy)
STPCPY(VG_Z_LD_LINUX_SO_2,        stpcpy)
STPCPY(VG_Z_LD_LINUX_X86_64_SO_2, stpcpy)
#elif defined(VGO_darwin)
STPCPY(VG_Z_DYLD,                 stpcpy)
#endif


#define MEMSET(soname, fnname) \
   void* VG_REPLACE_FUNCTION_ZU(soname,fnname)(void *s, Int c, SizeT n); \
   void* VG_REPLACE_FUNCTION_ZU(soname,fnname)(void *s, Int c, SizeT n) \
   { \
      Addr a  = (Addr)s;   \
      UInt c4 = (c & 0xFF); \
      c4 = (c4 << 8) | c4; \
      c4 = (c4 << 16) | c4; \
      while ((a & 3) != 0 && n >= 1) \
         { *(UChar*)a = (UChar)c; a += 1; n -= 1; } \
      while (n >= 4) \
         { *(UInt*)a = c4; a += 4; n -= 4; } \
      while (n >= 1) \
         { *(UChar*)a = (UChar)c; a += 1; n -= 1; } \
      return s; \
   }

MEMSET(VG_Z_LIBC_SONAME, memset)
#if defined(VGO_darwin)
MEMSET(VG_Z_DYLD,        memset)
#endif


#define MEMMOVE(soname, fnname) \
   void* VG_REPLACE_FUNCTION_ZU(soname,fnname) \
            (void *dstV, const void *srcV, SizeT n); \
   void* VG_REPLACE_FUNCTION_ZU(soname,fnname) \
            (void *dstV, const void *srcV, SizeT n) \
   { \
      SizeT i; \
      Char* dst = (Char*)dstV; \
      Char* src = (Char*)srcV; \
      if (dst < src) { \
         for (i = 0; i < n; i++) \
            dst[i] = src[i]; \
      } \
      else  \
      if (dst > src) { \
         for (i = 0; i < n; i++) \
            dst[n-i-1] = src[n-i-1]; \
      } \
      return dst; \
   }

MEMMOVE(VG_Z_LIBC_SONAME, memmove)
#if defined(VGO_darwin)
MEMMOVE(VG_Z_DYLD,        memmove)
#endif


#define BCOPY(soname, fnname) \
   void VG_REPLACE_FUNCTION_ZU(soname,fnname) \
            (const void *srcV, void *dstV, SizeT n); \
   void VG_REPLACE_FUNCTION_ZU(soname,fnname) \
            (const void *srcV, void *dstV, SizeT n) \
   { \
      SizeT i; \
      Char* dst = (Char*)dstV; \
      Char* src = (Char*)srcV; \
      if (dst < src) { \
         for (i = 0; i < n; i++) \
            dst[i] = src[i]; \
      } \
      else  \
      if (dst > src) { \
         for (i = 0; i < n; i++) \
            dst[n-i-1] = src[n-i-1]; \
      } \
   }

#if defined(VGO_darwin)
BCOPY(VG_Z_LIBC_SONAME, bcopy)
BCOPY(VG_Z_DYLD,        bcopy)
#endif


/* glibc 2.5 variant of memmove which checks the dest is big enough.
   There is no specific part of glibc that this is copied from. */
#define GLIBC25___MEMMOVE_CHK(soname, fnname) \
   void* VG_REPLACE_FUNCTION_ZU(soname,fnname) \
            (void *dstV, const void *srcV, SizeT n, SizeT destlen); \
   void* VG_REPLACE_FUNCTION_ZU(soname,fnname) \
            (void *dstV, const void *srcV, SizeT n, SizeT destlen) \
   { \
      extern void _exit(int status); \
      SizeT i; \
      Char* dst = (Char*)dstV; \
      Char* src = (Char*)srcV; \
      if (destlen < n) \
         goto badness; \
      if (dst < src) { \
         for (i = 0; i < n; i++) \
            dst[i] = src[i]; \
      } \
      else  \
      if (dst > src) { \
         for (i = 0; i < n; i++) \
            dst[n-i-1] = src[n-i-1]; \
      } \
      return dst; \
     badness: \
      VALGRIND_PRINTF_BACKTRACE( \
         "*** memmove_chk: buffer overflow detected ***: " \
         "program terminated\n"); \
     _exit(127); \
     /*NOTREACHED*/ \
     return NULL; \
   }

GLIBC25___MEMMOVE_CHK(VG_Z_LIBC_SONAME, __memmove_chk)


/* Find the first occurrence of C in S or the final NUL byte.  */
#define GLIBC232_STRCHRNUL(soname, fnname) \
   char* VG_REPLACE_FUNCTION_ZU(soname,fnname) (const char* s, int c_in); \
   char* VG_REPLACE_FUNCTION_ZU(soname,fnname) (const char* s, int c_in) \
   { \
      unsigned char  c        = (unsigned char) c_in; \
      unsigned char* char_ptr = (unsigned char *)s; \
      while (1) { \
         if (*char_ptr == 0) return char_ptr; \
         if (*char_ptr == c) return char_ptr; \
         char_ptr++; \
      } \
   }

GLIBC232_STRCHRNUL(VG_Z_LIBC_SONAME, strchrnul)


/* Find the first occurrence of C in S.  */
#define GLIBC232_RAWMEMCHR(soname, fnname) \
   char* VG_REPLACE_FUNCTION_ZU(soname,fnname) (const char* s, int c_in); \
   char* VG_REPLACE_FUNCTION_ZU(soname,fnname) (const char* s, int c_in) \
   { \
      unsigned char  c        = (unsigned char) c_in; \
      unsigned char* char_ptr = (unsigned char *)s; \
      while (1) { \
         if (*char_ptr == c) return char_ptr; \
         char_ptr++; \
      } \
   }

GLIBC232_RAWMEMCHR(VG_Z_LIBC_SONAME, rawmemchr)
#if defined (VGO_linux)
GLIBC232_RAWMEMCHR(VG_Z_LIBC_SONAME, __GI___rawmemchr)
#endif

/* glibc variant of strcpy that checks the dest is big enough.
   Copied from glibc-2.5/debug/test-strcpy_chk.c. */
#define GLIBC25___STRCPY_CHK(soname,fnname) \
   char* VG_REPLACE_FUNCTION_ZU(soname,fnname) \
                               (char* dst, const char* src, SizeT len); \
   char* VG_REPLACE_FUNCTION_ZU(soname,fnname) \
                               (char* dst, const char* src, SizeT len) \
   { \
      extern void _exit(int status); \
      char* ret = dst; \
      if (! len) \
         goto badness; \
      while ((*dst++ = *src++) != '\0') \
         if (--len == 0) \
            goto badness; \
      return ret; \
     badness: \
      VALGRIND_PRINTF_BACKTRACE( \
         "*** strcpy_chk: buffer overflow detected ***: " \
         "program terminated\n"); \
     _exit(127); \
     /*NOTREACHED*/ \
     return NULL; \
   }

GLIBC25___STRCPY_CHK(VG_Z_LIBC_SONAME, __strcpy_chk)


/* glibc variant of stpcpy that checks the dest is big enough.
   Copied from glibc-2.5/debug/test-stpcpy_chk.c. */
#define GLIBC25___STPCPY_CHK(soname,fnname) \
   char* VG_REPLACE_FUNCTION_ZU(soname,fnname) \
                               (char* dst, const char* src, SizeT len); \
   char* VG_REPLACE_FUNCTION_ZU(soname,fnname) \
                               (char* dst, const char* src, SizeT len) \
   { \
      extern void _exit(int status); \
      if (! len) \
         goto badness; \
      while ((*dst++ = *src++) != '\0') \
         if (--len == 0) \
            goto badness; \
      return dst - 1; \
     badness: \
      VALGRIND_PRINTF_BACKTRACE( \
         "*** stpcpy_chk: buffer overflow detected ***: " \
         "program terminated\n"); \
     _exit(127); \
     /*NOTREACHED*/ \
     return NULL; \
   }

GLIBC25___STPCPY_CHK(VG_Z_LIBC_SONAME, __stpcpy_chk)


/* mempcpy */
#define GLIBC25_MEMPCPY(soname, fnname) \
   void* VG_REPLACE_FUNCTION_ZU(soname,fnname) \
            ( void *dst, const void *src, SizeT len ); \
   void* VG_REPLACE_FUNCTION_ZU(soname,fnname) \
            ( void *dst, const void *src, SizeT len ) \
   { \
      register char *d; \
      register char *s; \
      SizeT len_saved = len; \
      \
      if (len == 0) \
         return dst; \
      \
      if (is_overlap(dst, src, len, len)) \
         RECORD_OVERLAP_ERROR("mempcpy", dst, src, len); \
      \
      if ( dst > src ) { \
         d = (char *)dst + len - 1; \
         s = (char *)src + len - 1; \
         while ( len-- ) { \
            *d-- = *s--; \
         } \
      } else if ( dst < src ) { \
         d = (char *)dst; \
         s = (char *)src; \
         while ( len-- ) { \
            *d++ = *s++; \
         } \
      } \
      return (void*)( ((char*)dst) + len_saved ); \
   }

GLIBC25_MEMPCPY(VG_Z_LIBC_SONAME, mempcpy)
#if defined(VGO_linux)
GLIBC25_MEMPCPY(VG_Z_LD_SO_1,     mempcpy) /* ld.so.1 */
#endif


#define GLIBC26___MEMCPY_CHK(soname, fnname) \
   void* VG_REPLACE_FUNCTION_ZU(soname,fnname) \
            (void* dst, const void* src, SizeT len, SizeT dstlen ); \
   void* VG_REPLACE_FUNCTION_ZU(soname,fnname) \
            (void* dst, const void* src, SizeT len, SizeT dstlen ) \
   { \
      extern void _exit(int status); \
      register char *d; \
      register char *s; \
      \
      if (dstlen < len) goto badness; \
      \
      if (len == 0) \
         return dst; \
      \
      if (is_overlap(dst, src, len, len)) \
         RECORD_OVERLAP_ERROR("memcpy_chk", dst, src, len); \
      \
      if ( dst > src ) { \
         d = (char *)dst + len - 1; \
         s = (char *)src + len - 1; \
         while ( len-- ) { \
            *d-- = *s--; \
         } \
      } else if ( dst < src ) { \
         d = (char *)dst; \
         s = (char *)src; \
         while ( len-- ) { \
            *d++ = *s++; \
         } \
      } \
      return dst; \
     badness: \
      VALGRIND_PRINTF_BACKTRACE( \
         "*** memcpy_chk: buffer overflow detected ***: " \
         "program terminated\n"); \
     _exit(127); \
     /*NOTREACHED*/ \
     return NULL; \
   }

GLIBC26___MEMCPY_CHK(VG_Z_LIBC_SONAME, __memcpy_chk)


#define STRSTR(soname, fnname) \
   void* VG_REPLACE_FUNCTION_ZU(soname,fnname) \
         (void* haystack, void* needle); \
   void* VG_REPLACE_FUNCTION_ZU(soname,fnname) \
         (void* haystack, void* needle) \
   { \
      UChar* h = (UChar*)haystack; \
      UChar* n = (UChar*)needle; \
      \
      /* find the length of n, not including terminating zero */ \
      UWord nlen = 0; \
      while (n[nlen]) nlen++; \
      \
      /* if n is the empty string, match immediately. */ \
      if (nlen == 0) return h; \
      \
      /* assert(nlen >= 1); */ \
      UChar n0 = n[0]; \
      \
      while (1) { \
         UChar hh = *h; \
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
            return h; \
         \
         h++; \
      } \
   }

#if defined(VGO_linux)
STRSTR(VG_Z_LIBC_SONAME,          strstr)
#endif


#define STRPBRK(soname, fnname) \
   void* VG_REPLACE_FUNCTION_ZU(soname,fnname) \
         (void* sV, void* acceptV); \
   void* VG_REPLACE_FUNCTION_ZU(soname,fnname) \
         (void* sV, void* acceptV) \
   { \
      UChar* s = (UChar*)sV; \
      UChar* accept = (UChar*)acceptV; \
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
         UChar sc = *s; \
         if (sc == 0) \
            break; \
         for (i = 0; i < nacc; i++) { \
            if (sc == accept[i]) \
               return s; \
         } \
         s++; \
      } \
      \
      return NULL; \
   }

#if defined(VGO_linux)
STRPBRK(VG_Z_LIBC_SONAME,          strpbrk)
#endif


#define STRCSPN(soname, fnname) \
   SizeT VG_REPLACE_FUNCTION_ZU(soname,fnname) \
         (void* sV, void* rejectV); \
   SizeT VG_REPLACE_FUNCTION_ZU(soname,fnname) \
         (void* sV, void* rejectV) \
   { \
      UChar* s = (UChar*)sV; \
      UChar* reject = (UChar*)rejectV; \
      \
      /* find the length of 'reject', not including terminating zero */ \
      UWord nrej = 0; \
      while (reject[nrej]) nrej++; \
      \
      UWord len = 0; \
      while (1) { \
         UWord i; \
         UChar sc = *s; \
         if (sc == 0) \
            break; \
         for (i = 0; i < nrej; i++) { \
            if (sc == reject[i]) \
               break; \
         } \
         /* assert(i >= 0 && i <= nrej); */ \
         if (i < nrej) \
            break; \
         s++; \
         len++; \
      } \
      \
      return len; \
   }

#if defined(VGO_linux)
STRCSPN(VG_Z_LIBC_SONAME,          strcspn)
#endif


// And here's a validated strspn replacement, should it
// become necessary.
//UWord mystrspn( UChar* s, UChar* accept )
//{
//   /* find the length of 'accept', not including terminating zero */
//   UWord nacc = 0;
//   while (accept[nacc]) nacc++;
//   if (nacc == 0) return 0;
//
//   UWord len = 0;
//   while (1) {
//      UWord i;
//      UChar sc = *s;
//      if (sc == 0)
//         break;
//      for (i = 0; i < nacc; i++) {
//         if (sc == accept[i])
//            break;
//      }
//      assert(i >= 0 && i <= nacc);
//      if (i == nacc)
//         break;
//      s++;
//      len++;
//   }
//
//   return len;
//}


/*------------------------------------------------------------*/
/*--- Improve definedness checking of process environment  ---*/
/*------------------------------------------------------------*/

#if defined(VGO_linux)

/* putenv */
int VG_WRAP_FUNCTION_ZU(VG_Z_LIBC_SONAME, putenv) (char* string);
int VG_WRAP_FUNCTION_ZU(VG_Z_LIBC_SONAME, putenv) (char* string)
{
    OrigFn fn;
    Word result;
    const char* p = string;
    VALGRIND_GET_ORIG_FN(fn);
    /* Now by walking over the string we magically produce
       traces when hitting undefined memory. */
    if (p)
        while (*p++)
            ;
    CALL_FN_W_W(result, fn, string);
    return result;
}

/* unsetenv */
int VG_WRAP_FUNCTION_ZU(VG_Z_LIBC_SONAME, unsetenv) (const char* name);
int VG_WRAP_FUNCTION_ZU(VG_Z_LIBC_SONAME, unsetenv) (const char* name)
{
    OrigFn fn;
    Word result;
    const char* p = name;
    VALGRIND_GET_ORIG_FN(fn);
    /* Now by walking over the string we magically produce
       traces when hitting undefined memory. */
    if (p)
        while (*p++)
            ;
    CALL_FN_W_W(result, fn, name);
    return result;
}

/* setenv */
int VG_WRAP_FUNCTION_ZU(VG_Z_LIBC_SONAME, setenv)
    (const char* name, const char* value, int overwrite);
int VG_WRAP_FUNCTION_ZU(VG_Z_LIBC_SONAME, setenv)
    (const char* name, const char* value, int overwrite)
{
    OrigFn fn;
    Word result;
    const char* p;
    VALGRIND_GET_ORIG_FN(fn);
    /* Now by walking over the string we magically produce
       traces when hitting undefined memory. */
    if (name)
        for (p = name; *p; p++)
            ;
    if (value)
        for (p = value; *p; p++)
            ;
    VALGRIND_CHECK_VALUE_IS_DEFINED (overwrite);
    CALL_FN_W_WWW(result, fn, name, value, overwrite);
    return result;
}

#endif /* defined(VGO_linux) */


/*------------------------------------------------------------*/
/*--- AIX stuff only after this point                      ---*/
/*------------------------------------------------------------*/

/* Generate replacements for strcat, strncat, strcpy, strncpy, strcmp
   in the given soname. */
#define Str5FNs(_soname)       \
    STRCAT(_soname, strcat)    \
   STRNCAT(_soname, strncat)   \
    STRCPY(_soname, strcpy)    \
   STRNCPY(_soname, strncpy)   \
    STRCMP(_soname, strcmp)

#if defined(VGP_ppc32_aix5)
Str5FNs(NONE)                             /* in main exe */
Str5FNs(libCZdaZLshrcoreZdoZR)            /* libC.a(shrcore.o) */
Str5FNs(libX11ZdaZLshr4ZdoZR)             /* libX11.a(shr4.o) */
Str5FNs(libXmZdaZLshrZaZdoZR)             /* libXm.a(shr*.o) */
Str5FNs(libXtZdaZLshr4ZdoZR)              /* libXt.a(shr4.o) */
Str5FNs(libppeZurZdaZLdynamicZdoZR)       /* libppe_r.a(dynamic.o) */
Str5FNs(libodmZdaZLshrZdoZR)              /* libodm.a(shr.o) */
Str5FNs(libmpiZurZdaZLmpicoreZurZdoZR)    /* libmpi_r.a(mpicore_r.o) */
Str5FNs(libmpiZurZdaZLmpipoeZurZdoZR)     /* libmpi_r.a(mpipoe_r.o) */
Str5FNs(libmpiZurZdaZLmpciZurZdoZR)       /* libmpi_r.a(mpci_r.o) */
Str5FNs(libslurmZdso)                     /* libslurm.so */
Str5FNs(libglibZdso)                      /* libglib.so */
Str5FNs(libIMZdaZLshrZdoZR)               /* libIM.a(shr.o) */
Str5FNs(libiconvZdaZLshr4ZdoZR)           /* libiconv.a(shr4.o) */
Str5FNs(libGLZdaZLshrZdoZR)               /* libGL.a(shr.o) */
Str5FNs(libgdkZdso)                       /* libgdk.so */
Str5FNs(libcursesZdaZLshr42ZdoZR)         /* libcurses.a(shr42.o) */
Str5FNs(libqtZda)                         /* libqt.a */
Str5FNs(ZaZLlibglibZhZaZdsoZaZR)          /* *(libglib-*.so*) */
Str5FNs(ZaZLlibfontconfigZdsoZaZR)        /* *(libfontconfig.so*) */
Str5FNs(libQtZaa)                         /* libQt*.a */
#endif
#if defined(VGP_ppc64_aix5)
Str5FNs(NONE)                             /* in main exe */
Str5FNs(libX11ZdaZLshrZu64ZdoZR)          /* libX11.a(shr_64.o) */
Str5FNs(libiconvZdaZLshr4Zu64ZdoZR)       /* libiconv.a(shr4_64.o) */
Str5FNs(libGLZdaZLshrZu64ZdoZR)           /* libGL.a(shr_64.o) */
Str5FNs(libppeZurZdaZLdynamic64ZdoZR)     /* libppe_r.a(dynamic64.o) */
Str5FNs(libodmZdaZLshrZu64ZdoZR)          /* libodm.a(shr_64.o) */
Str5FNs(libmpiZurZdaZLmpicore64ZurZdoZR)  /* libmpi_r.a(mpicore64_r.o) */
Str5FNs(libmpiZurZdaZLmpipoe64ZurZdoZR)   /* libmpi_r.a(mpipoe64_r.o) */
Str5FNs(libCZdaZLshrcoreZu64ZdoZR)        /* libC.a(shrcore_64.o) */
Str5FNs(libmpiZurZdaZLmpci64ZurZdoZR)     /* libmpi_r.a(mpci64_r.o) */
Str5FNs(libqtZda)                         /* libqt.a */
Str5FNs(ZaZLlibglibZhZaZdsoZaZR)          /* *(libglib-*.so*) */
Str5FNs(ZaZLlibfontconfigZdsoZaZR)        /* *(libfontconfig.so*) */
Str5FNs(libQtZaa)                         /* libQt*.a */
#endif


/* AIX's libm contains a sqrt implementation which does a nasty thing:
   it loads the initial estimate of the root into a FP register, but
   only the upper half of the number is initialised data.  Hence the
   least significant 32 mantissa bits are undefined, and it then uses
   Newton-Raphson iteration to compute the final, defined result.
   This fools memcheck completely; the only solution I can think of is
   provide our own substitute.  The _FAST variant is almost right
   except the result is not correctly rounded.  The _EXACT variant,
   which is selected by default, is always right; but it's also pretty
   darn slow. */

#if defined(VGP_ppc32_aix5) || defined(VGP_ppc64_aix5)
#define SQRT_FAST(soname, fnname) \
   double VG_REPLACE_FUNCTION_ZU(soname,fnname)( double x ); \
   double VG_REPLACE_FUNCTION_ZU(soname,fnname)( double x ) \
   { \
      static UInt T1[32] =  \
         { 0,       1024,   3062,   5746,   9193,  13348, \
           18162,  23592,  29598,  36145,  43202,  50740, \
           58733,  67158,  75992,  85215,  83599,  71378, \
           60428,  50647,  41945,  34246,  27478,  21581, \
           16499,  12183,   8588,   5674,   3403,   1742, \
           661,    130 }; \
      UInt x0, x1, sign, expo, mant0, bIGENDIAN = 1; \
      union { UInt w[2]; double d; } u; \
      u.d   = x; \
      x0    = u.w[1 - bIGENDIAN]; /* high half */ \
      x1    = u.w[bIGENDIAN];  /* low half */ \
      sign  = x0 >> 31; \
      expo  = (x0 >> 20) & 0x7FF; \
      mant0 = x0 & 0xFFFFF; \
      if ( (sign == 0 && expo >= 1 && expo <= 0x7FE) /* +normal */ \
           || (sign == 0 && expo == 0  \
                         && (mant0 | x1) > 0) /* +denorm */) { \
         /* common case; do Newton-Raphson */ \
         /* technically k should be signed int32, but since we're \
            always entering here with x > 0, doesn't matter that it's \
            unsigned. */ \
         double y; \
         UInt k = (x0>>1) + 0x1ff80000; \
         u.w[1 - bIGENDIAN] = k - T1[31&(k>>15)]; \
         u.w[bIGENDIAN] = 0; \
         y = u.d; \
         y = (y+x/y)/2.0 ; \
         y = (y+x/y)/2.0 ; \
         y = y-(y-x/y)/2.0 ; \
         return y; \
      } \
      if ( (sign == 1 && expo >= 1 && expo <= 0x7FE) /* -normal */ \
           || (sign == 1 && expo == 0  \
                         && (mant0 | x1) > 0) /* -denorm */) { \
         u.w[1 - bIGENDIAN] = 0xFFF00000; \
         u.w[bIGENDIAN] = 0x1; \
         return u.d; /* -Inf -> NaN */ \
      } \
      if ((expo | mant0 | x1) == 0) \
         return x; /* +/-zero -> self */ \
      if (expo == 0x7FF && (mant0 | x1) == 0) { \
         if (sign == 0) \
            return x; /* +Inf -> self */ \
         u.w[1 - bIGENDIAN] = 0xFFF00000; \
         u.w[bIGENDIAN] = 0x1; \
         return u.d; /* -Inf -> NaN */ \
      } \
      /* must be +/- NaN */ \
      return x; /* +/-NaN -> self */ \
   }

#define SQRT_EXACT(soname, fnname) \
   /* \
    * ==================================================== \
    * Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved. \
    * \
    * Developed at SunPro, a Sun Microsystems, Inc. business. \
    * Permission to use, copy, modify, and distribute this \
    * software is freely granted, provided that this notice \
    * is preserved. \
    * ==================================================== \
    */ \
   /* \
    * Return correctly rounded sqrt. \
    *           ------------------------------------------ \
    *           |  Use the hardware sqrt if you have one | \
    *           ------------------------------------------ \
    * Method: \
    *   Bit by bit method using integer arithmetic. (Slow, but portable) \
    *   1. Normalization \
    *      Scale x to y in [1,4) with even powers of 2: \
    *      find an integer k such that  1 <= (y=x*2^(2k)) < 4, then \
    *              sqrt(x) = 2^k * sqrt(y) \
    *   2. Bit by bit computation \
    *      Let q  = sqrt(y) truncated to i bit after binary point (q = 1), \
    *           i                                                   0 \
    *                                     i+1         2 \
    *          s  = 2*q , and      y  =  2   * ( y - q  ).         (1) \
    *           i      i            i                 i \
    * \
    *      To compute q    from q , one checks whether \
    *                  i+1       i \
    * \
    *                            -(i+1) 2 \
    *                      (q + 2      ) <= y.                     (2) \
    *                        i \
    *                                                            -(i+1) \
    *      If (2) is false, then q   = q ; otherwise q   = q  + 2      . \
    *                             i+1   i             i+1   i \
    * \
    *      With some algebric manipulation, it is not difficult to see \
    *      that (2) is equivalent to \
    *                             -(i+1) \
    *                      s  +  2       <= y                      (3) \
    *                       i                i \
    * \
    *      The advantage of (3) is that s  and y  can be computed by \
    *                                    i      i \
    *      the following recurrence formula: \
    *          if (3) is false \
    * \
    *          s     =  s  ,       y    = y   ;                    (4) \
    *           i+1      i          i+1    i \
    * \
    *          otherwise, \
    *                         -i                     -(i+1) \
    *          s     =  s  + 2  ,  y    = y  -  s  - 2             (5) \
    *           i+1      i          i+1    i     i \
    * \
    * \
    *      One may easily use induction to prove (4) and (5). \
    *      Note. Since the left hand side of (3) contain only i+2 bits, \
    *            it does not necessary to do a full (53-bit) comparison \
    *            in (3). \
    *   3. Final rounding \
    *      After generating the 53 bits result, we compute one more bit. \
    *      Together with the remainder, we can decide whether the \
    *      result is exact, bigger than 1/2ulp, or less than 1/2ulp \
    *      (it will never equal to 1/2ulp). \
    *      The rounding mode can be detected by checking whether \
    *      huge + tiny is equal to huge, and whether huge - tiny is \
    *      equal to huge for some floating point number "huge" and "tiny". \
    * \
    * Special cases: \
    *      sqrt(+-0) = +-0         ... exact \
    *      sqrt(inf) = inf \
    *      sqrt(-ve) = NaN         ... with invalid signal \
    *      sqrt(NaN) = NaN         ... with invalid signal for signaling NaN \
    * \
    */ \
   double VG_REPLACE_FUNCTION_ZU(soname,fnname)( double x ); \
   double VG_REPLACE_FUNCTION_ZU(soname,fnname)( double x ) \
   {  \
      const Int    bIGENDIAN = 1; \
      const double one = 1.0, tiny=1.0e-300; \
      double z; \
      Int sign = (Int)0x80000000; \
      Int ix0,s0,q,m,t,i; \
      UInt r,t1,s1,ix1,q1; \
      union { UInt w[2]; double d; } u; \
      u.d = x; \
      ix0 = u.w[1-bIGENDIAN]; \
      ix1 = u.w[bIGENDIAN];    \
      \
      /* take care of Inf and NaN */ \
      if((ix0&0x7ff00000)==0x7ff00000) { \
         return x*x+x;               /* sqrt(NaN)=NaN, sqrt(+inf)=+inf \
                                        sqrt(-inf)=sNaN */ \
      } \
      /* take care of zero */ \
      if(ix0<=0) { \
         if(((ix0&(~sign))|ix1)==0) return x;/* sqrt(+-0) = +-0 */ \
         else if(ix0<0) \
              return (x-x)/(x-x);             /* sqrt(-ve) = sNaN */ \
      } \
      /* normalize x */ \
      m = (ix0>>20); \
      if(m==0) {                              /* subnormal x */ \
         while(ix0==0) { \
            m -= 21; \
            ix0 |= (ix1>>11); ix1 <<= 21; \
         } \
         for(i=0;(ix0&0x00100000)==0;i++) ix0<<=1; \
         m -= i-1; \
         ix0 |= (ix1>>(32-i)); \
         ix1 <<= i; \
      } \
      m -= 1023;      /* unbias exponent */ \
      ix0 = (ix0&0x000fffff)|0x00100000; \
      if(m&1){        /* odd m, double x to make it even */ \
         ix0 += ix0 + ((ix1&sign)>>31); \
         ix1 += ix1; \
      } \
      m >>= 1;        /* m = [m/2] */ \
      /* generate sqrt(x) bit by bit */ \
      ix0 += ix0 + ((ix1&sign)>>31); \
      ix1 += ix1; \
      q = q1 = s0 = s1 = 0;   /* [q,q1] = sqrt(x) */ \
      r = 0x00200000;         /* r = moving bit from right to left */ \
      while(r!=0) { \
         t = s0+r; \
         if(t<=ix0) { \
            s0   = t+r; \
            ix0 -= t; \
            q   += r; \
         } \
         ix0 += ix0 + ((ix1&sign)>>31); \
         ix1 += ix1; \
         r>>=1; \
      } \
      r = sign; \
      while(r!=0) { \
         t1 = s1+r; \
         t  = s0; \
         if((t<ix0)||((t==ix0)&&(t1<=ix1))) { \
            s1  = t1+r; \
            if(((t1&sign)==sign)&&(s1&sign)==0) s0 += 1; \
            ix0 -= t; \
            if (ix1 < t1) ix0 -= 1; \
            ix1 -= t1; \
            q1  += r; \
         } \
         ix0 += ix0 + ((ix1&sign)>>31); \
         ix1 += ix1; \
         r>>=1; \
      } \
      /* use floating add to find out rounding direction */ \
      if((ix0|ix1)!=0) { \
         z = one-tiny; /* trigger inexact flag */ \
         if (z>=one) { \
            z = one+tiny; \
            if (q1==(UInt)0xffffffff) { q1=0; q += 1;} \
            else if (z>one) { \
                    if (q1==(UInt)0xfffffffe) q+=1; \
                    q1+=2; \
                 } else \
                    q1 += (q1&1); \
         } \
      } \
      ix0 = (q>>1)+0x3fe00000; \
      ix1 = q1>>1; \
      if ((q&1)==1) ix1 |= sign; \
      ix0 += (m <<20); \
      ix0 = u.w[1-bIGENDIAN] = ix0; \
      ix1 = u.w[bIGENDIAN] = ix1;    \
      z = u.d; \
      return z; \
   }

#if 0
SQRT_FAST(NONE, sqrt)  /* xlC generates these */
SQRT_FAST(NONE, _sqrt) /* xlf generates these */
#else
SQRT_EXACT(NONE, sqrt)  /* xlC generates these */
SQRT_EXACT(NONE, _sqrt) /* xlf generates these */
#endif

#endif /* defined(VGP_ppc32_aix5) */

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
