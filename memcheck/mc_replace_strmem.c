
/*--------------------------------------------------------------------*/
/*--- Replacements for strcpy(), memcpy() et al, which run on the  ---*/
/*--- simulated CPU.                                               ---*/
/*---                                          mc_replace_strmem.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of MemCheck, a heavyweight Valgrind tool for
   detecting memory errors.

   Copyright (C) 2000-2007 Julian Seward 
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
#define RECORD_OVERLAP_ERROR(s, src, dst, len) \
{ \
   Word unused_res; \
   VALGRIND_DO_CLIENT_REQUEST(unused_res, 0, \
			      _VG_USERREQ__MEMCHECK_RECORD_OVERLAP_ERROR, \
			      s, src, dst, len, 0); \
}

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
STRRCHR(m_libc_soname,   strrchr)
STRRCHR(m_libc_soname,   rindex)
STRRCHR(m_ld_linux_so_2, rindex)
   

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
STRCHR(m_libc_soname,          strchr)
STRCHR(m_ld_linux_so_2,        strchr)
STRCHR(m_ld_linux_x86_64_so_2, strchr)
STRCHR(m_libc_soname,          index)
STRCHR(m_ld_linux_so_2,        index)
STRCHR(m_ld_linux_x86_64_so_2, index)


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

STRCAT(m_libc_soname, strcat)


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

STRNCAT(m_libc_soname, strncat)


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

STRCPY(m_libc_soname, strcpy)


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

STRNCPY(m_libc_soname, strncpy)


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

STRNCMP(m_libc_soname, strncmp)


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

MEMCHR(m_libc_soname, memchr)


#define MEMCPY(soname, fnname) \
   void* VG_REPLACE_FUNCTION_ZU(soname,fnname) \
            ( void *dst, const void *src, SizeT len ); \
   void* VG_REPLACE_FUNCTION_ZU(soname,fnname) \
            ( void *dst, const void *src, SizeT len ) \
   { \
      register char *d; \
      register char *s; \
 \
      if (len == 0) \
         return dst; \
 \
      if (is_overlap(dst, src, len, len)) \
         RECORD_OVERLAP_ERROR("memcpy", dst, src, len); \
 \
      if ( dst > src ) { \
         d = (char *)dst + len - 1; \
         s = (char *)src + len - 1; \
         while ( len >= 4 ) { \
            *d-- = *s--; \
            *d-- = *s--; \
            *d-- = *s--; \
            *d-- = *s--; \
            len -= 4; \
         } \
         while ( len-- ) { \
            *d-- = *s--; \
         } \
      } else if ( dst < src ) { \
         d = (char *)dst; \
         s = (char *)src; \
         while ( len >= 4 ) { \
            *d++ = *s++; \
            *d++ = *s++; \
            *d++ = *s++; \
            *d++ = *s++; \
            len -= 4; \
         } \
         while ( len-- ) { \
            *d++ = *s++; \
         } \
      } \
      return dst; \
   }

MEMCPY(m_libc_soname, memcpy)
MEMCPY(m_ld_so_1,     memcpy) /* ld.so.1 */
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

MEMCMP(m_libc_soname, memcmp)
MEMCMP(m_libc_soname, bcmp)
MEMCMP(m_ld_so_1, bcmp)


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

STPCPY(m_libc_soname,         stpcpy)
STPCPY(m_ld_linux_so_2,        stpcpy)
STPCPY(m_ld_linux_x86_64_so_2, stpcpy)
   

#define MEMSET(soname, fnname) \
   void* VG_REPLACE_FUNCTION_ZU(soname,fnname)(void *s, Int c, SizeT n); \
   void* VG_REPLACE_FUNCTION_ZU(soname,fnname)(void *s, Int c, SizeT n) \
   { \
      unsigned char *cp = s; \
 \
      while(n--) \
         *cp++ = c; \
 \
      return s; \
   }

MEMSET(m_libc_soname, memset)


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

MEMMOVE(m_libc_soname, memmove)


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
         "program terminated"); \
     _exit(127); \
     /*NOTREACHED*/ \
     return NULL; \
   }

GLIBC25___MEMMOVE_CHK(m_libc_soname, __memmove_chk)


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

GLIBC232_STRCHRNUL(m_libc_soname, strchrnul)


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

GLIBC232_RAWMEMCHR(m_libc_soname, rawmemchr)


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
         "program terminated"); \
     _exit(127); \
     /*NOTREACHED*/ \
     return NULL; \
   }

GLIBC25___STRCPY_CHK(m_libc_soname, __strcpy_chk)


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
         "program terminated"); \
     _exit(127); \
     /*NOTREACHED*/ \
     return NULL; \
   }

GLIBC25___STPCPY_CHK(m_libc_soname, __stpcpy_chk)


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

GLIBC25_MEMPCPY(m_libc_soname, mempcpy)
GLIBC25_MEMPCPY(m_ld_so_1,     mempcpy) /* ld.so.1 */


/*------------------------------------------------------------*/
/*--- AIX stuff only after this point                      ---*/
/*------------------------------------------------------------*/

/* Generate replacements for strcat, strncat, strcpy, strncpy,
   in the given soname. */
#define Str4FNs(_soname)       \
    STRCAT(_soname, strcat)    \
   STRNCAT(_soname, strncat)   \
    STRCPY(_soname, strcpy)    \
   STRNCPY(_soname, strncpy)

#if defined(VGP_ppc32_aix5)
Str4FNs(NONE)                             /* in main exe */
Str4FNs(libCZdaZLshrcoreZdoZR)            /* libC.a(shrcore.o) */
Str4FNs(libX11ZdaZLshr4ZdoZR)             /* libX11.a(shr4.o) */
Str4FNs(libXmZdaZLshrZaZdoZR)             /* libXm.a(shr*.o) */
Str4FNs(libXtZdaZLshr4ZdoZR)              /* libXt.a(shr4.o) */
Str4FNs(libppeZurZdaZLdynamicZdoZR)       /* libppe_r.a(dynamic.o) */
Str4FNs(libodmZdaZLshrZdoZR)              /* libodm.a(shr.o) */
Str4FNs(libmpiZurZdaZLmpicoreZurZdoZR)    /* libmpi_r.a(mpicore_r.o) */
Str4FNs(libmpiZurZdaZLmpipoeZurZdoZR)     /* libmpi_r.a(mpipoe_r.o) */
Str4FNs(libmpiZurZdaZLmpciZurZdoZR)       /* libmpi_r.a(mpci_r.o) */
Str4FNs(libslurmZdso)                     /* libslurm.so */
Str4FNs(libglibZdso)                      /* libglib.so */
Str4FNs(libIMZdaZLshrZdoZR)               /* libIM.a(shr.o) */
Str4FNs(libiconvZdaZLshr4ZdoZR)           /* libiconv.a(shr4.o) */
Str4FNs(libGLZdaZLshrZdoZR)               /* libGL.a(shr.o) */
Str4FNs(libgdkZdso)                       /* libgdk.so */
Str4FNs(libcursesZdaZLshr42ZdoZR)         /* libcurses.a(shr42.o) */
Str4FNs(libqtZda)                         /* libqt.a */
#endif
#if defined(VGP_ppc64_aix5)
Str4FNs(NONE)                             /* in main exe */
Str4FNs(libX11ZdaZLshrZu64ZdoZR)          /* libX11.a(shr_64.o) */
Str4FNs(libiconvZdaZLshr4Zu64ZdoZR)       /* libiconv.a(shr4_64.o) */
Str4FNs(libGLZdaZLshrZu64ZdoZR)           /* libGL.a(shr_64.o) */
Str4FNs(libppeZurZdaZLdynamic64ZdoZR)     /* libppe_r.a(dynamic64.o) */
Str4FNs(libodmZdaZLshrZu64ZdoZR)          /* libodm.a(shr_64.o) */
Str4FNs(libmpiZurZdaZLmpicore64ZurZdoZR)  /* libmpi_r.a(mpicore64_r.o) */
Str4FNs(libmpiZurZdaZLmpipoe64ZurZdoZR)   /* libmpi_r.a(mpipoe64_r.o) */
Str4FNs(libCZdaZLshrcoreZu64ZdoZR)        /* libC.a(shrcore_64.o) */
Str4FNs(libmpiZurZdaZLmpci64ZurZdoZR)     /* libmpi_r.a(mpci64_r.o) */
Str4FNs(libqtZda)                         /* libqt.a */
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
