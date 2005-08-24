
/*--------------------------------------------------------------------*/
/*--- Replacements for strcpy(), memcpy() et al, which run on the  ---*/
/*--- simulated CPU.                                               ---*/
/*---                                         mac_replace_strmem.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of MemCheck, a heavyweight Valgrind tool for
   detecting memory errors.

   Copyright (C) 2000-2005 Julian Seward 
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
#include "pub_tool_profile.h"
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
#define RECORD_OVERLAP_ERROR(s, p_extra) \
{ \
   Word unused_res; \
   VALGRIND_MAGIC_SEQUENCE(unused_res, 0, \
			   _VG_USERREQ__MEMCHECK_RECORD_OVERLAP_ERROR, \
			   s, p_extra, 0, 0); \
}

static __inline__
void complain2 ( Char* s, char* dst, const char* src )
{
   OverlapExtra extra = {
      .src = (Addr)src, .dst = (Addr)dst, .len = -1,
   };
   RECORD_OVERLAP_ERROR( s, &extra );
}

static __inline__
void complain3 ( Char* s, void* dst, const void* src, int n )
{
   /* Must wrap it up here, because we cannot pass 4 args to core */
   OverlapExtra extra = {
      .src = (Addr)src, .dst = (Addr)dst, .len = n,
   };
   RECORD_OVERLAP_ERROR( s, &extra );
}

// Some handy Z-encoded names
#define  m_libc_so_6             libcZdsoZd6                // libc.so.6
#define  m_ld_linux_so_2         ldZhlinuxZdsoZd2           // ld-linux.so.2
#define  m_ld_linux_x86_64_so_2  ldZhlinuxZhx86Zh64ZdsoZd2  // ld-linux-x86-64.so.2


#define STRRCHR(soname, fnname) \
   char* VG_REPLACE_FUNCTION(soname,fnname)( const char* s, int c ); \
   char* VG_REPLACE_FUNCTION(soname,fnname)( const char* s, int c ) \
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
STRRCHR(m_libc_so_6,     strrchr)
STRRCHR(m_libc_so_6,     rindex)
STRRCHR(m_ld_linux_so_2, rindex)
   

#define STRCHR(soname, fnname) \
   char* VG_REPLACE_FUNCTION(soname,fnname) ( const char* s, int c ); \
   char* VG_REPLACE_FUNCTION(soname,fnname) ( const char* s, int c ) \
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
STRCHR(m_libc_so_6,            strchr)
STRCHR(m_ld_linux_so_2,        strchr)
STRCHR(m_ld_linux_x86_64_so_2, strchr)
STRCHR(m_libc_so_6,            index)
STRCHR(m_ld_linux_so_2,        index)
STRCHR(m_ld_linux_x86_64_so_2, index)


#define STRCAT(soname, fnname) \
   char* VG_REPLACE_FUNCTION(soname,fnname) ( char* dst, const char* src ); \
   char* VG_REPLACE_FUNCTION(soname,fnname) ( char* dst, const char* src ) \
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
         complain2("strcat", dst_orig, src_orig); \
 \
      return dst_orig; \
   }

STRCAT(m_libc_so_6, strcat)


#define STRNCAT(soname, fnname) \
   char* VG_REPLACE_FUNCTION(soname,fnname) ( char* dst, const char* src, SizeT n ); \
   char* VG_REPLACE_FUNCTION(soname,fnname) ( char* dst, const char* src, SizeT n ) \
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
         complain3("strncat", dst_orig, src_orig, n); \
 \
      return dst_orig; \
   }

STRNCAT(m_libc_so_6, strncat)
   

#define STRNLEN(soname, fnname) \
   SizeT VG_REPLACE_FUNCTION(soname,fnname) ( const char* str, SizeT n ); \
   SizeT VG_REPLACE_FUNCTION(soname,fnname) ( const char* str, SizeT n ) \
   { \
      SizeT i = 0; \
      while (i < n && str[i] != 0) i++; \
      return i; \
   }

STRNLEN(m_libc_so_6, strnlen)
   

// Note that this replacement often doesn't get used because gcc inlines
// calls to strlen() with its own built-in version.  This can be very
// confusing if you aren't expecting it.  Other small functions in this file
// may also be inline by gcc.
#define STRLEN(soname, fnname) \
   SizeT VG_REPLACE_FUNCTION(soname,fnname)( const char* str ); \
   SizeT VG_REPLACE_FUNCTION(soname,fnname)( const char* str ) \
   { \
      SizeT i = 0; \
      while (str[i] != 0) i++; \
      return i; \
   }

STRLEN(m_libc_so_6,            strlen)
STRLEN(m_ld_linux_so_2,        strlen)
STRLEN(m_ld_linux_x86_64_so_2, strlen)
   

#define STRCPY(soname, fnname) \
   char* VG_REPLACE_FUNCTION(soname, fnname) ( char* dst, const char* src ); \
   char* VG_REPLACE_FUNCTION(soname, fnname) ( char* dst, const char* src ) \
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
         complain2("strcpy", dst_orig, src_orig); \
 \
      return dst_orig; \
   }

STRCPY(m_libc_so_6, strcpy)


#define STRNCPY(soname, fnname) \
   char* VG_REPLACE_FUNCTION(soname, fnname) ( char* dst, const char* src, SizeT n ); \
   char* VG_REPLACE_FUNCTION(soname, fnname) ( char* dst, const char* src, SizeT n ) \
   { \
      const Char* src_orig = src; \
            Char* dst_orig = dst; \
      SizeT m = 0; \
 \
      while (m   < n && *src) { m++; *dst++ = *src++; } \
      /* Check for overlap after copying; all n bytes of dst are relevant, */ \
      /* but only m+1 bytes of src if terminator was found */ \
      if (is_overlap(dst_orig, src_orig, n, (m < n) ? m+1 : n)) \
         complain3("strncpy", dst, src, n); \
      while (m++ < n) *dst++ = 0;         /* must pad remainder with nulls */ \
 \
      return dst_orig; \
   }

STRNCPY(m_libc_so_6, strncpy)


#define STRNCMP(soname, fnname) \
   int VG_REPLACE_FUNCTION(soname,fnname) ( const char* s1, const char* s2, SizeT nmax ); \
   int VG_REPLACE_FUNCTION(soname,fnname) ( const char* s1, const char* s2, SizeT nmax ) \
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

STRNCMP(m_libc_so_6, strncmp)


#define STRCMP(soname, fnname) \
   int VG_REPLACE_FUNCTION(soname,fnname) ( const char* s1, const char* s2 ); \
   int VG_REPLACE_FUNCTION(soname,fnname) ( const char* s1, const char* s2 ) \
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

STRCMP(m_libc_so_6,            strcmp)
STRCMP(m_ld_linux_x86_64_so_2, strcmp)


#define MEMCHR(soname, fnname) \
   void* VG_REPLACE_FUNCTION(soname,fnname) (const void *s, int c, SizeT n); \
   void* VG_REPLACE_FUNCTION(soname,fnname) (const void *s, int c, SizeT n) \
   { \
      SizeT i; \
      UChar c0 = (UChar)c; \
      UChar* p = (UChar*)s; \
      for (i = 0; i < n; i++) \
         if (p[i] == c0) return (void*)(&p[i]); \
      return NULL; \
   }

MEMCHR(m_libc_so_6, memchr)


#define MEMCPY(soname, fnname) \
   void* VG_REPLACE_FUNCTION(soname,fnname)( void *dst, const void *src, SizeT len ); \
   void* VG_REPLACE_FUNCTION(soname,fnname)( void *dst, const void *src, SizeT len ) \
   { \
      register char *d; \
      register char *s; \
 \
      if (len == 0) \
         return dst; \
 \
      if (is_overlap(dst, src, len, len)) \
         complain3("memcpy", dst, src, len); \
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

MEMCPY(m_libc_so_6, memcpy)
   

#define MEMCMP(soname, fnname) \
   int VG_REPLACE_FUNCTION(soname,fnname)( const void *s1V, const void *s2V, SizeT n ); \
   int VG_REPLACE_FUNCTION(soname,fnname)( const void *s1V, const void *s2V, SizeT n ) \
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

MEMCMP(m_libc_so_6, memcmp)
MEMCMP(m_libc_so_6, bcmp)


/* Copy SRC to DEST, returning the address of the terminating '\0' in
   DEST. (minor variant of strcpy) */
#define STPCPY(soname, fnname) \
   char* VG_REPLACE_FUNCTION(soname,fnname) ( char* dst, const char* src ); \
   char* VG_REPLACE_FUNCTION(soname,fnname) ( char* dst, const char* src ) \
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
         complain2("stpcpy", dst_orig, src_orig); \
 \
      return dst; \
   }

STPCPY(m_libc_so_6,            stpcpy)
STPCPY(m_ld_linux_so_2,        stpcpy)
STPCPY(m_ld_linux_x86_64_so_2, stpcpy)
   

#define MEMSET(soname, fnname) \
   void* VG_REPLACE_FUNCTION(soname,fnname)(void *s, Int c, SizeT n); \
   void* VG_REPLACE_FUNCTION(soname,fnname)(void *s, Int c, SizeT n) \
   { \
      unsigned char *cp = s; \
 \
      while(n--) \
         *cp++ = c; \
 \
      return s; \
   }

MEMSET(m_libc_so_6, memset)


#define MEMMOVE(soname, fnname) \
   void* VG_REPLACE_FUNCTION(soname,fnname)(void *dstV, const void *srcV, SizeT n); \
   void* VG_REPLACE_FUNCTION(soname,fnname)(void *dstV, const void *srcV, SizeT n) \
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

MEMMOVE(m_libc_so_6, memmove)


/* Find the first occurrence of C in S or the final NUL byte.  */
#define GLIBC232_STRCHRNUL(soname, fnname) \
   char* VG_REPLACE_FUNCTION(soname,fnname) (const char* s, int c_in); \
   char* VG_REPLACE_FUNCTION(soname,fnname) (const char* s, int c_in) \
   { \
      unsigned char  c        = (unsigned char) c_in; \
      unsigned char* char_ptr = (unsigned char *)s; \
      while (1) { \
         if (*char_ptr == 0) return char_ptr; \
         if (*char_ptr == c) return char_ptr; \
         char_ptr++; \
      } \
   }

GLIBC232_STRCHRNUL(m_libc_so_6, strchrnul)


/* Find the first occurrence of C in S.  */
#define GLIBC232_RAWMEMCHR(soname, fnname) \
   char* VG_REPLACE_FUNCTION(soname,fnname) (const char* s, int c_in); \
   char* VG_REPLACE_FUNCTION(soname,fnname) (const char* s, int c_in) \
   { \
      unsigned char  c        = (unsigned char) c_in; \
      unsigned char* char_ptr = (unsigned char *)s; \
      while (1) { \
         if (*char_ptr == c) return char_ptr; \
         char_ptr++; \
      } \
   }

GLIBC232_RAWMEMCHR(m_libc_so_6, rawmemchr)


/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
