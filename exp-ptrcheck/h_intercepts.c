
/*--------------------------------------------------------------------*/
/*--- Ptrcheck: a pointer-use checker.             pc_intercepts.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Ptrcheck, a Valgrind tool for checking pointer
   use in programs.

   Copyright (C) 2003-2010 Nicholas Nethercote
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
      Char* dst_orig = dst; \
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


/*--------------------------------------------------------------------*/
/*--- end                                          pc_intercepts.c ---*/
/*--------------------------------------------------------------------*/
