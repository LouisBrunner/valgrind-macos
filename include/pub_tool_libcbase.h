
/*--------------------------------------------------------------------*/
/*--- Standalone libc stuff.                   pub_tool_libcbase.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

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

#ifndef __PUB_TOOL_LIBCBASE_H
#define __PUB_TOOL_LIBCBASE_H

/* ---------------------------------------------------------------------
   Char functions.
   ------------------------------------------------------------------ */

extern Bool VG_(isspace) ( Char c );
extern Bool VG_(isdigit) ( Char c );

/* ---------------------------------------------------------------------
   Converting strings to numbers
   ------------------------------------------------------------------ */

extern Long  VG_(atoll)   ( Char* str );     // base 10
extern Long  VG_(atoll36) ( Char* str );     // base 36

/* ---------------------------------------------------------------------
   String functions and macros
   ------------------------------------------------------------------ */

/* Use this for normal null-termination-style string comparison */
#define VG_STREQ(s1,s2) (s1 != NULL && s2 != NULL \
                         && VG_(strcmp)((s1),(s2))==0)

extern Int   VG_(strlen)         ( const Char* str );
extern Char* VG_(strcat)         ( Char* dest, const Char* src );
extern Char* VG_(strncat)        ( Char* dest, const Char* src, SizeT n );
extern Char* VG_(strpbrk)        ( const Char* s, const Char* accpt );
extern Char* VG_(strcpy)         ( Char* dest, const Char* src );
extern Char* VG_(strncpy)        ( Char* dest, const Char* src, SizeT ndest );
extern Int   VG_(strcmp)         ( const Char* s1, const Char* s2 );
extern Int   VG_(strncmp)        ( const Char* s1, const Char* s2, SizeT nmax );
extern Char* VG_(strstr)         ( const Char* haystack, Char* needle );
extern Char* VG_(strchr)         ( const Char* s, Char c );
extern Char* VG_(strrchr)        ( const Char* s, Char c );

/* Like strcmp() and strncmp(), but stop comparing at any whitespace. */
extern Int   VG_(strcmp_ws)      ( const Char* s1, const Char* s2 );
extern Int   VG_(strncmp_ws)     ( const Char* s1, const Char* s2, SizeT nmax );

/* Like strncpy(), but if 'src' is longer than 'ndest' inserts a '\0' as the
   last character. */
extern void  VG_(strncpy_safely) ( Char* dest, const Char* src, SizeT ndest );

/* Mini-regexp function.  Searches for 'pat' in 'str'.  Supports
 * meta-symbols '*' and '?'.  '\' escapes meta-symbols. */
extern Bool  VG_(string_match)   ( const Char* pat, const Char* str );

/* ---------------------------------------------------------------------
   mem* functions
   ------------------------------------------------------------------ */

extern void* VG_(memcpy) ( void *d, const void *s, SizeT sz );
extern void* VG_(memset) ( void *s, Int c, SizeT sz );
extern Int   VG_(memcmp) ( const void* s1, const void* s2, SizeT n );

/* ---------------------------------------------------------------------
   Address computation helpers
   ------------------------------------------------------------------ */

// Check if an address/whatever is aligned
#define VG_IS_4_ALIGNED(aaa_p)    (0 == (((Addr)(aaa_p)) & ((Addr)0x3)))
#define VG_IS_8_ALIGNED(aaa_p)    (0 == (((Addr)(aaa_p)) & ((Addr)0x7)))
#define VG_IS_16_ALIGNED(aaa_p)   (0 == (((Addr)(aaa_p)) & ((Addr)0xf)))
#define VG_IS_WORD_ALIGNED(aaa_p) (0 == (((Addr)(aaa_p)) & ((Addr)(sizeof(Addr)-1))))
#define VG_IS_PAGE_ALIGNED(aaa_p) (0 == (((Addr)(aaa_p)) & ((Addr)(VKI_PAGE_SIZE-1))))

/* ---------------------------------------------------------------------
   Misc useful functions
   ------------------------------------------------------------------ */

/* Like qsort(), but does shell-sort.  The size==1/2/4 cases are specialised. */
extern void VG_(ssort)( void* base, SizeT nmemb, SizeT size,
                        Int (*compar)(void*, void*) );

/* Returns the base-2 logarithm of x. */
extern Int VG_(log2) ( Int x );

#endif   // __PUB_TOOL_LIBCBASE_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
