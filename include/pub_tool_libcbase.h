
/*--------------------------------------------------------------------*/
/*--- Standalone libc stuff.                   pub_tool_libcbase.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2009 Julian Seward
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
extern Char VG_(tolower) ( Char c );

/* ---------------------------------------------------------------------
   Converting strings to numbers
   ------------------------------------------------------------------ */

// Convert strings to numbers according to various bases.  Leading
// whitespace is ignored.  A subsequent '-' or '+' is accepted.  For strtoll16,
// accepts an initial "0x" or "0X" prefix, but only if it's followed by a
// hex digit (if not, the '0' will be read and then it will stop on the
// "x"/"X".)  If 'endptr' isn't NULL, it gets filled in with the first
// non-digit char.  Returns 0 if no number could be converted, and 'endptr'
// is set to the start of the string.  None of them test that the number
// fits into 64 bits.
//
// Nb: if you're wondering why we don't just have a single VG_(strtoll) which
// takes a base, it's because I wanted it to assert if it was given a bogus
// base (the standard glibc one sets 'errno' in this case).  But
// m_libcbase.c doesn't import any code, not even vg_assert. --njn
// 
// Nb: we also don't provide VG_(atoll*);  these functions are worse than
// useless because they don't do any error checking and so accept malformed
// numbers and non-numbers -- eg. "123xyz" gives 123, and "foo" gives 0!
// If you really want that behaviour, you can use "VG_(strtoll10)(str, NULL)".
extern Long  VG_(strtoll10) ( Char* str, Char** endptr );
extern Long  VG_(strtoll16) ( Char* str, Char** endptr );

// Convert a string to a double.  After leading whitespace is ignored, a
// '+' or '-' is allowed, and then it accepts a non-empty sequence of
// decimal digits possibly containing a '.'.  Hexadecimal floats are not
// accepted, nor are "fancy" floats (eg. "3.4e-5", "NAN").
extern double VG_(strtod)  ( Char* str, Char** endptr );

/* ---------------------------------------------------------------------
   String functions and macros
   ------------------------------------------------------------------ */

/* Use this for normal null-termination-style string comparison. */
#define VG_STREQ(s1,s2) ( (s1 != NULL && s2 != NULL \
                           && VG_(strcmp)((s1),(s2))==0) ? True : False )
#define VG_STREQN(n,s1,s2) ( (s1 != NULL && s2 != NULL \
                             && VG_(strncmp)((s1),(s2),(n))==0) ? True : False )

extern SizeT VG_(strlen)         ( const Char* str );
extern Char* VG_(strcat)         ( Char* dest, const Char* src );
extern Char* VG_(strncat)        ( Char* dest, const Char* src, SizeT n );
extern Char* VG_(strpbrk)        ( const Char* s, const Char* accpt );
extern Char* VG_(strcpy)         ( Char* dest, const Char* src );
extern Char* VG_(strncpy)        ( Char* dest, const Char* src, SizeT ndest );
extern Int   VG_(strcmp)         ( const Char* s1, const Char* s2 );
extern Int   VG_(strcasecmp)     ( const Char* s1, const Char* s2 );
extern Int   VG_(strncmp)        ( const Char* s1, const Char* s2, SizeT nmax );
extern Int   VG_(strncasecmp)    ( const Char* s1, const Char* s2, SizeT nmax );
extern Char* VG_(strstr)         ( const Char* haystack, Char* needle );
extern Char* VG_(strcasestr)     ( const Char* haystack, Char* needle );
extern Char* VG_(strchr)         ( const Char* s, Char c );
extern Char* VG_(strrchr)        ( const Char* s, Char c );
extern SizeT VG_(strspn)         ( const Char* s, const Char* accpt );
extern SizeT VG_(strcspn)        ( const Char* s, const char* reject );

/* Like strncpy(), but if 'src' is longer than 'ndest' inserts a '\0' as the
   last character. */
extern void  VG_(strncpy_safely) ( Char* dest, const Char* src, SizeT ndest );

/* ---------------------------------------------------------------------
   mem* functions
   ------------------------------------------------------------------ */

extern void* VG_(memcpy) ( void *d, const void *s, SizeT sz );
extern void* VG_(memmove)( void *d, const void *s, SizeT sz );
extern void* VG_(memset) ( void *s, Int c, SizeT sz );
extern Int   VG_(memcmp) ( const void* s1, const void* s2, SizeT n );

/* ---------------------------------------------------------------------
   Address computation helpers
   ------------------------------------------------------------------ */

// Check if an address/whatever is aligned
#define VG_IS_2_ALIGNED(aaa_p)    (0 == (((Addr)(aaa_p)) & ((Addr)0x1)))
#define VG_IS_4_ALIGNED(aaa_p)    (0 == (((Addr)(aaa_p)) & ((Addr)0x3)))
#define VG_IS_8_ALIGNED(aaa_p)    (0 == (((Addr)(aaa_p)) & ((Addr)0x7)))
#define VG_IS_16_ALIGNED(aaa_p)   (0 == (((Addr)(aaa_p)) & ((Addr)0xf)))
#define VG_IS_32_ALIGNED(aaa_p)   (0 == (((Addr)(aaa_p)) & ((Addr)0x1f)))
#define VG_IS_WORD_ALIGNED(aaa_p) (0 == (((Addr)(aaa_p)) & ((Addr)(sizeof(Addr)-1))))
#define VG_IS_PAGE_ALIGNED(aaa_p) (0 == (((Addr)(aaa_p)) & ((Addr)(VKI_PAGE_SIZE-1))))

// 'a' -- the alignment -- must be a power of 2.
// The latter two require the vki-*.h header to be imported also.
#define VG_ROUNDDN(p, a)   ((Addr)(p) & ~((Addr)(a)-1))
#define VG_ROUNDUP(p, a)   VG_ROUNDDN((p)+(a)-1, (a))
#define VG_PGROUNDDN(p)    VG_ROUNDDN(p, VKI_PAGE_SIZE)
#define VG_PGROUNDUP(p)    VG_ROUNDUP(p, VKI_PAGE_SIZE)

/* ---------------------------------------------------------------------
   Misc useful functions
   ------------------------------------------------------------------ */

/* Like qsort().  The name VG_(ssort) is for historical reasons -- it used
 * to be a shell sort, but is now a quicksort. */
extern void VG_(ssort)( void* base, SizeT nmemb, SizeT size,
                        Int (*compar)(void*, void*) );

/* Returns the base-2 logarithm of x.  Returns -1 if x is not a power
   of two.  Nb: VG_(log2)(1) == 0.  */
extern Int VG_(log2) ( UInt x );

// A pseudo-random number generator returning a random UInt.  If pSeed
// is NULL, it uses its own seed, which starts at zero.  If pSeed is
// non-NULL, it uses and updates whatever pSeed points at.
extern UInt VG_(random) ( /*MOD*/UInt* pSeed );
#define VG_RAND_MAX (1ULL << 32)

#endif   // __PUB_TOOL_LIBCBASE_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
