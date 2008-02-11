
/*------------------------------------------------------------------------*/
/*--- Replacements for memcpy() et al, which run on the simulated CPU. ---*/
/*---                                               o_replace_memops.c ---*/
/*------------------------------------------------------------------------*/

/*
   This file is part of Omega, a Valgrind tool for instantly detecting
   memory leaks.

   Copyright (C) 2006-2008 Bryan "Brain Murders" Meredith

   Derived from mac_replace_strmem.c
   Copyright (C) 2000-2006 Julian Seward 
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

   The current maintainer is Rich Coe <richard.coe@med.ge.com>.
*/

#include <stdio.h>
#include "pub_tool_basics.h"
#include "valgrind.h"
#include "exp-omega.h"

/* ---------------------------------------------------------------------
   We have our own versions of these functions so that we can correctly
   track pointers that are duplicated or overwritten.

   THEY RUN ON THE SIMD CPU!
   ------------------------------------------------------------------ */

int I_WRAP_SONAME_FNNAME_ZU(NONE,main) ( int n, char *a[], char *e[] );
int I_WRAP_SONAME_FNNAME_ZU(NONE,main) ( int n, char *a[], char *e[] )
{
   int    r;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   VALGRIND_DO_ENTER_MAIN;
   CALL_FN_W_WWW(r, fn, n, a, e);
   VALGRIND_DO_LEAVE_MAIN;
   return r;
}


void* I_WRAP_SONAME_FNNAME_ZU(NONE,memcpy)( void *dst, const void *src, SizeT len );
void* I_WRAP_SONAME_FNNAME_ZU(NONE,memcpy)( void *dst, const void *src, SizeT len )
{
  register char *d;
  register char *s;
					   
  if (len == 0)
    return dst;

  if ( dst > src ) {
    d = (char *)dst + len - 1;
    s = (char *)src + len - 1;
    while ( len >= 4 ) {
      *d-- = *s--;
      *d-- = *s--;
      *d-- = *s--;
      *d-- = *s--;
      len -= 4;
    }
    while ( len-- ) {
      *d-- = *s--;
    }
  } else if ( dst < src ) {
    d = (char *)dst;
    s = (char *)src;
    while ( len >= 4 ) {
      *d++ = *s++;
      *d++ = *s++;
      *d++ = *s++;
      *d++ = *s++;
      len -= 4;
    }
    while ( len-- ) {
      *d++ = *s++;
    }
  }
  return dst;
}

void* I_WRAP_SONAME_FNNAME_ZU(NONE,mempcpy)( void *dst, const void *src, SizeT len );
void* I_WRAP_SONAME_FNNAME_ZU(NONE,mempcpy)( void *dst, const void *src, SizeT len )
{
  register char *d;
  register char *s;
					   
  if (len == 0)
    return dst;

  if ( dst > src ) {
    d = (char *)dst + len - 1;
    s = (char *)src + len - 1;
    while ( len >= 4 ) {
      *d-- = *s--;
      *d-- = *s--;
      *d-- = *s--;
      *d-- = *s--;
      len -= 4;
    }
    while ( len-- ) {
      *d-- = *s--;
    }
  } else if ( dst < src ) {
    d = (char *)dst;
    s = (char *)src;
    while ( len >= 4 ) {
      *d++ = *s++;
      *d++ = *s++;
      *d++ = *s++;
      *d++ = *s++;
      len -= 4;
    }
    while ( len-- ) {
      *d++ = *s++;
    }
  }
  return ((Char*)dst + len);
}

void* I_WRAP_SONAME_FNNAME_ZU(NONE,memmove)(void *dstV, const void *srcV, SizeT n);
void* I_WRAP_SONAME_FNNAME_ZU(NONE,memmove)(void *dstV, const void *srcV, SizeT n)
{
  SizeT i;
  Char* dst = (Char*)dstV;
  Char* src = (Char*)srcV;
  if (dst < src) {
    for (i = 0; i < n; i++)
      dst[i] = src[i];
  }
  else 
    if (dst > src) {
      for (i = 0; i < n; i++)
	dst[n-i-1] = src[n-i-1];
    }
  return dst;
}

void* I_WRAP_SONAME_FNNAME_ZU(NONE,memset)(void *s, Int c, SizeT n);
void* I_WRAP_SONAME_FNNAME_ZU(NONE,memset)(void *s, Int c, SizeT n)
{
  unsigned char *cp = s;
  
  while(n--)
    *cp++ = c;
  
  return s;
}

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
