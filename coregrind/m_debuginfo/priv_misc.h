
/*--------------------------------------------------------------------*/
/*--- Misc simple stuff lacking a better home.        priv_misc.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2008-2011 OpenWorks LLP
      info@open-works.co.uk

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

   Neither the names of the U.S. Department of Energy nor the
   University of California nor the names of its contributors may be
   used to endorse or promote products derived from this software
   without prior written permission.
*/

#ifndef __PRIV_MISC_H
#define __PRIV_MISC_H


/* Allocate(zeroed), free, strdup, memdup, all in VG_AR_DINFO. */
void*  ML_(dinfo_zalloc)( HChar* cc, SizeT szB );
void   ML_(dinfo_free)( void* v );
UChar* ML_(dinfo_strdup)( HChar* cc, const UChar* str );
UChar* ML_(dinfo_memdup)( HChar* cc, UChar* str, SizeT nStr );

/* Extract (possibly unaligned) data of various sizes from a buffer. */
Short ML_(read_Short)( UChar* data );
Int ML_(read_Int)( UChar* data );
Long ML_(read_Long)( UChar* data );
UShort ML_(read_UShort)( UChar* data );
UWord ML_(read_UWord)( UChar* data );
UInt ML_(read_UInt)( UChar* data );
ULong ML_(read_ULong)( UChar* data );
UChar ML_(read_UChar)( UChar* data );
Addr ML_(read_Addr)( UChar* data );

UChar* ML_(write_UShort)( UChar* ptr, UShort val );
UChar* ML_(write_UInt)( UChar* ptr, UInt val );
UChar* ML_(write_ULong)( UChar* ptr, ULong val );
UChar* ML_(write_UChar)( UChar* ptr, UChar val );
UChar* ML_(write_Addr)( UChar* ptr, Addr val );

/* A handy type, a la Haskell's Maybe type.  Yes, I know, C sucks.
   Been there.  Done that.  Seen the movie.  Got the T-shirt.  Etc. */
typedef struct { ULong ul; Bool b; } MaybeULong;


#endif /* ndef __PRIV_MISC_H */

/*--------------------------------------------------------------------*/
/*--- end                                              priv_misc.h ---*/
/*--------------------------------------------------------------------*/
