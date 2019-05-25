
/*--------------------------------------------------------------------*/
/*--- Misc simple stuff lacking a better home.        priv_misc.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2008-2017 OpenWorks LLP
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
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.

   Neither the names of the U.S. Department of Energy nor the
   University of California nor the names of its contributors may be
   used to endorse or promote products derived from this software
   without prior written permission.
*/

#ifndef __PRIV_MISC_H
#define __PRIV_MISC_H

#include "pub_core_basics.h"    // SizeT

/* Allocate(zeroed), free, strdup, memdup, shrink, all in VG_AR_DINFO.
   The allocation functions never return NULL. */
void*  ML_(dinfo_zalloc)( const HChar* cc, SizeT szB );
void   ML_(dinfo_free)( void* v );
HChar* ML_(dinfo_strdup)( const HChar* cc, const HChar* str );
void*  ML_(dinfo_memdup)( const HChar* cc, const void* str, SizeT nStr );
void*  ML_(dinfo_realloc) ( const HChar* cc, void* ptr, SizeT new_size );
void   ML_(dinfo_shrink_block)( void* ptr, SizeT szB );

/* Define functions to read/write types of various sizes from/to a
   (potentially unaligned) UChar *data buffer.
   Some archs can do efficient unaligned access. For these archs,
   do the load/store directly. For others, call the UAS (Un Aligned Safe)
   functions. */
#if defined(VGA_x86) || defined(VGA_amd64)

#define DEF_READ(type) \
static inline type VGAPPEND(vgModuleLocal_read_,type) ( const UChar* data ) \
{ \
   return (*(const type*)(data)); \
} \
type VGAPPEND(vgModuleLocal_readUAS_,type) ( const UChar* data )

#define DEF_WRITE(type) \
static inline UChar* VGAPPEND(vgModuleLocal_write_,type) ( UChar* ptr, type val ) \
{ \
   (*(type*)(ptr)) = val; \
   return ptr + sizeof(type);   \
} \
UChar* VGAPPEND(vgModuleLocal_writeUAS_,type) ( UChar* ptr, type val )

#else

#define DEF_READ(type) \
type VGAPPEND(vgModuleLocal_readUAS_,type) ( const UChar* data ); \
static inline type VGAPPEND(vgModuleLocal_read_,type) ( const UChar* data ) \
{ \
   return VGAPPEND(vgModuleLocal_readUAS_,type)(data); \
}

#define DEF_WRITE(type) \
UChar* VGAPPEND(vgModuleLocal_writeUAS_,type) ( UChar* ptr, type val ); \
static inline UChar* VGAPPEND(vgModuleLocal_write_,type) ( UChar* ptr, type val ) \
{ \
   return VGAPPEND(vgModuleLocal_writeUAS_,type)(ptr,val); \
}

#endif

/* Defines a bunch of functions such as
   Short ML_(read_Short)( const UChar* data );
   Int ML_(read_Int)( const UChar* data );
   ... */
DEF_READ(Short);
DEF_READ(Int);
DEF_READ(Long);
DEF_READ(UShort);
DEF_READ(UWord);
DEF_READ(UInt);
DEF_READ(ULong);
DEF_READ(Addr);

/* Defines a bunch of functions such as
   UChar* ML_(write_UShort)( UChar* ptr, UShort val );
   UChar* ML_(write_UInt)( UChar* ptr, UInt val );
   ... */
DEF_WRITE(UShort);
DEF_WRITE(UInt);
DEF_WRITE(ULong);
DEF_WRITE(Addr);

static inline UChar ML_(read_UChar)( const UChar* data )
{
   return data[0];
}
static inline UChar* ML_(write_UChar)( UChar* ptr, UChar val )
{  
   ptr[0] = val;
   return ptr + sizeof(UChar);
}

/* A handy type, a la Haskell's Maybe type.  Yes, I know, C sucks.
   Been there.  Done that.  Seen the movie.  Got the T-shirt.  Etc. */
typedef struct { ULong ul; Bool b; } MaybeULong;


#endif /* ndef __PRIV_MISC_H */

/*--------------------------------------------------------------------*/
/*--- end                                              priv_misc.h ---*/
/*--------------------------------------------------------------------*/
