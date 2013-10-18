
/*--------------------------------------------------------------------*/
/*--- Misc simple stuff lacking a better home.              misc.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2008-2013 OpenWorks LLP
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

#include "pub_core_basics.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcassert.h"
#include "pub_core_mallocfree.h"
#include "pub_core_xarray.h"

#include "priv_misc.h"            /* self */


void* ML_(dinfo_zalloc) ( const HChar* cc, SizeT szB ) {
   void* v;
   vg_assert(szB > 0);
   v = VG_(arena_malloc)( VG_AR_DINFO, cc, szB );
   vg_assert(v);
   VG_(memset)(v, 0, szB);
   return v;
}

void ML_(dinfo_free) ( void* v ) {
   VG_(arena_free)( VG_AR_DINFO, v );
}

HChar* ML_(dinfo_strdup) ( const HChar* cc, const HChar* str ) {
   return VG_(arena_strdup)( VG_AR_DINFO, cc, str );
}

void* ML_(dinfo_memdup) ( const HChar* cc, void* str, SizeT nStr ) {
   void* dst = VG_(arena_malloc)( VG_AR_DINFO, cc, nStr );
   tl_assert(dst);
   VG_(memcpy)(dst, str, nStr);
   return dst;
}

static inline Bool host_is_little_endian ( void ) {
   UInt x = 0x76543210;
   UChar* p = (UChar*)(&x);
   return toBool(*p == 0x10);
}

Short ML_(read_Short)( UChar* data ) {
   Short r = 0;
   if (host_is_little_endian()) {
      r = data[0]
          | ( ((UInt)data[1]) << 8 );
   } else {
      r = data[1]
          | ( ((UInt)data[0]) << 8 );
   }
   return r;
}

Int ML_(read_Int) ( UChar* data ) {
   Int r = 0;
   if (host_is_little_endian()) {
      r = data[0]
          | ( ((UInt)data[1]) << 8 )
          | ( ((UInt)data[2]) << 16 )
          | ( ((UInt)data[3]) << 24 );
   } else {
      r = data[3]
          | ( ((UInt)data[2]) << 8 )
          | ( ((UInt)data[1]) << 16 )
          | ( ((UInt)data[0]) << 24 );
   }
   return r;
}

Long ML_(read_Long) ( UChar* data ) {
   Long r = 0;
   if (host_is_little_endian()) {
      r = data[0]
          | ( ((ULong)data[1]) << 8 )
          | ( ((ULong)data[2]) << 16 )
          | ( ((ULong)data[3]) << 24 )
          | ( ((ULong)data[4]) << 32 )
          | ( ((ULong)data[5]) << 40 )
          | ( ((ULong)data[6]) << 48 )
          | ( ((ULong)data[7]) << 56 );
   } else {
      r = data[7]
          | ( ((ULong)data[6]) << 8 )
          | ( ((ULong)data[5]) << 16 )
          | ( ((ULong)data[4]) << 24 )
          | ( ((ULong)data[3]) << 32 )
          | ( ((ULong)data[2]) << 40 )
          | ( ((ULong)data[1]) << 48 )
          | ( ((ULong)data[0]) << 56 );
   }
   return r;
}

UShort ML_(read_UShort) ( UChar* data ) {
   UInt r = 0;
   if (host_is_little_endian()) {
      r = data[0]
          | ( ((UInt)data[1]) << 8 );
   } else {
      r = data[1]
          | ( ((UInt)data[0]) << 8 );
   }
   return r;
}

UChar *ML_(write_UShort) ( UChar* ptr, UShort val ) {
   if (host_is_little_endian()) {
      ptr[0] = val & 0xff;
      ptr[1] = ( val >> 8 ) & 0xff;
   } else {
      ptr[0] = ( val >> 8 ) & 0xff;
      ptr[1] = val & 0xff;
   }
   return ptr + sizeof(UShort);
}

UWord ML_(read_UWord) ( UChar* data ) {
   if (sizeof(UWord) == sizeof(UInt)) {
      return ML_(read_UInt)(data);
   } else if  (sizeof(UWord) == sizeof(ULong)) {
      return ML_(read_ULong)(data);
   } else {
      vg_assert(0);
   }
}

UInt ML_(read_UInt) ( UChar* data ) {
   UInt r = 0;
   if (host_is_little_endian()) {
      r = data[0]
          | ( ((UInt)data[1]) << 8 )
          | ( ((UInt)data[2]) << 16 )
          | ( ((UInt)data[3]) << 24 );
   } else {
      r = data[3]
          | ( ((UInt)data[2]) << 8 )
          | ( ((UInt)data[1]) << 16 )
          | ( ((UInt)data[0]) << 24 );
   }
   return r;
}

UChar* ML_(write_UInt) ( UChar* ptr, UInt val ) {
   if (host_is_little_endian()) {
      ptr[0] = val & 0xff;
      ptr[1] = ( val >> 8 ) & 0xff;
      ptr[2] = ( val >> 16 ) & 0xff;
      ptr[3] = ( val >> 24 ) & 0xff;
   } else {
      ptr[0] = ( val >> 24 ) & 0xff;
      ptr[1] = ( val >> 16 ) & 0xff;
      ptr[2] = ( val >> 8 ) & 0xff;
      ptr[3] = val & 0xff;
   }
   return ptr + sizeof(UInt);
}

ULong ML_(read_ULong) ( UChar* data ) {
   ULong r = 0;
   if (host_is_little_endian()) {
      r = data[0]
       | ( ((ULong)data[1]) << 8 )
       | ( ((ULong)data[2]) << 16 )
       | ( ((ULong)data[3]) << 24 )
       | ( ((ULong)data[4]) << 32 )
       | ( ((ULong)data[5]) << 40 )
       | ( ((ULong)data[6]) << 48 )
       | ( ((ULong)data[7]) << 56 );
   } else {
      r = data[7]
       | ( ((ULong)data[6]) << 8 )
       | ( ((ULong)data[5]) << 16 )
       | ( ((ULong)data[4]) << 24 )
       | ( ((ULong)data[3]) << 32 )
       | ( ((ULong)data[2]) << 40 )
       | ( ((ULong)data[1]) << 48 )
       | ( ((ULong)data[0]) << 56 );
   }
   return r;
}

UChar* ML_(write_ULong) ( UChar* ptr, ULong val ) {
   if (host_is_little_endian()) {
      ptr[0] = val & 0xff;
      ptr[1] = ( val >> 8 ) & 0xff;
      ptr[2] = ( val >> 16 ) & 0xff;
      ptr[3] = ( val >> 24 ) & 0xff;
      ptr[4] = ( val >> 32 ) & 0xff;
      ptr[5] = ( val >> 40 ) & 0xff;
      ptr[6] = ( val >> 48 ) & 0xff;
      ptr[7] = ( val >> 56 ) & 0xff;
   } else {
      ptr[0] = ( val >> 56 ) & 0xff;
      ptr[1] = ( val >> 48 ) & 0xff;
      ptr[2] = ( val >> 40 ) & 0xff;
      ptr[3] = ( val >> 32 ) & 0xff;
      ptr[4] = ( val >> 24 ) & 0xff;
      ptr[5] = ( val >> 16 ) & 0xff;
      ptr[6] = ( val >> 8 ) & 0xff;
      ptr[7] = val & 0xff;
   }
   return ptr + sizeof(ULong);
}

UChar ML_(read_UChar) ( UChar* data ) {
   return data[0];
}

UChar* ML_(write_UChar) ( UChar* ptr, UChar val ) {
   ptr[0] = val;
   return ptr + sizeof(UChar);
}

Addr ML_(read_Addr) ( UChar* data ) {
   if (sizeof(Addr) == sizeof(UInt)) {
      return ML_(read_UInt)(data);
   } else if  (sizeof(Addr) == sizeof(ULong)) {
      return ML_(read_ULong)(data);
   } else {
      vg_assert(0);
   }
}

UChar* ML_(write_Addr) ( UChar* ptr, Addr val ) {
   if (sizeof(Addr) == sizeof(UInt)) {
      return ML_(write_UInt)(ptr, val);
   } else if  (sizeof(Addr) == sizeof(ULong)) {
      return ML_(write_ULong)(ptr, val);
   } else {
      vg_assert(0);
   }
}


/*--------------------------------------------------------------------*/
/*--- end                                                   misc.c ---*/
/*--------------------------------------------------------------------*/
