/*---------------------------------------------------------------*/
/*--- begin                           guest_generic_helpers.h ---*/
/*---------------------------------------------------------------*/

/*
     This file is part of Valgrind, a dynamic binary instrumentation
     framework.

     Copyright (C) 2026 Alexandra Hájková <ahajkova@redhat.com>

     This program is free software; you can redistribute it and/or
     modify it under the terms of the GNU General Public License as
     published by the Free Software Foundation; either version 3 of the
     License, or (at your option) any later version.

     This program is distributed in the hope that it will be useful, but
     WITHOUT ANY WARRANTY; without even the implied warranty of
     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
     General Public License for more details.

     You should have received a copy of the GNU General Public License
     along with this program; if not, see <http://www.gnu.org/licenses/>.

     The GNU General Public License is contained in the file COPYING.
*/

/*   This file contains small helper functions shared between the x86
     and amd64 guest front ends and their corresponding helper files.
     These are pure C utilities (no VEX IR dependencies) used by both
     IR-building code (via guest_generic_sse.h) and runtime clean
     helpers (via guest_{amd64,x86}_helpers.c).
*/

#ifndef __VEX_GUEST_GENERIC_HELPERS_H
#define __VEX_GUEST_GENERIC_HELPERS_H

#include "libvex_basictypes.h"

static inline UChar abdU8 ( UChar xx, UChar yy ) {
   return toUChar(xx>yy ? xx-yy : yy-xx);
}

static inline UChar sel8x8_7 ( ULong w64 ) {
   UInt hi32 = toUInt(w64 >> 32);
   return toUChar(hi32 >> 24);
}
static inline UChar sel8x8_6 ( ULong w64 ) {
   UInt hi32 = toUInt(w64 >> 32);
   return toUChar(hi32 >> 16);
}
static inline UChar sel8x8_5 ( ULong w64 ) {
   UInt hi32 = toUInt(w64 >> 32);
   return toUChar(hi32 >> 8);
}
static inline UChar sel8x8_4 ( ULong w64 ) {
   UInt hi32 = toUInt(w64 >> 32);
   return toUChar(hi32 >> 0);
}
static inline UChar sel8x8_3 ( ULong w64 ) {
   UInt lo32 = toUInt(w64);
   return toUChar(lo32 >> 24);
}
static inline UChar sel8x8_2 ( ULong w64 ) {
   UInt lo32 = toUInt(w64);
   return toUChar(lo32 >> 16);
}
static inline UChar sel8x8_1 ( ULong w64 ) {
   UInt lo32 = toUInt(w64);
   return toUChar(lo32 >> 8);
}
static inline UChar sel8x8_0 ( ULong w64 ) {
   UInt lo32 = toUInt(w64);
   return toUChar(lo32 >> 0);
}

static inline ULong sad_8x4 ( ULong xx, ULong yy )
{
   UInt t = 0;
   t += (UInt)abdU8( sel8x8_3(xx), sel8x8_3(yy) );
   t += (UInt)abdU8( sel8x8_2(xx), sel8x8_2(yy) );
   t += (UInt)abdU8( sel8x8_1(xx), sel8x8_1(yy) );
   t += (UInt)abdU8( sel8x8_0(xx), sel8x8_0(yy) );
   return (ULong)t;
}

/* CALLED FROM GENERATED CODE: CLEAN HELPER */
static inline ULong g_calc_mpsadbw ( ULong sHi, ULong sLo,
                               ULong dHi, ULong dLo,
                               ULong imm_and_return_control_bit )
{
   UInt imm8     = imm_and_return_control_bit & 7;
   Bool calcHi   = (imm_and_return_control_bit >> 7) & 1;
   UInt srcOffsL = imm8 & 3; /* src offs in 32-bit (L) chunks */
   UInt dstOffsL = (imm8 >> 2) & 1; /* dst offs in ditto chunks */
   /* For src we only need 32 bits, so get them into the
      lower half of a 64 bit word. */
   ULong src = ((srcOffsL & 2) ? sHi : sLo) >> (32 * (srcOffsL & 1));
   /* For dst we need to get hold of 56 bits (7 bytes) from a total of
      11 bytes.  If calculating the low part of the result, need bytes
      dstOffsL * 4 + (0 .. 6); if calculating the high part,
      dstOffsL * 4 + (4 .. 10). */
   ULong dst;
   /* dstOffL = 0, Lo  ->  0 .. 6
      dstOffL = 1, Lo  ->  4 .. 10
      dstOffL = 0, Hi  ->  4 .. 10
      dstOffL = 1, Hi  ->  8 .. 14
   */
   if (calcHi && dstOffsL) {
      /* 8 .. 14 */
      dst = dHi & 0x00FFFFFFFFFFFFFFULL;
   }
   else if (!calcHi && !dstOffsL) {
      /* 0 .. 6 */
      dst = dLo & 0x00FFFFFFFFFFFFFFULL;
   }
   else {
      /* 4 .. 10 */
      dst = (dLo >> 32) | ((dHi & 0x00FFFFFFULL) << 32);
   }
   ULong r0  = sad_8x4( dst >>  0, src );
   ULong r1  = sad_8x4( dst >>  8, src );
   ULong r2  = sad_8x4( dst >> 16, src );
   ULong r3  = sad_8x4( dst >> 24, src );
   ULong res = (r3 << 48) | (r2 << 32) | (r1 << 16) | r0;
   return res;
}

#endif /* ndef __VEX_GUEST_GENERIC_HELPERS_H */

/*---------------------------------------------------------------*/
/*--- end                             guest_generic_helpers.h ---*/
/*---------------------------------------------------------------*/
