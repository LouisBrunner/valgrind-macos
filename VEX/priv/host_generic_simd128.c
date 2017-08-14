
/*---------------------------------------------------------------*/
/*--- begin                            host_generic_simd128.c ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2010-2010 OpenWorks GbR
      info@open-works.net

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
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
   02110-1301, USA.

   The GNU General Public License is contained in the file COPYING.
*/

/* Generic helper functions for doing 128-bit SIMD arithmetic in cases
   where the instruction selectors cannot generate code in-line.
   These are purely back-end entities and cannot be seen/referenced
   from IR. */

#include "libvex_basictypes.h"
#include "host_generic_simd128.h"


/* Primitive helpers always take args of the real type (signed vs
   unsigned) but return an unsigned result, so there's no conversion
   weirdness when stuffing results back in the V128 union fields,
   which are all unsigned. */

static inline UInt mul32 ( Int xx, Int yy )
{
   Int t = ((Int)xx) * ((Int)yy);
   return toUInt(t);
}

static inline UInt max32S ( Int xx, Int yy )
{
   return toUInt((xx > yy) ? xx : yy);
}

static inline UInt min32S ( Int xx, Int yy )
{
   return toUInt((xx < yy) ? xx : yy);
}

static inline UInt max32U ( UInt xx, UInt yy )
{
   return toUInt((xx > yy) ? xx : yy);
}

static inline UInt min32U ( UInt xx, UInt yy )
{
   return toUInt((xx < yy) ? xx : yy);
}

static inline UShort max16U ( UShort xx, UShort yy )
{
   return toUShort((xx > yy) ? xx : yy);
}

static inline UShort min16U ( UShort xx, UShort yy )
{
   return toUShort((xx < yy) ? xx : yy);
}

static inline UChar max8S ( Char xx, Char yy )
{
   return toUChar((xx > yy) ? xx : yy);
}

static inline UChar min8S ( Char xx, Char yy )
{
   return toUChar((xx < yy) ? xx : yy);
}

static inline ULong cmpGT64S ( Long xx, Long yy )
{
   return (((Long)xx) > ((Long)yy))
             ? 0xFFFFFFFFFFFFFFFFULL : 0ULL;
}

static inline ULong sar64 ( ULong v, UInt n )
{
   return ((Long)v) >> n;
}

static inline UChar sar8 ( UChar v, UInt n )
{
   return toUChar(((Char)v) >> n);
}

void h_generic_calc_Mul32x4 ( /*OUT*/V128* res,
                              V128* argL, V128* argR )
{
   res->w32[0] = mul32(argL->w32[0], argR->w32[0]);
   res->w32[1] = mul32(argL->w32[1], argR->w32[1]);
   res->w32[2] = mul32(argL->w32[2], argR->w32[2]);
   res->w32[3] = mul32(argL->w32[3], argR->w32[3]);
}

void h_generic_calc_Max32Sx4 ( /*OUT*/V128* res,
                               V128* argL, V128* argR )
{
   res->w32[0] = max32S(argL->w32[0], argR->w32[0]);
   res->w32[1] = max32S(argL->w32[1], argR->w32[1]);
   res->w32[2] = max32S(argL->w32[2], argR->w32[2]);
   res->w32[3] = max32S(argL->w32[3], argR->w32[3]);
}

void h_generic_calc_Min32Sx4 ( /*OUT*/V128* res,
                               V128* argL, V128* argR )
{
   res->w32[0] = min32S(argL->w32[0], argR->w32[0]);
   res->w32[1] = min32S(argL->w32[1], argR->w32[1]);
   res->w32[2] = min32S(argL->w32[2], argR->w32[2]);
   res->w32[3] = min32S(argL->w32[3], argR->w32[3]);
}

void h_generic_calc_Max32Ux4 ( /*OUT*/V128* res,
                               V128* argL, V128* argR )
{
   res->w32[0] = max32U(argL->w32[0], argR->w32[0]);
   res->w32[1] = max32U(argL->w32[1], argR->w32[1]);
   res->w32[2] = max32U(argL->w32[2], argR->w32[2]);
   res->w32[3] = max32U(argL->w32[3], argR->w32[3]);
}

void h_generic_calc_Min32Ux4 ( /*OUT*/V128* res,
                               V128* argL, V128* argR )
{
   res->w32[0] = min32U(argL->w32[0], argR->w32[0]);
   res->w32[1] = min32U(argL->w32[1], argR->w32[1]);
   res->w32[2] = min32U(argL->w32[2], argR->w32[2]);
   res->w32[3] = min32U(argL->w32[3], argR->w32[3]);
}

void h_generic_calc_Max16Ux8 ( /*OUT*/V128* res,
                               V128* argL, V128* argR )
{
   res->w16[0] = max16U(argL->w16[0], argR->w16[0]);
   res->w16[1] = max16U(argL->w16[1], argR->w16[1]);
   res->w16[2] = max16U(argL->w16[2], argR->w16[2]);
   res->w16[3] = max16U(argL->w16[3], argR->w16[3]);
   res->w16[4] = max16U(argL->w16[4], argR->w16[4]);
   res->w16[5] = max16U(argL->w16[5], argR->w16[5]);
   res->w16[6] = max16U(argL->w16[6], argR->w16[6]);
   res->w16[7] = max16U(argL->w16[7], argR->w16[7]);
}

void h_generic_calc_Min16Ux8 ( /*OUT*/V128* res,
                               V128* argL, V128* argR )
{
   res->w16[0] = min16U(argL->w16[0], argR->w16[0]);
   res->w16[1] = min16U(argL->w16[1], argR->w16[1]);
   res->w16[2] = min16U(argL->w16[2], argR->w16[2]);
   res->w16[3] = min16U(argL->w16[3], argR->w16[3]);
   res->w16[4] = min16U(argL->w16[4], argR->w16[4]);
   res->w16[5] = min16U(argL->w16[5], argR->w16[5]);
   res->w16[6] = min16U(argL->w16[6], argR->w16[6]);
   res->w16[7] = min16U(argL->w16[7], argR->w16[7]);
}

void h_generic_calc_Max8Sx16 ( /*OUT*/V128* res,
                               V128* argL, V128* argR )
{
   res->w8[ 0] = max8S(argL->w8[ 0], argR->w8[ 0]);
   res->w8[ 1] = max8S(argL->w8[ 1], argR->w8[ 1]);
   res->w8[ 2] = max8S(argL->w8[ 2], argR->w8[ 2]);
   res->w8[ 3] = max8S(argL->w8[ 3], argR->w8[ 3]);
   res->w8[ 4] = max8S(argL->w8[ 4], argR->w8[ 4]);
   res->w8[ 5] = max8S(argL->w8[ 5], argR->w8[ 5]);
   res->w8[ 6] = max8S(argL->w8[ 6], argR->w8[ 6]);
   res->w8[ 7] = max8S(argL->w8[ 7], argR->w8[ 7]);
   res->w8[ 8] = max8S(argL->w8[ 8], argR->w8[ 8]);
   res->w8[ 9] = max8S(argL->w8[ 9], argR->w8[ 9]);
   res->w8[10] = max8S(argL->w8[10], argR->w8[10]);
   res->w8[11] = max8S(argL->w8[11], argR->w8[11]);
   res->w8[12] = max8S(argL->w8[12], argR->w8[12]);
   res->w8[13] = max8S(argL->w8[13], argR->w8[13]);
   res->w8[14] = max8S(argL->w8[14], argR->w8[14]);
   res->w8[15] = max8S(argL->w8[15], argR->w8[15]);
}

void h_generic_calc_Min8Sx16 ( /*OUT*/V128* res,
                               V128* argL, V128* argR )
{
   res->w8[ 0] = min8S(argL->w8[ 0], argR->w8[ 0]);
   res->w8[ 1] = min8S(argL->w8[ 1], argR->w8[ 1]);
   res->w8[ 2] = min8S(argL->w8[ 2], argR->w8[ 2]);
   res->w8[ 3] = min8S(argL->w8[ 3], argR->w8[ 3]);
   res->w8[ 4] = min8S(argL->w8[ 4], argR->w8[ 4]);
   res->w8[ 5] = min8S(argL->w8[ 5], argR->w8[ 5]);
   res->w8[ 6] = min8S(argL->w8[ 6], argR->w8[ 6]);
   res->w8[ 7] = min8S(argL->w8[ 7], argR->w8[ 7]);
   res->w8[ 8] = min8S(argL->w8[ 8], argR->w8[ 8]);
   res->w8[ 9] = min8S(argL->w8[ 9], argR->w8[ 9]);
   res->w8[10] = min8S(argL->w8[10], argR->w8[10]);
   res->w8[11] = min8S(argL->w8[11], argR->w8[11]);
   res->w8[12] = min8S(argL->w8[12], argR->w8[12]);
   res->w8[13] = min8S(argL->w8[13], argR->w8[13]);
   res->w8[14] = min8S(argL->w8[14], argR->w8[14]);
   res->w8[15] = min8S(argL->w8[15], argR->w8[15]);
}

void h_generic_calc_CmpGT64Sx2 ( /*OUT*/V128* res,
                                 V128* argL, V128* argR )
{
   res->w64[0] = cmpGT64S(argL->w64[0], argR->w64[0]);
   res->w64[1] = cmpGT64S(argL->w64[1], argR->w64[1]);
}

/* ------------ Shifting ------------ */
/* Note that because these primops are undefined if the shift amount
   equals or exceeds the lane width, the shift amount is masked so
   that the scalar shifts are always in range.  In fact, given the
   semantics of these primops (Sar64x2, etc) it is an error if in
   fact we are ever given an out-of-range shift amount. 
*/
void h_generic_calc_SarN64x2 ( /*OUT*/V128* res,
                               V128* argL, UInt nn)
{
   /* vassert(nn < 64); */
   nn &= 63;
   res->w64[0] = sar64(argL->w64[0], nn);
   res->w64[1] = sar64(argL->w64[1], nn);
}

void h_generic_calc_SarN8x16 ( /*OUT*/V128* res,
                              V128* argL, UInt nn)
{
   /* vassert(nn < 8); */
   nn &= 7;
   res->w8[ 0] = sar8(argL->w8[ 0], nn);
   res->w8[ 1] = sar8(argL->w8[ 1], nn);
   res->w8[ 2] = sar8(argL->w8[ 2], nn);
   res->w8[ 3] = sar8(argL->w8[ 3], nn);
   res->w8[ 4] = sar8(argL->w8[ 4], nn);
   res->w8[ 5] = sar8(argL->w8[ 5], nn);
   res->w8[ 6] = sar8(argL->w8[ 6], nn);
   res->w8[ 7] = sar8(argL->w8[ 7], nn);
   res->w8[ 8] = sar8(argL->w8[ 8], nn);
   res->w8[ 9] = sar8(argL->w8[ 9], nn);
   res->w8[10] = sar8(argL->w8[10], nn);
   res->w8[11] = sar8(argL->w8[11], nn);
   res->w8[12] = sar8(argL->w8[12], nn);
   res->w8[13] = sar8(argL->w8[13], nn);
   res->w8[14] = sar8(argL->w8[14], nn);
   res->w8[15] = sar8(argL->w8[15], nn);
}

/*---------------------------------------------------------------*/
/*--- end                              host_generic_simd128.c ---*/
/*---------------------------------------------------------------*/
