
/*---------------------------------------------------------------*/
/*--- begin                             host_generic_simd64.c ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2004-2011 OpenWorks LLP
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

   Neither the names of the U.S. Department of Energy nor the
   University of California nor the names of its contributors may be
   used to endorse or promote products derived from this software
   without prior written permission.
*/

/* Generic helper functions for doing 64-bit SIMD arithmetic in cases
   where the instruction selectors cannot generate code in-line.
   These are purely back-end entities and cannot be seen/referenced
   from IR. */

#include "libvex_basictypes.h"
#include "host_generic_simd64.h"



/* Tuple/select functions for 32x2 vectors. */

static inline ULong mk32x2 ( UInt w1, UInt w0 ) {
   return (((ULong)w1) << 32) | ((ULong)w0);
}

static inline UInt sel32x2_1 ( ULong w64 ) {
   return 0xFFFFFFFF & toUInt(w64 >> 32);
}
static inline UInt sel32x2_0 ( ULong w64 ) {
   return 0xFFFFFFFF & toUInt(w64);
}


/* Tuple/select functions for 16x4 vectors.  gcc is pretty hopeless
   with 64-bit shifts so we give it a hand. */

static inline ULong mk16x4 ( UShort w3, UShort w2, 
                             UShort w1, UShort w0 ) {
   UInt hi32 = (((UInt)w3) << 16) | ((UInt)w2);
   UInt lo32 = (((UInt)w1) << 16) | ((UInt)w0);
   return mk32x2(hi32, lo32);
}

static inline UShort sel16x4_3 ( ULong w64 ) {
   UInt hi32 = toUInt(w64 >> 32);
   return toUShort(0xFFFF & (hi32 >> 16));
}
static inline UShort sel16x4_2 ( ULong w64 ) {
   UInt hi32 = toUInt(w64 >> 32);
   return toUShort(0xFFFF & hi32);
}
static inline UShort sel16x4_1 ( ULong w64 ) {
   UInt lo32 = (UInt)w64;
   return toUShort(0xFFFF & (lo32 >> 16));
}
static inline UShort sel16x4_0 ( ULong w64 ) {
   UInt lo32 = (UInt)w64;
   return toUShort(0xFFFF & lo32);
}


/* Tuple/select functions for 8x8 vectors. */

static inline ULong mk8x8 ( UChar w7, UChar w6,
                            UChar w5, UChar w4,
                            UChar w3, UChar w2,
                            UChar w1, UChar w0 ) {
   UInt hi32 =   (((UInt)w7) << 24) | (((UInt)w6) << 16)
               | (((UInt)w5) << 8)  | (((UInt)w4) << 0);
   UInt lo32 =   (((UInt)w3) << 24) | (((UInt)w2) << 16)
               | (((UInt)w1) << 8)  | (((UInt)w0) << 0);
   return mk32x2(hi32, lo32);
}

static inline UChar sel8x8_7 ( ULong w64 ) {
   UInt hi32 = toUInt(w64 >> 32);
   return toUChar(0xFF & (hi32 >> 24));
}
static inline UChar sel8x8_6 ( ULong w64 ) {
   UInt hi32 = toUInt(w64 >> 32);
   return toUChar(0xFF & (hi32 >> 16));
}
static inline UChar sel8x8_5 ( ULong w64 ) {
   UInt hi32 = toUInt(w64 >> 32);
   return toUChar(0xFF & (hi32 >> 8));
}
static inline UChar sel8x8_4 ( ULong w64 ) {
   UInt hi32 = toUInt(w64 >> 32);
   return toUChar(0xFF & (hi32 >> 0));
}
static inline UChar sel8x8_3 ( ULong w64 ) {
   UInt lo32 = (UInt)w64;
   return toUChar(0xFF & (lo32 >> 24));
}
static inline UChar sel8x8_2 ( ULong w64 ) {
   UInt lo32 = (UInt)w64;
   return toUChar(0xFF & (lo32 >> 16));
}
static inline UChar sel8x8_1 ( ULong w64 ) {
   UInt lo32 = (UInt)w64;
   return toUChar(0xFF & (lo32 >> 8));
}
static inline UChar sel8x8_0 ( ULong w64 ) {
   UInt lo32 = (UInt)w64;
   return toUChar(0xFF & (lo32 >> 0));
}

static inline UChar index8x8 ( ULong w64, UChar ix ) {
   ix &= 7;
   return toUChar((w64 >> (8*ix)) & 0xFF);
}


/* Scalar helpers. */

static inline Short qadd16S ( Short xx, Short yy ) 
{
   Int t = ((Int)xx) + ((Int)yy);
   if (t < -32768) t = -32768;
   if (t > 32767)  t = 32767;
   return (Short)t;
}

static inline Char qadd8S ( Char xx, Char yy )
{
   Int t = ((Int)xx) + ((Int)yy);
   if (t < -128) t = -128;
   if (t > 127)  t = 127;
   return (Char)t;
}

static inline UShort qadd16U ( UShort xx, UShort yy )
{
   UInt t = ((UInt)xx) + ((UInt)yy);
   if (t > 0xFFFF) t = 0xFFFF;
   return (UShort)t;
}

static inline UChar qadd8U ( UChar xx, UChar yy )
{
   UInt t = ((UInt)xx) + ((UInt)yy);
   if (t > 0xFF) t = 0xFF;
   return (UChar)t;
}

static inline Short qsub16S ( Short xx, Short yy )
{
   Int t = ((Int)xx) - ((Int)yy);
   if (t < -32768) t = -32768;
   if (t > 32767)  t = 32767;
   return (Short)t;
}

static inline Char qsub8S ( Char xx, Char yy )
{
   Int t = ((Int)xx) - ((Int)yy);
   if (t < -128) t = -128;
   if (t > 127)  t = 127;
   return (Char)t;
}

static inline UShort qsub16U ( UShort xx, UShort yy )
{
   Int t = ((Int)xx) - ((Int)yy);
   if (t < 0)      t = 0;
   if (t > 0xFFFF) t = 0xFFFF;
   return (UShort)t;
}

static inline UChar qsub8U ( UChar xx, UChar yy )
{
   Int t = ((Int)xx) - ((Int)yy);
   if (t < 0)    t = 0;
   if (t > 0xFF) t = 0xFF;
   return (UChar)t;
}

static inline Short mul16 ( Short xx, Short yy )
{
   Int t = ((Int)xx) * ((Int)yy);
   return (Short)t;
}

static inline Int mul32 ( Int xx, Int yy )
{
   Int t = ((Int)xx) * ((Int)yy);
   return (Int)t;
}

static inline Short mulhi16S ( Short xx, Short yy )
{
   Int t = ((Int)xx) * ((Int)yy);
   t >>=/*s*/ 16;
   return (Short)t;
}

static inline UShort mulhi16U ( UShort xx, UShort yy )
{
   UInt t = ((UInt)xx) * ((UInt)yy);
   t >>=/*u*/ 16;
   return (UShort)t;
}

static inline UInt cmpeq32 ( UInt xx, UInt yy )
{
   return xx==yy ? 0xFFFFFFFF : 0;
}

static inline UShort cmpeq16 ( UShort xx, UShort yy )
{
   return toUShort(xx==yy ? 0xFFFF : 0);
}

static inline UChar cmpeq8 ( UChar xx, UChar yy )
{
   return toUChar(xx==yy ? 0xFF : 0);
}

static inline UInt cmpgt32S ( Int xx, Int yy )
{
   return xx>yy ? 0xFFFFFFFF : 0;
}

static inline UShort cmpgt16S ( Short xx, Short yy )
{
   return toUShort(xx>yy ? 0xFFFF : 0);
}

static inline UChar cmpgt8S ( Char xx, Char yy )
{
   return toUChar(xx>yy ? 0xFF : 0);
}

static inline UInt cmpnez32 ( UInt xx )
{
   return xx==0 ? 0 : 0xFFFFFFFF;
}

static inline UShort cmpnez16 ( UShort xx )
{
   return toUShort(xx==0 ? 0 : 0xFFFF);
}

static inline UChar cmpnez8 ( UChar xx )
{
   return toUChar(xx==0 ? 0 : 0xFF);
}

static inline Short qnarrow32Sto16S ( UInt xx0 )
{
   Int xx = (Int)xx0;
   if (xx < -32768) xx = -32768;
   if (xx > 32767)  xx = 32767;
   return (Short)xx;
}

static inline Char qnarrow16Sto8S ( UShort xx0 )
{
   Short xx = (Short)xx0;
   if (xx < -128) xx = -128;
   if (xx > 127)  xx = 127;
   return (Char)xx;
}

static inline UChar qnarrow16Sto8U ( UShort xx0 )
{
   Short xx = (Short)xx0;
   if (xx < 0)   xx = 0;
   if (xx > 255) xx = 255;
   return (UChar)xx;
}

static inline UShort narrow32to16 ( UInt xx )
{
   return (UShort)xx;
}

static inline UChar narrow16to8 ( UShort xx )
{
   return (UChar)xx;
}

/* shifts: we don't care about out-of-range ones, since
   that is dealt with at a higher level. */

static inline UChar shl8 ( UChar v, UInt n )
{
   return toUChar(v << n);
}

static inline UChar sar8 ( UChar v, UInt n )
{
   return toUChar(((Char)v) >> n);
}

static inline UShort shl16 ( UShort v, UInt n )
{
   return toUShort(v << n);
}

static inline UShort shr16 ( UShort v, UInt n )
{
   return toUShort((((UShort)v) >> n));
}

static inline UShort sar16 ( UShort v, UInt n )
{
   return toUShort(((Short)v) >> n);
}

static inline UInt shl32 ( UInt v, UInt n )
{
   return v << n;
}

static inline UInt shr32 ( UInt v, UInt n )
{
   return (((UInt)v) >> n);
}

static inline UInt sar32 ( UInt v, UInt n )
{
   return ((Int)v) >> n;
}

static inline UChar avg8U ( UChar xx, UChar yy )
{
   UInt xxi = (UInt)xx;
   UInt yyi = (UInt)yy;
   UInt r   = (xxi + yyi + 1) >> 1;
   return (UChar)r;
}

static inline UShort avg16U ( UShort xx, UShort yy )
{
   UInt xxi = (UInt)xx;
   UInt yyi = (UInt)yy;
   UInt r   = (xxi + yyi + 1) >> 1;
   return (UShort)r;
}

static inline Short max16S ( Short xx, Short yy )
{
   return toUShort((xx > yy) ? xx : yy);
}

static inline UChar max8U ( UChar xx, UChar yy )
{
   return toUChar((xx > yy) ? xx : yy);
}

static inline Short min16S ( Short xx, Short yy )
{
   return toUShort((xx < yy) ? xx : yy);
}

static inline UChar min8U ( UChar xx, UChar yy )
{
   return toUChar((xx < yy) ? xx : yy);
}

static inline UShort hadd16U ( UShort xx, UShort yy )
{
   UInt xxi = (UInt)xx;
   UInt yyi = (UInt)yy;
   UInt r   = (xxi + yyi) >> 1;
   return (UShort)r;
}

static inline Short hadd16S ( Short xx, Short yy )
{
   Int xxi = (Int)xx;
   Int yyi = (Int)yy;
   Int r   = (xxi + yyi) >> 1;
   return (Short)r;
}

static inline UShort hsub16U ( UShort xx, UShort yy )
{
   UInt xxi = (UInt)xx;
   UInt yyi = (UInt)yy;
   UInt r   = (xxi - yyi) >> 1;
   return (UShort)r;
}

static inline Short hsub16S ( Short xx, Short yy )
{
   Int xxi = (Int)xx;
   Int yyi = (Int)yy;
   Int r   = (xxi - yyi) >> 1;
   return (Short)r;
}

static inline UChar hadd8U ( UChar xx, UChar yy )
{
   UInt xxi = (UInt)xx;
   UInt yyi = (UInt)yy;
   UInt r   = (xxi + yyi) >> 1;
   return (UChar)r;
}

static inline Char hadd8S ( Char xx, Char yy )
{
   Int xxi = (Int)xx;
   Int yyi = (Int)yy;
   Int r   = (xxi + yyi) >> 1;
   return (Char)r;
}

static inline UChar hsub8U ( UChar xx, UChar yy )
{
   UInt xxi = (UInt)xx;
   UInt yyi = (UInt)yy;
   UInt r   = (xxi - yyi) >> 1;
   return (UChar)r;
}

static inline Char hsub8S ( Char xx, Char yy )
{
   Int xxi = (Int)xx;
   Int yyi = (Int)yy;
   Int r   = (xxi - yyi) >> 1;
   return (Char)r;
}

static inline UInt absdiff8U ( UChar xx, UChar yy )
{
   UInt xxu = (UChar)xx;
   UInt yyu = (UChar)yy;
   return xxu >= yyu  ? xxu - yyu  : yyu - xxu;
}

/* ----------------------------------------------------- */
/* Start of the externally visible functions.  These simply
   implement the corresponding IR primops. */
/* ----------------------------------------------------- */

/* ------------ Normal addition ------------ */

ULong h_generic_calc_Add32x2 ( ULong xx, ULong yy )
{
   return mk32x2(
             sel32x2_1(xx) + sel32x2_1(yy),
             sel32x2_0(xx) + sel32x2_0(yy)
          );
}

ULong h_generic_calc_Add16x4 ( ULong xx, ULong yy )
{
   return mk16x4(
             toUShort( sel16x4_3(xx) + sel16x4_3(yy) ),
             toUShort( sel16x4_2(xx) + sel16x4_2(yy) ),
             toUShort( sel16x4_1(xx) + sel16x4_1(yy) ),
             toUShort( sel16x4_0(xx) + sel16x4_0(yy) )
          );
}

ULong h_generic_calc_Add8x8 ( ULong xx, ULong yy )
{
   return mk8x8(
             toUChar( sel8x8_7(xx) + sel8x8_7(yy) ),
             toUChar( sel8x8_6(xx) + sel8x8_6(yy) ),
             toUChar( sel8x8_5(xx) + sel8x8_5(yy) ),
             toUChar( sel8x8_4(xx) + sel8x8_4(yy) ),
             toUChar( sel8x8_3(xx) + sel8x8_3(yy) ),
             toUChar( sel8x8_2(xx) + sel8x8_2(yy) ),
             toUChar( sel8x8_1(xx) + sel8x8_1(yy) ),
             toUChar( sel8x8_0(xx) + sel8x8_0(yy) )
          );
}

/* ------------ Saturating addition ------------ */

ULong h_generic_calc_QAdd16Sx4 ( ULong xx, ULong yy )
{
   return mk16x4(
             qadd16S( sel16x4_3(xx), sel16x4_3(yy) ),
             qadd16S( sel16x4_2(xx), sel16x4_2(yy) ),
             qadd16S( sel16x4_1(xx), sel16x4_1(yy) ),
             qadd16S( sel16x4_0(xx), sel16x4_0(yy) )
          );
}

ULong h_generic_calc_QAdd8Sx8 ( ULong xx, ULong yy )
{
   return mk8x8(
             qadd8S( sel8x8_7(xx), sel8x8_7(yy) ),
             qadd8S( sel8x8_6(xx), sel8x8_6(yy) ),
             qadd8S( sel8x8_5(xx), sel8x8_5(yy) ),
             qadd8S( sel8x8_4(xx), sel8x8_4(yy) ),
             qadd8S( sel8x8_3(xx), sel8x8_3(yy) ),
             qadd8S( sel8x8_2(xx), sel8x8_2(yy) ),
             qadd8S( sel8x8_1(xx), sel8x8_1(yy) ),
             qadd8S( sel8x8_0(xx), sel8x8_0(yy) )
          );
}

ULong h_generic_calc_QAdd16Ux4 ( ULong xx, ULong yy )
{
   return mk16x4(
             qadd16U( sel16x4_3(xx), sel16x4_3(yy) ),
             qadd16U( sel16x4_2(xx), sel16x4_2(yy) ),
             qadd16U( sel16x4_1(xx), sel16x4_1(yy) ),
             qadd16U( sel16x4_0(xx), sel16x4_0(yy) )
          );
}

ULong h_generic_calc_QAdd8Ux8 ( ULong xx, ULong yy )
{
   return mk8x8(
             qadd8U( sel8x8_7(xx), sel8x8_7(yy) ),
             qadd8U( sel8x8_6(xx), sel8x8_6(yy) ),
             qadd8U( sel8x8_5(xx), sel8x8_5(yy) ),
             qadd8U( sel8x8_4(xx), sel8x8_4(yy) ),
             qadd8U( sel8x8_3(xx), sel8x8_3(yy) ),
             qadd8U( sel8x8_2(xx), sel8x8_2(yy) ),
             qadd8U( sel8x8_1(xx), sel8x8_1(yy) ),
             qadd8U( sel8x8_0(xx), sel8x8_0(yy) )
          );
}

/* ------------ Normal subtraction ------------ */

ULong h_generic_calc_Sub32x2 ( ULong xx, ULong yy )
{
   return mk32x2(
             sel32x2_1(xx) - sel32x2_1(yy),
             sel32x2_0(xx) - sel32x2_0(yy)
          );
}

ULong h_generic_calc_Sub16x4 ( ULong xx, ULong yy )
{
   return mk16x4(
             toUShort( sel16x4_3(xx) - sel16x4_3(yy) ),
             toUShort( sel16x4_2(xx) - sel16x4_2(yy) ),
             toUShort( sel16x4_1(xx) - sel16x4_1(yy) ),
             toUShort( sel16x4_0(xx) - sel16x4_0(yy) )
          );
}

ULong h_generic_calc_Sub8x8 ( ULong xx, ULong yy )
{
   return mk8x8(
             toUChar( sel8x8_7(xx) - sel8x8_7(yy) ),
             toUChar( sel8x8_6(xx) - sel8x8_6(yy) ),
             toUChar( sel8x8_5(xx) - sel8x8_5(yy) ),
             toUChar( sel8x8_4(xx) - sel8x8_4(yy) ),
             toUChar( sel8x8_3(xx) - sel8x8_3(yy) ),
             toUChar( sel8x8_2(xx) - sel8x8_2(yy) ),
             toUChar( sel8x8_1(xx) - sel8x8_1(yy) ),
             toUChar( sel8x8_0(xx) - sel8x8_0(yy) )
          );
}

/* ------------ Saturating subtraction ------------ */

ULong h_generic_calc_QSub16Sx4 ( ULong xx, ULong yy )
{
   return mk16x4(
             qsub16S( sel16x4_3(xx), sel16x4_3(yy) ),
             qsub16S( sel16x4_2(xx), sel16x4_2(yy) ),
             qsub16S( sel16x4_1(xx), sel16x4_1(yy) ),
             qsub16S( sel16x4_0(xx), sel16x4_0(yy) )
          );
}

ULong h_generic_calc_QSub8Sx8 ( ULong xx, ULong yy )
{
   return mk8x8(
             qsub8S( sel8x8_7(xx), sel8x8_7(yy) ),
             qsub8S( sel8x8_6(xx), sel8x8_6(yy) ),
             qsub8S( sel8x8_5(xx), sel8x8_5(yy) ),
             qsub8S( sel8x8_4(xx), sel8x8_4(yy) ),
             qsub8S( sel8x8_3(xx), sel8x8_3(yy) ),
             qsub8S( sel8x8_2(xx), sel8x8_2(yy) ),
             qsub8S( sel8x8_1(xx), sel8x8_1(yy) ),
             qsub8S( sel8x8_0(xx), sel8x8_0(yy) )
          );
}

ULong h_generic_calc_QSub16Ux4 ( ULong xx, ULong yy )
{
   return mk16x4(
             qsub16U( sel16x4_3(xx), sel16x4_3(yy) ),
             qsub16U( sel16x4_2(xx), sel16x4_2(yy) ),
             qsub16U( sel16x4_1(xx), sel16x4_1(yy) ),
             qsub16U( sel16x4_0(xx), sel16x4_0(yy) )
          );
}

ULong h_generic_calc_QSub8Ux8 ( ULong xx, ULong yy )
{
   return mk8x8(
             qsub8U( sel8x8_7(xx), sel8x8_7(yy) ),
             qsub8U( sel8x8_6(xx), sel8x8_6(yy) ),
             qsub8U( sel8x8_5(xx), sel8x8_5(yy) ),
             qsub8U( sel8x8_4(xx), sel8x8_4(yy) ),
             qsub8U( sel8x8_3(xx), sel8x8_3(yy) ),
             qsub8U( sel8x8_2(xx), sel8x8_2(yy) ),
             qsub8U( sel8x8_1(xx), sel8x8_1(yy) ),
             qsub8U( sel8x8_0(xx), sel8x8_0(yy) )
          );
}

/* ------------ Multiplication ------------ */

ULong h_generic_calc_Mul16x4 ( ULong xx, ULong yy )
{
   return mk16x4(
             mul16( sel16x4_3(xx), sel16x4_3(yy) ),
             mul16( sel16x4_2(xx), sel16x4_2(yy) ),
             mul16( sel16x4_1(xx), sel16x4_1(yy) ),
             mul16( sel16x4_0(xx), sel16x4_0(yy) )
          );
}

ULong h_generic_calc_Mul32x2 ( ULong xx, ULong yy )
{
   return mk32x2(
             mul32( sel32x2_1(xx), sel32x2_1(yy) ),
             mul32( sel32x2_0(xx), sel32x2_0(yy) )
          );
}

ULong h_generic_calc_MulHi16Sx4 ( ULong xx, ULong yy )
{
   return mk16x4(
             mulhi16S( sel16x4_3(xx), sel16x4_3(yy) ),
             mulhi16S( sel16x4_2(xx), sel16x4_2(yy) ),
             mulhi16S( sel16x4_1(xx), sel16x4_1(yy) ),
             mulhi16S( sel16x4_0(xx), sel16x4_0(yy) )
          );
}

ULong h_generic_calc_MulHi16Ux4 ( ULong xx, ULong yy )
{
   return mk16x4(
             mulhi16U( sel16x4_3(xx), sel16x4_3(yy) ),
             mulhi16U( sel16x4_2(xx), sel16x4_2(yy) ),
             mulhi16U( sel16x4_1(xx), sel16x4_1(yy) ),
             mulhi16U( sel16x4_0(xx), sel16x4_0(yy) )
          );
}

/* ------------ Comparison ------------ */

ULong h_generic_calc_CmpEQ32x2 ( ULong xx, ULong yy )
{
   return mk32x2(
             cmpeq32( sel32x2_1(xx), sel32x2_1(yy) ),
             cmpeq32( sel32x2_0(xx), sel32x2_0(yy) )
          );
}

ULong h_generic_calc_CmpEQ16x4 ( ULong xx, ULong yy )
{
   return mk16x4(
             cmpeq16( sel16x4_3(xx), sel16x4_3(yy) ),
             cmpeq16( sel16x4_2(xx), sel16x4_2(yy) ),
             cmpeq16( sel16x4_1(xx), sel16x4_1(yy) ),
             cmpeq16( sel16x4_0(xx), sel16x4_0(yy) )
          );
}

ULong h_generic_calc_CmpEQ8x8 ( ULong xx, ULong yy )
{
   return mk8x8(
             cmpeq8( sel8x8_7(xx), sel8x8_7(yy) ),
             cmpeq8( sel8x8_6(xx), sel8x8_6(yy) ),
             cmpeq8( sel8x8_5(xx), sel8x8_5(yy) ),
             cmpeq8( sel8x8_4(xx), sel8x8_4(yy) ),
             cmpeq8( sel8x8_3(xx), sel8x8_3(yy) ),
             cmpeq8( sel8x8_2(xx), sel8x8_2(yy) ),
             cmpeq8( sel8x8_1(xx), sel8x8_1(yy) ),
             cmpeq8( sel8x8_0(xx), sel8x8_0(yy) )
          );
}

ULong h_generic_calc_CmpGT32Sx2 ( ULong xx, ULong yy )
{
   return mk32x2(
             cmpgt32S( sel32x2_1(xx), sel32x2_1(yy) ),
             cmpgt32S( sel32x2_0(xx), sel32x2_0(yy) )
          );
}

ULong h_generic_calc_CmpGT16Sx4 ( ULong xx, ULong yy )
{
   return mk16x4(
             cmpgt16S( sel16x4_3(xx), sel16x4_3(yy) ),
             cmpgt16S( sel16x4_2(xx), sel16x4_2(yy) ),
             cmpgt16S( sel16x4_1(xx), sel16x4_1(yy) ),
             cmpgt16S( sel16x4_0(xx), sel16x4_0(yy) )
          );
}

ULong h_generic_calc_CmpGT8Sx8 ( ULong xx, ULong yy )
{
   return mk8x8(
             cmpgt8S( sel8x8_7(xx), sel8x8_7(yy) ),
             cmpgt8S( sel8x8_6(xx), sel8x8_6(yy) ),
             cmpgt8S( sel8x8_5(xx), sel8x8_5(yy) ),
             cmpgt8S( sel8x8_4(xx), sel8x8_4(yy) ),
             cmpgt8S( sel8x8_3(xx), sel8x8_3(yy) ),
             cmpgt8S( sel8x8_2(xx), sel8x8_2(yy) ),
             cmpgt8S( sel8x8_1(xx), sel8x8_1(yy) ),
             cmpgt8S( sel8x8_0(xx), sel8x8_0(yy) )
          );
}

ULong h_generic_calc_CmpNEZ32x2 ( ULong xx )
{
   return mk32x2(
             cmpnez32( sel32x2_1(xx) ),
             cmpnez32( sel32x2_0(xx) )
          );
}

ULong h_generic_calc_CmpNEZ16x4 ( ULong xx )
{
   return mk16x4(
             cmpnez16( sel16x4_3(xx) ),
             cmpnez16( sel16x4_2(xx) ),
             cmpnez16( sel16x4_1(xx) ),
             cmpnez16( sel16x4_0(xx) )
          );
}

ULong h_generic_calc_CmpNEZ8x8 ( ULong xx )
{
   return mk8x8(
             cmpnez8( sel8x8_7(xx) ),
             cmpnez8( sel8x8_6(xx) ),
             cmpnez8( sel8x8_5(xx) ),
             cmpnez8( sel8x8_4(xx) ),
             cmpnez8( sel8x8_3(xx) ),
             cmpnez8( sel8x8_2(xx) ),
             cmpnez8( sel8x8_1(xx) ),
             cmpnez8( sel8x8_0(xx) )
          );
}

/* ------------ Saturating narrowing ------------ */

ULong h_generic_calc_QNarrowBin32Sto16Sx4 ( ULong aa, ULong bb )
{
   UInt d = sel32x2_1(aa);
   UInt c = sel32x2_0(aa);
   UInt b = sel32x2_1(bb);
   UInt a = sel32x2_0(bb);
   return mk16x4( 
             qnarrow32Sto16S(d),
             qnarrow32Sto16S(c),
             qnarrow32Sto16S(b),
             qnarrow32Sto16S(a)
          );
}

ULong h_generic_calc_QNarrowBin16Sto8Sx8 ( ULong aa, ULong bb )
{
   UShort h = sel16x4_3(aa);
   UShort g = sel16x4_2(aa);
   UShort f = sel16x4_1(aa);
   UShort e = sel16x4_0(aa);
   UShort d = sel16x4_3(bb);
   UShort c = sel16x4_2(bb);
   UShort b = sel16x4_1(bb);
   UShort a = sel16x4_0(bb);
   return mk8x8( 
             qnarrow16Sto8S(h),
             qnarrow16Sto8S(g),
             qnarrow16Sto8S(f),
             qnarrow16Sto8S(e),
             qnarrow16Sto8S(d),
             qnarrow16Sto8S(c),
             qnarrow16Sto8S(b),
             qnarrow16Sto8S(a)
          );
}

ULong h_generic_calc_QNarrowBin16Sto8Ux8 ( ULong aa, ULong bb )
{
   UShort h = sel16x4_3(aa);
   UShort g = sel16x4_2(aa);
   UShort f = sel16x4_1(aa);
   UShort e = sel16x4_0(aa);
   UShort d = sel16x4_3(bb);
   UShort c = sel16x4_2(bb);
   UShort b = sel16x4_1(bb);
   UShort a = sel16x4_0(bb);
   return mk8x8( 
             qnarrow16Sto8U(h),
             qnarrow16Sto8U(g),
             qnarrow16Sto8U(f),
             qnarrow16Sto8U(e),
             qnarrow16Sto8U(d),
             qnarrow16Sto8U(c),
             qnarrow16Sto8U(b),
             qnarrow16Sto8U(a)
          );
}

/* ------------ Truncating narrowing ------------ */

ULong h_generic_calc_NarrowBin32to16x4 ( ULong aa, ULong bb )
{
   UInt d = sel32x2_1(aa);
   UInt c = sel32x2_0(aa);
   UInt b = sel32x2_1(bb);
   UInt a = sel32x2_0(bb);
   return mk16x4( 
             narrow32to16(d),
             narrow32to16(c),
             narrow32to16(b),
             narrow32to16(a)
          );
}

ULong h_generic_calc_NarrowBin16to8x8 ( ULong aa, ULong bb )
{
   UShort h = sel16x4_3(aa);
   UShort g = sel16x4_2(aa);
   UShort f = sel16x4_1(aa);
   UShort e = sel16x4_0(aa);
   UShort d = sel16x4_3(bb);
   UShort c = sel16x4_2(bb);
   UShort b = sel16x4_1(bb);
   UShort a = sel16x4_0(bb);
   return mk8x8( 
             narrow16to8(h),
             narrow16to8(g),
             narrow16to8(f),
             narrow16to8(e),
             narrow16to8(d),
             narrow16to8(c),
             narrow16to8(b),
             narrow16to8(a)
          );
}

/* ------------ Interleaving ------------ */

ULong h_generic_calc_InterleaveHI8x8 ( ULong aa, ULong bb )
{
   return mk8x8(
             sel8x8_7(aa),
             sel8x8_7(bb),
             sel8x8_6(aa),
             sel8x8_6(bb),
             sel8x8_5(aa),
             sel8x8_5(bb),
             sel8x8_4(aa),
             sel8x8_4(bb)
          );
}

ULong h_generic_calc_InterleaveLO8x8 ( ULong aa, ULong bb )
{
   return mk8x8(
             sel8x8_3(aa),
             sel8x8_3(bb),
             sel8x8_2(aa),
             sel8x8_2(bb),
             sel8x8_1(aa),
             sel8x8_1(bb),
             sel8x8_0(aa),
             sel8x8_0(bb)
          );
}

ULong h_generic_calc_InterleaveHI16x4 ( ULong aa, ULong bb )
{
   return mk16x4(
             sel16x4_3(aa),
             sel16x4_3(bb),
             sel16x4_2(aa),
             sel16x4_2(bb)
          );
}

ULong h_generic_calc_InterleaveLO16x4 ( ULong aa, ULong bb )
{
   return mk16x4(
             sel16x4_1(aa),
             sel16x4_1(bb),
             sel16x4_0(aa),
             sel16x4_0(bb)
          );
}

ULong h_generic_calc_InterleaveHI32x2 ( ULong aa, ULong bb )
{
   return mk32x2(
             sel32x2_1(aa),
             sel32x2_1(bb)
          );
}

ULong h_generic_calc_InterleaveLO32x2 ( ULong aa, ULong bb )
{
   return mk32x2(
             sel32x2_0(aa),
             sel32x2_0(bb)
          );
}

/* ------------ Concatenation ------------ */

ULong h_generic_calc_CatOddLanes16x4 ( ULong aa, ULong bb )
{
   return mk16x4(
             sel16x4_3(aa),
             sel16x4_1(aa),
             sel16x4_3(bb),
             sel16x4_1(bb)
          );
}

ULong h_generic_calc_CatEvenLanes16x4 ( ULong aa, ULong bb )
{
   return mk16x4(
             sel16x4_2(aa),
             sel16x4_0(aa),
             sel16x4_2(bb),
             sel16x4_0(bb)
          );
}

/* misc hack looking for a proper home */
ULong h_generic_calc_Perm8x8 ( ULong aa, ULong bb )
{
   return mk8x8(
             index8x8(aa, sel8x8_7(bb)),
             index8x8(aa, sel8x8_6(bb)),
             index8x8(aa, sel8x8_5(bb)),
             index8x8(aa, sel8x8_4(bb)),
             index8x8(aa, sel8x8_3(bb)),
             index8x8(aa, sel8x8_2(bb)),
             index8x8(aa, sel8x8_1(bb)),
             index8x8(aa, sel8x8_0(bb))
          );
}

/* ------------ Shifting ------------ */
/* Note that because these primops are undefined if the shift amount
   equals or exceeds the lane width, the shift amount is masked so
   that the scalar shifts are always in range.  In fact, given the
   semantics of these primops (ShlN16x4, etc) it is an error if in
   fact we are ever given an out-of-range shift amount. 
*/
ULong h_generic_calc_ShlN32x2 ( ULong xx, UInt nn )
{
   /* vassert(nn < 32); */
   nn &= 31;
   return mk32x2(
             shl32( sel32x2_1(xx), nn ),
             shl32( sel32x2_0(xx), nn )
          );
}

ULong h_generic_calc_ShlN16x4 ( ULong xx, UInt nn )
{
   /* vassert(nn < 16); */
   nn &= 15;
   return mk16x4(
             shl16( sel16x4_3(xx), nn ),
             shl16( sel16x4_2(xx), nn ),
             shl16( sel16x4_1(xx), nn ),
             shl16( sel16x4_0(xx), nn )
          );
}

ULong h_generic_calc_ShlN8x8  ( ULong xx, UInt nn )
{
   /* vassert(nn < 8); */
   nn &= 7;
   return mk8x8(
             shl8( sel8x8_7(xx), nn ),
             shl8( sel8x8_6(xx), nn ),
             shl8( sel8x8_5(xx), nn ),
             shl8( sel8x8_4(xx), nn ),
             shl8( sel8x8_3(xx), nn ),
             shl8( sel8x8_2(xx), nn ),
             shl8( sel8x8_1(xx), nn ),
             shl8( sel8x8_0(xx), nn )
          );
}

ULong h_generic_calc_ShrN32x2 ( ULong xx, UInt nn )
{
   /* vassert(nn < 32); */
   nn &= 31;
   return mk32x2(
             shr32( sel32x2_1(xx), nn ),
             shr32( sel32x2_0(xx), nn )
          );
}

ULong h_generic_calc_ShrN16x4 ( ULong xx, UInt nn )
{
   /* vassert(nn < 16); */
   nn &= 15;
   return mk16x4(
             shr16( sel16x4_3(xx), nn ),
             shr16( sel16x4_2(xx), nn ),
             shr16( sel16x4_1(xx), nn ),
             shr16( sel16x4_0(xx), nn )
          );
}

ULong h_generic_calc_SarN32x2 ( ULong xx, UInt nn )
{
   /* vassert(nn < 32); */
   nn &= 31;
   return mk32x2(
             sar32( sel32x2_1(xx), nn ),
             sar32( sel32x2_0(xx), nn )
          );
}

ULong h_generic_calc_SarN16x4 ( ULong xx, UInt nn )
{
   /* vassert(nn < 16); */
   nn &= 15;
   return mk16x4(
             sar16( sel16x4_3(xx), nn ),
             sar16( sel16x4_2(xx), nn ),
             sar16( sel16x4_1(xx), nn ),
             sar16( sel16x4_0(xx), nn )
          );
}

ULong h_generic_calc_SarN8x8 ( ULong xx, UInt nn )
{
   /* vassert(nn < 8); */
   nn &= 7;
   return mk8x8(
             sar8( sel8x8_7(xx), nn ),
             sar8( sel8x8_6(xx), nn ),
             sar8( sel8x8_5(xx), nn ),
             sar8( sel8x8_4(xx), nn ),
             sar8( sel8x8_3(xx), nn ),
             sar8( sel8x8_2(xx), nn ),
             sar8( sel8x8_1(xx), nn ),
             sar8( sel8x8_0(xx), nn )
          );
}

/* ------------ Averaging ------------ */

ULong h_generic_calc_Avg8Ux8 ( ULong xx, ULong yy )
{
   return mk8x8(
             avg8U( sel8x8_7(xx), sel8x8_7(yy) ),
             avg8U( sel8x8_6(xx), sel8x8_6(yy) ),
             avg8U( sel8x8_5(xx), sel8x8_5(yy) ),
             avg8U( sel8x8_4(xx), sel8x8_4(yy) ),
             avg8U( sel8x8_3(xx), sel8x8_3(yy) ),
             avg8U( sel8x8_2(xx), sel8x8_2(yy) ),
             avg8U( sel8x8_1(xx), sel8x8_1(yy) ),
             avg8U( sel8x8_0(xx), sel8x8_0(yy) )
          );
}

ULong h_generic_calc_Avg16Ux4 ( ULong xx, ULong yy )
{
   return mk16x4(
             avg16U( sel16x4_3(xx), sel16x4_3(yy) ),
             avg16U( sel16x4_2(xx), sel16x4_2(yy) ),
             avg16U( sel16x4_1(xx), sel16x4_1(yy) ),
             avg16U( sel16x4_0(xx), sel16x4_0(yy) )
          );
}

/* ------------ max/min ------------ */

ULong h_generic_calc_Max16Sx4 ( ULong xx, ULong yy )
{
   return mk16x4(
             max16S( sel16x4_3(xx), sel16x4_3(yy) ),
             max16S( sel16x4_2(xx), sel16x4_2(yy) ),
             max16S( sel16x4_1(xx), sel16x4_1(yy) ),
             max16S( sel16x4_0(xx), sel16x4_0(yy) )
          );
}

ULong h_generic_calc_Max8Ux8 ( ULong xx, ULong yy )
{
   return mk8x8(
             max8U( sel8x8_7(xx), sel8x8_7(yy) ),
             max8U( sel8x8_6(xx), sel8x8_6(yy) ),
             max8U( sel8x8_5(xx), sel8x8_5(yy) ),
             max8U( sel8x8_4(xx), sel8x8_4(yy) ),
             max8U( sel8x8_3(xx), sel8x8_3(yy) ),
             max8U( sel8x8_2(xx), sel8x8_2(yy) ),
             max8U( sel8x8_1(xx), sel8x8_1(yy) ),
             max8U( sel8x8_0(xx), sel8x8_0(yy) )
          );
}

ULong h_generic_calc_Min16Sx4 ( ULong xx, ULong yy )
{
   return mk16x4(
             min16S( sel16x4_3(xx), sel16x4_3(yy) ),
             min16S( sel16x4_2(xx), sel16x4_2(yy) ),
             min16S( sel16x4_1(xx), sel16x4_1(yy) ),
             min16S( sel16x4_0(xx), sel16x4_0(yy) )
          );
}

ULong h_generic_calc_Min8Ux8 ( ULong xx, ULong yy )
{
   return mk8x8(
             min8U( sel8x8_7(xx), sel8x8_7(yy) ),
             min8U( sel8x8_6(xx), sel8x8_6(yy) ),
             min8U( sel8x8_5(xx), sel8x8_5(yy) ),
             min8U( sel8x8_4(xx), sel8x8_4(yy) ),
             min8U( sel8x8_3(xx), sel8x8_3(yy) ),
             min8U( sel8x8_2(xx), sel8x8_2(yy) ),
             min8U( sel8x8_1(xx), sel8x8_1(yy) ),
             min8U( sel8x8_0(xx), sel8x8_0(yy) )
          );
}

/* ------------ SOME 32-bit SIMD HELPERS TOO ------------ */

/* Tuple/select functions for 16x2 vectors. */
static inline UInt mk16x2 ( UShort w1, UShort w2 ) {
   return (((UInt)w1) << 16) | ((UInt)w2);
}

static inline UShort sel16x2_1 ( UInt w32 ) {
   return 0xFFFF & (UShort)(w32 >> 16);
}
static inline UShort sel16x2_0 ( UInt w32 ) {
   return 0xFFFF & (UShort)(w32);
}

static inline UInt mk8x4 ( UChar w3, UChar w2,
                           UChar w1, UChar w0 ) {
   UInt w32 =   (((UInt)w3) << 24) | (((UInt)w2) << 16)
              | (((UInt)w1) << 8)  | (((UInt)w0) << 0);
   return w32;
}

static inline UChar sel8x4_3 ( UInt w32 ) {
   return toUChar(0xFF & (w32 >> 24));
}
static inline UChar sel8x4_2 ( UInt w32 ) {
   return toUChar(0xFF & (w32 >> 16));
}
static inline UChar sel8x4_1 ( UInt w32 ) {
   return toUChar(0xFF & (w32 >> 8));
}
static inline UChar sel8x4_0 ( UInt w32 ) {
   return toUChar(0xFF & (w32 >> 0));
}


/* ----------------------------------------------------- */
/* More externally visible functions.  These simply
   implement the corresponding IR primops. */
/* ----------------------------------------------------- */

/* ------ 16x2 ------ */

UInt h_generic_calc_Add16x2 ( UInt xx, UInt yy )
{
   return mk16x2( sel16x2_1(xx) + sel16x2_1(yy),
                  sel16x2_0(xx) + sel16x2_0(yy) );
}

UInt h_generic_calc_Sub16x2 ( UInt xx, UInt yy )
{
   return mk16x2( sel16x2_1(xx) - sel16x2_1(yy),
                  sel16x2_0(xx) - sel16x2_0(yy) );
}

UInt h_generic_calc_HAdd16Ux2 ( UInt xx, UInt yy )
{
   return mk16x2( hadd16U( sel16x2_1(xx), sel16x2_1(yy) ),
                  hadd16U( sel16x2_0(xx), sel16x2_0(yy) ) );
}

UInt h_generic_calc_HAdd16Sx2 ( UInt xx, UInt yy )
{
   return mk16x2( hadd16S( sel16x2_1(xx), sel16x2_1(yy) ),
                  hadd16S( sel16x2_0(xx), sel16x2_0(yy) ) );
}

UInt h_generic_calc_HSub16Ux2 ( UInt xx, UInt yy )
{
   return mk16x2( hsub16U( sel16x2_1(xx), sel16x2_1(yy) ),
                  hsub16U( sel16x2_0(xx), sel16x2_0(yy) ) );
}

UInt h_generic_calc_HSub16Sx2 ( UInt xx, UInt yy )
{
   return mk16x2( hsub16S( sel16x2_1(xx), sel16x2_1(yy) ),
                  hsub16S( sel16x2_0(xx), sel16x2_0(yy) ) );
}

UInt h_generic_calc_QAdd16Ux2 ( UInt xx, UInt yy )
{
   return mk16x2( qadd16U( sel16x2_1(xx), sel16x2_1(yy) ),
                  qadd16U( sel16x2_0(xx), sel16x2_0(yy) ) );
}

UInt h_generic_calc_QAdd16Sx2 ( UInt xx, UInt yy )
{
   return mk16x2( qadd16S( sel16x2_1(xx), sel16x2_1(yy) ),
                  qadd16S( sel16x2_0(xx), sel16x2_0(yy) ) );
}

UInt h_generic_calc_QSub16Ux2 ( UInt xx, UInt yy )
{
   return mk16x2( qsub16U( sel16x2_1(xx), sel16x2_1(yy) ),
                  qsub16U( sel16x2_0(xx), sel16x2_0(yy) ) );
}

UInt h_generic_calc_QSub16Sx2 ( UInt xx, UInt yy )
{
   return mk16x2( qsub16S( sel16x2_1(xx), sel16x2_1(yy) ),
                  qsub16S( sel16x2_0(xx), sel16x2_0(yy) ) );
}

/* ------ 8x4 ------ */

UInt h_generic_calc_Add8x4 ( UInt xx, UInt yy )
{
   return mk8x4(
             sel8x4_3(xx) + sel8x4_3(yy),
             sel8x4_2(xx) + sel8x4_2(yy),
             sel8x4_1(xx) + sel8x4_1(yy),
             sel8x4_0(xx) + sel8x4_0(yy)
          );
}

UInt h_generic_calc_Sub8x4 ( UInt xx, UInt yy )
{
   return mk8x4(
             sel8x4_3(xx) - sel8x4_3(yy),
             sel8x4_2(xx) - sel8x4_2(yy),
             sel8x4_1(xx) - sel8x4_1(yy),
             sel8x4_0(xx) - sel8x4_0(yy)
          );
}

UInt h_generic_calc_HAdd8Ux4 ( UInt xx, UInt yy )
{
   return mk8x4(
             hadd8U( sel8x4_3(xx), sel8x4_3(yy) ),
             hadd8U( sel8x4_2(xx), sel8x4_2(yy) ),
             hadd8U( sel8x4_1(xx), sel8x4_1(yy) ),
             hadd8U( sel8x4_0(xx), sel8x4_0(yy) )
          );
}

UInt h_generic_calc_HAdd8Sx4 ( UInt xx, UInt yy )
{
   return mk8x4(
             hadd8S( sel8x4_3(xx), sel8x4_3(yy) ),
             hadd8S( sel8x4_2(xx), sel8x4_2(yy) ),
             hadd8S( sel8x4_1(xx), sel8x4_1(yy) ),
             hadd8S( sel8x4_0(xx), sel8x4_0(yy) )
          );
}

UInt h_generic_calc_HSub8Ux4 ( UInt xx, UInt yy )
{
   return mk8x4(
             hsub8U( sel8x4_3(xx), sel8x4_3(yy) ),
             hsub8U( sel8x4_2(xx), sel8x4_2(yy) ),
             hsub8U( sel8x4_1(xx), sel8x4_1(yy) ),
             hsub8U( sel8x4_0(xx), sel8x4_0(yy) )
          );
}

UInt h_generic_calc_HSub8Sx4 ( UInt xx, UInt yy )
{
   return mk8x4(
             hsub8S( sel8x4_3(xx), sel8x4_3(yy) ),
             hsub8S( sel8x4_2(xx), sel8x4_2(yy) ),
             hsub8S( sel8x4_1(xx), sel8x4_1(yy) ),
             hsub8S( sel8x4_0(xx), sel8x4_0(yy) )
          );
}

UInt h_generic_calc_QAdd8Ux4 ( UInt xx, UInt yy )
{
   return mk8x4(
             qadd8U( sel8x4_3(xx), sel8x4_3(yy) ),
             qadd8U( sel8x4_2(xx), sel8x4_2(yy) ),
             qadd8U( sel8x4_1(xx), sel8x4_1(yy) ),
             qadd8U( sel8x4_0(xx), sel8x4_0(yy) )
          );
}

UInt h_generic_calc_QAdd8Sx4 ( UInt xx, UInt yy )
{
   return mk8x4(
             qadd8S( sel8x4_3(xx), sel8x4_3(yy) ),
             qadd8S( sel8x4_2(xx), sel8x4_2(yy) ),
             qadd8S( sel8x4_1(xx), sel8x4_1(yy) ),
             qadd8S( sel8x4_0(xx), sel8x4_0(yy) )
          );
}

UInt h_generic_calc_QSub8Ux4 ( UInt xx, UInt yy )
{
   return mk8x4(
             qsub8U( sel8x4_3(xx), sel8x4_3(yy) ),
             qsub8U( sel8x4_2(xx), sel8x4_2(yy) ),
             qsub8U( sel8x4_1(xx), sel8x4_1(yy) ),
             qsub8U( sel8x4_0(xx), sel8x4_0(yy) )
          );
}

UInt h_generic_calc_QSub8Sx4 ( UInt xx, UInt yy )
{
   return mk8x4(
             qsub8S( sel8x4_3(xx), sel8x4_3(yy) ),
             qsub8S( sel8x4_2(xx), sel8x4_2(yy) ),
             qsub8S( sel8x4_1(xx), sel8x4_1(yy) ),
             qsub8S( sel8x4_0(xx), sel8x4_0(yy) )
          );
}

UInt h_generic_calc_CmpNEZ16x2 ( UInt xx )
{
   return mk16x2(
             cmpnez16( sel16x2_1(xx) ),
             cmpnez16( sel16x2_0(xx) )
          );
}

UInt h_generic_calc_CmpNEZ8x4 ( UInt xx )
{
   return mk8x4(
             cmpnez8( sel8x4_3(xx) ),
             cmpnez8( sel8x4_2(xx) ),
             cmpnez8( sel8x4_1(xx) ),
             cmpnez8( sel8x4_0(xx) )
          );
}

UInt h_generic_calc_Sad8Ux4 ( UInt xx, UInt yy )
{
   return absdiff8U( sel8x4_3(xx), sel8x4_3(yy) )
          + absdiff8U( sel8x4_2(xx), sel8x4_2(yy) )
          + absdiff8U( sel8x4_1(xx), sel8x4_1(yy) )
          + absdiff8U( sel8x4_0(xx), sel8x4_0(yy) );
}


/*---------------------------------------------------------------*/
/*--- end                               host_generic_simd64.c ---*/
/*---------------------------------------------------------------*/
