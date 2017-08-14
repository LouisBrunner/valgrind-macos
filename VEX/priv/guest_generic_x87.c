
/*---------------------------------------------------------------*/
/*--- begin                               guest_generic_x87.c ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2004-2013 OpenWorks LLP
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

/* This file contains functions for doing some x87-specific
   operations.  Both the amd64 and x86 front ends (guests) indirectly
   call these functions via guest helper calls.  By putting them here,
   code duplication is avoided.  Some of these functions are tricky
   and hard to verify, so there is much to be said for only having one
   copy thereof.
*/

#include "libvex_basictypes.h"

#include "main_util.h"
#include "guest_generic_x87.h"


/* 80 and 64-bit floating point formats:

   80-bit:

    S  0       0-------0      zero
    S  0       0X------X      denormals
    S  1-7FFE  1X------X      normals (all normals have leading 1)
    S  7FFF    10------0      infinity
    S  7FFF    10X-----X      snan
    S  7FFF    11X-----X      qnan

   S is the sign bit.  For runs X----X, at least one of the Xs must be
   nonzero.  Exponent is 15 bits, fractional part is 63 bits, and
   there is an explicitly represented leading 1, and a sign bit,
   giving 80 in total.

   64-bit avoids the confusion of an explicitly represented leading 1
   and so is simpler:

    S  0      0------0   zero
    S  0      X------X   denormals
    S  1-7FE  any        normals
    S  7FF    0------0   infinity
    S  7FF    0X-----X   snan
    S  7FF    1X-----X   qnan

   Exponent is 11 bits, fractional part is 52 bits, and there is a 
   sign bit, giving 64 in total.
*/


static inline UInt read_bit_array ( UChar* arr, UInt n )
{
   UChar c = arr[n >> 3];
   c >>= (n&7);
   return c & 1;
}

static inline void write_bit_array ( UChar* arr, UInt n, UInt b )
{
   UChar c = arr[n >> 3];
   c = toUChar( c & ~(1 << (n&7)) );
   c = toUChar( c | ((b&1) << (n&7)) );
   arr[n >> 3] = c;
}

/* Convert an IEEE754 double (64-bit) into an x87 extended double
   (80-bit), mimicing the hardware fairly closely.  Both numbers are
   stored little-endian.  Limitations, all of which could be fixed,
   given some level of hassle:

   * Identity of NaNs is not preserved.

   See comments in the code for more details.
*/
void convert_f64le_to_f80le ( /*IN*/UChar* f64, /*OUT*/UChar* f80 )
{
   Bool  mantissaIsZero;
   Int   bexp, i, j, shift;
   UChar sign;

   sign = toUChar( (f64[7] >> 7) & 1 );
   bexp = (f64[7] << 4) | ((f64[6] >> 4) & 0x0F);
   bexp &= 0x7FF;

   mantissaIsZero = False;
   if (bexp == 0 || bexp == 0x7FF) {
      /* We'll need to know whether or not the mantissa (bits 51:0) is
         all zeroes in order to handle these cases.  So figure it
         out. */
      mantissaIsZero
         = toBool( 
              (f64[6] & 0x0F) == 0 
              && f64[5] == 0 && f64[4] == 0 && f64[3] == 0 
              && f64[2] == 0 && f64[1] == 0 && f64[0] == 0
           );
   }

   /* If the exponent is zero, either we have a zero or a denormal.
      Produce a zero.  This is a hack in that it forces denormals to
      zero.  Could do better. */
   if (bexp == 0) {
      f80[9] = toUChar( sign << 7 );
      f80[8] = f80[7] = f80[6] = f80[5] = f80[4]
             = f80[3] = f80[2] = f80[1] = f80[0] = 0;

      if (mantissaIsZero)
         /* It really is zero, so that's all we can do. */
         return;

      /* There is at least one 1-bit in the mantissa.  So it's a
         potentially denormalised double -- but we can produce a
         normalised long double.  Count the leading zeroes in the
         mantissa so as to decide how much to bump the exponent down
         by.  Note, this is SLOW. */
      shift = 0;
      for (i = 51; i >= 0; i--) {
        if (read_bit_array(f64, i))
           break;
        shift++;
      }

      /* and copy into place as many bits as we can get our hands on. */
      j = 63;
      for (i = 51 - shift; i >= 0; i--) {
         write_bit_array( f80, j,
     	 read_bit_array( f64, i ) );
         j--;
      }

      /* Set the exponent appropriately, and we're done. */
      bexp -= shift;
      bexp += (16383 - 1023);
      f80[9] = toUChar( (sign << 7) | ((bexp >> 8) & 0xFF) );
      f80[8] = toUChar( bexp & 0xFF );
      return;
   }

   /* If the exponent is 7FF, this is either an Infinity, a SNaN or
      QNaN, as determined by examining bits 51:0, thus:
          0  ... 0    Inf
          0X ... X    SNaN
          1X ... X    QNaN
      where at least one of the Xs is not zero.
   */
   if (bexp == 0x7FF) {
      if (mantissaIsZero) {
         /* Produce an appropriately signed infinity:
            S 1--1 (15)  1  0--0 (63)
         */
         f80[9] = toUChar( (sign << 7) | 0x7F );
         f80[8] = 0xFF;
         f80[7] = 0x80;
         f80[6] = f80[5] = f80[4] = f80[3] 
                = f80[2] = f80[1] = f80[0] = 0;
         return;
      }
      /* So it's either a QNaN or SNaN.  Distinguish by considering
         bit 51.  Note, this destroys all the trailing bits
         (identity?) of the NaN.  IEEE754 doesn't require preserving
         these (it only requires that there be one QNaN value and one
         SNaN value), but x87 does seem to have some ability to
         preserve them.  Anyway, here, the NaN's identity is
         destroyed.  Could be improved. */
      if (f64[6] & 8) {
         /* QNaN.  Make a canonical QNaN:
            S 1--1 (15)  1 1  0--0 (62) 
         */
         f80[9] = toUChar( (sign << 7) | 0x7F );
         f80[8] = 0xFF;
         f80[7] = 0xC0;
         f80[6] = f80[5] = f80[4] = f80[3] 
                = f80[2] = f80[1] = f80[0] = 0x00;
      } else {
         /* SNaN.  Make a SNaN:
            S 1--1 (15)  1 0  1--1 (62) 
         */
         f80[9] = toUChar( (sign << 7) | 0x7F );
         f80[8] = 0xFF;
         f80[7] = 0xBF;
         f80[6] = f80[5] = f80[4] = f80[3] 
                = f80[2] = f80[1] = f80[0] = 0xFF;
      }
      return;
   }

   /* It's not a zero, denormal, infinity or nan.  So it must be a
      normalised number.  Rebias the exponent and build the new
      number.  */
   bexp += (16383 - 1023);

   f80[9] = toUChar( (sign << 7) | ((bexp >> 8) & 0xFF) );
   f80[8] = toUChar( bexp & 0xFF );
   f80[7] = toUChar( (1 << 7) | ((f64[6] << 3) & 0x78) 
                              | ((f64[5] >> 5) & 7) );
   f80[6] = toUChar( ((f64[5] << 3) & 0xF8) | ((f64[4] >> 5) & 7) );
   f80[5] = toUChar( ((f64[4] << 3) & 0xF8) | ((f64[3] >> 5) & 7) );
   f80[4] = toUChar( ((f64[3] << 3) & 0xF8) | ((f64[2] >> 5) & 7) );
   f80[3] = toUChar( ((f64[2] << 3) & 0xF8) | ((f64[1] >> 5) & 7) );
   f80[2] = toUChar( ((f64[1] << 3) & 0xF8) | ((f64[0] >> 5) & 7) );
   f80[1] = toUChar( ((f64[0] << 3) & 0xF8) );
   f80[0] = toUChar( 0 );
}


/* Convert an x87 extended double (80-bit) into an IEEE 754 double
   (64-bit), mimicking the hardware fairly closely.  Both numbers are
   stored little-endian.  Limitations, both of which could be fixed,
   given some level of hassle:

   * Rounding following truncation could be a bit better.

   * Identity of NaNs is not preserved.

   See comments in the code for more details.
*/
void convert_f80le_to_f64le ( /*IN*/UChar* f80, /*OUT*/UChar* f64 )
{
   Bool  isInf;
   Int   bexp, i, j;
   UChar sign;

   sign = toUChar((f80[9] >> 7) & 1);
   bexp = (((UInt)f80[9]) << 8) | (UInt)f80[8];
   bexp &= 0x7FFF;

   /* If the exponent is zero, either we have a zero or a denormal.
      But an extended precision denormal becomes a double precision
      zero, so in either case, just produce the appropriately signed
      zero. */
   if (bexp == 0) {
      f64[7] = toUChar(sign << 7);
      f64[6] = f64[5] = f64[4] = f64[3] = f64[2] = f64[1] = f64[0] = 0;
      return;
   }
   
   /* If the exponent is 7FFF, this is either an Infinity, a SNaN or
      QNaN, as determined by examining bits 62:0, thus:
          10  ... 0    Inf
          10X ... X    SNaN
          11X ... X    QNaN
      where at least one of the Xs is not zero.
   */
   if (bexp == 0x7FFF) {
      isInf = toBool(
                 (f80[7] & 0x7F) == 0 
                 && f80[6] == 0 && f80[5] == 0 && f80[4] == 0 
                 && f80[3] == 0 && f80[2] == 0 && f80[1] == 0 
                 && f80[0] == 0
              );
      if (isInf) {
         if (0 == (f80[7] & 0x80))
            goto wierd_NaN;
         /* Produce an appropriately signed infinity:
            S 1--1 (11)  0--0 (52)
         */
         f64[7] = toUChar((sign << 7) | 0x7F);
         f64[6] = 0xF0;
         f64[5] = f64[4] = f64[3] = f64[2] = f64[1] = f64[0] = 0;
         return;
      }
      /* So it's either a QNaN or SNaN.  Distinguish by considering
         bit 61.  Note, this destroys all the trailing bits
         (identity?) of the NaN.  IEEE754 doesn't require preserving
         these (it only requires that there be one QNaN value and one
         SNaN value), but x87 does seem to have some ability to
         preserve them.  Anyway, here, the NaN's identity is
         destroyed.  Could be improved. */
      if (f80[7] & 0x40) {
         /* QNaN.  Make a canonical QNaN:
            S 1--1 (11)  1  0--0 (51) 
         */
         f64[7] = toUChar((sign << 7) | 0x7F);
         f64[6] = 0xF8;
         f64[5] = f64[4] = f64[3] = f64[2] = f64[1] = f64[0] = 0x00;
      } else {
         /* SNaN.  Make a SNaN:
            S 1--1 (11)  0  1--1 (51) 
         */
         f64[7] = toUChar((sign << 7) | 0x7F);
         f64[6] = 0xF7;
         f64[5] = f64[4] = f64[3] = f64[2] = f64[1] = f64[0] = 0xFF;
      }
      return;
   }

   /* If it's not a Zero, NaN or Inf, and the integer part (bit 62) is
      zero, the x87 FPU appears to consider the number denormalised
      and converts it to a QNaN. */
   if (0 == (f80[7] & 0x80)) {
      wierd_NaN:
      /* Strange hardware QNaN:
         S 1--1 (11)  1  0--0 (51) 
      */
      /* On a PIII, these QNaNs always appear with sign==1.  I have
         no idea why. */
      f64[7] = (1 /*sign*/ << 7) | 0x7F;
      f64[6] = 0xF8;
      f64[5] = f64[4] = f64[3] = f64[2] = f64[1] = f64[0] = 0;
      return;
   }

   /* It's not a zero, denormal, infinity or nan.  So it must be a 
      normalised number.  Rebias the exponent and consider. */
   bexp -= (16383 - 1023);
   if (bexp >= 0x7FF) {
      /* It's too big for a double.  Construct an infinity. */
      f64[7] = toUChar((sign << 7) | 0x7F);
      f64[6] = 0xF0;
      f64[5] = f64[4] = f64[3] = f64[2] = f64[1] = f64[0] = 0;
      return;
   }

   if (bexp <= 0) {
      /* It's too small for a normalised double.  First construct a
         zero and then see if it can be improved into a denormal.  */
      f64[7] = toUChar(sign << 7);
      f64[6] = f64[5] = f64[4] = f64[3] = f64[2] = f64[1] = f64[0] = 0;

      if (bexp < -52)
         /* Too small even for a denormal. */
         return;

      /* Ok, let's make a denormal.  Note, this is SLOW. */
      /* Copy bits 63, 62, 61, etc of the src mantissa into the dst, 
         indexes 52+bexp, 51+bexp, etc, until k+bexp < 0. */
      /* bexp is in range -52 .. 0 inclusive */
      for (i = 63; i >= 0; i--) {
         j = i - 12 + bexp;
         if (j < 0) break;
         /* We shouldn't really call vassert from generated code. */
         vassert(j >= 0 && j < 52);
         write_bit_array ( f64,
                           j,
                           read_bit_array ( f80, i ) );
      }
      /* and now we might have to round ... */
      if (read_bit_array(f80, 10+1 - bexp) == 1) 
         goto do_rounding;

      return;
   }

   /* Ok, it's a normalised number which is representable as a double.
      Copy the exponent and mantissa into place. */
   /*
   for (i = 0; i < 52; i++)
      write_bit_array ( f64,
                        i,
                        read_bit_array ( f80, i+11 ) );
   */
   f64[0] = toUChar( (f80[1] >> 3) | (f80[2] << 5) );
   f64[1] = toUChar( (f80[2] >> 3) | (f80[3] << 5) );
   f64[2] = toUChar( (f80[3] >> 3) | (f80[4] << 5) );
   f64[3] = toUChar( (f80[4] >> 3) | (f80[5] << 5) );
   f64[4] = toUChar( (f80[5] >> 3) | (f80[6] << 5) );
   f64[5] = toUChar( (f80[6] >> 3) | (f80[7] << 5) );

   f64[6] = toUChar( ((bexp << 4) & 0xF0) | ((f80[7] >> 3) & 0x0F) );

   f64[7] = toUChar( (sign << 7) | ((bexp >> 4) & 0x7F) );

   /* Now consider any rounding that needs to happen as a result of
      truncating the mantissa. */
   if (f80[1] & 4) /* read_bit_array(f80, 10) == 1) */ {

      /* If the bottom bits of f80 are "100 0000 0000", then the
         infinitely precise value is deemed to be mid-way between the
         two closest representable values.  Since we're doing
         round-to-nearest (the default mode), in that case it is the
         bit immediately above which indicates whether we should round
         upwards or not -- if 0, we don't.  All that is encapsulated
         in the following simple test. */
      if ((f80[1] & 0xF) == 4/*0100b*/ && f80[0] == 0)
         return;

      do_rounding:
      /* Round upwards.  This is a kludge.  Once in every 2^24
         roundings (statistically) the bottom three bytes are all 0xFF
         and so we don't round at all.  Could be improved. */
      if (f64[0] != 0xFF) { 
         f64[0]++; 
      }
      else 
      if (f64[0] == 0xFF && f64[1] != 0xFF) {
         f64[0] = 0;
         f64[1]++;
      }
      else      
      if (f64[0] == 0xFF && f64[1] == 0xFF && f64[2] != 0xFF) {
         f64[0] = 0;
         f64[1] = 0;
         f64[2]++;
      }
      /* else we don't round, but we should. */
   }
}


/* CALLED FROM GENERATED CODE: CLEAN HELPER */
/* Extract the signed significand or exponent component as per
   fxtract.  Arg and result are doubles travelling under the guise of
   ULongs.  Returns significand when getExp is zero and exponent
   otherwise. */
ULong x86amd64g_calculate_FXTRACT ( ULong arg, HWord getExp )
{
   ULong  uSig, uExp;
   /* Long   sSig; */
   Int    sExp, i;
   UInt   sign, expExp;

   /*
    S  7FF    0------0   infinity
    S  7FF    0X-----X   snan
    S  7FF    1X-----X   qnan
   */
   const ULong posInf  = 0x7FF0000000000000ULL;
   const ULong negInf  = 0xFFF0000000000000ULL;
   const ULong nanMask = 0x7FF0000000000000ULL;
   const ULong qNan    = 0x7FF8000000000000ULL;
   const ULong posZero = 0x0000000000000000ULL;
   const ULong negZero = 0x8000000000000000ULL;
   const ULong bit51   = 1ULL << 51;
   const ULong bit52   = 1ULL << 52;
   const ULong sigMask = bit52 - 1;

   /* Mimic Core i5 behaviour for special cases. */
   if (arg == posInf)
      return getExp ? posInf : posInf;
   if (arg == negInf)
      return getExp ? posInf : negInf;
   if ((arg & nanMask) == nanMask)
      return qNan | (arg & (1ULL << 63));
   if (arg == posZero)
      return getExp ? negInf : posZero;
   if (arg == negZero)
      return getExp ? negInf : negZero;

   /* Split into sign, exponent and significand. */
   sign = ((UInt)(arg >> 63)) & 1;

   /* Mask off exponent & sign. uSig is in range 0 .. 2^52-1. */
   uSig = arg & sigMask;

   /* Get the exponent. */
   sExp = ((Int)(arg >> 52)) & 0x7FF;

   /* Deal with denormals: if the exponent is zero, then the
      significand cannot possibly be zero (negZero/posZero are handled
      above).  Shift the significand left until bit 51 of it becomes
      1, and decrease the exponent accordingly.
   */
   if (sExp == 0) {
      for (i = 0; i < 52; i++) {
         if (uSig & bit51)
            break;
         uSig <<= 1;
         sExp--;
      }
      uSig <<= 1;
   } else {
      /* Add the implied leading-1 in the significand. */
      uSig |= bit52;
   }

   /* Roll in the sign. */
   /* sSig = uSig; */
   /* if (sign) sSig =- sSig; */

   /* Convert sig into a double.  This should be an exact conversion.
      Then divide by 2^52, which should give a value in the range 1.0
      to 2.0-epsilon, at least for normalised args. */
   /* dSig = (Double)sSig; */
   /* dSig /= 67108864.0;  */ /* 2^26 */
   /* dSig /= 67108864.0;  */ /* 2^26 */
   uSig &= sigMask;
   uSig |= 0x3FF0000000000000ULL;
   if (sign)
      uSig ^= negZero;

   /* Convert exp into a double.  Also an exact conversion. */
   /* dExp = (Double)(sExp - 1023); */
   sExp -= 1023;
   if (sExp == 0) {
      uExp = 0;
   } else {
      uExp   = sExp < 0 ? -sExp : sExp;
      expExp = 0x3FF +52;
      /* 1 <= uExp <= 1074 */
      /* Skip first 42 iterations of normalisation loop as we know they
         will always happen */
      uExp <<= 42;
      expExp -= 42;
      for (i = 0; i < 52-42; i++) {
         if (uExp & bit52)
            break;
         uExp <<= 1;
         expExp--;
      }
      uExp &= sigMask;
      uExp |= ((ULong)expExp) << 52;
      if (sExp < 0) uExp ^= negZero;
   }

   return getExp ? uExp : uSig;
}



/*---------------------------------------------------------*/
/*--- SSE4.2 PCMP{E,I}STR{I,M} helpers                  ---*/
/*---------------------------------------------------------*/

/* We need the definitions for OSZACP eflags/rflags offsets.
   #including guest_{amd64,x86}_defs.h causes chaos, so just copy the
   required values directly.  They are not going to change in the
   foreseeable future :-)
*/

#define SHIFT_O   11
#define SHIFT_S   7
#define SHIFT_Z   6
#define SHIFT_A   4
#define SHIFT_C   0
#define SHIFT_P   2

#define MASK_O    (1 << SHIFT_O)
#define MASK_S    (1 << SHIFT_S)
#define MASK_Z    (1 << SHIFT_Z)
#define MASK_A    (1 << SHIFT_A)
#define MASK_C    (1 << SHIFT_C)
#define MASK_P    (1 << SHIFT_P)


/* Count leading zeroes, w/ 0-produces-32 semantics, a la Hacker's
   Delight. */
static UInt clz32 ( UInt x )
{
   Int y, m, n;
   y = -(x >> 16);
   m = (y >> 16) & 16;
   n = 16 - m;
   x = x >> m;
   y = x - 0x100;
   m = (y >> 16) & 8;
   n = n + m;
   x = x << m;
   y = x - 0x1000;
   m = (y >> 16) & 4;
   n = n + m;
   x = x << m;
   y = x - 0x4000;
   m = (y >> 16) & 2;
   n = n + m;
   x = x << m;
   y = x >> 14;
   m = y & ~(y >> 1);
   return n + 2 - m;
}

static UInt ctz32 ( UInt x )
{
   return 32 - clz32((~x) & (x-1));
}

/* Convert a 4-bit value to a 32-bit value by cloning each bit 8
   times.  There's surely a better way to do this, but I don't know
   what it is. */
static UInt bits4_to_bytes4 ( UInt bits4 )
{
   UInt r = 0;
   r |= (bits4 & 1) ? 0x000000FF : 0;
   r |= (bits4 & 2) ? 0x0000FF00 : 0;
   r |= (bits4 & 4) ? 0x00FF0000 : 0;
   r |= (bits4 & 8) ? 0xFF000000 : 0;
   return r;
}


/* Convert a 2-bit value to a 32-bit value by cloning each bit 16
   times.  There's surely a better way to do this, but I don't know
   what it is. */
static UInt bits2_to_bytes4 ( UInt bits2 )
{
   UInt r = 0;
   r |= (bits2 & 1) ? 0x0000FFFF : 0;
   r |= (bits2 & 2) ? 0xFFFF0000 : 0;
   return r;
}


/* Given partial results from a pcmpXstrX operation (intRes1,
   basically), generate an I- or M-format output value, also the new
   OSZACP flags.  */
static
void compute_PCMPxSTRx_gen_output (/*OUT*/V128* resV,
                                   /*OUT*/UInt* resOSZACP,
                                   UInt intRes1,
                                   UInt zmaskL, UInt zmaskR,
                                   UInt validL,
                                   UInt pol, UInt idx,
                                   Bool isxSTRM )
{
   vassert((pol >> 2) == 0);
   vassert((idx >> 1) == 0);

   UInt intRes2 = 0;
   switch (pol) {
      case 0: intRes2 = intRes1;          break; // pol +
      case 1: intRes2 = ~intRes1;         break; // pol -
      case 2: intRes2 = intRes1;          break; // pol m+
      case 3: intRes2 = intRes1 ^ validL; break; // pol m-
   }
   intRes2 &= 0xFFFF;

   if (isxSTRM) {
 
      // generate M-format output (a bit or byte mask in XMM0)
      if (idx) {
         resV->w32[0] = bits4_to_bytes4( (intRes2 >>  0) & 0xF );
         resV->w32[1] = bits4_to_bytes4( (intRes2 >>  4) & 0xF );
         resV->w32[2] = bits4_to_bytes4( (intRes2 >>  8) & 0xF );
         resV->w32[3] = bits4_to_bytes4( (intRes2 >> 12) & 0xF );
      } else {
         resV->w32[0] = intRes2 & 0xFFFF;
         resV->w32[1] = 0;
         resV->w32[2] = 0;
         resV->w32[3] = 0;
      }

   } else {

      // generate I-format output (an index in ECX)
      // generate ecx value
      UInt newECX = 0;
      if (idx) {
         // index of ms-1-bit
         newECX = intRes2 == 0 ? 16 : (31 - clz32(intRes2));
      } else {
         // index of ls-1-bit
         newECX = intRes2 == 0 ? 16 : ctz32(intRes2);
      }

      resV->w32[0] = newECX;
      resV->w32[1] = 0;
      resV->w32[2] = 0;
      resV->w32[3] = 0;

   }

   // generate new flags, common to all ISTRI and ISTRM cases
   *resOSZACP    // A, P are zero
     = ((intRes2 == 0) ? 0 : MASK_C) // C == 0 iff intRes2 == 0
     | ((zmaskL == 0)  ? 0 : MASK_Z) // Z == 1 iff any in argL is 0
     | ((zmaskR == 0)  ? 0 : MASK_S) // S == 1 iff any in argR is 0
     | ((intRes2 & 1) << SHIFT_O);   // O == IntRes2[0]
}


/* Given partial results from a 16-bit pcmpXstrX operation (intRes1,
   basically), generate an I- or M-format output value, also the new
   OSZACP flags.  */
static
void compute_PCMPxSTRx_gen_output_wide (/*OUT*/V128* resV,
                                        /*OUT*/UInt* resOSZACP,
                                        UInt intRes1,
                                        UInt zmaskL, UInt zmaskR,
                                        UInt validL,
                                        UInt pol, UInt idx,
                                        Bool isxSTRM )
{
   vassert((pol >> 2) == 0);
   vassert((idx >> 1) == 0);

   UInt intRes2 = 0;
   switch (pol) {
      case 0: intRes2 = intRes1;          break; // pol +
      case 1: intRes2 = ~intRes1;         break; // pol -
      case 2: intRes2 = intRes1;          break; // pol m+
      case 3: intRes2 = intRes1 ^ validL; break; // pol m-
   }
   intRes2 &= 0xFF;

   if (isxSTRM) {
 
      // generate M-format output (a bit or byte mask in XMM0)
      if (idx) {
         resV->w32[0] = bits2_to_bytes4( (intRes2 >> 0) & 0x3 );
         resV->w32[1] = bits2_to_bytes4( (intRes2 >> 2) & 0x3 );
         resV->w32[2] = bits2_to_bytes4( (intRes2 >> 4) & 0x3 );
         resV->w32[3] = bits2_to_bytes4( (intRes2 >> 6) & 0x3 );
      } else {
         resV->w32[0] = intRes2 & 0xFF;
         resV->w32[1] = 0;
         resV->w32[2] = 0;
         resV->w32[3] = 0;
      }

   } else {

      // generate I-format output (an index in ECX)
      // generate ecx value
      UInt newECX = 0;
      if (idx) {
         // index of ms-1-bit
         newECX = intRes2 == 0 ? 8 : (31 - clz32(intRes2));
      } else {
         // index of ls-1-bit
         newECX = intRes2 == 0 ? 8 : ctz32(intRes2);
      }

      resV->w32[0] = newECX;
      resV->w32[1] = 0;
      resV->w32[2] = 0;
      resV->w32[3] = 0;

   }

   // generate new flags, common to all ISTRI and ISTRM cases
   *resOSZACP    // A, P are zero
     = ((intRes2 == 0) ? 0 : MASK_C) // C == 0 iff intRes2 == 0
     | ((zmaskL == 0)  ? 0 : MASK_Z) // Z == 1 iff any in argL is 0
     | ((zmaskR == 0)  ? 0 : MASK_S) // S == 1 iff any in argR is 0
     | ((intRes2 & 1) << SHIFT_O);   // O == IntRes2[0]
}


/* Compute result and new OSZACP flags for all PCMP{E,I}STR{I,M}
   variants on 8-bit data.

   For xSTRI variants, the new ECX value is placed in the 32 bits
   pointed to by *resV, and the top 96 bits are zeroed.  For xSTRM
   variants, the result is a 128 bit value and is placed at *resV in
   the obvious way.

   For all variants, the new OSZACP value is placed at *resOSZACP.

   argLV and argRV are the vector args.  The caller must prepare a
   16-bit mask for each, zmaskL and zmaskR.  For ISTRx variants this
   must be 1 for each zero byte of of the respective arg.  For ESTRx
   variants this is derived from the explicit length indication, and
   must be 0 in all places except at the bit index corresponding to
   the valid length (0 .. 16).  If the valid length is 16 then the
   mask must be all zeroes.  In all cases, bits 31:16 must be zero.

   imm8 is the original immediate from the instruction.  isSTRM
   indicates whether this is a xSTRM or xSTRI variant, which controls
   how much of *res is written.

   If the given imm8 case can be handled, the return value is True.
   If not, False is returned, and neither *res not *resOSZACP are
   altered.
*/

Bool compute_PCMPxSTRx ( /*OUT*/V128* resV,
                         /*OUT*/UInt* resOSZACP,
                         V128* argLV,  V128* argRV,
                         UInt zmaskL, UInt zmaskR,
                         UInt imm8,   Bool isxSTRM )
{
   vassert(imm8 < 0x80);
   vassert((zmaskL >> 16) == 0);
   vassert((zmaskR >> 16) == 0);

   /* Explicitly reject any imm8 values that haven't been validated,
      even if they would probably work.  Life is too short to have
      unvalidated cases in the code base. */
   switch (imm8) {
      case 0x00: case 0x02: case 0x08: case 0x0A: case 0x0C: case 0x0E:
      case 0x12: case 0x14: case 0x1A:
      case 0x30: case 0x34: case 0x38: case 0x3A:
      case 0x40: case 0x44: case 0x46: case 0x4A:
         break;
      default:
         return False;
   }

   UInt fmt = (imm8 >> 0) & 3; // imm8[1:0]  data format
   UInt agg = (imm8 >> 2) & 3; // imm8[3:2]  aggregation fn
   UInt pol = (imm8 >> 4) & 3; // imm8[5:4]  polarity
   UInt idx = (imm8 >> 6) & 1; // imm8[6]    1==msb/bytemask

   /*----------------------------------------*/
   /*-- strcmp on byte data                --*/
   /*----------------------------------------*/

   if (agg == 2/*equal each, aka strcmp*/
       && (fmt == 0/*ub*/ || fmt == 2/*sb*/)) {
      Int    i;
      UChar* argL = (UChar*)argLV;
      UChar* argR = (UChar*)argRV;
      UInt boolResII = 0;
      for (i = 15; i >= 0; i--) {
         UChar cL  = argL[i];
         UChar cR  = argR[i];
         boolResII = (boolResII << 1) | (cL == cR ? 1 : 0);
      }
      UInt validL = ~(zmaskL | -zmaskL);  // not(left(zmaskL))
      UInt validR = ~(zmaskR | -zmaskR);  // not(left(zmaskR))

      // do invalidation, common to all equal-each cases
      UInt intRes1
         = (boolResII & validL & validR)  // if both valid, use cmpres
           | (~ (validL | validR));       // if both invalid, force 1
                                          // else force 0
      intRes1 &= 0xFFFF;

      // generate I-format output
      compute_PCMPxSTRx_gen_output(
         resV, resOSZACP,
         intRes1, zmaskL, zmaskR, validL, pol, idx, isxSTRM
      );

      return True;
   }

   /*----------------------------------------*/
   /*-- set membership on byte data        --*/
   /*----------------------------------------*/

   if (agg == 0/*equal any, aka find chars in a set*/
       && (fmt == 0/*ub*/ || fmt == 2/*sb*/)) {
      /* argL: the string,  argR: charset */
      UInt   si, ci;
      UChar* argL    = (UChar*)argLV;
      UChar* argR    = (UChar*)argRV;
      UInt   boolRes = 0;
      UInt   validL  = ~(zmaskL | -zmaskL);  // not(left(zmaskL))
      UInt   validR  = ~(zmaskR | -zmaskR);  // not(left(zmaskR))

      for (si = 0; si < 16; si++) {
         if ((validL & (1 << si)) == 0)
            // run off the end of the string.
            break;
         UInt m = 0;
         for (ci = 0; ci < 16; ci++) {
            if ((validR & (1 << ci)) == 0) break;
            if (argR[ci] == argL[si]) { m = 1; break; }
         }
         boolRes |= (m << si);
      }

      // boolRes is "pre-invalidated"
      UInt intRes1 = boolRes & 0xFFFF;
   
      // generate I-format output
      compute_PCMPxSTRx_gen_output(
         resV, resOSZACP,
         intRes1, zmaskL, zmaskR, validL, pol, idx, isxSTRM
      );

      return True;
   }

   /*----------------------------------------*/
   /*-- substring search on byte data      --*/
   /*----------------------------------------*/

   if (agg == 3/*equal ordered, aka substring search*/
       && (fmt == 0/*ub*/ || fmt == 2/*sb*/)) {

      /* argL: haystack,  argR: needle */
      UInt   ni, hi;
      UChar* argL    = (UChar*)argLV;
      UChar* argR    = (UChar*)argRV;
      UInt   boolRes = 0;
      UInt   validL  = ~(zmaskL | -zmaskL);  // not(left(zmaskL))
      UInt   validR  = ~(zmaskR | -zmaskR);  // not(left(zmaskR))
      for (hi = 0; hi < 16; hi++) {
         UInt m = 1;
         for (ni = 0; ni < 16; ni++) {
            if ((validR & (1 << ni)) == 0) break;
            UInt i = ni + hi;
            if (i >= 16) break;
            if (argL[i] != argR[ni]) { m = 0; break; }
         }
         boolRes |= (m << hi);
         if ((validL & (1 << hi)) == 0)
            // run off the end of the haystack
            break;
      }

      // boolRes is "pre-invalidated"
      UInt intRes1 = boolRes & 0xFFFF;

      // generate I-format output
      compute_PCMPxSTRx_gen_output(
         resV, resOSZACP,
         intRes1, zmaskL, zmaskR, validL, pol, idx, isxSTRM
      );

      return True;
   }

   /*----------------------------------------*/
   /*-- ranges, unsigned byte data         --*/
   /*----------------------------------------*/

   if (agg == 1/*ranges*/
       && fmt == 0/*ub*/) {

      /* argL: string,  argR: range-pairs */
      UInt   ri, si;
      UChar* argL    = (UChar*)argLV;
      UChar* argR    = (UChar*)argRV;
      UInt   boolRes = 0;
      UInt   validL  = ~(zmaskL | -zmaskL);  // not(left(zmaskL))
      UInt   validR  = ~(zmaskR | -zmaskR);  // not(left(zmaskR))
      for (si = 0; si < 16; si++) {
         if ((validL & (1 << si)) == 0)
            // run off the end of the string
            break;
         UInt m = 0;
         for (ri = 0; ri < 16; ri += 2) {
            if ((validR & (3 << ri)) != (3 << ri)) break;
            if (argR[ri] <= argL[si] && argL[si] <= argR[ri+1]) { 
               m = 1; break;
            }
         }
         boolRes |= (m << si);
      }

      // boolRes is "pre-invalidated"
      UInt intRes1 = boolRes & 0xFFFF;

      // generate I-format output
      compute_PCMPxSTRx_gen_output(
         resV, resOSZACP,
         intRes1, zmaskL, zmaskR, validL, pol, idx, isxSTRM
      );

      return True;
   }

   /*----------------------------------------*/
   /*-- ranges, signed byte data           --*/
   /*----------------------------------------*/

   if (agg == 1/*ranges*/
       && fmt == 2/*sb*/) {

      /* argL: string,  argR: range-pairs */
      UInt   ri, si;
      Char*  argL    = (Char*)argLV;
      Char*  argR    = (Char*)argRV;
      UInt   boolRes = 0;
      UInt   validL  = ~(zmaskL | -zmaskL);  // not(left(zmaskL))
      UInt   validR  = ~(zmaskR | -zmaskR);  // not(left(zmaskR))
      for (si = 0; si < 16; si++) {
         if ((validL & (1 << si)) == 0)
            // run off the end of the string
            break;
         UInt m = 0;
         for (ri = 0; ri < 16; ri += 2) {
            if ((validR & (3 << ri)) != (3 << ri)) break;
            if (argR[ri] <= argL[si] && argL[si] <= argR[ri+1]) { 
               m = 1; break;
            }
         }
         boolRes |= (m << si);
      }

      // boolRes is "pre-invalidated"
      UInt intRes1 = boolRes & 0xFFFF;

      // generate I-format output
      compute_PCMPxSTRx_gen_output(
         resV, resOSZACP,
         intRes1, zmaskL, zmaskR, validL, pol, idx, isxSTRM
      );

      return True;
   }

   return False;
}


/* Compute result and new OSZACP flags for all PCMP{E,I}STR{I,M}
   variants on 16-bit characters.

   For xSTRI variants, the new ECX value is placed in the 32 bits
   pointed to by *resV, and the top 96 bits are zeroed.  For xSTRM
   variants, the result is a 128 bit value and is placed at *resV in
   the obvious way.

   For all variants, the new OSZACP value is placed at *resOSZACP.

   argLV and argRV are the vector args.  The caller must prepare a
   8-bit mask for each, zmaskL and zmaskR.  For ISTRx variants this
   must be 1 for each zero byte of of the respective arg.  For ESTRx
   variants this is derived from the explicit length indication, and
   must be 0 in all places except at the bit index corresponding to
   the valid length (0 .. 8).  If the valid length is 8 then the
   mask must be all zeroes.  In all cases, bits 31:8 must be zero.

   imm8 is the original immediate from the instruction.  isSTRM
   indicates whether this is a xSTRM or xSTRI variant, which controls
   how much of *res is written.

   If the given imm8 case can be handled, the return value is True.
   If not, False is returned, and neither *res not *resOSZACP are
   altered.
*/

Bool compute_PCMPxSTRx_wide ( /*OUT*/V128* resV,
                              /*OUT*/UInt* resOSZACP,
                              V128* argLV,  V128* argRV,
                              UInt zmaskL, UInt zmaskR,
                              UInt imm8,   Bool isxSTRM )
{
   vassert(imm8 < 0x80);
   vassert((zmaskL >> 8) == 0);
   vassert((zmaskR >> 8) == 0);

   /* Explicitly reject any imm8 values that haven't been validated,
      even if they would probably work.  Life is too short to have
      unvalidated cases in the code base. */
   switch (imm8) {
      case 0x01: case 0x03: case 0x09: case 0x0B: case 0x0D:
      case 0x13:            case 0x1B:
                            case 0x39: case 0x3B:
                 case 0x45:            case 0x4B:
         break;
      default:
         return False;
   }

   UInt fmt = (imm8 >> 0) & 3; // imm8[1:0]  data format
   UInt agg = (imm8 >> 2) & 3; // imm8[3:2]  aggregation fn
   UInt pol = (imm8 >> 4) & 3; // imm8[5:4]  polarity
   UInt idx = (imm8 >> 6) & 1; // imm8[6]    1==msb/bytemask

   /*----------------------------------------*/
   /*-- strcmp on wide data                --*/
   /*----------------------------------------*/

   if (agg == 2/*equal each, aka strcmp*/
       && (fmt == 1/*uw*/ || fmt == 3/*sw*/)) {
      Int     i;
      UShort* argL = (UShort*)argLV;
      UShort* argR = (UShort*)argRV;
      UInt boolResII = 0;
      for (i = 7; i >= 0; i--) {
         UShort cL  = argL[i];
         UShort cR  = argR[i];
         boolResII = (boolResII << 1) | (cL == cR ? 1 : 0);
      }
      UInt validL = ~(zmaskL | -zmaskL);  // not(left(zmaskL))
      UInt validR = ~(zmaskR | -zmaskR);  // not(left(zmaskR))

      // do invalidation, common to all equal-each cases
      UInt intRes1
         = (boolResII & validL & validR)  // if both valid, use cmpres
           | (~ (validL | validR));       // if both invalid, force 1
                                          // else force 0
      intRes1 &= 0xFF;

      // generate I-format output
      compute_PCMPxSTRx_gen_output_wide(
         resV, resOSZACP,
         intRes1, zmaskL, zmaskR, validL, pol, idx, isxSTRM
      );

      return True;
   }

   /*----------------------------------------*/
   /*-- set membership on wide data        --*/
   /*----------------------------------------*/

   if (agg == 0/*equal any, aka find chars in a set*/
       && (fmt == 1/*uw*/ || fmt == 3/*sw*/)) {
      /* argL: the string,  argR: charset */
      UInt    si, ci;
      UShort* argL    = (UShort*)argLV;
      UShort* argR    = (UShort*)argRV;
      UInt    boolRes = 0;
      UInt    validL  = ~(zmaskL | -zmaskL);  // not(left(zmaskL))
      UInt    validR  = ~(zmaskR | -zmaskR);  // not(left(zmaskR))

      for (si = 0; si < 8; si++) {
         if ((validL & (1 << si)) == 0)
            // run off the end of the string.
            break;
         UInt m = 0;
         for (ci = 0; ci < 8; ci++) {
            if ((validR & (1 << ci)) == 0) break;
            if (argR[ci] == argL[si]) { m = 1; break; }
         }
         boolRes |= (m << si);
      }

      // boolRes is "pre-invalidated"
      UInt intRes1 = boolRes & 0xFF;
   
      // generate I-format output
      compute_PCMPxSTRx_gen_output_wide(
         resV, resOSZACP,
         intRes1, zmaskL, zmaskR, validL, pol, idx, isxSTRM
      );

      return True;
   }

   /*----------------------------------------*/
   /*-- substring search on wide data      --*/
   /*----------------------------------------*/

   if (agg == 3/*equal ordered, aka substring search*/
       && (fmt == 1/*uw*/ || fmt == 3/*sw*/)) {

      /* argL: haystack,  argR: needle */
      UInt    ni, hi;
      UShort* argL    = (UShort*)argLV;
      UShort* argR    = (UShort*)argRV;
      UInt    boolRes = 0;
      UInt    validL  = ~(zmaskL | -zmaskL);  // not(left(zmaskL))
      UInt    validR  = ~(zmaskR | -zmaskR);  // not(left(zmaskR))
      for (hi = 0; hi < 8; hi++) {
         UInt m = 1;
         for (ni = 0; ni < 8; ni++) {
            if ((validR & (1 << ni)) == 0) break;
            UInt i = ni + hi;
            if (i >= 8) break;
            if (argL[i] != argR[ni]) { m = 0; break; }
         }
         boolRes |= (m << hi);
         if ((validL & (1 << hi)) == 0)
            // run off the end of the haystack
            break;
      }

      // boolRes is "pre-invalidated"
      UInt intRes1 = boolRes & 0xFF;

      // generate I-format output
      compute_PCMPxSTRx_gen_output_wide(
         resV, resOSZACP,
         intRes1, zmaskL, zmaskR, validL, pol, idx, isxSTRM
      );

      return True;
   }

   /*----------------------------------------*/
   /*-- ranges, unsigned wide data         --*/
   /*----------------------------------------*/

   if (agg == 1/*ranges*/
       && fmt == 1/*uw*/) {

      /* argL: string,  argR: range-pairs */
      UInt    ri, si;
      UShort* argL    = (UShort*)argLV;
      UShort* argR    = (UShort*)argRV;
      UInt    boolRes = 0;
      UInt    validL  = ~(zmaskL | -zmaskL);  // not(left(zmaskL))
      UInt    validR  = ~(zmaskR | -zmaskR);  // not(left(zmaskR))
      for (si = 0; si < 8; si++) {
         if ((validL & (1 << si)) == 0)
            // run off the end of the string
            break;
         UInt m = 0;
         for (ri = 0; ri < 8; ri += 2) {
            if ((validR & (3 << ri)) != (3 << ri)) break;
            if (argR[ri] <= argL[si] && argL[si] <= argR[ri+1]) { 
               m = 1; break;
            }
         }
         boolRes |= (m << si);
      }

      // boolRes is "pre-invalidated"
      UInt intRes1 = boolRes & 0xFF;

      // generate I-format output
      compute_PCMPxSTRx_gen_output_wide(
         resV, resOSZACP,
         intRes1, zmaskL, zmaskR, validL, pol, idx, isxSTRM
      );

      return True;
   }

   return False;
}


/*---------------------------------------------------------------*/
/*--- end                                 guest_generic_x87.c ---*/
/*---------------------------------------------------------------*/
