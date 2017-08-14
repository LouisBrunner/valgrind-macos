
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (guest-generic/g_generic_x87.c) is            ---*/
/*--- Copyright (C) OpenWorks LLP.  All rights reserved.      ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of LibVEX, a library for dynamic binary
   instrumentation and translation.

   Copyright (C) 2004-2008 OpenWorks LLP.  All rights reserved.

   This library is made available under a dual licensing scheme.

   If you link LibVEX against other code all of which is itself
   licensed under the GNU General Public License, version 2 dated June
   1991 ("GPL v2"), then you may use LibVEX under the terms of the GPL
   v2, as appearing in the file LICENSE.GPL.  If the file LICENSE.GPL
   is missing, you can obtain a copy of the GPL v2 from the Free
   Software Foundation Inc., 51 Franklin St, Fifth Floor, Boston, MA
   02110-1301, USA.

   For any other uses of LibVEX, you must first obtain a commercial
   license from OpenWorks LLP.  Please contact info@open-works.co.uk
   for information about commercial licensing.

   This software is provided by OpenWorks LLP "as is" and any express
   or implied warranties, including, but not limited to, the implied
   warranties of merchantability and fitness for a particular purpose
   are disclaimed.  In no event shall OpenWorks LLP be liable for any
   direct, indirect, incidental, special, exemplary, or consequential
   damages (including, but not limited to, procurement of substitute
   goods or services; loss of use, data, or profits; or business
   interruption) however caused and on any theory of liability,
   whether in contract, strict liability, or tort (including
   negligence or otherwise) arising in any way out of the use of this
   software, even if advised of the possibility of such damage.

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

#include "main/vex_util.h"
#include "guest-generic/g_generic_x87.h"


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
         /* QNaN.  Make a QNaN:
            S 1--1 (15)  1  1--1 (63) 
         */
         f80[9] = toUChar( (sign << 7) | 0x7F );
         f80[8] = 0xFF;
         f80[7] = 0xFF;
         f80[6] = f80[5] = f80[4] = f80[3] 
                = f80[2] = f80[1] = f80[0] = 0xFF;
      } else {
         /* SNaN.  Make a SNaN:
            S 1--1 (15)  0  1--1 (63) 
         */
         f80[9] = toUChar( (sign << 7) | 0x7F );
         f80[8] = 0xFF;
         f80[7] = 0x7F;
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
          0  ... 0    Inf
          0X ... X    SNaN
          1X ... X    QNaN
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
         bit 62.  Note, this destroys all the trailing bits
         (identity?) of the NaN.  IEEE754 doesn't require preserving
         these (it only requires that there be one QNaN value and one
         SNaN value), but x87 does seem to have some ability to
         preserve them.  Anyway, here, the NaN's identity is
         destroyed.  Could be improved. */
      if (f80[8] & 0x40) {
         /* QNaN.  Make a QNaN:
            S 1--1 (11)  1  1--1 (51) 
         */
         f64[7] = toUChar((sign << 7) | 0x7F);
         f64[6] = 0xFF;
         f64[5] = f64[4] = f64[3] = f64[2] = f64[1] = f64[0] = 0xFF;
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

   /* Mimic PIII behaviour for special cases. */
   if (arg == posInf)
      return getExp ? posInf : posInf;
   if (arg == negInf)
      return getExp ? posInf : negInf;
   if ((arg & nanMask) == nanMask)
      return qNan;
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


/*---------------------------------------------------------------*/
/*--- end                       guest-generic/h_generic_x87.c ---*/
/*---------------------------------------------------------------*/
