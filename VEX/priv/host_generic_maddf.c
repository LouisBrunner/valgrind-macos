
/*---------------------------------------------------------------*/
/*--- begin                              host_generic_maddf.c ---*/
/*---------------------------------------------------------------*/

/* 
   Compute x * y + z as ternary operation.
   Copyright (C) 2010-2013 Free Software Foundation, Inc.
   This file is part of the GNU C Library.
   Contributed by Jakub Jelinek <jakub@redhat.com>, 2010.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.
*/

/* Generic helper functions for doing FMA, i.e. compute x * y + z
   as ternary operation.
   These are purely back-end entities and cannot be seen/referenced
   from IR. */

#include "libvex_basictypes.h"
#include "host_generic_maddf.h"
#include "main_util.h"

/* This implementation relies on Double being more than twice as
   precise as Float and uses rounding to odd in order to avoid problems
   with double rounding.
   See a paper by Boldo and Melquiond:
   http://www.lri.fr/~melquion/doc/08-tc.pdf  */

#define FORCE_EVAL(X) __asm __volatile__ ("" : : "m" (X))

#if defined(__x86_64__) && defined(__SSE2_MATH__)
# define ENV_TYPE unsigned int
/* Save current rounding mode into ENV, hold exceptions, set rounding
   mode to rounding toward zero.  */
# define ROUNDTOZERO(env) \
   do {							\
      unsigned int mxcsr;				\
      __asm __volatile__ ("stmxcsr %0" : "=m" (mxcsr));	\
      (env) = mxcsr;					\
      mxcsr = (mxcsr | 0x7f80) & ~0x3f;			\
      __asm __volatile__ ("ldmxcsr %0" : : "m" (mxcsr));\
   } while (0)
/* Restore exceptions from ENV, return if inexact exception has been raised
   since ROUNDTOZERO.  */
# define RESET_TESTINEXACT(env) \
   ({							\
      unsigned int mxcsr, ret;				\
      __asm __volatile__ ("stmxcsr %0" : "=m" (mxcsr));	\
      ret = (mxcsr >> 5) & 1;				\
      mxcsr = (mxcsr & 0x3d) | (env);			\
      __asm __volatile__ ("ldmxcsr %0" : : "m" (mxcsr));\
      ret;						\
   })
/* Return if inexact exception has been raised since ROUNDTOZERO.  */
# define TESTINEXACT() \
   ({							\
      unsigned int mxcsr;				\
      __asm __volatile__ ("stmxcsr %0" : "=m" (mxcsr));	\
      (mxcsr >> 5) & 1;					\
   })
#endif

#define DBL_MANT_DIG 53
#define IEEE754_DOUBLE_BIAS 0x3ff

union vg_ieee754_double {
   Double d;

   /* This is the IEEE 754 double-precision format.  */
   struct {
#ifdef VKI_BIG_ENDIAN
      unsigned int negative:1;
      unsigned int exponent:11;
      unsigned int mantissa0:20;
      unsigned int mantissa1:32;
#else
      unsigned int mantissa1:32;
      unsigned int mantissa0:20;
      unsigned int exponent:11;
      unsigned int negative:1;
#endif
   } ieee;
};

void VEX_REGPARM(3)
     h_generic_calc_MAddF32 ( /*OUT*/Float* res,
                               Float* argX, Float* argY, Float* argZ )
{
#ifndef ENV_TYPE
   /* Lame fallback implementation.  */
   *res = *argX * *argY + *argZ;
#else
   ENV_TYPE env;
   /* Multiplication is always exact.  */
   Double temp = (Double) *argX * (Double) *argY;
   union vg_ieee754_double u;

   ROUNDTOZERO (env);

   /* Perform addition with round to odd.  */
   u.d = temp + (Double) *argZ;
   /* Ensure the addition is not scheduled after fetestexcept call.  */
   FORCE_EVAL (u.d);

   /* Reset rounding mode and test for inexact simultaneously.  */
   int j = RESET_TESTINEXACT (env);

   if ((u.ieee.mantissa1 & 1) == 0 && u.ieee.exponent != 0x7ff)
      u.ieee.mantissa1 |= j;

   /* And finally truncation with round to nearest.  */
   *res = (Float) u.d;
#endif
}


void VEX_REGPARM(3)
     h_generic_calc_MAddF64 ( /*OUT*/Double* res,
                               Double* argX, Double* argY, Double* argZ )
{
#ifndef ENV_TYPE
   /* Lame fallback implementation.  */
   *res = *argX * *argY + *argZ;
#else
   Double x = *argX, y = *argY, z = *argZ;
   union vg_ieee754_double u, v, w;
   int adjust = 0;
   u.d = x;
   v.d = y;
   w.d = z;
   if (UNLIKELY (u.ieee.exponent + v.ieee.exponent
                 >= 0x7ff + IEEE754_DOUBLE_BIAS - DBL_MANT_DIG)
       || UNLIKELY (u.ieee.exponent >= 0x7ff - DBL_MANT_DIG)
       || UNLIKELY (v.ieee.exponent >= 0x7ff - DBL_MANT_DIG)
       || UNLIKELY (w.ieee.exponent >= 0x7ff - DBL_MANT_DIG)
       || UNLIKELY (u.ieee.exponent + v.ieee.exponent
                    <= IEEE754_DOUBLE_BIAS + DBL_MANT_DIG)) {
      /* If z is Inf, but x and y are finite, the result should be
         z rather than NaN.  */
      if (w.ieee.exponent == 0x7ff
          && u.ieee.exponent != 0x7ff
          && v.ieee.exponent != 0x7ff) {
         *res = (z + x) + y;
         return;
      }
      /* If x or y or z is Inf/NaN, or if fma will certainly overflow,
         or if x * y is less than half of DBL_DENORM_MIN,
         compute as x * y + z.  */
      if (u.ieee.exponent == 0x7ff
          || v.ieee.exponent == 0x7ff
          || w.ieee.exponent == 0x7ff
          || u.ieee.exponent + v.ieee.exponent > 0x7ff + IEEE754_DOUBLE_BIAS
          || u.ieee.exponent + v.ieee.exponent
             < IEEE754_DOUBLE_BIAS - DBL_MANT_DIG - 2) {
         *res = x * y + z;
         return;
      }
      if (u.ieee.exponent + v.ieee.exponent
          >= 0x7ff + IEEE754_DOUBLE_BIAS - DBL_MANT_DIG) {
         /* Compute 1p-53 times smaller result and multiply
            at the end.  */
         if (u.ieee.exponent > v.ieee.exponent)
            u.ieee.exponent -= DBL_MANT_DIG;
         else
            v.ieee.exponent -= DBL_MANT_DIG;
         /* If x + y exponent is very large and z exponent is very small,
            it doesn't matter if we don't adjust it.  */
         if (w.ieee.exponent > DBL_MANT_DIG)
            w.ieee.exponent -= DBL_MANT_DIG;
         adjust = 1;
      } else if (w.ieee.exponent >= 0x7ff - DBL_MANT_DIG) {
         /* Similarly.
            If z exponent is very large and x and y exponents are
            very small, it doesn't matter if we don't adjust it.  */
         if (u.ieee.exponent > v.ieee.exponent) {
            if (u.ieee.exponent > DBL_MANT_DIG)
               u.ieee.exponent -= DBL_MANT_DIG;
         } else if (v.ieee.exponent > DBL_MANT_DIG)
            v.ieee.exponent -= DBL_MANT_DIG;
         w.ieee.exponent -= DBL_MANT_DIG;
         adjust = 1;
      } else if (u.ieee.exponent >= 0x7ff - DBL_MANT_DIG) {
         u.ieee.exponent -= DBL_MANT_DIG;
         if (v.ieee.exponent)
            v.ieee.exponent += DBL_MANT_DIG;
         else
            v.d *= 0x1p53;
      } else if (v.ieee.exponent >= 0x7ff - DBL_MANT_DIG) {
         v.ieee.exponent -= DBL_MANT_DIG;
         if (u.ieee.exponent)
            u.ieee.exponent += DBL_MANT_DIG;
         else
            u.d *= 0x1p53;
      } else /* if (u.ieee.exponent + v.ieee.exponent
                    <= IEEE754_DOUBLE_BIAS + DBL_MANT_DIG) */ {
         if (u.ieee.exponent > v.ieee.exponent)
            u.ieee.exponent += 2 * DBL_MANT_DIG;
         else
            v.ieee.exponent += 2 * DBL_MANT_DIG;
         if (w.ieee.exponent <= 4 * DBL_MANT_DIG + 4) {
            if (w.ieee.exponent)
               w.ieee.exponent += 2 * DBL_MANT_DIG;
            else
               w.d *= 0x1p106;
            adjust = -1;
         }
         /* Otherwise x * y should just affect inexact
            and nothing else.  */
      }
      x = u.d;
      y = v.d;
      z = w.d;
   }
   /* Multiplication m1 + m2 = x * y using Dekker's algorithm.  */
#  define C ((1 << (DBL_MANT_DIG + 1) / 2) + 1)
   Double x1 = x * C;
   Double y1 = y * C;
   Double m1 = x * y;
   x1 = (x - x1) + x1;
   y1 = (y - y1) + y1;
   Double x2 = x - x1;
   Double y2 = y - y1;
   Double m2 = (((x1 * y1 - m1) + x1 * y2) + x2 * y1) + x2 * y2;
#  undef C

   /* Addition a1 + a2 = z + m1 using Knuth's algorithm.  */
   Double a1 = z + m1;
   Double t1 = a1 - z;
   Double t2 = a1 - t1;
   t1 = m1 - t1;
   t2 = z - t2;
   Double a2 = t1 + t2;

   ENV_TYPE env;
   ROUNDTOZERO (env);

   /* Perform m2 + a2 addition with round to odd.  */
   u.d = a2 + m2;

   if (UNLIKELY (adjust < 0)) {
      if ((u.ieee.mantissa1 & 1) == 0)
         u.ieee.mantissa1 |= TESTINEXACT ();
      v.d = a1 + u.d;
      /* Ensure the addition is not scheduled after fetestexcept call.  */
      FORCE_EVAL (v.d);
   }

   /* Reset rounding mode and test for inexact simultaneously.  */
   int j = RESET_TESTINEXACT (env) != 0;

   if (LIKELY (adjust == 0)) {
      if ((u.ieee.mantissa1 & 1) == 0 && u.ieee.exponent != 0x7ff)
         u.ieee.mantissa1 |= j;
      /* Result is a1 + u.d.  */
      *res = a1 + u.d;
   } else if (LIKELY (adjust > 0)) {
      if ((u.ieee.mantissa1 & 1) == 0 && u.ieee.exponent != 0x7ff)
         u.ieee.mantissa1 |= j;
      /* Result is a1 + u.d, scaled up.  */
      *res = (a1 + u.d) * 0x1p53;
   } else {
      /* If a1 + u.d is exact, the only rounding happens during
         scaling down.  */
      if (j == 0) {
         *res = v.d * 0x1p-106;
         return;
      }
      /* If result rounded to zero is not subnormal, no double
         rounding will occur.  */
      if (v.ieee.exponent > 106) {
         *res = (a1 + u.d) * 0x1p-106;
         return;
      }
      /* If v.d * 0x1p-106 with round to zero is a subnormal above
         or equal to DBL_MIN / 2, then v.d * 0x1p-106 shifts mantissa
         down just by 1 bit, which means v.ieee.mantissa1 |= j would
         change the round bit, not sticky or guard bit.
         v.d * 0x1p-106 never normalizes by shifting up,
         so round bit plus sticky bit should be already enough
         for proper rounding.  */
      if (v.ieee.exponent == 106) {
         /* v.ieee.mantissa1 & 2 is LSB bit of the result before rounding,
            v.ieee.mantissa1 & 1 is the round bit and j is our sticky
            bit.  In round-to-nearest 001 rounds down like 00,
            011 rounds up, even though 01 rounds down (thus we need
            to adjust), 101 rounds down like 10 and 111 rounds up
            like 11.  */
         if ((v.ieee.mantissa1 & 3) == 1) {
            v.d *= 0x1p-106;
            if (v.ieee.negative)
               *res = v.d - 0x1p-1074;
            else
               *res = v.d + 0x1p-1074;
         } else
            *res = v.d * 0x1p-106;
         return;
      }
      v.ieee.mantissa1 |= j;
      *res = v.d * 0x1p-106;
      return;
    }
#endif
}

/*---------------------------------------------------------------*/
/*--- end                                 host_generic_maddf.c --*/
/*---------------------------------------------------------------*/
