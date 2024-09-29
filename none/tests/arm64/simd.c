#include <math.h>    // isnormal
#include <stdio.h>  
#include "simd.h"

/* Conversion based on IEEE half-precision, as described in the IEEE 754-2008
   standard and Arm Reference Manual 'A1.4.2 Half-precision floating-point
   formats' where hardware capability supports __fp16 (VEX_HWCAPS_ARM64_FP16
   and VEX_HWCAPS_ARM64_VFP16 set).
*/
UInt halfToSingleFPAsInt(UShort y)
{
   int s = (y >> 15) & 0x00000001;  // Sign bit
   int e = (y >> 10) & 0x0000001f;  // Exponent
   int f =  y        & 0x000003ff;  // Fraction

   // Handle +/- INF (7c00 and fc00 -INF) and +/-0
   if (e == 0) {
      if (f == 0)
         return s << 31;
      else {                           // Normalize
         while (!(f & 0x00000400)) {
            f <<= 1;
            e -=  1;
         }
         e += 1;
         f &= ~0x00000400;
      }
   } else if (e == 31) {
      if (f == 0)                         // INF
         return (s << 31) | 0x7f800000;
      else                                // NaN
         return (s << 31) | 0x7f800000 | (f << 13);
   }

   e = e + (127 - 15);
   f = f << 13;

   return ((s << 31) | (e << 23) | f);
}

UChar randUChar ( void )
{
   static UInt seed = 80021;
   seed = 1103515245 * seed + 12345;
   return (seed >> 17) & 0xFF;
}

/* Generates a random V128. Ensures that that it contains normalised FP numbers
   when viewed as either F16x8, F32x4 or F64x2, so that it is reasonable to use
   in FP test cases. */
void randV128 ( /*OUT*/V128* v, LaneTy ty )
{
   static UInt nCalls = 0, nIters = 0;
   Int i;
   nCalls++;
   while (1) {
      nIters++;
      for (i = 0; i < 16; i++) {
         v->u8[i] = randUChar();
      }
      if (isnormal(v->f32[0]) && isnormal(v->f32[1]) && isnormal(v->f32[2])
          && isnormal(v->f32[3]) && isnormal(v->f64[0]) && isnormal(v->f64[1])
          && isnormal(shortToSingle(v->f16[0])) && isnormal(shortToSingle(v->f16[1]))
          && isnormal(shortToSingle(v->f16[2])) && isnormal(shortToSingle(v->f16[3]))
          && isnormal(shortToSingle(v->f16[4])) && isnormal(shortToSingle(v->f16[5]))
          && isnormal(shortToSingle(v->f16[6])) && isnormal(shortToSingle(v->f16[7]))) {
        break;
     }
   }
   if (0 == (nCalls & 0xFF))
      printf("randV128: %u calls, %u iters\n", nCalls, nIters);
}

static Double special_values[10];
static Bool   special_values_initted = False;

static __attribute__((noinline))
Double negate ( Double d ) { return -d; }
static __attribute__((noinline))
Double divf64 ( Double x, Double y ) { return x/y; }

static __attribute__((noinline))
Double plusZero  ( void ) { return 0.0; }
static __attribute__((noinline))
Double minusZero ( void ) { return negate(plusZero()); }

static __attribute__((noinline))
Double plusOne  ( void ) { return 1.0; }
static __attribute__((noinline))
Double minusOne ( void ) { return negate(plusOne()); }

static __attribute__((noinline))
Double plusInf   ( void ) { return 1.0 / 0.0; }
static __attribute__((noinline))
Double minusInf  ( void ) { return negate(plusInf()); }

static __attribute__((noinline))
Double plusNaN  ( void ) { return divf64(plusInf(),plusInf()); }
static __attribute__((noinline))
Double minusNaN ( void ) { return negate(plusNaN()); }

static __attribute__((noinline))
Double plusDenorm  ( void ) { return 1.23e-315 / 1e3; }
static __attribute__((noinline))
Double minusDenorm ( void ) { return negate(plusDenorm()); }


static void ensure_special_values_initted ( void )
{
   if (special_values_initted) return;
   special_values[0] = plusZero();
   special_values[1] = minusZero();
   special_values[2] = plusOne();
   special_values[3] = minusOne();
   special_values[4] = plusInf();
   special_values[5] = minusInf();
   special_values[6] = plusNaN();
   special_values[7] = minusNaN();
   special_values[8] = plusDenorm();
   special_values[9] = minusDenorm();
   special_values_initted = True;
   int i;
   printf("\n");
   for (i = 0; i < 10; i++) {
      printf("special value %d = %e\n", i, special_values[i]);
   }
   printf("\n");
}

Double randDouble ( void )
{
   ensure_special_values_initted();
   UChar c = randUChar();
   if (c >= 128) {
      // return a normal number about half of the time.
      // 0 .. 2^63-1
      ULong u64 = randULong(TyDF);
      // -2^62 .. 2^62-1
      Long s64 = (Long)u64;
      // -2^55 .. 2^55-1
      s64 >>= (62-55);
      // and now as a float
      return (Double)s64;
   }
   c = randUChar() % 10;
   return special_values[c];
}

Float randFloat ( void )
{
   ensure_special_values_initted();
   UChar c = randUChar();
   if (c >= 128) {
      // return a normal number about half of the time.
      // 0 .. 2^63-1
      ULong u64 = randULong(TyDF);
      // -2^62 .. 2^62-1
      Long s64 = (Long)u64;
      // -2^25 .. 2^25-1
      s64 >>= (62-25);
      // and now as a float
      return (Float)s64;
   }
   c = randUChar() % 10;
   return special_values[c];
}

void randBlock_Doubles ( V128* block, Int nBlock )
{
   Int i;
   for (i = 0; i < nBlock; i++) {
      block[i].f64[0] = randDouble();
      block[i].f64[1] = randDouble();
   }
}

void randBlock_Floats ( V128* block, Int nBlock )
{
   Int i;
   for (i = 0; i < nBlock; i++) {
      block[i].f32[0] = randFloat();
      block[i].f32[1] = randFloat();
      block[i].f32[2] = randFloat();
      block[i].f32[3] = randFloat();
   }
}
