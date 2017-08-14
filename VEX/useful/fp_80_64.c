
#ifndef USED_AS_INCLUDE

#include "../pub/libvex_basictypes.h"
#include <stdio.h>
#include <malloc.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>


/* Test program for developing code for conversions between
   x87 64-bit and 80-bit floats.

   80-bit format exists only for x86/x86-64, and so the routines
   hardwire it as little-endian.  The 64-bit format (IEEE double)
   could exist on any platform, little or big-endian and so we
   have to take that into account.  IOW, these routines have to
   work correctly when compiled on both big- and little-endian
   targets, but the 80-bit images only ever have to exist in
   little-endian format. 
*/
static void show_f80 ( UChar* );
static void show_f64 ( UChar* );

static inline
UInt read_bit_array ( UChar* arr, UInt n )
{
   UChar c = arr[n >> 3];
   c >>= (n&7);
   return c & 1;
}

static inline
void write_bit_array ( UChar* arr, UInt n, UInt b )
{
   UChar c = arr[n >> 3];
   c &= ~(1 << (n&7));
   c |= ((b&1) << (n&7));
   arr[n >> 3] = c;
}


static void convert_f80le_to_f64le_HW ( /*IN*/UChar* f80, /*OUT*/UChar* f64 )
{
  asm volatile ("ffree %%st(7); fldt (%0); fstpl (%1)"
                :
                : "r" (&f80[0]), "r" (&f64[0])
                : "memory" );
}

static void convert_f64le_to_f80le_HW ( /*IN*/UChar* f64, /*OUT*/UChar* f80 )
{
  asm volatile ("ffree %%st(7); fldl (%0); fstpt (%1)"
                :
                : "r" (&f64[0]), "r" (&f80[0])
                : "memory" );
}

#endif /* ndef USED_AS_INCLUDE */



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

/* Convert a IEEE754 double (64-bit) into an x87 extended double
   (80-bit), mimicing the hardware fairly closely.  Both numbers are
   stored little-endian.  Limitations, all of which could be fixed,
   given some level of hassle:

   * Identity of NaNs is not preserved.

   See comments in the code for more details.  
*/
static void convert_f64le_to_f80le ( /*IN*/UChar* f64, /*OUT*/UChar* f80 )
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


/* Convert a x87 extended double (80-bit) into an IEEE 754 double
   (64-bit), mimicking the hardware fairly closely.  Both numbers are
   stored little-endian.  Limitations, both of which could be fixed,
   given some level of hassle:

   * Rounding following truncation could be a bit better.

   * Identity of NaNs is not preserved.

   See comments in the code for more details.
*/
static void convert_f80le_to_f64le ( /*IN*/UChar* f80, /*OUT*/UChar* f64 )
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
         assert(j >= 0 && j < 52);
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


#ifndef USED_AS_INCLUDE

//////////////

static void show_f80 ( UChar* f80 )
{
  Int i;
  printf("%d ", read_bit_array(f80, 79));

  for (i = 78; i >= 64; i--)
    printf("%d", read_bit_array(f80, i));

  printf(" %d ", read_bit_array(f80, 63));

  for (i = 62; i >= 0; i--)
    printf("%d", read_bit_array(f80, i));
}

static void show_f64le ( UChar* f64 )
{
  Int i;
  printf("%d     ", read_bit_array(f64, 63));

  for (i = 62; i >= 52; i--)
    printf("%d", read_bit_array(f64, i));

  printf("   ");
  for (i = 51; i >= 0; i--)
    printf("%d", read_bit_array(f64, i));
}

//////////////


/* Convert f80 to a 64-bit IEEE double using both the hardware and the
   soft version, and compare the results.  If they differ, print
   details and return 1.  If they are identical, return 0.
*/
int do_80_to_64_test ( Int test_no, UChar* f80, UChar* f64h, UChar* f64s)
{
   Char buf64s[100], buf64h[100];
   Bool same;
   Int k;
   convert_f80le_to_f64le_HW(f80, f64h);
   convert_f80le_to_f64le(f80, f64s);
   same = True;
   for (k = 0; k < 8; k++) {
      if (f64s[k] != f64h[k]) {
         same = False; break;
      }
   }
   /* bitwise identical */
   if (same)
      return 0;

   sprintf(buf64s, "%.16e", *(double*)f64s);
   sprintf(buf64h, "%.16e", *(double*)f64h);

   /* Not bitwise identical, but pretty darn close */
   if (0 == strcmp(buf64s, buf64h))
      return 0;

    printf("\n");
    printf("f80:  "); show_f80(f80); printf("\n");
    printf("f64h: "); show_f64le(f64h); printf("\n");
    printf("f64s: "); show_f64le(f64s); printf("\n");

    printf("[test %d]  %.16Le -> (hw %s, sw %s)\n", 
           test_no, *(long double*)f80,
           buf64h, buf64s );

    return 1;
}


/* Convert an IEEE 64-bit double to a x87 extended double (80 bit)
   using both the hardware and the soft version, and compare the
   results.  If they differ, print details and return 1.  If they are
   identical, return 0.  
*/
int do_64_to_80_test ( Int test_no, UChar* f64, UChar* f80h, UChar* f80s)
{
   Char buf80s[100], buf80h[100];
   Bool same;
   Int k;
   convert_f64le_to_f80le_HW(f64, f80h);
   convert_f64le_to_f80le(f64, f80s);
   same = True;
   for (k = 0; k < 10; k++) {
      if (f80s[k] != f80h[k]) {
         same = False; break;
      }
   }
   /* bitwise identical */
   if (same)
      return 0;

   sprintf(buf80s, "%.20Le", *(long double*)f80s);
   sprintf(buf80h, "%.20Le", *(long double*)f80h);

   /* Not bitwise identical, but pretty darn close */
   if (0 == strcmp(buf80s, buf80h))
      return 0;

    printf("\n");
    printf("f64:  "); show_f64le(f64); printf("\n");
    printf("f80h: "); show_f80(f80h); printf("\n");
    printf("f80s: "); show_f80(f80s); printf("\n");

    printf("[test %d]  %.16e -> (hw %s, sw %s)\n", 
           test_no, *(double*)f64,
           buf80h, buf80s );

    return 1;
}



void do_80_to_64_tests ( void )
{
   UInt b9,b8,b7,i, j;
   Int fails=0, tests=0;
   UChar* f64h = malloc(8);
   UChar* f64s = malloc(8);
   UChar* f80  = malloc(10);
   int STEP = 1;

   srandom(4343);

   /* Ten million random bit patterns */
   for (i = 0; i < 10000000; i++) {
     tests++;
     for (j = 0; j < 10; j++)
       f80[j] = (random() >> 7) & 255;

     fails += do_80_to_64_test(tests, f80, f64h, f64s);
   }

   /* 2^24 numbers in which the first 24 bits are tested exhaustively
      -- this covers the sign, exponent and leading part of the
      mantissa. */
   for (b9 = 0; b9 < 256; b9 += STEP) {
      for (b8 = 0; b8 < 256; b8 += STEP) {
         for (b7 = 0; b7 < 256; b7 += STEP) {
           tests++;
            for (i = 0; i < 10; i++) 
               f80[i] = 0;
            for (i = 0; i < 8; i++)
               f64h[i] = f64s[i] = 0;
            f80[9] = b9;
            f80[8] = b8;
            f80[7] = b7;

    fails += do_80_to_64_test(tests, f80, f64h, f64s);
   }}}

   printf("\n80 -> 64:  %d tests, %d fails\n\n", tests, fails);
}


void do_64_to_80_tests ( void )
{
   UInt b7,b6,b5,i, j;
   Int fails=0, tests=0;
   UChar* f80h = malloc(10);
   UChar* f80s = malloc(10);
   UChar* f64  = malloc(8);
   int STEP = 1;

   srandom(2323);

   /* Ten million random bit patterns */
   for (i = 0; i < 10000000; i++) {
     tests++;
     for (j = 0; j < 8; j++)
       f64[j] = (random() >> 13) & 255;

     fails += do_64_to_80_test(tests, f64, f80h, f80s);
   }

   /* 2^24 numbers in which the first 24 bits are tested exhaustively
      -- this covers the sign, exponent and leading part of the
      mantissa. */
   for (b7 = 0; b7 < 256; b7 += STEP) {
      for (b6 = 0; b6 < 256; b6 += STEP) {
         for (b5 = 0; b5 < 256; b5 += STEP) {
           tests++;
            for (i = 0; i < 8; i++) 
               f64[i] = 0;
            for (i = 0; i < 10; i++)
               f80h[i] = f80s[i] = 0;
            f64[7] = b7;
            f64[6] = b6;
            f64[5] = b5;

    fails += do_64_to_80_test(tests, f64, f80h, f80s);
   }}}

   printf("\n64 -> 80:  %d tests, %d fails\n\n", tests, fails);
}


int main ( void )
{
   do_80_to_64_tests();
   do_64_to_80_tests();
   return 0;
}

#endif /* ndef USED_AS_INCLUDE */
