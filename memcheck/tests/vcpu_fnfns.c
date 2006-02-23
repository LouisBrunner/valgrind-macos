
/* Program to check that the FP stuff underlying these common FP
   functions isn't too badly broken.  Carefully kludged to print the
   same answers on different platforms (even when run natively). */

#include <stdio.h>
#include <math.h>

int main ( void )
{
   double d;
   float f;
   int i;

   const double tinyD = 0.0000000001;
   const double tinyF = 0.0001;

   /* -------------------- any arg -------------------- */

   d = -2.0;
   for (i = 0; i < 41; i++) {
      printf("floorD(%+20.13e) = %+20.13e\n", d, floor(d));
      d += 0.1-tinyD;
   }
   f = -2.0;
   for (i = 0; i < 41; i++) {
      printf("floorF(%+20.4e) = %+20.4e\n", (double)f, (double)floorf(f));
      f += 0.1-tinyF;
   }


   d = -2.0;
   for (i = 0; i < 41; i++) {
      printf(" ceilD(%+20.13e) = %+20.13e\n", d, ceil(d));
      d += 0.1-tinyD;
   }
   f = -2.0;
   for (i = 0; i < 41; i++) {
      printf(" ceilF(%+20.4e) = %+20.4e\n", (double)f, (double)ceilf(f));
      f += 0.1-tinyF;
   }


   d = -2.0;
   for (i = 0; i < 41; i++) {
      printf("  sinD(%+20.13e) = %+20.13e\n", d, sin(d));
      d += 0.1-tinyD;
   }
   f = -2.0;
   for (i = 0; i < 41; i++) {
      printf("  sinF(%+20.4e) = %+20.4e\n", (double)f, (double)sinf(f));
      f += 0.1-tinyF;
   }


   d = -2.0;
   for (i = 0; i < 41; i++) {
      printf("  cosD(%+20.13e) = %+20.13e\n", d, cos(d));
      d += 0.1-tinyD;
   }
   f = -2.0;
   for (i = 0; i < 41; i++) {
      printf("  cosF(%+20.4e) = %+20.4e\n", (double)f, (double)cosf(f));
      f += 0.1-tinyF;
   }


   d = -2.0;
   for (i = 0; i < 41; i++) {
      printf("  tanD(%+20.13e) = %+20.13e\n", d, tan(d));
      d += 0.1-tinyD;
   }
   f = -2.0;
   for (i = 0; i < 41; i++) {
      printf("  tanF(%+20.4e) = %+20.4e\n", (double)f, (double)tanf(f));
      f += 0.1-tinyF;
   }


   d = -2.0;
   for (i = 0; i < 41; i++) {
      printf("  expD(%+20.13e) = %+20.13e\n", d, exp(d));
      d += 0.1-tinyD;
   }
   f = -2.0;
   for (i = 0; i < 41; i++) {
      printf("  expF(%+20.4e) = %+20.4e\n", (double)f, (double)expf(f));
      f += 0.1-tinyF;
   }

   /* -------------------- >= 0 arg -------------------- */

   d = 0.0;
   for (i = 0; i < 21; i++) {
      printf(" sqrtD(%+20.13e) = %+20.13e\n", d, sqrt(d));
      d += 0.1-tinyD;
   }
   f = 0.0;
   for (i = 0; i < 21; i++) {
      printf(" sqrtF(%+20.4e) = %+20.4e\n", (double)f, (double)sqrtf(f));
      f += 0.1-tinyF;
   }


   d = 0.0;
   for (i = 0; i < 21; i++) {
      printf("  logD(%+20.13e) = %+20.13e\n", d, log(d));
      d += 0.1-tinyD;
   }
   f = 0.0;
   for (i = 0; i < 21; i++) {
      printf("  logF(%+20.4e) = %+20.4e\n", (double)f, (double)logf(f));
      f += 0.1-tinyF;
   }


   d = 0.0;
   for (i = 0; i < 21; i++) {
      printf("log10D(%+20.13e) = %+20.13e\n", d, log10(d));
      d += 0.1-tinyD;
   }
   f = 0.0;
   for (i = 0; i < 21; i++) {
      printf("log10F(%+20.4e) = %+20.4e\n", (double)f, (double)log10f(f));
      f += 0.1-tinyF;
   }

   /* -------------------- -1 .. +1 arg -------------------- */

   d = -1.0;
   for (i = 0; i < 21; i++) {
      printf(" asinD(%+20.13e) = %+20.13e\n", d, asin(d));
      d += 0.1-tinyD;
   }
   f = -1.0;
   for (i = 0; i < 21; i++) {
      printf(" asinF(%+20.4e) = %+20.4e\n", (double)f, (double)asinf(f));
      f += 0.1-tinyF;
   }

   /* acos(double) seems very prone to accuracy loss near the end of
      the range (arg --> +1.0).  Hence is different from the rest to
      stop it getting so close to 1.0. */
   d = -1.0;
   for (i = 0; i < 21; i++) {
      printf(" acosD(%+20.13e) = %+20.10e\n", d, acos(d));
      d += 0.1 - 1000.0*tinyD;
   }
   f = -1.0;
   for (i = 0; i < 21; i++) {
      printf(" acosF(%+20.4e) = %+20.4e\n", (double)f, (double)acosf(f));
      f += 0.1-tinyF;
   }


   d = -1.0;
   for (i = 0; i < 21; i++) {
      printf(" atanD(%+20.13e) = %+20.13e\n", d, atan(d));
      d += 0.1-tinyD;
   }
   f = -1.0;
   for (i = 0; i < 21; i++) {
      printf(" atanF(%+20.4e) = %+20.4e\n", (double)f, (double)atanf(f));
      f += 0.1-tinyF;
   }


   d = -1.0;
   for (i = 0; i < 21; i++) {
      printf("atan2D(%+20.13e) = %+20.13e\n", d, atan2(d, 1.0));
      d += 0.1-tinyD;
   }
   f = -1.0;
   for (i = 0; i < 21; i++) {
      printf("atan2F(%+20.4e) = %+20.4e\n", (double)f, (double)atan2f(f,1.0));
      f += 0.1-tinyF;
   }

   return 0;
}
