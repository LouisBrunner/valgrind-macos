#include <stdio.h>
#include <assert.h>
#include <stdint.h>
#include <inttypes.h>
#include "opcodes.h"

/* Test "convert from fixed" with universally available rounding modes.
   Rounding mode is provided via FPC. */

volatile int32_t i32;
volatile int64_t i64;

const char *
rtext(unsigned fpc_round)
{
   switch (fpc_round) {
   case 0: return "[-> near]";
   case 1: return "[-> zero]";
   case 2: return "[-> +inf]";
   case 3: return "[-> -inf]";
   }
   assert(0);
}

void
set_rounding_mode(unsigned mode)
{
   printf("setting FPC rounding mode to %s\n", rtext(mode));
   register unsigned r asm("1") = mode;
   __asm__ volatile ( SFPC(1) : : "d"(r) );
}

void cefbr(unsigned mode)
{
   set_rounding_mode(mode);

   float out;

   __asm__ volatile("cefbr %[r1],%[r2]" : [r1] "=f"(out) : [r2] "d"(i32));
   printf("cefbr:  %"PRId32" -> %f\n", i32, out);
}

void cegbr(unsigned mode)
{
   set_rounding_mode(mode);

   float out;

   __asm__ volatile("cegbr %[r1],%[r2]" : [r1] "=f"(out) : [r2] "d"(i64));
   printf("cegbr:  %"PRId64" -> %f\n", i64, out);
}

void cdgbr(unsigned mode)
{
   set_rounding_mode(mode);

   double out;

   __asm__ volatile("cdgbr %[r1],%[r2]" : [r1] "=f"(out) : [r2] "d"(i64));
   printf("cegbr:  %"PRId64" -> %f\n", i64, out);
}


int main()
{
   int mode;

   /* i32 -> f32 */
   i32 = INT32_MAX;
   for (mode = 0; mode <= 3; ++mode) cefbr(mode);
   printf("\n");
   i32 = INT32_MIN;
   for (mode = 0; mode <= 3; ++mode) cefbr(mode);
   printf("\n");

   /* i64 -> f32 */
   i64 = INT64_MAX;
   for (mode = 0; mode <= 3; ++mode) cegbr(mode);
   printf("\n");
   i64 = INT64_MIN;
   for (mode = 0; mode <= 3; ++mode) cegbr(mode);
   printf("\n");

   /* i64 -> f64 */
   i64 = INT64_MAX;
   for (mode = 0; mode <= 3; ++mode) cdgbr(mode);
   printf("\n");
   i64 = INT64_MIN;
   for (mode = 0; mode <= 3; ++mode) cdgbr(mode);
   printf("\n");

   return 0;
}
