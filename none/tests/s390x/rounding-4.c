#include <stdio.h>
#include <assert.h>
#include "opcodes.h"

/* Test "load rounded" with universally available rounding modes.
   Rounding mode is provided via FPC.
   Also test "load lengthened" (which is independent of rounding modes). */

volatile double d;
volatile float f;

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


void
load_rounded(void)
{
   f = d;
   printf("load rounded  d = %10.3f     f = %10.3f\n", d, f);
}

void
load_lengthened(void)
{
   d = f;
   printf("load lengthened  d = %22.20g    f = %22.20g\n", d, f);
}

/* Tests for load rounded and load lengthened */
int main()
{
   d = 12345678.25;
   set_rounding_mode(0);
   load_rounded();
   set_rounding_mode(1);
   load_rounded();
   set_rounding_mode(2);
   load_rounded();
   set_rounding_mode(3);
   load_rounded();
   printf("======================================\n");
   d = 12345678.75;
   set_rounding_mode(0);
   load_rounded();
   set_rounding_mode(1);
   load_rounded();
   set_rounding_mode(2);
   load_rounded();
   set_rounding_mode(3);
   load_rounded();
   printf("======================================\n");
   d = -12345678.25;
   set_rounding_mode(0);
   load_rounded();
   set_rounding_mode(1);
   load_rounded();
   set_rounding_mode(2);
   load_rounded();
   set_rounding_mode(3);
   load_rounded();
   printf("======================================\n");
   d = -12345678.75;
   set_rounding_mode(0);
   load_rounded();
   set_rounding_mode(1);
   load_rounded();
   set_rounding_mode(2);
   load_rounded();
   set_rounding_mode(3);
   load_rounded();
   printf("\n");

   f = 1234.5678f;
   load_lengthened();

   return 0;
}
