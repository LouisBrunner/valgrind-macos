#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include "opcodes.h"

/* Basic test that we can set the rounding mode in the FPC and
   query it. Covers only generally available rounding modes. */

void
set_rounding_mode(unsigned mode)
{
   register unsigned r asm("1") = mode;
   __asm__ volatile ( SFPC(1) : : "d"(r) );
}

unsigned
get_rounding_mode(void)
{
   unsigned fpc;

   __asm__ volatile ("stfpc  %0" : "=Q"(fpc));

   return fpc & 0x7;
}


int main(void)
{
   int i;
   const unsigned rmodes[] = { 0, 1, 2, 3 };

   printf("initial rounding mode: %u\n", get_rounding_mode());

   for (i = 0; i < sizeof rmodes / sizeof rmodes[0]; ++i) {
      printf("setting rounding mode to %u\n", rmodes[i]);
      set_rounding_mode(rmodes[i]);
      printf("...checking: %u\n", get_rounding_mode());
   }

   return 0;
}
