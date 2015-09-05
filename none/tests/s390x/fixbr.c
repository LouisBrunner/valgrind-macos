#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>
#include "opcodes.h"
#include "rounding.h"

/* Test "fixbr"  with rounding mode given in insn (m3 field)
   Covers all generally available rounding modes that can be mapped to
   IRRoundingMode. As a consequence m3=1 which is "round to nearest with
   ties away from 0" is not tested here.
*/

const char *
rtext(unsigned m3_round)
{
   switch (m3_round) {
   case 0: return "[-> per fpc]";
   case 1: return "[-> nearest away]";
   case 3: return "[-> prepare short]";   // floating point extension fac needed
   case 4: return "[-> nearest even]";
   case 5: return "[-> 0]";
   case 6: return "[-> +inf]";
   case 7: return "[-> -inf]";
   }
   assert(0);
}

#define round_to_int(value,round)                             \
do {                                                          \
   long double src = value;                                   \
   long double dst;                                           \
                                                              \
   __asm__ volatile ("fixbr %[dst]," #round ",%[src]\n\t"     \
                     : [dst] "=f"(dst)                        \
                     : [src] "f"(src));                       \
                                                              \
   printf("fixbr %.5Lf\t-> %Lg  %s\n",                        \
          src, dst, rtext(round));                            \
} while (0)

#define fixbr(value,round) round_to_int(value,round)

void
set_rounding_mode(unsigned mode)
{
   register unsigned r asm("1") = mode;
   __asm__ volatile ( SFPC(1) : : "d"(r) );
}


int main(void)
{
   int j;
   static const long double dval[] = {
      1.25, 1.5, 2.5, 1.75, -1.25, -1.5, -2.5, -1.75, 0.0,
   };

   assert(sizeof(long double) == 16);
   
   /* f128 -> f128, round to int */
   for (j = 0; j < sizeof dval / sizeof dval[0]; ++j) {
      set_rounding_mode(FPC_BFP_ROUND_ZERO);
      fixbr(dval[j], M3_BFP_ROUND_NEAREST_EVEN);
      set_rounding_mode(FPC_BFP_ROUND_NEAREST_EVEN);
      fixbr(dval[j], M3_BFP_ROUND_ZERO);
      fixbr(dval[j], M3_BFP_ROUND_POSINF);
      fixbr(dval[j], M3_BFP_ROUND_NEGINF);
   }

   return 0;
}
