#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include "opcodes.h"

#define srnmt(b,d) \
   ({ \
      __asm__ volatile ( "lghi 8," #b "\n\t" \
                         SRNMT(8,d) \
                         ::: "8"); \
   })


/* Like srnmt above, except it uses r0 as a base register */
#define srnmt0(d) \
   ({ \
      __asm__ volatile ( SRNMT(0,d) \
                         ::: "0"); \
   })

unsigned
get_dfp_rounding_mode(void)
{
   unsigned fpc;

   __asm__ volatile ("stfpc  %0\n\t" : "=m"(fpc));

   return (fpc & 0x70) >> 4;
}

int main(void)
{
   printf("initial rounding mode = %u\n", get_dfp_rounding_mode());

   /* Set basic rounding modes in various ways */
   srnmt(1,002);  // 1 + 2 = 3
   printf("rounding mode = %u\n", get_dfp_rounding_mode());

   srnmt(2,000);
   printf("rounding mode = %u\n", get_dfp_rounding_mode());

   srnmt(0,001);
   printf("rounding mode = %u\n", get_dfp_rounding_mode());

   srnmt(0,000);
   printf("rounding mode = %u\n", get_dfp_rounding_mode());

   srnmt(7,000);
   printf("rounding mode = %u\n", get_dfp_rounding_mode());

   srnmt(0,006);
   printf("rounding mode = %u\n", get_dfp_rounding_mode());

   srnmt0(005);
   printf("rounding mode = %u\n", get_dfp_rounding_mode());

   srnmt0(004);
   printf("rounding mode = %u\n", get_dfp_rounding_mode());

   return 0;
}
