#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include "opcodes.h"
#include "rounding.h"

/* Instructions modifying / querying the FPC register:
   EFPC, LFPC, SFPC, STFPC, SRNM, SRNMB.

   No tests for SFASR and LFAS asa they are not implemented */

void
sfpc(unsigned mode)
{
   register unsigned r asm("1") = mode;
   __asm__ volatile ( SFPC(1) : : "d"(r) );
}

unsigned
get_rounding_mode(void)
{
   unsigned fpc1;
   uint64_t fpc2 = ~(uint64_t)0;

   __asm__ volatile ("stfpc  %0" : "=Q"(fpc1));
   __asm__ volatile ("efpc   %0" : "+d"(fpc2));

   assert((fpc1 & 0x7) == (fpc2 & 0x7));
   assert((fpc2 >> 32) == 0xFFFFFFFF);

   return fpc1 & 0x7;
}

#define srnm(b,d)                                         \
   ({                                                     \
      __asm__ volatile ("srnm  %[displ] (%[base])\n\t"    \
                        : : [base]"a"(b), [displ]"L"(d)); \
   })

#define srnmb(b,d)                                        \
   ({                                                     \
      __asm__ volatile ("srnmb  %[displ] (%[base])\n\t"   \
                        : : [base]"a"(b), [displ]"L"(d)); \
   })

int main(void)
{
   /* Order of rounding modes is such that the first rounding mode in
      the list differs from the default rounding mode which is
      FPC_BFP_ROUND_NEAREST_EVEN. */
   const unsigned rmodes[] = {
      FPC_BFP_ROUND_ZERO,
      FPC_BFP_ROUND_POSINF,
      FPC_BFP_ROUND_NEGINF,
      FPC_BFP_ROUND_PREPARE_SHORT,
      FPC_BFP_ROUND_NEAREST_EVEN,
   };

   int initial = get_rounding_mode();
   assert(initial == FPC_BFP_ROUND_NEAREST_EVEN);

   printf("initial rounding mode: %u\n", initial);

   printf("Setting FPC rounding mode via SFPC\n");
   for (int i = 0; i < sizeof rmodes / sizeof rmodes[0]; ++i) {
      printf("setting rounding mode to %u\n", rmodes[i]);
      sfpc(rmodes[i]);
      printf("...checking: %u\n", get_rounding_mode());
   }

   putchar('\n');
   printf("Setting FPC rounding mode via SRNM\n");
   /* Only rounding modes 0,1,2,3 are valid for SRNM */

   /* Rounding mode --> base register */
   printf(".... using base register\n");
   printf("setting rounding mode to %u\n", 0);
   srnm(0, 0);
   printf("...checking: %u\n", get_rounding_mode());
   printf("setting rounding mode to %u\n", 1);
   srnm(1, 0);
   printf("...checking: %u\n", get_rounding_mode());
   printf("setting rounding mode to %u\n", 2);
   srnm(2, 0);
   printf("...checking: %u\n", get_rounding_mode());
   printf("setting rounding mode to %u\n", 3);
   srnm(3, 0);
   printf("...checking: %u\n", get_rounding_mode());

   /* Rounding mode --> displacement */
   printf(".... using displacement\n");
   printf("setting rounding mode to %u\n", 0);
   srnm(0, 0);
   printf("...checking: %u\n", get_rounding_mode());
   printf("setting rounding mode to %u\n", 1);
   srnm(0, 1);
   printf("...checking: %u\n", get_rounding_mode());
   printf("setting rounding mode to %u\n", 2);
   srnm(0, 2);
   printf("...checking: %u\n", get_rounding_mode());
   printf("setting rounding mode to %u\n", 3);
   srnm(0, 3);
   printf("...checking: %u\n", get_rounding_mode());

   putchar('\n');
   printf("Setting FPC rounding mode via SRNMB\n");
   /* All rounding modes allowed */

   /* Rounding mode --> base register */
   printf(".... using base register\n");
   printf("setting rounding mode to %u\n", 0);
   srnmb(0, 0);
   printf("...checking: %u\n", get_rounding_mode());
   printf("setting rounding mode to %u\n", 1);
   srnmb(1, 0);
   printf("...checking: %u\n", get_rounding_mode());
   printf("setting rounding mode to %u\n", 2);
   srnmb(2, 0);
   printf("...checking: %u\n", get_rounding_mode());
   printf("setting rounding mode to %u\n", 3);
   srnmb(3, 0);
   printf("...checking: %u\n", get_rounding_mode());
   printf("setting rounding mode to %u\n", 7);
   srnmb(7, 0);
   printf("...checking: %u\n", get_rounding_mode());

   /* Rounding mode --> displacement */
   printf(".... using displacement\n");
   printf("setting rounding mode to %u\n", 0);
   srnmb(0, 0);
   printf("...checking: %u\n", get_rounding_mode());
   printf("setting rounding mode to %u\n", 1);
   srnmb(0, 1);
   printf("...checking: %u\n", get_rounding_mode());
   printf("setting rounding mode to %u\n", 2);
   srnmb(0, 2);
   printf("...checking: %u\n", get_rounding_mode());
   printf("setting rounding mode to %u\n", 3);
   srnmb(0, 3);
   printf("...checking: %u\n", get_rounding_mode());
   printf("setting rounding mode to %u\n", 7);
   srnmb(0, 7);
   printf("...checking: %u\n", get_rounding_mode());

   putchar('\n');
   printf("SRNM specific checks\n");
   /* Making sure base reg and displacement are added. */
   srnm(1,2);  // 1 + 2 = 3
   printf("rounding mode = %u\n", get_rounding_mode());
   /* Making sure extra bits get masked away */
   srnm(0,0xfff);  /* 3 */
   printf("rounding mode = %u\n", get_rounding_mode());
   srnm(0xfff,0);  /* 3 */
   printf("rounding mode = %u\n", get_rounding_mode());
   srnm(0xfff,0xfff);  /* 3 */
   printf("rounding mode = %u\n", get_rounding_mode());
   srnm(3,3);      /* 2 */
   printf("rounding mode = %u\n", get_rounding_mode());

   putchar('\n');
   printf("SRNMB specific checks\n");
   /* Making sure base reg and displacement are added. */
   srnmb(3,4);  // 3 + 4 = 7
   printf("rounding mode = %u\n", get_rounding_mode());

   putchar('\n');
   printf("LFPC\n");
   unsigned fpcval;
   
   sfpc(3);                                       /* Set rounding mode to 3 */
   printf("rounding mode = %u\n", get_rounding_mode());
   __asm__ volatile ("stfpc %0" : "=Q"(fpcval));  /* Store FPC -> fpcval; */
   fpcval = (fpcval & ~0x7) | 0x2;                /* modify rounding mode bits */
   __asm__ volatile ("lfpc  %0" : "=Q"(fpcval));  
   printf("rounding mode = %u\n", get_rounding_mode());
   
   return 0;
}
