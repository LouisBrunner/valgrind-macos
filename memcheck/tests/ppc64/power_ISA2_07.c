#include <stdio.h>
#include <config.h>

double foo = -1.0;
double FRT1;
double FRT2;

#ifdef HAS_ISA_2_07

/* b0 may be non-zero in lwarx/ldarx Power6 instrs */
void test_reservation()
{

   unsigned long RT;
   unsigned long base;
   unsigned long offset;
   unsigned arrB[] = { 0x00112233U, 0x44556677U, 0x8899aabbU, 0xccddeeffU };
   int arrH[] __attribute__ ((aligned (2))) = { 0xdeadbeef, 0xbad0beef, 0xbeefdead, 0xbeef0bad };

   /* The lbarx and lharx instructions were "phased in" in ISA 2.06.  That
    * means it they may show up in some implementations but not others. They
    * are in all ISA 2.08 implementations.
    */
   base = (unsigned long) &arrB;
   offset = ((unsigned long) &arrB[1]) - base;
   __asm__ volatile ("ori 20, %0, 0"::"r" (base));
   __asm__ volatile ("ori 21, %0, 0"::"r" (offset));
   __asm__ volatile ("lbarx %0, 20, 21, 1":"=r" (RT));
   printf("lbarx => 0x%lx\n", RT);

   base = (unsigned long) &arrH;
   offset = ((unsigned long) &arrH[1]) - base;
   __asm__ volatile ("ori 20, %0, 0"::"r" (base));
   __asm__ volatile ("ori 21, %0, 0"::"r" (offset));
   __asm__ volatile ("lharx %0, 20, 21, 1":"=r" (RT));
   printf("lharx => 0x%lx\n", RT);
}
#endif

int main(void)
{
#ifdef HAS_ISA_2_07
   (void) test_reservation();
#endif

   return 0;
}
