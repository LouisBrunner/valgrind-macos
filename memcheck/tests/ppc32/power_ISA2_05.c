#include <stdio.h>

double foo = -1.0;
double FRT1;
double FRT2;
int base256(int val)
{
/* interpret the  bitstream representing val as a base 256 number for testing
 * the parity instrs
 */
   int sum = 0;
   int scale = 1;
   int i;

   for (i = 0; i < 8; i++) {
      int bit = val & 1;
      sum = sum + bit * scale;
      val <<= 1;
      scale *= 256;
   }
   return sum;
}

void test_parity_instrs()
{
   unsigned long long_word;
   unsigned int word;
   int i, parity;

   for (i = 0; i < 50; i++) {
      word = base256(i);
      long_word = word;
      __asm__ volatile ("prtyd %0, %1":"=r" (parity):"r"(long_word));
      printf("prtyd (%x) => parity=%x\n", i, parity);
      __asm__ volatile ("prtyw %0, %1":"=r" (parity):"r"(word));
      printf("prtyw (%x) => parity=%x\n", i, parity);
   }
}

void test_lfiwax()
{
   unsigned long base;
   //   unsigned long offset;

   typedef struct {
      unsigned int hi;
      unsigned int lo;
   } int_pair_t;

   int_pair_t *ip;
   foo = -1024.0;
   base = (unsigned long) &foo;

   __asm__ volatile ("lfiwax %0, 0, %1":"=f" (FRT1):"r"(base));
   ip = (int_pair_t *) & FRT1;
   printf("lfiwax (%f) => FRT=(%x, %x)\n", foo, ip->hi, ip->lo);


}



/* lfdp FPp, DS(RA) : load float double pair
** FPp	= leftmost 64 bits stored at DS(RA)
** FPp+1= rightmost 64 bits stored at DS(RA)
** FPp must be an even float register
*/
void test_double_pair_instrs()
{
   typedef struct {
      double hi;
      double lo;
   } dbl_pair_t;

   /* the following decls are for alignment */
   int i;
   dbl_pair_t dbl_pair[3];      /* must be quad word aligned */
   unsigned long base;
   unsigned long offset;

   for (i = 0; i < 3; i++) {
      dbl_pair[i].hi = -1024.0 + i;
      dbl_pair[i].lo = 1024.0 + i + 1;
   }

   __asm__ volatile ("lfdp 10, %0"::"m" (dbl_pair[0]));
   __asm__ volatile ("fmr %0, 10":"=f" (FRT1));
   __asm__ volatile ("fmr %0, 11":"=f" (FRT2));
   printf("lfdp (%f, %f) => F_hi=%f, F_lo=%f\n",
          dbl_pair[0].hi, dbl_pair[0].lo, FRT1, FRT2);


   FRT1 = 2.2048;
   FRT2 = -4.1024;
   __asm__ volatile ("fmr 10, %0"::"f" (FRT1));
   __asm__ volatile ("fmr 11, %0"::"f" (FRT2));
   __asm__ volatile ("stfdp 10, %0"::"m" (dbl_pair[1]));
   printf("stfdp (%f, %f) => F_hi=%f, F_lo=%f\n",
          FRT1, FRT2, dbl_pair[1].hi, dbl_pair[1].lo);

   FRT1 = 0.0;
   FRT2 = -1.0;
   base = (unsigned long) &dbl_pair;
   offset = (unsigned long) &dbl_pair[1] - base;
   __asm__ volatile ("or 20, 0, %0"::"r" (base));
   __asm__ volatile ("or 21, 0, %0"::"r" (offset));
   __asm__ volatile ("lfdpx 10, 20, 21");
   __asm__ volatile ("fmr %0, 10":"=f" (FRT1));
   __asm__ volatile ("fmr %0, 11":"=f" (FRT2));
   printf("lfdpx (%f, %f) => F_hi=%f, F_lo=%f\n",
          dbl_pair[1].hi, dbl_pair[1].lo, FRT1, FRT2);

   FRT1 = 8.2048;
   FRT2 = -16.1024;
   base = (unsigned long) &dbl_pair;
   offset = (unsigned long) &dbl_pair[2] - base;
   __asm__ volatile ("or 20, 0, %0"::"r" (base));
   __asm__ volatile ("or 21, 0, %0"::"r" (offset));
   __asm__ volatile ("fmr %0, 10":"=f" (FRT1));
   __asm__ volatile ("fmr %0, 11":"=f" (FRT2));
   __asm__ volatile ("stfdpx 10, 20, 21");
   printf("stfdpx (%f, %f) => F_hi=%f, F_lo=%f\n",
          FRT1, FRT2, dbl_pair[2].hi, dbl_pair[2].lo);
}


/* The contents of FRB with bit set 0 set to bit 0 of FRA copied into FRT */
void test_fcpsgn()
{
   double A[] = {
      10.101010,
      -0.0,
      0.0,
      -10.101010
   };

   double B[] = {
      11.111111,
      -0.0,
      0.0,
      -11.111111
   };

   double FRT, FRA, FRB;
   int i, j;

   for (i = 0; i < 4; i++) {
      FRA = A[i];
      for (j = 0; j < 4; j++) {
         FRB = B[j];
         __asm__ volatile ("fcpsgn %0, %1, %2":"=f" (FRT):"f"(FRA),
                           "f"(FRB));
         printf("fcpsgn sign=%f, base=%f => %f\n", FRA, FRB, FRT);
      }
   }
}

/* b0 may be non-zero in lwarx/ldarx Power6 instrs */
void test_reservation()
{

   int RT;
   unsigned long base;
   unsigned long offset;
   long arr[4] = { 0xdeadbeef, 0xbad0beef, 0xbeefdead, 0xbeef0bad };


   base = (unsigned long) &arr;
   offset = (unsigned long) &arr[1] - base;
   __asm__ volatile ("or 20, 0, %0"::"r" (base));
   __asm__ volatile ("or 21, 0, %0"::"r" (offset));
   __asm__ volatile ("lwarx %0, 20, 21, 1":"=r" (RT));
   printf("lwarx => %x\n", RT);

#ifdef __powerpc64__
   offset = (unsigned long) &arr[1] - base;
   __asm__ volatile ("or 21, 0, %0"::"r" (offset));
   __asm__ volatile ("ldarx %0, 20, 21, 1":"=r" (RT));
   printf("ldarx => %x\n", RT);
#endif

}

int main(void)
{
   (void) test_reservation();
   test_fcpsgn();
   (void) test_double_pair_instrs();
   test_lfiwax();
   test_parity_instrs();
   return 0;
}
