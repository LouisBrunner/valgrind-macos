
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "tests/malloc.h"

typedef  unsigned char           UChar;
typedef  unsigned int            UInt;
typedef  unsigned long int       UWord;
typedef  unsigned long long int  ULong;

// What can we actually test here?  The instructions take no input and
// produce output which is by definition totally random.  So apart from
// not simply failing insn decode, there's nothing much to test.

// Get 10 values of each size, and check that they are not all the same
// (otherwise something's obviously wrong).  Now, statistically, it's
// highly unlikely that they are all the same.  For 10 16 bit ints, the
// probability of them being all the same is (I'd guess) (2^-16) ^ (10-1),
// that is, 2^-144.

ULong do_rdseed64 ( void )
{
   while (1) {
      ULong res = 0;
      ULong cflag = 0;
      __asm__ __volatile__(
         "movabsq $0x5555555555555555, %%r11 ; "
         "movq $0, %%r12 ; "
         "rdseed %%r11 ; "
         "setc %%r12b ; "
         "movq %%r11, %0 ; "
         "movq %%r12, %1"
         : "=r"(res), "=r"(cflag) : : "r11", "r12"
      );
      if (cflag == 1)
         return res;
   }
   /*NOTREACHED*/
}

ULong do_rdseed32 ( void )
{
   while (1) {
      ULong res = 0;
      ULong cflag = 0;
      __asm__ __volatile__(
         "movabsq $0x5555555555555555, %%r11 ; "
         "movq $0, %%r12 ; "
         "rdseed %%r11d ; "
         "setc %%r12b ; "
         "movq %%r11, %0 ; "
         "movq %%r12, %1"
         : "=r"(res), "=r"(cflag) : : "r11", "r12"
      );
      if (cflag == 1)
         return res;
   }
   /*NOTREACHED*/
}

ULong do_rdseed16 ( void )
{
   while (1) {
      ULong res = 0;
      ULong cflag = 0;
      __asm__ __volatile__(
         "movabsq $0x5555555555555555, %%r11 ; "
         "movq $0, %%r12 ; "
         "rdseed %%r11w ; "
         "setc %%r12b ; "
         "movq %%r11, %0 ; "
         "movq %%r12, %1"
         : "=r"(res), "=r"(cflag) : : "r11", "r12"
      );
      if (cflag == 1)
         return res;
   }
   /*NOTREACHED*/
}

void do_test ( ULong(*fn)(void),
               ULong mask
               /* with 1s indicating the random bits in the result */ )
{
   ULong arr[10];
   for (UInt i = 0; i < 10; i++) {
      arr[i] = fn();
   }

   // They really should all be different (to an extremely high probabilty.
   // See comment above.
   int allSame = 1/*true*/; // really, a Bool
   for (UInt i = 1; i < 10; i++) {
      if (arr[i] != arr[0]) {
         allSame = 0/*false*/;
         break;
      }
   }
   assert(!allSame);

   // The 0/32/48 leading bits of the result should have a particular value,
   // depending on the insn.  So print them, with the random part masked out.
   for (UInt i = 0; i < 10; i++) {
      printf("0x%016llx\n", arr[i] & ~mask);
   }
   printf("\n");
}

int main ( void )
{
   do_test( do_rdseed64, 0xFFFFFFFFFFFFFFFFULL );
   do_test( do_rdseed32, 0x00000000FFFFFFFFULL );
   do_test( do_rdseed16, 0x000000000000FFFFULL );
   return 0;
}
