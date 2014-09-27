#include <stdio.h>

#if defined(__mips_hard_float)
#define TESTINST_CFC1_CTC1(N)          \
{                                      \
   unsigned int out = 0;               \
   __asm__ __volatile__(               \
      "move $t0, %1"   "\n\t"          \
      "ctc1 $t0, $31"  "\n\t"          \
      "cfc1 $t1, $31"  "\n\t"          \
      "move %0,  $t1"  "\n\t"          \
      : "=r" (out)                     \
      : "r" (N)                        \
      : "t0", "t1"                     \
   );                                  \
   printf("out=%d, in=%d\n", out, N);  \
}
#else
#define TESTINST_CFC1_CTC1(N)
#endif

int main()
{
   int i;
   printf("--- CTC1, CFC1 ---\n");
   for (i = 0; i < 1024; i++) {
      TESTINST_CFC1_CTC1(i);
   }

   return 0;
}
