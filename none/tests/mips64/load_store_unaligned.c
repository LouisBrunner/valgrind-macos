#include <stdio.h>

#define N 16
#define SOLL 8 /* size of unsigned long long */

unsigned long long memSrc[] = {
   0x12345658121f1e1f, 0,          
   3,                  -1,
   0x232f2e2f56568441, 0x242c2b2b1236548c,
   0xffffffff252a2e2b, 0x262d2d2a4521dddd,
   0x3f343f3e22222222, 0x3e353d3c41231548,
   0x363a3c3b45421212, 0x3b373b3a4545afcb,
   0x454f4e4556984525, 0xfffffffffffffffc,
   0x474d474c55aaaaaa, 0x4a484a4c65665659
};

unsigned long long memDst[] = {
   0, 0, 0, 0,
   0, 0, 0, 0,
   0, 0, 0, 0,
   0, 0, 0, 0,
};

int main()
{
#if defined(__mips_hard_float)
   int i, index;
   unsigned long long outLoad;
#if (__mips_isa_rev < 6)
   for (i = 0; i < N * SOLL; i++) {
      outLoad = 0;
      __asm__ __volatile__(
         "move  $t0, %1"        "\n\t"
         "move  $t1, %2"        "\n\t"
         "luxc1 $f0, $t1($t0)"  "\n\t"
         "dmfc1 %0,  $f0"       "\n\t"
         "move  $t0, %3"        "\n\t"
         "suxc1 $f0, $t1($t0)"  "\n\t"
         : "=r" (outLoad)
         : "r" (memSrc), "r" (i), "r" (memDst)
         : "t0", "t1", "$f0"
      );
      index = (i / SOLL) % N;
      printf("i: %d, memSrc[%d]: 0x%llx, memDst[%d]: 0x%llx, outLoad: 0x%llx\n",
              i, index, memSrc[index], index, memDst[index], outLoad);
   }
#endif
#endif
   return 0;
}
