#include <stdio.h>

#define N 15
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

int main()
{
   int i, index;
   unsigned long long outLoad;
   for (i = 8; i < N * SOLL ; i++) {
      outLoad = 0;
      __asm__ __volatile__(
         "move  $t0, %1"        "\n\t"
         "move  $t1, %2"        "\n\t"
         "daddu $t0, $t0, $t1"  "\n\t"
         "move  $t1, $zero"     "\n\t"
         "ldl   $t1, 0($t0)"    "\n\t"
         "ldr   $t1, 7($t0)"    "\n\t"
         "move  %0,  $t1"       "\n\t"
         : "=r" (outLoad)
         : "r" (memSrc), "r" (i)
         : "t0", "t1"
      );
      index = (i / SOLL) % N;
      printf("i: %d, memSrc[%d]: 0x%llx, outLoad: 0x%llx\n",
              i, index, memSrc[index], outLoad);
   }
   return 0;
}
