
#include <stdio.h>


#define COMPILER_BARRIER  __asm__ __volatile("":::"cc","memory")

/* force the kernel to map in 15k below SP, so we can safely poke
   around there. */
__attribute__((noinline)) void make_below_sp_safe ( void )
{
   const int N = 15000;
   unsigned char a[N];
   int i;

   for (i = 0; i < N; i++) {
      a[i] = i & 0xFF;
   }

   COMPILER_BARRIER;

   unsigned int r = 0;
   for (i = 0; i < N; i++) {
      r = (r << 1) | (r >> 31);
      r ^= (unsigned int)a[i];
   }
   fprintf(stderr, "Checksum: %08x\n", r);
}


int main ( void )
{
   make_below_sp_safe();

   unsigned int res;
   __asm__ __volatile__("movl -8192(%%rsp), %0"
                        : "=r"(res) : : "memory","cc");
   fprintf(stderr, "Got %08x\n", res);
   return 0;
}
