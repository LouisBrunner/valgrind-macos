
// This should run on memcheck without reporting an undef-value error.
// See https://bugzilla.redhat.com/show_bug.cgi?id=2257546

#include <stdio.h>
#if defined(__APPLE__)
#include <stdlib.h>
#else
#include <malloc.h>
#endif

int main ( void ) 
{
   char* c1 = malloc(32);
   c1[0] = 'x'; c1[1] = 'y'; c1[2] = 'x'; c1[3] = 0;

   char* c2 = "foobarxyzzyfoobarzyzzyandawholelotmoretoo";  // strlen > 32

   long long int res;
   __asm__ __volatile__(
   "vmovdqu  (%1),   %%ymm4"           "\n\t"
   "vmovdqu  (%2),   %%ymm5"           "\n\t"
   "vpxor    %%ymm4, %%ymm5, %%ymm5"   "\n\t"
   "vptest   %%ymm5, %%ymm5"           "\n\t"
   "je       zzz1f"                    "\n\t"
   "mov      $99, %0"                  "\n\t"
   "jmp      zzzafter"                 "\n"
   "zzz1f:"                            "\n\t"
   "mov $88, %0"                       "\n"
   "zzzafter:"                         "\n\t"
   : /*OUT*/"=r"(res) : /*IN*/"r"(c1),"r"(c2) : /*TRASH*/"xmm4","xmm5","cc"
   );
   printf("res = %lld\n", res);
   free(c1);
   return 0;
}
