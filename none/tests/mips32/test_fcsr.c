#include <stdio.h>

int main ()
{
   int out [] = {0, 0};
   unsigned long long in  [] = {0x405EE0A3D70A3D71ULL, 0x3FF0000000000000ULL};
   __asm__ volatile("cfc1       $a1,   $31"         "\n\t"
                    "ldc1       $f0,   0(%0)"       "\n\t"
                    "ctc1       $zero, $31"         "\n\t"
                    "round.w.d  $f0,   $f0"         "\n\t"
                    "cfc1       $a2,   $31"         "\n\t"
                    "sw         $a2,   0(%1)"       "\n\t"
                    "ldc1       $f0,   8(%0)"       "\n\t"
                    "ctc1       $zero, $31"         "\n\t"
                    "round.w.d  $f0,   $f0"         "\n\t"
                    "cfc1       $a2,   $31"         "\n\t"
                    "sw         $a2,   4(%1)"       "\n\t"
                    "ctc1       $a1,   $31"         "\n\t"
                    :
                    : "r" (in), "r" (out)
                    : "a1", "a2", "t0", "$f0", "$f1"
                   );
   printf("FCSR::1: 0x%x, 2: 0x%x\n", out[0], out[1]);
   return 0;
}
