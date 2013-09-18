#include <stdio.h>

int main ()
{
   int out [] = {0, 0};
   __asm__ volatile("cfc1       $a1,   $31"         "\n\t"
                    "li         $t0,   0xd70a3d71"  "\n\t"
                    "mtc1       $t0,   $f0"         "\n\t"
                    "li         $t0,   0x405ee0a3"  "\n\t"
                    "mtc1       $t0,   $f1"         "\n\t"
                    "ctc1       $zero, $31"         "\n\t"
                    "round.w.d  $f0,   $f0"         "\n\t"
                    "cfc1       $a2,   $31"         "\n\t"
                    "sw         $a2,   0(%0)"       "\n\t"
                    "li         $t0,   0x00000000"  "\n\t"
                    "mtc1       $t0,   $f0"         "\n\t"
                    "li         $t0,   0x3ff00000"  "\n\t"
                    "mtc1       $t0,   $f1"         "\n\t"
                    "ctc1       $zero, $31"         "\n\t"
                    "round.w.d  $f0,   $f0"         "\n\t"
                    "cfc1       $a2,   $31"         "\n\t"
                    "sw         $a2,   4(%0)"       "\n\t"
                    "ctc1       $a1,   $31"         "\n\t"
                    :
                    : "r" (out)
                    : "a1", "a2", "t0", "$f0", "$f1"
                   );
   printf("FCSR::1: 0x%x, 2: 0x%x\n", out[0], out[1]);
   return 0;
}
