#include <stdio.h>

int main ()
{
   long out [] = {0, 0};
   __asm__ volatile("cfc1       $a1,   $31"                 "\n\t"
                    "dli        $t0,   0x405ee0a3d70a3d71"  "\n\t"
                    "dmtc1      $t0,   $f0"                 "\n\t"
                    "ctc1       $zero, $31"                 "\n\t"
                    "round.w.d  $f0,   $f0"                 "\n\t"
                    "cfc1       $a2,   $31"                 "\n\t"
                    "sd         $a2,   0(%0)"               "\n\t"
                    "dli        $t0,   0x3ff0000000000000"  "\n\t"
                    "dmtc1      $t0,   $f0"                 "\n\t"
                    "ctc1       $zero, $31"                 "\n\t"
                    "round.w.d  $f0,   $f0"                 "\n\t"
                    "cfc1       $a2,   $31"                 "\n\t"
                    "sd         $a2,   8(%0)"               "\n\t"
                    "ctc1       $a1,   $31"                 "\n\t"
                    :
                    : "r" (out)
                    : "a1", "a2", "t0", "$f0"
                   );
   printf("FCSR::1: 0x%lx, 2: 0x%lx\n", out[0], out[1]);
   return 0;
}
