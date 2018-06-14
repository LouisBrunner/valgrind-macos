#include <stdio.h>
#include "pub_core_basics.h"

/*
 * Bits 18 (NAN2008) and 19 (ABS2008) are preset by hardware and may differ
 * between platforms. Hence a macro to clear them before printing FCSR
 * values.
 */
#define FCSR_NAN2008 1 << 18
#define FCSR_ABS2008 1 << 19
#define FLAGS_RM_MASK 0xFFFFFFFF & ~(FCSR_ABS2008 | FCSR_NAN2008)
#define CLEAR_PRESETBITS_FCSR(fcsr) (fcsr & FLAGS_RM_MASK)

int main ()
{
#if defined(__mips_hard_float)
   RegWord out [] = {0, 0};
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
   printf("FCSR::1: 0x%" FMT_REGWORD "x, 2: 0x%" FMT_REGWORD "x\n",
          CLEAR_PRESETBITS_FCSR(out[0]), CLEAR_PRESETBITS_FCSR(out[1]));
#endif
   return 0;
}
