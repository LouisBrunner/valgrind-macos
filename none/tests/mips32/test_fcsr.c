#if defined(__mips_hard_float)

#include <stdio.h>

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
   printf("FCSR::1: 0x%x, 2: 0x%x\n", CLEAR_PRESETBITS_FCSR(out[0]),
                                      CLEAR_PRESETBITS_FCSR(out[1]));
   return 0;
}
#else
int main() {
   return 0;
}
#endif
