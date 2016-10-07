#include <stdio.h>
#include <stdint.h>
#include "opcodes.h"

#define BRASLCLOBBER "0","1","2","3","4","5","14",      \
      "f0","f1","f2","f3","f4","f5","f6","f7","memory","cc"

void inner_iter() { putchar('.'); }
void outer_iter() { putchar('+'); }

static void
check_brcth(int m, int n)
{
   unsigned long cnt = ((unsigned long) m << 32) | n;

   printf("brcth %d x %d (cnt=0x%lx): ", m, n, cnt);
   fflush(stdout);
   asm volatile("1: lr %[cnt],%[n]\n\t"
                "2: brasl 14,inner_iter\n\t"
                "brct %[cnt],2b\n\t"
                "brasl 14,outer_iter\n\t"
                ".insn ril,0xcc0600000000,%[cnt],1b\n\t" // BRCTH
                : [cnt]"+r" (cnt)
                : [n]"r" (n)
                : BRASLCLOBBER);
   printf(" (cnt=0x%lx)\n", cnt);
}

#define DO_RISBXG(insn, _r1, _r2, i3, i4, i5)                           \
   ({                                                                   \
      register unsigned long r1 asm ("1") = _r1;                        \
      register unsigned long r2 asm ("2") = _r2;                        \
      asm volatile(insn(1,2, i3, i4, i5)                                \
                   : "+d" (r1)                                          \
                   : "d" (r1), "d" (r2));                               \
      printf(#insn " r1(==%16.16lx),r2(==%16.16lx),0x" #i3 ",0x" #i4    \
             ",0x" #i5 " = %16.16lx\n", _r1, _r2, r1);                  \
   })

/* Some "random" 64-bit numbers. */

#define VAL_A 0x3a41e0a2afde1559
#define VAL_B 0x765487c11cd04ac4
#define VAL_C 0xb9cd1bdd399bef32
#define VAL_D 0xf8efadb884334ddd

static void
check_risbxg(void)
{
   /* Full copy low -> low */
   DO_RISBXG(RISBLG, VAL_A, VAL_B, 00, 1f, 00);
   DO_RISBXG(RISBLG, VAL_B, VAL_A, 00, 9f, 00);
   /* Full copy high -> low */
   DO_RISBXG(RISBLG, VAL_B, VAL_A, 00, 1f, 20);
   DO_RISBXG(RISBLG, VAL_A, VAL_B, 00, 9f, 20);
   /* Full copy high -> high */
   DO_RISBXG(RISBHG, VAL_C, VAL_D, 00, 1f, 00);
   DO_RISBXG(RISBHG, VAL_D, VAL_C, 00, 9f, 00);
   /* Full copy low -> high */
   DO_RISBXG(RISBHG, VAL_D, VAL_C, 00, 1f, 20);
   DO_RISBXG(RISBHG, VAL_C, VAL_D, 00, 9f, 20);
   /* Middle copy */
   DO_RISBXG(RISBLG, VAL_C, VAL_A, 0b, 13, 05);
   DO_RISBXG(RISBLG, VAL_C, VAL_A, 0b, 93, 05);
   DO_RISBXG(RISBHG, VAL_A, VAL_C, 03, 0c, 25);
   DO_RISBXG(RISBHG, VAL_A, VAL_C, 03, 8c, 25);
   /* "Outer" copy (start > end) */
   DO_RISBXG(RISBLG, VAL_B, VAL_D, 1d, 07, 0c);
   DO_RISBXG(RISBLG, VAL_B, VAL_D, 1d, 87, 0c);
   DO_RISBXG(RISBHG, VAL_D, VAL_B, 0c, 03, 25);
   DO_RISBXG(RISBHG, VAL_D, VAL_B, 0c, 83, 25);
}

/* Test high-word facility instructions. */

int main()
{
   check_brcth(5, 3);
   check_brcth(1, 30);
   check_brcth(16, 1);

   check_risbxg();
   return 0;
}
