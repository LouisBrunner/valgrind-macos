#include <stdio.h>
#include "opcodes.h"

/* The FLOGR insn reads from register R2 and writes to register R1 and
   R1 + 1. So we need to distinguish three cases:

   (1) All three registers R1, R1 + 1, and R2 are distinct
   (2) R2 == R1
   (3) R2 == R1 + 1

   These are tested by flogr1, flogr2, and flogr3, respectively. */

/* Call FLOGR on INPUT. The results are returned through the parms. */

/* R2 != R1 && R2 != R1 + 1 */
void
flogr1(unsigned long input, unsigned long *bitpos, unsigned long *modval,
       unsigned int *cc)
{
   unsigned int psw;
   register unsigned long value asm("4") = input;

   asm volatile ( FLOGR(2,4)
                  "ipm   %[psw]\n\t"
                  "stg   2, %[bitpos]\n\t"
                  "stg   3, %[modval]\n\t"
                  : [bitpos]"=m"(*bitpos), [modval]"=m"(*modval),
                    [psw]"=d"(psw)
                  : [val] "d"(value)
                  : "2", "3", "cc");

   *cc = psw >> 28;
#if 0
   printf("value = %lx,  bitpos = %lu,  modval = %lx,  cc = %d\n",
          value, *bitpos, *modval, *cc);
#endif
}

/* R2 == R1 */
void
flogr2(unsigned long input, unsigned long *bitpos, unsigned long *modval,
       unsigned int *cc)
{
   unsigned int psw;
   register unsigned long value asm("2") = input;

   asm volatile ( FLOGR(2,2)
                  "ipm   %[psw]\n\t"
                  "stg   2, %[bitpos]\n\t"
                  "stg   3, %[modval]\n\t"
                  : [bitpos]"=m"(*bitpos), [modval]"=m"(*modval),
                    [psw]"=d"(psw), [val] "+d"(value)
                  :
                  : "3", "cc");

   *cc = psw >> 28;
#if 0
   printf("value = %lx,  bitpos = %lu,  modval = %lx,  cc = %d\n",
          value, *bitpos, *modval, *cc);
#endif
}

/* R2 == R1 + 1 */
void
flogr3(unsigned long input, unsigned long *bitpos, unsigned long *modval,
       unsigned int *cc)
{
   unsigned int psw;
   register unsigned long value asm("3") = input;

   asm volatile ( FLOGR(2,3)
                  "ipm   %[psw]\n\t"
                  "stg   2, %[bitpos]\n\t"
                  "stg   3, %[modval]\n\t"
                  : [bitpos]"=m"(*bitpos), [modval]"=m"(*modval),
                    [psw]"=d"(psw), [val] "+d"(value)
                  :
                  : "2", "cc");

   *cc = psw >> 28;
#if 0
   printf("value = %lx,  bitpos = %lu,  modval = %lx,  cc = %d\n",
          value, *bitpos, *modval, *cc);
#endif
}

void
runtest(void (*func)(unsigned long, unsigned long *, unsigned long *,
                     unsigned int *))
{
   unsigned long bitpos, modval, value;
   unsigned int cc;
   int i;

   /* Value 0 is special */
   value = 0;
   func(value, &bitpos, &modval, &cc);
   if (modval != 0)  fprintf(stderr, "modval is wrong for %lx\n", value);
   if (bitpos != 64) fprintf(stderr, "bitpos is wrong for %lx\n", value);
   if (cc != 0)      fprintf(stderr, "cc is wrong for %lx\n", value);

   /* Test with exactly 1 bit set */
   for (i = 0; i < 64; ++i) {
     value = 1ull << i;
     func(value, &bitpos, &modval, &cc);
     if (modval != 0) fprintf(stderr, "modval is wrong for %lx\n", value);
     if (bitpos != 63 - i) fprintf(stderr, "bitpos is wrong for %lx\n", value);
     if (cc != 2)          fprintf(stderr, "cc is wrong for %lx\n", value);
   }

   /* Test with all bits 1 right from first 1 bit */
   for (i = 1; i < 64; ++i) {
     value = 1ull << i;
     value = value | (value - 1);
     func(value, &bitpos, &modval, &cc);
     if (modval != (value >> 1)) fprintf(stderr, "modval is wrong for %lx\n", value);
     if (bitpos != 63 - i) fprintf(stderr, "bitpos is wrong for %lx\n", value);
     if (cc != 2)          fprintf(stderr, "cc is wrong for %lx\n", value);
   }
}


int main()
{
   runtest(flogr1);
   runtest(flogr2);
   runtest(flogr3);

   return 0;
}
