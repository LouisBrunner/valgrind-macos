#include <stdio.h>


/* Call FLOGR on INPUT. The results are returned through the parms. */
void
flogr(unsigned long input, unsigned long *bitpos, unsigned long *modval,
      unsigned int *cc)
{
   unsigned int psw;
   register unsigned long value asm("4") = input;

   asm volatile ( "flogr 2, %[val]\n\t"
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

void
runtest(void)
{
   unsigned long bitpos, modval, value;
   unsigned int cc;
   int i;

   /* Value 0 is special */
   value = 0;
   flogr(value, &bitpos, &modval, &cc);
   if (modval != 0)  fprintf(stderr, "modval is wrong for %lx\n", value);
   if (bitpos != 64) fprintf(stderr, "bitpos is wrong for %lx\n", value);
   if (cc != 0)      fprintf(stderr, "cc is wrong for %lx\n", value);

   /* Test with exactly 1 bit set */
   for (i = 0; i < 64; ++i) {
     value = 1ull << i;
     flogr(value, &bitpos, &modval, &cc);
     if (modval != 0) fprintf(stderr, "modval is wrong for %lx\n", value);
     if (bitpos != 63 - i) fprintf(stderr, "bitpos is wrong for %lx\n", value);
     if (cc != 2)          fprintf(stderr, "cc is wrong for %lx\n", value);
   }

   /* Test with all bits 1 right from first 1 bit */
   for (i = 1; i < 64; ++i) {
     value = 1ull << i;
     value = value | (value - 1);
     flogr(value, &bitpos, &modval, &cc);
     if (modval != (value >> 1)) fprintf(stderr, "modval is wrong for %lx\n", value);
     if (bitpos != 63 - i) fprintf(stderr, "bitpos is wrong for %lx\n", value);
     if (cc != 2)          fprintf(stderr, "cc is wrong for %lx\n", value);
   }
}


int main()
{
   runtest();

   return 0;
}
