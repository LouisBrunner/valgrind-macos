#include <stdio.h>

union foo {
   char a;
   unsigned long val;
};

static int do_tmhh(unsigned long val, int eq, int neq)
{
   int ret = eq;
   int alt = neq;
   __asm__("tmhh    %[val],0xf000\n\t"
           "jo      1f\n\t"
           "nopr    0\n"
           "1:\t"
           "locgrne %[ret],%[alt]"
           : [ret] "+d"(ret)
           : [val] "d"(val), [alt] "d"(alt)
           : "cc");
   return ret;
}

static int do_tm(unsigned char val, int eq, int neq)
{
   int ret = eq;
   int alt = neq;
   __asm__("tm      %[val],0xf0\n\t"
           "jo      1f\n\t"
           "nopr    0\n"
           "1:\t"
           "locgrne %[ret],%[alt]"
           : [ret] "+d"(ret)
           : [val] "Q"(val), [alt] "d"(alt)
           : "cc");
   return ret;
}

int main(void)
{
   volatile union foo a;
   a.a = 0x40;
   printf("%c\n", do_tmhh(a.val, 'A', 'B'));
   a.val = a.val << 1;
   printf("%c\n", do_tmhh(a.val, 'C', 'D'));
   printf("%c\n", do_tm(a.a, 'E', 'F'));
   a.val = a.val << 1;
   printf("%c\n", do_tmhh(a.val, 'G', 'H'));
   printf("%c\n", do_tm(a.a, 'I', 'J'));
   return 0;
}
