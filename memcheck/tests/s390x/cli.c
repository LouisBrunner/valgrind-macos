#include <stdlib.h>

volatile unsigned long tmp;
static const char      use_idx[] = "01234567890abcdefghijklmnopqrstuvwxyz";

static void depend_on(int val) { tmp = use_idx[val]; }

static void pretend_write(unsigned long* val) { __asm__("" : "=m"(*val) : :); }

static void do_compares(unsigned long value)
{
   unsigned char val1 = value & 0xff;
   int result = 0;

   __asm__("cli  %[val],0x20\n\t"
           "jl   1f\n\t"
           "lghi %[res],1\n\t"
           "1:   nopr 0"
           : [res] "+d"(result)
           : [val] "Q"(val1)
           :);
   depend_on(result);

   __asm__("cli  %[val],0x40\n\t"
           "jnl  1f\n\t"
           "lghi %[res],2\n\t"
           "1:   nopr 0"
           : [res] "+d"(result)
           : [val] "Q"(val1)
           :);
   depend_on(result);
}

int main()
{
   unsigned long* buf = malloc(sizeof(unsigned long));
   pretend_write(buf);
   do_compares(*buf & 0x3f);
   free(buf);
   return 0;
}
