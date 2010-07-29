
#include <stdio.h>

typedef  unsigned long long int  ULong;
typedef  unsigned int            UInt;

__attribute__((noinline))
void do_lzcnt32 ( /*OUT*/UInt* flags, /*OUT*/UInt* res, UInt arg )
{
  UInt block[3] = { arg, 0, 0 };
  __asm__ __volatile__(
    "movl $0x55555555, %%esi" "\n\t"
    "lzcntl 0(%0), %%esi"     "\n\t"
    "movl %%esi, 4(%0)"       "\n\t"
    "pushfl"                  "\n\t"
    "popl %%esi"              "\n\t"
    "movl %%esi, 8(%0)"       "\n"
    : : "r"(&block[0]) : "esi","cc","memory"
  );
  *res = block[1];
  *flags = block[2] & 0x8d5;
}

__attribute__((noinline))
void do_lzcnt16 ( /*OUT*/UInt* flags, /*OUT*/UInt* res, UInt arg )
{
  UInt block[3] = { arg, 0, 0 };
  __asm__ __volatile__(
    "movl $0x55555555, %%esi" "\n\t"
    "lzcntw 0(%0), %%si"      "\n\t"
    "movl %%esi, 4(%0)"       "\n\t"
    "pushfl"                  "\n\t"
    "popl %%esi"              "\n\t"
    "movl %%esi, 8(%0)"       "\n"
    : : "r"(&block[0]) : "esi","cc","memory"
  );
  *res = block[1];
  *flags = block[2] & 0x8d5;
}

int main ( void )
{
   UInt w;

   w = 0xFEDC1928;
   while (1) {
      UInt res;
      UInt flags;
      do_lzcnt32(&flags, &res, w);
      printf("lzcntl %08x -> %08x %04x\n", w, res, flags);
      if (w == 0) break;
      w = ((w >> 2) | (w >> 1)) + (w / 17);
   }

   w = 0xFEDC1928;
   while (1) {
      UInt res;
      UInt flags;
      do_lzcnt16(&flags, &res, w);
      printf("lzcntw %08x -> %08x %04x\n", w, res, flags);
      if (w == 0) break;
      w = ((w >> 2) | (w >> 1)) + (w / 17);
   }

   return 0;
}
