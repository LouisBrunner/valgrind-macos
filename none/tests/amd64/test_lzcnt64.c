
#include <stdio.h>

typedef  unsigned long long int  ULong;
typedef  unsigned int            UInt;

__attribute__((noinline))
void do_lzcnt64 ( /*OUT*/UInt* flags, /*OUT*/ULong* res, ULong arg )
{
  ULong block[3] = { arg, 0ULL, 0ULL };
  __asm__ __volatile__(
    "movabsq $0x5555555555555555, %%r11" "\n\t"
    "lzcntq 0(%0), %%r11"     "\n\t"
    "movq %%r11, 8(%0)"       "\n\t"
    "pushfq"                  "\n\t"
    "popq %%r11"              "\n\t"
    "movq %%r11, 16(%0)"      "\n"
    : : "r"(&block[0]) : "r11","cc","memory"
  );
  *res = block[1];
  *flags = block[2] & 0x8d5;
}

__attribute__((noinline))
void do_lzcnt32 ( /*OUT*/UInt* flags, /*OUT*/ULong* res, ULong arg )
{
  ULong block[3] = { arg, 0ULL, 0ULL };
  __asm__ __volatile__(
    "movabsq $0x5555555555555555, %%r11" "\n\t"
    "lzcntl 0(%0), %%r11d"    "\n\t"
    "movq %%r11, 8(%0)"       "\n\t"
    "pushfq"                  "\n\t"
    "popq %%r11"              "\n\t"
    "movq %%r11, 16(%0)"      "\n"
    : : "r"(&block[0]) : "r11","cc","memory"
  );
  *res = block[1];
  *flags = block[2] & 0x8d5;
}

__attribute__((noinline))
void do_lzcnt16 ( /*OUT*/UInt* flags, /*OUT*/ULong* res, ULong arg )
{
  ULong block[3] = { arg, 0ULL, 0ULL };
  __asm__ __volatile__(
    "movabsq $0x5555555555555555, %%r11" "\n\t"
    "lzcntw 0(%0), %%r11w"    "\n\t"
    "movq %%r11, 8(%0)"       "\n\t"
    "pushfq"                  "\n\t"
    "popq %%r11"              "\n\t"
    "movq %%r11, 16(%0)"      "\n"
    : : "r"(&block[0]) : "r11","cc","memory"
  );
  *res = block[1];
  *flags = block[2] & 0x8d5;
}

int main ( void )
{
   ULong w;

   w = 0xFEDC192837475675ULL;
   while (1) {
      ULong res;
      UInt  flags;
      do_lzcnt64(&flags, &res, w);
      printf("lzcntq %016llx -> %016llx %04x\n", w, res, flags);
      if (w == 0) break;
      w = ((w >> 2) | (w >> 1)) + (w / 17ULL);
   }

   w = 0xFEDC192837475675ULL;
   while (1) {
      ULong res;
      UInt  flags;
      do_lzcnt32(&flags, &res, w);
      printf("lzcntl %016llx -> %016llx %04x\n", w, res, flags);
      if (w == 0) break;
      w = ((w >> 2) | (w >> 1)) + (w / 17ULL);
   }

   w = 0xFEDC192837475675ULL;
   while (1) {
      ULong res;
      UInt  flags;
      do_lzcnt16(&flags, &res, w);
      printf("lzcntw %016llx -> %016llx %04x\n", w, res, flags);
      if (w == 0) break;
      w = ((w >> 2) | (w >> 1)) + (w / 17ULL);
   }

   return 0;
}
