#include <stdio.h>
#include <stdint.h>


static void check_popcnt(uint64_t in, uint64_t expected_result,
                         int expected_cc)
{
   uint64_t out = ~expected_result;
   int cc = ~expected_cc;

   asm volatile(".insn rre, 0xb9e10000, %[out], %[in]\n\t"
                "ipm     %[cc]\n\t"
                "srl     %[cc],28\n\t"
                : [cc]"=d" (cc), [out]"=d" (out)
                : [in]"d" (in)
                : "cc");
   printf("popcnt %16lx -> %16lx %s  cc=%d %s\n",
          in, out, (out == expected_result ? "   " : "ERR"),
          cc, (cc == expected_cc ? "   " : "ERR"));
}

int main()
{
   check_popcnt(0, 0, 0);
   check_popcnt(1, 1, 1);
   check_popcnt(0x8000000000000000ULL, 0x0100000000000000ULL, 1);
   check_popcnt(0xffffffffffffffffULL, 0x0808080808080808ULL, 1);
   check_popcnt(0xff427e3800556bcdULL, 0x0802060300040505ULL, 1);
   return 0;
}
