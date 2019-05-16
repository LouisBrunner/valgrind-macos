#include "sub.h"

#define sgh(x, y) ".insn rxy,0xe30000000039, " x ", " y "\n"

static void do_regmem_insns(unsigned long m2)
{
   memsweep(sgh, m2, 0);
}

int main()
{
   for_each_m2(do_regmem_insns);
}
