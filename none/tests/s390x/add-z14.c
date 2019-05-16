#include "add.h"

#define agh(x, y) ".insn rxy,0xe30000000038, " x ", " y "\n"

static void do_regmem_insns(unsigned long m2)
{
   memsweep(agh, m2, 0);
}

int main()
{
   for_each_m2(do_regmem_insns);
}
