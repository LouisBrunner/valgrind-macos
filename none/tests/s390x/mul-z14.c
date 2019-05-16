#include "mul.h"

#define mg(x, y) ".insn rxy,0xe30000000084, " x ", " y "\n"
#define mgh(x, y) ".insn rxy,0xe3000000003c, " x ", " y "\n"
#define mgrk(x, y, z) ".insn rrf,0xb9ec0000, " x ", " y ", " z ", 0\n"
#define msc(x, y) ".insn rxy,0xe30000000053, " x ", " y "\n"
#define msrkc(x, y, z) ".insn rrf,0xb9fd0000, " x ", " y ", " z ", 0\n"
#define msgc(x, y) ".insn rxy,0xe30000000083, " x ", " y "\n"
#define msgrkc(x, y, z) ".insn rrf,0xb9ed0000, " x ", " y ", " z ", 0\n"

static void do_regmem_insns(unsigned long m2)
{
   memsweep(mg, m2);
   memsweep(mgh, m2);
   regregsweep(mgrk, m2);
   memsweep(msc, m2);
   regregsweep(msrkc, m2);
   memsweep(msgc, m2);
   regregsweep(msgrkc, m2);
}

int main()
{
   for_each_m2(do_regmem_insns);
}
