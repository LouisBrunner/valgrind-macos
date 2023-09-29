#include <stdio.h>
#include "mul.h"

#define MSFI(x, y) ".insn rilu,0xc20100000000," x "," y "\n"
#define MSGFI(x, y) ".insn rilu,0xc20000000000," x "," y "\n"

static void do_imm_insns(void)
{
	ximmsweep(MSFI,  00000000, 00000000);
	ximmsweep(MSFI,  ffffffff, ffffffff);
	ximmsweep(MSFI,  ffffffff, ffff8000);
	ximmsweep(MSFI,  00000000, 00007fff);
	ximmsweep(MSFI,  00000000, 00007fff);
	ximmsweep(MSFI,  00000000, 00007fff);
	ximmsweep(MSFI,  ffffffff, 80000000);
	ximmsweep(MSFI,  00000000, 7fffffff);
	ximmsweep(MSGFI, 00000000, 00000000);
	ximmsweep(MSGFI, ffffffff, ffffffff);
	ximmsweep(MSGFI, ffffffff, ffff8000);
	ximmsweep(MSGFI, 00000000, 00007fff);
	ximmsweep(MSGFI, 00000000, 00007fff);
	ximmsweep(MSGFI, 00000000, 00007fff);
	ximmsweep(MSGFI, ffffffff, 80000000);
	ximmsweep(MSGFI, 00000000, 7fffffff);

}

#define mhy(x, y) ".insn rxy,0xe3000000007c," x "," y "\n"
#define mfy(x, y) ".insn rxy,0xe3000000005c," x "," y "\n"

static void do_regmem_insns(unsigned long m2)
{
	memsweep(mhy, m2);
	memsweep(mfy, m2);
}

int main()
{
	for_each_m2(do_regmem_insns);
	do_imm_insns();
	return 0;
}
