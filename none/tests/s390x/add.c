#include <stdio.h>
#include "add.h"
#include "opcodes.h"

static void do_imm_insns(void)
{
	immsweep(ahi, 0, 0);
	immsweep(ahi, -1, 0);
	immsweep(ahi, -32768, 0);
	immsweep(ahi, 32767, 0);
	immsweep(aghi, 0, 0);
	immsweep(aghi, -1, 0);
	immsweep(aghi, -32768, 0);
	immsweep(aghi, 32767, 0);
}


static void do_regmem_insns(unsigned long s2)
{
	memsweep(a, s2, 0);
	memsweep(ah, s2, 0);
	memsweep(ag, s2, 0);
	memsweep(agf, s2, 0);
	memsweep(al, s2, 0);
	memsweep(alg, s2, 0);
	memsweep(agf, s2, 0);
	memsweep(algf, s2, 0);
	regsweep(ar, s2, 0);
	regsweep(agr, s2, 0);
	regsweep(agfr, s2, 0);
	regsweep(alr, s2, 0);
	regsweep(algr, s2, 0);
	regsweep(algfr, s2, 0);
	memsweep(alc, s2, 0);
	memsweep(alcg, s2, 0);
	regsweep(alcr, s2, 0);
	regsweep(alcgr, s2, 0);
	memsweep(alc, s2, 1);
	memsweep(alcg, s2, 1);
	regsweep(alcr, s2, 1);
	regsweep(alcgr, s2, 1);
	ldispsweep(AHY, s2, 0);
	ldispsweep(AY, s2, 0);
	ldispsweep(ALY, s2, 0);
}

int main()
{
	do_regmem_insns(0x0ul);
	do_regmem_insns(0x7ffffffffffffffful);
	do_regmem_insns(0x8000000000000000ul);
	do_regmem_insns(0xfffffffffffffffful);
	do_regmem_insns(0x7fffffff00000000ul);
	do_regmem_insns(0x8000000000000000ul);
	do_regmem_insns(0xffffffff00000000ul);
	do_regmem_insns(0x000000007ffffffful);
	do_regmem_insns(0x0000000080000000ul);
	do_regmem_insns(0x00000000fffffffful);
	do_regmem_insns(0x000000000000fffful);
	do_regmem_insns(0x0000000000007ffful);
	do_regmem_insns(0x0000000000008000ul);
	do_regmem_insns(0x000000000000fffful);

	do_imm_insns();

	return 0;
}
