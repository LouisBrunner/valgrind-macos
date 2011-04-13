#include <stdio.h>
#include "sub.h"
#include "opcodes.h"

static void do_regmem_insns(unsigned long s2)
{
	memsweep(s, s2, 0);
	memsweep(sh, s2, 0);
	memsweep(sg, s2, 0);
	memsweep(sgf, s2, 0);
	memsweep(sl, s2, 0);
	memsweep(slg, s2, 0);
	memsweep(sgf, s2, 0);
	memsweep(slgf, s2, 0);
	regsweep(sr, s2, 0);
	regsweep(sgr, s2, 0);
	regsweep(sgfr, s2, 0);
	regsweep(slr, s2, 0);
	regsweep(slgr, s2, 0);
	regsweep(slgfr, s2, 0);
	memsweep(slb, s2, 0);
	memsweep(slbg, s2, 0);
	regsweep(slbr, s2, 0);
	regsweep(slbgr, s2, 0);
	memsweep(slb, s2, 1);
	memsweep(slbg, s2, 1);
	regsweep(slbr, s2, 1);
	regsweep(slbgr, s2, 1);
	ldispsweep(SHY, s2, 0);
	ldispsweep(SLY, s2, 0);
	ldispsweep(SY, s2, 0);
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

	return 0;
}
