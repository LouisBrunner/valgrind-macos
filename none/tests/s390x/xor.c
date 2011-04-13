#include <stdio.h>
#include "xor.h"
#include "opcodes.h"

static void do_imm_insns(void)
{
	memimmsweep(xi, 0);
	memimmsweep(xi, 255);
	memimmsweep(xi, 128);
	memimmsweep(xi, 0xaa);
	memimmsweep(xi, 0x55);
	xiysweep(00);
	xiysweep(ff);
	xiysweep(80);
	xiysweep(aa);
	xiysweep(55);
}


static void do_regmem_insns(unsigned long s2)
{
	memsweep(x, s2);
	memsweep(xg, s2);
	regsweep(xr, s2);
	regsweep(xgr, s2);
	xysweep(s2);
}

int main()
{
	do_regmem_insns(0x0ul);
	do_regmem_insns(0x5555555555555555ul);
	do_regmem_insns(0xaaaaaaaaaaaaaaaaul);
	do_regmem_insns(0x8000000000000000ul);
	do_regmem_insns(0xfffffffffffffffful);
	do_regmem_insns(0x7fffffff00000000ul);
	do_regmem_insns(0x8000000000000000ul);
	do_regmem_insns(0xaaaaaaaa00000000ul);
	do_regmem_insns(0xffffffff00000000ul);
	do_regmem_insns(0x000000007ffffffful);
	do_regmem_insns(0x0000000080000000ul);
	do_regmem_insns(0x0000000055555555ul);
	do_regmem_insns(0x00000000fffffffful);
	do_regmem_insns(0x000000000000fffful);
	do_regmem_insns(0x0000000000007ffful);
	do_regmem_insns(0x0000000000008000ul);
	do_regmem_insns(0x000000000000fffful);

	do_imm_insns();

	return 0;
}
