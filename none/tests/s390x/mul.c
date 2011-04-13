#include <stdio.h>
#include "mul.h"
#include "opcodes.h"

static void do_imm_insns(void)
{
	immsweep(mhi, 0);
	immsweep(mhi, -1);
	immsweep(mhi, -32768);
	immsweep(mhi, 32767);
	immsweep(mghi, 0);
	immsweep(mghi, -1);
	immsweep(mghi, -32768);
	immsweep(mghi, 32767);
}


static void do_regmem_insns(unsigned long m2)
{
	memsweep(m, m2);
	regsweep(mr, m2);
	memsweep(mh, m2);
	memsweep(mlg, m2);
	regsweep(mlgr, m2);
	memsweep(ml, m2);
	regsweep(mlr, m2);
	memsweep(ms, m2);
	regsweep(msr, m2);
	memsweep(msg, m2);
	regsweep(msgr, m2);
	memsweep(msgf, m2);
	regsweep(msgfr, m2);
	msysweep(m2);
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
