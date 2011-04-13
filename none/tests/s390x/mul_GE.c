#include <stdio.h>
#include "mul.h"
#include "opcodes.h"

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


static void do_regmem_insns(unsigned long m2)
{
	mhysweep(m2);
	mfysweep(m2);
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
