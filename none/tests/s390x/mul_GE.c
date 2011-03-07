#include <stdio.h>
#include "mul.h"

static void do_imm_insns(void)
{
	immsweep(msfi, 0);
	immsweep(msfi, -1);
	immsweep(msfi, -32768);
	immsweep(msfi, 32767);
	immsweep(msfi, 32767);
	immsweep(msfi, 32767);
	immsweep(msfi, -2147483648);
	immsweep(msfi, 2147483647);
	immsweep(msgfi, 0);
	immsweep(msgfi, -1);
	immsweep(msgfi, -32768);
	immsweep(msgfi, 32767);
	immsweep(msgfi, 32767);
	immsweep(msgfi, 32767);
	immsweep(msgfi, -2147483648);
	immsweep(msgfi, 2147483647);

}


static void do_regmem_insns(unsigned long m2)
{
	memsweep(mhy, m2);
	memsweep(mfy, m2);
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
