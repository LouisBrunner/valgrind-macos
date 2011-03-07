#include <stdio.h>
#include "insert.h"

static void do_imm_insns(void)
{
	immsweep(iihh, 0x55);
	immsweep(iihl, 0x55);
	immsweep(iilh, 0x55);
	immsweep(iill, 0x55);
	immsweep(iihh, 0xaa);
	immsweep(iihl, 0xaa);
	immsweep(iilh, 0xaa);
	immsweep(iill, 0xaa);
	immsweep(iihh, 0xff);
	immsweep(iihl, 0xff);
	immsweep(iilh, 0xff);
	immsweep(iill, 0xff);
	immsweep(iihh, 0x0);
	immsweep(iihl, 0x0);
	immsweep(iilh, 0x0);
	immsweep(iill, 0x0);
	immsweep(iihh, 0xffff);
	immsweep(iihl, 0xffff);
	immsweep(iilh, 0xffff);
	immsweep(iill, 0xffff);
	immsweep(iihh, 0xaaaa);
	immsweep(iihl, 0xaaaa);
	immsweep(iilh, 0xaaaa);
	immsweep(iill, 0xaaaa);
	immsweep(iihh, 0x5555);
	immsweep(iihl, 0x5555);
	immsweep(iilh, 0x5555);
	immsweep(iill, 0x5555);
}


static void do_mem_insns(unsigned long s2)
{
	memsweep(ic, s2);
	memsweep(icy, s2);
}

int main()
{
	do_mem_insns(0x0ul);
	do_mem_insns(0x5555555555555555ul);
	do_mem_insns(0xaaaaaaaaaaaaaaaaul);
	do_mem_insns(0x8000000000000000ul);
	do_mem_insns(0xfffffffffffffffful);
	do_mem_insns(0x7fffffff00000000ul);
	do_mem_insns(0x8000000000000000ul);
	do_mem_insns(0xaaaaaaaa00000000ul);
	do_mem_insns(0xffffffff00000000ul);
	do_mem_insns(0x000000007ffffffful);
	do_mem_insns(0x0000000080000000ul);
	do_mem_insns(0x0000000055555555ul);
	do_mem_insns(0x00000000fffffffful);
	do_mem_insns(0x000000000000fffful);
	do_mem_insns(0x0000000000007ffful);
	do_mem_insns(0x0000000000008000ul);
	do_mem_insns(0x000000000000fffful);

	do_imm_insns();
	return 0;
}
