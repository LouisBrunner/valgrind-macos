#include <stdio.h>
#include "and.h"

static void do_imm_insns(void)
{
	memimmsweep(ni, 0);
	memimmsweep(ni, 255);
	memimmsweep(ni, 128);
	memimmsweep(ni, 0xaa);
	memimmsweep(ni, 0x55);
	memimmsweep(niy, 0);
	memimmsweep(niy, 255);
	memimmsweep(niy, 128);
	memimmsweep(niy, 0xaa);
	memimmsweep(niy, 0x55);
	immsweep(nihh, 0x55);
	immsweep(nihl, 0x55);
	immsweep(nilh, 0x55);
	immsweep(nill, 0x55);
	immsweep(nihh, 0xaa);
	immsweep(nihl, 0xaa);
	immsweep(nilh, 0xaa);
	immsweep(nill, 0xaa);
	immsweep(nihh, 0xff);
	immsweep(nihl, 0xff);
	immsweep(nilh, 0xff);
	immsweep(nill, 0xff);
	immsweep(nihh, 0x0);
	immsweep(nihl, 0x0);
	immsweep(nilh, 0x0);
	immsweep(nill, 0x0);
	immsweep(nihh, 0xffff);
	immsweep(nihl, 0xffff);
	immsweep(nilh, 0xffff);
	immsweep(nill, 0xffff);
	immsweep(nihh, 0xaaaa);
	immsweep(nihl, 0xaaaa);
	immsweep(nilh, 0xaaaa);
	immsweep(nill, 0xaaaa);
	immsweep(nihh, 0x5555);
	immsweep(nihl, 0x5555);
	immsweep(nilh, 0x5555);
	immsweep(nill, 0x5555);
}


static void do_regmem_insns(unsigned long s2)
{
	memsweep(n, s2);
	memsweep(ng, s2);
	regsweep(nr, s2);
	regsweep(ngr, s2);
	memsweep(ny, s2);
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
