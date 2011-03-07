#include <stdio.h>
#include "or.h"

static void do_imm_insns(void)
{
	memimmsweep(oi, 0);
	memimmsweep(oi, 255);
	memimmsweep(oi, 128);
	memimmsweep(oi, 0xaa);
	memimmsweep(oi, 0x55);
	memimmsweep(oiy, 0);
	memimmsweep(oiy, 255);
	memimmsweep(oiy, 128);
	memimmsweep(oiy, 0xaa);
	memimmsweep(oiy, 0x55);
	immsweep(oihh, 0x55);
	immsweep(oihl, 0x55);
	immsweep(oilh, 0x55);
	immsweep(oill, 0x55);
	immsweep(oihh, 0xaa);
	immsweep(oihl, 0xaa);
	immsweep(oilh, 0xaa);
	immsweep(oill, 0xaa);
	immsweep(oihh, 0xff);
	immsweep(oihl, 0xff);
	immsweep(oilh, 0xff);
	immsweep(oill, 0xff);
	immsweep(oihh, 0x0);
	immsweep(oihl, 0x0);
	immsweep(oilh, 0x0);
	immsweep(oill, 0x0);
	immsweep(oihh, 0xffff);
	immsweep(oihl, 0xffff);
	immsweep(oilh, 0xffff);
	immsweep(oill, 0xffff);
	immsweep(oihh, 0xaaaa);
	immsweep(oihl, 0xaaaa);
	immsweep(oilh, 0xaaaa);
	immsweep(oill, 0xaaaa);
	immsweep(oihh, 0x5555);
	immsweep(oihl, 0x5555);
	immsweep(oilh, 0x5555);
	immsweep(oill, 0x5555);
}


static void do_regmem_insns(unsigned long s2)
{
	memsweep(o, s2);
	memsweep(og, s2);
	regsweep(or, s2);
	regsweep(ogr, s2);
	memsweep(oy, s2);
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
