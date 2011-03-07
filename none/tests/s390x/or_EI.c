#include <stdio.h>
#include "or.h"

static void do_imm_insns(void)
{
	immsweep(oihf, 0);
	immsweep(oihf, 0xff);
	immsweep(oihf, 0x55);
	immsweep(oihf, 0xaa);
	immsweep(oihf, 0xffff);
	immsweep(oihf, 0x5555);
	immsweep(oihf, 0xaaaa);
	immsweep(oihf, 0xffff0000);
	immsweep(oihf, 0x55550000);
	immsweep(oihf, 0xaaaa0000);
	immsweep(oihf, 0xffffffff);
	immsweep(oihf, 0x55555555);
	immsweep(oihf, 0xaaaaaaaa);
	immsweep(oilf, 0);
	immsweep(oilf, 0xff);
	immsweep(oilf, 0x55);
	immsweep(oilf, 0xaa);
	immsweep(oilf, 0xffff);
	immsweep(oilf, 0x5555);
	immsweep(oilf, 0xaaaa);
	immsweep(oilf, 0xffff0000);
	immsweep(oilf, 0x55550000);
	immsweep(oilf, 0xaaaa0000);
	immsweep(oilf, 0xffffffff);
	immsweep(oilf, 0x55555555);
	immsweep(oilf, 0xaaaaaaaa);
}


int main()
{
	do_imm_insns();

	return 0;
}
