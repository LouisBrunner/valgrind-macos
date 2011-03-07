#include <stdio.h>
#include "insert.h"

static void do_imm_insns(void)
{
	immsweep(iihf, 0);
	immsweep(iihf, 0xff);
	immsweep(iihf, 0x55);
	immsweep(iihf, 0xaa);
	immsweep(iihf, 0xffff);
	immsweep(iihf, 0x5555);
	immsweep(iihf, 0xaaaa);
	immsweep(iihf, 0xffff0000);
	immsweep(iihf, 0x55550000);
	immsweep(iihf, 0xaaaa0000);
	immsweep(iihf, 0xffffffff);
	immsweep(iihf, 0x55555555);
	immsweep(iihf, 0xaaaaaaaa);
	immsweep(iilf, 0);
	immsweep(iilf, 0xff);
	immsweep(iilf, 0x55);
	immsweep(iilf, 0xaa);
	immsweep(iilf, 0xffff);
	immsweep(iilf, 0x5555);
	immsweep(iilf, 0xaaaa);
	immsweep(iilf, 0xffff0000);
	immsweep(iilf, 0x55550000);
	immsweep(iilf, 0xaaaa0000);
	immsweep(iilf, 0xffffffff);
	immsweep(iilf, 0x55555555);
	immsweep(iilf, 0xaaaaaaaa);
}

int main()
{
	do_imm_insns();
	return 0;
}
