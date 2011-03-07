#include <stdio.h>
#include "xor.h"

static void do_imm_insns(void)
{
	immsweep(xihf, 0);
	immsweep(xihf, 0xff);
	immsweep(xihf, 0x55);
	immsweep(xihf, 0xaa);
	immsweep(xihf, 0xffff);
	immsweep(xihf, 0x5555);
	immsweep(xihf, 0xaaaa);
	immsweep(xihf, 0xffff0000);
	immsweep(xihf, 0x55550000);
	immsweep(xihf, 0xaaaa0000);
	immsweep(xihf, 0xffffffff);
	immsweep(xihf, 0x55555555);
	immsweep(xihf, 0xaaaaaaaa);
	immsweep(xilf, 0);
	immsweep(xilf, 0xff);
	immsweep(xilf, 0x55);
	immsweep(xilf, 0xaa);
	immsweep(xilf, 0xffff);
	immsweep(xilf, 0x5555);
	immsweep(xilf, 0xaaaa);
	immsweep(xilf, 0xffff0000);
	immsweep(xilf, 0x55550000);
	immsweep(xilf, 0xaaaa0000);
	immsweep(xilf, 0xffffffff);
	immsweep(xilf, 0x55555555);
	immsweep(xilf, 0xaaaaaaaa);

}


int main()
{
	do_imm_insns();

	return 0;
}
