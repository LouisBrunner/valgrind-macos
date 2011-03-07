#include <stdio.h>
#include "and.h"

static void do_imm_insns(void)
{
	immsweep(nihf, 0);
	immsweep(nihf, 0xff);
	immsweep(nihf, 0x55);
	immsweep(nihf, 0xaa);
	immsweep(nihf, 0xffff);
	immsweep(nihf, 0x5555);
	immsweep(nihf, 0xaaaa);
	immsweep(nihf, 0xffff0000);
	immsweep(nihf, 0x55550000);
	immsweep(nihf, 0xaaaa0000);
	immsweep(nihf, 0xffffffff);
	immsweep(nihf, 0x55555555);
	immsweep(nihf, 0xaaaaaaaa);
}


int main()
{
	do_imm_insns();

	return 0;
}
