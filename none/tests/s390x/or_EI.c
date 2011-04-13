#include <stdio.h>
#include "or.h"
#include "opcodes.h"

static void do_imm_insns(void)
{
	immsweep(OIHF, 00000000);
	immsweep(OIHF, 000000ff);
	immsweep(OIHF, 00000055);
	immsweep(OIHF, 000000aa);
	immsweep(OIHF, 0000ffff);
	immsweep(OIHF, 00005555);
	immsweep(OIHF, 0000aaaa);
	immsweep(OIHF, ffff0000);
	immsweep(OIHF, 55550000);
	immsweep(OIHF, aaaa0000);
	immsweep(OIHF, ffffffff);
	immsweep(OIHF, 55555555);
	immsweep(OIHF, aaaaaaaa);
	immsweep(OILF, 00000000);
	immsweep(OILF, 000000ff);
	immsweep(OILF, 00000055);
	immsweep(OILF, 000000aa);
	immsweep(OILF, 0000ffff);
	immsweep(OILF, 00005555);
	immsweep(OILF, 0000aaaa);
	immsweep(OILF, ffff0000);
	immsweep(OILF, 55550000);
	immsweep(OILF, aaaa0000);
	immsweep(OILF, ffffffff);
	immsweep(OILF, 55555555);
	immsweep(OILF, aaaaaaaa);
}


int main()
{
	do_imm_insns();

	return 0;
}
