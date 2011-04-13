#include <stdio.h>
#include "insert.h"
#include "opcodes.h"

static void do_imm_insns(void)
{
	immsweep(IIHF, 00000000);
	immsweep(IIHF, 000000ff);
	immsweep(IIHF, 00000055);
	immsweep(IIHF, 000000aa);
	immsweep(IIHF, 0000ffff);
	immsweep(IIHF, 00005555);
	immsweep(IIHF, 0000aaaa);
	immsweep(IIHF, ffff0000);
	immsweep(IIHF, 55550000);
	immsweep(IIHF, aaaa0000);
	immsweep(IIHF, ffffffff);
	immsweep(IIHF, 55555555);
	immsweep(IIHF, aaaaaaaa);
	immsweep(IILF, 00000000);
	immsweep(IILF, 000000ff);
	immsweep(IILF, 00000055);
	immsweep(IILF, 000000aa);
	immsweep(IILF, 0000ffff);
	immsweep(IILF, 00005555);
	immsweep(IILF, 0000aaaa);
	immsweep(IILF, ffff0000);
	immsweep(IILF, 55550000);
	immsweep(IILF, aaaa0000);
	immsweep(IILF, ffffffff);
	immsweep(IILF, 55555555);
	immsweep(IILF, aaaaaaaa);
}

int main()
{
	do_imm_insns();
	return 0;
}
