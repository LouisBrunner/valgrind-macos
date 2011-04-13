#include <stdio.h>
#include "xor.h"
#include "opcodes.h"

static void do_imm_insns(void)
{
	immsweep(XIHF, 00000000);
	immsweep(XIHF, 000000ff);
	immsweep(XIHF, 00000055);
	immsweep(XIHF, 000000aa);
	immsweep(XIHF, 0000ffff);
	immsweep(XIHF, 00005555);
	immsweep(XIHF, 0000aaaa);
	immsweep(XIHF, ffff0000);
	immsweep(XIHF, 55550000);
	immsweep(XIHF, aaaa0000);
	immsweep(XIHF, ffffffff);
	immsweep(XIHF, 55555555);
	immsweep(XIHF, aaaaaaaa);
	immsweep(XILF, 00000000);
	immsweep(XILF, 000000ff);
	immsweep(XILF, 00000055);
	immsweep(XILF, 000000aa);
	immsweep(XILF, 0000ffff);
	immsweep(XILF, 00005555);
	immsweep(XILF, 0000aaaa);
	immsweep(XILF, ffff0000);
	immsweep(XILF, 55550000);
	immsweep(XILF, aaaa0000);
	immsweep(XILF, ffffffff);
	immsweep(XILF, 55555555);
	immsweep(XILF, aaaaaaaa);

}


int main()
{
	do_imm_insns();

	return 0;
}
