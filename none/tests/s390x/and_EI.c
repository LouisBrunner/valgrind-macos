#include <stdio.h>
#include "and.h"
#include "opcodes.h"

static void do_imm_insns(void)
{
	immsweep(NIHF, 00000000);
	immsweep(NIHF, 000000ff);
	immsweep(NIHF, 00000055);
	immsweep(NIHF, 000000aa);
	immsweep(NIHF, 0000ffff);
	immsweep(NIHF, 00005555);
	immsweep(NIHF, 0000aaaa);
	immsweep(NIHF, ffff0000);
	immsweep(NIHF, 55550000);
	immsweep(NIHF, aaaa0000);
	immsweep(NIHF, ffffffff);
	immsweep(NIHF, 55555555);
	immsweep(NIHF, aaaaaaaa);
}


int main()
{
	do_imm_insns();

	return 0;
}
