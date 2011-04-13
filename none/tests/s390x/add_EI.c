#include <stdio.h>
#include "add.h"
#include "opcodes.h"

static void do_imm_insns(void)
{
	ximmsweep(AFI,  00000000, 00000000, 0);
	ximmsweep(AFI,  ffffffff, ffffffff, 0);
	ximmsweep(AFI,  ffffffff, ffff8000, 0);
	ximmsweep(AFI,  00000000, 00007fff, 0);
	ximmsweep(AFI,  ffffffff, ffffffff, 0);
	ximmsweep(AFI,  ffffffff, 80000000, 0);
	ximmsweep(AFI,  00000000, 7fffffff, 0);

	ximmsweep(AGFI, 00000000, 00000000, 0);
	ximmsweep(AGFI, ffffffff, ffffffff, 0);
	ximmsweep(AGFI, ffffffff, ffff8000, 0);
	ximmsweep(AGFI, 00000000, 00007fff, 0);
	ximmsweep(AGFI, ffffffff, ffffffff, 0);
	ximmsweep(AGFI, ffffffff, 80000000, 0);
	ximmsweep(AGFI, 00000000, 7fffffff, 0);

	ximmsweep(ALFI, 00000000, 00000000, 0);
	ximmsweep(ALFI, 00000000, 0000ffff, 0);
	ximmsweep(ALFI, 00000000, 00008000, 0);
	ximmsweep(ALFI, 00000000, 00007fff, 0);
	ximmsweep(ALFI, 00000000, ffffffff, 0);
	ximmsweep(ALFI, 00000000, 80000000, 0);
	ximmsweep(ALFI, 00000000, 7fffffff, 0);

	ximmsweep(ALGFI, 00000000, 00000000, 0);
	ximmsweep(ALGFI, 00000000, 0000ffff, 0);
	ximmsweep(ALGFI, 00000000, 00008000, 0);
	ximmsweep(ALGFI, 00000000, 00007fff, 0);
	ximmsweep(ALGFI, 00000000, ffffffff, 0);
	ximmsweep(ALGFI, 00000000, 80000000, 0);
	ximmsweep(ALGFI, 00000000, 7fffffff, 0);
}

int main()
{
	do_imm_insns();

	return 0;
}
