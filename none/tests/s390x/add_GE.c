#include <stdio.h>
#include "add.h"
#include "opcodes.h"

static void do_memimm_insns(void)
{
	memimmsweep(ASI,   00, 0);
	memimmsweep(AGSI,  00, 0);
	memimmsweep(ALSI,  00, 0);
	memimmsweep(ALGSI, 00, 0);
	memimmsweep(ASI,   01, 0);
	memimmsweep(AGSI,  01, 0);
	memimmsweep(ALSI,  01, 0);
	memimmsweep(ALGSI, 01, 0);
	memimmsweep(ASI,   ff, 0);
	memimmsweep(AGSI,  ff, 0);
	memimmsweep(ALSI,  ff, 0);
	memimmsweep(ALGSI, ff, 0);
	memimmsweep(ASI,   80, 0);
	memimmsweep(AGSI,  80, 0);
	memimmsweep(ALSI,  80, 0);
	memimmsweep(ALGSI, 80, 0);
	memimmsweep(ASI,   7f, 0);
	memimmsweep(AGSI,  7f, 0);
	memimmsweep(ALSI,  7f, 0);
	memimmsweep(ALGSI, 7f, 0);
}

int main()
{
	do_memimm_insns();

	return 0;
}
