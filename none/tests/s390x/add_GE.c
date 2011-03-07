#include <stdio.h>
#include "add.h"

static void do_memimm_insns(void)
{
	memimmsweep(asi, 0, 0);
	memimmsweep(agsi, 0, 0);
	memimmsweep(alsi, 0, 0);
	memimmsweep(algsi, 0, 0);
	memimmsweep(asi, 1, 0);
	memimmsweep(agsi, 1, 0);
	memimmsweep(alsi, 1, 0);
	memimmsweep(algsi, 1, 0);
	memimmsweep(asi, -1, 0);
	memimmsweep(agsi, -1, 0);
	memimmsweep(alsi, -1, 0);
	memimmsweep(algsi, -1, 0);
	memimmsweep(asi, -128, 0);
	memimmsweep(agsi, -128, 0);
	memimmsweep(alsi, -128, 0);
	memimmsweep(algsi, -128, 0);
	memimmsweep(asi, 127, 0);
	memimmsweep(agsi, 127, 0);
	memimmsweep(alsi, 127, 0);
	memimmsweep(algsi, 127, 0);
}

int main()
{
	do_memimm_insns();

	return 0;
}
