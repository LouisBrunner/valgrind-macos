#include <stdio.h>
#include "div.h"

static void do_regmem_insns(unsigned long m2)
{
	memsweep(d, m2);
	regsweep(dr, m2);
	memsweep(dl, m2);
	regsweep(dlr, m2);
	memsweep(dlg, m2);
	regsweep(dlgr, m2);
	memsweep(dsg, m2);
	regsweep(dsgr, m2);
	memsweep(dsgf, m2);
	regsweep(dsgfr, m2);
}

int main()
{
	do_regmem_insns(0x7ffffffffffffffaul);
	do_regmem_insns(0x80000000f0000000ul);
	do_regmem_insns(0xfffffffafffffffaul);
	do_regmem_insns(0x7ffffffff0000000ul);
	do_regmem_insns(0x80000000f0000000ul);
	do_regmem_insns(0xfffffffaf0000000ul);
	do_regmem_insns(0x000000087ffffffful);
	do_regmem_insns(0x0000000480000000ul);
	do_regmem_insns(0x00000008fffffffaul);
	return 0;
}
