#include <stdio.h>
#include "add.h"

static void do_imm_insns(void)
{
	immsweep(afi, 0, 0);
	immsweep(afi, -1, 0);
	immsweep(afi, -32768, 0);
	immsweep(afi, 32767, 0);
	immsweep(afi, -1, 0);
	immsweep(afi, -2147483648, 0);
	immsweep(afi, 2147483647, 0);
	immsweep(agfi, 0, 0);
	immsweep(agfi, -1, 0);
	immsweep(agfi, -32768, 0);
	immsweep(agfi, 32767, 0);
	immsweep(agfi, -1, 0);
	immsweep(agfi, -2147483648, 0);
	immsweep(agfi, 2147483647, 0);
	immsweep(alfi, 0, 0);
	immsweep(alfi, 65535, 0);
	immsweep(alfi, 32768, 0);
	immsweep(alfi, 32767, 0);
	immsweep(alfi, 4294967295, 0);
	immsweep(alfi, 2147483648, 0);
	immsweep(alfi, 2147483647, 0);
	immsweep(algfi, 0, 0);
	immsweep(algfi, 65535, 0);
	immsweep(algfi, 32768, 0);
	immsweep(algfi, 32767, 0);
	immsweep(algfi, 4294967295, 0);
	immsweep(algfi, 2147483648, 0);
	immsweep(algfi, 2147483647, 0);

}

int main()
{
	do_imm_insns();

	return 0;
}
