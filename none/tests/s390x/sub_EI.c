#include <stdio.h>
#include "sub.h"

static void do_imm_insns(void)
{
	immsweep(slfi, 0, 0);
	immsweep(slfi, 65535, 0);
	immsweep(slfi, 32768, 0);
	immsweep(slfi, 32767, 0);
	immsweep(slfi, 4294967295, 0);
	immsweep(slfi, 2147483648, 0);
	immsweep(slfi, 2147483647, 0);
	immsweep(slgfi, 0, 0);
	immsweep(slgfi, 65535, 0);
	immsweep(slgfi, 32768, 0);
	immsweep(slgfi, 32767, 0);
	immsweep(slgfi, 4294967295, 0);
	immsweep(slgfi, 2147483648, 0);
	immsweep(slgfi, 2147483647, 0);

}

int main()
{
	do_imm_insns();

	return 0;
}
