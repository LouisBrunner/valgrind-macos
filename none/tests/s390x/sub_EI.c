#include <stdio.h>
#include "sub.h"
#include "opcodes.h"

static void do_imm_insns(void)
{
	immsweep(SLFI,  00000000, 0);
	immsweep(SLFI,  0000ffff, 0);
	immsweep(SLFI,  00008000, 0);
	immsweep(SLFI,  00007fff, 0);
	immsweep(SLFI,  ffffffff, 0);
	immsweep(SLFI,  80000000, 0);
	immsweep(SLFI,  7fffffff, 0);
	immsweep(SLGFI, 00000000, 0);
	immsweep(SLGFI, 0000ffff, 0);
	immsweep(SLGFI, 00008000, 0);
	immsweep(SLGFI, 00007fff, 0);
	immsweep(SLGFI, ffffffff, 0);
	immsweep(SLGFI, 80000000, 0);
	immsweep(SLGFI, 7fffffff, 0);

}

int main()
{
	do_imm_insns();

	return 0;
}
