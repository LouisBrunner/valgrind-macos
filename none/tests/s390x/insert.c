#include <stdio.h>
#include "insert.h"
#include "opcodes.h"

#define iihh(r1,i2)  ".long 0xa5" #r1 "0" #i2 "\n\t"
#define iihl(r1,i2)  ".long 0xa5" #r1 "1" #i2 "\n\t"
#define iilh(r1,i2)  ".long 0xa5" #r1 "2" #i2 "\n\t"
#define iill(r1,i2)  ".long 0xa5" #r1 "3" #i2 "\n\t"

static void do_imm_insns(void)
{
	immsweep(iihh, 0055);
	immsweep(iihl, 0055);
	immsweep(iilh, 0055);
	immsweep(iill, 0055);
	immsweep(iihh, 00aa);
	immsweep(iihl, 00aa);
	immsweep(iilh, 00aa);
	immsweep(iill, 00aa);
	immsweep(iihh, 00ff);
	immsweep(iihl, 00ff);
	immsweep(iilh, 00ff);
	immsweep(iill, 00ff);
	immsweep(iihh, 0000);
	immsweep(iihl, 0000);
	immsweep(iilh, 0000);
	immsweep(iill, 0000);
	immsweep(iihh, ffff);
	immsweep(iihl, ffff);
	immsweep(iilh, ffff);
	immsweep(iill, ffff);
	immsweep(iihh, aaaa);
	immsweep(iihl, aaaa);
	immsweep(iilh, aaaa);
	immsweep(iill, aaaa);
	immsweep(iihh, 5555);
	immsweep(iihl, 5555);
	immsweep(iilh, 5555);
	immsweep(iill, 5555);
}


static void do_mem_insns(unsigned long s2)
{
	memsweep(ic, s2);
	icysweep(s2);
}

int main()
{
	do_mem_insns(0x0ul);
	do_mem_insns(0x5555555555555555ul);
	do_mem_insns(0xaaaaaaaaaaaaaaaaul);
	do_mem_insns(0x8000000000000000ul);
	do_mem_insns(0xfffffffffffffffful);
	do_mem_insns(0x7fffffff00000000ul);
	do_mem_insns(0x8000000000000000ul);
	do_mem_insns(0xaaaaaaaa00000000ul);
	do_mem_insns(0xffffffff00000000ul);
	do_mem_insns(0x000000007ffffffful);
	do_mem_insns(0x0000000080000000ul);
	do_mem_insns(0x0000000055555555ul);
	do_mem_insns(0x00000000fffffffful);
	do_mem_insns(0x000000000000fffful);
	do_mem_insns(0x0000000000007ffful);
	do_mem_insns(0x0000000000008000ul);
	do_mem_insns(0x000000000000fffful);

	do_imm_insns();
	return 0;
}
