#include <stdio.h>
#include "or.h"
#include "opcodes.h"

#define oihh(r1,i2)  ".long 0xa5" #r1 "8" #i2 "\n\t"
#define oihl(r1,i2)  ".long 0xa5" #r1 "9" #i2 "\n\t"
#define oilh(r1,i2)  ".long 0xa5" #r1 "a" #i2 "\n\t"
#define oill(r1,i2)  ".long 0xa5" #r1 "b" #i2 "\n\t"

static void do_imm_insns(void)
{
	memimmsweep(oi, 0);
	memimmsweep(oi, 255);
	memimmsweep(oi, 128);
	memimmsweep(oi, 0xaa);
	memimmsweep(oi, 0x55);
	oiysweep(00);
	oiysweep(ff);
	oiysweep(80);
	oiysweep(aa);
	oiysweep(55);
	immsweep(oihh, 0055);
	immsweep(oihl, 0055);
	immsweep(oilh, 0055);
	immsweep(oill, 0055);
	immsweep(oihh, 00aa);
	immsweep(oihl, 00aa);
	immsweep(oilh, 00aa);
	immsweep(oill, 00aa);
	immsweep(oihh, 00ff);
	immsweep(oihl, 00ff);
	immsweep(oilh, 00ff);
	immsweep(oill, 00ff);
	immsweep(oihh, 0000);
	immsweep(oihl, 0000);
	immsweep(oilh, 0000);
	immsweep(oill, 0000);
	immsweep(oihh, ffff);
	immsweep(oihl, ffff);
	immsweep(oilh, ffff);
	immsweep(oill, ffff);
	immsweep(oihh, aaaa);
	immsweep(oihl, aaaa);
	immsweep(oilh, aaaa);
	immsweep(oill, aaaa);
	immsweep(oihh, 5555);
	immsweep(oihl, 5555);
	immsweep(oilh, 5555);
	immsweep(oill, 5555);
}


static void do_regmem_insns(unsigned long s2)
{
	memsweep(o, s2);
	memsweep(og, s2);
	regsweep(or, s2);
	regsweep(ogr, s2);
	oysweep(s2);
}

int main()
{
	do_regmem_insns(0x0ul);
	do_regmem_insns(0x5555555555555555ul);
	do_regmem_insns(0xaaaaaaaaaaaaaaaaul);
	do_regmem_insns(0x8000000000000000ul);
	do_regmem_insns(0xfffffffffffffffful);
	do_regmem_insns(0x7fffffff00000000ul);
	do_regmem_insns(0x8000000000000000ul);
	do_regmem_insns(0xaaaaaaaa00000000ul);
	do_regmem_insns(0xffffffff00000000ul);
	do_regmem_insns(0x000000007ffffffful);
	do_regmem_insns(0x0000000080000000ul);
	do_regmem_insns(0x0000000055555555ul);
	do_regmem_insns(0x00000000fffffffful);
	do_regmem_insns(0x000000000000fffful);
	do_regmem_insns(0x0000000000007ffful);
	do_regmem_insns(0x0000000000008000ul);
	do_regmem_insns(0x000000000000fffful);

	do_imm_insns();

	return 0;
}
