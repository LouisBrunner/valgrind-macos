#include <stdio.h>
#include "and.h"
#include "opcodes.h"

#define nihh(r1,i2)  ".long 0xa5" #r1 "4" #i2 "\n\t"
#define nihl(r1,i2)  ".long 0xa5" #r1 "5" #i2 "\n\t"
#define nilh(r1,i2)  ".long 0xa5" #r1 "6" #i2 "\n\t"
#define nill(r1,i2)  ".long 0xa5" #r1 "7" #i2 "\n\t"

static void do_imm_insns(void)
{
	memimmsweep(ni, 0);
	memimmsweep(ni, 255);
	memimmsweep(ni, 128);
	memimmsweep(ni, 0xaa);
	memimmsweep(ni, 0x55);
	niysweep(00);
	niysweep(ff);
	niysweep(80);
	niysweep(aa);
	niysweep(55);
	immsweep(nihh, 0055);
	immsweep(nihl, 0055);
	immsweep(nilh, 0055);
	immsweep(nill, 0055);
	immsweep(nihh, 00aa);
	immsweep(nihl, 00aa);
	immsweep(nilh, 00aa);
	immsweep(nill, 00aa);
	immsweep(nihh, 00ff);
	immsweep(nihl, 00ff);
	immsweep(nilh, 00ff);
	immsweep(nill, 00ff);
	immsweep(nihh, 0000);
	immsweep(nihl, 0000);
	immsweep(nilh, 0000);
	immsweep(nill, 0000);
	immsweep(nihh, ffff);
	immsweep(nihl, ffff);
	immsweep(nilh, ffff);
	immsweep(nill, ffff);
	immsweep(nihh, aaaa);
	immsweep(nihl, aaaa);
	immsweep(nilh, aaaa);
	immsweep(nill, aaaa);
	immsweep(nihh, 5555);
	immsweep(nihl, 5555);
	immsweep(nilh, 5555);
	immsweep(nill, 5555);
}


static void do_regmem_insns(unsigned long s2)
{
	memsweep(n, s2);
	memsweep(ng, s2);
	regsweep(nr, s2);
	regsweep(ngr, s2);
	nysweep(s2);
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
