#include <stdio.h>
#include "test.h"
#define icm(r1, mask, b) do {\
	asm volatile(	"lg 1, 0(%0)\n"  \
		"icm 1," #mask ",0(%1)\n" \
		"stg 1, 0(%0)\n" \
	:: "a" (r1), "a" (b) \
	: "1", "memory", "cc"); \
} while(0)

#define icmh(r1, mask, b) do {\
	asm volatile(	"lg 1, 0(%0)\n"  \
		"icmh 1," #mask ",0(%1)\n" \
		"stg 1, 0(%0)\n" \
	:: "a" (r1), "a" (b) \
	: "1", "memory", "cc"); \
} while(0)

int main()
{
	long a[320];
	char buffer[256];
	char *b1="\x80\x00\x00\x00";
	char *b2="\x00\x00\x00\x01";
	char *b3="\xff\x00\x00\x00";
	char *b4="\x00\xff\x00\x00";
	char *b5="\x00\x00\xff\x00";
	char *b6="\x00\x00\x00\xff";
	int n;
	int cc;

	for (n=0; n<320; n++)
		a[n] = n;

#define test(what, offset) do { \
	icm(&a[0+offset], 0, what); cc = get_cc(); \
	icm(&a[1+offset+cc], 1, what); cc = get_cc(); \
	icm(&a[2+offset+cc], 2, what); cc = get_cc(); \
	icm(&a[3+offset+cc], 3, what); cc = get_cc(); \
	icm(&a[4+offset+cc], 4, what); cc = get_cc(); \
	icm(&a[5+offset+cc], 5, what); cc = get_cc(); \
	icm(&a[6+offset+cc], 6, what); cc = get_cc(); \
	icm(&a[7+offset+cc], 7, what); cc = get_cc(); \
	icm(&a[8+offset+cc], 8, what); cc = get_cc(); \
	icm(&a[9+offset+cc], 9, what); cc = get_cc(); \
	icm(&a[10+offset+cc], 10, what); cc = get_cc(); \
	icm(&a[11+offset+cc], 11, what); cc = get_cc(); \
	icm(&a[12+offset+cc], 12, what); cc = get_cc(); \
	icm(&a[13+offset+cc], 13, what); cc = get_cc(); \
	icm(&a[14+offset+cc], 14, what); cc = get_cc(); \
	icm(&a[15+offset+cc], 15, what); cc = get_cc(); \
	icmh(&a[0+offset+cc], 0, what); cc = get_cc(); \
	icmh(&a[1+offset+cc], 1, what); cc = get_cc(); \
	icmh(&a[2+offset+cc], 2, what); cc = get_cc(); \
	icmh(&a[3+offset+cc], 3, what); cc = get_cc(); \
	icmh(&a[4+offset+cc], 4, what); cc = get_cc(); \
	icmh(&a[5+offset+cc], 5, what); cc = get_cc(); \
	icmh(&a[6+offset+cc], 6, what); cc = get_cc(); \
	icmh(&a[7+offset+cc], 7, what); cc = get_cc(); \
	icmh(&a[8+offset+cc], 8, what); cc = get_cc(); \
	icmh(&a[9+offset+cc], 9, what); cc = get_cc(); \
	icmh(&a[10+offset+cc], 10, what); cc = get_cc(); \
	icmh(&a[11+offset+cc], 11, what); cc = get_cc(); \
	icmh(&a[12+offset+cc], 12, what); cc = get_cc(); \
	icmh(&a[13+offset+cc], 13, what); cc = get_cc(); \
	icmh(&a[14+offset+cc], 14, what); cc = get_cc(); \
	icmh(&a[15+offset+cc], 15, what); \
} while (0)

	for (n=0; n<256; n++)
		buffer[n] = n;

	test(&buffer[0],0);
	test(&buffer[60],16);
	test(&buffer[120],32);
	test(&buffer[180],48);
	test(&buffer[240],64);
	test(&buffer[252],80);
	test(b1,96);
	test(b2,112);
	for (n=0; n<256; n++)
		buffer[n] = 255-n;
	test(&buffer[0],128);
	test(&buffer[60],144);
	test(&buffer[160],160);
	test(b3,176);
	test(b4,192);
	test(b5,208);
	test(b6,224);

	dump_field((char *) a, sizeof(a));

	return 0;
}
