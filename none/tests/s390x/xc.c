/* tests, xc,oc and nc */
#include <stdio.h>
#include "test.h"

void test_oc(void)
{
	char buf1[20] = "UUUUU*UUU****U*\0\0\0\0\n";
	char buf2[20] = "*U\0*\0\0UU*\0U*AUAA*UU\n";
	char zero[2] = "\0\0";

	printf("\nOC:\n");
	asm volatile ("oc %O0(1,%R0),%0\n"::"Q" (*zero),
		      "Q"(*zero):"memory");
	printf("CC:%d\n", get_cc());
	dump_field(zero, 2);

	asm volatile ("oc %O0(19,%R0),%1\n"::"Q" (*buf1),
		      "Q"(*buf2):"memory");
	printf("CC:%d\n", get_cc());
	dump_field(buf1, 20);
}

void test_nc(void)
{
	char buf1[20] = "UUUUU*UUU****U*\0\0\0\0\n";
	char buf2[20] = "*U\0*\0\0UU*\0U*AUAA*UU\n";
	char zero[2] = "\0\0";

	printf("\nNC:\n");
	asm volatile ("nc %O0(1,%R0),%0\n"::"Q" (*zero),
		      "Q"(*zero):"memory");
	printf("CC:%d\n", get_cc());
	dump_field(zero, 2);

	asm volatile ("nc %O0(19,%R0),%1\n"::"Q" (*buf1),
		      "Q"(*buf2):"memory");
	printf("CC:%d\n", get_cc());
	dump_field(buf1, 20);
}


void test_xc(void)
{
	char buf1[20] = "UUUUU*UUU****U*\0\0\0\0\n";
	char buf2[20] = "*U\0*\0\0UU*\0U*AUAA*UU\n";
	char buf3[20] = "0123456789abcdefghij";
	char zero[300] =
	    "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
	    "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
	    "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
	    "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
	    "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
	    "aaaaa";

	printf("\nXC:\n");
	asm volatile ("xc %O0(1,%R0),%0\n"::"Q" (*zero),
		      "Q"(*zero):"memory");
	printf("CC:%d\n", get_cc());
	dump_field(zero, 4);

	asm volatile ("xc %O0(10,%R0),%0\n"::"Q" (*zero),
		      "Q"(*zero):"memory");
	printf("CC:%d\n", get_cc());
	dump_field(zero, 12);

	asm volatile ("xc %O0(100,%R0),%0\n"::"Q" (*zero),
		      "Q"(*zero):"memory");
	printf("CC:%d\n", get_cc());
	dump_field(zero, 102);

	asm volatile ("xc %O0(256,%R0),%0\n"::"Q" (*zero),
		      "Q"(*zero):"memory");
	printf("CC:%d\n", get_cc());
	dump_field(zero, 257);

	asm volatile ("lghi 1,256 + 20\n"
		      "larl 2,1f\n"
		      "ex 1,0(2)\n"
		      "j 2f\n"
		      "1: xc 260(1,%0),260(%0)\n"
		      "2:\n"::"a" (zero), "a"(zero):"memory", "1", "2");
	printf("CC:%d\n", get_cc());
	dump_field(zero + 260, 30);

	asm volatile ("xc 0(19,%0),0(%1)\n"::"a" (buf1),
		      "a"(buf2):"memory");
	printf("CC:%d\n", get_cc());
	dump_field(buf1, 20);
	asm volatile ("xc 0(10,%0),0(%0)\n"::"a" (buf3):"memory");

	printf("CC:%d\n", get_cc());
	dump_field(buf3, 20);
	return;
}

int main()
{
	test_oc();
	test_nc();
	test_xc();
	return 0;
}
