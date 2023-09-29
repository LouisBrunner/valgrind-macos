#include <stdio.h>
#include <stdlib.h>

char b1[23] ="0123456789abcdefghijklm";
char b2[23] ="mlkjihgfedcba9876543210";
char b3[23] ="mmmmmmmmmmmmmmmmmmmmmmm";
char b4[23] ="00000000000000000000000";
char longbuf1[256];
char longbuf2[256];

static int clc(char *a1,char *a2, int l)
{
	int cc;

	asm volatile(	"larl 1, 1f\n"
			"ex %3,0(1)\n"
			"j 2f\n"
			"1: clc 0(1,%1),0(%2)\n"
			"2: ipm %0\n"
			"srl %0,28\n"
		:"=d" (cc)
		:"a" (a1), "a" (a2), "a" (l): "1", "cc");
	return cc;
}


void testrun(char *a1, char *a2, int l)
{
	int cc;

	cc = clc(a1, a2, l);
	printf("%d bytes:%d\n",l, cc);
}


void multiplex(int l, long offset1, long offset2)
{
	testrun(b1 + offset1, b1 + offset2, l);
	testrun(b1 + offset1, b2 + offset2, l);
	testrun(b1 + offset1, b3 + offset2, l);
	testrun(b1 + offset1, b4 + offset2, l);
	testrun(b2 + offset1, b2 + offset2, l);
	testrun(b2 + offset1, b3 + offset2, l);
	testrun(b2 + offset1, b4 + offset2, l);
	testrun(b3 + offset1, b3 + offset2, l);
	testrun(b3 + offset1, b4 + offset2, l);
	testrun(b4 + offset1, b4 + offset2, l);
}

void sweep(int l)
{
	multiplex(l, 0, 0);
	multiplex(l, 1, 0);
	multiplex(l, 1, 1);
	multiplex(l, 0, 1);
}

int main()
{
	sweep(0);
	sweep(1);
	sweep(2);
	sweep(3);
	sweep(4);
	sweep(5);
	sweep(22);
	testrun(longbuf1, longbuf2, 255);
	longbuf1[255] = 'a';
	testrun(longbuf1, longbuf2, 255);
	longbuf2[255] = 'b';
	testrun(longbuf1, longbuf2, 255);
	return 0;
}

