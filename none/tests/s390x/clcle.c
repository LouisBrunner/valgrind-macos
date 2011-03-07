#include <stdio.h>
#include <stdlib.h>

char b1[23] ="0123456789abcdefghijklm";
char b2[23] ="mlkjihgfedcba9876543210";
char b3[23] ="mmmmmmmmmmmmmmmmmmmmmmm";
char b4[23] ="00000000000000000000000";
char longbuf[17000000];

static int clcle(unsigned long *_a1, unsigned long *_l1, unsigned long *_a3, unsigned long *_l3, char _pad)
{
	register unsigned long a1 asm ("2") = *_a1;
	register unsigned long l1 asm ("3") = *_l1;
	register unsigned long a3 asm ("4") = *_a3;
	register unsigned long l3 asm ("5") = *_l3;
	register unsigned long pad asm ("6") = _pad;
	register unsigned long cc asm ("7");

	asm volatile(	"0: clcle 2,4,0(6)\n\t"
			"jo 0b\n\t"
			"ipm %0\n\t"
			"srl %0,28\n\t"
			:"=d" (cc), "+d" (a1),"+d" (l1), "+d" (a3), "+d" (l3)
			: "d" (pad)
			: "memory", "cc");
	*_a1 = a1;
	*_a3 = a3;
	*_l1 = l1;
	*_l3 = l3;

	return cc;
}


void testrun(void *_a1, unsigned long _l1, void *_a3, unsigned long _l3, char pad)
{
	unsigned long a1,a3,l1,l3;
	int cc;

	a1 = (unsigned long) _a1; l1 = _l1; a3 = (unsigned long) _a3; l3 = _l3;
	cc = clcle(&a1, &l1,  &a3, &l3, pad);
	printf("cc: %d, l1: %lu(%lu) l3: %lu(%lu) diff1: %lu diff3: %lu\n",
                cc, l1, _l1, l3, _l3, a1-(unsigned long) _a1, a3-(unsigned long) _a3);
}


void multiplex(unsigned long l1, unsigned long l3, char pad)
{
	testrun(b1, l1, b1, l3, pad);
	testrun(b1, l1, b2, l3, pad);
	testrun(b1, l1, b3, l3, pad);
	testrun(b1, l1, b4, l3, pad);
	testrun(b2, l1, b2, l3, pad);
	testrun(b2, l1, b3, l3, pad);
	testrun(b2, l1, b4, l3, pad);
	testrun(b3, l1, b3, l3, pad);
	testrun(b3, l1, b4, l3, pad);
	testrun(b4, l1, b4, l3, pad);
}

int main()
{
	multiplex(0,0,9);
	multiplex(1,0,9);
	multiplex(0,1,9);
	multiplex(1,1,9);
	multiplex(5,23,9);
	multiplex(23,5,9);
	testrun(longbuf,10000,longbuf,100000,0);
	testrun(longbuf,10000,longbuf,100000,128);
	testrun(longbuf,10000,longbuf,100000,255);
	exit(0);
}

