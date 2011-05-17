#include<stdio.h>
#include<time.h>
#include<stdlib.h>
#include<unistd.h>

int stckf(unsigned long *addr)
{
	int cc;
	asm volatile (	".insn s,0xb27c0000,%0\n"
			"ipm	%1\n"
			"srl	%1,28\n"
		      :"=Q" (*addr), "=d"(cc)::"memory", "cc");
	return cc;
}

unsigned long clockticks_in_msec(unsigned long b, unsigned long a)
{
	return (b - a) / 4096000UL;
}

int main()
{

	int cc;
	unsigned long start, end, diff;

	cc = stckf(&start);
	if (cc)
		printf("cc != 0!\n");
	sleep(1);
	cc = stckf(&end);
	if (cc)
		printf("cc != 0!\n");

	diff = clockticks_in_msec(end, start);
	if (diff >= 1000 && diff < 1500)
		printf("OK.....Testcase passed\n");
	else
		printf("FAILED.....Testcase failed\n");
	return 0;

}
