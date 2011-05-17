#include <stdio.h>
#include<stdlib.h>
#include<unistd.h>

union stcke {
	unsigned long buffer[2];
	struct reader {
		char pad;
		unsigned long long time;
		int time2;
		short program;
	} __attribute__ ((packed)) reader;
};

int stcke(unsigned long *addr)
{

	int cc;
	asm volatile (	"stcke %0\n"
			"ipm %1\n"
			"srl %1, 28\n"
		      :"+Q" (*addr), "=d"(cc)::"cc");

	return cc;
}

unsigned long clockticks_in_msec(unsigned long b, unsigned long a)
{
	return (b -a ) / 4096000UL;
}

int main()
{
	union stcke start, end;
	int cc;

	cc = stcke(start.buffer);
	if (cc)
		printf("cc != 0!\n");

	sleep(1);
	cc = stcke(end.buffer);
	if (cc)
		printf("cc != 0!\n");

	unsigned long c = clockticks_in_msec(end.reader.time,
					     start.reader.time);

	if (c >= 1000 && c < 1500)
		printf("OK.....Testcase passed\n");
	else
		printf("FAILED.....Testcase failed\n");

	return 0;

}
