#include <stdio.h>

static signed int test[] ={
	0,
	1,
	-1,
	0x7fffffff,
	0x80000000,
	0x12345678,
	0x87654321,
	0x55555555,
	0x11111111,
	0xaaaaaaaa,
};


static unsigned long hex_to_dec(signed int num)
{
	unsigned long dec;
	asm ("cvd %1,%0" : "=S" (dec) : "d" (num) : );
	return dec;
}

int main()
{
	int i;

	for (i = 0; i < sizeof(test) / sizeof(test[0]); i++)
		printf("%lx\n", hex_to_dec(test[i]));
	return 0;
}
