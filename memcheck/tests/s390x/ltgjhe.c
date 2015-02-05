#include <stdio.h>
#include "../../../none/tests/s390x/opcodes.h"
int main()
{
	int field1, field2;
	int result;

	/*
         * gcc does some tricks for checking the highest bit. It seems
         * to load a full word/double word.
         * By using mask=10 for brc (jhe) only the msb is influencing
         * the code flow. This test was inspired by 308427
         */
	asm volatile(	"oi %1,128\n\t"
			"la 1,%1\n\t"
			LTG(0,0,1,000,00)
			"jhe 1f\n\t"
			"lghi %0,0\n\t"
			"j 2f\n\t"
			"1:lghi %0,1\n\t"
			"2:\n\t"
			:"=d" (result)
			:"Q" (field1)
			:"0","cc");

	if (result)
		printf("Error\n");

	asm volatile(	"oi %1,128\n\t"
			"la 1,%1\n\t"
			LT(0,0,1,000,00)
			"jhe 1f\n\t"
			"lghi %0,0\n\t"
			"j 2f\n\t"
			"1:lghi %0,1\n\t"
			"2:\n\t"
			:"=d" (result)
			:"Q" (field2)
			:"0","cc");

	if (result)
		printf("Error\n");

	asm volatile(	"oi %1,128\n\t"
			"la 1,%1\n\t"
			LTG(0,0,1,000,00)
			"jl 1f\n\t"
			"lghi %0,1\n\t"
			"j 2f\n\t"
			"1:lghi %0,0\n\t"
			"2:\n\t"
			:"=d" (result)
			:"Q" (field1)
			:"0","cc");

	if (result)
		printf("Error\n");

	asm volatile(	"oi %1,128\n\t"
			"la 1,%1\n\t"
			LT(0,0,1,000,00)
			"jl 1f\n\t"
			"lghi %0,1\n\t"
			"j 2f\n\t"
			"1:lghi %0,0\n\t"
			"2:\n\t"
			:"=d" (result)
			:"Q" (field2)
			:"0","cc");

	if (result)
		printf("Error\n");

	return 0;
}
