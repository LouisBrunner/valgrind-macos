/* test segment register getting */
#include <stdio.h>

int main()
{
	unsigned short gsw;
	unsigned long  gsl;

	asm ("mov $~0, %0; movl %%gs, %0" : "=r" (gsl));
	asm ("mov $~0, %0; movw %%gs, %0" : "=r" (gsw));

	printf("%s\n", gsl == gsw ? "PASS" : "FAIL");

	return 0;
}
