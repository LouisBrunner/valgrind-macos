/* test data class tests for float, double, long double:  TCEB, TCDB, TCXB */
#include <math.h>
#include <stdio.h>

static int tcxb(long double f, long long num)
{
	int match;

	asm volatile("	tcxb %1,0(%2)\n"
			"ipm %0\n"
			"srl %0,28\n"
			: "=d" (match)
			: "f" (f), "a" (num)
			: "cc");
	return match;
}

static int tcdb(double f, long long num)
{
	int match;

	asm volatile("	tcdb %1,0(%2)\n"
			"ipm %0\n"
			"srl %0,28\n"
			: "=d" (match)
			: "f" (f), "a" (num)
			: "cc");
	return match;
}

static int tceb(float f, long long num)
{
	int match;

	asm volatile("	tceb %1,0(%2)\n"
			"ipm %0\n"
			"srl %0,28\n"
			: "=d" (match)
			: "f" (f), "a" (num)
			: "cc");
	return match;
}

int main()
{
	int i;

	for (i = 0; i < 64; i++) {
		if (sizeof (long double) == 16) {
			/* long double 128 bit */
			printf("%d", tcxb(+0.0l,	1UL<<i));
			printf("%d", tcxb(-0.0l,	1UL<<i));
			printf("%d", tcxb(+2.2l,	1UL<<i));
			printf("%d", tcxb(-2.2l,	1UL<<i));
			printf("%d", tcxb(+INFINITY,	1UL<<i));
			printf("%d", tcxb(-INFINITY,	1UL<<i));
			printf("%d", tcxb(+NAN,		1UL<<i));
			printf("%d", tcxb(-NAN,		1UL<<i));
		} else {
			/* long double 64 bit */
			printf("%d", tcdb(+0.0l,	1UL<<i));
			printf("%d", tcdb(-0.0l,	1UL<<i));
			printf("%d", tcdb(+2.2l,	1UL<<i));
			printf("%d", tcdb(-2.2l,	1UL<<i));
			printf("%d", tcdb(+INFINITY,	1UL<<i));
			printf("%d", tcdb(-INFINITY,	1UL<<i));
			printf("%d", tcdb(+NAN,		1UL<<i));
			printf("%d", tcdb(-NAN,		1UL<<i));
		}
		/* double 64 bit */
		printf("%d", tcdb(+0.0,		1UL<<i));
		printf("%d", tcdb(-0.0,		1UL<<i));
		printf("%d", tcdb(+2.2,		1UL<<i));
		printf("%d", tcdb(-2.2,		1UL<<i));
		printf("%d", tcdb(+INFINITY,	1UL<<i));
		printf("%d", tcdb(-INFINITY,	1UL<<i));
		printf("%d", tcdb(+NAN,		1UL<<i));
		printf("%d", tcdb(-NAN,		1UL<<i));


		/* float 32 bit */
		printf("%d", tceb(+0.0f,	1UL<<i));
		printf("%d", tceb(-0.0f,	1UL<<i));
		printf("%d", tceb(+2.2f,	1UL<<i));
		printf("%d", tceb(-2.2f,	1UL<<i));
		printf("%d", tceb(+INFINITY,	1UL<<i));
		printf("%d", tceb(-INFINITY,	1UL<<i));
		printf("%d", tceb(+NAN,		1UL<<i));
		printf("%d", tceb(-NAN,		1UL<<i));

		printf("\n");

	}
	return 0;
}
