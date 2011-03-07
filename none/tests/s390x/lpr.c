#include <limits.h>
#include <stdio.h>

int lpr(int org, int *new)
{
	int _new, cc;
	asm volatile(	"lpr %0,%2\n\t"
			"ipm %1\n\t"
			"srl %1,28\n\t"
			: "=d" (_new), "=d" (cc)
			: "d" (org)
			: "cc");
	*new = _new;
	return cc;
}

int lpgr(unsigned long org, unsigned long *new)
{
	unsigned long _new;
        int cc;
	asm volatile(	"lpgr %0,%2\n\t"
			"ipm %1\n\t"
			"srl %1,28\n\t"
			: "=d" (_new), "=d" (cc)
			: "d" (org)
			: "cc");
	*new = _new;
	return cc;
}

int lpgfr(unsigned long org, unsigned long *new)
{
	unsigned long _new;
        int cc;
	asm volatile(	"lpgfr %0,%2\n\t"
			"ipm %1\n\t"
			"srl %1,28\n\t"
			: "=d" (_new), "=d" (cc)
			: "d" (org)
			: "cc");
	*new = _new;
	return cc;
}


void t32(int value)
{
	int n,cc;

	cc = lpr(value, &n);

	printf("new: %d cc: %d\n", n, cc);
}

void t64(unsigned long value)
{
	int cc;
        unsigned long n;

	cc = lpgr(value, &n);

	printf("new: %ld cc: %d\n", n, cc);
}

void t3264(unsigned long value)
{
	int cc;
        unsigned long n;

	cc = lpgfr(value, &n);

	printf("new: %ld cc: %d\n", n, cc);
}



int main()
{
	printf("lpr\n");
	t32(0); t32(1); t32(-1);
	t32(INT_MAX); t32(INT_MIN); t32(UINT_MAX);

	printf("lpgr\n");
	t64(0); t64(1); t64(-1);
	t64(INT_MAX); t64(INT_MIN); t64(UINT_MAX);
	t64(LONG_MAX); t64(LONG_MIN); t64(ULONG_MAX);

	printf("lpgfr\n");
	t3264(0); t3264(1); t64(-1);
	t3264(INT_MAX); t3264(INT_MIN); t3264(UINT_MAX);
	t3264(LONG_MAX); t3264(LONG_MIN); t3264(ULONG_MAX);

	return 0;
}

