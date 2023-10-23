#ifndef TEST_H
#define TEST_H

#include <stdio.h>
#include <sys/time.h>
#include "svc.h"

/* Convenience macros and functions for testcases */

#define EXIT(ret)  svc1(__NR_exit, ret)

/* X must be a string constant */
#define SAY(x) say(x"\n", sizeof x)

/* Test COND and if it happens to be true, say so */
#define TEST(cond) do { if (cond) SAY(#cond); } while (0)

#define BRASLCLOBBER "0","1","2","3","4","5","14", \
		     "f0","f1","f2","f3","f4","f5","f6","f7"

int say(const char *text, unsigned long num_chars)
{
	return svc3(4, 1, (unsigned long) text, num_chars);
}

#define get_cc() \
({ \
	char __cc; \
	/* dont use IPM to better test spechelpers */ \
	asm volatile(	"	brc 8,1f\n\t" \
			"	brc 4,2f\n\t" \
			"	brc 2,3f\n\t" \
			"	brc 1,4f\n\t" \
			"	mvi %0,4\n\t" \
			"	j 0f\n\t" \
			"1:	mvi %0,0\n\t" \
			"	j 0f\n\t" \
			"2:	mvi %0,1\n\t" \
			"	j 0f\n\t" \
			"3:	mvi %0,2\n\t" \
			"	j 0f\n\t" \
			"4:	mvi %0,3\n\t" \
			"	j 0f\n\t" \
			"0:	bcr 0,0 /*nop*/\n\t" \
			:"=Q" (__cc)::"memory"); \
	__cc; \
})

static inline void dump_field(void *field, int size)
{
	int i;
	for (i=0; i < size; i++)
		printf("%2.2X ", ((char *) field)[i]);
}

static inline void mysleep(long sec, long usec)
{
	struct timeval tv;

	tv.tv_sec = sec;
	tv.tv_usec = usec;

	svc5(SYS_select, 0,0,0,0,(unsigned long) &tv);
}

#endif /* TEST_H */
