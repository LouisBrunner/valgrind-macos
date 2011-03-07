#include <stdio.h>
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
			:"=m" (__cc)::"memory"); \
	__cc; \
})

static inline void dump_field(void *field, int size)
{
	int i;
	for (i=0; i < size; i++)
		printf("%2.2X ", ((char *) field)[i]);
}
