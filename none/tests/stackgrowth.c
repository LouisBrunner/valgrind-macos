#include <stdio.h>
#include <string.h>

#define DEPTH	(4*1024)
#define FRAME	(1024)

static void test(int depth)
{
	volatile char frame[FRAME];

	memset((char *)frame, 0xff, sizeof(frame));

	if (depth > 1)
		test(depth-1);
}

int main()
{
	test(DEPTH);

	printf("PASSED\n");
	return 0;
}
