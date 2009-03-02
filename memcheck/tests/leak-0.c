#include <stdio.h>
#include <stdlib.h>
#include "leak.h"
#include "../memcheck.h"

// Live 0-sized blocks were being reported as leaked.
// Also, test that a pointer in a register is handled correctly.
int main()
{
	DECLARE_LEAK_COUNTERS;

	register char *foo;

        GET_INITIAL_LEAK_COUNTS;

	foo = malloc(0);

	GET_FINAL_LEAK_COUNTS;

	PRINT_LEAK_COUNTS(stderr);

	free(foo);
	return 0;
}
