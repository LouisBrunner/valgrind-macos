#include "../memcheck.h"
#include <stdlib.h>

int main()
{
	register char *foo /* asm("esi") */;

	foo = malloc(10);	/* not leaked */

	VALGRIND_DO_LEAK_CHECK;

	free(foo);

	return 0;
}
