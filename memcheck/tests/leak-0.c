#include <stdlib.h>
#include "../memcheck.h"

/* 
   Live 0-sized blocks were being reported as leaked.
 */
int main()
{
	void *volatile foo = malloc(0);

	//printf("&foo=%p foo=%p\n", &foo, foo);
	VALGRIND_DO_LEAK_CHECK;

	free(foo);
	return 0;
}
