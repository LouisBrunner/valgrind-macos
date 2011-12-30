/*
 * test case for valgrind realloc() bug
 */

#include <stdlib.h>
#include <assert.h>

int
main(void)
{
	void *p;
	void *r;

	p = malloc(1);
	assert(p != NULL);

	r = realloc(p, -1);
	assert(r == NULL);

	free(p);

	return 0;
}


