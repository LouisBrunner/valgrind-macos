#include <stdio.h>
#include <stdlib.h>
#include "../memcheck.h"

struct foo {
	struct foo *l, *r;
};

struct foo *mk(struct foo *l, struct foo *r)
{
	struct foo *f = malloc(sizeof(*f));
	f->l = l;
	f->r = r;

	return f;
}

int main()
{
	struct foo *volatile a, *volatile b, *volatile c;

	b = mk(mk(0, 0), 0);

	/* Partially leaked tree structure */
	a = mk(mk(mk(mk(mk(0, 0), 0), mk(mk(0, 0), b)), mk(0, 0)), mk(0,0));
		   
	a = NULL;

	VALGRIND_DO_LEAK_CHECK;

	b = NULL;		/* now leak old b */

	/* Completely leaked, but part of the structure is shared */
	b = mk(0,0);
	b->l = b;
	b->r = b;

	a = mk(b,b);
	c = mk(b,b);

	a = b = c = 0;

	VALGRIND_DO_LEAK_CHECK;

	return 0;
}
