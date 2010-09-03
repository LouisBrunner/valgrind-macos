#include <stdio.h>
#include <stdlib.h>
#include "leak.h"
#include "../memcheck.h"

struct n {
	struct n *l;
	struct n *r;
        // This ensures it's the same size on 32-bit and 64-bit platforms.
        char padding[ 2 * (8 - sizeof(struct n*)) ];
};

struct n *mk(struct n *l, struct n *r)
{
	struct n *n = malloc(sizeof(struct n));
	n->l = l;
	n->r = r;

	return n;
}

static struct n *mkcycle()
{
	register struct n *a, *b, *c;

	a = mk(0,0);
	b = mk(a,0);
	c = mk(b,0);
	a->l = c;

	return a;
}


int main()
{
	DECLARE_LEAK_COUNTERS;

	struct n *volatile c1, *volatile c2;

        GET_INITIAL_LEAK_COUNTS;

	/* two simple cycles */
	c1 = mkcycle();
	c2 = mkcycle();

	c1 = c2 = 0;

	/* one cycle linked to another */
	c1 = mkcycle();
	c2 = mkcycle();

	/* This is to make sure we end up merging cliques; see
	   mc_leakcheck.c */
	if (c1 < c2)
		c2->r = c1;
	else
		c1->r = c2;

	c1 = c2 = 0;

	/* two linked cycles */
	c1 = mkcycle();
	c2 = mkcycle();

	c1->r = c2;
	c2->r = c1;

	c1 = c2 = 0;

	CLEAR_CALLER_SAVED_REGS;

	GET_FINAL_LEAK_COUNTS;

	PRINT_LEAK_COUNTS(stderr);

	return 0;
}
