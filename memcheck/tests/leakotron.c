#include <stdio.h>
#include <stdlib.h>
#include "../memcheck.h"

/* 
   Incompetent memory management

   Simulate what happens when trying to manage a dense brancy cyclic
   structure with refcounting.
 */

static int bytes, blocks;

struct node {
	struct node *l, *r;
	int ref;
};

static void ref(struct node *n)
{
	if (n)
		n->ref++;
}

static void unref(struct node *n)
{
	if (n && --n->ref == 0) {
		bytes -= sizeof(*n);
		blocks--;
		free(n);
	}
}

static struct node *alloc()
{
	struct node *n = malloc(sizeof(*n));
	n->l = n->r = NULL;
	n->ref = 0;

	bytes += sizeof(*n);
	blocks++;

	return n;
}

static void assign(struct node **np, struct node *n)
{
	ref(n);
	unref(*np);

	*np = n;
}

static int rrange(int range)
{
	long long ret = rand() * (long long)range;

	return (int)(ret / RAND_MAX);
}

#define ITER	100000
#define N	1000

static struct node *nodes[N];

static struct node *mk()
{
	int idx = rrange(N);
	struct node *n;

	/* 
	   50% recycle
	   25% alloc new
	   25% return NULL
	 */
	if (rand() > RAND_MAX/2) {
		/* recycle existing block */
		n = nodes[idx];
	} else if (rand() > RAND_MAX/2) {
		/* alloc new block */
		n = alloc();
		assign(&n->l, mk());
		assign(&n->r, mk());
	} else {
		/* no block */
		n = NULL;
	}

	assign(&nodes[idx], n);

	return n;
}

int main()
{
	int i;
	int base_definite, base_dubious, base_reachable, base_suppressed;
	int definite, dubious, reachable, suppressed, total;

	/* get a baseline in case the runtime allocated some memory */
	VALGRIND_DO_LEAK_CHECK;
	base_definite = base_dubious = base_reachable = base_suppressed = 0;
	VALGRIND_COUNT_LEAKS(base_definite, base_dubious, 
			     base_reachable, base_suppressed);

	for(i = 0; i < ITER; i++) {
		mk();

		if ((i % (ITER/10)) == 0) {
			if (0)
				printf("%d living blocks, %d bytes\n",
				       blocks, bytes);
			VALGRIND_DO_LEAK_CHECK;
		}
	}
	
	/* "free all memory" */
	for(i = 0; i < N; i++)
		assign(&nodes[i], NULL);


	if (0)
		printf("FINISHED: %d living blocks, %d bytes\n",
		       blocks, bytes);

	VALGRIND_DO_LEAK_CHECK;

	/* Shouldn't be necessary, but COUNT_LEAKS doesn't define its
	   result values */
	definite = dubious = reachable = suppressed = 0;
	VALGRIND_COUNT_LEAKS(definite, dubious, reachable, suppressed);

	definite   -= base_definite;
	dubious    -= base_dubious;
	reachable  -= base_reachable;
	suppressed -= base_suppressed;

	total = definite+dubious+reachable+suppressed;

	if (0)
		printf("leaks: definite %d, dubious %d, reachable %d, suppressed %d = %d\n",
		       definite, dubious, reachable, suppressed, total);

	if (reachable != 0)
		printf("FAILED: I freed everything, "
		       "but there's still %d bytes reachable\n", 
		       reachable);
	else if (total != bytes)
		printf("FAILED: I count %d bytes, leakcheck says %d\n",
		       bytes, total);
	else
		printf("PASS\n");
	return 0;
}
