#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "../memcheck.h"
#include "leak.h"

/* 
   Incompetent memory management

   Simulate what happens when trying to manage a dense brancy cyclic
   structure with refcounting.
 */

static long bytes, blocks;

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
        long total;
        DECLARE_LEAK_COUNTERS;

        /* we require these longs to have same size as a machine word */
        assert(sizeof(long) == sizeof(void*));

	/* get a baseline in case the runtime allocated some memory */
        GET_INITIAL_LEAK_COUNTS;

	for (i = 0; i < ITER; i++) {
		mk();

		if ((i % (ITER/10)) == 0) {
			if (0)
				printf("%ld living blocks, %ld bytes\n",
				       blocks, bytes);
			//VALGRIND_DO_LEAK_CHECK;
		}
	}
	
	/* "free all memory" */
	for (i = 0; i < N; i++)
		assign(&nodes[i], NULL);


	if (0)
		printf("FINISHED: %ld living blocks, %ld bytes\n",
		       blocks, bytes);

        GET_FINAL_LEAK_COUNTS;

	total = L_bytes + D_bytes + R_bytes + S_bytes;

        if (0) {
		PRINT_LEAK_COUNTS(stderr);
        }

	if (R_bytes != 0)
		printf("FAILED: I freed everything, "
		       "but there's still %ld bytes (in %ld blocks) reachable\n", 
		       R_bytes, R_blocks);
	else if (total != bytes)
		printf("FAILED: I count %ld bytes, leakcheck says %ld\n",
		       bytes, total);
	else
		printf("PASS\n");
	return 0;
}
