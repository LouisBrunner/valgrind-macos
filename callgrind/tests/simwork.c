// Some work exercising the cache simulator
// with a simple call graph

#include "../callgrind.h"

#include <stdio.h>
#include <stdlib.h>

#define SIZE 100000

double *a, *b, *c;

void init()
{
   int i;
   for(i = 0; i< SIZE; i++) a[i] = b[i] = 1.0;
}

void do_add()
{
   int i;
   for(i = 0; i< SIZE; i++) {
	a[i] += 1.0;
	c[i] = a[i] + b[i];
   }
}

double do_sum()
{
   int i;
   double sum=0.0;

   do_add();
   for(i = 0; i< SIZE; i++) sum += c[i];

   return sum;
}

double do_some_work(int iter)
{
   double sum=0.0;

   if (iter > 0) sum += do_some_work(iter-1);
   do_add();
   sum += do_sum();

   return sum;
}

int main(void)
{
   double res;

   a = (double*) malloc(SIZE * sizeof(double));
   b = (double*) malloc(SIZE * sizeof(double));
   c = (double*) malloc(SIZE * sizeof(double));

   CALLGRIND_ZERO_STATS;
   init();
   res = do_some_work(1);
   CALLGRIND_DUMP_STATS;

   printf("Sum: %.0f\n", res);
   return RUNNING_ON_VALGRIND;
}

