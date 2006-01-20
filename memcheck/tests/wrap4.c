
#include <stdio.h>
#include "valgrind.h"

/* Check that it's safe to call a wrapped function from some other
   function's wrapper.  Note that because the wrapper for fact1
   actually interferes with the computation of the result, this
   program produces a different answer when run on V (162) from
   natively (120).
*/

static int fact1 ( int n );
static int fact2 ( int n );

/* This is needed to stop gcc4 turning 'fact' into a loop */
__attribute__((noinline))
int mul ( int x, int y ) { return x * y; }

int fact1 ( int n )
{
   if (n == 0) return 1; else return mul(n, fact2(n-1));
}
int fact2 ( int n )
{
   if (n == 0) return 1; else return mul(n, fact1(n-1));
}


int I_WRAP_SONAME_FNNAME_ZU(NONE,fact1) ( int n )
{
   int    r;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   printf("in wrapper1-pre:  fact(%d)\n", n);
   CALL_FN_W_W(r, fn, n);
   printf("in wrapper1-post: fact(%d) = %d\n", n, r);
   if (n >= 3) r += fact2(2);
   return r;
}

int I_WRAP_SONAME_FNNAME_ZU(NONE,fact2) ( int n )
{
   int    r;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   printf("in wrapper2-pre:  fact(%d)\n", n);
   CALL_FN_W_W(r, fn, n);
   printf("in wrapper2-post: fact(%d) = %d\n", n, r);
   return r;
}

/* --------------- */

int main ( void )
{
   int r;
   printf("computing fact1(5)\n");
   r = fact1(5);
   printf("fact1(5) = %d\n", r);
   return 0;
}
