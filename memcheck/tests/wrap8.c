
#include <stdio.h>
#include <malloc.h>
#include <stdlib.h>
#include "valgrind.h"

/* This is the same as wrap5.c, except that the recursion depth is 16.
   This is intended to check that on ppc64-linux, which uses a
   16-entry per-thread stack, the resulting stack overflow is caught.
   (Undetected overflows in redirection stacks are very bad news; they
   cause guest code to fail in all sorts of strange ways.)

   Hence this test has two expected outcomes:
   - on ppc64-linux, a stack overflow is caught, and V aborts.
   - on everything else, it runs successfully to completion.
*/

typedef 
   struct _Lard {
      struct _Lard* next; 
      char stuff[999]; 
   }
   Lard;

Lard* lard = NULL;
static int ctr = 0;

void addMoreLard ( void )
{
   Lard* p;
   ctr++;
   if ((ctr % 3) == 1) {
      p = malloc(sizeof(Lard));
      p->next = lard;
      lard = p;
   }
}


static int fact1 ( int n );
static int fact2 ( int n );

/* This is needed to stop gcc4 turning 'fact' into a loop */
__attribute__((noinline))
int mul ( int x, int y ) { return x * y; }

int fact1 ( int n )
{
   addMoreLard();
   if (n == 0) return 1; else return mul(n, fact2(n-1));
}
int fact2 ( int n )
{
   addMoreLard();
   if (n == 0) return 1; else return mul(n, fact1(n-1));
}


int I_WRAP_SONAME_FNNAME_ZU(NONE,fact1) ( int n )
{
   int    r;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   printf("in wrapper1-pre:  fact(%d)\n", n); fflush(stdout);
   addMoreLard();
   CALL_FN_W_W(r, fn, n);
   addMoreLard();
   printf("in wrapper1-post: fact(%d) = %d\n", n, r); fflush(stdout);
   if (n >= 3) r += fact2(2);
   return r;
}

int I_WRAP_SONAME_FNNAME_ZU(NONE,fact2) ( int n )
{
   int    r;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   printf("in wrapper2-pre:  fact(%d)\n", n); fflush(stdout);
   addMoreLard();
   CALL_FN_W_W(r, fn, n);
   addMoreLard();
   printf("in wrapper2-post: fact(%d) = %d\n", n, r); fflush(stdout);
   return r;
}

/* --------------- */

int main ( void )
{
   int r, n = 15; /* 14 succeeds on ppc64-linux, >= 15 fails */
   Lard *p, *p_next;
   printf("computing fact1(%d)\n", n); fflush(stdout);
   r = fact1(n);
   printf("fact1(%d) = %d\n", n, r); fflush(stdout);

   printf("allocated %d Lards\n", ctr); fflush(stdout);
   for (p = lard; p; p = p_next) {
      p_next = p->next;
      free(p);
   }

   return 0;
}
