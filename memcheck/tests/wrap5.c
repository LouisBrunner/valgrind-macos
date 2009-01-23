
#include <stdio.h>
#include <stdlib.h>
#include "valgrind.h"

/* As wrap4.c, but also throw in various calls to another redirected
   function (malloc) to check that that doesn't screw anything up.
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
   printf("in wrapper1-pre:  fact(%d)\n", n);
   addMoreLard();
   CALL_FN_W_W(r, fn, n);
   addMoreLard();
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
   addMoreLard();
   CALL_FN_W_W(r, fn, n);
   addMoreLard();
   printf("in wrapper2-post: fact(%d) = %d\n", n, r);
   return r;
}

/* --------------- */

int main ( void )
{
   int r;
   Lard *p, *p_next;
   printf("computing fact1(7)\n");
   r = fact1(7);
   printf("fact1(7) = %d\n", r);

   printf("allocated %d Lards\n", ctr);
   for (p = lard; p; p = p_next) {
      p_next = p->next;
      free(p);
   }

   return 0;
}
