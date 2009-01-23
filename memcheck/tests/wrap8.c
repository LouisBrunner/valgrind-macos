#include <unistd.h>
#include <stdio.h>
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
   Note, pre() and post() used so as to avoid printf, which messes
   up the call stacks on ppc64-linux due to intercept of mempcpy.
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
static void post ( char* s, int n, int r );
static void pre ( char* s, int n );
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
   pre("wrapper1", n);
   addMoreLard();
   CALL_FN_W_W(r, fn, n);
   addMoreLard();
   post("wrapper1", n, r);
   if (n >= 3) r += fact2(2);
   return r;
}

int I_WRAP_SONAME_FNNAME_ZU(NONE,fact2) ( int n )
{
   int    r;
   OrigFn fn;
   VALGRIND_GET_ORIG_FN(fn);
   pre("wrapper2", n);
   addMoreLard();
   CALL_FN_W_W(r, fn, n);
   addMoreLard();
   post("wrapper2", n, r);
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

static void send ( char* s )
{
  while (*s) {
    write(1, s, 1);
    s++;
  }
}

static void pre ( char* s, int n )
{
  char buf[50];
  fflush(stdout);
  sprintf(buf,"%d", n);
  send("in ");
  send(s);
  send("-pre:  fact(");
  send(buf);
  send(")\n");
  fflush(stdout);
}

static void post ( char* s, int n, int r )
{
  char buf[50];
  fflush(stdout);
  sprintf(buf,"%d", n);
  send("in ");
  send(s);
  send("-post: fact(");
  send(buf);
  send(") = ");
  sprintf(buf,"%d", r);
  send(buf);
  send("\n");
  fflush(stdout);
}
