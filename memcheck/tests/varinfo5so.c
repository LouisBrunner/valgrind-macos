
/* A concatenation of varinfo1 .. varinfo4 in a shared object.  This
   is to check for correct functionality in a non-zero-biased ELF
   executable. */

/* Relevant compile flags are:

   -Wall -g -I$prefix/include/valgrind

   eg -Wall -g -I`pwd`/Inst/include/valgrind
*/

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "memcheck/memcheck.h"

/* Cause memcheck to complain about the address "a" and so to print
   its best guess as to what "a" actually is.  a must be
   addressible. */
__attribute__((noinline))
void croak ( void* aV )
{
  char* a = (char*)aV;
  char* undefp = malloc(1);
  char saved = *a;
  assert(undefp);
  *a = *undefp;
  (void) VALGRIND_CHECK_MEM_IS_DEFINED(a, 1);
  *a = saved;
  free(undefp);
}

#include <stdio.h>

/* ------------ varinfo1 ------------ */

int global_u1;

int global_i1 = 17;

char global_u2[10];

char global_i2[10] = { 1,2,3,4,5,6,7,8,9,10 };

__attribute__((noinline))
static int varinfo1_main ( void )
{
  int local;
  char* onheap = malloc(3);
  assert(onheap);
  croak(onheap+1);
  free(onheap);

  croak( &global_u1 );
  croak( &global_i1 );
  croak( &global_u2[3] );
  croak( &global_i2[7] );
  croak( &local );
  return 0;
}

/* ------------ varinfo2 ------------ */
__attribute__((noinline))
static void foo2 ( void )
{
  int var;
  var = 1;
  { char var[10];
    var[6] = 4;
    croak( &var[7] );
    { struct { double foo; float bar; } var;
      croak ( 2 + (char*)&var.bar );
    }
  }
  croak( 1 + (char*)&var );
}
__attribute__((noinline))
static int varinfo2_main ( void )
{
  foo2();
  return 0;
}

/* ------------ varinfo3 ------------ */

static char static_global_def[10]    = {0,0,0,0,0, 0,0,0,0,0};
       char nonstatic_global_def[10] = {0,0,0,0,0, 0,0,0,0,0};
static char static_global_undef[10];
       char nonstatic_global_undef[10];
__attribute__((noinline))
static void bar3 ( char* p1, char* p2, char* p3, char* p4 )
{
   croak(p1);
   croak(p2);
   croak(p3);
   croak(p4);
}
__attribute__((noinline))
static void foo3 ( void )
{
   static char static_local_def[10]    = {0,0,0,0,0, 0,0,0,0,0};
          char nonstatic_local_def[10] = {0,0,0,0,0, 0,0,0,0,0};
   static char static_local_undef[10];
          char nonstatic_local_undef[10];
   croak ( 1 + (char*)&static_global_def );
   croak ( 2 + (char*)&nonstatic_global_def );
   croak ( 3 + (char*)&static_global_undef );
   croak ( 4 + (char*)&nonstatic_global_undef );
   bar3( 5 + (char*)&static_local_def,
         6 + (char*)&nonstatic_local_def,
         7 + (char*)&static_local_undef,
         8 + (char*)&nonstatic_local_undef );
}
__attribute__((noinline))
static int varinfo3_main ( void )
{
  foo3();
  return 0;
}

/* ------------ varinfo4 ------------ */

#include <string.h>

typedef struct { short c1; char* c2[3]; } XX;

typedef
   struct _str { int bing; int bong; XX xyzzy[77]; }
   Str;

__attribute__((noinline))
static int blah4 ( int x, int y )
{
  Str a[10];
  memset(a, 0, sizeof(a));
  croak(1 + (char*)(&a[3].xyzzy[x*y].c1));
  croak( (char*)(&a[5].bong) );
  croak( 1 + (char*)(&a[3].xyzzy[x*y].c2[2]) );
  memset(a, 0, sizeof(a));
  return a[3].xyzzy[x*y].c1;
}
__attribute__((noinline))
static int varinfo4_main ( void )
{
  fprintf(stderr, "answer is %d\n", blah4(3,7) );
  return 0;
}

/* ------------ varinfo5 ------------ */

void varinfo5_main ( void )
{
   varinfo1_main();
   varinfo2_main();
   varinfo3_main();
   varinfo4_main();
}
