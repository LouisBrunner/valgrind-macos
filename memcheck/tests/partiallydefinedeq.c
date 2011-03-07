
#include <stdio.h>
#include <stdlib.h>

// Do a test comparison.  By default memcheck does not use the
// expensive EQ/NE scheme as it would be too expensive.  The 
// assignment to *hack is a trick to fool memcheck's bogus-literal
// spotter into thinking this is a bb which needs unusually careful
// attention, and therefore the expensive EQ/NE scheme is used.

__attribute__((noinline)) // keep your grubby hands off this fn
void foo ( int* p1, int* p2, unsigned int * hack )
{
  *hack = 0x80808080;
  if (*p1 == *p2)
    printf("foo\n");
  else
    printf("bar\n");
}

static void bar ( void );
int main ( void )
{

  unsigned int hack;

  int* junk1 = malloc(sizeof(int));
  int* junk2 = malloc(sizeof(int));

  short* ps1 = (short*)junk1;
  short* ps2 = (short*)junk2;

  int*   pi1 = (int*)junk1;
  int*   pi2 = (int*)junk2;
  bar();
  // both words completely undefined.  This should give an error.
  foo(pi1,pi2, &hack);

  // set half of the words, but to different values; so this should
  // not give an error, since inspection of the defined parts 
  // shows the two values are not equal, and so the definedness of
  // the conclusion is unaffected by the undefined halves.
  *ps1 = 41;
  *ps2 = 42;
  foo(pi1,pi2, &hack);

  // set half of the words, but to the same value, so this forces the
  // result of the comparison to depend on the undefined halves.
  // should give an error
  *ps1 = 42;
  *ps2 = 42;
  foo(pi1,pi2, &hack);

  return 0;
}

// Note: on ppc32/64 the second call to foo() does give an error,
// since the expensive EQ/NE scheme does not apply to the CmpORD
// primops used by ppc.
//
// On arm, the "normal" (x86-like) comparison primops are used, so
// the expensive EQ/NE scheme could apply.  However, it doesn't,
// because the constant 0x80808080 is placed in a constant pool
// and so never appears as a literal, and so the instrumenter
// never spots it and so doesn't use the expensive scheme (for foo).
// Hence also on ARM we get 3 errors, not 2.
//
// s390x is even more complicated: Depending on the architecture
// level we have the 0x80808080 either in the literal pool (3 errors)
// or with the extended immediate facility in an instruction (2 errors).
static __attribute__((noinline)) void bar ( void )
{
#if defined(__powerpc__) || defined(__powerpc64__) || defined(__arm__)
  fprintf(stderr, "Currently running on ppc32/64/arm: this test should give 3 errors, not 2.\n");
#endif
#if defined(__s390__)
  fprintf(stderr, "On s390 we might see 2 or 3 errors.\n");
#endif
}
