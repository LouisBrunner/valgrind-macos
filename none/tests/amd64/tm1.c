
#include <stdio.h>
#include <stdlib.h>

/* An ultra-lame test program for RTM support, as available
   on Haswell CPUs. */

/* Attempt to run f(arg) as a transaction, and return a Boolean
   indicating success or otherwise. */

__attribute__((noinline))
static int transactionally_apply ( void(*f)(void*), void* arg )
{
  register int ok;
  __asm__ __volatile__(
     "  xbegin .Lzzqqfail" );
  f(arg);
  __asm__ __volatile__( 
     /* This is a bit tricky.  If the transaction succeeds, control
        will flow to this point.  If it fails, control continues at
        .Lzzqqfail, with the machine state looking the same as it did
        immediately before the xbegin was executed. */
     "  xend           \n\t"  /* declare the transaction to be complete */
     "  movl $1,%0     \n\t"  /* "ok = 1" */
     "  jmp .Lzzqqout  \n\t"  /* jump to the merge point */
     ".Lzzqqfail:      \n\t"  /* it failed .. */
     "  movl $0,%0     \n\t"  /* "ok = 0" */
     ".Lzzqqout:       \n\t"  /* this is the merge point */
     : "=r"(ok) : : "cc", "rax"
  );
  return ok;
}

void testfn ( void* arg )
{
}

int main ( void )
{
  long long int ok = transactionally_apply ( testfn, NULL );
  printf("transactionally_apply: ok = %lld (expected %d)\n", ok, 0);

  __asm__ __volatile__(
    "movq $0, %%rax  \n\t"
    "xtest           \n\t"   
    "setz %%al       \n\t"
    "movq %%rax, %0  \n\t"
    : "=r"(ok) : : "cc","rax"
  );
  printf("xtest: rflags.Z = %lld (expected %d)\n", ok, 1);

  /*
  printf("testing XACQUIRE / XRELEASE\n");
  int n = 0;
  __asm__ __volatile__(
     "xacquire lock incl (%0)   \n\t"
     "xrelease lock decl (%0)   \n\t"
     : : "r"(&n) : "cc", "memory"
  );
  */

  __asm__ __volatile__( "xabort $0x1" );
  printf("xabort: outside transaction is nop.\n");

  return 0;
}
