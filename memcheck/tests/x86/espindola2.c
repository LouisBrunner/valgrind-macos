
#include <stdlib.h>
#include <assert.h>

/* This should run without comment, but 3.2.1 (and presumably earlier)
   give a false uninit-value warning.  This was fixed by vex r1675
   which is a spec rule for COPY-CondP. */

int main ( void )
{
  int* x = malloc(4);
  assert(x);
  __asm__ __volatile__(
     "finit\n\t"
     "ffree %%st(0)\n\t"
     "ffree %%st(1)\n\t"
     "ffree %%st(2)\n\t"
     "ffree %%st(3)\n\t"
     "ffree %%st(4)\n\t"
     "ffree %%st(5)\n\t"
     "ffree %%st(6)\n\t"
     "ffree %%st(7)\n\t"
     "andb $128, (%0)\n\t"
     "fldz\n\t"
     "fldz\n\t"
     "fucompp\n\t"
     "fnstsw %%ax\n\t"
     "sahf\n\t"
     "jp .Lfoobar\n"
     ".Lfoobar:\n\t"
     "nop"
     : : "r"(x) : "eax", "cc"
  );
  free(x);
  return 0;
}
