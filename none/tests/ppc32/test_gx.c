
#include <stdio.h>
#include "tests/malloc.h"

/* This is a Marie Celeste instruction.  Some IBM documents think it
   exists, others don't.  The same appears to be true for
   implementations - ppc970 doesn't think it exists, but POWER5
   does. */
double do_fre ( double x )
{
  double block[2];
  block[0] = x;
  __asm__ __volatile__(
     "lfd %%f1, 0(%0)\n\t"
     ".long 0xfc200830\n\t" /* == fre %%f1,%%f1 */
     "stfd %%f1, 8(%0)"
     : /*out*/
     : /*in*/ "b" (&block[0])
     : /*trash*/ "memory", "fr1"
  );
  return block[1];
}

double do_fres ( double x )
{
  double block[2];
  block[0] = x;
  __asm__ __volatile__(
     "lfd %%f1, 0(%0)\n\t"
     "fres %%f1,%%f1\n\t"
     "stfd %%f1, 8(%0)"
     : /*out*/
     : /*in*/ "b" (&block[0])
     : /*trash*/ "memory", "fr1"
     );
  return block[1];
}

double do_frsqrte ( double x )
{
  double block[2];
  block[0] = x;
  __asm__ __volatile__(
     "lfd %%f1, 0(%0)\n\t"
     "frsqrte %%f1,%%f1\n\t"
     "stfd %%f1, 8(%0)"
     : /*out*/
     : /*in*/ "b" (&block[0])
     : /*trash*/ "memory", "fr1"
     );
  return block[1];
}

/* Another Marie Celeste insn. */
double do_frsqrtes ( double x )
{
  double block[2];
  block[0] = x;
  __asm__ __volatile__(
     "lfd %%f1, 0(%0)\n\t"
     ".long 0xec200834\n\t" /* == frsqrtes %%f1,%%f1 */
     "stfd %%f1, 8(%0)"
     : /*out*/
     : /*in*/ "b" (&block[0])
     : /*trash*/ "memory", "fr1"
     );
  return block[1];
}

////////////////////////////////////////////////////////////

void do_one ( char* name, 
              double(*f)(double), 
              double* args, int nargs, 
              char* argfmt, char* resfmt )
{
  int i;
  double a, r;
  printf("\n");

  for (i = 0; i < nargs; i++) {
    a = args[i];
    r = f(a);
    printf("%s ", name);
    printf(argfmt, a);
    printf(" -> ");
    printf(resfmt, r);
    printf("\n");
  }
}

int main ( void )
{
  int nargs = 19;
  double* args = malloc(nargs * sizeof(double));
  args[0]  =  0.0;
  args[1]  =  1.0 / 0.0; // inf
  args[2]  = -args[1]; //  -inf
  args[3]  = args[2]/args[2]; // nan
  args[4]  = -args[3]; // -nan
  args[5]  = -5e100;
  args[6]  = -5e20;
  args[7]  = -501.0;
  args[8]  = -6.0;
  args[9]  = -1.01;
  args[10] = -2e-20;
  args[11] = -2e-200;
  args[12] =  2e-200;
  args[13] =  2e-20;
  args[14] =  1.01;
  args[15] =  6.0;
  args[16] =  501.0;
  args[17] =  5e20;
  args[18] =  5e100;

  do_one( "fre",  do_fre,  args, nargs, "%e", "%4.1e");
  do_one( "fres", do_fres, args, nargs, "%e", "%4.1e");

  do_one( "frsqrte",  do_frsqrte,  args, nargs, "%e", "%4.1e");
  do_one( "frsqrtes", do_frsqrtes, args, nargs, "%e", "%4.1e");

  free(args);
  return 0;
}
