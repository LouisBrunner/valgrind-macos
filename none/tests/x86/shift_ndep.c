#include "tests/asm.h"
#include <stdio.h>

/* Test whether a shift by zero properly preserves the CC_NDEP thunk. */

/* Check whether the carry flag is properly preserved by a variable
   shift when the shift amount happens to be zero. */
int shift_ndep( void )
{
  char shift_amt = 0;
  int x = -2;
  /* First we set the carry flag. Then we increment %x, which sets
     CC_OP to X86G_CC_OP_INCL and stores the carry (1) in
     CC_NDEP. Then we left shift %x by a variable amount that happens
     to be zero, which should leave both %x and all the flags
     unchanged. Then we add-with-carry 0 to %x, which (assuming the
     carry is still set as it should be) increments %x again. Thus the
     expected final value for x is -2 + 1 + 1 = 0.

     If instead the shift clears CC_NDEP (as it would legally do if
     the shift amount were non-zero), this will be interpeted as
     clearing the carry bit, so the adc will be a no-op and the final
     value of %x will instead be -1.
  */
  asm (
       "stc"                    "\n\t"
       "inc %[x]"               "\n\t"
       "shl %[shift_amt], %[x]" "\n\t"
       "adc $0, %[x]"           "\n\t"
       : [x] "+r" (x) : [shift_amt] "c" (shift_amt));
  return x;
}

int main ( void )
{
  int r = shift_ndep();
  if (r == 0)
    printf("Passed (%d).\n", r);
  else
    printf("Failed (%d).\n", r);
  return 0;
}
