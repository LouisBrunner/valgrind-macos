
#include <stdio.h>
#include <assert.h>
#include <dlfcn.h>

/* see comments in preen_invar_so.c for explanation of this */


int main ( void )
{
  int i, r, sum = 0;
  char* im_a_global_array;
  void* hdl = dlopen("./preen_invars_so.so", RTLD_NOW);
  assert(hdl);
  im_a_global_array = dlsym(hdl, "im_a_global_array");
  assert(im_a_global_array);
  /* printf("%p %p\n", im_a_global_array, me_too_me_too); */

  /* poke around in the global array, so as to cause exp-ptrcheck
     to generate an Inv_Global invar for it. */
  for (i = 10/*ERROR*/; i >= 0; i--) {
     sum += im_a_global_array[i];
  }
  /* iterating 10 .. 0 causes an Unknown->Global transition at i = 9.
     We do it this way in order that at the end of a loop, there is a
     Global invar in place for the memory read in the loop, so that
     the subsequent dlclose (hence munmap) causes it to get preened.

     Unfortunately there's nothing to show that the preen was
     successful or happened at all.  The only way to see is from the
     -v output:

     --686--  sg_:  251 Invars preened, of which 1 changed

     It's the "1 changed" bit which is significant.
  */

  /* let's hope gcc is not clever enough to optimise this away, since
     if it does, then it will also nuke the preceding loop, and
     thereby render this test program useless. */

  if (sum & 1) printf("%s bar %d\n", "foo", sum & 1); else
               printf("foo %s %d\n", "bar", 1 - (sum & 1));

  /* Now close (== unmap) the array, so that exp-ptrcheck has to check
     its collection of Inv_Global invars, and remove this one from
     it. */
  r = dlclose(hdl);
  assert(r == 0);

  return 0;
}
