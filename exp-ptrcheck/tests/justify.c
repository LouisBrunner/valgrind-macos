#include <stdlib.h>
#include <assert.h>

// This is an example of an error found by Annelid, but not found by
// Memcheck -- because the wild read goes past the redzones of the pointer's
// block.
//
// Nb: for Memcheck to not spot this, relies on it putting the 2nd block in
// memory after the 1st block.

int main ( void )
{
   char c;
   char *c0, *c1;

   c0 = malloc(10000);
   c1 = malloc(10000);
   assert(c0 && c1);

   c = c0[15000];

   return 0;
}
