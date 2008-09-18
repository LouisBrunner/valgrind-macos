

#include <stdlib.h>
#include <assert.h>

int main ( void )
{
   char c, *c0 = malloc(0), *c1;

   c = *c0;    // bad

   c0 = realloc(c0, 10);
   assert(c0);

   c = *c0;    // ok

   c1 = c0;
   c0 = realloc(c0, 0);
   assert(!c0);

   c = *c1;    // bad, dangling

   return 0;
}
