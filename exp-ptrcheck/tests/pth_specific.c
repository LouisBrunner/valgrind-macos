#include <pthread.h>
#include <assert.h>
#include <stdlib.h>

// This shows that putting a segment pointer into a thread-specific data
// area and then getting it out again doesn't lose info -- even though the
// key allocation/getting is done on the real CPU where the skin can't see,
// the get/set of the info is done using that key on the simd CPU where it
// can see, so everything works out fine.

int main(void)
{
   pthread_key_t key;
   char *x, *z;
   char  y __attribute__((unused));

   x = malloc(100);

   y = x[-1];     // error
   x[1] = 'z';

   assert( 0 == pthread_key_create ( &key, NULL ) );
   assert( 0 == pthread_setspecific(  key, x ) );
   z = (char*)pthread_getspecific( key );
   assert( 0 != z );

   y = z[-1];     // error

   // ensure the key went in and out correctly
   assert(z == x);
   assert(z[1] == 'z');

   return 0;
}
