#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <malloc.h>
#include <errno.h>

int main ( void )
{
   // Nb: assuming VG_MIN_MALLOC_SZB is 8!
   // Should work with both 32-bit and 64-bit pointers, though.

   int* p;
   int  res;
   assert(sizeof(long int) == sizeof(void*));
  
   p = memalign(0, 100);      assert(0 == (long)p % 8);
   p = memalign(1, 100);      assert(0 == (long)p % 8);
   p = memalign(2, 100);      assert(0 == (long)p % 8);
   p = memalign(3, 100);      assert(0 == (long)p % 8);
   p = memalign(4, 100);      assert(0 == (long)p % 8);
   p = memalign(5, 100);      assert(0 == (long)p % 8);

   p = memalign(7, 100);      assert(0 == (long)p % 8);
   p = memalign(8, 100);      assert(0 == (long)p % 8);
   p = memalign(9, 100);      assert(0 == (long)p % 16);

   p = memalign(31, 100);     assert(0 == (long)p % 32);
   p = memalign(32, 100);     assert(0 == (long)p % 32);
   p = memalign(33, 100);     assert(0 == (long)p % 64);

   p = memalign(4095, 100);   assert(0 == (long)p % 4096);
   p = memalign(4096, 100);   assert(0 == (long)p % 4096);
   p = memalign(4097, 100);   assert(0 == (long)p % 8192);

   res = posix_memalign(&p, -1,100);      assert(EINVAL == res);
   res = posix_memalign(&p, 0, 100);      assert(0 == res && 0 == (long)p % 8);
   res = posix_memalign(&p, 1, 100);      assert(EINVAL == res);
   res = posix_memalign(&p, 2, 100);      assert(EINVAL == res);
   res = posix_memalign(&p, 3, 100);      assert(EINVAL == res);
   res = posix_memalign(&p, sizeof(void*), 100);
                                          assert(0 == res && 
                                                 0 == (long)p % sizeof(void*));

   res = posix_memalign(&p, 31, 100);     assert(EINVAL == res);
   res = posix_memalign(&p, 32, 100);     assert(0 == res &&
                                                 0 == (long)p % 32);
   res = posix_memalign(&p, 33, 100);     assert(EINVAL == res);

   res = posix_memalign(&p, 4095, 100);   assert(EINVAL == res);
   res = posix_memalign(&p, 4096, 100);   assert(0 == res &&
                                                 0 == (long)p % 4096); 
   res = posix_memalign(&p, 4097, 100);   assert(EINVAL == res);
   
   return 0;
}
