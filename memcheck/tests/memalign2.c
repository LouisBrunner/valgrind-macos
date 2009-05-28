
// These #defines attempt to ensure that posix_memalign() is declared, and
// so no spurious warning is given about using it.

// Advertise compliance of the code to the XSI (a POSIX superset that
// defines what a system must be like to be called "UNIX")
#undef _XOPEN_SOURCE
#define _XOPEN_SOURCE 600 
   
// Advertise compliance to POSIX
#undef _POSIX_C_SOURCE
#define _POSIX_C_SOURCE 200112L 

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include "tests/malloc.h"
#include <errno.h>

int main ( void )
{
#  if defined(VGO_aix5)
   // AIX 5.2 has neither memalign() nor posix_memalign();  do nothing.

#  elif defined(VGO_darwin)
   // Likewise for Mac OS X.

#  else
   // Nb: assuming VG_MIN_MALLOC_SZB is 8 or more...
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

#  define PM(a,b,c) posix_memalign((void**)a, b, c)

   res = PM(&p, -1,100);      assert(EINVAL == res);
   res = PM(&p, 0, 100);      assert(0 == res && 0 == (long)p % 8);
   res = PM(&p, 1, 100);      assert(EINVAL == res);
   res = PM(&p, 2, 100);      assert(EINVAL == res);
   res = PM(&p, 3, 100);      assert(EINVAL == res);
   res = PM(&p, sizeof(void*), 100);
                              assert(0 == res && 0 == (long)p % sizeof(void*));

   res = PM(&p, 31, 100);     assert(EINVAL == res);
   res = PM(&p, 32, 100);     assert(0 == res &&
                                                 0 == (long)p % 32);
   res = PM(&p, 33, 100);     assert(EINVAL == res);

   res = PM(&p, 4095, 100);   assert(EINVAL == res);
   res = PM(&p, 4096, 100);   assert(0 == res &&
                                                 0 == (long)p % 4096); 
   res = PM(&p, 4097, 100);   assert(EINVAL == res);

#  endif
   
   return 0;
}
