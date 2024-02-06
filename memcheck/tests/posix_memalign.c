
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
#include "../../config.h"

int main ( void )
{
#  if !defined(VGO_darwin) || (DARWIN_VERS >= DARWIN_10_6)
   // Nb: assuming VG_MIN_MALLOC_SZB is 8 or more...
   int* p;
   int  res;
   assert(sizeof(long int) == sizeof(void*));

#  define PM(a,b,c) posix_memalign((void**)a, b, c)

   // test for size 0
   res = PM(&p, 64, 0);
#if defined(VGO_solaris)
   assert(NULL == p);
#else
   assert(0 == res && p && 0 == (long)p % 64);
   free(p);
#endif

   res = PM(&p, -1,100);
   assert(EINVAL == res);
   res = PM(&p, 0, 100);
   assert(EINVAL == res);
   res = PM(&p, 1, 100);
   assert(EINVAL == res);
   res = PM(&p, 2, 100);
   assert(EINVAL == res);
   res = PM(&p, 3, 100);
   assert(EINVAL == res);
   res = PM(&p, sizeof(void*), 100);
   assert(0 == res && p && 0 == (long)p % sizeof(void*));
   free(p);
   res = PM(&p, 31, 100);
   assert(EINVAL == res);
   res = PM(&p, 32, 100);
   assert(0 == res && p && 0 == (long)p % 32);
   free(p);
   res = PM(&p, 33, 100);
   assert(EINVAL == res);
   res = PM(&p, 4095, 100);
   assert(EINVAL == res);
   res = PM(&p, 4096, 100);
   assert(0 == res && p && 0 == (long)p % 4096);
   free(p);
   res = PM(&p, 4097, 100);
   assert(EINVAL == res);
   res = PM(&p, 4 * 1024 * 1024, 100);
   assert(0 == res && p && 0 == (long)p % (4 * 1024 * 1024));
   free(p);
   res = PM(&p, 16 * 1024 * 1024, 100);
   assert(0 == res && p && 0 == (long)p % (16 * 1024 * 1024));
   free(p);
#endif
}
