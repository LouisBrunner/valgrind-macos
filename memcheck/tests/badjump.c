#include "tests/sys_mman.h"

int main ( void )
{
#if defined(__powerpc64__)
   /* on ppc64-linux, a function pointer points to a function
      descriptor, not to the function's entry point.  Hence to get
      uniform behaviour on all supported targets - a jump to an
      unmapped page - the following is needed. */
   unsigned long long int p[3];
   p[0] = (unsigned long long int)get_unmapped_page();
   p[1] = 0;
   p[2] = 0;
#else
   char* p = get_unmapped_page();
#endif
   return ((int(*)(void)) p) ();
}

