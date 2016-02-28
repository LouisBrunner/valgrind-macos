
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <assert.h>

int main ( void )
{
  void* a[10];
  int i;
  unsigned long pszB = sysconf(_SC_PAGE_SIZE);
  assert(sizeof(long) == sizeof(void*));
  assert(pszB == 4096 || pszB == 8192 || pszB == 16384 || pszB == 32768 || pszB == 65536); /* All on one line to match linenumbers in the .exp file. */

  for (i = 0; i < 10; i++) {
    a[i] = valloc(11111 * (i+1));
    /* check valloc really is returning page-aligned memory */
    assert( (((unsigned long)(a[i])) % pszB) == 0 );
    //    printf("I acquire %p\n", a[i]);
  }
  for (i = 0; i < 10; i++) {
    //    printf("I release %p\n", a[i]);
    free(a[i]);
  }
  free(a[9]);
  return 0;
}
