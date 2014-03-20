
/* Check some aspects of the use of the 
   VALGRIND_ENABLE_ADDR_ERROR_REPORTING_IN_RANGE and
   VALGRIND_DISABLE_ADDR_ERROR_REPORTING_IN_RANGE macros. */

#include <stdio.h>
#include <stdlib.h>

#include "../memcheck.h"

int main ( void )
{
  volatile int* volatile mem
     = (volatile int* volatile)malloc(1000 * sizeof(int));
  free((void*)mem);

  // Check that we get an invalid access complaint
  fprintf(stderr, "\nDoing invalid access.  Expect complaint.\n\n");
  mem[123] = 0;

  // Now disable error reporting in the range
  fprintf(stderr, "\nDisabling address error reporting for the range.\n\n");
  VALGRIND_DISABLE_ADDR_ERROR_REPORTING_IN_RANGE(mem, 1000 * sizeof(int));

  // Check that we get an invalid access complaint
  fprintf(stderr, "\nDoing invalid another access.  Expect no complaint.\n\n");
  mem[456] = 0;
 
  // Re-enable reporting on the first byte of one word from the ignore range
  fprintf(stderr, "\nPartially reenabling address error reporting.\n\n");
  VALGRIND_ENABLE_ADDR_ERROR_REPORTING_IN_RANGE(&mem[789], 1);

  // Check that we get an invalid access complaint
  fprintf(stderr, "\nDoing a third access.  Expect complaint.\n\n");
  mem[789] = 0;

  // And now quit and expect to see a warning about two remaining ranges
  fprintf(stderr, "\nExiting.  Expect warnings of 2 remaining ranges.\n\n");

  return 0;
}
