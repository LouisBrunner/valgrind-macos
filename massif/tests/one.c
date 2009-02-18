#include <stdlib.h>

// A test for a single allocation.  There are two .post.exp* files, for each
// of VG_MIN_MALLOC_SZB==8 and VG_MIN_MALLOC_SZB==16.

int main(void)
{
   malloc(1);
   return 0;
}
