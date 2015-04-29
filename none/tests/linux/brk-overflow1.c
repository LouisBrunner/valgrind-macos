#include <unistd.h>

volatile void *ptr;

/* The default size of the brk segment is 8 MB.
   Request more than that in a single request. */
int main()
{
   ptr = sbrk(9*1024*1024);

   return 0;
}
