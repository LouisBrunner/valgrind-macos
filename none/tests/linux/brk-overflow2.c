#include <unistd.h>

volatile void *ptr;

/* The default size of the brk segment is 8 MB.
   Request more than that in a sequence of requests */
int main()
{
   int i;
   for (i=0; i < 10; ++i) {
      ptr = sbrk(1024*1024);
   }
   return 0;
}
