#include <stdlib.h>
#include <sys/mman.h>
#include <stdio.h>
 
int main(void)
{
   void *p;

   int size = 2 * 1023 * 1024 * 1024;     // just under 2^31 (2GB)
 
   fprintf(stderr, "Attempting too-big malloc()...\n");
   p = malloc(size);          // way too big!
   if (p)
      fprintf(stderr, "huge malloc() succeeded??\n");

   fprintf(stderr, "Attempting too-big mmap()...\n");
   p = mmap( 0, size, PROT_READ|PROT_WRITE|PROT_EXEC,
             MAP_PRIVATE|MAP_ANON, -1, 0 );
   if (-1 != (int)p)
      fprintf(stderr, "huge mmap() succeeded??\n");

   return 0;
} 
