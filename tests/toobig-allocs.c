#include <stdlib.h>
#include <sys/mman.h>
#include <stdio.h>
 
int main(void)
{
   void *p;

   // This is the biggest word-sized signed number.  We use a signed number,
   // even though malloc takes an unsigned SizeT, because the "silly malloc
   // arg" checking done by memcheck treats the arg like a signed int in
   // order to detect the passing of a silly size arg like -1.
   unsigned long size = (~(0UL)) >> 1;
 
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
