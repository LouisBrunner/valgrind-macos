#include <sys/mman.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>

/* The quick sanity check of Memcheck (and other tools with shadow memory)
   relies on the first 64KB of memory never being used.  So our mmap()
   refuses to touch this area.  This program tests for that. */

int main(void)
{
   /* mmap(0x0, ... FIXED) should fail */
   int* m = mmap(0x0, 1000000, PROT_READ|PROT_WRITE, 
                 MAP_PRIVATE|MAP_ANON|MAP_FIXED, -1, 0);
   if (m != (int*)-1)
      printf("succeeded?!\n");

   /* mmap(0x1000, ... FIXED) should fail */
        m = mmap((void*)0x1000, 1000000, PROT_READ|PROT_WRITE, 
                 MAP_PRIVATE|MAP_ANON|MAP_FIXED, -1, 0);
   if (m != (int*)-1)
      printf("succeeded?!\n");

   /* mmap(0xa000, ... FIXED) should fail */
        m = mmap((void*)0xa000, 1000000, PROT_READ|PROT_WRITE, 
                 MAP_PRIVATE|MAP_ANON|MAP_FIXED, -1, 0);
   if (m != (int*)-1)
      printf("succeeded?!\n");

   /* mmap(0x10000, ... FIXED) should fail */
        m = mmap((void*)0x10000, 1000000, PROT_READ|PROT_WRITE, 
                 MAP_PRIVATE|MAP_ANON|MAP_FIXED, -1, 0);
   if (m == (int*)-1)
      printf("failed?!\n");

   return 0;
}
