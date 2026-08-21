#include "stdlib.h"

int main(void)
{
   char* a = aligned_alloc(256, 1024);
   char* b = aligned_alloc(256, 1024);
   free_sized(a, 1024);
   free_aligned_sized(b, 256, 1024);

   a = malloc(1000);
   free_sized(a, 1000);

   a = calloc(999, sizeof(*a));
   free_sized(a, 999);

   a = malloc(1001);
   a = realloc(a, 2001);
   free_sized(a, 2001);

   a = aligned_alloc(1, 256);
   free_aligned_sized(a, 1, 256);

   // some errors
   a = malloc(1000);
   // mismatch size, larger
   free_sized(a, 1024);
   a = malloc(1000);
   // mismatch size, smaller
   free_sized(a, 990);

   size_t s = 1024;
   size_t u;
   // make s uninitialised but keep value of 1024
   s += u;
   s -= u;
   a = aligned_alloc(256, 1024);
   // uninit size
   free_sized(a, s);

   a = malloc(1000);
   // align not power of 2
   free_aligned_sized(a, 1000, 1000);

   a = aligned_alloc(256, 4096);
   // align mismatch
   free_aligned_sized(a, 128, 4096);

   a = aligned_alloc(256, 2048);
   // size mismatch
   free_aligned_sized(a, 256, 1024);

   a = aligned_alloc(256, 1024);
   // uninit size
   free_aligned_sized(a, 256, s);

   a = aligned_alloc(1024, 1024);
   // uninit alignment
   free_aligned_sized(a, s, 1024);

   // NULL pointers
   free_sized(NULL, 32);
   free_aligned_sized(NULL, 32, 128);
}
