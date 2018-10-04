// Some basic allocations and accesses.

#include <stdint.h>
#include <stdlib.h>
#include <string.h>

int main(void)
{
   int64_t* m = malloc(1000);
   m[0] = 1;                     // write 8 bytes
   m[10] = m[1];                 // read and write 8 bytes

   char* c = calloc(1, 2000);
   for (int i = 0; i < 1000; i++) {
      c[i + 1000] = c[i];        // read and write 1000 bytes
   }

   char* r = realloc(m, 3000);
   for (int i = 0; i < 500; i++) {
      r[i + 2000] = 99;          // write 500 bytes
   }
                                 // totals: 1008 read, 1516 write
   free(c);

   return 0;
}
