// Some basic allocations and accesses.

#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include "dhat/dhat.h"

int main(void)
{
   int64_t* m = malloc(1000);
   m[0] = 1;                     // write 8 bytes
   m[10] = m[1];                 // read and write 8 bytes

   char* c = calloc(1, 2000);
   for (int i = 0; i < 1000; i++) {
      c[i + 1000] = c[i];        // read and write 1000 bytes
   }

   char* r = realloc(m, 3000);   // read and write 1000 bytes (memcpy)
   for (int i = 0; i < 500; i++) {
      r[i + 2000] = 99;          // write 500 bytes
   }

   c = realloc(c, 1000);         // read and write 1000 bytes (memcpy)

   free(c);
                                 // totals: 3008 read, 3516 write

   // Should be ignored because we're not in ad hoc mode.
   DHAT_AD_HOC_EVENT(100);

   return 0;
}
