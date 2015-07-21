#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
int main()
{
   __attribute__((unused)) char *p = malloc (1);
   char *b1 = malloc (128);
   char *b2 = malloc (128);
   fprintf (stderr, "b1 %#" PRIxPTR " b2 %#" PRIxPTR "\n",
                    (uintptr_t)b1, (uintptr_t)b2);

   // Try to land in b2 from b1, causing no error
   // with the default redzone-size, but having
   // an error with a bigger redzone-size.
   // We need to choose a value which lands in b2
   // on 32 bits and 64 bits.
   b1[127 + 70] = 'a';
   return 0;
}
