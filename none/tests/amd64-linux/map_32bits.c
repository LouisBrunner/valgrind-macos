#include <stdio.h>
#include "tests/sys_mman.h"
#include <stdlib.h>
#include <unistd.h>

int main()
{

   void *first = NULL;
   void *last;
   void *res;

   while (1) {
      res = mmap (NULL, 64 * 1024,
                  PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANONYMOUS|MAP_32BIT,
                  -1, 0);
      if (first == NULL) {
         first = res;
         if (first == (void *)-1) {
            perror ("first mmap");
            exit (1);
         }
         fprintf(stderr, "first mmap : %p\n", first);
      }
      if (res == (void *)-1) {
         fprintf(stderr, "last mmap ok: %p\n", last);
         break;
      }
      last = res;
   }

   /* And now, retry without MAP_32BIT */
   res = mmap (NULL, 64 * 1024,
               PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANONYMOUS,
               -1, 0);
   if (res == (void *)-1) {
      perror ("retry mmap");
      exit (1);
   }
   fprintf(stderr, "retry mmap ok: %p\n", res);

   return 0;
}
