#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>

/* Simple demonstration of lockset tracking at byte granularity. */

char bytes[10];

void* child_fn ( void* arg )
{
   int i;
   for (i = 0; i < 5; i++)
      bytes[2*i + 0] ++; /* child accesses: 0 2 4 6 8 */
   return NULL;
}

int main ( void )
{
   int i;
   pthread_t child;

   if (pthread_create(&child, NULL, child_fn, NULL)) {
      perror("pthread_create");
      exit(1);
   }

   /* Unprotected relative to child, but harmless, since different
      bytes accessed */
   for (i = 0; i < 5; i++)
      bytes[2*i + 1] ++; /* accesses: 1 3 5 7 9 */

   /* Unprotected relative to child, but harmful; same bytes */
   for (i = 0; i < 3; i++)
      bytes[3*i + 1] ++; /* accesses: 1 4(race!) 7 */

   if (pthread_join(child, NULL)) {
      perror("pthread join");
      exit(1);
   }

   return 0;
}
