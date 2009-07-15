
/* A simple test to demonstrate heap, stack, and global overrun
   detection. */

#include <stdio.h>
#include <stdlib.h>

short ga[100];

__attribute__((noinline))
int addup_wrongly ( short* arr )
{
   int sum = 0, i;
   for (i = 0; i <= 100; i++)
      sum += (int)arr[i];
   return sum;
}

__attribute__((noinline))
int do_other_stuff ( void )
{
   short la[100];
   return 123 + addup_wrongly(la);
}

__attribute__((noinline))
int do_stupid_malloc_stuff ( void )
{
   int sum = 0;
   unsigned char* duh = malloc(100 * sizeof(char));
   sum += duh[-1];
   free(duh);
   sum += duh[50];
   return sum;
}

int main ( void )
{
   long s = addup_wrongly(ga);
   s += do_other_stuff();
   s += do_stupid_malloc_stuff();
   if (s == 123456789) {
      fprintf(stdout, "well, i never!\n");
   } else {
      fprintf(stdout, "boringly as expected\n");
   }
   return 0;
}
