#include <stdlib.h>
/* To be run with --freelist-vol=1000000 --freelist-big-blocks=50000 */
static void jumped(void)
{
   ;
}
int main(int argc, char *argv[])
{
   char *semi_big = NULL;
   char *big = NULL;
   char *small = NULL;
   char *other_small = NULL;
   int i;
   int j;

   /* Verify that access via a dangling pointer to a big block bigger than
      the free list is found by memcheck (still on the free list). */
   semi_big = malloc (900000);
   big = malloc (1000001);
   free(semi_big);
   free(big);
   if (big[1000] > 0x0) jumped();
   if (semi_big[1000] > 0x0) jumped();

   /* Then verify that dangling pointers for small blocks is not hampered
      by doing big alloc/free. */
   small = malloc (10000);
   free(small);

   /* We should still have a nice error msg for the semi_big
      but not for the big block, which has been removed from the free list
      with the malloc of small above. */
   if (big[2000] > 0x0) jumped();
   if (semi_big[2000] > 0x0) jumped();

   big = NULL;

   {
      big = malloc (1000001);
      free(big);
      if (small[10] > 0x0) jumped();
      
      /* Do not common up the below in a loop. We
         want a different error/stack trace for each of
         these. */
      if (big[10] > 0x0) jumped();
   }
   
   
   for (i = 0; i < 100; i++) {
      other_small = malloc(10000);
      for (j = 0; j < 10000; j++)
         other_small[j] = 0x1;
   }
   if (small[10] > 0x0) jumped();
   return 0;
}
