/* Tests for some interesting cases in non-standard strlcpy(). */

#include <stdio.h>
#include <stdlib.h>
#include <strings.h>

int main(void)
{
   size_t copied;

   char *src = malloc(100);
   if (src == NULL) {
      fprintf(stderr, "Memory allocation failure.\n");
      return 1;
   }
   strcpy(src, "Hey, dude!");

   char *dst = malloc(10);
   if (dst == NULL) {
      fprintf(stderr, "Memory allocation failure.\n");
      return 1;
   }

   /* This is ok. */
   copied = strlcpy(dst, src, 10);
   if (copied != 10)
      fprintf(stderr, "Expected 10 but got %zu for test #1.\n", copied);

   /* Here dst is not large enough. */
   copied = strlcpy(dst, src, strlen(src) + 1);
   if (copied != 10)
      fprintf(stderr, "Expected 10 but got %zu for test #2.\n", copied);

   /* This is just a fancy way how to write strlen(src).
      Undocumented but heavily used. */
   copied = strlcpy(NULL, src, 0);
   if (copied != 10)
      fprintf(stderr, "Expected 10 but got %zu for test #3.\n", copied);

   /* Source and destination overlap. */
   strlcpy(src + 9, src, strlen(src) + 1);
   /* Return value is not checked because function behaviour
      is undefined in such case (and valgrind's differs). */

   return 0;
}

