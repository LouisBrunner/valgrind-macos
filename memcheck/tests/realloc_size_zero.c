#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

int main(void)
{

   char* p = malloc(1024);
   p[0] = '\0';
   errno = 0;
   p = realloc(p, 0);
   if (p) {
      printf("p not NULL after realloc 0\n");
   } else {
      printf("p is NULL after realloc 0\n");
   }
   if (errno) {
      perror("realloc(something, 0):");
   }
   if (p) {
      free(p);
   }

   errno = 0;
   void *ptr = NULL;
   volatile size_t size = 0U;
   char *p2 = realloc(ptr, size);
   if (p2) {
      printf("p2 not NULL after realloc 0\n");
   } else {
      printf("p2 is NULL after realloc 0\n");
   }
   if (errno) {
      perror("realloc(NULL, 0):");
   }
   if (p2) {
      free(p2);
   }
}
