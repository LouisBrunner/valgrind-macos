#include <stdlib.h>
//#include <malloc.h>
#include "../memcheck.h"
int main (int argc, char*argv[])
{
   size_t def_size = 1<<20;
   char *p;
   char *new_p;

   if (argc > 10000) def_size = def_size * 2;

   {
      size_t size = def_size;
      (void) VALGRIND_MAKE_MEM_UNDEFINED(&size, 1);
      p = malloc(size);
   }

   (void) VALGRIND_MAKE_MEM_UNDEFINED(&p, 1);
   new_p = realloc(p, def_size);

   (void) VALGRIND_MAKE_MEM_UNDEFINED(&new_p, 1);
   new_p = realloc(new_p, def_size);

   (void) VALGRIND_MAKE_MEM_UNDEFINED(&new_p, 1);
   free (new_p);

   {
      size_t nmemb = 1;
      (void) VALGRIND_MAKE_MEM_UNDEFINED(&nmemb, 1);
      new_p = calloc(nmemb, def_size);
      free (new_p);
   }
#if 0
   {
      size_t alignment = 1;
      (void) VALGRIND_MAKE_MEM_UNDEFINED(&alignment, 1);
      new_p = memalign(alignment, def_size);
      free(new_p);
   }
   
   {
      size_t nmemb = 16;
      size_t size = def_size;
      (void) VALGRIND_MAKE_MEM_UNDEFINED(&size, 1);
      new_p = memalign(nmemb, size);
      free(new_p);
   }

   {
      size_t size = def_size;
      (void) VALGRIND_MAKE_MEM_UNDEFINED(&size, 1);
      new_p = valloc(size);
      free (new_p);
   }
#endif
   return 0;
}
