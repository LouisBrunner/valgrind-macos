/* This exercises the code that was causing this bug:
  
     valgrind: vg_cachesim.c:389 (get_BBCC): Assertion `((Bool)0) == remove' 
     failed.

   in Cachegrind 1.0.0 and 1.0.1, that was caused by unloading symbols before
   invalidating translations.
*/

#include <stdio.h>
#include <stdlib.h>
#include <dlfcn.h>

int main(int argc, char **argv) {
   void *handle;
   void (*myprint)(void);
   char *error;

   handle = dlopen ("./myprint.so", RTLD_LAZY);
   if (!handle) {
       fputs (dlerror(), stderr);
       exit(1);
   }

   myprint = dlsym(handle, "myprint");
   if ((error = dlerror()) != NULL)  {
       fprintf (stderr, "%s\n", error);
       exit(1);
   }

   (*myprint)();

   /* Assertion failure was happening here */
   dlclose(handle);

   return 0;
}

