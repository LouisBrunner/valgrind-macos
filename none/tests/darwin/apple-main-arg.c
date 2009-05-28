#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <unistd.h>
#include <sys/syslimits.h>

// On Darwin there's this secret fourth argument, 'apple', which is a pointer
// to a string that contains the executable path, like argv[0], but unlike
// argv[0] it can't be changed using exec().

int main(int argc, char *argv[], char *envp[], char *apple[])
{
   char *pargv = calloc((PATH_MAX+1), sizeof(char)),
        *pappl = calloc((PATH_MAX+1), sizeof(char));
   int i;

   for (i = 0; envp[i]; i++)
      ;

   // envp[i]==NULL; envp[i+1]==apple[0]==executable_path
   assert(envp[i+1] == apple[0]);

   // Make sure realpath(argv[0]) == realpath(apple[0]).  (realpath resolves
   // symlinks.)
   realpath(argv[0], pargv);
   realpath(apple[0], pappl);
   assert(0 == strcmp(pargv, pappl));

   return 0;
}

