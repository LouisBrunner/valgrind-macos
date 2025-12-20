#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <unistd.h>
#include <sys/syslimits.h>
#include "../../../config.h"

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
   // PJF this changed with macOS 11, apple path now has a prefix
#if (DARWIN_VERS >= DARWIN_11_00)
   const char prefix[] = "executable_path=";
   const size_t prefix_len = strlen(prefix);
   assert(strncmp(apple[0], prefix, prefix_len) == 0);
   realpath(apple[0]+prefix_len, pappl);
   exit(0);
#else
   realpath(apple[0], pappl);
#endif
   realpath(argv[0], pargv);
   assert(0 == strcmp(pargv, pappl));

   return 0;
}

