#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <unistd.h>
#include <sys/syslimits.h>
#include "../../../config.h"

// used for debugging apple pointer issues, see
// https://bugs.kde.org/show_bug.cgi?id=517304
//#define DEBUG_ENV

// On Darwin there's this secret fourth argument, 'apple'.
// That's kind of like a cut down obfuscated version of auxv.
// For the moment we only support the first entry, executable_path=

int main(int argc, char *argv[], char *envp[], char *apple[])
{
   char *pargv = calloc((PATH_MAX+1), sizeof(char)),
        *pappl = calloc((PATH_MAX+1), sizeof(char));
   int i;

   for (i = 0; envp[i]; i++) {
#if defined(DEBUG_ENV)
      fprintf(stderr, "apple-main-arg: i %d &envp[i] %p envp[i] %s\n", i, &envp[i], envp[i]);
#endif
   }

#if defined(DEBUG_ENV)
   fprintf(stderr, "2 slots after envp\n");
   fprintf(stderr, "apple-main-arg: i %d &envp[i] %p envp[i] %s\n", i, &envp[i], envp[i]);
   fprintf(stderr, "apple-main-arg: i %d &envp[i] %p envp[i] %s\n", i+1, &envp[i+1], envp[i+1]);
   fprintf(stderr, "apple-main-arg: i %d &envp[i] %p envp[i] %s\n", i+2, &envp[i+2], envp[i+2]);
   fprintf(stderr, "apple %p\n", apple);
   int j = 0;
   while (apple[j]) {
      fprintf(stderr, "apple-main-arg: j %d &apple[j] %p apple[j] %s\n", j, &apple[j], apple[j]);
      ++j;
   }
   if (j == 0) {
      fprintf(stderr, "apple-main-arg: j %d &apple[j] %p apple[j] %s\n", j, &apple[j], apple[j]);
      fprintf(stderr, "apple-main-arg: 1 slot after apple\n");
      fprintf(stderr, "apple-main-arg: j %d &apple[j] %p apple[j] %s\n", j+1, &apple[j+1], apple[j+1]);
   } else {
      fprintf(stderr, "apple-main-arg: 1 slot after apple\n");
      fprintf(stderr, "apple-main-arg: j %d &apple[j] %p apple[j] %s\n", j, &apple[j], apple[j]);
   }
#endif

   // envp[i]==NULL; envp[i+1]==apple[0]==executable_path
   assert(envp[i+1] == apple[0]);

   // Make sure realpath(argv[0]) == realpath(apple[0]).  (realpath resolves
   // symlinks.)
   // PJF this changed with macOS 10.14, apple path now has a prefix
#if (DARWIN_VERS >= DARWIN_10_14)
   const char prefix[] = "executable_path=";
   const size_t prefix_len = strlen(prefix);
   assert(strncmp(apple[0], prefix, prefix_len) == 0);
   realpath(apple[0]+prefix_len, pappl);
#else
   realpath(apple[0], pappl);
#endif
   realpath(argv[0], pargv);
   assert(0 == strcmp(pargv, pappl));

   free(pargv);
   free(pappl);

   return 0;
}
