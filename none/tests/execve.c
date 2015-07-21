#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

int main(int argc, char **argv)
{
   if (argc == 1)
   {
      // This tests the case where argv and envp are NULL, which is easy to
      // get wrong because it's an unusual case.

#if defined(VGO_solaris)
      // Solaris requires non-NULL argv parameter
      char *const argv_exe[] = {"true", NULL};
      if (execve("/bin/true", argv_exe, NULL) < 0)
#elif defined(VGO_darwin)
      if (execve("/usr/bin/true", NULL, NULL) < 0)          
#else
      if (execve("/bin/true", NULL, NULL) < 0)
#endif
      {
         perror("execve");
         exit(1);
      }
   }
   
   exit(0);
}
