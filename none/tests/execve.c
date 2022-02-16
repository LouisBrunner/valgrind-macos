#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

int main(int argc, char **argv)
{
   if (argc == 1)
   {
      // This tests the case where argv and envp are NULL, which is easy to
      // get wrong because it's an unusual case. It is also bad and only
      // "worked" by accident with the linux kernel.

      char *const argv_exe[] = {"true", NULL};
      char *const v_null[] = { NULL };
      char *const v_minus_one[] = { (char *const) -1, NULL };

#if defined(VGO_solaris)
      const char *exe = "/bin/true";
#elif defined(VGO_darwin)
      const char *exe = "/usr/bin/true";
#elif defined(VGO_freebsd)
      const char *exe = "/usr/bin/true";
#else
      const char *exe = "/bin/true";
#endif

      /* Try some bad argv and envp arguments, make sure the executable
	 doesn't actually exists, so execve doesn't accidentally succeeds.  */
      if (execve("/%/", NULL, NULL) >= 0)
	printf ("WHAT?");
      if (execve("/%/", (void *)-1, NULL) >= 0)
	printf ("WHAT?");
      if (execve("/%/", v_null, NULL) >= 0)
	printf ("WHAT?");
      if (execve("/%/", v_null, v_null) >= 0)
	printf ("WHAT?");
      if (execve("/%/", v_minus_one, NULL) >= 0)
	printf ("WHAT?");
      if (execve("/%/", v_minus_one, v_null) >= 0)
	printf ("WHAT?");
      if (execve("/%/", v_minus_one, v_minus_one) >= 0)
	printf ("WHAT?");

      /* Finally a correct execve.  */
      if (execve(exe, argv_exe, NULL) < 0)
      {
         perror("execve");
         exit(1);
      }
   }
   
   exit(0);
}
