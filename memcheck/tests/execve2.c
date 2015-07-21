#include <assert.h>
#include <unistd.h>

int main ( int argc, char** argv, char** envp )
{
   char* null_filename = NULL;
   char *const argv_exe[] = {"true", NULL};

   execve(null_filename, NULL, NULL);
   // Solaris requires non-NULL argv param (this is not necessary on Linux)
   execve("../../tests/true", argv_exe, envp);
   assert(0);  // shouldn't get here
}
