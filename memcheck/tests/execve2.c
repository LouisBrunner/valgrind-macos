#include <assert.h>
#include <unistd.h>

int main ( int argc, char** argv, char** envp )
{
   char* null_filename = NULL;

   execve(null_filename,      NULL, NULL);
   execve("../../tests/true", NULL, envp);
   assert(0);  // shouldn't get here
}
