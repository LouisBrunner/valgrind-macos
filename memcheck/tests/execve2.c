#include <assert.h>
#include <unistd.h>

int main ( int argc, char** argv, char** envp )
{
   execve(NULL,        NULL, NULL);
   execve("../../tests/true", NULL, envp);
   assert(0);  // shouldn't get here
}
