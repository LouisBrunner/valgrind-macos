#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

int main(int argc, char **argv)
{
   if (argc == 1)
   {
      char *newargv[3] = { argv[0], "done", NULL };
     
      if (execve(argv[0], newargv, NULL) < 0)
      {
         perror("execve");
         exit(1);
      }
   }
   
   exit(0);
}
