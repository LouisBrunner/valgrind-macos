#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

int main(int argc, char **argv)
{
   if (argc == 1)
   {
      if (execve(argv[0], NULL, NULL) < 0)
      {
         perror("execve");
         exit(1);
      }
   }
   
   exit(0);
}
