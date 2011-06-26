#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <unistd.h>
#include <sys/wait.h>
int main()
{
   int mem = 0, pid;
   pid = fork();
   if (pid == -1) {
      mem = 1;
      perror("fork");
      exit(1);
   }

   if (pid == 0) {
      int burn;
      for (burn = 0; burn < 100000; burn++) /* burncpu */__asm__ __volatile("":::"memory") ;
      if (mem == 0)
         exit(0);
      else
         exit(1);
   } else {
      int ret;
      int status;
      while((ret = waitpid(pid, &status, 0)) != pid) {
         if (errno != EINTR) {
            perror("waitpid");
            exit(1);
         }
      }
      mem = status;
   }
   if (mem == 0)
      printf("mem is zero\n");

   return 0;
}
