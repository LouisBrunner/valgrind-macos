#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <unistd.h>
#include <sys/wait.h>
void fork_chain(int level)
{
   int pid;

   printf ("forking level %d\n", level);
   fflush (stdout);
   pid = fork();
   if (pid == -1) {
      perror("fork");
      exit(1);
   }

   if (pid == 0) {
      if (level > 0) {
         fork_chain (level - 1);
      }
   } else {
      int ret;
      int status;
      while((ret = waitpid(pid, &status, 0)) != pid) {
         if (errno != EINTR) {
            perror("waitpid");
            exit(1);
         }
      }
   }
}
int main()
{
   fork_chain (15);
   return 0;
}
