/* Tests that Valgrind retains control over blocked signals.
   If synchronous signals (SIGSEGV) would be blocked, kernel would
   simply kill the process. When operating properly, Valgrind involves
   its synchronous signal handler and reports on the signal delivery.

   Valgrind and libc all retain their sigmasks and lie to us politely
   about what the actual sigmask is. One of reliable tests is to fork
   another process (because libc thinks it blocks all signals before fork
   and the forked process inherits the sigmask) and try to SIGSEGV it.
 */

#include <assert.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/wait.h>

int main(void)
{
   pid_t pid = fork();
   if (pid < 0) {
      perror("fork");
      exit(1);
   } else if (pid == 0) {
      /* Causes SIGSEGV. */
      char *s = NULL;
      s[0] = 1;
   } else {
      pid_t ret;
      int status;
      
      while ((ret = waitpid(pid, &status, 0)) != pid) {
         if (errno != EINTR) {
            perror("waitpid");
            exit(1);
         }
      }

      if (WIFSIGNALED(status)) {
         assert(WTERMSIG(status) != 0);

         if (WTERMSIG(status) == SIGSEGV) {
            printf("PASS\n");
         } else {
            fprintf(stderr, "Child process died with unexpected signal %d.\n",
                    WTERMSIG(status));
         }
      } else if (WIFEXITED(status)) {
         if (WEXITSTATUS(status) == 0) {
            fprintf(stderr, "Child process exited without expected SIGSEGV "
                    "signal.\n");
         } else {
            fprintf(stderr, "Child process exited with unexpected status %d.\n",
                    WEXITSTATUS(status));
         }
      } else {
         fprintf(stderr, "Unrecognized status of child proces %x?\n", status);
      }
   }

   return 0;
}

