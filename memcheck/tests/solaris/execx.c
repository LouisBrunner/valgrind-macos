/* Tests variant of SYS_execve where the first argument is a file descriptor. */

#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/execx.h>
#include <sys/syscall.h>
#include <sys/wait.h>

static void test_EFAULT(void) {
   int ret = syscall(SYS_execve, -1, 0, 0, 0);
   int error = errno;
   if ((ret != -1) || (error != EFAULT))
      fprintf(stderr, "Expecting EFAULT\n");
}

static void test_EBADF(void) {
   int ret = syscall(SYS_execve, -1, 0, 0, EXEC_DESCRIPTOR);
   int error = errno;
   if ((ret != -1) || (error != EBADF))
      fprintf(stderr, "Expecting EBADF\n");
}

static int test_fexecve(char * const *envp) {
   int fd = open("/usr/bin/printf", O_EXEC);
   if (fd < 0) {
      perror("open");
      return 1;
   }

   pid_t pid = fork();
   if (pid == -1) {
      perror("fork");
      return 1;
   } else if (pid > 0) {
      /* parent */
   } else {
      char *argv[] = {"printf", "PASSED\n", NULL};

      if (fexecve(fd, argv, envp) < 0) {
         perror("fexecve");
         _exit(1);
      }

   }

   wait(NULL);
   return 0;
}

int main(int argc, const char *argv[], char * const *envp) {
   /* First exercise the syscall with some invalid input. */
   test_EFAULT();
   test_EBADF();

   return test_fexecve(envp);
}
