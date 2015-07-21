/* Functional tests for spawn() syscall invoked indirectly via posix_spawn()
   or system(). */

#include <assert.h>
#include <fcntl.h>
#include <spawn.h>
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include <sys/wait.h>


#define EXE_NAME "../../../tests/true"

static volatile int sigchld_handled = 0;
static void sigchld_handler(int sig, siginfo_t *sip, void *ucontext) {
   assert(sig == SIGCHLD);
   sigchld_handled = 1;
}

int main(int argc, char *const argv[], char *const envp[]) {
   int ret = system(EXE_NAME);
   if (ret != 0)
      perror("system");

   /* system() */
   ret = system(NULL);
   if (ret == 0)
      fprintf(stderr, "system() succeeded");

   /* posix_spawn(), no file actions, no attrs */
   char *const argv_exe[] = {"true", NULL};
   pid_t child;
   ret = posix_spawn(&child, EXE_NAME, NULL, NULL, argv_exe, envp);
   if (ret != 0)
      perror("posix_spawn");
   waitpid(child, NULL, 0);

   /* posix_spawn(), file actions, no attrs */
   posix_spawn_file_actions_t fa;
   ret = posix_spawn_file_actions_init(&fa);
   if (ret != 0)
      perror("posix_spawn_file_actions_init");
   ret = posix_spawn_file_actions_addopen(&fa, 10, "/dev/null", O_RDONLY, 0);
   if (ret != 0)
      perror("posix_spawn_file_actions_addopen");
   ret = posix_spawn(&child, EXE_NAME, &fa, NULL, argv_exe, envp);
   if (ret != 0)
      perror("posix_spawn");
   waitpid(child, NULL, 0);
   ret = posix_spawn_file_actions_destroy(&fa);
   if (ret != 0)
      perror("posix_spawn_file_actions_destroy");

   /* posix_spawn(), no file actions, attrs */
   posix_spawnattr_t spa;
   ret = posix_spawnattr_init(&spa);
   if (ret != 0)
      perror("posix_spawnattr_init");
   ret = posix_spawnattr_setflags(&spa, POSIX_SPAWN_RESETIDS);
   if (ret != 0)
      perror("posix_spawnattr_setflags");
   ret = posix_spawn(&child, EXE_NAME, NULL, &spa, argv_exe, envp);
   if (ret != 0)
      perror("posix_spawn");
   waitpid(child, NULL, 0);
   ret = posix_spawnattr_destroy(&spa);
   if (ret != 0)
      perror("posix_spawnattr_destroy");

   /* posix_spawn(), no file actions, no attrs, test SIGCHLD delivery */
   struct sigaction act;
   bzero(&act, sizeof(act));
   act.sa_sigaction = sigchld_handler;
   act.sa_flags = SA_SIGINFO;
   ret = sigaction(SIGCHLD, &act, NULL);
   if (ret != 0)
      perror("sigaction");
   sigchld_handled = 0;
   ret = posix_spawn(&child, EXE_NAME, NULL, NULL, argv_exe, envp);
   if (ret != 0)
      perror("posix_spawn");
   waitpid(child, NULL, 0);
   if (sigchld_handled == 1) {
      printf("PASS\n");
   } else {
      printf("FAIL\n");
   }

   /* posix_spawn(), no file actions, attrs, test *no* SIGCHLD delivery */
   ret = posix_spawnattr_init(&spa);
   if (ret != 0)
      perror("posix_spawnattr_init");
   ret = posix_spawnattr_setflags(&spa, POSIX_SPAWN_NOSIGCHLD_NP);
   if (ret != 0)
      perror("posix_spawnattr_setflags");
   sigchld_handled = 0;
   ret = posix_spawn(&child, EXE_NAME, NULL, &spa, argv_exe, envp);
   if (ret != 0)
      perror("posix_spawn");
   waitpid(child, NULL, 0);
   if (sigchld_handled == 0) {
      printf("PASS\n");
   } else {
      printf("FAIL\n");
   }
   ret = posix_spawnattr_destroy(&spa);
   if (ret != 0)
      perror("posix_spawnattr_destroy");

   return 0;
}
