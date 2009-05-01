// This tests handling of signals sent from outside the process in the
// following combinations:  sync and async signals, caught and uncaught
// signals, and while blocking or not blocking in a syscall.  This exercises
// various different paths in Valgrind's signal handling.
//
// It does this by installing signal handlers for one signal S, spawning
// another process P, sending S from P multiple times (all caught), then
// sending another signal from P (not caught).

#include <signal.h>
#include <unistd.h>
#include <sys/wait.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <time.h>

static const struct timespec bip = { 0, 1000000000 / 5 };   // 0.2 seconds.

static void handler(int sig)
{
}

/* Kill our child, but use a separate kill command.  This is so that
   it's running independently of Valgrind, and so is async with
   respect to thread scheduling. */
static void do_kill(int pid, int sig)
{
   int status;
   int killer;
   int ret;

   killer = vfork();
   if (killer == -1) {
      perror("killer/vfork");
      exit(1);
   }

   // In the child, exec 'kill' in order to send the signal.
   if (killer == 0) {
      char sigbuf[20];
      char pidbuf[20];
      sprintf(sigbuf, "-%d", sig);
      sprintf(pidbuf, "%d", pid);
      execl("/bin/kill", "kill", sigbuf, pidbuf, NULL);
      perror("exec failed");
      exit(1);
   }

   // In the parent, just wait for the child and then check it ran ok.
   do 
      ret = waitpid(killer, &status, 0);
   while (ret == -1 && errno == EINTR);

   if (ret != killer) {
      perror("kill/waitpid");
      exit(1);
   }

   if (!WIFEXITED(status) || WEXITSTATUS(status) != 0) {
      fprintf(stderr, "kill %d failed status=%s %d\n", killer, 
             WIFEXITED(status) ? "exit" : "signal", 
             WIFEXITED(status) ? WEXITSTATUS(status) : WTERMSIG(status));
      exit(1);
   }
}

static void test(int block, int caughtsig, int fatalsig)
{
   int pid;
   int status;
   int i;

   fprintf(stderr, "testing: blocking=%d caught=%d fatal=%d... ",
      block, caughtsig, fatalsig);

   pid = fork();
   if (pid == -1) {
      perror("fork");
      exit(1);
   }

   // In the child, install the signal handler, then wait for the signal to
   // arrive:
   // - if 'block' is set, wait on a system call;
   // - otherwise, wait in client code (by spinning).
   // The alarm() calls is so that if something breaks, we don't get stuck.
   if (pid == 0) {
      signal(caughtsig, handler);
      alarm(10);

      for (;;)
         if (block) {
            pause();
         }
   }

   // In the parent, send the signals.
   nanosleep(&bip, 0);           // Wait for child to get going.

   for (i = 0; i < 5; i++) {
      do_kill(pid, caughtsig);   // Should be caught.
      nanosleep(&bip, 0);
      do_kill(pid, caughtsig);   // Ditto.
      do_kill(pid, caughtsig);   // Ditto.
   }

   nanosleep(&bip, 0);

   do_kill(pid, fatalsig);       // Should kill it.
   
   // Check that the child behaved as expected when it received the signals.
   if (waitpid(pid, &status, 0) != pid) {
      fprintf(stderr, "FAILED: waitpid failed: %s\n", strerror(errno));

   } else if (!WIFSIGNALED(status) || WTERMSIG(status) != fatalsig) {
      fprintf(stderr, "FAILED: child exited with unexpected status %s %d\n",
             WIFEXITED(status) ? "exit" : "signal", 
             WIFEXITED(status) ? WEXITSTATUS(status) : WTERMSIG(status));

   } else {
      fprintf(stderr, "PASSED\n");
   }
}

int main()
{
   test(/*non-blocked*/0, /* sync*/SIGSEGV, /* sync*/SIGBUS);
   test(/*non-blocked*/0, /* sync*/SIGSEGV, /*async*/SIGHUP);
   test(/*non-blocked*/0, /*async*/SIGUSR1, /* sync*/SIGBUS);
   test(/*non-blocked*/0, /*async*/SIGUSR1, /*async*/SIGHUP);
   test(/*    blocked*/1, /* sync*/SIGSEGV, /* sync*/SIGBUS);
   test(/*    blocked*/1, /* sync*/SIGSEGV, /*async*/SIGHUP);
   test(/*    blocked*/1, /*async*/SIGUSR1, /* sync*/SIGBUS);
   test(/*    blocked*/1, /*async*/SIGUSR1, /*async*/SIGHUP);

   return 0;
}
