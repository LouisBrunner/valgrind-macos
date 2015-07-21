/* Test cancellation of a door_return call. */

#include <assert.h>
#include <door.h>
#include <errno.h>
#include <pthread.h>
#include <signal.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/lwp.h>
#include <unistd.h>

static volatile lwpid_t server_lwpid = 0;

static void server_procedure(void *cookie, char *argp, size_t arg_size,
                             door_desc_t *dp, uint_t n_desc)
{
   assert(0);
}

static void *my_server_thread(void *arg)
{
   server_lwpid = _lwp_self();
   door_return(NULL, 0, NULL, 0);
   return NULL;
}

static void create_door_thread(door_info_t *info)
{
   static int called = 0;
   pthread_t thread;
   int res;

   /* Allow to create only one server door thread. */
   assert(!called);
   called = 1;

   res = pthread_create(&thread, NULL, my_server_thread, NULL);
   if (res) {
      errno = res;
      perror("pthread_create");
      exit(1);
   }
}

static void signal_handler(int signo, siginfo_t *info, void *uc)
{
   const char str[] = "Signal caught.\n";
   size_t len = sizeof(str) - 1;
   ssize_t res;

   res = write(STDOUT_FILENO, str, len);
   assert(res == len);
}

int main(void)
{
   int res = 1;
   int did = -1;
   struct sigaction sa;

   sa.sa_sigaction = signal_handler;
   sa.sa_flags = SA_RESTART;
   if (sigfillset(&sa.sa_mask)) {
      perror("sigfillset");
      return 1;
   }
   if (sigaction(SIGINT, &sa, NULL)) {
      perror("sigaction");
      return 1;
   }

   door_server_create(create_door_thread);

   if ((did = door_create(server_procedure, NULL, 0)) < 0) {
      perror("door_create");
      return 1;
   }

   /* Let the server thread to run. */
   sleep(2);

   /* Send a signal to the server thread that should be already created and
      blocked in door_return. */
   if (_lwp_kill(server_lwpid, SIGINT)) {
      perror("_lwp_kill");
      goto out;
   }

   /* Let the other thread to run. */
   sleep(2);

   res = 0;

out:
   if (did >= 0 && door_revoke(did))
      perror("door_revoke");

   return res;
}

