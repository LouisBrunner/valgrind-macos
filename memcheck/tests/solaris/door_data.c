/* Simple door test. */

#include <door.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>

static char door_file[] = "/tmp/vgtest_door_data.XXXXXX";
static volatile int exit_now = 0;

static void child_handler(int sig)
{
   if (!exit_now) {
      fprintf(stderr, "Received premature SIGCHLD.\n");
      unlink(door_file);
      exit(1);
   }
}

/* SERVER CODE */
static void server_procedure(void *cookie, char *argp, size_t arg_size,
                             door_desc_t *dp, uint_t n_desc)
{
   char data[] = "Hello from server";

   if (!argp)
      goto out;

   if (arg_size > INT_MAX) {
      fprintf(stderr, "Value received from a client is too big.\n");
      goto out;
   }

   printf("SERVER: %.*s\n", (int)arg_size, argp);
   fflush(stdout);

out:
   /* Let server_main() know that we should exit. */
   *(int*)cookie = 1;

   door_return(data, strlen(data) + 1, NULL, 0);

   /* Function door_return() should never return. */
   perror("door_return");
   exit(1);
}

static int server_main(void)
{
   int res = 1;
   int did = -1;
   int attached = 0;

   /* Make sure nothing else is attached. */
   fdetach(door_file);

   if ((did = door_create(server_procedure, (void*)&exit_now, 0)) < 0) {
      perror("door_create");
      return 1;
   }

   /* Attach to file system. */
   if (fattach(did, door_file) < 0) {
      char str[100];
      snprintf(str, sizeof(str), "fattach %s", door_file);
      perror(str);
      goto out;
   }
   attached = 1;

   /* Poor man's termination. */
   while (!exit_now)
      sleep(1);

   res = 0;

out:
   if (attached && unlink(door_file)) {
      char str[100];
      snprintf(str, sizeof(str), "unlink %s", door_file);
      perror(str);
   }
   if (did >= 0 && door_revoke(did))
      perror("door_revoke");

   return res;
}

/* CLIENT CODE */
static int client_main(void)
{
   int did;
   char buf[128];
   int tries;
   door_arg_t params;
   struct door_info info;

   tries = 0;
   while (1) {
      /* Open the door file. */
      if ((did = open(door_file, O_RDWR)) >= 0)
         if (!door_info(did, &info))
            break;

      close(did);

      if (tries > 10) {
         char str[100];
         snprintf(str, sizeof(str), "door_info %s", door_file);
         perror(str);
         return 1;
      }

      tries++;
      sleep(1);
   }

   /* Set call parameters. */
   snprintf(buf, sizeof(buf), "Hello from client");
   params.data_ptr = buf;
   params.data_size = strlen(buf) + 1;
   params.desc_ptr = NULL;
   params.desc_num = 0;
   params.rbuf = buf;
   params.rsize = sizeof(buf);

   /* Make the call. */
   if (door_call(did, &params)) {
      perror("door_call");
      close(did);
      return 1;
   }

   close(did);

   /* Print a result of the call. */
   printf("CLIENT: %.*s\n", (int)params.rsize, params.rbuf);
   fflush(stdout);

   /* It's possible that the system allocated a new memory for rbuf.  Unmap it
      if it's the case */
   if (params.rbuf != buf)
      if (munmap(params.rbuf, params.rsize) != 0) {
         perror("munmap");
         return 1;
      }

   return 0;
}

/* MAIN CODE */
int main(void)
{
   struct sigaction sa;
   pid_t pid;
   int door_fd;

   /* Establish handler for client error exit. */
   sa.sa_handler = child_handler;
   sa.sa_flags = SA_NOCLDSTOP;
   if (sigemptyset(&sa.sa_mask)) {
      perror("sigemptyset");
      return 1;
   }
   if (sigaction(SIGCHLD, &sa, NULL)) {
      perror("sigaction");
      return 1;
   }

   door_fd = mkstemp(door_file);
   if (door_fd < 0) {
      perror("mkstemp");
      return 1;
   }
   close(door_fd);

   pid = fork();
   if (pid == -1) {
      perror("fork");
      return 1;
   }

   if (pid == 0) {
      return client_main();
   } else {
      int res = server_main();
      if (res == 0) {
         do {
            if (wait(NULL) == pid)
               break;
            if (errno != EINTR) {
               perror("wait");
               res = 1;
            }
         } while (errno == EINTR);
      }
      return res;
   }
}
