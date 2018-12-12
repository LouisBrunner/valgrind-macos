#include <stdio.h>
#include <pthread.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/wait.h>

#define NUM_THREADS 2

static pthread_t tid[NUM_THREADS];
static pid_t pids[NUM_THREADS];

static void *startproc(void *arg)
{
  pid_t pid;
  char *argv[] = { "/bin/ls", "/bin/ls", NULL };

  if ((pid = fork()) == -1) {
    perror("fork error");
  } else if (pid == 0) {
    dup2(2, 1);    // redirect stdout to stderr
    execv(argv[0], argv); // child
  } else {
    *((pid_t*)arg) = pid;
  }

  return NULL;
}

int main(int argc, char **argv)
{
  // No arguments means serialize the fork() calls. One argument means perform
  // both fork() calls concurrently.
  int serialize_fork = argc == 1;
  int i = 0;
  int err;

  for (i = 0; i < NUM_THREADS; i++) {
    err = pthread_create(&tid[i], NULL, &startproc, &pids[i]);
    if (err != 0)
      perror("pthread_create()");
    if (serialize_fork)
      pthread_join(tid[i], NULL);
  }
  if (!serialize_fork) {
    for (i = 0; i < NUM_THREADS; i++)
      if (tid[i])
        pthread_join(tid[i], NULL);
  }
  for (i = 0; i < NUM_THREADS; i++)
    waitpid(pids[i], &err, 0);

  return 0;
}
