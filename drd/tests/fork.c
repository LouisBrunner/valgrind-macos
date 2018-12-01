#include <stdio.h>
#include <pthread.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

static pthread_t tid[2];

static void *startproc(void *arg)
{
  pid_t pid;
  char *argv[] = { "/bin/ls", "/bin/ls", NULL };

  if ((pid = fork()) == -1) {
    perror("fork error");
  } else if (pid == 0) {
    dup2(2, 1);    // redirect stdout to stderr
    execv(argv[0], argv); // child
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

  for (i = 0; i < 2; i++) {
    err = pthread_create(&tid[i], NULL, &startproc, NULL);
    if (err != 0)
      perror("pthread_create()");
    if (serialize_fork)
      pthread_join(tid[i], NULL);
  }
  if (!serialize_fork) {
    for (i = 0; i < 2; i++)
      if (tid[i])
        pthread_join(tid[i], NULL);
  }
  return 0;
}
