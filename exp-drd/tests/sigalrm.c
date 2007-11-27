#include <assert.h>
#include <errno.h>
#include <pthread.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include <asm/unistd.h>
#include "../drd_clientreq.h"


static int s_debug = 0;


static int getktid()
{
  return syscall(__NR_gettid);
}

static int getvgtid()
{
  int res;
  VALGRIND_DO_CLIENT_REQUEST(res, 0, VG_USERREQ__GET_THREAD_SELF, 0, 0, 0,0,0);
  return res;
}

static void SignalHandler(const int iSignal)
{
  if (s_debug)
  {
    char msg[256];
    snprintf(msg, sizeof(msg), "Signal %d was delivered to kernel thread ID %d"
           " / Valgrind thread ID %d\n",
             iSignal, getktid(), getvgtid());
    write(STDOUT_FILENO, msg, strlen(msg));
  }
}

void* thread_func(void* thread_arg)
{
  struct timespec tsRemain, tsDelay;

  if (s_debug)
  {
    printf("thread: kernel thread ID %d  / Valgrind thread ID %d\n",
	   getktid(), getvgtid());
  }

  tsDelay.tv_sec = 10;
  tsDelay.tv_nsec = 0;
  clock_nanosleep(CLOCK_MONOTONIC, 0, &tsDelay, &tsRemain);
  //assert(result < 0 && errno == EINTR);

  return 0;
}

int main(int argc, char** argv)
{
  int vgthreadid;
  pthread_t threadid;
  struct timespec tsDelay;

  // Primitive argument parsing.
  if (argc > 1)
    s_debug = 1;

  vgthreadid = getvgtid();

  if (s_debug)
  {
    printf("main: kernel thread ID %d / Valgrind thread ID %d\n",
	   getktid(), vgthreadid);
  }

  {
    struct sigaction sa;
    memset(&sa, 0, sizeof(sa));
    sa.sa_handler = &SignalHandler;
    sigemptyset(&sa.sa_mask);
    sigaction(SIGALRM, &sa, 0);
  }

  pthread_create(&threadid, 0, thread_func, 0);
  // Wait until the thread is inside clock_nanosleep().
  tsDelay.tv_sec = 0;
  tsDelay.tv_nsec = 20 * 1000 * 1000;
  clock_nanosleep(CLOCK_MONOTONIC, 0, &tsDelay, 0);
  // And send SIGALRM to the thread.
  pthread_kill(threadid, SIGALRM);
  pthread_join(threadid, 0);

  return 0;
}
