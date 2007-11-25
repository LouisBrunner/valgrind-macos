#include <cassert>
#include <cerrno>
#include <cstdio>
#include <cstdlib>
#include <ctime>
#include <iostream>
#include <pthread.h>
#include <signal.h>
#include <unistd.h>
#include "../drd_clientreq.h"
#include <asm/unistd.h>


#define VALGRIND_START_NEW_SEGMENT    \
{                                                                       \
  int res;                                                              \
  VALGRIND_DO_CLIENT_REQUEST(res, 0, VG_USERREQ__DRD_START_NEW_SEGMENT, \
                             pthread_self(), 0, 0,0,0);                 \
}


static bool s_debug = false;


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

void* thread_func(void*)
{
  if (s_debug)
  {
    std::cout << "thread: kernel thread ID " << getktid()
              << " / Valgrind thread ID " << getvgtid() << "\n";
  }

  const timespec tsDelay = { 10, 0 };
  timespec tsRemain;
  clock_nanosleep(CLOCK_MONOTONIC, 0, &tsDelay, &tsRemain);
  //assert(result < 0 && errno == EINTR);

  return 0;
}

int main(int argc, char** )
{
  // Primitive argument parsing.
  if (argc > 1)
    s_debug = true;

  const int vgthreadid = getvgtid();

  if (s_debug)
  {
    std::cout << "main: kernel thread ID " << getktid()
              << " / Valgrind thread ID " << vgthreadid << std::endl;
  }

  {
    struct sigaction sa;
    memset(&sa, 0, sizeof(sa));
    sa.sa_handler = &SignalHandler;
    sigemptyset(&sa.sa_mask);
    sigaction(SIGALRM, &sa, 0);
  }

  pthread_t threadid;
  pthread_create(&threadid, 0, thread_func, 0);
  // Wait until the thread is inside clock_nanosleep().
  const timespec tsDelay = { 0, 20 * 1000 * 1000 };
  clock_nanosleep(CLOCK_MONOTONIC, 0, &tsDelay, 0);
  // And send SIGALRM to the thread.
  pthread_kill(threadid, SIGALRM);
  pthread_join(threadid, 0);

  return 0;
}
