/* See also https://bugs.kde.org/show_bug.cgi?id=432381. */

#define _GNU_SOURCE

#include <poll.h>
#include <pthread.h>
#include <signal.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <sys/timerfd.h>
#include <ucontext.h>
#include <unistd.h>
#include <valgrind/valgrind.h>

typedef struct thread_local {
  ucontext_t uc[3];
  size_t nrsw;
  int tfd;
} thread_local_t;

static void f(void *data, int n)
{
  enum { NR_SWITCHES = 200000 };
  struct pollfd pfd;
  thread_local_t *tlocal = data;

  while (1) {
    memset(&pfd, 0, sizeof(pfd));
    pfd.fd = tlocal->tfd;
    pfd.events = POLLIN;

    if (poll(&pfd, 1, 0) == 1) {
      if (++tlocal->nrsw == NR_SWITCHES)
        return;
      swapcontext(&tlocal->uc[n], &tlocal->uc[3 - n]);
    }
  }
}

void __valgrind_register_current_stack(void)
{
  pthread_attr_t attr;
  size_t stacksize;
  void *stack;

  if (pthread_getattr_np(pthread_self(), &attr) != 0)
    abort();

  if (pthread_attr_getstack(&attr, &stack, &stacksize) != 0)
    abort();

  VALGRIND_STACK_REGISTER(stack, stack + stacksize);
}

#define STACKSIZE 8192

void *worker(void *data)
{
  thread_local_t *tlocal = data;
  struct itimerspec it;

  __valgrind_register_current_stack();

  tlocal->tfd = timerfd_create(CLOCK_REALTIME, 0);
  if (tlocal->tfd < 0)
    abort();

  it.it_interval.tv_sec = 0;
  it.it_interval.tv_nsec = 1000;

  it.it_value.tv_sec = time(NULL);
  it.it_value.tv_nsec = 1000;

  if (timerfd_settime(tlocal->tfd, TFD_TIMER_ABSTIME, &it, NULL) < 0)
    abort();

  if (getcontext(&(tlocal->uc[1])) < 0)
    abort();
  if (getcontext(&(tlocal->uc[2])) < 0)
    abort();

  tlocal->uc[1].uc_link = &tlocal->uc[0];
  tlocal->uc[1].uc_stack.ss_sp = malloc(STACKSIZE);
  tlocal->uc[1].uc_stack.ss_size = STACKSIZE;
  makecontext(&tlocal->uc[1], (void (*)(void))f, 2, tlocal, 1);
  VALGRIND_STACK_REGISTER(tlocal->uc[1].uc_stack.ss_sp,
                          tlocal->uc[1].uc_stack.ss_sp +
                          tlocal->uc[1].uc_stack.ss_size);

  tlocal->uc[2].uc_link = &tlocal->uc[0];
  tlocal->uc[2].uc_stack.ss_sp = malloc(STACKSIZE);
  tlocal->uc[2].uc_stack.ss_size = STACKSIZE;
  makecontext(&tlocal->uc[2], (void (*)(void))f, 2, tlocal, 2);
  VALGRIND_STACK_REGISTER(tlocal->uc[2].uc_stack.ss_sp,
                          tlocal->uc[2].uc_stack.ss_sp +
                          tlocal->uc[2].uc_stack.ss_size);

  swapcontext(&tlocal->uc[0], &tlocal->uc[1]);
  return NULL;
}

int main(int argc, char *argv[])
{
  enum { NR = 32 };
  thread_local_t tlocal[NR];
  pthread_t thread[NR];
  int i;

  memset(tlocal, 0, sizeof(tlocal));

  for (i = 0; i < NR; i++)
    pthread_create(&thread[i], NULL, worker, &tlocal[i]);

  // Wait until the threads have been created.
  sleep(1);
  for (i = 0; i < NR; i++)
    pthread_kill(thread[i], SIGINT);

  for (i = 0; i < NR; i++)
    pthread_join(thread[i], NULL);

  return 0;
}
