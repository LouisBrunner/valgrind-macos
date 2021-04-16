/* See also https://bugs.kde.org/show_bug.cgi?id=432381. */

#define _GNU_SOURCE

#include "../../config.h"
#if defined(VGO_darwin)
#define _XOPEN_SOURCE
#endif

#include <assert.h>
#include <limits.h>
#include <pthread.h>
#include <signal.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <ucontext.h>
#include <unistd.h>
#include "valgrind.h"

#define STACKSIZE (PTHREAD_STACK_MIN + 4096)

typedef struct thread_local {
  ucontext_t uc[3];
  size_t nrsw;
} thread_local_t;

static void sig_alrm_handler(int signo) {
    _exit(1);
}

static void f(void *data, int n)
{
  enum { NR_SWITCHES = 200000 };
  thread_local_t *tlocal = data;

  while (1) {
    struct timespec delay = { .tv_nsec = 1000 };
    nanosleep(&delay, NULL);
    if (++tlocal->nrsw == NR_SWITCHES)
      return;
    swapcontext(&tlocal->uc[n], &tlocal->uc[3 - n]);
  }
}

void *worker(void *data)
{
  thread_local_t *tlocal = data;

  if (getcontext(&(tlocal->uc[1])) < 0)
    abort();
  if (getcontext(&(tlocal->uc[2])) < 0)
    abort();

  tlocal->uc[1].uc_link = &tlocal->uc[0];
  tlocal->uc[1].uc_stack.ss_sp = malloc(STACKSIZE);
  tlocal->uc[1].uc_stack.ss_size = STACKSIZE;
  makecontext(&tlocal->uc[1], (void (*)(void))f, 2, tlocal, 1);
  (void)VALGRIND_STACK_REGISTER(tlocal->uc[1].uc_stack.ss_sp,
                                tlocal->uc[1].uc_stack.ss_sp +
                                tlocal->uc[1].uc_stack.ss_size);

  tlocal->uc[2].uc_link = &tlocal->uc[0];
  tlocal->uc[2].uc_stack.ss_sp = malloc(STACKSIZE);
  tlocal->uc[2].uc_stack.ss_size = STACKSIZE;
  makecontext(&tlocal->uc[2], (void (*)(void))f, 2, tlocal, 2);
  (void)VALGRIND_STACK_REGISTER(tlocal->uc[2].uc_stack.ss_sp,
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
  pthread_attr_t attr;
  int i, res;

  signal(SIGALRM, sig_alrm_handler);
  memset(tlocal, 0, sizeof(tlocal));

  pthread_attr_init(&attr);
  res = pthread_attr_setstacksize(&attr, STACKSIZE);
  assert(res == 0);

  for (i = 0; i < NR; i++)
    pthread_create(&thread[i], &attr, worker, &tlocal[i]);

  pthread_attr_destroy(&attr);

  // Wait until the threads have been created.
  sleep(1);
  for (i = 0; i < NR; i++)
    pthread_kill(thread[i], SIGALRM);

  for (i = 0; i < NR; i++)
    pthread_join(thread[i], NULL);

  return 0;
}
