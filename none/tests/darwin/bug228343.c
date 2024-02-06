// https://bugs.kde.org/show_bug.cgi?id=228343

#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <signal.h>
#include <libkern/OSAtomic.h>
#include <pthread.h>

OSSpinLock sl = OS_SPINLOCK_INIT;
typedef void *(*worker_t)(void*);
typedef void (*Sigaction)(int, siginfo_t *, void *);

int GLOB=0;

static void EnableSigprof(Sigaction SignalHandler) {
 struct sigaction sa;
 sa.sa_sigaction = SignalHandler;
 sa.sa_flags = SA_RESTART | SA_SIGINFO;
 sigemptyset(&sa.sa_mask);
 if (sigaction(SIGPROF, &sa, NULL) != 0) {
   perror("sigaction");
   abort();
 }
 struct itimerval timer;
 timer.it_interval.tv_sec = 0;
 timer.it_interval.tv_usec = 1000000 / 10000;
 timer.it_value = timer.it_interval;
 if (setitimer(ITIMER_PROF, &timer, 0) != 0) {
   perror("setitimer");
   abort();
 }
}

void *Worker() {
 long int i;
 for (i = 0; i < 100000000; i++) {
   void *x = malloc((i % 64) + 1);
   free (x);
 }
 return NULL;
}

void SignalHandlerWithSpinlock(int sig, siginfo_t *siginfo, void *context) {
 OSSpinLockLock(&sl);
 GLOB++;
 OSSpinLockUnlock(&sl);
}

int main() {
 EnableSigprof(SignalHandlerWithSpinlock);
 pthread_t w_1;
 pthread_t w_2;
 pthread_create(&w_1, NULL, Worker, NULL);
 pthread_create(&w_2, NULL, Worker, NULL);
 pthread_join(w_1, NULL);
 pthread_join(w_2, NULL);
 printf("\tGLOB=%d\n", GLOB);
 return 0;
}
