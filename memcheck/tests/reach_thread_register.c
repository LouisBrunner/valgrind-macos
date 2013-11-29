#define _GNU_SOURCE 

#include <pthread.h>
#include <sched.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <config.h>

/* test based on code from Jeffrey Yasskin, slightly modified. */
/* Reproduces a false positive leak when a pointer is (only) in a live
   thread register, and another thread calls exit */

pthread_mutex_t mu = PTHREAD_MUTEX_INITIALIZER;
int cont = 1;

void* helper(void* v_bar) {
  pthread_barrier_t* bar = (pthread_barrier_t*)v_bar;
  register int* i = malloc(sizeof(*i));
  // Try hard to have i allocated in a register.
  *i = 3;
  pthread_barrier_wait(bar);
  pthread_mutex_lock(&mu);
  while (cont) {
#if defined(VGA_x86) || defined(VGA_amd64)
     // Below helps to have i really in a register. 
     asm volatile("test %0, %0" : : "S"(i));
#else
     // Not clear that for other archs, i is in a register.
     if (*i) // should do better for other archs.
        // "then" part after the #endif
#endif
     pthread_mutex_unlock(&mu);
     sched_yield();
     pthread_mutex_lock(&mu);
  }
  pthread_mutex_unlock(&mu);
  free((void *)i);
  fprintf(stderr, "Quitting the helper.\n");
  return NULL;
}

int main() {
  pthread_barrier_t bar;
  pthread_barrier_init(&bar, NULL, 2);
  pthread_t thr;
  pthread_create(&thr, NULL, &helper, &bar);
  pthread_barrier_wait(&bar);
  pthread_barrier_destroy(&bar);
  fprintf(stderr, "Abandoning the helper.\n");
  pthread_detach(thr);
  return 0;
}
