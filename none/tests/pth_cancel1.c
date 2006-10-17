#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <unistd.h>

static void thread_cleanup(void *arg)
{
  printf("cleaning up 0x%lx\n", (long)arg);

  return;
}

static void *thread_main(void *arg)
{
  pthread_cleanup_push(thread_cleanup, (void *)0x1234);
  pthread_cleanup_push(thread_cleanup, (void *)0x5678);

  if (pthread_setcanceltype(PTHREAD_CANCEL_ASYNCHRONOUS, NULL) != 0)
    {
      perror("pthread_setcanceltype");
      return NULL;
    }
  
  if (pthread_setcancelstate(PTHREAD_CANCEL_ENABLE, NULL) != 0)
    {
      perror("pthread_setcancelstate");
      return NULL;
    }

  pause();

  pthread_cleanup_pop(0);
  pthread_cleanup_pop(0);

  return NULL;
}

int main(int argc, char **argv)
{
  pthread_t tid;
  void *result;
  
  if (pthread_create(&tid, NULL, thread_main, NULL) != 0)
    {
      perror("pthread_create");
      exit(1);
    }

  sleep(1);

  if (pthread_cancel(tid) != 0)
    {
      perror("pthread_cancel");
      exit(1);
    }

  if (pthread_join(tid, &result) != 0)
    {
      perror("pthread_join");
      exit(1);
    }

  printf("result is %s\n", result == PTHREAD_CANCELED ? "correct" : "incorrect");
  
  exit(0);
}
