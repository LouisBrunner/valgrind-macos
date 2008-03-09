/** Test whether DRD recognizes LinuxThreads as LinuxThreads and NPTL as
 *  NPTL.
 */


#include <pthread.h>
#include <semaphore.h>
#include <stdio.h>
#include <unistd.h>


static sem_t s_sem;
static pid_t s_main_thread_pid;


void* thread_func(void* arg)
{
  if (s_main_thread_pid == getpid())
  {
    printf("NPTL or non-Linux POSIX threads implementation detected.\n");
  }
  else
  {
    printf("Detected LinuxThreads as POSIX threads implemenentation.\n");
  }
  sem_post(&s_sem);
  return 0;
}

int main(int argc, char** argv)
{
  pthread_t threadid;

  s_main_thread_pid = getpid();
  sem_init(&s_sem, 0, 0);
  pthread_create(&threadid, 0, thread_func, 0);
  sem_wait(&s_sem);
  pthread_join(threadid, 0);
  sem_destroy(&s_sem);
  return 0;
}
