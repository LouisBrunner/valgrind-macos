#include <pthread.h>
#include <signal.h>
#include <stdio.h>
#include <unistd.h>

/* thread function from Galaxy Communicator library */
static void *__Gal_SignalHandler(void *arg)
{
  sigset_t set;
  int sig;
  int res;

  sigfillset(&set);

  while(1) {
    res = sigwait(&set, &sig);
    printf("Received signal number %d\n", sig);
  }
}

/* function from my code */
static void signal_handler(int i)
{
  // nop
}

/* function from my code */
static void *serve(void *arg)
{
  sigset_t sig;
  sigemptyset(&sig);
  sigaddset(&sig, SIGUSR1);
  pthread_sigmask(SIG_UNBLOCK, &sig, NULL);

  for(;;) {
    /* somewhere in here, deeply buried within libcapi20, is a select(),
       that I am interrupting by sending SIGUSR1 to this thread */
  }
}

int main( void )
{
  pthread_t sig_thread, serve_thread;
  pthread_attr_t sig_attr;
  struct sigaction sigact;

  /* from Galaxy Communicator library */
  pthread_attr_init(&sig_attr);
  pthread_attr_setdetachstate(&sig_attr, PTHREAD_CREATE_DETACHED);
  pthread_create(&sig_thread, &sig_attr, __Gal_SignalHandler, NULL);

  /* from my code */
  sigemptyset(&sigact.sa_mask);
  sigact.sa_handler = signal_handler;
  sigact.sa_flags = 0;
  sigaction(SIGUSR1, &sigact, NULL);
  pthread_create(&serve_thread, NULL, serve, NULL);

  /* happens within my code */
  for(;;) {
    pthread_kill(serve_thread, SIGUSR1);
    sleep(1);
  }
  return 0;
}
