
#include <stdio.h>
#include <pthread.h>
#include <signal.h>


int main ( void )
{
  int res, sig;
  sigset_t set;
  sigfillset(&set);

  /* block all signals */
  pthread_sigmask(SIG_BLOCK, &set, NULL);

  printf("send me a signal, any signal\n");

  /* Wait for any signal in the set */
  res = sigwait(&set, &sig);

  printf("sigwait returned, res = %d, sig = %d\n", res, sig);
  return 0;
}
