
#include <stdio.h>
#include <unistd.h>
#include <pthread.h>
#include <signal.h>
#include <errno.h>
#include <assert.h>

int show ( void )
{
  int res, i, ret;
  sigset_t pend;
  res = sigpending(&pend);
  printf("pending signals:\n");
  assert(res == 0);
  ret = 0;
  for (i = 1; i < 64; i++) {
     if (sigismember(&pend,i)) {
        printf("   sig %d now pending\n", i);
	ret = 1;
     }
  }
  return ret;
}

void hdlr ( int sig )
{
   printf("signal %d arrived (unexpectedly!)\n", sig);
}

int main ( void )
{
  int res;
  sigset_t set;
  /* Force use of libpthread here */
  pthread_testcancel();

  printf("installing handler\n");
  signal(SIGINT, hdlr);
  /* and block it ... */
  sigemptyset(&set);
  sigaddset(&set, SIGINT);
  res = pthread_sigmask(SIG_BLOCK, &set, NULL);
  assert(res == 0);
  printf("installing handler done; please do Control-C\n");

  while (1) {
     res = show();
     if (res) break;
     sleep(1);
  }
  printf("control-C now pending -- bye\n");

  return 0;
}
