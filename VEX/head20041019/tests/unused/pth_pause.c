
#include <stdio.h>
#include <unistd.h>
#include <pthread.h>
#include <signal.h>
#include <errno.h>

void hdlr ( int sig )
{
   printf("signal %d arrived\n", sig);
}

int main ( void )
{
  int res;
  /* Force use of libpthread here */
  pthread_testcancel();

  printf("installing handler\n");
  signal(SIGINT, hdlr);
  printf("installing handler done; please do Control-C\n");

  res = pause();
  printf("pause done; res = %d, errno = %d\n", res, errno);

  printf("bye\n");

  return 0;
}
