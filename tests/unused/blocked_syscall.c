
#include <stdio.h>
#include <unistd.h>
#include <assert.h>
#include <signal.h>

int fds[2];

void the_sighandler ( int signo )
{
   int nw;
   //   assert(signo == SIGUSR1);
   printf("sighandler running; should unblock now\n");
   nw = write(fds[1], "zzz", 1);
   //  assert(nw == 1);
}

int main ( void )
{
   char buf[10];
   int res, nr;
   void* oldsh = signal(SIGUSR1, the_sighandler);
   assert(oldsh != SIG_ERR);
   printf("pid = %d\n", getpid());
   res = pipe(fds);
   assert (res == 0);
   printf("doing read(); this should block\n");
   nr = read(fds[0], buf, 1);
   /* blocks */
   printf("read returned %d\n", nr);
   return 0;
}
