
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

static struct sigaction oldChildHandlerData;

void theHandler(int arg)
{
  printf("handled %s\n", arg == SIGCHLD ? "SIGCHLD" : "?!unexpected signal?!" );
}

void setupHandlers()
{
  struct sigaction act;
  act.sa_handler=theHandler;
  sigemptyset(&(act.sa_mask));
  sigaddset(&(act.sa_mask), SIGCHLD);
  // Make sure we don't block this signal. gdb tends to do that :-(
  sigprocmask(SIG_UNBLOCK, &(act.sa_mask), 0);

  act.sa_flags = SA_NOCLDSTOP;

  // CC: take care of SunOS which automatically restarts interrupted system
  // calls (and thus does not have SA_RESTART)

#ifdef SA_RESTART
  act.sa_flags |= SA_RESTART;
#endif

  sigaction( SIGCHLD, &act, &oldChildHandlerData );

  act.sa_handler=SIG_IGN;
  sigemptyset(&(act.sa_mask));
  sigaddset(&(act.sa_mask), SIGPIPE);
  act.sa_flags = 0;
  sigaction( SIGPIPE, &act, 0L);
}

int main()
{
    int i;
    char buffer[200];
    setupHandlers();
    FILE *p = popen("echo Hallo World", "r");
    while (!feof(p)) {
        int n = fread(buffer, 200, 1, p);
        __attribute__((unused)) ssize_t nw = write(2, buffer, n);
    }
    fclose(p);
    for (i = 0; i < 1000000; i++) ;
    return 0;
}
