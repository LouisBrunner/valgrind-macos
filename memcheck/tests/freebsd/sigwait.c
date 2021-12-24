#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

// from stack overflow
// https://stackoverflow.com/questions/6326290/about-the-ambiguous-description-of-sigwait
// except the deliberate errors

static int sig_count;

void on_sigusr1(int sig)
{
    ++sig_count;
}

int main(void)
{
  // Set a signal handler for SIGUSR1
  signal(SIGUSR1, &on_sigusr1);

  // At program startup, SIGUSR1 is neither blocked nor pending, so raising it
  // will call the signal handler
  raise(SIGUSR1);

  // Now let's block SIGUSR1
  sigset_t* psigset = malloc(sizeof(sigset_t));
  sigemptyset(psigset);
  sigaddset(psigset, SIGUSR1);
  sigprocmask(SIG_BLOCK, psigset, NULL);

  // SIGUSR1 is now blocked, raising it will not call the signal handler
  raise(SIGUSR1);

  // SIGUSR1 is now blocked and pending -- this call to sigwait will return
  // immediately
  int sig;
  int result = sigwait(psigset, &sig);
  if(result == 0)
    printf("sigwait got signal: %d\n", sig);

  // SIGUSR1 is now no longer pending (but still blocked).  Raise it again and
  // unblock it
  raise(SIGUSR1);
  printf("About to unblock SIGUSR1\n");
  sigprocmask(SIG_UNBLOCK, psigset, NULL);
  printf("Unblocked SIGUSR1\n");

  assert(sig_count == 2);

  // now a couple of bad params
  // reblock
  sigprocmask(SIG_BLOCK, psigset, NULL);
  raise(SIGUSR1);

  int* psig = malloc(sizeof(int));
  free(psig);
  result = sigwait(psigset, psig);

  free(psigset);

  raise(SIGUSR1);

  result = sigwait(psigset, &sig);

  return 0;
}

