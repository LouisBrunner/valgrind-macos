#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <stdio.h>
#include <signal.h>
#include <pthread.h>

static void sig_usr1(int);

static pthread_t main_thread;

void *
child_main(void *no_args)
{
//  int i;
  
// Only do it once, to shorten test --njn
//  for (i = 0; i < 5; ++i)
//    {
      sleep (1);
      fprintf (stdout, "thread CHILD sending SIGUSR1 to thread MAIN\n");
      if (pthread_kill (main_thread, SIGUSR1) != 0)
        fprintf (stderr, "error doing pthread_kill\n"); 
//    }

  return no_args;
}

int
main(void)
{
  struct sigaction sigact;
  sigset_t newmask, oldmask;
  pthread_t child;

  memset(&newmask, 0, sizeof newmask);
  sigemptyset (&newmask);
  sigaddset (&newmask, SIGUSR1);

  if (pthread_sigmask (SIG_BLOCK, &newmask, &oldmask) != 0)
    fprintf (stderr, "SIG_BLOCK error");
  
  memset (&sigact, 0, sizeof sigact);
  sigact.sa_handler = sig_usr1;
  if (sigaction(SIGUSR1, &sigact, NULL) != 0)
    fprintf (stderr, "signal(SIGINT) error");
  
  main_thread = pthread_self ();
  if (pthread_create (&child, NULL, child_main, NULL) != 0)
    fprintf (stderr, "error creating thread");

  pthread_join (child, NULL);
  
  exit(0);
}

static void
sig_usr1 (int signo)
{
  fprintf (stderr, "SHOULD NOT BE HERE (SIGUSR1)!!!!\n");
  return;
}


