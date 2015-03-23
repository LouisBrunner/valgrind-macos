

#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include "tests/sys_mman.h"

void sig_handler(int sig){
  int var;
  fprintf(stderr, "caught signal, local var is on %p\n", &var);
}

int main(int argv, char** argc) {
  int res, i;
  stack_t sigstk;
  struct sigaction act;
  static const int size = SIGSTKSZ*2;
  // We give EXEC permissions because this won't work on ppc32 unless you
  // ask for an alt stack with EXEC permissions,
  // since signal returning requires execution of code on the stack.      
  char *stk = (char *)mmap(0, size, PROT_READ|PROT_WRITE|PROT_EXEC, 
                                    MAP_ANONYMOUS|MAP_PRIVATE, -1, 0);
  sigstk.ss_sp = stk;

  sigstk.ss_size = size;
  sigstk.ss_flags = 0;
  fprintf(stderr, "calling sigaltstack, stack base is %p\n", sigstk.ss_sp);
  if (sigaltstack(&sigstk,0)<0) perror("sigaltstack");

  fprintf(stderr,"setting sigaction\n");
  act.sa_flags=SA_ONSTACK;
  act.sa_handler=&sig_handler;
  sigemptyset(&act.sa_mask);
  res = sigaction(SIGUSR1,&act,0);
  fprintf(stderr, "res = %d\n", res);
  fprintf(stderr, "raising the signal\n");
  raise(SIGUSR1);
  
  /* Loop long enough so valgrind has a forced context switch and
     actually delivers the signal before the thread exits. */
  for (i = 0; i < 1000000; i++) ;

  fprintf(stderr, "done\n");
  return 0;
}
