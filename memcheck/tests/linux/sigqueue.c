/* Test program that invokes the Linux system call rt_sigqueueinfo(). */

#include <signal.h>
#include <string.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <syscall.h>
#include <unistd.h>

int main()
{
  siginfo_t *si;
  const size_t sz = sizeof(*si);

  fprintf(stderr, "sizeof(*si) = %zu\n", sz);
  fprintf(stderr, "%zd %zd %zd %zd\n",
          offsetof(siginfo_t, si_signo),
          offsetof(siginfo_t, si_errno),
          offsetof(siginfo_t, si_code),
          offsetof(siginfo_t, _sifields)
         );
  si = calloc(1, sz);
  si->si_signo = SIGWINCH;
  si->si_code = SI_QUEUE;
  si->si_pid = getpid();
  si->si_uid = getuid();
  syscall(__NR_rt_sigqueueinfo, getpid(), SIGWINCH, si);
  free(si);
  fprintf(stderr, "Done.\n");
  return 0;
}
