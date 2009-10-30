/* Test program that invokes the Linux system call rt_sigqueueinfo(). */

#include <signal.h>
#include <string.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <syscall.h>
#include <unistd.h>

int main(int argc, char **argv)
{
  siginfo_t *si;
  const size_t sz = sizeof(*si);

  if (argc == 1) {
    fprintf(stderr, "sizeof(*si) = %zu\n", sz);
    fprintf(stdout, "offsetof(siginfo_t, si_signo)  = %zd\n",
            offsetof(siginfo_t, si_signo));
    fprintf(stdout, "offsetof(siginfo_t, si_errno)  = %zd\n",
            offsetof(siginfo_t, si_errno));
    fprintf(stdout, "offsetof(siginfo_t, si_code)   = %zd\n",
            offsetof(siginfo_t, si_code));
    fprintf(stdout, "offsetof(siginfo_t, _sifields) = %zd\n",
            offsetof(siginfo_t, _sifields));
  }
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
