#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include "tests/sys_mman.h"

static void handler(int sig, siginfo_t *info, void *v)
{
	printf("info: sig=%d code=%d addr=%p\n",
	       info->si_signo, info->si_code, info->si_addr);
	exit(0);
}

/* Blocking a fault (for example SIGSEGV) won't work,
   and is the same as having the default handler. */
int main()
{
	int* unmapped_page = get_unmapped_page();
	struct sigaction sa;
	sigset_t mask;

	sa.sa_sigaction = handler;
	sigemptyset(&sa.sa_mask);
	sa.sa_flags = SA_SIGINFO;
	
	sigaction(SIGSEGV, &sa, NULL);

	sigfillset(&mask);
	sigprocmask(SIG_BLOCK, &mask, NULL);

	*(volatile int *)unmapped_page = 213;

	return 0;
}
