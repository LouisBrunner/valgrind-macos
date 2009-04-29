#include <stdio.h>
#include <stdlib.h>
#include <signal.h>

static void handler(int sig, siginfo_t *info, void *v)
{
	printf("info: sig=%d code=%d addr=0x%lx\n",
	       info->si_signo, info->si_code, (unsigned long)info->si_addr);
	exit(0);
}

int main()
{
	struct sigaction sa;
	int val;

	sa.sa_sigaction = handler;
	sigfillset(&sa.sa_mask);
	sa.sa_flags = SA_SIGINFO;
	
	sigaction(SIGSEGV, &sa, NULL);

	asm volatile("mov %1, %%fs; mov %%fs:0, %0" : "=r" (val) : "r"(4));

	printf("val=%d\n", val);

	return 0;
}
