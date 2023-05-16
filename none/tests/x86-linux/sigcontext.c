#define _GNU_SOURCE

#include <stdio.h>
#include <signal.h>
#include <unistd.h>
#include <sys/ucontext.h>
#include <syscall.h>

#define VAL1 0x11223344
#define VAL2 0x44332211

static void handler1(int sig, siginfo_t *si, ucontext_t *uc)
{
	/* Since the handler will be called as kill leaves the kernel,
	   this is replacing the kill syscall's return value. */
	if (uc->uc_mcontext.gregs[REG_EAX] != 0)
		printf("FAILED: handler2 expected eax == 0, not %d\n", uc->uc_mcontext.gregs[REG_EAX]);
	uc->uc_mcontext.gregs[REG_EAX] = VAL1;

	asm volatile (
		"movl	$0, %%edx\n"
		"movl	$0, %%esi\n"
		"movl	$0, %%edi\n"
		: : : "edx", "esi", "edi");
}

static void handler2(int sig, struct sigcontext sc)
{
	/* Since the handler will be called as kill leaves the kernel,
	   this is replacing the kill syscall's return value. */
	if (sc.eax != 0)
		printf("FAILED: handler2 expected eax == 0, not %p\n", (void*)sc.eax);

	sc.eax = VAL2;

	asm volatile (
		"movl	$0, %%edx\n"
		"movl	$0, %%esi\n"
		"movl	$0, %%edi\n"
		: : : "edx", "esi", "edi");
}

int main()
{
	struct sigaction sa;
	int ret;
	int v2, v3, v4;

	sa.sa_handler = (void*)handler1;
	sa.sa_flags = SA_SIGINFO;
	sigfillset(&sa.sa_mask);

	sigaction(SIGUSR1, &sa, NULL);

	sa.sa_handler = (void*)handler2;
	sa.sa_flags = 0;
	sigfillset(&sa.sa_mask);

	sigaction(SIGUSR2, &sa, NULL);

	asm volatile (
		//"movl	$0x11111111, %%ebp\n"
		"movl	$0x22222222, %%edx\n"
		"movl	$0x33333333, %%esi\n"
		"movl	$0x44444444, %%edi\n"
		"int $0x80"
		: "=a" (ret),  "=d" (v2), "=S" (v3), "=D" (v4)
		: "0" (__NR_kill), "b" (getpid()), "c" (SIGUSR1));
	printf("v2=%x v3=%x v4=%x\n", v2, v3, v4);

	if (ret == VAL1)
		printf("PASS %x\n", ret);
	else
		printf("FAIL ret=%x not %x\n", ret, VAL1);

	asm volatile (
		//"movl	$0x11111111, %%ebp\n"
		"movl	$0x22222222, %%edx\n"
		"movl	$0x33333333, %%esi\n"
		"movl	$0x44444444, %%edi\n"
		"int $0x80"
		: "=a" (ret),  "=d" (v2), "=S" (v3), "=D" (v4)
		: "0" (__NR_kill), "b" (getpid()), "c" (SIGUSR2));
	printf("v2=%x v3=%x v4=%x\n", v2, v3, v4);

	if (ret == VAL2)
		printf("PASS %x\n", ret);
	else
		printf("FAIL ret=%x not %x\n", ret, VAL2);

	return 0;
}
