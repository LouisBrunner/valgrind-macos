/* This changes the definition of ucontext_t */
#define _XOPEN_SOURCE 1
#include <stdio.h>
#include <unistd.h>
#include <signal.h>
#include <string.h>
#include <stdbool.h>
#include <valgrind.h>

#define offsetof(type, fld)	((unsigned long)&((type *)0)->fld)
#define stringify(x)		#x

static int verbose = 0;

#define _ASSERT_OP(a, op, b) \
    do { \
	unsigned long long _a = (unsigned long long)(a); \
	unsigned long long _b = (unsigned long long)(b); \
	if (verbose) \
	    fprintf(stderr, "%s:%d: ASSERT(" stringify(a) \
		    " " stringify(op) " " stringify(b) ")\n", \
		    __FILE__, __LINE__); \
	if (!(_a op _b)) { \
	    fprintf(stderr, "%s:%d: FAILED ASSERT((" stringify(a) \
		    "=0x%016llx) " stringify(op) " (" stringify(b) "=0x%016llx))\n", \
		    __FILE__, __LINE__, _a, _b); \
	    _exit(1); \
	} \
    } while(0)
#define ASSERT_EQ(a, b) _ASSERT_OP(a, ==, b)
#define ASSERT_NE(a, b) _ASSERT_OP(a, !=, b)
#define ASSERT_LTE(a, b) _ASSERT_OP(a, <=, b)
#define ASSERT_GTE(a, b) _ASSERT_OP(a, >=, b)
#define ASSERT(e) \
    do { \
	if (verbose) \
	    fprintf(stderr, "%s:%d: ASSERT(" stringify(e) ")\n", \
		    __FILE__, __LINE__); \
	if (!(e)) { \
	    fprintf(stderr, "%s:%d: FAILED ASSERT(" stringify(e) ")\n", \
		    __FILE__, __LINE__); \
	    _exit(1); \
	} \
    } while(0)


static bool using_int3 = false;
static volatile int sig_count = 0;
static volatile int ran_after_fault = 0;
static void *top_of_stack;
static void *bottom_of_stack;

void this_function_halts(void)
{
    int foo;
    bottom_of_stack = &foo - 4;
    /* EAX is used by compiler-generated code to
     * increment ran_after_fault, so we need to
     * preserve it in our asm section */
    unsigned long saved_eax;

    /* Set up registers with known values which will be tested in the signal handler */
    __asm__ volatile("movl %%eax, %0" : "=m"(saved_eax));
    __asm__ volatile("movl $0xfeed0101,%eax");
    __asm__ volatile("movl $0xfeed0202,%ebx");
    __asm__ volatile("movl $0xfeed0303,%ecx");
    __asm__ volatile("movl $0xfeed0404,%edx");
    __asm__ volatile("movl $0xfeed0505,%edi");
    __asm__ volatile("movl $0xfeed0606,%esi");
    __asm__ volatile("hlt");
    __asm__ volatile("movl %0, %%eax" : : "m"(saved_eax));
    ran_after_fault++;
}

void this_function_int3s(void)
{
    int foo;
    bottom_of_stack = &foo - 4;
    unsigned long saved_eax;

    /* Set up registers with known values which will be tested in the signal handler */
    __asm__ volatile("movl %%eax, %0" : "=m"(saved_eax));
    __asm__ volatile("movl $0xfeed0101,%eax");
    __asm__ volatile("movl $0xfeed0202,%ebx");
    __asm__ volatile("movl $0xfeed0303,%ecx");
    __asm__ volatile("movl $0xfeed0404,%edx");
    __asm__ volatile("movl $0xfeed0505,%edi");
    __asm__ volatile("movl $0xfeed0606,%esi");
    __asm__ volatile("int $3");
    __asm__ volatile("movl %0, %%eax" : : "m"(saved_eax));
    ran_after_fault++;
}


static void
handle_signal(int sig, siginfo_t *si, void *vuc)
{
    ucontext_t *uc = (ucontext_t *)vuc;

    if (verbose)
    {
	fprintf(stderr, "handle_signal\n");
	fflush(stderr);
    }

    sig_count++;
    ASSERT(sig_count == 1);

    int expected_sig = (using_int3 ? SIGTRAP : SIGSEGV);
    ASSERT_EQ(sig, expected_sig);
    ASSERT_NE(si, NULL);
    ASSERT_NE(uc, NULL);
    ASSERT_NE(uc->uc_mcontext, NULL);

    /* Test that the siginfo is set up right for this signal */
    ASSERT_EQ(si->si_signo, expected_sig);
    ASSERT_EQ(si->si_errno, 0);
    int expected_code = (using_int3 ? 1 : 0);
    ASSERT_EQ(si->si_code, expected_code);
    ASSERT_EQ(si->si_pid, 0);
    ASSERT_EQ(si->si_uid, 0);
    ASSERT_EQ(si->si_status, 0);
    ASSERT_EQ(si->si_addr, 0);
    ASSERT_EQ(si->si_band, 0);

    /* Test that various registers were saved in the signal ucontext */
    ASSERT_EQ(uc->uc_mcontext->__ss.__eax, 0xfeed0101);
    ASSERT_EQ(uc->uc_mcontext->__ss.__ebx, 0xfeed0202);
    ASSERT_EQ(uc->uc_mcontext->__ss.__ecx, 0xfeed0303);
    ASSERT_EQ(uc->uc_mcontext->__ss.__edx, 0xfeed0404);
    ASSERT_EQ(uc->uc_mcontext->__ss.__edi, 0xfeed0505);
    ASSERT_EQ(uc->uc_mcontext->__ss.__esi, 0xfeed0606);

    /* Test that the saved EBP and ESP point into roughly the right
     * part of the stack */
    ASSERT_GTE(uc->uc_mcontext->__ss.__ebp, bottom_of_stack);
    ASSERT_LTE(uc->uc_mcontext->__ss.__ebp, top_of_stack);
    ASSERT_GTE(uc->uc_mcontext->__ss.__esp, bottom_of_stack);
    ASSERT_LTE(uc->uc_mcontext->__ss.__esp, top_of_stack);

    /* Test that the saved EIP points into roughly the
     * right part of the text segment */
    char *calling_fn = (using_int3 ? (char *)&this_function_int3s : (char *)&this_function_halts);
    ASSERT_GTE(uc->uc_mcontext->__ss.__eip, calling_fn);
    ASSERT_LTE(uc->uc_mcontext->__ss.__eip, calling_fn+400);

    /*
    printf("	    RFLAGS 0x%016llx\n", (unsigned long long)uc->uc_mcontext->__ss.__rflags);
    */

    /*
     * Test that the EIP is restored from the signal ucontext;
     * this should skip past the HLT/INT instruction and
     * allow execution to continue back out to main()
     */
    if (verbose)
    {
	fprintf(stderr, "Setting up to return past the HLT\n");
	fflush(stderr);
    }
    uc->uc_mcontext->__ss.__eip += (using_int3 ? 0 : 1);

    if (verbose)
    {
	fprintf(stderr, "Returning from signal handler\n");
	fflush(stderr);
    }
}

int main(int argc, char **argv)
{
    int r;
    struct sigaction act;

    top_of_stack = (void *)&act;

    if (argc > 1 && !strcmp(argv[1], "--verbose"))
	verbose = 1;

    if (verbose)
	printf("Setting up signal handler\n");
    memset(&act, 0, sizeof(act));
    act.sa_sigaction = handle_signal;
    act.sa_flags |= SA_SIGINFO;
    if (RUNNING_ON_VALGRIND)
	using_int3 = true;
    r = sigaction((using_int3 ? SIGTRAP : SIGSEGV), &act, NULL);
    ASSERT_EQ(r, 0);

    if (verbose)
    {
	fprintf(stderr, "Calling function with a breakpoint insn in it\n");
	fflush(stderr);
    }
    if (using_int3)
	this_function_int3s();
    else
	this_function_halts();
    ASSERT_EQ(ran_after_fault, 1);

    fprintf(stderr, "PASS\n");
    return 0;
}
