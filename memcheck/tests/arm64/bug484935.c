#include <assert.h>
#include <signal.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>

static void signalHandler(int sig, siginfo_t* info, void* uctx_v)
{
   if (sig != SIGALRM)
      abort();
   if (info == 0)
      abort();
   if (uctx_v == 0)
      abort();
}

void* load_memory_content(void** ptr)
{
   void* result;
   __asm__ volatile(
      // load x0, x1, x2 with data from ptr, and loop for a while. If we get
      // a signal in the loop, these registers have uninitialized data in
      // them, but should be valid inside the signal handler. Without our
      // patch, valgrind complains. We can remove the individual lines from
      // the patch, and see each argument in turn affecting valgrind
      "LDR x0, [%1]\n"
      "LDR x1, [%1, #8]\n"
      "LDR x2, [%1, #16]\n"
      "mov %0, x0\n"
      "mov x3, #2000\n"
      "loop:"
      " subs x3, x3, #1\n"
      " b.ne loop\n"
      : "=r"(result)
      : "r"(ptr)
      : "x0", "x1", "x2", "x3");
   return result;
}

int main()
{
   struct sigaction sa;
   memset(&sa, 0, sizeof sa);
   sa.sa_flags     = SA_SIGINFO;
   sa.sa_sigaction = signalHandler;
   int rc          = sigaction(SIGALRM, &sa, 0);
   assert(rc == 0);
   struct itimerval timer = {{0, 1000}, {0, 1000}};
   setitimer(ITIMER_REAL, &timer, 0);
   void** q = malloc(100);
   for (int i = 0; i < 1000; ++i)
      load_memory_content(q);
}
