/* x86 variant of the amd64-solaris/context_sse.c test. */

#include <assert.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/syscall.h>
#include <ucontext.h>

#include "config.h"

static siginfo_t si;
static ucontext_t uc;
/* x0 is always zero, but is visible to Valgrind as uninitialised. */
static upad128_t x0;
static upad128_t d0 = {0};

static void sighandler(int sig, siginfo_t *sip, void *arg)
{
   ucontext_t *ucp = (ucontext_t *) arg;

   si = *sip;
   uc = *ucp;

   ucp->uc_mcontext.fpregs.fp_reg_set.fpchip_state.xmm[0] = d0;
   ucp->uc_mcontext.fpregs.fp_reg_set.fpchip_state.xmm[1] = x0;
}

int main(void)
{
   struct sigaction sa;
   pid_t pid;
   upad128_t out[8];
   upad128_t y0;

#if defined(SOLARIS_FPCHIP_STATE_TAKES_UNDERSCORE)
   struct _fpchip_state *fs;
#else
   struct fpchip_state *fs;
#endif
   fs = &uc.uc_mcontext.fpregs.fp_reg_set.fpchip_state;

   /* Uninitialised, but we know px[0] is 0x0. */
   upad128_t *px = malloc(sizeof(*px));
   x0 = px[0];

   /* Uninitialised, but we know py[0] is 0x0. */
   upad128_t *py = malloc(sizeof(*py));
   y0 = py[0];

   sa.sa_sigaction = sighandler;
   sa.sa_flags = SA_SIGINFO;
   if (sigfillset(&sa.sa_mask)) {
      perror("sigfillset");
      return 1;
   }
   if (sigaction(SIGUSR1, &sa, NULL)) {
      perror("sigaction");
      return 1;
   }

   pid = getpid();

   __asm__ __volatile__(
      /* Set values in the SSE registers. */
      "movups %[y0], %%xmm0\n"
      "movups %[d0], %%xmm1\n"
      "movups %[d0], %%xmm2\n"
      "movups %[y0], %%xmm3\n"
      "movups %[y0], %%xmm4\n"
      "movups %[d0], %%xmm5\n"
      "movups %[d0], %%xmm6\n"
      "movups %[y0], %%xmm7\n"

      /* Prepare syscall parameters. */
      "pushl  %[sig]\n"
      "pushl  %[pid]\n"
      "pushl  $0xdeadbeef\n"
      "movl   %[scall], %%eax\n"

      /* Trigger the signal handler. */
      "int    $0x91\n"
      "addl   $12, %%esp\n"
      "movups %%xmm0, 0x00 + %[out]\n"
      "movups %%xmm1, 0x10 + %[out]\n"
      "movups %%xmm2, 0x20 + %[out]\n"
      "movups %%xmm3, 0x30 + %[out]\n"
      "movups %%xmm4, 0x40 + %[out]\n"
      "movups %%xmm5, 0x50 + %[out]\n"
      "movups %%xmm6, 0x60 + %[out]\n"
      "movups %%xmm7, 0x70 + %[out]\n"
      : [out] "=m" (out[0])
      : [scall] "i" (SYS_kill), [pid] "a" (pid), [sig] "i" (SIGUSR1),
        [y0] "m" (y0), [d0] "m" (d0)
      : "edx", "cc", "memory");

   printf("Values in the signal handler:\n");
   printf("  xmm1=%Lf, xmm2=%Lf, xmm5=%Lf, xmm6=%Lf\n",
          fs->xmm[1]._q, fs->xmm[2]._q, fs->xmm[5]._q, fs->xmm[6]._q);
   /* Check that fs->xmm[0], fs->xmm[3], fs->xmm[4] and fs->xmm[7] contain
      uninitialised values (origin is py[0]). */
   if (fs->xmm[0]._q || fs->xmm[3]._q || fs->xmm[4]._q || fs->xmm[7]._q)
      assert(0);

   printf("Values after the return from the signal handler:\n");
   printf("  xmm0=%Lf, xmm2=%Lf, xmm5=%Lf, xmm6=%Lf\n",
          out[0]._q, out[2]._q, out[5]._q, out[6]._q);
   /* Check that out[1], out[3], out[4] and out[7] contain uninitialised
      values (origin is px[0]). */
   if (out[1]._q || out[3]._q || out[4]._q || out[7]._q)
      assert(0);

   return 0;
}

