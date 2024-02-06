/* x86 variant of the amd64-solaris/context_fpu.c test. */

#include <assert.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/syscall.h>
#include <ucontext.h>

static siginfo_t si;
static ucontext_t uc;
static float inhandler[8];

static void sighandler(int sig, siginfo_t *sip, void *arg)
{
   int i;
   ucontext_t *ucp = (ucontext_t *) arg;

   si = *sip;
   uc = *ucp;

   /* Reset the FP stack so it's possible to push other values onto it.  (It
      is fully filled in main() before triggering the signal handler).  Note
      that VEX also clears all FP values when the finit instruction is
      executed.  This provides another level of validation that the restore
      code is correct. */
   __asm__ __volatile__(
      "finit\n");

   /* Convert 80b values in mcontext to 32b values in the inhandler array. */
   for (i = 0; i < 8; i++) {
      __asm__ __volatile__(
         "fldt   %[in]\n"
         "fstps  %[out]\n"
         : [out] "=m" (inhandler[i])
         : [in] "m" (*((char*)&ucp->uc_mcontext.fpregs.fp_reg_set.fpchip_state
                       + 28 + i * 10)));
   }
}

int main(void)
{
   struct sigaction sa;
   pid_t pid;
   float out[8];
   float x0;

   /* Uninitialised, but we know px[0] is 0x0. */
   float *px = malloc(sizeof(*px));
   x0 = px[0];

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
      /* Set values in the FP stack. */
      "flds   %[x0]\n"
      "fld1\n"
      "flds   %[x0]\n"
      "fld1\n"
      "flds   %[x0]\n"
      "fld1\n"
      "flds   %[x0]\n"
      "fld1\n"

      /* Prepare syscall parameters. */
      "pushl  %[sig]\n"
      "pushl  %[pid]\n"
      "pushl  $0xdeadbeef\n"
      "movl   %[scall], %%eax\n"

      /* Trigger the signal handler. */
      "int    $0x91\n"
      "addl   $12, %%esp\n"
      "fstps  0x00 + %[out]\n"
      "fstps  0x04 + %[out]\n"
      "fstps  0x08 + %[out]\n"
      "fstps  0x0c + %[out]\n"
      "fstps  0x10 + %[out]\n"
      "fstps  0x14 + %[out]\n"
      "fstps  0x18 + %[out]\n"
      "fstps  0x1c + %[out]\n"
      : [out] "=m" (out[0])
      : [scall] "i" (SYS_kill), [pid] "a" (pid), [sig] "i" (SIGUSR1),
        [x0] "m" (x0)
      : "edx", "cc", "memory");

   printf("Values in the signal handler:\n");
   printf("  fp[0]=%f, fp[2]=%f, fp[4]=%f, fp[6]=%f\n",
          inhandler[0], inhandler[2], inhandler[4], inhandler[6]);
   /* Check that inhandler[1], inhandler[3], inhandler[5] and inhandler[7]
      contain uninitialised values (origin is px[0]). */
   if (inhandler[1] || inhandler[3] || inhandler[5] || inhandler[7])
      assert(0);

   printf("Values after the return from the signal handler:\n");
   printf("  fp[0]=%f, fp[2]=%f, fp[4]=%f, fp[6]=%f\n",
          out[0], out[2], out[4], out[6]);
   /* Check that out[1], out[3], out[5] and out[7] contain uninitialised
      values (origin is px[0]). */
   if (out[1] || out[3] || out[5] || out[7])
      assert(0);

   return 0;
}

