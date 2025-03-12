/* Test if values in integer registers are correctly propagated into and out of
   a signal handler and also check that the same applies for uninitialised
   values and their origins.

   Register usage in the test:
   zero (x0) -- unused
   ra   (x1) -- unused
   sp   (x2) -- unused
   gp   (x3) -- unused
   tp   (x4) -- unused

   t0   (x5) -- holds address of regs_in
   t1   (x6) -- holds address of regs_out
   a0  (x10) -- current pid
   a1  (x11) -- SIGUSR1
   a7  (x17) -- SYS_kill

                before signal   -> in signal handler      -> after return
   t2   (x7) -- 0,def           -> unchanged              -> 0,def
   s0   (x8) -- 0,undef         -> unchanged              -> 0,undef
   s1   (x9) -- 0,def           -> set to 0,undef         -> 0,undef
   a2  (x12) -- 0,undef         -> set to 0,def           -> 0,def
   a3  (x13) -- 1,def           -> increment by 1,def     -> 2,def
   a4  (x14) -- 1,undef         -> increment by 1,def     -> 2,undef
   a5  (x15) -- 1,def           -> increment by 1,undef   -> 2,undef
   a6  (x16) -- 1,undef         -> increment by 1,undef   -> 2,undef
   s2  (x18) -- ULONG_MAX,def   -> unchanged              -> ULONG_MAX,def
   s3  (x19) -- ULONG_MAX,undef -> unchanged              -> ULONG_MAX,undef
   s4  (x20) -- ULONG_MAX,def   -> set to 0,undef         -> 0,undef
   s5  (x21) -- ULONG_MAX,undef -> set to 0,def           -> 0,def
   s6  (x22) -- 0,def           -> set to ULONG_MAX,def   -> ULONG_MAX,def
   s7  (x23) -- 0,undef         -> set to ULONG_MAX,undef -> ULONG_MAX,undef
   s8  (x24) -- 0,def           -> decrement by 0,def     -> 0,def
   s9  (x25) -- 0,undef         -> decrement by 0,def     -> 0,undef
   s10 (x26) -- 0,def           -> decrement by 0,undef   -> 0,undef
   s11 (x27) -- 0,undef         -> decrement by 0,undef   -> 0,undef
   t3  (x28) -- 0,def           -> decrement by 1,def     -> ULONG_MAX,def
   t4  (x29) -- 0,undef         -> decrement by 1,def     -> ULONG_MAX,undef
   t5  (x30) -- 0,def           -> decrement by 1,undef   -> ULONG_MAX,undef
   t6  (x31) -- 0,undef         -> decrement by 1,undef   -> ULONG_MAX,undef
 */

#include <assert.h>
#include <limits.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/syscall.h>
#include <sys/ucontext.h>
#include <unistd.h>
#include <valgrind.h>

static ucontext_t    uc;
static unsigned long x0, x1;

static void sighandler(int sig, siginfo_t* sip, void* arg)
{
   ucontext_t* ucp = (ucontext_t*)arg;

   uc = *ucp;

   ucp->uc_mcontext.__gregs[9]  = x0;
   ucp->uc_mcontext.__gregs[12] = 0;
   ucp->uc_mcontext.__gregs[13] += 1;
   ucp->uc_mcontext.__gregs[14] += 1;
   ucp->uc_mcontext.__gregs[15] += x1;
   ucp->uc_mcontext.__gregs[16] += x1;
   ucp->uc_mcontext.__gregs[20] = x0;
   ucp->uc_mcontext.__gregs[21] = 0;
   ucp->uc_mcontext.__gregs[22] = ULONG_MAX;
   ucp->uc_mcontext.__gregs[23] = ULONG_MAX + x0;
   ucp->uc_mcontext.__gregs[24] -= 0;
   ucp->uc_mcontext.__gregs[25] -= 0;
   ucp->uc_mcontext.__gregs[26] -= x0;
   ucp->uc_mcontext.__gregs[27] -= x0;
   ucp->uc_mcontext.__gregs[28] -= 1;
   ucp->uc_mcontext.__gregs[29] -= 1;
   ucp->uc_mcontext.__gregs[30] -= x1;
   ucp->uc_mcontext.__gregs[31] -= x1;
}

int main(void)
{
   /* Uninitialised, but we know px0[0] is 0x0. */
   unsigned long* px0 = malloc(sizeof(*px0));
   x0                 = px0[0];

   /* Uninitialised, but we know px1[0] is 0x0. */
   unsigned long* px1 = malloc(sizeof(*px1));
   x1                 = px1[0] + 1;

   struct sigaction sa = {};
   sa.sa_sigaction     = sighandler;
   if (sigaction(SIGUSR1, &sa, NULL)) {
      perror("sigaction");
      return 1;
   }

   unsigned long regs_in[22] = {
      0,         x0,
      0,         x0,
      1,         x1,
      1,         x1,
      ULONG_MAX, ULONG_MAX + x0,
      ULONG_MAX, ULONG_MAX + x0,
      0,         x0,
      0,         x0,
      0,         x0,
      0,         x0,
      0,         x0,
   };
   unsigned long regs_out[22] = {};

   pid_t                   pid          = getpid();
   register unsigned long* t0 asm("t0") = regs_in;
   register unsigned long* t1 asm("t1") = regs_out;
   register unsigned long  a7 asm("a7") = SYS_kill;
   register unsigned long  a0 asm("a0") = pid;
   register unsigned long  a1 asm("a1") = SIGUSR1;
   __asm__ __volatile__(
      /* Spill all test registers, keep the 16-byte sp alignment. */
      "add sp, sp, -176\n\t"
      "sd t2, 0(sp)\n\t"
      "sd s0, 8(sp)\n\t"
      "sd s1, 16(sp)\n\t"
      "sd a2, 24(sp)\n\t"
      "sd a3, 32(sp)\n\t"
      "sd a4, 40(sp)\n\t"
      "sd a5, 48(sp)\n\t"
      "sd a6, 56(sp)\n\t"
      "sd s2, 64(sp)\n\t"
      "sd s3, 72(sp)\n\t"
      "sd s4, 80(sp)\n\t"
      "sd s5, 88(sp)\n\t"
      "sd s6, 96(sp)\n\t"
      "sd s7, 104(sp)\n\t"
      "sd s8, 112(sp)\n\t"
      "sd s9, 120(sp)\n\t"
      "sd s10, 128(sp)\n\t"
      "sd s11, 136(sp)\n\t"
      "sd t3, 144(sp)\n\t"
      "sd t4, 152(sp)\n\t"
      "sd t5, 160(sp)\n\t"
      "sd t6, 168(sp)\n\t"

      /* Set values in the test registers. */
      "ld t2, 0(%[in])\n\t"
      "ld s0, 8(%[in])\n\t"
      "ld s1, 16(%[in])\n\t"
      "ld a2, 24(%[in])\n\t"
      "ld a3, 32(%[in])\n\t"
      "ld a4, 40(%[in])\n\t"
      "ld a5, 48(%[in])\n\t"
      "ld a6, 56(%[in])\n\t"
      "ld s2, 64(%[in])\n\t"
      "ld s3, 72(%[in])\n\t"
      "ld s4, 80(%[in])\n\t"
      "ld s5, 88(%[in])\n\t"
      "ld s6, 96(%[in])\n\t"
      "ld s7, 104(%[in])\n\t"
      "ld s8, 112(%[in])\n\t"
      "ld s9, 120(%[in])\n\t"
      "ld s10, 128(%[in])\n\t"
      "ld s11, 136(%[in])\n\t"
      "ld t3, 144(%[in])\n\t"
      "ld t4, 152(%[in])\n\t"
      "ld t5, 160(%[in])\n\t"
      "ld t6, 168(%[in])\n\t"

      /* Trigger the signal handler. */
      "ecall\n\t"

      /* Store updated values in the test registers. */
      "sd t2, 0(%[out])\n\t"
      "sd s0, 8(%[out])\n\t"
      "sd s1, 16(%[out])\n\t"
      "sd a2, 24(%[out])\n\t"
      "sd a3, 32(%[out])\n\t"
      "sd a4, 40(%[out])\n\t"
      "sd a5, 48(%[out])\n\t"
      "sd a6, 56(%[out])\n\t"
      "sd s2, 64(%[out])\n\t"
      "sd s3, 72(%[out])\n\t"
      "sd s4, 80(%[out])\n\t"
      "sd s5, 88(%[out])\n\t"
      "sd s6, 96(%[out])\n\t"
      "sd s7, 104(%[out])\n\t"
      "sd s8, 112(%[out])\n\t"
      "sd s9, 120(%[out])\n\t"
      "sd s10, 128(%[out])\n\t"
      "sd s11, 136(%[out])\n\t"
      "sd t3, 144(%[out])\n\t"
      "sd t4, 152(%[out])\n\t"
      "sd t5, 160(%[out])\n\t"
      "sd t6, 168(%[out])\n\t"

      /* Restore their original values. */
      "ld t2, 0(sp)\n\t"
      "ld s0, 8(sp)\n\t"
      "ld s1, 16(sp)\n\t"
      "ld a2, 24(sp)\n\t"
      "ld a3, 32(sp)\n\t"
      "ld a4, 40(sp)\n\t"
      "ld a5, 48(sp)\n\t"
      "ld a6, 56(sp)\n\t"
      "ld s2, 64(sp)\n\t"
      "ld s3, 72(sp)\n\t"
      "ld s4, 80(sp)\n\t"
      "ld s5, 88(sp)\n\t"
      "ld s6, 96(sp)\n\t"
      "ld s7, 104(sp)\n\t"
      "ld s8, 112(sp)\n\t"
      "ld s9, 120(sp)\n\t"
      "ld s10, 128(sp)\n\t"
      "ld s11, 136(sp)\n\t"
      "ld t3, 144(sp)\n\t"
      "ld t4, 152(sp)\n\t"
      "ld t5, 160(sp)\n\t"
      "ld t6, 168(sp)\n\t"
      "add sp, sp, 176\n\t"
      :
      : [in] "r"(t0), [out] "r"(t1), "r"(a7), "r"(a0), "r"(a1)
      : "memory");

   printf("Values before the signal:\n");
   VALGRIND_DISABLE_ERROR_REPORTING;
   printf("  t2=%#lx\n", regs_in[0]);
   printf("  s0=%#lx\n", regs_in[1]);
   printf("  s1=%#lx\n", regs_in[2]);
   printf("  a2=%#lx\n", regs_in[3]);
   printf("  a3=%#lx\n", regs_in[4]);
   printf("  a4=%#lx\n", regs_in[5]);
   printf("  a5=%#lx\n", regs_in[6]);
   printf("  a6=%#lx\n", regs_in[7]);
   printf("  s2=%#lx\n", regs_in[8]);
   printf("  s3=%#lx\n", regs_in[9]);
   printf("  s4=%#lx\n", regs_in[10]);
   printf("  s5=%#lx\n", regs_in[11]);
   printf("  s6=%#lx\n", regs_in[12]);
   printf("  s7=%#lx\n", regs_in[13]);
   printf("  s8=%#lx\n", regs_in[14]);
   printf("  s9=%#lx\n", regs_in[15]);
   printf("  s10=%#lx\n", regs_in[16]);
   printf("  s11=%#lx\n", regs_in[17]);
   printf("  t3=%#lx\n", regs_in[18]);
   printf("  t4=%#lx\n", regs_in[19]);
   printf("  t5=%#lx\n", regs_in[20]);
   printf("  t6=%#lx\n", regs_in[21]);
   VALGRIND_ENABLE_ERROR_REPORTING;
   /* Check which registers contain uninitialized values. */
   assert(regs_in[0] == 0);
   assert(regs_in[1] == 0);
   assert(regs_in[2] == 0);
   assert(regs_in[3] == 0);
   assert(regs_in[4] == 1);
   assert(regs_in[5] == 1);
   assert(regs_in[6] == 1);
   assert(regs_in[7] == 1);
   assert(regs_in[8] == ULONG_MAX);
   assert(regs_in[9] == ULONG_MAX);
   assert(regs_in[10] == ULONG_MAX);
   assert(regs_in[11] == ULONG_MAX);
   assert(regs_in[12] == 0);
   assert(regs_in[13] == 0);
   assert(regs_in[14] == 0);
   assert(regs_in[15] == 0);
   assert(regs_in[16] == 0);
   assert(regs_in[17] == 0);
   assert(regs_in[18] == 0);
   assert(regs_in[19] == 0);
   assert(regs_in[20] == 0);
   assert(regs_in[21] == 0);

   printf("Values in the signal handler:\n");
   VALGRIND_DISABLE_ERROR_REPORTING;
   printf("  t2=%#lx\n", uc.uc_mcontext.__gregs[7]);
   printf("  s0=%#lx\n", uc.uc_mcontext.__gregs[8]);
   printf("  s1=%#lx\n", uc.uc_mcontext.__gregs[9]);
   printf("  a2=%#lx\n", uc.uc_mcontext.__gregs[12]);
   printf("  a3=%#lx\n", uc.uc_mcontext.__gregs[13]);
   printf("  a4=%#lx\n", uc.uc_mcontext.__gregs[14]);
   printf("  a5=%#lx\n", uc.uc_mcontext.__gregs[15]);
   printf("  a6=%#lx\n", uc.uc_mcontext.__gregs[16]);
   printf("  s2=%#lx\n", uc.uc_mcontext.__gregs[18]);
   printf("  s3=%#lx\n", uc.uc_mcontext.__gregs[19]);
   printf("  s4=%#lx\n", uc.uc_mcontext.__gregs[20]);
   printf("  s5=%#lx\n", uc.uc_mcontext.__gregs[21]);
   printf("  s6=%#lx\n", uc.uc_mcontext.__gregs[22]);
   printf("  s7=%#lx\n", uc.uc_mcontext.__gregs[23]);
   printf("  s8=%#lx\n", uc.uc_mcontext.__gregs[24]);
   printf("  s9=%#lx\n", uc.uc_mcontext.__gregs[25]);
   printf("  s10=%#lx\n", uc.uc_mcontext.__gregs[26]);
   printf("  s11=%#lx\n", uc.uc_mcontext.__gregs[27]);
   printf("  t3=%#lx\n", uc.uc_mcontext.__gregs[28]);
   printf("  t4=%#lx\n", uc.uc_mcontext.__gregs[29]);
   printf("  t5=%#lx\n", uc.uc_mcontext.__gregs[30]);
   printf("  t6=%#lx\n", uc.uc_mcontext.__gregs[31]);
   VALGRIND_ENABLE_ERROR_REPORTING;
   assert(uc.uc_mcontext.__gregs[7] == 0);
   assert(uc.uc_mcontext.__gregs[8] == 0);
   assert(uc.uc_mcontext.__gregs[9] == 0);
   assert(uc.uc_mcontext.__gregs[12] == 0);
   assert(uc.uc_mcontext.__gregs[13] == 1);
   assert(uc.uc_mcontext.__gregs[14] == 1);
   assert(uc.uc_mcontext.__gregs[15] == 1);
   assert(uc.uc_mcontext.__gregs[16] == 1);
   assert(uc.uc_mcontext.__gregs[18] == ULONG_MAX);
   assert(uc.uc_mcontext.__gregs[19] == ULONG_MAX);
   assert(uc.uc_mcontext.__gregs[20] == ULONG_MAX);
   assert(uc.uc_mcontext.__gregs[21] == ULONG_MAX);
   assert(uc.uc_mcontext.__gregs[22] == 0);
   assert(uc.uc_mcontext.__gregs[23] == 0);
   assert(uc.uc_mcontext.__gregs[24] == 0);
   assert(uc.uc_mcontext.__gregs[25] == 0);
   assert(uc.uc_mcontext.__gregs[26] == 0);
   assert(uc.uc_mcontext.__gregs[27] == 0);
   assert(uc.uc_mcontext.__gregs[28] == 0);
   assert(uc.uc_mcontext.__gregs[29] == 0);
   assert(uc.uc_mcontext.__gregs[30] == 0);
   assert(uc.uc_mcontext.__gregs[31] == 0);

   printf("Values after return from the signal handler:\n");
   VALGRIND_DISABLE_ERROR_REPORTING;
   printf("  t2=%#lx\n", regs_out[0]);
   printf("  s0=%#lx\n", regs_out[1]);
   printf("  s1=%#lx\n", regs_out[2]);
   printf("  a2=%#lx\n", regs_out[3]);
   printf("  a3=%#lx\n", regs_out[4]);
   printf("  a4=%#lx\n", regs_out[5]);
   printf("  a5=%#lx\n", regs_out[6]);
   printf("  a6=%#lx\n", regs_out[7]);
   printf("  s2=%#lx\n", regs_out[8]);
   printf("  s3=%#lx\n", regs_out[9]);
   printf("  s4=%#lx\n", regs_out[10]);
   printf("  s5=%#lx\n", regs_out[11]);
   printf("  s6=%#lx\n", regs_out[12]);
   printf("  s7=%#lx\n", regs_out[13]);
   printf("  s8=%#lx\n", regs_out[14]);
   printf("  s9=%#lx\n", regs_out[15]);
   printf("  s10=%#lx\n", regs_out[16]);
   printf("  s11=%#lx\n", regs_out[17]);
   printf("  t3=%#lx\n", regs_out[18]);
   printf("  t4=%#lx\n", regs_out[19]);
   printf("  t5=%#lx\n", regs_out[20]);
   printf("  t6=%#lx\n", regs_out[21]);
   VALGRIND_ENABLE_ERROR_REPORTING;
   assert(regs_out[0] == 0);
   assert(regs_out[1] == 0);
   assert(regs_out[2] == 0);
   assert(regs_out[3] == 0);
   assert(regs_out[4] == 2);
   assert(regs_out[5] == 2);
   assert(regs_out[6] == 2);
   assert(regs_out[7] == 2);
   assert(regs_out[8] == ULONG_MAX);
   assert(regs_out[9] == ULONG_MAX);
   assert(regs_out[10] == 0);
   assert(regs_out[11] == 0);
   assert(regs_out[12] == ULONG_MAX);
   assert(regs_out[13] == ULONG_MAX);
   assert(regs_out[14] == 0);
   assert(regs_out[15] == 0);
   assert(regs_out[16] == 0);
   assert(regs_out[17] == 0);
   assert(regs_out[18] == ULONG_MAX);
   assert(regs_out[19] == ULONG_MAX);
   assert(regs_out[20] == ULONG_MAX);
   assert(regs_out[21] == ULONG_MAX);

   free(px0);
   free(px1);

   return 0;
}
