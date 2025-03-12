/* Test if values in floating-point registers are correctly propagated into and
   out of a signal handler and also check that the same applies for
   uninitialised values and their origins.

   Register usage in the test:
              before signal -> in signal handler    -> after return
   f0      -- 0,def         -> unchanged            -> 0,def
   f1      -- 0,undef       -> unchanged            -> 0,undef
   f2      -- 0,def         -> set to 0,undef       -> 0,undef
   f3      -- 0,undef       -> set to 0,def         -> 0,def
   f4      -- 1,def         -> increment by 1,def   -> 2,def
   f5      -- 1,undef       -> increment by 1,def   -> 2,undef
   f6      -- 1,def         -> increment by 1,undef -> 2,undef
   f7      -- 1,undef       -> increment by 1,undef -> 2,undef
   f8      -- DBL_MAX,def   -> unchanged            -> DBL_MAX,def
   f9      -- DBL_MAX,undef -> unchanged            -> DBL_MAX,undef
   f10     -- DBL_MAX,def   -> set to 0,undef       -> 0,undef
   f11     -- DBL_MAX,undef -> set to 0,def         -> 0,def
   f12     -- 0,def         -> set to DBL_MAX,def   -> DBL_MAX,def
   f13     -- 0,undef       -> set to DBL_MAX,undef -> DBL_MAX,undef
   f14     -- 0,def         -> decrement by 0,def   -> 0,def
   f15     -- 0,undef       -> decrement by 0,def   -> 0,undef
   f16     -- 0,def         -> decrement by 0,undef -> 0,undef
   f17     -- 0,undef       -> decrement by 0,undef -> 0,undef
   f18     -- 0,def         -> decrement by 1,def   -> -1,def
   f19     -- 0,undef       -> decrement by 1,def   -> -1,undef
   f20     -- 0,def         -> decrement by 1,undef -> -1,undef
   f21     -- 0,undef       -> decrement by 1,undef -> -1,undef
   f22-f30 -- 0,def         -> set to 1,undef       -> 1,undef
   f31     -- 1,undef       -> set 0,def            -> 0,def
   fcsr:
    fflags -- 0b10101,def   -> set to 0b01010,undef -> 0b01010,undef
    frm    -- 0b001,undef   -> set to 0b100,def     -> 0b100,def
 */

#include <assert.h>
#include <float.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/syscall.h>
#include <sys/ucontext.h>
#include <unistd.h>
#include <valgrind.h>

typedef union {
   unsigned long u64;
   double        f64;
} uf64;

static ucontext_t    uc;
static unsigned long x0, x1;
static unsigned long dbl_max, dbl_p1, dbl_m1, dbl_p2;

static void sighandler(int sig, siginfo_t* sip, void* arg)
{
   ucontext_t* ucp = (ucontext_t*)arg;

   uc = *ucp;

   /* Reset fcsr so its undefinedness doesn't affect the following calculations.
    */
   __asm__ __volatile__("fscsr zero");

#define FPREG_MOD(fpreg, op, mod)                                              \
   do {                                                                        \
      uf64 t1, t2;                                                             \
      t1.u64 = fpreg;                                                          \
      t2.u64 = mod;                                                            \
      t1.f64 op t2.f64;                                                        \
      fpreg = t1.u64;                                                          \
   } while (0)
   FPREG_MOD(ucp->uc_mcontext.__fpregs.__d.__f[2], =, x0);
   FPREG_MOD(ucp->uc_mcontext.__fpregs.__d.__f[3], =, 0);
   FPREG_MOD(ucp->uc_mcontext.__fpregs.__d.__f[4], +=, dbl_p1);
   FPREG_MOD(ucp->uc_mcontext.__fpregs.__d.__f[5], +=, dbl_p1);
   FPREG_MOD(ucp->uc_mcontext.__fpregs.__d.__f[6], +=, dbl_p1 + x1);
   FPREG_MOD(ucp->uc_mcontext.__fpregs.__d.__f[7], +=, dbl_p1 + x1);
   FPREG_MOD(ucp->uc_mcontext.__fpregs.__d.__f[10], =, x0);
   FPREG_MOD(ucp->uc_mcontext.__fpregs.__d.__f[11], =, 0);
   FPREG_MOD(ucp->uc_mcontext.__fpregs.__d.__f[12], =, dbl_max);
   FPREG_MOD(ucp->uc_mcontext.__fpregs.__d.__f[13], =, dbl_max + x0);
   FPREG_MOD(ucp->uc_mcontext.__fpregs.__d.__f[14], -=, 0);
   FPREG_MOD(ucp->uc_mcontext.__fpregs.__d.__f[15], -=, 0);
   FPREG_MOD(ucp->uc_mcontext.__fpregs.__d.__f[16], -=, x0);
   FPREG_MOD(ucp->uc_mcontext.__fpregs.__d.__f[17], -=, x0);
   FPREG_MOD(ucp->uc_mcontext.__fpregs.__d.__f[18], -=, dbl_p1);
   FPREG_MOD(ucp->uc_mcontext.__fpregs.__d.__f[19], -=, dbl_p1);
   FPREG_MOD(ucp->uc_mcontext.__fpregs.__d.__f[20], -=, dbl_p1 + x1);
   FPREG_MOD(ucp->uc_mcontext.__fpregs.__d.__f[21], -=, dbl_p1 + x1);
   FPREG_MOD(ucp->uc_mcontext.__fpregs.__d.__f[22], =, dbl_p1 + x1);
   FPREG_MOD(ucp->uc_mcontext.__fpregs.__d.__f[23], =, dbl_p1 + x1);
   FPREG_MOD(ucp->uc_mcontext.__fpregs.__d.__f[24], =, dbl_p1 + x1);
   FPREG_MOD(ucp->uc_mcontext.__fpregs.__d.__f[25], =, dbl_p1 + x1);
   FPREG_MOD(ucp->uc_mcontext.__fpregs.__d.__f[26], =, dbl_p1 + x1);
   FPREG_MOD(ucp->uc_mcontext.__fpregs.__d.__f[27], =, dbl_p1 + x1);
   FPREG_MOD(ucp->uc_mcontext.__fpregs.__d.__f[28], =, dbl_p1 + x1);
   FPREG_MOD(ucp->uc_mcontext.__fpregs.__d.__f[29], =, dbl_p1 + x1);
   FPREG_MOD(ucp->uc_mcontext.__fpregs.__d.__f[30], =, dbl_p1 + x1);
   FPREG_MOD(ucp->uc_mcontext.__fpregs.__d.__f[31], =, 0);
#undef FPREG_MOD

   ucp->uc_mcontext.__fpregs.__d.__fcsr =
      0b100 << 5 | ((0b01010 | x0) & 0b11111);
}

int main(void)
{
   /* Uninitialised, but we know px0[0] is 0x0. */
   unsigned long* px0 = malloc(sizeof(*px0));
   x0                 = px0[0];

   /* Uninitialised, but we know px1[0] is 0x0. */
   unsigned long* px1 = malloc(sizeof(*px1));
   x1                 = px1[0];

   uf64 tmp;
   tmp.f64 = DBL_MAX;
   dbl_max = tmp.u64;
   tmp.f64 = 1.0;
   dbl_p1  = tmp.u64;
   tmp.f64 = -1.0;
   dbl_m1  = tmp.u64;
   tmp.f64 = 2.0;
   dbl_p2  = tmp.u64;

   struct sigaction sa = {};
   sa.sa_sigaction     = sighandler;
   if (sigaction(SIGUSR1, &sa, NULL)) {
      perror("sigaction");
      return 1;
   }

   unsigned long regs_in[33] = {
      0,
      x0,
      0,
      x0,
      dbl_p1,
      dbl_p1 + x1,
      dbl_p1,
      dbl_p1 + x1,
      dbl_max,
      dbl_max + x0,
      dbl_max,
      dbl_max + x0,
      0,
      x0,
      0,
      x0,
      0,
      x0,
      0,
      x0,
      0,
      x0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      0,
      dbl_p1 + x1,
      ((0b001 | x0) & 0b111) << 5 | 0b10101,
   };
   unsigned long regs_out[33] = {};

   pid_t                   pid          = getpid();
   register unsigned long* t0 asm("t0") = regs_in;
   register unsigned long* t1 asm("t1") = regs_out;
   register unsigned long  a7 asm("a7") = SYS_kill;
   register unsigned long  a0 asm("a0") = pid;
   register unsigned long  a1 asm("a1") = SIGUSR1;
   __asm__ __volatile__(
      /* Spill all test registers, keep the 16-byte sp alignment. */
      "add sp, sp, -272\n\t"
      "fsd f0, 0(sp)\n\t"
      "fsd f1, 8(sp)\n\t"
      "fsd f2, 16(sp)\n\t"
      "fsd f3, 24(sp)\n\t"
      "fsd f4, 32(sp)\n\t"
      "fsd f5, 40(sp)\n\t"
      "fsd f6, 48(sp)\n\t"
      "fsd f7, 56(sp)\n\t"
      "fsd f8, 64(sp)\n\t"
      "fsd f9, 72(sp)\n\t"
      "fsd f10, 80(sp)\n\t"
      "fsd f11, 88(sp)\n\t"
      "fsd f12, 96(sp)\n\t"
      "fsd f13, 104(sp)\n\t"
      "fsd f14, 112(sp)\n\t"
      "fsd f15, 120(sp)\n\t"
      "fsd f16, 128(sp)\n\t"
      "fsd f17, 136(sp)\n\t"
      "fsd f18, 144(sp)\n\t"
      "fsd f19, 152(sp)\n\t"
      "fsd f20, 160(sp)\n\t"
      "fsd f21, 168(sp)\n\t"
      "fsd f22, 176(sp)\n\t"
      "fsd f23, 184(sp)\n\t"
      "fsd f24, 192(sp)\n\t"
      "fsd f25, 200(sp)\n\t"
      "fsd f26, 208(sp)\n\t"
      "fsd f27, 216(sp)\n\t"
      "fsd f28, 224(sp)\n\t"
      "fsd f29, 232(sp)\n\t"
      "fsd f30, 240(sp)\n\t"
      "fsd f31, 248(sp)\n\t"
      "frcsr t2\n\t"
      "sd t2, 256(sp)\n\t"

      /* Set values in the test registers. */
      "fld f0, 0(%[in])\n\t"
      "fld f1, 8(%[in])\n\t"
      "fld f2, 16(%[in])\n\t"
      "fld f3, 24(%[in])\n\t"
      "fld f4, 32(%[in])\n\t"
      "fld f5, 40(%[in])\n\t"
      "fld f6, 48(%[in])\n\t"
      "fld f7, 56(%[in])\n\t"
      "fld f8, 64(%[in])\n\t"
      "fld f9, 72(%[in])\n\t"
      "fld f10, 80(%[in])\n\t"
      "fld f11, 88(%[in])\n\t"
      "fld f12, 96(%[in])\n\t"
      "fld f13, 104(%[in])\n\t"
      "fld f14, 112(%[in])\n\t"
      "fld f15, 120(%[in])\n\t"
      "fld f16, 128(%[in])\n\t"
      "fld f17, 136(%[in])\n\t"
      "fld f18, 144(%[in])\n\t"
      "fld f19, 152(%[in])\n\t"
      "fld f20, 160(%[in])\n\t"
      "fld f21, 168(%[in])\n\t"
      "fld f22, 176(%[in])\n\t"
      "fld f23, 184(%[in])\n\t"
      "fld f24, 192(%[in])\n\t"
      "fld f25, 200(%[in])\n\t"
      "fld f26, 208(%[in])\n\t"
      "fld f27, 216(%[in])\n\t"
      "fld f28, 224(%[in])\n\t"
      "fld f29, 232(%[in])\n\t"
      "fld f30, 240(%[in])\n\t"
      "fld f31, 248(%[in])\n\t"
      "ld t2, 256(%[in])\n\t"
      "fscsr t2\n\t"

      /* Trigger the signal handler. */
      "ecall\n\t"

      /* Store updated values in the test registers. */
      "fsd f0, 0(%[out])\n\t"
      "fsd f1, 8(%[out])\n\t"
      "fsd f2, 16(%[out])\n\t"
      "fsd f3, 24(%[out])\n\t"
      "fsd f4, 32(%[out])\n\t"
      "fsd f5, 40(%[out])\n\t"
      "fsd f6, 48(%[out])\n\t"
      "fsd f7, 56(%[out])\n\t"
      "fsd f8, 64(%[out])\n\t"
      "fsd f9, 72(%[out])\n\t"
      "fsd f10, 80(%[out])\n\t"
      "fsd f11, 88(%[out])\n\t"
      "fsd f12, 96(%[out])\n\t"
      "fsd f13, 104(%[out])\n\t"
      "fsd f14, 112(%[out])\n\t"
      "fsd f15, 120(%[out])\n\t"
      "fsd f16, 128(%[out])\n\t"
      "fsd f17, 136(%[out])\n\t"
      "fsd f18, 144(%[out])\n\t"
      "fsd f19, 152(%[out])\n\t"
      "fsd f20, 160(%[out])\n\t"
      "fsd f21, 168(%[out])\n\t"
      "fsd f22, 176(%[out])\n\t"
      "fsd f23, 184(%[out])\n\t"
      "fsd f24, 192(%[out])\n\t"
      "fsd f25, 200(%[out])\n\t"
      "fsd f26, 208(%[out])\n\t"
      "fsd f27, 216(%[out])\n\t"
      "fsd f28, 224(%[out])\n\t"
      "fsd f29, 232(%[out])\n\t"
      "fsd f30, 240(%[out])\n\t"
      "fsd f31, 248(%[out])\n\t"
      "frcsr t2\n\t"
      "sd t2, 256(%[out])\n\t"

      /* Restore their original values. */
      "fld f0, 0(sp)\n\t"
      "fld f1, 8(sp)\n\t"
      "fld f2, 16(sp)\n\t"
      "fld f3, 24(sp)\n\t"
      "fld f4, 32(sp)\n\t"
      "fld f5, 40(sp)\n\t"
      "fld f6, 48(sp)\n\t"
      "fld f7, 56(sp)\n\t"
      "fld f8, 64(sp)\n\t"
      "fld f9, 72(sp)\n\t"
      "fld f10, 80(sp)\n\t"
      "fld f11, 88(sp)\n\t"
      "fld f12, 96(sp)\n\t"
      "fld f13, 104(sp)\n\t"
      "fld f14, 112(sp)\n\t"
      "fld f15, 120(sp)\n\t"
      "fld f16, 128(sp)\n\t"
      "fld f17, 136(sp)\n\t"
      "fld f18, 144(sp)\n\t"
      "fld f19, 152(sp)\n\t"
      "fld f20, 160(sp)\n\t"
      "fld f21, 168(sp)\n\t"
      "fld f22, 176(sp)\n\t"
      "fld f23, 184(sp)\n\t"
      "fld f24, 192(sp)\n\t"
      "fld f25, 200(sp)\n\t"
      "fld f26, 208(sp)\n\t"
      "fld f27, 216(sp)\n\t"
      "fld f28, 224(sp)\n\t"
      "fld f29, 232(sp)\n\t"
      "fld f30, 240(sp)\n\t"
      "fld f31, 248(sp)\n\t"
      "ld t2, 256(sp)\n\t"
      "fscsr t2\n\t"
      "add sp, sp, 272\n\t"
      :
      : [in] "r"(t0), [out] "r"(t1), "r"(a7), "r"(a0), "r"(a1)
      : "t2", "memory");

   printf("Values before the signal:\n");
   VALGRIND_DISABLE_ERROR_REPORTING;
   printf("   f0=%#lx\n", regs_in[0]);
   printf("   f1=%#lx\n", regs_in[1]);
   printf("   f2=%#lx\n", regs_in[2]);
   printf("   f3=%#lx\n", regs_in[3]);
   printf("   f4=%#lx\n", regs_in[4]);
   printf("   f5=%#lx\n", regs_in[5]);
   printf("   f6=%#lx\n", regs_in[6]);
   printf("   f7=%#lx\n", regs_in[7]);
   printf("   f8=%#lx\n", regs_in[8]);
   printf("   f9=%#lx\n", regs_in[9]);
   printf("  f10=%#lx\n", regs_in[10]);
   printf("  f11=%#lx\n", regs_in[11]);
   printf("  f12=%#lx\n", regs_in[12]);
   printf("  f13=%#lx\n", regs_in[13]);
   printf("  f14=%#lx\n", regs_in[14]);
   printf("  f15=%#lx\n", regs_in[15]);
   printf("  f16=%#lx\n", regs_in[16]);
   printf("  f17=%#lx\n", regs_in[17]);
   printf("  f18=%#lx\n", regs_in[18]);
   printf("  f19=%#lx\n", regs_in[19]);
   printf("  f20=%#lx\n", regs_in[20]);
   printf("  f21=%#lx\n", regs_in[21]);
   printf("  f22=%#lx\n", regs_in[22]);
   printf("  f23=%#lx\n", regs_in[23]);
   printf("  f24=%#lx\n", regs_in[24]);
   printf("  f25=%#lx\n", regs_in[25]);
   printf("  f26=%#lx\n", regs_in[26]);
   printf("  f27=%#lx\n", regs_in[27]);
   printf("  f28=%#lx\n", regs_in[28]);
   printf("  f29=%#lx\n", regs_in[29]);
   printf("  f30=%#lx\n", regs_in[30]);
   printf("  f31=%#lx\n", regs_in[31]);
   printf(" fcsr=%#lx\n", regs_in[32]);
   VALGRIND_ENABLE_ERROR_REPORTING;
   /* Check which registers contain uninitialized values. */
   assert(regs_in[0] == 0);
   assert(regs_in[1] == 0);
   assert(regs_in[2] == 0);
   assert(regs_in[3] == 0);
   assert(regs_in[4] == dbl_p1);
   assert(regs_in[5] == dbl_p1);
   assert(regs_in[6] == dbl_p1);
   assert(regs_in[7] == dbl_p1);
   assert(regs_in[8] == dbl_max);
   assert(regs_in[9] == dbl_max);
   assert(regs_in[10] == dbl_max);
   assert(regs_in[11] == dbl_max);
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
   assert(regs_in[22] == 0);
   assert(regs_in[23] == 0);
   assert(regs_in[24] == 0);
   assert(regs_in[25] == 0);
   assert(regs_in[26] == 0);
   assert(regs_in[27] == 0);
   assert(regs_in[28] == 0);
   assert(regs_in[29] == 0);
   assert(regs_in[30] == 0);
   assert(regs_in[31] == dbl_p1);
   assert(((regs_in[32] >> 0) & 0b11111) == 0b10101);
   assert(((regs_in[32] >> 5) & 0b111) == 0b001);

   printf("Values in the signal handler:\n");
   VALGRIND_DISABLE_ERROR_REPORTING;
   printf("   f0=%#llx\n", uc.uc_mcontext.__fpregs.__d.__f[0]);
   printf("   f1=%#llx\n", uc.uc_mcontext.__fpregs.__d.__f[1]);
   printf("   f2=%#llx\n", uc.uc_mcontext.__fpregs.__d.__f[2]);
   printf("   f3=%#llx\n", uc.uc_mcontext.__fpregs.__d.__f[3]);
   printf("   f4=%#llx\n", uc.uc_mcontext.__fpregs.__d.__f[4]);
   printf("   f5=%#llx\n", uc.uc_mcontext.__fpregs.__d.__f[5]);
   printf("   f6=%#llx\n", uc.uc_mcontext.__fpregs.__d.__f[6]);
   printf("   f7=%#llx\n", uc.uc_mcontext.__fpregs.__d.__f[7]);
   printf("   f8=%#llx\n", uc.uc_mcontext.__fpregs.__d.__f[8]);
   printf("   f9=%#llx\n", uc.uc_mcontext.__fpregs.__d.__f[9]);
   printf("  f10=%#llx\n", uc.uc_mcontext.__fpregs.__d.__f[10]);
   printf("  f11=%#llx\n", uc.uc_mcontext.__fpregs.__d.__f[11]);
   printf("  f12=%#llx\n", uc.uc_mcontext.__fpregs.__d.__f[12]);
   printf("  f13=%#llx\n", uc.uc_mcontext.__fpregs.__d.__f[13]);
   printf("  f14=%#llx\n", uc.uc_mcontext.__fpregs.__d.__f[14]);
   printf("  f15=%#llx\n", uc.uc_mcontext.__fpregs.__d.__f[15]);
   printf("  f16=%#llx\n", uc.uc_mcontext.__fpregs.__d.__f[16]);
   printf("  f17=%#llx\n", uc.uc_mcontext.__fpregs.__d.__f[17]);
   printf("  f18=%#llx\n", uc.uc_mcontext.__fpregs.__d.__f[18]);
   printf("  f19=%#llx\n", uc.uc_mcontext.__fpregs.__d.__f[19]);
   printf("  f20=%#llx\n", uc.uc_mcontext.__fpregs.__d.__f[20]);
   printf("  f21=%#llx\n", uc.uc_mcontext.__fpregs.__d.__f[21]);
   printf("  f22=%#llx\n", uc.uc_mcontext.__fpregs.__d.__f[22]);
   printf("  f23=%#llx\n", uc.uc_mcontext.__fpregs.__d.__f[23]);
   printf("  f24=%#llx\n", uc.uc_mcontext.__fpregs.__d.__f[24]);
   printf("  f25=%#llx\n", uc.uc_mcontext.__fpregs.__d.__f[25]);
   printf("  f26=%#llx\n", uc.uc_mcontext.__fpregs.__d.__f[26]);
   printf("  f27=%#llx\n", uc.uc_mcontext.__fpregs.__d.__f[27]);
   printf("  f28=%#llx\n", uc.uc_mcontext.__fpregs.__d.__f[28]);
   printf("  f29=%#llx\n", uc.uc_mcontext.__fpregs.__d.__f[29]);
   printf("  f30=%#llx\n", uc.uc_mcontext.__fpregs.__d.__f[30]);
   printf("  f31=%#llx\n", uc.uc_mcontext.__fpregs.__d.__f[31]);
   printf(" fcsr=%#x\n", uc.uc_mcontext.__fpregs.__d.__fcsr);
   VALGRIND_ENABLE_ERROR_REPORTING;
   assert(uc.uc_mcontext.__fpregs.__d.__f[0] == 0);
   assert(uc.uc_mcontext.__fpregs.__d.__f[1] == 0);
   assert(uc.uc_mcontext.__fpregs.__d.__f[2] == 0);
   assert(uc.uc_mcontext.__fpregs.__d.__f[3] == 0);
   assert(uc.uc_mcontext.__fpregs.__d.__f[4] == dbl_p1);
   assert(uc.uc_mcontext.__fpregs.__d.__f[5] == dbl_p1);
   assert(uc.uc_mcontext.__fpregs.__d.__f[6] == dbl_p1);
   assert(uc.uc_mcontext.__fpregs.__d.__f[7] == dbl_p1);
   assert(uc.uc_mcontext.__fpregs.__d.__f[8] == dbl_max);
   assert(uc.uc_mcontext.__fpregs.__d.__f[9] == dbl_max);
   assert(uc.uc_mcontext.__fpregs.__d.__f[10] == dbl_max);
   assert(uc.uc_mcontext.__fpregs.__d.__f[11] == dbl_max);
   assert(uc.uc_mcontext.__fpregs.__d.__f[12] == 0);
   assert(uc.uc_mcontext.__fpregs.__d.__f[13] == 0);
   assert(uc.uc_mcontext.__fpregs.__d.__f[14] == 0);
   assert(uc.uc_mcontext.__fpregs.__d.__f[15] == 0);
   assert(uc.uc_mcontext.__fpregs.__d.__f[16] == 0);
   assert(uc.uc_mcontext.__fpregs.__d.__f[17] == 0);
   assert(uc.uc_mcontext.__fpregs.__d.__f[18] == 0);
   assert(uc.uc_mcontext.__fpregs.__d.__f[19] == 0);
   assert(uc.uc_mcontext.__fpregs.__d.__f[20] == 0);
   assert(uc.uc_mcontext.__fpregs.__d.__f[21] == 0);
   assert(uc.uc_mcontext.__fpregs.__d.__f[22] == 0);
   assert(uc.uc_mcontext.__fpregs.__d.__f[23] == 0);
   assert(uc.uc_mcontext.__fpregs.__d.__f[24] == 0);
   assert(uc.uc_mcontext.__fpregs.__d.__f[25] == 0);
   assert(uc.uc_mcontext.__fpregs.__d.__f[26] == 0);
   assert(uc.uc_mcontext.__fpregs.__d.__f[27] == 0);
   assert(uc.uc_mcontext.__fpregs.__d.__f[28] == 0);
   assert(uc.uc_mcontext.__fpregs.__d.__f[29] == 0);
   assert(uc.uc_mcontext.__fpregs.__d.__f[30] == 0);
   assert(uc.uc_mcontext.__fpregs.__d.__f[31] == dbl_p1);
   assert(((uc.uc_mcontext.__fpregs.__d.__fcsr >> 0) & 0b11111) == 0b10101);
   assert(((uc.uc_mcontext.__fpregs.__d.__fcsr >> 5) & 0b111) == 0b001);

   printf("Values after return from the signal handler:\n");
   VALGRIND_DISABLE_ERROR_REPORTING;
   printf("   f0=%#lx\n", regs_out[0]);
   printf("   f1=%#lx\n", regs_out[1]);
   printf("   f2=%#lx\n", regs_out[2]);
   printf("   f3=%#lx\n", regs_out[3]);
   printf("   f4=%#lx\n", regs_out[4]);
   printf("   f5=%#lx\n", regs_out[5]);
   printf("   f6=%#lx\n", regs_out[6]);
   printf("   f7=%#lx\n", regs_out[7]);
   printf("   f8=%#lx\n", regs_out[8]);
   printf("   f9=%#lx\n", regs_out[9]);
   printf("  f10=%#lx\n", regs_out[10]);
   printf("  f11=%#lx\n", regs_out[11]);
   printf("  f12=%#lx\n", regs_out[12]);
   printf("  f13=%#lx\n", regs_out[13]);
   printf("  f14=%#lx\n", regs_out[14]);
   printf("  f15=%#lx\n", regs_out[15]);
   printf("  f16=%#lx\n", regs_out[16]);
   printf("  f17=%#lx\n", regs_out[17]);
   printf("  f18=%#lx\n", regs_out[18]);
   printf("  f19=%#lx\n", regs_out[19]);
   printf("  f20=%#lx\n", regs_out[20]);
   printf("  f21=%#lx\n", regs_out[21]);
   printf("  f22=%#lx\n", regs_out[22]);
   printf("  f23=%#lx\n", regs_out[23]);
   printf("  f24=%#lx\n", regs_out[24]);
   printf("  f25=%#lx\n", regs_out[25]);
   printf("  f26=%#lx\n", regs_out[26]);
   printf("  f27=%#lx\n", regs_out[27]);
   printf("  f28=%#lx\n", regs_out[28]);
   printf("  f29=%#lx\n", regs_out[29]);
   printf("  f30=%#lx\n", regs_out[30]);
   printf("  f31=%#lx\n", regs_out[31]);
   printf(" fcsr=%#lx\n", regs_out[32]);
   VALGRIND_ENABLE_ERROR_REPORTING;
   assert(regs_out[0] == 0);
   assert(regs_out[1] == 0);
   assert(regs_out[2] == 0);
   assert(regs_out[3] == 0);
   assert(regs_out[4] == dbl_p2);
   assert(regs_out[5] == dbl_p2);
   assert(regs_out[6] == dbl_p2);
   assert(regs_out[7] == dbl_p2);
   assert(regs_out[8] == dbl_max);
   assert(regs_out[9] == dbl_max);
   assert(regs_out[10] == 0);
   assert(regs_out[11] == 0);
   assert(regs_out[12] == dbl_max);
   assert(regs_out[13] == dbl_max);
   assert(regs_out[14] == 0);
   assert(regs_out[15] == 0);
   assert(regs_out[16] == 0);
   assert(regs_out[17] == 0);
   assert(regs_out[18] == dbl_m1);
   assert(regs_out[19] == dbl_m1);
   assert(regs_out[20] == dbl_m1);
   assert(regs_out[21] == dbl_m1);
   assert(regs_out[22] == dbl_p1);
   assert(regs_out[23] == dbl_p1);
   assert(regs_out[24] == dbl_p1);
   assert(regs_out[25] == dbl_p1);
   assert(regs_out[26] == dbl_p1);
   assert(regs_out[27] == dbl_p1);
   assert(regs_out[28] == dbl_p1);
   assert(regs_out[29] == dbl_p1);
   assert(regs_out[30] == dbl_p1);
   assert(regs_out[31] == 0);
   assert(((regs_out[32] >> 0) & 0b11111) == 0b01010);
   assert(((regs_out[32] >> 5) & 0b111) == 0b100);

   free(px0);
   free(px1);

   return 0;
}
