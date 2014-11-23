
/* Can be compiled both as ARM or Thumb using
   gcc -Wall -g -O0 -mcpu=cortex-a8 -mfpu=neon -mfloat-abi=softfp -m{arm,thumb} -o vfp vfp.c
*/

#include <stdio.h>
#include <string.h>
#include <math.h>

static unsigned int f2u(float x) {
    union {
        float f;
        unsigned int u;
    } cvt;
    cvt.f = x;
    return cvt.u;
}

static unsigned int f2u0(double x) {
    union {
        double f;
        unsigned int u[2];
    } cvt;
    cvt.f = x;
    return cvt.u[0];
}

static unsigned int f2u1(double x) {
    union {
        double f;
        unsigned int u[2];
    } cvt;
    cvt.f = x;
    return cvt.u[1];
}

/* test macros to generate and output the result of a single instruction */

#define TESTINSN_bin_f64(instruction, QD, QM, QMtype, QMval0, QMval1, QN, QNtype, QNval0, QNval1) \
{ \
  unsigned int out[2] = { 0x55555555, 0x55555555 }; \
  unsigned int block[2]; \
  block[0] = block[1] = 0x3f800000; \
  \
  __asm__ volatile( \
      "vldr " #QD ", [%5]\n\t" \
      "vmov " #QM ", %1, %2 \n\t" \
      "vmov " #QN ", %3, %4 \n\t" \
      instruction "\n\t" \
      "vstmia %0, {" #QD "}\n\t" \
      : \
      : "r" (&out[0]), "r" (QMval0), "r" (QMval1), "r" (QNval0), "r" (QNval1), "r"(&block[0]) \
      : #QD, #QM, #QN, "memory" \
      ); \
  printf("%-24s :: Qd 0x%08x 0x%08x  Qm 0x%08x %08x" \
      "  Qn 0x%08x %08x\n", \
      instruction, out[1], out[0], QMval1, QMval0, QNval1, QNval0); \
}

#define TESTINSN_bin_f32(instruction, SD, SM, SMtype, SMval, SN, SNtype, SNval) \
{ \
  unsigned int out[2] = { 0x55555555, 0x55555555 }; \
  \
  __asm__ volatile( \
      "vmov.f32 " #SM ", %1\n\t" \
      "vmov.f32 " #SN ", %2\n\t" \
      "vmov.f32 " #SD ", %3\n\t" \
      instruction "\n\t" \
      "vstmia %0, {" #SD "}\n\t" \
      : \
      : "r" (&out[0]), "r" (SMval), "r" (SNval), "r" (0xaaaaaaaa) \
      : #SD, #SM, #SN, "memory" \
      ); \
  printf("%-24s :: Qd 0x%08x 0x%08x  Sm (" #SMtype ")0x%08x" \
      "  Sn (" #SNtype ")0x%08x\n", \
      instruction, out[1], out[0], SMval, SNval); \
}

int main(int argc, char **argv)
{
    printf("---- VFMA (fp, VFPv4) ----\n");
    TESTINSN_bin_f64("vfma.f64 d0,  d11, d12", d0,  d11, i32, f2u0(-INFINITY), f2u1(-INFINITY), d12, i32, f2u0(NAN), f2u1(NAN));
    TESTINSN_bin_f64("vfma.f64 d7,  d1,  d6",  d7,  d1,  i32, f2u0(INFINITY), f2u1(INFINITY), d6, i32, f2u0(NAN), f2u1(NAN));
    TESTINSN_bin_f64("vfma.f64 d0,  d5,  d2",  d0,  d5,  i32, f2u0(NAN), f2u1(NAN), d2, i32, f2u0(-1.0), f2u1(-1.0));
    TESTINSN_bin_f64("vfma.f64 d10, d13, d15", d10, d13, i32, f2u0(NAN), f2u1(NAN), d15, i32, f2u0(0.0), f2u1(0.0));
    TESTINSN_bin_f64("vfma.f64 d10, d13, d15", d10, d13, i32, f2u0(NAN), f2u1(NAN), d15, i32, f2u0(NAN), f2u1(NAN));
    TESTINSN_bin_f64("vfma.f64 d20, d25, d22", d20, d25, i32, f2u0(23.04), f2u1(23.04), d22, i32, f2u0(-45.5687), f2u1(-45.5687));
    TESTINSN_bin_f64("vfma.f64 d23, d24, d25", d23, d24, i32, f2u0(-347856.475), f2u1(-347856.475), d25, i32, f2u0(1346), f2u1(1346));
    TESTINSN_bin_f64("vfma.f64 d20, d31, d12", d20, d31, i32, f2u0(48755), f2u1(48755), d12, i32, f2u0(-45786.476), f2u1(-45786.476));
    TESTINSN_bin_f64("vfma.f64 d19, d25, d27", d19, d25, i32, f2u0(95867.76), f2u1(95867.76), d27, i32, f2u0(17065), f2u1(17065));
    TESTINSN_bin_f64("vfma.f64 d30, d15, d2",  d30, d15, i32, f2u0(-45667.24), f2u1(-45667.24), d2, i32, f2u0(-248562.76), f2u1(-248562.76));
    TESTINSN_bin_f64("vfma.f64 d23, d24, d5",  d23, d24, i32, f2u0(24), f2u1(24), d5, i32, f2u0(1346), f2u1(1346));
    TESTINSN_bin_f64("vfma.f64 d10, d11, d2",  d10, d11, i32, f2u0(48755), f2u1(48755), d2, i32, f2u0(1089), f2u1(1089));
    TESTINSN_bin_f64("vfma.f64 d29, d15, d7",  d29, d15, i32, f2u0(214), f2u1(214), d7, i32, f2u0(1752065), f2u1(1752065));
    TESTINSN_bin_f64("vfma.f64 d30, d11, d12", d30, d11, i32, f2u0(356047.56), f2u1(356047.56), d12, i32, f2u0(5867.009), f2u1(5867.009));
    TESTINSN_bin_f64("vfma.f64 d27, d21, d6",  d27, d21, i32, f2u0(34.00046), f2u1(34.00046), d6, i32, f2u0(0.0024575), f2u1(0.0024575));
    TESTINSN_bin_f64("vfma.f64 d30, d31, d2",  d30, d31, i32, f2u0(2754), f2u1(2754), d2, i32, f2u0(107), f2u1(107));
    TESTINSN_bin_f64("vfma.f64 d13, d24, d5",  d13, d24, i32, f2u0(874), f2u1(874), d5, i32, f2u0(1384.6), f2u1(1384.6));
    TESTINSN_bin_f64("vfma.f64 d10, d11, d2",  d10, d11, i32, f2u0(487.587), f2u1(487.587), d2, i32, f2u0(109), f2u1(109));
    TESTINSN_bin_f64("vfma.f64 d29, d25, d7",  d29, d25, i32, f2u0(-INFINITY), f2u1(-INFINITY), d7, i32, f2u0(1752), f2u1(1752));
    TESTINSN_bin_f64("vfma.f64 d0,  d11, d12", d0,  d11, i32, f2u0(INFINITY), f2u1(INFINITY), d12, i32, f2u0(-5786.47), f2u1(-5786.47));
    TESTINSN_bin_f64("vfma.f64 d27, d21, d16", d27, d21, i32, f2u0(456.2489562), f2u1(456.2489562), d16, i32, f2u0(-7.2945676), f2u1(-7.2945676));
    TESTINSN_bin_f64("vfma.f64 d0,  d5,  d2",  d0,  d5,  i32, f2u0(INFINITY), f2u1(INFINITY), d2, i32, f2u0(-INFINITY), f2u1(-INFINITY));
    TESTINSN_bin_f64("vfma.f64 d20, d13, d15", d20, d13, i32, f2u0(-INFINITY), f2u1(-INFINITY), d15, i32, f2u0(0.0), f2u1(0.0));
    TESTINSN_bin_f64("vfma.f64 d10, d23, d15", d10, d23, i32, f2u0(INFINITY), f2u1(INFINITY), d15, i32, f2u0(0.0), f2u1(0.0));
    TESTINSN_bin_f32("vfma.f32 s0,  s11, s12", s0,  s11, i32, f2u(-INFINITY), s12, i32, f2u(NAN));
    TESTINSN_bin_f32("vfma.f32 s7,  s1,  s6",  s7,  s1,  i32, f2u(INFINITY), s6, i32, f2u(NAN));
    TESTINSN_bin_f32("vfma.f32 s0,  s5,  s2",  s0,  s5,  i32, f2u(NAN), s2, i32, f2u(-1.0));
    TESTINSN_bin_f32("vfma.f32 s10, s13, s15", s10, s13, i32, f2u(NAN), s15, i32, f2u(0.0));
    TESTINSN_bin_f32("vfma.f32 s10, s13, s15", s10, s13, i32, f2u(NAN), s15, i32, f2u(NAN));
    TESTINSN_bin_f32("vfma.f32 s20, s25, s22", s20, s25, i32, f2u(23.04), s22, i32, f2u(-45.5687));
    TESTINSN_bin_f32("vfma.f32 s23, s24, s25", s23, s24, i32, f2u(-347856.475), s25, i32, f2u(1346));
    TESTINSN_bin_f32("vfma.f32 s20, s31, s12", s20, s31, i32, f2u(48755), s12, i32, f2u(-45786.476));
    TESTINSN_bin_f32("vfma.f32 s19, s25, s27", s19, s25, i32, f2u(95867.76), s27, i32, f2u(17065));
    TESTINSN_bin_f32("vfma.f32 s30, s15, s2",  s30, s15, i32, f2u(-45667.24), s2, i32, f2u(-248562.76));
    TESTINSN_bin_f32("vfma.f32 s23, s24, s5",  s23, s24, i32, f2u(24), s5, i32, f2u(1346));
    TESTINSN_bin_f32("vfma.f32 s10, s11, s2",  s10, s11, i32, f2u(48755), s2, i32, f2u(1089));
    TESTINSN_bin_f32("vfma.f32 s29, s15, s7",  s29, s15, i32, f2u(214), s7, i32, f2u(1752065));
    TESTINSN_bin_f32("vfma.f32 s30, s11, s12", s30, s11, i32, f2u(356047.56), s12, i32, f2u(5867.009));
    TESTINSN_bin_f32("vfma.f32 s27, s21, s6",  s27, s21, i32, f2u(34.00046), s6, i32, f2u(0.0024575));
    TESTINSN_bin_f32("vfma.f32 s30, s31, s2",  s30, s31, i32, f2u(2754), s2, i32, f2u(107));
    TESTINSN_bin_f32("vfma.f32 s13, s24, s5",  s13, s24, i32, f2u(874), s5, i32, f2u(1384.6));
    TESTINSN_bin_f32("vfma.f32 s10, s11, s2",  s10, s11, i32, f2u(487.587), s2, i32, f2u(109));
    TESTINSN_bin_f32("vfma.f32 s29, s25, s7",  s29, s25, i32, f2u(-INFINITY), s7, i32, f2u(1752));
    TESTINSN_bin_f32("vfma.f32 s0,  s11, s12", s0,  s11, i32, f2u(INFINITY), s12, i32, f2u(-5786.47));
    TESTINSN_bin_f32("vfma.f32 s27, s21, s16", s27, s21, i32, f2u(456.2489562), s16, i32, f2u(-7.2945676));
    TESTINSN_bin_f32("vfma.f32 s0,  s5,  s2",  s0,  s5,  i32, f2u(INFINITY), s2, i32, f2u(-INFINITY));
    TESTINSN_bin_f32("vfma.f32 s20, s13, s15", s20, s13, i32, f2u(-INFINITY), s15, i32, f2u(0.0));
    TESTINSN_bin_f32("vfma.f32 s10, s23, s15", s10, s23, i32, f2u(INFINITY), s15, i32, f2u(0.0));

    printf("---- VFMS (fp, VFPv4) ----\n");
    TESTINSN_bin_f64("vfms.f64 d0,  d11, d12", d0,  d11, i32, f2u0(-INFINITY), f2u1(-INFINITY), d12, i32, f2u0(NAN), f2u1(NAN));
    TESTINSN_bin_f64("vfms.f64 d7,  d1,  d6",  d7,  d1,  i32, f2u0(INFINITY), f2u1(INFINITY), d6, i32, f2u0(NAN), f2u1(NAN));
    TESTINSN_bin_f64("vfms.f64 d0,  d5,  d2",  d0,  d5,  i32, f2u0(NAN), f2u1(NAN), d2, i32, f2u0(-1.0), f2u1(-1.0));
    TESTINSN_bin_f64("vfms.f64 d10, d13, d15", d10, d13, i32, f2u0(NAN), f2u1(NAN), d15, i32, f2u0(0.0), f2u1(0.0));
    TESTINSN_bin_f64("vfms.f64 d10, d13, d15", d10, d13, i32, f2u0(NAN), f2u1(NAN), d15, i32, f2u0(NAN), f2u1(NAN));
    TESTINSN_bin_f64("vfms.f64 d20, d25, d22", d20, d25, i32, f2u0(23.04), f2u1(23.04), d22, i32, f2u0(-45.5687), f2u1(-45.5687));
    TESTINSN_bin_f64("vfms.f64 d23, d24, d25", d23, d24, i32, f2u0(-347856.475), f2u1(-347856.475), d25, i32, f2u0(1346), f2u1(1346));
    TESTINSN_bin_f64("vfms.f64 d20, d31, d12", d20, d31, i32, f2u0(48755), f2u1(48755), d12, i32, f2u0(-45786.476), f2u1(-45786.476));
    TESTINSN_bin_f64("vfms.f64 d19, d25, d27", d19, d25, i32, f2u0(95867.76), f2u1(95867.76), d27, i32, f2u0(17065), f2u1(17065));
    TESTINSN_bin_f64("vfms.f64 d30, d15, d2",  d30, d15, i32, f2u0(-45667.24), f2u1(-45667.24), d2, i32, f2u0(-248562.76), f2u1(-248562.76));
    TESTINSN_bin_f64("vfms.f64 d23, d24, d5",  d23, d24, i32, f2u0(24), f2u1(24), d5, i32, f2u0(1346), f2u1(1346));
    TESTINSN_bin_f64("vfms.f64 d10, d11, d2",  d10, d11, i32, f2u0(48755), f2u1(48755), d2, i32, f2u0(1089), f2u1(1089));
    TESTINSN_bin_f64("vfms.f64 d29, d15, d7",  d29, d15, i32, f2u0(214), f2u1(214), d7, i32, f2u0(1752065), f2u1(1752065));
    TESTINSN_bin_f64("vfms.f64 d30, d11, d12", d30, d11, i32, f2u0(356047.56), f2u1(356047.56), d12, i32, f2u0(5867.009), f2u1(5867.009));
    TESTINSN_bin_f64("vfms.f64 d27, d21, d6",  d27, d21, i32, f2u0(34.00046), f2u1(34.00046), d6, i32, f2u0(0.0024575), f2u1(0.0024575));
    TESTINSN_bin_f64("vfms.f64 d30, d31, d2",  d30, d31, i32, f2u0(2754), f2u1(2754), d2, i32, f2u0(107), f2u1(107));
    TESTINSN_bin_f64("vfms.f64 d13, d24, d5",  d13, d24, i32, f2u0(874), f2u1(874), d5, i32, f2u0(1384.6), f2u1(1384.6));
    TESTINSN_bin_f64("vfms.f64 d10, d11, d2",  d10, d11, i32, f2u0(487.587), f2u1(487.587), d2, i32, f2u0(109), f2u1(109));
    TESTINSN_bin_f64("vfms.f64 d29, d25, d7",  d29, d25, i32, f2u0(-INFINITY), f2u1(-INFINITY), d7, i32, f2u0(1752), f2u1(1752));
    TESTINSN_bin_f64("vfms.f64 d0,  d11, d12", d0,  d11, i32, f2u0(INFINITY), f2u1(INFINITY), d12, i32, f2u0(-5786.47), f2u1(-5786.47));
    TESTINSN_bin_f64("vfms.f64 d27, d21, d16", d27, d21, i32, f2u0(456.2489562), f2u1(456.2489562), d16, i32, f2u0(-7.2945676), f2u1(-7.2945676));
    TESTINSN_bin_f64("vfms.f64 d0,  d5,  d2",  d0,  d5,  i32, f2u0(INFINITY), f2u1(INFINITY), d2, i32, f2u0(-INFINITY), f2u1(-INFINITY));
    TESTINSN_bin_f64("vfms.f64 d20, d13, d15", d20, d13, i32, f2u0(-INFINITY), f2u1(-INFINITY), d15, i32, f2u0(0.0), f2u1(0.0));
    TESTINSN_bin_f64("vfms.f64 d10, d23, d15", d10, d23, i32, f2u0(INFINITY), f2u1(INFINITY), d15, i32, f2u0(0.0), f2u1(0.0));
    TESTINSN_bin_f32("vfms.f32 s0,  s11, s12", s0,  s11, i32, f2u(-INFINITY), s12, i32, f2u(NAN));
    TESTINSN_bin_f32("vfms.f32 s7,  s1,  s6",  s7,  s1,  i32, f2u(INFINITY), s6, i32, f2u(NAN));
    TESTINSN_bin_f32("vfms.f32 s0,  s5,  s2",  s0,  s5,  i32, f2u(NAN), s2, i32, f2u(-1.0));
    TESTINSN_bin_f32("vfms.f32 s10, s13, s15", s10, s13, i32, f2u(NAN), s15, i32, f2u(0.0));
    TESTINSN_bin_f32("vfms.f32 s10, s13, s15", s10, s13, i32, f2u(NAN), s15, i32, f2u(NAN));
    TESTINSN_bin_f32("vfms.f32 s20, s25, s22", s20, s25, i32, f2u(23.04), s22, i32, f2u(-45.5687));
    TESTINSN_bin_f32("vfms.f32 s23, s24, s25", s23, s24, i32, f2u(-347856.475), s25, i32, f2u(1346));
    TESTINSN_bin_f32("vfms.f32 s20, s31, s12", s20, s31, i32, f2u(48755), s12, i32, f2u(-45786.476));
    TESTINSN_bin_f32("vfms.f32 s19, s25, s27", s19, s25, i32, f2u(95867.76), s27, i32, f2u(17065));
    TESTINSN_bin_f32("vfms.f32 s30, s15, s2",  s30, s15, i32, f2u(-45667.24), s2, i32, f2u(-248562.76));
    TESTINSN_bin_f32("vfms.f32 s23, s24, s5",  s23, s24, i32, f2u(24), s5, i32, f2u(1346));
    TESTINSN_bin_f32("vfms.f32 s10, s11, s2",  s10, s11, i32, f2u(48755), s2, i32, f2u(1089));
    TESTINSN_bin_f32("vfms.f32 s29, s15, s7",  s29, s15, i32, f2u(214), s7, i32, f2u(1752065));
    TESTINSN_bin_f32("vfms.f32 s30, s11, s12", s30, s11, i32, f2u(356047.56), s12, i32, f2u(5867.009));
    TESTINSN_bin_f32("vfms.f32 s27, s21, s6",  s27, s21, i32, f2u(34.00046), s6, i32, f2u(0.0024575));
    TESTINSN_bin_f32("vfms.f32 s30, s31, s2",  s30, s31, i32, f2u(2754), s2, i32, f2u(107));
    TESTINSN_bin_f32("vfms.f32 s13, s24, s5",  s13, s24, i32, f2u(874), s5, i32, f2u(1384.6));
    TESTINSN_bin_f32("vfms.f32 s10, s11, s2",  s10, s11, i32, f2u(487.587), s2, i32, f2u(109));
    TESTINSN_bin_f32("vfms.f32 s29, s25, s7",  s29, s25, i32, f2u(-INFINITY), s7, i32, f2u(1752));
    TESTINSN_bin_f32("vfms.f32 s0,  s11, s12", s0,  s11, i32, f2u(INFINITY), s12, i32, f2u(-5786.47));
    TESTINSN_bin_f32("vfms.f32 s27, s21, s16", s27, s21, i32, f2u(456.2489562), s16, i32, f2u(-7.2945676));
    TESTINSN_bin_f32("vfms.f32 s0,  s5,  s2",  s0,  s5,  i32, f2u(INFINITY), s2, i32, f2u(-INFINITY));
    TESTINSN_bin_f32("vfms.f32 s20, s13, s15", s20, s13, i32, f2u(-INFINITY), s15, i32, f2u(0.0));
    TESTINSN_bin_f32("vfms.f32 s10, s23, s15", s10, s23, i32, f2u(INFINITY), s15, i32, f2u(0.0));

    printf("---- VFNMA (fp, VFPv4) ----\n");
    TESTINSN_bin_f64("vfnma.f64 d0,  d11, d12", d0,  d11, i32, f2u0(-INFINITY), f2u1(-INFINITY), d12, i32, f2u0(NAN), f2u1(NAN));
    TESTINSN_bin_f64("vfnma.f64 d7,  d1,  d6",  d7,  d1,  i32, f2u0(INFINITY), f2u1(INFINITY), d6, i32, f2u0(NAN), f2u1(NAN));
    TESTINSN_bin_f64("vfnma.f64 d0,  d5,  d2",  d0,  d5,  i32, f2u0(NAN), f2u1(NAN), d2, i32, f2u0(-1.0), f2u1(-1.0));
    TESTINSN_bin_f64("vfnma.f64 d10, d13, d15", d10, d13, i32, f2u0(NAN), f2u1(NAN), d15, i32, f2u0(0.0), f2u1(0.0));
    TESTINSN_bin_f64("vfnma.f64 d10, d13, d15", d10, d13, i32, f2u0(NAN), f2u1(NAN), d15, i32, f2u0(NAN), f2u1(NAN));
    TESTINSN_bin_f64("vfnma.f64 d20, d25, d22", d20, d25, i32, f2u0(23.04), f2u1(23.04), d22, i32, f2u0(-45.5687), f2u1(-45.5687));
    TESTINSN_bin_f64("vfnma.f64 d23, d24, d25", d23, d24, i32, f2u0(-347856.475), f2u1(-347856.475), d25, i32, f2u0(1346), f2u1(1346));
    TESTINSN_bin_f64("vfnma.f64 d20, d31, d12", d20, d31, i32, f2u0(48755), f2u1(48755), d12, i32, f2u0(-45786.476), f2u1(-45786.476));
    TESTINSN_bin_f64("vfnma.f64 d19, d25, d27", d19, d25, i32, f2u0(95867.76), f2u1(95867.76), d27, i32, f2u0(17065), f2u1(17065));
    TESTINSN_bin_f64("vfnma.f64 d30, d15, d2",  d30, d15, i32, f2u0(-45667.24), f2u1(-45667.24), d2, i32, f2u0(-248562.76), f2u1(-248562.76));
    TESTINSN_bin_f64("vfnma.f64 d23, d24, d5",  d23, d24, i32, f2u0(24), f2u1(24), d5, i32, f2u0(1346), f2u1(1346));
    TESTINSN_bin_f64("vfnma.f64 d10, d11, d2",  d10, d11, i32, f2u0(48755), f2u1(48755), d2, i32, f2u0(1089), f2u1(1089));
    TESTINSN_bin_f64("vfnma.f64 d29, d15, d7",  d29, d15, i32, f2u0(214), f2u1(214), d7, i32, f2u0(1752065), f2u1(1752065));
    TESTINSN_bin_f64("vfnma.f64 d30, d11, d12", d30, d11, i32, f2u0(356047.56), f2u1(356047.56), d12, i32, f2u0(5867.009), f2u1(5867.009));
    TESTINSN_bin_f64("vfnma.f64 d27, d21, d6",  d27, d21, i32, f2u0(34.00046), f2u1(34.00046), d6, i32, f2u0(0.0024575), f2u1(0.0024575));
    TESTINSN_bin_f64("vfnma.f64 d30, d31, d2",  d30, d31, i32, f2u0(2754), f2u1(2754), d2, i32, f2u0(107), f2u1(107));
    TESTINSN_bin_f64("vfnma.f64 d13, d24, d5",  d13, d24, i32, f2u0(874), f2u1(874), d5, i32, f2u0(1384.6), f2u1(1384.6));
    TESTINSN_bin_f64("vfnma.f64 d10, d11, d2",  d10, d11, i32, f2u0(487.587), f2u1(487.587), d2, i32, f2u0(109), f2u1(109));
    TESTINSN_bin_f64("vfnma.f64 d29, d25, d7",  d29, d25, i32, f2u0(-INFINITY), f2u1(-INFINITY), d7, i32, f2u0(1752), f2u1(1752));
    TESTINSN_bin_f64("vfnma.f64 d0,  d11, d12", d0,  d11, i32, f2u0(INFINITY), f2u1(INFINITY), d12, i32, f2u0(-5786.47), f2u1(-5786.47));
    TESTINSN_bin_f64("vfnma.f64 d27, d21, d16", d27, d21, i32, f2u0(456.2489562), f2u1(456.2489562), d16, i32, f2u0(-7.2945676), f2u1(-7.2945676));
    TESTINSN_bin_f64("vfnma.f64 d0,  d5,  d2",  d0,  d5,  i32, f2u0(INFINITY), f2u1(INFINITY), d2, i32, f2u0(-INFINITY), f2u1(-INFINITY));
    TESTINSN_bin_f64("vfnma.f64 d20, d13, d15", d20, d13, i32, f2u0(-INFINITY), f2u1(-INFINITY), d15, i32, f2u0(0.0), f2u1(0.0));
    TESTINSN_bin_f64("vfnma.f64 d10, d23, d15", d10, d23, i32, f2u0(INFINITY), f2u1(INFINITY), d15, i32, f2u0(0.0), f2u1(0.0));
    TESTINSN_bin_f32("vfnma.f32 s0,  s11, s12", s0,  s11, i32, f2u(-INFINITY), s12, i32, f2u(NAN));
    TESTINSN_bin_f32("vfnma.f32 s7,  s1,  s6",  s7,  s1,  i32, f2u(INFINITY), s6, i32, f2u(NAN));
    TESTINSN_bin_f32("vfnma.f32 s0,  s5,  s2",  s0,  s5,  i32, f2u(NAN), s2, i32, f2u(-1.0));
    TESTINSN_bin_f32("vfnma.f32 s10, s13, s15", s10, s13, i32, f2u(NAN), s15, i32, f2u(0.0));
    TESTINSN_bin_f32("vfnma.f32 s10, s13, s15", s10, s13, i32, f2u(NAN), s15, i32, f2u(NAN));
    TESTINSN_bin_f32("vfnma.f32 s20, s25, s22", s20, s25, i32, f2u(23.04), s22, i32, f2u(-45.5687));
    TESTINSN_bin_f32("vfnma.f32 s23, s24, s25", s23, s24, i32, f2u(-347856.475), s25, i32, f2u(1346));
    TESTINSN_bin_f32("vfnma.f32 s20, s31, s12", s20, s31, i32, f2u(48755), s12, i32, f2u(-45786.476));
    TESTINSN_bin_f32("vfnma.f32 s19, s25, s27", s19, s25, i32, f2u(95867.76), s27, i32, f2u(17065));
    TESTINSN_bin_f32("vfnma.f32 s30, s15, s2",  s30, s15, i32, f2u(-45667.24), s2, i32, f2u(-248562.76));
    TESTINSN_bin_f32("vfnma.f32 s23, s24, s5",  s23, s24, i32, f2u(24), s5, i32, f2u(1346));
    TESTINSN_bin_f32("vfnma.f32 s10, s11, s2",  s10, s11, i32, f2u(48755), s2, i32, f2u(1089));
    TESTINSN_bin_f32("vfnma.f32 s29, s15, s7",  s29, s15, i32, f2u(214), s7, i32, f2u(1752065));
    TESTINSN_bin_f32("vfnma.f32 s30, s11, s12", s30, s11, i32, f2u(356047.56), s12, i32, f2u(5867.009));
    TESTINSN_bin_f32("vfnma.f32 s27, s21, s6",  s27, s21, i32, f2u(34.00046), s6, i32, f2u(0.0024575));
    TESTINSN_bin_f32("vfnma.f32 s30, s31, s2",  s30, s31, i32, f2u(2754), s2, i32, f2u(107));
    TESTINSN_bin_f32("vfnma.f32 s13, s24, s5",  s13, s24, i32, f2u(874), s5, i32, f2u(1384.6));
    TESTINSN_bin_f32("vfnma.f32 s10, s11, s2",  s10, s11, i32, f2u(487.587), s2, i32, f2u(109));
    TESTINSN_bin_f32("vfnma.f32 s29, s25, s7",  s29, s25, i32, f2u(-INFINITY), s7, i32, f2u(1752));
    TESTINSN_bin_f32("vfnma.f32 s0,  s11, s12", s0,  s11, i32, f2u(INFINITY), s12, i32, f2u(-5786.47));
    TESTINSN_bin_f32("vfnma.f32 s27, s21, s16", s27, s21, i32, f2u(456.2489562), s16, i32, f2u(-7.2945676));
    TESTINSN_bin_f32("vfnma.f32 s0,  s5,  s2",  s0,  s5,  i32, f2u(INFINITY), s2, i32, f2u(-INFINITY));
    TESTINSN_bin_f32("vfnma.f32 s20, s13, s15", s20, s13, i32, f2u(-INFINITY), s15, i32, f2u(0.0));
    TESTINSN_bin_f32("vfnma.f32 s10, s23, s15", s10, s23, i32, f2u(INFINITY), s15, i32, f2u(0.0));

    printf("---- VFNMS (fp, VFPv4) ----\n");
    TESTINSN_bin_f64("vfnms.f64 d0,  d11, d12", d0,  d11, i32, f2u0(-INFINITY), f2u1(-INFINITY), d12, i32, f2u0(NAN), f2u1(NAN));
    TESTINSN_bin_f64("vfnms.f64 d7,  d1,  d6",  d7,  d1,  i32, f2u0(INFINITY), f2u1(INFINITY), d6, i32, f2u0(NAN), f2u1(NAN));
    TESTINSN_bin_f64("vfnms.f64 d0,  d5,  d2",  d0,  d5,  i32, f2u0(NAN), f2u1(NAN), d2, i32, f2u0(-1.0), f2u1(-1.0));
    TESTINSN_bin_f64("vfnms.f64 d10, d13, d15", d10, d13, i32, f2u0(NAN), f2u1(NAN), d15, i32, f2u0(0.0), f2u1(0.0));
    TESTINSN_bin_f64("vfnms.f64 d10, d13, d15", d10, d13, i32, f2u0(NAN), f2u1(NAN), d15, i32, f2u0(NAN), f2u1(NAN));
    TESTINSN_bin_f64("vfnms.f64 d20, d25, d22", d20, d25, i32, f2u0(23.04), f2u1(23.04), d22, i32, f2u0(-45.5687), f2u1(-45.5687));
    TESTINSN_bin_f64("vfnms.f64 d23, d24, d25", d23, d24, i32, f2u0(-347856.475), f2u1(-347856.475), d25, i32, f2u0(1346), f2u1(1346));
    TESTINSN_bin_f64("vfnms.f64 d20, d31, d12", d20, d31, i32, f2u0(48755), f2u1(48755), d12, i32, f2u0(-45786.476), f2u1(-45786.476));
    TESTINSN_bin_f64("vfnms.f64 d19, d25, d27", d19, d25, i32, f2u0(95867.76), f2u1(95867.76), d27, i32, f2u0(17065), f2u1(17065));
    TESTINSN_bin_f64("vfnms.f64 d30, d15, d2",  d30, d15, i32, f2u0(-45667.24), f2u1(-45667.24), d2, i32, f2u0(-248562.76), f2u1(-248562.76));
    TESTINSN_bin_f64("vfnms.f64 d23, d24, d5",  d23, d24, i32, f2u0(24), f2u1(24), d5, i32, f2u0(1346), f2u1(1346));
    TESTINSN_bin_f64("vfnms.f64 d10, d11, d2",  d10, d11, i32, f2u0(48755), f2u1(48755), d2, i32, f2u0(1089), f2u1(1089));
    TESTINSN_bin_f64("vfnms.f64 d29, d15, d7",  d29, d15, i32, f2u0(214), f2u1(214), d7, i32, f2u0(1752065), f2u1(1752065));
    TESTINSN_bin_f64("vfnms.f64 d30, d11, d12", d30, d11, i32, f2u0(356047.56), f2u1(356047.56), d12, i32, f2u0(5867.009), f2u1(5867.009));
    TESTINSN_bin_f64("vfnms.f64 d27, d21, d6",  d27, d21, i32, f2u0(34.00046), f2u1(34.00046), d6, i32, f2u0(0.0024575), f2u1(0.0024575));
    TESTINSN_bin_f64("vfnms.f64 d30, d31, d2",  d30, d31, i32, f2u0(2754), f2u1(2754), d2, i32, f2u0(107), f2u1(107));
    TESTINSN_bin_f64("vfnms.f64 d13, d24, d5",  d13, d24, i32, f2u0(874), f2u1(874), d5, i32, f2u0(1384.6), f2u1(1384.6));
    TESTINSN_bin_f64("vfnms.f64 d10, d11, d2",  d10, d11, i32, f2u0(487.587), f2u1(487.587), d2, i32, f2u0(109), f2u1(109));
    TESTINSN_bin_f64("vfnms.f64 d29, d25, d7",  d29, d25, i32, f2u0(-INFINITY), f2u1(-INFINITY), d7, i32, f2u0(1752), f2u1(1752));
    TESTINSN_bin_f64("vfnms.f64 d0,  d11, d12", d0,  d11, i32, f2u0(INFINITY), f2u1(INFINITY), d12, i32, f2u0(-5786.47), f2u1(-5786.47));
    TESTINSN_bin_f64("vfnms.f64 d27, d21, d16", d27, d21, i32, f2u0(456.2489562), f2u1(456.2489562), d16, i32, f2u0(-7.2945676), f2u1(-7.2945676));
    TESTINSN_bin_f64("vfnms.f64 d0,  d5,  d2",  d0,  d5,  i32, f2u0(INFINITY), f2u1(INFINITY), d2, i32, f2u0(-INFINITY), f2u1(-INFINITY));
    TESTINSN_bin_f64("vfnms.f64 d20, d13, d15", d20, d13, i32, f2u0(-INFINITY), f2u1(-INFINITY), d15, i32, f2u0(0.0), f2u1(0.0));
    TESTINSN_bin_f64("vfnms.f64 d10, d23, d15", d10, d23, i32, f2u0(INFINITY), f2u1(INFINITY), d15, i32, f2u0(0.0), f2u1(0.0));
    TESTINSN_bin_f32("vfnms.f32 s0,  s11, s12", s0,  s11, i32, f2u(-INFINITY), s12, i32, f2u(NAN));
    TESTINSN_bin_f32("vfnms.f32 s7,  s1,  s6",  s7,  s1,  i32, f2u(INFINITY), s6, i32, f2u(NAN));
    TESTINSN_bin_f32("vfnms.f32 s0,  s5,  s2",  s0,  s5,  i32, f2u(NAN), s2, i32, f2u(-1.0));
    TESTINSN_bin_f32("vfnms.f32 s10, s13, s15", s10, s13, i32, f2u(NAN), s15, i32, f2u(0.0));
    TESTINSN_bin_f32("vfnms.f32 s10, s13, s15", s10, s13, i32, f2u(NAN), s15, i32, f2u(NAN));
    TESTINSN_bin_f32("vfnms.f32 s20, s25, s22", s20, s25, i32, f2u(23.04), s22, i32, f2u(-45.5687));
    TESTINSN_bin_f32("vfnms.f32 s23, s24, s25", s23, s24, i32, f2u(-347856.475), s25, i32, f2u(1346));
    TESTINSN_bin_f32("vfnms.f32 s20, s31, s12", s20, s31, i32, f2u(48755), s12, i32, f2u(-45786.476));
    TESTINSN_bin_f32("vfnms.f32 s19, s25, s27", s19, s25, i32, f2u(95867.76), s27, i32, f2u(17065));
    TESTINSN_bin_f32("vfnms.f32 s30, s15, s2",  s30, s15, i32, f2u(-45667.24), s2, i32, f2u(-248562.76));
    TESTINSN_bin_f32("vfnms.f32 s23, s24, s5",  s23, s24, i32, f2u(24), s5, i32, f2u(1346));
    TESTINSN_bin_f32("vfnms.f32 s10, s11, s2",  s10, s11, i32, f2u(48755), s2, i32, f2u(1089));
    TESTINSN_bin_f32("vfnms.f32 s29, s15, s7",  s29, s15, i32, f2u(214), s7, i32, f2u(1752065));
    TESTINSN_bin_f32("vfnms.f32 s30, s11, s12", s30, s11, i32, f2u(356047.56), s12, i32, f2u(5867.009));
    TESTINSN_bin_f32("vfnms.f32 s27, s21, s6",  s27, s21, i32, f2u(34.00046), s6, i32, f2u(0.0024575));
    TESTINSN_bin_f32("vfnms.f32 s30, s31, s2",  s30, s31, i32, f2u(2754), s2, i32, f2u(107));
    TESTINSN_bin_f32("vfnms.f32 s13, s24, s5",  s13, s24, i32, f2u(874), s5, i32, f2u(1384.6));
    TESTINSN_bin_f32("vfnms.f32 s10, s11, s2",  s10, s11, i32, f2u(487.587), s2, i32, f2u(109));
    TESTINSN_bin_f32("vfnms.f32 s29, s25, s7",  s29, s25, i32, f2u(-INFINITY), s7, i32, f2u(1752));
    TESTINSN_bin_f32("vfnms.f32 s0,  s11, s12", s0,  s11, i32, f2u(INFINITY), s12, i32, f2u(-5786.47));
    TESTINSN_bin_f32("vfnms.f32 s27, s21, s16", s27, s21, i32, f2u(456.2489562), s16, i32, f2u(-7.2945676));
    TESTINSN_bin_f32("vfnms.f32 s0,  s5,  s2",  s0,  s5,  i32, f2u(INFINITY), s2, i32, f2u(-INFINITY));
    TESTINSN_bin_f32("vfnms.f32 s20, s13, s15", s20, s13, i32, f2u(-INFINITY), s15, i32, f2u(0.0));
    TESTINSN_bin_f32("vfnms.f32 s10, s23, s15", s10, s23, i32, f2u(INFINITY), s15, i32, f2u(0.0));

    return 0;
}
