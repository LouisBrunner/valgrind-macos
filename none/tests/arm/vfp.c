
/* Can be compiled both as ARM or Thumb using
   gcc -Wall -g -O0 -mcpu=cortex-a8 -mfpu=neon -mfloat-abi=softfp -m{arm,thumb} -o vfp vfp.c
*/

#include <stdio.h>
#include <string.h>
#include <math.h>

#ifndef __thumb__
// ARM
#define MOVE_to_FPSCR_from_R4 \
      ".word 0xEEE14A10 @ vmsr FPSCR, r4\n\t"
#define MOVE_to_R4_from_FPSCR \
      ".word 0xEEF14A10 @ vmrs r4, FPSCR\n\t"
#endif

#ifdef __thumb__
// Thumb
#define MOVE_to_FPSCR_from_R4 \
      ".word 0x4A10EEE1 @ vmsr FPSCR, r4\n\t"
#define MOVE_to_R4_from_FPSCR \
      ".word 0x4A10EEF1 @ vmrs r4, FPSCR\n\t"
#endif

static inline unsigned int f2u(float x) {
    union {
        float f;
        unsigned int u;
    } cvt;
    cvt.f = x;
    return cvt.u;
}

static inline unsigned int f2u0(double x) {
    union {
        double f;
        unsigned int u[2];
    } cvt;
    cvt.f = x;
    return cvt.u[0];
}

static inline unsigned int f2u1(double x) {
    union {
        double f;
        unsigned int u[2];
    } cvt;
    cvt.f = x;
    return cvt.u[1];
}

/* test macros to generate and output the result of a single instruction */

const unsigned int mem[] = {
    0x121f1e1f, 0x131b1a1b, 0x141c1f1c, 0x151d191d,
    0x232f2e2f, 0x242c2b2b, 0x252a2e2b, 0x262d2d2a,
    0x3f343f3e, 0x3e353d3c, 0x363a3c3b, 0x3b373b3a,
    0x454f4e45, 0x4e464d46, 0x474d474c, 0x4a484a4c
};

#define TESTINSN_vmovf32_imm(instruction, DD, imm) \
{ \
  unsigned int out[1]; \
\
  __asm__ volatile( \
      instruction ", #"#imm"\n\t"\
      "vstmia %0, {" #DD "}\n\t" \
      : \
      : "r" (out) \
      : #DD, "memory" \
      ); \
  printf("%s, #" #imm " :: Sd 0x%08x\n", \
      instruction, out[0]); \
}

#define TESTINSN_vmov_core_single(instruction, RN, SD, SDval) \
{ \
  unsigned int out[1]; \
\
  printf(#SD" 0x%08x\t",  SDval); \
  __asm__ volatile( \
      "mov " #RN ", #0\n\t" \
      "vmov.f32 " #SD ", %1\n\t" \
      instruction "\n\t" \
      "str " #RN ", [%0]\n\t" \
      : \
      : "r" (out), "r" (SDval) \
      : #SD, #RN, "memory" \
      ); \
  printf("%s :: "#RN" 0x%08x\n", \
      instruction, out[0]); \
}

#define TESTINSN_vmov_single_core(instruction, SD, RN, RNval) \
{ \
  unsigned int out[1]; \
\
  printf(#RN" 0x%08x\t",  RNval); \
  __asm__ volatile( \
      "mov " #RN ", %1\n\t" \
      "vmov " #SD ", #0x40000000\n\t" \
      instruction "\n\t"\
      "vstmia %0, {" #SD "}\n\t" \
      : \
      : "r" (out), "r" (RNval) \
      : #SD, #RN, "memory" \
      ); \
  printf("%s :: "#SD" 0x%08x\n", \
      instruction, out[0]); \
}

#define TESTINSN_vmov_2core_2single(instruction, RD1, RD2, SN, SM, SNval, SMval) \
{ \
  unsigned int out[2]; \
\
  printf("\t\t\t "#SN" 0x%08x "#SM" 0x%08x\n",  SNval, SMval); \
  __asm__ volatile( \
      "vmov " #SN ", %1\n\t" \
      "vmov " #SM ", %2\n\t" \
      "mov " #RD1 ", #0x4\n\t" \
      "mov " #RD2 ", #0x4\n\t" \
      instruction "\n\t"\
      "str " #RD1 ", [%0]\n\t" \
      "str " #RD2 ", [%0, #+4]\n\t" \
      : \
      : "r" (out), "r" (SNval), "r" (SMval) \
      : #RD1, #RD2, #SN, #SM, "memory" \
      ); \
  printf("%s :: "#RD1" 0x%08x "#RD2" 0x%08x\n", \
      instruction, out[0], out[1]); \
}

#define TESTINSN_vmov_2single_2core(instruction, SD1, SD2, RN, RM, RNval, RMval) \
{ \
  unsigned int out[2]; \
\
  printf("\t\t\t "#RN" 0x%08x "#RM" 0x%08x\n",  RNval, RMval); \
  __asm__ volatile( \
      "mov " #RN ", %1\n\t" \
      "mov " #RM ", %2\n\t" \
      "vmov " #SD1 ", #0x40000000\n\t" \
      "vmov " #SD2 ", #0x40000000\n\t" \
      instruction "\n\t"\
      "vstmia %0, {" #SD1 ", " #SD2 " }\n\t" \
      : \
      : "r" (out), "r" (RNval), "r" (RMval) \
      : #SD1, #SD2, #RN, #RM, "memory" \
      ); \
  printf("%s :: "#SD1" 0x%08x "#SD2" 0x%08x\n", \
      instruction, out[0], out[1]); \
}

#define TESTINSN_vmov_double_2core(instruction, DD, RN, RM, RNval, RMval) \
{ \
  unsigned int out[2]; \
\
  printf(#RN" 0x%08x "#RM" 0x%08x\t",  RNval, RMval); \
  __asm__ volatile( \
      "mov " #RN ", %1\n\t" \
      "mov " #RM ", %2\n\t" \
      "vmov.i8 " #DD ", #0x55\n\t" \
      instruction "\n\t"\
      "vstmia %0, {" #DD "}\n\t" \
      : \
      : "r" (out), "r" (RNval), "r" (RMval) \
      : #DD, #RN, #RM, "memory" \
      ); \
  printf("%s :: "#DD" 0x%08x 0x%08x\n", \
      instruction, out[0], out[1]); \
}

#define TESTINSN_vmov_2core_double(instruction, RD1, RD2, DN, DNval0, DNval1) \
{ \
  unsigned int out[2]; \
\
  printf(#DN" 0x%08x 0x%08x\t",  DNval0, DNval1); \
  __asm__ volatile( \
      "mov " #RD1 ", #55\n\t" \
      "mov " #RD2 ", #55\n\t" \
      "vmov " #DN ", %1, %2\n\t" \
      instruction "\n\t" \
      "str " #RD1 ", [%0]\n\t" \
      "str " #RD2 ", [%0, #+4]\n\t" \
      : \
      : "r" (out), "r" (DNval0), "r" (DNval1) \
      : #DN, #RD1, #RD2, "memory" \
      ); \
  printf("%s :: "#RD1" 0x%08x "#RD2" 0x%08x\n", \
      instruction, out[0], out[1]); \
}

#define TESTINSN_un_f64(instruction, DD, DM, DMtype, DMval0, DMval1) \
{ \
  unsigned int out[2]; \
\
  __asm__ volatile( \
      "vmov.i8 " #DD ", #0x55" "\n\t" \
      "vmov " #DM ", %1, %2 \n\t" \
      instruction "\n\t" \
      "vstmia %0, {" #DD "}\n\t" \
      : \
      : "r" (out), "r" (DMval0), "r" (DMval1) \
      : #DD, #DM, "memory" \
      ); \
  printf("%s :: Dd 0x%08x 0x%08x  Dm (" #DMtype ")0x%08x %08x\n", \
      instruction, out[1], out[0], DMval1, DMval0); \
}

#define TESTINSN_un_f32(instruction, SD, SM, SMtype, SMval) \
{ \
  unsigned int out[1]; \
\
  __asm__ volatile( \
      "vmov.f32 " #SM ", %1\n\t" \
      "vmov.f32 " #SD ", %2\n\t" \
      instruction "\n\t" \
      "vstmia %0, {" #SD "}\n\t" \
      : \
      : "r" (out), "r" (SMval), "r" (0xffffaaaa) \
      : #SD, #SM, "memory" \
      ); \
  printf("%s :: Sd 0x%08x Sm (" #SMtype ")0x%08x\n", \
      instruction, out[0], SMval); \
}

#define TESTINSN_un_cvt_ds(instruction, DD, SM, SMval) \
{ \
  unsigned int out[2]; \
\
  __asm__ volatile( \
      "vmov " #SM ", %1\n\t" \
      "vmov " #DD ", %2, %2\n\t" \
      instruction "\n\t" \
      "vstmia %0, {" #DD "}\n\t" \
      : \
      : "r" (out), "r" (SMval), "r" (0xffffaaaa) \
      : #DD, #SM, "memory" \
      ); \
  printf("%s :: Dd 0x%08x 0x%08x  Sm 0x%08x\n", \
      instruction, out[1], out[0], SMval); \
}

#define TESTINSN_un_cvt_sd(instruction, SD, DM, DMval0, DMval1) \
{ \
  unsigned int out[1]; \
\
  __asm__ volatile( \
      "vmov " #SD ", %3\n\t" \
      "vmov " #DM ", %1, %2\n\t" \
      instruction "\n\t" \
      "vstmia %0, {" #SD "}\n\t" \
      : \
      : "r" (out), "r" (DMval0), "r" (DMval1), "r" (0xffffaaaa) \
      : #SD, #DM, "memory" \
      ); \
  printf("%s :: Sd 0x%08x  Dm 0x%08x %08x\n", \
      instruction, out[0], DMval1, DMval0); \
}

#define TESTINSN_cvt_i32_f64(instruction, SD, DM, DMval0, DMval1) \
{ \
  unsigned int out[1]; \
\
  __asm__ volatile( \
      "vmov " #DM ", %1, %2\n\t" \
      "vmov " #SD ", %3\n\t" \
      instruction "\n\t" \
      "vstmia %0, {" #SD "}\n\t" \
      : \
      : "r" (out), "r" (DMval0), "r" (DMval1), "r" (0xffffaaaa) \
      : #SD, #DM, "memory" \
      ); \
 printf("%s :: Sd 0x%08x Dm 0x%08x %08x\n", \
      instruction, out[0], DMval1, DMval0); \
}

#define TESTINSN_cvt_f64_i32(instruction, DD, SM, SMval) \
{ \
  unsigned int out[2]; \
\
  __asm__ volatile( \
      "vmov " #SM ", %1\n\t" \
      "vmov " #DD ", %2, %2\n\t" \
      instruction "\n\t" \
      "vstmia %0, {" #DD "}\n\t" \
      : \
      : "r" (out), "r" (SMval), "r" (0xfffffff0) \
      : #DD, #SM, "memory" \
      ); \
 printf("%s :: Dd 0x%08x %08x Sm 0x%08x\n", \
      instruction, out[0], out[1], SMval); \
}

#define TESTINSN_un_f64_q_vmrs(instruction, DD, DM, DMtype, DMval, RN) \
{ \
  unsigned int out[2]; \
  unsigned int fpscr; \
\
  __asm__ volatile( \
      "vmov.i8 " #DD ", #0x55" "\n\t" \
      "mov r4, #0\n\t" \
      ".word 0xEEE14A10 @ vmsr FPSCR, "#RN"\n\t" \
      "vdup." #DMtype " " #DM ", %2\n\t" \
      instruction "\n\t" \
      "vstmia %1, {" #DD "}\n\t" \
      ".word 0xEEF14A10 @ vmrs "#RN", FPSCR\n\t" \
      "mov %0, r4" \
      : "=r" (fpscr) \
      : "r" (out), "r" (DMval) \
      : #DD, #DM, "memory", #RN \
      ); \
  printf("%s :: Dd 0x%08x 0x%08x  Dm (" #DMtype ")0x%08x  fpscr %08x\n", \
      instruction, out[1], out[0], DMval, fpscr); \
}

#define TESTINSN_core_to_scalar(instruction, DD, DM, DMval) \
{ \
  unsigned int out[2]; \
\
  __asm__ volatile( \
      "vmov.i8 " #DD ", #0x55" "\n\t" \
      "mov " #DM ", %1\n\t" \
      instruction "\n\t" \
      "vstmia %0, {" #DD "}\n\t" \
      : \
      : "r" (out), "r" (DMval) \
      : #DD, #DM, "memory" \
      ); \
  printf("%s :: Dd 0x%08x 0x%08x  Dm 0x%08x\n", \
      instruction, out[1], out[0], DMval); \
}

#define TESTINSN_vldr_f64(instruction, DD, RN, RNval, imm) \
{ \
  unsigned int out[2]; \
\
  __asm__ volatile( \
      "vmov.i8 " #DD ", #0x55" "\n\t" \
      "mov " #RN ", %1\n\t" \
      instruction "\n\t" \
      "vstmia %0, {" #DD "}\n\t" \
      : \
      : "r" (out), "r" (RNval) \
      : #DD, #RN, "memory" \
      ); \
  printf("%s :: Dd 0x%08x 0x%08x  *(int*) (Rn + shift) 0x%04x\n", \
      instruction, out[1], out[0], *(int*) (RNval + imm)); \
}

#define TESTINSN_vldr_f32(instruction, SD, RN, RNval, imm) \
{ \
  unsigned int out[1]; \
\
  __asm__ volatile( \
      "vmov " #SD ", %3" "\n\t" \
      "mov " #RN ", %1\n\t" \
      instruction "\n\t" \
      "vstmia %0, {" #SD "}\n\t" \
      : \
      : "r" (out), "r" (RNval), "r" (imm), "r" (0xffffffaa) \
      : #SD, #RN, "memory" \
      ); \
  printf("%s :: Sd 0x%08x  *(int*) (Rn + shift) 0x%04x\n", \
      instruction, out[0], *(int*) (RNval + imm)); \
}

#define TESTINSN_vstr64(instruction, DD, DDval, RM, RMval, imm) \
{ \
  unsigned int out[2]; \
\
  __asm__ volatile( \
      "vmov.i8 " #DD ", #" #DDval "\n\t" \
      "add %1, %1, #" #imm "\n\t" \
      "mov " #RM ", #0x55\n\t" \
      "str " #RM ", [%1]\n\t" \
      "str " #RM ", [%1, #4]\n\t" \
      "sub %1, %1, #" #imm "\n\t" \
      "mov " #RM ", %1\n\t" \
      instruction "\n\t" \
      "vstmia %0, {" #DD "}\n\t" \
      : \
      : "r" (out), "r" (RMval) \
      : #DD, #RM, "memory" \
      ); \
  printf("%s :: Dd 0x%08x 0x%08x  *(int*) (Rm + shift) 0x%04x\n", \
      instruction, out[1], out[0], *(int*) (RMval + imm)); \
}

#define TESTINSN_vstr32(instruction, SD, RM, RMval, imm) \
{ \
  unsigned int out[1]; \
\
  __asm__ volatile( \
      "vmov " #SD ", #0xbe280000\n\t" \
      "mov " #RM ", #0x55\n\t" \
      "str " #RM ", [%1, #" #imm "]\n\t" \
      "mov " #RM ", %1\n\t" \
      instruction "\n\t" \
      "vstmia %0, {" #SD "}\n\t" \
      : \
      : "r" (out), "r" (RMval) \
      : #SD, #RM, "memory" \
      ); \
  printf("%s :: Sd 0x%08x,  *(int*) (Rm + shift) 0x%04x\n", \
      instruction, out[0], *(int*) (RMval + imm)); \
}

#define TESTINSN_scalar_to_core(instruction, QD, QM, QMtype, QMval) \
{ \
  unsigned int out[2]; \
\
  __asm__ volatile( \
      "mov " #QD ", #0x55" "\n\t" \
      "vdup." #QMtype " " #QM ", %1\n\t" \
      instruction "\n\t" \
      "str " #QD ", [%0]\n\t" \
      : \
      : "r" (out), "r" (QMval) \
      : #QD, #QM, "memory" \
      ); \
  printf("%s :: Rd 0x%08x  Qm (" #QMtype ")0x%08x\n", \
      instruction, out[0], QMval); \
}

#define TESTINSN_VLDn(instruction, QD1, QD2, QD3, QD4) \
{ \
  unsigned int out[8]; \
\
  __asm__ volatile( \
      "vmov.i8 " #QD1 ", #0x55" "\n\t" \
      "vmov.i8 " #QD2 ", #0x55" "\n\t" \
      "vmov.i8 " #QD3 ", #0x55" "\n\t" \
      "vmov.i8 " #QD4 ", #0x55" "\n\t" \
      instruction ", [%1]\n\t" \
      "mov r4, %0\n\t" \
      "vstmia %0!, {" #QD1 "}\n\t" \
      "vstmia %0!, {" #QD2 "}\n\t" \
      "vstmia %0!, {" #QD3 "}\n\t" \
      "vstmia %0!, {" #QD4 "}\n\t" \
      "mov %0, r4\n\t" \
      : \
      : "r" (out), "r" (mem) \
      : #QD1, #QD2, #QD3, #QD4, "memory", "r4" \
      ); \
  printf("%s :: Result 0x%08x 0x%08x 0x%08x 0x%08x "\
          "0x%08x 0x%08x 0x%08x 0x%08x\n", \
      instruction, out[0], out[1], out[2], out[3], out[4],\
          out[5], out[6], out[7]); \
}

#define TESTINSN_VSTMIAnoWB(instruction, RN, QD, QDval) \
{ \
  unsigned int out[2]; \
\
  __asm__ volatile( \
      "vmov.i8 " #QD ", " #QDval "\n\t" \
      "mov " #RN ", %0\n\t" \
      instruction "\n\t" \
      : \
      : "r" (out), "r" (mem) \
      : #QD, "memory", #RN \
      ); \
  printf("%s :: Result 0x%08x 0x%08x\n", \
      instruction, out[0], out[1]); \
}

#define TESTINSN_VSTMIAnoWB32(instruction, RN, SD, SDval) \
{ \
  unsigned int out[1]; \
\
  __asm__ volatile( \
      "vmov " #SD ", %2\n\t" \
      "mov " #RN ", %0\n\t" \
      instruction "\n\t" \
      : \
      : "r" (out), "r" (mem), "r" (SDval) \
      : #SD, "memory", #RN \
      ); \
  printf("%s :: Result 0x%08x\n", \
      instruction, out[0]); \
}

#define TESTINSN_VSTMIAWB(RN, QD1, QD2) \
{ \
  unsigned int out[4]; \
\
  __asm__ volatile( \
      "vmov.i8 " #QD1 ", #0xa0" "\n\t"  \
      "vmov.i8 " #QD2 ", #0xb1" "\n\t" \
      "mov " #RN ", %0\n\t" \
      "vstmia " #RN "!, {" #QD1 "}\n\t" \
      "vstmia " #RN "!, {" #QD2 "}\n\t" \
      : \
      : "r" (out), "r" (mem) \
      : #QD1, #QD2, "memory", #RN \
      ); \
  printf("vstmia "#RN"!, "#QD1"; vstmia "#RN"!, "#QD2" :: Result 0x%08x 0x%08x 0x%08x 0x%08x\n", \
         out[0], out[1], out[2], out[3]); \
}

#define TESTINSN_VSTMIAWB32(RN, SD1, SD2) \
{ \
  unsigned int out[2]; \
\
  __asm__ volatile( \
      "vmov " #SD1 ", #0xbff80000" "\n\t" \
      "vmov " #SD2 ", #0x3fa80000" "\n\t" \
      "mov " #RN ", %0\n\t" \
      "vstmia " #RN "!, {" #SD1 "}\n\t" \
      "vstmia " #RN "!, {" #SD2 "}\n\t" \
      : \
      : "r" (out), "r" (mem) \
      : #SD1, #SD2, "memory", #RN \
      ); \
  printf("vstmia " #RN "!, "#SD1"; vstmia "#RN"!, "#SD2" :: Result 0x%08x 0x%08x\n", \
         out[0], out[1]); \
}

#define TESTINSN_VSTMDB(RN, QD1, QD2) \
{ \
    unsigned int out[4]; \
    long endout = (long) out + 8; \
\
  __asm__ volatile( \
      "vmov.i8 " #QD1 ", #0xaa" "\n\t" \
      "vmov.i8 " #QD2 ", #0xbb" "\n\t" \
      "mov " #RN ", %0\n\t" \
      "vstmdb " #RN "!, {" #QD1 "}\n\t" \
      "vstmdb " #RN "!, {" #QD2 "}\n\t" \
      "mov %0, " #RN "\n\t" \
      : \
      : "r" (endout), "r" (mem) \
      : #QD1, #QD2, "memory", #RN \
      ); \
  printf("vstmdb " #RN "!, " #QD2 "; vstmdb " #RN "!, " #QD2 \
         " :: Result 0x%08x 0x%08x 0x%08x 0x%08x\n", \
         out[0], out[1], out[2], out[3]); \
}

#define TESTINSN_VLDMIAnoWB(instruction, RN, QD) \
{ \
  unsigned int in[2] = {0xaa0, 0xbb1}; \
  unsigned int out[2]; \
\
  __asm__ volatile( \
      "vmov.i8 " #QD ", #0x55" "\n\t" \
      "mov " #RN ", %0\n\t" \
      instruction "\n\t" \
      "mov " #RN ", %1\n\t" \
      "vstmia " #RN ", {" #QD "}\n\t" \
      : \
      : "r" (in), "r" (out), "r" (mem) \
      : #QD, "memory", #RN \
      ); \
  printf("%s :: Result 0x%08x 0x%08x 0x%08x 0x%08x\n", \
         instruction, out[0], out[1], out[2], out[3]); \
}

#define TESTINSN_VLDMIAWB(RN, QD1, QD2) \
{ \
  unsigned int in[4] = {0xaa0, 0xbb1, 0xcc2, 0xdd3}; \
  unsigned int out[4]; \
\
  __asm__ volatile( \
      "vmov.i8 " #QD1 ", #0x55" "\n\t" \
      "vmov.i8 " #QD2 ", #0x55" "\n\t" \
      "mov " #RN ", %0\n\t" \
      "vldmia " #RN "!, {" #QD1 "}\n\t" \
      "vldmia " #RN "!, {" #QD2 "}\n\t" \
      "mov " #RN ", %1\n\t" \
      "vstmia " #RN "!, {" #QD1 "}\n\t" \
      "vstmia " #RN "!, {" #QD2 "}\n\t" \
      : \
      : "r" (in), "r" (out), "r" (mem) \
      : #QD1, #QD2, "memory", #RN \
      ); \
  printf("vldmia rN!, qD1; vldmia rN!, qD2 :: Result 0x%08x 0x%08x 0x%08x 0x%08x\n", \
         out[0], out[1], out[2], out[3]); \
}

#define TESTINSN_VLDMDB(RN, QD1, QD2) \
{ \
    unsigned int in[4] = {0xaa0, 0xbb1, 0xcc2, 0xdd3}; \
    unsigned int out[4]; \
    long endin = (long) in + 16; \
\
  __asm__ volatile( \
      "vmov.i8 " #QD1 ", #0x55" "\n\t" \
      "vmov.i8 " #QD2 ", #0x55" "\n\t" \
      "mov " #RN ", %0\n\t" \
      "vldmdb " #RN "!, {" #QD1 "}\n\t" \
      "vldmdb " #RN "!, {" #QD2 "}\n\t" \
      "mov " #RN ", %1\n\t" \
      "vstmia " #RN "!, {" #QD1 "}\n\t" \
      "vstmia " #RN "!, {" #QD2 "}\n\t" \
      : \
      : "r" (endin), "r" (out), "r" (mem) \
      : #QD1, #QD2, "memory", #RN \
      ); \
  printf("vldmdb rN!, qD1; vldmia rN!, qD2 :: Result 0x%08x 0x%08x 0x%08x 0x%08x\n", \
         out[0], out[1], out[2], out[3]); \
}

#define TESTINSN_VLDR(instruction, dD, rN, rNval, offset) \
{ \
  unsigned int out[2]; \
\
  __asm__ volatile( \
      "vmov.i8 " #dD ", #0x55\n\t" \
      "mov " #rN ", %1\n\t" \
      instruction ", #" #offset "]\n\t" \
      "vstmia %0, {" #dD "}\n\t" \
      : \
      : "r" (out), "r" (rNval) \
      : #dD, "memory" \
      ); \
      printf("%s :: dD 0x%08x 0x%08x  rN 0x%08x\n", \
      instruction, out[1], out[0], rNval); \
}


#define TESTINSN_vpush_vpop_f32(S1, Sval1, S2, Sval2, S3, Sval3, S4, S5, S6) \
{ \
  unsigned int out[6]; \
\
  __asm__ volatile( \
      "vmov "#S4", %4\n\t" \
      "vmov "#S5", %4\n\t" \
      "vmov "#S6", %4\n\t" \
      "vmov "#S1", %1\n\t" \
      "vmov "#S2", %2\n\t" \
      "vmov "#S3", %3\n\t" \
      "vpush {"#S1", "#S2"}\n\t" \
      "vpush {"#S3"}\n\t" \
      "vpop  {"#S4"}\n\t" \
      "vpop  {"#S5", "#S6"}\n\t" \
      "mov r4, %0\n\t" \
      "vstmia %0!, {"#S1"}\n\t" \
      "vstmia %0!, {"#S2"}\n\t" \
      "vstmia %0!, {"#S3"}\n\t" \
      "vstmia %0!, {"#S4"}\n\t" \
      "vstmia %0!, {"#S5"}\n\t" \
      "vstmia %0!, {"#S6"}\n\t" \
      "mov %0, r4\n\t" \
      : \
      : "r" (out), "r" (Sval1), "r" (Sval2), "r" (Sval3), "r" (0x55555555) \
      : #S1, #S2, #S3, #S4, #S5, #S6, "r4", "memory" \
      ); \
      printf(#S1" 0x%08x "#S2" 0x%08x "#S3" 0x%08x "#S4" 0x%08x "\
             #S5" 0x%08x "#S6" 0x%08x\n", out[0], out[1],\
             out[2], out[3], out[4], out[5]); \
}

#define TESTINSN_vpush_vpop_f64(D1, Dval10, Dval11, D2, Dval20, Dval21, D3, D4) \
{ \
  unsigned int out[8]; \
\
  __asm__ volatile( \
      "vmov "#D3", %4, %4\n\t" \
      "vmov "#D4", %4, %4\n\t" \
      "vmov "#D1", %1, %2\n\t" \
      "vmov "#D2", %3, %4\n\t" \
      "vpush {"#D1", "#D2"}\n\t" \
      "vpop  {"#D3", "#D4"}\n\t" \
      "mov r4, %0\n\t" \
      "vstmia %0!, {"#D1"}\n\t" \
      "vstmia %0!, {"#D2"}\n\t" \
      "vstmia %0!, {"#D3"}\n\t" \
      "vstmia %0!, {"#D4"}\n\t" \
      "mov %0, r4\n\t" \
      : \
      : "r" (out), "r" (Dval10), "r" (Dval11), "r" (Dval20), "r" (Dval21), "r" (0x55555555) \
      : #D1, #D2, #D3, #D4, "r4", "memory" \
      ); \
      printf(#D1" 0x%08x %08x "#D2" 0x%08x %08x "#D3" 0x%08x %08x "#D4" 0x%08x %08x\n",\
                  out[0],out[1],    out[2],out[3],    out[4],out[5],    out[6],out[7]); \
}

#define TESTINSN_VSTn(instruction, QD1, QD2, QD3, QD4) \
{ \
  unsigned int out[8]; \
\
  memset(out, 0x55, 8 * (sizeof(unsigned int)));\
  __asm__ volatile( \
      "mov r4, %1\n\t" \
      "vldmia %1!, {" #QD1 "}\n\t" \
      "vldmia %1!, {" #QD2 "}\n\t" \
      "vldmia %1!, {" #QD3 "}\n\t" \
      "vldmia %1!, {" #QD4 "}\n\t" \
      "mov %1, r4\n\t" \
      instruction ", [%0]\n\t" \
      : \
      : "r" (out), "r" (mem) \
      : #QD1, #QD2, #QD3, #QD4, "memory", "r4" \
      ); \
  printf("%s :: Result 0x%08x 0x%08x 0x%08x 0x%08x "\
          "0x%08x 0x%08x 0x%08x 0x%08x\n", \
      instruction, out[0], out[1], out[2], out[3], out[4],\
          out[5], out[6], out[7]); \
}

#define TESTINSN_bin(instruction, QD, QM, QMtype, QMval, QN, QNtype, QNval) \
{ \
  unsigned int out[2]; \
\
  __asm__ volatile( \
      "vmov.i8 " #QD ", #0x55" "\n\t" \
      "vdup." #QMtype " " #QM ", %1\n\t" \
      "vdup." #QNtype " " #QN ", %2\n\t" \
      instruction "\n\t" \
      "vstmia %0, {" #QD "}\n\t" \
      : \
      : "r" (out), "r" (QMval), "r" (QNval) \
      : #QD, #QM, #QN, "memory" \
      ); \
  printf("%s :: Qd 0x%08x 0x%08x  Qm (" #QMtype ")0x%08x" \
      "  Qn (" #QNtype ")0x%08x\n", \
      instruction, out[1], out[0], QMval, QNval); \
}

#define TESTINSN_bin_f64(instruction, QD, QM, QMtype, QMval0, QMval1, QN, QNtype, QNval0, QNval1) \
{ \
  unsigned int out[2]; \
\
  __asm__ volatile( \
      "vdup.i32 " #QD ", %5\n\t" \
      "vmov " #QM ", %1, %2 \n\t" \
      "vmov " #QN ", %3, %4 \n\t" \
      instruction "\n\t" \
      "vstmia %0, {" #QD "}\n\t" \
      : \
      : "r" (out), "r" (QMval0), "r" (QMval1), "r" (QNval0), "r" (QNval1), "r"(0x3f800000) \
      : #QD, #QM, #QN, "memory" \
      ); \
  printf("%s :: Qd 0x%08x 0x%08x  Qm 0x%08x %08x" \
      "  Qn 0x%08x %08x\n", \
      instruction, out[1], out[0], QMval1, QMval0, QNval1, QNval0); \
}

#define TESTINSN_bin_f32(instruction, SD, SM, SMtype, SMval, SN, SNtype, SNval) \
{ \
  unsigned int out[1]; \
\
  __asm__ volatile( \
      "vmov.f32 " #SM ", %1\n\t" \
      "vmov.f32 " #SN ", %2\n\t" \
      "vmov.f32 " #SD ", %3\n\t" \
      instruction "\n\t" \
      "vstmia %0, {" #SD "}\n\t" \
      : \
      : "r" (out), "r" (SMval), "r" (SNval), "r" (0xaaaaaaaa) \
      : #SD, #SM, #SN, "memory" \
      ); \
  printf("%s :: Sd 0x%08x Sm (" #SMtype ")0x%08x" \
      "  Sn (" #SNtype ")0x%08x\n", \
      instruction, out[0], SMval, SNval); \
}

#define TESTINSN_cmp_f64(instruction, DD, DDval0, DDval1, DM, DMval0, DMval1) \
{ \
  unsigned int out[1]; \
\
  __asm__ volatile( \
      "vmov " #DD ", %1, %2\n\t" \
      "vmov " #DM ", %3, %4\n\t" \
      "mov r4, #0\n\t" \
      MOVE_to_FPSCR_from_R4 \
      instruction "\n\t" \
      MOVE_to_R4_from_FPSCR \
      "str r4, [%0]\n\t" \
      : \
      : "r" (out), "r" (DDval0), "r" (DDval1),"r" (DMval0), "r" (DMval1) \
      : #DD, #DM, "r4", "memory" \
      ); \
  printf("%s :: FPSCR 0x%08x  Dd 0x%08x %08x" \
      "  Dm 0x%08x %08x\n", \
      instruction, out[0] & 0xffffff60, DDval1, DDval0, DMval1, DMval0); \
}

#define TESTINSN_cmp_f32(instruction, SD, SDval, SM, SMval) \
{ \
  unsigned int out[1]; \
\
  __asm__ volatile( \
      "vmov " #SD ", %1\n\t" \
      "vmov " #SM ", %2\n\t" \
      "mov r4, #0\n\t" \
      MOVE_to_FPSCR_from_R4 \
      instruction "\n\t" \
      MOVE_to_R4_from_FPSCR \
      "str r4, [%0]\n\t" \
      : \
      : "r" (out), "r" (SDval),"r" (SMval) \
      : #SD, #SM, "r4", "memory" \
      ); \
  printf("%s :: FPSCR 0x%01x  Sd 0x%08x" \
      "  Sm 0x%08x\n", \
      instruction, (out[0] & 0xf0000000) >> 28, SDval, SMval); \
}

#define TESTINSN_cmpz_f32(instruction, SD, SDval) \
{ \
  unsigned int out[1]; \
\
  __asm__ volatile( \
      "vmov " #SD ", %1\n\t" \
      instruction ", #0\n\t" \
      MOVE_to_R4_from_FPSCR \
      "vmov " #SD ", r4\n\t" \
      "vstmia %0, {" #SD "}\n\t" \
      : \
      : "r" (out), "r" (SDval)\
      : #SD, "r4", "memory" \
      ); \
  printf("%s :: FPSCR 0x%08x  Sd 0x%08x\n", \
      instruction, out[0] & 0xffffff60, SDval); \
}

#define TESTINSN_cmpz_f64(instruction, DD, DDval0, DDval1) \
{ \
  unsigned int out[1]; \
\
  __asm__ volatile( \
      "vmov " #DD ", %1, %2\n\t" \
      instruction ", #0\n\t" \
      MOVE_to_R4_from_FPSCR \
      "str r4, [%0]\n\t" \
      : \
      : "r" (out), "r" (DDval0), "r" (DDval1) \
      : #DD, "r4", "memory" \
      ); \
  printf("%s :: FPSCR 0x%08x  Dd 0x%08x %08x\n", \
      instruction, out[0] & 0xffffff60, DDval1, DDval0); \
}

static void do_vldm_vstm_check(void)
{
    int i;
    const char *format = "\t0x%08x\n";
    unsigned int data[] = {
        0x1a1b1c1d, 0x2a2b2c2d, 0x3a3b3c3d, 0x4a4b4c4d,
        0x5a5b5c5d, 0x6a6b6c6d, 0x7a7b7c7d, 0x8a8b8c8d,
        0x9a9b9c9d, 0xaaabacad, 0xbabbbcbd, 0xcacbcccd,
        0xdadbdcdd, 0xeaebeced, 0xfafbfcfd, 0x0a0b0c0d
    };
    unsigned int res;
    printf("do_vldm_vstm_check:\n");
    __asm__ volatile(
            "mov r1, %0\n\t"
            "vldmia r1!, {s16, s17, s18, s19}\n\t"
            "mov r0, %1\n\t"
            "sub r1, r1, %0\n\t"
            "mov r3, r1\n\t"
            "bl printf\n\t"
            "mov r0, %1\n\t"
            "vmov r1, s18\n\t"
            "mov r3, r1\n\t"
            "bl printf\n\t"
            "mov r0, %1\n\t"
            "vmov r1, s19\n\t"
            "mov r3, r1\n\t"
            "bl printf\n\t"
            "mov r0, %1\n\t"
            "vmov r1, s16\n\t"
            "mov r3, r1\n\t"
            "bl printf\n\t"
/* --- */
            "add r1, %0, #32\n\t"
            "vldmdb r1!, {s25, s26}\n\t"
            "mov r0, %1\n\t"
            "sub r1, r1, %0\n\t"
            "mov r3, r1\n\t"
            "bl printf\n\t"
            "mov r0, %1\n\t"
            "vmov r1, s25\n\t"
            "mov r3, r1\n\t"
            "bl printf\n\t"
            "mov r0, %1\n\t"
            "vmov r1, s26\n\t"
            "mov r3, r1\n\t"
            "bl printf\n\t"
/* --- */
            "add r1, %0, #4\n\t"
            "vldmia r1, {s20, s21, s22, s23}\n\t"
            "mov r0, %1\n\t"
            "sub r1, r1, %0\n\t"
            "mov r3, r1\n\t"
            "bl printf\n\t"
            "mov r0, %1\n\t"
            "vmov r1, s22\n\t"
            "mov r3, r1\n\t"
            "bl printf\n\t"
            "mov r0, %1\n\t"
            "vmov r1, s23\n\t"
            "mov r3, r1\n\t"
            "bl printf\n\t"
            "mov r0, %1\n\t"
            "vmov r1, s20\n\t"
            "mov r3, r1\n\t"
            "bl printf\n\t"
/* --- */
            "add r1, %0, #48\n\t"
            "vldmia r1!, {d30, d31}\n\t"
            "mov r0, %1\n\t"
            "sub r1, r1, %0\n\t"
            "mov r3, r1\n\t"
            "bl printf\n\t"
            "mov r0, %1\n\t"
            "vmov r1, r5, d30\n\t"
            "mov r3, r1\n\t"
            "bl printf\n\t"
            "mov r0, %1\n\t"
            "mov r1, r5\n\t"
            "mov r3, r1\n\t"
            "bl printf\n\t"
            "mov r0, %1\n\t"
            "vmov r1, r5, d31\n\t"
            "mov r3, r1\n\t"
            "bl printf\n\t"
            "mov r0, %1\n\t"
            "mov r1, r5\n\t"
            "mov r3, r1\n\t"
            "bl printf\n\t"
/* --- */
            "add r1, %0, #44\n\t"
            "vldmia r1, {d30, d31}\n\t"
            "mov r0, %1\n\t"
            "sub r1, r1, %0\n\t"
            "mov r3, r1\n\t"
            "bl printf\n\t"
            "mov r0, %1\n\t"
            "vmov r1, r5, d30\n\t"
            "mov r3, r1\n\t"
            "bl printf\n\t"
            "mov r0, %1\n\t"
            "mov r1, r5\n\t"
            "mov r3, r1\n\t"
            "bl printf\n\t"
            "mov r0, %1\n\t"
            "vmov r1, r5, d31\n\t"
            "mov r3, r1\n\t"
            "bl printf\n\t"
            "mov r0, %1\n\t"
            "mov r1, r5\n\t"
            "mov r3, r1\n\t"
            "bl printf\n\t"
/* --- */
            "add r1, %0, #40\n\t"
            "vldmdb r1!, {d30, d31}\n\t"
            "mov r0, %1\n\t"
            "sub r1, r1, %0\n\t"
            "mov r3, r1\n\t"
            "bl printf\n\t"
            "mov r0, %1\n\t"
            "vmov r1, r5, d30\n\t"
            "mov r3, r1\n\t"
            "bl printf\n\t"
            "mov r0, %1\n\t"
            "mov r1, r5\n\t"
            "mov r3, r1\n\t"
            "bl printf\n\t"
            "mov r0, %1\n\t"
            "vmov r1, r5, d31\n\t"
            "mov r3, r1\n\t"
            "bl printf\n\t"
            "mov r0, %1\n\t"
            "mov r1, r5\n\t"
            "mov r3, r1\n\t"
            "bl printf\n\t"
/* --- */
            "mov r0, #0x55\n\t"
            "vmov s20, r0\n\t"
            "mov r0, #0x56\n\t"
            "vmov s21, r0\n\t"
            "mov r0, #0x57\n\t"
            "vmov s22, r0\n\t"
            "mov r0, #0x58\n\t"
            "vmov s23, r0\n\t"
            "add r1, %0, #0\n\t"
            "vstmia r1!, {s20, s21, s22, s23}\n\t"
            "mov r0, %1\n\t"
            "sub r1, r1, %0\n\t"
            "mov r3, r1\n\t"
            "bl printf\n\t"
/* --- */
            "mov r0, #0x65\n\t"
            "vmov s16, r0\n\t"
            "mov r0, #0x66\n\t"
            "vmov s17, r0\n\t"
            "add r1, %0, #16\n\t"
            "vstmia r1, {s16, s17}\n\t"
            "mov r0, %1\n\t"
            "sub r1, r1, %0\n\t"
            "mov r3, r1\n\t"
            "bl printf\n\t"
/* --- */
            "mov r0, #0x75\n\t"
            "vmov s16, r0\n\t"
            "mov r0, #0x76\n\t"
            "vmov s17, r0\n\t"
            "add r1, %0, #32\n\t"
            "vstmdb r1!, {s16, s17}\n\t"
            "mov r0, %1\n\t"
            "sub r1, r1, %0\n\t"
            "mov r3, r1\n\t"
            "bl printf\n\t"
/* --- */
            "mov r0, #0x42\n\t"
            "mov r1, #0x43\n\t"
            "vmov d30, r0, r1\n\t"
            "mov r0, #0x40\n\t"
            "mov r1, #0x41\n\t"
            "vmov d31, r0, r1\n\t"
            "mov r0, #0x57\n\t"
            "add r1, %0, #32\n\t"
            "vstmia r1!, {d30, d31}\n\t"
            "mov r0, %1\n\t"
            "sub r1, r1, %0\n\t"
            "mov r3, r1\n\t"
            "bl printf\n\t"
/* --- */
            "mov r0, #0x32\n\t"
            "mov r1, #0x33\n\t"
            "vmov d10, r0, r1\n\t"
            "mov r0, #0x57\n\t"
            "add r1, %0, #48\n\t"
            "vstmia r1, {d10}\n\t"
            "mov r0, %1\n\t"
            "sub r1, r1, %0\n\t"
            "mov r3, r1\n\t"
            "bl printf\n\t"
/* --- */
            "mov r0, #0x22\n\t"
            "mov r1, #0x23\n\t"
            "vmov d10, r0, r1\n\t"
            "mov r0, #0x57\n\t"
            "add r1, %0, #64\n\t"
            "vstmdb r1!, {d10}\n\t"
            "mov r0, %1\n\t"
            "sub r1, r1, %0\n\t"
            "mov r3, r1\n\t"
            "bl printf\n\t"
        :
        : "r" (data), "r" (format), "r"(&res)
        : "r0", "r1", "r2", "r3", "r5", "r12", "r14", "memory",
          "s0", "s1", "s2", "s3", "s5", "s6", "s16", "s17", "s18", "s19",
          "s20", "s21", "s22", "s23", "s25", "s26",
          "d10", "d30", "d31"
        );
    printf("data:\n");
    for (i = 0; i < 16; i++) {
        printf("\t0x%08x\n", data[i]);
    }
}

int main(int argc, char **argv)
{
    do_vldm_vstm_check();

    printf("---- VMOV (ARM core register to scalar) ----\n");
    TESTINSN_core_to_scalar("vmov.32 d0[0],  r5", d0,  r5, f2u(13));
    TESTINSN_core_to_scalar("vmov.32 d1[1],  r6", d1,  r6, 0x12);
    TESTINSN_core_to_scalar("vmov.32 d20[0], r5", d20, r5, f2u(NAN));
    TESTINSN_core_to_scalar("vmov.32 d29[1], r6", d29, r6, f2u(172));
    TESTINSN_core_to_scalar("vmov.32 d30[0], r5", d30, r5, f2u(INFINITY));
    TESTINSN_core_to_scalar("vmov.32 d11[1], r6", d11, r6, f2u(-INFINITY));
    TESTINSN_core_to_scalar("vmov.32 d18[0], r5", d11, r5, f2u(653));
    TESTINSN_core_to_scalar("vmov.32 d9[1],  r6", d9,  r6, 12);
    TESTINSN_core_to_scalar("vmov.16 d0[0],  r5", d0,  r5, 13);
    TESTINSN_core_to_scalar("vmov.16 d14[1], r5", d14, r5, f2u(NAN));
    TESTINSN_core_to_scalar("vmov.16 d28[2], r6", d28, r6, 14);
    TESTINSN_core_to_scalar("vmov.16 d30[3], r1", d30, r1, 17);
    TESTINSN_core_to_scalar("vmov.16 d0[0],  r5", d0,  r5, f2u(INFINITY));
    TESTINSN_core_to_scalar("vmov.16 d7[1],  r5", d7,  r5, f2u(-INFINITY));
    TESTINSN_core_to_scalar("vmov.16 d21[2], r6", d21, r6, 14);
    TESTINSN_core_to_scalar("vmov.16 d17[3], r1", d17, r1, 17);
    TESTINSN_core_to_scalar("vmov.8  d0[0],  r5", d0,  r5, 13);
    TESTINSN_core_to_scalar("vmov.8  d10[1], r5", d10, r5, f2u(NAN));
    TESTINSN_core_to_scalar("vmov.8  d20[2], r5", d20, r5, f2u(INFINITY));
    TESTINSN_core_to_scalar("vmov.8  d30[3], r5", d30, r5, f2u(-INFINITY));
    TESTINSN_core_to_scalar("vmov.8  d13[4], r5", d13, r5, 213);
    TESTINSN_core_to_scalar("vmov.8  d17[5], r5", d17, r5, 1343);
    TESTINSN_core_to_scalar("vmov.8  d24[6], r5", d24, r5, 111);
    TESTINSN_core_to_scalar("vmov.8  d29[7], r5", d29, r5, 173);

    printf("---- VMOV (scalar to ARM core register) ----\n");
    TESTINSN_scalar_to_core("vmov.32   r5,  d0[0]",  r5,  d0,  i32, f2u(NAN));
    TESTINSN_scalar_to_core("vmov.32   r6,  d5[1]",  r6,  d5,  i32, f2u(INFINITY));
    TESTINSN_scalar_to_core("vmov.32   r4,  d10[0]", r4,  d10, i32, f2u(-INFINITY));
    TESTINSN_scalar_to_core("vmov.32   r5,  d15[1]", r5,  d15, i32, 0x11223344);
    TESTINSN_scalar_to_core("vmov.32   r9, d20[0]", r9, d20, i32, 0x11223344);
    TESTINSN_scalar_to_core("vmov.32   r8,  d25[1]", r8,  d25, i32, 0x11223344);
    TESTINSN_scalar_to_core("vmov.32   r0,  d30[0]", r0,  d30, i32, 0x11223344);
    TESTINSN_scalar_to_core("vmov.32   r2,  d19[1]", r2,  d19, i32, 0x11223344);
    TESTINSN_scalar_to_core("vmov.u16  r5,  d31[0]", r5,  d31, i32, f2u(NAN));
    TESTINSN_scalar_to_core("vmov.u16  r3,  d30[1]", r3,  d30, i32, f2u(INFINITY));
    TESTINSN_scalar_to_core("vmov.u16  r6,  d21[2]", r6,  d21, i32, f2u(-INFINITY));
    TESTINSN_scalar_to_core("vmov.u16  r9, d26[3]", r9, d26, i32, 0x11223344);
    TESTINSN_scalar_to_core("vmov.u16  r12, d11[0]", r12, d11, i32, 0x11223344);
    TESTINSN_scalar_to_core("vmov.u16  r0,  d10[1]", r0,  d10, i32, 0x11223344);
    TESTINSN_scalar_to_core("vmov.u16  r6,  d1[2]",  r6,  d1,  i32, 0x11223344);
    TESTINSN_scalar_to_core("vmov.u16  r8,  d5[3]",  r8,  d5,  i32, 0x11223344);
    TESTINSN_scalar_to_core("vmov.u8   r2,  d4[0]",  r2,  d4,  i32, f2u(NAN));
    TESTINSN_scalar_to_core("vmov.u8   r6,  d14[1]", r6,  d14, i32, f2u(INFINITY));
    TESTINSN_scalar_to_core("vmov.u8   r9, d24[2]", r9, d24, i32, f2u(-INFINITY));
    TESTINSN_scalar_to_core("vmov.u8   r8,  d31[3]", r8,  d31, i32, 0x11223344);
    TESTINSN_scalar_to_core("vmov.u8   r10, d29[4]", r10, d29, i32, 0x11223344);
    TESTINSN_scalar_to_core("vmov.u8   r3,  d19[5]", r3,  d19, i32, 0x11223344);
    TESTINSN_scalar_to_core("vmov.u8   r12, d12[6]", r12, d12, i32, 0x11223344);
    TESTINSN_scalar_to_core("vmov.u8   r10, d18[4]", r10, d18, i32, 0x11223344);
    TESTINSN_scalar_to_core("vmov.s16  r5,  d31[0]", r5,  d31, i32, f2u(NAN));
    TESTINSN_scalar_to_core("vmov.s16  r3,  d30[1]", r3,  d30, i32, f2u(INFINITY));
    TESTINSN_scalar_to_core("vmov.s16  r6,  d21[2]", r6,  d21, i32, f2u(-INFINITY));
    TESTINSN_scalar_to_core("vmov.s16  r9, d26[3]", r9, d26, i32, 0x11223344);
    TESTINSN_scalar_to_core("vmov.s16  r4,  d11[0]", r4,  d11, i32, 0x11223344);
    TESTINSN_scalar_to_core("vmov.s16  r0,  d10[1]", r0,  d10, i32, 0x11223344);
    TESTINSN_scalar_to_core("vmov.s16  r6,  d1[2]",  r6,  d1,  i32, 0x11223344);
    TESTINSN_scalar_to_core("vmov.s16  r8,  d5[3]",  r8,  d5,  i32, 0x11223344);
    TESTINSN_scalar_to_core("vmov.s8   r2,  d4[0]",  r2,  d4,  i32, f2u(NAN));
    TESTINSN_scalar_to_core("vmov.s8   r6,  d14[1]", r6,  d14, i32, f2u(INFINITY));
    TESTINSN_scalar_to_core("vmov.s8   r9, d24[2]", r9, d24, i32, f2u(-INFINITY));
    TESTINSN_scalar_to_core("vmov.s8   r8,  d31[3]", r8,  d31, i32, 0x11223344);
    TESTINSN_scalar_to_core("vmov.s8   r6,  d29[4]", r6,  d29, i32, 0x11223344);
    TESTINSN_scalar_to_core("vmov.s8   r3,  d19[5]", r3,  d19, i32, 0x11223344);
    TESTINSN_scalar_to_core("vmov.s8   r12, d12[6]", r12, d12, i32, 0x11223344);
    TESTINSN_scalar_to_core("vmov.s8   r10, d18[7]", r10, d18, i32, 0x11223344);

    printf("---- VMLA (fp) ----\n");
    TESTINSN_bin_f64("vmla.f64 d0,  d11, d12", d0,  d11, i32, f2u0(-INFINITY), f2u1(-INFINITY), d12, i32, f2u0(NAN), f2u1(NAN));
    TESTINSN_bin_f64("vmla.f64 d7,  d1,  d6",  d7,  d1,  i32, f2u0(INFINITY), f2u1(INFINITY), d6, i32, f2u0(NAN), f2u1(NAN));
    TESTINSN_bin_f64("vmla.f64 d0,  d5,  d2",  d0,  d5,  i32, f2u0(NAN), f2u1(NAN), d2, i32, f2u0(-1.0), f2u1(-1.0));
    TESTINSN_bin_f64("vmla.f64 d10, d13, d15", d10, d13, i32, f2u0(NAN), f2u1(NAN), d15, i32, f2u0(0.0), f2u1(0.0));
    TESTINSN_bin_f64("vmla.f64 d10, d13, d15", d10, d13, i32, f2u0(NAN), f2u1(NAN), d15, i32, f2u0(NAN), f2u1(NAN));
    TESTINSN_bin_f64("vmla.f64 d20, d25, d22", d20, d25, i32, f2u0(23.04), f2u1(23.04), d22, i32, f2u0(-45.5687), f2u1(-45.5687));
    TESTINSN_bin_f64("vmla.f64 d23, d24, d25", d23, d24, i32, f2u0(-347856.475), f2u1(-347856.475), d25, i32, f2u0(1346), f2u1(1346));
    TESTINSN_bin_f64("vmla.f64 d20, d31, d12", d20, d31, i32, f2u0(48755), f2u1(48755), d12, i32, f2u0(-45786.476), f2u1(-45786.476));
    TESTINSN_bin_f64("vmla.f64 d19, d25, d27", d19, d25, i32, f2u0(95867.76), f2u1(95867.76), d27, i32, f2u0(17065), f2u1(17065));
    TESTINSN_bin_f64("vmla.f64 d30, d15, d2",  d30, d15, i32, f2u0(-45667.24), f2u1(-45667.24), d2, i32, f2u0(-248562.76), f2u1(-248562.76));
    TESTINSN_bin_f64("vmla.f64 d23, d24, d5",  d23, d24, i32, f2u0(24), f2u1(24), d5, i32, f2u0(1346), f2u1(1346));
    TESTINSN_bin_f64("vmla.f64 d10, d11, d2",  d10, d11, i32, f2u0(48755), f2u1(48755), d2, i32, f2u0(1089), f2u1(1089));
    TESTINSN_bin_f64("vmla.f64 d29, d15, d7",  d29, d15, i32, f2u0(214), f2u1(214), d7, i32, f2u0(1752065), f2u1(1752065));
    TESTINSN_bin_f64("vmla.f64 d30, d11, d12", d30, d11, i32, f2u0(356047.56), f2u1(356047.56), d12, i32, f2u0(5867.009), f2u1(5867.009));
    TESTINSN_bin_f64("vmla.f64 d27, d21, d6",  d27, d21, i32, f2u0(34.00046), f2u1(34.00046), d6, i32, f2u0(0.0024575), f2u1(0.0024575));
    TESTINSN_bin_f64("vmla.f64 d30, d31, d2",  d30, d31, i32, f2u0(2754), f2u1(2754), d2, i32, f2u0(107), f2u1(107));
    TESTINSN_bin_f64("vmla.f64 d13, d24, d5",  d13, d24, i32, f2u0(874), f2u1(874), d5, i32, f2u0(1384.6), f2u1(1384.6));
    TESTINSN_bin_f64("vmla.f64 d10, d11, d2",  d10, d11, i32, f2u0(487.587), f2u1(487.587), d2, i32, f2u0(109), f2u1(109));
    TESTINSN_bin_f64("vmla.f64 d29, d25, d7",  d29, d25, i32, f2u0(-INFINITY), f2u1(-INFINITY), d7, i32, f2u0(1752), f2u1(1752));
    TESTINSN_bin_f64("vmla.f64 d0,  d11, d12", d0,  d11, i32, f2u0(INFINITY), f2u1(INFINITY), d12, i32, f2u0(-5786.47), f2u1(-5786.47));
    TESTINSN_bin_f64("vmla.f64 d27, d21, d16", d27, d21, i32, f2u0(456.2489562), f2u1(456.2489562), d16, i32, f2u0(-7.2945676), f2u1(-7.2945676));
    TESTINSN_bin_f64("vmla.f64 d0,  d5,  d2",  d0,  d5,  i32, f2u0(INFINITY), f2u1(INFINITY), d2, i32, f2u0(-INFINITY), f2u1(-INFINITY));
    TESTINSN_bin_f64("vmla.f64 d20, d13, d15", d20, d13, i32, f2u0(-INFINITY), f2u1(-INFINITY), d15, i32, f2u0(0.0), f2u1(0.0));
    TESTINSN_bin_f64("vmla.f64 d10, d23, d15", d10, d23, i32, f2u0(INFINITY), f2u1(INFINITY), d15, i32, f2u0(0.0), f2u1(0.0));
    TESTINSN_bin_f32("vmla.f32 s0,  s11, s12", s0,  s11, i32, f2u(-INFINITY), s12, i32, f2u(NAN));
    TESTINSN_bin_f32("vmla.f32 s7,  s1,  s6",  s7,  s1,  i32, f2u(INFINITY), s6, i32, f2u(NAN));
    TESTINSN_bin_f32("vmla.f32 s0,  s5,  s2",  s0,  s5,  i32, f2u(NAN), s2, i32, f2u(-1.0));
    TESTINSN_bin_f32("vmla.f32 s10, s13, s15", s10, s13, i32, f2u(NAN), s15, i32, f2u(0.0));
    TESTINSN_bin_f32("vmla.f32 s10, s13, s15", s10, s13, i32, f2u(NAN), s15, i32, f2u(NAN));
    TESTINSN_bin_f32("vmla.f32 s20, s25, s22", s20, s25, i32, f2u(23.04), s22, i32, f2u(-45.5687));
    TESTINSN_bin_f32("vmla.f32 s23, s24, s25", s23, s24, i32, f2u(-347856.475), s25, i32, f2u(1346));
    TESTINSN_bin_f32("vmla.f32 s20, s31, s12", s20, s31, i32, f2u(48755), s12, i32, f2u(-45786.476));
    TESTINSN_bin_f32("vmla.f32 s19, s25, s27", s19, s25, i32, f2u(95867.76), s27, i32, f2u(17065));
    TESTINSN_bin_f32("vmla.f32 s30, s15, s2",  s30, s15, i32, f2u(-45667.24), s2, i32, f2u(-248562.76));
    TESTINSN_bin_f32("vmla.f32 s23, s24, s5",  s23, s24, i32, f2u(24), s5, i32, f2u(1346));
    TESTINSN_bin_f32("vmla.f32 s10, s11, s2",  s10, s11, i32, f2u(48755), s2, i32, f2u(1089));
    TESTINSN_bin_f32("vmla.f32 s29, s15, s7",  s29, s15, i32, f2u(214), s7, i32, f2u(1752065));
    TESTINSN_bin_f32("vmla.f32 s30, s11, s12", s30, s11, i32, f2u(356047.56), s12, i32, f2u(5867.009));
    TESTINSN_bin_f32("vmla.f32 s27, s21, s6",  s27, s21, i32, f2u(34.00046), s6, i32, f2u(0.0024575));
    TESTINSN_bin_f32("vmla.f32 s30, s31, s2",  s30, s31, i32, f2u(2754), s2, i32, f2u(107));
    TESTINSN_bin_f32("vmla.f32 s13, s24, s5",  s13, s24, i32, f2u(874), s5, i32, f2u(1384.6));
    TESTINSN_bin_f32("vmla.f32 s10, s11, s2",  s10, s11, i32, f2u(487.587), s2, i32, f2u(109));
    TESTINSN_bin_f32("vmla.f32 s29, s25, s7",  s29, s25, i32, f2u(-INFINITY), s7, i32, f2u(1752));
    TESTINSN_bin_f32("vmla.f32 s0,  s11, s12", s0,  s11, i32, f2u(INFINITY), s12, i32, f2u(-5786.47));
    TESTINSN_bin_f32("vmla.f32 s27, s21, s16", s27, s21, i32, f2u(456.2489562), s16, i32, f2u(-7.2945676));
    TESTINSN_bin_f32("vmla.f32 s0,  s5,  s2",  s0,  s5,  i32, f2u(INFINITY), s2, i32, f2u(-INFINITY));
    TESTINSN_bin_f32("vmla.f32 s20, s13, s15", s20, s13, i32, f2u(-INFINITY), s15, i32, f2u(0.0));
    TESTINSN_bin_f32("vmla.f32 s10, s23, s15", s10, s23, i32, f2u(INFINITY), s15, i32, f2u(0.0));

    printf("---- VNMLA (fp) ----\n");
    TESTINSN_bin_f64("vnmla.f64 d0,  d11, d12", d0,  d11, i32, f2u0(-INFINITY), f2u1(-INFINITY), d12, i32, f2u0(NAN), f2u1(NAN));
    TESTINSN_bin_f64("vnmla.f64 d7,  d1,  d6",  d7,  d1,  i32, f2u0(INFINITY), f2u1(INFINITY), d6, i32, f2u0(NAN), f2u1(NAN));
    TESTINSN_bin_f64("vnmla.f64 d0,  d5,  d2",  d0,  d5,  i32, f2u0(NAN), f2u1(NAN), d2, i32, f2u0(-1.0), f2u1(-1.0));
    TESTINSN_bin_f64("vnmla.f64 d10, d13, d15", d10, d13, i32, f2u0(NAN), f2u1(NAN), d15, i32, f2u0(0.0), f2u1(0.0));
    TESTINSN_bin_f64("vnmla.f64 d10, d13, d15", d10, d13, i32, f2u0(NAN), f2u1(NAN), d15, i32, f2u0(NAN), f2u1(NAN));
    TESTINSN_bin_f64("vnmla.f64 d20, d25, d22", d20, d25, i32, f2u0(23.04), f2u1(23.04), d22, i32, f2u0(-45.5687), f2u1(-45.5687));
    TESTINSN_bin_f64("vnmla.f64 d23, d24, d25", d23, d24, i32, f2u0(-347856.475), f2u1(-347856.475), d25, i32, f2u0(1346), f2u1(1346));
    TESTINSN_bin_f64("vnmla.f64 d20, d31, d12", d20, d31, i32, f2u0(48755), f2u1(48755), d12, i32, f2u0(-45786.476), f2u1(-45786.476));
    TESTINSN_bin_f64("vnmla.f64 d19, d25, d27", d19, d25, i32, f2u0(95867.76), f2u1(95867.76), d27, i32, f2u0(17065), f2u1(17065));
    TESTINSN_bin_f64("vnmla.f64 d30, d15, d2",  d30, d15, i32, f2u0(-45667.24), f2u1(-45667.24), d2, i32, f2u0(-248562.76), f2u1(-248562.76));
    TESTINSN_bin_f64("vnmla.f64 d23, d24, d5",  d23, d24, i32, f2u0(24), f2u1(24), d5, i32, f2u0(1346), f2u1(1346));
    TESTINSN_bin_f64("vnmla.f64 d10, d11, d2",  d10, d11, i32, f2u0(48755), f2u1(48755), d2, i32, f2u0(1089), f2u1(1089));
    TESTINSN_bin_f64("vnmla.f64 d29, d15, d7",  d29, d15, i32, f2u0(214), f2u1(214), d7, i32, f2u0(1752065), f2u1(1752065));
    TESTINSN_bin_f64("vnmla.f64 d30, d11, d12", d30, d11, i32, f2u0(356047.56), f2u1(356047.56), d12, i32, f2u0(5867.009), f2u1(5867.009));
    TESTINSN_bin_f64("vnmla.f64 d27, d21, d6",  d27, d21, i32, f2u0(34.00046), f2u1(34.00046), d6, i32, f2u0(0.0024575), f2u1(0.0024575));
    TESTINSN_bin_f64("vnmla.f64 d30, d31, d2",  d30, d31, i32, f2u0(2754), f2u1(2754), d2, i32, f2u0(107), f2u1(107));
    TESTINSN_bin_f64("vnmla.f64 d13, d24, d5",  d13, d24, i32, f2u0(874), f2u1(874), d5, i32, f2u0(1384.6), f2u1(1384.6));
    TESTINSN_bin_f64("vnmla.f64 d10, d11, d2",  d10, d11, i32, f2u0(487.587), f2u1(487.587), d2, i32, f2u0(109), f2u1(109));
    TESTINSN_bin_f64("vnmla.f64 d29, d25, d7",  d29, d25, i32, f2u0(-INFINITY), f2u1(-INFINITY), d7, i32, f2u0(1752), f2u1(1752));
    TESTINSN_bin_f64("vnmla.f64 d0,  d11, d12", d0,  d11, i32, f2u0(INFINITY), f2u1(INFINITY), d12, i32, f2u0(-5786.47), f2u1(-5786.47));
    TESTINSN_bin_f64("vnmla.f64 d27, d21, d16", d27, d21, i32, f2u0(456.2489562), f2u1(456.2489562), d16, i32, f2u0(-7.2945676), f2u1(-7.2945676));
    TESTINSN_bin_f64("vnmla.f64 d0,  d5,  d2",  d0,  d5,  i32, f2u0(INFINITY), f2u1(INFINITY), d2, i32, f2u0(-INFINITY), f2u1(-INFINITY));
    TESTINSN_bin_f64("vnmla.f64 d20, d13, d15", d20, d13, i32, f2u0(-INFINITY), f2u1(-INFINITY), d15, i32, f2u0(0.0), f2u1(0.0));
    TESTINSN_bin_f64("vnmla.f64 d10, d23, d15", d10, d23, i32, f2u0(INFINITY), f2u1(INFINITY), d15, i32, f2u0(0.0), f2u1(0.0));
    TESTINSN_bin_f32("vnmla.f32 s0,  s11, s12", s0,  s11, i32, f2u(-INFINITY), s12, i32, f2u(NAN));
    TESTINSN_bin_f32("vnmla.f32 s7,  s1,  s6",  s7,  s1,  i32, f2u(INFINITY), s6, i32, f2u(NAN));
    TESTINSN_bin_f32("vnmla.f32 s0,  s5,  s2",  s0,  s5,  i32, f2u(NAN), s2, i32, f2u(-1.0));
    TESTINSN_bin_f32("vnmla.f32 s10, s13, s15", s10, s13, i32, f2u(NAN), s15, i32, f2u(0.0));
    TESTINSN_bin_f32("vnmla.f32 s10, s13, s15", s10, s13, i32, f2u(NAN), s15, i32, f2u(NAN));
    TESTINSN_bin_f32("vnmla.f32 s20, s25, s22", s20, s25, i32, f2u(23.04), s22, i32, f2u(-45.5687));
    TESTINSN_bin_f32("vnmla.f32 s23, s24, s25", s23, s24, i32, f2u(-347856.475), s25, i32, f2u(1346));
    TESTINSN_bin_f32("vnmla.f32 s20, s31, s12", s20, s31, i32, f2u(48755), s12, i32, f2u(-45786.476));
    TESTINSN_bin_f32("vnmla.f32 s19, s25, s27", s19, s25, i32, f2u(95867.76), s27, i32, f2u(17065));
    TESTINSN_bin_f32("vnmla.f32 s30, s15, s2",  s30, s15, i32, f2u(-45667.24), s2, i32, f2u(-248562.76));
    TESTINSN_bin_f32("vnmla.f32 s23, s24, s5",  s23, s24, i32, f2u(24), s5, i32, f2u(1346));
    TESTINSN_bin_f32("vnmla.f32 s10, s11, s2",  s10, s11, i32, f2u(48755), s2, i32, f2u(1089));
    TESTINSN_bin_f32("vnmla.f32 s29, s15, s7",  s29, s15, i32, f2u(214), s7, i32, f2u(1752065));
    TESTINSN_bin_f32("vnmla.f32 s30, s11, s12", s30, s11, i32, f2u(356047.56), s12, i32, f2u(5867.009));
    TESTINSN_bin_f32("vnmla.f32 s27, s21, s6",  s27, s21, i32, f2u(34.00046), s6, i32, f2u(0.0024575));
    TESTINSN_bin_f32("vnmla.f32 s30, s31, s2",  s30, s31, i32, f2u(2754), s2, i32, f2u(107));
    TESTINSN_bin_f32("vnmla.f32 s13, s24, s5",  s13, s24, i32, f2u(874), s5, i32, f2u(1384.6));
    TESTINSN_bin_f32("vnmla.f32 s10, s11, s2",  s10, s11, i32, f2u(487.587), s2, i32, f2u(109));
    TESTINSN_bin_f32("vnmla.f32 s29, s25, s7",  s29, s25, i32, f2u(-INFINITY), s7, i32, f2u(1752.));
    TESTINSN_bin_f32("vnmla.f32 s0,  s11, s12", s0,  s11, i32, f2u(INFINITY), s12, i32, f2u(-5786.47));
    TESTINSN_bin_f32("vnmla.f32 s27, s21, s16", s27, s21, i32, f2u(456.2489562), s16, i32, f2u(-7.2945676));
    TESTINSN_bin_f32("vnmla.f32 s0,  s5,  s2",  s0,  s5,  i32, f2u(INFINITY), s2, i32, f2u(-INFINITY));
    TESTINSN_bin_f32("vnmla.f32 s20, s13, s15", s20, s13, i32, f2u(-INFINITY), s15, i32, f2u(0.0));
    TESTINSN_bin_f32("vnmla.f32 s10, s23, s15", s10, s23, i32, f2u(INFINITY), s15, i32, f2u(0.0));

    printf("---- VMLS (fp) ----\n");
    TESTINSN_bin_f64("vmls.f64 d0,  d11, d12", d0,  d11, i32, f2u0(-INFINITY), f2u1(-INFINITY), d12, i32, f2u0(NAN), f2u1(NAN));
    TESTINSN_bin_f64("vmls.f64 d7,  d1,  d6",  d7,  d1,  i32, f2u0(INFINITY), f2u1(INFINITY), d6, i32, f2u0(NAN), f2u1(NAN));
    TESTINSN_bin_f64("vmls.f64 d0,  d5,  d2",  d0,  d5,  i32, f2u0(NAN), f2u1(NAN), d2, i32, f2u0(-1.0), f2u1(-1.0));
    TESTINSN_bin_f64("vmls.f64 d10, d13, d15", d10, d13, i32, f2u0(NAN), f2u1(NAN), d15, i32, f2u0(0.0), f2u1(0.0));
    TESTINSN_bin_f64("vmls.f64 d10, d13, d15", d10, d13, i32, f2u0(NAN), f2u1(NAN), d15, i32, f2u0(NAN), f2u1(NAN));
    TESTINSN_bin_f64("vmls.f64 d20, d25, d22", d20, d25, i32, f2u0(23.04), f2u1(23.04), d22, i32, f2u0(-45.5687), f2u1(-45.5687));
    TESTINSN_bin_f64("vmls.f64 d23, d24, d25", d23, d24, i32, f2u0(-347856.475), f2u1(-347856.475), d25, i32, f2u0(1346), f2u1(1346));
    TESTINSN_bin_f64("vmls.f64 d20, d31, d12", d20, d31, i32, f2u0(48755), f2u1(48755), d12, i32, f2u0(-45786.476), f2u1(-45786.476));
    TESTINSN_bin_f64("vmls.f64 d19, d25, d27", d19, d25, i32, f2u0(95867.76), f2u1(95867.76), d27, i32, f2u0(17065), f2u1(17065));
    TESTINSN_bin_f64("vmls.f64 d30, d15, d2",  d30, d15, i32, f2u0(-45667.24), f2u1(-45667.24), d2, i32, f2u0(-248562.76), f2u1(-248562.76));
    TESTINSN_bin_f64("vmls.f64 d23, d24, d5",  d23, d24, i32, f2u0(24), f2u1(24), d5, i32, f2u0(1346), f2u1(1346));
    TESTINSN_bin_f64("vmls.f64 d10, d11, d2",  d10, d11, i32, f2u0(48755), f2u1(48755), d2, i32, f2u0(1089), f2u1(1089));
    TESTINSN_bin_f64("vmls.f64 d29, d15, d7",  d29, d15, i32, f2u0(214), f2u1(214), d7, i32, f2u0(1752065), f2u1(1752065));
    TESTINSN_bin_f64("vmls.f64 d30, d11, d12", d30, d11, i32, f2u0(356047.56), f2u1(356047.56), d12, i32, f2u0(5867.009), f2u1(5867.009));
    TESTINSN_bin_f64("vmls.f64 d27, d21, d6",  d27, d21, i32, f2u0(34.00046), f2u1(34.00046), d6, i32, f2u0(0.0024575), f2u1(0.0024575));
    TESTINSN_bin_f64("vmls.f64 d30, d31, d2",  d30, d31, i32, f2u0(2754), f2u1(2754), d2, i32, f2u0(107), f2u1(107));
    TESTINSN_bin_f64("vmls.f64 d13, d24, d5",  d13, d24, i32, f2u0(874), f2u1(874), d5, i32, f2u0(1384.6), f2u1(1384.6));
    TESTINSN_bin_f64("vmls.f64 d10, d11, d2",  d10, d11, i32, f2u0(487.587), f2u1(487.587), d2, i32, f2u0(109), f2u1(109));
    TESTINSN_bin_f64("vmls.f64 d29, d25, d7",  d29, d25, i32, f2u0(-INFINITY), f2u1(-INFINITY), d7, i32, f2u0(1752), f2u1(1752));
    TESTINSN_bin_f64("vmls.f64 d0,  d11, d12", d0,  d11, i32, f2u0(INFINITY), f2u1(INFINITY), d12, i32, f2u0(-5786.47), f2u1(-5786.47));
    TESTINSN_bin_f64("vmls.f64 d27, d21, d16", d27, d21, i32, f2u0(456.2489562), f2u1(456.2489562), d16, i32, f2u0(-7.2945676), f2u1(-7.2945676));
    TESTINSN_bin_f64("vmls.f64 d0,  d5,  d2",  d0,  d5,  i32, f2u0(INFINITY), f2u1(INFINITY), d2, i32, f2u0(-INFINITY), f2u1(-INFINITY));
    TESTINSN_bin_f64("vmls.f64 d20, d13, d15", d20, d13, i32, f2u0(-INFINITY), f2u1(-INFINITY), d15, i32, f2u0(0.0), f2u1(0.0));
    TESTINSN_bin_f64("vmls.f64 d10, d23, d15", d10, d23, i32, f2u0(INFINITY), f2u1(INFINITY), d15, i32, f2u0(0.0), f2u1(0.0));
    TESTINSN_bin_f32("vmls.f32 s0,  s11, s12", s0,  s11, i32, f2u(-INFINITY), s12, i32, f2u(NAN));
    TESTINSN_bin_f32("vmls.f32 s7,  s1,  s6",  s7,  s1,  i32, f2u(INFINITY), s6, i32, f2u(NAN));
    TESTINSN_bin_f32("vmls.f32 s0,  s5,  s2",  s0,  s5,  i32, f2u(NAN), s2, i32, f2u(-1.0));
    TESTINSN_bin_f32("vmls.f32 s10, s13, s15", s10, s13, i32, f2u(NAN), s15, i32, f2u(0.0));
    TESTINSN_bin_f32("vmls.f32 s10, s13, s15", s10, s13, i32, f2u(NAN), s15, i32, f2u(NAN));
    TESTINSN_bin_f32("vmls.f32 s20, s25, s22", s20, s25, i32, f2u(23.04), s22, i32, f2u(-45.5687));
    TESTINSN_bin_f32("vmls.f32 s23, s24, s25", s23, s24, i32, f2u(-347856.475), s25, i32, f2u(1346));
    TESTINSN_bin_f32("vmls.f32 s20, s31, s12", s20, s31, i32, f2u(48755), s12, i32, f2u(-45786.476));
    TESTINSN_bin_f32("vmls.f32 s19, s25, s27", s19, s25, i32, f2u(95867.76), s27, i32, f2u(17065));
    TESTINSN_bin_f32("vmls.f32 s30, s15, s2",  s30, s15, i32, f2u(-45667.24), s2, i32, f2u(-248562.76));
    TESTINSN_bin_f32("vmls.f32 s23, s24, s5",  s23, s24, i32, f2u(24), s5, i32, f2u(1346));
    TESTINSN_bin_f32("vmls.f32 s10, s11, s2",  s10, s11, i32, f2u(48755), s2, i32, f2u(1089));
    TESTINSN_bin_f32("vmls.f32 s29, s15, s7",  s29, s15, i32, f2u(214), s7, i32, f2u(1752065));
    TESTINSN_bin_f32("vmls.f32 s30, s11, s12", s30, s11, i32, f2u(356047.56), s12, i32, f2u(5867.009));
    TESTINSN_bin_f32("vmls.f32 s27, s21, s6",  s27, s21, i32, f2u(34.00046), s6, i32, f2u(0.0024575));
    TESTINSN_bin_f32("vmls.f32 s30, s31, s2",  s30, s31, i32, f2u(2754), s2, i32, f2u(107));
    TESTINSN_bin_f32("vmls.f32 s13, s24, s5",  s13, s24, i32, f2u(874), s5, i32, f2u(1384.6));
    TESTINSN_bin_f32("vmls.f32 s10, s11, s2",  s10, s11, i32, f2u(487.587), s2, i32, f2u(109));
    TESTINSN_bin_f32("vmls.f32 s29, s25, s7",  s29, s25, i32, f2u(-INFINITY), s7, i32, f2u(1752));
    TESTINSN_bin_f32("vmls.f32 s0,  s11, s12", s0,  s11, i32, f2u(INFINITY), s12, i32, f2u(-5786.47));
    TESTINSN_bin_f32("vmls.f32 s27, s21, s16", s27, s21, i32, f2u(456.2489562), s16, i32, f2u(-7.2945676));
    TESTINSN_bin_f32("vmls.f32 s0,  s5,  s2",  s0,  s5,  i32, f2u(INFINITY), s2, i32, f2u(-INFINITY));
    TESTINSN_bin_f32("vmls.f32 s20, s13, s15", s20, s13, i32, f2u(-INFINITY), s15, i32, f2u(0.0));
    TESTINSN_bin_f32("vmls.f32 s10, s23, s15", s10, s23, i32, f2u(INFINITY), s15, i32, f2u(0.0));

    printf("---- VNMLS (fp) ----\n");
    TESTINSN_bin_f64("vnmls.f64 d0,  d11, d12", d0,  d11, i32, f2u0(-INFINITY), f2u1(-INFINITY), d12, i32, f2u0(NAN), f2u1(NAN));
    TESTINSN_bin_f64("vnmls.f64 d7,  d1,  d6",  d7,  d1,  i32, f2u0(INFINITY), f2u1(INFINITY), d6, i32, f2u0(NAN), f2u1(NAN));
    TESTINSN_bin_f64("vnmls.f64 d0,  d5,  d2",  d0,  d5,  i32, f2u0(NAN), f2u1(NAN), d2, i32, f2u0(-1.0), f2u1(-1.0));
    TESTINSN_bin_f64("vnmls.f64 d10, d13, d15", d10, d13, i32, f2u0(NAN), f2u1(NAN), d15, i32, f2u0(0.0), f2u1(0.0));
    TESTINSN_bin_f64("vnmls.f64 d10, d13, d15", d10, d13, i32, f2u0(NAN), f2u1(NAN), d15, i32, f2u0(NAN), f2u1(NAN));
    TESTINSN_bin_f64("vnmls.f64 d20, d25, d22", d20, d25, i32, f2u0(23.04), f2u1(23.04), d22, i32, f2u0(-45.5687), f2u1(-45.5687));
    TESTINSN_bin_f64("vnmls.f64 d23, d24, d25", d23, d24, i32, f2u0(-347856.475), f2u1(-347856.475), d25, i32, f2u0(1346), f2u1(1346));
    TESTINSN_bin_f64("vnmls.f64 d20, d31, d12", d20, d31, i32, f2u0(48755), f2u1(48755), d12, i32, f2u0(-45786.476), f2u1(-45786.476));
    TESTINSN_bin_f64("vnmls.f64 d19, d25, d27", d19, d25, i32, f2u0(95867.76), f2u1(95867.76), d27, i32, f2u0(17065), f2u1(17065));
    TESTINSN_bin_f64("vnmls.f64 d30, d15, d2",  d30, d15, i32, f2u0(-45667.24), f2u1(-45667.24), d2, i32, f2u0(-248562.76), f2u1(-248562.76));
    TESTINSN_bin_f64("vnmls.f64 d23, d24, d5",  d23, d24, i32, f2u0(24), f2u1(24), d5, i32, f2u0(1346), f2u1(1346));
    TESTINSN_bin_f64("vnmls.f64 d10, d11, d2",  d10, d11, i32, f2u0(48755), f2u1(48755), d2, i32, f2u0(1089), f2u1(1089));
    TESTINSN_bin_f64("vnmls.f64 d29, d15, d7",  d29, d15, i32, f2u0(214), f2u1(214), d7, i32, f2u0(1752065), f2u1(1752065));
    TESTINSN_bin_f64("vnmls.f64 d30, d11, d12", d30, d11, i32, f2u0(356047.56), f2u1(356047.56), d12, i32, f2u0(5867.009), f2u1(5867.009));
    TESTINSN_bin_f64("vnmls.f64 d27, d21, d6",  d27, d21, i32, f2u0(34.00046), f2u1(34.00046), d6, i32, f2u0(0.0024575), f2u1(0.0024575));
    TESTINSN_bin_f64("vnmls.f64 d30, d31, d2",  d30, d31, i32, f2u0(2754), f2u1(2754), d2, i32, f2u0(107), f2u1(107));
    TESTINSN_bin_f64("vnmls.f64 d13, d24, d5",  d13, d24, i32, f2u0(874), f2u1(874), d5, i32, f2u0(1384.6), f2u1(1384.6));
    TESTINSN_bin_f64("vnmls.f64 d10, d11, d2",  d10, d11, i32, f2u0(487.587), f2u1(487.587), d2, i32, f2u0(109), f2u1(109));
    TESTINSN_bin_f64("vnmls.f64 d29, d25, d7",  d29, d25, i32, f2u0(-INFINITY), f2u1(-INFINITY), d7, i32, f2u0(1752), f2u1(1752));
    TESTINSN_bin_f64("vnmls.f64 d0,  d11, d12", d0,  d11, i32, f2u0(INFINITY), f2u1(INFINITY), d12, i32, f2u0(-5786.47), f2u1(-5786.47));
    TESTINSN_bin_f64("vnmls.f64 d27, d21, d16", d27, d21, i32, f2u0(456.2489562), f2u1(456.2489562), d16, i32, f2u0(-7.2945676), f2u1(-7.2945676));
    TESTINSN_bin_f64("vnmls.f64 d0,  d5,  d2",  d0,  d5,  i32, f2u0(INFINITY), f2u1(INFINITY), d2, i32, f2u0(-INFINITY), f2u1(-INFINITY));
    TESTINSN_bin_f64("vnmls.f64 d20, d13, d15", d20, d13, i32, f2u0(-INFINITY), f2u1(-INFINITY), d15, i32, f2u0(0.0), f2u1(0.0));
    TESTINSN_bin_f64("vnmls.f64 d10, d23, d15", d10, d23, i32, f2u0(INFINITY), f2u1(INFINITY), d15, i32, f2u0(0.0), f2u1(0.0));
    TESTINSN_bin_f32("vnmls.f32 s0,  s11, s12", s0,  s11, i32, f2u(-INFINITY), s12, i32, f2u(NAN));
    TESTINSN_bin_f32("vnmls.f32 s7,  s1,  s6",  s7,  s1,  i32, f2u(INFINITY), s6, i32, f2u(NAN));
    TESTINSN_bin_f32("vnmls.f32 s0,  s5,  s2",  s0,  s5,  i32, f2u(NAN), s2, i32, f2u(-1.0));
    TESTINSN_bin_f32("vnmls.f32 s10, s13, s15", s10, s13, i32, f2u(NAN), s15, i32, f2u(0.0));
    TESTINSN_bin_f32("vnmls.f32 s10, s13, s15", s10, s13, i32, f2u(NAN), s15, i32, f2u(NAN));
    TESTINSN_bin_f32("vnmls.f32 s20, s25, s22", s20, s25, i32, f2u(23.04), s22, i32, f2u(-45.5687));
    TESTINSN_bin_f32("vnmls.f32 s23, s24, s25", s23, s24, i32, f2u(-347856.475), s25, i32, f2u(1346));
    TESTINSN_bin_f32("vnmls.f32 s20, s31, s12", s20, s31, i32, f2u(48755), s12, i32, f2u(-45786.476));
    TESTINSN_bin_f32("vnmls.f32 s19, s25, s27", s19, s25, i32, f2u(95867.76), s27, i32, f2u(17065));
    TESTINSN_bin_f32("vnmls.f32 s30, s15, s2",  s30, s15, i32, f2u(-45667.24), s2, i32, f2u(-248562.76));
    TESTINSN_bin_f32("vnmls.f32 s23, s24, s5",  s23, s24, i32, f2u(24), s5, i32, f2u(1346));
    TESTINSN_bin_f32("vnmls.f32 s10, s11, s2",  s10, s11, i32, f2u(48755), s2, i32, f2u(1089));
    TESTINSN_bin_f32("vnmls.f32 s29, s15, s7",  s29, s15, i32, f2u(214), s7, i32, f2u(1752065));
    TESTINSN_bin_f32("vnmls.f32 s30, s11, s12", s30, s11, i32, f2u(356047.56), s12, i32, f2u(5867.009));
    TESTINSN_bin_f32("vnmls.f32 s27, s21, s6",  s27, s21, i32, f2u(34.00046), s6, i32, f2u(0.0024575));
    TESTINSN_bin_f32("vnmls.f32 s30, s31, s2",  s30, s31, i32, f2u(2754), s2, i32, f2u(107));
    TESTINSN_bin_f32("vnmls.f32 s13, s24, s5",  s13, s24, i32, f2u(874), s5, i32, f2u(1384.6));
    TESTINSN_bin_f32("vnmls.f32 s10, s11, s2",  s10, s11, i32, f2u(487.587), s2, i32, f2u(109));
    TESTINSN_bin_f32("vnmls.f32 s29, s25, s7",  s29, s25, i32, f2u(-INFINITY), s7, i32, f2u(1752));
    TESTINSN_bin_f32("vnmls.f32 s0,  s11, s12", s0,  s11, i32, f2u(INFINITY), s12, i32, f2u(-5786.47));
    TESTINSN_bin_f32("vnmls.f32 s27, s21, s16", s27, s21, i32, f2u(456.2489562), s16, i32, f2u(-7.2945676));
    TESTINSN_bin_f32("vnmls.f32 s0,  s5,  s2",  s0,  s5,  i32, f2u(INFINITY), s2, i32, f2u(-INFINITY));
    TESTINSN_bin_f32("vnmls.f32 s20, s13, s15", s20, s13, i32, f2u(-INFINITY), s15, i32, f2u(0.0));
    TESTINSN_bin_f32("vnmls.f32 s10, s23, s15", s10, s23, i32, f2u(INFINITY), s15, i32, f2u(0.0));

    printf("---- VMUL (fp) ----\n");
    TESTINSN_bin_f64("vmul.f64 d0,  d11, d12", d0,  d11, i32, f2u0(-INFINITY), f2u1(-INFINITY), d12, i32, f2u0(NAN), f2u1(NAN));
    TESTINSN_bin_f64("vmul.f64 d7,  d1,  d6",  d7,  d1,  i32, f2u0(INFINITY), f2u1(INFINITY), d6, i32, f2u0(NAN), f2u1(NAN));
    TESTINSN_bin_f64("vmul.f64 d0,  d5,  d2",  d0,  d5,  i32, f2u0(NAN), f2u1(NAN), d2, i32, f2u0(-1.0), f2u1(-1.0));
    TESTINSN_bin_f64("vmul.f64 d10, d13, d15", d10, d13, i32, f2u0(NAN), f2u1(NAN), d15, i32, f2u0(0.0), f2u1(0.0));
    TESTINSN_bin_f64("vmul.f64 d10, d13, d15", d10, d13, i32, f2u0(NAN), f2u1(NAN), d15, i32, f2u0(NAN), f2u1(NAN));
    TESTINSN_bin_f64("vmul.f64 d20, d25, d22", d20, d25, i32, f2u0(23.04), f2u1(23.04), d22, i32, f2u0(-45.5687), f2u1(-45.5687));
    TESTINSN_bin_f64("vmul.f64 d23, d24, d25", d23, d24, i32, f2u0(-347856.475), f2u1(-347856.475), d25, i32, f2u0(1346), f2u1(1346));
    TESTINSN_bin_f64("vmul.f64 d20, d31, d12", d20, d31, i32, f2u0(48755), f2u1(48755), d12, i32, f2u0(-45786.476), f2u1(-45786.476));
    TESTINSN_bin_f64("vmul.f64 d19, d25, d27", d19, d25, i32, f2u0(95867.76), f2u1(95867.76), d27, i32, f2u0(17065), f2u1(17065));
    TESTINSN_bin_f64("vmul.f64 d30, d15, d2",  d30, d15, i32, f2u0(-45667.24), f2u1(-45667.24), d2, i32, f2u0(-248562.76), f2u1(-248562.76));
    TESTINSN_bin_f64("vmul.f64 d23, d24, d5",  d23, d24, i32, f2u0(24), f2u1(24), d5, i32, f2u0(1346), f2u1(1346));
    TESTINSN_bin_f64("vmul.f64 d10, d11, d2",  d10, d11, i32, f2u0(48755), f2u1(48755), d2, i32, f2u0(1089), f2u1(1089));
    TESTINSN_bin_f64("vmul.f64 d29, d15, d7",  d29, d15, i32, f2u0(214), f2u1(214), d7, i32, f2u0(1752065), f2u1(1752065));
    TESTINSN_bin_f64("vmul.f64 d30, d11, d12", d30, d11, i32, f2u0(356047.56), f2u1(356047.56), d12, i32, f2u0(5867.009), f2u1(5867.009));
    TESTINSN_bin_f64("vmul.f64 d27, d21, d6",  d27, d21, i32, f2u0(34.00046), f2u1(34.00046), d6, i32, f2u0(0.0024575), f2u1(0.0024575));
    TESTINSN_bin_f64("vmul.f64 d30, d31, d2",  d30, d31, i32, f2u0(2754), f2u1(2754), d2, i32, f2u0(107), f2u1(107));
    TESTINSN_bin_f64("vmul.f64 d13, d24, d5",  d13, d24, i32, f2u0(874), f2u1(874), d5, i32, f2u0(1384.6), f2u1(1384.6));
    TESTINSN_bin_f64("vmul.f64 d10, d11, d2",  d10, d11, i32, f2u0(487.587), f2u1(487.587), d2, i32, f2u0(109), f2u1(109));
    TESTINSN_bin_f64("vmul.f64 d29, d25, d7",  d29, d25, i32, f2u0(-INFINITY), f2u1(-INFINITY), d7, i32, f2u0(1752), f2u1(1752));
    TESTINSN_bin_f64("vmul.f64 d0,  d11, d12", d0,  d11, i32, f2u0(INFINITY), f2u1(INFINITY), d12, i32, f2u0(-5786.47), f2u1(-5786.47));
    TESTINSN_bin_f64("vmul.f64 d27, d21, d16", d27, d21, i32, f2u0(456.2489562), f2u1(456.2489562), d16, i32, f2u0(-7.2945676), f2u1(-7.2945676));
    TESTINSN_bin_f64("vmul.f64 d0,  d5,  d2",  d0,  d5,  i32, f2u0(INFINITY), f2u1(INFINITY), d2, i32, f2u0(-INFINITY), f2u1(-INFINITY));
    TESTINSN_bin_f64("vmul.f64 d20, d13, d15", d20, d13, i32, f2u0(-INFINITY), f2u1(-INFINITY), d15, i32, f2u0(0.0), f2u1(0.0));
    TESTINSN_bin_f64("vmul.f64 d10, d23, d15", d10, d23, i32, f2u0(INFINITY), f2u1(INFINITY), d15, i32, f2u0(0.0), f2u1(0.0));
    TESTINSN_bin_f32("vmul.f32 s0,  s11, s12", s0,  s11, i32, f2u(-INFINITY), s12, i32, f2u(NAN));
    TESTINSN_bin_f32("vmul.f32 s7,  s1,  s6",  s7,  s1,  i32, f2u(INFINITY), s6, i32, f2u(NAN));
    TESTINSN_bin_f32("vmul.f32 s0,  s5,  s2",  s0,  s5,  i32, f2u(NAN), s2, i32, f2u(-1.0));
    TESTINSN_bin_f32("vmul.f32 s10, s13, s15", s10, s13, i32, f2u(NAN), s15, i32, f2u(0.0));
    TESTINSN_bin_f32("vmul.f32 s10, s13, s15", s10, s13, i32, f2u(NAN), s15, i32, f2u(NAN));
    TESTINSN_bin_f32("vmul.f32 s20, s25, s22", s20, s25, i32, f2u(23.04), s22, i32, f2u(-45.5687));
    TESTINSN_bin_f32("vmul.f32 s23, s24, s25", s23, s24, i32, f2u(-347856.475), s25, i32, f2u(1346));
    TESTINSN_bin_f32("vmul.f32 s20, s31, s12", s20, s31, i32, f2u(48755), s12, i32, f2u(-45786.476));
    TESTINSN_bin_f32("vmul.f32 s19, s25, s27", s19, s25, i32, f2u(95867.76), s27, i32, f2u(17065));
    TESTINSN_bin_f32("vmul.f32 s30, s15, s2",  s30, s15, i32, f2u(-45667.24), s2, i32, f2u(-248562.76));
    TESTINSN_bin_f32("vmul.f32 s23, s24, s5",  s23, s24, i32, f2u(24), s5, i32, f2u(1346));
    TESTINSN_bin_f32("vmul.f32 s10, s11, s2",  s10, s11, i32, f2u(48755), s2, i32, f2u(1089));
    TESTINSN_bin_f32("vmul.f32 s29, s15, s7",  s29, s15, i32, f2u(214), s7, i32, f2u(1752065));
    TESTINSN_bin_f32("vmul.f32 s30, s11, s12", s30, s11, i32, f2u(356047.56), s12, i32, f2u(5867.009));
    TESTINSN_bin_f32("vmul.f32 s27, s21, s6",  s27, s21, i32, f2u(34.00046), s6, i32, f2u(0.0024575));
    TESTINSN_bin_f32("vmul.f32 s30, s31, s2",  s30, s31, i32, f2u(2754), s2, i32, f2u(107));
    TESTINSN_bin_f32("vmul.f32 s13, s24, s5",  s13, s24, i32, f2u(874), s5, i32, f2u(1384.6));
    TESTINSN_bin_f32("vmul.f32 s10, s11, s2",  s10, s11, i32, f2u(487.587), s2, i32, f2u(109));
    TESTINSN_bin_f32("vmul.f32 s29, s25, s7",  s29, s25, i32, f2u(-INFINITY), s7, i32, f2u(1752));
    TESTINSN_bin_f32("vmul.f32 s0,  s11, s12", s0,  s11, i32, f2u(INFINITY), s12, i32, f2u(-5786.47));
    TESTINSN_bin_f32("vmul.f32 s27, s21, s16", s27, s21, i32, f2u(456.2489562), s16, i32, f2u(-7.2945676));
    TESTINSN_bin_f32("vmul.f32 s0,  s5,  s2",  s0,  s5,  i32, f2u(INFINITY), s2, i32, f2u(-INFINITY));
    TESTINSN_bin_f32("vmul.f32 s20, s13, s15", s20, s13, i32, f2u(-INFINITY), s15, i32, f2u(0.0));
    TESTINSN_bin_f32("vmul.f32 s10, s23, s15", s10, s23, i32, f2u(INFINITY), s15, i32, f2u(0.0));

    printf("---- VNMUL (fp) ----\n");
    TESTINSN_bin_f64("vnmul.f64 d0,  d11, d12", d0,  d11, i32, f2u0(-INFINITY), f2u1(-INFINITY), d12, i32, f2u0(NAN), f2u1(NAN));
    TESTINSN_bin_f64("vnmul.f64 d7,  d1,  d6",  d7,  d1,  i32, f2u0(INFINITY), f2u1(INFINITY), d6, i32, f2u0(NAN), f2u1(NAN));
    TESTINSN_bin_f64("vnmul.f64 d0,  d5,  d2",  d0,  d5,  i32, f2u0(NAN), f2u1(NAN), d2, i32, f2u0(-1.0), f2u1(-1.0));
    TESTINSN_bin_f64("vnmul.f64 d10, d13, d15", d10, d13, i32, f2u0(NAN), f2u1(NAN), d15, i32, f2u0(0.0), f2u1(0.0));
    TESTINSN_bin_f64("vnmul.f64 d10, d13, d15", d10, d13, i32, f2u0(NAN), f2u1(NAN), d15, i32, f2u0(NAN), f2u1(NAN));
    TESTINSN_bin_f64("vnmul.f64 d20, d25, d22", d20, d25, i32, f2u0(23.04), f2u1(23.04), d22, i32, f2u0(-45.5687), f2u1(-45.5687));
    TESTINSN_bin_f64("vnmul.f64 d23, d24, d25", d23, d24, i32, f2u0(-347856.475), f2u1(-347856.475), d25, i32, f2u0(1346), f2u1(1346));
    TESTINSN_bin_f64("vnmul.f64 d20, d31, d12", d20, d31, i32, f2u0(48755), f2u1(48755), d12, i32, f2u0(-45786.476), f2u1(-45786.476));
    TESTINSN_bin_f64("vnmul.f64 d19, d25, d27", d19, d25, i32, f2u0(95867.76), f2u1(95867.76), d27, i32, f2u0(17065), f2u1(17065));
    TESTINSN_bin_f64("vnmul.f64 d30, d15, d2",  d30, d15, i32, f2u0(-45667.24), f2u1(-45667.24), d2, i32, f2u0(-248562.76), f2u1(-248562.76));
    TESTINSN_bin_f64("vnmul.f64 d23, d24, d5",  d23, d24, i32, f2u0(24), f2u1(24), d5, i32, f2u0(1346), f2u1(1346));
    TESTINSN_bin_f64("vnmul.f64 d10, d11, d2",  d10, d11, i32, f2u0(48755), f2u1(48755), d2, i32, f2u0(1089), f2u1(1089));
    TESTINSN_bin_f64("vnmul.f64 d29, d15, d7",  d29, d15, i32, f2u0(214), f2u1(214), d7, i32, f2u0(1752065), f2u1(1752065));
    TESTINSN_bin_f64("vnmul.f64 d30, d11, d12", d30, d11, i32, f2u0(356047.56), f2u1(356047.56), d12, i32, f2u0(5867.009), f2u1(5867.009));
    TESTINSN_bin_f64("vnmul.f64 d27, d21, d6",  d27, d21, i32, f2u0(34.00046), f2u1(34.00046), d6, i32, f2u0(0.0024575), f2u1(0.0024575));
    TESTINSN_bin_f64("vnmul.f64 d30, d31, d2",  d30, d31, i32, f2u0(2754), f2u1(2754), d2, i32, f2u0(107), f2u1(107));
    TESTINSN_bin_f64("vnmul.f64 d13, d24, d5",  d13, d24, i32, f2u0(874), f2u1(874), d5, i32, f2u0(1384.6), f2u1(1384.6));
    TESTINSN_bin_f64("vnmul.f64 d10, d11, d2",  d10, d11, i32, f2u0(487.587), f2u1(487.587), d2, i32, f2u0(109), f2u1(109));
    TESTINSN_bin_f64("vnmul.f64 d29, d25, d7",  d29, d25, i32, f2u0(-INFINITY), f2u1(-INFINITY), d7, i32, f2u0(1752), f2u1(1752));
    TESTINSN_bin_f64("vnmul.f64 d0,  d11, d12", d0,  d11, i32, f2u0(INFINITY), f2u1(INFINITY), d12, i32, f2u0(-5786.47), f2u1(-5786.47));
    TESTINSN_bin_f64("vnmul.f64 d27, d21, d16", d27, d21, i32, f2u0(456.2489562), f2u1(456.2489562), d16, i32, f2u0(-7.2945676), f2u1(-7.2945676));
    TESTINSN_bin_f64("vnmul.f64 d0,  d5,  d2",  d0,  d5,  i32, f2u0(INFINITY), f2u1(INFINITY), d2, i32, f2u0(-INFINITY), f2u1(-INFINITY));
    TESTINSN_bin_f64("vnmul.f64 d20, d13, d15", d20, d13, i32, f2u0(-INFINITY), f2u1(-INFINITY), d15, i32, f2u0(0.0), f2u1(0.0));
    TESTINSN_bin_f64("vnmul.f64 d10, d23, d15", d10, d23, i32, f2u0(INFINITY), f2u1(INFINITY), d15, i32, f2u0(0.0), f2u1(0.0));
    TESTINSN_bin_f32("vnmul.f32 s0,  s11, s12", s0,  s11, i32, f2u(-INFINITY), s12, i32, f2u(NAN));
    TESTINSN_bin_f32("vnmul.f32 s7,  s1,  s6",  s7,  s1,  i32, f2u(INFINITY), s6, i32, f2u(NAN));
    TESTINSN_bin_f32("vnmul.f32 s0,  s5,  s2",  s0,  s5,  i32, f2u(NAN), s2, i32, f2u(-1.0));
    TESTINSN_bin_f32("vnmul.f32 s10, s13, s15", s10, s13, i32, f2u(NAN), s15, i32, f2u(0.0));
    TESTINSN_bin_f32("vnmul.f32 s10, s13, s15", s10, s13, i32, f2u(NAN), s15, i32, f2u(NAN));
    TESTINSN_bin_f32("vnmul.f32 s20, s25, s22", s20, s25, i32, f2u(23.04), s22, i32, f2u(-45.5687));
    TESTINSN_bin_f32("vnmul.f32 s23, s24, s25", s23, s24, i32, f2u(-347856.475), s25, i32, f2u(1346));
    TESTINSN_bin_f32("vnmul.f32 s20, s31, s12", s20, s31, i32, f2u(48755), s12, i32, f2u(-45786.476));
    TESTINSN_bin_f32("vnmul.f32 s19, s25, s27", s19, s25, i32, f2u(95867.76), s27, i32, f2u(17065));
    TESTINSN_bin_f32("vnmul.f32 s30, s15, s2",  s30, s15, i32, f2u(-45667.24), s2, i32, f2u(-248562.76));
    TESTINSN_bin_f32("vnmul.f32 s23, s24, s5",  s23, s24, i32, f2u(24), s5, i32, f2u(1346));
    TESTINSN_bin_f32("vnmul.f32 s10, s11, s2",  s10, s11, i32, f2u(48755), s2, i32, f2u(1089));
    TESTINSN_bin_f32("vnmul.f32 s29, s15, s7",  s29, s15, i32, f2u(214), s7, i32, f2u(1752065));
    TESTINSN_bin_f32("vnmul.f32 s30, s11, s12", s30, s11, i32, f2u(356047.56), s12, i32, f2u(5867.009));
    TESTINSN_bin_f32("vnmul.f32 s27, s21, s6",  s27, s21, i32, f2u(34.00046), s6, i32, f2u(0.0024575));
    TESTINSN_bin_f32("vnmul.f32 s30, s31, s2",  s30, s31, i32, f2u(2754), s2, i32, f2u(107));
    TESTINSN_bin_f32("vnmul.f32 s13, s24, s5",  s13, s24, i32, f2u(874), s5, i32, f2u(1384.6));
    TESTINSN_bin_f32("vnmul.f32 s10, s11, s2",  s10, s11, i32, f2u(487.587), s2, i32, f2u(109));
    TESTINSN_bin_f32("vnmul.f32 s29, s25, s7",  s29, s25, i32, f2u(-INFINITY), s7, i32, f2u(1752));
    TESTINSN_bin_f32("vnmul.f32 s0,  s11, s12", s0,  s11, i32, f2u(INFINITY), s12, i32, f2u(-5786.47));
    TESTINSN_bin_f32("vnmul.f32 s27, s21, s16", s27, s21, i32, f2u(456.2489562), s16, i32, f2u(-7.2945676));
    TESTINSN_bin_f32("vnmul.f32 s0,  s5,  s2",  s0,  s5,  i32, f2u(INFINITY), s2, i32, f2u(-INFINITY));
    TESTINSN_bin_f32("vnmul.f32 s20, s13, s15", s20, s13, i32, f2u(-INFINITY), s15, i32, f2u(0.0));
    TESTINSN_bin_f32("vnmul.f32 s10, s23, s15", s10, s23, i32, f2u(INFINITY), s15, i32, f2u(0.0));

    printf("---- VADD (fp) ----\n");
    TESTINSN_bin_f64("vadd.f64 d0,  d11, d12", d0,  d11, i32, f2u0(-INFINITY), f2u1(-INFINITY), d12, i32, f2u0(NAN), f2u1(NAN));
    TESTINSN_bin_f64("vadd.f64 d7,  d1,  d6",  d7,  d1,  i32, f2u0(INFINITY), f2u1(INFINITY), d6, i32, f2u0(NAN), f2u1(NAN));
    TESTINSN_bin_f64("vadd.f64 d0,  d5,  d2",  d0,  d5,  i32, f2u0(NAN), f2u1(NAN), d2, i32, f2u0(-1.0), f2u1(-1.0));
    TESTINSN_bin_f64("vadd.f64 d10, d13, d15", d10, d13, i32, f2u0(NAN), f2u1(NAN), d15, i32, f2u0(0.0), f2u1(0.0));
    TESTINSN_bin_f64("vadd.f64 d10, d13, d15", d10, d13, i32, f2u0(NAN), f2u1(NAN), d15, i32, f2u0(NAN), f2u1(NAN));
    TESTINSN_bin_f64("vadd.f64 d20, d25, d22", d20, d25, i32, f2u0(23.04), f2u1(23.04), d22, i32, f2u0(-45.5687), f2u1(-45.5687));
    TESTINSN_bin_f64("vadd.f64 d23, d24, d25", d23, d24, i32, f2u0(-347856.475), f2u1(-347856.475), d25, i32, f2u0(1346), f2u1(1346));
    TESTINSN_bin_f64("vadd.f64 d20, d31, d12", d20, d31, i32, f2u0(48755), f2u1(48755), d12, i32, f2u0(-45786.476), f2u1(-45786.476));
    TESTINSN_bin_f64("vadd.f64 d19, d25, d27", d19, d25, i32, f2u0(95867.76), f2u1(95867.76), d27, i32, f2u0(17065), f2u1(17065));
    TESTINSN_bin_f64("vadd.f64 d30, d15, d2",  d30, d15, i32, f2u0(-45667.24), f2u1(-45667.24), d2, i32, f2u0(-248562.76), f2u1(-248562.76));
    TESTINSN_bin_f64("vadd.f64 d23, d24, d5",  d23, d24, i32, f2u0(24), f2u1(24), d5, i32, f2u0(1346), f2u1(1346));
    TESTINSN_bin_f64("vadd.f64 d10, d11, d2",  d10, d11, i32, f2u0(48755), f2u1(48755), d2, i32, f2u0(1089), f2u1(1089));
    TESTINSN_bin_f64("vadd.f64 d29, d15, d7",  d29, d15, i32, f2u0(214), f2u1(214), d7, i32, f2u0(1752065), f2u1(1752065));
    TESTINSN_bin_f64("vadd.f64 d30, d11, d12", d30, d11, i32, f2u0(356047.56), f2u1(356047.56), d12, i32, f2u0(5867.009), f2u1(5867.009));
    TESTINSN_bin_f64("vadd.f64 d27, d21, d6",  d27, d21, i32, f2u0(34.00046), f2u1(34.00046), d6, i32, f2u0(0.0024575), f2u1(0.0024575));
    TESTINSN_bin_f64("vadd.f64 d30, d31, d2",  d30, d31, i32, f2u0(2754), f2u1(2754), d2, i32, f2u0(107), f2u1(107));
    TESTINSN_bin_f64("vadd.f64 d13, d24, d5",  d13, d24, i32, f2u0(874), f2u1(874), d5, i32, f2u0(1384.6), f2u1(1384.6));
    TESTINSN_bin_f64("vadd.f64 d10, d11, d2",  d10, d11, i32, f2u0(487.587), f2u1(487.587), d2, i32, f2u0(109), f2u1(109));
    TESTINSN_bin_f64("vadd.f64 d29, d25, d7",  d29, d25, i32, f2u0(-INFINITY), f2u1(-INFINITY), d7, i32, f2u0(1752), f2u1(1752));
    TESTINSN_bin_f64("vadd.f64 d0,  d11, d12", d0,  d11, i32, f2u0(INFINITY), f2u1(INFINITY), d12, i32, f2u0(-5786.47), f2u1(-5786.47));
    TESTINSN_bin_f64("vadd.f64 d27, d21, d16", d27, d21, i32, f2u0(456.2489562), f2u1(456.2489562), d16, i32, f2u0(-7.2945676), f2u1(-7.2945676));
    TESTINSN_bin_f64("vadd.f64 d0,  d5,  d2",  d0,  d5,  i32, f2u0(INFINITY), f2u1(INFINITY), d2, i32, f2u0(-INFINITY), f2u1(-INFINITY));
    TESTINSN_bin_f64("vadd.f64 d20, d13, d15", d20, d13, i32, f2u0(-INFINITY), f2u1(-INFINITY), d15, i32, f2u0(0.0), f2u1(0.0));
    TESTINSN_bin_f64("vadd.f64 d10, d23, d15", d10, d23, i32, f2u0(INFINITY), f2u1(INFINITY), d15, i32, f2u0(0.0), f2u1(0.0));
    TESTINSN_bin_f32("vadd.f32 s0,  s11, s12", s0,  s11, i32, f2u(-INFINITY), s12, i32, f2u(NAN));
    TESTINSN_bin_f32("vadd.f32 s7,  s1,  s6",  s7,  s1,  i32, f2u(INFINITY), s6, i32, f2u(NAN));
    TESTINSN_bin_f32("vadd.f32 s0,  s5,  s2",  s0,  s5,  i32, f2u(NAN), s2, i32, f2u(-1.0));
    TESTINSN_bin_f32("vadd.f32 s10, s13, s15", s10, s13, i32, f2u(NAN), s15, i32, f2u(0.0));
    TESTINSN_bin_f32("vadd.f32 s10, s13, s15", s10, s13, i32, f2u(NAN), s15, i32, f2u(NAN));
    TESTINSN_bin_f32("vadd.f32 s20, s25, s22", s20, s25, i32, f2u(23.04), s22, i32, f2u(-45.5687));
    TESTINSN_bin_f32("vadd.f32 s23, s24, s25", s23, s24, i32, f2u(-347856.475), s25, i32, f2u(1346));
    TESTINSN_bin_f32("vadd.f32 s20, s31, s12", s20, s31, i32, f2u(48755), s12, i32, f2u(-45786.476));
    TESTINSN_bin_f32("vadd.f32 s19, s25, s27", s19, s25, i32, f2u(95867.76), s27, i32, f2u(17065));
    TESTINSN_bin_f32("vadd.f32 s30, s15, s2",  s30, s15, i32, f2u(-45667.24), s2, i32, f2u(-248562.76));
    TESTINSN_bin_f32("vadd.f32 s23, s24, s5",  s23, s24, i32, f2u(24), s5, i32, f2u(1346));
    TESTINSN_bin_f32("vadd.f32 s10, s11, s2",  s10, s11, i32, f2u(48755), s2, i32, f2u(1089));
    TESTINSN_bin_f32("vadd.f32 s29, s15, s7",  s29, s15, i32, f2u(214), s7, i32, f2u(1752065));
    TESTINSN_bin_f32("vadd.f32 s30, s11, s12", s30, s11, i32, f2u(356047.56), s12, i32, f2u(5867.009));
    TESTINSN_bin_f32("vadd.f32 s27, s21, s6",  s27, s21, i32, f2u(34.00046), s6, i32, f2u(0.0024575));
    TESTINSN_bin_f32("vadd.f32 s30, s31, s2",  s30, s31, i32, f2u(2754), s2, i32, f2u(107));
    TESTINSN_bin_f32("vadd.f32 s13, s24, s5",  s13, s24, i32, f2u(874), s5, i32, f2u(1384.6));
    TESTINSN_bin_f32("vadd.f32 s10, s11, s2",  s10, s11, i32, f2u(487.587), s2, i32, f2u(109));
    TESTINSN_bin_f32("vadd.f32 s29, s25, s7",  s29, s25, i32, f2u(-INFINITY), s7, i32, f2u(1752));
    TESTINSN_bin_f32("vadd.f32 s0,  s11, s12", s0,  s11, i32, f2u(INFINITY), s12, i32, f2u(-5786.47));
    TESTINSN_bin_f32("vadd.f32 s27, s21, s16", s27, s21, i32, f2u(456.2489562), s16, i32, f2u(-7.2945676));
    TESTINSN_bin_f32("vadd.f32 s0,  s5,  s2",  s0,  s5,  i32, f2u(INFINITY), s2, i32, f2u(-INFINITY));
    TESTINSN_bin_f32("vadd.f32 s20, s13, s15", s20, s13, i32, f2u(-INFINITY), s15, i32, f2u(0.0));
    TESTINSN_bin_f32("vadd.f32 s10, s23, s15", s10, s23, i32, f2u(INFINITY), s15, i32, f2u(0.0));

    printf("---- VSUB (fp) ----\n");
    TESTINSN_bin_f64("vsub.f64 d0,  d11, d12", d0,  d11, i32, f2u0(-INFINITY), f2u1(-INFINITY), d12, i32, f2u0(NAN), f2u1(NAN));
    TESTINSN_bin_f64("vsub.f64 d7,  d1,  d6",  d7,  d1,  i32, f2u0(INFINITY), f2u1(INFINITY), d6, i32, f2u0(NAN), f2u1(NAN));
    TESTINSN_bin_f64("vsub.f64 d0,  d5,  d2",  d0,  d5,  i32, f2u0(NAN), f2u1(NAN), d2, i32, f2u0(-1.0), f2u1(-1.0));
    TESTINSN_bin_f64("vsub.f64 d10, d13, d15", d10, d13, i32, f2u0(NAN), f2u1(NAN), d15, i32, f2u0(0.0), f2u1(0.0));
    TESTINSN_bin_f64("vsub.f64 d10, d13, d15", d10, d13, i32, f2u0(NAN), f2u1(NAN), d15, i32, f2u0(NAN), f2u1(NAN));
    TESTINSN_bin_f64("vsub.f64 d20, d25, d22", d20, d25, i32, f2u0(23.04), f2u1(23.04), d22, i32, f2u0(-45.5687), f2u1(-45.5687));
    TESTINSN_bin_f64("vsub.f64 d23, d24, d25", d23, d24, i32, f2u0(-347856.475), f2u1(-347856.475), d25, i32, f2u0(1346), f2u1(1346));
    TESTINSN_bin_f64("vsub.f64 d20, d31, d12", d20, d31, i32, f2u0(48755), f2u1(48755), d12, i32, f2u0(-45786.476), f2u1(-45786.476));
    TESTINSN_bin_f64("vsub.f64 d19, d25, d27", d19, d25, i32, f2u0(95867.76), f2u1(95867.76), d27, i32, f2u0(17065), f2u1(17065));
    TESTINSN_bin_f64("vsub.f64 d30, d15, d2",  d30, d15, i32, f2u0(-45667.24), f2u1(-45667.24), d2, i32, f2u0(-248562.76), f2u1(-248562.76));
    TESTINSN_bin_f64("vsub.f64 d23, d24, d5",  d23, d24, i32, f2u0(24), f2u1(24), d5, i32, f2u0(1346), f2u1(1346));
    TESTINSN_bin_f64("vsub.f64 d10, d11, d2",  d10, d11, i32, f2u0(48755), f2u1(48755), d2, i32, f2u0(1089), f2u1(1089));
    TESTINSN_bin_f64("vsub.f64 d29, d15, d7",  d29, d15, i32, f2u0(214), f2u1(214), d7, i32, f2u0(1752065), f2u1(1752065));
    TESTINSN_bin_f64("vsub.f64 d30, d11, d12", d30, d11, i32, f2u0(356047.56), f2u1(356047.56), d12, i32, f2u0(5867.009), f2u1(5867.009));
    TESTINSN_bin_f64("vsub.f64 d27, d21, d6",  d27, d21, i32, f2u0(34.00046), f2u1(34.00046), d6, i32, f2u0(0.0024575), f2u1(0.0024575));
    TESTINSN_bin_f64("vsub.f64 d30, d31, d2",  d30, d31, i32, f2u0(2754), f2u1(2754), d2, i32, f2u0(107), f2u1(107));
    TESTINSN_bin_f64("vsub.f64 d13, d24, d5",  d13, d24, i32, f2u0(874), f2u1(874), d5, i32, f2u0(1384.6), f2u1(1384.6));
    TESTINSN_bin_f64("vsub.f64 d10, d11, d2",  d10, d11, i32, f2u0(487.587), f2u1(487.587), d2, i32, f2u0(109), f2u1(109));
    TESTINSN_bin_f64("vsub.f64 d29, d25, d7",  d29, d25, i32, f2u0(-INFINITY), f2u1(-INFINITY), d7, i32, f2u0(1752), f2u1(1752));
    TESTINSN_bin_f64("vsub.f64 d0,  d11, d12", d0,  d11, i32, f2u0(INFINITY), f2u1(INFINITY), d12, i32, f2u0(-5786.47), f2u1(-5786.47));
    TESTINSN_bin_f64("vsub.f64 d27, d21, d16", d27, d21, i32, f2u0(456.2489562), f2u1(456.2489562), d16, i32, f2u0(-7.2945676), f2u1(-7.2945676));
    TESTINSN_bin_f64("vsub.f64 d0,  d5,  d2",  d0,  d5,  i32, f2u0(INFINITY), f2u1(INFINITY), d2, i32, f2u0(-INFINITY), f2u1(-INFINITY));
    TESTINSN_bin_f64("vsub.f64 d20, d13, d15", d20, d13, i32, f2u0(-INFINITY), f2u1(-INFINITY), d15, i32, f2u0(0.0), f2u1(0.0));
    TESTINSN_bin_f64("vsub.f64 d10, d23, d15", d10, d23, i32, f2u0(INFINITY), f2u1(INFINITY), d15, i32, f2u0(0.0), f2u1(0.0));
    TESTINSN_bin_f32("vsub.f32 s0,  s11, s12", s0,  s11, i32, f2u(-INFINITY), s12, i32, f2u(NAN));
    TESTINSN_bin_f32("vsub.f32 s7,  s1,  s6",  s7,  s1,  i32, f2u(INFINITY), s6, i32, f2u(NAN));
    TESTINSN_bin_f32("vsub.f32 s0,  s5,  s2",  s0,  s5,  i32, f2u(NAN), s2, i32, f2u(-1.0));
    TESTINSN_bin_f32("vsub.f32 s10, s13, s15", s10, s13, i32, f2u(NAN), s15, i32, f2u(0.0));
    TESTINSN_bin_f32("vsub.f32 s10, s13, s15", s10, s13, i32, f2u(NAN), s15, i32, f2u(NAN));
    TESTINSN_bin_f32("vsub.f32 s20, s25, s22", s20, s25, i32, f2u(23.04), s22, i32, f2u(-45.5687));
    TESTINSN_bin_f32("vsub.f32 s23, s24, s25", s23, s24, i32, f2u(-347856.475), s25, i32, f2u(1346));
    TESTINSN_bin_f32("vsub.f32 s20, s31, s12", s20, s31, i32, f2u(48755), s12, i32, f2u(-45786.476));
    TESTINSN_bin_f32("vsub.f32 s19, s25, s27", s19, s25, i32, f2u(95867.76), s27, i32, f2u(17065));
    TESTINSN_bin_f32("vsub.f32 s30, s15, s2",  s30, s15, i32, f2u(-45667.24), s2, i32, f2u(-248562.76));
    TESTINSN_bin_f32("vsub.f32 s23, s24, s5",  s23, s24, i32, f2u(24), s5, i32, f2u(1346));
    TESTINSN_bin_f32("vsub.f32 s10, s11, s2",  s10, s11, i32, f2u(48755), s2, i32, f2u(1089));
    TESTINSN_bin_f32("vsub.f32 s29, s15, s7",  s29, s15, i32, f2u(214), s7, i32, f2u(1752065));
    TESTINSN_bin_f32("vsub.f32 s30, s11, s12", s30, s11, i32, f2u(356047.56), s12, i32, f2u(5867.009));
    TESTINSN_bin_f32("vsub.f32 s27, s21, s6",  s27, s21, i32, f2u(34.00046), s6, i32, f2u(0.0024575));
    TESTINSN_bin_f32("vsub.f32 s30, s31, s2",  s30, s31, i32, f2u(2754), s2, i32, f2u(107));
    TESTINSN_bin_f32("vsub.f32 s13, s24, s5",  s13, s24, i32, f2u(874), s5, i32, f2u(1384.6));
    TESTINSN_bin_f32("vsub.f32 s10, s11, s2",  s10, s11, i32, f2u(487.587), s2, i32, f2u(109));
    TESTINSN_bin_f32("vsub.f32 s29, s25, s7",  s29, s25, i32, f2u(-INFINITY), s7, i32, f2u(1752));
    TESTINSN_bin_f32("vsub.f32 s0,  s11, s12", s0,  s11, i32, f2u(INFINITY), s12, i32, f2u(-5786.47));
    TESTINSN_bin_f32("vsub.f32 s27, s21, s16", s27, s21, i32, f2u(456.2489562), s16, i32, f2u(-7.2945676));
    TESTINSN_bin_f32("vsub.f32 s0,  s5,  s2",  s0,  s5,  i32, f2u(INFINITY), s2, i32, f2u(-INFINITY));
    TESTINSN_bin_f32("vsub.f32 s20, s13, s15", s20, s13, i32, f2u(-INFINITY), s15, i32, f2u(0.0));
    TESTINSN_bin_f32("vsub.f32 s10, s23, s15", s10, s23, i32, f2u(INFINITY), s15, i32, f2u(0.0));

    printf("---- VDIV (fp) ----\n");
    TESTINSN_bin_f64("vdiv.f64 d20, d25, d22", d20, d25, i32, f2u0(23.04), f2u1(23.04), d22, i32, f2u0(-45.5687), f2u1(-45.5687));
    TESTINSN_bin_f64("vdiv.f64 d23, d24, d25", d23, d24, i32, f2u0(-347856.475), f2u1(-347856.475), d25, i32, f2u0(1346), f2u1(1346));
    TESTINSN_bin_f64("vdiv.f64 d20, d31, d12", d20, d31, i32, f2u0(48755), f2u1(48755), d12, i32, f2u0(-45786.476), f2u1(-45786.476));
    TESTINSN_bin_f64("vdiv.f64 d19, d25, d27", d19, d25, i32, f2u0(95867.76), f2u1(95867.76), d27, i32, f2u0(17065), f2u1(17065));
    TESTINSN_bin_f64("vdiv.f64 d30, d15, d2",  d30, d15, i32, f2u0(-45667.24), f2u1(-45667.24), d2, i32, f2u0(-248562.76), f2u1(-248562.76));
    TESTINSN_bin_f64("vdiv.f64 d23, d24, d5",  d23, d24, i32, f2u0(24), f2u1(24), d5, i32, f2u0(1346), f2u1(1346));
    TESTINSN_bin_f64("vdiv.f64 d10, d11, d2",  d10, d11, i32, f2u0(48755), f2u1(48755), d2, i32, f2u0(1089), f2u1(1089));
    TESTINSN_bin_f64("vdiv.f64 d29, d15, d7",  d29, d15, i32, f2u0(214), f2u1(214), d7, i32, f2u0(1752065), f2u1(1752065));
    TESTINSN_bin_f64("vdiv.f64 d30, d11, d12", d30, d11, i32, f2u0(356047.56), f2u1(356047.56), d12, i32, f2u0(5867.009), f2u1(5867.009));
    TESTINSN_bin_f64("vdiv.f64 d27, d21, d6",  d27, d21, i32, f2u0(34.00046), f2u1(34.00046), d6, i32, f2u0(0.0024575), f2u1(0.0024575));
    TESTINSN_bin_f64("vdiv.f64 d30, d31, d2",  d30, d31, i32, f2u0(2754), f2u1(2754), d2, i32, f2u0(107), f2u1(107));
    TESTINSN_bin_f64("vdiv.f64 d13, d24, d5",  d13, d24, i32, f2u0(874), f2u1(874), d5, i32, f2u0(1384.6), f2u1(1384.6));
    TESTINSN_bin_f64("vdiv.f64 d10, d11, d2",  d10, d11, i32, f2u0(487.587), f2u1(487.587), d2, i32, f2u0(109), f2u1(109));
    TESTINSN_bin_f64("vdiv.f64 d29, d25, d7",  d29, d25, i32, f2u0(-INFINITY), f2u1(-INFINITY), d7, i32, f2u0(1752), f2u1(1752));
    TESTINSN_bin_f64("vdiv.f64 d0,  d11, d12", d0,  d11, i32, f2u0(INFINITY), f2u1(INFINITY), d12, i32, f2u0(-5786.47), f2u1(-5786.47));
    TESTINSN_bin_f64("vdiv.f64 d27, d21, d16", d27, d21, i32, f2u0(456.2489562), f2u1(456.2489562), d16, i32, f2u0(-7.2945676), f2u1(-7.2945676));
    TESTINSN_bin_f64("vdiv.f64 d0,  d5,  d2",  d0,  d5,  i32, f2u0(INFINITY), f2u1(INFINITY), d2, i32, f2u0(-INFINITY), f2u1(-INFINITY));
    TESTINSN_bin_f64("vdiv.f64 d20, d13, d15", d20, d13, i32, f2u0(-INFINITY), f2u1(-INFINITY), d15, i32, f2u0(0.0), f2u1(0.0));
    TESTINSN_bin_f64("vdiv.f64 d10, d23, d15", d10, d23, i32, f2u0(INFINITY), f2u1(INFINITY), d15, i32, f2u0(0.0), f2u1(0.0));
    TESTINSN_bin_f32("vdiv.f32 s20, s25, s22", s20, s25, i32, f2u(23.04), s22, i32, f2u(-45.5687));
    TESTINSN_bin_f32("vdiv.f32 s23, s24, s25", s23, s24, i32, f2u(-347856.475), s25, i32, f2u(1346));
    TESTINSN_bin_f32("vdiv.f32 s20, s31, s12", s20, s31, i32, f2u(48755), s12, i32, f2u(-45786.476));
    TESTINSN_bin_f32("vdiv.f32 s19, s25, s27", s19, s25, i32, f2u(95867.76), s27, i32, f2u(17065));
    TESTINSN_bin_f32("vdiv.f32 s30, s15, s2",  s30, s15, i32, f2u(-45667.24), s2, i32, f2u(-248562.76));
    TESTINSN_bin_f32("vdiv.f32 s23, s24, s5",  s23, s24, i32, f2u(24), s5, i32, f2u(1346));
    TESTINSN_bin_f32("vdiv.f32 s10, s11, s2",  s10, s11, i32, f2u(48755), s2, i32, f2u(1089));
    TESTINSN_bin_f32("vdiv.f32 s29, s15, s7",  s29, s15, i32, f2u(214), s7, i32, f2u(1752065));
    TESTINSN_bin_f32("vdiv.f32 s30, s11, s12", s30, s11, i32, f2u(356047.56), s12, i32, f2u(5867.009));
    TESTINSN_bin_f32("vdiv.f32 s27, s21, s6",  s27, s21, i32, f2u(34.00046), s6, i32, f2u(0.0024575));
    TESTINSN_bin_f32("vdiv.f32 s30, s31, s2",  s30, s31, i32, f2u(2754), s2, i32, f2u(107));
    TESTINSN_bin_f32("vdiv.f32 s13, s24, s5",  s13, s24, i32, f2u(874), s5, i32, f2u(1384.6));
    TESTINSN_bin_f32("vdiv.f32 s10, s11, s2",  s10, s11, i32, f2u(487.587), s2, i32, f2u(109));
    TESTINSN_bin_f32("vdiv.f32 s29, s25, s7",  s29, s25, i32, f2u(-INFINITY), s7, i32, f2u(1752));
    TESTINSN_bin_f32("vdiv.f32 s0,  s11, s12", s0,  s11, i32, f2u(INFINITY), s12, i32, f2u(-5786.47));
    TESTINSN_bin_f32("vdiv.f32 s27, s21, s16", s27, s21, i32, f2u(456.2489562), s16, i32, f2u(-7.2945676));
    TESTINSN_bin_f32("vdiv.f32 s0,  s5,  s2",  s0,  s5,  i32, f2u(INFINITY), s2, i32, f2u(-INFINITY));
    TESTINSN_bin_f32("vdiv.f32 s20, s13, s15", s20, s13, i32, f2u(-INFINITY), s15, i32, f2u(0.0));
    TESTINSN_bin_f32("vdiv.f32 s10, s23, s15", s10, s23, i32, f2u(INFINITY), s15, i32, f2u(0.0));

    printf("---- VABS ----\n");
    TESTINSN_un_f64("vabs.f64 d15, d4",  d15, d4,  i32, f2u0(NAN), f2u1(NAN));
    TESTINSN_un_f64("vabs.f64 d31, d4",  d31, d4,  i32, f2u0(NAN), f2u1(NAN));
    TESTINSN_un_f64("vabs.f64 d25, d25", d25, d24, i32, f2u0(INFINITY), f2u1(INFINITY));
    TESTINSN_un_f64("vabs.f64 d18, d17", d18, d17, i32, f2u0(INFINITY), f2u1(INFINITY));
    TESTINSN_un_f64("vabs.f64 d30, d1",  d30, d1,  i32, f2u0(-INFINITY), f2u1(-INFINITY));
    TESTINSN_un_f64("vabs.f64 d8,  d27", d8,  d27, i32, f2u0(-INFINITY), f2u1(-INFINITY));
    TESTINSN_un_f64("vabs.f64 d20, d1",  d20, d1,  i32, f2u0(76543.001002), f2u1(76543.001002));
    TESTINSN_un_f64("vabs.f64 d28, d7",  d28, d7,  i32, f2u0(-4856.234), f2u1(-4856.234));
    TESTINSN_un_f64("vabs.f64 d2,  d19", d2, d19,  i32,f2u0(87.098217), f2u1(87.098217));
    TESTINSN_un_f64("vabs.f64 d8,  d7",  d8, d7,  i32, f2u0(-122156.2), f2u1(-122156.2));
    TESTINSN_un_f32("vabs.f32 s15, s4",  s15, s4,  i32, f2u(NAN));
    TESTINSN_un_f32("vabs.f32 s31, s4",  s31, s4,  i32, f2u(NAN));
    TESTINSN_un_f32("vabs.f32 s25, s25", s25, s24, i32, f2u(INFINITY));
    TESTINSN_un_f32("vabs.f32 s18, s17", s18, s17, i32, f2u(INFINITY));
    TESTINSN_un_f32("vabs.f32 s30, s1",  s30, s1,  i32, f2u(-INFINITY));
    TESTINSN_un_f32("vabs.f32 s8,  s27", s8,  s27, i32, f2u(-INFINITY));
    TESTINSN_un_f32("vabs.f32 s20, s1",  s20, s1,  i32, f2u(76543.001002));
    TESTINSN_un_f32("vabs.f32 s28, s7",  s28, s7,  i32, f2u(-4856.234));
    TESTINSN_un_f32("vabs.f32 s2,  s19", s2, s19,  i32,f2u(87.098217));
    TESTINSN_un_f32("vabs.f32 s8,  s7",  s8, s7,  i32, f2u(-122156.2));

    printf("---- VNEG ----\n");
    TESTINSN_un_f64("vneg.f64 d15, d4",  d15, d4,  i32, f2u0(NAN), f2u1(NAN));
    TESTINSN_un_f64("vneg.f64 d31, d4",  d31, d4,  i32, f2u0(NAN), f2u1(NAN));
    TESTINSN_un_f64("vneg.f64 d25, d25", d25, d24, i32, f2u0(INFINITY), f2u1(INFINITY));
    TESTINSN_un_f64("vneg.f64 d18, d17", d18, d17, i32, f2u0(INFINITY), f2u1(INFINITY));
    TESTINSN_un_f64("vneg.f64 d30, d1",  d30, d1,  i32, f2u0(-INFINITY), f2u1(-INFINITY));
    TESTINSN_un_f64("vneg.f64 d8,  d27", d8,  d27, i32, f2u0(-INFINITY), f2u1(-INFINITY));
    TESTINSN_un_f64("vneg.f64 d20, d1",  d20, d1,  i32, f2u0(76543.001002), f2u1(76543.001002));
    TESTINSN_un_f64("vneg.f64 d28, d7",  d28, d7,  i32, f2u0(-4856.234), f2u1(-4856.234));
    TESTINSN_un_f64("vneg.f64 d2,  d19", d2, d19,  i32,f2u0(87.098217), f2u1(87.098217));
    TESTINSN_un_f64("vneg.f64 d8,  d7",  d8, d7,  i32, f2u0(-122156.2), f2u1(-122156.2));
    TESTINSN_un_f32("vneg.f32 s15, s4",  s15, s4,  i32, f2u(NAN));
    TESTINSN_un_f32("vneg.f32 s31, s4",  s31, s4,  i32, f2u(NAN));
    TESTINSN_un_f32("vneg.f32 s25, s25", s25, s24, i32, f2u(INFINITY));
    TESTINSN_un_f32("vneg.f32 s18, s17", s18, s17, i32, f2u(INFINITY));
    TESTINSN_un_f32("vneg.f32 s30, s1",  s30, s1,  i32, f2u(-INFINITY));
    TESTINSN_un_f32("vneg.f32 s8,  s27", s8,  s27, i32, f2u(-INFINITY));
    TESTINSN_un_f32("vneg.f32 s20, s1",  s20, s1,  i32, f2u(76543.001002));
    TESTINSN_un_f32("vneg.f32 s28, s7",  s28, s7,  i32, f2u(-4856.234));
    TESTINSN_un_f32("vneg.f32 s2,  s19", s2, s19,  i32,f2u(87.098217));
    TESTINSN_un_f32("vneg.f32 s8,  s7",  s8, s7,  i32, f2u(-122156.2));

    printf("---- VMOV (register) ----\n");
    TESTINSN_un_f64("vmov.f64 d15, d4",  d15, d4,  i32, f2u0(NAN), f2u1(NAN));
    TESTINSN_un_f64("vmov.f64 d31, d4",  d31, d4,  i32, f2u0(NAN), f2u1(NAN));
    TESTINSN_un_f64("vmov.f64 d25, d25", d25, d24, i32, f2u0(INFINITY), f2u1(INFINITY));
    TESTINSN_un_f64("vmov.f64 d18, d17", d18, d17, i32, f2u0(INFINITY), f2u1(INFINITY));
    TESTINSN_un_f64("vmov.f64 d30, d1",  d30, d1,  i32, f2u0(-INFINITY), f2u1(-INFINITY));
    TESTINSN_un_f64("vmov.f64 d8,  d27", d8,  d27, i32, f2u0(-INFINITY), f2u1(-INFINITY));
    TESTINSN_un_f64("vmov.f64 d20, d1",  d20, d1,  i32, f2u0(76543.001002), f2u1(76543.001002));
    TESTINSN_un_f64("vmov.f64 d28, d7",  d28, d7,  i32, f2u0(-4856.234), f2u1(-4856.234));
    TESTINSN_un_f64("vmov.f64 d2,  d19", d2, d19,  i32,f2u0(87.098217), f2u1(87.098217));
    TESTINSN_un_f64("vmov.f64 d8,  d7",  d8, d7,  i32, f2u0(-122156.2), f2u1(-122156.2));
    TESTINSN_un_f32("vmov.f32 s15, s4",  s15, s4,  i32, f2u(NAN));
    TESTINSN_un_f32("vmov.f32 s31, s4",  s31, s4,  i32, f2u(NAN));
    TESTINSN_un_f32("vmov.f32 s25, s25", s25, s24, i32, f2u(INFINITY));
    TESTINSN_un_f32("vmov.f32 s18, s17", s18, s17, i32, f2u(INFINITY));
    TESTINSN_un_f32("vmov.f32 s30, s1",  s30, s1,  i32, f2u(-INFINITY));
    TESTINSN_un_f32("vmov.f32 s8,  s27", s8,  s27, i32, f2u(-INFINITY));
    TESTINSN_un_f32("vmov.f32 s20, s1",  s20, s1,  i32, f2u(76543.001002));
    TESTINSN_un_f32("vmov.f32 s28, s7",  s28, s7,  i32, f2u(-4856.234));
    TESTINSN_un_f32("vmov.f32 s2,  s19", s2, s19,  i32,f2u(87.098217));
    TESTINSN_un_f32("vmov.f32 s8,  s7",  s8, s7,  i32, f2u(-122156.2));

    printf("---- VSQRT ----\n");
    TESTINSN_un_f64("vsqrt.f64 d15, d4",  d15, d4,  i32, f2u0(NAN), f2u1(NAN));
    TESTINSN_un_f64("vsqrt.f64 d31, d4",  d31, d4,  i32, f2u0(NAN), f2u1(NAN));
    TESTINSN_un_f64("vsqrt.f64 d25, d25", d25, d24, i32, f2u0(INFINITY), f2u1(INFINITY));
    TESTINSN_un_f64("vsqrt.f64 d18, d17", d18, d17, i32, f2u0(INFINITY), f2u1(INFINITY));
    TESTINSN_un_f64("vsqrt.f64 d30, d1",  d30, d1,  i32, f2u0(-INFINITY), f2u1(-INFINITY));
    TESTINSN_un_f64("vsqrt.f64 d8,  d27", d8,  d27, i32, f2u0(-INFINITY), f2u1(-INFINITY));
    TESTINSN_un_f64("vsqrt.f64 d20, d1",  d20, d1,  i32, f2u0(76543.001002), f2u1(76543.001002));
    TESTINSN_un_f64("vsqrt.f64 d28, d7",  d28, d7,  i32, f2u0(-4856.234), f2u1(-4856.234));
    TESTINSN_un_f64("vsqrt.f64 d2,  d19", d2, d19,  i32,f2u0(87.098217), f2u1(87.098217));
    TESTINSN_un_f64("vsqrt.f64 d8,  d7",  d8, d7,  i32, f2u0(-122156.2), f2u1(-122156.2));
    TESTINSN_un_f32("vsqrt.f32 s15, s4",  s15, s4,  i32, f2u(NAN));
    TESTINSN_un_f32("vsqrt.f32 s31, s4",  s31, s4,  i32, f2u(NAN));
    TESTINSN_un_f32("vsqrt.f32 s25, s25", s25, s24, i32, f2u(INFINITY));
    TESTINSN_un_f32("vsqrt.f32 s18, s17", s18, s17, i32, f2u(INFINITY));
    TESTINSN_un_f32("vsqrt.f32 s30, s1",  s30, s1,  i32, f2u(-INFINITY));
    TESTINSN_un_f32("vsqrt.f32 s8,  s27", s8,  s27, i32, f2u(-INFINITY));
    TESTINSN_un_f32("vsqrt.f32 s20, s1",  s20, s1,  i32, f2u(76543.001002));
    TESTINSN_un_f32("vsqrt.f32 s28, s7",  s28, s7,  i32, f2u(-4856.234));
    TESTINSN_un_f32("vsqrt.f32 s2,  s19", s2, s19,  i32,f2u(87.098217));
    TESTINSN_un_f32("vsqrt.f32 s8,  s7",  s8, s7,  i32, f2u(-122156.2));

    printf("---- VCVT (integer <-> fp) ----\n");
    TESTINSN_un_f32("vcvt.u32.f32 s0,  s1",  s0,  s1,  i32, f2u(3.2));
    TESTINSN_un_f32("vcvt.u32.f32 s10, s11", s10, s11, i32, f2u(3e22));
    TESTINSN_un_f32("vcvt.u32.f32 s15, s4",  s15, s4,  i32, f2u(3e9));
    TESTINSN_un_f32("vcvt.u32.f32 s25, s24", s25, s24, i32, f2u(-0.5));
    TESTINSN_un_f32("vcvt.u32.f32 s19, s21", s19, s21, i32, f2u(-7.1));
    TESTINSN_un_f32("vcvt.u32.f32 s12, s8",  s12, s8,  i32, f2u(8.0 - 1.0/1024.0));
    TESTINSN_un_f32("vcvt.u32.f32 s12, s18", s12, s18, i32, f2u(-8.0 + 1.0/1024.0));
    TESTINSN_un_f32("vcvt.u32.f32 s30, s1",  s30, s1, i32, f2u(0.0));
    TESTINSN_un_f32("vcvt.u32.f32 s11, s1",  s11, s1, i32, f2u(INFINITY));
    TESTINSN_un_f32("vcvt.u32.f32 s21, s12", s21, s12, i32, f2u(-INFINITY));
    TESTINSN_un_f32("vcvt.u32.f32 s20, s11", s20, s11, i32, f2u(NAN));
    TESTINSN_un_f32("vcvt.s32.f32 s29, s13", s29, s13, i32, f2u(NAN));
    TESTINSN_un_f32("vcvt.s32.f32 s9,  s19", s9,  s19, i32, f2u(0.0));
    TESTINSN_un_f32("vcvt.s32.f32 s0,  s17", s0,  s17, i32, f2u(INFINITY));
    TESTINSN_un_f32("vcvt.s32.f32 s0,  s1",  s0,  s1, i32, f2u(-INFINITY));
    TESTINSN_un_f32("vcvt.s32.f32 s30, s11", s30, s11, i32, f2u(3.2));
    TESTINSN_un_f32("vcvt.s32.f32 s20, s21", s20, s21, i32, f2u(3e22));
    TESTINSN_un_f32("vcvt.s32.f32 s15, s14", s15, s14, i32, f2u(3e9));
    TESTINSN_un_f32("vcvt.s32.f32 s15, s24", s15, s24, i32, f2u(-0.5));
    TESTINSN_un_f32("vcvt.s32.f32 s15, s29", s15, s29, i32, f2u(-7.1));
    TESTINSN_un_f32("vcvt.s32.f32 s12, s31", s12, s31, i32, f2u(8.0 - 1.0/1024.0));
    TESTINSN_un_f32("vcvt.s32.f32 s1,  s8",  s1,  s8, i32, f2u(-8.0 + 1.0/1024.0));

    TESTINSN_un_f32("vcvt.f32.u32 s30, s1",  s30, s1, i32, f2u(7));
    TESTINSN_un_f32("vcvt.f32.u32 s10, s17", s10, s17, i32, f2u(1 << 31));
    TESTINSN_un_f32("vcvt.f32.u32 s20, s1",  s20, s1, i32, f2u((1U << 31) + 1));
    TESTINSN_un_f32("vcvt.f32.u32 s24, s26", s24, s26, i32, f2u((1U << 31) - 1));
    TESTINSN_un_f32("vcvt.f32.u32 s0,  s14", s0,  s14, i32, f2u(0x30a0bcef));
    TESTINSN_un_f32("vcvt.f32.u32 s11, s1",  s11, s1, i32, f2u(INFINITY));
    TESTINSN_un_f32("vcvt.f32.u32 s21, s12", s21, s12, i32, f2u(-INFINITY));
    TESTINSN_un_f32("vcvt.f32.u32 s29, s13", s29, s13, i32, f2u(NAN));
    TESTINSN_un_f32("vcvt.f32.s32 s0,  s1",  s0,  s1, i32, f2u(7));
    TESTINSN_un_f32("vcvt.f32.s32 s30, s31", s30, s31, i32, f2u(1 << 31));
    TESTINSN_un_f32("vcvt.f32.s32 s0,  s12", s0,  s12, i32, f2u((1U << 31) + 1));
    TESTINSN_un_f32("vcvt.f32.s32 s10, s16", s10, s16, i32, f2u((1U << 31) - 1));
    TESTINSN_un_f32("vcvt.f32.s32 s1,  s8",  s1,  s8, i32, f2u(-8.0 + 1.0/1024.0));
    TESTINSN_un_f32("vcvt.f32.s32 s29, s13", s29, s13, i32, f2u(NAN));
    TESTINSN_un_f32("vcvt.f32.s32 s9,  s19", s9,  s19, i32, f2u(0.0));
    TESTINSN_un_f32("vcvt.f32.s32 s0,  s17", s0,  s17, i32, f2u(INFINITY));
    TESTINSN_un_f32("vcvt.f32.s32 s0,  s1",  s0,  s1, i32, f2u(-INFINITY));

    TESTINSN_cvt_i32_f64("vcvt.u32.f64 s0,  d1",  s0,  d1,  f2u0(3.2), f2u1(3.2));
    TESTINSN_cvt_i32_f64("vcvt.u32.f64 s13, d26", s13, d26, f2u0(234.54), f2u1(234.54));
    TESTINSN_cvt_i32_f64("vcvt.u32.f64 s29, d30", s29, d30, f2u0(46245.345), f2u1(46245.345));
    TESTINSN_cvt_i32_f64("vcvt.u32.f64 s30, d21", s30, d21, f2u0(-8.0 + 1.0/1024.0), f2u1(-8.0 + 1.0/1024.0));
    TESTINSN_cvt_i32_f64("vcvt.u32.f64 s11, d8",  s11, d8,  f2u0(INFINITY), f2u1(INFINITY));
    TESTINSN_cvt_i32_f64("vcvt.u32.f64 s8,  d12", s8,  d12, f2u0(-INFINITY), f2u1(-INFINITY));
    TESTINSN_cvt_i32_f64("vcvt.u32.f64 s19, d7",  s19, d7,  f2u0(NAN), f2u1(NAN));
    TESTINSN_cvt_i32_f64("vcvt.u32.f64 s16, d16", s16, d16, f2u0(76.67), f2u1(76.67));
    TESTINSN_cvt_i32_f64("vcvt.s32.f64 s0,  d1",  s0,  d1,  f2u0(3.2), f2u1(3.2));
    TESTINSN_cvt_i32_f64("vcvt.s32.f64 s13, d26", s13, d26, f2u0(234.54), f2u1(234.54));
    TESTINSN_cvt_i32_f64("vcvt.s32.f64 s29, d30", s29, d30, f2u0(46245.345), f2u1(46245.345));
    TESTINSN_cvt_i32_f64("vcvt.s32.f64 s30, d21", s30, d21, f2u0(-8.0 + 1.0/1024.0), f2u1(-8.0 + 1.0/1024.0));
    TESTINSN_cvt_i32_f64("vcvt.s32.f64 s11, d8",  s11, d8,  f2u0(INFINITY), f2u1(INFINITY));
    TESTINSN_cvt_i32_f64("vcvt.s32.f64 s8,  d12", s8,  d12, f2u0(-INFINITY), f2u1(-INFINITY));
    TESTINSN_cvt_i32_f64("vcvt.s32.f64 s19, d7",  s19, d7,  f2u0(NAN), f2u1(NAN));
    TESTINSN_cvt_i32_f64("vcvt.s32.f64 s16, d16", s16, d16, f2u0(76.67), f2u1(76.67));

    TESTINSN_cvt_f64_i32("vcvt.f64.u32 d0,  s1",  d0,  s1,  f2u(3.2));
    TESTINSN_cvt_f64_i32("vcvt.f64.u32 d30, s21", d30, s21, f2u(-656.42));
    TESTINSN_cvt_f64_i32("vcvt.f64.u32 d16, s12", d16, s12, f2u(870.024));
    TESTINSN_cvt_f64_i32("vcvt.f64.u32 d29, s7",  d29, s7,  f2u(-2543.4506));
    TESTINSN_cvt_f64_i32("vcvt.f64.u32 d12, s28", d12, s28, f2u(5.00003245));
    TESTINSN_cvt_f64_i32("vcvt.f64.u32 d7,  s5",  d7,  s5,  f2u(-INFINITY));
    TESTINSN_cvt_f64_i32("vcvt.f64.u32 d21, s20", d21, s20, f2u(INFINITY));
    TESTINSN_cvt_f64_i32("vcvt.f64.u32 d11, s11", d11, s11, f2u(NAN));
    TESTINSN_cvt_f64_i32("vcvt.f64.s32 d0,  s1",  d0,  s1,  f2u(3.2));
    TESTINSN_cvt_f64_i32("vcvt.f64.s32 d30, s21", d30, s21, f2u(-656.42));
    TESTINSN_cvt_f64_i32("vcvt.f64.s32 d16, s12", d16, s12, f2u(870.024));
    TESTINSN_cvt_f64_i32("vcvt.f64.s32 d29, s7",  d29, s7,  f2u(-2543.4506));
    TESTINSN_cvt_f64_i32("vcvt.f64.s32 d12, s28", d12, s28, f2u(5.00003245));
    TESTINSN_cvt_f64_i32("vcvt.f64.s32 d7,  s5",  d7,  s5,  f2u(-INFINITY));
    TESTINSN_cvt_f64_i32("vcvt.f64.s32 d21, s20", d21, s20, f2u(INFINITY));
    TESTINSN_cvt_f64_i32("vcvt.f64.s32 d11, s11", d11, s11, f2u(NAN));

/*    printf("---- VCVT (fixed <-> fp) ----\n");
    TESTINSN_un_f32("vcvt.u32.f32 s0,  s0,  #3",  s0,  s0, i32, f2u(3.2));
    TESTINSN_un_f32("vcvt.u32.f32 s11, s11, #1",  s11, s11, i32, f2u(3e22));
    TESTINSN_un_f32("vcvt.u32.f32 s15, s15, #32", s15, s15, i32, f2u(3e9));
    TESTINSN_un_f32("vcvt.u32.f32 s4,  s4,  #7",  s4,  s4, i32, f2u(-0.5));
    TESTINSN_un_f32("vcvt.u32.f32 s6,  s6,  #4",  s6,  s6, i32, f2u(-7.1));
    TESTINSN_un_f32("vcvt.u32.f32 s12, s12, #3",  s12, s12, i32, f2u(8.0 - 1.0/1024.0));
    TESTINSN_un_f32("vcvt.u32.f32 s8,  s8,  #3",  s8,  s8, i32, f2u(-8.0 + 1.0/1024.0));
    TESTINSN_un_f32("vcvt.u32.f32 s30, s30, #3",  s30, s30, i32, f2u(NAN));
    TESTINSN_un_f32("vcvt.u32.f32 s20, s20, #3",  s20, s20, i32, f2u(0.0));
    TESTINSN_un_f32("vcvt.u32.f32 s13, s13, #6",  s13, s13, i32, f2u(INFINITY));
    TESTINSN_un_f32("vcvt.u32.f32 s16, s16, #3",  s16, s16, i32, f2u(-INFINITY));
    TESTINSN_un_f32("vcvt.s32.f32 s1,  s1,  #5",  s1,  s1, i32, f2u(3.2));
    TESTINSN_un_f32("vcvt.s32.f32 s21, s21, #1",  s21, s21, i32, f2u(3e22));
    TESTINSN_un_f32("vcvt.s32.f32 s17, s17, #8",  s17, s17, i32, f2u(3e9));
    TESTINSN_un_f32("vcvt.s32.f32 s27, s27, #2",  s27, s27, i32, f2u(-0.5));
    TESTINSN_un_f32("vcvt.s32.f32 s15, s15, #1",  s15, s15, i32, f2u(-7.1));
    TESTINSN_un_f32("vcvt.s32.f32 s8,  s8,  #2",  s8,  s8, i32, f2u(8.0 - 1.0/1024.0));
    TESTINSN_un_f32("vcvt.s32.f32 s31, s31, #2",  s31, s31, i32, f2u(-8.0 + 1.0/1024.0));
    TESTINSN_un_f32("vcvt.s32.f32 s10, s10, #3",  s10, s10, i32, f2u(0.0));
    TESTINSN_un_f32("vcvt.s32.f32 s13, s13, #9",  s13, s13, i32, f2u(INFINITY));
    TESTINSN_un_f32("vcvt.s32.f32 s22, s22, #3",  s22, s22, i32, f2u(-INFINITY));
    TESTINSN_un_f32("vcvt.s32.f32 s1,  s1,  #7",  s1,  s1, i32, f2u(NAN));

    TESTINSN_un_f32("vcvt.f32.u32 s0,  s0,  #3",  s0,  s0, i32, f2u(3.2));
    TESTINSN_un_f32("vcvt.f32.u32 s11, s11, #1",  s11, s11, i32, f2u(3e22));
    TESTINSN_un_f32("vcvt.f32.u32 s15, s15, #32", s15, s15, i32, f2u(3e9));
    TESTINSN_un_f32("vcvt.f32.u32 s4,  s4,  #7",  s4,  s4, i32, f2u(-0.5));
    TESTINSN_un_f32("vcvt.f32.u32 s6,  s6,  #4",  s6,  s6, i32, f2u(-7.1));
    TESTINSN_un_f32("vcvt.f32.u32 s12, s12, #3",  s12, s12, i32, f2u(8.0 - 1.0/1024.0));
    TESTINSN_un_f32("vcvt.f32.u32 s8,  s8,  #3",  s8,  s8, i32, f2u(-8.0 + 1.0/1024.0));
    TESTINSN_un_f32("vcvt.f32.u32 s30, s30, #3",  s30, s30, i32, f2u(NAN));
    TESTINSN_un_f32("vcvt.f32.u32 s20, s20, #3",  s20, s20, i32, f2u(0.0));
    TESTINSN_un_f32("vcvt.f32.u32 s13, s13, #6",  s13, s13, i32, f2u(INFINITY));
    TESTINSN_un_f32("vcvt.f32.u32 s16, s16, #3",  s16, s16, i32, f2u(-INFINITY));
    TESTINSN_un_f32("vcvt.f32.s32 s1,  s1,  #5",  s1,  s1, i32, f2u(3.2));
    TESTINSN_un_f32("vcvt.f32.s32 s21, s21, #1",  s21, s21, i32, f2u(3e22));
    TESTINSN_un_f32("vcvt.f32.s32 s17, s17, #8",  s17, s17, i32, f2u(3e9));
    TESTINSN_un_f32("vcvt.f32.s32 s27, s27, #2",  s27, s27, i32, f2u(-0.5));
    TESTINSN_un_f32("vcvt.f32.s32 s15, s15, #1",  s15, s15, i32, f2u(-7.1));
    TESTINSN_un_f32("vcvt.f32.s32 s8,  s8,  #2",  s8,  s8, i32, f2u(8.0 - 1.0/1024.0));
    TESTINSN_un_f32("vcvt.f32.s32 s31, s31, #2",  s31, s31, i32, f2u(-8.0 + 1.0/1024.0));
    TESTINSN_un_f32("vcvt.f32.s32 s10, s10, #3",  s10, s10, i32, f2u(0.0));
    TESTINSN_un_f32("vcvt.f32.s32 s13, s13, #9",  s13, s13, i32, f2u(INFINITY));
    TESTINSN_un_f32("vcvt.f32.s32 s22, s22, #3",  s22, s22, i32, f2u(-INFINITY));
    TESTINSN_un_f32("vcvt.f32.s32 s1,  s1,  #7",  s1,  s1, i32, f2u(NAN));
    */

    printf("---- VCVT (single <-> double) ----\n");
    TESTINSN_un_cvt_ds("vcvt.f64.f32 d0,  s1",  d0,  s1,  f2u(3.2));
    TESTINSN_un_cvt_ds("vcvt.f64.f32 d29, s21",  d29, s21, f2u(234.65));
    TESTINSN_un_cvt_ds("vcvt.f64.f32 d16, s30",  d16, s30, f2u(-700.63));
    TESTINSN_un_cvt_ds("vcvt.f64.f32 d11, s7",  d11, s7,  f2u(8.0 - 1.0/1024.0));
    TESTINSN_un_cvt_ds("vcvt.f64.f32 d30, s3",  d30, s3,  f2u(-8.0 + 1.0/1024.0));
    TESTINSN_un_cvt_ds("vcvt.f64.f32 d7,  s19", d7,  s19, f2u(12.43303));
    TESTINSN_un_cvt_ds("vcvt.f64.f32 d2,  s11",  d2,  s11, f2u(65.4235));
    TESTINSN_un_cvt_ds("vcvt.f64.f32 d9,  s21",  d9,  s21, f2u(NAN));
    TESTINSN_un_cvt_ds("vcvt.f64.f32 d17, s29",  d17, s29, f2u(-INFINITY));
    TESTINSN_un_cvt_ds("vcvt.f64.f32 d19, s0",  d19, s0,  f2u(INFINITY));
    TESTINSN_un_cvt_sd("vcvt.f32.f64 s0,  d1",  s0,  d1,  f2u0(3.2), f2u1(3.2));
    TESTINSN_un_cvt_sd("vcvt.f32.f64 s29, d21",  s29, d21, f2u0(234.65), f2u1(234.65));
    TESTINSN_un_cvt_sd("vcvt.f32.f64 s16, d30",  s16, d30, f2u0(-700.63), f2u1(-700.63));
    TESTINSN_un_cvt_sd("vcvt.f32.f64 s11, d7",  s11, d7,  f2u0(8.0 - 1.0/1024.0), f2u1(8.0 - 1.0/1024.0));
    TESTINSN_un_cvt_sd("vcvt.f32.f64 s30, d3",  s30, d3,  f2u0(-8.0 + 1.0/1024.0), f2u1(-8.0 + 1.0/1024.0));
    TESTINSN_un_cvt_sd("vcvt.f32.f64 s7,  d19", s7,  d19, f2u0(12.43303), f2u1(12.43303));
    TESTINSN_un_cvt_sd("vcvt.f32.f64 s2,  d11",  s2,  d11, f2u0(65.4235), f2u1(65.4235));
    TESTINSN_un_cvt_sd("vcvt.f32.f64 s9,  d21",  s9,  d21, f2u0(NAN), f2u1(NAN));
    TESTINSN_un_cvt_sd("vcvt.f32.f64 s17, d29",  s17, d29, f2u0(-INFINITY), f2u1(-INFINITY));
    TESTINSN_un_cvt_sd("vcvt.f32.f64 s19, d0",  s19, d0,  f2u0(INFINITY), f2u1(INFINITY));

    printf("---- VCMP ----\n");
    TESTINSN_cmp_f64("vcmp.f64  d0,  d19",  d0, f2u0(-3.4567), f2u1(-3.4567), d19, f2u0(-2.6245), f2u1(-2.6245));
    TESTINSN_cmp_f64("vcmp.f64  d11, d16",  d11, f2u0(23475.45), f2u1(23475.45), d16, f2u0(3425.5), f2u1(3425.5));
    TESTINSN_cmp_f64("vcmp.f64  d21, d30",  d21, f2u0(-4524.5), f2u1(-4524.5), d30, f2u0(-452345.5), f2u1(-452345.5));
    TESTINSN_cmp_f64("vcmp.f64  d7,  d28",  d7,  f2u0(425.5), f2u1(425.5), d28, f2u0(-456.3), f2u1(-456.3));
    TESTINSN_cmp_f64("vcmp.f64  d29, d3",   d29, f2u0(INFINITY), f2u1(INFINITY), d3, f2u0(34562.45), f2u1(34562.45));
    TESTINSN_cmp_f64("vcmp.f64  d3,  d22",  d3,  f2u0(2.0), f2u1(2.0), d22, f2u0(2.0), f2u1(2.0));
    TESTINSN_cmp_f64("vcmp.f64  d3,  d22",  d3,  f2u0(12.023), f2u1(12.023), d22, f2u0(12.023), f2u1(12.023));
    TESTINSN_cmp_f64("vcmp.f64  d3,  d22",  d3,  f2u0(0.0), f2u1(0.0), d22, f2u0(0.0), f2u1(0.0));
    TESTINSN_cmp_f64("vcmp.f64  d9,  d2",   d9,  f2u0(INFINITY), f2u1(INFINITY), d2, f2u0(INFINITY), f2u1(INFINITY));
    TESTINSN_cmp_f64("vcmp.f64  d30, d15",  d30, f2u0(-INFINITY), f2u1(-INFINITY), d15, f2u0(-INFINITY), f2u1(-INFINITY));
    TESTINSN_cmp_f64("vcmp.f64  d0,  d19",  d0,  f2u0(-3.4567), f2u1(-3.4567), d19, f2u0(-2.6245), f2u1(-2.6245));
    TESTINSN_cmp_f64("vcmp.f64  d11, d16",  d11, f2u0(-5463.7), f2u1(-5463.7), d16, f2u0(-INFINITY), f2u1(-INFINITY));
    TESTINSN_cmp_f64("vcmp.f64  d21, d30",  d21, f2u0(-INFINITY), f2u1(-INFINITY), d30, f2u0(86.7), f2u1(86.7));
    TESTINSN_cmp_f64("vcmp.f64  d7,  d28",  d7,  f2u0(INFINITY), f2u1(INFINITY), d28, f2u0(-8567.456), f2u1(-8567.456));
    TESTINSN_cmp_f64("vcmp.f64  d29, d3",   d29, f2u0(-524.4), f2u1(-524.4), d3, f2u0(654.5), f2u1(654.5));
    TESTINSN_cmp_f64("vcmp.f64  d3,  d22",  d3,  f2u0(NAN), f2u1(NAN), d22, f2u0(-6.46524), f2u1(-6.46524));
    TESTINSN_cmp_f64("vcmp.f64  d9,  d2",   d9,  f2u0(56.563), f2u1(56.563), d2, f2u0(56.56), f2u1(56.56));
    TESTINSN_cmp_f64("vcmp.f64  d30, d15",  d30, f2u0(5365.60001), f2u1(5365.60001), d15, f2u0(56763.5), f2u1(56763.5));
    TESTINSN_cmp_f64("vcmpe.f64 d0,  d19",  d0,  f2u0(-3.4567), f2u1(-3.4567), d19, f2u0(-2.6245), f2u1(-2.6245));
    TESTINSN_cmp_f64("vcmpe.f64 d11, d16",  d11, f2u0(23475.45), f2u1(23475.45), d16, f2u0(3425.5), f2u1(3425.5));
    TESTINSN_cmp_f64("vcmpe.f64 d11, d16",  d11, f2u0(23475.45), f2u1(23475.45), d16, f2u0(NAN), f2u1(NAN));
    TESTINSN_cmp_f64("vcmpe.f64 d21, d30",  d21, f2u0(-4524.5), f2u1(-4524.5), d30, f2u0(-452345.5), f2u1(-452345.5));
    TESTINSN_cmp_f64("vcmpe.f64 d7,  d28",  d7,  f2u0(425.5), f2u1(425.5), d28, f2u0(-456.3), f2u1(-456.3));
    TESTINSN_cmp_f64("vcmpe.f64 d29, d3",   d29, f2u0(INFINITY), f2u1(INFINITY), d3, f2u0(34562.45), f2u1(34562.45));
    TESTINSN_cmp_f64("vcmpe.f64 d3,  d22",  d3,  f2u0(2.0), f2u1(2.0), d22, f2u0(2.0), f2u1(2.0));
    TESTINSN_cmp_f64("vcmpe.f64 d9,  d2",   d9,  f2u0(INFINITY), f2u1(INFINITY), d2, f2u0(INFINITY), f2u1(INFINITY));
    TESTINSN_cmp_f64("vcmpe.f64 d30, d15",  d30, f2u0(-INFINITY), f2u1(-INFINITY), d15, f2u0(-INFINITY), f2u1(-INFINITY));
    TESTINSN_cmp_f64("vcmpe.f64 d0,  d19",  d0,  f2u0(-3.4567), f2u1(-3.4567), d19, f2u0(-2.6245), f2u1(-2.6245));
    TESTINSN_cmp_f64("vcmpe.f64 d11, d16",  d11, f2u0(-5463.7), f2u1(-5463.7), d16, f2u0(-INFINITY), f2u1(-INFINITY));
    TESTINSN_cmp_f64("vcmpe.f64 d21, d30",  d21, f2u0(-INFINITY), f2u1(-INFINITY), d30, f2u0(86.7), f2u1(86.7));
    TESTINSN_cmp_f64("vcmpe.f64 d7,  d28",  d7,  f2u0(INFINITY), f2u1(INFINITY), d28, f2u0(-8567.456), f2u1(-8567.456));
    TESTINSN_cmp_f64("vcmpe.f64 d29, d3",   d29, f2u0(-524.4), f2u1(-524.4), d3, f2u0(654.5), f2u1(654.5));
    TESTINSN_cmp_f64("vcmpe.f64 d3,  d22",  d3,  f2u0(4624.5), f2u1(4624.5), d22, f2u0(-6.46524), f2u1(-6.46524));
    TESTINSN_cmp_f64("vcmpe.f64 d9,  d2",   d9,  f2u0(56.563), f2u1(56.563), d2, f2u0(56.56), f2u1(56.56));
    TESTINSN_cmp_f64("vcmpe.f64 d30, d15",  d30, f2u0(5365.60001), f2u1(5365.60001), d15, f2u0(56763.5), f2u1(56763.5));

    TESTINSN_cmp_f32("vcmp.f32  s0,  s19",  s0,  f2u(-3.4567),  s19, f2u(-2.6245));
    TESTINSN_cmp_f32("vcmp.f32  s11, s16",  s11, f2u(23475.45), s16, f2u(3425.5));
    TESTINSN_cmp_f32("vcmp.f32  s3,  s22",  s3,  f2u(2.0),      s22, f2u(2.0));
    TESTINSN_cmp_f32("vcmp.f32  s0,  s19",  s0,  f2u(-3.4567),  s19, f2u(-2.6245));
    TESTINSN_cmp_f32("vcmp.f32  s11, s16",  s11, f2u(23475.45), s16, f2u(3425.5));
    TESTINSN_cmp_f32("vcmp.f32  s21, s30",  s21, f2u(-4524.5),  s30, f2u(-452345.5));
    TESTINSN_cmp_f32("vcmp.f32  s7,  s28",  s7,  f2u(425.5),    s28, f2u(-456.3));
    TESTINSN_cmp_f32("vcmp.f32  s29, s3",   s29, f2u(INFINITY), s3, f2u(34562.45));
    TESTINSN_cmp_f32("vcmp.f32  s3,  s22",  s3,  f2u(12.023),   s22, f2u(12.023));
    TESTINSN_cmp_f32("vcmp.f32  s3,  s22",  s3,  f2u(0.0),      s22, f2u(0.0));
    TESTINSN_cmp_f32("vcmp.f32  s9,  s2",   s9,  f2u(INFINITY), s2, f2u(INFINITY));
    TESTINSN_cmp_f32("vcmp.f32  s30, s15",  s30, f2u(-INFINITY),s15, f2u(-INFINITY));
    TESTINSN_cmp_f32("vcmp.f32  s0,  s19",  s0,  f2u(-3.4567),  s19, f2u(-2.6245));
    TESTINSN_cmp_f32("vcmp.f32  s11, s16",  s11, f2u(-5463.7),  s16, f2u(-INFINITY));
    TESTINSN_cmp_f32("vcmp.f32  s21, s30",  s21, f2u(-INFINITY),s30, f2u(86.7));
    TESTINSN_cmp_f32("vcmp.f32  s7,  s28",  s7,  f2u(INFINITY), s28, f2u(-8567.456));
    TESTINSN_cmp_f32("vcmp.f32  s29, s3",   s29, f2u(-524.4),   s3, f2u(654.5));
    TESTINSN_cmp_f32("vcmp.f32  s3,  s22",  s3,  f2u(NAN),      s22, f2u(-6.46524));
    TESTINSN_cmp_f32("vcmp.f32  s9,  s2",   s9,  f2u(56.563),   s2, f2u(56.56));
    TESTINSN_cmp_f32("vcmp.f32  s30, s15",  s30, f2u(5365.60001), s15, f2u(56763.5));
    TESTINSN_cmp_f32("vcmpe.f32 s0,  s19",  s0,  f2u(-3.4567),  s19, f2u(-2.6245));
    TESTINSN_cmp_f32("vcmpe.f32 s11, s16",  s11, f2u(23475.45), s16, f2u(3425.5));
    TESTINSN_cmp_f32("vcmpe.f32 s11, s16",  s11, f2u(23475.45), s16, f2u(NAN));
    TESTINSN_cmp_f32("vcmpe.f32 s21, s30",  s21, f2u(-4524.5),  s30, f2u(-452345.5));
    TESTINSN_cmp_f32("vcmpe.f32 s7,  s28",  s7,  f2u(425.5),    s28, f2u(-456.3));
    TESTINSN_cmp_f32("vcmpe.f32 s29, s3",   s29, f2u(INFINITY), s3, f2u(34562.45));
    TESTINSN_cmp_f32("vcmpe.f32 s3,  s22",  s3,  f2u(2.0),      s22, f2u(2.0));
    TESTINSN_cmp_f32("vcmpe.f32 s9,  s2",   s9,  f2u(INFINITY), s2, f2u(INFINITY));
    TESTINSN_cmp_f32("vcmpe.f32 s30, s15",  s30, f2u(-INFINITY), s15, f2u(-INFINITY));
    TESTINSN_cmp_f32("vcmpe.f32 s0,  s19",  s0,  f2u(-3.4567),  s19, f2u(-2.6245));
    TESTINSN_cmp_f32("vcmpe.f32 s11, s16",  s11, f2u(-5463.7),  s16, f2u(-INFINITY));
    TESTINSN_cmp_f32("vcmpe.f32 s21, s30",  s21, f2u(-INFINITY), s30, f2u(86.7));
    TESTINSN_cmp_f32("vcmpe.f32 s7,  s28",  s7,  f2u(INFINITY), s28, f2u(-8567.456));
    TESTINSN_cmp_f32("vcmpe.f32 s29, s3",   s29, f2u(-524.4),   s3, f2u(654.5));
    TESTINSN_cmp_f32("vcmpe.f32 s3,  s22",  s3,  f2u(4624.5),   s22, f2u(-6.46524));
    TESTINSN_cmp_f32("vcmpe.f32 s9,  s2",   s9,  f2u(56.563),   s2, f2u(56.56));
    TESTINSN_cmp_f32("vcmpe.f32 s9,  s2",   s9,  f2u(0.0),   s2, f2u(56.56));
    TESTINSN_cmp_f32("vcmpe.f32 s9,  s2",   s9,  f2u(10.0),   s2, f2u(0.0));
    TESTINSN_cmp_f32("vcmpe.f32 s9,  s2",   s9,  f2u(0.0),   s2, f2u(0.0));
    TESTINSN_cmp_f32("vcmpe.f32 s9,  s2",   s9,  f2u(0.0),   s2, f2u(0.0));

    printf("---- VCMP (zero) ----\n");
    TESTINSN_cmpz_f64("vcmp.f64 d0",  d0,  f2u0(-3.4567), f2u1(-3.4567));
    TESTINSN_cmpz_f64("vcmp.f64 d11", d11, f2u0(23475.45), f2u1(23475.45));
    TESTINSN_cmpz_f64("vcmp.f64 d21", d21, f2u0(-4524.5), f2u1(-4524.5));
    TESTINSN_cmpz_f64("vcmp.f64 d7",  d7,  f2u0(425.5), f2u1(425.5));
    TESTINSN_cmpz_f64("vcmp.f64 d29", d29, f2u0(INFINITY), f2u1(INFINITY));
    TESTINSN_cmpz_f64("vcmp.f64 d3",  d3,  f2u0(2.0), f2u1(2.0));
    TESTINSN_cmpz_f64("vcmp.f64 d3",  d3,  f2u0(0.0), f2u1(0.0));
    TESTINSN_cmpz_f64("vcmp.f64 d9",  d9,  f2u0(INFINITY), f2u1(INFINITY));
    TESTINSN_cmpz_f64("vcmp.f64 d30", d30, f2u0(-INFINITY), f2u1(-INFINITY));
    TESTINSN_cmpz_f64("vcmp.f64 d0",  d0,  f2u0(-3.4567), f2u1(-3.4567));
    TESTINSN_cmpz_f64("vcmp.f64 d11", d11, f2u0(-5463.7), f2u1(-5463.7));
    TESTINSN_cmpz_f64("vcmp.f64 d21", d21, f2u0(-INFINITY), f2u1(-INFINITY));
    TESTINSN_cmpz_f64("vcmp.f64 d7",  d7,  f2u0(INFINITY), f2u1(INFINITY));
    TESTINSN_cmpz_f64("vcmp.f64 d29", d29, f2u0(-524.4), f2u1(-524.4));
    TESTINSN_cmpz_f64("vcmp.f64 d3",  d3,  f2u0(4624.5), f2u1(4624.5));
    TESTINSN_cmpz_f64("vcmp.f64 d9",  d9,  f2u0(NAN), f2u1(NAN));
    TESTINSN_cmpz_f64("vcmp.f64 d30", d30, f2u0(5365.60001), f2u1(5365.60001));

    TESTINSN_cmpz_f64("vcmpe.f64 d0",  d0,  f2u0(-3.4567), f2u1(-3.4567));
    TESTINSN_cmpz_f64("vcmpe.f64 d11", d11, f2u0(23475.45), f2u1(23475.45));
    TESTINSN_cmpz_f64("vcmpe.f64 d21", d21, f2u0(-4524.5), f2u1(-4524.5));
    TESTINSN_cmpz_f64("vcmpe.f64 d7",  d7,  f2u0(425.5), f2u1(425.5));
    TESTINSN_cmpz_f64("vcmpe.f64 d29", d29, f2u0(INFINITY), f2u1(INFINITY));
    TESTINSN_cmpz_f64("vcmpe.f64 d3",  d3,  f2u0(2.0), f2u1(2.0));
    TESTINSN_cmpz_f64("vcmpe.f64 d3",  d3,  f2u0(0.0), f2u1(0.0));
    TESTINSN_cmpz_f64("vcmpe.f64 d9",  d9,  f2u0(INFINITY), f2u1(INFINITY));
    TESTINSN_cmpz_f64("vcmpe.f64 d30", d30, f2u0(-INFINITY), f2u1(-INFINITY));
    TESTINSN_cmpz_f64("vcmpe.f64 d0",  d0,  f2u0(-3.4567), f2u1(-3.4567));
    TESTINSN_cmpz_f64("vcmpe.f64 d11", d11, f2u0(-5463.7), f2u1(-5463.7));
    TESTINSN_cmpz_f64("vcmpe.f64 d21", d21, f2u0(-INFINITY), f2u1(-INFINITY));
    TESTINSN_cmpz_f64("vcmpe.f64 d7",  d7,  f2u0(INFINITY), f2u1(INFINITY));
    TESTINSN_cmpz_f64("vcmpe.f64 d29", d29, f2u0(-524.4), f2u1(-524.4));
    TESTINSN_cmpz_f64("vcmpe.f64 d3",  d3,  f2u0(4624.5), f2u1(4624.5));
    TESTINSN_cmpz_f64("vcmpe.f64 d9",  d9,  f2u0(NAN), f2u1(NAN));
    TESTINSN_cmpz_f64("vcmpe.f64 d30", d30, f2u0(5365.60001), f2u1(5365.60001));

    TESTINSN_cmpz_f32("vcmp.f32 s0",  s0,  f2u(-3.4567));
    TESTINSN_cmpz_f32("vcmp.f32 s11", s11, f2u(23475.45));
    TESTINSN_cmpz_f32("vcmp.f32 s21", s21, f2u(-4524.5));
    TESTINSN_cmpz_f32("vcmp.f32 s7",  s7,  f2u(425.5));
    TESTINSN_cmpz_f32("vcmp.f32 s29", s29, f2u(INFINITY));
    TESTINSN_cmpz_f32("vcmp.f32 s3",  s3,  f2u(2.0));
    TESTINSN_cmpz_f32("vcmp.f32 s3",  s3,  f2u(0.0));
    TESTINSN_cmpz_f32("vcmp.f32 s9",  s9,  f2u(INFINITY));
    TESTINSN_cmpz_f32("vcmp.f32 s30", s30, f2u(-INFINITY));
    TESTINSN_cmpz_f32("vcmp.f32 s0",  s0,  f2u(-3.4567));
    TESTINSN_cmpz_f32("vcmp.f32 s11", s11, f2u(-5463.7));
    TESTINSN_cmpz_f32("vcmp.f32 s21", s21, f2u(-INFINITY));
    TESTINSN_cmpz_f32("vcmp.f32 s7",  s7,  f2u(INFINITY));
    TESTINSN_cmpz_f32("vcmp.f32 s29", s29, f2u(-524.4));
    TESTINSN_cmpz_f32("vcmp.f32 s3",  s3,  f2u(4624.5));
    TESTINSN_cmpz_f32("vcmp.f32 s9",  s9,  f2u(NAN));
    TESTINSN_cmpz_f32("vcmp.f32 s30", s30, f2u(5365.60001));
    TESTINSN_cmpz_f32("vcmpe.f32 s0",  s0,  f2u(-3.4567));
    TESTINSN_cmpz_f32("vcmpe.f32 s11", s11, f2u(23475.45));
    TESTINSN_cmpz_f32("vcmpe.f32 s21", s21, f2u(-4524.5));
    TESTINSN_cmpz_f32("vcmpe.f32 s7",  s7,  f2u(425.5));
    TESTINSN_cmpz_f32("vcmpe.f32 s29", s29, f2u(INFINITY));
    TESTINSN_cmpz_f32("vcmpe.f32 s3",  s3,  f2u(2.0));
    TESTINSN_cmpz_f32("vcmpe.f32 s3",  s3,  f2u(0.0));
    TESTINSN_cmpz_f32("vcmpe.f32 s9",  s9,  f2u(INFINITY));
    TESTINSN_cmpz_f32("vcmpe.f32 s30", s30, f2u(-INFINITY));
    TESTINSN_cmpz_f32("vcmpe.f32 s0",  s0,  f2u(-3.4567));
    TESTINSN_cmpz_f32("vcmpe.f32 s11", s11, f2u(-5463.7));
    TESTINSN_cmpz_f32("vcmpe.f32 s21", s21, f2u(-INFINITY));
    TESTINSN_cmpz_f32("vcmpe.f32 s7",  s7,  f2u(INFINITY));
    TESTINSN_cmpz_f32("vcmpe.f32 s29", s29, f2u(-524.4));
    TESTINSN_cmpz_f32("vcmpe.f32 s3",  s3,  f2u(4624.5));
    TESTINSN_cmpz_f32("vcmpe.f32 s9",  s9,  f2u(NAN));
    TESTINSN_cmpz_f32("vcmpe.f32 s30", s30, f2u(5365.60001));

    int numbers[8] ={ 0xaa0, 0xbb1, 0xcc2, 0xdd3, 0x11a, 0x22b, 0x33c, 0x44d };

    printf("---- VLDR ----\n");
    TESTINSN_vldr_f64("vldr d9,  [r6, #+4]",  d9,  r6,  (long) numbers + 8, 4);
    TESTINSN_vldr_f64("vldr d16, [r9, #-4]",  d16, r9,  (long) numbers + 8, -4);
    TESTINSN_vldr_f64("vldr d30, [r12]",      d30, r12, (long) numbers + 8, 0);
    TESTINSN_vldr_f64("vldr d22, [r9, #+8]", d22, r9, (long) numbers + 8, 8);
    TESTINSN_vldr_f64("vldr d29, [r2, #-8]",  d29, r2,  (long) numbers + 8, -8);
    TESTINSN_vldr_f64("vldr d8,  [r8, #+8]",  d8,  r8,  (long) numbers + 8, 8);
    TESTINSN_vldr_f64("vldr d11, [r12, #-4]", d11, r12, (long) numbers + 8, -4);
    TESTINSN_vldr_f64("vldr d18, [r3]",       d18, r3,  (long) numbers + 8, 0);
    TESTINSN_vldr_f64("vldr d5,  [r10, #+8]", d5,  r10, (long) numbers + 8, 8);
    TESTINSN_vldr_f64("vldr d17, [r10]",      d17, r10, (long) numbers + 8, 0);
    TESTINSN_vldr_f64("vldr d9,  [r9, #-4]", d9,  r9, (long) numbers + 8, -4);
    TESTINSN_vldr_f64("vldr d29, [r4, #-8]",  d29, r4,  (long) numbers + 8, -8);
    TESTINSN_vldr_f64("vldr d21, [r6, #+4]",  d21, r6,  (long) numbers + 8, 4);
    TESTINSN_vldr_f64("vldr d8,  [r4]",       d8,  r4,  (long) numbers + 8, 0);
    TESTINSN_vldr_f64("vldr d19, [r0, #-8]",  d19, r0,  (long) numbers + 8, -8);
    TESTINSN_vldr_f64("vldr d10, [r3, #+4]",  d10, r3,  (long) numbers + 8, 4);
    TESTINSN_vldr_f32("vldr s10, [r3, #+4]",  s10, r3,  (long) numbers + 8, 4);
    TESTINSN_vldr_f32("vldr s9,  [r6, #+4]",  s9,  r6,  (long) numbers + 8, 4);
    TESTINSN_vldr_f32("vldr s16, [r9, #-4]",  s16, r9,  (long) numbers + 8, -4);
    TESTINSN_vldr_f32("vldr s30, [r12]",      s30, r12, (long) numbers + 8, 0);
    TESTINSN_vldr_f32("vldr s22, [r9, #+8]", s22, r9, (long) numbers + 8, 8);
    TESTINSN_vldr_f32("vldr s29, [r2, #-8]",  s29, r2,  (long) numbers + 8, -8);
    TESTINSN_vldr_f32("vldr s8,  [r8, #+8]",  s8,  r8,  (long) numbers + 8, 8);
    TESTINSN_vldr_f32("vldr s11, [r12, #-4]", s11, r12, (long) numbers + 8, -4);
    TESTINSN_vldr_f32("vldr s18, [r3]",       s18, r3,  (long) numbers + 8, 0);
    TESTINSN_vldr_f32("vldr s5,  [r10, #+8]", s5,  r10, (long) numbers + 8, 8);
    TESTINSN_vldr_f32("vldr s17, [r10]",      s17, r10, (long) numbers + 8, 0);
    TESTINSN_vldr_f32("vldr s9,  [r9, #-4]", s9,  r9, (long) numbers + 8, -4);
    TESTINSN_vldr_f32("vldr s29, [r4, #-8]",  s29, r4,  (long) numbers + 8, -8);
    TESTINSN_vldr_f32("vldr s21, [r6, #+4]",  s21, r6,  (long) numbers + 8, 4);
    TESTINSN_vldr_f32("vldr s8,  [r4]",       s8,  r4,  (long) numbers + 8, 0);
    TESTINSN_vldr_f32("vldr s19, [r0, #-8]",  s19, r0,  (long) numbers + 8, -8);
    TESTINSN_vldr_f32("vldr s10, [r3, #+4]",  s10, r3,  (long) numbers + 8, 4);

    printf("---- VLDM (Increment After, writeback) ----\n");
    TESTINSN_VLDMIAWB(r6,  d17, d7);
    TESTINSN_VLDMIAWB(r4,  d11, d23);
    TESTINSN_VLDMIAWB(r9, d29, d12);
    TESTINSN_VLDMIAWB(r9,  d0,  d30);
    TESTINSN_VLDMIAWB(r12, d8,  d24);
    TESTINSN_VLDMIAWB(r3,  d27, d13);
    TESTINSN_VLDMIAWB(r10, d20, d30);
    TESTINSN_VLDMIAWB(r0,  d0,  d1);
    TESTINSN_VLDMIAWB(r8,  d15, d19);
    TESTINSN_VLDMIAWB(r3,  d31, d30);
    TESTINSN_VLDMIAWB(r10, d6,  d23);
    TESTINSN_VLDMIAWB(r8,  d8,  d16);
    TESTINSN_VLDMIAWB(r9,  d13, d11);
    TESTINSN_VLDMIAWB(r1,  d3,  d8);
    TESTINSN_VLDMIAWB(r2,  d4,  d8);
    TESTINSN_VLDMIAWB(r3,  d9,  d27);

    printf("---- VSTR ----\n");
    TESTINSN_vstr64("vstr d9,  [r6, #+4]",  d9,  0xa0, r6,  (long) numbers + 8, 4);
    TESTINSN_vstr64("vstr d16, [r9, #-4]",  d16, 0xb1, r9,  (long) numbers + 8, -4);
    TESTINSN_vstr64("vstr d30, [r12]",      d30, 0xc2, r12, (long) numbers + 8, 0);
    TESTINSN_vstr64("vstr d22, [r9, #+8]", d22, 0xd4, r9, (long) numbers + 8, 8);
    TESTINSN_vstr64("vstr d29, [r2, #-8]",  d29, 0x00, r2,  (long) numbers + 8, -8);
    TESTINSN_vstr64("vstr d8,  [r8, #+8]",  d8,  0x11, r8,  (long) numbers + 8, 8);
    TESTINSN_vstr64("vstr d11, [r12, #-4]", d11, 0x22, r12, (long) numbers + 8, -4);
    TESTINSN_vstr64("vstr d18, [r3]",       d18, 0x33, r3,  (long) numbers + 8, 0);
    TESTINSN_vstr64("vstr d5,  [r10, #+8]", d5,  0x99, r10, (long) numbers + 8, 8);
    TESTINSN_vstr64("vstr d17, [r10]",      d17, 0x77, r10, (long) numbers + 8, 0);
    TESTINSN_vstr64("vstr d9,  [r9, #-4]", d9,  0xee, r9, (long) numbers + 8, -4);
    TESTINSN_vstr64("vstr d29, [r4, #-8]",  d29, 0xff, r4,  (long) numbers + 8, -8);
    TESTINSN_vstr64("vstr d10, [r3, #+4]",  d10, 0xbc, r3,  (long) numbers + 8, 4);
    TESTINSN_vstr64("vstr d21, [r6, #+4]",  d21, 0x48, r6,  (long) numbers + 8, 4);
    TESTINSN_vstr64("vstr d8,  [r4]",       d8,  0x1f, r4,  (long) numbers + 8, 0);
    TESTINSN_vstr64("vstr d19, [r0, #-8]",  d19, 0xf9, r0,  (long) numbers + 8, -8);
    TESTINSN_vstr32("vstr s9,  [r6,  #+4]", s9,  r6,  (long) numbers + 8, 4);
    TESTINSN_vstr32("vstr s21, [r9, #-4]", s21, r9, (long) numbers + 8, -4);
    TESTINSN_vstr32("vstr s4,  [r3,  #+8]", s4,  r3,  (long) numbers + 8, 8);
    TESTINSN_vstr32("vstr s19, [r4,  #-8]", s19, r4,  (long) numbers + 8, -8);
    TESTINSN_vstr32("vstr s29, [r8]",       s29, r8,  (long) numbers + 8, 0);
    TESTINSN_vstr32("vstr s8,  [r12]",      s8,  r12, (long) numbers + 8, 0);
    TESTINSN_vstr32("vstr s16, [r0,  #+4]", s16, r0,  (long) numbers + 8, 4);
    TESTINSN_vstr32("vstr s0,  [r8,  #-4]", s0,  r8,  (long) numbers + 8, -4);
    TESTINSN_vstr32("vstr s3,  [r9,  #+8]", s3,  r9,  (long) numbers + 8, 8);
    TESTINSN_vstr32("vstr s9,  [r10, #-8]", s9,  r10, (long) numbers + 8, -8);
    TESTINSN_vstr32("vstr s11, [r2]",       s11, r2,  (long) numbers + 8, 0);
    TESTINSN_vstr32("vstr s30, [r0]",       s30, r0,  (long) numbers + 8, 0);

    printf("---- VSTM (Increment After, no writeback) ----\n");
    TESTINSN_VSTMIAnoWB("vstmia r6,  {d21}", r6,  d21, 0xab);
    TESTINSN_VSTMIAnoWB("vstmia r1,  {d1}",  r1,  d1, 0x13);
    TESTINSN_VSTMIAnoWB("vstmia r9,  {d2}",  r9,  d2, 0x78);
    TESTINSN_VSTMIAnoWB("vstmia r4,  {d30}", r4,  d30, 0x0);
    TESTINSN_VSTMIAnoWB("vstmia r12, {d23}", r12, d23, 0xb9);
    TESTINSN_VSTMIAnoWB("vstmia r6,  {d16}", r6,  d16, 0xa6);
    TESTINSN_VSTMIAnoWB("vstmia r6,  {d8}",  r6,  d8, 0x7f);
    TESTINSN_VSTMIAnoWB("vstmia r6,  {d27}", r6,  d27, 0xff);
    TESTINSN_VSTMIAnoWB("vstmia r5,  {d11}", r5,  d11, 0x32);
    TESTINSN_VSTMIAnoWB("vstmia r6,  {d4}",  r6,  d4, 0x10);
    TESTINSN_VSTMIAnoWB("vstmia r10, {d9}",  r10, d9, 0x4f);
    TESTINSN_VSTMIAnoWB("vstmia r9, {d29}", r9, d29, 0x97);
    TESTINSN_VSTMIAnoWB("vstmia r10, {d17}", r10, d17, 0xaa);
    TESTINSN_VSTMIAnoWB("vstmia r5,  {d5}",  r5,  d5, 0x2b);
    TESTINSN_VSTMIAnoWB("vstmia r9,  {d7}",  r9,  d7, 0x7b);
    TESTINSN_VSTMIAnoWB("vstmia r3,  {d16}", r3,  d16, 0x11);
    TESTINSN_VSTMIAnoWB32("vstmia r6,  {s21}", r6,  s21, 0xab);
    TESTINSN_VSTMIAnoWB32("vstmia r1,  {s1}",  r1,  s1,  0x13);
    TESTINSN_VSTMIAnoWB32("vstmia r9,  {s2}",  r9,  s2,  0x78);
    TESTINSN_VSTMIAnoWB32("vstmia r4,  {s30}", r4,  s30, 0x0);
    TESTINSN_VSTMIAnoWB32("vstmia r12, {s23}", r12, s23, 0xb9);
    TESTINSN_VSTMIAnoWB32("vstmia r6,  {s16}", r6,  s16, 0xa613451d);
    TESTINSN_VSTMIAnoWB32("vstmia r6,  {s8}",  r6,  s8,  0x7f);
    TESTINSN_VSTMIAnoWB32("vstmia r6,  {s27}", r6,  s27, f2u(-INFINITY));
    TESTINSN_VSTMIAnoWB32("vstmia r5,  {s11}", r5,  s11, f2u(NAN));
    TESTINSN_VSTMIAnoWB32("vstmia r6,  {s4}",  r6,  s4,  0x10ccb);
    TESTINSN_VSTMIAnoWB32("vstmia r10, {s9}",  r10, s9,  0x4f543);
    TESTINSN_VSTMIAnoWB32("vstmia r9, {s29}", r9, s29, 0x97001a);
    TESTINSN_VSTMIAnoWB32("vstmia r10, {s17}", r10, s17, 0xaa45f);
    TESTINSN_VSTMIAnoWB32("vstmia r5,  {s5}",  r5,  s5,  f2u(NAN));
    TESTINSN_VSTMIAnoWB32("vstmia r9,  {s7}",  r9,  s7,  f2u(-INFINITY));
    TESTINSN_VSTMIAnoWB32("vstmia r3,  {s16}", r3,  s16, f2u(INFINITY));

    printf("---- VSTM (Increment After, writeback) ----\n");
    TESTINSN_VSTMIAWB(r6,  d21, d2);
    TESTINSN_VSTMIAWB(r1,  d1, d5);
    TESTINSN_VSTMIAWB(r9,  d2, d17);
    TESTINSN_VSTMIAWB(r4,  d30, d21);
    TESTINSN_VSTMIAWB(r12, d23, d29);
    TESTINSN_VSTMIAWB(r6,  d16, d30);
    TESTINSN_VSTMIAWB(r6,  d8,  d12);
    TESTINSN_VSTMIAWB(r6,  d27, d24);
    TESTINSN_VSTMIAWB(r5,  d11, d13);
    TESTINSN_VSTMIAWB(r6,  d4,  d31);
    TESTINSN_VSTMIAWB(r10, d9,  d27);
    TESTINSN_VSTMIAWB(r9, d29, d17);
    TESTINSN_VSTMIAWB(r10, d17, d7);
    TESTINSN_VSTMIAWB(r5,  d5,  d8);
    TESTINSN_VSTMIAWB(r9,  d7,  d16);
    TESTINSN_VSTMIAWB(r3,  d16, d21);
    TESTINSN_VSTMIAWB32(r6,  s21, s2);
    TESTINSN_VSTMIAWB32(r12, s23, s21);
    TESTINSN_VSTMIAWB32(r3,  s3,  s3);
    TESTINSN_VSTMIAWB32(r10, s19, s5);
    TESTINSN_VSTMIAWB32(r2,  s3,  s12);
    TESTINSN_VSTMIAWB32(r8,  s7,  s10);
    TESTINSN_VSTMIAWB32(r4,  s30, s13);
    TESTINSN_VSTMIAWB32(r6,  s17, s17);
    TESTINSN_VSTMIAWB32(r9, s11, s21);
    TESTINSN_VSTMIAWB32(r9,  s8,  s30);
    TESTINSN_VSTMIAWB32(r3,  s12, s9);
    TESTINSN_VSTMIAWB32(r6,  s6,  s11);
    TESTINSN_VSTMIAWB32(r8,  s17, s12);
    TESTINSN_VSTMIAWB32(r9,  s12, s12);
    TESTINSN_VSTMIAWB32(r4,  s11, s11);

    printf("---- VLDM (Decrement Before) ----\n");
    TESTINSN_VLDMDB(r4,  d11, d23);
    TESTINSN_VLDMDB(r9, d29, d12);
    TESTINSN_VLDMDB(r9,  d0,  d30);
    TESTINSN_VLDMDB(r12, d8,  d24);
    TESTINSN_VLDMDB(r3,  d27, d13);
    TESTINSN_VLDMDB(r6,  d17, d7);
    TESTINSN_VLDMDB(r10, d20, d30);
    TESTINSN_VLDMDB(r0,  d0,  d1);
    TESTINSN_VLDMDB(r8,  d15, d19);
    TESTINSN_VLDMDB(r3,  d31, d30);
    TESTINSN_VLDMDB(r10, d6,  d23);
    TESTINSN_VLDMDB(r8,  d8,  d16);
    TESTINSN_VLDMDB(r9,  d13, d11);
    TESTINSN_VLDMDB(r1,  d3,  d8);
    TESTINSN_VLDMDB(r2,  d4,  d8);
    TESTINSN_VLDMDB(r3,  d9,  d27);

    printf("----- VMOV (immediate) -----\n");
    TESTINSN_vmovf32_imm("vmov s0",  s0,  0xbe880000);
    TESTINSN_vmovf32_imm("vmov s1",  s1,  0x3fa80000);
    TESTINSN_vmovf32_imm("vmov s2",  s2,  0xbf080000);
    TESTINSN_vmovf32_imm("vmov s5",  s5,  0x3eb80000);
    TESTINSN_vmovf32_imm("vmov s7",  s7,  0xbff80000);
    TESTINSN_vmovf32_imm("vmov s10", s10, 0xbe280000);
    TESTINSN_vmovf32_imm("vmov s12", s12, 0x40000000);
    TESTINSN_vmovf32_imm("vmov s13", s13, 0x3e880000);
    TESTINSN_vmovf32_imm("vmov s14", s14, 0xbee80000);
    TESTINSN_vmovf32_imm("vmov s15", s15, 0xc0b80000);

    printf("----- VMOV (ARM core register and single register) -----\n");
    TESTINSN_vmov_core_single("vmov r12, s12", r12, s12, 0x4000aad);
    TESTINSN_vmov_core_single("vmov r2,  s5",  r2,  s5,  0xab45e7);
    TESTINSN_vmov_core_single("vmov r5,  s15", r5,  s15, 0x00add12);
    TESTINSN_vmov_core_single("vmov r8,  s11", r8,  s11, 0x876450ff);
    TESTINSN_vmov_core_single("vmov r9, s5",  r9, s5,  0xffff);
    TESTINSN_vmov_core_single("vmov r10, s9",  r10, s9,  0x87d34f);
    TESTINSN_vmov_core_single("vmov r9,  s10", r9,  s10, 0x00ffff);
    TESTINSN_vmov_core_single("vmov r6,  s8",  r6,  s8,  0xad4f8);
    TESTINSN_vmov_core_single("vmov r4,  s14", r4,  s14, 0x920b8cf);
    TESTINSN_vmov_core_single("vmov r3,  s7",  r3,  s7,  f2u(NAN));
    TESTINSN_vmov_core_single("vmov r2,  s0",  r2,  s0,  f2u(-INFINITY));
    TESTINSN_vmov_core_single("vmov r0,  s1",  r0,  s1,  f2u(INFINITY));
    TESTINSN_vmov_single_core("vmov s2,  r9",  s2,  r9,  0x9465a);
    TESTINSN_vmov_single_core("vmov s14, r0",  s14, r0,  0xd0b87a);
    TESTINSN_vmov_single_core("vmov s4,  r2",  s4,  r2,  0x452bbc8);
    TESTINSN_vmov_single_core("vmov s7,  r8",  s7,  r8,  0xa7cb3d);
    TESTINSN_vmov_single_core("vmov s9,  r4",  s9,  r4,  0xdd8ec);
    TESTINSN_vmov_single_core("vmov s10, r12", s10, r12, 0x8a7b6e);
    TESTINSN_vmov_single_core("vmov s13, r9",  s13, r9,  0x4b00a);
    TESTINSN_vmov_single_core("vmov s3,  r3",  s3,  r3,  0x0023455);
    TESTINSN_vmov_single_core("vmov s5,  r5",  s5,  r5,  f2u(INFINITY));
    TESTINSN_vmov_single_core("vmov s8,  r6",  s8,  r6,  f2u(-INFINITY));
    TESTINSN_vmov_single_core("vmov s4,  r0",  s4,  r0,  0x000acb45);
    TESTINSN_vmov_single_core("vmov s0,  r6",  s0,  r6,  f2u(NAN));

    printf("----- VMOV (ARM two core registers and two single registers) -----\n");
    TESTINSN_vmov_2single_2core("vmov  s0,  s1,  r6,  r9",  s0,  s1,  r6,  r9, 0x43252acc, 0xabcc4);
    TESTINSN_vmov_2single_2core("vmov  s0,  s1,  r9,  r9",  s0,  s1,  r9,  r9, 0x43252acc, 0xabcc4);
    TESTINSN_vmov_2single_2core("vmov s30, s31,  r9,  r1", s30, s31,  r9,  r1, 0xaa2e2acc, 0x00337);
    TESTINSN_vmov_2single_2core("vmov s20, s21, r10, r9", s20, s21, r10, r9, f2u(NAN), f2u(NAN));
    TESTINSN_vmov_2single_2core("vmov s20, s21, r10, r9", s20, s21, r10, r9, f2u(NAN), f2u(INFINITY));
    TESTINSN_vmov_2single_2core("vmov s20, s21, r10, r9", s20, s21, r10, r9, f2u(NAN), f2u(-INFINITY));
    TESTINSN_vmov_2single_2core("vmov s20, s21, r10, r9", s20, s21, r10, r9, f2u(NAN), f2u(0));
    TESTINSN_vmov_2single_2core("vmov s20, s21, r10, r9", s20, s21, r10, r9, f2u(INFINITY), f2u(NAN));
    TESTINSN_vmov_2single_2core("vmov s20, s21, r10, r9", s20, s21, r10, r9, f2u(INFINITY), f2u(INFINITY));
    TESTINSN_vmov_2single_2core("vmov s20, s21, r10, r9", s20, s21, r10, r9, f2u(INFINITY), f2u(-INFINITY));
    TESTINSN_vmov_2single_2core("vmov s20, s21, r10, r9", s20, s21, r10, r9, f2u(INFINITY), f2u(0));
    TESTINSN_vmov_2single_2core("vmov s20, s21, r10, r9", s20, s21, r10, r9, f2u(-INFINITY), f2u(NAN));
    TESTINSN_vmov_2single_2core("vmov s20, s21, r10, r9", s20, s21, r10, r9, f2u(-INFINITY), f2u(INFINITY));
    TESTINSN_vmov_2single_2core("vmov s20, s21, r10, r9", s20, s21, r10, r9, f2u(-INFINITY), f2u(-INFINITY));
    TESTINSN_vmov_2single_2core("vmov s20, s21, r10, r9", s20, s21, r10, r9, f2u(-INFINITY), f2u(0));
    TESTINSN_vmov_2single_2core("vmov s20, s21, r10, r9", s20, s21, r10, r9, f2u(0), f2u(NAN));
    TESTINSN_vmov_2single_2core("vmov s20, s21, r10, r9", s20, s21, r10, r9, f2u(0), f2u(INFINITY));
    TESTINSN_vmov_2single_2core("vmov s20, s21, r10, r9", s20, s21, r10, r9, f2u(0), f2u(-INFINITY));
    TESTINSN_vmov_2single_2core("vmov s20, s21, r10, r9", s20, s21, r10, r9, f2u(0), f2u(0));
    TESTINSN_vmov_2single_2core("vmov s20, s21, r10, r9", s20, s21, r10, r9, f2u(NAN) + 1, f2u(NAN));
    TESTINSN_vmov_2single_2core("vmov s20, s21, r10, r9", s20, s21, r10, r9, f2u(NAN) + 1, f2u(0));
    TESTINSN_vmov_2single_2core("vmov s20, s21, r10, r9", s20, s21, r10, r9, f2u(NAN), f2u(NAN) - 1);
    TESTINSN_vmov_2single_2core("vmov s20, s21, r10, r9", s20, s21, r10, r9, f2u(0), f2u(NAN) - 1);
    TESTINSN_vmov_2core_2single("vmov r12,  r9, s12, s13", r12,  r9, s12, s13, 0x4000aad, 0xaffff);
    TESTINSN_vmov_2core_2single("vmov  r0,  r9, s12, s13",  r0,  r9, s12, s13, 0x40ee56d, 0x123ff);
    TESTINSN_vmov_2core_2single("vmov r12, r9, s12, s13", r12, r9, s12, s13, 0x4000aad, 0xaffff);
    TESTINSN_vmov_2core_2single("vmov r10, r9, s20, s21", r10, r9, s20, s21, f2u(NAN), f2u(NAN));
    TESTINSN_vmov_2core_2single("vmov r10, r9, s20, s21", r10, r9, s20, s21, f2u(NAN), f2u(INFINITY));
    TESTINSN_vmov_2core_2single("vmov r10, r9, s20, s21", r10, r9, s20, s21, f2u(NAN), f2u(-INFINITY));
    TESTINSN_vmov_2core_2single("vmov r10, r9, s20, s21", r10, r9, s20, s21, f2u(NAN), f2u(0));
    TESTINSN_vmov_2core_2single("vmov r10, r9, s20, s21", r10, r9, s20, s21, f2u(INFINITY), f2u(NAN));
    TESTINSN_vmov_2core_2single("vmov r10, r9, s20, s21", r10, r9, s20, s21, f2u(INFINITY), f2u(INFINITY));
    TESTINSN_vmov_2core_2single("vmov r10, r9, s20, s21", r10, r9, s20, s21, f2u(INFINITY), f2u(-INFINITY));
    TESTINSN_vmov_2core_2single("vmov r10, r9, s20, s21", r10, r9, s20, s21, f2u(INFINITY), f2u(0));
    TESTINSN_vmov_2core_2single("vmov r10, r9, s20, s21", r10, r9, s20, s21, f2u(-INFINITY), f2u(NAN));
    TESTINSN_vmov_2core_2single("vmov r10, r9, s20, s21", r10, r9, s20, s21, f2u(-INFINITY), f2u(INFINITY));
    TESTINSN_vmov_2core_2single("vmov r10, r9, s20, s21", r10, r9, s20, s21, f2u(-INFINITY), f2u(-INFINITY));
    TESTINSN_vmov_2core_2single("vmov r10, r9, s20, s21", r10, r9, s20, s21, f2u(-INFINITY), f2u(0));
    TESTINSN_vmov_2core_2single("vmov r10, r9, s20, s21", r10, r9, s20, s21, f2u(0), f2u(NAN));
    TESTINSN_vmov_2core_2single("vmov r10, r9, s20, s21", r10, r9, s20, s21, f2u(0), f2u(INFINITY));
    TESTINSN_vmov_2core_2single("vmov r10, r9, s20, s21", r10, r9, s20, s21, f2u(0), f2u(-INFINITY));
    TESTINSN_vmov_2core_2single("vmov r10, r9, s20, s21", r10, r9, s20, s21, f2u(0), f2u(0));
    TESTINSN_vmov_2core_2single("vmov r10, r9, s20, s21", r10, r9, s20, s21, f2u(NAN) + 1, f2u(NAN));
    TESTINSN_vmov_2core_2single("vmov r10, r9, s20, s21", r10, r9, s20, s21, f2u(NAN) + 1, f2u(0));
    TESTINSN_vmov_2core_2single("vmov r10, r9, s20, s21", r10, r9, s20, s21, f2u(NAN), f2u(NAN) - 1);
    TESTINSN_vmov_2core_2single("vmov r10, r9, s20, s21", r10, r9, s20, s21, f2u(0), f2u(NAN) - 1);

    printf("----- VMOV (ARM two core registers and double register) -----\n");
    TESTINSN_vmov_double_2core("vmov d3,  r6,  r9",  d3,  r6,  r9,  0x43252acc, 0x45bbd);
    TESTINSN_vmov_double_2core("vmov d4,  r10, r2",  d4,  r10, r2,  0x1243b4, 0x237ffb);
    TESTINSN_vmov_double_2core("vmov d21, r1,  r6",  d21, r1,  r6,  0x30cc4, 0x314c043);
    TESTINSN_vmov_double_2core("vmov d30, r9, r9", d30, r9, r9, 0x08ddf, 0x87bbca);
    TESTINSN_vmov_double_2core("vmov d29, r3,  r5",  d29, r3,  r5,  0xaaa0, 0xbbb1);
    TESTINSN_vmov_double_2core("vmov d16, r8,  r8",  d16, r8,  r8,  0xaa455bb, 0x13434);
    TESTINSN_vmov_double_2core("vmov d17, r12, r9", d17, r12, r9, 0x004003, 0x452bbc1);
    TESTINSN_vmov_double_2core("vmov d9,  r9,  r0",  d9,  r9,  r0,  0x134c, 0x41ffb6);
    TESTINSN_vmov_double_2core("vmov d7,  r0,  r6",  d7,  r0,  r6,  0x35425dcc, 0x0876c43);
    TESTINSN_vmov_double_2core("vmov d13, r3,  r9", d13, r3,  r9, f2u0(NAN), f2u1(NAN));
    TESTINSN_vmov_double_2core("vmov d19, r6,  r5",  d19, r6,  r5,  f2u0(INFINITY), f2u1(INFINITY));
    TESTINSN_vmov_double_2core("vmov d0,  r2,  r6",  d0,  r2,  r6,  f2u0(-INFINITY), f2u1(-INFINITY));

    TESTINSN_vmov_2core_double("vmov r3,  r6,  d9",  r3,  r6,  d9,  0x43252acc, 0x45bbd);
    TESTINSN_vmov_2core_double("vmov r4,  r10, d2",  r4,  r10, d2,  0x1243b4, 0x237ffb);
    TESTINSN_vmov_2core_double("vmov r2,  r1,  d6",  r2,  r1,  d6,  0x30cc4, 0x314c043);
    TESTINSN_vmov_2core_double("vmov r0,  r9, d11", r0,  r9, d11, 0x08ddf, 0x87bbca);
    TESTINSN_vmov_2core_double("vmov r9,  r3,  d5",  r9,  r3,  d5,  0xaaa0, 0xbbb1);
    TESTINSN_vmov_2core_double("vmov r10, r8,  d8",  r10, r8,  d8,  0xaa455bb, 0x13434);
    TESTINSN_vmov_2core_double("vmov r9, r12, d11", r9, r12, d11, 0x004003, 0x452bbc1);

    // ARM ARM says this is UNDEFINED, hence we don't decode it
    //TESTINSN_vmov_2core_double("vmov r9,  r9,  d0",  r9,  r9,  d0,  0x134c, 0x41ffb6);

    TESTINSN_vmov_2core_double("vmov r6,  r0,  d7",  r6,  r0,  d7,  0x35425dcc, 0x0876c43);
    TESTINSN_vmov_2core_double("vmov r12, r3,  d11", r12, r3,  d11, f2u0(NAN), f2u1(NAN));
    TESTINSN_vmov_2core_double("vmov r1,  r6,  d5",  r1,  r6,  d5,  f2u0(INFINITY), f2u1(INFINITY));
    TESTINSN_vmov_2core_double("vmov r0,  r2,  d7",  r0,  r2,  d7,  f2u0(-INFINITY), f2u1(-INFINITY));

    TESTINSN_vmov_2core_double("vmov r2,  r9,  d0",  r2,  r9,  d0,  f2u0(INFINITY), f2u1(INFINITY));
    TESTINSN_vmov_2core_double("vmov r6,  r9, d10", r6,  r9, d10, 0x14534c, 0x41ffb6);
    TESTINSN_vmov_2core_double("vmov r0,  r9, d20", r0,  r9, d20, f2u0(NAN), f2u1(NAN));

    printf("----- VPUSH, VPOP -----\n");
    TESTINSN_vpush_vpop_f32(s3, 0x00aaaaaa, s4, 0x00bbbbbb, s5, 0x00cccccc, s0, s1,  s2);
    TESTINSN_vpush_vpop_f32(s1, 0x000134f4, s2, 0x0870ccb3, s3, 0x00aba0f1, s9, s10, s11);
    TESTINSN_vpush_vpop_f32(s3, 0x00dddddd, s4, 0x00eeeeee, s5, 0x00ffffff, s0, s1,  s2);
    TESTINSN_vpush_vpop_f32(s11, 0x13454c,  s12, 0x341,     s13, 0xaac45f, s6, s7,  s8);
    TESTINSN_vpush_vpop_f32(s21, 0x0,       s22, f2u(NAN),  s23, f2u(INFINITY), s23, s24,  s25);
    TESTINSN_vpush_vpop_f32(s12, 0xffffff,  s13, 0xf542dd4, s14, f2u(-INFINITY), s11, s12,  s13);
    TESTINSN_vpush_vpop_f32(s25, 0x45cd,    s26, 0xa3ccb5,  s27, 0xbbcaf, s0, s1,  s2);
    TESTINSN_vpush_vpop_f32(s1,  f2u(NAN),  s2,  0xaaca3,   s3,  0x876008, s6, s7,  s8);
    TESTINSN_vpush_vpop_f32(s9,  0x2f43,    s10, f2u(INFINITY), s11, 0x3cc66a, s9, s10,  s11);
    TESTINSN_vpush_vpop_f32(s10, f2u(INFINITY), s11, 0x134cc5, s12, f2u(NAN), s2, s3,  s4);
    TESTINSN_vpush_vpop_f32(s7,  0xcc006d,  s8,  0x1308c,   s9,  0xabbc45, s21, s22,  s23);
    TESTINSN_vpush_vpop_f32(s19, f2u(-INFINITY), s20, 0x452146, s21, 0x388bbc, s4, s5,  s6);
    TESTINSN_vpush_vpop_f32(s16, 0x542aa,   s17, 0xaddcd5,  s18, 0x87acc, s18, s19,  s20);
    TESTINSN_vpush_vpop_f32(s22, 0x5ccb708, s23, 0x52345c,  s24, 0x98745c, s12, s13,  s14);
    TESTINSN_vpush_vpop_f32(s24, 0x99234f,  s25, 0x1aac,    s26, 0x3746c, s28, s29,  s30);
    TESTINSN_vpush_vpop_f32(s13, 0x134ccc,  s14, 0x6bb43,   s15, 0x834aa, s0,  s1,   s2);
    TESTINSN_vpush_vpop_f64(d3,  0x00aaaaaa, 0xaac3,     d4,  0x00bbbbbb, 0x34ccb,     d0, d1);
    TESTINSN_vpush_vpop_f64(d1,  0x000134f4, 0x341531,   d2,  0x0870ccb3, 0x4576bbc,   d9, d10);
    TESTINSN_vpush_vpop_f64(d3,  0x00dddddd, 0x13451cc,  d4,  0x00eeeeee, 0x123ddc8,   d0, d1);
    TESTINSN_vpush_vpop_f64(d11, 0x13454c,   0x541bbc3,  d12, 0x341,      0xccb5, d6,  d7);
    TESTINSN_vpush_vpop_f64(d21, 0x0,        0x123c33,   d22, f2u0(NAN),  f2u1(NAN),   d23, d24);
    TESTINSN_vpush_vpop_f64(d12, 0xffffff,   0x1940c,    d13, 0xf542dd4,  0x788ffc,    d11, d12);
    TESTINSN_vpush_vpop_f64(d25, 0x45cd,     0x1309c,    d26, 0xa3ccb5,   0x4588b,     d0, d1);
    TESTINSN_vpush_vpop_f64(d1,  f2u0(NAN),  f2u1(NAN),  d2,  0xaaca3,    0x1120a,     d6, d7);
    TESTINSN_vpush_vpop_f64(d9,  0x2f43,     0x19ff9,    d10, f2u0(INFINITY), f2u1(INFINITY), d9, d10);
    TESTINSN_vpush_vpop_f64(d10, f2u0(INFINITY), f2u1(INFINITY), d11, 0x134cc5, 0x78cbbd, d2, d3);
    TESTINSN_vpush_vpop_f64(d7,  0xcc006d,   0x28354,    d8,  0x1308c,    0x1993bc,    d21, d22);
    TESTINSN_vpush_vpop_f64(d19, f2u0(-INFINITY), f2u1(-INFINITY), d20, 0x452146, 0x138476c,  d4, d5);
    TESTINSN_vpush_vpop_f64(d16, 0x542aa,    0x12dd4,    d17, 0xaddcd5,   0x399cb,     d18, d19);
    TESTINSN_vpush_vpop_f64(d22, 0x5ccb708,  0x8009c,    d23, 0x52345c,   0x29902c,    d12, d13);
    TESTINSN_vpush_vpop_f64(d24, 0x99234f,   0x3457ff,   d25, 0x1aac,     0x1002cba,   d28, d29);
    TESTINSN_vpush_vpop_f64(d13, 0x134ccc,   0xfaa309,   d14, 0x6bb43,    0x199cb,     d0,  d1);

    return 0;
}
