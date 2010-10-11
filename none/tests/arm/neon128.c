
/* How to compile:

   gcc -O -g -Wall -mcpu=cortex-a8 -mfpu=neon -mfloat-abi=softfp \
       -marm -o neon128-a neon128.c

   or

   gcc -O -g -Wall -mcpu=cortex-a8 -mfpu=neon -mfloat-abi=softfp \
       -mthumb -o neon128-t neon128.c

*/

#include <stdio.h>
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

/* test macros to generate and output the result of a single instruction */

const unsigned int mem[] = {
   0x121f1e1f, 0x131b1a1b, 0x141c1f1c, 0x151d191d,
   0x232f2e2f, 0x242c2b2b, 0x252a2e2b, 0x262d2d2a,
   0x3f343f3e, 0x3e353d3c, 0x363a3c3b, 0x3b373b3a,
   0x454f4e45, 0x4e464d46, 0x474d474c, 0x4a484a4c
};

#define TESTINSN_imm(instruction, QD, imm) \
{ \
  unsigned int out[4]; \
\
  __asm__ volatile( \
      "vmov.i8 " #QD ", #0x55" "\n\t" \
      instruction ", #" #imm "\n\t" \
      "vstmia %0, {" #QD "}\n\t" \
      : \
      : "r" (out) \
      : #QD, "memory" \
      ); \
  printf("%s, #" #imm " :: Qd 0x%08x 0x%08x 0x%08x 0x%08x\n", \
      instruction, out[3], out[2], out[1], out[0]); \
}\
{ \
   unsigned int out[4];   \
   unsigned int addr = 0; \
   \
   __asm__ volatile( \
	 "mov %1, %2\n\t" \
	 "vldmia %1!, {" #QD "}\n\t" \
	 instruction ", #" #imm "\n\t" \
	 "vstmia %0, {" #QD "}\n\t" \
	 : \
	 : "r" (out), "r" (addr), "r" (mem) \
	 : #QD, "%2", "memory" \
	 ); \
   printf("%s, #" #imm " :: Qd 0x%08x 0x%08x 0x%08x 0x%08x\n", \
	 instruction, out[3], out[2], out[1], out[0]); \
}

#define TESTINSN_un(instruction, QD, QM, QMtype, QMval) \
{ \
  unsigned int out[4]; \
\
  __asm__ volatile( \
      "vmov.i8 " #QD ", #0x55" "\n\t" \
      "vdup." #QMtype " " #QM ", %1\n\t" \
      instruction "\n\t" \
      "vstmia %0, {" #QD "}\n\t" \
      : \
      : "r" (out), "r" (QMval) \
      : #QD, #QM, "memory" \
      ); \
  printf("%s :: Qd 0x%08x 0x%08x 0x%08x 0x%08x  Qm (" #QMtype ")0x%08x\n", \
      instruction, out[3], out[2], out[1], out[0], QMval); \
} \
{ \
   unsigned int out[4]; \
   unsigned int addr = 0; \
   \
   __asm__ volatile( \
	 "mov %2, %3\n\t" \
	 "vldmia %2!, {" #QD "}\n\t" \
	 "vldmia %2!, {" #QM "}\n\t" \
	 instruction "\n\t" \
	 "vstmia %0, {" #QD "}\n\t" \
	 : \
	 : "r" (out), "r" (QMval), "r" (addr), "r" (mem) \
	 : #QD, #QM, "%2", "memory" \
	 ); \
   printf("%s :: Qd 0x%08x 0x%08x 0x%08x 0x%08x  Qm (" #QMtype ")0x%08x\n", \
	 instruction, out[3], out[2], out[1], out[0], QMval); \
}


#define TESTINSN_un_q(instruction, QD, QM, QMtype, QMval) \
{ \
  unsigned int out[4]; \
  unsigned int fpscr; \
\
  __asm__ volatile( \
      "vmov.i8 " #QD ", #0x55" "\n\t" \
      "mov r4, #0\n\t" \
      MOVE_to_FPSCR_from_R4 \
      "vdup." #QMtype " " #QM ", %2\n\t" \
      instruction "\n\t" \
      "vstmia %1, {" #QD "}\n\t" \
      MOVE_to_R4_from_FPSCR \
      "mov %0, r4" \
      : "=r" (fpscr) \
      : "r" (out), "r" (QMval) \
      : #QD, #QM, "memory", "r4" \
      ); \
  printf("%s :: Qd 0x%08x 0x%08x 0x%08x 0x%08x  Qm (" #QMtype ")0x%08x" \
          "  fpscr: %08x\n", \
      instruction, out[3], out[2], out[1], out[0], QMval, fpscr); \
} \
{ \
   unsigned int out[4]; \
   unsigned int fpscr; \
   unsigned int addr = 0; \
   \
   __asm__ volatile( \
	 "vmov.i8 " #QD ", #0x55" "\n\t" \
	 "mov r4, #0\n\t" \
	 MOVE_to_FPSCR_from_R4 \
	 "mov %3, %4\n\t" \
	 "vldmia %3!, {" #QM "}\n\t" \
	 instruction "\n\t" \
	 "vstmia %1, {" #QD "}\n\t" \
	 MOVE_to_R4_from_FPSCR \
	 "mov %0, r4" \
	 : "=r" (fpscr) \
	 : "r" (out), "r" (QMval), "r" (addr), "r" (mem) \
	 : #QD, #QM, "memory", "r4" \
	 ); \
   printf("%s :: Qd 0x%08x 0x%08x 0x%08x 0x%08x  Qm (" #QMtype ")0x%08x" \
	 "  fpscr: %08x\n", \
	 instruction, out[3], out[2], out[1], out[0], QMval, fpscr); \
}

#define TESTINSN_bin(instruction, QD, QM, QMtype, QMval, QN, QNtype, QNval) \
{ \
  unsigned int out[4]; \
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
  printf("%s :: Qd 0x%08x 0x%08x 0x%08x 0x%08x  Qm (" #QMtype ")0x%08x" \
      "  Qn (" #QNtype ")0x%08x\n", \
      instruction, out[3], out[2], out[1], out[0], QMval, QNval); \
} \
/*{ \
      unsigned int out[4]; \
      unsigned int addr = 0; \
      \
      __asm__ volatile( \
	             "mov %0, %4\n\t" \
	             "vldmia %0!, {" #QM "}\n\t" \
	             "vmov.i8 " #QD ", #0x55" "\n\t" \
	             "vdup." #QNtype " " #QN ", %3\n\t" \
	             instruction "\n\t" \
	             "vstmia %1, {" #QD "}\n\t" \
	             : "+r" (addr) \
	             : "r" (out), "r" (QMval), "r" (QNval), "r" (mem) \
	             : #QD, #QM, #QN, "memory" \
	             ); \
      printf("%s :: Qd 0x%08x 0x%08x 0x%08x 0x%08x  Qm (" #QMtype ")0x%08x" \
	          "  Qn (" #QNtype ")0x%08x\n", \
	          instruction, out[3], out[2], out[1], out[0], QMval, QNval); \
} */

#define TESTINSN_bin_f(instruction, QD, QM, QMtype, QMval, QN, QNtype, QNval) \
{ \
  unsigned int out[4]; \
\
  __asm__ volatile( \
      "vdup.i32 " #QD ", %3\n\t" \
      "vdup." #QMtype " " #QM ", %1\n\t" \
      "vdup." #QNtype " " #QN ", %2\n\t" \
      instruction "\n\t" \
      "vstmia %0, {" #QD "}\n\t" \
      : \
      : "r" (out), "r" (QMval), "r" (QNval), "r" (0x3f800000) \
      : #QD, #QM, #QN, "memory" \
      ); \
  printf("%s :: Qd 0x%08x 0x%08x 0x%08x 0x%08x  Qm (" #QMtype ")0x%08x" \
      "  Qn (" #QNtype ")0x%08x\n", \
      instruction, out[3], out[2], out[1], out[0], QMval, QNval); \
} \
{ \
   unsigned int out[4]; \
   unsigned int addr = 0; \
   \
   __asm__ volatile( \
	 "vdup.i32 " #QD ", %3\n\t" \
	 "mov %4, %5\n\t" \
	 "vldmia %4!, {" #QM "}\n\t" \
	 "vdup." #QNtype " " #QN ", %2\n\t" \
	 instruction "\n\t" \
	 "vstmia %0, {" #QD "}\n\t" \
	 : \
	 : "r" (out), "r" (QMval), "r" (QNval), "r"(0x3f800000), "r" (addr), "r" (mem) \
	 : #QD, #QM, #QN, "memory" \
	 ); \
   printf("%s :: Qd 0x%08x 0x%08x 0x%08x 0x%08x  Qm (" #QMtype ")0x%08x" \
	 "  Qn (" #QNtype ")0x%08x\n", \
	 instruction, out[3], out[2], out[1], out[0], QMval, QNval); \
}

#define TESTINSN_bin_q(instruction, QD, QM, QMtype, QMval, QN, QNtype, QNval) \
{ \
  unsigned int out[4]; \
  unsigned int fpscr; \
\
  __asm__ volatile( \
      "vmov.i8 " #QD ", #0x55" "\n\t" \
      "mov r4, #0\n\t" \
      MOVE_to_FPSCR_from_R4 \
      "vdup." #QMtype " " #QM ", %2\n\t" \
      "vdup." #QNtype " " #QN ", %3\n\t" \
      instruction "\n\t" \
      "vstmia %1, {" #QD "}\n\t" \
      MOVE_to_R4_from_FPSCR \
      "mov %0, r4" \
      : "=r" (fpscr) \
      : "r" (out), "r" (QMval), "r" (QNval) \
      : #QD, #QM, #QN, "memory", "r4" \
      ); \
  printf("%s :: Qd 0x%08x 0x%08x 0x%08x 0x%08x  Qm (" #QMtype ")0x%08x" \
      "  Qn (" #QNtype ")0x%08x  fpscr: %08x\n", \
      instruction, out[3], out[2], out[1], out[0], QMval, QNval, fpscr); \
} \
{ \
      unsigned int out[4]; \
      unsigned int fpscr; \
      unsigned int addr = 0; \
      \
      __asm__ volatile( \
	             "vmov.i8 " #QD ", #0x55" "\n\t" \
	             "mov r4, #0\n\t" \
	             MOVE_to_FPSCR_from_R4 \
	             "mov %4, %5\n\t" \
	             "vldmia %4!, {" #QM "}\n\t" \
	             "vdup." #QNtype " " #QN ", %3\n\t" \
	             instruction "\n\t" \
	             "vstmia %1, {" #QD "}\n\t" \
	             MOVE_to_R4_from_FPSCR \
	             "mov %0, r4" \
	             : "=r" (fpscr) \
	             : "r" (out), "r" (QMval), "r" (QNval), "r" (addr), "r" (mem)  \
	             : #QD, #QM, #QN, "memory", "r4" \
	             ); \
      printf("%s :: Qd 0x%08x 0x%08x  Qm (" #QMtype ")0x%08x" \
	             "  Qn (" #QNtype ")0x%08x  fpscr: %08x\n", \
	             instruction, out[1], out[0], QMval, QNval, fpscr); \
      printf("%s :: Qd 0x%08x 0x%08x 0x%08x 0x%08x  Qm (" #QMtype ")0x%08x" \
	          "  Qn (" #QNtype ")0x%08x  fpscr: %08x\n", \
	          instruction, out[3], out[2], out[1], out[0], QMval, QNval, fpscr); \
}

#define TESTINSN_dual(instruction, QM, QMtype, QMval, QN, QNtype, QNval) \
{ \
  unsigned int out1[4]; \
  unsigned int out2[4]; \
\
  __asm__ volatile( \
      "vdup." #QMtype " " #QM ", %2\n\t" \
      "vdup." #QNtype " " #QN ", %3\n\t" \
      instruction "\n\t" \
      "vstmia %0, {" #QM "}\n\t" \
      "vstmia %1, {" #QN "}\n\t" \
      : \
      : "r" (out1), "r" (out2), "r" (QMval), "r" (QNval) \
      : #QM, #QN, "memory" \
      ); \
  printf("%s :: Qm 0x%08x 0x%08x 0x%08x 0x%08x  Qn 0x%08x 0x%08x 0x%08x 0x%08x" \
      "  Qm (" #QMtype ")0x%08x  Qn (" #QNtype ")0x%08x\n", \
      instruction, out1[3], out1[2], out1[1], out1[0], \
      out2[3], out2[2], out2[1], out2[0], QMval, QNval); \
} \
{ \
   unsigned int out1[4]; \
   unsigned int out2[4]; \
   unsigned int addr = 0;    \
   \
   __asm__ volatile( \
	 "mov %4, %5\n\t" \
	 "vldmia %4!, {" #QM "}\n\t" \
	 "vdup." #QNtype " " #QN ", %3\n\t" \
	 instruction "\n\t" \
	 "vstmia %0, {" #QM "}\n\t" \
	 "vstmia %1, {" #QN "}\n\t" \
	 : \
	 : "r" (out1), "r" (out2), "r" (QMval), "r" (QNval), "r" (addr), "r" (mem) \
	 : #QM, #QN, "%4", "memory" \
	 ); \
   printf("%s :: Qm 0x%08x 0x%08x 0x%08x 0x%08x  Qn 0x%08x 0x%08x 0x%08x 0x%08x\nQm (" \
#QMtype ")0x%08x" "  Qn (" #QNtype ")0x%08x\n", \
	 instruction, out1[3], out1[2], out1[1], out1[0],\
	 out2[3], out2[2], out2[1], out2[0], QMval, QNval); \
}

#if 0
#define TESTINSN_2reg_shift(instruction, QD, QM, QMtype, QMval, imm) \
{ \
  unsigned int out[4]; \
\
  __asm__ volatile( \
      "vmov.i8 " #QD ", #0x55" "\n\t" \
      "vdup." #QMtype " " #QM ", %1\n\t" \
      instruction ", #" #imm "\n\t" \
      "vstmia %0, {" #QD "}\n\t" \
      : \
      : "r" (out), "r" (QMval) \
      : #QD, #QM, "memory" \
      ); \
  printf("%s, #" #imm " :: Qd 0x%08x 0x%08x 0x%08x 0x%08x  Qm (" #QMtype ")0x%08x", \
      instruction, out[3], out[2], out[1], out[0], QMval); \
}
#endif

int main(int argc, char **argv)
{
    printf("----- VMOV (immediate) -----\n");
    TESTINSN_imm("vmov.i32 q0", q0, 0x7);
    TESTINSN_imm("vmov.i16 q1", q1, 0x7);
    TESTINSN_imm("vmov.i8 q2", q2, 0x7);
    TESTINSN_imm("vmov.i32 q5", q5, 0x700);
    TESTINSN_imm("vmov.i16 q7", q7, 0x700);
    TESTINSN_imm("vmov.i32 q10", q10, 0x70000);
    TESTINSN_imm("vmov.i32 q12", q12, 0x7000000);
    TESTINSN_imm("vmov.i32 q13", q13, 0x7FF);
    TESTINSN_imm("vmov.i32 q14", q14, 0x7FFFF);
    TESTINSN_imm("vmov.i64 q15", q15, 0xFF0000FF00FFFF00);

    printf("----- VMVN (immediate) -----\n");
    TESTINSN_imm("vmvn.i32 q0", q0, 0x7);
    TESTINSN_imm("vmvn.i16 q1", q1, 0x7);
    TESTINSN_imm("vmvn.i8 q2", q2, 0x7);
    TESTINSN_imm("vmvn.i32 q5", q5, 0x700);
    TESTINSN_imm("vmvn.i16 q7", q7, 0x700);
    TESTINSN_imm("vmvn.i32 q10", q10, 0x70000);
    TESTINSN_imm("vmvn.i32 q13", q13, 0x7000000);
    TESTINSN_imm("vmvn.i32 q11", q11, 0x7FF);
    TESTINSN_imm("vmvn.i32 q14", q14, 0x7FFFF);
    TESTINSN_imm("vmvn.i64 q15", q15, 0xFF0000FF00FFFF00);

    printf("----- VORR (immediate) -----\n");
    TESTINSN_imm("vorr.i32 q0", q0, 0x7);
    TESTINSN_imm("vorr.i16 q2", q2, 0x7);
    TESTINSN_imm("vorr.i32 q8", q8, 0x700);
    TESTINSN_imm("vorr.i16 q6", q6, 0x700);
    TESTINSN_imm("vorr.i32 q14", q14, 0x70000);
    TESTINSN_imm("vorr.i32 q15", q15, 0x7000000);

    printf("----- VBIC (immediate) -----\n");
    TESTINSN_imm("vbic.i32 q0", q0, 0x7);
    TESTINSN_imm("vbic.i16 q3", q3, 0x7);
    TESTINSN_imm("vbic.i32 q5", q5, 0x700);
    TESTINSN_imm("vbic.i16 q8", q8, 0x700);
    TESTINSN_imm("vbic.i32 q10", q10, 0x70000);
    TESTINSN_imm("vbic.i32 q15", q15, 0x7000000);

    printf("---- VMVN (register) ----\n");
    TESTINSN_un("vmvn q0, q1", q0, q1, i32, 24);
    TESTINSN_un("vmvn q10, q15", q10, q15, i32, 24);
    TESTINSN_un("vmvn q0, q14", q0, q14, i32, 24);

    printf("---- VMOV (register) ----\n");
    TESTINSN_un("vmov q0, q1", q0, q1, i32, 24);
    TESTINSN_un("vmov q10, q15", q10, q15, i32, 24);
    TESTINSN_un("vmov q0, q14", q0, q14, i32, 24);

    printf("---- VDUP (ARM core register) (tested indirectly) ----\n");
    TESTINSN_un("vmov q0, q1", q0, q1, i8, 7);
    TESTINSN_un("vmov q10, q11", q10, q11, i16, 7);
    TESTINSN_un("vmov q0, q15", q0, q15, i32, 7);

    printf("---- VADD ----\n");
    TESTINSN_bin("vadd.i32 q0, q1, q2", q0, q1, i32, 24, q2, i32, 120);
    TESTINSN_bin("vadd.i64 q0, q1, q2", q0, q1, i32, 140, q2, i32, 120);
    TESTINSN_bin("vadd.i32 q0, q1, q2", q0, q1, i32, 140, q2, i32, 120);
    TESTINSN_bin("vadd.i16 q0, q1, q2", q0, q1, i32, 140, q2, i32, 120);
    TESTINSN_bin("vadd.i8 q0, q1, q2", q0, q1, i32, 140, q2, i32, 120);
    TESTINSN_bin("vadd.i8 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vadd.i16 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vadd.i32 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vadd.i64 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vadd.i32 q10, q11, q12", q10, q11, i32, 24, q12, i32, 120);
    TESTINSN_bin("vadd.i64 q13, q14, q15", q13, q14, i32, 140, q15, i32, 120);

    printf("---- VSUB ----\n");
    TESTINSN_bin("vsub.i32 q0, q1, q2", q0, q1, i32, 24, q2, i32, 120);
    TESTINSN_bin("vsub.i64 q0, q1, q2", q0, q1, i32, 140, q2, i32, 120);
    TESTINSN_bin("vsub.i32 q0, q1, q2", q0, q1, i32, 140, q2, i32, 120);
    TESTINSN_bin("vsub.i16 q0, q1, q2", q0, q1, i32, 140, q2, i32, 120);
    TESTINSN_bin("vsub.i8 q0, q1, q2", q0, q1, i32, 140, q2, i32, 120);
    TESTINSN_bin("vsub.i8 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vsub.i16 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vsub.i32 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vsub.i64 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vsub.i32 q10, q11, q12", q10, q11, i32, 24, q12, i32, 120);
    TESTINSN_bin("vsub.i64 q13, q14, q15", q13, q14, i32, 140, q15, i32, 120);

    printf("---- VAND ----\n");
    TESTINSN_bin("vand q0, q1, q2", q0, q1, i8, 0x24, q2, i16, 0x77);
    TESTINSN_bin("vand q4, q6, q5", q4, q6, i8, 0xff, q5, i16, 0x57);
    TESTINSN_bin("vand q10, q11, q12", q10, q11, i8, 0xfe, q12, i8, 0xed);
    TESTINSN_bin("vand q15, q15, q15", q15, q15, i8, 0xff, q15, i8, 0xff);

    printf("---- VBIC ----\n");
    TESTINSN_bin("vbic q0, q1, q2", q0, q1, i8, 0x24, q2, i16, 0x77);
    TESTINSN_bin("vbic q4, q6, q5", q4, q6, i8, 0xff, q5, i16, 0x57);
    TESTINSN_bin("vbic q10, q11, q12", q10, q11, i8, 0xfe, q12, i8, 0xed);
    TESTINSN_bin("vbic q15, q15, q15", q15, q15, i8, 0xff, q15, i8, 0xff);

    printf("---- VORR ----\n");
    TESTINSN_bin("vorr q0, q1, q2", q0, q1, i8, 0x24, q2, i16, 0x73);
    TESTINSN_bin("vorr q7, q3, q0", q7, q3, i8, 0x24, q0, i16, 0xff);
    TESTINSN_bin("vorr q4, q4, q4", q4, q4, i16, 0xff, q4, i16, 0xff);
    TESTINSN_bin("vorr q2, q3, q15", q2, q3, i32, 0x24, q15, i32, 0x1f);

    printf("---- VORN ----\n");
    TESTINSN_bin("vorn q0, q1, q2", q0, q1, i8, 0x24, q2, i16, 0x73);
    TESTINSN_bin("vorn q7, q3, q0", q7, q3, i8, 0x24, q0, i16, 0xff);
    TESTINSN_bin("vorn q4, q4, q4", q4, q4, i16, 0xff, q4, i16, 0xff);
    TESTINSN_bin("vorn q2, q3, q15", q2, q3, i32, 0x24, q15, i32, 0x1f);

    printf("---- VEOR ----\n");
    TESTINSN_bin("veor q0, q1, q2", q0, q1, i8, 0x24, q2, i16, 0x77);
    TESTINSN_bin("veor q4, q6, q5", q4, q6, i8, 0xff, q5, i16, 0x57);
    TESTINSN_bin("veor q10, q11, q12", q10, q11, i8, 0xfe, q12, i8, 0xed);
    TESTINSN_bin("veor q15, q15, q15", q15, q15, i8, 0xff, q15, i8, 0xff);
    TESTINSN_bin("veor q0, q1, q2", q0, q1, i8, 0x24, q2, i16, 0x73);
    TESTINSN_bin("veor q7, q3, q0", q7, q3, i8, 0x24, q0, i16, 0xff);
    TESTINSN_bin("veor q4, q4, q4", q4, q4, i16, 0xff, q4, i16, 0xff);
    TESTINSN_bin("veor q2, q3, q15", q2, q3, i32, 0x24, q15, i32, 0x1f);

    printf("---- VBSL ----\n");
    TESTINSN_bin("vbsl q0, q1, q2", q0, q1, i8, 0x24, q2, i16, 0x77);
    TESTINSN_bin("vbsl q4, q6, q5", q4, q6, i8, 0xff, q5, i16, 0x57);
    TESTINSN_bin("vbsl q10, q11, q12", q10, q11, i8, 0xfe, q12, i8, 0xed);
    TESTINSN_bin("vbsl q15, q15, q15", q15, q15, i8, 0xff, q15, i8, 0xff);
    TESTINSN_bin("vbsl q0, q1, q2", q0, q1, i8, 0x24, q2, i16, 0x73);
    TESTINSN_bin("vbsl q7, q3, q0", q7, q3, i8, 0x24, q0, i16, 0xff);
    TESTINSN_bin("vbsl q4, q4, q4", q4, q4, i16, 0xff, q4, i16, 0xff);
    TESTINSN_bin("vbsl q2, q3, q15", q2, q3, i32, 0x24, q15, i32, 0x1f);

    printf("---- VBIT ----\n");
    TESTINSN_bin("vbit q0, q1, q2", q0, q1, i8, 0x24, q2, i16, 0x77);
    TESTINSN_bin("vbit q4, q6, q5", q4, q6, i8, 0xff, q5, i16, 0x57);
    TESTINSN_bin("vbit q10, q11, q12", q10, q11, i8, 0xfe, q12, i8, 0xed);
    TESTINSN_bin("vbit q15, q15, q15", q15, q15, i8, 0xff, q15, i8, 0xff);
    TESTINSN_bin("vbit q0, q1, q2", q0, q1, i8, 0x24, q2, i16, 0x73);
    TESTINSN_bin("vbit q7, q3, q0", q7, q3, i8, 0x24, q0, i16, 0xff);
    TESTINSN_bin("vbit q4, q4, q4", q4, q4, i16, 0xff, q4, i16, 0xff);
    TESTINSN_bin("vbit q2, q3, q15", q2, q3, i32, 0x24, q15, i32, 0x1f);

    printf("---- VBIF ----\n");
    TESTINSN_bin("vbif q0, q1, q2", q0, q1, i8, 0x24, q2, i16, 0x77);
    TESTINSN_bin("vbif q4, q6, q5", q4, q6, i8, 0xff, q5, i16, 0x57);
    TESTINSN_bin("vbif q10, q11, q12", q10, q11, i8, 0xfe, q12, i8, 0xed);
    TESTINSN_bin("vbif q15, q15, q15", q15, q15, i8, 0xff, q15, i8, 0xff);
    TESTINSN_bin("vbif q0, q1, q2", q0, q1, i8, 0x24, q2, i16, 0x73);
    TESTINSN_bin("vbif q7, q3, q0", q7, q3, i8, 0x24, q0, i16, 0xff);
    TESTINSN_bin("vbif q4, q4, q4", q4, q4, i16, 0xff, q4, i16, 0xff);
    TESTINSN_bin("vbif q2, q3, q15", q2, q3, i32, 0x24, q15, i32, 0x1f);

    printf("---- VEXT ----\n");
    TESTINSN_bin("vext.8 q0, q1, q2, #0", q0, q1, i8, 0x77, q2, i8, 0xff);
    TESTINSN_bin("vext.8 q0, q1, q2, #1", q0, q1, i8, 0x77, q2, i8, 0xff);
    TESTINSN_bin("vext.8 q0, q1, q2, #9", q0, q1, i8, 0x77, q2, i8, 0xff);
    TESTINSN_bin("vext.8 q0, q1, q2, #15", q0, q1, i8, 0x77, q2, i8, 0xff);
    TESTINSN_bin("vext.8 q10, q11, q12, #4", q10, q11, i8, 0x77, q12, i8, 0xff);
    TESTINSN_bin("vext.8 q0, q5, q15, #12", q0, q5, i8, 0x77, q15, i8, 0xff);

    printf("---- VHADD ----\n");
    TESTINSN_bin("vhadd.s32 q0, q1, q2", q0, q1, i32, 24, q2, i32, 120);
    TESTINSN_bin("vhadd.s32 q0, q1, q2", q0, q1, i32, 140, q2, i32, 120);
    TESTINSN_bin("vhadd.s16 q0, q1, q2", q0, q1, i32, 140, q2, i32, 120);
    TESTINSN_bin("vhadd.s8 q0, q1, q2", q0, q1, i32, 140, q2, i32, 120);
    TESTINSN_bin("vhadd.s8 q0, q1, q2", q0, q1, i8, 141, q2, i8, 121);
    TESTINSN_bin("vhadd.s8 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vhadd.s16 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vhadd.s32 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vhadd.s32 q10, q11, q12", q10, q11, i32, 24, q12, i32, 120);
    TESTINSN_bin("vhadd.u32 q0, q1, q2", q0, q1, i32, 24, q2, i32, 120);
    TESTINSN_bin("vhadd.u32 q0, q1, q2", q0, q1, i32, 140, q2, i32, 120);
    TESTINSN_bin("vhadd.u16 q0, q1, q2", q0, q1, i32, 140, q2, i32, 120);
    TESTINSN_bin("vhadd.u8 q0, q1, q2", q0, q1, i32, 140, q2, i32, 120);
    TESTINSN_bin("vhadd.u8 q0, q1, q2", q0, q1, i8, 141, q2, i8, 121);
    TESTINSN_bin("vhadd.u8 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vhadd.u16 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vhadd.u32 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vhadd.u32 q10, q11, q12", q10, q11, i32, 24, q12, i32, 120);

    printf("---- VHSUB ----\n");
    TESTINSN_bin("vhsub.s32 q0, q1, q2", q0, q1, i32, 24, q2, i32, 120);
    TESTINSN_bin("vhsub.s32 q0, q1, q2", q0, q1, i32, 140, q2, i32, 120);
    TESTINSN_bin("vhsub.s16 q0, q1, q2", q0, q1, i32, 140, q2, i32, 120);
    TESTINSN_bin("vhsub.s8 q0, q1, q2", q0, q1, i32, 140, q2, i32, 120);
    TESTINSN_bin("vhsub.s8 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vhsub.s16 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vhsub.s32 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vhsub.s32 q10, q11, q12", q10, q11, i32, 24, q12, i32, 120);
    TESTINSN_bin("vhsub.u32 q0, q1, q2", q0, q1, i32, 24, q2, i32, 120);
    TESTINSN_bin("vhsub.u32 q0, q1, q2", q0, q1, i32, 140, q2, i32, 120);
    TESTINSN_bin("vhsub.u16 q0, q1, q2", q0, q1, i32, 140, q2, i32, 120);
    TESTINSN_bin("vhsub.u8 q0, q1, q2", q0, q1, i32, 140, q2, i32, 120);
    TESTINSN_bin("vhsub.u8 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vhsub.u16 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vhsub.u32 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vhsub.u32 q10, q11, q12", q10, q11, i32, 24, q12, i32, 120);

    printf("---- VQADD ----\n");
    TESTINSN_bin_q("vqadd.s32 q0, q1, q2", q0, q1, i32, 24, q2, i32, 120);
    TESTINSN_bin_q("vqadd.s32 q0, q1, q2", q0, q1, i32, 140, q2, i32, 120);
    TESTINSN_bin_q("vqadd.s16 q0, q1, q2", q0, q1, i32, 140, q2, i32, 120);
    TESTINSN_bin_q("vqadd.s8 q0, q1, q2", q0, q1, i32, 140, q2, i32, 120);
    TESTINSN_bin_q("vqadd.s8 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 2);
    TESTINSN_bin_q("vqadd.s16 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 2);
    TESTINSN_bin_q("vqadd.s32 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 2);
    TESTINSN_bin_q("vqadd.s32 q10, q11, q12", q10, q11, i32, 24, q12, i32, 120);
    TESTINSN_bin_q("vqadd.u32 q0, q1, q2", q0, q1, i32, 24, q2, i32, 120);
    TESTINSN_bin_q("vqadd.u32 q0, q1, q2", q0, q1, i32, 140, q2, i32, 120);
    TESTINSN_bin_q("vqadd.u16 q0, q1, q2", q0, q1, i32, 140, q2, i32, 120);
    TESTINSN_bin_q("vqadd.u8 q0, q1, q2", q0, q1, i32, 140, q2, i32, 120);
    TESTINSN_bin_q("vqadd.u8 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 2);
    TESTINSN_bin_q("vqadd.u16 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 2);
    TESTINSN_bin_q("vqadd.u32 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 2);
    TESTINSN_bin_q("vqadd.u32 q10, q11, q12", q10, q11, i32, 24, q12, i32, 120);

    printf("---- VQSUB ----\n");
    TESTINSN_bin_q("vqsub.s32 q0, q1, q2", q0, q1, i32, 24, q2, i32, 120);
    TESTINSN_bin_q("vqsub.s32 q0, q1, q2", q0, q1, i32, 140, q2, i32, 120);
    TESTINSN_bin_q("vqsub.s16 q0, q1, q2", q0, q1, i32, 140, q2, i32, 120);
    TESTINSN_bin_q("vqsub.s8 q0, q1, q2", q0, q1, i32, 140, q2, i32, 120);
    TESTINSN_bin_q("vqsub.s8 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 2);
    TESTINSN_bin_q("vqsub.s16 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 2);
    TESTINSN_bin_q("vqsub.s32 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 2);
    TESTINSN_bin_q("vqsub.s32 q10, q11, q12", q10, q11, i32, 24, q12, i32, 120);
    TESTINSN_bin_q("vqsub.u32 q0, q1, q2", q0, q1, i32, 24, q2, i32, 120);
    TESTINSN_bin_q("vqsub.u32 q0, q1, q2", q0, q1, i32, 140, q2, i32, 120);
    TESTINSN_bin_q("vqsub.u16 q0, q1, q2", q0, q1, i32, 140, q2, i32, 120);
    TESTINSN_bin_q("vqsub.u8 q0, q1, q2", q0, q1, i32, 140, q2, i32, 120);
    TESTINSN_bin_q("vqsub.u8 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 2);
    TESTINSN_bin_q("vqsub.u16 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 2);
    TESTINSN_bin_q("vqsub.u32 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 2);
    TESTINSN_bin_q("vqsub.u32 q10, q11, q12", q10, q11, i32, 24, q12, i32, 120);

    printf("---- VRHADD ----\n");
    TESTINSN_bin("vrhadd.s32 q0, q1, q2", q0, q1, i32, 25, q2, i32, 120);
    TESTINSN_bin("vrhadd.s32 q0, q1, q2", q0, q1, i32, 25, q2, i32, 121);
    TESTINSN_bin("vrhadd.s32 q0, q1, q2", q0, q1, i32, 140, q2, i32, 120);
    TESTINSN_bin("vrhadd.s16 q0, q1, q2", q0, q1, i32, 140, q2, i32, 120);
    TESTINSN_bin("vrhadd.s8 q0, q1, q2", q0, q1, i32, 140, q2, i32, 120);
    TESTINSN_bin("vrhadd.s8 q5, q7, q5", q5, q7, i32, (1 << 31) + 1, q5, i32, (1 << 31) + 2);
    TESTINSN_bin("vrhadd.s16 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vrhadd.s32 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vrhadd.s8 q5, q7, q5", q5, q7, i32, (1 << 31) + 1, q5, i32, (1 << 31) + 3);
    TESTINSN_bin("vrhadd.s16 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 3);
    TESTINSN_bin("vrhadd.s32 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 3);
    TESTINSN_bin("vrhadd.s8 q5, q7, q5", q5, q7, i32, (1 << 31) + 4, q5, i32, (1 << 31) + 2);
    TESTINSN_bin("vrhadd.s16 q0, q1, q2", q0, q1, i32, (1 << 31) + 4, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vrhadd.s32 q0, q1, q2", q0, q1, i32, (1 << 31) + 4, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vrhadd.s32 q10, q11, q12", q10, q11, i32, 24, q12, i32, 120);
    TESTINSN_bin("vrhadd.u32 q0, q1, q2", q0, q1, i32, 25, q2, i32, 120);
    TESTINSN_bin("vrhadd.u32 q0, q1, q2", q0, q1, i32, 140, q2, i32, 120);
    TESTINSN_bin("vrhadd.u16 q0, q1, q2", q0, q1, i32, 140, q2, i32, 120);
    TESTINSN_bin("vrhadd.u8 q0, q1, q2", q0, q1, i32, 140, q2, i32, 120);
    TESTINSN_bin("vrhadd.u8 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vrhadd.u16 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vrhadd.u32 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vrhadd.u8 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 3);
    TESTINSN_bin("vrhadd.u16 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 3);
    TESTINSN_bin("vrhadd.u32 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 3);
    TESTINSN_bin("vrhadd.u8 q0, q1, q2", q0, q1, i32, (1 << 31) + 4, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vrhadd.u16 q0, q1, q2", q0, q1, i32, (1 << 31) + 4, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vrhadd.u32 q0, q1, q2", q0, q1, i32, (1 << 31) + 4, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vrhadd.u32 q10, q11, q12", q10, q11, i32, 24, q12, i32, 120);

    printf("---- VCGT ----\n");
    TESTINSN_bin("vcgt.s32 q0, q1, q2", q0, q1, i32, 25, q2, i32, 120);
    TESTINSN_bin("vcgt.s32 q0, q1, q2", q0, q1, i32, 25, q2, i32, 121);
    TESTINSN_bin("vcgt.s32 q0, q1, q2", q0, q1, i32, 140, q2, i32, 120);
    TESTINSN_bin("vcgt.s16 q0, q1, q2", q0, q1, i32, 140, q2, i32, 120);
    TESTINSN_bin("vcgt.s8 q0, q1, q2", q0, q1, i32, 140, q2, i32, 120);
    TESTINSN_bin("vcgt.s32 q0, q1, q2", q0, q1, i32, 120, q2, i32, 120);
    TESTINSN_bin("vcgt.s16 q0, q1, q2", q0, q1, i32, 120, q2, i32, 120);
    TESTINSN_bin("vcgt.s8 q0, q1, q2", q0, q1, i32, 120, q2, i32, 120);
    TESTINSN_bin("vcgt.s32 q0, q1, q2", q0, q1, i32, 120, q2, i32, 140);
    TESTINSN_bin("vcgt.s16 q0, q1, q2", q0, q1, i32, 120, q2, i32, 140);
    TESTINSN_bin("vcgt.s8 q0, q1, q2", q0, q1, i32, 120, q2, i32, 140);
    TESTINSN_bin("vcgt.s8 q5, q7, q5", q5, q7, i32, (1 << 31) + 3, q5, i32, (1 << 31) + 2);
    TESTINSN_bin("vcgt.s16 q0, q1, q2", q0, q1, i32, (1 << 31) + 3, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vcgt.s32 q0, q1, q2", q0, q1, i32, (1 << 31) + 3, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vcgt.s8 q5, q7, q5", q5, q7, i32, (1 << 31) + 1, q5, i32, (1 << 31) + 3);
    TESTINSN_bin("vcgt.s16 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 3);
    TESTINSN_bin("vcgt.s32 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 3);
    TESTINSN_bin("vcgt.s8 q5, q7, q5", q5, q7, i32, (1 << 31) + 2, q5, i32, (1 << 31) + 2);
    TESTINSN_bin("vcgt.s16 q0, q1, q2", q0, q1, i32, (1 << 31) + 2, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vcgt.s32 q0, q1, q2", q0, q1, i32, (1 << 31) + 2, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vcgt.s32 q10, q11, q12", q10, q11, i32, 24, q12, i32, 120);
    TESTINSN_bin("vcgt.u32 q0, q1, q2", q0, q1, i32, 25, q2, i32, 120);
    TESTINSN_bin("vcgt.u32 q0, q1, q2", q0, q1, i32, 140, q2, i32, 120);
    TESTINSN_bin("vcgt.u16 q0, q1, q2", q0, q1, i32, 140, q2, i32, 120);
    TESTINSN_bin("vcgt.u8 q0, q1, q2", q0, q1, i32, 140, q2, i32, 120);
    TESTINSN_bin("vcgt.u32 q0, q1, q2", q0, q1, i32, 120, q2, i32, 120);
    TESTINSN_bin("vcgt.u16 q0, q1, q2", q0, q1, i32, 120, q2, i32, 120);
    TESTINSN_bin("vcgt.u8 q0, q1, q2", q0, q1, i32, 120, q2, i32, 120);
    TESTINSN_bin("vcgt.u32 q0, q1, q2", q0, q1, i32, 140, q2, i32, 140);
    TESTINSN_bin("vcgt.u16 q0, q1, q2", q0, q1, i32, 140, q2, i32, 140);
    TESTINSN_bin("vcgt.u8 q0, q1, q2", q0, q1, i32, 140, q2, i32, 140);
    TESTINSN_bin("vcgt.u8 q0, q1, q2", q0, q1, i32, (1 << 31) + 3, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vcgt.u16 q0, q1, q2", q0, q1, i32, (1 << 31) + 3, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vcgt.u32 q0, q1, q2", q0, q1, i32, (1 << 31) + 3, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vcgt.u8 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 3);
    TESTINSN_bin("vcgt.u16 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 3);
    TESTINSN_bin("vcgt.u32 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 3);
    TESTINSN_bin("vcgt.u8 q0, q1, q2", q0, q1, i32, (1 << 31) + 2, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vcgt.u16 q0, q1, q2", q0, q1, i32, (1 << 31) + 2, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vcgt.u32 q0, q1, q2", q0, q1, i32, (1 << 31) + 2, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vcgt.u32 q10, q11, q12", q10, q11, i32, 24, q12, i32, 120);

    printf("---- VCGE ----\n");
    TESTINSN_bin("vcge.s32 q0, q1, q2", q0, q1, i32, 25, q2, i32, 120);
    TESTINSN_bin("vcge.s32 q0, q1, q2", q0, q1, i32, 25, q2, i32, 121);
    TESTINSN_bin("vcge.s32 q0, q1, q2", q0, q1, i32, 140, q2, i32, 120);
    TESTINSN_bin("vcge.s16 q0, q1, q2", q0, q1, i32, 140, q2, i32, 120);
    TESTINSN_bin("vcge.s8 q0, q1, q2", q0, q1, i32, 140, q2, i32, 120);
    TESTINSN_bin("vcge.s32 q0, q1, q2", q0, q1, i32, 120, q2, i32, 120);
    TESTINSN_bin("vcge.s16 q0, q1, q2", q0, q1, i32, 120, q2, i32, 120);
    TESTINSN_bin("vcge.s8 q0, q1, q2", q0, q1, i32, 120, q2, i32, 120);
    TESTINSN_bin("vcge.s32 q0, q1, q2", q0, q1, i32, 120, q2, i32, 140);
    TESTINSN_bin("vcge.s16 q0, q1, q2", q0, q1, i32, 120, q2, i32, 140);
    TESTINSN_bin("vcge.s8 q0, q1, q2", q0, q1, i32, 120, q2, i32, 140);
    TESTINSN_bin("vcge.s8 q5, q7, q5", q5, q7, i32, (1 << 31) + 3, q5, i32, (1 << 31) + 2);
    TESTINSN_bin("vcge.s16 q0, q1, q2", q0, q1, i32, (1 << 31) + 3, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vcge.s32 q0, q1, q2", q0, q1, i32, (1 << 31) + 3, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vcge.s8 q5, q7, q5", q5, q7, i32, (1 << 31) + 1, q5, i32, (1 << 31) + 3);
    TESTINSN_bin("vcge.s16 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 3);
    TESTINSN_bin("vcge.s32 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 3);
    TESTINSN_bin("vcge.s8 q5, q7, q5", q5, q7, i32, (1 << 31) + 2, q5, i32, (1 << 31) + 2);
    TESTINSN_bin("vcge.s16 q0, q1, q2", q0, q1, i32, (1 << 31) + 2, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vcge.s32 q0, q1, q2", q0, q1, i32, (1 << 31) + 2, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vcge.s32 q10, q11, q12", q10, q11, i32, 24, q12, i32, 120);
    TESTINSN_bin("vcge.u32 q0, q1, q2", q0, q1, i32, 25, q2, i32, 120);
    TESTINSN_bin("vcge.u32 q0, q1, q2", q0, q1, i32, 140, q2, i32, 120);
    TESTINSN_bin("vcge.u16 q0, q1, q2", q0, q1, i32, 140, q2, i32, 120);
    TESTINSN_bin("vcge.u8 q0, q1, q2", q0, q1, i32, 140, q2, i32, 120);
    TESTINSN_bin("vcge.u32 q0, q1, q2", q0, q1, i32, 120, q2, i32, 120);
    TESTINSN_bin("vcge.u16 q0, q1, q2", q0, q1, i32, 120, q2, i32, 120);
    TESTINSN_bin("vcge.u8 q0, q1, q2", q0, q1, i32, 120, q2, i32, 120);
    TESTINSN_bin("vcge.u32 q0, q1, q2", q0, q1, i32, 140, q2, i32, 140);
    TESTINSN_bin("vcge.u16 q0, q1, q2", q0, q1, i32, 140, q2, i32, 140);
    TESTINSN_bin("vcge.u8 q0, q1, q2", q0, q1, i32, 140, q2, i32, 140);
    TESTINSN_bin("vcge.u8 q0, q1, q2", q0, q1, i32, (1 << 31) + 3, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vcge.u16 q0, q1, q2", q0, q1, i32, (1 << 31) + 3, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vcge.u32 q0, q1, q2", q0, q1, i32, (1 << 31) + 3, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vcge.u8 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 3);
    TESTINSN_bin("vcge.u16 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 3);
    TESTINSN_bin("vcge.u32 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 3);
    TESTINSN_bin("vcge.u8 q0, q1, q2", q0, q1, i32, (1 << 31) + 2, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vcge.u16 q0, q1, q2", q0, q1, i32, (1 << 31) + 2, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vcge.u32 q0, q1, q2", q0, q1, i32, (1 << 31) + 2, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vcge.u32 q10, q11, q12", q10, q11, i32, 24, q12, i32, 120);

    printf("---- VSHL (register) ----\n");
    TESTINSN_bin("vshl.s8 q0, q1, q2", q0, q1, i32, 24, q2, i32, 1);
    TESTINSN_bin("vshl.s8 q8, q1, q12", q8, q1, i32, 24, q12, i32, 8);
    TESTINSN_bin("vshl.s8 q10, q11, q7", q10, q11, i32, 24, q7, i32, 4);
    TESTINSN_bin("vshl.s16 q3, q8, q11", q3, q8, i32, 14, q11, i32, 2);
    TESTINSN_bin("vshl.s16 q5, q12, q14", q5, q12, i32, (1 << 30), q14, i32, 1);
    TESTINSN_bin("vshl.s16 q15, q2, q1", q15, q2, i32, (1 << 30), q1, i32, 11);
    TESTINSN_bin("vshl.s32 q9, q12, q15", q9, q12, i32, (1 << 31) + 2, q15, i32, 2);
    TESTINSN_bin("vshl.s32 q11, q2, q0", q11, q2, i32, -1, q0, i32, 12);
    TESTINSN_bin("vshl.s32 q5, q2, q3", q5, q2, i32, (1 << 30), q3, i32, 21);
    TESTINSN_bin("vshl.s64 q15, q12, q4", q15, q12, i32, 5, q4, i32, 20);
    TESTINSN_bin("vshl.s64 q8, q2, q4", q8, q2, i32, 15, q4, i32, 4);
    TESTINSN_bin("vshl.s64 q5, q12, q4", q5, q12, i32, (1 << 31) + 1, q4, i32, 30);
    TESTINSN_bin("vshl.u8 q0, q1, q2", q0, q1, i32, 24, q2, i32, 1);
    TESTINSN_bin("vshl.u8 q8, q1, q12", q8, q1, i32, 24, q12, i32, 8);
    TESTINSN_bin("vshl.u8 q10, q11, q7", q10, q11, i32, 24, q7, i32, 4);
    TESTINSN_bin("vshl.u16 q3, q8, q11", q3, q8, i32, 14, q11, i32, 2);
    TESTINSN_bin("vshl.u16 q5, q12, q14", q5, q12, i32, (1 << 30), q14, i32, 1);
    TESTINSN_bin("vshl.u16 q15, q2, q1", q15, q2, i32, (1 << 30), q1, i32, 11);
    TESTINSN_bin("vshl.u32 q9, q12, q15", q9, q12, i32, (1 << 31) + 2, q15, i32, 2);
    TESTINSN_bin("vshl.u32 q11, q2, q0", q11, q2, i32, -1, q0, i32, 12);
    TESTINSN_bin("vshl.u32 q5, q2, q3", q5, q2, i32, (1 << 30), q3, i32, 21);
    TESTINSN_bin("vshl.u64 q15, q12, q4", q15, q12, i32, 5, q4, i32, 20);
    TESTINSN_bin("vshl.u64 q8, q2, q4", q8, q2, i32, 15, q4, i32, 4);
    TESTINSN_bin("vshl.u64 q5, q12, q4", q5, q12, i32, (1 << 31) + 1, q4, i32, 30);

    printf("---- VQSHL (register) ----\n");
    TESTINSN_bin_q("vqshl.s64 q0, q1, q2", q0, q1, i32, 1, q2, i32, 1);
    TESTINSN_bin_q("vqshl.s64 q3, q4, q5", q3, q4, i32, -127, q5, i32, 1);
    TESTINSN_bin_q("vqshl.s64 q3, q4, q5", q3, q4, i32, -127, q5, i32, -3);
    TESTINSN_bin_q("vqshl.s64 q0, q1, q2", q0, q1, i32, 16, q2, i32, 14);
    TESTINSN_bin_q("vqshl.s64 q13, q14, q15", q13, q14, i32, -17, q15, i32, -26);
    TESTINSN_bin_q("vqshl.s64 q7, q8, q2", q7, q8, i32, 24, q2, i32, -60);
    TESTINSN_bin_q("vqshl.s32 q3, q4, q15", q3, q4, i32, 127, q15, i32, -30);
    TESTINSN_bin_q("vqshl.s32 q2, q8, q4", q2, q8, i32, -11, q4, i32, -4);
    TESTINSN_bin_q("vqshl.s32 q12, q11, q13", q12, q11, i32, -120, q13, i32, -9);
    TESTINSN_bin_q("vqshl.s32 q0, q1, q2", q0, q1, i32, 34, q2, i32, -7);
    TESTINSN_bin_q("vqshl.s32 q9, q10, q11", q9, q10, i32, (1 << 31) + 8, q11, i32, -1);
    TESTINSN_bin_q("vqshl.s32 q13, q3, q5", q13, q3, i32, (1 << 27), q5, i32, 3);
    TESTINSN_bin_q("vqshl.s16 q11, q10, q2", q11, q10, i32, (1 << 31), q2, i32, -31);
    TESTINSN_bin_q("vqshl.s16 q3, q14, q7", q3, q14, i32, (1 << 31), q7, i32, -3);
    TESTINSN_bin_q("vqshl.s16 q0, q11, q2", q0, q11, i32, (1 << 31) + 256, q2, i32, -1);
    TESTINSN_bin_q("vqshl.s16 q1, q2, q3", q1, q2, i32, (1 << 31) + 256, q3, i32, -31);
    TESTINSN_bin_q("vqshl.s16 q3, q4, q5", q3, q4, i32, (1 << 31) + (1 << 29), q5, i32, -13);
    TESTINSN_bin_q("vqshl.s16 q0, q15, q2", q0, q15, i32, 1, q2, i32, 30);
    TESTINSN_bin_q("vqshl.s8 q2, q7, q11", q2, q7, i32, -1, q11, i32, 40);
    TESTINSN_bin_q("vqshl.s8 q13, q1, q2", q13, q1, i32, -4, q2, i32, 30);
    TESTINSN_bin_q("vqshl.s8 q3, q7, q5", q3, q7, i32, (1 << 31) + 11, q5, i32, 3);
    TESTINSN_bin_q("vqshl.s8 q10, q11, q12", q10, q11, i32, (1 << 16), q12, i32, 16);
    TESTINSN_bin_q("vqshl.s8 q6, q7, q8", q6, q7, i32, (1 << 30), q8, i32, 2);
    TESTINSN_bin_q("vqshl.s8 q10, q11, q12", q10, q11, i32, 24, q12, i32, 120);
    TESTINSN_bin_q("vqshl.u64 q0, q1, q2", q0, q1, i32, 1, q2, i32, 1);
    TESTINSN_bin_q("vqshl.u64 q3, q4, q5", q3, q4, i32, -127, q5, i32, 1);
    TESTINSN_bin_q("vqshl.u64 q3, q4, q5", q3, q4, i32, -127, q5, i32, -3);
    TESTINSN_bin_q("vqshl.u64 q0, q1, q2", q0, q1, i32, 16, q2, i32, 14);
    TESTINSN_bin_q("vqshl.u64 q13, q14, q15", q13, q14, i32, -17, q15, i32, -26);
    TESTINSN_bin_q("vqshl.u64 q7, q8, q2", q7, q8, i32, 24, q2, i32, -60);
    TESTINSN_bin_q("vqshl.u32 q3, q4, q15", q3, q4, i32, 127, q15, i32, -30);
    TESTINSN_bin_q("vqshl.u32 q2, q8, q4", q2, q8, i32, -11, q4, i32, -4);
    TESTINSN_bin_q("vqshl.u32 q12, q11, q13", q12, q11, i32, -120, q13, i32, -9);
    TESTINSN_bin_q("vqshl.u32 q0, q1, q2", q0, q1, i32, 34, q2, i32, -7);
    TESTINSN_bin_q("vqshl.u32 q9, q10, q11", q9, q10, i32, (1 << 31) + 8, q11, i32, -1);
    TESTINSN_bin_q("vqshl.u32 q13, q3, q5", q13, q3, i32, (1 << 27), q5, i32, 3);
    TESTINSN_bin_q("vqshl.u16 q11, q10, q2", q11, q10, i32, (1 << 31), q2, i32, -31);
    TESTINSN_bin_q("vqshl.u16 q3, q14, q7", q3, q14, i32, (1 << 31), q7, i32, -3);
    TESTINSN_bin_q("vqshl.u16 q0, q11, q2", q0, q11, i32, (1 << 31) + 256, q2, i32, -1);
    TESTINSN_bin_q("vqshl.u16 q1, q2, q3", q1, q2, i32, (1 << 31) + 256, q3, i32, -31);
    TESTINSN_bin_q("vqshl.u16 q3, q4, q5", q3, q4, i32, (1 << 31) + (1 << 29), q5, i32, -13);
    TESTINSN_bin_q("vqshl.u16 q0, q15, q2", q0, q15, i32, 1, q2, i32, 30);
    TESTINSN_bin_q("vqshl.u8 q2, q7, q11", q2, q7, i32, -1, q11, i32, 40);
    TESTINSN_bin_q("vqshl.u8 q13, q1, q2", q13, q1, i32, -4, q2, i32, 30);
    TESTINSN_bin_q("vqshl.u8 q3, q7, q5", q3, q7, i32, (1 << 31) + 11, q5, i32, 3);
    TESTINSN_bin_q("vqshl.u8 q10, q11, q12", q10, q11, i32, (1 << 16), q12, i32, 16);
    TESTINSN_bin_q("vqshl.u8 q6, q7, q8", q6, q7, i32, (1 << 30), q8, i32, 2);
    TESTINSN_bin_q("vqshl.u8 q10, q11, q12", q10, q11, i32, 24, q12, i32, 120);

    printf("---- VQSHL / VQSHLU (immediate) ----\n");
    TESTINSN_un_q("vqshl.s64 q0, q1, #1", q0, q1, i32, 1);
    TESTINSN_un_q("vqshl.s64 q15, q14, #1", q15, q14, i32, -127);
    TESTINSN_un_q("vqshl.s64 q5, q4, #0", q5, q4, i32, -127);
    TESTINSN_un_q("vqshl.s64 q5, q4, #63", q5, q4, i32, 16);
    TESTINSN_un_q("vqshl.s64 q5, q4, #60", q5, q4, i32, 16);
    TESTINSN_un_q("vqshl.s64 q5, q4, #59", q5, q4, i32, 16);
    TESTINSN_un_q("vqshl.s64 q5, q4, #58", q5, q4, i32, 16);
    TESTINSN_un_q("vqshl.s64 q5, q4, #17", q5, q4, i32, 16);
    TESTINSN_un_q("vqshl.s64 q5, q4, #63", q5, q4, i32, -1);
    TESTINSN_un_q("vqshl.s64 q5, q4, #60", q5, q4, i32, -1);
    TESTINSN_un_q("vqshl.s64 q5, q4, #7", q5, q4, i32, (1 << 31) + 2);
    TESTINSN_un_q("vqshl.s32 q10, q11, #1", q10, q11, i32, 1);
    TESTINSN_un_q("vqshl.s32 q15, q14, #1", q15, q14, i32, -127);
    TESTINSN_un_q("vqshl.s32 q5, q4, #0", q5, q4, i32, -127);
    TESTINSN_un_q("vqshl.s32 q5, q4, #31", q5, q4, i32, 16);
    TESTINSN_un_q("vqshl.s32 q5, q4, #28", q5, q4, i32, 16);
    TESTINSN_un_q("vqshl.s32 q5, q4, #27", q5, q4, i32, 16);
    TESTINSN_un_q("vqshl.s32 q5, q4, #26", q5, q4, i32, 16);
    TESTINSN_un_q("vqshl.s32 q5, q4, #17", q5, q4, i32, 16);
    TESTINSN_un_q("vqshl.s32 q5, q4, #31", q5, q4, i32, -1);
    TESTINSN_un_q("vqshl.s32 q5, q4, #29", q5, q4, i32, -1);
    TESTINSN_un_q("vqshl.s32 q5, q4, #7", q5, q4, i32, (1 << 31) + 2);
    TESTINSN_un_q("vqshl.s16 q9, q8, #1", q9, q8, i32, 1);
    TESTINSN_un_q("vqshl.s16 q15, q14, #1", q15, q14, i32, -127);
    TESTINSN_un_q("vqshl.s16 q5, q4, #0", q5, q4, i32, -127);
    TESTINSN_un_q("vqshl.s16 q9, q8, #15", q9, q8, i32, 16);
    TESTINSN_un_q("vqshl.s16 q5, q4, #12", q5, q4, i32, 16);
    TESTINSN_un_q("vqshl.s16 q5, q4, #11", q5, q4, i32, 16);
    TESTINSN_un_q("vqshl.s16 q5, q4, #10", q5, q4, i32, 16);
    TESTINSN_un_q("vqshl.s16 q5, q4, #4", q5, q4, i32, 16);
    TESTINSN_un_q("vqshl.s16 q5, q4, #15", q5, q4, i32, -1);
    TESTINSN_un_q("vqshl.s16 q5, q4, #12", q5, q4, i32, -1);
    TESTINSN_un_q("vqshl.s16 q5, q4, #7", q5, q4, i32, (1 << 31) + 2);
    TESTINSN_un_q("vqshl.s8 q0, q1, #1", q0, q1, i32, 1);
    TESTINSN_un_q("vqshl.s8 q15, q14, #1", q15, q14, i32, -127);
    TESTINSN_un_q("vqshl.s8 q5, q4, #0", q5, q4, i32, -127);
    TESTINSN_un_q("vqshl.s8 q5, q4, #7", q5, q4, i32, 16);
    TESTINSN_un_q("vqshl.s8 q5, q4, #4", q5, q4, i32, 16);
    TESTINSN_un_q("vqshl.s8 q5, q4, #3", q5, q4, i32, 16);
    TESTINSN_un_q("vqshl.s8 q5, q4, #2", q5, q4, i32, 16);
    TESTINSN_un_q("vqshl.s8 q5, q4, #1", q5, q4, i32, 16);
    TESTINSN_un_q("vqshl.s8 q5, q4, #7", q5, q4, i32, -1);
    TESTINSN_un_q("vqshl.s8 q5, q4, #5", q5, q4, i32, -1);
    TESTINSN_un_q("vqshl.s8 q5, q4, #2", q5, q4, i32, (1 << 31) + 2);
    TESTINSN_un_q("vqshl.u64 q0, q1, #1", q0, q1, i32, 1);
    TESTINSN_un_q("vqshl.u64 q15, q14, #1", q15, q14, i32, -127);
    TESTINSN_un_q("vqshl.u64 q5, q4, #0", q5, q4, i32, -127);
    TESTINSN_un_q("vqshl.u64 q5, q4, #63", q5, q4, i32, 16);
    TESTINSN_un_q("vqshl.u64 q5, q4, #60", q5, q4, i32, 16);
    TESTINSN_un_q("vqshl.u64 q5, q4, #59", q5, q4, i32, 16);
    TESTINSN_un_q("vqshl.u64 q5, q4, #58", q5, q4, i32, 16);
    TESTINSN_un_q("vqshl.u64 q5, q4, #17", q5, q4, i32, 16);
    TESTINSN_un_q("vqshl.u64 q5, q4, #63", q5, q4, i32, -1);
    TESTINSN_un_q("vqshl.u64 q5, q4, #60", q5, q4, i32, -1);
    TESTINSN_un_q("vqshl.u64 q5, q4, #7", q5, q4, i32, (1 << 31) + 2);
    TESTINSN_un_q("vqshl.u32 q10, q11, #1", q10, q11, i32, 1);
    TESTINSN_un_q("vqshl.u32 q15, q14, #1", q15, q14, i32, -127);
    TESTINSN_un_q("vqshl.u32 q5, q4, #0", q5, q4, i32, -127);
    TESTINSN_un_q("vqshl.u32 q5, q4, #31", q5, q4, i32, 16);
    TESTINSN_un_q("vqshl.u32 q5, q4, #28", q5, q4, i32, 16);
    TESTINSN_un_q("vqshl.u32 q5, q4, #27", q5, q4, i32, 16);
    TESTINSN_un_q("vqshl.u32 q5, q4, #26", q5, q4, i32, 16);
    TESTINSN_un_q("vqshl.u32 q5, q4, #17", q5, q4, i32, 16);
    TESTINSN_un_q("vqshl.u32 q5, q4, #31", q5, q4, i32, -1);
    TESTINSN_un_q("vqshl.u32 q5, q4, #29", q5, q4, i32, -1);
    TESTINSN_un_q("vqshl.u32 q5, q4, #7", q5, q4, i32, (1 << 31) + 2);
    TESTINSN_un_q("vqshl.u16 q9, q8, #1", q9, q8, i32, 1);
    TESTINSN_un_q("vqshl.u16 q15, q14, #1", q15, q14, i32, -127);
    TESTINSN_un_q("vqshl.u16 q5, q4, #0", q5, q4, i32, -127);
    TESTINSN_un_q("vqshl.u16 q9, q8, #15", q9, q8, i32, 16);
    TESTINSN_un_q("vqshl.u16 q5, q4, #12", q5, q4, i32, 16);
    TESTINSN_un_q("vqshl.u16 q5, q4, #11", q5, q4, i32, 16);
    TESTINSN_un_q("vqshl.u16 q5, q4, #10", q5, q4, i32, 16);
    TESTINSN_un_q("vqshl.u16 q5, q4, #4", q5, q4, i32, 16);
    TESTINSN_un_q("vqshl.u16 q5, q4, #15", q5, q4, i32, -1);
    TESTINSN_un_q("vqshl.u16 q5, q4, #12", q5, q4, i32, -1);
    TESTINSN_un_q("vqshl.u16 q5, q4, #7", q5, q4, i32, (1 << 31) + 2);
    TESTINSN_un_q("vqshl.u8 q0, q1, #1", q0, q1, i32, 1);
    TESTINSN_un_q("vqshl.u8 q15, q14, #1", q15, q14, i32, -127);
    TESTINSN_un_q("vqshl.u8 q5, q4, #0", q5, q4, i32, -127);
    TESTINSN_un_q("vqshl.u8 q5, q4, #7", q5, q4, i32, 16);
    TESTINSN_un_q("vqshl.u8 q5, q4, #4", q5, q4, i32, 16);
    TESTINSN_un_q("vqshl.u8 q5, q4, #3", q5, q4, i32, 16);
    TESTINSN_un_q("vqshl.u8 q5, q4, #2", q5, q4, i32, 16);
    TESTINSN_un_q("vqshl.u8 q5, q4, #1", q5, q4, i32, 16);
    TESTINSN_un_q("vqshl.u8 q5, q4, #7", q5, q4, i32, -1);
    TESTINSN_un_q("vqshl.u8 q5, q4, #5", q5, q4, i32, -1);
    TESTINSN_un_q("vqshl.u8 q5, q4, #2", q5, q4, i32, (1 << 31) + 2);
    TESTINSN_un_q("vqshlu.s64 q0, q1, #1", q0, q1, i32, 1);
    TESTINSN_un_q("vqshlu.s64 q15, q14, #1", q15, q14, i32, -127);
    TESTINSN_un_q("vqshlu.s64 q5, q4, #0", q5, q4, i32, -127);
    TESTINSN_un_q("vqshlu.s64 q5, q4, #63", q5, q4, i32, 16);
    TESTINSN_un_q("vqshlu.s64 q5, q4, #60", q5, q4, i32, 16);
    TESTINSN_un_q("vqshlu.s64 q5, q4, #59", q5, q4, i32, 16);
    TESTINSN_un_q("vqshlu.s64 q5, q4, #58", q5, q4, i32, 16);
    TESTINSN_un_q("vqshlu.s64 q5, q4, #17", q5, q4, i32, 16);
    TESTINSN_un_q("vqshlu.s64 q5, q4, #63", q5, q4, i32, -1);
    TESTINSN_un_q("vqshlu.s64 q5, q4, #60", q5, q4, i32, -1);
    TESTINSN_un_q("vqshlu.s64 q5, q4, #7", q5, q4, i32, (1 << 31) + 2);
    TESTINSN_un_q("vqshlu.s32 q10, q11, #1", q10, q11, i32, 1);
    TESTINSN_un_q("vqshlu.s32 q15, q14, #1", q15, q14, i32, -127);
    TESTINSN_un_q("vqshlu.s32 q5, q4, #0", q5, q4, i32, -127);
    TESTINSN_un_q("vqshlu.s32 q5, q4, #31", q5, q4, i32, 16);
    TESTINSN_un_q("vqshlu.s32 q5, q4, #28", q5, q4, i32, 16);
    TESTINSN_un_q("vqshlu.s32 q5, q4, #27", q5, q4, i32, 16);
    TESTINSN_un_q("vqshlu.s32 q5, q4, #26", q5, q4, i32, 16);
    TESTINSN_un_q("vqshlu.s32 q5, q4, #17", q5, q4, i32, 16);
    TESTINSN_un_q("vqshlu.s32 q5, q4, #31", q5, q4, i32, -1);
    TESTINSN_un_q("vqshlu.s32 q5, q4, #29", q5, q4, i32, -1);
    TESTINSN_un_q("vqshlu.s32 q5, q4, #7", q5, q4, i32, (1 << 31) + 2);
    TESTINSN_un_q("vqshlu.s16 q9, q8, #1", q9, q8, i32, 1);
    TESTINSN_un_q("vqshlu.s16 q15, q14, #1", q15, q14, i32, -127);
    TESTINSN_un_q("vqshlu.s16 q5, q4, #0", q5, q4, i32, -127);
    TESTINSN_un_q("vqshlu.s16 q9, q8, #15", q9, q8, i32, 16);
    TESTINSN_un_q("vqshlu.s16 q5, q4, #12", q5, q4, i32, 16);
    TESTINSN_un_q("vqshlu.s16 q5, q4, #11", q5, q4, i32, 16);
    TESTINSN_un_q("vqshlu.s16 q5, q4, #10", q5, q4, i32, 16);
    TESTINSN_un_q("vqshlu.s16 q5, q4, #4", q5, q4, i32, 16);
    TESTINSN_un_q("vqshlu.s16 q5, q4, #15", q5, q4, i32, -1);
    TESTINSN_un_q("vqshlu.s16 q5, q4, #12", q5, q4, i32, -1);
    TESTINSN_un_q("vqshlu.s16 q5, q4, #7", q5, q4, i32, (1 << 31) + 2);
    TESTINSN_un_q("vqshlu.s8 q0, q1, #1", q0, q1, i32, 1);
    TESTINSN_un_q("vqshlu.s8 q15, q14, #1", q15, q14, i32, -127);
    TESTINSN_un_q("vqshlu.s8 q5, q4, #0", q5, q4, i32, -127);
    TESTINSN_un_q("vqshlu.s8 q5, q4, #7", q5, q4, i32, 16);
    TESTINSN_un_q("vqshlu.s8 q5, q4, #4", q5, q4, i32, 16);
    TESTINSN_un_q("vqshlu.s8 q5, q4, #3", q5, q4, i32, 16);
    TESTINSN_un_q("vqshlu.s8 q5, q4, #2", q5, q4, i32, 16);
    TESTINSN_un_q("vqshlu.s8 q5, q4, #1", q5, q4, i32, 16);
    TESTINSN_un_q("vqshlu.s8 q5, q4, #7", q5, q4, i32, -1);
    TESTINSN_un_q("vqshlu.s8 q5, q4, #5", q5, q4, i32, -1);
    TESTINSN_un_q("vqshlu.s8 q5, q4, #2", q5, q4, i32, (1 << 31) + 2);

    printf("---- VQRSHL (register) ----\n");
    TESTINSN_bin_q("vqrshl.s64 q0, q1, q2", q0, q1, i32, 1, q2, i32, 1);
    TESTINSN_bin_q("vqrshl.s64 q3, q4, q5", q3, q4, i32, -127, q5, i32, 1);
    TESTINSN_bin_q("vqrshl.s64 q3, q4, q5", q3, q4, i32, -127, q5, i32, -3);
    TESTINSN_bin_q("vqrshl.s64 q0, q1, q2", q0, q1, i32, 16, q2, i32, 14);
    TESTINSN_bin_q("vqrshl.s64 q13, q14, q15", q13, q14, i32, -17, q15, i32, -26);
    TESTINSN_bin_q("vqrshl.s64 q7, q8, q2", q7, q8, i32, 24, q2, i32, -60);
    TESTINSN_bin_q("vqrshl.s32 q3, q4, q15", q3, q4, i32, 127, q15, i32, -30);
    TESTINSN_bin_q("vqrshl.s32 q2, q8, q4", q2, q8, i32, -11, q4, i32, -4);
    TESTINSN_bin_q("vqrshl.s32 q12, q11, q13", q12, q11, i32, -120, q13, i32, -9);
    TESTINSN_bin_q("vqrshl.s32 q0, q1, q2", q0, q1, i32, 34, q2, i32, -7);
    TESTINSN_bin_q("vqrshl.s32 q9, q10, q11", q9, q10, i32, (1 << 31) + 8, q11, i32, -1);
    TESTINSN_bin_q("vqrshl.s32 q13, q3, q5", q13, q3, i32, (1 << 27), q5, i32, 3);
    TESTINSN_bin_q("vqrshl.s16 q11, q10, q2", q11, q10, i32, (1 << 31), q2, i32, -31);
    TESTINSN_bin_q("vqrshl.s16 q3, q14, q7", q3, q14, i32, (1 << 31), q7, i32, -3);
    TESTINSN_bin_q("vqrshl.s16 q0, q11, q2", q0, q11, i32, (1 << 31) + 256, q2, i32, -1);
    TESTINSN_bin_q("vqrshl.s16 q1, q2, q3", q1, q2, i32, (1 << 31) + 256, q3, i32, -31);
    TESTINSN_bin_q("vqrshl.s16 q3, q4, q5", q3, q4, i32, (1 << 31) + (1 << 29), q5, i32, -13);
    TESTINSN_bin_q("vqrshl.s16 q0, q15, q2", q0, q15, i32, 1, q2, i32, 30);
    TESTINSN_bin_q("vqrshl.s8 q2, q7, q11", q2, q7, i32, 0xf, q11, i32, -1);
    TESTINSN_bin_q("vqrshl.s16 q2, q7, q11", q2, q7, i32, 0xf, q11, i32, -1);
    TESTINSN_bin_q("vqrshl.s32 q2, q7, q11", q2, q7, i32, 0xf, q11, i32, -1);
    TESTINSN_bin_q("vqrshl.s8 q2, q7, q11", q2, q7, i32, -1, q11, i32, -1);
    TESTINSN_bin_q("vqrshl.s16 q2, q7, q11", q2, q7, i32, -1, q11, i32, -1);
    TESTINSN_bin_q("vqrshl.s32 q2, q7, q11", q2, q7, i32, -1, q11, i32, -1);
    TESTINSN_bin_q("vqrshl.s8 q2, q7, q11", q2, q7, i32, -2, q11, i32, -1);
    TESTINSN_bin_q("vqrshl.s16 q2, q7, q11", q2, q7, i32, -2, q11, i32, -1);
    TESTINSN_bin_q("vqrshl.s32 q2, q7, q11", q2, q7, i32, -2, q11, i32, -1);
    TESTINSN_bin_q("vqrshl.s8 q2, q7, q11", q2, q7, i32, -1, q11, i32, 0);
    TESTINSN_bin_q("vqrshl.s16 q2, q7, q11", q2, q7, i32, -1, q11, i32, 0);
    TESTINSN_bin_q("vqrshl.s32 q2, q7, q11", q2, q7, i32, -1, q11, i32, 0);
    TESTINSN_bin_q("vqrshl.s8 q2, q7, q11", q2, q7, i32, -1, q11, i32, 40);
    TESTINSN_bin_q("vqrshl.s8 q13, q1, q2", q13, q1, i32, -4, q2, i32, 30);
    TESTINSN_bin_q("vqrshl.s8 q3, q7, q5", q3, q7, i32, (1 << 31) + 11, q5, i32, 3);
    TESTINSN_bin_q("vqrshl.s8 q10, q11, q12", q10, q11, i32, (1 << 16), q12, i32, 16);
    TESTINSN_bin_q("vqrshl.s8 q6, q7, q8", q6, q7, i32, (1 << 30), q8, i32, 2);
    TESTINSN_bin_q("vqrshl.s8 q10, q11, q12", q10, q11, i32, 24, q12, i32, 120);
    TESTINSN_bin_q("vqrshl.u64 q0, q1, q2", q0, q1, i32, 1, q2, i32, 1);
    TESTINSN_bin_q("vqrshl.u64 q3, q4, q5", q3, q4, i32, -127, q5, i32, 1);
    TESTINSN_bin_q("vqrshl.u64 q3, q4, q5", q3, q4, i32, -127, q5, i32, -3);
    TESTINSN_bin_q("vqrshl.u64 q0, q1, q2", q0, q1, i32, 16, q2, i32, 14);
    TESTINSN_bin_q("vqrshl.u64 q13, q14, q15", q13, q14, i32, -17, q15, i32, -26);
    TESTINSN_bin_q("vqrshl.u64 q7, q8, q2", q7, q8, i32, 24, q2, i32, -60);
    TESTINSN_bin_q("vqrshl.u32 q3, q4, q15", q3, q4, i32, 127, q15, i32, -30);
    TESTINSN_bin_q("vqrshl.u32 q2, q8, q4", q2, q8, i32, -11, q4, i32, -4);
    TESTINSN_bin_q("vqrshl.u32 q12, q11, q13", q12, q11, i32, -120, q13, i32, -9);
    TESTINSN_bin_q("vqrshl.u32 q0, q1, q2", q0, q1, i32, 34, q2, i32, -7);
    TESTINSN_bin_q("vqrshl.u32 q9, q10, q11", q9, q10, i32, (1 << 31) + 8, q11, i32, -1);
    TESTINSN_bin_q("vqrshl.u32 q13, q3, q5", q13, q3, i32, (1 << 27), q5, i32, 3);
    TESTINSN_bin_q("vqrshl.u16 q11, q10, q2", q11, q10, i32, (1 << 31), q2, i32, -31);
    TESTINSN_bin_q("vqrshl.u16 q3, q14, q7", q3, q14, i32, (1 << 31), q7, i32, -3);
    TESTINSN_bin_q("vqrshl.u16 q0, q11, q2", q0, q11, i32, (1 << 31) + 256, q2, i32, -1);
    TESTINSN_bin_q("vqrshl.u16 q1, q2, q3", q1, q2, i32, (1 << 31) + 256, q3, i32, -31);
    TESTINSN_bin_q("vqrshl.u16 q3, q4, q5", q3, q4, i32, (1 << 31) + (1 << 29), q5, i32, -13);
    TESTINSN_bin_q("vqrshl.u16 q0, q15, q2", q0, q15, i32, 1, q2, i32, 30);
    TESTINSN_bin_q("vqrshl.u8 q2, q7, q11", q2, q7, i32, -1, q11, i32, 40);
    TESTINSN_bin_q("vqrshl.u8 q2, q7, q11", q2, q7, i32, -1, q11, i32, -1);
    TESTINSN_bin_q("vqrshl.u8 q2, q7, q11", q2, q7, i32, 0xf, q11, i32, -1);
    TESTINSN_bin_q("vqrshl.u16 q2, q7, q11", q2, q7, i32, 0xf, q11, i32, -1);
    TESTINSN_bin_q("vqrshl.u32 q2, q7, q11", q2, q7, i32, 0xf, q11, i32, -1);
    TESTINSN_bin_q("vqrshl.u8 q2, q7, q11", q2, q7, i32, -2, q11, i32, -1);
    TESTINSN_bin_q("vqrshl.u16 q2, q7, q11", q2, q7, i32, -2, q11, i32, -1);
    TESTINSN_bin_q("vqrshl.u32 q2, q7, q11", q2, q7, i32, -2, q11, i32, -1);
    TESTINSN_bin_q("vqrshl.u8 q2, q7, q11", q2, q7, i32, -1, q11, i32, 0);
    TESTINSN_bin_q("vqrshl.u16 q2, q7, q11", q2, q7, i32, -1, q11, i32, 0);
    TESTINSN_bin_q("vqrshl.u32 q2, q7, q11", q2, q7, i32, -1, q11, i32, 0);
    TESTINSN_bin_q("vqrshl.u8 q13, q1, q2", q13, q1, i32, -4, q2, i32, 30);
    TESTINSN_bin_q("vqrshl.u8 q3, q7, q5", q3, q7, i32, (1 << 31) + 11, q5, i32, 3);
    TESTINSN_bin_q("vqrshl.u8 q10, q11, q12", q10, q11, i32, (1 << 16), q12, i32, 16);
    TESTINSN_bin_q("vqrshl.u8 q6, q7, q8", q6, q7, i32, (1 << 30), q8, i32, 2);
    TESTINSN_bin_q("vqrshl.u8 q10, q11, q12", q10, q11, i32, 24, q12, i32, 120);

    printf("---- VRSHL (register) ----\n");
    TESTINSN_bin("vrshl.s64 q0, q1, q2", q0, q1, i32, 1, q2, i32, 1);
    TESTINSN_bin("vrshl.s64 q3, q4, q5", q3, q4, i32, -127, q5, i32, 1);
    TESTINSN_bin("vrshl.s64 q3, q4, q5", q3, q4, i32, -127, q5, i32, -3);
    TESTINSN_bin("vrshl.s64 q0, q1, q2", q0, q1, i32, 16, q2, i32, 14);
    TESTINSN_bin("vrshl.s64 q13, q14, q15", q13, q14, i32, -17, q15, i32, -26);
    TESTINSN_bin("vrshl.s64 q7, q8, q2", q7, q8, i32, 24, q2, i32, -60);
    TESTINSN_bin("vrshl.s32 q3, q4, q15", q3, q4, i32, 127, q15, i32, -30);
    TESTINSN_bin("vrshl.s32 q2, q8, q4", q2, q8, i32, -11, q4, i32, -4);
    TESTINSN_bin("vrshl.s32 q12, q11, q13", q12, q11, i32, -120, q13, i32, -9);
    TESTINSN_bin("vrshl.s32 q0, q1, q2", q0, q1, i32, 34, q2, i32, -7);
    TESTINSN_bin("vrshl.s32 q9, q10, q11", q9, q10, i32, (1 << 31) + 8, q11, i32, -1);
    TESTINSN_bin("vrshl.s32 q13, q3, q5", q13, q3, i32, (1 << 27), q5, i32, 3);
    TESTINSN_bin("vrshl.s16 q11, q10, q2", q11, q10, i32, (1 << 31), q2, i32, -31);
    TESTINSN_bin("vrshl.s16 q3, q14, q7", q3, q14, i32, (1 << 31), q7, i32, -3);
    TESTINSN_bin("vrshl.s16 q0, q11, q2", q0, q11, i32, (1 << 31) + 256, q2, i32, -1);
    TESTINSN_bin("vrshl.s16 q1, q2, q3", q1, q2, i32, (1 << 31) + 256, q3, i32, -31);
    TESTINSN_bin("vrshl.s16 q3, q4, q5", q3, q4, i32, (1 << 31) + (1 << 29), q5, i32, -13);
    TESTINSN_bin("vrshl.s16 q0, q15, q2", q0, q15, i32, 1, q2, i32, 30);
    TESTINSN_bin("vrshl.s8 q2, q7, q11", q2, q7, i32, 0xf, q11, i32, -1);
    TESTINSN_bin("vrshl.s16 q2, q7, q11", q2, q7, i32, 0xf, q11, i32, -1);
    TESTINSN_bin("vrshl.s32 q2, q7, q11", q2, q7, i32, 0xf, q11, i32, -1);
    TESTINSN_bin("vrshl.s8 q2, q7, q11", q2, q7, i32, -1, q11, i32, -1);
    TESTINSN_bin("vrshl.s16 q2, q7, q11", q2, q7, i32, -1, q11, i32, -1);
    TESTINSN_bin("vrshl.s32 q2, q7, q11", q2, q7, i32, -1, q11, i32, -1);
    TESTINSN_bin("vrshl.s8 q2, q7, q11", q2, q7, i32, -2, q11, i32, -1);
    TESTINSN_bin("vrshl.s16 q2, q7, q11", q2, q7, i32, -2, q11, i32, -1);
    TESTINSN_bin("vrshl.s32 q2, q7, q11", q2, q7, i32, -2, q11, i32, -1);
    TESTINSN_bin("vrshl.s8 q2, q7, q11", q2, q7, i32, -1, q11, i32, 0);
    TESTINSN_bin("vrshl.s16 q2, q7, q11", q2, q7, i32, -1, q11, i32, 0);
    TESTINSN_bin("vrshl.s32 q2, q7, q11", q2, q7, i32, -1, q11, i32, 0);
    TESTINSN_bin("vrshl.s8 q2, q7, q11", q2, q7, i32, -1, q11, i32, 40);
    TESTINSN_bin("vrshl.s8 q13, q1, q2", q13, q1, i32, -4, q2, i32, 30);
    TESTINSN_bin("vrshl.s8 q3, q7, q5", q3, q7, i32, (1 << 31) + 11, q5, i32, 3);
    TESTINSN_bin("vrshl.s8 q10, q11, q12", q10, q11, i32, (1 << 16), q12, i32, 16);
    TESTINSN_bin("vrshl.s8 q6, q7, q8", q6, q7, i32, (1 << 30), q8, i32, 2);
    TESTINSN_bin("vrshl.s8 q10, q11, q12", q10, q11, i32, 24, q12, i32, 120);
    TESTINSN_bin("vrshl.u64 q0, q1, q2", q0, q1, i32, 1, q2, i32, 1);
    TESTINSN_bin("vrshl.u64 q3, q4, q5", q3, q4, i32, -127, q5, i32, 1);
    TESTINSN_bin("vrshl.u64 q3, q4, q5", q3, q4, i32, -127, q5, i32, -3);
    TESTINSN_bin("vrshl.u64 q0, q1, q2", q0, q1, i32, 16, q2, i32, 14);
    TESTINSN_bin("vrshl.u64 q13, q14, q15", q13, q14, i32, -17, q15, i32, -26);
    TESTINSN_bin("vrshl.u64 q7, q8, q2", q7, q8, i32, 24, q2, i32, -60);
    TESTINSN_bin("vrshl.u32 q3, q4, q15", q3, q4, i32, 127, q15, i32, -30);
    TESTINSN_bin("vrshl.u32 q2, q8, q4", q2, q8, i32, -11, q4, i32, -4);
    TESTINSN_bin("vrshl.u32 q12, q11, q13", q12, q11, i32, -120, q13, i32, -9);
    TESTINSN_bin("vrshl.u32 q0, q1, q2", q0, q1, i32, 34, q2, i32, -7);
    TESTINSN_bin("vrshl.u32 q9, q10, q11", q9, q10, i32, (1 << 31) + 8, q11, i32, -1);
    TESTINSN_bin("vrshl.u32 q13, q3, q5", q13, q3, i32, (1 << 27), q5, i32, 3);
    TESTINSN_bin("vrshl.u16 q11, q10, q2", q11, q10, i32, (1 << 31), q2, i32, -31);
    TESTINSN_bin("vrshl.u16 q3, q14, q7", q3, q14, i32, (1 << 31), q7, i32, -3);
    TESTINSN_bin("vrshl.u16 q0, q11, q2", q0, q11, i32, (1 << 31) + 256, q2, i32, -1);
    TESTINSN_bin("vrshl.u16 q1, q2, q3", q1, q2, i32, (1 << 31) + 256, q3, i32, -31);
    TESTINSN_bin("vrshl.u16 q3, q4, q5", q3, q4, i32, (1 << 31) + (1 << 29), q5, i32, -13);
    TESTINSN_bin("vrshl.u16 q0, q15, q2", q0, q15, i32, 1, q2, i32, 30);
    TESTINSN_bin("vrshl.u8 q2, q7, q11", q2, q7, i32, -1, q11, i32, 40);
    TESTINSN_bin("vrshl.u8 q2, q7, q11", q2, q7, i32, -1, q11, i32, -1);
    TESTINSN_bin("vrshl.u8 q2, q7, q11", q2, q7, i32, 0xf, q11, i32, -1);
    TESTINSN_bin("vrshl.u16 q2, q7, q11", q2, q7, i32, 0xf, q11, i32, -1);
    TESTINSN_bin("vrshl.u32 q2, q7, q11", q2, q7, i32, 0xf, q11, i32, -1);
    TESTINSN_bin("vrshl.u8 q2, q7, q11", q2, q7, i32, -1, q11, i32, -1);
    TESTINSN_bin("vrshl.u16 q2, q7, q11", q2, q7, i32, -1, q11, i32, -1);
    TESTINSN_bin("vrshl.u32 q2, q7, q11", q2, q7, i32, -1, q11, i32, -1);
    TESTINSN_bin("vrshl.u8 q2, q7, q11", q2, q7, i32, -2, q11, i32, -1);
    TESTINSN_bin("vrshl.u16 q2, q7, q11", q2, q7, i32, -2, q11, i32, -1);
    TESTINSN_bin("vrshl.u32 q2, q7, q11", q2, q7, i32, -2, q11, i32, -1);
    TESTINSN_bin("vrshl.u8 q13, q1, q2", q13, q1, i32, -4, q2, i32, 30);
    TESTINSN_bin("vrshl.u8 q3, q7, q5", q3, q7, i32, (1 << 31) + 11, q5, i32, 3);
    TESTINSN_bin("vrshl.u8 q10, q11, q12", q10, q11, i32, (1 << 16), q12, i32, 16);
    TESTINSN_bin("vrshl.u8 q6, q7, q8", q6, q7, i32, (1 << 30), q8, i32, 2);
    TESTINSN_bin("vrshl.u8 q10, q11, q12", q10, q11, i32, 24, q12, i32, 120);

    printf("---- VMAX (integer) ----\n");
    TESTINSN_bin("vmax.s32 q0, q1, q2", q0, q1, i32, 25, q2, i32, 121);
    TESTINSN_bin("vmax.s32 q0, q1, q2", q0, q1, i32, 250, q2, i32, 121);
    TESTINSN_bin("vmax.s32 q0, q1, q2", q0, q1, i32, 140, q2, i32, 140);
    TESTINSN_bin("vmax.s16 q0, q1, q2", q0, q1, i32, 140, q2, i32, 120);
    TESTINSN_bin("vmax.s8 q0, q1, q2", q0, q1, i32, 120, q2, i32, 120);
    TESTINSN_bin("vmax.s8 q5, q7, q5", q5, q7, i32, (1 << 31) + 1, q5, i32, (1 << 31) + 2);
    TESTINSN_bin("vmax.s16 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vmax.s32 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vmax.s8 q5, q7, q5", q5, q7, i32, (1 << 31) + 1, q5, i32, (1 << 31) + 3);
    TESTINSN_bin("vmax.s16 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 3);
    TESTINSN_bin("vmax.s32 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 3);
    TESTINSN_bin("vmax.s8 q5, q7, q5", q5, q7, i32, (1 << 31) + 4, q5, i32, (1 << 31) + 2);
    TESTINSN_bin("vmax.s16 q0, q1, q2", q0, q1, i32, (1 << 31) + 4, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vmax.s32 q0, q1, q2", q0, q1, i32, (1 << 31) + 4, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vmax.s32 q10, q11, q12", q10, q11, i32, 24, q12, i32, 120);
    TESTINSN_bin("vmax.u32 q0, q1, q2", q0, q1, i32, 25, q2, i32, 120);
    TESTINSN_bin("vmax.u32 q0, q1, q2", q0, q1, i32, 250, q2, i32, 120);
    TESTINSN_bin("vmax.u32 q0, q1, q2", q0, q1, i32, 140, q2, i32, 140);
    TESTINSN_bin("vmax.u16 q0, q1, q2", q0, q1, i32, 140, q2, i32, 120);
    TESTINSN_bin("vmax.u8 q0, q1, q2", q0, q1, i32, 120, q2, i32, 120);
    TESTINSN_bin("vmax.u8 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vmax.u16 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vmax.u32 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vmax.u8 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 3);
    TESTINSN_bin("vmax.u16 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 3);
    TESTINSN_bin("vmax.u32 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 3);
    TESTINSN_bin("vmax.u8 q0, q1, q2", q0, q1, i32, (1 << 31) + 4, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vmax.u16 q0, q1, q2", q0, q1, i32, (1 << 31) + 4, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vmax.u32 q0, q1, q2", q0, q1, i32, (1 << 31) + 4, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vmax.u32 q10, q11, q12", q10, q11, i32, 24, q12, i32, 120);

    printf("---- VMIN (integer) ----\n");
    TESTINSN_bin("vmin.s32 q0, q1, q2", q0, q1, i32, 25, q2, i32, 121);
    TESTINSN_bin("vmin.s32 q0, q1, q2", q0, q1, i32, 250, q2, i32, 121);
    TESTINSN_bin("vmin.s32 q0, q1, q2", q0, q1, i32, 140, q2, i32, 120);
    TESTINSN_bin("vmin.s16 q0, q1, q2", q0, q1, i32, 120, q2, i32, 120);
    TESTINSN_bin("vmin.s8 q0, q1, q2", q0, q1, i32, 140, q2, i32, 140);
    TESTINSN_bin("vmin.s8 q5, q7, q5", q5, q7, i32, (1 << 31) + 1, q5, i32, (1 << 31) + 2);
    TESTINSN_bin("vmin.s16 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vmin.s32 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vmin.s8 q5, q7, q5", q5, q7, i32, (1 << 31) + 1, q5, i32, (1 << 31) + 3);
    TESTINSN_bin("vmin.s16 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 3);
    TESTINSN_bin("vmin.s32 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 3);
    TESTINSN_bin("vmin.s8 q5, q7, q5", q5, q7, i32, (1 << 31) + 4, q5, i32, (1 << 31) + 2);
    TESTINSN_bin("vmin.s16 q0, q1, q2", q0, q1, i32, (1 << 31) + 4, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vmin.s32 q0, q1, q2", q0, q1, i32, (1 << 31) + 4, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vmin.s32 q10, q11, q12", q10, q11, i32, 24, q12, i32, 120);
    TESTINSN_bin("vmin.u32 q0, q1, q2", q0, q1, i32, 25, q2, i32, 120);
    TESTINSN_bin("vmin.u32 q0, q1, q2", q0, q1, i32, 250, q2, i32, 120);
    TESTINSN_bin("vmin.u32 q0, q1, q2", q0, q1, i32, 140, q2, i32, 120);
    TESTINSN_bin("vmin.u16 q0, q1, q2", q0, q1, i32, 120, q2, i32, 120);
    TESTINSN_bin("vmin.u8 q0, q1, q2", q0, q1, i32, 140, q2, i32, 140);
    TESTINSN_bin("vmin.u8 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vmin.u16 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vmin.u32 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vmin.u8 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 3);
    TESTINSN_bin("vmin.u16 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 3);
    TESTINSN_bin("vmin.u32 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 3);
    TESTINSN_bin("vmin.u8 q0, q1, q2", q0, q1, i32, (1 << 31) + 4, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vmin.u16 q0, q1, q2", q0, q1, i32, (1 << 31) + 4, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vmin.u32 q0, q1, q2", q0, q1, i32, (1 << 31) + 4, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vmin.u32 q10, q11, q12", q10, q11, i32, 24, q12, i32, 120);

    printf("---- VABD ----\n");
    TESTINSN_bin("vabd.s32 q0, q1, q2", q0, q1, i32, 25, q2, i32, 120);
    TESTINSN_bin("vabd.s32 q0, q1, q2", q0, q1, i32, 25, q2, i32, 121);
    TESTINSN_bin("vabd.s32 q0, q1, q2", q0, q1, i32, 140, q2, i32, -120);
    TESTINSN_bin("vabd.s16 q0, q1, q2", q0, q1, i32, 140, q2, i32, 120);
    TESTINSN_bin("vabd.s8 q0, q1, q2", q0, q1, i32, 140, q2, i32, 120);
    TESTINSN_bin("vabd.s8 q5, q7, q5", q5, q7, i32, (1 << 31) + 1, q5, i32, (1 << 31) + 2);
    TESTINSN_bin("vabd.s8 q5, q7, q5", q5, q7, i32, -255, q5, i32, (1 << 31) + 2);
    TESTINSN_bin("vabd.s8 q5, q7, q5", q5, q7, i32, (1 << 31) + 1, q5, i32, -200);
    TESTINSN_bin("vabd.s16 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vabd.s32 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vabd.s8 q5, q7, q5", q5, q7, i32, (1 << 31) + 1, q5, i32, (1 << 31) + 3);
    TESTINSN_bin("vabd.s16 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 3);
    TESTINSN_bin("vabd.s32 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 3);
    TESTINSN_bin("vabd.s8 q5, q7, q5", q5, q7, i32, (1 << 31) + 4, q5, i32, (1 << 31) + 2);
    TESTINSN_bin("vabd.s16 q0, q1, q2", q0, q1, i32, (1 << 31) + 4, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vabd.s32 q0, q1, q2", q0, q1, i32, (1 << 31) + 4, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vabd.s32 q10, q11, q12", q10, q11, i32, 24, q12, i32, 120);
    TESTINSN_bin("vabd.u32 q0, q1, q2", q0, q1, i32, 25, q2, i32, 120);
    TESTINSN_bin("vabd.u32 q0, q1, q2", q0, q1, i32, 140, q2, i32, 120);
    TESTINSN_bin("vabd.u16 q0, q1, q2", q0, q1, i32, -140, q2, i32, 120);
    TESTINSN_bin("vabd.u8 q0, q1, q2", q0, q1, i32, 140, q2, i32, 120);
    TESTINSN_bin("vabd.u8 q5, q7, q5", q5, q7, i32, -255, q5, i32, (1 << 31) + 2);
    TESTINSN_bin("vabd.u8 q5, q7, q5", q5, q7, i32, (1 << 31) + 1, q5, i32, -200);
    TESTINSN_bin("vabd.u8 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vabd.u16 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vabd.u32 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vabd.u8 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 3);
    TESTINSN_bin("vabd.u16 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 3);
    TESTINSN_bin("vabd.u32 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 3);
    TESTINSN_bin("vabd.u8 q0, q1, q2", q0, q1, i32, (1 << 31) + 4, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vabd.u16 q0, q1, q2", q0, q1, i32, (1 << 31) + 4, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vabd.u32 q0, q1, q2", q0, q1, i32, (1 << 31) + 4, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vabd.u32 q10, q11, q12", q10, q11, i32, 24, q12, i32, 120);

    printf("---- VABA ----\n");
    TESTINSN_bin("vaba.s32 q0, q1, q2", q0, q1, i32, 25, q2, i32, 120);
    TESTINSN_bin("vaba.s32 q0, q1, q2", q0, q1, i32, 25, q2, i32, 121);
    TESTINSN_bin("vaba.s32 q0, q1, q2", q0, q1, i32, 140, q2, i32, 120);
    TESTINSN_bin("vaba.s16 q0, q1, q2", q0, q1, i32, 140, q2, i32, 120);
    TESTINSN_bin("vaba.s8 q0, q1, q2", q0, q1, i32, 140, q2, i32, 120);
    TESTINSN_bin("vaba.s8 q5, q7, q5", q5, q7, i32, (1 << 31) + 1, q5, i32, (1 << 31) + 2);
    TESTINSN_bin("vaba.s8 q5, q7, q5", q5, q7, i32, -255, q5, i32, (1 << 31) + 2);
    TESTINSN_bin("vaba.s8 q5, q7, q5", q5, q7, i32, (1 << 31) + 1, q5, i32, -200);
    TESTINSN_bin("vaba.s16 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vaba.s32 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vaba.s8 q5, q7, q5", q5, q7, i32, (1 << 31) + 1, q5, i32, (1 << 31) + 3);
    TESTINSN_bin("vaba.s16 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 3);
    TESTINSN_bin("vaba.s32 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 3);
    TESTINSN_bin("vaba.s8 q5, q7, q5", q5, q7, i32, (1 << 31) + 4, q5, i32, (1 << 31) + 2);
    TESTINSN_bin("vaba.s16 q0, q1, q2", q0, q1, i32, (1 << 31) + 4, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vaba.s32 q0, q1, q2", q0, q1, i32, (1 << 31) + 4, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vaba.s32 q10, q11, q12", q10, q11, i32, 24, q12, i32, 120);
    TESTINSN_bin("vaba.u32 q0, q1, q2", q0, q1, i32, 25, q2, i32, 120);
    TESTINSN_bin("vaba.u32 q0, q1, q2", q0, q1, i32, 140, q2, i32, 120);
    TESTINSN_bin("vaba.u16 q0, q1, q2", q0, q1, i32, 140, q2, i32, 120);
    TESTINSN_bin("vaba.u8 q0, q1, q2", q0, q1, i32, 140, q2, i32, 120);
    TESTINSN_bin("vaba.u8 q5, q7, q5", q5, q7, i32, -255, q5, i32, (1 << 31) + 2);
    TESTINSN_bin("vaba.u8 q5, q7, q5", q5, q7, i32, (1 << 31) + 1, q5, i32, -200);
    TESTINSN_bin("vaba.u8 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vaba.u16 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vaba.u32 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vaba.u8 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 3);
    TESTINSN_bin("vaba.u16 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 3);
    TESTINSN_bin("vaba.u32 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 3);
    TESTINSN_bin("vaba.u8 q0, q1, q2", q0, q1, i32, (1 << 31) + 4, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vaba.u16 q0, q1, q2", q0, q1, i32, (1 << 31) + 4, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vaba.u32 q0, q1, q2", q0, q1, i32, (1 << 31) + 4, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vaba.u32 q10, q11, q12", q10, q11, i32, 24, q12, i32, 120);

    printf("---- VABAL ----\n");
    TESTINSN_bin("vabal.s32 q0, d1, d2", q0, d1, i32, 25, d2, i32, 120);
    TESTINSN_bin("vabal.s32 q0, d1, d2", q0, d1, i32, 25, d2, i32, 121);
    TESTINSN_bin("vabal.s32 q0, d1, d2", q0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin("vabal.s16 q0, d1, d2", q0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin("vabal.s8 q0, d1, d2", q0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin("vabal.s8 q5, d7, d5", q5, d7, i32, (1 << 31) + 1, d5, i32, (1 << 31) + 2);
    TESTINSN_bin("vabal.s8 q5, d7, d5", q5, d7, i32, -255, d5, i32, (1 << 31) + 2);
    TESTINSN_bin("vabal.s8 q5, d7, d5", q5, d7, i32, (1 << 31) + 1, d5, i32, -200);
    TESTINSN_bin("vabal.s16 q0, d1, d2", q0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vabal.s32 q0, d1, d2", q0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vabal.s8 q5, d7, d5", q5, d7, i32, (1 << 31) + 1, d5, i32, (1 << 31) + 3);
    TESTINSN_bin("vabal.s16 q0, d1, d2", q0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 3);
    TESTINSN_bin("vabal.s32 q0, d1, d2", q0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 3);
    TESTINSN_bin("vabal.s8 q5, d7, d5", q5, d7, i32, (1 << 31) + 4, d5, i32, (1 << 31) + 2);
    TESTINSN_bin("vabal.s16 q0, d1, d2", q0, d1, i32, (1 << 31) + 4, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vabal.s32 q0, d1, d2", q0, d1, i32, (1 << 31) + 4, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vabal.s32 q10, d31, d12", q10, d31, i32, 24, d12, i32, 120);
    TESTINSN_bin("vabal.u32 q0, d1, d2", q0, d1, i32, 25, d2, i32, 120);
    TESTINSN_bin("vabal.u32 q0, d1, d2", q0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin("vabal.u16 q0, d1, d2", q0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin("vabal.u8 q0, d1, d2", q0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin("vabal.u8 q5, d7, d5", q5, d7, i32, -255, d5, i32, (1 << 31) + 2);
    TESTINSN_bin("vabal.u8 q5, d7, d5", q5, d7, i32, (1 << 31) + 1, d5, i32, -200);
    TESTINSN_bin("vabal.u8 q0, d1, d2", q0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vabal.u16 q0, d1, d2", q0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vabal.u32 q0, d1, d2", q0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vabal.u8 q0, d1, d2", q0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 3);
    TESTINSN_bin("vabal.u16 q0, d1, d2", q0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 3);
    TESTINSN_bin("vabal.u32 q0, d1, d2", q0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 3);
    TESTINSN_bin("vabal.u8 q0, d1, d2", q0, d1, i32, (1 << 31) + 4, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vabal.u16 q0, d1, d2", q0, d1, i32, (1 << 31) + 4, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vabal.u32 q0, d1, d2", q0, d1, i32, (1 << 31) + 4, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vabal.u32 q10, d11, d12", q10, d11, i32, 24, d12, i32, 120);

    printf("---- VABDL ----\n");
    TESTINSN_bin("vabdl.s32 q0, d1, d2", q0, d1, i32, 25, d2, i32, 120);
    TESTINSN_bin("vabdl.s32 q0, d1, d2", q0, d1, i32, 25, d2, i32, 121);
    TESTINSN_bin("vabdl.s32 q0, d1, d2", q0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin("vabdl.s16 q0, d1, d2", q0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin("vabdl.s8 q0, d1, d2", q0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin("vabdl.s8 q5, d7, d5", q5, d7, i32, (1 << 31) + 1, d5, i32, (1 << 31) + 2);
    TESTINSN_bin("vabdl.s8 q5, d7, d5", q5, d7, i32, -255, d5, i32, (1 << 31) + 2);
    TESTINSN_bin("vabdl.s8 q5, d7, d5", q5, d7, i32, (1 << 31) + 1, d5, i32, -200);
    TESTINSN_bin("vabdl.s16 q0, d1, d2", q0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vabdl.s32 q0, d1, d2", q0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vabdl.s8 q5, d7, d5", q5, d7, i32, (1 << 31) + 1, d5, i32, (1 << 31) + 3);
    TESTINSN_bin("vabdl.s16 q0, d1, d2", q0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 3);
    TESTINSN_bin("vabdl.s32 q0, d1, d2", q0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 3);
    TESTINSN_bin("vabdl.s8 q5, d7, d5", q5, d7, i32, (1 << 31) + 4, d5, i32, (1 << 31) + 2);
    TESTINSN_bin("vabdl.s16 q0, d1, d2", q0, d1, i32, (1 << 31) + 4, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vabdl.s32 q0, d1, d2", q0, d1, i32, (1 << 31) + 4, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vabdl.s32 q10, d31, d12", q10, d31, i32, 24, d12, i32, 120);
    TESTINSN_bin("vabdl.u32 q0, d1, d2", q0, d1, i32, 25, d2, i32, 120);
    TESTINSN_bin("vabdl.u32 q0, d1, d2", q0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin("vabdl.u16 q0, d1, d2", q0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin("vabdl.u8 q0, d1, d2", q0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin("vabdl.u8 q5, d7, d5", q5, d7, i32, -255, d5, i32, (1 << 31) + 2);
    TESTINSN_bin("vabdl.u8 q5, d7, d5", q5, d7, i32, (1 << 31) + 1, d5, i32, -200);
    TESTINSN_bin("vabdl.u8 q0, d1, d2", q0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vabdl.u16 q0, d1, d2", q0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vabdl.u32 q0, d1, d2", q0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vabdl.u8 q0, d1, d2", q0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 3);
    TESTINSN_bin("vabdl.u16 q0, d1, d2", q0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 3);
    TESTINSN_bin("vabdl.u32 q0, d1, d2", q0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 3);
    TESTINSN_bin("vabdl.u8 q0, d1, d2", q0, d1, i32, (1 << 31) + 4, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vabdl.u16 q0, d1, d2", q0, d1, i32, (1 << 31) + 4, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vabdl.u32 q0, d1, d2", q0, d1, i32, (1 << 31) + 4, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vabdl.u32 q10, d11, d12", q10, d11, i32, 24, d12, i32, 120);

    printf("---- VTST ----\n");
    TESTINSN_bin("vtst.32 q0, q1, q2", q0, q1, i32, 24, q2, i32, 120);
    TESTINSN_bin("vtst.32 q3, q4, q5", q3, q4, i32, 140, q5, i32, 120);
    TESTINSN_bin("vtst.16 q6, q7, q8", q6, q7, i32, 120, q8, i32, 120);
    TESTINSN_bin("vtst.8 q9, q10, q12", q9, q10, i32, 140, q12, i32, 120);
    TESTINSN_bin("vtst.8 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vtst.16 q0, q1, q2", q0, q1, i32, (1 << 14) + 1, q2, i32, (1 << 14) + 1);
    TESTINSN_bin("vtst.32 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vtst.8 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, 2);
    TESTINSN_bin("vtst.16 q0, q1, q2", q0, q1, i32, (1 << 14) + 1, q2, i32, (1 << 14) + 1);
    TESTINSN_bin("vtst.32 q0, q1, q2", q0, q1, i32, 1, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vtst.32 q10, q11, q12", q10, q11, i32, 24, q12, i32, 120);

    printf("---- VCEQ ----\n");
    TESTINSN_bin("vceq.i32 q0, q1, q2", q0, q1, i32, 24, q2, i32, 120);
    TESTINSN_bin("vceq.i32 q3, q4, q5", q3, q4, i32, 140, q5, i32, 120);
    TESTINSN_bin("vceq.i16 q6, q7, q8", q6, q7, i32, 120, q8, i32, 120);
    TESTINSN_bin("vceq.i8 q9, q10, q12", q9, q10, i32, 140, q12, i32, 120);
    TESTINSN_bin("vceq.i8 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vceq.i16 q0, q1, q2", q0, q1, i32, (1 << 14) + 1, q2, i32, (1 << 14) + 1);
    TESTINSN_bin("vceq.i32 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vceq.i8 q0, q1, q2", q0, q1, i32, (1 << 31) + 1, q2, i32, 2);
    TESTINSN_bin("vceq.i16 q0, q1, q2", q0, q1, i32, 1, q2, i32, (1 << 14) + 1);
    TESTINSN_bin("vceq.i32 q0, q1, q2", q0, q1, i32, 1, q2, i32, (1 << 31) + 2);
    TESTINSN_bin("vceq.i32 q10, q11, q12", q10, q11, i32, 24, q12, i32, 120);

    printf("---- VMLA ----\n");
    TESTINSN_bin("vmla.i32 q0, q1, q2", q0, q1, i32, -24, q2, i32, 120);
    TESTINSN_bin("vmla.i32 q6, q7, q8", q6, q7, i32, 140, q8, i32, 120);
    TESTINSN_bin("vmla.i16 q9, q11, q12", q9, q11, i32, 0x140, q12, i32, 0x120);
    TESTINSN_bin("vmla.i16 q7, q1, q2", q7, q1, i32, 0x140, q2, i32, 0x120);
    TESTINSN_bin("vmla.i8 q0, q1, q2", q0, q1, i32, 140, q2, i32, -120);
    TESTINSN_bin("vmla.i8 q10, q11, q12", q10, q11, i32, (1 << 5) + 1, q12, i32, (1 << 3) + 2);
    TESTINSN_bin("vmla.i16 q4, q5, q6", q4, q5, i32, (1 << 14) + 1, q6, i32, (1 << 13) + 2);
    TESTINSN_bin("vmla.i16 q14, q5, q9", q14, q5, i32, (1 << 14) + 1, q9, i32, (1 << 13) + 2);
    TESTINSN_bin("vmla.i32 q7, q8, q9", q7, q8, i32, (1 << 31) + 1, q9, i32, (1 << 31) + 2);
    TESTINSN_bin("vmla.i8 q10, q13, q12", q10, q13, i32, (1 << 5) + 1, q12, i32, (1 << 3) + 2);
    TESTINSN_bin("vmla.i16 q4, q5, q6", q4, q5, i32, (1 << 28) + 0xfe, q6, i32, (1 << 13) + 2);
    TESTINSN_bin("vmla.i32 q7, q8, q9", q7, q8, i32, (1 << 31) + 1, q9, i32, (1 << 31) + 2);
    TESTINSN_bin("vmla.i32 q10, q11, q15", q10, q11, i32, 24, q15, i32, -120);

    printf("---- VMLS ----\n");
    TESTINSN_bin("vmls.i32 q0, q1, q2", q0, q1, i32, -24, q2, i32, 120);
    TESTINSN_bin("vmls.i32 q6, q7, q8", q6, q7, i32, 140, q8, i32, -120);
    TESTINSN_bin("vmls.i16 q9, q11, q12", q9, q11, i32, 0x140, q12, i32, 0x120);
    TESTINSN_bin("vmls.i8 q0, q1, q2", q0, q1, i32, 140, q2, i32, 120);
    TESTINSN_bin("vmls.i8 q10, q11, q12", q10, q11, i32, (1 << 5) + 1, q12, i32, (1 << 3) + 2);
    TESTINSN_bin("vmls.i16 q4, q5, q6", q4, q5, i32, (1 << 14) + 1, q6, i32, (1 << 13) + 2);
    TESTINSN_bin("vmls.i32 q7, q8, q9", q7, q8, i32, (1 << 31) + 1, q9, i32, (1 << 31) + 2);
    TESTINSN_bin("vmls.i8 q10, q13, q12", q10, q13, i32, (1 << 5) + 1, q12, i32, (1 << 3) + 2);
    TESTINSN_bin("vmls.i16 q4, q5, q6", q4, q5, i32, (1 << 28) + 0xfe, q6, i32, (1 << 13) + 2);
    TESTINSN_bin("vmls.i32 q7, q8, q9", q7, q8, i32, (1 << 31) + 1, q9, i32, (1 << 31) + 2);
    TESTINSN_bin("vmls.i32 q10, q11, q15", q10, q11, i32, -24, q15, i32, 120);

    printf("---- VMUL ----\n");
    TESTINSN_bin("vmul.i32 q0, q1, q2", q0, q1, i32, 24, q2, i32, 120);
    TESTINSN_bin("vmul.i32 q6, q7, q8", q6, q7, i32, 140, q8, i32, -120);
    TESTINSN_bin("vmul.i16 q9, q11, q12", q9, q11, i32, 0x140, q12, i32, 0x120);
    TESTINSN_bin("vmul.i8 q0, q1, q2", q0, q1, i32, 140, q2, i32, 120);
    TESTINSN_bin("vmul.i8 q10, q11, q12", q10, q11, i32, (1 << 5) + 1, q12, i32, (1 << 3) + 2);
    TESTINSN_bin("vmul.i16 q4, q5, q6", q4, q5, i32, (1 << 14) + 1, q6, i32, (1 << 13) + 2);
    TESTINSN_bin("vmul.i32 q7, q8, q9", q7, q8, i32, (1 << 31) + 1, q9, i32, (1 << 31) + 2);
    TESTINSN_bin("vmul.i8 q10, q11, q12", q10, q11, i32, (1 << 25) + 0xfeb2, q12, i32, (1 << 13) + 0xdf);
    TESTINSN_bin("vmul.i16 q4, q5, q6", q4, q5, i32, (1 << 14) - 0xabcd, q6, i32, (1 << 13) + 2);
    TESTINSN_bin("vmul.i32 q7, q8, q9", q7, q8, i32, (1 << 31), q9, i32, 12);
    TESTINSN_bin("vmul.i8 q10, q13, q12", q10, q13, i32, (1 << 5) + 1, q12, i32, (1 << 3) + 2);
    TESTINSN_bin("vmul.i16 q4, q5, q6", q4, q5, i32, (1 << 28) + 0xfe, q6, i32, (1 << 13) + 2);
    TESTINSN_bin("vmul.i32 q7, q8, q9", q7, q8, i32, (1 << 31) + 1, q9, i32, (1 << 31) + 2);
    TESTINSN_bin("vmul.i32 q10, q11, q15", q10, q11, i32, 24, q15, i32, 120);
    TESTINSN_bin("vmul.p8 q0, q1, q2", q0, q1, i32, 3, q2, i32, 3);
    TESTINSN_bin("vmul.p8 q0, q1, q2", q0, q1, i32, 12, q2, i8, 0x0f);

    printf("---- VMUL (by scalar) ----\n");
    TESTINSN_bin("vmul.i32 q0, q1, d4[0]", q0, q1, i32, 24, d4, i32, 120);
    TESTINSN_bin("vmul.i32 q15, q8, d7[1]", q15, q8, i32, 140, d4, i32, -120);
    TESTINSN_bin("vmul.i16 q10, q9, d7[3]", q10, q9, i32, 0x140, d7, i32, 0x120);
    TESTINSN_bin("vmul.i16 q4, q5, d6[2]", q4, q5, i32, (1 << 14) + 1, d6, i32, (1 << 13) + 2);
    TESTINSN_bin("vmul.i32 q4, q8, d15[1]", q4, q8, i32, (1 << 31) + 1, d15, i32, (1 << 31) + 2);
    TESTINSN_bin("vmul.i16 q4, q5, d6[0]", q4, q5, i32, (1 << 14) - 0xabcd, d6, i32, (1 << 13) + 2);
    TESTINSN_bin("vmul.i32 q7, q8, d1[1]", q7, q8, i32, (1 << 31), d1, i16, 12);
    TESTINSN_bin("vmul.i16 q4, q5, d6[0]", q4, q5, i32, (1 << 28) + 0xfe, d6, i32, (1 << 13) + 2);
    TESTINSN_bin("vmul.i32 q7, q8, d1[1]", q7, q8, i32, (1 << 31) + 1, d1, i32, (1 << 31) + 2);

    printf("---- VMLA (by scalar) ----\n");
    TESTINSN_bin("vmla.i32 q0, q1, d4[0]", q0, q1, i32, 24, d4, i32, 120);
    TESTINSN_bin("vmla.i32 q15, q8, d7[1]", q15, q8, i32, 140, d7, i32, -120);
    TESTINSN_bin("vmla.i16 q10, q9, d7[3]", q10, q9, i32, 0x140, d7, i32, 0x120);
    TESTINSN_bin("vmla.i16 q4, q5, d6[2]", q4, q5, i32, (1 << 14) + 1, d6, i32, (1 << 13) + 2);
    TESTINSN_bin("vmla.i32 q4, q8, d15[1]", q4, q8, i32, (1 << 31) + 1, d15, i32, (1 << 31) + 2);
    TESTINSN_bin("vmla.i16 q4, q5, d6[0]", q4, q5, i32, (1 << 14) - 0xabcd, d6, i32, (1 << 13) + 2);
    TESTINSN_bin("vmla.i32 q7, q8, d1[1]", q7, q8, i32, (1 << 31), d1, i16, 12);
    TESTINSN_bin("vmla.i16 q4, q5, d6[0]", q4, q5, i32, (1 << 28) + 0xfe, d6, i32, (1 << 13) + 2);
    TESTINSN_bin("vmla.i32 q7, q8, d1[1]", q7, q8, i32, (1 << 31) + 1, d1, i32, (1 << 31) + 2);

    printf("---- VMLS (by scalar) ----\n");
    TESTINSN_bin("vmls.i32 q0, q1, d4[0]", q0, q1, i32, 24, d4, i32, 120);
    TESTINSN_bin("vmls.i32 q15, q8, d7[1]", q15, q8, i32, 140, d7, i32, -120);
    TESTINSN_bin("vmls.i16 q10, q9, d7[3]", q10, q9, i32, 0x140, d7, i32, 0x120);
    TESTINSN_bin("vmls.i16 q4, q5, d6[2]", q4, q5, i32, (1 << 14) + 1, d6, i32, (1 << 13) + 2);
    TESTINSN_bin("vmls.i32 q4, q8, d15[1]", q4, q8, i32, (1 << 31) + 1, d15, i32, (1 << 31) + 2);
    TESTINSN_bin("vmls.i16 q4, q5, d6[0]", q4, q5, i32, (1 << 14) - 0xabcd, d6, i32, (1 << 13) + 2);
    TESTINSN_bin("vmls.i32 q7, q8, d1[1]", q7, q8, i32, (1 << 31), d1, i16, 12);
    TESTINSN_bin("vmls.i16 q4, q5, d6[0]", q4, q5, i32, (1 << 28) + 0xfe, d6, i32, (1 << 13) + 2);
    TESTINSN_bin("vmls.i32 q7, q8, d1[1]", q7, q8, i32, (1 << 31) + 1, d1, i32, (1 << 31) + 2);

    printf("---- VMULL (by scalar) ----\n");
    TESTINSN_bin("vmull.s32 q0, d2, d4[0]", q0, d2, i32, 24, d4, i32, 120);
    TESTINSN_bin("vmull.s32 q15, d8, d7[1]", q15, d8, i32, 140, d7, i32, -120);
    TESTINSN_bin("vmull.s16 q10, d31, d7[3]", q10, d31, i32, 0x140, d7, i32, 0x120);
    TESTINSN_bin("vmull.s16 q4, d5, d6[2]", q4, d5, i32, (1 << 14) + 1, d6, i32, (1 << 13) + 2);
    TESTINSN_bin("vmull.s32 q4, d7, d15[1]", q4, d7, i32, (1 << 31) + 1, d15, i32, (1 << 31) + 2);
    TESTINSN_bin("vmull.s16 q4, d5, d6[0]", q4, d5, i32, (1 << 14) - 0xabcd, d6, i32, (1 << 13) + 2);
    TESTINSN_bin("vmull.s32 q7, d7, d1[1]", q7, d7, i32, (1 << 31), d1, i16, 12);
    TESTINSN_bin("vmull.s16 q4, d5, d6[0]", q4, d5, i32, (1 << 28) + 0xfe, d6, i32, (1 << 13) + 2);
    TESTINSN_bin("vmull.s32 q7, d7, d1[1]", q7, d7, i32, (1 << 31) + 1, d1, i32, (1 << 31) + 2);
    TESTINSN_bin("vmull.u32 q0, d1, d4[0]", q0, d1, i32, 24, d4, i32, 120);
    TESTINSN_bin("vmull.u32 q15, d8, d7[1]", q15, d8, i32, 140, d4, i32, -120);
    TESTINSN_bin("vmull.u16 q10, d31, d7[3]", q10, d31, i32, 0x140, d7, i32, 0x120);
    TESTINSN_bin("vmull.u16 q4, d5, d6[2]", q4, d5, i32, (1 << 14) + 1, d6, i32, (1 << 13) + 2);
    TESTINSN_bin("vmull.u32 q4, d7, d15[1]", q4, d7, i32, (1 << 31) + 1, d15, i32, (1 << 31) + 2);
    TESTINSN_bin("vmull.u16 q4, d5, d6[0]", q4, d5, i32, (1 << 14) - 0xabcd, d6, i32, (1 << 13) + 2);
    TESTINSN_bin("vmull.u32 q7, d7, d1[1]", q7, d7, i32, (1 << 31), d1, i16, 12);
    TESTINSN_bin("vmull.u16 q4, d5, d6[0]", q4, d5, i32, (1 << 28) + 0xfe, d6, i32, (1 << 13) + 2);
    TESTINSN_bin("vmull.u32 q7, d7, d1[1]", q7, d7, i32, (1 << 31) + 1, d1, i32, (1 << 31) + 2);

    printf("---- VMLAL (by scalar) ----\n");
    TESTINSN_bin("vmlal.s32 q0, d2, d4[0]", q0, d2, i32, 24, d4, i32, 120);
    TESTINSN_bin("vmlal.s32 q15, d8, d7[1]", q15, d8, i32, 140, d7, i32, -120);
    TESTINSN_bin("vmlal.s16 q10, d31, d7[3]", q10, d31, i32, 0x140, d7, i32, 0x120);
    TESTINSN_bin("vmlal.s16 q4, d5, d6[2]", q4, d5, i32, (1 << 14) + 1, d6, i32, (1 << 13) + 2);
    TESTINSN_bin("vmlal.s32 q4, d7, d15[1]", q4, d7, i32, (1 << 31) + 1, d15, i32, (1 << 31) + 2);
    TESTINSN_bin("vmlal.s16 q4, d5, d6[0]", q4, d5, i32, (1 << 14) - 0xabcd, d6, i32, (1 << 13) + 2);
    TESTINSN_bin("vmlal.s32 q7, d7, d1[1]", q7, d7, i32, (1 << 31), d1, i16, 12);
    TESTINSN_bin("vmlal.s16 q4, d5, d6[0]", q4, d5, i32, (1 << 28) + 0xfe, d6, i32, (1 << 13) + 2);
    TESTINSN_bin("vmlal.s32 q7, d7, d1[1]", q7, d7, i32, (1 << 31) + 1, d1, i32, (1 << 31) + 2);
    TESTINSN_bin("vmlal.u32 q0, d1, d4[0]", q0, d1, i32, 24, d4, i32, 120);
    TESTINSN_bin("vmlal.u32 q15, d8, d7[1]", q15, d8, i32, 140, d4, i32, -120);
    TESTINSN_bin("vmlal.u16 q10, d31, d7[3]", q10, d31, i32, 0x140, d7, i32, 0x120);
    TESTINSN_bin("vmlal.u16 q4, d5, d6[2]", q4, d5, i32, (1 << 14) + 1, d6, i32, (1 << 13) + 2);
    TESTINSN_bin("vmlal.u32 q4, d7, d15[1]", q4, d7, i32, (1 << 31) + 1, d15, i32, (1 << 31) + 2);
    TESTINSN_bin("vmlal.u16 q4, d5, d6[0]", q4, d5, i32, (1 << 14) - 0xabcd, d6, i32, (1 << 13) + 2);
    TESTINSN_bin("vmlal.u32 q7, d7, d1[1]", q7, d7, i32, (1 << 31), d1, i16, 12);
    TESTINSN_bin("vmlal.u16 q4, d5, d6[0]", q4, d5, i32, (1 << 28) + 0xfe, d6, i32, (1 << 13) + 2);
    TESTINSN_bin("vmlal.u32 q7, d7, d1[1]", q7, d7, i32, (1 << 31) + 1, d1, i32, (1 << 31) + 2);

    printf("---- VMLSL (by scalar) ----\n");
    TESTINSN_bin("vmlsl.s32 q0, d2, d4[0]", q0, d2, i32, 24, d4, i32, 120);
    TESTINSN_bin("vmlsl.s32 q15, d8, d7[1]", q15, d8, i32, 140, d7, i32, -120);
    TESTINSN_bin("vmlsl.s16 q10, d31, d7[3]", q10, d31, i32, 0x140, d7, i32, 0x120);
    TESTINSN_bin("vmlsl.s16 q4, d5, d6[2]", q4, d5, i32, (1 << 14) + 1, d6, i32, (1 << 13) + 2);
    TESTINSN_bin("vmlsl.s32 q4, d7, d15[1]", q4, d7, i32, (1 << 31) + 1, d15, i32, (1 << 31) + 2);
    TESTINSN_bin("vmlsl.s16 q4, d5, d6[0]", q4, d5, i32, (1 << 14) - 0xabcd, d6, i32, (1 << 13) + 2);
    TESTINSN_bin("vmlsl.s32 q7, d7, d1[1]", q7, d7, i32, (1 << 31), d1, i16, 12);
    TESTINSN_bin("vmlsl.s16 q4, d5, d6[0]", q4, d5, i32, (1 << 28) + 0xfe, d6, i32, (1 << 13) + 2);
    TESTINSN_bin("vmlsl.s32 q7, d7, d1[1]", q7, d7, i32, (1 << 31) + 1, d1, i32, (1 << 31) + 2);
    TESTINSN_bin("vmlsl.u32 q0, d1, d4[0]", q0, d1, i32, 24, d4, i32, 120);
    TESTINSN_bin("vmlsl.u32 q15, d8, d7[1]", q15, d8, i32, 140, d4, i32, -120);
    TESTINSN_bin("vmlsl.u16 q10, d31, d7[3]", q10, d31, i32, 0x140, d7, i32, 0x120);
    TESTINSN_bin("vmlsl.u16 q4, d5, d6[2]", q4, d5, i32, (1 << 14) + 1, d6, i32, (1 << 13) + 2);
    TESTINSN_bin("vmlsl.u32 q4, d7, d15[1]", q4, d7, i32, (1 << 31) + 1, d15, i32, (1 << 31) + 2);
    TESTINSN_bin("vmlsl.u16 q4, d5, d6[0]", q4, d5, i32, (1 << 14) - 0xabcd, d6, i32, (1 << 13) + 2);
    TESTINSN_bin("vmlsl.u32 q7, d7, d1[1]", q7, d7, i32, (1 << 31), d1, i16, 12);
    TESTINSN_bin("vmlsl.u16 q4, d5, d6[0]", q4, d5, i32, (1 << 28) + 0xfe, d6, i32, (1 << 13) + 2);
    TESTINSN_bin("vmlsl.u32 q7, d7, d1[1]", q7, d7, i32, (1 << 31) + 1, d1, i32, (1 << 31) + 2);

    printf("---- VRSHR ----\n");
    TESTINSN_un("vrshr.s8 q0, q1, #0", q0, q1, i32, -1);
    TESTINSN_un("vrshr.s8 q0, q1, #1", q0, q1, i32, -1);
    TESTINSN_un("vrshr.s16 q3, q4, #2", q3, q4, i32, -0x7c);
    TESTINSN_un("vrshr.s32 q2, q5, #31", q2, q5, i32, -1);
    TESTINSN_un("vrshr.s8 q6, q7, #7", q6, q7, i32, 0xffff);
    TESTINSN_un("vrshr.s16 q8, q9, #12", q8, q9, i32, -10);
    TESTINSN_un("vrshr.s32 q10, q11, #5", q10, q11, i32, 10234);
    TESTINSN_un("vrshr.u8 q12, q13, #1", q12, q13, i32, -1);
    TESTINSN_un("vrshr.u16 q14, q15, #11", q14, q15, i32, -1);
    TESTINSN_un("vrshr.u32 q10, q11, #9", q10, q11, i32, 1000);
    TESTINSN_un("vrshr.u8 q7, q13, #7", q7, q13, i32, -1);
    TESTINSN_un("vrshr.u16 q8, q1, #5", q8, q1, i32, 0xabcf);
    TESTINSN_un("vrshr.u32 q12, q3, #15", q12, q3, i32, -0x1b0);
    TESTINSN_un("vrshr.u64 q0, q1, #42", q0, q1, i32, -1);
    TESTINSN_un("vrshr.s64 q6, q7, #12", q6, q7, i32, 0xfac);
    TESTINSN_un("vrshr.u64 q8, q4, #9", q8, q4, i32, 13560);
    TESTINSN_un("vrshr.s64 q9, q12, #11", q9, q12, i32, 98710);

    printf("---- VRSRA ----\n");
    TESTINSN_un("vrsra.s8 q0, q1, #1", q0, q1, i32, -1);
    TESTINSN_un("vrsra.s16 q3, q4, #2", q3, q4, i32, -0x7c);
    TESTINSN_un("vrsra.s32 q2, q5, #31", q2, q5, i32, -1);
    TESTINSN_un("vrsra.s8 q6, q7, #7", q6, q7, i32, 0xffff);
    TESTINSN_un("vrsra.s16 q8, q9, #12", q8, q9, i32, -10);
    TESTINSN_un("vrsra.s32 q10, q11, #5", q10, q11, i32, 10234);
    TESTINSN_un("vrsra.u8 q12, q13, #1", q12, q13, i32, -1);
    TESTINSN_un("vrsra.u16 q14, q15, #11", q14, q15, i32, -1);
    TESTINSN_un("vrsra.u32 q10, q11, #9", q10, q11, i32, 1000);
    TESTINSN_un("vrsra.u8 q7, q13, #7", q7, q13, i32, -1);
    TESTINSN_un("vrsra.u16 q8, q1, #5", q8, q1, i32, 0xabcf);
    TESTINSN_un("vrsra.u32 q12, q3, #15", q12, q3, i32, -0x1b0);
    TESTINSN_un("vrsra.u64 q0, q1, #42", q0, q1, i32, -1);
    TESTINSN_un("vrsra.s64 q6, q7, #12", q6, q7, i32, 0xfac);
    TESTINSN_un("vrsra.u64 q8, q4, #9", q8, q4, i32, 13560);
    TESTINSN_un("vrsra.s64 q9, q12, #11", q9, q12, i32, 98710);

    printf("---- VSHR ----\n");
    TESTINSN_un("vshr.s8 q0, q1, #0", q0, q1, i32, -1);
    TESTINSN_un("vshr.s8 q0, q1, #1", q0, q1, i32, -1);
    TESTINSN_un("vshr.s16 q3, q4, #2", q3, q4, i32, -0x7c);
    TESTINSN_un("vshr.s32 q2, q5, #31", q2, q5, i32, -1);
    TESTINSN_un("vshr.s8 q6, q7, #7", q6, q7, i32, 0xffff);
    TESTINSN_un("vshr.s16 q8, q9, #12", q8, q9, i32, -10);
    TESTINSN_un("vshr.s32 q10, q11, #5", q10, q11, i32, 10234);
    TESTINSN_un("vshr.u8 q12, q13, #1", q12, q13, i32, -1);
    TESTINSN_un("vshr.u16 q14, q15, #11", q14, q15, i32, -1);
    TESTINSN_un("vshr.u32 q10, q11, #9", q10, q11, i32, 1000);
    TESTINSN_un("vshr.u8 q7, q13, #7", q7, q13, i32, -1);
    TESTINSN_un("vshr.u16 q8, q1, #5", q8, q1, i32, 0xabcf);
    TESTINSN_un("vshr.u32 q12, q3, #15", q12, q3, i32, -0x1b0);
    TESTINSN_un("vshr.u64 q0, q1, #42", q0, q1, i32, -1);
    TESTINSN_un("vshr.s64 q6, q7, #12", q6, q7, i32, 0xfac);
    TESTINSN_un("vshr.u64 q8, q4, #9", q8, q4, i32, 13560);
    TESTINSN_un("vshr.s64 q9, q12, #11", q9, q12, i32, 98710);

    printf("---- VSRA ----\n");
    TESTINSN_un("vsra.s8 q0, q1, #1", q0, q1, i32, -1);
    TESTINSN_un("vsra.s16 q3, q4, #2", q3, q4, i32, -0x7c);
    TESTINSN_un("vsra.s32 q2, q5, #31", q2, q5, i32, -1);
    TESTINSN_un("vsra.s8 q6, q7, #7", q6, q7, i32, 0xffff);
    TESTINSN_un("vsra.s16 q8, q9, #12", q8, q9, i32, -10);
    TESTINSN_un("vsra.s32 q10, q11, #5", q10, q11, i32, 10234);
    TESTINSN_un("vsra.u8 q12, q13, #1", q12, q13, i32, -1);
    TESTINSN_un("vsra.u16 q14, q15, #11", q14, q15, i32, -1);
    TESTINSN_un("vsra.u32 q10, q11, #9", q10, q11, i32, 1000);
    TESTINSN_un("vsra.u8 q7, q13, #7", q7, q13, i32, -1);
    TESTINSN_un("vsra.u16 q8, q1, #5", q8, q1, i32, 0xabcf);
    TESTINSN_un("vsra.u32 q12, q3, #15", q12, q3, i32, -0x1b0);
    TESTINSN_un("vsra.u64 q0, q1, #42", q0, q1, i32, -1);
    TESTINSN_un("vsra.s64 q6, q7, #12", q6, q7, i32, 0xfac);
    TESTINSN_un("vsra.u64 q8, q4, #9", q8, q4, i32, 13560);
    TESTINSN_un("vsra.s64 q9, q12, #11", q9, q12, i32, 98710);

    printf("---- VSRI ----\n");
    TESTINSN_un("vsri.16 q0, q1, #1", q0, q1, i32, -1);
    TESTINSN_un("vsri.16 q3, q4, #2", q3, q4, i32, -0x7c);
    TESTINSN_un("vsri.32 q2, q5, #31", q2, q5, i32, -1);
    TESTINSN_un("vsri.8 q6, q7, #7", q6, q7, i32, 0xffff);
    TESTINSN_un("vsri.16 q8, q9, #12", q8, q9, i32, -10);
    TESTINSN_un("vsri.32 q10, q11, #5", q10, q11, i32, 10234);
    TESTINSN_un("vsri.8 q12, q13, #1", q12, q13, i32, -1);
    TESTINSN_un("vsri.16 q14, q15, #11", q14, q15, i32, -1);
    TESTINSN_un("vsri.32 q10, q11, #9", q10, q11, i32, 1000);
    TESTINSN_un("vsri.8 q7, q13, #7", q7, q13, i32, -1);
    TESTINSN_un("vsri.16 q8, q1, #5", q8, q1, i32, 0xabcf);
    TESTINSN_un("vsri.32 q12, q3, #15", q12, q3, i32, -0x1b0);
    TESTINSN_un("vsri.64 q0, q1, #42", q0, q1, i32, -1);
    TESTINSN_un("vsri.64 q6, q7, #12", q6, q7, i32, 0xfac);
    TESTINSN_un("vsri.64 q8, q4, #9", q8, q4, i32, 13560);
    TESTINSN_un("vsri.64 q9, q12, #11", q9, q12, i32, 98710);

    printf("---- VMOVL ----\n");
    TESTINSN_un("vmovl.u32 q0, d2", q0, d2, i32, 0x42);
    TESTINSN_un("vmovl.u16 q15, d2", q15, d2, i32, 0x42);
    TESTINSN_un("vmovl.u8 q3, d31", q0, d31, i32, 0x42);
    TESTINSN_un("vmovl.s32 q0, d2", q0, d2, i32, 0x42);
    TESTINSN_un("vmovl.s16 q15, d2", q15, d2, i32, 0x42);
    TESTINSN_un("vmovl.s8 q3, d31", q0, d31, i32, 0x42);
    TESTINSN_un("vmovl.u32 q0, d2", q0, d2, i8, 0xed);
    TESTINSN_un("vmovl.u16 q15, d2", q15, d2, i8, 0xed);
    TESTINSN_un("vmovl.u8 q3, d31", q0, d31, i8, 0xed);
    TESTINSN_un("vmovl.s32 q0, d2", q0, d2, i8, 0xed);
    TESTINSN_un("vmovl.s16 q15, d2", q15, d2, i8, 0xed);
    TESTINSN_un("vmovl.s8 q3, d31", q0, d31, i8, 0xed);

    printf("---- VABS ----\n");
    TESTINSN_un("vabs.s32 q0, q1", q0, q1, i32, 0x73);
    TESTINSN_un("vabs.s16 q15, q4", q15, q4, i32, 0x73);
    TESTINSN_un("vabs.s8 q8, q7", q8, q7, i32, 0x73);
    TESTINSN_un("vabs.s32 q0, q1", q0, q1, i32, 0xfe);
    TESTINSN_un("vabs.s16 q15, q4", q15, q4, i32, 0xef);
    TESTINSN_un("vabs.s8 q8, q7", q8, q7, i32, 0xde);
    TESTINSN_un("vabs.s32 q0, q1", q0, q1, i16, 0xfe0a);
    TESTINSN_un("vabs.s16 q15, q4", q15, q4, i16, 0xef0b);
    TESTINSN_un("vabs.s8 q8, q7", q8, q7, i16, 0xde0c);

    printf("---- VQABS ----\n");
    TESTINSN_un_q("vqabs.s32 q0, q1", q0, q1, i32, 0x73);
    TESTINSN_un_q("vqabs.s32 q0, q1", q0, q1, i32, 1 << 31);
    TESTINSN_un_q("vqabs.s16 q0, q1", q0, q1, i32, 1 << 31);
    TESTINSN_un_q("vqabs.s8 q0, q1", q0, q1, i32, 1 << 31);
    TESTINSN_un_q("vqabs.s16 q15, q4", q15, q4, i32, 0x73);
    TESTINSN_un_q("vqabs.s8 q8, q7", q8, q7, i32, 0x73);
    TESTINSN_un_q("vqabs.s32 q0, q1", q0, q1, i32, 0xfe);
    TESTINSN_un_q("vqabs.s16 q15, q4", q15, q4, i32, 0xef);
    TESTINSN_un_q("vqabs.s8 q8, q7", q8, q7, i32, 0xde);
    TESTINSN_un_q("vqabs.s32 q0, q1", q0, q1, i16, 0xfe0a);
    TESTINSN_un_q("vqabs.s16 q15, q4", q15, q4, i16, 0xef0b);
    TESTINSN_un_q("vqabs.s8 q8, q7", q8, q7, i16, 0xde0c);

    printf("---- VADDW ----\n");
    TESTINSN_bin("vaddw.s32 q0, q1, d4", q0, q1, i32, 0x73, d4, i8, 0x12);
    TESTINSN_bin("vaddw.s16 q15, q14, d4", q15, q14, i32, 0x73, d4, i8, 0x12);
    TESTINSN_bin("vaddw.s8 q0, q1, d31", q0, q1, i32, 0x73, d31, i8, 0x12);
    TESTINSN_bin("vaddw.u32 q0, q1, d4", q0, q1, i32, 0x73, d4, i8, 0x12);
    TESTINSN_bin("vaddw.u16 q0, q1, d4", q0, q1, i32, 0x73, d4, i8, 0x12);
    TESTINSN_bin("vaddw.u8 q0, q1, d4", q0, q1, i32, 0x73, d4, i8, 0x12);
    TESTINSN_bin("vaddw.s32 q0, q1, d4", q0, q1, i32, 0x73, d4, i8, 0xe2);
    TESTINSN_bin("vaddw.s16 q15, q14, d4", q15, q14, i32, 0x73, d4, i8, 0xe2);
    TESTINSN_bin("vaddw.s8 q0, q1, d31", q0, q1, i32, 0x73, d31, i8, 0xe2);
    TESTINSN_bin("vaddw.u32 q0, q1, d4", q0, q1, i32, 0x73, d4, i8, 0xe2);
    TESTINSN_bin("vaddw.u16 q0, q1, d4", q0, q1, i32, 0x73, d4, i8, 0xe2);
    TESTINSN_bin("vaddw.u8 q0, q1, d4", q0, q1, i32, 0x73, d4, i8, 0xe2);

    printf("---- VADDL ----\n");
    TESTINSN_bin("vaddl.s32 q0, d2, d4", q0, d2, i32, 0x73, d4, i8, 0x12);
    TESTINSN_bin("vaddl.s16 q15, d14, d4", q15, d14, i32, 0x73, d4, i8, 0x12);
    TESTINSN_bin("vaddl.s8 q0, d2, d31", q0, d2, i32, 0x73, d31, i8, 0x12);
    TESTINSN_bin("vaddl.u32 q0, d2, d4", q0, d2, i32, 0x73, d4, i8, 0x12);
    TESTINSN_bin("vaddl.u16 q0, d2, d4", q0, d2, i32, 0x73, d4, i8, 0x12);
    TESTINSN_bin("vaddl.u8 q0, d2, d4", q0, d2, i32, 0x73, d4, i8, 0x12);
    TESTINSN_bin("vaddl.s32 q0, d2, d4", q0, d2, i32, 0x73, d4, i8, 0xe2);
    TESTINSN_bin("vaddl.s16 q15, d14, d4", q15, d14, i32, 0x73, d4, i8, 0xe2);
    TESTINSN_bin("vaddl.s8 q0, d2, d31", q0, d2, i32, 0x73, d31, i8, 0xe2);
    TESTINSN_bin("vaddl.u32 q0, d2, d4", q0, d2, i32, 0x73, d4, i8, 0xe2);
    TESTINSN_bin("vaddl.u16 q0, d2, d4", q0, d2, i32, 0x73, d4, i8, 0xe2);
    TESTINSN_bin("vaddl.u8 q0, d2, d4", q0, d2, i32, 0x73, d4, i8, 0xe2);
    TESTINSN_bin("vaddl.s32 q0, d2, d4", q0, d2, i8, 0x93, d4, i8, 0x12);
    TESTINSN_bin("vaddl.s16 q15, d14, d4", q15, d14, i8, 0x93, d4, i8, 0x12);
    TESTINSN_bin("vaddl.s8 q0, d2, d31", q0, d2, i8, 0x99, d31, i8, 0x12);
    TESTINSN_bin("vaddl.u32 q0, d2, d4", q0, d2, i8, 0x93, d4, i8, 0x12);
    TESTINSN_bin("vaddl.u16 q0, d2, d4", q0, d2, i8, 0x93, d4, i8, 0x12);
    TESTINSN_bin("vaddl.u8 q0, d2, d4", q0, d2, i8, 0x93, d4, i8, 0x12);
    TESTINSN_bin("vaddl.s32 q0, d2, d4", q0, d2, i8, 0x93, d4, i8, 0xe2);
    TESTINSN_bin("vaddl.s16 q15, d14, d4", q15, d14, i8, 0x93, d4, i8, 0xe2);
    TESTINSN_bin("vaddl.s8 q0, d2, d31", q0, d2, i8, 0x93, d31, i8, 0xe2);
    TESTINSN_bin("vaddl.u32 q0, d2, d4", q0, d2, i8, 0x93, d4, i8, 0xe2);
    TESTINSN_bin("vaddl.u16 q0, d2, d4", q0, d2, i8, 0x93, d4, i8, 0xe2);
    TESTINSN_bin("vaddl.u8 q0, d2, d4", q0, d2, i8, 0x93, d4, i8, 0xe2);

    printf("---- VSUBW ----\n");
    TESTINSN_bin("vsubw.s32 q0, q1, d4", q0, q1, i32, 0x73, d4, i8, 0x12);
    TESTINSN_bin("vsubw.s16 q15, q14, d4", q15, q14, i32, 0x73, d4, i8, 0x12);
    TESTINSN_bin("vsubw.s8 q0, q1, d31", q0, q1, i32, 0x73, d31, i8, 0x12);
    TESTINSN_bin("vsubw.u32 q0, q1, d4", q0, q1, i32, 0x73, d4, i8, 0x12);
    TESTINSN_bin("vsubw.u16 q0, q1, d4", q0, q1, i32, 0x73, d4, i8, 0x12);
    TESTINSN_bin("vsubw.u8 q0, q1, d4", q0, q1, i32, 0x73, d4, i8, 0x12);
    TESTINSN_bin("vsubw.s32 q0, q1, d4", q0, q1, i32, 0x73, d4, i8, 0xe2);
    TESTINSN_bin("vsubw.s16 q15, q14, d4", q15, q14, i32, 0x73, d4, i8, 0xe2);
    TESTINSN_bin("vsubw.s8 q0, q1, d31", q0, q1, i32, 0x73, d31, i8, 0xe2);
    TESTINSN_bin("vsubw.u32 q0, q1, d4", q0, q1, i32, 0x73, d4, i8, 0xe2);
    TESTINSN_bin("vsubw.u16 q0, q1, d4", q0, q1, i32, 0x73, d4, i8, 0xe2);
    TESTINSN_bin("vsubw.u8 q0, q1, d4", q0, q1, i32, 0x73, d4, i8, 0xe2);

    printf("---- VSUBL ----\n");
    TESTINSN_bin("vsubl.s32 q0, d2, d4", q0, d2, i32, 0x73, d4, i8, 0x12);
    TESTINSN_bin("vsubl.s16 q15, d14, d4", q15, d14, i32, 0x73, d4, i8, 0x12);
    TESTINSN_bin("vsubl.s8 q0, d2, d31", q0, d2, i32, 0x73, d31, i8, 0x12);
    TESTINSN_bin("vsubl.u32 q0, d2, d4", q0, d2, i32, 0x73, d4, i8, 0x12);
    TESTINSN_bin("vsubl.u16 q0, d2, d4", q0, d2, i32, 0x73, d4, i8, 0x12);
    TESTINSN_bin("vsubl.u8 q0, d2, d4", q0, d2, i32, 0x73, d4, i8, 0x12);
    TESTINSN_bin("vsubl.s32 q0, d2, d4", q0, d2, i32, 0x73, d4, i8, 0xe2);
    TESTINSN_bin("vsubl.s16 q15, d14, d4", q15, d14, i32, 0x73, d4, i8, 0xe2);
    TESTINSN_bin("vsubl.s8 q0, d2, d31", q0, d2, i32, 0x73, d31, i8, 0xe2);
    TESTINSN_bin("vsubl.u32 q0, d2, d4", q0, d2, i32, 0x73, d4, i8, 0xe2);
    TESTINSN_bin("vsubl.u16 q0, d2, d4", q0, d2, i32, 0x73, d4, i8, 0xe2);
    TESTINSN_bin("vsubl.u8 q0, d2, d4", q0, d2, i32, 0x73, d4, i8, 0xe2);
    TESTINSN_bin("vsubl.s32 q0, d2, d4", q0, d2, i8, 0x93, d4, i8, 0x12);
    TESTINSN_bin("vsubl.s16 q15, d14, d4", q15, d14, i8, 0x93, d4, i8, 0x12);
    TESTINSN_bin("vsubl.s8 q0, d2, d31", q0, d2, i8, 0x99, d31, i8, 0x12);
    TESTINSN_bin("vsubl.u32 q0, d2, d4", q0, d2, i8, 0x93, d4, i8, 0x12);
    TESTINSN_bin("vsubl.u16 q0, d2, d4", q0, d2, i8, 0x93, d4, i8, 0x12);
    TESTINSN_bin("vsubl.u8 q0, d2, d4", q0, d2, i8, 0x93, d4, i8, 0x12);
    TESTINSN_bin("vsubl.s32 q0, d2, d4", q0, d2, i8, 0x93, d4, i8, 0xe2);
    TESTINSN_bin("vsubl.s16 q15, d14, d4", q15, d14, i8, 0x93, d4, i8, 0xe2);
    TESTINSN_bin("vsubl.s8 q0, d2, d31", q0, d2, i8, 0x93, d31, i8, 0xe2);
    TESTINSN_bin("vsubl.u32 q0, d2, d4", q0, d2, i8, 0x93, d4, i8, 0xe2);
    TESTINSN_bin("vsubl.u16 q0, d2, d4", q0, d2, i8, 0x93, d4, i8, 0xe2);
    TESTINSN_bin("vsubl.u8 q0, d2, d4", q0, d2, i8, 0x93, d4, i8, 0xe2);

    printf("---- VCEQ #0 ----\n");
    TESTINSN_un("vceq.i32 q0, q1, #0", q0, q1, i32, 0x21);
    TESTINSN_un("vceq.i16 q2, q1, #0", q2, q1, i32, 0x21);
    TESTINSN_un("vceq.i8 q10, q11, #0", q10, q11, i32, 0x21);
    TESTINSN_un("vceq.i32 q0, q1, #0", q0, q1, i32, 0x0);
    TESTINSN_un("vceq.i16 q2, q1, #0", q2, q1, i32, 0x0);
    TESTINSN_un("vceq.i8 q10, q11, #0", q10, q11, i32, 0x0);

    printf("---- VCGT #0 ----\n");
    TESTINSN_un("vcgt.s32 q0, q1, #0", q0, q1, i32, 0x21);
    TESTINSN_un("vcgt.s16 q2, q1, #0", q2, q1, i32, 0x21);
    TESTINSN_un("vcgt.s8 q10, q11, #0", q10, q11, i32, 0x21);
    TESTINSN_un("vcgt.s32 q0, q1, #0", q0, q1, i32, 0x0);
    TESTINSN_un("vcgt.s16 q2, q1, #0", q2, q1, i32, 0x0);
    TESTINSN_un("vcgt.s8 q10, q11, #0", q10, q11, i32, 0x0);
    TESTINSN_un("vcgt.s32 q0, q1, #0", q0, q1, i8, 0xef);
    TESTINSN_un("vcgt.s16 q2, q1, #0", q2, q1, i8, 0xed);
    TESTINSN_un("vcgt.s8 q10, q11, #0", q10, q11, i8, 0xae);

    printf("---- VCGE #0 ----\n");
    TESTINSN_un("vcge.s32 q0, q1, #0", q0, q1, i32, 0x21);
    TESTINSN_un("vcge.s16 q2, q1, #0", q2, q1, i32, 0x21);
    TESTINSN_un("vcge.s8 q10, q11, #0", q10, q11, i32, 0x21);
    TESTINSN_un("vcge.s32 q0, q1, #0", q0, q1, i32, 0x0);
    TESTINSN_un("vcge.s16 q2, q1, #0", q2, q1, i32, 0x0);
    TESTINSN_un("vcge.s8 q10, q11, #0", q10, q11, i32, 0x0);
    TESTINSN_un("vcge.s32 q0, q1, #0", q0, q1, i8, 0xef);
    TESTINSN_un("vcge.s16 q2, q1, #0", q2, q1, i8, 0xed);
    TESTINSN_un("vcge.s8 q10, q11, #0", q10, q11, i8, 0xae);
    TESTINSN_un("vcge.s32 q0, q1, #0", q0, q1, i32, 0xef);
    TESTINSN_un("vcge.s16 q2, q1, #0", q2, q1, i32, 0xed);
    TESTINSN_un("vcge.s8 q10, q11, #0", q10, q11, i32, 0xae);

    printf("---- VCLE #0 ----\n");
    TESTINSN_un("vcle.s32 q0, q1, #0", q0, q1, i32, 0x21);
    TESTINSN_un("vcle.s16 q2, q1, #0", q2, q1, i32, 0x21);
    TESTINSN_un("vcle.s8 q10, q11, #0", q10, q11, i32, 0x21);
    TESTINSN_un("vcle.s32 q0, q1, #0", q0, q1, i32, 0x0);
    TESTINSN_un("vcle.s16 q2, q1, #0", q2, q1, i32, 0x0);
    TESTINSN_un("vcle.s8 q10, q11, #0", q10, q11, i32, 0x0);
    TESTINSN_un("vcle.s32 q0, q1, #0", q0, q1, i8, 0xef);
    TESTINSN_un("vcle.s16 q2, q1, #0", q2, q1, i8, 0xed);
    TESTINSN_un("vcle.s8 q10, q11, #0", q10, q11, i8, 0xae);

    printf("---- VCLT #0 ----\n");
    TESTINSN_un("vclt.s32 q0, q1, #0", q0, q1, i32, 0x21);
    TESTINSN_un("vclt.s16 q2, q1, #0", q2, q1, i32, 0x21);
    TESTINSN_un("vclt.s8 q10, q11, #0", q10, q11, i32, 0x21);
    TESTINSN_un("vclt.s32 q0, q1, #0", q0, q1, i32, 0x0);
    TESTINSN_un("vclt.s16 q2, q1, #0", q2, q1, i32, 0x0);
    TESTINSN_un("vclt.s8 q10, q11, #0", q10, q11, i32, 0x0);
    TESTINSN_un("vclt.s32 q0, q1, #0", q0, q1, i8, 0xef);
    TESTINSN_un("vclt.s16 q2, q1, #0", q2, q1, i8, 0xed);
    TESTINSN_un("vclt.s8 q10, q11, #0", q10, q11, i8, 0xae);
    TESTINSN_un("vclt.s32 q0, q1, #0", q0, q1, i32, 0xef);
    TESTINSN_un("vclt.s16 q2, q1, #0", q2, q1, i32, 0xed);
    TESTINSN_un("vclt.s8 q10, q11, #0", q10, q11, i32, 0xae);

    printf("---- VCNT ----\n");
    TESTINSN_un("vcnt.8 q0, q1", q0, q1, i32, 0xac3d25eb);
    TESTINSN_un("vcnt.8 q11, q14", q11, q14, i32, 0xac3d25eb);
    TESTINSN_un("vcnt.8 q6, q2", q6, q2, i32, 0xad0eb);

    printf("---- VCLS ----\n");
    TESTINSN_un("vcls.s8 q0, q1", q0, q1, i32, 0x21);
    TESTINSN_un("vcls.s8 q10, q15", q10, q15, i8, 0x82);
    TESTINSN_un("vcls.s16 q0, q1", q0, q1, i32, 0x21);
    TESTINSN_un("vcls.s16 q15, q10", q15, q10, i8, 0x82);
    TESTINSN_un("vcls.s32 q6, q1", q6, q1, i32, 0x21);
    TESTINSN_un("vcls.s32 q10, q5", q10, q5, i8, 0x82);
    TESTINSN_un("vcls.s8 q2, q4", q2, q4, i8, 0xff);
    TESTINSN_un("vcls.s16 q2, q4", q2, q4, i8, 0xff);
    TESTINSN_un("vcls.s32 q2, q4", q2, q4, i8, 0xff);
    TESTINSN_un("vcls.s8 q2, q4", q2, q4, i16, 0xffef);
    TESTINSN_un("vcls.s16 q2, q4", q2, q4, i16, 0xffef);
    TESTINSN_un("vcls.s32 q2, q4", q2, q4, i16, 0xffef);
    TESTINSN_un("vcls.s8 q2, q4", q2, q4, i8, 0x00);
    TESTINSN_un("vcls.s16 q2, q4", q2, q4, i8, 0x00);
    TESTINSN_un("vcls.s32 q2, q4", q2, q4, i8, 0x00);
    TESTINSN_un("vcls.s8 q2, q4", q2, q4, i16, 0x00ef);
    TESTINSN_un("vcls.s16 q2, q4", q2, q4, i16, 0x00ef);
    TESTINSN_un("vcls.s32 q2, q4", q2, q4, i16, 0x00ef);

    printf("---- VCLZ ----\n");
    TESTINSN_un("vclz.i8 q0, q1", q0, q1, i32, 0x21);
    TESTINSN_un("vclz.i8 q10, q15", q10, q15, i8, 0x82);
    TESTINSN_un("vclz.i16 q0, q1", q0, q1, i32, 0x21);
    TESTINSN_un("vclz.i16 q15, q10", q15, q10, i8, 0x82);
    TESTINSN_un("vclz.i32 q6, q1", q6, q1, i32, 0x21);
    TESTINSN_un("vclz.i32 q10, q5", q10, q5, i8, 0x82);
    TESTINSN_un("vclz.i8 q2, q4", q2, q4, i8, 0xff);
    TESTINSN_un("vclz.i16 q2, q4", q2, q4, i8, 0xff);
    TESTINSN_un("vclz.i32 q2, q4", q2, q4, i8, 0xff);
    TESTINSN_un("vclz.i8 q2, q4", q2, q4, i16, 0xffef);
    TESTINSN_un("vclz.i16 q2, q4", q2, q4, i16, 0xffef);
    TESTINSN_un("vclz.i32 q2, q4", q2, q4, i16, 0xffef);
    TESTINSN_un("vclz.i8 q2, q4", q2, q4, i8, 0x00);
    TESTINSN_un("vclz.i16 q2, q4", q2, q4, i8, 0x00);
    TESTINSN_un("vclz.i32 q2, q4", q2, q4, i8, 0x00);
    TESTINSN_un("vclz.i8 q2, q4", q2, q4, i16, 0x00ef);
    TESTINSN_un("vclz.i16 q2, q4", q2, q4, i16, 0x00ef);
    TESTINSN_un("vclz.i32 q2, q4", q2, q4, i16, 0x00ef);

    printf("---- VSLI ----\n");
    TESTINSN_un("vsli.16 q0, q1, #1", q0, q1, i32, -1);
    TESTINSN_un("vsli.16 q3, q4, #2", q3, q4, i32, -0x7c);
    TESTINSN_un("vsli.32 q2, q5, #31", q2, q5, i32, -1);
    TESTINSN_un("vsli.8 q6, q7, #7", q6, q7, i32, 0xffff);
    TESTINSN_un("vsli.16 q8, q9, #12", q8, q9, i32, -10);
    TESTINSN_un("vsli.32 q10, q11, #5", q10, q11, i32, 10234);
    TESTINSN_un("vsli.8 q12, q13, #1", q12, q13, i32, -1);
    TESTINSN_un("vsli.16 q14, q15, #11", q14, q15, i32, -1);
    TESTINSN_un("vsli.32 q10, q11, #9", q10, q11, i32, 1000);
    TESTINSN_un("vsli.8 q7, q13, #7", q7, q13, i32, -1);
    TESTINSN_un("vsli.16 q8, q1, #1", q8, q1, i32, 0xabcf);
    TESTINSN_un("vsli.32 q12, q3, #15", q12, q3, i32, -0x1b0);
    TESTINSN_un("vsli.64 q0, q1, #42", q0, q1, i32, -1);
    TESTINSN_un("vsli.64 q6, q7, #12", q6, q7, i32, 0xfac);
    TESTINSN_un("vsli.64 q8, q4, #9", q8, q4, i32, 13560);
    TESTINSN_un("vsli.64 q9, q12, #11", q9, q12, i32, 98710);

    printf("---- VPADDL ----\n");
    TESTINSN_un("vpaddl.u32 q0, q1", q0, q1, i32, 24);
    TESTINSN_un("vpaddl.u32 q0, q1", q0, q1, i32, 140);
    TESTINSN_un("vpaddl.u16 q0, q1", q0, q1, i32, 140);
    TESTINSN_un("vpaddl.u8 q0, q1", q0, q1, i32, 140);
    TESTINSN_un("vpaddl.u8 q0, q1", q0, q1, i32, (1 << 31) + 1);
    TESTINSN_un("vpaddl.u16 q0, q1", q0, q1, i32, (1 << 31) + 1);
    TESTINSN_un("vpaddl.u32 q0, q1", q0, q1, i32, (1 << 31) + 1);
    TESTINSN_un("vpaddl.u32 q10, q11", q10, q11, i32, 24);
    TESTINSN_un("vpaddl.s32 q0, q1", q0, q1, i32, 24);
    TESTINSN_un("vpaddl.s32 q0, q1", q0, q1, i32, 140);
    TESTINSN_un("vpaddl.s16 q0, q1", q0, q1, i32, 140);
    TESTINSN_un("vpaddl.s8 q0, q1", q0, q1, i32, 140);
    TESTINSN_un("vpaddl.s8 q0, q1", q0, q1, i32, (1 << 31) + 1);
    TESTINSN_un("vpaddl.s16 q0, q1", q0, q1, i32, (1 << 31) + 1);
    TESTINSN_un("vpaddl.s32 q0, q1", q0, q1, i32, (1 << 31) + 1);
    TESTINSN_un("vpaddl.s32 q10, q11", q10, q11, i32, 24);

    printf("---- VPADAL ----\n");
    TESTINSN_un("vpadal.u32 q0, q1", q0, q1, i32, 24);
    TESTINSN_un("vpadal.u32 q0, q1", q0, q1, i32, 140);
    TESTINSN_un("vpadal.u16 q0, q1", q0, q1, i32, 140);
    TESTINSN_un("vpadal.u8 q0, q1", q0, q1, i8, 140);
    TESTINSN_un("vpadal.u8 q0, q1", q0, q1, i32, (1 << 31) + 1);
    TESTINSN_un("vpadal.u16 q0, q1", q0, q1, i32, (1 << 31) + 1);
    TESTINSN_un("vpadal.u32 q0, q1", q0, q1, i32, (1 << 31) + 1);
    TESTINSN_un("vpadal.u32 q10, q11", q10, q11, i32, 24);
    TESTINSN_un("vpadal.s32 q0, q1", q0, q1, i32, 24);
    TESTINSN_un("vpadal.s32 q0, q1", q0, q1, i32, 140);
    TESTINSN_un("vpadal.s16 q0, q1", q0, q1, i32, 140);
    TESTINSN_un("vpadal.s8 q0, q1", q0, q1, i8, 140);
    TESTINSN_un("vpadal.s8 q0, q1", q0, q1, i32, (1 << 31) + 1);
    TESTINSN_un("vpadal.s16 q0, q1", q0, q1, i32, (1 << 31) + 1);
    TESTINSN_un("vpadal.s32 q0, q1", q0, q1, i32, (1 << 31) + 1);
    TESTINSN_un("vpadal.s32 q10, q11", q10, q11, i32, 24);

    printf("---- VZIP ----\n");
    TESTINSN_dual("vzip.32 q0, q1", q0, i8, 0x12, q1, i8, 0x34);
    TESTINSN_dual("vzip.16 q1, q0", q0, i8, 0x12, q1, i8, 0x34);
    TESTINSN_dual("vzip.8 q10, q11", q10, i8, 0x12, q11, i8, 0x34);
    TESTINSN_dual("vzip.32 q0, q1", q0, i32, 0x12345678, q1, i32, 0x0a0b0c0d);
    TESTINSN_dual("vzip.16 q1, q0", q0, i32, 0x12345678, q1, i32, 0x0a0b0c0d);
    TESTINSN_dual("vzip.8 q10, q11", q10, i32, 0x12345678, q11, i32, 0x0a0b0c0d);

    printf("---- VUZP ----\n");
    TESTINSN_dual("vuzp.32 q0, q1", q0, i8, 0x12, q1, i8, 0x34);
    TESTINSN_dual("vuzp.16 q1, q0", q0, i8, 0x12, q1, i8, 0x34);
    TESTINSN_dual("vuzp.8 q10, q11", q10, i8, 0x12, q11, i8, 0x34);
    TESTINSN_dual("vuzp.32 q0, q1", q0, i32, 0x12345678, q1, i32, 0x0a0b0c0d);
    TESTINSN_dual("vuzp.16 q1, q0", q0, i32, 0x12345678, q1, i32, 0x0a0b0c0d);
    TESTINSN_dual("vuzp.8 q10, q11", q10, i32, 0x12345678, q11, i32, 0x0a0b0c0d);

    printf("---- VTRN ----\n");
    TESTINSN_dual("vtrn.32 q0, q1", q0, i8, 0x12, q1, i8, 0x34);
    TESTINSN_dual("vtrn.16 q1, q0", q0, i8, 0x12, q1, i8, 0x34);
    TESTINSN_dual("vtrn.8 q10, q11", q10, i8, 0x12, q11, i8, 0x34);
    TESTINSN_dual("vtrn.32 q0, q1", q0, i32, 0x12345678, q1, i32, 0x0a0b0c0d);
    TESTINSN_dual("vtrn.16 q1, q0", q0, i32, 0x12345678, q1, i32, 0x0a0b0c0d);
    TESTINSN_dual("vtrn.8 q10, q11", q10, i32, 0x12345678, q11, i32, 0x0a0b0c0d);

    printf("---- VSWP ----\n");
    TESTINSN_dual("vswp q0, q1", q0, i8, 0x12, q1, i8, 0x34);
    TESTINSN_dual("vswp q1, q0", q0, i8, 0x12, q1, i8, 0x34);
    TESTINSN_dual("vswp q10, q11", q10, i8, 0x12, q11, i8, 0x34);
    TESTINSN_dual("vswp q0, q1", q0, i32, 0x12345678, q1, i32, 0x0a0b0c0d);
    TESTINSN_dual("vswp q1, q0", q0, i32, 0x12345678, q1, i32, 0x0a0b0c0d);
    TESTINSN_dual("vswp q10, q11", q10, i32, 0x12345678, q11, i32, 0x0a0b0c0d);

    printf("---- VDUP ----\n");
    TESTINSN_un("vdup.8 q2, d2[0]", q2, d2, i32, 0xabc4657);
    TESTINSN_un("vdup.8 q3, d3[2]", q3, d3, i32, 0x7a1b3);
    TESTINSN_un("vdup.8 q1, d0[7]", q1, d0, i32, 0x713aaa);
    TESTINSN_un("vdup.8 q0, d4[3]", q0, d4, i32, 0xaa713);
    TESTINSN_un("vdup.8 q4, d28[4]", q4, d28, i32, 0x7b1c3);
    TESTINSN_un("vdup.16 q7, d19[3]", q7, d19, i32, 0x713ffff);
    TESTINSN_un("vdup.16 q15, d31[0]", q15, d31, i32, 0x7f00fa);
    TESTINSN_un("vdup.16 q6, d2[0]", q6, d2, i32, 0xffabcde);
    TESTINSN_un("vdup.16 q8, d22[3]", q8, d22, i32, 0x713);
    TESTINSN_un("vdup.16 q9, d2[0]", q9, d2, i32, 0x713);
    TESTINSN_un("vdup.32 q10, d17[1]", q10, d17, i32, 0x713);
    TESTINSN_un("vdup.32 q15, d11[0]", q15, d11, i32, 0x3);
    TESTINSN_un("vdup.32 q10, d29[1]", q10, d29, i32, 0xf00000aa);
    TESTINSN_un("vdup.32 q12, d0[1]", q12, d0, i32, 0xf);
    TESTINSN_un("vdup.32 q13, d13[0]", q13, d13, i32, -1);

    printf("---- VQDMULL ----\n");
    TESTINSN_bin_q("vqdmull.s32 q0, d1, d2", q0, d1, i32, 24, d2, i32, 120);
    TESTINSN_bin_q("vqdmull.s32 q6, d7, d8", q6, d7, i32, 140, d8, i32, -120);
    TESTINSN_bin_q("vqdmull.s16 q9, d11, d12", q9, d11, i32, 0x140, d12, i32, 0x120);
    TESTINSN_bin_q("vqdmull.s16 q4, d5, d6", q4, d5, i32, (1 << 14) + 1, d6, i32, (1 << 13) + 2);
    TESTINSN_bin_q("vqdmull.s32 q7, d8, d9", q7, d8, i32, (1 << 31) + 1, d9, i32, (1 << 31) + 2);
    TESTINSN_bin_q("vqdmull.s16 q4, d5, d6", q4, d5, i32, (1 << 14) - 0xabcd, d6, i32, (1 << 13) + 2);
    TESTINSN_bin_q("vqdmull.s32 q7, d8, d9", q7, d8, i32, (1 << 31), d9, i32, 12);
    TESTINSN_bin_q("vqdmull.s16 q4, d5, d6", q4, d5, i32, (1 << 28) + 0xfe, d6, i32, (1 << 13) + 2);
    TESTINSN_bin_q("vqdmull.s32 q7, d8, d9", q7, d8, i32, (1 << 31) + 1, d9, i32, (1 << 31) + 2);
    TESTINSN_bin_q("vqdmull.s32 q10, d11, d15", q10, d11, i32, 24, d15, i32, 120);
    TESTINSN_bin_q("vqdmull.s32 q10, d30, d31", q10, d30, i32, 1 << 31, d31, i32, 1 << 31);
    TESTINSN_bin_q("vqdmull.s16 q10, d30, d31", q10, d30, i32, 1 << 31, d31, i32, 1 << 31);
    TESTINSN_bin_q("vqdmull.s32 q10, d30, d31", q10, d30, i32, 1 << 30, d31, i32, 1 << 31);
    TESTINSN_bin_q("vqdmull.s16 q10, d30, d31", q10, d30, i32, 1 << 31, d31, i32, 1 << 30);

    printf("---- VQDMULL (by scalar) ----\n");
    TESTINSN_bin_q("vqdmull.s32 q0, d1, d7[0]", q0, d1, i32, 24, d7, i32, 120);
    TESTINSN_bin_q("vqdmull.s32 q6, d7, d6[0]", q6, d7, i32, 140, d6, i32, -120);
    TESTINSN_bin_q("vqdmull.s16 q9, d11, d7[2]", q9, d11, i32, 0x140, d7, i32, 0x120);
    TESTINSN_bin_q("vqdmull.s16 q4, d5, d6[2]", q4, d5, i32, (1 << 14) + 1, d6, i32, (1 << 13) + 2);
    TESTINSN_bin_q("vqdmull.s32 q7, d8, d3[1]", q7, d8, i32, (1 << 31) + 1, d3, i32, (1 << 31) + 2);
    TESTINSN_bin_q("vqdmull.s16 q4, d5, d6[1]", q4, d5, i32, (1 << 14) - 0xabcd, d6, i16, (1 << 13) + 2);
    TESTINSN_bin_q("vqdmull.s32 q7, d8, d3[0]", q7, d8, i32, (1 << 31), d3, i32, 12);
    TESTINSN_bin_q("vqdmull.s16 q4, d5, d6[2]", q4, d5, i32, (1 << 28) + 0xfe, d6, i32, (1 << 13) + 2);
    TESTINSN_bin_q("vqdmull.s32 q7, d8, d3[1]", q7, d8, i32, (1 << 31) + 1, d3, i32, (1 << 31) + 2);
    TESTINSN_bin_q("vqdmull.s32 q10, d11, d15[1]", q10, d11, i32, 24, d15, i32, 120);
    TESTINSN_bin_q("vqdmull.s32 q10, d30, d1[0]", q10, d30, i32, 1 << 31, d1, i32, 1 << 31);
    TESTINSN_bin_q("vqdmull.s16 q10, d30, d1[1]", q10, d30, i32, 1 << 31, d1, i32, 1 << 31);
    TESTINSN_bin_q("vqdmull.s32 q10, d30, d1[1]", q10, d30, i32, 1 << 30, d1, i32, 1 << 31);
    TESTINSN_bin_q("vqdmull.s16 q10, d30, d1[3]", q10, d30, i32, 1 << 31, d1, i32, 1 << 30);

    printf("---- VQDMLSL ----\n");
    TESTINSN_bin_q("vqdmlsl.s32 q0, d1, d2", q0, d1, i32, 24, d2, i32, 120);
    TESTINSN_bin_q("vqdmlsl.s32 q6, d7, d8", q6, d7, i32, 140, d8, i32, -120);
    TESTINSN_bin_q("vqdmlsl.s16 q9, d11, d12", q9, d11, i32, 0x140, d12, i32, 0x120);
    TESTINSN_bin_q("vqdmlsl.s16 q4, d5, d6", q4, d5, i32, (1 << 14) + 1, d6, i32, (1 << 13) + 2);
    TESTINSN_bin_q("vqdmlsl.s32 q7, d8, d9", q7, d8, i32, (1 << 31) + 1, d9, i32, (1 << 31) + 2);
    TESTINSN_bin_q("vqdmlsl.s16 q4, d5, d6", q4, d5, i32, (1 << 14) - 0xabcd, d6, i32, (1 << 13) + 2);
    TESTINSN_bin_q("vqdmlsl.s32 q7, d8, d9", q7, d8, i32, (1 << 31), d9, i32, 12);
    TESTINSN_bin_q("vqdmlsl.s16 q4, d5, d6", q4, d5, i32, (1 << 28) + 0xfe, d6, i32, (1 << 13) + 2);
    TESTINSN_bin_q("vqdmlsl.s32 q7, d8, d9", q7, d8, i32, (1 << 31) + 1, d9, i32, (1 << 31) + 2);
    TESTINSN_bin_q("vqdmlsl.s32 q10, d11, d15", q10, d11, i32, 24, d15, i32, 120);
    TESTINSN_bin_q("vqdmlsl.s32 q10, d30, d31", q10, d30, i32, 1 << 31, d31, i32, 1 << 31);
    TESTINSN_bin_q("vqdmlsl.s16 q10, d30, d31", q10, d30, i32, 1 << 31, d31, i32, 1 << 31);
    TESTINSN_bin_q("vqdmlsl.s32 q10, d30, d31", q10, d30, i32, 1 << 30, d31, i32, 1 << 31);
    TESTINSN_bin_q("vqdmlsl.s16 q10, d30, d31", q10, d30, i32, 1 << 31, d31, i32, 1 << 30);

    printf("---- VQDMLSL (by scalar) ----\n");
    TESTINSN_bin_q("vqdmlsl.s32 q0, d1, d7[0]", q0, d1, i32, 24, d7, i32, 120);
    TESTINSN_bin_q("vqdmlsl.s32 q6, d7, d6[0]", q6, d7, i32, 140, d6, i32, -120);
    TESTINSN_bin_q("vqdmlsl.s16 q9, d11, d7[2]", q9, d11, i32, 0x140, d7, i32, 0x120);
    TESTINSN_bin_q("vqdmlsl.s16 q4, d5, d6[2]", q4, d5, i32, (1 << 14) + 1, d6, i32, (1 << 13) + 2);
    TESTINSN_bin_q("vqdmlsl.s32 q7, d8, d3[1]", q7, d8, i32, (1 << 31) + 1, d3, i32, (1 << 31) + 2);
    TESTINSN_bin_q("vqdmlsl.s16 q4, d5, d6[1]", q4, d5, i32, (1 << 14) - 0xabcd, d6, i16, (1 << 13) + 2);
    TESTINSN_bin_q("vqdmlsl.s32 q7, d8, d3[0]", q7, d8, i32, (1 << 31), d3, i32, 12);
    TESTINSN_bin_q("vqdmlsl.s16 q4, d5, d6[2]", q4, d5, i32, (1 << 28) + 0xfe, d6, i32, (1 << 13) + 2);
    TESTINSN_bin_q("vqdmlsl.s32 q7, d8, d3[1]", q7, d8, i32, (1 << 31) + 1, d3, i32, (1 << 31) + 2);
    TESTINSN_bin_q("vqdmlsl.s32 q10, d11, d15[1]", q10, d11, i32, 24, d15, i32, 120);
    TESTINSN_bin_q("vqdmlsl.s32 q10, d30, d1[0]", q10, d30, i32, 1 << 31, d1, i32, 1 << 31);
    TESTINSN_bin_q("vqdmlsl.s16 q10, d30, d1[1]", q10, d30, i32, 1 << 31, d1, i32, 1 << 31);
    TESTINSN_bin_q("vqdmlsl.s32 q10, d30, d1[1]", q10, d30, i32, 1 << 30, d1, i32, 1 << 31);
    TESTINSN_bin_q("vqdmlsl.s16 q10, d30, d1[3]", q10, d30, i32, 1 << 31, d1, i32, 1 << 30);

    printf("---- VQDMLAL ----\n");
    TESTINSN_bin_q("vqdmlal.s32 q0, d1, d2", q0, d1, i32, 24, d2, i32, 120);
    TESTINSN_bin_q("vqdmlal.s32 q6, d7, d8", q6, d7, i32, 140, d8, i32, -120);
    TESTINSN_bin_q("vqdmlal.s16 q9, d11, d12", q9, d11, i32, 0x140, d12, i32, 0x120);
    TESTINSN_bin_q("vqdmlal.s16 q4, d5, d6", q4, d5, i32, (1 << 14) + 1, d6, i32, (1 << 13) + 2);
    TESTINSN_bin_q("vqdmlal.s32 q7, d8, d9", q7, d8, i32, (1 << 31) + 1, d9, i32, (1 << 31) + 2);
    TESTINSN_bin_q("vqdmlal.s16 q4, d5, d6", q4, d5, i32, (1 << 14) - 0xabcd, d6, i32, (1 << 13) + 2);
    TESTINSN_bin_q("vqdmlal.s32 q7, d8, d9", q7, d8, i32, (1 << 31), d9, i32, 12);
    TESTINSN_bin_q("vqdmlal.s16 q4, d5, d6", q4, d5, i32, (1 << 28) + 0xfe, d6, i32, (1 << 13) + 2);
    TESTINSN_bin_q("vqdmlal.s32 q7, d8, d9", q7, d8, i32, (1 << 31) + 1, d9, i32, (1 << 31) + 2);
    TESTINSN_bin_q("vqdmlal.s32 q10, d11, d15", q10, d11, i32, 24, d15, i32, 120);
    TESTINSN_bin_q("vqdmlal.s32 q10, d30, d31", q10, d30, i32, 1 << 31, d31, i32, 1 << 31);
    TESTINSN_bin_q("vqdmlal.s16 q10, d30, d31", q10, d30, i32, 1 << 31, d31, i32, 1 << 31);
    TESTINSN_bin_q("vqdmlal.s32 q10, d30, d31", q10, d30, i32, 1 << 30, d31, i32, 1 << 31);
    TESTINSN_bin_q("vqdmlal.s16 q10, d30, d31", q10, d30, i32, 1 << 31, d31, i32, 1 << 30);

    printf("---- VQDMLAL (by scalar) ----\n");
    TESTINSN_bin_q("vqdmlal.s32 q0, d1, d7[0]", q0, d1, i32, 24, d7, i32, 120);
    TESTINSN_bin_q("vqdmlal.s32 q6, d7, d6[0]", q6, d7, i32, 140, d6, i32, -120);
    TESTINSN_bin_q("vqdmlal.s16 q9, d11, d7[2]", q9, d11, i32, 0x140, d7, i32, 0x120);
    TESTINSN_bin_q("vqdmlal.s16 q4, d5, d6[2]", q4, d5, i32, (1 << 14) + 1, d6, i32, (1 << 13) + 2);
    TESTINSN_bin_q("vqdmlal.s32 q7, d8, d3[1]", q7, d8, i32, (1 << 31) + 1, d3, i32, (1 << 31) + 2);
    TESTINSN_bin_q("vqdmlal.s16 q4, d5, d6[1]", q4, d5, i32, (1 << 14) - 0xabcd, d6, i16, (1 << 13) + 2);
    TESTINSN_bin_q("vqdmlal.s32 q7, d8, d3[0]", q7, d8, i32, (1 << 31), d3, i32, 12);
    TESTINSN_bin_q("vqdmlal.s16 q4, d5, d6[2]", q4, d5, i32, (1 << 28) + 0xfe, d6, i32, (1 << 13) + 2);
    TESTINSN_bin_q("vqdmlal.s32 q7, d8, d3[1]", q7, d8, i32, (1 << 31) + 1, d3, i32, (1 << 31) + 2);
    TESTINSN_bin_q("vqdmlal.s32 q10, d11, d15[1]", q10, d11, i32, 24, d15, i32, 120);
    TESTINSN_bin_q("vqdmlal.s32 q10, d30, d1[0]", q10, d30, i32, 1 << 31, d1, i32, 1 << 31);
    TESTINSN_bin_q("vqdmlal.s16 q10, d30, d1[1]", q10, d30, i32, 1 << 31, d1, i32, 1 << 31);
    TESTINSN_bin_q("vqdmlal.s32 q10, d30, d1[1]", q10, d30, i32, 1 << 30, d1, i32, 1 << 31);
    TESTINSN_bin_q("vqdmlal.s16 q10, d30, d1[3]", q10, d30, i32, 1 << 31, d1, i32, 1 << 30);

    printf("---- VQDMULH ----\n");
    TESTINSN_bin_q("vqdmulh.s32 q0, q1, q2", q0, q1, i32, 24, q2, i32, 120);
    TESTINSN_bin_q("vqdmulh.s32 q6, q7, q8", q6, q7, i32, 140, q8, i32, -120);
    TESTINSN_bin_q("vqdmulh.s16 q9, q11, q12", q9, q11, i32, 0x140, q12, i32, 0x120);
    TESTINSN_bin_q("vqdmulh.s16 q4, q5, q6", q4, q5, i32, (1 << 14) + 1, q6, i32, (1 << 13) + 2);
    TESTINSN_bin_q("vqdmulh.s32 q7, q8, q9", q7, q8, i32, (1 << 31) + 1, q9, i32, (1 << 31) + 2);
    TESTINSN_bin_q("vqdmulh.s16 q4, q5, q6", q4, q5, i32, (1 << 14) - 0xabcd, q6, i32, (1 << 13) + 2);
    TESTINSN_bin_q("vqdmulh.s32 q7, q8, q9", q7, q8, i32, (1 << 31), q9, i32, 12);
    TESTINSN_bin_q("vqdmulh.s16 q4, q5, q6", q4, q5, i32, (1 << 28) + 0xfe, q6, i32, (1 << 13) + 2);
    TESTINSN_bin_q("vqdmulh.s32 q7, q8, q9", q7, q8, i32, (1 << 31) + 1, q9, i32, (1 << 31) + 2);
    TESTINSN_bin_q("vqdmulh.s32 q10, q11, q15", q10, q11, i32, 24, q15, i32, 120);
    TESTINSN_bin_q("vqdmulh.s32 q10, q14, q15", q10, q14, i32, 1 << 31, q15, i32, 1 << 31);
    TESTINSN_bin_q("vqdmulh.s16 q10, q14, q15", q10, q14, i32, 1 << 31, q15, i32, 1 << 31);
    TESTINSN_bin_q("vqdmulh.s32 q10, q14, q15", q10, q14, i32, 1 << 30, q15, i32, 1 << 31);
    TESTINSN_bin_q("vqdmulh.s16 q10, q14, q15", q10, q14, i32, 1 << 31, q15, i32, 1 << 30);

    printf("---- VQDMULH (by scalar) ----\n");
    TESTINSN_bin_q("vqdmulh.s32 q0, q1, d6[0]", q0, q1, i32, 24, d6, i32, 120);
    TESTINSN_bin_q("vqdmulh.s32 q6, q7, d1[1]", q6, q7, i32, 140, d1, i32, -120);
    TESTINSN_bin_q("vqdmulh.s16 q9, q11, d7[0]", q9, q11, i32, 0x140, d7, i32, 0x120);
    TESTINSN_bin_q("vqdmulh.s16 q4, q5, d6[0]", q4, q5, i32, (1 << 14) + 1, d6, i32, (1 << 13) + 2);
    TESTINSN_bin_q("vqdmulh.s32 q7, q8, d9[1]", q7, q8, i32, (1 << 31) + 1, d9, i32, (1 << 31) + 2);
    TESTINSN_bin_q("vqdmulh.s16 q4, q5, d6[1]", q4, q5, i32, (1 << 14) - 0xabcd, d6, i16, (1 << 13) + 2);
    TESTINSN_bin_q("vqdmulh.s32 q7, q8, d9[0]", q7, q8, i32, (1 << 31), d9, i32, 12);
    TESTINSN_bin_q("vqdmulh.s16 q4, q5, d6[2]", q4, q5, i32, (1 << 28) + 0xfe, d6, i32, (1 << 13) + 2);
    TESTINSN_bin_q("vqdmulh.s32 q7, q8, d9[0]", q7, q8, i32, (1 << 31) + 1, d9, i32, (1 << 31) + 2);
    TESTINSN_bin_q("vqdmulh.s32 q10, q11, d15[0]", q10, q11, i32, 24, d15, i32, 120);
    TESTINSN_bin_q("vqdmulh.s32 q10, q14, d15[1]", q10, q14, i32, 1 << 31, d7, i32, 1 << 31);
    TESTINSN_bin_q("vqdmulh.s16 q10, q14, d7[3]", q10, q14, i32, 1 << 31, q15, i32, 1 << 31);
    TESTINSN_bin_q("vqdmulh.s32 q10, q14, d15[1]", q10, q14, i32, 1 << 30, d15, i32, 1 << 31);
    TESTINSN_bin_q("vqdmulh.s16 q10, q14, d7[1]", q10, q14, i32, 1 << 31, d7, i32, 1 << 30);

    printf("---- VSHL (immediate) ----\n");
    TESTINSN_un("vshl.i64 q0, q1, #1", q0, q1, i32, 24);
    TESTINSN_un("vshl.i64 q5, q2, #1", q5, q2, i32, (1 << 30));
    TESTINSN_un("vshl.i64 q9, q12, #2", q9, q12, i32, (1 << 31) + 2);
    TESTINSN_un("vshl.i64 q11, q2, #12", q11, q2, i32, -1);
    TESTINSN_un("vshl.i64 q15, q12, #63", q15, q12, i32, 5);
    TESTINSN_un("vshl.i64 q5, q12, #62", q5, q12, i32, (1 << 31) + 1);
    TESTINSN_un("vshl.i32 q0, q1, #1", q0, q1, i32, 24);
    TESTINSN_un("vshl.i32 q5, q2, #1", q5, q2, i32, (1 << 30));
    TESTINSN_un("vshl.i32 q9, q12, #2", q9, q12, i32, (1 << 31) + 2);
    TESTINSN_un("vshl.i32 q11, q2, #12", q11, q2, i32, -1);
    TESTINSN_un("vshl.i32 q15, q12, #20", q15, q12, i32, 5);
    TESTINSN_un("vshl.i32 q5, q12, #30", q5, q12, i32, (1 << 31) + 1);
    TESTINSN_un("vshl.i16 q0, q1, #1", q0, q1, i16, 24);
    TESTINSN_un("vshl.i16 q5, q2, #1", q5, q2, i32, (1 << 30));
    TESTINSN_un("vshl.i16 q9, q12, #2", q9, q12, i32, (1 << 31) + 2);
    TESTINSN_un("vshl.i16 q11, q2, #12", q11, q2, i16, -1);
    TESTINSN_un("vshl.i16 q15, q12, #3", q15, q12, i16, 5);
    TESTINSN_un("vshl.i16 q5, q12, #14", q5, q12, i32, (1 << 31) + 1);
    TESTINSN_un("vshl.i8 q0, q1, #1", q0, q1, i8, 24);
    TESTINSN_un("vshl.i8 q5, q2, #1", q5, q2, i32, (1 << 30));
    TESTINSN_un("vshl.i8 q9, q12, #2", q9, q12, i32, (1 << 31) + 2);
    TESTINSN_un("vshl.i8 q11, q2, #7", q11, q2, i8, -1);
    TESTINSN_un("vshl.i8 q15, q12, #3", q15, q12, i8, 5);
    TESTINSN_un("vshl.i8 q5, q12, #6", q5, q12, i32, (1 << 31) + 1);

    printf("---- VNEG ----\n");
    TESTINSN_un("vneg.s32 q0, q1", q0, q1, i32, 0x73);
    TESTINSN_un("vneg.s16 q15, q4", q15, q4, i32, 0x73);
    TESTINSN_un("vneg.s8 q8, q7", q8, q7, i32, 0x73);
    TESTINSN_un("vneg.s32 q0, q1", q0, q1, i32, 0xfe);
    TESTINSN_un("vneg.s16 q15, q4", q15, q4, i32, 0xef);
    TESTINSN_un("vneg.s8 q8, q7", q8, q7, i32, 0xde);
    TESTINSN_un("vneg.s32 q0, q1", q0, q1, i16, 0xfe0a);
    TESTINSN_un("vneg.s16 q15, q4", q15, q4, i16, 0xef0b);
    TESTINSN_un("vneg.s8 q8, q7", q8, q7, i16, 0xde0c);

    printf("---- VQNEG ----\n");
    TESTINSN_un_q("vqneg.s32 q0, q1", q0, q1, i32, 0x73);
    TESTINSN_un_q("vqneg.s32 q0, q1", q0, q1, i32, 1 << 31);
    TESTINSN_un_q("vqneg.s16 q0, q1", q0, q1, i32, 1 << 31);
    TESTINSN_un_q("vqneg.s8 q0, q1", q0, q1, i32, 1 << 31);
    TESTINSN_un_q("vqneg.s16 q15, q4", q15, q4, i32, 0x73);
    TESTINSN_un_q("vqneg.s8 q8, q7", q8, q7, i32, 0x73);
    TESTINSN_un_q("vqneg.s32 q0, q1", q0, q1, i32, 0xfe);
    TESTINSN_un_q("vqneg.s16 q15, q4", q15, q4, i32, 0xef);
    TESTINSN_un_q("vqneg.s8 q8, q7", q8, q7, i32, 0xde);
    TESTINSN_un_q("vqneg.s32 q0, q1", q0, q1, i16, 0xfe0a);
    TESTINSN_un_q("vqneg.s16 q15, q4", q15, q4, i16, 0xef0b);
    TESTINSN_un_q("vqneg.s8 q8, q7", q8, q7, i16, 0xde0c);

    printf("---- VREV ----\n");
    TESTINSN_un("vrev64.8 q0, q1", q0, q1, i32, 0xaabbccdd);
    TESTINSN_un("vrev64.16 q10, q15", q10, q15, i32, 0xaabbccdd);
    TESTINSN_un("vrev64.32 q1, q14", q1, q14, i32, 0xaabbccdd);
    TESTINSN_un("vrev32.8 q0, q1", q0, q1, i32, 0xaabbccdd);
    TESTINSN_un("vrev32.16 q10, q15", q10, q15, i32, 0xaabbccdd);
    TESTINSN_un("vrev16.8 q0, q1", q0, q1, i32, 0xaabbccdd);

    printf("---- VSHLL ----\n");
    TESTINSN_un("vshll.s32 q0, d1, #1", q0, d1, i32, 24);
    TESTINSN_un("vshll.s32 q5, d2, #1", q5, d2, i32, (1 << 30));
    TESTINSN_un("vshll.s32 q9, d12, #2", q9, d12, i32, (1 << 31) + 2);
    TESTINSN_un("vshll.u32 q11, d2, #12", q11, d2, i32, -1);
    TESTINSN_un("vshll.u32 q15, d12, #20", q15, d12, i32, 5);
    TESTINSN_un("vshll.u32 q5, d22, #30", q5, d22, i32, (1 << 31) + 1);
    TESTINSN_un("vshll.s16 q0, d1, #1", q0, d1, i16, 24);
    TESTINSN_un("vshll.s16 q5, d2, #1", q5, d2, i32, (1 << 30));
    TESTINSN_un("vshll.s16 q9, d12, #2", q9, d12, i32, (1 << 31) + 2);
    TESTINSN_un("vshll.u16 q11, d2, #12", q11, d2, i16, -1);
    TESTINSN_un("vshll.u16 q15, d22, #3", q15, d22, i16, 5);
    TESTINSN_un("vshll.u16 q5, d12, #14", q5, d12, i32, (1 << 31) + 1);
    TESTINSN_un("vshll.s8 q0, d1, #1", q0, d1, i8, 24);
    TESTINSN_un("vshll.s8 q5, d2, #1", q5, d2, i32, (1 << 30));
    TESTINSN_un("vshll.s8 q9, d12, #2", q9, d12, i32, (1 << 31) + 2);
    TESTINSN_un("vshll.u8 q11, d2, #7", q11, d2, i8, -1);
    TESTINSN_un("vshll.u8 q15, d19, #3", q15, d19, i8, 5);
    TESTINSN_un("vshll.u8 q5, d12, #6", q5, d12, i32, (1 << 31) + 1);

    printf("---- VSHLL (max shift) ----\n");
    TESTINSN_un("vshll.i32 q0, d1, #32", q0, d1, i32, 24);
    TESTINSN_un("vshll.i32 q5, d2, #32", q5, d2, i32, (1 << 30));
    TESTINSN_un("vshll.i32 q11, d2, #32", q11, d2, i32, -1);
    TESTINSN_un("vshll.i32 q15, d12, #32", q15, d12, i32, 5);
    TESTINSN_un("vshll.i16 q0, d1, #16", q0, d1, i16, 24);
    TESTINSN_un("vshll.i16 q5, d2, #16", q5, d2, i32, (1 << 30));
    TESTINSN_un("vshll.i16 q11, d2, #16", q11, d2, i16, -1);
    TESTINSN_un("vshll.i16 q15, d22, #16", q15, d22, i16, 5);
    TESTINSN_un("vshll.i8 q0, d1, #8", q0, d1, i8, 24);
    TESTINSN_un("vshll.i8 q5, d2, #8", q5, d2, i32, (1 << 30));
    TESTINSN_un("vshll.i8 q11, d2, #8", q11, d2, i8, -1);
    TESTINSN_un("vshll.i8 q15, d19, #8", q15, d19, i8, 5);

    printf("---- VMULL ----\n");
    TESTINSN_bin("vmull.s8 q0, d1, d12", q0, d1, i32, 0xabcd4, d12, i32, 0xcefab1);
    TESTINSN_bin("vmull.s8 q9, d11, d12", q9, d11, i32, 0x140, d12, i32, 0x120);
    TESTINSN_bin("vmull.s8 q4, d5, d6", q4, d5, i32, (1 << 14) + 1, d6, i32, (1 << 13) + 2);
    TESTINSN_bin("vmull.s8 q4, d5, d6", q4, d5, i32, (1 << 14) - 0xabcd, d6, i32, (1 << 13) + 0xaa2);
    TESTINSN_bin("vmull.s8 q4, d5, d6", q4, d5, i32, (1 << 28) + 0xfe, d6, i32, (1 << 13) + 0x2bbc2d);
    TESTINSN_bin("vmull.s8 q10, d30, d31", q10, d30, i32, 1 << 31, d31, i32, 1 << 31);
    TESTINSN_bin("vmull.s8 q10, d30, d31", q10, d30, i32, 1 << 31, d31, i32, 1 << 30);
    TESTINSN_bin("vmull.u8 q0, d1, d12", q0, d1, i32, 0xabcd4, d12, i32, 0xcefab1);
    TESTINSN_bin("vmull.u8 q9, d11, d12", q9, d11, i32, 0x140, d12, i32, 0x120);
    TESTINSN_bin("vmull.u8 q4, d5, d6", q4, d5, i32, (1 << 14) + 1, d6, i32, (1 << 13) + 2);
    TESTINSN_bin("vmull.u8 q4, d5, d6", q4, d5, i32, (1 << 14) - 0xabcd, d6, i32, (1 << 13) + 0xaa2);
    TESTINSN_bin("vmull.u8 q4, d5, d6", q4, d5, i32, (1 << 28) + 0xfe, d6, i32, (1 << 13) + 0x2bbc2d);
    TESTINSN_bin("vmull.u8 q10, d30, d31", q10, d30, i32, 1 << 31, d31, i32, 1 << 31);
    TESTINSN_bin("vmull.u8 q10, d30, d31", q10, d30, i32, 1 << 31, d31, i32, 1 << 30);
    TESTINSN_bin("vmull.s16 q0, d1, d12", q0, d1, i32, 0xabcd4, d12, i32, 0xcefab1);
    TESTINSN_bin("vmull.s16 q9, d11, d12", q9, d11, i32, 0x140, d12, i32, 0x120);
    TESTINSN_bin("vmull.s16 q4, d5, d6", q4, d5, i32, (1 << 14) + 1, d6, i32, (1 << 13) + 2);
    TESTINSN_bin("vmull.s16 q4, d5, d6", q4, d5, i32, (1 << 14) - 0xabcd, d6, i32, (1 << 13) + 0xaa2);
    TESTINSN_bin("vmull.s16 q4, d5, d6", q4, d5, i32, (1 << 28) + 0xfe, d6, i32, (1 << 13) + 0x2bbc2d);
    TESTINSN_bin("vmull.s16 q10, d30, d31", q10, d30, i32, 1 << 31, d31, i32, 1 << 31);
    TESTINSN_bin("vmull.s16 q10, d30, d31", q10, d30, i32, 1 << 31, d31, i32, 1 << 30);
    TESTINSN_bin("vmull.u16 q0, d1, d12", q0, d1, i32, 0xabcd4, d12, i32, 0xcefab1);
    TESTINSN_bin("vmull.u16 q9, d11, d12", q9, d11, i32, 0x140, d12, i32, 0x120);
    TESTINSN_bin("vmull.u16 q4, d5, d6", q4, d5, i32, (1 << 14) + 1, d6, i32, (1 << 13) + 2);
    TESTINSN_bin("vmull.u16 q4, d5, d6", q4, d5, i32, (1 << 14) - 0xabcd, d6, i32, (1 << 13) + 0xaa2);
    TESTINSN_bin("vmull.u16 q4, d5, d6", q4, d5, i32, (1 << 28) + 0xfe, d6, i32, (1 << 13) + 0x2bbc2d);
    TESTINSN_bin("vmull.u16 q10, d30, d31", q10, d30, i32, 1 << 31, d31, i32, 1 << 31);
    TESTINSN_bin("vmull.u16 q10, d30, d31", q10, d30, i32, 1 << 31, d31, i32, 1 << 30);
    TESTINSN_bin("vmull.s32 q0, d1, d2", q0, d1, i32, 0xaabbcc4, d2, i32, 0x1b2c0a);
    TESTINSN_bin("vmull.s32 q6, d7, d8", q6, d7, i32, 140, d8, i32, -120);
    TESTINSN_bin("vmull.s32 q7, d8, d9", q7, d8, i32, (1 << 31) + 1, d9, i32, (1 << 31) + 2);
    TESTINSN_bin("vmull.s32 q7, d8, d9", q7, d8, i32, (1 << 31), d9, i32, 12);
    TESTINSN_bin("vmull.s32 q7, d8, d9", q7, d8, i32, (1 << 31) + 1, d9, i32, (1 << 31) + 2);
    TESTINSN_bin("vmull.s32 q10, d11, d15", q10, d11, i32, 24, d15, i32, 120);
    TESTINSN_bin("vmull.s32 q10, d30, d31", q10, d30, i32, 1 << 31, d31, i32, 1 << 31);
    TESTINSN_bin("vmull.s32 q10, d30, d31", q10, d30, i32, 1 << 30, d31, i32, 1 << 31);
    TESTINSN_bin("vmull.u32 q0, d1, d2", q0, d1, i32, 0xaabbcc4, d2, i32, 0x1b2c0a);
    TESTINSN_bin("vmull.u32 q6, d7, d8", q6, d7, i32, 140, d8, i32, -120);
    TESTINSN_bin("vmull.u32 q7, d8, d9", q7, d8, i32, (1 << 31) + 1, d9, i32, (1 << 31) + 2);
    TESTINSN_bin("vmull.u32 q7, d8, d9", q7, d8, i32, (1 << 31), d9, i32, 12);
    TESTINSN_bin("vmull.u32 q7, d8, d9", q7, d8, i32, (1 << 31) + 1, d9, i32, (1 << 31) + 2);
    TESTINSN_bin("vmull.u32 q10, d11, d15", q10, d11, i32, 24, d15, i32, 120);
    TESTINSN_bin("vmull.u32 q10, d30, d31", q10, d30, i32, 1 << 31, d31, i32, 1 << 31);
    TESTINSN_bin("vmull.u32 q10, d30, d31", q10, d30, i32, 1 << 30, d31, i32, 1 << 31);
    TESTINSN_bin("vmull.p8 q9, d11, d12", q9, d11, i32, 0x1a4b0c, d12, i32, 0xd1e2f0);
    TESTINSN_bin("vmull.p8 q4, d5, d6", q4, d5, i32, (1 << 14) + 1, d6, i32, (1 << 13) + 2);
    TESTINSN_bin("vmull.p8 q4, d15, d26", q4, d15, i32, (1 << 14) - 0xabcd, d26, i32, (1 << 13) + 0xaa2);
    TESTINSN_bin("vmull.p8 q14, d5, d6", q14, d5, i32, (1 << 28) + 0xefe, d6, i32, (1 << 13) + 0x2bbc2d);
    TESTINSN_bin("vmull.p8 q10, d30, d31", q10, d30, i32, 1 << 31, d31, i32, 1 << 31);
    TESTINSN_bin("vmull.p8 q10, d27, d31", q10, d27, i32, 1 << 31, d31, i32, 1 << 30);
    TESTINSN_bin("vmull.p8 q9, d11, d12", q9, d11, i32, 0x140, d12, i32, 0x120);
    TESTINSN_bin("vmull.p8 q4, d5, d6", q4, d5, i32, (1 << 28) + 0xfe, d6, i32, (1 << 13) + 0x2bbc2d);
    TESTINSN_bin("vmull.p8 q10, d30, d31", q10, d30, i32, 1 << 31, d31, i32, 1 << 30);

    printf("---- VMLAL ----\n");
    TESTINSN_bin("vmlal.s8 q0, d1, d12", q0, d1, i32, 0xabcd4, d12, i32, 0xcefab1);
    TESTINSN_bin("vmlal.s8 q9, d11, d12", q9, d11, i32, 0x140, d12, i32, 0x120);
    TESTINSN_bin("vmlal.s8 q4, d5, d6", q4, d5, i32, (1 << 14) + 1, d6, i32, (1 << 13) + 2);
    TESTINSN_bin("vmlal.s8 q4, d5, d6", q4, d5, i32, (1 << 14) - 0xabcd, d6, i32, (1 << 13) + 0xaa2);
    TESTINSN_bin("vmlal.s8 q4, d5, d6", q4, d5, i32, (1 << 28) + 0xfe, d6, i32, (1 << 13) + 0x2bbc2d);
    TESTINSN_bin("vmlal.s8 q10, d30, d31", q10, d30, i32, 1 << 31, d31, i32, 1 << 31);
    TESTINSN_bin("vmlal.s8 q10, d30, d31", q10, d30, i32, 1 << 31, d31, i32, 1 << 30);
    TESTINSN_bin("vmlal.u8 q0, d1, d12", q0, d1, i32, 0xabcd4, d12, i32, 0xcefab1);
    TESTINSN_bin("vmlal.u8 q9, d11, d12", q9, d11, i32, 0x140, d12, i32, 0x120);
    TESTINSN_bin("vmlal.u8 q4, d5, d6", q4, d5, i32, (1 << 14) + 1, d6, i32, (1 << 13) + 2);
    TESTINSN_bin("vmlal.u8 q4, d5, d6", q4, d5, i32, (1 << 14) - 0xabcd, d6, i32, (1 << 13) + 0xaa2);
    TESTINSN_bin("vmlal.u8 q4, d5, d6", q4, d5, i32, (1 << 28) + 0xfe, d6, i32, (1 << 13) + 0x2bbc2d);
    TESTINSN_bin("vmlal.u8 q10, d30, d31", q10, d30, i32, 1 << 31, d31, i32, 1 << 31);
    TESTINSN_bin("vmlal.u8 q10, d30, d31", q10, d30, i32, 1 << 31, d31, i32, 1 << 30);
    TESTINSN_bin("vmlal.s16 q0, d1, d12", q0, d1, i32, 0xabcd4, d12, i32, 0xcefab1);
    TESTINSN_bin("vmlal.s16 q9, d11, d12", q9, d11, i32, 0x140, d12, i32, 0x120);
    TESTINSN_bin("vmlal.s16 q4, d5, d6", q4, d5, i32, (1 << 14) + 1, d6, i32, (1 << 13) + 2);
    TESTINSN_bin("vmlal.s16 q4, d5, d6", q4, d5, i32, (1 << 14) - 0xabcd, d6, i32, (1 << 13) + 0xaa2);
    TESTINSN_bin("vmlal.s16 q4, d5, d6", q4, d5, i32, (1 << 28) + 0xfe, d6, i32, (1 << 13) + 0x2bbc2d);
    TESTINSN_bin("vmlal.s16 q10, d30, d31", q10, d30, i32, 1 << 31, d31, i32, 1 << 31);
    TESTINSN_bin("vmlal.s16 q10, d30, d31", q10, d30, i32, 1 << 31, d31, i32, 1 << 30);
    TESTINSN_bin("vmlal.u16 q0, d1, d12", q0, d1, i32, 0xabcd4, d12, i32, 0xcefab1);
    TESTINSN_bin("vmlal.u16 q9, d11, d12", q9, d11, i32, 0x140, d12, i32, 0x120);
    TESTINSN_bin("vmlal.u16 q4, d5, d6", q4, d5, i32, (1 << 14) + 1, d6, i32, (1 << 13) + 2);
    TESTINSN_bin("vmlal.u16 q4, d5, d6", q4, d5, i32, (1 << 14) - 0xabcd, d6, i32, (1 << 13) + 0xaa2);
    TESTINSN_bin("vmlal.u16 q4, d5, d6", q4, d5, i32, (1 << 28) + 0xfe, d6, i32, (1 << 13) + 0x2bbc2d);
    TESTINSN_bin("vmlal.u16 q10, d30, d31", q10, d30, i32, 1 << 31, d31, i32, 1 << 31);
    TESTINSN_bin("vmlal.u16 q10, d30, d31", q10, d30, i32, 1 << 31, d31, i32, 1 << 30);
    TESTINSN_bin("vmlal.s32 q0, d1, d2", q0, d1, i32, 0xaabbcc4, d2, i32, 0x1b2c0a);
    TESTINSN_bin("vmlal.s32 q6, d7, d8", q6, d7, i32, 140, d8, i32, -120);
    TESTINSN_bin("vmlal.s32 q7, d8, d9", q7, d8, i32, (1 << 31) + 1, d9, i32, (1 << 31) + 2);
    TESTINSN_bin("vmlal.s32 q7, d8, d9", q7, d8, i32, (1 << 31), d9, i32, 12);
    TESTINSN_bin("vmlal.s32 q7, d8, d9", q7, d8, i32, (1 << 31) + 1, d9, i32, (1 << 31) + 2);
    TESTINSN_bin("vmlal.s32 q10, d11, d15", q10, d11, i32, 24, d15, i32, 120);
    TESTINSN_bin("vmlal.s32 q10, d30, d31", q10, d30, i32, 1 << 31, d31, i32, 1 << 31);
    TESTINSN_bin("vmlal.s32 q10, d30, d31", q10, d30, i32, 1 << 30, d31, i32, 1 << 31);
    TESTINSN_bin("vmlal.u32 q0, d1, d2", q0, d1, i32, 0xaabbcc4, d2, i32, 0x1b2c0a);
    TESTINSN_bin("vmlal.u32 q6, d7, d8", q6, d7, i32, 140, d8, i32, -120);
    TESTINSN_bin("vmlal.u32 q7, d8, d9", q7, d8, i32, (1 << 31) + 1, d9, i32, (1 << 31) + 2);
    TESTINSN_bin("vmlal.u32 q7, d8, d9", q7, d8, i32, (1 << 31), d9, i32, 12);
    TESTINSN_bin("vmlal.u32 q7, d8, d9", q7, d8, i32, (1 << 31) + 1, d9, i32, (1 << 31) + 2);
    TESTINSN_bin("vmlal.u32 q10, d11, d15", q10, d11, i32, 24, d15, i32, 120);
    TESTINSN_bin("vmlal.u32 q10, d30, d31", q10, d30, i32, 1 << 31, d31, i32, 1 << 31);
    TESTINSN_bin("vmlal.u32 q10, d30, d31", q10, d30, i32, 1 << 30, d31, i32, 1 << 31);

    printf("---- VMLSL ----\n");
    TESTINSN_bin("vmlsl.s8 q0, d1, d12", q0, d1, i32, 0xabcd4, d12, i32, 0xcefab1);
    TESTINSN_bin("vmlsl.s8 q9, d11, d12", q9, d11, i32, 0x140, d12, i32, 0x120);
    TESTINSN_bin("vmlsl.s8 q4, d5, d6", q4, d5, i32, (1 << 14) + 1, d6, i32, (1 << 13) + 2);
    TESTINSN_bin("vmlsl.s8 q4, d5, d6", q4, d5, i32, (1 << 14) - 0xabcd, d6, i32, (1 << 13) + 0xaa2);
    TESTINSN_bin("vmlsl.s8 q4, d5, d6", q4, d5, i32, (1 << 28) + 0xfe, d6, i32, (1 << 13) + 0x2bbc2d);
    TESTINSN_bin("vmlsl.s8 q10, d30, d31", q10, d30, i32, 1 << 31, d31, i32, 1 << 31);
    TESTINSN_bin("vmlsl.s8 q10, d30, d31", q10, d30, i32, 1 << 31, d31, i32, 1 << 30);
    TESTINSN_bin("vmlsl.u8 q0, d1, d12", q0, d1, i32, 0xabcd4, d12, i32, 0xcefab1);
    TESTINSN_bin("vmlsl.u8 q9, d11, d12", q9, d11, i32, 0x140, d12, i32, 0x120);
    TESTINSN_bin("vmlsl.u8 q4, d5, d6", q4, d5, i32, (1 << 14) + 1, d6, i32, (1 << 13) + 2);
    TESTINSN_bin("vmlsl.u8 q4, d5, d6", q4, d5, i32, (1 << 14) - 0xabcd, d6, i32, (1 << 13) + 0xaa2);
    TESTINSN_bin("vmlsl.u8 q4, d5, d6", q4, d5, i32, (1 << 28) + 0xfe, d6, i32, (1 << 13) + 0x2bbc2d);
    TESTINSN_bin("vmlsl.u8 q10, d30, d31", q10, d30, i32, 1 << 31, d31, i32, 1 << 31);
    TESTINSN_bin("vmlsl.u8 q10, d30, d31", q10, d30, i32, 1 << 31, d31, i32, 1 << 30);
    TESTINSN_bin("vmlsl.s16 q0, d1, d12", q0, d1, i32, 0xabcd4, d12, i32, 0xcefab1);
    TESTINSN_bin("vmlsl.s16 q9, d11, d12", q9, d11, i32, 0x140, d12, i32, 0x120);
    TESTINSN_bin("vmlsl.s16 q4, d5, d6", q4, d5, i32, (1 << 14) + 1, d6, i32, (1 << 13) + 2);
    TESTINSN_bin("vmlsl.s16 q4, d5, d6", q4, d5, i32, (1 << 14) - 0xabcd, d6, i32, (1 << 13) + 0xaa2);
    TESTINSN_bin("vmlsl.s16 q4, d5, d6", q4, d5, i32, (1 << 28) + 0xfe, d6, i32, (1 << 13) + 0x2bbc2d);
    TESTINSN_bin("vmlsl.s16 q10, d30, d31", q10, d30, i32, 1 << 31, d31, i32, 1 << 31);
    TESTINSN_bin("vmlsl.s16 q10, d30, d31", q10, d30, i32, 1 << 31, d31, i32, 1 << 30);
    TESTINSN_bin("vmlsl.u16 q0, d1, d12", q0, d1, i32, 0xabcd4, d12, i32, 0xcefab1);
    TESTINSN_bin("vmlsl.u16 q9, d11, d12", q9, d11, i32, 0x140, d12, i32, 0x120);
    TESTINSN_bin("vmlsl.u16 q4, d5, d6", q4, d5, i32, (1 << 14) + 1, d6, i32, (1 << 13) + 2);
    TESTINSN_bin("vmlsl.u16 q4, d5, d6", q4, d5, i32, (1 << 14) - 0xabcd, d6, i32, (1 << 13) + 0xaa2);
    TESTINSN_bin("vmlsl.u16 q4, d5, d6", q4, d5, i32, (1 << 28) + 0xfe, d6, i32, (1 << 13) + 0x2bbc2d);
    TESTINSN_bin("vmlsl.u16 q10, d30, d31", q10, d30, i32, 1 << 31, d31, i32, 1 << 31);
    TESTINSN_bin("vmlsl.u16 q10, d30, d31", q10, d30, i32, 1 << 31, d31, i32, 1 << 30);
    TESTINSN_bin("vmlsl.s32 q0, d1, d2", q0, d1, i32, 0xaabbcc4, d2, i32, 0x1b2c0a);
    TESTINSN_bin("vmlsl.s32 q6, d7, d8", q6, d7, i32, 140, d8, i32, -120);
    TESTINSN_bin("vmlsl.s32 q7, d8, d9", q7, d8, i32, (1 << 31) + 1, d9, i32, (1 << 31) + 2);
    TESTINSN_bin("vmlsl.s32 q7, d8, d9", q7, d8, i32, (1 << 31), d9, i32, 12);
    TESTINSN_bin("vmlsl.s32 q7, d8, d9", q7, d8, i32, (1 << 31) + 1, d9, i32, (1 << 31) + 2);
    TESTINSN_bin("vmlsl.s32 q10, d11, d15", q10, d11, i32, 24, d15, i32, 120);
    TESTINSN_bin("vmlsl.s32 q10, d30, d31", q10, d30, i32, 1 << 31, d31, i32, 1 << 31);
    TESTINSN_bin("vmlsl.s32 q10, d30, d31", q10, d30, i32, 1 << 30, d31, i32, 1 << 31);
    TESTINSN_bin("vmlsl.u32 q0, d1, d2", q0, d1, i32, 0xaabbcc4, d2, i32, 0x1b2c0a);
    TESTINSN_bin("vmlsl.u32 q6, d7, d8", q6, d7, i32, 140, d8, i32, -120);
    TESTINSN_bin("vmlsl.u32 q7, d8, d9", q7, d8, i32, (1 << 31) + 1, d9, i32, (1 << 31) + 2);
    TESTINSN_bin("vmlsl.u32 q7, d8, d9", q7, d8, i32, (1 << 31), d9, i32, 12);
    TESTINSN_bin("vmlsl.u32 q7, d8, d9", q7, d8, i32, (1 << 31) + 1, d9, i32, (1 << 31) + 2);
    TESTINSN_bin("vmlsl.u32 q10, d11, d15", q10, d11, i32, 24, d15, i32, 120);
    TESTINSN_bin("vmlsl.u32 q10, d30, d31", q10, d30, i32, 1 << 31, d31, i32, 1 << 31);
    TESTINSN_bin("vmlsl.u32 q10, d30, d31", q10, d30, i32, 1 << 30, d31, i32, 1 << 31);

    printf("---- VQRDMULH ----\n");
    TESTINSN_bin_q("vqrdmulh.s32 q0, q1, q2", q0, q1, i32, 24, q2, i32, 120);
    TESTINSN_bin_q("vqrdmulh.s32 q6, q7, q8", q6, q7, i32, 140, q8, i32, -120);
    TESTINSN_bin_q("vqrdmulh.s16 q9, q11, q12", q9, q11, i32, 0x140, q12, i32, 0x120);
    TESTINSN_bin_q("vqrdmulh.s16 q4, q5, q6", q4, q5, i32, (1 << 14) + 1, q6, i32, (1 << 13) + 2);
    TESTINSN_bin_q("vqrdmulh.s32 q7, q8, q9", q7, q8, i32, (1 << 31) + 1, q9, i32, (1 << 31) + 2);
    TESTINSN_bin_q("vqrdmulh.s16 q4, q5, q6", q4, q5, i32, (1 << 14) - 0xabcd, q6, i32, (1 << 13) + 2);
    TESTINSN_bin_q("vqrdmulh.s32 q7, q8, q9", q7, q8, i32, (1 << 31), q9, i32, 12);
    TESTINSN_bin_q("vqrdmulh.s16 q4, q5, q6", q4, q5, i32, (1 << 28) + 0xfe, q6, i32, (1 << 13) + 2);
    TESTINSN_bin_q("vqrdmulh.s32 q7, q8, q9", q7, q8, i32, (1 << 31) + 1, q9, i32, (1 << 31) + 2);
    TESTINSN_bin_q("vqrdmulh.s32 q10, q11, q15", q10, q11, i32, 24, q15, i32, 120);
    TESTINSN_bin_q("vqrdmulh.s32 q10, q14, q15", q10, q14, i32, 1 << 31, q15, i32, 1 << 31);
    TESTINSN_bin_q("vqrdmulh.s32 q10, q14, q15", q10, q14, i32, 1 << 31, q15, i32, (1 << 31) + 1);
    TESTINSN_bin_q("vqrdmulh.s16 q10, q14, q15", q10, q14, i32, 1 << 31, q15, i32, 1 << 31);
    TESTINSN_bin_q("vqrdmulh.s32 q10, q14, q15", q10, q14, i32, 1 << 30, q15, i32, 1 << 31);
    TESTINSN_bin_q("vqrdmulh.s16 q10, q14, q15", q10, q14, i32, 1 << 31, q15, i32, 1 << 30);

    printf("---- VQRDMULH (by scalar) ----\n");
    TESTINSN_bin_q("vqrdmulh.s32 q0, q1, d6[0]", q0, q1, i32, 24, d6, i32, 120);
    TESTINSN_bin_q("vqrdmulh.s32 q6, q7, d1[1]", q6, q7, i32, 140, d1, i32, -120);
    TESTINSN_bin_q("vqrdmulh.s16 q9, q11, d7[0]", q9, q11, i32, 0x140, d7, i32, 0x120);
    TESTINSN_bin_q("vqrdmulh.s16 q4, q5, d6[0]", q4, q5, i32, (1 << 14) + 1, d6, i32, (1 << 13) + 2);
    TESTINSN_bin_q("vqrdmulh.s32 q7, q8, d9[1]", q7, q8, i32, (1 << 31) + 1, d9, i32, (1 << 31) + 2);
    TESTINSN_bin_q("vqrdmulh.s16 q4, q5, d6[1]", q4, q5, i32, (1 << 14) - 0xabcd, d6, i16, (1 << 13) + 2);
    TESTINSN_bin_q("vqrdmulh.s32 q7, q8, d9[0]", q7, q8, i32, (1 << 31), d9, i32, 12);
    TESTINSN_bin_q("vqrdmulh.s16 q4, q5, d6[2]", q4, q5, i32, (1 << 28) + 0xfe, d6, i32, (1 << 13) + 2);
    TESTINSN_bin_q("vqrdmulh.s32 q7, q8, d9[0]", q7, q8, i32, (1 << 31) + 1, d9, i32, (1 << 31) + 2);
    TESTINSN_bin_q("vqrdmulh.s32 q10, q11, d15[0]", q10, q11, i32, 24, d15, i32, 120);
    TESTINSN_bin_q("vqrdmulh.s32 q10, q14, d15[1]", q10, q14, i32, 1 << 31, d7, i32, 1 << 31);
    TESTINSN_bin_q("vqrdmulh.s16 q10, q14, d7[3]", q10, q14, i32, 1 << 31, q15, i32, (1 << 31) + 1);
    TESTINSN_bin_q("vqrdmulh.s32 q10, q14, d15[1]", q10, q14, i32, 1 << 30, d15, i32, 1 << 31);
    TESTINSN_bin_q("vqrdmulh.s16 q10, q14, d7[1]", q10, q14, i32, 1 << 31, d7, i32, 1 << 30);

    printf("---- VADD (fp) ----\n");
    TESTINSN_bin("vadd.f32 q0, q5, q2", q0, q5, i32, f2u(23.04), q2, i32, f2u(-45.5687));
    TESTINSN_bin("vadd.f32 q3, q4, q5", q3, q4, i32, f2u(-347856.475), q5, i32, f2u(1346));
    TESTINSN_bin("vadd.f32 q10, q11, q2", q10, q11, i32, f2u(48755), q2, i32, f2u(-45786.476));
    TESTINSN_bin("vadd.f32 q9, q5, q7", q9, q5, i32, f2u(95867.76), q7, i32, f2u(17065));
    TESTINSN_bin("vadd.f32 q0, q5, q2", q0, q5, i32, f2u(-45667.24), q2, i32, f2u(-248562.76));
    TESTINSN_bin("vadd.f32 q3, q4, q5", q3, q4, i32, f2u(24), q5, i32, f2u(1346));
    TESTINSN_bin("vadd.f32 q10, q11, q2", q10, q11, i32, f2u(48755), q2, i32, f2u(1089));
    TESTINSN_bin("vadd.f32 q9, q5, q7", q9, q5, i32, f2u(214), q7, i32, f2u(1752065));
    TESTINSN_bin("vadd.f32 q0, q11, q12", q0, q11, i32, f2u(356047.56), q12, i32, f2u(5867.009));
    TESTINSN_bin("vadd.f32 q7, q1, q6", q7, q1, i32, f2u(34.00046), q6, i32, f2u(0.0024575));
    TESTINSN_bin("vadd.f32 q0, q1, q2", q0, q1, i32, f2u(2754), q2, i32, f2u(107));
    TESTINSN_bin("vadd.f32 q3, q4, q5", q3, q4, i32, f2u(874), q5, i32, f2u(1384.6));
    TESTINSN_bin("vadd.f32 q10, q11, q2", q10, q11, i32, f2u(487.587), q2, i32, f2u(109));
    TESTINSN_bin("vadd.f32 q9, q5, q7", q9, q5, i32, f2u(2146), q7, i32, f2u(1752));
    TESTINSN_bin("vadd.f32 q0, q11, q12", q0, q11, i32, f2u(-56.25), q12, i32, f2u(-5786.47));
    TESTINSN_bin("vadd.f32 q7, q1, q6", q7, q1, i32, f2u(456.2489562), q6, i32, f2u(-7.2945676));
    TESTINSN_bin("vadd.f32 q0, q5, q2", q0, q5, i32, f2u(532.987), q2, i32, f2u(-0.0045876));
    TESTINSN_bin("vadd.f32 q10, q13, q15", q10, q13, i32, f2u(-485.2457), q15, i32, f2u(-567.245));
    TESTINSN_bin("vadd.f32 q10, q13, q15", q10, q13, i32, f2u(278456.45), q15, i32, f2u(8756.0076));
    TESTINSN_bin("vadd.f32 q0, q1, q2", q0, q1, i32, f2u(876988654), q2, i32, f2u(1224808797));
    TESTINSN_bin("vadd.f32 q0, q1, q2", q0, q1, i32, f2u(NAN), q2, i32, f2u(NAN));
    TESTINSN_bin("vadd.f32 q0, q1, q2", q0, q1, i32, f2u(NAN), q2, i32, f2u(1.0));
    TESTINSN_bin("vadd.f32 q0, q1, q2", q0, q1, i32, f2u(NAN), q2, i32, f2u(0.0));
    TESTINSN_bin("vadd.f32 q0, q1, q2", q0, q1, i32, f2u(NAN), q2, i32, f2u(INFINITY));
    TESTINSN_bin("vadd.f32 q0, q1, q2", q0, q1, i32, f2u(NAN), q2, i32, f2u(-INFINITY));
    TESTINSN_bin("vadd.f32 q0, q1, q2", q0, q1, i32, f2u(0.0), q2, i32, f2u(NAN));
    TESTINSN_bin("vadd.f32 q0, q1, q2", q0, q1, i32, f2u(0.0), q2, i32, f2u(1.0));
    TESTINSN_bin("vadd.f32 q0, q1, q2", q0, q1, i32, f2u(0.0), q2, i32, f2u(0.0));
    TESTINSN_bin("vadd.f32 q0, q1, q2", q0, q1, i32, f2u(0.0), q2, i32, f2u(INFINITY));
    TESTINSN_bin("vadd.f32 q0, q1, q2", q0, q1, i32, f2u(0.0), q2, i32, f2u(-INFINITY));
    TESTINSN_bin("vadd.f32 q0, q1, q2", q0, q1, i32, f2u(INFINITY), q2, i32, f2u(NAN));
    TESTINSN_bin("vadd.f32 q0, q1, q2", q0, q1, i32, f2u(INFINITY), q2, i32, f2u(1.0));
    TESTINSN_bin("vadd.f32 q0, q1, q2", q0, q1, i32, f2u(INFINITY), q2, i32, f2u(0.0));
    TESTINSN_bin("vadd.f32 q0, q1, q2", q0, q1, i32, f2u(INFINITY), q2, i32, f2u(INFINITY));
    TESTINSN_bin("vadd.f32 q0, q1, q2", q0, q1, i32, f2u(INFINITY), q2, i32, f2u(-INFINITY));
    TESTINSN_bin("vadd.f32 q0, q1, q2", q0, q1, i32, f2u(-INFINITY), q2, i32, f2u(NAN));
    TESTINSN_bin("vadd.f32 q0, q1, q2", q0, q1, i32, f2u(-INFINITY), q2, i32, f2u(1.0));
    TESTINSN_bin("vadd.f32 q0, q1, q2", q0, q1, i32, f2u(-INFINITY), q2, i32, f2u(0.0));
    TESTINSN_bin("vadd.f32 q0, q1, q2", q0, q1, i32, f2u(-INFINITY), q2, i32, f2u(INFINITY));
    TESTINSN_bin("vadd.f32 q0, q1, q2", q0, q1, i32, f2u(-INFINITY), q2, i32, f2u(-INFINITY));

    printf("---- VSUB (fp) ----\n");
    TESTINSN_bin("vsub.f32 q0, q5, q2", q0, q5, i32, f2u(23.04), q2, i32, f2u(-45.5687));
    TESTINSN_bin("vsub.f32 q3, q4, q5", q3, q4, i32, f2u(-347856.475), q5, i32, f2u(1346));
    TESTINSN_bin("vsub.f32 q10, q11, q2", q10, q11, i32, f2u(48755), q2, i32, f2u(-45786.476));
    TESTINSN_bin("vsub.f32 q9, q5, q7", q9, q5, i32, f2u(95867.76), q7, i32, f2u(17065));
    TESTINSN_bin("vsub.f32 q0, q5, q2", q0, q5, i32, f2u(-45667.24), q2, i32, f2u(-248562.76));
    TESTINSN_bin("vsub.f32 q3, q4, q5", q3, q4, i32, f2u(24.89), q5, i32, f2u(1346));
    TESTINSN_bin("vsub.f32 q10, q11, q2", q10, q11, i32, f2u(48755), q2, i32, f2u(1089));
    TESTINSN_bin("vsub.f32 q9, q5, q7", q9, q5, i32, f2u(214), q7, i32, f2u(1752065));
    TESTINSN_bin("vsub.f32 q0, q11, q12", q0, q11, i32, f2u(356047.56), q12, i32, f2u(5867.009));
    TESTINSN_bin("vsub.f32 q7, q1, q6", q7, q1, i32, f2u(34.00046), q6, i32, f2u(0.0024575));
    TESTINSN_bin("vsub.f32 q0, q1, q2", q0, q1, i32, f2u(2754), q2, i32, f2u(107));
    TESTINSN_bin("vsub.f32 q3, q4, q5", q3, q4, i32, f2u(874), q5, i32, f2u(1384.6));
    TESTINSN_bin("vsub.f32 q10, q11, q2", q10, q11, i32, f2u(487.587), q2, i32, f2u(109));
    TESTINSN_bin("vsub.f32 q9, q5, q7", q9, q5, i32, f2u(2146), q7, i32, f2u(1752));
    TESTINSN_bin("vsub.f32 q0, q11, q12", q0, q11, i32, f2u(-56.25), q12, i32, f2u(-5786.47));
    TESTINSN_bin("vsub.f32 q7, q1, q6", q7, q1, i32, f2u(456.2489562), q6, i32, f2u(-7.2945676));
    TESTINSN_bin("vsub.f32 q0, q5, q2", q0, q5, i32, f2u(532.987), q2, i32, f2u(-0.0045876));
    TESTINSN_bin("vsub.f32 q10, q13, q15", q10, q13, i32, f2u(-485.2457), q15, i32, f2u(-567.245));
    TESTINSN_bin("vsub.f32 q10, q13, q15", q10, q13, i32, f2u(278456.45), q15, i32, f2u(8756.0076));
    TESTINSN_bin("vsub.f32 q0, q1, q2", q0, q1, i32, f2u(876988654), q2, i32, f2u(1224808797));
    TESTINSN_bin("vsub.f32 q0, q1, q2", q0, q1, i32, f2u(NAN), q2, i32, f2u(NAN));
    TESTINSN_bin("vsub.f32 q0, q1, q2", q0, q1, i32, f2u(NAN), q2, i32, f2u(1.0));
    TESTINSN_bin("vsub.f32 q0, q1, q2", q0, q1, i32, f2u(NAN), q2, i32, f2u(0.0));
    TESTINSN_bin("vsub.f32 q0, q1, q2", q0, q1, i32, f2u(NAN), q2, i32, f2u(INFINITY));
    TESTINSN_bin("vsub.f32 q0, q1, q2", q0, q1, i32, f2u(NAN), q2, i32, f2u(-INFINITY));
    TESTINSN_bin("vsub.f32 q0, q1, q2", q0, q1, i32, f2u(0.0), q2, i32, f2u(NAN));
    TESTINSN_bin("vsub.f32 q0, q1, q2", q0, q1, i32, f2u(0.0), q2, i32, f2u(1.0));
    TESTINSN_bin("vsub.f32 q0, q1, q2", q0, q1, i32, f2u(0.0), q2, i32, f2u(0.0));
    TESTINSN_bin("vsub.f32 q0, q1, q2", q0, q1, i32, f2u(0.0), q2, i32, f2u(INFINITY));
    TESTINSN_bin("vsub.f32 q0, q1, q2", q0, q1, i32, f2u(0.0), q2, i32, f2u(-INFINITY));
    TESTINSN_bin("vsub.f32 q0, q1, q2", q0, q1, i32, f2u(INFINITY), q2, i32, f2u(NAN));
    TESTINSN_bin("vsub.f32 q0, q1, q2", q0, q1, i32, f2u(INFINITY), q2, i32, f2u(1.0));
    TESTINSN_bin("vsub.f32 q0, q1, q2", q0, q1, i32, f2u(INFINITY), q2, i32, f2u(0.0));
    TESTINSN_bin("vsub.f32 q0, q1, q2", q0, q1, i32, f2u(INFINITY), q2, i32, f2u(INFINITY));
    TESTINSN_bin("vsub.f32 q0, q1, q2", q0, q1, i32, f2u(INFINITY), q2, i32, f2u(-INFINITY));
    TESTINSN_bin("vsub.f32 q0, q1, q2", q0, q1, i32, f2u(-INFINITY), q2, i32, f2u(NAN));
    TESTINSN_bin("vsub.f32 q0, q1, q2", q0, q1, i32, f2u(-INFINITY), q2, i32, f2u(1.0));
    TESTINSN_bin("vsub.f32 q0, q1, q2", q0, q1, i32, f2u(-INFINITY), q2, i32, f2u(0.0));
    TESTINSN_bin("vsub.f32 q0, q1, q2", q0, q1, i32, f2u(-INFINITY), q2, i32, f2u(INFINITY));
    TESTINSN_bin("vsub.f32 q0, q1, q2", q0, q1, i32, f2u(-INFINITY), q2, i32, f2u(-INFINITY));

    printf("---- VABD (fp) ----\n");
    TESTINSN_bin("vabd.f32 q0, q5, q2", q0, q5, i32, f2u(23.04), q2, i32, f2u(-45.5687));
    TESTINSN_bin("vabd.f32 q3, q4, q5", q3, q4, i32, f2u(-347856.475), q5, i32, f2u(1346));
    TESTINSN_bin("vabd.f32 q10, q11, q2", q10, q11, i32, f2u(48755), q2, i32, f2u(-45786.476));
    TESTINSN_bin("vabd.f32 q9, q5, q7", q9, q5, i32, f2u(95867.76), q7, i32, f2u(17065));
    TESTINSN_bin("vabd.f32 q0, q5, q2", q0, q5, i32, f2u(-45667.24), q2, i32, f2u(-248562.76));
    TESTINSN_bin("vabd.f32 q3, q4, q5", q3, q4, i32, f2u(24), q5, i32, f2u(1346));
    TESTINSN_bin("vabd.f32 q10, q11, q2", q10, q11, i32, f2u(48755), q2, i32, f2u(1089));
    TESTINSN_bin("vabd.f32 q9, q5, q7", q9, q5, i32, f2u(214), q7, i32, f2u(1752065));
    TESTINSN_bin("vabd.f32 q0, q11, q12", q0, q11, i32, f2u(356047.56), q12, i32, f2u(5867.009));
    TESTINSN_bin("vabd.f32 q7, q1, q6", q7, q1, i32, f2u(34.00046), q6, i32, f2u(0.0024575));
    TESTINSN_bin("vabd.f32 q0, q1, q2", q0, q1, i32, f2u(2754), q2, i32, f2u(107));
    TESTINSN_bin("vabd.f32 q3, q4, q5", q3, q4, i32, f2u(874), q5, i32, f2u(1384.6));
    TESTINSN_bin("vabd.f32 q10, q11, q2", q10, q11, i32, f2u(487.587), q2, i32, f2u(109));
    TESTINSN_bin("vabd.f32 q9, q5, q7", q9, q5, i32, f2u(2146), q7, i32, f2u(1752));
    TESTINSN_bin("vabd.f32 q0, q11, q12", q0, q11, i32, f2u(-56.25), q12, i32, f2u(-5786.47));
    TESTINSN_bin("vabd.f32 q7, q1, q6", q7, q1, i32, f2u(456.2489562), q6, i32, f2u(-7.2945676));
    TESTINSN_bin("vabd.f32 q0, q5, q2", q0, q5, i32, f2u(532.987), q2, i32, f2u(-0.0045876));
    TESTINSN_bin("vabd.f32 q10, q13, q15", q10, q13, i32, f2u(-485.2457), q15, i32, f2u(-567.245));
    TESTINSN_bin("vabd.f32 q10, q13, q15", q10, q13, i32, f2u(278456.45), q15, i32, f2u(8756.0076));
    TESTINSN_bin("vabd.f32 q0, q1, q2", q0, q1, i32, f2u(876988654), q2, i32, f2u(1224808797));
    TESTINSN_bin("vabd.f32 q0, q1, q2", q0, q1, i32, f2u(NAN), q2, i32, f2u(NAN));
    TESTINSN_bin("vabd.f32 q0, q1, q2", q0, q1, i32, f2u(NAN), q2, i32, f2u(1.0));
    TESTINSN_bin("vabd.f32 q0, q1, q2", q0, q1, i32, f2u(NAN), q2, i32, f2u(0.0));
    TESTINSN_bin("vabd.f32 q0, q1, q2", q0, q1, i32, f2u(NAN), q2, i32, f2u(INFINITY));
    TESTINSN_bin("vabd.f32 q0, q1, q2", q0, q1, i32, f2u(NAN), q2, i32, f2u(-INFINITY));
    TESTINSN_bin("vabd.f32 q0, q1, q2", q0, q1, i32, f2u(0.0), q2, i32, f2u(NAN));
    TESTINSN_bin("vabd.f32 q0, q1, q2", q0, q1, i32, f2u(0.0), q2, i32, f2u(1.0));
    TESTINSN_bin("vabd.f32 q0, q1, q2", q0, q1, i32, f2u(0.0), q2, i32, f2u(0.0));
    TESTINSN_bin("vabd.f32 q0, q1, q2", q0, q1, i32, f2u(0.0), q2, i32, f2u(INFINITY));
    TESTINSN_bin("vabd.f32 q0, q1, q2", q0, q1, i32, f2u(0.0), q2, i32, f2u(-INFINITY));
    TESTINSN_bin("vabd.f32 q0, q1, q2", q0, q1, i32, f2u(INFINITY), q2, i32, f2u(NAN));
    TESTINSN_bin("vabd.f32 q0, q1, q2", q0, q1, i32, f2u(INFINITY), q2, i32, f2u(1.0));
    TESTINSN_bin("vabd.f32 q0, q1, q2", q0, q1, i32, f2u(INFINITY), q2, i32, f2u(0.0));
    TESTINSN_bin("vabd.f32 q0, q1, q2", q0, q1, i32, f2u(INFINITY), q2, i32, f2u(INFINITY));
    TESTINSN_bin("vabd.f32 q0, q1, q2", q0, q1, i32, f2u(INFINITY), q2, i32, f2u(-INFINITY));
    TESTINSN_bin("vabd.f32 q0, q1, q2", q0, q1, i32, f2u(-INFINITY), q2, i32, f2u(NAN));
    TESTINSN_bin("vabd.f32 q0, q1, q2", q0, q1, i32, f2u(-INFINITY), q2, i32, f2u(1.0));
    TESTINSN_bin("vabd.f32 q0, q1, q2", q0, q1, i32, f2u(-INFINITY), q2, i32, f2u(0.0));
    TESTINSN_bin("vabd.f32 q0, q1, q2", q0, q1, i32, f2u(-INFINITY), q2, i32, f2u(INFINITY));
    TESTINSN_bin("vabd.f32 q0, q1, q2", q0, q1, i32, f2u(-INFINITY), q2, i32, f2u(-INFINITY));

    printf("---- VMUL (fp) ----\n");
    TESTINSN_bin("vmul.f32 q0, q5, q2", q0, q5, i32, f2u(23.04), q2, i32, f2u(-45.5687));
    TESTINSN_bin("vmul.f32 q3, q4, q5", q3, q4, i32, f2u(-347856.475), q5, i32, f2u(1346));
    TESTINSN_bin("vmul.f32 q10, q11, q2", q10, q11, i32, f2u(48755), q2, i32, f2u(-45786.476));
    TESTINSN_bin("vmul.f32 q9, q5, q7", q9, q5, i32, f2u(95867.76), q7, i32, f2u(17065));
    TESTINSN_bin("vmul.f32 q0, q5, q2", q0, q5, i32, f2u(-45667.24), q2, i32, f2u(-248562.76));
    TESTINSN_bin("vmul.f32 q3, q4, q5", q3, q4, i32, f2u(24), q5, i32, f2u(1346));
    TESTINSN_bin("vmul.f32 q10, q11, q2", q10, q11, i32, f2u(48755), q2, i32, f2u(1089));
    TESTINSN_bin("vmul.f32 q9, q5, q7", q9, q5, i32, f2u(214), q7, i32, f2u(1752065));
    TESTINSN_bin("vmul.f32 q0, q11, q12", q0, q11, i32, f2u(356047.56), q12, i32, f2u(5867.009));
    TESTINSN_bin("vmul.f32 q7, q1, q6", q7, q1, i32, f2u(34.00046), q6, i32, f2u(0.0024575));
    TESTINSN_bin("vmul.f32 q0, q1, q2", q0, q1, i32, f2u(2754), q2, i32, f2u(107));
    TESTINSN_bin("vmul.f32 q3, q4, q5", q3, q4, i32, f2u(874), q5, i32, f2u(1384.6));
    TESTINSN_bin("vmul.f32 q10, q11, q2", q10, q11, i32, f2u(487.587), q2, i32, f2u(109));
    TESTINSN_bin("vmul.f32 q9, q5, q7", q9, q5, i32, f2u(2146), q7, i32, f2u(1752));
    TESTINSN_bin("vmul.f32 q0, q11, q12", q0, q11, i32, f2u(-56.25), q12, i32, f2u(-5786.47));
    TESTINSN_bin("vmul.f32 q7, q1, q6", q7, q1, i32, f2u(456.2489562), q6, i32, f2u(-7.2945676));
    TESTINSN_bin("vmul.f32 q0, q5, q2", q0, q5, i32, f2u(532.987), q2, i32, f2u(-0.0045876));
    TESTINSN_bin("vmul.f32 q10, q13, q15", q10, q13, i32, f2u(-485.2457), q15, i32, f2u(-567.245));
    TESTINSN_bin("vmul.f32 q10, q13, q15", q10, q13, i32, f2u(278456.45), q15, i32, f2u(8756.0076));
    TESTINSN_bin("vmul.f32 q0, q1, q2", q0, q1, i32, f2u(876988654), q2, i32, f2u(1224808797));
    TESTINSN_bin("vmul.f32 q0, q1, q2", q0, q1, i32, f2u(NAN), q2, i32, f2u(NAN));
    TESTINSN_bin("vmul.f32 q0, q1, q2", q0, q1, i32, f2u(NAN), q2, i32, f2u(1.0));
    TESTINSN_bin("vmul.f32 q0, q1, q2", q0, q1, i32, f2u(NAN), q2, i32, f2u(0.0));
    TESTINSN_bin("vmul.f32 q0, q1, q2", q0, q1, i32, f2u(NAN), q2, i32, f2u(INFINITY));
    TESTINSN_bin("vmul.f32 q0, q1, q2", q0, q1, i32, f2u(NAN), q2, i32, f2u(-INFINITY));
    TESTINSN_bin("vmul.f32 q0, q1, q2", q0, q1, i32, f2u(0.0), q2, i32, f2u(NAN));
    TESTINSN_bin("vmul.f32 q0, q1, q2", q0, q1, i32, f2u(0.0), q2, i32, f2u(1.0));
    TESTINSN_bin("vmul.f32 q0, q1, q2", q0, q1, i32, f2u(0.0), q2, i32, f2u(0.0));
    TESTINSN_bin("vmul.f32 q0, q1, q2", q0, q1, i32, f2u(0.0), q2, i32, f2u(INFINITY));
    TESTINSN_bin("vmul.f32 q0, q1, q2", q0, q1, i32, f2u(0.0), q2, i32, f2u(-INFINITY));
    TESTINSN_bin("vmul.f32 q0, q1, q2", q0, q1, i32, f2u(INFINITY), q2, i32, f2u(NAN));
    TESTINSN_bin("vmul.f32 q0, q1, q2", q0, q1, i32, f2u(INFINITY), q2, i32, f2u(1.0));
    TESTINSN_bin("vmul.f32 q0, q1, q2", q0, q1, i32, f2u(INFINITY), q2, i32, f2u(0.0));
    TESTINSN_bin("vmul.f32 q0, q1, q2", q0, q1, i32, f2u(INFINITY), q2, i32, f2u(INFINITY));
    TESTINSN_bin("vmul.f32 q0, q1, q2", q0, q1, i32, f2u(INFINITY), q2, i32, f2u(-INFINITY));
    TESTINSN_bin("vmul.f32 q0, q1, q2", q0, q1, i32, f2u(-INFINITY), q2, i32, f2u(NAN));
    TESTINSN_bin("vmul.f32 q0, q1, q2", q0, q1, i32, f2u(-INFINITY), q2, i32, f2u(1.0));
    TESTINSN_bin("vmul.f32 q0, q1, q2", q0, q1, i32, f2u(-INFINITY), q2, i32, f2u(0.0));
    TESTINSN_bin("vmul.f32 q0, q1, q2", q0, q1, i32, f2u(-INFINITY), q2, i32, f2u(INFINITY));
    TESTINSN_bin("vmul.f32 q0, q1, q2", q0, q1, i32, f2u(-INFINITY), q2, i32, f2u(-INFINITY));

    printf("---- VMLA (fp) ----\n");
    TESTINSN_bin_f("vmla.f32 q0, q5, q2", q0, q5, i32, f2u(23.04), q2, i32, f2u(-45.5687));
    TESTINSN_bin_f("vmla.f32 q3, q4, q5", q3, q4, i32, f2u(-347856.475), q5, i32, f2u(1346));
    TESTINSN_bin_f("vmla.f32 q10, q11, q2", q10, q11, i32, f2u(48755), q2, i32, f2u(-45786.476));
    TESTINSN_bin_f("vmla.f32 q9, q5, q7", q9, q5, i32, f2u(95867.76), q7, i32, f2u(17065));
    TESTINSN_bin_f("vmla.f32 q0, q5, q2", q0, q5, i32, f2u(-45667.24), q2, i32, f2u(-248562.76));
    TESTINSN_bin_f("vmla.f32 q3, q4, q5", q3, q4, i32, f2u(24), q5, i32, f2u(1346));
    TESTINSN_bin_f("vmla.f32 q10, q11, q2", q10, q11, i32, f2u(48755), q2, i32, f2u(1089));
    TESTINSN_bin_f("vmla.f32 q9, q5, q7", q9, q5, i32, f2u(214), q7, i32, f2u(1752065));
    TESTINSN_bin_f("vmla.f32 q0, q11, q12", q0, q11, i32, f2u(356047.56), q12, i32, f2u(5867.009));
    TESTINSN_bin_f("vmla.f32 q7, q1, q6", q7, q1, i32, f2u(34.00046), q6, i32, f2u(0.0024575));
    TESTINSN_bin_f("vmla.f32 q0, q1, q2", q0, q1, i32, f2u(2754), q2, i32, f2u(107));
    TESTINSN_bin_f("vmla.f32 q3, q4, q5", q3, q4, i32, f2u(874), q5, i32, f2u(1384.6));
    TESTINSN_bin_f("vmla.f32 q10, q11, q2", q10, q11, i32, f2u(487.587), q2, i32, f2u(109));
    TESTINSN_bin_f("vmla.f32 q9, q5, q7", q9, q5, i32, f2u(2146), q7, i32, f2u(1752));
    TESTINSN_bin_f("vmla.f32 q0, q11, q12", q0, q11, i32, f2u(-56.25), q12, i32, f2u(-5786.47));
    TESTINSN_bin_f("vmla.f32 q7, q1, q6", q7, q1, i32, f2u(456.2489562), q6, i32, f2u(-7.2945676));
    TESTINSN_bin_f("vmla.f32 q0, q5, q2", q0, q5, i32, f2u(532.987), q2, i32, f2u(-0.0045876));
    TESTINSN_bin_f("vmla.f32 q10, q13, q15", q10, q13, i32, f2u(-485.2457), q15, i32, f2u(-567.245));
    TESTINSN_bin_f("vmla.f32 q10, q13, q15", q10, q13, i32, f2u(278456.45), q15, i32, f2u(8756.0076));
    TESTINSN_bin_f("vmla.f32 q0, q1, q2", q0, q1, i32, f2u(876988654), q2, i32, f2u(1224808797));
    TESTINSN_bin_f("vmla.f32 q0, q1, q2", q0, q1, i32, f2u(NAN), q2, i32, f2u(NAN));
    TESTINSN_bin_f("vmla.f32 q0, q1, q2", q0, q1, i32, f2u(NAN), q2, i32, f2u(1.0));
    TESTINSN_bin_f("vmla.f32 q0, q1, q2", q0, q1, i32, f2u(NAN), q2, i32, f2u(0.0));
    TESTINSN_bin_f("vmla.f32 q0, q1, q2", q0, q1, i32, f2u(NAN), q2, i32, f2u(INFINITY));
    TESTINSN_bin_f("vmla.f32 q0, q1, q2", q0, q1, i32, f2u(NAN), q2, i32, f2u(-INFINITY));
    TESTINSN_bin_f("vmla.f32 q0, q1, q2", q0, q1, i32, f2u(0.0), q2, i32, f2u(NAN));
    TESTINSN_bin_f("vmla.f32 q0, q1, q2", q0, q1, i32, f2u(0.0), q2, i32, f2u(1.0));
    TESTINSN_bin_f("vmla.f32 q0, q1, q2", q0, q1, i32, f2u(0.0), q2, i32, f2u(0.0));
    TESTINSN_bin_f("vmla.f32 q0, q1, q2", q0, q1, i32, f2u(0.0), q2, i32, f2u(INFINITY));
    TESTINSN_bin_f("vmla.f32 q0, q1, q2", q0, q1, i32, f2u(0.0), q2, i32, f2u(-INFINITY));
    TESTINSN_bin_f("vmla.f32 q0, q1, q2", q0, q1, i32, f2u(INFINITY), q2, i32, f2u(NAN));
    TESTINSN_bin_f("vmla.f32 q0, q1, q2", q0, q1, i32, f2u(INFINITY), q2, i32, f2u(1.0));
    TESTINSN_bin_f("vmla.f32 q0, q1, q2", q0, q1, i32, f2u(INFINITY), q2, i32, f2u(0.0));
    TESTINSN_bin_f("vmla.f32 q0, q1, q2", q0, q1, i32, f2u(INFINITY), q2, i32, f2u(INFINITY));
    TESTINSN_bin_f("vmla.f32 q0, q1, q2", q0, q1, i32, f2u(INFINITY), q2, i32, f2u(-INFINITY));
    TESTINSN_bin_f("vmla.f32 q0, q1, q2", q0, q1, i32, f2u(-INFINITY), q2, i32, f2u(NAN));
    TESTINSN_bin_f("vmla.f32 q0, q1, q2", q0, q1, i32, f2u(-INFINITY), q2, i32, f2u(1.0));
    TESTINSN_bin_f("vmla.f32 q0, q1, q2", q0, q1, i32, f2u(-INFINITY), q2, i32, f2u(0.0));
    TESTINSN_bin_f("vmla.f32 q0, q1, q2", q0, q1, i32, f2u(-INFINITY), q2, i32, f2u(INFINITY));
    TESTINSN_bin_f("vmla.f32 q0, q1, q2", q0, q1, i32, f2u(-INFINITY), q2, i32, f2u(-INFINITY));

    printf("---- VMLA (fp by scalar) ----\n");
    TESTINSN_bin_f("vmla.f32 q0, q1, d4[0]", q0, q1, i32, f2u(24), d4, i32, f2u(120));
    TESTINSN_bin_f("vmla.f32 q15, q8, d7[1]", q15, q8, i32, f2u(140), d7, i32, f2u(-120));
    TESTINSN_bin_f("vmla.f32 q4, q8, d15[1]", q4, q8, i32, (1 << 31) + 1, d15, i32, (1 << 31) + 2);
    TESTINSN_bin_f("vmla.f32 q7, q8, d1[1]", q7, q8, i32, (1 << 31), d1, i16, 12);
    TESTINSN_bin_f("vmla.f32 q7, q8, d1[1]", q7, q8, i32, (1 << 31) + 1, d1, i32, (1 << 31) + 2);
    TESTINSN_bin_f("vmla.f32 q7, q8, d1[0]", q7, q8, i32, f2u(1e22), d1, i32, f2u(1e-19));
    TESTINSN_bin_f("vmla.f32 q7, q8, d1[0]", q7, q8, i32, f2u(1e12), d1, i32, f2u(1e11));
    TESTINSN_bin_f("vmla.f32 q0, q1, d2[0]", q0, q1, i32, f2u(NAN), d2, i32, f2u(NAN));
    TESTINSN_bin_f("vmla.f32 q0, q1, d2[0]", q0, q1, i32, f2u(NAN), d2, i32, f2u(1.0));
    TESTINSN_bin_f("vmla.f32 q0, q1, d2[0]", q0, q1, i32, f2u(NAN), d2, i32, f2u(0.0));
    TESTINSN_bin_f("vmla.f32 q0, q1, d2[0]", q0, q1, i32, f2u(NAN), d2, i32, f2u(INFINITY));
    TESTINSN_bin_f("vmla.f32 q0, q1, d2[0]", q0, q1, i32, f2u(NAN), d2, i32, f2u(-INFINITY));
    TESTINSN_bin_f("vmla.f32 q0, q1, d2[0]", q0, q1, i32, f2u(0.0), d2, i32, f2u(NAN));
    TESTINSN_bin_f("vmla.f32 q0, q1, d2[0]", q0, q1, i32, f2u(0.0), d2, i32, f2u(1.0));
    TESTINSN_bin_f("vmla.f32 q0, q1, d2[0]", q0, q1, i32, f2u(0.0), d2, i32, f2u(0.0));
    TESTINSN_bin_f("vmla.f32 q0, q1, d2[0]", q0, q1, i32, f2u(0.0), d2, i32, f2u(INFINITY));
    TESTINSN_bin_f("vmla.f32 q0, q1, d2[0]", q0, q1, i32, f2u(0.0), d2, i32, f2u(-INFINITY));
    TESTINSN_bin_f("vmla.f32 q0, q1, d2[0]", q0, q1, i32, f2u(INFINITY), d2, i32, f2u(NAN));
    TESTINSN_bin_f("vmla.f32 q0, q1, d2[0]", q0, q1, i32, f2u(INFINITY), d2, i32, f2u(1.0));
    TESTINSN_bin_f("vmla.f32 q0, q1, d2[0]", q0, q1, i32, f2u(INFINITY), d2, i32, f2u(0.0));
    TESTINSN_bin_f("vmla.f32 q0, q1, d2[0]", q0, q1, i32, f2u(INFINITY), d2, i32, f2u(INFINITY));
    TESTINSN_bin_f("vmla.f32 q0, q1, d2[0]", q0, q1, i32, f2u(INFINITY), d2, i32, f2u(-INFINITY));
    TESTINSN_bin_f("vmla.f32 q0, q1, d2[0]", q0, q1, i32, f2u(-INFINITY), d2, i32, f2u(NAN));
    TESTINSN_bin_f("vmla.f32 q0, q1, d2[0]", q0, q1, i32, f2u(-INFINITY), d2, i32, f2u(1.0));
    TESTINSN_bin_f("vmla.f32 q0, q1, d2[0]", q0, q1, i32, f2u(-INFINITY), d2, i32, f2u(0.0));
    TESTINSN_bin_f("vmla.f32 q0, q1, d2[0]", q0, q1, i32, f2u(-INFINITY), d2, i32, f2u(INFINITY));
    TESTINSN_bin_f("vmla.f32 q0, q1, d2[0]", q0, q1, i32, f2u(-INFINITY), d2, i32, f2u(-INFINITY));

    printf("---- VMLS (fp) ----\n");
    TESTINSN_bin_f("vmls.f32 q0, q5, q2", q0, q5, i32, f2u(23.04), q2, i32, f2u(-45.5687));
    TESTINSN_bin_f("vmls.f32 q3, q4, q5", q3, q4, i32, f2u(-347856.475), q5, i32, f2u(1346));
    TESTINSN_bin_f("vmls.f32 q10, q11, q2", q10, q11, i32, f2u(48755), q2, i32, f2u(-45786.476));
    TESTINSN_bin_f("vmls.f32 q9, q5, q7", q9, q5, i32, f2u(95867.76), q7, i32, f2u(17065));
    TESTINSN_bin_f("vmls.f32 q0, q5, q2", q0, q5, i32, f2u(-45667.24), q2, i32, f2u(-248562.76));
    TESTINSN_bin_f("vmls.f32 q3, q4, q5", q3, q4, i32, f2u(24), q5, i32, f2u(1346));
    TESTINSN_bin_f("vmls.f32 q10, q11, q2", q10, q11, i32, f2u(48755), q2, i32, f2u(1089));
    TESTINSN_bin_f("vmls.f32 q9, q5, q7", q9, q5, i32, f2u(214), q7, i32, f2u(1752065));
    TESTINSN_bin_f("vmls.f32 q0, q11, q12", q0, q11, i32, f2u(356047.56), q12, i32, f2u(5867.009));
    TESTINSN_bin_f("vmls.f32 q7, q1, q6", q7, q1, i32, f2u(34.00046), q6, i32, f2u(0.0024575));
    TESTINSN_bin_f("vmls.f32 q0, q1, q2", q0, q1, i32, f2u(2754), q2, i32, f2u(107));
    TESTINSN_bin_f("vmls.f32 q3, q4, q5", q3, q4, i32, f2u(874), q5, i32, f2u(1384.6));
    TESTINSN_bin_f("vmls.f32 q10, q11, q2", q10, q11, i32, f2u(487.587), q2, i32, f2u(109));
    TESTINSN_bin_f("vmls.f32 q9, q5, q7", q9, q5, i32, f2u(2146), q7, i32, f2u(1752));
    TESTINSN_bin_f("vmls.f32 q0, q11, q12", q0, q11, i32, f2u(-56.25), q12, i32, f2u(-5786.47));
    TESTINSN_bin_f("vmls.f32 q7, q1, q6", q7, q1, i32, f2u(456.2489562), q6, i32, f2u(-7.2945676));
    TESTINSN_bin_f("vmls.f32 q0, q5, q2", q0, q5, i32, f2u(532.987), q2, i32, f2u(-0.0045876));
    TESTINSN_bin_f("vmls.f32 q10, q13, q15", q10, q13, i32, f2u(-485.2457), q15, i32, f2u(-567.245));
    TESTINSN_bin_f("vmls.f32 q10, q13, q15", q10, q13, i32, f2u(278456.45), q15, i32, f2u(8756.0076));
    TESTINSN_bin_f("vmls.f32 q0, q1, q2", q0, q1, i32, f2u(876988654), q2, i32, f2u(1224808797));
    TESTINSN_bin_f("vmls.f32 q0, q1, q2", q0, q1, i32, f2u(NAN), q2, i32, f2u(NAN));
    TESTINSN_bin_f("vmls.f32 q0, q1, q2", q0, q1, i32, f2u(NAN), q2, i32, f2u(1.0));
    TESTINSN_bin_f("vmls.f32 q0, q1, q2", q0, q1, i32, f2u(NAN), q2, i32, f2u(0.0));
    TESTINSN_bin_f("vmls.f32 q0, q1, q2", q0, q1, i32, f2u(NAN), q2, i32, f2u(INFINITY));
    TESTINSN_bin_f("vmls.f32 q0, q1, q2", q0, q1, i32, f2u(NAN), q2, i32, f2u(-INFINITY));
    TESTINSN_bin_f("vmls.f32 q0, q1, q2", q0, q1, i32, f2u(0.0), q2, i32, f2u(NAN));
    TESTINSN_bin_f("vmls.f32 q0, q1, q2", q0, q1, i32, f2u(0.0), q2, i32, f2u(1.0));
    TESTINSN_bin_f("vmls.f32 q0, q1, q2", q0, q1, i32, f2u(0.0), q2, i32, f2u(0.0));
    TESTINSN_bin_f("vmls.f32 q0, q1, q2", q0, q1, i32, f2u(0.0), q2, i32, f2u(INFINITY));
    TESTINSN_bin_f("vmls.f32 q0, q1, q2", q0, q1, i32, f2u(0.0), q2, i32, f2u(-INFINITY));
    TESTINSN_bin_f("vmls.f32 q0, q1, q2", q0, q1, i32, f2u(INFINITY), q2, i32, f2u(NAN));
    TESTINSN_bin_f("vmls.f32 q0, q1, q2", q0, q1, i32, f2u(INFINITY), q2, i32, f2u(1.0));
    TESTINSN_bin_f("vmls.f32 q0, q1, q2", q0, q1, i32, f2u(INFINITY), q2, i32, f2u(0.0));
    TESTINSN_bin_f("vmls.f32 q0, q1, q2", q0, q1, i32, f2u(INFINITY), q2, i32, f2u(INFINITY));
    TESTINSN_bin_f("vmls.f32 q0, q1, q2", q0, q1, i32, f2u(INFINITY), q2, i32, f2u(-INFINITY));
    TESTINSN_bin_f("vmls.f32 q0, q1, q2", q0, q1, i32, f2u(-INFINITY), q2, i32, f2u(NAN));
    TESTINSN_bin_f("vmls.f32 q0, q1, q2", q0, q1, i32, f2u(-INFINITY), q2, i32, f2u(1.0));
    TESTINSN_bin_f("vmls.f32 q0, q1, q2", q0, q1, i32, f2u(-INFINITY), q2, i32, f2u(0.0));
    TESTINSN_bin_f("vmls.f32 q0, q1, q2", q0, q1, i32, f2u(-INFINITY), q2, i32, f2u(INFINITY));
    TESTINSN_bin_f("vmls.f32 q0, q1, q2", q0, q1, i32, f2u(-INFINITY), q2, i32, f2u(-INFINITY));

    printf("---- VMLS (fp by scalar) ----\n");
    TESTINSN_bin_f("vmls.f32 q0, q1, d4[0]", q0, q1, i32, f2u(24), d4, i32, f2u(120));
    TESTINSN_bin_f("vmls.f32 q15, q8, d7[1]", q15, q8, i32, f2u(140), d7, i32, f2u(-120));
    TESTINSN_bin_f("vmls.f32 q4, q8, d15[1]", q4, q8, i32, (1 << 31) + 1, d15, i32, (1 << 31) + 2);
    TESTINSN_bin_f("vmls.f32 q7, q8, d1[1]", q7, q8, i32, (1 << 31), d1, i16, 12);
    TESTINSN_bin_f("vmls.f32 q7, q8, d1[1]", q7, q8, i32, (1 << 31) + 1, d1, i32, (1 << 31) + 2);
    TESTINSN_bin_f("vmls.f32 q7, q8, d1[0]", q7, q8, i32, f2u(1e22), d1, i32, f2u(1e-19));
    TESTINSN_bin_f("vmls.f32 q7, q8, d1[0]", q7, q8, i32, f2u(1e12), d1, i32, f2u(1e11));
    TESTINSN_bin_f("vmls.f32 q0, q1, d2[0]", q0, q1, i32, f2u(NAN), d2, i32, f2u(NAN));
    TESTINSN_bin_f("vmls.f32 q0, q1, d2[0]", q0, q1, i32, f2u(NAN), d2, i32, f2u(1.0));
    TESTINSN_bin_f("vmls.f32 q0, q1, d2[0]", q0, q1, i32, f2u(NAN), d2, i32, f2u(0.0));
    TESTINSN_bin_f("vmls.f32 q0, q1, d2[0]", q0, q1, i32, f2u(NAN), d2, i32, f2u(INFINITY));
    TESTINSN_bin_f("vmls.f32 q0, q1, d2[0]", q0, q1, i32, f2u(NAN), d2, i32, f2u(-INFINITY));
    TESTINSN_bin_f("vmls.f32 q0, q1, d2[0]", q0, q1, i32, f2u(0.0), d2, i32, f2u(NAN));
    TESTINSN_bin_f("vmls.f32 q0, q1, d2[0]", q0, q1, i32, f2u(0.0), d2, i32, f2u(1.0));
    TESTINSN_bin_f("vmls.f32 q0, q1, d2[0]", q0, q1, i32, f2u(0.0), d2, i32, f2u(0.0));
    TESTINSN_bin_f("vmls.f32 q0, q1, d2[0]", q0, q1, i32, f2u(0.0), d2, i32, f2u(INFINITY));
    TESTINSN_bin_f("vmls.f32 q0, q1, d2[0]", q0, q1, i32, f2u(0.0), d2, i32, f2u(-INFINITY));
    TESTINSN_bin_f("vmls.f32 q0, q1, d2[0]", q0, q1, i32, f2u(INFINITY), d2, i32, f2u(NAN));
    TESTINSN_bin_f("vmls.f32 q0, q1, d2[0]", q0, q1, i32, f2u(INFINITY), d2, i32, f2u(1.0));
    TESTINSN_bin_f("vmls.f32 q0, q1, d2[0]", q0, q1, i32, f2u(INFINITY), d2, i32, f2u(0.0));
    TESTINSN_bin_f("vmls.f32 q0, q1, d2[0]", q0, q1, i32, f2u(INFINITY), d2, i32, f2u(INFINITY));
    TESTINSN_bin_f("vmls.f32 q0, q1, d2[0]", q0, q1, i32, f2u(INFINITY), d2, i32, f2u(-INFINITY));
    TESTINSN_bin_f("vmls.f32 q0, q1, d2[0]", q0, q1, i32, f2u(-INFINITY), d2, i32, f2u(NAN));
    TESTINSN_bin_f("vmls.f32 q0, q1, d2[0]", q0, q1, i32, f2u(-INFINITY), d2, i32, f2u(1.0));
    TESTINSN_bin_f("vmls.f32 q0, q1, d2[0]", q0, q1, i32, f2u(-INFINITY), d2, i32, f2u(0.0));
    TESTINSN_bin_f("vmls.f32 q0, q1, d2[0]", q0, q1, i32, f2u(-INFINITY), d2, i32, f2u(INFINITY));
    TESTINSN_bin_f("vmls.f32 q0, q1, d2[0]", q0, q1, i32, f2u(-INFINITY), d2, i32, f2u(-INFINITY));

    printf("---- VCVT (integer <-> fp) ----\n");
    TESTINSN_un("vcvt.u32.f32 q0, q1", q0, q1, i32, f2u(3.2));
    TESTINSN_un("vcvt.u32.f32 q10, q11", q10, q11, i32, f2u(3e22));
    TESTINSN_un("vcvt.u32.f32 q15, q4", q15, q4, i32, f2u(3e9));
    TESTINSN_un("vcvt.u32.f32 q15, q4", q15, q4, i32, f2u(-0.5));
    TESTINSN_un("vcvt.u32.f32 q15, q4", q15, q4, i32, f2u(-7.1));
    TESTINSN_un("vcvt.u32.f32 q12, q8", q12, q8, i32, f2u(8.0 - 1.0/1024.0));
    TESTINSN_un("vcvt.u32.f32 q12, q8", q12, q8, i32, f2u(-8.0 + 1.0/1024.0));
    TESTINSN_un("vcvt.s32.f32 q0, q1", q0, q1, i32, f2u(3.2));
    TESTINSN_un("vcvt.s32.f32 q10, q11", q10, q11, i32, f2u(3e22));
    TESTINSN_un("vcvt.s32.f32 q15, q4", q15, q4, i32, f2u(3e9));
    TESTINSN_un("vcvt.s32.f32 q15, q4", q15, q4, i32, f2u(-0.5));
    TESTINSN_un("vcvt.s32.f32 q15, q4", q15, q4, i32, f2u(-7.1));
    TESTINSN_un("vcvt.s32.f32 q12, q8", q12, q8, i32, f2u(8.0 - 1.0/1024.0));
    TESTINSN_un("vcvt.s32.f32 q12, q8", q12, q8, i32, f2u(-8.0 + 1.0/1024.0));
    TESTINSN_un("vcvt.f32.u32 q0, q1", q0, q1, i32, 7);
    TESTINSN_un("vcvt.f32.u32 q10, q11", q10, q11, i32, 1 << 31);
    TESTINSN_un("vcvt.f32.u32 q0, q1", q0, q1, i32, (1U << 31) + 1);
    TESTINSN_un("vcvt.f32.u32 q0, q1", q0, q1, i32, (1U << 31) - 1);
    TESTINSN_un("vcvt.f32.u32 q0, q14", q0, q14, i32, 0x30a0bcef);
    TESTINSN_un("vcvt.f32.s32 q0, q1", q0, q1, i32, 7);
    TESTINSN_un("vcvt.f32.s32 q10, q11", q10, q11, i32, 1 << 31);
    TESTINSN_un("vcvt.f32.s32 q0, q1", q0, q1, i32, (1U << 31) + 1);
    TESTINSN_un("vcvt.f32.s32 q0, q1", q0, q1, i32, (1U << 31) - 1);
    TESTINSN_un("vcvt.f32.s32 q0, q14", q0, q14, i32, 0x30a0bcef);
    TESTINSN_un("vcvt.u32.f32 q0, q1", q0, q1, i32, f2u(NAN));
    TESTINSN_un("vcvt.u32.f32 q0, q1", q0, q1, i32, f2u(0.0));
    TESTINSN_un("vcvt.u32.f32 q0, q1", q0, q1, i32, f2u(INFINITY));
    TESTINSN_un("vcvt.u32.f32 q0, q1", q0, q1, i32, f2u(-INFINITY));
    TESTINSN_un("vcvt.s32.f32 q0, q1", q0, q1, i32, f2u(NAN));
    TESTINSN_un("vcvt.s32.f32 q0, q1", q0, q1, i32, f2u(0.0));
    TESTINSN_un("vcvt.s32.f32 q0, q1", q0, q1, i32, f2u(INFINITY));
    TESTINSN_un("vcvt.s32.f32 q0, q1", q0, q1, i32, f2u(-INFINITY));

    printf("---- VCVT (fixed <-> fp) ----\n");
    TESTINSN_un("vcvt.u32.f32 q0, q1, #3", q0, q1, i32, f2u(3.2));
    TESTINSN_un("vcvt.u32.f32 q10, q11, #1", q10, q11, i32, f2u(3e22));
    TESTINSN_un("vcvt.u32.f32 q15, q4, #32", q15, q4, i32, f2u(3e9));
    TESTINSN_un("vcvt.u32.f32 q15, q4, #7", q15, q4, i32, f2u(-0.5));
    TESTINSN_un("vcvt.u32.f32 q15, q4, #4", q15, q4, i32, f2u(-7.1));
    TESTINSN_un("vcvt.u32.f32 q12, q8, #3", q12, q8, i32, f2u(8.0 - 1.0/1024.0));
    TESTINSN_un("vcvt.u32.f32 q12, q8, #3", q12, q8, i32, f2u(-8.0 + 1.0/1024.0));
    TESTINSN_un("vcvt.s32.f32 q0, q1, #5", q0, q1, i32, f2u(3.2));
    TESTINSN_un("vcvt.s32.f32 q10, q11, #1", q10, q11, i32, f2u(3e22));
    TESTINSN_un("vcvt.s32.f32 q15, q4, #8", q15, q4, i32, f2u(3e9));
    TESTINSN_un("vcvt.s32.f32 q15, q4, #2", q15, q4, i32, f2u(-0.5));
    TESTINSN_un("vcvt.s32.f32 q15, q4, #1", q15, q4, i32, f2u(-7.1));
    TESTINSN_un("vcvt.s32.f32 q12, q8, #2", q12, q8, i32, f2u(8.0 - 1.0/1024.0));
    TESTINSN_un("vcvt.s32.f32 q12, q8, #2", q12, q8, i32, f2u(-8.0 + 1.0/1024.0));
    TESTINSN_un("vcvt.f32.u32 q0, q1, #5", q0, q1, i32, 7);
    TESTINSN_un("vcvt.f32.u32 q10, q11, #9", q10, q11, i32, 1 << 31);
    TESTINSN_un("vcvt.f32.u32 q0, q1, #4", q0, q1, i32, (1U << 31) + 1);
    TESTINSN_un("vcvt.f32.u32 q0, q1, #6", q0, q1, i32, (1U << 31) - 1);
    TESTINSN_un("vcvt.f32.u32 q0, q14, #5", q0, q14, i32, 0x30a0bcef);
    TESTINSN_un("vcvt.f32.s32 q0, q1, #12", q0, q1, i32, 7);
    TESTINSN_un("vcvt.f32.s32 q10, q11, #8", q10, q11, i32, 1 << 31);
    TESTINSN_un("vcvt.f32.s32 q0, q1, #2", q0, q1, i32, (1U << 31) + 1);
    TESTINSN_un("vcvt.f32.s32 q0, q1, #1", q0, q1, i32, (1U << 31) - 1);
    TESTINSN_un("vcvt.f32.s32 q0, q14, #6", q0, q14, i32, 0x30a0bcef);
    TESTINSN_un("vcvt.u32.f32 q0, q1, #3", q0, q1, i32, f2u(NAN));
    TESTINSN_un("vcvt.u32.f32 q0, q1, #3", q0, q1, i32, f2u(0.0));
    TESTINSN_un("vcvt.u32.f32 q0, q1, #3", q0, q1, i32, f2u(INFINITY));
    TESTINSN_un("vcvt.u32.f32 q0, q1, #3", q0, q1, i32, f2u(-INFINITY));
    TESTINSN_un("vcvt.s32.f32 q0, q1, #3", q0, q1, i32, f2u(NAN));
    TESTINSN_un("vcvt.s32.f32 q0, q1, #3", q0, q1, i32, f2u(0.0));
    TESTINSN_un("vcvt.s32.f32 q0, q1, #3", q0, q1, i32, f2u(INFINITY));
    TESTINSN_un("vcvt.s32.f32 q0, q1, #3", q0, q1, i32, f2u(-INFINITY));

    printf("---- VMAX (fp) ----\n");
    TESTINSN_bin("vmax.f32 q0, q5, q2", q0, q5, i32, f2u(23.04), q2, i32, f2u(-45.5687));
    TESTINSN_bin("vmax.f32 q3, q4, q5", q3, q4, i32, f2u(-347856.475), q5, i32, f2u(1346));
    TESTINSN_bin("vmax.f32 q10, q11, q2", q10, q11, i32, f2u(48755), q2, i32, f2u(-45786.476));
    TESTINSN_bin("vmax.f32 q9, q5, q7", q9, q5, i32, f2u(95867.76), q7, i32, f2u(17065));
    TESTINSN_bin("vmax.f32 q0, q5, q2", q0, q5, i32, f2u(-45667.24), q2, i32, f2u(-248562.76));
    TESTINSN_bin("vmax.f32 q3, q4, q5", q3, q4, i32, f2u(24), q5, i32, f2u(1346));
    TESTINSN_bin("vmax.f32 q10, q11, q2", q10, q11, i32, f2u(48755), q2, i32, f2u(1089));
    TESTINSN_bin("vmax.f32 q9, q5, q7", q9, q5, i32, f2u(214), q7, i32, f2u(1752065));
    TESTINSN_bin("vmax.f32 q0, q11, q12", q0, q11, i32, f2u(356047.56), q12, i32, f2u(5867.009));
    TESTINSN_bin("vmax.f32 q7, q1, q6", q7, q1, i32, f2u(34.00046), q6, i32, f2u(0.0024575));
    TESTINSN_bin("vmax.f32 q0, q1, q2", q0, q1, i32, f2u(2754), q2, i32, f2u(107));
    TESTINSN_bin("vmax.f32 q3, q4, q5", q3, q4, i32, f2u(874), q5, i32, f2u(1384.6));
    TESTINSN_bin("vmax.f32 q10, q11, q2", q10, q11, i32, f2u(487.587), q2, i32, f2u(109));
    TESTINSN_bin("vmax.f32 q9, q5, q7", q9, q5, i32, f2u(2146), q7, i32, f2u(1752));
    TESTINSN_bin("vmax.f32 q0, q11, q12", q0, q11, i32, f2u(-56.25), q12, i32, f2u(-5786.47));
    TESTINSN_bin("vmax.f32 q7, q1, q6", q7, q1, i32, f2u(456.2489562), q6, i32, f2u(-7.2945676));
    TESTINSN_bin("vmax.f32 q0, q5, q2", q0, q5, i32, f2u(532.987), q2, i32, f2u(-0.0045876));
    TESTINSN_bin("vmax.f32 q10, q13, q15", q10, q13, i32, f2u(-485.2457), q15, i32, f2u(-567.245));
    TESTINSN_bin("vmax.f32 q10, q13, q15", q10, q13, i32, f2u(278456.45), q15, i32, f2u(8756.0076));
    TESTINSN_bin("vmax.f32 q0, q1, q2", q0, q1, i32, f2u(876988654), q2, i32, f2u(1224808797));
    TESTINSN_bin("vmax.f32 q0, q1, q2", q0, q1, i32, f2u(0), q2, i32, f2u(0));
    TESTINSN_bin("vmax.f32 q0, q1, q2", q0, q1, i32, f2u(1.0/1024.0), q2, i32, f2u(-1.0/1024.0));
    TESTINSN_bin("vmax.f32 q0, q1, q2", q0, q1, i32, f2u(-1.0/1024.0), q2, i32, f2u(1.0/1024.0));
    TESTINSN_bin("vmax.f32 q0, q1, q2", q0, q1, i32, f2u(2342+1.0/1024.0), q2, i32, f2u(2342-1.0/1024.0));
    TESTINSN_bin("vmax.f32 q0, q1, q2", q0, q1, i32, f2u(-2342+1.0/1024.0), q2, i32, f2u(-2342-1.0/1024.0));
    TESTINSN_bin("vmax.f32 q0, q1, q2", q0, q1, i32, f2u(89276+1.0/1024.0), q2, i32, f2u(89276+1.0/1024.0));
    TESTINSN_bin("vmax.f32 q0, q1, q2", q0, q1, i32, f2u(NAN), q2, i32, f2u(NAN));
    TESTINSN_bin("vmax.f32 q0, q1, q2", q0, q1, i32, f2u(NAN), q2, i32, f2u(1.0));
    TESTINSN_bin("vmax.f32 q0, q1, q2", q0, q1, i32, f2u(NAN), q2, i32, f2u(0.0));
    TESTINSN_bin("vmax.f32 q0, q1, q2", q0, q1, i32, f2u(NAN), q2, i32, f2u(INFINITY));
    TESTINSN_bin("vmax.f32 q0, q1, q2", q0, q1, i32, f2u(NAN), q2, i32, f2u(-INFINITY));
    TESTINSN_bin("vmax.f32 q0, q1, q2", q0, q1, i32, f2u(0.0), q2, i32, f2u(NAN));
    TESTINSN_bin("vmax.f32 q0, q1, q2", q0, q1, i32, f2u(0.0), q2, i32, f2u(1.0));
    TESTINSN_bin("vmax.f32 q0, q1, q2", q0, q1, i32, f2u(0.0), q2, i32, f2u(0.0));
    TESTINSN_bin("vmax.f32 q0, q1, q2", q0, q1, i32, f2u(0.0), q2, i32, f2u(INFINITY));
    TESTINSN_bin("vmax.f32 q0, q1, q2", q0, q1, i32, f2u(0.0), q2, i32, f2u(-INFINITY));
    TESTINSN_bin("vmax.f32 q0, q1, q2", q0, q1, i32, f2u(INFINITY), q2, i32, f2u(NAN));
    TESTINSN_bin("vmax.f32 q0, q1, q2", q0, q1, i32, f2u(INFINITY), q2, i32, f2u(1.0));
    TESTINSN_bin("vmax.f32 q0, q1, q2", q0, q1, i32, f2u(INFINITY), q2, i32, f2u(0.0));
    TESTINSN_bin("vmax.f32 q0, q1, q2", q0, q1, i32, f2u(INFINITY), q2, i32, f2u(INFINITY));
    TESTINSN_bin("vmax.f32 q0, q1, q2", q0, q1, i32, f2u(INFINITY), q2, i32, f2u(-INFINITY));
    TESTINSN_bin("vmax.f32 q0, q1, q2", q0, q1, i32, f2u(-INFINITY), q2, i32, f2u(NAN));
    TESTINSN_bin("vmax.f32 q0, q1, q2", q0, q1, i32, f2u(-INFINITY), q2, i32, f2u(1.0));
    TESTINSN_bin("vmax.f32 q0, q1, q2", q0, q1, i32, f2u(-INFINITY), q2, i32, f2u(0.0));
    TESTINSN_bin("vmax.f32 q0, q1, q2", q0, q1, i32, f2u(-INFINITY), q2, i32, f2u(INFINITY));
    TESTINSN_bin("vmax.f32 q0, q1, q2", q0, q1, i32, f2u(-INFINITY), q2, i32, f2u(-INFINITY));

    printf("---- VMIN (fp) ----\n");
    TESTINSN_bin("vmin.f32 q0, q5, q2", q0, q5, i32, f2u(23.04), q2, i32, f2u(-45.5687));
    TESTINSN_bin("vmin.f32 q3, q4, q5", q3, q4, i32, f2u(-347856.475), q5, i32, f2u(1346));
    TESTINSN_bin("vmin.f32 q10, q11, q2", q10, q11, i32, f2u(48755), q2, i32, f2u(-45786.476));
    TESTINSN_bin("vmin.f32 q9, q5, q7", q9, q5, i32, f2u(95867.76), q7, i32, f2u(17065));
    TESTINSN_bin("vmin.f32 q0, q5, q2", q0, q5, i32, f2u(-45667.24), q2, i32, f2u(-248562.76));
    TESTINSN_bin("vmin.f32 q3, q4, q5", q3, q4, i32, f2u(24), q5, i32, f2u(1346));
    TESTINSN_bin("vmin.f32 q10, q11, q2", q10, q11, i32, f2u(48755), q2, i32, f2u(1089));
    TESTINSN_bin("vmin.f32 q9, q5, q7", q9, q5, i32, f2u(214), q7, i32, f2u(1752065));
    TESTINSN_bin("vmin.f32 q0, q11, q12", q0, q11, i32, f2u(356047.56), q12, i32, f2u(5867.009));
    TESTINSN_bin("vmin.f32 q7, q1, q6", q7, q1, i32, f2u(34.00046), q6, i32, f2u(0.0024575));
    TESTINSN_bin("vmin.f32 q0, q1, q2", q0, q1, i32, f2u(2754), q2, i32, f2u(107));
    TESTINSN_bin("vmin.f32 q3, q4, q5", q3, q4, i32, f2u(874), q5, i32, f2u(1384.6));
    TESTINSN_bin("vmin.f32 q10, q11, q2", q10, q11, i32, f2u(487.587), q2, i32, f2u(109));
    TESTINSN_bin("vmin.f32 q9, q5, q7", q9, q5, i32, f2u(2146), q7, i32, f2u(1752));
    TESTINSN_bin("vmin.f32 q0, q11, q12", q0, q11, i32, f2u(-56.25), q12, i32, f2u(-5786.47));
    TESTINSN_bin("vmin.f32 q7, q1, q6", q7, q1, i32, f2u(456.2489562), q6, i32, f2u(-7.2945676));
    TESTINSN_bin("vmin.f32 q0, q5, q2", q0, q5, i32, f2u(532.987), q2, i32, f2u(-0.0045876));
    TESTINSN_bin("vmin.f32 q10, q13, q15", q10, q13, i32, f2u(-485.2457), q15, i32, f2u(-567.245));
    TESTINSN_bin("vmin.f32 q10, q13, q15", q10, q13, i32, f2u(278456.45), q15, i32, f2u(8756.0076));
    TESTINSN_bin("vmin.f32 q0, q1, q2", q0, q1, i32, f2u(876988654), q2, i32, f2u(1224808797));
    TESTINSN_bin("vmin.f32 q0, q1, q2", q0, q1, i32, f2u(0), q2, i32, f2u(0));
    TESTINSN_bin("vmin.f32 q0, q1, q2", q0, q1, i32, f2u(1.0/1024.0), q2, i32, f2u(-1.0/1024.0));
    TESTINSN_bin("vmin.f32 q0, q1, q2", q0, q1, i32, f2u(-1.0/1024.0), q2, i32, f2u(1.0/1024.0));
    TESTINSN_bin("vmin.f32 q0, q1, q2", q0, q1, i32, f2u(2342+1.0/1024.0), q2, i32, f2u(2342-1.0/1024.0));
    TESTINSN_bin("vmin.f32 q0, q1, q2", q0, q1, i32, f2u(-2342+1.0/1024.0), q2, i32, f2u(-2342-1.0/1024.0));
    TESTINSN_bin("vmin.f32 q0, q1, q2", q0, q1, i32, f2u(89276+1.0/1024.0), q2, i32, f2u(89276+1.0/1024.0));
    TESTINSN_bin("vmin.f32 q0, q1, q2", q0, q1, i32, f2u(NAN), q2, i32, f2u(NAN));
    TESTINSN_bin("vmin.f32 q0, q1, q2", q0, q1, i32, f2u(NAN), q2, i32, f2u(1.0));
    TESTINSN_bin("vmin.f32 q0, q1, q2", q0, q1, i32, f2u(NAN), q2, i32, f2u(0.0));
    TESTINSN_bin("vmin.f32 q0, q1, q2", q0, q1, i32, f2u(NAN), q2, i32, f2u(INFINITY));
    TESTINSN_bin("vmin.f32 q0, q1, q2", q0, q1, i32, f2u(NAN), q2, i32, f2u(-INFINITY));
    TESTINSN_bin("vmin.f32 q0, q1, q2", q0, q1, i32, f2u(0.0), q2, i32, f2u(NAN));
    TESTINSN_bin("vmin.f32 q0, q1, q2", q0, q1, i32, f2u(0.0), q2, i32, f2u(1.0));
    TESTINSN_bin("vmin.f32 q0, q1, q2", q0, q1, i32, f2u(0.0), q2, i32, f2u(0.0));
    TESTINSN_bin("vmin.f32 q0, q1, q2", q0, q1, i32, f2u(0.0), q2, i32, f2u(INFINITY));
    TESTINSN_bin("vmin.f32 q0, q1, q2", q0, q1, i32, f2u(0.0), q2, i32, f2u(-INFINITY));
    TESTINSN_bin("vmin.f32 q0, q1, q2", q0, q1, i32, f2u(INFINITY), q2, i32, f2u(NAN));
    TESTINSN_bin("vmin.f32 q0, q1, q2", q0, q1, i32, f2u(INFINITY), q2, i32, f2u(1.0));
    TESTINSN_bin("vmin.f32 q0, q1, q2", q0, q1, i32, f2u(INFINITY), q2, i32, f2u(0.0));
    TESTINSN_bin("vmin.f32 q0, q1, q2", q0, q1, i32, f2u(INFINITY), q2, i32, f2u(INFINITY));
    TESTINSN_bin("vmin.f32 q0, q1, q2", q0, q1, i32, f2u(INFINITY), q2, i32, f2u(-INFINITY));
    TESTINSN_bin("vmin.f32 q0, q1, q2", q0, q1, i32, f2u(-INFINITY), q2, i32, f2u(NAN));
    TESTINSN_bin("vmin.f32 q0, q1, q2", q0, q1, i32, f2u(-INFINITY), q2, i32, f2u(1.0));
    TESTINSN_bin("vmin.f32 q0, q1, q2", q0, q1, i32, f2u(-INFINITY), q2, i32, f2u(0.0));
    TESTINSN_bin("vmin.f32 q0, q1, q2", q0, q1, i32, f2u(-INFINITY), q2, i32, f2u(INFINITY));
    TESTINSN_bin("vmin.f32 q0, q1, q2", q0, q1, i32, f2u(-INFINITY), q2, i32, f2u(-INFINITY));

    printf("---- VRECPE ----\n");
    TESTINSN_un("vrecpe.u32 q0, q1", q0, q1, i32, f2u(3.2));
    TESTINSN_un("vrecpe.u32 q10, q11", q10, q11, i32, f2u(3e22));
    TESTINSN_un("vrecpe.u32 q15, q4", q15, q4, i32, f2u(3e9));
    TESTINSN_un("vrecpe.u32 q15, q4", q15, q4, i32, f2u(-0.5));
    TESTINSN_un("vrecpe.u32 q15, q4", q15, q4, i32, f2u(-7.1));
    TESTINSN_un("vrecpe.u32 q12, q8", q12, q8, i32, f2u(8.0 - 1.0/1024.0));
    TESTINSN_un("vrecpe.u32 q12, q8", q12, q8, i32, f2u(-8.0 + 1.0/1024.0));
    TESTINSN_un("vrecpe.u32 q0, q1", q0, q1, i32, f2u(3.2));
    TESTINSN_un("vrecpe.u32 q10, q11", q10, q11, i32, f2u(3e22));
    TESTINSN_un("vrecpe.u32 q15, q4", q15, q4, i32, f2u(3e9));
    TESTINSN_un("vrecpe.f32 q15, q4", q15, q4, i32, f2u(-0.5));
    TESTINSN_un("vrecpe.f32 q15, q4", q15, q4, i32, f2u(-7.1));
    TESTINSN_un("vrecpe.f32 q12, q8", q12, q8, i32, f2u(8.0 - 1.0/1024.0));
    TESTINSN_un("vrecpe.f32 q12, q8", q12, q8, i32, f2u(-8.0 + 1.0/1024.0));
    TESTINSN_un("vrecpe.f32 q0, q1", q0, q1, i32, 7);
    TESTINSN_un("vrecpe.f32 q10, q11", q10, q11, i32, 1 << 31);
    TESTINSN_un("vrecpe.f32 q0, q1", q0, q1, i32, (1U << 31) + 1);
    TESTINSN_un("vrecpe.f32 q0, q1", q0, q1, i32, (1U << 31) - 1);
    TESTINSN_un("vrecpe.f32 q0, q14", q0, q14, i32, 0x30a0bcef);
    TESTINSN_un("vrecpe.f32 q0, q1", q0, q1, i32, 7);
    TESTINSN_un("vrecpe.f32 q10, q11", q10, q11, i32, 1 << 31);
    TESTINSN_un("vrecpe.f32 q0, q1", q0, q1, i32, (1U << 31) + 1);
    TESTINSN_un("vrecpe.f32 q0, q1", q0, q1, i32, (1U << 31) - 1);
    TESTINSN_un("vrecpe.f32 q0, q14", q0, q14, i32, 0x30a0bcef);
    TESTINSN_un("vrecpe.f32 q0, q1", q0, q1, i32, f2u(NAN));
    TESTINSN_un("vrecpe.f32 q0, q1", q0, q1, i32, f2u(0.0));
    TESTINSN_un("vrecpe.f32 q0, q1", q0, q1, i32, f2u(INFINITY));
    TESTINSN_un("vrecpe.f32 q0, q1", q0, q1, i32, f2u(-INFINITY));
    TESTINSN_un("vrecpe.f32 q0, q1", q0, q1, i32, f2u(NAN));
    TESTINSN_un("vrecpe.f32 q0, q1", q0, q1, i32, f2u(0.0));
    TESTINSN_un("vrecpe.f32 q0, q1", q0, q1, i32, f2u(INFINITY));
    TESTINSN_un("vrecpe.f32 q0, q1", q0, q1, i32, f2u(-INFINITY));

    printf("---- VRECPS ----\n");
    TESTINSN_bin("vrecps.f32 q0, q5, q2", q0, q5, i32, f2u(23.04), q2, i32, f2u(-45.5687));
    TESTINSN_bin("vrecps.f32 q3, q4, q5", q3, q4, i32, f2u(-347856.475), q5, i32, f2u(1346));
    TESTINSN_bin("vrecps.f32 q10, q11, q2", q10, q11, i32, f2u(48755), q2, i32, f2u(-45786.476));
    TESTINSN_bin("vrecps.f32 q9, q5, q7", q9, q5, i32, f2u(95867.76), q7, i32, f2u(17065));
    TESTINSN_bin("vrecps.f32 q0, q5, q2", q0, q5, i32, f2u(-45667.24), q2, i32, f2u(-248562.76));
    TESTINSN_bin("vrecps.f32 q3, q4, q5", q3, q4, i32, f2u(24), q5, i32, f2u(1346));
    TESTINSN_bin("vrecps.f32 q10, q11, q2", q10, q11, i32, f2u(48755), q2, i32, f2u(1089));
    TESTINSN_bin("vrecps.f32 q9, q5, q7", q9, q5, i32, f2u(214), q7, i32, f2u(1752065));
    TESTINSN_bin("vrecps.f32 q0, q11, q12", q0, q11, i32, f2u(356047.56), q12, i32, f2u(5867.009));
    TESTINSN_bin("vrecps.f32 q7, q1, q6", q7, q1, i32, f2u(34.00046), q6, i32, f2u(0.0024575));
    TESTINSN_bin("vrecps.f32 q0, q1, q2", q0, q1, i32, f2u(2754), q2, i32, f2u(107));
    TESTINSN_bin("vrecps.f32 q3, q4, q5", q3, q4, i32, f2u(874), q5, i32, f2u(1384.6));
    TESTINSN_bin("vrecps.f32 q10, q11, q2", q10, q11, i32, f2u(487.587), q2, i32, f2u(109));
    TESTINSN_bin("vrecps.f32 q9, q5, q7", q9, q5, i32, f2u(2146), q7, i32, f2u(1752));
    TESTINSN_bin("vrecps.f32 q0, q11, q12", q0, q11, i32, f2u(-56.25), q12, i32, f2u(-5786.47));
    TESTINSN_bin("vrecps.f32 q7, q1, q6", q7, q1, i32, f2u(456.2489562), q6, i32, f2u(-7.2945676));
    TESTINSN_bin("vrecps.f32 q0, q5, q2", q0, q5, i32, f2u(532.987), q2, i32, f2u(-0.0045876));
    TESTINSN_bin("vrecps.f32 q10, q13, q15", q10, q13, i32, f2u(-485.2457), q15, i32, f2u(-567.245));
    TESTINSN_bin("vrecps.f32 q10, q13, q15", q10, q13, i32, f2u(278456.45), q15, i32, f2u(8756.0076));
    TESTINSN_bin("vrecps.f32 q0, q1, q2", q0, q1, i32, f2u(876988654), q2, i32, f2u(1224808797));
    TESTINSN_bin("vrecps.f32 q0, q1, q2", q0, q1, i32, f2u(NAN), q2, i32, f2u(NAN));
    TESTINSN_bin("vrecps.f32 q0, q1, q2", q0, q1, i32, f2u(NAN), q2, i32, f2u(1.0));
    TESTINSN_bin("vrecps.f32 q0, q1, q2", q0, q1, i32, f2u(NAN), q2, i32, f2u(0.0));
    TESTINSN_bin("vrecps.f32 q0, q1, q2", q0, q1, i32, f2u(NAN), q2, i32, f2u(INFINITY));
    TESTINSN_bin("vrecps.f32 q0, q1, q2", q0, q1, i32, f2u(NAN), q2, i32, f2u(-INFINITY));
    TESTINSN_bin("vrecps.f32 q0, q1, q2", q0, q1, i32, f2u(0.0), q2, i32, f2u(NAN));
    TESTINSN_bin("vrecps.f32 q0, q1, q2", q0, q1, i32, f2u(0.0), q2, i32, f2u(1.0));
    TESTINSN_bin("vrecps.f32 q0, q1, q2", q0, q1, i32, f2u(0.0), q2, i32, f2u(0.0));
    TESTINSN_bin("vrecps.f32 q0, q1, q2", q0, q1, i32, f2u(0.0), q2, i32, f2u(INFINITY));
    TESTINSN_bin("vrecps.f32 q0, q1, q2", q0, q1, i32, f2u(0.0), q2, i32, f2u(-INFINITY));
    TESTINSN_bin("vrecps.f32 q0, q1, q2", q0, q1, i32, f2u(INFINITY), q2, i32, f2u(NAN));
    TESTINSN_bin("vrecps.f32 q0, q1, q2", q0, q1, i32, f2u(INFINITY), q2, i32, f2u(1.0));
    TESTINSN_bin("vrecps.f32 q0, q1, q2", q0, q1, i32, f2u(INFINITY), q2, i32, f2u(0.0));
    TESTINSN_bin("vrecps.f32 q0, q1, q2", q0, q1, i32, f2u(INFINITY), q2, i32, f2u(INFINITY));
    TESTINSN_bin("vrecps.f32 q0, q1, q2", q0, q1, i32, f2u(INFINITY), q2, i32, f2u(-INFINITY));
    TESTINSN_bin("vrecps.f32 q0, q1, q2", q0, q1, i32, f2u(-INFINITY), q2, i32, f2u(NAN));
    TESTINSN_bin("vrecps.f32 q0, q1, q2", q0, q1, i32, f2u(-INFINITY), q2, i32, f2u(1.0));
    TESTINSN_bin("vrecps.f32 q0, q1, q2", q0, q1, i32, f2u(-INFINITY), q2, i32, f2u(0.0));
    TESTINSN_bin("vrecps.f32 q0, q1, q2", q0, q1, i32, f2u(-INFINITY), q2, i32, f2u(INFINITY));
    TESTINSN_bin("vrecps.f32 q0, q1, q2", q0, q1, i32, f2u(-INFINITY), q2, i32, f2u(-INFINITY));

    printf("---- VABS (fp) ----\n");
    TESTINSN_un("vabs.f32 q0, q1", q0, q1, i32, f2u(3.2));
    TESTINSN_un("vabs.f32 q10, q11", q10, q11, i32, f2u(3e22));
    TESTINSN_un("vabs.f32 q15, q4", q15, q4, i32, f2u(3e9));
    TESTINSN_un("vabs.f32 q15, q4", q15, q4, i32, f2u(-0.5));
    TESTINSN_un("vabs.f32 q15, q4", q15, q4, i32, f2u(-7.1));
    TESTINSN_un("vabs.f32 q12, q8", q12, q8, i32, f2u(8.0 - 1.0/1024.0));
    TESTINSN_un("vabs.f32 q12, q8", q12, q8, i32, f2u(-8.0 + 1.0/1024.0));
    TESTINSN_un("vabs.f32 q0, q1", q0, q1, i32, f2u(3.2));
    TESTINSN_un("vabs.f32 q10, q11", q10, q11, i32, f2u(3e22));
    TESTINSN_un("vabs.f32 q15, q4", q15, q4, i32, f2u(3e9));
    TESTINSN_un("vabs.f32 q15, q4", q15, q4, i32, f2u(-0.5));
    TESTINSN_un("vabs.f32 q15, q4", q15, q4, i32, f2u(-7.1));
    TESTINSN_un("vabs.f32 q12, q8", q12, q8, i32, f2u(8.0 - 1.0/1024.0));
    TESTINSN_un("vabs.f32 q12, q8", q12, q8, i32, f2u(-8.0 + 1.0/1024.0));
    TESTINSN_un("vabs.f32 q0, q1", q0, q1, i32, 7);
    TESTINSN_un("vabs.f32 q10, q11", q10, q11, i32, 1 << 31);
    TESTINSN_un("vabs.f32 q0, q1", q0, q1, i32, (1U << 31) + 1);
    TESTINSN_un("vabs.f32 q0, q1", q0, q1, i32, (1U << 31) - 1);
    TESTINSN_un("vabs.f32 q0, q14", q0, q14, i32, 0x30a0bcef);
    TESTINSN_un("vabs.f32 q0, q1", q0, q1, i32, 7);
    TESTINSN_un("vabs.f32 q10, q11", q10, q11, i32, 1 << 31);
    TESTINSN_un("vabs.f32 q0, q1", q0, q1, i32, (1U << 31) + 1);
    TESTINSN_un("vabs.f32 q0, q1", q0, q1, i32, (1U << 31) - 1);
    TESTINSN_un("vabs.f32 q0, q14", q0, q14, i32, 0x30a0bcef);
    TESTINSN_un("vabs.f32 q0, q1", q0, q1, i32, f2u(NAN));
    TESTINSN_un("vabs.f32 q0, q1", q0, q1, i32, f2u(0.0));
    TESTINSN_un("vabs.f32 q0, q1", q0, q1, i32, f2u(INFINITY));
    TESTINSN_un("vabs.f32 q0, q1", q0, q1, i32, f2u(-INFINITY));
    TESTINSN_un("vabs.f32 q0, q1", q0, q1, i32, f2u(NAN));
    TESTINSN_un("vabs.f32 q0, q1", q0, q1, i32, f2u(0.0));
    TESTINSN_un("vabs.f32 q0, q1", q0, q1, i32, f2u(INFINITY));
    TESTINSN_un("vabs.f32 q0, q1", q0, q1, i32, f2u(-INFINITY));

    printf("---- VCGT (fp) ----\n");
    TESTINSN_bin("vcgt.f32 q0, q1, q2", q0, q1, i32, f2u(0.5), q2, i32, f2u(-0.5));
    TESTINSN_bin("vcgt.f32 q2, q15, q12", q2, q15, i32, f2u(-0.53), q12, i32, f2u(0.52));
    TESTINSN_bin("vcgt.f32 q15, q7, q8", q15, q7, i32, f2u(231.45), q7, i32, f2u(231.45));
    TESTINSN_bin("vcgt.f32 q0, q5, q2", q0, q5, i32, f2u(23.04), q2, i32, f2u(-45.5687));
    TESTINSN_bin("vcgt.f32 q3, q4, q5", q3, q4, i32, f2u(-347856.475), q5, i32, f2u(1346));
    TESTINSN_bin("vcgt.f32 q10, q11, q2", q10, q11, i32, f2u(48755), q2, i32, f2u(-45786.476));
    TESTINSN_bin("vcgt.f32 q9, q5, q7", q9, q5, i32, f2u(95867.76), q7, i32, f2u(17065));
    TESTINSN_bin("vcgt.f32 q0, q5, q2", q0, q5, i32, f2u(-45667.24), q2, i32, f2u(-248562.76));
    TESTINSN_bin("vcgt.f32 q3, q4, q5", q3, q4, i32, f2u(24), q5, i32, f2u(1346));
    TESTINSN_bin("vcgt.f32 q10, q11, q2", q10, q11, i32, f2u(48755), q2, i32, f2u(1089));
    TESTINSN_bin("vcgt.f32 q9, q5, q7", q9, q5, i32, f2u(214), q7, i32, f2u(1752065));
    TESTINSN_bin("vcgt.f32 q0, q11, q12", q0, q11, i32, f2u(356047.56), q12, i32, f2u(5867.009));
    TESTINSN_bin("vcgt.f32 q7, q1, q6", q7, q1, i32, f2u(34.00046), q6, i32, f2u(0.0024575));
    TESTINSN_bin("vcgt.f32 q0, q1, q2", q0, q1, i32, f2u(2754), q2, i32, f2u(107));
    TESTINSN_bin("vcgt.f32 q3, q4, q5", q3, q4, i32, f2u(874), q5, i32, f2u(1384.6));
    TESTINSN_bin("vcgt.f32 q10, q11, q2", q10, q11, i32, f2u(487.587), q2, i32, f2u(109));
    TESTINSN_bin("vcgt.f32 q9, q5, q7", q9, q5, i32, f2u(2146), q7, i32, f2u(1752));
    TESTINSN_bin("vcgt.f32 q0, q11, q12", q0, q11, i32, f2u(-56.25), q12, i32, f2u(-5786.47));
    TESTINSN_bin("vcgt.f32 q7, q1, q6", q7, q1, i32, f2u(456.2489562), q6, i32, f2u(-7.2945676));
    TESTINSN_bin("vcgt.f32 q0, q5, q2", q0, q5, i32, f2u(532.987), q2, i32, f2u(-0.0045876));
    TESTINSN_bin("vcgt.f32 q10, q13, q15", q10, q13, i32, f2u(-485.2457), q15, i32, f2u(-567.245));
    TESTINSN_bin("vcgt.f32 q10, q13, q15", q10, q13, i32, f2u(278456.45), q15, i32, f2u(8756.0076));
    TESTINSN_bin("vcgt.f32 q0, q1, q2", q0, q1, i32, f2u(876988654), q2, i32, f2u(1224808797));
    TESTINSN_bin("vcgt.f32 q0, q1, q2", q0, q1, i32, f2u(0), q2, i32, f2u(0));
    TESTINSN_bin("vcgt.f32 q0, q1, q2", q0, q1, i32, f2u(1.0/1024.0), q2, i32, f2u(-1.0/1024.0));
    TESTINSN_bin("vcgt.f32 q0, q1, q2", q0, q1, i32, f2u(-1.0/1024.0), q2, i32, f2u(1.0/1024.0));
    TESTINSN_bin("vcgt.f32 q0, q1, q2", q0, q1, i32, f2u(2342+1.0/1024.0), q2, i32, f2u(2342-1.0/1024.0));
    TESTINSN_bin("vcgt.f32 q0, q1, q2", q0, q1, i32, f2u(-2342+1.0/1024.0), q2, i32, f2u(-2342-1.0/1024.0));
    TESTINSN_bin("vcgt.f32 q0, q1, q2", q0, q1, i32, f2u(89276+1.0/1024.0), q2, i32, f2u(89276+1.0/1024.0));
    TESTINSN_bin("vcgt.f32 q0, q1, q2", q0, q1, i32, f2u(NAN), q2, i32, f2u(NAN));
    TESTINSN_bin("vcgt.f32 q0, q1, q2", q0, q1, i32, f2u(NAN), q2, i32, f2u(1.0));
    TESTINSN_bin("vcgt.f32 q0, q1, q2", q0, q1, i32, f2u(NAN), q2, i32, f2u(0.0));
    TESTINSN_bin("vcgt.f32 q0, q1, q2", q0, q1, i32, f2u(NAN), q2, i32, f2u(INFINITY));
    TESTINSN_bin("vcgt.f32 q0, q1, q2", q0, q1, i32, f2u(NAN), q2, i32, f2u(-INFINITY));
    TESTINSN_bin("vcgt.f32 q0, q1, q2", q0, q1, i32, f2u(0.0), q2, i32, f2u(NAN));
    TESTINSN_bin("vcgt.f32 q0, q1, q2", q0, q1, i32, f2u(0.0), q2, i32, f2u(1.0));
    TESTINSN_bin("vcgt.f32 q0, q1, q2", q0, q1, i32, f2u(0.0), q2, i32, f2u(0.0));
    TESTINSN_bin("vcgt.f32 q0, q1, q2", q0, q1, i32, f2u(0.0), q2, i32, f2u(INFINITY));
    TESTINSN_bin("vcgt.f32 q0, q1, q2", q0, q1, i32, f2u(0.0), q2, i32, f2u(-INFINITY));
    TESTINSN_bin("vcgt.f32 q0, q1, q2", q0, q1, i32, f2u(INFINITY), q2, i32, f2u(NAN));
    TESTINSN_bin("vcgt.f32 q0, q1, q2", q0, q1, i32, f2u(INFINITY), q2, i32, f2u(1.0));
    TESTINSN_bin("vcgt.f32 q0, q1, q2", q0, q1, i32, f2u(INFINITY), q2, i32, f2u(0.0));
    TESTINSN_bin("vcgt.f32 q0, q1, q2", q0, q1, i32, f2u(INFINITY), q2, i32, f2u(INFINITY));
    TESTINSN_bin("vcgt.f32 q0, q1, q2", q0, q1, i32, f2u(INFINITY), q2, i32, f2u(-INFINITY));
    TESTINSN_bin("vcgt.f32 q0, q1, q2", q0, q1, i32, f2u(-INFINITY), q2, i32, f2u(NAN));
    TESTINSN_bin("vcgt.f32 q0, q1, q2", q0, q1, i32, f2u(-INFINITY), q2, i32, f2u(1.0));
    TESTINSN_bin("vcgt.f32 q0, q1, q2", q0, q1, i32, f2u(-INFINITY), q2, i32, f2u(0.0));
    TESTINSN_bin("vcgt.f32 q0, q1, q2", q0, q1, i32, f2u(-INFINITY), q2, i32, f2u(INFINITY));
    TESTINSN_bin("vcgt.f32 q0, q1, q2", q0, q1, i32, f2u(-INFINITY), q2, i32, f2u(-INFINITY));

    printf("---- VCGE (fp) ----\n");
    TESTINSN_bin("vcge.f32 q0, q1, q2", q0, q1, i32, f2u(0.5), q2, i32, f2u(-0.5));
    TESTINSN_bin("vcge.f32 q2, q15, q12", q2, q15, i32, f2u(-0.53), q12, i32, f2u(0.52));
    TESTINSN_bin("vcge.f32 q15, q7, q8", q15, q7, i32, f2u(231.45), q7, i32, f2u(231.45));
    TESTINSN_bin("vcge.f32 q0, q5, q2", q0, q5, i32, f2u(23.04), q2, i32, f2u(-45.5687));
    TESTINSN_bin("vcge.f32 q3, q4, q5", q3, q4, i32, f2u(-347856.475), q5, i32, f2u(1346));
    TESTINSN_bin("vcge.f32 q10, q11, q2", q10, q11, i32, f2u(48755), q2, i32, f2u(-45786.476));
    TESTINSN_bin("vcge.f32 q9, q5, q7", q9, q5, i32, f2u(95867.76), q7, i32, f2u(17065));
    TESTINSN_bin("vcge.f32 q0, q5, q2", q0, q5, i32, f2u(-45667.24), q2, i32, f2u(-248562.76));
    TESTINSN_bin("vcge.f32 q3, q4, q5", q3, q4, i32, f2u(24), q5, i32, f2u(1346));
    TESTINSN_bin("vcge.f32 q10, q11, q2", q10, q11, i32, f2u(48755), q2, i32, f2u(1089));
    TESTINSN_bin("vcge.f32 q9, q5, q7", q9, q5, i32, f2u(214), q7, i32, f2u(1752065));
    TESTINSN_bin("vcge.f32 q0, q11, q12", q0, q11, i32, f2u(356047.56), q12, i32, f2u(5867.009));
    TESTINSN_bin("vcge.f32 q7, q1, q6", q7, q1, i32, f2u(34.00046), q6, i32, f2u(0.0024575));
    TESTINSN_bin("vcge.f32 q0, q1, q2", q0, q1, i32, f2u(2754), q2, i32, f2u(107));
    TESTINSN_bin("vcge.f32 q3, q4, q5", q3, q4, i32, f2u(874), q5, i32, f2u(1384.6));
    TESTINSN_bin("vcge.f32 q10, q11, q2", q10, q11, i32, f2u(487.587), q2, i32, f2u(109));
    TESTINSN_bin("vcge.f32 q9, q5, q7", q9, q5, i32, f2u(2146), q7, i32, f2u(1752));
    TESTINSN_bin("vcge.f32 q0, q11, q12", q0, q11, i32, f2u(-56.25), q12, i32, f2u(-5786.47));
    TESTINSN_bin("vcge.f32 q7, q1, q6", q7, q1, i32, f2u(456.2489562), q6, i32, f2u(-7.2945676));
    TESTINSN_bin("vcge.f32 q0, q5, q2", q0, q5, i32, f2u(532.987), q2, i32, f2u(-0.0045876));
    TESTINSN_bin("vcge.f32 q10, q13, q15", q10, q13, i32, f2u(-485.2457), q15, i32, f2u(-567.245));
    TESTINSN_bin("vcge.f32 q10, q13, q15", q10, q13, i32, f2u(278456.45), q15, i32, f2u(8756.0076));
    TESTINSN_bin("vcge.f32 q0, q1, q2", q0, q1, i32, f2u(876988654), q2, i32, f2u(1224808797));
    TESTINSN_bin("vcge.f32 q0, q1, q2", q0, q1, i32, f2u(0), q2, i32, f2u(0));
    TESTINSN_bin("vcge.f32 q0, q1, q2", q0, q1, i32, f2u(1.0/1024.0), q2, i32, f2u(-1.0/1024.0));
    TESTINSN_bin("vcge.f32 q0, q1, q2", q0, q1, i32, f2u(-1.0/1024.0), q2, i32, f2u(1.0/1024.0));
    TESTINSN_bin("vcge.f32 q0, q1, q2", q0, q1, i32, f2u(2342+1.0/1024.0), q2, i32, f2u(2342-1.0/1024.0));
    TESTINSN_bin("vcge.f32 q0, q1, q2", q0, q1, i32, f2u(-2342+1.0/1024.0), q2, i32, f2u(-2342-1.0/1024.0));
    TESTINSN_bin("vcge.f32 q0, q1, q2", q0, q1, i32, f2u(89276+1.0/1024.0), q2, i32, f2u(89276+1.0/1024.0));
    TESTINSN_bin("vcge.f32 q0, q1, q2", q0, q1, i32, f2u(NAN), q2, i32, f2u(NAN));
    TESTINSN_bin("vcge.f32 q0, q1, q2", q0, q1, i32, f2u(NAN), q2, i32, f2u(1.0));
    TESTINSN_bin("vcge.f32 q0, q1, q2", q0, q1, i32, f2u(NAN), q2, i32, f2u(0.0));
    TESTINSN_bin("vcge.f32 q0, q1, q2", q0, q1, i32, f2u(NAN), q2, i32, f2u(INFINITY));
    TESTINSN_bin("vcge.f32 q0, q1, q2", q0, q1, i32, f2u(NAN), q2, i32, f2u(-INFINITY));
    TESTINSN_bin("vcge.f32 q0, q1, q2", q0, q1, i32, f2u(0.0), q2, i32, f2u(NAN));
    TESTINSN_bin("vcge.f32 q0, q1, q2", q0, q1, i32, f2u(0.0), q2, i32, f2u(1.0));
    TESTINSN_bin("vcge.f32 q0, q1, q2", q0, q1, i32, f2u(0.0), q2, i32, f2u(0.0));
    TESTINSN_bin("vcge.f32 q0, q1, q2", q0, q1, i32, f2u(0.0), q2, i32, f2u(INFINITY));
    TESTINSN_bin("vcge.f32 q0, q1, q2", q0, q1, i32, f2u(0.0), q2, i32, f2u(-INFINITY));
    TESTINSN_bin("vcge.f32 q0, q1, q2", q0, q1, i32, f2u(INFINITY), q2, i32, f2u(NAN));
    TESTINSN_bin("vcge.f32 q0, q1, q2", q0, q1, i32, f2u(INFINITY), q2, i32, f2u(1.0));
    TESTINSN_bin("vcge.f32 q0, q1, q2", q0, q1, i32, f2u(INFINITY), q2, i32, f2u(0.0));
    TESTINSN_bin("vcge.f32 q0, q1, q2", q0, q1, i32, f2u(INFINITY), q2, i32, f2u(INFINITY));
    TESTINSN_bin("vcge.f32 q0, q1, q2", q0, q1, i32, f2u(INFINITY), q2, i32, f2u(-INFINITY));
    TESTINSN_bin("vcge.f32 q0, q1, q2", q0, q1, i32, f2u(-INFINITY), q2, i32, f2u(NAN));
    TESTINSN_bin("vcge.f32 q0, q1, q2", q0, q1, i32, f2u(-INFINITY), q2, i32, f2u(1.0));
    TESTINSN_bin("vcge.f32 q0, q1, q2", q0, q1, i32, f2u(-INFINITY), q2, i32, f2u(0.0));
    TESTINSN_bin("vcge.f32 q0, q1, q2", q0, q1, i32, f2u(-INFINITY), q2, i32, f2u(INFINITY));
    TESTINSN_bin("vcge.f32 q0, q1, q2", q0, q1, i32, f2u(-INFINITY), q2, i32, f2u(-INFINITY));

    printf("---- VACGT (fp) ----\n");
    TESTINSN_bin("vacgt.f32 q0, q1, q2", q0, q1, i32, f2u(0.5), q2, i32, f2u(-0.5));
    TESTINSN_bin("vacgt.f32 q2, q15, q12", q2, q15, i32, f2u(-0.53), q12, i32, f2u(0.52));
    TESTINSN_bin("vacgt.f32 q15, q7, q8", q15, q7, i32, f2u(231.45), q7, i32, f2u(231.45));
    TESTINSN_bin("vacgt.f32 q0, q5, q2", q0, q5, i32, f2u(23.04), q2, i32, f2u(-45.5687));
    TESTINSN_bin("vacgt.f32 q3, q4, q5", q3, q4, i32, f2u(-347856.475), q5, i32, f2u(1346));
    TESTINSN_bin("vacgt.f32 q10, q11, q2", q10, q11, i32, f2u(48755), q2, i32, f2u(-45786.476));
    TESTINSN_bin("vacgt.f32 q9, q5, q7", q9, q5, i32, f2u(95867.76), q7, i32, f2u(17065));
    TESTINSN_bin("vacgt.f32 q0, q5, q2", q0, q5, i32, f2u(-45667.24), q2, i32, f2u(-248562.76));
    TESTINSN_bin("vacgt.f32 q3, q4, q5", q3, q4, i32, f2u(24), q5, i32, f2u(1346));
    TESTINSN_bin("vacgt.f32 q10, q11, q2", q10, q11, i32, f2u(48755), q2, i32, f2u(1089));
    TESTINSN_bin("vacgt.f32 q9, q5, q7", q9, q5, i32, f2u(214), q7, i32, f2u(1752065));
    TESTINSN_bin("vacgt.f32 q0, q11, q12", q0, q11, i32, f2u(356047.56), q12, i32, f2u(5867.009));
    TESTINSN_bin("vacgt.f32 q7, q1, q6", q7, q1, i32, f2u(34.00046), q6, i32, f2u(0.0024575));
    TESTINSN_bin("vacgt.f32 q0, q1, q2", q0, q1, i32, f2u(2754), q2, i32, f2u(107));
    TESTINSN_bin("vacgt.f32 q3, q4, q5", q3, q4, i32, f2u(874), q5, i32, f2u(1384.6));
    TESTINSN_bin("vacgt.f32 q10, q11, q2", q10, q11, i32, f2u(487.587), q2, i32, f2u(109));
    TESTINSN_bin("vacgt.f32 q9, q5, q7", q9, q5, i32, f2u(2146), q7, i32, f2u(1752));
    TESTINSN_bin("vacgt.f32 q0, q11, q12", q0, q11, i32, f2u(-56.25), q12, i32, f2u(-5786.47));
    TESTINSN_bin("vacgt.f32 q7, q1, q6", q7, q1, i32, f2u(456.2489562), q6, i32, f2u(-7.2945676));
    TESTINSN_bin("vacgt.f32 q0, q5, q2", q0, q5, i32, f2u(532.987), q2, i32, f2u(-0.0045876));
    TESTINSN_bin("vacgt.f32 q10, q13, q15", q10, q13, i32, f2u(-485.2457), q15, i32, f2u(-567.245));
    TESTINSN_bin("vacgt.f32 q10, q13, q15", q10, q13, i32, f2u(278456.45), q15, i32, f2u(8756.0076));
    TESTINSN_bin("vacgt.f32 q0, q1, q2", q0, q1, i32, f2u(876988654), q2, i32, f2u(1224808797));
    TESTINSN_bin("vacgt.f32 q0, q1, q2", q0, q1, i32, f2u(0), q2, i32, f2u(0));
    TESTINSN_bin("vacgt.f32 q0, q1, q2", q0, q1, i32, f2u(1.0/1024.0), q2, i32, f2u(-1.0/1024.0));
    TESTINSN_bin("vacgt.f32 q0, q1, q2", q0, q1, i32, f2u(-1.0/1024.0), q2, i32, f2u(1.0/1024.0));
    TESTINSN_bin("vacgt.f32 q0, q1, q2", q0, q1, i32, f2u(2342+1.0/1024.0), q2, i32, f2u(2342-1.0/1024.0));
    TESTINSN_bin("vacgt.f32 q0, q1, q2", q0, q1, i32, f2u(-2342+1.0/1024.0), q2, i32, f2u(-2342-1.0/1024.0));
    TESTINSN_bin("vacgt.f32 q0, q1, q2", q0, q1, i32, f2u(89276+1.0/1024.0), q2, i32, f2u(89276+1.0/1024.0));
    TESTINSN_bin("vacgt.f32 q0, q1, q2", q0, q1, i32, f2u(NAN), q2, i32, f2u(NAN));
    TESTINSN_bin("vacgt.f32 q0, q1, q2", q0, q1, i32, f2u(NAN), q2, i32, f2u(1.0));
    TESTINSN_bin("vacgt.f32 q0, q1, q2", q0, q1, i32, f2u(NAN), q2, i32, f2u(0.0));
    TESTINSN_bin("vacgt.f32 q0, q1, q2", q0, q1, i32, f2u(NAN), q2, i32, f2u(INFINITY));
    TESTINSN_bin("vacgt.f32 q0, q1, q2", q0, q1, i32, f2u(NAN), q2, i32, f2u(-INFINITY));
    TESTINSN_bin("vacgt.f32 q0, q1, q2", q0, q1, i32, f2u(0.0), q2, i32, f2u(NAN));
    TESTINSN_bin("vacgt.f32 q0, q1, q2", q0, q1, i32, f2u(0.0), q2, i32, f2u(1.0));
    TESTINSN_bin("vacgt.f32 q0, q1, q2", q0, q1, i32, f2u(0.0), q2, i32, f2u(0.0));
    TESTINSN_bin("vacgt.f32 q0, q1, q2", q0, q1, i32, f2u(0.0), q2, i32, f2u(INFINITY));
    TESTINSN_bin("vacgt.f32 q0, q1, q2", q0, q1, i32, f2u(0.0), q2, i32, f2u(-INFINITY));
    TESTINSN_bin("vacgt.f32 q0, q1, q2", q0, q1, i32, f2u(INFINITY), q2, i32, f2u(NAN));
    TESTINSN_bin("vacgt.f32 q0, q1, q2", q0, q1, i32, f2u(INFINITY), q2, i32, f2u(1.0));
    TESTINSN_bin("vacgt.f32 q0, q1, q2", q0, q1, i32, f2u(INFINITY), q2, i32, f2u(0.0));
    TESTINSN_bin("vacgt.f32 q0, q1, q2", q0, q1, i32, f2u(INFINITY), q2, i32, f2u(INFINITY));
    TESTINSN_bin("vacgt.f32 q0, q1, q2", q0, q1, i32, f2u(INFINITY), q2, i32, f2u(-INFINITY));
    TESTINSN_bin("vacgt.f32 q0, q1, q2", q0, q1, i32, f2u(-INFINITY), q2, i32, f2u(NAN));
    TESTINSN_bin("vacgt.f32 q0, q1, q2", q0, q1, i32, f2u(-INFINITY), q2, i32, f2u(1.0));
    TESTINSN_bin("vacgt.f32 q0, q1, q2", q0, q1, i32, f2u(-INFINITY), q2, i32, f2u(0.0));
    TESTINSN_bin("vacgt.f32 q0, q1, q2", q0, q1, i32, f2u(-INFINITY), q2, i32, f2u(INFINITY));
    TESTINSN_bin("vacgt.f32 q0, q1, q2", q0, q1, i32, f2u(-INFINITY), q2, i32, f2u(-INFINITY));

    printf("---- VACGE (fp) ----\n");
    TESTINSN_bin("vacge.f32 q0, q1, q2", q0, q1, i32, f2u(0.5), q2, i32, f2u(-0.5));
    TESTINSN_bin("vacge.f32 q2, q15, q12", q2, q15, i32, f2u(-0.53), q12, i32, f2u(0.52));
    TESTINSN_bin("vacge.f32 q15, q7, q8", q15, q7, i32, f2u(231.45), q7, i32, f2u(231.45));
    TESTINSN_bin("vacge.f32 q0, q5, q2", q0, q5, i32, f2u(23.04), q2, i32, f2u(-45.5687));
    TESTINSN_bin("vacge.f32 q3, q4, q5", q3, q4, i32, f2u(-347856.475), q5, i32, f2u(1346));
    TESTINSN_bin("vacge.f32 q10, q11, q2", q10, q11, i32, f2u(48755), q2, i32, f2u(-45786.476));
    TESTINSN_bin("vacge.f32 q9, q5, q7", q9, q5, i32, f2u(95867.76), q7, i32, f2u(17065));
    TESTINSN_bin("vacge.f32 q0, q5, q2", q0, q5, i32, f2u(-45667.24), q2, i32, f2u(-248562.76));
    TESTINSN_bin("vacge.f32 q3, q4, q5", q3, q4, i32, f2u(24), q5, i32, f2u(1346));
    TESTINSN_bin("vacge.f32 q10, q11, q2", q10, q11, i32, f2u(48755), q2, i32, f2u(1089));
    TESTINSN_bin("vacge.f32 q9, q5, q7", q9, q5, i32, f2u(214), q7, i32, f2u(1752065));
    TESTINSN_bin("vacge.f32 q0, q11, q12", q0, q11, i32, f2u(356047.56), q12, i32, f2u(5867.009));
    TESTINSN_bin("vacge.f32 q7, q1, q6", q7, q1, i32, f2u(34.00046), q6, i32, f2u(0.0024575));
    TESTINSN_bin("vacge.f32 q0, q1, q2", q0, q1, i32, f2u(2754), q2, i32, f2u(107));
    TESTINSN_bin("vacge.f32 q3, q4, q5", q3, q4, i32, f2u(874), q5, i32, f2u(1384.6));
    TESTINSN_bin("vacge.f32 q10, q11, q2", q10, q11, i32, f2u(487.587), q2, i32, f2u(109));
    TESTINSN_bin("vacge.f32 q9, q5, q7", q9, q5, i32, f2u(2146), q7, i32, f2u(1752));
    TESTINSN_bin("vacge.f32 q0, q11, q12", q0, q11, i32, f2u(-56.25), q12, i32, f2u(-5786.47));
    TESTINSN_bin("vacge.f32 q7, q1, q6", q7, q1, i32, f2u(456.2489562), q6, i32, f2u(-7.2945676));
    TESTINSN_bin("vacge.f32 q0, q5, q2", q0, q5, i32, f2u(532.987), q2, i32, f2u(-0.0045876));
    TESTINSN_bin("vacge.f32 q10, q13, q15", q10, q13, i32, f2u(-485.2457), q15, i32, f2u(-567.245));
    TESTINSN_bin("vacge.f32 q10, q13, q15", q10, q13, i32, f2u(278456.45), q15, i32, f2u(8756.0076));
    TESTINSN_bin("vacge.f32 q0, q1, q2", q0, q1, i32, f2u(876988654), q2, i32, f2u(1224808797));
    TESTINSN_bin("vacge.f32 q0, q1, q2", q0, q1, i32, f2u(0), q2, i32, f2u(0));
    TESTINSN_bin("vacge.f32 q0, q1, q2", q0, q1, i32, f2u(1.0/1024.0), q2, i32, f2u(-1.0/1024.0));
    TESTINSN_bin("vacge.f32 q0, q1, q2", q0, q1, i32, f2u(-1.0/1024.0), q2, i32, f2u(1.0/1024.0));
    TESTINSN_bin("vacge.f32 q0, q1, q2", q0, q1, i32, f2u(2342+1.0/1024.0), q2, i32, f2u(2342-1.0/1024.0));
    TESTINSN_bin("vacge.f32 q0, q1, q2", q0, q1, i32, f2u(-2342+1.0/1024.0), q2, i32, f2u(-2342-1.0/1024.0));
    TESTINSN_bin("vacge.f32 q0, q1, q2", q0, q1, i32, f2u(89276+1.0/1024.0), q2, i32, f2u(89276+1.0/1024.0));
    TESTINSN_bin("vacge.f32 q0, q1, q2", q0, q1, i32, f2u(NAN), q2, i32, f2u(NAN));
    TESTINSN_bin("vacge.f32 q0, q1, q2", q0, q1, i32, f2u(NAN), q2, i32, f2u(1.0));
    TESTINSN_bin("vacge.f32 q0, q1, q2", q0, q1, i32, f2u(NAN), q2, i32, f2u(0.0));
    TESTINSN_bin("vacge.f32 q0, q1, q2", q0, q1, i32, f2u(NAN), q2, i32, f2u(INFINITY));
    TESTINSN_bin("vacge.f32 q0, q1, q2", q0, q1, i32, f2u(NAN), q2, i32, f2u(-INFINITY));
    TESTINSN_bin("vacge.f32 q0, q1, q2", q0, q1, i32, f2u(0.0), q2, i32, f2u(NAN));
    TESTINSN_bin("vacge.f32 q0, q1, q2", q0, q1, i32, f2u(0.0), q2, i32, f2u(1.0));
    TESTINSN_bin("vacge.f32 q0, q1, q2", q0, q1, i32, f2u(0.0), q2, i32, f2u(0.0));
    TESTINSN_bin("vacge.f32 q0, q1, q2", q0, q1, i32, f2u(0.0), q2, i32, f2u(INFINITY));
    TESTINSN_bin("vacge.f32 q0, q1, q2", q0, q1, i32, f2u(0.0), q2, i32, f2u(-INFINITY));
    TESTINSN_bin("vacge.f32 q0, q1, q2", q0, q1, i32, f2u(INFINITY), q2, i32, f2u(NAN));
    TESTINSN_bin("vacge.f32 q0, q1, q2", q0, q1, i32, f2u(INFINITY), q2, i32, f2u(1.0));
    TESTINSN_bin("vacge.f32 q0, q1, q2", q0, q1, i32, f2u(INFINITY), q2, i32, f2u(0.0));
    TESTINSN_bin("vacge.f32 q0, q1, q2", q0, q1, i32, f2u(INFINITY), q2, i32, f2u(INFINITY));
    TESTINSN_bin("vacge.f32 q0, q1, q2", q0, q1, i32, f2u(INFINITY), q2, i32, f2u(-INFINITY));
    TESTINSN_bin("vacge.f32 q0, q1, q2", q0, q1, i32, f2u(-INFINITY), q2, i32, f2u(NAN));
    TESTINSN_bin("vacge.f32 q0, q1, q2", q0, q1, i32, f2u(-INFINITY), q2, i32, f2u(1.0));
    TESTINSN_bin("vacge.f32 q0, q1, q2", q0, q1, i32, f2u(-INFINITY), q2, i32, f2u(0.0));
    TESTINSN_bin("vacge.f32 q0, q1, q2", q0, q1, i32, f2u(-INFINITY), q2, i32, f2u(INFINITY));
    TESTINSN_bin("vacge.f32 q0, q1, q2", q0, q1, i32, f2u(-INFINITY), q2, i32, f2u(-INFINITY));

    printf("---- VCEQ (fp) ----\n");
    TESTINSN_bin("vceq.f32 q0, q1, q2", q0, q1, i32, f2u(0.5), q2, i32, f2u(-0.5));
    TESTINSN_bin("vceq.f32 q2, q15, q12", q2, q15, i32, f2u(-0.53), q12, i32, f2u(0.52));
    TESTINSN_bin("vceq.f32 q15, q7, q8", q15, q7, i32, f2u(231.45), q7, i32, f2u(231.45));
    TESTINSN_bin("vceq.f32 q0, q5, q2", q0, q5, i32, f2u(23.04), q2, i32, f2u(-45.5687));
    TESTINSN_bin("vceq.f32 q3, q4, q5", q3, q4, i32, f2u(-347856.475), q5, i32, f2u(1346));
    TESTINSN_bin("vceq.f32 q10, q11, q2", q10, q11, i32, f2u(48755), q2, i32, f2u(-45786.476));
    TESTINSN_bin("vceq.f32 q9, q5, q7", q9, q5, i32, f2u(95867.76), q7, i32, f2u(17065));
    TESTINSN_bin("vceq.f32 q0, q5, q2", q0, q5, i32, f2u(-45667.24), q2, i32, f2u(-248562.76));
    TESTINSN_bin("vceq.f32 q3, q4, q5", q3, q4, i32, f2u(24), q5, i32, f2u(1346));
    TESTINSN_bin("vceq.f32 q10, q11, q2", q10, q11, i32, f2u(48755), q2, i32, f2u(1089));
    TESTINSN_bin("vceq.f32 q9, q5, q7", q9, q5, i32, f2u(214), q7, i32, f2u(1752065));
    TESTINSN_bin("vceq.f32 q0, q11, q12", q0, q11, i32, f2u(356047.56), q12, i32, f2u(5867.009));
    TESTINSN_bin("vceq.f32 q7, q1, q6", q7, q1, i32, f2u(34.00046), q6, i32, f2u(0.0024575));
    TESTINSN_bin("vceq.f32 q0, q1, q2", q0, q1, i32, f2u(2754), q2, i32, f2u(107));
    TESTINSN_bin("vceq.f32 q3, q4, q5", q3, q4, i32, f2u(874), q5, i32, f2u(1384.6));
    TESTINSN_bin("vceq.f32 q10, q11, q2", q10, q11, i32, f2u(487.587), q2, i32, f2u(109));
    TESTINSN_bin("vceq.f32 q9, q5, q7", q9, q5, i32, f2u(2146), q7, i32, f2u(1752));
    TESTINSN_bin("vceq.f32 q0, q11, q12", q0, q11, i32, f2u(-56.25), q12, i32, f2u(-5786.47));
    TESTINSN_bin("vceq.f32 q7, q1, q6", q7, q1, i32, f2u(456.2489562), q6, i32, f2u(-7.2945676));
    TESTINSN_bin("vceq.f32 q0, q5, q2", q0, q5, i32, f2u(532.987), q2, i32, f2u(-0.0045876));
    TESTINSN_bin("vceq.f32 q10, q13, q15", q10, q13, i32, f2u(-485.2457), q15, i32, f2u(-567.245));
    TESTINSN_bin("vceq.f32 q10, q13, q15", q10, q13, i32, f2u(278456.45), q15, i32, f2u(8756.0076));
    TESTINSN_bin("vceq.f32 q0, q1, q2", q0, q1, i32, f2u(876988654), q2, i32, f2u(1224808797));
    TESTINSN_bin("vceq.f32 q0, q1, q2", q0, q1, i32, f2u(0), q2, i32, f2u(0));
    TESTINSN_bin("vceq.f32 q0, q1, q2", q0, q1, i32, f2u(1.0/1024.0), q2, i32, f2u(-1.0/1024.0));
    TESTINSN_bin("vceq.f32 q0, q1, q2", q0, q1, i32, f2u(-1.0/1024.0), q2, i32, f2u(1.0/1024.0));
    TESTINSN_bin("vceq.f32 q0, q1, q2", q0, q1, i32, f2u(2342+1.0/1024.0), q2, i32, f2u(2342-1.0/1024.0));
    TESTINSN_bin("vceq.f32 q0, q1, q2", q0, q1, i32, f2u(-2342+1.0/1024.0), q2, i32, f2u(-2342-1.0/1024.0));
    TESTINSN_bin("vceq.f32 q0, q1, q2", q0, q1, i32, f2u(89276+1.0/1024.0), q2, i32, f2u(89276+1.0/1024.0));
    TESTINSN_bin("vceq.f32 q0, q1, q2", q0, q1, i32, f2u(NAN), q2, i32, f2u(NAN));
    TESTINSN_bin("vceq.f32 q0, q1, q2", q0, q1, i32, f2u(NAN), q2, i32, f2u(1.0));
    TESTINSN_bin("vceq.f32 q0, q1, q2", q0, q1, i32, f2u(NAN), q2, i32, f2u(0.0));
    TESTINSN_bin("vceq.f32 q0, q1, q2", q0, q1, i32, f2u(NAN), q2, i32, f2u(INFINITY));
    TESTINSN_bin("vceq.f32 q0, q1, q2", q0, q1, i32, f2u(NAN), q2, i32, f2u(-INFINITY));
    TESTINSN_bin("vceq.f32 q0, q1, q2", q0, q1, i32, f2u(0.0), q2, i32, f2u(NAN));
    TESTINSN_bin("vceq.f32 q0, q1, q2", q0, q1, i32, f2u(0.0), q2, i32, f2u(1.0));
    TESTINSN_bin("vceq.f32 q0, q1, q2", q0, q1, i32, f2u(0.0), q2, i32, f2u(0.0));
    TESTINSN_bin("vceq.f32 q0, q1, q2", q0, q1, i32, f2u(0.0), q2, i32, f2u(INFINITY));
    TESTINSN_bin("vceq.f32 q0, q1, q2", q0, q1, i32, f2u(0.0), q2, i32, f2u(-INFINITY));
    TESTINSN_bin("vceq.f32 q0, q1, q2", q0, q1, i32, f2u(INFINITY), q2, i32, f2u(NAN));
    TESTINSN_bin("vceq.f32 q0, q1, q2", q0, q1, i32, f2u(INFINITY), q2, i32, f2u(1.0));
    TESTINSN_bin("vceq.f32 q0, q1, q2", q0, q1, i32, f2u(INFINITY), q2, i32, f2u(0.0));
    TESTINSN_bin("vceq.f32 q0, q1, q2", q0, q1, i32, f2u(INFINITY), q2, i32, f2u(INFINITY));
    TESTINSN_bin("vceq.f32 q0, q1, q2", q0, q1, i32, f2u(INFINITY), q2, i32, f2u(-INFINITY));
    TESTINSN_bin("vceq.f32 q0, q1, q2", q0, q1, i32, f2u(-INFINITY), q2, i32, f2u(NAN));
    TESTINSN_bin("vceq.f32 q0, q1, q2", q0, q1, i32, f2u(-INFINITY), q2, i32, f2u(1.0));
    TESTINSN_bin("vceq.f32 q0, q1, q2", q0, q1, i32, f2u(-INFINITY), q2, i32, f2u(0.0));
    TESTINSN_bin("vceq.f32 q0, q1, q2", q0, q1, i32, f2u(-INFINITY), q2, i32, f2u(INFINITY));
    TESTINSN_bin("vceq.f32 q0, q1, q2", q0, q1, i32, f2u(-INFINITY), q2, i32, f2u(-INFINITY));

    printf("---- VCEQ (fp) #0 ----\n");
    TESTINSN_un("vceq.f32 q0, q1, #0", q0, q1, i32, 0x01000000);
    TESTINSN_un("vceq.f32 q0, q1, #0", q0, q1, i32, 0x1);
    TESTINSN_un("vceq.f32 q2, q1, #0", q2, q1, i32, 1 << 31);
    TESTINSN_un("vceq.f32 q2, q1, #0", q2, q1, i32, f2u(23.04));
    TESTINSN_un("vceq.f32 q2, q1, #0", q2, q1, i32, f2u(-23.04));
    TESTINSN_un("vceq.f32 q10, q15, #0", q10, q15, i32, 0x0);
    TESTINSN_un("vceq.f32 q0, q1, #0", q0, q1, i32, f2u(NAN));
    TESTINSN_un("vceq.f32 q0, q1, #0", q0, q1, i32, f2u(0.0));
    TESTINSN_un("vceq.f32 q0, q1, #0", q0, q1, i32, f2u(INFINITY));
    TESTINSN_un("vceq.f32 q0, q1, #0", q0, q1, i32, f2u(-INFINITY));

    printf("---- VCGT (fp) #0 ----\n");
    TESTINSN_un("vcgt.f32 q0, q1, #0", q0, q1, i32, 0x01000000);
    TESTINSN_un("vcgt.f32 q0, q1, #0", q0, q1, i32, 0x1);
    TESTINSN_un("vcgt.f32 q2, q1, #0", q2, q1, i32, 1 << 31);
    TESTINSN_un("vcgt.f32 q2, q1, #0", q2, q1, i32, f2u(23.04));
    TESTINSN_un("vcgt.f32 q2, q1, #0", q2, q1, i32, f2u(-23.04));
    TESTINSN_un("vcgt.f32 q10, q15, #0", q10, q15, i32, 0x0);
    TESTINSN_un("vcgt.f32 q0, q1, #0", q0, q1, i32, f2u(NAN));
    TESTINSN_un("vcgt.f32 q0, q1, #0", q0, q1, i32, f2u(0.0));
    TESTINSN_un("vcgt.f32 q0, q1, #0", q0, q1, i32, f2u(INFINITY));
    TESTINSN_un("vcgt.f32 q0, q1, #0", q0, q1, i32, f2u(-INFINITY));

    printf("---- VCLT (fp) #0 ----\n");
    TESTINSN_un("vclt.f32 q0, q1, #0", q0, q1, i32, 0x01000000);
    TESTINSN_un("vclt.f32 q0, q1, #0", q0, q1, i32, 0x1);
    TESTINSN_un("vclt.f32 q2, q1, #0", q2, q1, i32, 1 << 31);
    TESTINSN_un("vclt.f32 q2, q1, #0", q2, q1, i32, f2u(23.04));
    TESTINSN_un("vclt.f32 q2, q1, #0", q2, q1, i32, f2u(-23.04));
    TESTINSN_un("vclt.f32 q10, q15, #0", q10, q15, i32, 0x0);
    TESTINSN_un("vclt.f32 q0, q1, #0", q0, q1, i32, f2u(NAN));
    TESTINSN_un("vclt.f32 q0, q1, #0", q0, q1, i32, f2u(0.0));
    TESTINSN_un("vclt.f32 q0, q1, #0", q0, q1, i32, f2u(INFINITY));
    TESTINSN_un("vclt.f32 q0, q1, #0", q0, q1, i32, f2u(-INFINITY));

    printf("---- VCGE (fp) #0 ----\n");
    TESTINSN_un("vcge.f32 q0, q1, #0", q0, q1, i32, 0x01000000);
    TESTINSN_un("vcge.f32 q0, q1, #0", q0, q1, i32, 0x1);
    TESTINSN_un("vcge.f32 q2, q1, #0", q2, q1, i32, 1 << 31);
    TESTINSN_un("vcge.f32 q2, q1, #0", q2, q1, i32, f2u(23.04));
    TESTINSN_un("vcge.f32 q2, q1, #0", q2, q1, i32, f2u(-23.04));
    TESTINSN_un("vcge.f32 q10, q15, #0", q10, q15, i32, 0x0);
    TESTINSN_un("vcge.f32 q0, q1, #0", q0, q1, i32, f2u(NAN));
    TESTINSN_un("vcge.f32 q0, q1, #0", q0, q1, i32, f2u(0.0));
    TESTINSN_un("vcge.f32 q0, q1, #0", q0, q1, i32, f2u(INFINITY));
    TESTINSN_un("vcge.f32 q0, q1, #0", q0, q1, i32, f2u(-INFINITY));

    printf("---- VCLE (fp) #0 ----\n");
    TESTINSN_un("vcle.f32 q0, q1, #0", q0, q1, i32, 0x01000000);
    TESTINSN_un("vcle.f32 q0, q1, #0", q0, q1, i32, 0x1);
    TESTINSN_un("vcle.f32 q2, q1, #0", q2, q1, i32, 1 << 31);
    TESTINSN_un("vcle.f32 q2, q1, #0", q2, q1, i32, f2u(23.04));
    TESTINSN_un("vcle.f32 q2, q1, #0", q2, q1, i32, f2u(-23.04));
    TESTINSN_un("vcle.f32 q10, q15, #0", q10, q15, i32, 0x0);
    TESTINSN_un("vcle.f32 q0, q1, #0", q0, q1, i32, f2u(NAN));
    TESTINSN_un("vcle.f32 q0, q1, #0", q0, q1, i32, f2u(0.0));
    TESTINSN_un("vcle.f32 q0, q1, #0", q0, q1, i32, f2u(INFINITY));
    TESTINSN_un("vcle.f32 q0, q1, #0", q0, q1, i32, f2u(-INFINITY));

    printf("---- VNEG (fp) ----\n");
    TESTINSN_un("vneg.f32 q0, q1", q0, q1, i32, 0x01000000);
    TESTINSN_un("vneg.f32 q0, q1", q0, q1, i32, 0x1);
    TESTINSN_un("vneg.f32 q2, q1", q2, q1, i32, 1 << 31);
    TESTINSN_un("vneg.f32 q2, q1", q2, q1, i32, f2u(23.04));
    TESTINSN_un("vneg.f32 q2, q1", q2, q1, i32, f2u(-23.04));
    TESTINSN_un("vneg.f32 q10, q15", q10, q15, i32, 0x0);
    TESTINSN_un("vneg.f32 q0, q1", q0, q1, i32, f2u(NAN));
    TESTINSN_un("vneg.f32 q0, q1", q0, q1, i32, f2u(0.0));
    TESTINSN_un("vneg.f32 q0, q1", q0, q1, i32, f2u(INFINITY));
    TESTINSN_un("vneg.f32 q0, q1", q0, q1, i32, f2u(-INFINITY));

    printf("---- VRSQRTS ----\n");
    TESTINSN_bin("vrsqrts.f32 q0, q5, q2", q0, q5, i32, f2u(23.04), q2, i32, f2u(-45.5687));
    TESTINSN_bin("vrsqrts.f32 q3, q4, q5", q3, q4, i32, f2u(-347856.475), q5, i32, f2u(1346));
    TESTINSN_bin("vrsqrts.f32 q10, q11, q2", q10, q11, i32, f2u(48755), q2, i32, f2u(-45786.476));
    TESTINSN_bin("vrsqrts.f32 q9, q5, q7", q9, q5, i32, f2u(95867.76), q7, i32, f2u(17065));
    TESTINSN_bin("vrsqrts.f32 q0, q5, q2", q0, q5, i32, f2u(-45667.24), q2, i32, f2u(-248562.76));
    TESTINSN_bin("vrsqrts.f32 q3, q4, q5", q3, q4, i32, f2u(24), q5, i32, f2u(1346));
    TESTINSN_bin("vrsqrts.f32 q10, q11, q2", q10, q11, i32, f2u(48755), q2, i32, f2u(1089));
    TESTINSN_bin("vrsqrts.f32 q9, q5, q7", q9, q5, i32, f2u(214), q7, i32, f2u(1752065));
    TESTINSN_bin("vrsqrts.f32 q0, q11, q12", q0, q11, i32, f2u(356047.56), q12, i32, f2u(5867.009));
    TESTINSN_bin("vrsqrts.f32 q7, q1, q6", q7, q1, i32, f2u(34.00046), q6, i32, f2u(0.0024575));
    TESTINSN_bin("vrsqrts.f32 q0, q1, q2", q0, q1, i32, f2u(2754), q2, i32, f2u(107));
    TESTINSN_bin("vrsqrts.f32 q3, q4, q5", q3, q4, i32, f2u(874), q5, i32, f2u(1384.6));
    TESTINSN_bin("vrsqrts.f32 q10, q11, q2", q10, q11, i32, f2u(487.587), q2, i32, f2u(109));
    TESTINSN_bin("vrsqrts.f32 q9, q5, q7", q9, q5, i32, f2u(2146), q7, i32, f2u(1752));
    TESTINSN_bin("vrsqrts.f32 q0, q11, q12", q0, q11, i32, f2u(-56.25), q12, i32, f2u(-5786.47));
    TESTINSN_bin("vrsqrts.f32 q7, q1, q6", q7, q1, i32, f2u(456.2489562), q6, i32, f2u(-7.2945676));
    TESTINSN_bin("vrsqrts.f32 q0, q5, q2", q0, q5, i32, f2u(532.987), q2, i32, f2u(-0.0045876));
    TESTINSN_bin("vrsqrts.f32 q10, q13, q15", q10, q13, i32, f2u(-485.2457), q15, i32, f2u(-567.245));
    TESTINSN_bin("vrsqrts.f32 q10, q13, q15", q10, q13, i32, f2u(278456.45), q15, i32, f2u(8756.0076));
    TESTINSN_bin("vrsqrts.f32 q0, q1, q2", q0, q1, i32, f2u(876988654), q2, i32, f2u(1224808797));
    TESTINSN_bin("vrsqrts.f32 q0, q1, q2", q0, q1, i32, f2u(NAN), q2, i32, f2u(NAN));
    TESTINSN_bin("vrsqrts.f32 q0, q1, q2", q0, q1, i32, f2u(NAN), q2, i32, f2u(1.0));
    TESTINSN_bin("vrsqrts.f32 q0, q1, q2", q0, q1, i32, f2u(NAN), q2, i32, f2u(0.0));
    TESTINSN_bin("vrsqrts.f32 q0, q1, q2", q0, q1, i32, f2u(NAN), q2, i32, f2u(INFINITY));
    TESTINSN_bin("vrsqrts.f32 q0, q1, q2", q0, q1, i32, f2u(NAN), q2, i32, f2u(-INFINITY));
    TESTINSN_bin("vrsqrts.f32 q0, q1, q2", q0, q1, i32, f2u(0.0), q2, i32, f2u(NAN));
    TESTINSN_bin("vrsqrts.f32 q0, q1, q2", q0, q1, i32, f2u(0.0), q2, i32, f2u(1.0));
    TESTINSN_bin("vrsqrts.f32 q0, q1, q2", q0, q1, i32, f2u(0.0), q2, i32, f2u(0.0));
    TESTINSN_bin("vrsqrts.f32 q0, q1, q2", q0, q1, i32, f2u(0.0), q2, i32, f2u(INFINITY));
    TESTINSN_bin("vrsqrts.f32 q0, q1, q2", q0, q1, i32, f2u(0.0), q2, i32, f2u(-INFINITY));
    TESTINSN_bin("vrsqrts.f32 q0, q1, q2", q0, q1, i32, f2u(INFINITY), q2, i32, f2u(NAN));
    TESTINSN_bin("vrsqrts.f32 q0, q1, q2", q0, q1, i32, f2u(INFINITY), q2, i32, f2u(1.0));
    TESTINSN_bin("vrsqrts.f32 q0, q1, q2", q0, q1, i32, f2u(INFINITY), q2, i32, f2u(0.0));
    TESTINSN_bin("vrsqrts.f32 q0, q1, q2", q0, q1, i32, f2u(INFINITY), q2, i32, f2u(INFINITY));
    TESTINSN_bin("vrsqrts.f32 q0, q1, q2", q0, q1, i32, f2u(INFINITY), q2, i32, f2u(-INFINITY));
    TESTINSN_bin("vrsqrts.f32 q0, q1, q2", q0, q1, i32, f2u(-INFINITY), q2, i32, f2u(NAN));
    TESTINSN_bin("vrsqrts.f32 q0, q1, q2", q0, q1, i32, f2u(-INFINITY), q2, i32, f2u(1.0));
    TESTINSN_bin("vrsqrts.f32 q0, q1, q2", q0, q1, i32, f2u(-INFINITY), q2, i32, f2u(0.0));
    TESTINSN_bin("vrsqrts.f32 q0, q1, q2", q0, q1, i32, f2u(-INFINITY), q2, i32, f2u(INFINITY));
    TESTINSN_bin("vrsqrts.f32 q0, q1, q2", q0, q1, i32, f2u(-INFINITY), q2, i32, f2u(-INFINITY));

    printf("---- VRSQRTE (fp) ----\n");
    TESTINSN_un("vrsqrte.f32 q0, q1", q0, q1, i32, f2u(3.2));
    TESTINSN_un("vrsqrte.f32 q10, q11", q10, q11, i32, f2u(3e22));
    TESTINSN_un("vrsqrte.f32 q15, q4", q15, q4, i32, f2u(3e9));
    TESTINSN_un("vrsqrte.f32 q15, q4", q15, q4, i32, f2u(-0.5));
    TESTINSN_un("vrsqrte.f32 q15, q4", q15, q4, i32, f2u(-7.1));
    TESTINSN_un("vrsqrte.f32 q12, q8", q12, q8, i32, f2u(8.0 - 1.0/1024.0));
    TESTINSN_un("vrsqrte.f32 q12, q8", q12, q8, i32, f2u(-8.0 + 1.0/1024.0));
    TESTINSN_un("vrsqrte.f32 q0, q1", q0, q1, i32, f2u(3.2));
    TESTINSN_un("vrsqrte.f32 q10, q11", q10, q11, i32, f2u(3e22));
    TESTINSN_un("vrsqrte.f32 q15, q4", q15, q4, i32, f2u(3e9));
    TESTINSN_un("vrsqrte.f32 q15, q4", q15, q4, i32, f2u(-0.5));
    TESTINSN_un("vrsqrte.f32 q15, q4", q15, q4, i32, f2u(-7.1));
    TESTINSN_un("vrsqrte.f32 q12, q8", q12, q8, i32, f2u(8.0 - 1.0/1024.0));
    TESTINSN_un("vrsqrte.f32 q12, q8", q12, q8, i32, f2u(-8.0 + 1.0/1024.0));
    TESTINSN_un("vrsqrte.f32 q0, q1", q0, q1, i32, 7);
    TESTINSN_un("vrsqrte.f32 q10, q11", q10, q11, i32, 1 << 31); 
    TESTINSN_un("vrsqrte.f32 q0, q1", q0, q1, i32, (1U << 31) + 1);
    TESTINSN_un("vrsqrte.f32 q0, q1", q0, q1, i32, (1U << 31) - 1);
    TESTINSN_un("vrsqrte.f32 q0, q14", q0, q14, i32, 0x30a0bcef);
    TESTINSN_un("vrsqrte.f32 q0, q1", q0, q1, i32, 7);
    TESTINSN_un("vrsqrte.f32 q10, q11", q10, q11, i32, 1 << 31); 
    TESTINSN_un("vrsqrte.f32 q0, q1", q0, q1, i32, (1U << 31) + 1);
    TESTINSN_un("vrsqrte.f32 q0, q1", q0, q1, i32, (1U << 31) - 1);
    TESTINSN_un("vrsqrte.f32 q0, q14", q0, q14, i32, 0x30a0bcef);
    TESTINSN_un("vrsqrte.f32 q0, q1", q0, q1, i32, f2u(NAN));
    TESTINSN_un("vrsqrte.f32 q0, q1", q0, q1, i32, f2u(0.0));
    TESTINSN_un("vrsqrte.f32 q0, q1", q0, q1, i32, f2u(INFINITY));
    TESTINSN_un("vrsqrte.f32 q0, q1", q0, q1, i32, f2u(-INFINITY));

    return 0;
}
