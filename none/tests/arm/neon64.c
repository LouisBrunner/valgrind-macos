
/* How to compile:

   gcc -O -g -Wall -mcpu=cortex-a8 -mfpu=neon -mfloat-abi=softfp \
       -marm -o neon64-a neon64.c

   or

   gcc -O -g -Wall -mcpu=cortex-a8 -mfpu=neon -mfloat-abi=softfp \
       -mthumb -o neon64-t neon64.c

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

/* test macros to generate and output the result of a single instruction */

const unsigned int mem[] = {
    0x121f1e1f, 0x131b1a1b, 0x141c1f1c, 0x151d191d,
    0x232f2e2f, 0x242c2b2b, 0x252a2e2b, 0x262d2d2a,
    0x3f343f3e, 0x3e353d3c, 0x363a3c3b, 0x3b373b3a,
    0x454f4e45, 0x4e464d46, 0x474d474c, 0x4a484a4c
};

#define TESTINSN_imm(instruction, QD, imm) \
{ \
  unsigned int out[2]; \
\
  __asm__ volatile( \
      "vmov.i8 " #QD ", #0x55" "\n\t" \
      instruction ", #" #imm "\n\t" \
      "vstmia %0, {" #QD "}\n\t" \
      : \
      : "r" (out) \
      : #QD, "memory" \
      ); \
  printf("%s, #" #imm " :: Qd 0x%08x 0x%08x\n", \
      instruction, out[1], out[0]); \
} \
{ \
   unsigned int out[2];   \
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
   printf("%s, #" #imm " :: Qd 0x%08x 0x%08x\n", \
	 instruction, out[1], out[0]); \
}

#define TESTINSN_un(instruction, QD, QM, QMtype, QMval) \
{ \
  unsigned int out[2]; \
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
  printf("%s :: Qd 0x%08x 0x%08x  Qm (" #QMtype ")0x%08x\n", \
      instruction, out[1], out[0], QMval); \
} \
{ \
   unsigned int out[2]; \
   unsigned int addr = 0; \
   \
   __asm__ volatile( \
	 "mov %2, %3\n\t" \
	 "vldmia %2!, {" #QD "}\n\t" \
	 "vldmia %2!, {" #QM "}\n\t" \
	 instruction "\n\t" \
	 "vstmia %0, {" #QD "}\n\t" \
	 "vstmia %0, {" #QD "}\n\t" \
	 : \
	 : "r" (out), "r" (QMval), "r" (addr), "r" (mem) \
	 : #QD, #QM, "%2", "memory" \
	 ); \
   printf("%s :: Qd 0x%08x 0x%08x  Qm (" #QMtype ")0x%08x\n", \
	 instruction, out[1], out[0], QMval ); \
}

#define TESTINSN_un_q(instruction, QD, QM, QMtype, QMval) \
{ \
  unsigned int out[2]; \
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
  printf("%s :: Qd 0x%08x 0x%08x  Qm (" #QMtype ")0x%08x  fpscr %08x\n", \
      instruction, out[1], out[0], QMval, fpscr); \
} \
{ \
   unsigned int out[2]; \
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
   printf("%s :: Qd 0x%08x 0x%08x  Qm (" #QMtype ")0x%08x  fpscr %08x\n", \
	 instruction, out[1], out[0], QMval, fpscr); \
}

#define TESTINSN_core_to_scalar(instruction, QD, QM, QMval) \
{ \
  unsigned int out[2]; \
\
  __asm__ volatile( \
      "vmov.i8 " #QD ", #0x55" "\n\t" \
      "mov " #QM ", %1\n\t" \
      instruction "\n\t" \
      "vstmia %0, {" #QD "}\n\t" \
      : \
      : "r" (out), "r" (QMval) \
      : #QD, #QM, "memory" \
      ); \
  printf("%s :: Qd 0x%08x 0x%08x  Qm 0x%08x\n", \
      instruction, out[1], out[0], QMval); \
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
  unsigned int out[9]; \
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
      "str %1, [%2]\n\t" \
      "mov %0, r4\n\t" \
      : \
      : "r" (out), "r" (mem), "r"(&out[8]) \
      : #QD1, #QD2, #QD3, #QD4, "memory", "r4" \
      ); \
  printf("%s :: Result 0x%08x 0x%08x 0x%08x 0x%08x "\
          "0x%08x 0x%08x 0x%08x 0x%08x  delta %d\n", \
      instruction, out[0], out[1], out[2], out[3], out[4],\
          out[5], out[6], out[7], (int)out[8]-(int)mem); \
}

#define TESTINSN_VSTn(instruction, QD1, QD2, QD3, QD4) \
{ \
  unsigned int out[9]; \
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
      "str %0, [%2]\n\t" \
      : \
      : "r" (out), "r" (mem), "r"(&out[8]) \
      : #QD1, #QD2, #QD3, #QD4, "memory", "r4" \
      ); \
  printf("%s :: Result 0x%08x 0x%08x 0x%08x 0x%08x "\
          "0x%08x 0x%08x 0x%08x 0x%08x  delta %d\n", \
      instruction, out[0], out[1], out[2], out[3], out[4],\
          out[5], out[6], out[7], (int)out[8]-(int)out); \
}

#define TESTINSN_VLDn_WB(instruction, QD1, QD2, QD3, QD4) \
{ \
   unsigned int out[9]; \
   unsigned int addr = 0; \
   \
   __asm__ volatile( \
	 "mov %0, %2\n\t" \
	 "vmov.i8 " #QD1 ", #0x55" "\n\t" \
	 "vmov.i8 " #QD2 ", #0x55" "\n\t" \
	 "vmov.i8 " #QD3 ", #0x55" "\n\t" \
	 "vmov.i8 " #QD4 ", #0x55" "\n\t" \
	 instruction ", [%0]!\n\t" \
	 "mov r4, %1\n\t" \
	 "vstmia %1!, {" #QD1 "}\n\t" \
	 "vstmia %1!, {" #QD2 "}\n\t" \
	 "vstmia %1!, {" #QD3 "}\n\t" \
	 "vstmia %1!, {" #QD4 "}\n\t" \
	 "str %0, [%3]\n\t" \
	 "mov %1, r4\n\t" \
	 : "+r" (addr) \
	 : "r" (out), "r" (mem), "r"(&out[8]) \
	 : #QD1, #QD2, #QD3, #QD4, "memory", "r4" \
	 ); \
   printf("%s :: Result 0x%08x 0x%08x 0x%08x 0x%08x "\
	 "0x%08x 0x%08x 0x%08x 0x%08x  delta %d\n", \
	 instruction, out[0], out[1], out[2], out[3], out[4],\
	 out[5], out[6], out[7], (int)out[8]-(int)mem); \
}

#define TESTINSN_VSTn_WB(instruction, QD1, QD2, QD3, QD4) \
{ \
   unsigned int out[9]; \
   unsigned int addr = 0;    \
   \
   memset(out, 0x55, 8 * (sizeof(unsigned int)));\
   __asm__ volatile( \
	 "mov %0, %1\n\t" \
	 "mov r4, %2\n\t" \
	 "vldmia r4!, {" #QD1 "}\n\t" \
	 "vldmia r4!, {" #QD2 "}\n\t" \
	 "vldmia r4!, {" #QD3 "}\n\t" \
	 "vldmia r4!, {" #QD4 "}\n\t" \
	 instruction ", [%0]!\n\t" \
	 "str %0, [%3]\n\t" \
	 : "+r" (addr) \
	 : "r" (out), "r" (mem), "r"(&out[8]) \
	 : #QD1, #QD2, #QD3, #QD4, "memory", "r4", "0" \
	 ); \
   printf("%s :: Result 0x%08x 0x%08x 0x%08x 0x%08x "\
	 "0x%08x 0x%08x 0x%08x 0x%08x  delta %d\n", \
	 instruction, out[0], out[1], out[2], out[3], out[4],\
	 out[5], out[6], out[7], (int)out[8]-(int)out); \
}

#define TESTINSN_VLDn_RI(instruction, QD1, QD2, QD3, QD4, RM, RMval) \
{ \
   unsigned int out[9];  \
   unsigned int addr = 0;    \
   \
   __asm__ volatile( \
	 "mov %0, %2\n\t" \
	 "vmov.i8 " #QD1 ", #0x55" "\n\t" \
	 "vmov.i8 " #QD2 ", #0x55" "\n\t" \
	 "vmov.i8 " #QD3 ", #0x55" "\n\t" \
	 "vmov.i8 " #QD4 ", #0x55" "\n\t" \
	 "mov " #RM ", %4\n\t" \
	 instruction ", [%0], " #RM "\n\t" \
	 "mov r4, %1\n\t" \
	 "vstmia %1!, {" #QD1 "}\n\t" \
	 "vstmia %1!, {" #QD2 "}\n\t" \
	 "vstmia %1!, {" #QD3 "}\n\t" \
	 "vstmia %1!, {" #QD4 "}\n\t" \
	 "str %0, [%3]\n\t" \
	 "mov %1, r4\n\t" \
	 : "+r" (addr) \
	 : "r" (out), "r" (mem), "r"(&out[8]), "r"(RMval) \
	 : #QD1, #QD2, #QD3, #QD4, "memory", "r4", #RM \
	 ); \
   printf("%s :: Result 0x%08x 0x%08x 0x%08x 0x%08x "\
	 "0x%08x 0x%08x 0x%08x 0x%08x  delta %d\n", \
	 instruction, out[0], out[1], out[2], out[3], out[4],\
	 out[5], out[6], out[7], (int)out[8]-(int)addr); \
}


#define TESTINSN_VSTn_RI(instruction, QD1, QD2, QD3, QD4, RM, RMval) \
{ \
   unsigned int out[9]; \
   unsigned int addr = 0;    \
   \
   memset(out, 0x55, 8 * (sizeof(unsigned int)));\
   __asm__ volatile( \
	 "mov %0, %1\n\t" \
	 "mov r4, %2\n\t" \
	 "vldmia r4!, {" #QD1 "}\n\t" \
	 "vldmia r4!, {" #QD2 "}\n\t" \
	 "vldmia r4!, {" #QD3 "}\n\t" \
	 "vldmia r4!, {" #QD4 "}\n\t" \
	 "mov " #RM ", %4\n\t" \
	 instruction ", [%0], " #RM "\n\t" \
	 "str %0, [%3]\n\t" \
	 : "+r" (addr) \
	 : "r" (out), "r" (mem), "r"(&out[8]), "r"(#RMval) \
	 : #QD1, #QD2, #QD3, #QD4, "memory", "r4", #RM \
	 ); \
   printf("%s :: Result 0x%08x 0x%08x 0x%08x 0x%08x "\
	 "0x%08x 0x%08x 0x%08x 0x%08x  delta %d\n", \
	 instruction, out[0], out[1], out[2], out[3], out[4],\
	 out[5], out[6], out[7], (int)out[8]-(int)out); \
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
} \
{ \
   unsigned int out[2]; \
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
   printf("%s :: Qd 0x%08x 0x%08x  Qm (" #QMtype ")0x%08x" \
	 "  Qn (" #QNtype ")0x%08x\n", \
	 instruction, out[1], out[0], QMval, QNval); \
}

#define TESTINSN_bin_f(instruction, QD, QM, QMtype, QMval, QN, QNtype, QNval) \
{ \
  unsigned int out[2]; \
\
  __asm__ volatile( \
      "vdup.i32 " #QD ", %3\n\t" \
      "vdup." #QMtype " " #QM ", %1\n\t" \
      "vdup." #QNtype " " #QN ", %2\n\t" \
      instruction "\n\t" \
      "vstmia %0, {" #QD "}\n\t" \
      : \
      : "r" (out), "r" (QMval), "r" (QNval), "r"(0x3f800000) \
      : #QD, #QM, #QN, "memory" \
      ); \
  printf("%s :: Qd 0x%08x 0x%08x  Qm (" #QMtype ")0x%08x" \
      "  Qn (" #QNtype ")0x%08x\n", \
      instruction, out[1], out[0], QMval, QNval); \
} \
{ \
     unsigned int out[2]; \
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
     printf("%s :: Qd 0x%08x 0x%08x  Qm (" #QMtype ")0x%08x" \
	         "  Qn (" #QNtype ")0x%08x\n", \
	         instruction, out[1], out[0], QMval, QNval); \
}

#define TESTINSN_tbl(instruction, QD, QM, QMtype, QMval, QN1, QN1type, QN1val, \
        QN2, QN2type, QN2val, QN3, QN3type, QN3val, QN4, QN4type, QN4val) \
{ \
  unsigned int out[2]; \
\
  __asm__ volatile( \
      "vmov.i8 " #QD ", #0x55" "\n\t" \
      "vdup." #QMtype " " #QM ", %1\n\t" \
      "vdup." #QN1type " " #QN1 ", %2\n\t" \
      "vdup." #QN2type " " #QN2 ", %3\n\t" \
      "vdup." #QN3type " " #QN3 ", %4\n\t" \
      "vdup." #QN4type " " #QN4 ", %5\n\t" \
      instruction "\n\t" \
      "vstmia %0, {" #QD "}\n\t" \
      : \
      : "r" (out), "r" (QMval), "r" (QN1val), "r" (QN2val), "r" (QN3val), \
        "r" (QN4val) \
      : #QD, #QM, #QN1, #QN2, #QN3, #QN4, "memory" \
      ); \
  printf("%s :: Qd 0x%08x 0x%08x  Qm (" #QMtype ")0x%08x" \
      "  Qn1 (" #QN1type ")0x%08x" \
      "  Qn2 (" #QN2type ")0x%08x" \
      "  Qn3 (" #QN3type ")0x%08x" \
      "  Qn4 (" #QN4type ")0x%08x\n", \
      instruction, out[1], out[0], QMval, QN1val, QN2val, QN3val, QN4val); \
} \
{ \
   unsigned int out[2]; \
   unsigned int addr = 0; \
   \
   __asm__ volatile( \
	 "mov %6, %7\n\t" \
	 "vmov.i8 " #QD ", #0x55" "\n\t" \
	 "vdup." #QMtype " " #QM ", %1\n\t" \
	 "vldmia %6!, {" #QN1 "}\n\t" \
	 "vdup." #QN2type " " #QN2 ", %3\n\t" \
	 "vldmia %6!, {" #QN3 "}\n\t" \
	 "vdup." #QN4type " " #QN4 ", %5\n\t" \
	 instruction "\n\t" \
	 "vstmia %0, {" #QD "}\n\t" \
	 : \
	 : "r" (out), "r" (QMval), "r" (QN1val), "r" (QN2val), "r" (QN3val), \
	 "r" (QN4val), "r" (addr), "r" (mem) \
	 : #QD, #QM, #QN1, #QN2, #QN3, #QN4, "memory" \
	 ); \
   printf("%s :: Qd 0x%08x 0x%08x  Qm (" #QMtype ")0x%08x" \
	 "  Qn1 (" #QN1type ")0x%08x" \
	 "  Qn2 (" #QN2type ")0x%08x" \
	 "  Qn3 (" #QN3type ")0x%08x" \
	 "  Qn4 (" #QN4type ")0x%08x\n", \
	 instruction, out[1], out[0], QMval, QN1val, QN2val, QN3val, QN4val); \
}

#define TESTINSN_tbl_1(instruction, QD, QM, QMtype, QMval, QN1, QN1type, QN1val) \
    TESTINSN_tbl(instruction, QD, QM, QMtype, QMval, QN1, QN1type, QN1val, \
            QN1, QN1type, QN1val, QN1, QN1type, QN1val, QN1, QN1type, QN1val)
#define TESTINSN_tbl_2(instruction, QD, QM, QMtype, QMval, QN1, QN1type, QN1val, \
        QN2, QN2type, QN2val) \
    TESTINSN_tbl(instruction, QD, QM, QMtype, QMval, QN1, QN1type, QN1val, \
            QN2, QN2type, QN2val, QN1, QN1type, QN1val, QN2, QN2type, QN2val)
#define TESTINSN_tbl_3(instruction, QD, QM, QMtype, QMval, QN1, QN1type, QN1val, \
        QN2, QN2type, QN2val, QN3, QN3type, QN3val) \
    TESTINSN_tbl(instruction, QD, QM, QMtype, QMval, QN1, QN1type, QN1val, \
            QN2, QN2type, QN2val, QN3, QN3type, QN3val, QN2, QN2type, QN2val)
#define TESTINSN_tbl_4(instruction, QD, QM, QMtype, QMval, QN1, QN1type, QN1val, \
        QN2, QN2type, QN2val, QN3, QN3type, QN3val, QN4, QN4type, QN4val) \
    TESTINSN_tbl(instruction, QD, QM, QMtype, QMval, QN1, QN1type, QN1val, \
            QN2, QN2type, QN2val, QN3, QN3type, QN3val, QN4, QN4type, QN4val)

#define TESTINSN_bin_q(instruction, QD, QM, QMtype, QMval, QN, QNtype, QNval) \
{ \
  unsigned int out[2]; \
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
  printf("%s :: Qd 0x%08x 0x%08x  Qm (" #QMtype ")0x%08x" \
      "  Qn (" #QNtype ")0x%08x  fpscr: %08x\n", \
      instruction, out[1], out[0], QMval, QNval, fpscr); \
} \
{ \
     unsigned int out[2]; \
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
}

#define TESTINSN_dual(instruction, QM, QMtype, QMval, QN, QNtype, QNval) \
{ \
   unsigned int out1[2]; \
   unsigned int out2[2]; \
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
	 : #QM, #QN, "memory" \
	 ); \
   printf("%s :: Qm 0x%08x 0x%08x  Qn 0x%08x 0x%08x  Qm (" #QMtype ")0x%08x" \
	 "  Qn (" #QNtype ")0x%08x\n", \
	 instruction, out1[1], out1[0], out2[1], out2[0], QMval, QNval); \
} \
{ \
     unsigned int out1[2]; \
     unsigned int out2[2]; \
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
     printf("%s :: Qm 0x%08x 0x%08x  Qn 0x%08x 0x%08x  Qm (" #QMtype ")0x%08x" \
	         "  Qn (" #QNtype ")0x%08x\n", \
	         instruction, out1[1], out1[0], out2[1], out2[0], QMval, QNval); \
}

#if 0
#define TESTINSN_2reg_shift(instruction, QD, QM, QMtype, QMval, imm) \
{ \
  unsigned int out[2]; \
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
  printf("%s, #" #imm " :: Qd 0x%08x 0x%08x  Qm (" #QMtype ")0x%08x", \
      instruction, out[1], out[0], QMval); \
}
#endif

int main(int argc, char **argv)
{
    printf("----- VMOV (immediate) -----\n");
    TESTINSN_imm("vmov.i32 d0", d0, 0x7);
    TESTINSN_imm("vmov.i16 d1", d1, 0x7);
    TESTINSN_imm("vmov.i8 d2", d2, 0x7);
    TESTINSN_imm("vmov.i32 d5", d5, 0x700);
    TESTINSN_imm("vmov.i16 d7", d7, 0x700);
    TESTINSN_imm("vmov.i32 d10", d10, 0x70000);
    TESTINSN_imm("vmov.i32 d12", d12, 0x7000000);
    TESTINSN_imm("vmov.i32 d13", d13, 0x7FF);
    TESTINSN_imm("vmov.i32 d14", d14, 0x7FFFF);
    TESTINSN_imm("vmov.i64 d15", d15, 0xFF0000FF00FFFF00);

    printf("----- VMVN (immediate) -----\n");
    TESTINSN_imm("vmvn.i32 d0", d0, 0x7);
    TESTINSN_imm("vmvn.i16 d1", d1, 0x7);
    TESTINSN_imm("vmvn.i8 d2", d2, 0x7);
    TESTINSN_imm("vmvn.i32 d5", d5, 0x700);
    TESTINSN_imm("vmvn.i16 d7", d7, 0x700);
    TESTINSN_imm("vmvn.i32 d10", d10, 0x70000);
    TESTINSN_imm("vmvn.i32 d13", d13, 0x7000000);
    TESTINSN_imm("vmvn.i32 d11", d11, 0x7FF);
    TESTINSN_imm("vmvn.i32 d14", d14, 0x7FFFF);
    TESTINSN_imm("vmvn.i64 d15", d15, 0xFF0000FF00FFFF00);

    printf("----- VORR (immediate) -----\n");
    TESTINSN_imm("vorr.i32 d0", d0, 0x7);
    TESTINSN_imm("vorr.i16 d2", d2, 0x7);
    TESTINSN_imm("vorr.i32 d8", d8, 0x700);
    TESTINSN_imm("vorr.i16 d6", d6, 0x700);
    TESTINSN_imm("vorr.i32 d14", d14, 0x70000);
    TESTINSN_imm("vorr.i32 d15", d15, 0x7000000);

    printf("----- VBIC (immediate) -----\n");
    TESTINSN_imm("vbic.i32 d0", d0, 0x7);
    TESTINSN_imm("vbic.i16 d3", d3, 0x7);
    TESTINSN_imm("vbic.i32 d5", d5, 0x700);
    TESTINSN_imm("vbic.i16 d8", d8, 0x700);
    TESTINSN_imm("vbic.i32 d10", d10, 0x70000);
    TESTINSN_imm("vbic.i32 d15", d15, 0x7000000);

    printf("---- VMVN (register) ----\n");
    TESTINSN_un("vmvn d0, d1", d0, d1, i32, 24);
    TESTINSN_un("vmvn d10, d15", d10, d15, i32, 24);
    TESTINSN_un("vmvn d0, d14", d0, d14, i32, 24);

    printf("---- VMOV (register) ----\n");
    TESTINSN_un("vmov d0, d1", d0, d1, i32, 24);
    TESTINSN_un("vmov d10, d15", d10, d15, i32, 24);
    TESTINSN_un("vmov d0, d14", d0, d14, i32, 24);

    printf("---- VDUP (ARM core register) (tested indirectly) ----\n");
    TESTINSN_un("vmov d0, d1", d0, d1, i8, 7);
    TESTINSN_un("vmov d10, d11", d10, d11, i16, 7);
    TESTINSN_un("vmov d0, d15", d0, d15, i32, 7);

    printf("---- VADD ----\n");
    TESTINSN_bin("vadd.i32 d0, d1, d2", d0, d1, i32, 24, d2, i32, 120);
    TESTINSN_bin("vadd.i64 d0, d1, d2", d0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin("vadd.i32 d0, d1, d2", d0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin("vadd.i16 d0, d1, d2", d0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin("vadd.i8 d0, d1, d2", d0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin("vadd.i8 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vadd.i16 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vadd.i32 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vadd.i64 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vadd.i32 d10, d11, d12", d10, d11, i32, 24, d12, i32, 120);
    TESTINSN_bin("vadd.i64 d13, d14, d15", d13, d14, i32, 140, d15, i32, 120);

    printf("---- VSUB ----\n");
    TESTINSN_bin("vsub.i32 d0, d1, d2", d0, d1, i32, 24, d2, i32, 120);
    TESTINSN_bin("vsub.i64 d0, d1, d2", d0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin("vsub.i32 d0, d1, d2", d0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin("vsub.i16 d0, d1, d2", d0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin("vsub.i8 d0, d1, d2", d0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin("vsub.i8 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vsub.i16 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vsub.i32 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vsub.i64 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vsub.i32 d10, d11, d12", d10, d11, i32, 24, d12, i32, 120);
    TESTINSN_bin("vsub.i64 d13, d14, d15", d13, d14, i32, 140, d15, i32, 120);

    printf("---- VAND ----\n");
    TESTINSN_bin("vand d0, d1, d2", d0, d1, i8, 0x24, d2, i16, 0x77);
    TESTINSN_bin("vand d4, d6, d5", d4, d6, i8, 0xff, d5, i16, 0x57);
    TESTINSN_bin("vand d10, d11, d12", d10, d11, i8, 0xfe, d12, i8, 0xed);
    TESTINSN_bin("vand d15, d15, d15", d15, d15, i8, 0xff, d15, i8, 0xff);

    printf("---- VBIC ----\n");
    TESTINSN_bin("vbic d0, d1, d2", d0, d1, i8, 0x24, d2, i16, 0x77);
    TESTINSN_bin("vbic d4, d6, d5", d4, d6, i8, 0xff, d5, i16, 0x57);
    TESTINSN_bin("vbic d10, d11, d12", d10, d11, i8, 0xfe, d12, i8, 0xed);
    TESTINSN_bin("vbic d15, d15, d15", d15, d15, i8, 0xff, d15, i8, 0xff);

    printf("---- VORR ----\n");
    TESTINSN_bin("vorr d0, d1, d2", d0, d1, i8, 0x24, d2, i16, 0x73);
    TESTINSN_bin("vorr d7, d3, d0", d7, d3, i8, 0x24, d0, i16, 0xff);
    TESTINSN_bin("vorr d4, d4, d4", d4, d4, i16, 0xff, d4, i16, 0xff);
    TESTINSN_bin("vorr d2, d3, d15", d2, d3, i32, 0x24, d15, i32, 0x1f);

    printf("---- VORN ----\n");
    TESTINSN_bin("vorn d0, d1, d2", d0, d1, i8, 0x24, d2, i16, 0x73);
    TESTINSN_bin("vorn d7, d3, d0", d7, d3, i8, 0x24, d0, i16, 0xff);
    TESTINSN_bin("vorn d4, d4, d4", d4, d4, i16, 0xff, d4, i16, 0xff);
    TESTINSN_bin("vorn d2, d3, d15", d2, d3, i32, 0x24, d15, i32, 0x1f);

    printf("---- VEOR ----\n");
    TESTINSN_bin("veor d0, d1, d2", d0, d1, i8, 0x24, d2, i16, 0x77);
    TESTINSN_bin("veor d4, d6, d5", d4, d6, i8, 0xff, d5, i16, 0x57);
    TESTINSN_bin("veor d10, d11, d12", d10, d11, i8, 0xfe, d12, i8, 0xed);
    TESTINSN_bin("veor d15, d15, d15", d15, d15, i8, 0xff, d15, i8, 0xff);
    TESTINSN_bin("veor d0, d1, d2", d0, d1, i8, 0x24, d2, i16, 0x73);
    TESTINSN_bin("veor d7, d3, d0", d7, d3, i8, 0x24, d0, i16, 0xff);
    TESTINSN_bin("veor d4, d4, d4", d4, d4, i16, 0xff, d4, i16, 0xff);
    TESTINSN_bin("veor d2, d3, d15", d2, d3, i32, 0x24, d15, i32, 0x1f);

    printf("---- VBSL ----\n");
    TESTINSN_bin("vbsl d0, d1, d2", d0, d1, i8, 0x24, d2, i16, 0x77);
    TESTINSN_bin("vbsl d4, d6, d5", d4, d6, i8, 0xff, d5, i16, 0x57);
    TESTINSN_bin("vbsl d10, d11, d12", d10, d11, i8, 0xfe, d12, i8, 0xed);
    TESTINSN_bin("vbsl d15, d15, d15", d15, d15, i8, 0xff, d15, i8, 0xff);
    TESTINSN_bin("vbsl d0, d1, d2", d0, d1, i8, 0x24, d2, i16, 0x73);
    TESTINSN_bin("vbsl d7, d3, d0", d7, d3, i8, 0x24, d0, i16, 0xff);
    TESTINSN_bin("vbsl d4, d4, d4", d4, d4, i16, 0xff, d4, i16, 0xff);
    TESTINSN_bin("vbsl d2, d3, d15", d2, d3, i32, 0x24, d15, i32, 0x1f);

    printf("---- VBIT ----\n");
    TESTINSN_bin("vbit d0, d1, d2", d0, d1, i8, 0x24, d2, i16, 0x77);
    TESTINSN_bin("vbit d4, d6, d5", d4, d6, i8, 0xff, d5, i16, 0x57);
    TESTINSN_bin("vbit d10, d11, d12", d10, d11, i8, 0xfe, d12, i8, 0xed);
    TESTINSN_bin("vbit d15, d15, d15", d15, d15, i8, 0xff, d15, i8, 0xff);
    TESTINSN_bin("vbit d0, d1, d2", d0, d1, i8, 0x24, d2, i16, 0x73);
    TESTINSN_bin("vbit d7, d3, d0", d7, d3, i8, 0x24, d0, i16, 0xff);
    TESTINSN_bin("vbit d4, d4, d4", d4, d4, i16, 0xff, d4, i16, 0xff);
    TESTINSN_bin("vbit d2, d3, d15", d2, d3, i32, 0x24, d15, i32, 0x1f);

    printf("---- VBIF ----\n");
    TESTINSN_bin("vbif d0, d1, d2", d0, d1, i8, 0x24, d2, i16, 0x77);
    TESTINSN_bin("vbif d4, d6, d5", d4, d6, i8, 0xff, d5, i16, 0x57);
    TESTINSN_bin("vbif d10, d11, d12", d10, d11, i8, 0xfe, d12, i8, 0xed);
    TESTINSN_bin("vbif d15, d15, d15", d15, d15, i8, 0xff, d15, i8, 0xff);
    TESTINSN_bin("vbif d0, d1, d2", d0, d1, i8, 0x24, d2, i16, 0x73);
    TESTINSN_bin("vbif d7, d3, d0", d7, d3, i8, 0x24, d0, i16, 0xff);
    TESTINSN_bin("vbif d4, d4, d4", d4, d4, i16, 0xff, d4, i16, 0xff);
    TESTINSN_bin("vbif d2, d3, d15", d2, d3, i32, 0x24, d15, i32, 0x1f);

    printf("---- VEXT ----\n");
    TESTINSN_bin("vext.8 d0, d1, d2, #0", d0, d1, i8, 0x77, d2, i8, 0xff);
    TESTINSN_bin("vext.8 d0, d1, d2, #1", d0, d1, i8, 0x77, d2, i8, 0xff);
    TESTINSN_bin("vext.8 d0, d1, d2, #7", d0, d1, i8, 0x77, d2, i8, 0xff);
    TESTINSN_bin("vext.8 d0, d1, d2, #6", d0, d1, i8, 0x77, d2, i8, 0xff);
    TESTINSN_bin("vext.8 d10, d11, d12, #4", d10, d11, i8, 0x77, d12, i8, 0xff);
    TESTINSN_bin("vext.8 d0, d5, d15, #5", d0, d5, i8, 0x77, d15, i8, 0xff);

    printf("---- VHADD ----\n");
    TESTINSN_bin("vhadd.s32 d0, d1, d2", d0, d1, i32, 24, d2, i32, 120);
    TESTINSN_bin("vhadd.s32 d0, d1, d2", d0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin("vhadd.s16 d0, d1, d2", d0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin("vhadd.s8 d0, d1, d2", d0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin("vhadd.s8 d0, d1, d2", d0, d1, i8, 141, d2, i8, 121);
    TESTINSN_bin("vhadd.s8 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vhadd.s16 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vhadd.s32 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vhadd.s32 d10, d11, d12", d10, d11, i32, 24, d12, i32, 120);
    TESTINSN_bin("vhadd.u32 d0, d1, d2", d0, d1, i32, 24, d2, i32, 120);
    TESTINSN_bin("vhadd.u32 d0, d1, d2", d0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin("vhadd.u16 d0, d1, d2", d0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin("vhadd.u8 d0, d1, d2", d0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin("vhadd.u8 d0, d1, d2", d0, d1, i8, 141, d2, i8, 121);
    TESTINSN_bin("vhadd.u8 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vhadd.u16 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vhadd.u32 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vhadd.u32 d10, d11, d12", d10, d11, i32, 24, d12, i32, 120);

    printf("---- VHSUB ----\n");
    TESTINSN_bin("vhsub.s32 d0, d1, d2", d0, d1, i32, 24, d2, i32, 120);
    TESTINSN_bin("vhsub.s32 d0, d1, d2", d0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin("vhsub.s16 d0, d1, d2", d0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin("vhsub.s8 d0, d1, d2", d0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin("vhsub.s8 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vhsub.s16 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vhsub.s32 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vhsub.s32 d10, d11, d12", d10, d11, i32, 24, d12, i32, 120);
    TESTINSN_bin("vhsub.u32 d0, d1, d2", d0, d1, i32, 24, d2, i32, 120);
    TESTINSN_bin("vhsub.u32 d0, d1, d2", d0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin("vhsub.u16 d0, d1, d2", d0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin("vhsub.u8 d0, d1, d2", d0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin("vhsub.u8 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vhsub.u16 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vhsub.u32 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vhsub.u32 d10, d11, d12", d10, d11, i32, 24, d12, i32, 120);

    printf("---- VQADD ----\n");
    TESTINSN_bin_q("vqadd.s32 d0, d1, d2", d0, d1, i32, 24, d2, i32, 120);
    TESTINSN_bin_q("vqadd.s32 d0, d1, d2", d0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin_q("vqadd.s16 d0, d1, d2", d0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin_q("vqadd.s8 d0, d1, d2", d0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin_q("vqadd.s8 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2);
    TESTINSN_bin_q("vqadd.s16 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2);
    TESTINSN_bin_q("vqadd.s32 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2);
    TESTINSN_bin_q("vqadd.s32 d10, d11, d12", d10, d11, i32, 24, d12, i32, 120);
    TESTINSN_bin_q("vqadd.u32 d0, d1, d2", d0, d1, i32, 24, d2, i32, 120);
    TESTINSN_bin_q("vqadd.u32 d0, d1, d2", d0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin_q("vqadd.u16 d0, d1, d2", d0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin_q("vqadd.u8 d0, d1, d2", d0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin_q("vqadd.u8 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2);
    TESTINSN_bin_q("vqadd.u16 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2);
    TESTINSN_bin_q("vqadd.u32 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2);
    TESTINSN_bin_q("vqadd.u32 d10, d11, d12", d10, d11, i32, 24, d12, i32, 120);

    printf("---- VQSUB ----\n");
    TESTINSN_bin_q("vqsub.s32 d0, d1, d2", d0, d1, i32, 24, d2, i32, 120);
    TESTINSN_bin_q("vqsub.s32 d0, d1, d2", d0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin_q("vqsub.s16 d0, d1, d2", d0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin_q("vqsub.s8 d0, d1, d2", d0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin_q("vqsub.s8 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2);
    TESTINSN_bin_q("vqsub.s16 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2);
    TESTINSN_bin_q("vqsub.s32 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2);
    TESTINSN_bin_q("vqsub.s32 d10, d11, d12", d10, d11, i32, 24, d12, i32, 120);
    TESTINSN_bin_q("vqsub.u32 d0, d1, d2", d0, d1, i32, 24, d2, i32, 120);
    TESTINSN_bin_q("vqsub.u32 d0, d1, d2", d0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin_q("vqsub.u16 d0, d1, d2", d0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin_q("vqsub.u8 d0, d1, d2", d0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin_q("vqsub.u8 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2);
    TESTINSN_bin_q("vqsub.u16 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2);
    TESTINSN_bin_q("vqsub.u32 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2);
    TESTINSN_bin_q("vqsub.u32 d10, d11, d12", d10, d11, i32, 24, d12, i32, 120);

    printf("---- VRHADD ----\n");
    TESTINSN_bin("vrhadd.s32 d0, d1, d2", d0, d1, i32, 25, d2, i32, 120);
    TESTINSN_bin("vrhadd.s32 d0, d1, d2", d0, d1, i32, 25, d2, i32, 121);
    TESTINSN_bin("vrhadd.s32 d0, d1, d2", d0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin("vrhadd.s16 d0, d1, d2", d0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin("vrhadd.s8 d0, d1, d2", d0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin("vrhadd.s8 d5, d7, d5", d5, d7, i32, (1 << 31) + 1, d5, i32, (1 << 31) + 2);
    TESTINSN_bin("vrhadd.s16 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vrhadd.s32 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vrhadd.s8 d5, d7, d5", d5, d7, i32, (1 << 31) + 1, d5, i32, (1 << 31) + 3);
    TESTINSN_bin("vrhadd.s16 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 3);
    TESTINSN_bin("vrhadd.s32 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 3);
    TESTINSN_bin("vrhadd.s8 d5, d7, d5", d5, d7, i32, (1 << 31) + 4, d5, i32, (1 << 31) + 2);
    TESTINSN_bin("vrhadd.s16 d0, d1, d2", d0, d1, i32, (1 << 31) + 4, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vrhadd.s32 d0, d1, d2", d0, d1, i32, (1 << 31) + 4, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vrhadd.s32 d10, d11, d12", d10, d11, i32, 24, d12, i32, 120);
    TESTINSN_bin("vrhadd.u32 d0, d1, d2", d0, d1, i32, 25, d2, i32, 120);
    TESTINSN_bin("vrhadd.u32 d0, d1, d2", d0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin("vrhadd.u16 d0, d1, d2", d0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin("vrhadd.u8 d0, d1, d2", d0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin("vrhadd.u8 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vrhadd.u16 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vrhadd.u32 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vrhadd.u8 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 3);
    TESTINSN_bin("vrhadd.u16 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 3);
    TESTINSN_bin("vrhadd.u32 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 3);
    TESTINSN_bin("vrhadd.u8 d0, d1, d2", d0, d1, i32, (1 << 31) + 4, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vrhadd.u16 d0, d1, d2", d0, d1, i32, (1 << 31) + 4, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vrhadd.u32 d0, d1, d2", d0, d1, i32, (1 << 31) + 4, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vrhadd.u32 d10, d11, d12", d10, d11, i32, 24, d12, i32, 120);

    printf("---- VCGT ----\n");
    TESTINSN_bin("vcgt.s32 d0, d1, d2", d0, d1, i32, 25, d2, i32, 120);
    TESTINSN_bin("vcgt.s32 d0, d1, d2", d0, d1, i32, 25, d2, i32, 121);
    TESTINSN_bin("vcgt.s32 d0, d1, d2", d0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin("vcgt.s16 d0, d1, d2", d0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin("vcgt.s8 d0, d1, d2", d0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin("vcgt.s32 d0, d1, d2", d0, d1, i32, 120, d2, i32, 120);
    TESTINSN_bin("vcgt.s16 d0, d1, d2", d0, d1, i32, 120, d2, i32, 120);
    TESTINSN_bin("vcgt.s8 d0, d1, d2", d0, d1, i32, 120, d2, i32, 120);
    TESTINSN_bin("vcgt.s32 d0, d1, d2", d0, d1, i32, 120, d2, i32, 140);
    TESTINSN_bin("vcgt.s16 d0, d1, d2", d0, d1, i32, 120, d2, i32, 140);
    TESTINSN_bin("vcgt.s8 d0, d1, d2", d0, d1, i32, 120, d2, i32, 140);
    TESTINSN_bin("vcgt.s8 d5, d7, d5", d5, d7, i32, (1 << 31) + 3, d5, i32, (1 << 31) + 2);
    TESTINSN_bin("vcgt.s16 d0, d1, d2", d0, d1, i32, (1 << 31) + 3, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vcgt.s32 d0, d1, d2", d0, d1, i32, (1 << 31) + 3, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vcgt.s8 d5, d7, d5", d5, d7, i32, (1 << 31) + 1, d5, i32, (1 << 31) + 3);
    TESTINSN_bin("vcgt.s16 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 3);
    TESTINSN_bin("vcgt.s32 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 3);
    TESTINSN_bin("vcgt.s8 d5, d7, d5", d5, d7, i32, (1 << 31) + 2, d5, i32, (1 << 31) + 2);
    TESTINSN_bin("vcgt.s16 d0, d1, d2", d0, d1, i32, (1 << 31) + 2, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vcgt.s32 d0, d1, d2", d0, d1, i32, (1 << 31) + 2, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vcgt.s32 d10, d11, d12", d10, d11, i32, 24, d12, i32, 120);
    TESTINSN_bin("vcgt.u32 d0, d1, d2", d0, d1, i32, 25, d2, i32, 120);
    TESTINSN_bin("vcgt.u32 d0, d1, d2", d0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin("vcgt.u16 d0, d1, d2", d0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin("vcgt.u8 d0, d1, d2", d0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin("vcgt.u32 d0, d1, d2", d0, d1, i32, 120, d2, i32, 120);
    TESTINSN_bin("vcgt.u16 d0, d1, d2", d0, d1, i32, 120, d2, i32, 120);
    TESTINSN_bin("vcgt.u8 d0, d1, d2", d0, d1, i32, 120, d2, i32, 120);
    TESTINSN_bin("vcgt.u32 d0, d1, d2", d0, d1, i32, 140, d2, i32, 140);
    TESTINSN_bin("vcgt.u16 d0, d1, d2", d0, d1, i32, 140, d2, i32, 140);
    TESTINSN_bin("vcgt.u8 d0, d1, d2", d0, d1, i32, 140, d2, i32, 140);
    TESTINSN_bin("vcgt.u8 d0, d1, d2", d0, d1, i32, (1 << 31) + 3, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vcgt.u16 d0, d1, d2", d0, d1, i32, (1 << 31) + 3, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vcgt.u32 d0, d1, d2", d0, d1, i32, (1 << 31) + 3, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vcgt.u8 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 3);
    TESTINSN_bin("vcgt.u16 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 3);
    TESTINSN_bin("vcgt.u32 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 3);
    TESTINSN_bin("vcgt.u8 d0, d1, d2", d0, d1, i32, (1 << 31) + 2, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vcgt.u16 d0, d1, d2", d0, d1, i32, (1 << 31) + 2, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vcgt.u32 d0, d1, d2", d0, d1, i32, (1 << 31) + 2, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vcgt.u32 d10, d11, d12", d10, d11, i32, 24, d12, i32, 120);

    printf("---- VCGE ----\n");
    TESTINSN_bin("vcge.s32 d0, d1, d2", d0, d1, i32, 25, d2, i32, 120);
    TESTINSN_bin("vcge.s32 d0, d1, d2", d0, d1, i32, 25, d2, i32, 121);
    TESTINSN_bin("vcge.s32 d0, d1, d2", d0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin("vcge.s16 d0, d1, d2", d0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin("vcge.s8 d0, d1, d2", d0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin("vcge.s32 d0, d1, d2", d0, d1, i32, 120, d2, i32, 120);
    TESTINSN_bin("vcge.s16 d0, d1, d2", d0, d1, i32, 120, d2, i32, 120);
    TESTINSN_bin("vcge.s8 d0, d1, d2", d0, d1, i32, 120, d2, i32, 120);
    TESTINSN_bin("vcge.s32 d0, d1, d2", d0, d1, i32, 120, d2, i32, 140);
    TESTINSN_bin("vcge.s16 d0, d1, d2", d0, d1, i32, 120, d2, i32, 140);
    TESTINSN_bin("vcge.s8 d0, d1, d2", d0, d1, i32, 120, d2, i32, 140);
    TESTINSN_bin("vcge.s8 d5, d7, d5", d5, d7, i32, (1 << 31) + 3, d5, i32, (1 << 31) + 2);
    TESTINSN_bin("vcge.s16 d0, d1, d2", d0, d1, i32, (1 << 31) + 3, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vcge.s32 d0, d1, d2", d0, d1, i32, (1 << 31) + 3, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vcge.s8 d5, d7, d5", d5, d7, i32, (1 << 31) + 1, d5, i32, (1 << 31) + 3);
    TESTINSN_bin("vcge.s16 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 3);
    TESTINSN_bin("vcge.s32 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 3);
    TESTINSN_bin("vcge.s8 d5, d7, d5", d5, d7, i32, (1 << 31) + 2, d5, i32, (1 << 31) + 2);
    TESTINSN_bin("vcge.s16 d0, d1, d2", d0, d1, i32, (1 << 31) + 2, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vcge.s32 d0, d1, d2", d0, d1, i32, (1 << 31) + 2, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vcge.s32 d10, d11, d12", d10, d11, i32, 24, d12, i32, 120);
    TESTINSN_bin("vcge.u32 d0, d1, d2", d0, d1, i32, 25, d2, i32, 120);
    TESTINSN_bin("vcge.u32 d0, d1, d2", d0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin("vcge.u16 d0, d1, d2", d0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin("vcge.u8 d0, d1, d2", d0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin("vcge.u32 d0, d1, d2", d0, d1, i32, 120, d2, i32, 120);
    TESTINSN_bin("vcge.u16 d0, d1, d2", d0, d1, i32, 120, d2, i32, 120);
    TESTINSN_bin("vcge.u8 d0, d1, d2", d0, d1, i32, 120, d2, i32, 120);
    TESTINSN_bin("vcge.u32 d0, d1, d2", d0, d1, i32, 140, d2, i32, 140);
    TESTINSN_bin("vcge.u16 d0, d1, d2", d0, d1, i32, 140, d2, i32, 140);
    TESTINSN_bin("vcge.u8 d0, d1, d2", d0, d1, i32, 140, d2, i32, 140);
    TESTINSN_bin("vcge.u8 d0, d1, d2", d0, d1, i32, (1 << 31) + 3, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vcge.u16 d0, d1, d2", d0, d1, i32, (1 << 31) + 3, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vcge.u32 d0, d1, d2", d0, d1, i32, (1 << 31) + 3, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vcge.u8 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 3);
    TESTINSN_bin("vcge.u16 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 3);
    TESTINSN_bin("vcge.u32 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 3);
    TESTINSN_bin("vcge.u8 d0, d1, d2", d0, d1, i32, (1 << 31) + 2, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vcge.u16 d0, d1, d2", d0, d1, i32, (1 << 31) + 2, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vcge.u32 d0, d1, d2", d0, d1, i32, (1 << 31) + 2, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vcge.u32 d10, d11, d12", d10, d11, i32, 24, d12, i32, 120);

    printf("---- VSHL (register) ----\n");
    TESTINSN_bin("vshl.s8 d0, d1, d2", d0, d1, i32, 24, d2, i32, 1);
    TESTINSN_bin("vshl.s8 d8, d1, d12", d8, d1, i32, 24, d12, i32, 8);
    TESTINSN_bin("vshl.s8 d10, d31, d7", d10, d31, i32, 24, d7, i32, 4);
    TESTINSN_bin("vshl.s16 d3, d8, d11", d3, d8, i32, 14, d11, i32, 2);
    TESTINSN_bin("vshl.s16 d5, d12, d14", d5, d12, i32, (1 << 30), d14, i32, 1);
    TESTINSN_bin("vshl.s16 d15, d2, d1", d15, d2, i32, (1 << 30), d1, i32, 11); 
    TESTINSN_bin("vshl.s32 d9, d12, d19", d9, d12, i32, (1 << 31) + 2, d19, i32, 2);
    TESTINSN_bin("vshl.s32 d11, d22, d0", d11, d22, i32, -1, d0, i32, 12); 
    TESTINSN_bin("vshl.s32 d5, d2, d3", d5, d2, i32, (1 << 30), d3, i32, 21); 
    TESTINSN_bin("vshl.s64 d15, d12, d4", d15, d12, i32, 5, d4, i32, 20); 
    TESTINSN_bin("vshl.s64 d8, d2, d4", d8, d2, i32, 15, d4, i32, 4);
    TESTINSN_bin("vshl.s64 d5, d12, d4", d5, d12, i32, (1 << 31) + 1, d4, i32, 30); 
    TESTINSN_bin("vshl.s64 d15, d2, d4", d15, d2, i32, 0xffabcd59, d4, i32, 0xabcdefab); 
    TESTINSN_bin("vshl.s64 d8, d2, d4", d8, d2, i32, 15, d4, i32, 0x400bb5);
    TESTINSN_bin("vshl.s64 d5, d12, d4", d5, d12, i32, (1 << 31) + 1, d4, i32, 0x30abcff); 
    TESTINSN_bin("vshl.u8 d0, d1, d2", d0, d1, i32, 24, d2, i32, 1);
    TESTINSN_bin("vshl.u8 d8, d1, d12", d8, d1, i32, 24, d12, i32, 8);
    TESTINSN_bin("vshl.u8 d10, d11, d7", d10, d11, i32, 24, d7, i32, 4);
    TESTINSN_bin("vshl.u16 d3, d8, d11", d3, d8, i32, 14, d11, i32, 2);
    TESTINSN_bin("vshl.u16 d5, d12, d14", d5, d12, i32, (1 << 30), d14, i32, 1);
    TESTINSN_bin("vshl.u16 d15, d2, d1", d15, d2, i32, (1 << 30), d1, i32, 11);
    TESTINSN_bin("vshl.u32 d9, d12, d15", d9, d12, i32, (1 << 31) + 2, d15, i32, 2);
    TESTINSN_bin("vshl.u32 d11, d2, d0", d11, d2, i32, -1, d0, i32, 12);
    TESTINSN_bin("vshl.u32 d5, d2, d3", d5, d2, i32, (1 << 30), d3, i32, 21);
    TESTINSN_bin("vshl.u64 d15, d12, d4", d15, d12, i32, 5, d4, i32, 20);
    TESTINSN_bin("vshl.u64 d8, d2, d4", d8, d2, i32, 15, d4, i32, 4);
    TESTINSN_bin("vshl.u64 d5, d12, d4", d5, d12, i32, (1 << 31) + 1, d4, i32, 30);
    TESTINSN_bin("vshl.u64 d15, d2, d4", d15, d2, i32, 0xffabcd59, d4, i32, 0xabcdefab); 
    TESTINSN_bin("vshl.u64 d8, d2, d4", d8, d2, i32, 15, d4, i32, 0x400bb5);
    TESTINSN_bin("vshl.u64 d5, d12, d4", d5, d12, i32, (1 << 31) + 1, d4, i32, 0x30abcff); 

    printf("---- VQSHL (register) ----\n");
    TESTINSN_bin_q("vqshl.s64 d0, d1, d2", d0, d1, i32, 1, d2, i32, 1);
    TESTINSN_bin_q("vqshl.s64 d3, d4, d5", d3, d4, i32, -127, d5, i32, 1);
    TESTINSN_bin_q("vqshl.s64 d3, d4, d5", d3, d4, i32, -127, d5, i32, -3);
    TESTINSN_bin_q("vqshl.s64 d0, d1, d2", d0, d1, i32, 16, d2, i32, 14);
    TESTINSN_bin_q("vqshl.s64 d13, d14, d31", d13, d14, i32, -17, d31, i32, -26);
    TESTINSN_bin_q("vqshl.s64 d7, d8, d2", d7, d8, i32, 24, d2, i32, -60);
    TESTINSN_bin_q("vqshl.s32 d3, d4, d15", d3, d4, i32, 127, d15, i32, -30);
    TESTINSN_bin_q("vqshl.s32 d2, d8, d4", d2, d8, i32, -11, d4, i32, -4);
    TESTINSN_bin_q("vqshl.s32 d12, d11, d13", d12, d11, i32, -120, d13, i32, -9);
    TESTINSN_bin_q("vqshl.s32 d0, d1, d2", d0, d1, i32, 34, d2, i32, -7);
    TESTINSN_bin_q("vqshl.s32 d9, d30, d11", d9, d30, i32, (1 << 31) + 8, d11, i32, -1);
    TESTINSN_bin_q("vqshl.s32 d13, d3, d5", d13, d3, i32, (1 << 27), d5, i32, 3);
    TESTINSN_bin_q("vqshl.s16 d11, d10, d2", d11, d10, i32, (1 << 31), d2, i32, -31);
    TESTINSN_bin_q("vqshl.s16 d3, d14, d7", d3, d14, i32, (1 << 31), d7, i32, -3);
    TESTINSN_bin_q("vqshl.s16 d0, d11, d2", d0, d11, i32, (1 << 31) + 256, d2, i32, -1);
    TESTINSN_bin_q("vqshl.s16 d1, d2, d3", d1, d2, i32, (1 << 31) + 256, d3, i32, -31);
    TESTINSN_bin_q("vqshl.s16 d3, d4, d5", d3, d4, i32, (1 << 31) + (1 << 29), d5, i32, -13);
    TESTINSN_bin_q("vqshl.s16 d0, d15, d2", d0, d15, i32, 1, d2, i32, 30);
    TESTINSN_bin_q("vqshl.s8 d2, d7, d11", d2, d7, i32, -1, d11, i32, 40);
    TESTINSN_bin_q("vqshl.s8 d13, d1, d2", d13, d1, i32, -4, d2, i32, 30);
    TESTINSN_bin_q("vqshl.s8 d3, d7, d5", d3, d7, i32, (1 << 31) + 11, d5, i32, 3);
    TESTINSN_bin_q("vqshl.s8 d10, d11, d12", d10, d11, i32, (1 << 16), d12, i32, 16);
    TESTINSN_bin_q("vqshl.s8 d6, d7, d8", d6, d7, i32, (1 << 30), d8, i32, 2);
    TESTINSN_bin_q("vqshl.s8 d10, d11, d12", d10, d11, i32, 24, d12, i32, 120);
    TESTINSN_bin_q("vqshl.u64 d0, d1, d2", d0, d1, i32, 1, d2, i32, 1);
    TESTINSN_bin_q("vqshl.u64 d3, d4, d5", d3, d4, i32, -127, d5, i32, 1);
    TESTINSN_bin_q("vqshl.u64 d3, d4, d5", d3, d4, i32, -127, d5, i32, -3);
    TESTINSN_bin_q("vqshl.u64 d0, d1, d2", d0, d1, i32, 16, d2, i32, 14);
    TESTINSN_bin_q("vqshl.u64 d13, d14, d15", d13, d14, i32, -17, d15, i32, -26);
    TESTINSN_bin_q("vqshl.u64 d7, d8, d2", d7, d8, i32, 24, d2, i32, -60);
    TESTINSN_bin_q("vqshl.u32 d3, d4, d15", d3, d4, i32, 127, d15, i32, -30);
    TESTINSN_bin_q("vqshl.u32 d2, d8, d4", d2, d8, i32, -11, d4, i32, -4);
    TESTINSN_bin_q("vqshl.u32 d12, d31, d13", d12, d31, i32, -120, d13, i32, -9);
    TESTINSN_bin_q("vqshl.u32 d0, d1, d2", d0, d1, i32, 34, d2, i32, -7);
    TESTINSN_bin_q("vqshl.u32 d9, d10, d11", d9, d10, i32, (1 << 31) + 8, d11, i32, -1);
    TESTINSN_bin_q("vqshl.u32 d13, d3, d5", d13, d3, i32, (1 << 27), d5, i32, 3);
    TESTINSN_bin_q("vqshl.u16 d11, d10, d2", d11, d10, i32, (1 << 31), d2, i32, -31);
    TESTINSN_bin_q("vqshl.u16 d3, d14, d7", d3, d14, i32, (1 << 31), d7, i32, -3);
    TESTINSN_bin_q("vqshl.u16 d0, d11, d2", d0, d11, i32, (1 << 31) + 256, d2, i32, -1);
    TESTINSN_bin_q("vqshl.u16 d1, d2, d3", d1, d2, i32, (1 << 31) + 256, d3, i32, -31);
    TESTINSN_bin_q("vqshl.u16 d3, d4, d5", d3, d4, i32, (1 << 31) + (1 << 29), d5, i32, -13);
    TESTINSN_bin_q("vqshl.u16 d0, d15, d2", d0, d15, i32, 1, d2, i32, 30);
    TESTINSN_bin_q("vqshl.u8 d2, d7, d11", d2, d7, i32, -1, d11, i32, 40);
    TESTINSN_bin_q("vqshl.u8 d13, d1, d2", d13, d1, i32, -4, d2, i32, 30);
    TESTINSN_bin_q("vqshl.u8 d3, d7, d5", d3, d7, i32, (1 << 31) + 11, d5, i32, 3);
    TESTINSN_bin_q("vqshl.u8 d10, d11, d12", d10, d11, i32, (1 << 16), d12, i32, 16);
    TESTINSN_bin_q("vqshl.u8 d6, d7, d8", d6, d7, i32, (1 << 30), d8, i32, 2);
    TESTINSN_bin_q("vqshl.u8 d10, d11, d12", d10, d11, i32, 24, d12, i32, 120);

    printf("---- VQSHL / VQSHLU (immediate) ----\n");
    TESTINSN_un_q("vqshl.s64 d0, d1, #1", d0, d1, i32, 1);
    TESTINSN_un_q("vqshl.s64 d31, d30, #1", d31, d30, i32, -127);
    TESTINSN_un_q("vqshl.s64 d5, d4, #0", d5, d4, i32, -127);
    TESTINSN_un_q("vqshl.s64 d5, d4, #63", d5, d4, i32, 16);
    TESTINSN_un_q("vqshl.s64 d5, d4, #60", d5, d4, i32, 16);
    TESTINSN_un_q("vqshl.s64 d5, d4, #59", d5, d4, i32, 16);
    TESTINSN_un_q("vqshl.s64 d5, d4, #58", d5, d4, i32, 16);
    TESTINSN_un_q("vqshl.s64 d5, d4, #17", d5, d4, i32, 16);
    TESTINSN_un_q("vqshl.s64 d5, d4, #63", d5, d4, i32, -1);
    TESTINSN_un_q("vqshl.s64 d5, d4, #60", d5, d4, i32, -1);
    TESTINSN_un_q("vqshl.s64 d5, d4, #7", d5, d4, i32, (1 << 31) + 2);
    TESTINSN_un_q("vqshl.s32 d10, d11, #1", d10, d11, i32, 1);
    TESTINSN_un_q("vqshl.s32 d31, d30, #1", d31, d30, i32, -127);
    TESTINSN_un_q("vqshl.s32 d5, d4, #0", d5, d4, i32, -127);
    TESTINSN_un_q("vqshl.s32 d5, d4, #31", d5, d4, i32, 16);
    TESTINSN_un_q("vqshl.s32 d5, d4, #28", d5, d4, i32, 16);
    TESTINSN_un_q("vqshl.s32 d5, d4, #27", d5, d4, i32, 16);
    TESTINSN_un_q("vqshl.s32 d5, d4, #26", d5, d4, i32, 16);
    TESTINSN_un_q("vqshl.s32 d5, d4, #17", d5, d4, i32, 16);
    TESTINSN_un_q("vqshl.s32 d5, d4, #31", d5, d4, i32, -1);
    TESTINSN_un_q("vqshl.s32 d5, d4, #29", d5, d4, i32, -1);
    TESTINSN_un_q("vqshl.s32 d5, d4, #7", d5, d4, i32, (1 << 31) + 2);
    TESTINSN_un_q("vqshl.s16 d9, d8, #1", d9, d8, i32, 1);
    TESTINSN_un_q("vqshl.s16 d31, d30, #1", d31, d30, i32, -127);
    TESTINSN_un_q("vqshl.s16 d5, d4, #0", d5, d4, i32, -127);
    TESTINSN_un_q("vqshl.s16 d9, d8, #15", d9, d8, i32, 16);
    TESTINSN_un_q("vqshl.s16 d5, d4, #12", d5, d4, i32, 16);
    TESTINSN_un_q("vqshl.s16 d5, d4, #11", d5, d4, i32, 16);
    TESTINSN_un_q("vqshl.s16 d5, d4, #10", d5, d4, i32, 16);
    TESTINSN_un_q("vqshl.s16 d5, d4, #4", d5, d4, i32, 16);
    TESTINSN_un_q("vqshl.s16 d5, d4, #15", d5, d4, i32, -1);
    TESTINSN_un_q("vqshl.s16 d5, d4, #12", d5, d4, i32, -1);
    TESTINSN_un_q("vqshl.s16 d5, d4, #7", d5, d4, i32, (1 << 31) + 2);
    TESTINSN_un_q("vqshl.s8 d0, d1, #1", d0, d1, i32, 1);
    TESTINSN_un_q("vqshl.s8 d31, d30, #1", d31, d30, i32, -127);
    TESTINSN_un_q("vqshl.s8 d5, d4, #0", d5, d4, i32, -127);
    TESTINSN_un_q("vqshl.s8 d5, d4, #7", d5, d4, i32, 16);
    TESTINSN_un_q("vqshl.s8 d25, d4, #4", d25, d4, i32, 16);
    TESTINSN_un_q("vqshl.s8 d5, d4, #3", d5, d4, i32, 16);
    TESTINSN_un_q("vqshl.s8 d5, d4, #2", d5, d4, i32, 16);
    TESTINSN_un_q("vqshl.s8 d5, d4, #1", d5, d4, i32, 16);
    TESTINSN_un_q("vqshl.s8 d5, d4, #7", d5, d4, i32, -1);
    TESTINSN_un_q("vqshl.s8 d5, d4, #5", d5, d4, i32, -1);
    TESTINSN_un_q("vqshl.s8 d5, d4, #2", d5, d4, i32, (1 << 31) + 2);
    TESTINSN_un_q("vqshl.u64 d0, d1, #1", d0, d1, i32, 1);
    TESTINSN_un_q("vqshl.u64 d31, d30, #1", d31, d30, i32, -127);
    TESTINSN_un_q("vqshl.u64 d5, d4, #0", d5, d4, i32, -127);
    TESTINSN_un_q("vqshl.u64 d5, d4, #63", d5, d4, i32, 16);
    TESTINSN_un_q("vqshl.u64 d5, d4, #60", d5, d4, i32, 16);
    TESTINSN_un_q("vqshl.u64 d5, d4, #59", d5, d4, i32, 16);
    TESTINSN_un_q("vqshl.u64 d5, d4, #58", d5, d4, i32, 16);
    TESTINSN_un_q("vqshl.u64 d5, d4, #17", d5, d4, i32, 16);
    TESTINSN_un_q("vqshl.u64 d5, d4, #63", d5, d4, i32, -1);
    TESTINSN_un_q("vqshl.u64 d5, d4, #60", d5, d4, i32, -1);
    TESTINSN_un_q("vqshl.u64 d5, d4, #7", d5, d4, i32, (1 << 31) + 2);
    TESTINSN_un_q("vqshl.u32 d10, d11, #1", d10, d11, i32, 1);
    TESTINSN_un_q("vqshl.u32 d31, d30, #1", d31, d30, i32, -127);
    TESTINSN_un_q("vqshl.u32 d5, d4, #0", d5, d4, i32, -127);
    TESTINSN_un_q("vqshl.u32 d5, d4, #31", d5, d4, i32, 16);
    TESTINSN_un_q("vqshl.u32 d5, d4, #28", d5, d4, i32, 16);
    TESTINSN_un_q("vqshl.u32 d5, d4, #27", d5, d4, i32, 16);
    TESTINSN_un_q("vqshl.u32 d5, d4, #26", d5, d4, i32, 16);
    TESTINSN_un_q("vqshl.u32 d5, d4, #17", d5, d4, i32, 16);
    TESTINSN_un_q("vqshl.u32 d5, d4, #31", d5, d4, i32, -1);
    TESTINSN_un_q("vqshl.u32 d5, d4, #29", d5, d4, i32, -1);
    TESTINSN_un_q("vqshl.u32 d5, d4, #7", d5, d4, i32, (1 << 31) + 2);
    TESTINSN_un_q("vqshl.u16 d9, d8, #1", d9, d8, i32, 1);
    TESTINSN_un_q("vqshl.u16 d31, d30, #1", d31, d30, i32, -127);
    TESTINSN_un_q("vqshl.u16 d5, d4, #0", d5, d4, i32, -127);
    TESTINSN_un_q("vqshl.u16 d9, d8, #15", d9, d8, i32, 16);
    TESTINSN_un_q("vqshl.u16 d5, d4, #12", d5, d4, i32, 16);
    TESTINSN_un_q("vqshl.u16 d5, d4, #11", d5, d4, i32, 16);
    TESTINSN_un_q("vqshl.u16 d5, d4, #10", d5, d4, i32, 16);
    TESTINSN_un_q("vqshl.u16 d5, d4, #4", d5, d4, i32, 16);
    TESTINSN_un_q("vqshl.u16 d5, d4, #15", d5, d4, i32, -1);
    TESTINSN_un_q("vqshl.u16 d5, d4, #12", d5, d4, i32, -1);
    TESTINSN_un_q("vqshl.u16 d5, d4, #7", d5, d4, i32, (1 << 31) + 2);
    TESTINSN_un_q("vqshl.u8 d0, d1, #1", d0, d1, i32, 1);
    TESTINSN_un_q("vqshl.u8 d31, d30, #1", d31, d30, i32, -127);
    TESTINSN_un_q("vqshl.u8 d5, d4, #0", d5, d4, i32, -127);
    TESTINSN_un_q("vqshl.u8 d5, d4, #7", d5, d4, i32, 16);
    TESTINSN_un_q("vqshl.u8 d5, d4, #4", d5, d4, i32, 16);
    TESTINSN_un_q("vqshl.u8 d5, d4, #3", d5, d4, i32, 16);
    TESTINSN_un_q("vqshl.u8 d5, d4, #2", d5, d4, i32, 16);
    TESTINSN_un_q("vqshl.u8 d5, d4, #1", d5, d4, i32, 16);
    TESTINSN_un_q("vqshl.u8 d5, d4, #7", d5, d4, i32, -1);
    TESTINSN_un_q("vqshl.u8 d5, d4, #5", d5, d4, i32, -1);
    TESTINSN_un_q("vqshl.u8 d5, d4, #2", d5, d4, i32, (1 << 31) + 2);
    TESTINSN_un_q("vqshlu.s64 d0, d1, #1", d0, d1, i32, 1);
    TESTINSN_un_q("vqshlu.s64 d31, d30, #1", d31, d30, i32, -127);
    TESTINSN_un_q("vqshlu.s64 d5, d4, #0", d5, d4, i32, -127);
    TESTINSN_un_q("vqshlu.s64 d5, d4, #63", d5, d4, i32, 16);
    TESTINSN_un_q("vqshlu.s64 d5, d4, #60", d5, d4, i32, 16);
    TESTINSN_un_q("vqshlu.s64 d5, d4, #59", d5, d4, i32, 16);
    TESTINSN_un_q("vqshlu.s64 d5, d4, #58", d5, d4, i32, 16);
    TESTINSN_un_q("vqshlu.s64 d5, d4, #17", d5, d4, i32, 16);
    TESTINSN_un_q("vqshlu.s64 d5, d4, #63", d5, d4, i32, -1);
    TESTINSN_un_q("vqshlu.s64 d5, d4, #60", d5, d4, i32, -1);
    TESTINSN_un_q("vqshlu.s64 d5, d4, #7", d5, d4, i32, (1 << 31) + 2);
    TESTINSN_un_q("vqshlu.s32 d10, d11, #1", d10, d11, i32, 1);
    TESTINSN_un_q("vqshlu.s32 d31, d30, #1", d31, d30, i32, -127);
    TESTINSN_un_q("vqshlu.s32 d5, d4, #0", d5, d4, i32, -127);
    TESTINSN_un_q("vqshlu.s32 d5, d4, #31", d5, d4, i32, 16);
    TESTINSN_un_q("vqshlu.s32 d25, d24, #28", d25, d24, i32, 16);
    TESTINSN_un_q("vqshlu.s32 d5, d4, #27", d5, d4, i32, 16);
    TESTINSN_un_q("vqshlu.s32 d5, d4, #26", d5, d4, i32, 16);
    TESTINSN_un_q("vqshlu.s32 d5, d4, #17", d5, d4, i32, 16);
    TESTINSN_un_q("vqshlu.s32 d5, d24, #31", d5, d24, i32, -1);
    TESTINSN_un_q("vqshlu.s32 d5, d4, #29", d5, d4, i32, -1);
    TESTINSN_un_q("vqshlu.s32 d5, d4, #7", d5, d4, i32, (1 << 31) + 2);
    TESTINSN_un_q("vqshlu.s16 d9, d8, #1", d9, d8, i32, 1);
    TESTINSN_un_q("vqshlu.s16 d31, d30, #1", d31, d30, i32, -127);
    TESTINSN_un_q("vqshlu.s16 d5, d4, #0", d5, d4, i32, -127);
    TESTINSN_un_q("vqshlu.s16 d9, d8, #15", d9, d8, i32, 16);
    TESTINSN_un_q("vqshlu.s16 d5, d4, #12", d5, d4, i32, 16);
    TESTINSN_un_q("vqshlu.s16 d5, d4, #11", d5, d4, i32, 16);
    TESTINSN_un_q("vqshlu.s16 d5, d4, #10", d5, d4, i32, 16);
    TESTINSN_un_q("vqshlu.s16 d5, d4, #4", d5, d4, i32, 16);
    TESTINSN_un_q("vqshlu.s16 d15, d14, #15", d15, d14, i32, -1);
    TESTINSN_un_q("vqshlu.s16 d5, d4, #12", d5, d4, i32, -1);
    TESTINSN_un_q("vqshlu.s16 d5, d4, #7", d5, d4, i32, (1 << 31) + 2);
    TESTINSN_un_q("vqshlu.s8 d0, d1, #1", d0, d1, i32, 1);
    TESTINSN_un_q("vqshlu.s8 d31, d30, #1", d31, d30, i32, -127);
    TESTINSN_un_q("vqshlu.s8 d5, d4, #0", d5, d4, i32, -127);
    TESTINSN_un_q("vqshlu.s8 d5, d4, #7", d5, d4, i32, 16);
    TESTINSN_un_q("vqshlu.s8 d5, d4, #4", d5, d4, i32, 16);
    TESTINSN_un_q("vqshlu.s8 d5, d4, #3", d5, d4, i32, 16);
    TESTINSN_un_q("vqshlu.s8 d5, d4, #2", d5, d4, i32, 16);
    TESTINSN_un_q("vqshlu.s8 d5, d4, #1", d5, d4, i32, 16);
    TESTINSN_un_q("vqshlu.s8 d5, d4, #7", d5, d4, i32, -1);
    TESTINSN_un_q("vqshlu.s8 d5, d4, #5", d5, d4, i32, -1);
    TESTINSN_un_q("vqshlu.s8 d5, d4, #2", d5, d4, i32, (1 << 31) + 2);

    printf("---- VQRSHL (register) ----\n");
    TESTINSN_bin_q("vqrshl.s64 d0, d1, d2", d0, d1, i32, 1, d2, i32, 1);
    TESTINSN_bin_q("vqrshl.s64 d3, d4, d5", d3, d4, i32, -127, d5, i32, 1);
    TESTINSN_bin_q("vqrshl.s64 d3, d4, d5", d3, d4, i32, -127, d5, i32, -3);
    TESTINSN_bin_q("vqrshl.s64 d0, d1, d2", d0, d1, i32, 16, d2, i32, 14);
    TESTINSN_bin_q("vqrshl.s64 d13, d14, d15", d13, d14, i32, -17, d15, i32, -26);
    TESTINSN_bin_q("vqrshl.s64 d7, d8, d2", d7, d8, i32, 24, d2, i32, -60);
    TESTINSN_bin_q("vqrshl.s32 d3, d4, d15", d3, d4, i32, 127, d15, i32, -30);
    TESTINSN_bin_q("vqrshl.s32 d2, d8, d4", d2, d8, i32, -11, d4, i32, -4);
    TESTINSN_bin_q("vqrshl.s32 d12, d11, d13", d12, d11, i32, -120, d13, i32, -9);
    TESTINSN_bin_q("vqrshl.s32 d0, d1, d2", d0, d1, i32, 34, d2, i32, -7);
    TESTINSN_bin_q("vqrshl.s32 d9, d10, d11", d9, d10, i32, (1 << 31) + 8, d11, i32, -1);
    TESTINSN_bin_q("vqrshl.s32 d13, d3, d5", d13, d3, i32, (1 << 27), d5, i32, 3);
    TESTINSN_bin_q("vqrshl.s16 d11, d10, d2", d11, d10, i32, (1 << 31), d2, i32, -31);
    TESTINSN_bin_q("vqrshl.s16 d3, d14, d7", d3, d14, i32, (1 << 31), d7, i32, -3);
    TESTINSN_bin_q("vqrshl.s16 d0, d31, d2", d0, d31, i32, (1 << 31) + 256, d2, i32, -1);
    TESTINSN_bin_q("vqrshl.s16 d1, d2, d3", d1, d2, i32, (1 << 31) + 256, d3, i32, -31);
    TESTINSN_bin_q("vqrshl.s16 d3, d4, d5", d3, d4, i32, (1 << 31) + (1 << 29), d5, i32, -13);
    TESTINSN_bin_q("vqrshl.s16 d0, d15, d2", d0, d15, i32, 1, d2, i32, 30);
    TESTINSN_bin_q("vqrshl.s8 d2, d7, d11", d2, d7, i32, 0xf, d11, i32, -1);
    TESTINSN_bin_q("vqrshl.s16 d2, d7, d11", d2, d7, i32, 0xf, d11, i32, -1);
    TESTINSN_bin_q("vqrshl.s32 d2, d7, d11", d2, d7, i32, 0xf, d11, i32, -1);
    TESTINSN_bin_q("vqrshl.s8 d2, d7, d11", d2, d7, i32, -1, d11, i32, -1);
    TESTINSN_bin_q("vqrshl.s16 d2, d7, d11", d2, d7, i32, -1, d11, i32, -1);
    TESTINSN_bin_q("vqrshl.s32 d2, d7, d11", d2, d7, i32, -1, d11, i32, -1);
    TESTINSN_bin_q("vqrshl.s8 d2, d7, d11", d2, d7, i32, -2, d11, i32, -1);
    TESTINSN_bin_q("vqrshl.s16 d2, d7, d11", d2, d7, i32, -2, d11, i32, -1);
    TESTINSN_bin_q("vqrshl.s32 d2, d7, d11", d2, d7, i32, -2, d11, i32, -1);
    TESTINSN_bin_q("vqrshl.s8 d2, d7, d11", d2, d7, i32, -1, d11, i32, 0);
    TESTINSN_bin_q("vqrshl.s16 d2, d7, d11", d2, d7, i32, -1, d11, i32, 0);
    TESTINSN_bin_q("vqrshl.s32 d2, d7, d31", d2, d7, i32, -1, d31, i32, 0);
    TESTINSN_bin_q("vqrshl.s8 d2, d7, d11", d2, d7, i32, -1, d11, i32, 40);
    TESTINSN_bin_q("vqrshl.s8 d13, d1, d2", d13, d1, i32, -4, d2, i32, 30);
    TESTINSN_bin_q("vqrshl.s8 d3, d7, d5", d3, d7, i32, (1 << 31) + 11, d5, i32, 3);
    TESTINSN_bin_q("vqrshl.s8 d10, d11, d12", d10, d11, i32, (1 << 16), d12, i32, 16);
    TESTINSN_bin_q("vqrshl.s8 d6, d7, d8", d6, d7, i32, (1 << 30), d8, i32, 2);
    TESTINSN_bin_q("vqrshl.s8 d10, d11, d12", d10, d11, i32, 24, d12, i32, 120);
    TESTINSN_bin_q("vqrshl.u64 d0, d1, d2", d0, d1, i32, 1, d2, i32, 1);
    TESTINSN_bin_q("vqrshl.u64 d3, d4, d5", d3, d4, i32, -127, d5, i32, 1);
    TESTINSN_bin_q("vqrshl.u64 d3, d4, d5", d3, d4, i32, -127, d5, i32, -3);
    TESTINSN_bin_q("vqrshl.u64 d0, d1, d2", d0, d1, i32, 16, d2, i32, 14);
    TESTINSN_bin_q("vqrshl.u64 d13, d14, d15", d13, d14, i32, -17, d15, i32, -26);
    TESTINSN_bin_q("vqrshl.u64 d7, d8, d2", d7, d8, i32, 24, d2, i32, -60);
    TESTINSN_bin_q("vqrshl.u32 d3, d4, d15", d3, d4, i32, 127, d15, i32, -30);
    TESTINSN_bin_q("vqrshl.u32 d2, d8, d4", d2, d8, i32, -11, d4, i32, -4);
    TESTINSN_bin_q("vqrshl.u32 d12, d11, d13", d12, d11, i32, -120, d13, i32, -9);
    TESTINSN_bin_q("vqrshl.u32 d0, d1, d2", d0, d1, i32, 34, d2, i32, -7);
    TESTINSN_bin_q("vqrshl.u32 d9, d10, d11", d9, d10, i32, (1 << 31) + 8, d11, i32, -1);
    TESTINSN_bin_q("vqrshl.u32 d13, d3, d5", d13, d3, i32, (1 << 27), d5, i32, 3);
    TESTINSN_bin_q("vqrshl.u16 d11, d10, d2", d11, d10, i32, (1 << 31), d2, i32, -31);
    TESTINSN_bin_q("vqrshl.u16 d3, d14, d7", d3, d14, i32, (1 << 31), d7, i32, -3);
    TESTINSN_bin_q("vqrshl.u16 d0, d31, d2", d0, d31, i32, (1 << 31) + 256, d2, i32, -1);
    TESTINSN_bin_q("vqrshl.u16 d1, d2, d3", d1, d2, i32, (1 << 31) + 256, d3, i32, -31);
    TESTINSN_bin_q("vqrshl.u16 d3, d4, d5", d3, d4, i32, (1 << 31) + (1 << 29), d5, i32, -13);
    TESTINSN_bin_q("vqrshl.u16 d0, d15, d2", d0, d15, i32, 1, d2, i32, 30);
    TESTINSN_bin_q("vqrshl.u8 d2, d7, d11", d2, d7, i32, -1, d11, i32, 40);
    TESTINSN_bin_q("vqrshl.u8 d2, d7, d11", d2, d7, i32, -1, d11, i32, -1);
    TESTINSN_bin_q("vqrshl.u8 d2, d7, d11", d2, d7, i32, 0xf, d11, i32, -1);
    TESTINSN_bin_q("vqrshl.u16 d2, d7, d11", d2, d7, i32, 0xf, d11, i32, -1);
    TESTINSN_bin_q("vqrshl.u32 d2, d7, d11", d2, d7, i32, 0xf, d11, i32, -1);
    TESTINSN_bin_q("vqrshl.u8 d2, d7, d11", d2, d7, i32, -2, d11, i32, -1);
    TESTINSN_bin_q("vqrshl.u16 d2, d7, d11", d2, d7, i32, -2, d11, i32, -1);
    TESTINSN_bin_q("vqrshl.u32 d2, d7, d11", d2, d7, i32, -2, d11, i32, -1);
    TESTINSN_bin_q("vqrshl.u8 d2, d7, d11", d2, d7, i32, -1, d11, i32, 0);
    TESTINSN_bin_q("vqrshl.u16 d2, d7, d11", d2, d7, i32, -1, d11, i32, 0);
    TESTINSN_bin_q("vqrshl.u32 d2, d7, d11", d2, d7, i32, -1, d11, i32, 0);
    TESTINSN_bin_q("vqrshl.u8 d13, d1, d2", d13, d1, i32, -4, d2, i32, 30);
    TESTINSN_bin_q("vqrshl.u8 d3, d7, d5", d3, d7, i32, (1 << 31) + 11, d5, i32, 3);
    TESTINSN_bin_q("vqrshl.u8 d10, d11, d12", d10, d11, i32, (1 << 16), d12, i32, 16);
    TESTINSN_bin_q("vqrshl.u8 d6, d7, d8", d6, d7, i32, (1 << 30), d8, i32, 2);
    TESTINSN_bin_q("vqrshl.u8 d10, d11, d12", d10, d11, i32, 24, d12, i32, 120);

    printf("---- VRSHL (register) ----\n");
    TESTINSN_bin("vrshl.s64 d0, d1, d2", d0, d1, i32, 1, d2, i32, 1);
    TESTINSN_bin("vrshl.s64 d3, d4, d5", d3, d4, i32, -127, d5, i32, 1);
    TESTINSN_bin("vrshl.s64 d3, d4, d5", d3, d4, i32, -127, d5, i32, -3);
    TESTINSN_bin("vrshl.s64 d0, d1, d2", d0, d1, i32, 16, d2, i32, 14);
    TESTINSN_bin("vrshl.s64 d13, d14, d15", d13, d14, i32, -17, d15, i32, -26);
    TESTINSN_bin("vrshl.s64 d7, d8, d2", d7, d8, i32, 24, d2, i32, -60);
    TESTINSN_bin("vrshl.s32 d3, d4, d15", d3, d4, i32, 127, d15, i32, -30);
    TESTINSN_bin("vrshl.s32 d2, d8, d4", d2, d8, i32, -11, d4, i32, -4);
    TESTINSN_bin("vrshl.s32 d12, d11, d13", d12, d11, i32, -120, d13, i32, -9);
    TESTINSN_bin("vrshl.s32 d0, d1, d2", d0, d1, i32, 34, d2, i32, -7);
    TESTINSN_bin("vrshl.s32 d9, d10, d11", d9, d10, i32, (1 << 31) + 8, d11, i32, -1);
    TESTINSN_bin("vrshl.s32 d13, d3, d5", d13, d3, i32, (1 << 27), d5, i32, 3);
    TESTINSN_bin("vrshl.s16 d11, d10, d2", d11, d10, i32, (1 << 31), d2, i32, -31);
    TESTINSN_bin("vrshl.s16 d3, d14, d7", d3, d14, i32, (1 << 31), d7, i32, -3);
    TESTINSN_bin("vrshl.s16 d0, d11, d2", d0, d11, i32, (1 << 31) + 256, d2, i32, -1);
    TESTINSN_bin("vrshl.s16 d1, d2, d3", d1, d2, i32, (1 << 31) + 256, d3, i32, -31);
    TESTINSN_bin("vrshl.s16 d3, d4, d5", d3, d4, i32, (1 << 31) + (1 << 29), d5, i32, -13);
    TESTINSN_bin("vrshl.s16 d0, d15, d2", d0, d15, i32, 1, d2, i32, 30);
    TESTINSN_bin("vrshl.s8 d2, d7, d11", d2, d7, i32, 0xf, d11, i32, -1);
    TESTINSN_bin("vrshl.s16 d2, d7, d11", d2, d7, i32, 0xf, d11, i32, -1);
    TESTINSN_bin("vrshl.s32 d2, d7, d11", d2, d7, i32, 0xf, d11, i32, -1);
    TESTINSN_bin("vrshl.s8 d2, d7, d31", d2, d7, i32, -1, d31, i32, -1);
    TESTINSN_bin("vrshl.s16 d2, d7, d31", d2, d7, i32, -1, d31, i32, -1);
    TESTINSN_bin("vrshl.s32 d2, d7, d31", d2, d7, i32, -1, d31, i32, -1);
    TESTINSN_bin("vrshl.s8 d2, d7, d11", d2, d7, i32, -2, d11, i32, -1);
    TESTINSN_bin("vrshl.s16 d2, d7, d11", d2, d7, i32, -2, d11, i32, -1);
    TESTINSN_bin("vrshl.s32 d2, d7, d11", d2, d7, i32, -2, d11, i32, -1);
    TESTINSN_bin("vrshl.s8 d2, d7, d11", d2, d7, i32, -1, d11, i32, 0);
    TESTINSN_bin("vrshl.s16 d2, d7, d11", d2, d7, i32, -1, d11, i32, 0);
    TESTINSN_bin("vrshl.s32 d2, d7, d11", d2, d7, i32, -1, d11, i32, 0);
    TESTINSN_bin("vrshl.s8 d2, d7, d11", d2, d7, i32, -1, d11, i32, 40);
    TESTINSN_bin("vrshl.s8 d13, d1, d2", d13, d1, i32, -4, d2, i32, 30);
    TESTINSN_bin("vrshl.s8 d3, d7, d5", d3, d7, i32, (1 << 31) + 11, d5, i32, 3);
    TESTINSN_bin("vrshl.s8 d10, d11, d12", d10, d11, i32, (1 << 16), d12, i32, 16);
    TESTINSN_bin("vrshl.s8 d6, d7, d8", d6, d7, i32, (1 << 30), d8, i32, 2);
    TESTINSN_bin("vrshl.s8 d10, d11, d12", d10, d11, i32, 24, d12, i32, 120);
    TESTINSN_bin("vrshl.u64 d0, d1, d2", d0, d1, i32, 1, d2, i32, 1);
    TESTINSN_bin("vrshl.u64 d3, d4, d5", d3, d4, i32, -127, d5, i32, 1);
    TESTINSN_bin("vrshl.u64 d3, d4, d5", d3, d4, i32, -127, d5, i32, -3);
    TESTINSN_bin("vrshl.u64 d0, d1, d2", d0, d1, i32, 16, d2, i32, 14);
    TESTINSN_bin("vrshl.u64 d13, d14, d15", d13, d14, i32, -17, d15, i32, -26);
    TESTINSN_bin("vrshl.u64 d7, d8, d2", d7, d8, i32, 24, d2, i32, -60);
    TESTINSN_bin("vrshl.u32 d3, d4, d15", d3, d4, i32, 127, d15, i32, -30);
    TESTINSN_bin("vrshl.u32 d2, d8, d4", d2, d8, i32, -11, d4, i32, -4);
    TESTINSN_bin("vrshl.u32 d12, d11, d13", d12, d11, i32, -120, d13, i32, -9);
    TESTINSN_bin("vrshl.u32 d0, d1, d2", d0, d1, i32, 34, d2, i32, -7);
    TESTINSN_bin("vrshl.u32 d9, d10, d11", d9, d10, i32, (1 << 31) + 8, d11, i32, -1);
    TESTINSN_bin("vrshl.u32 d13, d3, d5", d13, d3, i32, (1 << 27), d5, i32, 3);
    TESTINSN_bin("vrshl.u16 d11, d10, d2", d11, d10, i32, (1 << 31), d2, i32, -31);
    TESTINSN_bin("vrshl.u16 d3, d14, d7", d3, d14, i32, (1 << 31), d7, i32, -3);
    TESTINSN_bin("vrshl.u16 d0, d31, d2", d0, d31, i32, (1 << 31) + 256, d2, i32, -1);
    TESTINSN_bin("vrshl.u16 d1, d2, d3", d1, d2, i32, (1 << 31) + 256, d3, i32, -31);
    TESTINSN_bin("vrshl.u16 d3, d4, d5", d3, d4, i32, (1 << 31) + (1 << 29), d5, i32, -13);
    TESTINSN_bin("vrshl.u16 d0, d15, d2", d0, d15, i32, 1, d2, i32, 30);
    TESTINSN_bin("vrshl.u8 d2, d7, d11", d2, d7, i32, -1, d11, i32, 40);
    TESTINSN_bin("vrshl.u8 d2, d7, d11", d2, d7, i32, -1, d11, i32, -1);
    TESTINSN_bin("vrshl.u8 d2, d7, d11", d2, d7, i32, 0xf, d11, i32, -1);
    TESTINSN_bin("vrshl.u16 d2, d7, d11", d2, d7, i32, 0xf, d11, i32, -1);
    TESTINSN_bin("vrshl.u32 d2, d7, d11", d2, d7, i32, 0xf, d11, i32, -1);
    TESTINSN_bin("vrshl.u8 d2, d7, d11", d2, d7, i32, -1, d11, i32, -1);
    TESTINSN_bin("vrshl.u16 d2, d7, d11", d2, d7, i32, -1, d11, i32, -1);
    TESTINSN_bin("vrshl.u32 d2, d7, d11", d2, d7, i32, -1, d11, i32, -1);
    TESTINSN_bin("vrshl.u8 d2, d7, d31", d2, d7, i32, -2, d31, i32, -1);
    TESTINSN_bin("vrshl.u16 d2, d7, d31", d2, d7, i32, -2, d31, i32, -1);
    TESTINSN_bin("vrshl.u32 d2, d7, d31", d2, d7, i32, -2, d31, i32, -1);
    TESTINSN_bin("vrshl.u8 d13, d1, d2", d13, d1, i32, -4, d2, i32, 30);
    TESTINSN_bin("vrshl.u8 d3, d7, d5", d3, d7, i32, (1 << 31) + 11, d5, i32, 3);
    TESTINSN_bin("vrshl.u8 d10, d11, d12", d10, d11, i32, (1 << 16), d12, i32, 16);
    TESTINSN_bin("vrshl.u8 d6, d7, d8", d6, d7, i32, (1 << 30), d8, i32, 2);
    TESTINSN_bin("vrshl.u8 d10, d11, d12", d10, d11, i32, 24, d12, i32, 120);

    printf("---- VMAX (integer) ----\n");
    TESTINSN_bin("vmax.s32 d0, d1, d2", d0, d1, i32, 25, d2, i32, 121);
    TESTINSN_bin("vmax.s32 d0, d1, d2", d0, d1, i32, 250, d2, i32, 121);
    TESTINSN_bin("vmax.s32 d0, d1, d2", d0, d1, i32, 140, d2, i32, 140);
    TESTINSN_bin("vmax.s16 d0, d1, d2", d0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin("vmax.s8 d0, d1, d2", d0, d1, i32, 120, d2, i32, 120);
    TESTINSN_bin("vmax.s8 d5, d7, d5", d5, d7, i32, (1 << 31) + 1, d5, i32, (1 << 31) + 2); 
    TESTINSN_bin("vmax.s16 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2); 
    TESTINSN_bin("vmax.s32 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2); 
    TESTINSN_bin("vmax.s8 d5, d7, d5", d5, d7, i32, (1 << 31) + 1, d5, i32, (1 << 31) + 3); 
    TESTINSN_bin("vmax.s16 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 3); 
    TESTINSN_bin("vmax.s32 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 3); 
    TESTINSN_bin("vmax.s8 d5, d7, d5", d5, d7, i32, (1 << 31) + 4, d5, i32, (1 << 31) + 2); 
    TESTINSN_bin("vmax.s16 d0, d1, d2", d0, d1, i32, (1 << 31) + 4, d2, i32, (1 << 31) + 2); 
    TESTINSN_bin("vmax.s32 d0, d1, d2", d0, d1, i32, (1 << 31) + 4, d2, i32, (1 << 31) + 2); 
    TESTINSN_bin("vmax.s32 d10, d11, d12", d10, d11, i32, 24, d12, i32, 120);
    TESTINSN_bin("vmax.u32 d0, d1, d2", d0, d1, i32, 25, d2, i32, 120);
    TESTINSN_bin("vmax.u32 d0, d1, d2", d0, d1, i32, 250, d2, i32, 120);
    TESTINSN_bin("vmax.u32 d0, d1, d2", d0, d1, i32, 140, d2, i32, 140);
    TESTINSN_bin("vmax.u16 d0, d1, d2", d0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin("vmax.u8 d0, d1, d2", d0, d1, i32, 120, d2, i32, 120);
    TESTINSN_bin("vmax.u8 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2); 
    TESTINSN_bin("vmax.u16 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2); 
    TESTINSN_bin("vmax.u32 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2); 
    TESTINSN_bin("vmax.u8 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 3); 
    TESTINSN_bin("vmax.u16 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 3); 
    TESTINSN_bin("vmax.u32 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 3); 
    TESTINSN_bin("vmax.u8 d0, d1, d2", d0, d1, i32, (1 << 31) + 4, d2, i32, (1 << 31) + 2); 
    TESTINSN_bin("vmax.u16 d0, d1, d2", d0, d1, i32, (1 << 31) + 4, d2, i32, (1 << 31) + 2); 
    TESTINSN_bin("vmax.u32 d0, d1, d2", d0, d1, i32, (1 << 31) + 4, d2, i32, (1 << 31) + 2); 
    TESTINSN_bin("vmax.u32 d10, d11, d12", d10, d11, i32, 24, d12, i32, 120);  

    printf("---- VMIN (integer) ----\n");
    TESTINSN_bin("vmin.s32 d0, d1, d2", d0, d1, i32, 25, d2, i32, 121);
    TESTINSN_bin("vmin.s32 d0, d1, d2", d0, d1, i32, 250, d2, i32, 121);
    TESTINSN_bin("vmin.s32 d0, d1, d2", d0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin("vmin.s16 d0, d1, d2", d0, d1, i32, 120, d2, i32, 120);
    TESTINSN_bin("vmin.s8 d0, d1, d2", d0, d1, i32, 140, d2, i32, 140);
    TESTINSN_bin("vmin.s8 d5, d7, d5", d5, d7, i32, (1 << 31) + 1, d5, i32, (1 << 31) + 2); 
    TESTINSN_bin("vmin.s16 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2); 
    TESTINSN_bin("vmin.s32 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2); 
    TESTINSN_bin("vmin.s8 d5, d7, d5", d5, d7, i32, (1 << 31) + 1, d5, i32, (1 << 31) + 3); 
    TESTINSN_bin("vmin.s16 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 3); 
    TESTINSN_bin("vmin.s32 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 3); 
    TESTINSN_bin("vmin.s8 d5, d7, d5", d5, d7, i32, (1 << 31) + 4, d5, i32, (1 << 31) + 2); 
    TESTINSN_bin("vmin.s16 d0, d1, d2", d0, d1, i32, (1 << 31) + 4, d2, i32, (1 << 31) + 2); 
    TESTINSN_bin("vmin.s32 d0, d1, d2", d0, d1, i32, (1 << 31) + 4, d2, i32, (1 << 31) + 2); 
    TESTINSN_bin("vmin.s32 d10, d11, d12", d10, d11, i32, 24, d12, i32, 120);
    TESTINSN_bin("vmin.u32 d0, d1, d2", d0, d1, i32, 25, d2, i32, 120);
    TESTINSN_bin("vmin.u32 d0, d1, d2", d0, d1, i32, 250, d2, i32, 120);
    TESTINSN_bin("vmin.u32 d0, d1, d2", d0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin("vmin.u16 d0, d1, d2", d0, d1, i32, 120, d2, i32, 120);
    TESTINSN_bin("vmin.u8 d0, d1, d2", d0, d1, i32, 140, d2, i32, 140);
    TESTINSN_bin("vmin.u8 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2); 
    TESTINSN_bin("vmin.u16 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2); 
    TESTINSN_bin("vmin.u32 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2); 
    TESTINSN_bin("vmin.u8 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 3); 
    TESTINSN_bin("vmin.u16 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 3); 
    TESTINSN_bin("vmin.u32 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 3); 
    TESTINSN_bin("vmin.u8 d0, d1, d2", d0, d1, i32, (1 << 31) + 4, d2, i32, (1 << 31) + 2); 
    TESTINSN_bin("vmin.u16 d0, d1, d2", d0, d1, i32, (1 << 31) + 4, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vmin.u32 d0, d1, d2", d0, d1, i32, (1 << 31) + 4, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vmin.u32 d10, d11, d12", d10, d11, i32, 24, d12, i32, 120);

    printf("---- VABD ----\n");
    TESTINSN_bin("vabd.s32 d0, d1, d2", d0, d1, i32, 25, d2, i32, 120);
    TESTINSN_bin("vabd.s32 d0, d1, d2", d0, d1, i32, 25, d2, i32, 121);
    TESTINSN_bin("vabd.s32 d0, d1, d2", d0, d1, i32, 140, d2, i32, -120);
    TESTINSN_bin("vabd.s16 d0, d1, d2", d0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin("vabd.s8 d0, d1, d2", d0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin("vabd.s8 d5, d7, d5", d5, d7, i32, (1 << 31) + 1, d5, i32, (1 << 31) + 2);
    TESTINSN_bin("vabd.s8 d5, d7, d5", d5, d7, i32, -255, d5, i32, (1 << 31) + 2);
    TESTINSN_bin("vabd.s8 d5, d7, d5", d5, d7, i32, (1 << 31) + 1, d5, i32, -200);
    TESTINSN_bin("vabd.s16 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vabd.s32 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vabd.s8 d5, d7, d5", d5, d7, i32, (1 << 31) + 1, d5, i32, (1 << 31) + 3);
    TESTINSN_bin("vabd.s16 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 3);
    TESTINSN_bin("vabd.s32 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 3);
    TESTINSN_bin("vabd.s8 d5, d7, d5", d5, d7, i32, (1 << 31) + 4, d5, i32, (1 << 31) + 2);
    TESTINSN_bin("vabd.s16 d0, d1, d2", d0, d1, i32, (1 << 31) + 4, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vabd.s32 d0, d1, d2", d0, d1, i32, (1 << 31) + 4, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vabd.s32 d10, d11, d12", d10, d11, i32, 24, d12, i32, 120);
    TESTINSN_bin("vabd.u32 d0, d1, d2", d0, d1, i32, 25, d2, i32, 120);
    TESTINSN_bin("vabd.u32 d0, d1, d2", d0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin("vabd.u16 d0, d1, d2", d0, d1, i32, -140, d2, i32, 120);
    TESTINSN_bin("vabd.u8 d0, d1, d2", d0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin("vabd.u8 d5, d7, d5", d5, d7, i32, -255, d5, i32, (1 << 31) + 2);
    TESTINSN_bin("vabd.u8 d5, d7, d5", d5, d7, i32, (1 << 31) + 1, d5, i32, -200);
    TESTINSN_bin("vabd.u8 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vabd.u16 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vabd.u32 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vabd.u8 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 3);
    TESTINSN_bin("vabd.u16 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 3);
    TESTINSN_bin("vabd.u32 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 3);
    TESTINSN_bin("vabd.u8 d0, d1, d2", d0, d1, i32, (1 << 31) + 4, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vabd.u16 d0, d1, d2", d0, d1, i32, (1 << 31) + 4, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vabd.u32 d0, d1, d2", d0, d1, i32, (1 << 31) + 4, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vabd.u32 d10, d11, d12", d10, d11, i32, 24, d12, i32, 120);

    printf("---- VABA ----\n");
    TESTINSN_bin("vaba.s32 d0, d1, d2", d0, d1, i32, 25, d2, i32, 120);
    TESTINSN_bin("vaba.s32 d0, d1, d2", d0, d1, i32, 25, d2, i32, 121);
    TESTINSN_bin("vaba.s32 d0, d1, d2", d0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin("vaba.s16 d0, d1, d2", d0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin("vaba.s8 d0, d1, d2", d0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin("vaba.s8 d5, d7, d5", d5, d7, i32, (1 << 31) + 1, d5, i32, (1 << 31) + 2);
    TESTINSN_bin("vaba.s8 d5, d7, d5", d5, d7, i32, -255, d5, i32, (1 << 31) + 2);
    TESTINSN_bin("vaba.s8 d5, d7, d5", d5, d7, i32, (1 << 31) + 1, d5, i32, -200);
    TESTINSN_bin("vaba.s16 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vaba.s32 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vaba.s8 d5, d7, d5", d5, d7, i32, (1 << 31) + 1, d5, i32, (1 << 31) + 3);
    TESTINSN_bin("vaba.s16 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 3);
    TESTINSN_bin("vaba.s32 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 3);
    TESTINSN_bin("vaba.s8 d5, d7, d5", d5, d7, i32, (1 << 31) + 4, d5, i32, (1 << 31) + 2);
    TESTINSN_bin("vaba.s16 d0, d1, d2", d0, d1, i32, (1 << 31) + 4, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vaba.s32 d0, d1, d2", d0, d1, i32, (1 << 31) + 4, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vaba.s32 d10, d11, d12", d10, d11, i32, 24, d12, i32, 120);
    TESTINSN_bin("vaba.u32 d0, d1, d2", d0, d1, i32, 25, d2, i32, 120);
    TESTINSN_bin("vaba.u32 d0, d1, d2", d0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin("vaba.u16 d0, d1, d2", d0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin("vaba.u8 d0, d1, d2", d0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin("vaba.u8 d5, d7, d5", d5, d7, i32, -255, d5, i32, (1 << 31) + 2);
    TESTINSN_bin("vaba.u8 d5, d7, d5", d5, d7, i32, (1 << 31) + 1, d5, i32, -200);
    TESTINSN_bin("vaba.u8 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vaba.u16 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vaba.u32 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vaba.u8 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 3);
    TESTINSN_bin("vaba.u16 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 3);
    TESTINSN_bin("vaba.u32 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 3);
    TESTINSN_bin("vaba.u8 d0, d1, d2", d0, d1, i32, (1 << 31) + 4, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vaba.u16 d0, d1, d2", d0, d1, i32, (1 << 31) + 4, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vaba.u32 d0, d1, d2", d0, d1, i32, (1 << 31) + 4, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vaba.u32 d10, d11, d12", d10, d11, i32, 24, d12, i32, 120);

    printf("---- VTST ----\n");
    TESTINSN_bin("vtst.32 d0, d1, d2", d0, d1, i32, 24, d2, i32, 120);
    TESTINSN_bin("vtst.32 d3, d4, d5", d3, d4, i32, 140, d5, i32, 120);
    TESTINSN_bin("vtst.16 d6, d7, d8", d6, d7, i32, 120, d8, i32, 120);
    TESTINSN_bin("vtst.8 d9, d10, d12", d9, d10, i32, 140, d12, i32, 120);
    TESTINSN_bin("vtst.8 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2); 
    TESTINSN_bin("vtst.16 d0, d1, d2", d0, d1, i32, (1 << 14) + 1, d2, i32, (1 << 14) + 1); 
    TESTINSN_bin("vtst.32 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2); 
    TESTINSN_bin("vtst.8 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, 2); 
    TESTINSN_bin("vtst.16 d0, d1, d2", d0, d1, i32, (1 << 14) + 1, d2, i32, (1 << 14) + 1); 
    TESTINSN_bin("vtst.32 d0, d1, d2", d0, d1, i32, 1, d2, i32, (1 << 31) + 2); 
    TESTINSN_bin("vtst.32 d10, d11, d12", d10, d11, i32, 24, d12, i32, 120);

    printf("---- VCEQ ----\n");
    TESTINSN_bin("vceq.i32 d0, d1, d2", d0, d1, i32, 24, d2, i32, 120);
    TESTINSN_bin("vceq.i32 d3, d4, d5", d3, d4, i32, 140, d5, i32, 120);
    TESTINSN_bin("vceq.i16 d6, d7, d8", d6, d7, i32, 120, d8, i32, 120);
    TESTINSN_bin("vceq.i8 d9, d10, d12", d9, d10, i32, 140, d12, i32, 120);
    TESTINSN_bin("vceq.i8 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2); 
    TESTINSN_bin("vceq.i16 d0, d1, d2", d0, d1, i32, (1 << 14) + 1, d2, i32, (1 << 14) + 1); 
    TESTINSN_bin("vceq.i32 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2); 
    TESTINSN_bin("vceq.i8 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, 2); 
    TESTINSN_bin("vceq.i16 d0, d1, d2", d0, d1, i32, 1, d2, i32, (1 << 14) + 1); 
    TESTINSN_bin("vceq.i32 d0, d1, d2", d0, d1, i32, 1, d2, i32, (1 << 31) + 2); 
    TESTINSN_bin("vceq.i32 d10, d11, d12", d10, d11, i32, 24, d12, i32, 120);

    printf("---- VMLA ----\n");
    TESTINSN_bin("vmla.i32 d0, d1, d2", d0, d1, i32, -24, d2, i32, 120);
    TESTINSN_bin("vmla.i32 d6, d7, d8", d6, d7, i32, 140, d8, i32, 120);
    TESTINSN_bin("vmla.i16 d9, d11, d12", d9, d11, i32, 0x140, d12, i32, 0x120);
    TESTINSN_bin("vmla.i8 d0, d1, d2", d0, d1, i32, 140, d2, i32, -120);
    TESTINSN_bin("vmla.i8 d10, d11, d12", d10, d11, i32, (1 << 5) + 1, d12, i32, (1 << 3) + 2); 
    TESTINSN_bin("vmla.i16 d4, d5, d6", d4, d5, i32, (1 << 14) + 1, d6, i32, (1 << 13) + 2); 
    TESTINSN_bin("vmla.i32 d7, d8, d9", d7, d8, i32, (1 << 31) + 1, d9, i32, (1 << 31) + 2); 
    TESTINSN_bin("vmla.i8 d10, d13, d12", d10, d13, i32, (1 << 5) + 1, d12, i32, (1 << 3) + 2); 
    TESTINSN_bin("vmla.i16 d4, d5, d6", d4, d5, i32, (1 << 28) + 0xfe, d6, i32, (1 << 13) + 2); 
    TESTINSN_bin("vmla.i32 d7, d8, d9", d7, d8, i32, (1 << 31) + 1, d9, i32, (1 << 31) + 2); 
    TESTINSN_bin("vmla.i32 d10, d11, d15", d10, d11, i32, 24, d15, i32, -120);

    printf("---- VMLS ----\n");
    TESTINSN_bin("vmls.i32 d0, d1, d2", d0, d1, i32, -24, d2, i32, 120);
    TESTINSN_bin("vmls.i32 d6, d7, d8", d6, d7, i32, 140, d8, i32, -120);
    TESTINSN_bin("vmls.i16 d9, d11, d12", d9, d11, i32, 0x140, d12, i32, 0x120);
    TESTINSN_bin("vmls.i8 d0, d1, d2", d0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin("vmls.i8 d10, d11, d12", d10, d11, i32, (1 << 5) + 1, d12, i32, (1 << 3) + 2); 
    TESTINSN_bin("vmls.i16 d4, d5, d6", d4, d5, i32, (1 << 14) + 1, d6, i32, (1 << 13) + 2); 
    TESTINSN_bin("vmls.i32 d7, d8, d9", d7, d8, i32, (1 << 31) + 1, d9, i32, (1 << 31) + 2); 
    TESTINSN_bin("vmls.i8 d10, d13, d12", d10, d13, i32, (1 << 5) + 1, d12, i32, (1 << 3) + 2); 
    TESTINSN_bin("vmls.i16 d4, d5, d6", d4, d5, i32, (1 << 28) + 0xfe, d6, i32, (1 << 13) + 2); 
    TESTINSN_bin("vmls.i32 d7, d8, d9", d7, d8, i32, (1 << 31) + 1, d9, i32, (1 << 31) + 2); 
    TESTINSN_bin("vmls.i32 d10, d11, d15", d10, d11, i32, -24, d15, i32, 120);

    printf("---- VMUL ----\n");
    TESTINSN_bin("vmul.i32 d0, d1, d2", d0, d1, i32, 24, d2, i32, 120);
    TESTINSN_bin("vmul.i32 d6, d7, d8", d6, d7, i32, 140, d8, i32, -120);
    TESTINSN_bin("vmul.i16 d9, d11, d12", d9, d11, i32, 0x140, d12, i32, 0x120);
    TESTINSN_bin("vmul.i8 d0, d1, d2", d0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin("vmul.i8 d10, d11, d12", d10, d11, i32, (1 << 5) + 1, d12, i32, (1 << 3) + 2);
    TESTINSN_bin("vmul.i16 d4, d5, d6", d4, d5, i32, (1 << 14) + 1, d6, i32, (1 << 13) + 2);
    TESTINSN_bin("vmul.i32 d7, d8, d9", d7, d8, i32, (1 << 31) + 1, d9, i32, (1 << 31) + 2);
    TESTINSN_bin("vmul.i8 d10, d11, d12", d10, d11, i32, (1 << 25) + 0xfeb2, d12, i32, (1 << 13) + 0xdf);
    TESTINSN_bin("vmul.i16 d4, d5, d6", d4, d5, i32, (1 << 14) - 0xabcd, d6, i32, (1 << 13) + 2);
    TESTINSN_bin("vmul.i32 d7, d8, d9", d7, d8, i32, (1 << 31), d9, i32, 12);
    TESTINSN_bin("vmul.i8 d10, d13, d12", d10, d13, i32, (1 << 5) + 1, d12, i32, (1 << 3) + 2);
    TESTINSN_bin("vmul.i16 d4, d5, d6", d4, d5, i32, (1 << 28) + 0xfe, d6, i32, (1 << 13) + 2);
    TESTINSN_bin("vmul.i32 d7, d8, d9", d7, d8, i32, (1 << 31) + 1, d9, i32, (1 << 31) + 2);
    TESTINSN_bin("vmul.i32 d10, d11, d15", d10, d11, i32, 24, d15, i32, 120);
    TESTINSN_bin("vmul.p8 q0, q1, q2", q0, q1, i32, 3, q2, i32, 3);
    TESTINSN_bin("vmul.p8 q0, q1, q2", q0, q1, i32, 12, q2, i8, 0x0f);

    printf("---- VMUL (by scalar) ----\n");
    TESTINSN_bin("vmul.i32 d0, d1, d4[0]", d0, d1, i32, 24, d4, i32, 120);
    TESTINSN_bin("vmul.i32 d31, d8, d7[1]", d31, d8, i32, 140, d7, i32, -120);
    TESTINSN_bin("vmul.i16 d30, d9, d7[3]", d30, d9, i32, 0x140, d7, i32, 0x120);
    TESTINSN_bin("vmul.i16 d4, d5, d6[2]", d4, d5, i32, (1 << 14) + 1, d6, i32, (1 << 13) + 2);
    TESTINSN_bin("vmul.i32 d4, d8, d15[1]", d4, d8, i32, (1 << 31) + 1, d15, i32, (1 << 31) + 2);
    TESTINSN_bin("vmul.i16 d4, d5, d6[0]", d4, d5, i32, (1 << 14) - 0xabcd, d6, i32, (1 << 13) + 2);
    TESTINSN_bin("vmul.i32 d7, d8, d1[1]", d7, d8, i32, (1 << 31), d1, i16, 12);
    TESTINSN_bin("vmul.i16 d4, d5, d6[0]", d4, d5, i32, (1 << 28) + 0xfe, d6, i32, (1 << 13) + 2);
    TESTINSN_bin("vmul.i32 d7, d8, d1[1]", d7, d8, i32, (1 << 31) + 1, d1, i32, (1 << 31) + 2);

    printf("---- VMLA (by scalar) ----\n");
    TESTINSN_bin("vmla.i32 d0, d1, d4[0]", d0, d1, i32, 24, d4, i32, 120);
    TESTINSN_bin("vmla.i32 d31, d8, d7[1]", d31, d8, i32, 140, d7, i32, -120);
    TESTINSN_bin("vmla.i16 d30, d9, d7[3]", d30, d9, i32, 0x140, d7, i32, 0x120);
    TESTINSN_bin("vmla.i16 d4, d5, d6[2]", d4, d5, i32, (1 << 14) + 1, d6, i32, (1 << 13) + 2);
    TESTINSN_bin("vmla.i32 d4, d8, d15[1]", d4, d8, i32, (1 << 31) + 1, d15, i32, (1 << 31) + 2);
    TESTINSN_bin("vmla.i16 d4, d5, d6[0]", d4, d5, i32, (1 << 14) - 0xabcd, d6, i32, (1 << 13) + 2);
    TESTINSN_bin("vmla.i32 d7, d8, d1[1]", d7, d8, i32, (1 << 31), d1, i16, 12);
    TESTINSN_bin("vmla.i16 d4, d5, d6[0]", d4, d5, i32, (1 << 28) + 0xfe, d6, i32, (1 << 13) + 2);
    TESTINSN_bin("vmla.i32 d7, d8, d1[1]", d7, d8, i32, (1 << 31) + 1, d1, i32, (1 << 31) + 2);

    printf("---- VMLS (by scalar) ----\n");
    TESTINSN_bin("vmls.i32 d0, d1, d4[0]", q0, q1, i32, 24, d4, i32, 120);
    TESTINSN_bin("vmls.i32 d31, d8, d7[1]", d31, d8, i32, 140, d7, i32, -120);
    TESTINSN_bin("vmls.i16 d30, d9, d7[3]", d30, d9, i32, 0x140, d7, i32, 0x120);
    TESTINSN_bin("vmls.i16 d4, d5, d6[2]", d4, d5, i32, (1 << 14) + 1, d6, i32, (1 << 13) + 2);
    TESTINSN_bin("vmls.i32 d4, d8, d15[1]", d4, d8, i32, (1 << 31) + 1, d15, i32, (1 << 31) + 2);
    TESTINSN_bin("vmls.i16 d4, d5, d6[0]", d4, d5, i32, (1 << 14) - 0xabcd, d6, i32, (1 << 13) + 2);
    TESTINSN_bin("vmls.i32 d7, d8, d1[1]", d7, d8, i32, (1 << 31), d1, i16, 12);
    TESTINSN_bin("vmls.i16 d4, d5, d6[0]", d4, d5, i32, (1 << 28) + 0xfe, d6, i32, (1 << 13) + 2);
    TESTINSN_bin("vmls.i32 d7, d8, d1[1]", d7, d8, i32, (1 << 31) + 1, d1, i32, (1 << 31) + 2);

    printf("---- VRSHR ----\n");
    TESTINSN_un("vrshr.s8 d0, d1, #0", d0, d1, i32, -1);
    TESTINSN_un("vrshr.s8 d0, d1, #1", d0, d1, i32, -1);
    TESTINSN_un("vrshr.s16 d3, d4, #2", d3, d4, i32, -0x7c);
    TESTINSN_un("vrshr.s32 d2, d5, #31", d2, d5, i32, -1);
    TESTINSN_un("vrshr.s8 d6, d7, #7", d6, d7, i32, 0xffff);
    TESTINSN_un("vrshr.s16 d8, d9, #12", d8, d9, i32, -10);
    TESTINSN_un("vrshr.s32 d10, d11, #5", d10, d11, i32, 10234);
    TESTINSN_un("vrshr.u8 d12, d13, #1", d12, d13, i32, -1);
    TESTINSN_un("vrshr.u16 d14, d15, #11", d14, d15, i32, -1);
    TESTINSN_un("vrshr.u32 d10, d11, #9", d10, d11, i32, 1000);
    TESTINSN_un("vrshr.u8 d7, d13, #7", d7, d13, i32, -1);
    TESTINSN_un("vrshr.u16 d8, d1, #5", d8, d1, i32, 0xabcf);
    TESTINSN_un("vrshr.u32 d12, d3, #15", d12, d3, i32, -0x1b0);
    TESTINSN_un("vrshr.u64 d0, d1, #42", d0, d1, i32, -1);
    TESTINSN_un("vrshr.s64 d6, d7, #12", d6, d7, i32, 0xfac);
    TESTINSN_un("vrshr.u64 d8, d4, #9", d8, d4, i32, 13560);
    TESTINSN_un("vrshr.s64 d9, d12, #11", d9, d12, i32, 98710);

    printf("---- VRSRA ----\n");
    TESTINSN_un("vrsra.s8 d0, d1, #1", d0, d1, i32, -1);
    TESTINSN_un("vrsra.s16 d3, d4, #2", d3, d4, i32, -0x7c);
    TESTINSN_un("vrsra.s32 d2, d5, #31", d2, d5, i32, -1);
    TESTINSN_un("vrsra.s8 d6, d7, #7", d6, d7, i32, 0xffff);
    TESTINSN_un("vrsra.s16 d8, d9, #12", d8, d9, i32, -10);
    TESTINSN_un("vrsra.s32 d10, d11, #5", d10, d11, i32, 10234);
    TESTINSN_un("vrsra.u8 d12, d13, #1", d12, d13, i32, -1);
    TESTINSN_un("vrsra.u16 d14, d15, #11", d14, d15, i32, -1);
    TESTINSN_un("vrsra.u32 d10, d11, #9", d10, d11, i32, 1000);
    TESTINSN_un("vrsra.u8 d7, d13, #7", d7, d13, i32, -1);
    TESTINSN_un("vrsra.u16 d8, d1, #5", d8, d1, i32, 0xabcf);
    TESTINSN_un("vrsra.u32 d12, d3, #15", d12, d3, i32, -0x1b0);
    TESTINSN_un("vrsra.u64 d0, d1, #42", d0, d1, i32, -1);
    TESTINSN_un("vrsra.s64 d6, d7, #12", d6, d7, i32, 0xfac);
    TESTINSN_un("vrsra.u64 d8, d4, #9", d8, d4, i32, 13560);
    TESTINSN_un("vrsra.s64 d9, d12, #11", d9, d12, i32, 98710);

    printf("---- VSHR ----\n");
    TESTINSN_un("vshr.s8 d0, d1, #0", d0, d1, i32, -1);
    TESTINSN_un("vshr.s8 d0, d1, #1", d0, d1, i32, -1);
    TESTINSN_un("vshr.s16 d3, d4, #2", d3, d4, i32, -0x7c);
    TESTINSN_un("vshr.s32 d2, d5, #31", d2, d5, i32, -1);
    TESTINSN_un("vshr.s8 d6, d7, #7", d6, d7, i32, 0xffff);
    TESTINSN_un("vshr.s16 d8, d9, #12", d8, d9, i32, -10);
    TESTINSN_un("vshr.s32 d10, d11, #5", d10, d11, i32, 10234);
    TESTINSN_un("vshr.u8 d12, d13, #1", d12, d13, i32, -1);
    TESTINSN_un("vshr.u16 d14, d15, #11", d14, d15, i32, -1);
    TESTINSN_un("vshr.u32 d10, d11, #9", d10, d11, i32, 1000);
    TESTINSN_un("vshr.u8 d7, d13, #7", d7, d13, i32, -1);
    TESTINSN_un("vshr.u16 d8, d1, #5", d8, d1, i32, 0xabcf);
    TESTINSN_un("vshr.u32 d12, d3, #15", d12, d3, i32, -0x1b0);
    TESTINSN_un("vshr.u64 d0, d1, #42", d0, d1, i32, -1);
    TESTINSN_un("vshr.s64 d6, d7, #12", d6, d7, i32, 0xfac);
    TESTINSN_un("vshr.u64 d8, d4, #9", d8, d4, i32, 13560);
    TESTINSN_un("vshr.s64 d9, d12, #11", d9, d12, i32, 98710);

    printf("---- VSRA ----\n");
    TESTINSN_un("vsra.s8 d0, d1, #1", d0, d1, i32, -1);
    TESTINSN_un("vsra.s16 d3, d4, #2", d3, d4, i32, -0x7c);
    TESTINSN_un("vsra.s32 d2, d5, #31", d2, d5, i32, -1);
    TESTINSN_un("vsra.s8 d6, d7, #7", d6, d7, i32, 0xffff);
    TESTINSN_un("vsra.s16 d8, d9, #12", d8, d9, i32, -10);
    TESTINSN_un("vsra.s32 d10, d11, #5", d10, d11, i32, 10234);
    TESTINSN_un("vsra.u8 d12, d13, #1", d12, d13, i32, -1);
    TESTINSN_un("vsra.u16 d14, d15, #11", d14, d15, i32, -1);
    TESTINSN_un("vsra.u32 d10, d11, #9", d10, d11, i32, 1000);
    TESTINSN_un("vsra.u8 d7, d13, #7", d7, d13, i32, -1);
    TESTINSN_un("vsra.u16 d8, d1, #5", d8, d1, i32, 0xabcf);
    TESTINSN_un("vsra.u32 d12, d3, #15", d12, d3, i32, -0x1b0);
    TESTINSN_un("vsra.u64 d0, d1, #42", d0, d1, i32, -1);
    TESTINSN_un("vsra.s64 d6, d7, #12", d6, d7, i32, 0xfac);
    TESTINSN_un("vsra.u64 d8, d4, #9", d8, d4, i32, 13560);
    TESTINSN_un("vsra.s64 d9, d12, #11", d9, d12, i32, 98710);

    printf("---- VSRI ----\n");
    TESTINSN_un("vsri.16 d0, d1, #1", d0, d1, i32, -1);
    TESTINSN_un("vsri.16 d3, d4, #2", d3, d4, i32, -0x7c);
    TESTINSN_un("vsri.32 d2, d5, #31", d2, d5, i32, -1);
    TESTINSN_un("vsri.8 d6, d7, #7", d6, d7, i32, 0xffff);
    TESTINSN_un("vsri.16 d8, d9, #12", d8, d9, i32, -10);
    TESTINSN_un("vsri.32 d10, d11, #5", d10, d11, i32, 10234);
    TESTINSN_un("vsri.8 d12, d13, #1", d12, d13, i32, -1);
    TESTINSN_un("vsri.16 d14, d15, #11", d14, d15, i32, -1);
    TESTINSN_un("vsri.32 d10, d11, #9", d10, d11, i32, 1000);
    TESTINSN_un("vsri.8 d7, d13, #7", d7, d13, i32, -1);
    TESTINSN_un("vsri.16 d8, d1, #5", d8, d1, i32, 0xabcf);
    TESTINSN_un("vsri.32 d12, d3, #15", d12, d3, i32, -0x1b0);
    TESTINSN_un("vsri.64 d0, d1, #42", d0, d1, i32, -1);
    TESTINSN_un("vsri.64 d6, d7, #12", d6, d7, i32, 0xfac);
    TESTINSN_un("vsri.64 d8, d4, #9", d8, d4, i32, 13560);
    TESTINSN_un("vsri.64 d9, d12, #11", d9, d12, i32, 98710);

    printf("---- VMOV (ARM core register to scalar) ----\n");
    TESTINSN_core_to_scalar("vmov.32 d0[0], r5", d0, r5, 13);
    TESTINSN_core_to_scalar("vmov.32 d1[1], r3", d1, r3, 12);
    TESTINSN_core_to_scalar("vmov.16 d0[0], r5", d0, r5, 13);
    TESTINSN_core_to_scalar("vmov.16 d2[2], r6", d2, r6, 14);
    TESTINSN_core_to_scalar("vmov.16 d3[3], r1", d3, r1, 17);
    TESTINSN_core_to_scalar("vmov.8 d0[0], r5", d0, r5, 13);
    TESTINSN_core_to_scalar("vmov.8 d0[1], r5", d0, r5, 13);
    TESTINSN_core_to_scalar("vmov.8 d0[2], r5", d0, r5, 13);
    TESTINSN_core_to_scalar("vmov.8 d0[3], r5", d0, r5, 13);
    TESTINSN_core_to_scalar("vmov.8 d0[4], r5", d0, r5, 13);
    TESTINSN_core_to_scalar("vmov.8 d0[5], r5", d0, r5, 13);
    TESTINSN_core_to_scalar("vmov.8 d0[6], r5", d0, r5, 13);
    TESTINSN_core_to_scalar("vmov.8 d31[7], r5", d31, r5, 13);

    printf("---- VMOV (scalar toARM core register) ----\n");
    TESTINSN_scalar_to_core("vmov.32 r5, d0[0]", r5, d0, i32, 0x11223344);
    TESTINSN_scalar_to_core("vmov.32 r6, d5[1]", r6, d5, i32, 0x11223344);
    TESTINSN_scalar_to_core("vmov.u16 r5, d31[0]", r5, d31, i32, 0x11223344);
    TESTINSN_scalar_to_core("vmov.u16 r5, d30[1]", r5, d30, i32, 0x11223344);
    TESTINSN_scalar_to_core("vmov.u16 r5, d31[2]", r5, d31, i32, 0x11223344);
    TESTINSN_scalar_to_core("vmov.u16 r5, d31[3]", r5, d31, i32, 0x11223344);
    TESTINSN_scalar_to_core("vmov.u8 r2, d4[0]", r2, d4, i32, 0x11223344);
    TESTINSN_scalar_to_core("vmov.u8 r2, d4[1]", r2, d4, i32, 0x11223344);
    TESTINSN_scalar_to_core("vmov.u8 r2, d4[2]", r2, d4, i32, 0x11223344);
    TESTINSN_scalar_to_core("vmov.u8 r2, d4[3]", r2, d4, i32, 0x11223344);
    TESTINSN_scalar_to_core("vmov.u8 r2, d4[4]", r2, d4, i32, 0x11223344);
    TESTINSN_scalar_to_core("vmov.u8 r2, d4[5]", r2, d4, i32, 0x11223344);
    TESTINSN_scalar_to_core("vmov.u8 r2, d4[6]", r2, d4, i32, 0x11223344);
    TESTINSN_scalar_to_core("vmov.u8 r2, d4[7]", r2, d4, i32, 0x11223344);
    TESTINSN_scalar_to_core("vmov.s16 r5, d31[0]", r5, d31, i8, 128);
    TESTINSN_scalar_to_core("vmov.s16 r5, d30[1]", r5, d30, i8, 128);
    TESTINSN_scalar_to_core("vmov.s16 r5, d31[2]", r5, d31, i8, 128);
    TESTINSN_scalar_to_core("vmov.s16 r5, d31[3]", r5, d31, i8, 128);
    TESTINSN_scalar_to_core("vmov.s8 r2, d4[0]", r2, d4, i8, 128);
    TESTINSN_scalar_to_core("vmov.s8 r2, d4[1]", r2, d4, i8, 128);
    TESTINSN_scalar_to_core("vmov.s8 r2, d4[2]", r2, d4, i8, 128);
    TESTINSN_scalar_to_core("vmov.s8 r2, d4[3]", r2, d4, i8, 128);
    TESTINSN_scalar_to_core("vmov.s8 r2, d4[4]", r2, d4, i8, 128);
    TESTINSN_scalar_to_core("vmov.s8 r2, d4[5]", r2, d4, i8, 130);
    TESTINSN_scalar_to_core("vmov.s8 r2, d4[6]", r2, d4, i8, 129);
    TESTINSN_scalar_to_core("vmov.s8 r2, d4[7]", r2, d4, i8, 131);

    printf("---- VLD1 (multiple single elements) ----\n");
    TESTINSN_VLDn("vld1.8 {d0}", d0, d0, d0, d0);
    TESTINSN_VLDn("vld1.16 {d0}", d0, d0, d0, d0);
    TESTINSN_VLDn("vld1.32 {d0}", d0, d0, d0, d0);
    TESTINSN_VLDn("vld1.64 {d0}", d0, d0, d0, d0);
    TESTINSN_VLDn("vld1.8 {d9}", d9, d9, d9, d9);
    TESTINSN_VLDn("vld1.16 {d17}", d17, d17, d17, d17);
    TESTINSN_VLDn("vld1.32 {d31}", d31, d31, d31, d31);
    TESTINSN_VLDn("vld1.64 {d14}", d14, d14, d14, d14);
    TESTINSN_VLDn("vld1.8 {d0-d1}", d0, d1, d0, d1);
    TESTINSN_VLDn("vld1.16 {d0-d1}", d0, d1, d0, d1);
    TESTINSN_VLDn("vld1.32 {d5-d6}", d5, d6, d5, d6);
    TESTINSN_VLDn("vld1.64 {d30-d31}", d30, d31, d30, d31);
    TESTINSN_VLDn("vld1.8 {d0-d2}", d0, d1, d2, d0);
    TESTINSN_VLDn("vld1.16 {d0-d2}", d0, d1, d2, d0);
    TESTINSN_VLDn("vld1.32 {d0-d2}", d0, d1, d2, d0);
    TESTINSN_VLDn("vld1.64 {d0-d2}", d0, d1, d2, d0);
    TESTINSN_VLDn("vld1.8 {d0-d3}", d0, d1, d2, d3);
    TESTINSN_VLDn("vld1.16 {d0-d3}", d0, d1, d2, d3);
    TESTINSN_VLDn("vld1.32 {d0-d3}", d0, d1, d2, d3);
    TESTINSN_VLDn("vld1.64 {d0-d3}", d0, d1, d2, d3);

    printf("---- VLD1 (single element to one lane) ----\n");
    TESTINSN_VLDn("vld1.32 {d0[0]}", d0, d0, d0, d0);
    TESTINSN_VLDn("vld1.32 {d0[1]}", d0, d0, d0, d0);
    TESTINSN_VLDn("vld1.16 {d1[0]}", d1, d1, d1, d1);
    TESTINSN_VLDn("vld1.16 {d1[1]}", d1, d1, d1, d1);
    TESTINSN_VLDn("vld1.16 {d1[2]}", d1, d1, d1, d1);
    TESTINSN_VLDn("vld1.16 {d1[3]}", d1, d1, d1, d1);
    TESTINSN_VLDn("vld1.8 {d0[7]}", d0, d0, d0, d0);
    TESTINSN_VLDn("vld1.8 {d1[6]}", d1, d1, d1, d1);
    TESTINSN_VLDn("vld1.8 {d0[5]}", d0, d0, d0, d0);
    TESTINSN_VLDn("vld1.8 {d0[4]}", d0, d0, d0, d0);
    TESTINSN_VLDn("vld1.8 {d20[3]}", d20, d20, d20, d20);
    TESTINSN_VLDn("vld1.8 {d0[2]}", d0, d0, d0, d0);
    TESTINSN_VLDn("vld1.8 {d17[1]}", d17, d17, d17, d17);
    TESTINSN_VLDn("vld1.8 {d30[0]}", d30, d30, d30, d30);

    printf("---- VLD1 (single element to all lanes) ----\n");
    TESTINSN_VLDn("vld1.8 {d0[]}", d0, d0, d0, d0);
    TESTINSN_VLDn("vld1.16 {d0[]}", d0, d0, d0, d0);
    TESTINSN_VLDn("vld1.32 {d0[]}", d0, d0, d0, d0);
    TESTINSN_VLDn("vld1.8 {d9[]}", d9, d9, d9, d9);
    TESTINSN_VLDn("vld1.16 {d17[]}", d17, d17, d17, d17);
    TESTINSN_VLDn("vld1.32 {d31[]}", d31, d31, d31, d31);
    TESTINSN_VLDn("vld1.8 {d0[],d1[]}", d0, d1, d0, d1);
    TESTINSN_VLDn("vld1.16 {d0[],d1[]}", d0, d1, d0, d1);
    TESTINSN_VLDn("vld1.32 {d5[],d6[]}", d5, d6, d5, d6);

    printf("---- VLD2 (multiple 2-elements) ----\n");
    TESTINSN_VLDn("vld2.8 {d30-d31}", d30, d31, d30, d31);
    TESTINSN_VLDn("vld2.16 {d0-d1}", d0, d1, d0, d1);
    TESTINSN_VLDn("vld2.32 {d0-d1}", d0, d1, d0, d1);
    TESTINSN_VLDn("vld2.8 {d10,d12}", d10, d12, d10, d12);
    TESTINSN_VLDn("vld2.16 {d20,d22}", d20, d22, d20, d22);
    TESTINSN_VLDn("vld2.32 {d0,d2}", d0, d2, d0, d2);
    TESTINSN_VLDn("vld2.8 {d0-d3}", d0, d1, d2, d3);
    TESTINSN_VLDn("vld2.16 {d20-d23}", d20, d21, d22, d23);
    TESTINSN_VLDn("vld2.32 {d0-d3}", d0, d1, d2, d3);

    printf("---- VLD2 (single 2-element structure to one lane) ----\n");
    TESTINSN_VLDn("vld2.32 {d0[0],d1[0]}", d0, d1, d0, d1);
    TESTINSN_VLDn("vld2.32 {d0[1],d1[1]}", d0, d1, d0, d1);
    TESTINSN_VLDn("vld2.32 {d0[0],d2[0]}", d0, d2, d0, d2);
    TESTINSN_VLDn("vld2.32 {d0[1],d2[1]}", d0, d2, d0, d2);
    TESTINSN_VLDn("vld2.16 {d1[0],d2[0]}", d1, d2, d1, d2);
    TESTINSN_VLDn("vld2.16 {d1[1],d2[1]}", d1, d2, d1, d2);
    TESTINSN_VLDn("vld2.16 {d1[2],d2[2]}", d1, d2, d1, d2);
    TESTINSN_VLDn("vld2.16 {d1[3],d2[3]}", d1, d2, d1, d2);
    TESTINSN_VLDn("vld2.16 {d1[0],d3[0]}", d1, d3, d1, d3);
    TESTINSN_VLDn("vld2.16 {d1[1],d3[1]}", d1, d3, d1, d3);
    TESTINSN_VLDn("vld2.16 {d1[2],d3[2]}", d1, d3, d1, d3);
    TESTINSN_VLDn("vld2.16 {d1[3],d3[3]}", d1, d3, d1, d3);
    TESTINSN_VLDn("vld2.8 {d0[7],d1[7]}", d0, d1, d0, d1);
    TESTINSN_VLDn("vld2.8 {d1[6],d2[6]}", d1, d2, d1, d2);
    TESTINSN_VLDn("vld2.8 {d0[5],d1[5]}", d0, d1, d0, d1);
    TESTINSN_VLDn("vld2.8 {d0[4],d1[4]}", d0, d1, d0, d1);
    TESTINSN_VLDn("vld2.8 {d20[3],d21[3]}", d20, d21, d20, d21);
    TESTINSN_VLDn("vld2.8 {d0[2],d1[2]}", d0, d1, d0, d1);
    TESTINSN_VLDn("vld2.8 {d17[1],d18[1]}", d17, d18, d17, d18);
    TESTINSN_VLDn("vld2.8 {d30[0],d31[0]}", d30, d31, d30, d31);

    printf("---- VLD2 (2-elements to all lanes) ----\n");
    TESTINSN_VLDn("vld2.8 {d0[],d1[]}", d0, d1, d0, d1);
    TESTINSN_VLDn("vld2.16 {d0[],d1[]}", d0, d1, d0, d1);
    TESTINSN_VLDn("vld2.32 {d0[],d1[]}", d0, d1, d0, d1);
    TESTINSN_VLDn("vld2.8 {d9[],d11[]}", d9, d11, d9, d11);
    TESTINSN_VLDn("vld2.16 {d17[],d18[]}", d17, d18, d17, d18);
    TESTINSN_VLDn("vld2.32 {d30[],d31[]}", d30, d31, d30, d31);
    TESTINSN_VLDn("vld2.8 {d0[],d2[]}", d0, d2, d0, d2);
    TESTINSN_VLDn("vld2.16 {d0[],d2[]}", d0, d2, d0, d2);
    TESTINSN_VLDn("vld2.32 {d5[],d7[]}", d5, d7, d5, d7);

    printf("---- VLD3 (multiple 3-elements) ----\n");
    TESTINSN_VLDn("vld3.8 {d20-d22}", d20, d21, d22, d20);
    TESTINSN_VLDn("vld3.16 {d0-d2}", d0, d1, d2, d0);
    TESTINSN_VLDn("vld3.32 {d0-d2}", d0, d1, d2, d0);
    TESTINSN_VLDn("vld3.8 {d0,d2,d4}", d0, d2, d4, d0);
    TESTINSN_VLDn("vld3.16 {d20,d22,d24}", d20, d22, d24, d20);
    TESTINSN_VLDn("vld3.32 {d0,d2,d4}", d0, d2, d4, d0);

    printf("---- VLD3 (single 3-element structure to one lane) ----\n");
    TESTINSN_VLDn("vld3.32 {d0[0],d1[0],d2[0]}", d0, d1, d2, d1);
    TESTINSN_VLDn("vld3.32 {d0[1],d1[1],d2[1]}", d0, d1, d2, d1);
    TESTINSN_VLDn("vld3.32 {d0[0],d2[0],d4[0]}", d0, d2, d4, d2);
    TESTINSN_VLDn("vld3.32 {d0[1],d2[1],d4[1]}", d0, d2, d4, d2);
    TESTINSN_VLDn("vld3.16 {d1[0],d2[0],d3[0]}", d1, d2, d3, d2);
    TESTINSN_VLDn("vld3.16 {d1[1],d2[1],d3[1]}", d1, d2, d3, d2);
    TESTINSN_VLDn("vld3.16 {d1[2],d2[2],d3[2]}", d1, d2, d3, d2);
    TESTINSN_VLDn("vld3.16 {d1[3],d2[3],d3[3]}", d1, d2, d3, d2);
    TESTINSN_VLDn("vld3.16 {d1[0],d3[0],d5[0]}", d1, d3, d3, d5);
    TESTINSN_VLDn("vld3.16 {d1[1],d3[1],d5[1]}", d1, d3, d3, d5);
    TESTINSN_VLDn("vld3.16 {d1[2],d3[2],d5[2]}", d1, d3, d3, d5);
    TESTINSN_VLDn("vld3.16 {d1[3],d3[3],d5[3]}", d1, d3, d3, d5);
    TESTINSN_VLDn("vld3.8 {d0[7],d1[7],d2[7]}", d0, d1, d2, d1);
    TESTINSN_VLDn("vld3.8 {d1[6],d2[6],d3[6]}", d1, d2, d3, d2);
    TESTINSN_VLDn("vld3.8 {d0[5],d1[5],d2[5]}", d0, d1, d2, d1);
    TESTINSN_VLDn("vld3.8 {d0[4],d1[4],d2[4]}", d0, d1, d2, d1);
    TESTINSN_VLDn("vld3.8 {d20[3],d21[3],d22[3]}", d20, d21, d22, d21);
    TESTINSN_VLDn("vld3.8 {d0[2],d1[2],d2[2]}", d0, d1, d2, d1);
    TESTINSN_VLDn("vld3.8 {d17[1],d18[1],d19[1]}", d17, d18, d19, d18);
    TESTINSN_VLDn("vld3.8 {d29[0],d30[0],d31[0]}", d30, d31, d29, d31);

    printf("---- VLD3 (3-elements to all lanes) ----\n");
    TESTINSN_VLDn("vld3.8 {d0[],d1[],d2[]}", d0, d1, d2, d1);
    TESTINSN_VLDn("vld3.16 {d0[],d1[],d2[]}", d0, d1, d2, d1);
    TESTINSN_VLDn("vld3.32 {d0[],d1[],d2[]}", d0, d1, d2, d1);
    TESTINSN_VLDn("vld3.8 {d9[],d11[],d13[]}", d9, d11, d13, d11);
    TESTINSN_VLDn("vld3.16 {d17[],d18[],d19[]}", d17, d18, d19, d18);
    TESTINSN_VLDn("vld3.32 {d29[],d30[],d31[]}", d29, d30, d30, d31);
    TESTINSN_VLDn("vld3.8 {d0[],d2[],d4[]}", d0, d2, d4, d2);
    TESTINSN_VLDn("vld3.16 {d0[],d2[],d4[]}", d0, d2, d4, d2);
    TESTINSN_VLDn("vld3.32 {d5[],d7[],d9[]}", d5, d7, d9, d7);

    printf("---- VLD4 (multiple 3-elements) ----\n");
    TESTINSN_VLDn("vld4.8 {d0-d3}", d0, d1, d2, d3);
    TESTINSN_VLDn("vld4.16 {d20-d23}", d20, d21, d22, d23);
    TESTINSN_VLDn("vld4.32 {d0-d3}", d0, d1, d2, d3);
    TESTINSN_VLDn("vld4.8 {d0,d2,d4,d6}", d0, d2, d4, d6);
    TESTINSN_VLDn("vld4.16 {d1,d3,d5,d7}", d1, d3, d5, d7);
    TESTINSN_VLDn("vld4.32 {d20,d22,d24,d26}", d20, d22, d24, d26);

    printf("---- VLD4 (single 4-element structure to one lane) ----\n");
    TESTINSN_VLDn("vld4.32 {d0[0],d1[0],d2[0],d3[0]}", d0, d1, d2, d3);
    TESTINSN_VLDn("vld4.32 {d0[1],d1[1],d2[1],d3[1]}", d0, d1, d2, d4);
    TESTINSN_VLDn("vld4.32 {d0[0],d2[0],d4[0],d6[0]}", d0, d2, d4, d6);
    TESTINSN_VLDn("vld4.32 {d0[1],d2[1],d4[1],d6[1]}", d0, d2, d4, d6);
    TESTINSN_VLDn("vld4.16 {d1[0],d2[0],d3[0],d4[0]}", d1, d2, d3, d4);
    TESTINSN_VLDn("vld4.16 {d1[1],d2[1],d3[1],d4[1]}", d1, d2, d3, d4);
    TESTINSN_VLDn("vld4.16 {d1[2],d2[2],d3[2],d4[2]}", d1, d2, d3, d4);
    TESTINSN_VLDn("vld4.16 {d1[3],d2[3],d3[3],d4[3]}", d1, d2, d3, d4);
    TESTINSN_VLDn("vld4.16 {d1[0],d3[0],d5[0],d7[0]}", d1, d3, d5, d7);
    TESTINSN_VLDn("vld4.16 {d1[1],d3[1],d5[1],d7[1]}", d1, d3, d5, d7);
    TESTINSN_VLDn("vld4.16 {d1[2],d3[2],d5[2],d7[2]}", d1, d3, d5, d7);
    TESTINSN_VLDn("vld4.16 {d1[3],d3[3],d5[3],d7[3]}", d1, d3, d5, d7);
    TESTINSN_VLDn("vld4.8 {d0[7],d1[7],d2[7],d3[7]}", d0, d1, d2, d3);
    TESTINSN_VLDn("vld4.8 {d1[6],d2[6],d3[6],d4[6]}", d1, d2, d3, d4);
    TESTINSN_VLDn("vld4.8 {d0[5],d1[5],d2[5],d3[5]}", d0, d1, d2, d3);
    TESTINSN_VLDn("vld4.8 {d0[4],d1[4],d2[4],d3[4]}", d0, d1, d2, d3);
    TESTINSN_VLDn("vld4.8 {d20[3],d21[3],d22[3],d23[3]}", d20, d21, d22, d23);
    TESTINSN_VLDn("vld4.8 {d0[2],d1[2],d2[2],d3[2]}", d0, d1, d2, d3);
    TESTINSN_VLDn("vld4.8 {d17[1],d18[1],d19[1],d20[1]}", d17, d18, d19, d20);
    TESTINSN_VLDn("vld4.8 {d28[0],d29[0],d30[0],d31[0]}", d28, d29, d30, d31);

    printf("---- VLD4 (4-elements to all lanes) ----\n");
    TESTINSN_VLDn("vld4.8 {d0[],d1[],d2[],d3[]}", d0, d1, d2, d3);
    TESTINSN_VLDn("vld4.16 {d0[],d1[],d2[],d3[]}", d0, d1, d2, d3);
    TESTINSN_VLDn("vld4.32 {d0[],d1[],d2[],d3[]}", d0, d1, d2, d3);
    TESTINSN_VLDn("vld4.8 {d9[],d11[],d13[],d15[]}", d9, d11, d13, d15);
    TESTINSN_VLDn("vld4.16 {d17[],d18[],d19[],d20[]}", d17, d18, d19, d20);
    TESTINSN_VLDn("vld4.32 {d28[],d29[],d30[],d31[]}", d28, d29, d30, d31);
    TESTINSN_VLDn("vld4.8 {d0[],d2[],d4[],d6[]}", d0, d2, d4, d6);
    TESTINSN_VLDn("vld4.16 {d0[],d2[],d4[],d6[]}", d0, d2, d4, d6);
    TESTINSN_VLDn("vld4.32 {d5[],d7[],d9[],d11[]}", d5, d7, d9, d11);

    printf("---- VST1 (multiple single elements) ----\n");
    TESTINSN_VSTn("vst1.8 {d0}", d0, d0, d0, d0);
    TESTINSN_VSTn("vst1.16 {d0}", d0, d0, d0, d0);
    TESTINSN_VSTn("vst1.32 {d0}", d0, d0, d0, d0);
    TESTINSN_VSTn("vst1.64 {d0}", d0, d0, d0, d0);
    TESTINSN_VSTn("vst1.8 {d9}", d9, d9, d9, d9);
    TESTINSN_VSTn("vst1.16 {d17}", d17, d17, d17, d17);
    TESTINSN_VSTn("vst1.32 {d31}", d31, d31, d31, d31);
    TESTINSN_VSTn("vst1.64 {d14}", d14, d14, d14, d14);
    TESTINSN_VSTn("vst1.8 {d0-d1}", d0, d1, d0, d1);
    TESTINSN_VSTn("vst1.16 {d0-d1}", d0, d1, d0, d1);
    TESTINSN_VSTn("vst1.32 {d5-d6}", d5, d6, d5, d6);
    TESTINSN_VSTn("vst1.64 {d30-d31}", d30, d31, d30, d31);
    TESTINSN_VSTn("vst1.8 {d0-d2}", d0, d1, d2, d0);
    TESTINSN_VSTn("vst1.16 {d0-d2}", d0, d1, d2, d0);
    TESTINSN_VSTn("vst1.32 {d0-d2}", d0, d1, d2, d0);
    TESTINSN_VSTn("vst1.64 {d0-d2}", d0, d1, d2, d0);
    TESTINSN_VSTn("vst1.8 {d0-d3}", d0, d1, d2, d3);
    TESTINSN_VSTn("vst1.16 {d0-d3}", d0, d1, d2, d3);
    TESTINSN_VSTn("vst1.32 {d0-d3}", d0, d1, d2, d3);
    TESTINSN_VSTn("vst1.64 {d0-d3}", d0, d1, d2, d3);

    printf("---- VST1 (single element from one lane) ----\n");
    TESTINSN_VSTn("vst1.32 {d0[0]}", d0, d0, d0, d0);
    TESTINSN_VSTn("vst1.32 {d0[1]}", d0, d0, d0, d0);
    TESTINSN_VSTn("vst1.16 {d1[0]}", d1, d1, d1, d1);
    TESTINSN_VSTn("vst1.16 {d1[1]}", d1, d1, d1, d1);
    TESTINSN_VSTn("vst1.16 {d1[2]}", d1, d1, d1, d1);
    TESTINSN_VSTn("vst1.16 {d1[3]}", d1, d1, d1, d1);
    TESTINSN_VSTn("vst1.8 {d0[7]}", d0, d0, d0, d0);
    TESTINSN_VSTn("vst1.8 {d1[6]}", d1, d1, d1, d1);
    TESTINSN_VSTn("vst1.8 {d0[5]}", d0, d0, d0, d0);
    TESTINSN_VSTn("vst1.8 {d0[4]}", d0, d0, d0, d0);
    TESTINSN_VSTn("vst1.8 {d20[3]}", d20, d20, d20, d20);
    TESTINSN_VSTn("vst1.8 {d0[2]}", d0, d0, d0, d0);
    TESTINSN_VSTn("vst1.8 {d17[1]}", d17, d17, d17, d17);
    TESTINSN_VSTn("vst1.8 {d30[0]}", d30, d30, d30, d30);

    printf("---- VST2 (multiple 2-elements) ----\n");
    TESTINSN_VSTn("vst2.8 {d30-d31}", d30, d31, d30, d31);
    TESTINSN_VSTn("vst2.16 {d0-d1}", d0, d1, d0, d1);
    TESTINSN_VSTn("vst2.32 {d0-d1}", d0, d1, d0, d1);
    TESTINSN_VSTn("vst2.8 {d10,d12}", d10, d12, d10, d12);
    TESTINSN_VSTn("vst2.16 {d20,d22}", d20, d22, d20, d22);
    TESTINSN_VSTn("vst2.32 {d0,d2}", d0, d2, d0, d2);
    TESTINSN_VSTn("vst2.8 {d0-d3}", d0, d1, d2, d3);
    TESTINSN_VSTn("vst2.16 {d20-d23}", d20, d21, d22, d23);
    TESTINSN_VSTn("vst2.32 {d0-d3}", d0, d1, d2, d3);

    printf("---- VST2 (single 2-element structure from one lane) ----\n");
    TESTINSN_VSTn("vst2.32 {d0[0],d1[0]}", d0, d1, d0, d1);
    TESTINSN_VSTn("vst2.32 {d0[1],d1[1]}", d0, d1, d0, d1);
    TESTINSN_VSTn("vst2.32 {d0[0],d2[0]}", d0, d2, d0, d2);
    TESTINSN_VSTn("vst2.32 {d0[1],d2[1]}", d0, d2, d0, d2);
    TESTINSN_VSTn("vst2.16 {d1[0],d2[0]}", d1, d2, d1, d2);
    TESTINSN_VSTn("vst2.16 {d1[1],d2[1]}", d1, d2, d1, d2);
    TESTINSN_VSTn("vst2.16 {d1[2],d2[2]}", d1, d2, d1, d2);
    TESTINSN_VSTn("vst2.16 {d1[3],d2[3]}", d1, d2, d1, d2);
    TESTINSN_VSTn("vst2.16 {d1[0],d3[0]}", d1, d3, d1, d3);
    TESTINSN_VSTn("vst2.16 {d1[1],d3[1]}", d1, d3, d1, d3);
    TESTINSN_VSTn("vst2.16 {d1[2],d3[2]}", d1, d3, d1, d3);
    TESTINSN_VSTn("vst2.16 {d1[3],d3[3]}", d1, d3, d1, d3);
    TESTINSN_VSTn("vst2.8 {d0[7],d1[7]}", d0, d1, d0, d1);
    TESTINSN_VSTn("vst2.8 {d1[6],d2[6]}", d1, d2, d1, d2);
    TESTINSN_VSTn("vst2.8 {d0[5],d1[5]}", d0, d1, d0, d1);
    TESTINSN_VSTn("vst2.8 {d0[4],d1[4]}", d0, d1, d0, d1);
    TESTINSN_VSTn("vst2.8 {d20[3],d21[3]}", d20, d21, d20, d21);
    TESTINSN_VSTn("vst2.8 {d0[2],d1[2]}", d0, d1, d0, d1);
    TESTINSN_VSTn("vst2.8 {d17[1],d18[1]}", d17, d18, d17, d18);
    TESTINSN_VSTn("vst2.8 {d30[0],d31[0]}", d30, d31, d30, d31);

    printf("---- VST3 (multiple 3-elements) ----\n");
    TESTINSN_VSTn("vst3.8 {d20-d22}", d20, d21, d22, d20);
    TESTINSN_VSTn("vst3.16 {d0-d2}", d0, d1, d2, d0);
    TESTINSN_VSTn("vst3.32 {d0-d2}", d0, d1, d2, d0);
    TESTINSN_VSTn("vst3.8 {d0,d2,d4}", d0, d2, d4, d0);
    TESTINSN_VSTn("vst3.16 {d20,d22,d24}", d20, d22, d24, d20);
    TESTINSN_VSTn("vst3.32 {d0,d2,d4}", d0, d2, d4, d0);

    printf("---- VST3 (single 3-element structure from one lane) ----\n");
    TESTINSN_VSTn("vst3.32 {d0[0],d1[0],d2[0]}", d0, d1, d2, d1);
    TESTINSN_VSTn("vst3.32 {d0[1],d1[1],d2[1]}", d0, d1, d2, d1);
    TESTINSN_VSTn("vst3.32 {d0[0],d2[0],d4[0]}", d0, d2, d4, d2);
    TESTINSN_VSTn("vst3.32 {d0[1],d2[1],d4[1]}", d0, d2, d4, d2);
    TESTINSN_VSTn("vst3.16 {d1[0],d2[0],d3[0]}", d1, d2, d3, d2);
    TESTINSN_VSTn("vst3.16 {d1[1],d2[1],d3[1]}", d1, d2, d3, d2);
    TESTINSN_VSTn("vst3.16 {d1[2],d2[2],d3[2]}", d1, d2, d3, d2);
    TESTINSN_VSTn("vst3.16 {d1[3],d2[3],d3[3]}", d1, d2, d3, d2);
    TESTINSN_VSTn("vst3.16 {d1[0],d3[0],d5[0]}", d1, d3, d3, d5);
    TESTINSN_VSTn("vst3.16 {d1[1],d3[1],d5[1]}", d1, d3, d3, d5);
    TESTINSN_VSTn("vst3.16 {d1[2],d3[2],d5[2]}", d1, d3, d3, d5);
    TESTINSN_VSTn("vst3.16 {d1[3],d3[3],d5[3]}", d1, d3, d3, d5);
    TESTINSN_VSTn("vst3.8 {d0[7],d1[7],d2[7]}", d0, d1, d2, d1);
    TESTINSN_VSTn("vst3.8 {d1[6],d2[6],d3[6]}", d1, d2, d3, d2);
    TESTINSN_VSTn("vst3.8 {d0[5],d1[5],d2[5]}", d0, d1, d2, d1);
    TESTINSN_VSTn("vst3.8 {d0[4],d1[4],d2[4]}", d0, d1, d2, d1);
    TESTINSN_VSTn("vst3.8 {d20[3],d21[3],d22[3]}", d20, d21, d22, d21);
    TESTINSN_VSTn("vst3.8 {d0[2],d1[2],d2[2]}", d0, d1, d2, d1);
    TESTINSN_VSTn("vst3.8 {d17[1],d18[1],d19[1]}", d17, d18, d19, d18);
    TESTINSN_VSTn("vst3.8 {d29[0],d30[0],d31[0]}", d30, d31, d29, d31);

    printf("---- VST4 (multiple 4-elements) ----\n");
    TESTINSN_VSTn("vst4.8 {d0-d3}", d0, d1, d2, d3);
    TESTINSN_VSTn("vst4.16 {d20-d23}", d20, d21, d22, d23);
    TESTINSN_VSTn("vst4.32 {d0-d3}", d0, d1, d2, d3);
    TESTINSN_VSTn("vst4.8 {d0,d2,d4,d6}", d0, d2, d4, d6);
    TESTINSN_VSTn("vst4.16 {d1,d3,d5,d7}", d1, d3, d5, d7);
    TESTINSN_VSTn("vst4.32 {d20,d22,d24,d26}", d20, d22, d24, d26);

    printf("---- VST4 (single 4-element structure from one lane) ----\n");
    TESTINSN_VSTn("vst4.32 {d0[0],d1[0],d2[0],d3[0]}", d0, d1, d2, d3);
    TESTINSN_VSTn("vst4.32 {d0[1],d1[1],d2[1],d3[1]}", d0, d1, d2, d4);
    TESTINSN_VSTn("vst4.32 {d0[0],d2[0],d4[0],d6[0]}", d0, d2, d4, d6);
    TESTINSN_VSTn("vst4.32 {d0[1],d2[1],d4[1],d6[1]}", d0, d2, d4, d6);
    TESTINSN_VSTn("vst4.16 {d1[0],d2[0],d3[0],d4[0]}", d1, d2, d3, d4);
    TESTINSN_VSTn("vst4.16 {d1[1],d2[1],d3[1],d4[1]}", d1, d2, d3, d4);
    TESTINSN_VSTn("vst4.16 {d1[2],d2[2],d3[2],d4[2]}", d1, d2, d3, d4);
    TESTINSN_VSTn("vst4.16 {d1[3],d2[3],d3[3],d4[3]}", d1, d2, d3, d4);
    TESTINSN_VSTn("vst4.16 {d1[0],d3[0],d5[0],d7[0]}", d1, d3, d5, d7);
    TESTINSN_VSTn("vst4.16 {d1[1],d3[1],d5[1],d7[1]}", d1, d3, d5, d7);
    TESTINSN_VSTn("vst4.16 {d1[2],d3[2],d5[2],d7[2]}", d1, d3, d5, d7);
    TESTINSN_VSTn("vst4.16 {d1[3],d3[3],d5[3],d7[3]}", d1, d3, d5, d7);
    TESTINSN_VSTn("vst4.8 {d0[7],d1[7],d2[7],d3[7]}", d0, d1, d2, d3);
    TESTINSN_VSTn("vst4.8 {d1[6],d2[6],d3[6],d4[6]}", d1, d2, d3, d4);
    TESTINSN_VSTn("vst4.8 {d0[5],d1[5],d2[5],d3[5]}", d0, d1, d2, d3);
    TESTINSN_VSTn("vst4.8 {d0[4],d1[4],d2[4],d3[4]}", d0, d1, d2, d3);
    TESTINSN_VSTn("vst4.8 {d20[3],d21[3],d22[3],d23[3]}", d20, d21, d22, d23);
    TESTINSN_VSTn("vst4.8 {d0[2],d1[2],d2[2],d3[2]}", d0, d1, d2, d3);
    TESTINSN_VSTn("vst4.8 {d17[1],d18[1],d19[1],d20[1]}", d17, d18, d19, d20);
    TESTINSN_VSTn("vst4.8 {d28[0],d29[0],d30[0],d31[0]}", d28, d29, d30, d31);

    printf("---- VLD1 (multiple single elements) ----\n");
    TESTINSN_VLDn_WB("vld1.8 {d0}", d0, d0, d0, d0);
    TESTINSN_VLDn_WB("vld1.16 {d0}", d0, d0, d0, d0);
    TESTINSN_VLDn_WB("vld1.32 {d0}", d0, d0, d0, d0);
    TESTINSN_VLDn_WB("vld1.64 {d0}", d0, d0, d0, d0);
    TESTINSN_VLDn_WB("vld1.8 {d9}", d9, d9, d9, d9);
    TESTINSN_VLDn_WB("vld1.16 {d17}", d17, d17, d17, d17);
    TESTINSN_VLDn_WB("vld1.32 {d31}", d31, d31, d31, d31);
    TESTINSN_VLDn_WB("vld1.64 {d14}", d14, d14, d14, d14);
    TESTINSN_VLDn_WB("vld1.8 {d0-d1}", d0, d1, d0, d1);
    TESTINSN_VLDn_WB("vld1.16 {d0-d1}", d0, d1, d0, d1);
    TESTINSN_VLDn_WB("vld1.32 {d5-d6}", d5, d6, d5, d6);
    TESTINSN_VLDn_WB("vld1.64 {d30-d31}", d30, d31, d30, d31);
    TESTINSN_VLDn_WB("vld1.8 {d0-d2}", d0, d1, d2, d0);
    TESTINSN_VLDn_WB("vld1.16 {d0-d2}", d0, d1, d2, d0);
    TESTINSN_VLDn_WB("vld1.32 {d0-d2}", d0, d1, d2, d0);
    TESTINSN_VLDn_WB("vld1.64 {d0-d2}", d0, d1, d2, d0);
    TESTINSN_VLDn_WB("vld1.8 {d0-d3}", d0, d1, d2, d3);
    TESTINSN_VLDn_WB("vld1.16 {d0-d3}", d0, d1, d2, d3);
    TESTINSN_VLDn_WB("vld1.32 {d0-d3}", d0, d1, d2, d3);
    TESTINSN_VLDn_WB("vld1.64 {d0-d3}", d0, d1, d2, d3);

    printf("---- VLD1 (single element to one lane) ----\n");
    TESTINSN_VLDn_WB("vld1.32 {d0[0]}", d0, d0, d0, d0);
    TESTINSN_VLDn_WB("vld1.32 {d0[1]}", d0, d0, d0, d0);
    TESTINSN_VLDn_WB("vld1.16 {d1[0]}", d1, d1, d1, d1);
    TESTINSN_VLDn_WB("vld1.16 {d1[1]}", d1, d1, d1, d1);
    TESTINSN_VLDn_WB("vld1.16 {d1[2]}", d1, d1, d1, d1);
    TESTINSN_VLDn_WB("vld1.16 {d1[3]}", d1, d1, d1, d1);
    TESTINSN_VLDn_WB("vld1.8 {d0[7]}", d0, d0, d0, d0);
    TESTINSN_VLDn_WB("vld1.8 {d1[6]}", d1, d1, d1, d1);
    TESTINSN_VLDn_WB("vld1.8 {d0[5]}", d0, d0, d0, d0);
    TESTINSN_VLDn_WB("vld1.8 {d0[4]}", d0, d0, d0, d0);
    TESTINSN_VLDn_WB("vld1.8 {d20[3]}", d20, d20, d20, d20);
    TESTINSN_VLDn_WB("vld1.8 {d0[2]}", d0, d0, d0, d0);
    TESTINSN_VLDn_WB("vld1.8 {d17[1]}", d17, d17, d17, d17);
    TESTINSN_VLDn_WB("vld1.8 {d30[0]}", d30, d30, d30, d30);

    printf("---- VLD1 (single element to all lanes) ----\n");
    TESTINSN_VLDn_WB("vld1.8 {d0[]}", d0, d0, d0, d0);
    TESTINSN_VLDn_WB("vld1.16 {d0[]}", d0, d0, d0, d0);
    TESTINSN_VLDn_WB("vld1.32 {d0[]}", d0, d0, d0, d0);
    TESTINSN_VLDn_WB("vld1.8 {d9[]}", d9, d9, d9, d9);
    TESTINSN_VLDn_WB("vld1.16 {d17[]}", d17, d17, d17, d17);
    TESTINSN_VLDn_WB("vld1.32 {d31[]}", d31, d31, d31, d31);
    TESTINSN_VLDn_WB("vld1.8 {d0[],d1[]}", d0, d1, d0, d1);
    TESTINSN_VLDn_WB("vld1.16 {d0[],d1[]}", d0, d1, d0, d1);
    TESTINSN_VLDn_WB("vld1.32 {d5[],d6[]}", d5, d6, d5, d6);

    printf("---- VLD2 (multiple 2-elements) ----\n");
    TESTINSN_VLDn_WB("vld2.8 {d30-d31}", d30, d31, d30, d31);
    TESTINSN_VLDn_WB("vld2.16 {d0-d1}", d0, d1, d0, d1);
    TESTINSN_VLDn_WB("vld2.32 {d0-d1}", d0, d1, d0, d1);
    TESTINSN_VLDn_WB("vld2.8 {d10,d12}", d10, d12, d10, d12);
    TESTINSN_VLDn_WB("vld2.16 {d20,d22}", d20, d22, d20, d22);
    TESTINSN_VLDn_WB("vld2.32 {d0,d2}", d0, d2, d0, d2);
    TESTINSN_VLDn_WB("vld2.8 {d0-d3}", d0, d1, d2, d3);
    TESTINSN_VLDn_WB("vld2.16 {d20-d23}", d20, d21, d22, d23);
    TESTINSN_VLDn_WB("vld2.32 {d0-d3}", d0, d1, d2, d3);

    printf("---- VLD2 (single 2-element structure to one lane) ----\n");
    TESTINSN_VLDn_WB("vld2.32 {d0[0],d1[0]}", d0, d1, d0, d1);
    TESTINSN_VLDn_WB("vld2.32 {d0[1],d1[1]}", d0, d1, d0, d1);
    TESTINSN_VLDn_WB("vld2.32 {d0[0],d2[0]}", d0, d2, d0, d2);
    TESTINSN_VLDn_WB("vld2.32 {d0[1],d2[1]}", d0, d2, d0, d2);
    TESTINSN_VLDn_WB("vld2.16 {d1[0],d2[0]}", d1, d2, d1, d2);
    TESTINSN_VLDn_WB("vld2.16 {d1[1],d2[1]}", d1, d2, d1, d2);
    TESTINSN_VLDn_WB("vld2.16 {d1[2],d2[2]}", d1, d2, d1, d2);
    TESTINSN_VLDn_WB("vld2.16 {d1[3],d2[3]}", d1, d2, d1, d2);
    TESTINSN_VLDn_WB("vld2.16 {d1[0],d3[0]}", d1, d3, d1, d3);
    TESTINSN_VLDn_WB("vld2.16 {d1[1],d3[1]}", d1, d3, d1, d3);
    TESTINSN_VLDn_WB("vld2.16 {d1[2],d3[2]}", d1, d3, d1, d3);
    TESTINSN_VLDn_WB("vld2.16 {d1[3],d3[3]}", d1, d3, d1, d3);
    TESTINSN_VLDn_WB("vld2.8 {d0[7],d1[7]}", d0, d1, d0, d1);
    TESTINSN_VLDn_WB("vld2.8 {d1[6],d2[6]}", d1, d2, d1, d2);
    TESTINSN_VLDn_WB("vld2.8 {d0[5],d1[5]}", d0, d1, d0, d1);
    TESTINSN_VLDn_WB("vld2.8 {d0[4],d1[4]}", d0, d1, d0, d1);
    TESTINSN_VLDn_WB("vld2.8 {d20[3],d21[3]}", d20, d21, d20, d21);
    TESTINSN_VLDn_WB("vld2.8 {d0[2],d1[2]}", d0, d1, d0, d1);
    TESTINSN_VLDn_WB("vld2.8 {d17[1],d18[1]}", d17, d18, d17, d18);
    TESTINSN_VLDn_WB("vld2.8 {d30[0],d31[0]}", d30, d31, d30, d31);

    printf("---- VLD2 (2-elements to all lanes) ----\n");
    TESTINSN_VLDn_WB("vld2.8 {d0[],d1[]}", d0, d1, d0, d1);
    TESTINSN_VLDn_WB("vld2.16 {d0[],d1[]}", d0, d1, d0, d1);
    TESTINSN_VLDn_WB("vld2.32 {d0[],d1[]}", d0, d1, d0, d1);
    TESTINSN_VLDn_WB("vld2.8 {d9[],d11[]}", d9, d11, d9, d11);
    TESTINSN_VLDn_WB("vld2.16 {d17[],d18[]}", d17, d18, d17, d18);
    TESTINSN_VLDn_WB("vld2.32 {d30[],d31[]}", d30, d31, d30, d31);
    TESTINSN_VLDn_WB("vld2.8 {d0[],d2[]}", d0, d2, d0, d2);
    TESTINSN_VLDn_WB("vld2.16 {d0[],d2[]}", d0, d2, d0, d2);
    TESTINSN_VLDn_WB("vld2.32 {d5[],d7[]}", d5, d7, d5, d7);

    printf("---- VLD3 (multiple 3-elements) ----\n");
    TESTINSN_VLDn_WB("vld3.8 {d20-d22}", d20, d21, d22, d20);
    TESTINSN_VLDn_WB("vld3.16 {d0-d2}", d0, d1, d2, d0);
    TESTINSN_VLDn_WB("vld3.32 {d0-d2}", d0, d1, d2, d0);
    TESTINSN_VLDn_WB("vld3.8 {d0,d2,d4}", d0, d2, d4, d0);
    TESTINSN_VLDn_WB("vld3.16 {d20,d22,d24}", d20, d22, d24, d20);
    TESTINSN_VLDn_WB("vld3.32 {d0,d2,d4}", d0, d2, d4, d0);

    printf("---- VLD3 (single 3-element structure to one lane) ----\n");
    TESTINSN_VLDn_WB("vld3.32 {d0[0],d1[0],d2[0]}", d0, d1, d2, d1);
    TESTINSN_VLDn_WB("vld3.32 {d0[1],d1[1],d2[1]}", d0, d1, d2, d1);
    TESTINSN_VLDn_WB("vld3.32 {d0[0],d2[0],d4[0]}", d0, d2, d4, d2);
    TESTINSN_VLDn_WB("vld3.32 {d0[1],d2[1],d4[1]}", d0, d2, d4, d2);
    TESTINSN_VLDn_WB("vld3.16 {d1[0],d2[0],d3[0]}", d1, d2, d3, d2);
    TESTINSN_VLDn_WB("vld3.16 {d1[1],d2[1],d3[1]}", d1, d2, d3, d2);
    TESTINSN_VLDn_WB("vld3.16 {d1[2],d2[2],d3[2]}", d1, d2, d3, d2);
    TESTINSN_VLDn_WB("vld3.16 {d1[3],d2[3],d3[3]}", d1, d2, d3, d2);
    TESTINSN_VLDn_WB("vld3.16 {d1[0],d3[0],d5[0]}", d1, d3, d3, d5);
    TESTINSN_VLDn_WB("vld3.16 {d1[1],d3[1],d5[1]}", d1, d3, d3, d5);
    TESTINSN_VLDn_WB("vld3.16 {d1[2],d3[2],d5[2]}", d1, d3, d3, d5);
    TESTINSN_VLDn_WB("vld3.16 {d1[3],d3[3],d5[3]}", d1, d3, d3, d5);
    TESTINSN_VLDn_WB("vld3.8 {d0[7],d1[7],d2[7]}", d0, d1, d2, d1);
    TESTINSN_VLDn_WB("vld3.8 {d1[6],d2[6],d3[6]}", d1, d2, d3, d2);
    TESTINSN_VLDn_WB("vld3.8 {d0[5],d1[5],d2[5]}", d0, d1, d2, d1);
    TESTINSN_VLDn_WB("vld3.8 {d0[4],d1[4],d2[4]}", d0, d1, d2, d1);
    TESTINSN_VLDn_WB("vld3.8 {d20[3],d21[3],d22[3]}", d20, d21, d22, d21);
    TESTINSN_VLDn_WB("vld3.8 {d0[2],d1[2],d2[2]}", d0, d1, d2, d1);
    TESTINSN_VLDn_WB("vld3.8 {d17[1],d18[1],d19[1]}", d17, d18, d19, d18);
    TESTINSN_VLDn_WB("vld3.8 {d29[0],d30[0],d31[0]}", d30, d31, d29, d31);

    printf("---- VLD3 (3-elements to all lanes) ----\n");
    TESTINSN_VLDn_WB("vld3.8 {d0[],d1[],d2[]}", d0, d1, d2, d1);
    TESTINSN_VLDn_WB("vld3.16 {d0[],d1[],d2[]}", d0, d1, d2, d1);
    TESTINSN_VLDn_WB("vld3.32 {d0[],d1[],d2[]}", d0, d1, d2, d1);
    TESTINSN_VLDn_WB("vld3.8 {d9[],d11[],d13[]}", d9, d11, d13, d11);
    TESTINSN_VLDn_WB("vld3.16 {d17[],d18[],d19[]}", d17, d18, d19, d18);
    TESTINSN_VLDn_WB("vld3.32 {d29[],d30[],d31[]}", d29, d30, d30, d31);
    TESTINSN_VLDn_WB("vld3.8 {d0[],d2[],d4[]}", d0, d2, d4, d2);
    TESTINSN_VLDn_WB("vld3.16 {d0[],d2[],d4[]}", d0, d2, d4, d2);
    TESTINSN_VLDn_WB("vld3.32 {d5[],d7[],d9[]}", d5, d7, d9, d7);

    printf("---- VLD4 (multiple 3-elements) ----\n");
    TESTINSN_VLDn_WB("vld4.8 {d0-d3}", d0, d1, d2, d3);
    TESTINSN_VLDn_WB("vld4.16 {d20-d23}", d20, d21, d22, d23);
    TESTINSN_VLDn_WB("vld4.32 {d0-d3}", d0, d1, d2, d3);
    TESTINSN_VLDn_WB("vld4.8 {d0,d2,d4,d6}", d0, d2, d4, d6);
    TESTINSN_VLDn_WB("vld4.16 {d1,d3,d5,d7}", d1, d3, d5, d7);
    TESTINSN_VLDn_WB("vld4.32 {d20,d22,d24,d26}", d20, d22, d24, d26);

    printf("---- VLD4 (single 4-element structure to one lane) ----\n");
    TESTINSN_VLDn_WB("vld4.32 {d0[0],d1[0],d2[0],d3[0]}", d0, d1, d2, d3);
    TESTINSN_VLDn_WB("vld4.32 {d0[1],d1[1],d2[1],d3[1]}", d0, d1, d2, d4);
    TESTINSN_VLDn_WB("vld4.32 {d0[0],d2[0],d4[0],d6[0]}", d0, d2, d4, d6);
    TESTINSN_VLDn_WB("vld4.32 {d0[1],d2[1],d4[1],d6[1]}", d0, d2, d4, d6);
    TESTINSN_VLDn_WB("vld4.16 {d1[0],d2[0],d3[0],d4[0]}", d1, d2, d3, d4);
    TESTINSN_VLDn_WB("vld4.16 {d1[1],d2[1],d3[1],d4[1]}", d1, d2, d3, d4);
    TESTINSN_VLDn_WB("vld4.16 {d1[2],d2[2],d3[2],d4[2]}", d1, d2, d3, d4);
    TESTINSN_VLDn_WB("vld4.16 {d1[3],d2[3],d3[3],d4[3]}", d1, d2, d3, d4);
    TESTINSN_VLDn_WB("vld4.16 {d1[0],d3[0],d5[0],d7[0]}", d1, d3, d5, d7);
    TESTINSN_VLDn_WB("vld4.16 {d1[1],d3[1],d5[1],d7[1]}", d1, d3, d5, d7);
    TESTINSN_VLDn_WB("vld4.16 {d1[2],d3[2],d5[2],d7[2]}", d1, d3, d5, d7);
    TESTINSN_VLDn_WB("vld4.16 {d1[3],d3[3],d5[3],d7[3]}", d1, d3, d5, d7);
    TESTINSN_VLDn_WB("vld4.8 {d0[7],d1[7],d2[7],d3[7]}", d0, d1, d2, d3);
    TESTINSN_VLDn_WB("vld4.8 {d1[6],d2[6],d3[6],d4[6]}", d1, d2, d3, d4);
    TESTINSN_VLDn_WB("vld4.8 {d0[5],d1[5],d2[5],d3[5]}", d0, d1, d2, d3);
    TESTINSN_VLDn_WB("vld4.8 {d0[4],d1[4],d2[4],d3[4]}", d0, d1, d2, d3);
    TESTINSN_VLDn_WB("vld4.8 {d20[3],d21[3],d22[3],d23[3]}", d20, d21, d22, d23);
    TESTINSN_VLDn_WB("vld4.8 {d0[2],d1[2],d2[2],d3[2]}", d0, d1, d2, d3);
    TESTINSN_VLDn_WB("vld4.8 {d17[1],d18[1],d19[1],d20[1]}", d17, d18, d19, d20);
    TESTINSN_VLDn_WB("vld4.8 {d28[0],d29[0],d30[0],d31[0]}", d28, d29, d30, d31);

    printf("---- VLD4 (4-elements to all lanes) ----\n");
    TESTINSN_VLDn_WB("vld4.8 {d0[],d1[],d2[],d3[]}", d0, d1, d2, d3);
    TESTINSN_VLDn_WB("vld4.16 {d0[],d1[],d2[],d3[]}", d0, d1, d2, d3);
    TESTINSN_VLDn_WB("vld4.32 {d0[],d1[],d2[],d3[]}", d0, d1, d2, d3);
    TESTINSN_VLDn_WB("vld4.8 {d9[],d11[],d13[],d15[]}", d9, d11, d13, d15);
    TESTINSN_VLDn_WB("vld4.16 {d17[],d18[],d19[],d20[]}", d17, d18, d19, d20);
    TESTINSN_VLDn_WB("vld4.32 {d28[],d29[],d30[],d31[]}", d28, d29, d30, d31);
    TESTINSN_VLDn_WB("vld4.8 {d0[],d2[],d4[],d6[]}", d0, d2, d4, d6);
    TESTINSN_VLDn_WB("vld4.16 {d0[],d2[],d4[],d6[]}", d0, d2, d4, d6);
    TESTINSN_VLDn_WB("vld4.32 {d5[],d7[],d9[],d11[]}", d5, d7, d9, d11);

    printf("---- VST1 (multiple single elements) ----\n");
    TESTINSN_VSTn_WB("vst1.8 {d0}", d0, d0, d0, d0);
    TESTINSN_VSTn_WB("vst1.16 {d0}", d0, d0, d0, d0);
    TESTINSN_VSTn_WB("vst1.32 {d0}", d0, d0, d0, d0);
    TESTINSN_VSTn_WB("vst1.64 {d0}", d0, d0, d0, d0);
    TESTINSN_VSTn_WB("vst1.8 {d9}", d9, d9, d9, d9);
    TESTINSN_VSTn_WB("vst1.16 {d17}", d17, d17, d17, d17);
    TESTINSN_VSTn_WB("vst1.32 {d31}", d31, d31, d31, d31);
    TESTINSN_VSTn_WB("vst1.64 {d14}", d14, d14, d14, d14);
    TESTINSN_VSTn_WB("vst1.8 {d0-d1}", d0, d1, d0, d1);
    TESTINSN_VSTn_WB("vst1.16 {d0-d1}", d0, d1, d0, d1);
    TESTINSN_VSTn_WB("vst1.32 {d5-d6}", d5, d6, d5, d6);
    TESTINSN_VSTn_WB("vst1.64 {d30-d31}", d30, d31, d30, d31);
    TESTINSN_VSTn_WB("vst1.8 {d0-d2}", d0, d1, d2, d0);
    TESTINSN_VSTn_WB("vst1.16 {d0-d2}", d0, d1, d2, d0);
    TESTINSN_VSTn_WB("vst1.32 {d0-d2}", d0, d1, d2, d0);
    TESTINSN_VSTn_WB("vst1.64 {d0-d2}", d0, d1, d2, d0);
    TESTINSN_VSTn_WB("vst1.8 {d0-d3}", d0, d1, d2, d3);
    TESTINSN_VSTn_WB("vst1.16 {d0-d3}", d0, d1, d2, d3);
    TESTINSN_VSTn_WB("vst1.32 {d0-d3}", d0, d1, d2, d3);
    TESTINSN_VSTn_WB("vst1.64 {d0-d3}", d0, d1, d2, d3);

    printf("---- VST1 (single element from one lane) ----\n");
    TESTINSN_VSTn_WB("vst1.32 {d0[0]}", d0, d0, d0, d0);
    TESTINSN_VSTn_WB("vst1.32 {d0[1]}", d0, d0, d0, d0);
    TESTINSN_VSTn_WB("vst1.16 {d1[0]}", d1, d1, d1, d1);
    TESTINSN_VSTn_WB("vst1.16 {d1[1]}", d1, d1, d1, d1);
    TESTINSN_VSTn_WB("vst1.16 {d1[2]}", d1, d1, d1, d1);
    TESTINSN_VSTn_WB("vst1.16 {d1[3]}", d1, d1, d1, d1);
    TESTINSN_VSTn_WB("vst1.8 {d0[7]}", d0, d0, d0, d0);
    TESTINSN_VSTn_WB("vst1.8 {d1[6]}", d1, d1, d1, d1);
    TESTINSN_VSTn_WB("vst1.8 {d0[5]}", d0, d0, d0, d0);
    TESTINSN_VSTn_WB("vst1.8 {d0[4]}", d0, d0, d0, d0);
    TESTINSN_VSTn_WB("vst1.8 {d20[3]}", d20, d20, d20, d20);
    TESTINSN_VSTn_WB("vst1.8 {d0[2]}", d0, d0, d0, d0);
    TESTINSN_VSTn_WB("vst1.8 {d17[1]}", d17, d17, d17, d17);
    TESTINSN_VSTn_WB("vst1.8 {d30[0]}", d30, d30, d30, d30);

    printf("---- VST2 (multiple 2-elements) ----\n");
    TESTINSN_VSTn_WB("vst2.8 {d30-d31}", d30, d31, d30, d31);
    TESTINSN_VSTn_WB("vst2.16 {d0-d1}", d0, d1, d0, d1);
    TESTINSN_VSTn_WB("vst2.32 {d0-d1}", d0, d1, d0, d1);
    TESTINSN_VSTn_WB("vst2.8 {d10,d12}", d10, d12, d10, d12);
    TESTINSN_VSTn_WB("vst2.16 {d20,d22}", d20, d22, d20, d22);
    TESTINSN_VSTn_WB("vst2.32 {d0,d2}", d0, d2, d0, d2);
    TESTINSN_VSTn_WB("vst2.8 {d0-d3}", d0, d1, d2, d3);
    TESTINSN_VSTn_WB("vst2.16 {d20-d23}", d20, d21, d22, d23);
    TESTINSN_VSTn_WB("vst2.32 {d0-d3}", d0, d1, d2, d3);

    printf("---- VST2 (single 2-element structure from one lane) ----\n");
    TESTINSN_VSTn_WB("vst2.32 {d0[0],d1[0]}", d0, d1, d0, d1);
    TESTINSN_VSTn_WB("vst2.32 {d0[1],d1[1]}", d0, d1, d0, d1);
    TESTINSN_VSTn_WB("vst2.32 {d0[0],d2[0]}", d0, d2, d0, d2);
    TESTINSN_VSTn_WB("vst2.32 {d0[1],d2[1]}", d0, d2, d0, d2);
    TESTINSN_VSTn_WB("vst2.16 {d1[0],d2[0]}", d1, d2, d1, d2);
    TESTINSN_VSTn_WB("vst2.16 {d1[1],d2[1]}", d1, d2, d1, d2);
    TESTINSN_VSTn_WB("vst2.16 {d1[2],d2[2]}", d1, d2, d1, d2);
    TESTINSN_VSTn_WB("vst2.16 {d1[3],d2[3]}", d1, d2, d1, d2);
    TESTINSN_VSTn_WB("vst2.16 {d1[0],d3[0]}", d1, d3, d1, d3);
    TESTINSN_VSTn_WB("vst2.16 {d1[1],d3[1]}", d1, d3, d1, d3);
    TESTINSN_VSTn_WB("vst2.16 {d1[2],d3[2]}", d1, d3, d1, d3);
    TESTINSN_VSTn_WB("vst2.16 {d1[3],d3[3]}", d1, d3, d1, d3);
    TESTINSN_VSTn_WB("vst2.8 {d0[7],d1[7]}", d0, d1, d0, d1);
    TESTINSN_VSTn_WB("vst2.8 {d1[6],d2[6]}", d1, d2, d1, d2);
    TESTINSN_VSTn_WB("vst2.8 {d0[5],d1[5]}", d0, d1, d0, d1);
    TESTINSN_VSTn_WB("vst2.8 {d0[4],d1[4]}", d0, d1, d0, d1);
    TESTINSN_VSTn_WB("vst2.8 {d20[3],d21[3]}", d20, d21, d20, d21);
    TESTINSN_VSTn_WB("vst2.8 {d0[2],d1[2]}", d0, d1, d0, d1);
    TESTINSN_VSTn_WB("vst2.8 {d17[1],d18[1]}", d17, d18, d17, d18);
    TESTINSN_VSTn_WB("vst2.8 {d30[0],d31[0]}", d30, d31, d30, d31);

    printf("---- VST3 (multiple 3-elements) ----\n");
    TESTINSN_VSTn_WB("vst3.8 {d20-d22}", d20, d21, d22, d20);
    TESTINSN_VSTn_WB("vst3.16 {d0-d2}", d0, d1, d2, d0);
    TESTINSN_VSTn_WB("vst3.32 {d0-d2}", d0, d1, d2, d0);
    TESTINSN_VSTn_WB("vst3.8 {d0,d2,d4}", d0, d2, d4, d0);
    TESTINSN_VSTn_WB("vst3.16 {d20,d22,d24}", d20, d22, d24, d20);
    TESTINSN_VSTn_WB("vst3.32 {d0,d2,d4}", d0, d2, d4, d0);

    printf("---- VST3 (single 3-element structure from one lane) ----\n");
    TESTINSN_VSTn_WB("vst3.32 {d0[0],d1[0],d2[0]}", d0, d1, d2, d1);
    TESTINSN_VSTn_WB("vst3.32 {d0[1],d1[1],d2[1]}", d0, d1, d2, d1);
    TESTINSN_VSTn_WB("vst3.32 {d0[0],d2[0],d4[0]}", d0, d2, d4, d2);
    TESTINSN_VSTn_WB("vst3.32 {d0[1],d2[1],d4[1]}", d0, d2, d4, d2);
    TESTINSN_VSTn_WB("vst3.16 {d1[0],d2[0],d3[0]}", d1, d2, d3, d2);
    TESTINSN_VSTn_WB("vst3.16 {d1[1],d2[1],d3[1]}", d1, d2, d3, d2);
    TESTINSN_VSTn_WB("vst3.16 {d1[2],d2[2],d3[2]}", d1, d2, d3, d2);
    TESTINSN_VSTn_WB("vst3.16 {d1[3],d2[3],d3[3]}", d1, d2, d3, d2);
    TESTINSN_VSTn_WB("vst3.16 {d1[0],d3[0],d5[0]}", d1, d3, d3, d5);
    TESTINSN_VSTn_WB("vst3.16 {d1[1],d3[1],d5[1]}", d1, d3, d3, d5);
    TESTINSN_VSTn_WB("vst3.16 {d1[2],d3[2],d5[2]}", d1, d3, d3, d5);
    TESTINSN_VSTn_WB("vst3.16 {d1[3],d3[3],d5[3]}", d1, d3, d3, d5);
    TESTINSN_VSTn_WB("vst3.8 {d0[7],d1[7],d2[7]}", d0, d1, d2, d1);
    TESTINSN_VSTn_WB("vst3.8 {d1[6],d2[6],d3[6]}", d1, d2, d3, d2);
    TESTINSN_VSTn_WB("vst3.8 {d0[5],d1[5],d2[5]}", d0, d1, d2, d1);
    TESTINSN_VSTn_WB("vst3.8 {d0[4],d1[4],d2[4]}", d0, d1, d2, d1);
    TESTINSN_VSTn_WB("vst3.8 {d20[3],d21[3],d22[3]}", d20, d21, d22, d21);
    TESTINSN_VSTn_WB("vst3.8 {d0[2],d1[2],d2[2]}", d0, d1, d2, d1);
    TESTINSN_VSTn_WB("vst3.8 {d17[1],d18[1],d19[1]}", d17, d18, d19, d18);
    TESTINSN_VSTn_WB("vst3.8 {d29[0],d30[0],d31[0]}", d30, d31, d29, d31);

    printf("---- VST4 (multiple 4-elements) ----\n");
    TESTINSN_VSTn_WB("vst4.8 {d0-d3}", d0, d1, d2, d3);
    TESTINSN_VSTn_WB("vst4.16 {d20-d23}", d20, d21, d22, d23);
    TESTINSN_VSTn_WB("vst4.32 {d0-d3}", d0, d1, d2, d3);
    TESTINSN_VSTn_WB("vst4.8 {d0,d2,d4,d6}", d0, d2, d4, d6);
    TESTINSN_VSTn_WB("vst4.16 {d1,d3,d5,d7}", d1, d3, d5, d7);
    TESTINSN_VSTn_WB("vst4.32 {d20,d22,d24,d26}", d20, d22, d24, d26);

    printf("---- VST4 (single 4-element structure from one lane) ----\n");
    TESTINSN_VSTn_WB("vst4.32 {d0[0],d1[0],d2[0],d3[0]}", d0, d1, d2, d3);
    TESTINSN_VSTn_WB("vst4.32 {d0[1],d1[1],d2[1],d3[1]}", d0, d1, d2, d4);
    TESTINSN_VSTn_WB("vst4.32 {d0[0],d2[0],d4[0],d6[0]}", d0, d2, d4, d6);
    TESTINSN_VSTn_WB("vst4.32 {d0[1],d2[1],d4[1],d6[1]}", d0, d2, d4, d6);
    TESTINSN_VSTn_WB("vst4.16 {d1[0],d2[0],d3[0],d4[0]}", d1, d2, d3, d4);
    TESTINSN_VSTn_WB("vst4.16 {d1[1],d2[1],d3[1],d4[1]}", d1, d2, d3, d4);
    TESTINSN_VSTn_WB("vst4.16 {d1[2],d2[2],d3[2],d4[2]}", d1, d2, d3, d4);
    TESTINSN_VSTn_WB("vst4.16 {d1[3],d2[3],d3[3],d4[3]}", d1, d2, d3, d4);
    TESTINSN_VSTn_WB("vst4.16 {d1[0],d3[0],d5[0],d7[0]}", d1, d3, d5, d7);
    TESTINSN_VSTn_WB("vst4.16 {d1[1],d3[1],d5[1],d7[1]}", d1, d3, d5, d7);
    TESTINSN_VSTn_WB("vst4.16 {d1[2],d3[2],d5[2],d7[2]}", d1, d3, d5, d7);
    TESTINSN_VSTn_WB("vst4.16 {d1[3],d3[3],d5[3],d7[3]}", d1, d3, d5, d7);
    TESTINSN_VSTn_WB("vst4.8 {d0[7],d1[7],d2[7],d3[7]}", d0, d1, d2, d3);
    TESTINSN_VSTn_WB("vst4.8 {d1[6],d2[6],d3[6],d4[6]}", d1, d2, d3, d4);
    TESTINSN_VSTn_WB("vst4.8 {d0[5],d1[5],d2[5],d3[5]}", d0, d1, d2, d3);
    TESTINSN_VSTn_WB("vst4.8 {d0[4],d1[4],d2[4],d3[4]}", d0, d1, d2, d3);
    TESTINSN_VSTn_WB("vst4.8 {d20[3],d21[3],d22[3],d23[3]}", d20, d21, d22, d23);
    TESTINSN_VSTn_WB("vst4.8 {d0[2],d1[2],d2[2],d3[2]}", d0, d1, d2, d3);
    TESTINSN_VSTn_WB("vst4.8 {d17[1],d18[1],d19[1],d20[1]}", d17, d18, d19, d20);
    TESTINSN_VSTn_WB("vst4.8 {d28[0],d29[0],d30[0],d31[0]}", d28, d29, d30, d31);

    printf("---- VLD1 (multiple single elements) ----\n");
    TESTINSN_VLDn_RI("vld1.8 {d0}", d0, d0, d0, d0, r5, 13);
    TESTINSN_VLDn_RI("vld1.16 {d0}", d0, d0, d0, d0, r8, 13);
    TESTINSN_VLDn_RI("vld1.32 {d0}", d0, d0, d0, d0, r5, 42);
    TESTINSN_VLDn_RI("vld1.64 {d0}", d0, d0, d0, d0, r5, 0);
    TESTINSN_VLDn_RI("vld1.8 {d9}", d9, d9, d9, d9, r5, 13);
    TESTINSN_VLDn_RI("vld1.16 {d17}", d17, d17, d17, d17, r6, 13);
    TESTINSN_VLDn_RI("vld1.32 {d31}", d31, d31, d31, d31, r5, -3);
    TESTINSN_VLDn_RI("vld1.64 {d14}", d14, d14, d14, d14, r5, 13);
    TESTINSN_VLDn_RI("vld1.8 {d0-d1}", d0, d1, d0, d1, r5, 13);
    TESTINSN_VLDn_RI("vld1.16 {d0-d1}", d0, d1, d0, d1, r5, 13);
    TESTINSN_VLDn_RI("vld1.32 {d5-d6}", d5, d6, d5, d6, r5, 13);
    TESTINSN_VLDn_RI("vld1.64 {d30-d31}", d30, d31, d30, d31, r5, 13);
    TESTINSN_VLDn_RI("vld1.8 {d0-d2}", d0, d1, d2, d0, r5, 13);
    TESTINSN_VLDn_RI("vld1.16 {d0-d2}", d0, d1, d2, d0, r5, 13);
    TESTINSN_VLDn_RI("vld1.32 {d0-d2}", d0, d1, d2, d0, r5, 13);
    TESTINSN_VLDn_RI("vld1.64 {d0-d2}", d0, d1, d2, d0, r5, 13);
    TESTINSN_VLDn_RI("vld1.8 {d0-d3}", d0, d1, d2, d3, r5, 13);
    TESTINSN_VLDn_RI("vld1.16 {d0-d3}", d0, d1, d2, d3, r5, 13);
    TESTINSN_VLDn_RI("vld1.32 {d0-d3}", d0, d1, d2, d3, r5, 13);
    TESTINSN_VLDn_RI("vld1.64 {d0-d3}", d0, d1, d2, d3, r5, 13);

    printf("---- VLD1 (single element to one lane) ----\n");
    TESTINSN_VLDn_RI("vld1.32 {d0[0]}", d0, d0, d0, d0, r5, 13);
    TESTINSN_VLDn_RI("vld1.32 {d0[1]}", d0, d0, d0, d0, r9, 42);
    TESTINSN_VLDn_RI("vld1.16 {d1[0]}", d1, d1, d1, d1, r5, 13);
    TESTINSN_VLDn_RI("vld1.16 {d1[1]}", d1, d1, d1, d1, r1, 0);
    TESTINSN_VLDn_RI("vld1.16 {d1[2]}", d1, d1, d1, d1, r5, -3);
    TESTINSN_VLDn_RI("vld1.16 {d1[3]}", d1, d1, d1, d1, r5, 13);
    TESTINSN_VLDn_RI("vld1.8 {d0[7]}", d0, d0, d0, d0, r5, 13);
    TESTINSN_VLDn_RI("vld1.8 {d1[6]}", d1, d1, d1, d1, r5, 13);
    TESTINSN_VLDn_RI("vld1.8 {d0[5]}", d0, d0, d0, d0, r5, 13);
    TESTINSN_VLDn_RI("vld1.8 {d0[4]}", d0, d0, d0, d0, r5, 13);
    TESTINSN_VLDn_RI("vld1.8 {d20[3]}", d20, d20, d20, d20, r5, 13);
    TESTINSN_VLDn_RI("vld1.8 {d0[2]}", d0, d0, d0, d0, r5, 13);
    TESTINSN_VLDn_RI("vld1.8 {d17[1]}", d17, d17, d17, d17, r5, 13);
    TESTINSN_VLDn_RI("vld1.8 {d30[0]}", d30, d30, d30, d30, r5, 13);

    printf("---- VLD1 (single element to all lanes) ----\n");
    TESTINSN_VLDn_RI("vld1.8 {d0[]}", d0, d0, d0, d0, r5, 13);
    TESTINSN_VLDn_RI("vld1.16 {d0[]}", d0, d0, d0, d0, r9, 42);
    TESTINSN_VLDn_RI("vld1.32 {d0[]}", d0, d0, d0, d0, r1, 0);
    TESTINSN_VLDn_RI("vld1.8 {d9[]}", d9, d9, d9, d9, r5, -3);
    TESTINSN_VLDn_RI("vld1.16 {d17[]}", d17, d17, d17, d17, r5, 13);
    TESTINSN_VLDn_RI("vld1.32 {d31[]}", d31, d31, d31, d31, r5, 13);
    TESTINSN_VLDn_RI("vld1.8 {d0[],d1[]}", d0, d1, d0, d1, r5, 13);
    TESTINSN_VLDn_RI("vld1.16 {d0[],d1[]}", d0, d1, d0, d1, r5, 13);
    TESTINSN_VLDn_RI("vld1.32 {d5[],d6[]}", d5, d6, d5, d6, r5, 13);

    printf("---- VLD2 (multiple 2-elements) ----\n");
    TESTINSN_VLDn_RI("vld2.8 {d30-d31}", d30, d31, d30, d31, r5, 13);
    TESTINSN_VLDn_RI("vld2.16 {d0-d1}", d0, d1, d0, d1, r9, 42);
    TESTINSN_VLDn_RI("vld2.32 {d0-d1}", d0, d1, d0, d1, r1, 0);
    TESTINSN_VLDn_RI("vld2.8 {d10,d12}", d10, d12, d10, d12, r5, -3);
    TESTINSN_VLDn_RI("vld2.16 {d20,d22}", d20, d22, d20, d22, r5, 13);
    TESTINSN_VLDn_RI("vld2.32 {d0,d2}", d0, d2, d0, d2, r5, 13);
    TESTINSN_VLDn_RI("vld2.8 {d0-d3}", d0, d1, d2, d3, r5, 13);
    TESTINSN_VLDn_RI("vld2.16 {d20-d23}", d20, d21, d22, d23, r5, 13);
    TESTINSN_VLDn_RI("vld2.32 {d0-d3}", d0, d1, d2, d3, r5, 13);

    printf("---- VLD2 (single 2-element structure to one lane) ----\n");
    TESTINSN_VLDn_RI("vld2.32 {d0[0],d1[0]}", d0, d1, d0, d1, r5, 13);
    TESTINSN_VLDn_RI("vld2.32 {d0[1],d1[1]}", d0, d1, d0, d1, r9, 42);
    TESTINSN_VLDn_RI("vld2.32 {d0[0],d2[0]}", d0, d2, d0, d2, r1, 0);
    TESTINSN_VLDn_RI("vld2.32 {d0[1],d2[1]}", d0, d2, d0, d2, r5, -3);
    TESTINSN_VLDn_RI("vld2.16 {d1[0],d2[0]}", d1, d2, d1, d2, r5, 13);
    TESTINSN_VLDn_RI("vld2.16 {d1[1],d2[1]}", d1, d2, d1, d2, r5, 13);
    TESTINSN_VLDn_RI("vld2.16 {d1[2],d2[2]}", d1, d2, d1, d2, r5, 13);
    TESTINSN_VLDn_RI("vld2.16 {d1[3],d2[3]}", d1, d2, d1, d2, r5, 13);
    TESTINSN_VLDn_RI("vld2.16 {d1[0],d3[0]}", d1, d3, d1, d3, r5, 13);
    TESTINSN_VLDn_RI("vld2.16 {d1[1],d3[1]}", d1, d3, d1, d3, r5, 13);
    TESTINSN_VLDn_RI("vld2.16 {d1[2],d3[2]}", d1, d3, d1, d3, r5, 13);
    TESTINSN_VLDn_RI("vld2.16 {d1[3],d3[3]}", d1, d3, d1, d3, r5, 13);
    TESTINSN_VLDn_RI("vld2.8 {d0[7],d1[7]}", d0, d1, d0, d1, r5, 13);
    TESTINSN_VLDn_RI("vld2.8 {d1[6],d2[6]}", d1, d2, d1, d2, r5, 13);
    TESTINSN_VLDn_RI("vld2.8 {d0[5],d1[5]}", d0, d1, d0, d1, r5, 13);
    TESTINSN_VLDn_RI("vld2.8 {d0[4],d1[4]}", d0, d1, d0, d1, r5, 13);
    TESTINSN_VLDn_RI("vld2.8 {d20[3],d21[3]}", d20, d21, d20, d21, r5, 13);
    TESTINSN_VLDn_RI("vld2.8 {d0[2],d1[2]}", d0, d1, d0, d1, r5, 13);
    TESTINSN_VLDn_RI("vld2.8 {d17[1],d18[1]}", d17, d18, d17, d18, r5, 13);
    TESTINSN_VLDn_RI("vld2.8 {d30[0],d31[0]}", d30, d31, d30, d31, r5, 13);

    printf("---- VLD2 (2-elements to all lanes) ----\n");
    TESTINSN_VLDn_RI("vld2.8 {d0[],d1[]}", d0, d1, d0, d1, r5, 13);
    TESTINSN_VLDn_RI("vld2.16 {d0[],d1[]}", d0, d1, d0, d1, r9, 42);
    TESTINSN_VLDn_RI("vld2.32 {d0[],d1[]}", d0, d1, d0, d1, r1, 0);
    TESTINSN_VLDn_RI("vld2.8 {d9[],d11[]}", d9, d11, d9, d11, r5, -3);
    TESTINSN_VLDn_RI("vld2.16 {d17[],d18[]}", d17, d18, d17, d18, r5, 13);
    TESTINSN_VLDn_RI("vld2.32 {d30[],d31[]}", d30, d31, d30, d31, r5, 13);
    TESTINSN_VLDn_RI("vld2.8 {d0[],d2[]}", d0, d2, d0, d2, r5, 13);
    TESTINSN_VLDn_RI("vld2.16 {d0[],d2[]}", d0, d2, d0, d2, r5, 13);
    TESTINSN_VLDn_RI("vld2.32 {d5[],d7[]}", d5, d7, d5, d7, r5, 13);

    printf("---- VLD3 (multiple 3-elements) ----\n");
    TESTINSN_VLDn_RI("vld3.8 {d20-d22}", d20, d21, d22, d20, r5, 13);
    TESTINSN_VLDn_RI("vld3.16 {d0-d2}", d0, d1, d2, d0, r9, 42);
    TESTINSN_VLDn_RI("vld3.32 {d0-d2}", d0, d1, d2, d0, r1, 0);
    TESTINSN_VLDn_RI("vld3.8 {d0,d2,d4}", d0, d2, d4, d0, r5, -3);
    TESTINSN_VLDn_RI("vld3.16 {d20,d22,d24}", d20, d22, d24, d20, r5, 13);
    TESTINSN_VLDn_RI("vld3.32 {d0,d2,d4}", d0, d2, d4, d0, r5, 13);

    printf("---- VLD3 (single 3-element structure to one lane) ----\n");
    TESTINSN_VLDn_RI("vld3.32 {d0[0],d1[0],d2[0]}", d0, d1, d2, d1, r5, 13);
    TESTINSN_VLDn_RI("vld3.32 {d0[1],d1[1],d2[1]}", d0, d1, d2, d1, r9, 42);
    TESTINSN_VLDn_RI("vld3.32 {d0[0],d2[0],d4[0]}", d0, d2, d4, d2, r1, 0);
    TESTINSN_VLDn_RI("vld3.32 {d0[1],d2[1],d4[1]}", d0, d2, d4, d2, r5, -3);
    TESTINSN_VLDn_RI("vld3.16 {d1[0],d2[0],d3[0]}", d1, d2, d3, d2, r5, 13);
    TESTINSN_VLDn_RI("vld3.16 {d1[1],d2[1],d3[1]}", d1, d2, d3, d2, r5, 13);
    TESTINSN_VLDn_RI("vld3.16 {d1[2],d2[2],d3[2]}", d1, d2, d3, d2, r5, 13);
    TESTINSN_VLDn_RI("vld3.16 {d1[3],d2[3],d3[3]}", d1, d2, d3, d2, r5, 13);
    TESTINSN_VLDn_RI("vld3.16 {d1[0],d3[0],d5[0]}", d1, d3, d3, d5, r5, 13);
    TESTINSN_VLDn_RI("vld3.16 {d1[1],d3[1],d5[1]}", d1, d3, d3, d5, r5, 13);
    TESTINSN_VLDn_RI("vld3.16 {d1[2],d3[2],d5[2]}", d1, d3, d3, d5, r5, 13);
    TESTINSN_VLDn_RI("vld3.16 {d1[3],d3[3],d5[3]}", d1, d3, d3, d5, r5, 13);
    TESTINSN_VLDn_RI("vld3.8 {d0[7],d1[7],d2[7]}", d0, d1, d2, d1, r5, 13);
    TESTINSN_VLDn_RI("vld3.8 {d1[6],d2[6],d3[6]}", d1, d2, d3, d2, r5, 13);
    TESTINSN_VLDn_RI("vld3.8 {d0[5],d1[5],d2[5]}", d0, d1, d2, d1, r5, 13);
    TESTINSN_VLDn_RI("vld3.8 {d0[4],d1[4],d2[4]}", d0, d1, d2, d1, r5, 13);
    TESTINSN_VLDn_RI("vld3.8 {d20[3],d21[3],d22[3]}", d20, d21, d22, d21, r5, 13);
    TESTINSN_VLDn_RI("vld3.8 {d0[2],d1[2],d2[2]}", d0, d1, d2, d1, r5, 13);
    TESTINSN_VLDn_RI("vld3.8 {d17[1],d18[1],d19[1]}", d17, d18, d19, d18, r5, 13);
    TESTINSN_VLDn_RI("vld3.8 {d29[0],d30[0],d31[0]}", d30, d31, d29, d31, r5, 13);

    printf("---- VLD3 (3-elements to all lanes) ----\n");
    TESTINSN_VLDn_RI("vld3.8 {d0[],d1[],d2[]}", d0, d1, d2, d1, r5, 13);
    TESTINSN_VLDn_RI("vld3.16 {d0[],d1[],d2[]}", d0, d1, d2, d1, r9, 42);
    TESTINSN_VLDn_RI("vld3.32 {d0[],d1[],d2[]}", d0, d1, d2, d1, r1, 0);
    TESTINSN_VLDn_RI("vld3.8 {d9[],d11[],d13[]}", d9, d11, d13, d11, r5, -3);
    TESTINSN_VLDn_RI("vld3.16 {d17[],d18[],d19[]}", d17, d18, d19, d18, r5, 13);
    TESTINSN_VLDn_RI("vld3.32 {d29[],d30[],d31[]}", d29, d30, d30, d31, r5, 13);
    TESTINSN_VLDn_RI("vld3.8 {d0[],d2[],d4[]}", d0, d2, d4, d2, r5, 13);
    TESTINSN_VLDn_RI("vld3.16 {d0[],d2[],d4[]}", d0, d2, d4, d2, r5, 13);
    TESTINSN_VLDn_RI("vld3.32 {d5[],d7[],d9[]}", d5, d7, d9, d7, r5, 13);

    printf("---- VLD4 (multiple 3-elements) ----\n");
    TESTINSN_VLDn_RI("vld4.8 {d0-d3}", d0, d1, d2, d3, r5, 13);
    TESTINSN_VLDn_RI("vld4.16 {d20-d23}", d20, d21, d22, d23, r9, 0);
    TESTINSN_VLDn_RI("vld4.32 {d0-d3}", d0, d1, d2, d3, r0, 42);
    TESTINSN_VLDn_RI("vld4.8 {d0,d2,d4,d6}", d0, d2, d4, d6, r5, -3);
    TESTINSN_VLDn_RI("vld4.16 {d1,d3,d5,d7}", d1, d3, d5, d7, r5, 13);
    TESTINSN_VLDn_RI("vld4.32 {d20,d22,d24,d26}", d20, d22, d24, d26, r5, 13);

    printf("---- VLD4 (single 4-element structure to one lane) ----\n");
    TESTINSN_VLDn_RI("vld4.32 {d0[0],d1[0],d2[0],d3[0]}", d0, d1, d2, d3, r5, 13);
    TESTINSN_VLDn_RI("vld4.32 {d0[1],d1[1],d2[1],d3[1]}", d0, d1, d2, d4, r9, 42);
    TESTINSN_VLDn_RI("vld4.32 {d0[0],d2[0],d4[0],d6[0]}", d0, d2, d4, d6, r1, 0);
    TESTINSN_VLDn_RI("vld4.32 {d0[1],d2[1],d4[1],d6[1]}", d0, d2, d4, d6, r5, -3);
    TESTINSN_VLDn_RI("vld4.16 {d1[0],d2[0],d3[0],d4[0]}", d1, d2, d3, d4, r5, 13);
    TESTINSN_VLDn_RI("vld4.16 {d1[1],d2[1],d3[1],d4[1]}", d1, d2, d3, d4, r5, 13);
    TESTINSN_VLDn_RI("vld4.16 {d1[2],d2[2],d3[2],d4[2]}", d1, d2, d3, d4, r5, 13);
    TESTINSN_VLDn_RI("vld4.16 {d1[3],d2[3],d3[3],d4[3]}", d1, d2, d3, d4, r5, 13);
    TESTINSN_VLDn_RI("vld4.16 {d1[0],d3[0],d5[0],d7[0]}", d1, d3, d5, d7, r5, 13);
    TESTINSN_VLDn_RI("vld4.16 {d1[1],d3[1],d5[1],d7[1]}", d1, d3, d5, d7, r5, 13);
    TESTINSN_VLDn_RI("vld4.16 {d1[2],d3[2],d5[2],d7[2]}", d1, d3, d5, d7, r5, 13);
    TESTINSN_VLDn_RI("vld4.16 {d1[3],d3[3],d5[3],d7[3]}", d1, d3, d5, d7, r5, 13);
    TESTINSN_VLDn_RI("vld4.8 {d0[7],d1[7],d2[7],d3[7]}", d0, d1, d2, d3, r5, 13);
    TESTINSN_VLDn_RI("vld4.8 {d1[6],d2[6],d3[6],d4[6]}", d1, d2, d3, d4, r5, 13);
    TESTINSN_VLDn_RI("vld4.8 {d0[5],d1[5],d2[5],d3[5]}", d0, d1, d2, d3, r5, 13);
    TESTINSN_VLDn_RI("vld4.8 {d0[4],d1[4],d2[4],d3[4]}", d0, d1, d2, d3, r5, 13);
    TESTINSN_VLDn_RI("vld4.8 {d20[3],d21[3],d22[3],d23[3]}", d20, d21, d22, d23, r5, 13);
    TESTINSN_VLDn_RI("vld4.8 {d0[2],d1[2],d2[2],d3[2]}", d0, d1, d2, d3, r5, 13);
    TESTINSN_VLDn_RI("vld4.8 {d17[1],d18[1],d19[1],d20[1]}", d17, d18, d19, d20, r5, 13);
    TESTINSN_VLDn_RI("vld4.8 {d28[0],d29[0],d30[0],d31[0]}", d28, d29, d30, d31, r5, 13);

    printf("---- VLD4 (4-elements to all lanes) ----\n");
    TESTINSN_VLDn_RI("vld4.8 {d0[],d1[],d2[],d3[]}", d0, d1, d2, d3, r5, 13);
    TESTINSN_VLDn_RI("vld4.16 {d0[],d1[],d2[],d3[]}", d0, d1, d2, d3, r9, 42);
    TESTINSN_VLDn_RI("vld4.32 {d0[],d1[],d2[],d3[]}", d0, d1, d2, d3, r1, 0);
    TESTINSN_VLDn_RI("vld4.8 {d9[],d11[],d13[],d15[]}", d9, d11, d13, d15, r5, -3);
    TESTINSN_VLDn_RI("vld4.16 {d17[],d18[],d19[],d20[]}", d17, d18, d19, d20, r5, 13);
    TESTINSN_VLDn_RI("vld4.32 {d28[],d29[],d30[],d31[]}", d28, d29, d30, d31, r5, 13);
    TESTINSN_VLDn_RI("vld4.8 {d0[],d2[],d4[],d6[]}", d0, d2, d4, d6, r5, 13);
    TESTINSN_VLDn_RI("vld4.16 {d0[],d2[],d4[],d6[]}", d0, d2, d4, d6, r5, 13);
    TESTINSN_VLDn_RI("vld4.32 {d5[],d7[],d9[],d11[]}", d5, d7, d9, d11, r5, 13);

    printf("---- VST1 (multiple single elements) ----\n");
    TESTINSN_VSTn_RI("vst1.8 {d0}", d0, d0, d0, d0, r5, 13);
    TESTINSN_VSTn_RI("vst1.16 {d0}", d0, d0, d0, d0, r9, 42);
    TESTINSN_VSTn_RI("vst1.32 {d0}", d0, d0, d0, d0, r5, 0);
    TESTINSN_VSTn_RI("vst1.64 {d0}", d0, d0, d0, d0, r5, -3);
    TESTINSN_VSTn_RI("vst1.8 {d9}", d9, d9, d9, d9, r5, 13);
    TESTINSN_VSTn_RI("vst1.16 {d17}", d17, d17, d17, d17, r5, 13);
    TESTINSN_VSTn_RI("vst1.32 {d31}", d31, d31, d31, d31, r5, 13);
    TESTINSN_VSTn_RI("vst1.64 {d14}", d14, d14, d14, d14, r5, 13);
    TESTINSN_VSTn_RI("vst1.8 {d0-d1}", d0, d1, d0, d1, r5, 13);
    TESTINSN_VSTn_RI("vst1.16 {d0-d1}", d0, d1, d0, d1, r5, 13);
    TESTINSN_VSTn_RI("vst1.32 {d5-d6}", d5, d6, d5, d6, r5, 13);
    TESTINSN_VSTn_RI("vst1.64 {d30-d31}", d30, d31, d30, d31, r5, 13);
    TESTINSN_VSTn_RI("vst1.8 {d0-d2}", d0, d1, d2, d0, r5, 13);
    TESTINSN_VSTn_RI("vst1.16 {d0-d2}", d0, d1, d2, d0, r5, 13);
    TESTINSN_VSTn_RI("vst1.32 {d0-d2}", d0, d1, d2, d0, r5, 13);
    TESTINSN_VSTn_RI("vst1.64 {d0-d2}", d0, d1, d2, d0, r5, 13);
    TESTINSN_VSTn_RI("vst1.8 {d0-d3}", d0, d1, d2, d3, r5, 13);
    TESTINSN_VSTn_RI("vst1.16 {d0-d3}", d0, d1, d2, d3, r5, 13);
    TESTINSN_VSTn_RI("vst1.32 {d0-d3}", d0, d1, d2, d3, r5, 13);
    TESTINSN_VSTn_RI("vst1.64 {d0-d3}", d0, d1, d2, d3, r5, 13);

    printf("---- VST1 (single element from one lane) ----\n");
    TESTINSN_VSTn_RI("vst1.32 {d0[0]}", d0, d0, d0, d0, r5, 13);
    TESTINSN_VSTn_RI("vst1.32 {d0[1]}", d0, d0, d0, d0, r9, 42);
    TESTINSN_VSTn_RI("vst1.16 {d1[0]}", d1, d1, d1, d1, r1, 0);
    TESTINSN_VSTn_RI("vst1.16 {d1[1]}", d1, d1, d1, d1, r5, -3);
    TESTINSN_VSTn_RI("vst1.16 {d1[2]}", d1, d1, d1, d1, r5, 13);
    TESTINSN_VSTn_RI("vst1.16 {d1[3]}", d1, d1, d1, d1, r5, 13);
    TESTINSN_VSTn_RI("vst1.8 {d0[7]}", d0, d0, d0, d0, r5, 13);
    TESTINSN_VSTn_RI("vst1.8 {d1[6]}", d1, d1, d1, d1, r5, 13);
    TESTINSN_VSTn_RI("vst1.8 {d0[5]}", d0, d0, d0, d0, r5, 13);
    TESTINSN_VSTn_RI("vst1.8 {d0[4]}", d0, d0, d0, d0, r5, 13);
    TESTINSN_VSTn_RI("vst1.8 {d20[3]}", d20, d20, d20, d20, r5, 13);
    TESTINSN_VSTn_RI("vst1.8 {d0[2]}", d0, d0, d0, d0, r5, 13);
    TESTINSN_VSTn_RI("vst1.8 {d17[1]}", d17, d17, d17, d17, r5, 13);
    TESTINSN_VSTn_RI("vst1.8 {d30[0]}", d30, d30, d30, d30, r5, 13);

    printf("---- VST2 (multiple 2-elements) ----\n");
    TESTINSN_VSTn_RI("vst2.8 {d30-d31}", d30, d31, d30, d31, r5, 13);
    TESTINSN_VSTn_RI("vst2.16 {d0-d1}", d0, d1, d0, d1, r9, 42);
    TESTINSN_VSTn_RI("vst2.32 {d0-d1}", d0, d1, d0, d1, r1, 0);
    TESTINSN_VSTn_RI("vst2.8 {d10,d12}", d10, d12, d10, d12, r5, -3);
    TESTINSN_VSTn_RI("vst2.16 {d20,d22}", d20, d22, d20, d22, r5, 13);
    TESTINSN_VSTn_RI("vst2.32 {d0,d2}", d0, d2, d0, d2, r5, 13);
    TESTINSN_VSTn_RI("vst2.8 {d0-d3}", d0, d1, d2, d3, r5, 13);
    TESTINSN_VSTn_RI("vst2.16 {d20-d23}", d20, d21, d22, d23, r5, 13);
    TESTINSN_VSTn_RI("vst2.32 {d0-d3}", d0, d1, d2, d3, r5, 13);

    printf("---- VST2 (single 2-element structure from one lane) ----\n");
    TESTINSN_VSTn_RI("vst2.32 {d0[0],d1[0]}", d0, d1, d0, d1, r5, 13);
    TESTINSN_VSTn_RI("vst2.32 {d0[1],d1[1]}", d0, d1, d0, d1, r9, 42);
    TESTINSN_VSTn_RI("vst2.32 {d0[0],d2[0]}", d0, d2, d0, d2, r1, 0);
    TESTINSN_VSTn_RI("vst2.32 {d0[1],d2[1]}", d0, d2, d0, d2, r5, -3);
    TESTINSN_VSTn_RI("vst2.16 {d1[0],d2[0]}", d1, d2, d1, d2, r5, 13);
    TESTINSN_VSTn_RI("vst2.16 {d1[1],d2[1]}", d1, d2, d1, d2, r5, 13);
    TESTINSN_VSTn_RI("vst2.16 {d1[2],d2[2]}", d1, d2, d1, d2, r5, 13);
    TESTINSN_VSTn_RI("vst2.16 {d1[3],d2[3]}", d1, d2, d1, d2, r5, 13);
    TESTINSN_VSTn_RI("vst2.16 {d1[0],d3[0]}", d1, d3, d1, d3, r5, 13);
    TESTINSN_VSTn_RI("vst2.16 {d1[1],d3[1]}", d1, d3, d1, d3, r5, 13);
    TESTINSN_VSTn_RI("vst2.16 {d1[2],d3[2]}", d1, d3, d1, d3, r5, 13);
    TESTINSN_VSTn_RI("vst2.16 {d1[3],d3[3]}", d1, d3, d1, d3, r5, 13);
    TESTINSN_VSTn_RI("vst2.8 {d0[7],d1[7]}", d0, d1, d0, d1, r5, 13);
    TESTINSN_VSTn_RI("vst2.8 {d1[6],d2[6]}", d1, d2, d1, d2, r5, 13);
    TESTINSN_VSTn_RI("vst2.8 {d0[5],d1[5]}", d0, d1, d0, d1, r5, 13);
    TESTINSN_VSTn_RI("vst2.8 {d0[4],d1[4]}", d0, d1, d0, d1, r5, 13);
    TESTINSN_VSTn_RI("vst2.8 {d20[3],d21[3]}", d20, d21, d20, d21, r5, 13);
    TESTINSN_VSTn_RI("vst2.8 {d0[2],d1[2]}", d0, d1, d0, d1, r5, 13);
    TESTINSN_VSTn_RI("vst2.8 {d17[1],d18[1]}", d17, d18, d17, d18, r5, 13);
    TESTINSN_VSTn_RI("vst2.8 {d30[0],d31[0]}", d30, d31, d30, d31, r5, 13);

    printf("---- VST3 (multiple 3-elements) ----\n");
    TESTINSN_VSTn_RI("vst3.8 {d20-d22}", d20, d21, d22, d20, r5, 13);
    TESTINSN_VSTn_RI("vst3.16 {d0-d2}", d0, d1, d2, d0, r9, 42);
    TESTINSN_VSTn_RI("vst3.32 {d0-d2}", d0, d1, d2, d0, r1, 0);
    TESTINSN_VSTn_RI("vst3.8 {d0,d2,d4}", d0, d2, d4, d0, r5, -3);
    TESTINSN_VSTn_RI("vst3.16 {d20,d22,d24}", d20, d22, d24, d20, r5, 13);
    TESTINSN_VSTn_RI("vst3.32 {d0,d2,d4}", d0, d2, d4, d0, r5, 13);

    printf("---- VST3 (single 3-element structure from one lane) ----\n");
    TESTINSN_VSTn_RI("vst3.32 {d0[0],d1[0],d2[0]}", d0, d1, d2, d1, r5, 13);
    TESTINSN_VSTn_RI("vst3.32 {d0[1],d1[1],d2[1]}", d0, d1, d2, d1, r9, 42);
    TESTINSN_VSTn_RI("vst3.32 {d0[0],d2[0],d4[0]}", d0, d2, d4, d2, r1, 0);
    TESTINSN_VSTn_RI("vst3.32 {d0[1],d2[1],d4[1]}", d0, d2, d4, d2, r5, -3);
    TESTINSN_VSTn_RI("vst3.16 {d1[0],d2[0],d3[0]}", d1, d2, d3, d2, r5, 13);
    TESTINSN_VSTn_RI("vst3.16 {d1[1],d2[1],d3[1]}", d1, d2, d3, d2, r5, 13);
    TESTINSN_VSTn_RI("vst3.16 {d1[2],d2[2],d3[2]}", d1, d2, d3, d2, r5, 13);
    TESTINSN_VSTn_RI("vst3.16 {d1[3],d2[3],d3[3]}", d1, d2, d3, d2, r5, 13);
    TESTINSN_VSTn_RI("vst3.16 {d1[0],d3[0],d5[0]}", d1, d3, d3, d5, r5, 13);
    TESTINSN_VSTn_RI("vst3.16 {d1[1],d3[1],d5[1]}", d1, d3, d3, d5, r5, 13);
    TESTINSN_VSTn_RI("vst3.16 {d1[2],d3[2],d5[2]}", d1, d3, d3, d5, r5, 13);
    TESTINSN_VSTn_RI("vst3.16 {d1[3],d3[3],d5[3]}", d1, d3, d3, d5, r5, 13);
    TESTINSN_VSTn_RI("vst3.8 {d0[7],d1[7],d2[7]}", d0, d1, d2, d1, r5, 13);
    TESTINSN_VSTn_RI("vst3.8 {d1[6],d2[6],d3[6]}", d1, d2, d3, d2, r5, 13);
    TESTINSN_VSTn_RI("vst3.8 {d0[5],d1[5],d2[5]}", d0, d1, d2, d1, r5, 13);
    TESTINSN_VSTn_RI("vst3.8 {d0[4],d1[4],d2[4]}", d0, d1, d2, d1, r5, 13);
    TESTINSN_VSTn_RI("vst3.8 {d20[3],d21[3],d22[3]}", d20, d21, d22, d21, r5, 13);
    TESTINSN_VSTn_RI("vst3.8 {d0[2],d1[2],d2[2]}", d0, d1, d2, d1, r5, 13);
    TESTINSN_VSTn_RI("vst3.8 {d17[1],d18[1],d19[1]}", d17, d18, d19, d18, r5, 13);
    TESTINSN_VSTn_RI("vst3.8 {d29[0],d30[0],d31[0]}", d30, d31, d29, d31, r5, 13);

    printf("---- VST4 (multiple 4-elements) ----\n");
    TESTINSN_VSTn_RI("vst4.8 {d0-d3}", d0, d1, d2, d3, r5, 13);
    TESTINSN_VSTn_RI("vst4.16 {d20-d23}", d20, d21, d22, d23, r9, 42);
    TESTINSN_VSTn_RI("vst4.32 {d0-d3}", d0, d1, d2, d3, r1, 0);
    TESTINSN_VSTn_RI("vst4.8 {d0,d2,d4,d6}", d0, d2, d4, d6, r5, -3);
    TESTINSN_VSTn_RI("vst4.16 {d1,d3,d5,d7}", d1, d3, d5, d7, r5, 13);
    TESTINSN_VSTn_RI("vst4.32 {d20,d22,d24,d26}", d20, d22, d24, d26, r5, 13);

    printf("---- VST4 (single 4-element structure from one lane) ----\n");
    TESTINSN_VSTn_RI("vst4.32 {d0[0],d1[0],d2[0],d3[0]}", d0, d1, d2, d3, r5, 13);
    TESTINSN_VSTn_RI("vst4.32 {d0[1],d1[1],d2[1],d3[1]}", d0, d1, d2, d4, r9, 42);
    TESTINSN_VSTn_RI("vst4.32 {d0[0],d2[0],d4[0],d6[0]}", d0, d2, d4, d6, r1, 0);
    TESTINSN_VSTn_RI("vst4.32 {d0[1],d2[1],d4[1],d6[1]}", d0, d2, d4, d6, r5, -3);
    TESTINSN_VSTn_RI("vst4.16 {d1[0],d2[0],d3[0],d4[0]}", d1, d2, d3, d4, r5, 13);
    TESTINSN_VSTn_RI("vst4.16 {d1[1],d2[1],d3[1],d4[1]}", d1, d2, d3, d4, r5, 13);
    TESTINSN_VSTn_RI("vst4.16 {d1[2],d2[2],d3[2],d4[2]}", d1, d2, d3, d4, r5, 13);
    TESTINSN_VSTn_RI("vst4.16 {d1[3],d2[3],d3[3],d4[3]}", d1, d2, d3, d4, r5, 13);
    TESTINSN_VSTn_RI("vst4.16 {d1[0],d3[0],d5[0],d7[0]}", d1, d3, d5, d7, r5, 13);
    TESTINSN_VSTn_RI("vst4.16 {d1[1],d3[1],d5[1],d7[1]}", d1, d3, d5, d7, r5, 13);
    TESTINSN_VSTn_RI("vst4.16 {d1[2],d3[2],d5[2],d7[2]}", d1, d3, d5, d7, r5, 13);
    TESTINSN_VSTn_RI("vst4.16 {d1[3],d3[3],d5[3],d7[3]}", d1, d3, d5, d7, r5, 13);
    TESTINSN_VSTn_RI("vst4.8 {d0[7],d1[7],d2[7],d3[7]}", d0, d1, d2, d3, r5, 13);
    TESTINSN_VSTn_RI("vst4.8 {d1[6],d2[6],d3[6],d4[6]}", d1, d2, d3, d4, r5, 13);
    TESTINSN_VSTn_RI("vst4.8 {d0[5],d1[5],d2[5],d3[5]}", d0, d1, d2, d3, r5, 13);
    TESTINSN_VSTn_RI("vst4.8 {d0[4],d1[4],d2[4],d3[4]}", d0, d1, d2, d3, r5, 13);
    TESTINSN_VSTn_RI("vst4.8 {d20[3],d21[3],d22[3],d23[3]}", d20, d21, d22, d23, r5, 13);
    TESTINSN_VSTn_RI("vst4.8 {d0[2],d1[2],d2[2],d3[2]}", d0, d1, d2, d3, r5, 13);
    TESTINSN_VSTn_RI("vst4.8 {d17[1],d18[1],d19[1],d20[1]}", d17, d18, d19, d20, r5, 13);
    TESTINSN_VSTn_RI("vst4.8 {d28[0],d29[0],d30[0],d31[0]}", d28, d29, d30, d31, r5, 13);

    printf("---- VMOVN ----\n");
    TESTINSN_bin("vmovn.i32 d0, q0", d0, d0, i32, 0x32, d1, i32, 0x24);
    TESTINSN_bin("vmovn.i16 d7, q5", d7, d10, i32, 0x32, d11, i32, 0x24);
    TESTINSN_bin("vmovn.i64 d31, q0", d31, d0, i32, 0x32, d1, i32, 0x24);
    TESTINSN_bin("vmovn.i32 d0, q0", d0, d0, i8, 0xff, d1, i8, 0xf0);
    TESTINSN_bin("vmovn.i16 d7, q5", d7, d10, i16, 0xdead, d11, i16, 0xbeef);
    TESTINSN_bin("vmovn.i64 d31, q0", d31, d0, i32, 0xff00fe0f, d1, i8, 0x24);

    printf("---- VQMOVN ----\n");
    TESTINSN_bin_q("vqmovn.u32 d0, q0", d0, d0, i32, 0x32, d1, i32, 0x24);
    TESTINSN_bin_q("vqmovn.u16 d7, q5", d7, d10, i32, 0x32, d11, i32, 0x24);
    TESTINSN_bin_q("vqmovn.u64 d31, q0", d31, d0, i32, 0x32, d1, i32, 0x24);
    TESTINSN_bin_q("vqmovn.u32 d0, q0", d0, d0, i8, 0xff, d1, i8, 0xf0);
    TESTINSN_bin_q("vqmovn.u16 d7, q5", d7, d10, i16, 0xdead, d11, i16, 0xbeef);
    TESTINSN_bin_q("vqmovn.u64 d31, q0", d31, d0, i32, 0xff00fe0f, d1, i8, 0x24);
    TESTINSN_bin_q("vqmovn.s32 d0, q0", d0, d0, i32, 0x32, d1, i32, 0x24);
    TESTINSN_bin_q("vqmovn.s16 d7, q5", d7, d10, i32, 0x32, d11, i32, 0x24);
    TESTINSN_bin_q("vqmovn.s64 d31, q0", d31, d0, i32, 0x32, d1, i32, 0x24);
    TESTINSN_bin_q("vqmovn.s32 d0, q0", d0, d0, i8, 0xff, d1, i8, 0xf0);
    TESTINSN_bin_q("vqmovn.s16 d7, q5", d7, d10, i16, 0xdead, d11, i16, 0xbeef);
    TESTINSN_bin_q("vqmovn.s64 d31, q0", d31, d0, i32, 0xff00fe0f, d1, i8, 0x24);
    TESTINSN_bin_q("vqmovn.s32 d0, q0", d0, d0, i8, 0xff, d1, i8, 0xff);
    TESTINSN_bin_q("vqmovn.s16 d7, q5", d7, d10, i8, 0xff, d11, i16, 0xff);
    TESTINSN_bin_q("vqmovn.s64 d31, q0", d31, d0, i8, 0xff, d1, i8, 0xff);

    printf("---- VQMOVN ----\n");
    TESTINSN_bin_q("vqmovun.s32 d0, q0", d0, d0, i32, 0x32, d1, i32, 0x24);
    TESTINSN_bin_q("vqmovun.s16 d7, q5", d7, d10, i32, 0x32, d11, i32, 0x24);
    TESTINSN_bin_q("vqmovun.s64 d31, q0", d31, d0, i32, 0x32, d1, i32, 0x24);
    TESTINSN_bin_q("vqmovun.s32 d0, q0", d0, d0, i8, 0xff, d1, i8, 0xf0);
    TESTINSN_bin_q("vqmovun.s16 d7, q5", d7, d10, i16, 0xdead, d11, i16, 0xbeef);
    TESTINSN_bin_q("vqmovun.s64 d31, q0", d31, d0, i32, 0xff00fe0f, d1, i8, 0x24);
    TESTINSN_bin_q("vqmovun.s32 d0, q0", d0, d0, i8, 0xff, d1, i8, 0xff);
    TESTINSN_bin_q("vqmovun.s16 d7, q5", d7, d10, i8, 0xff, d11, i16, 0xff);
    TESTINSN_bin_q("vqmovun.s64 d31, q0", d31, d0, i8, 0xff, d1, i8, 0xff);

    printf("---- VABS ----\n");
    TESTINSN_un("vabs.s32 d0, d1", d0, d1, i32, 0x73);
    TESTINSN_un("vabs.s16 d15, d4", d15, d4, i32, 0x73);
    TESTINSN_un("vabs.s8 d8, d7", d8, d7, i32, 0x73);
    TESTINSN_un("vabs.s32 d0, d1", d0, d1, i32, 0xfe);
    TESTINSN_un("vabs.s16 d31, d4", d31, d4, i32, 0xef);
    TESTINSN_un("vabs.s8 d8, d7", d8, d7, i32, 0xde);
    TESTINSN_un("vabs.s32 d0, d1", d0, d1, i16, 0xfe0a);
    TESTINSN_un("vabs.s16 d15, d4", d15, d4, i16, 0xef0b);
    TESTINSN_un("vabs.s8 d8, d7", d8, d7, i16, 0xde0c);

    printf("---- VQABS ----\n");
    TESTINSN_un_q("vqabs.s32 d0, d1", d0, d1, i32, 0x73);
    TESTINSN_un_q("vqabs.s32 d0, d1", d0, d1, i32, 1 << 31);
    TESTINSN_un_q("vqabs.s16 d0, d1", d0, d1, i32, 1 << 31);
    TESTINSN_un_q("vqabs.s8 d0, d1", d0, d1, i32, 1 << 31);
    TESTINSN_un_q("vqabs.s16 d15, d4", d15, d4, i32, 0x73);
    TESTINSN_un_q("vqabs.s8 d8, d7", d8, d7, i32, 0x73);
    TESTINSN_un_q("vqabs.s32 d0, d1", d0, d1, i32, 0xfe);
    TESTINSN_un_q("vqabs.s16 d31, d4", d31, d4, i32, 0xef);
    TESTINSN_un_q("vqabs.s8 d8, d7", d8, d7, i32, 0xde);
    TESTINSN_un_q("vqabs.s32 d0, d1", d0, d1, i16, 0xfe0a);
    TESTINSN_un_q("vqabs.s16 d15, d4", d15, d4, i16, 0xef0b);
    TESTINSN_un_q("vqabs.s8 d8, d7", d8, d7, i16, 0xde0c);

    printf("---- VADDHN ----\n");
    TESTINSN_bin("vaddhn.i32 d0, q1, q1", d0, q1, i32, 0x73, q1, i32, 0x72);
    TESTINSN_bin("vaddhn.i16 d0, q1, q2", d0, q1, i32, 0x73, q2, i32, 0x72);
    TESTINSN_bin("vaddhn.i32 d0, q1, q2", d0, q1, i32, 0x73, q2, i32, 0x72);
    TESTINSN_bin("vaddhn.i64 d0, q1, q2", d0, q1, i32, 0x73, q2, i32, 0x72);
    TESTINSN_bin("vaddhn.i16 d0, q15, q2", d0, q15, i16, 0xef73, q2, i32, 0x0172);
    TESTINSN_bin("vaddhn.i32 d31, q1, q2", d31, q1, i16, 0xef73, q2, i32, 0x0172);
    TESTINSN_bin("vaddhn.i64 d0, q1, q8", d0, q1, i16, 0xef73, q8, i32, 0x0172);
    TESTINSN_bin("vaddhn.i32 d0, q1, q1", d0, q1, i8, 0x73, q1, i32, 0x72);
    TESTINSN_bin("vaddhn.i16 d0, q1, q2", d0, q1, i8, 0x73, q2, i32, 0x72);
    TESTINSN_bin("vaddhn.i32 d0, q1, q2", d0, q1, i8, 0x73, q2, i32, 0x72);
    TESTINSN_bin("vaddhn.i64 d0, q1, q2", d0, q1, i8, 0x73, q2, i32, 0x72);

    printf("---- VRADDHN ----\n");
    TESTINSN_bin("vraddhn.i32 d0, q1, q1", d0, q1, i32, 0x73, q1, i32, 0x72);
    TESTINSN_bin("vraddhn.i16 d0, q1, q2", d0, q1, i32, 0x73, q2, i32, 0x72);
    TESTINSN_bin("vraddhn.i32 d0, q1, q2", d0, q1, i32, 0x73, q2, i32, 0x72);
    TESTINSN_bin("vraddhn.i64 d0, q1, q2", d0, q1, i32, 0x73, q2, i32, 0x72);
    TESTINSN_bin("vraddhn.i16 d0, q15, q2", d0, q15, i16, 0xef73, q2, i32, 0x0172);
    TESTINSN_bin("vraddhn.i32 d31, q1, q2", d31, q1, i16, 0xef73, q2, i32, 0x0172);
    TESTINSN_bin("vraddhn.i64 d0, q1, q8", d0, q1, i16, 0xef73, q8, i32, 0x0172);
    TESTINSN_bin("vraddhn.i32 d0, q1, q1", d0, q1, i8, 0x73, q1, i32, 0x72);
    TESTINSN_bin("vraddhn.i16 d0, q1, q2", d0, q1, i8, 0x73, q2, i32, 0x72);
    TESTINSN_bin("vraddhn.i32 d0, q1, q2", d0, q1, i8, 0x73, q2, i32, 0x72);
    TESTINSN_bin("vraddhn.i64 d0, q1, q2", d0, q1, i8, 0x73, q2, i32, 0x72);
    TESTINSN_bin("vraddhn.i16 d0, q15, q2", d0, q15, i16, 0xef73, q2, i32, 0x0102);
    TESTINSN_bin("vraddhn.i32 d31, q1, q2", d31, q1, i16, 0xef73, q2, i32, 0x0102);
    TESTINSN_bin("vraddhn.i64 d0, q1, q8", d0, q1, i16, 0xef73, q8, i32, 0x0102);
    TESTINSN_bin("vraddhn.i32 d0, q1, q1", d0, q1, i8, 0x73, q1, i32, 0x02);
    TESTINSN_bin("vraddhn.i16 d0, q1, q2", d0, q1, i8, 0x73, q2, i32, 0x02);
    TESTINSN_bin("vraddhn.i32 d0, q1, q2", d0, q1, i8, 0x73, q2, i32, 0x02);
    TESTINSN_bin("vraddhn.i64 d0, q1, q2", d0, q1, i8, 0x73, q2, i32, 0x02);

    printf("---- VSUBHN ----\n");
    TESTINSN_bin("vsubhn.i32 d0, q1, q1", d0, q1, i32, 0x73, q1, i32, 0x72);
    TESTINSN_bin("vsubhn.i16 d0, q1, q2", d0, q1, i32, 0x73, q2, i32, 0x72);
    TESTINSN_bin("vsubhn.i32 d0, q1, q2", d0, q1, i32, 0x73, q2, i32, 0x72);
    TESTINSN_bin("vsubhn.i64 d0, q1, q2", d0, q1, i32, 0x73, q2, i32, 0x72);
    TESTINSN_bin("vsubhn.i16 d0, q15, q2", d0, q15, i16, 0xef73, q2, i32, 0x0172);
    TESTINSN_bin("vsubhn.i32 d31, q1, q2", d31, q1, i16, 0xef73, q2, i32, 0x0172);
    TESTINSN_bin("vsubhn.i64 d0, q1, q8", d0, q1, i16, 0xef73, q8, i32, 0x0172);
    TESTINSN_bin("vsubhn.i32 d0, q1, q1", d0, q1, i8, 0x73, q1, i32, 0x72);
    TESTINSN_bin("vsubhn.i16 d0, q1, q2", d0, q1, i8, 0x73, q2, i32, 0x72);
    TESTINSN_bin("vsubhn.i32 d0, q1, q2", d0, q1, i8, 0x73, q2, i32, 0x72);
    TESTINSN_bin("vsubhn.i64 d0, q1, q2", d0, q1, i8, 0x73, q2, i32, 0x72);

    printf("---- VRSUBHN ----\n");
    TESTINSN_bin("vrsubhn.i32 d0, q1, q1", d0, q1, i32, 0x73, q1, i32, 0x72);
    TESTINSN_bin("vrsubhn.i16 d0, q1, q2", d0, q1, i32, 0x73, q2, i32, 0x72);
    TESTINSN_bin("vrsubhn.i32 d0, q1, q2", d0, q1, i32, 0x73, q2, i32, 0x72);
    TESTINSN_bin("vrsubhn.i64 d0, q1, q2", d0, q1, i32, 0x73, q2, i32, 0x72);
    TESTINSN_bin("vrsubhn.i16 d0, q15, q2", d0, q15, i16, 0xef73, q2, i32, 0x0172);
    TESTINSN_bin("vrsubhn.i32 d31, q1, q2", d31, q1, i16, 0xef73, q2, i32, 0x0172);
    TESTINSN_bin("vrsubhn.i64 d0, q1, q8", d0, q1, i16, 0xef73, q8, i32, 0x0172);
    TESTINSN_bin("vrsubhn.i32 d0, q1, q1", d0, q1, i8, 0x73, q1, i32, 0x72);
    TESTINSN_bin("vrsubhn.i16 d0, q1, q2", d0, q1, i8, 0x73, q2, i32, 0x72);
    TESTINSN_bin("vrsubhn.i32 d0, q1, q2", d0, q1, i8, 0x73, q2, i32, 0x72);
    TESTINSN_bin("vrsubhn.i64 d0, q1, q2", d0, q1, i8, 0x73, q2, i32, 0x72);
    TESTINSN_bin("vrsubhn.i16 d0, q15, q2", d0, q15, i16, 0xef93, q2, i32, 0x0102);
    TESTINSN_bin("vrsubhn.i32 d31, q1, q2", d31, q1, i16, 0xef93, q2, i32, 0x0102);
    TESTINSN_bin("vrsubhn.i64 d0, q1, q8", d0, q1, i16, 0xef93, q8, i32, 0x0102);
    TESTINSN_bin("vrsubhn.i32 d0, q1, q1", d0, q1, i8, 0x93, q1, i32, 0x02);
    TESTINSN_bin("vrsubhn.i16 d0, q1, q2", d0, q1, i8, 0x93, q2, i32, 0x02);
    TESTINSN_bin("vrsubhn.i32 d0, q1, q2", d0, q1, i8, 0x93, q2, i32, 0x02);
    TESTINSN_bin("vrsubhn.i64 d0, q1, q2", d0, q1, i8, 0x93, q2, i32, 0x02);

    printf("---- VCEQ #0 ----\n");
    TESTINSN_un("vceq.i32 d0, d1, #0", d0, d1, i32, 0x21);
    TESTINSN_un("vceq.i16 d2, d1, #0", d2, d1, i32, 0x21);
    TESTINSN_un("vceq.i8 d10, d11, #0", d10, d11, i32, 0x21);
    TESTINSN_un("vceq.i32 d0, d1, #0", d0, d1, i32, 0x0);
    TESTINSN_un("vceq.i16 d2, d1, #0", d2, d1, i32, 0x0);
    TESTINSN_un("vceq.i8 d10, d31, #0", d10, d31, i32, 0x0);

    printf("---- VCGT #0 ----\n");
    TESTINSN_un("vcgt.s32 d0, d1, #0", d0, d1, i32, 0x21);
    TESTINSN_un("vcgt.s16 d2, d1, #0", d2, d1, i32, 0x21);
    TESTINSN_un("vcgt.s8 d10, d31, #0", d10, d31, i32, 0x21);
    TESTINSN_un("vcgt.s32 d0, d1, #0", d0, d1, i32, 0x0);
    TESTINSN_un("vcgt.s16 d2, d1, #0", d2, d1, i32, 0x0);
    TESTINSN_un("vcgt.s8 d10, d11, #0", d10, d11, i32, 0x0);
    TESTINSN_un("vcgt.s32 d0, d1, #0", d0, d1, i8, 0xef);
    TESTINSN_un("vcgt.s16 d2, d1, #0", d2, d1, i8, 0xed);
    TESTINSN_un("vcgt.s8 d10, d11, #0", d10, d11, i8, 0xae);

    printf("---- VCGE #0 ----\n");
    TESTINSN_un("vcge.s32 d0, d1, #0", d0, d1, i32, 0x21);
    TESTINSN_un("vcge.s16 d2, d1, #0", d2, d1, i32, 0x21);
    TESTINSN_un("vcge.s8 d10, d11, #0", d10, d11, i32, 0x21);
    TESTINSN_un("vcge.s32 d0, d1, #0", d0, d1, i32, 0x0);
    TESTINSN_un("vcge.s16 d2, d1, #0", d2, d1, i32, 0x0);
    TESTINSN_un("vcge.s8 d10, d31, #0", d10, d31, i32, 0x0);
    TESTINSN_un("vcge.s32 d0, d1, #0", d0, d1, i8, 0xef);
    TESTINSN_un("vcge.s16 d2, d1, #0", d2, d1, i8, 0xed);
    TESTINSN_un("vcge.s8 d10, d11, #0", d10, d11, i8, 0xae);
    TESTINSN_un("vcge.s32 d0, d1, #0", d0, d1, i32, 0xef);
    TESTINSN_un("vcge.s16 d2, d1, #0", d2, d1, i32, 0xed);
    TESTINSN_un("vcge.s8 d10, d11, #0", d10, d11, i32, 0xae);

    printf("---- VCLE #0 ----\n");
    TESTINSN_un("vcle.s32 d0, d1, #0", d0, d1, i32, 0x21);
    TESTINSN_un("vcle.s16 d2, d1, #0", d2, d1, i32, 0x21);
    TESTINSN_un("vcle.s8 d10, d11, #0", d10, d11, i32, 0x21);
    TESTINSN_un("vcle.s32 d0, d1, #0", d0, d1, i32, 0x0);
    TESTINSN_un("vcle.s16 d2, d1, #0", d2, d1, i32, 0x0);
    TESTINSN_un("vcle.s8 d10, d31, #0", d10, d31, i32, 0x0);
    TESTINSN_un("vcle.s32 d0, d1, #0", d0, d1, i8, 0xef);
    TESTINSN_un("vcle.s16 d2, d1, #0", d2, d1, i8, 0xed);
    TESTINSN_un("vcle.s8 d10, d11, #0", d10, d11, i8, 0xae);

    printf("---- VCLT #0 ----\n");
    TESTINSN_un("vclt.s32 d0, d1, #0", d0, d1, i32, 0x21);
    TESTINSN_un("vclt.s16 d2, d1, #0", d2, d1, i32, 0x21);
    TESTINSN_un("vclt.s8 d10, d11, #0", d10, d11, i32, 0x21);
    TESTINSN_un("vclt.s32 d0, d1, #0", d0, d1, i32, 0x0);
    TESTINSN_un("vclt.s16 d2, d1, #0", d2, d1, i32, 0x0);
    TESTINSN_un("vclt.s8 d10, d11, #0", d10, d11, i32, 0x0);
    TESTINSN_un("vclt.s32 d0, d1, #0", d0, d1, i8, 0xef);
    TESTINSN_un("vclt.s16 d2, d1, #0", d2, d1, i8, 0xed);
    TESTINSN_un("vclt.s8 d10, d31, #0", d10, d31, i8, 0xae);
    TESTINSN_un("vclt.s32 d0, d1, #0", d0, d1, i32, 0xef);
    TESTINSN_un("vclt.s16 d2, d1, #0", d2, d1, i32, 0xed);
    TESTINSN_un("vclt.s8 d10, d11, #0", d10, d11, i32, 0xae);

    printf("---- VCNT ----\n");
    TESTINSN_un("vcnt.8 d0, d1", d0, d1, i32, 0xac3d25eb);
    TESTINSN_un("vcnt.8 d11, d14", d11, d14, i32, 0xac3d25eb);
    TESTINSN_un("vcnt.8 d6, d2", d6, d2, i32, 0xad0eb);

    printf("---- VCLS ----\n");
    TESTINSN_un("vcls.s8 d0, d1", d0, d1, i32, 0x21);
    TESTINSN_un("vcls.s8 d30, d31", d30, d31, i8, 0x82);
    TESTINSN_un("vcls.s16 d0, d1", d0, d1, i32, 0x21);
    TESTINSN_un("vcls.s16 d31, d30", d31, d30, i8, 0x82);
    TESTINSN_un("vcls.s32 d6, d1", d6, d1, i32, 0x21);
    TESTINSN_un("vcls.s32 d30, d5", d30, d5, i8, 0x82);
    TESTINSN_un("vcls.s8 d2, d4", d2, d4, i8, 0xff);
    TESTINSN_un("vcls.s16 d2, d4", d2, d4, i8, 0xff);
    TESTINSN_un("vcls.s32 d2, d4", d2, d4, i8, 0xff);
    TESTINSN_un("vcls.s8 d2, d4", d2, d4, i16, 0xffef);
    TESTINSN_un("vcls.s16 d2, d4", d2, d4, i16, 0xffef);
    TESTINSN_un("vcls.s32 d2, d4", d2, d4, i16, 0xffef);
    TESTINSN_un("vcls.s8 d2, d4", d2, d4, i8, 0x00);
    TESTINSN_un("vcls.s16 d2, d4", d2, d4, i8, 0x00);
    TESTINSN_un("vcls.s32 d2, d4", d2, d4, i8, 0x00);
    TESTINSN_un("vcls.s8 d2, d4", d2, d4, i16, 0x00ef);
    TESTINSN_un("vcls.s16 d2, d4", d2, d4, i16, 0x00ef);
    TESTINSN_un("vcls.s32 d2, d4", d2, d4, i16, 0x00ef);

    printf("---- VCLZ ----\n");
    TESTINSN_un("vclz.i8 d0, d1", d0, d1, i32, 0x21);
    TESTINSN_un("vclz.i8 d30, d31", d30, d31, i8, 0x82);
    TESTINSN_un("vclz.i16 d0, d1", d0, d1, i32, 0x21);
    TESTINSN_un("vclz.i16 d31, d30", d31, d30, i8, 0x82);
    TESTINSN_un("vclz.i32 d6, d1", d6, d1, i32, 0x21);
    TESTINSN_un("vclz.i32 d30, d5", d30, d5, i8, 0x82);
    TESTINSN_un("vclz.i8 d2, d4", d2, d4, i8, 0xff);
    TESTINSN_un("vclz.i16 d2, d4", d2, d4, i8, 0xff);
    TESTINSN_un("vclz.i32 d2, d4", d2, d4, i8, 0xff);
    TESTINSN_un("vclz.i8 d2, d4", d2, d4, i16, 0xffef);
    TESTINSN_un("vclz.i16 d2, d4", d2, d4, i16, 0xffef);
    TESTINSN_un("vclz.i32 d2, d4", d2, d4, i16, 0xffef);
    TESTINSN_un("vclz.i8 d2, d4", d2, d4, i8, 0x00);
    TESTINSN_un("vclz.i16 d2, d4", d2, d4, i8, 0x00);
    TESTINSN_un("vclz.i32 d2, d4", d2, d4, i8, 0x00);
    TESTINSN_un("vclz.i8 d2, d4", d2, d4, i16, 0x00ef);
    TESTINSN_un("vclz.i16 d2, d4", d2, d4, i16, 0x00ef);
    TESTINSN_un("vclz.i32 d2, d4", d2, d4, i16, 0x00ef);

    printf("---- VSLI ----\n");
    TESTINSN_un("vsli.16 d0, d1, #1", d0, d1, i32, 7);
    TESTINSN_un("vsli.16 d3, d4, #2", d3, d4, i32, -0x7c);
    TESTINSN_un("vsli.32 d2, d5, #31", d2, d5, i32, -1);
    TESTINSN_un("vsli.8 d6, d7, #7", d6, d7, i32, 0xffff);
    TESTINSN_un("vsli.16 d8, d9, #12", d8, d9, i32, -10);
    TESTINSN_un("vsli.32 d10, d11, #5", d10, d11, i32, 10234);
    TESTINSN_un("vsli.8 d12, d13, #1", d12, d13, i32, -1);
    TESTINSN_un("vsli.16 d14, d15, #11", d14, d15, i32, -1);
    TESTINSN_un("vsli.32 d10, d11, #9", d10, d11, i32, 1000);
    TESTINSN_un("vsli.8 d7, d13, #7", d7, d13, i32, -1);
    TESTINSN_un("vsli.16 d8, d1, #1", d8, d1, i32, 0xabcf);
    TESTINSN_un("vsli.32 d12, d3, #15", d12, d3, i32, -0x1b0);
    TESTINSN_un("vsli.64 d0, d1, #42", d0, d1, i32, -1);
    TESTINSN_un("vsli.64 d6, d7, #12", d6, d7, i32, 0xfac);
    TESTINSN_un("vsli.64 d8, d4, #9", d8, d4, i32, 13560);
    TESTINSN_un("vsli.64 d9, d12, #11", d9, d12, i32, 98710);

    printf("---- VPADD ----\n");
    TESTINSN_bin("vpadd.i32 d0, d1, d2", d0, d1, i32, 24, d2, i32, 120);
    TESTINSN_bin("vpadd.i32 d0, d1, d2", d0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin("vpadd.i16 d0, d1, d2", d0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin("vpadd.i8 d0, d1, d2", d0, d1, i32, 140, d2, i32, 120);
    TESTINSN_bin("vpadd.i8 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vpadd.i16 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vpadd.i32 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vpadd.i32 d10, d11, d12", d10, d11, i32, 24, d12, i32, 120);

    printf("---- VPADDL ----\n");
    TESTINSN_un("vpaddl.u32 d0, d1", d0, d1, i32, 24);
    TESTINSN_un("vpaddl.u32 d0, d1", d0, d1, i32, 140);
    TESTINSN_un("vpaddl.u16 d0, d1", d0, d1, i32, 140);
    TESTINSN_un("vpaddl.u8 d0, d1", d0, d1, i32, 140);
    TESTINSN_un("vpaddl.u8 d0, d1", d0, d1, i32, (1 << 31) + 1);
    TESTINSN_un("vpaddl.u16 d0, d1", d0, d1, i32, (1 << 31) + 1);
    TESTINSN_un("vpaddl.u32 d0, d1", d0, d1, i32, (1 << 31) + 1);
    TESTINSN_un("vpaddl.u32 d10, d11", d10, d11, i32, 24);
    TESTINSN_un("vpaddl.s32 d0, d1", d0, d1, i32, 24);
    TESTINSN_un("vpaddl.s32 d0, d1", d0, d1, i32, 140);
    TESTINSN_un("vpaddl.s16 d0, d1", d0, d1, i32, 140);
    TESTINSN_un("vpaddl.s8 d0, d1", d0, d1, i32, 140);
    TESTINSN_un("vpaddl.s8 d0, d1", d0, d1, i32, (1 << 31) + 1);
    TESTINSN_un("vpaddl.s16 d0, d1", d0, d1, i32, (1 << 31) + 1);
    TESTINSN_un("vpaddl.s32 d0, d1", d0, d1, i32, (1 << 31) + 1);
    TESTINSN_un("vpaddl.s32 d10, d11", d10, d11, i32, 24);

    printf("---- VPADAL ----\n");
    TESTINSN_un("vpadal.u32 d0, d1", d0, d1, i32, 24);
    TESTINSN_un("vpadal.u32 d0, d1", d0, d1, i32, 140);
    TESTINSN_un("vpadal.u16 d0, d1", d0, d1, i32, 140);
    TESTINSN_un("vpadal.u8 d0, d1", d0, d1, i8, 140);
    TESTINSN_un("vpadal.u8 d0, d1", d0, d1, i32, (1 << 31) + 1);
    TESTINSN_un("vpadal.u16 d0, d1", d0, d1, i32, (1 << 31) + 1);
    TESTINSN_un("vpadal.u32 d0, d1", d0, d1, i32, (1 << 31) + 1);
    TESTINSN_un("vpadal.u32 d10, d11", d10, d11, i32, 24);
    TESTINSN_un("vpadal.s32 d0, d1", d0, d1, i32, 24);
    TESTINSN_un("vpadal.s32 d0, d1", d0, d1, i32, 140);
    TESTINSN_un("vpadal.s16 d0, d1", d0, d1, i32, 140);
    TESTINSN_un("vpadal.s8 d0, d1", d0, d1, i8, 140);
    TESTINSN_un("vpadal.s8 d0, d1", d0, d1, i32, (1 << 31) + 1);
    TESTINSN_un("vpadal.s16 d0, d1", d0, d1, i32, (1 << 31) + 1);
    TESTINSN_un("vpadal.s32 d0, d1", d0, d1, i32, (1 << 31) + 1);
    TESTINSN_un("vpadal.s32 d10, d11", d10, d11, i32, 24);

    printf("---- VZIP ----\n");
    TESTINSN_dual("vzip.32 d0, d1", d0, i8, 0x12, d1, i8, 0x34);
    TESTINSN_dual("vzip.16 d1, d0", d0, i8, 0x12, d1, i8, 0x34);
    TESTINSN_dual("vzip.8 d10, d11", d10, i8, 0x12, d11, i8, 0x34);
    TESTINSN_dual("vzip.32 d0, d1", d0, i32, 0x12345678, d1, i32, 0x0a0b0c0d);
    TESTINSN_dual("vzip.16 d1, d0", d0, i32, 0x12345678, d1, i32, 0x0a0b0c0d);
    TESTINSN_dual("vzip.8 d30, d31", d30, i32, 0x12345678, d31, i32, 0x0a0b0c0d);

    printf("---- VUZP ----\n");
    TESTINSN_dual("vuzp.32 d0, d1", d0, i8, 0x12, d1, i8, 0x34);
    TESTINSN_dual("vuzp.16 d1, d0", d0, i8, 0x12, d1, i8, 0x34);
    TESTINSN_dual("vuzp.8 d10, d11", d10, i8, 0x12, d11, i8, 0x34);
    TESTINSN_dual("vuzp.32 d0, d1", d0, i32, 0x12345678, d1, i32, 0x0a0b0c0d);
    TESTINSN_dual("vuzp.16 d1, d0", d0, i32, 0x12345678, d1, i32, 0x0a0b0c0d);
    TESTINSN_dual("vuzp.8 d30, d31", d30, i32, 0x12345678, d31, i32, 0x0a0b0c0d);

    printf("---- VTRN ----\n");
    TESTINSN_dual("vtrn.32 d0, d1", d0, i8, 0x12, d1, i8, 0x34);
    TESTINSN_dual("vtrn.16 d1, d0", d0, i8, 0x12, d1, i8, 0x34);
    TESTINSN_dual("vtrn.8 d10, d11", d10, i8, 0x12, d11, i8, 0x34);
    TESTINSN_dual("vtrn.32 d0, d1", d0, i32, 0x12345678, d1, i32, 0x0a0b0c0d);
    TESTINSN_dual("vtrn.16 d1, d0", d0, i32, 0x12345678, d1, i32, 0x0a0b0c0d);
    TESTINSN_dual("vtrn.8 d30, d31", d30, i32, 0x12345678, d31, i32, 0x0a0b0c0d);

    printf("---- VSWP ----\n");
    TESTINSN_dual("vswp d0, d1", d0, i8, 0x12, d1, i8, 0x34);
    TESTINSN_dual("vswp d1, d0", d0, i8, 0x12, d1, i8, 0x34);
    TESTINSN_dual("vswp d10, d11", d10, i8, 0x12, d11, i8, 0x34);
    TESTINSN_dual("vswp d0, d1", d0, i32, 0x12345678, d1, i32, 0x0a0b0c0d);
    TESTINSN_dual("vswp d1, d0", d0, i32, 0x12345678, d1, i32, 0x0a0b0c0d);
    TESTINSN_dual("vswp d30, d31", d30, i32, 0x12345678, d31, i32, 0x0a0b0c0d);

    printf("---- VSHRN ----\n");
    TESTINSN_un("vshrn.i16 d0, q1, #1", d0, q1, i32, -1);
    TESTINSN_un("vshrn.i16 d3, q4, #2", d3, q4, i32, -0x7c);
    TESTINSN_un("vshrn.i32 d2, q5, #10", d2, q5, i32, -1);
    TESTINSN_un("vshrn.i32 d2, q5, #1", d2, q5, i32, 0x7fffffff);
    TESTINSN_un("vshrn.i64 d6, q7, #7", d6, q7, i32, 0xffff);
    TESTINSN_un("vshrn.i16 d8, q9, #8", d8, q9, i32, -10);
    TESTINSN_un("vshrn.i32 d10, q11, #5", d10, q11, i32, 10234);
    TESTINSN_un("vshrn.i64 d12, q13, #1", d12, q13, i32, -1);
    TESTINSN_un("vshrn.i16 d14, q15, #6", d14, q15, i32, -1);
    TESTINSN_un("vshrn.i32 d10, q11, #9", d10, q11, i32, 1000);
    TESTINSN_un("vshrn.i64 d7, q13, #7", d7, q13, i32, -1);
    TESTINSN_un("vshrn.i16 d8, q1, #1", d8, q1, i32, 0xabcf);
    TESTINSN_un("vshrn.i32 d12, q3, #15", d12, q3, i32, -0x1b0);
    TESTINSN_un("vshrn.i64 d0, q1, #22", d0, q1, i32, -1);
    TESTINSN_un("vshrn.i64 d6, q7, #12", d6, q7, i32, 0xfac);
    TESTINSN_un("vshrn.i64 d8, q4, #9", d8, q4, i32, 13560);
    TESTINSN_un("vshrn.i64 d9, q12, #11", d9, q12, i32, 98710);

    printf("---- VDUP ----\n");
    TESTINSN_un("vdup.8 d12, d2[0]", d12, d2, i32, 0xabc4657);
    TESTINSN_un("vdup.8 d0, d3[2]", d0, d3, i32, 0x7a1b3);
    TESTINSN_un("vdup.8 d1, d0[7]", d1, d0, i32, 0x713aaa);
    TESTINSN_un("vdup.8 d10, d4[3]", d10, d4, i32, 0xaa713);
    TESTINSN_un("vdup.8 d4, d28[4]", d4, d28, i32, 0x7b1c3);
    TESTINSN_un("vdup.16 d17, d19[1]", d17, d19, i32, 0x713ffff);
    TESTINSN_un("vdup.16 d15, d31[2]", d15, d31, i32, 0x7f00fa);
    TESTINSN_un("vdup.16 d6, d2[0]", d6, d2, i32, 0xffabcde);
    TESTINSN_un("vdup.16 d8, d22[3]", d8, d22, i32, 0x713);
    TESTINSN_un("vdup.16 d9, d2[0]", d9, d2, i32, 0x713);
    TESTINSN_un("vdup.32 d10, d17[1]", d10, d17, i32, 0x713);
    TESTINSN_un("vdup.32 d15, d11[0]", d15, d11, i32, 0x3);
    TESTINSN_un("vdup.32 d30, d29[1]", d30, d29, i32, 0xf00000aa);
    TESTINSN_un("vdup.32 d22, d0[1]", d22, d0, i32, 0xf);
    TESTINSN_un("vdup.32 d13, d13[0]", d13, d13, i32, -1);

    printf("---- VQDMULH ----\n");
    TESTINSN_bin_q("vqdmulh.s32 d0, d1, d2", d0, d1, i32, 24, d2, i32, 120);
    TESTINSN_bin_q("vqdmulh.s32 d6, d7, d8", d6, d7, i32, 140, d8, i32, -120);
    TESTINSN_bin_q("vqdmulh.s16 d9, d11, d12", d9, d11, i32, 0x140, d12, i32, 0x120);
    TESTINSN_bin_q("vqdmulh.s16 d4, d5, d6", d4, d5, i32, (1 << 14) + 1, d6, i32, (1 << 13) + 2);
    TESTINSN_bin_q("vqdmulh.s32 d7, d8, d9", d7, d8, i32, (1 << 31) + 1, d9, i32, (1 << 31) + 2);
    TESTINSN_bin_q("vqdmulh.s16 d4, d5, d6", d4, d5, i32, (1 << 14) - 0xabcd, d6, i32, (1 << 13) + 2);
    TESTINSN_bin_q("vqdmulh.s32 d7, d8, d9", d7, d8, i32, (1 << 31), d9, i32, 12);
    TESTINSN_bin_q("vqdmulh.s16 d4, d5, d6", d4, d5, i32, (1 << 28) + 0xfe, d6, i32, (1 << 13) + 2);
    TESTINSN_bin_q("vqdmulh.s32 d7, d8, d9", d7, d8, i32, (1 << 31) + 1, d9, i32, (1 << 31) + 2);
    TESTINSN_bin_q("vqdmulh.s32 d10, d11, d15", d10, d11, i32, 24, d15, i32, 120);
    TESTINSN_bin_q("vqdmulh.s32 d10, d30, d31", d10, d30, i32, 1 << 31, d31, i32, 1 << 31);
    TESTINSN_bin_q("vqdmulh.s16 d10, d30, d31", d10, d30, i32, 1 << 31, d31, i32, 1 << 31);
    TESTINSN_bin_q("vqdmulh.s32 d10, d30, d31", d10, d30, i32, 1 << 30, d31, i32, 1 << 31);
    TESTINSN_bin_q("vqdmulh.s16 d10, d30, d31", d10, d30, i32, 1 << 31, d31, i32, 1 << 30);

    printf("---- VQDMULH (by scalar) ----\n");
    TESTINSN_bin_q("vqdmulh.s32 d0, d1, d6[0]", d0, d1, i32, 24, d6, i32, 120);
    TESTINSN_bin_q("vqdmulh.s32 d6, d7, d1[1]", d6, d7, i32, 140, d1, i32, -120);
    TESTINSN_bin_q("vqdmulh.s16 d9, d11, d7[0]", d9, d11, i32, 0x140, d7, i32, 0x120);
    TESTINSN_bin_q("vqdmulh.s16 d4, d5, d6[0]", d4, d5, i32, (1 << 14) + 1, d6, i32, (1 << 13) + 2);
    TESTINSN_bin_q("vqdmulh.s32 d7, d8, d9[1]", d7, d8, i32, (1 << 31) + 1, d9, i32, (1 << 31) + 2);
    TESTINSN_bin_q("vqdmulh.s16 d4, d5, d6[1]", d4, d5, i32, (1 << 14) - 0xabcd, d6, i16, (1 << 13) + 2);
    TESTINSN_bin_q("vqdmulh.s32 d7, d8, d9[0]", d7, d8, i32, (1 << 31), d9, i32, 12);
    TESTINSN_bin_q("vqdmulh.s16 d4, d5, d6[2]", d4, d5, i32, (1 << 28) + 0xfe, d6, i32, (1 << 13) + 2);
    TESTINSN_bin_q("vqdmulh.s32 d7, d8, d9[0]", d7, d8, i32, (1 << 31) + 1, d9, i32, (1 << 31) + 2);
    TESTINSN_bin_q("vqdmulh.s32 d10, d31, d15[0]", d10, d31, i32, 24, d15, i32, 120);
    TESTINSN_bin_q("vqdmulh.s32 d10, d14, d15[1]", d10, d14, i32, 1 << 31, d7, i32, 1 << 31);
    TESTINSN_bin_q("vqdmulh.s16 d10, d14, d7[3]", d10, d14, i32, 1 << 31, q15, i32, 1 << 31);
    TESTINSN_bin_q("vqdmulh.s32 d10, d14, d15[1]", d10, d14, i32, 1 << 30, d15, i32, 1 << 31);
    TESTINSN_bin_q("vqdmulh.s16 d31, d14, d7[1]", d31, d14, i32, 1 << 31, d7, i32, 1 << 30);

    printf("---- VSHRN ----\n");
    TESTINSN_un("vshrn.i64 d2, q2, #1", d2, q2, i32, 0xabc4657);
    TESTINSN_un("vshrn.i64 d3, q3, #0", d3, q3, i32, 0x7a1b3);
    TESTINSN_un("vshrn.i64 d1, q0, #3", d1, q0, i32, 0x713aaa);
    TESTINSN_un("vshrn.i64 d0, q4, #5", d0, q4, i32, 0xaa713);
    TESTINSN_un("vshrn.i64 d4, q8, #11", d4, q8, i32, 0x7b1c3);
    TESTINSN_un("vshrn.i16 d7, q12, #6", d7, q12, i32, 0x713ffff);
    TESTINSN_un("vshrn.i16 d15, q11, #2", d15, q11, i32, 0x7f00fa);
    TESTINSN_un("vshrn.i16 d6, q2, #4", d6, q2, i32, 0xffabc);
    TESTINSN_un("vshrn.i16 d8, q12, #3", d8, q12, i32, 0x713);
    TESTINSN_un("vshrn.i16 d9, q2, #7", d9, q2, i32, 0x713);
    TESTINSN_un("vshrn.i32 d10, q13, #2", d10, q13, i32, 0x713);
    TESTINSN_un("vshrn.i32 d15, q11, #1", d15, q11, i32, 0x3);
    TESTINSN_un("vshrn.i32 d10, q9, #5", d10, q9, i32, 0xf00000aa);
    TESTINSN_un("vshrn.i32 d12, q0, #6", d12, q0, i32, 0xf);
    TESTINSN_un("vshrn.i32 d13, q13, #2", d13, q13, i32, -1);

    printf("---- VQSHRN ----\n");
    TESTINSN_un_q("vqshrn.s16 d0, q1, #1", d0, q1, i32, -1);
    TESTINSN_un_q("vqshrn.s16 d3, q4, #2", d3, q4, i32, -0x7c);
    TESTINSN_un_q("vqshrn.s32 d2, q5, #10", d2, q5, i32, -1);
    TESTINSN_un_q("vqshrn.s32 d2, q5, #1", d2, q5, i32, 0x7fffffff);
    TESTINSN_un_q("vqshrn.s16 d2, q5, #1", d2, q5, i16, 0x7fff);
    TESTINSN_un_q("vqshrn.s64 d6, q7, #7", d6, q7, i32, 0xffff);
    TESTINSN_un_q("vqshrn.s16 d8, q9, #8", d8, q9, i32, -10);
    TESTINSN_un_q("vqshrn.s32 d10, q11, #5", d10, q11, i32, 10234);
    TESTINSN_un_q("vqshrn.s64 d12, q13, #1", d12, q13, i32, -1);
    TESTINSN_un_q("vqshrn.s16 d14, q15, #6", d14, q15, i32, -1);
    TESTINSN_un_q("vqshrn.s32 d10, q11, #9", d10, q11, i32, 1000);
    TESTINSN_un_q("vqshrn.s64 d7, q13, #7", d7, q13, i32, -1);
    TESTINSN_un_q("vqshrn.s16 d8, q1, #1", d8, q1, i32, 0xabcf);
    TESTINSN_un_q("vqshrn.s32 d8, q1, #1", d8, q1, i32, 0xabcf);
    TESTINSN_un_q("vqshrn.s32 d12, q3, #15", d12, q3, i32, -0x1b0);
    TESTINSN_un_q("vqshrn.s64 d0, q1, #22", d0, q1, i32, -1);
    TESTINSN_un_q("vqshrn.s64 d6, q7, #12", d6, q7, i32, 0xfac);
    TESTINSN_un_q("vqshrn.s64 d8, q4, #9", d8, q4, i32, 13560);
    TESTINSN_un_q("vqshrn.s64 d9, q12, #11", d9, q12, i32, 98710);
    TESTINSN_un_q("vqshrn.u16 d0, q1, #1", d0, q1, i32, -1);
    TESTINSN_un_q("vqshrn.u16 d3, q4, #2", d3, q4, i32, -0x7c);
    TESTINSN_un_q("vqshrn.u32 d2, q5, #10", d2, q5, i32, -1);
    TESTINSN_un_q("vqshrn.u32 d2, q5, #1", d2, q5, i32, 0x7fffffff);
    TESTINSN_un_q("vqshrn.u16 d2, q5, #1", d2, q5, i16, 0x7fff);
    TESTINSN_un_q("vqshrn.u64 d6, q7, #7", d6, q7, i32, 0xffff);
    TESTINSN_un_q("vqshrn.u16 d8, q9, #8", d8, q9, i32, -10);
    TESTINSN_un_q("vqshrn.u32 d10, q11, #5", d10, q11, i32, 10234);
    TESTINSN_un_q("vqshrn.u64 d12, q13, #1", d12, q13, i32, -1);
    TESTINSN_un_q("vqshrn.u16 d14, q15, #6", d14, q15, i32, -1);
    TESTINSN_un_q("vqshrn.u32 d10, q11, #9", d10, q11, i32, 1000);
    TESTINSN_un_q("vqshrn.u64 d7, q13, #7", d7, q13, i32, -1);
    TESTINSN_un_q("vqshrn.u16 d8, q1, #1", d8, q1, i32, 0xabcf);
    TESTINSN_un_q("vqshrn.u32 d8, q1, #1", d8, q1, i32, 0xabcf);
    TESTINSN_un_q("vqshrn.u32 d12, q3, #15", d12, q3, i32, -0x1b0);
    TESTINSN_un_q("vqshrn.u64 d0, q1, #22", d0, q1, i32, -1);
    TESTINSN_un_q("vqshrn.u64 d6, q7, #12", d6, q7, i32, 0xfac);
    TESTINSN_un_q("vqshrn.u64 d8, q4, #9", d8, q4, i32, 13560);
    TESTINSN_un_q("vqshrn.u64 d9, q12, #11", d9, q12, i32, 98710);

    printf("---- VQSHRUN ----\n");
    TESTINSN_un_q("vqshrun.s16 d0, q1, #1", d0, q1, i32, -1);
    TESTINSN_un_q("vqshrun.s16 d3, q4, #2", d3, q4, i32, -0x7c);
    TESTINSN_un_q("vqshrun.s32 d2, q5, #10", d2, q5, i32, -1);
    TESTINSN_un_q("vqshrun.s32 d2, q5, #1", d2, q5, i32, 0x7fffffff);
    TESTINSN_un_q("vqshrun.s16 d2, q5, #1", d2, q5, i16, 0x7fff);
    TESTINSN_un_q("vqshrun.s64 d6, q7, #7", d6, q7, i32, 0xffff);
    TESTINSN_un_q("vqshrun.s16 d8, q9, #8", d8, q9, i32, -10);
    TESTINSN_un_q("vqshrun.s32 d10, q11, #5", d10, q11, i32, 10234);
    TESTINSN_un_q("vqshrun.s64 d12, q13, #1", d12, q13, i32, -1);
    TESTINSN_un_q("vqshrun.s16 d14, q15, #6", d14, q15, i32, -1);
    TESTINSN_un_q("vqshrun.s32 d10, q11, #9", d10, q11, i32, 1000);
    TESTINSN_un_q("vqshrun.s64 d7, q13, #7", d7, q13, i32, -1);
    TESTINSN_un_q("vqshrun.s16 d8, q1, #1", d8, q1, i32, 0xabcf);
    TESTINSN_un_q("vqshrun.s32 d8, q1, #1", d8, q1, i32, 0xabcf);
    TESTINSN_un_q("vqshrun.s32 d12, q3, #15", d12, q3, i32, -0x1b0);
    TESTINSN_un_q("vqshrun.s64 d0, q1, #22", d0, q1, i32, -1);
    TESTINSN_un_q("vqshrun.s64 d6, q7, #12", d6, q7, i32, 0xfac);
    TESTINSN_un_q("vqshrun.s64 d8, q4, #9", d8, q4, i32, 13560);
    TESTINSN_un_q("vqshrun.s64 d9, q12, #11", d9, q12, i32, 98710);

    printf("---- VQRSHRN ----\n");
    TESTINSN_un_q("vqrshrn.s16 d0, q1, #1", d0, q1, i32, -1);
    TESTINSN_un_q("vqrshrn.s16 d3, q4, #2", d3, q4, i32, -0x7c);
    TESTINSN_un_q("vqrshrn.s32 d2, q5, #10", d2, q5, i32, -1);
    TESTINSN_un_q("vqrshrn.s32 d2, q5, #1", d2, q5, i32, 0x7fffffff);
    TESTINSN_un_q("vqrshrn.s16 d2, q5, #1", d2, q5, i16, 0x7fff);
    TESTINSN_un_q("vqrshrn.s64 d6, q7, #7", d6, q7, i32, 0xffff);
    TESTINSN_un_q("vqrshrn.s16 d8, q9, #8", d8, q9, i32, -10);
    TESTINSN_un_q("vqrshrn.s32 d10, q11, #5", d10, q11, i32, 10234);
    TESTINSN_un_q("vqrshrn.s64 d12, q13, #1", d12, q13, i32, -1);
    TESTINSN_un_q("vqrshrn.s16 d14, q15, #6", d14, q15, i32, -1);
    TESTINSN_un_q("vqrshrn.s32 d10, q11, #9", d10, q11, i32, 1000);
    TESTINSN_un_q("vqrshrn.s64 d7, q13, #7", d7, q13, i32, -1);
    TESTINSN_un_q("vqrshrn.s16 d8, q1, #1", d8, q1, i32, 0xabcf);
    TESTINSN_un_q("vqrshrn.s32 d8, q1, #1", d8, q1, i32, 0xabcf);
    TESTINSN_un_q("vqrshrn.s32 d12, q3, #15", d12, q3, i32, -0x1b0);
    TESTINSN_un_q("vqrshrn.s64 d0, q1, #22", d0, q1, i32, -1);
    TESTINSN_un_q("vqrshrn.s64 d6, q7, #12", d6, q7, i32, 0xfac);
    TESTINSN_un_q("vqrshrn.s64 d8, q4, #9", d8, q4, i32, 13560);
    TESTINSN_un_q("vqrshrn.s64 d9, q12, #11", d9, q12, i32, 98710);
    TESTINSN_un_q("vqrshrn.u16 d0, q1, #1", d0, q1, i32, -1);
    TESTINSN_un_q("vqrshrn.u16 d3, q4, #2", d3, q4, i32, -0x7c);
    TESTINSN_un_q("vqrshrn.u32 d2, q5, #10", d2, q5, i32, -1);
    TESTINSN_un_q("vqrshrn.u32 d2, q5, #1", d2, q5, i32, 0x7fffffff);
    TESTINSN_un_q("vqrshrn.u16 d2, q5, #1", d2, q5, i16, 0x7fff);
    TESTINSN_un_q("vqrshrn.u64 d6, q7, #7", d6, q7, i32, 0xffff);
    TESTINSN_un_q("vqrshrn.u16 d8, q9, #8", d8, q9, i32, -10);
    TESTINSN_un_q("vqrshrn.u32 d10, q11, #5", d10, q11, i32, 10234);
    TESTINSN_un_q("vqrshrn.u64 d12, q13, #1", d12, q13, i32, -1);
    TESTINSN_un_q("vqrshrn.u16 d14, q15, #6", d14, q15, i32, -1);
    TESTINSN_un_q("vqrshrn.u32 d10, q11, #9", d10, q11, i32, 1000);
    TESTINSN_un_q("vqrshrn.u64 d7, q13, #7", d7, q13, i32, -1);
    TESTINSN_un_q("vqrshrn.u16 d8, q1, #1", d8, q1, i32, 0xabcf);
    TESTINSN_un_q("vqrshrn.u32 d8, q1, #1", d8, q1, i32, 0xabcf);
    TESTINSN_un_q("vqrshrn.u32 d12, q3, #15", d12, q3, i32, -0x1b0);
    TESTINSN_un_q("vqrshrn.u64 d0, q1, #22", d0, q1, i32, -1);
    TESTINSN_un_q("vqrshrn.u64 d6, q7, #12", d6, q7, i32, 0xfac);
    TESTINSN_un_q("vqrshrn.u64 d8, q4, #9", d8, q4, i32, 13560);
    TESTINSN_un_q("vqrshrn.u64 d9, q12, #11", d9, q12, i32, 98710);

    printf("---- VQRSHRUN ----\n");
    TESTINSN_un_q("vqrshrun.s16 d0, q1, #1", d0, q1, i32, -1);
    TESTINSN_un_q("vqrshrun.s16 d3, q4, #2", d3, q4, i32, -0x7c);
    TESTINSN_un_q("vqrshrun.s32 d2, q5, #10", d2, q5, i32, -1);
    TESTINSN_un_q("vqrshrun.s32 d2, q5, #1", d2, q5, i32, 0x7fffffff);
    TESTINSN_un_q("vqrshrun.s16 d2, q5, #1", d2, q5, i16, 0x7fff);
    TESTINSN_un_q("vqrshrun.s64 d6, q7, #7", d6, q7, i32, 0xffff);
    TESTINSN_un_q("vqrshrun.s16 d8, q9, #8", d8, q9, i32, -10);
    TESTINSN_un_q("vqrshrun.s32 d10, q11, #5", d10, q11, i32, 10234);
    TESTINSN_un_q("vqrshrun.s64 d12, q13, #1", d12, q13, i32, -1);
    TESTINSN_un_q("vqrshrun.s16 d14, q15, #6", d14, q15, i32, -1);
    TESTINSN_un_q("vqrshrun.s32 d10, q11, #9", d10, q11, i32, 1000);
    TESTINSN_un_q("vqrshrun.s64 d7, q13, #7", d7, q13, i32, -1);
    TESTINSN_un_q("vqrshrun.s16 d8, q1, #1", d8, q1, i32, 0xabcf);
    TESTINSN_un_q("vqrshrun.s32 d8, q1, #1", d8, q1, i32, 0xabcf);
    TESTINSN_un_q("vqrshrun.s32 d12, q3, #15", d12, q3, i32, -0x1b0);
    TESTINSN_un_q("vqrshrun.s64 d0, q1, #22", d0, q1, i32, -1);
    TESTINSN_un_q("vqrshrun.s64 d6, q7, #12", d6, q7, i32, 0xfac);
    TESTINSN_un_q("vqrshrun.s64 d8, q4, #9", d8, q4, i32, 13560);
    TESTINSN_un_q("vqrshrun.s64 d9, q12, #11", d9, q12, i32, 98710);

    printf("---- VRSHRN ----\n");
    TESTINSN_un("vrshrn.i64 d2, q2, #1", d2, q2, i32, 0xabc4657);
    TESTINSN_un("vrshrn.i64 d3, q3, #0", d3, q3, i32, 0x7a1b3);
    TESTINSN_un("vrshrn.i64 d1, q0, #3", d1, q0, i32, 0x713aaa);
    TESTINSN_un("vrshrn.i64 d0, q4, #5", d0, q4, i32, 0xaa713);
    TESTINSN_un("vrshrn.i64 d4, q8, #11", d4, q8, i32, 0x7b1c3);
    TESTINSN_un("vrshrn.i16 d7, q12, #6", d7, q12, i32, 0x713ffff);
    TESTINSN_un("vrshrn.i16 d15, q11, #2", d15, q11, i32, 0x7f00fa);
    TESTINSN_un("vrshrn.i16 d6, q2, #4", d6, q2, i32, 0xffabc);
    TESTINSN_un("vrshrn.i16 d8, q12, #3", d8, q12, i32, 0x713);
    TESTINSN_un("vrshrn.i16 d9, q2, #7", d9, q2, i32, 0x713);
    TESTINSN_un("vrshrn.i32 d10, q13, #2", d10, q13, i32, 0x713);
    TESTINSN_un("vrshrn.i32 d15, q11, #1", d15, q11, i32, 0x3);
    TESTINSN_un("vrshrn.i32 d10, q9, #5", d10, q9, i32, 0xf00000aa);
    TESTINSN_un("vrshrn.i32 d12, q0, #6", d12, q0, i32, 0xf);
    TESTINSN_un("vrshrn.i32 d13, q13, #2", d13, q13, i32, -1);

    printf("---- VSHL (immediate) ----\n");
    TESTINSN_un("vshl.i64 d0, d1, #1", d0, d1, i32, 24);
    TESTINSN_un("vshl.i64 d5, d2, #1", d5, d2, i32, (1 << 30));
    TESTINSN_un("vshl.i64 d9, d12, #2", d9, d12, i32, (1 << 31) + 2);
    TESTINSN_un("vshl.i64 d11, d2, #12", d11, d2, i32, -1);
    TESTINSN_un("vshl.i64 d15, d12, #63", d15, d12, i32, 5);
    TESTINSN_un("vshl.i64 d5, d12, #62", d5, d12, i32, (1 << 31) + 1);
    TESTINSN_un("vshl.i32 d0, d1, #1", d0, d1, i32, 24);
    TESTINSN_un("vshl.i32 d5, d2, #1", d5, d2, i32, (1 << 30));
    TESTINSN_un("vshl.i32 d9, d12, #2", d9, d12, i32, (1 << 31) + 2);
    TESTINSN_un("vshl.i32 d11, d2, #12", d11, d2, i32, -1);
    TESTINSN_un("vshl.i32 d15, d12, #20", d15, d12, i32, 5);
    TESTINSN_un("vshl.i32 d5, d12, #30", d5, d12, i32, (1 << 31) + 1);
    TESTINSN_un("vshl.i16 d0, d1, #1", d0, d1, i16, 24);
    TESTINSN_un("vshl.i16 d5, d2, #1", d5, d2, i32, (1 << 30));
    TESTINSN_un("vshl.i16 d9, d12, #2", d9, d12, i32, (1 << 31) + 2);
    TESTINSN_un("vshl.i16 d11, d2, #12", d11, d2, i16, -1);
    TESTINSN_un("vshl.i16 d15, d12, #3", d15, d12, i16, 5);
    TESTINSN_un("vshl.i16 d5, d12, #14", d5, d12, i32, (1 << 31) + 1);
    TESTINSN_un("vshl.i8 d0, d1, #1", d0, d1, i8, 24);
    TESTINSN_un("vshl.i8 d5, d2, #1", d5, d2, i32, (1 << 30));
    TESTINSN_un("vshl.i8 d9, d12, #2", d9, d12, i32, (1 << 31) + 2);
    TESTINSN_un("vshl.i8 d11, d2, #7", d11, d2, i8, -1);
    TESTINSN_un("vshl.i8 d15, d12, #3", d15, d12, i8, 5);
    TESTINSN_un("vshl.i8 d5, d12, #6", d5, d12, i32, (1 << 31) + 1);

    printf("---- VNEG ----\n");
    TESTINSN_un("vneg.s32 d0, d1", d0, d1, i32, 0x73);
    TESTINSN_un("vneg.s16 d15, d4", d15, d4, i32, 0x73);
    TESTINSN_un("vneg.s8 d8, d7", d8, d7, i32, 0x73);
    TESTINSN_un("vneg.s32 d0, d1", d0, d1, i32, 0xfe);
    TESTINSN_un("vneg.s16 d31, d4", d31, d4, i32, 0xef);
    TESTINSN_un("vneg.s8 d8, d7", d8, d7, i32, 0xde);
    TESTINSN_un("vneg.s32 d0, d1", d0, d1, i16, 0xfe0a);
    TESTINSN_un("vneg.s16 d15, d4", d15, d4, i16, 0xef0b);
    TESTINSN_un("vneg.s8 d8, d7", d8, d7, i16, 0xde0c);

    printf("---- VQNEG ----\n");
    TESTINSN_un_q("vqneg.s32 d0, d1", d0, d1, i32, 0x73);
    TESTINSN_un_q("vqneg.s32 d0, d1", d0, d1, i32, 1 << 31);
    TESTINSN_un_q("vqneg.s16 d0, d1", d0, d1, i32, 1 << 31);
    TESTINSN_un_q("vqneg.s8 d0, d1", d0, d1, i32, 1 << 31);
    TESTINSN_un_q("vqneg.s16 d15, d4", d15, d4, i32, 0x73);
    TESTINSN_un_q("vqneg.s8 d8, d7", d8, d7, i32, 0x73);
    TESTINSN_un_q("vqneg.s32 d0, d1", d0, d1, i32, 0xfe);
    TESTINSN_un_q("vqneg.s16 d31, d4", d31, d4, i32, 0xef);
    TESTINSN_un_q("vqneg.s8 d8, d7", d8, d7, i32, 0xde);
    TESTINSN_un_q("vqneg.s32 d0, d1", d0, d1, i16, 0xfe0a);
    TESTINSN_un_q("vqneg.s16 d15, d4", d15, d4, i16, 0xef0b);
    TESTINSN_un_q("vqneg.s8 d8, d7", d8, d7, i16, 0xde0c);

    printf("---- VREV ----\n");
    TESTINSN_un("vrev64.8 d0, d1", d0, d1, i32, 0xaabbccdd);
    TESTINSN_un("vrev64.16 d10, d31", d10, d31, i32, 0xaabbccdd);
    TESTINSN_un("vrev64.32 d1, d14", d1, d14, i32, 0xaabbccdd);
    TESTINSN_un("vrev32.8 d0, d1", d0, d1, i32, 0xaabbccdd);
    TESTINSN_un("vrev32.16 d30, d15", d30, d15, i32, 0xaabbccdd);
    TESTINSN_un("vrev16.8 d0, d1", d0, d1, i32, 0xaabbccdd);

    printf("---- VTBL ----\n");
    TESTINSN_tbl_1("vtbl.8 d0, {d2}, d1", d0, d1, i8, 0, d2, i32, 0x12345678);
    TESTINSN_tbl_1("vtbl.8 d0, {d31}, d1", d0, d1, i8, 0x07, d31, i32, 0x12345678);
    TESTINSN_tbl_1("vtbl.8 d0, {d20}, d1", d0, d1, i8, 1, d20, i32, 0x12345678);
    TESTINSN_tbl_1("vtbl.8 d0, {d2}, d31", d0, d31, i8, 2, d2, i32, 0x12345678);
    TESTINSN_tbl_1("vtbl.8 d30, {d2}, d1", d30, d1, i32, 0x07030501, d2, i32, 0x12345678);
    TESTINSN_tbl_1("vtbl.8 d31, {d2}, d1", d31, d1, i16, 0x0104, d2, i32, 0x12345678);
    TESTINSN_tbl_1("vtbl.8 d30, {d2}, d1", d30, d1, i32, 0x07080501, d2, i32, 0x12345678);
    TESTINSN_tbl_1("vtbl.8 d30, {d2}, d1", d30, d1, i32, 0x07ed05ee, d2, i32, 0x12345678);
    TESTINSN_tbl_2("vtbl.8 d0, {d2-d3}, d1", d0, d1, i8, 0, d2, i32, 0x12345678, d3, i32, 0xa1a2a3a4);
    TESTINSN_tbl_2("vtbl.8 d0, {d1-d2}, d3", d0, d3, i8, 0xa, d1, i32, 0x12345678, d2, i32, 0xa1a2a3a4);
    TESTINSN_tbl_2("vtbl.8 d0, {d30-d31}, d1", d0, d1, i8, 0xf, d30, i32, 0x12345678, d31, i32, 0xa1a2a3a4);
    TESTINSN_tbl_2("vtbl.8 d0, {d22-d23}, d1", d0, d1, i8, 9, d22, i32, 0x12345678, d23, i32, 0xa1a2a3a4);
    TESTINSN_tbl_2("vtbl.8 d0, {d22-d23}, d1", d0, d1, i8, 15, d22, i32, 0x12345678, d23, i32, 0xa1a2a3a4);
    TESTINSN_tbl_2("vtbl.8 d0, {d22-d23}, d1", d0, d1, i8, 4, d22, i32, 0x12345678, d23, i32, 0xa1a2a3a4);
    TESTINSN_tbl_2("vtbl.8 d0, {d22-d23}, d1", d0, d1, i8, 14, d22, i32, 0x12345678, d23, i32, 0xa1a2a3a4);
    TESTINSN_tbl_2("vtbl.8 d0, {d22-d23}, d1", d0, d1, i8, 15, d22, i32, 0x12345678, d23, i32, 0xa1a2a3a4);
    TESTINSN_tbl_2("vtbl.8 d30, {d2-d3}, d31", d30, d31, i32, 0x07030501, d2, i32, 0x12345678, d3, i32, 0xa1a2a3a4);
    TESTINSN_tbl_2("vtbl.8 d30, {d2-d3}, d31", d30, d31, i32, 0x0c0a0501, d2, i32, 0x12345678, d3, i32, 0xa1a2a3a4);
    TESTINSN_tbl_2("vtbl.8 d30, {d2-d3}, d31", d30, d31, i32, 0x070e0e01, d2, i32, 0x12345678, d3, i32, 0xa1a2a3a4);
    TESTINSN_tbl_2("vtbl.8 d30, {d2-d3}, d31", d30, d31, i32, 0x0d130f01, d2, i32, 0x12345678, d3, i32, 0xa1a2a3a4);
    TESTINSN_tbl_2("vtbl.8 d30, {d2-d3}, d31", d30, d31, i32, 0x07030511, d2, i32, 0x12345678, d3, i32, 0xa1a2a3a4);
    TESTINSN_tbl_3("vtbl.8 d0, {d2-d4}, d1", d0, d1, i8, 0, d2, i32, 0x12345678, d3, i32, 0xa1a2a3a4, d4, i32, 0xcacbcccd);
    TESTINSN_tbl_3("vtbl.8 d0, {d1-d3}, d10", d0, d10, i8, 0x11, d1, i32, 0x12345678, d2, i32, 0xa1a2a3a4, d3, i32, 0xcacbcccd);
    TESTINSN_tbl_3("vtbl.8 d0, {d29-d31}, d1", d0, d1, i8, 0x17, d29, i32, 0x12345678, d30, i32, 0xa1a2a3a4, d31, i32, 0xcacbcccd);
    TESTINSN_tbl_3("vtbl.8 d0, {d22-d24}, d1", d0, d1, i8, 9, d22, i32, 0x12345678, d23, i32, 0xa1a2a3a4, d24, i32, 0xcacbcccd);
    TESTINSN_tbl_3("vtbl.8 d0, {d22-d24}, d1", d0, d1, i8, 15, d22, i32, 0x12345678, d23, i32, 0xa1a2a3a4, d24, i32, 0xcacbcccd);
    TESTINSN_tbl_3("vtbl.8 d0, {d22-d24}, d1", d0, d1, i8, 4, d22, i32, 0x12345678, d23, i32, 0xa1a2a3a4, d24, i32, 0xcacbcccd);
    TESTINSN_tbl_3("vtbl.8 d0, {d22-d24}, d1", d0, d1, i8, 16, d22, i32, 0x12345678, d23, i32, 0xa1a2a3a4, d24, i32, 0xcacbcccd);
    TESTINSN_tbl_3("vtbl.8 d0, {d22-d24}, d1", d0, d1, i8, 17, d22, i32, 0x12345678, d23, i32, 0xa1a2a3a4, d24, i32, 0xcacbcccd);
    TESTINSN_tbl_3("vtbl.8 d30, {d2-d4}, d31", d30, d31, i32, 0x0a031504, d2, i32, 0x12345678, d3, i32, 0xa1a2a3a4, d4, i32, 0xcacbcccd);
    TESTINSN_tbl_3("vtbl.8 d30, {d2-d4}, d31", d30, d31, i32, 0x0c0a0501, d2, i32, 0x12345678, d3, i32, 0xa1a2a3a4, d4, i32, 0xcacbcccd);
    TESTINSN_tbl_3("vtbl.8 d30, {d2-d4}, d31", d30, d31, i32, 0x170efe0f, d2, i32, 0x12345678, d3, i32, 0xa1a2a3a4, d4, i32, 0xcacbcccd);
    TESTINSN_tbl_3("vtbl.8 d30, {d2-d4}, d31", d30, d31, i32, 0x0d130f11, d2, i32, 0x12345678, d3, i32, 0xa1a2a3a4, d4, i32, 0xcacbcccd);
    TESTINSN_tbl_3("vtbl.8 d30, {d2-d4}, d31", d30, d31, i32, 0x070f1511, d2, i32, 0x12345678, d3, i32, 0xa1a2a3a4, d4, i32, 0xcacbcccd);
    TESTINSN_tbl_4("vtbl.8 d0, {d2-d5}, d1", d0, d1, i8, 0, d2, i32, 0x12345678, d3, i32, 0xa1a2a3a4, d4, i32, 0xcacbcccd, d5, i32, 0xfefdfcfb);
    TESTINSN_tbl_4("vtbl.8 d0, {d1-d4}, d10", d0, d10, i8, 0x11, d1, i32, 0x12345678, d2, i32, 0xa1a2a3a4, d3, i32, 0xcacbcccd, d4, i32, 0xfefdfcfb);
    TESTINSN_tbl_4("vtbl.8 d0, {d28-d31}, d1", d0, d1, i8, 0x17, d28, i32, 0x12345678, d29, i32, 0xa1a2a3a4, d30, i32, 0xcacbcccd, d31, i32, 0xfefdfcfb);
    TESTINSN_tbl_4("vtbl.8 d0, {d22-d25}, d1", d0, d1, i8, 9, d22, i32, 0x12345678, d23, i32, 0xa1a2a3a4, d24, i32, 0xcacbcccd, d25, i32, 0xfefdfcfb);
    TESTINSN_tbl_4("vtbl.8 d0, {d22-d25}, d1", d0, d1, i8, 0x1a, d22, i32, 0x12345678, d23, i32, 0xa1a2a3a4, d24, i32, 0xcacbcccd, d25, i32, 0xfefdfcfb);
    TESTINSN_tbl_4("vtbl.8 d0, {d22-d25}, d1", d0, d1, i8, 4, d22, i32, 0x12345678, d23, i32, 0xa1a2a3a4, d24, i32, 0xcacbcccd, d25, i32, 0xfefdfcfb);
    TESTINSN_tbl_4("vtbl.8 d0, {d22-d25}, d1", d0, d1, i8, 0x16, d22, i32, 0x12345678, d23, i32, 0xa1a2a3a4, d24, i32, 0xcacbcccd, d25, i32, 0xfefdfcfb);
    TESTINSN_tbl_4("vtbl.8 d0, {d22-d25}, d1", d0, d1, i8, 0x1f, d22, i32, 0x12345678, d23, i32, 0xa1a2a3a4, d24, i32, 0xcacbcccd, d25, i32, 0xfefdfcfb);
    TESTINSN_tbl_4("vtbl.8 d30, {d2-d5}, d31", d30, d31, i32, 0x1a0315ff, d2, i32, 0x12345678, d3, i32, 0xa1a2a3a4, d4, i32, 0xcacbcccd, d5, i32, 0xfefdfcfb);
    TESTINSN_tbl_4("vtbl.8 d30, {d2-d5}, d31", d30, d31, i32, 0x0c0a0501, d2, i32, 0x12345678, d3, i32, 0xa1a2a3a4, d4, i32, 0xcacbcccd, d5, i32, 0xfefdfcfb);
    TESTINSN_tbl_4("vtbl.8 d30, {d2-d5}, d31", d30, d31, i32, 0x171efe0f, d2, i32, 0x12345678, d3, i32, 0xa1a2a3a4, d4, i32, 0xcacbcccd, d5, i32, 0xfefdfcfb);
    TESTINSN_tbl_4("vtbl.8 d30, {d2-d5}, d31", d30, d31, i32, 0x1d130f1a, d2, i32, 0x12345678, d3, i32, 0xa1a2a3a4, d4, i32, 0xcacbcccd, d5, i32, 0xfefdfcfb);
    TESTINSN_tbl_4("vtbl.8 d30, {d2-d5}, d31", d30, d31, i32, 0x17101c11, d2, i32, 0x12345678, d3, i32, 0xa1a2a3a4, d4, i32, 0xcacbcccd, d5, i32, 0xfefdfcfb);

    printf("---- VTBX ----\n");
    TESTINSN_tbl_1("vtbx.8 d0, {d2}, d1", d0, d1, i8, 0, d2, i32, 0x12345678);
    TESTINSN_tbl_1("vtbx.8 d0, {d31}, d1", d0, d1, i8, 0x07, d31, i32, 0x12345678);
    TESTINSN_tbl_1("vtbx.8 d0, {d20}, d1", d0, d1, i8, 1, d20, i32, 0x12345678);
    TESTINSN_tbl_1("vtbx.8 d0, {d2}, d31", d0, d31, i8, 2, d2, i32, 0x12345678);
    TESTINSN_tbl_1("vtbx.8 d30, {d2}, d1", d30, d1, i32, 0x07030501, d2, i32, 0x12345678);
    TESTINSN_tbl_1("vtbx.8 d31, {d2}, d1", d31, d1, i16, 0x0104, d2, i32, 0x12345678);
    TESTINSN_tbl_1("vtbx.8 d30, {d2}, d1", d30, d1, i32, 0x07080501, d2, i32, 0x12345678);
    TESTINSN_tbl_1("vtbx.8 d30, {d2}, d1", d30, d1, i32, 0x07ed05ee, d2, i32, 0x12345678);
    TESTINSN_tbl_2("vtbx.8 d0, {d2-d3}, d1", d0, d1, i8, 0, d2, i32, 0x12345678, d3, i32, 0xa1a2a3a4);
    TESTINSN_tbl_2("vtbx.8 d0, {d1-d2}, d3", d0, d3, i8, 0xa, d1, i32, 0x12345678, d2, i32, 0xa1a2a3a4);
    TESTINSN_tbl_2("vtbx.8 d0, {d30-d31}, d1", d0, d1, i8, 0xf, d30, i32, 0x12345678, d31, i32, 0xa1a2a3a4);
    TESTINSN_tbl_2("vtbx.8 d0, {d22-d23}, d1", d0, d1, i8, 9, d22, i32, 0x12345678, d23, i32, 0xa1a2a3a4);
    TESTINSN_tbl_2("vtbx.8 d0, {d22-d23}, d1", d0, d1, i8, 15, d22, i32, 0x12345678, d23, i32, 0xa1a2a3a4);
    TESTINSN_tbl_2("vtbx.8 d0, {d22-d23}, d1", d0, d1, i8, 4, d22, i32, 0x12345678, d23, i32, 0xa1a2a3a4);
    TESTINSN_tbl_2("vtbx.8 d0, {d22-d23}, d1", d0, d1, i8, 14, d22, i32, 0x12345678, d23, i32, 0xa1a2a3a4);
    TESTINSN_tbl_2("vtbx.8 d0, {d22-d23}, d1", d0, d1, i8, 15, d22, i32, 0x12345678, d23, i32, 0xa1a2a3a4);
    TESTINSN_tbl_2("vtbx.8 d30, {d2-d3}, d31", d30, d31, i32, 0x07030501, d2, i32, 0x12345678, d3, i32, 0xa1a2a3a4);
    TESTINSN_tbl_2("vtbx.8 d30, {d2-d3}, d31", d30, d31, i32, 0x0c0a0501, d2, i32, 0x12345678, d3, i32, 0xa1a2a3a4);
    TESTINSN_tbl_2("vtbx.8 d30, {d2-d3}, d31", d30, d31, i32, 0x070e0e01, d2, i32, 0x12345678, d3, i32, 0xa1a2a3a4);
    TESTINSN_tbl_2("vtbx.8 d30, {d2-d3}, d31", d30, d31, i32, 0x0d130f01, d2, i32, 0x12345678, d3, i32, 0xa1a2a3a4);
    TESTINSN_tbl_2("vtbx.8 d30, {d2-d3}, d31", d30, d31, i32, 0x07030511, d2, i32, 0x12345678, d3, i32, 0xa1a2a3a4);
    TESTINSN_tbl_3("vtbx.8 d0, {d2-d4}, d1", d0, d1, i8, 0, d2, i32, 0x12345678, d3, i32, 0xa1a2a3a4, d4, i32, 0xcacbcccd);
    TESTINSN_tbl_3("vtbx.8 d0, {d1-d3}, d10", d0, d10, i8, 0x11, d1, i32, 0x12345678, d2, i32, 0xa1a2a3a4, d3, i32, 0xcacbcccd);
    TESTINSN_tbl_3("vtbx.8 d0, {d29-d31}, d1", d0, d1, i8, 0x17, d29, i32, 0x12345678, d30, i32, 0xa1a2a3a4, d31, i32, 0xcacbcccd);
    TESTINSN_tbl_3("vtbx.8 d0, {d22-d24}, d1", d0, d1, i8, 9, d22, i32, 0x12345678, d23, i32, 0xa1a2a3a4, d24, i32, 0xcacbcccd);
    TESTINSN_tbl_3("vtbx.8 d0, {d22-d24}, d1", d0, d1, i8, 15, d22, i32, 0x12345678, d23, i32, 0xa1a2a3a4, d24, i32, 0xcacbcccd);
    TESTINSN_tbl_3("vtbx.8 d0, {d22-d24}, d1", d0, d1, i8, 4, d22, i32, 0x12345678, d23, i32, 0xa1a2a3a4, d24, i32, 0xcacbcccd);
    TESTINSN_tbl_3("vtbx.8 d0, {d22-d24}, d1", d0, d1, i8, 16, d22, i32, 0x12345678, d23, i32, 0xa1a2a3a4, d24, i32, 0xcacbcccd);
    TESTINSN_tbl_3("vtbx.8 d0, {d22-d24}, d1", d0, d1, i8, 17, d22, i32, 0x12345678, d23, i32, 0xa1a2a3a4, d24, i32, 0xcacbcccd);
    TESTINSN_tbl_3("vtbx.8 d30, {d2-d4}, d31", d30, d31, i32, 0x0a031504, d2, i32, 0x12345678, d3, i32, 0xa1a2a3a4, d4, i32, 0xcacbcccd);
    TESTINSN_tbl_3("vtbx.8 d30, {d2-d4}, d31", d30, d31, i32, 0x0c0a0501, d2, i32, 0x12345678, d3, i32, 0xa1a2a3a4, d4, i32, 0xcacbcccd);
    TESTINSN_tbl_3("vtbx.8 d30, {d2-d4}, d31", d30, d31, i32, 0x170efe0f, d2, i32, 0x12345678, d3, i32, 0xa1a2a3a4, d4, i32, 0xcacbcccd);
    TESTINSN_tbl_3("vtbx.8 d30, {d2-d4}, d31", d30, d31, i32, 0x0d130f11, d2, i32, 0x12345678, d3, i32, 0xa1a2a3a4, d4, i32, 0xcacbcccd);
    TESTINSN_tbl_3("vtbx.8 d30, {d2-d4}, d31", d30, d31, i32, 0x070f1511, d2, i32, 0x12345678, d3, i32, 0xa1a2a3a4, d4, i32, 0xcacbcccd);
    TESTINSN_tbl_4("vtbx.8 d0, {d2-d5}, d1", d0, d1, i8, 0, d2, i32, 0x12345678, d3, i32, 0xa1a2a3a4, d4, i32, 0xcacbcccd, d5, i32, 0xfefdfcfb);
    TESTINSN_tbl_4("vtbx.8 d0, {d1-d4}, d10", d0, d10, i8, 0x11, d1, i32, 0x12345678, d2, i32, 0xa1a2a3a4, d3, i32, 0xcacbcccd, d4, i32, 0xfefdfcfb);
    TESTINSN_tbl_4("vtbx.8 d0, {d28-d31}, d1", d0, d1, i8, 0x17, d28, i32, 0x12345678, d29, i32, 0xa1a2a3a4, d30, i32, 0xcacbcccd, d31, i32, 0xfefdfcfb);
    TESTINSN_tbl_4("vtbx.8 d0, {d22-d25}, d1", d0, d1, i8, 9, d22, i32, 0x12345678, d23, i32, 0xa1a2a3a4, d24, i32, 0xcacbcccd, d25, i32, 0xfefdfcfb);
    TESTINSN_tbl_4("vtbx.8 d0, {d22-d25}, d1", d0, d1, i8, 0x1a, d22, i32, 0x12345678, d23, i32, 0xa1a2a3a4, d24, i32, 0xcacbcccd, d25, i32, 0xfefdfcfb);
    TESTINSN_tbl_4("vtbx.8 d0, {d22-d25}, d1", d0, d1, i8, 4, d22, i32, 0x12345678, d23, i32, 0xa1a2a3a4, d24, i32, 0xcacbcccd, d25, i32, 0xfefdfcfb);
    TESTINSN_tbl_4("vtbx.8 d0, {d22-d25}, d1", d0, d1, i8, 0x16, d22, i32, 0x12345678, d23, i32, 0xa1a2a3a4, d24, i32, 0xcacbcccd, d25, i32, 0xfefdfcfb);
    TESTINSN_tbl_4("vtbx.8 d0, {d22-d25}, d1", d0, d1, i8, 0x1f, d22, i32, 0x12345678, d23, i32, 0xa1a2a3a4, d24, i32, 0xcacbcccd, d25, i32, 0xfefdfcfb);
    TESTINSN_tbl_4("vtbx.8 d30, {d2-d5}, d31", d30, d31, i32, 0x1a0315ff, d2, i32, 0x12345678, d3, i32, 0xa1a2a3a4, d4, i32, 0xcacbcccd, d5, i32, 0xfefdfcfb);
    TESTINSN_tbl_4("vtbx.8 d30, {d2-d5}, d31", d30, d31, i32, 0x0c0a0501, d2, i32, 0x12345678, d3, i32, 0xa1a2a3a4, d4, i32, 0xcacbcccd, d5, i32, 0xfefdfcfb);
    TESTINSN_tbl_4("vtbx.8 d30, {d2-d5}, d31", d30, d31, i32, 0x171efe0f, d2, i32, 0x12345678, d3, i32, 0xa1a2a3a4, d4, i32, 0xcacbcccd, d5, i32, 0xfefdfcfb);
    TESTINSN_tbl_4("vtbx.8 d30, {d2-d5}, d31", d30, d31, i32, 0x1d130f1a, d2, i32, 0x12345678, d3, i32, 0xa1a2a3a4, d4, i32, 0xcacbcccd, d5, i32, 0xfefdfcfb);
    TESTINSN_tbl_4("vtbx.8 d30, {d2-d5}, d31", d30, d31, i32, 0x17101c11, d2, i32, 0x12345678, d3, i32, 0xa1a2a3a4, d4, i32, 0xcacbcccd, d5, i32, 0xfefdfcfb);

    printf("---- VPMAX (integer) ----\n");
    TESTINSN_bin("vpmax.s32 d0, d1, d2", d0, d1, i32, 25, d2, i32, 121);
    TESTINSN_bin("vpmax.s32 d0, d1, d2", d0, d1, i32, 250, d2, i32, 121);
    TESTINSN_bin("vpmax.s32 d0, d1, d2", d0, d1, i32, 140, d2, i32, 140);
    TESTINSN_bin("vpmax.s16 d0, d1, d2", d0, d1, i32, 0x01200140, d2, i32, 120);
    TESTINSN_bin("vpmax.s8 d0, d1, d2", d0, d1, i32, 120, d2, i32, 120);
    TESTINSN_bin("vpmax.s8 d5, d7, d5", d5, d7, i32, (1 << 31) + 1, d5, i32, (1 << 31) + 2);
    TESTINSN_bin("vpmax.s16 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vpmax.s32 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vpmax.s8 d5, d7, d5", d5, d7, i32, (1 << 31) + 1, d5, i32, (1 << 31) + 3);
    TESTINSN_bin("vpmax.s16 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 3);
    TESTINSN_bin("vpmax.s32 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 3);
    TESTINSN_bin("vpmax.s8 d5, d7, d5", d5, d7, i32, (1 << 31) + 4, d5, i32, (1 << 31) + 2);
    TESTINSN_bin("vpmax.s16 d0, d1, d2", d0, d1, i32, (1 << 31) + 4, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vpmax.s32 d0, d1, d2", d0, d1, i32, (1 << 31) + 4, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vpmax.s32 d10, d11, d12", d10, d11, i32, 24, d12, i32, 120);
    TESTINSN_bin("vpmax.u32 d0, d1, d2", d0, d1, i32, 25, d2, i32, 120);
    TESTINSN_bin("vpmax.u32 d0, d1, d2", d0, d1, i32, 250, d2, i32, 120);
    TESTINSN_bin("vpmax.u32 d0, d1, d2", d0, d1, i32, 140, d2, i32, 140);
    TESTINSN_bin("vpmax.u16 d0, d1, d2", d0, d1, i32, 0x01200140, d2, i32, 120);
    TESTINSN_bin("vpmax.u8 d0, d1, d2", d0, d1, i32, 0x01202120, d2, i32, 120);
    TESTINSN_bin("vpmax.u8 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vpmax.u16 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vpmax.u32 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vpmax.u8 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 3);
    TESTINSN_bin("vpmax.u16 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 3);
    TESTINSN_bin("vpmax.u32 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 3);
    TESTINSN_bin("vpmax.u8 d0, d1, d2", d0, d1, i32, (1 << 31) + 4, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vpmax.u16 d0, d1, d2", d0, d1, i32, (1 << 31) + 4, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vpmax.u32 d0, d1, d2", d0, d1, i32, (1 << 31) + 4, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vpmax.u32 d10, d11, d12", d10, d11, i32, 24, d12, i32, 120);

    printf("---- VPMIN (integer) ----\n");
    TESTINSN_bin("vpmin.s32 d0, d1, d2", d0, d1, i32, 25, d2, i32, 121);
    TESTINSN_bin("vpmin.s32 d0, d1, d2", d0, d1, i32, 250, d2, i32, 121);
    TESTINSN_bin("vpmin.s32 d0, d1, d2", d0, d1, i32, 140, d2, i32, 140);
    TESTINSN_bin("vpmin.s16 d0, d1, d2", d0, d1, i32, 0x01200140, d2, i32, 120);
    TESTINSN_bin("vpmin.s8 d0, d1, d2", d0, d1, i32, 120, d2, i32, 120);
    TESTINSN_bin("vpmin.s8 d5, d7, d5", d5, d7, i32, (1 << 31) + 1, d5, i32, (1 << 31) + 2);
    TESTINSN_bin("vpmin.s16 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vpmin.s32 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vpmin.s8 d5, d7, d5", d5, d7, i32, (1 << 31) + 1, d5, i32, (1 << 31) + 3);
    TESTINSN_bin("vpmin.s16 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 3);
    TESTINSN_bin("vpmin.s32 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 3);
    TESTINSN_bin("vpmin.s8 d5, d7, d5", d5, d7, i32, (1 << 31) + 4, d5, i32, (1 << 31) + 2);
    TESTINSN_bin("vpmin.s16 d0, d1, d2", d0, d1, i32, (1 << 31) + 4, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vpmin.s32 d0, d1, d2", d0, d1, i32, (1 << 31) + 4, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vpmin.s32 d10, d11, d12", d10, d11, i32, 24, d12, i32, 120);
    TESTINSN_bin("vpmin.u32 d0, d1, d2", d0, d1, i32, 25, d2, i32, 120);
    TESTINSN_bin("vpmin.u32 d0, d1, d2", d0, d1, i32, 250, d2, i32, 120);
    TESTINSN_bin("vpmin.u32 d0, d1, d2", d0, d1, i32, 140, d2, i32, 140);
    TESTINSN_bin("vpmin.u16 d0, d1, d2", d0, d1, i32, 0x01200140, d2, i32, 120);
    TESTINSN_bin("vpmin.u8 d0, d1, d2", d0, d1, i32, 0x01202120, d2, i32, 120);
    TESTINSN_bin("vpmin.u8 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vpmin.u16 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vpmin.u32 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vpmin.u8 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 3);
    TESTINSN_bin("vpmin.u16 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 3);
    TESTINSN_bin("vpmin.u32 d0, d1, d2", d0, d1, i32, (1 << 31) + 1, d2, i32, (1 << 31) + 3);
    TESTINSN_bin("vpmin.u8 d0, d1, d2", d0, d1, i32, (1 << 31) + 4, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vpmin.u16 d0, d1, d2", d0, d1, i32, (1 << 31) + 4, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vpmin.u32 d0, d1, d2", d0, d1, i32, (1 << 31) + 4, d2, i32, (1 << 31) + 2);
    TESTINSN_bin("vpmin.u32 d10, d11, d12", d10, d11, i32, 24, d12, i32, 120);

    printf("---- VQRDMULH ----\n");
    TESTINSN_bin_q("vqrdmulh.s32 d0, d1, d2", d0, d1, i32, 24, d2, i32, 120);
    TESTINSN_bin_q("vqrdmulh.s32 d6, d7, d8", d6, d7, i32, 140, d8, i32, -120);
    TESTINSN_bin_q("vqrdmulh.s16 d9, d11, d12", d9, d11, i32, 0x140, d12, i32, 0x120);
    TESTINSN_bin_q("vqrdmulh.s16 d4, d5, d6", d4, d5, i32, (1 << 14) + 1, d6, i32, (1 << 13) + 2);
    TESTINSN_bin_q("vqrdmulh.s32 d7, d8, d9", d7, d8, i32, (1 << 31) + 1, d9, i32, (1 << 31) + 2);
    TESTINSN_bin_q("vqrdmulh.s16 d4, d5, d6", d4, d5, i32, (1 << 14) - 0xabcd, d6, i32, (1 << 13) + 2);
    TESTINSN_bin_q("vqrdmulh.s32 d7, d8, d9", d7, d8, i32, (1 << 31), d9, i32, 12); 
    TESTINSN_bin_q("vqrdmulh.s16 d4, d5, d6", d4, d5, i32, (1 << 28) + 0xfe, d6, i32, (1 << 13) + 2);
    TESTINSN_bin_q("vqrdmulh.s32 d7, d8, d9", d7, d8, i32, (1 << 31) + 1, d9, i32, (1 << 31) + 2);
    TESTINSN_bin_q("vqrdmulh.s32 d10, d11, d15", d10, d11, i32, 24, d15, i32, 120);
    TESTINSN_bin_q("vqrdmulh.s32 d10, d30, d31", d10, d30, i32, 1 << 31, d31, i32, 1 << 31); 
    TESTINSN_bin_q("vqrdmulh.s16 d10, d30, d31", d10, d30, i32, 1 << 31, d31, i32, (1 << 31) + 1); 
    TESTINSN_bin_q("vqrdmulh.s32 d10, d30, d31", d10, d30, i32, 1 << 30, d31, i32, 1 << 31); 
    TESTINSN_bin_q("vqrdmulh.s16 d10, d30, d31", d10, d30, i32, 1 << 31, d31, i32, 1 << 30);

    printf("---- VQRDMULH (by scalar) ----\n");
    TESTINSN_bin_q("vqrdmulh.s32 d0, d1, d6[0]", d0, d1, i32, 24, d6, i32, 120);
    TESTINSN_bin_q("vqrdmulh.s32 d6, d7, d1[1]", d6, d7, i32, 140, d1, i32, -120);
    TESTINSN_bin_q("vqrdmulh.s16 d9, d11, d7[0]", d9, d11, i32, 0x140, d7, i32, 0x120);
    TESTINSN_bin_q("vqrdmulh.s16 d4, d5, d6[0]", d4, d5, i32, (1 << 14) + 1, d6, i32, (1 << 13) + 2);
    TESTINSN_bin_q("vqrdmulh.s32 d7, d8, d9[1]", d7, d8, i32, (1 << 31) + 1, d9, i32, (1 << 31) + 2);
    TESTINSN_bin_q("vqrdmulh.s16 d4, d5, d6[1]", d4, d5, i32, (1 << 14) - 0xabcd, d6, i16, (1 << 13) + 2);
    TESTINSN_bin_q("vqrdmulh.s32 d7, d8, d9[0]", d7, d8, i32, (1 << 31), d9, i32, 12);
    TESTINSN_bin_q("vqrdmulh.s16 d4, d5, d6[2]", d4, d5, i32, (1 << 28) + 0xfe, d6, i32, (1 << 13) + 2);
    TESTINSN_bin_q("vqrdmulh.s32 d7, d8, d9[0]", d7, d8, i32, (1 << 31) + 1, d9, i32, (1 << 31) + 2);
    TESTINSN_bin_q("vqrdmulh.s32 d10, d31, d15[0]", d10, d31, i32, 24, d15, i32, 120);
    TESTINSN_bin_q("vqrdmulh.s32 d10, d14, d15[1]", d10, d14, i32, 1 << 31, d7, i32, 1 << 31);
    TESTINSN_bin_q("vqrdmulh.s16 d10, d14, d7[3]", d10, d14, i32, 1 << 31, q15, i32, (1 << 31) + 1);
    TESTINSN_bin_q("vqrdmulh.s32 d10, d14, d15[1]", d10, d14, i32, 1 << 30, d15, i32, 1 << 31);
    TESTINSN_bin_q("vqrdmulh.s16 d31, d14, d7[1]", d31, d14, i32, 1 << 31, d7, i32, 1 << 30);

    printf("---- VADD (fp) ----\n");
    TESTINSN_bin("vadd.f32 d0, d5, d2", d0, d5, i32, f2u(23.04), d2, i32, f2u(-45.5687));
    TESTINSN_bin("vadd.f32 d3, d4, d5", d3, d4, i32, f2u(-347856.475), d5, i32, f2u(1346));
    TESTINSN_bin("vadd.f32 d10, d11, d2", d10, d11, i32, f2u(48755), d2, i32, f2u(-45786.476));
    TESTINSN_bin("vadd.f32 d9, d5, d7", d9, d5, i32, f2u(95867.76), d7, i32, f2u(17065));
    TESTINSN_bin("vadd.f32 d0, d5, d2", d0, d5, i32, f2u(-45667.24), d2, i32, f2u(-248562.76));
    TESTINSN_bin("vadd.f32 d3, d4, d5", d3, d4, i32, f2u(24.87556), d5, i32, f2u(1346.0004));
    TESTINSN_bin("vadd.f32 d10, d11, d2", d10, d11, i32, f2u(48755.7), d2, i32, f2u(1089.2));
    TESTINSN_bin("vadd.f32 d9, d5, d7", d9, d5, i32, f2u(214), d7, i32, f2u(1752065));
    TESTINSN_bin("vadd.f32 d0, d11, d12", d0, d11, i32, f2u(356047.56), d12, i32, f2u(5867.009));
    TESTINSN_bin("vadd.f32 d7, d1, d6", d7, d1, i32, f2u(34.00046), d6, i32, f2u(0.0024575));
    TESTINSN_bin("vadd.f32 d0, d1, d2", d0, d1, i32, f2u(2754), d2, i32, f2u(107));
    TESTINSN_bin("vadd.f32 d3, d4, d5", d3, d4, i32, f2u(874), d5, i32, f2u(1384.6));
    TESTINSN_bin("vadd.f32 d10, d11, d2", d10, d11, i32, f2u(487.587), d2, i32, f2u(109));
    TESTINSN_bin("vadd.f32 d9, d5, d7", d9, d5, i32, f2u(2146), d7, i32, f2u(1752));
    TESTINSN_bin("vadd.f32 d0, d11, d12", d0, d11, i32, f2u(-56.25), d12, i32, f2u(-5786.47));
    TESTINSN_bin("vadd.f32 d7, d1, d6", d7, d1, i32, f2u(456.2489562), d6, i32, f2u(-7.2945676));
    TESTINSN_bin("vadd.f32 d0, d5, d2", d0, d5, i32, f2u(532.987), d2, i32, f2u(-0.0045876));
    TESTINSN_bin("vadd.f32 d10, d13, d15", d10, d13, i32, f2u(-485.2457), d15, i32, f2u(-567.245));
    TESTINSN_bin("vadd.f32 d10, d13, d15", d10, d13, i32, f2u(278456.45), d15, i32, f2u(8756.0076));
    TESTINSN_bin("vadd.f32 d0, d1, d2", d0, d1, i32, f2u(876988654), d2, i32, f2u(1224808797));
    TESTINSN_bin("vadd.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(NAN));
    TESTINSN_bin("vadd.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(1.0));
    TESTINSN_bin("vadd.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(0.0));
    TESTINSN_bin("vadd.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(INFINITY));
    TESTINSN_bin("vadd.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(-INFINITY));
    TESTINSN_bin("vadd.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(NAN));
    TESTINSN_bin("vadd.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(1.0));
    TESTINSN_bin("vadd.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(0.0));
    TESTINSN_bin("vadd.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(INFINITY));
    TESTINSN_bin("vadd.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(-INFINITY));
    TESTINSN_bin("vadd.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(NAN));
    TESTINSN_bin("vadd.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(1.0));
    TESTINSN_bin("vadd.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(0.0));
    TESTINSN_bin("vadd.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(INFINITY));
    TESTINSN_bin("vadd.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(-INFINITY));
    TESTINSN_bin("vadd.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(NAN));
    TESTINSN_bin("vadd.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(1.0));
    TESTINSN_bin("vadd.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(0.0));
    TESTINSN_bin("vadd.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(INFINITY));
    TESTINSN_bin("vadd.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(-INFINITY));

    printf("---- VSUB (fp) ----\n");
    TESTINSN_bin("vsub.f32 d0, d5, d2", d0, d5, i32, f2u(23.04), d2, i32, f2u(-45.5687));
    TESTINSN_bin("vsub.f32 d3, d4, d5", d3, d4, i32, f2u(-347856.475), d5, i32, f2u(1346));
    TESTINSN_bin("vsub.f32 d10, d11, d2", d10, d11, i32, f2u(48755), d2, i32, f2u(-45786.476));
    TESTINSN_bin("vsub.f32 d9, d5, d7", d9, d5, i32, f2u(95867.76), d7, i32, f2u(17065));
    TESTINSN_bin("vsub.f32 d0, d5, d2", d0, d5, i32, f2u(-45667.24), d2, i32, f2u(-248562.76));
    TESTINSN_bin("vsub.f32 d3, d4, d5", d3, d4, i32, f2u(24), d5, i32, f2u(1346));
    TESTINSN_bin("vsub.f32 d10, d11, d2", d10, d11, i32, f2u(48755), d2, i32, f2u(1089));
    TESTINSN_bin("vsub.f32 d9, d5, d7", d9, d5, i32, f2u(214), d7, i32, f2u(1752065));
    TESTINSN_bin("vsub.f32 d0, d11, d12", d0, d11, i32, f2u(356047.56), d12, i32, f2u(5867.009));
    TESTINSN_bin("vsub.f32 d7, d1, d6", d7, d1, i32, f2u(34.00046), d6, i32, f2u(0.0024575));
    TESTINSN_bin("vsub.f32 d0, d1, d2", d0, d1, i32, f2u(2754), d2, i32, f2u(107));
    TESTINSN_bin("vsub.f32 d3, d4, d5", d3, d4, i32, f2u(874), d5, i32, f2u(1384.6));
    TESTINSN_bin("vsub.f32 d10, d11, d2", d10, d11, i32, f2u(487.587), d2, i32, f2u(109));
    TESTINSN_bin("vsub.f32 d9, d5, d7", d9, d5, i32, f2u(2146), d7, i32, f2u(1752));
    TESTINSN_bin("vsub.f32 d0, d11, d12", d0, d11, i32, f2u(-56.25), d12, i32, f2u(-5786.47));
    TESTINSN_bin("vsub.f32 d7, d1, d6", d7, d1, i32, f2u(456.2489562), d6, i32, f2u(-7.2945676));
    TESTINSN_bin("vsub.f32 d0, d5, d2", d0, d5, i32, f2u(532.987), d2, i32, f2u(-0.0045876));
    TESTINSN_bin("vsub.f32 d10, d13, d15", d10, d13, i32, f2u(-485.2457), d15, i32, f2u(-567.245));
    TESTINSN_bin("vsub.f32 d10, d13, d15", d10, d13, i32, f2u(278456.45), d15, i32, f2u(8756.0076));
    TESTINSN_bin("vsub.f32 d0, d1, d2", d0, d1, i32, f2u(876988654), d2, i32, f2u(1224808797));
    TESTINSN_bin("vsub.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(NAN));
    TESTINSN_bin("vsub.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(1.0));
    TESTINSN_bin("vsub.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(0.0));
    TESTINSN_bin("vsub.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(INFINITY));
    TESTINSN_bin("vsub.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(-INFINITY));
    TESTINSN_bin("vsub.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(NAN));
    TESTINSN_bin("vsub.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(1.0));
    TESTINSN_bin("vsub.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(0.0));
    TESTINSN_bin("vsub.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(INFINITY));
    TESTINSN_bin("vsub.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(-INFINITY));
    TESTINSN_bin("vsub.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(NAN));
    TESTINSN_bin("vsub.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(1.0));
    TESTINSN_bin("vsub.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(0.0));
    TESTINSN_bin("vsub.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(INFINITY));
    TESTINSN_bin("vsub.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(-INFINITY));
    TESTINSN_bin("vsub.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(NAN));
    TESTINSN_bin("vsub.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(1.0));
    TESTINSN_bin("vsub.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(0.0));
    TESTINSN_bin("vsub.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(INFINITY));
    TESTINSN_bin("vsub.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(-INFINITY));

    printf("---- VMUL (fp) ----\n");
    TESTINSN_bin("vmul.f32 d0, d5, d2", d0, d5, i32, f2u(23.04), d2, i32, f2u(-45.5687));
    TESTINSN_bin("vmul.f32 d3, d4, d5", d3, d4, i32, f2u(-347856.475), d5, i32, f2u(1346));
    TESTINSN_bin("vmul.f32 d10, d11, d2", d10, d11, i32, f2u(48755), d2, i32, f2u(-45786.476));
    TESTINSN_bin("vmul.f32 d9, d5, d7", d9, d5, i32, f2u(95867.76), d7, i32, f2u(17065));
    TESTINSN_bin("vmul.f32 d0, d5, d2", d0, d5, i32, f2u(-45667.24), d2, i32, f2u(-248562.76));
    TESTINSN_bin("vmul.f32 d3, d4, d5", d3, d4, i32, f2u(24), d5, i32, f2u(1346));
    TESTINSN_bin("vmul.f32 d10, d11, d2", d10, d11, i32, f2u(48755), d2, i32, f2u(1089));
    TESTINSN_bin("vmul.f32 d9, d5, d7", d9, d5, i32, f2u(214), d7, i32, f2u(1752065));
    TESTINSN_bin("vmul.f32 d0, d11, d12", d0, d11, i32, f2u(356047.56), d12, i32, f2u(5867.009));
    TESTINSN_bin("vmul.f32 d7, d1, d6", d7, d1, i32, f2u(34.00046), d6, i32, f2u(0.0024575));
    TESTINSN_bin("vmul.f32 d0, d1, d2", d0, d1, i32, f2u(2754), d2, i32, f2u(107));
    TESTINSN_bin("vmul.f32 d3, d4, d5", d3, d4, i32, f2u(874), d5, i32, f2u(1384.6));
    TESTINSN_bin("vmul.f32 d10, d11, d2", d10, d11, i32, f2u(487.587), d2, i32, f2u(109));
    TESTINSN_bin("vmul.f32 d9, d5, d7", d9, d5, i32, f2u(2146), d7, i32, f2u(1752));
    TESTINSN_bin("vmul.f32 d0, d11, d12", d0, d11, i32, f2u(-56.25), d12, i32, f2u(-5786.47));
    TESTINSN_bin("vmul.f32 d7, d1, d6", d7, d1, i32, f2u(456.2489562), d6, i32, f2u(-7.2945676));
    TESTINSN_bin("vmul.f32 d0, d5, d2", d0, d5, i32, f2u(532.987), d2, i32, f2u(-0.0045876));
    TESTINSN_bin("vmul.f32 d10, d13, d15", d10, d13, i32, f2u(-485.2457), d15, i32, f2u(-567.245));
    TESTINSN_bin("vmul.f32 d10, d13, d15", d10, d13, i32, f2u(278456.45), d15, i32, f2u(8756.0076));
    TESTINSN_bin("vmul.f32 d0, d1, d2", d0, d1, i32, f2u(876988654), d2, i32, f2u(1224808797));
    TESTINSN_bin("vmul.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(NAN));
    TESTINSN_bin("vmul.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(1.0));
    TESTINSN_bin("vmul.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(0.0));
    TESTINSN_bin("vmul.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(INFINITY));
    TESTINSN_bin("vmul.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(-INFINITY));
    TESTINSN_bin("vmul.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(NAN));
    TESTINSN_bin("vmul.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(1.0));
    TESTINSN_bin("vmul.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(0.0));
    TESTINSN_bin("vmul.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(INFINITY));
    TESTINSN_bin("vmul.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(-INFINITY));
    TESTINSN_bin("vmul.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(NAN));
    TESTINSN_bin("vmul.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(1.0));
    TESTINSN_bin("vmul.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(0.0));
    TESTINSN_bin("vmul.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(INFINITY));
    TESTINSN_bin("vmul.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(-INFINITY));
    TESTINSN_bin("vmul.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(NAN));
    TESTINSN_bin("vmul.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(1.0));
    TESTINSN_bin("vmul.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(0.0));
    TESTINSN_bin("vmul.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(INFINITY));
    TESTINSN_bin("vmul.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(-INFINITY));

    printf("---- VMLA (fp) ----\n");
    TESTINSN_bin_f("vmla.f32 d0, d5, d2", d0, d5, i32, f2u(23.04), d2, i32, f2u(-45.5687));
    TESTINSN_bin_f("vmla.f32 d3, d4, d5", d3, d4, i32, f2u(-347856.475), d5, i32, f2u(1346));
    TESTINSN_bin_f("vmla.f32 d10, d11, d2", d10, d11, i32, f2u(48755), d2, i32, f2u(-45786.476));
    TESTINSN_bin_f("vmla.f32 d9, d5, d7", d9, d5, i32, f2u(95867.76), d7, i32, f2u(17065));
    TESTINSN_bin_f("vmla.f32 d0, d5, d2", d0, d5, i32, f2u(-45667.24), d2, i32, f2u(-248562.76));
    TESTINSN_bin_f("vmla.f32 d3, d4, d5", d3, d4, i32, f2u(24), d5, i32, f2u(1346));
    TESTINSN_bin_f("vmla.f32 d10, d11, d2", d10, d11, i32, f2u(48755), d2, i32, f2u(1089));
    TESTINSN_bin_f("vmla.f32 d9, d5, d7", d9, d5, i32, f2u(214), d7, i32, f2u(1752065));
    TESTINSN_bin_f("vmla.f32 d0, d11, d12", d0, d11, i32, f2u(356047.56), d12, i32, f2u(5867.009));
    TESTINSN_bin_f("vmla.f32 d7, d1, d6", d7, d1, i32, f2u(34.00046), d6, i32, f2u(0.0024575));
    TESTINSN_bin_f("vmla.f32 d0, d1, d2", d0, d1, i32, f2u(2754), d2, i32, f2u(107));
    TESTINSN_bin_f("vmla.f32 d3, d4, d5", d3, d4, i32, f2u(874), d5, i32, f2u(1384.6));
    TESTINSN_bin_f("vmla.f32 d10, d11, d2", d10, d11, i32, f2u(487.587), d2, i32, f2u(109));
    TESTINSN_bin_f("vmla.f32 d9, d5, d7", d9, d5, i32, f2u(2146), d7, i32, f2u(1752));
    TESTINSN_bin_f("vmla.f32 d0, d11, d12", d0, d11, i32, f2u(-56.25), d12, i32, f2u(-5786.47));
    TESTINSN_bin_f("vmla.f32 d7, d1, d6", d7, d1, i32, f2u(456.2489562), d6, i32, f2u(-7.2945676));
    TESTINSN_bin_f("vmla.f32 d0, d5, d2", d0, d5, i32, f2u(532.987), d2, i32, f2u(-0.0045876));
    TESTINSN_bin_f("vmla.f32 d10, d13, d15", d10, d13, i32, f2u(-485.2457), d15, i32, f2u(-567.245));
    TESTINSN_bin_f("vmla.f32 d10, d13, d15", d10, d13, i32, f2u(278456.45), d15, i32, f2u(8756.0076));
    TESTINSN_bin_f("vmla.f32 d0, d1, d2", d0, d1, i32, f2u(876988654), d2, i32, f2u(1224808797));
    TESTINSN_bin_f("vmla.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(NAN));
    TESTINSN_bin_f("vmla.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(1.0));
    TESTINSN_bin_f("vmla.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(0.0));
    TESTINSN_bin_f("vmla.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(INFINITY));
    TESTINSN_bin_f("vmla.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(-INFINITY));
    TESTINSN_bin_f("vmla.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(NAN));
    TESTINSN_bin_f("vmla.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(1.0));
    TESTINSN_bin_f("vmla.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(0.0));
    TESTINSN_bin_f("vmla.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(INFINITY));
    TESTINSN_bin_f("vmla.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(-INFINITY));
    TESTINSN_bin_f("vmla.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(NAN));
    TESTINSN_bin_f("vmla.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(1.0));
    TESTINSN_bin_f("vmla.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(0.0));
    TESTINSN_bin_f("vmla.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(INFINITY));
    TESTINSN_bin_f("vmla.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(-INFINITY));
    TESTINSN_bin_f("vmla.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(NAN));
    TESTINSN_bin_f("vmla.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(1.0));
    TESTINSN_bin_f("vmla.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(0.0));
    TESTINSN_bin_f("vmla.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(INFINITY));
    TESTINSN_bin_f("vmla.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(-INFINITY));

    printf("---- VMLA (fp by scalar) ----\n");
    TESTINSN_bin_f("vmla.f32 d0, d1, d4[0]", d0, d1, i32, f2u(24), d4, i32, f2u(120));
    TESTINSN_bin_f("vmla.f32 d31, d8, d7[1]", d31, d8, i32, f2u(140), d7, i32, f2u(-120));
    TESTINSN_bin_f("vmla.f32 d4, d8, d15[1]", d4, d8, i32, (1 << 31) + 1, d15, i32, (1 << 31) + 2);
    TESTINSN_bin_f("vmla.f32 d7, d8, d1[1]", d7, d8, i32, (1 << 31), d1, i16, 12);
    TESTINSN_bin_f("vmla.f32 d17, d8, d1[1]", d17, d8, i32, (1 << 31) + 1, d1, i32, (1 << 31) + 2);
    TESTINSN_bin_f("vmla.f32 d7, d8, d1[0]", d7, d8, i32, f2u(1e22), d1, i32, f2u(1e-19));
    TESTINSN_bin_f("vmla.f32 d7, d24, d1[0]", d7, d24, i32, f2u(1e12), d1, i32, f2u(1e11));
    TESTINSN_bin_f("vmla.f32 d0, d1, d2[0]", d0, d1, i32, f2u(NAN), d2, i32, f2u(NAN));
    TESTINSN_bin_f("vmla.f32 d0, d1, d2[0]", d0, d1, i32, f2u(NAN), d2, i32, f2u(1.0));
    TESTINSN_bin_f("vmla.f32 d0, d1, d2[0]", d0, d1, i32, f2u(NAN), d2, i32, f2u(0.0));
    TESTINSN_bin_f("vmla.f32 d0, d1, d2[0]", d0, d1, i32, f2u(NAN), d2, i32, f2u(INFINITY));
    TESTINSN_bin_f("vmla.f32 d0, d1, d2[0]", d0, d1, i32, f2u(NAN), d2, i32, f2u(-INFINITY));
    TESTINSN_bin_f("vmla.f32 d0, d1, d2[0]", d0, d1, i32, f2u(0.0), d2, i32, f2u(NAN));
    TESTINSN_bin_f("vmla.f32 d0, d1, d2[0]", d0, d1, i32, f2u(0.0), d2, i32, f2u(1.0));
    TESTINSN_bin_f("vmla.f32 d0, d1, d2[0]", d0, d1, i32, f2u(0.0), d2, i32, f2u(0.0));
    TESTINSN_bin_f("vmla.f32 d0, d1, d2[0]", d0, d1, i32, f2u(0.0), d2, i32, f2u(INFINITY));
    TESTINSN_bin_f("vmla.f32 d0, d1, d2[0]", d0, d1, i32, f2u(0.0), d2, i32, f2u(-INFINITY));
    TESTINSN_bin_f("vmla.f32 d0, d1, d2[0]", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(NAN));
    TESTINSN_bin_f("vmla.f32 d0, d1, d2[0]", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(1.0));
    TESTINSN_bin_f("vmla.f32 d0, d1, d2[0]", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(0.0));
    TESTINSN_bin_f("vmla.f32 d0, d1, d2[0]", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(INFINITY));
    TESTINSN_bin_f("vmla.f32 d0, d1, d2[0]", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(-INFINITY));
    TESTINSN_bin_f("vmla.f32 d0, d1, d2[0]", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(NAN));
    TESTINSN_bin_f("vmla.f32 d0, d1, d2[0]", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(1.0));
    TESTINSN_bin_f("vmla.f32 d0, d1, d2[0]", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(0.0));
    TESTINSN_bin_f("vmla.f32 d0, d1, d2[0]", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(INFINITY));
    TESTINSN_bin_f("vmla.f32 d0, d1, d2[0]", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(-INFINITY));

    printf("---- VMLS (fp) ----\n");
    TESTINSN_bin_f("vmls.f32 d0, d5, d2", d0, d5, i32, f2u(23.04), d2, i32, f2u(-45.5687));
    TESTINSN_bin_f("vmls.f32 d3, d4, d5", d3, d4, i32, f2u(-347856.475), d5, i32, f2u(1346));
    TESTINSN_bin_f("vmls.f32 d10, d11, d2", d10, d11, i32, f2u(48755), d2, i32, f2u(-45786.476));
    TESTINSN_bin_f("vmls.f32 d9, d5, d7", d9, d5, i32, f2u(95867.76), d7, i32, f2u(17065));
    TESTINSN_bin_f("vmls.f32 d0, d5, d2", d0, d5, i32, f2u(-45667.24), d2, i32, f2u(-248562.76));
    TESTINSN_bin_f("vmls.f32 d3, d4, d5", d3, d4, i32, f2u(24), d5, i32, f2u(1346));
    TESTINSN_bin_f("vmls.f32 d10, d11, d2", d10, d11, i32, f2u(48755), d2, i32, f2u(1089));
    TESTINSN_bin_f("vmls.f32 d9, d5, d7", d9, d5, i32, f2u(214), d7, i32, f2u(1752065));
    TESTINSN_bin_f("vmls.f32 d0, d11, d12", d0, d11, i32, f2u(356047.56), d12, i32, f2u(5867.009));
    TESTINSN_bin_f("vmls.f32 d7, d1, d6", d7, d1, i32, f2u(34.00046), d6, i32, f2u(0.0024575));
    TESTINSN_bin_f("vmls.f32 d0, d1, d2", d0, d1, i32, f2u(2754), d2, i32, f2u(107));
    TESTINSN_bin_f("vmls.f32 d3, d4, d5", d3, d4, i32, f2u(874), d5, i32, f2u(1384.6));
    TESTINSN_bin_f("vmls.f32 d10, d11, d2", d10, d11, i32, f2u(487.587), d2, i32, f2u(109));
    TESTINSN_bin_f("vmls.f32 d9, d5, d7", d9, d5, i32, f2u(2146), d7, i32, f2u(1752));
    TESTINSN_bin_f("vmls.f32 d0, d11, d12", d0, d11, i32, f2u(-56.25), d12, i32, f2u(-5786.47));
    TESTINSN_bin_f("vmls.f32 d7, d1, d6", d7, d1, i32, f2u(456.2489562), d6, i32, f2u(-7.2945676));
    TESTINSN_bin_f("vmls.f32 d0, d5, d2", d0, d5, i32, f2u(532.987), d2, i32, f2u(-0.0045876));
    TESTINSN_bin_f("vmls.f32 d10, d13, d15", d10, d13, i32, f2u(-485.2457), d15, i32, f2u(-567.245));
    TESTINSN_bin_f("vmls.f32 d10, d13, d15", d10, d13, i32, f2u(278456.45), d15, i32, f2u(8756.0076));
    TESTINSN_bin_f("vmls.f32 d0, d1, d2", d0, d1, i32, f2u(876988654), d2, i32, f2u(1224808797));
    TESTINSN_bin_f("vmls.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(NAN));
    TESTINSN_bin_f("vmls.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(1.0));
    TESTINSN_bin_f("vmls.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(0.0));
    TESTINSN_bin_f("vmls.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(INFINITY));
    TESTINSN_bin_f("vmls.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(-INFINITY));
    TESTINSN_bin_f("vmls.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(NAN));
    TESTINSN_bin_f("vmls.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(1.0));
    TESTINSN_bin_f("vmls.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(0.0));
    TESTINSN_bin_f("vmls.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(INFINITY));
    TESTINSN_bin_f("vmls.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(-INFINITY));
    TESTINSN_bin_f("vmls.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(NAN));
    TESTINSN_bin_f("vmls.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(1.0));
    TESTINSN_bin_f("vmls.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(0.0));
    TESTINSN_bin_f("vmls.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(INFINITY));
    TESTINSN_bin_f("vmls.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(-INFINITY));
    TESTINSN_bin_f("vmls.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(NAN));
    TESTINSN_bin_f("vmls.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(1.0));
    TESTINSN_bin_f("vmls.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(0.0));
    TESTINSN_bin_f("vmls.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(INFINITY));
    TESTINSN_bin_f("vmls.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(-INFINITY));

    printf("---- VMLS (fp by scalar) ----\n");
    TESTINSN_bin_f("vmls.f32 d0, d1, d4[0]", d0, d1, i32, f2u(24), d4, i32, f2u(120));
    TESTINSN_bin_f("vmls.f32 d31, d8, d7[1]", d31, d8, i32, f2u(140), d7, i32, f2u(-120));
    TESTINSN_bin_f("vmls.f32 d4, d8, d15[1]", d4, d8, i32, (1 << 31) + 1, d15, i32, (1 << 31) + 2);
    TESTINSN_bin_f("vmls.f32 d7, d8, d1[1]", d7, d8, i32, (1 << 31), d1, i16, 12);
    TESTINSN_bin_f("vmls.f32 d17, d8, d1[1]", d17, d8, i32, (1 << 31) + 1, d1, i32, (1 << 31) + 2);
    TESTINSN_bin_f("vmls.f32 d7, d8, d1[0]", d7, d8, i32, f2u(1e22), d1, i32, f2u(1e-19));
    TESTINSN_bin_f("vmls.f32 d7, d24, d1[0]", d7, d24, i32, f2u(1e12), d1, i32, f2u(1e11));
    TESTINSN_bin_f("vmls.f32 d0, d1, d2[0]", d0, d1, i32, f2u(NAN), d2, i32, f2u(NAN));
    TESTINSN_bin_f("vmls.f32 d0, d1, d2[0]", d0, d1, i32, f2u(NAN), d2, i32, f2u(1.0));
    TESTINSN_bin_f("vmls.f32 d0, d1, d2[0]", d0, d1, i32, f2u(NAN), d2, i32, f2u(0.0));
    TESTINSN_bin_f("vmls.f32 d0, d1, d2[0]", d0, d1, i32, f2u(NAN), d2, i32, f2u(INFINITY));
    TESTINSN_bin_f("vmls.f32 d0, d1, d2[0]", d0, d1, i32, f2u(NAN), d2, i32, f2u(-INFINITY));
    TESTINSN_bin_f("vmls.f32 d0, d1, d2[0]", d0, d1, i32, f2u(0.0), d2, i32, f2u(NAN));
    TESTINSN_bin_f("vmls.f32 d0, d1, d2[0]", d0, d1, i32, f2u(0.0), d2, i32, f2u(1.0));
    TESTINSN_bin_f("vmls.f32 d0, d1, d2[0]", d0, d1, i32, f2u(0.0), d2, i32, f2u(0.0));
    TESTINSN_bin_f("vmls.f32 d0, d1, d2[0]", d0, d1, i32, f2u(0.0), d2, i32, f2u(INFINITY));
    TESTINSN_bin_f("vmls.f32 d0, d1, d2[0]", d0, d1, i32, f2u(0.0), d2, i32, f2u(-INFINITY));
    TESTINSN_bin_f("vmls.f32 d0, d1, d2[0]", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(NAN));
    TESTINSN_bin_f("vmls.f32 d0, d1, d2[0]", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(1.0));
    TESTINSN_bin_f("vmls.f32 d0, d1, d2[0]", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(0.0));
    TESTINSN_bin_f("vmls.f32 d0, d1, d2[0]", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(INFINITY));
    TESTINSN_bin_f("vmls.f32 d0, d1, d2[0]", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(-INFINITY));
    TESTINSN_bin_f("vmls.f32 d0, d1, d2[0]", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(NAN));
    TESTINSN_bin_f("vmls.f32 d0, d1, d2[0]", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(1.0));
    TESTINSN_bin_f("vmls.f32 d0, d1, d2[0]", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(0.0));
    TESTINSN_bin_f("vmls.f32 d0, d1, d2[0]", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(INFINITY));
    TESTINSN_bin_f("vmls.f32 d0, d1, d2[0]", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(-INFINITY));

    printf("---- VABD (fp) ----\n");
    TESTINSN_bin("vabd.f32 d0, d5, d2", d0, d5, i32, f2u(23.04), d2, i32, f2u(-45.5687));
    TESTINSN_bin("vabd.f32 d3, d4, d5", d3, d4, i32, f2u(-347856.475), d5, i32, f2u(1346));
    TESTINSN_bin("vabd.f32 d10, d11, d2", d10, d11, i32, f2u(48755), d2, i32, f2u(-45786.476));
    TESTINSN_bin("vabd.f32 d9, d5, d7", d9, d5, i32, f2u(95867.76), d7, i32, f2u(17065));
    TESTINSN_bin("vabd.f32 d0, d5, d2", d0, d5, i32, f2u(-45667.24), d2, i32, f2u(-248562.76));
    TESTINSN_bin("vabd.f32 d3, d4, d5", d3, d4, i32, f2u(24), d5, i32, f2u(1346));
    TESTINSN_bin("vabd.f32 d10, d11, d2", d10, d11, i32, f2u(48755), d2, i32, f2u(1089));
    TESTINSN_bin("vabd.f32 d9, d5, d7", d9, d5, i32, f2u(214), d7, i32, f2u(1752065));
    TESTINSN_bin("vabd.f32 d0, d11, d12", d0, d11, i32, f2u(356047.56), d12, i32, f2u(5867.009));
    TESTINSN_bin("vabd.f32 d7, d1, d6", d7, d1, i32, f2u(34.00046), d6, i32, f2u(0.0024575));
    TESTINSN_bin("vabd.f32 d0, d1, d2", d0, d1, i32, f2u(2754), d2, i32, f2u(107));
    TESTINSN_bin("vabd.f32 d3, d4, d5", d3, d4, i32, f2u(874), d5, i32, f2u(1384.6));
    TESTINSN_bin("vabd.f32 d10, d11, d2", d10, d11, i32, f2u(487.587), d2, i32, f2u(109));
    TESTINSN_bin("vabd.f32 d9, d5, d7", d9, d5, i32, f2u(2146), d7, i32, f2u(1752));
    TESTINSN_bin("vabd.f32 d0, d11, d12", d0, d11, i32, f2u(-56.25), d12, i32, f2u(-5786.47));
    TESTINSN_bin("vabd.f32 d7, d1, d6", d7, d1, i32, f2u(456.2489562), d6, i32, f2u(-7.2945676));
    TESTINSN_bin("vabd.f32 d0, d5, d2", d0, d5, i32, f2u(532.987), d2, i32, f2u(-0.0045876));
    TESTINSN_bin("vabd.f32 d10, d13, d15", d10, d13, i32, f2u(-485.2457), d15, i32, f2u(-567.245));
    TESTINSN_bin("vabd.f32 d10, d13, d15", d10, d13, i32, f2u(278456.45), d15, i32, f2u(8756.0076));
    TESTINSN_bin("vabd.f32 d0, d1, d2", d0, d1, i32, f2u(876988654), d2, i32, f2u(1224808797));

    printf("---- VPADD (fp) ----\n");
    TESTINSN_bin("vpadd.f32 d0, d5, d2", d0, d5, i32, f2u(23.04), d2, i32, f2u(-45.5687));
    TESTINSN_bin("vpadd.f32 d3, d4, d5", d3, d4, i32, f2u(-347856.475), d5, i32, f2u(1346));
    TESTINSN_bin("vpadd.f32 d10, d11, d2", d10, d11, i32, f2u(48755), d2, i32, f2u(-45786.476));
    TESTINSN_bin("vpadd.f32 d9, d5, d7", d9, d5, i32, f2u(95867.76), d7, i32, f2u(17065));
    TESTINSN_bin("vpadd.f32 d0, d5, d2", d0, d5, i32, f2u(-45667.24), d2, i32, f2u(-248562.76));
    TESTINSN_bin("vpadd.f32 d3, d4, d5", d3, d4, i32, f2u(24), d5, i32, f2u(1346));
    TESTINSN_bin("vpadd.f32 d10, d11, d2", d10, d11, i32, f2u(48755), d2, i32, f2u(1089));
    TESTINSN_bin("vpadd.f32 d9, d5, d7", d9, d5, i32, f2u(214), d7, i32, f2u(1752065));
    TESTINSN_bin("vpadd.f32 d0, d11, d12", d0, d11, i32, f2u(356047.56), d12, i32, f2u(5867.009));
    TESTINSN_bin("vpadd.f32 d7, d1, d6", d7, d1, i32, f2u(34.00046), d6, i32, f2u(0.0024575));
    TESTINSN_bin("vpadd.f32 d0, d1, d2", d0, d1, i32, f2u(2754), d2, i32, f2u(107));
    TESTINSN_bin("vpadd.f32 d3, d4, d5", d3, d4, i32, f2u(874), d5, i32, f2u(1384.6));
    TESTINSN_bin("vpadd.f32 d10, d11, d2", d10, d11, i32, f2u(487.587), d2, i32, f2u(109));
    TESTINSN_bin("vpadd.f32 d9, d5, d7", d9, d5, i32, f2u(2146), d7, i32, f2u(1752));
    TESTINSN_bin("vpadd.f32 d0, d11, d12", d0, d11, i32, f2u(-56.25), d12, i32, f2u(-5786.47));
    TESTINSN_bin("vpadd.f32 d7, d1, d6", d7, d1, i32, f2u(456.2489562), d6, i32, f2u(-7.2945676));
    TESTINSN_bin("vpadd.f32 d0, d5, d2", d0, d5, i32, f2u(532.987), d2, i32, f2u(-0.0045876));
    TESTINSN_bin("vpadd.f32 d10, d13, d15", d10, d13, i32, f2u(-485.2457), d15, i32, f2u(-567.245));
    TESTINSN_bin("vpadd.f32 d10, d13, d15", d10, d13, i32, f2u(278456.45), d15, i32, f2u(8756.0076));
    TESTINSN_bin("vpadd.f32 d0, d1, d2", d0, d1, i32, f2u(876988654), d2, i32, f2u(1224808797));
    TESTINSN_bin("vpadd.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(NAN));
    TESTINSN_bin("vpadd.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(1.0));
    TESTINSN_bin("vpadd.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(0.0));
    TESTINSN_bin("vpadd.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(INFINITY));
    TESTINSN_bin("vpadd.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(-INFINITY));
    TESTINSN_bin("vpadd.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(NAN));
    TESTINSN_bin("vpadd.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(1.0));
    TESTINSN_bin("vpadd.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(0.0));
    TESTINSN_bin("vpadd.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(INFINITY));
    TESTINSN_bin("vpadd.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(-INFINITY));
    TESTINSN_bin("vpadd.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(NAN));
    TESTINSN_bin("vpadd.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(1.0));
    TESTINSN_bin("vpadd.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(0.0));
    TESTINSN_bin("vpadd.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(INFINITY));
    TESTINSN_bin("vpadd.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(-INFINITY));
    TESTINSN_bin("vpadd.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(NAN));
    TESTINSN_bin("vpadd.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(1.0));
    TESTINSN_bin("vpadd.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(0.0));
    TESTINSN_bin("vpadd.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(INFINITY));
    TESTINSN_bin("vpadd.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(-INFINITY));

    printf("---- VCVT (integer <-> fp) ----\n");
    TESTINSN_un("vcvt.u32.f32 d0, d1", d0, d1, i32, f2u(3.2));
    TESTINSN_un("vcvt.u32.f32 d10, d11", d10, d11, i32, f2u(3e22));
    TESTINSN_un("vcvt.u32.f32 d15, d4", d15, d4, i32, f2u(3e9));
    TESTINSN_un("vcvt.u32.f32 d15, d4", d15, d4, i32, f2u(-0.5));
    TESTINSN_un("vcvt.u32.f32 d15, d4", d15, d4, i32, f2u(-7.1));
    TESTINSN_un("vcvt.u32.f32 d12, d8", d12, d8, i32, f2u(8.0 - 1.0/1024.0));
    TESTINSN_un("vcvt.u32.f32 d12, d8", d12, d8, i32, f2u(-8.0 + 1.0/1024.0));
    TESTINSN_un("vcvt.s32.f32 d0, d1", d0, d1, i32, f2u(3.2));
    TESTINSN_un("vcvt.s32.f32 d20, d21", d20, d21, i32, f2u(3e22));
    TESTINSN_un("vcvt.s32.f32 d15, d4", d15, d4, i32, f2u(3e9));
    TESTINSN_un("vcvt.s32.f32 d15, d4", d15, d4, i32, f2u(-0.5));
    TESTINSN_un("vcvt.s32.f32 d15, d4", d15, d4, i32, f2u(-7.1));
    TESTINSN_un("vcvt.s32.f32 d12, d8", d12, d8, i32, f2u(8.0 - 1.0/1024.0));
    TESTINSN_un("vcvt.s32.f32 d12, d8", d12, d8, i32, f2u(-8.0 + 1.0/1024.0));
    TESTINSN_un("vcvt.f32.u32 d0, d1", d0, d1, i32, 7);
    TESTINSN_un("vcvt.f32.u32 d10, d11", d10, d11, i32, 1 << 31);
    TESTINSN_un("vcvt.f32.u32 d0, d1", d0, d1, i32, (1U << 31) + 1);
    TESTINSN_un("vcvt.f32.u32 d24, d26", d24, d26, i32, (1U << 31) - 1);
    TESTINSN_un("vcvt.f32.u32 d0, d14", d0, d14, i32, 0x30a0bcef);
    TESTINSN_un("vcvt.f32.s32 d0, d1", d0, d1, i32, 7);
    TESTINSN_un("vcvt.f32.s32 d30, d31", d30, d31, i32, 1 << 31);
    TESTINSN_un("vcvt.f32.s32 d0, d1", d0, d1, i32, (1U << 31) + 1);
    TESTINSN_un("vcvt.f32.s32 d0, d1", d0, d1, i32, (1U << 31) - 1);
    TESTINSN_un("vcvt.u32.f32 d0, d1", d0, d1, i32, f2u(NAN));
    TESTINSN_un("vcvt.u32.f32 d0, d1", d0, d1, i32, f2u(0.0));
    TESTINSN_un("vcvt.u32.f32 d0, d1", d0, d1, i32, f2u(INFINITY));
    TESTINSN_un("vcvt.u32.f32 d0, d1", d0, d1, i32, f2u(-INFINITY));
    TESTINSN_un("vcvt.s32.f32 d0, d1", d0, d1, i32, f2u(NAN));
    TESTINSN_un("vcvt.s32.f32 d0, d1", d0, d1, i32, f2u(0.0));
    TESTINSN_un("vcvt.s32.f32 d0, d1", d0, d1, i32, f2u(INFINITY));
    TESTINSN_un("vcvt.s32.f32 d0, d1", d0, d1, i32, f2u(-INFINITY));

    printf("---- VCVT (fixed <-> fp) ----\n");
    TESTINSN_un("vcvt.u32.f32 d0, d1, #3", d0, d1, i32, f2u(3.2));
    TESTINSN_un("vcvt.u32.f32 d10, d11, #1", d10, d11, i32, f2u(3e22));
    TESTINSN_un("vcvt.u32.f32 d15, d4, #32", d15, d4, i32, f2u(3e9));
    TESTINSN_un("vcvt.u32.f32 d15, d4, #7", d15, d4, i32, f2u(-0.5));
    TESTINSN_un("vcvt.u32.f32 d15, d4, #4", d15, d4, i32, f2u(-7.1));
    TESTINSN_un("vcvt.u32.f32 d12, d8, #3", d12, d8, i32, f2u(8.0 - 1.0/1024.0));
    TESTINSN_un("vcvt.u32.f32 d12, d8, #3", d12, d8, i32, f2u(-8.0 + 1.0/1024.0));
    TESTINSN_un("vcvt.s32.f32 d0, d1, #5", d0, d1, i32, f2u(3.2));
    TESTINSN_un("vcvt.s32.f32 d20, d21, #1", d20, d21, i32, f2u(3e22));
    TESTINSN_un("vcvt.s32.f32 d15, d4, #8", d15, d4, i32, f2u(3e9));
    TESTINSN_un("vcvt.s32.f32 d15, d4, #2", d15, d4, i32, f2u(-0.5));
    TESTINSN_un("vcvt.s32.f32 d15, d4, #1", d15, d4, i32, f2u(-7.1));
    TESTINSN_un("vcvt.s32.f32 d12, d8, #2", d12, d8, i32, f2u(8.0 - 1.0/1024.0));
    TESTINSN_un("vcvt.s32.f32 d12, d8, #2", d12, d8, i32, f2u(-8.0 + 1.0/1024.0));
    TESTINSN_un("vcvt.f32.u32 d0, d1, #5", d0, d1, i32, 7);
    TESTINSN_un("vcvt.f32.u32 d10, d11, #9", d10, d11, i32, 1 << 31);
    TESTINSN_un("vcvt.f32.u32 d0, d1, #4", d0, d1, i32, (1U << 31) + 1);
    TESTINSN_un("vcvt.f32.u32 d24, d26, #6", d24, d26, i32, (1U << 31) - 1);
    TESTINSN_un("vcvt.f32.u32 d0, d14, #5", d0, d14, i32, 0x30a0bcef);
    TESTINSN_un("vcvt.f32.s32 d0, d1, #12", d0, d1, i32, 7);
    TESTINSN_un("vcvt.f32.s32 d30, d31, #8", d30, d31, i32, 1 << 31);
    TESTINSN_un("vcvt.f32.s32 d0, d1, #1", d0, d1, i32, (1U << 31) + 1);
    TESTINSN_un("vcvt.f32.s32 d0, d1, #6", d0, d1, i32, (1U << 31) - 1);
    TESTINSN_un("vcvt.f32.s32 d0, d14, #2", d0, d14, i32, 0x30a0bcef);
    TESTINSN_un("vcvt.u32.f32 d0, d1, #3", d0, d1, i32, f2u(NAN));
    TESTINSN_un("vcvt.u32.f32 d0, d1, #3", d0, d1, i32, f2u(0.0));
    TESTINSN_un("vcvt.u32.f32 d0, d1, #3", d0, d1, i32, f2u(INFINITY));
    TESTINSN_un("vcvt.u32.f32 d0, d1, #3", d0, d1, i32, f2u(-INFINITY));
    TESTINSN_un("vcvt.s32.f32 d0, d1, #3", d0, d1, i32, f2u(NAN));
    TESTINSN_un("vcvt.s32.f32 d0, d1, #3", d0, d1, i32, f2u(0.0));
    TESTINSN_un("vcvt.s32.f32 d0, d1, #3", d0, d1, i32, f2u(INFINITY));
    TESTINSN_un("vcvt.s32.f32 d0, d1, #3", d0, d1, i32, f2u(-INFINITY));

    printf("---- VMAX (fp) ----\n");
    TESTINSN_bin("vmax.f32 d0, d5, d2", d0, d5, i32, f2u(23.04), d2, i32, f2u(-45.5687));
    TESTINSN_bin("vmax.f32 d3, d4, d5", d3, d4, i32, f2u(-347856.475), d5, i32, f2u(1346));
    TESTINSN_bin("vmax.f32 d10, d11, d2", d10, d11, i32, f2u(48755), d2, i32, f2u(-45786.476));
    TESTINSN_bin("vmax.f32 d9, d5, d7", d9, d5, i32, f2u(95867.76), d7, i32, f2u(17065));
    TESTINSN_bin("vmax.f32 d0, d5, d2", d0, d5, i32, f2u(-45667.24), d2, i32, f2u(-248562.76));
    TESTINSN_bin("vmax.f32 d3, d4, d5", d3, d4, i32, f2u(24.87556), d5, i32, f2u(1346.0004));
    TESTINSN_bin("vmax.f32 d10, d11, d2", d10, d11, i32, f2u(48755.7), d2, i32, f2u(1089.2));
    TESTINSN_bin("vmax.f32 d9, d5, d7", d9, d5, i32, f2u(214), d7, i32, f2u(1752065));
    TESTINSN_bin("vmax.f32 d0, d11, d12", d0, d11, i32, f2u(356047.56), d12, i32, f2u(5867.009));
    TESTINSN_bin("vmax.f32 d7, d1, d6", d7, d1, i32, f2u(34.00046), d6, i32, f2u(0.0024575));
    TESTINSN_bin("vmax.f32 d0, d1, d2", d0, d1, i32, f2u(2754), d2, i32, f2u(107));
    TESTINSN_bin("vmax.f32 d3, d4, d5", d3, d4, i32, f2u(874), d5, i32, f2u(1384.6));
    TESTINSN_bin("vmax.f32 d10, d11, d2", d10, d11, i32, f2u(487.587), d2, i32, f2u(109));
    TESTINSN_bin("vmax.f32 d9, d5, d7", d9, d5, i32, f2u(2146), d7, i32, f2u(1752));
    TESTINSN_bin("vmax.f32 d0, d11, d12", d0, d11, i32, f2u(-56.25), d12, i32, f2u(-5786.47));
    TESTINSN_bin("vmax.f32 d7, d1, d6", d7, d1, i32, f2u(456.2489562), d6, i32, f2u(-7.2945676));
    TESTINSN_bin("vmax.f32 d0, d5, d2", d0, d5, i32, f2u(532.987), d2, i32, f2u(-0.0045876));
    TESTINSN_bin("vmax.f32 d10, d13, d15", d10, d13, i32, f2u(-485.2457), d15, i32, f2u(-567.245));
    TESTINSN_bin("vmax.f32 d10, d13, d15", d10, d13, i32, f2u(278456.45), d15, i32, f2u(8756.0076));
    TESTINSN_bin("vmax.f32 d0, d1, d2", d0, d1, i32, f2u(876988654), d2, i32, f2u(1224808797));
    TESTINSN_bin("vmax.f32 d0, d1, d2", d0, d1, i32, f2u(0), d2, i32, f2u(0));
    TESTINSN_bin("vmax.f32 d0, d1, d2", d0, d1, i32, f2u(1.0/1024.0), d2, i32, f2u(-1.0/1024.0));
    TESTINSN_bin("vmax.f32 d0, d1, d2", d0, d1, i32, f2u(-1.0/1024.0), d2, i32, f2u(1.0/1024.0));
    TESTINSN_bin("vmax.f32 d0, d1, d2", d0, d1, i32, f2u(2342+1.0/1024.0), d2, i32, f2u(2342-1.0/1024.0));
    TESTINSN_bin("vmax.f32 d0, d1, d2", d0, d1, i32, f2u(-2342+1.0/1024.0), d2, i32, f2u(-2342-1.0/1024.0));
    TESTINSN_bin("vmax.f32 d0, d1, d2", d0, d1, i32, f2u(89276+1.0/1024.0), d2, i32, f2u(98276+1.0/1024.0));
    TESTINSN_bin("vmax.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(NAN));
    TESTINSN_bin("vmax.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(1.0));
    TESTINSN_bin("vmax.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(0.0));
    TESTINSN_bin("vmax.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(INFINITY));
    TESTINSN_bin("vmax.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(-INFINITY));
    TESTINSN_bin("vmax.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(NAN));
    TESTINSN_bin("vmax.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(1.0));
    TESTINSN_bin("vmax.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(0.0));
    TESTINSN_bin("vmax.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(INFINITY));
    TESTINSN_bin("vmax.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(-INFINITY));
    TESTINSN_bin("vmax.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(NAN));
    TESTINSN_bin("vmax.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(1.0));
    TESTINSN_bin("vmax.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(0.0));
    TESTINSN_bin("vmax.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(INFINITY));
    TESTINSN_bin("vmax.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(-INFINITY));
    TESTINSN_bin("vmax.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(NAN));
    TESTINSN_bin("vmax.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(1.0));
    TESTINSN_bin("vmax.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(0.0));
    TESTINSN_bin("vmax.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(INFINITY));
    TESTINSN_bin("vmax.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(-INFINITY));

    printf("---- VMIN (fp) ----\n");
    TESTINSN_bin("vmin.f32 d0, d5, d2", d0, d5, i32, f2u(23.04), d2, i32, f2u(-45.5687));
    TESTINSN_bin("vmin.f32 d3, d4, d5", d3, d4, i32, f2u(-347856.475), d5, i32, f2u(1346));
    TESTINSN_bin("vmin.f32 d10, d11, d2", d10, d11, i32, f2u(48755), d2, i32, f2u(-45786.476));
    TESTINSN_bin("vmin.f32 d9, d5, d7", d9, d5, i32, f2u(95867.76), d7, i32, f2u(17065));
    TESTINSN_bin("vmin.f32 d0, d5, d2", d0, d5, i32, f2u(-45667.24), d2, i32, f2u(-248562.76));
    TESTINSN_bin("vmin.f32 d3, d4, d5", d3, d4, i32, f2u(24.87556), d5, i32, f2u(1346.0004));
    TESTINSN_bin("vmin.f32 d10, d11, d2", d10, d11, i32, f2u(48755.7), d2, i32, f2u(1089.2));
    TESTINSN_bin("vmin.f32 d9, d5, d7", d9, d5, i32, f2u(214), d7, i32, f2u(1752065));
    TESTINSN_bin("vmin.f32 d0, d11, d12", d0, d11, i32, f2u(356047.56), d12, i32, f2u(5867.009));
    TESTINSN_bin("vmin.f32 d7, d1, d6", d7, d1, i32, f2u(34.00046), d6, i32, f2u(0.0024575));
    TESTINSN_bin("vmin.f32 d0, d1, d2", d0, d1, i32, f2u(2754), d2, i32, f2u(107));
    TESTINSN_bin("vmin.f32 d3, d4, d5", d3, d4, i32, f2u(874), d5, i32, f2u(1384.6));
    TESTINSN_bin("vmin.f32 d10, d11, d2", d10, d11, i32, f2u(487.587), d2, i32, f2u(109));
    TESTINSN_bin("vmin.f32 d9, d5, d7", d9, d5, i32, f2u(2146), d7, i32, f2u(1752));
    TESTINSN_bin("vmin.f32 d0, d11, d12", d0, d11, i32, f2u(-56.25), d12, i32, f2u(-5786.47));
    TESTINSN_bin("vmin.f32 d7, d1, d6", d7, d1, i32, f2u(456.2489562), d6, i32, f2u(-7.2945676));
    TESTINSN_bin("vmin.f32 d0, d5, d2", d0, d5, i32, f2u(532.987), d2, i32, f2u(-0.0045876));
    TESTINSN_bin("vmin.f32 d10, d13, d15", d10, d13, i32, f2u(-485.2457), d15, i32, f2u(-567.245));
    TESTINSN_bin("vmin.f32 d10, d13, d15", d10, d13, i32, f2u(278456.45), d15, i32, f2u(8756.0076));
    TESTINSN_bin("vmin.f32 d0, d1, d2", d0, d1, i32, f2u(876988654), d2, i32, f2u(1224808797));
    TESTINSN_bin("vmin.f32 d0, d1, d2", d0, d1, i32, f2u(0), d2, i32, f2u(0));
    TESTINSN_bin("vmin.f32 d0, d1, d2", d0, d1, i32, f2u(1.0/1024.0), d2, i32, f2u(-1.0/1024.0));
    TESTINSN_bin("vmin.f32 d0, d1, d2", d0, d1, i32, f2u(-1.0/1024.0), d2, i32, f2u(1.0/1024.0));
    TESTINSN_bin("vmin.f32 d0, d1, d2", d0, d1, i32, f2u(2342+1.0/1024.0), d2, i32, f2u(2342-1.0/1024.0));
    TESTINSN_bin("vmin.f32 d0, d1, d2", d0, d1, i32, f2u(-2342+1.0/1024.0), d2, i32, f2u(-2342-1.0/1024.0));
    TESTINSN_bin("vmin.f32 d0, d1, d2", d0, d1, i32, f2u(89276+1.0/1024.0), d2, i32, f2u(98276+1.0/1024.0));
    TESTINSN_bin("vmin.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(NAN));
    TESTINSN_bin("vmin.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(1.0));
    TESTINSN_bin("vmin.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(0.0));
    TESTINSN_bin("vmin.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(INFINITY));
    TESTINSN_bin("vmin.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(-INFINITY));
    TESTINSN_bin("vmin.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(NAN));
    TESTINSN_bin("vmin.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(1.0));
    TESTINSN_bin("vmin.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(0.0));
    TESTINSN_bin("vmin.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(INFINITY));
    TESTINSN_bin("vmin.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(-INFINITY));
    TESTINSN_bin("vmin.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(NAN));
    TESTINSN_bin("vmin.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(1.0));
    TESTINSN_bin("vmin.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(0.0));
    TESTINSN_bin("vmin.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(INFINITY));
    TESTINSN_bin("vmin.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(-INFINITY));
    TESTINSN_bin("vmin.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(NAN));
    TESTINSN_bin("vmin.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(1.0));
    TESTINSN_bin("vmin.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(0.0));
    TESTINSN_bin("vmin.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(INFINITY));
    TESTINSN_bin("vmin.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(-INFINITY));

    printf("---- VPMAX (fp) ----\n");
    TESTINSN_bin("vpmax.f32 d0, d5, d2", d0, d5, i32, f2u(23.04), d2, i32, f2u(-45.5687));
    TESTINSN_bin("vpmax.f32 d3, d4, d5", d3, d4, i32, f2u(-347856.475), d5, i32, f2u(1346));
    TESTINSN_bin("vpmax.f32 d10, d11, d2", d10, d11, i32, f2u(48755), d2, i32, f2u(-45786.476));
    TESTINSN_bin("vpmax.f32 d9, d5, d7", d9, d5, i32, f2u(95867.76), d7, i32, f2u(17065));
    TESTINSN_bin("vpmax.f32 d0, d5, d2", d0, d5, i32, f2u(-45667.24), d2, i32, f2u(-248562.76));
    TESTINSN_bin("vpmax.f32 d3, d4, d5", d3, d4, i32, f2u(24.87556), d5, i32, f2u(1346.0004));
    TESTINSN_bin("vpmax.f32 d10, d11, d2", d10, d11, i32, f2u(48755.7), d2, i32, f2u(1089.2));
    TESTINSN_bin("vpmax.f32 d9, d5, d7", d9, d5, i32, f2u(214), d7, i32, f2u(1752065));
    TESTINSN_bin("vpmax.f32 d0, d11, d12", d0, d11, i32, f2u(356047.56), d12, i32, f2u(5867.009));
    TESTINSN_bin("vpmax.f32 d7, d1, d6", d7, d1, i32, f2u(34.00046), d6, i32, f2u(0.0024575));
    TESTINSN_bin("vpmax.f32 d0, d1, d2", d0, d1, i32, f2u(2754), d2, i32, f2u(107));
    TESTINSN_bin("vpmax.f32 d3, d4, d5", d3, d4, i32, f2u(874), d5, i32, f2u(1384.6));
    TESTINSN_bin("vpmax.f32 d10, d11, d2", d10, d11, i32, f2u(487.587), d2, i32, f2u(109));
    TESTINSN_bin("vpmax.f32 d9, d5, d7", d9, d5, i32, f2u(2146), d7, i32, f2u(1752));
    TESTINSN_bin("vpmax.f32 d0, d11, d12", d0, d11, i32, f2u(-56.25), d12, i32, f2u(-5786.47));
    TESTINSN_bin("vpmax.f32 d7, d1, d6", d7, d1, i32, f2u(456.2489562), d6, i32, f2u(-7.2945676));
    TESTINSN_bin("vpmax.f32 d0, d5, d2", d0, d5, i32, f2u(532.987), d2, i32, f2u(-0.0045876));
    TESTINSN_bin("vpmax.f32 d10, d13, d15", d10, d13, i32, f2u(-485.2457), d15, i32, f2u(-567.245));
    TESTINSN_bin("vpmax.f32 d10, d13, d15", d10, d13, i32, f2u(278456.45), d15, i32, f2u(8756.0076));
    TESTINSN_bin("vpmax.f32 d0, d1, d2", d0, d1, i32, f2u(876988654), d2, i32, f2u(1224808797));
    TESTINSN_bin("vpmax.f32 d0, d1, d2", d0, d1, i32, f2u(0), d2, i32, f2u(0));
    TESTINSN_bin("vpmax.f32 d0, d1, d2", d0, d1, i32, f2u(1.0/1024.0), d2, i32, f2u(-1.0/1024.0));
    TESTINSN_bin("vpmax.f32 d0, d1, d2", d0, d1, i32, f2u(-1.0/1024.0), d2, i32, f2u(1.0/1024.0));
    TESTINSN_bin("vpmax.f32 d0, d1, d2", d0, d1, i32, f2u(2342+1.0/1024.0), d2, i32, f2u(2342-1.0/1024.0));
    TESTINSN_bin("vpmax.f32 d0, d1, d2", d0, d1, i32, f2u(-2342+1.0/1024.0), d2, i32, f2u(-2342-1.0/1024.0));
    TESTINSN_bin("vpmax.f32 d0, d1, d2", d0, d1, i32, f2u(89276+1.0/1024.0), d2, i32, f2u(98276+1.0/1024.0));
    TESTINSN_bin("vpmax.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(NAN));
    TESTINSN_bin("vpmax.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(1.0));
    TESTINSN_bin("vpmax.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(0.0));
    TESTINSN_bin("vpmax.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(INFINITY));
    TESTINSN_bin("vpmax.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(-INFINITY));
    TESTINSN_bin("vpmax.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(NAN));
    TESTINSN_bin("vpmax.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(1.0));
    TESTINSN_bin("vpmax.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(0.0));
    TESTINSN_bin("vpmax.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(INFINITY));
    TESTINSN_bin("vpmax.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(-INFINITY));
    TESTINSN_bin("vpmax.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(NAN));
    TESTINSN_bin("vpmax.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(1.0));
    TESTINSN_bin("vpmax.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(0.0));
    TESTINSN_bin("vpmax.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(INFINITY));
    TESTINSN_bin("vpmax.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(-INFINITY));
    TESTINSN_bin("vpmax.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(NAN));
    TESTINSN_bin("vpmax.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(1.0));
    TESTINSN_bin("vpmax.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(0.0));
    TESTINSN_bin("vpmax.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(INFINITY));
    TESTINSN_bin("vpmax.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(-INFINITY));

    printf("---- VPMIN (fp) ----\n");
    TESTINSN_bin("vpmin.f32 d0, d5, d2", d0, d5, i32, f2u(23.04), d2, i32, f2u(-45.5687));
    TESTINSN_bin("vpmin.f32 d3, d4, d5", d3, d4, i32, f2u(-347856.475), d5, i32, f2u(1346));
    TESTINSN_bin("vpmin.f32 d10, d11, d2", d10, d11, i32, f2u(48755), d2, i32, f2u(-45786.476));
    TESTINSN_bin("vpmin.f32 d9, d5, d7", d9, d5, i32, f2u(95867.76), d7, i32, f2u(17065));
    TESTINSN_bin("vpmin.f32 d0, d5, d2", d0, d5, i32, f2u(-45667.24), d2, i32, f2u(-248562.76));
    TESTINSN_bin("vpmin.f32 d3, d4, d5", d3, d4, i32, f2u(24.87556), d5, i32, f2u(1346.0004));
    TESTINSN_bin("vpmin.f32 d10, d11, d2", d10, d11, i32, f2u(48755.7), d2, i32, f2u(1089.2));
    TESTINSN_bin("vpmin.f32 d9, d5, d7", d9, d5, i32, f2u(214), d7, i32, f2u(1752065));
    TESTINSN_bin("vpmin.f32 d0, d11, d12", d0, d11, i32, f2u(356047.56), d12, i32, f2u(5867.009));
    TESTINSN_bin("vpmin.f32 d7, d1, d6", d7, d1, i32, f2u(34.00046), d6, i32, f2u(0.0024575));
    TESTINSN_bin("vpmin.f32 d0, d1, d2", d0, d1, i32, f2u(2754), d2, i32, f2u(107));
    TESTINSN_bin("vpmin.f32 d3, d4, d5", d3, d4, i32, f2u(874), d5, i32, f2u(1384.6));
    TESTINSN_bin("vpmin.f32 d10, d11, d2", d10, d11, i32, f2u(487.587), d2, i32, f2u(109));
    TESTINSN_bin("vpmin.f32 d9, d5, d7", d9, d5, i32, f2u(2146), d7, i32, f2u(1752));
    TESTINSN_bin("vpmin.f32 d0, d11, d12", d0, d11, i32, f2u(-56.25), d12, i32, f2u(-5786.47));
    TESTINSN_bin("vpmin.f32 d7, d1, d6", d7, d1, i32, f2u(456.2489562), d6, i32, f2u(-7.2945676));
    TESTINSN_bin("vpmin.f32 d0, d5, d2", d0, d5, i32, f2u(532.987), d2, i32, f2u(-0.0045876));
    TESTINSN_bin("vpmin.f32 d10, d13, d15", d10, d13, i32, f2u(-485.2457), d15, i32, f2u(-567.245));
    TESTINSN_bin("vpmin.f32 d10, d13, d15", d10, d13, i32, f2u(278456.45), d15, i32, f2u(8756.0076));
    TESTINSN_bin("vpmin.f32 d0, d1, d2", d0, d1, i32, f2u(876988654), d2, i32, f2u(1224808797));
    TESTINSN_bin("vpmin.f32 d0, d1, d2", d0, d1, i32, f2u(0), d2, i32, f2u(0));
    TESTINSN_bin("vpmin.f32 d0, d1, d2", d0, d1, i32, f2u(1.0/1024.0), d2, i32, f2u(-1.0/1024.0));
    TESTINSN_bin("vpmin.f32 d0, d1, d2", d0, d1, i32, f2u(-1.0/1024.0), d2, i32, f2u(1.0/1024.0));
    TESTINSN_bin("vpmin.f32 d0, d1, d2", d0, d1, i32, f2u(2342+1.0/1024.0), d2, i32, f2u(2342-1.0/1024.0));
    TESTINSN_bin("vpmin.f32 d0, d1, d2", d0, d1, i32, f2u(-2342+1.0/1024.0), d2, i32, f2u(-2342-1.0/1024.0));
    TESTINSN_bin("vpmin.f32 d0, d1, d2", d0, d1, i32, f2u(89276+1.0/1024.0), d2, i32, f2u(98276+1.0/1024.0));
    TESTINSN_bin("vpmin.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(NAN));
    TESTINSN_bin("vpmin.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(1.0));
    TESTINSN_bin("vpmin.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(0.0));
    TESTINSN_bin("vpmin.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(INFINITY));
    TESTINSN_bin("vpmin.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(-INFINITY));
    TESTINSN_bin("vpmin.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(NAN));
    TESTINSN_bin("vpmin.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(1.0));
    TESTINSN_bin("vpmin.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(0.0));
    TESTINSN_bin("vpmin.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(INFINITY));
    TESTINSN_bin("vpmin.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(-INFINITY));
    TESTINSN_bin("vpmin.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(NAN));
    TESTINSN_bin("vpmin.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(1.0));
    TESTINSN_bin("vpmin.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(0.0));
    TESTINSN_bin("vpmin.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(INFINITY));
    TESTINSN_bin("vpmin.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(-INFINITY));
    TESTINSN_bin("vpmin.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(NAN));
    TESTINSN_bin("vpmin.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(1.0));
    TESTINSN_bin("vpmin.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(0.0));
    TESTINSN_bin("vpmin.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(INFINITY));
    TESTINSN_bin("vpmin.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(-INFINITY));

    printf("---- VRECPE ----\n");
    TESTINSN_un("vrecpe.u32 d0, d1", d0, d1, i32, f2u(3.2));
    TESTINSN_un("vrecpe.u32 d0, d1", d0, d1, i32, f2u(-653.2));
    TESTINSN_un("vrecpe.u32 d10, d11", d10, d11, i32, f2u(3e22));
    TESTINSN_un("vrecpe.u32 d15, d4", d15, d4, i32, f2u(3e9));
    TESTINSN_un("vrecpe.u32 d15, d4", d15, d4, i32, f2u(-0.5));
    TESTINSN_un("vrecpe.u32 d15, d4", d15, d4, i32, f2u(-7.1));
    TESTINSN_un("vrecpe.u32 d12, d8", d12, d8, i32, f2u(8.0 - 1.0/1024.0));
    TESTINSN_un("vrecpe.u32 d12, d8", d12, d8, i32, f2u(-8.0 + 1.0/1024.0));
    TESTINSN_un("vrecpe.u32 d0, d1", d0, d1, i32, f2u(3.2));
    TESTINSN_un("vrecpe.u32 d10, d11", d10, d11, i32, f2u(3e22));
    TESTINSN_un("vrecpe.u32 d15, d4", d15, d4, i32, f2u(3e9));
    TESTINSN_un("vrecpe.f32 d15, d4", d15, d4, i32, f2u(-0.5));
    TESTINSN_un("vrecpe.f32 d15, d4", d15, d4, i32, f2u(-7.1));
    TESTINSN_un("vrecpe.f32 d12, d8", d12, d8, i32, f2u(8.0 - 1.0/1024.0));
    TESTINSN_un("vrecpe.f32 d12, d8", d12, d8, i32, f2u(-8.0 + 1.0/1024.0));
    TESTINSN_un("vrecpe.f32 d0, d1", d0, d1, i32, 7);
    TESTINSN_un("vrecpe.f32 d10, d11", d10, d11, i32, 1 << 31);
    TESTINSN_un("vrecpe.f32 d0, d1", d0, d1, i32, (1U << 31) + 1);
    TESTINSN_un("vrecpe.f32 d0, d1", d0, d1, i32, (1U << 31) - 1);
    TESTINSN_un("vrecpe.f32 d0, d14", d0, d14, i32, 0x30a0bcef);
    TESTINSN_un("vrecpe.f32 d0, d1", d0, d1, i32, 7);
    TESTINSN_un("vrecpe.f32 d10, d11", d10, d11, i32, 1 << 31);
    TESTINSN_un("vrecpe.f32 d0, d1", d0, d1, i32, (1U << 31) + 1);
    TESTINSN_un("vrecpe.f32 d0, d1", d0, d1, i32, (1U << 31) - 1);
    TESTINSN_un("vrecpe.f32 d0, d14", d0, d14, i32, 0x30a0bcef);
    TESTINSN_un("vrecpe.f32 d0, d1", d0, d1, i32, f2u(NAN));
    TESTINSN_un("vrecpe.f32 d0, d1", d0, d1, i32, f2u(0.0));
    TESTINSN_un("vrecpe.f32 d0, d1", d0, d1, i32, f2u(INFINITY));
    TESTINSN_un("vrecpe.f32 d0, d1", d0, d1, i32, f2u(-INFINITY));

    printf("---- VRECPS ----\n");
    TESTINSN_bin("vrecps.f32 d0, d5, d2", d0, d5, i32, f2u(23.04), d2, i32, f2u(-45.5687));
    TESTINSN_bin("vrecps.f32 d3, d4, d5", d3, d4, i32, f2u(-347856.475), d5, i32, f2u(1346));
    TESTINSN_bin("vrecps.f32 d10, d11, d2", d10, d11, i32, f2u(48755), d2, i32, f2u(-45786.476));
    TESTINSN_bin("vrecps.f32 d9, d5, d7", d9, d5, i32, f2u(95867.76), d7, i32, f2u(17065));
    TESTINSN_bin("vrecps.f32 d0, d5, d2", d0, d5, i32, f2u(-45667.24), d2, i32, f2u(-248562.76));
    TESTINSN_bin("vrecps.f32 d3, d4, d5", d3, d4, i32, f2u(24), d5, i32, f2u(1346)); 
    TESTINSN_bin("vrecps.f32 d10, d11, d2", d10, d11, i32, f2u(48755), d2, i32, f2u(1089));
    TESTINSN_bin("vrecps.f32 d9, d5, d7", d9, d5, i32, f2u(214), d7, i32, f2u(1752065));
    TESTINSN_bin("vrecps.f32 d0, d11, d12", d0, d11, i32, f2u(356047.56), d12, i32, f2u(5867.009));
    TESTINSN_bin("vrecps.f32 d7, d1, d6", d7, d1, i32, f2u(34.00046), d6, i32, f2u(0.0024575));
    TESTINSN_bin("vrecps.f32 d0, d1, d2", d0, d1, i32, f2u(2754), d2, i32, f2u(107));
    TESTINSN_bin("vrecps.f32 d3, d4, d5", d3, d4, i32, f2u(874), d5, i32, f2u(1384.6));
    TESTINSN_bin("vrecps.f32 d10, d11, d2", d10, d11, i32, f2u(487.587), d2, i32, f2u(109));
    TESTINSN_bin("vrecps.f32 d9, d5, d7", d9, d5, i32, f2u(2146), d7, i32, f2u(1752)); 
    TESTINSN_bin("vrecps.f32 d0, d11, d12", d0, d11, i32, f2u(-56.25), d12, i32, f2u(-5786.47));
    TESTINSN_bin("vrecps.f32 d7, d1, d6", d7, d1, i32, f2u(456.2489562), d6, i32, f2u(-7.2945676));
    TESTINSN_bin("vrecps.f32 d0, d5, d2", d0, d5, i32, f2u(532.987), d2, i32, f2u(-0.0045876));
    TESTINSN_bin("vrecps.f32 d10, d13, d15", d10, d13, i32, f2u(-485.2457), d15, i32, f2u(-567.245));
    TESTINSN_bin("vrecps.f32 d10, d13, d15", d10, d13, i32, f2u(278456.45), d15, i32, f2u(8756.0076));
    TESTINSN_bin("vrecps.f32 d0, d1, d2", d0, d1, i32, f2u(876988654), d2, i32, f2u(1224808797));
    TESTINSN_bin("vrecps.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(NAN));
    TESTINSN_bin("vrecps.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(1.0));
    TESTINSN_bin("vrecps.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(0.0));
    TESTINSN_bin("vrecps.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(INFINITY));
    TESTINSN_bin("vrecps.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(-INFINITY));
    TESTINSN_bin("vrecps.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(NAN));
    TESTINSN_bin("vrecps.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(1.0));
    TESTINSN_bin("vrecps.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(0.0));
    TESTINSN_bin("vrecps.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(INFINITY));
    TESTINSN_bin("vrecps.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(-INFINITY));
    TESTINSN_bin("vrecps.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(NAN));
    TESTINSN_bin("vrecps.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(1.0));
    TESTINSN_bin("vrecps.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(0.0));
    TESTINSN_bin("vrecps.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(INFINITY));
    TESTINSN_bin("vrecps.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(-INFINITY));
    TESTINSN_bin("vrecps.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(NAN));
    TESTINSN_bin("vrecps.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(1.0));
    TESTINSN_bin("vrecps.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(0.0));
    TESTINSN_bin("vrecps.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(INFINITY));
    TESTINSN_bin("vrecps.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(-INFINITY));

    printf("---- VABS (fp) ----\n");
    TESTINSN_un("vabs.f32 d0, d1", d0, d1, i32, f2u(3.2));
    TESTINSN_un("vabs.f32 d10, d11", d10, d11, i32, f2u(3e22));
    TESTINSN_un("vabs.f32 d15, d4", d15, d4, i32, f2u(3e9));
    TESTINSN_un("vabs.f32 d15, d4", d15, d4, i32, f2u(-0.5));
    TESTINSN_un("vabs.f32 d15, d4", d15, d4, i32, f2u(-7.1));
    TESTINSN_un("vabs.f32 d12, d8", d12, d8, i32, f2u(8.0 - 1.0/1024.0));
    TESTINSN_un("vabs.f32 d12, d8", d12, d8, i32, f2u(-8.0 + 1.0/1024.0));
    TESTINSN_un("vabs.f32 d0, d1", d0, d1, i32, f2u(3.2));
    TESTINSN_un("vabs.f32 d10, d11", d10, d11, i32, f2u(3e22));
    TESTINSN_un("vabs.f32 d15, d4", d15, d4, i32, f2u(3e9));
    TESTINSN_un("vabs.f32 d15, d4", d15, d4, i32, f2u(-0.5));
    TESTINSN_un("vabs.f32 d15, d4", d15, d4, i32, f2u(-7.1));
    TESTINSN_un("vabs.f32 d12, d8", d12, d8, i32, f2u(8.0 - 1.0/1024.0));
    TESTINSN_un("vabs.f32 d12, d8", d12, d8, i32, f2u(-8.0 + 1.0/1024.0));
    TESTINSN_un("vabs.f32 d0, d1", d0, d1, i32, 7);
    TESTINSN_un("vabs.f32 d10, d11", d10, d11, i32, 1 << 31); 
    TESTINSN_un("vabs.f32 d0, d1", d0, d1, i32, (1U << 31) + 1);
    TESTINSN_un("vabs.f32 d0, d1", d0, d1, i32, (1U << 31) - 1);
    TESTINSN_un("vabs.f32 d0, d14", d0, d14, i32, 0x30a0bcef);
    TESTINSN_un("vabs.f32 d0, d1", d0, d1, i32, 7);
    TESTINSN_un("vabs.f32 d10, d11", d10, d11, i32, 1 << 31); 
    TESTINSN_un("vabs.f32 d0, d1", d0, d1, i32, (1U << 31) + 1);
    TESTINSN_un("vabs.f32 d0, d1", d0, d1, i32, (1U << 31) - 1);
    TESTINSN_un("vabs.f32 d0, d14", d0, d14, i32, 0x30a0bcef);
    TESTINSN_un("vabs.f32 d0, d1", d0, d1, i32, f2u(NAN));
    TESTINSN_un("vabs.f32 d0, d1", d0, d1, i32, f2u(0.0));
    TESTINSN_un("vabs.f32 d0, d1", d0, d1, i32, f2u(INFINITY));
    TESTINSN_un("vabs.f32 d0, d1", d0, d1, i32, f2u(-INFINITY));

    printf("---- VCGT (fp) ----\n");
    TESTINSN_bin("vcgt.f32 d0, d1, d2", d0, d1, i32, f2u(0.5), d2, i32, f2u(-0.5));
    TESTINSN_bin("vcgt.f32 d2, d15, d12", d2, d15, i32, f2u(-0.53), d12, i32, f2u(0.52));
    TESTINSN_bin("vcgt.f32 d15, d7, d8", d15, d7, i32, f2u(231.45), d7, i32, f2u(231.45));
    TESTINSN_bin("vcgt.f32 d0, d5, d2", d0, d5, i32, f2u(23.04), d2, i32, f2u(-45.5687));
    TESTINSN_bin("vcgt.f32 d3, d4, d5", d3, d4, i32, f2u(-347856.475), d5, i32, f2u(1346));
    TESTINSN_bin("vcgt.f32 d10, d11, d2", d10, d11, i32, f2u(48755), d2, i32, f2u(-45786.476));
    TESTINSN_bin("vcgt.f32 d9, d5, d7", d9, d5, i32, f2u(95867.76), d7, i32, f2u(17065));
    TESTINSN_bin("vcgt.f32 d0, d5, d2", d0, d5, i32, f2u(-45667.24), d2, i32, f2u(-248562.76));
    TESTINSN_bin("vcgt.f32 d3, d4, d5", d3, d4, i32, f2u(24.87556), d5, i32, f2u(1346.0004));
    TESTINSN_bin("vcgt.f32 d10, d31, d2", d10, d31, i32, f2u(48755.7), d2, i32, f2u(1089.2));
    TESTINSN_bin("vcgt.f32 d9, d5, d7", d9, d5, i32, f2u(214), d7, i32, f2u(1752065));
    TESTINSN_bin("vcgt.f32 d0, d11, d12", d0, d11, i32, f2u(356047.56), d12, i32, f2u(5867.009));
    TESTINSN_bin("vcgt.f32 d7, d1, d6", d7, d1, i32, f2u(34.00046), d6, i32, f2u(0.0024575));
    TESTINSN_bin("vcgt.f32 d0, d1, d2", d0, d1, i32, f2u(2754), d2, i32, f2u(107));
    TESTINSN_bin("vcgt.f32 d3, d4, d5", d3, d4, i32, f2u(874), d5, i32, f2u(1384.6));
    TESTINSN_bin("vcgt.f32 d20, d21, d2", d20, d21, i32, f2u(487.587), d2, i32, f2u(109));
    TESTINSN_bin("vcgt.f32 d9, d5, d7", d9, d5, i32, f2u(2146), d7, i32, f2u(1752));
    TESTINSN_bin("vcgt.f32 d0, d11, d12", d0, d11, i32, f2u(-56.25), d12, i32, f2u(-5786.47));
    TESTINSN_bin("vcgt.f32 d7, d1, d6", d7, d1, i32, f2u(456.2489562), d6, i32, f2u(-7.2945676));
    TESTINSN_bin("vcgt.f32 d0, d5, d2", d0, d5, i32, f2u(532.987), d2, i32, f2u(-0.0045876));
    TESTINSN_bin("vcgt.f32 d10, d13, d15", d10, d13, i32, f2u(-485.2457), d15, i32, f2u(-567.245));
    TESTINSN_bin("vcgt.f32 d10, d13, d15", d10, d13, i32, f2u(278456.45), d15, i32, f2u(8756.0076));
    TESTINSN_bin("vcgt.f32 d0, d1, d2", d0, d1, i32, f2u(876988654), d2, i32, f2u(1224808797));
    TESTINSN_bin("vcgt.f32 d0, d1, d2", d0, d1, i32, f2u(0), d2, i32, f2u(0));
    TESTINSN_bin("vcgt.f32 d0, d1, d2", d0, d1, i32, f2u(1.0/1024.0), d2, i32, f2u(-1.0/1024.0));
    TESTINSN_bin("vcgt.f32 d0, d1, d2", d0, d1, i32, f2u(-1.0/1024.0), d2, i32, f2u(1.0/1024.0));
    TESTINSN_bin("vcgt.f32 d0, d1, d2", d0, d1, i32, f2u(2342+1.0/1024.0), d2, i32, f2u(2342-1.0/1024.0));
    TESTINSN_bin("vcgt.f32 d0, d1, d2", d0, d1, i32, f2u(-2342+1.0/1024.0), d2, i32, f2u(-2342-1.0/1024.0));
    TESTINSN_bin("vcgt.f32 d0, d1, d2", d0, d1, i32, f2u(89276+1.0/1024.0), d2, i32, f2u(98276+1.0/1024.0));
    TESTINSN_bin("vcgt.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(NAN));
    TESTINSN_bin("vcgt.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(1.0));
    TESTINSN_bin("vcgt.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(0.0));
    TESTINSN_bin("vcgt.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(INFINITY));
    TESTINSN_bin("vcgt.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(-INFINITY));
    TESTINSN_bin("vcgt.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(NAN));
    TESTINSN_bin("vcgt.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(1.0));
    TESTINSN_bin("vcgt.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(0.0));
    TESTINSN_bin("vcgt.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(INFINITY));
    TESTINSN_bin("vcgt.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(-INFINITY));
    TESTINSN_bin("vcgt.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(NAN));
    TESTINSN_bin("vcgt.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(1.0));
    TESTINSN_bin("vcgt.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(0.0));
    TESTINSN_bin("vcgt.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(INFINITY));
    TESTINSN_bin("vcgt.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(-INFINITY));
    TESTINSN_bin("vcgt.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(NAN));
    TESTINSN_bin("vcgt.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(1.0));
    TESTINSN_bin("vcgt.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(0.0));
    TESTINSN_bin("vcgt.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(INFINITY));
    TESTINSN_bin("vcgt.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(-INFINITY));

    printf("---- VCGE (fp) ----\n");
    TESTINSN_bin("vcge.f32 d0, d1, d2", d0, d1, i32, f2u(0.5), d2, i32, f2u(-0.5));
    TESTINSN_bin("vcge.f32 d2, d15, d12", d2, d15, i32, f2u(-0.53), d12, i32, f2u(0.52));
    TESTINSN_bin("vcge.f32 d15, d7, d8", d15, d7, i32, f2u(231.45), d7, i32, f2u(231.45));
    TESTINSN_bin("vcge.f32 d0, d5, d2", d0, d5, i32, f2u(23.04), d2, i32, f2u(-45.5687));
    TESTINSN_bin("vcge.f32 d3, d4, d5", d3, d4, i32, f2u(-347856.475), d5, i32, f2u(1346));
    TESTINSN_bin("vcge.f32 d10, d11, d2", d10, d11, i32, f2u(48755), d2, i32, f2u(-45786.476));
    TESTINSN_bin("vcge.f32 d9, d5, d7", d9, d5, i32, f2u(95867.76), d7, i32, f2u(17065));
    TESTINSN_bin("vcge.f32 d0, d5, d2", d0, d5, i32, f2u(-45667.24), d2, i32, f2u(-248562.76));
    TESTINSN_bin("vcge.f32 d3, d4, d5", d3, d4, i32, f2u(24.87556), d5, i32, f2u(1346.0004));
    TESTINSN_bin("vcge.f32 d10, d31, d2", d10, d31, i32, f2u(48755.7), d2, i32, f2u(1089.2));
    TESTINSN_bin("vcge.f32 d9, d5, d7", d9, d5, i32, f2u(214), d7, i32, f2u(1752065));
    TESTINSN_bin("vcge.f32 d0, d11, d12", d0, d11, i32, f2u(356047.56), d12, i32, f2u(5867.009));
    TESTINSN_bin("vcge.f32 d7, d1, d6", d7, d1, i32, f2u(34.00046), d6, i32, f2u(0.0024575));
    TESTINSN_bin("vcge.f32 d0, d1, d2", d0, d1, i32, f2u(2754), d2, i32, f2u(107));
    TESTINSN_bin("vcge.f32 d3, d4, d5", d3, d4, i32, f2u(874), d5, i32, f2u(1384.6));
    TESTINSN_bin("vcge.f32 d20, d21, d2", d20, d21, i32, f2u(487.587), d2, i32, f2u(109));
    TESTINSN_bin("vcge.f32 d9, d5, d7", d9, d5, i32, f2u(2146), d7, i32, f2u(1752));
    TESTINSN_bin("vcge.f32 d0, d11, d12", d0, d11, i32, f2u(-56.25), d12, i32, f2u(-5786.47));
    TESTINSN_bin("vcge.f32 d7, d1, d6", d7, d1, i32, f2u(456.2489562), d6, i32, f2u(-7.2945676));
    TESTINSN_bin("vcge.f32 d0, d5, d2", d0, d5, i32, f2u(532.987), d2, i32, f2u(-0.0045876));
    TESTINSN_bin("vcge.f32 d10, d13, d15", d10, d13, i32, f2u(-485.2457), d15, i32, f2u(-567.245));
    TESTINSN_bin("vcge.f32 d10, d13, d15", d10, d13, i32, f2u(278456.45), d15, i32, f2u(8756.0076));
    TESTINSN_bin("vcge.f32 d0, d1, d2", d0, d1, i32, f2u(876988654), d2, i32, f2u(1224808797));
    TESTINSN_bin("vcge.f32 d0, d1, d2", d0, d1, i32, f2u(0), d2, i32, f2u(0));
    TESTINSN_bin("vcge.f32 d0, d1, d2", d0, d1, i32, f2u(1.0/1024.0), d2, i32, f2u(-1.0/1024.0));
    TESTINSN_bin("vcge.f32 d0, d1, d2", d0, d1, i32, f2u(-1.0/1024.0), d2, i32, f2u(1.0/1024.0));
    TESTINSN_bin("vcge.f32 d0, d1, d2", d0, d1, i32, f2u(2342+1.0/1024.0), d2, i32, f2u(2342-1.0/1024.0));
    TESTINSN_bin("vcge.f32 d0, d1, d2", d0, d1, i32, f2u(-2342+1.0/1024.0), d2, i32, f2u(-2342-1.0/1024.0));
    TESTINSN_bin("vcge.f32 d0, d1, d2", d0, d1, i32, f2u(89276+1.0/1024.0), d2, i32, f2u(98276+1.0/1024.0));
    TESTINSN_bin("vcge.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(NAN));
    TESTINSN_bin("vcge.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(1.0));
    TESTINSN_bin("vcge.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(0.0));
    TESTINSN_bin("vcge.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(INFINITY));
    TESTINSN_bin("vcge.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(-INFINITY));
    TESTINSN_bin("vcge.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(NAN));
    TESTINSN_bin("vcge.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(1.0));
    TESTINSN_bin("vcge.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(0.0));
    TESTINSN_bin("vcge.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(INFINITY));
    TESTINSN_bin("vcge.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(-INFINITY));
    TESTINSN_bin("vcge.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(NAN));
    TESTINSN_bin("vcge.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(1.0));
    TESTINSN_bin("vcge.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(0.0));
    TESTINSN_bin("vcge.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(INFINITY));
    TESTINSN_bin("vcge.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(-INFINITY));
    TESTINSN_bin("vcge.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(NAN));
    TESTINSN_bin("vcge.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(1.0));
    TESTINSN_bin("vcge.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(0.0));
    TESTINSN_bin("vcge.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(INFINITY));
    TESTINSN_bin("vcge.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(-INFINITY));

    printf("---- VACGT (fp) ----\n");
    TESTINSN_bin("vacgt.f32 d0, d1, d2", d0, d1, i32, f2u(0.5), d2, i32, f2u(-0.5));
    TESTINSN_bin("vacgt.f32 d2, d15, d12", d2, d15, i32, f2u(-0.53), d12, i32, f2u(0.52));
    TESTINSN_bin("vacgt.f32 d15, d7, d8", d15, d7, i32, f2u(231.45), d7, i32, f2u(231.45));
    TESTINSN_bin("vacgt.f32 d0, d5, d2", d0, d5, i32, f2u(23.04), d2, i32, f2u(-45.5687));
    TESTINSN_bin("vacgt.f32 d3, d4, d5", d3, d4, i32, f2u(-347856.475), d5, i32, f2u(1346));
    TESTINSN_bin("vacgt.f32 d10, d11, d2", d10, d11, i32, f2u(48755), d2, i32, f2u(-45786.476));
    TESTINSN_bin("vacgt.f32 d9, d5, d7", d9, d5, i32, f2u(95867.76), d7, i32, f2u(17065));
    TESTINSN_bin("vacgt.f32 d0, d5, d2", d0, d5, i32, f2u(-45667.24), d2, i32, f2u(-248562.76));
    TESTINSN_bin("vacgt.f32 d3, d4, d5", d3, d4, i32, f2u(24.87556), d5, i32, f2u(1346.0004));
    TESTINSN_bin("vacgt.f32 d10, d31, d2", d10, d31, i32, f2u(48755.7), d2, i32, f2u(1089.2));
    TESTINSN_bin("vacgt.f32 d9, d5, d7", d9, d5, i32, f2u(214), d7, i32, f2u(1752065));
    TESTINSN_bin("vacgt.f32 d0, d11, d12", d0, d11, i32, f2u(356047.56), d12, i32, f2u(5867.009));
    TESTINSN_bin("vacgt.f32 d7, d1, d6", d7, d1, i32, f2u(34.00046), d6, i32, f2u(0.0024575));
    TESTINSN_bin("vacgt.f32 d0, d1, d2", d0, d1, i32, f2u(2754), d2, i32, f2u(107));
    TESTINSN_bin("vacgt.f32 d3, d4, d5", d3, d4, i32, f2u(874), d5, i32, f2u(1384.6));
    TESTINSN_bin("vacgt.f32 d20, d21, d2", d20, d21, i32, f2u(487.587), d2, i32, f2u(109));
    TESTINSN_bin("vacgt.f32 d9, d5, d7", d9, d5, i32, f2u(2146), d7, i32, f2u(1752));
    TESTINSN_bin("vacgt.f32 d0, d11, d12", d0, d11, i32, f2u(-56.25), d12, i32, f2u(-5786.47));
    TESTINSN_bin("vacgt.f32 d7, d1, d6", d7, d1, i32, f2u(456.2489562), d6, i32, f2u(-7.2945676));
    TESTINSN_bin("vacgt.f32 d0, d5, d2", d0, d5, i32, f2u(532.987), d2, i32, f2u(-0.0045876));
    TESTINSN_bin("vacgt.f32 d10, d13, d15", d10, d13, i32, f2u(-485.2457), d15, i32, f2u(-567.245));
    TESTINSN_bin("vacgt.f32 d10, d13, d15", d10, d13, i32, f2u(278456.45), d15, i32, f2u(8756.0076));
    TESTINSN_bin("vacgt.f32 d0, d1, d2", d0, d1, i32, f2u(876988654), d2, i32, f2u(1224808797));
    TESTINSN_bin("vacgt.f32 d0, d1, d2", d0, d1, i32, f2u(0), d2, i32, f2u(0));
    TESTINSN_bin("vacgt.f32 d0, d1, d2", d0, d1, i32, f2u(1.0/1024.0), d2, i32, f2u(-1.0/1024.0));
    TESTINSN_bin("vacgt.f32 d0, d1, d2", d0, d1, i32, f2u(-1.0/1024.0), d2, i32, f2u(1.0/1024.0));
    TESTINSN_bin("vacgt.f32 d0, d1, d2", d0, d1, i32, f2u(2342+1.0/1024.0), d2, i32, f2u(2342-1.0/1024.0));
    TESTINSN_bin("vacgt.f32 d0, d1, d2", d0, d1, i32, f2u(-2342+1.0/1024.0), d2, i32, f2u(-2342-1.0/1024.0));
    TESTINSN_bin("vacgt.f32 d0, d1, d2", d0, d1, i32, f2u(89276+1.0/1024.0), d2, i32, f2u(98276+1.0/1024.0));
    TESTINSN_bin("vacgt.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(NAN));
    TESTINSN_bin("vacgt.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(1.0));
    TESTINSN_bin("vacgt.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(0.0));
    TESTINSN_bin("vacgt.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(INFINITY));
    TESTINSN_bin("vacgt.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(-INFINITY));
    TESTINSN_bin("vacgt.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(NAN));
    TESTINSN_bin("vacgt.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(1.0));
    TESTINSN_bin("vacgt.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(0.0));
    TESTINSN_bin("vacgt.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(INFINITY));
    TESTINSN_bin("vacgt.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(-INFINITY));
    TESTINSN_bin("vacgt.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(NAN));
    TESTINSN_bin("vacgt.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(1.0));
    TESTINSN_bin("vacgt.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(0.0));
    TESTINSN_bin("vacgt.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(INFINITY));
    TESTINSN_bin("vacgt.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(-INFINITY));
    TESTINSN_bin("vacgt.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(NAN));
    TESTINSN_bin("vacgt.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(1.0));
    TESTINSN_bin("vacgt.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(0.0));
    TESTINSN_bin("vacgt.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(INFINITY));
    TESTINSN_bin("vacgt.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(-INFINITY));

    printf("---- VACGE (fp) ----\n");
    TESTINSN_bin("vacge.f32 d0, d1, d2", d0, d1, i32, f2u(0.5), d2, i32, f2u(-0.5));
    TESTINSN_bin("vacge.f32 d2, d15, d12", d2, d15, i32, f2u(-0.53), d12, i32, f2u(0.52));
    TESTINSN_bin("vacge.f32 d15, d7, d8", d15, d7, i32, f2u(231.45), d7, i32, f2u(231.45));
    TESTINSN_bin("vacge.f32 d0, d5, d2", d0, d5, i32, f2u(23.04), d2, i32, f2u(-45.5687));
    TESTINSN_bin("vacge.f32 d3, d4, d5", d3, d4, i32, f2u(-347856.475), d5, i32, f2u(1346));
    TESTINSN_bin("vacge.f32 d10, d11, d2", d10, d11, i32, f2u(48755), d2, i32, f2u(-45786.476));
    TESTINSN_bin("vacge.f32 d9, d5, d7", d9, d5, i32, f2u(95867.76), d7, i32, f2u(17065));
    TESTINSN_bin("vacge.f32 d0, d5, d2", d0, d5, i32, f2u(-45667.24), d2, i32, f2u(-248562.76));
    TESTINSN_bin("vacge.f32 d3, d4, d5", d3, d4, i32, f2u(24.87556), d5, i32, f2u(1346.0004));
    TESTINSN_bin("vacge.f32 d10, d31, d2", d10, d31, i32, f2u(48755.7), d2, i32, f2u(1089.2));
    TESTINSN_bin("vacge.f32 d9, d5, d7", d9, d5, i32, f2u(214), d7, i32, f2u(1752065));
    TESTINSN_bin("vacge.f32 d0, d11, d12", d0, d11, i32, f2u(356047.56), d12, i32, f2u(5867.009));
    TESTINSN_bin("vacge.f32 d7, d1, d6", d7, d1, i32, f2u(34.00046), d6, i32, f2u(0.0024575));
    TESTINSN_bin("vacge.f32 d0, d1, d2", d0, d1, i32, f2u(2754), d2, i32, f2u(107));
    TESTINSN_bin("vacge.f32 d3, d4, d5", d3, d4, i32, f2u(874), d5, i32, f2u(1384.6));
    TESTINSN_bin("vacge.f32 d20, d21, d2", d20, d21, i32, f2u(487.587), d2, i32, f2u(109));
    TESTINSN_bin("vacge.f32 d9, d5, d7", d9, d5, i32, f2u(2146), d7, i32, f2u(1752));
    TESTINSN_bin("vacge.f32 d0, d11, d12", d0, d11, i32, f2u(-56.25), d12, i32, f2u(-5786.47));
    TESTINSN_bin("vacge.f32 d7, d1, d6", d7, d1, i32, f2u(456.2489562), d6, i32, f2u(-7.2945676));
    TESTINSN_bin("vacge.f32 d0, d5, d2", d0, d5, i32, f2u(532.987), d2, i32, f2u(-0.0045876));
    TESTINSN_bin("vacge.f32 d10, d13, d15", d10, d13, i32, f2u(-485.2457), d15, i32, f2u(-567.245));
    TESTINSN_bin("vacge.f32 d10, d13, d15", d10, d13, i32, f2u(278456.45), d15, i32, f2u(8756.0076));
    TESTINSN_bin("vacge.f32 d0, d1, d2", d0, d1, i32, f2u(876988654), d2, i32, f2u(1224808797));
    TESTINSN_bin("vacge.f32 d0, d1, d2", d0, d1, i32, f2u(0), d2, i32, f2u(0));
    TESTINSN_bin("vacge.f32 d0, d1, d2", d0, d1, i32, f2u(1.0/1024.0), d2, i32, f2u(-1.0/1024.0));
    TESTINSN_bin("vacge.f32 d0, d1, d2", d0, d1, i32, f2u(-1.0/1024.0), d2, i32, f2u(1.0/1024.0));
    TESTINSN_bin("vacge.f32 d0, d1, d2", d0, d1, i32, f2u(2342+1.0/1024.0), d2, i32, f2u(2342-1.0/1024.0));
    TESTINSN_bin("vacge.f32 d0, d1, d2", d0, d1, i32, f2u(-2342+1.0/1024.0), d2, i32, f2u(-2342-1.0/1024.0));
    TESTINSN_bin("vacge.f32 d0, d1, d2", d0, d1, i32, f2u(89276+1.0/1024.0), d2, i32, f2u(98276+1.0/1024.0));
    TESTINSN_bin("vacge.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(NAN));
    TESTINSN_bin("vacge.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(1.0));
    TESTINSN_bin("vacge.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(0.0));
    TESTINSN_bin("vacge.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(INFINITY));
    TESTINSN_bin("vacge.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(-INFINITY));
    TESTINSN_bin("vacge.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(NAN));
    TESTINSN_bin("vacge.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(1.0));
    TESTINSN_bin("vacge.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(0.0));
    TESTINSN_bin("vacge.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(INFINITY));
    TESTINSN_bin("vacge.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(-INFINITY));
    TESTINSN_bin("vacge.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(NAN));
    TESTINSN_bin("vacge.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(1.0));
    TESTINSN_bin("vacge.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(0.0));
    TESTINSN_bin("vacge.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(INFINITY));
    TESTINSN_bin("vacge.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(-INFINITY));
    TESTINSN_bin("vacge.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(NAN));
    TESTINSN_bin("vacge.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(1.0));
    TESTINSN_bin("vacge.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(0.0));
    TESTINSN_bin("vacge.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(INFINITY));
    TESTINSN_bin("vacge.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(-INFINITY));

    printf("---- VCEQ (fp) ----\n");
    TESTINSN_bin("vceq.f32 d0, d1, d2", d0, d1, i32, f2u(0.5), d2, i32, f2u(-0.5));
    TESTINSN_bin("vceq.f32 d2, d15, d12", d2, d15, i32, f2u(-0.53), d12, i32, f2u(0.52));
    TESTINSN_bin("vceq.f32 d15, d7, d8", d15, d7, i32, f2u(231.45), d7, i32, f2u(231.45));
    TESTINSN_bin("vceq.f32 d0, d5, d2", d0, d5, i32, f2u(23.04), d2, i32, f2u(-45.5687));
    TESTINSN_bin("vceq.f32 d3, d4, d5", d3, d4, i32, f2u(-347856.475), d5, i32, f2u(1346));
    TESTINSN_bin("vceq.f32 d10, d11, d2", d10, d11, i32, f2u(48755), d2, i32, f2u(-45786.476));
    TESTINSN_bin("vceq.f32 d9, d5, d7", d9, d5, i32, f2u(95867.76), d7, i32, f2u(17065));
    TESTINSN_bin("vceq.f32 d0, d5, d2", d0, d5, i32, f2u(-45667.24), d2, i32, f2u(-248562.76));
    TESTINSN_bin("vceq.f32 d3, d4, d5", d3, d4, i32, f2u(24.87556), d5, i32, f2u(1346.0004));
    TESTINSN_bin("vceq.f32 d10, d31, d2", d10, d31, i32, f2u(48755.7), d2, i32, f2u(1089.2));
    TESTINSN_bin("vceq.f32 d9, d5, d7", d9, d5, i32, f2u(214), d7, i32, f2u(1752065));
    TESTINSN_bin("vceq.f32 d0, d11, d12", d0, d11, i32, f2u(356047.56), d12, i32, f2u(5867.009));
    TESTINSN_bin("vceq.f32 d7, d1, d6", d7, d1, i32, f2u(34.00046), d6, i32, f2u(0.0024575));
    TESTINSN_bin("vceq.f32 d0, d1, d2", d0, d1, i32, f2u(2754), d2, i32, f2u(107));
    TESTINSN_bin("vceq.f32 d3, d4, d5", d3, d4, i32, f2u(874), d5, i32, f2u(1384.6));
    TESTINSN_bin("vceq.f32 d20, d21, d2", d20, d21, i32, f2u(487.587), d2, i32, f2u(109));
    TESTINSN_bin("vceq.f32 d9, d5, d7", d9, d5, i32, f2u(2146), d7, i32, f2u(1752));
    TESTINSN_bin("vceq.f32 d0, d11, d12", d0, d11, i32, f2u(-56.25), d12, i32, f2u(-5786.47));
    TESTINSN_bin("vceq.f32 d7, d1, d6", d7, d1, i32, f2u(456.2489562), d6, i32, f2u(-7.2945676));
    TESTINSN_bin("vceq.f32 d0, d5, d2", d0, d5, i32, f2u(532.987), d2, i32, f2u(-0.0045876));
    TESTINSN_bin("vceq.f32 d10, d13, d15", d10, d13, i32, f2u(-485.2457), d15, i32, f2u(-567.245));
    TESTINSN_bin("vceq.f32 d10, d13, d15", d10, d13, i32, f2u(278456.45), d15, i32, f2u(8756.0076));
    TESTINSN_bin("vceq.f32 d0, d1, d2", d0, d1, i32, f2u(876988654), d2, i32, f2u(1224808797));
    TESTINSN_bin("vceq.f32 d0, d1, d2", d0, d1, i32, f2u(0), d2, i32, f2u(0));
    TESTINSN_bin("vceq.f32 d0, d1, d2", d0, d1, i32, f2u(1.0/1024.0), d2, i32, f2u(-1.0/1024.0));
    TESTINSN_bin("vceq.f32 d0, d1, d2", d0, d1, i32, f2u(-1.0/1024.0), d2, i32, f2u(1.0/1024.0));
    TESTINSN_bin("vceq.f32 d0, d1, d2", d0, d1, i32, f2u(2342+1.0/1024.0), d2, i32, f2u(2342-1.0/1024.0));
    TESTINSN_bin("vceq.f32 d0, d1, d2", d0, d1, i32, f2u(-2342+1.0/1024.0), d2, i32, f2u(-2342-1.0/1024.0));
    TESTINSN_bin("vceq.f32 d0, d1, d2", d0, d1, i32, f2u(89276+1.0/1024.0), d2, i32, f2u(98276+1.0/1024.0));
    TESTINSN_bin("vceq.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(NAN));
    TESTINSN_bin("vceq.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(1.0));
    TESTINSN_bin("vceq.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(0.0));
    TESTINSN_bin("vceq.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(INFINITY));
    TESTINSN_bin("vceq.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(-INFINITY));
    TESTINSN_bin("vceq.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(NAN));
    TESTINSN_bin("vceq.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(1.0));
    TESTINSN_bin("vceq.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(0.0));
    TESTINSN_bin("vceq.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(INFINITY));
    TESTINSN_bin("vceq.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(-INFINITY));
    TESTINSN_bin("vceq.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(NAN));
    TESTINSN_bin("vceq.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(1.0));
    TESTINSN_bin("vceq.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(0.0));
    TESTINSN_bin("vceq.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(INFINITY));
    TESTINSN_bin("vceq.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(-INFINITY));
    TESTINSN_bin("vceq.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(NAN));
    TESTINSN_bin("vceq.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(1.0));
    TESTINSN_bin("vceq.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(0.0));
    TESTINSN_bin("vceq.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(INFINITY));
    TESTINSN_bin("vceq.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(-INFINITY));

    printf("---- VCEQ (fp) #0 ----\n");
    TESTINSN_un("vceq.f32 d0, d1, #0", d0, d1, i32, 0x01000000);
    TESTINSN_un("vceq.f32 d0, d1, #0", d0, d1, i32, 0x1);
    TESTINSN_un("vceq.f32 d2, d1, #0", d2, d1, i32, 1 << 31);
    TESTINSN_un("vceq.f32 d2, d1, #0", d2, d1, i32, f2u(23.04));
    TESTINSN_un("vceq.f32 d2, d31, #0", d2, d31, i32, f2u(-23.04));
    TESTINSN_un("vceq.f32 d30, d15, #0", d30, d15, i32, 0x0);
    TESTINSN_un("vceq.f32 d0, d1, #0", d0, d1, i32, f2u(NAN));
    TESTINSN_un("vceq.f32 d0, d1, #0", d0, d1, i32, f2u(0.0));
    TESTINSN_un("vceq.f32 d0, d1, #0", d0, d1, i32, f2u(INFINITY));
    TESTINSN_un("vceq.f32 d0, d1, #0", d0, d1, i32, f2u(-INFINITY));

    printf("---- VCGT (fp) #0 ----\n");
    TESTINSN_un("vcgt.f32 d0, d1, #0", d0, d1, i32, 0x01000000);
    TESTINSN_un("vcgt.f32 d0, d1, #0", d0, d1, i32, 0x1);
    TESTINSN_un("vcgt.f32 d2, d1, #0", d2, d1, i32, 1 << 31);
    TESTINSN_un("vcgt.f32 d2, d1, #0", d2, d1, i32, f2u(23.04));
    TESTINSN_un("vcgt.f32 d2, d31, #0", d2, d31, i32, f2u(-23.04));
    TESTINSN_un("vcgt.f32 d30, d15, #0", d30, d15, i32, 0x0);
    TESTINSN_un("vcgt.f32 d0, d1, #0", d0, d1, i32, f2u(NAN));
    TESTINSN_un("vcgt.f32 d0, d1, #0", d0, d1, i32, f2u(0.0));
    TESTINSN_un("vcgt.f32 d0, d1, #0", d0, d1, i32, f2u(INFINITY));
    TESTINSN_un("vcgt.f32 d0, d1, #0", d0, d1, i32, f2u(-INFINITY));

    printf("---- VCLT (fp) #0 ----\n");
    TESTINSN_un("vclt.f32 d0, d1, #0", d0, d1, i32, 0x01000000);
    TESTINSN_un("vclt.f32 d0, d1, #0", d0, d1, i32, 0x1);
    TESTINSN_un("vclt.f32 d2, d1, #0", d2, d1, i32, 1 << 31);
    TESTINSN_un("vclt.f32 d2, d1, #0", d2, d1, i32, f2u(23.04));
    TESTINSN_un("vclt.f32 d2, d31, #0", d2, d31, i32, f2u(-23.04));
    TESTINSN_un("vclt.f32 d30, d15, #0", d30, d15, i32, 0x0);
    TESTINSN_un("vclt.f32 d0, d1, #0", d0, d1, i32, f2u(NAN));
    TESTINSN_un("vclt.f32 d0, d1, #0", d0, d1, i32, f2u(0.0));
    TESTINSN_un("vclt.f32 d0, d1, #0", d0, d1, i32, f2u(INFINITY));
    TESTINSN_un("vclt.f32 d0, d1, #0", d0, d1, i32, f2u(-INFINITY));

    printf("---- VCGE (fp) #0 ----\n");
    TESTINSN_un("vcge.f32 d0, d1, #0", d0, d1, i32, 0x01000000);
    TESTINSN_un("vcge.f32 d0, d1, #0", d0, d1, i32, 0x1);
    TESTINSN_un("vcge.f32 d2, d1, #0", d2, d1, i32, 1 << 31);
    TESTINSN_un("vcge.f32 d2, d1, #0", d2, d1, i32, f2u(23.04));
    TESTINSN_un("vcge.f32 d2, d31, #0", d2, d31, i32, f2u(-23.04));
    TESTINSN_un("vcge.f32 d30, d15, #0", d30, d15, i32, 0x0);
    TESTINSN_un("vcle.f32 d0, d1, #0", d0, d1, i32, f2u(NAN));
    TESTINSN_un("vcle.f32 d0, d1, #0", d0, d1, i32, f2u(0.0));
    TESTINSN_un("vcle.f32 d0, d1, #0", d0, d1, i32, f2u(INFINITY));
    TESTINSN_un("vcle.f32 d0, d1, #0", d0, d1, i32, f2u(-INFINITY));

    printf("---- VCLE (fp) #0 ----\n");
    TESTINSN_un("vcle.f32 d0, d1, #0", d0, d1, i32, 0x01000000);
    TESTINSN_un("vcle.f32 d0, d1, #0", d0, d1, i32, 0x1);
    TESTINSN_un("vcle.f32 d2, d1, #0", d2, d1, i32, 1 << 31);
    TESTINSN_un("vcle.f32 d2, d1, #0", d2, d1, i32, f2u(23.04));
    TESTINSN_un("vcle.f32 d2, d31, #0", d2, d31, i32, f2u(-23.04));
    TESTINSN_un("vcle.f32 d30, d15, #0", d30, d15, i32, 0x0);
    TESTINSN_un("vcle.f32 d0, d1, #0", d0, d1, i32, f2u(NAN));
    TESTINSN_un("vcle.f32 d0, d1, #0", d0, d1, i32, f2u(0.0));
    TESTINSN_un("vcle.f32 d0, d1, #0", d0, d1, i32, f2u(INFINITY));
    TESTINSN_un("vcle.f32 d0, d1, #0", d0, d1, i32, f2u(-INFINITY));

    printf("---- VNEG (fp) ----\n");
    TESTINSN_un("vneg.f32 d0, d1", d0, d1, i32, 0x01000000);
    TESTINSN_un("vneg.f32 d0, d1", d0, d1, i32, 0x1);
    TESTINSN_un("vneg.f32 d2, d1", d2, d1, i32, 1 << 31);
    TESTINSN_un("vneg.f32 d2, d1", d2, d1, i32, f2u(23.04));
    TESTINSN_un("vneg.f32 d2, d31", d2, d31, i32, f2u(-23.04));
    TESTINSN_un("vneg.f32 d30, d15", d30, d15, i32, 0x0);
    TESTINSN_un("vneg.f32 d0, d1", d0, d1, i32, f2u(NAN));
    TESTINSN_un("vneg.f32 d0, d1", d0, d1, i32, f2u(0.0));
    TESTINSN_un("vneg.f32 d0, d1", d0, d1, i32, f2u(INFINITY));
    TESTINSN_un("vneg.f32 d0, d1", d0, d1, i32, f2u(-INFINITY));

    printf("---- VRSQRTS ----\n");
    TESTINSN_bin("vrsqrts.f32 d0, d5, d2", d0, d5, i32, f2u(23.04), d2, i32, f2u(-45.5687));
    TESTINSN_bin("vrsqrts.f32 d3, d4, d5", d3, d4, i32, f2u(-347856.475), d5, i32, f2u(1346));
    TESTINSN_bin("vrsqrts.f32 d10, d11, d2", d10, d11, i32, f2u(48755), d2, i32, f2u(-45786.476));
    TESTINSN_bin("vrsqrts.f32 d9, d5, d7", d9, d5, i32, f2u(95867.76), d7, i32, f2u(17065));
    TESTINSN_bin("vrsqrts.f32 d0, d5, d2", d0, d5, i32, f2u(-45667.24), d2, i32, f2u(-248562.76));
    TESTINSN_bin("vrsqrts.f32 d3, d4, d5", d3, d4, i32, f2u(24), d5, i32, f2u(1346));
    TESTINSN_bin("vrsqrts.f32 d10, d11, d2", d10, d11, i32, f2u(48755), d2, i32, f2u(1089));
    TESTINSN_bin("vrsqrts.f32 d9, d5, d7", d9, d5, i32, f2u(214), d7, i32, f2u(1752065));
    TESTINSN_bin("vrsqrts.f32 d0, d11, d12", d0, d11, i32, f2u(356047.56), d12, i32, f2u(5867.009));
    TESTINSN_bin("vrsqrts.f32 d7, d1, d6", d7, d1, i32, f2u(34.00046), d6, i32, f2u(0.0024575));
    TESTINSN_bin("vrsqrts.f32 d0, d1, d2", d0, d1, i32, f2u(2754), d2, i32, f2u(107));
    TESTINSN_bin("vrsqrts.f32 d3, d4, d5", d3, d4, i32, f2u(874), d5, i32, f2u(1384.6));
    TESTINSN_bin("vrsqrts.f32 d10, d11, d2", d10, d11, i32, f2u(487.587), d2, i32, f2u(109));
    TESTINSN_bin("vrsqrts.f32 d9, d5, d7", d9, d5, i32, f2u(2146), d7, i32, f2u(1752)); 
    TESTINSN_bin("vrsqrts.f32 d0, d11, d12", d0, d11, i32, f2u(-56.25), d12, i32, f2u(-5786.47));
    TESTINSN_bin("vrsqrts.f32 d7, d1, d6", d7, d1, i32, f2u(456.2489562), d6, i32, f2u(-7.2945676));
    TESTINSN_bin("vrsqrts.f32 d0, d5, d2", d0, d5, i32, f2u(532.987), d2, i32, f2u(-0.0045876));
    TESTINSN_bin("vrsqrts.f32 d10, d13, d15", d10, d13, i32, f2u(-485.2457), d15, i32, f2u(-567.245));
    TESTINSN_bin("vrsqrts.f32 d10, d13, d15", d10, d13, i32, f2u(278456.45), d15, i32, f2u(8756.0076));
    TESTINSN_bin("vrsqrts.f32 d0, d1, d2", d0, d1, i32, f2u(876988654), d2, i32, f2u(1224808797));
    TESTINSN_bin("vrsqrts.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(NAN));
    TESTINSN_bin("vrsqrts.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(1.0));
    TESTINSN_bin("vrsqrts.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(0.0));
    TESTINSN_bin("vrsqrts.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(INFINITY));
    TESTINSN_bin("vrsqrts.f32 d0, d1, d2", d0, d1, i32, f2u(NAN), d2, i32, f2u(-INFINITY));
    TESTINSN_bin("vrsqrts.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(NAN));
    TESTINSN_bin("vrsqrts.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(1.0));
    TESTINSN_bin("vrsqrts.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(0.0));
    TESTINSN_bin("vrsqrts.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(INFINITY));
    TESTINSN_bin("vrsqrts.f32 d0, d1, d2", d0, d1, i32, f2u(0.0), d2, i32, f2u(-INFINITY));
    TESTINSN_bin("vrsqrts.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(NAN));
    TESTINSN_bin("vrsqrts.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(1.0));
    TESTINSN_bin("vrsqrts.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(0.0));
    TESTINSN_bin("vrsqrts.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(INFINITY));
    TESTINSN_bin("vrsqrts.f32 d0, d1, d2", d0, d1, i32, f2u(INFINITY), d2, i32, f2u(-INFINITY));
    TESTINSN_bin("vrsqrts.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(NAN));
    TESTINSN_bin("vrsqrts.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(1.0));
    TESTINSN_bin("vrsqrts.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(0.0));
    TESTINSN_bin("vrsqrts.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(INFINITY));
    TESTINSN_bin("vrsqrts.f32 d0, d1, d2", d0, d1, i32, f2u(-INFINITY), d2, i32, f2u(-INFINITY));

    printf("---- VRSQRTE (fp) ----\n");
    TESTINSN_un("vrsqrte.f32 d0, d1", d0, d1, i32, f2u(3.2));
    TESTINSN_un("vrsqrte.f32 d10, d11", d10, d11, i32, f2u(3e22));
    TESTINSN_un("vrsqrte.f32 d15, d4", d15, d4, i32, f2u(3e9));
    TESTINSN_un("vrsqrte.f32 d15, d4", d15, d4, i32, f2u(-0.5));
    TESTINSN_un("vrsqrte.f32 d15, d4", d15, d4, i32, f2u(-7.1));
    TESTINSN_un("vrsqrte.f32 d12, d8", d12, d8, i32, f2u(8.0 - 1.0/1024.0));
    TESTINSN_un("vrsqrte.f32 d12, d8", d12, d8, i32, f2u(-8.0 + 1.0/1024.0));
    TESTINSN_un("vrsqrte.f32 d0, d1", d0, d1, i32, f2u(3.2));
    TESTINSN_un("vrsqrte.f32 d10, d11", d10, d11, i32, f2u(3e22));
    TESTINSN_un("vrsqrte.f32 d15, d4", d15, d4, i32, f2u(3e9));
    TESTINSN_un("vrsqrte.f32 d15, d4", d15, d4, i32, f2u(-0.5));
    TESTINSN_un("vrsqrte.f32 d15, d4", d15, d4, i32, f2u(-7.1));
    TESTINSN_un("vrsqrte.f32 d12, d8", d12, d8, i32, f2u(8.0 - 1.0/1024.0));
    TESTINSN_un("vrsqrte.f32 d12, d8", d12, d8, i32, f2u(-8.0 + 1.0/1024.0));
    TESTINSN_un("vrsqrte.f32 d0, d1", d0, d1, i32, 7);
    TESTINSN_un("vrsqrte.f32 d10, d11", d10, d11, i32, 1 << 31); 
    TESTINSN_un("vrsqrte.f32 d0, d1", d0, d1, i32, (1U << 31) + 1);
    TESTINSN_un("vrsqrte.f32 d0, d1", d0, d1, i32, (1U << 31) - 1);
    TESTINSN_un("vrsqrte.f32 d0, d14", d0, d14, i32, 0x30a0bcef);
    TESTINSN_un("vrsqrte.f32 d0, d1", d0, d1, i32, 7);
    TESTINSN_un("vrsqrte.f32 d10, d11", d10, d11, i32, 1 << 31); 
    TESTINSN_un("vrsqrte.f32 d0, d1", d0, d1, i32, (1U << 31) + 1);
    TESTINSN_un("vrsqrte.f32 d0, d1", d0, d1, i32, (1U << 31) - 1);
    TESTINSN_un("vrsqrte.f32 d0, d14", d0, d14, i32, 0x30a0bcef);
    TESTINSN_un("vrsqrte.f32 d0, d1", d0, d1, i32, f2u(NAN));
    TESTINSN_un("vrsqrte.f32 d0, d1", d0, d1, i32, f2u(0.0));
    TESTINSN_un("vrsqrte.f32 d0, d1", d0, d1, i32, f2u(INFINITY));
    TESTINSN_un("vrsqrte.f32 d0, d1", d0, d1, i32, f2u(-INFINITY));

    return 0;
}
