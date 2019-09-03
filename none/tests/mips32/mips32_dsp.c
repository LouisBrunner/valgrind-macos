#include <stdio.h>
/* Independent tests for each DSP instruction from MIPS32 DSP ASE instruction
   set */

unsigned int mem[] = {
   0x121f1e1f, 0, 3, -1,
   0x232f2e2f, 0x242c2b2b, 0x252a2e2b, 0x262d2d2a,
   0x3f343f3e, 0x3e353d3c, 0x363a3c3b, 0x3b373b3a,
   0x454f4e45, 0x4e464d46, 0x474d474c, 0x4a484a4c
};

void ppMem(unsigned int* _mem, int _len)
{
   int i;
   printf("MEM:\n");
   for (i = 0; i < _len; i=i+4)
   {
      printf("0x%08x, 0x%08x, 0x%08x, 0x%08x\n",
            _mem[i], _mem[i+1], _mem[i+2], _mem[i+3]);
   }
   _mem[0] = 0x121f1e1f;
   _mem[1] = 0;
   _mem[2] = 3;
   _mem[3] = -1;
   _mem[4] = 0x232f2e2f;
   _mem[5] = 0x242c2b2b;
   _mem[6] = 0x252a2e2b;
   _mem[7] = 0x262d2d2a;
   _mem[8] = 0x3f343f3e;
   _mem[9] = 0x3e353d3c;
   _mem[10] = 0x363a3c3b;
   _mem[11] = 0x3b373b3a;
   _mem[12] = 0x454f4e45;
   _mem[13] = 0x4e464d46;
   _mem[14] = 0x474d474c;
   _mem[15] = 0x4a484a4c;
}

#define TESTDSPINST_RD_RT_DSPC(instruction, RTval, RD, RT) \
{                                                          \
   int out = 0xdeadbeef;                                   \
   int dspCtrl = 0x0;                                      \
   __asm__ volatile(                                       \
      ".set dsp; \n\t"                                     \
      "li $" #RD ", 0 \n\t"                                \
      "move $" #RT ", %2 \n\t"                             \
      "wrdsp $zero, 0x3f \n\t"                             \
      instruction " \n\t"                                  \
      "move %0, $" #RD " \n\t"                             \
      "rddsp %1, 0x3f \n\t"                                \
      : "=&r" (out), "=&r" (dspCtrl)                       \
      : "r" (RTval)                                        \
      : #RT, #RD                                           \
   );                                                      \
   printf("%s :: rd 0x%08x rt 0x%08x DSPControl 0x%x\n",   \
         instruction, out, RTval, dspCtrl);                \
}

#define TESTDSPINST_RD_RT_NODSPC(instruction, RTval, RD, RT) \
{                                                            \
   int out = 0xdeadbeef;                                     \
   __asm__ volatile(                                         \
      ".set dsp; \n\t"                                       \
      "li $" #RD ", 0 \n\t"                                  \
      "move $" #RT ", %1 \n\t"                               \
      instruction " \n\t"                                    \
      "move %0, $" #RD " \n\t"                               \
      : "=&r" (out)                                          \
      : "r" (RTval)                                          \
      : #RT, #RD                                             \
   );                                                        \
   printf("%s :: rd 0x%08x rt 0x%08x \n",                    \
         instruction, out, RTval);                           \
}

#define TESTDSPINST_RD_RS_RT_DSPC(instruction, RSval, RTval, RD, RS, RT) \
{                                                                        \
   int out = 0xdeadbeef;                                                 \
   int dspCtrl = 0x0;                                                    \
   __asm__ volatile(                                                     \
      ".set dsp; \n\t"                                                   \
      "li $" #RD ", 0 \n\t"                                              \
      "wrdsp $zero, 0x3f \n\t"                                           \
      "move $" #RS ", %2 \n\t"                                           \
      "move $" #RT ", %3 \n\t"                                           \
      instruction " \n\t"                                                \
      "move %0, $" #RD " \n\t"                                           \
      "rddsp %1, 0x3f \n\t"                                              \
      : "=&r" (out), "=&r" (dspCtrl)                                     \
      : "r" (RSval), "r"(RTval)                                          \
      : #RD, #RS, #RT                                                    \
   );                                                                    \
   printf("%s :: rs 0x%08x rt 0x%08x out 0x%08x DSPCtrl 0x%08x\n",       \
        instruction, RSval, RTval, out, dspCtrl);                        \
}

#define TESTDSPINST_BPOSGE32(instruction, RDval, POSval, RD, POSreg) \
{                                                                    \
   unsigned int out = 0;                                             \
   __asm__ volatile(                                                 \
      ".set dsp; \n\t"                                               \
      "move $" #POSreg ", %1 \n\t"                                   \
      "wrdsp $" #POSreg ", 0x3f \n\t"                                \
      "move $" #RD ", %2 \n\t"                                       \
      instruction" end"instruction#RDval" \n\t"                      \
      "nop \n\t"                                                     \
      "addiu $" #RD ", $" #RD", 5 \n\t"                              \
      "end"instruction#RDval": \n\t"                                 \
      "addiu $" #RD ", $" #RD", 1 \n\t"                              \
      "move %0, $" #RD " \n\t"                                       \
      : "=&r" (out)                                                  \
      : "r" (POSval), "r" (RDval)                                    \
      : #RD, #POSreg                                                 \
      );                                                             \
      printf(instruction" :: %d, POSval: %d\n",                      \
        out, POSval);                                                \
}

#define TESTDSPINST_RS_RT_DSPC(instruction, RSval, RTval, RS, RT) \
{                                                                 \
   int dspCtrl = 0x0;                                             \
   __asm__ volatile(                                              \
      ".set dsp; \n\t"                                            \
      "wrdsp $zero, 0x3f \n\t"                                    \
      "move $" #RS ", %1 \n\t"                                    \
      "move $" #RT ", %2 \n\t"                                    \
      instruction " \n\t"                                         \
      "rddsp %0, 0x3f \n\t"                                       \
      : "=&r" (dspCtrl)                                           \
      : "r" (RSval), "r"(RTval)                                   \
      : #RS, #RT                                                  \
   );                                                             \
   printf("%s :: rs 0x%08x rt 0x%08x DSPCtrl 0x%08x \n",          \
        instruction, RSval, RTval, dspCtrl);                      \
}

#define TESTDSPINST_RD_RS_RT_NODSPC(instruction, RSval, RTval, RD, RS, RT) \
{                                                                          \
   int out = 0xdeadbeef;                                                   \
   __asm__ volatile(                                                       \
      ".set dsp; \n\t"                                                     \
      "li $" #RD ", 0 \n\t"                                                \
      "move $" #RS ", %1 \n\t"                                             \
      "move $" #RT ", %2 \n\t"                                             \
      instruction " \n\t"                                                  \
      "move %0, $" #RD " \n\t"                                             \
      : "=&r" (out)                                                        \
      : "r" (RSval), "r"(RTval)                                            \
      : #RD, #RS, #RT                                                      \
   );                                                                      \
   printf("%s :: rs 0x%08x rt 0x%08x out 0x%08x\n",                        \
        instruction, RSval, RTval, out);                                   \
}

#define TESTDSPINST_AC_RS_RT_DSPC(instruction, ac, RSval, RTval, HIval, LOval, \
                                  RS, RT)                                      \
{                                                                              \
   int out_hi = 0xdeadbeef;                                                    \
   int out_lo = 0xdeadbeef;                                                    \
   int dspCtrl = 0x0;                                                          \
   __asm__ volatile(                                                           \
      ".set dsp; \n\t"                                                         \
      "move $" #RS ", %5 \n\t"                                                 \
      "move $" #RT ", %6 \n\t"                                                 \
      "mthi $" #RS", $" ac " \n\t"                                             \
      "mtlo $" #RT", $" ac " \n\t"                                             \
      "move $" #RS ", %3 \n\t"                                                 \
      "move $" #RT ", %4 \n\t"                                                 \
      "wrdsp $zero, 0x3f \n\t"                                                 \
      instruction "  \n\t"                                                     \
      "rddsp %2, 0x3f \n\t"                                                    \
      "mfhi %0, $" ac " \n\t"                                                  \
      "mflo %1, $" ac " \n\t"                                                  \
      : "=&r" (out_hi), "=&r" (out_lo), "=&r" (dspCtrl)                        \
      : "r" (RSval), "r"(RTval), "r" (HIval), "r"(LOval)                       \
      : #RS, #RT                                                               \
   );                                                                          \
   printf("%s :: rs 0x%08x rt 0x%08x inHI 0x%08x inLO 0x%08x outHI 0x%08x outLO\
          0x%08x dspCtrl 0x%08x\n",instruction, RSval, RTval, HIval, LOval,    \
          out_hi, out_lo, dspCtrl);                                            \
}

#define TESTDSPINST_AC_RS_RT_NODSPC(instruction, ac, RSval, RTval, HIval,      \
                                    LOval, RS, RT)                             \
{                                                                              \
   int out_hi = 0xdeadbeef;                                                    \
   int out_lo = 0xdeadbeef;                                                    \
   __asm__ volatile(                                                           \
      ".set dsp; \n\t"                                                         \
      "move $" #RS ", %4 \n\t"                                                 \
      "move $" #RT ", %5 \n\t"                                                 \
      "mthi $" #RS", $" ac " \n\t"                                             \
      "mtlo $" #RT", $" ac " \n\t"                                             \
      "move $" #RS ", %2 \n\t"                                                 \
      "move $" #RT ", %3 \n\t"                                                 \
      instruction " \n\t"                                                      \
      "mfhi %0, $" ac " \n\t"                                                  \
      "mflo %1, $" ac " \n\t"                                                  \
      : "=&r" (out_hi), "=&r" (out_lo)                                         \
      : "r" (RSval), "r"(RTval), "r" (HIval), "r"(LOval)                       \
      : #RS, #RT                                                               \
   );                                                                          \
   printf("%s :: rs 0x%08x rt 0x%08x inHI 0x%08x inLO 0x%08x outHI 0x%08x outLO\
          0x%08x \n",instruction, RSval, RTval, HIval, LOval, out_hi, out_lo); \
}

#define TESTDSPINST_EXT(instruction, ac, RT, HIval, LOval, size, pos) \
{                                                                     \
   int out = 0xdeadbeef;                                              \
   int dspCtrl = 0x0;                                                 \
   __asm__ volatile(                                                  \
      ".set dsp; \n\t"                                                \
      "move $" #RT ", %2 \n\t"                                        \
      "wrdsp $" #RT ", 0x3f \n\t"                                     \
      "move $" #RT ", %3 \n\t"                                        \
      "mthi $" #RT", $" ac " \n\t"                                    \
      "move $" #RT ", %4 \n\t"                                        \
      "mtlo $" #RT", $" ac " \n\t"                                    \
      instruction " \n\t"                                             \
      "rddsp %1, 0x3f \n\t"                                           \
      "move %0, $" #RT " \n\t"                                        \
      : "=&r" (out), "=&r" (dspCtrl)                                  \
      : "r" (pos), "r" (HIval), "r" (LOval)                           \
      : #RT                                                           \
   );                                                                 \
   printf("%s :: rt 0x%08x %s 0x%08x%08x size %2d DSPCtrl 0x%08x\n",  \
       instruction, out, ac, HIval, LOval, size, dspCtrl);            \
}

#define TESTDSPINST_EXTV(instruction, ac, RT, HIval, LOval, RS, RSval, pos) \
{                                                                           \
   int out = 0xdeadbeef;                                                    \
   int dspCtrl = 0x0;                                                       \
   __asm__ volatile(                                                        \
      ".set dsp; \n\t"                                                      \
      "move $" #RS ", %5 \n\t"                                              \
      "move $" #RT ", %2 \n\t"                                              \
      "wrdsp $" #RT ", 0x3f \n\t"                                           \
      "move $" #RT ", %3 \n\t"                                              \
      "mthi $" #RT", $" ac " \n\t"                                          \
      "move $" #RT ", %4 \n\t"                                              \
      "mtlo $" #RT", $" ac " \n\t"                                          \
      instruction " \n\t"                                                   \
      "rddsp %1, 0x3f \n\t"                                                 \
      "move %0, $" #RT " \n\t"                                              \
      : "=&r" (out), "=&r" (dspCtrl)                                        \
      : "r" (pos), "r" (HIval), "r" (LOval), "r" (RSval)                    \
      : #RT, #RS                                                            \
   );                                                                       \
   printf("%s :: rt 0x%08x %s 0x%08x%08x rs 0x%08x DSPCtrl 0x%08x\n",       \
          instruction, out, ac, HIval, LOval, RSval, dspCtrl);              \
}

#define TESTDSPINST_INSV(instruction, RTval, RSval, RT, RS, _pos, _size)       \
{                                                                              \
   unsigned int out;                                                           \
   __asm__ volatile(                                                           \
      ".set dsp; \n\t"                                                         \
      "move $" #RS ", %3 \n\t"                                                 \
      "wrdsp $" #RS ", 0x1 \n\t"                                               \
      "move $" #RS ", %4 \n\t"                                                 \
      "wrdsp $" #RS ", 0x2 \n\t"                                               \
      "move $" #RS", %1 \n\t"                                                  \
      "move $" #RT", %2 \n\t"                                                  \
      "insv $" #RT ", $" #RS " \n\t"                                           \
      "move %0, $" #RT " \n\t"                                                 \
     : "=&r" (out)                                                             \
     : "r" (RSval), "r" (RTval), "r" (_pos), "r" (_size)                       \
     : #RS, #RT                                                                \
   );                                                                          \
   printf("insv :: out: 0x%08x rtIN 0x%08x rsIN 0x%08x posI %2d sizeI %2d \n", \
         out, RTval, RSval, _pos, _size>>7); \
}

#define TESTDSPINST_LWX(index, RT, RS)                 \
{                                                      \
    unsigned int out;                                  \
   __asm__ volatile(                                   \
      ".set dsp; \n\t"                                 \
     "move $" #RS", %1 \n\t"                           \
     "move $" #RT", %2 \n\t"                           \
     "lwx %0, $" #RT "($"#RS") \n\t"                   \
    : "=&r" (out)                                      \
    : "r" (mem), "r" (index)                           \
    : #RT, #RS, "memory"                               \
    );                                                 \
   printf("lwx :: out: 0x%08x mem[%d]\n", out, index); \
}

#define TESTDSPINST_LHX(index, RT, RS)                 \
{                                                      \
    unsigned int out;                                  \
   __asm__ volatile(                                   \
      ".set dsp; \n\t"                                 \
     "move $" #RS", %1 \n\t"                           \
     "move $" #RT", %2 \n\t"                           \
     "lhx %0, $" #RT "($"#RS") \n\t"                   \
    : "=&r" (out)                                      \
    : "r" (mem), "r" (index)                           \
    : #RT, #RS, "memory"                               \
    );                                                 \
   printf("lhx :: out: 0x%08x mem[%d]\n", out, index); \
}

#define TESTDSPINST_LBUX(index, RT, RS)                 \
{                                                       \
    unsigned int out;                                   \
   __asm__ volatile(                                    \
      ".set dsp; \n\t"                                  \
     "move $" #RS", %1 \n\t"                            \
     "move $" #RT", %2 \n\t"                            \
     "lbux %0, $" #RT "($"#RS") \n\t"                   \
    : "=&r" (out)                                       \
    : "r" (mem), "r" (index)                            \
    : #RT, #RS, "memory"                                \
    );                                                  \
   printf("lbux :: out: 0x%08x mem[%d]\n", out, index); \
}

#define TESTDSPINST_HILO(ac, RSval_hi, RSval_lo)                             \
{                                                                            \
   unsigned int HI = 0xdeadbeef;                                             \
   unsigned int LO = 0xdeadbeef;                                             \
   __asm__ volatile(                                                         \
      ".set dsp; \n\t"                                                       \
      "move $t0, %2 \n\t"                                                    \
      "move $t1, %3 \n\t"                                                    \
      "mthi $t0, $" ac " \n\t"                                               \
      "mtlo $t1, $" ac " \n\t"                                               \
      "mfhi %0, $" ac " \n\t"                                                \
      "mflo %1, $" ac " \n\t"                                                \
     : "=&r" (HI), "=&r" (LO)                                                \
     : "r" (RSval_hi), "r" (RSval_lo)                                        \
     : "t0", "t1"                                                            \
   );                                                                        \
   printf("rs_hi: 0x%08x rs_lo: 0x%08x %s out HI: 0x%08x, out LO: 0x%08x\n", \
          RSval_hi, RSval_lo, ac, HI, LO);                                   \
}

#define TESTDSPINST_MTHLIP(instruction, ac, HIval, LOval, RSval, RS, pos)      \
{                                                                              \
   unsigned int outHI;                                                         \
   unsigned int outLO;                                                         \
   unsigned int dspCtrl;                                                       \
   __asm__ volatile(                                                           \
      ".set dsp; \n\t"                                                         \
      "move $" #RS ", %3\n\t"                                                  \
      "mthi $" #RS", $" ac "\n\t"                                              \
      "move $" #RS ", %4\n\t"                                                  \
      "mtlo $" #RS", $" ac "\n\t"                                              \
      "move $" #RS ", %5\n\t"                                                  \
      "wrdsp $" #RS ", 0x1 \n\t"                                               \
      "move $" #RS ", %6\n\t"                                                  \
      instruction "\n\t"                                                       \
      "mfhi %0, $" ac "\n\t"                                                   \
      "mflo %1, $" ac "\n\t"                                                   \
      "rddsp %2, 0x1 \n\t"                                                     \
     : "=&r" (outHI), "=&r" (outLO), "=&r" (dspCtrl)                           \
     : "r" (HIval), "r" (LOval), "r" (pos), "r" (RSval)                        \
     : #RS                                                                     \
   );                                                                          \
   printf("mthlip :: acIn: 0x%08x%08x rsIn 0x%08x posIn 0x%08x acOut 0x%08x%08x\
          posOut 0x%08x\n", HIval, LOval, RSval, pos, outHI, outLO, dspCtrl);  \
}

#define TESTDSPINST_PICK(instruction, instruction1, RSval, RTval, RD, RS, RT) \
{                                                                             \
   int out = 0xdeadbeef;                                                      \
   int dspCtrl1 = 0x0;                                                        \
   __asm__ volatile(                                                          \
      ".set dsp; \n\t"                                                        \
      "li $" #RD ", 0 \n\t"                                                   \
      "wrdsp $zero, 0x1f \n\t"                                                \
      "move $" #RS ", %2 \n\t"                                                \
      "move $" #RT ", %3 \n\t"                                                \
      instruction1 " \n\t"                                                    \
      "rddsp %1, 0x1f \n\t"                                                   \
      instruction " \n\t"                                                     \
      "move %0, $" #RD " \n\t"                                                \
      : "=&r" (out), "=&r" (dspCtrl1)                                         \
      : "r" (RSval), "r"(RTval)                                               \
      : #RD, #RS, #RT                                                         \
   );                                                                         \
   printf("%s :: %s rs 0x%08x rt 0x%08x out 0x%08x DSPCtrl1 0x%x\n",          \
          instruction, instruction1, RSval, RTval, out, dspCtrl1);            \
}

#define TESTDSPINST_RADDU_W_QB(instruction, RSval, RD, RS)                    \
{                                                                             \
   int out = 0xdeadbeef;                                                      \
   __asm__ volatile(                                                          \
      ".set dsp; \n\t"                                                        \
      "move $" #RS ", %1 \n\t"                                                \
      instruction " \n\t"                                                     \
      "move %0, $" #RD " \n\t"                                                \
      : "=&r" (out)                                                           \
      : "r" (RSval)                                                           \
      : #RD, #RS                                                              \
   );                                                                         \
   printf("%s :: out 0x%08x rs 0x%08x\n",                                     \
        instruction, out, RSval);                                             \
}

#define TESTDSPINST_RDDSPWRDSP(REGval, mask)                               \
{                                                                          \
   int out = 0xdeadbeef;                                                   \
   __asm__ volatile(                                                       \
      ".set dsp; \n\t"                                                     \
      "move $t0, %1 \n\t"                                                  \
      "wrdsp $t0, " #mask " \n\t"                                          \
      "rddsp %0, " #mask " \n\t"                                           \
      : "=&r" (out)                                                        \
      : "r" (REGval)                                                       \
      : "t0"                                                               \
   );                                                                      \
   printf("outVal 0x%08x inVal 0x%08x mask 0x%08x \n", out, REGval, mask); \
}

#define TESTDSPINST_RD_IMM_NODSPC(instruction, Imm, RD)           \
{                                                                 \
   int out = 0xdeadbeef;                                          \
   __asm__ volatile(                                              \
      ".set dsp; \n\t"                                            \
      "li $" #RD ", 0 \n\t"                                       \
      instruction " \n\t"                                         \
      "move %0, $" #RD " \n\t"                                    \
      : "=&r" (out)                                               \
      :                                                           \
      : #RD                                                       \
   );                                                             \
   printf("%s :: rd 0x%08x imm 0x%08x\n", instruction, out, Imm); \
}

#define TESTDSPINST_SHILO(ac, HIval, LOval, shift)                             \
{                                                                              \
   int outHI = 0xdeadbeef;                                                     \
   int outLO = 0xdeadbeef;                                                     \
   __asm__ volatile(                                                           \
      ".set dsp; \n\t"                                                         \
      "move $t0, %2 \n\t"                                                      \
      "move $t1, %3 \n\t"                                                      \
      "mthi $t0, $" ac " \n\t"                                                 \
      "mtlo $t1, $" ac " \n\t"                                                 \
      "shilo $" ac ", " #shift " \n\t"                                         \
      "mfhi %0, $" ac " \n\t"                                                  \
      "mflo %1, $" ac " \n\t"                                                  \
      : "=&r" (outHI), "=&r" (outLO)                                           \
      : "r" (HIval), "r" (LOval)                                               \
      : "t0", "t1"                                                             \
   );                                                                          \
   printf("shilo %s, %3d inAcc = 0x%08x%08x outAcc = 0x%08x%08x\n", ac, shift, \
          HIval, LOval, outHI, outLO);                                         \
}

#define TESTDSP_SHILOV(ac, HIval, LOval, RSval, RS)                            \
{                                                                              \
   int outHI = 0xdeadbeef;                                                     \
   int outLO = 0xdeadbeef;                                                     \
   __asm__ volatile(                                                           \
      ".set dsp; \n\t"                                                         \
      "move $" #RS ", %2 \n\t"                                                 \
      "mthi $" #RS ", $" ac " \n\t"                                            \
      "move $" #RS ", %3 \n\t"                                                 \
      "mtlo $" #RS ", $" ac " \n\t"                                            \
      "move $" #RS ", %4 \n\t"                                                 \
      "shilov $" ac ", $" #RS " \n\t"                                          \
      "mfhi %0, $" ac " \n\t"                                                  \
      "mflo %1, $" ac " \n\t"                                                  \
      : "=&r" (outHI), "=&r" (outLO)                                           \
      : "r" (HIval), "r" (LOval), "r" (RSval)                                  \
      : #RS                                                                    \
   );                                                                          \
   printf("shilov %s, rs 0x%08x inAcc = 0x%08x%08x outAcc = 0x%08x%08x\n", ac, \
          RSval, HIval, LOval, outHI, outLO);                                  \
}

#define TESTDSPINST_RD_RT_SA_DSPC(instruction, RTval, SAval, RD, RT)        \
{                                                                           \
   int out = 0xdeadbeef;                                                    \
   int dspCtrl = 0x0;                                                       \
   __asm__ volatile(                                                        \
      ".set dsp; \n\t"                                                      \
      "li $" #RD ", 0 \n\t"                                                 \
      "wrdsp $zero, 0x3f \n\t"                                              \
      "move $" #RT ", %2 \n\t"                                              \
      instruction " \n\t"                                                   \
      "rddsp %1, 0x3f \n\t"                                                 \
      "move %0, $" #RD " \n\t"                                              \
      : "=&r" (out), "=&r" (dspCtrl)                                        \
      : "r"(RTval)                                                          \
      : #RD, #RT                                                            \
   );                                                                       \
   printf("%s :: rd 0x%08x rt 0x%08x sa %2d DSPCtrl 0x%08x\n", instruction, \
          out, RTval, SAval, dspCtrl);                                      \
}

#define TESTDSPINST_RD_RT_SA_NODSPC(instruction, RTval, SAval, RD, RT)   \
{                                                                        \
   int out = 0xdeadbeef;                                                 \
   __asm__ volatile(                                                     \
      ".set dsp; \n\t"                                                   \
      "li $" #RD ", 0 \n\t"                                              \
      "move $" #RT ", %1 \n\t"                                           \
      instruction " \n\t"                                                \
      "move %0, $" #RD " \n\t"                                           \
      : "=&r" (out)                                                      \
      : "r"(RTval)                                                       \
      : #RD, #RT                                                         \
   );                                                                    \
   printf("%s :: rd 0x%08x rt 0x%08x sa %2d\n", instruction, out, RTval, \
          SAval);                                                        \
}

#define TESTDSPINST_RD_RT_RS_DSPC(instruction, RTval, RSval, RD, RT, RS)       \
{                                                                              \
   int out = 0xdeadbeef;                                                       \
   int dspCtrl = 0x0;                                                          \
   __asm__ volatile(                                                           \
      ".set dsp; \n\t"                                                         \
      "li $" #RD ", 0 \n\t"                                                    \
      "wrdsp $zero, 0x3f \n\t"                                                 \
      "move $" #RT ", %2 \n\t"                                                 \
      "move $" #RS ", %3 \n\t"                                                 \
      instruction " \n\t"                                                      \
      "rddsp %1, 0x3f \n\t"                                                    \
      "move %0, $" #RD " \n\t"                                                 \
      : "=&r" (out), "=&r" (dspCtrl)                                           \
      : "r"(RTval), "r"(RSval)                                                 \
      : #RD, #RT, #RS                                                          \
   );                                                                          \
   printf("%s :: rd 0x%08x rt 0x%08x rs 0x%08x DSPCtrl 0x%08x\n", instruction, \
          out, RTval, RSval, dspCtrl);                                         \
}

#define TESTDSPINST_RD_RT_RS_NODSPC(instruction, RTval, RSval, RD, RT, RS)  \
{                                                                           \
   int out = 0xdeadbeef;                                                    \
   __asm__ volatile(                                                        \
      ".set dsp; \n\t"                                                      \
      "li $" #RD ", 0 \n\t"                                                 \
      "move $" #RT ", %1 \n\t"                                              \
      "move $" #RS ", %2 \n\t"                                              \
      instruction " \n\t"                                                   \
      "move %0, $" #RD " \n\t"                                              \
      : "=&r" (out)                                                         \
      : "r"(RTval), "r"(RSval)                                              \
      : #RD, #RT, #RS                                                       \
   );                                                                       \
   printf("%s :: rd 0x%08x rt 0x%08x rs 0x%08x\n", instruction, out, RTval, \
          RSval);                                                           \
}

int main(int argc, char **argv)
{
#if (__mips==32) && (__mips_isa_rev>=2)
   printf("-------- ABSQ_S.PH --------\n");
   TESTDSPINST_RD_RT_DSPC("absq_s.ph $t0, $t1", 0x00000000, t0, t1);
   TESTDSPINST_RD_RT_DSPC("absq_s.ph $t2, $t3", 0x00000286, t2, t3);
   TESTDSPINST_RD_RT_DSPC("absq_s.ph $t4, $t1", 0xfabc2435, t4, t1);
   TESTDSPINST_RD_RT_DSPC("absq_s.ph $t6, $t7", 0x73468000, t6, t7);
   TESTDSPINST_RD_RT_DSPC("absq_s.ph $t5, $t3", 0x80000000, t5, t3);
   TESTDSPINST_RD_RT_DSPC("absq_s.ph $t2, $t4", 0xffffffff, t2, t4);
   TESTDSPINST_RD_RT_DSPC("absq_s.ph $t0, $t8", 0xfff45fff, t0, t8);
   TESTDSPINST_RD_RT_DSPC("absq_s.ph $t4, $t4", 0x00000555, t4, t4);
   TESTDSPINST_RD_RT_DSPC("absq_s.ph $t0, $t1", 0x23534870, t0, t1);
   TESTDSPINST_RD_RT_DSPC("absq_s.ph $t2, $t3", 0x0555adec, t2, t3);
   TESTDSPINST_RD_RT_DSPC("absq_s.ph $t4, $t1", 0x980b7cde, t4, t1);
   TESTDSPINST_RD_RT_DSPC("absq_s.ph $t6, $t7", 0xf973437b, t6, t7);
   TESTDSPINST_RD_RT_DSPC("absq_s.ph $t5, $t3", 0x23c54b6e, t5, t3);
   TESTDSPINST_RD_RT_DSPC("absq_s.ph $t2, $t4", 0x55555555, t2, t4);
   TESTDSPINST_RD_RT_DSPC("absq_s.ph $t0, $t8", 0xc4dbfe20, t0, t8);
   TESTDSPINST_RD_RT_DSPC("absq_s.ph $t4, $t4", 0x734680bc, t4, t4);
   TESTDSPINST_RD_RT_DSPC("absq_s.ph $t0, $t1", 0x00354565, t0, t1);
   TESTDSPINST_RD_RT_DSPC("absq_s.ph $t2, $t3", 0xbacabaca, t2, t3);
   TESTDSPINST_RD_RT_DSPC("absq_s.ph $t4, $t1", 0xdba38ec9, t4, t1);
   TESTDSPINST_RD_RT_DSPC("absq_s.ph $t6, $t7", 0x0b300286, t6, t7);
   TESTDSPINST_RD_RT_DSPC("absq_s.ph $t5, $t3", 0xabababab, t5, t3);
   TESTDSPINST_RD_RT_DSPC("absq_s.ph $t2, $t4", 0x00086755, t2, t4);
   TESTDSPINST_RD_RT_DSPC("absq_s.ph $t0, $t8", 0x8f8f8f80, t0, t8);
   TESTDSPINST_RD_RT_DSPC("absq_s.ph $t4, $t4", 0xeeeeeeee, t4, t4);
   TESTDSPINST_RD_RT_DSPC("absq_s.ph $t0, $t1", 0x1bdbdbdb, t0, t1);
   TESTDSPINST_RD_RT_DSPC("absq_s.ph $t2, $t3", 0xdecadeca, t2, t3);
   TESTDSPINST_RD_RT_DSPC("absq_s.ph $t4, $t1", 0x93474bde, t4, t1);
   TESTDSPINST_RD_RT_DSPC("absq_s.ph $t6, $t7", 0xfabfabfa, t6, t7);
   TESTDSPINST_RD_RT_DSPC("absq_s.ph $t5, $t3", 0x083b3571, t5, t3);
   TESTDSPINST_RD_RT_DSPC("absq_s.ph $t2, $t4", 0xb9743941, t2, t4);
   TESTDSPINST_RD_RT_DSPC("absq_s.ph $t0, $t8", 0xbc80f924, t0, t8);
   TESTDSPINST_RD_RT_DSPC("absq_s.ph $t4, $t4", 0xcc3c201c, t4, t4);
   TESTDSPINST_RD_RT_DSPC("absq_s.ph $t0, $t1", 0x1ebaf88e, t0, t1);
   TESTDSPINST_RD_RT_DSPC("absq_s.ph $t2, $t3", 0x722d5e20, t2, t3);
   TESTDSPINST_RD_RT_DSPC("absq_s.ph $t4, $t1", 0xa1d6f791, t4, t1);
   TESTDSPINST_RD_RT_DSPC("absq_s.ph $t6, $t7", 0x7b11bee7, t6, t7);
   TESTDSPINST_RD_RT_DSPC("absq_s.ph $t5, $t3", 0xa5631488, t5, t3);
   TESTDSPINST_RD_RT_DSPC("absq_s.ph $t2, $t4", 0xb10bcc65, t2, t4);
   TESTDSPINST_RD_RT_DSPC("absq_s.ph $t0, $t8", 0x73f39fca, t0, t8);
   TESTDSPINST_RD_RT_DSPC("absq_s.ph $t4, $t4", 0x80008000, t4, t4);
   TESTDSPINST_RD_RT_DSPC("absq_s.ph $t0, $t1",     -23456, t0, t1);
   TESTDSPINST_RD_RT_DSPC("absq_s.ph $t2, $t3",  123498746, t2, t3);
   TESTDSPINST_RD_RT_DSPC("absq_s.ph $t4, $t1",        -13, t4, t1);
   TESTDSPINST_RD_RT_DSPC("absq_s.ph $t6, $t7",       -237, t6, t7);

   printf("-------- ABSQ_S.W --------\n");
   TESTDSPINST_RD_RT_DSPC("absq_s.w $t0, $t1", 0x00000000, t0, t1);
   TESTDSPINST_RD_RT_DSPC("absq_s.w $t2, $t3", 0x00000286, t2, t3);
   TESTDSPINST_RD_RT_DSPC("absq_s.w $t4, $t1", 0xfabc2435, t4, t1);
   TESTDSPINST_RD_RT_DSPC("absq_s.w $t6, $t7", 0x73468000, t6, t7);
   TESTDSPINST_RD_RT_DSPC("absq_s.w $t5, $t3", 0x80000000, t5, t3);
   TESTDSPINST_RD_RT_DSPC("absq_s.w $t2, $t4", 0xffffffff, t2, t4);
   TESTDSPINST_RD_RT_DSPC("absq_s.w $t0, $t8", 0xfff45fff, t0, t8);
   TESTDSPINST_RD_RT_DSPC("absq_s.w $t4, $t4", 0x00000555, t4, t4);
   TESTDSPINST_RD_RT_DSPC("absq_s.w $t0, $t1", 0x23534870, t0, t1);
   TESTDSPINST_RD_RT_DSPC("absq_s.w $t2, $t3", 0x0555adec, t2, t3);
   TESTDSPINST_RD_RT_DSPC("absq_s.w $t4, $t1", 0x980b7cde, t4, t1);
   TESTDSPINST_RD_RT_DSPC("absq_s.w $t6, $t7", 0xf973437b, t6, t7);
   TESTDSPINST_RD_RT_DSPC("absq_s.w $t5, $t3", 0x23c54b6e, t5, t3);
   TESTDSPINST_RD_RT_DSPC("absq_s.w $t2, $t4", 0x55555555, t2, t4);
   TESTDSPINST_RD_RT_DSPC("absq_s.w $t0, $t8", 0xc4dbfe20, t0, t8);
   TESTDSPINST_RD_RT_DSPC("absq_s.w $t4, $t4", 0x734680bc, t4, t4);
   TESTDSPINST_RD_RT_DSPC("absq_s.w $t0, $t1", 0x00354565, t0, t1);
   TESTDSPINST_RD_RT_DSPC("absq_s.w $t2, $t3", 0xbacabaca, t2, t3);
   TESTDSPINST_RD_RT_DSPC("absq_s.w $t4, $t1", 0xdba38ec9, t4, t1);
   TESTDSPINST_RD_RT_DSPC("absq_s.w $t6, $t7", 0x0b300286, t6, t7);
   TESTDSPINST_RD_RT_DSPC("absq_s.w $t5, $t3", 0xabababab, t5, t3);
   TESTDSPINST_RD_RT_DSPC("absq_s.w $t2, $t4", 0x00086755, t2, t4);
   TESTDSPINST_RD_RT_DSPC("absq_s.w $t0, $t8", 0x8f8f8f80, t0, t8);
   TESTDSPINST_RD_RT_DSPC("absq_s.w $t4, $t4", 0xeeeeeeee, t4, t4);
   TESTDSPINST_RD_RT_DSPC("absq_s.w $t0, $t1", 0x1bdbdbdb, t0, t1);
   TESTDSPINST_RD_RT_DSPC("absq_s.w $t2, $t3", 0xdecadeca, t2, t3);
   TESTDSPINST_RD_RT_DSPC("absq_s.w $t4, $t1", 0x93474bde, t4, t1);
   TESTDSPINST_RD_RT_DSPC("absq_s.w $t6, $t7", 0xfabfabfa, t6, t7);
   TESTDSPINST_RD_RT_DSPC("absq_s.w $t5, $t3", 0x083b3571, t5, t3);
   TESTDSPINST_RD_RT_DSPC("absq_s.w $t2, $t4", 0xb9743941, t2, t4);
   TESTDSPINST_RD_RT_DSPC("absq_s.w $t0, $t8", 0xbc80f924, t0, t8);
   TESTDSPINST_RD_RT_DSPC("absq_s.w $t4, $t4", 0xcc3c201c, t4, t4);
   TESTDSPINST_RD_RT_DSPC("absq_s.w $t0, $t1", 0x1ebaf88e, t0, t1);
   TESTDSPINST_RD_RT_DSPC("absq_s.w $t2, $t3", 0x722d5e20, t2, t3);
   TESTDSPINST_RD_RT_DSPC("absq_s.w $t4, $t1", 0xa1d6f791, t4, t1);
   TESTDSPINST_RD_RT_DSPC("absq_s.w $t6, $t7", 0x7b11bee7, t6, t7);
   TESTDSPINST_RD_RT_DSPC("absq_s.w $t5, $t3", 0xa5631488, t5, t3);
   TESTDSPINST_RD_RT_DSPC("absq_s.w $t2, $t4", 0xb10bcc65, t2, t4);
   TESTDSPINST_RD_RT_DSPC("absq_s.w $t0, $t8", 0x73f39fca, t0, t8);
   TESTDSPINST_RD_RT_DSPC("absq_s.w $t4, $t4", 0x80000000, t4, t4);
   TESTDSPINST_RD_RT_DSPC("absq_s.w $t0, $t1",     -23456, t0, t1);
   TESTDSPINST_RD_RT_DSPC("absq_s.w $t2, $t3",  123498746, t2, t3);
   TESTDSPINST_RD_RT_DSPC("absq_s.w $t4, $t1",        -13, t4, t1);
   TESTDSPINST_RD_RT_DSPC("absq_s.w $t6, $t7",       -237, t6, t7);

   printf("-------- ADDQ.PH --------\n");
   TESTDSPINST_RD_RS_RT_DSPC("addq.ph $t0, $t1, $t2", 0x00000000, 0x00000000,
                             t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("addq.ph $t2, $t3, $t4", 0x00045fb2, 0x00000286,
                             t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("addq.ph $t4, $t1, $t5", 0x00002435, 0xffff3421,
                             t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("addq.ph $t6, $t7, $t3", 0x07654cb8, 0x734680bc,
                             t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("addq.ph $t5, $t3, $t2", 0xf973437b, 0x80000000,
                             t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("addq.ph $t2, $t4, $t8", 0x00010001, 0xfa3259ff,
                             t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("addq.ph $t0, $t8, $t0", 0x7fff7322, 0x77ff7fff,
                             t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("addq.ph $t4, $t6, $t1", 0x0034c420, 0x00000555,
                             t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("addq.ph $t2, $t3, $t4", 0x00000004, 1073741824,
                             t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("addq.ph $t4, $t1, $t5", 0x80002435, 0x80003421,
                             t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("addq.ph $t6, $t7, $t3", 0x76548000, 0x73468000,
                             t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("addq.ph $t5, $t3, $t2", 0x80000000, 0x80620020,
                             t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("addq.ph $t2, $t4, $t8", 0x00010001, 0xffffffff,
                             t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("addq.ph $t0, $t8, $t0", 0x7fff7fff, 0x7fff7fff,
                             t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("addq.ph $t4, $t6, $t1", 0x0000c420, 0x00000555,
                             t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("addq.ph $t0, $t1, $t2", 0x000a2300, 0x83bc1900,
                             t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("addq.ph $t2, $t3, $t4", 0x80000000, 0x80000000,
                             t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("addq.ph $t4, $t1, $t5", 0xaaaaaaaa, 0x55555555,
                             t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("addq.ph $t6, $t7, $t3", 0x00000018, 0xffff2435,
                             t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("addq.ph $t5, $t3, $t2", 0xbabababa, 0xabababab,
                             t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("addq.ph $t2, $t4, $t8", 0xf0f0f0f0, 0xfc79b4d2,
                             t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("addq.ph $t0, $t8, $t0", 0xfbde3976, 0x00000000,
                             t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("addq.ph $t4, $t6, $t1", 0x23534870, 0x00354565,
                             t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("addq.ph $t0, $t1, $t2", 0x980b7cde, 0x00086755,
                             t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("addq.ph $t2, $t3, $t4", 0x00000018, 0x8f8f8f8f,
                             t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("addq.ph $t4, $t1, $t5", 0x92784656, 0xeeeeeeee,
                             t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("addq.ph $t6, $t7, $t3", 0xcacacaca, 0x1bdbdbdb,
                             t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("addq.ph $t5, $t3, $t2", 0xbacabaca, 0xdecadeca,
                             t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("addq.ph $t2, $t4, $t8", 0x12fadeb4, 0x93474bde,
                             t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("addq.ph $t0, $t8, $t0", 0x7c000790, 0xfc0007ff,
                             t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("addq.ph $t4, $t6, $t1", 0xffffffff, 0xffffffff,
                             t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("addq.ph $t0, $t1, $t2", 0xf2f4df1f, 0xcb4ab48f,
                             t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("addq.ph $t2, $t3, $t4", 0x435f909a, 0xaf8f7e18,
                             t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("addq.ph $t4, $t1, $t5", 0x2106ba5f, 0x87df4510,
                             t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("addq.ph $t6, $t7, $t3", 0x246a6376, 0xabf4e8e1,
                             t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("addq.ph $t5, $t3, $t2", 0x1046a1a3, 0xf4c0eeac,
                             t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("addq.ph $t2, $t4, $t8", 0x638ca515, 0x006a54f2,
                             t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("addq.ph $t0, $t8, $t0", 0xf63e7a9d, 0x79f74493,
                             t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("addq.ph $t4, $t6, $t1", 0xbd6845cd, 0x9c09e313,
                             t4, t6, t1);

   printf("-------- ADDQ_S.PH --------\n");
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.ph $t0, $t1, $t2", 0x00000000, 0x00000000,
                             t0, t1, t2);               
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.ph $t2, $t3, $t4", 0x00045fb2, 0x00000286,
                             t2, t3, t4);               
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.ph $t4, $t1, $t5", 0x00002435, 0xffff3421,
                             t4, t1, t5);               
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.ph $t6, $t7, $t3", 0x07654cb8, 0x734680bc,
                             t6, t7, t3);               
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.ph $t5, $t3, $t2", 0xf973437b, 0x80000000,
                             t5, t3, t2);               
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.ph $t0, $t1, $t2", 0x00010001, 0xfa3259ff,
                             t0, t1, t2);               
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.ph $t2, $t3, $t4", 0x7fff7322, 0x77ff7fff,
                             t2, t3, t4);               
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.ph $t4, $t1, $t5", 0x0034c420, 0x00000555,
                             t4, t1, t5);               
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.ph $t6, $t7, $t3", 0x00000004, 1073741824,
                             t6, t7, t3);               
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.ph $t5, $t3, $t2", 0x80002435, 0x80003421,
                             t5, t3, t2);               
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.ph $t2, $t4, $t8", 0x76548000, 0x73468000,
                             t2, t4, t8);               
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.ph $t0, $t8, $t0", 0x80000000, 0x80620020,
                             t0, t8, t0);               
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.ph $t4, $t6, $t1", 0x00010001, 0xffffffff,
                             t4, t6, t1);               
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.ph $t2, $t3, $t4", 0x7fff7fff, 0x7fff7fff,
                             t2, t3, t4);               
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.ph $t4, $t1, $t5", 0x0000c420, 0x00000555,
                             t4, t1, t5);               
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.ph $t6, $t7, $t3", 0x000a2300, 0x83bc1900,
                             t6, t7, t3);               
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.ph $t5, $t3, $t2", 0x80000000, 0x80000000,
                             t5, t3, t2);               
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.ph $t2, $t4, $t8", 0xaaaaaaaa, 0x55555555,
                             t2, t4, t8);               
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.ph $t0, $t8, $t0", 0x00000018, 0xffff2435,
                             t0, t8, t0);               
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.ph $t4, $t6, $t1", 0xbabababa, 0xabababab,
                             t4, t6, t1);               
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.ph $t0, $t1, $t2", 0xf0f0f0f0, 0xfc79b4d2,
                             t0, t1, t2);               
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.ph $t2, $t3, $t4", 0xfbde3976, 0x00000000,
                             t2, t3, t4);               
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.ph $t4, $t1, $t5", 0x23534870, 0x00354565,
                             t4, t1, t5);               
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.ph $t6, $t7, $t3", 0x980b7cde, 0x00086755,
                             t6, t7, t3);               
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.ph $t5, $t3, $t2", 0x00000018, 0x8f8f8f8f,
                             t5, t3, t2);               
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.ph $t2, $t4, $t8", 0x92784656, 0xeeeeeeee,
                             t2, t4, t8);               
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.ph $t0, $t8, $t0", 0xcacacaca, 0x1bdbdbdb,
                             t0, t8, t0);               
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.ph $t4, $t6, $t1", 0xbacabaca, 0xdecadeca,
                             t4, t6, t1);               
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.ph $t0, $t1, $t2", 0x12fadeb4, 0x93474bde,
                             t0, t1, t2);               
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.ph $t2, $t3, $t4", 0x7c000790, 0xfc0007ff,
                             t2, t3, t4);               
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.ph $t4, $t1, $t5", 0xffffffff, 0xffffffff,
                             t4, t1, t5);               
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.ph $t6, $t7, $t3", 0xf2f4df1f, 0xcb4ab48f,
                             t6, t7, t3);               
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.ph $t5, $t3, $t2", 0x435f909a, 0xaf8f7e18,
                             t5, t3, t2);               
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.ph $t2, $t4, $t8", 0x2106ba5f, 0x87df4510,
                             t2, t4, t8);               
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.ph $t0, $t8, $t0", 0x246a6376, 0xabf4e8e1,
                             t0, t8, t0);               
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.ph $t4, $t6, $t1", 0x1046a1a3, 0xf4c0eeac,
                             t4, t6, t1);               
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.ph $t0, $t1, $t2", 0x638ca515, 0x006a54f2,
                             t0, t1, t2);               
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.ph $t2, $t3, $t4", 0xf63e7a9d, 0x79f74493,
                             t2, t3, t4);               
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.ph $t4, $t1, $t5", 0xbd6845cd, 0x9c09e313,
                             t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.ph $t6, $t7, $t3", 0x234ba291, 0xbb64981c, 
                             t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.ph $t5, $t3, $t2", 0x120934de, 0xad2c7601,
                             t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.ph $t2, $t4, $t8", 0xf5643908, 0xbaff3492,
                             t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.ph $t0, $t8, $t0", 0x88503331, 0xd60e34a2,
                             t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.ph $t4, $t6, $t1", 0x7b5309ac, 0xc5487201,
                             t4, t6, t1);

   printf("-------- ADDQ_S.W --------\n");
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.w $t0, $t1, $t2", 0x00000000, 0x00000000,
                             t0, t1, t2);              
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.w $t2, $t3, $t4", 0x00045fb2, 0x00000286,
                             t2, t3, t4);              
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.w $t4, $t1, $t5", 0x00002435, 0xffff3421,
                             t4, t1, t5);              
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.w $t6, $t7, $t3", 0x07654cb8, 0x734680bc,
                             t6, t7, t3);              
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.w $t5, $t3, $t2", 0xf973437b, 0x80000000,
                             t5, t3, t2);              
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.w $t2, $t4, $t8", 0x00010001, 0xfa3259ff,
                             t2, t4, t8);              
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.w $t0, $t8, $t0", 0x7fff7322, 0x77ff7fff,
                             t0, t8, t0);              
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.w $t4, $t6, $t1", 0x0034c420, 0x00000555,
                             t4, t6, t1);              
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.w $t2, $t3, $t4", 0x00000004, 1073741824,
                             t2, t3, t4);              
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.w $t4, $t1, $t5", 0x80002435, 0x80003421,
                             t4, t1, t5);              
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.w $t6, $t7, $t3", 0x76548000, 0x73468000,
                             t6, t7, t3);              
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.w $t5, $t3, $t2", 0x80000000, 0x80620020,
                             t5, t3, t2);              
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.w $t2, $t4, $t8", 0x00010001, 0xffffffff,
                             t2, t4, t8);              
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.w $t0, $t8, $t0", 0x7fff7fff, 0x7fff7fff,
                             t0, t8, t0);              
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.w $t4, $t6, $t1", 0x0000c420, 0x00000555,
                             t4, t6, t1);              
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.w $t0, $t1, $t2", 0x000a2300, 0x83bc1900,
                             t0, t1, t2);              
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.w $t2, $t3, $t4", 0x80000000, 0x80000000,
                             t2, t3, t4);              
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.w $t4, $t1, $t5", 0xaaaaaaaa, 0x55555555,
                             t4, t1, t5);              
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.w $t6, $t7, $t3", 0x00000018, 0xffff2435,
                             t6, t7, t3);              
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.w $t5, $t3, $t2", 0xbabababa, 0xabababab,
                             t5, t3, t2);              
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.w $t2, $t4, $t8", 0xf0f0f0f0, 0xfc79b4d2,
                             t2, t4, t8);              
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.w $t0, $t8, $t0", 0xfbde3976, 0x00000000,
                             t0, t8, t0);              
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.w $t4, $t6, $t1", 0x23534870, 0x00354565,
                             t4, t6, t1);              
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.w $t0, $t1, $t2", 0x980b7cde, 0x00086755,
                             t0, t1, t2);              
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.w $t2, $t3, $t4", 0x00000018, 0x8f8f8f8f,
                             t2, t3, t4);              
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.w $t4, $t1, $t5", 0x92784656, 0xeeeeeeee,
                             t4, t1, t5);              
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.w $t6, $t7, $t3", 0xcacacaca, 0x1bdbdbdb,
                             t6, t7, t3);              
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.w $t5, $t3, $t2", 0xbacabaca, 0xdecadeca,
                             t5, t3, t2);              
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.w $t2, $t4, $t8", 0x12fadeb4, 0x93474bde,
                             t2, t4, t8);              
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.w $t0, $t8, $t0", 0x7c000790, 0xfc0007ff,
                             t0, t8, t0);              
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.w $t4, $t6, $t1", 0xffffffff, 0xffffffff,
                             t4, t6, t1);              
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.w $t0, $t1, $t2", 0xf2f4df1f, 0xcb4ab48f,
                             t0, t1, t2);              
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.w $t2, $t3, $t4", 0x435f909a, 0xaf8f7e18,
                             t2, t3, t4);              
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.w $t4, $t1, $t5", 0x2106ba5f, 0x87df4510,
                             t4, t1, t5);              
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.w $t6, $t7, $t3", 0x246a6376, 0xabf4e8e1,
                             t6, t7, t3);              
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.w $t5, $t3, $t2", 0x1046a1a3, 0xf4c0eeac,
                             t5, t3, t2);              
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.w $t2, $t4, $t8", 0x638ca515, 0x006a54f2,
                             t2, t4, t8);              
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.w $t0, $t8, $t0", 0xf63e7a9d, 0x79f74493,
                             t0, t8, t0);              
   TESTDSPINST_RD_RS_RT_DSPC("addq_s.w $t4, $t6, $t1", 0xbd6845cd, 0x9c09e313,
                             t4, t6, t1);

   printf("-------- ADDSC --------\n");
   TESTDSPINST_RD_RS_RT_DSPC("addsc $t0, $t1, $t2", 0x00000000, 0x00000000,
                             t0, t1, t2);           
   TESTDSPINST_RD_RS_RT_DSPC("addsc $t2, $t3, $t4", 0x00045fb2, 0x00000286,
                             t2, t3, t4);           
   TESTDSPINST_RD_RS_RT_DSPC("addsc $t4, $t1, $t5", 0x00002435, 0xffff3421,
                             t4, t1, t5);           
   TESTDSPINST_RD_RS_RT_DSPC("addsc $t6, $t7, $t3", 0x07654cb8, 0x734680bc,
                             t6, t7, t3);           
   TESTDSPINST_RD_RS_RT_DSPC("addsc $t5, $t3, $t2", 0xf973437b, 0x80000000,
                             t5, t3, t2);           
   TESTDSPINST_RD_RS_RT_DSPC("addsc $t2, $t4, $t8", 0x00010001, 0xfa3259ff,
                             t2, t4, t8);           
   TESTDSPINST_RD_RS_RT_DSPC("addsc $t0, $t8, $t0", 0x7fff7322, 0x77ff7fff,
                             t0, t8, t0);           
   TESTDSPINST_RD_RS_RT_DSPC("addsc $t4, $t6, $t1", 0x0034c420, 0x00000555,
                             t4, t6, t1);           
   TESTDSPINST_RD_RS_RT_DSPC("addsc $t2, $t3, $t4", 0x00000004, 1073741824,
                             t2, t3, t4);           
   TESTDSPINST_RD_RS_RT_DSPC("addsc $t4, $t1, $t5", 0x80002435, 0x80003421,
                             t4, t1, t5);           
   TESTDSPINST_RD_RS_RT_DSPC("addsc $t6, $t7, $t3", 0x76548000, 0x73468000,
                             t6, t7, t3);           
   TESTDSPINST_RD_RS_RT_DSPC("addsc $t5, $t3, $t2", 0x80000000, 0x80620020,
                             t5, t3, t2);           
   TESTDSPINST_RD_RS_RT_DSPC("addsc $t2, $t4, $t8", 0x00010001, 0xffffffff,
                             t2, t4, t8);           
   TESTDSPINST_RD_RS_RT_DSPC("addsc $t0, $t8, $t0", 0x7fff7fff, 0x7fff7fff,
                             t0, t8, t0);           
   TESTDSPINST_RD_RS_RT_DSPC("addsc $t4, $t6, $t1", 0x0000c420, 0x00000555,
                             t4, t6, t1);           
   TESTDSPINST_RD_RS_RT_DSPC("addsc $t0, $t1, $t2", 0x000a2300, 0x83bc1900,
                             t0, t1, t2);           
   TESTDSPINST_RD_RS_RT_DSPC("addsc $t2, $t3, $t4", 0x80000000, 0x80000000,
                             t2, t3, t4);           
   TESTDSPINST_RD_RS_RT_DSPC("addsc $t4, $t1, $t5", 0xaaaaaaaa, 0x55555555,
                             t4, t1, t5);           
   TESTDSPINST_RD_RS_RT_DSPC("addsc $t6, $t7, $t3", 0x00000018, 0xffff2435,
                             t6, t7, t3);           
   TESTDSPINST_RD_RS_RT_DSPC("addsc $t5, $t3, $t2", 0xbabababa, 0xabababab,
                             t5, t3, t2);           
   TESTDSPINST_RD_RS_RT_DSPC("addsc $t2, $t4, $t8", 0xf0f0f0f0, 0xfc79b4d2,
                             t2, t4, t8);           
   TESTDSPINST_RD_RS_RT_DSPC("addsc $t0, $t8, $t0", 0xfbde3976, 0x00000000,
                             t0, t8, t0);           
   TESTDSPINST_RD_RS_RT_DSPC("addsc $t4, $t6, $t1", 0x23534870, 0x00354565,
                             t4, t6, t1);           
   TESTDSPINST_RD_RS_RT_DSPC("addsc $t0, $t1, $t2", 0x980b7cde, 0x00086755,
                             t0, t1, t2);           
   TESTDSPINST_RD_RS_RT_DSPC("addsc $t2, $t3, $t4", 0x00000018, 0x8f8f8f8f,
                             t2, t3, t4);           
   TESTDSPINST_RD_RS_RT_DSPC("addsc $t4, $t1, $t5", 0x92784656, 0xeeeeeeee,
                             t4, t1, t5);           
   TESTDSPINST_RD_RS_RT_DSPC("addsc $t6, $t7, $t3", 0xcacacaca, 0x1bdbdbdb,
                             t6, t7, t3);           
   TESTDSPINST_RD_RS_RT_DSPC("addsc $t5, $t3, $t2", 0xbacabaca, 0xdecadeca,
                             t5, t3, t2);           
   TESTDSPINST_RD_RS_RT_DSPC("addsc $t2, $t4, $t8", 0x12fadeb4, 0x93474bde,
                             t2, t4, t8);           
   TESTDSPINST_RD_RS_RT_DSPC("addsc $t0, $t8, $t0", 0x7c000790, 0xfc0007ff,
                             t0, t8, t0);           
   TESTDSPINST_RD_RS_RT_DSPC("addsc $t4, $t6, $t1", 0xffffffff, 0xffffffff,
                             t4, t6, t1);           
   TESTDSPINST_RD_RS_RT_DSPC("addsc $t0, $t1, $t2", 0xf2f4df1f, 0xcb4ab48f,
                             t0, t1, t2);           
   TESTDSPINST_RD_RS_RT_DSPC("addsc $t2, $t3, $t4", 0x435f909a, 0xaf8f7e18,
                             t2, t3, t4);           
   TESTDSPINST_RD_RS_RT_DSPC("addsc $t4, $t1, $t5", 0x2106ba5f, 0x87df4510,
                             t4, t1, t5);           
   TESTDSPINST_RD_RS_RT_DSPC("addsc $t6, $t7, $t3", 0x246a6376, 0xabf4e8e1,
                             t6, t7, t3);           
   TESTDSPINST_RD_RS_RT_DSPC("addsc $t5, $t3, $t2", 0x1046a1a3, 0xf4c0eeac,
                             t5, t3, t2);           
   TESTDSPINST_RD_RS_RT_DSPC("addsc $t2, $t4, $t8", 0x638ca515, 0x006a54f2,
                             t2, t4, t8);           
   TESTDSPINST_RD_RS_RT_DSPC("addsc $t0, $t8, $t0", 0xf63e7a9d, 0x79f74493,
                             t0, t8, t0);           
   TESTDSPINST_RD_RS_RT_DSPC("addsc $t4, $t6, $t1", 0xbd6845cd, 0x9c09e313,
                             t4, t6, t1);

   printf("-------- ADDU.QB --------\n");
   TESTDSPINST_RD_RS_RT_DSPC("addu.qb $t0, $t1, $t2", 0x00000000, 0x00000000,
                             t0, t1, t2);             
   TESTDSPINST_RD_RS_RT_DSPC("addu.qb $t2, $t3, $t4", 0x00045fb2, 0x00000286,
                             t2, t3, t4);             
   TESTDSPINST_RD_RS_RT_DSPC("addu.qb $t4, $t1, $t5", 0x00002435, 0xffff3421,
                             t4, t1, t5);             
   TESTDSPINST_RD_RS_RT_DSPC("addu.qb $t6, $t7, $t3", 0x07654cb8, 0x734680bc,
                             t6, t7, t3);             
   TESTDSPINST_RD_RS_RT_DSPC("addu.qb $t5, $t3, $t2", 0xf973437b, 0x80000000,
                             t5, t3, t2);             
   TESTDSPINST_RD_RS_RT_DSPC("addu.qb $t2, $t4, $t8", 0x00010001, 0xfa3259ff,
                             t2, t4, t8);             
   TESTDSPINST_RD_RS_RT_DSPC("addu.qb $t0, $t8, $t0", 0x7fff7322, 0x77ff7fff,
                             t0, t8, t0);             
   TESTDSPINST_RD_RS_RT_DSPC("addu.qb $t4, $t6, $t1", 0x0034c420, 0x00000555,
                             t4, t6, t1);             
   TESTDSPINST_RD_RS_RT_DSPC("addu.qb $t2, $t3, $t4", 0x00000004, 1073741824,
                             t2, t3, t4);             
   TESTDSPINST_RD_RS_RT_DSPC("addu.qb $t4, $t1, $t5", 0x80002435, 0x80003421,
                             t4, t1, t5);             
   TESTDSPINST_RD_RS_RT_DSPC("addu.qb $t6, $t7, $t3", 0x76548000, 0x73468000,
                             t6, t7, t3);             
   TESTDSPINST_RD_RS_RT_DSPC("addu.qb $t5, $t3, $t2", 0x80000000, 0x80620020,
                             t5, t3, t2);             
   TESTDSPINST_RD_RS_RT_DSPC("addu.qb $t2, $t4, $t8", 0x00010001, 0xffffffff,
                             t2, t4, t8);             
   TESTDSPINST_RD_RS_RT_DSPC("addu.qb $t0, $t8, $t0", 0x7fff7fff, 0x7fff7fff,
                             t0, t8, t0);             
   TESTDSPINST_RD_RS_RT_DSPC("addu.qb $t4, $t6, $t1", 0x0000c420, 0x00000555,
                             t4, t6, t1);             
   TESTDSPINST_RD_RS_RT_DSPC("addu.qb $t0, $t1, $t2", 0x000a2300, 0x83bc1900,
                             t0, t1, t2);             
   TESTDSPINST_RD_RS_RT_DSPC("addu.qb $t2, $t3, $t4", 0x80000000, 0x80000000,
                             t2, t3, t4);             
   TESTDSPINST_RD_RS_RT_DSPC("addu.qb $t4, $t1, $t5", 0xaaaaaaaa, 0x55555555,
                             t4, t1, t5);             
   TESTDSPINST_RD_RS_RT_DSPC("addu.qb $t6, $t7, $t3", 0x00000018, 0xffff2435,
                             t6, t7, t3);             
   TESTDSPINST_RD_RS_RT_DSPC("addu.qb $t5, $t3, $t2", 0xbabababa, 0xabababab,
                             t5, t3, t2);             
   TESTDSPINST_RD_RS_RT_DSPC("addu.qb $t2, $t4, $t8", 0xf0f0f0f0, 0xfc79b4d2,
                             t2, t4, t8);             
   TESTDSPINST_RD_RS_RT_DSPC("addu.qb $t0, $t8, $t0", 0xfbde3976, 0x00000000,
                             t0, t8, t0);             
   TESTDSPINST_RD_RS_RT_DSPC("addu.qb $t4, $t6, $t1", 0x23534870, 0x00354565,
                             t4, t6, t1);             
   TESTDSPINST_RD_RS_RT_DSPC("addu.qb $t0, $t1, $t2", 0x980b7cde, 0x00086755,
                             t0, t1, t2);             
   TESTDSPINST_RD_RS_RT_DSPC("addu.qb $t2, $t3, $t4", 0x00000018, 0x8f8f8f8f,
                             t2, t3, t4);             
   TESTDSPINST_RD_RS_RT_DSPC("addu.qb $t4, $t1, $t5", 0x92784656, 0xeeeeeeee,
                             t4, t1, t5);             
   TESTDSPINST_RD_RS_RT_DSPC("addu.qb $t6, $t7, $t3", 0xcacacaca, 0x1bdbdbdb,
                             t6, t7, t3);             
   TESTDSPINST_RD_RS_RT_DSPC("addu.qb $t5, $t3, $t2", 0xbacabaca, 0xdecadeca,
                             t5, t3, t2);             
   TESTDSPINST_RD_RS_RT_DSPC("addu.qb $t2, $t4, $t8", 0x12fadeb4, 0x93474bde,
                             t2, t4, t8);             
   TESTDSPINST_RD_RS_RT_DSPC("addu.qb $t0, $t8, $t0", 0x7c000790, 0xfc0007ff,
                             t0, t8, t0);             
   TESTDSPINST_RD_RS_RT_DSPC("addu.qb $t4, $t6, $t1", 0xffffffff, 0xffffffff,
                             t4, t6, t1);             
   TESTDSPINST_RD_RS_RT_DSPC("addu.qb $t0, $t1, $t2", 0xf2f4df1f, 0xcb4ab48f,
                             t0, t1, t2);             
   TESTDSPINST_RD_RS_RT_DSPC("addu.qb $t2, $t3, $t4", 0x435f909a, 0xaf8f7e18,
                             t2, t3, t4);             
   TESTDSPINST_RD_RS_RT_DSPC("addu.qb $t4, $t1, $t5", 0x2106ba5f, 0x87df4510,
                             t4, t1, t5);             
   TESTDSPINST_RD_RS_RT_DSPC("addu.qb $t6, $t7, $t3", 0x246a6376, 0xabf4e8e1,
                             t6, t7, t3);             
   TESTDSPINST_RD_RS_RT_DSPC("addu.qb $t5, $t3, $t2", 0x1046a1a3, 0xf4c0eeac,
                             t5, t3, t2);             
   TESTDSPINST_RD_RS_RT_DSPC("addu.qb $t2, $t4, $t8", 0x638ca515, 0x006a54f2,
                             t2, t4, t8);             
   TESTDSPINST_RD_RS_RT_DSPC("addu.qb $t0, $t8, $t0", 0xf63e7a9d, 0x79f74493,
                             t0, t8, t0);             
   TESTDSPINST_RD_RS_RT_DSPC("addu.qb $t4, $t6, $t1", 0xbd6845cd, 0x9c09e313,
                             t4, t6, t1);

   printf("-------- ADDU_S.QB --------\n");
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.qb $t0, $t1, $t2", 0x00000000, 0x00000000,
                             t0, t1, t2);               
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.qb $t2, $t3, $t4", 0x00045fb2, 0x00000286,
                             t2, t3, t4);               
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.qb $t4, $t1, $t5", 0x00002435, 0xffff3421,
                             t4, t1, t5);               
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.qb $t6, $t7, $t3", 0x07654cb8, 0x734680bc,
                             t6, t7, t3);               
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.qb $t5, $t3, $t2", 0xf973437b, 0x80000000,
                             t5, t3, t2);               
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.qb $t2, $t4, $t8", 0x00010001, 0xfa3259ff,
                             t2, t4, t8);               
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.qb $t0, $t8, $t0", 0x7fff7322, 0x77ff7fff,
                             t0, t8, t0);               
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.qb $t4, $t6, $t1", 0x0034c420, 0x00000555,
                             t4, t6, t1);               
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.qb $t2, $t3, $t4", 0x00000004, 1073741824,
                             t2, t3, t4);               
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.qb $t4, $t1, $t5", 0x80002435, 0x80003421,
                             t4, t1, t5);               
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.qb $t6, $t7, $t3", 0x76548000, 0x73468000,
                             t6, t7, t3);               
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.qb $t5, $t3, $t2", 0x80000000, 0x80620020,
                             t5, t3, t2);               
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.qb $t2, $t4, $t8", 0x00010001, 0xffffffff,
                             t2, t4, t8);               
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.qb $t0, $t8, $t0", 0x7fff7fff, 0x7fff7fff,
                             t0, t8, t0);               
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.qb $t4, $t6, $t1", 0x0000c420, 0x00000555,
                             t4, t6, t1);               
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.qb $t0, $t1, $t2", 0x000a2300, 0x83bc1900,
                             t0, t1, t2);               
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.qb $t2, $t3, $t4", 0x80000000, 0x80000000,
                             t2, t3, t4);               
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.qb $t4, $t1, $t5", 0xaaaaaaaa, 0x55555555,
                             t4, t1, t5);               
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.qb $t6, $t7, $t3", 0x00000018, 0xffff2435,
                             t6, t7, t3);               
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.qb $t5, $t3, $t2", 0xbabababa, 0xabababab,
                             t5, t3, t2);               
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.qb $t2, $t4, $t8", 0xf0f0f0f0, 0xfc79b4d2,
                             t2, t4, t8);               
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.qb $t0, $t8, $t0", 0xfbde3976, 0x00000000,
                             t0, t8, t0);               
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.qb $t4, $t6, $t1", 0x23534870, 0x00354565,
                             t4, t6, t1);               
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.qb $t0, $t1, $t2", 0x980b7cde, 0x00086755,
                             t0, t1, t2);               
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.qb $t2, $t3, $t4", 0x00000018, 0x8f8f8f8f,
                             t2, t3, t4);               
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.qb $t4, $t1, $t5", 0x92784656, 0xeeeeeeee,
                             t4, t1, t5);               
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.qb $t6, $t7, $t3", 0xcacacaca, 0x1bdbdbdb,
                             t6, t7, t3);               
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.qb $t5, $t3, $t2", 0xbacabaca, 0xdecadeca,
                             t5, t3, t2);               
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.qb $t2, $t4, $t8", 0x12fadeb4, 0x93474bde,
                             t2, t4, t8);               
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.qb $t0, $t8, $t0", 0x7c000790, 0xfc0007ff,
                             t0, t8, t0);               
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.qb $t4, $t6, $t1", 0xffffffff, 0xffffffff,
                             t4, t6, t1);               
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.qb $t0, $t1, $t2", 0xf2f4df1f, 0xcb4ab48f,
                             t0, t1, t2);               
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.qb $t2, $t3, $t4", 0x435f909a, 0xaf8f7e18,
                             t2, t3, t4);               
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.qb $t4, $t1, $t5", 0x2106ba5f, 0x87df4510,
                             t4, t1, t5);               
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.qb $t6, $t7, $t3", 0x246a6376, 0xabf4e8e1,
                             t6, t7, t3);               
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.qb $t5, $t3, $t2", 0x1046a1a3, 0xf4c0eeac,
                             t5, t3, t2);               
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.qb $t2, $t4, $t8", 0x638ca515, 0x006a54f2,
                             t2, t4, t8);               
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.qb $t0, $t8, $t0", 0xf63e7a9d, 0x79f74493,
                             t0, t8, t0);               
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.qb $t4, $t6, $t1", 0xbd6845cd, 0x9c09e313,
                             t4, t6, t1);

   printf("-------- ADDWC --------\n");
   TESTDSPINST_RD_RS_RT_DSPC("addwc $t0, $t1, $t2", 0x00000000, 0x00000000,
                             t0, t1, t2);           
   TESTDSPINST_RD_RS_RT_DSPC("addwc $t2, $t3, $t4", 0x00045fb2, 0x00000286,
                             t2, t3, t4);           
   TESTDSPINST_RD_RS_RT_DSPC("addwc $t4, $t1, $t5", 0x00002435, 0xffff3421,
                             t4, t1, t5);           
   TESTDSPINST_RD_RS_RT_DSPC("addwc $t6, $t7, $t3", 0x07654cb8, 0x734680bc,
                             t6, t7, t3);           
   TESTDSPINST_RD_RS_RT_DSPC("addwc $t5, $t3, $t2", 0xf973437b, 0x80000000,
                             t5, t3, t2);           
   TESTDSPINST_RD_RS_RT_DSPC("addwc $t2, $t4, $t8", 0x00010001, 0xfa3259ff,
                             t2, t4, t8);           
   TESTDSPINST_RD_RS_RT_DSPC("addwc $t0, $t8, $t0", 0x7fff7322, 0x77ff7fff,
                             t0, t8, t0);           
   TESTDSPINST_RD_RS_RT_DSPC("addwc $t4, $t6, $t1", 0x0034c420, 0x00000555,
                             t4, t6, t1);           
   TESTDSPINST_RD_RS_RT_DSPC("addwc $t2, $t3, $t4", 0x00000004, 1073741824,
                             t2, t3, t4);           
   TESTDSPINST_RD_RS_RT_DSPC("addwc $t4, $t1, $t5", 0x80002435, 0x80003421,
                             t4, t1, t5);           
   TESTDSPINST_RD_RS_RT_DSPC("addwc $t6, $t7, $t3", 0x76548000, 0x73468000,
                             t6, t7, t3);           
   TESTDSPINST_RD_RS_RT_DSPC("addwc $t5, $t3, $t2", 0x80000000, 0x80620020,
                             t5, t3, t2);           
   TESTDSPINST_RD_RS_RT_DSPC("addwc $t2, $t4, $t8", 0x00010001, 0xffffffff,
                             t2, t4, t8);           
   TESTDSPINST_RD_RS_RT_DSPC("addwc $t0, $t8, $t0", 0x7fff7fff, 0x7fff7fff,
                             t0, t8, t0);           
   TESTDSPINST_RD_RS_RT_DSPC("addwc $t4, $t6, $t1", 0x0000c420, 0x00000555,
                             t4, t6, t1);           
   TESTDSPINST_RD_RS_RT_DSPC("addwc $t0, $t1, $t2", 0x000a2300, 0x83bc1900,
                             t0, t1, t2);           
   TESTDSPINST_RD_RS_RT_DSPC("addwc $t2, $t3, $t4", 0x80000000, 0x80000000,
                             t2, t3, t4);           
   TESTDSPINST_RD_RS_RT_DSPC("addwc $t4, $t1, $t5", 0xaaaaaaaa, 0x55555555,
                             t4, t1, t5);           
   TESTDSPINST_RD_RS_RT_DSPC("addwc $t6, $t7, $t3", 0x00000018, 0xffff2435,
                             t6, t7, t3);           
   TESTDSPINST_RD_RS_RT_DSPC("addwc $t5, $t3, $t2", 0xbabababa, 0xabababab,
                             t5, t3, t2);           
   TESTDSPINST_RD_RS_RT_DSPC("addwc $t2, $t4, $t8", 0xf0f0f0f0, 0xfc79b4d2,
                             t2, t4, t8);           
   TESTDSPINST_RD_RS_RT_DSPC("addwc $t0, $t8, $t0", 0xfbde3976, 0x00000000,
                             t0, t8, t0);           
   TESTDSPINST_RD_RS_RT_DSPC("addwc $t4, $t6, $t1", 0x23534870, 0x00354565,
                             t4, t6, t1);           
   TESTDSPINST_RD_RS_RT_DSPC("addwc $t0, $t1, $t2", 0x980b7cde, 0x00086755,
                             t0, t1, t2);           
   TESTDSPINST_RD_RS_RT_DSPC("addwc $t2, $t3, $t4", 0x00000018, 0x8f8f8f8f,
                             t2, t3, t4);           
   TESTDSPINST_RD_RS_RT_DSPC("addwc $t4, $t1, $t5", 0x92784656, 0xeeeeeeee,
                             t4, t1, t5);           
   TESTDSPINST_RD_RS_RT_DSPC("addwc $t6, $t7, $t3", 0xcacacaca, 0x1bdbdbdb,
                             t6, t7, t3);           
   TESTDSPINST_RD_RS_RT_DSPC("addwc $t5, $t3, $t2", 0xbacabaca, 0xdecadeca,
                             t5, t3, t2);           
   TESTDSPINST_RD_RS_RT_DSPC("addwc $t2, $t4, $t8", 0x12fadeb4, 0x93474bde,
                             t2, t4, t8);           
   TESTDSPINST_RD_RS_RT_DSPC("addwc $t0, $t8, $t0", 0x7c000790, 0xfc0007ff,
                             t0, t8, t0);           
   TESTDSPINST_RD_RS_RT_DSPC("addwc $t4, $t6, $t1", 0xffffffff, 0xffffffff,
                             t4, t6, t1);           
   TESTDSPINST_RD_RS_RT_DSPC("addwc $t0, $t1, $t2", 0xf2f4df1f, 0xcb4ab48f,
                             t0, t1, t2);           
   TESTDSPINST_RD_RS_RT_DSPC("addwc $t2, $t3, $t4", 0x435f909a, 0xaf8f7e18,
                             t2, t3, t4);           
   TESTDSPINST_RD_RS_RT_DSPC("addwc $t4, $t1, $t5", 0x2106ba5f, 0x87df4510,
                             t4, t1, t5);           
   TESTDSPINST_RD_RS_RT_DSPC("addwc $t6, $t7, $t3", 0x246a6376, 0xabf4e8e1,
                             t6, t7, t3);           
   TESTDSPINST_RD_RS_RT_DSPC("addwc $t5, $t3, $t2", 0x1046a1a3, 0xf4c0eeac,
                             t5, t3, t2);           
   TESTDSPINST_RD_RS_RT_DSPC("addwc $t2, $t4, $t8", 0x638ca515, 0x006a54f2,
                             t2, t4, t8);           
   TESTDSPINST_RD_RS_RT_DSPC("addwc $t0, $t8, $t0", 0xf63e7a9d, 0x79f74493,
                             t0, t8, t0);           
   TESTDSPINST_RD_RS_RT_DSPC("addwc $t4, $t6, $t1", 0xbd6845cd, 0x9c09e313,
                             t4, t6, t1);

   printf("-------- BITREV --------\n");
   TESTDSPINST_RD_RT_NODSPC("bitrev $t0, $t1", 0x09ba4800, t0, t1);
   TESTDSPINST_RD_RT_NODSPC("bitrev $t2, $t3", 0x80003286, t2, t3);
   TESTDSPINST_RD_RT_NODSPC("bitrev $t4, $t1", 0xfabc2435, t4, t1);
   TESTDSPINST_RD_RT_NODSPC("bitrev $t6, $t7", 0x73468000, t6, t7);
   TESTDSPINST_RD_RT_NODSPC("bitrev $t5, $t3", 0x803c6900, t5, t3);
   TESTDSPINST_RD_RT_NODSPC("bitrev $t2, $t4", 0xffad492f, t2, t4);
   TESTDSPINST_RD_RT_NODSPC("bitrev $t0, $t8", 0xfff45fff, t0, t8);
   TESTDSPINST_RD_RT_NODSPC("bitrev $t2, $t4", 0x00000555, t2, t4);
   TESTDSPINST_RD_RT_NODSPC("bitrev $t0, $t1", 0x0098f308, t0, t1);
   TESTDSPINST_RD_RT_NODSPC("bitrev $t2, $t3", 0x80000000, t2, t3);
   TESTDSPINST_RD_RT_NODSPC("bitrev $t4, $t1", 0x55555555, t4, t1);
   TESTDSPINST_RD_RT_NODSPC("bitrev $t6, $t7", 0xffff2435, t6, t7);
   TESTDSPINST_RD_RT_NODSPC("bitrev $t5, $t3", 0xabababab, t5, t3);
   TESTDSPINST_RD_RT_NODSPC("bitrev $t2, $t4", 0xfc79b4d2, t2, t4);
   TESTDSPINST_RD_RT_NODSPC("bitrev $t0, $t8", 0x00000000, t0, t8);
   TESTDSPINST_RD_RT_NODSPC("bitrev $t2, $t4", 0x00354565, t2, t4);
   TESTDSPINST_RD_RT_NODSPC("bitrev $t0, $t1", 0x00086755, t0, t1);
   TESTDSPINST_RD_RT_NODSPC("bitrev $t2, $t3", 0x8f8f8f8f, t2, t3);
   TESTDSPINST_RD_RT_NODSPC("bitrev $t4, $t1", 0xeeeeeeee, t4, t1);
   TESTDSPINST_RD_RT_NODSPC("bitrev $t6, $t7", 0x1bdbdbdb, t6, t7);
   TESTDSPINST_RD_RT_NODSPC("bitrev $t5, $t3", 0xdecadeca, t5, t3);
   TESTDSPINST_RD_RT_NODSPC("bitrev $t2, $t4", 0x93474bde, t2, t4);
   TESTDSPINST_RD_RT_NODSPC("bitrev $t0, $t8", 0xfc0007ff, t0, t8);
   TESTDSPINST_RD_RT_NODSPC("bitrev $t2, $t4", 0xffffffff, t2, t4);
   TESTDSPINST_RD_RT_NODSPC("bitrev $t0, $t1", 0xcb4ab48f, t0, t1);
   TESTDSPINST_RD_RT_NODSPC("bitrev $t2, $t3", 0xaf8f7e18, t2, t3);
   TESTDSPINST_RD_RT_NODSPC("bitrev $t4, $t1", 0x87df4510, t4, t1);
   TESTDSPINST_RD_RT_NODSPC("bitrev $t6, $t7", 0xabf4e8e1, t6, t7);
   TESTDSPINST_RD_RT_NODSPC("bitrev $t5, $t3", 0xf4c0eeac, t5, t3);
   TESTDSPINST_RD_RT_NODSPC("bitrev $t2, $t4", 0x006a54f2, t2, t4);
   TESTDSPINST_RD_RT_NODSPC("bitrev $t0, $t8", 0x79f74493, t0, t8);
   TESTDSPINST_RD_RT_NODSPC("bitrev $t2, $t4", 0x9c09e313, t2, t4);

   printf("-------- BPOSGE32 --------\n");
   TESTDSPINST_BPOSGE32("bposge32", 0, 0, v0, t1);
   TESTDSPINST_BPOSGE32("bposge32", 1, 1, v1, t0);
   TESTDSPINST_BPOSGE32("bposge32", 2, 32, a0, t1);
   TESTDSPINST_BPOSGE32("bposge32", 3, 17, a1, t2);
   TESTDSPINST_BPOSGE32("bposge32", 4, 8, a2, t0);
   TESTDSPINST_BPOSGE32("bposge32", 5, 60, a3, t0);
   TESTDSPINST_BPOSGE32("bposge32", 6, 0x5, t0, t1);
   TESTDSPINST_BPOSGE32("bposge32", 7, -3, t1, t2);
   TESTDSPINST_BPOSGE32("bposge32", 8, 125, t2, t3);
   TESTDSPINST_BPOSGE32("bposge32", 9, 7, t3, t4);
   TESTDSPINST_BPOSGE32("bposge32", 10, 42, t4, t5);
   TESTDSPINST_BPOSGE32("bposge32", 11, 53, t5, t6);
   TESTDSPINST_BPOSGE32("bposge32", 12, 99, t6, t7);
   TESTDSPINST_BPOSGE32("bposge32", 13, 12, s0, t1);
   TESTDSPINST_BPOSGE32("bposge32", 14, 4, v0, t9);
   TESTDSPINST_BPOSGE32("bposge32", 15, 6, t9, t8);

   printf("-------- CMP.EQ.PH --------\n");
   TESTDSPINST_RS_RT_DSPC("cmp.eq.ph $t0, $t1", 0x00000000, 0x0fffffff, t0, t1);
   TESTDSPINST_RS_RT_DSPC("cmp.eq.ph $t2, $t3", 0x00045fb2, 0x00000286, t2, t3);
   TESTDSPINST_RS_RT_DSPC("cmp.eq.ph $t4, $t1", 0xfabc2435, 0xfabc3421, t4, t1);
   TESTDSPINST_RS_RT_DSPC("cmp.eq.ph $t6, $t7", 0x07654cb8, 0x73464cb8, t6, t7);
   TESTDSPINST_RS_RT_DSPC("cmp.eq.ph $t5, $t3", 0xf973437b, 0x80000000, t5, t3);
   TESTDSPINST_RS_RT_DSPC("cmp.eq.ph $t2, $t4", 0x00010001, 0xffffffff, t2, t4);
   TESTDSPINST_RS_RT_DSPC("cmp.eq.ph $t0, $t8", 0x7fff7fff, 0x7fff7fff, t0, t8);
   TESTDSPINST_RS_RT_DSPC("cmp.eq.ph $t4, $t6", 0x0000c420, 0x00000555, t4, t6);
   TESTDSPINST_RS_RT_DSPC("cmp.eq.ph $t0, $t1", 0x00000000, 0x00000000, t0, t1);
   TESTDSPINST_RS_RT_DSPC("cmp.eq.ph $t2, $t3", 0x80000000, 0x80000000, t2, t3);
   TESTDSPINST_RS_RT_DSPC("cmp.eq.ph $t4, $t1", 0xaaaaaaaa, 0x55555555, t4, t1);
   TESTDSPINST_RS_RT_DSPC("cmp.eq.ph $t6, $t7", 0x00000018, 0xffff2435, t6, t7);
   TESTDSPINST_RS_RT_DSPC("cmp.eq.ph $t5, $t3", 0xbabababa, 0xabababab, t5, t3);
   TESTDSPINST_RS_RT_DSPC("cmp.eq.ph $t2, $t4", 0xf0f0f0f0, 0xfc79b4d2, t2, t4);
   TESTDSPINST_RS_RT_DSPC("cmp.eq.ph $t0, $t8", 0xfbde3976, 0x00000000, t0, t8);
   TESTDSPINST_RS_RT_DSPC("cmp.eq.ph $t4, $t6", 0x23534870, 0x00354565, t4, t6);
   TESTDSPINST_RS_RT_DSPC("cmp.eq.ph $t0, $t1", 0x980b7cde, 0x00086755, t0, t1);
   TESTDSPINST_RS_RT_DSPC("cmp.eq.ph $t2, $t3", 0x00000018, 0x8f8f8f8f, t2, t3);
   TESTDSPINST_RS_RT_DSPC("cmp.eq.ph $t4, $t1", 0x92784656, 0xeeeeeeee, t4, t1);
   TESTDSPINST_RS_RT_DSPC("cmp.eq.ph $t6, $t7", 0xcacacaca, 0x1bdbdbdb, t6, t7);
   TESTDSPINST_RS_RT_DSPC("cmp.eq.ph $t5, $t3", 0xbacabaca, 0xdecadeca, t5, t3);
   TESTDSPINST_RS_RT_DSPC("cmp.eq.ph $t2, $t4", 0x12fadeb4, 0x93474bde, t2, t4);
   TESTDSPINST_RS_RT_DSPC("cmp.eq.ph $t0, $t8", 0x7c000790, 0xfc0007ff, t0, t8);
   TESTDSPINST_RS_RT_DSPC("cmp.eq.ph $t4, $t6", 0xffffffff, 0xffffffff, t4, t6);
   TESTDSPINST_RS_RT_DSPC("cmp.eq.ph $t0, $t1", 0xf2f4df1f, 0xcb4ab48f, t0, t1);
   TESTDSPINST_RS_RT_DSPC("cmp.eq.ph $t2, $t3", 0x435f909a, 0xaf8f7e18, t2, t3);
   TESTDSPINST_RS_RT_DSPC("cmp.eq.ph $t4, $t1", 0x2106ba5f, 0x87df4510, t4, t1);
   TESTDSPINST_RS_RT_DSPC("cmp.eq.ph $t6, $t7", 0x246a6376, 0xabf4e8e1, t6, t7);
   TESTDSPINST_RS_RT_DSPC("cmp.eq.ph $t5, $t3", 0x1046a1a3, 0xf4c0eeac, t5, t3);
   TESTDSPINST_RS_RT_DSPC("cmp.eq.ph $t2, $t4", 0x638ca515, 0x006a54f2, t2, t4);
   TESTDSPINST_RS_RT_DSPC("cmp.eq.ph $t0, $t8", 0xf63e7a9d, 0x79f74493, t0, t8);
   TESTDSPINST_RS_RT_DSPC("cmp.eq.ph $t4, $t6", 0xbd6845cd, 0x9c09e313, t4, t6);

   printf("-------- CMP.LT.PH --------\n");
   TESTDSPINST_RS_RT_DSPC("cmp.lt.ph $t0, $t1", 0x00000000, 0x0fffffff, t0, t1);
   TESTDSPINST_RS_RT_DSPC("cmp.lt.ph $t2, $t3", 0x00045fb2, 0x00000286, t2, t3);
   TESTDSPINST_RS_RT_DSPC("cmp.lt.ph $t4, $t1", 0xfabc2435, 0xfabc3421, t4, t1);
   TESTDSPINST_RS_RT_DSPC("cmp.lt.ph $t6, $t7", 0x07654cb8, 0x73464cb8, t6, t7);
   TESTDSPINST_RS_RT_DSPC("cmp.lt.ph $t5, $t3", 0xf973437b, 0x80000000, t5, t3);
   TESTDSPINST_RS_RT_DSPC("cmp.lt.ph $t2, $t4", 0x00010001, 0xffffffff, t2, t4);
   TESTDSPINST_RS_RT_DSPC("cmp.lt.ph $t0, $t8", 0x7fff7fff, 0x7fff7fff, t0, t8);
   TESTDSPINST_RS_RT_DSPC("cmp.lt.ph $t4, $t6", 0x0000c420, 0x00000555, t4, t6);
   TESTDSPINST_RS_RT_DSPC("cmp.lt.ph $t0, $t1", 0x00000000, 0x00000000, t0, t1);
   TESTDSPINST_RS_RT_DSPC("cmp.lt.ph $t2, $t3", 0x80000000, 0x80000000, t2, t3);
   TESTDSPINST_RS_RT_DSPC("cmp.lt.ph $t4, $t1", 0xaaaaaaaa, 0x55555555, t4, t1);
   TESTDSPINST_RS_RT_DSPC("cmp.lt.ph $t6, $t7", 0x00000018, 0xffff2435, t6, t7);
   TESTDSPINST_RS_RT_DSPC("cmp.lt.ph $t5, $t3", 0xbabababa, 0xabababab, t5, t3);
   TESTDSPINST_RS_RT_DSPC("cmp.lt.ph $t2, $t4", 0xf0f0f0f0, 0xfc79b4d2, t2, t4);
   TESTDSPINST_RS_RT_DSPC("cmp.lt.ph $t0, $t8", 0xfbde3976, 0x00000000, t0, t8);
   TESTDSPINST_RS_RT_DSPC("cmp.lt.ph $t4, $t6", 0x23534870, 0x00354565, t4, t6);
   TESTDSPINST_RS_RT_DSPC("cmp.lt.ph $t0, $t1", 0x980b7cde, 0x00086755, t0, t1);
   TESTDSPINST_RS_RT_DSPC("cmp.lt.ph $t2, $t3", 0x00000018, 0x8f8f8f8f, t2, t3);
   TESTDSPINST_RS_RT_DSPC("cmp.lt.ph $t4, $t1", 0x92784656, 0xeeeeeeee, t4, t1);
   TESTDSPINST_RS_RT_DSPC("cmp.lt.ph $t6, $t7", 0xcacacaca, 0x1bdbdbdb, t6, t7);
   TESTDSPINST_RS_RT_DSPC("cmp.lt.ph $t5, $t3", 0xbacabaca, 0xdecadeca, t5, t3);
   TESTDSPINST_RS_RT_DSPC("cmp.lt.ph $t2, $t4", 0x12fadeb4, 0x93474bde, t2, t4);
   TESTDSPINST_RS_RT_DSPC("cmp.lt.ph $t0, $t8", 0x7c000790, 0xfc0007ff, t0, t8);
   TESTDSPINST_RS_RT_DSPC("cmp.lt.ph $t4, $t6", 0xffffffff, 0xffffffff, t4, t6);
   TESTDSPINST_RS_RT_DSPC("cmp.lt.ph $t0, $t1", 0xf2f4df1f, 0xcb4ab48f, t0, t1);
   TESTDSPINST_RS_RT_DSPC("cmp.lt.ph $t2, $t3", 0x435f909a, 0xaf8f7e18, t2, t3);
   TESTDSPINST_RS_RT_DSPC("cmp.lt.ph $t4, $t1", 0x2106ba5f, 0x87df4510, t4, t1);
   TESTDSPINST_RS_RT_DSPC("cmp.lt.ph $t6, $t7", 0x246a6376, 0xabf4e8e1, t6, t7);
   TESTDSPINST_RS_RT_DSPC("cmp.lt.ph $t5, $t3", 0x1046a1a3, 0xf4c0eeac, t5, t3);
   TESTDSPINST_RS_RT_DSPC("cmp.lt.ph $t2, $t4", 0x638ca515, 0x006a54f2, t2, t4);
   TESTDSPINST_RS_RT_DSPC("cmp.lt.ph $t0, $t8", 0xf63e7a9d, 0x79f74493, t0, t8);
   TESTDSPINST_RS_RT_DSPC("cmp.lt.ph $t4, $t6", 0xbd6845cd, 0x9c09e313, t4, t6);

   printf("-------- CMP.LE.PH --------\n");
   TESTDSPINST_RS_RT_DSPC("cmp.le.ph $t0, $t1", 0x00000000, 0x0fffffff, t0, t1);
   TESTDSPINST_RS_RT_DSPC("cmp.le.ph $t2, $t3", 0x00045fb2, 0x00000286, t2, t3);
   TESTDSPINST_RS_RT_DSPC("cmp.le.ph $t4, $t1", 0xfabc2435, 0xfabc3421, t4, t1);
   TESTDSPINST_RS_RT_DSPC("cmp.le.ph $t6, $t7", 0x07654cb8, 0x73464cb8, t6, t7);
   TESTDSPINST_RS_RT_DSPC("cmp.le.ph $t5, $t3", 0xf973437b, 0x80000000, t5, t3);
   TESTDSPINST_RS_RT_DSPC("cmp.le.ph $t2, $t4", 0x00010001, 0xffffffff, t2, t4);
   TESTDSPINST_RS_RT_DSPC("cmp.le.ph $t0, $t8", 0x7fff7fff, 0x7fff7fff, t0, t8);
   TESTDSPINST_RS_RT_DSPC("cmp.le.ph $t4, $t6", 0x0000c420, 0x00000555, t4, t6);
   TESTDSPINST_RS_RT_DSPC("cmp.le.ph $t0, $t1", 0x00000000, 0x00000000, t0, t1);
   TESTDSPINST_RS_RT_DSPC("cmp.le.ph $t2, $t3", 0x80000000, 0x80000000, t2, t3);
   TESTDSPINST_RS_RT_DSPC("cmp.le.ph $t4, $t1", 0xaaaaaaaa, 0x55555555, t4, t1);
   TESTDSPINST_RS_RT_DSPC("cmp.le.ph $t6, $t7", 0x00000018, 0xffff2435, t6, t7);
   TESTDSPINST_RS_RT_DSPC("cmp.le.ph $t5, $t3", 0xbabababa, 0xabababab, t5, t3);
   TESTDSPINST_RS_RT_DSPC("cmp.le.ph $t2, $t4", 0xf0f0f0f0, 0xfc79b4d2, t2, t4);
   TESTDSPINST_RS_RT_DSPC("cmp.le.ph $t0, $t8", 0xfbde3976, 0x00000000, t0, t8);
   TESTDSPINST_RS_RT_DSPC("cmp.le.ph $t4, $t6", 0x23534870, 0x00354565, t4, t6);
   TESTDSPINST_RS_RT_DSPC("cmp.le.ph $t0, $t1", 0x980b7cde, 0x00086755, t0, t1);
   TESTDSPINST_RS_RT_DSPC("cmp.le.ph $t2, $t3", 0x00000018, 0x8f8f8f8f, t2, t3);
   TESTDSPINST_RS_RT_DSPC("cmp.le.ph $t4, $t1", 0x92784656, 0xeeeeeeee, t4, t1);
   TESTDSPINST_RS_RT_DSPC("cmp.le.ph $t6, $t7", 0xcacacaca, 0x1bdbdbdb, t6, t7);
   TESTDSPINST_RS_RT_DSPC("cmp.le.ph $t5, $t3", 0xbacabaca, 0xdecadeca, t5, t3);
   TESTDSPINST_RS_RT_DSPC("cmp.le.ph $t2, $t4", 0x12fadeb4, 0x93474bde, t2, t4);
   TESTDSPINST_RS_RT_DSPC("cmp.le.ph $t0, $t8", 0x7c000790, 0xfc0007ff, t0, t8);
   TESTDSPINST_RS_RT_DSPC("cmp.le.ph $t4, $t6", 0xffffffff, 0xffffffff, t4, t6);
   TESTDSPINST_RS_RT_DSPC("cmp.le.ph $t0, $t1", 0xf2f4df1f, 0xcb4ab48f, t0, t1);
   TESTDSPINST_RS_RT_DSPC("cmp.le.ph $t2, $t3", 0x435f909a, 0xaf8f7e18, t2, t3);
   TESTDSPINST_RS_RT_DSPC("cmp.le.ph $t4, $t1", 0x2106ba5f, 0x87df4510, t4, t1);
   TESTDSPINST_RS_RT_DSPC("cmp.le.ph $t6, $t7", 0x246a6376, 0xabf4e8e1, t6, t7);
   TESTDSPINST_RS_RT_DSPC("cmp.le.ph $t5, $t3", 0x1046a1a3, 0xf4c0eeac, t5, t3);
   TESTDSPINST_RS_RT_DSPC("cmp.le.ph $t2, $t4", 0x638ca515, 0x006a54f2, t2, t4);
   TESTDSPINST_RS_RT_DSPC("cmp.le.ph $t0, $t8", 0xf63e7a9d, 0x79f74493, t0, t8);
   TESTDSPINST_RS_RT_DSPC("cmp.le.ph $t4, $t6", 0xbd6845cd, 0x9c09e313, t4, t6);

   printf("-------- CMPGU.EQ.QB --------\n");
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.eq.qb $t0, $t1, $t2", 0x00672300,
                               0x00000000, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.eq.qb $t2, $t3, $t4", 0x00045fb2,
                               0x00000286, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.eq.qb $t4, $t1, $t5", 0x00002435,
                               0xffff3421, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.eq.qb $t6, $t7, $t3", 0x07654cb8,
                               0x734680bc, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.eq.qb $t5, $t3, $t2", 0xf973437b,
                               0x80000000, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.eq.qb $t2, $t4, $t8", 0x00010001,
                               0xf08b4631, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.eq.qb $t0, $t8, $t0", 0x5cbd891a,
                               0x7fff7fff, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.eq.qb $t4, $t6, $t1", 0x0000c420,
                               0x0ab64555, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.eq.qb $t2, $t3, $t4", 0x00000004,
                               1073741824, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.eq.qb $t4, $t1, $t5", 0x80002435,
                               0x80003421, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.eq.qb $t6, $t7, $t3", 0x76548000,
                               0x73468000, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.eq.qb $t5, $t3, $t2", 0x8007c560,
                               0x80000000, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.eq.qb $t2, $t4, $t8", 0x00010001,
                               0xffffffff, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.eq.qb $t0, $t8, $t0", 0x7fff7fff,
                               0x7fff7fff, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.eq.qb $t4, $t6, $t1", 0x0000c420,
                               0x00000555, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.eq.qb $t0, $t1, $t2", 0x00000000,
                               0x00000000, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.eq.qb $t2, $t3, $t4", 0x80000000,
                               0x80000000, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.eq.qb $t4, $t1, $t5", 0xaaaaaaaa,
                               0x55555555, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.eq.qb $t6, $t7, $t3", 0x00000018,
                               0xffff2435, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.eq.qb $t5, $t3, $t2", 0xbabababa,
                               0xabababab, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.eq.qb $t2, $t4, $t8", 0xf0f0f0f0,
                               0xfc79b4d2, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.eq.qb $t0, $t8, $t0", 0xfbde3976,
                               0x00000000, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.eq.qb $t4, $t6, $t1", 0x23534870,
                               0x00354565, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.eq.qb $t0, $t1, $t2", 0x980b7cde,
                               0x00086755, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.eq.qb $t2, $t3, $t4", 0x00000018,
                               0x8f8f8f8f, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.eq.qb $t4, $t1, $t5", 0x92784656,
                               0xeeeeeeee, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.eq.qb $t6, $t7, $t3", 0xcacacaca,
                               0x1bdbdbdb, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.eq.qb $t5, $t3, $t2", 0xbacabaca,
                               0xdecadeca, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.eq.qb $t2, $t4, $t8", 0x12fadeb4,
                               0x93474bde, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.eq.qb $t0, $t8, $t0", 0x7c000790,
                               0xfc0007ff, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.eq.qb $t4, $t6, $t1", 0xffffffff,
                               0xffffffff, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.eq.qb $t0, $t1, $t2", 0xf2f4df1f,
                               0xcb4ab48f, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.eq.qb $t2, $t3, $t4", 0x435f909a,
                               0xaf8f7e18, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.eq.qb $t4, $t1, $t5", 0x2106ba5f,
                               0x87df4510, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.eq.qb $t6, $t7, $t3", 0x246a6376,
                               0xabf4e8e1, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.eq.qb $t5, $t3, $t2", 0x1046a1a3,
                               0xf4c0eeac, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.eq.qb $t2, $t4, $t8", 0x638ca515,
                               0x006a54f2, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.eq.qb $t0, $t8, $t0", 0xf63e7a9d,
                               0x79f74493, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.eq.qb $t4, $t6, $t1", 0xbd6845cd,
                               0x9c09e313, t4, t6, t1);

   printf("-------- CMPGU.LT.QB --------\n");
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.lt.qb $t0, $t1, $t2", 0x00672300,
                               0x00000000, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.lt.qb $t2, $t3, $t4", 0x00045fb2,
                               0x00000286, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.lt.qb $t4, $t1, $t5", 0x00002435,
                               0xffff3421, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.lt.qb $t6, $t7, $t3", 0x07654cb8,
                               0x734680bc, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.lt.qb $t5, $t3, $t2", 0xf973437b,
                               0x80000000, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.lt.qb $t2, $t4, $t8", 0x00010001,
                               0xf08b4631, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.lt.qb $t0, $t8, $t0", 0x5cbd891a,
                               0x7fff7fff, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.lt.qb $t4, $t6, $t1", 0x0000c420,
                               0x0ab64555, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.lt.qb $t2, $t3, $t4", 0x00000004,
                               1073741824, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.lt.qb $t4, $t1, $t5", 0x80002435,
                               0x80003421, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.lt.qb $t6, $t7, $t3", 0x76548000,
                               0x73468000, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.lt.qb $t5, $t3, $t2", 0x8007c560,
                               0x80000000, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.lt.qb $t2, $t4, $t8", 0x00010001,
                               0xffffffff, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.lt.qb $t0, $t8, $t0", 0x7fff7fff,
                               0x7fff7fff, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.lt.qb $t4, $t6, $t1", 0x0000c420,
                               0x00000555, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.lt.qb $t0, $t1, $t2", 0x00000000,
                               0x00000000, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.lt.qb $t2, $t3, $t4", 0x80000000,
                               0x80000000, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.lt.qb $t4, $t1, $t5", 0xaaaaaaaa,
                               0x55555555, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.lt.qb $t6, $t7, $t3", 0x00000018,
                               0xffff2435, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.lt.qb $t5, $t3, $t2", 0xbabababa,
                               0xabababab, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.lt.qb $t2, $t4, $t8", 0xf0f0f0f0,
                               0xfc79b4d2, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.lt.qb $t0, $t8, $t0", 0xfbde3976,
                               0x00000000, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.lt.qb $t4, $t6, $t1", 0x23534870,
                               0x00354565, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.lt.qb $t0, $t1, $t2", 0x980b7cde,
                               0x00086755, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.lt.qb $t2, $t3, $t4", 0x00000018,
                               0x8f8f8f8f, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.lt.qb $t4, $t1, $t5", 0x92784656,
                               0xeeeeeeee, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.lt.qb $t6, $t7, $t3", 0xcacacaca,
                               0x1bdbdbdb, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.lt.qb $t5, $t3, $t2", 0xbacabaca,
                               0xdecadeca, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.lt.qb $t2, $t4, $t8", 0x12fadeb4,
                               0x93474bde, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.lt.qb $t0, $t8, $t0", 0x7c000790,
                               0xfc0007ff, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.lt.qb $t4, $t6, $t1", 0xffffffff,
                               0xffffffff, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.lt.qb $t0, $t1, $t2", 0xf2f4df1f,
                               0xcb4ab48f, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.lt.qb $t2, $t3, $t4", 0x435f909a,
                               0xaf8f7e18, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.lt.qb $t4, $t1, $t5", 0x2106ba5f,
                               0x87df4510, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.lt.qb $t6, $t7, $t3", 0x246a6376,
                               0xabf4e8e1, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.lt.qb $t5, $t3, $t2", 0x1046a1a3,
                               0xf4c0eeac, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.lt.qb $t2, $t4, $t8", 0x638ca515,
                               0x006a54f2, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.lt.qb $t0, $t8, $t0", 0xf63e7a9d,
                               0x79f74493, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.lt.qb $t4, $t6, $t1", 0xbd6845cd,
                               0x9c09e313, t4, t6, t1);

   printf("-------- CMPGU.LE.QB --------\n");
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.le.qb $t0, $t1, $t2", 0x00672300,
                               0x00000000, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.le.qb $t2, $t3, $t4", 0x00045fb2,
                               0x00000286, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.le.qb $t4, $t1, $t5", 0x00002435,
                               0xffff3421, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.le.qb $t6, $t7, $t3", 0x07654cb8,
                               0x734680bc, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.le.qb $t5, $t3, $t2", 0xf973437b,
                               0x80000000, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.le.qb $t2, $t4, $t8", 0x00010001,
                               0xf08b4631, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.le.qb $t0, $t8, $t0", 0x5cbd891a,
                               0x7fff7fff, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.le.qb $t4, $t6, $t1", 0x0000c420,
                               0x0ab64555, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.le.qb $t2, $t3, $t4", 0x00000004,
                               1073741824, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.le.qb $t4, $t1, $t5", 0x80002435,
                               0x80003421, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.le.qb $t6, $t7, $t3", 0x76548000,
                               0x73468000, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.le.qb $t5, $t3, $t2", 0x8007c560,
                               0x80000000, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.le.qb $t2, $t4, $t8", 0x00010001,
                               0xffffffff, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.le.qb $t0, $t8, $t0", 0x7fff7fff,
                               0x7fff7fff, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.le.qb $t4, $t6, $t1", 0x0000c420,
                               0x00000555, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.le.qb $t0, $t1, $t2", 0x00000000,
                               0x00000000, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.le.qb $t2, $t3, $t4", 0x80000000,
                               0x80000000, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.le.qb $t4, $t1, $t5", 0xaaaaaaaa,
                               0x55555555, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.le.qb $t6, $t7, $t3", 0x00000018,
                               0xffff2435, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.le.qb $t5, $t3, $t2", 0xbabababa,
                               0xabababab, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.le.qb $t2, $t4, $t8", 0xf0f0f0f0,
                               0xfc79b4d2, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.le.qb $t0, $t8, $t0", 0xfbde3976,
                               0x00000000, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.le.qb $t4, $t6, $t1", 0x23534870,
                               0x00354565, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.le.qb $t0, $t1, $t2", 0x980b7cde,
                               0x00086755, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.le.qb $t2, $t3, $t4", 0x00000018,
                               0x8f8f8f8f, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.le.qb $t4, $t1, $t5", 0x92784656,
                               0xeeeeeeee, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.le.qb $t6, $t7, $t3", 0xcacacaca,
                               0x1bdbdbdb, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.le.qb $t5, $t3, $t2", 0xbacabaca,
                               0xdecadeca, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.le.qb $t2, $t4, $t8", 0x12fadeb4,
                               0x93474bde, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.le.qb $t0, $t8, $t0", 0x7c000790,
                               0xfc0007ff, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.le.qb $t4, $t6, $t1", 0xffffffff,
                               0xffffffff, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.le.qb $t0, $t1, $t2", 0xf2f4df1f,
                               0xcb4ab48f, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.le.qb $t2, $t3, $t4", 0x435f909a,
                               0xaf8f7e18, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.le.qb $t4, $t1, $t5", 0x2106ba5f,
                               0x87df4510, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.le.qb $t6, $t7, $t3", 0x246a6376,
                               0xabf4e8e1, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.le.qb $t5, $t3, $t2", 0x1046a1a3,
                               0xf4c0eeac, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.le.qb $t2, $t4, $t8", 0x638ca515,
                               0x006a54f2, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.le.qb $t0, $t8, $t0", 0xf63e7a9d,
                               0x79f74493, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("cmpgu.le.qb $t4, $t6, $t1", 0xbd6845cd,
                               0x9c09e313, t4, t6, t1);

   printf("-------- CMPU.EQ.QB --------\n");
   TESTDSPINST_RS_RT_DSPC("cmpu.eq.qb $t0, $t1", 0x00000000, 0x0fffffff, t0,
                          t1);
   TESTDSPINST_RS_RT_DSPC("cmpu.eq.qb $t2, $t3", 0x00005fb2, 0x00000286, t2,
                          t3);
   TESTDSPINST_RS_RT_DSPC("cmpu.eq.qb $t4, $t1", 0xfabc2435, 0xfabc3421, t4,
                          t1);
   TESTDSPINST_RS_RT_DSPC("cmpu.eq.qb $t6, $t7", 0x07654cb8, 0x73464cb8, t6,
                          t7);
   TESTDSPINST_RS_RT_DSPC("cmpu.eq.qb $t5, $t3", 0xf973437b, 0x80734300, t5,
                          t3);
   TESTDSPINST_RS_RT_DSPC("cmpu.eq.qb $t2, $t4", 0x00010001, 0xffffffff, t2,
                          t4);
   TESTDSPINST_RS_RT_DSPC("cmpu.eq.qb $t0, $t8", 0x7fff7fff, 0x7fff7fff, t0,
                          t8);
   TESTDSPINST_RS_RT_DSPC("cmpu.eq.qb $t4, $t6", 0x0000c420, 0x0000c420, t4,
                          t6);
   TESTDSPINST_RS_RT_DSPC("cmpu.eq.qb $t0, $t1", 0x00000000, 0x00000000, t0,
                          t1);
   TESTDSPINST_RS_RT_DSPC("cmpu.eq.qb $t2, $t3", 0x80000000, 0x80000000, t2,
                          t3);
   TESTDSPINST_RS_RT_DSPC("cmpu.eq.qb $t4, $t1", 0xaaaaaaaa, 0x55555555, t4,
                          t1);
   TESTDSPINST_RS_RT_DSPC("cmpu.eq.qb $t6, $t7", 0x00000018, 0xffff2435, t6,
                          t7);
   TESTDSPINST_RS_RT_DSPC("cmpu.eq.qb $t5, $t3", 0xbabababa, 0xabababab, t5,
                          t3);
   TESTDSPINST_RS_RT_DSPC("cmpu.eq.qb $t2, $t4", 0xf0f0f0f0, 0xfc79b4d2, t2,
                          t4);
   TESTDSPINST_RS_RT_DSPC("cmpu.eq.qb $t0, $t8", 0xfbde3976, 0x00000000, t0,
                          t8);
   TESTDSPINST_RS_RT_DSPC("cmpu.eq.qb $t4, $t6", 0x23534870, 0x00354565, t4,
                          t6);
   TESTDSPINST_RS_RT_DSPC("cmpu.eq.qb $t0, $t1", 0x980b7cde, 0x00086755, t0,
                          t1);
   TESTDSPINST_RS_RT_DSPC("cmpu.eq.qb $t2, $t3", 0x00000018, 0x8f8f8f8f, t2,
                          t3);
   TESTDSPINST_RS_RT_DSPC("cmpu.eq.qb $t4, $t1", 0x92784656, 0xeeeeeeee, t4,
                          t1);
   TESTDSPINST_RS_RT_DSPC("cmpu.eq.qb $t6, $t7", 0xcacacaca, 0x1bdbdbdb, t6,
                          t7);
   TESTDSPINST_RS_RT_DSPC("cmpu.eq.qb $t5, $t3", 0xbacabaca, 0xdecadeca, t5,
                          t3);
   TESTDSPINST_RS_RT_DSPC("cmpu.eq.qb $t2, $t4", 0x12fadeb4, 0x93474bde, t2,
                          t4);
   TESTDSPINST_RS_RT_DSPC("cmpu.eq.qb $t0, $t8", 0x7c000790, 0xfc0007ff, t0,
                          t8);
   TESTDSPINST_RS_RT_DSPC("cmpu.eq.qb $t4, $t6", 0xffffffff, 0xffffffff, t4,
                          t6);
   TESTDSPINST_RS_RT_DSPC("cmpu.eq.qb $t0, $t1", 0xf2f4df1f, 0xcb4ab48f, t0,
                          t1);
   TESTDSPINST_RS_RT_DSPC("cmpu.eq.qb $t2, $t3", 0x435f909a, 0xaf8f7e18, t2,
                          t3);
   TESTDSPINST_RS_RT_DSPC("cmpu.eq.qb $t4, $t1", 0x2106ba5f, 0x87df4510, t4,
                          t1);
   TESTDSPINST_RS_RT_DSPC("cmpu.eq.qb $t6, $t7", 0x246a6376, 0xabf4e8e1, t6,
                          t7);
   TESTDSPINST_RS_RT_DSPC("cmpu.eq.qb $t5, $t3", 0x1046a1a3, 0xf4c0eeac, t5,
                          t3);
   TESTDSPINST_RS_RT_DSPC("cmpu.eq.qb $t2, $t4", 0x638ca515, 0x006a54f2, t2,
                          t4);
   TESTDSPINST_RS_RT_DSPC("cmpu.eq.qb $t0, $t8", 0xf63e7a9d, 0x79f74493, t0,
                          t8);
   TESTDSPINST_RS_RT_DSPC("cmpu.eq.qb $t4, $t6", 0xbd6845cd, 0x9c09e313, t4,
                          t6);

   printf("-------- CMPU.LT.QB --------\n");
   TESTDSPINST_RS_RT_DSPC("cmpu.lt.qb $t0, $t1", 0x00000000, 0x0fffffff, t0,
                          t1);
   TESTDSPINST_RS_RT_DSPC("cmpu.lt.qb $t2, $t3", 0x00045fb2, 0x01080286, t2,
                          t3);
   TESTDSPINST_RS_RT_DSPC("cmpu.lt.qb $t4, $t1", 0xfabc2435, 0xfabc3421, t4,
                          t1);
   TESTDSPINST_RS_RT_DSPC("cmpu.lt.qb $t6, $t7", 0x07654cb8, 0x73464cb8, t6,
                          t7);
   TESTDSPINST_RS_RT_DSPC("cmpu.lt.qb $t5, $t3", 0xf973437b, 0x80000000, t5,
                          t3);
   TESTDSPINST_RS_RT_DSPC("cmpu.lt.qb $t2, $t4", 0xffffffff, 0x00010001, t2,
                          t4);
   TESTDSPINST_RS_RT_DSPC("cmpu.lt.qb $t0, $t8", 0x7fff7fff, 0x7fff7fff, t0,
                          t8);
   TESTDSPINST_RS_RT_DSPC("cmpu.lt.qb $t4, $t6", 0x0000c420, 0x00000555, t4,
                          t6);
   TESTDSPINST_RS_RT_DSPC("cmpu.lt.qb $t0, $t1", 0x00000000, 0x00000000, t0,
                          t1);
   TESTDSPINST_RS_RT_DSPC("cmpu.lt.qb $t2, $t3", 0x80000000, 0x80000000, t2,
                          t3);
   TESTDSPINST_RS_RT_DSPC("cmpu.lt.qb $t4, $t1", 0xaaaaaaaa, 0x55555555, t4,
                          t1);
   TESTDSPINST_RS_RT_DSPC("cmpu.lt.qb $t6, $t7", 0x00000018, 0xffff2435, t6,
                          t7);
   TESTDSPINST_RS_RT_DSPC("cmpu.lt.qb $t5, $t3", 0xbabababa, 0xabababab, t5,
                          t3);
   TESTDSPINST_RS_RT_DSPC("cmpu.lt.qb $t2, $t4", 0xf0f0f0f0, 0xfc79b4d2, t2,
                          t4);
   TESTDSPINST_RS_RT_DSPC("cmpu.lt.qb $t0, $t8", 0xfbde3976, 0x00000000, t0,
                          t8);
   TESTDSPINST_RS_RT_DSPC("cmpu.lt.qb $t4, $t6", 0x23534870, 0x00354565, t4,
                          t6);
   TESTDSPINST_RS_RT_DSPC("cmpu.lt.qb $t0, $t1", 0x980b7cde, 0x00086755, t0,
                          t1);
   TESTDSPINST_RS_RT_DSPC("cmpu.lt.qb $t2, $t3", 0x00000018, 0x8f8f8f8f, t2,
                          t3);
   TESTDSPINST_RS_RT_DSPC("cmpu.lt.qb $t4, $t1", 0x92784656, 0xeeeeeeee, t4,
                          t1);
   TESTDSPINST_RS_RT_DSPC("cmpu.lt.qb $t6, $t7", 0xcacacaca, 0x1bdbdbdb, t6,
                          t7);
   TESTDSPINST_RS_RT_DSPC("cmpu.lt.qb $t5, $t3", 0xbacabaca, 0xdecadeca, t5,
                          t3);
   TESTDSPINST_RS_RT_DSPC("cmpu.lt.qb $t2, $t4", 0x12fadeb4, 0x93474bde, t2,
                          t4);
   TESTDSPINST_RS_RT_DSPC("cmpu.lt.qb $t0, $t8", 0x7c000790, 0xfc0007ff, t0,
                          t8);
   TESTDSPINST_RS_RT_DSPC("cmpu.lt.qb $t4, $t6", 0xffffffff, 0xffffffff, t4,
                          t6);
   TESTDSPINST_RS_RT_DSPC("cmpu.lt.qb $t0, $t1", 0xf2f4df1f, 0xcb4ab48f, t0,
                          t1);
   TESTDSPINST_RS_RT_DSPC("cmpu.lt.qb $t2, $t3", 0x435f909a, 0xaf8f7e18, t2,
                          t3);
   TESTDSPINST_RS_RT_DSPC("cmpu.lt.qb $t4, $t1", 0x2106ba5f, 0x87df4510, t4,
                          t1);
   TESTDSPINST_RS_RT_DSPC("cmpu.lt.qb $t6, $t7", 0x246a6376, 0xabf4e8e1, t6,
                          t7);
   TESTDSPINST_RS_RT_DSPC("cmpu.lt.qb $t5, $t3", 0x1046a1a3, 0xf4c0eeac, t5,
                          t3);
   TESTDSPINST_RS_RT_DSPC("cmpu.lt.qb $t2, $t4", 0x638ca515, 0x006a54f2, t2,
                          t4);
   TESTDSPINST_RS_RT_DSPC("cmpu.lt.qb $t0, $t8", 0xf63e7a9d, 0x79f74493, t0,
                          t8);
   TESTDSPINST_RS_RT_DSPC("cmpu.lt.qb $t4, $t6", 0xbd6845cd, 0x9c09e313, t4,
                          t6);

   printf("-------- CMPU.LE.QB --------\n");
   TESTDSPINST_RS_RT_DSPC("cmpu.le.qb $t0, $t1", 0x00000000, 0x0fffffff, t0,
                          t1);
   TESTDSPINST_RS_RT_DSPC("cmpu.le.qb $t2, $t3", 0x00045fb2, 0x01040286, t2,
                          t3);
   TESTDSPINST_RS_RT_DSPC("cmpu.le.qb $t4, $t1", 0xfabc2435, 0xfabc3421, t4,
                          t1);
   TESTDSPINST_RS_RT_DSPC("cmpu.le.qb $t6, $t7", 0x07654cb8, 0x73464cb8, t6,
                          t7);
   TESTDSPINST_RS_RT_DSPC("cmpu.le.qb $t5, $t3", 0xf973437b, 0x80000000, t5,
                          t3);
   TESTDSPINST_RS_RT_DSPC("cmpu.le.qb $t2, $t4", 0x00010001, 0xffffffff, t2,
                          t4);
   TESTDSPINST_RS_RT_DSPC("cmpu.le.qb $t0, $t8", 0x7fff7fff, 0x7fff7fff, t0,
                          t8);
   TESTDSPINST_RS_RT_DSPC("cmpu.le.qb $t4, $t6", 0x0000c420, 0x00000555, t4,
                          t6);
   TESTDSPINST_RS_RT_DSPC("cmpu.le.qb $t0, $t1", 0x00000000, 0x00000000, t0,
                          t1);
   TESTDSPINST_RS_RT_DSPC("cmpu.le.qb $t2, $t3", 0x80000000, 0x80000000, t2,
                          t3);
   TESTDSPINST_RS_RT_DSPC("cmpu.le.qb $t4, $t1", 0xaaaaaaaa, 0x55555555, t4,
                          t1);
   TESTDSPINST_RS_RT_DSPC("cmpu.le.qb $t6, $t7", 0x00000018, 0xffff2435, t6,
                          t7);
   TESTDSPINST_RS_RT_DSPC("cmpu.le.qb $t5, $t3", 0xbabababa, 0xabababab, t5,
                          t3);
   TESTDSPINST_RS_RT_DSPC("cmpu.le.qb $t2, $t4", 0xf0f0f0f0, 0xfc79b4d2, t2,
                          t4);
   TESTDSPINST_RS_RT_DSPC("cmpu.le.qb $t0, $t8", 0xfbde3976, 0x00000000, t0,
                          t8);
   TESTDSPINST_RS_RT_DSPC("cmpu.le.qb $t4, $t6", 0x23534870, 0x00354565, t4,
                          t6);
   TESTDSPINST_RS_RT_DSPC("cmpu.le.qb $t0, $t1", 0x980b7cde, 0x00086755, t0,
                          t1);
   TESTDSPINST_RS_RT_DSPC("cmpu.le.qb $t2, $t3", 0x00000018, 0x8f8f8f8f, t2,
                          t3);
   TESTDSPINST_RS_RT_DSPC("cmpu.le.qb $t4, $t1", 0x92784656, 0xeeeeeeee, t4,
                          t1);
   TESTDSPINST_RS_RT_DSPC("cmpu.le.qb $t6, $t7", 0xcacacaca, 0x1bdbdbdb, t6,
                          t7);
   TESTDSPINST_RS_RT_DSPC("cmpu.le.qb $t5, $t3", 0xbacabaca, 0xdecadeca, t5,
                          t3);
   TESTDSPINST_RS_RT_DSPC("cmpu.le.qb $t2, $t4", 0x12fadeb4, 0x93474bde, t2,
                          t4);
   TESTDSPINST_RS_RT_DSPC("cmpu.le.qb $t0, $t8", 0x7c000790, 0xfc0007ff, t0,
                          t8);
   TESTDSPINST_RS_RT_DSPC("cmpu.le.qb $t4, $t6", 0xffffffff, 0xffffffff, t4,
                          t6);
   TESTDSPINST_RS_RT_DSPC("cmpu.le.qb $t0, $t1", 0xf2f4df1f, 0xcb4ab48f, t0,
                          t1);
   TESTDSPINST_RS_RT_DSPC("cmpu.le.qb $t2, $t3", 0x435f909a, 0xaf8f7e18, t2,
                          t3);
   TESTDSPINST_RS_RT_DSPC("cmpu.le.qb $t4, $t1", 0x2106ba5f, 0x87df4510, t4,
                          t1);
   TESTDSPINST_RS_RT_DSPC("cmpu.le.qb $t6, $t7", 0x246a6376, 0xabf4e8e1, t6,
                          t7);
   TESTDSPINST_RS_RT_DSPC("cmpu.le.qb $t5, $t3", 0x1046a1a3, 0xf4c0eeac, t5,
                          t3);
   TESTDSPINST_RS_RT_DSPC("cmpu.le.qb $t2, $t4", 0x638ca515, 0x006a54f2, t2,
                          t4);
   TESTDSPINST_RS_RT_DSPC("cmpu.le.qb $t0, $t8", 0xf63e7a9d, 0x79f74493, t0,
                          t8);
   TESTDSPINST_RS_RT_DSPC("cmpu.le.qb $t4, $t6", 0xbd6845cd, 0x9c09e313, t4,
                          t6);

   printf("-------- DPAQ_S.W.PH --------\n");
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_s.w.ph $ac3, $t4, $t5", "ac3", 0x00000000,
                             0x00000000, 0xffffffff, 0x80000000, t4, t5);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_s.w.ph $ac0, $t0, $t1", "ac0", 0x00000004,
                             1073741824, 0x00000000, 0x00000006, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_s.w.ph $ac1, $t2, $t3", "ac1", 0x80002435,
                             0x80003421, 0x00000000, 1073741824, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_s.w.ph $ac3, $t6, $t7", "ac3", 0x76548000,
                             0x73468000, 0x00000000, 0x7fffffff, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_s.w.ph $ac0, $t5, $t3", "ac0", 0x80000000,
                             0x80000000, 0x00000000, 0x00000001, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_s.w.ph $ac1, $t2, $t4", "ac1", 0x00010001,
                             0xffffffff, 0xffffffff, 0xffffffff, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_s.w.ph $ac2, $t0, $t8", "ac2", 0x7fff7fff,
                             0x7fff7fff, 0xffffffff, 0xffffffff, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_s.w.ph $ac0, $t0, $t1", "ac0", 0x0000c420,
                             0x00000555, 0x00000000, 0x0fde3126, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_s.w.ph $ac1, $t2, $t3", "ac1", 0x00000000,
                             0x00000000, 0x00000000, 0x55555555, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_s.w.ph $ac2, $t4, $t1", "ac2", 0x80000000,
                             0x80000000, 0xffffffff, 0xffff2435, t4, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_s.w.ph $ac3, $t6, $t7", "ac3", 0xaaaaaaaa,
                             0x55555555, 0xffffffff, 0xabababab, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_s.w.ph $ac0, $t5, $t3", "ac0", 0x00000018,
                             0xffff2435, 0xffffffff, 0xfc79b4d2, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_s.w.ph $ac1, $t2, $t4", "ac1", 0xbabababa,
                             0xabababab, 0x00000000, 0x00000000, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_s.w.ph $ac2, $t0, $t8", "ac2", 0xf0f0f0f0,
                             0xfc79b4d2, 0x00000000, 0x00000000, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_s.w.ph $ac3, $t4, $t5", "ac3", 0xfbde3976,
                             0x00000000, 0x00000000, 0x12349876, t4, t5);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_s.w.ph $ac0, $t0, $t1", "ac0", 0x23534870,
                             0x00354565, 0x00000000, 0x00354565, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_s.w.ph $ac1, $t2, $t3", "ac1", 0x980b7cde,
                             0x00086755, 0x00000000, 0x00086755, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_s.w.ph $ac2, $t4, $t1", "ac2", 0x00000018,
                             0x8f8f8f8f, 0xffffffff, 0x8f8f8f8f, t4, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_s.w.ph $ac3, $t6, $t7", "ac3", 0x92784656,
                             0xeeeeeeee, 0xffffffff, 0xeeeeeeee, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_s.w.ph $ac0, $t5, $t3", "ac0", 0xcacacaca,
                             0x1bdbdbdb, 0x00000000, 0x1bdbdbdb, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_s.w.ph $ac1, $t2, $t4", "ac1", 0xbacabaca,
                             0xdecadeca, 0xffffffff, 0xdecadeca, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_s.w.ph $ac2, $t0, $t8", "ac2", 0x12fadeb4,
                             0x93474bde, 0xffffffff, 0x93474bde, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_s.w.ph $ac3, $t4, $t5", "ac3", 0x7c000790,
                             0xfc0007ff, 0xffffffff, 0xfabfabfa, t4, t5);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_s.w.ph $ac2, $t0, $t8", "ac2", 0xffffffff,
                             0xffffffff, 0x00000000, 0x083b3571, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_s.w.ph $ac0, $t0, $t1", "ac0", 0x24a3291e,
                             0x5648e540, 0xffffffff, 0xb9743941, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_s.w.ph $ac1, $t2, $t3", "ac1", 0xdd91eebf,
                             0xc54f79e6, 0xffffffff, 0xbce5f924, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_s.w.ph $ac2, $t4, $t1", "ac2", 0xf7ce2ec6,
                             0x5fc92974, 0xffffffff, 0xcc3c201c, t4, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_s.w.ph $ac3, $t6, $t7", "ac3", 0xbc1083e8,
                             0x7e08184e, 0x00000000, 0x1ebaf88e, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_s.w.ph $ac0, $t5, $t3", "ac0", 0xa617cc31,
                             0x71c8315f, 0x00000000, 0x722d5e20, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_s.w.ph $ac1, $t2, $t4", "ac1", 0xdfe1e8f0,
                             0x9493110e, 0xffffffff, 0xa1d6f791, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_s.w.ph $ac2, $t0, $t8", "ac2", 0x31458a23,
                             0xbb246228, 0x00000000, 0x7b11bee7, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_s.w.ph $ac3, $t4, $t5", "ac3", 0x848af791,
                             0x339d8d88, 0xffffffff, 0xa5631488, t4, t5);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_s.w.ph $ac0, $t0, $t1", "ac0", 0xda3bacdc,
                             0x70974249, 0xffffffff, 0xb10bcc65, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_s.w.ph $ac1, $t2, $t3", "ac1", 0x649d5cbd,
                             0x8a8d4e7d, 0x00000000, 0x73f39fca, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_s.w.ph $ac2, $t4, $t1", "ac2", 0xc0c8c881,
                             0xeb1b4335, 0x00000000, 0x5648e540, t4, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_s.w.ph $ac3, $t6, $t7", "ac3", 0x7dd81a20,
                             0x0cd6b508, 0xffffffff, 0xc54f79e6, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_s.w.ph $ac0, $t5, $t3", "ac0", 0x7fff7fff,
                             0x6731e282, 0x00000000, 0x5fc92974, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_s.w.ph $ac1, $t2, $t4", "ac1", 0x00000555,
                             0xb6edf28f, 0x00000000, 0x7e08184e, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_s.w.ph $ac2, $t0, $t8", "ac2", 0x00000000,
                             0x4b4ec9ca, 0x00000000, 0x71c8315f, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_s.w.ph $ac3, $t4, $t5", "ac3", 0x80000000,
                             0xc1037fa4, 0xffffffff, 0x9493110e, t4, t5);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_s.w.ph $ac1, $t2, $t4", "ac1", 0x55555555,
                             0xcb4ab48f, 0xffffffff, 0xbb246228, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_s.w.ph $ac2, $t0, $t8", "ac2", 0xffff8000,
                             0xaf8f8000, 0x00000000, 0x339d8d88, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_s.w.ph $ac0, $t0, $t1", "ac0", 0xabababab,
                             0x87df4510, 0x00000000, 0x70974249, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_s.w.ph $ac1, $t2, $t3", "ac1", 0xfc79b4d2,
                             0xabf4e8e1, 0xffffffff, 0x8a8d4e7d, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_s.w.ph $ac2, $t4, $t1", "ac2", 0x00000000,
                             0xf4c0eeac, 0xffffffff, 0xeb1b4335, t4, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_s.w.ph $ac3, $t6, $t7", "ac3", 0x00354565,
                             0x006a54f2, 0x00000000, 0x0cd6b508, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_s.w.ph $ac0, $t5, $t3", "ac0", 0x00086755,
                             0x79f74493, 0x00000000, 0x6731e282, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_s.w.ph $ac1, $t2, $t4", "ac1", 0xffff8000,
                             0x9c098000, 0xffffffff, 0xb6edf28f, t2, t4);


   printf("-------- DPAQ_SA.L.W -------- \n");
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_sa.l.w $ac3, $t4, $t5", "ac3", 0x00000000,
                             0x00000000, 0xffffffff, 0x80000000, t4, t5);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_sa.l.w $ac0, $t0, $t1", "ac0", 0x00000004,
                             1073741824, 0x00000000, 0x00000006, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_sa.l.w $ac1, $t2, $t3", "ac1", 0x80002435,
                             0x80003421, 0x00000000, 1073741824, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_sa.l.w $ac3, $t6, $t7", "ac3", 0x76548000,
                             0x73468000, 0x00000000, 0x7fffffff, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_sa.l.w $ac0, $t5, $t3", "ac0", 0x80000000,
                             0x80000000, 0x00000000, 0x00000001, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_sa.l.w $ac1, $t2, $t4", "ac1", 0x00010001,
                             0xffffffff, 0xffffffff, 0xffffffff, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_sa.l.w $ac2, $t0, $t8", "ac2", 0x7fff7fff,
                             0x7fff7fff, 0xffffffff, 0xffffffff, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_sa.l.w $ac0, $t0, $t1", "ac0", 0x0000c420,
                             0x00000555, 0x00000000, 0x0fde3126, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_sa.l.w $ac1, $t2, $t3", "ac1", 0x00000000,
                             0x00000000, 0x00000000, 0x55555555, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_sa.l.w $ac2, $t4, $t1", "ac2", 0x80000000,
                             0x80000000, 0xffffffff, 0xffff2435, t4, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_sa.l.w $ac3, $t6, $t7", "ac3", 0xaaaaaaaa,
                             0x55555555, 0xffffffff, 0xabababab, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_sa.l.w $ac0, $t5, $t3", "ac0", 0x00000018,
                             0xffff2435, 0xffffffff, 0xfc79b4d2, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_sa.l.w $ac1, $t2, $t4", "ac1", 0xbabababa,
                             0xabababab, 0x00000000, 0x00000000, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_sa.l.w $ac2, $t0, $t8", "ac2", 0xf0f0f0f0,
                             0xfc79b4d2, 0x00000000, 0x00000000, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_sa.l.w $ac3, $t4, $t5", "ac3", 0xfbde3976,
                             0x00000000, 0x00000000, 0x12349876, t4, t5);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_sa.l.w $ac0, $t0, $t1", "ac0", 0x23534870,
                             0x00354565, 0x00000000, 0x00354565, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_sa.l.w $ac1, $t2, $t3", "ac1", 0x980b7cde,
                             0x00086755, 0x00000000, 0x00086755, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_sa.l.w $ac2, $t4, $t1", "ac2", 0x00000018,
                             0x8f8f8f8f, 0xffffffff, 0x8f8f8f8f, t4, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_sa.l.w $ac3, $t6, $t7", "ac3", 0x92784656,
                             0xeeeeeeee, 0xffffffff, 0xeeeeeeee, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_sa.l.w $ac0, $t5, $t3", "ac0", 0xcacacaca,
                             0x1bdbdbdb, 0x00000000, 0x1bdbdbdb, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_sa.l.w $ac1, $t2, $t4", "ac1", 0xbacabaca,
                             0xdecadeca, 0xffffffff, 0xdecadeca, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_sa.l.w $ac2, $t0, $t8", "ac2", 0x12fadeb4,
                             0x93474bde, 0xffffffff, 0x93474bde, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_sa.l.w $ac3, $t4, $t5", "ac3", 0x7c000790,
                             0xfc0007ff, 0xffffffff, 0xfabfabfa, t4, t5);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_sa.l.w $ac2, $t0, $t8", "ac2", 0xffffffff,
                             0xffffffff, 0x00000000, 0x083b3571, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_sa.l.w $ac0, $t0, $t1", "ac0", 0x24a3291e,
                             0x5648e540, 0xffffffff, 0xb9743941, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_sa.l.w $ac1, $t2, $t3", "ac1", 0xdd91eebf,
                             0xc54f79e6, 0xffffffff, 0xbce5f924, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_sa.l.w $ac2, $t4, $t1", "ac2", 0xf7ce2ec6,
                             0x5fc92974, 0xffffffff, 0xcc3c201c, t4, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_sa.l.w $ac3, $t6, $t7", "ac3", 0xbc1083e8,
                             0x7e08184e, 0x00000000, 0x1ebaf88e, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_sa.l.w $ac0, $t5, $t3", "ac0", 0xa617cc31,
                             0x71c8315f, 0x00000000, 0x722d5e20, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_sa.l.w $ac1, $t2, $t4", "ac1", 0xdfe1e8f0,
                             0x9493110e, 0xffffffff, 0xa1d6f791, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_sa.l.w $ac2, $t0, $t8", "ac2", 0x31458a23,
                             0xbb246228, 0x00000000, 0x7b11bee7, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_sa.l.w $ac3, $t4, $t5", "ac3", 0x848af791,
                             0x339d8d88, 0xffffffff, 0xa5631488, t4, t5);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_sa.l.w $ac0, $t0, $t1", "ac0", 0xda3bacdc,
                             0x70974249, 0xffffffff, 0xb10bcc65, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_sa.l.w $ac1, $t2, $t3", "ac1", 0x649d5cbd,
                             0x8a8d4e7d, 0x00000000, 0x73f39fca, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_sa.l.w $ac2, $t4, $t1", "ac2", 0xc0c8c881,
                             0xeb1b4335, 0x00000000, 0x5648e540, t4, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_sa.l.w $ac3, $t6, $t7", "ac3", 0x7dd81a20,
                             0x0cd6b508, 0xffffffff, 0xc54f79e6, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_sa.l.w $ac0, $t5, $t3", "ac0", 0x7fff7fff,
                             0x6731e282, 0x00000000, 0x5fc92974, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_sa.l.w $ac1, $t2, $t4", "ac1", 0x00000555,
                             0xb6edf28f, 0x00000000, 0x7e08184e, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_sa.l.w $ac2, $t0, $t8", "ac2", 0x00000000,
                             0x4b4ec9ca, 0x00000000, 0x71c8315f, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_sa.l.w $ac3, $t4, $t5", "ac3", 0x80000000,
                             0xc1037fa4, 0xffffffff, 0x9493110e, t4, t5);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_sa.l.w $ac1, $t2, $t4", "ac1", 0x55555555,
                             0xcb4ab48f, 0xffffffff, 0xbb246228, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_sa.l.w $ac2, $t0, $t8", "ac2", 0xffff8000,
                             0xaf8f8000, 0x00000000, 0x339d8d88, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_sa.l.w $ac0, $t0, $t1", "ac0", 0xabababab,
                             0x87df4510, 0x00000000, 0x70974249, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_sa.l.w $ac1, $t2, $t3", "ac1", 0xfc79b4d2,
                             0xabf4e8e1, 0xffffffff, 0x8a8d4e7d, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_sa.l.w $ac2, $t4, $t1", "ac2", 0x00000000,
                             0xf4c0eeac, 0xffffffff, 0xeb1b4335, t4, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_sa.l.w $ac3, $t6, $t7", "ac3", 0x00354565,
                             0x006a54f2, 0x00000000, 0x0cd6b508, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_sa.l.w $ac0, $t5, $t3", "ac0", 0x00086755,
                             0x79f74493, 0x00000000, 0x6731e282, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpaq_sa.l.w $ac1, $t2, $t4", "ac1", 0xffff8000,
                             0x9c098000, 0xffffffff, 0xb6edf28f, t2, t4);


   printf("-------- DPAU.H.QBL --------\n");
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbl $ac3, $t4, $t5", "ac3", 0x00000000,
                               0x00000000, 0xffffffff, 0x80000000, t4, t5);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbl $ac0, $t0, $t1", "ac0", 0x00000004,
                               1073741824, 0x00000000, 0x00000006, t0, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbl $ac1, $t2, $t3", "ac1", 0x80002435,
                               0x80003421, 0x00000000, 1073741824, t2, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbl $ac3, $t6, $t7", "ac3", 0x76548000,
                               0x73468000, 0x00000000, 0x7fffffff, t6, t7);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbl $ac0, $t5, $t3", "ac0", 0x80000000,
                               0x80000000, 0x00000000, 0x00000001, t5, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbl $ac1, $t2, $t4", "ac1", 0x00010001,
                               0xffffffff, 0xffffffff, 0xffffffff, t2, t4);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbl $ac2, $t0, $t8", "ac2", 0x7fff7fff,
                               0x7fff7fff, 0xffffffff, 0xffffffff, t0, t8);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbl $ac0, $t0, $t1", "ac0", 0x0000c420,
                               0x00000555, 0x00000000, 0x0fde3126, t0, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbl $ac1, $t2, $t3", "ac1", 0x00000000,
                               0x00000000, 0x00000000, 0x55555555, t2, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbl $ac2, $t4, $t1", "ac2", 0x80000000,
                               0x80000000, 0xffffffff, 0xffff2435, t4, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbl $ac3, $t6, $t7", "ac3", 0xaaaaaaaa,
                               0x55555555, 0xffffffff, 0xabababab, t6, t7);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbl $ac0, $t5, $t3", "ac0", 0x00000018,
                               0xffff2435, 0xffffffff, 0xfc79b4d2, t5, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbl $ac1, $t2, $t4", "ac1", 0xbabababa,
                               0xabababab, 0x00000000, 0x00000000, t2, t4);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbl $ac2, $t0, $t8", "ac2", 0xf0f0f0f0,
                               0xfc79b4d2, 0x00000000, 0x00000000, t0, t8);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbl $ac3, $t4, $t5", "ac3", 0xfbde3976,
                               0x00000000, 0x00000000, 0x12349876, t4, t5);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbl $ac0, $t0, $t1", "ac0", 0x23534870,
                               0x00354565, 0x00000000, 0x00354565, t0, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbl $ac1, $t2, $t3", "ac1", 0x980b7cde,
                               0x00086755, 0x00000000, 0x00086755, t2, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbl $ac2, $t4, $t1", "ac2", 0x00000018,
                               0x8f8f8f8f, 0xffffffff, 0x8f8f8f8f, t4, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbl $ac3, $t6, $t7", "ac3", 0x92784656,
                               0xeeeeeeee, 0xffffffff, 0xeeeeeeee, t6, t7);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbl $ac0, $t5, $t3", "ac0", 0xcacacaca,
                               0x1bdbdbdb, 0x00000000, 0x1bdbdbdb, t5, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbl $ac1, $t2, $t4", "ac1", 0xbacabaca,
                               0xdecadeca, 0xffffffff, 0xdecadeca, t2, t4);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbl $ac2, $t0, $t8", "ac2", 0x12fadeb4,
                               0x93474bde, 0xffffffff, 0x93474bde, t0, t8);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbl $ac3, $t4, $t5", "ac3", 0x7c000790,
                               0xfc0007ff, 0xffffffff, 0xfabfabfa, t4, t5);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbl $ac2, $t0, $t8", "ac2", 0xffffffff,
                               0xffffffff, 0x00000000, 0x083b3571, t0, t8);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbl $ac0, $t0, $t1", "ac0", 0x24a3291e,
                               0x5648e540, 0xffffffff, 0xb9743941, t0, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbl $ac1, $t2, $t3", "ac1", 0xdd91eebf,
                               0xc54f79e6, 0xffffffff, 0xbce5f924, t2, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbl $ac2, $t4, $t1", "ac2", 0xf7ce2ec6,
                               0x5fc92974, 0xffffffff, 0xcc3c201c, t4, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbl $ac3, $t6, $t7", "ac3", 0xbc1083e8,
                               0x7e08184e, 0x00000000, 0x1ebaf88e, t6, t7);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbl $ac0, $t5, $t3", "ac0", 0xa617cc31,
                               0x71c8315f, 0x00000000, 0x722d5e20, t5, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbl $ac1, $t2, $t4", "ac1", 0xdfe1e8f0,
                               0x9493110e, 0xffffffff, 0xa1d6f791, t2, t4);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbl $ac2, $t0, $t8", "ac2", 0x31458a23,
                               0xbb246228, 0x00000000, 0x7b11bee7, t0, t8);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbl $ac3, $t4, $t5", "ac3", 0x848af791,
                               0x339d8d88, 0xffffffff, 0xa5631488, t4, t5);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbl $ac0, $t0, $t1", "ac0", 0xda3bacdc,
                               0x70974249, 0xffffffff, 0xb10bcc65, t0, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbl $ac1, $t2, $t3", "ac1", 0x649d5cbd,
                               0x8a8d4e7d, 0x00000000, 0x73f39fca, t2, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbl $ac2, $t4, $t1", "ac2", 0xc0c8c881,
                               0xeb1b4335, 0x00000000, 0x5648e540, t4, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbl $ac3, $t6, $t7", "ac3", 0x7dd81a20,
                               0x0cd6b508, 0xffffffff, 0xc54f79e6, t6, t7);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbl $ac0, $t5, $t3", "ac0", 0x7fff7fff,
                               0x6731e282, 0x00000000, 0x5fc92974, t5, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbl $ac1, $t2, $t4", "ac1", 0x00000555,
                               0xb6edf28f, 0x00000000, 0x7e08184e, t2, t4);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbl $ac2, $t0, $t8", "ac2", 0x00000000,
                               0x4b4ec9ca, 0x00000000, 0x71c8315f, t0, t8);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbl $ac3, $t4, $t5", "ac3", 0x80000000,
                               0xc1037fa4, 0xffffffff, 0x9493110e, t4, t5);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbl $ac1, $t2, $t4", "ac1", 0x55555555,
                               0xcb4ab48f, 0xffffffff, 0xbb246228, t2, t4);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbl $ac2, $t0, $t8", "ac2", 0xffff8000,
                               0xaf8f8000, 0x00000000, 0x339d8d88, t0, t8);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbl $ac0, $t0, $t1", "ac0", 0xabababab,
                               0x87df4510, 0x00000000, 0x70974249, t0, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbl $ac1, $t2, $t3", "ac1", 0xfc79b4d2,
                               0xabf4e8e1, 0xffffffff, 0x8a8d4e7d, t2, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbl $ac2, $t4, $t1", "ac2", 0x00000000,
                               0xf4c0eeac, 0xffffffff, 0xeb1b4335, t4, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbl $ac3, $t6, $t7", "ac3", 0x00354565,
                               0x006a54f2, 0x00000000, 0x0cd6b508, t6, t7);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbl $ac0, $t5, $t3", "ac0", 0x00086755,
                               0x79f74493, 0x00000000, 0x6731e282, t5, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbl $ac1, $t2, $t4", "ac1", 0xffff8000,
                               0x9c098000, 0xffffffff, 0xb6edf28f, t2, t4);

   printf("-------- DPAU.H.QBR --------\n");
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbr $ac3, $t4, $t5", "ac3", 0x00000000,
                               0x00000000, 0xffffffff, 0x80000000, t4, t5);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbr $ac0, $t0, $t1", "ac0", 0x00000004,
                               1073741824, 0x00000000, 0x00000006, t0, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbr $ac1, $t2, $t3", "ac1", 0x80002435,
                               0x80003421, 0x00000000, 1073741824, t2, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbr $ac3, $t6, $t7", "ac3", 0x76548000,
                               0x73468000, 0x00000000, 0x7fffffff, t6, t7);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbr $ac0, $t5, $t3", "ac0", 0x80000000,
                               0x80000000, 0x00000000, 0x00000001, t5, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbr $ac1, $t2, $t4", "ac1", 0x00010001,
                               0xffffffff, 0xffffffff, 0xffffffff, t2, t4);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbr $ac2, $t0, $t8", "ac2", 0x7fff7fff,
                               0x7fff7fff, 0xffffffff, 0xffffffff, t0, t8);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbr $ac0, $t0, $t1", "ac0", 0x0000c420,
                               0x00000555, 0x00000000, 0x0fde3126, t0, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbr $ac1, $t2, $t3", "ac1", 0x00000000,
                               0x00000000, 0x00000000, 0x55555555, t2, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbr $ac2, $t4, $t1", "ac2", 0x80000000,
                               0x80000000, 0xffffffff, 0xffff2435, t4, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbr $ac3, $t6, $t7", "ac3", 0xaaaaaaaa,
                               0x55555555, 0xffffffff, 0xabababab, t6, t7);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbr $ac0, $t5, $t3", "ac0", 0x00000018,
                               0xffff2435, 0xffffffff, 0xfc79b4d2, t5, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbr $ac1, $t2, $t4", "ac1", 0xbabababa,
                               0xabababab, 0x00000000, 0x00000000, t2, t4);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbr $ac2, $t0, $t8", "ac2", 0xf0f0f0f0,
                               0xfc79b4d2, 0x00000000, 0x00000000, t0, t8);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbr $ac3, $t4, $t5", "ac3", 0xfbde3976,
                               0x00000000, 0x00000000, 0x12349876, t4, t5);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbr $ac0, $t0, $t1", "ac0", 0x23534870,
                               0x00354565, 0x00000000, 0x00354565, t0, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbr $ac1, $t2, $t3", "ac1", 0x980b7cde,
                               0x00086755, 0x00000000, 0x00086755, t2, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbr $ac2, $t4, $t1", "ac2", 0x00000018,
                               0x8f8f8f8f, 0xffffffff, 0x8f8f8f8f, t4, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbr $ac3, $t6, $t7", "ac3", 0x92784656,
                               0xeeeeeeee, 0xffffffff, 0xeeeeeeee, t6, t7);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbr $ac0, $t5, $t3", "ac0", 0xcacacaca,
                               0x1bdbdbdb, 0x00000000, 0x1bdbdbdb, t5, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbr $ac1, $t2, $t4", "ac1", 0xbacabaca,
                               0xdecadeca, 0xffffffff, 0xdecadeca, t2, t4);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbr $ac2, $t0, $t8", "ac2", 0x12fadeb4,
                               0x93474bde, 0xffffffff, 0x93474bde, t0, t8);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbr $ac3, $t4, $t5", "ac3", 0x7c000790,
                               0xfc0007ff, 0xffffffff, 0xfabfabfa, t4, t5);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbr $ac2, $t0, $t8", "ac2", 0xffffffff,
                               0xffffffff, 0x00000000, 0x083b3571, t0, t8);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbr $ac0, $t0, $t1", "ac0", 0x24a3291e,
                               0x5648e540, 0xffffffff, 0xb9743941, t0, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbr $ac1, $t2, $t3", "ac1", 0xdd91eebf,
                               0xc54f79e6, 0xffffffff, 0xbce5f924, t2, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbr $ac2, $t4, $t1", "ac2", 0xf7ce2ec6,
                               0x5fc92974, 0xffffffff, 0xcc3c201c, t4, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbr $ac3, $t6, $t7", "ac3", 0xbc1083e8,
                               0x7e08184e, 0x00000000, 0x1ebaf88e, t6, t7);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbr $ac0, $t5, $t3", "ac0", 0xa617cc31,
                               0x71c8315f, 0x00000000, 0x722d5e20, t5, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbr $ac1, $t2, $t4", "ac1", 0xdfe1e8f0,
                               0x9493110e, 0xffffffff, 0xa1d6f791, t2, t4);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbr $ac2, $t0, $t8", "ac2", 0x31458a23,
                               0xbb246228, 0x00000000, 0x7b11bee7, t0, t8);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbr $ac3, $t4, $t5", "ac3", 0x848af791,
                               0x339d8d88, 0xffffffff, 0xa5631488, t4, t5);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbr $ac0, $t0, $t1", "ac0", 0xda3bacdc,
                               0x70974249, 0xffffffff, 0xb10bcc65, t0, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbr $ac1, $t2, $t3", "ac1", 0x649d5cbd,
                               0x8a8d4e7d, 0x00000000, 0x73f39fca, t2, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbr $ac2, $t4, $t1", "ac2", 0xc0c8c881,
                               0xeb1b4335, 0x00000000, 0x5648e540, t4, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbr $ac3, $t6, $t7", "ac3", 0x7dd81a20,
                               0x0cd6b508, 0xffffffff, 0xc54f79e6, t6, t7);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbr $ac0, $t5, $t3", "ac0", 0x7fff7fff,
                               0x6731e282, 0x00000000, 0x5fc92974, t5, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbr $ac1, $t2, $t4", "ac1", 0x00000555,
                               0xb6edf28f, 0x00000000, 0x7e08184e, t2, t4);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbr $ac2, $t0, $t8", "ac2", 0x00000000,
                               0x4b4ec9ca, 0x00000000, 0x71c8315f, t0, t8);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbr $ac3, $t4, $t5", "ac3", 0x80000000,
                               0xc1037fa4, 0xffffffff, 0x9493110e, t4, t5);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbr $ac1, $t2, $t4", "ac1", 0x55555555,
                               0xcb4ab48f, 0xffffffff, 0xbb246228, t2, t4);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbr $ac2, $t0, $t8", "ac2", 0xffff8000,
                               0xaf8f8000, 0x00000000, 0x339d8d88, t0, t8);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbr $ac0, $t0, $t1", "ac0", 0xabababab,
                               0x87df4510, 0x00000000, 0x70974249, t0, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbr $ac1, $t2, $t3", "ac1", 0xfc79b4d2,
                               0xabf4e8e1, 0xffffffff, 0x8a8d4e7d, t2, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbr $ac2, $t4, $t1", "ac2", 0x00000000,
                               0xf4c0eeac, 0xffffffff, 0xeb1b4335, t4, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbr $ac3, $t6, $t7", "ac3", 0x00354565,
                               0x006a54f2, 0x00000000, 0x0cd6b508, t6, t7);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbr $ac0, $t5, $t3", "ac0", 0x00086755,
                               0x79f74493, 0x00000000, 0x6731e282, t5, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("dpau.h.qbr $ac1, $t2, $t4", "ac1", 0xffff8000,
                               0x9c098000, 0xffffffff, 0xb6edf28f, t2, t4);

   printf("-------- DPSQ_S.W.PH --------\n");
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_s.w.ph $ac3, $t4, $t5", "ac3", 0x00000000,
                             0x00000000, 0xffffffff, 0x80000000, t4, t5);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_s.w.ph $ac0, $t0, $t1", "ac0", 0x00000004,
                             1073741824, 0x00000000, 0x00000006, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_s.w.ph $ac1, $t2, $t3", "ac1", 0x80002435,
                             0x80003421, 0x00000000, 1073741824, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_s.w.ph $ac3, $t6, $t7", "ac3", 0x76548000,
                             0x73468000, 0x00000000, 0x7fffffff, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_s.w.ph $ac0, $t5, $t3", "ac0", 0x80000000,
                             0x80000000, 0x00000000, 0x00000001, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_s.w.ph $ac1, $t2, $t4", "ac1", 0x00010001,
                             0xffffffff, 0xffffffff, 0xffffffff, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_s.w.ph $ac2, $t0, $t8", "ac2", 0x7fff7fff,
                             0x7fff7fff, 0xffffffff, 0xffffffff, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_s.w.ph $ac0, $t0, $t1", "ac0", 0x0000c420,
                             0x00000555, 0x00000000, 0x0fde3126, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_s.w.ph $ac1, $t2, $t3", "ac1", 0x00000000,
                             0x00000000, 0x00000000, 0x55555555, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_s.w.ph $ac2, $t4, $t1", "ac2", 0x80000000,
                             0x80000000, 0xffffffff, 0xffff2435, t4, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_s.w.ph $ac3, $t6, $t7", "ac3", 0xaaaaaaaa,
                             0x55555555, 0xffffffff, 0xabababab, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_s.w.ph $ac0, $t5, $t3", "ac0", 0x00000018,
                             0xffff2435, 0xffffffff, 0xfc79b4d2, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_s.w.ph $ac1, $t2, $t4", "ac1", 0xbabababa,
                             0xabababab, 0x00000000, 0x00000000, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_s.w.ph $ac2, $t0, $t8", "ac2", 0xf0f0f0f0,
                             0xfc79b4d2, 0x00000000, 0x00000000, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_s.w.ph $ac3, $t4, $t5", "ac3", 0xfbde3976,
                             0x00000000, 0x00000000, 0x12349876, t4, t5);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_s.w.ph $ac0, $t0, $t1", "ac0", 0x23534870,
                             0x00354565, 0x00000000, 0x00354565, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_s.w.ph $ac1, $t2, $t3", "ac1", 0x980b7cde,
                             0x00086755, 0x00000000, 0x00086755, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_s.w.ph $ac2, $t4, $t1", "ac2", 0x00000018,
                             0x8f8f8f8f, 0xffffffff, 0x8f8f8f8f, t4, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_s.w.ph $ac3, $t6, $t7", "ac3", 0x92784656,
                             0xeeeeeeee, 0xffffffff, 0xeeeeeeee, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_s.w.ph $ac0, $t5, $t3", "ac0", 0xcacacaca,
                             0x1bdbdbdb, 0x00000000, 0x1bdbdbdb, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_s.w.ph $ac1, $t2, $t4", "ac1", 0xbacabaca,
                             0xdecadeca, 0xffffffff, 0xdecadeca, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_s.w.ph $ac2, $t0, $t8", "ac2", 0x12fadeb4,
                             0x93474bde, 0xffffffff, 0x93474bde, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_s.w.ph $ac3, $t4, $t5", "ac3", 0x7c000790,
                             0xfc0007ff, 0xffffffff, 0xfabfabfa, t4, t5);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_s.w.ph $ac2, $t0, $t8", "ac2", 0xffffffff,
                             0xffffffff, 0x00000000, 0x083b3571, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_s.w.ph $ac0, $t0, $t1", "ac0", 0x24a3291e,
                             0x5648e540, 0xffffffff, 0xb9743941, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_s.w.ph $ac1, $t2, $t3", "ac1", 0xdd91eebf,
                             0xc54f79e6, 0xffffffff, 0xbce5f924, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_s.w.ph $ac2, $t4, $t1", "ac2", 0xf7ce2ec6,
                             0x5fc92974, 0xffffffff, 0xcc3c201c, t4, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_s.w.ph $ac3, $t6, $t7", "ac3", 0xbc1083e8,
                             0x7e08184e, 0x00000000, 0x1ebaf88e, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_s.w.ph $ac0, $t5, $t3", "ac0", 0xa617cc31,
                             0x71c8315f, 0x00000000, 0x722d5e20, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_s.w.ph $ac1, $t2, $t4", "ac1", 0xdfe1e8f0,
                             0x9493110e, 0xffffffff, 0xa1d6f791, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_s.w.ph $ac2, $t0, $t8", "ac2", 0x31458a23,
                             0xbb246228, 0x00000000, 0x7b11bee7, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_s.w.ph $ac3, $t4, $t5", "ac3", 0x848af791,
                             0x339d8d88, 0xffffffff, 0xa5631488, t4, t5);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_s.w.ph $ac0, $t0, $t1", "ac0", 0xda3bacdc,
                             0x70974249, 0xffffffff, 0xb10bcc65, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_s.w.ph $ac1, $t2, $t3", "ac1", 0x649d5cbd,
                             0x8a8d4e7d, 0x00000000, 0x73f39fca, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_s.w.ph $ac2, $t4, $t1", "ac2", 0xc0c8c881,
                             0xeb1b4335, 0x00000000, 0x5648e540, t4, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_s.w.ph $ac3, $t6, $t7", "ac3", 0x7dd81a20,
                             0x0cd6b508, 0xffffffff, 0xc54f79e6, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_s.w.ph $ac0, $t5, $t3", "ac0", 0x7fff7fff,
                             0x6731e282, 0x00000000, 0x5fc92974, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_s.w.ph $ac1, $t2, $t4", "ac1", 0x00000555,
                             0xb6edf28f, 0x00000000, 0x7e08184e, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_s.w.ph $ac2, $t0, $t8", "ac2", 0x00000000,
                             0x4b4ec9ca, 0x00000000, 0x71c8315f, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_s.w.ph $ac3, $t4, $t5", "ac3", 0x80000000,
                             0xc1037fa4, 0xffffffff, 0x9493110e, t4, t5);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_s.w.ph $ac1, $t2, $t4", "ac1", 0x55555555,
                             0xcb4ab48f, 0xffffffff, 0xbb246228, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_s.w.ph $ac2, $t0, $t8", "ac2", 0xffff8000,
                             0xaf8f8000, 0x00000000, 0x339d8d88, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_s.w.ph $ac0, $t0, $t1", "ac0", 0xabababab,
                             0x87df4510, 0x00000000, 0x70974249, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_s.w.ph $ac1, $t2, $t3", "ac1", 0xfc79b4d2,
                             0xabf4e8e1, 0xffffffff, 0x8a8d4e7d, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_s.w.ph $ac2, $t4, $t1", "ac2", 0x00000000,
                             0xf4c0eeac, 0xffffffff, 0xeb1b4335, t4, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_s.w.ph $ac3, $t6, $t7", "ac3", 0x00354565,
                             0x006a54f2, 0x00000000, 0x0cd6b508, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_s.w.ph $ac0, $t5, $t3", "ac0", 0x00086755,
                             0x79f74493, 0x00000000, 0x6731e282, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_s.w.ph $ac1, $t2, $t4", "ac1", 0xffff8000,
                             0x9c098000, 0xffffffff, 0xb6edf28f, t2, t4);


   printf("-------- DPSQ_SA.L.W --------\n");
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_sa.l.w $ac3, $t4, $t5", "ac3", 0x00000000,
                             0x00000000, 0xffffffff, 0x80000000, t4, t5);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_sa.l.w $ac0, $t0, $t1", "ac0", 0x00000004,
                             1073741824, 0x00000000, 0x00000006, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_sa.l.w $ac1, $t2, $t3", "ac1", 0x80002435,
                             0x80003421, 0x00000000, 1073741824, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_sa.l.w $ac3, $t6, $t7", "ac3", 0x76548000,
                             0x73468000, 0x00000000, 0x7fffffff, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_sa.l.w $ac0, $t5, $t3", "ac0", 0x80000000,
                             0x80000000, 0x00000000, 0x00000001, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_sa.l.w $ac1, $t2, $t4", "ac1", 0x00010001,
                             0xffffffff, 0xffffffff, 0xffffffff, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_sa.l.w $ac2, $t0, $t8", "ac2", 0x7fff7fff,
                             0x7fff7fff, 0xffffffff, 0xffffffff, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_sa.l.w $ac0, $t0, $t1", "ac0", 0x0000c420,
                             0x00000555, 0x00000000, 0x0fde3126, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_sa.l.w $ac1, $t2, $t3", "ac1", 0x00000000,
                             0x00000000, 0x00000000, 0x55555555, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_sa.l.w $ac2, $t4, $t1", "ac2", 0x80000000,
                             0x80000000, 0xffffffff, 0xffff2435, t4, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_sa.l.w $ac3, $t6, $t7", "ac3", 0xaaaaaaaa,
                             0x55555555, 0xffffffff, 0xabababab, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_sa.l.w $ac0, $t5, $t3", "ac0", 0x00000018,
                             0xffff2435, 0xffffffff, 0xfc79b4d2, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_sa.l.w $ac1, $t2, $t4", "ac1", 0xbabababa,
                             0xabababab, 0x00000000, 0x00000000, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_sa.l.w $ac2, $t0, $t8", "ac2", 0xf0f0f0f0,
                             0xfc79b4d2, 0x00000000, 0x00000000, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_sa.l.w $ac3, $t4, $t5", "ac3", 0xfbde3976,
                             0x00000000, 0x00000000, 0x12349876, t4, t5);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_sa.l.w $ac0, $t0, $t1", "ac0", 0x23534870,
                             0x00354565, 0x00000000, 0x00354565, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_sa.l.w $ac1, $t2, $t3", "ac1", 0x980b7cde,
                             0x00086755, 0x00000000, 0x00086755, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_sa.l.w $ac2, $t4, $t1", "ac2", 0x00000018,
                             0x8f8f8f8f, 0xffffffff, 0x8f8f8f8f, t4, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_sa.l.w $ac3, $t6, $t7", "ac3", 0x92784656,
                             0xeeeeeeee, 0xffffffff, 0xeeeeeeee, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_sa.l.w $ac0, $t5, $t3", "ac0", 0xcacacaca,
                             0x1bdbdbdb, 0x00000000, 0x1bdbdbdb, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_sa.l.w $ac1, $t2, $t4", "ac1", 0xbacabaca,
                             0xdecadeca, 0xffffffff, 0xdecadeca, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_sa.l.w $ac2, $t0, $t8", "ac2", 0x12fadeb4,
                             0x93474bde, 0xffffffff, 0x93474bde, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_sa.l.w $ac3, $t4, $t5", "ac3", 0x7c000790,
                             0xfc0007ff, 0xffffffff, 0xfabfabfa, t4, t5);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_sa.l.w $ac2, $t0, $t8", "ac2", 0xffffffff,
                             0xffffffff, 0x00000000, 0x083b3571, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_sa.l.w $ac0, $t0, $t1", "ac0", 0x24a3291e,
                             0x5648e540, 0xffffffff, 0xb9743941, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_sa.l.w $ac1, $t2, $t3", "ac1", 0xdd91eebf,
                             0xc54f79e6, 0xffffffff, 0xbce5f924, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_sa.l.w $ac2, $t4, $t1", "ac2", 0xf7ce2ec6,
                             0x5fc92974, 0xffffffff, 0xcc3c201c, t4, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_sa.l.w $ac3, $t6, $t7", "ac3", 0xbc1083e8,
                             0x7e08184e, 0x00000000, 0x1ebaf88e, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_sa.l.w $ac0, $t5, $t3", "ac0", 0xa617cc31,
                             0x71c8315f, 0x00000000, 0x722d5e20, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_sa.l.w $ac1, $t2, $t4", "ac1", 0xdfe1e8f0,
                             0x9493110e, 0xffffffff, 0xa1d6f791, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_sa.l.w $ac2, $t0, $t8", "ac2", 0x31458a23,
                             0xbb246228, 0x00000000, 0x7b11bee7, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_sa.l.w $ac3, $t4, $t5", "ac3", 0x848af791,
                             0x339d8d88, 0xffffffff, 0xa5631488, t4, t5);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_sa.l.w $ac0, $t0, $t1", "ac0", 0xda3bacdc,
                             0x70974249, 0xffffffff, 0xb10bcc65, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_sa.l.w $ac1, $t2, $t3", "ac1", 0x649d5cbd,
                             0x8a8d4e7d, 0x00000000, 0x73f39fca, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_sa.l.w $ac2, $t4, $t1", "ac2", 0xc0c8c881,
                             0xeb1b4335, 0x00000000, 0x5648e540, t4, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_sa.l.w $ac3, $t6, $t7", "ac3", 0x7dd81a20,
                             0x0cd6b508, 0xffffffff, 0xc54f79e6, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_sa.l.w $ac0, $t5, $t3", "ac0", 0x7fff7fff,
                             0x6731e282, 0x00000000, 0x5fc92974, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_sa.l.w $ac1, $t2, $t4", "ac1", 0x00000555,
                             0xb6edf28f, 0x00000000, 0x7e08184e, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_sa.l.w $ac2, $t0, $t8", "ac2", 0x00000000,
                             0x4b4ec9ca, 0x00000000, 0x71c8315f, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_sa.l.w $ac3, $t4, $t5", "ac3", 0x80000000,
                             0xc1037fa4, 0xffffffff, 0x9493110e, t4, t5);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_sa.l.w $ac1, $t2, $t4", "ac1", 0x55555555,
                             0xcb4ab48f, 0xffffffff, 0xbb246228, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_sa.l.w $ac2, $t0, $t8", "ac2", 0xffff8000,
                             0xaf8f8000, 0x00000000, 0x339d8d88, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_sa.l.w $ac0, $t0, $t1", "ac0", 0xabababab,
                             0x87df4510, 0x00000000, 0x70974249, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_sa.l.w $ac1, $t2, $t3", "ac1", 0xfc79b4d2,
                             0xabf4e8e1, 0xffffffff, 0x8a8d4e7d, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_sa.l.w $ac2, $t4, $t1", "ac2", 0x00000000,
                             0xf4c0eeac, 0xffffffff, 0xeb1b4335, t4, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_sa.l.w $ac3, $t6, $t7", "ac3", 0x00354565,
                             0x006a54f2, 0x00000000, 0x0cd6b508, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_sa.l.w $ac0, $t5, $t3", "ac0", 0x00086755,
                             0x79f74493, 0x00000000, 0x6731e282, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpsq_sa.l.w $ac1, $t2, $t4", "ac1", 0xffff8000,
                             0x9c098000, 0xffffffff, 0xb6edf28f, t2, t4);


   printf("-------- DPSU.H.QBL --------\n");
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbl $ac3, $t4, $t5", "ac3", 0x00000000,
                               0x00000000, 0xffffffff, 0x80000000, t4, t5);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbl $ac0, $t0, $t1", "ac0", 0x00000004,
                               1073741824, 0x00000000, 0x00000006, t0, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbl $ac1, $t2, $t3", "ac1", 0x80002435,
                               0x80003421, 0x00000000, 1073741824, t2, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbl $ac3, $t6, $t7", "ac3", 0x76548000,
                               0x73468000, 0x00000000, 0x7fffffff, t6, t7);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbl $ac0, $t5, $t3", "ac0", 0x80000000,
                               0x80000000, 0x00000000, 0x00000001, t5, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbl $ac1, $t2, $t4", "ac1", 0x00010001,
                               0xffffffff, 0xffffffff, 0xffffffff, t2, t4);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbl $ac2, $t0, $t8", "ac2", 0x7fff7fff,
                               0x7fff7fff, 0xffffffff, 0xffffffff, t0, t8);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbl $ac0, $t0, $t1", "ac0", 0x0000c420,
                               0x00000555, 0x00000000, 0x0fde3126, t0, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbl $ac1, $t2, $t3", "ac1", 0x00000000,
                               0x00000000, 0x00000000, 0x55555555, t2, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbl $ac2, $t4, $t1", "ac2", 0x80000000,
                               0x80000000, 0xffffffff, 0xffff2435, t4, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbl $ac3, $t6, $t7", "ac3", 0xaaaaaaaa,
                               0x55555555, 0xffffffff, 0xabababab, t6, t7);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbl $ac0, $t5, $t3", "ac0", 0x00000018,
                               0xffff2435, 0xffffffff, 0xfc79b4d2, t5, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbl $ac1, $t2, $t4", "ac1", 0xbabababa,
                               0xabababab, 0x00000000, 0x00000000, t2, t4);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbl $ac2, $t0, $t8", "ac2", 0xf0f0f0f0,
                               0xfc79b4d2, 0x00000000, 0x00000000, t0, t8);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbl $ac3, $t4, $t5", "ac3", 0xfbde3976,
                               0x00000000, 0x00000000, 0x12349876, t4, t5);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbl $ac0, $t0, $t1", "ac0", 0x23534870,
                               0x00354565, 0x00000000, 0x00354565, t0, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbl $ac1, $t2, $t3", "ac1", 0x980b7cde,
                               0x00086755, 0x00000000, 0x00086755, t2, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbl $ac2, $t4, $t1", "ac2", 0x00000018,
                               0x8f8f8f8f, 0xffffffff, 0x8f8f8f8f, t4, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbl $ac3, $t6, $t7", "ac3", 0x92784656,
                               0xeeeeeeee, 0xffffffff, 0xeeeeeeee, t6, t7);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbl $ac0, $t5, $t3", "ac0", 0xcacacaca,
                               0x1bdbdbdb, 0x00000000, 0x1bdbdbdb, t5, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbl $ac1, $t2, $t4", "ac1", 0xbacabaca,
                               0xdecadeca, 0xffffffff, 0xdecadeca, t2, t4);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbl $ac2, $t0, $t8", "ac2", 0x12fadeb4,
                               0x93474bde, 0xffffffff, 0x93474bde, t0, t8);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbl $ac3, $t4, $t5", "ac3", 0x7c000790,
                               0xfc0007ff, 0xffffffff, 0xfabfabfa, t4, t5);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbl $ac2, $t0, $t8", "ac2", 0xffffffff,
                               0xffffffff, 0x00000000, 0x083b3571, t0, t8);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbl $ac0, $t0, $t1", "ac0", 0x24a3291e,
                               0x5648e540, 0xffffffff, 0xb9743941, t0, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbl $ac1, $t2, $t3", "ac1", 0xdd91eebf,
                               0xc54f79e6, 0xffffffff, 0xbce5f924, t2, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbl $ac2, $t4, $t1", "ac2", 0xf7ce2ec6,
                               0x5fc92974, 0xffffffff, 0xcc3c201c, t4, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbl $ac3, $t6, $t7", "ac3", 0xbc1083e8,
                               0x7e08184e, 0x00000000, 0x1ebaf88e, t6, t7);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbl $ac0, $t5, $t3", "ac0", 0xa617cc31,
                               0x71c8315f, 0x00000000, 0x722d5e20, t5, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbl $ac1, $t2, $t4", "ac1", 0xdfe1e8f0,
                               0x9493110e, 0xffffffff, 0xa1d6f791, t2, t4);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbl $ac2, $t0, $t8", "ac2", 0x31458a23,
                               0xbb246228, 0x00000000, 0x7b11bee7, t0, t8);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbl $ac3, $t4, $t5", "ac3", 0x848af791,
                               0x339d8d88, 0xffffffff, 0xa5631488, t4, t5);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbl $ac0, $t0, $t1", "ac0", 0xda3bacdc,
                               0x70974249, 0xffffffff, 0xb10bcc65, t0, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbl $ac1, $t2, $t3", "ac1", 0x649d5cbd,
                               0x8a8d4e7d, 0x00000000, 0x73f39fca, t2, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbl $ac2, $t4, $t1", "ac2", 0xc0c8c881,
                               0xeb1b4335, 0x00000000, 0x5648e540, t4, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbl $ac3, $t6, $t7", "ac3", 0x7dd81a20,
                               0x0cd6b508, 0xffffffff, 0xc54f79e6, t6, t7);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbl $ac0, $t5, $t3", "ac0", 0x7fff7fff,
                               0x6731e282, 0x00000000, 0x5fc92974, t5, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbl $ac1, $t2, $t4", "ac1", 0x00000555,
                               0xb6edf28f, 0x00000000, 0x7e08184e, t2, t4);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbl $ac2, $t0, $t8", "ac2", 0x00000000,
                               0x4b4ec9ca, 0x00000000, 0x71c8315f, t0, t8);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbl $ac3, $t4, $t5", "ac3", 0x80000000,
                               0xc1037fa4, 0xffffffff, 0x9493110e, t4, t5);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbl $ac1, $t2, $t4", "ac1", 0x55555555,
                               0xcb4ab48f, 0xffffffff, 0xbb246228, t2, t4);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbl $ac2, $t0, $t8", "ac2", 0xffff8000,
                               0xaf8f8000, 0x00000000, 0x339d8d88, t0, t8);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbl $ac0, $t0, $t1", "ac0", 0xabababab,
                               0x87df4510, 0x00000000, 0x70974249, t0, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbl $ac1, $t2, $t3", "ac1", 0xfc79b4d2,
                               0xabf4e8e1, 0xffffffff, 0x8a8d4e7d, t2, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbl $ac2, $t4, $t1", "ac2", 0x00000000,
                               0xf4c0eeac, 0xffffffff, 0xeb1b4335, t4, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbl $ac3, $t6, $t7", "ac3", 0x00354565,
                               0x006a54f2, 0x00000000, 0x0cd6b508, t6, t7);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbl $ac0, $t5, $t3", "ac0", 0x00086755,
                               0x79f74493, 0x00000000, 0x6731e282, t5, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbl $ac1, $t2, $t4", "ac1", 0xffff8000,
                               0x9c098000, 0xffffffff, 0xb6edf28f, t2, t4);

   printf("-------- DPSU.H.QBR --------\n");
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbr $ac3, $t4, $t5", "ac3", 0x00000000,
                               0x00000000, 0xffffffff, 0x80000000, t4, t5);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbr $ac0, $t0, $t1", "ac0", 0x00000004,
                               1073741824, 0x00000000, 0x00000006, t0, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbr $ac1, $t2, $t3", "ac1", 0x80002435,
                               0x80003421, 0x00000000, 1073741824, t2, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbr $ac3, $t6, $t7", "ac3", 0x76548000,
                               0x73468000, 0x00000000, 0x7fffffff, t6, t7);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbr $ac0, $t5, $t3", "ac0", 0x80000000,
                               0x80000000, 0x00000000, 0x00000001, t5, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbr $ac1, $t2, $t4", "ac1", 0x00010001,
                               0xffffffff, 0xffffffff, 0xffffffff, t2, t4);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbr $ac2, $t0, $t8", "ac2", 0x7fff7fff,
                               0x7fff7fff, 0xffffffff, 0xffffffff, t0, t8);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbr $ac0, $t0, $t1", "ac0", 0x0000c420,
                               0x00000555, 0x00000000, 0x0fde3126, t0, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbr $ac1, $t2, $t3", "ac1", 0x00000000,
                               0x00000000, 0x00000000, 0x55555555, t2, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbr $ac2, $t4, $t1", "ac2", 0x80000000,
                               0x80000000, 0xffffffff, 0xffff2435, t4, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbr $ac3, $t6, $t7", "ac3", 0xaaaaaaaa,
                               0x55555555, 0xffffffff, 0xabababab, t6, t7);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbr $ac0, $t5, $t3", "ac0", 0x00000018,
                               0xffff2435, 0xffffffff, 0xfc79b4d2, t5, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbr $ac1, $t2, $t4", "ac1", 0xbabababa,
                               0xabababab, 0x00000000, 0x00000000, t2, t4);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbr $ac2, $t0, $t8", "ac2", 0xf0f0f0f0,
                               0xfc79b4d2, 0x00000000, 0x00000000, t0, t8);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbr $ac3, $t4, $t5", "ac3", 0xfbde3976,
                               0x00000000, 0x00000000, 0x12349876, t4, t5);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbr $ac0, $t0, $t1", "ac0", 0x23534870,
                               0x00354565, 0x00000000, 0x00354565, t0, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbr $ac1, $t2, $t3", "ac1", 0x980b7cde,
                               0x00086755, 0x00000000, 0x00086755, t2, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbr $ac2, $t4, $t1", "ac2", 0x00000018,
                               0x8f8f8f8f, 0xffffffff, 0x8f8f8f8f, t4, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbr $ac3, $t6, $t7", "ac3", 0x92784656,
                               0xeeeeeeee, 0xffffffff, 0xeeeeeeee, t6, t7);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbr $ac0, $t5, $t3", "ac0", 0xcacacaca,
                               0x1bdbdbdb, 0x00000000, 0x1bdbdbdb, t5, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbr $ac1, $t2, $t4", "ac1", 0xbacabaca,
                               0xdecadeca, 0xffffffff, 0xdecadeca, t2, t4);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbr $ac2, $t0, $t8", "ac2", 0x12fadeb4,
                               0x93474bde, 0xffffffff, 0x93474bde, t0, t8);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbr $ac3, $t4, $t5", "ac3", 0x7c000790,
                               0xfc0007ff, 0xffffffff, 0xfabfabfa, t4, t5);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbr $ac2, $t0, $t8", "ac2", 0xffffffff,
                               0xffffffff, 0x00000000, 0x083b3571, t0, t8);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbr $ac0, $t0, $t1", "ac0", 0x24a3291e,
                               0x5648e540, 0xffffffff, 0xb9743941, t0, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbr $ac1, $t2, $t3", "ac1", 0xdd91eebf,
                               0xc54f79e6, 0xffffffff, 0xbce5f924, t2, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbr $ac2, $t4, $t1", "ac2", 0xf7ce2ec6,
                               0x5fc92974, 0xffffffff, 0xcc3c201c, t4, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbr $ac3, $t6, $t7", "ac3", 0xbc1083e8,
                               0x7e08184e, 0x00000000, 0x1ebaf88e, t6, t7);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbr $ac0, $t5, $t3", "ac0", 0xa617cc31,
                               0x71c8315f, 0x00000000, 0x722d5e20, t5, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbr $ac1, $t2, $t4", "ac1", 0xdfe1e8f0,
                               0x9493110e, 0xffffffff, 0xa1d6f791, t2, t4);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbr $ac2, $t0, $t8", "ac2", 0x31458a23,
                               0xbb246228, 0x00000000, 0x7b11bee7, t0, t8);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbr $ac3, $t4, $t5", "ac3", 0x848af791,
                               0x339d8d88, 0xffffffff, 0xa5631488, t4, t5);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbr $ac0, $t0, $t1", "ac0", 0xda3bacdc,
                               0x70974249, 0xffffffff, 0xb10bcc65, t0, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbr $ac1, $t2, $t3", "ac1", 0x649d5cbd,
                               0x8a8d4e7d, 0x00000000, 0x73f39fca, t2, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbr $ac2, $t4, $t1", "ac2", 0xc0c8c881,
                               0xeb1b4335, 0x00000000, 0x5648e540, t4, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbr $ac3, $t6, $t7", "ac3", 0x7dd81a20,
                               0x0cd6b508, 0xffffffff, 0xc54f79e6, t6, t7);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbr $ac0, $t5, $t3", "ac0", 0x7fff7fff,
                               0x6731e282, 0x00000000, 0x5fc92974, t5, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbr $ac1, $t2, $t4", "ac1", 0x00000555,
                               0xb6edf28f, 0x00000000, 0x7e08184e, t2, t4);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbr $ac2, $t0, $t8", "ac2", 0x00000000,
                               0x4b4ec9ca, 0x00000000, 0x71c8315f, t0, t8);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbr $ac3, $t4, $t5", "ac3", 0x80000000,
                               0xc1037fa4, 0xffffffff, 0x9493110e, t4, t5);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbr $ac1, $t2, $t4", "ac1", 0x55555555,
                               0xcb4ab48f, 0xffffffff, 0xbb246228, t2, t4);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbr $ac2, $t0, $t8", "ac2", 0xffff8000,
                               0xaf8f8000, 0x00000000, 0x339d8d88, t0, t8);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbr $ac0, $t0, $t1", "ac0", 0xabababab,
                               0x87df4510, 0x00000000, 0x70974249, t0, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbr $ac1, $t2, $t3", "ac1", 0xfc79b4d2,
                               0xabf4e8e1, 0xffffffff, 0x8a8d4e7d, t2, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbr $ac2, $t4, $t1", "ac2", 0x00000000,
                               0xf4c0eeac, 0xffffffff, 0xeb1b4335, t4, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbr $ac3, $t6, $t7", "ac3", 0x00354565,
                               0x006a54f2, 0x00000000, 0x0cd6b508, t6, t7);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbr $ac0, $t5, $t3", "ac0", 0x00086755,
                               0x79f74493, 0x00000000, 0x6731e282, t5, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsu.h.qbr $ac1, $t2, $t4", "ac1", 0xffff8000,
                               0x9c098000, 0xffffffff, 0xb6edf28f, t2, t4);

   printf("-------- EXTP --------\n");
   TESTDSPINST_EXT("extp $t1, $ac0, 31", "ac0", t1, 0x80000000, 0x80000000, 31,
                   42);
   TESTDSPINST_EXT("extp $t2, $ac1, 17", "ac1", t2, (1<<31)+1, (1 <<31)+2, 17,
                   31);
   TESTDSPINST_EXT("extp $t3, $ac2,  4", "ac2", t3, 0x00000000, 0x00000006,  4,
                   55);
   TESTDSPINST_EXT("extp $t4, $ac3, 12", "ac3", t4, 0x00000004, 1073741824, 12,
                   22);
   TESTDSPINST_EXT("extp $t5, $ac0,  3", "ac0", t5, 0x7fffffff, 0x7fffffff,  3,
                   63);
   TESTDSPINST_EXT("extp $t6, $ac1,  8", "ac1", t6, 0xffffffff, 0x00000001,  8,
                   13);
   TESTDSPINST_EXT("extp $t7, $ac2, 16", "ac2", t7, 0x00000001, 0xffffffff, 16,
                   60);
   TESTDSPINST_EXT("extp $t8, $ac3, 22", "ac3", t8, 0xffffffff, 0xffffffff, 22,
                   22);
   TESTDSPINST_EXT("extp $t0, $ac0,  9", "ac0", t0, 0x0000cdfe, 0x0fde3126,  9,
                   18);
   TESTDSPINST_EXT("extp $t2, $ac2, 16", "ac2", t2, 0x00000018, 0xffff2435, 16,
                   30);
   TESTDSPINST_EXT("extp $t3, $ac3,  6", "ac3", t3, 0xbabababa, 0xabababab,  6,
                   17);
   TESTDSPINST_EXT("extp $t4, $ac0, 13", "ac0", t4, 0xf0f0f0f0, 0xfc79b4d2, 13,
                   32);
   TESTDSPINST_EXT("extp $t5, $ac1, 19", "ac1", t5, 0x00000000, 0x00000000, 19,
                   20);
   TESTDSPINST_EXT("extp $t6, $ac2, 27", "ac2", t6, 0xfbde3976, 0x00000000, 27,
                   30);
   TESTDSPINST_EXT("extp $t7, $ac3,  7", "ac3", t7, 0xb0ed7654, 0x12349876,  7,
                   32);
   TESTDSPINST_EXT("extp $t8, $ac0, 11", "ac0", t8, 0x23534870, 0x35004565, 11,
                   37);
   TESTDSPINST_EXT("extp $t0, $ac1, 26", "ac1", t0, 0x980b7cde, 0x86700055, 26,
                   61);
   TESTDSPINST_EXT("extp $t1, $ac2, 15", "ac2", t1, 0x00000018, 0x8f8f8f8f, 15,
                   28);
   TESTDSPINST_EXT("extp $t2, $ac3,  2", "ac3", t2, 0x92784656, 0xeeeeeeee,  2,
                   14);
   TESTDSPINST_EXT("extp $t3, $ac0,  1", "ac0", t3, 0xcacacaca, 0x1bdbdbdb,  1,
                   58);
   TESTDSPINST_EXT("extp $t4, $ac1,  0", "ac1", t4, 0xbacabaca, 0xdecadeca,  0,
                   19);

   printf("-------- EXTPDP --------\n");
   TESTDSPINST_EXT("extpdp $t1, $ac0, 31", "ac0", t1, 0x80000000, 0x80000000,
                   31, 42);
   TESTDSPINST_EXT("extpdp $t2, $ac1, 17", "ac1", t2, (1<<31)+1, (1<<31)+2, 17,
                   31);
   TESTDSPINST_EXT("extpdp $t3, $ac2,  4", "ac2", t3, 0x00000000, 0x00000006,
                    4, 55);
   TESTDSPINST_EXT("extpdp $t4, $ac3, 12", "ac3", t4, 0x00000004, 1073741824,
                   12, 22);
   TESTDSPINST_EXT("extpdp $t5, $ac0,  3", "ac0", t5, 0x7fffffff, 0x7fffffff,
                    3, 63);
   TESTDSPINST_EXT("extpdp $t6, $ac1,  8", "ac1", t6, 0xffffffff, 0x00000001,
                    8, 13);
   TESTDSPINST_EXT("extpdp $t7, $ac2, 16", "ac2", t7, 0x00000001, 0xffffffff,
                   16, 60);
   TESTDSPINST_EXT("extpdp $t8, $ac3, 22", "ac3", t8, 0xffffffff, 0xffffffff,
                   22, 22);
   TESTDSPINST_EXT("extpdp $t0, $ac0,  9", "ac0", t0, 0x0000cdfe, 0x0fde3126,
                    9, 18);
   TESTDSPINST_EXT("extpdp $t3, $ac3,  6", "ac3", t3, 0xbabababa, 0xabababab,
                    6, 17);
   TESTDSPINST_EXT("extpdp $t4, $ac0, 13", "ac0", t4, 0xf0f0f0f0, 0xfc79b4d2,
                   13, 32);
   TESTDSPINST_EXT("extpdp $t5, $ac1, 19", "ac1", t5, 0x00000000, 0x00000000,
                   19, 20);
   TESTDSPINST_EXT("extpdp $t6, $ac2, 27", "ac2", t6, 0xfbde3976, 0x00000000,
                   27, 30);
   TESTDSPINST_EXT("extpdp $t7, $ac3,  7", "ac3", t7, 0x0bed7654, 0x12349876,
                    7, 32);
   TESTDSPINST_EXT("extpdp $t8, $ac0, 11", "ac0", t8, 0x23534870, 0x00354565,
                   11, 37);
   TESTDSPINST_EXT("extpdp $t0, $ac1, 26", "ac1", t0, 0x980b7cde, 0x00086755,
                   26, 61);
   TESTDSPINST_EXT("extpdp $t1, $ac2, 15", "ac2", t1, 0x00000018, 0x8f8f8f8f,
                   15, 28);
   TESTDSPINST_EXT("extpdp $t2, $ac3,  2", "ac3", t2, 0x92784656, 0xeeeeeeee,
                    2, 14);
   TESTDSPINST_EXT("extpdp $t3, $ac0,  1", "ac0", t3, 0xcacacaca, 0x1bdbdbdb,
                    1, 58);
   TESTDSPINST_EXT("extpdp $t4, $ac1,  0", "ac1", t4, 0xbacabaca, 0xdecadeca,
                    0, 19);

   printf("-------- EXTPDPV --------\n");
   TESTDSPINST_EXTV("extpdpv $t1, $ac0, $t2", "ac0", t1, 0x80000000, 0x80000000,
                    t2, 0x80000000, 42);
   TESTDSPINST_EXTV("extpdpv $t2, $ac1, $t3", "ac1", t2, (1<<31)+1, (1<<31)+2,
                    t3, 0x06, 31);
   TESTDSPINST_EXTV("extpdpv $t3, $ac2, $t4", "ac2", t3, 0x00000000, 0x00000006,
                    t4, 0x7fffffff, 55);
   TESTDSPINST_EXTV("extpdpv $t5, $ac0, $t6", "ac0", t5, 0x7fffffff, 0x7fffffff,
                    t6, 0x0fde3126, 63);
   TESTDSPINST_EXTV("extpdpv $t7, $ac2, $t8", "ac2", t7, 0x00000001, 0xffffffff,
                    t8, 0xaaaaaaaa, 60);
   TESTDSPINST_EXTV("extpdpv $t8, $ac3, $t9", "ac3", t8, 0xffffffff, 0xffffffff,
                    t9, 0xffff2435, 22);
   TESTDSPINST_EXTV("extpdpv $t4, $ac0, $t3", "ac0", t4, 0xf0f0f0f0, 0xfc79b4d2,
                    t3, 0x12349876, 32);
   TESTDSPINST_EXTV("extpdpv $t5, $ac1, $t4", "ac1", t5, 0x00000000, 0x00000000,
                    t4, 0x00354565, 20);
   TESTDSPINST_EXTV("extpdpv $t6, $ac2, $t5", "ac2", t6, 0xfbde3976, 0x00000000,
                    t5, 0x00086755, 30);
   TESTDSPINST_EXTV("extpdpv $t7, $ac3, $t6", "ac3", t7, 0x0bed7654, 0x12349876,
                    t6, 0x00000018, 32);
   TESTDSPINST_EXTV("extpdpv $t8, $ac0, $t7", "ac0", t8, 0x23534870, 0x00354565,
                    t7, 0x23534870, 37);
   TESTDSPINST_EXTV("extpdpv $t0, $ac1, $t8", "ac1", t0, 0x980b7cde, 0x00086755,
                    t8, 0x92784656, 61);
   TESTDSPINST_EXTV("extpdpv $t1, $ac2, $t9", "ac2", t1, 0x00000018, 0x8f8f8f8f,
                    t9, 0xeeeeeeee, 28);
   TESTDSPINST_EXTV("extpdpv $t2, $ac3, $t1", "ac3", t2, 0x92784656, 0xeeeeeeee,
                    t1, 0xcacacaca, 14);
   TESTDSPINST_EXTV("extpdpv $t3, $ac0, $t1", "ac0", t3, 0xcacacaca, 0x1bdbdbdb,
                    t1, 0xbacabaca, 58);
   TESTDSPINST_EXTV("extpdpv $t4, $ac1, $t4", "ac1", t4, 0xbacabaca, 0xdecadeca,
                    t4, 0x1bdbdbdb, 19);

   printf("-------- EXTPV --------\n");
   TESTDSPINST_EXTV("extpv $t1, $ac0, $t2", "ac0", t1, 0x80000000, 0x80000000,
                    t2, 0x80000000, 42);
   TESTDSPINST_EXTV("extpv $t2, $ac1, $t3", "ac1", t2, (1<<31)+1, (1<<31)+2, t3,
                    0x06, 31);
   TESTDSPINST_EXTV("extpv $t3, $ac2, $t4", "ac2", t3, 0x00000000, 0x00000006,
                    t4, 0x7fffffff, 55);
   TESTDSPINST_EXTV("extpv $t5, $ac0, $t6", "ac0", t5, 0x7fffffff, 0x7fffffff,
                    t6, 0x0fde3126, 63);
   TESTDSPINST_EXTV("extpv $t7, $ac2, $t8", "ac2", t7, 0x00000001, 0xffffffff,
                    t8, 0xaaaaaaaa, 60);
   TESTDSPINST_EXTV("extpv $t8, $ac3, $t9", "ac3", t8, 0xffffffff, 0xffffffff,
                    t9, 0xffff2435, 22);
   TESTDSPINST_EXTV("extpv $t4, $ac0, $t3", "ac0", t4, 0xf0f0f0f0, 0xfc79b4d2,
                    t3, 0x12349876, 32);
   TESTDSPINST_EXTV("extpv $t5, $ac1, $t4", "ac1", t5, 0x00000000, 0x00000000,
                    t4, 0x00354565, 20);
   TESTDSPINST_EXTV("extpv $t6, $ac2, $t5", "ac2", t6, 0xfbde3976, 0x00000000,
                    t5, 0x00086755, 30);
   TESTDSPINST_EXTV("extpv $t7, $ac3, $t6", "ac3", t7, 0x0bed7654, 0x12349876,
                    t6, 0x00000018, 32);
   TESTDSPINST_EXTV("extpv $t8, $ac0, $t7", "ac0", t8, 0x23534870, 0x00354565,
                    t7, 0x23534870, 37);
   TESTDSPINST_EXTV("extpv $t0, $ac1, $t8", "ac1", t0, 0x980b7cde, 0x00086755,
                    t8, 0x92784656, 61);
   TESTDSPINST_EXTV("extpv $t1, $ac2, $t9", "ac2", t1, 0x00000018, 0x8f8f8f8f,
                    t9, 0xeeeeeeee, 28);
   TESTDSPINST_EXTV("extpv $t2, $ac3, $t1", "ac3", t2, 0x92784656, 0xeeeeeeee,
                    t1, 0xcacacaca, 14);
   TESTDSPINST_EXTV("extpv $t3, $ac0, $t1", "ac0", t3, 0xcacacaca, 0x1bdbdbdb,
                    t1, 0xbacabaca, 58);
   TESTDSPINST_EXTV("extpv $t4, $ac1, $t4", "ac1", t4, 0xbacabaca, 0xdecadeca,
                    t4, 0x1bdbdbdb, 19);

   printf("-------- EXTR_S.H --------\n");
   TESTDSPINST_EXT("extr_s.h $t4, $ac3,  0", "ac3", t4, 0x00000000, 0x00000000,
                   0, 0);
   TESTDSPINST_EXT("extr_s.h $t5, $ac0, 31", "ac0", t5, 0x7fffffff, 0xcbcdef01,
                   31, 0);
   TESTDSPINST_EXT("extr_s.h $t6, $ac1, 31", "ac1", t6, 0x3fffffff, 0x2bcdef01,
                   31, 0);
   TESTDSPINST_EXT("extr_s.h $t7, $ac2,  0", "ac2", t7, 0xff34bc6f, 0xffffffff,
                   0, 0);
   TESTDSPINST_EXT("extr_s.h $t8, $ac3,  1", "ac3", t8, 0x00000000, 0xfffffffe,
                   1, 0);
   TESTDSPINST_EXT("extr_s.h $t1, $ac0, 31", "ac0", t1, 0x80000000, 0x80000000,
                   31, 0);
   TESTDSPINST_EXT("extr_s.h $t2, $ac1, 17", "ac1", t2, (1<<31)+1, (1<<31)+2,
                   17, 0);
   TESTDSPINST_EXT("extr_s.h $t3, $ac2,  4", "ac2", t3, 0x00000000, 0x00000006,
                   4, 0);
   TESTDSPINST_EXT("extr_s.h $t4, $ac3, 12", "ac3", t4, 0x00000004, 1073741824,
                   12, 0);
   TESTDSPINST_EXT("extr_s.h $t5, $ac0,  3", "ac0", t5, 0x7fffffff, 0x7fffffff,
                   3, 0);
   TESTDSPINST_EXT("extr_s.h $t6, $ac1,  8", "ac1", t6, 0xffffffff, 0x00000001,
                   8, 0);
   TESTDSPINST_EXT("extr_s.h $t7, $ac2, 16", "ac2", t7, 0x00000001, 0xffffffff,
                   16, 0);
   TESTDSPINST_EXT("extr_s.h $t8, $ac3, 22", "ac3", t8, 0xffffffff, 0xffffffff,
                   22, 0);
   TESTDSPINST_EXT("extr_s.h $t0, $ac0,  9", "ac0", t0, 0x0000cdfe, 0x0fde3126,
                   9, 0);
   TESTDSPINST_EXT("extr_s.h $t2, $ac2, 14", "ac2", t2, 0x00000018, 0xffff2435,
                   14, 0);
   TESTDSPINST_EXT("extr_s.h $t1, $ac1, 20", "ac1", t1, 0xaaaaaaaa, 0x55555555,
                   20, 0);
   TESTDSPINST_EXT("extr_s.h $t2, $ac2, 16", "ac2", t2, 0x00000018, 0xffff2435,
                   16, 0);
   TESTDSPINST_EXT("extr_s.h $t1, $ac1,  5", "ac1", t1, 0xaaaaaaaa, 0x55555555,
                   5, 0);
   TESTDSPINST_EXT("extr_s.h $t2, $ac2, 22", "ac2", t2, 0x00000018, 0xffff2435,
                   22, 0);
   TESTDSPINST_EXT("extr_s.h $t3, $ac3,  6", "ac3", t3, 0xbabababa, 0xabababab,
                   6, 0);
   TESTDSPINST_EXT("extr_s.h $t4, $ac0, 13", "ac0", t4, 0xf0f0f0f0, 0xfc79b4d2,
                   13, 0);
   TESTDSPINST_EXT("extr_s.h $t5, $ac1, 19", "ac1", t5, 0x00000000, 0x00000000,
                   19, 0);
   TESTDSPINST_EXT("extr_s.h $t6, $ac2, 27", "ac2", t6, 0xfbde3976, 0x00000000,
                   27, 0);
   TESTDSPINST_EXT("extr_s.h $t7, $ac3,  7", "ac3", t7, 0x0bed7654, 0x12349876,
                   7, 0);
   TESTDSPINST_EXT("extr_s.h $t8, $ac0, 11", "ac0", t8, 0x23534870, 0x00354565,
                   11, 0);
   TESTDSPINST_EXT("extr_s.h $t0, $ac1, 26", "ac1", t0, 0x980b7cde, 0x00086755,
                   26, 0);
   TESTDSPINST_EXT("extr_s.h $t1, $ac2, 15", "ac2", t1, 0x00000018, 0x8f8f8f8f,
                   15, 0);
   TESTDSPINST_EXT("extr_s.h $t2, $ac3,  2", "ac3", t2, 0x92784656, 0xeeeeeeee,
                   2, 0);
   TESTDSPINST_EXT("extr_s.h $t3, $ac0,  1", "ac0", t3, 0xcacacaca, 0x1bdbdbdb,
                   1, 0);
   TESTDSPINST_EXT("extr_s.h $t4, $ac1,  0", "ac1", t4, 0xbacabaca, 0xdecadeca,
                   0, 0);
   TESTDSPINST_EXT("extr_s.h $t5, $ac0,  3", "ac0", t5, 0x00000000, 0x5fc92974,
                   3, 0);
   TESTDSPINST_EXT("extr_s.h $t6, $ac1,  8", "ac1", t6, 0x00000000, 0x7e08184e,
                   8, 0);
   TESTDSPINST_EXT("extr_s.h $t7, $ac2, 16", "ac2", t7, 0x00000000, 0x71c8315f,
                   16, 0);
   TESTDSPINST_EXT("extr_s.h $t8, $ac3, 22", "ac3", t8, 0xffffffff, 0x9493110e,
                   22, 0);
   TESTDSPINST_EXT("extr_s.h $t0, $ac0,  9", "ac0", t0, 0xffffffff, 0xbb246228,
                   9, 0);
   TESTDSPINST_EXT("extr_s.h $t1, $ac1,  5", "ac1", t1, 0x00000000, 0x339d8d88,
                   5, 0);
   TESTDSPINST_EXT("extr_s.h $t2, $ac2, 14", "ac2", t2, 0x00000000, 0x70974249,
                   14, 0);
   TESTDSPINST_EXT("extr_s.h $t1, $ac1, 20", "ac1", t1, 0xffffffff, 0x8a8d4e7d,
                   20, 0);
   TESTDSPINST_EXT("extr_s.h $t2, $ac2, 16", "ac2", t2, 0xffffffff, 0xeb1b4335,
                   16, 0);
   TESTDSPINST_EXT("extr_s.h $t1, $ac1,  5", "ac1", t1, 0x00000000, 0x0cd6b508,
                   5, 0);
   TESTDSPINST_EXT("extr_s.h $t2, $ac2, 22", "ac2", t2, 0x00000000, 0x6731e282,
                   22, 0);
   TESTDSPINST_EXT("extr_s.h $t3, $ac3,  6", "ac3", t3, 0xffffffff, 0xb6edf28f,
                   6, 0);
   TESTDSPINST_EXT("extr_s.h $t4, $ac0, 13", "ac0", t4, 0x00000000, 0x4b4ec9ca,
                   13, 0);
   TESTDSPINST_EXT("extr_s.h $t5, $ac1, 19", "ac1", t5, 0xffffffff, 0xc1037fa4,
                   19, 0);
   TESTDSPINST_EXT("extr_s.h $t6, $ac2, 27", "ac2", t6, 0xffffffff, 0xcb4ab48f,
                   27, 0);
   TESTDSPINST_EXT("extr_s.h $t7, $ac3,  7", "ac3", t7, 0xffffffff, 0xaf8f7e18,
                   7, 0);
   TESTDSPINST_EXT("extr_s.h $t8, $ac0, 11", "ac0", t8, 0xffffffff, 0x87df4510,
                   11, 0);
   TESTDSPINST_EXT("extr_s.h $t0, $ac1, 26", "ac1", t0, 0xffffffff, 0xabf4e8e1,
                   26, 0);
   TESTDSPINST_EXT("extr_s.h $t1, $ac2, 15", "ac2", t1, 0xffffffff, 0xf4c0eeac,
                   15, 0);
   TESTDSPINST_EXT("extr_s.h $t2, $ac3,  2", "ac3", t2, 0x00000000, 0x006a54f2,
                   2, 0);
   TESTDSPINST_EXT("extr_s.h $t3, $ac0,  1", "ac0", t3, 0x00000000, 0x79f74493,
                   1, 0);
   TESTDSPINST_EXT("extr_s.h $t4, $ac1,  0", "ac1", t4, 0xffffffff, 0x9c09e313,
                   0, 0);

   printf("-------- EXTR.W --------\n");
   TESTDSPINST_EXT("extr.w $t4, $ac3,  0", "ac3", t4, 0x00000000, 0x00000000,
                   0, 0);
   TESTDSPINST_EXT("extr.w $t5, $ac0, 31", "ac0", t5, 0x7fffffff, 0xcbcdef01,
                   31, 0);
   TESTDSPINST_EXT("extr.w $t6, $ac1, 31", "ac1", t6, 0x3fffffff, 0x2bcdef01,
                   31, 0);
   TESTDSPINST_EXT("extr.w $t7, $ac2,  0", "ac2", t7, 0xffffffff, 0xffffffff,
                   0, 0);
   TESTDSPINST_EXT("extr.w $t8, $ac3,  1", "ac3", t8, 0x00000000, 0xfffffffe,
                   1, 0);
   TESTDSPINST_EXT("extr.w $t1, $ac0, 31", "ac0", t1, 0x80000000, 0x80000000,
                   31, 0);
   TESTDSPINST_EXT("extr.w $t2, $ac1, 17", "ac1", t2, (1<<31)+1, (1<<31)+2, 17,
                   0);
   TESTDSPINST_EXT("extr.w $t3, $ac2,  4", "ac2", t3, 0x00000000, 0x00000006,
                   4, 0);
   TESTDSPINST_EXT("extr.w $t4, $ac3, 12", "ac3", t4, 0x00000004, 1073741824,
                   12, 0);
   TESTDSPINST_EXT("extr.w $t5, $ac0,  3", "ac0", t5, 0x7fffffff, 0x7fffffff,
                   3, 0);
   TESTDSPINST_EXT("extr.w $t6, $ac1,  8", "ac1", t6, 0xffffffff, 0x00000001,
                   8, 0);
   TESTDSPINST_EXT("extr.w $t7, $ac2, 16", "ac2", t7, 0x00000001, 0xffffffff,
                   16, 0);
   TESTDSPINST_EXT("extr.w $t8, $ac3, 22", "ac3", t8, 0xffffffff, 0xffffffff,
                   22, 0);
   TESTDSPINST_EXT("extr.w $t0, $ac0,  9", "ac0", t0, 0x0000cdfe, 0xfd0e3126,
                   9, 0);
   TESTDSPINST_EXT("extr.w $t2, $ac2, 14", "ac2", t2, 0x00000018, 0xffff2435,
                   14, 0);
   TESTDSPINST_EXT("extr.w $t1, $ac1, 20", "ac1", t1, 0xaaaaaaaa, 0x55555555,
                   20, 0);
   TESTDSPINST_EXT("extr.w $t2, $ac2, 16", "ac2", t2, 0x00000018, 0xffff2435,
                   16, 0);
   TESTDSPINST_EXT("extr.w $t1, $ac1,  5", "ac1", t1, 0xaaaaaaaa, 0x55555555,
                   5, 0);
   TESTDSPINST_EXT("extr.w $t2, $ac2, 22", "ac2", t2, 0x00000018, 0xffff2435,
                   22, 0);
   TESTDSPINST_EXT("extr.w $t3, $ac3,  6", "ac3", t3, 0xbabababa, 0xabababab,
                   6, 0);
   TESTDSPINST_EXT("extr.w $t4, $ac0, 13", "ac0", t4, 0xf0f0f0f0, 0xfc79b4d2,
                   13, 0);
   TESTDSPINST_EXT("extr.w $t5, $ac1, 19", "ac1", t5, 0x00000000, 0x00000000,
                   19, 0);
   TESTDSPINST_EXT("extr.w $t6, $ac2, 27", "ac2", t6, 0xfbde3976, 0x00000000,
                   27, 0);
   TESTDSPINST_EXT("extr.w $t7, $ac3,  7", "ac3", t7, 0x0bed7654, 0x12349876,
                   7, 0);
   TESTDSPINST_EXT("extr.w $t8, $ac0, 11", "ac0", t8, 0x23534870, 0x00354565,
                   11, 0);
   TESTDSPINST_EXT("extr.w $t0, $ac1, 26", "ac1", t0, 0x980b7cde, 0x80006755,
                   26, 0);
   TESTDSPINST_EXT("extr.w $t1, $ac2, 15", "ac2", t1, 0x00000018, 0x8f8f8f8f,
                   15, 0);
   TESTDSPINST_EXT("extr.w $t2, $ac3,  2", "ac3", t2, 0x92784656, 0xeeeeeeee,
                   2, 0);
   TESTDSPINST_EXT("extr.w $t3, $ac0,  1", "ac0", t3, 0xcacacaca, 0x1bdbdbdb,
                   1, 0);
   TESTDSPINST_EXT("extr.w $t4, $ac1,  0", "ac1", t4, 0xbacabaca, 0xdecadeca,
                   0, 0);
   TESTDSPINST_EXT("extr.w $t5, $ac0,  3", "ac0", t5, 0x00000000, 0x5fc92974,
                   3, 0);
   TESTDSPINST_EXT("extr.w $t6, $ac1,  8", "ac1", t6, 0x00000000, 0x7e08184e,
                   8, 0);
   TESTDSPINST_EXT("extr.w $t7, $ac2, 16", "ac2", t7, 0x00000000, 0x71c8315f,
                   16, 0);
   TESTDSPINST_EXT("extr.w $t8, $ac3, 22", "ac3", t8, 0xffffffff, 0x9493110e,
                   22, 0);
   TESTDSPINST_EXT("extr.w $t0, $ac0,  9", "ac0", t0, 0xffffffff, 0xbb246228,
                   9, 0);
   TESTDSPINST_EXT("extr.w $t1, $ac1,  5", "ac1", t1, 0x00000000, 0x339d8d88,
                   5, 0);
   TESTDSPINST_EXT("extr.w $t2, $ac2, 14", "ac2", t2, 0x00000000, 0x70974249,
                   14, 0);
   TESTDSPINST_EXT("extr.w $t1, $ac1, 20", "ac1", t1, 0xffffffff, 0x8a8d4e7d,
                   20, 0);
   TESTDSPINST_EXT("extr.w $t2, $ac2, 16", "ac2", t2, 0xffffffff, 0xeb1b4335,
                   16, 0);
   TESTDSPINST_EXT("extr.w $t1, $ac1,  5", "ac1", t1, 0x00000000, 0x0cd6b508,
                   5, 0);
   TESTDSPINST_EXT("extr.w $t2, $ac2, 22", "ac2", t2, 0x00000000, 0x6731e282,
                   22, 0);
   TESTDSPINST_EXT("extr.w $t3, $ac3,  6", "ac3", t3, 0xffffffff, 0xb6edf28f,
                   6, 0);
   TESTDSPINST_EXT("extr.w $t4, $ac0, 13", "ac0", t4, 0x00000000, 0x4b4ec9ca,
                   13, 0);
   TESTDSPINST_EXT("extr.w $t5, $ac1, 19", "ac1", t5, 0xffffffff, 0xc1037fa4,
                   19, 0);
   TESTDSPINST_EXT("extr.w $t6, $ac2, 27", "ac2", t6, 0xffffffff, 0xcb4ab48f,
                   27, 0);
   TESTDSPINST_EXT("extr.w $t7, $ac3,  7", "ac3", t7, 0xffffffff, 0xaf8f7e18,
                   7, 0);
   TESTDSPINST_EXT("extr.w $t8, $ac0, 11", "ac0", t8, 0xffffffff, 0x87df4510,
                   11, 0);
   TESTDSPINST_EXT("extr.w $t0, $ac1, 26", "ac1", t0, 0xffffffff, 0xabf4e8e1,
                   26, 0);
   TESTDSPINST_EXT("extr.w $t1, $ac2, 15", "ac2", t1, 0xffffffff, 0xf4c0eeac,
                   15, 0);
   TESTDSPINST_EXT("extr.w $t2, $ac3,  2", "ac3", t2, 0x00000000, 0x006a54f2,
                   2, 0);
   TESTDSPINST_EXT("extr.w $t3, $ac0,  1", "ac0", t3, 0x00000000, 0x79f74493,
                   1, 0);
   TESTDSPINST_EXT("extr.w $t4, $ac1,  0", "ac1", t4, 0xffffffff, 0x9c09e313,
                   0, 0);

   printf("-------- EXTR_R.W --------\n");
   TESTDSPINST_EXT("extr_r.w $t4, $ac3,  0", "ac3", t4, 0x00000000, 0x00000000,
                   0, 0);
   TESTDSPINST_EXT("extr_r.w $t5, $ac0, 31", "ac0", t5, 0x7fffffff, 0xcbcdef01,
                   31, 0);
   TESTDSPINST_EXT("extr_r.w $t6, $ac1, 31", "ac1", t6, 0x3fffffff, 0x2bcdef01,
                   31, 0);
   TESTDSPINST_EXT("extr_r.w $t7, $ac2,  0", "ac2", t7, 0x987b2fff, 0xffffffff,
                   0, 0);
   TESTDSPINST_EXT("extr_r.w $t8, $ac3,  1", "ac3", t8, 0x000cd320, 0xfffffffe,
                   1, 0);
   TESTDSPINST_EXT("extr_r.w $t1, $ac0, 31", "ac0", t1, 0xfff9b541, 0x80000000,
                   31, 0);
   TESTDSPINST_EXT("extr_r.w $t3, $ac2,  4", "ac2", t3, 0x0008b31c, 0x00000006,
                   4, 0);
   TESTDSPINST_EXT("extr_r.w $t5, $ac0,  3", "ac0", t5, 0x0086b3ad, 0x7fffffff,
                   3, 0);
   TESTDSPINST_EXT("extr_r.w $t6, $ac1,  8", "ac1", t6, 0x00097b51, 0x00000001,
                   8, 0);
   TESTDSPINST_EXT("extr_r.w $t7, $ac2, 16", "ac2", t7, 0xfcde43ff, 0xffffffff,
                   16, 0);
   TESTDSPINST_EXT("extr_r.w $t0, $ac0,  9", "ac0", t0, 0xffffca26, 0xfd0e3126,
                   9, 0);
   TESTDSPINST_EXT("extr_r.w $t1, $ac1,  5", "ac1", t1, 0x00000000, 0x55555555,
                   5, 0);
   TESTDSPINST_EXT("extr_r.w $t2, $ac2, 14", "ac2", t2, 0xffffffff, 0xffff2435,
                   14, 0);
   TESTDSPINST_EXT("extr_r.w $t1, $ac1, 20", "ac1", t1, 0x00000000, 0x55555555,
                   20, 0);
   TESTDSPINST_EXT("extr_r.w $t2, $ac2, 16", "ac2", t2, 0xffffffff, 0xffff2435,
                   16, 0);
   TESTDSPINST_EXT("extr_r.w $t1, $ac1,  5", "ac1", t1, 0x00000000, 0x55555555,
                   5, 0);
   TESTDSPINST_EXT("extr_r.w $t2, $ac2, 22", "ac2", t2, 0xffffffff, 0xffff2435,
                   22, 0);
   TESTDSPINST_EXT("extr_r.w $t3, $ac3,  6", "ac3", t3, 0xffffffff, 0xabababab,
                   6, 0);
   TESTDSPINST_EXT("extr_r.w $t4, $ac0, 13", "ac0", t4, 0xffffffff, 0xfc79b4d2,
                   13, 0);
   TESTDSPINST_EXT("extr_r.w $t5, $ac1, 19", "ac1", t5, 0x00000000, 0x00000000,
                   19, 0);
   TESTDSPINST_EXT("extr_r.w $t6, $ac2, 27", "ac2", t6, 0x00000000, 0x00000000,
                   27, 0);
   TESTDSPINST_EXT("extr_r.w $t7, $ac3,  7", "ac3", t7, 0x02934b00, 0x12349876,
                   7, 0);
   TESTDSPINST_EXT("extr_r.w $t8, $ac0, 11", "ac0", t8, 0x0008cad0, 0x00354565,
                   11, 0);
   TESTDSPINST_EXT("extr_r.w $t0, $ac1, 26", "ac1", t0, 0xf65c8fff, 0x80006755,
                   26, 0);
   TESTDSPINST_EXT("extr_r.w $t1, $ac2, 15", "ac2", t1, 0xfffff001, 0x8f8f8f8f,
                   15, 0);
   TESTDSPINST_EXT("extr_r.w $t2, $ac3,  2", "ac3", t2, 0xbad69420, 0xeeeeeeee,
                   2, 0);
   TESTDSPINST_EXT("extr_r.w $t3, $ac0,  1", "ac0", t3, 0x00000000, 0x1bdbdbdb,
                   1, 0);
   TESTDSPINST_EXT("extr_r.w $t4, $ac1,  0", "ac1", t4, 0xffffffff, 0xdecadeca,
                   0, 0);
   TESTDSPINST_EXT("extr_r.w $t5, $ac0,  3", "ac0", t5, 0x00000000, 0x5fc92974,
                   3, 0);
   TESTDSPINST_EXT("extr_r.w $t6, $ac1,  8", "ac1", t6, 0x00000000, 0x7e08184e,
                   8, 0);
   TESTDSPINST_EXT("extr_r.w $t7, $ac2, 16", "ac2", t7, 0x00000000, 0x71c8315f,
                   16, 0);
   TESTDSPINST_EXT("extr_r.w $t8, $ac3, 22", "ac3", t8, 0xffffffff, 0x9493110e,
                   22, 0);
   TESTDSPINST_EXT("extr_r.w $t0, $ac0,  9", "ac0", t0, 0xffffffff, 0xbb246228,
                   9, 0);
   TESTDSPINST_EXT("extr_r.w $t1, $ac1,  5", "ac1", t1, 0x00000000, 0x339d8d88,
                   5, 0);
   TESTDSPINST_EXT("extr_r.w $t2, $ac2, 14", "ac2", t2, 0x00000000, 0x70974249,
                   14, 0);
   TESTDSPINST_EXT("extr_r.w $t1, $ac1, 20", "ac1", t1, 0xffffffff, 0x8a8d4e7d,
                   20, 0);
   TESTDSPINST_EXT("extr_r.w $t2, $ac2, 16", "ac2", t2, 0xffffffff, 0xeb1b4335,
                   16, 0);
   TESTDSPINST_EXT("extr_r.w $t1, $ac1,  5", "ac1", t1, 0x00000000, 0x0cd6b508,
                   5, 0);
   TESTDSPINST_EXT("extr_r.w $t2, $ac2, 22", "ac2", t2, 0x00000000, 0x6731e282,
                   22, 0);
   TESTDSPINST_EXT("extr_r.w $t3, $ac3,  6", "ac3", t3, 0xffffffff, 0xb6edf28f,
                   6, 0);
   TESTDSPINST_EXT("extr_r.w $t4, $ac0, 13", "ac0", t4, 0x00000000, 0x4b4ec9ca,
                   13, 0);
   TESTDSPINST_EXT("extr_r.w $t5, $ac1, 19", "ac1", t5, 0xffffffff, 0xc1037fa4,
                   19, 0);
   TESTDSPINST_EXT("extr_r.w $t6, $ac2, 27", "ac2", t6, 0xffffffff, 0xcb4ab48f,
                   27, 0);
   TESTDSPINST_EXT("extr_r.w $t7, $ac3,  7", "ac3", t7, 0xffffffff, 0xaf8f7e18,
                   7, 0);
   TESTDSPINST_EXT("extr_r.w $t8, $ac0, 11", "ac0", t8, 0xffffffff, 0x87df4510,
                   11, 0);
   TESTDSPINST_EXT("extr_r.w $t0, $ac1, 26", "ac1", t0, 0xffffffff, 0xabf4e8e1,
                   26, 0);
   TESTDSPINST_EXT("extr_r.w $t1, $ac2, 15", "ac2", t1, 0xffffffff, 0xf4c0eeac,
                   15, 0);
   TESTDSPINST_EXT("extr_r.w $t2, $ac3,  2", "ac3", t2, 0x00000000, 0x006a54f2,
                   2, 0);
   TESTDSPINST_EXT("extr_r.w $t3, $ac0,  1", "ac0", t3, 0x00000000, 0x79f74493,
                   1, 0);
   TESTDSPINST_EXT("extr_r.w $t4, $ac1,  0", "ac1", t4, 0xffffffff, 0x9c09e313,
                   0, 0);

   printf("-------- EXTR_RS.W --------\n");
   TESTDSPINST_EXT("extr_rs.w $t4, $ac3,  0", "ac3", t4, 0x00000000, 0x00000000,
                   0, 0);                                
   TESTDSPINST_EXT("extr_rs.w $t5, $ac0, 31", "ac0", t5, 0x7fffffff, 0xcbcdef01,
                   31, 0);                               
   TESTDSPINST_EXT("extr_rs.w $t6, $ac1, 31", "ac1", t6, 0x3fffffff, 0x2bcdef01,
                   31, 0);                               
   TESTDSPINST_EXT("extr_rs.w $t7, $ac2,  0", "ac2", t7, 0x987b2fff, 0xffffffff,
                   0, 0);                                
   TESTDSPINST_EXT("extr_rs.w $t8, $ac3,  1", "ac3", t8, 0x000cd320, 0xfffffffe,
                   1, 0);                                
   TESTDSPINST_EXT("extr_rs.w $t3, $ac2,  4", "ac2", t3, 0xfff9b541, 0x80000000,
                   4, 0);                                
   TESTDSPINST_EXT("extr_rs.w $t5, $ac0,  3", "ac0", t5, 0x0008b31c, 0x00000006,
                   3, 0);                                
   TESTDSPINST_EXT("extr_rs.w $t6, $ac1,  8", "ac1", t6, 0x0086b3ad, 0x7fffffff,
                   8, 0);                                
   TESTDSPINST_EXT("extr_rs.w $t7, $ac2, 16", "ac2", t7, 0x00097b51, 0x00000001,
                   16, 0);                               
   TESTDSPINST_EXT("extr_rs.w $t8, $ac3, 22", "ac3", t8, 0xfcde43ff, 0xffffffff, 
                   2, 0);                                
   TESTDSPINST_EXT("extr_rs.w $t0, $ac0,  9", "ac0", t0, 0xffffca26, 0xfd0e3126,
                   9, 0);                                
   TESTDSPINST_EXT("extr_rs.w $t1, $ac1,  5", "ac1", t1, 0x00000000, 0x55555555,
                   5, 0);                                
   TESTDSPINST_EXT("extr_rs.w $t2, $ac2, 14", "ac2", t2, 0xffffffff, 0xffff2435,
                   14, 0);                               
   TESTDSPINST_EXT("extr_rs.w $t1, $ac1, 20", "ac1", t1, 0x00000000, 0x55555555, 
                   0, 0);                                
   TESTDSPINST_EXT("extr_rs.w $t2, $ac2, 16", "ac2", t2, 0xffffffff, 0xffff2435,
                   16, 0);                               
   TESTDSPINST_EXT("extr_rs.w $t1, $ac1,  5", "ac1", t1, 0x00000000, 0x55555555,
                   5, 0);                                
   TESTDSPINST_EXT("extr_rs.w $t2, $ac2, 22", "ac2", t2, 0xffffffff, 0xffff2435,
                   22, 0);                               
   TESTDSPINST_EXT("extr_rs.w $t3, $ac3,  6", "ac3", t3, 0xffffffff, 0xabababab,
                   6, 0);                                
   TESTDSPINST_EXT("extr_rs.w $t4, $ac0, 13", "ac0", t4, 0xffffffff, 0xfc79b4d2,
                   13, 0);                               
   TESTDSPINST_EXT("extr_rs.w $t5, $ac1, 19", "ac1", t5, 0x00000000, 0x00000000,
                   19, 0);                               
   TESTDSPINST_EXT("extr_rs.w $t6, $ac2, 27", "ac2", t6, 0x00000000, 0x00000000,
                   27, 0);                               
   TESTDSPINST_EXT("extr_rs.w $t7, $ac3,  7", "ac3", t7, 0x02934b00, 0x12349876,
                   7, 0);                                
   TESTDSPINST_EXT("extr_rs.w $t8, $ac0, 11", "ac0", t8, 0x0008cad0, 0x00354565,
                   11, 0);                               
   TESTDSPINST_EXT("extr_rs.w $t0, $ac1, 26", "ac1", t0, 0xf65c8fff, 0x80006755,
                   26, 0);                               
   TESTDSPINST_EXT("extr_rs.w $t1, $ac2, 15", "ac2", t1, 0xfffff001, 0x8f8f8f8f,
                   15, 0);                               
   TESTDSPINST_EXT("extr_rs.w $t2, $ac3,  2", "ac3", t2, 0xbad69420, 0xeeeeeeee,
                   2, 0);                                
   TESTDSPINST_EXT("extr_rs.w $t3, $ac0,  1", "ac0", t3, 0x00000000, 0x1bdbdbdb,
                   1, 0);                                
   TESTDSPINST_EXT("extr_rs.w $t4, $ac1,  0", "ac1", t4, 0xffffffff, 0xdecadeca,
                   0, 0);                                
   TESTDSPINST_EXT("extr_rs.w $t5, $ac0,  3", "ac0", t5, 0x00000000, 0x5fc92974,
                   3, 0);                                
   TESTDSPINST_EXT("extr_rs.w $t6, $ac1,  8", "ac1", t6, 0x00000000, 0x7e08184e,
                   8, 0);                                
   TESTDSPINST_EXT("extr_rs.w $t7, $ac2, 16", "ac2", t7, 0x00000000, 0x71c8315f,
                   16, 0);                               
   TESTDSPINST_EXT("extr_rs.w $t8, $ac3, 22", "ac3", t8, 0xffffffff, 0x9493110e,
                   22, 0);                               
   TESTDSPINST_EXT("extr_rs.w $t0, $ac0,  9", "ac0", t0, 0xffffffff, 0xbb246228,
                   9, 0);                                
   TESTDSPINST_EXT("extr_rs.w $t1, $ac1,  5", "ac1", t1, 0x00000000, 0x339d8d88,
                   5, 0);                                
   TESTDSPINST_EXT("extr_rs.w $t2, $ac2, 14", "ac2", t2, 0x00000000, 0x70974249,
                   14, 0);                               
   TESTDSPINST_EXT("extr_rs.w $t1, $ac1, 20", "ac1", t1, 0xffffffff, 0x8a8d4e7d,
                   20, 0);                               
   TESTDSPINST_EXT("extr_rs.w $t2, $ac2, 16", "ac2", t2, 0xffffffff, 0xeb1b4335,
                   16, 0);                               
   TESTDSPINST_EXT("extr_rs.w $t1, $ac1,  5", "ac1", t1, 0x00000000, 0x0cd6b508,
                   5, 0);                                
   TESTDSPINST_EXT("extr_rs.w $t2, $ac2, 22", "ac2", t2, 0x00000000, 0x6731e282,
                   22, 0);                               
   TESTDSPINST_EXT("extr_rs.w $t3, $ac3,  6", "ac3", t3, 0xffffffff, 0xb6edf28f,
                   6, 0);                                
   TESTDSPINST_EXT("extr_rs.w $t4, $ac0, 13", "ac0", t4, 0x00000000, 0x4b4ec9ca,
                   13, 0);                               
   TESTDSPINST_EXT("extr_rs.w $t5, $ac1, 19", "ac1", t5, 0xffffffff, 0xc1037fa4,
                   19, 0);                               
   TESTDSPINST_EXT("extr_rs.w $t6, $ac2, 27", "ac2", t6, 0xffffffff, 0xcb4ab48f,
                   27, 0);                               
   TESTDSPINST_EXT("extr_rs.w $t7, $ac3,  7", "ac3", t7, 0xffffffff, 0xaf8f7e18,
                   7, 0);                                
   TESTDSPINST_EXT("extr_rs.w $t8, $ac0, 11", "ac0", t8, 0xffffffff, 0x87df4510,
                   11, 0);                               
   TESTDSPINST_EXT("extr_rs.w $t0, $ac1, 26", "ac1", t0, 0xffffffff, 0xabf4e8e1,
                   26, 0);                               
   TESTDSPINST_EXT("extr_rs.w $t1, $ac2, 15", "ac2", t1, 0xffffffff, 0xf4c0eeac,
                   15, 0);                               
   TESTDSPINST_EXT("extr_rs.w $t2, $ac3,  2", "ac3", t2, 0x00000000, 0x006a54f2,
                   2, 0);                                
   TESTDSPINST_EXT("extr_rs.w $t3, $ac0,  1", "ac0", t3, 0x00000000, 0x79f74493,
                   1, 0);                                
   TESTDSPINST_EXT("extr_rs.w $t4, $ac1,  0", "ac1", t4, 0xffffffff, 0x9c09e313,
                   0, 0);

   printf("-------- EXTRV_S.H --------\n");
   TESTDSPINST_EXTV("extrv_s.h $t1, $ac1, $t3", "ac1", t1, 0x00000000,
                    0x00000000, t3, 0xbababa00, 0);
   TESTDSPINST_EXTV("extrv_s.h $t2, $ac2, $t4", "ac2", t2, 0x7fffffff,
                    0xcbcdef01, t4, 0xfbde391f, 0);
   TESTDSPINST_EXTV("extrv_s.h $t1, $ac1, $t7", "ac1", t1, 0x3fffffff,
                    0x2bcdef01, t7, 0x5555551f, 0);
   TESTDSPINST_EXTV("extrv_s.h $t2, $ac2, $t5", "ac2", t2, 0xffffffff,
                    0xffffffff, t5, 0x0000cd00, 0);
   TESTDSPINST_EXTV("extrv_s.h $t1, $ac1, $t2", "ac1", t1, 0x00000000,
                    0xfffffffe, t2, 0x80000001, 0);
   TESTDSPINST_EXTV("extrv_s.h $t2, $ac1, $t1", "ac1", t2, (1<<31)+1, (1<<31)+2,
                    t1, 0x12349876, 0);
   TESTDSPINST_EXTV("extrv_s.h $t1, $ac0, $t0", "ac0", t1, 0x80000000,
                    0x80000000, t0, 0x12349876, 0);
   TESTDSPINST_EXTV("extrv_s.h $t3, $ac2, $t2", "ac2", t3, 0x00000000,
                    0x00000006, t2, 0x00354565, 0);
   TESTDSPINST_EXTV("extrv_s.h $t4, $ac3, $t3", "ac3", t4, 0x00000004,
                    1073741824, t3, 0x00086755, 0);
   TESTDSPINST_EXTV("extrv_s.h $t5, $ac0, $t4", "ac0", t5, 0x7fffffff,
                    0x7fffffff, t4, 0x00000018, 0);
   TESTDSPINST_EXTV("extrv_s.h $t6, $ac1, $t5", "ac1", t6, 0xffffffff,
                    0x00000001, t5, 0x23534870, 0);
   TESTDSPINST_EXTV("extrv_s.h $t7, $ac2, $t6", "ac2", t7, 0x00000001,
                    0xffffffff, t6, 0x92784656, 0);
   TESTDSPINST_EXTV("extrv_s.h $t8, $ac3, $t7", "ac3", t8, 0xffffffff,
                    0xffffffff, t7, 0xeeeeeeee, 0);
   TESTDSPINST_EXTV("extrv_s.h $t0, $ac0, $t8", "ac0", t0, 0x0000cdfe,
                    0x0fde3126, t8, 0xcacacaca, 0);
   TESTDSPINST_EXTV("extrv_s.h $t1, $ac1, $t0", "ac1", t1, 0xaaaaaaaa,
                    0x55555555, t0, 0xbacabaca, 0);
   TESTDSPINST_EXTV("extrv_s.h $t2, $ac2, $t1", "ac2", t2, 0x00000018,
                    0xffff2435, t1, 0x1bdbdbdb, 0);
   TESTDSPINST_EXTV("extrv_s.h $t1, $ac1, $t2", "ac1", t1, 0xaaaaaaaa,
                    0x55555555, t2, 0x0cd6b508, 0);
   TESTDSPINST_EXTV("extrv_s.h $t2, $ac2, $t3", "ac2", t2, 0x00000018,
                    0xffff2435, t3, 0x6731e282, 0);
   TESTDSPINST_EXTV("extrv_s.h $t1, $ac1, $t4", "ac1", t1, 0xaaaaaaaa,
                    0x55555555, t4, 0xb6edf28f, 0);
   TESTDSPINST_EXTV("extrv_s.h $t2, $ac2, $t5", "ac2", t2, 0x00000018,
                    0xffff2435, t5, 0x4b4ec9ca, 0);
   TESTDSPINST_EXTV("extrv_s.h $t3, $ac3, $t6", "ac3", t3, 0xbabababa,
                    0xabababab, t6, 0xc1037fa4, 0);
   TESTDSPINST_EXTV("extrv_s.h $t4, $ac0, $t7", "ac0", t4, 0xf0f0f0f0,
                    0xfc79b4d2, t7, 0xcb4ab48f, 0);
   TESTDSPINST_EXTV("extrv_s.h $t5, $ac1, $t8", "ac1", t5, 0x00000000,
                    0x00000000, t8, 0xaf8f7e18, 0);
   TESTDSPINST_EXTV("extrv_s.h $t6, $ac2, $t0", "ac2", t6, 0xfbde3976,
                    0x00000000, t0, 0x87df4510, 0);
   TESTDSPINST_EXTV("extrv_s.h $t7, $ac3, $t1", "ac3", t7, 0x0bed7654,
                    0x12349876, t1, 0xabf4e8e1, 0);
   TESTDSPINST_EXTV("extrv_s.h $t8, $ac0, $t2", "ac0", t8, 0x23534870,
                    0x00354565, t2, 0xf4c0eeac, 0);
   TESTDSPINST_EXTV("extrv_s.h $t0, $ac1, $t3", "ac1", t0, 0x980b7cde,
                    0x00086755, t3, 0x006a54f2, 0);
   TESTDSPINST_EXTV("extrv_s.h $t1, $ac2, $t4", "ac2", t1, 0x00000018,
                    0x8f8f8f8f, t4, 0x79f74493, 0);
   TESTDSPINST_EXTV("extrv_s.h $t2, $ac3, $t5", "ac3", t2, 0x92784656,
                    0xeeeeeeee, t5, 0x9c09e313, 0);
   TESTDSPINST_EXTV("extrv_s.h $t3, $ac0, $t6", "ac0", t3, 0xcacacaca,
                    0x1bdbdbdb, t6, 0x0fde3126, 0);
   TESTDSPINST_EXTV("extrv_s.h $t4, $ac1, $t7", "ac1", t4, 0xbacabaca,
                    0xdecadeca, t7, 0x55555555, 0);
   TESTDSPINST_EXTV("extrv_s.h $t5, $ac0, $t8", "ac0", t5, 0x00000000,
                    0x5fc92974, t8, 0xffff2435, 0);
   TESTDSPINST_EXTV("extrv_s.h $t6, $ac1, $t0", "ac1", t6, 0x00000000,
                    0x7e08184e, t0, 0x55555555, 0);
   TESTDSPINST_EXTV("extrv_s.h $t7, $ac2, $t1", "ac2", t7, 0x00000000,
                    0x71c8315f, t1, 0xffff2435, 0);
   TESTDSPINST_EXTV("extrv_s.h $t8, $ac3, $t2", "ac3", t8, 0xffffffff,
                    0x9493110e, t2, 0x55555555, 0);
   TESTDSPINST_EXTV("extrv_s.h $t0, $ac0, $t3", "ac0", t0, 0xffffffff,
                    0xbb246228, t3, 0xffff2435, 0);
   TESTDSPINST_EXTV("extrv_s.h $t1, $ac1, $t4", "ac1", t1, 0x00000000,
                    0x339d8d88, t4, 0xabababab, 0);
   TESTDSPINST_EXTV("extrv_s.h $t2, $ac2, $t5", "ac2", t2, 0x00000000,
                    0x70974249, t5, 0xfc79b4d2, 0);
   TESTDSPINST_EXTV("extrv_s.h $t1, $ac1, $t6", "ac1", t1, 0xffffffff,
                    0x8a8d4e7d, t6, 0x00000000, 0);
   TESTDSPINST_EXTV("extrv_s.h $t2, $ac2, $t7", "ac2", t2, 0xffffffff,
                    0xeb1b4335, t7, 0x00000000, 0);
   TESTDSPINST_EXTV("extrv_s.h $t1, $ac1, $t8", "ac1", t1, 0x00000000,
                    0x0cd6b508, t8, 0x12349876, 0);
   TESTDSPINST_EXTV("extrv_s.h $t2, $ac2, $t0", "ac2", t2, 0x00000000,
                    0x6731e282, t0, 0x00354565, 0);
   TESTDSPINST_EXTV("extrv_s.h $t3, $ac3, $t1", "ac3", t3, 0xffffffff,
                    0xb6edf28f, t1, 0x00086755, 0);
   TESTDSPINST_EXTV("extrv_s.h $t4, $ac0, $t2", "ac0", t4, 0x00000000,
                    0x4b4ec9ca, t2, 0x8f8f8f8f, 0);
   TESTDSPINST_EXTV("extrv_s.h $t5, $ac1, $t3", "ac1", t5, 0xffffffff,
                    0xc1037fa4, t3, 0xeeeeeeee, 0);
   TESTDSPINST_EXTV("extrv_s.h $t6, $ac2, $t3", "ac2", t6, 0xffffffff,
                    0xcb4ab48f, t3, 0x1bdbdbdb, 0);
   TESTDSPINST_EXTV("extrv_s.h $t7, $ac3, $t4", "ac3", t7, 0xffffffff,
                    0xaf8f7e18, t4, 0xbb246228, 0);
   TESTDSPINST_EXTV("extrv_s.h $t8, $ac0, $t5", "ac0", t8, 0xffffffff,
                    0x87df4510, t5, 0x339d8d88, 0);
   TESTDSPINST_EXTV("extrv_s.h $t0, $ac1, $t6", "ac1", t0, 0xffffffff,
                    0xabf4e8e1, t6, 0x70974249, 0);
   TESTDSPINST_EXTV("extrv_s.h $t1, $ac2, $t7", "ac2", t1, 0xffffffff,
                    0xf4c0eeac, t7, 0x8a8d4e7d, 0);
   TESTDSPINST_EXTV("extrv_s.h $t2, $ac3, $t8", "ac3", t2, 0x00000000,
                    0x006a54f2, t8, 0xeb1b4335, 0);
   TESTDSPINST_EXTV("extrv_s.h $t3, $ac0, $t0", "ac0", t3, 0x00000000,
                    0x79f74493, t0, 0x0cd6b508, 0);
   TESTDSPINST_EXTV("extrv_s.h $t4, $ac1, $t1", "ac1", t4, 0xffffffff,
                    0x9c09e313, t1, 0x6731e282, 0);

   printf("-------- EXTRV.W --------\n");
   TESTDSPINST_EXTV("extrv.w $t1, $ac1, $t3", "ac1", t1, 0x00000000, 0x00000000,
                    t3, 0xbababa00, 0);
   TESTDSPINST_EXTV("extrv.w $t2, $ac2, $t4", "ac2", t2, 0x7fffffff, 0xcbcdef01,
                    t4, 0xfbde391f, 0);
   TESTDSPINST_EXTV("extrv.w $t1, $ac1, $t7", "ac1", t1, 0x3fffffff, 0x2bcdef01,
                    t7, 0x5555551f, 0);
   TESTDSPINST_EXTV("extrv.w $t2, $ac2, $t5", "ac2", t2, 0xffffffff, 0xffffffff,
                    t5, 0x0000cd00, 0);
   TESTDSPINST_EXTV("extrv.w $t1, $ac1, $t2", "ac1", t1, 0x00000000, 0xfffffffe,
                    t2, 0x80000001, 0);
   TESTDSPINST_EXTV("extrv.w $t1, $ac1, $t3", "ac1", t1, 0xaaabad3a, 0x55555555,
                    t3, 0xbababa05, 0);
   TESTDSPINST_EXTV("extrv.w $t2, $ac2, $t4", "ac2", t2, 0x00000018, 0xffff2435,
                    t4, 0xfbde390e, 0);
   TESTDSPINST_EXTV("extrv.w $t1, $ac1, $t7", "ac1", t1, 0xaaaaa221, 0x55555555,
                    t7, 0x55555514, 0);
   TESTDSPINST_EXTV("extrv.w $t2, $ac2, $t5", "ac2", t2, 0x00000018, 0xffff2435,
                    t5, 0x0000cd10, 0);
   TESTDSPINST_EXTV("extrv.w $t1, $ac1, $t2", "ac1", t1, 0xaaaaaaaa, 0x55555555,
                    t2, 0x80000005, 0);
   TESTDSPINST_EXTV("extrv.w $t2, $ac2, $t3", "ac2", t2, 0x00000018, 0xffff2435,
                    t3, 0x7fffff16, 0);
   TESTDSPINST_EXTV("extrv.w $t0, $ac0, $t1", "ac0", t0, 0x0000cdfe, 0x0fde3126,
                    t1, 0xbabababa, 0);
   TESTDSPINST_EXTV("extrv.w $t3, $ac3, $t2", "ac3", t3, 0xbabababa, 0xabababab,
                    t2, 0xfbde3976, 0);
   TESTDSPINST_EXTV("extrv.w $t6, $ac1, $t7", "ac1", t6, 0xffffffff, 0x00000001,
                    t7, 0x55555555, 0);
   TESTDSPINST_EXTV("extrv.w $t4, $ac3, $t5", "ac3", t4, 0x00000004, 1073741824,
                    t5, 0x0000cdfe, 0);
   TESTDSPINST_EXTV("extrv.w $t1, $ac0, $t2", "ac0", t1, 0x80000000, 0x80000000,
                    t2, 0x80000000, 0);
   TESTDSPINST_EXTV("extrv.w $t2, $ac1, $t3", "ac1", t2, (1<<31)+1, (1<<31)+2,
                    t3, 0x06, 0);
   TESTDSPINST_EXTV("extrv.w $t3, $ac2, $t4", "ac2", t3, 0x00000000, 0x00000006,
                    t4, 0x7fffffff, 0);
   TESTDSPINST_EXTV("extrv.w $t5, $ac0, $t6", "ac0", t5, 0x7fffffff, 0x7fffffff,
                    t6, 0x0fde3126, 0);
   TESTDSPINST_EXTV("extrv.w $t7, $ac2, $t8", "ac2", t7, 0x00000001, 0xffffffff,
                    t8, 0xaaaaaaaa, 0);
   TESTDSPINST_EXTV("extrv.w $t8, $ac3, $t9", "ac3", t8, 0xffffffff, 0xffffffff,
                    t9, 0xffff2435, 0);
   TESTDSPINST_EXTV("extrv.w $t4, $ac0, $t3", "ac0", t4, 0xf0f0f0f0, 0xfc79b4d2,
                    t3, 0x12349876, 0);
   TESTDSPINST_EXTV("extrv.w $t5, $ac1, $t4", "ac1", t5, 0x00000000, 0x00000000,
                    t4, 0x00354565, 0);
   TESTDSPINST_EXTV("extrv.w $t6, $ac2, $t5", "ac2", t6, 0xfbde3976, 0x00000000,
                    t5, 0x00086755, 0);
   TESTDSPINST_EXTV("extrv.w $t7, $ac3, $t6", "ac3", t7, 0x0bed7654, 0x12349876,
                    t6, 0x00000018, 0);
   TESTDSPINST_EXTV("extrv.w $t8, $ac0, $t7", "ac0", t8, 0x23534870, 0x00354565,
                    t7, 0x23534870, 0);
   TESTDSPINST_EXTV("extrv.w $t0, $ac1, $t8", "ac1", t0, 0x980b7cde, 0x00086755,
                    t8, 0x92784656, 0);
   TESTDSPINST_EXTV("extrv.w $t1, $ac2, $t9", "ac2", t1, 0x00000018, 0x8f8f8f8f,
                    t9, 0xeeeeeeee, 0);
   TESTDSPINST_EXTV("extrv.w $t2, $ac3, $t1", "ac3", t2, 0x92784656, 0xeeeeeeee,
                    t1, 0xcacacaca, 0);
   TESTDSPINST_EXTV("extrv.w $t3, $ac0, $t1", "ac0", t3, 0xcacacaca, 0x1bdbdbdb,
                    t1, 0xbacabaca, 0);
   TESTDSPINST_EXTV("extrv.w $t4, $ac1, $t4", "ac1", t4, 0xbacabaca, 0xdecadeca,
                    t4, 0x1bdbdbdb, 0);
   TESTDSPINST_EXTV("extrv.w $t5, $ac0, $t8", "ac0", t5, 0x00000000, 0x5fc92974,
                    t8, 0xffff2435, 0);
   TESTDSPINST_EXTV("extrv.w $t6, $ac1, $t0", "ac1", t6, 0x00000000, 0x7e08184e,
                    t0, 0x55555555, 0);
   TESTDSPINST_EXTV("extrv.w $t7, $ac2, $t1", "ac2", t7, 0x00000000, 0x71c8315f,
                    t1, 0xffff2435, 0);
   TESTDSPINST_EXTV("extrv.w $t8, $ac3, $t2", "ac3", t8, 0xffffffff, 0x9493110e,
                    t2, 0x55555555, 0);
   TESTDSPINST_EXTV("extrv.w $t0, $ac0, $t3", "ac0", t0, 0xffffffff, 0xbb246228,
                    t3, 0xffff2435, 0);
   TESTDSPINST_EXTV("extrv.w $t1, $ac1, $t4", "ac1", t1, 0x00000000, 0x339d8d88,
                    t4, 0xabababab, 0);
   TESTDSPINST_EXTV("extrv.w $t2, $ac2, $t5", "ac2", t2, 0x00000000, 0x70974249,
                    t5, 0xfc79b4d2, 0);
   TESTDSPINST_EXTV("extrv.w $t1, $ac1, $t6", "ac1", t1, 0xffffffff, 0x8a8d4e7d,
                    t6, 0x00000000, 0);
   TESTDSPINST_EXTV("extrv.w $t2, $ac2, $t7", "ac2", t2, 0xffffffff, 0xeb1b4335,
                    t7, 0x00000000, 0);
   TESTDSPINST_EXTV("extrv.w $t1, $ac1, $t8", "ac1", t1, 0x00000000, 0x0cd6b508,
                    t8, 0x12349876, 0);
   TESTDSPINST_EXTV("extrv.w $t2, $ac2, $t0", "ac2", t2, 0x00000000, 0x6731e282,
                    t0, 0x00354565, 0);
   TESTDSPINST_EXTV("extrv.w $t3, $ac3, $t1", "ac3", t3, 0xffffffff, 0xb6edf28f,
                    t1, 0x00086755, 0);
   TESTDSPINST_EXTV("extrv.w $t4, $ac0, $t2", "ac0", t4, 0x00000000, 0x4b4ec9ca,
                    t2, 0x8f8f8f8f, 0);
   TESTDSPINST_EXTV("extrv.w $t5, $ac1, $t3", "ac1", t5, 0xffffffff, 0xc1037fa4,
                    t3, 0xeeeeeeee, 0);
   TESTDSPINST_EXTV("extrv.w $t6, $ac2, $t3", "ac2", t6, 0xffffffff, 0xcb4ab48f,
                    t3, 0x1bdbdbdb, 0);
   TESTDSPINST_EXTV("extrv.w $t7, $ac3, $t4", "ac3", t7, 0xffffffff, 0xaf8f7e18,
                    t4, 0xbb246228, 0);
   TESTDSPINST_EXTV("extrv.w $t8, $ac0, $t5", "ac0", t8, 0xffffffff, 0x87df4510,
                    t5, 0x339d8d88, 0);
   TESTDSPINST_EXTV("extrv.w $t0, $ac1, $t6", "ac1", t0, 0xffffffff, 0xabf4e8e1,
                    t6, 0x70974249, 0);
   TESTDSPINST_EXTV("extrv.w $t1, $ac2, $t7", "ac2", t1, 0xffffffff, 0xf4c0eeac,
                    t7, 0x8a8d4e7d, 0);
   TESTDSPINST_EXTV("extrv.w $t2, $ac3, $t8", "ac3", t2, 0x00000000, 0x006a54f2,
                    t8, 0xeb1b4335, 0);
   TESTDSPINST_EXTV("extrv.w $t3, $ac0, $t0", "ac0", t3, 0x00000000, 0x79f74493,
                    t0, 0x0cd6b508, 0);
   TESTDSPINST_EXTV("extrv.w $t4, $ac1, $t1", "ac1", t4, 0xffffffff, 0x9c09e313,
                    t1, 0x6731e282, 0);

   printf("-------- EXTRV_R.W --------\n");
   TESTDSPINST_EXTV("extrv_r.w $t1, $ac1, $t3", "ac1", t1, 0x00000000,
                    0x00000000, t3, 0xbababa00, 0);
   TESTDSPINST_EXTV("extrv_r.w $t2, $ac2, $t4", "ac2", t2, 0x7fffffff,
                    0xcbcdef01, t4, 0xfbde391f, 0);
   TESTDSPINST_EXTV("extrv_r.w $t1, $ac1, $t7", "ac1", t1, 0x3fffffff,
                    0x2bcdef01, t7, 0x5555551f, 0);
   TESTDSPINST_EXTV("extrv_r.w $t2, $ac2, $t5", "ac2", t2, 0xffffffff,
                    0xffffffff, t5, 0x0000cd00, 0);
   TESTDSPINST_EXTV("extrv_r.w $t1, $ac1, $t2", "ac1", t1, 0x00000000,
                    0xfffffffe, t2, 0x80000001, 0);
   TESTDSPINST_EXTV("extrv_r.w $t1, $ac1, $t3", "ac1", t1, 0x00000000,
                    0x55555555, t3, 0xbababa05, 0);
   TESTDSPINST_EXTV("extrv_r.w $t2, $ac2, $t4", "ac2", t2, 0xffffffff,
                    0xffff2435, t4, 0xfbde390e, 0);
   TESTDSPINST_EXTV("extrv_r.w $t1, $ac1, $t7", "ac1", t1, 0x00000000,
                    0x55555555, t7, 0x55555514, 0);
   TESTDSPINST_EXTV("extrv_r.w $t2, $ac2, $t5", "ac2", t2, 0xffffffff,
                    0xffff2435, t5, 0x0000cd10, 0);
   TESTDSPINST_EXTV("extrv_r.w $t1, $ac1, $t2", "ac1", t1, 0x00000000,
                    0x55555555, t2, 0x80000005, 0);
   TESTDSPINST_EXTV("extrv_r.w $t2, $ac2, $t3", "ac2", t2, 0xffffffff,
                    0xffff2435, t3, 0x7fffff16, 0);
   TESTDSPINST_EXTV("extrv_r.w $t0, $ac0, $t1", "ac0", t0, 0x00000000,
                    0x0fde3126, t1, 0xbabababa, 0);
   TESTDSPINST_EXTV("extrv_r.w $t3, $ac3, $t2", "ac3", t3, 0xffffffff,
                    0xabababab, t2, 0xfbde3976, 0);
   TESTDSPINST_EXTV("extrv_r.w $t6, $ac1, $t7", "ac1", t6, 0x00000000,
                    0x00000001, t7, 0x55555555, 0);
   TESTDSPINST_EXTV("extrv_r.w $t1, $ac0, $t2", "ac0", t1, 0xffffffff,
                    0x80000000, t2, 0x80000000, 0);
   TESTDSPINST_EXTV("extrv_r.w $t3, $ac2, $t4", "ac2", t3, 0x00000000,
                    0x00000006, t4, 0x7fffffff, 0);
   TESTDSPINST_EXTV("extrv_r.w $t5, $ac0, $t6", "ac0", t5, 0x00000000,
                    0x7fffffff, t6, 0x0fde3126, 0);
   TESTDSPINST_EXTV("extrv_r.w $t7, $ac2, $t8", "ac2", t7, 0xffffffff,
                    0xffffffff, t8, 0xaaaaaaaa, 0);
   TESTDSPINST_EXTV("extrv_r.w $t8, $ac3, $t9", "ac3", t8, 0xffffffff,
                    0xffffffff, t9, 0xffff2435, 0);
   TESTDSPINST_EXTV("extrv_r.w $t4, $ac0, $t3", "ac0", t4, 0xffffffff,
                    0xfc79b4d2, t3, 0x12349876, 0);
   TESTDSPINST_EXTV("extrv_r.w $t5, $ac1, $t4", "ac1", t5, 0x00000000,
                    0x00000000, t4, 0x00354565, 0);
   TESTDSPINST_EXTV("extrv_r.w $t6, $ac2, $t5", "ac2", t6, 0x00000000,
                    0x00000000, t5, 0x00086755, 0);
   TESTDSPINST_EXTV("extrv_r.w $t7, $ac3, $t6", "ac3", t7, 0x00000000,
                    0x12349876, t6, 0x00000018, 0);
   TESTDSPINST_EXTV("extrv_r.w $t8, $ac0, $t7", "ac0", t8, 0x00000000,
                    0x00354565, t7, 0x23534870, 0);
   TESTDSPINST_EXTV("extrv_r.w $t0, $ac1, $t8", "ac1", t0, 0x00000000,
                    0x00086755, t8, 0x92784656, 0);
   TESTDSPINST_EXTV("extrv_r.w $t1, $ac2, $t9", "ac2", t1, 0xffffffff,
                    0x8f8f8f8f, t9, 0xeeeeeeee, 0);
   TESTDSPINST_EXTV("extrv_r.w $t2, $ac3, $t1", "ac3", t2, 0xffffffff,
                    0xeeeeeeee, t1, 0xcacacaca, 0);
   TESTDSPINST_EXTV("extrv_r.w $t3, $ac0, $t1", "ac0", t3, 0x00000000,
                    0x1bdbdbdb, t1, 0xbacabaca, 0);
   TESTDSPINST_EXTV("extrv_r.w $t4, $ac1, $t4", "ac1", t4, 0xffffffff,
                    0xdecadeca, t4, 0x1bdbdbdb, 0);
   TESTDSPINST_EXTV("extrv_r.w $t5, $ac0, $t8", "ac0", t5, 0xf0f0f0f0,
                    0x5fc92974, t8, 0xffff2435, 0);        
   TESTDSPINST_EXTV("extrv_r.w $t6, $ac1, $t0", "ac1", t6, 0x00000000,
                    0x7e08184e, t0, 0x55555555, 0);        
   TESTDSPINST_EXTV("extrv_r.w $t7, $ac2, $t1", "ac2", t7, 0xfbde3976,
                    0x71c8315f, t1, 0xffff2435, 0);        
   TESTDSPINST_EXTV("extrv_r.w $t8, $ac3, $t2", "ac3", t8, 0x0bed7654,
                    0x9493110e, t2, 0x55555555, 0);        
   TESTDSPINST_EXTV("extrv_r.w $t0, $ac0, $t3", "ac0", t0, 0x23534870,
                    0xbb246228, t3, 0xffff2435, 0);        
   TESTDSPINST_EXTV("extrv_r.w $t1, $ac1, $t4", "ac1", t1, 0x980b7cde,
                    0x339d8d88, t4, 0xabababab, 0);        
   TESTDSPINST_EXTV("extrv_r.w $t2, $ac2, $t5", "ac2", t2, 0x00000018,
                    0x70974249, t5, 0xfc79b4d2, 0);        
   TESTDSPINST_EXTV("extrv_r.w $t1, $ac1, $t6", "ac1", t1, 0x92784656,
                    0x8a8d4e7d, t6, 0x00000000, 0);        
   TESTDSPINST_EXTV("extrv_r.w $t2, $ac2, $t7", "ac2", t2, 0xcacacaca,
                    0xeb1b4335, t7, 0x00000000, 0);        
   TESTDSPINST_EXTV("extrv_r.w $t1, $ac1, $t8", "ac1", t1, 0xbacabaca,
                    0x0cd6b508, t8, 0x12349876, 0);        
   TESTDSPINST_EXTV("extrv_r.w $t2, $ac2, $t0", "ac2", t2, 0x00000000,
                    0x6731e282, t0, 0x00354565, 0);        
   TESTDSPINST_EXTV("extrv_r.w $t3, $ac3, $t1", "ac3", t3, 0x00000000,
                    0xb6edf28f, t1, 0x00086755, 0);        
   TESTDSPINST_EXTV("extrv_r.w $t4, $ac0, $t2", "ac0", t4, 0x00000000,
                    0x4b4ec9ca, t2, 0x8f8f8f8f, 0);        
   TESTDSPINST_EXTV("extrv_r.w $t5, $ac1, $t3", "ac1", t5, 0xffffffff,
                    0xc1037fa4, t3, 0xeeeeeeee, 0);        
   TESTDSPINST_EXTV("extrv_r.w $t6, $ac2, $t3", "ac2", t6, 0xffffffff,
                    0xcb4ab48f, t3, 0x1bdbdbdb, 0);        
   TESTDSPINST_EXTV("extrv_r.w $t7, $ac3, $t4", "ac3", t7, 0x00000000,
                    0xaf8f7e18, t4, 0xbb246228, 0);        
   TESTDSPINST_EXTV("extrv_r.w $t8, $ac0, $t5", "ac0", t8, 0x00000000,
                    0x87df4510, t5, 0x339d8d88, 0);        
   TESTDSPINST_EXTV("extrv_r.w $t0, $ac1, $t6", "ac1", t0, 0xffffffff,
                    0xabf4e8e1, t6, 0x70974249, 0);        
   TESTDSPINST_EXTV("extrv_r.w $t1, $ac2, $t7", "ac2", t1, 0xffffffff,
                    0xf4c0eeac, t7, 0x8a8d4e7d, 0);        
   TESTDSPINST_EXTV("extrv_r.w $t2, $ac3, $t8", "ac3", t2, 0x00000000,
                    0x006a54f2, t8, 0xeb1b4335, 0);        
   TESTDSPINST_EXTV("extrv_r.w $t3, $ac0, $t0", "ac0", t3, 0x00000000,
                    0x79f74493, t0, 0x0cd6b508, 0);        
   TESTDSPINST_EXTV("extrv_r.w $t4, $ac1, $t1", "ac1", t4, 0xffffffff,
                    0x9c09e313, t1, 0x6731e282, 0);

   printf("-------- EXTRV_RS.W --------\n");
   TESTDSPINST_EXTV("extrv_rs.w $t1, $ac1, $t3", "ac1", t1, 0x00000000,
                    0x00000000, t3, 0xbababa00, 0);
   TESTDSPINST_EXTV("extrv_rs.w $t1, $ac1, $t3", "ac1", t1, 0x987b2fff,
                    0xffffffff, t3, 0xbababa00, 0);
   TESTDSPINST_EXTV("extrv_rs.w $t2, $ac2, $t4", "ac2", t2, 0x7fffffff,
                    0xcbcdef01, t4, 0xfbde391f, 0);
   TESTDSPINST_EXTV("extrv_rs.w $t1, $ac1, $t7", "ac1", t1, 0x3fffffff,
                    0x2bcdef01, t7, 0x5555551f, 0);
   TESTDSPINST_EXTV("extrv_rs.w $t2, $ac2, $t5", "ac2", t2, 0xffffffff,
                    0xffffffff, t5, 0x0000cd00, 0);
   TESTDSPINST_EXTV("extrv_rs.w $t1, $ac1, $t2", "ac1", t1, 0x00000000,
                    0xfffffffe, t2, 0x80000001, 0);
   TESTDSPINST_EXTV("extrv_rs.w $t1, $ac1, $t3", "ac1", t1, 0x00000000,
                    0x55555555, t3, 0xbababa05, 0);
   TESTDSPINST_EXTV("extrv_rs.w $t2, $ac2, $t4", "ac2", t2, 0xffffffff,
                    0xffff2435, t4, 0xfbde390e, 0);
   TESTDSPINST_EXTV("extrv_rs.w $t1, $ac1, $t7", "ac1", t1, 0x00000000,
                    0x55555555, t7, 0x55555514, 0);
   TESTDSPINST_EXTV("extrv_rs.w $t2, $ac2, $t5", "ac2", t2, 0xffffffff,
                    0xffff2435, t5, 0x0000cd10, 0);
   TESTDSPINST_EXTV("extrv_rs.w $t1, $ac1, $t2", "ac1", t1, 0x00000000,
                    0x55555555, t2, 0x80000005, 0);
   TESTDSPINST_EXTV("extrv_rs.w $t2, $ac2, $t3", "ac2", t2, 0xffffffff,
                    0xffff2435, t3, 0x7fffff16, 0);
   TESTDSPINST_EXTV("extrv_rs.w $t0, $ac0, $t1", "ac0", t0, 0x00000000,
                    0x0fde3126, t1, 0xbabababa, 0);
   TESTDSPINST_EXTV("extrv_rs.w $t3, $ac3, $t2", "ac3", t3, 0xffffffff,
                    0xabababab, t2, 0xfbde3976, 0);
   TESTDSPINST_EXTV("extrv_rs.w $t6, $ac1, $t7", "ac1", t6, 0x00000000,
                    0x00000001, t7, 0x55555555, 0);
   TESTDSPINST_EXTV("extrv_rs.w $t1, $ac0, $t2", "ac0", t1, 0xffffffff,
                    0x80000000, t2, 0x80000000, 0);
   TESTDSPINST_EXTV("extrv_rs.w $t5, $ac0, $t6", "ac0", t5, 0x00000000,
                    0x7fffffff, t6, 0x0fde3126, 0);
   TESTDSPINST_EXTV("extrv_rs.w $t7, $ac2, $t8", "ac2", t7, 0xffffffff,
                    0xffffffff, t8, 0xaaaaaaaa, 0);
   TESTDSPINST_EXTV("extrv_rs.w $t8, $ac3, $t9", "ac3", t8, 0xffffffff,
                    0xffffffff, t9, 0xffff2435, 0);
   TESTDSPINST_EXTV("extrv_rs.w $t4, $ac0, $t3", "ac0", t4, 0xffffffff,
                    0xfc79b4d2, t3, 0x12349876, 0);
   TESTDSPINST_EXTV("extrv_rs.w $t5, $ac1, $t4", "ac1", t5, 0xf0f0f0f0,
                    0x00000000, t4, 0x00354565, 0);         
   TESTDSPINST_EXTV("extrv_rs.w $t6, $ac2, $t5", "ac2", t6, 0x00000000,
                    0x00000000, t5, 0x00086755, 0);         
   TESTDSPINST_EXTV("extrv_rs.w $t7, $ac3, $t6", "ac3", t7, 0xfbde3976,
                    0x12349876, t6, 0x00000018, 0);         
   TESTDSPINST_EXTV("extrv_rs.w $t8, $ac0, $t7", "ac0", t8, 0x0bed7654,
                    0x00354565, t7, 0x23534870, 0);         
   TESTDSPINST_EXTV("extrv_rs.w $t0, $ac1, $t8", "ac1", t0, 0x23534870,
                    0x00086755, t8, 0x92784656, 0);         
   TESTDSPINST_EXTV("extrv_rs.w $t1, $ac2, $t9", "ac2", t1, 0x980b7cde,
                    0x8f8f8f8f, t9, 0xeeeeeeee, 0);         
   TESTDSPINST_EXTV("extrv_rs.w $t2, $ac3, $t1", "ac3", t2, 0x00000018,
                    0xeeeeeeee, t1, 0xcacacaca, 0);         
   TESTDSPINST_EXTV("extrv_rs.w $t3, $ac0, $t1", "ac0", t3, 0x92784656,
                    0x1bdbdbdb, t1, 0xbacabaca, 0);         
   TESTDSPINST_EXTV("extrv_rs.w $t4, $ac1, $t4", "ac1", t4, 0xcacacaca,
                    0xdecadeca, t4, 0x1bdbdbdb, 0);         
   TESTDSPINST_EXTV("extrv_rs.w $t5, $ac0, $t8", "ac0", t5, 0xbacabaca,
                    0x5fc92974, t8, 0xffff2435, 0);
   TESTDSPINST_EXTV("extrv_rs.w $t6, $ac1, $t0", "ac1", t6, 0x00000000,
                    0x7e08184e, t0, 0x55555555, 0);
   TESTDSPINST_EXTV("extrv_rs.w $t7, $ac2, $t1", "ac2", t7, 0x00000000,
                    0x71c8315f, t1, 0xffff2435, 0);
   TESTDSPINST_EXTV("extrv_rs.w $t8, $ac3, $t2", "ac3", t8, 0xffffffff,
                    0x9493110e, t2, 0x55555555, 0);
   TESTDSPINST_EXTV("extrv_rs.w $t0, $ac0, $t3", "ac0", t0, 0xffffffff,
                    0xbb246228, t3, 0xffff2435, 0);
   TESTDSPINST_EXTV("extrv_rs.w $t1, $ac1, $t4", "ac1", t1, 0x00000000,
                    0x339d8d88, t4, 0xabababab, 0);
   TESTDSPINST_EXTV("extrv_rs.w $t2, $ac2, $t5", "ac2", t2, 0x00000000,
                    0x70974249, t5, 0xfc79b4d2, 0);
   TESTDSPINST_EXTV("extrv_rs.w $t1, $ac1, $t6", "ac1", t1, 0xffffffff,
                    0x8a8d4e7d, t6, 0x00000000, 0);
   TESTDSPINST_EXTV("extrv_rs.w $t2, $ac2, $t7", "ac2", t2, 0xffffffff,
                    0xeb1b4335, t7, 0x00000000, 0);
   TESTDSPINST_EXTV("extrv_rs.w $t1, $ac1, $t8", "ac1", t1, 0x00000000,
                    0x0cd6b508, t8, 0x12349876, 0);
   TESTDSPINST_EXTV("extrv_rs.w $t2, $ac2, $t0", "ac2", t2, 0x00000000,
                    0x6731e282, t0, 0x00354565, 0);
   TESTDSPINST_EXTV("extrv_rs.w $t3, $ac3, $t1", "ac3", t3, 0xffffffff,
                    0xb6edf28f, t1, 0x00086755, 0);
   TESTDSPINST_EXTV("extrv_rs.w $t4, $ac0, $t2", "ac0", t4, 0x00000000,
                    0x4b4ec9ca, t2, 0x8f8f8f8f, 0);
   TESTDSPINST_EXTV("extrv_rs.w $t5, $ac1, $t3", "ac1", t5, 0xffffffff,
                    0xc1037fa4, t3, 0xeeeeeeee, 0);
   TESTDSPINST_EXTV("extrv_rs.w $t6, $ac2, $t3", "ac2", t6, 0xffffffff,
                    0xcb4ab48f, t3, 0x1bdbdbdb, 0);
   TESTDSPINST_EXTV("extrv_rs.w $t7, $ac3, $t4", "ac3", t7, 0xffffffff,
                    0xaf8f7e18, t4, 0xbb246228, 0);
   TESTDSPINST_EXTV("extrv_rs.w $t8, $ac0, $t5", "ac0", t8, 0xffffffff,
                    0x87df4510, t5, 0x339d8d88, 0);
   TESTDSPINST_EXTV("extrv_rs.w $t0, $ac1, $t6", "ac1", t0, 0xffffffff,
                    0xabf4e8e1, t6, 0x70974249, 0);
   TESTDSPINST_EXTV("extrv_rs.w $t1, $ac2, $t7", "ac2", t1, 0xffffffff,
                    0xf4c0eeac, t7, 0x8a8d4e7d, 0);
   TESTDSPINST_EXTV("extrv_rs.w $t2, $ac3, $t8", "ac3", t2, 0x00000000,
                    0x006a54f2, t8, 0xeb1b4335, 0);
   TESTDSPINST_EXTV("extrv_rs.w $t3, $ac0, $t0", "ac0", t3, 0x00000000,
                    0x79f74493, t0, 0x0cd6b508, 0);
   TESTDSPINST_EXTV("extrv_rs.w $t4, $ac1, $t1", "ac1", t4, 0xffffffff,
                    0x9c09e313, t1, 0x6731e282, 0);

   printf("-------- INSV --------\n");
   TESTDSPINST_INSV("insv $t5, $t3", 0xffffffff, 0x00000000, t5, t3,  7,
                    ( 1<<7));
   TESTDSPINST_INSV("insv $t2, $t4", 0x004dfbe5, 0xe87927cc, t2, t4,  2,
                    (15<<7));
   TESTDSPINST_INSV("insv $t0, $t8", 0xf6a3fa3c, 0x083b3571, t0, t8,  4,
                    ( 2<<7));
   TESTDSPINST_INSV("insv $t0, $t1", 0xbf17fb9a, 0xb9743941, t0, t1, 13,
                    ( 0<<7));
   TESTDSPINST_INSV("insv $t2, $t3", 0x2c0bd024, 0xbce5f924, t2, t3,  5,
                    ( 5<<7));
   TESTDSPINST_INSV("insv $t5, $t3", 0x288593c0, 0x722d5e20, t5, t3, 28,
                    ( 4<<7));
   TESTDSPINST_INSV("insv $t2, $t4", 0x4d7ff5b4, 0xa1d6f791, t2, t4,  0,
                    (32<<7));
   TESTDSPINST_INSV("insv $t0, $t8", 0x4557be13, 0x7b11bee7, t0, t8, 16,
                    (15<<7));
   TESTDSPINST_INSV("insv $t4, $t5", 0xadcf5772, 0xa5631488, t4, t5, 15,
                    ( 7<<7));
   TESTDSPINST_INSV("insv $t0, $t1", 0x989a7235, 0xb10bcc65, t0, t1, 19,
                    ( 8<<7));
   TESTDSPINST_INSV("insv $t2, $t3", 0x4d6f393a, 0x73f39fca, t2, t3, 30,
                    ( 1<<7));
   TESTDSPINST_INSV("insv $t4, $t1", 0x24a3291e, 0x5648e540, t4, t1,  1,
                    (28<<7));
   TESTDSPINST_INSV("insv $t6, $t7", 0xdd91eebf, 0xc54f79e6, t6, t7, 17,
                    ( 5<<7));
   TESTDSPINST_INSV("insv $t5, $t3", 0xf7ce2ec6, 0x5fc92974, t5, t3, 26,
                    ( 2<<7));
   TESTDSPINST_INSV("insv $t2, $t4", 0xbc1083e8, 0x7e08184e, t2, t4, 14,
                    (13<<7));
   TESTDSPINST_INSV("insv $t0, $t8", 0xa617cc31, 0x71c8315f, t0, t8,  8,
                    (17<<7));
   TESTDSPINST_INSV("insv $t4, $t5", 0xdfe1e8f0, 0x9493110e, t4, t5,  9,
                    (11<<7));
   TESTDSPINST_INSV("insv $t2, $t4", 0x31458a23, 0xbb246228, t2, t4, 23,
                    ( 9<<7));
   TESTDSPINST_INSV("insv $t0, $t8", 0x848af791, 0x339d8d88, t0, t8,  6,
                    (19<<7));
   TESTDSPINST_INSV("insv $t0, $t1", 0xda3bacdc, 0x70974249, t0, t1, 19,
                    ( 8<<7));
   TESTDSPINST_INSV("insv $t0, $t1", 0x2fff0000, 0x00000001, t0, t1,  7,
                    (13<<7));
   TESTDSPINST_INSV("insv $t2, $t3", 0x2fff0000, 0x73741802, t2, t3,  2,
                    (23<<7));
   TESTDSPINST_INSV("insv $t4, $t1", 0x2fff0000, 0x80003403, t4, t1,  4,
                    (28<<7));
   TESTDSPINST_INSV("insv $t0, $t1", 0xabababab, 0x00000000, t0, t1,  0,
                    (22<<7));
   TESTDSPINST_INSV("insv $t2, $t3", 0xdecadeca, 0x80000000, t2, t3, 26,
                    ( 0<<7));
   TESTDSPINST_INSV("insv $t4, $t1", 0xbacabaca, 0x55555555, t4, t1, 12,
                    ( 3<<7));
   TESTDSPINST_INSV("insv $t6, $t7", 0x3545ff80, 0xffff2434, t6, t7,  1,
                    (23<<7));
   TESTDSPINST_INSV("insv $t2, $t4", 0xc4dbfe20, 0xfc79b4d2, t2, t4, 11,
                    ( 8<<7));
   TESTDSPINST_INSV("insv $t0, $t8", 0x00000000, 0x00000000, t0, t8, 16,
                    ( 9<<7));
   TESTDSPINST_INSV("insv $t0, $t1", 0xad80bce4, 0x00086755, t0, t1, 13,
                    (17<<7));
   TESTDSPINST_INSV("insv $t2, $t3", 0x7f003245, 0x8f8f8f8f, t2, t3,  8,
                    (18<<7));
   TESTDSPINST_INSV("insv $t5, $t3", 0x980b7cde, 0xdecadeca, t5, t3,  4,
                    (15<<7));

{
   printf("DSP LWX\n");
   ppMem(mem, 16);
   int i;
   for(i = 0; i < 64; i+=4){
      TESTDSPINST_LWX(i, t0, t1);
   }
}

{
   printf("DSP LHX\n");
   ppMem(mem, 16);
   int i;
   for(i = 0; i < 64; i+=2){
      TESTDSPINST_LHX(i, t0, t1);
   }
}

{
   printf("DSP LBUX\n");
   ppMem(mem, 16);
   int i;
   for(i = 0; i < 64; i++){
      TESTDSPINST_LBUX(i, t0, t1);
   }
}

   printf("-------- MADD --------\n");
   TESTDSPINST_AC_RS_RT_NODSPC("madd $ac3, $t4, $t5", "ac3", 0x00000000,
                               0x00000000, 0xffffffff, 0x80000000, t4, t5);
   TESTDSPINST_AC_RS_RT_NODSPC("madd $ac0, $t0, $t1", "ac0", 0x00000004,
                               1073741824, 0x00000000, 0x00000006, t0, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("madd $ac1, $t2, $t3", "ac1", 0x80002435,
                               0x80003421, 0x00000000, 1073741824, t2, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("madd $ac3, $t6, $t7", "ac3", 0x76548000,
                               0x73468000, 0x00000000, 0x7fffffff, t6, t7);
   TESTDSPINST_AC_RS_RT_NODSPC("madd $ac0, $t5, $t3", "ac0", 0x80000000,
                               0x80000000, 0x00000000, 0x00000001, t5, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("madd $ac1, $t2, $t4", "ac1", 0x00010001,
                               0xffffffff, 0xffffffff, 0xffffffff, t2, t4);
   TESTDSPINST_AC_RS_RT_NODSPC("madd $ac2, $t0, $t8", "ac2", 0x7fff7fff,
                               0x7fff7fff, 0xffffffff, 0xffffffff, t0, t8);
   TESTDSPINST_AC_RS_RT_NODSPC("madd $ac0, $t0, $t1", "ac0", 0x0000c420,
                               0x00000555, 0x00000000, 0x0fde3126, t0, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("madd $ac1, $t2, $t3", "ac1", 0x00000000,
                               0x00000000, 0x00000000, 0x55555555, t2, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("madd $ac2, $t4, $t1", "ac2", 0x80000000,
                               0x80000000, 0xffffffff, 0xffff2435, t4, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("madd $ac3, $t6, $t7", "ac3", 0xaaaaaaaa,
                               0x55555555, 0xffffffff, 0xabababab, t6, t7);
   TESTDSPINST_AC_RS_RT_NODSPC("madd $ac0, $t5, $t3", "ac0", 0x00000018,
                               0xffff2435, 0xffffffff, 0xfc79b4d2, t5, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("madd $ac1, $t2, $t4", "ac1", 0xbabababa,
                               0xabababab, 0x00000000, 0x00000000, t2, t4);
   TESTDSPINST_AC_RS_RT_NODSPC("madd $ac2, $t0, $t8", "ac2", 0xf0f0f0f0,
                               0xfc79b4d2, 0x00000000, 0x00000000, t0, t8);
   TESTDSPINST_AC_RS_RT_NODSPC("madd $ac3, $t4, $t5", "ac3", 0xfbde3976,
                               0x00000000, 0x00000000, 0x12349876, t4, t5);
   TESTDSPINST_AC_RS_RT_NODSPC("madd $ac0, $t0, $t1", "ac0", 0x23534870,
                               0x00354565, 0x00000000, 0x00354565, t0, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("madd $ac1, $t2, $t3", "ac1", 0x980b7cde,
                               0x00086755, 0x00000000, 0x00086755, t2, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("madd $ac2, $t4, $t1", "ac2", 0x00000018,
                               0x8f8f8f8f, 0xffffffff, 0x8f8f8f8f, t4, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("madd $ac3, $t6, $t7", "ac3", 0x92784656,
                               0xeeeeeeee, 0xffffffff, 0xeeeeeeee, t6, t7);
   TESTDSPINST_AC_RS_RT_NODSPC("madd $ac0, $t5, $t3", "ac0", 0xcacacaca,
                               0x1bdbdbdb, 0x00000000, 0x1bdbdbdb, t5, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("madd $ac1, $t2, $t4", "ac1", 0xbacabaca,
                               0xdecadeca, 0xffffffff, 0xdecadeca, t2, t4);
   TESTDSPINST_AC_RS_RT_NODSPC("madd $ac2, $t0, $t8", "ac2", 0x12fadeb4,
                               0x93474bde, 0xffffffff, 0x93474bde, t0, t8);
   TESTDSPINST_AC_RS_RT_NODSPC("madd $ac3, $t4, $t5", "ac3", 0x7c000790,
                               0xfc0007ff, 0xffffffff, 0xfabfabfa, t4, t5);
   TESTDSPINST_AC_RS_RT_NODSPC("madd $ac2, $t0, $t8", "ac2", 0xffffffff,
                               0xffffffff, 0x00000000, 0x083b3571, t0, t8);
   TESTDSPINST_AC_RS_RT_NODSPC("madd $ac0, $t0, $t1", "ac0", 0x24a3291e,
                               0x5648e540, 0xffffffff, 0xb9743941, t0, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("madd $ac1, $t2, $t3", "ac1", 0xdd91eebf,
                               0xc54f79e6, 0xffffffff, 0xbce5f924, t2, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("madd $ac2, $t4, $t1", "ac2", 0xf7ce2ec6,
                               0x5fc92974, 0xffffffff, 0xcc3c201c, t4, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("madd $ac3, $t6, $t7", "ac3", 0xbc1083e8,
                               0x7e08184e, 0x00000000, 0x1ebaf88e, t6, t7);
   TESTDSPINST_AC_RS_RT_NODSPC("madd $ac0, $t5, $t3", "ac0", 0xa617cc31,
                               0x71c8315f, 0x00000000, 0x722d5e20, t5, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("madd $ac1, $t2, $t4", "ac1", 0xdfe1e8f0,
                               0x9493110e, 0xffffffff, 0xa1d6f791, t2, t4);
   TESTDSPINST_AC_RS_RT_NODSPC("madd $ac2, $t0, $t8", "ac2", 0x31458a23,
                               0xbb246228, 0x00000000, 0x7b11bee7, t0, t8);
   TESTDSPINST_AC_RS_RT_NODSPC("madd $ac3, $t4, $t5", "ac3", 0x848af791,
                               0x339d8d88, 0xffffffff, 0xa5631488, t4, t5);
   TESTDSPINST_AC_RS_RT_NODSPC("madd $ac0, $t0, $t1", "ac0", 0xda3bacdc,
                               0x70974249, 0xffffffff, 0xb10bcc65, t0, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("madd $ac1, $t2, $t3", "ac1", 0x649d5cbd,
                               0x8a8d4e7d, 0x00000000, 0x73f39fca, t2, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("madd $ac2, $t4, $t1", "ac2", 0xc0c8c881,
                               0xeb1b4335, 0x00000000, 0x5648e540, t4, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("madd $ac3, $t6, $t7", "ac3", 0x7dd81a20,
                               0x0cd6b508, 0xffffffff, 0xc54f79e6, t6, t7);
   TESTDSPINST_AC_RS_RT_NODSPC("madd $ac0, $t5, $t3", "ac0", 0x7fff7fff,
                               0x6731e282, 0x00000000, 0x5fc92974, t5, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("madd $ac1, $t2, $t4", "ac1", 0x00000555,
                               0xb6edf28f, 0x00000000, 0x7e08184e, t2, t4);
   TESTDSPINST_AC_RS_RT_NODSPC("madd $ac2, $t0, $t8", "ac2", 0x00000000,
                               0x4b4ec9ca, 0x00000000, 0x71c8315f, t0, t8);
   TESTDSPINST_AC_RS_RT_NODSPC("madd $ac3, $t4, $t5", "ac3", 0x80000000,
                               0xc1037fa4, 0xffffffff, 0x9493110e, t4, t5);
   TESTDSPINST_AC_RS_RT_NODSPC("madd $ac1, $t2, $t4", "ac1", 0x55555555,
                               0xcb4ab48f, 0xffffffff, 0xbb246228, t2, t4);
   TESTDSPINST_AC_RS_RT_NODSPC("madd $ac2, $t0, $t8", "ac2", 0xffff8000,
                               0xaf8f8000, 0x00000000, 0x339d8d88, t0, t8);
   TESTDSPINST_AC_RS_RT_NODSPC("madd $ac0, $t0, $t1", "ac0", 0xabababab,
                               0x87df4510, 0x00000000, 0x70974249, t0, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("madd $ac1, $t2, $t3", "ac1", 0xfc79b4d2,
                               0xabf4e8e1, 0xffffffff, 0x8a8d4e7d, t2, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("madd $ac2, $t4, $t1", "ac2", 0x00000000,
                               0xf4c0eeac, 0xffffffff, 0xeb1b4335, t4, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("madd $ac3, $t6, $t7", "ac3", 0x00354565,
                               0x006a54f2, 0x00000000, 0x0cd6b508, t6, t7);
   TESTDSPINST_AC_RS_RT_NODSPC("madd $ac0, $t5, $t3", "ac0", 0x00086755,
                               0x79f74493, 0x00000000, 0x6731e282, t5, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("madd $ac1, $t2, $t4", "ac1", 0xffff8000,
                               0x9c098000, 0xffffffff, 0xb6edf28f, t2, t4);

   printf("-------- MADDU --------\n");
   TESTDSPINST_AC_RS_RT_NODSPC("maddu $ac3, $t4, $t5", "ac3", 0x00000000,
                               0x00000000, 0xffffffff, 0x80000000, t4, t5);
   TESTDSPINST_AC_RS_RT_NODSPC("maddu $ac0, $t0, $t1", "ac0", 0x00000004,
                               1073741824, 0x00000000, 0x00000006, t0, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("maddu $ac1, $t2, $t3", "ac1", 0x80002435,
                               0x80003421, 0x00000000, 1073741824, t2, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("maddu $ac3, $t6, $t7", "ac3", 0x76548000,
                               0x73468000, 0x00000000, 0x7fffffff, t6, t7);
   TESTDSPINST_AC_RS_RT_NODSPC("maddu $ac0, $t5, $t3", "ac0", 0x80000000,
                               0x80000000, 0x00000000, 0x00000001, t5, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("maddu $ac1, $t2, $t4", "ac1", 0x00010001,
                               0xffffffff, 0xffffffff, 0xffffffff, t2, t4);
   TESTDSPINST_AC_RS_RT_NODSPC("maddu $ac2, $t0, $t8", "ac2", 0x7fff7fff,
                               0x7fff7fff, 0xffffffff, 0xffffffff, t0, t8);
   TESTDSPINST_AC_RS_RT_NODSPC("maddu $ac0, $t0, $t1", "ac0", 0x0000c420,
                               0x00000555, 0x00000000, 0x0fde3126, t0, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("maddu $ac1, $t2, $t3", "ac1", 0x00000000,
                               0x00000000, 0x00000000, 0x55555555, t2, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("maddu $ac2, $t4, $t1", "ac2", 0x80000000,
                               0x80000000, 0xffffffff, 0xffff2435, t4, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("maddu $ac3, $t6, $t7", "ac3", 0xaaaaaaaa,
                               0x55555555, 0xffffffff, 0xabababab, t6, t7);
   TESTDSPINST_AC_RS_RT_NODSPC("maddu $ac0, $t5, $t3", "ac0", 0x00000018,
                               0xffff2435, 0xffffffff, 0xfc79b4d2, t5, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("maddu $ac1, $t2, $t4", "ac1", 0xbabababa,
                               0xabababab, 0x00000000, 0x00000000, t2, t4);
   TESTDSPINST_AC_RS_RT_NODSPC("maddu $ac2, $t0, $t8", "ac2", 0xf0f0f0f0,
                               0xfc79b4d2, 0x00000000, 0x00000000, t0, t8);
   TESTDSPINST_AC_RS_RT_NODSPC("maddu $ac3, $t4, $t5", "ac3", 0xfbde3976,
                               0x00000000, 0x00000000, 0x12349876, t4, t5);
   TESTDSPINST_AC_RS_RT_NODSPC("maddu $ac0, $t0, $t1", "ac0", 0x23534870,
                               0x00354565, 0x00000000, 0x00354565, t0, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("maddu $ac1, $t2, $t3", "ac1", 0x980b7cde,
                               0x00086755, 0x00000000, 0x00086755, t2, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("maddu $ac2, $t4, $t1", "ac2", 0x00000018,
                               0x8f8f8f8f, 0xffffffff, 0x8f8f8f8f, t4, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("maddu $ac3, $t6, $t7", "ac3", 0x92784656,
                               0xeeeeeeee, 0xffffffff, 0xeeeeeeee, t6, t7);
   TESTDSPINST_AC_RS_RT_NODSPC("maddu $ac0, $t5, $t3", "ac0", 0xcacacaca,
                               0x1bdbdbdb, 0x00000000, 0x1bdbdbdb, t5, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("maddu $ac1, $t2, $t4", "ac1", 0xbacabaca,
                               0xdecadeca, 0xffffffff, 0xdecadeca, t2, t4);
   TESTDSPINST_AC_RS_RT_NODSPC("maddu $ac2, $t0, $t8", "ac2", 0x12fadeb4,
                               0x93474bde, 0xffffffff, 0x93474bde, t0, t8);
   TESTDSPINST_AC_RS_RT_NODSPC("maddu $ac3, $t4, $t5", "ac3", 0x7c000790,
                               0xfc0007ff, 0xffffffff, 0xfabfabfa, t4, t5);
   TESTDSPINST_AC_RS_RT_NODSPC("maddu $ac2, $t0, $t8", "ac2", 0xffffffff,
                               0xffffffff, 0x00000000, 0x083b3571, t0, t8);
   TESTDSPINST_AC_RS_RT_NODSPC("maddu $ac0, $t0, $t1", "ac0", 0x24a3291e,
                               0x5648e540, 0xffffffff, 0xb9743941, t0, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("maddu $ac1, $t2, $t3", "ac1", 0xdd91eebf,
                               0xc54f79e6, 0xffffffff, 0xbce5f924, t2, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("maddu $ac2, $t4, $t1", "ac2", 0xf7ce2ec6,
                               0x5fc92974, 0xffffffff, 0xcc3c201c, t4, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("maddu $ac3, $t6, $t7", "ac3", 0xbc1083e8,
                               0x7e08184e, 0x00000000, 0x1ebaf88e, t6, t7);
   TESTDSPINST_AC_RS_RT_NODSPC("maddu $ac0, $t5, $t3", "ac0", 0xa617cc31,
                               0x71c8315f, 0x00000000, 0x722d5e20, t5, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("maddu $ac1, $t2, $t4", "ac1", 0xdfe1e8f0,
                               0x9493110e, 0xffffffff, 0xa1d6f791, t2, t4);
   TESTDSPINST_AC_RS_RT_NODSPC("maddu $ac2, $t0, $t8", "ac2", 0x31458a23,
                               0xbb246228, 0x00000000, 0x7b11bee7, t0, t8);
   TESTDSPINST_AC_RS_RT_NODSPC("maddu $ac3, $t4, $t5", "ac3", 0x848af791,
                               0x339d8d88, 0xffffffff, 0xa5631488, t4, t5);
   TESTDSPINST_AC_RS_RT_NODSPC("maddu $ac0, $t0, $t1", "ac0", 0xda3bacdc,
                               0x70974249, 0xffffffff, 0xb10bcc65, t0, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("maddu $ac1, $t2, $t3", "ac1", 0x649d5cbd,
                               0x8a8d4e7d, 0x00000000, 0x73f39fca, t2, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("maddu $ac2, $t4, $t1", "ac2", 0xc0c8c881,
                               0xeb1b4335, 0x00000000, 0x5648e540, t4, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("maddu $ac3, $t6, $t7", "ac3", 0x7dd81a20,
                               0x0cd6b508, 0xffffffff, 0xc54f79e6, t6, t7);
   TESTDSPINST_AC_RS_RT_NODSPC("maddu $ac0, $t5, $t3", "ac0", 0x7fff7fff,
                               0x6731e282, 0x00000000, 0x5fc92974, t5, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("maddu $ac1, $t2, $t4", "ac1", 0x00000555,
                               0xb6edf28f, 0x00000000, 0x7e08184e, t2, t4);
   TESTDSPINST_AC_RS_RT_NODSPC("maddu $ac2, $t0, $t8", "ac2", 0x00000000,
                               0x4b4ec9ca, 0x00000000, 0x71c8315f, t0, t8);
   TESTDSPINST_AC_RS_RT_NODSPC("maddu $ac3, $t4, $t5", "ac3", 0x80000000,
                               0xc1037fa4, 0xffffffff, 0x9493110e, t4, t5);
   TESTDSPINST_AC_RS_RT_NODSPC("maddu $ac1, $t2, $t4", "ac1", 0x55555555,
                               0xcb4ab48f, 0xffffffff, 0xbb246228, t2, t4);
   TESTDSPINST_AC_RS_RT_NODSPC("maddu $ac2, $t0, $t8", "ac2", 0xffff8000,
                               0xaf8f8000, 0x00000000, 0x339d8d88, t0, t8);
   TESTDSPINST_AC_RS_RT_NODSPC("maddu $ac0, $t0, $t1", "ac0", 0xabababab,
                               0x87df4510, 0x00000000, 0x70974249, t0, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("maddu $ac1, $t2, $t3", "ac1", 0xfc79b4d2,
                               0xabf4e8e1, 0xffffffff, 0x8a8d4e7d, t2, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("maddu $ac2, $t4, $t1", "ac2", 0x00000000,
                               0xf4c0eeac, 0xffffffff, 0xeb1b4335, t4, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("maddu $ac3, $t6, $t7", "ac3", 0x00354565,
                               0x006a54f2, 0x00000000, 0x0cd6b508, t6, t7);
   TESTDSPINST_AC_RS_RT_NODSPC("maddu $ac0, $t5, $t3", "ac0", 0x00086755,
                               0x79f74493, 0x00000000, 0x6731e282, t5, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("maddu $ac1, $t2, $t4", "ac1", 0xffff8000,
                               0x9c098000, 0xffffffff, 0xb6edf28f, t2, t4);

   printf("-------- MSUB --------\n");
   TESTDSPINST_AC_RS_RT_NODSPC("msub $ac3, $t4, $t5", "ac3", 0x00000000,
                               0x00000000, 0xffffffff, 0x80000000, t4, t5);
   TESTDSPINST_AC_RS_RT_NODSPC("msub $ac0, $t0, $t1", "ac0", 0x00000004,
                               1073741824, 0x00000000, 0x00000006, t0, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("msub $ac1, $t2, $t3", "ac1", 0x80002435,
                               0x80003421, 0x00000000, 1073741824, t2, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("msub $ac3, $t6, $t7", "ac3", 0x76548000,
                               0x73468000, 0x00000000, 0x7fffffff, t6, t7);
   TESTDSPINST_AC_RS_RT_NODSPC("msub $ac0, $t5, $t3", "ac0", 0x80000000,
                               0x80000000, 0x00000000, 0x00000001, t5, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("msub $ac1, $t2, $t4", "ac1", 0x00010001,
                               0xffffffff, 0xffffffff, 0xffffffff, t2, t4);
   TESTDSPINST_AC_RS_RT_NODSPC("msub $ac2, $t0, $t8", "ac2", 0x7fff7fff,
                               0x7fff7fff, 0xffffffff, 0xffffffff, t0, t8);
   TESTDSPINST_AC_RS_RT_NODSPC("msub $ac0, $t0, $t1", "ac0", 0x0000c420,
                               0x00000555, 0x00000000, 0x0fde3126, t0, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("msub $ac1, $t2, $t3", "ac1", 0x00000000,
                               0x00000000, 0x00000000, 0x55555555, t2, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("msub $ac2, $t4, $t1", "ac2", 0x80000000,
                               0x80000000, 0xffffffff, 0xffff2435, t4, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("msub $ac3, $t6, $t7", "ac3", 0xaaaaaaaa,
                               0x55555555, 0xffffffff, 0xabababab, t6, t7);
   TESTDSPINST_AC_RS_RT_NODSPC("msub $ac0, $t5, $t3", "ac0", 0x00000018,
                               0xffff2435, 0xffffffff, 0xfc79b4d2, t5, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("msub $ac1, $t2, $t4", "ac1", 0xbabababa,
                               0xabababab, 0x00000000, 0x00000000, t2, t4);
   TESTDSPINST_AC_RS_RT_NODSPC("msub $ac2, $t0, $t8", "ac2", 0xf0f0f0f0,
                               0xfc79b4d2, 0x00000000, 0x00000000, t0, t8);
   TESTDSPINST_AC_RS_RT_NODSPC("msub $ac3, $t4, $t5", "ac3", 0xfbde3976,
                               0x00000000, 0x00000000, 0x12349876, t4, t5);
   TESTDSPINST_AC_RS_RT_NODSPC("msub $ac0, $t0, $t1", "ac0", 0x23534870,
                               0x00354565, 0x00000000, 0x00354565, t0, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("msub $ac1, $t2, $t3", "ac1", 0x980b7cde,
                               0x00086755, 0x00000000, 0x00086755, t2, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("msub $ac2, $t4, $t1", "ac2", 0x00000018,
                               0x8f8f8f8f, 0xffffffff, 0x8f8f8f8f, t4, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("msub $ac3, $t6, $t7", "ac3", 0x92784656,
                               0xeeeeeeee, 0xffffffff, 0xeeeeeeee, t6, t7);
   TESTDSPINST_AC_RS_RT_NODSPC("msub $ac0, $t5, $t3", "ac0", 0xcacacaca,
                               0x1bdbdbdb, 0x00000000, 0x1bdbdbdb, t5, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("msub $ac1, $t2, $t4", "ac1", 0xbacabaca,
                               0xdecadeca, 0xffffffff, 0xdecadeca, t2, t4);
   TESTDSPINST_AC_RS_RT_NODSPC("msub $ac2, $t0, $t8", "ac2", 0x12fadeb4,
                               0x93474bde, 0xffffffff, 0x93474bde, t0, t8);
   TESTDSPINST_AC_RS_RT_NODSPC("msub $ac3, $t4, $t5", "ac3", 0x7c000790,
                               0xfc0007ff, 0xffffffff, 0xfabfabfa, t4, t5);
   TESTDSPINST_AC_RS_RT_NODSPC("msub $ac2, $t0, $t8", "ac2", 0xffffffff,
                               0xffffffff, 0x00000000, 0x083b3571, t0, t8);
   TESTDSPINST_AC_RS_RT_NODSPC("msub $ac0, $t0, $t1", "ac0", 0x24a3291e,
                               0x5648e540, 0xffffffff, 0xb9743941, t0, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("msub $ac1, $t2, $t3", "ac1", 0xdd91eebf,
                               0xc54f79e6, 0xffffffff, 0xbce5f924, t2, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("msub $ac2, $t4, $t1", "ac2", 0xf7ce2ec6,
                               0x5fc92974, 0xffffffff, 0xcc3c201c, t4, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("msub $ac3, $t6, $t7", "ac3", 0xbc1083e8,
                               0x7e08184e, 0x00000000, 0x1ebaf88e, t6, t7);
   TESTDSPINST_AC_RS_RT_NODSPC("msub $ac0, $t5, $t3", "ac0", 0xa617cc31,
                               0x71c8315f, 0x00000000, 0x722d5e20, t5, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("msub $ac1, $t2, $t4", "ac1", 0xdfe1e8f0,
                               0x9493110e, 0xffffffff, 0xa1d6f791, t2, t4);
   TESTDSPINST_AC_RS_RT_NODSPC("msub $ac2, $t0, $t8", "ac2", 0x31458a23,
                               0xbb246228, 0x00000000, 0x7b11bee7, t0, t8);
   TESTDSPINST_AC_RS_RT_NODSPC("msub $ac3, $t4, $t5", "ac3", 0x848af791,
                               0x339d8d88, 0xffffffff, 0xa5631488, t4, t5);
   TESTDSPINST_AC_RS_RT_NODSPC("msub $ac0, $t0, $t1", "ac0", 0xda3bacdc,
                               0x70974249, 0xffffffff, 0xb10bcc65, t0, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("msub $ac1, $t2, $t3", "ac1", 0x649d5cbd,
                               0x8a8d4e7d, 0x00000000, 0x73f39fca, t2, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("msub $ac2, $t4, $t1", "ac2", 0xc0c8c881,
                               0xeb1b4335, 0x00000000, 0x5648e540, t4, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("msub $ac3, $t6, $t7", "ac3", 0x7dd81a20,
                               0x0cd6b508, 0xffffffff, 0xc54f79e6, t6, t7);
   TESTDSPINST_AC_RS_RT_NODSPC("msub $ac0, $t5, $t3", "ac0", 0x7fff7fff,
                               0x6731e282, 0x00000000, 0x5fc92974, t5, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("msub $ac1, $t2, $t4", "ac1", 0x00000555,
                               0xb6edf28f, 0x00000000, 0x7e08184e, t2, t4);
   TESTDSPINST_AC_RS_RT_NODSPC("msub $ac2, $t0, $t8", "ac2", 0x00000000,
                               0x4b4ec9ca, 0x00000000, 0x71c8315f, t0, t8);
   TESTDSPINST_AC_RS_RT_NODSPC("msub $ac3, $t4, $t5", "ac3", 0x80000000,
                               0xc1037fa4, 0xffffffff, 0x9493110e, t4, t5);
   TESTDSPINST_AC_RS_RT_NODSPC("msub $ac1, $t2, $t4", "ac1", 0x55555555,
                               0xcb4ab48f, 0xffffffff, 0xbb246228, t2, t4);
   TESTDSPINST_AC_RS_RT_NODSPC("msub $ac2, $t0, $t8", "ac2", 0xffff8000,
                               0xaf8f8000, 0x00000000, 0x339d8d88, t0, t8);
   TESTDSPINST_AC_RS_RT_NODSPC("msub $ac0, $t0, $t1", "ac0", 0xabababab,
                               0x87df4510, 0x00000000, 0x70974249, t0, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("msub $ac1, $t2, $t3", "ac1", 0xfc79b4d2,
                               0xabf4e8e1, 0xffffffff, 0x8a8d4e7d, t2, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("msub $ac2, $t4, $t1", "ac2", 0x00000000,
                               0xf4c0eeac, 0xffffffff, 0xeb1b4335, t4, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("msub $ac3, $t6, $t7", "ac3", 0x00354565,
                               0x006a54f2, 0x00000000, 0x0cd6b508, t6, t7);
   TESTDSPINST_AC_RS_RT_NODSPC("msub $ac0, $t5, $t3", "ac0", 0x00086755,
                               0x79f74493, 0x00000000, 0x6731e282, t5, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("msub $ac1, $t2, $t4", "ac1", 0xffff8000,
                               0x9c098000, 0xffffffff, 0xb6edf28f, t2, t4);

   printf("-------- MSUBU --------\n");
   TESTDSPINST_AC_RS_RT_NODSPC("msubu $ac3, $t4, $t5", "ac3", 0x00000000,
                               0x00000000, 0xffffffff, 0x80000000, t4, t5);
   TESTDSPINST_AC_RS_RT_NODSPC("msubu $ac0, $t0, $t1", "ac0", 0x00000004,
                               1073741824, 0x00000000, 0x00000006, t0, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("msubu $ac1, $t2, $t3", "ac1", 0x80002435,
                               0x80003421, 0x00000000, 1073741824, t2, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("msubu $ac3, $t6, $t7", "ac3", 0x76548000,
                               0x73468000, 0x00000000, 0x7fffffff, t6, t7);
   TESTDSPINST_AC_RS_RT_NODSPC("msubu $ac0, $t5, $t3", "ac0", 0x80000000,
                               0x80000000, 0x00000000, 0x00000001, t5, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("msubu $ac1, $t2, $t4", "ac1", 0x00010001,
                               0xffffffff, 0xffffffff, 0xffffffff, t2, t4);
   TESTDSPINST_AC_RS_RT_NODSPC("msubu $ac2, $t0, $t8", "ac2", 0x7fff7fff,
                               0x7fff7fff, 0xffffffff, 0xffffffff, t0, t8);
   TESTDSPINST_AC_RS_RT_NODSPC("msubu $ac0, $t0, $t1", "ac0", 0x0000c420,
                               0x00000555, 0x00000000, 0x0fde3126, t0, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("msubu $ac1, $t2, $t3", "ac1", 0x00000000,
                               0x00000000, 0x00000000, 0x55555555, t2, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("msubu $ac2, $t4, $t1", "ac2", 0x80000000,
                               0x80000000, 0xffffffff, 0xffff2435, t4, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("msubu $ac3, $t6, $t7", "ac3", 0xaaaaaaaa,
                               0x55555555, 0xffffffff, 0xabababab, t6, t7);
   TESTDSPINST_AC_RS_RT_NODSPC("msubu $ac0, $t5, $t3", "ac0", 0x00000018,
                               0xffff2435, 0xffffffff, 0xfc79b4d2, t5, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("msubu $ac1, $t2, $t4", "ac1", 0xbabababa,
                               0xabababab, 0x00000000, 0x00000000, t2, t4);
   TESTDSPINST_AC_RS_RT_NODSPC("msubu $ac2, $t0, $t8", "ac2", 0xf0f0f0f0,
                               0xfc79b4d2, 0x00000000, 0x00000000, t0, t8);
   TESTDSPINST_AC_RS_RT_NODSPC("msubu $ac3, $t4, $t5", "ac3", 0xfbde3976,
                               0x00000000, 0x00000000, 0x12349876, t4, t5);
   TESTDSPINST_AC_RS_RT_NODSPC("msubu $ac0, $t0, $t1", "ac0", 0x23534870,
                               0x00354565, 0x00000000, 0x00354565, t0, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("msubu $ac1, $t2, $t3", "ac1", 0x980b7cde,
                               0x00086755, 0x00000000, 0x00086755, t2, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("msubu $ac2, $t4, $t1", "ac2", 0x00000018,
                               0x8f8f8f8f, 0xffffffff, 0x8f8f8f8f, t4, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("msubu $ac3, $t6, $t7", "ac3", 0x92784656,
                               0xeeeeeeee, 0xffffffff, 0xeeeeeeee, t6, t7);
   TESTDSPINST_AC_RS_RT_NODSPC("msubu $ac0, $t5, $t3", "ac0", 0xcacacaca,
                               0x1bdbdbdb, 0x00000000, 0x1bdbdbdb, t5, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("msubu $ac1, $t2, $t4", "ac1", 0xbacabaca,
                               0xdecadeca, 0xffffffff, 0xdecadeca, t2, t4);
   TESTDSPINST_AC_RS_RT_NODSPC("msubu $ac2, $t0, $t8", "ac2", 0x12fadeb4,
                               0x93474bde, 0xffffffff, 0x93474bde, t0, t8);
   TESTDSPINST_AC_RS_RT_NODSPC("msubu $ac3, $t4, $t5", "ac3", 0x7c000790,
                               0xfc0007ff, 0xffffffff, 0xfabfabfa, t4, t5);
   TESTDSPINST_AC_RS_RT_NODSPC("msubu $ac2, $t0, $t8", "ac2", 0xffffffff,
                               0xffffffff, 0x00000000, 0x083b3571, t0, t8);
   TESTDSPINST_AC_RS_RT_NODSPC("msubu $ac0, $t0, $t1", "ac0", 0x24a3291e,
                               0x5648e540, 0xffffffff, 0xb9743941, t0, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("msubu $ac1, $t2, $t3", "ac1", 0xdd91eebf,
                               0xc54f79e6, 0xffffffff, 0xbce5f924, t2, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("msubu $ac2, $t4, $t1", "ac2", 0xf7ce2ec6,
                               0x5fc92974, 0xffffffff, 0xcc3c201c, t4, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("msubu $ac3, $t6, $t7", "ac3", 0xbc1083e8,
                               0x7e08184e, 0x00000000, 0x1ebaf88e, t6, t7);
   TESTDSPINST_AC_RS_RT_NODSPC("msubu $ac0, $t5, $t3", "ac0", 0xa617cc31,
                               0x71c8315f, 0x00000000, 0x722d5e20, t5, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("msubu $ac1, $t2, $t4", "ac1", 0xdfe1e8f0,
                               0x9493110e, 0xffffffff, 0xa1d6f791, t2, t4);
   TESTDSPINST_AC_RS_RT_NODSPC("msubu $ac2, $t0, $t8", "ac2", 0x31458a23,
                               0xbb246228, 0x00000000, 0x7b11bee7, t0, t8);
   TESTDSPINST_AC_RS_RT_NODSPC("msubu $ac3, $t4, $t5", "ac3", 0x848af791,
                               0x339d8d88, 0xffffffff, 0xa5631488, t4, t5);
   TESTDSPINST_AC_RS_RT_NODSPC("msubu $ac0, $t0, $t1", "ac0", 0xda3bacdc,
                               0x70974249, 0xffffffff, 0xb10bcc65, t0, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("msubu $ac1, $t2, $t3", "ac1", 0x649d5cbd,
                               0x8a8d4e7d, 0x00000000, 0x73f39fca, t2, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("msubu $ac2, $t4, $t1", "ac2", 0xc0c8c881,
                               0xeb1b4335, 0x00000000, 0x5648e540, t4, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("msubu $ac3, $t6, $t7", "ac3", 0x7dd81a20,
                               0x0cd6b508, 0xffffffff, 0xc54f79e6, t6, t7);
   TESTDSPINST_AC_RS_RT_NODSPC("msubu $ac0, $t5, $t3", "ac0", 0x7fff7fff,
                               0x6731e282, 0x00000000, 0x5fc92974, t5, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("msubu $ac1, $t2, $t4", "ac1", 0x00000555,
                               0xb6edf28f, 0x00000000, 0x7e08184e, t2, t4);
   TESTDSPINST_AC_RS_RT_NODSPC("msubu $ac2, $t0, $t8", "ac2", 0x00000000,
                               0x4b4ec9ca, 0x00000000, 0x71c8315f, t0, t8);
   TESTDSPINST_AC_RS_RT_NODSPC("msubu $ac3, $t4, $t5", "ac3", 0x80000000,
                               0xc1037fa4, 0xffffffff, 0x9493110e, t4, t5);
   TESTDSPINST_AC_RS_RT_NODSPC("msubu $ac1, $t2, $t4", "ac1", 0x55555555,
                               0xcb4ab48f, 0xffffffff, 0xbb246228, t2, t4);
   TESTDSPINST_AC_RS_RT_NODSPC("msubu $ac2, $t0, $t8", "ac2", 0xffff8000,
                               0xaf8f8000, 0x00000000, 0x339d8d88, t0, t8);
   TESTDSPINST_AC_RS_RT_NODSPC("msubu $ac0, $t0, $t1", "ac0", 0xabababab,
                               0x87df4510, 0x00000000, 0x70974249, t0, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("msubu $ac1, $t2, $t3", "ac1", 0xfc79b4d2,
                               0xabf4e8e1, 0xffffffff, 0x8a8d4e7d, t2, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("msubu $ac2, $t4, $t1", "ac2", 0x00000000,
                               0xf4c0eeac, 0xffffffff, 0xeb1b4335, t4, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("msubu $ac3, $t6, $t7", "ac3", 0x00354565,
                               0x006a54f2, 0x00000000, 0x0cd6b508, t6, t7);
   TESTDSPINST_AC_RS_RT_NODSPC("msubu $ac0, $t5, $t3", "ac0", 0x00086755,
                               0x79f74493, 0x00000000, 0x6731e282, t5, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("msubu $ac1, $t2, $t4", "ac1", 0xffff8000,
                               0x9c098000, 0xffffffff, 0xb6edf28f, t2, t4);

   printf("-------- MAQ_S.W.PHR --------\n");
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phr $ac3, $t4, $t5", "ac3", 0x00000000,
                             0x00000000, 0xffffffff, 0x80000000, t4, t5);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phr $ac0, $t0, $t1", "ac0", 0x00000004,
                             1073741824, 0x00000000, 0x00000006, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phr $ac1, $t2, $t3", "ac1", 0x80002435,
                             0x80003421, 0x00000000, 1073741824, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phr $ac3, $t6, $t7", "ac3", 0x76548000,
                             0x73468000, 0x00000000, 0x7fffffff, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phr $ac0, $t5, $t3", "ac0", 0x80000000,
                             0x80000000, 0x00000000, 0x00000001, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phr $ac1, $t2, $t4", "ac1", 0x00010001,
                             0xffffffff, 0xffffffff, 0xffffffff, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phr $ac2, $t0, $t8", "ac2", 0x7fff7fff,
                             0x7fff7fff, 0xffffffff, 0xffffffff, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phr $ac0, $t0, $t1", "ac0", 0x0000c420,
                             0x00000555, 0x00000000, 0x0fde3126, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phr $ac1, $t2, $t3", "ac1", 0x00000000,
                             0x00000000, 0x00000000, 0x55555555, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phr $ac2, $t4, $t1", "ac2", 0x80000000,
                             0x80000000, 0xffffffff, 0xffff2435, t4, t1);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phr $ac3, $t6, $t7", "ac3", 0xaaaaaaaa,
                             0x55555555, 0xffffffff, 0xabababab, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phr $ac0, $t5, $t3", "ac0", 0x00000018,
                             0xffff2435, 0xffffffff, 0xfc79b4d2, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phr $ac1, $t2, $t4", "ac1", 0xbabababa,
                             0xabababab, 0x00000000, 0x00000000, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phr $ac2, $t0, $t8", "ac2", 0xf0f0f0f0,
                             0xfc79b4d2, 0x00000000, 0x00000000, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phr $ac3, $t4, $t5", "ac3", 0xfbde3976,
                             0x00000000, 0x00000000, 0x12349876, t4, t5);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phr $ac0, $t0, $t1", "ac0", 0x23534870,
                             0x00354565, 0x00000000, 0x00354565, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phr $ac1, $t2, $t3", "ac1", 0x980b7cde,
                             0x00086755, 0x00000000, 0x00086755, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phr $ac2, $t4, $t1", "ac2", 0x00000018,
                             0x8f8f8f8f, 0xffffffff, 0x8f8f8f8f, t4, t1);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phr $ac3, $t6, $t7", "ac3", 0x92784656,
                             0xeeeeeeee, 0xffffffff, 0xeeeeeeee, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phr $ac0, $t5, $t3", "ac0", 0xcacacaca,
                             0x1bdbdbdb, 0x00000000, 0x1bdbdbdb, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phr $ac1, $t2, $t4", "ac1", 0xbacabaca,
                             0xdecadeca, 0xffffffff, 0xdecadeca, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phr $ac2, $t0, $t8", "ac2", 0x12fadeb4,
                             0x93474bde, 0xffffffff, 0x93474bde, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phr $ac3, $t4, $t5", "ac3", 0x7c000790,
                             0xfc0007ff, 0xffffffff, 0xfabfabfa, t4, t5);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phr $ac2, $t0, $t8", "ac2", 0xffffffff,
                             0xffffffff, 0x00000000, 0x083b3571, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phr $ac0, $t0, $t1", "ac0", 0x24a3291e,
                             0x5648e540, 0xffffffff, 0xb9743941, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phr $ac1, $t2, $t3", "ac1", 0xdd91eebf,
                             0xc54f79e6, 0xffffffff, 0xbce5f924, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phr $ac2, $t4, $t1", "ac2", 0xf7ce2ec6,
                             0x5fc92974, 0xffffffff, 0xcc3c201c, t4, t1);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phr $ac3, $t6, $t7", "ac3", 0xbc1083e8,
                             0x7e08184e, 0x00000000, 0x1ebaf88e, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phr $ac0, $t5, $t3", "ac0", 0xa617cc31,
                             0x71c8315f, 0x00000000, 0x722d5e20, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phr $ac1, $t2, $t4", "ac1", 0xdfe1e8f0,
                             0x9493110e, 0xffffffff, 0xa1d6f791, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phr $ac2, $t0, $t8", "ac2", 0x31458a23,
                             0xbb246228, 0x00000000, 0x7b11bee7, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phr $ac3, $t4, $t5", "ac3", 0x848af791,
                             0x339d8d88, 0xffffffff, 0xa5631488, t4, t5);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phr $ac0, $t0, $t1", "ac0", 0xda3bacdc,
                             0x70974249, 0xffffffff, 0xb10bcc65, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phr $ac1, $t2, $t3", "ac1", 0x649d5cbd,
                             0x8a8d4e7d, 0x00000000, 0x73f39fca, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phr $ac2, $t4, $t1", "ac2", 0xc0c8c881,
                             0xeb1b4335, 0x00000000, 0x5648e540, t4, t1);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phr $ac3, $t6, $t7", "ac3", 0x7dd81a20,
                             0x0cd6b508, 0xffffffff, 0xc54f79e6, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phr $ac0, $t5, $t3", "ac0", 0x7fff7fff,
                             0x6731e282, 0x00000000, 0x5fc92974, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phr $ac1, $t2, $t4", "ac1", 0x00000555,
                             0xb6edf28f, 0x00000000, 0x7e08184e, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phr $ac2, $t0, $t8", "ac2", 0x00000000,
                             0x4b4ec9ca, 0x00000000, 0x71c8315f, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phr $ac3, $t4, $t5", "ac3", 0x80000000,
                             0xc1037fa4, 0xffffffff, 0x9493110e, t4, t5);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phr $ac1, $t2, $t4", "ac1", 0x55555555,
                             0xcb4ab48f, 0xffffffff, 0xbb246228, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phr $ac2, $t0, $t8", "ac2", 0xffff8000,
                             0xaf8f8000, 0x00000000, 0x339d8d88, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phr $ac0, $t0, $t1", "ac0", 0xabababab,
                             0x87df4510, 0x00000000, 0x70974249, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phr $ac1, $t2, $t3", "ac1", 0xfc79b4d2,
                             0xabf4e8e1, 0xffffffff, 0x8a8d4e7d, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phr $ac2, $t4, $t1", "ac2", 0x00000000,
                             0xf4c0eeac, 0xffffffff, 0xeb1b4335, t4, t1);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phr $ac3, $t6, $t7", "ac3", 0x00354565,
                             0x006a54f2, 0x00000000, 0x0cd6b508, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phr $ac0, $t5, $t3", "ac0", 0x00086755,
                             0x79f74493, 0x00000000, 0x6731e282, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phr $ac1, $t2, $t4", "ac1", 0xffff8000,
                             0x9c098000, 0xffffffff, 0xb6edf28f, t2, t4);

   printf("-------- MAQ_SA.W.PHR --------\n");
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phr $ac3, $t4, $t5", "ac3", 0x00000000,
                             0x00000000, 0xffffffff, 0x80000000, t4, t5);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phr $ac0, $t0, $t1", "ac0", 0x00000004,
                             1073741824, 0x00000000, 0x00000006, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phr $ac1, $t2, $t3", "ac1", 0x80002435,
                             0x80003421, 0x00000000, 1073741824, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phr $ac3, $t6, $t7", "ac3", 0x76548000,
                             0x73468000, 0x00000000, 0x7fffffff, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phr $ac0, $t5, $t3", "ac0", 0x80000000,
                             0x80000000, 0x00000000, 0x00000001, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phr $ac1, $t2, $t4", "ac1", 0x00010001,
                             0xffffffff, 0xffffffff, 0xffffffff, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phr $ac2, $t0, $t8", "ac2", 0x7fff7fff,
                             0x7fff7fff, 0xffffffff, 0xffffffff, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phr $ac0, $t0, $t1", "ac0", 0x0000c420,
                             0x00000555, 0x00000000, 0x0fde3126, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phr $ac1, $t2, $t3", "ac1", 0x00000000,
                             0x00000000, 0x00000000, 0x55555555, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phr $ac2, $t4, $t1", "ac2", 0x80000000,
                             0x80000000, 0xffffffff, 0xffff2435, t4, t1);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phr $ac3, $t6, $t7", "ac3", 0xaaaaaaaa,
                             0x55555555, 0xffffffff, 0xabababab, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phr $ac0, $t5, $t3", "ac0", 0x00000018,
                             0xffff2435, 0xffffffff, 0xfc79b4d2, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phr $ac1, $t2, $t4", "ac1", 0xbabababa,
                             0xabababab, 0x00000000, 0x00000000, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phr $ac2, $t0, $t8", "ac2", 0xf0f0f0f0,
                             0xfc79b4d2, 0x00000000, 0x00000000, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phr $ac3, $t4, $t5", "ac3", 0xfbde3976,
                             0x00000000, 0x00000000, 0x12349876, t4, t5);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phr $ac0, $t0, $t1", "ac0", 0x23534870,
                             0x00354565, 0x00000000, 0x00354565, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phr $ac1, $t2, $t3", "ac1", 0x980b7cde,
                             0x00086755, 0x00000000, 0x00086755, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phr $ac2, $t4, $t1", "ac2", 0x00000018,
                             0x8f8f8f8f, 0xffffffff, 0x8f8f8f8f, t4, t1);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phr $ac3, $t6, $t7", "ac3", 0x92784656,
                             0xeeeeeeee, 0xffffffff, 0xeeeeeeee, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phr $ac0, $t5, $t3", "ac0", 0xcacacaca,
                             0x1bdbdbdb, 0x00000000, 0x1bdbdbdb, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phr $ac1, $t2, $t4", "ac1", 0xbacabaca,
                             0xdecadeca, 0xffffffff, 0xdecadeca, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phr $ac2, $t0, $t8", "ac2", 0x12fadeb4,
                             0x93474bde, 0xffffffff, 0x93474bde, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phr $ac3, $t4, $t5", "ac3", 0x7c000790,
                             0xfc0007ff, 0xffffffff, 0xfabfabfa, t4, t5);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phr $ac2, $t0, $t8", "ac2", 0xffffffff,
                             0xffffffff, 0x00000000, 0x083b3571, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phr $ac0, $t0, $t1", "ac0", 0x24a3291e,
                             0x5648e540, 0xffffffff, 0xb9743941, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phr $ac1, $t2, $t3", "ac1", 0xdd91eebf,
                             0xc54f79e6, 0xffffffff, 0xbce5f924, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phr $ac2, $t4, $t1", "ac2", 0xf7ce2ec6,
                             0x5fc92974, 0xffffffff, 0xcc3c201c, t4, t1);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phr $ac3, $t6, $t7", "ac3", 0xbc1083e8,
                             0x7e08184e, 0x00000000, 0x1ebaf88e, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phr $ac0, $t5, $t3", "ac0", 0xa617cc31,
                             0x71c8315f, 0x00000000, 0x722d5e20, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phr $ac1, $t2, $t4", "ac1", 0xdfe1e8f0,
                             0x9493110e, 0xffffffff, 0xa1d6f791, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phr $ac2, $t0, $t8", "ac2", 0x31458a23,
                             0xbb246228, 0x00000000, 0x7b11bee7, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phr $ac3, $t4, $t5", "ac3", 0x848af791,
                             0x339d8d88, 0xffffffff, 0xa5631488, t4, t5);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phr $ac0, $t0, $t1", "ac0", 0xda3bacdc,
                             0x70974249, 0xffffffff, 0xb10bcc65, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phr $ac1, $t2, $t3", "ac1", 0x649d5cbd,
                             0x8a8d4e7d, 0x00000000, 0x73f39fca, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phr $ac2, $t4, $t1", "ac2", 0xc0c8c881,
                             0xeb1b4335, 0x00000000, 0x5648e540, t4, t1);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phr $ac3, $t6, $t7", "ac3", 0x7dd81a20,
                             0x0cd6b508, 0xffffffff, 0xc54f79e6, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phr $ac0, $t5, $t3", "ac0", 0x7fff7fff,
                             0x6731e282, 0x00000000, 0x5fc92974, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phr $ac1, $t2, $t4", "ac1", 0x00000555,
                             0xb6edf28f, 0x00000000, 0x7e08184e, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phr $ac2, $t0, $t8", "ac2", 0x00000000,
                             0x4b4ec9ca, 0x00000000, 0x71c8315f, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phr $ac3, $t4, $t5", "ac3", 0x80000000,
                             0xc1037fa4, 0xffffffff, 0x9493110e, t4, t5);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phr $ac1, $t2, $t4", "ac1", 0x55555555,
                             0xcb4ab48f, 0xffffffff, 0xbb246228, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phr $ac2, $t0, $t8", "ac2", 0xffff8000,
                             0xaf8f8000, 0x00000000, 0x339d8d88, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phr $ac0, $t0, $t1", "ac0", 0xabababab,
                             0x87df4510, 0x00000000, 0x70974249, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phr $ac1, $t2, $t3", "ac1", 0xfc79b4d2,
                             0xabf4e8e1, 0xffffffff, 0x8a8d4e7d, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phr $ac2, $t4, $t1", "ac2", 0x00000000,
                             0xf4c0eeac, 0xffffffff, 0xeb1b4335, t4, t1);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phr $ac3, $t6, $t7", "ac3", 0x00354565,
                             0x006a54f2, 0x00000000, 0x0cd6b508, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phr $ac0, $t5, $t3", "ac0", 0x00086755,
                             0x79f74493, 0x00000000, 0x6731e282, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phr $ac1, $t2, $t4", "ac1", 0xffff8000,
                             0x9c098000, 0xffffffff, 0xb6edf28f, t2, t4);

   printf("-------- MAQ_S.W.PHL --------\n");
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phl $ac3, $t4, $t5", "ac3", 0x00000000,
                             0x00000000, 0xffffffff, 0x80000000, t4, t5);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phl $ac0, $t0, $t1", "ac0", 0x00000004,
                             1073741824, 0x00000000, 0x00000006, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phl $ac1, $t2, $t3", "ac1", 0x80002435,
                             0x80003421, 0x00000000, 1073741824, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phl $ac3, $t6, $t7", "ac3", 0x76548000,
                             0x73468000, 0x00000000, 0x7fffffff, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phl $ac0, $t5, $t3", "ac0", 0x80000000,
                             0x80000000, 0x00000000, 0x00000001, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phl $ac1, $t2, $t4", "ac1", 0x00010001,
                             0xffffffff, 0xffffffff, 0xffffffff, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phl $ac2, $t0, $t8", "ac2", 0x7fff7fff,
                             0x7fff7fff, 0xffffffff, 0xffffffff, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phl $ac0, $t0, $t1", "ac0", 0x0000c420,
                             0x00000555, 0x00000000, 0x0fde3126, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phl $ac1, $t2, $t3", "ac1", 0x00000000,
                             0x00000000, 0x00000000, 0x55555555, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phl $ac2, $t4, $t1", "ac2", 0x80000000,
                             0x80000000, 0xffffffff, 0xffff2435, t4, t1);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phl $ac3, $t6, $t7", "ac3", 0xaaaaaaaa,
                             0x55555555, 0xffffffff, 0xabababab, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phl $ac0, $t5, $t3", "ac0", 0x00000018,
                             0xffff2435, 0xffffffff, 0xfc79b4d2, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phl $ac1, $t2, $t4", "ac1", 0xbabababa,
                             0xabababab, 0x00000000, 0x00000000, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phl $ac2, $t0, $t8", "ac2", 0xf0f0f0f0,
                             0xfc79b4d2, 0x00000000, 0x00000000, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phl $ac3, $t4, $t5", "ac3", 0xfbde3976,
                             0x00000000, 0x00000000, 0x12349876, t4, t5);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phl $ac0, $t0, $t1", "ac0", 0x23534870,
                             0x00354565, 0x00000000, 0x00354565, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phl $ac1, $t2, $t3", "ac1", 0x980b7cde,
                             0x00086755, 0x00000000, 0x00086755, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phl $ac2, $t4, $t1", "ac2", 0x00000018,
                             0x8f8f8f8f, 0xffffffff, 0x8f8f8f8f, t4, t1);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phl $ac3, $t6, $t7", "ac3", 0x92784656,
                             0xeeeeeeee, 0xffffffff, 0xeeeeeeee, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phl $ac0, $t5, $t3", "ac0", 0xcacacaca,
                             0x1bdbdbdb, 0x00000000, 0x1bdbdbdb, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phl $ac1, $t2, $t4", "ac1", 0xbacabaca,
                             0xdecadeca, 0xffffffff, 0xdecadeca, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phl $ac2, $t0, $t8", "ac2", 0x12fadeb4,
                             0x93474bde, 0xffffffff, 0x93474bde, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phl $ac3, $t4, $t5", "ac3", 0x7c000790,
                             0xfc0007ff, 0xffffffff, 0xfabfabfa, t4, t5);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phl $ac2, $t0, $t8", "ac2", 0xffffffff,
                             0xffffffff, 0x00000000, 0x083b3571, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phl $ac0, $t0, $t1", "ac0", 0x24a3291e,
                             0x5648e540, 0xffffffff, 0xb9743941, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phl $ac1, $t2, $t3", "ac1", 0xdd91eebf,
                             0xc54f79e6, 0xffffffff, 0xbce5f924, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phl $ac2, $t4, $t1", "ac2", 0xf7ce2ec6,
                             0x5fc92974, 0xffffffff, 0xcc3c201c, t4, t1);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phl $ac3, $t6, $t7", "ac3", 0xbc1083e8,
                             0x7e08184e, 0x00000000, 0x1ebaf88e, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phl $ac0, $t5, $t3", "ac0", 0xa617cc31,
                             0x71c8315f, 0x00000000, 0x722d5e20, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phl $ac1, $t2, $t4", "ac1", 0xdfe1e8f0,
                             0x9493110e, 0xffffffff, 0xa1d6f791, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phl $ac2, $t0, $t8", "ac2", 0x31458a23,
                             0xbb246228, 0x00000000, 0x7b11bee7, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phl $ac3, $t4, $t5", "ac3", 0x848af791,
                             0x339d8d88, 0xffffffff, 0xa5631488, t4, t5);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phl $ac0, $t0, $t1", "ac0", 0xda3bacdc,
                             0x70974249, 0xffffffff, 0xb10bcc65, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phl $ac1, $t2, $t3", "ac1", 0x649d5cbd,
                             0x8a8d4e7d, 0x00000000, 0x73f39fca, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phl $ac2, $t4, $t1", "ac2", 0xc0c8c881,
                             0xeb1b4335, 0x00000000, 0x5648e540, t4, t1);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phl $ac3, $t6, $t7", "ac3", 0x7dd81a20,
                             0x0cd6b508, 0xffffffff, 0xc54f79e6, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phl $ac0, $t5, $t3", "ac0", 0x7fff7fff,
                             0x6731e282, 0x00000000, 0x5fc92974, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phl $ac1, $t2, $t4", "ac1", 0x00000555,
                             0xb6edf28f, 0x00000000, 0x7e08184e, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phl $ac2, $t0, $t8", "ac2", 0x00000000,
                             0x4b4ec9ca, 0x00000000, 0x71c8315f, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phl $ac3, $t4, $t5", "ac3", 0x80000000,
                             0xc1037fa4, 0xffffffff, 0x9493110e, t4, t5);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phl $ac1, $t2, $t4", "ac1", 0x55555555,
                             0xcb4ab48f, 0xffffffff, 0xbb246228, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phl $ac2, $t0, $t8", "ac2", 0xffff8000,
                             0xaf8f8000, 0x00000000, 0x339d8d88, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phl $ac0, $t0, $t1", "ac0", 0xabababab,
                             0x87df4510, 0x00000000, 0x70974249, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phl $ac1, $t2, $t3", "ac1", 0xfc79b4d2,
                             0xabf4e8e1, 0xffffffff, 0x8a8d4e7d, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phl $ac2, $t4, $t1", "ac2", 0x00000000,
                             0xf4c0eeac, 0xffffffff, 0xeb1b4335, t4, t1);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phl $ac3, $t6, $t7", "ac3", 0x00354565,
                             0x006a54f2, 0x00000000, 0x0cd6b508, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phl $ac0, $t5, $t3", "ac0", 0x00086755,
                             0x79f74493, 0x00000000, 0x6731e282, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("maq_s.w.phl $ac1, $t2, $t4", "ac1", 0xffff8000,
                             0x9c098000, 0xffffffff, 0xb6edf28f, t2, t4);


   printf("-------- MAQ_SA.W.PHL --------\n");
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phl $ac3, $t4, $t5", "ac3", 0x00000000,
                             0x00000000, 0xffffffff, 0x80000000, t4, t5);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phl $ac0, $t0, $t1", "ac0", 0x00000004,
                             1073741824, 0x00000000, 0x00000006, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phl $ac1, $t2, $t3", "ac1", 0x80002435,
                             0x80003421, 0x00000000, 1073741824, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phl $ac3, $t6, $t7", "ac3", 0x76548000,
                             0x73468000, 0x00000000, 0x7fffffff, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phl $ac0, $t5, $t3", "ac0", 0x80000000,
                             0x80000000, 0x00000000, 0x00000001, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phl $ac1, $t2, $t4", "ac1", 0x00010001,
                             0xffffffff, 0xffffffff, 0xffffffff, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phl $ac2, $t0, $t8", "ac2", 0x7fff7fff,
                             0x7fff7fff, 0xffffffff, 0xffffffff, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phl $ac0, $t0, $t1", "ac0", 0x0000c420,
                             0x00000555, 0x00000000, 0x0fde3126, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phl $ac1, $t2, $t3", "ac1", 0x00000000,
                             0x00000000, 0x00000000, 0x55555555, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phl $ac2, $t4, $t1", "ac2", 0x80000000,
                             0x80000000, 0xffffffff, 0xffff2435, t4, t1);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phl $ac3, $t6, $t7", "ac3", 0xaaaaaaaa,
                             0x55555555, 0xffffffff, 0xabababab, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phl $ac0, $t5, $t3", "ac0", 0x00000018,
                             0xffff2435, 0xffffffff, 0xfc79b4d2, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phl $ac1, $t2, $t4", "ac1", 0xbabababa,
                             0xabababab, 0x00000000, 0x00000000, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phl $ac2, $t0, $t8", "ac2", 0xf0f0f0f0,
                             0xfc79b4d2, 0x00000000, 0x00000000, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phl $ac3, $t4, $t5", "ac3", 0xfbde3976,
                             0x00000000, 0x00000000, 0x12349876, t4, t5);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phl $ac0, $t0, $t1", "ac0", 0x23534870,
                             0x00354565, 0x00000000, 0x00354565, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phl $ac1, $t2, $t3", "ac1", 0x980b7cde,
                             0x00086755, 0x00000000, 0x00086755, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phl $ac2, $t4, $t1", "ac2", 0x00000018,
                             0x8f8f8f8f, 0xffffffff, 0x8f8f8f8f, t4, t1);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phl $ac3, $t6, $t7", "ac3", 0x92784656,
                             0xeeeeeeee, 0xffffffff, 0xeeeeeeee, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phl $ac0, $t5, $t3", "ac0", 0xcacacaca,
                             0x1bdbdbdb, 0x00000000, 0x1bdbdbdb, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phl $ac1, $t2, $t4", "ac1", 0xbacabaca,
                             0xdecadeca, 0xffffffff, 0xdecadeca, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phl $ac2, $t0, $t8", "ac2", 0x12fadeb4,
                             0x93474bde, 0xffffffff, 0x93474bde, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phl $ac3, $t4, $t5", "ac3", 0x7c000790,
                             0xfc0007ff, 0xffffffff, 0xfabfabfa, t4, t5);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phl $ac2, $t0, $t8", "ac2", 0xffffffff,
                             0xffffffff, 0x00000000, 0x083b3571, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phl $ac0, $t0, $t1", "ac0", 0x24a3291e,
                             0x5648e540, 0xffffffff, 0xb9743941, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phl $ac1, $t2, $t3", "ac1", 0xdd91eebf,
                             0xc54f79e6, 0xffffffff, 0xbce5f924, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phl $ac2, $t4, $t1", "ac2", 0xf7ce2ec6,
                             0x5fc92974, 0xffffffff, 0xcc3c201c, t4, t1);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phl $ac3, $t6, $t7", "ac3", 0xbc1083e8,
                             0x7e08184e, 0x00000000, 0x1ebaf88e, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phl $ac0, $t5, $t3", "ac0", 0xa617cc31,
                             0x71c8315f, 0x00000000, 0x722d5e20, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phl $ac1, $t2, $t4", "ac1", 0xdfe1e8f0,
                             0x9493110e, 0xffffffff, 0xa1d6f791, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phl $ac2, $t0, $t8", "ac2", 0x31458a23,
                             0xbb246228, 0x00000000, 0x7b11bee7, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phl $ac3, $t4, $t5", "ac3", 0x848af791,
                             0x339d8d88, 0xffffffff, 0xa5631488, t4, t5);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phl $ac0, $t0, $t1", "ac0", 0xda3bacdc,
                             0x70974249, 0xffffffff, 0xb10bcc65, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phl $ac1, $t2, $t3", "ac1", 0x649d5cbd,
                             0x8a8d4e7d, 0x00000000, 0x73f39fca, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phl $ac2, $t4, $t1", "ac2", 0xc0c8c881,
                             0xeb1b4335, 0x00000000, 0x5648e540, t4, t1);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phl $ac3, $t6, $t7", "ac3", 0x7dd81a20,
                             0x0cd6b508, 0xffffffff, 0xc54f79e6, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phl $ac0, $t5, $t3", "ac0", 0x7fff7fff,
                             0x6731e282, 0x00000000, 0x5fc92974, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phl $ac1, $t2, $t4", "ac1", 0x00000555,
                             0xb6edf28f, 0x00000000, 0x7e08184e, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phl $ac2, $t0, $t8", "ac2", 0x00000000,
                             0x4b4ec9ca, 0x00000000, 0x71c8315f, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phl $ac3, $t4, $t5", "ac3", 0x80000000,
                             0xc1037fa4, 0xffffffff, 0x9493110e, t4, t5);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phl $ac1, $t2, $t4", "ac1", 0x55555555,
                             0xcb4ab48f, 0xffffffff, 0xbb246228, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phl $ac2, $t0, $t8", "ac2", 0xffff8000,
                             0xaf8f8000, 0x00000000, 0x339d8d88, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phl $ac0, $t0, $t1", "ac0", 0xabababab,
                             0x87df4510, 0x00000000, 0x70974249, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phl $ac1, $t2, $t3", "ac1", 0xfc79b4d2,
                             0xabf4e8e1, 0xffffffff, 0x8a8d4e7d, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phl $ac2, $t4, $t1", "ac2", 0x00000000,
                             0xf4c0eeac, 0xffffffff, 0xeb1b4335, t4, t1);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phl $ac3, $t6, $t7", "ac3", 0x00354565,
                             0x006a54f2, 0x00000000, 0x0cd6b508, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phl $ac0, $t5, $t3", "ac0", 0x00086755,
                             0x79f74493, 0x00000000, 0x6731e282, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("maq_sa.w.phl $ac1, $t2, $t4", "ac1", 0xffff8000,
                             0x9c098000, 0xffffffff, 0xb6edf28f, t2, t4);

   printf("-------- MTHI, MTLO, MFHI, MFLO --------\n");
   TESTDSPINST_HILO("ac0", 0x00000000, 0x00000006);
   TESTDSPINST_HILO("ac1", 0x00000055, 0x00000286);
   TESTDSPINST_HILO("ac2", 0x00000018, 0x00000fff);
   TESTDSPINST_HILO("ac3", 0x7fffffff, 0x7fffffff);
   TESTDSPINST_HILO("ac0", 0xffffffff, 0x00000001);
   TESTDSPINST_HILO("ac1", 0x00000001, 0xffffffff);
   TESTDSPINST_HILO("ac2", 0x00000002, 0x00000006);
   TESTDSPINST_HILO("ac3", 0x00000356, 0x00000555);

   printf("-------- MODSUB --------\n");
   TESTDSPINST_RD_RS_RT_NODSPC("modsub $t0, $t1, $t2", 0x00000000, 0x00000000,
                               t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("modsub $t2, $t3, $t4", 0x00045fb2, 0x00000286,
                               t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("modsub $t4, $t1, $t5", 0x00002435, 0xffff3421,
                               t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("modsub $t6, $t7, $t3", 0x07654cb8, 0x734680bc,
                               t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("modsub $t5, $t3, $t2", 0xf973437b, 0x80000000,
                               t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("modsub $t2, $t4, $t8", 0x00010001, 0xffffffff,
                               t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("modsub $t0, $t8, $t0", 0x7fff7fff, 0x7fff7fff,
                               t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("modsub $t4, $t6, $t1", 0x0000c420, 0x00000555,
                               t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("modsub $t2, $t3, $t4", 0x00000004, 1073741824,
                               t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("modsub $t4, $t1, $t5", 0x80002435, 0x80003421,
                               t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("modsub $t6, $t7, $t3", 0x76548000, 0x73468000,
                               t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("modsub $t5, $t3, $t2", 0x80000000, 0x80000000,
                               t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("modsub $t2, $t4, $t8", 0x00010001, 0xffffffff,
                               t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("modsub $t0, $t8, $t0", 0x7fff7fff, 0x7fff7fff,
                               t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("modsub $t4, $t6, $t1", 0x0000c420, 0x00000555,
                               t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("modsub $t0, $t1, $t2", 0x00000000, 0x00000000,
                               t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("modsub $t2, $t3, $t4", 0x80000000, 0x80000000,
                               t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("modsub $t4, $t1, $t5", 0xaaaaaaaa, 0x55555555,
                               t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("modsub $t6, $t7, $t3", 0x00000018, 0xffff2435,
                               t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("modsub $t5, $t3, $t2", 0xbabababa, 0xabababab,
                               t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("modsub $t2, $t4, $t8", 0xf0f0f0f0, 0xfc79b4d2,
                               t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("modsub $t0, $t8, $t0", 0xfbde3976, 0x00000000,
                               t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("modsub $t4, $t6, $t1", 0x23534870, 0x00354565,
                               t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("modsub $t0, $t1, $t2", 0x980b7cde, 0x00086755,
                               t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("modsub $t2, $t3, $t4", 0x00000018, 0x8f8f8f8f,
                               t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("modsub $t4, $t1, $t5", 0x92784656, 0xeeeeeeee,
                               t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("modsub $t6, $t7, $t3", 0xcacacaca, 0x1bdbdbdb,
                               t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("modsub $t5, $t3, $t2", 0xbacabaca, 0xdecadeca,
                               t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("modsub $t2, $t4, $t8", 0x12fadeb4, 0x93474bde,
                               t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("modsub $t0, $t8, $t0", 0x7c000790, 0xfc0007ff,
                               t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("modsub $t4, $t6, $t1", 0xffffffff, 0xffffffff,
                               t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("modsub $t0, $t1, $t2", 0xf2f4df1f, 0xcb4ab48f,
                               t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("modsub $t2, $t3, $t4", 0x435f909a, 0xaf8f7e18,
                               t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("modsub $t4, $t1, $t5", 0x2106ba5f, 0x87df4510,
                               t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("modsub $t6, $t7, $t3", 0x246a6376, 0xabf4e8e1,
                               t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("modsub $t5, $t3, $t2", 0x1046a1a3, 0xf4c0eeac,
                               t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("modsub $t2, $t4, $t8", 0x638ca515, 0x006a54f2,
                               t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("modsub $t0, $t8, $t0", 0xf63e7a9d, 0x79f74493,
                               t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("modsub $t4, $t6, $t1", 0xbd6845cd, 0x9c09e313,
                               t4, t6, t1);

   printf("-------- MTHLIP --------\n");
   TESTDSPINST_MTHLIP("mthlip $t1, $ac1", "ac1", 0xda3bacdc, 0x70974249,
                      0x00000000, t1,  7);
   TESTDSPINST_MTHLIP("mthlip $t2, $ac2", "ac2", 0x2fff0000, 0x00000001,
                      0xe87927cc, t2,  2);
   TESTDSPINST_MTHLIP("mthlip $t1, $ac1", "ac1", 0x2fff0000, 0x73741802,
                      0x083b3571, t1,  4);
   TESTDSPINST_MTHLIP("mthlip $t2, $ac2", "ac2", 0x2fff0000, 0x80003403,
                      0xb9743941, t2, 13);
   TESTDSPINST_MTHLIP("mthlip $t1, $ac1", "ac1", 0xff460000, 0x73468004,
                      0xbce5f924, t1,  5);
   TESTDSPINST_MTHLIP("mthlip $t2, $ac2", "ac2", 0x00008000, 0x80000000,
                      0xcc3c201c, t2, 22);
   TESTDSPINST_MTHLIP("mthlip $t0, $ac0", "ac0", 0x00010001, 0xffffff07,
                      0x1ebaf88e, t0, 31);
   TESTDSPINST_MTHLIP("mthlip $t3, $ac3", "ac3", 0x7fff7fff, 0x7fff7f07,
                      0x722d5e20, t3, 28);
   TESTDSPINST_MTHLIP("mthlip $t6, $ac1", "ac1", 0xffffffff, 0x00000505,
                      0xa1d6f791, t6,  0);
   TESTDSPINST_MTHLIP("mthlip $t4, $ac3", "ac3", 0xabababab, 0x00000000,
                      0x7b11bee7, t4, 26);
   TESTDSPINST_MTHLIP("mthlip $t1, $ac0", "ac0", 0xdecadeca, 0x80000000,
                      0xa5631488, t1, 12);
   TESTDSPINST_MTHLIP("mthlip $t2, $ac1", "ac1", 0xbacabaca, 0x55555555,
                      0xb10bcc65, t2,  1);
   TESTDSPINST_MTHLIP("mthlip $t3, $ac2", "ac2", 0x3545ff80, 0xffff2434,
                      0x73f39fca, t3, 31);
   TESTDSPINST_MTHLIP("mthlip $t5, $ac0", "ac0", 0x734680bc, 0xabababa3,
                      0x5648e540, t5, 11);
   TESTDSPINST_MTHLIP("mthlip $t7, $ac2", "ac2", 0xc4dbfe20, 0xfc79b4d2,
                      0xc54f79e6, t7, 16);
   TESTDSPINST_MTHLIP("mthlip $t8, $ac3", "ac3", 0x00000000, 0x00000000,
                      0x5fc92974, t8, 29);
   TESTDSPINST_MTHLIP("mthlip $t4, $ac0", "ac0", 0x55555555, 0x00354561,
                      0x7e08184e, t4, 13);
   TESTDSPINST_MTHLIP("mthlip $t5, $ac1", "ac1", 0xad80bce4, 0x00086755,
                      0x71c8315f, t5,  8);
   TESTDSPINST_MTHLIP("mthlip $t6, $ac2", "ac2", 0x7f003245, 0x8f8f8f8f,
                      0x9493110e, t6,  7);
   TESTDSPINST_MTHLIP("mthlip $t7, $ac3", "ac3", 0x93474bde, 0xeeeeeeee,
                      0xbb246228, t7, 21);
   TESTDSPINST_MTHLIP("mthlip $t8, $ac0", "ac0", 0xf97343ff, 0x1bdbdbdb,
                      0x339d8d88, t8,  4);
   TESTDSPINST_MTHLIP("mthlip $t0, $ac1", "ac1", 0x980b7cde, 0xdecadeca,
                      0x70974249, t0, 32);
   TESTDSPINST_MTHLIP("mthlip $t1, $ac2", "ac2", 0x0555adec, 0x93474bde,
                      0x339d8d88, t1, 33);
   TESTDSPINST_MTHLIP("mthlip $t1, $ac0", "ac0", 0x00000000, 0x0cd6b508,
                      0x12349876, t1, 12);
   TESTDSPINST_MTHLIP("mthlip $t2, $ac1", "ac1", 0x00000000, 0x6731e282,
                      0x00354565, t2,  1);
   TESTDSPINST_MTHLIP("mthlip $t3, $ac2", "ac2", 0xffffffff, 0xb6edf28f,
                      0x00086755, t3, 31);
   TESTDSPINST_MTHLIP("mthlip $t5, $ac0", "ac0", 0x00000000, 0x4b4ec9ca,
                      0x8f8f8f8f, t5, 11);
   TESTDSPINST_MTHLIP("mthlip $t7, $ac2", "ac2", 0xffffffff, 0xc1037fa4,
                      0xeeeeeeee, t7, 16);
   TESTDSPINST_MTHLIP("mthlip $t8, $ac3", "ac3", 0xffffffff, 0xcb4ab48f,
                      0x1bdbdbdb, t8, 29);
   TESTDSPINST_MTHLIP("mthlip $t4, $ac0", "ac0", 0xffffffff, 0xaf8f7e18,
                      0xbb246228, t4, 13);
   TESTDSPINST_MTHLIP("mthlip $t5, $ac1", "ac1", 0xffffffff, 0x87df4510,
                      0x339d8d88, t5,  8);
   TESTDSPINST_MTHLIP("mthlip $t6, $ac2", "ac2", 0xffffffff, 0xabf4e8e1,
                      0x70974249, t6,  7);
   TESTDSPINST_MTHLIP("mthlip $t7, $ac3", "ac3", 0xffffffff, 0xf4c0eeac,
                      0x8a8d4e7d, t7, 21);
   TESTDSPINST_MTHLIP("mthlip $t8, $ac0", "ac0", 0x00000000, 0x006a54f2,
                      0xeb1b4335, t8,  4);
   TESTDSPINST_MTHLIP("mthlip $t0, $ac1", "ac1", 0x00000000, 0x79f74493,
                      0x0cd6b508, t0, 32);
   TESTDSPINST_MTHLIP("mthlip $t1, $ac2", "ac2", 0xffffffff, 0x9c09e313,
                      0x6731e282, t1, 33);

   printf("-------- MULEQ_S.W.PHL --------\n");
   TESTDSPINST_RD_RS_RT_DSPC("muleq_s.w.phl $t0, $t1, $t2", 0x00000000,
                             0x00000000, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("muleq_s.w.phl $t2, $t3, $t4", 0x00045fb2,
                             0x00000286, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("muleq_s.w.phl $t4, $t1, $t5", 0x80002435,
                             0x80003421, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("muleq_s.w.phl $t6, $t7, $t3", 0x07654cb8,
                             0x734680bc, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("muleq_s.w.phl $t5, $t3, $t2", 0xf973437b,
                             0x80000000, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("muleq_s.w.phl $t2, $t4, $t8", 0x00010001,
                             0xffffffff, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("muleq_s.w.phl $t0, $t8, $t0", 0x7fff7fff,
                             0x7fff7fff, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("muleq_s.w.phl $t4, $t6, $t1", 0x0000c420,
                             0x00000555, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("muleq_s.w.phl $t2, $t3, $t4", 0x80000000,
                             0x80000000, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("muleq_s.w.phl $t4, $t1, $t5", 0xaaaaaaaa,
                             0x55555555, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("muleq_s.w.phl $t6, $t7, $t3", 0x00000018,
                             0xffff2435, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("muleq_s.w.phl $t5, $t3, $t2", 0xbabababa,
                             0xabababab, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("muleq_s.w.phl $t2, $t4, $t8", 0xf0f0f0f0,
                             0xfc79b4d2, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("muleq_s.w.phl $t0, $t8, $t0", 0xfbde3976,
                             0x00000000, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("muleq_s.w.phl $t4, $t6, $t1", 0x23534870,
                             0x00354565, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("muleq_s.w.phl $t0, $t1, $t2", 0x980b7cde,
                             0x00086755, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("muleq_s.w.phl $t2, $t3, $t4", 0x00000018,
                             0x8f8f8f8f, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("muleq_s.w.phl $t4, $t1, $t5", 0x92784656,
                             0xeeeeeeee, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("muleq_s.w.phl $t6, $t7, $t3", 0xcacacaca,
                             0x1bdbdbdb, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("muleq_s.w.phl $t5, $t3, $t2", 0xbacabaca,
                             0xdecadeca, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("muleq_s.w.phl $t2, $t4, $t8", 0x12fadeb4,
                             0x93474bde, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("muleq_s.w.phl $t0, $t8, $t0", 0x7c000790,
                             0xfc0007ff, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("muleq_s.w.phl $t4, $t6, $t1", 0xffffffff,
                             0xffffffff, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("muleq_s.w.phl $t0, $t1, $t2", 0xf2f4df1f,
                             0xcb4ab48f, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("muleq_s.w.phl $t2, $t3, $t4", 0x435f909a,
                             0xaf8f7e18, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("muleq_s.w.phl $t4, $t1, $t5", 0x2106ba5f,
                             0x87df4510, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("muleq_s.w.phl $t6, $t7, $t3", 0x246a6376,
                             0xabf4e8e1, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("muleq_s.w.phl $t5, $t3, $t2", 0x1046a1a3,
                             0xf4c0eeac, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("muleq_s.w.phl $t2, $t4, $t8", 0x638ca515,
                             0x006a54f2, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("muleq_s.w.phl $t0, $t8, $t0", 0xf63e7a9d,
                             0x79f74493, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("muleq_s.w.phl $t4, $t6, $t1", 0xbd6845cd,
                             0x9c09e313, t4, t6, t1);

   printf("-------- MULEQ_S.W.PHR --------\n");
   TESTDSPINST_RD_RS_RT_DSPC("muleq_s.w.phr $t0, $t1, $t2", 0x00000000,
                             0x00000000, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("muleq_s.w.phr $t2, $t3, $t4", 0x00045fb2,
                             0x00000286, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("muleq_s.w.phr $t4, $t1, $t5", 0x80002435,
                             0x80003421, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("muleq_s.w.phr $t6, $t7, $t3", 0x07654cb8,
                             0x734680bc, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("muleq_s.w.phr $t5, $t3, $t2", 0xf973437b,
                             0x80000000, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("muleq_s.w.phr $t2, $t4, $t8", 0x00010001,
                             0xffffffff, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("muleq_s.w.phr $t0, $t8, $t0", 0x7fff7fff,
                             0x7fff7fff, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("muleq_s.w.phr $t4, $t6, $t1", 0x0000c420,
                             0x00000555, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("muleq_s.w.phr $t2, $t3, $t4", 0x80000000,
                             0x80000000, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("muleq_s.w.phr $t4, $t1, $t5", 0xaaaaaaaa,
                             0x55555555, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("muleq_s.w.phr $t6, $t7, $t3", 0x00000018,
                             0xffff2435, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("muleq_s.w.phr $t5, $t3, $t2", 0xbabababa,
                             0xabababab, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("muleq_s.w.phr $t2, $t4, $t8", 0xf0f0f0f0,
                             0xfc79b4d2, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("muleq_s.w.phr $t0, $t8, $t0", 0xfbde3976,
                             0x00000000, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("muleq_s.w.phr $t4, $t6, $t1", 0x23534870,
                             0x00354565, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("muleq_s.w.phr $t0, $t1, $t2", 0x980b7cde,
                             0x00086755, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("muleq_s.w.phr $t2, $t3, $t4", 0x00000018,
                             0x8f8f8f8f, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("muleq_s.w.phr $t4, $t1, $t5", 0x92784656,
                             0xeeeeeeee, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("muleq_s.w.phr $t6, $t7, $t3", 0xcacacaca,
                             0x1bdbdbdb, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("muleq_s.w.phr $t5, $t3, $t2", 0xbacabaca,
                             0xdecadeca, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("muleq_s.w.phr $t2, $t4, $t8", 0x12fadeb4,
                             0x93474bde, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("muleq_s.w.phr $t0, $t8, $t0", 0x7c000790,
                             0xfc0007ff, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("muleq_s.w.phr $t4, $t6, $t1", 0xffffffff,
                             0xffffffff, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("muleq_s.w.phr $t0, $t1, $t2", 0xf2f4df1f,
                             0xcb4ab48f, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("muleq_s.w.phr $t2, $t3, $t4", 0x435f909a,
                             0xaf8f7e18, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("muleq_s.w.phr $t4, $t1, $t5", 0x2106ba5f,
                             0x87df4510, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("muleq_s.w.phr $t6, $t7, $t3", 0x246a6376,
                             0xabf4e8e1, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("muleq_s.w.phr $t5, $t3, $t2", 0x1046a1a3,
                             0xf4c0eeac, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("muleq_s.w.phr $t2, $t4, $t8", 0x638ca515,
                             0x006a54f2, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("muleq_s.w.phr $t0, $t8, $t0", 0xf63e7a9d,
                             0x79f74493, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("muleq_s.w.phr $t4, $t6, $t1", 0xbd6845cd,
                             0x9c09e313, t4, t6, t1);

   printf("-------- MULEU_S.PH.QBL --------\n");
   TESTDSPINST_RD_RS_RT_DSPC("muleu_s.ph.qbl $t0, $t1, $t2", 0x00000000,
                             0x00000000, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("muleu_s.ph.qbl $t2, $t3, $t4", 0x00045fb2,
                             0x00000286, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("muleu_s.ph.qbl $t4, $t1, $t5", 0x80002435,
                             0x80003421, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("muleu_s.ph.qbl $t6, $t7, $t3", 0x07654cb8,
                             0x734680bc, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("muleu_s.ph.qbl $t5, $t3, $t2", 0xf973437b,
                             0x80000000, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("muleu_s.ph.qbl $t2, $t4, $t8", 0x00010001,
                             0xffffffff, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("muleu_s.ph.qbl $t0, $t8, $t0", 0x7fff7fff,
                             0x7fff7fff, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("muleu_s.ph.qbl $t4, $t6, $t1", 0x0000c420,
                             0x00000555, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("muleu_s.ph.qbl $t2, $t3, $t4", 0x80000000,
                             0x80000000, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("muleu_s.ph.qbl $t4, $t1, $t5", 0xaaaa8000,
                             0x55558000, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("muleu_s.ph.qbl $t6, $t7, $t3", 0x00000018,
                             0xffff2435, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("muleu_s.ph.qbl $t5, $t3, $t2", 0xbabababa,
                             0xabababab, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("muleu_s.ph.qbl $t2, $t4, $t8", 0xf0f0f0f0,
                             0xfc79b4d2, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("muleu_s.ph.qbl $t0, $t8, $t0", 0xfbde3976,
                             0x00000000, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("muleu_s.ph.qbl $t4, $t6, $t1", 0x23534870,
                             0x00354565, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("muleu_s.ph.qbl $t0, $t1, $t2", 0x980b7cde,
                             0x00086755, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("muleu_s.ph.qbl $t2, $t3, $t4", 0x00000018,
                             0x8f8f8f8f, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("muleu_s.ph.qbl $t4, $t1, $t5", 0x92784656,
                             0xeeeeeeee, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("muleu_s.ph.qbl $t6, $t7, $t3", 0xcacacaca,
                             0x1bdbdbdb, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("muleu_s.ph.qbl $t5, $t3, $t2", 0xbacabaca,
                             0xdecadeca, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("muleu_s.ph.qbl $t2, $t4, $t8", 0x12fadeb4,
                             0x93474bde, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("muleu_s.ph.qbl $t0, $t8, $t0", 0x7c000790,
                             0xfc0007ff, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("muleu_s.ph.qbl $t4, $t6, $t1", 0xffffffff,
                             0xffffffff, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("muleu_s.ph.qbl $t0, $t1, $t2", 0xffffffff,
                             0xcb4ab48f, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("muleu_s.ph.qbl $t2, $t3, $t4", 0xffffffff,
                             0xaf8f7e18, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("muleu_s.ph.qbl $t4, $t1, $t5", 0xffffffff,
                             0x87df4510, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("muleu_s.ph.qbl $t6, $t7, $t3", 0xffffffff,
                             0xabf4e8e1, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("muleu_s.ph.qbl $t5, $t3, $t2", 0xffffffff,
                             0xf4c0eeac, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("muleu_s.ph.qbl $t2, $t4, $t8", 0x00000000,
                             0x006a54f2, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("muleu_s.ph.qbl $t0, $t8, $t0", 0x00000000,
                             0x79f74493, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("muleu_s.ph.qbl $t4, $t6, $t1", 0xffffffff,
                             0x9c09e313, t4, t6, t1);

   printf("-------- MULEU_S.PH.QBR --------\n");
   TESTDSPINST_RD_RS_RT_DSPC("muleu_s.ph.qbr $t0, $t1, $t2", 0x00000000,
                             0x00000000, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("muleu_s.ph.qbr $t2, $t3, $t4", 0x00045fb2,
                             0x00000286, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("muleu_s.ph.qbr $t4, $t1, $t5", 0x80002435,
                             0x80003421, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("muleu_s.ph.qbr $t6, $t7, $t3", 0x07654cb8,
                             0x734680bc, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("muleu_s.ph.qbr $t5, $t3, $t2", 0xf973437b,
                             0x80000000, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("muleu_s.ph.qbr $t2, $t4, $t8", 0x00010001,
                             0xffffffff, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("muleu_s.ph.qbr $t0, $t8, $t0", 0x7fff7fff,
                             0x7fff7fff, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("muleu_s.ph.qbr $t4, $t6, $t1", 0x0000c420,
                             0x00000555, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("muleu_s.ph.qbr $t2, $t3, $t4", 0x80000000,
                             0x80000000, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("muleu_s.ph.qbr $t4, $t1, $t5", 0xaaaa8000,
                             0x55558000, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("muleu_s.ph.qbr $t6, $t7, $t3", 0x00000018,
                             0xffff2435, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("muleu_s.ph.qbr $t5, $t3, $t2", 0xbabababa,
                             0xabababab, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("muleu_s.ph.qbr $t2, $t4, $t8", 0xf0f0f0f0,
                             0xfc79b4d2, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("muleu_s.ph.qbr $t0, $t8, $t0", 0xfbde3976,
                             0x00000000, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("muleu_s.ph.qbr $t4, $t6, $t1", 0x23534870,
                             0x00354565, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("muleu_s.ph.qbr $t0, $t1, $t2", 0x980b7cde,
                             0x00086755, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("muleu_s.ph.qbr $t2, $t3, $t4", 0x00000018,
                             0x8f8f8f8f, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("muleu_s.ph.qbr $t4, $t1, $t5", 0x92784656,
                             0xeeeeeeee, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("muleu_s.ph.qbr $t6, $t7, $t3", 0xcacacaca,
                             0x1bdbdbdb, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("muleu_s.ph.qbr $t5, $t3, $t2", 0xbacabaca,
                             0xdecadeca, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("muleu_s.ph.qbr $t2, $t4, $t8", 0x12fadeb4,
                             0x93474bde, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("muleu_s.ph.qbr $t0, $t8, $t0", 0x7c000790,
                             0xfc0007ff, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("muleu_s.ph.qbr $t4, $t6, $t1", 0xffffffff,
                             0xffffffff, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("muleu_s.ph.qbr $t0, $t1, $t2", 0xffffffff,
                             0xcb4ab48f, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("muleu_s.ph.qbr $t2, $t3, $t4", 0xffffffff,
                             0xaf8f7e18, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("muleu_s.ph.qbr $t4, $t1, $t5", 0xffffffff,
                             0x87df4510, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("muleu_s.ph.qbr $t6, $t7, $t3", 0xffffffff,
                             0xabf4e8e1, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("muleu_s.ph.qbr $t5, $t3, $t2", 0xffffffff,
                             0xf4c0eeac, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("muleu_s.ph.qbr $t2, $t4, $t8", 0x00000000,
                             0x006a54f2, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("muleu_s.ph.qbr $t0, $t8, $t0", 0x00000000,
                             0x79f74493, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("muleu_s.ph.qbr $t4, $t6, $t1", 0xffffffff,
                             0x9c09e313, t4, t6, t1);

   printf("-------- MULQ_RS.PH --------\n");
   TESTDSPINST_RD_RS_RT_DSPC("mulq_rs.ph $t0, $t1, $t2", 0x00000000, 0x00000000,
                             t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_rs.ph $t2, $t3, $t4", 0x00045fb2, 0x00000286,
                             t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_rs.ph $t4, $t1, $t5", 0x80002435, 0x80003421,
                             t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_rs.ph $t6, $t7, $t3", 0x07654cb8, 0x734680bc,
                             t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_rs.ph $t5, $t3, $t2", 0xf973437b, 0x80000000,
                             t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_rs.ph $t2, $t4, $t8", 0x00010001, 0xffffffff,
                             t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_rs.ph $t0, $t8, $t0", 0x7fff7fff, 0x7fff7fff,
                             t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_rs.ph $t4, $t6, $t1", 0x0000c420, 0x00000555,
                             t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_rs.ph $t2, $t3, $t4", 0x80000000, 0x80000000,
                             t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_rs.ph $t4, $t1, $t5", 0xaaaa8000, 0x55558000,
                             t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_rs.ph $t6, $t7, $t3", 0x00000018, 0xffff2435,
                             t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_rs.ph $t5, $t3, $t2", 0xbabababa, 0xabababab,
                             t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_rs.ph $t2, $t4, $t8", 0xf0f0f0f0, 0xfc79b4d2,
                             t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_rs.ph $t0, $t8, $t0", 0xfbde3976, 0x00000000,
                             t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_rs.ph $t4, $t6, $t1", 0x23534870, 0x00354565,
                             t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_rs.ph $t0, $t1, $t2", 0x980b7cde, 0x00086755,
                             t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_rs.ph $t2, $t3, $t4", 0x00000018, 0x8f8f8f8f,
                             t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_rs.ph $t4, $t1, $t5", 0x92784656, 0xeeeeeeee,
                             t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_rs.ph $t6, $t7, $t3", 0xcacacaca, 0x1bdbdbdb,
                             t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_rs.ph $t5, $t3, $t2", 0xbacabaca, 0xdecadeca,
                             t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_rs.ph $t2, $t4, $t8", 0x12fadeb4, 0x93474bde,
                             t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_rs.ph $t0, $t8, $t0", 0x7c000790, 0xfc0007ff,
                             t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_rs.ph $t4, $t6, $t1", 0xffffffff, 0xffffffff,
                             t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_rs.ph $t0, $t1, $t2", 0xf2f4df1f, 0xcb4ab48f,
                             t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_rs.ph $t2, $t3, $t4", 0x435f909a, 0xaf8f7e18,
                             t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_rs.ph $t4, $t1, $t5", 0x2106ba5f, 0x87df4510,
                             t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_rs.ph $t6, $t7, $t3", 0x246a6376, 0xabf4e8e1,
                             t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_rs.ph $t5, $t3, $t2", 0x1046a1a3, 0xf4c0eeac,
                             t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_rs.ph $t2, $t4, $t8", 0x638ca515, 0x006a54f2,
                             t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_rs.ph $t0, $t8, $t0", 0xf63e7a9d, 0x79f74493,
                             t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_rs.ph $t4, $t6, $t1", 0xbd6845cd, 0x9c09e313,
                             t4, t6, t1);

   printf("-------- MULSAQ_S.W.PH --------\n");
   TESTDSPINST_AC_RS_RT_DSPC("mulsaq_s.w.ph $ac3, $t4, $t5", "ac3", 0x00000000,
                             0x00000000, 0xffffffff, 0x80000000, t4, t5);
   TESTDSPINST_AC_RS_RT_DSPC("mulsaq_s.w.ph $ac0, $t0, $t1", "ac0", 0x00000004,
                             1073741824, 0x00000000, 0x00000006, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("mulsaq_s.w.ph $ac1, $t2, $t3", "ac1", 0x80002435,
                             0x80003421, 0x00000000, 1073741824, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("mulsaq_s.w.ph $ac3, $t6, $t7", "ac3", 0x76548000,
                             0x73468000, 0x00000000, 0x7fffffff, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("mulsaq_s.w.ph $ac0, $t5, $t3", "ac0", 0x80000000,
                             0x80000000, 0x00000000, 0x00000001, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("mulsaq_s.w.ph $ac1, $t2, $t4", "ac1", 0x00010001,
                             0xffffffff, 0xffffffff, 0xffffffff, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("mulsaq_s.w.ph $ac2, $t0, $t8", "ac2", 0x7fff7fff,
                             0x7fff7fff, 0xffffffff, 0xffffffff, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("mulsaq_s.w.ph $ac0, $t0, $t1", "ac0", 0x0000c420,
                             0x00000555, 0x00000000, 0x0fde3126, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("mulsaq_s.w.ph $ac1, $t2, $t3", "ac1", 0x00000000,
                             0x00000000, 0x00000000, 0x55555555, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("mulsaq_s.w.ph $ac2, $t4, $t1", "ac2", 0x80000000,
                             0x80000000, 0xffffffff, 0xffff2435, t4, t1);
   TESTDSPINST_AC_RS_RT_DSPC("mulsaq_s.w.ph $ac3, $t6, $t7", "ac3", 0xaaaaaaaa,
                             0x55555555, 0xffffffff, 0xabababab, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("mulsaq_s.w.ph $ac0, $t5, $t3", "ac0", 0x00000018,
                             0xffff2435, 0xffffffff, 0xfc79b4d2, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("mulsaq_s.w.ph $ac1, $t2, $t4", "ac1", 0xbabababa,
                             0xabababab, 0x00000000, 0x00000000, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("mulsaq_s.w.ph $ac2, $t0, $t8", "ac2", 0xf0f0f0f0,
                             0xfc79b4d2, 0x00000000, 0x00000000, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("mulsaq_s.w.ph $ac3, $t4, $t5", "ac3", 0xfbde3976,
                             0x00000000, 0x00000000, 0x12349876, t4, t5);
   TESTDSPINST_AC_RS_RT_DSPC("mulsaq_s.w.ph $ac0, $t0, $t1", "ac0", 0x23534870,
                             0x00354565, 0x00000000, 0x00354565, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("mulsaq_s.w.ph $ac1, $t2, $t3", "ac1", 0x980b7cde,
                             0x00086755, 0x00000000, 0x00086755, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("mulsaq_s.w.ph $ac2, $t4, $t1", "ac2", 0x00000018,
                             0x8f8f8f8f, 0xffffffff, 0x8f8f8f8f, t4, t1);
   TESTDSPINST_AC_RS_RT_DSPC("mulsaq_s.w.ph $ac3, $t6, $t7", "ac3", 0x92784656,
                             0xeeeeeeee, 0xffffffff, 0xeeeeeeee, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("mulsaq_s.w.ph $ac0, $t5, $t3", "ac0", 0xcacacaca,
                             0x1bdbdbdb, 0x00000000, 0x1bdbdbdb, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("mulsaq_s.w.ph $ac1, $t2, $t4", "ac1", 0xbacabaca,
                             0xdecadeca, 0xffffffff, 0xdecadeca, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("mulsaq_s.w.ph $ac2, $t0, $t8", "ac2", 0x12fadeb4,
                             0x93474bde, 0xffffffff, 0x93474bde, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("mulsaq_s.w.ph $ac3, $t4, $t5", "ac3", 0x7c000790,
                             0xfc0007ff, 0xffffffff, 0xfabfabfa, t4, t5);
   TESTDSPINST_AC_RS_RT_DSPC("mulsaq_s.w.ph $ac2, $t0, $t8", "ac2", 0xffffffff,
                             0xffffffff, 0x00000000, 0x083b3571, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("mulsaq_s.w.ph $ac0, $t0, $t1", "ac0", 0x24a3291e,
                             0x5648e540, 0xffffffff, 0xb9743941, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("mulsaq_s.w.ph $ac1, $t2, $t3", "ac1", 0xdd91eebf,
                             0xc54f79e6, 0xffffffff, 0xbce5f924, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("mulsaq_s.w.ph $ac2, $t4, $t1", "ac2", 0xf7ce2ec6,
                             0x5fc92974, 0xffffffff, 0xcc3c201c, t4, t1);
   TESTDSPINST_AC_RS_RT_DSPC("mulsaq_s.w.ph $ac3, $t6, $t7", "ac3", 0xbc1083e8,
                             0x7e08184e, 0x00000000, 0x1ebaf88e, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("mulsaq_s.w.ph $ac0, $t5, $t3", "ac0", 0xa617cc31,
                             0x71c8315f, 0x00000000, 0x722d5e20, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("mulsaq_s.w.ph $ac1, $t2, $t4", "ac1", 0xdfe1e8f0,
                             0x9493110e, 0xffffffff, 0xa1d6f791, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("mulsaq_s.w.ph $ac2, $t0, $t8", "ac2", 0x31458a23,
                             0xbb246228, 0x00000000, 0x7b11bee7, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("mulsaq_s.w.ph $ac3, $t4, $t5", "ac3", 0x848af791,
                             0x339d8d88, 0xffffffff, 0xa5631488, t4, t5);
   TESTDSPINST_AC_RS_RT_DSPC("mulsaq_s.w.ph $ac0, $t0, $t1", "ac0", 0xda3bacdc,
                             0x70974249, 0xffffffff, 0xb10bcc65, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("mulsaq_s.w.ph $ac1, $t2, $t3", "ac1", 0x649d5cbd,
                             0x8a8d4e7d, 0x00000000, 0x73f39fca, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("mulsaq_s.w.ph $ac2, $t4, $t1", "ac2", 0xc0c8c881,
                             0xeb1b4335, 0x00000000, 0x5648e540, t4, t1);
   TESTDSPINST_AC_RS_RT_DSPC("mulsaq_s.w.ph $ac3, $t6, $t7", "ac3", 0x7dd81a20,
                             0x0cd6b508, 0xffffffff, 0xc54f79e6, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("mulsaq_s.w.ph $ac0, $t5, $t3", "ac0", 0x7fff7fff,
                             0x6731e282, 0x00000000, 0x5fc92974, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("mulsaq_s.w.ph $ac1, $t2, $t4", "ac1", 0x00000555,
                             0xb6edf28f, 0x00000000, 0x7e08184e, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("mulsaq_s.w.ph $ac2, $t0, $t8", "ac2", 0x00000000,
                             0x4b4ec9ca, 0x00000000, 0x71c8315f, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("mulsaq_s.w.ph $ac3, $t4, $t5", "ac3", 0x80000000,
                             0xc1037fa4, 0xffffffff, 0x9493110e, t4, t5);
   TESTDSPINST_AC_RS_RT_DSPC("mulsaq_s.w.ph $ac1, $t2, $t4", "ac1", 0x55555555,
                             0xcb4ab48f, 0xffffffff, 0xbb246228, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("mulsaq_s.w.ph $ac2, $t0, $t8", "ac2", 0xffff8000,
                             0xaf8f8000, 0x00000000, 0x339d8d88, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("mulsaq_s.w.ph $ac0, $t0, $t1", "ac0", 0xabababab,
                             0x87df4510, 0x00000000, 0x70974249, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("mulsaq_s.w.ph $ac1, $t2, $t3", "ac1", 0xfc79b4d2,
                             0xabf4e8e1, 0xffffffff, 0x8a8d4e7d, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("mulsaq_s.w.ph $ac2, $t4, $t1", "ac2", 0x00000000,
                             0xf4c0eeac, 0xffffffff, 0xeb1b4335, t4, t1);
   TESTDSPINST_AC_RS_RT_DSPC("mulsaq_s.w.ph $ac3, $t6, $t7", "ac3", 0x00354565,
                             0x006a54f2, 0x00000000, 0x0cd6b508, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("mulsaq_s.w.ph $ac0, $t5, $t3", "ac0", 0x00086755,
                             0x79f74493, 0x00000000, 0x6731e282, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("mulsaq_s.w.ph $ac1, $t2, $t4", "ac1", 0xffff8000,
                             0x9c098000, 0xffffffff, 0xb6edf28f, t2, t4);

   printf("-------- MULT --------\n");
   TESTDSPINST_AC_RS_RT_NODSPC("mult $ac3, $t4, $t5", "ac3", 0x00000000,
                               0x00000000, 0xffffffff, 0x80000000, t4, t5);
   TESTDSPINST_AC_RS_RT_NODSPC("mult $ac0, $t0, $t1", "ac0", 0x00000004,
                               1073741824, 0x00000000, 0x00000006, t0, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("mult $ac1, $t2, $t3", "ac1", 0x80002435,
                               0x80003421, 0x00000000, 1073741824, t2, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("mult $ac3, $t6, $t7", "ac3", 0x76548000,
                               0x73468000, 0x00000000, 0x7fffffff, t6, t7);
   TESTDSPINST_AC_RS_RT_NODSPC("mult $ac0, $t5, $t3", "ac0", 0x80000000,
                               0x80000000, 0x00000000, 0x00000001, t5, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("mult $ac1, $t2, $t4", "ac1", 0x00010001,
                               0xffffffff, 0xffffffff, 0xffffffff, t2, t4);
   TESTDSPINST_AC_RS_RT_NODSPC("mult $ac2, $t0, $t8", "ac2", 0x7fff7fff,
                               0x7fff7fff, 0xffffffff, 0xffffffff, t0, t8);
   TESTDSPINST_AC_RS_RT_NODSPC("mult $ac0, $t0, $t1", "ac0", 0x0000c420,
                               0x00000555, 0x00000000, 0x0fde3126, t0, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("mult $ac1, $t2, $t3", "ac1", 0x00000000,
                               0x00000000, 0x00000000, 0x55555555, t2, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("mult $ac2, $t4, $t1", "ac2", 0x80000000,
                               0x80000000, 0xffffffff, 0xffff2435, t4, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("mult $ac3, $t6, $t7", "ac3", 0xaaaaaaaa,
                               0x55555555, 0xffffffff, 0xabababab, t6, t7);
   TESTDSPINST_AC_RS_RT_NODSPC("mult $ac0, $t5, $t3", "ac0", 0x00000018,
                               0xffff2435, 0xffffffff, 0xfc79b4d2, t5, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("mult $ac1, $t2, $t4", "ac1", 0xbabababa,
                               0xabababab, 0x00000000, 0x00000000, t2, t4);
   TESTDSPINST_AC_RS_RT_NODSPC("mult $ac2, $t0, $t8", "ac2", 0xf0f0f0f0,
                               0xfc79b4d2, 0x00000000, 0x00000000, t0, t8);
   TESTDSPINST_AC_RS_RT_NODSPC("mult $ac3, $t4, $t5", "ac3", 0xfbde3976,
                               0x00000000, 0x00000000, 0x12349876, t4, t5);
   TESTDSPINST_AC_RS_RT_NODSPC("mult $ac0, $t0, $t1", "ac0", 0x23534870,
                               0x00354565, 0x00000000, 0x00354565, t0, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("mult $ac1, $t2, $t3", "ac1", 0x980b7cde,
                               0x00086755, 0x00000000, 0x00086755, t2, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("mult $ac2, $t4, $t1", "ac2", 0x00000018,
                               0x8f8f8f8f, 0xffffffff, 0x8f8f8f8f, t4, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("mult $ac3, $t6, $t7", "ac3", 0x92784656,
                               0xeeeeeeee, 0xffffffff, 0xeeeeeeee, t6, t7);
   TESTDSPINST_AC_RS_RT_NODSPC("mult $ac0, $t5, $t3", "ac0", 0xcacacaca,
                               0x1bdbdbdb, 0x00000000, 0x1bdbdbdb, t5, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("mult $ac1, $t2, $t4", "ac1", 0xbacabaca,
                               0xdecadeca, 0xffffffff, 0xdecadeca, t2, t4);
   TESTDSPINST_AC_RS_RT_NODSPC("mult $ac2, $t0, $t8", "ac2", 0x12fadeb4,
                               0x93474bde, 0xffffffff, 0x93474bde, t0, t8);
   TESTDSPINST_AC_RS_RT_NODSPC("mult $ac3, $t4, $t5", "ac3", 0x7c000790,
                               0xfc0007ff, 0xffffffff, 0xfabfabfa, t4, t5);
   TESTDSPINST_AC_RS_RT_NODSPC("mult $ac2, $t0, $t8", "ac2", 0xffffffff,
                               0xffffffff, 0x00000000, 0x083b3571, t0, t8);
   TESTDSPINST_AC_RS_RT_NODSPC("mult $ac0, $t0, $t1", "ac0", 0x24a3291e,
                               0x5648e540, 0xffffffff, 0xb9743941, t0, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("mult $ac1, $t2, $t3", "ac1", 0xdd91eebf,
                               0xc54f79e6, 0xffffffff, 0xbce5f924, t2, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("mult $ac2, $t4, $t1", "ac2", 0xf7ce2ec6,
                               0x5fc92974, 0xffffffff, 0xcc3c201c, t4, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("mult $ac3, $t6, $t7", "ac3", 0xbc1083e8,
                               0x7e08184e, 0x00000000, 0x1ebaf88e, t6, t7);
   TESTDSPINST_AC_RS_RT_NODSPC("mult $ac0, $t5, $t3", "ac0", 0xa617cc31,
                               0x71c8315f, 0x00000000, 0x722d5e20, t5, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("mult $ac1, $t2, $t4", "ac1", 0xdfe1e8f0,
                               0x9493110e, 0xffffffff, 0xa1d6f791, t2, t4);
   TESTDSPINST_AC_RS_RT_NODSPC("mult $ac2, $t0, $t8", "ac2", 0x31458a23,
                               0xbb246228, 0x00000000, 0x7b11bee7, t0, t8);
   TESTDSPINST_AC_RS_RT_NODSPC("mult $ac3, $t4, $t5", "ac3", 0x848af791,
                               0x339d8d88, 0xffffffff, 0xa5631488, t4, t5);
   TESTDSPINST_AC_RS_RT_NODSPC("mult $ac0, $t0, $t1", "ac0", 0xda3bacdc,
                               0x70974249, 0xffffffff, 0xb10bcc65, t0, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("mult $ac1, $t2, $t3", "ac1", 0x649d5cbd,
                               0x8a8d4e7d, 0x00000000, 0x73f39fca, t2, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("mult $ac2, $t4, $t1", "ac2", 0xc0c8c881,
                               0xeb1b4335, 0x00000000, 0x5648e540, t4, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("mult $ac3, $t6, $t7", "ac3", 0x7dd81a20,
                               0x0cd6b508, 0xffffffff, 0xc54f79e6, t6, t7);
   TESTDSPINST_AC_RS_RT_NODSPC("mult $ac0, $t5, $t3", "ac0", 0x7fff7fff,
                               0x6731e282, 0x00000000, 0x5fc92974, t5, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("mult $ac1, $t2, $t4", "ac1", 0x00000555,
                               0xb6edf28f, 0x00000000, 0x7e08184e, t2, t4);
   TESTDSPINST_AC_RS_RT_NODSPC("mult $ac2, $t0, $t8", "ac2", 0x00000000,
                               0x4b4ec9ca, 0x00000000, 0x71c8315f, t0, t8);
   TESTDSPINST_AC_RS_RT_NODSPC("mult $ac3, $t4, $t5", "ac3", 0x80000000,
                               0xc1037fa4, 0xffffffff, 0x9493110e, t4, t5);
   TESTDSPINST_AC_RS_RT_NODSPC("mult $ac1, $t2, $t4", "ac1", 0x55555555,
                               0xcb4ab48f, 0xffffffff, 0xbb246228, t2, t4);
   TESTDSPINST_AC_RS_RT_NODSPC("mult $ac2, $t0, $t8", "ac2", 0xffff8000,
                               0xaf8f8000, 0x00000000, 0x339d8d88, t0, t8);
   TESTDSPINST_AC_RS_RT_NODSPC("mult $ac0, $t0, $t1", "ac0", 0xabababab,
                               0x87df4510, 0x00000000, 0x70974249, t0, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("mult $ac1, $t2, $t3", "ac1", 0xfc79b4d2,
                               0xabf4e8e1, 0xffffffff, 0x8a8d4e7d, t2, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("mult $ac2, $t4, $t1", "ac2", 0x00000000,
                               0xf4c0eeac, 0xffffffff, 0xeb1b4335, t4, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("mult $ac3, $t6, $t7", "ac3", 0x00354565,
                               0x006a54f2, 0x00000000, 0x0cd6b508, t6, t7);
   TESTDSPINST_AC_RS_RT_NODSPC("mult $ac0, $t5, $t3", "ac0", 0x00086755,
                               0x79f74493, 0x00000000, 0x6731e282, t5, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("mult $ac1, $t2, $t4", "ac1", 0xffff8000,
                               0x9c098000, 0xffffffff, 0xb6edf28f, t2, t4);

   printf("-------- MULTU --------\n");
   TESTDSPINST_AC_RS_RT_NODSPC("multu $ac3, $t4, $t5", "ac3", 0x00000000,
                               0x00000000, 0xffffffff, 0x80000000, t4, t5);
   TESTDSPINST_AC_RS_RT_NODSPC("multu $ac0, $t0, $t1", "ac0", 0x00000004,
                               1073741824, 0x00000000, 0x00000006, t0, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("multu $ac1, $t2, $t3", "ac1", 0x80002435,
                               0x80003421, 0x00000000, 1073741824, t2, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("multu $ac3, $t6, $t7", "ac3", 0x76548000,
                               0x73468000, 0x00000000, 0x7fffffff, t6, t7);
   TESTDSPINST_AC_RS_RT_NODSPC("multu $ac0, $t5, $t3", "ac0", 0x80000000,
                               0x80000000, 0x00000000, 0x00000001, t5, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("multu $ac1, $t2, $t4", "ac1", 0x00010001,
                               0xffffffff, 0xffffffff, 0xffffffff, t2, t4);
   TESTDSPINST_AC_RS_RT_NODSPC("multu $ac2, $t0, $t8", "ac2", 0x7fff7fff,
                               0x7fff7fff, 0xffffffff, 0xffffffff, t0, t8);
   TESTDSPINST_AC_RS_RT_NODSPC("multu $ac0, $t0, $t1", "ac0", 0x0000c420,
                               0x00000555, 0x00000000, 0x0fde3126, t0, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("multu $ac1, $t2, $t3", "ac1", 0x00000000,
                               0x00000000, 0x00000000, 0x55555555, t2, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("multu $ac2, $t4, $t1", "ac2", 0x80000000,
                               0x80000000, 0xffffffff, 0xffff2435, t4, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("multu $ac3, $t6, $t7", "ac3", 0xaaaaaaaa,
                               0x55555555, 0xffffffff, 0xabababab, t6, t7);
   TESTDSPINST_AC_RS_RT_NODSPC("multu $ac0, $t5, $t3", "ac0", 0x00000018,
                               0xffff2435, 0xffffffff, 0xfc79b4d2, t5, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("multu $ac1, $t2, $t4", "ac1", 0xbabababa,
                               0xabababab, 0x00000000, 0x00000000, t2, t4);
   TESTDSPINST_AC_RS_RT_NODSPC("multu $ac2, $t0, $t8", "ac2", 0xf0f0f0f0,
                               0xfc79b4d2, 0x00000000, 0x00000000, t0, t8);
   TESTDSPINST_AC_RS_RT_NODSPC("multu $ac3, $t4, $t5", "ac3", 0xfbde3976,
                               0x00000000, 0x00000000, 0x12349876, t4, t5);
   TESTDSPINST_AC_RS_RT_NODSPC("multu $ac0, $t0, $t1", "ac0", 0x23534870,
                               0x00354565, 0x00000000, 0x00354565, t0, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("multu $ac1, $t2, $t3", "ac1", 0x980b7cde,
                               0x00086755, 0x00000000, 0x00086755, t2, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("multu $ac2, $t4, $t1", "ac2", 0x00000018,
                               0x8f8f8f8f, 0xffffffff, 0x8f8f8f8f, t4, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("multu $ac3, $t6, $t7", "ac3", 0x92784656,
                               0xeeeeeeee, 0xffffffff, 0xeeeeeeee, t6, t7);
   TESTDSPINST_AC_RS_RT_NODSPC("multu $ac0, $t5, $t3", "ac0", 0xcacacaca,
                               0x1bdbdbdb, 0x00000000, 0x1bdbdbdb, t5, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("multu $ac1, $t2, $t4", "ac1", 0xbacabaca,
                               0xdecadeca, 0xffffffff, 0xdecadeca, t2, t4);
   TESTDSPINST_AC_RS_RT_NODSPC("multu $ac2, $t0, $t8", "ac2", 0x12fadeb4,
                               0x93474bde, 0xffffffff, 0x93474bde, t0, t8);
   TESTDSPINST_AC_RS_RT_NODSPC("multu $ac3, $t4, $t5", "ac3", 0x7c000790,
                               0xfc0007ff, 0xffffffff, 0xfabfabfa, t4, t5);
   TESTDSPINST_AC_RS_RT_NODSPC("multu $ac2, $t0, $t8", "ac2", 0xffffffff,
                               0xffffffff, 0x00000000, 0x083b3571, t0, t8);
   TESTDSPINST_AC_RS_RT_NODSPC("multu $ac0, $t0, $t1", "ac0", 0x24a3291e,
                               0x5648e540, 0xffffffff, 0xb9743941, t0, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("multu $ac1, $t2, $t3", "ac1", 0xdd91eebf,
                               0xc54f79e6, 0xffffffff, 0xbce5f924, t2, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("multu $ac2, $t4, $t1", "ac2", 0xf7ce2ec6,
                               0x5fc92974, 0xffffffff, 0xcc3c201c, t4, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("multu $ac3, $t6, $t7", "ac3", 0xbc1083e8,
                               0x7e08184e, 0x00000000, 0x1ebaf88e, t6, t7);
   TESTDSPINST_AC_RS_RT_NODSPC("multu $ac0, $t5, $t3", "ac0", 0xa617cc31,
                               0x71c8315f, 0x00000000, 0x722d5e20, t5, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("multu $ac1, $t2, $t4", "ac1", 0xdfe1e8f0,
                               0x9493110e, 0xffffffff, 0xa1d6f791, t2, t4);
   TESTDSPINST_AC_RS_RT_NODSPC("multu $ac2, $t0, $t8", "ac2", 0x31458a23,
                               0xbb246228, 0x00000000, 0x7b11bee7, t0, t8);
   TESTDSPINST_AC_RS_RT_NODSPC("multu $ac3, $t4, $t5", "ac3", 0x848af791,
                               0x339d8d88, 0xffffffff, 0xa5631488, t4, t5);
   TESTDSPINST_AC_RS_RT_NODSPC("multu $ac0, $t0, $t1", "ac0", 0xda3bacdc,
                               0x70974249, 0xffffffff, 0xb10bcc65, t0, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("multu $ac1, $t2, $t3", "ac1", 0x649d5cbd,
                               0x8a8d4e7d, 0x00000000, 0x73f39fca, t2, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("multu $ac2, $t4, $t1", "ac2", 0xc0c8c881,
                               0xeb1b4335, 0x00000000, 0x5648e540, t4, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("multu $ac3, $t6, $t7", "ac3", 0x7dd81a20,
                               0x0cd6b508, 0xffffffff, 0xc54f79e6, t6, t7);
   TESTDSPINST_AC_RS_RT_NODSPC("multu $ac0, $t5, $t3", "ac0", 0x7fff7fff,
                               0x6731e282, 0x00000000, 0x5fc92974, t5, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("multu $ac1, $t2, $t4", "ac1", 0x00000555,
                               0xb6edf28f, 0x00000000, 0x7e08184e, t2, t4);
   TESTDSPINST_AC_RS_RT_NODSPC("multu $ac2, $t0, $t8", "ac2", 0x00000000,
                               0x4b4ec9ca, 0x00000000, 0x71c8315f, t0, t8);
   TESTDSPINST_AC_RS_RT_NODSPC("multu $ac3, $t4, $t5", "ac3", 0x80000000,
                               0xc1037fa4, 0xffffffff, 0x9493110e, t4, t5);
   TESTDSPINST_AC_RS_RT_NODSPC("multu $ac1, $t2, $t4", "ac1", 0x55555555,
                               0xcb4ab48f, 0xffffffff, 0xbb246228, t2, t4);
   TESTDSPINST_AC_RS_RT_NODSPC("multu $ac2, $t0, $t8", "ac2", 0xffff8000,
                               0xaf8f8000, 0x00000000, 0x339d8d88, t0, t8);
   TESTDSPINST_AC_RS_RT_NODSPC("multu $ac0, $t0, $t1", "ac0", 0xabababab,
                               0x87df4510, 0x00000000, 0x70974249, t0, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("multu $ac1, $t2, $t3", "ac1", 0xfc79b4d2,
                               0xabf4e8e1, 0xffffffff, 0x8a8d4e7d, t2, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("multu $ac2, $t4, $t1", "ac2", 0x00000000,
                               0xf4c0eeac, 0xffffffff, 0xeb1b4335, t4, t1);
   TESTDSPINST_AC_RS_RT_NODSPC("multu $ac3, $t6, $t7", "ac3", 0x00354565,
                               0x006a54f2, 0x00000000, 0x0cd6b508, t6, t7);
   TESTDSPINST_AC_RS_RT_NODSPC("multu $ac0, $t5, $t3", "ac0", 0x00086755,
                               0x79f74493, 0x00000000, 0x6731e282, t5, t3);
   TESTDSPINST_AC_RS_RT_NODSPC("multu $ac1, $t2, $t4", "ac1", 0xffff8000,
                               0x9c098000, 0xffffffff, 0xb6edf28f, t2, t4);

   printf("-------- PACKRL.PH --------\n");
   TESTDSPINST_RD_RS_RT_NODSPC("packrl.ph $t0, $t1, $t2", 0x00000000,
                               0x00000000, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("packrl.ph $t2, $t3, $t4", 0x045fb232,
                               0x00028632, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("packrl.ph $t4, $t1, $t5", 0xfabc3435,
                               0xfabc3421, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("packrl.ph $t6, $t7, $t3", 0x07654cb8,
                               0x734680bc, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("packrl.ph $t5, $t3, $t2", 0xf973437b,
                               0x80000000, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("packrl.ph $t2, $t4, $t8", 0x00ff0001,
                               0xff01ffff, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("packrl.ph $t0, $t8, $t0", 0x7fff7fff,
                               0x7fff7fff, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("packrl.ph $t4, $t6, $t1", 0x0000c420,
                               0x00000555, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("packrl.ph $t0, $t1, $t2", 0x00000000,
                               0x00000000, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("packrl.ph $t2, $t3, $t4", 0x80000000,
                               0x80000000, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("packrl.ph $t4, $t1, $t5", 0xaaaaaaaa,
                               0x55555555, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("packrl.ph $t6, $t7, $t3", 0x00000018,
                               0xffff2435, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("packrl.ph $t5, $t3, $t2", 0xbabababa,
                               0xabababab, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("packrl.ph $t2, $t4, $t8", 0xf0f0f0f0,
                               0xfc79b4d2, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("packrl.ph $t0, $t8, $t0", 0xfbde3976,
                               0x00000000, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("packrl.ph $t4, $t6, $t1", 0x23534870,
                               0x00354565, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("packrl.ph $t0, $t1, $t2", 0x980b7cde,
                               0x00086755, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("packrl.ph $t2, $t3, $t4", 0x00000018,
                               0x8f8f8f8f, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("packrl.ph $t4, $t1, $t5", 0x92784656,
                               0xeeeeeeee, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("packrl.ph $t6, $t7, $t3", 0xcacacaca,
                               0x1bdbdbdb, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("packrl.ph $t5, $t3, $t2", 0xbacabaca,
                               0xdecadeca, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("packrl.ph $t2, $t4, $t8", 0x12fadeb4,
                               0x93474bde, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("packrl.ph $t0, $t8, $t0", 0x7c000790,
                               0xfc0007ff, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("packrl.ph $t4, $t6, $t1", 0xffffffff,
                               0xffffffff, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("packrl.ph $t0, $t1, $t2", 0xf2f4df1f,
                               0xcb4ab48f, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("packrl.ph $t2, $t3, $t4", 0x435f909a,
                               0xaf8f7e18, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("packrl.ph $t4, $t1, $t5", 0x2106ba5f,
                               0x87df4510, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("packrl.ph $t6, $t7, $t3", 0x246a6376,
                               0xabf4e8e1, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("packrl.ph $t5, $t3, $t2", 0x1046a1a3,
                               0xf4c0eeac, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("packrl.ph $t2, $t4, $t8", 0x638ca515,
                               0x006a54f2, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("packrl.ph $t0, $t8, $t0", 0xf63e7a9d,
                               0x79f74493, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("packrl.ph $t4, $t6, $t1", 0xbd6845cd,
                               0x9c09e313, t4, t6, t1);

   printf("-------- PICK.QB --------\n");
   TESTDSPINST_PICK("pick.qb $t5, $t3, $t2", "cmp.eq.ph $t3, $t2", 0xf973437b,
                    0x807343bc, t5, t3, t2);
   TESTDSPINST_PICK("pick.qb $t2, $t4, $t8", "cmp.eq.ph $t4, $t8", 0x00ff0001,
                    0x00ff0001, t2, t4, t8);
   TESTDSPINST_PICK("pick.qb $t3, $t8, $t0", "cmp.eq.ph $t8, $t0", 0x7fff7fff,
                    0x7fff7fff, t3, t8, t0);
   TESTDSPINST_PICK("pick.qb $t4, $t6, $t1", "cmp.eq.ph $t6, $t1", 0x0000c420,
                    0x00000555, t4, t6, t1);
   TESTDSPINST_PICK("pick.qb $t2, $t3, $t4", "cmp.lt.ph $t3, $t4", 0xf973437b,
                    0x807342bc, t2, t3, t4);
   TESTDSPINST_PICK("pick.qb $t4, $t1, $t5", "cmp.lt.ph $t1, $t5", 0x00ff0001,
                    0xff0100ff, t4, t1, t5);
   TESTDSPINST_PICK("pick.qb $t6, $t7, $t3", "cmp.lt.ph $t7, $t3", 0x7fff7fff,
                    0x7fff7fff, t6, t7, t3);
   TESTDSPINST_PICK("pick.qb $t0, $t1, $t2", "cmp.lt.ph $t1, $t2", 0x0000c420,
                    0x00000555, t0, t1, t2);
   TESTDSPINST_PICK("pick.qb $t2, $t3, $t4", "cmp.le.ph $t3, $t4", 0xf973437b,
                    0x807342bc, t2, t3, t4);
   TESTDSPINST_PICK("pick.qb $t4, $t1, $t5", "cmp.le.ph $t1, $t5", 0x00ff0001,
                    0xff0100ff, t4, t1, t5);
   TESTDSPINST_PICK("pick.qb $t6, $t7, $t3", "cmp.le.ph $t7, $t3", 0x7fff7fff,
                    0x7fff7fff, t6, t7, t3);
   TESTDSPINST_PICK("pick.qb $t0, $t1, $t2", "cmp.le.ph $t1, $t2", 0x0000c420,
                    0x00000555, t0, t1, t2);
   TESTDSPINST_PICK("pick.qb $t5, $t3, $t2", "cmp.eq.ph $t3, $t2", 0x1046a1a3,
                    0xf4c0eeac, t5, t3, t2);
   TESTDSPINST_PICK("pick.qb $t2, $t4, $t8", "cmp.eq.ph $t4, $t8", 0x638ca515,
                    0x006a54f2, t2, t4, t8);
   TESTDSPINST_PICK("pick.qb $t3, $t8, $t0", "cmp.eq.ph $t8, $t0", 0xf63e7a9d,
                    0x79f74493, t3, t8, t0);
   TESTDSPINST_PICK("pick.qb $t4, $t6, $t1", "cmp.eq.ph $t6, $t1", 0xbd6845cd,
                    0x9c09e313, t4, t6, t1);
   TESTDSPINST_PICK("pick.qb $t2, $t3, $t4", "cmp.lt.ph $t3, $t4", 0x1046a1a3,
                    0xf4c0eeac, t2, t3, t4);
   TESTDSPINST_PICK("pick.qb $t4, $t1, $t5", "cmp.lt.ph $t1, $t5", 0x638ca515,
                    0x006a54f2, t4, t1, t5);
   TESTDSPINST_PICK("pick.qb $t6, $t7, $t3", "cmp.lt.ph $t7, $t3", 0xf63e7a9d,
                    0x79f74493, t6, t7, t3);
   TESTDSPINST_PICK("pick.qb $t0, $t1, $t2", "cmp.lt.ph $t1, $t2", 0xbd6845cd,
                    0x9c09e313, t0, t1, t2);
   TESTDSPINST_PICK("pick.qb $t2, $t3, $t4", "cmp.le.ph $t3, $t4", 0x1046a1a3,
                    0xf4c0eeac, t2, t3, t4);
   TESTDSPINST_PICK("pick.qb $t4, $t1, $t5", "cmp.le.ph $t1, $t5", 0x638ca515,
                    0x006a54f2, t4, t1, t5);
   TESTDSPINST_PICK("pick.qb $t6, $t7, $t3", "cmp.le.ph $t7, $t3", 0xf63e7a9d,
                    0x79f74493, t6, t7, t3);
   TESTDSPINST_PICK("pick.qb $t0, $t1, $t2", "cmp.le.ph $t1, $t2", 0xbd6845cd,
                    0x9c09e313, t0, t1, t2);
   TESTDSPINST_PICK("pick.qb $t5, $t3, $t2", "cmp.eq.ph $t3, $t2", 0x92784656,
                    0xeeeeeeee, t5, t3, t2);
   TESTDSPINST_PICK("pick.qb $t2, $t4, $t8", "cmp.eq.ph $t4, $t8", 0xcacacaca,
                    0x1bdbdbdb, t2, t4, t8);
   TESTDSPINST_PICK("pick.qb $t3, $t8, $t0", "cmp.eq.ph $t8, $t0", 0xbacabaca,
                    0xdecadeca, t3, t8, t0);
   TESTDSPINST_PICK("pick.qb $t4, $t6, $t1", "cmp.eq.ph $t6, $t1", 0x12fadeb4,
                    0x93474bde, t4, t6, t1);
   TESTDSPINST_PICK("pick.qb $t2, $t3, $t4", "cmp.lt.ph $t3, $t4", 0x92784656,
                    0xeeeeeeee, t2, t3, t4);
   TESTDSPINST_PICK("pick.qb $t4, $t1, $t5", "cmp.lt.ph $t1, $t5", 0xcacacaca,
                    0x1bdbdbdb, t4, t1, t5);
   TESTDSPINST_PICK("pick.qb $t6, $t7, $t3", "cmp.lt.ph $t7, $t3", 0xbacabaca,
                    0xdecadeca, t6, t7, t3);
   TESTDSPINST_PICK("pick.qb $t0, $t1, $t2", "cmp.lt.ph $t1, $t2", 0x12fadeb4,
                    0x93474bde, t0, t1, t2);
   TESTDSPINST_PICK("pick.qb $t2, $t3, $t4", "cmp.le.ph $t3, $t4", 0x92784656,
                    0xeeeeeeee, t2, t3, t4);
   TESTDSPINST_PICK("pick.qb $t4, $t1, $t5", "cmp.le.ph $t1, $t5", 0xcacacaca,
                    0x1bdbdbdb, t4, t1, t5);
   TESTDSPINST_PICK("pick.qb $t6, $t7, $t3", "cmp.le.ph $t7, $t3", 0xbacabaca,
                    0xdecadeca, t6, t7, t3);
   TESTDSPINST_PICK("pick.qb $t0, $t1, $t2", "cmp.le.ph $t1, $t2", 0x12fadeb4,
                    0x93474bde, t0, t1, t2);

   printf("-------- PRECEQ.W.PHL --------\n");
   TESTDSPINST_RD_RT_NODSPC("preceq.w.phl $t0, $t1", 0x00000000, t0, t1);
   TESTDSPINST_RD_RT_NODSPC("preceq.w.phl $t2, $t3", 0x80003286, t2, t3);
   TESTDSPINST_RD_RT_NODSPC("preceq.w.phl $t4, $t1", 0xfabc2435, t4, t1);
   TESTDSPINST_RD_RT_NODSPC("preceq.w.phl $t6, $t7", 0x73468000, t6, t7);
   TESTDSPINST_RD_RT_NODSPC("preceq.w.phl $t5, $t3", 0x80000000, t5, t3);
   TESTDSPINST_RD_RT_NODSPC("preceq.w.phl $t2, $t4", 0xffffffff, t2, t4);
   TESTDSPINST_RD_RT_NODSPC("preceq.w.phl $t0, $t8", 0xfff45fff, t0, t8);
   TESTDSPINST_RD_RT_NODSPC("preceq.w.phl $t4, $t4", 0x00000555, t4, t4);
   TESTDSPINST_RD_RT_NODSPC("preceq.w.phl $t0, $t1", 0x00005340, t0, t1);
   TESTDSPINST_RD_RT_NODSPC("preceq.w.phl $t2, $t3", 0x80000000, t2, t3);
   TESTDSPINST_RD_RT_NODSPC("preceq.w.phl $t4, $t1", 0x55555555, t4, t1);
   TESTDSPINST_RD_RT_NODSPC("preceq.w.phl $t6, $t7", 0xffff2435, t6, t7);
   TESTDSPINST_RD_RT_NODSPC("preceq.w.phl $t5, $t3", 0xabababab, t5, t3);
   TESTDSPINST_RD_RT_NODSPC("preceq.w.phl $t2, $t4", 0xfc79b4d2, t2, t4);
   TESTDSPINST_RD_RT_NODSPC("preceq.w.phl $t0, $t8", 0x00000000, t0, t8);
   TESTDSPINST_RD_RT_NODSPC("preceq.w.phl $t4, $t4", 0x00354565, t4, t4);
   TESTDSPINST_RD_RT_NODSPC("preceq.w.phl $t0, $t1", 0x00086755, t0, t1);
   TESTDSPINST_RD_RT_NODSPC("preceq.w.phl $t2, $t3", 0x8f8f8f8f, t2, t3);
   TESTDSPINST_RD_RT_NODSPC("preceq.w.phl $t4, $t1", 0xeeeeeeee, t4, t1);
   TESTDSPINST_RD_RT_NODSPC("preceq.w.phl $t6, $t7", 0x1bdbdbdb, t6, t7);
   TESTDSPINST_RD_RT_NODSPC("preceq.w.phl $t5, $t3", 0xdecadeca, t5, t3);
   TESTDSPINST_RD_RT_NODSPC("preceq.w.phl $t2, $t4", 0x93474bde, t2, t4);
   TESTDSPINST_RD_RT_NODSPC("preceq.w.phl $t0, $t8", 0xfc0007ff, t0, t8);
   TESTDSPINST_RD_RT_NODSPC("preceq.w.phl $t4, $t4", 0xffffffff, t4, t4);
   TESTDSPINST_RD_RT_NODSPC("preceq.w.phl $t0, $t1", 0xcb4ab48f, t0, t1);
   TESTDSPINST_RD_RT_NODSPC("preceq.w.phl $t2, $t3", 0xaf8f7e18, t2, t3);
   TESTDSPINST_RD_RT_NODSPC("preceq.w.phl $t4, $t1", 0x87df4510, t4, t1);
   TESTDSPINST_RD_RT_NODSPC("preceq.w.phl $t6, $t7", 0xabf4e8e1, t6, t7);
   TESTDSPINST_RD_RT_NODSPC("preceq.w.phl $t5, $t3", 0xf4c0eeac, t5, t3);
   TESTDSPINST_RD_RT_NODSPC("preceq.w.phl $t2, $t4", 0x006a54f2, t2, t4);
   TESTDSPINST_RD_RT_NODSPC("preceq.w.phl $t0, $t8", 0x79f74493, t0, t8);
   TESTDSPINST_RD_RT_NODSPC("preceq.w.phl $t4, $t4", 0x9c09e313, t4, t4);

   printf("-------- PICK.PH --------\n");
   TESTDSPINST_PICK("pick.ph $t5, $t3, $t2", "cmp.eq.ph $t3, $t2", 0xf973437b,
                    0x807343bc, t5, t3, t2);
   TESTDSPINST_PICK("pick.ph $t2, $t4, $t8", "cmp.eq.ph $t4, $t8", 0x00ff0001,
                    0x00ff0001, t2, t4, t8);
   TESTDSPINST_PICK("pick.ph $t3, $t8, $t0", "cmp.eq.ph $t8, $t0", 0x7fff7fff,
                    0x7fff7fff, t3, t8, t0);
   TESTDSPINST_PICK("pick.ph $t4, $t6, $t1", "cmp.eq.ph $t6, $t1", 0x0000c420,
                    0x00000555, t4, t6, t1);
   TESTDSPINST_PICK("pick.ph $t2, $t3, $t4", "cmp.lt.ph $t3, $t4", 0xf973437b,
                    0x807342bc, t2, t3, t4);
   TESTDSPINST_PICK("pick.ph $t4, $t1, $t5", "cmp.lt.ph $t1, $t5", 0x00ff0001,
                    0xff0100ff, t4, t1, t5);
   TESTDSPINST_PICK("pick.ph $t6, $t7, $t3", "cmp.lt.ph $t7, $t3", 0x7fff7fff,
                    0x7fff7fff, t6, t7, t3);
   TESTDSPINST_PICK("pick.ph $t0, $t1, $t2", "cmp.lt.ph $t1, $t2", 0x0000c420,
                    0x00000555, t0, t1, t2);
   TESTDSPINST_PICK("pick.ph $t2, $t3, $t4", "cmp.le.ph $t3, $t4", 0xf973437b,
                    0x807342bc, t2, t3, t4);
   TESTDSPINST_PICK("pick.ph $t4, $t1, $t5", "cmp.le.ph $t1, $t5", 0x00ff0001,
                    0xff0100ff, t4, t1, t5);
   TESTDSPINST_PICK("pick.ph $t6, $t7, $t3", "cmp.le.ph $t7, $t3", 0x7fff7fff,
                    0x7fff7fff, t6, t7, t3);
   TESTDSPINST_PICK("pick.ph $t0, $t1, $t2", "cmp.le.ph $t1, $t2", 0x0000c420,
                    0x00000555, t0, t1, t2);
   TESTDSPINST_PICK("pick.ph $t5, $t3, $t2", "cmp.eq.ph $t3, $t2", 0x1046a1a3,
                    0xf4c0eeac, t5, t3, t2);
   TESTDSPINST_PICK("pick.ph $t2, $t4, $t8", "cmp.eq.ph $t4, $t8", 0x638ca515,
                    0x006a54f2, t2, t4, t8);
   TESTDSPINST_PICK("pick.ph $t3, $t8, $t0", "cmp.eq.ph $t8, $t0", 0xf63e7a9d,
                    0x79f74493, t3, t8, t0);
   TESTDSPINST_PICK("pick.ph $t4, $t6, $t1", "cmp.eq.ph $t6, $t1", 0xbd6845cd,
                    0x9c09e313, t4, t6, t1);
   TESTDSPINST_PICK("pick.ph $t2, $t3, $t4", "cmp.lt.ph $t3, $t4", 0x1046a1a3,
                    0xf4c0eeac, t2, t3, t4);
   TESTDSPINST_PICK("pick.ph $t4, $t1, $t5", "cmp.lt.ph $t1, $t5", 0x638ca515,
                    0x006a54f2, t4, t1, t5);
   TESTDSPINST_PICK("pick.ph $t6, $t7, $t3", "cmp.lt.ph $t7, $t3", 0xf63e7a9d,
                    0x79f74493, t6, t7, t3);
   TESTDSPINST_PICK("pick.ph $t0, $t1, $t2", "cmp.lt.ph $t1, $t2", 0xbd6845cd,
                    0x9c09e313, t0, t1, t2);
   TESTDSPINST_PICK("pick.ph $t2, $t3, $t4", "cmp.le.ph $t3, $t4", 0x1046a1a3,
                    0xf4c0eeac, t2, t3, t4);
   TESTDSPINST_PICK("pick.ph $t4, $t1, $t5", "cmp.le.ph $t1, $t5", 0x638ca515,
                    0x006a54f2, t4, t1, t5);
   TESTDSPINST_PICK("pick.ph $t6, $t7, $t3", "cmp.le.ph $t7, $t3", 0xf63e7a9d,
                    0x79f74493, t6, t7, t3);
   TESTDSPINST_PICK("pick.ph $t0, $t1, $t2", "cmp.le.ph $t1, $t2", 0xbd6845cd,
                    0x9c09e313, t0, t1, t2);
   TESTDSPINST_PICK("pick.ph $t5, $t3, $t2", "cmp.eq.ph $t3, $t2", 0x92784656,
                    0xeeeeeeee, t5, t3, t2);
   TESTDSPINST_PICK("pick.ph $t2, $t4, $t8", "cmp.eq.ph $t4, $t8", 0xcacacaca,
                    0x1bdbdbdb, t2, t4, t8);
   TESTDSPINST_PICK("pick.ph $t3, $t8, $t0", "cmp.eq.ph $t8, $t0", 0xbacabaca,
                    0xdecadeca, t3, t8, t0);
   TESTDSPINST_PICK("pick.ph $t4, $t6, $t1", "cmp.eq.ph $t6, $t1", 0x12fadeb4,
                    0x93474bde, t4, t6, t1);
   TESTDSPINST_PICK("pick.ph $t2, $t3, $t4", "cmp.lt.ph $t3, $t4", 0x92784656,
                    0xeeeeeeee, t2, t3, t4);
   TESTDSPINST_PICK("pick.ph $t4, $t1, $t5", "cmp.lt.ph $t1, $t5", 0xcacacaca,
                    0x1bdbdbdb, t4, t1, t5);
   TESTDSPINST_PICK("pick.ph $t6, $t7, $t3", "cmp.lt.ph $t7, $t3", 0xbacabaca,
                    0xdecadeca, t6, t7, t3);
   TESTDSPINST_PICK("pick.ph $t0, $t1, $t2", "cmp.lt.ph $t1, $t2", 0x12fadeb4,
                    0x93474bde, t0, t1, t2);
   TESTDSPINST_PICK("pick.ph $t2, $t3, $t4", "cmp.le.ph $t3, $t4", 0x92784656,
                    0xeeeeeeee, t2, t3, t4);
   TESTDSPINST_PICK("pick.ph $t4, $t1, $t5", "cmp.le.ph $t1, $t5", 0xcacacaca,
                    0x1bdbdbdb, t4, t1, t5);
   TESTDSPINST_PICK("pick.ph $t6, $t7, $t3", "cmp.le.ph $t7, $t3", 0xbacabaca,
                    0xdecadeca, t6, t7, t3);
   TESTDSPINST_PICK("pick.ph $t0, $t1, $t2", "cmp.le.ph $t1, $t2", 0x12fadeb4,
                    0x93474bde, t0, t1, t2);

   printf("-------- PRECEQ.W.PHR --------\n");
   TESTDSPINST_RD_RT_NODSPC("preceq.w.phr $t0, $t1", 0x00000000, t0, t1);
   TESTDSPINST_RD_RT_NODSPC("preceq.w.phr $t2, $t3", 0x80003286, t2, t3);
   TESTDSPINST_RD_RT_NODSPC("preceq.w.phr $t4, $t1", 0xfabc2435, t4, t1);
   TESTDSPINST_RD_RT_NODSPC("preceq.w.phr $t6, $t7", 0x73468000, t6, t7);
   TESTDSPINST_RD_RT_NODSPC("preceq.w.phr $t5, $t3", 0x80000000, t5, t3);
   TESTDSPINST_RD_RT_NODSPC("preceq.w.phr $t2, $t4", 0xffffffff, t2, t4);
   TESTDSPINST_RD_RT_NODSPC("preceq.w.phr $t0, $t8", 0xfff45fff, t0, t8);
   TESTDSPINST_RD_RT_NODSPC("preceq.w.phr $t4, $t4", 0x00000555, t4, t4);
   TESTDSPINST_RD_RT_NODSPC("preceq.w.phr $t0, $t1", 0x00005340, t0, t1);
   TESTDSPINST_RD_RT_NODSPC("preceq.w.phr $t2, $t3", 0x80000000, t2, t3);
   TESTDSPINST_RD_RT_NODSPC("preceq.w.phr $t4, $t1", 0x55555555, t4, t1);
   TESTDSPINST_RD_RT_NODSPC("preceq.w.phr $t6, $t7", 0xffff2435, t6, t7);
   TESTDSPINST_RD_RT_NODSPC("preceq.w.phr $t5, $t3", 0xabababab, t5, t3);
   TESTDSPINST_RD_RT_NODSPC("preceq.w.phr $t2, $t4", 0xfc79b4d2, t2, t4);
   TESTDSPINST_RD_RT_NODSPC("preceq.w.phr $t0, $t8", 0x00000000, t0, t8);
   TESTDSPINST_RD_RT_NODSPC("preceq.w.phr $t4, $t4", 0x00354565, t4, t4);
   TESTDSPINST_RD_RT_NODSPC("preceq.w.phr $t0, $t1", 0x00086755, t0, t1);
   TESTDSPINST_RD_RT_NODSPC("preceq.w.phr $t2, $t3", 0x8f8f8f8f, t2, t3);
   TESTDSPINST_RD_RT_NODSPC("preceq.w.phr $t4, $t1", 0xeeeeeeee, t4, t1);
   TESTDSPINST_RD_RT_NODSPC("preceq.w.phr $t6, $t7", 0x1bdbdbdb, t6, t7);
   TESTDSPINST_RD_RT_NODSPC("preceq.w.phr $t5, $t3", 0xdecadeca, t5, t3);
   TESTDSPINST_RD_RT_NODSPC("preceq.w.phr $t2, $t4", 0x93474bde, t2, t4);
   TESTDSPINST_RD_RT_NODSPC("preceq.w.phr $t0, $t8", 0xfc0007ff, t0, t8);
   TESTDSPINST_RD_RT_NODSPC("preceq.w.phr $t4, $t4", 0xffffffff, t4, t4);
   TESTDSPINST_RD_RT_NODSPC("preceq.w.phr $t0, $t1", 0xcb4ab48f, t0, t1);
   TESTDSPINST_RD_RT_NODSPC("preceq.w.phr $t2, $t3", 0xaf8f7e18, t2, t3);
   TESTDSPINST_RD_RT_NODSPC("preceq.w.phr $t4, $t1", 0x87df4510, t4, t1);
   TESTDSPINST_RD_RT_NODSPC("preceq.w.phr $t6, $t7", 0xabf4e8e1, t6, t7);
   TESTDSPINST_RD_RT_NODSPC("preceq.w.phr $t5, $t3", 0xf4c0eeac, t5, t3);
   TESTDSPINST_RD_RT_NODSPC("preceq.w.phr $t2, $t4", 0x006a54f2, t2, t4);
   TESTDSPINST_RD_RT_NODSPC("preceq.w.phr $t0, $t8", 0x79f74493, t0, t8);
   TESTDSPINST_RD_RT_NODSPC("preceq.w.phr $t4, $t4", 0x9c09e313, t4, t4);

   printf("-------- PRECEQU.PH.QBL --------\n");
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbl $t0, $t1", 0x00000000, t0, t1);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbl $t2, $t3", 0x80003286, t2, t3);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbl $t4, $t1", 0xfabc2435, t4, t1);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbl $t6, $t7", 0x73468000, t6, t7);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbl $t5, $t3", 0x80000000, t5, t3);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbl $t2, $t4", 0xffffffff, t2, t4);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbl $t0, $t8", 0xfff45fff, t0, t8);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbl $t4, $t4", 0x00000555, t4, t4);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbl $t0, $t1", 0x00005340, t0, t1);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbl $t2, $t3", 0x80000000, t2, t3);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbl $t4, $t1", 0x55555555, t4, t1);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbl $t6, $t7", 0xffff2435, t6, t7);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbl $t5, $t3", 0xabababab, t5, t3);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbl $t2, $t4", 0xfc79b4d2, t2, t4);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbl $t0, $t8", 0x00000000, t0, t8);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbl $t4, $t4", 0x00354565, t4, t4);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbl $t0, $t1", 0x00086755, t0, t1);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbl $t2, $t3", 0x8f8f8f8f, t2, t3);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbl $t4, $t1", 0xeeeeeeee, t4, t1);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbl $t6, $t7", 0x1bdbdbdb, t6, t7);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbl $t5, $t3", 0xdecadeca, t5, t3);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbl $t2, $t4", 0x93474bde, t2, t4);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbl $t0, $t8", 0xfc0007ff, t0, t8);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbl $t4, $t4", 0xffffffff, t4, t4);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbl $t0, $t1", 0xcb4ab48f, t0, t1);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbl $t2, $t3", 0xaf8f7e18, t2, t3);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbl $t4, $t1", 0x87df4510, t4, t1);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbl $t6, $t7", 0xabf4e8e1, t6, t7);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbl $t5, $t3", 0xf4c0eeac, t5, t3);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbl $t2, $t4", 0x006a54f2, t2, t4);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbl $t0, $t8", 0x79f74493, t0, t8);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbl $t4, $t4", 0x9c09e313, t4, t4);

   printf("-------- PRECEQU.PH.QBLA --------\n");
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbla $t0, $t1", 0x00000000, t0, t1);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbla $t2, $t3", 0x80003286, t2, t3);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbla $t4, $t1", 0xfabc2435, t4, t1);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbla $t6, $t7", 0x73468000, t6, t7);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbla $t5, $t3", 0x80000000, t5, t3);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbla $t2, $t4", 0xffffffff, t2, t4);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbla $t0, $t8", 0xfff45fff, t0, t8);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbla $t4, $t4", 0x00000555, t4, t4);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbla $t0, $t1", 0x00005340, t0, t1);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbla $t2, $t3", 0x80000000, t2, t3);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbla $t4, $t1", 0x55555555, t4, t1);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbla $t6, $t7", 0xffff2435, t6, t7);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbla $t5, $t3", 0xabababab, t5, t3);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbla $t2, $t4", 0xfc79b4d2, t2, t4);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbla $t0, $t8", 0x00000000, t0, t8);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbla $t4, $t4", 0x00354565, t4, t4);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbla $t0, $t1", 0x00086755, t0, t1);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbla $t2, $t3", 0x8f8f8f8f, t2, t3);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbla $t4, $t1", 0xeeeeeeee, t4, t1);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbla $t6, $t7", 0x1bdbdbdb, t6, t7);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbla $t5, $t3", 0xdecadeca, t5, t3);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbla $t2, $t4", 0x93474bde, t2, t4);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbla $t0, $t8", 0xfc0007ff, t0, t8);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbla $t4, $t4", 0xffffffff, t4, t4);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbla $t0, $t1", 0xcb4ab48f, t0, t1);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbla $t2, $t3", 0xaf8f7e18, t2, t3);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbla $t4, $t1", 0x87df4510, t4, t1);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbla $t6, $t7", 0xabf4e8e1, t6, t7);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbla $t5, $t3", 0xf4c0eeac, t5, t3);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbla $t2, $t4", 0x006a54f2, t2, t4);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbla $t0, $t8", 0x79f74493, t0, t8);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbla $t4, $t4", 0x9c09e313, t4, t4);

   printf("-------- PRECEQU.PH.QBR --------\n");
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbr $t0, $t1", 0x00000000, t0, t1);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbr $t2, $t3", 0x80003286, t2, t3);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbr $t4, $t1", 0xfabc2435, t4, t1);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbr $t6, $t7", 0x73468000, t6, t7);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbr $t5, $t3", 0x80000000, t5, t3);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbr $t2, $t4", 0xffffffff, t2, t4);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbr $t0, $t8", 0xfff45fff, t0, t8);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbr $t4, $t4", 0x00000555, t4, t4);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbr $t0, $t1", 0x00005340, t0, t1);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbr $t2, $t3", 0x80000000, t2, t3);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbr $t4, $t1", 0x55555555, t4, t1);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbr $t6, $t7", 0xffff2435, t6, t7);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbr $t5, $t3", 0xabababab, t5, t3);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbr $t2, $t4", 0xfc79b4d2, t2, t4);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbr $t0, $t8", 0x00000000, t0, t8);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbr $t4, $t4", 0x00354565, t4, t4);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbr $t0, $t1", 0x00086755, t0, t1);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbr $t2, $t3", 0x8f8f8f8f, t2, t3);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbr $t4, $t1", 0xeeeeeeee, t4, t1);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbr $t6, $t7", 0x1bdbdbdb, t6, t7);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbr $t5, $t3", 0xdecadeca, t5, t3);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbr $t2, $t4", 0x93474bde, t2, t4);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbr $t0, $t8", 0xfc0007ff, t0, t8);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbr $t4, $t4", 0xffffffff, t4, t4);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbr $t0, $t1", 0xcb4ab48f, t0, t1);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbr $t2, $t3", 0xaf8f7e18, t2, t3);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbr $t4, $t1", 0x87df4510, t4, t1);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbr $t6, $t7", 0xabf4e8e1, t6, t7);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbr $t5, $t3", 0xf4c0eeac, t5, t3);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbr $t2, $t4", 0x006a54f2, t2, t4);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbr $t0, $t8", 0x79f74493, t0, t8);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbr $t4, $t4", 0x9c09e313, t4, t4);

   printf("-------- PRECEQU.PH.QBRA --------\n");
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbra $t0, $t1", 0x00000000, t0, t1);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbra $t2, $t3", 0x80003286, t2, t3);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbra $t4, $t1", 0xfabc2435, t4, t1);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbra $t6, $t7", 0x73468000, t6, t7);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbra $t5, $t3", 0x80000000, t5, t3);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbra $t2, $t4", 0xffffffff, t2, t4);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbra $t0, $t8", 0xfff45fff, t0, t8);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbra $t4, $t4", 0x00000555, t4, t4);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbra $t0, $t1", 0x00005340, t0, t1);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbra $t2, $t3", 0x80000000, t2, t3);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbra $t4, $t1", 0x55555555, t4, t1);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbra $t6, $t7", 0xffff2435, t6, t7);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbra $t5, $t3", 0xabababab, t5, t3);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbra $t2, $t4", 0xfc79b4d2, t2, t4);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbra $t0, $t8", 0x00000000, t0, t8);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbra $t4, $t4", 0x00354565, t4, t4);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbra $t0, $t1", 0x00086755, t0, t1);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbra $t2, $t3", 0x8f8f8f8f, t2, t3);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbra $t4, $t1", 0xeeeeeeee, t4, t1);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbra $t6, $t7", 0x1bdbdbdb, t6, t7);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbra $t5, $t3", 0xdecadeca, t5, t3);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbra $t2, $t4", 0x93474bde, t2, t4);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbra $t0, $t8", 0xfc0007ff, t0, t8);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbra $t4, $t4", 0xffffffff, t4, t4);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbra $t0, $t1", 0xcb4ab48f, t0, t1);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbra $t2, $t3", 0xaf8f7e18, t2, t3);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbra $t4, $t1", 0x87df4510, t4, t1);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbra $t6, $t7", 0xabf4e8e1, t6, t7);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbra $t5, $t3", 0xf4c0eeac, t5, t3);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbra $t2, $t4", 0x006a54f2, t2, t4);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbra $t0, $t8", 0x79f74493, t0, t8);
   TESTDSPINST_RD_RT_NODSPC("precequ.ph.qbra $t4, $t4", 0x9c09e313, t4, t4);

   printf("-------- PRECEU.PH.QBL --------\n");
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbl $t0, $t1", 0x00000000, t0, t1);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbl $t2, $t3", 0x80003286, t2, t3);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbl $t4, $t1", 0xfabc2435, t4, t1);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbl $t6, $t7", 0x73468000, t6, t7);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbl $t5, $t3", 0x80000000, t5, t3);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbl $t2, $t4", 0xffffffff, t2, t4);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbl $t0, $t8", 0xfff45fff, t0, t8);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbl $t4, $t4", 0x00000555, t4, t4);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbl $t0, $t1", 0x00005340, t0, t1);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbl $t2, $t3", 0x80000000, t2, t3);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbl $t4, $t1", 0x55555555, t4, t1);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbl $t6, $t7", 0xffff2435, t6, t7);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbl $t5, $t3", 0xabababab, t5, t3);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbl $t2, $t4", 0xfc79b4d2, t2, t4);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbl $t0, $t8", 0x00000000, t0, t8);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbl $t4, $t4", 0x00354565, t4, t4);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbl $t0, $t1", 0x00086755, t0, t1);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbl $t2, $t3", 0x8f8f8f8f, t2, t3);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbl $t4, $t1", 0xeeeeeeee, t4, t1);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbl $t6, $t7", 0x1bdbdbdb, t6, t7);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbl $t5, $t3", 0xdecadeca, t5, t3);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbl $t2, $t4", 0x93474bde, t2, t4);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbl $t0, $t8", 0xfc0007ff, t0, t8);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbl $t4, $t4", 0xffffffff, t4, t4);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbl $t0, $t1", 0xcb4ab48f, t0, t1);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbl $t2, $t3", 0xaf8f7e18, t2, t3);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbl $t4, $t1", 0x87df4510, t4, t1);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbl $t6, $t7", 0xabf4e8e1, t6, t7);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbl $t5, $t3", 0xf4c0eeac, t5, t3);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbl $t2, $t4", 0x006a54f2, t2, t4);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbl $t0, $t8", 0x79f74493, t0, t8);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbl $t4, $t4", 0x9c09e313, t4, t4);

   printf("-------- PRECEU.PH.QBLA --------\n");
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbla $t0, $t1", 0x00000000, t0, t1);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbla $t2, $t3", 0x80003286, t2, t3);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbla $t4, $t1", 0xfabc2435, t4, t1);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbla $t6, $t7", 0x73468000, t6, t7);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbla $t5, $t3", 0x80000000, t5, t3);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbla $t2, $t4", 0xffffffff, t2, t4);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbla $t0, $t8", 0xfff45fff, t0, t8);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbla $t4, $t4", 0x00000555, t4, t4);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbla $t0, $t1", 0x00005340, t0, t1);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbla $t2, $t3", 0x80000000, t2, t3);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbla $t4, $t1", 0x55555555, t4, t1);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbla $t6, $t7", 0xffff2435, t6, t7);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbla $t5, $t3", 0xabababab, t5, t3);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbla $t2, $t4", 0xfc79b4d2, t2, t4);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbla $t0, $t8", 0x00000000, t0, t8);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbla $t4, $t4", 0x00354565, t4, t4);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbla $t0, $t1", 0x00086755, t0, t1);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbla $t2, $t3", 0x8f8f8f8f, t2, t3);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbla $t4, $t1", 0xeeeeeeee, t4, t1);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbla $t6, $t7", 0x1bdbdbdb, t6, t7);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbla $t5, $t3", 0xdecadeca, t5, t3);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbla $t2, $t4", 0x93474bde, t2, t4);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbla $t0, $t8", 0xfc0007ff, t0, t8);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbla $t4, $t4", 0xffffffff, t4, t4);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbla $t0, $t1", 0xcb4ab48f, t0, t1);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbla $t2, $t3", 0xaf8f7e18, t2, t3);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbla $t4, $t1", 0x87df4510, t4, t1);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbla $t6, $t7", 0xabf4e8e1, t6, t7);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbla $t5, $t3", 0xf4c0eeac, t5, t3);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbla $t2, $t4", 0x006a54f2, t2, t4);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbla $t0, $t8", 0x79f74493, t0, t8);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbla $t4, $t4", 0x9c09e313, t4, t4);

   printf("-------- PRECEU.PH.QBR --------\n");
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbr $t0, $t1", 0x00000000, t0, t1);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbr $t2, $t3", 0x80003286, t2, t3);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbr $t4, $t1", 0xfabc2435, t4, t1);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbr $t6, $t7", 0x73468000, t6, t7);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbr $t5, $t3", 0x80000000, t5, t3);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbr $t2, $t4", 0xffffffff, t2, t4);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbr $t0, $t8", 0xfff45fff, t0, t8);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbr $t4, $t4", 0x00000555, t4, t4);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbr $t0, $t1", 0x00005340, t0, t1);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbr $t2, $t3", 0x80000000, t2, t3);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbr $t4, $t1", 0x55555555, t4, t1);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbr $t6, $t7", 0xffff2435, t6, t7);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbr $t5, $t3", 0xabababab, t5, t3);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbr $t2, $t4", 0xfc79b4d2, t2, t4);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbr $t0, $t8", 0x00000000, t0, t8);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbr $t4, $t4", 0x00354565, t4, t4);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbr $t0, $t1", 0x00086755, t0, t1);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbr $t2, $t3", 0x8f8f8f8f, t2, t3);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbr $t4, $t1", 0xeeeeeeee, t4, t1);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbr $t6, $t7", 0x1bdbdbdb, t6, t7);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbr $t5, $t3", 0xdecadeca, t5, t3);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbr $t2, $t4", 0x93474bde, t2, t4);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbr $t0, $t8", 0xfc0007ff, t0, t8);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbr $t4, $t4", 0xffffffff, t4, t4);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbr $t0, $t1", 0xcb4ab48f, t0, t1);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbr $t2, $t3", 0xaf8f7e18, t2, t3);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbr $t4, $t1", 0x87df4510, t4, t1);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbr $t6, $t7", 0xabf4e8e1, t6, t7);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbr $t5, $t3", 0xf4c0eeac, t5, t3);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbr $t2, $t4", 0x006a54f2, t2, t4);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbr $t0, $t8", 0x79f74493, t0, t8);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbr $t4, $t4", 0x9c09e313, t4, t4);

   printf("-------- PRECEU.PH.QBRA --------\n");
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbra $t0, $t1", 0x00000000, t0, t1);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbra $t2, $t3", 0x80003286, t2, t3);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbra $t4, $t1", 0xfabc2435, t4, t1);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbra $t6, $t7", 0x73468000, t6, t7);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbra $t5, $t3", 0x80000000, t5, t3);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbra $t2, $t4", 0xffffffff, t2, t4);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbra $t0, $t8", 0xfff45fff, t0, t8);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbra $t4, $t4", 0x00000555, t4, t4);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbra $t0, $t1", 0x00005340, t0, t1);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbra $t2, $t3", 0x80000000, t2, t3);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbra $t4, $t1", 0x55555555, t4, t1);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbra $t6, $t7", 0xffff2435, t6, t7);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbra $t5, $t3", 0xabababab, t5, t3);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbra $t2, $t4", 0xfc79b4d2, t2, t4);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbra $t0, $t8", 0x00000000, t0, t8);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbra $t4, $t4", 0x00354565, t4, t4);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbra $t0, $t1", 0x00086755, t0, t1);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbra $t2, $t3", 0x8f8f8f8f, t2, t3);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbra $t4, $t1", 0xeeeeeeee, t4, t1);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbra $t6, $t7", 0x1bdbdbdb, t6, t7);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbra $t5, $t3", 0xdecadeca, t5, t3);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbra $t2, $t4", 0x93474bde, t2, t4);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbra $t0, $t8", 0xfc0007ff, t0, t8);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbra $t4, $t4", 0xffffffff, t4, t4);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbra $t0, $t1", 0xcb4ab48f, t0, t1);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbra $t2, $t3", 0xaf8f7e18, t2, t3);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbra $t4, $t1", 0x87df4510, t4, t1);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbra $t6, $t7", 0xabf4e8e1, t6, t7);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbra $t5, $t3", 0xf4c0eeac, t5, t3);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbra $t2, $t4", 0x006a54f2, t2, t4);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbra $t0, $t8", 0x79f74493, t0, t8);
   TESTDSPINST_RD_RT_NODSPC("preceu.ph.qbra $t4, $t4", 0x9c09e313, t4, t4);

   printf("-------- PRECRQ.QB.PH --------\n");
   TESTDSPINST_RD_RS_RT_NODSPC("precrq.qb.ph $t0, $t1, $t2", 0x00000000,
                               0x00000000, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("precrq.qb.ph $t2, $t3, $t4", 0x045fb232,
                               0x00028632, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("precrq.qb.ph $t4, $t1, $t5", 0xfabc3435,
                               0xfabc3421, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("precrq.qb.ph $t6, $t7, $t3", 0x07654cb8,
                               0x734680bc, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("precrq.qb.ph $t5, $t3, $t2", 0xf973437b,
                               0x80000000, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("precrq.qb.ph $t2, $t4, $t8", 0x00ff0001,
                               0xff01ffff, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("precrq.qb.ph $t0, $t8, $t0", 0x7fff7fff,
                               0x7fff7fff, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("precrq.qb.ph $t4, $t6, $t1", 0x0000c420,
                               0x00000555, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("precrq.qb.ph $t0, $t1, $t2", 0x00000000,
                               0x00000000, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("precrq.qb.ph $t2, $t3, $t4", 0x80000000,
                               0x80000000, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("precrq.qb.ph $t4, $t1, $t5", 0xaaaaaaaa,
                               0x55555555, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("precrq.qb.ph $t6, $t7, $t3", 0x00000018,
                               0xffff2435, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("precrq.qb.ph $t5, $t3, $t2", 0xbabababa,
                               0xabababab, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("precrq.qb.ph $t2, $t4, $t8", 0xf0f0f0f0,
                               0xfc79b4d2, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("precrq.qb.ph $t0, $t8, $t0", 0xfbde3976,
                               0x00000000, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("precrq.qb.ph $t4, $t6, $t1", 0x23534870,
                               0x00354565, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("precrq.qb.ph $t0, $t1, $t2", 0x980b7cde,
                               0x00086755, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("precrq.qb.ph $t2, $t3, $t4", 0x00000018,
                               0x8f8f8f8f, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("precrq.qb.ph $t4, $t1, $t5", 0x92784656,
                               0xeeeeeeee, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("precrq.qb.ph $t6, $t7, $t3", 0xcacacaca,
                               0x1bdbdbdb, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("precrq.qb.ph $t5, $t3, $t2", 0xbacabaca,
                               0xdecadeca, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("precrq.qb.ph $t2, $t4, $t8", 0x12fadeb4,
                               0x93474bde, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("precrq.qb.ph $t0, $t8, $t0", 0x7c000790,
                               0xfc0007ff, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("precrq.qb.ph $t4, $t6, $t1", 0xffffffff,
                               0xffffffff, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("precrq.qb.ph $t0, $t1, $t2", 0xf2f4df1f,
                               0xcb4ab48f, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("precrq.qb.ph $t2, $t3, $t4", 0x435f909a,
                               0xaf8f7e18, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("precrq.qb.ph $t4, $t1, $t5", 0x2106ba5f,
                               0x87df4510, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("precrq.qb.ph $t6, $t7, $t3", 0x246a6376,
                               0xabf4e8e1, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("precrq.qb.ph $t5, $t3, $t2", 0x1046a1a3,
                               0xf4c0eeac, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("precrq.qb.ph $t2, $t4, $t8", 0x638ca515,
                               0x006a54f2, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("precrq.qb.ph $t0, $t8, $t0", 0xf63e7a9d,
                               0x79f74493, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("precrq.qb.ph $t4, $t6, $t1", 0xbd6845cd,
                               0x9c09e313, t4, t6, t1);

   printf("-------- PRECRQ.PH.W --------\n");
   TESTDSPINST_RD_RS_RT_NODSPC("precrq.ph.w $t0, $t1, $t2", 0x00000000,
                               0x00000000, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("precrq.ph.w $t2, $t3, $t4", 0x045fb232,
                               0x00028632, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("precrq.ph.w $t4, $t1, $t5", 0xfabc3435,
                               0xfabc3421, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("precrq.ph.w $t6, $t7, $t3", 0x07654cb8,
                               0x734680bc, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("precrq.ph.w $t5, $t3, $t2", 0xf973437b,
                               0x80000000, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("precrq.ph.w $t2, $t4, $t8", 0x00ff0001,
                               0xff01ffff, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("precrq.ph.w $t0, $t8, $t0", 0x7fff7fff,
                               0x7fff7fff, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("precrq.ph.w $t4, $t6, $t1", 0x0000c420,
                               0x00000555, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("precrq.ph.w $t0, $t1, $t2", 0x00000000,
                               0x00000000, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("precrq.ph.w $t2, $t3, $t4", 0x80000000,
                               0x80000000, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("precrq.ph.w $t4, $t1, $t5", 0xaaaaaaaa,
                               0x55555555, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("precrq.ph.w $t6, $t7, $t3", 0x00000018,
                               0xffff2435, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("precrq.ph.w $t5, $t3, $t2", 0xbabababa,
                               0xabababab, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("precrq.ph.w $t2, $t4, $t8", 0xf0f0f0f0,
                               0xfc79b4d2, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("precrq.ph.w $t0, $t8, $t0", 0xfbde3976,
                               0x00000000, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("precrq.ph.w $t4, $t6, $t1", 0x23534870,
                               0x00354565, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("precrq.ph.w $t0, $t1, $t2", 0x980b7cde,
                               0x00086755, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("precrq.ph.w $t2, $t3, $t4", 0x00000018,
                               0x8f8f8f8f, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("precrq.ph.w $t4, $t1, $t5", 0x92784656,
                               0xeeeeeeee, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("precrq.ph.w $t6, $t7, $t3", 0xcacacaca,
                               0x1bdbdbdb, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("precrq.ph.w $t5, $t3, $t2", 0xbacabaca,
                               0xdecadeca, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("precrq.ph.w $t2, $t4, $t8", 0x12fadeb4,
                               0x93474bde, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("precrq.ph.w $t0, $t8, $t0", 0x7c000790,
                               0xfc0007ff, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("precrq.ph.w $t4, $t6, $t1", 0xffffffff,
                               0xffffffff, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("precrq.ph.w $t0, $t1, $t2", 0xf2f4df1f,
                               0xcb4ab48f, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("precrq.ph.w $t2, $t3, $t4", 0x435f909a,
                               0xaf8f7e18, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("precrq.ph.w $t4, $t1, $t5", 0x2106ba5f,
                               0x87df4510, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("precrq.ph.w $t6, $t7, $t3", 0x246a6376,
                               0xabf4e8e1, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("precrq.ph.w $t5, $t3, $t2", 0x1046a1a3,
                               0xf4c0eeac, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("precrq.ph.w $t2, $t4, $t8", 0x638ca515,
                               0x006a54f2, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("precrq.ph.w $t0, $t8, $t0", 0xf63e7a9d,
                               0x79f74493, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("precrq.ph.w $t4, $t6, $t1", 0xbd6845cd,
                               0x9c09e313, t4, t6, t1);

   printf("-------- PRECRQ_RS.PH.W --------\n");
   TESTDSPINST_RD_RS_RT_DSPC("precrq_rs.ph.w $t0, $t1, $t2", 0x00000000,
                             0x00000000, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("precrq_rs.ph.w $t2, $t3, $t4", 0x045fb232,
                             0x00028632, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("precrq_rs.ph.w $t4, $t1, $t5", 0xfabc3435,
                             0xfabc3421, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("precrq_rs.ph.w $t6, $t7, $t3", 0x07654cb8,
                             0x734680bc, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("precrq_rs.ph.w $t5, $t3, $t2", 0xf973437b,
                             0x80000000, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("precrq_rs.ph.w $t2, $t4, $t8", 0x00ff0001,
                             0xff01ffff, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("precrq_rs.ph.w $t0, $t8, $t0", 0x7fffd004,
                             0x7fff7fff, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("precrq_rs.ph.w $t0, $t8, $t0", 0x7fffd004,
                             0x7fff7fff, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("precrq_rs.ph.w $t4, $t6, $t1", 0x0000c420,
                             0x00000555, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("precrq_rs.ph.w $t0, $t1, $t2", 0x7fff8000,
                             0xffff8000, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("precrq_rs.ph.w $t2, $t3, $t4", 0x80000000,
                             0x80000000, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("precrq_rs.ph.w $t4, $t1, $t5", 0x7fffaaaa,
                             0x55555555, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("precrq_rs.ph.w $t6, $t7, $t3", 0x00000018,
                             0xffff2435, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("precrq_rs.ph.w $t5, $t3, $t2", 0xbabababa,
                             0xabababab, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("precrq_rs.ph.w $t2, $t4, $t8", 0xf0f0f0f0,
                             0xfc79b4d2, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("precrq_rs.ph.w $t0, $t8, $t0", 0xfbde3976,
                             0x00000000, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("precrq_rs.ph.w $t4, $t6, $t1", 0x23534870,
                             0x00354565, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("precrq_rs.ph.w $t0, $t1, $t2", 0x980b7cde,
                             0x00086755, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("precrq_rs.ph.w $t2, $t3, $t4", 0x00000018,
                             0x8f8f8f8f, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("precrq_rs.ph.w $t4, $t1, $t5", 0x92784656,
                             0xeeeeeeee, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("precrq_rs.ph.w $t6, $t7, $t3", 0xcacacaca,
                             0x1bdbdbdb, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("precrq_rs.ph.w $t5, $t3, $t2", 0xbacabaca,
                             0xdecadeca, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("precrq_rs.ph.w $t2, $t4, $t8", 0x12fadeb4,
                             0x93474bde, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("precrq_rs.ph.w $t0, $t8, $t0", 0x7fffffff,
                             0xfc0007ff, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("precrq_rs.ph.w $t4, $t6, $t1", 0xffffffff,
                             0xffffffff, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("precrq_rs.ph.w $t0, $t1, $t2", 0xf2f4df1f,
                             0xcb4ab48f, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("precrq_rs.ph.w $t2, $t3, $t4", 0x435f909a,
                             0xaf8f7e18, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("precrq_rs.ph.w $t4, $t1, $t5", 0x2106ba5f,
                             0x87df4510, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("precrq_rs.ph.w $t6, $t7, $t3", 0x246a6376,
                             0xabf4e8e1, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("precrq_rs.ph.w $t5, $t3, $t2", 0x1046a1a3,
                             0xf4c0eeac, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("precrq_rs.ph.w $t2, $t4, $t8", 0x638ca515,
                             0x006a54f2, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("precrq_rs.ph.w $t0, $t8, $t0", 0xf63e7a9d,
                             0x79f74493, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("precrq_rs.ph.w $t4, $t6, $t1", 0xbd6845cd,
                             0x9c09e313, t4, t6, t1);

   printf("-------- PRECRQU_S.QB.PH --------\n");
   TESTDSPINST_RD_RS_RT_DSPC("precrqu_s.qb.ph $t0, $t1, $t2", 0x00000000,
                             0x00000000, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("precrqu_s.qb.ph $t2, $t3, $t4", 0x045fb232,
                             0x00028632, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("precrqu_s.qb.ph $t4, $t1, $t5", 0xfabc3435,
                             0xfabc3421, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("precrqu_s.qb.ph $t6, $t7, $t3", 0x07654cb8,
                             0x734680bc, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("precrqu_s.qb.ph $t5, $t3, $t2", 0xf973437b,
                             0x80000000, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("precrqu_s.qb.ph $t2, $t4, $t8", 0x00ff0001,
                             0xff01ffff, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("precrqu_s.qb.ph $t0, $t8, $t0", 0x7fff7004,
                             0x7fff7fff, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("precrqu_s.qb.ph $t4, $t6, $t1", 0x0000c420,
                             0x00000555, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("precrqu_s.qb.ph $t0, $t1, $t2", 0x00000000,
                             0x00000000, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("precrqu_s.qb.ph $t2, $t3, $t4", 0x80000000,
                             0x80000000, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("precrqu_s.qb.ph $t4, $t1, $t5", 0xaaaaaaaa,
                             0x55555555, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("precrqu_s.qb.ph $t6, $t7, $t3", 0x00000018,
                             0xffff2435, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("precrqu_s.qb.ph $t5, $t3, $t2", 0xbabababa,
                             0xabababab, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("precrqu_s.qb.ph $t2, $t4, $t8", 0xf0f0f0f0,
                             0xfc79b4d2, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("precrqu_s.qb.ph $t0, $t8, $t0", 0xfbde3976,
                             0x00000000, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("precrqu_s.qb.ph $t4, $t6, $t1", 0x23534870,
                             0x00354565, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("precrqu_s.qb.ph $t0, $t1, $t2", 0x980b7cde,
                             0x00086755, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("precrqu_s.qb.ph $t2, $t3, $t4", 0x00000018,
                             0x8f8f8f8f, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("precrqu_s.qb.ph $t4, $t1, $t5", 0x92784656,
                             0xeeeeeeee, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("precrqu_s.qb.ph $t6, $t7, $t3", 0xcacacaca,
                             0x1bdbdbdb, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("precrqu_s.qb.ph $t5, $t3, $t2", 0xbacabaca,
                             0xdecadeca, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("precrqu_s.qb.ph $t2, $t4, $t8", 0x12fadeb4,
                             0x93474bde, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("precrqu_s.qb.ph $t0, $t8, $t0", 0x7c000790,
                             0xfc0007ff, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("precrqu_s.qb.ph $t4, $t6, $t1", 0xffffffff,
                             0xffffffff, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("precrqu_s.qb.ph $t0, $t1, $t2", 0xf2f4df1f,
                             0xcb4ab48f, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("precrqu_s.qb.ph $t2, $t3, $t4", 0x435f909a,
                             0xaf8f7e18, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("precrqu_s.qb.ph $t4, $t1, $t5", 0x2106ba5f,
                             0x87df4510, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("precrqu_s.qb.ph $t6, $t7, $t3", 0x246a6376,
                             0xabf4e8e1, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("precrqu_s.qb.ph $t5, $t3, $t2", 0x1046a1a3,
                             0xf4c0eeac, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("precrqu_s.qb.ph $t2, $t4, $t8", 0x638ca515,
                             0x006a54f2, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("precrqu_s.qb.ph $t0, $t8, $t0", 0xf63e7a9d,
                             0x79f74493, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("precrqu_s.qb.ph $t4, $t6, $t1", 0xbd6845cd,
                             0x9c09e313, t4, t6, t1);

   printf("-------- RADDU.W.QB --------\n");
   TESTDSPINST_RADDU_W_QB("raddu.w.qb $t3, $t2", 0x55555555, t3, t2);
   TESTDSPINST_RADDU_W_QB("raddu.w.qb $t4, $t1", 0xffff2435, t4, t1);
   TESTDSPINST_RADDU_W_QB("raddu.w.qb $t7, $t2", 0x55555555, t7, t2);
   TESTDSPINST_RADDU_W_QB("raddu.w.qb $t5, $t1", 0xffff2435, t5, t1);
   TESTDSPINST_RADDU_W_QB("raddu.w.qb $t2, $t2", 0x55435755, t2, t2);
   TESTDSPINST_RADDU_W_QB("raddu.w.qb $t3, $t1", 0xffff2435, t3, t1);
   TESTDSPINST_RADDU_W_QB("raddu.w.qb $t1, $t2", 0x0fde3126, t1, t2);
   TESTDSPINST_RADDU_W_QB("raddu.w.qb $t2, $t0", 0xabababab, t2, t0);
   TESTDSPINST_RADDU_W_QB("raddu.w.qb $t7, $t3", 0x00000001, t7, t3);
   TESTDSPINST_RADDU_W_QB("raddu.w.qb $t5, $t6", 1073741824, t5, t6);
   TESTDSPINST_RADDU_W_QB("raddu.w.qb $t2, $t4", 0x80000000, t2, t4);
   TESTDSPINST_RADDU_W_QB("raddu.w.qb $t6, $t1", 0x7fffffff, t6, t1);
   TESTDSPINST_RADDU_W_QB("raddu.w.qb $t8, $t5", 0x23534870, t8, t5);
   TESTDSPINST_RADDU_W_QB("raddu.w.qb $t9, $t7", 0xffffffff, t9, t7);
   TESTDSPINST_RADDU_W_QB("raddu.w.qb $t3, $t8", 0xfc79b4d2, t3, t8);
   TESTDSPINST_RADDU_W_QB("raddu.w.qb $t4, $t4", 0x00000000, t4, t4);
   TESTDSPINST_RADDU_W_QB("raddu.w.qb $t5, $t5", 0x00000000, t5, t5);
   TESTDSPINST_RADDU_W_QB("raddu.w.qb $t6, $t6", 0x12349876, t6, t6);
   TESTDSPINST_RADDU_W_QB("raddu.w.qb $t7, $t7", 0x00354565, t7, t7);
   TESTDSPINST_RADDU_W_QB("raddu.w.qb $t8, $t8", 0x00086755, t8, t8);
   TESTDSPINST_RADDU_W_QB("raddu.w.qb $t9, $t0", 0x8f8f8f8f, t9, t0);
   TESTDSPINST_RADDU_W_QB("raddu.w.qb $t1, $t1", 0xeeeeeeee, t1, t1);
   TESTDSPINST_RADDU_W_QB("raddu.w.qb $t1, $t2", 0x1bdbdbdb, t1, t2);
   TESTDSPINST_RADDU_W_QB("raddu.w.qb $t4, $t3", 0xdecadeca, t4, t3);
   TESTDSPINST_RADDU_W_QB("raddu.w.qb $t8, $t4", 0x5fc92974, t8, t4);
   TESTDSPINST_RADDU_W_QB("raddu.w.qb $t0, $t5", 0x7e08184e, t0, t5);
   TESTDSPINST_RADDU_W_QB("raddu.w.qb $t1, $t6", 0x71c8315f, t1, t6);
   TESTDSPINST_RADDU_W_QB("raddu.w.qb $t2, $t7", 0x9493110e, t2, t7);
   TESTDSPINST_RADDU_W_QB("raddu.w.qb $t3, $t8", 0xbb246228, t3, t8);
   TESTDSPINST_RADDU_W_QB("raddu.w.qb $t4, $t0", 0x339d8d88, t4, t0);
   TESTDSPINST_RADDU_W_QB("raddu.w.qb $t5, $t1", 0x70974249, t5, t1);
   TESTDSPINST_RADDU_W_QB("raddu.w.qb $t6, $t2", 0x8a8d4e7d, t6, t2);
   TESTDSPINST_RADDU_W_QB("raddu.w.qb $t7, $t1", 0xeb1b4335, t7, t1);
   TESTDSPINST_RADDU_W_QB("raddu.w.qb $t8, $t2", 0x0cd6b508, t8, t2);
   TESTDSPINST_RADDU_W_QB("raddu.w.qb $t0, $t1", 0x6731e282, t0, t1);
   TESTDSPINST_RADDU_W_QB("raddu.w.qb $t1, $t2", 0xb6edf28f, t1, t2);
   TESTDSPINST_RADDU_W_QB("raddu.w.qb $t2, $t3", 0x4b4ec9ca, t2, t3);
   TESTDSPINST_RADDU_W_QB("raddu.w.qb $t3, $t4", 0xc1037fa4, t3, t4);
   TESTDSPINST_RADDU_W_QB("raddu.w.qb $t3, $t5", 0xcb4ab48f, t3, t5);
   TESTDSPINST_RADDU_W_QB("raddu.w.qb $t4, $t6", 0xaf8f7e18, t4, t6);
   TESTDSPINST_RADDU_W_QB("raddu.w.qb $t4, $t6", 0xaf8f7e18, t4, t6);

   printf("-------- RDDSP/WRDSP --------\n");
   TESTDSPINST_RDDSPWRDSP(0x35055512, 0x0000001f);
   TESTDSPINST_RDDSPWRDSP(0x00000000, 0x00000002);
   TESTDSPINST_RDDSPWRDSP(0x80003286, 0x00000004);
   TESTDSPINST_RDDSPWRDSP(0xfabc2435, 0x00000008);
   TESTDSPINST_RDDSPWRDSP(0x73468000, 0x00000016);
   TESTDSPINST_RDDSPWRDSP(0x80000000, 0x00000020);
   TESTDSPINST_RDDSPWRDSP(0xffffffff, 0x00000022);
   TESTDSPINST_RDDSPWRDSP(0xfff45fff, 0x0000003f);
   TESTDSPINST_RDDSPWRDSP(0x00000555, 0x00000013);
   TESTDSPINST_RDDSPWRDSP(0x23534870, 0x00000014);
   TESTDSPINST_RDDSPWRDSP(0x0555adec, 0x00000010);
   TESTDSPINST_RDDSPWRDSP(0x980b7cde, 0x00000015);
   TESTDSPINST_RDDSPWRDSP(0xf973437b, 0x00000011);
   TESTDSPINST_RDDSPWRDSP(0x93474bde, 0x00000007);
   TESTDSPINST_RDDSPWRDSP(0x55555555, 0x00000009);
   TESTDSPINST_RDDSPWRDSP(0xc4dbfe20, 0x00000006);
   TESTDSPINST_RDDSPWRDSP(0x734680bc, 0x00000000);
   TESTDSPINST_RDDSPWRDSP(0x00354565, 0x00000003);
   TESTDSPINST_RDDSPWRDSP(0xbacabaca, 0x00000021);
   TESTDSPINST_RDDSPWRDSP(0xdecadeca, 0x00000016);
   TESTDSPINST_RDDSPWRDSP(0x00000286, 0x00000001);
   TESTDSPINST_RDDSPWRDSP(0xabababab, 0x00000026);

   printf("-------- REPL.PH --------\n");
   TESTDSPINST_RD_IMM_NODSPC("repl.ph $t0, 0", 0, t0);
   TESTDSPINST_RD_IMM_NODSPC("repl.ph $t1, 1", 1, t1);
   TESTDSPINST_RD_IMM_NODSPC("repl.ph $t2, -1", -1, t2);
   TESTDSPINST_RD_IMM_NODSPC("repl.ph $t3, -129", -129, t3);
   TESTDSPINST_RD_IMM_NODSPC("repl.ph $t4, -2", -2, t4);
   TESTDSPINST_RD_IMM_NODSPC("repl.ph $t0, 0x123", 0x123, t0);
   TESTDSPINST_RD_IMM_NODSPC("repl.ph $t1, 0x07b", 0x07b, t1);
   TESTDSPINST_RD_IMM_NODSPC("repl.ph $t2, 0x1c8", 0x1c8, t2);
   TESTDSPINST_RD_IMM_NODSPC("repl.ph $t3, 0x080", 0x080, t3);
   TESTDSPINST_RD_IMM_NODSPC("repl.ph $t4, 0x07f", 0x07f, t4);
   TESTDSPINST_RD_IMM_NODSPC("repl.ph $t5, 0x1ff", 0x1ff, t5);
   TESTDSPINST_RD_IMM_NODSPC("repl.ph $t6, 0x000", 0x000, t6);
   TESTDSPINST_RD_IMM_NODSPC("repl.ph $t7, 0x177", 0x177, t7);
   TESTDSPINST_RD_IMM_NODSPC("repl.ph $t0, 0x1de", 0x1de, t0);
   TESTDSPINST_RD_IMM_NODSPC("repl.ph $t1, 0x018", 0x018, t1);
   TESTDSPINST_RD_IMM_NODSPC("repl.ph $t2, 0x056", 0x056, t2);
   TESTDSPINST_RD_IMM_NODSPC("repl.ph $t3, 0x1ca", 0x1ca, t3);
   TESTDSPINST_RD_IMM_NODSPC("repl.ph $t4, 0x1ab", 0x1ab, t4);
   TESTDSPINST_RD_IMM_NODSPC("repl.ph $t5, 0x1d2", 0x1d2, t5);
   TESTDSPINST_RD_IMM_NODSPC("repl.ph $t6, 0x000", 0x000, t6);
   TESTDSPINST_RD_IMM_NODSPC("repl.ph $t7, 0x065", 0x065, t7);
   TESTDSPINST_RD_IMM_NODSPC("repl.ph $t0, 0x055", 0x055, t0);
   TESTDSPINST_RD_IMM_NODSPC("repl.ph $t1, 0x08f", 0x08f, t1);
   TESTDSPINST_RD_IMM_NODSPC("repl.ph $t2, 0x0ee", 0x0ee, t2);
   TESTDSPINST_RD_IMM_NODSPC("repl.ph $t3, 0x1db", 0x1db, t3);
   TESTDSPINST_RD_IMM_NODSPC("repl.ph $t4, 0x1ca", 0x1ca, t4);
   TESTDSPINST_RD_IMM_NODSPC("repl.ph $t5, 0x1de", 0x1de, t5);
   TESTDSPINST_RD_IMM_NODSPC("repl.ph $t6, 0x0ff", 0x0ff, t6);
   TESTDSPINST_RD_IMM_NODSPC("repl.ph $t7, 0x0ff", 0x0ff, t7);
   TESTDSPINST_RD_IMM_NODSPC("repl.ph $t0, 0x08f", 0x08f, t0);
   TESTDSPINST_RD_IMM_NODSPC("repl.ph $t1, 0x118", 0x118, t1);
   TESTDSPINST_RD_IMM_NODSPC("repl.ph $t2, 0x110", 0x110, t2);
   TESTDSPINST_RD_IMM_NODSPC("repl.ph $t3, 0x1e1", 0x1e1, t3);
   TESTDSPINST_RD_IMM_NODSPC("repl.ph $t4, 0x1ac", 0x1ac, t4);
   TESTDSPINST_RD_IMM_NODSPC("repl.ph $t5, 0x0f2", 0x0f2, t5);
   TESTDSPINST_RD_IMM_NODSPC("repl.ph $t6, 0x093", 0x093, t6);
   TESTDSPINST_RD_IMM_NODSPC("repl.ph $t7, 0x013", 0x013, t7);

   printf("-------- REPL.QB --------\n");
   TESTDSPINST_RD_IMM_NODSPC("repl.qb $t0, 0x23", 0x23, t0);
   TESTDSPINST_RD_IMM_NODSPC("repl.qb $t1, 0x7b", 0x7b, t1);
   TESTDSPINST_RD_IMM_NODSPC("repl.qb $t2, 0xc8", 0xc8, t2);
   TESTDSPINST_RD_IMM_NODSPC("repl.qb $t3, 0x80", 0x80, t3);
   TESTDSPINST_RD_IMM_NODSPC("repl.qb $t4, 0x7f", 0x7f, t4);
   TESTDSPINST_RD_IMM_NODSPC("repl.qb $t5, 0xff", 0xff, t5);
   TESTDSPINST_RD_IMM_NODSPC("repl.qb $t6, 0x00", 0x00, t6);
   TESTDSPINST_RD_IMM_NODSPC("repl.qb $t7, 0x77", 0x77, t7);
   TESTDSPINST_RD_IMM_NODSPC("repl.qb $t0, 0xde", 0xde, t0);
   TESTDSPINST_RD_IMM_NODSPC("repl.qb $t1, 0x18", 0x18, t1);
   TESTDSPINST_RD_IMM_NODSPC("repl.qb $t2, 0x56", 0x56, t2);
   TESTDSPINST_RD_IMM_NODSPC("repl.qb $t3, 0xca", 0xca, t3);
   TESTDSPINST_RD_IMM_NODSPC("repl.qb $t4, 0xab", 0xab, t4);
   TESTDSPINST_RD_IMM_NODSPC("repl.qb $t5, 0xd2", 0xd2, t5);
   TESTDSPINST_RD_IMM_NODSPC("repl.qb $t6, 0x00", 0x00, t6);
   TESTDSPINST_RD_IMM_NODSPC("repl.qb $t7, 0x65", 0x65, t7);
   TESTDSPINST_RD_IMM_NODSPC("repl.qb $t0, 0x55", 0x55, t0);
   TESTDSPINST_RD_IMM_NODSPC("repl.qb $t1, 0x8f", 0x8f, t1);
   TESTDSPINST_RD_IMM_NODSPC("repl.qb $t2, 0xee", 0xee, t2);
   TESTDSPINST_RD_IMM_NODSPC("repl.qb $t3, 0xdb", 0xdb, t3);
   TESTDSPINST_RD_IMM_NODSPC("repl.qb $t4, 0xca", 0xca, t4);
   TESTDSPINST_RD_IMM_NODSPC("repl.qb $t5, 0xde", 0xde, t5);
   TESTDSPINST_RD_IMM_NODSPC("repl.qb $t6, 0xff", 0xff, t6);
   TESTDSPINST_RD_IMM_NODSPC("repl.qb $t7, 0xff", 0xff, t7);
   TESTDSPINST_RD_IMM_NODSPC("repl.qb $t0, 0x8f", 0x8f, t0);
   TESTDSPINST_RD_IMM_NODSPC("repl.qb $t1, 0x18", 0x18, t1);
   TESTDSPINST_RD_IMM_NODSPC("repl.qb $t2, 0x10", 0x10, t2);
   TESTDSPINST_RD_IMM_NODSPC("repl.qb $t3, 0xe1", 0xe1, t3);
   TESTDSPINST_RD_IMM_NODSPC("repl.qb $t4, 0xac", 0xac, t4);
   TESTDSPINST_RD_IMM_NODSPC("repl.qb $t5, 0xf2", 0xf2, t5);
   TESTDSPINST_RD_IMM_NODSPC("repl.qb $t6, 0x93", 0x93, t6);
   TESTDSPINST_RD_IMM_NODSPC("repl.qb $t7, 0x13", 0x13, t7);

   printf("-------- REPLV.PH --------\n");
   TESTDSPINST_RD_RT_NODSPC("replv.ph $t0, $t1", 0x00000000, t0, t1);
   TESTDSPINST_RD_RT_NODSPC("replv.ph $t2, $t3", 0x80003286, t2, t3);
   TESTDSPINST_RD_RT_NODSPC("replv.ph $t4, $t1", 0xfabc2435, t4, t1);
   TESTDSPINST_RD_RT_NODSPC("replv.ph $t6, $t7", 0x73468000, t6, t7);
   TESTDSPINST_RD_RT_NODSPC("replv.ph $t5, $t3", 0x80000000, t5, t3);
   TESTDSPINST_RD_RT_NODSPC("replv.ph $t2, $t4", 0xffffffff, t2, t4);
   TESTDSPINST_RD_RT_NODSPC("replv.ph $t0, $t8", 0xfff45fff, t0, t8);
   TESTDSPINST_RD_RT_NODSPC("replv.ph $t4, $t4", 0x00000555, t4, t4);
   TESTDSPINST_RD_RT_NODSPC("replv.ph $t0, $t1", 0x00005340, t0, t1);
   TESTDSPINST_RD_RT_NODSPC("replv.ph $t2, $t3", 0x80000000, t2, t3);
   TESTDSPINST_RD_RT_NODSPC("replv.ph $t4, $t1", 0x55555555, t4, t1);
   TESTDSPINST_RD_RT_NODSPC("replv.ph $t6, $t7", 0xffff2435, t6, t7);
   TESTDSPINST_RD_RT_NODSPC("replv.ph $t5, $t3", 0xabababab, t5, t3);
   TESTDSPINST_RD_RT_NODSPC("replv.ph $t2, $t4", 0xfc79b4d2, t2, t4);
   TESTDSPINST_RD_RT_NODSPC("replv.ph $t0, $t8", 0x00000000, t0, t8);
   TESTDSPINST_RD_RT_NODSPC("replv.ph $t4, $t4", 0x00354565, t4, t4);
   TESTDSPINST_RD_RT_NODSPC("replv.ph $t0, $t1", 0x00086755, t0, t1);
   TESTDSPINST_RD_RT_NODSPC("replv.ph $t2, $t3", 0x8f8f8f8f, t2, t3);
   TESTDSPINST_RD_RT_NODSPC("replv.ph $t4, $t1", 0xeeeeeeee, t4, t1);
   TESTDSPINST_RD_RT_NODSPC("replv.ph $t6, $t7", 0x1bdbdbdb, t6, t7);
   TESTDSPINST_RD_RT_NODSPC("replv.ph $t5, $t3", 0xdecadeca, t5, t3);
   TESTDSPINST_RD_RT_NODSPC("replv.ph $t2, $t4", 0x93474bde, t2, t4);
   TESTDSPINST_RD_RT_NODSPC("replv.ph $t0, $t8", 0xfc0007ff, t0, t8);
   TESTDSPINST_RD_RT_NODSPC("replv.ph $t4, $t4", 0xffffffff, t4, t4);
   TESTDSPINST_RD_RT_NODSPC("replv.ph $t0, $t1", 0xcb4ab48f, t0, t1);
   TESTDSPINST_RD_RT_NODSPC("replv.ph $t2, $t3", 0xaf8f7e18, t2, t3);
   TESTDSPINST_RD_RT_NODSPC("replv.ph $t4, $t1", 0x87df4510, t4, t1);
   TESTDSPINST_RD_RT_NODSPC("replv.ph $t6, $t7", 0xabf4e8e1, t6, t7);
   TESTDSPINST_RD_RT_NODSPC("replv.ph $t5, $t3", 0xf4c0eeac, t5, t3);
   TESTDSPINST_RD_RT_NODSPC("replv.ph $t2, $t4", 0x006a54f2, t2, t4);
   TESTDSPINST_RD_RT_NODSPC("replv.ph $t0, $t8", 0x79f74493, t0, t8);
   TESTDSPINST_RD_RT_NODSPC("replv.ph $t4, $t4", 0x9c09e313, t4, t4);

   printf("-------- REPLV.QB --------\n");
   TESTDSPINST_RD_RT_NODSPC("replv.qb $t0, $t1", 0x00000000, t0, t1);
   TESTDSPINST_RD_RT_NODSPC("replv.qb $t2, $t3", 0x80003286, t2, t3);
   TESTDSPINST_RD_RT_NODSPC("replv.qb $t4, $t1", 0xfabc2435, t4, t1);
   TESTDSPINST_RD_RT_NODSPC("replv.qb $t6, $t7", 0x73468000, t6, t7);
   TESTDSPINST_RD_RT_NODSPC("replv.qb $t5, $t3", 0x80000000, t5, t3);
   TESTDSPINST_RD_RT_NODSPC("replv.qb $t2, $t4", 0xffffffff, t2, t4);
   TESTDSPINST_RD_RT_NODSPC("replv.qb $t0, $t8", 0xfff45fff, t0, t8);
   TESTDSPINST_RD_RT_NODSPC("replv.qb $t4, $t4", 0x00000555, t4, t4);
   TESTDSPINST_RD_RT_NODSPC("replv.qb $t0, $t1", 0x00005340, t0, t1);
   TESTDSPINST_RD_RT_NODSPC("replv.qb $t2, $t3", 0x80000000, t2, t3);
   TESTDSPINST_RD_RT_NODSPC("replv.qb $t4, $t1", 0x55555555, t4, t1);
   TESTDSPINST_RD_RT_NODSPC("replv.qb $t6, $t7", 0xffff2435, t6, t7);
   TESTDSPINST_RD_RT_NODSPC("replv.qb $t5, $t3", 0xabababab, t5, t3);
   TESTDSPINST_RD_RT_NODSPC("replv.qb $t2, $t4", 0xfc79b4d2, t2, t4);
   TESTDSPINST_RD_RT_NODSPC("replv.qb $t0, $t8", 0x00000000, t0, t8);
   TESTDSPINST_RD_RT_NODSPC("replv.qb $t4, $t4", 0x00354565, t4, t4);
   TESTDSPINST_RD_RT_NODSPC("replv.qb $t0, $t1", 0x00086755, t0, t1);
   TESTDSPINST_RD_RT_NODSPC("replv.qb $t2, $t3", 0x8f8f8f8f, t2, t3);
   TESTDSPINST_RD_RT_NODSPC("replv.qb $t4, $t1", 0xeeeeeeee, t4, t1);
   TESTDSPINST_RD_RT_NODSPC("replv.qb $t6, $t7", 0x1bdbdbdb, t6, t7);
   TESTDSPINST_RD_RT_NODSPC("replv.qb $t5, $t3", 0xdecadeca, t5, t3);
   TESTDSPINST_RD_RT_NODSPC("replv.qb $t2, $t4", 0x93474bde, t2, t4);
   TESTDSPINST_RD_RT_NODSPC("replv.qb $t0, $t8", 0xfc0007ff, t0, t8);
   TESTDSPINST_RD_RT_NODSPC("replv.qb $t4, $t4", 0xffffffff, t4, t4);
   TESTDSPINST_RD_RT_NODSPC("replv.qb $t0, $t1", 0xcb4ab48f, t0, t1);
   TESTDSPINST_RD_RT_NODSPC("replv.qb $t2, $t3", 0xaf8f7e18, t2, t3);
   TESTDSPINST_RD_RT_NODSPC("replv.qb $t4, $t1", 0x87df4510, t4, t1);
   TESTDSPINST_RD_RT_NODSPC("replv.qb $t6, $t7", 0xabf4e8e1, t6, t7);
   TESTDSPINST_RD_RT_NODSPC("replv.qb $t5, $t3", 0xf4c0eeac, t5, t3);
   TESTDSPINST_RD_RT_NODSPC("replv.qb $t2, $t4", 0x006a54f2, t2, t4);
   TESTDSPINST_RD_RT_NODSPC("replv.qb $t0, $t8", 0x79f74493, t0, t8);
   TESTDSPINST_RD_RT_NODSPC("replv.qb $t4, $t4", 0x9c09e313, t4, t4);

   printf("-------- SHILO --------\n");
   TESTDSPINST_SHILO("ac3", 0x980b7cde, 0x0243ade5, -5);
   TESTDSPINST_SHILO("ac0", 0x80003286, 0x00002340, 12);
   TESTDSPINST_SHILO("ac1", 0x23534870, 0x0bca3470,  7);
   TESTDSPINST_SHILO("ac2", 0x0555adec, 0x1245bef6,  3);
   TESTDSPINST_SHILO("ac3", 0x980b7cde, 0x0243ade5, -5);
   TESTDSPINST_SHILO("ac0", 0xf97343ff, 0x0bce2434, -13);
   TESTDSPINST_SHILO("ac1", 0x93474bde, 0x0bcde433, 31);
   TESTDSPINST_SHILO("ac2", 0x7f003245, 0x000432fe, -32);
   TESTDSPINST_SHILO("ac3", 0xad80bce4, 0x0241bce0,  8);
   TESTDSPINST_SHILO("ac0", 0x55555555, 0xbcdea87a, 20);
   TESTDSPINST_SHILO("ac1", 0x00000000, 0x00000007, 22);
   TESTDSPINST_SHILO("ac2", 0xc4dbfe20, 0x000023b6, -19);
   TESTDSPINST_SHILO("ac3", 0x734680bc, 0x000deab5, 16);
   TESTDSPINST_SHILO("ac0", 0x3545ff80, 0x00000004, -2);
   TESTDSPINST_SHILO("ac1", 0xbacabaca, 0x00000003, -4);
   TESTDSPINST_SHILO("ac2", 0xdecadeca, 0x00000002, -18);
   TESTDSPINST_SHILO("ac3", 0xabababab, 0x00000001,  0);
   TESTDSPINST_SHILO("ac0", 0xffffffff, 0x00000000,  1);
   TESTDSPINST_SHILO("ac1", 0x7fff7fff, 0x0bce3457, 30);
   TESTDSPINST_SHILO("ac2", 0x00010001, 0x00ca6ced, -30);
   TESTDSPINST_SHILO("ac3", 0x00000080, 0x5bc34109, -24);
   TESTDSPINST_SHILO("ac0", 0xff460000, 0x4bacd342,  5);
   TESTDSPINST_SHILO("ac1", 0x2fff0000, 0x03bcde24,  9);
   TESTDSPINST_SHILO("ac2", 0x2fff0000, 0x02234379, 16);
   TESTDSPINST_SHILO("ac3", 0x2fff0000, 0x01098789, -12);

   printf("-------- SHILOV --------\n");
   TESTDSP_SHILOV("ac3", 0x980b7cde, 0x0243ade5, 0x01098789, t5);
   TESTDSP_SHILOV("ac0", 0x80003286, 0x00002340, 0x0241bce0, t1);
   TESTDSP_SHILOV("ac1", 0x23534870, 0x0bca3470, 0xc4dbfe20, t1);
   TESTDSP_SHILOV("ac2", 0x0555adec, 0x1245bef6, 0x93474bde, t3);
   TESTDSP_SHILOV("ac3", 0x980b7cde, 0x0243ade5, 0x7f003245, t7);
   TESTDSP_SHILOV("ac0", 0xf97343ff, 0x0bce2434, 0x0241bce0, t3);
   TESTDSP_SHILOV("ac1", 0x93474bde, 0x0bcde433, 0x0bce3457, t4);
   TESTDSP_SHILOV("ac2", 0x7f003245, 0x000432fe, 0xbacabaca, t8);
   TESTDSP_SHILOV("ac3", 0xad80bce4, 0x0241bce0, 0x734680bc, t1);
   TESTDSP_SHILOV("ac0", 0x55555555, 0xbcdea87a, 0x3545ff80, t3);
   TESTDSP_SHILOV("ac1", 0x00000000, 0x00000007, 0x7fff7fff, t1);
   TESTDSP_SHILOV("ac2", 0xc4dbfe20, 0x000023b6, 0xc4dbfe20, t7);
   TESTDSP_SHILOV("ac3", 0x734680bc, 0x000deab5,         16, t3);
   TESTDSP_SHILOV("ac0", 0x3545ff80, 0x00000004,         -2, t4);
   TESTDSP_SHILOV("ac1", 0xbacabaca, 0x00000003,         -4, t8);
   TESTDSP_SHILOV("ac2", 0xdecadeca, 0x00000002,        -18, t5);
   TESTDSP_SHILOV("ac3", 0xabababab, 0x00000001,          0, t1);
   TESTDSP_SHILOV("ac0", 0xffffffff, 0x00000000,          1, t3);
   TESTDSP_SHILOV("ac1", 0x7fff7fff, 0x0bce3457,         30, t1);
   TESTDSP_SHILOV("ac2", 0x00010001, 0x00ca6ced,        -30, t7);
   TESTDSP_SHILOV("ac3", 0x00000080, 0x5bc34109,        -24, t3);
   TESTDSP_SHILOV("ac0", 0xff460000, 0x4bacd342,        -32, t4);
   TESTDSP_SHILOV("ac1", 0x2fff0000, 0x03bcde24,         31, t8);
   TESTDSP_SHILOV("ac2", 0x2fff0000, 0x02234379,         16, t5);
   TESTDSP_SHILOV("ac3", 0x2fff0000, 0x01098789,        -12, t5);

   printf("-------- SHLL.PH --------\n");
   TESTDSPINST_RD_RT_SA_DSPC("shll.ph $t0, $t1,  0", 0x00000000,  0, t0, t1);
   TESTDSPINST_RD_RT_SA_DSPC("shll.ph $t2, $t3,  1", 0x2fff0000,  1, t2, t3);
   TESTDSPINST_RD_RT_SA_DSPC("shll.ph $t4, $t1,  2", 0x2fff0000,  2, t4, t1);
   TESTDSPINST_RD_RT_SA_DSPC("shll.ph $t6, $t7,  3", 0x2fff0000,  3, t6, t7);
   TESTDSPINST_RD_RT_SA_DSPC("shll.ph $t5, $t3,  4", 0x80000000,  4, t5, t3);
   TESTDSPINST_RD_RT_SA_DSPC("shll.ph $t2, $t4,  5", 0xff01ffff,  5, t2, t4);
   TESTDSPINST_RD_RT_SA_DSPC("shll.ph $t0, $t8,  6", 0x7fff7fff,  6, t0, t8);
   TESTDSPINST_RD_RT_SA_DSPC("shll.ph $t4, $t6,  7", 0x00000555,  7, t4, t6);
   TESTDSPINST_RD_RT_SA_DSPC("shll.ph $t0, $t1,  8", 0x00000000,  8, t0, t1);
   TESTDSPINST_RD_RT_SA_DSPC("shll.ph $t2, $t3,  9", 0x80000000,  9, t2, t3);
   TESTDSPINST_RD_RT_SA_DSPC("shll.ph $t4, $t1, 10", 0x55555555, 10, t4, t1);
   TESTDSPINST_RD_RT_SA_DSPC("shll.ph $t6, $t7, 11", 0xffff2435, 11, t6, t7);
   TESTDSPINST_RD_RT_SA_DSPC("shll.ph $t5, $t3, 12", 0xabababab, 12, t5, t3);
   TESTDSPINST_RD_RT_SA_DSPC("shll.ph $t2, $t4, 13", 0xfc79b4d2, 13, t2, t4);
   TESTDSPINST_RD_RT_SA_DSPC("shll.ph $t0, $t8, 14", 0x00000000, 14, t0, t8);
   TESTDSPINST_RD_RT_SA_DSPC("shll.ph $t4, $t6, 15", 0x00354565, 15, t4, t6);
   TESTDSPINST_RD_RT_SA_DSPC("shll.ph $t0, $t1,  0", 0x00086755,  0, t0, t1);
   TESTDSPINST_RD_RT_SA_DSPC("shll.ph $t2, $t3,  1", 0x8f8f8f8f,  1, t2, t3);
   TESTDSPINST_RD_RT_SA_DSPC("shll.ph $t4, $t1,  2", 0xeeeeeeee,  2, t4, t1);
   TESTDSPINST_RD_RT_SA_DSPC("shll.ph $t6, $t7,  3", 0x1bdbdbdb,  3, t6, t7);
   TESTDSPINST_RD_RT_SA_DSPC("shll.ph $t5, $t3,  4", 0xdecadeca,  4, t5, t3);
   TESTDSPINST_RD_RT_SA_DSPC("shll.ph $t2, $t4,  5", 0x93474bde,  5, t2, t4);
   TESTDSPINST_RD_RT_SA_DSPC("shll.ph $t0, $t8,  6", 0xfc0007ff,  6, t0, t8);
   TESTDSPINST_RD_RT_SA_DSPC("shll.ph $t4, $t6,  7", 0xffffffff,  7, t4, t6);
   TESTDSPINST_RD_RT_SA_DSPC("shll.ph $t0, $t1,  8", 0xcb4ab48f,  8, t0, t1);
   TESTDSPINST_RD_RT_SA_DSPC("shll.ph $t2, $t3,  9", 0xaf8f7e18,  9, t2, t3);
   TESTDSPINST_RD_RT_SA_DSPC("shll.ph $t4, $t1, 10", 0x87df4510, 10, t4, t1);
   TESTDSPINST_RD_RT_SA_DSPC("shll.ph $t6, $t7, 11", 0xabf4e8e1, 11, t6, t7);
   TESTDSPINST_RD_RT_SA_DSPC("shll.ph $t5, $t3, 12", 0xf4c0eeac, 12, t5, t3);
   TESTDSPINST_RD_RT_SA_DSPC("shll.ph $t2, $t4, 13", 0x006a54f2, 13, t2, t4);
   TESTDSPINST_RD_RT_SA_DSPC("shll.ph $t0, $t8, 14", 0x79f74493, 14, t0, t8);
   TESTDSPINST_RD_RT_SA_DSPC("shll.ph $t4, $t6, 15", 0x9c09e313, 15, t4, t6);

   printf("-------- SHLL_S.PH --------\n");
   TESTDSPINST_RD_RT_SA_DSPC("shll_s.ph $t0, $t1,  0", 0x00000000,  0, t0, t1);
   TESTDSPINST_RD_RT_SA_DSPC("shll_s.ph $t2, $t3,  1", 0x2fff0000,  1, t2, t3);
   TESTDSPINST_RD_RT_SA_DSPC("shll_s.ph $t4, $t1,  2", 0x2fff0000,  2, t4, t1);
   TESTDSPINST_RD_RT_SA_DSPC("shll_s.ph $t6, $t7,  3", 0x2fff0000,  3, t6, t7);
   TESTDSPINST_RD_RT_SA_DSPC("shll_s.ph $t5, $t3,  4", 0x80000000,  4, t5, t3);
   TESTDSPINST_RD_RT_SA_DSPC("shll_s.ph $t2, $t4,  5", 0xff01ffff,  5, t2, t4);
   TESTDSPINST_RD_RT_SA_DSPC("shll_s.ph $t0, $t8,  6", 0x7fff7fff,  6, t0, t8);
   TESTDSPINST_RD_RT_SA_DSPC("shll_s.ph $t4, $t6,  7", 0x00000555,  7, t4, t6);
   TESTDSPINST_RD_RT_SA_DSPC("shll_s.ph $t0, $t1,  8", 0x00000000,  8, t0, t1);
   TESTDSPINST_RD_RT_SA_DSPC("shll_s.ph $t2, $t3,  9", 0x80000000,  9, t2, t3);
   TESTDSPINST_RD_RT_SA_DSPC("shll_s.ph $t4, $t1, 10", 0x55555555, 10, t4, t1);
   TESTDSPINST_RD_RT_SA_DSPC("shll_s.ph $t6, $t7, 11", 0xffff2435, 11, t6, t7);
   TESTDSPINST_RD_RT_SA_DSPC("shll_s.ph $t5, $t3, 12", 0xabababab, 12, t5, t3);
   TESTDSPINST_RD_RT_SA_DSPC("shll_s.ph $t2, $t4, 13", 0xfc79b4d2, 13, t2, t4);
   TESTDSPINST_RD_RT_SA_DSPC("shll_s.ph $t0, $t8, 14", 0x00000000, 14, t0, t8);
   TESTDSPINST_RD_RT_SA_DSPC("shll_s.ph $t4, $t6, 15", 0x00354565, 15, t4, t6);
   TESTDSPINST_RD_RT_SA_DSPC("shll_s.ph $t0, $t1,  0", 0x00086755,  0, t0, t1);
   TESTDSPINST_RD_RT_SA_DSPC("shll_s.ph $t2, $t3,  1", 0x8f8f8f8f,  1, t2, t3);
   TESTDSPINST_RD_RT_SA_DSPC("shll_s.ph $t4, $t1,  2", 0xeeeeeeee,  2, t4, t1);
   TESTDSPINST_RD_RT_SA_DSPC("shll_s.ph $t6, $t7,  3", 0x1bdbdbdb,  3, t6, t7);
   TESTDSPINST_RD_RT_SA_DSPC("shll_s.ph $t5, $t3,  4", 0xdecadeca,  4, t5, t3);
   TESTDSPINST_RD_RT_SA_DSPC("shll_s.ph $t2, $t4,  5", 0x93474bde,  5, t2, t4);
   TESTDSPINST_RD_RT_SA_DSPC("shll_s.ph $t0, $t8,  6", 0xfc0007ff,  6, t0, t8);
   TESTDSPINST_RD_RT_SA_DSPC("shll_s.ph $t4, $t6,  7", 0xffffffff,  7, t4, t6);
   TESTDSPINST_RD_RT_SA_DSPC("shll_s.ph $t0, $t1,  8", 0xcb4ab48f,  8, t0, t1);
   TESTDSPINST_RD_RT_SA_DSPC("shll_s.ph $t2, $t3,  9", 0xaf8f7e18,  9, t2, t3);
   TESTDSPINST_RD_RT_SA_DSPC("shll_s.ph $t4, $t1, 10", 0x87df4510, 10, t4, t1);
   TESTDSPINST_RD_RT_SA_DSPC("shll_s.ph $t6, $t7, 11", 0xabf4e8e1, 11, t6, t7);
   TESTDSPINST_RD_RT_SA_DSPC("shll_s.ph $t5, $t3, 12", 0xf4c0eeac, 12, t5, t3);
   TESTDSPINST_RD_RT_SA_DSPC("shll_s.ph $t2, $t4, 13", 0x006a54f2, 13, t2, t4);
   TESTDSPINST_RD_RT_SA_DSPC("shll_s.ph $t0, $t8, 14", 0x79f74493, 14, t0, t8);
   TESTDSPINST_RD_RT_SA_DSPC("shll_s.ph $t4, $t6, 15", 0x9c09e313, 15, t4, t6);

   printf("-------- SHLL.QB --------\n");
   TESTDSPINST_RD_RT_SA_DSPC("shll.qb $t0, $t1, 1", 0x00000000, 1, t0, t1);
   TESTDSPINST_RD_RT_SA_DSPC("shll.qb $t2, $t3, 2", 0x2fff0000, 2, t2, t3);
   TESTDSPINST_RD_RT_SA_DSPC("shll.qb $t4, $t1, 3", 0x2fff0000, 3, t4, t1);
   TESTDSPINST_RD_RT_SA_DSPC("shll.qb $t6, $t7, 4", 0x2fff0000, 4, t6, t7);
   TESTDSPINST_RD_RT_SA_DSPC("shll.qb $t5, $t3, 0", 0x80000000, 0, t5, t3);
   TESTDSPINST_RD_RT_SA_DSPC("shll.qb $t2, $t4, 7", 0xff01ffff, 7, t2, t4);
   TESTDSPINST_RD_RT_SA_DSPC("shll.qb $t0, $t8, 7", 0x7fff7fff, 7, t0, t8);
   TESTDSPINST_RD_RT_SA_DSPC("shll.qb $t4, $t6, 0", 0x00000555, 0, t4, t6);
   TESTDSPINST_RD_RT_SA_DSPC("shll.qb $t0, $t1, 1", 0x00000000, 1, t0, t1);
   TESTDSPINST_RD_RT_SA_DSPC("shll.qb $t2, $t3, 2", 0x80000000, 2, t2, t3);
   TESTDSPINST_RD_RT_SA_DSPC("shll.qb $t4, $t1, 3", 0x55555555, 3, t4, t1);
   TESTDSPINST_RD_RT_SA_DSPC("shll.qb $t6, $t7, 4", 0xffff2435, 4, t6, t7);
   TESTDSPINST_RD_RT_SA_DSPC("shll.qb $t5, $t3, 5", 0xabababab, 5, t5, t3);
   TESTDSPINST_RD_RT_SA_DSPC("shll.qb $t2, $t4, 6", 0xfc79b4d2, 6, t2, t4);
   TESTDSPINST_RD_RT_SA_DSPC("shll.qb $t0, $t8, 7", 0x00000000, 7, t0, t8);
   TESTDSPINST_RD_RT_SA_DSPC("shll.qb $t4, $t6, 0", 0x00354565, 0, t4, t6);
   TESTDSPINST_RD_RT_SA_DSPC("shll.qb $t0, $t1, 1", 0x00086755, 1, t0, t1);
   TESTDSPINST_RD_RT_SA_DSPC("shll.qb $t2, $t3, 2", 0x8f8f8f8f, 2, t2, t3);
   TESTDSPINST_RD_RT_SA_DSPC("shll.qb $t4, $t1, 3", 0xeeeeeeee, 3, t4, t1);
   TESTDSPINST_RD_RT_SA_DSPC("shll.qb $t6, $t7, 4", 0x1bdbdbdb, 4, t6, t7);
   TESTDSPINST_RD_RT_SA_DSPC("shll.qb $t5, $t3, 5", 0xdecadeca, 5, t5, t3);
   TESTDSPINST_RD_RT_SA_DSPC("shll.qb $t2, $t4, 6", 0x93474bde, 6, t2, t4);
   TESTDSPINST_RD_RT_SA_DSPC("shll.qb $t0, $t8, 7", 0xfc0007ff, 7, t0, t8);
   TESTDSPINST_RD_RT_SA_DSPC("shll.qb $t4, $t6, 0", 0xffffffff, 0, t4, t6);
   TESTDSPINST_RD_RT_SA_DSPC("shll.qb $t0, $t1, 3", 0xcb4ab48f, 3, t0, t1);
   TESTDSPINST_RD_RT_SA_DSPC("shll.qb $t2, $t3, 4", 0xaf8f7e18, 4, t2, t3);
   TESTDSPINST_RD_RT_SA_DSPC("shll.qb $t4, $t1, 0", 0x87df4510, 0, t4, t1);
   TESTDSPINST_RD_RT_SA_DSPC("shll.qb $t6, $t7, 7", 0xabf4e8e1, 7, t6, t7);
   TESTDSPINST_RD_RT_SA_DSPC("shll.qb $t5, $t3, 7", 0xf4c0eeac, 7, t5, t3);
   TESTDSPINST_RD_RT_SA_DSPC("shll.qb $t2, $t4, 5", 0x006a54f2, 5, t2, t4);
   TESTDSPINST_RD_RT_SA_DSPC("shll.qb $t0, $t8, 1", 0x79f74493, 1, t0, t8);
   TESTDSPINST_RD_RT_SA_DSPC("shll.qb $t4, $t6, 2", 0x9c09e313, 2, t4, t6);

   printf("-------- SHLL_S.W --------\n");
   TESTDSPINST_RD_RT_SA_DSPC("shll_s.w $t0, $t1,  0", 0x00000000,  0, t0, t1);
   TESTDSPINST_RD_RT_SA_DSPC("shll_s.w $t2, $t3,  1", 0x2fff0000,  1, t2, t3);
   TESTDSPINST_RD_RT_SA_DSPC("shll_s.w $t4, $t1,  2", 0x2fff0000,  2, t4, t1);
   TESTDSPINST_RD_RT_SA_DSPC("shll_s.w $t6, $t7,  3", 0x2fff0000,  3, t6, t7);
   TESTDSPINST_RD_RT_SA_DSPC("shll_s.w $t5, $t3,  4", 0x80000000,  4, t5, t3);
   TESTDSPINST_RD_RT_SA_DSPC("shll_s.w $t2, $t4,  5", 0xff01ffff,  5, t2, t4);
   TESTDSPINST_RD_RT_SA_DSPC("shll_s.w $t0, $t8,  6", 0x7fff7fff,  6, t0, t8);
   TESTDSPINST_RD_RT_SA_DSPC("shll_s.w $t4, $t6,  7", 0x00000555,  7, t4, t6);
   TESTDSPINST_RD_RT_SA_DSPC("shll_s.w $t0, $t1,  8", 0x00000000,  8, t0, t1);
   TESTDSPINST_RD_RT_SA_DSPC("shll_s.w $t2, $t3,  9", 0x80000000,  9, t2, t3);
   TESTDSPINST_RD_RT_SA_DSPC("shll_s.w $t4, $t1, 10", 0x55555555, 10, t4, t1);
   TESTDSPINST_RD_RT_SA_DSPC("shll_s.w $t6, $t7, 11", 0xffff2435, 11, t6, t7);
   TESTDSPINST_RD_RT_SA_DSPC("shll_s.w $t5, $t3, 12", 0xabababab, 12, t5, t3);
   TESTDSPINST_RD_RT_SA_DSPC("shll_s.w $t2, $t4, 13", 0xfc79b4d2, 13, t2, t4);
   TESTDSPINST_RD_RT_SA_DSPC("shll_s.w $t0, $t8, 14", 0x00000000, 14, t0, t8);
   TESTDSPINST_RD_RT_SA_DSPC("shll_s.w $t4, $t6, 15", 0x00354565, 15, t4, t6);
   TESTDSPINST_RD_RT_SA_DSPC("shll_s.w $t0, $t1, 16", 0x00086755,  0, t0, t1);
   TESTDSPINST_RD_RT_SA_DSPC("shll_s.w $t2, $t3, 17", 0x8f8f8f8f,  1, t2, t3);
   TESTDSPINST_RD_RT_SA_DSPC("shll_s.w $t4, $t1, 18", 0xeeeeeeee,  2, t4, t1);
   TESTDSPINST_RD_RT_SA_DSPC("shll_s.w $t6, $t7, 19", 0x1bdbdbdb,  3, t6, t7);
   TESTDSPINST_RD_RT_SA_DSPC("shll_s.w $t5, $t3, 20", 0xdecadeca,  4, t5, t3);
   TESTDSPINST_RD_RT_SA_DSPC("shll_s.w $t2, $t4, 21", 0x93474bde,  5, t2, t4);
   TESTDSPINST_RD_RT_SA_DSPC("shll_s.w $t0, $t8, 22", 0xfc0007ff,  6, t0, t8);
   TESTDSPINST_RD_RT_SA_DSPC("shll_s.w $t4, $t6, 23", 0xffffffff,  7, t4, t6);
   TESTDSPINST_RD_RT_SA_DSPC("shll_s.w $t0, $t1, 24", 0xcb4ab48f,  8, t0, t1);
   TESTDSPINST_RD_RT_SA_DSPC("shll_s.w $t2, $t3, 25", 0xaf8f7e18,  9, t2, t3);
   TESTDSPINST_RD_RT_SA_DSPC("shll_s.w $t4, $t1, 26", 0x87df4510, 10, t4, t1);
   TESTDSPINST_RD_RT_SA_DSPC("shll_s.w $t6, $t7, 27", 0xabf4e8e1, 11, t6, t7);
   TESTDSPINST_RD_RT_SA_DSPC("shll_s.w $t5, $t3, 28", 0xf4c0eeac, 12, t5, t3);
   TESTDSPINST_RD_RT_SA_DSPC("shll_s.w $t2, $t4, 29", 0x006a54f2, 13, t2, t4);
   TESTDSPINST_RD_RT_SA_DSPC("shll_s.w $t0, $t8, 30", 0x79f74493, 14, t0, t8);
   TESTDSPINST_RD_RT_SA_DSPC("shll_s.w $t4, $t6, 31", 0x9c09e313, 15, t4, t6);

   printf("-------- SHLLV.PH --------\n");
   TESTDSPINST_RD_RT_RS_DSPC("shllv.ph $t0, $t1, $t2", 0x7fffffff, 0x00000000,
                             t0, t1, t2);
   TESTDSPINST_RD_RT_RS_DSPC("shllv.ph $t2, $t3, $t4", 0x80000000, 0x00000000,
                             t2, t3, t4);
   TESTDSPINST_RD_RT_RS_DSPC("shllv.ph $t4, $t1, $t5", 0x2fff0000, 0xfabc3401,
                             t4, t1, t5);
   TESTDSPINST_RD_RT_RS_DSPC("shllv.ph $t6, $t7, $t3", 0x2fff0000, 0x73468002,
                             t6, t7, t3);
   TESTDSPINST_RD_RT_RS_DSPC("shllv.ph $t5, $t3, $t2", 0x2fff0000, 0x80000003,
                             t5, t3, t2);
   TESTDSPINST_RD_RT_RS_DSPC("shllv.ph $t2, $t4, $t8", 0x00ff0001, 0xff01ffff,
                             t2, t4, t8);
   TESTDSPINST_RD_RT_RS_DSPC("shllv.ph $t0, $t8, $t0", 0x7fff7004, 0x7fff7fff,
                             t0, t8, t0);
   TESTDSPINST_RD_RT_RS_DSPC("shllv.ph $t4, $t6, $t1", 0x0000c420, 0x00000555,
                             t4, t6, t1);
   TESTDSPINST_RD_RT_RS_DSPC("shllv.ph $t0, $t1, $t2", 0x00000000, 0x00000000,
                             t0, t1, t2);
   TESTDSPINST_RD_RT_RS_DSPC("shllv.ph $t2, $t3, $t4", 0x80000000, 0x80000000,
                             t2, t3, t4);
   TESTDSPINST_RD_RT_RS_DSPC("shllv.ph $t4, $t1, $t5", 0xaaaaaaaa, 0x55555555,
                             t4, t1, t5);
   TESTDSPINST_RD_RT_RS_DSPC("shllv.ph $t6, $t7, $t3", 0x00000018, 0xffff2435,
                             t6, t7, t3);
   TESTDSPINST_RD_RT_RS_DSPC("shllv.ph $t5, $t3, $t2", 0xbabababa, 0xabababab,
                             t5, t3, t2);
   TESTDSPINST_RD_RT_RS_DSPC("shllv.ph $t2, $t4, $t8", 0xf0f0f0f0, 0xfc79b4d2,
                             t2, t4, t8);
   TESTDSPINST_RD_RT_RS_DSPC("shllv.ph $t0, $t8, $t0", 0xfbde3976, 0x00000000,
                             t0, t8, t0);
   TESTDSPINST_RD_RT_RS_DSPC("shllv.ph $t4, $t6, $t1", 0x23534870, 0x00354565,
                             t4, t6, t1);
   TESTDSPINST_RD_RT_RS_DSPC("shllv.ph $t0, $t1, $t2", 0x980b7cde, 0x00086755,
                             t0, t1, t2);
   TESTDSPINST_RD_RT_RS_DSPC("shllv.ph $t2, $t3, $t4", 0x00000018, 0x8f8f8f8f,
                             t2, t3, t4);
   TESTDSPINST_RD_RT_RS_DSPC("shllv.ph $t4, $t1, $t5", 0x92784656, 0xeeeeeeee,
                             t4, t1, t5);
   TESTDSPINST_RD_RT_RS_DSPC("shllv.ph $t6, $t7, $t3", 0xcacacaca, 0x1bdbdbdb,
                             t6, t7, t3);
   TESTDSPINST_RD_RT_RS_DSPC("shllv.ph $t5, $t3, $t2", 0xbacabaca, 0xdecadeca,
                             t5, t3, t2);
   TESTDSPINST_RD_RT_RS_DSPC("shllv.ph $t2, $t4, $t8", 0x12fadeb4, 0x93474bde,
                             t2, t4, t8);
   TESTDSPINST_RD_RT_RS_DSPC("shllv.ph $t0, $t8, $t0", 0x7c000790, 0xfc0007ff,
                             t0, t8, t0);
   TESTDSPINST_RD_RT_RS_DSPC("shllv.ph $t4, $t6, $t1", 0xffffffff, 0xffffffff,
                             t4, t6, t1);
   TESTDSPINST_RD_RT_RS_DSPC("shllv.ph $t0, $t1, $t2", 0xf2f4df1f, 0xcb4ab48f,
                             t0, t1, t2);
   TESTDSPINST_RD_RT_RS_DSPC("shllv.ph $t2, $t3, $t4", 0x435f909a, 0xaf8f7e18,
                             t2, t3, t4);
   TESTDSPINST_RD_RT_RS_DSPC("shllv.ph $t4, $t1, $t5", 0x2106ba5f, 0x87df4510,
                             t4, t1, t5);
   TESTDSPINST_RD_RT_RS_DSPC("shllv.ph $t6, $t7, $t3", 0x246a6376, 0xabf4e8e1,
                             t6, t7, t3);
   TESTDSPINST_RD_RT_RS_DSPC("shllv.ph $t5, $t3, $t2", 0x1046a1a3, 0xf4c0eeac,
                             t5, t3, t2);
   TESTDSPINST_RD_RT_RS_DSPC("shllv.ph $t2, $t4, $t8", 0x638ca515, 0x006a54f2,
                             t2, t4, t8);
   TESTDSPINST_RD_RT_RS_DSPC("shllv.ph $t0, $t8, $t0", 0xf63e7a9d, 0x79f74493,
                             t0, t8, t0);
   TESTDSPINST_RD_RT_RS_DSPC("shllv.ph $t4, $t6, $t1", 0xbd6845cd, 0x9c09e313,
                             t4, t6, t1);

   printf("-------- SHLLV_S.PH --------\n");
   TESTDSPINST_RD_RT_RS_DSPC("shllv_s.ph $t0, $t1, $t2", 0x7fffffff, 0x00000000,
                             t0, t1, t2);
   TESTDSPINST_RD_RT_RS_DSPC("shllv_s.ph $t2, $t3, $t4", 0x80000000, 0x00000000,
                             t2, t3, t4);
   TESTDSPINST_RD_RT_RS_DSPC("shllv_s.ph $t4, $t1, $t5", 0x2fff0000, 0xfabc3401,
                             t4, t1, t5);
   TESTDSPINST_RD_RT_RS_DSPC("shllv_s.ph $t6, $t7, $t3", 0x2fff0000, 0x73468002,
                             t6, t7, t3);
   TESTDSPINST_RD_RT_RS_DSPC("shllv_s.ph $t5, $t3, $t2", 0x2fff0000, 0x80000003,
                             t5, t3, t2);
   TESTDSPINST_RD_RT_RS_DSPC("shllv_s.ph $t2, $t4, $t8", 0x00ff0001, 0xff01ffff,
                             t2, t4, t8);
   TESTDSPINST_RD_RT_RS_DSPC("shllv_s.ph $t0, $t8, $t0", 0x7fff7004, 0x7fff7fff,
                             t0, t8, t0);
   TESTDSPINST_RD_RT_RS_DSPC("shllv_s.ph $t4, $t6, $t1", 0x0000c420, 0x00000555,
                             t4, t6, t1);
   TESTDSPINST_RD_RT_RS_DSPC("shllv_s.ph $t0, $t1, $t2", 0x00000000, 0x00000000,
                             t0, t1, t2);
   TESTDSPINST_RD_RT_RS_DSPC("shllv_s.ph $t2, $t3, $t4", 0x80000000, 0x80000000,
                             t2, t3, t4);
   TESTDSPINST_RD_RT_RS_DSPC("shllv_s.ph $t4, $t1, $t5", 0xaaaaaaaa, 0x55555555,
                             t4, t1, t5);
   TESTDSPINST_RD_RT_RS_DSPC("shllv_s.ph $t6, $t7, $t3", 0x00000018, 0xffff2435,
                             t6, t7, t3);
   TESTDSPINST_RD_RT_RS_DSPC("shllv_s.ph $t5, $t3, $t2", 0xbabababa, 0xabababab,
                             t5, t3, t2);
   TESTDSPINST_RD_RT_RS_DSPC("shllv_s.ph $t2, $t4, $t8", 0xf0f0f0f0, 0xfc79b4d2,
                             t2, t4, t8);
   TESTDSPINST_RD_RT_RS_DSPC("shllv_s.ph $t0, $t8, $t0", 0xfbde3976, 0x00000000,
                             t0, t8, t0);
   TESTDSPINST_RD_RT_RS_DSPC("shllv_s.ph $t4, $t6, $t1", 0x23534870, 0x00354565,
                             t4, t6, t1);
   TESTDSPINST_RD_RT_RS_DSPC("shllv_s.ph $t0, $t1, $t2", 0x980b7cde, 0x00086755,
                             t0, t1, t2);
   TESTDSPINST_RD_RT_RS_DSPC("shllv_s.ph $t2, $t3, $t4", 0x00000018, 0x8f8f8f8f,
                             t2, t3, t4);
   TESTDSPINST_RD_RT_RS_DSPC("shllv_s.ph $t4, $t1, $t5", 0x92784656, 0xeeeeeeee,
                             t4, t1, t5);
   TESTDSPINST_RD_RT_RS_DSPC("shllv_s.ph $t6, $t7, $t3", 0xcacacaca, 0x1bdbdbdb,
                             t6, t7, t3);
   TESTDSPINST_RD_RT_RS_DSPC("shllv_s.ph $t5, $t3, $t2", 0xbacabaca, 0xdecadeca,
                             t5, t3, t2);
   TESTDSPINST_RD_RT_RS_DSPC("shllv_s.ph $t2, $t4, $t8", 0x12fadeb4, 0x93474bde,
                             t2, t4, t8);
   TESTDSPINST_RD_RT_RS_DSPC("shllv_s.ph $t0, $t8, $t0", 0x7c000790, 0xfc0007ff,
                             t0, t8, t0);
   TESTDSPINST_RD_RT_RS_DSPC("shllv_s.ph $t4, $t6, $t1", 0xffffffff, 0xffffffff,
                             t4, t6, t1);
   TESTDSPINST_RD_RT_RS_DSPC("shllv_s.ph $t0, $t1, $t2", 0xf2f4df1f, 0xcb4ab48f,
                             t0, t1, t2);
   TESTDSPINST_RD_RT_RS_DSPC("shllv_s.ph $t2, $t3, $t4", 0x435f909a, 0xaf8f7e18,
                             t2, t3, t4);
   TESTDSPINST_RD_RT_RS_DSPC("shllv_s.ph $t4, $t1, $t5", 0x2106ba5f, 0x87df4510,
                             t4, t1, t5);
   TESTDSPINST_RD_RT_RS_DSPC("shllv_s.ph $t6, $t7, $t3", 0x246a6376, 0xabf4e8e1,
                             t6, t7, t3);
   TESTDSPINST_RD_RT_RS_DSPC("shllv_s.ph $t5, $t3, $t2", 0x1046a1a3, 0xf4c0eeac,
                             t5, t3, t2);
   TESTDSPINST_RD_RT_RS_DSPC("shllv_s.ph $t2, $t4, $t8", 0x638ca515, 0x006a54f2,
                             t2, t4, t8);
   TESTDSPINST_RD_RT_RS_DSPC("shllv_s.ph $t0, $t8, $t0", 0xf63e7a9d, 0x79f74493,
                             t0, t8, t0);
   TESTDSPINST_RD_RT_RS_DSPC("shllv_s.ph $t4, $t6, $t1", 0xbd6845cd, 0x9c09e313,
                             t4, t6, t1);

   printf("-------- SHLLV.QB --------\n");
   TESTDSPINST_RD_RT_RS_DSPC("shllv.qb $t0, $t1, $t2", 0x7fffffff, 0x00000000,
                             t0, t1, t2);
   TESTDSPINST_RD_RT_RS_DSPC("shllv.qb $t2, $t3, $t4", 0x80000000, 0x00000000,
                             t2, t3, t4);
   TESTDSPINST_RD_RT_RS_DSPC("shllv.qb $t4, $t1, $t5", 0x2fff0000, 0xfabc3401,
                             t4, t1, t5);
   TESTDSPINST_RD_RT_RS_DSPC("shllv.qb $t6, $t7, $t3", 0x2fff0000, 0x73468002,
                             t6, t7, t3);
   TESTDSPINST_RD_RT_RS_DSPC("shllv.qb $t5, $t3, $t2", 0x2fff0000, 0x80000003,
                             t5, t3, t2);
   TESTDSPINST_RD_RT_RS_DSPC("shllv.qb $t2, $t4, $t8", 0x00ff0001, 0xff01ffff,
                             t2, t4, t8);
   TESTDSPINST_RD_RT_RS_DSPC("shllv.qb $t0, $t8, $t0", 0x7fff7004, 0x7fff7fff,
                             t0, t8, t0);
   TESTDSPINST_RD_RT_RS_DSPC("shllv.qb $t4, $t6, $t1", 0x0000c420, 0x00000555,
                             t4, t6, t1);
   TESTDSPINST_RD_RT_RS_DSPC("shllv.qb $t0, $t1, $t2", 0x00000000, 0x9348572b,
                             t0, t1, t2);
   TESTDSPINST_RD_RT_RS_DSPC("shllv.qb $t2, $t3, $t4", 0x80000000, 0x80023450,
                             t2, t3, t4);
   TESTDSPINST_RD_RT_RS_DSPC("shllv.qb $t4, $t1, $t5", 0xaaaaaaaa, 0x55555555,
                             t4, t1, t5);
   TESTDSPINST_RD_RT_RS_DSPC("shllv.qb $t6, $t7, $t3", 0x00000018, 0xffff2435,
                             t6, t7, t3);
   TESTDSPINST_RD_RT_RS_DSPC("shllv.qb $t5, $t3, $t2", 0xbabababa, 0xabababab,
                             t5, t3, t2);
   TESTDSPINST_RD_RT_RS_DSPC("shllv.qb $t2, $t4, $t8", 0xf0f0f0f0, 0xfc79b4d2,
                             t2, t4, t8);
   TESTDSPINST_RD_RT_RS_DSPC("shllv.qb $t0, $t8, $t0", 0xfbde3976, 0x00000000,
                             t0, t8, t0);
   TESTDSPINST_RD_RT_RS_DSPC("shllv.qb $t4, $t6, $t1", 0x23534870, 0x00354565,
                             t4, t6, t1);
   TESTDSPINST_RD_RT_RS_DSPC("shllv.qb $t0, $t1, $t2", 0x980b7cde, 0x00086755,
                             t0, t1, t2);
   TESTDSPINST_RD_RT_RS_DSPC("shllv.qb $t2, $t3, $t4", 0x00000018, 0x8f8f8f8f,
                             t2, t3, t4);
   TESTDSPINST_RD_RT_RS_DSPC("shllv.qb $t4, $t1, $t5", 0x92784656, 0xeeeeeeee,
                             t4, t1, t5);
   TESTDSPINST_RD_RT_RS_DSPC("shllv.qb $t6, $t7, $t3", 0xcacacaca, 0x1bdbdbdb,
                             t6, t7, t3);
   TESTDSPINST_RD_RT_RS_DSPC("shllv.qb $t5, $t3, $t2", 0xbacabaca, 0xdecadeca,
                             t5, t3, t2);
   TESTDSPINST_RD_RT_RS_DSPC("shllv.qb $t2, $t4, $t8", 0x12fadeb4, 0x93474bde,
                             t2, t4, t8);
   TESTDSPINST_RD_RT_RS_DSPC("shllv.qb $t0, $t8, $t0", 0x7c000790, 0xfc0007ff,
                             t0, t8, t0);
   TESTDSPINST_RD_RT_RS_DSPC("shllv.qb $t4, $t6, $t1", 0xffffffff, 0xffffffff,
                             t4, t6, t1);
   TESTDSPINST_RD_RT_RS_DSPC("shllv.qb $t0, $t1, $t2", 0xf2f4df1f, 0xcb4ab48f,
                             t0, t1, t2);
   TESTDSPINST_RD_RT_RS_DSPC("shllv.qb $t2, $t3, $t4", 0x435f909a, 0xaf8f7e18,
                             t2, t3, t4);
   TESTDSPINST_RD_RT_RS_DSPC("shllv.qb $t4, $t1, $t5", 0x2106ba5f, 0x87df4510,
                             t4, t1, t5);
   TESTDSPINST_RD_RT_RS_DSPC("shllv.qb $t6, $t7, $t3", 0x246a6376, 0xabf4e8e1,
                             t6, t7, t3);
   TESTDSPINST_RD_RT_RS_DSPC("shllv.qb $t5, $t3, $t2", 0x1046a1a3, 0xf4c0eeac,
                             t5, t3, t2);
   TESTDSPINST_RD_RT_RS_DSPC("shllv.qb $t2, $t4, $t8", 0x638ca515, 0x006a54f2,
                             t2, t4, t8);
   TESTDSPINST_RD_RT_RS_DSPC("shllv.qb $t0, $t8, $t0", 0xf63e7a9d, 0x79f74493,
                             t0, t8, t0);
   TESTDSPINST_RD_RT_RS_DSPC("shllv.qb $t4, $t6, $t1", 0xbd6845cd, 0x9c09e313,
                             t4, t6, t1);

   printf("-------- SHLLV_S.W --------\n");
   TESTDSPINST_RD_RT_RS_DSPC("shllv_s.w $t0, $t1, $t2", 0x7fffffff, 0x00000000,
                             t0, t1, t2);
   TESTDSPINST_RD_RT_RS_DSPC("shllv_s.w $t2, $t3, $t4", 0x80000000, 0x00000000,
                             t2, t3, t4);
   TESTDSPINST_RD_RT_RS_DSPC("shllv_s.w $t4, $t1, $t5", 0x2fff0000, 0xfabc3401,
                             t4, t1, t5);
   TESTDSPINST_RD_RT_RS_DSPC("shllv_s.w $t6, $t7, $t3", 0x2fff0000, 0x73468002,
                             t6, t7, t3);
   TESTDSPINST_RD_RT_RS_DSPC("shllv_s.w $t5, $t3, $t2", 0x2fff0000, 0x80000003,
                             t5, t3, t2);
   TESTDSPINST_RD_RT_RS_DSPC("shllv_s.w $t2, $t4, $t8", 0x00ff0001, 0xff01ffff,
                             t2, t4, t8);
   TESTDSPINST_RD_RT_RS_DSPC("shllv_s.w $t0, $t8, $t0", 0x7fff7004, 0x7fff7fff,
                             t0, t8, t0);
   TESTDSPINST_RD_RT_RS_DSPC("shllv_s.w $t4, $t6, $t1", 0x0000c420, 0x00000555,
                             t4, t6, t1);
   TESTDSPINST_RD_RT_RS_DSPC("shllv_s.w $t0, $t1, $t2", 0x00000000, 0x00000000,
                             t0, t1, t2);
   TESTDSPINST_RD_RT_RS_DSPC("shllv_s.w $t2, $t3, $t4", 0x80000000, 0x80000000,
                             t2, t3, t4);
   TESTDSPINST_RD_RT_RS_DSPC("shllv_s.w $t4, $t1, $t5", 0xaaaaaaaa, 0x55555555,
                             t4, t1, t5);
   TESTDSPINST_RD_RT_RS_DSPC("shllv_s.w $t6, $t7, $t3", 0x00000018, 0xffff2435,
                             t6, t7, t3);
   TESTDSPINST_RD_RT_RS_DSPC("shllv_s.w $t5, $t3, $t2", 0xbabababa, 0xabababab,
                             t5, t3, t2);
   TESTDSPINST_RD_RT_RS_DSPC("shllv_s.w $t2, $t4, $t8", 0xf0f0f0f0, 0xfc79b4d2,
                             t2, t4, t8);
   TESTDSPINST_RD_RT_RS_DSPC("shllv_s.w $t0, $t8, $t0", 0xfbde3976, 0x00000000,
                             t0, t8, t0);
   TESTDSPINST_RD_RT_RS_DSPC("shllv_s.w $t4, $t6, $t1", 0x23534870, 0x00354565,
                             t4, t6, t1);
   TESTDSPINST_RD_RT_RS_DSPC("shllv_s.w $t0, $t1, $t2", 0x980b7cde, 0x00086755,
                             t0, t1, t2);
   TESTDSPINST_RD_RT_RS_DSPC("shllv_s.w $t2, $t3, $t4", 0x00000018, 0x8f8f8f8f,
                             t2, t3, t4);
   TESTDSPINST_RD_RT_RS_DSPC("shllv_s.w $t4, $t1, $t5", 0x92784656, 0xeeeeeeee,
                             t4, t1, t5);
   TESTDSPINST_RD_RT_RS_DSPC("shllv_s.w $t6, $t7, $t3", 0xcacacaca, 0x1bdbdbdb,
                             t6, t7, t3);
   TESTDSPINST_RD_RT_RS_DSPC("shllv_s.w $t5, $t3, $t2", 0xbacabaca, 0xdecadeca,
                             t5, t3, t2);
   TESTDSPINST_RD_RT_RS_DSPC("shllv_s.w $t2, $t4, $t8", 0x12fadeb4, 0x93474bde,
                             t2, t4, t8);
   TESTDSPINST_RD_RT_RS_DSPC("shllv_s.w $t0, $t8, $t0", 0x7c000790, 0xfc0007ff,
                             t0, t8, t0);
   TESTDSPINST_RD_RT_RS_DSPC("shllv_s.w $t4, $t6, $t1", 0xffffffff, 0xffffffff,
                             t4, t6, t1);
   TESTDSPINST_RD_RT_RS_DSPC("shllv_s.w $t0, $t1, $t2", 0xf2f4df1f, 0xcb4ab48f,
                             t0, t1, t2);
   TESTDSPINST_RD_RT_RS_DSPC("shllv_s.w $t2, $t3, $t4", 0x435f909a, 0xaf8f7e18,
                             t2, t3, t4);
   TESTDSPINST_RD_RT_RS_DSPC("shllv_s.w $t4, $t1, $t5", 0x2106ba5f, 0x87df4510,
                             t4, t1, t5);
   TESTDSPINST_RD_RT_RS_DSPC("shllv_s.w $t6, $t7, $t3", 0x246a6376, 0xabf4e8e1,
                             t6, t7, t3);
   TESTDSPINST_RD_RT_RS_DSPC("shllv_s.w $t5, $t3, $t2", 0x1046a1a3, 0xf4c0eeac,
                             t5, t3, t2);
   TESTDSPINST_RD_RT_RS_DSPC("shllv_s.w $t2, $t4, $t8", 0x638ca515, 0x006a54f2,
                             t2, t4, t8);
   TESTDSPINST_RD_RT_RS_DSPC("shllv_s.w $t0, $t8, $t0", 0xf63e7a9d, 0x79f74493,
                             t0, t8, t0);
   TESTDSPINST_RD_RT_RS_DSPC("shllv_s.w $t4, $t6, $t1", 0xbd6845cd, 0x9c09e313,
                             t4, t6, t1);

   printf("-------- SHRA.PH --------\n");
   TESTDSPINST_RD_RT_SA_NODSPC("shra.ph $t0, $t1,  0", 0x00000000,  0, t0, t1);
   TESTDSPINST_RD_RT_SA_NODSPC("shra.ph $t2, $t3,  1", 0x00028632,  1, t2, t3);
   TESTDSPINST_RD_RT_SA_NODSPC("shra.ph $t4, $t1,  2", 0xfabc3421,  2, t4, t1);
   TESTDSPINST_RD_RT_SA_NODSPC("shra.ph $t6, $t7,  3", 0x734680bc,  3, t6, t7);
   TESTDSPINST_RD_RT_SA_NODSPC("shra.ph $t5, $t3,  4", 0x80000000,  4, t5, t3);
   TESTDSPINST_RD_RT_SA_NODSPC("shra.ph $t2, $t4,  5", 0xff01ffff,  5, t2, t4);
   TESTDSPINST_RD_RT_SA_NODSPC("shra.ph $t0, $t8,  6", 0x7fff7fff,  6, t0, t8);
   TESTDSPINST_RD_RT_SA_NODSPC("shra.ph $t4, $t6,  7", 0x00000555,  7, t4, t6);
   TESTDSPINST_RD_RT_SA_NODSPC("shra.ph $t0, $t1,  8", 0x00000000,  8, t0, t1);
   TESTDSPINST_RD_RT_SA_NODSPC("shra.ph $t2, $t3,  9", 0x80000000,  9, t2, t3);
   TESTDSPINST_RD_RT_SA_NODSPC("shra.ph $t4, $t1, 10", 0x55555555, 10, t4, t1);
   TESTDSPINST_RD_RT_SA_NODSPC("shra.ph $t6, $t7, 11", 0xffff2435, 11, t6, t7);
   TESTDSPINST_RD_RT_SA_NODSPC("shra.ph $t5, $t3, 12", 0xabababab, 12, t5, t3);
   TESTDSPINST_RD_RT_SA_NODSPC("shra.ph $t2, $t4, 13", 0xfc79b4d2, 13, t2, t4);
   TESTDSPINST_RD_RT_SA_NODSPC("shra.ph $t0, $t8, 14", 0x00000000, 14, t0, t8);
   TESTDSPINST_RD_RT_SA_NODSPC("shra.ph $t4, $t6, 15", 0x00354565, 15, t4, t6);
   TESTDSPINST_RD_RT_SA_NODSPC("shra.ph $t0, $t1,  0", 0x00086755,  0, t0, t1);
   TESTDSPINST_RD_RT_SA_NODSPC("shra.ph $t2, $t3,  1", 0x8f8f8f8f,  1, t2, t3);
   TESTDSPINST_RD_RT_SA_NODSPC("shra.ph $t4, $t1,  2", 0xeeeeeeee,  2, t4, t1);
   TESTDSPINST_RD_RT_SA_NODSPC("shra.ph $t6, $t7,  3", 0x1bdbdbdb,  3, t6, t7);
   TESTDSPINST_RD_RT_SA_NODSPC("shra.ph $t5, $t3,  4", 0xdecadeca,  4, t5, t3);
   TESTDSPINST_RD_RT_SA_NODSPC("shra.ph $t2, $t4,  5", 0x93474bde,  5, t2, t4);
   TESTDSPINST_RD_RT_SA_NODSPC("shra.ph $t0, $t8,  6", 0xfc0007ff,  6, t0, t8);
   TESTDSPINST_RD_RT_SA_NODSPC("shra.ph $t4, $t6,  7", 0xffffffff,  7, t4, t6);
   TESTDSPINST_RD_RT_SA_NODSPC("shra.ph $t0, $t1,  8", 0xcb4ab48f,  8, t0, t1);
   TESTDSPINST_RD_RT_SA_NODSPC("shra.ph $t2, $t3,  9", 0xaf8f7e18,  9, t2, t3);
   TESTDSPINST_RD_RT_SA_NODSPC("shra.ph $t4, $t1, 10", 0x87df4510, 10, t4, t1);
   TESTDSPINST_RD_RT_SA_NODSPC("shra.ph $t6, $t7, 11", 0xabf4e8e1, 11, t6, t7);
   TESTDSPINST_RD_RT_SA_NODSPC("shra.ph $t5, $t3, 12", 0xf4c0eeac, 12, t5, t3);
   TESTDSPINST_RD_RT_SA_NODSPC("shra.ph $t2, $t4, 13", 0x006a54f2, 13, t2, t4);
   TESTDSPINST_RD_RT_SA_NODSPC("shra.ph $t0, $t8, 14", 0x79f74493, 14, t0, t8);
   TESTDSPINST_RD_RT_SA_NODSPC("shra.ph $t4, $t6, 15", 0x9c09e313, 15, t4, t6);

   printf("-------- SHRA_R.PH --------\n");
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.ph $t0, $t1,  0", 0x00000000,  0, t0,
                               t1);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.ph $t2, $t3,  1", 0x00028632,  1, t2,
                               t3);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.ph $t4, $t1,  2", 0xfabc3421,  2, t4,
                               t1);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.ph $t6, $t7,  3", 0x734680bc,  3, t6,
                               t7);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.ph $t5, $t3,  4", 0x80000000,  4, t5,
                               t3);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.ph $t2, $t4,  5", 0xff01ffff,  5, t2,
                               t4);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.ph $t0, $t8,  6", 0x7fff7fff,  6, t0,
                               t8);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.ph $t4, $t6,  7", 0x00000555,  7, t4,
                               t6);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.ph $t0, $t1,  8", 0x00000000,  8, t0,
                               t1);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.ph $t2, $t3,  9", 0x80000000,  9, t2,
                               t3);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.ph $t4, $t1, 10", 0x55555555, 10, t4,
                               t1);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.ph $t6, $t7, 11", 0xffff2435, 11, t6,
                               t7);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.ph $t5, $t3, 12", 0xabababab, 12, t5,
                               t3);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.ph $t2, $t4, 13", 0xfc79b4d2, 13, t2,
                               t4);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.ph $t0, $t8, 14", 0x00000000, 14, t0,
                               t8);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.ph $t4, $t6, 15", 0x00354565, 15, t4,
                               t6);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.ph $t0, $t1,  0", 0x00086755,  0, t0,
                               t1);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.ph $t2, $t3,  1", 0x8f8f8f8f,  1, t2,
                               t3);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.ph $t4, $t1,  2", 0xeeeeeeee,  2, t4,
                               t1);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.ph $t6, $t7,  3", 0x1bdbdbdb,  3, t6,
                               t7);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.ph $t5, $t3,  4", 0xdecadeca,  4, t5,
                               t3);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.ph $t2, $t4,  5", 0x93474bde,  5, t2,
                               t4);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.ph $t0, $t8,  6", 0xfc0007ff,  6, t0,
                               t8);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.ph $t4, $t6,  7", 0xffffffff,  7, t4,
                               t6);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.ph $t0, $t1,  8", 0xcb4ab48f,  8, t0,
                               t1);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.ph $t2, $t3,  9", 0xaf8f7e18,  9, t2,
                               t3);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.ph $t4, $t1, 10", 0x87df4510, 10, t4,
                               t1);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.ph $t6, $t7, 11", 0xabf4e8e1, 11, t6,
                               t7);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.ph $t5, $t3, 12", 0xf4c0eeac, 12, t5,
                               t3);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.ph $t2, $t4, 13", 0x006a54f2, 13, t2,
                               t4);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.ph $t0, $t8, 14", 0x79f74493, 14, t0,
                               t8);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.ph $t4, $t6, 15", 0x9c09e313, 15, t4,
                               t6);

   printf("-------- SHRA_R.W --------\n");
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.w $t2, $t3,  1", 0x2,  1, t2, t3);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.w $t2, $t3, 16", 0x80000000, 16, t2, t3);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.w $t2, $t3,  1", 0x10001,  1, t2, t3);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.w $t2, $t3, 17", 0x10001, 17, t2, t3);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.w $t2, $t3, 17", 0x80010001, 17, t2, t3);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.w $t2, $t3,  0", 0x7fffffff,  0, t2, t3);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.w $t2, $t3,  1", 0x7fffffff,  1, t2, t3);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.w $t2, $t3,  2", 0x7ffffffe,  2, t2, t3);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.w $t0, $t1,  0", 0x00000000,  0, t0, t1);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.w $t4, $t1,  4", 0xfabc3435,  4, t4, t1);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.w $t6, $t7, 17", 0x07654cb8, 17, t6, t7);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.w $t5, $t3, 31", 0xf973437b, 31, t5, t3);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.w $t2, $t4,  8", 0x00ff0001,  8, t2, t4);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.w $t0, $t8, 11", 0x7fff7fff, 11, t0, t8);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.w $t4, $t6, 13", 0x0000c420, 13, t4, t6);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.w $t0, $t1,  2", 0x00000000,  2, t0, t1);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.w $t2, $t3,  6", 0x80000000,  6, t2, t3);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.w $t4, $t1,  7", 0xaaaaaaaa,  7, t4, t1);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.w $t6, $t7, 19", 0x00000018, 19, t6, t7);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.w $t5, $t3, 31", 0xbabababa, 31, t5, t3);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.w $t2, $t4,  4", 0xf0f0f0f0,  4, t2, t4);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.w $t0, $t8, 12", 0xfbde3976, 12, t0, t8);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.w $t4, $t6, 10", 0x23534870, 10, t4, t6);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.w $t0, $t1, 20", 0x980b7cde, 20, t0, t1);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.w $t2, $t3, 21", 0x00000018, 21, t2, t3);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.w $t4, $t1, 24", 0x92784656, 24, t4, t1);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.w $t6, $t7, 27", 0xcacacaca, 27, t6, t7);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.w $t5, $t3,  1", 0xbacabaca,  1, t5, t3);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.w $t2, $t4, 18", 0x12fadeb4, 18, t2, t4);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.w $t0, $t8, 10", 0x7c000790, 10, t0, t8);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.w $t4, $t6, 16", 0xffffffff, 16, t4, t6);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.w $t0, $t1,  0", 0xf2f4df1f,  0, t0, t1);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.w $t2, $t3, 14", 0x435f909a, 14, t2, t3);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.w $t4, $t1,  5", 0x2106ba5f,  5, t4, t1);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.w $t6, $t7,  7", 0x246a6376,  7, t6, t7);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.w $t5, $t3,  9", 0x1046a1a3,  9, t5, t3);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.w $t2, $t4,  3", 0x638ca515,  3, t2, t4);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.w $t0, $t8, 15", 0xf63e7a9d, 15, t0, t8);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.w $t4, $t6, 11", 0xbd6845cd, 11, t4, t6);

   printf("-------- SHRAV.PH --------\n");
   TESTDSPINST_RD_RS_RT_NODSPC("shrav.ph $t0, $t1, $t2", 0x7fffffff, 0x00000000,
                               t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav.ph $t2, $t3, $t4", 0x80000000, 0x00000000,
                               t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav.ph $t4, $t1, $t5", 0xfabc3435, 0xfabc3421,
                               t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav.ph $t6, $t7, $t3", 0x07654cb8, 0x734680bc,
                               t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav.ph $t5, $t3, $t2", 0xf973437b, 0x80000000,
                               t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav.ph $t2, $t4, $t8", 0x00ff0001, 0xff01ffff,
                               t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav.ph $t0, $t8, $t0", 0x7fff7004, 0x7fff7fff,
                               t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav.ph $t4, $t6, $t1", 0x0000c420, 0x00000555,
                               t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav.ph $t0, $t1, $t2", 0x00000000, 0x00000000,
                               t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav.ph $t2, $t3, $t4", 0x80000000, 0x80000000,
                               t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav.ph $t4, $t1, $t5", 0xaaaaaaaa, 0x55555555,
                               t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav.ph $t6, $t7, $t3", 0x00000018, 0xffff2435,
                               t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav.ph $t5, $t3, $t2", 0xbabababa, 0xabababab,
                               t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav.ph $t2, $t4, $t8", 0xf0f0f0f0, 0xfc79b4d2,
                               t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav.ph $t0, $t8, $t0", 0xfbde3976, 0x00000000,
                               t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav.ph $t4, $t6, $t1", 0x23534870, 0x00354565,
                               t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav.ph $t0, $t1, $t2", 0x980b7cde, 0x00086755,
                               t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav.ph $t2, $t3, $t4", 0x00000018, 0x8f8f8f8f,
                               t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav.ph $t4, $t1, $t5", 0x92784656, 0xeeeeeeee,
                               t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav.ph $t6, $t7, $t3", 0xcacacaca, 0x1bdbdbdb,
                               t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav.ph $t5, $t3, $t2", 0xbacabaca, 0xdecadeca,
                               t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav.ph $t2, $t4, $t8", 0x12fadeb4, 0x93474bde,
                               t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav.ph $t0, $t8, $t0", 0x7c000790, 0xfc0007ff,
                               t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav.ph $t4, $t6, $t1", 0xffffffff, 0xffffffff,
                               t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav.ph $t0, $t1, $t2", 0xf2f4df1f, 0xcb4ab48f,
                               t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav.ph $t2, $t3, $t4", 0x435f909a, 0xaf8f7e18,
                               t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav.ph $t4, $t1, $t5", 0x2106ba5f, 0x87df4510,
                               t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav.ph $t6, $t7, $t3", 0x246a6376, 0xabf4e8e1,
                               t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav.ph $t5, $t3, $t2", 0x1046a1a3, 0xf4c0eeac,
                               t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav.ph $t2, $t4, $t8", 0x638ca515, 0x006a54f2,
                               t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav.ph $t0, $t8, $t0", 0xf63e7a9d, 0x79f74493,
                               t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav.ph $t4, $t6, $t1", 0xbd6845cd, 0x9c09e313,
                               t4, t6, t1);

   printf("-------- SHRAV_R.PH --------\n");
   TESTDSPINST_RD_RS_RT_NODSPC("shrav_r.ph $t0, $t1, $t2", 0x7fffffff,
                               0x00000000, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav_r.ph $t2, $t3, $t4", 0x80000000,
                               0x00000000, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav_r.ph $t4, $t1, $t5", 0xfabc3435,
                               0xfabc3421, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav_r.ph $t6, $t7, $t3", 0x07654cb8,
                               0x734680bc, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav_r.ph $t5, $t3, $t2", 0xf973437b,
                               0x80000000, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav_r.ph $t2, $t4, $t8", 0x00ff0001,
                               0xff01ffff, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav_r.ph $t0, $t8, $t0", 0x7fff7004,
                               0x7fff7fff, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav_r.ph $t4, $t6, $t1", 0x0000c420,
                               0x00000555, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav_r.ph $t0, $t1, $t2", 0x00000000,
                               0x00000000, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav_r.ph $t2, $t3, $t4", 0x80000000,
                               0x80000000, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav_r.ph $t4, $t1, $t5", 0xaaaaaaaa,
                               0x55555555, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav_r.ph $t6, $t7, $t3", 0x00000018,
                               0xffff2435, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav_r.ph $t5, $t3, $t2", 0xbabababa,
                               0xabababab, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav_r.ph $t2, $t4, $t8", 0xf0f0f0f0,
                               0xfc79b4d2, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav_r.ph $t0, $t8, $t0", 0xfbde3976,
                               0x00000000, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav_r.ph $t4, $t6, $t1", 0x23534870,
                               0x00354565, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav_r.ph $t0, $t1, $t2", 0x980b7cde,
                               0x00086755, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav_r.ph $t2, $t3, $t4", 0x00000018,
                               0x8f8f8f8f, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav_r.ph $t4, $t1, $t5", 0x92784656,
                               0xeeeeeeee, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav_r.ph $t6, $t7, $t3", 0xcacacaca,
                               0x1bdbdbdb, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav_r.ph $t5, $t3, $t2", 0xbacabaca,
                               0xdecadeca, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav_r.ph $t2, $t4, $t8", 0x12fadeb4,
                               0x93474bde, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav_r.ph $t0, $t8, $t0", 0x7c000790,
                               0xfc0007ff, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav_r.ph $t4, $t6, $t1", 0xffffffff,
                               0xffffffff, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav_r.ph $t0, $t1, $t2", 0xf2f4df1f,
                               0xcb4ab48f, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav_r.ph $t2, $t3, $t4", 0x435f909a,
                               0xaf8f7e18, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav_r.ph $t4, $t1, $t5", 0x2106ba5f,
                               0x87df4510, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav_r.ph $t6, $t7, $t3", 0x246a6376,
                               0xabf4e8e1, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav_r.ph $t5, $t3, $t2", 0x1046a1a3,
                               0xf4c0eeac, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav_r.ph $t2, $t4, $t8", 0x638ca515,
                               0x006a54f2, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav_r.ph $t0, $t8, $t0", 0xf63e7a9d,
                               0x79f74493, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav_r.ph $t4, $t6, $t1", 0xbd6845cd,
                               0x9c09e313, t4, t6, t1);

   printf("-------- SHRAV_R.W --------\n");
   TESTDSPINST_RD_RS_RT_NODSPC("shrav_r.w $t0, $t1, $t2", 0x7fffffff,
                               0x00000000, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav_r.w $t2, $t3, $t4", 0x80000000,
                               0x00000000, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav_r.w $t4, $t1, $t5", 0xfabc3435,
                               0xfabc3421, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav_r.w $t6, $t7, $t3", 0x07654cb8,
                               0x734680bc, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav_r.w $t5, $t3, $t2", 0xf973437b,
                               0x80000000, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav_r.w $t2, $t4, $t8", 0x00ff0001,
                               0xff01ffff, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav_r.w $t0, $t8, $t0", 0x7fff7004,
                               0x7fff7fff, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav_r.w $t4, $t6, $t1", 0x0000c420,
                               0x00000555, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav_r.w $t0, $t1, $t2", 0x00000000,
                               0x00000000, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav_r.w $t2, $t3, $t4", 0x80000000,
                               0x80000000, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav_r.w $t4, $t1, $t5", 0xaaaaaaaa,
                               0x55555555, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav_r.w $t6, $t7, $t3", 0x00000018,
                               0xffff2435, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav_r.w $t5, $t3, $t2", 0xbabababa,
                               0xabababab, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav_r.w $t2, $t4, $t8", 0xf0f0f0f0,
                               0xfc79b4d2, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav_r.w $t0, $t8, $t0", 0xfbde3976,
                               0x00000000, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav_r.w $t4, $t6, $t1", 0x23534870,
                               0x00354565, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav_r.w $t0, $t1, $t2", 0x980b7cde,
                               0x00086755, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav_r.w $t2, $t3, $t4", 0x00000018,
                               0x8f8f8f8f, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav_r.w $t4, $t1, $t5", 0x92784656,
                               0xeeeeeeee, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav_r.w $t6, $t7, $t3", 0xcacacaca,
                               0x1bdbdbdb, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav_r.w $t5, $t3, $t2", 0xbacabaca,
                               0xdecadeca, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav_r.w $t2, $t4, $t8", 0x12fadeb4,
                               0x93474bde, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav_r.w $t0, $t8, $t0", 0x7c000790,
                               0xfc0007ff, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav_r.w $t4, $t6, $t1", 0xffffffff,
                               0xffffffff, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav_r.w $t0, $t1, $t2", 0xf2f4df1f,
                               0xcb4ab48f, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav_r.w $t2, $t3, $t4", 0x435f909a,
                               0xaf8f7e18, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav_r.w $t4, $t1, $t5", 0x2106ba5f,
                               0x87df4510, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav_r.w $t6, $t7, $t3", 0x246a6376,
                               0xabf4e8e1, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav_r.w $t5, $t3, $t2", 0x1046a1a3,
                               0xf4c0eeac, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav_r.w $t2, $t4, $t8", 0x638ca515,
                               0x006a54f2, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav_r.w $t0, $t8, $t0", 0xf63e7a9d,
                               0x79f74493, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("shrav_r.w $t4, $t6, $t1", 0xbd6845cd,
                               0x9c09e313, t4, t6, t1);

   printf("-------- SHRL.QB --------\n");
   TESTDSPINST_RD_RT_SA_NODSPC("shrl.qb $t0, $t1, 1", 0x2fff0000, 1, t0, t1);
   TESTDSPINST_RD_RT_SA_NODSPC("shrl.qb $t2, $t3, 2", 0x2fff0000, 2, t2, t3);
   TESTDSPINST_RD_RT_SA_NODSPC("shrl.qb $t4, $t1, 3", 0x2fff0000, 3, t4, t1);
   TESTDSPINST_RD_RT_SA_NODSPC("shrl.qb $t6, $t7, 4", 0xff460000, 4, t6, t7);
   TESTDSPINST_RD_RT_SA_NODSPC("shrl.qb $t5, $t3, 0", 0x80000000, 0, t5, t3);
   TESTDSPINST_RD_RT_SA_NODSPC("shrl.qb $t2, $t4, 7", 0xff01ffff, 7, t2, t4);
   TESTDSPINST_RD_RT_SA_NODSPC("shrl.qb $t0, $t8, 7", 0x7fff7fff, 7, t0, t8);
   TESTDSPINST_RD_RT_SA_NODSPC("shrl.qb $t4, $t6, 0", 0x00000555, 0, t4, t6);
   TESTDSPINST_RD_RT_SA_NODSPC("shrl.qb $t0, $t1, 1", 0x00000000, 1, t0, t1);
   TESTDSPINST_RD_RT_SA_NODSPC("shrl.qb $t2, $t3, 2", 0x80000000, 2, t2, t3);
   TESTDSPINST_RD_RT_SA_NODSPC("shrl.qb $t4, $t1, 3", 0x55555555, 3, t4, t1);
   TESTDSPINST_RD_RT_SA_NODSPC("shrl.qb $t6, $t7, 4", 0xffff2435, 4, t6, t7);
   TESTDSPINST_RD_RT_SA_NODSPC("shrl.qb $t5, $t3, 5", 0xabababab, 5, t5, t3);
   TESTDSPINST_RD_RT_SA_NODSPC("shrl.qb $t2, $t4, 6", 0xfc79b4d2, 6, t2, t4);
   TESTDSPINST_RD_RT_SA_NODSPC("shrl.qb $t0, $t8, 7", 0x00000000, 7, t0, t8);
   TESTDSPINST_RD_RT_SA_NODSPC("shrl.qb $t4, $t6, 0", 0x00354565, 0, t4, t6);
   TESTDSPINST_RD_RT_SA_NODSPC("shrl.qb $t0, $t1, 1", 0x00086755, 1, t0, t1);
   TESTDSPINST_RD_RT_SA_NODSPC("shrl.qb $t2, $t3, 2", 0x8f8f8f8f, 2, t2, t3);
   TESTDSPINST_RD_RT_SA_NODSPC("shrl.qb $t4, $t1, 3", 0xeeeeeeee, 3, t4, t1);
   TESTDSPINST_RD_RT_SA_NODSPC("shrl.qb $t6, $t7, 4", 0x1bdbdbdb, 4, t6, t7);
   TESTDSPINST_RD_RT_SA_NODSPC("shrl.qb $t5, $t3, 5", 0xdecadeca, 5, t5, t3);
   TESTDSPINST_RD_RT_SA_NODSPC("shrl.qb $t2, $t4, 6", 0x93474bde, 6, t2, t4);
   TESTDSPINST_RD_RT_SA_NODSPC("shrl.qb $t0, $t8, 7", 0xfc0007ff, 7, t0, t8);
   TESTDSPINST_RD_RT_SA_NODSPC("shrl.qb $t4, $t6, 0", 0xffffffff, 0, t4, t6);
   TESTDSPINST_RD_RT_SA_NODSPC("shrl.qb $t0, $t1, 3", 0xcb4ab48f, 3, t0, t1);
   TESTDSPINST_RD_RT_SA_NODSPC("shrl.qb $t2, $t3, 4", 0xaf8f7e18, 4, t2, t3);
   TESTDSPINST_RD_RT_SA_NODSPC("shrl.qb $t4, $t1, 0", 0x87df4510, 0, t4, t1);
   TESTDSPINST_RD_RT_SA_NODSPC("shrl.qb $t6, $t7, 7", 0xabf4e8e1, 7, t6, t7);
   TESTDSPINST_RD_RT_SA_NODSPC("shrl.qb $t5, $t3, 7", 0xf4c0eeac, 7, t5, t3);
   TESTDSPINST_RD_RT_SA_NODSPC("shrl.qb $t2, $t4, 5", 0x006a54f2, 5, t2, t4);
   TESTDSPINST_RD_RT_SA_NODSPC("shrl.qb $t0, $t8, 1", 0x79f74493, 1, t0, t8);
   TESTDSPINST_RD_RT_SA_NODSPC("shrl.qb $t4, $t6, 2", 0x9c09e313, 2, t4, t6);

   printf("-------- SHRLV.QB -------- \n");
   TESTDSPINST_RD_RT_RS_NODSPC("shrlv.qb $t0, $t1, $t2", 0x2fff0000, 0x00000001,
                               t0, t1, t2);
   TESTDSPINST_RD_RT_RS_NODSPC("shrlv.qb $t2, $t3, $t4", 0x2fff0000, 0x73741802,
                               t2, t3, t4);
   TESTDSPINST_RD_RT_RS_NODSPC("shrlv.qb $t4, $t1, $t5", 0x2fff0000, 0x80003403,
                               t4, t1, t5);
   TESTDSPINST_RD_RT_RS_NODSPC("shrlv.qb $t6, $t7, $t3", 0xff460000, 0x73468004,
                               t6, t7, t3);
   TESTDSPINST_RD_RT_RS_NODSPC("shrlv.qb $t5, $t3, $t2", 0x00008000, 0x80000000,
                               t5, t3, t2);
   TESTDSPINST_RD_RT_RS_NODSPC("shrlv.qb $t2, $t4, $t8", 0x00010001, 0xffffff07,
                               t2, t4, t8);
   TESTDSPINST_RD_RT_RS_NODSPC("shrlv.qb $t0, $t8, $t0", 0x7fff7fff, 0x7fff7f07,
                               t0, t8, t0);
   TESTDSPINST_RD_RT_RS_NODSPC("shrlv.qb $t4, $t6, $t1", 0xffffffff, 0x00000505,
                               t4, t6, t1);
   TESTDSPINST_RD_RT_RS_NODSPC("shrlv.qb $t0, $t1, $t2", 0xabababab, 0x00000000,
                               t0, t1, t2);
   TESTDSPINST_RD_RT_RS_NODSPC("shrlv.qb $t2, $t3, $t4", 0xdecadeca, 0x80000000,
                               t2, t3, t4);
   TESTDSPINST_RD_RT_RS_NODSPC("shrlv.qb $t4, $t1, $t5", 0xbacabaca, 0x55555555,
                               t4, t1, t5);
   TESTDSPINST_RD_RT_RS_NODSPC("shrlv.qb $t6, $t7, $t3", 0x3545ff80, 0xffff2434,
                               t6, t7, t3);
   TESTDSPINST_RD_RT_RS_NODSPC("shrlv.qb $t5, $t3, $t2", 0x734680bc, 0xabababa3,
                               t5, t3, t2);
   TESTDSPINST_RD_RT_RS_NODSPC("shrlv.qb $t2, $t4, $t8", 0xc4dbfe20, 0xfc79b4d2,
                               t2, t4, t8);
   TESTDSPINST_RD_RT_RS_NODSPC("shrlv.qb $t0, $t8, $t0", 0x00000000, 0x00000000,
                               t0, t8, t0);
   TESTDSPINST_RD_RT_RS_NODSPC("shrlv.qb $t4, $t3, $t1", 0x55555555, 0x00354561,
                               t4, t3, t1);
   TESTDSPINST_RD_RT_RS_NODSPC("shrlv.qb $t0, $t1, $t2", 0xad80bce4, 0x00086755,
                               t0, t1, t2);
   TESTDSPINST_RD_RT_RS_NODSPC("shrlv.qb $t2, $t3, $t4", 0x7f003245, 0x8f8f8f8f,
                               t2, t3, t4);
   TESTDSPINST_RD_RT_RS_NODSPC("shrlv.qb $t4, $t1, $t5", 0x93474bde, 0xeeeeeeee,
                               t4, t1, t5);
   TESTDSPINST_RD_RT_RS_NODSPC("shrlv.qb $t6, $t7, $t3", 0xf97343ff, 0x1bdbdbdb,
                               t6, t7, t3);
   TESTDSPINST_RD_RT_RS_NODSPC("shrlv.qb $t5, $t3, $t2", 0x980b7cde, 0xdecadeca,
                               t5, t3, t2);
   TESTDSPINST_RD_RT_RS_NODSPC("shrlv.qb $t2, $t4, $t8", 0x0555adec, 0x93474bde,
                               t2, t4, t8);
   TESTDSPINST_RD_RT_RS_NODSPC("shrlv.qb $t0, $t8, $t0", 0x23534870, 0xfc0007ff,
                               t0, t8, t0);
   TESTDSPINST_RD_RT_RS_NODSPC("shrlv.qb $t4, $t3, $t1", 0x80003286, 0xffffffff,
                               t4, t3, t1);
   TESTDSPINST_RD_RT_RS_NODSPC("shrlv.qb $t4, $t6, $t1", 0x4387ffff, 0xdecadeca,
                               t4, t6, t1);
   TESTDSPINST_RD_RT_RS_NODSPC("shrlv.qb $t0, $t1, $t2", 0x0cd6b508, 0xbacabaca,
                               t0, t1, t2);
   TESTDSPINST_RD_RT_RS_NODSPC("shrlv.qb $t2, $t3, $t4", 0x6731e282, 0x3545ff80,
                               t2, t3, t4);
   TESTDSPINST_RD_RT_RS_NODSPC("shrlv.qb $t4, $t1, $t5", 0x26edf28f, 0x734680bc,
                               t4, t1, t5);
   TESTDSPINST_RD_RT_RS_NODSPC("shrlv.qb $t6, $t7, $t3", 0x4b4ec9ca, 0xc4dbfe20,
                               t6, t7, t3);
   TESTDSPINST_RD_RT_RS_NODSPC("shrlv.qb $t5, $t3, $t2", 0xc1037fa4, 0x00000000,
                               t5, t3, t2);
   TESTDSPINST_RD_RT_RS_NODSPC("shrlv.qb $t2, $t4, $t8", 0xcb4ab48f, 0x55555555,
                               t2, t4, t8);
   TESTDSPINST_RD_RT_RS_NODSPC("shrlv.qb $t0, $t8, $t0", 0xaf8f7e18, 0xad80bce4,
                               t0, t8, t0);
   TESTDSPINST_RD_RT_RS_NODSPC("shrlv.qb $t4, $t3, $t1", 0x87df4510, 0x7f003245,
                               t4, t3, t1);
   TESTDSPINST_RD_RT_RS_NODSPC("shrlv.qb $t0, $t1, $t2", 0xabf4e8e1, 0x93474bde,
                               t0, t1, t2);
   TESTDSPINST_RD_RT_RS_NODSPC("shrlv.qb $t2, $t3, $t4", 0xf4c0eeac, 0xf97343ff,
                               t2, t3, t4);
   TESTDSPINST_RD_RT_RS_NODSPC("shrlv.qb $t4, $t1, $t5", 0x006a54f2, 0x980b7cde,
                               t4, t1, t5);
   TESTDSPINST_RD_RT_RS_NODSPC("shrlv.qb $t6, $t7, $t3", 0x79f74493, 0x0555adec,
                               t6, t7, t3);
   TESTDSPINST_RD_RT_RS_NODSPC("shrlv.qb $t5, $t3, $t2", 0x9c09e313, 0x23534870,
                               t5, t3, t2);
   TESTDSPINST_RD_RT_RS_NODSPC("shrlv.qb $t2, $t4, $t8", 0x9c09e313, 0x9c09e313,
                               t2, t4, t8);
   TESTDSPINST_RD_RT_RS_NODSPC("shrlv.qb $t0, $t8, $t0", 0x80000000, 0x80000000,
                               t0, t8, t0);
   TESTDSPINST_RD_RT_RS_NODSPC("shrlv.qb $t4, $t3, $t1", 0x004d8000, 0x004d8000,
                               t4, t3, t1);

   printf("-------- SUBQ.PH --------\n");
   TESTDSPINST_RD_RS_RT_DSPC("subq.ph $t0, $t1, $t2", 0x00000000, 0x00000000,
                             t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("subq.ph $t2, $t3, $t4", 0x045fb232, 0x00028632,
                             t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("subq.ph $t4, $t1, $t5", 0xfabc3435, 0xfabc3421,
                             t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("subq.ph $t6, $t7, $t3", 0x07654cb8, 0x734680bc,
                             t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("subq.ph $t5, $t3, $t2", 0xf973437b, 0x80000000,
                             t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("subq.ph $t2, $t4, $t8", 0x00ff0001, 0xff01ffff,
                             t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("subq.ph $t0, $t8, $t0", 0x7fff7004, 0x7fff7fff,
                             t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("subq.ph $t4, $t6, $t1", 0x0000c420, 0x00000555,
                             t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("subq.ph $t0, $t1, $t2", 0x00000000, 0x00000000,
                             t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("subq.ph $t2, $t3, $t4", 0x80000000, 0x80000000,
                             t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("subq.ph $t4, $t1, $t5", 0xaaaaaaaa, 0x55555555,
                             t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("subq.ph $t6, $t7, $t3", 0x00000018, 0xffff2435,
                             t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("subq.ph $t5, $t3, $t2", 0xbabababa, 0xabababab,
                             t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("subq.ph $t2, $t4, $t8", 0xf0f0f0f0, 0xfc79b4d2,
                             t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("subq.ph $t0, $t8, $t0", 0xfbde3976, 0x00000000,
                             t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("subq.ph $t4, $t6, $t1", 0x23534870, 0x00354565,
                             t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("subq.ph $t0, $t1, $t2", 0x980b7cde, 0x00086755,
                             t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("subq.ph $t2, $t3, $t4", 0x00000018, 0x8f8f8f8f,
                             t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("subq.ph $t4, $t1, $t5", 0x92784656, 0xeeeeeeee,
                             t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("subq.ph $t6, $t7, $t3", 0xcacacaca, 0x1bdbdbdb,
                             t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("subq.ph $t5, $t3, $t2", 0xbacabaca, 0xdecadeca,
                             t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("subq.ph $t2, $t4, $t8", 0x12fadeb4, 0x93474bde,
                             t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("subq.ph $t0, $t8, $t0", 0x7c000790, 0xfc0007ff,
                             t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("subq.ph $t4, $t6, $t1", 0xffffffff, 0xffffffff,
                             t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("subq.ph $t0, $t1, $t2", 0xf2f4df1f, 0xcb4ab48f,
                             t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("subq.ph $t2, $t3, $t4", 0x435f909a, 0xaf8f7e18,
                             t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("subq.ph $t4, $t1, $t5", 0x2106ba5f, 0x87df4510,
                             t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("subq.ph $t6, $t7, $t3", 0x246a6376, 0xabf4e8e1,
                             t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("subq.ph $t5, $t3, $t2", 0x1046a1a3, 0xf4c0eeac,
                             t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("subq.ph $t2, $t4, $t8", 0x638ca515, 0x006a54f2,
                             t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("subq.ph $t0, $t8, $t0", 0xf63e7a9d, 0x79f74493,
                             t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("subq.ph $t4, $t6, $t1", 0xbd6845cd, 0x9c09e313,
                             t4, t6, t1);

   printf("-------- SUBQ_S.PH --------\n");
   TESTDSPINST_RD_RS_RT_DSPC("subq_s.ph $t0, $t1, $t2", 0x00000000, 0x00000000,
                             t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("subq_s.ph $t2, $t3, $t4", 0x00020002, 0x00010001,
                             t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("subq_s.ph $t4, $t1, $t5", 0x0002fffe, 0x0001ffff,
                             t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("subq_s.ph $t6, $t7, $t3", 0x7fff8000, 0x7fff8000,
                             t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("subq_s.ph $t5, $t3, $t2", 0x7fff8000, 0x7ffe8001,
                             t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("subq_s.ph $t0, $t1, $t2", 0x00000000, 0x00000000,
                             t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("subq_s.ph $t2, $t3, $t4", 0x045fb232, 0x00028632,
                             t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("subq_s.ph $t4, $t1, $t5", 0xfabc3435, 0xfabc3421,
                             t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("subq_s.ph $t6, $t7, $t3", 0x07654cb8, 0x734680bc,
                             t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("subq_s.ph $t5, $t3, $t2", 0xf973437b, 0x80000000,
                             t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("subq_s.ph $t2, $t4, $t8", 0x00ff0001, 0xff01ffff,
                             t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("subq_s.ph $t0, $t8, $t0", 0x7fff7004, 0x7fff7fff,
                             t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("subq_s.ph $t4, $t6, $t1", 0x0000c420, 0x00000555,
                             t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("subq_s.ph $t0, $t1, $t2", 0x00000000, 0x00000000,
                             t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("subq_s.ph $t2, $t3, $t4", 0x80000000, 0x80000000,
                             t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("subq_s.ph $t4, $t1, $t5", 0xaaaaaaaa, 0x55555555,
                             t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("subq_s.ph $t6, $t7, $t3", 0x00000018, 0xffff2435,
                             t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("subq_s.ph $t5, $t3, $t2", 0xbabababa, 0xabababab,
                             t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("subq_s.ph $t2, $t4, $t8", 0xf0f0f0f0, 0xfc79b4d2,
                             t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("subq_s.ph $t0, $t8, $t0", 0xfbde3976, 0x00000000,
                             t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("subq_s.ph $t4, $t6, $t1", 0x23534870, 0x00354565,
                             t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("subq_s.ph $t0, $t1, $t2", 0x980b7cde, 0x00086755,
                             t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("subq_s.ph $t2, $t3, $t4", 0x00000018, 0x8f8f8f8f,
                             t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("subq_s.ph $t4, $t1, $t5", 0x92784656, 0xeeeeeeee,
                             t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("subq_s.ph $t6, $t7, $t3", 0xcacacaca, 0x1bdbdbdb,
                             t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("subq_s.ph $t5, $t3, $t2", 0xbacabaca, 0xdecadeca,
                             t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("subq_s.ph $t2, $t4, $t8", 0x12fadeb4, 0x93474bde,
                             t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("subq_s.ph $t0, $t8, $t0", 0x7c000790, 0xfc0007ff,
                             t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("subq_s.ph $t4, $t6, $t1", 0xffffffff, 0xffffffff,
                             t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("subq_s.ph $t0, $t1, $t2", 0xf2f4df1f, 0xcb4ab48f,
                             t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("subq_s.ph $t2, $t3, $t4", 0x435f909a, 0xaf8f7e18,
                             t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("subq_s.ph $t4, $t1, $t5", 0x2106ba5f, 0x87df4510,
                             t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("subq_s.ph $t6, $t7, $t3", 0x246a6376, 0xabf4e8e1,
                             t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("subq_s.ph $t5, $t3, $t2", 0x1046a1a3, 0xf4c0eeac,
                             t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("subq_s.ph $t2, $t4, $t8", 0x638ca515, 0x006a54f2,
                             t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("subq_s.ph $t0, $t8, $t0", 0xf63e7a9d, 0x79f74493,
                             t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("subq_s.ph $t4, $t6, $t1", 0xbd6845cd, 0x9c09e313,
                             t4, t6, t1);

   printf("-------- SUBQ_S.W --------\n");
   TESTDSPINST_RD_RS_RT_DSPC("subq_s.w $t0, $t1, $t2", 0x7fffffff, 0x00000000,
                             t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("subq_s.w $t2, $t3, $t4", 0x80000000, 0x00000000,
                             t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("subq_s.w $t4, $t1, $t5", 0xfabc3435, 0xfabc3421,
                             t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("subq_s.w $t6, $t7, $t3", 0x07654cb8, 0x734680bc,
                             t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("subq_s.w $t5, $t3, $t2", 0xf973437b, 0x80000000,
                             t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("subq_s.w $t2, $t4, $t8", 0x00ff0001, 0xff01ffff,
                             t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("subq_s.w $t0, $t8, $t0", 0x7fff7004, 0x7fff7fff,
                             t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("subq_s.w $t4, $t6, $t1", 0x0000c420, 0x00000555,
                             t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("subq_s.w $t0, $t1, $t2", 0x00000000, 0x00000000,
                             t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("subq_s.w $t2, $t3, $t4", 0x80000000, 0x80000000,
                             t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("subq_s.w $t4, $t1, $t5", 0xaaaaaaaa, 0x55555555,
                             t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("subq_s.w $t6, $t7, $t3", 0x00000018, 0xffff2435,
                             t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("subq_s.w $t5, $t3, $t2", 0xbabababa, 0xabababab,
                             t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("subq_s.w $t2, $t4, $t8", 0xf0f0f0f0, 0xfc79b4d2,
                             t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("subq_s.w $t0, $t8, $t0", 0xfbde3976, 0x00000000,
                             t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("subq_s.w $t4, $t6, $t1", 0x23534870, 0x00354565,
                             t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("subq_s.w $t0, $t1, $t2", 0x980b7cde, 0x00086755,
                             t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("subq_s.w $t2, $t3, $t4", 0x00000018, 0x8f8f8f8f,
                             t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("subq_s.w $t4, $t1, $t5", 0x92784656, 0xeeeeeeee,
                             t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("subq_s.w $t6, $t7, $t3", 0xcacacaca, 0x1bdbdbdb,
                             t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("subq_s.w $t5, $t3, $t2", 0xbacabaca, 0xdecadeca,
                             t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("subq_s.w $t2, $t4, $t8", 0x12fadeb4, 0x93474bde,
                             t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("subq_s.w $t0, $t8, $t0", 0x7c000790, 0xfc0007ff,
                             t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("subq_s.w $t4, $t6, $t1", 0xffffffff, 0xffffffff,
                             t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("subq_s.w $t0, $t1, $t2", 0xf2f4df1f, 0xcb4ab48f,
                             t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("subq_s.w $t2, $t3, $t4", 0x435f909a, 0xaf8f7e18,
                             t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("subq_s.w $t4, $t1, $t5", 0x2106ba5f, 0x87df4510,
                             t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("subq_s.w $t6, $t7, $t3", 0x246a6376, 0xabf4e8e1,
                             t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("subq_s.w $t5, $t3, $t2", 0x1046a1a3, 0xf4c0eeac,
                             t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("subq_s.w $t2, $t4, $t8", 0x638ca515, 0x006a54f2,
                             t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("subq_s.w $t0, $t8, $t0", 0xf63e7a9d, 0x79f74493,
                             t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("subq_s.w $t4, $t6, $t1", 0xbd6845cd, 0x9c09e313,
                             t4, t6, t1);

   printf("-------- SUBU.QB --------\n");
   TESTDSPINST_RD_RS_RT_DSPC("subu.qb $t0, $t1, $t2", 0x7fffffff, 0x00000000,
                             t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("subu.qb $t2, $t3, $t4", 0x80000000, 0x00000000,
                             t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("subu.qb $t4, $t1, $t5", 0xfabc3435, 0xfabc3421,
                             t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("subu.qb $t6, $t7, $t3", 0x07654cb8, 0x734680bc,
                             t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("subu.qb $t5, $t3, $t2", 0xf973437b, 0x80000000,
                             t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("subu.qb $t2, $t4, $t8", 0x00ff0001, 0xff01ffff,
                             t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("subu.qb $t0, $t8, $t0", 0x7fff7004, 0x7fff7fff,
                             t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("subu.qb $t4, $t6, $t1", 0x0000c420, 0x00000555,
                             t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("subu.qb $t0, $t1, $t2", 0x00000000, 0x00000000,
                             t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("subu.qb $t2, $t3, $t4", 0x80000000, 0x80000000,
                             t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("subu.qb $t4, $t1, $t5", 0xaaaaaaaa, 0x55555555,
                             t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("subu.qb $t6, $t7, $t3", 0x00000018, 0xffff2435,
                             t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("subu.qb $t5, $t3, $t2", 0xbabababa, 0xabababab,
                             t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("subu.qb $t2, $t4, $t8", 0xf0f0f0f0, 0xfc79b4d2,
                             t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("subu.qb $t0, $t8, $t0", 0xfbde3976, 0x00000000,
                             t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("subu.qb $t4, $t6, $t1", 0x23534870, 0x00354565,
                             t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("subu.qb $t0, $t1, $t2", 0x980b7cde, 0x00086755,
                             t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("subu.qb $t2, $t3, $t4", 0x00000018, 0x8f8f8f8f,
                             t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("subu.qb $t4, $t1, $t5", 0x92784656, 0xeeeeeeee,
                             t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("subu.qb $t6, $t7, $t3", 0xcacacaca, 0x1bdbdbdb,
                             t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("subu.qb $t5, $t3, $t2", 0xbacabaca, 0xdecadeca,
                             t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("subu.qb $t2, $t4, $t8", 0x12fadeb4, 0x93474bde,
                             t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("subu.qb $t0, $t8, $t0", 0x7c000790, 0xfc0007ff,
                             t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("subu.qb $t4, $t6, $t1", 0xffffffff, 0xffffffff,
                             t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("subu.qb $t0, $t1, $t2", 0xf2f4df1f, 0xcb4ab48f,
                             t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("subu.qb $t2, $t3, $t4", 0x435f909a, 0xaf8f7e18,
                             t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("subu.qb $t4, $t1, $t5", 0x2106ba5f, 0x87df4510,
                             t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("subu.qb $t6, $t7, $t3", 0x246a6376, 0xabf4e8e1,
                             t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("subu.qb $t5, $t3, $t2", 0x1046a1a3, 0xf4c0eeac,
                             t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("subu.qb $t2, $t4, $t8", 0x638ca515, 0x006a54f2,
                             t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("subu.qb $t0, $t8, $t0", 0xf63e7a9d, 0x79f74493,
                             t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("subu.qb $t4, $t6, $t1", 0xbd6845cd, 0x9c09e313,
                             t4, t6, t1);

   printf("-------- SUBU_S.QB --------\n");
   TESTDSPINST_RD_RS_RT_DSPC("subu_s.qb $t0, $t1, $t2", 0x7fffffff, 0x00000000,
                             t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("subu_s.qb $t2, $t3, $t4", 0x80000000, 0x00000000,
                             t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("subu_s.qb $t4, $t1, $t5", 0xfabc3435, 0xfabc3421,
                             t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("subu_s.qb $t6, $t7, $t3", 0x07654cb8, 0x734680bc,
                             t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("subu_s.qb $t5, $t3, $t2", 0xf973437b, 0x80000000,
                             t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("subu_s.qb $t2, $t4, $t8", 0x00ff0001, 0xff01ffff,
                             t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("subu_s.qb $t0, $t8, $t0", 0x7fff7004, 0x7fff7fff,
                             t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("subu_s.qb $t4, $t6, $t1", 0x0000c420, 0x00000555,
                             t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("subu_s.qb $t0, $t1, $t2", 0x00000000, 0x00000000,
                             t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("subu_s.qb $t2, $t3, $t4", 0x80000000, 0x80000000,
                             t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("subu_s.qb $t4, $t1, $t5", 0xaaaaaaaa, 0x55555555,
                             t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("subu_s.qb $t6, $t7, $t3", 0x00000018, 0xffff2435,
                             t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("subu_s.qb $t5, $t3, $t2", 0xbabababa, 0xabababab,
                             t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("subu_s.qb $t2, $t4, $t8", 0xf0f0f0f0, 0xfc79b4d2,
                             t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("subu_s.qb $t0, $t8, $t0", 0xfbde3976, 0x00000000,
                             t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("subu_s.qb $t4, $t6, $t1", 0x23534870, 0x00354565,
                             t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("subu_s.qb $t0, $t1, $t2", 0x980b7cde, 0x00086755,
                             t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("subu_s.qb $t2, $t3, $t4", 0x00000018, 0x8f8f8f8f,
                             t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("subu_s.qb $t4, $t1, $t5", 0x92784656, 0xeeeeeeee,
                             t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("subu_s.qb $t6, $t7, $t3", 0xcacacaca, 0x1bdbdbdb,
                             t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("subu_s.qb $t5, $t3, $t2", 0xbacabaca, 0xdecadeca,
                             t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("subu_s.qb $t2, $t4, $t8", 0x12fadeb4, 0x93474bde,
                             t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("subu_s.qb $t0, $t8, $t0", 0x7c000790, 0xfc0007ff,
                             t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("subu_s.qb $t4, $t6, $t1", 0xffffffff, 0xffffffff,
                             t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("subu_s.qb $t0, $t1, $t2", 0xf2f4df1f, 0xcb4ab48f,
                             t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("subu_s.qb $t2, $t3, $t4", 0x435f909a, 0xaf8f7e18,
                             t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("subu_s.qb $t4, $t1, $t5", 0x2106ba5f, 0x87df4510,
                             t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("subu_s.qb $t6, $t7, $t3", 0x246a6376, 0xabf4e8e1,
                             t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("subu_s.qb $t5, $t3, $t2", 0x1046a1a3, 0xf4c0eeac,
                             t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("subu_s.qb $t2, $t4, $t8", 0x638ca515, 0x006a54f2,
                             t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("subu_s.qb $t0, $t8, $t0", 0xf63e7a9d, 0x79f74493,
                             t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("subu_s.qb $t4, $t6, $t1", 0xbd6845cd, 0x9c09e313,
                             t4, t6, t1);
#endif

   return 0;
}