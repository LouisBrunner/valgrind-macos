#include <stdio.h>
/* Independent tests for each DSP instruction from MIPS32 DSP ASEr2 instruction
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

#define TESTDSPINST_RD_RT_DSPC(instruction, RTval, RD, RT)                 \
{                                                                          \
   int out = 0xdeadbeef;                                                   \
   int dspCtrl = 0x0;                                                      \
   __asm__ volatile(                                                       \
      ".set dspr2; \n\t"                                                   \
      "li $" #RD ", 0 \n\t"                                                \
      "move $" #RT ", %2 \n\t"                                             \
      "wrdsp $zero, 0x3f \n\t"                                             \
      instruction " \n\t"                                                  \
      "move %0, $" #RD " \n\t"                                             \
      "rddsp %1, 0x3f  \n\t"                                               \
      : "=&r" (out), "=&r" (dspCtrl)                                       \
      : "r" (RTval)                                                        \
      : #RT, #RD                                                           \
   );                                                                      \
   printf("%s :: rd 0x%08x rt 0x%08x DSPControl 0x%x\n", instruction, out, \
          RTval, dspCtrl);                                                 \
}

#define TESTDSPINST_RD_RT_NODSPC(instruction, RTval, RD, RT)        \
{                                                                   \
   int out = 0xdeadbeef;                                            \
   __asm__ volatile(                                                \
      ".set dspr2; \n\t"                                            \
      "li $" #RD ", 0 \n\t"                                         \
      "move $" #RT ", %1 \n\t"                                      \
      instruction " \n\t"                                           \
      "move %0, $" #RD " \n\t"                                      \
      : "=&r" (out)                                                 \
      : "r" (RTval)                                                 \
      : #RT, #RD                                                    \
   );                                                               \
   printf("%s :: rd 0x%08x rt 0x%08x \n", instruction, out, RTval); \
}

#define TESTDSPINST_RD_RT_RS_NODSPC(instruction, RTval, RSval)               \
{                                                                            \
   int out = 0;                                                              \
   __asm__ __volatile__(                                                     \
      ".set dspr2; \n\t"                                                     \
      "move $t1, %1 \n\t"                                                    \
      "move $t2, %2 \n\t"                                                    \
      instruction" $t0, $t1, $t2 \n\t"                                       \
      "move %0, $t0 \n\t"                                                    \
      : "=&r" (out)                                                          \
      : "r" (RTval), "r" (RSval)                                             \
      : "t0", "t1", "t2"                                                     \
   );                                                                        \
   printf("%s   out=0x%08x, RTval=0x%08x, RSval=0x%08x\n", instruction, out, \
          RTval, RSval);                                                     \
}

#define TESTDSPINST_RD_RS_RT_DSPC(instruction, RSval, RTval, RD, RS, RT)       \
{                                                                              \
   int out = 0xdeadbeef;                                                       \
   int dspCtrl = 0x0;                                                          \
   __asm__ volatile(                                                           \
      ".set dspr2; \n\t"                                                       \
      "li $" #RD ", 0 \n\t"                                                    \
      "wrdsp $zero, 0x3f \n\t"                                                 \
      "move $" #RS ", %2 \n\t"                                                 \
      "move $" #RT ", %3 \n\t"                                                 \
      instruction " \n\t"                                                      \
      "move %0, $" #RD " \n\t"                                                 \
      "rddsp %1, 0x3f \n\t"                                                    \
      : "=&r" (out), "=&r" (dspCtrl)                                           \
      : "r" (RSval), "r"(RTval)                                                \
      : #RD, #RS, #RT                                                          \
   );                                                                          \
   printf("%s :: rs 0x%08x rt 0x%08x out 0x%08x DSPCtrl 0x%08x\n", instruction,\
          RSval, RTval, out, dspCtrl);                                         \
}

#define TESTDSPINST_BPOSGE32(instruction, RDval, POSval, RD, POSreg) \
{                                                                    \
   unsigned int out = 0;                                             \
   __asm__ volatile(                                                 \
      ".set dspr2; \n\t"                                             \
      "move $" #POSreg ", %1 \n\t"                                   \
      "wrdsp $" #POSreg ", 0x3f \n\t"                                \
      "move $" #RD ", %2 \n\t"                                       \
      instruction" end"instruction#RDval" \n\t"                      \
      "nop \n\t"                                                     \
      "addi $" #RD ", $" #RD", 5 \n\t"                               \
      "end"instruction#RDval": \n\t"                                 \
      "addi $" #RD ", $" #RD", 1 \n\t"                               \
      "move %0, $" #RD " \n\t"                                       \
      : "=&r" (out)                                                  \
      : "r" (POSval), "r" (RDval)                                    \
      : #RD, #POSreg                                                 \
      );                                                             \
      printf(instruction" :: %d, POSval: %d\n", out, POSval);        \
}

#define TESTDSPINST_RS_RT_DSPC(instruction, RSval, RTval, RS, RT)            \
{                                                                            \
   int dspCtrl = 0x0;                                                        \
   __asm__ volatile(                                                         \
      ".set dspr2; \n\t"                                                     \
      "wrdsp $zero, 0x3f \n\t"                                               \
      "move $" #RS ", %1 \n\t"                                               \
      "move $" #RT ", %2 \n\t"                                               \
      instruction " \n\t"                                                    \
      "rddsp %0, 0x3f \n\t"                                                  \
      : "=&r" (dspCtrl)                                                      \
      : "r" (RSval), "r"(RTval)                                              \
      : #RS, #RT                                                             \
   );                                                                        \
   printf("%s :: rs 0x%08x rt 0x%08x DSPCtrl 0x%08x \n", instruction, RSval, \
          RTval, dspCtrl);                                                   \
}

#define TESTDSPINST_RD_RS_RT_NODSPC(instruction, RSval, RTval, RD, RS, RT)     \
{                                                                              \
   int out = 0xdeadbeef;                                                       \
   __asm__ volatile(                                                           \
      ".set dspr2; \n\t"                                                       \
      "li $" #RD ", 0 \n\t"                                                    \
      "move $" #RS ", %1 \n\t"                                                 \
      "move $" #RT ", %2 \n\t"                                                 \
      instruction " \n\t"                                                      \
      "move %0, $" #RD " \n\t"                                                 \
      : "=&r" (out)                                                            \
      : "r" (RSval), "r"(RTval)                                                \
      : #RD, #RS, #RT                                                          \
   );                                                                          \
   printf("%s :: rs 0x%08x rt 0x%08x out 0x%08x\n", instruction, RSval, RTval, \
          out);                                                                \
}

#define TESTDSPINST_AC_RS_RT_DSPC(instruction, ac, RSval, RTval, HIval, LOval, \
                                  RS, RT)                                      \
{                                                                              \
   int out_hi = 0xdeadbeef;                                                    \
   int out_lo = 0xdeadbeef;                                                    \
   int dspCtrl = 0x0;                                                          \
   __asm__ volatile(                                                           \
      ".set dspr2; \n\t"                                                       \
      "move $" #RS ", %5 \n\t"                                                 \
      "move $" #RT ", %6 \n\t"                                                 \
      "mthi $" #RS", $" ac " \n\t"                                             \
      "mtlo $" #RT", $" ac " \n\t"                                             \
      "move $" #RS ", %3 \n\t"                                                 \
      "move $" #RT ", %4 \n\t"                                                 \
      "wrdsp $zero, 0x3f \n\t"                                                 \
      instruction " \n\t"                                                      \
      "rddsp %2, 0x3f \n\t"                                                    \
      "mfhi %0, $" ac " \n\t"                                                  \
      "mflo %1, $" ac " \n\t"                                                  \
      : "=&r" (out_hi), "=&r" (out_lo), "=&r" (dspCtrl)                        \
      : "r" (RSval), "r"(RTval), "r" (HIval), "r"(LOval)                       \
      : #RS, #RT                                                               \
   );                                                                          \
   printf("%s :: rs 0x%08x rt 0x%08x inHI 0x%08x inLO 0x%08x outHI 0x%08x "    \
          "outLO 0x%08x dspCtrl 0x%08x\n",instruction, RSval, RTval, HIval,    \
          LOval, out_hi, out_lo, dspCtrl);\
}

#define TESTDSPINST_AC_RS_RT_NODSPC(instruction, HIval, LOval, RSval, RTval) \
{                                                                            \
   int HIout = 0;                                                            \
   int LOout = 0;                                                            \
   __asm__ __volatile__(                                                     \
      ".set dspr2; \n\t"                                                     \
      "li $t0, 0 \n\t"                                                       \
      "li $t1, 0 \n\t"                                                       \
      "mthi %2, $ac0 \n\t"                                                   \
      "mtlo %3, $ac0 \n\t"                                                   \
      "move $t0, %4 \n\t"                                                    \
      "move $t1, %5 \n\t"                                                    \
      instruction" $ac0, $t0, $t1 \n\t"                                      \
      "mfhi %0, $ac0 \n\t"                                                   \
      "mflo %1, $ac0 \n\t"                                                   \
      : "=&r" (HIout), "=&r" (LOout)                                         \
      : "r" (HIval), "r" (LOval), "r" (RSval), "r" (RTval)                   \
      : "t0", "t1"                                                           \
   );                                                                        \
   printf("%s   HIout=0x%08x, LOout=0x%08x, HIin=0x%08x, LOin=0x%08x, "      \
          "RSval=0x%08x, RTval=0x%08x\n", instruction, HIout, LOout, HIval,  \
          LOval, RSval, RTval);                                              \
}

#define TESTDSPINST_EXT(instruction, ac, RT, HIval, LOval, size, pos) \
{                                                                     \
   int out = 0xdeadbeef;                                              \
   int dspCtrl = 0x0;                                                 \
   __asm__ volatile(                                                  \
      ".set dspr2; \n\t"                                              \
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
          instruction, out, ac, HIval, LOval, size, dspCtrl);         \
}

#define TESTDSPINST_EXTV(instruction, ac, RT, HIval, LOval, RS, RSval, pos) \
{                                                                           \
   int out = 0xdeadbeef;                                                    \
   int dspCtrl = 0x0;                                                       \
   __asm__ volatile(                                                        \
      ".set dspr2; \n\t"                                                    \
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
      ".set dspr2; \n\t"                                                       \
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
         out, RTval, RSval, _pos, _size>>7);                                   \
}

#define TESTDSPINST_LWX(index, RT, RS)                 \
{                                                      \
    unsigned int out;                                  \
   __asm__ volatile(                                   \
      ".set dspr2; \n\t"                               \
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
      ".set dspr2; \n\t"                               \
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
      ".set dspr2; \n\t"                                \
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
      ".set dspr2; \n\t"                                                     \
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

#define TESTDSPINST_MTHLIP(instruction, ac, HIval, LOval, RSval, RS, pos) \
{                                                                         \
   unsigned int outHI;                                                    \
   unsigned int outLO;                                                    \
   unsigned int dspCtrl;                                                  \
   __asm__ volatile(                                                      \
      ".set dspr2; \n\t"                                                  \
      "move $" #RS ", %3 \n\t"                                            \
      "mthi $" #RS", $" ac " \n\t"                                        \
      "move $" #RS ", %4 \n\t"                                            \
      "mtlo $" #RS", $" ac " \n\t"                                        \
      "move $" #RS ", %5 \n\t"                                            \
      "wrdsp $" #RS ", 0x1 \n\t"                                          \
      "move $" #RS ", %6 \n\t"                                            \
      instruction " \n\t"                                                 \
      "mfhi %0, $" ac " \n\t"                                             \
      "mflo %1, $" ac " \n\t"                                             \
      "rddsp %2, 0x1 \n\t"                                                \
     : "=&r" (outHI), "=&r" (outLO), "=&r" (dspCtrl)                      \
     : "r" (HIval), "r" (LOval), "r" (pos), "r" (RSval)                   \
     : #RS                                                                \
   );                                                                     \
   printf("mthlip :: acIn: 0x%08x%08x rsIn 0x%08x posIn 0x%08x acOut "    \
          "0x%08x%08x posOut 0x%08x\n", HIval, LOval, RSval, pos, outHI,  \
          outLO, dspCtrl);                                                \
}

#define TESTDSPINST_PICK(instruction, instruction1, RSval, RTval, RD, RS, RT) \
{                                                                             \
   int out = 0xdeadbeef;                                                      \
   int dspCtrl1 = 0x0;                                                        \
   __asm__ volatile(                                                          \
      ".set dspr2; \n\t"                                                      \
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
        instruction, instruction1, RSval, RTval, out, dspCtrl1);              \
}

#define TESTDSPINST_RADDU_W_QB(instruction, RSval, RD, RS)          \
{                                                                   \
   int out = 0xdeadbeef;                                            \
   __asm__ volatile(                                                \
      ".set dspr2; \n\t"                                            \
      "move $" #RS ", %1 \n\t"                                      \
      instruction " \n\t"                                           \
      "move %0, $" #RD " \n\t"                                      \
      : "=&r" (out)                                                 \
      : "r" (RSval)                                                 \
      : #RD, #RS                                                    \
   );                                                               \
   printf("%s :: out 0x%08x rs 0x%08x\n", instruction, out, RSval); \
}

#define TESTDSPINST_RDDSPWRDSP(REGval, mask)                               \
{                                                                          \
   int out = 0xdeadbeef;                                                   \
   __asm__ volatile(                                                       \
      ".set dspr2; \n\t"                                                   \
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
      ".set dspr2; \n\t"                                          \
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
      ".set dspr2; \n\t"                                                       \
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

#define TESTDSP_SHILOV(ac, HIval, LOval, RSval, RS)                         \
{                                                                           \
   int outHI = 0xdeadbeef;                                                  \
   int outLO = 0xdeadbeef;                                                  \
   __asm__ volatile(                                                        \
      ".set dspr2; \n\t"                                                    \
      "move $" #RS ", %2 \n\t"                                              \
      "mthi $" #RS ", $" ac " \n\t"                                         \
      "move $" #RS ", %3 \n\t"                                              \
      "mtlo $t1, $" ac " \n\t"                                              \
      "move $" #RS ", %4 \n\t"                                              \
      "shilov $" ac ", $" #RS " \n\t"                                       \
      "mfhi %0, $" ac " \n\t"                                               \
      "mflo %1, $" ac " \n\t"                                               \
      : "=&r" (outHI), "=&r" (outLO)                                        \
      : "r" (HIval), "r" (LOval), "r" (RSval)                               \
      : #RS                                                                 \
   );                                                                       \
   printf("shilov %s, rs 0x%08x inAcc = 0x%08x%08x outAcc = 0x%08x%08x\n",  \
           ac, RSval, HIval, LOval, outHI, outLO);                          \
}

#define TESTDSPINST_RD_RT_SA_DSPC(instruction, RTval, SAval, RD, RT)        \
{                                                                           \
   int out = 0xdeadbeef;                                                    \
   int dspCtrl = 0x0;                                                       \
   __asm__ volatile(                                                        \
      ".set dspr2; \n\t"                                                    \
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
      ".set dspr2; \n\t"                                                 \
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

#define TESTDSPINST_RT_RS_SA_NODSPC(instruction, RSval, RTval, SAval, RT, RS) \
{                                                                             \
   int out = 0xdeadbeef;                                                      \
   __asm__ volatile(                                                          \
      ".set dspr2; \n\t"                                                      \
      "move $" #RS ", %1 \n\t"                                                \
      "move $" #RT ", %2 \n\t"                                                \
      instruction " \n\t"                                                     \
      "move %0, $" #RT " \n\t"                                                \
      : "=&r" (out)                                                           \
      : "r" (RSval), "r"(RTval)                                               \
      : #RS, #RT                                                              \
   );                                                                         \
   printf("%s :: rt 0x%08x rs 0x%08x out 0x%08x \n", instruction, RTval,      \
          RSval, out);                                                        \
}

int main(int argc, char **argv)
{
#if (__mips==32) && (__mips_isa_rev>=2)
   printf("-------- ABSQ_S.QB --------\n");
   TESTDSPINST_RD_RT_DSPC("absq_s.qb $t0, $t1", 0x00000000, t0, t1);
   TESTDSPINST_RD_RT_DSPC("absq_s.qb $t2, $t3", 0x00000286, t2, t3);
   TESTDSPINST_RD_RT_DSPC("absq_s.qb $t4, $t1", 0xfabc2435, t4, t1);
   TESTDSPINST_RD_RT_DSPC("absq_s.qb $t6, $t7", 0x734680bc, t6, t7);
   TESTDSPINST_RD_RT_DSPC("absq_s.qb $t5, $t3", 0x80000000, t5, t3);
   TESTDSPINST_RD_RT_DSPC("absq_s.qb $t2, $t4", 0xffffffff, t2, t4);
   TESTDSPINST_RD_RT_DSPC("absq_s.qb $t0, $t8", 0xfff45fff, t0, t8);
   TESTDSPINST_RD_RT_DSPC("absq_s.qb $t4, $t4", 0x00000555, t4, t4);
   TESTDSPINST_RD_RT_DSPC("absq_s.qb $t0, $t1", 0x23534870, t0, t1);
   TESTDSPINST_RD_RT_DSPC("absq_s.qb $t2, $t3", 0x0555adec, t2, t3);
   TESTDSPINST_RD_RT_DSPC("absq_s.qb $t4, $t1", 0x980b7cde, t4, t1);
   TESTDSPINST_RD_RT_DSPC("absq_s.qb $t6, $t7", 0xf973437b, t6, t7);
   TESTDSPINST_RD_RT_DSPC("absq_s.qb $t5, $t3", 0x93474bde, t5, t3);
   TESTDSPINST_RD_RT_DSPC("absq_s.qb $t2, $t4", 0x55555555, t2, t4);
   TESTDSPINST_RD_RT_DSPC("absq_s.qb $t0, $t8", 0xc4dbfe20, t0, t8);
   TESTDSPINST_RD_RT_DSPC("absq_s.qb $t4, $t4", 0x734680bc, t4, t4);
   TESTDSPINST_RD_RT_DSPC("absq_s.qb $t0, $t1", 0x00354565, t0, t1);
   TESTDSPINST_RD_RT_DSPC("absq_s.qb $t2, $t3", 0xbacabaca, t2, t3);
   TESTDSPINST_RD_RT_DSPC("absq_s.qb $t4, $t1", 0xdecadeca, t4, t1);
   TESTDSPINST_RD_RT_DSPC("absq_s.qb $t6, $t7", 0x00000286, t6, t7);
   TESTDSPINST_RD_RT_DSPC("absq_s.qb $t5, $t3", 0xabababab, t5, t3);
   TESTDSPINST_RD_RT_DSPC("absq_s.qb $t2, $t4", 0x00086755, t2, t4);
   TESTDSPINST_RD_RT_DSPC("absq_s.qb $t0, $t8", 0x8f8f8f80, t0, t8);
   TESTDSPINST_RD_RT_DSPC("absq_s.qb $t4, $t4", 0xeeeeeeee, t4, t4);
   TESTDSPINST_RD_RT_DSPC("absq_s.qb $t0, $t1", 0x1bdbdbdb, t0, t1);
   TESTDSPINST_RD_RT_DSPC("absq_s.qb $t2, $t3", 0xdecadeca, t2, t3);
   TESTDSPINST_RD_RT_DSPC("absq_s.qb $t4, $t1", 0x93474bde, t4, t1);
   TESTDSPINST_RD_RT_DSPC("absq_s.qb $t6, $t7", 0xfabfabfa, t6, t7);
   TESTDSPINST_RD_RT_DSPC("absq_s.qb $t5, $t3", 0x083b3571, t5, t3);
   TESTDSPINST_RD_RT_DSPC("absq_s.qb $t2, $t4", 0xb9743941, t2, t4);
   TESTDSPINST_RD_RT_DSPC("absq_s.qb $t0, $t8", 0xbc80f924, t0, t8);
   TESTDSPINST_RD_RT_DSPC("absq_s.qb $t4, $t4", 0xcc3c201c, t4, t4);
   TESTDSPINST_RD_RT_DSPC("absq_s.qb $t0, $t1", 0x1ebaf88e, t0, t1);
   TESTDSPINST_RD_RT_DSPC("absq_s.qb $t2, $t3", 0x722d5e20, t2, t3);
   TESTDSPINST_RD_RT_DSPC("absq_s.qb $t4, $t1", 0xa1d6f791, t4, t1);
   TESTDSPINST_RD_RT_DSPC("absq_s.qb $t6, $t7", 0x7b11bee7, t6, t7);
   TESTDSPINST_RD_RT_DSPC("absq_s.qb $t5, $t3", 0xa5631488, t5, t3);
   TESTDSPINST_RD_RT_DSPC("absq_s.qb $t2, $t4", 0xb10bcc65, t2, t4);
   TESTDSPINST_RD_RT_DSPC("absq_s.qb $t0, $t8", 0x73f39fca, t0, t8);
   TESTDSPINST_RD_RT_DSPC("absq_s.qb $t4, $t4", 0x80808080, t4, t4);

   printf("-------- ADDQH.PH --------\n");
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.ph $t0, $t1, $t2", 0x00000000, 0x00000000,
                               t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.ph $t2, $t3, $t4", 0x00045fb2, 0x00000286,
                               t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.ph $t4, $t1, $t5", 0x00002435, 0xffff3421,
                               t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.ph $t6, $t7, $t3", 0x07654cb8, 0x734680bc,
                               t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.ph $t5, $t3, $t2", 0xf973437b, 0x80000000,
                               t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.ph $t2, $t4, $t8", 0x00010001, 0xffffffff,
                               t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.ph $t0, $t8, $t0", 0x7fff7fff, 0x7fff7fff,
                               t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.ph $t4, $t6, $t1", 0x0000c420, 0x00000555,
                               t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.ph $t2, $t3, $t4", 0x00000004, 1073741824,
                               t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.ph $t4, $t1, $t5", 0x80002435, 0x80003421,
                               t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.ph $t6, $t7, $t3", 0x76548000, 0x73468000,
                               t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.ph $t5, $t3, $t2", 0x80000000, 0x80000000,
                               t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.ph $t2, $t4, $t8", 0x00010001, 0xffffffff,
                               t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.ph $t0, $t8, $t0", 0x7fff7fff, 0x7fff7fff,
                               t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.ph $t4, $t6, $t1", 0x0000c420, 0x00000555,
                               t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.ph $t0, $t1, $t2", 0x00000000, 0x00000000,
                               t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.ph $t2, $t3, $t4", 0x80000000, 0x80000000,
                               t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.ph $t4, $t1, $t5", 0xaaaaaaaa, 0x55555555,
                               t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.ph $t6, $t7, $t3", 0x00000018, 0xffff2435,
                               t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.ph $t5, $t3, $t2", 0xbabababa, 0xabababab,
                               t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.ph $t2, $t4, $t8", 0xf0f0f0f0, 0xfc79b4d2,
                               t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.ph $t0, $t8, $t0", 0xfbde3976, 0x00000000,
                               t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.ph $t4, $t6, $t1", 0x23534870, 0x00354565,
                               t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.ph $t0, $t1, $t2", 0x980b7cde, 0x00086755,
                               t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.ph $t2, $t3, $t4", 0x00000018, 0x8f8f8f8f,
                               t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.ph $t4, $t1, $t5", 0x92784656, 0xeeeeeeee,
                               t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.ph $t6, $t7, $t3", 0xcacacaca, 0x1bdbdbdb,
                               t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.ph $t5, $t3, $t2", 0xbacabaca, 0xdecadeca,
                               t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.ph $t2, $t4, $t8", 0x12fadeb4, 0x93474bde,
                               t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.ph $t0, $t8, $t0", 0x7c000790, 0xfc0007ff,
                               t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.ph $t4, $t6, $t1", 0xffffffff, 0xffffffff,
                               t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.ph $t0, $t1, $t2", 0xf2f4df1f, 0xcb4ab48f,
                               t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.ph $t2, $t3, $t4", 0x435f909a, 0xaf8f7e18,
                               t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.ph $t4, $t1, $t5", 0x2106ba5f, 0x87df4510,
                               t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.ph $t6, $t7, $t3", 0x246a6376, 0xabf4e8e1,
                               t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.ph $t5, $t3, $t2", 0x1046a1a3, 0xf4c0eeac,
                               t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.ph $t2, $t4, $t8", 0x638ca515, 0x006a54f2,
                               t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.ph $t0, $t8, $t0", 0xf63e7a9d, 0x79f74493,
                               t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.ph $t4, $t6, $t1", 0xbd6845cd, 0x9c09e313,
                               t4, t6, t1);

   printf("-------- ADDQH_R.PH --------\n");
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.ph $t0, $t1, $t2", 0x00000000,
                               0x00000000, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.ph $t2, $t3, $t4", 0x00045fb2,
                               0x00000286, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.ph $t4, $t1, $t5", 0x00002435,
                               0xffff3421, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.ph $t6, $t7, $t3", 0x07654cb8,
                               0x734680bc, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.ph $t5, $t3, $t2", 0xf973437b,
                               0x80000000, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.ph $t2, $t4, $t8", 0x00010001,
                               0xffffffff, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.ph $t0, $t8, $t0", 0x7fff7fff,
                               0x7fff7fff, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.ph $t4, $t6, $t1", 0x0000c420,
                               0x00000555, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.ph $t2, $t3, $t4", 0x00000004,
                               1073741824, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.ph $t4, $t1, $t5", 0x80002435,
                               0x80003421, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.ph $t6, $t7, $t3", 0x76548000,
                               0x73468000, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.ph $t5, $t3, $t2", 0x80000000,
                               0x80000000, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.ph $t2, $t4, $t8", 0x00010001,
                               0xffffffff, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.ph $t0, $t8, $t0", 0x7fff7fff,
                               0x7fff7fff, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.ph $t4, $t6, $t1", 0x0000c420,
                               0x00000555, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.ph $t0, $t1, $t2", 0x00000000,
                               0x00000000, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.ph $t2, $t3, $t4", 0x80000000,
                               0x80000000, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.ph $t4, $t1, $t5", 0xaaaaaaaa,
                               0x55555555, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.ph $t6, $t7, $t3", 0x00000018,
                               0xffff2435, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.ph $t5, $t3, $t2", 0xbabababa,
                               0xabababab, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.ph $t2, $t4, $t8", 0xf0f0f0f0,
                               0xfc79b4d2, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.ph $t0, $t8, $t0", 0xfbde3976,
                               0x00000000, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.ph $t4, $t6, $t1", 0x23534870,
                               0x00354565, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.ph $t0, $t1, $t2", 0x980b7cde,
                               0x00086755, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.ph $t2, $t3, $t4", 0x00000018,
                               0x8f8f8f8f, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.ph $t4, $t1, $t5", 0x92784656,
                               0xeeeeeeee, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.ph $t6, $t7, $t3", 0xcacacaca,
                               0x1bdbdbdb, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.ph $t5, $t3, $t2", 0xbacabaca,
                               0xdecadeca, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.ph $t2, $t4, $t8", 0x12fadeb4,
                               0x93474bde, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.ph $t0, $t8, $t0", 0x7c000790,
                               0xfc0007ff, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.ph $t4, $t6, $t1", 0xffffffff,
                               0xffffffff, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.ph $t0, $t1, $t2", 0xf2f4df1f,
                               0xcb4ab48f, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.ph $t2, $t3, $t4", 0x435f909a,
                               0xaf8f7e18, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.ph $t4, $t1, $t5", 0x2106ba5f,
                               0x87df4510, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.ph $t6, $t7, $t3", 0x246a6376,
                               0xabf4e8e1, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.ph $t5, $t3, $t2", 0x1046a1a3,
                               0xf4c0eeac, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.ph $t2, $t4, $t8", 0x638ca515,
                               0x006a54f2, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.ph $t0, $t8, $t0", 0xf63e7a9d,
                               0x79f74493, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.ph $t4, $t6, $t1", 0xbd6845cd,
                               0x9c09e313, t4, t6, t1);

   printf("-------- ADDQH.W --------\n");
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.w $t0, $t1, $t2", 0x00000000, 0x00000000,
                               t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.w $t2, $t3, $t4", 0x00045fb2, 0x00000286,
                               t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.w $t4, $t1, $t5", 0x00002435, 0xffff3421,
                               t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.w $t6, $t7, $t3", 0x07654cb8, 0x734680bc,
                               t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.w $t5, $t3, $t2", 0xf973437b, 0x80000000,
                               t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.w $t2, $t4, $t8", 0x00010001, 0xffffffff,
                               t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.w $t0, $t8, $t0", 0x7fff7fff, 0x7fff7fff,
                               t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.w $t4, $t6, $t1", 0x0000c420, 0x00000555,
                               t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.w $t2, $t3, $t4", 0x00000004, 1073741824,
                               t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.w $t4, $t1, $t5", 0x80002435, 0x80003421,
                               t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.w $t6, $t7, $t3", 0x76548000, 0x73468000,
                               t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.w $t5, $t3, $t2", 0x80000000, 0x80000000,
                               t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.w $t2, $t4, $t8", 0x00010001, 0xffffffff,
                               t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.w $t0, $t8, $t0", 0x7fff7fff, 0x7fff7fff,
                               t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.w $t4, $t6, $t1", 0x0000c420, 0x00000555,
                               t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.w $t0, $t1, $t2", 0x00000000, 0x00000000,
                               t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.w $t2, $t3, $t4", 0x80000000, 0x80000000,
                               t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.w $t4, $t1, $t5", 0xaaaaaaaa, 0x55555555,
                               t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.w $t6, $t7, $t3", 0x00000018, 0xffff2435,
                               t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.w $t5, $t3, $t2", 0xbabababa, 0xabababab,
                               t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.w $t2, $t4, $t8", 0xf0f0f0f0, 0xfc79b4d2,
                               t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.w $t0, $t8, $t0", 0xfbde3976, 0x00000000,
                               t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.w $t4, $t6, $t1", 0x23534870, 0x00354565,
                               t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.w $t0, $t1, $t2", 0x980b7cde, 0x00086755,
                               t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.w $t2, $t3, $t4", 0x00000018, 0x8f8f8f8f,
                               t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.w $t4, $t1, $t5", 0x92784656, 0xeeeeeeee,
                               t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.w $t6, $t7, $t3", 0xcacacaca, 0x1bdbdbdb,
                               t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.w $t5, $t3, $t2", 0xbacabaca, 0xdecadeca,
                               t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.w $t2, $t4, $t8", 0x12fadeb4, 0x93474bde,
                               t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.w $t0, $t8, $t0", 0x7c000790, 0xfc0007ff,
                               t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.w $t4, $t6, $t1", 0xffffffff, 0xffffffff,
                               t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.w $t0, $t1, $t2", 0xf2f4df1f, 0xcb4ab48f,
                               t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.w $t2, $t3, $t4", 0x435f909a, 0xaf8f7e18,
                               t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.w $t4, $t1, $t5", 0x2106ba5f, 0x87df4510,
                               t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.w $t6, $t7, $t3", 0x246a6376, 0xabf4e8e1,
                               t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.w $t5, $t3, $t2", 0x1046a1a3, 0xf4c0eeac,
                               t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.w $t2, $t4, $t8", 0x638ca515, 0x006a54f2,
                               t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.w $t0, $t8, $t0", 0xf63e7a9d, 0x79f74493,
                               t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh.w $t4, $t6, $t1", 0xbd6845cd, 0x9c09e313,
                               t4, t6, t1);

   printf("-------- ADDQH_R.W --------\n");
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.w $t0, $t1, $t2", 0x00000000,
                               0x00000000, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.w $t2, $t3, $t4", 0x00045fb2,
                               0x00000286, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.w $t4, $t1, $t5", 0x00002435,
                               0xffff3421, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.w $t6, $t7, $t3", 0x07654cb8,
                               0x734680bc, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.w $t5, $t3, $t2", 0xf973437b,
                               0x80000000, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.w $t2, $t4, $t8", 0x00010001,
                               0xffffffff, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.w $t0, $t8, $t0", 0x7fff7fff,
                               0x7fff7fff, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.w $t4, $t6, $t1", 0x0000c420,
                               0x00000555, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.w $t2, $t3, $t4", 0x00000004,
                               1073741824, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.w $t4, $t1, $t5", 0x80002435,
                               0x80003421, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.w $t6, $t7, $t3", 0x76548000,
                               0x73468000, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.w $t5, $t3, $t2", 0x80000000,
                               0x80000000, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.w $t2, $t4, $t8", 0x00010001,
                               0xffffffff, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.w $t0, $t8, $t0", 0x7fff7fff,
                               0x7fff7fff, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.w $t4, $t6, $t1", 0x0000c420,
                               0x00000555, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.w $t0, $t1, $t2", 0x00000000,
                               0x00000000, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.w $t2, $t3, $t4", 0x80000000,
                               0x80000000, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.w $t4, $t1, $t5", 0xaaaaaaaa,
                               0x55555555, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.w $t6, $t7, $t3", 0x00000018,
                               0xffff2435, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.w $t5, $t3, $t2", 0xbabababa,
                               0xabababab, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.w $t2, $t4, $t8", 0xf0f0f0f0,
                               0xfc79b4d2, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.w $t0, $t8, $t0", 0xfbde3976,
                               0x00000000, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.w $t4, $t6, $t1", 0x23534870,
                               0x00354565, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.w $t0, $t1, $t2", 0x980b7cde,
                               0x00086755, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.w $t2, $t3, $t4", 0x00000018,
                               0x8f8f8f8f, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.w $t4, $t1, $t5", 0x92784656,
                               0xeeeeeeee, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.w $t6, $t7, $t3", 0xcacacaca,
                               0x1bdbdbdb, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.w $t5, $t3, $t2", 0xbacabaca,
                               0xdecadeca, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.w $t2, $t4, $t8", 0x12fadeb4,
                               0x93474bde, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.w $t0, $t8, $t0", 0x7c000790,
                               0xfc0007ff, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.w $t4, $t6, $t1", 0xffffffff,
                               0xffffffff, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.w $t0, $t1, $t2", 0xf2f4df1f,
                               0xcb4ab48f, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.w $t2, $t3, $t4", 0x435f909a,
                               0xaf8f7e18, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.w $t4, $t1, $t5", 0x2106ba5f,
                               0x87df4510, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.w $t6, $t7, $t3", 0x246a6376,
                               0xabf4e8e1, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.w $t5, $t3, $t2", 0x1046a1a3,
                               0xf4c0eeac, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.w $t2, $t4, $t8", 0x638ca515,
                               0x006a54f2, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.w $t0, $t8, $t0", 0xf63e7a9d,
                               0x79f74493, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("addqh_r.w $t4, $t6, $t1", 0xbd6845cd,
                               0x9c09e313, t4, t6, t1);

   printf("-------- ADDU.PH --------\n");
   TESTDSPINST_RD_RS_RT_DSPC("addu.ph $t0, $t1, $t2", 0x00000000, 0x00000000,
                             t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("addu.ph $t2, $t3, $t4", 0x00045fb2, 0x00000286,
                             t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("addu.ph $t4, $t1, $t5", 0x00002435, 0xffff3421,
                             t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("addu.ph $t6, $t7, $t3", 0x07654cb8, 0x734680bc,
                             t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("addu.ph $t5, $t3, $t2", 0xf973437b, 0x80000000,
                             t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("addu.ph $t2, $t4, $t8", 0x00010001, 0xffffffff,
                             t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("addu.ph $t0, $t8, $t0", 0x7fff7fff, 0x7fff7fff,
                             t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("addu.ph $t4, $t6, $t1", 0x0000c420, 0x00000555,
                             t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("addu.ph $t2, $t3, $t4", 0x00000004, 1073741824,
                             t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("addu.ph $t4, $t1, $t5", 0x80002435, 0x80003421,
                             t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("addu.ph $t6, $t7, $t3", 0x76548000, 0x73468000,
                             t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("addu.ph $t5, $t3, $t2", 0x80000000, 0x80000000,
                             t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("addu.ph $t2, $t4, $t8", 0x00010001, 0xffffffff,
                             t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("addu.ph $t0, $t8, $t0", 0x7fff7fff, 0x7fff7fff,
                             t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("addu.ph $t4, $t6, $t1", 0x0000c420, 0x00000555,
                             t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("addu.ph $t0, $t1, $t2", 0x00000000, 0x00000000,
                             t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("addu.ph $t2, $t3, $t4", 0x80000000, 0x80000000,
                             t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("addu.ph $t4, $t1, $t5", 0xaaaaaaaa, 0x55555555,
                             t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("addu.ph $t6, $t7, $t3", 0x00000018, 0xffff2435,
                             t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("addu.ph $t5, $t3, $t2", 0xbabababa, 0xabababab,
                             t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("addu.ph $t2, $t4, $t8", 0xf0f0f0f0, 0xfc79b4d2,
                             t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("addu.ph $t0, $t8, $t0", 0xfbde3976, 0x00000000,
                             t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("addu.ph $t4, $t6, $t1", 0x23534870, 0x00354565,
                             t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("addu.ph $t0, $t1, $t2", 0x980b7cde, 0x00086755,
                             t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("addu.ph $t2, $t3, $t4", 0x00000018, 0x8f8f8f8f,
                             t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("addu.ph $t4, $t1, $t5", 0x92784656, 0xeeeeeeee,
                             t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("addu.ph $t6, $t7, $t3", 0xcacacaca, 0x1bdbdbdb,
                             t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("addu.ph $t5, $t3, $t2", 0xbacabaca, 0xdecadeca,
                             t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("addu.ph $t2, $t4, $t8", 0x12fadeb4, 0x93474bde,
                             t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("addu.ph $t0, $t8, $t0", 0x7c000790, 0xfc0007ff,
                             t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("addu.ph $t4, $t6, $t1", 0xffffffff, 0xffffffff,
                             t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("addu.ph $t0, $t1, $t2", 0xf2f4df1f, 0xcb4ab48f,
                             t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("addu.ph $t2, $t3, $t4", 0x435f909a, 0xaf8f7e18,
                             t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("addu.ph $t4, $t1, $t5", 0x2106ba5f, 0x87df4510,
                             t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("addu.ph $t6, $t7, $t3", 0x246a6376, 0xabf4e8e1,
                             t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("addu.ph $t5, $t3, $t2", 0x1046a1a3, 0xf4c0eeac,
                             t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("addu.ph $t2, $t4, $t8", 0x638ca515, 0x006a54f2,
                             t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("addu.ph $t0, $t8, $t0", 0xf63e7a9d, 0x79f74493,
                             t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("addu.ph $t4, $t6, $t1", 0xbd6845cd, 0x9c09e313,
                             t4, t6, t1);

   printf("-------- ADDU_S.PH --------\n");
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.ph $t0, $t1, $t2", 0x00000000, 0x00000000,
                             t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.ph $t2, $t3, $t4", 0x00045fb2, 0x00000286,
                             t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.ph $t4, $t1, $t5", 0x00002435, 0xffff3421,
                             t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.ph $t6, $t7, $t3", 0x07654cb8, 0x734680bc,
                             t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.ph $t5, $t3, $t2", 0xf973437b, 0x80000000,
                             t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.ph $t2, $t4, $t8", 0x00010001, 0xffffffff,
                             t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.ph $t0, $t8, $t0", 0x7fff7fff, 0x7fff7fff,
                             t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.ph $t4, $t6, $t1", 0x0000c420, 0x00000555,
                             t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.ph $t2, $t3, $t4", 0x00000004, 1073741824,
                             t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.ph $t4, $t1, $t5", 0x80002435, 0x80003421,
                             t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.ph $t6, $t7, $t3", 0x76548000, 0x73468000,
                             t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.ph $t5, $t3, $t2", 0x80000000, 0x80000000,
                             t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.ph $t2, $t4, $t8", 0x00010001, 0xffffffff,
                             t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.ph $t0, $t8, $t0", 0x7fff7fff, 0x7fff7fff,
                             t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.ph $t4, $t6, $t1", 0x0000c420, 0x00000555,
                             t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.ph $t0, $t1, $t2", 0x00000000, 0x00000000,
                             t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.ph $t2, $t3, $t4", 0x80000000, 0x80000000,
                             t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.ph $t4, $t1, $t5", 0xaaaaaaaa, 0x55555555,
                             t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.ph $t6, $t7, $t3", 0x00000018, 0xffff2435,
                             t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.ph $t5, $t3, $t2", 0xbabababa, 0xabababab,
                             t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.ph $t2, $t4, $t8", 0xf0f0f0f0, 0xfc79b4d2,
                             t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.ph $t0, $t8, $t0", 0xfbde3976, 0x00000000,
                             t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.ph $t4, $t6, $t1", 0x23534870, 0x00354565,
                             t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.ph $t0, $t1, $t2", 0x980b7cde, 0x00086755,
                             t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.ph $t2, $t3, $t4", 0x00000018, 0x8f8f8f8f,
                             t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.ph $t4, $t1, $t5", 0x92784656, 0xeeeeeeee,
                             t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.ph $t6, $t7, $t3", 0xcacacaca, 0x1bdbdbdb,
                             t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.ph $t5, $t3, $t2", 0xbacabaca, 0xdecadeca,
                             t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.ph $t2, $t4, $t8", 0x12fadeb4, 0x93474bde,
                             t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.ph $t0, $t8, $t0", 0x7c000790, 0xfc0007ff,
                             t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.ph $t4, $t6, $t1", 0xffffffff, 0xffffffff,
                             t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.ph $t0, $t1, $t2", 0xf2f4df1f, 0xcb4ab48f,
                             t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.ph $t2, $t3, $t4", 0x435f909a, 0xaf8f7e18,
                             t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.ph $t4, $t1, $t5", 0x2106ba5f, 0x87df4510,
                             t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.ph $t6, $t7, $t3", 0x246a6376, 0xabf4e8e1,
                             t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.ph $t5, $t3, $t2", 0x1046a1a3, 0xf4c0eeac,
                             t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.ph $t2, $t4, $t8", 0x638ca515, 0x006a54f2,
                             t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.ph $t0, $t8, $t0", 0xf63e7a9d, 0x79f74493,
                             t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("addu_s.ph $t4, $t6, $t1", 0xbd6845cd, 0x9c09e313,
                             t4, t6, t1);

   printf("-------- ADDUH.QB --------\n");
   TESTDSPINST_RD_RS_RT_NODSPC("adduh.qb $t0, $t1, $t2", 0x00000000, 0x00000000,
                               t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh.qb $t2, $t3, $t4", 0x00045fb2, 0x00000286,
                               t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh.qb $t4, $t1, $t5", 0x00002435, 0xffff3421,
                               t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh.qb $t6, $t7, $t3", 0x07654cb8, 0x734680bc,
                               t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh.qb $t5, $t3, $t2", 0xf973437b, 0x80000000,
                               t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh.qb $t2, $t4, $t8", 0x00010001, 0xffffffff,
                               t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh.qb $t0, $t8, $t0", 0x7fff7fff, 0x7fff7fff,
                               t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh.qb $t4, $t6, $t1", 0x0000c420, 0x00000555,
                               t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh.qb $t2, $t3, $t4", 0x00000004, 1073741824,
                               t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh.qb $t4, $t1, $t5", 0x80002435, 0x80003421,
                               t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh.qb $t6, $t7, $t3", 0x76548000, 0x73468000,
                               t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh.qb $t5, $t3, $t2", 0x80000000, 0x80000000,
                               t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh.qb $t2, $t4, $t8", 0x00010001, 0xffffffff,
                               t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh.qb $t0, $t8, $t0", 0x7fff7fff, 0x7fff7fff,
                               t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh.qb $t4, $t6, $t1", 0x0000c420, 0x00000555,
                               t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh.qb $t0, $t1, $t2", 0x00000000, 0x00000000,
                               t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh.qb $t2, $t3, $t4", 0x80000000, 0x80000000,
                               t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh.qb $t4, $t1, $t5", 0xaaaaaaaa, 0x55555555,
                               t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh.qb $t6, $t7, $t3", 0x00000018, 0xffff2435,
                               t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh.qb $t5, $t3, $t2", 0xbabababa, 0xabababab,
                               t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh.qb $t2, $t4, $t8", 0xf0f0f0f0, 0xfc79b4d2,
                               t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh.qb $t0, $t8, $t0", 0xfbde3976, 0x00000000,
                               t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh.qb $t4, $t6, $t1", 0x23534870, 0x00354565,
                               t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh.qb $t0, $t1, $t2", 0x980b7cde, 0x00086755,
                               t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh.qb $t2, $t3, $t4", 0x00000018, 0x8f8f8f8f,
                               t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh.qb $t4, $t1, $t5", 0x92784656, 0xeeeeeeee,
                               t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh.qb $t6, $t7, $t3", 0xcacacaca, 0x1bdbdbdb,
                               t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh.qb $t5, $t3, $t2", 0xbacabaca, 0xdecadeca,
                               t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh.qb $t2, $t4, $t8", 0x12fadeb4, 0x93474bde,
                               t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh.qb $t0, $t8, $t0", 0x7c000790, 0xfc0007ff,
                               t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh.qb $t4, $t6, $t1", 0xffffffff, 0xffffffff,
                               t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh.qb $t0, $t1, $t2", 0xf2f4df1f, 0xcb4ab48f,
                               t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh.qb $t2, $t3, $t4", 0x435f909a, 0xaf8f7e18,
                               t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh.qb $t4, $t1, $t5", 0x2106ba5f, 0x87df4510,
                               t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh.qb $t6, $t7, $t3", 0x246a6376, 0xabf4e8e1,
                               t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh.qb $t5, $t3, $t2", 0x1046a1a3, 0xf4c0eeac,
                               t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh.qb $t2, $t4, $t8", 0x638ca515, 0x006a54f2,
                               t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh.qb $t0, $t8, $t0", 0xf63e7a9d, 0x79f74493,
                               t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh.qb $t4, $t6, $t1", 0xbd6845cd, 0x9c09e313,
                               t4, t6, t1);

   printf("-------- ADDUH_R.QB --------\n");
   TESTDSPINST_RD_RS_RT_NODSPC("adduh_r.qb $t0, $t1, $t2", 0x00000000,
                               0x00000000, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh_r.qb $t2, $t3, $t4", 0x00045fb2,
                               0x00000286, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh_r.qb $t4, $t1, $t5", 0x00002435,
                               0xffff3421, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh_r.qb $t6, $t7, $t3", 0x07654cb8,
                               0x734680bc, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh_r.qb $t5, $t3, $t2", 0xf973437b,
                               0x80000000, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh_r.qb $t2, $t4, $t8", 0x00010001,
                               0xffffffff, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh_r.qb $t0, $t8, $t0", 0x7fff7fff,
                               0x7fff7fff, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh_r.qb $t4, $t6, $t1", 0x0000c420,
                               0x00000555, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh_r.qb $t2, $t3, $t4", 0x00000004,
                               1073741824, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh_r.qb $t4, $t1, $t5", 0x80002435,
                               0x80003421, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh_r.qb $t6, $t7, $t3", 0x76548000,
                               0x73468000, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh_r.qb $t5, $t3, $t2", 0x80000000,
                               0x80000000, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh_r.qb $t2, $t4, $t8", 0x00010001,
                               0xffffffff, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh_r.qb $t0, $t8, $t0", 0x7fff7fff,
                               0x7fff7fff, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh_r.qb $t4, $t6, $t1", 0x0000c420,
                               0x00000555, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh_r.qb $t0, $t1, $t2", 0x00000000,
                               0x00000000, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh_r.qb $t2, $t3, $t4", 0x80000000,
                               0x80000000, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh_r.qb $t4, $t1, $t5", 0xaaaaaaaa,
                               0x55555555, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh_r.qb $t6, $t7, $t3", 0x00000018,
                               0xffff2435, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh_r.qb $t5, $t3, $t2", 0xbabababa,
                               0xabababab, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh_r.qb $t2, $t4, $t8", 0xf0f0f0f0,
                               0xfc79b4d2, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh_r.qb $t0, $t8, $t0", 0xfbde3976,
                               0x00000000, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh_r.qb $t4, $t6, $t1", 0x23534870,
                               0x00354565, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh_r.qb $t0, $t1, $t2", 0x980b7cde,
                               0x00086755, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh_r.qb $t2, $t3, $t4", 0x00000018,
                               0x8f8f8f8f, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh_r.qb $t4, $t1, $t5", 0x92784656,
                               0xeeeeeeee, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh_r.qb $t6, $t7, $t3", 0xcacacaca,
                               0x1bdbdbdb, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh_r.qb $t5, $t3, $t2", 0xbacabaca,
                               0xdecadeca, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh_r.qb $t2, $t4, $t8", 0x12fadeb4,
                               0x93474bde, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh_r.qb $t0, $t8, $t0", 0x7c000790,
                               0xfc0007ff, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh_r.qb $t4, $t6, $t1", 0xffffffff,
                               0xffffffff, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh_r.qb $t0, $t1, $t2", 0xf2f4df1f,
                               0xcb4ab48f, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh_r.qb $t2, $t3, $t4", 0x435f909a,
                               0xaf8f7e18, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh_r.qb $t4, $t1, $t5", 0x2106ba5f,
                               0x87df4510, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh_r.qb $t6, $t7, $t3", 0x246a6376,
                               0xabf4e8e1, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh_r.qb $t5, $t3, $t2", 0x1046a1a3,
                               0xf4c0eeac, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh_r.qb $t2, $t4, $t8", 0x638ca515,
                               0x006a54f2, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh_r.qb $t0, $t8, $t0", 0xf63e7a9d,
                               0x79f74493, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("adduh_r.qb $t4, $t6, $t1", 0xbd6845cd,
                               0x9c09e313, t4, t6, t1);

   printf("-------- APPEND --------\n");
   TESTDSPINST_RT_RS_SA_NODSPC("append $t0, $t1,  0", 0x00000000, 0x0fffffff,
                               0, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("append $t2, $t3,  1", 0x00045fb2, 0x00000286,
                               1, t2, t3);
   TESTDSPINST_RT_RS_SA_NODSPC("append $t4, $t1,  4", 0xfabc2435, 0x0ffb3421,
                               4, t4, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("append $t6, $t7, 17", 0x07654cb8, 0x734680bc,
                               17, t6, t7);
   TESTDSPINST_RT_RS_SA_NODSPC("append $t5, $t3, 31", 0xf973437b, 0x80000000,
                               31, t5, t3);
   TESTDSPINST_RT_RS_SA_NODSPC("append $t2, $t4,  8", 0x00010001, 0xffffffff,
                               8, t2, t4);
   TESTDSPINST_RT_RS_SA_NODSPC("append $t0, $t8, 11", 0x7fff7fff, 0x7fff7fff,
                               11, t0, t8);
   TESTDSPINST_RT_RS_SA_NODSPC("append $t4, $t6, 13", 0x0000c420, 0x00000555,
                               13, t4, t6);
   TESTDSPINST_RT_RS_SA_NODSPC("append $t0, $t1,  2", 0x00000000, 0x00000000,
                               2, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("append $t2, $t3,  6", 0x80000000, 0x80000000,
                               6, t2, t3);
   TESTDSPINST_RT_RS_SA_NODSPC("append $t4, $t1,  7", 0xaaaaaaaa, 0x55555555,
                               7, t4, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("append $t6, $t7, 19", 0x00000018, 0xffff2435,
                               19, t6, t7);
   TESTDSPINST_RT_RS_SA_NODSPC("append $t5, $t3, 31", 0xbabababa, 0xabababab,
                               31, t5, t3);
   TESTDSPINST_RT_RS_SA_NODSPC("append $t2, $t4,  4", 0xf0f0f0f0, 0xfc79b4d2,
                               4, t2, t4);
   TESTDSPINST_RT_RS_SA_NODSPC("append $t0, $t8, 12", 0xfbde3976, 0x00000000,
                               12, t0, t8);
   TESTDSPINST_RT_RS_SA_NODSPC("append $t4, $t6, 10", 0x23534870, 0x00354565,
                               10, t4, t6);
   TESTDSPINST_RT_RS_SA_NODSPC("append $t0, $t1, 20", 0x980b7cde, 0x00086755,
                               20, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("append $t2, $t3, 21", 0x00000018, 0x8f8f8f8f,
                               21, t2, t3);
   TESTDSPINST_RT_RS_SA_NODSPC("append $t4, $t1, 24", 0x92784656, 0xeeeeeeee,
                               24, t4, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("append $t6, $t7, 27", 0xcacacaca, 0x1bdbdbdb,
                               27, t6, t7);
   TESTDSPINST_RT_RS_SA_NODSPC("append $t5, $t3,  1", 0xbacabaca, 0xdecadeca,
                               1, t5, t3);
   TESTDSPINST_RT_RS_SA_NODSPC("append $t2, $t4, 18", 0x12fadeb4, 0x93474bde,
                               18, t2, t4);
   TESTDSPINST_RT_RS_SA_NODSPC("append $t0, $t8, 10", 0x7c000790, 0xfc0007ff,
                               10, t0, t8);
   TESTDSPINST_RT_RS_SA_NODSPC("append $t4, $t6, 16", 0xffffffff, 0xffffffff,
                               16, t4, t6);
   TESTDSPINST_RT_RS_SA_NODSPC("append $t0, $t1,  0", 0xf2f4df1f, 0xcb4ab48f,
                               0, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("append $t2, $t3, 14", 0x435f909a, 0xaf8f7e18,
                               14, t2, t3);
   TESTDSPINST_RT_RS_SA_NODSPC("append $t4, $t1,  5", 0x2106ba5f, 0x87df4510,
                               5, t4, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("append $t6, $t7,  7", 0x246a6376, 0xabf4e8e1,
                               7, t6, t7);
   TESTDSPINST_RT_RS_SA_NODSPC("append $t5, $t3,  9", 0x1046a1a3, 0xf4c0eeac,
                               9, t5, t3);
   TESTDSPINST_RT_RS_SA_NODSPC("append $t2, $t4,  3", 0x638ca515, 0x006a54f2,
                               3, t2, t4);
   TESTDSPINST_RT_RS_SA_NODSPC("append $t0, $t8, 15", 0xf63e7a9d, 0x79f74493,
                               15, t0, t8);
   TESTDSPINST_RT_RS_SA_NODSPC("append $t4, $t6, 11", 0xbd6845cd, 0x9c09e313,
                               1, t4, t6);

   printf("-------- BALIGN --------\n");
   TESTDSPINST_RT_RS_SA_NODSPC("balign $t0, $t1,  0", 0x00000000, 0x0fffffff, 0,
                               t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("balign $t2, $t3,  1", 0x00045fb2, 0x00000286, 1,
                               t2, t3);
   TESTDSPINST_RT_RS_SA_NODSPC("balign $t6, $t7,  3", 0x07654cb8, 0x734680bc, 3,
                               t6, t7);
   TESTDSPINST_RT_RS_SA_NODSPC("balign $t5, $t3,  0", 0xf973437b, 0x80000000, 0,
                               t5, t3);
   TESTDSPINST_RT_RS_SA_NODSPC("balign $t2, $t4,  1", 0x00010001, 0xffffffff, 1,
                               t2, t4);
   TESTDSPINST_RT_RS_SA_NODSPC("balign $t4, $t6,  3", 0x0000c420, 0x00000555, 3,
                               t4, t6);
   TESTDSPINST_RT_RS_SA_NODSPC("balign $t0, $t1,  0", 0x00000000, 0x00000000, 0,
                               t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("balign $t2, $t3,  1", 0x80000000, 0x80000000, 1,
                               t2, t3);
   TESTDSPINST_RT_RS_SA_NODSPC("balign $t6, $t7,  3", 0x00000018, 0xffff2435, 3,
                               t6, t7);
   TESTDSPINST_RT_RS_SA_NODSPC("balign $t5, $t3,  0", 0xbabababa, 0xabababab, 0,
                               t5, t3);
   TESTDSPINST_RT_RS_SA_NODSPC("balign $t2, $t4,  1", 0xf0f0f0f0, 0xfc79b4d2, 1,
                               t2, t4);
   TESTDSPINST_RT_RS_SA_NODSPC("balign $t4, $t6,  3", 0x23534870, 0x00354565, 3,
                               t4, t6);
   TESTDSPINST_RT_RS_SA_NODSPC("balign $t0, $t1,  0", 0x980b7cde, 0x00086755, 0,
                               t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("balign $t2, $t3,  1", 0x00000018, 0x8f8f8f8f, 1,
                               t2, t3);
   TESTDSPINST_RT_RS_SA_NODSPC("balign $t6, $t7,  3", 0xcacacaca, 0x1bdbdbdb, 3,
                               t6, t7);
   TESTDSPINST_RT_RS_SA_NODSPC("balign $t5, $t3,  0", 0xbacabaca, 0xdecadeca, 0,
                               t5, t3);
   TESTDSPINST_RT_RS_SA_NODSPC("balign $t2, $t4,  1", 0x12fadeb4, 0x93474bde, 1,
                               t2, t4);
   TESTDSPINST_RT_RS_SA_NODSPC("balign $t4, $t6,  3", 0xffffffff, 0xffffffff, 3,
                               t4, t6);
   TESTDSPINST_RT_RS_SA_NODSPC("balign $t0, $t1,  0", 0xf2f4df1f, 0xcb4ab48f, 0,
                               t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("balign $t2, $t3,  1", 0x435f909a, 0xaf8f7e18, 1,
                               t2, t3);
   TESTDSPINST_RT_RS_SA_NODSPC("balign $t6, $t7,  3", 0x246a6376, 0xabf4e8e1, 3,
                               t6, t7);
   TESTDSPINST_RT_RS_SA_NODSPC("balign $t5, $t3,  0", 0x1046a1a3, 0xf4c0eeac, 0,
                               t5, t3);
   TESTDSPINST_RT_RS_SA_NODSPC("balign $t2, $t4,  1", 0x638ca515, 0x006a54f2, 1,
                               t2, t4);
   TESTDSPINST_RT_RS_SA_NODSPC("balign $t4, $t6,  3", 0xbd6845cd, 0x9c09e313, 3,
                               t4, t6);

   printf("-------- CMPGDU.EQ.QB --------\n");
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.eq.qb $t0, $t1, $t2", 0x00000000,
                             0x00000000, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.eq.qb $t2, $t3, $t4", 0x00045fb2,
                             0x00000286, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.eq.qb $t4, $t1, $t5", 0x00002435,
                             0xffff3421, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.eq.qb $t6, $t7, $t3", 0x07654cb8,
                             0x734680bc, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.eq.qb $t5, $t3, $t2", 0xf973437b,
                             0x80000000, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.eq.qb $t2, $t4, $t8", 0x00010001,
                             0xffffffff, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.eq.qb $t0, $t8, $t0", 0x7fff7fff,
                             0x7fff7fff, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.eq.qb $t4, $t6, $t1", 0x0000c420,
                             0x00000555, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.eq.qb $t2, $t3, $t4", 0x00000004,
                             1073741824, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.eq.qb $t4, $t1, $t5", 0x80002435,
                             0x80003421, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.eq.qb $t6, $t7, $t3", 0x76548000,
                             0x73468000, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.eq.qb $t5, $t3, $t2", 0x80000000,
                             0x80000000, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.eq.qb $t2, $t4, $t8", 0x00010001,
                             0xffffffff, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.eq.qb $t0, $t8, $t0", 0x7fff7fff,
                             0x7fff7fff, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.eq.qb $t4, $t6, $t1", 0x0000c420,
                             0x00000555, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.eq.qb $t0, $t1, $t2", 0x00000000,
                             0x00000000, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.eq.qb $t2, $t3, $t4", 0x80000000,
                             0x80000000, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.eq.qb $t4, $t1, $t5", 0xaaaaaaaa,
                             0x55555555, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.eq.qb $t6, $t7, $t3", 0x00000018,
                             0xffff2435, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.eq.qb $t5, $t3, $t2", 0xbabababa,
                             0xabababab, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.eq.qb $t2, $t4, $t8", 0xf0f0f0f0,
                             0xfc79b4d2, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.eq.qb $t0, $t8, $t0", 0xfbde3976,
                             0x00000000, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.eq.qb $t4, $t6, $t1", 0x23534870,
                             0x00354565, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.eq.qb $t0, $t1, $t2", 0x980b7cde,
                             0x00086755, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.eq.qb $t2, $t3, $t4", 0x00000018,
                             0x8f8f8f8f, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.eq.qb $t4, $t1, $t5", 0x92784656,
                             0xeeeeeeee, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.eq.qb $t6, $t7, $t3", 0xcacacaca,
                             0x1bdbdbdb, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.eq.qb $t5, $t3, $t2", 0xbacabaca,
                             0xdecadeca, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.eq.qb $t2, $t4, $t8", 0x12fadeb4,
                             0x93474bde, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.eq.qb $t0, $t8, $t0", 0x7c000790,
                             0xfc0007ff, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.eq.qb $t4, $t6, $t1", 0xffffffff,
                             0xffffffff, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.eq.qb $t0, $t1, $t2", 0xf2f4df1f,
                             0xcb4ab48f, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.eq.qb $t2, $t3, $t4", 0x435f909a,
                             0xaf8f7e18, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.eq.qb $t4, $t1, $t5", 0x2106ba5f,
                             0x87df4510, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.eq.qb $t6, $t7, $t3", 0x246a6376,
                             0xabf4e8e1, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.eq.qb $t5, $t3, $t2", 0x1046a1a3,
                             0xf4c0eeac, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.eq.qb $t2, $t4, $t8", 0x638ca515,
                             0x006a54f2, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.eq.qb $t0, $t8, $t0", 0xf63e7a9d,
                             0x79f74493, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.eq.qb $t4, $t6, $t1", 0xbd6845cd,
                             0x9c09e313, t4, t6, t1);

   printf("-------- CMPGDU.LT.QB --------\n");
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.lt.qb $t0, $t1, $t2", 0x00000000,
                             0x00000000, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.lt.qb $t2, $t3, $t4", 0x00045fb2,
                             0x00000286, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.lt.qb $t4, $t1, $t5", 0x00002435,
                             0xffff3421, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.lt.qb $t6, $t7, $t3", 0x07654cb8,
                             0x734680bc, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.lt.qb $t5, $t3, $t2", 0xf973437b,
                             0x80000000, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.lt.qb $t2, $t4, $t8", 0x00010001,
                             0xffffffff, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.lt.qb $t0, $t8, $t0", 0x7fff7fff,
                             0x7fff7fff, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.lt.qb $t4, $t6, $t1", 0x0000c420,
                             0x00000555, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.lt.qb $t2, $t3, $t4", 0x00000004,
                             1073741824, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.lt.qb $t4, $t1, $t5", 0x80002435,
                             0x80003421, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.lt.qb $t6, $t7, $t3", 0x76548000,
                             0x73468000, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.lt.qb $t5, $t3, $t2", 0x80000000,
                             0x80000000, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.lt.qb $t2, $t4, $t8", 0x00010001,
                             0xffffffff, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.lt.qb $t0, $t8, $t0", 0x7fff7fff,
                             0x7fff7fff, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.lt.qb $t4, $t6, $t1", 0x0000c420,
                             0x00000555, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.lt.qb $t0, $t1, $t2", 0x00000000,
                             0x00000000, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.lt.qb $t2, $t3, $t4", 0x80000000,
                             0x80000000, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.lt.qb $t4, $t1, $t5", 0xaaaaaaaa,
                             0x55555555, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.lt.qb $t6, $t7, $t3", 0x00000018,
                             0xffff2435, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.lt.qb $t5, $t3, $t2", 0xbabababa,
                             0xabababab, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.lt.qb $t2, $t4, $t8", 0xf0f0f0f0,
                             0xfc79b4d2, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.lt.qb $t0, $t8, $t0", 0xfbde3976,
                             0x00000000, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.lt.qb $t4, $t6, $t1", 0x23534870,
                             0x00354565, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.lt.qb $t0, $t1, $t2", 0x980b7cde,
                             0x00086755, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.lt.qb $t2, $t3, $t4", 0x00000018,
                             0x8f8f8f8f, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.lt.qb $t4, $t1, $t5", 0x92784656,
                             0xeeeeeeee, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.lt.qb $t6, $t7, $t3", 0xcacacaca,
                             0x1bdbdbdb, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.lt.qb $t5, $t3, $t2", 0xbacabaca,
                             0xdecadeca, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.lt.qb $t2, $t4, $t8", 0x12fadeb4,
                             0x93474bde, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.lt.qb $t0, $t8, $t0", 0x7c000790,
                             0xfc0007ff, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.lt.qb $t4, $t6, $t1", 0xffffffff,
                             0xffffffff, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.lt.qb $t0, $t1, $t2", 0xf2f4df1f,
                             0xcb4ab48f, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.lt.qb $t2, $t3, $t4", 0x435f909a,
                             0xaf8f7e18, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.lt.qb $t4, $t1, $t5", 0x2106ba5f,
                             0x87df4510, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.lt.qb $t6, $t7, $t3", 0x246a6376,
                             0xabf4e8e1, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.lt.qb $t5, $t3, $t2", 0x1046a1a3,
                             0xf4c0eeac, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.lt.qb $t2, $t4, $t8", 0x638ca515,
                             0x006a54f2, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.lt.qb $t0, $t8, $t0", 0xf63e7a9d,
                             0x79f74493, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.lt.qb $t4, $t6, $t1", 0xbd6845cd,
                             0x9c09e313, t4, t6, t1);

   printf("-------- CMPGDU.LE.QB --------\n");
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.le.qb $t0, $t1, $t2", 0x00000000,
                             0x00000000, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.le.qb $t2, $t3, $t4", 0x00045fb2,
                             0x00000286, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.le.qb $t4, $t1, $t5", 0x00002435,
                             0xffff3421, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.le.qb $t6, $t7, $t3", 0x07654cb8,
                             0x734680bc, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.le.qb $t5, $t3, $t2", 0xf973437b,
                             0x80000000, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.le.qb $t2, $t4, $t8", 0x00010001,
                             0xffffffff, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.le.qb $t0, $t8, $t0", 0x7fff7fff,
                             0x7fff7fff, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.le.qb $t4, $t6, $t1", 0x0000c420,
                             0x00000555, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.le.qb $t2, $t3, $t4", 0x00000004,
                             1073741824, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.le.qb $t4, $t1, $t5", 0x80002435,
                             0x80003421, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.le.qb $t6, $t7, $t3", 0x76548000,
                             0x73468000, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.le.qb $t5, $t3, $t2", 0x80000000,
                             0x80000000, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.le.qb $t2, $t4, $t8", 0x00010001,
                             0xffffffff, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.le.qb $t0, $t8, $t0", 0x7fff7fff,
                             0x7fff7fff, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.le.qb $t4, $t6, $t1", 0x0000c420,
                             0x00000555, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.le.qb $t0, $t1, $t2", 0x00000000,
                             0x00000000, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.le.qb $t2, $t3, $t4", 0x80000000,
                             0x80000000, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.le.qb $t4, $t1, $t5", 0xaaaaaaaa,
                             0x55555555, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.le.qb $t6, $t7, $t3", 0x00000018,
                             0xffff2435, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.le.qb $t5, $t3, $t2", 0xbabababa,
                             0xabababab, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.le.qb $t2, $t4, $t8", 0xf0f0f0f0,
                             0xfc79b4d2, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.le.qb $t0, $t8, $t0", 0xfbde3976,
                             0x00000000, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.le.qb $t4, $t6, $t1", 0x23534870,
                             0x00354565, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.le.qb $t0, $t1, $t2", 0x980b7cde,
                             0x00086755, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.le.qb $t2, $t3, $t4", 0x00000018,
                             0x8f8f8f8f, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.le.qb $t4, $t1, $t5", 0x92784656,
                             0xeeeeeeee, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.le.qb $t6, $t7, $t3", 0xcacacaca,
                             0x1bdbdbdb, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.le.qb $t5, $t3, $t2", 0xbacabaca,
                             0xdecadeca, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.le.qb $t2, $t4, $t8", 0x12fadeb4,
                             0x93474bde, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.le.qb $t0, $t8, $t0", 0x7c000790,
                             0xfc0007ff, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.le.qb $t4, $t6, $t1", 0xffffffff,
                             0xffffffff, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.le.qb $t0, $t1, $t2", 0xf2f4df1f,
                             0xcb4ab48f, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.le.qb $t2, $t3, $t4", 0x435f909a,
                             0xaf8f7e18, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.le.qb $t4, $t1, $t5", 0x2106ba5f,
                             0x87df4510, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.le.qb $t6, $t7, $t3", 0x246a6376,
                             0xabf4e8e1, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.le.qb $t5, $t3, $t2", 0x1046a1a3,
                             0xf4c0eeac, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.le.qb $t2, $t4, $t8", 0x638ca515,
                             0x006a54f2, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.le.qb $t0, $t8, $t0", 0xf63e7a9d,
                             0x79f74493, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("cmpgdu.le.qb $t4, $t6, $t1", 0xbd6845cd,
                             0x9c09e313, t4, t6, t1);

   printf("-------- DPA.W.PH --------\n");
   TESTDSPINST_AC_RS_RT_NODSPC("dpa.w.ph", 0xffffffff, 0x00000000, 0x3277ffff,
                               0x4387ffff);
   TESTDSPINST_AC_RS_RT_NODSPC("dpa.w.ph", 0x00000000, 0x00000000, 0x00000004,
                               0x00000005);
   TESTDSPINST_AC_RS_RT_NODSPC("dpa.w.ph", 0xffffffff, 0xffffffff, 0x80000000,
                               0x80000000);
   TESTDSPINST_AC_RS_RT_NODSPC("dpa.w.ph", 0xeeeeffff, 0x00002345, 0x3277ffff,
                               0x4387ffff);
   TESTDSPINST_AC_RS_RT_NODSPC("dpa.w.ph", 0xffffaaaa, 0x12340000, 0x3277ffff,
                               0x4387ffff);
   TESTDSPINST_AC_RS_RT_NODSPC("dpa.w.ph", 0x00000000, 0x00000000, 0x80000000,
                               0x80000000);
   TESTDSPINST_AC_RS_RT_NODSPC("dpa.w.ph", 0xffffffff, 0x80008000, 0x80000000,
                               0x80000000);
   TESTDSPINST_AC_RS_RT_NODSPC("dpa.w.ph", 0x00000000, 0x00000000, 0x80000004,
                               0x00000005);
   TESTDSPINST_AC_RS_RT_NODSPC("dpa.w.ph", 0xffffffff, 0xffffffff, 0x80008000,
                               0x4387ffff);
   TESTDSPINST_AC_RS_RT_NODSPC("dpa.w.ph", 0xeeeeffff, 0x00002345, 0x80008000,
                               0x80008000);
   TESTDSPINST_AC_RS_RT_NODSPC("dpa.w.ph", 0xffffaaaa, 0x12340000, 0x3277ffff,
                               0x4387ffff);
   TESTDSPINST_AC_RS_RT_NODSPC("dpa.w.ph", 0x00000000, 0x00000000, 0x80008000,
                               0x80008000);
   TESTDSPINST_AC_RS_RT_NODSPC("dpa.w.ph", 0xffffffff, 0x00000000, 0x3277ffff,
                               0x4387ffff);
   TESTDSPINST_AC_RS_RT_NODSPC("dpa.w.ph", 0x80000000, 0xffff0000, 0x3277eeee,
                               0x80008000);
   TESTDSPINST_AC_RS_RT_NODSPC("dpa.w.ph", 0xffffffff, 0x00000000, 0x80000000,
                               0x80000000);
   TESTDSPINST_AC_RS_RT_NODSPC("dpa.w.ph", 0x0000ffff, 0xffff0000, 0x3277eeee,
                               0x4387ffff);
   TESTDSPINST_AC_RS_RT_NODSPC("dpa.w.ph", 0xffffffff, 0x0000ffff, 0x80008000,
                               0x4387ffff);
   TESTDSPINST_AC_RS_RT_NODSPC("dpa.w.ph", 0xffff1234, 0x00000000, 0xffff3277,
                               0x4387ffff);
   TESTDSPINST_AC_RS_RT_NODSPC("dpa.w.ph", 0x5678ffff, 0x3277ffff, 0x80000000,
                               0x80000000);
   TESTDSPINST_AC_RS_RT_NODSPC("dpa.w.ph", 0xffffffff, 0x00000000, 0xffff3277,
                               0xffff6543);
   TESTDSPINST_AC_RS_RT_NODSPC("dpa.w.ph", 0xffffffff, 0xffffffff, 0xffffffff,
                               0xffffffff);
   TESTDSPINST_AC_RS_RT_NODSPC("dpa.w.ph", 0xffffbbbb, 0xeeee0000, 0x80008000,
                               0x4387ffff);
   TESTDSPINST_AC_RS_RT_NODSPC("dpa.w.ph", 0xffffffff, 0x34560000, 0x3277ffff,
                               0x4387cccc);
   TESTDSPINST_AC_RS_RT_NODSPC("dpa.w.ph", 0xbbbbffff, 0x0000ffff, 0xeeeeffff,
                               0x4387ffff);
   TESTDSPINST_AC_RS_RT_NODSPC("dpa.w.ph", 0x12345678, 0xffffffff, 0xffffffff,
                               0xffffffff);
   TESTDSPINST_AC_RS_RT_NODSPC("dpa.w.ph", 0xffffbbbb, 0x12345678, 0x80008000,
                               0x4387ffff);
   TESTDSPINST_AC_RS_RT_NODSPC("dpa.w.ph", 0xffffffff, 0x34560000, 0x87654321,
                               0x80008000);
   TESTDSPINST_AC_RS_RT_NODSPC("dpa.w.ph", 0xbbbbffff, 0x0000ffff, 0xeeeeffff,
                               0x80000000);
   TESTDSPINST_AC_RS_RT_NODSPC("dpa.w.ph", 0xffffaaaa, 0x12340000, 0x3277ffff,
                               0x4387ffff);
   TESTDSPINST_AC_RS_RT_NODSPC("dpa.w.ph", 0x80000000, 0x00000000, 0x80008000,
                               0x80008000);
   TESTDSPINST_AC_RS_RT_NODSPC("dpa.w.ph", 0xffffffff, 0x80008000, 0x3277ffff,
                               0x4387ffff);
   TESTDSPINST_AC_RS_RT_NODSPC("dpa.w.ph", 0x0000ffff, 0xffff0000, 0x3277eeee,
                               0x4387ffff);
   TESTDSPINST_AC_RS_RT_NODSPC("dpa.w.ph", -24, 120, -24, 120);
   TESTDSPINST_AC_RS_RT_NODSPC("dpa.w.ph", 140, 120, 140, 120);
   TESTDSPINST_AC_RS_RT_NODSPC("dpa.w.ph", 0x00000004, 1073741824, 0x00000004,
                               1073741824);
   TESTDSPINST_AC_RS_RT_NODSPC("dpa.w.ph", (1<<31)+1, (1<<31)+2, (1<<31)+1,
                               (1<<31)+2);
   TESTDSPINST_AC_RS_RT_NODSPC("dpa.w.ph", 0x80002431, 0x4b908000, 0x80002431,
                               0x4b908000);
   TESTDSPINST_AC_RS_RT_NODSPC("dpa.w.ph", 0x004d8000, 0x800027cc, 0x004d8000,
                               0x800027cc);
   TESTDSPINST_AC_RS_RT_NODSPC("dpa.w.ph", 0xf6a3fa3c, 0x083b3571, 0xf6a3fa3c,
                               0x083b3571);
   TESTDSPINST_AC_RS_RT_NODSPC("dpa.w.ph", 0xbf17fb9a, 0xb9743941, 0xbf17fb9a,
                               0xb9743941);
   TESTDSPINST_AC_RS_RT_NODSPC("dpa.w.ph", 0x2c0bd024, 0xbce5f924, 0x2c0bd024,
                               0xbce5f924);
   TESTDSPINST_AC_RS_RT_NODSPC("dpa.w.ph", 0x3e976e2e, 0xcc3c201c, 0x3e976e2e,
                               0xcc3c201c);
   TESTDSPINST_AC_RS_RT_NODSPC("dpa.w.ph", 0xb4bfb365, 0x1ebaf88e, 0xb4bfb365,
                               0x1ebaf88e);
   TESTDSPINST_AC_RS_RT_NODSPC("dpa.w.ph", 0x288593c0, 0x722d5e20, 0x288593c0,
                               0x722d5e20);
   TESTDSPINST_AC_RS_RT_NODSPC("dpa.w.ph", 0x4d7ff5b4, 0xa1d6f791, 0x4d7ff5b4,
                               0xa1d6f791);
   TESTDSPINST_AC_RS_RT_NODSPC("dpa.w.ph", 0x4557be13, 0x7b11bee7, 0x4557be13,
                               0x7b11bee7);
   TESTDSPINST_AC_RS_RT_NODSPC("dpa.w.ph", 0xadcf5772, 0xa5631488, 0xadcf5772,
                               0xa5631488);
   TESTDSPINST_AC_RS_RT_NODSPC("dpa.w.ph", 0x989a7235, 0xb10bcc65, 0x989a7235,
                               0xb10bcc65);
   TESTDSPINST_AC_RS_RT_NODSPC("dpa.w.ph", 0x4d6f393a, 0x73f39fca, 0x4d6f393a,
                               0x73f39fca);
   TESTDSPINST_AC_RS_RT_NODSPC("dpa.w.ph", 0x24a3291e, 0x5648e540, 0x24a3291e,
                               0x5648e540);
   TESTDSPINST_AC_RS_RT_NODSPC("dpa.w.ph", 0xdd91eebf, 0xc54f79e6, 0xdd91eebf,
                               0xc54f79e6);
   TESTDSPINST_AC_RS_RT_NODSPC("dpa.w.ph", 0xf7ce2ec6, 0x5fc92974, 0xf7ce2ec6,
                               0x5fc92974);
   TESTDSPINST_AC_RS_RT_NODSPC("dpa.w.ph", 0xbc1083e8, 0x7e08184e, 0xbc1083e8,
                               0x7e08184e);
   TESTDSPINST_AC_RS_RT_NODSPC("dpa.w.ph", 0xa617cc31, 0x71c8315f, 0xa617cc31,
                               0x71c8315f);
   TESTDSPINST_AC_RS_RT_NODSPC("dpa.w.ph", 0xdfe1e8f0, 0x9493110e, 0xdfe1e8f0,
                               0x9493110e);
   TESTDSPINST_AC_RS_RT_NODSPC("dpa.w.ph", 0x31458a23, 0xbb246228, 0x31458a23,
                               0xbb246228);
   TESTDSPINST_AC_RS_RT_NODSPC("dpa.w.ph", 0x848af791, 0x339d8d88, 0x848af791,
                               0x339d8d88);
   TESTDSPINST_AC_RS_RT_NODSPC("dpa.w.ph", 0xda3bacdc, 0x70974249, 0xda3bacdc,
                               0x70974249);
   TESTDSPINST_AC_RS_RT_NODSPC("dpa.w.ph", 0x649d5cbd, 0x8a8d4e7d, 0x649d5cbd,
                               0x8a8d4e7d);
   TESTDSPINST_AC_RS_RT_NODSPC("dpa.w.ph", 0xc0c8c881, 0xeb1b4335, 0xc0c8c881,
                               0xeb1b4335);
   TESTDSPINST_AC_RS_RT_NODSPC("dpa.w.ph", 0x7dd81a20, 0x0cd6b508, 0x7dd81a20,
                               0x0cd6b508);
   TESTDSPINST_AC_RS_RT_NODSPC("dpa.w.ph", 0x00000000, 0x6731e282, 0x00000000,
                               0x6731e282);
   TESTDSPINST_AC_RS_RT_NODSPC("dpa.w.ph", 0xffffffff, 0xb6edf28f, 0xffffffff,
                               0xb6edf28f);
   TESTDSPINST_AC_RS_RT_NODSPC("dpa.w.ph", 0x00000000, 0x4b4ec9ca, 0x00000000,
                               0x4b4ec9ca);
   TESTDSPINST_AC_RS_RT_NODSPC("dpa.w.ph", 0xffffffff, 0xc1037fa4, 0xffffffff,
                               0xc1037fa4);
   TESTDSPINST_AC_RS_RT_NODSPC("dpa.w.ph", 0xffffffff, 0xcb4ab48f, 0xffffffff,
                               0xcb4ab48f);
   TESTDSPINST_AC_RS_RT_NODSPC("dpa.w.ph", 0xffffffff, 0xaf8f7e18, 0xffffffff,
                               0xaf8f7e18);
   TESTDSPINST_AC_RS_RT_NODSPC("dpa.w.ph", 0xffffffff, 0x87df4510, 0xffffffff,
                               0x87df4510);
   TESTDSPINST_AC_RS_RT_NODSPC("dpa.w.ph", 0xffffffff, 0xabf4e8e1, 0xffffffff,
                               0xabf4e8e1);
   TESTDSPINST_AC_RS_RT_NODSPC("dpa.w.ph", 0xffffffff, 0xf4c0eeac, 0xffffffff,
                               0xf4c0eeac);
   TESTDSPINST_AC_RS_RT_NODSPC("dpa.w.ph", 0x00000000, 0x006a54f2, 0x00000000,
                               0x006a54f2);
   TESTDSPINST_AC_RS_RT_NODSPC("dpa.w.ph", 0x00000000, 0x79f74493, 0x00000000,
                               0x79f74493);
   TESTDSPINST_AC_RS_RT_NODSPC("dpa.w.ph", 0xffffffff, 0x9c09e313, 0xffffffff,
                               0x9c09e313);

   printf("-------- DPAQX_S.W.PH -------- \n");
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_s.w.ph $ac3, $t4, $t5", "ac3", 0x00000000,
                             0x00000000, 0xffffffff, 0x80000000, t4, t5);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_s.w.ph $ac0, $t0, $t1", "ac0", 0x00000004,
                             1073741824, 0x00000000, 0x00000006, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_s.w.ph $ac1, $t2, $t3", "ac1", 0x80002435,
                             0x80003421, 0x00000000, 1073741824, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_s.w.ph $ac3, $t6, $t7", "ac3", 0x76548000,
                             0x73468000, 0x00000000, 0x7fffffff, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_s.w.ph $ac0, $t5, $t3", "ac0", 0x80000000,
                             0x80000000, 0x00000000, 0x00000001, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_s.w.ph $ac1, $t2, $t4", "ac1", 0x00010001,
                             0xffffffff, 0xffffffff, 0xffffffff, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_s.w.ph $ac2, $t0, $t8", "ac2", 0x7fff7fff,
                             0x7fff7fff, 0xffffffff, 0xffffffff, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_s.w.ph $ac0, $t0, $t1", "ac0", 0x0000c420,
                             0x00000555, 0x00000000, 0x0fde3126, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_s.w.ph $ac1, $t2, $t3", "ac1", 0x00000000,
                             0x00000000, 0x00000000, 0x55555555, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_s.w.ph $ac2, $t4, $t1", "ac2", 0x80000000,
                             0x80000000, 0xffffffff, 0xffff2435, t4, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_s.w.ph $ac3, $t6, $t7", "ac3", 0xaaaaaaaa,
                             0x55555555, 0xffffffff, 0xabababab, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_s.w.ph $ac0, $t5, $t3", "ac0", 0x00000018,
                             0xffff2435, 0xffffffff, 0xfc79b4d2, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_s.w.ph $ac1, $t2, $t4", "ac1", 0xbabababa,
                             0xabababab, 0x00000000, 0x00000000, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_s.w.ph $ac2, $t0, $t8", "ac2", 0xf0f0f0f0,
                             0xfc79b4d2, 0x00000000, 0x00000000, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_s.w.ph $ac3, $t4, $t5", "ac3", 0xfbde3976,
                             0x00000000, 0x00000000, 0x12349876, t4, t5);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_s.w.ph $ac0, $t0, $t1", "ac0", 0x23534870,
                             0x00354565, 0x00000000, 0x00354565, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_s.w.ph $ac1, $t2, $t3", "ac1", 0x980b7cde,
                             0x00086755, 0x00000000, 0x00086755, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_s.w.ph $ac2, $t4, $t1", "ac2", 0x00000018,
                             0x8f8f8f8f, 0xffffffff, 0x8f8f8f8f, t4, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_s.w.ph $ac3, $t6, $t7", "ac3", 0x92784656,
                             0xeeeeeeee, 0xffffffff, 0xeeeeeeee, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_s.w.ph $ac0, $t5, $t3", "ac0", 0xcacacaca,
                             0x1bdbdbdb, 0x00000000, 0x1bdbdbdb, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_s.w.ph $ac1, $t2, $t4", "ac1", 0xbacabaca,
                             0xdecadeca, 0xffffffff, 0xdecadeca, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_s.w.ph $ac2, $t0, $t8", "ac2", 0x12fadeb4,
                             0x93474bde, 0xffffffff, 0x93474bde, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_s.w.ph $ac3, $t4, $t5", "ac3", 0x7c000790,
                             0xfc0007ff, 0xffffffff, 0xfabfabfa, t4, t5);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_s.w.ph $ac2, $t0, $t8", "ac2", 0xffffffff,
                             0xffffffff, 0x00000000, 0x083b3571, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_s.w.ph $ac0, $t0, $t1", "ac0", 0x24a3291e,
                             0x5648e540, 0xffffffff, 0xb9743941, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_s.w.ph $ac1, $t2, $t3", "ac1", 0xdd91eebf,
                             0xc54f79e6, 0xffffffff, 0xbce5f924, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_s.w.ph $ac2, $t4, $t1", "ac2", 0xf7ce2ec6,
                             0x5fc92974, 0xffffffff, 0xcc3c201c, t4, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_s.w.ph $ac3, $t6, $t7", "ac3", 0xbc1083e8,
                             0x7e08184e, 0x00000000, 0x1ebaf88e, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_s.w.ph $ac0, $t5, $t3", "ac0", 0xa617cc31,
                             0x71c8315f, 0x00000000, 0x722d5e20, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_s.w.ph $ac1, $t2, $t4", "ac1", 0xdfe1e8f0,
                             0x9493110e, 0xffffffff, 0xa1d6f791, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_s.w.ph $ac2, $t0, $t8", "ac2", 0x31458a23,
                             0xbb246228, 0x00000000, 0x7b11bee7, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_s.w.ph $ac3, $t4, $t5", "ac3", 0x848af791,
                             0x339d8d88, 0xffffffff, 0xa5631488, t4, t5);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_s.w.ph $ac0, $t0, $t1", "ac0", 0xda3bacdc,
                             0x70974249, 0xffffffff, 0xb10bcc65, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_s.w.ph $ac1, $t2, $t3", "ac1", 0x649d5cbd,
                             0x8a8d4e7d, 0x00000000, 0x73f39fca, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_s.w.ph $ac2, $t4, $t1", "ac2", 0xc0c8c881,
                             0xeb1b4335, 0x00000000, 0x5648e540, t4, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_s.w.ph $ac3, $t6, $t7", "ac3", 0x7dd81a20,
                             0x0cd6b508, 0xffffffff, 0xc54f79e6, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_s.w.ph $ac0, $t5, $t3", "ac0", 0x7fff7fff,
                             0x6731e282, 0x00000000, 0x5fc92974, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_s.w.ph $ac1, $t2, $t4", "ac1", 0x00000555,
                             0xb6edf28f, 0x00000000, 0x7e08184e, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_s.w.ph $ac2, $t0, $t8", "ac2", 0x00000000,
                             0x4b4ec9ca, 0x00000000, 0x71c8315f, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_s.w.ph $ac3, $t4, $t5", "ac3", 0x80000000,
                             0xc1037fa4, 0xffffffff, 0x9493110e, t4, t5);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_s.w.ph $ac1, $t2, $t4", "ac1", 0x55555555,
                             0xcb4ab48f, 0xffffffff, 0xbb246228, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_s.w.ph $ac2, $t0, $t8", "ac2", 0xffff8000,
                             0xaf8f8000, 0x00000000, 0x339d8d88, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_s.w.ph $ac0, $t0, $t1", "ac0", 0xabababab,
                             0x87df4510, 0x00000000, 0x70974249, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_s.w.ph $ac1, $t2, $t3", "ac1", 0xfc79b4d2,
                             0xabf4e8e1, 0xffffffff, 0x8a8d4e7d, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_s.w.ph $ac2, $t4, $t1", "ac2", 0x00000000,
                             0xf4c0eeac, 0xffffffff, 0xeb1b4335, t4, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_s.w.ph $ac3, $t6, $t7", "ac3", 0x00354565,
                             0x006a54f2, 0x00000000, 0x0cd6b508, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_s.w.ph $ac0, $t5, $t3", "ac0", 0x00086755,
                             0x79f74493, 0x00000000, 0x6731e282, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_s.w.ph $ac1, $t2, $t4", "ac1", 0xffff8000,
                             0x9c098000, 0xffffffff, 0xb6edf28f, t2, t4);

   printf("-------- DPAQX_SA.W.PH -------- \n");
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_sa.w.ph $ac3, $t4, $t5", "ac3", 0x00000000,
                             0x00000000, 0xffffffff, 0x80000000, t4, t5);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_sa.w.ph $ac0, $t0, $t1", "ac0", 0x00000004,
                             1073741824, 0x00000000, 0x00000006, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_sa.w.ph $ac1, $t2, $t3", "ac1", 0x80002435,
                             0x80003421, 0x00000000, 1073741824, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_sa.w.ph $ac3, $t6, $t7", "ac3", 0x76548000,
                             0x73468000, 0x00000000, 0x7fffffff, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_sa.w.ph $ac0, $t5, $t3", "ac0", 0x80000000,
                             0x80000000, 0x00000000, 0x00000001, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_sa.w.ph $ac1, $t2, $t4", "ac1", 0x00010001,
                             0xffffffff, 0xffffffff, 0xffffffff, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_sa.w.ph $ac2, $t0, $t8", "ac2", 0x7fff7fff,
                             0x7fff7fff, 0xffffffff, 0xffffffff, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_sa.w.ph $ac0, $t0, $t1", "ac0", 0x0000c420,
                             0x00000555, 0x00000000, 0x0fde3126, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_sa.w.ph $ac1, $t2, $t3", "ac1", 0x00000000,
                             0x00000000, 0x00000000, 0x55555555, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_sa.w.ph $ac2, $t4, $t1", "ac2", 0x80000000,
                             0x80000000, 0xffffffff, 0xffff2435, t4, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_sa.w.ph $ac3, $t6, $t7", "ac3", 0xaaaaaaaa,
                             0x55555555, 0xffffffff, 0xabababab, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_sa.w.ph $ac0, $t5, $t3", "ac0", 0x00000018,
                             0xffff2435, 0xffffffff, 0xfc79b4d2, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_sa.w.ph $ac1, $t2, $t4", "ac1", 0xbabababa,
                             0xabababab, 0x00000000, 0x00000000, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_sa.w.ph $ac2, $t0, $t8", "ac2", 0xf0f0f0f0,
                             0xfc79b4d2, 0x00000000, 0x00000000, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_sa.w.ph $ac3, $t4, $t5", "ac3", 0xfbde3976,
                             0x00000000, 0x00000000, 0x12349876, t4, t5);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_sa.w.ph $ac0, $t0, $t1", "ac0", 0x23534870,
                             0x00354565, 0x00000000, 0x00354565, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_sa.w.ph $ac1, $t2, $t3", "ac1", 0x980b7cde,
                             0x00086755, 0x00000000, 0x00086755, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_sa.w.ph $ac2, $t4, $t1", "ac2", 0x00000018,
                             0x8f8f8f8f, 0xffffffff, 0x8f8f8f8f, t4, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_sa.w.ph $ac3, $t6, $t7", "ac3", 0x92784656,
                             0xeeeeeeee, 0xffffffff, 0xeeeeeeee, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_sa.w.ph $ac0, $t5, $t3", "ac0", 0xcacacaca,
                             0x1bdbdbdb, 0x00000000, 0x1bdbdbdb, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_sa.w.ph $ac1, $t2, $t4", "ac1", 0xbacabaca,
                             0xdecadeca, 0xffffffff, 0xdecadeca, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_sa.w.ph $ac2, $t0, $t8", "ac2", 0x12fadeb4,
                             0x93474bde, 0xffffffff, 0x93474bde, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_sa.w.ph $ac3, $t4, $t5", "ac3", 0x7c000790,
                             0xfc0007ff, 0xffffffff, 0xfabfabfa, t4, t5);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_sa.w.ph $ac2, $t0, $t8", "ac2", 0xffffffff,
                             0xffffffff, 0x00000000, 0x083b3571, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_sa.w.ph $ac0, $t0, $t1", "ac0", 0x24a3291e,
                             0x5648e540, 0xffffffff, 0xb9743941, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_sa.w.ph $ac1, $t2, $t3", "ac1", 0xdd91eebf,
                             0xc54f79e6, 0xffffffff, 0xbce5f924, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_sa.w.ph $ac2, $t4, $t1", "ac2", 0xf7ce2ec6,
                             0x5fc92974, 0xffffffff, 0xcc3c201c, t4, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_sa.w.ph $ac3, $t6, $t7", "ac3", 0xbc1083e8,
                             0x7e08184e, 0x00000000, 0x1ebaf88e, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_sa.w.ph $ac0, $t5, $t3", "ac0", 0xa617cc31,
                             0x71c8315f, 0x00000000, 0x722d5e20, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_sa.w.ph $ac1, $t2, $t4", "ac1", 0xdfe1e8f0,
                             0x9493110e, 0xffffffff, 0xa1d6f791, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_sa.w.ph $ac2, $t0, $t8", "ac2", 0x31458a23,
                             0xbb246228, 0x00000000, 0x7b11bee7, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_sa.w.ph $ac3, $t4, $t5", "ac3", 0x848af791,
                             0x339d8d88, 0xffffffff, 0xa5631488, t4, t5);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_sa.w.ph $ac0, $t0, $t1", "ac0", 0xda3bacdc,
                             0x70974249, 0xffffffff, 0xb10bcc65, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_sa.w.ph $ac1, $t2, $t3", "ac1", 0x649d5cbd,
                             0x8a8d4e7d, 0x00000000, 0x73f39fca, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_sa.w.ph $ac2, $t4, $t1", "ac2", 0xc0c8c881,
                             0xeb1b4335, 0x00000000, 0x5648e540, t4, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_sa.w.ph $ac3, $t6, $t7", "ac3", 0x7dd81a20,
                             0x0cd6b508, 0xffffffff, 0xc54f79e6, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_sa.w.ph $ac0, $t5, $t3", "ac0", 0x7fff7fff,
                             0x6731e282, 0x00000000, 0x5fc92974, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_sa.w.ph $ac1, $t2, $t4", "ac1", 0x00000555,
                             0xb6edf28f, 0x00000000, 0x7e08184e, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_sa.w.ph $ac2, $t0, $t8", "ac2", 0x00000000,
                             0x4b4ec9ca, 0x00000000, 0x71c8315f, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_sa.w.ph $ac3, $t4, $t5", "ac3", 0x80000000,
                             0xc1037fa4, 0xffffffff, 0x9493110e, t4, t5);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_sa.w.ph $ac1, $t2, $t4", "ac1", 0x55555555,
                             0xcb4ab48f, 0xffffffff, 0xbb246228, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_sa.w.ph $ac2, $t0, $t8", "ac2", 0xffff8000,
                             0xaf8f8000, 0x00000000, 0x339d8d88, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_sa.w.ph $ac0, $t0, $t1", "ac0", 0xabababab,
                             0x87df4510, 0x00000000, 0x70974249, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_sa.w.ph $ac1, $t2, $t3", "ac1", 0xfc79b4d2,
                             0xabf4e8e1, 0xffffffff, 0x8a8d4e7d, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_sa.w.ph $ac2, $t4, $t1", "ac2", 0x00000000,
                             0xf4c0eeac, 0xffffffff, 0xeb1b4335, t4, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_sa.w.ph $ac3, $t6, $t7", "ac3", 0x00354565,
                             0x006a54f2, 0x00000000, 0x0cd6b508, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_sa.w.ph $ac0, $t5, $t3", "ac0", 0x00086755,
                             0x79f74493, 0x00000000, 0x6731e282, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpaqx_sa.w.ph $ac1, $t2, $t4", "ac1", 0xffff8000,
                             0x9c098000, 0xffffffff, 0xb6edf28f, t2, t4);

   printf("-------- DPAX.W.PH --------\n");
   TESTDSPINST_AC_RS_RT_NODSPC("dpax.w.ph", 0xffffffff, 0x00000000, 0x3277ffff,
                               0x4387ffff);
   TESTDSPINST_AC_RS_RT_NODSPC("dpax.w.ph", 0x00000000, 0x00000000, 0x00000004,
                               0x00000005);
   TESTDSPINST_AC_RS_RT_NODSPC("dpax.w.ph", 0xffffffff, 0xffffffff, 0x80000000,
                               0x80000000);
   TESTDSPINST_AC_RS_RT_NODSPC("dpax.w.ph", 0xeeeeffff, 0x00002345, 0x3277ffff,
                               0x4387ffff);
   TESTDSPINST_AC_RS_RT_NODSPC("dpax.w.ph", 0xffffaaaa, 0x12340000, 0x3277ffff,
                               0x4387ffff);
   TESTDSPINST_AC_RS_RT_NODSPC("dpax.w.ph", 0x00000000, 0x00000000, 0x80000000,
                               0x80000000);
   TESTDSPINST_AC_RS_RT_NODSPC("dpax.w.ph", 0xffffffff, 0x80008000, 0x80000000,
                               0x80000000);
   TESTDSPINST_AC_RS_RT_NODSPC("dpax.w.ph", 0x00000000, 0x00000000, 0x80000004,
                               0x00000005);
   TESTDSPINST_AC_RS_RT_NODSPC("dpax.w.ph", 0xffffffff, 0xffffffff, 0x80008000,
                               0x4387ffff);
   TESTDSPINST_AC_RS_RT_NODSPC("dpax.w.ph", 0xeeeeffff, 0x00002345, 0x80008000,
                               0x80008000);
   TESTDSPINST_AC_RS_RT_NODSPC("dpax.w.ph", 0xffffaaaa, 0x12340000, 0x3277ffff,
                               0x4387ffff);
   TESTDSPINST_AC_RS_RT_NODSPC("dpax.w.ph", 0x00000000, 0x00000000, 0x80008000,
                               0x80008000);
   TESTDSPINST_AC_RS_RT_NODSPC("dpax.w.ph", 0xffffffff, 0x00000000, 0x3277ffff,
                               0x4387ffff);
   TESTDSPINST_AC_RS_RT_NODSPC("dpax.w.ph", 0x80000000, 0xffff0000, 0x3277eeee,
                               0x80008000);
   TESTDSPINST_AC_RS_RT_NODSPC("dpax.w.ph", 0xffffffff, 0x00000000, 0x80000000,
                               0x80000000);
   TESTDSPINST_AC_RS_RT_NODSPC("dpax.w.ph", 0x0000ffff, 0xffff0000, 0x3277eeee,
                               0x4387ffff);
   TESTDSPINST_AC_RS_RT_NODSPC("dpax.w.ph", 0xffffffff, 0x0000ffff, 0x80008000,
                               0x4387ffff);
   TESTDSPINST_AC_RS_RT_NODSPC("dpax.w.ph", 0xffff1234, 0x00000000, 0xffff3277,
                               0x4387ffff);
   TESTDSPINST_AC_RS_RT_NODSPC("dpax.w.ph", 0x5678ffff, 0x3277ffff, 0x80000000,
                               0x80000000);
   TESTDSPINST_AC_RS_RT_NODSPC("dpax.w.ph", 0xffffffff, 0x00000000, 0xffff3277,
                               0xffff6543);
   TESTDSPINST_AC_RS_RT_NODSPC("dpax.w.ph", 0xffffffff, 0xffffffff, 0xffffffff,
                               0xffffffff);
   TESTDSPINST_AC_RS_RT_NODSPC("dpax.w.ph", 0xffffbbbb, 0xeeee0000, 0x80008000,
                               0x4387ffff);
   TESTDSPINST_AC_RS_RT_NODSPC("dpax.w.ph", 0xffffffff, 0x34560000, 0x3277ffff,
                               0x4387cccc);
   TESTDSPINST_AC_RS_RT_NODSPC("dpax.w.ph", 0xbbbbffff, 0x0000ffff, 0xeeeeffff,
                               0x4387ffff);
   TESTDSPINST_AC_RS_RT_NODSPC("dpax.w.ph", 0x12345678, 0xffffffff, 0xffffffff,
                               0xffffffff);
   TESTDSPINST_AC_RS_RT_NODSPC("dpax.w.ph", 0xffffbbbb, 0x12345678, 0x80008000,
                               0x4387ffff);
   TESTDSPINST_AC_RS_RT_NODSPC("dpax.w.ph", 0xffffffff, 0x34560000, 0x87654321,
                               0x80008000);
   TESTDSPINST_AC_RS_RT_NODSPC("dpax.w.ph", 0xbbbbffff, 0x0000ffff, 0xeeeeffff,
                               0x80000000);
   TESTDSPINST_AC_RS_RT_NODSPC("dpax.w.ph", 0xffffaaaa, 0x12340000, 0x3277ffff,
                               0x4387ffff);
   TESTDSPINST_AC_RS_RT_NODSPC("dpax.w.ph", 0x80000000, 0x00000000, 0x80008000,
                               0x80008000);
   TESTDSPINST_AC_RS_RT_NODSPC("dpax.w.ph", 0xffffffff, 0x80008000, 0x3277ffff,
                               0x4387ffff);
   TESTDSPINST_AC_RS_RT_NODSPC("dpax.w.ph", 0x0000ffff, 0xffff0000, 0x3277eeee,
                               0x4387ffff);
   TESTDSPINST_AC_RS_RT_NODSPC("dpax.w.ph", -24, 120, -24, 120);
   TESTDSPINST_AC_RS_RT_NODSPC("dpax.w.ph", 140, 120, 140, 120);
   TESTDSPINST_AC_RS_RT_NODSPC("dpax.w.ph", 0x00000004, 1073741824, 0x00000004,
                               1073741824);
   TESTDSPINST_AC_RS_RT_NODSPC("dpax.w.ph", (1<<31)+1, (1<<31)+2, (1<<31)+1,
                               (1<<31)+2);
   TESTDSPINST_AC_RS_RT_NODSPC("dpax.w.ph", 0x80002431, 0x4b908000, 0x80002431,
                               0x4b908000);
   TESTDSPINST_AC_RS_RT_NODSPC("dpax.w.ph", 0x004d8000, 0x800027cc, 0x004d8000,
                               0x800027cc);
   TESTDSPINST_AC_RS_RT_NODSPC("dpax.w.ph", 0xf6a3fa3c, 0x083b3571, 0xf6a3fa3c,
                               0x083b3571);
   TESTDSPINST_AC_RS_RT_NODSPC("dpax.w.ph", 0xbf17fb9a, 0xb9743941, 0xbf17fb9a,
                               0xb9743941);
   TESTDSPINST_AC_RS_RT_NODSPC("dpax.w.ph", 0x2c0bd024, 0xbce5f924, 0x2c0bd024,
                               0xbce5f924);
   TESTDSPINST_AC_RS_RT_NODSPC("dpax.w.ph", 0x3e976e2e, 0xcc3c201c, 0x3e976e2e,
                               0xcc3c201c);
   TESTDSPINST_AC_RS_RT_NODSPC("dpax.w.ph", 0xb4bfb365, 0x1ebaf88e, 0xb4bfb365,
                               0x1ebaf88e);
   TESTDSPINST_AC_RS_RT_NODSPC("dpax.w.ph", 0x288593c0, 0x722d5e20, 0x288593c0,
                               0x722d5e20);
   TESTDSPINST_AC_RS_RT_NODSPC("dpax.w.ph", 0x4d7ff5b4, 0xa1d6f791, 0x4d7ff5b4,
                               0xa1d6f791);
   TESTDSPINST_AC_RS_RT_NODSPC("dpax.w.ph", 0x4557be13, 0x7b11bee7, 0x4557be13,
                               0x7b11bee7);
   TESTDSPINST_AC_RS_RT_NODSPC("dpax.w.ph", 0xadcf5772, 0xa5631488, 0xadcf5772,
                               0xa5631488);
   TESTDSPINST_AC_RS_RT_NODSPC("dpax.w.ph", 0x989a7235, 0xb10bcc65, 0x989a7235,
                               0xb10bcc65);
   TESTDSPINST_AC_RS_RT_NODSPC("dpax.w.ph", 0x4d6f393a, 0x73f39fca, 0x4d6f393a,
                               0x73f39fca);
   TESTDSPINST_AC_RS_RT_NODSPC("dpax.w.ph", 0x24a3291e, 0x5648e540, 0x24a3291e,
                               0x5648e540);
   TESTDSPINST_AC_RS_RT_NODSPC("dpax.w.ph", 0xdd91eebf, 0xc54f79e6, 0xdd91eebf,
                               0xc54f79e6);
   TESTDSPINST_AC_RS_RT_NODSPC("dpax.w.ph", 0xf7ce2ec6, 0x5fc92974, 0xf7ce2ec6,
                               0x5fc92974);
   TESTDSPINST_AC_RS_RT_NODSPC("dpax.w.ph", 0xbc1083e8, 0x7e08184e, 0xbc1083e8,
                               0x7e08184e);
   TESTDSPINST_AC_RS_RT_NODSPC("dpax.w.ph", 0xa617cc31, 0x71c8315f, 0xa617cc31,
                               0x71c8315f);
   TESTDSPINST_AC_RS_RT_NODSPC("dpax.w.ph", 0xdfe1e8f0, 0x9493110e, 0xdfe1e8f0,
                               0x9493110e);
   TESTDSPINST_AC_RS_RT_NODSPC("dpax.w.ph", 0x31458a23, 0xbb246228, 0x31458a23,
                               0xbb246228);
   TESTDSPINST_AC_RS_RT_NODSPC("dpax.w.ph", 0x848af791, 0x339d8d88, 0x848af791,
                               0x339d8d88);
   TESTDSPINST_AC_RS_RT_NODSPC("dpax.w.ph", 0xda3bacdc, 0x70974249, 0xda3bacdc,
                               0x70974249);
   TESTDSPINST_AC_RS_RT_NODSPC("dpax.w.ph", 0x649d5cbd, 0x8a8d4e7d, 0x649d5cbd,
                               0x8a8d4e7d);
   TESTDSPINST_AC_RS_RT_NODSPC("dpax.w.ph", 0xc0c8c881, 0xeb1b4335, 0xc0c8c881,
                               0xeb1b4335);
   TESTDSPINST_AC_RS_RT_NODSPC("dpax.w.ph", 0x7dd81a20, 0x0cd6b508, 0x7dd81a20,
                               0x0cd6b508);
   TESTDSPINST_AC_RS_RT_NODSPC("dpax.w.ph", 0x00000000, 0x6731e282, 0x00000000,
                               0x6731e282);
   TESTDSPINST_AC_RS_RT_NODSPC("dpax.w.ph", 0xffffffff, 0xb6edf28f, 0xffffffff,
                               0xb6edf28f);
   TESTDSPINST_AC_RS_RT_NODSPC("dpax.w.ph", 0x00000000, 0x4b4ec9ca, 0x00000000,
                               0x4b4ec9ca);
   TESTDSPINST_AC_RS_RT_NODSPC("dpax.w.ph", 0xffffffff, 0xc1037fa4, 0xffffffff,
                               0xc1037fa4);
   TESTDSPINST_AC_RS_RT_NODSPC("dpax.w.ph", 0xffffffff, 0xcb4ab48f, 0xffffffff,
                               0xcb4ab48f);
   TESTDSPINST_AC_RS_RT_NODSPC("dpax.w.ph", 0xffffffff, 0xaf8f7e18, 0xffffffff,
                               0xaf8f7e18);
   TESTDSPINST_AC_RS_RT_NODSPC("dpax.w.ph", 0xffffffff, 0x87df4510, 0xffffffff,
                               0x87df4510);
   TESTDSPINST_AC_RS_RT_NODSPC("dpax.w.ph", 0xffffffff, 0xabf4e8e1, 0xffffffff,
                               0xabf4e8e1);
   TESTDSPINST_AC_RS_RT_NODSPC("dpax.w.ph", 0xffffffff, 0xf4c0eeac, 0xffffffff,
                               0xf4c0eeac);
   TESTDSPINST_AC_RS_RT_NODSPC("dpax.w.ph", 0x00000000, 0x006a54f2, 0x00000000,
                               0x006a54f2);
   TESTDSPINST_AC_RS_RT_NODSPC("dpax.w.ph", 0x00000000, 0x79f74493, 0x00000000,
                               0x79f74493);
   TESTDSPINST_AC_RS_RT_NODSPC("dpax.w.ph", 0xffffffff, 0x9c09e313, 0xffffffff,
                               0x9c09e313);

   printf("-------- DPS.W.PH --------\n");
   TESTDSPINST_AC_RS_RT_NODSPC("dps.w.ph", 0xffffffff, 0x00000000, 0x3277ffff,
                               0x4387ffff);
   TESTDSPINST_AC_RS_RT_NODSPC("dps.w.ph", 0x00000000, 0x00000000, 0x00000004,
                               0x00000005);
   TESTDSPINST_AC_RS_RT_NODSPC("dps.w.ph", 0xffffffff, 0xffffffff, 0x80000000,
                               0x80000000);
   TESTDSPINST_AC_RS_RT_NODSPC("dps.w.ph", 0xeeeeffff, 0x00002345, 0x3277ffff,
                               0x4387ffff);
   TESTDSPINST_AC_RS_RT_NODSPC("dps.w.ph", 0xffffaaaa, 0x12340000, 0x3277ffff,
                               0x4387ffff);
   TESTDSPINST_AC_RS_RT_NODSPC("dps.w.ph", 0x00000000, 0x00000000, 0x80000000,
                               0x80000000);
   TESTDSPINST_AC_RS_RT_NODSPC("dps.w.ph", 0xffffffff, 0x80008000, 0x80000000,
                               0x80000000);
   TESTDSPINST_AC_RS_RT_NODSPC("dps.w.ph", 0x00000000, 0x00000000, 0x80000004,
                               0x00000005);
   TESTDSPINST_AC_RS_RT_NODSPC("dps.w.ph", 0xffffffff, 0xffffffff, 0x80008000,
                               0x4387ffff);
   TESTDSPINST_AC_RS_RT_NODSPC("dps.w.ph", 0xeeeeffff, 0x00002345, 0x80008000,
                               0x80008000);
   TESTDSPINST_AC_RS_RT_NODSPC("dps.w.ph", 0xffffaaaa, 0x12340000, 0x3277ffff,
                               0x4387ffff);
   TESTDSPINST_AC_RS_RT_NODSPC("dps.w.ph", 0x00000000, 0x00000000, 0x80008000,
                               0x80008000);
   TESTDSPINST_AC_RS_RT_NODSPC("dps.w.ph", 0xffffffff, 0x00000000, 0x3277ffff,
                               0x4387ffff);
   TESTDSPINST_AC_RS_RT_NODSPC("dps.w.ph", 0x80000000, 0xffff0000, 0x3277eeee,
                               0x80008000);
   TESTDSPINST_AC_RS_RT_NODSPC("dps.w.ph", 0xffffffff, 0x00000000, 0x80000000,
                               0x80000000);
   TESTDSPINST_AC_RS_RT_NODSPC("dps.w.ph", 0x0000ffff, 0xffff0000, 0x3277eeee,
                               0x4387ffff);
   TESTDSPINST_AC_RS_RT_NODSPC("dps.w.ph", 0xffffffff, 0x0000ffff, 0x80008000,
                               0x4387ffff);
   TESTDSPINST_AC_RS_RT_NODSPC("dps.w.ph", 0xffff1234, 0x00000000, 0xffff3277,
                               0x4387ffff);
   TESTDSPINST_AC_RS_RT_NODSPC("dps.w.ph", 0x5678ffff, 0x3277ffff, 0x80000000,
                               0x80000000);
   TESTDSPINST_AC_RS_RT_NODSPC("dps.w.ph", 0xffffffff, 0x00000000, 0xffff3277,
                               0xffff6543);
   TESTDSPINST_AC_RS_RT_NODSPC("dps.w.ph", 0xffffffff, 0xffffffff, 0xffffffff,
                               0xffffffff);
   TESTDSPINST_AC_RS_RT_NODSPC("dps.w.ph", 0xffffbbbb, 0xeeee0000, 0x80008000,
                               0x4387ffff);
   TESTDSPINST_AC_RS_RT_NODSPC("dps.w.ph", 0xffffffff, 0x34560000, 0x3277ffff,
                               0x4387cccc);
   TESTDSPINST_AC_RS_RT_NODSPC("dps.w.ph", 0xbbbbffff, 0x0000ffff, 0xeeeeffff,
                               0x4387ffff);
   TESTDSPINST_AC_RS_RT_NODSPC("dps.w.ph", 0x12345678, 0xffffffff, 0xffffffff,
                               0xffffffff);
   TESTDSPINST_AC_RS_RT_NODSPC("dps.w.ph", 0xffffbbbb, 0x12345678, 0x80008000,
                               0x4387ffff);
   TESTDSPINST_AC_RS_RT_NODSPC("dps.w.ph", 0xffffffff, 0x34560000, 0x87654321,
                               0x80008000);
   TESTDSPINST_AC_RS_RT_NODSPC("dps.w.ph", 0xbbbbffff, 0x0000ffff, 0xeeeeffff,
                               0x80000000);
   TESTDSPINST_AC_RS_RT_NODSPC("dps.w.ph", 0xffffaaaa, 0x12340000, 0x3277ffff,
                               0x4387ffff);
   TESTDSPINST_AC_RS_RT_NODSPC("dps.w.ph", 0x80000000, 0x00000000, 0x80008000,
                               0x80008000);
   TESTDSPINST_AC_RS_RT_NODSPC("dps.w.ph", 0xffffffff, 0x80008000, 0x3277ffff,
                               0x4387ffff);
   TESTDSPINST_AC_RS_RT_NODSPC("dps.w.ph", 0x0000ffff, 0xffff0000, 0x3277eeee,
                               0x4387ffff);
   TESTDSPINST_AC_RS_RT_NODSPC("dps.w.ph", -24, 120, -24, 120);
   TESTDSPINST_AC_RS_RT_NODSPC("dps.w.ph", 140, 120, 140, 120);
   TESTDSPINST_AC_RS_RT_NODSPC("dps.w.ph", 0x00000004, 1073741824, 0x00000004,
                               1073741824);
   TESTDSPINST_AC_RS_RT_NODSPC("dps.w.ph", (1<<31)+1, (1<<31)+2, (1<<31)+1,
                               (1<<31)+2);
   TESTDSPINST_AC_RS_RT_NODSPC("dps.w.ph", 0x80002431, 0x4b908000, 0x80002431,
                               0x4b908000);
   TESTDSPINST_AC_RS_RT_NODSPC("dps.w.ph", 0x004d8000, 0x800027cc, 0x004d8000,
                               0x800027cc);
   TESTDSPINST_AC_RS_RT_NODSPC("dps.w.ph", 0xf6a3fa3c, 0x083b3571, 0xf6a3fa3c,
                               0x083b3571);
   TESTDSPINST_AC_RS_RT_NODSPC("dps.w.ph", 0xbf17fb9a, 0xb9743941, 0xbf17fb9a,
                               0xb9743941);
   TESTDSPINST_AC_RS_RT_NODSPC("dps.w.ph", 0x2c0bd024, 0xbce5f924, 0x2c0bd024,
                               0xbce5f924);
   TESTDSPINST_AC_RS_RT_NODSPC("dps.w.ph", 0x3e976e2e, 0xcc3c201c, 0x3e976e2e,
                               0xcc3c201c);
   TESTDSPINST_AC_RS_RT_NODSPC("dps.w.ph", 0xb4bfb365, 0x1ebaf88e, 0xb4bfb365,
                               0x1ebaf88e);
   TESTDSPINST_AC_RS_RT_NODSPC("dps.w.ph", 0x288593c0, 0x722d5e20, 0x288593c0,
                               0x722d5e20);
   TESTDSPINST_AC_RS_RT_NODSPC("dps.w.ph", 0x4d7ff5b4, 0xa1d6f791, 0x4d7ff5b4,
                               0xa1d6f791);
   TESTDSPINST_AC_RS_RT_NODSPC("dps.w.ph", 0x4557be13, 0x7b11bee7, 0x4557be13,
                               0x7b11bee7);
   TESTDSPINST_AC_RS_RT_NODSPC("dps.w.ph", 0xadcf5772, 0xa5631488, 0xadcf5772,
                               0xa5631488);
   TESTDSPINST_AC_RS_RT_NODSPC("dps.w.ph", 0x989a7235, 0xb10bcc65, 0x989a7235,
                               0xb10bcc65);
   TESTDSPINST_AC_RS_RT_NODSPC("dps.w.ph", 0x4d6f393a, 0x73f39fca, 0x4d6f393a,
                               0x73f39fca);
   TESTDSPINST_AC_RS_RT_NODSPC("dps.w.ph", 0x24a3291e, 0x5648e540, 0x24a3291e,
                               0x5648e540);
   TESTDSPINST_AC_RS_RT_NODSPC("dps.w.ph", 0xdd91eebf, 0xc54f79e6, 0xdd91eebf,
                               0xc54f79e6);
   TESTDSPINST_AC_RS_RT_NODSPC("dps.w.ph", 0xf7ce2ec6, 0x5fc92974, 0xf7ce2ec6,
                               0x5fc92974);
   TESTDSPINST_AC_RS_RT_NODSPC("dps.w.ph", 0xbc1083e8, 0x7e08184e, 0xbc1083e8,
                               0x7e08184e);
   TESTDSPINST_AC_RS_RT_NODSPC("dps.w.ph", 0xa617cc31, 0x71c8315f, 0xa617cc31,
                               0x71c8315f);
   TESTDSPINST_AC_RS_RT_NODSPC("dps.w.ph", 0xdfe1e8f0, 0x9493110e, 0xdfe1e8f0,
                               0x9493110e);
   TESTDSPINST_AC_RS_RT_NODSPC("dps.w.ph", 0x31458a23, 0xbb246228, 0x31458a23,
                               0xbb246228);
   TESTDSPINST_AC_RS_RT_NODSPC("dps.w.ph", 0x848af791, 0x339d8d88, 0x848af791,
                               0x339d8d88);
   TESTDSPINST_AC_RS_RT_NODSPC("dps.w.ph", 0xda3bacdc, 0x70974249, 0xda3bacdc,
                               0x70974249);
   TESTDSPINST_AC_RS_RT_NODSPC("dps.w.ph", 0x649d5cbd, 0x8a8d4e7d, 0x649d5cbd,
                               0x8a8d4e7d);
   TESTDSPINST_AC_RS_RT_NODSPC("dps.w.ph", 0xc0c8c881, 0xeb1b4335, 0xc0c8c881,
                               0xeb1b4335);
   TESTDSPINST_AC_RS_RT_NODSPC("dps.w.ph", 0x7dd81a20, 0x0cd6b508, 0x7dd81a20,
                               0x0cd6b508);
   TESTDSPINST_AC_RS_RT_NODSPC("dps.w.ph", 0x00000000, 0x6731e282, 0x00000000,
                               0x6731e282);
   TESTDSPINST_AC_RS_RT_NODSPC("dps.w.ph", 0xffffffff, 0xb6edf28f, 0xffffffff,
                               0xb6edf28f);
   TESTDSPINST_AC_RS_RT_NODSPC("dps.w.ph", 0x00000000, 0x4b4ec9ca, 0x00000000,
                               0x4b4ec9ca);
   TESTDSPINST_AC_RS_RT_NODSPC("dps.w.ph", 0xffffffff, 0xc1037fa4, 0xffffffff,
                               0xc1037fa4);
   TESTDSPINST_AC_RS_RT_NODSPC("dps.w.ph", 0xffffffff, 0xcb4ab48f, 0xffffffff,
                               0xcb4ab48f);
   TESTDSPINST_AC_RS_RT_NODSPC("dps.w.ph", 0xffffffff, 0xaf8f7e18, 0xffffffff,
                               0xaf8f7e18);
   TESTDSPINST_AC_RS_RT_NODSPC("dps.w.ph", 0xffffffff, 0x87df4510, 0xffffffff,
                               0x87df4510);
   TESTDSPINST_AC_RS_RT_NODSPC("dps.w.ph", 0xffffffff, 0xabf4e8e1, 0xffffffff,
                               0xabf4e8e1);
   TESTDSPINST_AC_RS_RT_NODSPC("dps.w.ph", 0xffffffff, 0xf4c0eeac, 0xffffffff,
                               0xf4c0eeac);
   TESTDSPINST_AC_RS_RT_NODSPC("dps.w.ph", 0x00000000, 0x006a54f2, 0x00000000,
                               0x006a54f2);
   TESTDSPINST_AC_RS_RT_NODSPC("dps.w.ph", 0x00000000, 0x79f74493, 0x00000000,
                               0x79f74493);
   TESTDSPINST_AC_RS_RT_NODSPC("dps.w.ph", 0xffffffff, 0x9c09e313, 0xffffffff,
                               0x9c09e313);

   printf("-------- DPSQX_S.W.PH -------- \n");
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_s.w.ph $ac3, $t4, $t5", "ac3", 0x00000000,
                             0x00000000, 0xffffffff, 0x80000000, t4, t5);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_s.w.ph $ac0, $t0, $t1", "ac0", 0x00000004,
                             1073741824, 0x00000000, 0x00000006, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_s.w.ph $ac1, $t2, $t3", "ac1", 0x80002435,
                             0x80003421, 0x00000000, 1073741824, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_s.w.ph $ac3, $t6, $t7", "ac3", 0x76548000,
                             0x73468000, 0x00000000, 0x7fffffff, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_s.w.ph $ac0, $t5, $t3", "ac0", 0x80000000,
                             0x80000000, 0x00000000, 0x00000001, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_s.w.ph $ac1, $t2, $t4", "ac1", 0x00010001,
                             0xffffffff, 0xffffffff, 0xffffffff, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_s.w.ph $ac2, $t0, $t8", "ac2", 0x7fff7fff,
                             0x7fff7fff, 0xffffffff, 0xffffffff, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_s.w.ph $ac0, $t0, $t1", "ac0", 0x0000c420,
                             0x00000555, 0x00000000, 0x0fde3126, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_s.w.ph $ac1, $t2, $t3", "ac1", 0x00000000,
                             0x00000000, 0x00000000, 0x55555555, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_s.w.ph $ac2, $t4, $t1", "ac2", 0x80000000,
                             0x80000000, 0xffffffff, 0xffff2435, t4, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_s.w.ph $ac3, $t6, $t7", "ac3", 0xaaaaaaaa,
                             0x55555555, 0xffffffff, 0xabababab, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_s.w.ph $ac0, $t5, $t3", "ac0", 0x00000018,
                             0xffff2435, 0xffffffff, 0xfc79b4d2, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_s.w.ph $ac1, $t2, $t4", "ac1", 0xbabababa,
                             0xabababab, 0x00000000, 0x00000000, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_s.w.ph $ac2, $t0, $t8", "ac2", 0xf0f0f0f0,
                             0xfc79b4d2, 0x00000000, 0x00000000, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_s.w.ph $ac3, $t4, $t5", "ac3", 0xfbde3976,
                             0x00000000, 0x00000000, 0x12349876, t4, t5);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_s.w.ph $ac0, $t0, $t1", "ac0", 0x23534870,
                             0x00354565, 0x00000000, 0x00354565, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_s.w.ph $ac1, $t2, $t3", "ac1", 0x980b7cde,
                             0x00086755, 0x00000000, 0x00086755, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_s.w.ph $ac2, $t4, $t1", "ac2", 0x00000018,
                             0x8f8f8f8f, 0xffffffff, 0x8f8f8f8f, t4, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_s.w.ph $ac3, $t6, $t7", "ac3", 0x92784656,
                             0xeeeeeeee, 0xffffffff, 0xeeeeeeee, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_s.w.ph $ac0, $t5, $t3", "ac0", 0xcacacaca,
                             0x1bdbdbdb, 0x00000000, 0x1bdbdbdb, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_s.w.ph $ac1, $t2, $t4", "ac1", 0xbacabaca,
                             0xdecadeca, 0xffffffff, 0xdecadeca, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_s.w.ph $ac2, $t0, $t8", "ac2", 0x12fadeb4,
                             0x93474bde, 0xffffffff, 0x93474bde, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_s.w.ph $ac3, $t4, $t5", "ac3", 0x7c000790,
                             0xfc0007ff, 0xffffffff, 0xfabfabfa, t4, t5);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_s.w.ph $ac2, $t0, $t8", "ac2", 0xffffffff,
                             0xffffffff, 0x00000000, 0x083b3571, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_s.w.ph $ac0, $t0, $t1", "ac0", 0x24a3291e,
                             0x5648e540, 0xffffffff, 0xb9743941, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_s.w.ph $ac1, $t2, $t3", "ac1", 0xdd91eebf,
                             0xc54f79e6, 0xffffffff, 0xbce5f924, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_s.w.ph $ac2, $t4, $t1", "ac2", 0xf7ce2ec6,
                             0x5fc92974, 0xffffffff, 0xcc3c201c, t4, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_s.w.ph $ac3, $t6, $t7", "ac3", 0xbc1083e8,
                             0x7e08184e, 0x00000000, 0x1ebaf88e, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_s.w.ph $ac0, $t5, $t3", "ac0", 0xa617cc31,
                             0x71c8315f, 0x00000000, 0x722d5e20, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_s.w.ph $ac1, $t2, $t4", "ac1", 0xdfe1e8f0,
                             0x9493110e, 0xffffffff, 0xa1d6f791, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_s.w.ph $ac2, $t0, $t8", "ac2", 0x31458a23,
                             0xbb246228, 0x00000000, 0x7b11bee7, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_s.w.ph $ac3, $t4, $t5", "ac3", 0x848af791,
                             0x339d8d88, 0xffffffff, 0xa5631488, t4, t5);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_s.w.ph $ac0, $t0, $t1", "ac0", 0xda3bacdc,
                             0x70974249, 0xffffffff, 0xb10bcc65, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_s.w.ph $ac1, $t2, $t3", "ac1", 0x649d5cbd,
                             0x8a8d4e7d, 0x00000000, 0x73f39fca, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_s.w.ph $ac2, $t4, $t1", "ac2", 0xc0c8c881,
                             0xeb1b4335, 0x00000000, 0x5648e540, t4, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_s.w.ph $ac3, $t6, $t7", "ac3", 0x7dd81a20,
                             0x0cd6b508, 0xffffffff, 0xc54f79e6, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_s.w.ph $ac0, $t5, $t3", "ac0", 0x7fff7fff,
                             0x6731e282, 0x00000000, 0x5fc92974, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_s.w.ph $ac1, $t2, $t4", "ac1", 0x00000555,
                             0xb6edf28f, 0x00000000, 0x7e08184e, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_s.w.ph $ac2, $t0, $t8", "ac2", 0x00000000,
                             0x4b4ec9ca, 0x00000000, 0x71c8315f, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_s.w.ph $ac3, $t4, $t5", "ac3", 0x80000000,
                             0xc1037fa4, 0xffffffff, 0x9493110e, t4, t5);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_s.w.ph $ac1, $t2, $t4", "ac1", 0x55555555,
                             0xcb4ab48f, 0xffffffff, 0xbb246228, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_s.w.ph $ac2, $t0, $t8", "ac2", 0xffff8000,
                             0xaf8f8000, 0x00000000, 0x339d8d88, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_s.w.ph $ac0, $t0, $t1", "ac0", 0xabababab,
                             0x87df4510, 0x00000000, 0x70974249, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_s.w.ph $ac1, $t2, $t3", "ac1", 0xfc79b4d2,
                             0xabf4e8e1, 0xffffffff, 0x8a8d4e7d, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_s.w.ph $ac2, $t4, $t1", "ac2", 0x00000000,
                             0xf4c0eeac, 0xffffffff, 0xeb1b4335, t4, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_s.w.ph $ac3, $t6, $t7", "ac3", 0x00354565,
                             0x006a54f2, 0x00000000, 0x0cd6b508, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_s.w.ph $ac0, $t5, $t3", "ac0", 0x00086755,
                             0x79f74493, 0x00000000, 0x6731e282, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_s.w.ph $ac1, $t2, $t4", "ac1", 0xffff8000,
                             0x9c098000, 0xffffffff, 0xb6edf28f, t2, t4);

   printf("-------- DPSQX_SA.W.PH -------- \n");
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_sa.w.ph $ac3, $t4, $t5", "ac3", 0x00000000,
                             0x00000000, 0xffffffff, 0x80000000, t4, t5);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_sa.w.ph $ac0, $t0, $t1", "ac0", 0x00000004,
                             1073741824, 0x00000000, 0x00000006, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_sa.w.ph $ac1, $t2, $t3", "ac1", 0x80002435,
                             0x80003421, 0x00000000, 1073741824, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_sa.w.ph $ac3, $t6, $t7", "ac3", 0x76548000,
                             0x73468000, 0x00000000, 0x7fffffff, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_sa.w.ph $ac0, $t5, $t3", "ac0", 0x80000000,
                             0x80000000, 0x00000000, 0x00000001, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_sa.w.ph $ac1, $t2, $t4", "ac1", 0x00010001,
                             0xcfffefff, 0xffffffff, 0xffffffff, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_sa.w.ph $ac2, $t0, $t8", "ac2", 0x7fff7fff,
                             0x7fff7fff, 0xffffffff, 0xffffffff, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_sa.w.ph $ac0, $t0, $t1", "ac0", 0x0000c420,
                             0x00000555, 0x00000000, 0x0fde3126, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_sa.w.ph $ac1, $t2, $t3", "ac1", 0x00000000,
                             0x00000000, 0x00000000, 0x55555555, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_sa.w.ph $ac2, $t4, $t1", "ac2", 0x80000000,
                             0x80000000, 0xffffffff, 0xffff2435, t4, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_sa.w.ph $ac3, $t6, $t7", "ac3", 0xaaaaaaaa,
                             0x55555555, 0xffffffff, 0xabababab, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_sa.w.ph $ac0, $t5, $t3", "ac0", 0x00000018,
                             0xfbff2435, 0xffffffff, 0xfc79b4d2, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_sa.w.ph $ac1, $t2, $t4", "ac1", 0xbabababa,
                             0xabababab, 0x00000000, 0x00000000, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_sa.w.ph $ac2, $t0, $t8", "ac2", 0xf0f0f0f0,
                             0xfc79b4d2, 0x00000000, 0x00000000, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_sa.w.ph $ac3, $t4, $t5", "ac3", 0xfbde3976,
                             0x00000000, 0x00000000, 0x12349876, t4, t5);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_sa.w.ph $ac0, $t0, $t1", "ac0", 0x23534870,
                             0x00354565, 0x00000000, 0x00354565, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_sa.w.ph $ac1, $t2, $t3", "ac1", 0x980b7cde,
                             0x00086755, 0x00000000, 0x00086755, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_sa.w.ph $ac2, $t4, $t1", "ac2", 0x00000018,
                             0x8f8f8f8f, 0xffffffff, 0x8f8f8f8f, t4, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_sa.w.ph $ac3, $t6, $t7", "ac3", 0x92784656,
                             0xeeeeeeee, 0xffffffff, 0xeeeeeeee, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_sa.w.ph $ac0, $t5, $t3", "ac0", 0xcacacaca,
                             0x1bdbdbdb, 0x00000000, 0x1bdbdbdb, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_sa.w.ph $ac1, $t2, $t4", "ac1", 0xbacabaca,
                             0xdecadeca, 0xffffffff, 0xdecadeca, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_sa.w.ph $ac2, $t0, $t8", "ac2", 0x12fadeb4,
                             0x93474bde, 0xffffffff, 0x93474bde, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_sa.w.ph $ac3, $t4, $t5", "ac3", 0x7c000790,
                             0xfc0007ff, 0xffffffff, 0xfabfabfa, t4, t5);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_sa.w.ph $ac2, $t0, $t8", "ac2", 0xbfff54ff,
                             0xfb32ff01, 0x00000000, 0x083b3571, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_sa.w.ph $ac0, $t0, $t1", "ac0", 0x24a3291e,
                             0x5648e540, 0xffffffff, 0xb9743941, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_sa.w.ph $ac1, $t2, $t3", "ac1", 0xdd91eebf,
                             0xc54f79e6, 0xffffffff, 0xbce5f924, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_sa.w.ph $ac2, $t4, $t1", "ac2", 0xf7ce2ec6,
                             0x5fc92974, 0xffffffff, 0xcc3c201c, t4, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_sa.w.ph $ac3, $t6, $t7", "ac3", 0xbc1083e8,
                             0x7e08184e, 0x00000000, 0x1ebaf88e, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_sa.w.ph $ac0, $t5, $t3", "ac0", 0xa617cc31,
                             0x71c8315f, 0x00000000, 0x722d5e20, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_sa.w.ph $ac1, $t2, $t4", "ac1", 0xdfe1e8f0,
                             0x9493110e, 0xffffffff, 0xa1d6f791, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_sa.w.ph $ac2, $t0, $t8", "ac2", 0x31458a23,
                             0xbb246228, 0x00000000, 0x7b11bee7, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_sa.w.ph $ac3, $t4, $t5", "ac3", 0x848af791,
                             0x339d8d88, 0xffffffff, 0xa5631488, t4, t5);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_sa.w.ph $ac0, $t0, $t1", "ac0", 0xda3bacdc,
                             0x70974249, 0xffffffff, 0xb10bcc65, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_sa.w.ph $ac1, $t2, $t3", "ac1", 0x649d5cbd,
                             0x8a8d4e7d, 0x00000000, 0x73f39fca, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_sa.w.ph $ac2, $t4, $t1", "ac2", 0xc0c8c881,
                             0xeb1b4335, 0x00000000, 0x5648e540, t4, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_sa.w.ph $ac3, $t6, $t7", "ac3", 0x7dd81a20,
                             0x0cd6b508, 0xffffffff, 0xc54f79e6, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_sa.w.ph $ac0, $t5, $t3", "ac0", 0x7fff7fff,
                             0x6731e282, 0x00000000, 0x5fc92974, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_sa.w.ph $ac1, $t2, $t4", "ac1", 0x00000555,
                             0xb6edf28f, 0x00000000, 0x7e08184e, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_sa.w.ph $ac2, $t0, $t8", "ac2", 0x00000000,
                             0x4b4ec9ca, 0x00000000, 0x71c8315f, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_sa.w.ph $ac3, $t4, $t5", "ac3", 0x80000000,
                             0xc1037fa4, 0xffffffff, 0x9493110e, t4, t5);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_sa.w.ph $ac1, $t2, $t4", "ac1", 0x55555555,
                             0xcb4ab48f, 0xffffffff, 0xbb246228, t2, t4);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_sa.w.ph $ac2, $t0, $t8", "ac2", 0x0fff8000,
                             0xaf8f8000, 0x00000000, 0x339d8d88, t0, t8);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_sa.w.ph $ac0, $t0, $t1", "ac0", 0xabababab,
                             0x87df4510, 0x00000000, 0x70974249, t0, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_sa.w.ph $ac1, $t2, $t3", "ac1", 0xfc79b4d2,
                             0xabf4e8e1, 0xffffffff, 0x8a8d4e7d, t2, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_sa.w.ph $ac2, $t4, $t1", "ac2", 0x00000000,
                             0xf4c0eeac, 0xffffffff, 0xeb1b4335, t4, t1);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_sa.w.ph $ac3, $t6, $t7", "ac3", 0x00354565,
                             0x006a54f2, 0x00000000, 0x0cd6b508, t6, t7);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_sa.w.ph $ac0, $t5, $t3", "ac0", 0x00086755,
                             0x79f74493, 0x00000000, 0x6731e282, t5, t3);
   TESTDSPINST_AC_RS_RT_DSPC("dpsqx_sa.w.ph $ac1, $t2, $t4", "ac1", 0x98548000,
                             0x9c098000, 0xffffffff, 0xb6edf28f, t2, t4);

   printf("-------- DPSX.W.PH --------\n");
   TESTDSPINST_AC_RS_RT_NODSPC("dpsx.w.ph", 0xffffffff, 0x00000000, 0x3277ffff,
                               0x4387ffff);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsx.w.ph", 0x00000000, 0x00000000, 0x00000004,
                               0x00000005);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsx.w.ph", 0xffffffff, 0xffffffff, 0x80000000,
                               0x80000000);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsx.w.ph", 0xeeeeffff, 0x00002345, 0x3277ffff,
                               0x4387ffff);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsx.w.ph", 0xffffaaaa, 0x12340000, 0x3277ffff,
                               0x4387ffff);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsx.w.ph", 0x00000000, 0x00000000, 0x80000000,
                               0x80000000);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsx.w.ph", 0xffffffff, 0x80008000, 0x80000000,
                               0x80000000);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsx.w.ph", 0x00000000, 0x00000000, 0x80000004,
                               0x00000005);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsx.w.ph", 0xffffffff, 0xffffffff, 0x80008000,
                               0x4387ffff);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsx.w.ph", 0xeeeeffff, 0x00002345, 0x80008000,
                               0x80008000);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsx.w.ph", 0xffffaaaa, 0x12340000, 0x3277ffff,
                               0x4387ffff);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsx.w.ph", 0x00000000, 0x00000000, 0x80008000,
                               0x80008000);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsx.w.ph", 0xffffffff, 0x00000000, 0x3277ffff,
                               0x4387ffff);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsx.w.ph", 0x80000000, 0xffff0000, 0x3277eeee,
                               0x80008000);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsx.w.ph", 0xffffffff, 0x00000000, 0x80000000,
                               0x80000000);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsx.w.ph", 0x0000ffff, 0xffff0000, 0x3277eeee,
                               0x4387ffff);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsx.w.ph", 0xffffffff, 0x0000ffff, 0x80008000,
                               0x4387ffff);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsx.w.ph", 0xffff1234, 0x00000000, 0xffff3277,
                               0x4387ffff);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsx.w.ph", 0x5678ffff, 0x3277ffff, 0x80000000,
                               0x80000000);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsx.w.ph", 0xffffffff, 0x00000000, 0xffff3277,
                               0xffff6543);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsx.w.ph", 0xffffffff, 0xffffffff, 0xffffffff,
                               0xffffffff);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsx.w.ph", 0xffffbbbb, 0xeeee0000, 0x80008000,
                               0x4387ffff);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsx.w.ph", 0xffffffff, 0x34560000, 0x3277ffff,
                               0x4387cccc);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsx.w.ph", 0xbbbbffff, 0x0000ffff, 0xeeeeffff,
                               0x4387ffff);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsx.w.ph", 0x12345678, 0xffffffff, 0xffffffff,
                               0xffffffff);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsx.w.ph", 0xffffbbbb, 0x12345678, 0x80008000,
                               0x4387ffff);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsx.w.ph", 0xffffffff, 0x34560000, 0x87654321,
                               0x80008000);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsx.w.ph", 0xbbbbffff, 0x0000ffff, 0xeeeeffff,
                               0x80000000);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsx.w.ph", 0xffffaaaa, 0x12340000, 0x3277ffff,
                               0x4387ffff);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsx.w.ph", 0x80000000, 0x00000000, 0x80008000,
                               0x80008000);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsx.w.ph", 0xffffffff, 0x80008000, 0x3277ffff,
                               0x4387ffff);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsx.w.ph", 0x0000ffff, 0xffff0000, 0x3277eeee,
                               0x4387ffff);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsx.w.ph", -24, 120, -24, 120);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsx.w.ph", 140, 120, 140, 120);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsx.w.ph", 0x00000004, 1073741824, 0x00000004,
                               1073741824);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsx.w.ph", (1<<31)+1, (1<<31)+2, (1<<31)+1,
                               (1<<31)+2);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsx.w.ph", 0x80002431, 0x4b908000, 0x80002431,
                               0x4b908000);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsx.w.ph", 0x004d8000, 0x800027cc, 0x004d8000,
                               0x800027cc);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsx.w.ph", 0xf6a3fa3c, 0x083b3571, 0xf6a3fa3c,
                               0x083b3571);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsx.w.ph", 0xbf17fb9a, 0xb9743941, 0xbf17fb9a,
                               0xb9743941);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsx.w.ph", 0x2c0bd024, 0xbce5f924, 0x2c0bd024,
                               0xbce5f924);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsx.w.ph", 0x3e976e2e, 0xcc3c201c, 0x3e976e2e,
                               0xcc3c201c);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsx.w.ph", 0xb4bfb365, 0x1ebaf88e, 0xb4bfb365,
                               0x1ebaf88e);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsx.w.ph", 0x288593c0, 0x722d5e20, 0x288593c0,
                               0x722d5e20);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsx.w.ph", 0x4d7ff5b4, 0xa1d6f791, 0x4d7ff5b4,
                               0xa1d6f791);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsx.w.ph", 0x4557be13, 0x7b11bee7, 0x4557be13,
                               0x7b11bee7);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsx.w.ph", 0xadcf5772, 0xa5631488, 0xadcf5772,
                               0xa5631488);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsx.w.ph", 0x989a7235, 0xb10bcc65, 0x989a7235,
                               0xb10bcc65);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsx.w.ph", 0x4d6f393a, 0x73f39fca, 0x4d6f393a,
                               0x73f39fca);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsx.w.ph", 0x24a3291e, 0x5648e540, 0x24a3291e,
                               0x5648e540);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsx.w.ph", 0xdd91eebf, 0xc54f79e6, 0xdd91eebf,
                               0xc54f79e6);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsx.w.ph", 0xf7ce2ec6, 0x5fc92974, 0xf7ce2ec6,
                               0x5fc92974);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsx.w.ph", 0xbc1083e8, 0x7e08184e, 0xbc1083e8,
                               0x7e08184e);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsx.w.ph", 0xa617cc31, 0x71c8315f, 0xa617cc31,
                               0x71c8315f);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsx.w.ph", 0xdfe1e8f0, 0x9493110e, 0xdfe1e8f0,
                               0x9493110e);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsx.w.ph", 0x31458a23, 0xbb246228, 0x31458a23,
                               0xbb246228);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsx.w.ph", 0x848af791, 0x339d8d88, 0x848af791,
                               0x339d8d88);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsx.w.ph", 0xda3bacdc, 0x70974249, 0xda3bacdc,
                               0x70974249);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsx.w.ph", 0x649d5cbd, 0x8a8d4e7d, 0x649d5cbd,
                               0x8a8d4e7d);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsx.w.ph", 0xc0c8c881, 0xeb1b4335, 0xc0c8c881,
                               0xeb1b4335);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsx.w.ph", 0x7dd81a20, 0x0cd6b508, 0x7dd81a20,
                               0x0cd6b508);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsx.w.ph", 0x00000000, 0x6731e282, 0x00000000,
                               0x6731e282);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsx.w.ph", 0xffffffff, 0xb6edf28f, 0xffffffff,
                               0xb6edf28f);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsx.w.ph", 0x00000000, 0x4b4ec9ca, 0x00000000,
                               0x4b4ec9ca);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsx.w.ph", 0xffffffff, 0xc1037fa4, 0xffffffff,
                               0xc1037fa4);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsx.w.ph", 0xffffffff, 0xcb4ab48f, 0xffffffff,
                               0xcb4ab48f);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsx.w.ph", 0xffffffff, 0xaf8f7e18, 0xffffffff,
                               0xaf8f7e18);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsx.w.ph", 0xffffffff, 0x87df4510, 0xffffffff,
                               0x87df4510);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsx.w.ph", 0xffffffff, 0xabf4e8e1, 0xffffffff,
                               0xabf4e8e1);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsx.w.ph", 0xffffffff, 0xf4c0eeac, 0xffffffff,
                               0xf4c0eeac);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsx.w.ph", 0x00000000, 0x006a54f2, 0x00000000,
                               0x006a54f2);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsx.w.ph", 0x00000000, 0x79f74493, 0x00000000,
                               0x79f74493);
   TESTDSPINST_AC_RS_RT_NODSPC("dpsx.w.ph", 0xffffffff, 0x9c09e313, 0xffffffff,
                               0x9c09e313);

   printf("-------- MUL.PH --------\n");
   TESTDSPINST_RD_RS_RT_DSPC("mul.ph $t0, $t1, $t2", 0x00000000, 0x00000000, t0,
                             t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("mul.ph $t2, $t3, $t4", 0x00045fb2, 0x00000286, t2,
                             t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("mul.ph $t4, $t1, $t5", 0x00002435, 0xffff3421, t4,
                             t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("mul.ph $t6, $t7, $t3", 0x07654cb8, 0x734680bc, t6,
                             t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("mul.ph $t5, $t3, $t2", 0xf973437b, 0x80000000, t5,
                             t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("mul.ph $t2, $t4, $t8", 0x00010001, 0xffffffff, t2,
                             t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("mul.ph $t0, $t8, $t0", 0x7fff7fff, 0x7fff7fff, t0,
                             t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("mul.ph $t4, $t6, $t1", 0x0000c420, 0x00000555, t4,
                             t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("mul.ph $t2, $t3, $t4", 0x00000004, 1073741824, t2,
                             t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("mul.ph $t4, $t1, $t5", 0x80002435, 0x80003421, t4,
                             t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("mul.ph $t6, $t7, $t3", 0x76548000, 0x73468000, t6,
                             t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("mul.ph $t5, $t3, $t2", 0x80000000, 0x80000000, t5,
                             t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("mul.ph $t2, $t4, $t8", 0x00010001, 0xffffffff, t2,
                             t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("mul.ph $t0, $t8, $t0", 0x7fff7fff, 0x7fff7fff, t0,
                             t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("mul.ph $t4, $t6, $t1", 0x0000c420, 0x00000555, t4,
                             t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("mul.ph $t0, $t1, $t2", 0x00000000, 0x00000000, t0,
                             t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("mul.ph $t2, $t3, $t4", 0x80000000, 0x80000000, t2,
                             t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("mul.ph $t4, $t1, $t5", 0xaaaaaaaa, 0x55555555, t4,
                             t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("mul.ph $t6, $t7, $t3", 0x00000018, 0xffff2435, t6,
                             t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("mul.ph $t5, $t3, $t2", 0xbabababa, 0xabababab, t5,
                             t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("mul.ph $t2, $t4, $t8", 0xf0f0f0f0, 0xfc79b4d2, t2,
                             t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("mul.ph $t0, $t8, $t0", 0xfbde3976, 0x00000000, t0,
                             t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("mul.ph $t4, $t6, $t1", 0x23534870, 0x00354565, t4,
                             t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("mul.ph $t0, $t1, $t2", 0x980b7cde, 0x00086755, t0,
                             t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("mul.ph $t2, $t3, $t4", 0x00000018, 0x8f8f8f8f, t2,
                             t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("mul.ph $t4, $t1, $t5", 0x92784656, 0xeeeeeeee, t4,
                             t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("mul.ph $t6, $t7, $t3", 0xcacacaca, 0x1bdbdbdb, t6,
                             t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("mul.ph $t5, $t3, $t2", 0xbacabaca, 0xdecadeca, t5,
                             t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("mul.ph $t2, $t4, $t8", 0x12fadeb4, 0x93474bde, t2,
                             t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("mul.ph $t0, $t8, $t0", 0x7c000790, 0xfc0007ff, t0,
                             t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("mul.ph $t4, $t6, $t1", 0xffffffff, 0xffffffff, t4,
                             t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("mul.ph $t0, $t1, $t2", 0xf2f4df1f, 0xcb4ab48f, t0,
                             t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("mul.ph $t2, $t3, $t4", 0x435f909a, 0xaf8f7e18, t2,
                             t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("mul.ph $t4, $t1, $t5", 0x2106ba5f, 0x87df4510, t4,
                             t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("mul.ph $t6, $t7, $t3", 0x246a6376, 0xabf4e8e1, t6,
                             t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("mul.ph $t5, $t3, $t2", 0x1046a1a3, 0xf4c0eeac, t5,
                             t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("mul.ph $t2, $t4, $t8", 0x638ca515, 0x006a54f2, t2,
                             t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("mul.ph $t0, $t8, $t0", 0xf63e7a9d, 0x79f74493, t0,
                             t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("mul.ph $t4, $t6, $t1", 0xbd6845cd, 0x9c09e313, t4,
                             t6, t1);

   printf("-------- MUL_S.PH --------\n");
   TESTDSPINST_RD_RS_RT_DSPC("mul_s.ph $t0, $t1, $t2", 0x00000000, 0x00000000,
                             t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("mul_s.ph $t2, $t3, $t4", 0x00045fb2, 0x00000286,
                             t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("mul_s.ph $t4, $t1, $t5", 0x00002435, 0xffff3421,
                             t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("mul_s.ph $t6, $t7, $t3", 0x07654cb8, 0x734680bc,
                             t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("mul_s.ph $t5, $t3, $t2", 0xf973437b, 0x80000000,
                             t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("mul_s.ph $t2, $t4, $t8", 0x00010001, 0xffffffff,
                             t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("mul_s.ph $t0, $t8, $t0", 0x7fff7fff, 0x7fff7fff,
                             t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("mul_s.ph $t4, $t6, $t1", 0x0000c420, 0x00000555,
                             t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("mul_s.ph $t2, $t3, $t4", 0x00000004, 1073741824,
                             t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("mul_s.ph $t4, $t1, $t5", 0x80002435, 0x80003421,
                             t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("mul_s.ph $t6, $t7, $t3", 0x76548000, 0x73468000,
                             t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("mul_s.ph $t5, $t3, $t2", 0x80000000, 0x80000000,
                             t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("mul_s.ph $t2, $t4, $t8", 0x00010001, 0xffffffff,
                             t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("mul_s.ph $t0, $t8, $t0", 0x7fff7fff, 0x7fff7fff,
                             t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("mul_s.ph $t4, $t6, $t1", 0x0000c420, 0x00000555,
                             t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("mul_s.ph $t0, $t1, $t2", 0x00000000, 0x00000000,
                             t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("mul_s.ph $t2, $t3, $t4", 0x80000000, 0x80000000,
                             t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("mul_s.ph $t4, $t1, $t5", 0xaaaaaaaa, 0x55555555,
                             t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("mul_s.ph $t6, $t7, $t3", 0x00000018, 0xffff2435,
                             t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("mul_s.ph $t5, $t3, $t2", 0xbabababa, 0xabababab,
                             t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("mul_s.ph $t2, $t4, $t8", 0xf0f0f0f0, 0xfc79b4d2,
                             t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("mul_s.ph $t0, $t8, $t0", 0xfbde3976, 0x00000000,
                             t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("mul_s.ph $t4, $t6, $t1", 0x23534870, 0x00354565,
                             t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("mul_s.ph $t0, $t1, $t2", 0x980b7cde, 0x00086755,
                             t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("mul_s.ph $t2, $t3, $t4", 0x00000018, 0x8f8f8f8f,
                             t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("mul_s.ph $t4, $t1, $t5", 0x92784656, 0xeeeeeeee,
                             t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("mul_s.ph $t6, $t7, $t3", 0xcacacaca, 0x1bdbdbdb,
                             t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("mul_s.ph $t5, $t3, $t2", 0xbacabaca, 0xdecadeca,
                             t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("mul_s.ph $t2, $t4, $t8", 0x12fadeb4, 0x93474bde,
                             t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("mul_s.ph $t0, $t8, $t0", 0x7c000790, 0xfc0007ff,
                             t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("mul_s.ph $t4, $t6, $t1", 0xffffffff, 0xffffffff,
                             t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("mul_s.ph $t0, $t1, $t2", 0xf2f4df1f, 0xcb4ab48f,
                             t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("mul_s.ph $t2, $t3, $t4", 0x435f909a, 0xaf8f7e18,
                             t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("mul_s.ph $t4, $t1, $t5", 0x2106ba5f, 0x87df4510,
                             t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("mul_s.ph $t6, $t7, $t3", 0x246a6376, 0xabf4e8e1,
                             t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("mul_s.ph $t5, $t3, $t2", 0x1046a1a3, 0xf4c0eeac,
                             t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("mul_s.ph $t2, $t4, $t8", 0x638ca515, 0x006a54f2,
                             t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("mul_s.ph $t0, $t8, $t0", 0xf63e7a9d, 0x79f74493,
                             t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("mul_s.ph $t4, $t6, $t1", 0xbd6845cd, 0x9c09e313,
                             t4, t6, t1);

   printf("-------- MULQ_S.PH --------\n");
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.ph $t0, $t1, $t2", 0x00000000, 0x00000000,
                             t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.ph $t2, $t3, $t4", 0x00045fb2, 0x00000286,
                             t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.ph $t4, $t1, $t5", 0x00002435, 0xffff3421,
                             t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.ph $t6, $t7, $t3", 0x07654cb8, 0x734680bc,
                             t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.ph $t5, $t3, $t2", 0xf973437b, 0x80000000,
                             t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.ph $t2, $t4, $t8", 0x00010001, 0xffffffff,
                             t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.ph $t0, $t8, $t0", 0x7fff7fff, 0x7fff7fff,
                             t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.ph $t4, $t6, $t1", 0x0000c420, 0x00000555,
                             t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.ph $t2, $t3, $t4", 0x00000004, 1073741824,
                             t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.ph $t4, $t1, $t5", 0x80002435, 0x80003421,
                             t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.ph $t6, $t7, $t3", 0x76548000, 0x73468000,
                             t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.ph $t5, $t3, $t2", 0x80000000, 0x80000000,
                             t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.ph $t2, $t4, $t8", 0x00010001, 0xffffffff,
                             t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.ph $t0, $t8, $t0", 0x7fff7fff, 0x7fff7fff,
                             t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.ph $t4, $t6, $t1", 0x0000c420, 0x00000555,
                             t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.ph $t0, $t1, $t2", 0x00000000, 0x00000000,
                             t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.ph $t2, $t3, $t4", 0x80000000, 0x80000000,
                             t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.ph $t4, $t1, $t5", 0xaaaaaaaa, 0x55555555,
                             t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.ph $t6, $t7, $t3", 0x00000018, 0xffff2435,
                             t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.ph $t5, $t3, $t2", 0xbabababa, 0xabababab,
                             t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.ph $t2, $t4, $t8", 0xf0f0f0f0, 0xfc79b4d2,
                             t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.ph $t0, $t8, $t0", 0xfbde3976, 0x00000000,
                             t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.ph $t4, $t6, $t1", 0x23534870, 0x00354565,
                             t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.ph $t0, $t1, $t2", 0x980b7cde, 0x00086755,
                             t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.ph $t2, $t3, $t4", 0x00000018, 0x8f8f8f8f,
                             t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.ph $t4, $t1, $t5", 0x92784656, 0xeeeeeeee,
                             t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.ph $t6, $t7, $t3", 0xcacacaca, 0x1bdbdbdb,
                             t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.ph $t5, $t3, $t2", 0xbacabaca, 0xdecadeca,
                             t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.ph $t2, $t4, $t8", 0x12fadeb4, 0x93474bde,
                             t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.ph $t0, $t8, $t0", 0x7c000790, 0xfc0007ff,
                             t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.ph $t4, $t6, $t1", 0xffffffff, 0xffffffff,
                             t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.ph $t0, $t1, $t2", 0xf2f4df1f, 0xcb4ab48f,
                             t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.ph $t2, $t3, $t4", 0x435f909a, 0xaf8f7e18,
                             t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.ph $t4, $t1, $t5", 0x2106ba5f, 0x87df4510,
                             t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.ph $t6, $t7, $t3", 0x246a6376, 0xabf4e8e1,
                             t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.ph $t5, $t3, $t2", 0x1046a1a3, 0xf4c0eeac,
                             t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.ph $t2, $t4, $t8", 0x638ca515, 0x006a54f2,
                             t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.ph $t0, $t8, $t0", 0xf63e7a9d, 0x79f74493,
                             t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.ph $t4, $t6, $t1", 0xbd6845cd, 0x9c09e313,
                             t4, t6, t1);

   printf("-------- MULQ_RS.W --------\n");
   TESTDSPINST_RD_RS_RT_DSPC("mulq_rs.w $t0, $t1, $t2", 0x00000000, 0x00000000,
                             t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_rs.w $t2, $t3, $t4", 0x00045fb2, 0x00000286,
                             t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_rs.w $t4, $t1, $t5", 0x00002435, 0xffff3421,
                             t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_rs.w $t6, $t7, $t3", 0x07654cb8, 0x734680bc,
                             t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_rs.w $t5, $t3, $t2", 0xf973437b, 0x80000000,
                             t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_rs.w $t2, $t4, $t8", 0x00010001, 0xffffffff,
                             t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_rs.w $t0, $t8, $t0", 0x7fff7fff, 0x7fff7fff,
                             t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_rs.w $t4, $t6, $t1", 0x0000c420, 0x00000555,
                             t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_rs.w $t2, $t3, $t4", 0x00000004, 1073741824,
                             t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_rs.w $t4, $t1, $t5", 0x80002435, 0x80003421,
                             t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_rs.w $t6, $t7, $t3", 0x76548000, 0x73468000,
                             t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_rs.w $t5, $t3, $t2", 0x80000000, 0x80000000,
                             t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_rs.w $t2, $t4, $t8", 0x00010001, 0xffffffff,
                             t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_rs.w $t0, $t8, $t0", 0x7fff7fff, 0x7fff7fff,
                             t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_rs.w $t4, $t6, $t1", 0x0000c420, 0x00000555,
                             t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_rs.w $t0, $t1, $t2", 0x00000000, 0x00000000,
                             t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_rs.w $t2, $t3, $t4", 0x80000000, 0x80000000,
                             t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_rs.w $t4, $t1, $t5", 0xaaaaaaaa, 0x55555555,
                             t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_rs.w $t6, $t7, $t3", 0x00000018, 0xffff2435,
                             t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_rs.w $t5, $t3, $t2", 0xbabababa, 0xabababab,
                             t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_rs.w $t2, $t4, $t8", 0xf0f0f0f0, 0xfc79b4d2,
                             t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_rs.w $t0, $t8, $t0", 0xfbde3976, 0x00000000,
                             t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_rs.w $t4, $t6, $t1", 0x23534870, 0x00354565,
                             t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_rs.w $t0, $t1, $t2", 0x980b7cde, 0x00086755,
                             t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_rs.w $t2, $t3, $t4", 0x00000018, 0x8f8f8f8f,
                             t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_rs.w $t4, $t1, $t5", 0x92784656, 0xeeeeeeee,
                             t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_rs.w $t6, $t7, $t3", 0xcacacaca, 0x1bdbdbdb,
                             t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_rs.w $t5, $t3, $t2", 0xbacabaca, 0xdecadeca,
                             t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_rs.w $t2, $t4, $t8", 0x12fadeb4, 0x93474bde,
                             t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_rs.w $t0, $t8, $t0", 0x7c000790, 0xfc0007ff,
                             t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_rs.w $t4, $t6, $t1", 0xffffffff, 0xffffffff,
                             t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_rs.w $t0, $t1, $t2", 0xf2f4df1f, 0xcb4ab48f,
                             t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_rs.w $t2, $t3, $t4", 0x435f909a, 0xaf8f7e18,
                             t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_rs.w $t4, $t1, $t5", 0x2106ba5f, 0x87df4510,
                             t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_rs.w $t6, $t7, $t3", 0x246a6376, 0xabf4e8e1,
                             t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_rs.w $t5, $t3, $t2", 0x1046a1a3, 0xf4c0eeac,
                             t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_rs.w $t2, $t4, $t8", 0x638ca515, 0x006a54f2,
                             t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_rs.w $t0, $t8, $t0", 0xf63e7a9d, 0x79f74493,
                             t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_rs.w $t4, $t6, $t1", 0xbd6845cd, 0x9c09e313,
                             t4, t6, t1);

   printf("-------- MULQ_S.W --------\n");
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.w $t0, $t1, $t2", 0x00000000, 0x00000000,
                             t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.w $t2, $t3, $t4", 0x00045fb2, 0x00000286,
                             t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.w $t4, $t1, $t5", 0x00002435, 0xffff3421,
                             t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.w $t6, $t7, $t3", 0x07654cb8, 0x734680bc,
                             t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.w $t5, $t3, $t2", 0xf973437b, 0x80000000,
                             t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.w $t2, $t4, $t8", 0x00010001, 0xffffffff,
                             t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.w $t0, $t8, $t0", 0x7fff7fff, 0x7fff7fff,
                             t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.w $t4, $t6, $t1", 0x0000c420, 0x00000555,
                             t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.w $t2, $t3, $t4", 0x00000004, 1073741824,
                             t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.w $t4, $t1, $t5", 0x80002435, 0x80003421,
                             t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.w $t6, $t7, $t3", 0x76548000, 0x73468000,
                             t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.w $t5, $t3, $t2", 0x80000000, 0x80000000,
                             t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.w $t2, $t4, $t8", 0x00010001, 0xffffffff,
                             t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.w $t0, $t8, $t0", 0x7fff7fff, 0x7fff7fff,
                             t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.w $t4, $t6, $t1", 0x0000c420, 0x00000555,
                             t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.w $t0, $t1, $t2", 0x00000000, 0x00000000,
                             t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.w $t2, $t3, $t4", 0x80000000, 0x80000000,
                             t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.w $t4, $t1, $t5", 0xaaaaaaaa, 0x55555555,
                             t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.w $t6, $t7, $t3", 0x00000018, 0xffff2435,
                             t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.w $t5, $t3, $t2", 0xbabababa, 0xabababab,
                             t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.w $t2, $t4, $t8", 0xf0f0f0f0, 0xfc79b4d2,
                             t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.w $t0, $t8, $t0", 0xfbde3976, 0x00000000,
                             t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.w $t4, $t6, $t1", 0x23534870, 0x00354565,
                             t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.w $t0, $t1, $t2", 0x980b7cde, 0x00086755,
                             t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.w $t2, $t3, $t4", 0x00000018, 0x8f8f8f8f,
                             t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.w $t4, $t1, $t5", 0x92784656, 0xeeeeeeee,
                             t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.w $t6, $t7, $t3", 0xcacacaca, 0x1bdbdbdb,
                             t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.w $t5, $t3, $t2", 0xbacabaca, 0xdecadeca,
                             t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.w $t2, $t4, $t8", 0x12fadeb4, 0x93474bde,
                             t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.w $t0, $t8, $t0", 0x7c000790, 0xfc0007ff,
                             t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.w $t4, $t6, $t1", 0xffffffff, 0xffffffff,
                             t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.w $t0, $t1, $t2", 0xf2f4df1f, 0xcb4ab48f,
                             t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.w $t2, $t3, $t4", 0x435f909a, 0xaf8f7e18,
                             t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.w $t4, $t1, $t5", 0x2106ba5f, 0x87df4510,
                             t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.w $t6, $t7, $t3", 0x246a6376, 0xabf4e8e1,
                             t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.w $t5, $t3, $t2", 0x1046a1a3, 0xf4c0eeac,
                             t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.w $t2, $t4, $t8", 0x638ca515, 0x006a54f2,
                             t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.w $t0, $t8, $t0", 0xf63e7a9d, 0x79f74493,
                             t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("mulq_s.w $t4, $t6, $t1", 0xbd6845cd, 0x9c09e313,
                             t4, t6, t1);

   printf("-------- MULSA.W.PH --------\n");
   TESTDSPINST_AC_RS_RT_NODSPC("mulsa.w.ph", 0xffffffff, 0x00000000, 0x3277ffff,
                               0x4387ffff);
   TESTDSPINST_AC_RS_RT_NODSPC("mulsa.w.ph", 0x00000000, 0x00000000, 0x00000004,
                               0x00000005);
   TESTDSPINST_AC_RS_RT_NODSPC("mulsa.w.ph", 0xffffffff, 0xffffffff, 0x80000000,
                               0x80000000);
   TESTDSPINST_AC_RS_RT_NODSPC("mulsa.w.ph", 0xeeeeffff, 0x00002345, 0x3277ffff,
                               0x4387ffff);
   TESTDSPINST_AC_RS_RT_NODSPC("mulsa.w.ph", 0xffffaaaa, 0x12340000, 0x3277ffff,
                               0x4387ffff);
   TESTDSPINST_AC_RS_RT_NODSPC("mulsa.w.ph", 0x00000000, 0x00000000, 0x80000000,
                               0x80000000);
   TESTDSPINST_AC_RS_RT_NODSPC("mulsa.w.ph", 0xffffffff, 0x80008000, 0x80000000,
                               0x80000000);
   TESTDSPINST_AC_RS_RT_NODSPC("mulsa.w.ph", 0x00000000, 0x00000000, 0x80000004,
                               0x00000005);
   TESTDSPINST_AC_RS_RT_NODSPC("mulsa.w.ph", 0xffffffff, 0xffffffff, 0x80008000,
                               0x4387ffff);
   TESTDSPINST_AC_RS_RT_NODSPC("mulsa.w.ph", 0xeeeeffff, 0x00002345, 0x80008000,
                               0x80008000);
   TESTDSPINST_AC_RS_RT_NODSPC("mulsa.w.ph", 0xffffaaaa, 0x12340000, 0x3277ffff,
                               0x4387ffff);
   TESTDSPINST_AC_RS_RT_NODSPC("mulsa.w.ph", 0x00000000, 0x00000000, 0x80008000,
                               0x80008000);
   TESTDSPINST_AC_RS_RT_NODSPC("mulsa.w.ph", 0xffffffff, 0x00000000, 0x3277ffff,
                               0x4387ffff);
   TESTDSPINST_AC_RS_RT_NODSPC("mulsa.w.ph", 0x80000000, 0xffff0000, 0x3277eeee,
                               0x80008000);
   TESTDSPINST_AC_RS_RT_NODSPC("mulsa.w.ph", 0xffffffff, 0x00000000, 0x80000000,
                               0x80000000);
   TESTDSPINST_AC_RS_RT_NODSPC("mulsa.w.ph", 0x0000ffff, 0xffff0000, 0x3277eeee,
                               0x4387ffff);
   TESTDSPINST_AC_RS_RT_NODSPC("mulsa.w.ph", 0xffffffff, 0x0000ffff, 0x80008000,
                               0x4387ffff);
   TESTDSPINST_AC_RS_RT_NODSPC("mulsa.w.ph", 0xffff1234, 0x00000000, 0xffff3277,
                               0x4387ffff);
   TESTDSPINST_AC_RS_RT_NODSPC("mulsa.w.ph", 0x5678ffff, 0x3277ffff, 0x80000000,
                               0x80000000);
   TESTDSPINST_AC_RS_RT_NODSPC("mulsa.w.ph", 0xffffffff, 0x00000000, 0xffff3277,
                               0xffff6543);
   TESTDSPINST_AC_RS_RT_NODSPC("mulsa.w.ph", 0xffffffff, 0xffffffff, 0xffffffff,
                               0xffffffff);
   TESTDSPINST_AC_RS_RT_NODSPC("mulsa.w.ph", 0xffffbbbb, 0xeeee0000, 0x80008000,
                               0x4387ffff);
   TESTDSPINST_AC_RS_RT_NODSPC("mulsa.w.ph", 0xffffffff, 0x34560000, 0x3277ffff,
                               0x4387cccc);
   TESTDSPINST_AC_RS_RT_NODSPC("mulsa.w.ph", 0xbbbbffff, 0x0000ffff, 0xeeeeffff,
                               0x4387ffff);
   TESTDSPINST_AC_RS_RT_NODSPC("mulsa.w.ph", 0x12345678, 0xffffffff, 0xffffffff,
                               0xffffffff);
   TESTDSPINST_AC_RS_RT_NODSPC("mulsa.w.ph", 0xffffbbbb, 0x12345678, 0x80008000,
                               0x4387ffff);
   TESTDSPINST_AC_RS_RT_NODSPC("mulsa.w.ph", 0xffffffff, 0x34560000, 0x87654321,
                               0x80008000);
   TESTDSPINST_AC_RS_RT_NODSPC("mulsa.w.ph", 0xbbbbffff, 0x0000ffff, 0xeeeeffff,
                               0x80000000);
   TESTDSPINST_AC_RS_RT_NODSPC("mulsa.w.ph", 0xffffaaaa, 0x12340000, 0x3277ffff,
                               0x4387ffff);
   TESTDSPINST_AC_RS_RT_NODSPC("mulsa.w.ph", 0x80000000, 0x00000000, 0x80008000,
                               0x80008000);
   TESTDSPINST_AC_RS_RT_NODSPC("mulsa.w.ph", 0xffffffff, 0x80008000, 0x3277ffff,
                               0x4387ffff);
   TESTDSPINST_AC_RS_RT_NODSPC("mulsa.w.ph", 0x0000ffff, 0xffff0000, 0x3277eeee,
                               0x4387ffff);
   TESTDSPINST_AC_RS_RT_NODSPC("mulsa.w.ph", -24, 120, -24, 120);
   TESTDSPINST_AC_RS_RT_NODSPC("mulsa.w.ph", 140, 120, 140, 120);
   TESTDSPINST_AC_RS_RT_NODSPC("mulsa.w.ph", 0x00000004, 1073741824, 0x00000004,
                               1073741824);
   TESTDSPINST_AC_RS_RT_NODSPC("mulsa.w.ph", (1<<31)+1, (1<<31)+2, (1<<31)+1,
                               (1<<31)+2);
   TESTDSPINST_AC_RS_RT_NODSPC("mulsa.w.ph", 0x80002431, 0x4b908000, 0x80002431,
                               0x4b908000);
   TESTDSPINST_AC_RS_RT_NODSPC("mulsa.w.ph", 0x004d8000, 0x800027cc, 0x004d8000,
                               0x800027cc);
   TESTDSPINST_AC_RS_RT_NODSPC("mulsa.w.ph", 0xf6a3fa3c, 0x083b3571, 0xf6a3fa3c,
                               0x083b3571);
   TESTDSPINST_AC_RS_RT_NODSPC("mulsa.w.ph", 0xbf17fb9a, 0xb9743941, 0xbf17fb9a,
                               0xb9743941);
   TESTDSPINST_AC_RS_RT_NODSPC("mulsa.w.ph", 0x2c0bd024, 0xbce5f924, 0x2c0bd024,
                               0xbce5f924);
   TESTDSPINST_AC_RS_RT_NODSPC("mulsa.w.ph", 0x3e976e2e, 0xcc3c201c, 0x3e976e2e,
                               0xcc3c201c);
   TESTDSPINST_AC_RS_RT_NODSPC("mulsa.w.ph", 0xb4bfb365, 0x1ebaf88e, 0xb4bfb365,
                               0x1ebaf88e);
   TESTDSPINST_AC_RS_RT_NODSPC("mulsa.w.ph", 0x288593c0, 0x722d5e20, 0x288593c0,
                               0x722d5e20);
   TESTDSPINST_AC_RS_RT_NODSPC("mulsa.w.ph", 0x4d7ff5b4, 0xa1d6f791, 0x4d7ff5b4,
                               0xa1d6f791);
   TESTDSPINST_AC_RS_RT_NODSPC("mulsa.w.ph", 0x4557be13, 0x7b11bee7, 0x4557be13,
                               0x7b11bee7);
   TESTDSPINST_AC_RS_RT_NODSPC("mulsa.w.ph", 0xadcf5772, 0xa5631488, 0xadcf5772,
                               0xa5631488);
   TESTDSPINST_AC_RS_RT_NODSPC("mulsa.w.ph", 0x989a7235, 0xb10bcc65, 0x989a7235,
                               0xb10bcc65);
   TESTDSPINST_AC_RS_RT_NODSPC("mulsa.w.ph", 0x4d6f393a, 0x73f39fca, 0x4d6f393a,
                               0x73f39fca);
   TESTDSPINST_AC_RS_RT_NODSPC("mulsa.w.ph", 0x24a3291e, 0x5648e540, 0x24a3291e,
                               0x5648e540);
   TESTDSPINST_AC_RS_RT_NODSPC("mulsa.w.ph", 0xdd91eebf, 0xc54f79e6, 0xdd91eebf,
                               0xc54f79e6);
   TESTDSPINST_AC_RS_RT_NODSPC("mulsa.w.ph", 0xf7ce2ec6, 0x5fc92974, 0xf7ce2ec6,
                               0x5fc92974);
   TESTDSPINST_AC_RS_RT_NODSPC("mulsa.w.ph", 0xbc1083e8, 0x7e08184e, 0xbc1083e8,
                               0x7e08184e);
   TESTDSPINST_AC_RS_RT_NODSPC("mulsa.w.ph", 0xa617cc31, 0x71c8315f, 0xa617cc31,
                               0x71c8315f);
   TESTDSPINST_AC_RS_RT_NODSPC("mulsa.w.ph", 0xdfe1e8f0, 0x9493110e, 0xdfe1e8f0,
                               0x9493110e);
   TESTDSPINST_AC_RS_RT_NODSPC("mulsa.w.ph", 0x31458a23, 0xbb246228, 0x31458a23,
                               0xbb246228);
   TESTDSPINST_AC_RS_RT_NODSPC("mulsa.w.ph", 0x848af791, 0x339d8d88, 0x848af791,
                               0x339d8d88);
   TESTDSPINST_AC_RS_RT_NODSPC("mulsa.w.ph", 0xda3bacdc, 0x70974249, 0xda3bacdc,
                               0x70974249);
   TESTDSPINST_AC_RS_RT_NODSPC("mulsa.w.ph", 0x649d5cbd, 0x8a8d4e7d, 0x649d5cbd,
                               0x8a8d4e7d);
   TESTDSPINST_AC_RS_RT_NODSPC("mulsa.w.ph", 0xc0c8c881, 0xeb1b4335, 0xc0c8c881,
                               0xeb1b4335);
   TESTDSPINST_AC_RS_RT_NODSPC("mulsa.w.ph", 0x7dd81a20, 0x0cd6b508, 0x7dd81a20,
                               0x0cd6b508);
   TESTDSPINST_AC_RS_RT_NODSPC("mulsa.w.ph", 0x00000000, 0x6731e282, 0x00000000,
                               0x6731e282);
   TESTDSPINST_AC_RS_RT_NODSPC("mulsa.w.ph", 0xffffffff, 0xb6edf28f, 0xffffffff,
                               0xb6edf28f);
   TESTDSPINST_AC_RS_RT_NODSPC("mulsa.w.ph", 0x00000000, 0x4b4ec9ca, 0x00000000,
                               0x4b4ec9ca);
   TESTDSPINST_AC_RS_RT_NODSPC("mulsa.w.ph", 0xffffffff, 0xc1037fa4, 0xffffffff,
                               0xc1037fa4);
   TESTDSPINST_AC_RS_RT_NODSPC("mulsa.w.ph", 0xffffffff, 0xcb4ab48f, 0xffffffff,
                               0xcb4ab48f);
   TESTDSPINST_AC_RS_RT_NODSPC("mulsa.w.ph", 0xffffffff, 0xaf8f7e18, 0xffffffff,
                               0xaf8f7e18);
   TESTDSPINST_AC_RS_RT_NODSPC("mulsa.w.ph", 0xffffffff, 0x87df4510, 0xffffffff,
                               0x87df4510);
   TESTDSPINST_AC_RS_RT_NODSPC("mulsa.w.ph", 0xffffffff, 0xabf4e8e1, 0xffffffff,
                               0xabf4e8e1);
   TESTDSPINST_AC_RS_RT_NODSPC("mulsa.w.ph", 0xffffffff, 0xf4c0eeac, 0xffffffff,
                               0xf4c0eeac);
   TESTDSPINST_AC_RS_RT_NODSPC("mulsa.w.ph", 0x00000000, 0x006a54f2, 0x00000000,
                               0x006a54f2);
   TESTDSPINST_AC_RS_RT_NODSPC("mulsa.w.ph", 0x00000000, 0x79f74493, 0x00000000,
                               0x79f74493);
   TESTDSPINST_AC_RS_RT_NODSPC("mulsa.w.ph", 0xffffffff, 0x9c09e313, 0xffffffff,
                               0x9c09e313);

   printf("-------- PRECR.QB.PH --------\n");
   TESTDSPINST_RD_RS_RT_NODSPC("precr.qb.ph $t0, $t1, $t2", 0x00000000,
                               0x00000000, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("precr.qb.ph $t2, $t3, $t4", 0x045fb232,
                               0x00028632, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("precr.qb.ph $t4, $t1, $t5", 0xfabc3435,
                               0xfabc3421, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("precr.qb.ph $t6, $t7, $t3", 0x07654cb8,
                               0x734680bc, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("precr.qb.ph $t5, $t3, $t2", 0xf973437b,
                               0x80000000, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("precr.qb.ph $t2, $t4, $t8", 0x00ff0001,
                               0xff01ffff, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("precr.qb.ph $t0, $t8, $t0", 0x7fff7fff,
                               0x7fff7fff, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("precr.qb.ph $t4, $t6, $t1", 0x0000c420,
                               0x00000555, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("precr.qb.ph $t0, $t1, $t2", 0x00000000,
                               0x00000000, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("precr.qb.ph $t2, $t3, $t4", 0x80000000,
                               0x80000000, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("precr.qb.ph $t4, $t1, $t5", 0xaaaaaaaa,
                               0x55555555, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("precr.qb.ph $t6, $t7, $t3", 0x00000018,
                               0xffff2435, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("precr.qb.ph $t5, $t3, $t2", 0xbabababa,
                               0xabababab, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("precr.qb.ph $t2, $t4, $t8", 0xf0f0f0f0,
                               0xfc79b4d2, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("precr.qb.ph $t0, $t8, $t0", 0xfbde3976,
                               0x00000000, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("precr.qb.ph $t4, $t6, $t1", 0x23534870,
                               0x00354565, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("precr.qb.ph $t0, $t1, $t2", 0x980b7cde,
                               0x00086755, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("precr.qb.ph $t2, $t3, $t4", 0x00000018,
                               0x8f8f8f8f, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("precr.qb.ph $t4, $t1, $t5", 0x92784656,
                               0xeeeeeeee, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("precr.qb.ph $t6, $t7, $t3", 0xcacacaca,
                               0x1bdbdbdb, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("precr.qb.ph $t5, $t3, $t2", 0xbacabaca,
                               0xdecadeca, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("precr.qb.ph $t2, $t4, $t8", 0x12fadeb4,
                               0x93474bde, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("precr.qb.ph $t0, $t8, $t0", 0x7c000790,
                               0xfc0007ff, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("precr.qb.ph $t4, $t6, $t1", 0xffffffff,
                               0xffffffff, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("precr.qb.ph $t0, $t1, $t2", 0xf2f4df1f,
                               0xcb4ab48f, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("precr.qb.ph $t2, $t3, $t4", 0x435f909a,
                               0xaf8f7e18, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("precr.qb.ph $t4, $t1, $t5", 0x2106ba5f,
                               0x87df4510, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("precr.qb.ph $t6, $t7, $t3", 0x246a6376,
                               0xabf4e8e1, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("precr.qb.ph $t5, $t3, $t2", 0x1046a1a3,
                               0xf4c0eeac, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("precr.qb.ph $t2, $t4, $t8", 0x638ca515,
                               0x006a54f2, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("precr.qb.ph $t0, $t8, $t0", 0xf63e7a9d,
                               0x79f74493, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("precr.qb.ph $t4, $t6, $t1", 0xbd6845cd,
                               0x9c09e313, t4, t6, t1);

   printf("-------- PRECR_SRA.PH.W --------\n");
   TESTDSPINST_RT_RS_SA_NODSPC("precr_sra.ph.w $t0, $t1,  0", 0x00000000,
                               0x00000000,  0, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("precr_sra.ph.w $t2, $t3,  1", 0x045fb232,
                               0x00028632,  1, t2, t3);
   TESTDSPINST_RT_RS_SA_NODSPC("precr_sra.ph.w $t4, $t1,  4", 0xfabc3435,
                               0xfabc3421,  4, t4, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("precr_sra.ph.w $t6, $t7, 17", 0x07654cb8,
                               0x734680bc, 17, t6, t7);
   TESTDSPINST_RT_RS_SA_NODSPC("precr_sra.ph.w $t5, $t3, 31", 0xf973437b,
                               0x80000000, 31, t5, t3);
   TESTDSPINST_RT_RS_SA_NODSPC("precr_sra.ph.w $t2, $t4,  8", 0x00ff0001,
                               0xff01ffff,  8, t2, t4);
   TESTDSPINST_RT_RS_SA_NODSPC("precr_sra.ph.w $t0, $t8, 11", 0x7fff7fff,
                               0x7fff7fff, 11, t0, t8);
   TESTDSPINST_RT_RS_SA_NODSPC("precr_sra.ph.w $t4, $t6, 13", 0x0000c420,
                               0x00000555, 13, t4, t6);
   TESTDSPINST_RT_RS_SA_NODSPC("precr_sra.ph.w $t0, $t1,  2", 0x00000000,
                               0x00000000,  2, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("precr_sra.ph.w $t2, $t3,  6", 0x80000000,
                               0x80000000,  6, t2, t3);
   TESTDSPINST_RT_RS_SA_NODSPC("precr_sra.ph.w $t4, $t1,  7", 0xaaaaaaaa,
                               0x55555555,  7, t4, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("precr_sra.ph.w $t6, $t7, 19", 0x00000018,
                               0xffff2435, 19, t6, t7);
   TESTDSPINST_RT_RS_SA_NODSPC("precr_sra.ph.w $t5, $t3, 31", 0xbabababa,
                               0xabababab, 31, t5, t3);
   TESTDSPINST_RT_RS_SA_NODSPC("precr_sra.ph.w $t2, $t4,  4", 0xf0f0f0f0,
                               0xfc79b4d2,  4, t2, t4);
   TESTDSPINST_RT_RS_SA_NODSPC("precr_sra.ph.w $t0, $t8, 12", 0xfbde3976,
                               0x00000000, 12, t0, t8);
   TESTDSPINST_RT_RS_SA_NODSPC("precr_sra.ph.w $t4, $t6, 10", 0x23534870,
                               0x00354565, 10, t4, t6);
   TESTDSPINST_RT_RS_SA_NODSPC("precr_sra.ph.w $t0, $t1, 20", 0x980b7cde,
                               0x00086755, 20, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("precr_sra.ph.w $t2, $t3, 21", 0x00000018,
                               0x8f8f8f8f, 21, t2, t3);
   TESTDSPINST_RT_RS_SA_NODSPC("precr_sra.ph.w $t4, $t1, 24", 0x92784656,
                               0xeeeeeeee, 24, t4, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("precr_sra.ph.w $t6, $t7, 27", 0xcacacaca,
                               0x1bdbdbdb, 27, t6, t7);
   TESTDSPINST_RT_RS_SA_NODSPC("precr_sra.ph.w $t5, $t3,  1", 0xbacabaca,
                               0xdecadeca,  1, t5, t3);
   TESTDSPINST_RT_RS_SA_NODSPC("precr_sra.ph.w $t2, $t4, 18", 0x12fadeb4,
                               0x93474bde, 18, t2, t4);
   TESTDSPINST_RT_RS_SA_NODSPC("precr_sra.ph.w $t0, $t8, 10", 0x7c000790,
                               0xfc0007ff, 10, t0, t8);
   TESTDSPINST_RT_RS_SA_NODSPC("precr_sra.ph.w $t4, $t6, 16", 0xffffffff,
                               0xffffffff, 16, t4, t6);
   TESTDSPINST_RT_RS_SA_NODSPC("precr_sra.ph.w $t0, $t1,  0", 0xf2f4df1f,
                               0xcb4ab48f,  0, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("precr_sra.ph.w $t2, $t3, 14", 0x435f909a,
                               0xaf8f7e18, 14, t2, t3);
   TESTDSPINST_RT_RS_SA_NODSPC("precr_sra.ph.w $t4, $t1,  5", 0x2106ba5f,
                               0x87df4510,  5, t4, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("precr_sra.ph.w $t6, $t7,  7", 0x246a6376,
                               0xabf4e8e1,  7, t6, t7);
   TESTDSPINST_RT_RS_SA_NODSPC("precr_sra.ph.w $t5, $t3,  9", 0x1046a1a3,
                               0xf4c0eeac,  9, t5, t3);
   TESTDSPINST_RT_RS_SA_NODSPC("precr_sra.ph.w $t2, $t4,  3", 0x638ca515,
                               0x006a54f2,  3, t2, t4);
   TESTDSPINST_RT_RS_SA_NODSPC("precr_sra.ph.w $t0, $t8, 15", 0xf63e7a9d,
                               0x79f74493, 15, t0, t8);
   TESTDSPINST_RT_RS_SA_NODSPC("precr_sra.ph.w $t4, $t6, 11", 0xbd6845cd,
                               0x9c09e313, 11, t4, t6);

   printf("-------- PRECR_SRA_R.PH.W --------\n");
   TESTDSPINST_RT_RS_SA_NODSPC("precr_sra_r.ph.w $t0, $t1,  0", 0x00000000,
                               0x00000000,  0, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("precr_sra_r.ph.w $t2, $t3,  1", 0x045fb232,
                               0x00028632,  1, t2, t3);
   TESTDSPINST_RT_RS_SA_NODSPC("precr_sra_r.ph.w $t4, $t1,  4", 0xfabc3435,
                               0xfabc3421,  4, t4, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("precr_sra_r.ph.w $t6, $t7, 17", 0x07654cb8,
                               0x734680bc, 17, t6, t7);
   TESTDSPINST_RT_RS_SA_NODSPC("precr_sra_r.ph.w $t5, $t3, 31", 0xf973437b,
                               0x80000000, 31, t5, t3);
   TESTDSPINST_RT_RS_SA_NODSPC("precr_sra_r.ph.w $t2, $t4,  8", 0x00ff0001,
                               0xff01ffff,  8, t2, t4);
   TESTDSPINST_RT_RS_SA_NODSPC("precr_sra_r.ph.w $t0, $t8, 11", 0x7fff7fff,
                               0x7fff7fff, 11, t0, t8);
   TESTDSPINST_RT_RS_SA_NODSPC("precr_sra_r.ph.w $t4, $t6, 13", 0x0000c420,
                               0x00000555, 13, t4, t6);
   TESTDSPINST_RT_RS_SA_NODSPC("precr_sra_r.ph.w $t0, $t1,  2", 0x00000000,
                               0x00000000,  2, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("precr_sra_r.ph.w $t2, $t3,  6", 0x80000000,
                               0x80000000,  6, t2, t3);
   TESTDSPINST_RT_RS_SA_NODSPC("precr_sra_r.ph.w $t4, $t1,  7", 0xaaaaaaaa,
                               0x55555555,  7, t4, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("precr_sra_r.ph.w $t6, $t7, 19", 0x00000018,
                               0xffff2435, 19, t6, t7);
   TESTDSPINST_RT_RS_SA_NODSPC("precr_sra_r.ph.w $t5, $t3, 31", 0xbabababa,
                               0xabababab, 31, t5, t3);
   TESTDSPINST_RT_RS_SA_NODSPC("precr_sra_r.ph.w $t2, $t4,  4", 0xf0f0f0f0,
                               0xfc79b4d2,  4, t2, t4);
   TESTDSPINST_RT_RS_SA_NODSPC("precr_sra_r.ph.w $t0, $t8, 12", 0xfbde3976,
                               0x00000000, 12, t0, t8);
   TESTDSPINST_RT_RS_SA_NODSPC("precr_sra_r.ph.w $t4, $t6, 10", 0x23534870,
                               0x00354565, 10, t4, t6);
   TESTDSPINST_RT_RS_SA_NODSPC("precr_sra_r.ph.w $t0, $t1, 20", 0x980b7cde,
                               0x00086755, 20, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("precr_sra_r.ph.w $t2, $t3, 21", 0x00000018,
                               0x8f8f8f8f, 21, t2, t3);
   TESTDSPINST_RT_RS_SA_NODSPC("precr_sra_r.ph.w $t4, $t1, 24", 0x92784656,
                               0xeeeeeeee, 24, t4, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("precr_sra_r.ph.w $t6, $t7, 27", 0xcacacaca,
                               0x1bdbdbdb, 27, t6, t7);
   TESTDSPINST_RT_RS_SA_NODSPC("precr_sra_r.ph.w $t5, $t3,  1", 0xbacabaca,
                               0xdecadeca,  1, t5, t3);
   TESTDSPINST_RT_RS_SA_NODSPC("precr_sra_r.ph.w $t2, $t4, 18", 0x12fadeb4,
                               0x93474bde, 18, t2, t4);
   TESTDSPINST_RT_RS_SA_NODSPC("precr_sra_r.ph.w $t0, $t8, 10", 0x7c000790,
                               0xfc0007ff, 10, t0, t8);
   TESTDSPINST_RT_RS_SA_NODSPC("precr_sra_r.ph.w $t4, $t6, 16", 0xffffffff,
                               0xffffffff, 16, t4, t6);
   TESTDSPINST_RT_RS_SA_NODSPC("precr_sra_r.ph.w $t0, $t1,  0", 0xf2f4df1f,
                               0xcb4ab48f,  0, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("precr_sra_r.ph.w $t2, $t3, 14", 0x435f909a,
                               0xaf8f7e18, 14, t2, t3);
   TESTDSPINST_RT_RS_SA_NODSPC("precr_sra_r.ph.w $t4, $t1,  5", 0x2106ba5f,
                               0x87df4510,  5, t4, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("precr_sra_r.ph.w $t6, $t7,  7", 0x246a6376,
                               0xabf4e8e1,  7, t6, t7);
   TESTDSPINST_RT_RS_SA_NODSPC("precr_sra_r.ph.w $t5, $t3,  9", 0x1046a1a3,
                               0xf4c0eeac,  9, t5, t3);
   TESTDSPINST_RT_RS_SA_NODSPC("precr_sra_r.ph.w $t2, $t4,  3", 0x638ca515,
                               0x006a54f2,  3, t2, t4);
   TESTDSPINST_RT_RS_SA_NODSPC("precr_sra_r.ph.w $t0, $t8, 15", 0xf63e7a9d,
                               0x79f74493, 15, t0, t8);
   TESTDSPINST_RT_RS_SA_NODSPC("precr_sra_r.ph.w $t4, $t6, 11", 0xbd6845cd,
                               0x9c09e313, 11, t4, t6);

   printf("-------- PREPEND --------\n");
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  3", 0xffff86fc, 0xfffffe02,
                               3, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  0", 0x00000000, 0xfffffe06,
                               0, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  3", 0x7fffffff, 0x80000000,
                               3, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  0", 0x80000000, 0x00000000,
                               0, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  0", 0x00000000, 0xffffff23,
                               0, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  7", 0xff76947a, 0x00000000,
                               7, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  0", 0x00000035, 0xffffffc0,
                               0, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  0", 0x80000000, 0x01130b02,
                               0, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  0", 0x7ffffffe, 0xc0000001,
                               0, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  0", 0x7fffffff, 0x7fffffff,
                               0, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  0", 0x1a75980e, 0x1b4c3c1e,
                               0, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  1", 0xffffffff, 0x7ffffffc,
                               1, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 13", 0xc0000003, 0x00000000,
                               13, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  1", 0xfffffffe, 0x00000000,
                               1, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 31", 0x1ffffffe, 0x7fffffff,
                               31, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  0", 0x00000000, 0x00000000,
                               0, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 31", 0x7fffffff, 0xfffffda8,
                               31, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 12", 0xc0000007, 0xfffff6b2,
                               12, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  1", 0x002a923e, 0x1ffffff8,
                               1, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 31", 0xfffffee4, 0x00000010,
                               31, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 31", 0xf8034c6b, 0xfb766d64,
                               31, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  4", 0x7ffffffb, 0x0000002b,
                               4, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  0", 0xfffffffa, 0x7fffffff,
                               0, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  9", 0x55555555, 0x7fffffff,
                               9, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 18", 0x80000000, 0x80000005,
                               18, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  0", 0x7fffffff, 0x000004fc,
                               0, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 31", 0x0035d189, 0x80000000,
                               31, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  0", 0x8000007f, 0xff00ff00,
                               0, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 12", 0x00000000, 0x03557af6,
                               12, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  0", 0x000000e3, 0x0008f4ab,
                               0, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  2", 0x00000000, 0x00000000,
                               2, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  3", 0x0f0f0f0f, 0x00000003,
                               3, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  0", 0x002559bf, 0xdb6db6db,
                               0, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  2", 0x66666666, 0x7fffff80,
                               2, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 30", 0x80000000, 0xfa17edf5,
                               30, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  7", 0xc0000001, 0x00003cb5,
                               7, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  0", 0xc71c71c7, 0x00000335,
                               0, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  0", 0x8000001f, 0xfffffef3,
                               0, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 12", 0x00000006, 0x80000002,
                               12, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 15", 0xffffffd5, 0xf8c02863,
                               15, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 25", 0x33299df8, 0x000011fb,
                               25, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 12", 0x00000000, 0xc000000f,
                               12, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 31", 0x00000001, 0x00000000,
                               31, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  5", 0x7ffffffb, 0x7ffffffd,
                               5, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 25", 0x80000000, 0x7fffffff,
                               25, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  4", 0x0ffffffc, 0x0ffffffc,
                               4, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 31", 0xfffffe6a, 0xc000001f,
                               31, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  2", 0xfeb380e9, 0x7ffffffd,
                               2, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  1", 0x7ffffffa, 0x00000000,
                               1, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  2", 0x8000000f, 0xdb6db6db,
                               2, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 24", 0xfffd232e, 0x00001548,
                               24, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  1", 0xc000001f, 0x00000000,
                               1, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 22", 0x33333333, 0x00000000,
                               22, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 30", 0x0000db1b, 0xc0000001,
                               30, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 10", 0xffffffff, 0xe0000007,
                               10, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  1", 0x00000000, 0xffffff5f,
                               1, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 31", 0x0d2efcd1, 0x8000003f,
                               31, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  7", 0x00000000, 0xffffff6d,
                               7, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  1", 0x00081a70, 0xe4606c5a,
                               1, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 31", 0x80000000, 0xe0000003,
                               31, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  0", 0x00000000, 0xfff9ab12,
                               0, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 31", 0x80000000, 0x7fffffff,
                               31, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 17", 0xffffd894, 0x00000000,
                               17, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 31", 0x0ffffffe, 0xfff6ab08,
                               31, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 30", 0x80000000, 0x00000000,
                               30, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 14", 0x7fffffff, 0x00000006,
                               14, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  2", 0x80000000, 0x7fffffff,
                               2, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 10", 0xfffff23b, 0x00000000,
                               10, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 31", 0x80000000, 0x80000000,
                               31, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  0", 0x00000011, 0x7fffffff,
                               0, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  1", 0xfffffffa, 0x1c71c71c,
                               1, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  1", 0xcccccccc, 0xf8000001,
                               1, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 31", 0x80000000, 0x80000000,
                               31, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  0", 0xfdb739b8, 0x000000e1,
                               0, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 19", 0x7fffffff, 0x80000000,
                               19, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  1", 0xfffff216, 0x80000000,
                               1, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 31", 0x3ffffff8, 0x00000000,
                               31, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  0", 0xfea7a4f9, 0xfffffffe,
                               0, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 31", 0x7fffffff, 0x7fffffff,
                               31, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 25", 0x7fffffff, 0x00000006,
                               25, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  8", 0x7fffffff, 0x8e38e38e,
                               8, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 31", 0x00000000, 0xfffffffe,
                               31, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  0", 0x7fffffff, 0x00000000,
                               0, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 30", 0x80000000, 0xffffffd3,
                               30, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  0", 0xf0000001, 0x00000006,
                               0, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 21", 0xfffffff8, 0x00000002,
                               21, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  8", 0x66666666, 0x49249249,
                               8, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 31", 0x7fffffff, 0x000000ff,
                               31, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  0", 0x1ffffff8, 0x80000001,
                               0, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 31", 0xe0000003, 0xffffffcb,
                               31, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  2", 0xfffb2342, 0x0000001f,
                               2, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  1", 0x80000000, 0xfffffffe,
                               1, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  0", 0x03fffffe, 0x00000c17,
                               0, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  1", 0x00000000, 0xc0000003,
                               1, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 13", 0x03fffffe, 0xfffd39d5,
                               13, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  6", 0x0003192a, 0x0000d270,
                               6, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 21", 0x3ffffffe, 0x00000000,
                               21, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  2", 0x80000000, 0x80000000,
                               2, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 31", 0x1ffffff0, 0xff62154e,
                               31, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 13", 0x7fffffff, 0xffffff2e,
                               13, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  1", 0xf8000001, 0x80000000,
                               1, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 27", 0x7fffffff, 0xe0000003,
                               27, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 15", 0xe000000f, 0x0000368b,
                               15, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 25", 0x7fffffff, 0x80000000,
                               25, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 10", 0x0000029c, 0x00001460,
                               10, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 19", 0xfdd559c5, 0x000acd42,
                               19, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  3", 0x3ffffff8, 0x0081575f,
                               3, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  0", 0xffffed07, 0x00000000,
                               0, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  3", 0x7fffffff, 0x00000002,
                               3, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  6", 0x3fffffe0, 0x07fffffe,
                               6, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  0", 0x000a559a, 0x7fffffff,
                               0, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  1", 0x1e4c379a, 0x00000ae6,
                               1, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  1", 0x00000001, 0xfffffff0,
                               1, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  4", 0x0000bcb1, 0xfffffffe,
                               4, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  1", 0x002192fc, 0x7fffffff,
                               1, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  1", 0x00000000, 0xfffff7e4,
                               1, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  0", 0x7ffffff0, 0x00000000,
                               0, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  2", 0x7ffffff8, 0x80000007,
                               2, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 31", 0xfffffffa, 0xfffffffa,
                               31, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  0", 0x7ffffffa, 0x7fffffff,
                               0, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  0", 0x00000000, 0x7ffffff0,
                               0, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 31", 0x8000001f, 0x000000ff,
                               31, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 27", 0x00000000, 0x00000000,
                               27, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  1", 0x00107408, 0x07fffffc,
                               1, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 31", 0x7fffffff, 0x7fffffff,
                               31, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 31", 0xaaaaaaaa, 0xffffe12e,
                               31, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 14", 0xf8000001, 0x00000000,
                               14, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  0", 0x29fc2bb3, 0x80000000,
                               0, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 27", 0x000036c7, 0xfffffd48,
                               27, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  0", 0xffffffba, 0x0f0f0f0f,
                               0, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 30", 0xffffc303, 0x80000005,
                               30, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  7", 0xffff0000, 0x00000003,
                               7, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 29", 0xf0000003, 0xfffffffe,
                               29, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 31", 0x00000005, 0xfff4bf84,
                               31, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  0", 0xfffcc6a5, 0x7fffffff,
                               0, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  0", 0xe0000003, 0x66666666,
                               0, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  3", 0x00000000, 0x00000827,
                               3, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  4", 0x80000000, 0x7fffffff,
                               4, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  0", 0xe0000003, 0x80000000,
                               0, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 25", 0x80000000, 0x92492492,
                               25, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  0", 0x000001f9, 0x1ce10bb3,
                               0, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 17", 0x00000000, 0x3ffffffc,
                               17, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 30", 0xfffffffe, 0x00001ca3,
                               30, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 27", 0x00000de7, 0xfff0eda7,
                               27, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  1", 0x0ffffffc, 0x00000000,
                               1, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  1", 0x01e63dae, 0x8000007f,
                               1, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  3", 0x80000000, 0x000072d7,
                               3, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 31", 0x000d0717, 0xfffffffa,
                               31, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  9", 0xfffffff8, 0x80000000,
                               9, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  0", 0x00000000, 0x00000000,
                               0, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 31", 0x00006000, 0xf0000003,
                               31, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  7", 0x7fffffff, 0x07fffffc,
                               7, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 10", 0x7fffffff, 0x00000000,
                               10, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  1", 0xffffffd8, 0x00000003,
                               1, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 10", 0x7fffffff, 0x7fffffff,
                               10, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  0", 0x0000003f, 0x0000003f,
                               0, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  8", 0xfe67cb2a, 0x00000000,
                               8, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 11", 0x0000a3af, 0x7fffffff,
                               11, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  0", 0x80000000, 0xe0000003,
                               0, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  9", 0x33333333, 0x00000007,
                               9, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 22", 0x00000022, 0xfffffff1,
                               22, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  4", 0xffffe940, 0x00a6984a,
                               4, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  6", 0x80000000, 0x00000000,
                               6, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  0", 0xffffe59b, 0x80000000,
                               0, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  5", 0x80000000, 0x80000000,
                               5, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 22", 0x00000000, 0x7fffffc0,
                               22, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 14", 0x00000334, 0x8000007f,
                               14, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  0", 0x7ffffff0, 0xfffffffe,
                               0, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  4", 0xff00ff00, 0x00000000,
                               4, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 17", 0x3ffffffe, 0x80000000,
                               17, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 31", 0x80000000, 0x80000000,
                               31, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  8", 0x2b9440fc, 0x00000001,
                               8, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 10", 0x00000002, 0xffffd5c8,
                               10, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  0", 0xfff138af, 0x80000000,
                               0, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 16", 0x00000000, 0x00000000,
                               16, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  8", 0x00000002, 0xe38e38e3,
                               8, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  0", 0xf4911d04, 0x8000007f,
                               0, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  0", 0x7ffffff0, 0x00000000,
                               0, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  8", 0xffe21573, 0xffffe109,
                               8, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  0", 0x7fffffff, 0xc890ef17,
                               0, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  6", 0x80000001, 0xfffdb214,
                               6, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 28", 0xfee9b599, 0x00000000,
                               28, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  1", 0xffffe956, 0x0000016c,
                               1, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  9", 0xffee2d5e, 0x00000000,
                               9, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  0", 0xfffffffe, 0x00000000,
                               0, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  7", 0xfffff66c, 0xfffff66c,
                               7, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 15", 0x00000fb4, 0x80000000,
                               15, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  3", 0x00000000, 0x7fffffff,
                               3, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  0", 0x00000735, 0xffffffff,
                               0, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  3", 0x80000000, 0x1ffffffc,
                               3, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 24", 0xffffb926, 0x00000000,
                               24, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  0", 0x00002b8d, 0x8000007f,
                               0, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  2", 0x00000000, 0x00000017,
                               2, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  0", 0x80000000, 0xff9a0952,
                               0, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  0", 0x0f76e7cf, 0x0f76e7cf,
                               0, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 31", 0x7fffffff, 0x1c71c71c,
                               31, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  6", 0x80000001, 0xfffffffa,
                               6, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 10", 0xfffff747, 0x0000000f,
                               10, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 10", 0xe0000007, 0xe0000007,
                               10, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 27", 0xffff6c3a, 0xffffffed,
                               27, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 29", 0x7fffffff, 0x00008c98,
                               29, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  0", 0x80000004, 0x00000000,
                               0, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  0", 0x00000128, 0x0000003c,
                               0, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  3", 0x00161769, 0x00de5fe1,
                               3, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 22", 0xffffff80, 0xf0000007,
                               22, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 15", 0x00000e92, 0x7fffffff,
                               15, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 31", 0x00000000, 0x0000ffff,
                               31, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 31", 0xe0000001, 0x7fffffff,
                               31, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  6", 0x7fffffff, 0x00000000,
                               6, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 31", 0xfffffe0c, 0xfffffe0c,
                               31, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  1", 0x80000000, 0x00084488,
                               1, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  2", 0x80000000, 0x7fffffff,
                               2, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 31", 0x80000000, 0x80000000,
                               31, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 14", 0x00000000, 0xfffffffe,
                               14, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 31", 0xc71c71c7, 0x00000000,
                               31, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 15", 0x80000000, 0x00000000,
                               15, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  1", 0x07fffffc, 0x0001594b,
                               1, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  8", 0x80000000, 0x80000000,
                               8, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  0", 0x1ffffff0, 0x8000007f,
                               0, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  2", 0xffca0d15, 0x7ffffff8,
                               2, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 31", 0xfffffffc, 0x00000000,
                               31, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  4", 0x00000002, 0x80000000,
                               4, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 22", 0x80000000, 0x00293c72,
                               22, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 10", 0xffffff58, 0xffffff80,
                               10, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  3", 0x00ff00ff, 0x80000000,
                               3, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 31", 0xffffff80, 0xf0f0f0f0,
                               31, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 19", 0x000037a4, 0x0012b63d,
                               19, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  7", 0x3ffffff8, 0xfffffff8,
                               7, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 29", 0x00000000, 0xffff4929,
                               29, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  1", 0xfffffff0, 0xf8000003,
                               1, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 24", 0x00000000, 0x7fffffff,
                               24, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 15", 0x00000007, 0x00000009,
                               15, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 15", 0xffffff9d, 0xffffffe0,
                               15, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 25", 0xfffe3d99, 0x80000000,
                               25, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  7", 0x00000000, 0x00000000,
                               7, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 31", 0x80000005, 0xfef34c96,
                               31, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 29", 0xffffffe8, 0x7fffffff,
                               29, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 31", 0x00000000, 0x0ffffff8,
                               31, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  0", 0x7ffffffe, 0xff0fceda,
                               0, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  0", 0x00000000, 0x0002e398,
                               0, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  0", 0x00000000, 0x00000000,
                               0, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t2, $t3,  1", 0x045fb232, 0x00028632,
                               1, t2, t3);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t4, $t1,  4", 0xfabc3435, 0xfabc3421,
                               4, t4, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t6, $t7, 17", 0x07654cb8, 0x734680bc,
                               17, t6, t7);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t5, $t3, 31", 0xf973437b, 0x80000000,
                               31, t5, t3);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t2, $t4,  8", 0x00ff0001, 0xff01ffff,
                               8, t2, t4);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t8, 11", 0x7fff7fff, 0x7fff7fff,
                               11, t0, t8);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t4, $t6, 13", 0x0000c420, 0x00000555,
                               13, t4, t6);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  2", 0x00000000, 0x00000000,
                               2, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t2, $t3,  6", 0x80000000, 0x80000000,
                               6, t2, t3);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t4, $t1,  7", 0xaaaaaaaa, 0x55555555,
                               7, t4, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t6, $t7, 19", 0x00000018, 0xffff2435,
                               19, t6, t7);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t5, $t3, 31", 0xbabababa, 0xabababab,
                               31, t5, t3);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t2, $t4,  4", 0xf0f0f0f0, 0xfc79b4d2,
                               4, t2, t4);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t8, 12", 0xfbde3976, 0x00000000,
                               12, t0, t8);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t4, $t6, 10", 0x23534870, 0x00354565,
                               10, t4, t6);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1, 20", 0x980b7cde, 0x00086755,
                               20, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t2, $t3, 21", 0x00000018, 0x8f8f8f8f,
                               21, t2, t3);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t4, $t1, 24", 0x92784656, 0xeeeeeeee,
                               24, t4, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t6, $t7, 27", 0xcacacaca, 0x1bdbdbdb,
                               27, t6, t7);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t5, $t3,  1", 0xbacabaca, 0xdecadeca,
                               1, t5, t3);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t2, $t4, 18", 0x12fadeb4, 0x93474bde,
                               18, t2, t4);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t8, 10", 0x7c000790, 0xfc0007ff,
                               10, t0, t8);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t4, $t6, 16", 0xffffffff, 0xffffffff,
                               16, t4, t6);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t1,  0", 0xf2f4df1f, 0xcb4ab48f,
                               0, t0, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t2, $t3, 14", 0x435f909a, 0xaf8f7e18,
                               14, t2, t3);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t4, $t1,  5", 0x2106ba5f, 0x87df4510,
                               5, t4, t1);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t6, $t7,  7", 0x246a6376, 0xabf4e8e1,
                               7, t6, t7);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t5, $t3,  9", 0x1046a1a3, 0xf4c0eeac,
                               9, t5, t3);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t2, $t4,  3", 0x638ca515, 0x006a54f2,
                               3, t2, t4);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t0, $t8, 15", 0xf63e7a9d, 0x79f74493,
                               15, t0, t8);
   TESTDSPINST_RT_RS_SA_NODSPC("prepend $t4, $t6, 11", 0xbd6845cd, 0x9c09e313,
                               11, t4, t6);

   printf("-------- SHRA.QB --------\n");
   TESTDSPINST_RD_RT_SA_NODSPC("shra.qb $t0, $t1, 1", 0x00000000, 1, t0, t1);
   TESTDSPINST_RD_RT_SA_NODSPC("shra.qb $t2, $t3, 2", 0x00028632, 2, t2, t3);
   TESTDSPINST_RD_RT_SA_NODSPC("shra.qb $t4, $t1, 3", 0xfabc3421, 3, t4, t1);
   TESTDSPINST_RD_RT_SA_NODSPC("shra.qb $t6, $t7, 4", 0x734680bc, 4, t6, t7);
   TESTDSPINST_RD_RT_SA_NODSPC("shra.qb $t5, $t3, 0", 0x80000000, 0, t5, t3);
   TESTDSPINST_RD_RT_SA_NODSPC("shra.qb $t2, $t4, 7", 0xff01ffff, 7, t2, t4);
   TESTDSPINST_RD_RT_SA_NODSPC("shra.qb $t0, $t8, 7", 0x7fff7fff, 7, t0, t8);
   TESTDSPINST_RD_RT_SA_NODSPC("shra.qb $t4, $t6, 0", 0x00000555, 0, t4, t6);
   TESTDSPINST_RD_RT_SA_NODSPC("shra.qb $t0, $t1, 1", 0x00000000, 1, t0, t1);
   TESTDSPINST_RD_RT_SA_NODSPC("shra.qb $t2, $t3, 2", 0x80000000, 2, t2, t3);
   TESTDSPINST_RD_RT_SA_NODSPC("shra.qb $t4, $t1, 3", 0x55555555, 3, t4, t1);
   TESTDSPINST_RD_RT_SA_NODSPC("shra.qb $t6, $t7, 4", 0xffff2435, 4, t6, t7);
   TESTDSPINST_RD_RT_SA_NODSPC("shra.qb $t5, $t3, 5", 0xabababab, 5, t5, t3);
   TESTDSPINST_RD_RT_SA_NODSPC("shra.qb $t2, $t4, 6", 0xfc79b4d2, 6, t2, t4);
   TESTDSPINST_RD_RT_SA_NODSPC("shra.qb $t0, $t8, 7", 0x00000000, 7, t0, t8);
   TESTDSPINST_RD_RT_SA_NODSPC("shra.qb $t4, $t6, 0", 0x00354565, 0, t4, t6);
   TESTDSPINST_RD_RT_SA_NODSPC("shra.qb $t0, $t1, 1", 0x00086755, 1, t0, t1);
   TESTDSPINST_RD_RT_SA_NODSPC("shra.qb $t2, $t3, 2", 0x8f8f8f8f, 2, t2, t3);
   TESTDSPINST_RD_RT_SA_NODSPC("shra.qb $t4, $t1, 3", 0xeeeeeeee, 3, t4, t1);
   TESTDSPINST_RD_RT_SA_NODSPC("shra.qb $t6, $t7, 4", 0x1bdbdbdb, 4, t6, t7);
   TESTDSPINST_RD_RT_SA_NODSPC("shra.qb $t5, $t3, 5", 0xdecadeca, 5, t5, t3);
   TESTDSPINST_RD_RT_SA_NODSPC("shra.qb $t2, $t4, 6", 0x93474bde, 6, t2, t4);
   TESTDSPINST_RD_RT_SA_NODSPC("shra.qb $t0, $t8, 7", 0xfc0007ff, 7, t0, t8);
   TESTDSPINST_RD_RT_SA_NODSPC("shra.qb $t4, $t6, 0", 0xffffffff, 0, t4, t6);
   TESTDSPINST_RD_RT_SA_NODSPC("shra.qb $t0, $t1, 3", 0xcb4ab48f, 3, t0, t1);
   TESTDSPINST_RD_RT_SA_NODSPC("shra.qb $t2, $t3, 4", 0xaf8f7e18, 4, t2, t3);
   TESTDSPINST_RD_RT_SA_NODSPC("shra.qb $t4, $t1, 0", 0x87df4510, 0, t4, t1);
   TESTDSPINST_RD_RT_SA_NODSPC("shra.qb $t6, $t7, 7", 0xabf4e8e1, 7, t6, t7);
   TESTDSPINST_RD_RT_SA_NODSPC("shra.qb $t5, $t3, 7", 0xf4c0eeac, 7, t5, t3);
   TESTDSPINST_RD_RT_SA_NODSPC("shra.qb $t2, $t4, 5", 0x006a54f2, 5, t2, t4);
   TESTDSPINST_RD_RT_SA_NODSPC("shra.qb $t0, $t8, 1", 0x79f74493, 1, t0, t8);
   TESTDSPINST_RD_RT_SA_NODSPC("shra.qb $t4, $t6, 2", 0x9c09e313, 2, t4, t6);

   printf("-------- SHRA_R.QB --------\n");
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.qb $t0, $t1, 1", 0x00000000, 1, t0, t1);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.qb $t2, $t3, 2", 0x00028632, 2, t2, t3);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.qb $t4, $t1, 3", 0xfabc3421, 3, t4, t1);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.qb $t6, $t7, 4", 0x734680bc, 4, t6, t7);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.qb $t5, $t3, 0", 0x80000000, 0, t5, t3);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.qb $t2, $t4, 7", 0xff01ffff, 7, t2, t4);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.qb $t0, $t8, 7", 0x7fff7fff, 7, t0, t8);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.qb $t4, $t6, 0", 0x00000555, 0, t4, t6);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.qb $t0, $t1, 1", 0x00000000, 1, t0, t1);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.qb $t2, $t3, 2", 0x80000000, 2, t2, t3);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.qb $t4, $t1, 3", 0x55555555, 3, t4, t1);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.qb $t6, $t7, 4", 0xffff2435, 4, t6, t7);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.qb $t5, $t3, 5", 0xabababab, 5, t5, t3);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.qb $t2, $t4, 6", 0xfc79b4d2, 6, t2, t4);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.qb $t0, $t8, 7", 0x00000000, 7, t0, t8);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.qb $t4, $t6, 0", 0x00354565, 0, t4, t6);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.qb $t0, $t1, 1", 0x00086755, 1, t0, t1);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.qb $t2, $t3, 2", 0x8f8f8f8f, 2, t2, t3);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.qb $t4, $t1, 3", 0xeeeeeeee, 3, t4, t1);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.qb $t6, $t7, 4", 0x1bdbdbdb, 4, t6, t7);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.qb $t5, $t3, 5", 0xdecadeca, 5, t5, t3);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.qb $t2, $t4, 6", 0x93474bde, 6, t2, t4);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.qb $t0, $t8, 7", 0xfc0007ff, 7, t0, t8);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.qb $t4, $t6, 0", 0xffffffff, 0, t4, t6);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.qb $t0, $t1, 3", 0xcb4ab48f, 3, t0, t1);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.qb $t2, $t3, 4", 0xaf8f7e18, 4, t2, t3);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.qb $t4, $t1, 0", 0x87df4510, 0, t4, t1);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.qb $t6, $t7, 7", 0xabf4e8e1, 7, t6, t7);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.qb $t5, $t3, 7", 0xf4c0eeac, 7, t5, t3);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.qb $t2, $t4, 5", 0x006a54f2, 5, t2, t4);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.qb $t0, $t8, 1", 0x79f74493, 1, t0, t8);
   TESTDSPINST_RD_RT_SA_NODSPC("shra_r.qb $t4, $t6, 2", 0x9c09e313, 2, t4, t6);

   printf("-------- SHRAV.QB --------\n");
   TESTDSPINST_RD_RT_RS_NODSPC("shrav.qb", 0x3277ffff, 0x00000001);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav.qb", 0x00000005, 0x73741802);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav.qb", 0x00000000, 0x80003403);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav.qb", 0x4387ffff, 0x73468004);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav.qb", 0x4387ffff, 0x80000000);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav.qb", 0x80000000, 0xffffff07);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav.qb", 0x80000000, 0x7fff7f07);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav.qb", 0x00000005, 0x00000505);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav.qb", 0x4387ffff, 0x00000000);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav.qb", 0x80008000, 0x80000000);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav.qb", 0x4387ffff, 0x55555555);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav.qb", 0x80008000, 0xffff2434);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav.qb", 0xffff4387, 0xabababa3);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav.qb", 0x80008000, 0xfc79b4d2);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav.qb", 0x80000000, 0x00000000);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav.qb", 0x43871234, 0x00354561);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav.qb", 0x1234ffff, 0x00086755);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav.qb", 0x4387ffff, 0x8f8f8f8f);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav.qb", 0x80000000, 0xeeeeeeee);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav.qb", 0xffff6543, 0x1bdbdbdb);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav.qb", 0xffffffff, 0xdecadeca);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav.qb", 0xffff4387, 0x93474bde);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav.qb", 0x4387cccc, 0xfc0007ff);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav.qb", 0x4387ffff, 0xffffffff);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav.qb", 0xffffffff, 0xdecadeca);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav.qb", 0x4387ffff, 0xbacabaca);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav.qb", 0x80008000, 0x3545ff80);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav.qb", 0x80000000, 0x734680bc);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav.qb", 0x4387ffff, 0xc4dbfe20);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav.qb", 0x80008000, 0x00000000);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav.qb", 0x4387ffff, 0x55555555);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav.qb", 0xffff4387, 0xad80bce4);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav.qb", 0x7fffffff, 0x00000000);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav.qb", 0x80000000, 0x00000000);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav.qb", 0xfabc3435, 0xfabc3421);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav.qb", 0x07654cb8, 0x734680bc);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav.qb", 0xf973437b, 0x80000000);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav.qb", 0x00ff0001, 0xff01ffff);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav.qb", 0x7fff7004, 0x7fff7fff);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav.qb", 0x0000c420, 0x00000555);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav.qb", 0x00000000, 0x00000000);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav.qb", 0x80000000, 0x80000000);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav.qb", 0xaaaaaaaa, 0x55555555);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav.qb", 0x00000018, 0xffff2435);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav.qb", 0xbabababa, 0xabababab);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav.qb", 0xf0f0f0f0, 0xfc79b4d2);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav.qb", 0xfbde3976, 0x00000000);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav.qb", 0x23534870, 0x00354565);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav.qb", 0x980b7cde, 0x00086755);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav.qb", 0x00000018, 0x8f8f8f8f);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav.qb", 0x92784656, 0xeeeeeeee);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav.qb", 0xcacacaca, 0x1bdbdbdb);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav.qb", 0xbacabaca, 0xdecadeca);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav.qb", 0x12fadeb4, 0x93474bde);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav.qb", 0x7c000790, 0xfc0007ff);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav.qb", 0xffffffff, 0xffffffff);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav.qb", 0xf2f4df1f, 0xcb4ab48f);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav.qb", 0x435f909a, 0xaf8f7e18);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav.qb", 0x2106ba5f, 0x87df4510);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav.qb", 0x246a6376, 0xabf4e8e1);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav.qb", 0x1046a1a3, 0xf4c0eeac);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav.qb", 0x638ca515, 0x006a54f2);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav.qb", 0xf63e7a9d, 0x79f74493);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav.qb", 0xbd6845cd, 0x9c09e313);

   printf("-------- SHRAV_R.QB --------\n");
   TESTDSPINST_RD_RT_RS_NODSPC("shrav_r.qb", 0x3277ffff, 0x00000001);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav_r.qb", 0x00000005, 0x73741802);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav_r.qb", 0x00000000, 0x80003403);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav_r.qb", 0x4387ffff, 0x73468004);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav_r.qb", 0x4387ffff, 0x80000000);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav_r.qb", 0x80000000, 0xffffff07);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav_r.qb", 0x80000000, 0x7fff7f07);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav_r.qb", 0x00000005, 0x00000505);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav_r.qb", 0x4387ffff, 0x00000000);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav_r.qb", 0x80008000, 0x80000000);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav_r.qb", 0x4387ffff, 0x55555555);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav_r.qb", 0x80008000, 0xffff2434);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav_r.qb", 0xffff4387, 0xabababa3);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav_r.qb", 0x80008000, 0xfc79b4d2);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav_r.qb", 0x80000000, 0x00000000);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav_r.qb", 0x43871234, 0x00354561);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav_r.qb", 0x1234ffff, 0x00086755);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav_r.qb", 0x4387ffff, 0x8f8f8f8f);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav_r.qb", 0x80000000, 0xeeeeeeee);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav_r.qb", 0xffff6543, 0x1bdbdbdb);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav_r.qb", 0xffffffff, 0xdecadeca);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav_r.qb", 0xffff4387, 0x93474bde);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav_r.qb", 0x4387cccc, 0xfc0007ff);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav_r.qb", 0x4387ffff, 0xffffffff);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav_r.qb", 0xffffffff, 0xdecadeca);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav_r.qb", 0x4387ffff, 0xbacabaca);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav_r.qb", 0x80008000, 0x3545ff80);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav_r.qb", 0x80000000, 0x734680bc);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav_r.qb", 0x4387ffff, 0xc4dbfe20);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav_r.qb", 0x80008000, 0x00000000);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav_r.qb", 0x4387ffff, 0x55555555);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav_r.qb", 0xffff4387, 0xad80bce4);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav_r.qb", 0x7fffffff, 0x00000000);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav_r.qb", 0x80000000, 0x00000000);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav_r.qb", 0xfabc3435, 0xfabc3421);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav_r.qb", 0x07654cb8, 0x734680bc);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav_r.qb", 0xf973437b, 0x80000000);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav_r.qb", 0x00ff0001, 0xff01ffff);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav_r.qb", 0x7fff7004, 0x7fff7fff);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav_r.qb", 0x0000c420, 0x00000555);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav_r.qb", 0x00000000, 0x00000000);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav_r.qb", 0x80000000, 0x80000000);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav_r.qb", 0xaaaaaaaa, 0x55555555);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav_r.qb", 0x00000018, 0xffff2435);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav_r.qb", 0xbabababa, 0xabababab);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav_r.qb", 0xf0f0f0f0, 0xfc79b4d2);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav_r.qb", 0xfbde3976, 0x00000000);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav_r.qb", 0x23534870, 0x00354565);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav_r.qb", 0x980b7cde, 0x00086755);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav_r.qb", 0x00000018, 0x8f8f8f8f);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav_r.qb", 0x92784656, 0xeeeeeeee);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav_r.qb", 0xcacacaca, 0x1bdbdbdb);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav_r.qb", 0xbacabaca, 0xdecadeca);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav_r.qb", 0x12fadeb4, 0x93474bde);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav_r.qb", 0x7c000790, 0xfc0007ff);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav_r.qb", 0xffffffff, 0xffffffff);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav_r.qb", 0xf2f4df1f, 0xcb4ab48f);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav_r.qb", 0x435f909a, 0xaf8f7e18);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav_r.qb", 0x2106ba5f, 0x87df4510);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav_r.qb", 0x246a6376, 0xabf4e8e1);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav_r.qb", 0x1046a1a3, 0xf4c0eeac);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav_r.qb", 0x638ca515, 0x006a54f2);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav_r.qb", 0xf63e7a9d, 0x79f74493);
   TESTDSPINST_RD_RT_RS_NODSPC("shrav_r.qb", 0xbd6845cd, 0x9c09e313);

   printf("-------- SHRL.PH --------\n");
   TESTDSPINST_RD_RT_SA_NODSPC("shrl.ph $t0, $t1,  0", 0x00000000,  0, t0, t1);
   TESTDSPINST_RD_RT_SA_NODSPC("shrl.ph $t2, $t3,  1", 0x00028632,  1, t2, t3);
   TESTDSPINST_RD_RT_SA_NODSPC("shrl.ph $t4, $t1,  2", 0xfabc3421,  2, t4, t1);
   TESTDSPINST_RD_RT_SA_NODSPC("shrl.ph $t6, $t7,  3", 0x734680bc,  3, t6, t7);
   TESTDSPINST_RD_RT_SA_NODSPC("shrl.ph $t5, $t3,  4", 0x80000000,  4, t5, t3);
   TESTDSPINST_RD_RT_SA_NODSPC("shrl.ph $t2, $t4,  5", 0xff01ffff,  5, t2, t4);
   TESTDSPINST_RD_RT_SA_NODSPC("shrl.ph $t0, $t8,  6", 0x7fff7fff,  6, t0, t8);
   TESTDSPINST_RD_RT_SA_NODSPC("shrl.ph $t4, $t6,  7", 0x00000555,  7, t4, t6);
   TESTDSPINST_RD_RT_SA_NODSPC("shrl.ph $t0, $t1,  8", 0x00000000,  8, t0, t1);
   TESTDSPINST_RD_RT_SA_NODSPC("shrl.ph $t2, $t3,  9", 0x80000000,  9, t2, t3);
   TESTDSPINST_RD_RT_SA_NODSPC("shrl.ph $t4, $t1, 10", 0x55555555, 10, t4, t1);
   TESTDSPINST_RD_RT_SA_NODSPC("shrl.ph $t6, $t7, 11", 0xffff2435, 11, t6, t7);
   TESTDSPINST_RD_RT_SA_NODSPC("shrl.ph $t5, $t3, 12", 0xabababab, 12, t5, t3);
   TESTDSPINST_RD_RT_SA_NODSPC("shrl.ph $t2, $t4, 13", 0xfc79b4d2, 13, t2, t4);
   TESTDSPINST_RD_RT_SA_NODSPC("shrl.ph $t0, $t8, 14", 0x00000000, 14, t0, t8);
   TESTDSPINST_RD_RT_SA_NODSPC("shrl.ph $t4, $t6, 15", 0x00354565, 15, t4, t6);
   TESTDSPINST_RD_RT_SA_NODSPC("shrl.ph $t0, $t1,  0", 0x00086755,  0, t0, t1);
   TESTDSPINST_RD_RT_SA_NODSPC("shrl.ph $t2, $t3,  1", 0x8f8f8f8f,  1, t2, t3);
   TESTDSPINST_RD_RT_SA_NODSPC("shrl.ph $t4, $t1,  2", 0xeeeeeeee,  2, t4, t1);
   TESTDSPINST_RD_RT_SA_NODSPC("shrl.ph $t6, $t7,  3", 0x1bdbdbdb,  3, t6, t7);
   TESTDSPINST_RD_RT_SA_NODSPC("shrl.ph $t5, $t3,  4", 0xdecadeca,  4, t5, t3);
   TESTDSPINST_RD_RT_SA_NODSPC("shrl.ph $t2, $t4,  5", 0x93474bde,  5, t2, t4);
   TESTDSPINST_RD_RT_SA_NODSPC("shrl.ph $t0, $t8,  6", 0xfc0007ff,  6, t0, t8);
   TESTDSPINST_RD_RT_SA_NODSPC("shrl.ph $t4, $t6,  7", 0xffffffff,  7, t4, t6);
   TESTDSPINST_RD_RT_SA_NODSPC("shrl.ph $t0, $t1,  8", 0xcb4ab48f,  8, t0, t1);
   TESTDSPINST_RD_RT_SA_NODSPC("shrl.ph $t2, $t3,  9", 0xaf8f7e18,  9, t2, t3);
   TESTDSPINST_RD_RT_SA_NODSPC("shrl.ph $t4, $t1, 10", 0x87df4510, 10, t4, t1);
   TESTDSPINST_RD_RT_SA_NODSPC("shrl.ph $t6, $t7, 11", 0xabf4e8e1, 11, t6, t7);
   TESTDSPINST_RD_RT_SA_NODSPC("shrl.ph $t5, $t3, 12", 0xf4c0eeac, 12, t5, t3);
   TESTDSPINST_RD_RT_SA_NODSPC("shrl.ph $t2, $t4, 13", 0x006a54f2, 13, t2, t4);
   TESTDSPINST_RD_RT_SA_NODSPC("shrl.ph $t0, $t8, 14", 0x79f74493, 14, t0, t8);
   TESTDSPINST_RD_RT_SA_NODSPC("shrl.ph $t4, $t6, 15", 0x9c09e313, 15, t4, t6);

   printf("-------- SHRLV.PH --------\n");
   TESTDSPINST_RD_RS_RT_NODSPC("shrlv.ph $t0, $t1, $t2", 0x7fffffff, 0x00000000,
                               t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("shrlv.ph $t2, $t3, $t4", 0x80000000, 0x00000000,
                               t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("shrlv.ph $t4, $t1, $t5", 0xfabc3435, 0xfabc3421,
                               t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("shrlv.ph $t6, $t7, $t3", 0x07654cb8, 0x734680bc,
                               t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("shrlv.ph $t5, $t3, $t2", 0xf973437b, 0x80000000,
                               t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("shrlv.ph $t2, $t4, $t8", 0x00ff0001, 0xff01ffff,
                               t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("shrlv.ph $t0, $t8, $t0", 0x7fff7004, 0x7fff7fff,
                               t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("shrlv.ph $t4, $t6, $t1", 0x0000c420, 0x00000555,
                               t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("shrlv.ph $t0, $t1, $t2", 0x00000000, 0x00000000,
                               t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("shrlv.ph $t2, $t3, $t4", 0x80000000, 0x80000000,
                               t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("shrlv.ph $t4, $t1, $t5", 0xaaaaaaaa, 0x55555555,
                               t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("shrlv.ph $t6, $t7, $t3", 0x00000018, 0xffff2435,
                               t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("shrlv.ph $t5, $t3, $t2", 0xbabababa, 0xabababab,
                               t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("shrlv.ph $t2, $t4, $t8", 0xf0f0f0f0, 0xfc79b4d2,
                               t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("shrlv.ph $t0, $t8, $t0", 0xfbde3976, 0x00000000,
                               t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("shrlv.ph $t4, $t6, $t1", 0x23534870, 0x00354565,
                               t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("shrlv.ph $t0, $t1, $t2", 0x980b7cde, 0x00086755,
                               t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("shrlv.ph $t2, $t3, $t4", 0x00000018, 0x8f8f8f8f,
                               t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("shrlv.ph $t4, $t1, $t5", 0x92784656, 0xeeeeeeee,
                               t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("shrlv.ph $t6, $t7, $t3", 0xcacacaca, 0x1bdbdbdb,
                               t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("shrlv.ph $t5, $t3, $t2", 0xbacabaca, 0xdecadeca,
                               t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("shrlv.ph $t2, $t4, $t8", 0x12fadeb4, 0x93474bde,
                               t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("shrlv.ph $t0, $t8, $t0", 0x7c000790, 0xfc0007ff,
                               t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("shrlv.ph $t4, $t6, $t1", 0xffffffff, 0xffffffff,
                               t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("shrlv.ph $t0, $t1, $t2", 0xf2f4df1f, 0xcb4ab48f,
                               t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("shrlv.ph $t2, $t3, $t4", 0x435f909a, 0xaf8f7e18,
                               t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("shrlv.ph $t4, $t1, $t5", 0x2106ba5f, 0x87df4510,
                               t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("shrlv.ph $t6, $t7, $t3", 0x246a6376, 0xabf4e8e1,
                               t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("shrlv.ph $t5, $t3, $t2", 0x1046a1a3, 0xf4c0eeac,
                               t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("shrlv.ph $t2, $t4, $t8", 0x638ca515, 0x006a54f2,
                               t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("shrlv.ph $t0, $t8, $t0", 0xf63e7a9d, 0x79f74493,
                               t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("shrlv.ph $t4, $t6, $t1", 0xbd6845cd, 0x9c09e313,
                               t4, t6, t1);

   printf("-------- SUBQH.PH --------\n");
   TESTDSPINST_RD_RS_RT_NODSPC("subqh.ph $t0, $t1, $t2", 0x7fffffff, 0x00000000,
                               t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh.ph $t2, $t3, $t4", 0x80000000, 0x00000000,
                               t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh.ph $t4, $t1, $t5", 0xfabc3435, 0xfabc3421,
                               t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh.ph $t6, $t7, $t3", 0x07654cb8, 0x734680bc,
                               t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh.ph $t5, $t3, $t2", 0xf973437b, 0x80000000,
                               t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh.ph $t2, $t4, $t8", 0x00ff0001, 0xff01ffff,
                               t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh.ph $t0, $t8, $t0", 0x7fff7004, 0x7fff7fff,
                               t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh.ph $t4, $t6, $t1", 0x0000c420, 0x00000555,
                               t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh.ph $t0, $t1, $t2", 0x00000000, 0x00000000,
                               t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh.ph $t2, $t3, $t4", 0x80000000, 0x80000000,
                               t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh.ph $t4, $t1, $t5", 0xaaaaaaaa, 0x55555555,
                               t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh.ph $t6, $t7, $t3", 0x00000018, 0xffff2435,
                               t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh.ph $t5, $t3, $t2", 0xbabababa, 0xabababab,
                               t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh.ph $t2, $t4, $t8", 0xf0f0f0f0, 0xfc79b4d2,
                               t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh.ph $t0, $t8, $t0", 0xfbde3976, 0x00000000,
                               t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh.ph $t4, $t6, $t1", 0x23534870, 0x00354565,
                               t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh.ph $t0, $t1, $t2", 0x980b7cde, 0x00086755,
                               t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh.ph $t2, $t3, $t4", 0x00000018, 0x8f8f8f8f,
                               t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh.ph $t4, $t1, $t5", 0x92784656, 0xeeeeeeee,
                               t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh.ph $t6, $t7, $t3", 0xcacacaca, 0x1bdbdbdb,
                               t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh.ph $t5, $t3, $t2", 0xbacabaca, 0xdecadeca,
                               t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh.ph $t2, $t4, $t8", 0x12fadeb4, 0x93474bde,
                               t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh.ph $t0, $t8, $t0", 0x7c000790, 0xfc0007ff,
                               t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh.ph $t4, $t6, $t1", 0xffffffff, 0xffffffff,
                               t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh.ph $t0, $t1, $t2", 0xf2f4df1f, 0xcb4ab48f,
                               t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh.ph $t2, $t3, $t4", 0x435f909a, 0xaf8f7e18,
                               t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh.ph $t4, $t1, $t5", 0x2106ba5f, 0x87df4510,
                               t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh.ph $t6, $t7, $t3", 0x246a6376, 0xabf4e8e1,
                               t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh.ph $t5, $t3, $t2", 0x1046a1a3, 0xf4c0eeac,
                               t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh.ph $t2, $t4, $t8", 0x638ca515, 0x006a54f2,
                               t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh.ph $t0, $t8, $t0", 0xf63e7a9d, 0x79f74493,
                               t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh.ph $t4, $t6, $t1", 0xbd6845cd, 0x9c09e313,
                               t4, t6, t1);

   printf("-------- SUBQH_R.PH --------\n");
   TESTDSPINST_RD_RS_RT_NODSPC("subqh_r.ph $t0, $t1, $t2", 0x7fffffff,
                               0x00000000, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh_r.ph $t2, $t3, $t4", 0x80000000,
                               0x00000000, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh_r.ph $t4, $t1, $t5", 0xfabc3435,
                               0xfabc3421, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh_r.ph $t6, $t7, $t3", 0x07654cb8,
                               0x734680bc, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh_r.ph $t5, $t3, $t2", 0xf973437b,
                               0x80000000, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh_r.ph $t2, $t4, $t8", 0x00ff0001,
                               0xff01ffff, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh_r.ph $t0, $t8, $t0", 0x7fff7004,
                               0x7fff7fff, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh_r.ph $t4, $t6, $t1", 0x0000c420,
                               0x00000555, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh_r.ph $t0, $t1, $t2", 0x00000000,
                               0x00000000, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh_r.ph $t2, $t3, $t4", 0x80000000,
                               0x80000000, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh_r.ph $t4, $t1, $t5", 0xaaaaaaaa,
                               0x55555555, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh_r.ph $t6, $t7, $t3", 0x00000018,
                               0xffff2435, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh_r.ph $t5, $t3, $t2", 0xbabababa,
                               0xabababab, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh_r.ph $t2, $t4, $t8", 0xf0f0f0f0,
                               0xfc79b4d2, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh_r.ph $t0, $t8, $t0", 0xfbde3976,
                               0x00000000, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh_r.ph $t4, $t6, $t1", 0x23534870,
                               0x00354565, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh_r.ph $t0, $t1, $t2", 0x980b7cde,
                               0x00086755, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh_r.ph $t2, $t3, $t4", 0x00000018,
                               0x8f8f8f8f, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh_r.ph $t4, $t1, $t5", 0x92784656,
                               0xeeeeeeee, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh_r.ph $t6, $t7, $t3", 0xcacacaca,
                               0x1bdbdbdb, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh_r.ph $t5, $t3, $t2", 0xbacabaca,
                               0xdecadeca, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh_r.ph $t2, $t4, $t8", 0x12fadeb4,
                               0x93474bde, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh_r.ph $t0, $t8, $t0", 0x7c000790,
                               0xfc0007ff, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh_r.ph $t4, $t6, $t1", 0xffffffff,
                               0xffffffff, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh_r.ph $t0, $t1, $t2", 0xf2f4df1f,
                               0xcb4ab48f, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh_r.ph $t2, $t3, $t4", 0x435f909a,
                               0xaf8f7e18, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh_r.ph $t4, $t1, $t5", 0x2106ba5f,
                               0x87df4510, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh_r.ph $t6, $t7, $t3", 0x246a6376,
                               0xabf4e8e1, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh_r.ph $t5, $t3, $t2", 0x1046a1a3,
                               0xf4c0eeac, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh_r.ph $t2, $t4, $t8", 0x638ca515,
                               0x006a54f2, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh_r.ph $t0, $t8, $t0", 0xf63e7a9d,
                               0x79f74493, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh_r.ph $t4, $t6, $t1", 0xbd6845cd,
                               0x9c09e313, t4, t6, t1);

   printf("-------- SUBQH.W --------\n");
   TESTDSPINST_RD_RS_RT_NODSPC("subqh.w $t0, $t1, $t2", 0x7fffffff, 0x00000000,
                               t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh.w $t2, $t3, $t4", 0x80000000, 0x00000000,
                               t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh.w $t4, $t1, $t5", 0xfabc3435, 0xfabc3421,
                               t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh.w $t6, $t7, $t3", 0x07654cb8, 0x734680bc,
                               t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh.w $t5, $t3, $t2", 0xf973437b, 0x80000000,
                               t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh.w $t2, $t4, $t8", 0x00ff0001, 0xff01ffff,
                               t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh.w $t0, $t8, $t0", 0x7fff7004, 0x7fff7fff,
                               t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh.w $t4, $t6, $t1", 0x0000c420, 0x00000555,
                               t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh.w $t0, $t1, $t2", 0x00000000, 0x00000000,
                               t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh.w $t2, $t3, $t4", 0x80000000, 0x80000000,
                               t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh.w $t4, $t1, $t5", 0xaaaaaaaa, 0x55555555,
                               t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh.w $t6, $t7, $t3", 0x00000018, 0xffff2435,
                               t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh.w $t5, $t3, $t2", 0xbabababa, 0xabababab,
                               t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh.w $t2, $t4, $t8", 0xf0f0f0f0, 0xfc79b4d2,
                               t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh.w $t0, $t8, $t0", 0xfbde3976, 0x00000000,
                               t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh.w $t4, $t6, $t1", 0x23534870, 0x00354565,
                               t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh.w $t0, $t1, $t2", 0x980b7cde, 0x00086755,
                               t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh.w $t2, $t3, $t4", 0x00000018, 0x8f8f8f8f,
                               t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh.w $t4, $t1, $t5", 0x92784656, 0xeeeeeeee,
                               t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh.w $t6, $t7, $t3", 0xcacacaca, 0x1bdbdbdb,
                               t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh.w $t5, $t3, $t2", 0xbacabaca, 0xdecadeca,
                               t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh.w $t2, $t4, $t8", 0x12fadeb4, 0x93474bde,
                               t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh.w $t0, $t8, $t0", 0x7c000790, 0xfc0007ff,
                               t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh.w $t4, $t6, $t1", 0xffffffff, 0xffffffff,
                               t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh.w $t0, $t1, $t2", 0xf2f4df1f, 0xcb4ab48f,
                               t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh.w $t2, $t3, $t4", 0x435f909a, 0xaf8f7e18,
                               t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh.w $t4, $t1, $t5", 0x2106ba5f, 0x87df4510,
                               t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh.w $t6, $t7, $t3", 0x246a6376, 0xabf4e8e1,
                               t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh.w $t5, $t3, $t2", 0x1046a1a3, 0xf4c0eeac,
                               t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh.w $t2, $t4, $t8", 0x638ca515, 0x006a54f2,
                               t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh.w $t0, $t8, $t0", 0xf63e7a9d, 0x79f74493,
                               t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh.w $t4, $t6, $t1", 0xbd6845cd, 0x9c09e313,
                               t4, t6, t1);

   printf("-------- SUBQH_R.W --------\n");
   TESTDSPINST_RD_RS_RT_NODSPC("subqh_r.w $t0, $t1, $t2", 0x7fffffff,
                               0x00000000, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh_r.w $t2, $t3, $t4", 0x80000000,
                               0x00000000, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh_r.w $t4, $t1, $t5", 0xfabc3435,
                               0xfabc3421, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh_r.w $t6, $t7, $t3", 0x07654cb8,
                               0x734680bc, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh_r.w $t5, $t3, $t2", 0xf973437b,
                               0x80000000, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh_r.w $t2, $t4, $t8", 0x00ff0001,
                               0xff01ffff, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh_r.w $t0, $t8, $t0", 0x7fff7004,
                               0x7fff7fff, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh_r.w $t4, $t6, $t1", 0x0000c420,
                               0x00000555, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh_r.w $t0, $t1, $t2", 0x00000000,
                               0x00000000, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh_r.w $t2, $t3, $t4", 0x80000000,
                               0x80000000, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh_r.w $t4, $t1, $t5", 0xaaaaaaaa,
                               0x55555555, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh_r.w $t6, $t7, $t3", 0x00000018,
                               0xffff2435, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh_r.w $t5, $t3, $t2", 0xbabababa,
                               0xabababab, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh_r.w $t2, $t4, $t8", 0xf0f0f0f0,
                               0xfc79b4d2, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh_r.w $t0, $t8, $t0", 0xfbde3976,
                               0x00000000, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh_r.w $t4, $t6, $t1", 0x23534870,
                               0x00354565, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh_r.w $t0, $t1, $t2", 0x980b7cde,
                               0x00086755, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh_r.w $t2, $t3, $t4", 0x00000018,
                               0x8f8f8f8f, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh_r.w $t4, $t1, $t5", 0x92784656,
                               0xeeeeeeee, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh_r.w $t6, $t7, $t3", 0xcacacaca,
                               0x1bdbdbdb, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh_r.w $t5, $t3, $t2", 0xbacabaca,
                               0xdecadeca, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh_r.w $t2, $t4, $t8", 0x12fadeb4,
                               0x93474bde, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh_r.w $t0, $t8, $t0", 0x7c000790,
                               0xfc0007ff, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh_r.w $t4, $t6, $t1", 0xffffffff,
                               0xffffffff, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh_r.w $t0, $t1, $t2", 0xf2f4df1f,
                               0xcb4ab48f, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh_r.w $t2, $t3, $t4", 0x435f909a,
                               0xaf8f7e18, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh_r.w $t4, $t1, $t5", 0x2106ba5f,
                               0x87df4510, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh_r.w $t6, $t7, $t3", 0x246a6376,
                               0xabf4e8e1, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh_r.w $t5, $t3, $t2", 0x1046a1a3,
                               0xf4c0eeac, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh_r.w $t2, $t4, $t8", 0x638ca515,
                               0x006a54f2, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh_r.w $t0, $t8, $t0", 0xf63e7a9d,
                               0x79f74493, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("subqh_r.w $t4, $t6, $t1", 0xbd6845cd,
                               0x9c09e313, t4, t6, t1);

   printf("-------- SUBU.PH --------\n");
   TESTDSPINST_RD_RS_RT_DSPC("subu.ph $t0, $t1, $t2", 0x7fffffff, 0x00000000,
                             t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("subu.ph $t2, $t3, $t4", 0x80000000, 0x00000000,
                             t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("subu.ph $t4, $t1, $t5", 0xfabc3435, 0xfabc3421,
                             t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("subu.ph $t6, $t7, $t3", 0x07654cb8, 0x734680bc,
                             t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("subu.ph $t5, $t3, $t2", 0xf973437b, 0x80000000,
                             t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("subu.ph $t2, $t4, $t8", 0x00ff0001, 0xff01ffff,
                             t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("subu.ph $t0, $t8, $t0", 0x7fff7004, 0x7fff7fff,
                             t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("subu.ph $t4, $t6, $t1", 0x0000c420, 0x00000555,
                             t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("subu.ph $t0, $t1, $t2", 0x00000000, 0x00000000,
                             t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("subu.ph $t2, $t3, $t4", 0x80000000, 0x80000000,
                             t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("subu.ph $t4, $t1, $t5", 0xaaaaaaaa, 0x55555555,
                             t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("subu.ph $t6, $t7, $t3", 0x00000018, 0xffff2435,
                             t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("subu.ph $t5, $t3, $t2", 0xbabababa, 0xabababab,
                             t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("subu.ph $t2, $t4, $t8", 0xf0f0f0f0, 0xfc79b4d2,
                             t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("subu.ph $t0, $t8, $t0", 0xfbde3976, 0x00000000,
                             t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("subu.ph $t4, $t6, $t1", 0x23534870, 0x00354565,
                             t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("subu.ph $t0, $t1, $t2", 0x980b7cde, 0x00086755,
                             t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("subu.ph $t2, $t3, $t4", 0x00000018, 0x8f8f8f8f,
                             t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("subu.ph $t4, $t1, $t5", 0x92784656, 0xeeeeeeee,
                             t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("subu.ph $t6, $t7, $t3", 0xcacacaca, 0x1bdbdbdb,
                             t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("subu.ph $t5, $t3, $t2", 0xbacabaca, 0xdecadeca,
                             t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("subu.ph $t2, $t4, $t8", 0x12fadeb4, 0x93474bde,
                             t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("subu.ph $t0, $t8, $t0", 0x7c000790, 0xfc0007ff,
                             t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("subu.ph $t4, $t6, $t1", 0xffffffff, 0xffffffff,
                             t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("subu.ph $t0, $t1, $t2", 0xf2f4df1f, 0xcb4ab48f,
                             t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("subu.ph $t2, $t3, $t4", 0x435f909a, 0xaf8f7e18,
                             t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("subu.ph $t4, $t1, $t5", 0x2106ba5f, 0x87df4510,
                             t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("subu.ph $t6, $t7, $t3", 0x246a6376, 0xabf4e8e1,
                             t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("subu.ph $t5, $t3, $t2", 0x1046a1a3, 0xf4c0eeac,
                             t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("subu.ph $t2, $t4, $t8", 0x638ca515, 0x006a54f2,
                             t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("subu.ph $t0, $t8, $t0", 0xf63e7a9d, 0x79f74493,
                             t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("subu.ph $t4, $t6, $t1", 0xbd6845cd, 0x9c09e313,
                             t4, t6, t1);

   printf("-------- SUBU_S.PH --------\n");
   TESTDSPINST_RD_RS_RT_DSPC("subu_s.ph $t0, $t1, $t2", 0x7fffffff, 0x00000000,
                             t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("subu_s.ph $t2, $t3, $t4", 0x80000000, 0x00000000,
                             t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("subu_s.ph $t4, $t1, $t5", 0xfabc3435, 0xfabc3421,
                             t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("subu_s.ph $t6, $t7, $t3", 0x07654cb8, 0x734680bc,
                             t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("subu_s.ph $t5, $t3, $t2", 0xf973437b, 0x80000000,
                             t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("subu_s.ph $t2, $t4, $t8", 0x00ff0001, 0xff01ffff,
                             t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("subu_s.ph $t0, $t8, $t0", 0x7fff7004, 0x7fff7fff,
                             t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("subu_s.ph $t4, $t6, $t1", 0x0000c420, 0x00000555,
                             t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("subu_s.ph $t0, $t1, $t2", 0x00000000, 0x00000000,
                             t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("subu_s.ph $t2, $t3, $t4", 0x80000000, 0x80000000,
                             t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("subu_s.ph $t4, $t1, $t5", 0xaaaaaaaa, 0x55555555,
                             t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("subu_s.ph $t6, $t7, $t3", 0x00000018, 0xffff2435,
                             t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("subu_s.ph $t5, $t3, $t2", 0xbabababa, 0xabababab,
                             t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("subu_s.ph $t2, $t4, $t8", 0xf0f0f0f0, 0xfc79b4d2,
                             t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("subu_s.ph $t0, $t8, $t0", 0xfbde3976, 0x00000000,
                             t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("subu_s.ph $t4, $t6, $t1", 0x23534870, 0x00354565,
                             t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("subu_s.ph $t0, $t1, $t2", 0x980b7cde, 0x00086755,
                             t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("subu_s.ph $t2, $t3, $t4", 0x00000018, 0x8f8f8f8f,
                             t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("subu_s.ph $t4, $t1, $t5", 0x92784656, 0xeeeeeeee,
                             t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("subu_s.ph $t6, $t7, $t3", 0xcacacaca, 0x1bdbdbdb,
                             t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("subu_s.ph $t5, $t3, $t2", 0xbacabaca, 0xdecadeca,
                             t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("subu_s.ph $t2, $t4, $t8", 0x12fadeb4, 0x93474bde,
                             t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("subu_s.ph $t0, $t8, $t0", 0x7c000790, 0xfc0007ff,
                             t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("subu_s.ph $t4, $t6, $t1", 0xffffffff, 0xffffffff,
                             t4, t6, t1);
   TESTDSPINST_RD_RS_RT_DSPC("subu_s.ph $t0, $t1, $t2", 0xf2f4df1f, 0xcb4ab48f,
                             t0, t1, t2);
   TESTDSPINST_RD_RS_RT_DSPC("subu_s.ph $t2, $t3, $t4", 0x435f909a, 0xaf8f7e18,
                             t2, t3, t4);
   TESTDSPINST_RD_RS_RT_DSPC("subu_s.ph $t4, $t1, $t5", 0x2106ba5f, 0x87df4510,
                             t4, t1, t5);
   TESTDSPINST_RD_RS_RT_DSPC("subu_s.ph $t6, $t7, $t3", 0x246a6376, 0xabf4e8e1,
                             t6, t7, t3);
   TESTDSPINST_RD_RS_RT_DSPC("subu_s.ph $t5, $t3, $t2", 0x1046a1a3, 0xf4c0eeac,
                             t5, t3, t2);
   TESTDSPINST_RD_RS_RT_DSPC("subu_s.ph $t2, $t4, $t8", 0x638ca515, 0x006a54f2,
                             t2, t4, t8);
   TESTDSPINST_RD_RS_RT_DSPC("subu_s.ph $t0, $t8, $t0", 0xf63e7a9d, 0x79f74493,
                             t0, t8, t0);
   TESTDSPINST_RD_RS_RT_DSPC("subu_s.ph $t4, $t6, $t1", 0xbd6845cd, 0x9c09e313,
                             t4, t6, t1);

   printf("-------- SUBUH.QB --------\n");
   TESTDSPINST_RD_RS_RT_NODSPC("subuh.qb $t0, $t1, $t2", 0x7fffffff, 0x00000000,
                               t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("subuh.qb $t2, $t3, $t4", 0x80000000, 0x00000000,
                               t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("subuh.qb $t4, $t1, $t5", 0xfabc3435, 0xfabc3421,
                               t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("subuh.qb $t6, $t7, $t3", 0x07654cb8, 0x734680bc,
                               t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("subuh.qb $t5, $t3, $t2", 0xf973437b, 0x80000000,
                               t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("subuh.qb $t2, $t4, $t8", 0x00ff0001, 0xff01ffff,
                               t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("subuh.qb $t0, $t8, $t0", 0x7fff7004, 0x7fff7fff,
                               t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("subuh.qb $t4, $t6, $t1", 0x0000c420, 0x00000555,
                               t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("subuh.qb $t0, $t1, $t2", 0x00000000, 0x00000000,
                               t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("subuh.qb $t2, $t3, $t4", 0x80000000, 0x80000000,
                               t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("subuh.qb $t4, $t1, $t5", 0xaaaaaaaa, 0x55555555,
                               t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("subuh.qb $t6, $t7, $t3", 0x00000018, 0xffff2435,
                               t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("subuh.qb $t5, $t3, $t2", 0xbabababa, 0xabababab,
                               t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("subuh.qb $t2, $t4, $t8", 0xf0f0f0f0, 0xfc79b4d2,
                               t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("subuh.qb $t0, $t8, $t0", 0xfbde3976, 0x00000000,
                               t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("subuh.qb $t4, $t6, $t1", 0x23534870, 0x00354565,
                               t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("subuh.qb $t0, $t1, $t2", 0x980b7cde, 0x00086755,
                               t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("subuh.qb $t2, $t3, $t4", 0x00000018, 0x8f8f8f8f,
                               t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("subuh.qb $t4, $t1, $t5", 0x92784656, 0xeeeeeeee,
                               t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("subuh.qb $t6, $t7, $t3", 0xcacacaca, 0x1bdbdbdb,
                               t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("subuh.qb $t5, $t3, $t2", 0xbacabaca, 0xdecadeca,
                               t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("subuh.qb $t2, $t4, $t8", 0x12fadeb4, 0x93474bde,
                               t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("subuh.qb $t0, $t8, $t0", 0x7c000790, 0xfc0007ff,
                               t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("subuh.qb $t4, $t6, $t1", 0xffffffff, 0xffffffff,
                               t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("subuh.qb $t0, $t1, $t2", 0xf2f4df1f, 0xcb4ab48f,
                               t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("subuh.qb $t2, $t3, $t4", 0x435f909a, 0xaf8f7e18,
                               t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("subuh.qb $t4, $t1, $t5", 0x2106ba5f, 0x87df4510,
                               t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("subuh.qb $t6, $t7, $t3", 0x246a6376, 0xabf4e8e1,
                               t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("subuh.qb $t5, $t3, $t2", 0x1046a1a3, 0xf4c0eeac,
                               t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("subuh.qb $t2, $t4, $t8", 0x638ca515, 0x006a54f2,
                               t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("subuh.qb $t0, $t8, $t0", 0xf63e7a9d, 0x79f74493,
                               t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("subuh.qb $t4, $t6, $t1", 0xbd6845cd, 0x9c09e313,
                               t4, t6, t1);

   printf("-------- SUBUH_R.QB --------\n");
   TESTDSPINST_RD_RS_RT_NODSPC("subuh_r.qb $t0, $t1, $t2", 0x7fffffff,
                               0x00000000, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("subuh_r.qb $t2, $t3, $t4", 0x80000000,
                               0x00000000, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("subuh_r.qb $t4, $t1, $t5", 0xfabc3435,
                               0xfabc3421, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("subuh_r.qb $t6, $t7, $t3", 0x07654cb8,
                               0x734680bc, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("subuh_r.qb $t5, $t3, $t2", 0xf973437b,
                               0x80000000, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("subuh_r.qb $t2, $t4, $t8", 0x00ff0001,
                               0xff01ffff, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("subuh_r.qb $t0, $t8, $t0", 0x7fff7004,
                               0x7fff7fff, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("subuh_r.qb $t4, $t6, $t1", 0x0000c420,
                               0x00000555, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("subuh_r.qb $t0, $t1, $t2", 0x00000000,
                               0x00000000, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("subuh_r.qb $t2, $t3, $t4", 0x80000000,
                               0x80000000, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("subuh_r.qb $t4, $t1, $t5", 0xaaaaaaaa,
                               0x55555555, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("subuh_r.qb $t6, $t7, $t3", 0x00000018,
                               0xffff2435, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("subuh_r.qb $t5, $t3, $t2", 0xbabababa,
                               0xabababab, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("subuh_r.qb $t2, $t4, $t8", 0xf0f0f0f0,
                               0xfc79b4d2, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("subuh_r.qb $t0, $t8, $t0", 0xfbde3976,
                               0x00000000, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("subuh_r.qb $t4, $t6, $t1", 0x23534870,
                               0x00354565, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("subuh_r.qb $t0, $t1, $t2", 0x980b7cde,
                               0x00086755, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("subuh_r.qb $t2, $t3, $t4", 0x00000018,
                               0x8f8f8f8f, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("subuh_r.qb $t4, $t1, $t5", 0x92784656,
                               0xeeeeeeee, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("subuh_r.qb $t6, $t7, $t3", 0xcacacaca,
                               0x1bdbdbdb, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("subuh_r.qb $t5, $t3, $t2", 0xbacabaca,
                               0xdecadeca, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("subuh_r.qb $t2, $t4, $t8", 0x12fadeb4,
                               0x93474bde, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("subuh_r.qb $t0, $t8, $t0", 0x7c000790,
                               0xfc0007ff, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("subuh_r.qb $t4, $t6, $t1", 0xffffffff,
                               0xffffffff, t4, t6, t1);
   TESTDSPINST_RD_RS_RT_NODSPC("subuh_r.qb $t0, $t1, $t2", 0xf2f4df1f,
                               0xcb4ab48f, t0, t1, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("subuh_r.qb $t2, $t3, $t4", 0x435f909a,
                               0xaf8f7e18, t2, t3, t4);
   TESTDSPINST_RD_RS_RT_NODSPC("subuh_r.qb $t4, $t1, $t5", 0x2106ba5f,
                               0x87df4510, t4, t1, t5);
   TESTDSPINST_RD_RS_RT_NODSPC("subuh_r.qb $t6, $t7, $t3", 0x246a6376,
                               0xabf4e8e1, t6, t7, t3);
   TESTDSPINST_RD_RS_RT_NODSPC("subuh_r.qb $t5, $t3, $t2", 0x1046a1a3,
                               0xf4c0eeac, t5, t3, t2);
   TESTDSPINST_RD_RS_RT_NODSPC("subuh_r.qb $t2, $t4, $t8", 0x638ca515,
                               0x006a54f2, t2, t4, t8);
   TESTDSPINST_RD_RS_RT_NODSPC("subuh_r.qb $t0, $t8, $t0", 0xf63e7a9d,
                               0x79f74493, t0, t8, t0);
   TESTDSPINST_RD_RS_RT_NODSPC("subuh_r.qb $t4, $t6, $t1", 0xbd6845cd,
                               0x9c09e313, t4, t6, t1);
#endif

   return 0;
}