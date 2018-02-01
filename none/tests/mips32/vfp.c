#if defined(__mips_hard_float)
#include <setjmp.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

unsigned int mem[] = {
   0x4095A266, 0x66666666,
   0xBFF00000, 0x00000000,
   0x3FF00000, 0x00000000,
   0x252a2e2b, 0x262d2d2a,
   0xFFFFFFFF, 0xFFFFFFFF,
   0x41D26580, 0xB487E5C9,
   0x42026580, 0xB750E388,
   0x3E45798E, 0xE2308C3A,
   0x3FBF9ADD, 0x3746F65F
};

long long meml[] = {
   0x236457894095A266, 0x7777777766666666,
   0xBFF00000aaaaccde, 0x0004563217800000,
   0x3FF0556644770000, 0x0002255889900000,
   0x25254123698a2e2b, 0x21a2b3d6f62d2d2a,
   0xFFaabb22ccFFFFFF, 0x542698eeFFFFFFFF,
   0x41D2658041D26580, 0xB487E5C9B487E5C9,
   0x420774411aa26580, 0xaabbccddB750E388,
   0xffffeeee3E45798E, 0xccccccccE2308C3A,
   0x123abb983FBF9ADD, 0x002255443746F65F
};

float fs_f[] = {
   0, 456.2489562, 3, -1,
   1384.6, -7.2945676, 1000000000, -5786.47,
   1752, 0.0024575, 0.00000001, -248562.76,
   -45786.476, 456.2489562, 34.00046, 45786.476,
   1752065, 107, -45667.24, -7.2945676,
   -347856.475, 356047.56, -1.0, 23.04
};

double fs_d[] = {
   0, 456.2489562, 3, -1,
   1384.6, -7.2945676, 1000000000, -5786.47,
   1752, 0.0024575, 0.00000001, -248562.76,
   -45786.476, 456.2489562, 34.00046, 45786.476,
   1752065, 107, -45667.24, -7.2945676,
   -347856.475, 356047.56, -1.0, 23.04
};

double mem1[] = {
   0, 0, 0, 0,
   0, 0, 0, 0,
   0, 0, 0, 0,
   0, 0, 0, 0
};

float mem1f[] = {
   0, 0, 0, 0,
   0, 0, 0, 0,
   0, 0, 0, 0,
   0, 0, 0, 0
};

// ldc1 $f0, 0($t1)
#define TESTINSN5LOAD(instruction, RTval, offset, RT)            \
{                                                                \
   double out;                                                   \
   uint64_t outl;                                                \
   __asm__ volatile(                                             \
      "move $t1, %1\n\t"                                         \
      "li $t0, " #RTval"\n\t"                                    \
      instruction "\n\t"                                         \
      "mov.d %0, $" #RT "\n\t"                                   \
      "sdc1 $" #RT ", 0(%3) \n\t"                                \
      : "=&f" (out)                                              \
      : "r" (mem), "r" (RTval), "r" (&outl)                      \
      : "t0", "t1", "$"#RT, "memory"                             \
   );                                                            \
   printf("%s :: ft 0x%x%x\n",                                   \
          instruction, (uint32_t)outl, (uint32_t)(outl >> 32));  \
}

// lwc1 $f0, 0($t1)
#define TESTINSN5LOADw(instruction, RTval, offset, RT)           \
{                                                                \
   double out;                                                   \
   int out1;                                                     \
   __asm__ volatile(                                             \
      "move $t1, %2\n\t"                                         \
      "li $t0, " #RTval"\n\t"                                    \
      instruction "\n\t"                                         \
      "mov.d %0, $" #RT "\n\t"                                   \
      "mfc1 %1, $" #RT "\n\t"                                    \
      : "=f" (out), "=r" (out1)                                  \
      : "r" (mem), "r" (RTval)                                   \
      : "t0", "t1", "$"#RT, "memory"                             \
   );                                                            \
   printf("%s :: ft 0x%x\n",                                     \
          instruction, out1);                                    \
}

// lwxc1 $f0, $a3($v0)
#define TESTINSN6LOADw(instruction, indexVal, fd, index, base)   \
{                                                                \
   int out;                                                      \
   __asm__ volatile(                                             \
      "move $" #base ", %1\n\t"                                  \
      "li $" #index ", " #indexVal"\n\t"                         \
      instruction "\n\t"                                         \
      "mfc1 %0, $" #fd "\n\t"                                    \
      : "=r" (out)                                               \
      : "r" (mem)                                                \
      : #base, #index, "$"#fd, "memory"                          \
   );                                                            \
   printf("%s :: ft 0x%x\n",                                     \
          instruction, out);                                     \
}

// ldxc1 $f0, $a3($v0)
#define TESTINSN6LOADd(instruction, indexVal, fd, index, base)   \
{                                                                \
   uint64_t out;                                                 \
   __asm__ volatile(                                             \
      "move $" #base ", %0\n\t"                                  \
      "li $" #index ", " #indexVal"\n\t"                         \
      instruction "\n\t"                                         \
      "sdc1 $"#fd ", 0(%1)"                                      \
      : : "r" (mem), "r" (&out)                                  \
      : #base, #index, "$"#fd, "memory"                          \
   );                                                            \
   printf("%s :: ft lo: 0x%x, ft hi: 0x%x\n",                    \
          instruction, (uint32_t)out, (uint32_t)(out >> 32));    \
}

// luxc1 $f0, $a3($v0)
#define TESTINSN6LOADlu(instruction, indexVal, fd, index, base)  \
{                                                                \
   uint64_t out;                                                 \
   __asm__ volatile(                                             \
      "move $" #base ", %0\n\t"                                  \
      "li $" #index ", " #indexVal"\n\t"                         \
      instruction "\n\t"                                         \
      "sdc1 $"#fd ", 0(%1)"                                      \
      : : "r" (meml), "r" (&out)                                 \
      : #base, #index, "$"#fd, "memory"                          \
   );                                                            \
   printf("%s :: ft lo: 0x%x, ft hi: 0x%x\n",                    \
          instruction, (uint32_t)out, (uint32_t)(out >> 32));    \
}

// sdc1 $f0, 0($t0)
#define TESTINST1(offset)                                        \
{                                                                \
   unsigned int out;                                             \
   __asm__ volatile(                                             \
      "move $t0, %1\n\t"                                         \
      "move $t1, %2\n\t"                                         \
      "ldc1 $f0, "#offset"($t1)\n\t"                             \
      "sdc1 $f0, "#offset"($t0) \n\t"                            \
      "lw %0, "#offset"($t0)\n\t"                                \
      : "=r" (out)                                               \
      : "r" (mem1), "r" (fs_d)                                   \
      : "t1", "t0", "$f0", "memory"                              \
   );                                                            \
   printf("sdc1 $f0, 0($t0) :: out: 0x%x\n",                     \
          out);                                                  \
}

// sdxc1 $f0, $t2($t0)
#define TESTINST1a(offset)                                       \
{                                                                \
   unsigned int out;                                             \
   unsigned int out1;                                            \
   __asm__ volatile(                                             \
      "move $t0, %2\n\t"                                         \
      "move $t1, %3\n\t"                                         \
      "li $t2, "#offset"\n\t"                                    \
      "ldc1 $f0, "#offset"($t1)\n\t"                             \
      "sdxc1 $f0, $t2($t0) \n\t"                                 \
      "lw %0, "#offset"($t0)\n\t"                                \
      "addi $t0, $t0, 4 \n\t"                                    \
      "lw %1, "#offset"($t0)\n\t"                                \
      : "=r" (out), "=r" (out1)                                  \
      : "r" (mem1), "r" (fs_d)                                   \
      : "t2", "t1", "t0", "$f0", "memory"                        \
   );                                                            \
   printf("sdc1 $f0, #t2($t0) :: out: 0x%x : out1: 0x%x\n",      \
          out, out1);                                            \
}

// SUXC1 $f0, $t2($t0)
#define TESTINST1b(offset, unligned_offset)                      \
{                                                                \
   unsigned int out;                                             \
   unsigned int out1;                                            \
   __asm__ volatile(                                             \
      "move $t0, %2\n\t"                                         \
      "move $t1, %3\n\t"                                         \
      "li $t2, "#unligned_offset"\n\t"                           \
      "ldc1 $f0, "#offset"($t1)\n\t"                             \
      "suxc1 $f0, $t2($t0) \n\t"                                 \
      "lw %0, "#offset"($t0)\n\t"                                \
      "addi $t0, $t0, 4 \n\t"                                    \
      "lw %1, "#offset"($t0)\n\t"                                \
      : "=r" (out), "=r" (out1)                                  \
      : "r" (mem1), "r" (fs_d)                                   \
      : "t2", "t1", "t0", "$f0", "memory"                        \
   );                                                            \
   printf("suxc1 $f0, #t2($t0) :: out: 0x%x : out1: 0x%x\n",     \
          out, out1);                                            \
}

// swc1 $f0, 0($t0)
#define TESTINST2(offset)                                        \
{                                                                \
   unsigned int out;                                             \
   __asm__ volatile(                                             \
      "move $t0, %1\n\t"                                         \
      "move $t1, %2\n\t"                                         \
      "lwc1 $f0, "#offset"($t1)\n\t"                             \
      "swc1 $f0, "#offset"($t0) \n\t"                            \
      "lw %0, "#offset"($t0)\n\t"                                \
      : "=r" (out)                                               \
      : "r" (mem1f), "r" (fs_f)                                  \
      : "t1", "t0", "$f0", "memory"                              \
   );                                                            \
   printf("swc1 $f0, 0($t0) :: out: 0x%x\n",                     \
          out);                                                  \
}

// SWXC1 $f0, $t2($t0)
#define TESTINST2a(offset)                                       \
{                                                                \
   unsigned int out;                                             \
   __asm__ volatile(                                             \
      "move $t0, %1\n\t"                                         \
      "move $t1, %2\n\t"                                         \
      "li $t2, "#offset" \n\t"                                   \
      "lwc1 $f0, "#offset"($t1)\n\t"                             \
      "swxc1 $f0, $t2($t0) \n\t"                                 \
      "lw %0, "#offset"($t0)\n\t"                                \
      : "=r" (out)                                               \
      : "r" (mem1f), "r" (fs_f)                                  \
      : "t2", "t1", "t0", "$f0", "memory"                        \
   );                                                            \
   printf("swxc1 $f0, 0($t0) :: out: 0x%x\n",                    \
          out);                                                  \
}

#if (__mips==32) && (__mips_isa_rev>=2) && (__mips_isa_rev<6) \
    && (__mips_fpr==64 || __mips_fpr==xx)

#define TEST_FPU64                \
   __asm__ __volatile__(          \
      "cvt.l.s $f0, $f0"  "\n\t"  \
      :                           \
      :                           \
      : "$f0"                     \
   );

static void handler(int sig)
{
   exit(0);
}
#endif

void ppMem(double *m, int len)
{
   int i;
   printf("MEM1:\n");
   for (i = 0; i < len; i=i+4)
   {
      printf("%lf, %lf, %lf, %lf\n", m[i], m[i+1], m[i+2], m[i+3]);
      m[i] = 0;
      m[i+1] = 0;
      m[i+2] = 0;
      m[i+3] = 0;
   }
}

void ppMemF(float *m, int len)
{
   int i;
   printf("MEM1:\n");
   for (i = 0; i < len; i=i+4)
   {
      printf("%lf, %lf, %lf, %lf\n", m[i], m[i+1], m[i+2], m[i+3]);
      m[i] = 0;
      m[i+1] = 0;
      m[i+2] = 0;
      m[i+3] = 0;
   }
}

int main()
{
   printf("LDC1\n");
   TESTINSN5LOAD("ldc1 $f0, 0($t1)", 0, 0, f0);
   TESTINSN5LOAD("ldc1 $f0, 8($t1)", 0, 8, f0);
   TESTINSN5LOAD("ldc1 $f0, 16($t1)", 0, 16, f0);
   TESTINSN5LOAD("ldc1 $f0, 24($t1)", 0, 24, f0);
   TESTINSN5LOAD("ldc1 $f0, 32($t1)", 0, 32, f0);
   TESTINSN5LOAD("ldc1 $f0, 40($t1)", 0, 40, f0);
   TESTINSN5LOAD("ldc1 $f0, 48($t1)", 0, 48, f0);
   TESTINSN5LOAD("ldc1 $f0, 56($t1)", 0, 56, f0);
   TESTINSN5LOAD("ldc1 $f0, 64($t1)", 0, 64, f0);
   TESTINSN5LOAD("ldc1 $f0, 0($t1)", 0, 0, f0);
   TESTINSN5LOAD("ldc1 $f0, 8($t1)", 0, 8, f0);
   TESTINSN5LOAD("ldc1 $f0, 16($t1)", 0, 16, f0);
   TESTINSN5LOAD("ldc1 $f0, 24($t1)", 0, 24, f0);
   TESTINSN5LOAD("ldc1 $f0, 32($t1)", 0, 32, f0);
   TESTINSN5LOAD("ldc1 $f0, 40($t1)", 0, 40, f0);
   TESTINSN5LOAD("ldc1 $f0, 48($t1)", 0, 48, f0);
   TESTINSN5LOAD("ldc1 $f0, 56($t1)", 0, 56, f0);
   TESTINSN5LOAD("ldc1 $f0, 0($t1)", 0, 0, f0);
   TESTINSN5LOAD("ldc1 $f0, 8($t1)", 0, 8, f0);
   TESTINSN5LOAD("ldc1 $f0, 16($t1)", 0, 16, f0);
   TESTINSN5LOAD("ldc1 $f0, 24($t1)", 0, 24, f0);
   TESTINSN5LOAD("ldc1 $f0, 32($t1)", 0, 32, f0);
   TESTINSN5LOAD("ldc1 $f0, 40($t1)", 0, 40, f0);
   TESTINSN5LOAD("ldc1 $f0, 48($t1)", 0, 48, f0);
   TESTINSN5LOAD("ldc1 $f0, 56($t1)", 0, 56, f0);
   TESTINSN5LOAD("ldc1 $f0, 64($t1)", 0, 64, f0);
   TESTINSN5LOAD("ldc1 $f0, 0($t1)", 0, 0, f0);

   printf("LWC1\n");
   TESTINSN5LOADw("lwc1 $f0, 0($t1)", 0, 0, f0);
   TESTINSN5LOADw("lwc1 $f0, 4($t1)", 0, 4, f0);
   TESTINSN5LOADw("lwc1 $f0, 8($t1)", 0, 8, f0);
   TESTINSN5LOADw("lwc1 $f0, 12($t1)", 0, 12, f0);
   TESTINSN5LOADw("lwc1 $f0, 16($t1)", 0, 16, f0);
   TESTINSN5LOADw("lwc1 $f0, 20($t1)", 0, 20, f0);
   TESTINSN5LOADw("lwc1 $f0, 24($t1)", 0, 24, f0);
   TESTINSN5LOADw("lwc1 $f0, 28($t1)", 0, 28, f0);
   TESTINSN5LOADw("lwc1 $f0, 32($t1)", 0, 32, f0);
   TESTINSN5LOADw("lwc1 $f0, 36($t1)", 0, 36, f0);
   TESTINSN5LOADw("lwc1 $f0, 40($t1)", 0, 40, f0);
   TESTINSN5LOADw("lwc1 $f0, 44($t1)", 0, 44, f0);
   TESTINSN5LOADw("lwc1 $f0, 48($t1)", 0, 48, f0);
   TESTINSN5LOADw("lwc1 $f0, 52($t1)", 0, 52, f0);
   TESTINSN5LOADw("lwc1 $f0, 56($t1)", 0, 56, f0);
   TESTINSN5LOADw("lwc1 $f0, 60($t1)", 0, 60, f0);
   TESTINSN5LOADw("lwc1 $f0, 64($t1)", 0, 64, f0);
   TESTINSN5LOADw("lwc1 $f0, 0($t1)", 0, 0, f0);
   TESTINSN5LOADw("lwc1 $f0, 8($t1)", 0, 8, f0);
   TESTINSN5LOADw("lwc1 $f0, 16($t1)", 0, 16, f0);
   TESTINSN5LOADw("lwc1 $f0, 24($t1)", 0, 24, f0);
   TESTINSN5LOADw("lwc1 $f0, 32($t1)", 0, 32, f0);
   TESTINSN5LOADw("lwc1 $f0, 40($t1)", 0, 40, f0);
   TESTINSN5LOADw("lwc1 $f0, 48($t1)", 0, 48, f0);
   TESTINSN5LOADw("lwc1 $f0, 56($t1)", 0, 56, f0);
   TESTINSN5LOADw("lwc1 $f0, 64($t1)", 0, 64, f0);
   TESTINSN5LOADw("lwc1 $f0, 0($t1)", 0, 0, f0);

#if (__mips==32) && (__mips_isa_rev>=2) && (__mips_isa_rev<6)
   printf("LWXC1\n");
   TESTINSN6LOADw("lwxc1 $f0, $a3($v0)", 0, f0, a3, v0);
   TESTINSN6LOADw("lwxc1 $f0, $a3($v0)", 4, f0, a3, v0);
   TESTINSN6LOADw("lwxc1 $f0, $a3($v0)", 8, f0, a3, v0);
   TESTINSN6LOADw("lwxc1 $f0, $a3($v0)", 12, f0, a3, v0);
   TESTINSN6LOADw("lwxc1 $f0, $a3($v0)", 16, f0, a3, v0);
   TESTINSN6LOADw("lwxc1 $f0, $a3($v0)", 20, f0, a3, v0);
   TESTINSN6LOADw("lwxc1 $f0, $a3($v0)", 24, f0, a3, v0);
   TESTINSN6LOADw("lwxc1 $f0, $a3($v0)", 28, f0, a3, v0);
   TESTINSN6LOADw("lwxc1 $f0, $a3($v0)", 32, f0, a3, v0);
   TESTINSN6LOADw("lwxc1 $f0, $a3($v0)", 36, f0, a3, v0);
   TESTINSN6LOADw("lwxc1 $f0, $a3($v0)", 40, f0, a3, v0);
   TESTINSN6LOADw("lwxc1 $f0, $a3($v0)", 44, f0, a3, v0);
   TESTINSN6LOADw("lwxc1 $f0, $a3($v0)", 48, f0, a3, v0);
   TESTINSN6LOADw("lwxc1 $f0, $a3($v0)", 52, f0, a3, v0);
   TESTINSN6LOADw("lwxc1 $f0, $a3($v0)", 56, f0, a3, v0);
   TESTINSN6LOADw("lwxc1 $f0, $a3($v0)", 60, f0, a3, v0);
   TESTINSN6LOADw("lwxc1 $f0, $a3($v0)", 64, f0, a3, v0);
   TESTINSN6LOADw("lwxc1 $f0, $a3($v0)", 0, f0, a3, v0);
   TESTINSN6LOADw("lwxc1 $f0, $a3($v0)", 4, f0, a3, v0);
   TESTINSN6LOADw("lwxc1 $f0, $a3($v0)", 8, f0, a3, v0);
   TESTINSN6LOADw("lwxc1 $f0, $a3($v0)", 12, f0, a3, v0);
   TESTINSN6LOADw("lwxc1 $f0, $a3($v0)", 16, f0, a3, v0);
   TESTINSN6LOADw("lwxc1 $f0, $a3($v0)", 20, f0, a3, v0);
   TESTINSN6LOADw("lwxc1 $f0, $a3($v0)", 24, f0, a3, v0);
   TESTINSN6LOADw("lwxc1 $f0, $a3($v0)", 28, f0, a3, v0);
   TESTINSN6LOADw("lwxc1 $f0, $a3($v0)", 32, f0, a3, v0);
   TESTINSN6LOADw("lwxc1 $f0, $a3($v0)", 36, f0, a3, v0);
   TESTINSN6LOADw("lwxc1 $f0, $a3($v0)", 40, f0, a3, v0);
   TESTINSN6LOADw("lwxc1 $f0, $a3($v0)", 44, f0, a3, v0);
   TESTINSN6LOADw("lwxc1 $f0, $a3($v0)", 48, f0, a3, v0);
   TESTINSN6LOADw("lwxc1 $f0, $a3($v0)", 52, f0, a3, v0);
   TESTINSN6LOADw("lwxc1 $f0, $a3($v0)", 56, f0, a3, v0);

   printf("LDXC1\n");
   TESTINSN6LOADd("ldxc1 $f0, $a3($v0)", 0, f0, a3, v0);
   TESTINSN6LOADd("ldxc1 $f0, $a3($v0)", 8, f0, a3, v0);
   TESTINSN6LOADd("ldxc1 $f0, $a3($v0)", 16, f0, a3, v0);
   TESTINSN6LOADd("ldxc1 $f0, $a3($v0)", 24, f0, a3, v0);
   TESTINSN6LOADd("ldxc1 $f0, $a3($v0)", 32, f0, a3, v0);
   TESTINSN6LOADd("ldxc1 $f0, $a3($v0)", 40, f0, a3, v0);
   TESTINSN6LOADd("ldxc1 $f0, $a3($v0)", 48, f0, a3, v0);
   TESTINSN6LOADd("ldxc1 $f0, $a3($v0)", 56, f0, a3, v0);
   TESTINSN6LOADd("ldxc1 $f0, $a3($v0)", 64, f0, a3, v0);
   TESTINSN6LOADd("ldxc1 $f0, $a3($v0)", 0, f0, a3, v0);
   TESTINSN6LOADd("ldxc1 $f0, $a3($v0)", 8, f0, a3, v0);
   TESTINSN6LOADd("ldxc1 $f0, $a3($v0)", 16, f0, a3, v0);
   TESTINSN6LOADd("ldxc1 $f0, $a3($v0)", 24, f0, a3, v0);
   TESTINSN6LOADd("ldxc1 $f0, $a3($v0)", 32, f0, a3, v0);
   TESTINSN6LOADd("ldxc1 $f0, $a3($v0)", 40, f0, a3, v0);
   TESTINSN6LOADd("ldxc1 $f0, $a3($v0)", 48, f0, a3, v0);
   TESTINSN6LOADd("ldxc1 $f0, $a3($v0)", 56, f0, a3, v0);
   TESTINSN6LOADd("ldxc1 $f0, $a3($v0)", 64, f0, a3, v0);
   TESTINSN6LOADd("ldxc1 $f0, $a3($v0)", 0, f0, a3, v0);
   TESTINSN6LOADd("ldxc1 $f0, $a3($v0)", 8, f0, a3, v0);
   TESTINSN6LOADd("ldxc1 $f0, $a3($v0)", 16, f0, a3, v0);
   TESTINSN6LOADd("ldxc1 $f0, $a3($v0)", 24, f0, a3, v0);
   TESTINSN6LOADd("ldxc1 $f0, $a3($v0)", 32, f0, a3, v0);
   TESTINSN6LOADd("ldxc1 $f0, $a3($v0)", 40, f0, a3, v0);
   TESTINSN6LOADd("ldxc1 $f0, $a3($v0)", 48, f0, a3, v0);
   TESTINSN6LOADd("ldxc1 $f0, $a3($v0)", 56, f0, a3, v0);
   TESTINSN6LOADd("ldxc1 $f0, $a3($v0)", 64, f0, a3, v0);
   TESTINSN6LOADd("ldxc1 $f0, $a3($v0)", 0, f0, a3, v0);
   TESTINSN6LOADd("ldxc1 $f0, $a3($v0)", 8, f0, a3, v0);
   TESTINSN6LOADd("ldxc1 $f0, $a3($v0)", 16, f0, a3, v0);
   TESTINSN6LOADd("ldxc1 $f0, $a3($v0)", 24, f0, a3, v0);
   TESTINSN6LOADd("ldxc1 $f0, $a3($v0)", 32, f0, a3, v0);
#endif

   printf("SDC1\n");
   TESTINST1(0);
   TESTINST1(8);
   TESTINST1(16);
   TESTINST1(24);
   TESTINST1(32);
   TESTINST1(40);
   TESTINST1(48);
   TESTINST1(56);
   TESTINST1(64);
   ppMem(mem1, 16);

#if (__mips==32) && (__mips_isa_rev>=2) && (__mips_isa_rev<6)
   printf("SDXC1\n");
   TESTINST1a(0);
   TESTINST1a(8);
   TESTINST1a(16);
   TESTINST1a(24);
   TESTINST1a(32);
   TESTINST1a(40);
   TESTINST1a(48);
   TESTINST1a(56);
   TESTINST1a(64);
   ppMem(mem1, 16);
#endif

   printf("SWC1\n");
   TESTINST2(0);
   TESTINST2(8);
   TESTINST2(16);
   TESTINST2(24);
   TESTINST2(32);
   TESTINST2(40);
   TESTINST2(48);
   TESTINST2(56);
   TESTINST2(64);
   ppMemF(mem1f, 16);

#if (__mips==32) && (__mips_isa_rev>=2) && (__mips_isa_rev<6)
   printf("SWXC1\n");
   TESTINST2a(0);
   TESTINST2a(8);
   TESTINST2a(16);
   TESTINST2a(24);
   TESTINST2a(32);
   TESTINST2a(40);
   TESTINST2a(48);
   TESTINST2a(56);
   TESTINST2a(64);
   ppMemF(mem1f, 16);
#endif

#if (__mips==32) && (__mips_isa_rev>=2) && (__mips_isa_rev<6) \
    && (__mips_fpr==64 || __mips_fpr==xx)
   signal(SIGILL, handler);
   /* Test fpu64 mode. */
   TEST_FPU64;

   printf("luxc1\n");
   TESTINSN6LOADlu("luxc1 $f0, $a3($v0)", 0, f0, a3, v0);
   TESTINSN6LOADlu("luxc1 $f0, $a3($v0)", 8, f0, a3, v0);
   TESTINSN6LOADlu("luxc1 $f0, $a3($v0)", 16, f0, a3, v0);
   TESTINSN6LOADlu("luxc1 $f0, $a3($v0)", 24, f0, a3, v0);
   TESTINSN6LOADlu("luxc1 $f0, $a3($v0)", 32, f0, a3, v0);
   TESTINSN6LOADlu("luxc1 $f0, $a3($v0)", 40, f0, a3, v0);
   TESTINSN6LOADlu("luxc1 $f0, $a3($v0)", 48, f0, a3, v0);
   TESTINSN6LOADlu("luxc1 $f0, $a3($v0)", 56, f0, a3, v0);
   TESTINSN6LOADlu("luxc1 $f0, $a3($v0)", 64, f0, a3, v0);
   TESTINSN6LOADlu("luxc1 $f0, $a3($v0)", 0, f0, a3, v0);
   TESTINSN6LOADlu("luxc1 $f0, $a3($v0)", 8, f0, a3, v0);
   TESTINSN6LOADlu("luxc1 $f0, $a3($v0)", 16, f0, a3, v0);
   TESTINSN6LOADlu("luxc1 $f0, $a3($v0)", 24, f0, a3, v0);
   TESTINSN6LOADlu("luxc1 $f0, $a3($v0)", 32, f0, a3, v0);
   TESTINSN6LOADlu("luxc1 $f0, $a3($v0)", 40, f0, a3, v0);
   TESTINSN6LOADlu("luxc1 $f0, $a3($v0)", 48, f0, a3, v0);
   TESTINSN6LOADlu("luxc1 $f0, $a3($v0)", 56, f0, a3, v0);
   TESTINSN6LOADlu("luxc1 $f0, $a3($v0)", 64, f0, a3, v0);
   TESTINSN6LOADlu("luxc1 $f0, $a3($v0)", 0, f0, a3, v0);
   TESTINSN6LOADlu("luxc1 $f0, $a3($v0)", 8, f0, a3, v0);
   TESTINSN6LOADlu("luxc1 $f0, $a3($v0)", 16, f0, a3, v0);
   TESTINSN6LOADlu("luxc1 $f0, $a3($v0)", 24, f0, a3, v0);
   TESTINSN6LOADlu("luxc1 $f0, $a3($v0)", 32, f0, a3, v0);
   TESTINSN6LOADlu("luxc1 $f0, $a3($v0)", 40, f0, a3, v0);
   TESTINSN6LOADlu("luxc1 $f0, $a3($v0)", 48, f0, a3, v0);
   TESTINSN6LOADlu("luxc1 $f0, $a3($v0)", 56, f0, a3, v0);
   TESTINSN6LOADlu("luxc1 $f0, $a3($v0)", 64, f0, a3, v0);
   TESTINSN6LOADlu("luxc1 $f0, $a3($v0)", 0, f0, a3, v0);
   TESTINSN6LOADlu("luxc1 $f0, $a3($v0)", 8, f0, a3, v0);
   TESTINSN6LOADlu("luxc1 $f0, $a3($v0)", 16, f0, a3, v0);
   TESTINSN6LOADlu("luxc1 $f0, $a3($v0)", 24, f0, a3, v0);
   TESTINSN6LOADlu("luxc1 $f0, $a3($v0)", 32, f0, a3, v0);

   printf("SUXC1\n");
   TESTINST1b(0, 0);
   TESTINST1b(0, 1);
   TESTINST1b(8, 8);
   TESTINST1b(8, 9);
   TESTINST1b(16, 16);
   TESTINST1b(16, 17);
   TESTINST1b(24, 24);
   TESTINST1b(24, 25);
   TESTINST1b(32, 32);
   TESTINST1b(32, 35);
   TESTINST1b(40, 40);
   TESTINST1b(40, 42);
   TESTINST1b(48, 48);
   TESTINST1b(48, 50);
   TESTINST1b(56, 56);
   TESTINST1b(56, 60);
   TESTINST1b(64, 64);
   TESTINST1b(64, 67);
   ppMem(mem1, 16);
#endif
   return 0;
}
#else
int main() {
   return 0;
}
#endif
