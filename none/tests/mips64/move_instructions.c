#include <stdio.h>
#include "const.h"

const float reg_val_f[] = {
   -347856.475, 356047.56,   -1.0,       23.04,
   1752,        0.0024575,   0.00000001, -248562.76,
   1384.6,      -7.2945676,  1000000000, -5786.47,
   -347856.475, 356047.56,   -1.0,       23.04,
   0,           456.2489562, 3,          -1,
   -45786.476,  456.2489562, 34.00046,   45786.476,
   0,           456.2489562, 3,          -1,
   1384.6,      -7.2945676,  1000000000, -5786.47,
   1752,        0.0024575,   0.00000001, -248562.76,
   -45786.476,  456.2489562, 34.00046,   45786.476,
   1752065,     107,         -45667.24,  -7.2945676,
   -347856.475, 356047.56,   -1.0,       23.04,
   -347856.475, 356047.56,   -1.0,       23.04,
   1752,        0.0024575,   0.00000001, -248562.76,
   1384.6,      -7.2945676,  1000000000, -5786.47,
   -347856.475, 356047.56,   -1.0,       23.04,
   0,           456.2489562, 3,          -1,
   -45786.476,  456.2489562, 34.00046,   45786.476,
   0,           456.2489562, 3,          -1,
   1384.6,      -7.2945676,  1000000000, -5786.47,
   1752,        0.0024575,   0.00000001, -248562.76,
   -45786.476,  456.2489562, 34.00046,   45786.476,
   1752065,     107,         -45667.24,  -7.2945676,
   -347856.475, 356047.56,   -1.0,       23.04
};

const float fs1_f[] = {
   0,           456.2489562, 3,          -1,
   1384.6,      -7.2945676,  1000000000, -5786.47,
   1752,        0.0024575,   0.00000001, -248562.76,
   -45786.476,  456.2489562, 34.00046,   45786.476,
   1752065,     107,         -45667.24,  -7.2945676,
   -347856.475, 356047.56,   -1.0,       23.04
};

const double fs2_f[] = {
   0,           456.2489562, 3,           -1,
   -7.2945676,  1384.6,      1000000000,  -5786.47,
   1752,        0.0024575,   -248562.76,  0.00000001,
   -45786.476,  45786.476,   456.2489562, 34.00046,
   1752065,     107,         -45667.24,   -7.2945676,
   -347856.475, 23.04        -1.0,        356047.56
};

#if defined(__mips_hard_float)
#define TEST1(mem)                                           \
{                                                            \
   unsigned long long out;                                   \
   __asm__ __volatile__(                                     \
      ".set  noreorder"  "\n\t"                              \
      ".set  nomacro"    "\n\t"                              \
      "move  $t0, %1"    "\n\t"                              \
      "mtc1  $t0, $f0"   "\n\t"                              \
      "mov.s $f1, $f0"   "\n\t"                              \
      "mfc1  $t1, $f1"   "\n\t"                              \
      "move  %0,  $t1"   "\n\t"                              \
      ".set  reorder"    "\n\t"                              \
      ".set  macro"      "\n\t"                              \
      : "=r" (out)                                           \
      : "r" (mem)                                            \
      : "t0", "t1", "$f0", "$f1"                             \
   );                                                        \
   printf("mtc1, mov.s, mfc1 :: mem: 0x%llx out: 0x%llx\n",  \
          (long long)mem, out);                              \
}

#define TEST2(mem)                                             \
{                                                              \
   unsigned long long out;                                     \
   __asm__ __volatile__(                                       \
      ".set  noreorder"  "\n\t"                                \
      ".set  nomacro"    "\n\t"                                \
      "move  $t0, %1"    "\n\t"                                \
      "dmtc1 $t0, $f2"   "\n\t"                                \
      "mov.d $f0, $f2"   "\n\t"                                \
      "dmfc1 $t1, $f0"   "\n\t"                                \
      "move  %0,  $t1"   "\n\t"                                \
      ".set  reorder"  "\n\t"                                  \
      ".set  macro"    "\n\t"                                  \
      : "=r" (out)                                             \
      : "r" (mem)                                              \
      : "t0", "t1", "$f0", "$f2"                               \
   );                                                          \
   printf("dmtc1, mov.d, dmfc1 :: mem: 0x%llx out: 0x%llx\n",  \
          (long long)mem, out);                                \
}
#if (__mips_isa_rev < 6)
/* movX.s fd, fs */
#define TEST3(instruction, FD, FS, cc, offset)    \
{                                                 \
   unsigned int out;                              \
   __asm__ __volatile__(                          \
      "li     $t0,    1"               "\n\t"     \
      "move   $t1,    %1"              "\n\t"     \
      "mtc1   $t0,    $f0"             "\n\t"     \
      "mtc1   $t1,    $f2"             "\n\t"     \
      "dmtc1  $zero,  $"#FD            "\n\t"     \
      "dmtc1  $zero,  $"#FS            "\n\t"     \
      "c.eq.s $f0,    $f2"             "\n\t"     \
      "move   $t0,    %2"              "\n\t"     \
      "lwc1   $"#FS", "#offset"($t0)"  "\n\t"     \
      instruction                      "\n\t"     \
      "mfc1   %0,     $"#FD            "\n\t"     \
      : "=r" (out)                                \
      : "r" (cc), "r" (reg_val_f)                 \
      : "t0", "t1", "$"#FD, "$"#FS, "$f0", "$f2"  \
   );                                             \
   printf("%s :: out: 0x%x, cc: %d\n",            \
          instruction, out, cc);                  \
}

/* movX.d fd, fs */
#define TEST3d(instruction, FD, FS, cc, offset)   \
{                                                 \
   unsigned long long out;                        \
   __asm__ __volatile__(                          \
      "li     $t0,    1"               "\n\t"     \
      "move   $t1,    %1"              "\n\t"     \
      "mtc1   $t0,    $f0"             "\n\t"     \
      "mtc1   $t1,    $f2"             "\n\t"     \
      "dmtc1  $zero,  $"#FD            "\n\t"     \
      "c.eq.s $f0,    $f2"             "\n\t"     \
      "move   $t0,    %2"              "\n\t"     \
      "ldc1   $"#FS", "#offset"($t0)"  "\n\t"     \
      instruction                      "\n\t"     \
      "dmfc1  %0,     $"#FD            "\n\t"     \
      : "=r" (out)                                \
      : "r" (cc), "r" (reg_val_f)                 \
      : "t0", "t1", "$"#FD, "$"#FS, "$f0", "$f2"  \
   );                                             \
   printf("%s :: out: 0x%llx, cc: %d\n",          \
          instruction, out, cc);                  \
}
#endif

/* movX.s fd, fs, rt */
#define TEST4(instruction, offset, RTval, FD, FS, RT)  \
{                                                      \
   unsigned int out;                                   \
   __asm__ __volatile__(                               \
      "move  $"#RT", %2"              "\n\t"           \
      "dmtc1 $zero,  $"#FD            "\n\t"           \
      "dmtc1 $zero,  $"#FS            "\n\t"           \
      "move  $t0,    %1"              "\n\t"           \
      "lwc1  $"#FS", "#offset"($t0)"  "\n\t"           \
      instruction                     "\n\t"           \
      "mfc1 %0,     $"#FD"\n\t"                        \
      : "=r" (out)                                     \
      : "r" (reg_val_f), "r" (RTval)                   \
      : "t0", #RT, "$"#FD, "$"#FS                      \
   );                                                  \
   printf("%s :: out: 0x%x\n", instruction, out);      \
}

/* movX.d fd, fs, rt */
#define TEST4d(instruction, offset, RTval, FD, FS, RT)  \
{                                                       \
   unsigned long long out;                              \
   __asm__ __volatile__(                                \
      "move  $"#RT", %2"              "\n\t"            \
      "dmtc1 $zero,  $"#FD            "\n\t"            \
      "dmtc1 $zero,  $"#FS            "\n\t"            \
      "move  $t0,    %1"              "\n\t"            \
      "ldc1  $"#FS", "#offset"($t0)"  "\n\t"            \
      instruction                     "\n\t"            \
      "dmfc1 %0,     $"#FD            "\n\t"            \
      : "=r" (out)                                      \
      : "r" (reg_val_f), "r" (RTval)                    \
      : #RT, "t0", "$"#FD, "$"#FS                       \
   );                                                   \
   printf("%s :: out: 0x%llx\n", instruction, out);     \
}

#if (__mips_isa_rev < 6)
/* movf, movt */
#define TEST5(instruction, RDval, RSval, RD, RS)                  \
{                                                                 \
   unsigned long long out;                                        \
   __asm__ __volatile__(                                          \
      "c.eq.s      %3,     %4"             "\n\t"                 \
      "move        $"#RD", %1"             "\n\t"                 \
      "move        $"#RS", %2"             "\n\t"                 \
      instruction" $"#RD", $"#RS", $fcc0"  "\n\t"                 \
      "move        %0,     $"#RD           "\n\t"                 \
      : "=r" (out)                                                \
      : "r" (RDval), "r" (RSval), "f" (fs1_f[i]), "f" (fs2_f[i])  \
      : #RD, #RS                                                  \
   );                                                             \
   printf("%s ::  RDval: 0x%x, RSval: 0x%x, out: 0x%llx\n",       \
          instruction, RDval, RSval, out);                        \
}
#endif
#endif

int main()
{
#if defined(__mips_hard_float)
   int i;
   init_reg_val2();

   for (i = 0; i < N; i++) {
      TEST1(reg_val1[i]);
      TEST2(reg_val1[i]);
      TEST1(reg_val2[i]);
      TEST2(reg_val2[i]);
   }

#if (__mips_isa_rev < 6)
   printf("--- MOVF.S ---\n");
   TEST3("movf.s  $f4, $f6, $fcc0", f4, f6, 1, 0);
   TEST3("movf.s  $f4, $f6, $fcc0", f4, f6, 1, 8);
   TEST3("movf.s  $f4, $f6, $fcc0", f4, f6, 1, 16);
   TEST3("movf.s  $f4, $f6, $fcc0", f4, f6, 1, 24);
   TEST3("movf.s  $f4, $f6, $fcc0", f4, f6, 1, 32)
   TEST3("movf.s  $f4, $f6, $fcc0", f4, f6, 1, 40)
   TEST3("movf.s  $f4, $f6, $fcc0", f4, f6, 1, 48)
   TEST3("movf.s  $f4, $f6, $fcc0", f4, f6, 1, 56)
   TEST3("movf.s  $f4, $f6, $fcc0", f4, f6, 0, 0);
   TEST3("movf.s  $f4, $f6, $fcc0", f4, f6, 0, 8);
   TEST3("movf.s  $f4, $f6, $fcc0", f4, f6, 0, 16);
   TEST3("movf.s  $f4, $f6, $fcc0", f4, f6, 0, 24);
   TEST3("movf.s  $f4, $f6, $fcc0", f4, f6, 0, 32);
   TEST3("movf.s  $f4, $f6, $fcc0", f4, f6, 0, 40);
   TEST3("movf.s  $f4, $f6, $fcc0", f4, f6, 0, 48);
   TEST3("movf.s  $f4, $f6, $fcc0", f4, f6, 0, 56);

   printf("--- MOVF.D ---\n");
   TEST3d("movf.d  $f4, $f6, $fcc0", f4, f6, 1, 0);
   TEST3d("movf.d  $f4, $f6, $fcc0", f4, f6, 1, 8);
   TEST3d("movf.d  $f4, $f6, $fcc0", f4, f6, 1, 16);
   TEST3d("movf.d  $f4, $f6, $fcc0", f4, f6, 1, 24);
   TEST3d("movf.d  $f4, $f6, $fcc0", f4, f6, 1, 32);
   TEST3d("movf.d  $f4, $f6, $fcc0", f4, f6, 1, 40)
   TEST3d("movf.d  $f4, $f6, $fcc0", f4, f6, 1, 48)
   TEST3d("movf.d  $f4, $f6, $fcc0", f4, f6, 1, 56)
   TEST3d("movf.d  $f4, $f6, $fcc0", f4, f6, 0, 0);
   TEST3d("movf.d  $f4, $f6, $fcc0", f4, f6, 0, 8);
   TEST3d("movf.d  $f4, $f6, $fcc0", f4, f6, 0, 16);
   TEST3d("movf.d  $f4, $f6, $fcc0", f4, f6, 0, 24);
   TEST3d("movf.d  $f4, $f6, $fcc0", f4, f6, 0, 32);
   TEST3d("movf.d  $f4, $f6, $fcc0", f4, f6, 0, 40);
   TEST3d("movf.d  $f4, $f6, $fcc0", f4, f6, 0, 48);
   TEST3d("movf.d  $f4, $f6, $fcc0", f4, f6, 0, 56);

   printf("--- MOVN.S ---\n");
   TEST4("movn.s  $f0, $f2, $11", 0,  0,          f0, f2, 11);
   TEST4("movn.s  $f0, $f2, $11", 0,  1,          f0, f2, 11);
   TEST4("movn.s  $f0, $f2, $11", 8,  0xffff,     f0, f2, 11);
   TEST4("movn.s  $f0, $f2, $11", 16, -1,         f0, f2, 11);
   TEST4("movn.s  $f0, $f2, $11", 16, 5,          f0, f2, 11);
   TEST4("movn.s  $f0, $f2, $11", 24, 0,          f0, f2, 11);
   TEST4("movn.s  $f0, $f2, $11", 24, 0,          f0, f2, 11);
   TEST4("movn.s  $f0, $f2, $11", 32, 5,          f0, f2, 11);
   TEST4("movn.s  $f0, $f2, $11", 32, 125487,     f0, f2, 11);
   TEST4("movn.s  $f0, $f2, $11", 40, 68,         f0, f2, 11);
   TEST4("movn.s  $f0, $f2, $11", 40, -122544,    f0, f2, 11);
   TEST4("movn.s  $f0, $f2, $11", 48, 0,          f0, f2, 11);
   TEST4("movn.s  $f0, $f2, $11", 48, 0,          f0, f2, 11);
   TEST4("movn.s  $f0, $f2, $11", 56, 0xffffffff, f0, f2, 11);
   TEST4("movn.s  $f0, $f2, $11", 56, 0x80000000, f0, f2, 11);
   TEST4("movn.s  $f0, $f2, $11", 64, 0x7fffffff, f0, f2, 11);

   printf("--- MOVN.D ---\n");
   TEST4d("movn.d $f0, $f2, $11", 0,  0,          f0, f2, 11);
   TEST4d("movn.d $f0, $f2, $11", 0,  1,          f0, f2, 11);
   TEST4d("movn.d $f0, $f2, $11", 8,  0xffff,     f0, f2, 11);
   TEST4d("movn.d $f0, $f2, $11", 8,  -1,         f0, f2, 11);
   TEST4d("movn.d $f0, $f2, $11", 16, 5,          f0, f2, 11);
   TEST4d("movn.d $f0, $f2, $11", 24, 0,          f0, f2, 11);
   TEST4d("movn.d $f0, $f2, $11", 24, 0,          f0, f2, 11);
   TEST4d("movn.d $f0, $f2, $11", 32, 5,          f0, f2, 11);
   TEST4d("movn.d $f0, $f2, $11", 32, 125487,     f0, f2, 11);
   TEST4d("movn.d $f0, $f2, $11", 40, 68,         f0, f2, 11);
   TEST4d("movn.d $f0, $f2, $11", 40, -122544,    f0, f2, 11);
   TEST4d("movn.d $f0, $f2, $11", 48, 0,          f0, f2, 11);
   TEST4d("movn.d $f0, $f2, $11", 48, 0,          f0, f2, 11);
   TEST4d("movn.d $f0, $f2, $11", 56, 0xffffffff, f0, f2, 11);
   TEST4d("movn.d $f0, $f2, $11", 56, 0x80000000, f0, f2, 11);
   TEST4d("movn.d $f0, $f2, $11", 64, 0x7fffffff, f0, f2, 11);

   printf("--- MOVT.S ---\n");
   TEST3("movt.s  $f4, $f6, $fcc0", f4, f6, 1, 0);
   TEST3("movt.s  $f4, $f6, $fcc0", f4, f6, 1, 0);
   TEST3("movt.s  $f4, $f6, $fcc0", f4, f6, 1, 8);
   TEST3("movt.s  $f4, $f6, $fcc0", f4, f6, 1, 16);
   TEST3("movt.s  $f4, $f6, $fcc0", f4, f6, 1, 24);
   TEST3("movt.s  $f4, $f6, $fcc0", f4, f6, 1, 32);
   TEST3("movt.s  $f4, $f6, $fcc0", f4, f6, 1, 40)
   TEST3("movt.s  $f4, $f6, $fcc0", f4, f6, 1, 48)
   TEST3("movt.s  $f4, $f6, $fcc0", f4, f6, 1, 56)
   TEST3("movt.s  $f4, $f6, $fcc0", f4, f6, 0, 0);
   TEST3("movt.s  $f4, $f6, $fcc0", f4, f6, 0, 8);
   TEST3("movt.s  $f4, $f6, $fcc0", f4, f6, 0, 16);
   TEST3("movt.s  $f4, $f6, $fcc0", f4, f6, 0, 24);
   TEST3("movt.s  $f4, $f6, $fcc0", f4, f6, 0, 32);
   TEST3("movt.s  $f4, $f6, $fcc0", f4, f6, 0, 40);
   TEST3("movt.s  $f4, $f6, $fcc0", f4, f6, 0, 48);
   TEST3("movt.s  $f4, $f6, $fcc0", f4, f6, 0, 56);

   printf("--- MOVT.D ---\n");
   TEST3d("movt.d  $f4, $f6, $fcc0", f4, f6, 1, 0);
   TEST3d("movt.d  $f4, $f6, $fcc0", f4, f6, 1, 0);
   TEST3d("movt.d  $f4, $f6, $fcc0", f4, f6, 1, 8);
   TEST3d("movt.d  $f4, $f6, $fcc0", f4, f6, 1, 16);
   TEST3d("movt.d  $f4, $f6, $fcc0", f4, f6, 1, 24);
   TEST3d("movt.d  $f4, $f6, $fcc0", f4, f6, 1, 32);
   TEST3d("movt.d  $f4, $f6, $fcc0", f4, f6, 1, 40)
   TEST3d("movt.d  $f4, $f6, $fcc0", f4, f6, 1, 48)
   TEST3d("movt.d  $f4, $f6, $fcc0", f4, f6, 1, 56)
   TEST3d("movt.d  $f4, $f6, $fcc0", f4, f6, 0, 0);
   TEST3d("movt.d  $f4, $f6, $fcc0", f4, f6, 0, 8);
   TEST3d("movt.d  $f4, $f6, $fcc0", f4, f6, 0, 16);
   TEST3d("movt.d  $f4, $f6, $fcc0", f4, f6, 0, 24);
   TEST3d("movt.d  $f4, $f6, $fcc0", f4, f6, 0, 32);
   TEST3d("movt.d  $f4, $f6, $fcc0", f4, f6, 0, 40);
   TEST3d("movt.d  $f4, $f6, $fcc0", f4, f6, 0, 48);
   TEST3d("movt.d  $f4, $f6, $fcc0", f4, f6, 0, 56);

   printf("--- MOVZ.S ---\n");
   TEST4("movz.s $f0, $f2, $11", 0,  0,          f0, f2, 11);
   TEST4("movz.s $f0, $f2, $11", 8,  1,          f0, f2, 11);
   TEST4("movz.s $f0, $f2, $11", 8,  0xffff,     f0, f2, 11);
   TEST4("movz.s $f0, $f2, $11", 16, -1,         f0, f2, 11);
   TEST4("movz.s $f0, $f2, $11", 16, 5,          f0, f2, 11);
   TEST4("movz.s $f0, $f2, $11", 24, 0,          f0, f2, 11);
   TEST4("movz.s $f0, $f2, $11", 24, 0,          f0, f2, 11);
   TEST4("movz.s $f0, $f2, $11", 32, 5,          f0, f2, 11);
   TEST4("movz.s $f0, $f2, $11", 32, 125487,     f0, f2, 11);
   TEST4("movz.s $f0, $f2, $11", 40, 68,         f0, f2, 11);
   TEST4("movz.s $f0, $f2, $11", 40, -122544,    f0, f2, 11);
   TEST4("movz.s $f0, $f2, $11", 48, 0,          f0, f2, 11);
   TEST4("movz.s $f0, $f2, $11", 48, 0,          f0, f2, 11);
   TEST4("movz.s $f0, $f2, $11", 56, 0xffffffff, f0, f2, 11);
   TEST4("movz.s $f0, $f2, $11", 56, 0x80000000, f0, f2, 11);
   TEST4("movz.s $f0, $f2, $11", 64, 0x7fffffff, f0, f2, 11);

   printf("--- MOVZ.D ---\n");
   TEST4d("movz.d $f0, $f2, $11", 0,  0,          f0, f2, 11);
   TEST4d("movz.d $f0, $f2, $11", 0,  1,          f0, f2, 11);
   TEST4d("movz.d $f0, $f2, $11", 8,  0xffff,     f0, f2, 11);
   TEST4d("movz.d $f0, $f2, $11", 16, -1,         f0, f2, 11);
   TEST4d("movz.d $f0, $f2, $11", 16, 5,          f0, f2, 11);
   TEST4d("movz.d $f0, $f2, $11", 24, 0,          f0, f2, 11);
   TEST4d("movz.d $f0, $f2, $11", 24, 0,          f0, f2, 11);
   TEST4d("movz.d $f0, $f2, $11", 32, 5,          f0, f2, 11);
   TEST4d("movz.d $f0, $f2, $11", 32, 125487,     f0, f2, 11);
   TEST4d("movz.d $f0, $f2, $11", 40, 68,         f0, f2, 11);
   TEST4d("movz.d $f0, $f2, $11", 40, -122544,    f0, f2, 11);
   TEST4d("movz.d $f0, $f2, $11", 48, 0,          f0, f2, 11);
   TEST4d("movz.d $f0, $f2, $11", 48, 0,          f0, f2, 11);
   TEST4d("movz.d $f0, $f2, $11", 56, 0xffffffff, f0, f2, 11);
   TEST4d("movz.d $f0, $f2, $11", 56, 0x80000000, f0, f2, 11);
   TEST4d("movz.d $f0, $f2, $11", 64, 0x7fffffff, f0, f2, 11);

   printf("--- MOVF --- if FPConditionalCode(cc) == 0 then "
          "out = RSval else out = RDval\n");
   for (i = 0; i < 24; i++) {
      TEST5("movf", 0xaaaaaaaa, 0x80000000, t0, t1);
      TEST5("movf", 0xccccffff, 0xffffffff, t1, t2);
      TEST5("movf", 0xffffaaaa, 0xaaaaffff, t3, t1);
      TEST5("movf", 0x0,        0xffffffff, t3, t0);
   }

   printf("--- MOVT --- if FPConditionalCode(cc) == 1 then "
          "out = RSval else out = RDval\n");
   for (i = 0; i < 24; i++) {
      TEST5("movt", 0x0,        0xffffffff, t0, t1);
      TEST5("movt", 0x11111111, 0xeeeeffff, t1, t2);
      TEST5("movt", 0x5555ffff, 0xffffffff, t3, t1);
      TEST5("movt", 0xeeeeeeee, 0xffffeeee, t3, t0);
   }
#endif
#endif
   return 0;
}
