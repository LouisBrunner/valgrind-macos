#include "const.h"

#define TEST1(instruction, offset, mem)        \
{                                              \
   RegWord out = 0;                            \
   __asm__ __volatile__(                       \
      "move        $t0, %1"        "\n\t"      \
      "move        $t1, %2"        "\n\t"      \
      "daddu       $t0, $t0, $t1"  "\n\t"      \
      "move        $t1, $zero"     "\n\t"      \
      instruction" $t1, 0($t0)"    "\n\t"      \
      "move        %0,  $t1"       "\n\t"      \
      : "=r" (out)                             \
      : "r" (mem), "r" (offset)                \
      : "t0", "t1"                             \
   );                                          \
   printf("%s :: offset: 0x%x, out: 0x%"       \
          FMT_REGWORD "x\n",  instruction,     \
          offset, out);                        \
}

#define TEST2(instruction, offset)                           \
{                                                            \
   RegWord out = 0;                                          \
   RegWord outHI = 0;                                        \
   __asm__ __volatile__(                                     \
      "move        $t0, %2"          "\n\t"                  \
      "move        $t1, %4"          "\n\t"                  \
      "daddu       $t0, $t0, $t1"    "\n\t"                  \
      "ld          $t3, 0($t0)"      "\n\t"                  \
      "move        $t1, %3"          "\n\t"                  \
      "move        $t2, %4"          "\n\t"                  \
      "daddu       $t1, $t1, $t2"    "\n\t"                  \
      instruction" $t3, 0($t1)"      "\n\t"                  \
      "li          $t2, 7"           "\n\t"                  \
      "nor         $t2, $t2, $zero"  "\n\t"                  \
      "and         $t1, $t1, $t2"    "\n\t"                  \
      "ld          %0,  0($t1)"      "\n\t"                  \
      "ld          %1,  8($t1)"      "\n\t"                  \
      : "=r" (out), "=r" (outHI)                             \
      : "r" (reg_val2) , "r" (reg_val_zero), "r" (offset)    \
      : "t0", "t1", "t2", "t3"                               \
   );                                                        \
   printf("%s :: offset: 0x%x, out: 0x%" FMT_REGWORD "x, "   \
          "outHI: 0x%" FMT_REGWORD "x\n", instruction,       \
          offset, out, outHI);                               \
}

#define TEST3(instruction, offset, mem)         \
{                                               \
   RegWord out = 0;                             \
   __asm__ __volatile__(                        \
      "move        $t0,   %1"        "\n\t"     \
      "move        $t1,   %2"        "\n\t"     \
      "daddu       $t0,   $t0, $t1"  "\n\t"     \
      "dmtc1       $zero, $f0"       "\n\t"     \
      instruction" $f0,  0($t0)"     "\n\t"     \
      "dmfc1       %0,    $f0"       "\n\t"     \
      : "=r" (out)                              \
      : "r" (mem) , "r" (offset)                \
      : "t0", "t1", "$f0"                       \
   );                                           \
   printf("%s :: offset: 0x%x, out: 0x%"        \
          FMT_REGWORD "x\n", instruction,       \
          offset, out);                         \
}

#define TEST3w(instruction, offset, mem)      \
{                                             \
   unsigned int out = 0;                      \
   __asm__ __volatile__(                      \
      "move        $t0,   %1"        "\n\t"   \
      "move        $t1,   %2"        "\n\t"   \
      "daddu       $t0,   $t0, $t1"  "\n\t"   \
      "dmtc1       $zero, $f0"       "\n\t"   \
      instruction" $f0,  0($t0)"     "\n\t"   \
      "mfc1        %0,    $f0"       "\n\t"   \
      : "=r" (out)                            \
      : "r" (mem) , "r" (offset)              \
      : "t0", "t1", "$f0"                     \
   );                                         \
   printf("%s :: offset: 0x%x, out: 0x%x\n",  \
          instruction, offset, out);          \
}

#define TEST4(instruction, offset)                         \
{                                                          \
   RegWord out = 0;                                        \
   __asm__ __volatile__(                                   \
      "move        $t0, %1"        "\n\t"                  \
      "move        $t1, %3"        "\n\t"                  \
      "daddu       $t0, $t0, $t1"  "\n\t"                  \
      "ld          $t2, 0($t0)"    "\n\t"                  \
      "move        $t0, %2"        "\n\t"                  \
      "daddu       $t0, $t0, $t1"  "\n\t"                  \
      "dmtc1       $t2, $f0"       "\n\t"                  \
      instruction" $f0, 0($t0)"    "\n\t"                  \
      "ld          %0,  0($t0)"    "\n\t"                  \
      : "=r" (out)                                         \
      : "r" (reg_val1) , "r" (reg_val_zero), "r" (offset)  \
      : "t0", "t1", "t2", "$f0"                            \
   );                                                      \
   printf("%s :: offset: 0x%x, out: 0x%" FMT_REGWORD "x\n",\
          instruction, offset, out);                       \
}

#define TEST5(instruction, offset, mem)         \
{                                               \
   RegWord out = 0;                             \
   __asm__ __volatile__(                        \
      "move        $t0,   %1"        "\n\t"     \
      "move        $t1,   %2"        "\n\t"     \
      "dmtc1       $zero, $f0"       "\n\t"     \
      instruction" $f0,   $t1($t0)"  "\n\t"     \
      "dmfc1       %0,    $f0"       "\n\t"     \
      : "=r" (out)                              \
      : "r" (mem) , "r" (offset)                \
      : "t0", "t1", "$f0"                       \
   );                                           \
   printf("%s :: offset: 0x%x, out: 0x%"        \
          FMT_REGWORD "x\n", instruction,       \
          offset, out);                         \
}

#define TEST5w(instruction, offset, mem)      \
{                                             \
   unsigned int out = 0;                      \
   __asm__ __volatile__(                      \
      "move        $t0,   %1"        "\n\t"   \
      "move        $t1,   %2"        "\n\t"   \
      "dmtc1       $zero, $f0"       "\n\t"   \
      instruction" $f0,   $t1($t0)"  "\n\t"   \
      "mfc1        %0,    $f0"       "\n\t"   \
      : "=r" (out)                            \
      : "r" (mem) , "r" (offset)              \
      : "t0", "t1", "$f0"                     \
   );                                         \
   printf("%s :: offset: 0x%x, out: 0x%x\n",  \
          instruction, offset, out);          \
}

#define TEST6(instruction, offset)                         \
{                                                          \
   RegWord out = 0;                                        \
   __asm__ __volatile__(                                   \
      "move        $t0, %1"        "\n\t"                  \
      "move        $t1, %3"        "\n\t"                  \
      "daddu       $t0, $t0, $t1"  "\n\t"                  \
      "ld          $t3, 0($t0)"    "\n\t"                  \
      "move        $t1, %2"        "\n\t"                  \
      "move        $t2, %3"        "\n\t"                  \
      "daddu       $t1, $t1, $t2"  "\n\t"                  \
      "dmtc1       $t3, $f3"       "\n\t"                  \
      "move        $t0, %3"        "\n\t"                  \
      instruction" $f3, $t0($t1)"  "\n\t"                  \
      "ld          %0,  0($t1)"    "\n\t"                  \
      : "=r" (out)                                         \
      : "r" (reg_val2) , "r" (reg_val_zero), "r" (offset)  \
      : "t0", "t1", "t2", "t3"                             \
   );                                                      \
   printf("%s :: offset: 0x%x, out: 0x%" FMT_REGWORD "x\n",\
          instruction, offset, out);                       \
}
