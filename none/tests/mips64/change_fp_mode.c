#if defined(__mips_hard_float)

#include <elf.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/prctl.h>
#include "pub_core_basics.h"

#if !defined(PR_SET_FP_MODE)
#   define PR_SET_FP_MODE 45
#endif

#if !defined(PR_GET_FP_MODE)
#   define PR_GET_FP_MODE 46
#endif

#define TEST_LD(instruction, source)                              \
{                                                                 \
   unsigned int result1, result2;                                 \
   __asm__ volatile(                                              \
      ".set push\n\t"                                             \
      ".set noreorder\n\t"                                        \
      "li $t0, 0x5a5a\n\t"                                        \
      "mtc1 $t0, $f0\n\t"                                         \
      "mtc1 $t0, $f1\n\t"                                         \
      "move $t0, %2\n\t"                                          \
      instruction"\n\t"                                           \
      "swc1 $f0, %0\n\t"                                          \
      "swc1 $f1, %1\n\t"                                          \
      ".set pop\n\t"                                              \
      : "=m"(result1), "=m"(result2)                              \
      : "r" (&source)                                             \
      : "t0", "$f0", "$f1");                                      \
   printf(instruction" :: lo32(f1): %x, lo32(f0): %x\n",          \
          result2, result1);                                      \
}

#define _TEST_ST(instruction)                                     \
   __asm__ volatile(                                              \
      ".set push\n\t"                                             \
      ".set noreorder\n\t"                                        \
      "li $t0, 0x5a5a\n\t"                                        \
      "dmtc1 $t0, $f1\n\t"                                        \
      "move $t0, %0\n\t"                                          \
      "ldc1 $f0, 0($t0)\n\t"                                      \
      "move $t0, %1\n\t"                                          \
      instruction"\n\t"                                           \
      ".set pop\n\t"                                              \
      :                                                           \
      : "r" (&source64), "r" (&result)                            \
      : "t0", "$f0", "$f1", "memory")

#define TEST_ST64(instruction)                                    \
{                                                                 \
   RegWord result;                                                \
   _TEST_ST(instruction);                                         \
   printf(instruction" :: mem: %" FMT_REGWORD "x\n", result);     \
}

#define TEST_ST32(instruction)                                    \
{                                                                 \
   unsigned int result;                                           \
   _TEST_ST(instruction);                                         \
   printf(instruction" :: mem: %x\n", result);                    \
}

#define TEST_MT(instruction)                                      \
{                                                                 \
   unsigned int result1, result2;                                 \
   __asm__ volatile(                                              \
      ".set push\n\t"                                             \
      ".set noreorder\n\t"                                        \
      "li $t0, 0x5a5a\n\t"                                        \
      "mtc1 $t0, $f0\n\t"                                         \
      "mtc1 $t0, $f1\n\t"                                         \
      "ld $t0, %2\n\t"                                            \
      instruction"\n\t"                                           \
      "swc1 $f0, %0\n\t"                                          \
      "swc1 $f1, %1\n\t"                                          \
      ".set pop\n\t"                                              \
      : "=m"(result1), "=m"(result2)                              \
      : "m" (source64)                                            \
      : "t0", "$f0", "$f1");                                      \
   printf(instruction" :: lo32(f1): %x, lo32(f0): %x\n",          \
          result2, result1);                                      \
}

#define TEST_MF(instruction)                                      \
{                                                                 \
   RegWord result;                                                \
   __asm__ volatile(                                              \
      ".set push\n\t"                                             \
      ".set noreorder\n\t"                                        \
      "li $t0, 0x5a5a\n\t"                                        \
      "dmtc1 $t0, $f1\n\t"                                        \
      "ldc1 $f0, %1\n\t"                                          \
      "move $t0, $0\n\t"                                          \
      instruction"\n\t"                                           \
      "sd $t0, %0\n\t"                                            \
      ".set pop\n\t"                                              \
      : "=m" (result)                                             \
      : "m" (source64)                                            \
      : "t0", "$f0", "$f1");                                      \
   printf(instruction" :: t0: %" FMT_REGWORD "x\n", result);      \
}

#define TEST_MOVE(instruction)                                    \
{                                                                 \
   unsigned int result1, result2;                                 \
   __asm__ volatile(                                              \
      ".set push\n\t"                                             \
      ".set noreorder\n\t"                                        \
      "li $t0, 0x5a5a\n\t"                                        \
      "mtc1 $t0, $f0\n\t"                                         \
      "li $t0, 0x6b6b\n\t"                                        \
      "mtc1 $t0, $f1\n\t"                                         \
      "li $t0, 0x7c7c\n\t"                                        \
      "dmtc1 $t0, $f2\n\t"                                        \
      "ldc1 $f2, %2\n\t"                                          \
      instruction"\n\t"                                           \
      "swc1 $f0, %0\n\t"                                          \
      "swc1 $f1, %1\n\t"                                          \
      ".set pop\n\t"                                              \
      : "=m"(result1), "=m"(result2)                              \
      : "m" (source64)                                            \
      : "t0", "$f0", "$f1", "$f2");                               \
   printf(instruction" :: lo32(f1): %x, lo32(f0): %x\n",          \
          result2, result1);                                      \
}

ULong source64 = 0x1234567890abcdefull;
unsigned int  source32 = 0x12345678u;

/* Determine FP mode based on sdc1 behavior
   returns 1 if FR = 1 mode is detected (assumes FRE = 0) */
static int get_fp_mode(void) {
   unsigned long long result = 0;
   __asm__ volatile(
      ".set push\n\t"
      ".set noreorder\n\t"
      "lui $t0, 0x3ff0\n\t"
      "ldc1 $f0, %0\n\t"
      "mtc1 $t0, $f1\n\t"
      "sdc1 $f0, %0\n\t"
      ".set pop\n\t"
      : "+m"(result)
      :
      : "t0", "$f0", "$f1", "memory");

   return (result != 0x3ff0000000000000ull);
}

static void fatal_error(const char* msg) {
   fprintf(stderr, "Error: %s\n", msg);
   exit(1);
}

static void test(int* fr_prctl, int* fr_detected) {
#if (__mips_isa_rev<6)
   *fr_prctl = prctl(PR_GET_FP_MODE);
   *fr_detected = get_fp_mode();

   if (*fr_prctl < 0) {
      fatal_error("prctl(PR_GET_FP_MODE) fails.");
   }

   printf("fr_prctl: %d, fr_detected: %d\n", *fr_prctl, *fr_detected);

   if (*fr_prctl != *fr_detected) {
      fatal_error("fr_prctl != fr_detected");
   }

   TEST_LD("lwc1 $f0, 0($t0)", source32);
   TEST_LD("lwc1 $f1, 0($t0)", source32);

   TEST_LD("lwxc1 $f0, $0($t0)", source32);
   TEST_LD("lwxc1 $f1, $0($t0)", source32);

   TEST_LD("ldc1 $f0, 0($t0)", source64);
   TEST_LD("ldc1 $f1, 0($t0)", source64);

   TEST_LD("ldxc1 $f0, $0($t0)", source64);
   TEST_LD("ldxc1 $f1, $0($t0)", source64);

   TEST_ST32("swc1 $f0, 0($t0)");
   TEST_ST32("swc1 $f1, 0($t0)");

   TEST_ST32("swxc1 $f0, $0($t0)");
   TEST_ST32("swxc1 $f1, $0($t0)");

   TEST_ST64("sdc1 $f0, 0($t0)");
   TEST_ST64("sdc1 $f1, 0($t0)");

   TEST_ST64("sdxc1 $f0, $0($t0)");
   TEST_ST64("sdxc1 $f1, $0($t0)");

   TEST_MT("mtc1 $t0, $f0");
   TEST_MT("mtc1 $t0, $f1");

   TEST_MT("dmtc1 $t0, $f0");
   TEST_MT("dmtc1 $t0, $f1");

   TEST_MF("mfc1 $t0, $f0");
   TEST_MF("mfc1 $t0, $f1");

   TEST_MF("dmfc1 $t0, $f0");
   TEST_MF("dmfc1 $t0, $f1");

   TEST_MOVE("movn.s $f0, $f2, $t0");
   TEST_MOVE("movn.s $f0, $f1, $t0");
   TEST_MOVE("movn.s $f1, $f2, $t0");
   TEST_MOVE("movn.s $f0, $f2, $0");
   TEST_MOVE("movn.s $f0, $f1, $0");
   TEST_MOVE("movn.s $f1, $f2, $0");

   TEST_MOVE("movn.d $f0, $f2, $t0");
   TEST_MOVE("movn.d $f0, $f1, $t0");
   TEST_MOVE("movn.d $f1, $f2, $t0");
   TEST_MOVE("movn.d $f0, $f2, $0");
   TEST_MOVE("movn.d $f0, $f1, $0");
   TEST_MOVE("movn.d $f1, $f2, $0");

   TEST_MOVE("movz.s $f0, $f2, $t0");
   TEST_MOVE("movz.s $f0, $f1, $t0");
   TEST_MOVE("movz.s $f1, $f2, $t0");
   TEST_MOVE("movz.s $f0, $f2, $0");
   TEST_MOVE("movz.s $f0, $f1, $0");
   TEST_MOVE("movz.s $f1, $f2, $0");

   TEST_MOVE("movz.d $f0, $f2, $t0");
   TEST_MOVE("movz.d $f0, $f1, $t0");
   TEST_MOVE("movz.d $f1, $f2, $t0");
   TEST_MOVE("movz.d $f0, $f2, $0");
   TEST_MOVE("movz.d $f0, $f1, $0");
   TEST_MOVE("movz.d $f1, $f2, $0");
#endif
}

int main() {
   int fr_prctl, fr_detected;

   test(&fr_prctl, &fr_detected);

   /* FP64 */
   if (fr_prctl == 1) {

      /* Change mode to FP32 */
      if (prctl(PR_SET_FP_MODE, 0) != 0) {
         fatal_error("prctl(PR_SET_FP_MODE, 0) fails.");
      }

      test(&fr_prctl, &fr_detected);

      /* Change back FP mode */
      if (prctl(PR_SET_FP_MODE, 1) != 0) {
         fatal_error("prctl(PR_SET_FP_MODE, 1) fails.");
      }

      test(&fr_prctl, &fr_detected);
   }

   return 0;
}
#else
int main() {
   return 0;
}
#endif
