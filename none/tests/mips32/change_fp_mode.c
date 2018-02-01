#if defined(__mips_hard_float)

#include <elf.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/prctl.h>

#if !defined(PR_SET_FP_MODE)
#   define PR_SET_FP_MODE 45
#endif

#if !defined(PR_GET_FP_MODE)
#   define PR_GET_FP_MODE 46
#endif

/* Determine FP mode based on sdc1 behavior
   returns 1 if FR = 1 mode is detected. */

static int get_fp_mode(void) {
   unsigned long long result = 0;
   __asm__ volatile(
      ".set push\n\t"
      ".set noreorder\n\t"
      ".set oddspreg\n\t"
      "lui $t0, 0x3FF0\n\t"
      "ldc1 $f0, %0\n\t"
      "mtc1 $t0, $f1\n\t"
      "sdc1 $f0, %0\n\t"
      ".set pop\n\t"
      : "+m"(result)
      :
      : "t0", "$f0", "$f1", "memory");

   return (result != 0x3FF0000000000000ull);
}

static void fatal_error(const char* msg) {
   fprintf(stderr, "Error: %s\n", msg);
   exit(1);
}

static void test(int* fr_prctl, int* fr_detected) {
   *fr_prctl = prctl(PR_GET_FP_MODE);
   *fr_detected = get_fp_mode();

   if (*fr_prctl < 0) {
      fatal_error("prctl(PR_GET_FP_MODE) fails.");
   }

   printf("fr_prctl: %d, fr_detected: %d\n", *fr_prctl, *fr_detected);
#if (__mips_isa_rev >= 6)
   if ((*fr_prctl != *fr_detected) && ((*fr_prctl != 3) || (*fr_detected != 0))) {
      fatal_error("fr_prctl != fr_detected");
   }
#else
   if (*fr_prctl != *fr_detected) {
      fatal_error("fr_prctl != fr_detected");
   }
#endif
}

int main() {
   int fr_prctl, fr_detected;

   test(&fr_prctl, &fr_detected);

   /* FP64 */
   if (fr_prctl == 1) {

#if (__mips_isa_rev >= 6)

      unsigned int w;
      unsigned long long l;

      /* Change mode to FRE */
      if (prctl(PR_SET_FP_MODE, 3) != 0) {
         fatal_error("prctl(PR_SET_FP_MODE, 3) fails.");
      }

      printf("Write to odd, read from even... ");

      w = 0x12345678;
      l = 0xAAAAAAAABBBBBBBB;

      __asm__ volatile (
         "ldc1 $f0, 0(%1)  \n\t"
         "mtc1 %0, $f1     \n\t"
         "sdc1 $f0, 0(%1)  \n\t"
         : : "r" (w), "r" (&l) : "memory", "$f0", "$f1"
      );

      if (l == 0x12345678BBBBBBBBull) printf("OK\n");
         else printf ("Error: l = %llX\n", l);

      printf("Write to even, read from odd... ");

      w = 0xAAAAAAAA;
      l = 0x12345678AAAAAAAA;

      __asm__ volatile (
         "ldc1 $f0, 0(%1)  \n\t"
         "mfc1 %0, $f1     \n\t"
         : "+r" (w) : "r" (&l) : "$f0", "$f1"
      );

      if (w == 0x12345678ul) printf("OK\n");
         else printf ("Error: w = %X\n", w);

      printf("Write to low part, check high part... ");

      w = 0xBBBBBBBB;
      l = 0x12345678AAAAAAAA;

      __asm__ volatile (
         "ldc1 $f0, 0(%1)  \n\t"
         "mtc1 %0, $f0     \n\t"
         "sdc1 $f0, 0(%1)  \n\t"
         : : "r" (w), "r" (&l) : "memory", "$f0"
      );

      if (l == 0x12345678BBBBBBBBull) printf("OK\n");
         else printf ("Error: l = %llX\n", l);

#else
      /* Change mode to FP32 */
      if (prctl(PR_SET_FP_MODE, 0) != 0) {
         fatal_error("prctl(PR_SET_FP_MODE, 0) fails.");
      }
#endif
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
