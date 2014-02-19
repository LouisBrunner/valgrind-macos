#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <setjmp.h>

#define MAX_ARR 24
#define PERROR \
        printf("This test is testing mips32r2 instructions in fpu64 mode.\n");
#define FLAGS_RM_MASK 0xFFFFFFFF

typedef enum {
   CVTLS,   CVTLD,   ROUNDLS, ROUNDLD,
   TRUNCLS, TRUNCLD, FLOORLS, FLOORLD,
   CEILLS,  CEILLD
} flt_round_op_t;

const char *flt_round_op_names[] = {
   "cvt.l.s",   "cvt.l.d",   "round.l.s", "round.l.d",
   "trunc.l.s", "trunc.l.d", "floor.l.s", "floor.l.d"
   "ceil.l.s",  "ceil.l.d"
};

typedef enum {
   TO_NEAREST=0, TO_ZERO, TO_PLUS_INFINITY, TO_MINUS_INFINITY } round_mode_t;
char *round_mode_name[] = { "near", "zero", "+inf", "-inf" };

const float fs_f[] = {
   0,         456.25,   3,          -1,
   1384.5,    -7.25,    1000000000, -5786.25,
   1752,      0.015625, 0.03125,    -248562.75,
   -45786.5,  456,      34.03125,   45786.75,
   1752065,   107,      -45667.25,  -7,
   -347856.5, 356047,   -1.25,      23.0625
};

const double fs_d[] = {
   0,         456.25,   3,          -1,
   1384.5,    -7.25,    1000000000, -5786.25,
   1752,      0.015625, 0.03125,    -24856226678933.75,
   -45786.5,  456,      34.03125,   45786.75,
   1752065,   107,      -45667.25,  -7,
   -347856.5, 356047,   -1.25,      23.0625
};

#define UNOPsl(op)                \
   __asm__ __volatile__(          \
      op"   $f0, %2"     "\n\t"   \
      "sdc1 $f0, 0(%1)"  "\n\t"   \
      "cfc1 %0,  $31"    "\n\t"   \
      : "=r" (fcsr)               \
      : "r"(&fd_l), "f"(fs_f[i])  \
      : "$f0"                     \
   );

#define UNOPdl(op)                \
   __asm__ __volatile__(          \
      op"   $f0, %2"     "\n\t"   \
      "sdc1 $f0, 0(%1)"  "\n\t"   \
      "cfc1 %0,  $31"    "\n\t"   \
      : "=r" (fcsr)               \
      : "r"(&fd_l), "f"(fs_d[i])  \
      : "$f0"                     \
   );

#define TEST_FPU64                \
   __asm__ __volatile__(          \
      "cvt.l.s $f0, $f0"  "\n\t"  \
      :                           \
      :                           \
      : "$f0"                     \
   );

#if (__mips==32) && (__mips_isa_rev>=2) && (__mips_fpr==64)
void set_rounding_mode(round_mode_t mode)
{
   switch(mode) {
      case TO_NEAREST:
         __asm__ volatile("ctc1 $zero, $31"  "\n\t");
         break;
      case TO_ZERO:
         __asm__ volatile("li    $t0, 0x1"  "\n\t"
                          "ctc1  $t0, $31"  "\n\t");
         break;
      case TO_PLUS_INFINITY:
          __asm__ volatile("li    $t0, 0x2"  "\n\t"
                           "ctc1  $t0, $31"  "\n\t");
         break;
      case TO_MINUS_INFINITY:
          __asm__ volatile("li    $t0, 0x3"  "\n\t"
                           "ctc1  $t0, $31"  "\n\t");
         break;
   }
}

struct test {
   void (*test)(void);
   int sig;
   int code;
};

static void handler(int sig)
{
   PERROR;
   exit(0);
}

int FCSRRoundingMode(flt_round_op_t op)
{
   long long int fd_l;
   int i;
   int fcsr = 0;
   round_mode_t rm;
   for (rm = TO_NEAREST; rm <= TO_MINUS_INFINITY; rm ++) {
      printf("roundig mode: %s\n", round_mode_name[rm]);
      for (i = 0; i < MAX_ARR; i++) {
         set_rounding_mode(rm);
         switch(op) {
            case CVTLS:
               UNOPsl("cvt.l.s");
               printf("%s %lld %f\n",
                      flt_round_op_names[op], fd_l, fs_f[i]);
               printf("fcsr: 0x%x\n", fcsr & FLAGS_RM_MASK);
               break;
            case CVTLD:
               UNOPdl("cvt.l.d");
               printf("%s %lld %lf\n",
                      flt_round_op_names[op], fd_l, fs_d[i]);
               printf("fcsr: 0x%x\n", fcsr & FLAGS_RM_MASK);
               break;
            case ROUNDLS:
               UNOPsl("round.l.s");
               printf("%s %lld %f\n",
                      flt_round_op_names[op], fd_l, fs_f[i]);
               printf("fcsr: 0x%x\n", fcsr & FLAGS_RM_MASK);
               break;
            case ROUNDLD:
               UNOPdl("round.l.d");
               printf("%s %lld %lf\n",
                      flt_round_op_names[op], fd_l, fs_d[i]);
               printf("fcsr: 0x%x\n", fcsr & FLAGS_RM_MASK);
               break;
            case TRUNCLS:
               UNOPsl("trunc.l.s");
               printf("%s %lld %f\n",
                      flt_round_op_names[op], fd_l, fs_f[i]);
               printf("fcsr: 0x%x\n", fcsr & FLAGS_RM_MASK);
               break;
            case TRUNCLD:
               UNOPdl("trunc.l.d");
               printf("%s %lld %lf\n",
                      flt_round_op_names[op], fd_l, fs_d[i]);
               printf("fcsr: 0x%x\n", fcsr & FLAGS_RM_MASK);
               break;
            case FLOORLS:
               UNOPsl("floor.l.s");
               printf("%s %lld %f\n",
                      flt_round_op_names[op], fd_l, fs_f[i]);
               printf("fcsr: 0x%x\n", fcsr & FLAGS_RM_MASK);
               break;
            case FLOORLD:
               UNOPdl("floor.l.d");
               printf("%s %lld %lf\n",
                      flt_round_op_names[op], fd_l, fs_d[i]);
               printf("fcsr: 0x%x\n", fcsr & FLAGS_RM_MASK);
               break;
            case CEILLS:
               UNOPsl("ceil.l.s");
               printf("%s %lld %f\n",
                      flt_round_op_names[op], fd_l, fs_f[i]);
               printf("fcsr: 0x%x\n", fcsr & FLAGS_RM_MASK);
               break;
            case CEILLD:
               UNOPdl("ceil.l.d");
               printf("%s %lld %lf\n",
                      flt_round_op_names[op], fd_l, fs_d[i]);
               printf("fcsr: 0x%x\n", fcsr & FLAGS_RM_MASK);
               break;
            default:
               printf("error\n");
               break;
         }
      }
   }
   return 0;
}
#endif


int main()
{
#if (__mips==32) && (__mips_isa_rev>=2) && (__mips_fpr==64)
   flt_round_op_t op;
   signal(SIGILL, handler);
   /* Test fpu64 mode. */
   TEST_FPU64;
   printf("-------------------------- %s --------------------------\n",
          "test FPU Conversion Operations Using the FCSR Rounding Mode");
   for (op = CVTLS; op <= CEILLD; op++)
      FCSRRoundingMode(op);
#else
   PERROR;
#endif
   return 0;
}
