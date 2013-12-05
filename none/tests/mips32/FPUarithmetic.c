#include <stdio.h>
#include <stdlib.h>

typedef enum {
   ABSS=0, ABSD,
   ADDS, ADDD,
   DIVS, DIVD,
   MULS, MULD,
   NEGS, NEGD,
   SQRTS, SQRTD,
   SUBS, SUBD,
   RECIPS, RECIPD,
   RSQRTS, RSQRTD
} flt_art_op_t;

const char *flt_art_op_names[] = {
   "abs.s", "abs.d",
   "add.s", "add.d",
   "div.s", "div.d",
   "mul.s", "mul.d",
   "neg.s", "neg.d",
   "sqrt.s", "sqrt.d",
   "sub.s", "sub.d",
   "recip.s", "recip.d",
   "rsqrt.s", "rsqrt.d"
};

typedef enum {
   TO_NEAREST=0, TO_ZERO, TO_PLUS_INFINITY, TO_MINUS_INFINITY } round_mode_t;
char *round_mode_name[] = { "near", "zero", "+inf", "-inf" };

const double fs_d[] = {
   0,         456.25,   3,          -1,
   1384.5,    -7.25,    1000000000, -5786.5,
   1752,      0.015625, 0.03125,    -248562.75,
   456,       -45786.5, 34.03125,   45786.75,
   1752065,   107,      -45667.25,  -7,
   -347856.5, 356047.5, -1.0,       23.0625
};

const double ft_d[] = {
   -456.25,    -45786.5, 34.03125,   45786.75,
   1752065,   107,      -45667.25,  -7.25,
   -347856.5, 356047.5, -1.0,       23.0625,
   0,         456.25,   3,          -1,
   1384.5,    -7,       1000000000, -5786.5,
   1752,      0.015625, 0.03125,    -248562.75
};

const float fs_f[] = {
   0,         456.25,   3,          -1,
   1384.5,    -7.25,    1000000000, -5786.5,
   1752,      0.015625, 0.03125,    -248562.75,
   456,       -45786.5, 34.03125,   45786.75,
   1752065,   107,      -45667.25,  -7,
   -347856.5, 356047.5, -1.0,       23.0625
};

const float ft_f[] = {
   -456.25,  -4578.5,   34.03125, 4578.75,
   175,     107,      -456.25,  -7.25,
   -3478.5, 356.5,    -1.0,     23.0625,
   0,       456.25,   3,        -1,
   1384.5,  -7,       100,      -5786.5,
   1752,    0.015625, 0.03125,  -248562.75
};

#define UNOPdd(op) \
        fd_d = 0;  \
        __asm__ volatile( \
					op" %0, %1\n\t" \
					: "=f"(fd_d) : "f"(fs_d[i]));

#define UNOPff(op) \
        fd_f = 0;  \
        __asm__ volatile( \
					op" %0, %1\n\t" \
					: "=f"(fd_f) : "f"(fs_f[i]));

#define BINOPf(op) \
        fd_f = 0;  \
        __asm__ volatile( \
					op" %0, %1, %2\n\t" \
					: "=f"(fd_f) : "f"(fs_f[i]) , "f"(ft_f[i]));

#define BINOPd(op) \
        fd_d = 0;  \
        __asm__ volatile( \
					op" %0, %1, %2\n\t" \
					: "=f"(fd_d) : "f"(fs_d[i]) , "f"(ft_d[i]));

void set_rounding_mode(round_mode_t mode)
{
   switch(mode) {
      case TO_NEAREST:
         __asm__ volatile("cfc1 $t0, $31\n\t"
                          "srl $t0, 2\n\t"
                          "sll $t0, 2\n\t"
                          "ctc1 $t0, $31\n\t");
         break;
      case TO_ZERO:
         __asm__ volatile("cfc1 $t0, $31\n\t"
                          "srl $t0, 2\n\t"
                          "sll $t0, 2\n\t"
                          "addiu $t0, 1\n\t"
                          "ctc1 $t0, $31\n\t");
         break;
      case TO_PLUS_INFINITY:
         __asm__ volatile("cfc1 $t0, $31\n\t"
                          "srl $t0, 2\n\t"
                          "sll $t0, 2\n\t"
                          "addiu $t0, 2\n\t"
                          "ctc1 $t0, $31\n\t");
         break;
      case TO_MINUS_INFINITY:
         __asm__ volatile("cfc1 $t0, $31\n\t"
                          "srl $t0, 2\n\t"
                          "sll $t0, 2\n\t"
                          "addiu $t0, 3\n\t"
                          "ctc1 $t0, $31\n\t");
         break;
   }
}

int arithmeticOperations(flt_art_op_t op)
{
   double fd_d = 0;
   float fd_f = 0;
   int i = 0;
   round_mode_t rm;
   for (rm = TO_NEAREST; rm <= TO_MINUS_INFINITY; rm ++) {
      set_rounding_mode(rm);
      printf("rounding mode: %s\n", round_mode_name[rm]);
      for (i = 0; i < 24; i++)
      {
         switch(op) {
            case ABSS:
                 UNOPff("abs.s");
                 printf("%s %f %f\n", flt_art_op_names[op], fd_f, fs_f[i]);
                 break;
            case ABSD:
                 UNOPdd("abs.d");
                 printf("%s %lf %lf\n", flt_art_op_names[op], fd_d, fs_d[i]);
                 break;
            case ADDS:
                 BINOPf("add.s");
                 printf("%s %f %f %f\n", flt_art_op_names[op], fd_f, fs_f[i], ft_f[i]);
                 break;
            case ADDD:
                 BINOPd("add.d");
                 printf("%s %lf %lf %lf\n", flt_art_op_names[op], fd_d, fs_d[i], ft_d[i]);
                 break;
            case DIVS:
                 BINOPf("div.s");
                 printf("%s %f %f %f\n", flt_art_op_names[op], fd_f, fs_f[i], ft_f[i]);
                 break;
            case DIVD:
                 BINOPd("div.d");
                 printf("%s %lf %lf %lf\n", flt_art_op_names[op], fd_d, fs_d[i], ft_d[i]);
                 break;
            case MULS:
                 BINOPf("mul.s");
                 printf("%s %f %f %f\n", flt_art_op_names[op], fd_f, fs_f[i], ft_f[i]);
                 break;
            case MULD:
                 BINOPd("mul.d");
                 printf("%s %lf %lf %lf\n", flt_art_op_names[op], fd_d, fs_d[i], ft_d[i]);
                 break;
            case NEGS:
                 UNOPff("neg.s");
                 printf("%s %f %f\n", flt_art_op_names[op], fd_f, fs_f[i]);
                 break;
            case NEGD:
                 UNOPdd("neg.d");
                 printf("%s %lf %lf\n", flt_art_op_names[op], fd_d, fs_d[i]);
                 break;
            case SQRTS:
                 UNOPff("sqrt.s");
                 printf("%s %f %f\n", flt_art_op_names[op], fd_f, fs_f[i]);
                 break;
            case SQRTD:
                 UNOPdd("sqrt.d");
                 printf("%s %lf %lf\n", flt_art_op_names[op], fd_d, fs_d[i]);
                 break;
            case SUBS:
                 BINOPf("sub.s");
                 printf("%s %f %f %f\n", flt_art_op_names[op], fd_f, fs_f[i], ft_f[i]);
                 break;
            case SUBD:
                 BINOPd("sub.d");
                 printf("%s %lf %lf %lf\n", flt_art_op_names[op], fd_d, fs_d[i], ft_d[i]);
                 break;
            case RECIPS:
#if (__mips==32) && (__mips_isa_rev>=2)
                 UNOPff("recip.s");
                 printf("%s %f %f\n", flt_art_op_names[op], fd_f, fs_f[i]);
#endif
                 break;
            case RECIPD:
#if (__mips==32) && (__mips_isa_rev>=2)
                 UNOPdd("recip.d");
                 printf("%s %lf %lf\n", flt_art_op_names[op], fd_d, fs_d[i]);
#endif
                 break;
            case RSQRTS:
#if (__mips==32) && (__mips_isa_rev>=2)
                 UNOPff("rsqrt.s");
                 printf("%s %f %f\n", flt_art_op_names[op], fd_f, fs_f[i]);
#endif
                 break;
            case RSQRTD:
#if (__mips==32) && (__mips_isa_rev>=2)
                 UNOPdd("rsqrt.d");
                 printf("%s %lf %lf\n", flt_art_op_names[op], fd_d, fs_d[i]);
#endif
                 break;
            default:
               printf("error\n");
               break;
         }
      }
   }
   return 0;
}

int main()
{
   flt_art_op_t op;

   printf("-------------------------- %s --------------------------\n",
        "test FPU Arithmetic Operations");
   for (op = ABSS; op <= RECIPD; op++) {
      arithmeticOperations(op);
   }

   return 0;
}

