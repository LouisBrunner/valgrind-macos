#include <stdio.h>
#include <math.h>
#include "rounding_mode.h"
#include "macro_fpu.h"

int arithmeticOperations(flt_art_op_t op) 
{
   double fd_d = 0;
   float fd_f = 0;
   int i = 0;
   int fcsr = 0;
   round_mode_t rm;
   for (rm = TO_NEAREST; rm <= TO_MINUS_INFINITY; rm ++) {
      set_rounding_mode(rm);
      printf("roundig mode: %s\n", round_mode_name[rm]);
      for (i = 0; i < MAX_ARR; i++) {
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
               printf("%s %f %f %f\n",
                      flt_art_op_names[op], fd_f, fs_f[i], ft_f[i]);
               break;
            case ADDD:
               BINOPd("add.d");
               printf("%s %lf %lf %lf\n",
                      flt_art_op_names[op], fd_d, fs_d[i], ft_d[i]);
               break;
            case DIVS:
               BINOPf("div.s");
               printf("%s %f %f %f\n",
                      flt_art_op_names[op], roundf(fd_f), fs_f[i], ft_f[i]);
               break;
            case DIVD:
               BINOPd("div.d");
               printf("%s %lf %lf %lf\n",
                      flt_art_op_names[op], round(fd_d), fs_d[i], ft_d[i]);
               break;
            case MULS:
               BINOPf("mul.s");
               printf("%s %f %f %f\n",
                      flt_art_op_names[op], roundf(fd_f), fs_f[i], ft_f[i]);
               break;
            case MULD:
               BINOPd("mul.d");
               printf("%s %lf %lf %lf\n",
                      flt_art_op_names[op], round(fd_d), fs_d[i], ft_d[i]);
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
               printf("%s %f %f\n",
                      flt_art_op_names[op], roundf(fd_f), fs_f[i]);
               break;
            case SQRTD:
               UNOPdd("sqrt.d");
               printf("%s %lf %lf\n",
                      flt_art_op_names[op], round(fd_d), fs_d[i]);
               break;
            case SUBS:
               BINOPf("sub.s");
               printf("%s %f %f %f\n",
                      flt_art_op_names[op], fd_f, fs_f[i], ft_f[i]);
               break;
            case SUBD:
               BINOPd("sub.d");
               printf("%s %lf %lf %lf\n",
                      flt_art_op_names[op], fd_d, fs_d[i], ft_d[i]);
               break;
            case RECIPS:
               UNOPff("recip.s");
               printf("%s %f %f\n",
                      flt_art_op_names[op], roundf(fd_f), fs_f[i]);
               break;
            case RECIPD:
               UNOPdd("recip.d");
               printf("%s %lf %lf\n",
                      flt_art_op_names[op], round(fd_d), fs_d[i]);
               break;
            case RSQRTS:
               if (fs_f[i] >= 0) {
                  UNOPff("rsqrt.s");
                  printf("%s %f %f\n",
                         flt_art_op_names[op], roundf(fd_f), fs_f[i]);
               }
               break;
            case RSQRTD:
               if (fs_d[i] >= 0) {
                  UNOPdd("rsqrt.d");
                  printf("%s %lf %lf\n",
                         flt_art_op_names[op], round(fd_d), fs_d[i]);
               }
               break;
            case MSUBS:
               TRIOPf("msub.s");
               printf("%s %f %f %f %f\n",flt_art_op_names[op], roundf(fd_f),
                                         fr_f[i], fs_f[i], ft_f[i]);
               break;
            case MSUBD:
               TRIOPd("msub.d");
               printf("%s %lf %lf %lf %lf\n", flt_art_op_names[op], round(fd_d),
                                              fr_d[i], fs_d[i], ft_d[i]);
               break;
            case MADDS:
               TRIOPf("madd.s");
               printf("%s %f %f %f %f\n", flt_art_op_names[op], roundf(fd_f),
                                          fr_f[i], fs_f[i], ft_f[i]);
               break;
            case MADDD:
               TRIOPd("madd.d");
               printf("%s %lf %lf %lf %lf\n", flt_art_op_names[op], round(fd_d),
                                              fr_d[i], fs_d[i], ft_d[i]);
               break;
            case NMADDS:
               TRIOPf("nmadd.s");
               printf("%s %f %f %f %f\n", flt_art_op_names[op], roundf(fd_f),
                                          fr_f[i], fs_f[i], ft_f[i]);
               break;
            case NMADDD:
               TRIOPd("nmadd.d");
               printf("%s %lf %lf %lf %lf\n", flt_art_op_names[op], round(fd_d),
                                              fr_d[i], fs_d[i], ft_d[i]);
               break;
            case NMSUBS:
               TRIOPf("nmsub.s");
               printf("%s %f %f %f %f\n", flt_art_op_names[op], roundf(fd_f),
                                          fr_f[i], fs_f[i], ft_f[i]);
               break;
            case NMSUBD:
               TRIOPd("nmsub.d");
               printf("%s 0x%lf %lf %lf %lf\n", flt_art_op_names[op],
                                                round(fd_d), fr_d[i], fs_d[i],
                                                ft_d[i]);
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
   for (op = ABSS; op <= NMSUBD; op++) {
      arithmeticOperations(op);
   }

   return 0;
}
