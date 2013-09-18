#include <stdio.h>
#include "rounding_mode.h"
#include "macro_fpu.h"

int directedRoundingMode(flt_dir_op_t op) {
   int fd_w = 0;
   long long int fd_l = 0;
   int i;
   int fcsr = 0;
   for (i = 0; i < MAX_ARR; i++) {
      switch(op) {
         case CEILWS:
              UNOPfw("ceil.w.s");
              printf("%s %d %f\n", flt_dir_op_names[op], fd_w, fs_f[i]);
              printf("fcsr: 0x%x\n", fcsr);
              break;
         case CEILWD:
              UNOPdw("ceil.w.d");
              printf("%s %d %lf\n", flt_dir_op_names[op], fd_w, fs_d[i]);
              printf("fcsr: 0x%x\n", fcsr);
              break;
         case FLOORWS:
              UNOPfw("floor.w.s");
              printf("%s %d %f\n", flt_dir_op_names[op], fd_w, fs_f[i]);
              printf("fcsr: 0x%x\n", fcsr);
              break;
         case FLOORWD:
              UNOPdw("floor.w.d");
              printf("%s %d %lf\n", flt_dir_op_names[op], fd_w, fs_d[i]);
              printf("fcsr: 0x%x\n", fcsr);
              break;
         case ROUNDWS:
              UNOPfw("round.w.s");
              printf("%s %d %f\n", flt_dir_op_names[op], fd_w, fs_f[i]);
              printf("fcsr: 0x%x\n", fcsr);
              break;
         case ROUNDWD:
              UNOPdw("round.w.d");
              printf("%s %d %lf\n", flt_dir_op_names[op], fd_w, fs_d[i]);
              printf("fcsr: 0x%x\n", fcsr);
              break;
         case TRUNCWS:
              UNOPfw("trunc.w.s");
              printf("%s %d %f\n", flt_dir_op_names[op], fd_w, fs_f[i]);
              printf("fcsr: 0x%x\n", fcsr);
              break;
         case TRUNCWD:
              UNOPdw("trunc.w.d");
              printf("%s %d %lf\n", flt_dir_op_names[op], fd_w, fs_d[i]);
              printf("fcsr: 0x%x\n", fcsr);
              break;
         case CEILLS:
              UNOPsl("ceil.l.s");
              printf("%s %lld %f\n", flt_dir_op_names[op], fd_l, fs_f[i]);
              printf("fcsr: 0x%x\n", fcsr);
              break;
         case CEILLD:
              UNOPdl("ceil.l.d");
              printf("%s %lld %lf\n", flt_dir_op_names[op], fd_l, fs_d[i]);
              printf("fcsr: 0x%x\n", fcsr);
              break;
         case FLOORLS:
              UNOPsl("floor.l.s");
              printf("%s %lld %f\n", flt_dir_op_names[op], fd_l, fs_f[i]);
              printf("fcsr: 0x%x\n", fcsr);
              break;
         case FLOORLD:
              UNOPdl("floor.l.d");
              printf("%s %lld %lf\n", flt_dir_op_names[op], fd_l, fs_d[i]);
              printf("fcsr: 0x%x\n", fcsr);
              break;
         case ROUNDLS:
              UNOPsl("round.l.s");
              printf("%s %lld %f\n", flt_dir_op_names[op], fd_l, fs_f[i]);
              printf("fcsr: 0x%x\n", fcsr);
              break;
         case ROUNDLD:
              UNOPdl("round.l.d");
              printf("%s %lld %lf\n", flt_dir_op_names[op], fd_l, fs_d[i]);
              printf("fcsr: 0x%x\n", fcsr);
              break;
         case TRUNCLS:
              UNOPsl("trunc.l.s");
              printf("%s %lld %f\n", flt_dir_op_names[op], fd_l, fs_f[i]);
              printf("fcsr: 0x%x\n", fcsr);
              break;
         case TRUNCLD:
              UNOPdl("trunc.l.d");
              printf("%s %lld %lf\n", flt_dir_op_names[op], fd_l, fs_d[i]);
              printf("fcsr: 0x%x\n", fcsr);
              break;
        default:
            printf("error\n");
            break;
        }
    }
   return 0;
}

int FCSRRoundingMode(flt_round_op_t op1)
{
   double fd_d = 0;
   float fd_f = 0;
   int fd_w = 0;
   long long int fd_l = 0;
   int i;
   int fcsr = 0;
   round_mode_t rm;
   for (rm = TO_NEAREST; rm <= TO_MINUS_INFINITY; rm ++) {
      set_rounding_mode(rm);
      printf("roundig mode: %s\n", round_mode_name[rm]);
      for (i = 0; i < MAX_ARR; i++) {
         set_rounding_mode(rm);
         switch(op1) {
            case CVTDS:
                 UNOPfd("cvt.d.s");
                 printf("%s %lf %lf\n", flt_round_op_names[op1], fd_d, fs_f[i]);
                 printf("fcsr: 0x%x\n", fcsr);
                 break;
            case CVTDW:
                 UNOPwd("cvt.d.w");
                 printf("%s %lf %d\n", flt_round_op_names[op1], fd_d, fs_w[i]);
                 printf("fcsr: 0x%x\n", fcsr);
                 break;
            case CVTSD:
                 UNOPdf("cvt.s.d");
                 printf("%s %f %lf\n", flt_round_op_names[op1], fd_f, fs_d[i]);
                 printf("fcsr: 0x%x\n", fcsr);
                 break;
            case CVTSW:
                 UNOPwf("cvt.s.w");
                 printf("%s %f %d\n", flt_round_op_names[op1], fd_f, fs_w[i]);
                 printf("fcsr: 0x%x\n", fcsr);
                 break;
            case CVTWS:
                 UNOPfw("cvt.w.s");
                 printf("%s %d %f\n", flt_round_op_names[op1], fd_w, fs_f[i]);
                 printf("fcsr: 0x%x\n", fcsr);
                 break;
            case CVTWD:
                 UNOPdw("cvt.w.d");
                 printf("%s %d %lf\n", flt_round_op_names[op1], fd_w, fs_d[i]);
                 printf("fcsr: 0x%x\n", fcsr);
                 break;
            case CVTDL:
                 UNOPld("cvt.d.l");
                 printf("%s %lf %ld\n", flt_round_op_names[op1], fd_d, fs_l[i]);
                 printf("fcsr: 0x%x\n", fcsr);
                 break;
            case CVTLS:
                 UNOPsl("cvt.l.s");
                 printf("%s %lld %f\n", flt_round_op_names[op1], fd_l, fs_f[i]);
                 printf("fcsr: 0x%x\n", fcsr);
                 break;
            case CVTLD:
                 UNOPdl("cvt.l.d");
                 printf("%s %lld %lf\n", flt_round_op_names[op1], fd_l, fs_d[i]);
                 printf("fcsr: 0x%x\n", fcsr);
                 break;
            case CVTSL:
                 UNOPls("cvt.s.l");
                 printf("%s %f %ld\n", flt_round_op_names[op1], fd_f, fs_l[i]);
                 printf("fcsr: 0x%x\n", fcsr);
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
   flt_dir_op_t op;
   flt_round_op_t op1;

   printf("-------------------------- %s --------------------------\n",
        "test FPU Conversion Operations Using a Directed Rounding Mode");
   for (op = CEILWS; op <= TRUNCLS; op++) {
      directedRoundingMode(op);
   }

   printf("-------------------------- %s --------------------------\n",
        "test FPU Conversion Operations Using the FCSR Rounding Mode");
   for (op1 = CVTDS; op1 <= CVTSL; op1++) {
      FCSRRoundingMode(op1);
   }
   return 0;
}
