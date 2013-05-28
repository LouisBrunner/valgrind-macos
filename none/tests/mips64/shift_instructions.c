#include <stdio.h>
#include "const.h"
#include "macro_int.h"

typedef enum {
    DROTR=0,  DROTR32,   DROTRV,    DSLL,
    DSLL32,   DSLLV,     DSRA,      DSRA32,
    DSRAV,    DSRL,      DSRL32,    DSRLV,
    ROTR,     ROTRV,     SLL,       SLLV,
    SRA,      SRAV,      SRL,       SRLV
} logical_op;

int main()
{
   logical_op op;
   int i;
   init_reg_val2();
   for (op = DROTR; op <= SRLV; op++) {
      for (i = 0; i < N; i++) {
         switch(op) {
            case DROTR:
               /* Release 2 Only */
#if (__mips == 64) && (__mips_isa_rev >= 2)
               TEST2("drotr $t0, $t1, 0x00", reg_val1[i], 0x00, t0, t1);
               TEST2("drotr $t2, $t3, 0x1f", reg_val1[i], 0x1f, t2, t3);
               TEST2("drotr $a0, $a1, 0x0f", reg_val1[i], 0x0f, a0, a1);
               TEST2("drotr $s0, $s1, 0x03", reg_val1[i], 0x03, s0, s1);
               TEST2("drotr $t0, $t1, 0x00", reg_val2[i], 0x00, t0, t1);
               TEST2("drotr $t2, $t3, 0x1f", reg_val2[i], 0x1f, t2, t3);
               TEST2("drotr $a0, $a1, 0x0f", reg_val2[i], 0x0f, a0, a1);
               TEST2("drotr $s0, $s1, 0x03", reg_val2[i], 0x03, s0, s1);
#endif
               break;
            case DROTR32:
               /* Release 2 Only */
#if (__mips == 64) && (__mips_isa_rev >= 2)
               TEST2("drotr32 $t0, $t1, 0x00", reg_val1[i], 0x00, t0, t1);
               TEST2("drotr32 $t2, $t3, 0x1f", reg_val1[i], 0x1f, t2, t3);
               TEST2("drotr32 $a0, $a1, 0x0f", reg_val1[i], 0x0f, a0, a1);
               TEST2("drotr32 $s0, $s1, 0x03", reg_val1[i], 0x03, s0, s1);
               TEST2("drotr32 $t0, $t1, 0x00", reg_val2[i], 0x00, t0, t1);
               TEST2("drotr32 $t2, $t3, 0x1f", reg_val2[i], 0x1f, t2, t3);
               TEST2("drotr32 $a0, $a1, 0x0f", reg_val2[i], 0x0f, a0, a1);
               TEST2("drotr32 $s0, $s1, 0x03", reg_val2[i], 0x03, s0, s1);
#endif
               break;
            case DROTRV:
               /* Release 2 Only */
#if (__mips == 64) && (__mips_isa_rev >= 2)
               TEST1("drotrv $t0, $t1, $t2", reg_val1[i], reg_val1[N-i-1],
                                             t0, t1, t2);
               TEST1("drotrv $s0, $s1, $s2", reg_val2[i], reg_val2[N-i-1],
                                             s0, s1, s2);
#endif
               break;
            case DSLL:
               TEST2("dsll $t0, $t1, 0x00", reg_val1[i], 0x00, t0, t1);
               TEST2("dsll $t2, $t3, 0x1f", reg_val1[i], 0x1f, t2, t3);
               TEST2("dsll $a0, $a1, 0x0f", reg_val1[i], 0x0f, a0, a1);
               TEST2("dsll $s0, $s1, 0x03", reg_val1[i], 0x03, s0, s1);
               TEST2("dsll $t0, $t1, 0x00", reg_val2[i], 0x00, t0, t1);
               TEST2("dsll $t2, $t3, 0x1f", reg_val2[i], 0x1f, t2, t3);
               TEST2("dsll $a0, $a1, 0x0f", reg_val2[i], 0x0f, a0, a1);
               TEST2("dsll $s0, $s1, 0x03", reg_val2[i], 0x03, s0, s1);
               break;

            case DSLL32:
               TEST2("dsll32 $t0, $t1, 0x00", reg_val1[i], 0x00, t0, t1);
               TEST2("dsll32 $t2, $t3, 0x1f", reg_val1[i], 0x1f, t2, t3);
               TEST2("dsll32 $a0, $a1, 0x0f", reg_val1[i], 0x0f, a0, a1);
               TEST2("dsll32 $s0, $s1, 0x03", reg_val1[i], 0x03, s0, s1);
               TEST2("dsll32 $t0, $t1, 0x00", reg_val2[i], 0x00, t0, t1);
               TEST2("dsll32 $t2, $t3, 0x1f", reg_val2[i], 0x1f, t2, t3);
               TEST2("dsll32 $a0, $a1, 0x0f", reg_val2[i], 0x0f, a0, a1);
               TEST2("dsll32 $s0, $s1, 0x03", reg_val2[i], 0x03, s0, s1);
               break;

            case DSLLV:
               TEST1("dsllv $t0, $t1, $t2", reg_val1[i], reg_val1[N-i-1],
                                            t0, t1, t2);
               TEST1("dsllv $s0, $s1, $s2", reg_val2[i], reg_val2[N-i-1],
                                            s0, s1, s2);
               break;

            case DSRA:
               TEST2("dsra $t0, $t1, 0x00", reg_val1[i], 0x00, t0, t1);
               TEST2("dsra $t2, $t3, 0x1f", reg_val1[i], 0x1f, t2, t3);
               TEST2("dsra $a0, $a1, 0x0f", reg_val1[i], 0x0f, a0, a1);
               TEST2("dsra $s0, $s1, 0x03", reg_val1[i], 0x03, s0, s1);
               TEST2("dsra $t0, $t1, 0x00", reg_val2[i], 0x00, t0, t1);
               TEST2("dsra $t2, $t3, 0x1f", reg_val2[i], 0x1f, t2, t3);
               TEST2("dsra $a0, $a1, 0x0f", reg_val2[i], 0x0f, a0, a1);
               TEST2("dsra $s0, $s1, 0x03", reg_val2[i], 0x03, s0, s1);
               break;

            case DSRA32:
               TEST2("dsra32 $t0, $t1, 0x00", reg_val1[i], 0x00, t0, t1);
               TEST2("dsra32 $t2, $t3, 0x1f", reg_val1[i], 0x1f, t2, t3);
               TEST2("dsra32 $a0, $a1, 0x0f", reg_val1[i], 0x0f, a0, a1);
               TEST2("dsra32 $s0, $s1, 0x03", reg_val1[i], 0x03, s0, s1);
               TEST2("dsra32 $t0, $t1, 0x00", reg_val2[i], 0x00, t0, t1);
               TEST2("dsra32 $t2, $t3, 0x1f", reg_val2[i], 0x1f, t2, t3);
               TEST2("dsra32 $a0, $a1, 0x0f", reg_val2[i], 0x0f, a0, a1);
               TEST2("dsra32 $s0, $s1, 0x03", reg_val2[i], 0x03, s0, s1);
               break;

            case DSRAV:
               TEST1("dsrav $t0, $t1, $t2", reg_val1[i], reg_val1[N-i-1],
                                            t0, t1, t2);
               TEST1("dsrav $s0, $s1, $s2", reg_val2[i], reg_val2[N-i-1],
                                            s0, s1, s2);
               break;

            case DSRL:
               TEST2("dsrl $t0, $t1, 0x00", reg_val1[i], 0x00, t0, t1);
               TEST2("dsrl $t2, $t3, 0x1f", reg_val1[i], 0x1f, t2, t3);
               TEST2("dsrl $a0, $a1, 0x0f", reg_val1[i], 0x0f, a0, a1);
               TEST2("dsrl $s0, $s1, 0x03", reg_val1[i], 0x03, s0, s1);
               TEST2("dsrl $t0, $t1, 0x00", reg_val2[i], 0x00, t0, t1);
               TEST2("dsrl $t2, $t3, 0x1f", reg_val2[i], 0x1f, t2, t3);
               TEST2("dsrl $a0, $a1, 0x0f", reg_val2[i], 0x0f, a0, a1);
               TEST2("dsrl $s0, $s1, 0x03", reg_val2[i], 0x03, s0, s1);
               break;

            case DSRL32:
               TEST2("dsrl32 $t0, $t1, 0x00", reg_val1[i], 0x00, t0, t1);
               TEST2("dsrl32 $t2, $t3, 0x1f", reg_val1[i], 0x1f, t2, t3);
               TEST2("dsrl32 $a0, $a1, 0x0f", reg_val1[i], 0x0f, a0, a1);
               TEST2("dsrl32 $s0, $s1, 0x03", reg_val1[i], 0x03, s0, s1);
               TEST2("dsrl32 $t0, $t1, 0x00", reg_val2[i], 0x00, t0, t1);
               TEST2("dsrl32 $t2, $t3, 0x1f", reg_val2[i], 0x1f, t2, t3);
               TEST2("dsrl32 $a0, $a1, 0x0f", reg_val2[i], 0x0f, a0, a1);
               TEST2("dsrl32 $s0, $s1, 0x03", reg_val2[i], 0x03, s0, s1);
               break;

            case DSRLV:
               TEST1("dsrlv $t0, $t1, $t2", reg_val1[i], reg_val1[N-i-1],
                                            t0, t1, t2);
               TEST1("dsrlv $s0, $s1, $s2", reg_val2[i], reg_val2[N-i-1],
                                            s0, s1, s2);
               break;

            case ROTR:
               /* Release 2 Only */
#if (__mips == 64) && (__mips_isa_rev >= 2)
               TEST2("rotr $t0, $t1, 0x00", reg_val1[i], 0x00, t0, t1);
               TEST2("rotr $t2, $t3, 0x1f", reg_val1[i], 0x1f, t2, t3);
               TEST2("rotr $a0, $a1, 0x0f", reg_val1[i], 0x0f, a0, a1);
               TEST2("rotr $s0, $s1, 0x03", reg_val1[i], 0x03, s0, s1);
#endif
               break;
            case ROTRV:
               /* Release 2 Only */
#if (__mips == 64) && (__mips_isa_rev >= 2)
               TEST1("rotrv $t0, $t1, $t2", reg_val1[i], reg_val1[N-i-1],
                                            t0, t1, t2);
#endif
               break;
            case SLL:
               TEST2("sll $t0, $t1, 0x00", reg_val1[i], 0x00, t0, t1);
               TEST2("sll $t2, $t3, 0x1f", reg_val1[i], 0x1f, t2, t3);
               TEST2("sll $a0, $a1, 0x0f", reg_val1[i], 0x0f, a0, a1);
               TEST2("sll $s0, $s1, 0x03", reg_val1[i], 0x03, s0, s1);
               TEST2("sll $t0, $t1, 0x00", reg_val2[i], 0x00, t0, t1);
               TEST2("sll $t2, $t3, 0x1f", reg_val2[i], 0x1f, t2, t3);
               TEST2("sll $a0, $a1, 0x0f", reg_val2[i], 0x0f, a0, a1);
               TEST2("sll $s0, $s1, 0x03", reg_val2[i], 0x03, s0, s1);
               break;

            case SLLV:
               TEST1("sllv $t0, $t1, $t2", reg_val1[i], reg_val1[N-i-1],
                                           t0, t1, t2);
               TEST1("sllv $s0, $s1, $s2", reg_val2[i], reg_val2[N-i-1],
                                           s0, s1, s2);
               break;

            case SRA:
               TEST2("sra $t0, $t1, 0x00", reg_val1[i], 0x00, t0, t1);
               TEST2("sra $t2, $t3, 0x1f", reg_val1[i], 0x1f, t2, t3);
               TEST2("sra $a0, $a1, 0x0f", reg_val1[i], 0x0f, a0, a1);
               TEST2("sra $s0, $s1, 0x03", reg_val1[i], 0x03, s0, s1);
               break;

            case SRAV:
               TEST1("srav $t0, $t1, $t2", reg_val1[i], reg_val1[N-i-1],
                                           t0, t1, t2);
               break;

            case SRL:
               TEST2("srl $t0, $t1, 0x00", reg_val1[i], 0x00, t0, t1);
               TEST2("srl $t2, $t3, 0x1f", reg_val1[i], 0x1f, t2, t3);
               TEST2("srl $a0, $a1, 0x0f", reg_val1[i], 0x0f, a0, a1);
               TEST2("srl $s0, $s1, 0x03", reg_val1[i], 0x03, s0, s1);
               break;

            case SRLV:
               TEST1("srlv $t0, $t1, $t2", reg_val1[i], reg_val1[N-i-1],
                                           t0, t1, t2);
               break;

         }
      }
   }
   return 0;
}
