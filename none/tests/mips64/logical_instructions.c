#include <stdio.h>
#include "const.h"
#include "macro_int.h"

typedef enum {
   AND=0,  ANDI,   LUI,    NOR,
   OR,     ORI,    XOR,    XORI
} logical_op;

int main()
{
   logical_op op;
   int i;
   init_reg_val2();
   for (op = AND; op <= XORI; op++) {
      for (i = 0; i < N; i++) {
         switch (op) {
            case AND:
               /* No Integer Overflow exception occurs under any
                  circumstances. */
               TEST1("and $t0, $t1, $t2", reg_val1[i], reg_val1[N-i-1],
                                          t0, t1, t2);
               TEST1("and $s0, $s1, $s2", reg_val2[i], reg_val2[N-i-1],
                                          s0, s1, s2);
               break;

            case ANDI:
               /* No Integer Overflow exception occurs under any
                  circumstances. */
               TEST2("andi $t0, $t1, 0xff",   reg_val1[i], 0xff,   t0, t1);
               TEST2("andi $t2, $t3, 0xffff", reg_val1[i], 0xffff, t2, t3);
               TEST2("andi $a0, $a1, 0x0",    reg_val1[i], 0x0,    a0, a1);
               TEST2("andi $s0, $s1, 0x23",   reg_val1[i], 0x23,   s0, s1);
               TEST2("andi $t0, $t1, 0xff",   reg_val2[i], 0xff,   t0, t1);
               TEST2("andi $t2, $t3, 0xffff", reg_val2[i], 0xffff, t2, t3);
               TEST2("andi $a0, $a1, 0x0",    reg_val2[i], 0x0,    a0, a1);
               TEST2("andi $s0, $s1, 0x23",   reg_val2[i], 0x23,   s0, s1);
               break;

            case LUI:
               /* No Integer Overflow exception occurs under any
                  circumstances. */
               if (i == 0) {
                  TEST6("lui $t0, 0xffff", 0xffff, t0);
                  TEST6("lui $a0, 0x0",    0x0,    a0);
                  TEST6("lui $t9, 0xff",   0xff,   t9);
                  TEST6("lui $v0, 0xfff",  0xfff,  v0);
                  TEST6("lui $s0, 0x2",    0x2,    s0);
               }
               break;

            case NOR:
               /* No arithmetic exception occurs under any circumstances. */
               TEST1("nor $t0, $t1, $t2", reg_val1[i], reg_val1[N-i-1],
                                          t0, t1, t2);
               TEST1("nor $s0, $s1, $s2", reg_val2[i], reg_val2[N-i-1],
                                          s0, s1, s2);
               break;

            case OR:
               /* No arithmetic exception occurs under any circumstances. */
               TEST1("or $t0, $t1, $t2", reg_val1[i], reg_val1[N-i-1],
                                         t0, t1, t2);
               TEST1("or $s0, $s1, $s2", reg_val2[i], reg_val2[N-i-1],
                                         s0, s1, s2);
               break;

            case ORI:
               /* No arithmetic exception occurs under any circumstances. */
               TEST2("ori $t0, $t1, 0xff",   reg_val1[i], 0xff,   t0, t1);
               TEST2("ori $t2, $t3, 0xffff", reg_val1[i], 0xffff, t2, t3);
               TEST2("ori $a0, $a1, 0x0",    reg_val1[i], 0x0,    a0, a1);
               TEST2("ori $s0, $s1, 0x23",   reg_val1[i], 0x23,   s0, s1);
               TEST2("ori $t0, $t1, 0xff",   reg_val2[i], 0xff,   t0, t1);
               TEST2("ori $t2, $t3, 0xffff", reg_val2[i], 0xffff, t2, t3);
               TEST2("ori $a0, $a1, 0x0",    reg_val2[i], 0x0,    a0, a1);
               TEST2("ori $s0, $s1, 0x23",   reg_val2[i], 0x23,   s0, s1);
               break;

            case XOR:
               /* No arithmetic exception occurs under any circumstances. */
               TEST1("xor $t0, $t1, $t2", reg_val1[i], reg_val1[N-i-1],
                                          t0, t1, t2);
               TEST1("xor $s0, $s1, $s2", reg_val2[i], reg_val2[N-i-1],
                                          s0, s1, s2);
               break;

            case XORI:
               /* No arithmetic exception occurs under any circumstances. */
               TEST2("xori $t0, $t1, 0xff",   reg_val1[i], 0xff,   t0, t1);
               TEST2("xori $t2, $t3, 0xffff", reg_val1[i], 0xffff, t2, t3);
               TEST2("xori $a0, $a1, 0x0",    reg_val1[i], 0x0,    a0, a1);
               TEST2("xori $s0, $s1, 0x23",   reg_val1[i], 0x23,   s0, s1);
               TEST2("xori $t0, $t1, 0xff",   reg_val2[i], 0xff,   t0, t1);
               TEST2("xori $t2, $t3, 0xffff", reg_val2[i], 0xffff, t2, t3);
               TEST2("xori $a0, $a1, 0x0",    reg_val2[i], 0x0,    a0, a1);
               TEST2("xori $s0, $s1, 0x23",   reg_val2[i], 0x23,   s0, s1);
               break;
         }
      }
   }
   return 0;
}
