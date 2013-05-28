#include <stdio.h>
#include "const.h"
#include "macro_int.h"

typedef enum {
   ADD=0,  ADDI,   ADDIU,  ADDU,
   CLO,    CLZ,    DADD,   DADDI,
   DADDIU, DADDU,  DCLO,   DCLZ,
   DDIV,   DDIVU,  DIV,    DIVU,
   DMULT,  DMULTU, DSUB,   DSUBU,
   MADD,   MADDU,  MSUB,   MSUBU,
   MUL,    MULT,   MULTU,  MOVN,
   MOVZ,   SEB,    SEH,    SLT,
   SLTI,   SLTIU,  SLTU,   SUB,
   SUBU
} arithmetic_op;

int main()
{
   arithmetic_op op;
   int i;
   init_reg_val2();

   for (op = ADD; op <= SUBU; op++) {
      for (i = 0; i < N; i++) {
         switch(op) {
            case ADD:
               /* If either GPR rt or GPR rs does not contain sign-extended
                  32-bit values (bits 63..31 equal), then the result of the
                  operation is UNPREDICTABLE. */
               TEST1("add $t0, $t1, $t2", reg_val1[i], reg_val1[N-i-1],
                                          t0, t1, t2);
               break;

            case ADDI:
               /* If GPR rs does not contain a sign-extended 32-bit
                  value (bits 63..31 equal), then the result of the operation
                  is UNPREDICTABLE. */
               TEST2("addi $t0, $t1, 0xff",   reg_val1[i], 0xff,   t0, t1);
               TEST2("addi $t2, $t3, 0xffff", reg_val1[i], 0xffff, t2, t3);
               TEST2("addi $a0, $a1, 0x0",    reg_val1[i], 0x0,    a0, a1);
               TEST2("addi $s0, $s1, 0x23",   reg_val1[i], 0x23,   s0, s1);
               break;

            case ADDIU:
               /* If GPR rs does not contain a sign-extended 32-bit
                  value (bits 63..31 equal), then the result of the operation
                  is UNPREDICTABLE. */
               TEST2("addiu $t0, $t1, 0xff",   reg_val1[i], 0xff,   t0, t1);
               TEST2("addiu $t2, $t3, 0xffff", reg_val1[i], 0xffff, t2, t3);
               TEST2("addiu $a0, $a1, 0x0",    reg_val1[i], 0x0,    a0, a1);
               TEST2("addiu $s0, $s1, 0x23",   reg_val1[i], 0x23,   s0, s1);
               break;

            case ADDU:
               /* If either GPR rt or GPR rs does not contain sign-extended
                  32-bit values (bits 63..31 equal), then the result of the
                  operation is UNPREDICTABLE. */
               TEST1("addu $t0, $t1, $t2", reg_val1[i], reg_val1[N-i-1],
                                           t0, t1, t2);
               break;

            case CLO:
               /* If GPR rs does not contain a sign-extended 32-bit
                  value (bits 63..31 equal), then the results of the operation
                  are UNPREDICTABLE. */
               TEST3("clo $t0, $t1", reg_val1[i], t0, t1);
               break;

            case CLZ:
               /* If GPR rs does not contain a sign-extended 32-bit
                  value (bits 63..31 equal), then the results of the operation
                  are UNPREDICTABLE. */
               TEST3("clz $t0, $t1", reg_val1[i], t0, t1);
               break;

            case DADD:
               /* If the addition results in 64-bit 2âs complement arithmetic
                  overflow, then the destination register is not modified and
                  an IntegerOverflow exception occurs. */
               TEST1("dadd $t0, $t1, $t2", reg_val1[i], reg_val1[N-i-1],
                                           t0, t1, t2);
               break;

            case DADDI:
               /* If the addition results in 64-bit 2âs complement arithmetic
                  overflow, then the destination register is not modified and
                  an Integer Overflow exception occurs. */
               TEST2("daddi $t0, $t1, 0xff",   reg_val1[i], 0xff,   t0, t1);
               TEST2("daddi $t2, $t3, 0xffff", reg_val1[i], 0xffff, t2, t3);
               TEST2("daddi $a0, $a1, 0x0",    reg_val1[i], 0x0,    a0, a1);
               TEST2("daddi $s0, $s1, 0x23",   reg_val1[i], 0x23,   s0, s1);
               TEST2("daddi $t0, $t1, 0xff",   reg_val2[i], 0xff,   t0, t1);
               TEST2("daddi $t2, $t3, 0xffff", reg_val2[i], 0xffff, t2, t3);
               TEST2("daddi $a0, $a1, 0x0",    reg_val2[i], 0x0,    a0, a1);
               TEST2("daddi $s0, $s1, 0x23",   reg_val2[i], 0x23,   s0, s1);
               break;

            case DADDIU:
               /* No Integer Overflow exception occurs under any
                  circumstances. */
               TEST2("daddiu $t0, $t1, 0xff",   reg_val1[i], 0xff,   t0, t1);
               TEST2("daddiu $t2, $t3, 0xffff", reg_val1[i], 0xffff, t2, t3);
               TEST2("daddiu $a0, $a1, 0x0",    reg_val1[i], 0x0,    a0, a1);
               TEST2("daddiu $s0, $s1, 0x23",   reg_val1[i], 0x23,   s0, s1);
               TEST2("daddiu $t0, $t1, 0xff",   reg_val2[i], 0xff,   t0, t1);
               TEST2("daddiu $t2, $t3, 0xffff", reg_val2[i], 0xffff, t2, t3);
               TEST2("daddiu $a0, $a1, 0x0",    reg_val2[i], 0x0,    a0, a1);
               TEST2("daddiu $s0, $s1, 0x23",   reg_val2[i], 0x23,   s0, s1);
               break;

            case DADDU:
               /* No Integer Overflow exception occurs under any
                  circumstances. */
               TEST1("daddu $t0, $t1, $t2", reg_val1[i], reg_val1[N-i-1],
                                            t0, t1, t2);
               TEST1("daddu $s0, $s1, $s2", reg_val2[i], reg_val2[N-i-1],
                                            s0, s1, s2);
               break;

            case DCLO:
               /* No arithmetic exception occurs under any circumstances. */
               TEST3("dclo $t0, $t1", reg_val1[i], t0, t1);
               TEST3("dclo $v0, $v1", reg_val2[i], v0, v1);
               break;

            case DCLZ:
               /* No arithmetic exception occurs under any circumstances. */
               TEST3("dclz $t0, $t1", reg_val1[i], t0, t1);
               TEST3("dclz $v0, $v1", reg_val2[i], v0, v1);
               break;

            case DDIV:
               /* If the divisor in GPR rt is zero, the arithmetic result value
                  is UNPREDICTABLE. */
               if (reg_val1[N-i-1] != 0)
                  TEST4("ddiv $t0, $t1", reg_val1[i], reg_val1[N-i-1], t0, t1);

               if (reg_val2[N-i-1] != 0)
                  TEST4("ddiv $v0, $v1", reg_val2[i], reg_val2[N-i-1], v0, v1);

               break;

            case DDIVU:
               /* If the divisor in GPR rt is zero, the arithmetic result value
                  is UNPREDICTABLE. */
               if (reg_val1[N-i-1] != 0)
                  TEST4("ddivu $t0, $t1", reg_val1[i], reg_val1[N-i-1], t0, t1);

               if (reg_val2[N-i-1] != 0)
                  TEST4("ddivu $v0, $v1", reg_val2[i], reg_val2[N-i-1], v0, v1);

               break;

            case DIV:
               /* If either GPR rt or GPR rs does not contain sign-extended
                  32-bit values (bits 63..31 equal), then the result of the
                  operation is UNPREDICTABLE.
                  If the divisor in GPR rt is zero, the arithmetic result
                  value is UNPREDICTABLE. */
               if (reg_val1[N-i-1] != 0)
                  TEST4("div $t0, $t1", reg_val1[i], reg_val1[N-i-1], t0, t1);

               break;

            case DIVU:
               /* If either GPR rt or GPR rs does not contain sign-extended
                  32-bit values (bits 63..31 equal), then the result of the
                  operation is UNPREDICTABLE.
                  If the divisor in GPR rt is zero, the arithmetic result
                  value is UNPREDICTABLE. */
               if (reg_val1[N-i-1] != 0)
                  TEST4("divu $t0, $t1", reg_val1[i], reg_val1[N-i-1], t0, t1);

               break;

            case DMULT:
               /* No arithmetic exception occurs under any circumstances. */
               TEST4("dmult $t0, $t1", reg_val1[i], reg_val1[N-i-1], t0, t1);
               TEST4("dmult $v0, $v1", reg_val2[i], reg_val2[N-i-1], v0, v1);
               break;

            case DMULTU:
               /* No arithmetic exception occurs under any circumstances. */
               TEST4("dmultu $t0, $t1", reg_val1[i], reg_val1[N-i-1], t0, t1);
               TEST4("dmultu $v0, $v1", reg_val2[i], reg_val2[N-i-1], v0, v1);
               break;

            case DSUB:
               /* If the subtraction results in 64-bit 2âs complement
                  arithmetic overflow, then the destination register is not
                  modified and an Integer Overflow exception occurs. */
               TEST1("dsub $t0, $t1, $t2", reg_val1[i], reg_val1[N-i-1],
                                           t0, t1, t2);
               break;

            case DSUBU:
               /* No Integer Overflow exception occurs under any
                  circumstances. */
               TEST1("dsubu $t0, $t1, $t2", reg_val1[i], reg_val1[N-i-1],
                                            t0, t1, t2);
               TEST1("dsubu $s0, $s1, $s2", reg_val2[i], reg_val2[N-i-1],
                                            s0, s1, s2);
               break;

            case MADD:
               /* If GPRs rs or rt do not contain sign-extended 32-bit
                  values (bits 63..31 equal), then the results of the operation
                  are UNPREDICTABLE. */
               TEST5("madd $t0, $t1", reg_val1[i], reg_val1[N-i-1], t0, t1);
               break;

            case MADDU:
               /* If GPRs rs or rt do not contain sign-extended 32-bit
                  values (bits 63..31 equal), then the results of the operation
                  are UNPREDICTABLE. */
               TEST5("maddu $t0, $t1", reg_val1[i], reg_val1[N-i-1], t0, t1);
               break;

            case MSUB:
               /* If GPR rs or rt do not contain a sign-extended 32-bit
                  value (bits 63..31 equal), then the results of the operation
                  are UNPREDICTABLE. */
               TEST5("msub $t0, $t1", reg_val1[i], reg_val1[N-i-1], t0, t1);
               break;

            case MSUBU:
               /* If GPRs rs or rt do not contain sign-extended 32-bit
                  values (bits 63..31 equal), then the results of the operation
                  are UNPREDICTABLE.
                  This instruction does not provide the capability of writing
                  directly to a target GPR. */
               TEST5("msubu $t0, $t1", reg_val1[i], reg_val1[N-i-1], t0, t1);
               break;

            case MUL:
               /* On 64-bit processors, if either GPR rt or GPR rs does not
                  contain sign-extended 32-bit values (bits 63..31 equal), then
                  the result of the operation is UNPREDICTABLE. */
               TEST1("mul $t0, $t1, $t2", reg_val1[i], reg_val1[N-i-1],
                                          t0, t1, t2);
               break;

            case MULT:
               /* On 64-bit processors, if either GPR rt or GPR rs does not
                  contain sign-extended 32-bit values (bits 63..31 equal), then
                  the result of the operation is UNPREDICTABLE. */
               TEST4("mult $t0, $t1", reg_val1[i], reg_val1[N-i-1], t0, t1);
               break;

            case MULTU:
               /* On 64-bit processors, if either GPR rt or GPR rs does not 
                  contain sign-extended 32-bit values (bits 63..31 equal), then
                  the result of the operation is UNPREDICTABLE. */
               TEST4("multu $t0, $t1", reg_val1[i], reg_val1[N-i-1], t0, t1);
               break;

            case MOVN:
               /* The arithmetic comparison does not cause an Integer Overflow
                  exception. */
               TEST1("movn $t0, $t1, $t2", reg_val1[i], reg_val1[N-i-1],
                                           t0, t1, t2);
               TEST1("movn $s0, $s1, $s2", reg_val2[i], reg_val2[N-i-1],
                                           s0, s1, s2);
               break;

            case MOVZ:
               /* The arithmetic comparison does not cause an Integer Overflow
                  exception. */
               TEST1("movz $t0, $t1, $t2", reg_val1[i], reg_val1[N-i-1],
                                           t0, t1, t2);
               TEST1("movz $s0, $s1, $s2", reg_val2[i], reg_val2[N-i-1],
                                           s0, s1, s2);
               break;

            case SEB:
#if (__mips==64) && (__mips_isa_rev>=2)
               /* If GPR rt does not contain a sign-extended 32-bit
                  value (bits 63..31 equal), then the result of the operation
                  is UNPREDICTABLE. */
               TEST3("seb $t0, $t1", reg_val1[i], t0, t1);
#endif
               break;

            case SEH:
#if (__mips==64) && (__mips_isa_rev>=2)
               /* If GPR rt does not contain a sign-extended 32-bit
                  value (bits 63..31 equal), then the result of the operation
                  is UNPREDICTABLE. */
               TEST3("seh $t0, $t1", reg_val1[i], t0, t1);
#endif
               break;

            case SLT:
               /* The arithmetic comparison does not cause an Integer Overflow
                  exception. */
               TEST1("slt $t0, $t1, $t2", reg_val1[i], reg_val1[N-i-1],
                                          t0, t1, t2);
               break;

            case SLTI:
               /* The arithmetic comparison does not cause an Integer Overflow
                  exception. */
               TEST2("slti $t0, $t1, 0xff",   reg_val1[i], 0xff,   t0, t1);
               TEST2("slti $t2, $t3, 0xffff", reg_val1[i], 0xffff, t2, t3);
               TEST2("slti $a0, $a1, 0x0",    reg_val1[i], 0x0,    a0, a1);
               TEST2("slti $s0, $s1, 0x23",   reg_val1[i], 0x23,   s0, s1);
               TEST2("slti $t0, $t1, 0xff",   reg_val2[i], 0xff,   t0, t1);
               TEST2("slti $t2, $t3, 0xffff", reg_val2[i], 0xffff, t2, t3);
               TEST2("slti $a0, $a1, 0x0",    reg_val2[i], 0x0,    a0, a1);
               TEST2("slti $s0, $s1, 0x23",   reg_val2[i], 0x23,   s0, s1);
               break;

            case SLTIU:
               /* The arithmetic comparison does not cause an Integer Overflow
                  exception. */
               TEST2("sltiu $t0, $t1, 0xff",   reg_val1[i], 0xff,   t0, t1);
               TEST2("sltiu $t2, $t3, 0xffff", reg_val1[i], 0xffff, t2, t3);
               TEST2("sltiu $a0, $a1, 0x0",    reg_val1[i], 0x0,    a0, a1);
               TEST2("sltiu $s0, $s1, 0x23",   reg_val1[i], 0x23,   s0, s1);
               TEST2("sltiu $t0, $t1, 0xff",   reg_val2[i], 0xff,   t0, t1);
               TEST2("sltiu $t2, $t3, 0xffff", reg_val2[i], 0xffff, t2, t3);
               TEST2("sltiu $a0, $a1, 0x0",    reg_val2[i], 0x0,    a0, a1);
               TEST2("sltiu $s0, $s1, 0x23",   reg_val2[i], 0x23,   s0, s1);
               break;

            case SLTU:
               /* The arithmetic comparison does not cause an Integer Overflow
                  exception. */
               TEST1("sltu $t0, $t1, $t2", reg_val1[i], reg_val1[N-i-1],
                                           t0, t1, t2);
               TEST1("sltu $s0, $s1, $s2", reg_val2[i], reg_val2[N-i-1],
                                           s0, s1, s2);
               break;

            case SUB:
               /* On 64-bit processors, if either GPR rt or GPR rs does not
                  contain sign-extended 32-bit values (bits 63..31 equal), then
                  the result of the operation is UNPREDICTABLE. */
               if (i < 8 || (i > 15 && i < 22))
                  TEST1("sub $t0, $t1, $t2", reg_val1[i], reg_val1[N-i-1],
                                             t0, t1, t2);
               break;

            case SUBU:
               /* On 64-bit processors, if either GPR rt or GPR rs does not
                  contain sign-extended 32-bit values (bits 63..31 equal), then
                  the result of the operation is UNPREDICTABLE. */
               TEST1("subu $t0, $t1, $t2", reg_val1[i], reg_val1[N-i-1],
                                           t0, t1, t2);
               break;

            default:
               printf("Error!\n");
               break;
         }
      }
   }
   return 0;
}
