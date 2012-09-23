/* Test signed integer comparison ops
   cr, cgr, cgfr, c, cg, cgf, cfi, cgfi

   missing: cy, crl, cgrl, cgfrl
*/

#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>
#include <limits.h>
#include "opcodes.h"

#undef RIL_RI
#define RIL_RI(op1,r1,op2,i2)  \
            ".short 0x" #op1 #r1 #op2 "\n\t"  \
            ".long  " #i2 "\n\t"


/* Perform a single signed comparison
   Both operands in register */
#define SCOMP_REG_REG(insn, v1, v2)        \
({                                         \
   int cc;                                 \
   int64_t op1 = v1;                       \
   int64_t op2 = v2;                       \
   asm volatile(   #insn " %1, %2\n\t"     \
                   "ipm %0\n\t"            \
                   "srl %0,28\n\t"         \
                   : "=d" (cc)             \
                   : "d" (op1), "d" (op2)  \
                   : "cc");                \
   printf("%.6s (%"PRId64", %"PRId64") --> cc = %d\n", \
          #insn, op1, op2, cc);            \
})

/* Perform a single signed comparison
   Left operand in register, right operand in memory */
#define SCOMP_REG_MEM(insn, v1, v2, op2_t) \
({                                         \
   int cc;                                 \
   int64_t op1 = v1;                       \
   op2_t   op2 = v2;                       \
   asm volatile(   #insn " %1, %2\n\t"     \
                   "ipm %0\n\t"            \
                   "srl %0,28\n\t"         \
                   : "=d" (cc)             \
                   : "d" (op1), "Q" (op2)  \
                   : "cc");                \
   printf("%.6s (%"PRId64", %"PRId64") --> cc = %d\n", \
          #insn, op1, (int64_t)op2, cc);            \
})

/* Perform a single signed comparison
   Left operand in register, right operand is an immediate constant */
#define SCOMP_REG_IMM(insn, v1, v2)        \
({                                         \
   int cc;                                 \
   register int64_t op1 asm("8") = v1;     \
   asm volatile(   insn(8, v2)             \
                   "ipm %0\n\t"            \
                   "srl %0,28\n\t"         \
                   : "=d" (cc)             \
                   : "d" (op1)             \
                   : "cc");           \
   printf("%.6s (%"PRId64", %"PRId64") --> cc = %d\n", \
          #insn, op1, (int64_t)v2, cc);            \
})

/* Run a sequence of signed comparisons for a given insn */
#define run_scomp_reg_reg(insn) \
({                              \
   SCOMP_REG_REG(insn,  0,  0); \
   SCOMP_REG_REG(insn,  0,  1); \
   SCOMP_REG_REG(insn,  0, -1); \
   SCOMP_REG_REG(insn,  1,  0); \
   SCOMP_REG_REG(insn, -1,  0); \
   SCOMP_REG_REG(insn, -2, -1); \
   SCOMP_REG_REG(insn, -2, -2); \
   SCOMP_REG_REG(insn, -2, -3); \
   SCOMP_REG_REG(insn,  2,  1); \
   SCOMP_REG_REG(insn,  2,  2); \
   SCOMP_REG_REG(insn,  2,  3); \
   SCOMP_REG_REG(insn, -2,  1); \
   SCOMP_REG_REG(insn,  2, -1); \
   SCOMP_REG_REG(insn,  INT8_MIN,   INT8_MIN); \
   SCOMP_REG_REG(insn,  INT8_MIN,   INT8_MAX); \
   SCOMP_REG_REG(insn,  INT8_MAX,   INT8_MIN); \
   SCOMP_REG_REG(insn,  INT8_MAX,   INT8_MAX); \
   SCOMP_REG_REG(insn,  INT16_MIN,  INT16_MIN); \
   SCOMP_REG_REG(insn,  INT16_MIN,  INT16_MAX); \
   SCOMP_REG_REG(insn,  INT16_MAX,  INT16_MIN); \
   SCOMP_REG_REG(insn,  INT16_MAX,  INT16_MAX); \
   SCOMP_REG_REG(insn,  INT32_MIN,  INT32_MIN); \
   SCOMP_REG_REG(insn,  INT32_MIN,  INT32_MAX); \
   SCOMP_REG_REG(insn,  INT32_MAX,  INT32_MIN); \
   SCOMP_REG_REG(insn,  INT32_MAX,  INT32_MAX); \
})

/* Run a sequence of signed comparisons for a given insn */
#define run_scomp_reg_mem(insn, op2_t) \
({                              \
   SCOMP_REG_MEM(insn,  0,  0, op2_t); \
   SCOMP_REG_MEM(insn,  0,  1, op2_t); \
   SCOMP_REG_MEM(insn,  0, -1, op2_t); \
   SCOMP_REG_MEM(insn,  1,  0, op2_t); \
   SCOMP_REG_MEM(insn, -1,  0, op2_t); \
   SCOMP_REG_MEM(insn, -2, -1, op2_t); \
   SCOMP_REG_MEM(insn, -2, -2, op2_t); \
   SCOMP_REG_MEM(insn, -2, -3, op2_t); \
   SCOMP_REG_MEM(insn,  2,  1, op2_t); \
   SCOMP_REG_MEM(insn,  2,  2, op2_t); \
   SCOMP_REG_MEM(insn,  2,  3, op2_t); \
   SCOMP_REG_MEM(insn, -2,  1, op2_t); \
   SCOMP_REG_MEM(insn,  2, -1, op2_t); \
   SCOMP_REG_MEM(insn,  INT8_MIN,   INT8_MIN, op2_t); \
   SCOMP_REG_MEM(insn,  INT8_MIN,   INT8_MAX, op2_t); \
   SCOMP_REG_MEM(insn,  INT8_MAX,   INT8_MIN, op2_t); \
   SCOMP_REG_MEM(insn,  INT8_MAX,   INT8_MAX, op2_t); \
   SCOMP_REG_MEM(insn,  INT16_MIN,  INT16_MIN, op2_t); \
   SCOMP_REG_MEM(insn,  INT16_MIN,  INT16_MAX, op2_t); \
   SCOMP_REG_MEM(insn,  INT16_MAX,  INT16_MIN, op2_t); \
   SCOMP_REG_MEM(insn,  INT16_MAX,  INT16_MAX, op2_t); \
   SCOMP_REG_MEM(insn,  INT32_MIN,  INT32_MIN, op2_t); \
   SCOMP_REG_MEM(insn,  INT32_MIN,  INT32_MAX, op2_t); \
   SCOMP_REG_MEM(insn,  INT32_MAX,  INT32_MIN, op2_t); \
   SCOMP_REG_MEM(insn,  INT32_MAX,  INT32_MAX, op2_t); \
})

/* Run a sequence of signed comparisons for a given insn */
#define run_scomp_reg_imm(insn) \
({                              \
   SCOMP_REG_IMM(insn,  0,  0); \
   SCOMP_REG_IMM(insn,  0,  1); \
   SCOMP_REG_IMM(insn,  0, -1); \
   SCOMP_REG_IMM(insn,  1,  0); \
   SCOMP_REG_IMM(insn, -1,  0); \
   SCOMP_REG_IMM(insn, -2, -1); \
   SCOMP_REG_IMM(insn, -2, -2); \
   SCOMP_REG_IMM(insn, -2, -3); \
   SCOMP_REG_IMM(insn,  2,  1); \
   SCOMP_REG_IMM(insn,  2,  2); \
   SCOMP_REG_IMM(insn,  2,  3); \
   SCOMP_REG_IMM(insn, -2,  1); \
   SCOMP_REG_IMM(insn,  2, -1); \
   SCOMP_REG_IMM(insn,  INT8_MIN,   INT8_MIN); \
   SCOMP_REG_IMM(insn,  INT8_MIN,   INT8_MAX); \
   SCOMP_REG_IMM(insn,  INT8_MAX,   INT8_MIN); \
   SCOMP_REG_IMM(insn,  INT8_MAX,   INT8_MAX); \
   SCOMP_REG_IMM(insn,  INT16_MIN,  INT16_MIN); \
   SCOMP_REG_IMM(insn,  INT16_MIN,  INT16_MAX); \
   SCOMP_REG_IMM(insn,  INT16_MAX,  INT16_MIN); \
   SCOMP_REG_IMM(insn,  INT16_MAX,  INT16_MAX); \
   SCOMP_REG_IMM(insn,  INT32_MIN,  INT32_MIN); \
   SCOMP_REG_IMM(insn,  INT32_MIN,  INT32_MAX); \
   SCOMP_REG_IMM(insn,  INT32_MAX,  INT32_MIN); \
   SCOMP_REG_IMM(insn,  INT32_MAX,  INT32_MAX); \
})

void
signed_comparison_reg_reg(void)
{
   run_scomp_reg_reg(cr);

   run_scomp_reg_reg(cgr);
   /* Special cases for cgr */
   SCOMP_REG_REG(cgr, INT64_MIN, INT64_MIN);
   SCOMP_REG_REG(cgr, INT64_MIN, INT64_MAX);
   SCOMP_REG_REG(cgr, INT64_MAX, INT64_MIN);
   SCOMP_REG_REG(cgr, INT64_MAX, INT64_MAX);

   run_scomp_reg_reg(cgfr);
   /* Special cases for cgfr */
   SCOMP_REG_REG(cgfr, INT64_MIN, INT32_MIN);
   SCOMP_REG_REG(cgfr, INT64_MIN, INT32_MAX);
   SCOMP_REG_REG(cgfr, INT64_MAX, INT32_MIN);
   SCOMP_REG_REG(cgfr, INT64_MAX, INT32_MAX);
}

void
signed_comparison_reg_mem(void)
{
   run_scomp_reg_mem(c, int32_t);

   run_scomp_reg_mem(cg, int64_t);
   /* Special cases for cg */
   SCOMP_REG_MEM(cg, INT64_MIN, INT64_MIN, int64_t);
   SCOMP_REG_MEM(cg, INT64_MIN, INT64_MAX, int64_t);
   SCOMP_REG_MEM(cg, INT64_MAX, INT64_MIN, int64_t);
   SCOMP_REG_MEM(cg, INT64_MAX, INT64_MAX, int64_t);

   run_scomp_reg_mem(cgf, int32_t);
   /* Special cases for cgf */
   SCOMP_REG_MEM(cgf, INT64_MIN, INT32_MIN, int32_t);
   SCOMP_REG_MEM(cgf, INT64_MIN, INT32_MAX, int32_t);
   SCOMP_REG_MEM(cgf, INT64_MAX, INT32_MIN, int32_t);
   SCOMP_REG_MEM(cgf, INT64_MAX, INT32_MAX, int32_t);
}

void
signed_comparison_reg_imm(void)
{
   run_scomp_reg_imm(CFI);

   run_scomp_reg_imm(CGFI);
   /* Special cases for cgfi */
   SCOMP_REG_IMM(CGFI, INT64_MIN, INT32_MIN);
   SCOMP_REG_IMM(CGFI, INT64_MIN, INT32_MAX);
   SCOMP_REG_IMM(CGFI, INT64_MAX, INT32_MIN);
   SCOMP_REG_IMM(CGFI, INT64_MAX, INT32_MAX);
}


int main(void)
{
   signed_comparison_reg_reg();
   signed_comparison_reg_mem();
   signed_comparison_reg_imm();
   
   return 0;
}
