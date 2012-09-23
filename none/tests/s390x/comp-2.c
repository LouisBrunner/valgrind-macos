/* Test unsigned integer comparison ops
   clr, clgr, clgfr, cl, clg, clgf, clfi, clgfi

   missing: cly, clrl, clgrl, clgfrl
*/

#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>
#include <limits.h>
#include "opcodes.h"

#undef RIL_RU
#define RIL_RU(op1,r1,op2,i2)  \
            ".short 0x" #op1 #r1 #op2 "\n\t"  \
            ".long  " #i2 "\n\t"


/* Perform a single unsigned comparison
   Both operands in register */
#define SCOMP_REG_REG(insn, v1, v2)        \
({                                         \
   int cc;                                 \
   uint64_t op1 = v1;                      \
   uint64_t op2 = v2;                      \
   asm volatile(   #insn " %1, %2\n\t"     \
                   "ipm %0\n\t"            \
                   "srl %0,28\n\t"         \
                   : "=d" (cc)             \
                   : "d" (op1), "d" (op2)  \
                   : "cc");                \
   printf("%.6s (%"PRIu64", %"PRIu64") --> cc = %d\n", \
          #insn, op1, op2, cc);            \
})

/* Perform a single unsigned comparison
   Left operand in register, right operand in memory */
#define SCOMP_REG_MEM(insn, v1, v2, op2_t) \
({                                         \
   int cc;                                 \
   uint64_t op1 = v1;                      \
   op2_t    op2 = v2;                      \
   asm volatile(   #insn " %1, %2\n\t"     \
                   "ipm %0\n\t"            \
                   "srl %0,28\n\t"         \
                   : "=d" (cc)             \
                   : "d" (op1), "Q" (op2)  \
                   : "cc");                \
   printf("%.6s (%"PRIu64", %"PRIu64") --> cc = %d\n", \
          #insn, op1, (uint64_t)op2, cc);            \
})

/* Perform a single unsigned comparison
   Left operand in register, right operand is an immediate constant */
#define SCOMP_REG_IMM(insn, v1, v2)        \
({                                         \
   int cc;                                 \
   register uint64_t op1 asm("8") = v1;    \
   asm volatile(   insn(8, v2)             \
                   "ipm %0\n\t"            \
                   "srl %0,28\n\t"         \
                   : "=d" (cc)             \
                   : "d" (op1)             \
                   : "cc");           \
   printf("%.6s (%"PRIu64", %"PRIu64") --> cc = %d\n", \
          #insn, op1, (uint64_t)v2, cc);            \
})

/* Run a sequence of unsigned comparisons for a given insn */
#define run_scomp_reg_reg(insn) \
({                              \
   SCOMP_REG_REG(insn,  0,  0); \
   SCOMP_REG_REG(insn,  0,  1); \
   SCOMP_REG_REG(insn,  1,  0); \
   SCOMP_REG_REG(insn,  2,  1); \
   SCOMP_REG_REG(insn,  2,  2); \
   SCOMP_REG_REG(insn,  2,  3); \
   SCOMP_REG_REG(insn,  0,  INT8_MAX); \
   SCOMP_REG_REG(insn,  INT8_MAX, 0); \
   SCOMP_REG_REG(insn,  INT8_MAX, INT8_MAX-1); \
   SCOMP_REG_REG(insn,  INT8_MAX, INT8_MAX);  \
   SCOMP_REG_REG(insn,  0,  INT16_MAX); \
   SCOMP_REG_REG(insn,  INT16_MAX, 0); \
   SCOMP_REG_REG(insn,  INT16_MAX, INT16_MAX); \
   SCOMP_REG_REG(insn,  INT16_MAX, INT16_MAX-1); \
   SCOMP_REG_REG(insn,  0,  INT32_MAX); \
   SCOMP_REG_REG(insn,  INT32_MAX, 0); \
   SCOMP_REG_REG(insn,  INT32_MAX, INT32_MAX); \
   SCOMP_REG_REG(insn,  INT32_MAX, INT32_MAX-1); \
})

/* Run a sequence of signed comparisons for a given insn */
#define run_scomp_reg_mem(insn, op2_t) \
({                              \
   SCOMP_REG_MEM(insn,  0,  0, op2_t); \
   SCOMP_REG_MEM(insn,  0,  1, op2_t); \
   SCOMP_REG_MEM(insn,  1,  0, op2_t); \
   SCOMP_REG_MEM(insn,  2,  1, op2_t); \
   SCOMP_REG_MEM(insn,  2,  2, op2_t); \
   SCOMP_REG_MEM(insn,  2,  3, op2_t); \
   SCOMP_REG_MEM(insn,  0,   INT8_MAX, op2_t); \
   SCOMP_REG_MEM(insn,  INT8_MAX,   0, op2_t); \
   SCOMP_REG_MEM(insn,  INT8_MAX,   INT8_MAX-1, op2_t); \
   SCOMP_REG_MEM(insn,  INT8_MAX,   INT8_MAX,   op2_t); \
   SCOMP_REG_MEM(insn,  0,  INT16_MAX, op2_t); \
   SCOMP_REG_MEM(insn,  INT16_MAX,  0, op2_t); \
   SCOMP_REG_MEM(insn,  INT16_MAX,  INT16_MAX-1, op2_t); \
   SCOMP_REG_MEM(insn,  INT16_MAX,  INT16_MAX,   op2_t); \
   SCOMP_REG_MEM(insn,  0,  INT32_MAX, op2_t); \
   SCOMP_REG_MEM(insn,  INT32_MAX,  0, op2_t); \
   SCOMP_REG_MEM(insn,  INT32_MAX,  INT32_MAX-1, op2_t); \
   SCOMP_REG_MEM(insn,  INT32_MAX,  INT32_MAX,   op2_t); \
})

/* Run a sequence of signed comparisons for a given insn */
#define run_scomp_reg_imm(insn) \
({                              \
   SCOMP_REG_IMM(insn,  0,  0); \
   SCOMP_REG_IMM(insn,  0,  1); \
   SCOMP_REG_IMM(insn,  1,  0); \
   SCOMP_REG_IMM(insn,  2,  1); \
   SCOMP_REG_IMM(insn,  2,  2); \
   SCOMP_REG_IMM(insn,  2,  3); \
   SCOMP_REG_IMM(insn,  INT8_MAX, 0); \
   SCOMP_REG_IMM(insn,  INT8_MAX, INT8_MAX-1); \
   SCOMP_REG_IMM(insn,  INT8_MAX, INT8_MAX);  \
   SCOMP_REG_IMM(insn,  0,  INT16_MAX); \
   SCOMP_REG_IMM(insn,  INT16_MAX, 0); \
   SCOMP_REG_IMM(insn,  INT16_MAX, INT16_MAX); \
   SCOMP_REG_IMM(insn,  INT16_MAX, INT16_MAX-1); \
   SCOMP_REG_IMM(insn,  0,  INT32_MAX); \
   SCOMP_REG_IMM(insn,  INT32_MAX, 0); \
   SCOMP_REG_IMM(insn,  INT32_MAX, INT32_MAX); \
   SCOMP_REG_IMM(insn,  INT32_MAX, INT32_MAX-1); \
})

void
signed_comparison_reg_reg(void)
{
   run_scomp_reg_reg(clr);

   run_scomp_reg_reg(clgr);
   /* Special cases for clgr */
   SCOMP_REG_REG(clgr, INT64_MIN, INT64_MIN);
   SCOMP_REG_REG(clgr, INT64_MIN, INT64_MAX);
   SCOMP_REG_REG(clgr, INT64_MAX, INT64_MIN);
   SCOMP_REG_REG(clgr, INT64_MAX, INT64_MAX);

   run_scomp_reg_reg(clgfr);
   /* Special cases for clgfr */
   SCOMP_REG_REG(clgfr, INT64_MIN, INT32_MIN);
   SCOMP_REG_REG(clgfr, INT64_MIN, INT32_MAX);
   SCOMP_REG_REG(clgfr, INT64_MAX, INT32_MIN);
   SCOMP_REG_REG(clgfr, INT64_MAX, INT32_MAX);
}

void
signed_comparison_reg_mem(void)
{
   run_scomp_reg_mem(cl, int32_t);

   run_scomp_reg_mem(clg, int64_t);
   /* Special cases for clg */
   SCOMP_REG_MEM(clg, INT64_MIN, INT64_MIN, int64_t);
   SCOMP_REG_MEM(clg, INT64_MIN, INT64_MAX, int64_t);
   SCOMP_REG_MEM(clg, INT64_MAX, INT64_MIN, int64_t);
   SCOMP_REG_MEM(clg, INT64_MAX, INT64_MAX, int64_t);

   run_scomp_reg_mem(clgf, int32_t);
   /* Special cases for clgf */
   SCOMP_REG_MEM(clgf, INT64_MIN, INT32_MIN, int32_t);
   SCOMP_REG_MEM(clgf, INT64_MIN, INT32_MAX, int32_t);
   SCOMP_REG_MEM(clgf, INT64_MAX, INT32_MIN, int32_t);
   SCOMP_REG_MEM(clgf, INT64_MAX, INT32_MAX, int32_t);
}

void
signed_comparison_reg_imm(void)
{
   run_scomp_reg_imm(CLFI);

   run_scomp_reg_imm(CLGFI);
   /* Special cases for clgfi */
   SCOMP_REG_IMM(CLGFI, INT64_MIN, INT32_MIN);
   SCOMP_REG_IMM(CLGFI, INT64_MIN, INT32_MAX);
   SCOMP_REG_IMM(CLGFI, INT64_MAX, INT32_MIN);
   SCOMP_REG_IMM(CLGFI, INT64_MAX, INT32_MAX);
}


int main(void)
{
   signed_comparison_reg_reg();
   signed_comparison_reg_mem();
   signed_comparison_reg_imm();
   
   return 0;
}
