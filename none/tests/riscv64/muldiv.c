/* Tests for the RV64M standard multiplication and division instruction-set
   extension. */

#include "testinst.h"

static void test_muldiv_shared(void)
{
   printf(
      "RV64M multiplication and division instruction set, shared operations\n");

   /* ------------------ mul rd, rs1, rs2 ------------------- */
   TESTINST_1_2(4, "mul a0, a1, a2", 0x0000000000005000, 0x0000000000002000, a0,
                a1, a2);
   TESTINST_1_2(4, "mul a0, a1, a2", 0x7fffffffffffffff, 0x0000000000000002, a0,
                a1, a2);
   TESTINST_1_2(4, "mul a0, a1, a2", 0x7fffffffffffffff, 0x7fffffffffffffff, a0,
                a1, a2);
   TESTINST_1_2(4, "mul a0, a1, a2", 0x7fffffffffffffff, 0xffffffffffffffff, a0,
                a1, a2);
   TESTINST_1_2(4, "mul a0, a1, a2", 0xffffffffffffffff, 0xffffffffffffffff, a0,
                a1, a2);
   TESTINST_1_2(4, "mul a0, a1, a2", 0x8000000000000000, 0x0000000000000002, a0,
                a1, a2);
   TESTINST_1_2(4, "mul a0, a1, a2", 0x8000000000000000, 0xffffffffffffffff, a0,
                a1, a2);
   TESTINST_1_2(4, "mul a0, a1, a2", 0x8000000000000000, 0x8000000000000000, a0,
                a1, a2);
   TESTINST_1_2(4, "mul a0, a1, a2", 0x0000000000000001, 0x0000000000000000, a0,
                a1, a2);
   TESTINST_1_2(4, "mul a0, a1, a2", 0xffffffffffffffff, 0x0000000000000000, a0,
                a1, a2);

   TESTINST_1_2(4, "mul t4, t5, t6", 0x0000000000001000, 0x0000000000002000, t4,
                t5, t6);
   TESTINST_1_2(4, "mul zero, a0, a1", 0x0000000000001000, 0x0000000000002000,
                zero, a0, a1);

   /* ------------------ mulh rd, rs1, rs2 ------------------ */
   TESTINST_1_2(4, "mulh a0, a1, a2", 0x0000000000005000, 0x0000000000002000,
                a0, a1, a2);
   TESTINST_1_2(4, "mulh a0, a1, a2", 0x7fffffffffffffff, 0x0000000000000002,
                a0, a1, a2);
   TESTINST_1_2(4, "mulh a0, a1, a2", 0x7fffffffffffffff, 0x7fffffffffffffff,
                a0, a1, a2);
   TESTINST_1_2(4, "mulh a0, a1, a2", 0x7fffffffffffffff, 0xffffffffffffffff,
                a0, a1, a2);
   TESTINST_1_2(4, "mulh a0, a1, a2", 0xffffffffffffffff, 0xffffffffffffffff,
                a0, a1, a2);
   TESTINST_1_2(4, "mulh a0, a1, a2", 0x8000000000000000, 0x0000000000000002,
                a0, a1, a2);
   TESTINST_1_2(4, "mulh a0, a1, a2", 0x8000000000000000, 0xffffffffffffffff,
                a0, a1, a2);
   TESTINST_1_2(4, "mulh a0, a1, a2", 0x8000000000000000, 0x8000000000000000,
                a0, a1, a2);
   TESTINST_1_2(4, "mulh a0, a1, a2", 0x0000000000000001, 0x0000000000000000,
                a0, a1, a2);
   TESTINST_1_2(4, "mulh a0, a1, a2", 0xffffffffffffffff, 0x0000000000000000,
                a0, a1, a2);

   TESTINST_1_2(4, "mulh t4, t5, t6", 0x0000000000001000, 0x0000000000002000,
                t4, t5, t6);
   TESTINST_1_2(4, "mulh zero, a0, a1", 0x0000000000001000, 0x0000000000002000,
                zero, a0, a1);

   /* ----------------- mulhsu rd, rs1, rs2 ----------------- */
   /* Not currently handled. */

   /* ----------------- mulhu rd, rs1, rs2 ------------------ */
   TESTINST_1_2(4, "mulhu a0, a1, a2", 0x0000000000005000, 0x0000000000002000,
                a0, a1, a2);
   TESTINST_1_2(4, "mulhu a0, a1, a2", 0x7fffffffffffffff, 0x0000000000000002,
                a0, a1, a2);
   TESTINST_1_2(4, "mulhu a0, a1, a2", 0x7fffffffffffffff, 0x7fffffffffffffff,
                a0, a1, a2);
   TESTINST_1_2(4, "mulhu a0, a1, a2", 0x7fffffffffffffff, 0xffffffffffffffff,
                a0, a1, a2);
   TESTINST_1_2(4, "mulhu a0, a1, a2", 0xffffffffffffffff, 0xffffffffffffffff,
                a0, a1, a2);
   TESTINST_1_2(4, "mulhu a0, a1, a2", 0x8000000000000000, 0x0000000000000002,
                a0, a1, a2);
   TESTINST_1_2(4, "mulhu a0, a1, a2", 0x8000000000000000, 0xffffffffffffffff,
                a0, a1, a2);
   TESTINST_1_2(4, "mulhu a0, a1, a2", 0x8000000000000000, 0x8000000000000000,
                a0, a1, a2);
   TESTINST_1_2(4, "mulhu a0, a1, a2", 0x0000000000000001, 0x0000000000000000,
                a0, a1, a2);
   TESTINST_1_2(4, "mulhu a0, a1, a2", 0xffffffffffffffff, 0x0000000000000000,
                a0, a1, a2);

   TESTINST_1_2(4, "mulhu t4, t5, t6", 0x0000000000001000, 0x0000000000002000,
                t4, t5, t6);
   TESTINST_1_2(4, "mulhu zero, a0, a1", 0x0000000000001000, 0x0000000000002000,
                zero, a0, a1);

   /* ------------------ div rd, rs1, rs2 ------------------- */
   TESTINST_1_2(4, "div a0, a1, a2", 0x0000000000005000, 0x0000000000002000, a0,
                a1, a2);
   TESTINST_1_2(4, "div a0, a1, a2", 0x7fffffffffffffff, 0x0000000000000002, a0,
                a1, a2);
   TESTINST_1_2(4, "div a0, a1, a2", 0x7fffffffffffffff, 0x7fffffffffffffff, a0,
                a1, a2);
   TESTINST_1_2(4, "div a0, a1, a2", 0x7fffffffffffffff, 0xffffffffffffffff, a0,
                a1, a2);
   TESTINST_1_2(4, "div a0, a1, a2", 0xffffffffffffffff, 0xffffffffffffffff, a0,
                a1, a2);
   TESTINST_1_2(4, "div a0, a1, a2", 0x8000000000000000, 0x0000000000000002, a0,
                a1, a2);
   TESTINST_1_2(4, "div a0, a1, a2", 0x8000000000000000, 0xffffffffffffffff, a0,
                a1, a2);
   TESTINST_1_2(4, "div a0, a1, a2", 0x8000000000000000, 0x8000000000000000, a0,
                a1, a2);
   TESTINST_1_2(4, "div a0, a1, a2", 0x0000000000000001, 0x0000000000000000, a0,
                a1, a2);
   TESTINST_1_2(4, "div a0, a1, a2", 0xffffffffffffffff, 0x0000000000000000, a0,
                a1, a2);

   TESTINST_1_2(4, "div t4, t5, t6", 0x0000000000005000, 0x0000000000002000, t4,
                t5, t6);
   TESTINST_1_2(4, "div zero, a0, a1", 0x0000000000005000, 0x0000000000002000,
                zero, a0, a1);

   /* ------------------ divu rd, rs1, rs2 ------------------ */
   TESTINST_1_2(4, "divu a0, a1, a2", 0x0000000000005000, 0x0000000000002000,
                a0, a1, a2);
   TESTINST_1_2(4, "divu a0, a1, a2", 0x7fffffffffffffff, 0x0000000000000002,
                a0, a1, a2);
   TESTINST_1_2(4, "divu a0, a1, a2", 0x7fffffffffffffff, 0x7fffffffffffffff,
                a0, a1, a2);
   TESTINST_1_2(4, "divu a0, a1, a2", 0x7fffffffffffffff, 0xffffffffffffffff,
                a0, a1, a2);
   TESTINST_1_2(4, "divu a0, a1, a2", 0xffffffffffffffff, 0xffffffffffffffff,
                a0, a1, a2);
   TESTINST_1_2(4, "divu a0, a1, a2", 0x8000000000000000, 0x0000000000000002,
                a0, a1, a2);
   TESTINST_1_2(4, "divu a0, a1, a2", 0x8000000000000000, 0xffffffffffffffff,
                a0, a1, a2);
   TESTINST_1_2(4, "divu a0, a1, a2", 0x8000000000000000, 0x8000000000000000,
                a0, a1, a2);
   TESTINST_1_2(4, "divu a0, a1, a2", 0x0000000000000001, 0x0000000000000000,
                a0, a1, a2);
   TESTINST_1_2(4, "divu a0, a1, a2", 0xffffffffffffffff, 0x0000000000000000,
                a0, a1, a2);

   TESTINST_1_2(4, "divu t4, t5, t6", 0x0000000000005000, 0x0000000000002000,
                t4, t5, t6);
   TESTINST_1_2(4, "divu zero, a0, a1", 0x0000000000005000, 0x0000000000002000,
                zero, a0, a1);

   /* ------------------ rem rd, rs1, rs2 ------------------- */
   TESTINST_1_2(4, "rem a0, a1, a2", 0x0000000000005000, 0x0000000000002000, a0,
                a1, a2);
   TESTINST_1_2(4, "rem a0, a1, a2", 0x7fffffffffffffff, 0x0000000000000002, a0,
                a1, a2);
   TESTINST_1_2(4, "rem a0, a1, a2", 0x7fffffffffffffff, 0x7fffffffffffffff, a0,
                a1, a2);
   TESTINST_1_2(4, "rem a0, a1, a2", 0x7fffffffffffffff, 0xffffffffffffffff, a0,
                a1, a2);
   TESTINST_1_2(4, "rem a0, a1, a2", 0xffffffffffffffff, 0xffffffffffffffff, a0,
                a1, a2);
   TESTINST_1_2(4, "rem a0, a1, a2", 0x8000000000000000, 0x0000000000000002, a0,
                a1, a2);
   TESTINST_1_2(4, "rem a0, a1, a2", 0x8000000000000000, 0xffffffffffffffff, a0,
                a1, a2);
   TESTINST_1_2(4, "rem a0, a1, a2", 0x8000000000000000, 0x8000000000000000, a0,
                a1, a2);
   TESTINST_1_2(4, "rem a0, a1, a2", 0x0000000000000001, 0x0000000000000000, a0,
                a1, a2);
   TESTINST_1_2(4, "rem a0, a1, a2", 0xffffffffffffffff, 0x0000000000000000, a0,
                a1, a2);

   TESTINST_1_2(4, "rem t4, t5, t6", 0x0000000000005000, 0x0000000000002000, t4,
                t5, t6);
   TESTINST_1_2(4, "rem zero, a0, a1", 0x0000000000005000, 0x0000000000002000,
                zero, a0, a1);

   /* ------------------ remu rd, rs1, rs2 ------------------ */
   TESTINST_1_2(4, "remu a0, a1, a2", 0x0000000000005000, 0x0000000000002000,
                a0, a1, a2);
   TESTINST_1_2(4, "remu a0, a1, a2", 0x7fffffffffffffff, 0x0000000000000002,
                a0, a1, a2);
   TESTINST_1_2(4, "remu a0, a1, a2", 0x7fffffffffffffff, 0x7fffffffffffffff,
                a0, a1, a2);
   TESTINST_1_2(4, "remu a0, a1, a2", 0x7fffffffffffffff, 0xffffffffffffffff,
                a0, a1, a2);
   TESTINST_1_2(4, "remu a0, a1, a2", 0xffffffffffffffff, 0xffffffffffffffff,
                a0, a1, a2);
   TESTINST_1_2(4, "remu a0, a1, a2", 0x8000000000000000, 0x0000000000000002,
                a0, a1, a2);
   TESTINST_1_2(4, "remu a0, a1, a2", 0x8000000000000000, 0xffffffffffffffff,
                a0, a1, a2);
   TESTINST_1_2(4, "remu a0, a1, a2", 0x8000000000000000, 0x8000000000000000,
                a0, a1, a2);
   TESTINST_1_2(4, "remu a0, a1, a2", 0x0000000000000001, 0x0000000000000000,
                a0, a1, a2);
   TESTINST_1_2(4, "remu a0, a1, a2", 0xffffffffffffffff, 0x0000000000000000,
                a0, a1, a2);

   TESTINST_1_2(4, "remu t4, t5, t6", 0x0000000000005000, 0x0000000000002000,
                t4, t5, t6);
   TESTINST_1_2(4, "remu zero, a0, a1", 0x0000000000005000, 0x0000000000002000,
                zero, a0, a1);

   printf("\n");
}

static void test_muldiv_additions(void)
{
   printf("RV64M multiplication and division instruction set, additions\n");

   /* ------------------ mulw rd, rs1, rs2 ------------------ */
   TESTINST_1_2(4, "mulw a0, a1, a2", 0x0000000000005000, 0x0000000000002000,
                a0, a1, a2);
   TESTINST_1_2(4, "mulw a0, a1, a2", 0x000000007fffffff, 0x0000000000000002,
                a0, a1, a2);
   TESTINST_1_2(4, "mulw a0, a1, a2", 0x000000007fffffff, 0x000000007fffffff,
                a0, a1, a2);
   TESTINST_1_2(4, "mulw a0, a1, a2", 0x000000007fffffff, 0xffffffffffffffff,
                a0, a1, a2);
   TESTINST_1_2(4, "mulw a0, a1, a2", 0xffffffffffffffff, 0xffffffffffffffff,
                a0, a1, a2);
   TESTINST_1_2(4, "mulw a0, a1, a2", 0x0000000080000000, 0x0000000000000002,
                a0, a1, a2);
   TESTINST_1_2(4, "mulw a0, a1, a2", 0x0000000080000000, 0xffffffffffffffff,
                a0, a1, a2);
   TESTINST_1_2(4, "mulw a0, a1, a2", 0x0000000080000000, 0x0000000080000000,
                a0, a1, a2);
   TESTINST_1_2(4, "mulw a0, a1, a2", 0x0000000000000001, 0x0000000000000000,
                a0, a1, a2);
   TESTINST_1_2(4, "mulw a0, a1, a2", 0xffffffffffffffff, 0x0000000000000000,
                a0, a1, a2);

   TESTINST_1_2(4, "mulw t4, t5, t6", 0x0000000000001000, 0x0000000000002000,
                t4, t5, t6);
   TESTINST_1_2(4, "mulw zero, a0, a1", 0x0000000000001000, 0x0000000000002000,
                zero, a0, a1);

   /* ------------------ divw rd, rs1, rs2 ------------------ */
   TESTINST_1_2(4, "divw a0, a1, a2", 0x0000000000005000, 0x0000000000002000,
                a0, a1, a2);
   TESTINST_1_2(4, "divw a0, a1, a2", 0x000000007fffffff, 0x0000000000000002,
                a0, a1, a2);
   TESTINST_1_2(4, "divw a0, a1, a2", 0x000000007fffffff, 0x000000007fffffff,
                a0, a1, a2);
   TESTINST_1_2(4, "divw a0, a1, a2", 0x000000007fffffff, 0xffffffffffffffff,
                a0, a1, a2);
   TESTINST_1_2(4, "divw a0, a1, a2", 0xffffffffffffffff, 0xffffffffffffffff,
                a0, a1, a2);
   TESTINST_1_2(4, "divw a0, a1, a2", 0x0000000080000000, 0x0000000000000002,
                a0, a1, a2);
   TESTINST_1_2(4, "divw a0, a1, a2", 0x0000000080000000, 0xffffffffffffffff,
                a0, a1, a2);
   TESTINST_1_2(4, "divw a0, a1, a2", 0x0000000080000000, 0x0000000080000000,
                a0, a1, a2);
   TESTINST_1_2(4, "divw a0, a1, a2", 0x0000000000000001, 0x0000000000000000,
                a0, a1, a2);
   TESTINST_1_2(4, "divw a0, a1, a2", 0xffffffffffffffff, 0x0000000000000000,
                a0, a1, a2);

   TESTINST_1_2(4, "divw t4, t5, t6", 0x0000000000001000, 0x0000000000002000,
                t4, t5, t6);
   TESTINST_1_2(4, "divw zero, a0, a1", 0x0000000000001000, 0x0000000000002000,
                zero, a0, a1);

   /* ----------------- divuw rd, rs1, rs2 ------------------ */
   TESTINST_1_2(4, "divuw a0, a1, a2", 0x0000000000005000, 0x0000000000002000,
                a0, a1, a2);
   TESTINST_1_2(4, "divuw a0, a1, a2", 0x000000007fffffff, 0x0000000000000002,
                a0, a1, a2);
   TESTINST_1_2(4, "divuw a0, a1, a2", 0x000000007fffffff, 0x000000007fffffff,
                a0, a1, a2);
   TESTINST_1_2(4, "divuw a0, a1, a2", 0x000000007fffffff, 0xffffffffffffffff,
                a0, a1, a2);
   TESTINST_1_2(4, "divuw a0, a1, a2", 0xffffffffffffffff, 0xffffffffffffffff,
                a0, a1, a2);
   TESTINST_1_2(4, "divuw a0, a1, a2", 0x0000000080000000, 0x0000000000000002,
                a0, a1, a2);
   TESTINST_1_2(4, "divuw a0, a1, a2", 0x0000000080000000, 0xffffffffffffffff,
                a0, a1, a2);
   TESTINST_1_2(4, "divuw a0, a1, a2", 0x0000000080000000, 0x0000000080000000,
                a0, a1, a2);
   TESTINST_1_2(4, "divuw a0, a1, a2", 0x0000000000000001, 0x0000000000000000,
                a0, a1, a2);
   TESTINST_1_2(4, "divuw a0, a1, a2", 0xffffffffffffffff, 0x0000000000000000,
                a0, a1, a2);

   TESTINST_1_2(4, "divuw t4, t5, t6", 0x0000000000001000, 0x0000000000002000,
                t4, t5, t6);
   TESTINST_1_2(4, "divuw zero, a0, a1", 0x0000000000001000, 0x0000000000002000,
                zero, a0, a1);

   /* ------------------ remw rd, rs1, rs2 ------------------ */
   TESTINST_1_2(4, "remw a0, a1, a2", 0x0000000000005000, 0x0000000000002000,
                a0, a1, a2);
   TESTINST_1_2(4, "remw a0, a1, a2", 0x000000007fffffff, 0x0000000000000002,
                a0, a1, a2);
   TESTINST_1_2(4, "remw a0, a1, a2", 0x000000007fffffff, 0x000000007fffffff,
                a0, a1, a2);
   TESTINST_1_2(4, "remw a0, a1, a2", 0x000000007fffffff, 0xffffffffffffffff,
                a0, a1, a2);
   TESTINST_1_2(4, "remw a0, a1, a2", 0xffffffffffffffff, 0xffffffffffffffff,
                a0, a1, a2);
   TESTINST_1_2(4, "remw a0, a1, a2", 0x0000000080000000, 0x0000000000000002,
                a0, a1, a2);
   TESTINST_1_2(4, "remw a0, a1, a2", 0x0000000080000000, 0xffffffffffffffff,
                a0, a1, a2);
   TESTINST_1_2(4, "remw a0, a1, a2", 0x0000000080000000, 0x0000000080000000,
                a0, a1, a2);
   TESTINST_1_2(4, "remw a0, a1, a2", 0x0000000000000001, 0x0000000000000000,
                a0, a1, a2);
   TESTINST_1_2(4, "remw a0, a1, a2", 0xffffffffffffffff, 0x0000000000000000,
                a0, a1, a2);

   TESTINST_1_2(4, "remw t4, t5, t6", 0x0000000000001000, 0x0000000000002000,
                t4, t5, t6);
   TESTINST_1_2(4, "remw zero, a0, a1", 0x0000000000001000, 0x0000000000002000,
                zero, a0, a1);

   /* ----------------- remuw rd, rs1, rs2 ------------------ */
   TESTINST_1_2(4, "remuw a0, a1, a2", 0x0000000000005000, 0x0000000000002000,
                a0, a1, a2);
   TESTINST_1_2(4, "remuw a0, a1, a2", 0x000000007fffffff, 0x0000000000000002,
                a0, a1, a2);
   TESTINST_1_2(4, "remuw a0, a1, a2", 0x000000007fffffff, 0x000000007fffffff,
                a0, a1, a2);
   TESTINST_1_2(4, "remuw a0, a1, a2", 0x000000007fffffff, 0xffffffffffffffff,
                a0, a1, a2);
   TESTINST_1_2(4, "remuw a0, a1, a2", 0xffffffffffffffff, 0xffffffffffffffff,
                a0, a1, a2);
   TESTINST_1_2(4, "remuw a0, a1, a2", 0x0000000080000000, 0x0000000000000002,
                a0, a1, a2);
   TESTINST_1_2(4, "remuw a0, a1, a2", 0x0000000080000000, 0xffffffffffffffff,
                a0, a1, a2);
   TESTINST_1_2(4, "remuw a0, a1, a2", 0x0000000080000000, 0x0000000080000000,
                a0, a1, a2);
   TESTINST_1_2(4, "remuw a0, a1, a2", 0x0000000000000001, 0x0000000000000000,
                a0, a1, a2);
   TESTINST_1_2(4, "remuw a0, a1, a2", 0xffffffffffffffff, 0x0000000000000000,
                a0, a1, a2);

   TESTINST_1_2(4, "remuw t4, t5, t6", 0x0000000000001000, 0x0000000000002000,
                t4, t5, t6);
   TESTINST_1_2(4, "remuw zero, a0, a1", 0x0000000000001000, 0x0000000000002000,
                zero, a0, a1);
}

int main(void)
{
   test_muldiv_shared();
   test_muldiv_additions();
   return 0;
}
