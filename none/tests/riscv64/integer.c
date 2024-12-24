/* Tests for the RV64I base integer instruction set. */

#include "testinst.h"

static void test_integer_shared(void)
{
   printf("RV64I base instruction set, shared operations\n");

   /* ----------------- lui rd, imm[31:12] ------------------ */
   TESTINST_1_0(4, "lui a0, 0", a0);
   TESTINST_1_0(4, "lui a0, 1", a0);
   TESTINST_1_0(4, "lui a0, 2", a0);
   TESTINST_1_0(4, "lui a0, 4", a0);
   TESTINST_1_0(4, "lui a0, 8", a0);
   TESTINST_1_0(4, "lui a0, 16", a0);
   TESTINST_1_0(4, "lui a0, 32", a0);
   TESTINST_1_0(4, "lui a0, 64", a0);
   TESTINST_1_0(4, "lui a0, 128", a0);
   TESTINST_1_0(4, "lui a0, 256", a0);
   TESTINST_1_0(4, "lui a0, 512", a0);
   TESTINST_1_0(4, "lui a0, 1024", a0);
   TESTINST_1_0(4, "lui a0, 2048", a0);
   TESTINST_1_0(4, "lui a0, 4096", a0);
   TESTINST_1_0(4, "lui a0, 8192", a0);
   TESTINST_1_0(4, "lui a0, 16384", a0);
   TESTINST_1_0(4, "lui a0, 32768", a0);
   TESTINST_1_0(4, "lui a0, 65536", a0);
   TESTINST_1_0(4, "lui a0, 131072", a0);
   TESTINST_1_0(4, "lui a0, 262144", a0);
   TESTINST_1_0(4, "lui a0, 524288", a0);
   TESTINST_1_0(4, "lui a0, 1048575", a0);

   TESTINST_1_0(4, "lui t6, 1", t6);
   TESTINST_1_0(4, "lui zero, 1", zero);

   /* ---------------- auipc rd, imm[31:12] ----------------- */
   TESTINST_1_0_AUIPC(4, "auipc a0, 0", a0);
   TESTINST_1_0_AUIPC(4, "auipc a0, 1", a0);
   TESTINST_1_0_AUIPC(4, "auipc a0, 2", a0);
   TESTINST_1_0_AUIPC(4, "auipc a0, 4", a0);
   TESTINST_1_0_AUIPC(4, "auipc a0, 8", a0);
   TESTINST_1_0_AUIPC(4, "auipc a0, 16", a0);
   TESTINST_1_0_AUIPC(4, "auipc a0, 32", a0);
   TESTINST_1_0_AUIPC(4, "auipc a0, 64", a0);
   TESTINST_1_0_AUIPC(4, "auipc a0, 128", a0);
   TESTINST_1_0_AUIPC(4, "auipc a0, 256", a0);
   TESTINST_1_0_AUIPC(4, "auipc a0, 512", a0);
   TESTINST_1_0_AUIPC(4, "auipc a0, 1024", a0);
   TESTINST_1_0_AUIPC(4, "auipc a0, 2048", a0);
   TESTINST_1_0_AUIPC(4, "auipc a0, 4096", a0);
   TESTINST_1_0_AUIPC(4, "auipc a0, 8192", a0);
   TESTINST_1_0_AUIPC(4, "auipc a0, 16384", a0);
   TESTINST_1_0_AUIPC(4, "auipc a0, 32768", a0);
   TESTINST_1_0_AUIPC(4, "auipc a0, 65536", a0);
   TESTINST_1_0_AUIPC(4, "auipc a0, 131072", a0);
   TESTINST_1_0_AUIPC(4, "auipc a0, 262144", a0);
   TESTINST_1_0_AUIPC(4, "auipc a0, 524288", a0);
   TESTINST_1_0_AUIPC(4, "auipc a0, 1048575", a0);

   TESTINST_1_0_AUIPC(4, "auipc t6, 1", t6);
   TESTINST_1_0_AUIPC(4, "auipc zero, 1", zero);

   /* ------------------ jal rd, imm[20:1] ------------------ */
   /* Note: Only the imm[11:1] range is tested. */
   TESTINST_1_0_JAL_RANGE(4, "jal t0, .+4", 4, t0);
   TESTINST_1_0_JAL_RANGE(4, "jal t0, .+6", 6, t0);
   TESTINST_1_0_JAL_RANGE(4, "jal t0, .+8", 8, t0);
   TESTINST_1_0_JAL_RANGE(4, "jal t0, .+16", 16, t0);
   TESTINST_1_0_JAL_RANGE(4, "jal t0, .+32", 32, t0);
   TESTINST_1_0_JAL_RANGE(4, "jal t0, .+64", 64, t0);
   TESTINST_1_0_JAL_RANGE(4, "jal t0, .+128", 128, t0);
   TESTINST_1_0_JAL_RANGE(4, "jal t0, .+256", 256, t0);
   TESTINST_1_0_JAL_RANGE(4, "jal t0, .+512", 512, t0);
   TESTINST_1_0_JAL_RANGE(4, "jal t0, .+1024", 1024, t0);
   TESTINST_1_0_JAL_RANGE(4, "jal t0, .+2048", 2048, t0);
   TESTINST_1_0_JAL_RANGE(4, "jal t0, .-4", -4, t0);
   TESTINST_1_0_JAL_RANGE(4, "jal t0, .-6", -6, t0);
   TESTINST_1_0_JAL_RANGE(4, "jal t0, .-2048", -2048, t0);

   TESTINST_1_0_JAL_RANGE(4, "jal t6, .+4", 4, t6);
   TESTINST_1_0_JAL_RANGE(4, "jal zero, .+4", 4, zero);

   /* --------------- jalr rd, imm[11:0](rs1) --------------- */
   TESTINST_1_1_JALR_RANGE(4, "jalr ra, 0(t0)", "1f+4", 4, ra, t0);
   TESTINST_1_1_JALR_RANGE(4, "jalr ra, 0(t0)", "1f+6", 6, ra, t0);
   TESTINST_1_1_JALR_RANGE(4, "jalr ra, 0(t0)", "1f+8", 8, ra, t0);
   TESTINST_1_1_JALR_RANGE(4, "jalr ra, 0(t0)", "1f-4", -4, ra, t0);
   TESTINST_1_1_JALR_RANGE(4, "jalr ra, 0(t0)", "1f-6", -6, ra, t0);
   TESTINST_1_1_JALR_RANGE(4, "jalr ra, 0(t0)", "1f-8", -8, ra, t0);

   TESTINST_1_1_JALR_RANGE(4, "jalr ra, 0(t0)", "1f-8", -8, ra, t0);
   TESTINST_1_1_JALR_RANGE(4, "jalr ra, 1(t0)", "1f-9", -8, ra, t0);
   TESTINST_1_1_JALR_RANGE(4, "jalr ra, 2(t0)", "1f-10", -8, ra, t0);
   TESTINST_1_1_JALR_RANGE(4, "jalr ra, 4(t0)", "1f-12", -8, ra, t0);
   TESTINST_1_1_JALR_RANGE(4, "jalr ra, 8(t0)", "1f-16", -8, ra, t0);
   TESTINST_1_1_JALR_RANGE(4, "jalr ra, 16(t0)", "1f-24", -8, ra, t0);
   TESTINST_1_1_JALR_RANGE(4, "jalr ra, 32(t0)", "1f-40", -8, ra, t0);
   TESTINST_1_1_JALR_RANGE(4, "jalr ra, 64(t0)", "1f-72", -8, ra, t0);
   TESTINST_1_1_JALR_RANGE(4, "jalr ra, 128(t0)", "1f-136", -8, ra, t0);
   TESTINST_1_1_JALR_RANGE(4, "jalr ra, 256(t0)", "1f-264", -8, ra, t0);
   TESTINST_1_1_JALR_RANGE(4, "jalr ra, 512(t0)", "1f-520", -8, ra, t0);
   TESTINST_1_1_JALR_RANGE(4, "jalr ra, 1024(t0)", "1f-1032", -8, ra, t0);
   TESTINST_1_1_JALR_RANGE(4, "jalr ra, 2047(t0)", "1f-2055", -8, ra, t0);
   TESTINST_1_1_JALR_RANGE(4, "jalr ra, -1(t0)", "1f-7", -8, ra, t0);
   TESTINST_1_1_JALR_RANGE(4, "jalr ra, -2048(t0)", "1f+2040", -8, ra, t0);

   TESTINST_1_1_JALR_RANGE(4, "jalr ra, 0(t6)", "1f+4", 4, ra, t6);
   TESTINST_1_1_JALR_RANGE(4, "jalr zero, 0(a0)", "1f+4", 4, zero, a0);

   /* --------------- beq rs1, rs2, imm[12:1] --------------- */
   /* Note: Only the imm[11:1] range is tested. */
   TESTINST_0_2_Bxx_RANGE(4, "beq a0, a1, .+4", 0, 0, 4, a0, a1);
   TESTINST_0_2_Bxx_RANGE(4, "beq a0, a1, .+6", 0, 0, 6, a0, a1);
   TESTINST_0_2_Bxx_RANGE(4, "beq a0, a1, .+8", 0, 0, 8, a0, a1);
   TESTINST_0_2_Bxx_RANGE(4, "beq a0, a1, .+16", 0, 0, 16, a0, a1);
   TESTINST_0_2_Bxx_RANGE(4, "beq a0, a1, .+32", 0, 0, 32, a0, a1);
   TESTINST_0_2_Bxx_RANGE(4, "beq a0, a1, .+64", 0, 0, 64, a0, a1);
   TESTINST_0_2_Bxx_RANGE(4, "beq a0, a1, .+128", 0, 0, 128, a0, a1);
   TESTINST_0_2_Bxx_RANGE(4, "beq a0, a1, .+256", 0, 0, 256, a0, a1);
   TESTINST_0_2_Bxx_RANGE(4, "beq a0, a1, .+512", 0, 0, 512, a0, a1);
   TESTINST_0_2_Bxx_RANGE(4, "beq a0, a1, .+1024", 0, 0, 1024, a0, a1);
   TESTINST_0_2_Bxx_RANGE(4, "beq a0, a1, .+2048", 0, 0, 2048, a0, a1);
   TESTINST_0_2_Bxx_RANGE(4, "beq a0, a1, .-4", 0, 0, -4, a0, a1);
   TESTINST_0_2_Bxx_RANGE(4, "beq a0, a1, .-6", 0, 0, -6, a0, a1);
   TESTINST_0_2_Bxx_RANGE(4, "beq a0, a1, .-2048", 0, 0, -2048, a0, a1);

   TESTINST_0_2_Bxx_RANGE(4, "beq t5, t6, .+4", 0, 0, 4, t5, t6);
   TESTINST_0_2_Bxx_COND(4, "beq a0, a1, 1f", 0, 0, a0, a1);
   TESTINST_0_2_Bxx_COND(4, "beq a0, a1, 1f", 0, 1, a0, a1);
   TESTINST_0_2_Bxx_COND(4, "beq a0, a1, 1f", 1, 0, a0, a1);
   TESTINST_0_2_Bxx_COND(4, "beq a0, a1, 1f", 1, 1, a0, a1);
   TESTINST_0_2_Bxx_COND(4, "beq a0, a1, 1f", 0, 0, a0, a1);
   TESTINST_0_2_Bxx_COND(4, "beq a0, a1, 1f", 1, 0, a0, a1);
   TESTINST_0_2_Bxx_COND(4, "beq a1, a0, 1f", 0, 0, a1, a0);
   TESTINST_0_2_Bxx_COND(4, "beq a1, a0, 1f", 0, 1, a1, a0);
   TESTINST_0_2_Bxx_COND(4, "beq a0, a1, 1f", 0, -1, a0, a1);
   TESTINST_0_2_Bxx_COND(4, "beq a0, a1, 1f", -1, 0, a0, a1);
   TESTINST_0_2_Bxx_COND(4, "beq a0, a1, 1f", -1, -1, a0, a1);

   /* --------------- bne rs1, rs2, imm[12:1] --------------- */
   TESTINST_0_2_Bxx_COND(4, "bne a0, a1, 1f", 0, 0, a0, a1);
   TESTINST_0_2_Bxx_COND(4, "bne a0, a1, 1f", 0, 1, a0, a1);
   TESTINST_0_2_Bxx_COND(4, "bne a0, a1, 1f", 1, 0, a0, a1);
   TESTINST_0_2_Bxx_COND(4, "bne a0, a1, 1f", 1, 1, a0, a1);
   TESTINST_0_2_Bxx_COND(4, "bne a0, a1, 1f", 0, 0, a0, a1);
   TESTINST_0_2_Bxx_COND(4, "bne a0, a1, 1f", 1, 0, a0, a1);
   TESTINST_0_2_Bxx_COND(4, "bne a1, a0, 1f", 0, 0, a1, a0);
   TESTINST_0_2_Bxx_COND(4, "bne a1, a0, 1f", 0, 1, a1, a0);
   TESTINST_0_2_Bxx_COND(4, "bne a0, a1, 1f", 0, -1, a0, a1);
   TESTINST_0_2_Bxx_COND(4, "bne a0, a1, 1f", -1, 0, a0, a1);
   TESTINST_0_2_Bxx_COND(4, "bne a0, a1, 1f", -1, -1, a0, a1);

   /* --------------- blt rs1, rs2, imm[12:1] --------------- */
   TESTINST_0_2_Bxx_COND(4, "blt a0, a1, 1f", 0, 0, a0, a1);
   TESTINST_0_2_Bxx_COND(4, "blt a0, a1, 1f", 0, 1, a0, a1);
   TESTINST_0_2_Bxx_COND(4, "blt a0, a1, 1f", 1, 0, a0, a1);
   TESTINST_0_2_Bxx_COND(4, "blt a0, a1, 1f", 1, 1, a0, a1);
   TESTINST_0_2_Bxx_COND(4, "blt a0, a1, 1f", 0, 0, a0, a1);
   TESTINST_0_2_Bxx_COND(4, "blt a0, a1, 1f", 1, 0, a0, a1);
   TESTINST_0_2_Bxx_COND(4, "blt a1, a0, 1f", 0, 0, a1, a0);
   TESTINST_0_2_Bxx_COND(4, "blt a1, a0, 1f", 0, 1, a1, a0);
   TESTINST_0_2_Bxx_COND(4, "blt a0, a1, 1f", 0, -1, a0, a1);
   TESTINST_0_2_Bxx_COND(4, "blt a0, a1, 1f", -1, 0, a0, a1);
   TESTINST_0_2_Bxx_COND(4, "blt a0, a1, 1f", -1, -1, a0, a1);

   /* --------------- bge rs1, rs2, imm[12:1] --------------- */
   TESTINST_0_2_Bxx_COND(4, "bge a0, a1, 1f", 0, 0, a0, a1);
   TESTINST_0_2_Bxx_COND(4, "bge a0, a1, 1f", 0, 1, a0, a1);
   TESTINST_0_2_Bxx_COND(4, "bge a0, a1, 1f", 1, 0, a0, a1);
   TESTINST_0_2_Bxx_COND(4, "bge a0, a1, 1f", 1, 1, a0, a1);
   TESTINST_0_2_Bxx_COND(4, "bge a0, a1, 1f", 0, 0, a0, a1);
   TESTINST_0_2_Bxx_COND(4, "bge a0, a1, 1f", 1, 0, a0, a1);
   TESTINST_0_2_Bxx_COND(4, "bge a1, a0, 1f", 0, 0, a1, a0);
   TESTINST_0_2_Bxx_COND(4, "bge a1, a0, 1f", 0, 1, a1, a0);
   TESTINST_0_2_Bxx_COND(4, "bge a0, a1, 1f", 0, -1, a0, a1);
   TESTINST_0_2_Bxx_COND(4, "bge a0, a1, 1f", -1, 0, a0, a1);
   TESTINST_0_2_Bxx_COND(4, "bge a0, a1, 1f", -1, -1, a0, a1);

   /* -------------- bltu rs1, rs2, imm[12:1] --------------- */
   TESTINST_0_2_Bxx_COND(4, "bltu a0, a1, 1f", 0, 0, a0, a1);
   TESTINST_0_2_Bxx_COND(4, "bltu a0, a1, 1f", 0, 1, a0, a1);
   TESTINST_0_2_Bxx_COND(4, "bltu a0, a1, 1f", 1, 0, a0, a1);
   TESTINST_0_2_Bxx_COND(4, "bltu a0, a1, 1f", 1, 1, a0, a1);
   TESTINST_0_2_Bxx_COND(4, "bltu a0, a1, 1f", 0, 0, a0, a1);
   TESTINST_0_2_Bxx_COND(4, "bltu a0, a1, 1f", 1, 0, a0, a1);
   TESTINST_0_2_Bxx_COND(4, "bltu a1, a0, 1f", 0, 0, a1, a0);
   TESTINST_0_2_Bxx_COND(4, "bltu a1, a0, 1f", 0, 1, a1, a0);
   TESTINST_0_2_Bxx_COND(4, "bltu a0, a1, 1f", 0, -1, a0, a1);
   TESTINST_0_2_Bxx_COND(4, "bltu a0, a1, 1f", -1, 0, a0, a1);
   TESTINST_0_2_Bxx_COND(4, "bltu a0, a1, 1f", -1, -1, a0, a1);

   /* -------------- bgeu rs1, rs2, imm[12:1] --------------- */
   TESTINST_0_2_Bxx_COND(4, "bgeu a0, a1, 1f", 0, 0, a0, a1);
   TESTINST_0_2_Bxx_COND(4, "bgeu a0, a1, 1f", 0, 1, a0, a1);
   TESTINST_0_2_Bxx_COND(4, "bgeu a0, a1, 1f", 1, 0, a0, a1);
   TESTINST_0_2_Bxx_COND(4, "bgeu a0, a1, 1f", 1, 1, a0, a1);
   TESTINST_0_2_Bxx_COND(4, "bgeu a0, a1, 1f", 0, 0, a0, a1);
   TESTINST_0_2_Bxx_COND(4, "bgeu a0, a1, 1f", 1, 0, a0, a1);
   TESTINST_0_2_Bxx_COND(4, "bgeu a1, a0, 1f", 0, 0, a1, a0);
   TESTINST_0_2_Bxx_COND(4, "bgeu a1, a0, 1f", 0, 1, a1, a0);
   TESTINST_0_2_Bxx_COND(4, "bgeu a0, a1, 1f", 0, -1, a0, a1);
   TESTINST_0_2_Bxx_COND(4, "bgeu a0, a1, 1f", -1, 0, a0, a1);
   TESTINST_0_2_Bxx_COND(4, "bgeu a0, a1, 1f", -1, -1, a0, a1);

   /* ---------------- lb rd, imm[11:0](rs1) ---------------- */
   TESTINST_1_1_LOAD(4, "lb a0, 0(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lb a0, 1(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lb a0, 2(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lb a0, 4(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lb a0, 8(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lb a0, 16(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lb a0, 32(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lb a0, 64(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lb a0, 128(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lb a0, 256(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lb a0, 512(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lb a0, 1024(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lb a0, 2047(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lb a0, -1(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lb a0, -2048(a1)", a0, a1);

   TESTINST_1_1_LOAD(4, "lb a4, 0(a5)", a4, a5);
   TESTINST_1_1_LOAD(4, "lb zero, 0(a0)", zero, a0);

   /* ---------------- lh rd, imm[11:0](rs1) ---------------- */
   TESTINST_1_1_LOAD(4, "lh a0, 0(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lh a0, 2(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lh a0, 4(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lh a0, 8(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lh a0, 16(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lh a0, 32(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lh a0, 64(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lh a0, 128(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lh a0, 256(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lh a0, 512(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lh a0, 1024(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lh a0, 2046(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lh a0, -2(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lh a0, -2048(a1)", a0, a1);

   TESTINST_1_1_LOAD(4, "lh a4, 0(a5)", a4, a5);
   TESTINST_1_1_LOAD(4, "lh zero, 0(a0)", zero, a0);

   /* ---------------- lw rd, imm[11:0](rs1) ---------------- */
   TESTINST_1_1_LOAD(4, "lw a0, 0(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lw a0, 4(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lw a0, 8(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lw a0, 16(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lw a0, 32(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lw a0, 64(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lw a0, 128(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lw a0, 256(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lw a0, 512(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lw a0, 1024(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lw a0, 2044(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lw a0, -4(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lw a0, -2048(a1)", a0, a1);

   TESTINST_1_1_LOAD(4, "lw a4, 0(a5)", a4, a5);
   TESTINST_1_1_LOAD(4, "lw zero, 0(a0)", zero, a0);

   /* --------------- lbu rd, imm[11:0](rs1) ---------------- */
   TESTINST_1_1_LOAD(4, "lbu a0, 0(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lbu a0, 1(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lbu a0, 2(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lbu a0, 4(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lbu a0, 8(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lbu a0, 16(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lbu a0, 32(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lbu a0, 64(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lbu a0, 128(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lbu a0, 256(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lbu a0, 512(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lbu a0, 1024(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lbu a0, 2047(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lbu a0, -1(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lbu a0, -2048(a1)", a0, a1);

   TESTINST_1_1_LOAD(4, "lbu a4, 0(a5)", a4, a5);
   TESTINST_1_1_LOAD(4, "lbu zero, 0(a0)", zero, a0);

   /* --------------- lhu rd, imm[11:0](rs1) ---------------- */
   TESTINST_1_1_LOAD(4, "lhu a0, 0(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lhu a0, 2(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lhu a0, 4(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lhu a0, 8(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lhu a0, 16(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lhu a0, 32(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lhu a0, 64(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lhu a0, 128(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lhu a0, 256(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lhu a0, 512(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lhu a0, 1024(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lhu a0, 2046(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lhu a0, -2(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lhu a0, -2048(a1)", a0, a1);

   TESTINST_1_1_LOAD(4, "lhu a4, 0(a5)", a4, a5);
   TESTINST_1_1_LOAD(4, "lhu zero, 0(a0)", zero, a0);

   /* --------------- sb rs2, imm[11:0](rs1) ---------------- */
   TESTINST_0_2_STORE(4, "sb a0, 0(a1)", 0xabcdef0123456789, a0, a1);
   TESTINST_0_2_STORE(4, "sb a0, 1(a1)", 0xabcdef0123456789, a0, a1);
   TESTINST_0_2_STORE(4, "sb a0, 2(a1)", 0xabcdef0123456789, a0, a1);
   TESTINST_0_2_STORE(4, "sb a0, 4(a1)", 0xabcdef0123456789, a0, a1);
   TESTINST_0_2_STORE(4, "sb a0, 8(a1)", 0xabcdef0123456789, a0, a1);
   TESTINST_0_2_STORE(4, "sb a0, 16(a1)", 0xabcdef0123456789, a0, a1);
   TESTINST_0_2_STORE(4, "sb a0, 32(a1)", 0xabcdef0123456789, a0, a1);
   TESTINST_0_2_STORE(4, "sb a0, 64(a1)", 0xabcdef0123456789, a0, a1);
   TESTINST_0_2_STORE(4, "sb a0, 128(a1)", 0xabcdef0123456789, a0, a1);
   TESTINST_0_2_STORE(4, "sb a0, 256(a1)", 0xabcdef0123456789, a0, a1);
   TESTINST_0_2_STORE(4, "sb a0, 512(a1)", 0xabcdef0123456789, a0, a1);
   TESTINST_0_2_STORE(4, "sb a0, 1024(a1)", 0xabcdef0123456789, a0, a1);
   TESTINST_0_2_STORE(4, "sb a0, 2047(a1)", 0xabcdef0123456789, a0, a1);
   TESTINST_0_2_STORE(4, "sb a0, -1(a1)", 0xabcdef0123456789, a0, a1);
   TESTINST_0_2_STORE(4, "sb a0, -2048(a1)", 0xabcdef0123456789, a0, a1);

   TESTINST_0_2_STORE(4, "sb a4, 0(a5)", 0xabcdef0123456789, a4, a5);

   /* --------------- sh rs2, imm[11:0](rs1) ---------------- */
   TESTINST_0_2_STORE(4, "sh a0, 0(a1)", 0xabcdef0123456789, a0, a1);
   TESTINST_0_2_STORE(4, "sh a0, 2(a1)", 0xabcdef0123456789, a0, a1);
   TESTINST_0_2_STORE(4, "sh a0, 4(a1)", 0xabcdef0123456789, a0, a1);
   TESTINST_0_2_STORE(4, "sh a0, 8(a1)", 0xabcdef0123456789, a0, a1);
   TESTINST_0_2_STORE(4, "sh a0, 16(a1)", 0xabcdef0123456789, a0, a1);
   TESTINST_0_2_STORE(4, "sh a0, 32(a1)", 0xabcdef0123456789, a0, a1);
   TESTINST_0_2_STORE(4, "sh a0, 64(a1)", 0xabcdef0123456789, a0, a1);
   TESTINST_0_2_STORE(4, "sh a0, 128(a1)", 0xabcdef0123456789, a0, a1);
   TESTINST_0_2_STORE(4, "sh a0, 256(a1)", 0xabcdef0123456789, a0, a1);
   TESTINST_0_2_STORE(4, "sh a0, 512(a1)", 0xabcdef0123456789, a0, a1);
   TESTINST_0_2_STORE(4, "sh a0, 1024(a1)", 0xabcdef0123456789, a0, a1);
   TESTINST_0_2_STORE(4, "sh a0, 2046(a1)", 0xabcdef0123456789, a0, a1);
   TESTINST_0_2_STORE(4, "sh a0, -2(a1)", 0xabcdef0123456789, a0, a1);
   TESTINST_0_2_STORE(4, "sh a0, -2048(a1)", 0xabcdef0123456789, a0, a1);

   TESTINST_0_2_STORE(4, "sh a4, 0(a5)", 0xabcdef0123456789, a4, a5);

   /* --------------- sw rs2, imm[11:0](rs1) ---------------- */
   TESTINST_0_2_STORE(4, "sw a0, 0(a1)", 0xabcdef0123456789, a0, a1);
   TESTINST_0_2_STORE(4, "sw a0, 4(a1)", 0xabcdef0123456789, a0, a1);
   TESTINST_0_2_STORE(4, "sw a0, 8(a1)", 0xabcdef0123456789, a0, a1);
   TESTINST_0_2_STORE(4, "sw a0, 16(a1)", 0xabcdef0123456789, a0, a1);
   TESTINST_0_2_STORE(4, "sw a0, 32(a1)", 0xabcdef0123456789, a0, a1);
   TESTINST_0_2_STORE(4, "sw a0, 64(a1)", 0xabcdef0123456789, a0, a1);
   TESTINST_0_2_STORE(4, "sw a0, 128(a1)", 0xabcdef0123456789, a0, a1);
   TESTINST_0_2_STORE(4, "sw a0, 256(a1)", 0xabcdef0123456789, a0, a1);
   TESTINST_0_2_STORE(4, "sw a0, 512(a1)", 0xabcdef0123456789, a0, a1);
   TESTINST_0_2_STORE(4, "sw a0, 1024(a1)", 0xabcdef0123456789, a0, a1);
   TESTINST_0_2_STORE(4, "sw a0, 2044(a1)", 0xabcdef0123456789, a0, a1);
   TESTINST_0_2_STORE(4, "sw a0, -4(a1)", 0xabcdef0123456789, a0, a1);
   TESTINST_0_2_STORE(4, "sw a0, -2048(a1)", 0xabcdef0123456789, a0, a1);

   TESTINST_0_2_STORE(4, "sw a4, 0(a5)", 0xabcdef0123456789, a4, a5);

   /* --------------- addi rd, rs1, imm[11:0] --------------- */
   TESTINST_1_1(4, "addi a0, a1, 1", 0x0000000000001000, a0, a1);
   TESTINST_1_1(4, "addi a0, a1, 2", 0x0000000000001000, a0, a1);
   TESTINST_1_1(4, "addi a0, a1, 4", 0x0000000000001000, a0, a1);
   TESTINST_1_1(4, "addi a0, a1, 8", 0x0000000000001000, a0, a1);
   TESTINST_1_1(4, "addi a0, a1, 16", 0x0000000000001000, a0, a1);
   TESTINST_1_1(4, "addi a0, a1, 32", 0x0000000000001000, a0, a1);
   TESTINST_1_1(4, "addi a0, a1, 64", 0x0000000000001000, a0, a1);
   TESTINST_1_1(4, "addi a0, a1, 128", 0x0000000000001000, a0, a1);
   TESTINST_1_1(4, "addi a0, a1, 256", 0x0000000000001000, a0, a1);
   TESTINST_1_1(4, "addi a0, a1, 1024", 0x0000000000001000, a0, a1);
   TESTINST_1_1(4, "addi a0, a1, 2047", 0x0000000000001000, a0, a1);
   TESTINST_1_1(4, "addi a0, a1, -1", 0x0000000000001000, a0, a1);
   TESTINST_1_1(4, "addi a0, a1, -2048", 0x0000000000001000, a0, a1);

   TESTINST_1_1(4, "addi a0, a1, 1", 0x000000007fffffff, a0, a1);
   TESTINST_1_1(4, "addi a0, a1, 1", 0x00000000fffffffe, a0, a1);
   TESTINST_1_1(4, "addi a0, a1, 1", 0x00000000ffffffff, a0, a1);
   TESTINST_1_1(4, "addi t5, t6, 1", 0x0000000000001000, t5, t6);
   TESTINST_1_1(4, "addi zero, a0, 1", 0x0000000000001000, zero, a0);

   /* --------------- slti rd, rs1, imm[11:0] --------------- */
   TESTINST_1_1(4, "slti a0, a1, 0", 0x0000000000000000, a0, a1);
   TESTINST_1_1(4, "slti a0, a1, 0", 0x0000000000000001, a0, a1);
   TESTINST_1_1(4, "slti a0, a1, 0", 0xffffffffffffffff, a0, a1);
   TESTINST_1_1(4, "slti a0, a1, 0x7ff", 0x00000000000007ff, a0, a1);
   TESTINST_1_1(4, "slti a0, a1, 0x7ff", 0x0000000000000800, a0, a1);
   TESTINST_1_1(4, "slti a0, a1, 0xffffffffffffffff", 0xffffffffffffffff, a0,
                a1);
   TESTINST_1_1(4, "slti a0, a1, 0xffffffffffffffff", 0x0000000000000000, a0,
                a1);

   TESTINST_1_1(4, "slti t5, t6, 0", 0x0000000000000000, t5, t6);
   TESTINST_1_1(4, "slti t5, t6, 0", 0x0000000000000001, t5, t6);
   TESTINST_1_1(4, "slti zero, a0, 1", 0x0000000000000000, zero, a0);

   /* -------------- sltiu rd, rs1, imm[11:0] --------------- */
   TESTINST_1_1(4, "sltiu a0, a1, 0", 0x0000000000000000, a0, a1);
   TESTINST_1_1(4, "sltiu a0, a1, 0", 0x0000000000000001, a0, a1);
   TESTINST_1_1(4, "sltiu a0, a1, 0", 0xffffffffffffffff, a0, a1);
   TESTINST_1_1(4, "sltiu a0, a1, 0x7ff", 0x00000000000007ff, a0, a1);
   TESTINST_1_1(4, "sltiu a0, a1, 0x7ff", 0x0000000000000800, a0, a1);
   TESTINST_1_1(4, "sltiu a0, a1, 0xffffffffffffffff", 0xffffffffffffffff, a0,
                a1);
   TESTINST_1_1(4, "sltiu a0, a1, 0xffffffffffffffff", 0x0000000000000000, a0,
                a1);

   TESTINST_1_1(4, "sltiu t5, t6, 0", 0x0000000000000000, t5, t6);
   TESTINST_1_1(4, "sltiu t5, t6, 0", 0x0000000000000001, t5, t6);
   TESTINST_1_1(4, "sltiu zero, a0, 1", 0x0000000000000000, zero, a0);

   /* --------------- xori rd, rs1, imm[11:0] --------------- */
   TESTINST_1_1(4, "xori a0, a1, 0", 0x0000ffff0000ffff, a0, a1);
   TESTINST_1_1(4, "xori a0, a1, 0", 0xffff0000ffff0000, a0, a1);
   TESTINST_1_1(4, "xori a0, a1, 0x7ff", 0x0000ffff0000ffff, a0, a1);
   TESTINST_1_1(4, "xori a0, a1, 0x7ff", 0xffff0000ffff0000, a0, a1);
   TESTINST_1_1(4, "xori a0, a1, 0xffffffffffffffff", 0x0000ffff0000ffff, a0,
                a1);
   TESTINST_1_1(4, "xori a0, a1, 0xffffffffffffffff", 0xffff0000ffff0000, a0,
                a1);

   TESTINST_1_1(4, "xori t5, t6, 0", 0x0000ffff0000ffff, t5, t6);
   TESTINST_1_1(4, "xori zero, a0, 0x7ff", 0x0000ffff0000ffff, zero, a0);

   /* --------------- ori rd, rs1, imm[11:0] ---------------- */
   TESTINST_1_1(4, "ori a0, a1, 0", 0x0000ffff0000ffff, a0, a1);
   TESTINST_1_1(4, "ori a0, a1, 0", 0xffff0000ffff0000, a0, a1);
   TESTINST_1_1(4, "ori a0, a1, 0x7ff", 0x0000ffff0000ffff, a0, a1);
   TESTINST_1_1(4, "ori a0, a1, 0x7ff", 0xffff0000ffff0000, a0, a1);
   TESTINST_1_1(4, "ori a0, a1, 0xffffffffffffffff", 0x0000ffff0000ffff, a0,
                a1);
   TESTINST_1_1(4, "ori a0, a1, 0xffffffffffffffff", 0xffff0000ffff0000, a0,
                a1);

   TESTINST_1_1(4, "ori t5, t6, 0", 0x0000ffff0000ffff, t5, t6);
   TESTINST_1_1(4, "ori zero, a0, 0x7ff", 0x0000ffff0000ffff, zero, a0);

   /* --------------- andi rd, rs1, imm[11:0] --------------- */
   TESTINST_1_1(4, "andi a0, a1, 0", 0x0000ffff0000ffff, a0, a1);
   TESTINST_1_1(4, "andi a0, a1, 0", 0xffff0000ffff0000, a0, a1);
   TESTINST_1_1(4, "andi a0, a1, 0x7ff", 0x0000ffff0000ffff, a0, a1);
   TESTINST_1_1(4, "andi a0, a1, 0x7ff", 0xffff0000ffff0000, a0, a1);
   TESTINST_1_1(4, "andi a0, a1, 0xffffffffffffffff", 0x0000ffff0000ffff, a0,
                a1);
   TESTINST_1_1(4, "andi a0, a1, 0xffffffffffffffff", 0xffff0000ffff0000, a0,
                a1);

   TESTINST_1_1(4, "andi t5, t6, 0", 0x0000ffff0000ffff, t5, t6);
   TESTINST_1_1(4, "andi zero, a0, 0x7ff", 0x0000ffff0000ffff, zero, a0);

   /* --------------- slli rd, rs1, uimm[5:0] --------------- */
   TESTINST_1_1(4, "slli a0, a1, 0", 0xabcdef0123456789, a0, a1);
   TESTINST_1_1(4, "slli a0, a1, 1", 0xabcdef0123456789, a0, a1);
   TESTINST_1_1(4, "slli a0, a1, 2", 0xabcdef0123456789, a0, a1);
   TESTINST_1_1(4, "slli a0, a1, 4", 0xabcdef0123456789, a0, a1);
   TESTINST_1_1(4, "slli a0, a1, 8", 0xabcdef0123456789, a0, a1);
   TESTINST_1_1(4, "slli a0, a1, 16", 0xabcdef0123456789, a0, a1);
   TESTINST_1_1(4, "slli a0, a1, 32", 0xabcdef0123456789, a0, a1);
   TESTINST_1_1(4, "slli a0, a1, 63", 0xabcdef0123456789, a0, a1);

   TESTINST_1_1(4, "slli t5, t6, 1", 0xabcdef0123456789, t5, t6);
   TESTINST_1_1(4, "slli zero, a0, 1", 0xabcdef0123456789, zero, a0);

   /* --------------- srli rd, rs1, uimm[5:0] --------------- */
   TESTINST_1_1(4, "srli a0, a1, 0", 0xabcdef0123456789, a0, a1);
   TESTINST_1_1(4, "srli a0, a1, 1", 0xabcdef0123456789, a0, a1);
   TESTINST_1_1(4, "srli a0, a1, 2", 0xabcdef0123456789, a0, a1);
   TESTINST_1_1(4, "srli a0, a1, 4", 0xabcdef0123456789, a0, a1);
   TESTINST_1_1(4, "srli a0, a1, 8", 0xabcdef0123456789, a0, a1);
   TESTINST_1_1(4, "srli a0, a1, 16", 0xabcdef0123456789, a0, a1);
   TESTINST_1_1(4, "srli a0, a1, 32", 0xabcdef0123456789, a0, a1);
   TESTINST_1_1(4, "srli a0, a1, 63", 0xabcdef0123456789, a0, a1);

   TESTINST_1_1(4, "srli t5, t6, 1", 0xabcdef0123456789, t5, t6);
   TESTINST_1_1(4, "srli zero, a0, 1", 0xabcdef0123456789, zero, a0);

   /* --------------- srai rd, rs1, uimm[5:0] --------------- */
   TESTINST_1_1(4, "srai a0, a1, 0", 0xabcdef0123456789, a0, a1);
   TESTINST_1_1(4, "srai a0, a1, 1", 0xabcdef0123456789, a0, a1);
   TESTINST_1_1(4, "srai a0, a1, 2", 0xabcdef0123456789, a0, a1);
   TESTINST_1_1(4, "srai a0, a1, 4", 0xabcdef0123456789, a0, a1);
   TESTINST_1_1(4, "srai a0, a1, 8", 0xabcdef0123456789, a0, a1);
   TESTINST_1_1(4, "srai a0, a1, 16", 0xabcdef0123456789, a0, a1);
   TESTINST_1_1(4, "srai a0, a1, 32", 0xabcdef0123456789, a0, a1);
   TESTINST_1_1(4, "srai a0, a1, 63", 0xabcdef0123456789, a0, a1);

   TESTINST_1_1(4, "srai t5, t6, 1", 0xabcdef0123456789, t5, t6);
   TESTINST_1_1(4, "srai zero, a0, 1", 0xabcdef0123456789, zero, a0);

   /* ------------------ add rd, rs1, rs2 ------------------- */
   TESTINST_1_2(4, "add a0, a1, a2", 0x0000000000001000, 0x0000000000002000, a0,
                a1, a2);
   TESTINST_1_2(4, "add a0, a1, a2", 0x000000007fffffff, 0x0000000000000001, a0,
                a1, a2);
   TESTINST_1_2(4, "add a0, a1, a2", 0x00000000fffffffe, 0x0000000000000001, a0,
                a1, a2);
   TESTINST_1_2(4, "add a0, a1, a2", 0x00000000ffffffff, 0x0000000000000001, a0,
                a1, a2);
   TESTINST_1_2(4, "add a0, a1, a2", 0xfffffffffffffffe, 0x0000000000000001, a0,
                a1, a2);
   TESTINST_1_2(4, "add a0, a1, a2", 0xffffffffffffffff, 0x0000000000000001, a0,
                a1, a2);

   TESTINST_1_2(4, "add t4, t5, t6", 0x0000000000001000, 0x0000000000002000, t4,
                t5, t6);
   TESTINST_1_2(4, "add zero, a0, a1", 0x0000000000001000, 0x0000000000002000,
                zero, a0, a1);

   /* ------------------ sub rd, rs1, rs2 ------------------- */
   TESTINST_1_2(4, "sub a0, a1, a2", 0x0000000000001000, 0x0000000000000fff, a0,
                a1, a2);
   TESTINST_1_2(4, "sub a0, a1, a2", 0x0000000000001000, 0x0000000000001000, a0,
                a1, a2);
   TESTINST_1_2(4, "sub a0, a1, a2", 0x0000000000001000, 0x0000000000001001, a0,
                a1, a2);
   TESTINST_1_2(4, "sub a0, a1, a2", 0xffffffffffffffff, 0x0000000000000000, a0,
                a1, a2);
   TESTINST_1_2(4, "sub a0, a1, a2", 0x0000000100000000, 0x0000000000000001, a0,
                a1, a2);

   TESTINST_1_2(4, "sub t4, t5, t6", 0x0000000000001000, 0x0000000000000fff, t4,
                t5, t6);
   TESTINST_1_2(4, "sub zero, a0, a1", 0x0000000000001000, 0x0000000000000fff,
                zero, a0, a1);

   /* ------------------ sll rd, rs1, rs2 ------------------- */
   TESTINST_1_2(4, "sll a0, a1, a2", 0xabcdef0123456789, 0, a0, a1, a2);
   TESTINST_1_2(4, "sll a0, a1, a2", 0xabcdef0123456789, 1, a0, a1, a2);
   TESTINST_1_2(4, "sll a0, a1, a2", 0xabcdef0123456789, 2, a0, a1, a2);
   TESTINST_1_2(4, "sll a0, a1, a2", 0xabcdef0123456789, 4, a0, a1, a2);
   TESTINST_1_2(4, "sll a0, a1, a2", 0xabcdef0123456789, 8, a0, a1, a2);
   TESTINST_1_2(4, "sll a0, a1, a2", 0xabcdef0123456789, 16, a0, a1, a2);
   TESTINST_1_2(4, "sll a0, a1, a2", 0xabcdef0123456789, 32, a0, a1, a2);
   TESTINST_1_2(4, "sll a0, a1, a2", 0xabcdef0123456789, 63, a0, a1, a2);
   TESTINST_1_2(4, "sll a0, a1, a2", 0xabcdef0123456789, 64, a0, a1, a2);

   TESTINST_1_2(4, "sll t4, t5, t6", 0xabcdef0123456789, 1, t4, t5, t6);
   TESTINST_1_2(4, "sll zero, a0, a1", 0xabcdef0123456789, 1, zero, a0, a1);

   /* ------------------ slt rd, rs1, rs2 ------------------- */
   TESTINST_1_2(4, "slt a0, a1, a2", 0x0000000000000000, 0x0000000000000000, a0,
                a1, a2);
   TESTINST_1_2(4, "slt a0, a1, a2", 0x0000000000000000, 0x0000000000000001, a0,
                a1, a2);
   TESTINST_1_2(4, "slt a0, a1, a2", 0x0000000000000000, 0xffffffffffffffff, a0,
                a1, a2);

   TESTINST_1_2(4, "slt t4, t5, t6", 0x0000000000000000, 0x0000000000000000, t4,
                t5, t6);
   TESTINST_1_2(4, "slt t4, t5, t6", 0x0000000000000000, 0x0000000000000001, t4,
                t5, t6);
   TESTINST_1_2(4, "slt zero, a0, a1", 0x0000000000000000, 0x0000000000000001,
                zero, a0, a1);

   /* ------------------ sltu rd, rs1, rs2 ------------------ */
   TESTINST_1_2(4, "sltu a0, a1, a2", 0x0000000000000000, 0x0000000000000000,
                a0, a1, a2);
   TESTINST_1_2(4, "sltu a0, a1, a2", 0x0000000000000000, 0x0000000000000001,
                a0, a1, a2);
   TESTINST_1_2(4, "sltu a0, a1, a2", 0x0000000000000000, 0xffffffffffffffff,
                a0, a1, a2);

   TESTINST_1_2(4, "sltu t4, t5, t6", 0x0000000000000000, 0x0000000000000000,
                t4, t5, t6);
   TESTINST_1_2(4, "sltu t4, t5, t6", 0x0000000000000000, 0x0000000000000001,
                t4, t5, t6);
   TESTINST_1_2(4, "sltu zero, a0, a1", 0x0000000000000000, 0x0000000000000001,
                zero, a0, a1);

   /* ------------------ xor rd, rs1, rs2 ------------------- */
   TESTINST_1_2(4, "xor a0, a1, a2", 0x0000ffff0000ffff, 0x00000000ffffffff, a0,
                a1, a2);
   TESTINST_1_2(4, "xor t4, t5, t6", 0x0000ffff0000ffff, 0x00000000ffffffff, t4,
                t5, t6);
   TESTINST_1_2(4, "xor zero, a0, a1", 0x0000ffff0000ffff, 0x00000000ffffffff,
                zero, a0, a1);

   /* ------------------ srl rd, rs1, rs2 ------------------- */
   TESTINST_1_2(4, "srl a0, a1, a2", 0xabcdef0123456789, 0, a0, a1, a2);
   TESTINST_1_2(4, "srl a0, a1, a2", 0xabcdef0123456789, 1, a0, a1, a2);
   TESTINST_1_2(4, "srl a0, a1, a2", 0xabcdef0123456789, 2, a0, a1, a2);
   TESTINST_1_2(4, "srl a0, a1, a2", 0xabcdef0123456789, 4, a0, a1, a2);
   TESTINST_1_2(4, "srl a0, a1, a2", 0xabcdef0123456789, 8, a0, a1, a2);
   TESTINST_1_2(4, "srl a0, a1, a2", 0xabcdef0123456789, 16, a0, a1, a2);
   TESTINST_1_2(4, "srl a0, a1, a2", 0xabcdef0123456789, 32, a0, a1, a2);
   TESTINST_1_2(4, "srl a0, a1, a2", 0xabcdef0123456789, 63, a0, a1, a2);
   TESTINST_1_2(4, "srl a0, a1, a2", 0xabcdef0123456789, 64, a0, a1, a2);

   TESTINST_1_2(4, "srl t4, t5, t6", 0xabcdef0123456789, 1, t4, t5, t6);
   TESTINST_1_2(4, "srl zero, a0, a1", 0xabcdef0123456789, 1, zero, a0, a1);

   /* ------------------ sra rd, rs1, rs2 ------------------- */
   TESTINST_1_2(4, "sra a0, a1, a2", 0xabcdef0123456789, 0, a0, a1, a2);
   TESTINST_1_2(4, "sra a0, a1, a2", 0xabcdef0123456789, 1, a0, a1, a2);
   TESTINST_1_2(4, "sra a0, a1, a2", 0xabcdef0123456789, 2, a0, a1, a2);
   TESTINST_1_2(4, "sra a0, a1, a2", 0xabcdef0123456789, 4, a0, a1, a2);
   TESTINST_1_2(4, "sra a0, a1, a2", 0xabcdef0123456789, 8, a0, a1, a2);
   TESTINST_1_2(4, "sra a0, a1, a2", 0xabcdef0123456789, 16, a0, a1, a2);
   TESTINST_1_2(4, "sra a0, a1, a2", 0xabcdef0123456789, 32, a0, a1, a2);
   TESTINST_1_2(4, "sra a0, a1, a2", 0xabcdef0123456789, 63, a0, a1, a2);
   TESTINST_1_2(4, "sra a0, a1, a2", 0xabcdef0123456789, 64, a0, a1, a2);

   TESTINST_1_2(4, "sra t4, t5, t6", 0xabcdef0123456789, 1, t4, t5, t6);
   TESTINST_1_2(4, "sra zero, a0, a1", 0xabcdef0123456789, 1, zero, a0, a1);

   /* ------------------- or rd, rs1, rs2 ------------------- */
   TESTINST_1_2(4, "or a0, a1, a2", 0x0000ffff0000ffff, 0x00000000ffffffff, a0,
                a1, a2);
   TESTINST_1_2(4, "or t4, t5, t6", 0x0000ffff0000ffff, 0x00000000ffffffff, t4,
                t5, t6);
   TESTINST_1_2(4, "or zero, a0, a1", 0x0000ffff0000ffff, 0x00000000ffffffff,
                zero, a0, a1);

   /* ------------------ and rd, rs1, rs2 ------------------- */
   TESTINST_1_2(4, "and a0, a1, a2", 0x0000ffff0000ffff, 0x00000000ffffffff, a0,
                a1, a2);
   TESTINST_1_2(4, "and t4, t5, t6", 0x0000ffff0000ffff, 0x00000000ffffffff, t4,
                t5, t6);
   TESTINST_1_2(4, "and zero, a0, a1", 0x0000ffff0000ffff, 0x00000000ffffffff,
                zero, a0, a1);

   /* ------------------------ fence ------------------------ */
   TESTINST_0_0(4, "fence");

   /* ------------------------ ecall ------------------------ */
   /* Not tested here. */

   /* ----------------------- ebreak ------------------------ */
   /* Not tested here. */

   printf("\n");
}

static void test_integer_additions(void)
{
   printf("RV64I base instruction set, additions\n");

   /* --------------- lwu rd, imm[11:0](rs1) ---------------- */
   TESTINST_1_1_LOAD(4, "lwu a0, 0(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lwu a0, 4(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lwu a0, 8(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lwu a0, 16(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lwu a0, 32(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lwu a0, 64(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lwu a0, 128(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lwu a0, 256(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lwu a0, 512(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lwu a0, 1024(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lwu a0, 2044(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lwu a0, -4(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "lwu a0, -2048(a1)", a0, a1);

   TESTINST_1_1_LOAD(4, "lwu a4, 0(a5)", a4, a5);
   TESTINST_1_1_LOAD(4, "lwu zero, 0(a0)", zero, a0);

   /* ---------------- ld rd, imm[11:0](rs1) ---------------- */
   TESTINST_1_1_LOAD(4, "ld a0, 0(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "ld a0, 4(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "ld a0, 8(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "ld a0, 16(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "ld a0, 32(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "ld a0, 64(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "ld a0, 128(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "ld a0, 256(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "ld a0, 512(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "ld a0, 1024(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "ld a0, 2040(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "ld a0, -4(a1)", a0, a1);
   TESTINST_1_1_LOAD(4, "ld a0, -2048(a1)", a0, a1);

   TESTINST_1_1_LOAD(4, "ld a4, 0(a5)", a4, a5);
   TESTINST_1_1_LOAD(4, "ld zero, 0(a0)", zero, a0);

   /* --------------- sd rs2, imm[11:0](rs1) ---------------- */
   TESTINST_0_2_STORE(4, "sd a0, 0(a1)", 0xabcdef0123456789, a0, a1);
   TESTINST_0_2_STORE(4, "sd a0, 4(a1)", 0xabcdef0123456789, a0, a1);
   TESTINST_0_2_STORE(4, "sd a0, 8(a1)", 0xabcdef0123456789, a0, a1);
   TESTINST_0_2_STORE(4, "sd a0, 16(a1)", 0xabcdef0123456789, a0, a1);
   TESTINST_0_2_STORE(4, "sd a0, 32(a1)", 0xabcdef0123456789, a0, a1);
   TESTINST_0_2_STORE(4, "sd a0, 64(a1)", 0xabcdef0123456789, a0, a1);
   TESTINST_0_2_STORE(4, "sd a0, 128(a1)", 0xabcdef0123456789, a0, a1);
   TESTINST_0_2_STORE(4, "sd a0, 256(a1)", 0xabcdef0123456789, a0, a1);
   TESTINST_0_2_STORE(4, "sd a0, 512(a1)", 0xabcdef0123456789, a0, a1);
   TESTINST_0_2_STORE(4, "sd a0, 1024(a1)", 0xabcdef0123456789, a0, a1);
   TESTINST_0_2_STORE(4, "sd a0, 2040(a1)", 0xabcdef0123456789, a0, a1);
   TESTINST_0_2_STORE(4, "sd a0, -4(a1)", 0xabcdef0123456789, a0, a1);
   TESTINST_0_2_STORE(4, "sd a0, -2048(a1)", 0xabcdef0123456789, a0, a1);

   TESTINST_0_2_STORE(4, "sd a4, 0(a5)", 0xabcdef0123456789, a4, a5);

   /* -------------- addiw rd, rs1, imm[11:0] --------------- */
   TESTINST_1_1(4, "addiw a0, a1, 1", 0x0000000000001000, a0, a1);
   TESTINST_1_1(4, "addiw a0, a1, 2", 0x0000000000001000, a0, a1);
   TESTINST_1_1(4, "addiw a0, a1, 4", 0x0000000000001000, a0, a1);
   TESTINST_1_1(4, "addiw a0, a1, 8", 0x0000000000001000, a0, a1);
   TESTINST_1_1(4, "addiw a0, a1, 16", 0x0000000000001000, a0, a1);
   TESTINST_1_1(4, "addiw a0, a1, 32", 0x0000000000001000, a0, a1);
   TESTINST_1_1(4, "addiw a0, a1, 64", 0x0000000000001000, a0, a1);
   TESTINST_1_1(4, "addiw a0, a1, 128", 0x0000000000001000, a0, a1);
   TESTINST_1_1(4, "addiw a0, a1, 256", 0x0000000000001000, a0, a1);
   TESTINST_1_1(4, "addiw a0, a1, 1024", 0x0000000000001000, a0, a1);
   TESTINST_1_1(4, "addiw a0, a1, 2047", 0x0000000000001000, a0, a1);
   TESTINST_1_1(4, "addiw a0, a1, -1", 0x0000000000001000, a0, a1);
   TESTINST_1_1(4, "addiw a0, a1, -2048", 0x0000000000001000, a0, a1);

   TESTINST_1_1(4, "addiw a0, a1, 1", 0x000000007fffffff, a0, a1);
   TESTINST_1_1(4, "addiw a0, a1, 1", 0x00000000fffffffe, a0, a1);
   TESTINST_1_1(4, "addiw a0, a1, 1", 0x00000000ffffffff, a0, a1);
   TESTINST_1_1(4, "addiw t5, t6, 1", 0x0000000000001000, t5, t6);
   TESTINST_1_1(4, "addiw zero, a0, 1", 0x0000000000001000, zero, a0);

   /* -------------- slliw rd, rs1, uimm[4:0] --------------- */
   TESTINST_1_1(4, "slliw a0, a1, 0", 0xabcdef0123456789, a0, a1);
   TESTINST_1_1(4, "slliw a0, a1, 1", 0xabcdef0123456789, a0, a1);
   TESTINST_1_1(4, "slliw a0, a1, 2", 0xabcdef0123456789, a0, a1);
   TESTINST_1_1(4, "slliw a0, a1, 4", 0xabcdef0123456789, a0, a1);
   TESTINST_1_1(4, "slliw a0, a1, 8", 0xabcdef0123456789, a0, a1);
   TESTINST_1_1(4, "slliw a0, a1, 16", 0xabcdef0123456789, a0, a1);
   TESTINST_1_1(4, "slliw a0, a1, 31", 0xabcdef0123456789, a0, a1);

   TESTINST_1_1(4, "slliw t5, t6, 1", 0xabcdef0123456789, t5, t6);
   TESTINST_1_1(4, "slliw zero, a0, 1", 0xabcdef0123456789, zero, a0);

   /* -------------- srliw rd, rs1, uimm[4:0] --------------- */
   TESTINST_1_1(4, "srliw a0, a1, 0", 0xabcdef0123456789, a0, a1);
   TESTINST_1_1(4, "srliw a0, a1, 1", 0xabcdef0123456789, a0, a1);
   TESTINST_1_1(4, "srliw a0, a1, 2", 0xabcdef0123456789, a0, a1);
   TESTINST_1_1(4, "srliw a0, a1, 4", 0xabcdef0123456789, a0, a1);
   TESTINST_1_1(4, "srliw a0, a1, 8", 0xabcdef0123456789, a0, a1);
   TESTINST_1_1(4, "srliw a0, a1, 16", 0xabcdef0123456789, a0, a1);
   TESTINST_1_1(4, "srliw a0, a1, 31", 0xabcdef0123456789, a0, a1);

   TESTINST_1_1(4, "srliw t5, t6, 1", 0xabcdef0123456789, t5, t6);
   TESTINST_1_1(4, "srliw zero, a0, 1", 0xabcdef0123456789, zero, a0);

   /* -------------- sraiw rd, rs1, uimm[4:0] --------------- */
   TESTINST_1_1(4, "sraiw a0, a1, 0", 0xabcdef0123456789, a0, a1);
   TESTINST_1_1(4, "sraiw a0, a1, 1", 0xabcdef0123456789, a0, a1);
   TESTINST_1_1(4, "sraiw a0, a1, 2", 0xabcdef0123456789, a0, a1);
   TESTINST_1_1(4, "sraiw a0, a1, 4", 0xabcdef0123456789, a0, a1);
   TESTINST_1_1(4, "sraiw a0, a1, 8", 0xabcdef0123456789, a0, a1);
   TESTINST_1_1(4, "sraiw a0, a1, 16", 0xabcdef0123456789, a0, a1);
   TESTINST_1_1(4, "sraiw a0, a1, 31", 0xabcdef0123456789, a0, a1);

   TESTINST_1_1(4, "srai t5, t6, 1", 0xabcdef0123456789, t5, t6);
   TESTINST_1_1(4, "srai zero, a0, 1", 0xabcdef0123456789, zero, a0);

   /* ------------------ addw rd, rs1, rs2 ------------------ */
   TESTINST_1_2(4, "addw a0, a1, a2", 0x0000000000001000, 0x0000000000002000, a0,
                a1, a2);
   TESTINST_1_2(4, "addw a0, a1, a2", 0x000000007fffffff, 0x0000000000000001, a0,
                a1, a2);
   TESTINST_1_2(4, "addw a0, a1, a2", 0x00000000fffffffe, 0x0000000000000001, a0,
                a1, a2);
   TESTINST_1_2(4, "addw a0, a1, a2", 0x00000000ffffffff, 0x0000000000000001, a0,
                a1, a2);
   TESTINST_1_2(4, "addw a0, a1, a2", 0xfffffffffffffffe, 0x0000000000000001, a0,
                a1, a2);
   TESTINST_1_2(4, "addw a0, a1, a2", 0xffffffffffffffff, 0x0000000000000001, a0,
                a1, a2);

   TESTINST_1_2(4, "addw t4, t5, t6", 0x0000000000001000, 0x0000000000002000, t4,
                t5, t6);
   TESTINST_1_2(4, "addw zero, a0, a1", 0x0000000000001000, 0x0000000000002000,
                zero, a0, a1);

   /* ------------------ subw rd, rs1, rs2 ------------------ */
   TESTINST_1_2(4, "subw a0, a1, a2", 0x0000000000001000, 0x0000000000000fff, a0,
                a1, a2);
   TESTINST_1_2(4, "subw a0, a1, a2", 0x0000000000001000, 0x0000000000001000, a0,
                a1, a2);
   TESTINST_1_2(4, "subw a0, a1, a2", 0x0000000000001000, 0x0000000000001001, a0,
                a1, a2);
   TESTINST_1_2(4, "subw a0, a1, a2", 0xffffffffffffffff, 0x0000000000000000, a0,
                a1, a2);
   TESTINST_1_2(4, "subw a0, a1, a2", 0x0000000100000000, 0x0000000000000001, a0,
                a1, a2);

   TESTINST_1_2(4, "subw t4, t5, t6", 0x0000000000001000, 0x0000000000000fff, t4,
                t5, t6);
   TESTINST_1_2(4, "subw zero, a0, a1", 0x0000000000001000, 0x0000000000000fff,
                zero, a0, a1);

   /* ------------------ sllw rd, rs1, rs2 ------------------ */
   TESTINST_1_2(4, "sllw a0, a1, a2", 0xabcdef0123456789, 0, a0, a1, a2);
   TESTINST_1_2(4, "sllw a0, a1, a2", 0xabcdef0123456789, 1, a0, a1, a2);
   TESTINST_1_2(4, "sllw a0, a1, a2", 0xabcdef0123456789, 2, a0, a1, a2);
   TESTINST_1_2(4, "sllw a0, a1, a2", 0xabcdef0123456789, 4, a0, a1, a2);
   TESTINST_1_2(4, "sllw a0, a1, a2", 0xabcdef0123456789, 8, a0, a1, a2);
   TESTINST_1_2(4, "sllw a0, a1, a2", 0xabcdef0123456789, 16, a0, a1, a2);
   TESTINST_1_2(4, "sllw a0, a1, a2", 0xabcdef0123456789, 31, a0, a1, a2);
   TESTINST_1_2(4, "sllw a0, a1, a2", 0xabcdef0123456789, 32, a0, a1, a2);

   TESTINST_1_2(4, "sllw t4, t5, t6", 0xabcdef0123456789, 1, t4, t5, t6);
   TESTINST_1_2(4, "sllw zero, a0, a1", 0xabcdef0123456789, 1, zero, a0, a1);

   /* ------------------ srlw rd, rs1, rs2 ------------------ */
   TESTINST_1_2(4, "srlw a0, a1, a2", 0xabcdef0123456789, 0, a0, a1, a2);
   TESTINST_1_2(4, "srlw a0, a1, a2", 0xabcdef0123456789, 1, a0, a1, a2);
   TESTINST_1_2(4, "srlw a0, a1, a2", 0xabcdef0123456789, 2, a0, a1, a2);
   TESTINST_1_2(4, "srlw a0, a1, a2", 0xabcdef0123456789, 4, a0, a1, a2);
   TESTINST_1_2(4, "srlw a0, a1, a2", 0xabcdef0123456789, 8, a0, a1, a2);
   TESTINST_1_2(4, "srlw a0, a1, a2", 0xabcdef0123456789, 16, a0, a1, a2);
   TESTINST_1_2(4, "srlw a0, a1, a2", 0xabcdef0123456789, 31, a0, a1, a2);
   TESTINST_1_2(4, "srlw a0, a1, a2", 0xabcdef0123456789, 32, a0, a1, a2);

   TESTINST_1_2(4, "srlw t4, t5, t6", 0xabcdef0123456789, 1, t4, t5, t6);
   TESTINST_1_2(4, "srlw zero, a0, a1", 0xabcdef0123456789, 1, zero, a0, a1);

   /* ------------------ sraw rd, rs1, rs2 ------------------ */
   TESTINST_1_2(4, "sraw a0, a1, a2", 0xabcdef0123456789, 0, a0, a1, a2);
   TESTINST_1_2(4, "sraw a0, a1, a2", 0xabcdef0123456789, 1, a0, a1, a2);
   TESTINST_1_2(4, "sraw a0, a1, a2", 0xabcdef0123456789, 2, a0, a1, a2);
   TESTINST_1_2(4, "sraw a0, a1, a2", 0xabcdef0123456789, 4, a0, a1, a2);
   TESTINST_1_2(4, "sraw a0, a1, a2", 0xabcdef0123456789, 8, a0, a1, a2);
   TESTINST_1_2(4, "sraw a0, a1, a2", 0xabcdef0123456789, 16, a0, a1, a2);
   TESTINST_1_2(4, "sraw a0, a1, a2", 0xabcdef0123456789, 31, a0, a1, a2);
   TESTINST_1_2(4, "sraw a0, a1, a2", 0xabcdef0123456789, 32, a0, a1, a2);

   TESTINST_1_2(4, "sraw t4, t5, t6", 0xabcdef0123456789, 1, t4, t5, t6);
   TESTINST_1_2(4, "sraw zero, a0, a1", 0xabcdef0123456789, 1, zero, a0, a1);
}

int main(void)
{
   test_integer_shared();
   test_integer_additions();
   return 0;
}
