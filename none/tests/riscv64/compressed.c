/* Tests for the RV64C standard compressed instruction-set extension. */

#include "testinst.h"

static void test_compressed_00(void)
{
   printf("RV64C compressed instruction set, quadrant 0\n");

   /* ------------- c.addi4spn rd, nzuimm[9:2] -------------- */
   TESTINST_1_1(2, "c.addi4spn a0, sp, 4", 0x0000000000001000, a0, sp);
   TESTINST_1_1(2, "c.addi4spn a0, sp, 8", 0x0000000000001000, a0, sp);
   TESTINST_1_1(2, "c.addi4spn a0, sp, 16", 0x0000000000001000, a0, sp);
   TESTINST_1_1(2, "c.addi4spn a0, sp, 32", 0x0000000000001000, a0, sp);
   TESTINST_1_1(2, "c.addi4spn a0, sp, 64", 0x0000000000001000, a0, sp);
   TESTINST_1_1(2, "c.addi4spn a0, sp, 128", 0x0000000000001000, a0, sp);
   TESTINST_1_1(2, "c.addi4spn a0, sp, 256", 0x0000000000001000, a0, sp);
   TESTINST_1_1(2, "c.addi4spn a0, sp, 512", 0x0000000000001000, a0, sp);
   TESTINST_1_1(2, "c.addi4spn a0, sp, 1020", 0x0000000000001000, a0, sp);

   TESTINST_1_1(2, "c.addi4spn a0, sp, 4", 0x000000007ffffffc, a0, sp);
   TESTINST_1_1(2, "c.addi4spn a0, sp, 4", 0x00000000fffffffb, a0, sp);
   TESTINST_1_1(2, "c.addi4spn a0, sp, 4", 0x00000000fffffffc, a0, sp);
   TESTINST_1_1(2, "c.addi4spn a5, sp, 4", 0x0000000000001000, a0, sp);

   /* -------------- c.fld rd, uimm[7:3](rs1) --------------- */
   TESTINST_1_1_FLOAD(2, "c.fld fa0, 0(a1)", fa0, a1);
   TESTINST_1_1_FLOAD(2, "c.fld fa0, 8(a1)", fa0, a1);
   TESTINST_1_1_FLOAD(2, "c.fld fa0, 16(a1)", fa0, a1);
   TESTINST_1_1_FLOAD(2, "c.fld fa0, 32(a1)", fa0, a1);
   TESTINST_1_1_FLOAD(2, "c.fld fa0, 64(a1)", fa0, a1);
   TESTINST_1_1_FLOAD(2, "c.fld fa0, 128(a1)", fa0, a1);
   TESTINST_1_1_FLOAD(2, "c.fld fa0, 248(a1)", fa0, a1);

   TESTINST_1_1_FLOAD(2, "c.fld fa4, 0(a5)", fa4, a5);

   /* --------------- c.lw rd, uimm[6:2](rs1) --------------- */
   TESTINST_1_1_LOAD(2, "c.lw a0, 0(a1)", a0, a1);
   TESTINST_1_1_LOAD(2, "c.lw a0, 4(a1)", a0, a1);
   TESTINST_1_1_LOAD(2, "c.lw a0, 8(a1)", a0, a1);
   TESTINST_1_1_LOAD(2, "c.lw a0, 16(a1)", a0, a1);
   TESTINST_1_1_LOAD(2, "c.lw a0, 32(a1)", a0, a1);
   TESTINST_1_1_LOAD(2, "c.lw a0, 64(a1)", a0, a1);
   TESTINST_1_1_LOAD(2, "c.lw a0, 124(a1)", a0, a1);

   TESTINST_1_1_LOAD(2, "c.lw a4, 0(a5)", a4, a5);

   /* --------------- c.ld rd, uimm[7:3](rs1) --------------- */
   TESTINST_1_1_LOAD(2, "c.ld a0, 0(a1)", a0, a1);
   TESTINST_1_1_LOAD(2, "c.ld a0, 8(a1)", a0, a1);
   TESTINST_1_1_LOAD(2, "c.ld a0, 16(a1)", a0, a1);
   TESTINST_1_1_LOAD(2, "c.ld a0, 32(a1)", a0, a1);
   TESTINST_1_1_LOAD(2, "c.ld a0, 64(a1)", a0, a1);
   TESTINST_1_1_LOAD(2, "c.ld a0, 128(a1)", a0, a1);
   TESTINST_1_1_LOAD(2, "c.ld a0, 248(a1)", a0, a1);

   TESTINST_1_1_LOAD(2, "c.ld a4, 0(a5)", a4, a5);

   /* -------------- c.fsd rs2, uimm[7:3](rs1) -------------- */
   TESTINST_0_2_FSTORE(2, "c.fsd fa0, 0(a1)", 0xabcdef0123456789, fa0, a1);
   TESTINST_0_2_FSTORE(2, "c.fsd fa0, 8(a1)", 0xabcdef0123456789, fa0, a1);
   TESTINST_0_2_FSTORE(2, "c.fsd fa0, 16(a1)", 0xabcdef0123456789, fa0, a1);
   TESTINST_0_2_FSTORE(2, "c.fsd fa0, 32(a1)", 0xabcdef0123456789, fa0, a1);
   TESTINST_0_2_FSTORE(2, "c.fsd fa0, 64(a1)", 0xabcdef0123456789, fa0, a1);
   TESTINST_0_2_FSTORE(2, "c.fsd fa0, 128(a1)", 0xabcdef0123456789, fa0, a1);
   TESTINST_0_2_FSTORE(2, "c.fsd fa0, 248(a1)", 0xabcdef0123456789, fa0, a1);

   TESTINST_0_2_FSTORE(2, "c.fsd fa4, 0(a5)", 0xabcdef0123456789, fa4, a5);

   /* -------------- c.sw rs2, uimm[6:2](rs1) --------------- */
   TESTINST_0_2_STORE(2, "c.sw a0, 0(a1)", 0xabcdef0123456789, a0, a1);
   TESTINST_0_2_STORE(2, "c.sw a0, 4(a1)", 0xabcdef0123456789, a0, a1);
   TESTINST_0_2_STORE(2, "c.sw a0, 8(a1)", 0xabcdef0123456789, a0, a1);
   TESTINST_0_2_STORE(2, "c.sw a0, 16(a1)", 0xabcdef0123456789, a0, a1);
   TESTINST_0_2_STORE(2, "c.sw a0, 32(a1)", 0xabcdef0123456789, a0, a1);
   TESTINST_0_2_STORE(2, "c.sw a0, 64(a1)", 0xabcdef0123456789, a0, a1);
   TESTINST_0_2_STORE(2, "c.sw a0, 124(a1)", 0xabcdef0123456789, a0, a1);

   TESTINST_0_2_STORE(2, "c.sw a4, 0(a5)", 0xabcdef0123456789, a4, a5);

   /* -------------- c.sd rs2, uimm[7:3](rs1) --------------- */
   TESTINST_0_2_STORE(2, "c.sd a0, 0(a1)", 0xabcdef0123456789, a0, a1);
   TESTINST_0_2_STORE(2, "c.sd a0, 8(a1)", 0xabcdef0123456789, a0, a1);
   TESTINST_0_2_STORE(2, "c.sd a0, 16(a1)", 0xabcdef0123456789, a0, a1);
   TESTINST_0_2_STORE(2, "c.sd a0, 32(a1)", 0xabcdef0123456789, a0, a1);
   TESTINST_0_2_STORE(2, "c.sd a0, 64(a1)", 0xabcdef0123456789, a0, a1);
   TESTINST_0_2_STORE(2, "c.sd a0, 128(a1)", 0xabcdef0123456789, a0, a1);
   TESTINST_0_2_STORE(2, "c.sd a0, 248(a1)", 0xabcdef0123456789, a0, a1);

   TESTINST_0_2_STORE(2, "c.sd a4, 0(a5)", 0xabcdef0123456789, a4, a5);

   printf("\n");
}

static void test_compressed_01(void)
{
   printf("RV64C compressed instruction set, quadrant 1\n");

   /* ------------------------ c.nop ------------------------ */
   TESTINST_0_0(2, "c.nop");

   /* -------------- c.addi rd_rs1, nzimm[5:0] -------------- */
   TESTINST_1_1(2, "c.addi a0, 1", 0x0000000000001000, a0, a0);
   TESTINST_1_1(2, "c.addi a0, 2", 0x0000000000001000, a0, a0);
   TESTINST_1_1(2, "c.addi a0, 4", 0x0000000000001000, a0, a0);
   TESTINST_1_1(2, "c.addi a0, 8", 0x0000000000001000, a0, a0);
   TESTINST_1_1(2, "c.addi a0, 16", 0x0000000000001000, a0, a0);
   TESTINST_1_1(2, "c.addi a0, 31", 0x0000000000001000, a0, a0);
   TESTINST_1_1(2, "c.addi a0, -1", 0x0000000000001000, a0, a0);
   TESTINST_1_1(2, "c.addi a0, -32", 0x0000000000001000, a0, a0);

   TESTINST_1_1(2, "c.addi a0, 1", 0x000000007fffffff, a0, a0);
   TESTINST_1_1(2, "c.addi a0, 1", 0x00000000fffffffe, a0, a0);
   TESTINST_1_1(2, "c.addi a0, 1", 0x00000000ffffffff, a0, a0);
   TESTINST_1_1(2, "c.addi t6, 1", 0x0000000000001000, t6, t6);

   /* -------------- c.addiw rd_rs1, imm[5:0] --------------- */
   TESTINST_1_1(2, "c.addiw a0, 0", 0x0000000000001000, a0, a0);
   TESTINST_1_1(2, "c.addiw a0, 1", 0x0000000000001000, a0, a0);
   TESTINST_1_1(2, "c.addiw a0, 2", 0x0000000000001000, a0, a0);
   TESTINST_1_1(2, "c.addiw a0, 4", 0x0000000000001000, a0, a0);
   TESTINST_1_1(2, "c.addiw a0, 8", 0x0000000000001000, a0, a0);
   TESTINST_1_1(2, "c.addiw a0, 16", 0x0000000000001000, a0, a0);
   TESTINST_1_1(2, "c.addiw a0, 31", 0x0000000000001000, a0, a0);
   TESTINST_1_1(2, "c.addiw a0, -1", 0x0000000000001000, a0, a0);
   TESTINST_1_1(2, "c.addiw a0, -32", 0x0000000000001000, a0, a0);

   TESTINST_1_1(2, "c.addiw a0, 1", 0x000000007fffffff, a0, a0);
   TESTINST_1_1(2, "c.addiw a0, 1", 0x00000000fffffffe, a0, a0);
   TESTINST_1_1(2, "c.addiw a0, 1", 0x00000000ffffffff, a0, a0);
   TESTINST_1_1(2, "c.addiw t6, 0", 0x0000000000001000, t6, t6);

   /* ------------------ c.li rd, imm[5:0] ------------------ */
   TESTINST_1_0(2, "c.li a0, 0", a0);
   TESTINST_1_0(2, "c.li a0, 1", a0);
   TESTINST_1_0(2, "c.li a0, 2", a0);
   TESTINST_1_0(2, "c.li a0, 4", a0);
   TESTINST_1_0(2, "c.li a0, 8", a0);
   TESTINST_1_0(2, "c.li a0, 15", a0);
   TESTINST_1_0(2, "c.li a0, -1", a0);
   TESTINST_1_0(2, "c.li a0, -16", a0);

   TESTINST_1_0(2, "c.li t6, 1", t6);

   /* ---------------- c.addi16sp nzimm[9:4] ---------------- */
   TESTINST_1_1(2, "c.addi16sp sp, 16", 0x0000000000001000, sp, sp);
   TESTINST_1_1(2, "c.addi16sp sp, 32", 0x0000000000001000, sp, sp);
   TESTINST_1_1(2, "c.addi16sp sp, 64", 0x0000000000001000, sp, sp);
   TESTINST_1_1(2, "c.addi16sp sp, 128", 0x0000000000001000, sp, sp);
   TESTINST_1_1(2, "c.addi16sp sp, 256", 0x0000000000001000, sp, sp);
   TESTINST_1_1(2, "c.addi16sp sp, 496", 0x0000000000001000, sp, sp);
   TESTINST_1_1(2, "c.addi16sp sp, -16", 0x0000000000001000, sp, sp);
   TESTINST_1_1(2, "c.addi16sp sp, -512", 0x0000000000001000, sp, sp);

   TESTINST_1_1(2, "c.addi16sp sp, 16", 0x000000007ffffff0, sp, sp);
   TESTINST_1_1(2, "c.addi16sp sp, 16", 0x00000000ffffffef, sp, sp);
   TESTINST_1_1(2, "c.addi16sp sp, 16", 0x00000000fffffff0, sp, sp);

   /* --------------- c.lui rd, nzimm[17:12] ---------------- */
   TESTINST_1_0(2, "c.lui a0, 1", a0);
   TESTINST_1_0(2, "c.lui a0, 2", a0);
   TESTINST_1_0(2, "c.lui a0, 4", a0);
   TESTINST_1_0(2, "c.lui a0, 8", a0);
   TESTINST_1_0(2, "c.lui a0, 16", a0);
   TESTINST_1_0(2, "c.lui a0, 31", a0);
   TESTINST_1_0(2, "c.lui a0, 0xfffff" /* -1 */, a0);
   TESTINST_1_0(2, "c.lui a0, 0xfffe0" /* -32 */, a0);

   TESTINST_1_0(2, "c.lui t6, 1", t6);

   /* ------------- c.srli rd_rs1, nzuimm[5:0] -------------- */
   TESTINST_1_1(2, "c.srli a0, 1", 0xabcdef0123456789, a0, a0);
   TESTINST_1_1(2, "c.srli a0, 2", 0xabcdef0123456789, a0, a0);
   TESTINST_1_1(2, "c.srli a0, 4", 0xabcdef0123456789, a0, a0);
   TESTINST_1_1(2, "c.srli a0, 8", 0xabcdef0123456789, a0, a0);
   TESTINST_1_1(2, "c.srli a0, 16", 0xabcdef0123456789, a0, a0);
   TESTINST_1_1(2, "c.srli a0, 32", 0xabcdef0123456789, a0, a0);
   TESTINST_1_1(2, "c.srli a0, 63", 0xabcdef0123456789, a0, a0);

   TESTINST_1_1(2, "c.srli a5, 1", 0xabcdef0123456789, a5, a5);

   /* ------------- c.srai rd_rs1, nzuimm[5:0] -------------- */
   TESTINST_1_1(2, "c.srai a0, 1", 0xabcdef0123456789, a0, a0);
   TESTINST_1_1(2, "c.srai a0, 2", 0xabcdef0123456789, a0, a0);
   TESTINST_1_1(2, "c.srai a0, 4", 0xabcdef0123456789, a0, a0);
   TESTINST_1_1(2, "c.srai a0, 8", 0xabcdef0123456789, a0, a0);
   TESTINST_1_1(2, "c.srai a0, 16", 0xabcdef0123456789, a0, a0);
   TESTINST_1_1(2, "c.srai a0, 32", 0xabcdef0123456789, a0, a0);
   TESTINST_1_1(2, "c.srai a0, 63", 0xabcdef0123456789, a0, a0);

   TESTINST_1_1(2, "c.srai a5, 1", 0xabcdef0123456789, a5, a5);

   /* --------------- c.andi rd_rs1, imm[5:0] --------------- */
   TESTINST_1_1(2, "c.andi a0, 0", 0xffffffffffffffff, a0, a0);
   TESTINST_1_1(2, "c.andi a0, 1", 0xffffffffffffffff, a0, a0);
   TESTINST_1_1(2, "c.andi a0, 2", 0xffffffffffffffff, a0, a0);
   TESTINST_1_1(2, "c.andi a0, 4", 0xffffffffffffffff, a0, a0);
   TESTINST_1_1(2, "c.andi a0, 8", 0xffffffffffffffff, a0, a0);
   TESTINST_1_1(2, "c.andi a0, 16", 0xffffffffffffffff, a0, a0);
   TESTINST_1_1(2, "c.andi a0, 31", 0xffffffffffffffff, a0, a0);

   TESTINST_1_1(2, "c.andi a5, 0", 0xffffffffffffffff, a5, a5);

   /* ------------------ c.sub rd_rs1, rs2 ------------------ */
   TESTINST_1_2(2, "c.sub a0, a1", 0x0000000000001000, 0x0000000000000fff, a0,
                a0, a1);
   TESTINST_1_2(2, "c.sub a0, a1", 0x0000000000001000, 0x0000000000001000, a0,
                a0, a1);
   TESTINST_1_2(2, "c.sub a0, a1", 0x0000000000001000, 0x0000000000001001, a0,
                a0, a1);
   TESTINST_1_2(2, "c.sub a0, a1", 0xffffffffffffffff, 0x0000000000000000, a0,
                a0, a1);
   TESTINST_1_2(2, "c.sub a0, a1", 0x0000000100000000, 0x0000000000000001, a0,
                a0, a1);
   TESTINST_1_2(2, "c.sub a4, a5", 0x0000000000001000, 0x0000000000000fff, a4,
                a4, a5);

   /* ------------------ c.xor rd_rs1, rs2 ------------------ */
   TESTINST_1_2(2, "c.xor a0, a1", 0x0000ffff0000ffff, 0x00000000ffffffff, a0,
                a0, a1);
   TESTINST_1_2(2, "c.xor a4, a5", 0x0000ffff0000ffff, 0x00000000ffffffff, a4,
                a4, a5);

   /* ------------------ c.or rd_rs1, rs2 ------------------- */
   TESTINST_1_2(2, "c.or a0, a1", 0x0000ffff0000ffff, 0x00000000ffffffff, a0,
                a0, a1);
   TESTINST_1_2(2, "c.or a4, a5", 0x0000ffff0000ffff, 0x00000000ffffffff, a4,
                a4, a5);

   /* ------------------ c.and rd_rs1, rs2 ------------------ */
   TESTINST_1_2(2, "c.and a0, a1", 0x0000ffff0000ffff, 0x00000000ffffffff, a0,
                a0, a1);
   TESTINST_1_2(2, "c.and a4, a5", 0x0000ffff0000ffff, 0x00000000ffffffff, a4,
                a4, a5);

   /* ----------------- c.subw rd_rs1, rs2 ------------------ */
   TESTINST_1_2(2, "c.subw a0, a1", 0x0000000000001000, 0x0000000000000fff, a0,
                a0, a1);
   TESTINST_1_2(2, "c.subw a0, a1", 0x0000000000001000, 0x0000000000001000, a0,
                a0, a1);
   TESTINST_1_2(2, "c.subw a0, a1", 0x0000000000001000, 0x0000000000001001, a0,
                a0, a1);
   TESTINST_1_2(2, "c.subw a0, a1", 0xffffffffffffffff, 0x0000000000000000, a0,
                a0, a1);
   TESTINST_1_2(2, "c.subw a0, a1", 0x0000000100000000, 0x0000000000000001, a0,
                a0, a1);
   TESTINST_1_2(2, "c.subw a4, a5", 0x0000000000001000, 0x0000000000000fff, a4,
                a4, a5);

   /* ----------------- c.addw rd_rs1, rs2 ------------------ */
   TESTINST_1_2(2, "c.addw a0, a1", 0x0000000000001000, 0x0000000000002000, a0,
                a0, a1);
   TESTINST_1_2(2, "c.addw a0, a1", 0x000000007fffffff, 0x0000000000000001, a0,
                a0, a1);
   TESTINST_1_2(2, "c.addw a0, a1", 0x00000000fffffffe, 0x0000000000000001, a0,
                a0, a1);
   TESTINST_1_2(2, "c.addw a0, a1", 0x00000000ffffffff, 0x0000000000000001, a0,
                a0, a1);
   TESTINST_1_2(2, "c.addw a0, a1", 0xfffffffffffffffe, 0x0000000000000001, a0,
                a0, a1);
   TESTINST_1_2(2, "c.addw a0, a1", 0xffffffffffffffff, 0x0000000000000001, a0,
                a0, a1);
   TESTINST_1_2(2, "c.addw a4, a5", 0x0000000000001000, 0x0000000000002000, a4,
                a4, a5);

   /* -------------------- c.j imm[11:1] -------------------- */
   TESTINST_0_0_J_RANGE(2, "c.j .+4", 4);
   TESTINST_0_0_J_RANGE(2, "c.j .+6", 6);
   TESTINST_0_0_J_RANGE(2, "c.j .+8", 8);
   TESTINST_0_0_J_RANGE(2, "c.j .+16", 16);
   TESTINST_0_0_J_RANGE(2, "c.j .+32", 32);
   TESTINST_0_0_J_RANGE(2, "c.j .+64", 64);
   TESTINST_0_0_J_RANGE(2, "c.j .+128", 128);
   TESTINST_0_0_J_RANGE(2, "c.j .+256", 256);
   TESTINST_0_0_J_RANGE(2, "c.j .+512", 512);
   TESTINST_0_0_J_RANGE(2, "c.j .+1024", 1024);
   TESTINST_0_0_J_RANGE(2, "c.j .+2044", 2044);
   TESTINST_0_0_J_RANGE(2, "c.j .-4", -4);
   TESTINST_0_0_J_RANGE(2, "c.j .-6", -6);
   TESTINST_0_0_J_RANGE(2, "c.j .-2048", -2048);

   /* ---------------- c.beqz rs1, imm[8:1] ----------------- */
   TESTINST_0_1_BxxZ_RANGE(2, "c.beqz a0, .+4", 0, 4, a0);
   TESTINST_0_1_BxxZ_RANGE(2, "c.beqz a0, .+6", 0, 6, a0);
   TESTINST_0_1_BxxZ_RANGE(2, "c.beqz a0, .+8", 0, 8, a0);
   TESTINST_0_1_BxxZ_RANGE(2, "c.beqz a0, .+16", 0, 16, a0);
   TESTINST_0_1_BxxZ_RANGE(2, "c.beqz a0, .+32", 0, 32, a0);
   TESTINST_0_1_BxxZ_RANGE(2, "c.beqz a0, .+64", 0, 64, a0);
   TESTINST_0_1_BxxZ_RANGE(2, "c.beqz a0, .+128", 0, 128, a0);
   TESTINST_0_1_BxxZ_RANGE(2, "c.beqz a0, .+252", 0, 252, a0);
   TESTINST_0_1_BxxZ_RANGE(2, "c.beqz a0, .-4", 0, -4, a0);
   TESTINST_0_1_BxxZ_RANGE(2, "c.beqz a0, .-6", 0, -6, a0);
   TESTINST_0_1_BxxZ_RANGE(2, "c.beqz a0, .-256", 0, -256, a0);

   TESTINST_0_1_BxxZ_RANGE(2, "c.beqz a5, .+4", 0, 4, a5);
   TESTINST_0_1_BxxZ_COND(2, "c.beqz a0, 1f", 0, a0);
   TESTINST_0_1_BxxZ_COND(2, "c.beqz a0, 1f", 1, a0);

   /* ---------------- c.bnez rs1, imm[8:1] ----------------- */
   TESTINST_0_1_BxxZ_RANGE(2, "c.bnez a0, .+4", 1, 4, a0);
   TESTINST_0_1_BxxZ_RANGE(2, "c.bnez a0, .+6", 1, 6, a0);
   TESTINST_0_1_BxxZ_RANGE(2, "c.bnez a0, .+8", 1, 8, a0);
   TESTINST_0_1_BxxZ_RANGE(2, "c.bnez a0, .+16", 1, 16, a0);
   TESTINST_0_1_BxxZ_RANGE(2, "c.bnez a0, .+32", 1, 32, a0);
   TESTINST_0_1_BxxZ_RANGE(2, "c.bnez a0, .+64", 1, 64, a0);
   TESTINST_0_1_BxxZ_RANGE(2, "c.bnez a0, .+128", 1, 128, a0);
   TESTINST_0_1_BxxZ_RANGE(2, "c.bnez a0, .+252", 1, 252, a0);
   TESTINST_0_1_BxxZ_RANGE(2, "c.bnez a0, .-4", 1, -4, a0);
   TESTINST_0_1_BxxZ_RANGE(2, "c.bnez a0, .-6", 1, -6, a0);
   TESTINST_0_1_BxxZ_RANGE(2, "c.bnez a0, .-256", 1, -256, a0);

   TESTINST_0_1_BxxZ_RANGE(2, "c.bnez a5, .+4", 1, 4, a5);
   TESTINST_0_1_BxxZ_COND(2, "c.bnez a0, 1f", 0, a0);
   TESTINST_0_1_BxxZ_COND(2, "c.bnez a0, 1f", 1, a0);

   printf("\n");
}

static void test_compressed_10(void)
{
   printf("RV64C compressed instruction set, quadrant 2\n");

   /* ------------- c.slli rd_rs1, nzuimm[5:0] -------------- */
   TESTINST_1_1(2, "c.slli a0, 1", 0xabcdef0123456789, a0, a0);
   TESTINST_1_1(2, "c.slli a0, 2", 0xabcdef0123456789, a0, a0);
   TESTINST_1_1(2, "c.slli a0, 4", 0xabcdef0123456789, a0, a0);
   TESTINST_1_1(2, "c.slli a0, 8", 0xabcdef0123456789, a0, a0);
   TESTINST_1_1(2, "c.slli a0, 16", 0xabcdef0123456789, a0, a0);
   TESTINST_1_1(2, "c.slli a0, 32", 0xabcdef0123456789, a0, a0);
   TESTINST_1_1(2, "c.slli a0, 63", 0xabcdef0123456789, a0, a0);

   TESTINST_1_1(2, "c.slli a5, 1", 0xabcdef0123456789, a5, a5);

   /* -------------- c.fldsp rd, uimm[8:3](x2) -------------- */
   TESTINST_1_1_FLOAD(2, "c.fldsp fa0, 0(sp)", fa0, sp);
   TESTINST_1_1_FLOAD(2, "c.fldsp fa0, 8(sp)", fa0, sp);
   TESTINST_1_1_FLOAD(2, "c.fldsp fa0, 16(sp)", fa0, sp);
   TESTINST_1_1_FLOAD(2, "c.fldsp fa0, 32(sp)", fa0, sp);
   TESTINST_1_1_FLOAD(2, "c.fldsp fa0, 64(sp)", fa0, sp);
   TESTINST_1_1_FLOAD(2, "c.fldsp fa0, 128(sp)", fa0, sp);
   TESTINST_1_1_FLOAD(2, "c.fldsp fa0, 256(sp)", fa0, sp);
   TESTINST_1_1_FLOAD(2, "c.fldsp fa0, 504(sp)", fa0, sp);

   TESTINST_1_1_FLOAD(2, "c.fldsp fa5, 0(sp)", fa5, sp);

   /* -------------- c.lwsp rd, uimm[7:2](x2) --------------- */
   TESTINST_1_1_LOAD(2, "c.lwsp a0, 0(sp)", a0, sp);
   TESTINST_1_1_LOAD(2, "c.lwsp a0, 4(sp)", a0, sp);
   TESTINST_1_1_LOAD(2, "c.lwsp a0, 8(sp)", a0, sp);
   TESTINST_1_1_LOAD(2, "c.lwsp a0, 16(sp)", a0, sp);
   TESTINST_1_1_LOAD(2, "c.lwsp a0, 32(sp)", a0, sp);
   TESTINST_1_1_LOAD(2, "c.lwsp a0, 64(sp)", a0, sp);
   TESTINST_1_1_LOAD(2, "c.lwsp a0, 128(sp)", a0, sp);
   TESTINST_1_1_LOAD(2, "c.lwsp a0, 252(sp)", a0, sp);

   TESTINST_1_1_LOAD(2, "c.lwsp a5, 0(sp)", a5, sp);

   /* -------------- c.ldsp rd, uimm[8:3](x2) --------------- */
   TESTINST_1_1_LOAD(2, "c.ldsp a0, 0(sp)", a0, sp);
   TESTINST_1_1_LOAD(2, "c.ldsp a0, 8(sp)", a0, sp);
   TESTINST_1_1_LOAD(2, "c.ldsp a0, 16(sp)", a0, sp);
   TESTINST_1_1_LOAD(2, "c.ldsp a0, 32(sp)", a0, sp);
   TESTINST_1_1_LOAD(2, "c.ldsp a0, 64(sp)", a0, sp);
   TESTINST_1_1_LOAD(2, "c.ldsp a0, 128(sp)", a0, sp);
   TESTINST_1_1_LOAD(2, "c.ldsp a0, 256(sp)", a0, sp);
   TESTINST_1_1_LOAD(2, "c.ldsp a0, 504(sp)", a0, sp);

   TESTINST_1_1_LOAD(2, "c.ldsp a5, 0(sp)", a5, sp);

   /* ---------------------- c.jr rs1 ----------------------- */
   TESTINST_0_1_JR_RANGE(2, "c.jr t0", "1f+4", 4, t0);
   TESTINST_0_1_JR_RANGE(2, "c.jr t0", "1f+6", 6, t0);
   TESTINST_0_1_JR_RANGE(2, "c.jr t0", "1f+8", 8, t0);
   TESTINST_0_1_JR_RANGE(2, "c.jr t0", "1f-4", -4, t0);
   TESTINST_0_1_JR_RANGE(2, "c.jr t0", "1f-6", -6, t0);
   TESTINST_0_1_JR_RANGE(2, "c.jr t0", "1f-8", -8, t0);

   TESTINST_0_1_JR_RANGE(2, "c.jr t6", "1f+4", 4, t6);

   /* -------------------- c.mv rd, rs2 --------------------- */
   TESTINST_1_1(2, "c.mv t0, t6", 0xabcdef0123456789, t0, t6);
   TESTINST_1_1(2, "c.mv t6, t0", 0xabcdef0123456789, t6, t0);
   TESTINST_1_1(2, "c.mv s0, s11", 0xabcdef0123456789, s0, s11);
   TESTINST_1_1(2, "c.mv s11, s0", 0xabcdef0123456789, s11, s0);
   TESTINST_1_1(2, "c.mv a0, a7", 0xabcdef0123456789, a0, a7);
   TESTINST_1_1(2, "c.mv a7, a0", 0xabcdef0123456789, a7, a0);

   /* --------------------- c.jalr rs1 ---------------------- */
   TESTINST_1_1_JALR_RANGE(2, "c.jalr t0", "1f+4", 4, ra, t0);
   TESTINST_1_1_JALR_RANGE(2, "c.jalr t0", "1f+6", 6, ra, t0);
   TESTINST_1_1_JALR_RANGE(2, "c.jalr t0", "1f+8", 8, ra, t0);
   TESTINST_1_1_JALR_RANGE(2, "c.jalr t0", "1f-4", -4, ra, t0);
   TESTINST_1_1_JALR_RANGE(2, "c.jalr t0", "1f-6", -6, ra, t0);
   TESTINST_1_1_JALR_RANGE(2, "c.jalr t0", "1f-8", -8, ra, t0);

   TESTINST_1_1_JALR_RANGE(2, "c.jalr t6", "1f+4", 4, ra, t6);

   /* ------------------ c.add rd_rs1, rs2 ------------------ */
   TESTINST_1_2(2, "c.add a0, a1", 0x0000000000001000, 0x0000000000002000, a0,
                a0, a1);
   TESTINST_1_2(2, "c.add a0, a1", 0x000000007fffffff, 0x0000000000000001, a0,
                a0, a1);
   TESTINST_1_2(2, "c.add a0, a1", 0x00000000fffffffe, 0x0000000000000001, a0,
                a0, a1);
   TESTINST_1_2(2, "c.add a0, a1", 0x00000000ffffffff, 0x0000000000000001, a0,
                a0, a1);
   TESTINST_1_2(2, "c.add a0, a1", 0xfffffffffffffffe, 0x0000000000000001, a0,
                a0, a1);
   TESTINST_1_2(2, "c.add a0, a1", 0xffffffffffffffff, 0x0000000000000001, a0,
                a0, a1);
   TESTINST_1_2(2, "c.add a4, a5", 0x0000000000001000, 0x0000000000002000, a4,
                a4, a5);

   /* ------------- c.fsdsp rs2, uimm[8:3](x2) -------------- */
   TESTINST_0_2_FSTORE(2, "c.fsdsp fa0, 0(sp)", 0xabcdef0123456789, fa0, sp);
   TESTINST_0_2_FSTORE(2, "c.fsdsp fa0, 8(sp)", 0xabcdef0123456789, fa0, sp);
   TESTINST_0_2_FSTORE(2, "c.fsdsp fa0, 16(sp)", 0xabcdef0123456789, fa0, sp);
   TESTINST_0_2_FSTORE(2, "c.fsdsp fa0, 32(sp)", 0xabcdef0123456789, fa0, sp);
   TESTINST_0_2_FSTORE(2, "c.fsdsp fa0, 64(sp)", 0xabcdef0123456789, fa0, sp);
   TESTINST_0_2_FSTORE(2, "c.fsdsp fa0, 128(sp)", 0xabcdef0123456789, fa0, sp);
   TESTINST_0_2_FSTORE(2, "c.fsdsp fa0, 256(sp)", 0xabcdef0123456789, fa0, sp);
   TESTINST_0_2_FSTORE(2, "c.fsdsp fa0, 504(sp)", 0xabcdef0123456789, fa0, sp);

   TESTINST_0_2_FSTORE(2, "c.fsdsp fa5, 0(sp)", 0xabcdef0123456789, fa5, sp);

   /* -------------- c.swsp rs2, uimm[7:2](x2) -------------- */
   TESTINST_0_2_STORE(2, "c.swsp a0, 0(sp)", 0xabcdef0123456789, a0, sp);
   TESTINST_0_2_STORE(2, "c.swsp a0, 4(sp)", 0xabcdef0123456789, a0, sp);
   TESTINST_0_2_STORE(2, "c.swsp a0, 8(sp)", 0xabcdef0123456789, a0, sp);
   TESTINST_0_2_STORE(2, "c.swsp a0, 16(sp)", 0xabcdef0123456789, a0, sp);
   TESTINST_0_2_STORE(2, "c.swsp a0, 32(sp)", 0xabcdef0123456789, a0, sp);
   TESTINST_0_2_STORE(2, "c.swsp a0, 64(sp)", 0xabcdef0123456789, a0, sp);
   TESTINST_0_2_STORE(2, "c.swsp a0, 128(sp)", 0xabcdef0123456789, a0, sp);
   TESTINST_0_2_STORE(2, "c.swsp a0, 252(sp)", 0xabcdef0123456789, a0, sp);

   TESTINST_0_2_STORE(2, "c.swsp a5, 0(sp)", 0xabcdef0123456789, a5, sp);

   /* -------------- c.sdsp rs2, uimm[8:3](x2) -------------- */
   TESTINST_0_2_STORE(2, "c.sdsp a0, 0(sp)", 0xabcdef0123456789, a0, sp);
   TESTINST_0_2_STORE(2, "c.sdsp a0, 8(sp)", 0xabcdef0123456789, a0, sp);
   TESTINST_0_2_STORE(2, "c.sdsp a0, 16(sp)", 0xabcdef0123456789, a0, sp);
   TESTINST_0_2_STORE(2, "c.sdsp a0, 32(sp)", 0xabcdef0123456789, a0, sp);
   TESTINST_0_2_STORE(2, "c.sdsp a0, 64(sp)", 0xabcdef0123456789, a0, sp);
   TESTINST_0_2_STORE(2, "c.sdsp a0, 128(sp)", 0xabcdef0123456789, a0, sp);
   TESTINST_0_2_STORE(2, "c.sdsp a0, 256(sp)", 0xabcdef0123456789, a0, sp);
   TESTINST_0_2_STORE(2, "c.sdsp a0, 504(sp)", 0xabcdef0123456789, a0, sp);

   TESTINST_0_2_STORE(2, "c.sdsp a5, 0(sp)", 0xabcdef0123456789, a5, sp);
}

int main(void)
{
   test_compressed_00();
   test_compressed_01();
   test_compressed_10();
   return 0;
}
