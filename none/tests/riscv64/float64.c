/* Tests for the RV64D standard double-precision floating-point instruction-set
   extension. */

#include "testinst.h"

static void test_float64_shared(void)
{
   printf("RV64D double-precision FP instruction set, shared operations\n");

   /* --------------- fld rd, imm[11:0](rs1) ---------------- */
   TESTINST_1_1_FLOAD(4, "fld fa0, 0(a1)", fa0, a1);
   TESTINST_1_1_FLOAD(4, "fld fa0, 4(a1)", fa0, a1);
   TESTINST_1_1_FLOAD(4, "fld fa0, 8(a1)", fa0, a1);
   TESTINST_1_1_FLOAD(4, "fld fa0, 16(a1)", fa0, a1);
   TESTINST_1_1_FLOAD(4, "fld fa0, 32(a1)", fa0, a1);
   TESTINST_1_1_FLOAD(4, "fld fa0, 64(a1)", fa0, a1);
   TESTINST_1_1_FLOAD(4, "fld fa0, 128(a1)", fa0, a1);
   TESTINST_1_1_FLOAD(4, "fld fa0, 256(a1)", fa0, a1);
   TESTINST_1_1_FLOAD(4, "fld fa0, 512(a1)", fa0, a1);
   TESTINST_1_1_FLOAD(4, "fld fa0, 1024(a1)", fa0, a1);
   TESTINST_1_1_FLOAD(4, "fld fa0, 2040(a1)", fa0, a1);
   TESTINST_1_1_FLOAD(4, "fld fa0, -4(a1)", fa0, a1);
   TESTINST_1_1_FLOAD(4, "fld fa0, -2048(a1)", fa0, a1);

   TESTINST_1_1_FLOAD(4, "fld fa4, 0(a5)", fa4, a5);

   /* --------------- fsd rs2, imm[11:0](rs1) --------------- */
   TESTINST_0_2_FSTORE(4, "fsd fa0, 0(a1)", 0xabcdef0123456789, fa0, a1);
   TESTINST_0_2_FSTORE(4, "fsd fa0, 4(a1)", 0xabcdef0123456789, fa0, a1);
   TESTINST_0_2_FSTORE(4, "fsd fa0, 8(a1)", 0xabcdef0123456789, fa0, a1);
   TESTINST_0_2_FSTORE(4, "fsd fa0, 16(a1)", 0xabcdef0123456789, fa0, a1);
   TESTINST_0_2_FSTORE(4, "fsd fa0, 32(a1)", 0xabcdef0123456789, fa0, a1);
   TESTINST_0_2_FSTORE(4, "fsd fa0, 64(a1)", 0xabcdef0123456789, fa0, a1);
   TESTINST_0_2_FSTORE(4, "fsd fa0, 128(a1)", 0xabcdef0123456789, fa0, a1);
   TESTINST_0_2_FSTORE(4, "fsd fa0, 256(a1)", 0xabcdef0123456789, fa0, a1);
   TESTINST_0_2_FSTORE(4, "fsd fa0, 512(a1)", 0xabcdef0123456789, fa0, a1);
   TESTINST_0_2_FSTORE(4, "fsd fa0, 1024(a1)", 0xabcdef0123456789, fa0, a1);
   TESTINST_0_2_FSTORE(4, "fsd fa0, 2040(a1)", 0xabcdef0123456789, fa0, a1);
   TESTINST_0_2_FSTORE(4, "fsd fa0, -4(a1)", 0xabcdef0123456789, fa0, a1);
   TESTINST_0_2_FSTORE(4, "fsd fa0, -2048(a1)", 0xabcdef0123456789, fa0, a1);

   TESTINST_0_2_FSTORE(4, "fsd fa4, 0(a5)", 0xabcdef0123456789, fa4, a5);

   /* ------------ fmadd.d rd, rs1, rs2, rs3, rm ------------ */
   /* 3.0 * 2.0 + 1.0 -> 7.0 */
   TESTINST_1_3_F(4, "fmadd.d fa0, fa1, fa2, fa3", 0x4008000000000000,
                  0x4000000000000000, 0x3ff0000000000000, 0x00, fa0, fa1, fa2,
                  fa3);
   /* 1.0 * 1.0 + -1.0 -> 0.0 */
   TESTINST_1_3_F(4, "fmadd.d fa0, fa1, fa2, fa3", 0x3ff0000000000000,
                  0x3ff0000000000000, 0xbff0000000000000, 0x00, fa0, fa1, fa2,
                  fa3);
   /* 2.0 * DBL_TRUE_MIN + -DBL_TRUE_MIN -> DBL_TRUE_MIN (no UF because exact)
    */
   TESTINST_1_3_F(4, "fmadd.d fa0, fa1, fa2, fa3", 0x4000000000000000,
                  0x0000000000000001, 0x8000000000000001, 0x00, fa0, fa1, fa2,
                  fa3);
   /* 2.0 * DBL_MAX + -DBL_MAX -> DBL_MAX */
   TESTINST_1_3_F(4, "fmadd.d fa0, fa1, fa2, fa3", 0x4000000000000000,
                  0x7fefffffffffffff, 0xffefffffffffffff, 0x00, fa0, fa1, fa2,
                  fa3);
   /* 2.0 * DBL_MAX + 0.0 -> INFINITY (OF, NX) */
   TESTINST_1_3_F(4, "fmadd.d fa0, fa1, fa2, fa3", 0x4000000000000000,
                  0x7fefffffffffffff, 0x0000000000000000, 0x00, fa0, fa1, fa2,
                  fa3);
   /* 2.0 * INFINITY + -INFINITY -> qNAN (NV) */
   TESTINST_1_3_F(4, "fmadd.d fa0, fa1, fa2, fa3", 0x4000000000000000,
                  0x7ff0000000000000, 0xfff0000000000000, 0x00, fa0, fa1, fa2,
                  fa3);

   /* 1.0 * 1.0 + DBL_EPSILON/2 (RNE) -> 1.0 (NX) */
   TESTINST_1_3_F(4, "fmadd.d fa0, fa1, fa2, fa3, rne", 0x3ff0000000000000,
                  0x3ff0000000000000, 0x3ca0000000000000, 0x00, fa0, fa1, fa2,
                  fa3);
   /* 1.0 * nextafter(1.0) + DBL_EPSILON/2 (RNE) -> 2nextafter(1.0) (NX) */
   TESTINST_1_3_F(4, "fmadd.d fa0, fa1, fa2, fa3, rne", 0x3ff0000000000000,
                  0x3ff0000000000001, 0x3ca0000000000000, 0x00, fa0, fa1, fa2,
                  fa3);
   /* 1.0 * 1.0 + DBL_EPSILON/2 (RTZ) -> 1.0 (NX) */
   TESTINST_1_3_F(4, "fmadd.d fa0, fa1, fa2, fa3, rtz", 0x3ff0000000000000,
                  0x3ff0000000000000, 0x3ca0000000000000, 0x00, fa0, fa1, fa2,
                  fa3);
   /* 1.0 * -1.0 + -DBL_EPSILON/2 (RTZ) -> -1.0 (NX) */
   TESTINST_1_3_F(4, "fmadd.d fa0, fa1, fa2, fa3, rtz", 0x3ff0000000000000,
                  0xbff0000000000000, 0xbca0000000000000, 0x00, fa0, fa1, fa2,
                  fa3);
   /* 1.0 * 1.0 + DBL_EPSILON/2 (RDN) -> 1.0 (NX) */
   TESTINST_1_3_F(4, "fmadd.d fa0, fa1, fa2, fa3, rdn", 0x3ff0000000000000,
                  0x3ff0000000000000, 0x3ca0000000000000, 0x00, fa0, fa1, fa2,
                  fa3);
   /* 1.0 * -1.0 + -DBL_EPSILON/2 (RDN) -> -nextafter(1.0) (NX) */
   TESTINST_1_3_F(4, "fmadd.d fa0, fa1, fa2, fa3, rdn", 0x3ff0000000000000,
                  0xbff0000000000000, 0xbca0000000000000, 0x00, fa0, fa1, fa2,
                  fa3);
   /* 1.0 * 1.0 + DBL_EPSILON/2 (RUP) -> nextafter(1.0) (NX) */
   TESTINST_1_3_F(4, "fmadd.d fa0, fa1, fa2, fa3, rup", 0x3ff0000000000000,
                  0x3ff0000000000000, 0x3ca0000000000000, 0x00, fa0, fa1, fa2,
                  fa3);
   /* 1.0 * -1.0 + -DBL_EPSILON/2 (RUP) -> -1.0 (NX) */
   TESTINST_1_3_F(4, "fmadd.d fa0, fa1, fa2, fa3, rup", 0x3ff0000000000000,
                  0xbff0000000000000, 0xbca0000000000000, 0x00, fa0, fa1, fa2,
                  fa3);
   /* 1.0 * 1.0 + DBL_EPSILON/2 (RMM) -> nextafter(1.0) (NX) */
   TESTINST_1_3_F(4, "fmadd.d fa0, fa1, fa2, fa3, rmm", 0x3ff0000000000000,
                  0x3ff0000000000000, 0x3ca0000000000000, 0x00, fa0, fa1, fa2,
                  fa3);
   /* 1.0 * -1.0 + -DBL_EPSILON/2 (RMM) -> -nextafter(1.0) (NX) */
   TESTINST_1_3_F(4, "fmadd.d fa0, fa1, fa2, fa3, rmm", 0x3ff0000000000000,
                  0xbff0000000000000, 0xbca0000000000000, 0x00, fa0, fa1, fa2,
                  fa3);

   /* 1.0 * 1.0 + DBL_EPSILON/2 (DYN-RNE) -> 1.0 (NX) */
   TESTINST_1_3_F(4, "fmadd.d fa0, fa1, fa2, fa3", 0x3ff0000000000000,
                  0x3ff0000000000000, 0x3ca0000000000000, 0x00, fa0, fa1, fa2,
                  fa3);
   /* 1.0 * nextafter(1.0) + DBL_EPSILON/2 (DYN-RNE) -> 2nextafter(1.0) (NX) */
   TESTINST_1_3_F(4, "fmadd.d fa0, fa1, fa2, fa3", 0x3ff0000000000000,
                  0x3ff0000000000001, 0x3ca0000000000000, 0x00, fa0, fa1, fa2,
                  fa3);
   /* 1.0 * 1.0 + DBL_EPSILON/2 (DYN-RTZ) -> 1.0 (NX) */
   TESTINST_1_3_F(4, "fmadd.d fa0, fa1, fa2, fa3", 0x3ff0000000000000,
                  0x3ff0000000000000, 0x3ca0000000000000, 0x20, fa0, fa1, fa2,
                  fa3);
   /* 1.0 * -1.0 + -DBL_EPSILON/2 (DYN-RTZ) -> -1.0 (NX) */
   TESTINST_1_3_F(4, "fmadd.d fa0, fa1, fa2, fa3", 0x3ff0000000000000,
                  0xbff0000000000000, 0xbca0000000000000, 0x20, fa0, fa1, fa2,
                  fa3);
   /* 1.0 * 1.0 + DBL_EPSILON/2 (DYN-RDN) -> 1.0 (NX) */
   TESTINST_1_3_F(4, "fmadd.d fa0, fa1, fa2, fa3", 0x3ff0000000000000,
                  0x3ff0000000000000, 0x3ca0000000000000, 0x40, fa0, fa1, fa2,
                  fa3);
   /* 1.0 * -1.0 + -DBL_EPSILON/2 (DYN-RDN) -> -nextafter(1.0) (NX) */
   TESTINST_1_3_F(4, "fmadd.d fa0, fa1, fa2, fa3", 0x3ff0000000000000,
                  0xbff0000000000000, 0xbca0000000000000, 0x40, fa0, fa1, fa2,
                  fa3);
   /* 1.0 * 1.0 + DBL_EPSILON/2 (DYN-RUP) -> nextafter(1.0) (NX) */
   TESTINST_1_3_F(4, "fmadd.d fa0, fa1, fa2, fa3", 0x3ff0000000000000,
                  0x3ff0000000000000, 0x3ca0000000000000, 0x60, fa0, fa1, fa2,
                  fa3);
   /* 1.0 * -1.0 + -DBL_EPSILON/2 (DYN-RUP) -> -1.0 (NX) */
   TESTINST_1_3_F(4, "fmadd.d fa0, fa1, fa2, fa3", 0x3ff0000000000000,
                  0xbff0000000000000, 0xbca0000000000000, 0x60, fa0, fa1, fa2,
                  fa3);
   /* 1.0 * 1.0 + DBL_EPSILON/2 (DYN-RMM) -> nextafter(1.0) (NX) */
   TESTINST_1_3_F(4, "fmadd.d fa0, fa1, fa2, fa3", 0x3ff0000000000000,
                  0x3ff0000000000000, 0x3ca0000000000000, 0x80, fa0, fa1, fa2,
                  fa3);
   /* 1.0 * -1.0 + -DBL_EPSILON/2 (DYN-RMM) -> -nextafter(1.0) (NX) */
   TESTINST_1_3_F(4, "fmadd.d fa0, fa1, fa2, fa3", 0x3ff0000000000000,
                  0xbff0000000000000, 0xbca0000000000000, 0x80, fa0, fa1, fa2,
                  fa3);

   /* ------------ fmsub.d rd, rs1, rs2, rs3, rm ------------ */
   /* 3.0 * 2.0 - 1.0 -> 5.0 */
   TESTINST_1_3_F(4, "fmsub.d fa0, fa1, fa2, fa3", 0x4008000000000000,
                  0x4000000000000000, 0x3ff0000000000000, 0x00, fa0, fa1, fa2,
                  fa3);
   /* 1.0 * 1.0 - 1.0 -> 0.0 */
   TESTINST_1_3_F(4, "fmsub.d fa0, fa1, fa2, fa3", 0x3ff0000000000000,
                  0x3ff0000000000000, 0x3ff0000000000000, 0x00, fa0, fa1, fa2,
                  fa3);
   /* 2.0 * DBL_TRUE_MIN - DBL_TRUE_MIN -> DBL_TRUE_MIN (no UF because exact)
    */
   TESTINST_1_3_F(4, "fmsub.d fa0, fa1, fa2, fa3", 0x4000000000000000,
                  0x0000000000000001, 0x0000000000000001, 0x00, fa0, fa1, fa2,
                  fa3);
   /* 2.0 * DBL_MAX - DBL_MAX -> DBL_MAX */
   TESTINST_1_3_F(4, "fmsub.d fa0, fa1, fa2, fa3", 0x4000000000000000,
                  0x7fefffffffffffff, 0x7fefffffffffffff, 0x00, fa0, fa1, fa2,
                  fa3);
   /* 2.0 * DBL_MAX - 0.0 -> INFINITY (OF, NX) */
   TESTINST_1_3_F(4, "fmsub.d fa0, fa1, fa2, fa3", 0x4000000000000000,
                  0x7fefffffffffffff, 0x0000000000000000, 0x00, fa0, fa1, fa2,
                  fa3);
   /* 2.0 * INFINITY - INFINITY -> qNAN (NV) */
   TESTINST_1_3_F(4, "fmsub.d fa0, fa1, fa2, fa3", 0x4000000000000000,
                  0x7ff0000000000000, 0x7ff0000000000000, 0x00, fa0, fa1, fa2,
                  fa3);

   /* 1.0 * nextafter(1.0) - DBL_EPSILON/2 (RNE) -> 1.0 (NX) */
   TESTINST_1_3_F(4, "fmsub.d fa0, fa1, fa2, fa3, rne", 0x3ff0000000000000,
                  0x3ff0000000000001, 0x3ca0000000000000, 0x00, fa0, fa1, fa2,
                  fa3);
   /* 1.0 * 2nextafter(1.0) - DBL_EPSILON/2 (RNE) -> 2nextafter(1.0) (NX) */
   TESTINST_1_3_F(4, "fmsub.d fa0, fa1, fa2, fa3, rne", 0x3ff0000000000000,
                  0x3ff0000000000002, 0x3ca0000000000000, 0x00, fa0, fa1, fa2,
                  fa3);
   /* 1.0 * nextafter(1.0) - DBL_EPSILON/2 (RTZ) -> 1.0 (NX) */
   TESTINST_1_3_F(4, "fmsub.d fa0, fa1, fa2, fa3, rtz", 0x3ff0000000000000,
                  0x3ff0000000000001, 0x3ca0000000000000, 0x00, fa0, fa1, fa2,
                  fa3);
   /* 1.0 * -1.0 - DBL_EPSILON/2 (RTZ) -> -1.0 (NX) */
   TESTINST_1_3_F(4, "fmsub.d fa0, fa1, fa2, fa3, rtz", 0x3ff0000000000000,
                  0xbff0000000000000, 0x3ca0000000000000, 0x00, fa0, fa1, fa2,
                  fa3);
   /* 1.0 * nextafter(1.0) - DBL_EPSILON/2 (RDN) -> 1.0 (NX) */
   TESTINST_1_3_F(4, "fmsub.d fa0, fa1, fa2, fa3, rdn", 0x3ff0000000000000,
                  0x3ff0000000000001, 0x3ca0000000000000, 0x00, fa0, fa1, fa2,
                  fa3);
   /* 1.0 * -1.0 - DBL_EPSILON/2 (RDN) -> -nextafter(1.0) (NX) */
   TESTINST_1_3_F(4, "fmsub.d fa0, fa1, fa2, fa3, rdn", 0x3ff0000000000000,
                  0xbff0000000000000, 0x3ca0000000000000, 0x00, fa0, fa1, fa2,
                  fa3);
   /* 1.0 * nextafter(1.0) - DBL_EPSILON/2 (RUP) -> nextafter(1.0) (NX) */
   TESTINST_1_3_F(4, "fmsub.d fa0, fa1, fa2, fa3, rup", 0x3ff0000000000000,
                  0x3ff0000000000001, 0x3ca0000000000000, 0x00, fa0, fa1, fa2,
                  fa3);
   /* 1.0 * -1.0 - DBL_EPSILON/2 (RUP) -> -1.0 (NX) */
   TESTINST_1_3_F(4, "fmsub.d fa0, fa1, fa2, fa3, rup", 0x3ff0000000000000,
                  0xbff0000000000000, 0x3ca0000000000000, 0x00, fa0, fa1, fa2,
                  fa3);
   /* 1.0 * nextafter(1.0) - DBL_EPSILON/2 (RMM) -> nextafter(1.0) (NX) */
   TESTINST_1_3_F(4, "fmsub.d fa0, fa1, fa2, fa3, rmm", 0x3ff0000000000000,
                  0x3ff0000000000001, 0x3ca0000000000000, 0x00, fa0, fa1, fa2,
                  fa3);
   /* 1.0 * -1.0 - DBL_EPSILON/2 (RMM) -> -nextafter(1.0) (NX) */
   TESTINST_1_3_F(4, "fmsub.d fa0, fa1, fa2, fa3, rmm", 0x3ff0000000000000,
                  0xbff0000000000000, 0x3ca0000000000000, 0x00, fa0, fa1, fa2,
                  fa3);

   /* 1.0 * nextafter(1.0) - DBL_EPSILON/2 (DYN-RNE) -> 1.0 (NX) */
   TESTINST_1_3_F(4, "fmsub.d fa0, fa1, fa2, fa3", 0x3ff0000000000000,
                  0x3ff0000000000001, 0x3ca0000000000000, 0x00, fa0, fa1, fa2,
                  fa3);
   /* 1.0 * 2nextafter(1.0) - DBL_EPSILON/2 (DYN-RNE) -> 2nextafter(1.0) (NX) */
   TESTINST_1_3_F(4, "fmsub.d fa0, fa1, fa2, fa3", 0x3ff0000000000000,
                  0x3ff0000000000002, 0x3ca0000000000000, 0x00, fa0, fa1, fa2,
                  fa3);
   /* 1.0 * nextafter(1.0) - DBL_EPSILON/2 (DYN-RTZ) -> 1.0 (NX) */
   TESTINST_1_3_F(4, "fmsub.d fa0, fa1, fa2, fa3", 0x3ff0000000000000,
                  0x3ff0000000000001, 0x3ca0000000000000, 0x20, fa0, fa1, fa2,
                  fa3);
   /* 1.0 * -1.0 - DBL_EPSILON/2 (DYN-RTZ) -> -1.0 (NX) */
   TESTINST_1_3_F(4, "fmsub.d fa0, fa1, fa2, fa3", 0x3ff0000000000000,
                  0xbff0000000000000, 0x3ca0000000000000, 0x20, fa0, fa1, fa2,
                  fa3);
   /* 1.0 * nextafter(1.0) - DBL_EPSILON/2 (DYN-RDN) -> 1.0 (NX) */
   TESTINST_1_3_F(4, "fmsub.d fa0, fa1, fa2, fa3", 0x3ff0000000000000,
                  0x3ff0000000000001, 0x3ca0000000000000, 0x40, fa0, fa1, fa2,
                  fa3);
   /* 1.0 * -1.0 - DBL_EPSILON/2 (DYN-RDN) -> -nextafter(1.0) (NX) */
   TESTINST_1_3_F(4, "fmsub.d fa0, fa1, fa2, fa3", 0x3ff0000000000000,
                  0xbff0000000000000, 0x3ca0000000000000, 0x40, fa0, fa1, fa2,
                  fa3);
   /* 1.0 * nextafter(1.0) - DBL_EPSILON/2 (DYN-RUP) -> nextafter(1.0) (NX) */
   TESTINST_1_3_F(4, "fmsub.d fa0, fa1, fa2, fa3", 0x3ff0000000000000,
                  0x3ff0000000000001, 0x3ca0000000000000, 0x60, fa0, fa1, fa2,
                  fa3);
   /* 1.0 * -1.0 - DBL_EPSILON/2 (DYN-RUP) -> -1.0 (NX) */
   TESTINST_1_3_F(4, "fmsub.d fa0, fa1, fa2, fa3", 0x3ff0000000000000,
                  0xbff0000000000000, 0x3ca0000000000000, 0x60, fa0, fa1, fa2,
                  fa3);
   /* 1.0 * nextafter(1.0) - DBL_EPSILON/2 (DYN-RMM) -> nextafter(1.0) (NX) */
   TESTINST_1_3_F(4, "fmsub.d fa0, fa1, fa2, fa3", 0x3ff0000000000000,
                  0x3ff0000000000001, 0x3ca0000000000000, 0x80, fa0, fa1, fa2,
                  fa3);
   /* 1.0 * -1.0 - DBL_EPSILON/2 (DYN-RMM) -> -nextafter(1.0) (NX) */
   TESTINST_1_3_F(4, "fmsub.d fa0, fa1, fa2, fa3", 0x3ff0000000000000,
                  0xbff0000000000000, 0x3ca0000000000000, 0x80, fa0, fa1, fa2,
                  fa3);

   /* ----------- fnmsub.d rd, rs1, rs2, rs3, rm ------------ */
   /* -(3.0 * 2.0) + 1.0 -> -5.0 */
   TESTINST_1_3_F(4, "fnmsub.d fa0, fa1, fa2, fa3", 0x4008000000000000,
                  0x4000000000000000, 0x3ff0000000000000, 0x00, fa0, fa1, fa2,
                  fa3);
   /* -(1.0 * 1.0) + 1.0 -> 0.0 */
   TESTINST_1_3_F(4, "fnmsub.d fa0, fa1, fa2, fa3", 0x3ff0000000000000,
                  0x3ff0000000000000, 0x3ff0000000000000, 0x00, fa0, fa1, fa2,
                  fa3);
   /* -(2.0 * DBL_TRUE_MIN) + DBL_TRUE_MIN -> -DBL_TRUE_MIN (no UF because
      exact) */
   TESTINST_1_3_F(4, "fnmsub.d fa0, fa1, fa2, fa3", 0x4000000000000000,
                  0x0000000000000001, 0x0000000000000001, 0x00, fa0, fa1, fa2,
                  fa3);
   /* -(2.0 * DBL_MAX) + DBL_MAX -> -DBL_MAX */
   TESTINST_1_3_F(4, "fnmsub.d fa0, fa1, fa2, fa3", 0x4000000000000000,
                  0x7fefffffffffffff, 0x7fefffffffffffff, 0x00, fa0, fa1, fa2,
                  fa3);
   /* -(2.0 * DBL_MAX) + 0.0 -> -INFINITY (OF, NX) */
   TESTINST_1_3_F(4, "fnmsub.d fa0, fa1, fa2, fa3", 0x4000000000000000,
                  0x7fefffffffffffff, 0x0000000000000000, 0x00, fa0, fa1, fa2,
                  fa3);
   /* -(2.0 * INFINITY) + INFINITY -> qNAN (NV) */
   TESTINST_1_3_F(4, "fnmsub.d fa0, fa1, fa2, fa3", 0x4000000000000000,
                  0x7ff0000000000000, 0x7ff0000000000000, 0x00, fa0, fa1, fa2,
                  fa3);

   /* -(-1.0 * 1.0) + DBL_EPSILON/2 (RNE) -> 1.0 (NX) */
   TESTINST_1_3_F(4, "fnmsub.d fa0, fa1, fa2, fa3, rne", 0xbff0000000000000,
                  0x3ff0000000000000, 0x3ca0000000000000, 0x00, fa0, fa1, fa2,
                  fa3);
   /* -(-1.0 * nextafter(1.0)) + DBL_EPSILON/2 (RNE) -> 2nextafter(1.0) (NX) */
   TESTINST_1_3_F(4, "fnmsub.d fa0, fa1, fa2, fa3, rne", 0xbff0000000000000,
                  0x3ff0000000000001, 0x3ca0000000000000, 0x00, fa0, fa1, fa2,
                  fa3);
   /* -(-1.0 * 1.0) + DBL_EPSILON/2 (RTZ) -> 1.0 (NX) */
   TESTINST_1_3_F(4, "fnmsub.d fa0, fa1, fa2, fa3, rtz", 0xbff0000000000000,
                  0x3ff0000000000000, 0x3ca0000000000000, 0x00, fa0, fa1, fa2,
                  fa3);
   /* -(1.0 * 1.0) + -DBL_EPSILON/2 (RTZ) -> -1.0 (NX) */
   TESTINST_1_3_F(4, "fnmsub.d fa0, fa1, fa2, fa3, rtz", 0x3ff0000000000000,
                  0x3ff0000000000000, 0xbca0000000000000, 0x00, fa0, fa1, fa2,
                  fa3);
   /* -(-1.0 * 1.0) + DBL_EPSILON/2 (RDN) -> 1.0 (NX) */
   TESTINST_1_3_F(4, "fnmsub.d fa0, fa1, fa2, fa3, rdn", 0xbff0000000000000,
                  0x3ff0000000000000, 0x3ca0000000000000, 0x00, fa0, fa1, fa2,
                  fa3);
   /* -(1.0 * 1.0) + -DBL_EPSILON/2 (RDN) -> -nextafter(1.0) (NX) */
   TESTINST_1_3_F(4, "fnmsub.d fa0, fa1, fa2, fa3, rdn", 0x3ff0000000000000,
                  0x3ff0000000000000, 0xbca0000000000000, 0x00, fa0, fa1, fa2,
                  fa3);
   /* -(-1.0 * 1.0) + DBL_EPSILON/2 (RUP) -> nextafter(1.0) (NX) */
   TESTINST_1_3_F(4, "fnmsub.d fa0, fa1, fa2, fa3, rup", 0xbff0000000000000,
                  0x3ff0000000000000, 0x3ca0000000000000, 0x00, fa0, fa1, fa2,
                  fa3);
   /* -(1.0 * 1.0) + -DBL_EPSILON/2 (RUP) -> -1.0 (NX) */
   TESTINST_1_3_F(4, "fnmsub.d fa0, fa1, fa2, fa3, rup", 0x3ff0000000000000,
                  0x3ff0000000000000, 0xbca0000000000000, 0x00, fa0, fa1, fa2,
                  fa3);
   /* -(-1.0 * 1.0) + DBL_EPSILON/2 (RMM) -> nextafter(1.0) (NX) */
   TESTINST_1_3_F(4, "fnmsub.d fa0, fa1, fa2, fa3, rmm", 0xbff0000000000000,
                  0x3ff0000000000000, 0x3ca0000000000000, 0x00, fa0, fa1, fa2,
                  fa3);
   /* -(1.0 * 1.0) + -DBL_EPSILON/2 (RMM) -> -nextafter(1.0) (NX) */
   TESTINST_1_3_F(4, "fnmsub.d fa0, fa1, fa2, fa3, rmm", 0x3ff0000000000000,
                  0x3ff0000000000000, 0xbca0000000000000, 0x00, fa0, fa1, fa2,
                  fa3);

   /* -(-1.0 * 1.0) + DBL_EPSILON/2 (DYN-RNE) -> 1.0 (NX) */
   TESTINST_1_3_F(4, "fnmsub.d fa0, fa1, fa2, fa3", 0xbff0000000000000,
                  0x3ff0000000000000, 0x3ca0000000000000, 0x00, fa0, fa1, fa2,
                  fa3);
   /* -(-1.0 * nextafter(1.0)) + DBL_EPSILON/2 (DYN-RNE) -> 2nextafter(1.0) (NX)
    */
   TESTINST_1_3_F(4, "fnmsub.d fa0, fa1, fa2, fa3", 0xbff0000000000000,
                  0x3ff0000000000001, 0x3ca0000000000000, 0x00, fa0, fa1, fa2,
                  fa3);
   /* -(-1.0 * 1.0) + DBL_EPSILON/2 (DYN-RTZ) -> 1.0 (NX) */
   TESTINST_1_3_F(4, "fnmsub.d fa0, fa1, fa2, fa3", 0xbff0000000000000,
                  0x3ff0000000000000, 0x3ca0000000000000, 0x20, fa0, fa1, fa2,
                  fa3);
   /* -(1.0 * 1.0) + -DBL_EPSILON/2 (DYN-RTZ) -> -1.0 (NX) */
   TESTINST_1_3_F(4, "fnmsub.d fa0, fa1, fa2, fa3", 0x3ff0000000000000,
                  0x3ff0000000000000, 0xbca0000000000000, 0x20, fa0, fa1, fa2,
                  fa3);
   /* -(-1.0 * 1.0) + DBL_EPSILON/2 (DYN-RDN) -> 1.0 (NX) */
   TESTINST_1_3_F(4, "fnmsub.d fa0, fa1, fa2, fa3", 0xbff0000000000000,
                  0x3ff0000000000000, 0x3ca0000000000000, 0x40, fa0, fa1, fa2,
                  fa3);
   /* -(1.0 * 1.0) + -DBL_EPSILON/2 (DYN-RDN) -> -nextafter(1.0) (NX) */
   TESTINST_1_3_F(4, "fnmsub.d fa0, fa1, fa2, fa3", 0x3ff0000000000000,
                  0x3ff0000000000000, 0xbca0000000000000, 0x40, fa0, fa1, fa2,
                  fa3);
   /* -(-1.0 * 1.0) + DBL_EPSILON/2 (DYN-RUP) -> nextafter(1.0) (NX) */
   TESTINST_1_3_F(4, "fnmsub.d fa0, fa1, fa2, fa3", 0xbff0000000000000,
                  0x3ff0000000000000, 0x3ca0000000000000, 0x60, fa0, fa1, fa2,
                  fa3);
   /* -(1.0 * 1.0) + -DBL_EPSILON/2 (DYN-RUP) -> -1.0 (NX) */
   TESTINST_1_3_F(4, "fnmsub.d fa0, fa1, fa2, fa3", 0x3ff0000000000000,
                  0x3ff0000000000000, 0xbca0000000000000, 0x60, fa0, fa1, fa2,
                  fa3);
   /* -(-1.0 * 1.0) + DBL_EPSILON/2 (DYN-RMM) -> nextafter(1.0) (NX) */
   TESTINST_1_3_F(4, "fnmsub.d fa0, fa1, fa2, fa3", 0xbff0000000000000,
                  0x3ff0000000000000, 0x3ca0000000000000, 0x80, fa0, fa1, fa2,
                  fa3);
   /* -(1.0 * 1.0) + -DBL_EPSILON/2 (DYN-RMM) -> -nextafter(1.0) (NX) */
   TESTINST_1_3_F(4, "fnmsub.d fa0, fa1, fa2, fa3", 0x3ff0000000000000,
                  0x3ff0000000000000, 0xbca0000000000000, 0x80, fa0, fa1, fa2,
                  fa3);

   /* ----------- fnmadd.d rd, rs1, rs2, rs3, rm ------------ */
   /* -(3.0 * 2.0) - 1.0 -> -7.0 */
   TESTINST_1_3_F(4, "fnmadd.d fa0, fa1, fa2, fa3", 0x4008000000000000,
                  0x4000000000000000, 0x3ff0000000000000, 0x00, fa0, fa1, fa2,
                  fa3);
   /* -(1.0 * 1.0) - -1.0 -> 0.0 */
   TESTINST_1_3_F(4, "fnmadd.d fa0, fa1, fa2, fa3", 0x3ff0000000000000,
                  0x3ff0000000000000, 0xbff0000000000000, 0x00, fa0, fa1, fa2,
                  fa3);
   /* -(2.0 * DBL_TRUE_MIN) - -DBL_TRUE_MIN -> -DBL_TRUE_MIN (no UF because
      exact) */
   TESTINST_1_3_F(4, "fnmadd.d fa0, fa1, fa2, fa3", 0x4000000000000000,
                  0x0000000000000001, 0x8000000000000001, 0x00, fa0, fa1, fa2,
                  fa3);
   /* -(2.0 * DBL_MAX) - -DBL_MAX -> -DBL_MAX */
   TESTINST_1_3_F(4, "fnmadd.d fa0, fa1, fa2, fa3", 0x4000000000000000,
                  0x7fefffffffffffff, 0xffefffffffffffff, 0x00, fa0, fa1, fa2,
                  fa3);
   /* -(2.0 * DBL_MAX) - 0.0 -> -INFINITY (OF, NX) */
   TESTINST_1_3_F(4, "fnmadd.d fa0, fa1, fa2, fa3", 0x4000000000000000,
                  0x7fefffffffffffff, 0x0000000000000000, 0x00, fa0, fa1, fa2,
                  fa3);
   /* -(2.0 * INFINITY) - -INFINITY -> qNAN (NV) */
   TESTINST_1_3_F(4, "fnmadd.d fa0, fa1, fa2, fa3", 0x4000000000000000,
                  0x7ff0000000000000, 0xfff0000000000000, 0x00, fa0, fa1, fa2,
                  fa3);

   /* -(-1.0 * nextafter(1.0)) - DBL_EPSILON/2 (RNE) -> 1.0 (NX) */
   TESTINST_1_3_F(4, "fnmadd.d fa0, fa1, fa2, fa3, rne", 0xbff0000000000000,
                  0x3ff0000000000001, 0x3ca0000000000000, 0x00, fa0, fa1, fa2,
                  fa3);
   /* -(-1.0 * 2nextafter(1.0)) - DBL_EPSILON/2 (RNE) -> 2nextafter(1.0) (NX) */
   TESTINST_1_3_F(4, "fnmadd.d fa0, fa1, fa2, fa3, rne", 0xbff0000000000000,
                  0x3ff0000000000002, 0x3ca0000000000000, 0x00, fa0, fa1, fa2,
                  fa3);
   /* -(-1.0 * nextafter(1.0)) - DBL_EPSILON/2 (RTZ) -> 1.0 (NX) */
   TESTINST_1_3_F(4, "fnmadd.d fa0, fa1, fa2, fa3, rtz", 0xbff0000000000000,
                  0x3ff0000000000001, 0x3ca0000000000000, 0x00, fa0, fa1, fa2,
                  fa3);
   /* -(1.0 * 1.0) - DBL_EPSILON/2 (RTZ) -> -1.0 (NX) */
   TESTINST_1_3_F(4, "fnmadd.d fa0, fa1, fa2, fa3, rtz", 0x3ff0000000000000,
                  0x3ff0000000000000, 0x3ca0000000000000, 0x00, fa0, fa1, fa2,
                  fa3);
   /* -(-1.0 * nextafter(1.0)) - DBL_EPSILON/2 (RDN) -> 1.0 (NX) */
   TESTINST_1_3_F(4, "fnmadd.d fa0, fa1, fa2, fa3, rdn", 0xbff0000000000000,
                  0x3ff0000000000001, 0x3ca0000000000000, 0x00, fa0, fa1, fa2,
                  fa3);
   /* -(1.0 * 1.0) - DBL_EPSILON/2 (RDN) -> -nextafter(1.0) (NX) */
   TESTINST_1_3_F(4, "fnmadd.d fa0, fa1, fa2, fa3, rdn", 0x3ff0000000000000,
                  0x3ff0000000000000, 0x3ca0000000000000, 0x00, fa0, fa1, fa2,
                  fa3);
   /* -(-1.0 * nextafter(1.0)) - DBL_EPSILON/2 (RUP) -> nextafter(1.0) (NX) */
   TESTINST_1_3_F(4, "fnmadd.d fa0, fa1, fa2, fa3, rup", 0xbff0000000000000,
                  0x3ff0000000000001, 0x3ca0000000000000, 0x00, fa0, fa1, fa2,
                  fa3);
   /* -(1.0 * 1.0) - DBL_EPSILON/2 (RUP) -> -1.0 (NX) */
   TESTINST_1_3_F(4, "fnmadd.d fa0, fa1, fa2, fa3, rup", 0x3ff0000000000000,
                  0x3ff0000000000000, 0x3ca0000000000000, 0x00, fa0, fa1, fa2,
                  fa3);
   /* -(-1.0 * nextafter(1.0)) - DBL_EPSILON/2 (RMM) -> nextafter(1.0) (NX) */
   TESTINST_1_3_F(4, "fnmadd.d fa0, fa1, fa2, fa3, rmm", 0xbff0000000000000,
                  0x3ff0000000000001, 0x3ca0000000000000, 0x00, fa0, fa1, fa2,
                  fa3);
   /* -(1.0 * 1.0) - DBL_EPSILON/2 (RMM) -> -nextafter(1.0) (NX) */
   TESTINST_1_3_F(4, "fnmadd.d fa0, fa1, fa2, fa3, rmm", 0x3ff0000000000000,
                  0x3ff0000000000000, 0x3ca0000000000000, 0x00, fa0, fa1, fa2,
                  fa3);

   /* -(-1.0 * nextafter(1.0)) - DBL_EPSILON/2 (DYN-RNE) -> 1.0 (NX) */
   TESTINST_1_3_F(4, "fnmadd.d fa0, fa1, fa2, fa3", 0xbff0000000000000,
                  0x3ff0000000000001, 0x3ca0000000000000, 0x00, fa0, fa1, fa2,
                  fa3);
   /* -(-1.0 * 2nextafter(1.0)) - DBL_EPSILON/2 (DYN-RNE) -> 2nextafter(1.0)
      (NX) */
   TESTINST_1_3_F(4, "fnmadd.d fa0, fa1, fa2, fa3", 0xbff0000000000000,
                  0x3ff0000000000002, 0x3ca0000000000000, 0x00, fa0, fa1, fa2,
                  fa3);
   /* -(-1.0 * nextafter(1.0)) - DBL_EPSILON/2 (DYN-RTZ) -> 1.0 (NX) */
   TESTINST_1_3_F(4, "fnmadd.d fa0, fa1, fa2, fa3", 0xbff0000000000000,
                  0x3ff0000000000001, 0x3ca0000000000000, 0x20, fa0, fa1, fa2,
                  fa3);
   /* -(1.0 * 1.0) - DBL_EPSILON/2 (DYN-RTZ) -> -1.0 (NX) */
   TESTINST_1_3_F(4, "fnmadd.d fa0, fa1, fa2, fa3", 0x3ff0000000000000,
                  0x3ff0000000000000, 0x3ca0000000000000, 0x20, fa0, fa1, fa2,
                  fa3);
   /* -(-1.0 * nextafter(1.0)) - DBL_EPSILON/2 (DYN-RDN) -> 1.0 (NX) */
   TESTINST_1_3_F(4, "fnmadd.d fa0, fa1, fa2, fa3", 0xbff0000000000000,
                  0x3ff0000000000001, 0x3ca0000000000000, 0x40, fa0, fa1, fa2,
                  fa3);
   /* -(1.0 * 1.0) - DBL_EPSILON/2 (DYN-RDN) -> -nextafter(1.0) (NX) */
   TESTINST_1_3_F(4, "fnmadd.d fa0, fa1, fa2, fa3", 0x3ff0000000000000,
                  0x3ff0000000000000, 0x3ca0000000000000, 0x40, fa0, fa1, fa2,
                  fa3);
   /* -(-1.0 * nextafter(1.0)) - DBL_EPSILON/2 (DYN-RUP) -> nextafter(1.0) (NX)
    */
   TESTINST_1_3_F(4, "fnmadd.d fa0, fa1, fa2, fa3", 0xbff0000000000000,
                  0x3ff0000000000001, 0x3ca0000000000000, 0x60, fa0, fa1, fa2,
                  fa3);
   /* -(1.0 * 1.0) - DBL_EPSILON/2 (DYN-RUP) -> -1.0 (NX) */
   TESTINST_1_3_F(4, "fnmadd.d fa0, fa1, fa2, fa3", 0x3ff0000000000000,
                  0x3ff0000000000000, 0x3ca0000000000000, 0x60, fa0, fa1, fa2,
                  fa3);
   /* -(-1.0 * nextafter(1.0)) - DBL_EPSILON/2 (DYN-RMM) -> nextafter(1.0) (NX)
    */
   TESTINST_1_3_F(4, "fnmadd.d fa0, fa1, fa2, fa3", 0xbff0000000000000,
                  0x3ff0000000000001, 0x3ca0000000000000, 0x80, fa0, fa1, fa2,
                  fa3);
   /* -(1.0 * 1.0) - DBL_EPSILON/2 (DYN-RMM) -> -nextafter(1.0) (NX) */
   TESTINST_1_3_F(4, "fnmadd.d fa0, fa1, fa2, fa3", 0x3ff0000000000000,
                  0x3ff0000000000000, 0x3ca0000000000000, 0x80, fa0, fa1, fa2,
                  fa3);

   /* --------------- fadd.d rd, rs1, rs2, rm --------------- */
   /* 2.0 + 1.0 -> 3.0 */
   TESTINST_1_2_F(4, "fadd.d fa0, fa1, fa2", 0x4000000000000000,
                  0x3ff0000000000000, 0x00, fa0, fa1, fa2);
   /* 1.0 + -1.0 -> 0.0 */
   TESTINST_1_2_F(4, "fadd.d fa0, fa1, fa2", 0x3ff0000000000000,
                  0xbff0000000000000, 0x00, fa0, fa1, fa2);
   /* DBL_TRUE_MIN + DBL_TRUE_MIN -> 2*DBL_TRUE_MIN (no UF because exact) */
   TESTINST_1_2_F(4, "fadd.d fa0, fa1, fa2", 0x0000000000000001,
                  0x0000000000000001, 0x00, fa0, fa1, fa2);
   /* DBL_MAX + DBL_MAX -> INFINITY (OF, NX) */
   TESTINST_1_2_F(4, "fadd.d fa0, fa1, fa2", 0x7fefffffffffffff,
                  0x7fefffffffffffff, 0x00, fa0, fa1, fa2);
   /* -DBL_MAX + -DBL_MAX -> -INFINITY (OF, NX) */
   TESTINST_1_2_F(4, "fadd.d fa0, fa1, fa2", 0xffefffffffffffff,
                  0xffefffffffffffff, 0x00, fa0, fa1, fa2);
   /* nextafter(DBL_MIN) + -DBL_MIN -> DBL_TRUE_MIN (no UF because exact) */
   TESTINST_1_2_F(4, "fadd.d fa0, fa1, fa2", 0x0010000000000001,
                  0x8010000000000000, 0x00, fa0, fa1, fa2);
   /* INFINITY + -INFINITY -> qNAN (NV) */
   TESTINST_1_2_F(4, "fadd.d fa0, fa1, fa2", 0x7ff0000000000000,
                  0xfff0000000000000, 0x00, fa0, fa1, fa2);

   /* 1.0 + DBL_EPSILON/2 (RNE) -> 1.0 (NX) */
   TESTINST_1_2_F(4, "fadd.d fa0, fa1, fa2, rne", 0x3ff0000000000000,
                  0x3ca0000000000000, 0x00, fa0, fa1, fa2);
   /* nextafter(1.0) + DBL_EPSILON/2 (RNE) -> 2nextafter(1.0) (NX) */
   TESTINST_1_2_F(4, "fadd.d fa0, fa1, fa2, rne", 0x3ff0000000000001,
                  0x3ca0000000000000, 0x00, fa0, fa1, fa2);
   /* 1.0 + DBL_EPSILON/2 (RTZ) -> 1.0 (NX) */
   TESTINST_1_2_F(4, "fadd.d fa0, fa1, fa2, rtz", 0x3ff0000000000000,
                  0x3ca0000000000000, 0x00, fa0, fa1, fa2);
   /* -1.0 + -DBL_EPSILON/2 (RTZ) -> -1.0 (NX) */
   TESTINST_1_2_F(4, "fadd.d fa0, fa1, fa2, rtz", 0xbff0000000000000,
                  0xbca0000000000000, 0x00, fa0, fa1, fa2);
   /* 1.0 + DBL_EPSILON/2 (RDN) -> 1.0 (NX) */
   TESTINST_1_2_F(4, "fadd.d fa0, fa1, fa2, rdn", 0x3ff0000000000000,
                  0x3ca0000000000000, 0x00, fa0, fa1, fa2);
   /* -1.0 + -DBL_EPSILON/2 (RDN) -> -nextafter(1.0) (NX) */
   TESTINST_1_2_F(4, "fadd.d fa0, fa1, fa2, rdn", 0xbff0000000000000,
                  0xbca0000000000000, 0x00, fa0, fa1, fa2);
   /* 1.0 + DBL_EPSILON/2 (RUP) -> nextafter(1.0) (NX) */
   TESTINST_1_2_F(4, "fadd.d fa0, fa1, fa2, rup", 0x3ff0000000000000,
                  0x3ca0000000000000, 0x00, fa0, fa1, fa2);
   /* -1.0 + -DBL_EPSILON/2 (RUP) -> -1.0 (NX) */
   TESTINST_1_2_F(4, "fadd.d fa0, fa1, fa2, rup", 0xbff0000000000000,
                  0xbca0000000000000, 0x00, fa0, fa1, fa2);
   /* 1.0 + DBL_EPSILON/2 (RMM) -> nextafter(1.0) (NX) */
   TESTINST_1_2_F(4, "fadd.d fa0, fa1, fa2, rmm", 0x3ff0000000000000,
                  0x3ca0000000000000, 0x00, fa0, fa1, fa2);
   /* -1.0 + -DBL_EPSILON/2 (RMM) -> -nextafter(1.0) (NX) */
   TESTINST_1_2_F(4, "fadd.d fa0, fa1, fa2, rmm", 0xbff0000000000000,
                  0xbca0000000000000, 0x00, fa0, fa1, fa2);

   /* 1.0 + DBL_EPSILON/2 (DYN-RNE) -> 1.0 (NX) */
   TESTINST_1_2_F(4, "fadd.d fa0, fa1, fa2", 0x3ff0000000000000,
                  0x3ca0000000000000, 0x00, fa0, fa1, fa2);
   /* nextafter(1.0) + DBL_EPSILON/2 (DYN-RNE) -> 2nextafter(1.0) (NX) */
   TESTINST_1_2_F(4, "fadd.d fa0, fa1, fa2", 0x3ff0000000000001,
                  0x3ca0000000000000, 0x00, fa0, fa1, fa2);
   /* 1.0 + DBL_EPSILON/2 (DYN-RTZ) -> 1.0 (NX) */
   TESTINST_1_2_F(4, "fadd.d fa0, fa1, fa2", 0x3ff0000000000000,
                  0x3ca0000000000000, 0x20, fa0, fa1, fa2);
   /* -1.0 + -DBL_EPSILON/2 (DYN-RTZ) -> -1.0 (NX) */
   TESTINST_1_2_F(4, "fadd.d fa0, fa1, fa2", 0xbff0000000000000,
                  0xbca0000000000000, 0x20, fa0, fa1, fa2);
   /* 1.0 + DBL_EPSILON/2 (DYN-RDN) -> 1.0 (NX) */
   TESTINST_1_2_F(4, "fadd.d fa0, fa1, fa2", 0x3ff0000000000000,
                  0x3ca0000000000000, 0x40, fa0, fa1, fa2);
   /* -1.0 + -DBL_EPSILON/2 (DYN-RDN) -> -nextafter(1.0) (NX) */
   TESTINST_1_2_F(4, "fadd.d fa0, fa1, fa2", 0xbff0000000000000,
                  0xbca0000000000000, 0x40, fa0, fa1, fa2);
   /* 1.0 + DBL_EPSILON/2 (DYN-RUP) -> nextafter(1.0) (NX) */
   TESTINST_1_2_F(4, "fadd.d fa0, fa1, fa2", 0x3ff0000000000000,
                  0x3ca0000000000000, 0x60, fa0, fa1, fa2);
   /* -1.0 + -DBL_EPSILON/2 (DYN-RUP) -> -1.0 (NX) */
   TESTINST_1_2_F(4, "fadd.d fa0, fa1, fa2", 0xbff0000000000000,
                  0xbca0000000000000, 0x60, fa0, fa1, fa2);
   /* 1.0 + DBL_EPSILON/2 (DYN-RMM) -> nextafter(1.0) (NX) */
   TESTINST_1_2_F(4, "fadd.d fa0, fa1, fa2", 0x3ff0000000000000,
                  0x3ca0000000000000, 0x80, fa0, fa1, fa2);
   /* -1.0 + -DBL_EPSILON/2 (DYN-RMM) -> -nextafter(1.0) (NX) */
   TESTINST_1_2_F(4, "fadd.d fa0, fa1, fa2", 0xbff0000000000000,
                  0xbca0000000000000, 0x80, fa0, fa1, fa2);

   /* --------------- fsub.d rd, rs1, rs2, rm --------------- */
   /* 2.0 - 1.0 -> 1.0 */
   TESTINST_1_2_F(4, "fsub.d fa0, fa1, fa2", 0x4000000000000000,
                  0x3ff0000000000000, 0x00, fa0, fa1, fa2);
   /* 1.0 - 1.0 -> 0.0 */
   TESTINST_1_2_F(4, "fsub.d fa0, fa1, fa2", 0x3ff0000000000000,
                  0x3ff0000000000000, 0x00, fa0, fa1, fa2);
   /* DBL_TRUE_MIN - -DBL_TRUE_MIN -> 2*DBL_TRUE_MIN (no UF because exact) */
   TESTINST_1_2_F(4, "fsub.d fa0, fa1, fa2", 0x0000000000000001,
                  0x8000000000000001, 0x00, fa0, fa1, fa2);
   /* DBL_MAX - -DBL_MAX -> INFINITY (OF, NX) */
   TESTINST_1_2_F(4, "fsub.d fa0, fa1, fa2", 0x7fefffffffffffff,
                  0xffefffffffffffff, 0x00, fa0, fa1, fa2);
   /* -DBL_MAX - DBL_MAX -> -INFINITY (OF, NX) */
   TESTINST_1_2_F(4, "fsub.d fa0, fa1, fa2", 0xffefffffffffffff,
                  0x7fefffffffffffff, 0x00, fa0, fa1, fa2);
   /* nextafter(DBL_MIN) - DBL_MIN -> DBL_TRUE_MIN (no UF because exact) */
   TESTINST_1_2_F(4, "fsub.d fa0, fa1, fa2", 0x0010000000000001,
                  0x0010000000000000, 0x00, fa0, fa1, fa2);
   /* INFINITY - INFINITY -> qNAN (NV) */
   TESTINST_1_2_F(4, "fsub.d fa0, fa1, fa2", 0x7ff0000000000000,
                  0x7ff0000000000000, 0x00, fa0, fa1, fa2);

   /* nextafter(1.0) - DBL_EPSILON/2 (RNE) -> 1.0 (NX) */
   TESTINST_1_2_F(4, "fsub.d fa0, fa1, fa2, rne", 0x3ff0000000000001,
                  0x3ca0000000000000, 0x00, fa0, fa1, fa2);
   /* 2nextafter(1.0) - DBL_EPSILON/2 (RNE) -> 2nextafter(1.0) (NX) */
   TESTINST_1_2_F(4, "fsub.d fa0, fa1, fa2, rne", 0x3ff0000000000002,
                  0x3ca0000000000000, 0x00, fa0, fa1, fa2);
   /* nextafter(1.0) - DBL_EPSILON/2 (RTZ) -> 1.0 (NX) */
   TESTINST_1_2_F(4, "fsub.d fa0, fa1, fa2, rtz", 0x3ff0000000000001,
                  0x3ca0000000000000, 0x00, fa0, fa1, fa2);
   /* -1.0 - DBL_EPSILON/2 (RTZ) -> -1.0 (NX) */
   TESTINST_1_2_F(4, "fsub.d fa0, fa1, fa2, rtz", 0xbff0000000000000,
                  0x3ca0000000000000, 0x00, fa0, fa1, fa2);
   /* nextafter(1.0) - DBL_EPSILON/2 (RDN) -> 1.0 (NX) */
   TESTINST_1_2_F(4, "fsub.d fa0, fa1, fa2, rdn", 0x3ff0000000000001,
                  0x3ca0000000000000, 0x00, fa0, fa1, fa2);
   /* -1.0 - DBL_EPSILON/2 (RDN) -> -nextafter(1.0) (NX) */
   TESTINST_1_2_F(4, "fsub.d fa0, fa1, fa2, rdn", 0xbff0000000000000,
                  0x3ca0000000000000, 0x00, fa0, fa1, fa2);
   /* nextafter(1.0) - DBL_EPSILON/2 (RUP) -> nextafter(1.0) (NX) */
   TESTINST_1_2_F(4, "fsub.d fa0, fa1, fa2, rup", 0x3ff0000000000001,
                  0x3ca0000000000000, 0x00, fa0, fa1, fa2);
   /* -1.0 - DBL_EPSILON/2 (RUP) -> -1.0 (NX) */
   TESTINST_1_2_F(4, "fsub.d fa0, fa1, fa2, rup", 0xbff0000000000000,
                  0x3ca0000000000000, 0x00, fa0, fa1, fa2);
   /* nextafter(1.0) - DBL_EPSILON/2 (RMM) -> nextafter(1.0) (NX) */
   TESTINST_1_2_F(4, "fsub.d fa0, fa1, fa2, rmm", 0x3ff0000000000001,
                  0x3ca0000000000000, 0x00, fa0, fa1, fa2);
   /* -1.0 - DBL_EPSILON/2 (RMM) -> -nextafter(1.0) (NX) */
   TESTINST_1_2_F(4, "fsub.d fa0, fa1, fa2, rmm", 0xbff0000000000000,
                  0x3ca0000000000000, 0x00, fa0, fa1, fa2);

   /* nextafter(1.0) - DBL_EPSILON/2 (DYN-RNE) -> 1.0 (NX) */
   TESTINST_1_2_F(4, "fsub.d fa0, fa1, fa2", 0x3ff0000000000001,
                  0x3ca0000000000000, 0x00, fa0, fa1, fa2);
   /* 2nextafter(1.0) - DBL_EPSILON/2 (DYN-RNE) -> 2nextafter(1.0) (NX) */
   TESTINST_1_2_F(4, "fsub.d fa0, fa1, fa2", 0x3ff0000000000002,
                  0x3ca0000000000000, 0x00, fa0, fa1, fa2);
   /* nextafter(1.0) - DBL_EPSILON/2 (DYN-RTZ) -> 1.0 (NX) */
   TESTINST_1_2_F(4, "fsub.d fa0, fa1, fa2", 0x3ff0000000000001,
                  0x3ca0000000000000, 0x20, fa0, fa1, fa2);
   /* -1.0 - DBL_EPSILON/2 (DYN-RTZ) -> -1.0 (NX) */
   TESTINST_1_2_F(4, "fsub.d fa0, fa1, fa2", 0xbff0000000000000,
                  0x3ca0000000000000, 0x20, fa0, fa1, fa2);
   /* nextafter(1.0) - DBL_EPSILON/2 (DYN-RDN) -> 1.0 (NX) */
   TESTINST_1_2_F(4, "fsub.d fa0, fa1, fa2", 0x3ff0000000000001,
                  0x3ca0000000000000, 0x40, fa0, fa1, fa2);
   /* -1.0 - DBL_EPSILON/2 (DYN-RDN) -> -nextafter(1.0) (NX) */
   TESTINST_1_2_F(4, "fsub.d fa0, fa1, fa2", 0xbff0000000000000,
                  0x3ca0000000000000, 0x40, fa0, fa1, fa2);
   /* nextafter(1.0) - DBL_EPSILON/2 (DYN-RUP) -> nextafter(1.0) (NX) */
   TESTINST_1_2_F(4, "fsub.d fa0, fa1, fa2", 0x3ff0000000000001,
                  0x3ca0000000000000, 0x60, fa0, fa1, fa2);
   /* -1.0 - DBL_EPSILON/2 (DYN-RUP) -> -1.0 (NX) */
   TESTINST_1_2_F(4, "fsub.d fa0, fa1, fa2", 0xbff0000000000000,
                  0x3ca0000000000000, 0x60, fa0, fa1, fa2);
   /* nextafter(1.0) - DBL_EPSILON/2 (DYN-RMM) -> nextafter(1.0) (NX) */
   TESTINST_1_2_F(4, "fsub.d fa0, fa1, fa2", 0x3ff0000000000001,
                  0x3ca0000000000000, 0x80, fa0, fa1, fa2);
   /* -1.0 - DBL_EPSILON/2 (DYN-RMM) -> -nextafter(1.0) (NX) */
   TESTINST_1_2_F(4, "fsub.d fa0, fa1, fa2", 0xbff0000000000000,
                  0x3ca0000000000000, 0x80, fa0, fa1, fa2);

   /* --------------- fmul.d rd, rs1, rs2, rm --------------- */
   /* 2.0 * 1.0 -> 2.0 */
   TESTINST_1_2_F(4, "fmul.d fa0, fa1, fa2", 0x4000000000000000,
                  0x3ff0000000000000, 0x00, fa0, fa1, fa2);
   /* 1.0 * 0.0 -> 0.0 */
   TESTINST_1_2_F(4, "fmul.d fa0, fa1, fa2", 0x3ff0000000000000,
                  0x0000000000000000, 0x00, fa0, fa1, fa2);
   /* 2**-537 * 2**-537 -> 2**-1074 aka DBL_TRUE_MIN (no UF because exact) */
   TESTINST_1_2_F(4, "fmul.d fa0, fa1, fa2", 0x1e60000000000000,
                  0x1e60000000000000, 0x00, fa0, fa1, fa2);
   /* DBL_MAX * DBL_MAX -> INFINITY (OF, NX) */
   TESTINST_1_2_F(4, "fmul.d fa0, fa1, fa2", 0x7fefffffffffffff,
                  0x7fefffffffffffff, 0x00, fa0, fa1, fa2);
   /* DBL_MAX * -DBL_MAX -> -INFINITY (OF, NX) */
   TESTINST_1_2_F(4, "fmul.d fa0, fa1, fa2", 0x7fefffffffffffff,
                  0xffefffffffffffff, 0x00, fa0, fa1, fa2);
   /* 1.0 * INFINITY -> INFINITY */
   TESTINST_1_2_F(4, "fmul.d fa0, fa1, fa2", 0x3ff0000000000000,
                  0x7ff0000000000000, 0x00, fa0, fa1, fa2);
   /* 0.0 * INFINITY -> qNAN (NV) */
   TESTINST_1_2_F(4, "fmul.d fa0, fa1, fa2", 0x0000000000000000,
                  0x7ff0000000000000, 0x00, fa0, fa1, fa2);

   /* DBL_TRUE_MIN * 0.5 (RNE) -> 0.0 (UF, NX) */
   TESTINST_1_2_F(4, "fmul.d fa0, fa1, fa2, rne", 0x0000000000000001,
                  0x3fe0000000000000, 0x00, fa0, fa1, fa2);
   /* 3*DBL_TRUE_MIN * 0.5 (RNE) -> 2*DBL_TRUE_MIN (UF, NX) */
   TESTINST_1_2_F(4, "fmul.d fa0, fa1, fa2, rne", 0x0000000000000003,
                  0x3fe0000000000000, 0x00, fa0, fa1, fa2);
   /* DBL_TRUE_MIN * 0.5 (RTZ) -> 0.0 (UF, NX) */
   TESTINST_1_2_F(4, "fmul.d fa0, fa1, fa2, rtz", 0x0000000000000001,
                  0x3fe0000000000000, 0x00, fa0, fa1, fa2);
   /* -DBL_TRUE_MIN * 0.5 (RTZ) -> -0.0 (UF, NX) */
   TESTINST_1_2_F(4, "fmul.d fa0, fa1, fa2, rtz", 0x8000000000000001,
                  0x3fe0000000000000, 0x00, fa0, fa1, fa2);
   /* DBL_TRUE_MIN * 0.5 (RDN) -> 0.0 (UF, NX) */
   TESTINST_1_2_F(4, "fmul.d fa0, fa1, fa2, rdn", 0x0000000000000001,
                  0x3fe0000000000000, 0x00, fa0, fa1, fa2);
   /* -DBL_TRUE_MIN * 0.5 (RDN) -> -DBL_TRUE_MIN (UF, NX) */
   TESTINST_1_2_F(4, "fmul.d fa0, fa1, fa2, rdn", 0x8000000000000001,
                  0x3fe0000000000000, 0x00, fa0, fa1, fa2);
   /* DBL_TRUE_MIN * 0.5 (RUP) -> DBL_TRUE_MIN (UF, NX) */
   TESTINST_1_2_F(4, "fmul.d fa0, fa1, fa2, rup", 0x0000000000000001,
                  0x3fe0000000000000, 0x00, fa0, fa1, fa2);
   /* -DBL_TRUE_MIN * 0.5 (RUP) -> -0.0 (UF, NX) */
   TESTINST_1_2_F(4, "fmul.d fa0, fa1, fa2, rup", 0x8000000000000001,
                  0x3fe0000000000000, 0x00, fa0, fa1, fa2);
   /* DBL_TRUE_MIN * 0.5 (RMM) -> DBL_TRUE_MIN (UF, NX) */
   TESTINST_1_2_F(4, "fmul.d fa0, fa1, fa2, rmm", 0x0000000000000001,
                  0x3fe0000000000000, 0x00, fa0, fa1, fa2);
   /* -DBL_TRUE_MIN * 0.5 (RMM) -> -DBL_TRUE_MIN (UF, NX) */
   TESTINST_1_2_F(4, "fmul.d fa0, fa1, fa2, rmm", 0x8000000000000001,
                  0x3fe0000000000000, 0x00, fa0, fa1, fa2);

   /* DBL_TRUE_MIN * 0.5 (DYN-RNE) -> 0.0 (UF, NX) */
   TESTINST_1_2_F(4, "fmul.d fa0, fa1, fa2", 0x0000000000000001,
                  0x3fe0000000000000, 0x00, fa0, fa1, fa2);
   /* 3*DBL_TRUE_MIN * 0.5 (DYN-RNE) -> 2*DBL_TRUE_MIN (UF, NX) */
   TESTINST_1_2_F(4, "fmul.d fa0, fa1, fa2", 0x0000000000000003,
                  0x3fe0000000000000, 0x00, fa0, fa1, fa2);
   /* DBL_TRUE_MIN * 0.5 (DYN-RTZ) -> 0.0 (UF, NX) */
   TESTINST_1_2_F(4, "fmul.d fa0, fa1, fa2", 0x0000000000000001,
                  0x3fe0000000000000, 0x20, fa0, fa1, fa2);
   /* -DBL_TRUE_MIN * 0.5 (DYN-RTZ) -> -0.0 (UF, NX) */
   TESTINST_1_2_F(4, "fmul.d fa0, fa1, fa2", 0x8000000000000001,
                  0x3fe0000000000000, 0x20, fa0, fa1, fa2);
   /* DBL_TRUE_MIN * 0.5 (DYN-RDN) -> 0.0 (UF, NX) */
   TESTINST_1_2_F(4, "fmul.d fa0, fa1, fa2", 0x0000000000000001,
                  0x3fe0000000000000, 0x40, fa0, fa1, fa2);
   /* -DBL_TRUE_MIN * 0.5 (DYN-RDN) -> -DBL_TRUE_MIN (UF, NX) */
   TESTINST_1_2_F(4, "fmul.d fa0, fa1, fa2", 0x8000000000000001,
                  0x3fe0000000000000, 0x40, fa0, fa1, fa2);
   /* DBL_TRUE_MIN * 0.5 (DYN-RUP) -> DBL_TRUE_MIN (UF, NX) */
   TESTINST_1_2_F(4, "fmul.d fa0, fa1, fa2", 0x0000000000000001,
                  0x3fe0000000000000, 0x60, fa0, fa1, fa2);
   /* -DBL_TRUE_MIN * 0.5 (DYN-RUP) -> -0.0 (UF, NX) */
   TESTINST_1_2_F(4, "fmul.d fa0, fa1, fa2", 0x8000000000000001,
                  0x3fe0000000000000, 0x60, fa0, fa1, fa2);
   /* DBL_TRUE_MIN * 0.5 (DYN-RMM) -> DBL_TRUE_MIN (UF, NX) */
   TESTINST_1_2_F(4, "fmul.d fa0, fa1, fa2", 0x0000000000000001,
                  0x3fe0000000000000, 0x80, fa0, fa1, fa2);
   /* -DBL_TRUE_MIN * 0.5 (DYN-RMM) -> -DBL_TRUE_MIN (UF, NX) */
   TESTINST_1_2_F(4, "fmul.d fa0, fa1, fa2", 0x8000000000000001,
                  0x3fe0000000000000, 0x80, fa0, fa1, fa2);

   /* --------------- fdiv.d rd, rs1, rs2, rm --------------- */
   /* 2.0 / 1.0 -> 2.0 */
   TESTINST_1_2_F(4, "fdiv.d fa0, fa1, fa2", 0x4000000000000000,
                  0x3ff0000000000000, 0x00, fa0, fa1, fa2);
   /* 0.0 / 1.0 -> 0.0 */
   TESTINST_1_2_F(4, "fdiv.d fa0, fa1, fa2", 0x0000000000000000,
                  0x3ff0000000000000, 0x00, fa0, fa1, fa2);
   /* 1.0 / 2**1023 -> 1**-1023 (no UF because exact) */
   TESTINST_1_2_F(4, "fdiv.d fa0, fa1, fa2", 0x3ff0000000000000,
                  0x7fe0000000000000, 0x00, fa0, fa1, fa2);
   /* DBL_MAX / 0.5 -> INFINITY (OF, NX) */
   TESTINST_1_2_F(4, "fdiv.d fa0, fa1, fa2", 0x7fefffffffffffff,
                  0x3fe0000000000000, 0x00, fa0, fa1, fa2);
   /* DBL_MAX / -0.5 -> -INFINITY (OF, NX) */
   TESTINST_1_2_F(4, "fdiv.d fa0, fa1, fa2", 0x7fefffffffffffff,
                  0xbfe0000000000000, 0x00, fa0, fa1, fa2);
   /* 1.0 / INFINITY -> 0.0 */
   TESTINST_1_2_F(4, "fdiv.d fa0, fa1, fa2", 0x3ff0000000000000,
                  0x7ff0000000000000, 0x00, fa0, fa1, fa2);
   /* 1.0 / 0.0 -> INFINITY (DZ) */
   TESTINST_1_2_F(4, "fdiv.d fa0, fa1, fa2", 0x3ff0000000000000,
                  0x0000000000000000, 0x00, fa0, fa1, fa2);
   /* 0.0 / 0.0 -> qNAN (NV) */
   TESTINST_1_2_F(4, "fdiv.d fa0, fa1, fa2", 0x0000000000000000,
                  0x0000000000000000, 0x00, fa0, fa1, fa2);

   /* DBL_TRUE_MIN / 2.0 (RNE) -> 0.0 (UF, NX) */
   TESTINST_1_2_F(4, "fdiv.d fa0, fa1, fa2, rne", 0x0000000000000001,
                  0x4000000000000000, 0x00, fa0, fa1, fa2);
   /* 3*DBL_TRUE_MIN / 2.0 (RNE) -> 2*DBL_TRUE_MIN (UF, NX) */
   TESTINST_1_2_F(4, "fdiv.d fa0, fa1, fa2, rne", 0x0000000000000003,
                  0x4000000000000000, 0x00, fa0, fa1, fa2);
   /* DBL_TRUE_MIN / 2.0 (RTZ) -> 0.0 (UF, NX) */
   TESTINST_1_2_F(4, "fdiv.d fa0, fa1, fa2, rtz", 0x0000000000000001,
                  0x4000000000000000, 0x00, fa0, fa1, fa2);
   /* -DBL_TRUE_MIN / 2.0 (RTZ) -> -0.0 (UF, NX) */
   TESTINST_1_2_F(4, "fdiv.d fa0, fa1, fa2, rtz", 0x8000000000000001,
                  0x4000000000000000, 0x00, fa0, fa1, fa2);
   /* DBL_TRUE_MIN / 2.0 (RDN) -> 0.0 (UF, NX) */
   TESTINST_1_2_F(4, "fdiv.d fa0, fa1, fa2, rdn", 0x0000000000000001,
                  0x4000000000000000, 0x00, fa0, fa1, fa2);
   /* -DBL_TRUE_MIN / 2.0 (RDN) -> -DBL_TRUE_MIN (UF, NX) */
   TESTINST_1_2_F(4, "fdiv.d fa0, fa1, fa2, rdn", 0x8000000000000001,
                  0x4000000000000000, 0x00, fa0, fa1, fa2);
   /* DBL_TRUE_MIN / 2.0 (RUP) -> DBL_TRUE_MIN (UF, NX) */
   TESTINST_1_2_F(4, "fdiv.d fa0, fa1, fa2, rup", 0x0000000000000001,
                  0x4000000000000000, 0x00, fa0, fa1, fa2);
   /* -DBL_TRUE_MIN / 2.0 (RUP) -> -0.0 (UF, NX) */
   TESTINST_1_2_F(4, "fdiv.d fa0, fa1, fa2, rup", 0x8000000000000001,
                  0x4000000000000000, 0x00, fa0, fa1, fa2);
   /* DBL_TRUE_MIN / 2.0 (RMM) -> DBL_TRUE_MIN (UF, NX) */
   TESTINST_1_2_F(4, "fdiv.d fa0, fa1, fa2, rmm", 0x0000000000000001,
                  0x4000000000000000, 0x00, fa0, fa1, fa2);
   /* -DBL_TRUE_MIN / 2.0 (RMM) -> -DBL_TRUE_MIN (UF, NX) */
   TESTINST_1_2_F(4, "fdiv.d fa0, fa1, fa2, rmm", 0x8000000000000001,
                  0x4000000000000000, 0x00, fa0, fa1, fa2);

   /* DBL_TRUE_MIN / 2.0 (DYN-RNE) -> 0.0 (UF, NX) */
   TESTINST_1_2_F(4, "fdiv.d fa0, fa1, fa2", 0x0000000000000001,
                  0x4000000000000000, 0x00, fa0, fa1, fa2);
   /* 3*DBL_TRUE_MIN / 2.0 (DYN-RNE) -> 2*DBL_TRUE_MIN (UF, NX) */
   TESTINST_1_2_F(4, "fdiv.d fa0, fa1, fa2", 0x0000000000000003,
                  0x4000000000000000, 0x00, fa0, fa1, fa2);
   /* DBL_TRUE_MIN / 2.0 (DYN-RTZ) -> 0.0 (UF, NX) */
   TESTINST_1_2_F(4, "fdiv.d fa0, fa1, fa2", 0x0000000000000001,
                  0x4000000000000000, 0x20, fa0, fa1, fa2);
   /* -DBL_TRUE_MIN / 2.0 (DYN-RTZ) -> -0.0 (UF, NX) */
   TESTINST_1_2_F(4, "fdiv.d fa0, fa1, fa2", 0x8000000000000001,
                  0x4000000000000000, 0x20, fa0, fa1, fa2);
   /* DBL_TRUE_MIN / 2.0 (DYN-RDN) -> 0.0 (UF, NX) */
   TESTINST_1_2_F(4, "fdiv.d fa0, fa1, fa2", 0x0000000000000001,
                  0x4000000000000000, 0x40, fa0, fa1, fa2);
   /* -DBL_TRUE_MIN / 2.0 (DYN-RDN) -> -DBL_TRUE_MIN (UF, NX) */
   TESTINST_1_2_F(4, "fdiv.d fa0, fa1, fa2", 0x8000000000000001,
                  0x4000000000000000, 0x40, fa0, fa1, fa2);
   /* DBL_TRUE_MIN / 2.0 (DYN-RUP) -> DBL_TRUE_MIN (UF, NX) */
   TESTINST_1_2_F(4, "fdiv.d fa0, fa1, fa2", 0x0000000000000001,
                  0x4000000000000000, 0x60, fa0, fa1, fa2);
   /* -DBL_TRUE_MIN / 2.0 (DYN-RUP) -> -0.0 (UF, NX) */
   TESTINST_1_2_F(4, "fdiv.d fa0, fa1, fa2", 0x8000000000000001,
                  0x4000000000000000, 0x60, fa0, fa1, fa2);
   /* DBL_TRUE_MIN / 2.0 (DYN-RMM) -> DBL_TRUE_MIN (UF, NX) */
   TESTINST_1_2_F(4, "fdiv.d fa0, fa1, fa2", 0x0000000000000001,
                  0x4000000000000000, 0x80, fa0, fa1, fa2);
   /* -DBL_TRUE_MIN / 2.0 (DYN-RMM) -> -DBL_TRUE_MIN (UF, NX) */
   TESTINST_1_2_F(4, "fdiv.d fa0, fa1, fa2", 0x8000000000000001,
                  0x4000000000000000, 0x80, fa0, fa1, fa2);

   /* ----------------- fsqrt.d rd, rs1, rm ----------------- */
   /* sqrt(0.0) -> 0.0 */
   TESTINST_1_1_F(4, "fsqrt.d fa0, fa1", 0x0000000000000000, 0x00, fa0, fa1);
   /* sqrt(INFINITY) -> INFINITY */
   TESTINST_1_1_F(4, "fsqrt.d fa0, fa1", 0x7ff0000000000000, 0x00, fa0, fa1);
   /* sqrt(DBL_TRUE_MIN) -> 2**-537 */
   TESTINST_1_1_F(4, "fsqrt.d fa0, fa1", 0x0000000000000001, 0x00, fa0, fa1);
   /* sqrt(qNAN) -> qNAN */
   TESTINST_1_1_F(4, "fsqrt.d fa0, fa1", 0x7ff8000000000000, 0x00, fa0, fa1);
   /* sqrt(-1.0) -> qNAN (NV) */
   TESTINST_1_1_F(4, "fsqrt.d fa0, fa1", 0xbff0000000000000, 0x00, fa0, fa1);

   /* sqrt(nextafter(1.0)) (RNE) -> 1.0 (NX) */
   TESTINST_1_1_F(4, "fsqrt.d fa0, fa1, rne", 0x3ff0000000000001, 0x00, fa0,
                  fa1);
   /* sqrt(2nextafter(1.0)) (RNE) -> nextafter(1.0) (NX) */
   TESTINST_1_1_F(4, "fsqrt.d fa0, fa1, rne", 0x3ff0000000000002, 0x00, fa0,
                  fa1);
   /* sqrt(nextafter(1.0)) (RTZ) -> 1.0 (NX) */
   TESTINST_1_1_F(4, "fsqrt.d fa0, fa1, rtz", 0x3ff0000000000001, 0x00, fa0,
                  fa1);
   /* sqrt(2nextafter(1.0)) (RTZ) -> 1.0 (NX) */
   TESTINST_1_1_F(4, "fsqrt.d fa0, fa1, rtz", 0x3ff0000000000002, 0x00, fa0,
                  fa1);
   /* sqrt(nextafter(1.0)) (RDN) -> 1.0 (NX) */
   TESTINST_1_1_F(4, "fsqrt.d fa0, fa1, rdn", 0x3ff0000000000001, 0x00, fa0,
                  fa1);
   /* sqrt(2nextafter(1.0)) (RDN) -> 1.0 (NX) */
   TESTINST_1_1_F(4, "fsqrt.d fa0, fa1, rdn", 0x3ff0000000000002, 0x00, fa0,
                  fa1);
   /* sqrt(nextafter(1.0)) (RUP) -> nextafter(1.0) (NX) */
   TESTINST_1_1_F(4, "fsqrt.d fa0, fa1, rup", 0x3ff0000000000001, 0x00, fa0,
                  fa1);
   /* sqrt(2nextafter(1.0)) (RUP) -> nextafter(1.0) (NX) */
   TESTINST_1_1_F(4, "fsqrt.d fa0, fa1, rup", 0x3ff0000000000002, 0x00, fa0,
                  fa1);
   /* sqrt(nextafter(1.0)) (RMM) -> 1.0 (NX) */
   TESTINST_1_1_F(4, "fsqrt.d fa0, fa1, rmm", 0x3ff0000000000001, 0x00, fa0,
                  fa1);
   /* sqrt(2nextafter(1.0)) (RMM) -> nextafter(1.0) (NX) */
   TESTINST_1_1_F(4, "fsqrt.d fa0, fa1, rmm", 0x3ff0000000000002, 0x00, fa0,
                  fa1);

   /* sqrt(nextafter(1.0)) (DYN-RNE) -> 1.0 (NX) */
   TESTINST_1_1_F(4, "fsqrt.d fa0, fa1", 0x3ff0000000000001, 0x00, fa0, fa1);
   /* sqrt(2nextafter(1.0)) (DYN-RNE) -> nextafter(1.0) (NX) */
   TESTINST_1_1_F(4, "fsqrt.d fa0, fa1", 0x3ff0000000000002, 0x00, fa0, fa1);
   /* sqrt(nextafter(1.0)) (DYN-RTZ) -> 1.0 (NX) */
   TESTINST_1_1_F(4, "fsqrt.d fa0, fa1", 0x3ff0000000000001, 0x20, fa0, fa1);
   /* sqrt(2nextafter(1.0)) (DYN-RTZ) -> 1.0 (NX) */
   TESTINST_1_1_F(4, "fsqrt.d fa0, fa1", 0x3ff0000000000002, 0x20, fa0, fa1);
   /* sqrt(nextafter(1.0)) (DYN-RDN) -> 1.0 (NX) */
   TESTINST_1_1_F(4, "fsqrt.d fa0, fa1", 0x3ff0000000000001, 0x40, fa0, fa1);
   /* sqrt(2nextafter(1.0)) (DYN-RDN) -> 1.0 (NX) */
   TESTINST_1_1_F(4, "fsqrt.d fa0, fa1", 0x3ff0000000000002, 0x40, fa0, fa1);
   /* sqrt(nextafter(1.0)) (DYN-RUP) -> nextafter(1.0) (NX) */
   TESTINST_1_1_F(4, "fsqrt.d fa0, fa1", 0x3ff0000000000001, 0x60, fa0, fa1);
   /* sqrt(2nextafter(1.0)) (DYN-RUP) -> nextafter(1.0) (NX) */
   TESTINST_1_1_F(4, "fsqrt.d fa0, fa1", 0x3ff0000000000002, 0x60, fa0, fa1);
   /* sqrt(nextafter(1.0)) (DYN-RMM) -> 1.0 (NX) */
   TESTINST_1_1_F(4, "fsqrt.d fa0, fa1", 0x3ff0000000000001, 0x80, fa0, fa1);
   /* sqrt(2nextafter(1.0)) (DYN-RMM) -> nextafter(1.0) (NX) */
   TESTINST_1_1_F(4, "fsqrt.d fa0, fa1", 0x3ff0000000000002, 0x80, fa0, fa1);

   /* ---------------- fsgnj.d rd, rs1, rs2 ----------------- */
   /* fmv.d rd, rs1 */
   TESTINST_1_2_F(4, "fsgnj.d fa0, fa1, fa1", 0x3ff0000000000000,
                  0x3ff0000000000000, 0x00, fa0, fa1, fa1);
   TESTINST_1_2_F(4, "fsgnj.d fa0, fa1, fa1", 0xbff0000000000000,
                  0xbff0000000000000, 0x00, fa0, fa1, fa1);

   /* fsgnj(1.0, +) -> 1.0 */
   TESTINST_1_2_F(4, "fsgnj.d fa0, fa1, fa2", 0x3ff0000000000000,
                  0x7fffffffffffffff, 0x00, fa0, fa1, fa2);
   /* fsgnj(1.0, -) -> -1.0 */
   TESTINST_1_2_F(4, "fsgnj.d fa0, fa1, fa2", 0x3ff0000000000000,
                  0x8000000000000000, 0x00, fa0, fa1, fa2);
   /* fsgnj(-1.0, +) -> 1.0 */
   TESTINST_1_2_F(4, "fsgnj.d fa0, fa1, fa2", 0xbff0000000000000,
                  0x7fffffffffffffff, 0x00, fa0, fa1, fa2);
   /* fsgnj(-1.0, -) -> -1.0 */
   TESTINST_1_2_F(4, "fsgnj.d fa0, fa1, fa2", 0xbff0000000000000,
                  0x8000000000000000, 0x00, fa0, fa1, fa2);

   /* ---------------- fsgnjn.d rd, rs1, rs2 ---------------- */
   /* fneg.d rd, rs1 */
   TESTINST_1_2_F(4, "fsgnjn.d fa0, fa1, fa1", 0x3ff0000000000000,
                  0x3ff0000000000000, 0x00, fa0, fa1, fa1);
   TESTINST_1_2_F(4, "fsgnjn.d fa0, fa1, fa1", 0xbff0000000000000,
                  0xbff0000000000000, 0x00, fa0, fa1, fa1);

   /* fsgnjn(1.0, +) -> -1.0 */
   TESTINST_1_2_F(4, "fsgnjn.d fa0, fa1, fa2", 0x3ff0000000000000,
                  0x7fffffffffffffff, 0x00, fa0, fa1, fa2);
   /* fsgnjn(1.0, -) -> 1.0 */
   TESTINST_1_2_F(4, "fsgnjn.d fa0, fa1, fa2", 0x3ff0000000000000,
                  0x8000000000000000, 0x00, fa0, fa1, fa2);
   /* fsgnjn(-1.0, +) -> -1.0 */
   TESTINST_1_2_F(4, "fsgnjn.d fa0, fa1, fa2", 0xbff0000000000000,
                  0x7fffffffffffffff, 0x00, fa0, fa1, fa2);
   /* fsgnjn(-1.0, -) -> 1.0 */
   TESTINST_1_2_F(4, "fsgnjn.d fa0, fa1, fa2", 0xbff0000000000000,
                  0x8000000000000000, 0x00, fa0, fa1, fa2);

   /* ---------------- fsgnjx.d rd, rs1, rs2 ---------------- */
   /* fabs.d rd, rs1 */
   TESTINST_1_2_F(4, "fsgnjx.d fa0, fa1, fa1", 0x3ff0000000000000,
                  0x3ff0000000000000, 0x00, fa0, fa1, fa1);
   TESTINST_1_2_F(4, "fsgnjx.d fa0, fa1, fa1", 0xbff0000000000000,
                  0xbff0000000000000, 0x00, fa0, fa1, fa1);

   /* fsgnjx(1.0, +) -> 1.0 */
   TESTINST_1_2_F(4, "fsgnjx.d fa0, fa1, fa2", 0x3ff0000000000000,
                  0x7fffffffffffffff, 0x00, fa0, fa1, fa2);
   /* fsgnjx(1.0, -) -> -1.0 */
   TESTINST_1_2_F(4, "fsgnjx.d fa0, fa1, fa2", 0x3ff0000000000000,
                  0x8000000000000000, 0x00, fa0, fa1, fa2);
   /* fsgnjx(-1.0, +) -> -1.0 */
   TESTINST_1_2_F(4, "fsgnjx.d fa0, fa1, fa2", 0xbff0000000000000,
                  0x7fffffffffffffff, 0x00, fa0, fa1, fa2);
   /* fsgnjx(-1.0, -) -> 1.0 */
   TESTINST_1_2_F(4, "fsgnjx.d fa0, fa1, fa2", 0xbff0000000000000,
                  0x8000000000000000, 0x00, fa0, fa1, fa2);

   /* ----------------- fmin.d rd, rs1, rs2 ----------------- */
   /* min(0.0, 1.0) -> 0.0 */
   TESTINST_1_2_F(4, "fmin.d fa0, fa1, fa2", 0x0000000000000000,
                  0x3ff0000000000000, 0x00, fa0, fa1, fa2);
   /* min(0.0, -0.0) -> -0.0 */
   TESTINST_1_2_F(4, "fmin.d fa0, fa1, fa2", 0x0000000000000000,
                  0x8000000000000000, 0x00, fa0, fa1, fa2);
   /* min(-0.0, 0.0) -> -0.0 */
   TESTINST_1_2_F(4, "fmin.d fa0, fa1, fa2", 0x8000000000000000,
                  0x0000000000000000, 0x00, fa0, fa1, fa2);
   /* min(INFINITY, INFINITY) -> INFINITY */
   TESTINST_1_2_F(4, "fmin.d fa0, fa1, fa2", 0x7ff0000000000000,
                  0x7ff0000000000000, 0x00, fa0, fa1, fa2);
   /* min(0.0, qNAN) -> 0.0 */
   TESTINST_1_2_F(4, "fmin.d fa0, fa1, fa2", 0x0000000000000000,
                  0x7ff8000000000000, 0x00, fa0, fa1, fa2);
   /* min(0.0, sNAN) -> 0.0 (NV) */
   TESTINST_1_2_F(4, "fmin.d fa0, fa1, fa2", 0x0000000000000000,
                  0x7ff4000000000000, 0x00, fa0, fa1, fa2);

   /* ----------------- fmax.d rd, rs1, rs2 ----------------- */
   /* max(0.0, 1.0) -> 1.0 */
   TESTINST_1_2_F(4, "fmax.d fa0, fa1, fa2", 0x0000000000000000,
                  0x3ff0000000000000, 0x00, fa0, fa1, fa2);
   /* max(0.0, -0.0) -> 0.0 */
   TESTINST_1_2_F(4, "fmax.d fa0, fa1, fa2", 0x0000000000000000,
                  0x8000000000000000, 0x00, fa0, fa1, fa2);
   /* max(-0.0, 0.0) -> 0.0 */
   TESTINST_1_2_F(4, "fmax.d fa0, fa1, fa2", 0x8000000000000000,
                  0x0000000000000000, 0x00, fa0, fa1, fa2);
   /* max(INFINITY, INFINITY) -> INFINITY */
   TESTINST_1_2_F(4, "fmax.d fa0, fa1, fa2", 0x7ff0000000000000,
                  0x7ff0000000000000, 0x00, fa0, fa1, fa2);
   /* max(0.0, qNAN) -> 0.0 */
   TESTINST_1_2_F(4, "fmax.d fa0, fa1, fa2", 0x0000000000000000,
                  0x7ff8000000000000, 0x00, fa0, fa1, fa2);
   /* max(0.0, sNAN) -> 0.0 (NV) */
   TESTINST_1_2_F(4, "fmax.d fa0, fa1, fa2", 0x0000000000000000,
                  0x7ff4000000000000, 0x00, fa0, fa1, fa2);

   /* ---------------- fcvt.s.d rd, rs1, rm ----------------- */
   /* 0.0 -> 0.0 */
   TESTINST_1_1_F(4, "fcvt.s.d fa0, fa1", 0x0000000000000000, 0x00, fa0, fa1);
   /* DBL_TRUE_MIN -> 0.0 (UF, NX) */
   TESTINST_1_1_F(4, "fcvt.s.d fa0, fa1", 0x0000000000000001, 0x00, fa0, fa1);
   /* INFINITY -> INFINITY */
   TESTINST_1_1_F(4, "fcvt.s.d fa0, fa1", 0x7ff0000000000000, 0x00, fa0, fa1);
   /* qNAN -> qNAN */
   TESTINST_1_1_F(4, "fcvt.s.d fa0, fa1", 0x7ff8000000000000, 0x00, fa0, fa1);
   /* FLT_MAX -> FLT_MAX */
   TESTINST_1_1_F(4, "fcvt.s.d fa0, fa1", 0x47efffffe0000000, 0x00, fa0, fa1);
   /* -FLT_MAX -> -FLT_MAX */
   TESTINST_1_1_F(4, "fcvt.s.d fa0, fa1", 0xc7efffffe0000000, 0x00, fa0, fa1);
   /* nextafter(FLT_MAX) -> FLT_MAX (NX) */
   TESTINST_1_1_F(4, "fcvt.s.d fa0, fa1", 0x47efffffe0000001, 0x00, fa0, fa1);
   /* -nextafter(FLT_MAX) -> -FLT_MAX (NX) */
   TESTINST_1_1_F(4, "fcvt.s.d fa0, fa1", 0xc7efffffe0000001, 0x00, fa0, fa1);
   /* DBL_MAX -> FLT_MAX (OF, NX) */
   TESTINST_1_1_F(4, "fcvt.s.d fa0, fa1", 0x7fefffffffffffff, 0x00, fa0, fa1);

   /* 1.0 + FLT_EPSILON/2 (RNE) -> 1.0 (NX) */
   TESTINST_1_1_F(4, "fcvt.s.d fa0, fa1, rne", 0x3ff0000010000000, 0x00, fa0,
                  fa1);
   /* nextafterf(1.0) + FLT_EPSILON/2 (RNE) -> 2nextafterf(1.0) (NX) */
   TESTINST_1_1_F(4, "fcvt.s.d fa0, fa1, rne", 0x3ff0000030000000, 0x00, fa0,
                  fa1);
   /* 1.0 + FLT_EPSILON/2 (RTZ) -> 1.0 (NX) */
   TESTINST_1_1_F(4, "fcvt.s.d fa0, fa1, rtz", 0x3ff0000010000000, 0x00, fa0,
                  fa1);
   /* -1.0 + -FLT_EPSILON/2 (RTZ) -> -1.0 (NX) */
   TESTINST_1_1_F(4, "fcvt.s.d fa0, fa1, rtz", 0xbff0000010000000, 0x00, fa0,
                  fa1);
   /* 1.0 + FLT_EPSILON/2 (RDN) -> 1.0 (NX) */
   TESTINST_1_1_F(4, "fcvt.s.d fa0, fa1, rdn", 0x3ff0000010000000, 0x00, fa0,
                  fa1);
   /* -1.0 + -FLT_EPSILON/2 (RDN) -> -nextafterf(1.0) (NX) */
   TESTINST_1_1_F(4, "fcvt.s.d fa0, fa1, rdn", 0xbff0000010000000, 0x00, fa0,
                  fa1);
   /* 1.0 + FLT_EPSILON/2 (RUP) -> nextafterf(1.0) (NX) */
   TESTINST_1_1_F(4, "fcvt.s.d fa0, fa1, rup", 0x3ff0000010000000, 0x00, fa0,
                  fa1);
   /* -1.0 + -FLT_EPSILON/2 (RUP) -> -1.0 (NX) */
   TESTINST_1_1_F(4, "fcvt.s.d fa0, fa1, rup", 0xbff0000010000000, 0x00, fa0,
                  fa1);
   /* 1.0 + FLT_EPSILON/2 (RMM) -> nextafterf(1.0) (NX) */
   TESTINST_1_1_F(4, "fcvt.s.d fa0, fa1, rmm", 0x3ff0000010000000, 0x00, fa0,
                  fa1);
   /* -1.0 + -FLT_EPSILON/2 (RMM) -> -nextafterf(1.0) (NX) */
   TESTINST_1_1_F(4, "fcvt.s.d fa0, fa1, rmm", 0xbff0000010000000, 0x00, fa0,
                  fa1);

   /* 1.0 + FLT_EPSILON/2 (DYN-RNE) -> 1.0 (NX) */
   TESTINST_1_1_F(4, "fcvt.s.d fa0, fa1", 0x3ff0000010000000, 0x00, fa0, fa1);
   /* nextafterf(1.0) + FLT_EPSILON/2 (DYN-RNE) -> 2nextafterf(1.0) (NX) */
   TESTINST_1_1_F(4, "fcvt.s.d fa0, fa1", 0x3ff0000030000000, 0x00, fa0, fa1);
   /* 1.0 + FLT_EPSILON/2 (DYN-RTZ) -> 1.0 (NX) */
   TESTINST_1_1_F(4, "fcvt.s.d fa0, fa1", 0x3ff0000010000000, 0x20, fa0, fa1);
   /* -1.0 + -FLT_EPSILON/2 (DYN-RTZ) -> -1.0 (NX) */
   TESTINST_1_1_F(4, "fcvt.s.d fa0, fa1", 0xbff0000010000000, 0x20, fa0, fa1);
   /* 1.0 + FLT_EPSILON/2 (DYN-RDN) -> 1.0 (NX) */
   TESTINST_1_1_F(4, "fcvt.s.d fa0, fa1", 0x3ff0000010000000, 0x40, fa0, fa1);
   /* -1.0 + -FLT_EPSILON/2 (DYN-RDN) -> -nextafterf(1.0) (NX) */
   TESTINST_1_1_F(4, "fcvt.s.d fa0, fa1", 0xbff0000010000000, 0x40, fa0, fa1);
   /* 1.0 + FLT_EPSILON/2 (DYN-RUP) -> nextafterf(1.0) (NX) */
   TESTINST_1_1_F(4, "fcvt.s.d fa0, fa1", 0x3ff0000010000000, 0x60, fa0, fa1);
   /* -1.0 + -FLT_EPSILON/2 (DYN-RUP) -> -1.0 (NX) */
   TESTINST_1_1_F(4, "fcvt.s.d fa0, fa1", 0xbff0000010000000, 0x60, fa0, fa1);
   /* 1.0 + FLT_EPSILON/2 (DYN-RMM) -> nextafterf(1.0) (NX) */
   TESTINST_1_1_F(4, "fcvt.s.d fa0, fa1", 0x3ff0000010000000, 0x80, fa0, fa1);
   /* -1.0 + -FLT_EPSILON/2 (DYN-RMM) -> -nextafterf(1.0) (NX) */
   TESTINST_1_1_F(4, "fcvt.s.d fa0, fa1", 0xbff0000010000000, 0x80, fa0, fa1);

   /* ---------------- fcvt.d.s rd, rs1, rm ----------------- */
   /* 0.0 -> 0.0 */
   TESTINST_1_1_F(4, "fcvt.d.s fa0, fa1", 0xffffffff00000000, 0x00, fa0, fa1);
   /* FLT_TRUE_MIN -> FLT_TRUE_MIN */
   TESTINST_1_1_F(4, "fcvt.d.s fa0, fa1", 0xffffffff00000001, 0x00, fa0, fa1);
   /* INFINITY -> INFINITY */
   TESTINST_1_1_F(4, "fcvt.d.s fa0, fa1", 0xffffffff7f800000, 0x00, fa0, fa1);
   /* qNAN -> qNAN */
   TESTINST_1_1_F(4, "fcvt.d.s fa0, fa1", 0xffffffff7fc00000, 0x00, fa0, fa1);
   /* FLT_MAX -> FLT_MAX */
   TESTINST_1_1_F(4, "fcvt.d.s fa0, fa1", 0xffffffff7f7fffff, 0x00, fa0, fa1);
   /* -FLT_MAX -> -FLT_MAX */
   TESTINST_1_1_F(4, "fcvt.d.s fa0, fa1", 0xffffffffff7fffff, 0x00, fa0, fa1);
#if 0
   /* TODO Implement correctly. */
   /* non-NaN-boxed 0.0 -> qNaN */
   TESTINST_1_1_F(4, "fcvt.d.s fa0, fa1", 0x0000000000000000, 0x00, fa0, fa1);
#endif

   /* ----------------- feq.d rd, rs1, rs2 ------------------ */
   /* 0.0 == 1.0 -> 0 */
   TESTINST_1_2_FCMP(4, "feq.d a0, fa0, fa1", 0x0000000000000000,
                     0x3ff0000000000000, 0x00, a0, fa0, fa1);
   /* 0.0 == 0.0 -> 1 */
   TESTINST_1_2_FCMP(4, "feq.d a0, fa0, fa1", 0x0000000000000000,
                     0x0000000000000000, 0x00, a0, fa0, fa1);
   /* 0.0 == -0.0 -> 1 */
   TESTINST_1_2_FCMP(4, "feq.d a0, fa0, fa1", 0x0000000000000000,
                     0x8000000000000000, 0x00, a0, fa0, fa1);
   /* -0.0 == 0.0 -> 1 */
   TESTINST_1_2_FCMP(4, "feq.d a0, fa0, fa1", 0x8000000000000000,
                     0x0000000000000000, 0x00, a0, fa0, fa1);
   /* INFINITY == INFINITY -> 1 */
   TESTINST_1_2_FCMP(4, "feq.d a0, fa0, fa1", 0x7ff0000000000000,
                     0x7ff0000000000000, 0x00, a0, fa0, fa1);
   /* 0.0 == qNAN -> 0 */
   TESTINST_1_2_FCMP(4, "feq.d a0, fa0, fa1", 0x0000000000000000,
                     0x7ff8000000000000, 0x00, a0, fa0, fa1);
   /* 0.0 == sNAN -> 0 (NV) */
   TESTINST_1_2_FCMP(4, "feq.d a0, fa0, fa1", 0x0000000000000000,
                     0x7ff4000000000000, 0x00, a0, fa0, fa1);

   /* sNAN == sNAN (rd=zero) -> 0 (NV) */
   TESTINST_1_2_FCMP(4, "feq.d zero, fa0, fa1", 0x7ff4000000000000,
                     0x7ff4000000000000, 0x00, zero, fa0, fa1);

   /* ----------------- flt.d rd, rs1, rs2 ------------------ */
   /* 0.0 < 0.0 -> 0 */
   TESTINST_1_2_FCMP(4, "flt.d a0, fa0, fa1", 0x0000000000000000,
                     0x0000000000000000, 0x00, a0, fa0, fa1);
   /* 0.0 < 1.0 -> 1 */
   TESTINST_1_2_FCMP(4, "flt.d a0, fa0, fa1", 0x0000000000000000,
                     0x3ff0000000000000, 0x00, a0, fa0, fa1);
   /* 0.0 < -0.0 -> 0 */
   TESTINST_1_2_FCMP(4, "flt.d a0, fa0, fa1", 0x0000000000000000,
                     0x8000000000000000, 0x00, a0, fa0, fa1);
   /* -0.0 < 0.0 -> 0 */
   TESTINST_1_2_FCMP(4, "flt.d a0, fa0, fa1", 0x8000000000000000,
                     0x0000000000000000, 0x00, a0, fa0, fa1);
   /* INFINITY < INFINITY -> 0 */
   TESTINST_1_2_FCMP(4, "flt.d a0, fa0, fa1", 0x7ff0000000000000,
                     0x7ff0000000000000, 0x00, a0, fa0, fa1);
   /* 0.0 < qNAN -> 0 (NV) */
   TESTINST_1_2_FCMP(4, "flt.d a0, fa0, fa1", 0x0000000000000000,
                     0x7ff8000000000000, 0x00, a0, fa0, fa1);
   /* 0.0 < sNAN -> 0 (NV) */
   TESTINST_1_2_FCMP(4, "flt.d a0, fa0, fa1", 0x0000000000000000,
                     0x7ff4000000000000, 0x00, a0, fa0, fa1);

   /* sNAN < sNAN (rd=zero) -> 0 (NV) */
   TESTINST_1_2_FCMP(4, "flt.d zero, fa0, fa1", 0x7ff4000000000000,
                     0x7ff4000000000000, 0x00, zero, fa0, fa1);

   /* ----------------- fle.d rd, rs1, rs2 ------------------ */
   /* 1.0 < 0.0 -> 0 */
   TESTINST_1_2_FCMP(4, "fle.d a0, fa0, fa1", 0x3ff0000000000000,
                     0x0000000000000000, 0x00, a0, fa0, fa1);
   /* 0.0 <= 0.0 -> 1 */
   TESTINST_1_2_FCMP(4, "fle.d a0, fa0, fa1", 0x0000000000000000,
                     0x0000000000000000, 0x00, a0, fa0, fa1);
   /* 0.0 <= 1.0 -> 1 */
   TESTINST_1_2_FCMP(4, "fle.d a0, fa0, fa1", 0x0000000000000000,
                     0x3ff0000000000000, 0x00, a0, fa0, fa1);
   /* 0.0 <= -0.0 -> 1 */
   TESTINST_1_2_FCMP(4, "fle.d a0, fa0, fa1", 0x0000000000000000,
                     0x8000000000000000, 0x00, a0, fa0, fa1);
   /* -0.0 <= 0.0 -> 1 */
   TESTINST_1_2_FCMP(4, "fle.d a0, fa0, fa1", 0x8000000000000000,
                     0x0000000000000000, 0x00, a0, fa0, fa1);
   /* INFINITY <= INFINITY -> 1 */
   TESTINST_1_2_FCMP(4, "fle.d a0, fa0, fa1", 0x7ff0000000000000,
                     0x7ff0000000000000, 0x00, a0, fa0, fa1);
   /* 0.0 <= qNAN -> 0 (NV) */
   TESTINST_1_2_FCMP(4, "fle.d a0, fa0, fa1", 0x0000000000000000,
                     0x7ff8000000000000, 0x00, a0, fa0, fa1);
   /* 0.0 <= sNAN -> 0 (NV) */
   TESTINST_1_2_FCMP(4, "fle.d a0, fa0, fa1", 0x0000000000000000,
                     0x7ff4000000000000, 0x00, a0, fa0, fa1);

   /* sNAN <= sNAN (rd=zero) -> 0 (NV) */
   TESTINST_1_2_FCMP(4, "fle.d zero, fa0, fa1", 0x7ff4000000000000,
                     0x7ff4000000000000, 0x00, zero, fa0, fa1);

   /* ------------------ fclass.d rd, rs1 ------------------- */
   /* fclass(-INFINITY) -> 0x001 */
   TESTINST_1_1_IF(4, "fclass.d a0, fa0", 0xfff0000000000000, 0x00, a0, fa0);
   /* fclass(-1.0) -> 0x002 */
   TESTINST_1_1_IF(4, "fclass.d a0, fa0", 0xbff0000000000000, 0x00, a0, fa0);
   /* fclass(-DBL_TRUE_MIN) -> 0x004 */
   TESTINST_1_1_IF(4, "fclass.d a0, fa0", 0x8000000000000001, 0x00, a0, fa0);
   /* fclass(-0.0) -> 0x008 */
   TESTINST_1_1_IF(4, "fclass.d a0, fa0", 0x8000000000000000, 0x00, a0, fa0);
   /* fclass(0.0) -> 0x010 */
   TESTINST_1_1_IF(4, "fclass.d a0, fa0", 0x0000000000000000, 0x00, a0, fa0);
   /* fclass(DBL_TRUE_MIN) -> 0x020 */
   TESTINST_1_1_IF(4, "fclass.d a0, fa0", 0x0000000000000001, 0x00, a0, fa0);
   /* fclass(1.0) -> 0x040 */
   TESTINST_1_1_IF(4, "fclass.d a0, fa0", 0x3ff0000000000000, 0x00, a0, fa0);
   /* fclass(INFINITY) -> 0x080 */
   TESTINST_1_1_IF(4, "fclass.d a0, fa0", 0x7ff0000000000000, 0x00, a0, fa0);
   /* fclass(sNAN) -> 0x100 */
   TESTINST_1_1_IF(4, "fclass.d a0, fa0", 0x7ff4000000000000, 0x00, a0, fa0);
   /* fclass(qNAN) -> 0x200 */
   TESTINST_1_1_IF(4, "fclass.d a0, fa0", 0x7ff8000000000000, 0x00, a0, fa0);

   /* fclass(-INFINITY) (rd=zero) -> 0x000 */
   TESTINST_1_1_IF(4, "fclass.d zero, fa0", 0xfff0000000000000, 0x00, zero,
                   fa0);

   /* ---------------- fcvt.w.d rd, rs1, rm ----------------- */
   /* 0.0 -> 0 */
   TESTINST_1_1_IF(4, "fcvt.w.d a0, fa0", 0x0000000000000000, 0x00, a0, fa0);
   /* DBL_TRUE_MIN -> 0 (NX) */
   TESTINST_1_1_IF(4, "fcvt.w.d a0, fa0", 0x0000000000000001, 0x00, a0, fa0);
   /* INFINITY -> 2**31-1 aka INT_MAX (NV)  */
   TESTINST_1_1_IF(4, "fcvt.w.d a0, fa0", 0x7ff0000000000000, 0x00, a0, fa0);
   /* qNAN -> 2**31-1 aka INT_MAX (NV) */
   TESTINST_1_1_IF(4, "fcvt.w.d a0, fa0", 0x7ff8000000000000, 0x00, a0, fa0);
   /* 2**31-1 -> 2**31-1 aka INT_MAX */
   TESTINST_1_1_IF(4, "fcvt.w.d a0, fa0", 0x41dfffffffc00000, 0x00, a0, fa0);
   /* -2**31 -> -2**31 aka INT_MIN */
   TESTINST_1_1_IF(4, "fcvt.w.d a0, fa0", 0xc1e0000000000000, 0x00, a0, fa0);
   /* 2**31 -> 2**31-1 aka INT_MAX (NV) */
   TESTINST_1_1_IF(4, "fcvt.w.d a0, fa0", 0x41e0000000000000, 0x00, a0, fa0);
   /* -2**31-1 -> -2**31 aka INT_MIN (NV) */
   TESTINST_1_1_IF(4, "fcvt.w.d a0, fa0", 0xc1e0000000200000, 0x00, a0, fa0);

   /* 1.0 (rd=zero) -> 0 */
   TESTINST_1_1_IF(4, "fcvt.w.d zero, fa0", 0x3ff0000000000000, 0x00, zero,
                   fa0);

   /* 0.5 (RNE) -> 0 (NX) */
   TESTINST_1_1_IF(4, "fcvt.w.d a0, fa0, rne", 0x3fe0000000000000, 0x00, a0,
                   fa0);
   /* 1.5 (RNE) -> 2 (NX) */
   TESTINST_1_1_IF(4, "fcvt.w.d a0, fa0, rne", 0x3ff8000000000000, 0x00, a0,
                   fa0);
   /* 0.5 (RTZ) -> 0 (NX) */
   TESTINST_1_1_IF(4, "fcvt.w.d a0, fa0, rtz", 0x3fe0000000000000, 0x00, a0,
                   fa0);
   /* -0.5 (RTZ) -> 0 (NX) */
   TESTINST_1_1_IF(4, "fcvt.w.d a0, fa0, rtz", 0xbfe0000000000000, 0x00, a0,
                   fa0);
   /* 0.5 (RDN) -> 0 (NX) */
   TESTINST_1_1_IF(4, "fcvt.w.d a0, fa0, rdn", 0x3fe0000000000000, 0x00, a0,
                   fa0);
   /* -0.5 (RDN) -> -1 (NX) */
   TESTINST_1_1_IF(4, "fcvt.w.d a0, fa0, rdn", 0xbfe0000000000000, 0x00, a0,
                   fa0);
   /* 0.5 (RUP) -> 1 (NX) */
   TESTINST_1_1_IF(4, "fcvt.w.d a0, fa0, rup", 0x3fe0000000000000, 0x00, a0,
                   fa0);
   /* -0.5 (RUP) -> 0 (NX) */
   TESTINST_1_1_IF(4, "fcvt.w.d a0, fa0, rup", 0xbfe0000000000000, 0x00, a0,
                   fa0);
   /* 0.5 (RMM) -> 1 (NX) */
   TESTINST_1_1_IF(4, "fcvt.w.d a0, fa0, rmm", 0x3fe0000000000000, 0x00, a0,
                   fa0);
   /* -0.5 (RMM) -> -1 (NX) */
   TESTINST_1_1_IF(4, "fcvt.w.d a0, fa0, rmm", 0xbfe0000000000000, 0x00, a0,
                   fa0);

   /* 0.5 (DYN-RNE) -> 0 (NX) */
   TESTINST_1_1_IF(4, "fcvt.w.d a0, fa0", 0x3fe0000000000000, 0x00, a0, fa0);
   /* 1.5 (DYN-RNE) -> 2 (NX) */
   TESTINST_1_1_IF(4, "fcvt.w.d a0, fa0", 0x3ff8000000000000, 0x00, a0, fa0);
   /* 0.5 (DYN-RTZ) -> 0 (NX) */
   TESTINST_1_1_IF(4, "fcvt.w.d a0, fa0", 0x3fe0000000000000, 0x20, a0, fa0);
   /* -0.5 (DYN-RTZ) -> 0 (NX) */
   TESTINST_1_1_IF(4, "fcvt.w.d a0, fa0", 0xbfe0000000000000, 0x20, a0, fa0);
   /* 0.5 (DYN-RDN) -> 0 (NX) */
   TESTINST_1_1_IF(4, "fcvt.w.d a0, fa0", 0x3fe0000000000000, 0x40, a0, fa0);
   /* -0.5 (DYN-RDN) -> -1 (NX) */
   TESTINST_1_1_IF(4, "fcvt.w.d a0, fa0", 0xbfe0000000000000, 0x40, a0, fa0);
   /* 0.5 (DYN-RUP) -> 1 (NX) */
   TESTINST_1_1_IF(4, "fcvt.w.d a0, fa0", 0x3fe0000000000000, 0x60, a0, fa0);
   /* -0.5 (DYN-RUP) -> 0 (NX) */
   TESTINST_1_1_IF(4, "fcvt.w.d a0, fa0", 0xbfe0000000000000, 0x60, a0, fa0);
   /* 0.5 (DYN-RMM) -> 1 (NX) */
   TESTINST_1_1_IF(4, "fcvt.w.d a0, fa0", 0x3fe0000000000000, 0x80, a0, fa0);
   /* -0.5 (DYN-RMM) -> -1 (NX) */
   TESTINST_1_1_IF(4, "fcvt.w.d a0, fa0", 0xbfe0000000000000, 0x80, a0, fa0);

   /* ---------------- fcvt.wu.d rd, rs1, rm ---------------- */
   /* 0.0 -> 0 */
   TESTINST_1_1_IF(4, "fcvt.wu.d a0, fa0", 0x0000000000000000, 0x00, a0, fa0);
   /* DBL_TRUE_MIN -> 0 (NX) */
   TESTINST_1_1_IF(4, "fcvt.wu.d a0, fa0", 0x0000000000000001, 0x00, a0, fa0);
   /* INFINITY -> 2**32-1 aka UINT_MAX (NV) */
   TESTINST_1_1_IF(4, "fcvt.wu.d a0, fa0", 0x7ff0000000000000, 0x00, a0, fa0);
   /* qNAN -> 2**32-1 aka UINT_MAX (NV) */
   TESTINST_1_1_IF(4, "fcvt.wu.d a0, fa0", 0x7ff8000000000000, 0x00, a0, fa0);
   /* 2**32-1 -> 2**32-1 aka UINT_MAX */
   TESTINST_1_1_IF(4, "fcvt.wu.d a0, fa0", 0x41efffffffe00000, 0x00, a0, fa0);
   /* -1.0 -> 0 (NV) */
   TESTINST_1_1_IF(4, "fcvt.wu.d a0, fa0", 0xbff0000000000000, 0x00, a0, fa0);

   /* 1.0 (rd=zero) -> 0 */
   TESTINST_1_1_IF(4, "fcvt.wu.d zero, fa0", 0x3ff0000000000000, 0x00, zero,
                   fa0);

   /* 0.5 (RNE) -> 0 (NX) */
   TESTINST_1_1_IF(4, "fcvt.wu.d a0, fa0, rne", 0x3fe0000000000000, 0x00, a0,
                   fa0);
   /* 1.5 (RNE) -> 2 (NX) */
   TESTINST_1_1_IF(4, "fcvt.wu.d a0, fa0, rne", 0x3ff8000000000000, 0x00, a0,
                   fa0);
   /* 0.5 (RTZ) -> 0 (NX) */
   TESTINST_1_1_IF(4, "fcvt.wu.d a0, fa0, rtz", 0x3fe0000000000000, 0x00, a0,
                   fa0);
   /* 0.5 (RDN) -> 0 (NX) */
   TESTINST_1_1_IF(4, "fcvt.wu.d a0, fa0, rdn", 0x3fe0000000000000, 0x00, a0,
                   fa0);
   /* 0.5 (RUP) -> 1 (NX) */
   TESTINST_1_1_IF(4, "fcvt.wu.d a0, fa0, rup", 0x3fe0000000000000, 0x00, a0,
                   fa0);
   /* 0.5 (RMM) -> 1 (NX) */
   TESTINST_1_1_IF(4, "fcvt.wu.d a0, fa0, rmm", 0x3fe0000000000000, 0x00, a0,
                   fa0);

   /* 0.5 (DYN-RNE) -> 0 (NX) */
   TESTINST_1_1_IF(4, "fcvt.wu.d a0, fa0", 0x3fe0000000000000, 0x00, a0, fa0);
   /* 1.5 (DYN-RNE) -> 2 (NX) */
   TESTINST_1_1_IF(4, "fcvt.wu.d a0, fa0", 0x3ff8000000000000, 0x00, a0, fa0);
   /* 0.5 (DYN-RTZ) -> 0 (NX) */
   TESTINST_1_1_IF(4, "fcvt.wu.d a0, fa0", 0x3fe0000000000000, 0x20, a0, fa0);
   /* 0.5 (DYN-RDN) -> 0 (NX) */
   TESTINST_1_1_IF(4, "fcvt.wu.d a0, fa0", 0x3fe0000000000000, 0x40, a0, fa0);
   /* 0.5 (DYN-RUP) -> 1 (NX) */
   TESTINST_1_1_IF(4, "fcvt.wu.d a0, fa0", 0x3fe0000000000000, 0x60, a0, fa0);
   /* 0.5 (DYN-RMM) -> 1 (NX) */
   TESTINST_1_1_IF(4, "fcvt.wu.d a0, fa0", 0x3fe0000000000000, 0x80, a0, fa0);

   /* ---------------- fcvt.d.w rd, rs1, rm ----------------- */
   /* 0 -> 0.0 */
   TESTINST_1_1_FI(4, "fcvt.d.w fa0, a0", 0x0000000000000000, 0x00, fa0, a0);
   /* 2**31-1 aka INT_MAX -> 2**31-1 */
   TESTINST_1_1_FI(4, "fcvt.d.w fa0, a0", 0x000000007fffffff, 0x00, fa0, a0);
   /* -2**31 aka INT_MIN -> -2**31 */
   TESTINST_1_1_FI(4, "fcvt.d.w fa0, a0", 0xffffffff80000000, 0x00, fa0, a0);

   /* ---------------- fcvt.d.wu rd, rs1, rm ---------------- */
   /* 0 -> 0.0 */
   TESTINST_1_1_FI(4, "fcvt.d.wu fa0, a0", 0x0000000000000000, 0x00, fa0, a0);
   /* 2**32-1 aka UINT_MAX -> 2**32-1 */
   TESTINST_1_1_FI(4, "fcvt.d.wu fa0, a0", 0x00000000ffffffff, 0x00, fa0, a0);

   printf("\n");
}

static void test_float64_additions(void)
{
   printf("RV64D double-precision FP instruction set, additions\n");

   /* ---------------- fcvt.l.d rd, rs1, rm ----------------- */
   /* 0.0 -> 0 */
   TESTINST_1_1_IF(4, "fcvt.l.d a0, fa0", 0x0000000000000000, 0x00, a0, fa0);
   /* DBL_TRUE_MIN -> 0 (NX) */
   TESTINST_1_1_IF(4, "fcvt.l.d a0, fa0", 0x0000000000000001, 0x00, a0, fa0);
   /* INFINITY -> 2**63-1 aka LONG_MAX (NV) */
   TESTINST_1_1_IF(4, "fcvt.l.d a0, fa0", 0x7ff0000000000000, 0x00, a0, fa0);
   /* qNAN -> 2**63-1 aka LONG_MAX (NV) */
   TESTINST_1_1_IF(4, "fcvt.l.d a0, fa0", 0x7ff8000000000000, 0x00, a0, fa0);
   /* nextafter(2**63, 0.0) -> 2**63-1024 */
   TESTINST_1_1_IF(4, "fcvt.l.d a0, fa0", 0x43dfffffffffffff, 0x00, a0, fa0);
   /* -2**63 -> -2**63 aka LONG_MIN */
   TESTINST_1_1_IF(4, "fcvt.l.d a0, fa0", 0xc3e0000000000000, 0x00, a0, fa0);
   /* 2**63 -> 2**63-1 aka LONG_MAX (NV) */
   TESTINST_1_1_IF(4, "fcvt.l.d a0, fa0", 0x43e0000000000000, 0x00, a0, fa0);
   /* -nextafter(2**63) -> -2**63 aka LONG_MIN (NV) */
   TESTINST_1_1_IF(4, "fcvt.l.d a0, fa0", 0xc3e0000000000001, 0x00, a0, fa0);

   /* 1.0 (rd=zero) -> 0 */
   TESTINST_1_1_IF(4, "fcvt.l.d zero, fa0", 0x3ff0000000000000, 0x00, zero,
                   fa0);

   /* 0.5 (RNE) -> 0 (NX) */
   TESTINST_1_1_IF(4, "fcvt.l.d a0, fa0, rne", 0x3fe0000000000000, 0x00, a0,
                   fa0);
   /* 1.5 (RNE) -> 2 (NX) */
   TESTINST_1_1_IF(4, "fcvt.l.d a0, fa0, rne", 0x3ff8000000000000, 0x00, a0,
                   fa0);
   /* 0.5 (RTZ) -> 0 (NX) */
   TESTINST_1_1_IF(4, "fcvt.l.d a0, fa0, rtz", 0x3fe0000000000000, 0x00, a0,
                   fa0);
   /* -0.5 (RTZ) -> 0 (NX) */
   TESTINST_1_1_IF(4, "fcvt.l.d a0, fa0, rtz", 0xbfe0000000000000, 0x00, a0,
                   fa0);
   /* 0.5 (RDN) -> 0 (NX) */
   TESTINST_1_1_IF(4, "fcvt.l.d a0, fa0, rdn", 0x3fe0000000000000, 0x00, a0,
                   fa0);
   /* -0.5 (RDN) -> -1 (NX) */
   TESTINST_1_1_IF(4, "fcvt.l.d a0, fa0, rdn", 0xbfe0000000000000, 0x00, a0,
                   fa0);
   /* 0.5 (RUP) -> 1 (NX) */
   TESTINST_1_1_IF(4, "fcvt.l.d a0, fa0, rup", 0x3fe0000000000000, 0x00, a0,
                   fa0);
   /* -0.5 (RUP) -> 0 (NX) */
   TESTINST_1_1_IF(4, "fcvt.l.d a0, fa0, rup", 0xbfe0000000000000, 0x00, a0,
                   fa0);
   /* 0.5 (RMM) -> 1 (NX) */
   TESTINST_1_1_IF(4, "fcvt.l.d a0, fa0, rmm", 0x3fe0000000000000, 0x00, a0,
                   fa0);
   /* -0.5 (RMM) -> -1 (NX) */
   TESTINST_1_1_IF(4, "fcvt.l.d a0, fa0, rmm", 0xbfe0000000000000, 0x00, a0,
                   fa0);

   /* 0.5 (DYN-RNE) -> 0 (NX) */
   TESTINST_1_1_IF(4, "fcvt.l.d a0, fa0", 0x3fe0000000000000, 0x00, a0, fa0);
   /* 1.5 (DYN-RNE) -> 2 (NX) */
   TESTINST_1_1_IF(4, "fcvt.l.d a0, fa0", 0x3ff8000000000000, 0x00, a0, fa0);
   /* 0.5 (DYN-RTZ) -> 0 (NX) */
   TESTINST_1_1_IF(4, "fcvt.l.d a0, fa0", 0x3fe0000000000000, 0x20, a0, fa0);
   /* -0.5 (DYN-RTZ) -> 0 (NX) */
   TESTINST_1_1_IF(4, "fcvt.l.d a0, fa0", 0xbfe0000000000000, 0x20, a0, fa0);
   /* 0.5 (DYN-RDN) -> 0 (NX) */
   TESTINST_1_1_IF(4, "fcvt.l.d a0, fa0", 0x3fe0000000000000, 0x40, a0, fa0);
   /* -0.5 (DYN-RDN) -> -1 (NX) */
   TESTINST_1_1_IF(4, "fcvt.l.d a0, fa0", 0xbfe0000000000000, 0x40, a0, fa0);
   /* 0.5 (DYN-RUP) -> 1 (NX) */
   TESTINST_1_1_IF(4, "fcvt.l.d a0, fa0", 0x3fe0000000000000, 0x60, a0, fa0);
   /* -0.5 (DYN-RUP) -> 0 (NX) */
   TESTINST_1_1_IF(4, "fcvt.l.d a0, fa0", 0xbfe0000000000000, 0x60, a0, fa0);
   /* 0.5 (DYN-RMM) -> 1 (NX) */
   TESTINST_1_1_IF(4, "fcvt.l.d a0, fa0", 0x3fe0000000000000, 0x80, a0, fa0);
   /* -0.5 (DYN-RMM) -> -1 (NX) */
   TESTINST_1_1_IF(4, "fcvt.l.d a0, fa0", 0xbfe0000000000000, 0x80, a0, fa0);

   /* ---------------- fcvt.lu.d rd, rs1, rm ---------------- */
   /* 0.0 -> 0 */
   TESTINST_1_1_IF(4, "fcvt.lu.d a0, fa0", 0x0000000000000000, 0x00, a0, fa0);
   /* DBL_TRUE_MIN -> 0 (NX) */
   TESTINST_1_1_IF(4, "fcvt.lu.d a0, fa0", 0x0000000000000001, 0x00, a0, fa0);
   /* INFINITY -> 2**64-1 aka ULONG_MAX (NV) */
   TESTINST_1_1_IF(4, "fcvt.lu.d a0, fa0", 0x7ff0000000000000, 0x00, a0, fa0);
   /* qNAN -> 2**64-1 aka ULONG_MAX (NV) */
   TESTINST_1_1_IF(4, "fcvt.lu.d a0, fa0", 0x7ff8000000000000, 0x00, a0, fa0);
   /* nextafter(2**64, 0.0) -> 2**63-2048 */
   TESTINST_1_1_IF(4, "fcvt.lu.d a0, fa0", 0x43efffffffffffff, 0x00, a0, fa0);
   /* 2**64 -> 2**64-1 aka ULONG_MAX (NV) */
   TESTINST_1_1_IF(4, "fcvt.lu.d a0, fa0", 0x43f0000000000000, 0x00, a0, fa0);
   /* -1.0 -> 0 (NV) */
   TESTINST_1_1_IF(4, "fcvt.lu.d a0, fa0", 0xbff0000000000000, 0x00, a0, fa0);

   /* 1.0 (rd=zero) -> 0 */
   TESTINST_1_1_IF(4, "fcvt.lu.d zero, fa0", 0x3ff0000000000000, 0x00, zero,
                   fa0);

   /* 0.5 (RNE) -> 0 (NX) */
   TESTINST_1_1_IF(4, "fcvt.lu.d a0, fa0, rne", 0x3fe0000000000000, 0x00, a0,
                   fa0);
   /* 1.5 (RNE) -> 2 (NX) */
   TESTINST_1_1_IF(4, "fcvt.lu.d a0, fa0, rne", 0x3ff8000000000000, 0x00, a0,
                   fa0);
   /* 0.5 (RTZ) -> 0 (NX) */
   TESTINST_1_1_IF(4, "fcvt.lu.d a0, fa0, rtz", 0x3fe0000000000000, 0x00, a0,
                   fa0);
   /* 0.5 (RDN) -> 0 (NX) */
   TESTINST_1_1_IF(4, "fcvt.lu.d a0, fa0, rdn", 0x3fe0000000000000, 0x00, a0,
                   fa0);
   /* 0.5 (RUP) -> 1 (NX) */
   TESTINST_1_1_IF(4, "fcvt.lu.d a0, fa0, rup", 0x3fe0000000000000, 0x00, a0,
                   fa0);
   /* 0.5 (RMM) -> 1 (NX) */
   TESTINST_1_1_IF(4, "fcvt.lu.d a0, fa0, rmm", 0x3fe0000000000000, 0x00, a0,
                   fa0);

   /* 0.5 (DYN-RNE) -> 0 (NX) */
   TESTINST_1_1_IF(4, "fcvt.lu.d a0, fa0", 0x3fe0000000000000, 0x00, a0, fa0);
   /* 1.5 (DYN-RNE) -> 2 (NX) */
   TESTINST_1_1_IF(4, "fcvt.lu.d a0, fa0", 0x3ff8000000000000, 0x00, a0, fa0);
   /* 0.5 (DYN-RTZ) -> 0 (NX) */
   TESTINST_1_1_IF(4, "fcvt.lu.d a0, fa0", 0x3fe0000000000000, 0x20, a0, fa0);
   /* 0.5 (DYN-RDN) -> 0 (NX) */
   TESTINST_1_1_IF(4, "fcvt.lu.d a0, fa0", 0x3fe0000000000000, 0x40, a0, fa0);
   /* 0.5 (DYN-RUP) -> 1 (NX) */
   TESTINST_1_1_IF(4, "fcvt.lu.d a0, fa0", 0x3fe0000000000000, 0x60, a0, fa0);
   /* 0.5 (DYN-RMM) -> 1 (NX) */
   TESTINST_1_1_IF(4, "fcvt.lu.d a0, fa0", 0x3fe0000000000000, 0x80, a0, fa0);

   /* ------------------- fmv.x.d rd, rs1 ------------------- */
   TESTINST_1_1_IF(4, "fmv.x.d a0, fa0", 0xabcdef0123456789, 0x00, a0, fa0);

   /* 1.0 (rd=zero) -> 0 */
   TESTINST_1_1_IF(4, "fmv.x.d zero, fa0", 0x3ff0000000000000, 0x00, zero, fa0);

   /* ---------------- fcvt.d.l rd, rs1, rm ----------------- */
   /* 0 -> 0.0 */
   TESTINST_1_1_FI(4, "fcvt.d.l fa0, a0", 0x0000000000000000, 0x00, fa0, a0);
   /* 2**63-1024 -> nextafter(2**63, 0.0) */
   TESTINST_1_1_FI(4, "fcvt.d.l fa0, a0", 0x7ffffffffffffc00, 0x00, fa0, a0);
   /* 2**63-1 aka LONG_MAX -> 2**63 (NX) */
   TESTINST_1_1_FI(4, "fcvt.d.l fa0, a0", 0x7fffffffffffffff, 0x00, fa0, a0);
   /* -2**63 aka LONG_MIN -> -2**63 */
   TESTINST_1_1_FI(4, "fcvt.d.l fa0, a0", 0x8000000000000000, 0x00, fa0, a0);

   /* 2**53+1 (RNE) -> 2**53 (NX) */
   TESTINST_1_1_FI(4, "fcvt.d.l fa0, a0, rne", 0x0020000000000001, 0x00, fa0,
                   a0);
   /* 2**53+3 (RNE) -> 2**53+4 (NX) */
   TESTINST_1_1_FI(4, "fcvt.d.l fa0, a0, rne", 0x0020000000000003, 0x00, fa0,
                   a0);
   /* 2**53+1 (RTZ) -> 2**53 (NX) */
   TESTINST_1_1_FI(4, "fcvt.d.l fa0, a0, rtz", 0x0020000000000001, 0x00, fa0,
                   a0);
   /* -2**53-1 (RTZ) -> -2**53 (NX) */
   TESTINST_1_1_FI(4, "fcvt.d.l fa0, a0, rtz", 0xffdfffffffffffff, 0x00, fa0,
                   a0);
   /* 2**53+1 (RDN) -> 2**53 (NX) */
   TESTINST_1_1_FI(4, "fcvt.d.l fa0, a0, rdn", 0x0020000000000001, 0x00, fa0,
                   a0);
   /* -2**53-1 (RDN) -> -2**53-2 (NX) */
   TESTINST_1_1_FI(4, "fcvt.d.l fa0, a0, rdn", 0xffdfffffffffffff, 0x00, fa0,
                   a0);
   /* 2**53+1 (RUP) -> 2**53+2 (NX) */
   TESTINST_1_1_FI(4, "fcvt.d.l fa0, a0, rup", 0x0020000000000001, 0x00, fa0,
                   a0);
   /* -2**53-1 (RUP) -> -2**53 (NX) */
   TESTINST_1_1_FI(4, "fcvt.d.l fa0, a0, rup", 0xffdfffffffffffff, 0x00, fa0,
                   a0);
   /* 2**53+1 (RMM) -> 2**53+2 (NX) */
   TESTINST_1_1_FI(4, "fcvt.d.l fa0, a0, rmm", 0x0020000000000001, 0x00, fa0,
                   a0);
   /* -2**53-1 (RMM) -> -2**53-2 (NX) */
   TESTINST_1_1_FI(4, "fcvt.d.l fa0, a0, rmm", 0xffdfffffffffffff, 0x00, fa0,
                   a0);

   /* 2**53+1 (DYN-RNE) -> 2**53 (NX) */
   TESTINST_1_1_FI(4, "fcvt.d.l fa0, a0", 0x0020000000000001, 0x00, fa0, a0);
   /* 2**53+3 (DYN-RNE) -> 2**53+4 (NX) */
   TESTINST_1_1_FI(4, "fcvt.d.l fa0, a0", 0x0020000000000003, 0x00, fa0, a0);
   /* 2**53+1 (DYN-RTZ) -> 2**53 (NX) */
   TESTINST_1_1_FI(4, "fcvt.d.l fa0, a0", 0x0020000000000001, 0x20, fa0, a0);
   /* -2**53-1 (DYN-RTZ) -> -2**53 (NX) */
   TESTINST_1_1_FI(4, "fcvt.d.l fa0, a0", 0xffdfffffffffffff, 0x20, fa0, a0);
   /* 2**53+1 (DYN-RDN) -> 2**53 (NX) */
   TESTINST_1_1_FI(4, "fcvt.d.l fa0, a0", 0x0020000000000001, 0x40, fa0, a0);
   /* -2**53-1 (DYN-RDN) -> -2**53-2 (NX) */
   TESTINST_1_1_FI(4, "fcvt.d.l fa0, a0", 0xffdfffffffffffff, 0x40, fa0, a0);
   /* 2**53+1 (DYN-RUP) -> 2**53+2 (NX) */
   TESTINST_1_1_FI(4, "fcvt.d.l fa0, a0", 0x0020000000000001, 0x60, fa0, a0);
   /* -2**53-1 (DYN-RUP) -> -2**53 (NX) */
   TESTINST_1_1_FI(4, "fcvt.d.l fa0, a0", 0xffdfffffffffffff, 0x60, fa0, a0);
   /* 2**53+1 (DYN-RMM) -> 2**53+2 (NX) */
   TESTINST_1_1_FI(4, "fcvt.d.l fa0, a0", 0x0020000000000001, 0x80, fa0, a0);
   /* -2**53-1 (DYN-RMM) -> -2**53-2 (NX) */
   TESTINST_1_1_FI(4, "fcvt.d.l fa0, a0", 0xffdfffffffffffff, 0x80, fa0, a0);

   /* ---------------- fcvt.d.lu rd, rs1, rm ---------------- */
   /* 0 -> 0.0 */
   TESTINST_1_1_FI(4, "fcvt.d.lu fa0, a0", 0x0000000000000000, 0x00, fa0, a0);
   /* 2**64-2048 -> nextafter(2**64, 0.0) */
   TESTINST_1_1_FI(4, "fcvt.d.lu fa0, a0", 0xfffffffffffff800, 0x00, fa0, a0);
   /* 2**64-1 aka ULONG_MAX -> 2**64 (NX) */
   TESTINST_1_1_FI(4, "fcvt.d.lu fa0, a0", 0xffffffffffffffff, 0x00, fa0, a0);

   /* 2**53+1 (RNE) -> 2**53 (NX) */
   TESTINST_1_1_FI(4, "fcvt.d.lu fa0, a0, rne", 0x0020000000000001, 0x00, fa0,
                   a0);
   /* 2**53+3 (RNE) -> 2**53+4 (NX) */
   TESTINST_1_1_FI(4, "fcvt.d.lu fa0, a0, rne", 0x0020000000000003, 0x00, fa0,
                   a0);
   /* 2**53+1 (RTZ) -> 2**53 (NX) */
   TESTINST_1_1_FI(4, "fcvt.d.lu fa0, a0, rtz", 0x0020000000000001, 0x00, fa0,
                   a0);
   /* 2**53+1 (RDN) -> 2**53 (NX) */
   TESTINST_1_1_FI(4, "fcvt.d.lu fa0, a0, rdn", 0x0020000000000001, 0x00, fa0,
                   a0);
   /* 2**53+1 (RUP) -> 2**53+2 (NX) */
   TESTINST_1_1_FI(4, "fcvt.d.lu fa0, a0, rup", 0x0020000000000001, 0x00, fa0,
                   a0);
   /* 2**53+1 (RMM) -> 2**53+2 (NX) */
   TESTINST_1_1_FI(4, "fcvt.d.lu fa0, a0, rmm", 0x0020000000000001, 0x00, fa0,
                   a0);

   /* 2**53+1 (DYN-RNE) -> 2**53 (NX) */
   TESTINST_1_1_FI(4, "fcvt.d.lu fa0, a0", 0x0020000000000001, 0x00, fa0, a0);
   /* 2**53+3 (DYN-RNE) -> 2**53+4 (NX) */
   TESTINST_1_1_FI(4, "fcvt.d.lu fa0, a0", 0x0020000000000003, 0x00, fa0, a0);
   /* 2**53+1 (DYN-RTZ) -> 2**53 (NX) */
   TESTINST_1_1_FI(4, "fcvt.d.lu fa0, a0", 0x0020000000000001, 0x20, fa0, a0);
   /* 2**53+1 (DYN-RDN) -> 2**53 (NX) */
   TESTINST_1_1_FI(4, "fcvt.d.lu fa0, a0", 0x0020000000000001, 0x40, fa0, a0);
   /* 2**53+1 (DYN-RUP) -> 2**53+2 (NX) */
   TESTINST_1_1_FI(4, "fcvt.d.lu fa0, a0", 0x0020000000000001, 0x60, fa0, a0);
   /* 2**53+1 (DYN-RMM) -> 2**53+2 (NX) */
   TESTINST_1_1_FI(4, "fcvt.d.lu fa0, a0", 0x0020000000000001, 0x80, fa0, a0);

   /* ------------------- fmv.d.x rd, rs1 ------------------- */
   TESTINST_1_1_FI(4, "fmv.d.x fa0, a0", 0xabcdef0123456789, 0x00, fa0, a0);
}

int main(void)
{
   test_float64_shared();
   test_float64_additions();
   return 0;
}
