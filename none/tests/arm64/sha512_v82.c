
#include <stdio.h>
#include <assert.h>
#include "simd.h"

/* ---------------------------------------------------------------- */
/* -- Tests, in the same order that they appear in main()        -- */
/* -- The full list of instructions tested appear at the end of  -- */
/* -- this file.                                                 -- */
/* ---------------------------------------------------------------- */

// SHA512H <Qd>, <Qn>, <Vm>.2D

GEN_THREEVEC_TEST(sha512h_00_01_02, "sha512h q0, q1, v2.2d", 0, 1, 2)
GEN_THREEVEC_TEST(sha512h_01_02_03, "sha512h q1, q2, v3.2d", 1, 2, 3)
GEN_THREEVEC_TEST(sha512h_02_03_04, "sha512h q2, q3, v4.2d", 2, 3, 4)
GEN_THREEVEC_TEST(sha512h_03_04_05, "sha512h q3, q4, v5.2d", 3, 4, 5)
GEN_THREEVEC_TEST(sha512h_04_05_06, "sha512h q4, q5, v6.2d", 4, 5, 6)
GEN_THREEVEC_TEST(sha512h_05_06_07, "sha512h q5, q6, v7.2d", 5, 6, 7)
GEN_THREEVEC_TEST(sha512h_06_07_08, "sha512h q6, q7, v8.2d", 6, 7, 8)
GEN_THREEVEC_TEST(sha512h_07_08_09, "sha512h q7, q8, v9.2d", 7, 8, 9)
GEN_THREEVEC_TEST(sha512h_08_09_10, "sha512h q8, q9, v10.2d", 8, 9, 10)
GEN_THREEVEC_TEST(sha512h_09_10_11, "sha512h q9, q10, v11.2d", 9, 10, 11)
GEN_THREEVEC_TEST(sha512h_10_11_12, "sha512h q10, q11, v12.2d", 10, 11, 12)
GEN_THREEVEC_TEST(sha512h_11_12_13, "sha512h q11, q12, v13.2d", 11, 12, 13)
GEN_THREEVEC_TEST(sha512h_12_13_14, "sha512h q12, q13, v14.2d", 12, 13, 14)
GEN_THREEVEC_TEST(sha512h_13_14_15, "sha512h q13, q14, v15.2d", 13, 14, 15)
GEN_THREEVEC_TEST(sha512h_14_15_16, "sha512h q14, q15, v16.2d", 14, 15, 16)
GEN_THREEVEC_TEST(sha512h_15_16_17, "sha512h q15, q16, v17.2d", 15, 16, 17)
GEN_THREEVEC_TEST(sha512h_16_17_18, "sha512h q16, q17, v18.2d", 16, 17, 18)
GEN_THREEVEC_TEST(sha512h_17_18_19, "sha512h q17, q18, v19.2d", 17, 18, 19)
GEN_THREEVEC_TEST(sha512h_18_19_20, "sha512h q18, q19, v20.2d", 18, 19, 20)
GEN_THREEVEC_TEST(sha512h_19_20_21, "sha512h q19, q20, v21.2d", 19, 20, 21)
GEN_THREEVEC_TEST(sha512h_20_21_22, "sha512h q20, q21, v22.2d", 20, 21, 22)
GEN_THREEVEC_TEST(sha512h_21_22_23, "sha512h q21, q22, v23.2d", 21, 22, 23)
GEN_THREEVEC_TEST(sha512h_22_23_24, "sha512h q22, q23, v24.2d", 22, 23, 24)
GEN_THREEVEC_TEST(sha512h_23_24_25, "sha512h q23, q24, v25.2d", 23, 24, 25)
GEN_THREEVEC_TEST(sha512h_24_25_26, "sha512h q24, q25, v26.2d", 24, 25, 26)
GEN_THREEVEC_TEST(sha512h_25_26_27, "sha512h q25, q26, v27.2d", 25, 26, 27)
GEN_THREEVEC_TEST(sha512h_26_27_28, "sha512h q26, q27, v28.2d", 26, 27, 28)
GEN_THREEVEC_TEST(sha512h_27_28_29, "sha512h q27, q28, v29.2d", 27, 28, 29)
GEN_THREEVEC_TEST(sha512h_28_29_30, "sha512h q28, q29, v30.2d", 28, 29, 30)
GEN_THREEVEC_TEST(sha512h_29_30_31, "sha512h q29, q30, v31.2d", 29, 30, 31)

// SHA512H2 <Qd>, <Qn>, <Vm>.2D

GEN_THREEVEC_TEST(sha512h2_00_01_02, "sha512h2 q0, q1, v2.2d", 0, 1, 2)
GEN_THREEVEC_TEST(sha512h2_01_02_03, "sha512h2 q1, q2, v3.2d", 1, 2, 3)
GEN_THREEVEC_TEST(sha512h2_02_03_04, "sha512h2 q2, q3, v4.2d", 2, 3, 4)
GEN_THREEVEC_TEST(sha512h2_03_04_05, "sha512h2 q3, q4, v5.2d", 3, 4, 5)
GEN_THREEVEC_TEST(sha512h2_04_05_06, "sha512h2 q4, q5, v6.2d", 4, 5, 6)
GEN_THREEVEC_TEST(sha512h2_05_06_07, "sha512h2 q5, q6, v7.2d", 5, 6, 7)
GEN_THREEVEC_TEST(sha512h2_06_07_08, "sha512h2 q6, q7, v8.2d", 6, 7, 8)
GEN_THREEVEC_TEST(sha512h2_07_08_09, "sha512h2 q7, q8, v9.2d", 7, 8, 9)
GEN_THREEVEC_TEST(sha512h2_08_09_10, "sha512h2 q8, q9, v10.2d", 8, 9, 10)
GEN_THREEVEC_TEST(sha512h2_09_10_11, "sha512h2 q9, q10, v11.2d", 9, 10, 11)
GEN_THREEVEC_TEST(sha512h2_10_11_12, "sha512h2 q10, q11, v12.2d", 10, 11, 12)
GEN_THREEVEC_TEST(sha512h2_11_12_13, "sha512h2 q11, q12, v13.2d", 11, 12, 13)
GEN_THREEVEC_TEST(sha512h2_12_13_14, "sha512h2 q12, q13, v14.2d", 12, 13, 14)
GEN_THREEVEC_TEST(sha512h2_13_14_15, "sha512h2 q13, q14, v15.2d", 13, 14, 15)
GEN_THREEVEC_TEST(sha512h2_14_15_16, "sha512h2 q14, q15, v16.2d", 14, 15, 16)
GEN_THREEVEC_TEST(sha512h2_15_16_17, "sha512h2 q15, q16, v17.2d", 15, 16, 17)
GEN_THREEVEC_TEST(sha512h2_16_17_18, "sha512h2 q16, q17, v18.2d", 16, 17, 18)
GEN_THREEVEC_TEST(sha512h2_17_18_19, "sha512h2 q17, q18, v19.2d", 17, 18, 19)
GEN_THREEVEC_TEST(sha512h2_18_19_20, "sha512h2 q18, q19, v20.2d", 18, 19, 20)
GEN_THREEVEC_TEST(sha512h2_19_20_21, "sha512h2 q19, q20, v21.2d", 19, 20, 21)
GEN_THREEVEC_TEST(sha512h2_20_21_22, "sha512h2 q20, q21, v22.2d", 20, 21, 22)
GEN_THREEVEC_TEST(sha512h2_21_22_23, "sha512h2 q21, q22, v23.2d", 21, 22, 23)
GEN_THREEVEC_TEST(sha512h2_22_23_24, "sha512h2 q22, q23, v24.2d", 22, 23, 24)
GEN_THREEVEC_TEST(sha512h2_23_24_25, "sha512h2 q23, q24, v25.2d", 23, 24, 25)
GEN_THREEVEC_TEST(sha512h2_24_25_26, "sha512h2 q24, q25, v26.2d", 24, 25, 26)
GEN_THREEVEC_TEST(sha512h2_25_26_27, "sha512h2 q25, q26, v27.2d", 25, 26, 27)
GEN_THREEVEC_TEST(sha512h2_26_27_28, "sha512h2 q26, q27, v28.2d", 26, 27, 28)
GEN_THREEVEC_TEST(sha512h2_27_28_29, "sha512h2 q27, q28, v29.2d", 27, 28, 29)
GEN_THREEVEC_TEST(sha512h2_28_29_30, "sha512h2 q28, q29, v30.2d", 28, 29, 30)
GEN_THREEVEC_TEST(sha512h2_29_30_31, "sha512h2 q29, q30, v31.2d", 29, 30, 31)

// SHA512SU0 <Vd>.2D, <Vn>.2D

GEN_TWOVEC_TEST(sha512su0_00_01, "sha512su0 v0.2d, v1.2d", 0, 1)
GEN_TWOVEC_TEST(sha512su0_01_02, "sha512su0 v1.2d, v2.2d", 1, 2)
GEN_TWOVEC_TEST(sha512su0_02_03, "sha512su0 v2.2d, v3.2d", 2, 3)
GEN_TWOVEC_TEST(sha512su0_03_04, "sha512su0 v3.2d, v4.2d", 3, 4)
GEN_TWOVEC_TEST(sha512su0_04_05, "sha512su0 v4.2d, v5.2d", 4, 5)
GEN_TWOVEC_TEST(sha512su0_05_06, "sha512su0 v5.2d, v6.2d", 5, 6)
GEN_TWOVEC_TEST(sha512su0_06_07, "sha512su0 v6.2d, v7.2d", 6, 7)
GEN_TWOVEC_TEST(sha512su0_07_08, "sha512su0 v7.2d, v8.2d", 7, 8)
GEN_TWOVEC_TEST(sha512su0_08_09, "sha512su0 v8.2d, v9.2d", 8, 9)
GEN_TWOVEC_TEST(sha512su0_09_10, "sha512su0 v9.2d, v10.2d", 9, 10)
GEN_TWOVEC_TEST(sha512su0_10_11, "sha512su0 v10.2d, v11.2d", 10, 11)
GEN_TWOVEC_TEST(sha512su0_11_12, "sha512su0 v11.2d, v12.2d", 11, 12)
GEN_TWOVEC_TEST(sha512su0_12_13, "sha512su0 v12.2d, v13.2d", 12, 13)
GEN_TWOVEC_TEST(sha512su0_13_14, "sha512su0 v13.2d, v14.2d", 13, 14)
GEN_TWOVEC_TEST(sha512su0_14_15, "sha512su0 v14.2d, v15.2d", 14, 15)
GEN_TWOVEC_TEST(sha512su0_15_16, "sha512su0 v15.2d, v16.2d", 15, 16)
GEN_TWOVEC_TEST(sha512su0_16_17, "sha512su0 v16.2d, v17.2d", 16, 17)
GEN_TWOVEC_TEST(sha512su0_17_18, "sha512su0 v17.2d, v18.2d", 17, 18)
GEN_TWOVEC_TEST(sha512su0_18_19, "sha512su0 v18.2d, v19.2d", 18, 19)
GEN_TWOVEC_TEST(sha512su0_19_20, "sha512su0 v19.2d, v20.2d", 19, 20)
GEN_TWOVEC_TEST(sha512su0_20_21, "sha512su0 v20.2d, v21.2d", 20, 21)
GEN_TWOVEC_TEST(sha512su0_21_22, "sha512su0 v21.2d, v22.2d", 21, 22)
GEN_TWOVEC_TEST(sha512su0_22_23, "sha512su0 v22.2d, v23.2d", 22, 23)
GEN_TWOVEC_TEST(sha512su0_23_24, "sha512su0 v23.2d, v24.2d", 23, 24)
GEN_TWOVEC_TEST(sha512su0_24_25, "sha512su0 v24.2d, v25.2d", 24, 25)
GEN_TWOVEC_TEST(sha512su0_25_26, "sha512su0 v25.2d, v26.2d", 25, 26)
GEN_TWOVEC_TEST(sha512su0_26_27, "sha512su0 v26.2d, v27.2d", 26, 27)
GEN_TWOVEC_TEST(sha512su0_27_28, "sha512su0 v27.2d, v28.2d", 27, 28)
GEN_TWOVEC_TEST(sha512su0_28_29, "sha512su0 v28.2d, v29.2d", 28, 29)
GEN_TWOVEC_TEST(sha512su0_29_30, "sha512su0 v29.2d, v30.2d", 29, 30)
GEN_TWOVEC_TEST(sha512su0_30_31, "sha512su0 v30.2d, v31.2d", 30, 31)

// SHA512SU1 <Vd>.2D, <Vn>.2D, <Vm>.2D

GEN_THREEVEC_TEST(sha512su1_00_01_02, "sha512su1 v0.2d, v1.2d, v2.2d", 0, 1, 2)
GEN_THREEVEC_TEST(sha512su1_01_02_03, "sha512su1 v1.2d, v2.2d, v3.2d", 1, 2, 3)
GEN_THREEVEC_TEST(sha512su1_02_03_04, "sha512su1 v2.2d, v3.2d, v4.2d", 2, 3, 4)
GEN_THREEVEC_TEST(sha512su1_03_04_05, "sha512su1 v3.2d, v4.2d, v5.2d", 3, 4, 5)
GEN_THREEVEC_TEST(sha512su1_04_05_06, "sha512su1 v4.2d, v5.2d, v6.2d", 4, 5, 6)
GEN_THREEVEC_TEST(sha512su1_05_06_07, "sha512su1 v5.2d, v6.2d, v7.2d", 5, 6, 7)
GEN_THREEVEC_TEST(sha512su1_06_07_08, "sha512su1 v6.2d, v7.2d, v8.2d", 6, 7, 8)
GEN_THREEVEC_TEST(sha512su1_07_08_09, "sha512su1 v7.2d, v8.2d, v9.2d", 7, 8, 9)
GEN_THREEVEC_TEST(sha512su1_08_09_10, "sha512su1 v8.2d, v9.2d, v10.2d", 8, 9, 10)
GEN_THREEVEC_TEST(sha512su1_09_10_11, "sha512su1 v9.2d, v10.2d, v11.2d", 9, 10, 11)
GEN_THREEVEC_TEST(sha512su1_10_11_12, "sha512su1 v10.2d, v11.2d, v12.2d", 10, 11, 12)
GEN_THREEVEC_TEST(sha512su1_11_12_13, "sha512su1 v11.2d, v12.2d, v13.2d", 11, 12, 13)
GEN_THREEVEC_TEST(sha512su1_12_13_14, "sha512su1 v12.2d, v13.2d, v14.2d", 12, 13, 14)
GEN_THREEVEC_TEST(sha512su1_13_14_15, "sha512su1 v13.2d, v14.2d, v15.2d", 13, 14, 15)
GEN_THREEVEC_TEST(sha512su1_14_15_16, "sha512su1 v14.2d, v15.2d, v16.2d", 14, 15, 16)
GEN_THREEVEC_TEST(sha512su1_15_16_17, "sha512su1 v15.2d, v16.2d, v17.2d", 15, 16, 17)
GEN_THREEVEC_TEST(sha512su1_16_17_18, "sha512su1 v16.2d, v17.2d, v18.2d", 16, 17, 18)
GEN_THREEVEC_TEST(sha512su1_17_18_19, "sha512su1 v17.2d, v18.2d, v19.2d", 17, 18, 19)
GEN_THREEVEC_TEST(sha512su1_18_19_20, "sha512su1 v18.2d, v19.2d, v20.2d", 18, 19, 20)
GEN_THREEVEC_TEST(sha512su1_19_20_21, "sha512su1 v19.2d, v20.2d, v21.2d", 19, 20, 21)
GEN_THREEVEC_TEST(sha512su1_20_21_22, "sha512su1 v20.2d, v21.2d, v22.2d", 20, 21, 22)
GEN_THREEVEC_TEST(sha512su1_21_22_23, "sha512su1 v21.2d, v22.2d, v23.2d", 21, 22, 23)
GEN_THREEVEC_TEST(sha512su1_22_23_24, "sha512su1 v22.2d, v23.2d, v24.2d", 22, 23, 24)
GEN_THREEVEC_TEST(sha512su1_23_24_25, "sha512su1 v23.2d, v24.2d, v25.2d", 23, 24, 25)
GEN_THREEVEC_TEST(sha512su1_24_25_26, "sha512su1 v24.2d, v25.2d, v26.2d", 24, 25, 26)
GEN_THREEVEC_TEST(sha512su1_25_26_27, "sha512su1 v25.2d, v26.2d, v27.2d", 25, 26, 27)
GEN_THREEVEC_TEST(sha512su1_26_27_28, "sha512su1 v26.2d, v27.2d, v28.2d", 26, 27, 28)
GEN_THREEVEC_TEST(sha512su1_27_28_29, "sha512su1 v27.2d, v28.2d, v29.2d", 27, 28, 29)
GEN_THREEVEC_TEST(sha512su1_28_29_30, "sha512su1 v28.2d, v29.2d, v30.2d", 28, 29, 30)
GEN_THREEVEC_TEST(sha512su1_29_30_31, "sha512su1 v29.2d, v30.2d, v31.2d", 29, 30, 31)

/* ---------------------------------------------------------------- */
/* -- main()                                                     -- */
/* ---------------------------------------------------------------- */

int main ( void )
{
   assert(sizeof(V128) == 16);

   printf("\nSHA512H <Qd>, <Qn>, <Vm>.2D\n\n");

   if (1) test_sha512h_00_01_02(TyD);
   if (1) test_sha512h_01_02_03(TyD);
   if (1) test_sha512h_02_03_04(TyD);
   if (1) test_sha512h_03_04_05(TyD);
   if (1) test_sha512h_04_05_06(TyD);
   if (1) test_sha512h_05_06_07(TyD);
   if (1) test_sha512h_06_07_08(TyD);
   if (1) test_sha512h_07_08_09(TyD);
   if (1) test_sha512h_08_09_10(TyD);
   if (1) test_sha512h_09_10_11(TyD);
   if (1) test_sha512h_10_11_12(TyD);
   if (1) test_sha512h_11_12_13(TyD);
   if (1) test_sha512h_12_13_14(TyD);
   if (1) test_sha512h_13_14_15(TyD);
   if (1) test_sha512h_14_15_16(TyD);
   if (1) test_sha512h_15_16_17(TyD);
   if (1) test_sha512h_16_17_18(TyD);
   if (1) test_sha512h_17_18_19(TyD);
   if (1) test_sha512h_18_19_20(TyD);
   if (1) test_sha512h_19_20_21(TyD);
   if (1) test_sha512h_20_21_22(TyD);
   if (1) test_sha512h_21_22_23(TyD);
   if (1) test_sha512h_22_23_24(TyD);
   if (1) test_sha512h_23_24_25(TyD);
   if (1) test_sha512h_24_25_26(TyD);
   if (1) test_sha512h_25_26_27(TyD);
   if (1) test_sha512h_26_27_28(TyD);
   if (1) test_sha512h_27_28_29(TyD);
   if (1) test_sha512h_28_29_30(TyD);
   if (1) test_sha512h_29_30_31(TyD);

   printf("\nSHA512H2 <Qd>, <Qn>, <Vm>.2D\n\n");

   if (1) test_sha512h2_00_01_02(TyD);
   if (1) test_sha512h2_01_02_03(TyD);
   if (1) test_sha512h2_02_03_04(TyD);
   if (1) test_sha512h2_03_04_05(TyD);
   if (1) test_sha512h2_04_05_06(TyD);
   if (1) test_sha512h2_05_06_07(TyD);
   if (1) test_sha512h2_06_07_08(TyD);
   if (1) test_sha512h2_07_08_09(TyD);
   if (1) test_sha512h2_08_09_10(TyD);
   if (1) test_sha512h2_09_10_11(TyD);
   if (1) test_sha512h2_10_11_12(TyD);
   if (1) test_sha512h2_11_12_13(TyD);
   if (1) test_sha512h2_12_13_14(TyD);
   if (1) test_sha512h2_13_14_15(TyD);
   if (1) test_sha512h2_14_15_16(TyD);
   if (1) test_sha512h2_15_16_17(TyD);
   if (1) test_sha512h2_16_17_18(TyD);
   if (1) test_sha512h2_17_18_19(TyD);
   if (1) test_sha512h2_18_19_20(TyD);
   if (1) test_sha512h2_19_20_21(TyD);
   if (1) test_sha512h2_20_21_22(TyD);
   if (1) test_sha512h2_21_22_23(TyD);
   if (1) test_sha512h2_22_23_24(TyD);
   if (1) test_sha512h2_23_24_25(TyD);
   if (1) test_sha512h2_24_25_26(TyD);
   if (1) test_sha512h2_25_26_27(TyD);
   if (1) test_sha512h2_26_27_28(TyD);
   if (1) test_sha512h2_27_28_29(TyD);
   if (1) test_sha512h2_28_29_30(TyD);
   if (1) test_sha512h2_29_30_31(TyD);

   printf("\nSHA512SU0 <Vd>.2D, <Vn>.2D\n\n");

   if (1) test_sha512su0_00_01(TyD);
   if (1) test_sha512su0_01_02(TyD);
   if (1) test_sha512su0_02_03(TyD);
   if (1) test_sha512su0_03_04(TyD);
   if (1) test_sha512su0_04_05(TyD);
   if (1) test_sha512su0_05_06(TyD);
   if (1) test_sha512su0_06_07(TyD);
   if (1) test_sha512su0_07_08(TyD);
   if (1) test_sha512su0_08_09(TyD);
   if (1) test_sha512su0_09_10(TyD);
   if (1) test_sha512su0_10_11(TyD);
   if (1) test_sha512su0_11_12(TyD);
   if (1) test_sha512su0_12_13(TyD);
   if (1) test_sha512su0_13_14(TyD);
   if (1) test_sha512su0_14_15(TyD);
   if (1) test_sha512su0_15_16(TyD);
   if (1) test_sha512su0_16_17(TyD);
   if (1) test_sha512su0_17_18(TyD);
   if (1) test_sha512su0_18_19(TyD);
   if (1) test_sha512su0_19_20(TyD);
   if (1) test_sha512su0_20_21(TyD);
   if (1) test_sha512su0_21_22(TyD);
   if (1) test_sha512su0_22_23(TyD);
   if (1) test_sha512su0_23_24(TyD);
   if (1) test_sha512su0_24_25(TyD);
   if (1) test_sha512su0_25_26(TyD);
   if (1) test_sha512su0_26_27(TyD);
   if (1) test_sha512su0_27_28(TyD);
   if (1) test_sha512su0_28_29(TyD);
   if (1) test_sha512su0_29_30(TyD);
   if (1) test_sha512su0_30_31(TyD);

   printf("\nSHA512SU1 <Vd>.2D, <Vn>.2D, <Vm>.2D\n\n");

   if (1) test_sha512su1_00_01_02(TyD);
   if (1) test_sha512su1_01_02_03(TyD);
   if (1) test_sha512su1_02_03_04(TyD);
   if (1) test_sha512su1_03_04_05(TyD);
   if (1) test_sha512su1_04_05_06(TyD);
   if (1) test_sha512su1_05_06_07(TyD);
   if (1) test_sha512su1_06_07_08(TyD);
   if (1) test_sha512su1_07_08_09(TyD);
   if (1) test_sha512su1_08_09_10(TyD);
   if (1) test_sha512su1_09_10_11(TyD);
   if (1) test_sha512su1_10_11_12(TyD);
   if (1) test_sha512su1_11_12_13(TyD);
   if (1) test_sha512su1_12_13_14(TyD);
   if (1) test_sha512su1_13_14_15(TyD);
   if (1) test_sha512su1_14_15_16(TyD);
   if (1) test_sha512su1_15_16_17(TyD);
   if (1) test_sha512su1_16_17_18(TyD);
   if (1) test_sha512su1_17_18_19(TyD);
   if (1) test_sha512su1_18_19_20(TyD);
   if (1) test_sha512su1_19_20_21(TyD);
   if (1) test_sha512su1_20_21_22(TyD);
   if (1) test_sha512su1_21_22_23(TyD);
   if (1) test_sha512su1_22_23_24(TyD);
   if (1) test_sha512su1_23_24_25(TyD);
   if (1) test_sha512su1_24_25_26(TyD);
   if (1) test_sha512su1_25_26_27(TyD);
   if (1) test_sha512su1_26_27_28(TyD);
   if (1) test_sha512su1_27_28_29(TyD);
   if (1) test_sha512su1_28_29_30(TyD);
   if (1) test_sha512su1_29_30_31(TyD);

   return 0;
}

/* ---------------------------------------------------------------- */
/* -- List of instructions tested in order of execution.         -- */
/* -- Useful strings when searching for blocks of test cases.    -- */
/* ---------------------------------------------------------------- */
/*
   SHA512H <Qd>, <Qn>, <Vm>.2D SHA512 Hash update part 1
   SHA512H2 <Qd>, <Qn>, <Vm>.2D SHA512 Hash update part 2
   SHA512SU0 <Vd>.2D, <Vn>.2D SHA512 Schedule Update 0
   SHA512SU1 <Vd>.2D, <Vn>.2D, <Vm>.2D SHA512 Schedule Update 1
*/
