#include "vector.h"

#define s390_generate_float_test(insn, asm_string) \
   s390_test_generate(v##insn##00,   "v" #insn " " asm_string ",0, 0") \
   s390_test_generate(v##insn##01,   "v" #insn " " asm_string ",0, 1") \
   s390_test_generate(v##insn##03,   "v" #insn " " asm_string ",0, 3") \
   s390_test_generate(v##insn##04,   "v" #insn " " asm_string ",0, 4") \
   s390_test_generate(v##insn##05,   "v" #insn " " asm_string ",0, 5") \
   s390_test_generate(v##insn##06,   "v" #insn " " asm_string ",0, 6") \
   s390_test_generate(v##insn##07,   "v" #insn " " asm_string ",0, 7") \
   s390_test_generate(w##insn##00,   "w" #insn " " asm_string ",0, 0") \
   s390_test_generate(w##insn##01,   "w" #insn " " asm_string ",0, 1") \
   s390_test_generate(w##insn##03,   "w" #insn " " asm_string ",0, 3") \
   s390_test_generate(w##insn##04,   "w" #insn " " asm_string ",0, 4") \
   s390_test_generate(w##insn##05,   "w" #insn " " asm_string ",0, 5") \
   s390_test_generate(w##insn##06,   "w" #insn " " asm_string ",0, 6") \
   s390_test_generate(w##insn##07,   "w" #insn " " asm_string ",0, 7") \

#define s390_call_float_test(insn, info) \
   test_with_selective_printing(v ##insn ## 00, info); \
   test_with_selective_printing(v ##insn ## 01, info); \
   test_with_selective_printing(v ##insn ## 03, info); \
   test_with_selective_printing(v ##insn ## 04, info); \
   test_with_selective_printing(v ##insn ## 05, info); \
   test_with_selective_printing(v ##insn ## 06, info); \
   test_with_selective_printing(v ##insn ## 07, info); \
   test_with_selective_printing(w ##insn ## 00, info | V128_V_RES_ZERO_ONLY); \
   test_with_selective_printing(w ##insn ## 01, info | V128_V_RES_ZERO_ONLY); \
   test_with_selective_printing(w ##insn ## 03, info | V128_V_RES_ZERO_ONLY); \
   test_with_selective_printing(w ##insn ## 04, info | V128_V_RES_ZERO_ONLY); \
   test_with_selective_printing(w ##insn ## 05, info | V128_V_RES_ZERO_ONLY); \
   test_with_selective_printing(w ##insn ## 06, info | V128_V_RES_ZERO_ONLY); \
   test_with_selective_printing(w ##insn ## 07, info | V128_V_RES_ZERO_ONLY); \

s390_generate_float_test(cdgb, " %%v5, %%v1")
s390_generate_float_test(cdlgb, " %%v5, %%v1")
s390_generate_float_test(cgdb, " %%v5, %%v1")
s390_generate_float_test(clgdb, " %%v5, %%v1")
s390_generate_float_test(fidb, " %%v5, %%v1")
s390_generate_float_test(ledb, " %%v5, %%v1")

s390_test_generate(vldeb, "vldeb %%v5, %%v1")
s390_test_generate(wldeb, "wldeb %%v5, %%v1")

s390_test_generate(vflcdb, "vflcdb %%v5, %%v1")
s390_test_generate(wflcdb, "wflcdb %%v5, %%v1")
s390_test_generate(vflndb, "vflndb %%v5, %%v1")
s390_test_generate(wflndb, "wflndb %%v5, %%v1")
s390_test_generate(vflpdb, "vflpdb %%v5, %%v1")
s390_test_generate(wflpdb, "wflpdb %%v5, %%v1")

s390_test_generate(vfadb, "vfadb %%v5, %%v1, %%v2")
s390_test_generate(wfadb, "wfadb %%v5, %%v1, %%v2")
s390_test_generate(vfsdb, "vfsdb %%v5, %%v1, %%v2")
s390_test_generate(wfsdb, "wfsdb %%v5, %%v1, %%v2")
s390_test_generate(vfmdb, "vfmdb %%v5, %%v1, %%v2")
s390_test_generate(wfmdb, "wfmdb %%v5, %%v1, %%v2")
s390_test_generate(vfddb, "vfddb %%v5, %%v1, %%v2")
s390_test_generate(wfddb, "wfddb %%v5, %%v1, %%v2")

s390_test_generate(vfsqdb, "vfsqdb %%v5, %%v1")
s390_test_generate(wfsqdb, "wfsqdb %%v5, %%v1")

s390_test_generate(vfmadb, "vfmadb %%v5, %%v1, %%v2, %%v3")
s390_test_generate(wfmadb, "wfmadb %%v5, %%v1, %%v2, %%v3")
s390_test_generate(vfmsdb, "vfmsdb %%v5, %%v1, %%v2, %%v3")
s390_test_generate(wfmsdb, "wfmsdb %%v5, %%v1, %%v2, %%v3")

s390_test_generate(wfcdb, "wfcdb %%v1, %%v2\n" S390_TEST_PUT_CC_TO_RESULT)
s390_test_generate(wfkdb, "wfkdb %%v1, %%v2\n" S390_TEST_PUT_CC_TO_RESULT)

s390_test_generate(vfcedb,  "vfcedb  %%v5, %%v1, %%v2")
s390_test_generate(wfcedb,  "wfcedb  %%v5, %%v1, %%v2")
s390_test_generate(vfcedbs, "vfcedbs %%v5, %%v1, %%v2\n" S390_TEST_PUT_CC_TO_RESULT)
s390_test_generate(wfcedbs, "wfcedbs %%v5, %%v1, %%v2\n" S390_TEST_PUT_CC_TO_RESULT)

s390_test_generate(vfchdb,  "vfchdb  %%v5, %%v1, %%v2")
s390_test_generate(wfchdb,  "wfchdb  %%v5, %%v1, %%v2")
s390_test_generate(vfchdbs, "vfchdbs %%v5, %%v1, %%v2\n" S390_TEST_PUT_CC_TO_RESULT)
s390_test_generate(wfchdbs, "wfchdbs %%v5, %%v1, %%v2\n" S390_TEST_PUT_CC_TO_RESULT)

s390_test_generate(vfchedb,  "vfchedb  %%v5, %%v1, %%v2")
s390_test_generate(wfchedb,  "wfchedb  %%v5, %%v1, %%v2")
s390_test_generate(vfchedbs, "vfchedbs %%v5, %%v1, %%v2\n" S390_TEST_PUT_CC_TO_RESULT)
s390_test_generate(wfchedbs, "wfchedbs %%v5, %%v1, %%v2\n" S390_TEST_PUT_CC_TO_RESULT)

s390_test_generate(vftcidb0,    "vftcidb %%v5, %%v1,  0  \n" S390_TEST_PUT_CC_TO_RESULT)
s390_test_generate(vftcidb1,    "vftcidb %%v5, %%v1,  1  \n" S390_TEST_PUT_CC_TO_RESULT)
s390_test_generate(vftcidb2,    "vftcidb %%v5, %%v1,  2  \n" S390_TEST_PUT_CC_TO_RESULT)
s390_test_generate(vftcidb3,    "vftcidb %%v5, %%v1,  0  \n" S390_TEST_PUT_CC_TO_RESULT)
s390_test_generate(vftcidb4,    "vftcidb %%v5, %%v1,  4  \n" S390_TEST_PUT_CC_TO_RESULT)
s390_test_generate(vftcidb8,    "vftcidb %%v5, %%v1,  8  \n" S390_TEST_PUT_CC_TO_RESULT)
s390_test_generate(vftcidb16,   "vftcidb %%v5, %%v1, 16  \n" S390_TEST_PUT_CC_TO_RESULT)
s390_test_generate(vftcidb32,   "vftcidb %%v5, %%v1, 32  \n" S390_TEST_PUT_CC_TO_RESULT)
s390_test_generate(vftcidb64,   "vftcidb %%v5, %%v1, 64  \n" S390_TEST_PUT_CC_TO_RESULT)
s390_test_generate(vftcidb128,  "vftcidb %%v5, %%v1, 128 \n" S390_TEST_PUT_CC_TO_RESULT)
s390_test_generate(vftcidb256,  "vftcidb %%v5, %%v1, 256 \n" S390_TEST_PUT_CC_TO_RESULT)
s390_test_generate(vftcidb512,  "vftcidb %%v5, %%v1, 512 \n" S390_TEST_PUT_CC_TO_RESULT)
s390_test_generate(vftcidb1024, "vftcidb %%v5, %%v1, 1024\n" S390_TEST_PUT_CC_TO_RESULT)
s390_test_generate(vftcidb2048, "vftcidb %%v5, %%v1, 2048\n" S390_TEST_PUT_CC_TO_RESULT)

int main()
{
   size_t iteration = 0;

   s390_call_float_test(cdgb, (V128_V_RES_AS_FLOAT64 | V128_V_ARG1_AS_INT));
   s390_call_float_test(cdlgb, (V128_V_RES_AS_FLOAT64 | V128_V_ARG1_AS_INT));
   s390_call_float_test(cgdb, (V128_V_RES_AS_INT | V128_V_ARG1_AS_FLOAT64));
   s390_call_float_test(clgdb, (V128_V_RES_AS_INT | V128_V_ARG1_AS_FLOAT64));
   s390_call_float_test(fidb, (V128_V_RES_AS_FLOAT64 | V128_V_ARG1_AS_FLOAT64));
   s390_call_float_test(ledb, (V128_V_RES_AS_FLOAT32 | V128_V_RES_EVEN_ONLY |
                               V128_V_ARG1_AS_FLOAT64));

   test_with_selective_printing(vldeb, (V128_V_RES_AS_FLOAT64 |
                                        V128_V_ARG1_AS_FLOAT64));
   test_with_selective_printing(wldeb, (V128_V_RES_AS_FLOAT64 |
                                        V128_V_ARG1_AS_FLOAT64 |
                                        V128_V_RES_ZERO_ONLY));

   test_with_selective_printing(vflcdb, (V128_V_RES_AS_FLOAT64 |
                                         V128_V_ARG1_AS_FLOAT64));
   test_with_selective_printing(wflcdb, (V128_V_RES_AS_FLOAT64 |
                                         V128_V_ARG1_AS_FLOAT64 |
                                        V128_V_RES_ZERO_ONLY));
   test_with_selective_printing(vflndb, (V128_V_RES_AS_FLOAT64 |
                                         V128_V_ARG1_AS_FLOAT64));
   test_with_selective_printing(wflndb, (V128_V_RES_AS_FLOAT64 |
                                         V128_V_ARG1_AS_FLOAT64 |
                                        V128_V_RES_ZERO_ONLY));
   test_with_selective_printing(vflpdb, (V128_V_RES_AS_FLOAT64 |
                                         V128_V_ARG1_AS_FLOAT64));
   test_with_selective_printing(wflpdb, (V128_V_RES_AS_FLOAT64 |
                                         V128_V_ARG1_AS_FLOAT64 |
                                        V128_V_RES_ZERO_ONLY));

   test_with_selective_printing(vfadb, (V128_V_RES_AS_FLOAT64 |
                                        V128_V_ARG1_AS_FLOAT64 |
                                        V128_V_ARG2_AS_FLOAT64));
   test_with_selective_printing(wfadb, (V128_V_RES_AS_FLOAT64 |
                                        V128_V_ARG1_AS_FLOAT64 |
                                        V128_V_ARG2_AS_FLOAT64 |
                                        V128_V_RES_ZERO_ONLY));
   test_with_selective_printing(vfsdb, (V128_V_RES_AS_FLOAT64 |
                                        V128_V_ARG1_AS_FLOAT64 |
                                        V128_V_ARG2_AS_FLOAT64));
   test_with_selective_printing(wfsdb, (V128_V_RES_AS_FLOAT64 |
                                        V128_V_ARG1_AS_FLOAT64 |
                                        V128_V_ARG2_AS_FLOAT64 |
                                        V128_V_RES_ZERO_ONLY));
   test_with_selective_printing(vfmdb, (V128_V_RES_AS_FLOAT64 |
                                        V128_V_ARG1_AS_FLOAT64 |
                                        V128_V_ARG2_AS_FLOAT64));
   test_with_selective_printing(wfmdb, (V128_V_RES_AS_FLOAT64 |
                                        V128_V_ARG1_AS_FLOAT64 |
                                        V128_V_ARG2_AS_FLOAT64 |
                                        V128_V_RES_ZERO_ONLY));
   test_with_selective_printing(vfddb, (V128_V_RES_AS_FLOAT64 |
                                        V128_V_ARG1_AS_FLOAT64 |
                                        V128_V_ARG2_AS_FLOAT64));
   test_with_selective_printing(wfddb, (V128_V_RES_AS_FLOAT64 |
                                        V128_V_ARG1_AS_FLOAT64 |
                                        V128_V_ARG2_AS_FLOAT64 |
                                        V128_V_RES_ZERO_ONLY));
 
   test_with_selective_printing(vfsqdb, (V128_V_RES_AS_FLOAT64 |
                                         V128_V_ARG1_AS_FLOAT64));
   test_with_selective_printing(wfsqdb, (V128_V_RES_AS_FLOAT64 |
                                         V128_V_ARG1_AS_FLOAT64 |
                                        V128_V_RES_ZERO_ONLY));

   test_with_selective_printing(vfmadb, (V128_V_RES_AS_FLOAT64 |
                                         V128_V_ARG1_AS_FLOAT64 |
                                         V128_V_ARG2_AS_FLOAT64 |
                                         V128_V_ARG3_AS_FLOAT64));
   test_with_selective_printing(wfmadb, (V128_V_RES_AS_FLOAT64 |
                                         V128_V_ARG1_AS_FLOAT64 |
                                         V128_V_ARG2_AS_FLOAT64 |
                                         V128_V_ARG3_AS_FLOAT64 |
                                        V128_V_RES_ZERO_ONLY));
   test_with_selective_printing(vfmsdb, (V128_V_RES_AS_FLOAT64 |
                                         V128_V_ARG1_AS_FLOAT64 |
                                         V128_V_ARG2_AS_FLOAT64 |
                                         V128_V_ARG3_AS_FLOAT64));
   test_with_selective_printing(wfmsdb, (V128_V_RES_AS_FLOAT64 |
                                         V128_V_ARG1_AS_FLOAT64 |
                                         V128_V_ARG2_AS_FLOAT64 |
                                         V128_V_ARG3_AS_FLOAT64 |
                                        V128_V_RES_ZERO_ONLY));

   test_with_selective_printing(wfcdb, (V128_V_ARG1_AS_FLOAT64 |
                                        V128_V_ARG2_AS_FLOAT64 |
                                        V128_R_RES |
                                        V128_V_RES_ZERO_ONLY));
   test_with_selective_printing(wfkdb, (V128_V_ARG1_AS_FLOAT64 |
                                        V128_V_ARG2_AS_FLOAT64 |
                                        V128_R_RES |
                                        V128_V_RES_ZERO_ONLY));

   test_with_selective_printing(vfcedb,  (V128_V_RES_AS_INT |
                                          V128_V_ARG1_AS_FLOAT64 |
                                          V128_V_ARG2_AS_FLOAT64));
   test_with_selective_printing(wfcedb,  (V128_V_RES_AS_INT |
                                          V128_V_ARG1_AS_FLOAT64 |
                                          V128_V_ARG2_AS_FLOAT64 |
                                          V128_V_RES_ZERO_ONLY));
   test_with_selective_printing(vfcedbs, (V128_V_RES_AS_INT |
                                          V128_V_ARG1_AS_FLOAT64 |
                                          V128_V_ARG2_AS_FLOAT64 |
                                          V128_R_RES));
   test_with_selective_printing(wfcedbs, (V128_V_RES_AS_INT |
                                          V128_V_ARG1_AS_FLOAT64 |
                                          V128_V_ARG2_AS_FLOAT64 |
                                          V128_R_RES |
                                          V128_V_RES_ZERO_ONLY));

   test_with_selective_printing(vfchdb,  (V128_V_RES_AS_INT |
                                          V128_V_ARG1_AS_FLOAT64 |
                                          V128_V_ARG2_AS_FLOAT64));
   test_with_selective_printing(wfchdb,  (V128_V_RES_AS_INT |
                                          V128_V_ARG1_AS_FLOAT64 |
                                          V128_V_ARG2_AS_FLOAT64 |
                                          V128_V_RES_ZERO_ONLY));
   test_with_selective_printing(vfchdbs, (V128_V_RES_AS_INT |
                                          V128_V_ARG1_AS_FLOAT64 |
                                          V128_V_ARG2_AS_FLOAT64 |
                                          V128_R_RES));
   test_with_selective_printing(wfchdbs, (V128_V_RES_AS_INT |
                                          V128_V_ARG1_AS_FLOAT64 |
                                          V128_V_ARG2_AS_FLOAT64 |
                                          V128_R_RES |
                                          V128_V_RES_ZERO_ONLY));

   test_with_selective_printing(vfchedb,  (V128_V_RES_AS_INT |
                                           V128_V_ARG1_AS_FLOAT64 |
                                           V128_V_ARG2_AS_FLOAT64));
   test_with_selective_printing(wfchedb,  (V128_V_RES_AS_INT |
                                           V128_V_ARG1_AS_FLOAT64 |
                                           V128_V_ARG2_AS_FLOAT64 |
                                           V128_V_RES_ZERO_ONLY));
   test_with_selective_printing(vfchedbs, (V128_V_RES_AS_INT |
                                           V128_V_ARG1_AS_FLOAT64 |
                                           V128_V_ARG2_AS_FLOAT64 |
                                           V128_R_RES));
   test_with_selective_printing(wfchedbs, (V128_V_RES_AS_INT |
                                           V128_V_ARG1_AS_FLOAT64 |
                                           V128_V_ARG2_AS_FLOAT64 |
                                           V128_R_RES |
                                           V128_V_RES_ZERO_ONLY));

   test_with_selective_printing(vftcidb0,    (V128_V_RES_AS_INT |
                                              V128_V_ARG1_AS_FLOAT64 |
                                              V128_R_RES));
   test_with_selective_printing(vftcidb1,    (V128_V_RES_AS_INT |
                                              V128_V_ARG1_AS_FLOAT64 |
                                              V128_R_RES));
   test_with_selective_printing(vftcidb2,    (V128_V_RES_AS_INT |
                                              V128_V_ARG1_AS_FLOAT64 |
                                              V128_R_RES));
   test_with_selective_printing(vftcidb3,    (V128_V_RES_AS_INT |
                                              V128_V_ARG1_AS_FLOAT64 |
                                              V128_R_RES));
   test_with_selective_printing(vftcidb4,    (V128_V_RES_AS_INT |
                                              V128_V_ARG1_AS_FLOAT64 |
                                              V128_R_RES));
   test_with_selective_printing(vftcidb8,    (V128_V_RES_AS_INT |
                                              V128_V_ARG1_AS_FLOAT64 |
                                              V128_R_RES));
   test_with_selective_printing(vftcidb16,   (V128_V_RES_AS_INT |
                                              V128_V_ARG1_AS_FLOAT64 |
                                              V128_R_RES));
   test_with_selective_printing(vftcidb32,   (V128_V_RES_AS_INT |
                                              V128_V_ARG1_AS_FLOAT64 |
                                              V128_R_RES));
   test_with_selective_printing(vftcidb64,   (V128_V_RES_AS_INT |
                                              V128_V_ARG1_AS_FLOAT64 |
                                              V128_R_RES));
   test_with_selective_printing(vftcidb128,  (V128_V_RES_AS_INT |
                                              V128_V_ARG1_AS_FLOAT64 |
                                              V128_R_RES));
   test_with_selective_printing(vftcidb256,  (V128_V_RES_AS_INT |
                                              V128_V_ARG1_AS_FLOAT64 |
                                              V128_R_RES));
   test_with_selective_printing(vftcidb512,  (V128_V_RES_AS_INT |
                                              V128_V_ARG1_AS_FLOAT64 |
                                              V128_R_RES));
   test_with_selective_printing(vftcidb1024, (V128_V_RES_AS_INT |
                                              V128_V_ARG1_AS_FLOAT64 |
                                              V128_R_RES));
   test_with_selective_printing(vftcidb2048, (V128_V_RES_AS_INT |
                                              V128_V_ARG1_AS_FLOAT64 |
                                              V128_R_RES));

   return 0;
}
