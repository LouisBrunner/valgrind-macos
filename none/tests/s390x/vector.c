#include "vector.h"


s390_test_generate(vl_vst, "vl %%v5, %[v_arg1]")
s390_test_generate(vlr, "vlr %%v5, %%v1")

s390_test_generate(vlrep, "vlrepg %%v1, %[m_arg1]\n" \
                          "vlrepf %%v2, %[m_arg2]\n" \
                          "vlreph %%v3, %[m_arg3]\n" \
                          "vlrepb %%v5, %[m_arg1]")

s390_test_generate(vle, "vleb %%v5, %[m_arg1], 14\n" \
                        "vleh %%v5, %[m_arg2],  6\n" \
                        "vlef %%v5, %[m_arg3],  2\n" \
                        "vleg %%v5, %[m_arg1],  0")

s390_test_generate(vlei_pos, "vleib %%v5, 0x091a, 14\n" \
                             "vleih %%v5, 0x1bbb,  6\n" \
                             "vleif %%v5, 0x1ccc,  2\n" \
                             "vleig %%v5, 0x1ddd,  0")

s390_test_generate(vlei_neg, "vleib %%v5, -0x091a, 14\n" \
                             "vleih %%v5, -0x1bbb,  6\n" \
                             "vleif %%v5, -0x1ccc,  2\n" \
                             "vleig %%v5, -0x1ddd,  0")

s390_test_generate(vlvg, "vlvgb %%v1, %[r_arg1], 12(0)\n" \
                         "vlvgh %%v2, %[r_arg2],  6(0)\n" \
                         "vlvgf %%v3, %[r_arg3],  2(0)\n" \
                         "vlvgg %%v5, %[r_arg1],  1(0)")

s390_test_generate(vgbm, "vgbm  %%v1, 0xf00f \n" \
                         "vzero %%v2 \n" \
                         "vone  %%v5")

s390_test_generate(vgm, "vgmb %%v1, 4,  6 \n" \
                        "vgmh %%v2, 2, 14 \n" \
                        "vgmf %%v3, 3, 23 \n"
                        "vgmg %%v3, 1, 55")

s390_test_generate(vllez, "vllezb %%v1, %[m_arg1]\n" \
                          "vllezh %%v2, %[m_arg2]\n" \
                          "vllezf %%v3, %[m_arg3]\n"
                          "vllezg %%v5, %[m_arg1]")

s390_test_generate(vgef, "vzero %%v1\n" \
                         "vgef %%v5, 16(%%v1, %[r_memory_pool]), 2 \n")
s390_test_generate(vgeg, "vzero %%v1\n" \
                         "vgeg %%v5, 32(%%v1, %[r_memory_pool]), 1 \n")

s390_test_generate(vlm_vstm, "vlm  %%v1, %%v3, %[m_memory_pool]\n" \
                             "vstm %%v1, %%v3, %[m_memory_pool]\n")

s390_test_generate(vlvgp, "vlvgp %%v1, %[r_arg1], %[r_arg2]")

s390_test_generate(vlgv, "vlgvb %[r_arg1], %%v1, 12(0)\n" \
                         "vlgvh %[r_arg2], %%v2,  6(0)\n" \
                         "vlgvf %[r_arg3], %%v3,  2(0)\n"
                         "vlgvg %[r_arg1], %%v5,  1(0)")

s390_test_generate(vmrhb, "vmrhb %%v5, %%v1, %%v2")
s390_test_generate(vmrhh, "vmrhh %%v5, %%v1, %%v2")
s390_test_generate(vmrhf, "vmrhf %%v5, %%v1, %%v2")
s390_test_generate(vmrhg, "vmrhg %%v5, %%v1, %%v2")

s390_test_generate(vmrlb, "vmrlb %%v5, %%v1, %%v2")
s390_test_generate(vmrlh, "vmrlh %%v5, %%v1, %%v2")
s390_test_generate(vmrlf, "vmrlf %%v5, %%v1, %%v2")
s390_test_generate(vmrlg, "vmrlg %%v5, %%v1, %%v2")

s390_test_generate(vpkh,   "vpkh %%v5, %%v1, %%v2")
s390_test_generate(vpkf,   "vpkf %%v5, %%v1, %%v2")
s390_test_generate(vpkg,   "vpkg %%v5, %%v1, %%v2")
s390_test_generate(vpksh,  "vpksh %%v5, %%v1, %%v2")
s390_test_generate(vpksf,  "vpksf %%v5, %%v1, %%v2")
s390_test_generate(vpksg,  "vpksg %%v5, %%v1, %%v2")
s390_test_generate(vpklsh, "vpklsh %%v5, %%v1, %%v2")
s390_test_generate(vpklsf, "vpklsf %%v5, %%v1, %%v2")
s390_test_generate(vpklsg, "vpklsg %%v5, %%v1, %%v2")

s390_test_generate(vpkshs,  "vpkshs %%v5, %%v1, %%v2\n" \
                            "ipm %[r_result]\n" \
                            "srl %[r_result], 28")
s390_test_generate(vpksfs,  "vpksfs %%v5, %%v1, %%v2\n" \
                            "ipm %[r_result]\n" \
                            "srl %[r_result], 28")
s390_test_generate(vpksgs,  "vpksgs %%v5, %%v1, %%v2\n" \
                            "ipm %[r_result]\n" \
                            "srl %[r_result], 28")
s390_test_generate(vpklshs, "vpklshs %%v5, %%v1, %%v2\n" \
                            "ipm %[r_result]\n" \
                            "srl %[r_result], 28")
s390_test_generate(vpklsfs, "vpklsfs %%v5, %%v1, %%v2\n" \
                            "ipm %[r_result]\n" \
                            "srl %[r_result], 28")
s390_test_generate(vpklsgs, "vpklsgs %%v5, %%v1, %%v2\n" \
                            "ipm %[r_result]\n" \
                            "srl %[r_result], 28")

s390_test_generate(vperm, "vperm %%v5, %%v1, %%v2, %%v3")

s390_test_generate(vpdi0, "vpdi %%v5, %%v1, %%v2, 0")
s390_test_generate(vpdi1, "vpdi %%v5, %%v1, %%v2, 1")
s390_test_generate(vpdi4, "vpdi %%v5, %%v1, %%v2, 4")
s390_test_generate(vpdi5, "vpdi %%v5, %%v1, %%v2, 5")

s390_test_generate(vscef, "vzero %%v1\n" \
                          "vscef %%v5, 0(%%v1, %[r_memory_pool]), 1")
s390_test_generate(vsceg, "vzero %%v1\n" \
                          "vsceg %%v5, 16(%%v1, %[r_memory_pool]), 1")

s390_test_generate(vsegb, "vsegb %%v5, %%v1")
s390_test_generate(vsegh, "vsegh %%v5, %%v1")
s390_test_generate(vsegf, "vsegf %%v5, %%v1")

s390_test_generate(vste, "vsteb %%v1, %[m_arg1], 4\n" \
                         "vsteh %%v1, %[m_arg2], 3\n" \
                         "vstef %%v1, %[m_arg3], 2\n" \
                         "vsteg %%v1, %[m_arg1], 1\n")

s390_test_generate(vuph, "vuphb %%v2, %%v1\n" \
                         "vuphh %%v3, %%v1\n" \
                         "vuphf %%v5, %%v1")
s390_test_generate(vuplh, "vuplhb %%v2, %%v1\n" \
                          "vuplhh %%v3, %%v1\n" \
                          "vuplhf %%v5, %%v1")
s390_test_generate(vupl, "vuplb  %%v2, %%v1\n" \
                         "vuplhw %%v3, %%v1\n" \
                         "vuplf  %%v5, %%v1")
s390_test_generate(vupll, "vupllb %%v2, %%v1\n" \
                          "vupllh %%v3, %%v1\n" \
                          "vupllf %%v5, %%v1")

s390_test_generate(vrepb, "vrepb %%v5, %%v1, 1")
s390_test_generate(vreph, "vreph %%v5, %%v1, 1")
s390_test_generate(vrepf, "vrepf %%v5, %%v1, 1")
s390_test_generate(vrepg, "vrepg %%v5, %%v1, 1")

s390_test_generate(vrepib, "vrepib %%v5, -0x0fee")
s390_test_generate(vrepih, "vrepih %%v5, -0x0fee")
s390_test_generate(vrepif, "vrepif %%v5, -0x0fee")
s390_test_generate(vrepig, "vrepig %%v5, -0x0fee")

s390_test_generate(vsel, "vsel  %%v5, %%v1, %%v2, %%v3")
s390_test_generate(vll_vstl, "vll  %%v5, %[r_arg1], %[v_arg1]\n" \
                             "vstl %%v5, %[r_arg1], %[v_arg2]\n" \
                             "vl   %%v2, %[v_arg2]")

s390_test_generate(vlbb, "vlbb  %%v5, 60(%[r_memory_pool]), 0")

/* Test the correctness of work with VR's > 16.
   VSEL is choosed just because it have four arguments.

   The algorithm (the tested VR's are %%v21 - %%v24):
      - save tested VR's to temporary location
      - copy compile-time known values to tested VR's
      - perform VSEL on tested VR's
      - copy tested VR's to printed results
      - restore saved VR's from temporary location
 */
s390_test_generate(test_upper16_registers, \
                   "vstm  %%v21, %%v24, %[m_memory_pool]\n" \
                   \
                   "vlr %%v21, %%v1\n" \
                   "vlr %%v22, %%v2\n" \
                   "vlr %%v23, %%v3\n" \
                   "vlr %%v24, %%v5\n" \
                   \
                   "vsel %%v24, %%v21, %%v22, %%v23\n" \
                   \
                   "vlr  %%v1, %%v21\n" \
                   "vlr  %%v2, %%v22\n" \
                   "vlr  %%v3, %%v23\n" \
                   "vlr  %%v5, %%v24\n" \
                   \
                   "vlm  %%v21, %%v24, %[m_memory_pool]\n")

int main() {
   size_t iteration = 0;
   randomize_memory_pool();

   /* Some of insn depend only on immediate arguments, which are known at compile time.
      There is no need to run them more than once. We use test_once() for them.
    */

   /* Theese insn are used in every test. Test them first */
   test(vl_vst);
   test_once(vgbm);

   test(vlr);
   test(vlrep);

   test(vle);

   test(vlei_pos);
   test(vlei_neg);

   test(vlgv);
   test(vgm);

   test(vllez);
   test(vlvgp);
   test(vlvg);

   test(vmrhb);
   test(vmrhh);
   test(vmrhf);
   test(vmrhg);

   test(vmrlb);
   test(vmrlh);
   test(vmrlf);
   test(vmrlg);

   test(vpkh);
   test(vpkf);
   test(vpkg);

   test(vperm);
   test(vpdi0);
   test(vpdi1);
   test(vpdi4);
   test(vpdi5);

   test(vsegb);
   test(vsegh);
   test(vsegf);

   test(vste);

   test(vuph);
   test(vuplh);
   test(vupl);
   test(vupll);

   test(vrepb);
   test(vreph);
   test(vrepf);
   test(vrepg);

   test_once(vrepib);
   test_once(vrepih);
   test_once(vrepif);
   test_once(vrepig);

   test(vpksh);
   test(vpkshs);
   test(vpksf);
   test(vpksfs);
   test(vpksg);
   test(vpksgs);

   test(vpklsh);
   test(vpklshs);
   test(vpklsf);
   test(vpklsfs);
   test(vpklsg);
   test(vpklsgs);

   test(vsel);
   test(vll_vstl);

   test(vlbb, (randomize_memory_pool()));
   test(vlm_vstm, (randomize_memory_pool()));
   test(vgef, (randomize_memory_pool()));
   test(vgeg, (randomize_memory_pool()));
   test(vscef, (randomize_memory_pool()));
   test(vsceg, (randomize_memory_pool()));

   test_once(test_upper16_registers);

   return 0;
}
