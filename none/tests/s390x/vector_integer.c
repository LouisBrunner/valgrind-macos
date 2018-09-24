#include "vector.h"

s390_test_generate(vab,   "vab %%v5, %%v1, %%v2")
s390_test_generate(vah,   "vah %%v5, %%v1, %%v2")
s390_test_generate(vaf,   "vaf %%v5, %%v1, %%v2")
s390_test_generate(vag,   "vag %%v5, %%v1, %%v2")
s390_test_generate(vaq,   "vaq %%v5, %%v1, %%v2")

s390_test_generate(vsb,   "vsb %%v5, %%v1, %%v2")
s390_test_generate(vsh,   "vsh %%v5, %%v1, %%v2")
s390_test_generate(vsf,   "vsf %%v5, %%v1, %%v2")
s390_test_generate(vsg,   "vsg %%v5, %%v1, %%v2")
s390_test_generate(vsq,   "vsq %%v5, %%v1, %%v2")

s390_test_generate(vn,   "vn  %%v5, %%v1, %%v2")
s390_test_generate(vnc,  "vnc %%v5, %%v1, %%v2")
s390_test_generate(vx,   "vx  %%v5, %%v1, %%v2")
s390_test_generate(vo,   "vo  %%v5, %%v1, %%v2")
s390_test_generate(vno,  "vno %%v5, %%v1, %%v2")

s390_test_generate(vavgb,   "vavgb  %%v5, %%v1, %%v2")
s390_test_generate(vavgh,   "vavgh  %%v5, %%v1, %%v2")
s390_test_generate(vavgf,   "vavgf  %%v5, %%v1, %%v2")
s390_test_generate(vavgg,   "vavgg  %%v5, %%v1, %%v2")
s390_test_generate(vavglb,  "vavglb %%v5, %%v1, %%v2")
s390_test_generate(vavglh,  "vavglh %%v5, %%v1, %%v2")
s390_test_generate(vavglf,  "vavglf %%v5, %%v1, %%v2")
s390_test_generate(vavglg,  "vavglg %%v5, %%v1, %%v2")

s390_test_generate(vmxb,   "vmxb  %%v5, %%v1, %%v2")
s390_test_generate(vmxh,   "vmxh  %%v5, %%v1, %%v2")
s390_test_generate(vmxf,   "vmxf  %%v5, %%v1, %%v2")
s390_test_generate(vmxg,   "vmxg  %%v5, %%v1, %%v2")
s390_test_generate(vmxlb,  "vmxlb %%v5, %%v1, %%v2")
s390_test_generate(vmxlh,  "vmxlh %%v5, %%v1, %%v2")
s390_test_generate(vmxlf,  "vmxlf %%v5, %%v1, %%v2")
s390_test_generate(vmxlg,  "vmxlg %%v5, %%v1, %%v2")

s390_test_generate(vmnb,   "vmnb  %%v5, %%v1, %%v2")
s390_test_generate(vmnh,   "vmnh  %%v5, %%v1, %%v2")
s390_test_generate(vmnf,   "vmnf  %%v5, %%v1, %%v2")
s390_test_generate(vmng,   "vmng  %%v5, %%v1, %%v2")
s390_test_generate(vmnlb,  "vmnlb %%v5, %%v1, %%v2")
s390_test_generate(vmnlh,  "vmnlh %%v5, %%v1, %%v2")
s390_test_generate(vmnlf,  "vmnlf %%v5, %%v1, %%v2")
s390_test_generate(vmnlg,  "vmnlg %%v5, %%v1, %%v2")

s390_test_generate(vlcb,  "vlcb %%v5, %%v1")
s390_test_generate(vlch,  "vlch %%v5, %%v1")
s390_test_generate(vlcf,  "vlcf %%v5, %%v1")
s390_test_generate(vlcg,  "vlcg %%v5, %%v1")

s390_test_generate(vlpb,  "vlpb %%v5, %%v1")
s390_test_generate(vlph,  "vlph %%v5, %%v1")
s390_test_generate(vlpf,  "vlpf %%v5, %%v1")
s390_test_generate(vlpg,  "vlpg %%v5, %%v1")

s390_test_generate(vchb,   "vchb  %%v5, %%v1, %%v2")
s390_test_generate(vchbs,  "vchbs  %%v5, %%v1, %%v2\n" \
                   S390_TEST_PUT_CC_TO_RESULT)
s390_test_generate(vchh,   "vchh  %%v5, %%v1, %%v2")
s390_test_generate(vchhs,  "vchhs  %%v5, %%v1, %%v2\n" \
                   S390_TEST_PUT_CC_TO_RESULT)
s390_test_generate(vchf,   "vchf  %%v5, %%v1, %%v2")
s390_test_generate(vchfs,  "vchfs  %%v5, %%v1, %%v2\n" \
                   S390_TEST_PUT_CC_TO_RESULT)
s390_test_generate(vchg,   "vchg  %%v5, %%v1, %%v2")
s390_test_generate(vchgs,  "vchgs  %%v5, %%v1, %%v2\n" \
                   S390_TEST_PUT_CC_TO_RESULT)
s390_test_generate(vchlb,  "vchlb %%v5, %%v1, %%v2")
s390_test_generate(vchlbs, "vchlbs %%v5, %%v1, %%v2\n" \
                   S390_TEST_PUT_CC_TO_RESULT)
s390_test_generate(vchlh,  "vchlh %%v5, %%v1, %%v2")
s390_test_generate(vchlhs, "vchlhs %%v5, %%v1, %%v2\n" \
                   S390_TEST_PUT_CC_TO_RESULT)
s390_test_generate(vchlf,  "vchlf %%v5, %%v1, %%v2")
s390_test_generate(vchlfs, "vchlfs %%v5, %%v1, %%v2\n" \
                   S390_TEST_PUT_CC_TO_RESULT)
s390_test_generate(vchlg,  "vchlg %%v5, %%v1, %%v2")
s390_test_generate(vchlgs, "vchlgs %%v5, %%v1, %%v2\n" \
                   S390_TEST_PUT_CC_TO_RESULT)

s390_test_generate(vclzb,  "vclzb %%v5, %%v1")
s390_test_generate(vclzh,  "vclzh %%v5, %%v1")
s390_test_generate(vclzf,  "vclzf %%v5, %%v1")
s390_test_generate(vclzg,  "vclzg %%v5, %%v1")

s390_test_generate(vctzb,  "vctzb %%v5, %%v1")
s390_test_generate(vctzh,  "vctzh %%v5, %%v1")
s390_test_generate(vctzf,  "vctzf %%v5, %%v1")
s390_test_generate(vctzg,  "vctzg %%v5, %%v1")

s390_test_generate(vmhb,   "vmhb  %%v5, %%v1, %%v2")
s390_test_generate(vmhh,   "vmhh  %%v5, %%v1, %%v2")
s390_test_generate(vmhf,   "vmhf  %%v5, %%v1, %%v2")
s390_test_generate(vmlhb,  "vmlhb %%v5, %%v1, %%v2")
s390_test_generate(vmlhh,  "vmlhh %%v5, %%v1, %%v2")
s390_test_generate(vmlhf,  "vmlhf %%v5, %%v1, %%v2")

s390_test_generate(vmlb,   "vmlb  %%v5, %%v1, %%v2")
s390_test_generate(vmlhw,  "vmlhw %%v5, %%v1, %%v2")
s390_test_generate(vmlf,   "vmlf  %%v5, %%v1, %%v2")

s390_test_generate(vmeb,   "vmeb   %%v5, %%v1, %%v2")
s390_test_generate(vmeh,   "vmeh   %%v5, %%v1, %%v2")
s390_test_generate(vmef,   "vmef   %%v5, %%v1, %%v2")
s390_test_generate(vmleb,  "vmleb  %%v5, %%v1, %%v2")
s390_test_generate(vmleh,  "vmleh  %%v5, %%v1, %%v2")
s390_test_generate(vmlef,  "vmlef  %%v5, %%v1, %%v2")

s390_test_generate(vpopct, "vpopct %%v5, %%v1, 0")

s390_test_generate(veslb, "veslb %%v5, %%v1, 4(%[r_arg1])")
s390_test_generate(veslh, "veslh %%v5, %%v1, 3(%[r_arg2])")
s390_test_generate(veslf, "veslf %%v5, %%v1, 2(%[r_arg3])")
s390_test_generate(veslg, "veslg %%v5, %%v1, 1(%[r_arg1])")
s390_test_generate(veslvb,"veslvb  %%v5, %%v1, %%v2")
s390_test_generate(veslvh,"veslvh  %%v5, %%v1, %%v2")
s390_test_generate(veslvf,"veslvf  %%v5, %%v1, %%v2")
s390_test_generate(veslvg,"veslvg  %%v5, %%v1, %%v2")

s390_test_generate(vesrab, "vesrab %%v5, %%v1, 4(%[r_arg1])")
s390_test_generate(vesrah, "vesrah %%v5, %%v1, 3(%[r_arg2])")
s390_test_generate(vesraf, "vesraf %%v5, %%v1, 2(%[r_arg3])")
s390_test_generate(vesrag, "vesrag %%v5, %%v1, 1(%[r_arg1])")
s390_test_generate(vesravb,"vesravb  %%v5, %%v1, %%v2")
s390_test_generate(vesravh,"vesravh  %%v5, %%v1, %%v2")
s390_test_generate(vesravf,"vesravf  %%v5, %%v1, %%v2")
s390_test_generate(vesravg,"vesravg  %%v5, %%v1, %%v2")

s390_test_generate(vesrlb, "vesrlb %%v5, %%v1, 4(%[r_arg1])")
s390_test_generate(vesrlh, "vesrlh %%v5, %%v1, 3(%[r_arg2])")
s390_test_generate(vesrlf, "vesrlf %%v5, %%v1, 2(%[r_arg3])")
s390_test_generate(vesrlg, "vesrlg %%v5, %%v1, 1(%[r_arg1])")
s390_test_generate(vesrlvb,"vesrlvb  %%v5, %%v1, %%v2")
s390_test_generate(vesrlvh,"vesrlvh  %%v5, %%v1, %%v2")
s390_test_generate(vesrlvf,"vesrlvf  %%v5, %%v1, %%v2")
s390_test_generate(vesrlvg,"vesrlvg  %%v5, %%v1, %%v2")

s390_test_generate(verllb, "verllb %%v5, %%v1, 4(%[r_arg1])")
s390_test_generate(verllh, "verllh %%v5, %%v1, 3(%[r_arg2])")
s390_test_generate(verllf, "verllf %%v5, %%v1, 2(%[r_arg3])")
s390_test_generate(verllg, "verllg %%v5, %%v1, 1(%[r_arg1])")
s390_test_generate(verllvb,"verllvb  %%v5, %%v1, %%v2")
s390_test_generate(verllvh,"verllvh  %%v5, %%v1, %%v2")
s390_test_generate(verllvf,"verllvf  %%v5, %%v1, %%v2")
s390_test_generate(verllvg,"verllvg  %%v5, %%v1, %%v2")

s390_test_generate(vsl,  "vlrepb %%v2, %[m_arg1]\n" \
                         "vsl    %%v5, %%v1, %%v2")
s390_test_generate(vsrl, "vlrepb %%v2, %[m_arg1]\n" \
                         "vsrl   %%v5, %%v1, %%v2")
s390_test_generate(vsra, "vlrepb %%v2, %[m_arg1]\n" \
                         "vsra   %%v5, %%v1, %%v2")

s390_test_generate(verimb, "verimb  %%v5, %%v1, %%v2, 5")
s390_test_generate(verimh, "verimh  %%v5, %%v1, %%v2, 13")
s390_test_generate(verimf, "verimf  %%v5, %%v1, %%v2, 27")
s390_test_generate(verimg, "verimg  %%v5, %%v1, %%v2, 45")

s390_test_generate(vecb, "vecb %%v1, %%v2\n" \
                         S390_TEST_PUT_CC_TO_RESULT)
s390_test_generate(vech, "vech %%v1, %%v2\n" \
                         S390_TEST_PUT_CC_TO_RESULT)
s390_test_generate(vecf, "vecf %%v1, %%v2\n" \
                         S390_TEST_PUT_CC_TO_RESULT)
s390_test_generate(vecg, "vecg %%v1, %%v2\n" \
                         S390_TEST_PUT_CC_TO_RESULT)

s390_test_generate(veclb, "veclb  %%v1, %%v2\n" \
                          S390_TEST_PUT_CC_TO_RESULT)
s390_test_generate(veclh, "veclh  %%v1, %%v2\n" \
                          S390_TEST_PUT_CC_TO_RESULT)
s390_test_generate(veclf, "veclf  %%v1, %%v2\n" \
                          S390_TEST_PUT_CC_TO_RESULT)
s390_test_generate(veclg, "veclg  %%v1, %%v2\n" \
                          S390_TEST_PUT_CC_TO_RESULT)

s390_test_generate(vceqb,  "vceqb   %%v5, %%v1, %%v2")
s390_test_generate(vceqh,  "vceqh   %%v5, %%v1, %%v2")
s390_test_generate(vceqf,  "vceqf   %%v5, %%v1, %%v2")
s390_test_generate(vceqg,  "vceqg   %%v5, %%v1, %%v2")
s390_test_generate(vceqbs, "vceqbs  %%v5, %%v1, %%v2\n" \
                           S390_TEST_PUT_CC_TO_RESULT)
s390_test_generate(vceqhs, "vceqhs  %%v5, %%v1, %%v2\n" \
                           S390_TEST_PUT_CC_TO_RESULT)
s390_test_generate(vceqfs, "vceqfs  %%v5, %%v1, %%v2\n" \
                           S390_TEST_PUT_CC_TO_RESULT)
s390_test_generate(vceqgs, "vceqgs  %%v5, %%v1, %%v2\n" \
                           S390_TEST_PUT_CC_TO_RESULT)

s390_test_generate(vslb,  "vslb  %%v5, %%v1, %%v2")
s390_test_generate(vsrlb, "vsrlb  %%v5, %%v1, %%v2")
s390_test_generate(vsrab, "vsrab  %%v5, %%v1, %%v2")

s390_test_generate(vsldb0,  "vsldb  %%v5, %%v1, %%v2, 0")
s390_test_generate(vsldb4,  "vsldb  %%v5, %%v1, %%v2, 4")
s390_test_generate(vsldb16, "vsldb  %%v5, %%v1, %%v2, 16")
s390_test_generate(vsldb24, "vsldb  %%v5, %%v1, %%v2, 24")

s390_test_generate(vmob,   "vmob   %%v5, %%v1, %%v2")
s390_test_generate(vmoh,   "vmoh   %%v5, %%v1, %%v2")
s390_test_generate(vmof,   "vmof   %%v5, %%v1, %%v2")
s390_test_generate(vmlob,  "vmlob  %%v5, %%v1, %%v2")
s390_test_generate(vmloh,  "vmloh  %%v5, %%v1, %%v2")
s390_test_generate(vmlof,  "vmlof  %%v5, %%v1, %%v2")

s390_test_generate(vmaeb,  "vmaeb %%v5, %%v1, %%v2, %%v3")
s390_test_generate(vmaeh,  "vmaeh %%v5, %%v1, %%v2, %%v3")
s390_test_generate(vmaef,  "vmaef %%v5, %%v1, %%v2, %%v3")
s390_test_generate(vmaleb, "vmaleb %%v5, %%v1, %%v2, %%v3")
s390_test_generate(vmaleh, "vmaleh %%v5, %%v1, %%v2, %%v3")
s390_test_generate(vmalef, "vmalef %%v5, %%v1, %%v2, %%v3")

s390_test_generate(vmaob,  "vmaob %%v5, %%v1, %%v2, %%v3")
s390_test_generate(vmaoh,  "vmaoh %%v5, %%v1, %%v2, %%v3")
s390_test_generate(vmaof,  "vmaof %%v5, %%v1, %%v2, %%v3")
s390_test_generate(vmalob, "vmalob %%v5, %%v1, %%v2, %%v3")
s390_test_generate(vmaloh, "vmaloh %%v5, %%v1, %%v2, %%v3")
s390_test_generate(vmalof, "vmalof %%v5, %%v1, %%v2, %%v3")

s390_test_generate(vmalb,  "vmalb %%v5, %%v1, %%v2, %%v3")
s390_test_generate(vmalhw, "vmalhw %%v5, %%v1, %%v2, %%v3")
s390_test_generate(vmalf,  "vmalf %%v5, %%v1, %%v2, %%v3")

s390_test_generate(vmahb,  "vmahb %%v5, %%v1, %%v2, %%v3")
s390_test_generate(vmahh,  "vmahh %%v5, %%v1, %%v2, %%v3")
s390_test_generate(vmahf,  "vmahf %%v5, %%v1, %%v2, %%v3")
s390_test_generate(vmalhb, "vmalhb %%v5, %%v1, %%v2, %%v3")
s390_test_generate(vmalhh, "vmalhh %%v5, %%v1, %%v2, %%v3")
s390_test_generate(vmalhf, "vmalhf %%v5, %%v1, %%v2, %%v3")

s390_test_generate(vsumb,  "vsumb %%v5, %%v1, %%v2")
s390_test_generate(vsumh,  "vsumh %%v5, %%v1, %%v2")

s390_test_generate(vsumgh, "vsumgh %%v5, %%v1, %%v2")
s390_test_generate(vsumgf, "vsumgf %%v5, %%v1, %%v2")

s390_test_generate(vsumqf, "vsumqf %%v5, %%v1, %%v2")
s390_test_generate(vsumqg, "vsumqg %%v5, %%v1, %%v2")

s390_test_generate(vtm0, "vone %%v3\n" \
                         "vx   %%v2, %%v1, %%v3\n" \
                         "vtm  %%v1, %%v2\n" \
                         S390_TEST_PUT_CC_TO_RESULT)
s390_test_generate(vtm0_zeromask, "vzero %%v2\n" \
                                  "vtm  %%v1, %%v2\n" \
                                  S390_TEST_PUT_CC_TO_RESULT)
s390_test_generate(vtm1, "vone %%v2\n" \
                         "vtm  %%v1, %%v2\n" \
                         S390_TEST_PUT_CC_TO_RESULT)
s390_test_generate(vtm3_equalargs, "vlr %%v2, %%v1\n" \
                                   "vtm  %%v1, %%v2\n" \
                                   S390_TEST_PUT_CC_TO_RESULT)
s390_test_generate(vtm3, "vlr  %%v2, %%v1\n" \
                         "vgbm %%v3, 0xff00\n" \
                         "vn   %%v2, %%v2, %%v3\n" \
                         "vtm  %%v1, %%v2\n" \
                         S390_TEST_PUT_CC_TO_RESULT)

s390_test_generate(vacq,  "vacq %%v5, %%v1, %%v2, %%v3")
s390_test_generate(vaccb, "vaccb %%v5, %%v1, %%v2")
s390_test_generate(vacch, "vacch %%v5, %%v1, %%v2")
s390_test_generate(vaccf, "vaccf %%v5, %%v1, %%v2")
s390_test_generate(vaccg, "vaccg %%v5, %%v1, %%v2")
s390_test_generate(vaccq, "vaccq %%v5, %%v1, %%v2")
s390_test_generate(vacccq,"vacccq %%v5, %%v1, %%v2, %%v3")

s390_test_generate(vcksm,  "vcksm %%v5, %%v1, %%v2")

s390_test_generate(vgfmb, "vgfmb %%v5, %%v1, %%v2")
s390_test_generate(vgfmh, "vgfmh %%v5, %%v1, %%v2")
s390_test_generate(vgfmf, "vgfmf %%v5, %%v1, %%v2")
s390_test_generate(vgfmg, "vgfmg %%v5, %%v1, %%v2")

s390_test_generate(vgfmab, "vgfmab %%v5, %%v1, %%v2, %%v3")
s390_test_generate(vgfmah, "vgfmah %%v5, %%v1, %%v2, %%v3")
s390_test_generate(vgfmaf, "vgfmaf %%v5, %%v1, %%v2, %%v3")
s390_test_generate(vgfmag, "vgfmag %%v5, %%v1, %%v2, %%v3")

s390_test_generate(vsbiq,  "vsbiq %%v5, %%v1, %%v2, %%v3")
s390_test_generate(vscbib, "vscbib %%v5, %%v1, %%v2")
s390_test_generate(vscbih, "vscbih %%v5, %%v1, %%v2")
s390_test_generate(vscbif, "vscbif %%v5, %%v1, %%v2")
s390_test_generate(vscbig, "vscbig %%v5, %%v1, %%v2")
s390_test_generate(vscbiq, "vscbiq %%v5, %%v1, %%v2")
s390_test_generate(vsbcbiq,"vsbcbiq %%v5, %%v1, %%v2, %%v3")

int main() {
   size_t iteration = 0;

   test(vab);
   test(vah);
   test(vaf);
   test(vag);
   test(vaq);

   test(vsb);
   test(vsh);
   test(vsf);
   test(vsg);
   test(vsq);

   test(vn);
   test(vnc);
   test(vx);
   test(vno);
   test(vo);

   test(vavgb);
   test(vavgh);
   test(vavgf);
   test(vavgg);
   test(vavglb);
   test(vavglh);
   test(vavglf);
   test(vavglg);

   test(vmxb);
   test(vmxh);
   test(vmxf);
   test(vmxg);
   test(vmxlb);
   test(vmxlh);
   test(vmxlf);
   test(vmxlg);

   test(vmnb);
   test(vmnh);
   test(vmnf);
   test(vmng);
   test(vmnlb);
   test(vmnlh);
   test(vmnlf);
   test(vmnlg);

   test(vlcb);
   test(vlch);
   test(vlcf);
   test(vlcg);

   test(vlpb);
   test(vlph);
   test(vlpf);
   test(vlpg);

   test(vchb);
   test(vchbs);
   test(vchh);
   test(vchhs);
   test(vchf);
   test(vchfs);
   test(vchg);
   test(vchgs);
   test(vchlb);
   test(vchlbs);
   test(vchlh);
   test(vchlhs);
   test(vchlf);
   test(vchlfs);
   test(vchlg);
   test(vchlgs);

   test(vclzb);
   test(vclzh);
   test(vclzf);
   test(vclzg);

   test(vctzb);
   test(vctzh);
   test(vctzf);
   test(vctzg);

   test(vmhb);
   test(vmhh);
   test(vmhf);
   test(vmlhb);
   test(vmlhh);
   test(vmlhf);

   test(vmlb);
   test(vmlhw);
   test(vmlf);

   test(vmeb);
   test(vmeh);
   test(vmef);
   test(vmleb);
   test(vmleh);
   test(vmlef);

   test(vmob);
   test(vmoh);
   test(vmof);
   test(vmlob);
   test(vmloh);
   test(vmlof);

   test(veslb);
   test(veslh);
   test(veslf);
   test(veslg);
   test(veslvb);
   test(veslvh);
   test(veslvf);
   test(veslvg);

   test(vesrab);
   test(vesrah);
   test(vesraf);
   test(vesrag);
   test(vesravb);
   test(vesravh);
   test(vesravf);
   test(vesravg);

   test(vesrlb);
   test(vesrlh);
   test(vesrlf);
   test(vesrlg);
   test(vesrlvb);
   test(vesrlvh);
   test(vesrlvf);
   test(vesrlvg);

   test(verllb);
   test(verllh);
   test(verllf);
   test(verllg);
   test(verllvb);
   test(verllvh);
   test(verllvf);
   test(verllvg);

   test(vsl);
   test(vsrl);
   test(vsra);

   test(verimb);
   test(verimh);
   test(verimf);
   test(verimg);

   test(vecb);
   test(vech);
   test(vecf);
   test(vecg);
   test(veclb);
   test(veclh);
   test(veclf);
   test(veclg);

   test(vceqb);
   test(vceqh);
   test(vceqf);
   test(vceqg);
   test(vceqbs);
   test(vceqhs);
   test(vceqfs);
   test(vceqgs);

   test(vslb);
   test(vsrlb);
   test(vsrab);

   test_once(vsldb0);
   test_once(vsldb4);
   test_once(vsldb16);
   test_once(vsldb24);

   test(vmaeb);
   test(vmaeh);
   test(vmaef);
   test(vmaleb);
   test(vmaleh);
   test(vmalef);

   test(vmaob);
   test(vmaoh);
   test(vmaof);
   test(vmalob);
   test(vmaloh);
   test(vmalof);

   test(vmalb);
   test(vmalhw);
   test(vmalf);

   test(vmahb);
   test(vmahh);
   test(vmahf);
   test(vmalhb);
   test(vmalhh);
   test(vmalhf);

   test(vsumb);
   test(vsumh);
   test(vsumgh);
   test(vsumgf);
   test(vsumqf);
   test(vsumqg);

   test_once(vtm0);
   test_once(vtm0_zeromask);
   test_once(vtm1);
   test_once(vtm3_equalargs);
   test_once(vtm3);

   test(vacq);
   test(vaccb);
   test(vacch);
   test(vaccf);
   test(vaccg);
   test(vaccq);
   test(vacccq);

   test(vcksm);

   test(vgfmb);
   test(vgfmh);
   test(vgfmf);
   test(vgfmg);

   test(vgfmab);
   test(vgfmah);
   test(vgfmaf);
   test(vgfmag);

   test(vsbiq);
   test(vscbib);
   test(vscbih);
   test(vscbif);
   test(vscbig);
   test(vscbiq);
   test(vsbcbiq);

   test(vpopct);

   return 0;
}
