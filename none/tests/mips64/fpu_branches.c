#include <stdio.h>
#include "macro_fpu.h"

int main()
{
#if defined(__mips_hard_float) && (__mips_isa_rev < 6)
   int i = 0;

   printf("--- BC1F ---  if fs == ft then " \
          "out = ft else out = fs + ft\n");
   for (i = 0; i < MAX_ARR; i++) {
      TESTINST1s("bc1f", i);
      TESTINST1d("bc1f", i);
   }

   printf("--- BC1T ---  if fs != ft then " \
          "out = fs + ft else out = ft\n");
   for (i = 0; i < MAX_ARR; i++) {
      TESTINST1s("bc1t", i);
      TESTINST1d("bc1t", i);
   }

   printf("--- BC1FL ---  if fs == ft then " \
          "out = ft else out = fs + ft\n");
   for (i = 0; i < MAX_ARR; i++) {
      TESTINST2s("bc1fl", i);
      TESTINST2d("bc1fl", i);
   }

   printf("--- BC1TL ---  if fs != ft then " \
          "out = ft else out = fs + ft\n");
   for (i = 0; i < MAX_ARR; i++) {
      TESTINST2s("bc1tl", i);
      TESTINST2d("bc1tl", i);
   }

   printf("--- C.F.S/D ---  if false then " \
          "out = fs + ft else out = fs\n");
   for (i = 0; i < MAX_ARR; i++) {
      TESTINST_CONDs("c.f.s", i);
      TESTINST_CONDd("c.f.d", i);
   }

   printf("--- C.UN.S/D ---  if unordered(fs, ft) then " \
          "out = fs + ft else out = fs\n");
   for (i = 0; i < MAX_ARR; i++) {
      TESTINST_CONDs("c.un.s", i);
      TESTINST_CONDd("c.un.d", i);
   }

   printf("--- C.EQ.S/D ---  if fs == ft then " \
          "out = fs + ft else out = fs\n");
   for (i = 0; i < MAX_ARR; i++) {
      TESTINST_CONDs("c.eq.s", i);
      TESTINST_CONDd("c.eq.d", i);
   }

   printf("--- C.UEQ.S/D ---  if (unordered(fs, ft) or (fs == ft)) then " \
          "out = fs + ft else out = fs\n");
   for (i = 0; i < MAX_ARR; i++) {
      TESTINST_CONDs("c.ueq.s", i);
      TESTINST_CONDd("c.ueq.d", i);
   }

   printf("--- C.OLT.S/D ---  if (ordered(fs, ft) or (fs < ft)) then " \
          "out = fs + ft else out = fs\n");
   for (i = 0; i < MAX_ARR; i++) {
      TESTINST_CONDs("c.olt.s", i);
      TESTINST_CONDd("c.olt.d", i);
   }

   printf("--- C.ULT.S/D ---  if (unordered(fs, ft) or (fs < ft)) then " \
          "out = fs + ft else out = fs\n");
   for (i = 0; i < MAX_ARR; i++) {
      TESTINST_CONDs("c.ult.s", i);
      TESTINST_CONDd("c.ult.d", i);
   }

   printf("--- C.OLE.S/D ---  if (ordered(fs, ft) or (fs <= ft)) then " \
          "out = fs + ft else out = fs\n");
   for (i = 0; i < MAX_ARR; i++) {
      TESTINST_CONDs("c.ole.s", i);
      TESTINST_CONDd("c.ole.d", i);
   }

   printf("--- C.ULE.S/D ---  if (unordered(fs, ft) or (fs <= ft)) then " \
          "out = fs + ft else out = fs\n");
   for (i = 0; i < MAX_ARR; i++) {
      TESTINST_CONDs("c.ule.s", i);
      TESTINST_CONDd("c.ule.d", i);
   }

   printf("--- C.SF.S/D ---  if signaling false then " \
          "out = fs + ft else out = fs\n");
   for (i = 0; i < MAX_ARR; i++) {
      TESTINST_CONDs("c.sf.s", i);
      TESTINST_CONDd("c.sf.d", i);
   }

   printf("--- C.NGLE.S/D --- if not ((fs > ft) or (fs <= ft)) then " \
          "out = fs + ft else out = fs\n");
   for (i = 0; i < MAX_ARR; i++) {
      TESTINST_CONDs("c.ngle.s", i);
      TESTINST_CONDd("c.ngle.d", i);
   }

   printf("--- C.SEQ.S/D ---  if signaling equal then " \
          "out = fs + ft else out = fs\n");
   for (i = 0; i < MAX_ARR; i++) {
      TESTINST_CONDs("c.seq.s", i);
      TESTINST_CONDd("c.seq.d", i);
   }

   printf("--- C.NGL.S/D ---  if not ((fs > ft) or (fs < ft)) then " \
          "out = fs + ft else out = fs\n");
   for (i = 0; i < MAX_ARR; i++) {
      TESTINST_CONDs("c.ngl.s", i);
      TESTINST_CONDd("c.ngl.d", i);
   }

   printf("--- C.LT.S/D ---  if fs < ft then " \
          "out = fs + ft else out = fs\n");
   for (i = 0; i < MAX_ARR; i++) {
      TESTINST_CONDs("c.lt.s", i);
      TESTINST_CONDd("c.lt.d", i);
   }

   printf("--- C.NGE.S/D ---  if not (fs >= ft) then " \
          "out = fs + ft else out = fs\n");
   for (i = 0; i < MAX_ARR; i++) {
      TESTINST_CONDs("c.nge.s", i);
      TESTINST_CONDd("c.nge.d", i);
   }

   printf("--- C.LE.S/D ---  if fs <= ft then " \
          "out = fs + ft else out = fs\n");
   for (i = 0; i < MAX_ARR; i++) {
      TESTINST_CONDs("c.le.s", i);
      TESTINST_CONDd("c.le.d", i);
   }

   printf("--- C.NGT.S/D ---  if not (fs > ft) then " \
          "out = fs + ft else out = fs\n");
   for (i = 0; i < MAX_ARR; i++) {
      TESTINST_CONDs("c.ngt.s", i);
      TESTINST_CONDd("c.ngt.d", i);
   }
#endif
   return 0;
}

