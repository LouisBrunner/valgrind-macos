#include <stdio.h>

#define MAX_ARR 24

const float fs_f[] = {
   0,         456.25,   3,          -1,
   1384.5,    -7.25,    1000000000, -5786.5,
   1752,      0.015625, 0.03125,    -248562.75,
   -45786.5,  456,      34.03125,   45786.75,
   1752065,   107,      -45667.25,  -7,
   -347856.5, 356047.5, -1.0,       23.0625
};

const float ft_f[] = {
   -4578.5, 456.25,   34.03125, 4578.75,
   175,     107,      -456.25,  -7.25,
   -3478.5, 356.5,    -1.0,     23.0625,
   0,       456.25,   3,        -1,
   1384.5,  -7,       100,      -5786.5,
   1752,    0.015625, 0.03125,  -248562.75
};

const double fs_d[] = {
   0,         456.25,   3,          -1,
   1384.5,    -7.25,    1000000000, -5786.5,
   1752,      0.015625, 0.03125,    -248562.75,
   -45786.5,  456,      34.03125,   45786.75,
   1752065,   107,      -45667.25,  -7,
   -347856.5, 356047.5, -1.0,       23.0625
};

const double ft_d[] = {
   -45786.5,  456.25,   34.03125,   45786.75,
   1752065,   107,      -45667.25,  -7.25,
   -347856.5, 356047.5, -1.0,       23.0625,
   0,         456.25,   3,          -1,
   1384.5,    -7,       1000000000, -5786.5,
   1752,      0.015625, 0.03125,    -248562.75
};

/* Conditional macros.*/
#define TESTINST1s(instruction, RDval)               \
{                                                    \
   float outf = 0;                                   \
   __asm__ __volatile__(                             \
      ".set        noreorder"                "\n\t"  \
      "mov.s       $f0, %1"                  "\n\t"  \
      "mov.s       $f2, %2"                  "\n\t"  \
      "c.eq.s      $f0, $f2"                 "\n\t"  \
      instruction" end"instruction"s"#RDval  "\n\t"  \
      "nop"                                  "\n\t"  \
      "add.s       $f0, $f0, $f2"            "\n\t"  \
      "end"instruction"s"#RDval":"           "\n\t"  \
      "mov.s       %0,  $f0"                 "\n\t"  \
      ".set        reorder"                  "\n\t"  \
      : "=f" (outf)                                  \
      : "f" (fs_f[i]) , "f" (ft_f[i])                \
      : "$f0", "$f2"                                 \
   );                                                \
   printf("%s, c.eq.s   out=%f, fs=%f, ft=%f\n",     \
          instruction, outf, fs_f[i], ft_f[i]);      \
}

#define TESTINST1d(instruction, RDval)               \
{                                                    \
   double outd = 0;                                  \
   __asm__ __volatile__(                             \
      ".set        noreorder"                "\n\t"  \
      "mov.d       $f0, %1"                  "\n\t"  \
      "mov.d       $f2, %2"                  "\n\t"  \
      "c.eq.d      $f0, $f2"                 "\n\t"  \
      instruction" end"instruction"d"#RDval  "\n\t"  \
      "nop"                                  "\n\t"  \
      "add.d       $f0, $f0, $f2"            "\n\t"  \
      "end"instruction"d"#RDval":"           "\n\t"  \
      "mov.d       %0,  $f0"                 "\n\t"  \
      ".set        reorder"                  "\n\t"  \
      : "=f" (outd)                                  \
      : "f" (fs_d[i]) , "f" (ft_d[i])                \
      : "$f0", "$f1", "$f2", "$f3"                   \
   );                                                \
   printf("%s, c.eq.d   out=%f, fs=%f, ft=%f\n",     \
          instruction, outd, fs_d[i], ft_d[i]);      \
}

#define TESTINST2s(instruction, RDval)               \
{                                                    \
   float outf = 0;                                   \
   __asm__ __volatile__(                             \
      ".set        noreorder"                "\n\t"  \
      "mov.s       $f0, %1"                  "\n\t"  \
      "mov.s       $f2, %2"                  "\n\t"  \
      "c.eq.s      $f0, $f2"                 "\n\t"  \
      instruction" end"instruction"s"#RDval  "\n\t"  \
      "add.s       $f0, $f0, $f2"            "\n\t"  \
      "end"instruction"s"#RDval":"           "\n\t"  \
      "mov.s       %0, $f0"                  "\n\t"  \
      ".set        reorder"                  "\n\t"  \
      : "=f" (outf)                                  \
      : "f" (fs_f[i]) , "f" (ft_f[i])                \
      : "$f0", "$f2"                                 \
   );                                                \
   printf("%s, c.eq.s   out=%f, fs=%f, ft=%f\n",     \
          instruction, outf, fs_f[i], ft_f[i]);      \
}

#define TESTINST2d(instruction, RDval)               \
{                                                    \
   double outd = 0;                                  \
   __asm__ __volatile__(                             \
      ".set        noreorder"                "\n\t"  \
      "mov.d       $f0, %1"                  "\n\t"  \
      "mov.d       $f2, %2"                  "\n\t"  \
      "c.eq.d      $f0, $f2"                 "\n\t"  \
      instruction" end"instruction"d"#RDval  "\n\t"  \
      "add.d       $f0, $f0, $f2"            "\n\t"  \
      "end"instruction"d"#RDval":"           "\n\t"  \
      "mov.d       %0,  $f0"                 "\n\t"  \
      ".set        reorder"                  "\n\t"  \
      : "=f" (outd)                                  \
      : "f" (fs_d[i]) , "f" (ft_d[i])                \
      : "$f0", "$f1", "$f2", "$f3"                   \
   );                                                \
   printf("%s, c.eq.d   out=%f, fs=%f, ft=%f\n",     \
          instruction, outd, fs_d[i], ft_d[i]);      \
}

#define TESTINST_CONDs(instruction, RDval)       \
{                                                \
   float outf = 0;                               \
   __asm__ __volatile__(                         \
      ".set        noreorder"         "\n\t"     \
      "mov.s       $f0, %1"           "\n\t"     \
      "mov.s       $f2, %2"           "\n\t"     \
      instruction" $f0, $f2"          "\n\t"     \
      "bc1f end"instruction"s"#RDval  "\n\t"     \
      "nop"                           "\n\t"     \
      "add.s       $f0, $f0, $f2"     "\n\t"     \
      "end"instruction"s"#RDval":"    "\n\t"     \
      "mov.s       %0,  $f0"          "\n\t"     \
      ".set        reorder"           "\n\t"     \
      : "=f" (outf)                              \
      : "f" (fs_f[i]) , "f" (ft_f[i])            \
      : "$f0", "$f1", "$f2"                      \
   );                                            \
   printf("%s, bc1f   out=%f, fs=%f, ft=%f\n",   \
          instruction, outf, fs_f[i], ft_f[i]);  \
}

#define TESTINST_CONDd(instruction, RDval)       \
{                                                \
   double outd = 0;                              \
   __asm__ __volatile__(                         \
      ".set        noreorder"         "\n\t"     \
      "mov.d       $f0, %1"           "\n\t"     \
      "mov.d       $f2, %2"           "\n\t"     \
      instruction" $f0, $f2"          "\n\t"     \
      "bc1f end"instruction"d"#RDval  "\n\t"     \
      "nop"                           "\n\t"     \
      "add.d       $f0, $f0, $f2"     "\n\t"     \
      "end"instruction"d"#RDval":"    "\n\t"     \
      "mov.d       %0,  $f0"          "\n\t"     \
      ".set        reorder"           "\n\t"     \
      : "=f" (outd)                              \
      : "f" (fs_d[i]) , "f" (ft_d[i])            \
      : "$f0", "$f1", "$f2", "$f2"               \
   );                                            \
   printf("%s, bc1f   out=%f, fs=%f, ft=%f\n",   \
          instruction, outd, fs_d[i], ft_d[i]);  \
}

int main()
{
   int i = 0;

   printf("--- BC1F ---  if fs != ft then " \
          "out = fs else out = fs + ft\n");
   for (i = 0; i < MAX_ARR; i++) {
      TESTINST1s("bc1f", i);
      TESTINST1d("bc1f", i);
   }

   printf("--- BC1T ---  if fs == ft then " \
          "out = fs else out = fs + ft\n");
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
          "out = fs else out = fs + ft\n");
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
   return 0;
}

