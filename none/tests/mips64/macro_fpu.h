#include "const.h"

typedef enum {
   ABSS=0, ABSD,   ADDS,   ADDD,
   DIVS,   DIVD,   MULS,   MULD,
   NEGS,   NEGD,   SQRTS,  SQRTD,
   SUBS,   SUBD,   RECIPS, RECIPD,
   RSQRTS, RSQRTD, MSUBS,  MSUBD,
   MADDS,  MADDD,  NMADDS, NMADDD,
   NMSUBS, NMSUBD
} flt_art_op_t;

typedef enum {
   CEILWS=0, CEILWD,  FLOORWS,  FLOORWD,
   ROUNDWS,  ROUNDWD, TRUNCWS,  TRUNCWD,
   CEILLS,   CEILLD,  FLOORLS,  FLOORLD,
   ROUNDLS,  ROUNDLD, TRUNCLS,  TRUNCLD
} flt_dir_op_t;

typedef enum {
   CVTDS, CVTDW, CVTSD, CVTSW,
   CVTWS, CVTWD, CVTDL, CVTLS,
   CVTLD, CVTSL,
} flt_round_op_t;

const char *flt_art_op_names[] = {
   "abs.s",   "abs.d",   "add.s",   "add.d",
   "div.s",   "div.d",   "mul.s",   "mul.d",
   "neg.s",   "neg.d",   "sqrt.s",  "sqrt.d",
   "sub.s",   "sub.d",   "recip.s", "recip.d",
   "rsqrt.s", "rsqrt.d", "msub.s",  "msub.d",
   "madd.s",  "madd.d",  "nmadd.s", "nmadd.d",
   "nmsub.s", "nmsub.d"
};

const char *flt_dir_op_names[] = {
   "ceil.w.s",  "ceil.w.d",
   "floor.w.s", "floor.w.d",
   "round.w.s", "round.w.d",
   "trunc.w.s", "trunc.w.d",
   "ceil.l.s",  "ceil.l.d",
   "floor.l.s", "floor.l.d",
   "round.l.s", "round.l.d",
   "trunc.l.s", "trunc.l.d"
};

const char *flt_round_op_names[] = {
   "cvt.d.s", "cvt.d.w",
   "cvt.s.d", "cvt.s.w",
   "cvt.w.s", "cvt.w.d",
   "cvt.d.l", "cvt.l.s",
   "cvt.l.d", "cvt.s.l",
};

#define UNOPdd(op)               \
   fd_d = 0;                     \
   __asm__ __volatile__(         \
      op"   %1, %2"   "\n\t"     \
      "cfc1 %0, $31"  "\n\t"     \
      : "=r" (fcsr), "=f"(fd_d)  \
      : "f"(fs_d[i])             \
   );

#define UNOPff(op)               \
   fd_f = 0;                     \
   __asm__ __volatile__(         \
      op"   %1, %2"   "\n\t"     \
      "cfc1 %0, $31"  "\n\t"     \
      : "=r" (fcsr), "=f"(fd_f)  \
      : "f"(fs_f[i])             \
   );

#define UNOPfd(op)               \
   fd_d = 0;                     \
   __asm__ __volatile__(         \
      op"   %1, %2"   "\n\t"     \
      "cfc1 %0, $31"  "\n\t"     \
      : "=r" (fcsr), "=f"(fd_d)  \
      : "f"(fs_f[i])             \
   );

#define UNOPdf(op)               \
   fd_f = 0;                     \
   __asm__ __volatile__(         \
      op"   %1, %2"   "\n\t"     \
      "cfc1 %0, $31"  "\n\t"     \
      : "=r" (fcsr), "=f"(fd_f)  \
      : "f"(fs_d[i])             \
   );

#define UNOPfw(op)               \
   fd_w = 0;                     \
   __asm__ __volatile__(         \
      op"   $f0, %2"   "\n\t"    \
      "mfc1 %1,  $f0"  "\n\t"    \
      "cfc1 %0,  $31"  "\n\t"    \
      : "=r" (fcsr), "=r"(fd_w)  \
      : "f"(fs_f[i])             \
      : "$f0"                    \
   );

#define UNOPdw(op)               \
   fd_w = 0;                     \
   __asm__ __volatile__(         \
      op"   $f0, %2"   "\n\t"    \
      "mfc1 %1,  $f0"  "\n\t"    \
      "cfc1 %0,  $31"  "\n\t"    \
      : "=r" (fcsr), "=r"(fd_w)  \
      : "f"(fs_d[i])             \
      : "$f0"                    \
   );

#define UNOPwd(op)               \
   fd_d = 0;                     \
   __asm__ __volatile__(         \
      "mtc1 %2,  $f0"  "\n\t"    \
      op"   %1,  $f0"  "\n\t"    \
      "cfc1 %0,  $31"  "\n\t"    \
      : "=r" (fcsr), "=f"(fd_d)  \
      : "r"(fs_w[i])             \
      : "$f0"                    \
   );

#define UNOPwf(op)               \
   fd_f = 0;                     \
   __asm__ __volatile__(         \
      "mtc1 %2,  $f0"  "\n\t"    \
      op"   %1,  $f0"  "\n\t"    \
      "cfc1 %0,  $31"  "\n\t"    \
      : "=r" (fcsr), "=f"(fd_f)  \
      : "r"(fs_w[i])             \
      : "$f0"                    \
   );

#define UNOPld(op)               \
   fd_d = 0;                     \
   __asm__ __volatile__(         \
      "dmtc1 %2, $f0"  "\n\t"    \
      op"    %1, $f0"  "\n\t"    \
      "cfc1  %0, $31"  "\n\t"    \
      : "=r" (fcsr), "=f"(fd_d)  \
      : "r"(fs_l[i])             \
      : "$f0"                    \
   );

#define UNOPdl(op)               \
   fd_l = 0;                     \
   __asm__ __volatile__(         \
      op"    $f0, %2"   "\n\t"   \
      "dmfc1 %1,  $f0"  "\n\t"   \
      "cfc1  %0,  $31"  "\n\t"   \
      : "=r" (fcsr), "=r"(fd_l)  \
      : "f"(fs_d[i])             \
      : "$f0"                    \
   );

#define UNOPls(op)               \
   fd_f = 0;                     \
   __asm__ __volatile__(         \
      "dmtc1 %2, $f0"  "\n\t"    \
      op"    %1, $f0"  "\n\t"    \
      "cfc1  %0, $31"  "\n\t"    \
      : "=r" (fcsr), "=f"(fd_f)  \
      : "r"(fs_l[i])             \
      : "$f0"                    \
   );

#define UNOPsl(op)               \
   fd_l = 0;                     \
   __asm__ __volatile__(         \
      op"    $f0, %2"   "\n\t"   \
      "dmfc1 %1,  $f0"  "\n\t"   \
      "cfc1  %0,  $31"  "\n\t"   \
      : "=r" (fcsr), "=r"(fd_l)  \
      : "f"(fs_f[i])             \
      : "$f0"                    \
   );

#define BINOPf(op)                    \
   fd_f = 0;                          \
   __asm__ __volatile__(              \
      op"    %1, %2, %3"  "\n\t"      \
      "cfc1  %0, $31"     "\n\t"      \
      : "=r" (fcsr), "=f" (fd_f)      \
      : "f" (fs_f[i]), "f" (ft_f[i])  \
   );

#define BINOPd(op)                    \
   fd_d = 0;                          \
   __asm__ __volatile__(              \
      op" %1, %2, %3"  "\n\t"         \
      "cfc1  %0, $31"     "\n\t"      \
      : "=r" (fcsr), "=f"(fd_d)       \
      : "f" (fs_d[i]), "f" (ft_d[i])  \
   );

#define TRIOPf(op)                                    \
   fd_f = 0;                                          \
   __asm__ __volatile__(                              \
      op"    %1, %2, %3, %4"  "\n\t"                  \
      "cfc1  %0, $31"         "\n\t"                  \
      : "=r" (fcsr), "=f" (fd_f)                      \
      : "f" (fr_f[i]), "f" (fs_f[i]) , "f" (ft_f[i])  \
   );

#define TRIOPd(op)                                    \
   fd_d = 0;                                          \
   __asm__ __volatile__(                              \
      op"    %1, %2, %3, %4"  "\n\t"                  \
      "cfc1  %0, $31"         "\n\t"                  \
      : "=r" (fcsr), "=f"(fd_d)                       \
      : "f" (fr_d[i]), "f" (fs_d[i]) , "f" (ft_d[i])  \
   );

/* Conditional macros.*/
#define TESTINST1s(instruction, RDval)               \
{                                                    \
   float outf = 0;                                   \
   __asm__ __volatile__(                             \
      ".set        noreorder"                "\n\t"  \
      "mov.s       $f1,   %1"                "\n\t"  \
      "mov.s       $f2,   %2"                "\n\t"  \
      "mtc1        $zero, $f0"               "\n\t"  \
      "c.eq.s      $f1,   $f2"               "\n\t"  \
      instruction" end"instruction"s"#RDval  "\n\t"  \
      "nop"                                  "\n\t"  \
      "add.s       $f0,   $f0, $f1"          "\n\t"  \
      "end"instruction"s"#RDval":"           "\n\t"  \
      "add.s       $f0,   $f0, $f2"          "\n\t"  \
      "mov.s       %0,    $f0"               "\n\t"  \
      ".set        reorder"                  "\n\t"  \
      : "=f" (outf)                                  \
      : "f" (fs_f[i]) , "f" (ft_f[i])                \
      : "$f0", "$f1", "$f2"                          \
   );                                                \
   printf("%s, c.eq.s   out=%f, fs=%f, ft=%f\n",     \
          instruction, outf, fs_f[i], ft_f[i]);      \
}

#define TESTINST1d(instruction, RDval)               \
{                                                    \
   double outd = 0;                                  \
   __asm__ __volatile__(                             \
      ".set        noreorder"                "\n\t"  \
      "mov.d       $f1,   %1"                "\n\t"  \
      "mov.d       $f2,   %2"                "\n\t"  \
      "dmtc1       $zero, $f0"               "\n\t"  \
      "c.eq.d      $f1,   $f2"               "\n\t"  \
      instruction" end"instruction"d"#RDval  "\n\t"  \
      "nop"                                  "\n\t"  \
      "add.d       $f0,   $f0, $f1"          "\n\t"  \
      "end"instruction"d"#RDval":"           "\n\t"  \
      "add.d       $f0,   $f0, $f2"          "\n\t"  \
      "mov.d       %0,    $f0"               "\n\t"  \
      ".set        reorder"                  "\n\t"  \
      : "=f" (outd)                                  \
      : "f" (fs_d[i]) , "f" (ft_d[i])                \
      : "$f0", "$f1", "$f2"                          \
   );                                                \
   printf("%s, c.eq.d   out=%f, fs=%f, ft=%f\n",     \
          instruction, outd, fs_d[i], ft_d[i]);      \
}

#define TESTINST2s(instruction, RDval)               \
{                                                    \
   float outf = 0;                                   \
   __asm__ __volatile__(                             \
      ".set        noreorder"                "\n\t"  \
      "mov.s       $f1,   %1"                "\n\t"  \
      "mov.s       $f2,   %2"                "\n\t"  \
      "mtc1        $zero, $f0"               "\n\t"  \
      "c.eq.s      $f1,   $f2"               "\n\t"  \
      instruction" end"instruction"s"#RDval  "\n\t"  \
      "add.s       $f0,   $f0, $f1"          "\n\t"  \
      "end"instruction"s"#RDval":"           "\n\t"  \
      "add.s       $f0,   $f0, $f2"          "\n\t"  \
      "mov.s       %0, $f0"                  "\n\t"  \
      ".set        reorder"                  "\n\t"  \
      : "=f" (outf)                                  \
      : "f" (fs_f[i]) , "f" (ft_f[i])                \
      : "$f0", "$f1", "$f2"                          \
   );                                                \
   printf("%s, c.eq.s   out=%f, fs=%f, ft=%f\n",     \
          instruction, outf, fs_f[i], ft_f[i]);      \
}

#define TESTINST2d(instruction, RDval)               \
{                                                    \
   double outd = 0;                                  \
   __asm__ __volatile__(                             \
      ".set        noreorder"                "\n\t"  \
      "mov.d       $f1,   %1"                "\n\t"  \
      "mov.d       $f2,   %2"                "\n\t"  \
      "dmtc1       $zero, $f0"               "\n\t"  \
      "c.eq.d      $f1,   $f2"               "\n\t"  \
      instruction" end"instruction"d"#RDval  "\n\t"  \
      "add.d       $f0,   $f0, $f1"          "\n\t"  \
      "end"instruction"d"#RDval":"           "\n\t"  \
      "add.d       $f0,   $f0, $f2"          "\n\t"  \
      "mov.d       %0, $f0"                  "\n\t"  \
      ".set        reorder"                  "\n\t"  \
      : "=f" (outd)                                  \
      : "f" (fs_d[i]) , "f" (ft_d[i])                \
      : "$f0", "$f1", "$f2"                          \
   );                                                \
   printf("%s, c.eq.d   out=%f, fs=%f, ft=%f\n",     \
          instruction, outd, fs_d[i], ft_d[i]);      \
}

#define TESTINST_CONDs(instruction, RDval)       \
{                                                \
   float outf = 0;                               \
   __asm__ __volatile__(                         \
      ".set        noreorder"         "\n\t"     \
      "mov.s       $f1,   %1"         "\n\t"     \
      "mov.s       $f2,   %2"         "\n\t"     \
      "mov.s       $f0,   %1"         "\n\t"     \
      instruction" $f1,   $f2"        "\n\t"     \
      "bc1f end"instruction"s"#RDval  "\n\t"     \
      "nop"                           "\n\t"     \
      "add.s       $f0,   $f0, $f2"   "\n\t"     \
      "end"instruction"s"#RDval":"    "\n\t"     \
      "mov.s       %0,    $f0"        "\n\t"     \
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
      "mov.d       $f1,   %1"         "\n\t"     \
      "mov.d       $f2,   %2"         "\n\t"     \
      "mov.d       $f0,   %1"         "\n\t"     \
      instruction" $f1,   $f2"        "\n\t"     \
      "bc1f end"instruction"d"#RDval  "\n\t"     \
      "nop"                           "\n\t"     \
      "add.d       $f0,   $f0, $f2"   "\n\t"     \
      "end"instruction"d"#RDval":"    "\n\t"     \
      "mov.d       %0,    $f0"        "\n\t"     \
      ".set        reorder"           "\n\t"     \
      : "=f" (outd)                              \
      : "f" (fs_d[i]) , "f" (ft_d[i])            \
      : "$f0", "$f1", "$f2"                      \
   );                                            \
   printf("%s, bc1f   out=%f, fs=%f, ft=%f\n",   \
          instruction, outd, fs_d[i], ft_d[i]);  \
}
