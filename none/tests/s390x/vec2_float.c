#include <stdio.h>

typedef unsigned char __attribute__((vector_size(16))) charvec;

#define TO_CHARVEC(val)                                                        \
   ({                                                                          \
      union {                                                                  \
         typeof(val) x;                                                        \
         charvec     v;                                                        \
      } vec_pun = {val};                                                       \
      vec_pun.v;                                                               \
   })

#define FROM_CHARVEC(type, vec)                                                \
   ({                                                                          \
      union {                                                                  \
         charvec v;                                                            \
         type    x;                                                            \
      } vec_pun = {vec};                                                       \
      vec_pun.x;                                                               \
   })

typedef struct {
   unsigned int x[4];
} u2_v;

typedef struct {
   unsigned long x[2];
} u3_v;

typedef struct {
   unsigned long x[2]; /* avoid 128-bit integers */
} u4_v;

typedef struct {
   float x[4];
} f2_v;

typedef struct {
   double x[2];
} f3_v;

typedef struct {
   long double x[1];
} f4_v;

#define INF (1./0.)
#define NAN (0./0.)

static const u2_v vec_u2_a = {{-1, 0x7ffffff0, 42, 0}};
static const u2_v vec_u2_b = {{0x7fffff80, 1, 1000, -0x7caffe}};
static const u2_v vec_u2_c = {{0x7fffff, 0x70000000, -10000, 0xffff}};
static const u2_v vec_u2_e = {{0x7fffffff, 0x8000000, 0, 42}};

static const f2_v vec_f2_a = {{16777215., -16777215., 42.5, 10000.}};
static const f2_v vec_f2_b = {{4., 3., 42.5, 1.}};
static const f2_v vec_f2_c = {{0., INF, -0., -INF}};
static const f2_v vec_f2_d = {{-16777214., 16777214., -23., -9999.}};
static const f2_v vec_f2_e = {{NAN, -NAN, NAN, -NAN}};

static const f3_v vec_f3_a = {{(double)((1UL << 52) - 1), -16777215.}};
static const f3_v vec_f3_b = {{0.1, 3.}};
static const f3_v vec_f3_c = {{-0., INF}};
static const f3_v vec_f3_e = {{NAN, -NAN}};

static const f4_v vec_f4_a = {{16777215.0L}};
static const f4_v vec_f4_b = {{4.0L}};
static const f4_v vec_f4_c = {{INF}};
static const f4_v vec_f4_d = {{-23.0}};
static const f4_v vec_f4_e = {{NAN}};

static const u3_v vec_ini = {{0x0112233445566778, 0x899aabbccddeeff0}};

/* -- Dump a vector depending on FP format and single-element control -- */

static void dump_cc(int cc)
{
   if (cc) {
      printf("  cc=%d", cc);
   }
   putchar('\n');
}

static void dump_vu2(charvec v)
{
   u2_v u = FROM_CHARVEC(u2_v, v);
   printf("\t%d | %d | %d | %d", u.x[0], u.x[1], u.x[2], u.x[3]);
}

static void dump_wu2(charvec v)
{
   u2_v u = FROM_CHARVEC(u2_v, v);
   printf("\t%d | - | - | -", u.x[0]);
}

static void dump_wu4(charvec v)
{
   u4_v u = FROM_CHARVEC(u4_v, v);
   if (u.x[0] == 0) {
      printf("\t%ld", u.x[1]);
   } else if (u.x[0] == -1UL) {
      printf("\t-%ld", -u.x[1]);
   } else {
      printf("\t0x%016lx%016lx", u.x[0], u.x[1]);
   }
}

static void dump_vf2(charvec v)
{
   f2_v u = FROM_CHARVEC(f2_v, v);
   printf("\t%a | %a | %a | %a", u.x[0], u.x[1], u.x[2], u.x[3]);
}

static void dump_ef2(charvec v)
{
   f2_v u = FROM_CHARVEC(f2_v, v);
   printf("\t%a | - | %a | -", u.x[0], u.x[2]);
}

static void dump_vf3(charvec v)
{
   f3_v u = FROM_CHARVEC(f3_v, v);
   printf("\t%a | %a", u.x[0], u.x[1]);
}

static void dump_wf3(charvec v)
{
   f3_v u = FROM_CHARVEC(f3_v, v);
   printf("\t%a | -", u.x[0]);
}

static void dump_wf2(charvec v)
{
   f2_v u = FROM_CHARVEC(f2_v, v);
   printf("\t%a | - | - | -", u.x[0]);
}

static void dump_wf4(charvec v)
{
   f4_v u = FROM_CHARVEC(f4_v, v);
   printf("\t%La", u.x[0]);
}

/* -- Vector unary operators -- */

#define TEST_EXEC(opc1,opc2,fmt,ty1,ty2,insn,m3,m4,m5)                  \
   do {                                                                 \
      puts(#insn);                                                      \
      test_##insn##_##m3##_##m4##_##m5(vec_##ty2##_a);                  \
      test_##insn##_##m3##_##m4##_##m5(vec_##ty2##_b);                  \
      test_##insn##_##m3##_##m4##_##m5(vec_##ty2##_c);                  \
      test_##insn##_##m3##_##m4##_##m5(vec_##ty2##_e);                  \
   } while (0)

#define TEST_GENERATE(opc1, opc2, fmt, ty1, ty2, insn, m3, m4, m5)             \
   static void test_##insn##_##m3##_##m4##_##m5(ty2##_v a)                     \
   {                                                                           \
      charvec out = TO_CHARVEC(vec_ini);                                       \
      int     cc  = -1;                                                        \
      __asm__("cr 0,0\n\t"                                                     \
              ".insn vrr,0x" #opc1 "00000000" #opc2 ",%[out],%[a],0," #m3      \
              "," #m4 "," #m5 "\n\t"                                           \
              "ipm     %[cc]\n\t"                                              \
              "srl     %[cc],28"                                               \
              : [cc] "=d"(cc), [out] "+v"(out)                                 \
              : [a] "v"(TO_CHARVEC(a))                                         \
              : "cc");                                                         \
      dump_##fmt##ty1(out);                                                    \
      dump_cc(cc);                                                             \
   }

#define INSNS                                         \
   XTEST(e7,c0, v,u2, f2, vclfeb, 2,4,0);             \
   XTEST(e7,c0, w,u2, f2, wclfeb, 2,12,0);            \
   XTEST(e7,c1, v,f2, u2, vcelfb, 2,4,0);             \
   XTEST(e7,c1, w,f2, u2, wcelfb, 2,12,0);            \
   XTEST(e7,c2, v,u2, f2, vcfeb, 2,4,0);              \
   XTEST(e7,c2, w,u2, f2, wcfeb, 2,12,0);             \
   XTEST(e7,c3, v,f2, u2, vcefb, 2,4,0);              \
   XTEST(e7,c3, w,f2, u2, wcefb, 2,12,0);             \
   XTEST(e7,c4, v,f3, f2, vldeb, 2,0,0);              \
   XTEST(e7,c4, w,f3, f2, wldeb, 2,8,0);              \
   XTEST(e7,c4, w,f4, f3, wflld, 3,8,0);              \
   XTEST(e7,c5, e,f2, f3, vledb, 3,4,0);              \
   XTEST(e7,c5, w,f2, f3, wledb, 3,12,0);             \
   XTEST(e7,c5, w,f3, f4, wflrx, 4,12,0);             \
   XTEST(e7,c7, v,f2, f2, vfisb, 2,0,0);              \
   XTEST(e7,c7, w,f2, f2, wfisb, 2,8,0);              \
   XTEST(e7,c7, w,f4, f4, wfixb, 4,8,0);              \
   XTEST(e7,cc, v,f2, f2, vflcsb, 2,0,0);             \
   XTEST(e7,cc, w,f2, f2, wflcsb, 2,8,0);             \
   XTEST(e7,cc, w,f4, f4, wflcxb, 4,8,0);             \
   XTEST(e7,cc, v,f2, f2, vflnsb, 2,0,1);             \
   XTEST(e7,cc, w,f2, f2, wflnsb, 2,8,1);             \
   XTEST(e7,cc, w,f4, f4, wflnxb, 4,8,1);             \
   XTEST(e7,cc, v,f2, f2, vflpsb, 2,0,2);             \
   XTEST(e7,cc, w,f2, f2, wflpsb, 2,8,2);             \
   XTEST(e7,cc, w,f4, f4, wflpxb, 4,8,2);             \
   XTEST(e7,ce, v,f2, f2, vfsqsb, 2,0,0);             \
   XTEST(e7,ce, w,f2, f2, wfsqsb, 2,8,0);             \
   XTEST(e7,ce, w,f4, f4, wfsqxb, 4,8,0);

#define XTEST TEST_GENERATE
INSNS
#undef XTEST

static void test_all_unops()
{
#define XTEST TEST_EXEC
   INSNS
#undef XTEST
}

#undef INSNS
#undef TEST_GENERATE
#undef TEST_EXEC

/* -- Vector binary operators -- */

#define TEST_EXEC(opc1,opc2,fmt,ty1,insn,m4,m5,m6)                      \
   do {                                                                 \
      puts(#insn);                                                      \
      test_##insn##_##m4##_##m5##_##m6(vec_f##m4##_a, vec_f##m4##_b);   \
      test_##insn##_##m4##_##m5##_##m6(vec_f##m4##_b, vec_f##m4##_a);   \
      test_##insn##_##m4##_##m5##_##m6(vec_f##m4##_a, vec_f##m4##_c);   \
      test_##insn##_##m4##_##m5##_##m6(vec_f##m4##_d, vec_f##m4##_e);   \
   } while (0)

#define TEST_GENERATE(opc1, opc2, fmt, ty1, insn, m4, m5, m6)                  \
   static void test_##insn##_##m4##_##m5##_##m6(f##m4##_v a, f##m4##_v b)      \
   {                                                                           \
      charvec out = TO_CHARVEC(vec_ini);                                       \
      int     cc  = -1;                                                        \
      __asm__("cr 0,0\n\t"                                                     \
              ".insn vrr,0x" #opc1 "00000000" #opc2 ",%[out],%[a],%[b]," #m4   \
              "," #m5 "," #m6 "\n\t"                                           \
              "ipm     %[cc]\n\t"                                              \
              "srl     %[cc],28"                                               \
              : [cc] "=d"(cc), [out] "+v"(out)                                 \
              : [a] "v"(TO_CHARVEC(a)), [b] "v"(TO_CHARVEC(b))                 \
              : "cc");                                                         \
      dump_##fmt##ty1(out);                                                    \
      dump_cc(cc);                                                             \
   }

#define INSNS                                     \
   XTEST(e7,e2, v,f2, vfssb, 2,0,0);              \
   XTEST(e7,e2, w,f2, wfssb, 2,8,0);              \
   XTEST(e7,e2, w,f4, wfsxb, 4,8,0);              \
   XTEST(e7,e3, v,f2, vfasb, 2,0,0);              \
   XTEST(e7,e3, w,f2, wfasb, 2,8,0);              \
   XTEST(e7,e3, w,f4, wfaxb, 4,8,0);              \
   XTEST(e7,e5, v,f2, vfdsb, 2,0,0);              \
   XTEST(e7,e5, w,f2, wfdsb, 2,8,0);              \
   XTEST(e7,e5, w,f4, wfdxb, 4,8,0);              \
   XTEST(e7,e7, v,f2, vfmsb, 2,0,0);              \
   XTEST(e7,e7, w,f2, wfmsb, 2,8,0);              \
   XTEST(e7,e7, w,f4, wfmxb, 4,8,0);              \
   XTEST(e7,e8, v,u2, vfcesb, 2,0,0);             \
   XTEST(e7,e8, w,u2, wfcesb, 2,8,0);             \
   XTEST(e7,e8, w,u4, wfcexb, 4,8,0);             \
   XTEST(e7,e8, v,u2, vfcesbs, 2,0,1);            \
   XTEST(e7,e8, w,u2, wfcesbs, 2,8,1);            \
   XTEST(e7,e8, w,u4, wfcexbs, 4,8,1);            \
   XTEST(e7,ea, v,u2, vfchesb, 2,0,0);            \
   XTEST(e7,ea, w,u2, wfchesb, 2,8,0);            \
   XTEST(e7,ea, w,u4, wfchexb, 4,8,0);            \
   XTEST(e7,ea, v,u2, vfchesbs, 2,0,1);           \
   XTEST(e7,ea, w,u2, wfchesbs, 2,8,1);           \
   XTEST(e7,ea, w,u4, wfchexbs, 4,8,1);           \
   XTEST(e7,ee, v,f2, vfminsb, 2,0,0);            \
   XTEST(e7,ee, w,f2, wfminsb, 2,8,0);            \
   XTEST(e7,ee, w,f4, wfminxb, 4,8,0);            \
   XTEST(e7,ef, v,f2, vfmaxsb, 2,0,0);            \
   XTEST(e7,ef, w,f2, wfmaxsb, 2,8,0);            \
   XTEST(e7,ef, w,f4, wfmaxxb, 4,8,0);            \
   XTEST(e7,eb, v,u2, vfchsb, 2,0,0);             \
   XTEST(e7,eb, w,u2, wfchsb, 2,8,0);             \
   XTEST(e7,eb, w,u4, wfchxb, 4,8,0);             \
   XTEST(e7,eb, v,u2, vfchsbs, 2,0,1);            \
   XTEST(e7,eb, w,u2, wfchsbs, 2,8,1);            \
   XTEST(e7,eb, w,u4, wfchxbs, 4,8,1);

#define XTEST TEST_GENERATE
INSNS
#undef XTEST

static void test_all_binops()
{
#define XTEST TEST_EXEC
   INSNS
#undef XTEST
}

#undef INSNS
#undef TEST_GENERATE
#undef TEST_EXEC

/* -- Vector ternary operators -- */

#define TEST_EXEC(opc1,opc2,fmt,ty,insn,m5,m6)                          \
   do {                                                                 \
      puts(#insn);                                                      \
      test_##insn##_##m5##_##m6(vec_##ty##_a, vec_##ty##_b, vec_##ty##_c); \
      test_##insn##_##m5##_##m6(vec_##ty##_b, vec_##ty##_a, vec_##ty##_d); \
   } while (0)

#define TEST_GENERATE(opc1, opc2, fmt, ty, insn, m5, m6)                       \
   static void test_##insn##_##m5##_##m6(ty##_v a, ty##_v b, ty##_v c)         \
   {                                                                           \
      charvec          out                = TO_CHARVEC(vec_ini);               \
      int              cc                 = -1;                                \
      register charvec my_b __asm__("v6") = TO_CHARVEC(b);                     \
      register charvec my_c __asm__("v7") = TO_CHARVEC(c);                     \
      __asm__("cr 0,0\n\t"                                                     \
              ".insn vri,0x" #opc1 "00000000" #opc2 ",%[out],%[a],0x6" #m6     \
              "0,7," #m5 "\n\t"                                                \
              "ipm     %[cc]\n\t"                                              \
              "srl     %[cc],28"                                               \
              : [cc] "=d"(cc), [out] "+v"(out)                                 \
              : [a] "v"(TO_CHARVEC(a)), [b] "v"(my_b), [c] "v"(my_c)           \
              : "cc");                                                         \
      dump_##fmt##ty(out);                                                     \
      dump_cc(cc);                                                             \
   }

#define INSNS                                   \
   XTEST(e7,8e, v,f2, vfmssb, 0,2);             \
   XTEST(e7,8e, w,f2, wfmssb, 8,2);             \
   XTEST(e7,8e, w,f4, wfmsxb, 8,4);             \
   XTEST(e7,8f, v,f2, vfmasb, 0,2);             \
   XTEST(e7,8f, w,f2, wfmasb, 8,2);             \
   XTEST(e7,8f, w,f4, wfmaxb, 8,4);             \
   XTEST(e7,9e, v,f2, vfnmssb, 0,2);            \
   XTEST(e7,9e, w,f2, wfnmssb, 8,2);            \
   XTEST(e7,9e, w,f4, wfnmsxb, 8,4);            \
   XTEST(e7,9f, v,f2, vfnmasb, 0,2);            \
   XTEST(e7,9f, w,f2, wfnmasb, 8,2);            \
   XTEST(e7,9f, w,f4, wfnmaxb, 8,4);

#define XTEST TEST_GENERATE
INSNS
#undef XTEST

static void test_all_ternops()
{
#define XTEST TEST_EXEC
   INSNS
#undef XTEST
}

#undef INSNS
#undef TEST_GENERATE
#undef TEST_EXEC

/* -- Vector scalar compare operators -- */

#define TEST_EXEC(opc1,opc2,ty,insn,m3,m4)                              \
   do {                                                                 \
      puts(#insn);                                                      \
      test_##insn##_##m3##_##m4(vec_##ty##_a, vec_##ty##_b);            \
      test_##insn##_##m3##_##m4(vec_##ty##_b, vec_##ty##_a);            \
      test_##insn##_##m3##_##m4(vec_##ty##_c, vec_##ty##_c);            \
      test_##insn##_##m3##_##m4(vec_##ty##_a, vec_##ty##_e);            \
   } while (0)

#define TEST_GENERATE(opc1, opc2, ty, insn, m3, m4)                            \
   static void test_##insn##_##m3##_##m4(ty##_v a, ty##_v b)                   \
   {                                                                           \
      int cc = -1;                                                             \
      __asm__("cr 0,0\n\t"                                                     \
              ".insn vrr,0x" #opc1 "00000000" #opc2 ",%[a],%[b],0," #m3        \
              "," #m4 ",0\n\t"                                                 \
              "ipm     %[cc]\n\t"                                              \
              "srl     %[cc],28"                                               \
              : [cc] "=d"(cc)                                                  \
              : [a] "v"(TO_CHARVEC(a)), [b] "v"(TO_CHARVEC(b))                 \
              : "cc");                                                         \
      printf("\tcc=%d\n", cc);                                                 \
   }

#define INSNS                                         \
   XTEST(e7,cb, f2, wfcsb, 2,0);                      \
   XTEST(e7,cb, f4, wfcxb, 4,0);

#define XTEST TEST_GENERATE
INSNS
#undef XTEST

static void test_all_scalar_compares()
{
#define XTEST TEST_EXEC
   INSNS
#undef XTEST
}

#undef INSNS
#undef TEST_GENERATE
#undef TEST_EXEC

/* -- Vector FP test data class -- */

#define TEST_EXEC(opc1,opc2,fmt,ty1,ty2,insn,i3,m4,m5)                  \
   do {                                                                 \
      puts(#insn);                                                      \
      test_##insn##_##m3##_##m4(vec_##ty2##_a);                         \
      test_##insn##_##m3##_##m4(vec_##ty2##_b);                         \
      test_##insn##_##m3##_##m4(vec_##ty2##_c);                         \
      test_##insn##_##m3##_##m4(vec_##ty2##_d);                         \
      test_##insn##_##m3##_##m4(vec_##ty2##_e);                         \
   } while (0)

#define TEST_GENERATE(opc1, opc2, fmt, ty1, ty2, insn, i3, m4, m5)             \
   static void test_##insn##_##m3##_##m4(ty2##_v a)                            \
   {                                                                           \
      charvec out = TO_CHARVEC(vec_ini);                                       \
      int     cc  = -1;                                                        \
      __asm__("cr 0,0\n\t"                                                     \
              ".insn vri,0x" #opc1 "00000000" #opc2 ",%[out],%[a]," #i3        \
              "," #m4 "," #m5 "\n\t"                                           \
              "ipm     %[cc]\n\t"                                              \
              "srl     %[cc],28"                                               \
              : [cc] "=d"(cc), [out] "+v"(out)                                 \
              : [a] "v"(TO_CHARVEC(a))                                         \
              : "cc");                                                         \
      dump_##fmt##ty1(out);                                                    \
      dump_cc(cc);                                                             \
   }

#define INSNS                                         \
   XTEST(e7,4a, v,u2,f2, vftcisb, 0x864,2,0);         \
   XTEST(e7,4a, w,u2,f2, wftcisb, 0x520,2,8);         \
   XTEST(e7,4a, w,u4,f4, wftcixb, 0x3c0,4,0);

#define XTEST TEST_GENERATE
INSNS
#undef XTEST

static void test_all_vftci()
{
#define XTEST TEST_EXEC
   INSNS
#undef XTEST
}

#undef INSNS
#undef TEST_GENERATE
#undef TEST_EXEC


int main()
{
   test_all_unops();
   test_all_binops();
   test_all_ternops();
   test_all_scalar_compares();
   test_all_vftci();
   return 0;
}
