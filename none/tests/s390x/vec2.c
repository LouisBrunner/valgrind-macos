#include <stdio.h>

#define VECTOR __attribute__ ((vector_size (16)))

typedef unsigned long VECTOR ulong_v;
typedef float VECTOR float_v;

static const ulong_v vec_a   = { 0x0123456789abcdef, 0xfedcba9876543210 };
static const ulong_v vec_b   = { 0xfedcba9876543210, 0x0123456789abcdef };
static const ulong_v vec_c   = { 0x8040201008040201, 0x7fbfdfeff7fbfdfe };
static const ulong_v vec_one = { -1, -1 };
static const ulong_v vec_ini = { 0x0112233445566778, 0x899aabbccddeeff0 };

static const float_v vec_fa  = { 16777215., -16777215., 42.5, 10000. };
static const float_v vec_fb  = { 4., 3., 2., 1. };

/* -- Vector shift -- */

#define TEST_GENERATE(insn)                             \
   static void test_##insn(ulong_v a, ulong_v b)        \
   {                                                    \
      ulong_v out;                                      \
      __asm__(                                          \
         #insn " %[out],%[a],%[b]"                      \
         : [out] "=v" (out)                             \
         : [a] "v" (a),                                 \
           [b] "v" (b)                                  \
         : );                                           \
      printf("\t%016lx %016lx\n", out[0], out[1]);      \
   }

#define TEST_EXEC(insn)                         \
   do {                                         \
      puts(#insn);                              \
      test_##insn(vec_a, vec_b);                \
      test_##insn(vec_b, vec_a);                \
      test_##insn(vec_c, vec_a);                \
      test_##insn(vec_one, vec_b);              \
   } while (0)

#define INSNS                                   \
   XTEST(vsl);                                  \
   XTEST(vsrl);                                 \
   XTEST(vsra);

#define XTEST TEST_GENERATE
INSNS
#undef XTEST

static void test_all_single_bitshifts()
{
#define XTEST TEST_EXEC
   INSNS
#undef XTEST
}
#undef INSNS
#undef TEST_EXEC
#undef TEST_GENERATE

/* -- Vector load element-/byte-swapped -- */

#define TEST_EXEC(opc1,opc2,insn,m3)            \
   do {                                         \
      puts(#insn " " #m3);                      \
      test_##insn##_##m3(vec_a);                \
      test_##insn##_##m3(vec_b);                \
   } while (0)

#define TEST_GENERATE(opc1,opc2,insn,m3)                                \
   static void test_##insn##_##m3(ulong_v a)                            \
   {                                                                    \
      ulong_v out = vec_ini;                                            \
      __asm__(                                                          \
         ".insn vrx,0x" #opc1 "00000000" #opc2 ",%[out],%[a]," #m3      \
         : [out] "+v" (out)                                             \
         : [a] "R" (a)                                                  \
         : );                                                           \
      printf("\t%016lx %016lx\n", out[0], out[1]);                      \
   }

#define INSNS                                   \
   XTEST(e6,01, vlebrh, 0);                     \
   XTEST(e6,01, vlebrh, 7);                     \
   XTEST(e6,01, vlebrh, 2);                     \
   XTEST(e6,03, vlebrf, 0);                     \
   XTEST(e6,03, vlebrf, 3);                     \
   XTEST(e6,03, vlebrf, 1);                     \
   XTEST(e6,02, vlebrg, 0);                     \
   XTEST(e6,02, vlebrg, 1);                     \
   XTEST(e6,04, vllebrz, 1);                    \
   XTEST(e6,04, vllebrz, 2);                    \
   XTEST(e6,04, vllebrz, 3);                    \
   XTEST(e6,04, vllebrz, 6);                    \
   XTEST(e6,05, vlbrrep, 1);                    \
   XTEST(e6,05, vlbrrep, 2);                    \
   XTEST(e6,05, vlbrrep, 3);                    \
   XTEST(e6,06, vlbr, 1);                       \
   XTEST(e6,06, vlbr, 2);                       \
   XTEST(e6,06, vlbr, 3);                       \
   XTEST(e6,06, vlbr, 4);                       \
   XTEST(e6,07, vler, 1);                       \
   XTEST(e6,07, vler, 2);                       \
   XTEST(e6,07, vler, 3);

#define XTEST TEST_GENERATE
INSNS
#undef XTEST

static void test_all_swapped_loads()
{
#define XTEST TEST_EXEC
   INSNS
#undef XTEST
}

#undef INSNS
#undef TEST_GENERATE

/* -- Vector store element-/byte-swapped -- */

#define TEST_GENERATE(opc1,opc2,insn,m3)                                \
   static void test_##insn##_##m3(ulong_v a)                            \
   {                                                                    \
      ulong_v out = vec_ini;                                            \
      __asm__(                                                          \
         ".insn vrx,0x" #opc1 "00000000" #opc2 ",%[a],%[out]," #m3      \
         : [out] "+R" (out)                                             \
         : [a] "v" (a)                                                  \
         : );                                                           \
      printf("\t%016lx %016lx\n", out[0], out[1]);                      \
   }

#define INSNS                                   \
   XTEST(e6,09, vstebrh, 0);                    \
   XTEST(e6,09, vstebrh, 7);                    \
   XTEST(e6,09, vstebrh, 2);                    \
   XTEST(e6,0b, vstebrf, 0);                    \
   XTEST(e6,0b, vstebrf, 3);                    \
   XTEST(e6,0b, vstebrf, 1);                    \
   XTEST(e6,0a, vstebrg, 0);                    \
   XTEST(e6,0a, vstebrg, 1);                    \
   XTEST(e6,0e, vstbr, 1);                      \
   XTEST(e6,0e, vstbr, 2);                      \
   XTEST(e6,0e, vstbr, 3);                      \
   XTEST(e6,0e, vstbr, 4);                      \
   XTEST(e6,0f, vster, 1);                      \
   XTEST(e6,0f, vster, 2);                      \
   XTEST(e6,0f, vster, 3);

#define XTEST TEST_GENERATE
INSNS
#undef XTEST

static void test_all_swapped_stores()
{
#define XTEST TEST_EXEC
   INSNS
#undef XTEST
}

#undef INSNS
#undef TEST_EXEC
#undef TEST_GENERATE

/* -- Vector shift double by bit -- */

#define TEST_GENERATE(opc1,opc2,insn,i4)                \
   static void test_##insn##_##i4(ulong_v a, ulong_v b) \
   {                                                    \
      ulong_v out = vec_ini;                            \
      __asm__(                                          \
         ".insn vrr,0x" #opc1 "00000000" #opc2          \
         ",%[out],%[a],%[b],0," #i4 ",0"                \
         : [out] "+v" (out)                             \
         : [a] "v" (a),                                 \
           [b] "v" (b)                                  \
         : );                                           \
      printf("\t%016lx %016lx\n", out[0], out[1]);      \
   }

#define TEST_EXEC(opc1,opc2,insn,i4)            \
   do {                                         \
      puts(#insn " " #i4);                      \
      test_##insn##_##i4(vec_a, vec_one);       \
      test_##insn##_##i4(vec_b, vec_a);         \
   } while (0)

#define INSNS                                   \
   XTEST(e7,86,vsld,0);                         \
   XTEST(e7,86,vsld,7);                         \
   XTEST(e7,86,vsld,4);                         \
   XTEST(e7,87,vsrd,0);                         \
   XTEST(e7,87,vsrd,7);                         \
   XTEST(e7,87,vsrd,4);

#define XTEST TEST_GENERATE
INSNS
#undef XTEST

static void test_all_double_bitshifts()
{
#define XTEST TEST_EXEC
   INSNS
#undef XTEST
}

#undef INSNS
#undef TEST_EXEC
#undef TEST_GENERATE

/* -- Vector integer -> FP conversions -- */

#define TEST_GENERATE(opc1,opc2,insn,m4)                                \
   static void test_##insn##_##m4(ulong_v a)                            \
   {                                                                    \
      float_v out;                                                      \
      __asm__(                                                          \
         ".insn vrr,0x" #opc1 "00000000" #opc2                          \
         ",%[out],%[a],0,2," #m4 ",0"                                   \
         : [out] "=v" (out)                                             \
         : [a] "v" (a)                                                  \
         : );                                                           \
      if (m4 & 8)                                                       \
         printf("\t%a - - -\n", out[0]);                                \
      else                                                              \
         printf("\t%a %a %a %a\n", out[0], out[1], out[2], out[3]);     \
   }

#define TEST_EXEC(opc1,opc2,insn,m4)            \
   do {                                         \
      puts(#insn " " #m4);                      \
      test_##insn##_##m4(vec_a);                \
      test_##insn##_##m4(vec_c);                \
   } while (0)

#define INSNS                                   \
   XTEST(e7,c1,vcfpl,0);                        \
   XTEST(e7,c1,vcfpl,8);                        \
   XTEST(e7,c3,vcfps,0);                        \
   XTEST(e7,c3,vcfps,8);

#define XTEST TEST_GENERATE
INSNS
#undef XTEST

static void test_all_int_fp_conversions()
{
#define XTEST TEST_EXEC
   INSNS
#undef XTEST
}

#undef INSNS
#undef TEST_EXEC
#undef TEST_GENERATE

/* -- Vector FP -> integer conversions -- */

#define TEST_GENERATE(opc1,opc2,insn,m4)                                \
   static void test_##insn##_##m4(float_v a)                            \
   {                                                                    \
      unsigned int VECTOR out;                                          \
      __asm__(                                                          \
         ".insn vrr,0x" #opc1 "00000000" #opc2                          \
         ",%[out],%[a],0,2," #m4 ",0"                                   \
         : [out] "=v" (out)                                             \
         : [a] "v" (a)                                                  \
         : );                                                           \
      if (m4 & 8)                                                       \
         printf("\t%08x - - -\n", out[0]);                              \
      else                                                              \
         printf("\t%08x %08x %08x %08x\n",                              \
                out[0], out[1], out[2], out[3]);                        \
   }

#define TEST_EXEC(opc1,opc2,insn,m4)            \
   do {                                         \
      puts(#insn " " #m4);                      \
      test_##insn##_##m4(vec_fa);               \
      test_##insn##_##m4(vec_fb);               \
   } while (0)

#define INSNS                                   \
   XTEST(e7,c0,vclfp,0);                        \
   XTEST(e7,c0,vclfp,8);                        \
   XTEST(e7,c2,vcsfp,0);                        \
   XTEST(e7,c2,vcsfp,8);

#define XTEST TEST_GENERATE
INSNS
#undef XTEST

static void test_all_fp_int_conversions()
{
#define XTEST TEST_EXEC
   INSNS
#undef XTEST
}

#undef INSNS
#undef TEST_EXEC
#undef TEST_GENERATE

/* -- Vector generate mask -- */

#define XTEST(insn, i2, i3)                                                    \
   do {                                                                        \
      ulong_v out = vec_ini;                                                   \
      puts(#insn " " #i2 "," #i3);                                             \
      __asm__(#insn " %[out]," #i2 "," #i3 : [out] "+v"(out) : :);             \
      printf("\t%016lx %016lx\n", out[0], out[1]);                             \
   } while (0)

static void test_all_generate_mask()
{
   XTEST(vgmb, 2, 1);
   XTEST(vgmb, 0xf7, 0x30);
   XTEST(vgmb, 0, 0);
   XTEST(vgmh, 3, 2);
   XTEST(vgmh, 15, 15);
   XTEST(vgmf, 4, 3);
   XTEST(vgmf, 16, 17);
   XTEST(vgmg, 55, 63);
   XTEST(vgmg, 43, 55);
   XTEST(vgmg, 63, 2);
}

#undef XTEST

int main()
{
   test_all_single_bitshifts();
   test_all_swapped_loads();
   test_all_swapped_stores();
   test_all_double_bitshifts();
   test_all_int_fp_conversions();
   test_all_fp_int_conversions();
   test_all_generate_mask();
   return 0;
}
