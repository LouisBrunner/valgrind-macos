#include <stdio.h>

#define VECTOR __attribute__((vector_size(16)))

typedef unsigned long VECTOR ulong_v;
typedef float VECTOR         float_v;

static const ulong_v v0 = {0x0000000000000000, 0x0000000000000000};
static const ulong_v v8 = {0x8000000000000000, 0x0000000000000000};
static const ulong_v va = {0x56aaaaaaaaaaaaaa, 0xbcdeccf0aa330f55};
static const ulong_v vb = {0xbbbbbbbbbbbbbbbb, 0x6789f0aa4c0f5533};
static const ulong_v vc = {0x00cc00000000c08c, 0x1234aaccf055330f};
static const ulong_v vd = {0x0000000000000000, 0x00000cafe0000000};
static const ulong_v ve = {0xfedcba9876543210, 0x0000000000000000};
static const ulong_v vf = {0xffffffffffffffff, 0xffffffffffffffff};

/* -- Vector evaluate -- */

#define TEST_VEVAL(mh, ml)                                                     \
   {                                                                           \
      ulong_v          out;                                                    \
      register ulong_v c __asm__("v4") = vc;                                   \
      __asm__(".insn vrr,0xe70000000088,%0,%1,%2,4,0x" #ml ",0x" #mh           \
              : [out] "=v"(out)                                                \
              : "v"(va), "v"(vb), "v"(c)                                       \
              :);                                                              \
      printf("\t%016lx %016lx\n", out[0], out[1]);                             \
   }

static void test_all_veval(void)
{
   puts("veval");
   TEST_VEVAL(0, 0);
   TEST_VEVAL(0, 1);
   TEST_VEVAL(0, 2);
   TEST_VEVAL(1, 0);
   TEST_VEVAL(8, 0);
   TEST_VEVAL(4, e);
   TEST_VEVAL(f, f);
}

/* -- Vector generate element masks -- */

#define TEST_VGEM(m3, op)                                                      \
   {                                                                           \
      ulong_v out;                                                             \
      __asm__(".insn vri,0xe70000000054,%0,%1,0," #m3 ",0"                     \
              : [out] "=v"(out)                                                \
              : "v"(op)                                                        \
              :);                                                              \
      printf("\t%016lx %016lx\n", out[0], out[1]);                             \
   }

static void test_all_vgem(void)
{
   puts("vgem");
   TEST_VGEM(0, va);
   TEST_VGEM(1, va);
   TEST_VGEM(2, va);
   TEST_VGEM(3, va);
   TEST_VGEM(4, va);
   TEST_VGEM(4, vb);
}

/* -- Various vector insns with a single input operand -- */

#define TEST_V1OP(m3, opc, op)                                                 \
   {                                                                           \
      ulong_v out;                                                             \
      __asm__(".insn vrr,0xe700000000" opc ",%0,%1,0," #m3 ",0,0"              \
              : [out] "=v"(out)                                                \
              : "v"(op)                                                        \
              :);                                                              \
      printf("\t%016lx %016lx\n", out[0], out[1]);                             \
   }

static void test_all_v1op(void)
{
   puts("vupll");
   TEST_V1OP(3, "d4", va);
   TEST_V1OP(3, "d4", ve);
   TEST_V1OP(3, "d4", vc);

   puts("vuplh");
   TEST_V1OP(3, "d5", va);
   TEST_V1OP(3, "d5", ve);
   TEST_V1OP(3, "d5", vc);

   puts("vupl");
   TEST_V1OP(3, "d6", va);
   TEST_V1OP(3, "d6", ve);
   TEST_V1OP(3, "d6", vc);

   puts("vuph");
   TEST_V1OP(3, "d7", va);
   TEST_V1OP(3, "d7", ve);
   TEST_V1OP(3, "d7", vc);

   puts("vclz");
   TEST_V1OP(4, "53", v0);
   TEST_V1OP(4, "53", vc);
   TEST_V1OP(4, "53", vd);
   TEST_V1OP(4, "53", ve);

   puts("vctz");
   TEST_V1OP(4, "52", v0);
   TEST_V1OP(4, "52", vc);
   TEST_V1OP(4, "52", vd);
   TEST_V1OP(4, "52", ve);

   puts("vlc");
   TEST_V1OP(4, "de", v0);
   TEST_V1OP(4, "de", vc);
   TEST_V1OP(4, "de", ve);
   TEST_V1OP(4, "de", v8);

   puts("vlp");
   TEST_V1OP(4, "df", v0);
   TEST_V1OP(4, "df", vc);
   TEST_V1OP(4, "df", ve);
   TEST_V1OP(4, "de", v8);
}

/* -- Various vector insns with two input operands -- */

#define TEST_V2OP(m4, opc, a, b, m5)                                           \
   {                                                                           \
      ulong_v out;                                                             \
      __asm__(".insn vrr,0xe700000000" opc ",%0,%1,%2," #m4 "," #m5 ",0"       \
              : [out] "=v"(out)                                                \
              : "v"(a), "v"(b)                                                 \
              :);                                                              \
      printf("\t%016lx %016lx\n", out[0], out[1]);                             \
   }

#define TEST_V2OPC(m4, opc, a, b, m5)                                          \
   {                                                                           \
      ulong_v out;                                                             \
      int     cc;                                                              \
                                                                               \
      __asm__("cr    0,0\n\t" /* Clear CC */                                   \
              ".insn vrr,0xe700000000" opc ",%0,%2,%3," #m4 ",0," #m5 "\n\t"   \
              "ipm   %[cc]\n\t"                                                \
              "srl   %[cc],28\n"                                               \
              : [out] "=v"(out), [cc] "=d"(cc)                                 \
              : "v"(a), "v"(b)                                                 \
              : "cc");                                                         \
      printf("\t%016lx %016lx cc=%d\n", out[0], out[1], cc);                   \
   }

static void test_all_v2op(void)
{
   puts("vmx");
   TEST_V2OP(4, "ff", va, vb, 0);
   TEST_V2OP(4, "ff", v0, v8, 0);
   TEST_V2OP(4, "ff", v0, vd, 0);
   TEST_V2OP(4, "ff", v8, ve, 0);

   puts("vmxl");
   TEST_V2OP(4, "fd", va, vb, 0);
   TEST_V2OP(4, "fd", v0, v8, 0);
   TEST_V2OP(4, "fd", v0, vd, 0);
   TEST_V2OP(4, "fd", v8, ve, 0);

   puts("vmn");
   TEST_V2OP(4, "fe", va, vb, 0);
   TEST_V2OP(4, "fe", v0, v8, 0);
   TEST_V2OP(4, "fe", v0, vd, 0);
   TEST_V2OP(4, "fe", v8, ve, 0);

   puts("vmnl");
   TEST_V2OP(4, "fc", va, vb, 0);
   TEST_V2OP(4, "fc", v0, v8, 0);
   TEST_V2OP(4, "fc", v0, vd, 0);
   TEST_V2OP(4, "fc", v8, ve, 0);

   puts("vavg");
   TEST_V2OP(4, "f2", v0, v8, 0);
   TEST_V2OP(4, "f2", vc, vd, 0);
   TEST_V2OP(4, "f2", va, vb, 0);
   TEST_V2OP(4, "f2", ve, v8, 0);

   puts("vavgl");
   TEST_V2OP(4, "f0", v0, v8, 0);
   TEST_V2OP(4, "f0", vc, vd, 0);
   TEST_V2OP(4, "f0", va, vb, 0);
   TEST_V2OP(4, "f0", ve, v8, 0);

   puts("vceq");
   TEST_V2OPC(4, "f8", vb, vb, 0);
   TEST_V2OPC(4, "f8", ve, v8, 0);
   TEST_V2OPC(4, "f8", vc, vc, 1);
   TEST_V2OPC(4, "f8", vd, v0, 1);
   TEST_V2OPC(4, "f8", ve, v0, 1);
   TEST_V2OPC(4, "f8", vb, ve, 1);

   puts("vch");
   TEST_V2OPC(4, "fb", vb, vb, 0);
   TEST_V2OPC(4, "fb", ve, v8, 0);
   TEST_V2OPC(4, "fb", vc, vc, 1);
   TEST_V2OPC(4, "fb", vd, v0, 1);
   TEST_V2OPC(4, "fb", ve, v0, 1);
   TEST_V2OPC(4, "fb", vb, ve, 1);

   puts("vchl");
   TEST_V2OP(4, "f9", vb, vb, 0);
   TEST_V2OP(4, "f9", ve, v8, 0);
   TEST_V2OP(4, "f9", vc, vc, 1);
   TEST_V2OP(4, "f9", vd, v0, 1);
   TEST_V2OP(4, "f9", ve, v0, 1);
   TEST_V2OP(4, "f9", vb, ve, 1);

   puts("vme");
   TEST_V2OP(3, "a6", v8, vc, 0);
   TEST_V2OP(3, "a6", va, vb, 0);
   TEST_V2OP(3, "a6", vc, vc, 0);
   TEST_V2OP(3, "a6", vc, ve, 0);

   puts("vmle");
   TEST_V2OP(3, "a4", v8, vc, 0);
   TEST_V2OP(3, "a4", va, vb, 0);
   TEST_V2OP(3, "a4", vc, vc, 0);
   TEST_V2OP(3, "a4", vc, ve, 0);

   puts("vmo");
   TEST_V2OP(3, "a7", ve, va, 0);
   TEST_V2OP(3, "a7", va, vb, 0);
   TEST_V2OP(3, "a7", vc, vd, 0);
   TEST_V2OP(3, "a7", vd, vd, 0);

   puts("vmlo");
   TEST_V2OP(3, "a5", ve, va, 0);
   TEST_V2OP(3, "a5", va, vb, 0);
   TEST_V2OP(3, "a5", vc, vd, 0);
   TEST_V2OP(3, "a5", vd, vd, 0);

   puts("vml");
   TEST_V2OP(3, "a2", v8, vc, 0);
   TEST_V2OP(3, "a2", va, vb, 0);
   TEST_V2OP(3, "a2", va, vc, 0);
   TEST_V2OP(3, "a2", vf, vf, 0);
   TEST_V2OP(4, "a2", v8, vc, 0);
   TEST_V2OP(4, "a2", va, vb, 0);
   TEST_V2OP(4, "a2", va, vc, 0);
   TEST_V2OP(4, "a2", vf, vf, 0);

   puts("vmh");
   TEST_V2OP(3, "a3", v8, vc, 0);
   TEST_V2OP(3, "a3", va, vb, 0);
   TEST_V2OP(3, "a3", va, vc, 0);
   TEST_V2OP(3, "a3", vf, vf, 0);
   TEST_V2OP(4, "a3", v8, vc, 0);
   TEST_V2OP(4, "a3", va, vb, 0);
   TEST_V2OP(4, "a3", va, vc, 0);
   TEST_V2OP(4, "a3", vf, vf, 0);

   puts("vmlh");
   TEST_V2OP(3, "a1", v8, vc, 0);
   TEST_V2OP(3, "a1", va, vb, 0);
   TEST_V2OP(3, "a1", va, vc, 0);
   TEST_V2OP(3, "a1", vf, vf, 0);
   TEST_V2OP(4, "a1", v8, vc, 0);
   TEST_V2OP(4, "a1", va, vb, 0);
   TEST_V2OP(4, "a1", va, vc, 0);
   TEST_V2OP(4, "a1", vf, vf, 0);
}

/* -- Various vector insns with three input operands -- */

#define TEST_V3OP(m5, opc, a, b, c)                                            \
   {                                                                           \
      ulong_v          out;                                                    \
      register ulong_v v3 __asm__("v3") = b;                                   \
      register ulong_v v4 __asm__("v4") = c;                                   \
                                                                               \
      __asm__(".insn vri,0xe700000000" opc ",%0,%1,0x3" #m5 "0,4,0"            \
              : [out] "=v"(out)                                                \
              : "v"(a), "v"(v3), "v"(v4)                                       \
              :);                                                              \
      printf("\t%016lx %016lx\n", out[0], out[1]);                             \
   }

static void test_all_v3op(void)
{
   puts("vblend");
   TEST_V3OP(0, "89", va, vb, vc);
   TEST_V3OP(1, "89", va, vb, vc);
   TEST_V3OP(2, "89", va, vb, vc);
   TEST_V3OP(3, "89", va, vb, vc);
   TEST_V3OP(4, "89", va, vb, vc);

   puts("vmae");
   TEST_V3OP(3, "ae", v8, vc, v0);
   TEST_V3OP(3, "ae", va, vb, vc);
   TEST_V3OP(3, "ae", va, vc, vb);
   TEST_V3OP(3, "ae", v8, v8, ve);

   puts("vmale");
   TEST_V3OP(3, "ac", v8, vc, v0);
   TEST_V3OP(3, "ac", va, vb, vc);
   TEST_V3OP(3, "ac", va, vc, vb);
   TEST_V3OP(3, "ac", v8, v8, ve);

   puts("vmao");
   TEST_V3OP(3, "af", ve, va, v0);
   TEST_V3OP(3, "af", va, vb, vc);
   TEST_V3OP(3, "af", vc, vd, vb);
   TEST_V3OP(3, "af", vd, vd, ve);

   puts("vmalo");
   TEST_V3OP(3, "ad", ve, va, v0);
   TEST_V3OP(3, "ad", va, vb, vc);
   TEST_V3OP(3, "ad", vc, vd, vb);
   TEST_V3OP(3, "ad", vd, vd, ve);

   puts("vmal");
   TEST_V3OP(3, "aa", v8, vc, vd);
   TEST_V3OP(3, "aa", va, vb, vc);
   TEST_V3OP(3, "aa", va, vc, vb);
   TEST_V3OP(3, "aa", vf, vf, v8);
   TEST_V3OP(4, "aa", v8, vc, vd);
   TEST_V3OP(4, "aa", va, vb, vc);
   TEST_V3OP(4, "aa", va, vc, vb);
   TEST_V3OP(4, "aa", vf, vf, v8);

   puts("vmah");
   TEST_V3OP(3, "ab", v8, vc, vd);
   TEST_V3OP(3, "ab", va, vb, vc);
   TEST_V3OP(3, "ab", va, vc, vb);
   TEST_V3OP(3, "ab", vf, vf, ve);
   TEST_V3OP(4, "ab", v8, vc, vd);
   TEST_V3OP(4, "ab", va, vb, vc);
   TEST_V3OP(4, "ab", va, vc, vb);
   TEST_V3OP(4, "ab", vf, vf, v8);

   puts("vmalh");
   TEST_V3OP(3, "a9", v8, vc, vd);
   TEST_V3OP(3, "a9", va, vb, vc);
   TEST_V3OP(3, "a9", va, vc, vb);
   TEST_V3OP(3, "a9", vf, vf, ve);
   TEST_V3OP(4, "a9", v8, vc, vd);
   TEST_V3OP(4, "a9", va, vb, vc);
   TEST_V3OP(4, "a9", va, vc, vb);
   TEST_V3OP(4, "a9", vf, vf, v8);
}

/* -- Vector element compare (logical) -- */

#define TEST_VEC(m3, opc, a, b)                                                \
   {                                                                           \
      int cc;                                                                  \
                                                                               \
      __asm__("cr    0,0\n\t" /* Clear CC */                                   \
              ".insn vrr,0xe700000000" opc ",%1,%2,0," #m3 ",0,0\n\t"          \
              "ipm   %[cc]\n\t"                                                \
              "srl   %[cc],28\n"                                               \
              : [cc] "=d"(cc)                                                  \
              : "v"(a), "v"(b)                                                 \
              : "cc");                                                         \
      printf("\tcc=%d\n", cc);                                                 \
   }

static void test_all_vec(void)
{
   puts("vec");
   TEST_VEC(4, "db", vc, vc);
   TEST_VEC(4, "db", vd, v0);
   TEST_VEC(4, "db", ve, v0);
   TEST_VEC(4, "db", vb, ve);

   puts("vecl");
   TEST_VEC(4, "d9", vc, vc);
   TEST_VEC(4, "d9", vd, v0);
   TEST_VEC(4, "d9", ve, v0);
   TEST_VEC(4, "d9", vb, ve);
}

/* -- Run all tests -- */

int main(void)
{
   test_all_veval();
   test_all_vgem();
   test_all_v1op();
   test_all_v2op();
   test_all_v3op();
   test_all_vec();
   return 0;
}
