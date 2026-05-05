#include <stdio.h>

#define VECTOR __attribute__((vector_size(16)))

typedef unsigned long VECTOR ulong_v;
typedef float VECTOR         float_v;

static const ulong_v v0 = {0x0000000000000000, 0x0000000000000000};
static const ulong_v v8 = {0x8000000000000000, 0x0000000000000000};
static const ulong_v v2 = {0x0123456780000000, 0x0321456700333112};
static const ulong_v v3 = {0x012345678abcdef0, 0x8000000000000000};
static const ulong_v va = {0x7777777700000000, 0xffffffff00000000};
static const ulong_v vb = {0xab6b5205d99cbdc7, 0x3ba178cee0c63063};
static const ulong_v vc = {0x00000023ffffffff, 0x000000050000000b};
static const ulong_v vd = {0x0000000000000000, 0x0000000000001234};
static const ulong_v ve = {0x0000000000123456, 0xffffffffffffffff};
static const ulong_v vf = {0xffffffffffffffff, 0xffffffffffffffff};

/* -- Various vector insns with two input operands -- */

#define TEST_DIVOP(m4, opc, a, b, m5)                                          \
   {                                                                           \
      ulong_v out;                                                             \
      __asm__(".insn vrr,0xe700000000" opc ",%0,%1,%2," #m4 "," #m5 ",0"       \
              : [out] "=v"(out)                                                \
              : "v"(a), "v"(b)                                                 \
              :);                                                              \
      printf("\t%016lx %016lx\n", out[0], out[1]);                             \
   }

#define TEST_MULTIPLE(name, opc)                                               \
   puts(name);                                                                 \
   TEST_DIVOP(2, opc, vb, v0, 8);                                              \
   TEST_DIVOP(2, opc, va, vb, 0);                                              \
   TEST_DIVOP(2, opc, vb, va, 8);                                              \
   TEST_DIVOP(2, opc, vb, vc, 0);                                              \
   TEST_DIVOP(2, opc, v2, vc, 8);                                              \
   putchar('\n');                                                              \
   TEST_DIVOP(3, opc, vb, vc, 0);                                              \
   TEST_DIVOP(3, opc, vb, vd, 8);                                              \
   TEST_DIVOP(3, opc, v3, ve, 8);                                              \
   putchar('\n');                                                              \
   TEST_DIVOP(4, opc, vb, v0, 8);                                              \
   TEST_DIVOP(4, opc, vb, vd, 0);                                              \
   TEST_DIVOP(4, opc, v8, vf, 8);                                              \
   TEST_DIVOP(4, opc, vc, vf, 0);

/* -- Run all tests -- */

int main(void)
{
   TEST_MULTIPLE("vd", "b2");
   putchar('\n');
   TEST_MULTIPLE("vr", "b3");
   putchar('\n');
   TEST_MULTIPLE("vdl", "b0");
   putchar('\n');
   TEST_MULTIPLE("vrl", "b1");
   return 0;
}
