#include <stdio.h>
#include <assert.h>

typedef  unsigned char UChar;
typedef  unsigned int  UInt;
typedef  signed int    Int;

#define ITERS 1

union _V128 {
   UChar  u8[16];
};
typedef  union _V128   V128;

static inline UChar randUChar ( void )
{
   static UInt seed = 80021;
   seed = 1103515245 * seed + 12345;
   return (seed >> 17) & 0xFF;
}

/* Generates a random V128. */
static void randV128 ( /*OUT*/V128* v)
{
   static UInt nCalls = 0;
   Int i;
   nCalls++;
   for (i = 0; i < 16; i++) {
      v->u8[i] = randUChar();
   }
   if (0 == (nCalls & 0xFF))
      printf("randV128: %u calls\n", nCalls);
}

static void showV128 ( V128* v )
{
   Int i;
   for (i = 15; i >= 0; i--)
      printf("%02x", (Int)v->u8[i]);
}

#define GEN_BINARY_TEST_BODY(INSN,SUFFIXD,SUFFIXN,SUFFIXM) \
      Int i; \
      for (i = 0; i < ITERS; i++) { \
         V128 block[3]; \
         randV128(&block[0]); \
         randV128(&block[1]); \
         randV128(&block[2]); \
         __asm__ __volatile__( \
            "ldr q7, [%0, #0];" \
            "ldr q8, [%0, #16];" \
            "ldr q9, [%0, #32];" \
            #INSN " v9." #SUFFIXD ", v7." #SUFFIXN ", v8." SUFFIXM " ; " \
            "str q9, [%0, #32];" \
            : : "r"(&block[0]) : "memory", "v7", "v8", "v9" \
         ); \
         printf(#INSN " v9." #SUFFIXD \
                ", v7." #SUFFIXN ", v8." SUFFIXM " "); \
         showV128(&block[0]); printf(" "); \
         showV128(&block[1]); printf(" "); \
         showV128(&block[2]); printf("\n"); \
      } \

#define GEN_BINARY_TEST_BY_ELEM(INSN,SUFFIXD,SUFFIXN,MELEM) \
   __attribute__((noinline)) \
   static void test_##INSN##_##SUFFIXD##_##SUFFIXN##_elem_##MELEM () { \
      GEN_BINARY_TEST_BODY(INSN,SUFFIXD,SUFFIXN,"4b[" #MELEM "]") \
   }

#define GEN_BINARY_TEST(INSN,SUFFIXD,SUFFIXN,SUFFIXM) \
   __attribute__((noinline)) \
   static void test_##INSN##_##SUFFIXD##_##SUFFIXN##_##SUFFIXM () { \
      GEN_BINARY_TEST_BODY(INSN,SUFFIXD,SUFFIXN,#SUFFIXM) \
   }

GEN_BINARY_TEST(sdot, 2s, 8b, 8b)
GEN_BINARY_TEST(udot, 2s, 8b, 8b)
GEN_BINARY_TEST(sdot, 4s, 16b, 16b)
GEN_BINARY_TEST(udot, 4s, 16b, 16b)
GEN_BINARY_TEST_BY_ELEM(sdot, 2s, 8b, 0)
GEN_BINARY_TEST_BY_ELEM(udot, 2s, 8b, 1)
GEN_BINARY_TEST_BY_ELEM(sdot, 4s, 16b, 2)
GEN_BINARY_TEST_BY_ELEM(udot, 4s, 16b, 3)

int main ( void )
{
   assert(sizeof(V128) == 16);

   // ======================== {S,U}DOT by element ====================
   // sdot 2s,8b,4b[0]
   // udot 2s,8b,4b[1]
   // sdot 4s,16b,4b[2]
   // udot 4s,16b,4b[3]
   test_sdot_2s_8b_elem_0();
   test_udot_2s_8b_elem_1();
   test_sdot_4s_16b_elem_2();
   test_udot_4s_16b_elem_3();

   // ======================== {S,U}DOT vector ========================
   // sdot 2s,8b,8b
   // udot 2s,8b,8b
   // sdot 4s,16b,16b
   // udot 4s,16b,16b
   test_sdot_2s_8b_8b();
   test_udot_2s_8b_8b();
   test_sdot_4s_16b_16b();
   test_udot_4s_16b_16b();

   return 0;
}
