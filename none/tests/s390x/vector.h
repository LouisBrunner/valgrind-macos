#ifndef S390_TEST_VECTOR_H
#define S390_TEST_VECTOR_H

#include "stdbool.h"
#include "stdint.h"
#include "stdio.h"
#include "string.h"

/* How many times should every test be executed? */
#ifndef S390_TEST_COUNT
#define S390_TEST_COUNT 10
#endif

/* Test the instruction exactly one time. */
#define test_once(insn) test_##insn ()

/* Test the instruction exactly S390_TEST_COUNT times.
   "..." arguments specifies code which must be executed after each tests
 */
#define test(insn, ...) \
   for(iteration = 0; iteration < S390_TEST_COUNT; iteration++) \
     { test_once(insn); \
        __VA_ARGS__; \
     }

#define test_with_selective_printing(insn, info) \
   for(iteration = 0; iteration < S390_TEST_COUNT; iteration++) \
     { test_ ## insn ## _selective(info); }

#ifdef __GNUC__
/* GCC complains about __int128 with -pedantic */
/* Hope that we will have int128_t in C standard someday. */
#pragma GCC diagnostic ignored "-Wpedantic"
#endif

typedef union {
   uint8_t u8[16];
   int8_t s8[16];

   uint16_t u16[8];
   int16_t s16[8];

   uint32_t u32[4];
   int32_t s32[4];
   float   f32[4];

   uint64_t u64[2];
   int64_t s64[2];
   double f64[2];

   unsigned __int128 u128[1];
   __int128 s128[1];
} V128;

typedef enum {
   V128_NO_PRINTING = 0,
   V128_V_RES_AS_INT  = 1 << 0,
   V128_V_ARG1_AS_INT = 1 << 1,
   V128_V_ARG2_AS_INT = 1 << 2,
   V128_V_ARG3_AS_INT = 1 << 3,
   V128_V_RES_AS_FLOAT64 = 1 << 4,
   V128_V_ARG1_AS_FLOAT64 = 1 << 5,
   V128_V_ARG2_AS_FLOAT64 = 1 << 6,
   V128_V_ARG3_AS_FLOAT64 = 1 << 7,
   V128_V_RES_AS_FLOAT32 = 1 << 8,
   V128_V_ARG1_AS_FLOAT32 = 1 << 9,
   V128_V_ARG2_AS_FLOAT32 = 1 << 10,
   V128_V_ARG3_AS_FLOAT32 = 1 << 11,
   V128_R_RES = 1 << 12,
   V128_R_ARG1 = 1 << 13,
   V128_R_ARG2 = 1 << 14,
   V128_R_ARG3 = 1 << 15,
   V128_V_RES_EVEN_ONLY = 1 << 16,
   V128_V_RES_ZERO_ONLY = 1 << 17,
   V128_PRINT_ALL = (V128_V_RES_AS_INT |
                     V128_V_ARG1_AS_INT |
                     V128_V_ARG2_AS_INT |
                     V128_V_ARG3_AS_INT |
                     V128_R_RES |
                     V128_R_ARG1 |
                     V128_R_ARG2 |
                     V128_R_ARG3),
} s390x_test_usageInfo;

void print_hex(const V128 value) {
   printf("%016lx | %016lx\n", value.u64[0], value.u64[1]);
}

void print_f32(const V128 value, int even_only, int zero_only) {
   if (zero_only)
      printf("%a | -- | -- | --\n", value.f32[0]);
   else if (even_only)
      printf("%a | -- | %a | --\n", value.f32[0], value.f32[2]);
   else
      printf("%a | %a | %a | %a\n",
             value.f32[0], value.f32[1], value.f32[2], value.f32[3]);
}

void print_f64(const V128 value, int zero_only) {
   if (zero_only)
      printf("%a | --\n", value.f64[0]);
   else
      printf("%a | %a\n", value.f64[0], value.f64[1]);
}

void print_uint64_t(const uint64_t value) {
   printf("%016lx\n", value);
}

uint8_t random_element ( void )
{
   static uint32_t seed = 80021;
   seed = 1103515245 * seed + 12345;
   return (seed >> 17) & 0xFF;
}

void random_V128 (V128 *block)
{
   size_t i;
   for(i = 0; i < 16; i++)
   {
      block->u8[i] = random_element();
   }
}

uint64_t random_uint64_t()
{
   uint64_t result = 0ULL;
   uint8_t  *ptr = (uint8_t *) &result;
   size_t i;
   for(i = 0; i < 8; i++)
   {
      ptr[i] = random_element();
   }

   return result;
}

/* Memory pool with some random data. Used for some instruction which need
   an address to some memory chunk.
   Pool should be large enough for all insn that use it.
   (64 bytes and aligning are needed by VLBB insn)

   Content of this pool must be updated every iteration but not from test to test.
*/
uint8_t random_memory_pool[64] __attribute__ ((aligned (64)));
void randomize_memory_pool()
{
   size_t i;
   for(i = 0; i < sizeof(random_memory_pool) / sizeof(random_memory_pool[0]); i++)
   {
      random_memory_pool[i] = random_element();
   }
}

/* Define a test for input. Takes up theese arguments:
      insn        -- instruction name
      asm_string  -- line (or multiple lines) with asm mnemonics for instruction

   The folowing registers layout expected:
      ("r" for register form and m for memory form)
      v1         -- vector arg1
      v2         -- vector arg2
      v3         -- vector arg3
      v5         -- vector result
      [{r,m}_arg1]   -- integer arg1
      [{r,m}_arg2]   -- integer arg2
      [{r,m}_arg3]   -- integer arg3
      [{r,m}_result]      -- integer result
      [{r,m}_memory_pool] -- address of random memory pool. Usefull for some instructions

*/
#define s390_test_generate(insn, asm_string) \
static void test_##insn##_selective(const s390x_test_usageInfo info) \
{ \
   V128 v_result = { .u64 = {0ULL, 0ULL} }; \
   V128 v_arg1; \
   V128 v_arg2; \
   V128 v_arg3; \
   uint64_t r_arg1 = random_uint64_t(); \
   uint64_t r_arg2 = random_uint64_t(); \
   uint64_t r_arg3 = random_uint64_t(); \
   uint64_t r_result = 0ULL; \
    \
   random_V128(&v_arg1); \
   random_V128(&v_arg2); \
   random_V128(&v_arg3); \
    \
   __asm__ volatile( \
       "vl  %%v1, %[v_arg1]\n" \
       "vl  %%v2, %[v_arg2]\n" \
       "vl  %%v3, %[v_arg3]\n" \
       "vone %%v5\n" \
       "srnmb 1(0)\n " \
       asm_string "\n"\
       "vst %%v5, %[v_result]\n" \
       "vst %%v1, %[v_arg1]\n" \
       "vst %%v2, %[v_arg2]\n" \
       "vst %%v3, %[v_arg3]\n" \
       : [v_result]      "=m" (v_result), \
         [m_result]      "=m" (r_result), \
         [r_result]      "+d" (r_result), \
         [r_arg1]        "+d" (r_arg1), \
         [r_arg2]        "+d" (r_arg2), \
         [r_arg3]        "+d" (r_arg3) \
       : [v_arg1]        "m" (v_arg1), \
         [v_arg2]        "m" (v_arg2), \
         [v_arg3]        "m" (v_arg3), \
         [m_arg1]        "m" (r_arg1), \
         [m_arg2]        "m" (r_arg2), \
         [m_arg3]        "m" (r_arg3), \
         [r_memory_pool] "r" (random_memory_pool), \
         [m_memory_pool] "m" (random_memory_pool) \
       : "memory", "cc", \
         "r1", "r2", "r3", "r5", \
         "v1", "v2", "v3", "v5"); \
    \
   printf("insn %s:\n", #insn); \
   if (info & V128_V_ARG1_AS_INT) \
      {printf("  v_arg1   = "); print_hex(v_arg1);} \
   if (info & V128_V_ARG2_AS_INT) \
      {printf("  v_arg2   = "); print_hex(v_arg2);} \
   if (info & V128_V_ARG3_AS_INT) \
      {printf("  v_arg3   = "); print_hex(v_arg3);} \
   if (info & V128_V_RES_AS_INT) \
      {printf("  v_result = "); print_hex(v_result);} \
   \
   if (info & V128_V_ARG1_AS_FLOAT64) \
      {printf("  v_arg1   = "); print_f64(v_arg1, 0);} \
   if (info & V128_V_ARG2_AS_FLOAT64) \
      {printf("  v_arg2   = "); print_f64(v_arg2, 0);} \
   if (info & V128_V_ARG3_AS_FLOAT64) \
      {printf("  v_arg3   = "); print_f64(v_arg3, 0);} \
   if (info & V128_V_RES_AS_FLOAT64) { \
      printf("  v_result = "); \
      print_f64(v_result, info & V128_V_RES_ZERO_ONLY); \
   } \
   \
   if (info & V128_V_ARG1_AS_FLOAT32) \
      {printf("  v_arg1   = "); print_f32(v_arg1, 0, 0);} \
   if (info & V128_V_ARG2_AS_FLOAT32) \
      {printf("  v_arg2   = "); print_f32(v_arg2, 0, 0);} \
   if (info & V128_V_ARG3_AS_FLOAT32) \
      {printf("  v_arg3   = "); print_f32(v_arg3, 0, 0);} \
   if (info & V128_V_RES_AS_FLOAT32) { \
      printf("  v_result = "); \
      print_f32(v_result, info & V128_V_RES_EVEN_ONLY, \
                info & V128_V_RES_ZERO_ONLY); \
   } \
   if (info & V128_R_ARG1) \
      {printf("  r_arg1   = "); print_uint64_t(r_arg1);} \
   if (info & V128_R_ARG2) \
      {printf("  r_arg2   = "); print_uint64_t(r_arg2);} \
   if (info & V128_R_ARG3) \
      {printf("  r_arg3   = "); print_uint64_t(r_arg3);} \
   if (info & V128_R_RES) \
      {printf("  r_result = "); print_uint64_t(r_result);} \
} \
__attribute__((unused)) static void test_##insn() \
{ \
   test_##insn##_selective (V128_PRINT_ALL); \
}

/* Stores CC to %[r_result].
   Usefull when testing instructions which modify condition code.
*/
#define S390_TEST_PUT_CC_TO_RESULT "ipm %[r_result] \n srl %[r_result], 28 \n"

#endif
