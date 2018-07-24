#include "stdio.h"
#include "stdbool.h"
#include "stdint.h"
#include "string.h"
#include "vector.h"

#define S390_PPNO_SHA512_BLOCK_SIZE (240 / 8)
static uint64_t block[S390_PPNO_SHA512_BLOCK_SIZE];
#define HASH_COUNT 2 /* exactly two hashes for generate */
static uint64_t hash[HASH_COUNT];

static void print_sha512_block()
{
   printf("Current block state:\n");
   for(size_t elem = 0; elem < S390_PPNO_SHA512_BLOCK_SIZE; elem++)
   {
      print_uint64_t(block[elem]);
   }

   printf("end of block.\n");
}

static void print_sha512_hash()
{
   printf("Current hash:\n");
   for(size_t index = 0; index < HASH_COUNT; index++)
      print_uint64_t(hash[index]);

   printf("end of hash\n");
}

/* The problem with this test is different results on different architectures.
   E.g. z13 will return (1 << 0)|(1 << 3) value while more recent versions of CPU
   can (and will!) add their own operations here. Anyway z13 or any later arch will
   have "SHA-512-DRNG" (3-rd bit of result) and "Query" (0-th bit) functions
   so we test only this bits and ignore others.
*/
static bool test_ppno_query()
{
   uint32_t output[4];
   register uint64_t functionCode __asm__("0") = 0ULL;
   register uint64_t paramBlock __asm__("1") = (uint64_t) &output;

   __asm__ volatile (
         ".insn rre, 0xb93c0000, %%r2, %%r4 \n" // GPR's are ignored here
         : "+d"(functionCode), "+d"(paramBlock)
         :
         : "cc", "memory"
      );

   /* 0x9 = (1 << 0)|(1 << 3), see explanation above */
   uint32_t expected = 0x90000000U;
   uint32_t after = output[0];

   bool result = expected == after;
   if(!result)
      printf("ERROR: basic ppno functions are not supported : %d\n", after);

   return result;
}

static void test_ppno_sha512_seed()
{
   /* Important! The program should zero paramBlock for seeding. */
   memset(block, 0, sizeof(block));

   uint8_t seed[512];
   for(size_t index = 0; index < 512; index++)
      seed[index] = random_element();
   seed[511] = 0;
   const uint64_t seedLength = strlen((char*)seed);

   register uint64_t functionCode __asm__("0") = 0x83ULL;
   register uint64_t paramBlock __asm__("1") = (uint64_t) &block;
   register uint64_t gpr2 __asm__("2") = (uint64_t) &seed;
   register uint64_t gpr3 __asm__("3") = seedLength;
   __asm__ volatile (
         ".insn rre, 0xb93c0000, %%r4, %%r2 \n"
         :
         : "d"(functionCode), "d"(paramBlock), "d"(gpr2), "d"(gpr3)
         : "cc", "memory"
      );

   printf("seedLength = %ld\n", seedLength);
   print_sha512_block();
}

static void test_ppno_sha512_gen()
{
   register uint64_t functionCode __asm__("0") = 0x3ULL;
   register uint64_t paramBlock __asm__("1") = (uint64_t) &block;
   register uint64_t gpr2 __asm__("2") = (uint64_t) &hash;
   register uint64_t gpr3 __asm__("3") = HASH_COUNT;

   __asm__ volatile (
         "0: .insn rre, 0xb93c0000, %%r2, %%r4 \n"
         "   brc 1, 0 \n" /* handle partial completion */
         :
         : "d"(functionCode), "d"(paramBlock), "d"(gpr2), "d"(gpr3)
         : "cc", "memory"
      );

   print_sha512_hash();
   print_sha512_block();
}

static void test_ppno_sha512()
{
   printf(" === sha512_seed: ===\n");
   test_ppno_sha512_seed();
   return;
   printf(" === sha512_gen: ===\n");
   test_ppno_sha512_gen();

   memset(hash, 0, sizeof(hash));
}

int main()
{
   size_t iteration;
   if(!test_ppno_query())
      return 1;

   test(ppno_sha512);

   return 0;
}
