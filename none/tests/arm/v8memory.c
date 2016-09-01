
/*
gcc -g -o v8memory_a -march=armv8-a -mfpu=crypto-neon-fp-armv8 \
   none/tests/arm/v8memory.c -I. -Wall -marm

gcc -g -o v8memory_t -march=armv8-a -mfpu=crypto-neon-fp-armv8 \
   none/tests/arm/v8memory.c -I. -Wall -mthumb   
*/

/* These tests unfortunately are unable to check the relative
   placement (or, even, presence) of the required memory fences
   relative to the store/load required.  They only verify the
   data-movement component. */

#include <stdio.h>
#include <malloc.h>  // memalign
#include <string.h>  // memset
#include "tests/malloc.h"
#include <assert.h>

typedef  unsigned char           UChar;
typedef  unsigned short int      UShort;
typedef  unsigned int            UInt;
typedef  signed int              Int;
typedef  unsigned char           UChar;
typedef  signed long long int    Long;
typedef  unsigned long long int  ULong;

typedef  unsigned char           Bool;
#define False ((Bool)0)
#define True  ((Bool)1)

static inline UChar randUChar ( void )
{
   static UInt seed = 90210; // Somewhere in Beverly Hills, allegedly.
   seed = 1103515245 * seed + 12345;
   return (seed >> 17) & 0xFF;
}

static UInt randUInt ( void )
{
   Int i;
   UInt r = 0;
   for (i = 0; i < 4; i++) {
      r = (r << 8) | (UInt)(0xFF & randUChar());
   }
   return r;
}

static void show_block_xor ( UChar* block1, UChar* block2, Int n )
{
   Int i;
   printf("  ");
   for (i = 0; i < n; i++) {
      if (i > 0 && 0 == (i & 15)) printf("\n  ");
      if (0 == (i & 15)) printf("[%3d]  ", i);
      UInt diff = 0xFF & (UInt)(block1[i] - block2[i]);
      if (diff == 0)
         printf(".. ");
      else
         printf("%02x ", diff);
   }
   printf("\n");
}


// INSN may mention the following regs as containing load/store data:
//      r2 r3 r6 r9
// INSN must mention the following reg as containing the EA: r10
//
// INSN can use r4 and r5 as scratch
//
// In: rand: memory area (128 bytes), r2, r3, r6, r9
//       r10 pointing to middle of memory area
//
// Out: memory area, r2, r3, r6, r9, r10
//
// What is printed out: the XOR of the new and old versions of the
// following:
//    the memory area
//    r2, r3 r6 r9 r10

#define MEM_TEST(INSN) { \
  int i; \
  const int N = 128; \
  UChar* area1 = memalign16(N); \
  UChar* area2 = memalign16(N); \
  for (i = 0; i < N; i++) area1[i] = area2[i] = randUChar(); \
  UInt block1[5]; \
  UInt block2[5]; \
  /* 0:r2    1:r3    2:r6    3:r9    4:r10 */ \
  for (i = 0; i < 5; i++) block1[i] = block2[i] = randUInt(); \
  block1[4] = block2[4] = (UInt)(&area1[N/2]); \
  __asm__ __volatile__( \
    "ldr r2,  [%0, #0]  ; " \
    "ldr r3,  [%0, #4]  ; " \
    "ldr r6,  [%0, #8]  ; " \
    "ldr r9,  [%0, #12] ; " \
    "ldr r10, [%0, #16] ; " \
    INSN " ; " \
    "str r2,  [%0, #0]  ; " \
    "str r3,  [%0, #4]  ; " \
    "str r6,  [%0, #8]  ; " \
    "str r9,  [%0, #12] ; " \
    "str r10, [%0, #16] ; " \
    : : "r"(&block1[0]) : "r2", "r3", "r4", "r5", "r6", "r9", "r10", \
        "memory", "cc" \
  ); \
  printf("%s  with  r10 = middle_of_block\n", INSN); \
  show_block_xor(&area1[0], &area2[0], N); \
  printf("  %08x  r2  (xor, data intreg #1)\n", block1[0] ^ block2[0]); \
  printf("  %08x  r3  (xor, data intreg #2)\n", block1[1] ^ block2[1]); \
  printf("  %08x  r6  (xor, data intreg #3)\n", block1[2] ^ block2[2]); \
  printf("  %08x  r9  (xor, data intreg #4)\n", block1[3] ^ block2[3]); \
  printf("  %08x  r10 (xor, addr intreg #1)\n", block1[4] ^ block2[4]); \
  printf("\n"); \
  free(area1); free(area2); \
  }


int main ( void )
{
   ////////////////////////////////////////////////////////////////
   printf("LDA{,B,H} (reg)\n\n");
   MEM_TEST("lda  r6, [r10]")
   MEM_TEST("ldab r9, [r10]")
   MEM_TEST("ldah r3, [r10]")

   ////////////////////////////////////////////////////////////////
   printf("STL{,B,H} (reg)\n\n");
   MEM_TEST("stl  r6, [r10]")
   MEM_TEST("stlb r9, [r10]")
   MEM_TEST("stlh r3, [r10]")

   ////////////////////////////////////////////////////////////////
   printf("LDAEX{,B,H,D} (reg)\n\n");
   MEM_TEST("ldaex  r6, [r10]")
   MEM_TEST("ldaexb r9, [r10]")
   MEM_TEST("ldaexh r3, [r10]")
   MEM_TEST("ldaexd r2, r3, [r10]")

   ////////////////////////////////////////////////////////////////
   // These verify that stlex* do notice a cleared (missing) reservation.
   printf("STLEX{,B,H,D} (reg) -- expected to fail\n\n");
   MEM_TEST("clrex; stlex  r9, r6, [r10]")
   MEM_TEST("clrex; stlexb r9, r6, [r10]")
   MEM_TEST("clrex; stlexh r9, r3, [r10]")
   MEM_TEST("clrex; stlexd r9, r2, r3, [r10]")

   ////////////////////////////////////////////////////////////////
   // These verify that stlex* do notice a successful reservation.
   // By using ldaex* to create the reservation in the first place,
   // they also verify that ldaex* actually create a reservation.
   printf("STLEX{,B,H,D} (reg) -- expected to succeed\n\n");
   MEM_TEST("ldaex  r2, [r10] ; stlex  r9, r6, [r10]")
   MEM_TEST("ldaexb r2, [r10] ; stlexb r9, r6, [r10]")
   MEM_TEST("ldaexh r2, [r10] ; stlexh r9, r3, [r10]")
   MEM_TEST("mov r4, r2 ; mov r5, r3 ; " // preserve r2/r3 around the ldrexd
            "ldaexd r2, r3, [r10] ; "
            "mov r2, r4 ; mov r3, r5 ; "
            "stlexd r9, r2, r3, [r10]")

   return 0;
}
