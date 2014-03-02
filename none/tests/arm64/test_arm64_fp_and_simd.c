
#include <stdio.h>
#include <assert.h>
#include <malloc.h>  // memalign
#include <string.h>  // memset
#include <math.h>    // isnormal

typedef  unsigned char           UChar;
typedef  unsigned short int      UShort;
typedef  unsigned int            UInt;
typedef  signed int              Int;
typedef  unsigned char           UChar;
typedef  unsigned long long int  ULong;

typedef  unsigned char           Bool;
#define False ((Bool)0)
#define True  ((Bool)1)


#define ITERS 1


union _V128 {
   UChar  u8[16];
   UShort u16[8];
   UInt   u32[4];
   ULong  u64[2];
   float  f32[4];
   double f64[2];
};
typedef  union _V128   V128;

static inline UChar randUChar ( void )
{
   static UInt seed = 80021;
   seed = 1103515245 * seed + 12345;
   return (seed >> 17) & 0xFF;
}

static ULong randULong ( void )
{
   Int i;
   ULong r = 0;
   for (i = 0; i < 8; i++) {
      r = (r << 8) | (ULong)(0xFF & randUChar());
   }
   return r;
}

/* Generates a random V128.  Ensures that that it contains normalised
   FP numbers when viewed as either F32x4 or F64x2, so that it is
   reasonable to use in FP test cases. */
static void randV128 ( V128* v )
{
   static UInt nCalls = 0, nIters = 0;
   Int i;
   nCalls++;
   while (1) {
      nIters++;
      for (i = 0; i < 16; i++) {
         v->u8[i] = randUChar();
      }
      if (isnormal(v->f32[0]) && isnormal(v->f32[1]) && isnormal(v->f32[2])
          && isnormal(v->f32[3]) && isnormal(v->f64[0]) && isnormal(v->f64[1]))
        break;
   }
   if (0 == (nCalls & 0xFF))
      printf("randV128: %u calls, %u iters\n", nCalls, nIters);
}

static void showV128 ( V128* v )
{
   Int i;
   for (i = 15; i >= 0; i--)
      printf("%02x", (Int)v->u8[i]);
}

__attribute__((unused))
static void* memalign16(size_t szB)
{
   void* x;
   x = memalign(16, szB);
   assert(x);
   assert(0 == ((16-1) & (unsigned long)x));
   return x;
}


void test_UMINV ( void )
{
  int i;
  V128 block[2];

  /* -- 4s -- */

  for (i = 0; i < 10; i++) {
    memset(&block, 0x55, sizeof(block));
    randV128(&block[0]);
    randV128(&block[1]);
    __asm__ __volatile__(
       "ldr   q7, [%0, #0]   ; "
       "uminv s8, v7.4s   ; "
       "str   q8, [%0, #16] "
       : : "r"(&block[0]) : "memory", "v7", "v8"
                         );
    printf("UMINV v8, v7.4s  ");
    showV128(&block[0]); printf("  ");
    showV128(&block[1]); printf("\n");
  }

  /* -- 8h -- */

  for (i = 0; i < 10; i++) {
    memset(&block, 0x55, sizeof(block));
    randV128(&block[0]);
    randV128(&block[1]);
    __asm__ __volatile__(
       "ldr   q7, [%0, #0]   ; "
       "uminv h8, v7.8h   ; "
       "str   q8, [%0, #16] "
       : : "r"(&block[0]) : "memory", "v7", "v8"
                         );
    printf("UMINV h8, v7.8h  ");
    showV128(&block[0]); printf("  ");
    showV128(&block[1]); printf("\n");
  }

  /* -- 4h -- */

  for (i = 0; i < 10; i++) {
    memset(&block, 0x55, sizeof(block));
    randV128(&block[0]);
    randV128(&block[1]);
    __asm__ __volatile__(
       "ldr   q7, [%0, #0]   ; "
       "uminv h8, v7.4h   ; "
       "str   q8, [%0, #16] "
       : : "r"(&block[0]) : "memory", "v7", "v8"
                         );
    printf("UMINV h8, v7.4h  ");
    showV128(&block[0]); printf("  ");
    showV128(&block[1]); printf("\n");
  }

  /* -- 16b -- */

  for (i = 0; i < 10; i++) {
    memset(&block, 0x55, sizeof(block));
    randV128(&block[0]);
    randV128(&block[1]);
    __asm__ __volatile__(
       "ldr   q7, [%0, #0]   ; "
       "uminv b8, v7.16b   ; "
       "str   q8, [%0, #16] "
       : : "r"(&block[0]) : "memory", "v7", "v8"
                         );
    printf("UMINV b8, v7.16b  ");
    showV128(&block[0]); printf("  ");
    showV128(&block[1]); printf("\n");
  }

  /* -- 8b -- */

  for (i = 0; i < 10; i++) {
    memset(&block, 0x55, sizeof(block));
    randV128(&block[0]);
    randV128(&block[1]);
    __asm__ __volatile__(
       "ldr   q7, [%0, #0]   ; "
       "uminv b8, v7.8b   ; "
       "str   q8, [%0, #16] "
       : : "r"(&block[0]) : "memory", "v7", "v8"
                         );
    printf("UMINV b8, v7.8b  ");
    showV128(&block[0]); printf("  ");
    showV128(&block[1]); printf("\n");
  }

}


void test_UMAXV ( void )
{
  int i;
  V128 block[2];

  /* -- 4s -- */

  for (i = 0; i < 10; i++) {
    memset(&block, 0x55, sizeof(block));
    randV128(&block[0]);
    randV128(&block[1]);
    __asm__ __volatile__(
       "ldr   q7, [%0, #0]   ; "
       "umaxv s8, v7.4s   ; "
       "str   q8, [%0, #16] "
       : : "r"(&block[0]) : "memory", "v7", "v8"
                         );
    printf("UMAXV v8, v7.4s  ");
    showV128(&block[0]); printf("  ");
    showV128(&block[1]); printf("\n");
  }

  /* -- 8h -- */

  for (i = 0; i < 10; i++) {
    memset(&block, 0x55, sizeof(block));
    randV128(&block[0]);
    randV128(&block[1]);
    __asm__ __volatile__(
       "ldr   q7, [%0, #0]   ; "
       "umaxv h8, v7.8h   ; "
       "str   q8, [%0, #16] "
       : : "r"(&block[0]) : "memory", "v7", "v8"
                         );
    printf("UMAXV h8, v7.8h  ");
    showV128(&block[0]); printf("  ");
    showV128(&block[1]); printf("\n");
  }

  /* -- 4h -- */

  for (i = 0; i < 10; i++) {
    memset(&block, 0x55, sizeof(block));
    randV128(&block[0]);
    randV128(&block[1]);
    __asm__ __volatile__(
       "ldr   q7, [%0, #0]   ; "
       "umaxv h8, v7.4h   ; "
       "str   q8, [%0, #16] "
       : : "r"(&block[0]) : "memory", "v7", "v8"
                         );
    printf("UMAXV h8, v7.4h  ");
    showV128(&block[0]); printf("  ");
    showV128(&block[1]); printf("\n");
  }

  /* -- 16b -- */

  for (i = 0; i < 10; i++) {
    memset(&block, 0x55, sizeof(block));
    randV128(&block[0]);
    randV128(&block[1]);
    __asm__ __volatile__(
       "ldr   q7, [%0, #0]   ; "
       "umaxv b8, v7.16b   ; "
       "str   q8, [%0, #16] "
       : : "r"(&block[0]) : "memory", "v7", "v8"
                         );
    printf("UMAXV b8, v7.16b  ");
    showV128(&block[0]); printf("  ");
    showV128(&block[1]); printf("\n");
  }

  /* -- 8b -- */

  for (i = 0; i < 10; i++) {
    memset(&block, 0x55, sizeof(block));
    randV128(&block[0]);
    randV128(&block[1]);
    __asm__ __volatile__(
       "ldr   q7, [%0, #0]   ; "
       "umaxv b8, v7.8b   ; "
       "str   q8, [%0, #16] "
       : : "r"(&block[0]) : "memory", "v7", "v8"
                         );
    printf("UMAXV b8, v7.8b  ");
    showV128(&block[0]); printf("  ");
    showV128(&block[1]); printf("\n");
  }

}


void test_INS_general ( void )
{
  V128 block[3];

  /* -- D[0..1] -- */

  memset(&block, 0x55, sizeof(block));
  block[1].u64[0] = randULong();
  __asm__ __volatile__(
     "ldr q7, [%0, #0]   ; "
     "ldr x19, [%0, #16] ; "
     "ins v7.d[0], x19   ; "
     "str q7, [%0, #32] "
     : : "r"(&block[0]) : "memory", "x19", "v7"
  );
  printf("INS v7.u64[0],x19  ");
  showV128(&block[0]); printf("  %016llx  ", block[1].u64[0]);
  showV128(&block[2]); printf("\n");

  memset(&block, 0x55, sizeof(block));
  block[1].u64[0] = randULong();
  __asm__ __volatile__(
     "ldr q7, [%0, #0]   ; "
     "ldr x19, [%0, #16] ; "
     "ins v7.d[1], x19   ; "
     "str q7, [%0, #32] "
     : : "r"(&block[0]) : "memory", "x19", "v7"
  );
  printf("INS v7.d[1],x19  ");
  showV128(&block[0]); printf("  %016llx  ", block[1].u64[0]);
  showV128(&block[2]); printf("\n");

  /* -- S[0..3] -- */

  memset(&block, 0x55, sizeof(block));
  block[1].u64[0] = randULong();
  __asm__ __volatile__(
     "ldr q7, [%0, #0]   ; "
     "ldr x19, [%0, #16] ; "
     "ins v7.s[0], w19   ; "
     "str q7, [%0, #32] "
     : : "r"(&block[0]) : "memory", "x19", "v7"
  );
  printf("INS v7.s[0],x19  ");
  showV128(&block[0]); printf("  %016llx  ", block[1].u64[0]);
  showV128(&block[2]); printf("\n");

  memset(&block, 0x55, sizeof(block));
  block[1].u64[0] = randULong();
  __asm__ __volatile__(
     "ldr q7, [%0, #0]   ; "
     "ldr x19, [%0, #16] ; "
     "ins v7.s[1], w19   ; "
     "str q7, [%0, #32] "
     : : "r"(&block[0]) : "memory", "x19", "v7"
  );
  printf("INS v7.s[1],x19  ");
  showV128(&block[0]); printf("  %016llx  ", block[1].u64[0]);
  showV128(&block[2]); printf("\n");

  memset(&block, 0x55, sizeof(block));
  block[1].u64[0] = randULong();
  __asm__ __volatile__(
     "ldr q7, [%0, #0]   ; "
     "ldr x19, [%0, #16] ; "
     "ins v7.s[2], w19   ; "
     "str q7, [%0, #32] "
     : : "r"(&block[0]) : "memory", "x19", "v7"
  );
  printf("INS v7.s[2],x19  ");
  showV128(&block[0]); printf("  %016llx  ", block[1].u64[0]);
  showV128(&block[2]); printf("\n");

  memset(&block, 0x55, sizeof(block));
  block[1].u64[0] = randULong();
  __asm__ __volatile__(
     "ldr q7, [%0, #0]   ; "
     "ldr x19, [%0, #16] ; "
     "ins v7.s[3], w19   ; "
     "str q7, [%0, #32] "
     : : "r"(&block[0]) : "memory", "x19", "v7"
  );
  printf("INS v7.s[3],x19  ");
  showV128(&block[0]); printf("  %016llx  ", block[1].u64[0]);
  showV128(&block[2]); printf("\n");

  /* -- H[0..7] -- */

  memset(&block, 0x55, sizeof(block));
  block[1].u64[0] = randULong();
  __asm__ __volatile__(
     "ldr q7, [%0, #0]   ; "
     "ldr x19, [%0, #16] ; "
     "ins v7.h[0], w19   ; "
     "str q7, [%0, #32] "
     : : "r"(&block[0]) : "memory", "x19", "v7"
  );
  printf("INS v7.h[0],x19  ");
  showV128(&block[0]); printf("  %016llx  ", block[1].u64[0]);
  showV128(&block[2]); printf("\n");

  memset(&block, 0x55, sizeof(block));
  block[1].u64[0] = randULong();
  __asm__ __volatile__(
     "ldr q7, [%0, #0]   ; "
     "ldr x19, [%0, #16] ; "
     "ins v7.h[1], w19   ; "
     "str q7, [%0, #32] "
     : : "r"(&block[0]) : "memory", "x19", "v7"
  );
  printf("INS v7.h[1],x19  ");
  showV128(&block[0]); printf("  %016llx  ", block[1].u64[0]);
  showV128(&block[2]); printf("\n");

  memset(&block, 0x55, sizeof(block));
  block[1].u64[0] = randULong();
  __asm__ __volatile__(
     "ldr q7, [%0, #0]   ; "
     "ldr x19, [%0, #16] ; "
     "ins v7.h[2], w19   ; "
     "str q7, [%0, #32] "
     : : "r"(&block[0]) : "memory", "x19", "v7"
  );
  printf("INS v7.h[2],x19  ");
  showV128(&block[0]); printf("  %016llx  ", block[1].u64[0]);
  showV128(&block[2]); printf("\n");

  memset(&block, 0x55, sizeof(block));
  block[1].u64[0] = randULong();
  __asm__ __volatile__(
     "ldr q7, [%0, #0]   ; "
     "ldr x19, [%0, #16] ; "
     "ins v7.h[3], w19   ; "
     "str q7, [%0, #32] "
     : : "r"(&block[0]) : "memory", "x19", "v7"
  );
  printf("INS v7.h[3],x19  ");
  showV128(&block[0]); printf("  %016llx  ", block[1].u64[0]);
  showV128(&block[2]); printf("\n");

  memset(&block, 0x55, sizeof(block));
  block[1].u64[0] = randULong();
  __asm__ __volatile__(
     "ldr q7, [%0, #0]   ; "
     "ldr x19, [%0, #16] ; "
     "ins v7.h[4], w19   ; "
     "str q7, [%0, #32] "
     : : "r"(&block[0]) : "memory", "x19", "v7"
  );
  printf("INS v7.h[4],x19  ");
  showV128(&block[0]); printf("  %016llx  ", block[1].u64[0]);
  showV128(&block[2]); printf("\n");

  memset(&block, 0x55, sizeof(block));
  block[1].u64[0] = randULong();
  __asm__ __volatile__(
     "ldr q7, [%0, #0]   ; "
     "ldr x19, [%0, #16] ; "
     "ins v7.h[5], w19   ; "
     "str q7, [%0, #32] "
     : : "r"(&block[0]) : "memory", "x19", "v7"
  );
  printf("INS v7.h[5],x19  ");
  showV128(&block[0]); printf("  %016llx  ", block[1].u64[0]);
  showV128(&block[2]); printf("\n");

  memset(&block, 0x55, sizeof(block));
  block[1].u64[0] = randULong();
  __asm__ __volatile__(
     "ldr q7, [%0, #0]   ; "
     "ldr x19, [%0, #16] ; "
     "ins v7.h[6], w19   ; "
     "str q7, [%0, #32] "
     : : "r"(&block[0]) : "memory", "x19", "v7"
  );
  printf("INS v7.h[6],x19  ");
  showV128(&block[0]); printf("  %016llx  ", block[1].u64[0]);
  showV128(&block[2]); printf("\n");

  memset(&block, 0x55, sizeof(block));
  block[1].u64[0] = randULong();
  __asm__ __volatile__(
     "ldr q7, [%0, #0]   ; "
     "ldr x19, [%0, #16] ; "
     "ins v7.h[7], w19   ; "
     "str q7, [%0, #32] "
     : : "r"(&block[0]) : "memory", "x19", "v7"
  );
  printf("INS v7.h[7],x19  ");
  showV128(&block[0]); printf("  %016llx  ", block[1].u64[0]);
  showV128(&block[2]); printf("\n");

  /* -- B[0,15] -- */

  memset(&block, 0x55, sizeof(block));
  block[1].u64[0] = randULong();
  __asm__ __volatile__(
     "ldr q7, [%0, #0]   ; "
     "ldr x19, [%0, #16] ; "
     "ins v7.b[0], w19   ; "
     "str q7, [%0, #32] "
     : : "r"(&block[0]) : "memory", "x19", "v7"
  );
  printf("INS v7.b[0],x19  ");
  showV128(&block[0]); printf("  %016llx  ", block[1].u64[0]);
  showV128(&block[2]); printf("\n");

  memset(&block, 0x55, sizeof(block));
  block[1].u64[0] = randULong();
  __asm__ __volatile__(
     "ldr q7, [%0, #0]   ; "
     "ldr x19, [%0, #16] ; "
     "ins v7.b[15], w19   ; "
     "str q7, [%0, #32] "
     : : "r"(&block[0]) : "memory", "x19", "v7"
  );
  printf("INS v7.b[15],x19 ");
  showV128(&block[0]); printf("  %016llx  ", block[1].u64[0]);
  showV128(&block[2]); printf("\n");
}



void test_SMINV ( void )
{
  int i;
  V128 block[2];

  /* -- 4s -- */

  for (i = 0; i < 10; i++) {
    memset(&block, 0x55, sizeof(block));
    randV128(&block[0]);
    randV128(&block[1]);
    __asm__ __volatile__(
       "ldr   q7, [%0, #0]   ; "
       "sminv s8, v7.4s   ; "
       "str   q8, [%0, #16] "
       : : "r"(&block[0]) : "memory", "v7", "v8"
                         );
    printf("SMINV v8, v7.4s  ");
    showV128(&block[0]); printf("  ");
    showV128(&block[1]); printf("\n");
  }

  /* -- 8h -- */

  for (i = 0; i < 10; i++) {
    memset(&block, 0x55, sizeof(block));
    randV128(&block[0]);
    randV128(&block[1]);
    __asm__ __volatile__(
       "ldr   q7, [%0, #0]   ; "
       "sminv h8, v7.8h   ; "
       "str   q8, [%0, #16] "
       : : "r"(&block[0]) : "memory", "v7", "v8"
                         );
    printf("SMINV h8, v7.8h  ");
    showV128(&block[0]); printf("  ");
    showV128(&block[1]); printf("\n");
  }

  /* -- 4h -- */

  for (i = 0; i < 10; i++) {
    memset(&block, 0x55, sizeof(block));
    randV128(&block[0]);
    randV128(&block[1]);
    __asm__ __volatile__(
       "ldr   q7, [%0, #0]   ; "
       "sminv h8, v7.4h   ; "
       "str   q8, [%0, #16] "
       : : "r"(&block[0]) : "memory", "v7", "v8"
                         );
    printf("SMINV h8, v7.4h  ");
    showV128(&block[0]); printf("  ");
    showV128(&block[1]); printf("\n");
  }

  /* -- 16b -- */

  for (i = 0; i < 10; i++) {
    memset(&block, 0x55, sizeof(block));
    randV128(&block[0]);
    randV128(&block[1]);
    __asm__ __volatile__(
       "ldr   q7, [%0, #0]   ; "
       "sminv b8, v7.16b   ; "
       "str   q8, [%0, #16] "
       : : "r"(&block[0]) : "memory", "v7", "v8"
                         );
    printf("SMINV b8, v7.16b  ");
    showV128(&block[0]); printf("  ");
    showV128(&block[1]); printf("\n");
  }

  /* -- 8b -- */

  for (i = 0; i < 10; i++) {
    memset(&block, 0x55, sizeof(block));
    randV128(&block[0]);
    randV128(&block[1]);
    __asm__ __volatile__(
       "ldr   q7, [%0, #0]   ; "
       "sminv b8, v7.8b   ; "
       "str   q8, [%0, #16] "
       : : "r"(&block[0]) : "memory", "v7", "v8"
                         );
    printf("SMINV b8, v7.8b  ");
    showV128(&block[0]); printf("  ");
    showV128(&block[1]); printf("\n");
  }

}


void test_SMAXV ( void )
{
  int i;
  V128 block[2];

  /* -- 4s -- */

  for (i = 0; i < 10; i++) {
    memset(&block, 0x55, sizeof(block));
    randV128(&block[0]);
    randV128(&block[1]);
    __asm__ __volatile__(
       "ldr   q7, [%0, #0]   ; "
       "smaxv s8, v7.4s   ; "
       "str   q8, [%0, #16] "
       : : "r"(&block[0]) : "memory", "v7", "v8"
                         );
    printf("SMAXV v8, v7.4s  ");
    showV128(&block[0]); printf("  ");
    showV128(&block[1]); printf("\n");
  }

  /* -- 8h -- */

  for (i = 0; i < 10; i++) {
    memset(&block, 0x55, sizeof(block));
    randV128(&block[0]);
    randV128(&block[1]);
    __asm__ __volatile__(
       "ldr   q7, [%0, #0]   ; "
       "smaxv h8, v7.8h   ; "
       "str   q8, [%0, #16] "
       : : "r"(&block[0]) : "memory", "v7", "v8"
                         );
    printf("SMAXV h8, v7.8h  ");
    showV128(&block[0]); printf("  ");
    showV128(&block[1]); printf("\n");
  }

  /* -- 4h -- */

  for (i = 0; i < 10; i++) {
    memset(&block, 0x55, sizeof(block));
    randV128(&block[0]);
    randV128(&block[1]);
    __asm__ __volatile__(
       "ldr   q7, [%0, #0]   ; "
       "smaxv h8, v7.4h   ; "
       "str   q8, [%0, #16] "
       : : "r"(&block[0]) : "memory", "v7", "v8"
                         );
    printf("SMAXV h8, v7.4h  ");
    showV128(&block[0]); printf("  ");
    showV128(&block[1]); printf("\n");
  }

  /* -- 16b -- */

  for (i = 0; i < 10; i++) {
    memset(&block, 0x55, sizeof(block));
    randV128(&block[0]);
    randV128(&block[1]);
    __asm__ __volatile__(
       "ldr   q7, [%0, #0]   ; "
       "smaxv b8, v7.16b   ; "
       "str   q8, [%0, #16] "
       : : "r"(&block[0]) : "memory", "v7", "v8"
                         );
    printf("SMAXV b8, v7.16b  ");
    showV128(&block[0]); printf("  ");
    showV128(&block[1]); printf("\n");
  }

  /* -- 8b -- */

  for (i = 0; i < 10; i++) {
    memset(&block, 0x55, sizeof(block));
    randV128(&block[0]);
    randV128(&block[1]);
    __asm__ __volatile__(
       "ldr   q7, [%0, #0]   ; "
       "smaxv b8, v7.8b   ; "
       "str   q8, [%0, #16] "
       : : "r"(&block[0]) : "memory", "v7", "v8"
                         );
    printf("SMAXV b8, v7.8b  ");
    showV128(&block[0]); printf("  ");
    showV128(&block[1]); printf("\n");
  }

}

/* Note this also sets the destination register to a known value (0x55..55)
   since it can sometimes be an input to the instruction too. */
#define GEN_BINARY_TEST(INSN,SUFFIX) \
  __attribute__((noinline)) \
  static void test_##INSN##_##SUFFIX ( void ) { \
     Int i; \
     for (i = 0; i < ITERS; i++) { \
        V128 block[3]; \
        memset(block, 0x55, sizeof(block)); \
        randV128(&block[0]); \
        randV128(&block[1]); \
        randV128(&block[2]); \
        __asm__ __volatile__( \
           "ldr   q7, [%0, #0]   ; " \
           "ldr   q8, [%0, #16]   ; " \
           "ldr   q9, [%0, #32]   ; " \
           #INSN " v9." #SUFFIX ", v7." #SUFFIX ", v8." #SUFFIX " ; " \
           "str   q9, [%0, #32] " \
           : : "r"(&block[0]) : "memory", "v7", "v8", "v9" \
        ); \
        printf(#INSN   " v9." #SUFFIX ", v7." #SUFFIX ", v8." #SUFFIX "  "); \
        showV128(&block[0]); printf("  "); \
        showV128(&block[1]); printf("  "); \
        showV128(&block[2]); printf("\n"); \
     } \
  }


/* Note this also sets the destination register to a known value (0x55..55)
   since it can sometimes be an input to the instruction too. */
#define GEN_SHIFT_TEST(INSN,SUFFIXD,SUFFIXN,AMOUNT) \
  __attribute__((noinline)) \
  static void test_##INSN##_##SUFFIXD##_##SUFFIXN##_##AMOUNT ( void ) { \
     Int i; \
     for (i = 0; i < ITERS; i++) { \
        V128 block[2]; \
        memset(block, 0x55, sizeof(block)); \
        randV128(&block[0]); \
        randV128(&block[1]); \
        __asm__ __volatile__( \
           "ldr   q7, [%0, #0]   ; " \
           "ldr   q8, [%0, #16]   ; " \
           #INSN " v8." #SUFFIXD ", v7." #SUFFIXN ", #" #AMOUNT " ; " \
           "str   q8, [%0, #16] " \
           : : "r"(&block[0]) : "memory", "v7", "v8" \
        ); \
        printf(#INSN   " v8." #SUFFIXD ", v7." #SUFFIXN ", #" #AMOUNT "  "); \
        showV128(&block[0]); printf("  "); \
        showV128(&block[1]); printf("\n"); \
     } \
  }

/* Note this also sets the destination register to a known value (0x55..55)
   since it can sometimes be an input to the instruction too. */
#define GEN_UNARY_TEST(INSN,SUFFIXD,SUFFIXN) \
  __attribute__((noinline)) \
  static void test_##INSN##_##SUFFIXD##_##SUFFIXN ( void ) { \
     Int i; \
     for (i = 0; i < ITERS; i++) { \
        V128 block[2]; \
        memset(block, 0x55, sizeof(block)); \
        randV128(&block[0]); \
        randV128(&block[1]); \
        __asm__ __volatile__( \
           "ldr   q7, [%0, #0]   ; " \
           "ldr   q8, [%0, #16]   ; " \
           #INSN " v8." #SUFFIXD ", v7." #SUFFIXN " ; " \
           "str   q8, [%0, #16] " \
           : : "r"(&block[0]) : "memory", "v7", "v8" \
        ); \
        printf(#INSN   " v8." #SUFFIXD ", v7." #SUFFIXN); \
        showV128(&block[0]); printf("  "); \
        showV128(&block[1]); printf("\n"); \
     } \
  }


GEN_BINARY_TEST(umax, 4s)
GEN_BINARY_TEST(umax, 8h)
GEN_BINARY_TEST(umax, 4h)
GEN_BINARY_TEST(umax, 16b)
GEN_BINARY_TEST(umax, 8b)

GEN_BINARY_TEST(umin, 4s)
GEN_BINARY_TEST(umin, 8h)
GEN_BINARY_TEST(umin, 4h)
GEN_BINARY_TEST(umin, 16b)
GEN_BINARY_TEST(umin, 8b)

GEN_BINARY_TEST(smax, 4s)
GEN_BINARY_TEST(smax, 8h)
GEN_BINARY_TEST(smax, 4h)
GEN_BINARY_TEST(smax, 16b)
GEN_BINARY_TEST(smax, 8b)

GEN_BINARY_TEST(smin, 4s)
GEN_BINARY_TEST(smin, 8h)
GEN_BINARY_TEST(smin, 4h)
GEN_BINARY_TEST(smin, 16b)
GEN_BINARY_TEST(smin, 8b)

GEN_BINARY_TEST(add, 2d)
GEN_BINARY_TEST(add, 4s)
GEN_BINARY_TEST(add, 2s)
GEN_BINARY_TEST(add, 8h)
GEN_BINARY_TEST(add, 4h)
GEN_BINARY_TEST(add, 16b)
GEN_BINARY_TEST(add, 8b)

GEN_BINARY_TEST(sub, 2d)
GEN_BINARY_TEST(sub, 4s)
GEN_BINARY_TEST(sub, 2s)
GEN_BINARY_TEST(sub, 8h)
GEN_BINARY_TEST(sub, 4h)
GEN_BINARY_TEST(sub, 16b)
GEN_BINARY_TEST(sub, 8b)

GEN_BINARY_TEST(mul, 4s)
GEN_BINARY_TEST(mul, 2s)
GEN_BINARY_TEST(mul, 8h)
GEN_BINARY_TEST(mul, 4h)
GEN_BINARY_TEST(mul, 16b)
GEN_BINARY_TEST(mul, 8b)

GEN_BINARY_TEST(mla, 4s)
GEN_BINARY_TEST(mla, 2s)
GEN_BINARY_TEST(mla, 8h)
GEN_BINARY_TEST(mla, 4h)
GEN_BINARY_TEST(mla, 16b)
GEN_BINARY_TEST(mla, 8b)

GEN_BINARY_TEST(mls, 4s)
GEN_BINARY_TEST(mls, 2s)
GEN_BINARY_TEST(mls, 8h)
GEN_BINARY_TEST(mls, 4h)
GEN_BINARY_TEST(mls, 16b)
GEN_BINARY_TEST(mls, 8b)

GEN_BINARY_TEST(and, 16b)
GEN_BINARY_TEST(and, 8b)

GEN_BINARY_TEST(bic, 16b)
GEN_BINARY_TEST(bic, 8b)

GEN_BINARY_TEST(orr, 16b)
GEN_BINARY_TEST(orr, 8b)

GEN_BINARY_TEST(orn, 16b)
GEN_BINARY_TEST(orn, 8b)

GEN_BINARY_TEST(eor, 16b)
GEN_BINARY_TEST(eor, 8b)

GEN_BINARY_TEST(bsl, 16b)
GEN_BINARY_TEST(bsl, 8b)

GEN_BINARY_TEST(bit, 16b)
GEN_BINARY_TEST(bit, 8b)

GEN_BINARY_TEST(bif, 16b)
GEN_BINARY_TEST(bif, 8b)

GEN_BINARY_TEST(cmeq, 2d)
GEN_BINARY_TEST(cmeq, 4s)
GEN_BINARY_TEST(cmeq, 2s)
GEN_BINARY_TEST(cmeq, 8h)
GEN_BINARY_TEST(cmeq, 4h)
GEN_BINARY_TEST(cmeq, 16b)
GEN_BINARY_TEST(cmeq, 8b)

GEN_BINARY_TEST(cmtst, 2d)
GEN_BINARY_TEST(cmtst, 4s)
GEN_BINARY_TEST(cmtst, 2s)
GEN_BINARY_TEST(cmtst, 8h)
GEN_BINARY_TEST(cmtst, 4h)
GEN_BINARY_TEST(cmtst, 16b)
GEN_BINARY_TEST(cmtst, 8b)

GEN_BINARY_TEST(cmhi, 2d)
GEN_BINARY_TEST(cmhi, 4s)
GEN_BINARY_TEST(cmhi, 2s)
GEN_BINARY_TEST(cmhi, 8h)
GEN_BINARY_TEST(cmhi, 4h)
GEN_BINARY_TEST(cmhi, 16b)
GEN_BINARY_TEST(cmhi, 8b)

GEN_BINARY_TEST(cmgt, 2d)
GEN_BINARY_TEST(cmgt, 4s)
GEN_BINARY_TEST(cmgt, 2s)
GEN_BINARY_TEST(cmgt, 8h)
GEN_BINARY_TEST(cmgt, 4h)
GEN_BINARY_TEST(cmgt, 16b)
GEN_BINARY_TEST(cmgt, 8b)

GEN_BINARY_TEST(cmhs, 2d)
GEN_BINARY_TEST(cmhs, 4s)
GEN_BINARY_TEST(cmhs, 2s)
GEN_BINARY_TEST(cmhs, 8h)
GEN_BINARY_TEST(cmhs, 4h)
GEN_BINARY_TEST(cmhs, 16b)
GEN_BINARY_TEST(cmhs, 8b)

GEN_BINARY_TEST(cmge, 2d)
GEN_BINARY_TEST(cmge, 4s)
GEN_BINARY_TEST(cmge, 2s)
GEN_BINARY_TEST(cmge, 8h)
GEN_BINARY_TEST(cmge, 4h)
GEN_BINARY_TEST(cmge, 16b)
GEN_BINARY_TEST(cmge, 8b)

GEN_SHIFT_TEST(ushr, 2d, 2d, 1)
GEN_SHIFT_TEST(ushr, 2d, 2d, 13)
GEN_SHIFT_TEST(ushr, 2d, 2d, 63)
GEN_SHIFT_TEST(sshr, 2d, 2d, 1)
GEN_SHIFT_TEST(sshr, 2d, 2d, 13)
GEN_SHIFT_TEST(sshr, 2d, 2d, 63)
GEN_SHIFT_TEST(shl,  2d, 2d, 1)
GEN_SHIFT_TEST(shl,  2d, 2d, 13)
GEN_SHIFT_TEST(shl,  2d, 2d, 63)

GEN_SHIFT_TEST(ushr, 4s, 4s, 1)
GEN_SHIFT_TEST(ushr, 4s, 4s, 13)
GEN_SHIFT_TEST(ushr, 4s, 4s, 31)
GEN_SHIFT_TEST(sshr, 4s, 4s, 1)
GEN_SHIFT_TEST(sshr, 4s, 4s, 13)
GEN_SHIFT_TEST(sshr, 4s, 4s, 31)
GEN_SHIFT_TEST(shl,  4s, 4s, 1)
GEN_SHIFT_TEST(shl,  4s, 4s, 13)
GEN_SHIFT_TEST(shl,  4s, 4s, 31)

GEN_SHIFT_TEST(ushr, 2s, 2s, 1)
GEN_SHIFT_TEST(ushr, 2s, 2s, 13)
GEN_SHIFT_TEST(ushr, 2s, 2s, 31)
GEN_SHIFT_TEST(sshr, 2s, 2s, 1)
GEN_SHIFT_TEST(sshr, 2s, 2s, 13)
GEN_SHIFT_TEST(sshr, 2s, 2s, 31)
GEN_SHIFT_TEST(shl,  2s, 2s, 1)
GEN_SHIFT_TEST(shl,  2s, 2s, 13)
GEN_SHIFT_TEST(shl,  2s, 2s, 31)

GEN_SHIFT_TEST(ushr, 8h, 8h, 1)
GEN_SHIFT_TEST(ushr, 8h, 8h, 13)
GEN_SHIFT_TEST(ushr, 8h, 8h, 15)
GEN_SHIFT_TEST(sshr, 8h, 8h, 1)
GEN_SHIFT_TEST(sshr, 8h, 8h, 13)
GEN_SHIFT_TEST(sshr, 8h, 8h, 15)
GEN_SHIFT_TEST(shl,  8h, 8h, 1)
GEN_SHIFT_TEST(shl,  8h, 8h, 13)
GEN_SHIFT_TEST(shl,  8h, 8h, 15)

GEN_SHIFT_TEST(ushr, 4h, 4h, 1)
GEN_SHIFT_TEST(ushr, 4h, 4h, 13)
GEN_SHIFT_TEST(ushr, 4h, 4h, 15)
GEN_SHIFT_TEST(sshr, 4h, 4h, 1)
GEN_SHIFT_TEST(sshr, 4h, 4h, 13)
GEN_SHIFT_TEST(sshr, 4h, 4h, 15)
GEN_SHIFT_TEST(shl,  4h, 4h, 1)
GEN_SHIFT_TEST(shl,  4h, 4h, 13)
GEN_SHIFT_TEST(shl,  4h, 4h, 15)

GEN_SHIFT_TEST(ushr, 16b, 16b, 1)
GEN_SHIFT_TEST(ushr, 16b, 16b, 7)
GEN_SHIFT_TEST(sshr, 16b, 16b, 1)
GEN_SHIFT_TEST(sshr, 16b, 16b, 7)
GEN_SHIFT_TEST(shl,  16b, 16b, 1)
GEN_SHIFT_TEST(shl,  16b, 16b, 7)

GEN_SHIFT_TEST(ushr, 8b, 8b, 1)
GEN_SHIFT_TEST(ushr, 8b, 8b, 7)
GEN_SHIFT_TEST(sshr, 8b, 8b, 1)
GEN_SHIFT_TEST(sshr, 8b, 8b, 7)
GEN_SHIFT_TEST(shl,  8b, 8b, 1)
GEN_SHIFT_TEST(shl,  8b, 8b, 7)

GEN_SHIFT_TEST(ushll,  2d, 2s, 0)
GEN_SHIFT_TEST(ushll,  2d, 2s, 15)
GEN_SHIFT_TEST(ushll,  2d, 2s, 31)
GEN_SHIFT_TEST(ushll2, 2d, 4s, 0)
GEN_SHIFT_TEST(ushll2, 2d, 4s, 15)
GEN_SHIFT_TEST(ushll2, 2d, 4s, 31)

GEN_SHIFT_TEST(sshll,  2d, 2s, 0)
GEN_SHIFT_TEST(sshll,  2d, 2s, 15)
GEN_SHIFT_TEST(sshll,  2d, 2s, 31)
GEN_SHIFT_TEST(sshll2, 2d, 4s, 0)
GEN_SHIFT_TEST(sshll2, 2d, 4s, 15)
GEN_SHIFT_TEST(sshll2, 2d, 4s, 31)

GEN_UNARY_TEST(xtn,  2s, 2d)
GEN_UNARY_TEST(xtn2, 4s, 2d)
GEN_UNARY_TEST(xtn,  4h, 4s)
GEN_UNARY_TEST(xtn2, 8h, 4s)


/* Generate a test that involves one integer reg and one vector reg,
   with no bias as towards which is input or output. */
#define GEN_ONEINT_ONEVEC_TEST(TESTNAME,INSN,INTREGNO,VECREGNO) \
  __attribute__((noinline)) \
  static void test_##TESTNAME ( void ) { \
     Int i; \
     for (i = 0; i < ITERS; i++) { \
        V128 block[4]; \
        memset(block, 0x55, sizeof(block)); \
        randV128(&block[0]); \
        randV128(&block[1]); \
        randV128(&block[2]); \
        randV128(&block[3]); \
        __asm__ __volatile__( \
           "ldr   q"#VECREGNO", [%0, #0]  ; " \
           "ldr   x"#INTREGNO", [%0, #16] ; " \
           INSN " ; " \
           "str   q"#VECREGNO", [%0, #32] ; " \
           "str   x"#INTREGNO", [%0, #48] ; " \
           : : "r"(&block[0]) : "memory", "v"#VECREGNO, "x"#INTREGNO \
        ); \
        printf(INSN   "   "); \
        showV128(&block[0]); printf("  "); \
        showV128(&block[1]); printf("  "); \
        showV128(&block[2]); printf("  "); \
        showV128(&block[3]); printf("\n"); \
     } \
  }

GEN_ONEINT_ONEVEC_TEST(umov_01, "umov x9, v10.d[0]", 9, 10)
GEN_ONEINT_ONEVEC_TEST(umov_02, "umov x9, v10.d[1]", 9, 10)
GEN_ONEINT_ONEVEC_TEST(umov_03, "umov w9, v10.s[0]", 9, 10)
GEN_ONEINT_ONEVEC_TEST(umov_04, "umov w9, v10.s[3]", 9, 10)
GEN_ONEINT_ONEVEC_TEST(umov_05, "umov w9, v10.h[0]", 9, 10)
GEN_ONEINT_ONEVEC_TEST(umov_06, "umov w9, v10.h[7]", 9, 10)
GEN_ONEINT_ONEVEC_TEST(umov_07, "umov w9, v10.b[0]", 9, 10)
GEN_ONEINT_ONEVEC_TEST(umov_08, "umov w9, v10.b[15]", 9, 10)

GEN_ONEINT_ONEVEC_TEST(smov_01, "smov x9, v10.s[0]", 9, 10)
GEN_ONEINT_ONEVEC_TEST(smov_02, "smov x9, v10.s[3]", 9, 10)

GEN_ONEINT_ONEVEC_TEST(smov_03, "smov x9, v10.h[0]", 9, 10)
GEN_ONEINT_ONEVEC_TEST(smov_04, "smov x9, v10.h[7]", 9, 10)
GEN_ONEINT_ONEVEC_TEST(smov_05, "smov w9, v10.h[0]", 9, 10)
GEN_ONEINT_ONEVEC_TEST(smov_06, "smov w9, v10.h[7]", 9, 10)

GEN_ONEINT_ONEVEC_TEST(smov_07, "smov x9, v10.b[0]", 9, 10)
GEN_ONEINT_ONEVEC_TEST(smov_08, "smov x9, v10.b[15]", 9, 10)
GEN_ONEINT_ONEVEC_TEST(smov_09, "smov w9, v10.b[0]", 9, 10)
GEN_ONEINT_ONEVEC_TEST(smov_10, "smov w9, v10.b[15]", 9, 10)

/* Generate a test that involves two vector regs,
   with no bias as towards which is input or output. */
#define GEN_TWOVEC_TEST(TESTNAME,INSN,VECREG1NO,VECREG2NO) \
  __attribute__((noinline)) \
  static void test_##TESTNAME ( void ) { \
     Int i; \
     for (i = 0; i < ITERS; i++) { \
        V128 block[4]; \
        memset(block, 0x55, sizeof(block)); \
        randV128(&block[0]); \
        randV128(&block[1]); \
        randV128(&block[2]); \
        randV128(&block[3]); \
        __asm__ __volatile__( \
           "ldr   q"#VECREG1NO", [%0, #0]  ; " \
           "ldr   q"#VECREG2NO", [%0, #16] ; " \
           INSN " ; " \
           "str   q"#VECREG1NO", [%0, #32] ; " \
           "str   q"#VECREG2NO", [%0, #48] ; " \
           : : "r"(&block[0]) : "memory", "v"#VECREG1NO, "v"#VECREG2NO \
        ); \
        printf(INSN   "   "); \
        showV128(&block[0]); printf("  "); \
        showV128(&block[1]); printf("  "); \
        showV128(&block[2]); printf("  "); \
        showV128(&block[3]); printf("\n"); \
     } \
  }

GEN_TWOVEC_TEST(fcvtn_01, "fcvtn  v22.2s, v23.2d", 22, 23)
GEN_TWOVEC_TEST(fcvtn_02, "fcvtn2 v22.4s, v23.2d", 22, 23)

GEN_UNARY_TEST(neg, 2d, 2d)
GEN_UNARY_TEST(neg, 4s, 4s)
GEN_UNARY_TEST(neg, 2s, 2s)
GEN_UNARY_TEST(neg, 8h, 8h)
GEN_UNARY_TEST(neg, 4h, 4h)
GEN_UNARY_TEST(neg, 16b, 16b)
GEN_UNARY_TEST(neg, 8b,  8b)
GEN_BINARY_TEST(fadd, 2d)
GEN_BINARY_TEST(fadd, 4s)
GEN_BINARY_TEST(fadd, 2s)
GEN_BINARY_TEST(fsub, 2d)
GEN_BINARY_TEST(fsub, 4s)
GEN_BINARY_TEST(fsub, 2s)
GEN_BINARY_TEST(fmul, 2d)
GEN_BINARY_TEST(fmul, 4s)
GEN_BINARY_TEST(fmul, 2s)
GEN_BINARY_TEST(fdiv, 2d)
GEN_BINARY_TEST(fdiv, 4s)
GEN_BINARY_TEST(fdiv, 2s)
GEN_BINARY_TEST(fmla, 2d)
GEN_BINARY_TEST(fmla, 4s)
GEN_BINARY_TEST(fmla, 2s)
GEN_BINARY_TEST(fmls, 2d)
GEN_BINARY_TEST(fmls, 4s)
GEN_BINARY_TEST(fmls, 2s)
GEN_BINARY_TEST(fabd, 2d)
GEN_BINARY_TEST(fabd, 4s)
GEN_BINARY_TEST(fabd, 2s)

/* Generate a test that involves three vector regs,
   with no bias as towards which is input or output. */
#define GEN_THREEVEC_TEST(TESTNAME,INSN,VECREG1NO,VECREG2NO,VECREG3NO)  \
  __attribute__((noinline)) \
  static void test_##TESTNAME ( void ) { \
     Int i; \
     for (i = 0; i < ITERS; i++) { \
        V128 block[6]; \
        memset(block, 0x55, sizeof(block)); \
        randV128(&block[0]); \
        randV128(&block[1]); \
        randV128(&block[2]); \
        randV128(&block[3]); \
        randV128(&block[4]); \
        randV128(&block[5]); \
        __asm__ __volatile__( \
           "ldr   q"#VECREG1NO", [%0, #0]  ; " \
           "ldr   q"#VECREG2NO", [%0, #16] ; " \
           "ldr   q"#VECREG3NO", [%0, #32] ; " \
           INSN " ; " \
           "str   q"#VECREG1NO", [%0, #48] ; " \
           "str   q"#VECREG2NO", [%0, #64] ; " \
           "str   q"#VECREG3NO", [%0, #80] ; " \
           : : "r"(&block[0]) : "memory", "v"#VECREG1NO, "v"#VECREG2NO, "v"#VECREG3NO \
        ); \
        printf(INSN   "   "); \
        showV128(&block[0]); printf("  "); \
        showV128(&block[1]); printf("  "); \
        showV128(&block[2]); printf("  "); \
        showV128(&block[3]); printf("  "); \
        showV128(&block[4]); printf("  "); \
        showV128(&block[5]); printf("\n"); \
     } \
  }

GEN_THREEVEC_TEST(add_d_d_d, "add d21, d22, d23", 21, 22, 23)
GEN_THREEVEC_TEST(sub_d_d_d, "sub d21, d22, d23", 21, 22, 23)

/* overkill -- don't need two vecs, only one */
GEN_TWOVEC_TEST(fmov_scalar_imm_01, "fmov d22, #0.125", 22, 23)
GEN_TWOVEC_TEST(fmov_scalar_imm_02, "fmov d22, #-4.0",  22, 23)
GEN_TWOVEC_TEST(fmov_scalar_imm_03, "fmov d22, #1.0",   22, 23)
GEN_TWOVEC_TEST(fmov_scalar_imm_04, "fmov s22, #0.125", 22, 23)
GEN_TWOVEC_TEST(fmov_scalar_imm_05, "fmov s22, #-4.0",  22, 23)
GEN_TWOVEC_TEST(fmov_scalar_imm_06, "fmov s22, #-1.0",   22, 23)

GEN_ONEINT_ONEVEC_TEST(fmov_gen_01, "fmov s7,      w15", 15, 7)
GEN_ONEINT_ONEVEC_TEST(fmov_gen_02, "fmov d7,      x15", 15, 7)
GEN_ONEINT_ONEVEC_TEST(fmov_gen_03, "fmov v7.d[1], x15", 15, 7)
GEN_ONEINT_ONEVEC_TEST(fmov_gen_04, "fmov w15,      s7", 15, 7)
GEN_ONEINT_ONEVEC_TEST(fmov_gen_05, "fmov x15,      d7", 15, 7)
GEN_ONEINT_ONEVEC_TEST(fmov_gen_06, "fmov x15, v7.d[1]", 15, 7)

GEN_TWOVEC_TEST(movi_vector_imm_01, "fmov d22,    #0.125", 22, 23)
GEN_TWOVEC_TEST(movi_vector_imm_02, "fmov d22,    #-4.0",  22, 23)
GEN_TWOVEC_TEST(movi_vector_imm_03, "fmov d22,    #1.0",   22, 23)
GEN_TWOVEC_TEST(movi_vector_imm_04, "fmov v22.2d, #0.125", 22, 23)
GEN_TWOVEC_TEST(movi_vector_imm_05, "fmov v22.2d, #-4.0",  22, 23)
GEN_TWOVEC_TEST(movi_vector_imm_06, "fmov v22.2d, #1.0",   22, 23)

GEN_ONEINT_ONEVEC_TEST(sucvtf_01, "scvtf s7, w15", 15, 7)
GEN_ONEINT_ONEVEC_TEST(sucvtf_02, "scvtf d7, w15", 15, 7)
GEN_ONEINT_ONEVEC_TEST(sucvtf_03, "scvtf s7, x15", 15, 7)
GEN_ONEINT_ONEVEC_TEST(sucvtf_04, "scvtf d7, x15", 15, 7)
GEN_ONEINT_ONEVEC_TEST(sucvtf_05, "ucvtf s7, w15", 15, 7)
GEN_ONEINT_ONEVEC_TEST(sucvtf_06, "ucvtf d7, w15", 15, 7)
GEN_ONEINT_ONEVEC_TEST(sucvtf_07, "ucvtf s7, x15", 15, 7)
GEN_ONEINT_ONEVEC_TEST(sucvtf_08, "ucvtf d7, x15", 15, 7)

GEN_THREEVEC_TEST(fadd_d,  "fadd d2, d11, d29", 2, 11, 29)
GEN_THREEVEC_TEST(fadd_s,  "fadd s2, s11, s29", 2, 11, 29)
GEN_THREEVEC_TEST(fsub_d,  "fsub d2, d11, d29", 2, 11, 29)
GEN_THREEVEC_TEST(fsub_s,  "fsub s2, s11, s29", 2, 11, 29)
GEN_THREEVEC_TEST(fmul_d,  "fmul d2, d11, d29", 2, 11, 29)
GEN_THREEVEC_TEST(fmul_s,  "fmul s2, s11, s29", 2, 11, 29)
GEN_THREEVEC_TEST(fdiv_d,  "fdiv d2, d11, d29", 2, 11, 29)
GEN_THREEVEC_TEST(fdiv_s,  "fdiv s2, s11, s29", 2, 11, 29)
GEN_THREEVEC_TEST(fnmul_d, "fnmul d2, d11, d29", 2, 11, 29)
GEN_THREEVEC_TEST(fnmul_s, "fnmul s2, s11, s29", 2, 11, 29)

GEN_THREEVEC_TEST(fabd_d,  "fabd d2, d11, d29", 2, 11, 29)
GEN_THREEVEC_TEST(fabd_s,  "fabd s2, s11, s29", 2, 11, 29)

GEN_TWOVEC_TEST(fmov_d,  "fmov d22, d23",   22, 23)
GEN_TWOVEC_TEST(fmov_s,  "fmov s22, s23",   22, 23)
GEN_TWOVEC_TEST(fabs_d,  "fabs d22, d23",   22, 23)
GEN_TWOVEC_TEST(fabs_s,  "fabs s22, s23",   22, 23)
GEN_TWOVEC_TEST(fneg_d,  "fneg d22, d23",   22, 23)
GEN_TWOVEC_TEST(fneg_s,  "fneg s22, s23",   22, 23)
GEN_TWOVEC_TEST(fsqrt_d, "fsqrt d22, d23",   22, 23)
GEN_TWOVEC_TEST(fsqrt_s, "fsqrt s22, s23",   22, 23)

GEN_UNARY_TEST(fneg, 2d, 2d)
GEN_UNARY_TEST(fneg, 4s, 4s)
GEN_UNARY_TEST(fneg, 2s, 2s)
GEN_UNARY_TEST(fabs, 2d, 2d)
GEN_UNARY_TEST(fabs, 4s, 4s)
GEN_UNARY_TEST(fabs, 2s, 2s)

/* IMPORTANT: keep the tests in here in the same order as the
   implementations are in guest_arm64_toIR.c. */
int main ( void )
{
   assert(sizeof(V128) == 16);

   printf("BEGIN: FMOV (general)\n");
   test_fmov_gen_01();
   test_fmov_gen_02();
   test_fmov_gen_03();
   test_fmov_gen_04();
   test_fmov_gen_05();
   test_fmov_gen_06();
   printf("END:   FMOV (general)\n\n");

   printf("BEGIN: FMOV (scalar, immediate)\n");
   test_fmov_scalar_imm_01();
   test_fmov_scalar_imm_02();
   test_fmov_scalar_imm_03();
   test_fmov_scalar_imm_04();
   test_fmov_scalar_imm_05();
   test_fmov_scalar_imm_06();
   printf("END:   FMOV (scalar, immediate)\n\n");

   printf("BEGIN: {FMOV,MOVI} (vector, immediate)\n");
   test_movi_vector_imm_01();
   test_movi_vector_imm_02();
   test_movi_vector_imm_03();
   test_movi_vector_imm_04();
   test_movi_vector_imm_05();
   test_movi_vector_imm_06();
   printf("END:   {FMOV,MOVI} (vector, immediate)\n\n");

   printf("BEGIN: {S,U}CVTF (scalar, integer)\n");
   test_sucvtf_01();
   test_sucvtf_02();
   test_sucvtf_03();
   test_sucvtf_04();
   //test_sucvtf_05();
   test_sucvtf_06();
   test_sucvtf_07();
   test_sucvtf_08();
   printf("END:   {S,U}CVTF (scalar, integer) (MISSING 1 case of 8)\n\n");

   printf("BEGIN: F{ADD,SUB,MUL,DIV,NMUL} (scalar)\n");
   test_fadd_d();
   test_fadd_s();
   test_fsub_d();
   test_fsub_s();
   test_fmul_d();
   test_fmul_s();
   test_fdiv_d();
   test_fdiv_s();
   test_fnmul_d();
   test_fnmul_s();
   printf("END:   F{ADD,SUB,MUL,DIV,NMUL} (scalar)\n\n");

   printf("BEGIN: F{MOV,ABS,NEG,SQRT} D/D or S/S\n");
   test_fmov_d();
   test_fmov_s();
   test_fabs_d();
   test_fabs_s();
   test_fneg_d();
   test_fneg_s();
   test_fsqrt_d();
   test_fsqrt_s();
   printf("END:   F{MOV,ABS,NEG,SQRT} D/D or S/S\n\n");

   printf("BEGIN: F{ABS,NEG} (vector)\n");
   test_fabs_2d_2d();
   //test_fabs_4s_4s();
   //test_fabs_2s_2s();
   test_fneg_2d_2d();
   //test_fneg_4s_4s();
   //test_fneg_2s_2s();
   printf("END:   F{ABS,NEG} (vector) (MISSING 4s/2s cases)\n\n");

   printf("FCMP,FCMPE MISSING\n\n");

   printf("F{N}M{ADD,SUB} MISSING\n\n");

   printf("FCVT{N,P,M,Z}{S,U} (scalar, integer) MISSING\n\n");

   printf("FRINT{I,M,P,Z} (scalar) MISSING\n\n");

   printf("FCVT (scalar) MISSING\n\n");

   printf("BEGIN: FABD (scalar) MISSING\n");
   test_fabd_d();
   test_fabd_s();
   printf("END:   FABD (scalar) MISSING\n\n");

   printf("{S,U}CVTF (vector, integer) MISSING\n\n");

   printf("BEGIN: F{ADD,SUB,MUL,DIV,MLA,MLS,ABD} (vector)\n");
   test_fadd_2d();
   test_fadd_4s();
   test_fadd_2s();
   test_fsub_2d();
   test_fsub_4s();
   test_fsub_2s();
   test_fmul_2d();
   test_fmul_4s();
   test_fmul_2s();
   test_fdiv_2d();
   test_fdiv_4s();
   test_fdiv_2s();
   test_fmla_2d();
   test_fmla_4s();
   test_fmla_2s();
   test_fmls_2d();
   test_fmls_4s();
   test_fmls_2s();
   test_fabd_2d();
   //test_fabd_4s();
   //test_fabd_2s();
   printf("END:   F{ADD,SUB,MUL,DIV,MLA,MLS,ABD} (vector) (MISSING fabd 2s/4s)\n\n");

   printf("BEGIN: FCVTN (MISSING 16F <- 32F cases)\n");
   test_fcvtn_01();
   test_fcvtn_02();
   printf("END:   FCVTN (MISSING 16F <- 32F cases)\n\n");

   printf("BEGIN: ADD/SUB (vector)\n");
   test_add_2d();
   test_add_4s();
   test_add_2s();
   test_add_8h();
   test_add_4h();
   //test_add_16b();
   //test_add_8b();
   test_sub_2d();
   test_sub_4s();
   test_sub_2s();
   test_sub_8h();
   test_sub_4h();
   //test_sub_16b();
   //test_sub_8b();
   printf("END:   ADD/SUB (vector) (MISSING b16/b8 cases)\n\n");

   printf("BEGIN: ADD/SUB (scalar)\n");
   test_add_d_d_d();
   test_sub_d_d_d();
   printf("END:   ADD/SUB (scalar)\n\n");

   printf("BEGIN: MUL/PMUL/MLA/MLS (vector)\n");
   test_mul_4s();
   test_mul_2s();
   test_mul_8h();
   test_mul_4h();
   //test_mul_16b();
   //test_mul_8b();
   test_mla_4s();
   test_mla_2s();
   test_mla_8h();
   test_mla_4h();
   //test_mla_16b();
   //test_mla_8b();
   test_mls_4s();
   test_mls_2s();
   test_mls_8h();
   test_mls_4h();
   //test_mls_16b();
   //test_mls_8b();
   printf("END:   MUL/PMUL/MLA/MLS (vector) (partly MISSING)\n\n");

   printf("BEGIN: {S,U}{MIN,MAX} (vector)\n");
   test_umax_4s();
   test_umax_8h();
   test_umax_4h();
   test_umax_16b();
   test_umax_8b();
   test_umin_4s();
   test_umin_8h();
   test_umin_4h();
   test_umin_16b();
   test_umin_8b();
   test_smax_4s();
   test_smax_8h();
   test_smax_4h();
   test_smax_16b();
   test_smax_8b();
   test_smin_4s();
   test_smin_8h();
   test_smin_4h();
   test_smin_16b();
   test_smin_8b();
   printf("END:   {S,U}{MIN,MAX} (vector)\n\n");

   printf("BEGIN: {S,U}{MIN,MAX}V\n");
   test_UMINV();
   test_UMAXV();
   test_SMINV();
   test_SMAXV();
   printf("END:   {S,U}{MIN,MAX}V\n\n");

   printf("BEGIN: {AND,BIC,ORR,ORN} (vector)\n");
   test_and_16b();
   test_and_8b();
   test_bic_16b();
   test_bic_8b();
   test_orr_16b();
   test_orr_8b();
   test_orn_16b();
   test_orn_8b();
   printf("END:   {AND,BIC,ORR,ORN} (vector)\n\n");

   printf("BEGIN: CM{EQ,HI,HS,GE,GT,TST,LE,LT} (vector)\n\n");
   test_cmeq_2d();
#if 0
   test_cmeq_4s();
   test_cmeq_2s();
   test_cmeq_8h();
   test_cmeq_4h();
   test_cmeq_16b();
   test_cmeq_8b();
   test_cmtst_2d();
   test_cmtst_4s();
   test_cmtst_2s();
   test_cmtst_8h();
   test_cmtst_4h();
   test_cmtst_16b();
   test_cmtst_8b();
   test_cmhi_2d();
   test_cmhi_4s();
   test_cmhi_2s();
   test_cmhi_8h();
   test_cmhi_4h();
   test_cmhi_16b();
   test_cmhi_8b();
   test_cmgt_2d();
   test_cmgt_4s();
   test_cmgt_2s();
   test_cmgt_8h();
   test_cmgt_4h();
   test_cmgt_16b();
   test_cmgt_8b();
   test_cmhs_2d();
   test_cmhs_4s();
   test_cmhs_2s();
   test_cmhs_8h();
   test_cmhs_4h();
   test_cmhs_16b();
   test_cmhs_8b();
   test_cmge_2d();
   test_cmge_4s();
   test_cmge_2s();
   test_cmge_8h();
   test_cmge_4h();
   test_cmge_16b();
   test_cmge_8b();
#endif
   printf("END:   CM{EQ,HI,HS,GE,GT,TST,LE,LT} (vector) "
          "(w/zero and many other cases MISSING)\n\n");

   printf("BEGIN: {EOR,BSL,BIT,BIF} (vector)\n");
   test_eor_16b();
   test_eor_8b();
   test_bsl_16b();
   test_bsl_8b();
   test_bit_16b();
   test_bit_8b();
   test_bif_16b();
   test_bif_8b();
   printf("END:   {EOR,BSL,BIT,BIF} (vector)\n\n");

   printf("BEGIN: {USHR,SSHR,SHL} (vector, immediate)\n");
   test_ushr_2d_2d_1();
   test_ushr_2d_2d_13();
   test_ushr_2d_2d_63();
   test_sshr_2d_2d_1();
   test_sshr_2d_2d_13();
   test_sshr_2d_2d_63();
#if 0
   test_shl_2d_2d_1();
   test_shl_2d_2d_13();
   test_shl_2d_2d_63();

   test_ushr_4s_4s_1();
   test_ushr_4s_4s_13();
   test_ushr_4s_4s_31();
   test_sshr_4s_4s_1();
   test_sshr_4s_4s_13();
   test_sshr_4s_4s_31();
#endif
   test_shl_4s_4s_1();
   test_shl_4s_4s_13();
   test_shl_4s_4s_31();
#if 0
   test_ushr_2s_2s_1();
   test_ushr_2s_2s_13();
   test_ushr_2s_2s_31();
   test_sshr_2s_2s_1();
   test_sshr_2s_2s_13();
   test_sshr_2s_2s_31();
   test_shl_2s_2s_1();
   test_shl_2s_2s_13();
   test_shl_2s_2s_31();

   test_ushr_8h_8h_1();
   test_ushr_8h_8h_13();
   test_ushr_8h_8h_15();
   test_sshr_8h_8h_1();
   test_sshr_8h_8h_13();
   test_sshr_8h_8h_15();
   test_shl_8h_8h_1();
   test_shl_8h_8h_13();
   test_shl_8h_8h_15();

   test_ushr_4h_4h_1();
   test_ushr_4h_4h_13();
   test_ushr_4h_4h_15();
   test_sshr_4h_4h_1();
   test_sshr_4h_4h_13();
   test_sshr_4h_4h_15();
   test_shl_4h_4h_1();
   test_shl_4h_4h_13();
   test_shl_4h_4h_15();

   test_ushr_16b_16b_1();
   test_ushr_16b_16b_7();
   test_sshr_16b_16b_1();
   test_sshr_16b_16b_7();
   test_shl_16b_16b_1();
   test_shl_16b_16b_7();

   test_ushr_8b_8b_1();
   test_ushr_8b_8b_7();
   test_sshr_8b_8b_1();
   test_sshr_8b_8b_7();
   test_shl_8b_8b_1();
   test_shl_8b_8b_7();
#endif
   printf("END:   {USHR,SSHR,SHL} (vector, immediate) (many cases MISSING)\n\n");

   printf("BEGIN: {U,S}SHLL{,2}\n");
   test_ushll_2d_2s_0();
   test_ushll_2d_2s_15();
   test_ushll_2d_2s_31();
   test_ushll2_2d_4s_0();
   test_ushll2_2d_4s_15();
   test_ushll2_2d_4s_31();
   test_sshll_2d_2s_0();
   test_sshll_2d_2s_15();
   test_sshll_2d_2s_31();
   test_sshll2_2d_4s_0();
   test_sshll2_2d_4s_15();
   test_sshll2_2d_4s_31();
   printf("END:   {U,S}SHLL{,2} (MISSING h_b and s_h versions)\n\n");

   printf("BEGIN: XTN{,2}\n");
   test_xtn_2s_2d();
   test_xtn2_4s_2d();
   test_xtn_4h_4s();
   test_xtn2_8h_4s();
   printf("END:   XTN{,2} (MISSING b_h versions)\n\n");

   printf("DUP (element, vector) COMPLETELY MISSING\n\n");

   printf("DUP (general, vector) COMPLETELY MISSING\n\n");

   printf("BEGIN: {S,U}MOV\n");
   test_umov_01();
   test_umov_02();
   test_umov_03();
   test_umov_04();
   test_umov_05();
   test_umov_06();
   test_umov_07();
   test_umov_08();
   test_smov_01();
   test_smov_02();
   test_smov_03();
   test_smov_04();
   test_smov_05();
   test_smov_06();
   test_smov_07();
   test_smov_08();
   test_smov_09();
   test_smov_10();
   printf("END:   {S,U}MOV\n\n");

   printf("BEGIN: INS (general)\n");
   test_INS_general();
   printf("END:   INS (general)\n\n");

   printf("BEGIN: NEG (vector)\n");
   test_neg_2d_2d();
   test_neg_4s_4s();
   test_neg_2s_2s();
   test_neg_8h_8h();
   test_neg_4h_4h();
   //test_neg_16b_16b();
   //test_neg_8b_8b();
   printf("END:   NEG (vector) (MISSING 8b/16b)\n\n");

   return 0;
}
