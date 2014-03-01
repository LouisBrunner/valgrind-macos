
#include <stdio.h>
#include <assert.h>
#include <malloc.h>  // memalign
#include <string.h>  // memset

typedef  unsigned char           UChar;
typedef  unsigned short int      UShort;
typedef  unsigned int            UInt;
typedef  signed int              Int;
typedef  unsigned char           UChar;
typedef  unsigned long long int  ULong;

typedef  unsigned char           Bool;
#define False ((Bool)0)
#define True  ((Bool)1)


union _V128 {
   UChar  b[16];
   UShort h[8];
   UInt   i[4];
   ULong  d[2];
};
typedef  union _V128   V128;

static UChar randUChar ( void )
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

static void randV128 ( V128* v )
{
   Int i;
   for (i = 0; i < 16; i++)
      v->b[i] = randUChar();
}

static void showV128 ( V128* v )
{
   Int i;
   for (i = 15; i >= 0; i--)
      printf("%02x", (Int)v->b[i]);
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
  block[1].d[0] = randULong();
  __asm__ __volatile__(
     "ldr q7, [%0, #0]   ; "
     "ldr x19, [%0, #16] ; "
     "ins v7.d[0], x19   ; "
     "str q7, [%0, #32] "
     : : "r"(&block[0]) : "memory", "x19", "v7"
  );
  printf("INS v7.d[0],x19  ");
  showV128(&block[0]); printf("  %016llx  ", block[1].d[0]);
  showV128(&block[2]); printf("\n");

  memset(&block, 0x55, sizeof(block));
  block[1].d[0] = randULong();
  __asm__ __volatile__(
     "ldr q7, [%0, #0]   ; "
     "ldr x19, [%0, #16] ; "
     "ins v7.d[1], x19   ; "
     "str q7, [%0, #32] "
     : : "r"(&block[0]) : "memory", "x19", "v7"
  );
  printf("INS v7.d[1],x19  ");
  showV128(&block[0]); printf("  %016llx  ", block[1].d[0]);
  showV128(&block[2]); printf("\n");

  /* -- S[0..3] -- */

  memset(&block, 0x55, sizeof(block));
  block[1].d[0] = randULong();
  __asm__ __volatile__(
     "ldr q7, [%0, #0]   ; "
     "ldr x19, [%0, #16] ; "
     "ins v7.s[0], w19   ; "
     "str q7, [%0, #32] "
     : : "r"(&block[0]) : "memory", "x19", "v7"
  );
  printf("INS v7.s[0],x19  ");
  showV128(&block[0]); printf("  %016llx  ", block[1].d[0]);
  showV128(&block[2]); printf("\n");

  memset(&block, 0x55, sizeof(block));
  block[1].d[0] = randULong();
  __asm__ __volatile__(
     "ldr q7, [%0, #0]   ; "
     "ldr x19, [%0, #16] ; "
     "ins v7.s[1], w19   ; "
     "str q7, [%0, #32] "
     : : "r"(&block[0]) : "memory", "x19", "v7"
  );
  printf("INS v7.s[1],x19  ");
  showV128(&block[0]); printf("  %016llx  ", block[1].d[0]);
  showV128(&block[2]); printf("\n");

  memset(&block, 0x55, sizeof(block));
  block[1].d[0] = randULong();
  __asm__ __volatile__(
     "ldr q7, [%0, #0]   ; "
     "ldr x19, [%0, #16] ; "
     "ins v7.s[2], w19   ; "
     "str q7, [%0, #32] "
     : : "r"(&block[0]) : "memory", "x19", "v7"
  );
  printf("INS v7.s[2],x19  ");
  showV128(&block[0]); printf("  %016llx  ", block[1].d[0]);
  showV128(&block[2]); printf("\n");

  memset(&block, 0x55, sizeof(block));
  block[1].d[0] = randULong();
  __asm__ __volatile__(
     "ldr q7, [%0, #0]   ; "
     "ldr x19, [%0, #16] ; "
     "ins v7.s[3], w19   ; "
     "str q7, [%0, #32] "
     : : "r"(&block[0]) : "memory", "x19", "v7"
  );
  printf("INS v7.s[3],x19  ");
  showV128(&block[0]); printf("  %016llx  ", block[1].d[0]);
  showV128(&block[2]); printf("\n");

  /* -- H[0..7] -- */

  memset(&block, 0x55, sizeof(block));
  block[1].d[0] = randULong();
  __asm__ __volatile__(
     "ldr q7, [%0, #0]   ; "
     "ldr x19, [%0, #16] ; "
     "ins v7.h[0], w19   ; "
     "str q7, [%0, #32] "
     : : "r"(&block[0]) : "memory", "x19", "v7"
  );
  printf("INS v7.h[0],x19  ");
  showV128(&block[0]); printf("  %016llx  ", block[1].d[0]);
  showV128(&block[2]); printf("\n");

  memset(&block, 0x55, sizeof(block));
  block[1].d[0] = randULong();
  __asm__ __volatile__(
     "ldr q7, [%0, #0]   ; "
     "ldr x19, [%0, #16] ; "
     "ins v7.h[1], w19   ; "
     "str q7, [%0, #32] "
     : : "r"(&block[0]) : "memory", "x19", "v7"
  );
  printf("INS v7.h[1],x19  ");
  showV128(&block[0]); printf("  %016llx  ", block[1].d[0]);
  showV128(&block[2]); printf("\n");

  memset(&block, 0x55, sizeof(block));
  block[1].d[0] = randULong();
  __asm__ __volatile__(
     "ldr q7, [%0, #0]   ; "
     "ldr x19, [%0, #16] ; "
     "ins v7.h[2], w19   ; "
     "str q7, [%0, #32] "
     : : "r"(&block[0]) : "memory", "x19", "v7"
  );
  printf("INS v7.h[2],x19  ");
  showV128(&block[0]); printf("  %016llx  ", block[1].d[0]);
  showV128(&block[2]); printf("\n");

  memset(&block, 0x55, sizeof(block));
  block[1].d[0] = randULong();
  __asm__ __volatile__(
     "ldr q7, [%0, #0]   ; "
     "ldr x19, [%0, #16] ; "
     "ins v7.h[3], w19   ; "
     "str q7, [%0, #32] "
     : : "r"(&block[0]) : "memory", "x19", "v7"
  );
  printf("INS v7.h[3],x19  ");
  showV128(&block[0]); printf("  %016llx  ", block[1].d[0]);
  showV128(&block[2]); printf("\n");

  memset(&block, 0x55, sizeof(block));
  block[1].d[0] = randULong();
  __asm__ __volatile__(
     "ldr q7, [%0, #0]   ; "
     "ldr x19, [%0, #16] ; "
     "ins v7.h[4], w19   ; "
     "str q7, [%0, #32] "
     : : "r"(&block[0]) : "memory", "x19", "v7"
  );
  printf("INS v7.h[4],x19  ");
  showV128(&block[0]); printf("  %016llx  ", block[1].d[0]);
  showV128(&block[2]); printf("\n");

  memset(&block, 0x55, sizeof(block));
  block[1].d[0] = randULong();
  __asm__ __volatile__(
     "ldr q7, [%0, #0]   ; "
     "ldr x19, [%0, #16] ; "
     "ins v7.h[5], w19   ; "
     "str q7, [%0, #32] "
     : : "r"(&block[0]) : "memory", "x19", "v7"
  );
  printf("INS v7.h[5],x19  ");
  showV128(&block[0]); printf("  %016llx  ", block[1].d[0]);
  showV128(&block[2]); printf("\n");

  memset(&block, 0x55, sizeof(block));
  block[1].d[0] = randULong();
  __asm__ __volatile__(
     "ldr q7, [%0, #0]   ; "
     "ldr x19, [%0, #16] ; "
     "ins v7.h[6], w19   ; "
     "str q7, [%0, #32] "
     : : "r"(&block[0]) : "memory", "x19", "v7"
  );
  printf("INS v7.h[6],x19  ");
  showV128(&block[0]); printf("  %016llx  ", block[1].d[0]);
  showV128(&block[2]); printf("\n");

  memset(&block, 0x55, sizeof(block));
  block[1].d[0] = randULong();
  __asm__ __volatile__(
     "ldr q7, [%0, #0]   ; "
     "ldr x19, [%0, #16] ; "
     "ins v7.h[7], w19   ; "
     "str q7, [%0, #32] "
     : : "r"(&block[0]) : "memory", "x19", "v7"
  );
  printf("INS v7.h[7],x19  ");
  showV128(&block[0]); printf("  %016llx  ", block[1].d[0]);
  showV128(&block[2]); printf("\n");

  /* -- B[0,15] -- */

  memset(&block, 0x55, sizeof(block));
  block[1].d[0] = randULong();
  __asm__ __volatile__(
     "ldr q7, [%0, #0]   ; "
     "ldr x19, [%0, #16] ; "
     "ins v7.b[0], w19   ; "
     "str q7, [%0, #32] "
     : : "r"(&block[0]) : "memory", "x19", "v7"
  );
  printf("INS v7.b[0],x19  ");
  showV128(&block[0]); printf("  %016llx  ", block[1].d[0]);
  showV128(&block[2]); printf("\n");

  memset(&block, 0x55, sizeof(block));
  block[1].d[0] = randULong();
  __asm__ __volatile__(
     "ldr q7, [%0, #0]   ; "
     "ldr x19, [%0, #16] ; "
     "ins v7.b[15], w19   ; "
     "str q7, [%0, #32] "
     : : "r"(&block[0]) : "memory", "x19", "v7"
  );
  printf("INS v7.b[15],x19 ");
  showV128(&block[0]); printf("  %016llx  ", block[1].d[0]);
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

#define ITERS 100

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

GEN_SHIFT_TEST(ushr, 4s, 4s, 1)
GEN_SHIFT_TEST(ushr, 4s, 4s, 13)
GEN_SHIFT_TEST(ushr, 4s, 4s, 31)
GEN_SHIFT_TEST(sshr, 4s, 4s, 1)
GEN_SHIFT_TEST(sshr, 4s, 4s, 13)
GEN_SHIFT_TEST(sshr, 4s, 4s, 31)

GEN_SHIFT_TEST(ushr, 2s, 2s, 1)
GEN_SHIFT_TEST(ushr, 2s, 2s, 13)
GEN_SHIFT_TEST(ushr, 2s, 2s, 31)
GEN_SHIFT_TEST(sshr, 2s, 2s, 1)
GEN_SHIFT_TEST(sshr, 2s, 2s, 13)
GEN_SHIFT_TEST(sshr, 2s, 2s, 31)

GEN_SHIFT_TEST(ushr, 8h, 8h, 1)
GEN_SHIFT_TEST(ushr, 8h, 8h, 13)
GEN_SHIFT_TEST(ushr, 8h, 8h, 15)
GEN_SHIFT_TEST(sshr, 8h, 8h, 1)
GEN_SHIFT_TEST(sshr, 8h, 8h, 13)
GEN_SHIFT_TEST(sshr, 8h, 8h, 15)

GEN_SHIFT_TEST(ushr, 4h, 4h, 1)
GEN_SHIFT_TEST(ushr, 4h, 4h, 13)
GEN_SHIFT_TEST(ushr, 4h, 4h, 15)
GEN_SHIFT_TEST(sshr, 4h, 4h, 1)
GEN_SHIFT_TEST(sshr, 4h, 4h, 13)
GEN_SHIFT_TEST(sshr, 4h, 4h, 15)

GEN_SHIFT_TEST(ushr, 16b, 16b, 1)
GEN_SHIFT_TEST(ushr, 16b, 16b, 7)
GEN_SHIFT_TEST(sshr, 16b, 16b, 1)
GEN_SHIFT_TEST(sshr, 16b, 16b, 7)

GEN_SHIFT_TEST(ushr, 8b, 8b, 1)
GEN_SHIFT_TEST(ushr, 8b, 8b, 7)
GEN_SHIFT_TEST(sshr, 8b, 8b, 1)
GEN_SHIFT_TEST(sshr, 8b, 8b, 7)

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

int main ( void )
{
   assert(sizeof(V128) == 16);

   printf("FMOV (general) MISSING\n");
   printf("FMOV (scalar, immediate) MISSING\n");
   printf("{FMOV,MOVI} (vector, immediate) MISSING\n");
   printf("{S,U}CVTF (scalar, integer) MISSING\n");
   printf("F{ADD,SUB,MUL,DIV,NMUL} (scalar) MISSING\n");
   printf("F{MOV,ABS,NEG,SQRT} D/D or S/S MISSING\n");
   printf("F{ABS,NEG} (vector) MISSING\n");
   printf("FCMP,FCMPE MISSING\n");
   printf("F{N}M{ADD,SUB} MISSING\n");
   printf("FCVT{N,P,M,Z}{S,U} (scalar, integer) MISSING\n");
   printf("FRINT{I,M,P,Z} (scalar) MISSING\n");
   printf("FCVT (scalar) MISSING\n");
   printf("FABD (scalar) MISSING\n");
   printf("{S,U}CVTF (vector, integer) MISSING\n");
   printf("F{ADD,SUB,MUL,DIV,MLA,MLS} (vector) MISSING\n");

   printf("ADD/SUB (vector) MISSING\n");
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

   printf("ADD/SUB (scalar) MISSING\n");

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
   printf("MUL/PMUL/MLA/MLS (vector) (partly MISSING)\n");

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

   test_UMINV();
   test_UMAXV();
   test_SMINV();
   test_SMAXV();

   test_and_16b();
   test_and_8b();
   test_bic_16b();
   test_bic_8b();
   test_orr_16b();
   test_orr_8b();
   test_orn_16b();
   test_orn_8b();

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
   printf("CM{EQ,HI,HS,GE,GT,TST,LE,LT} (vector) (w/zero cases MISSING)\n");

   test_eor_16b();
   test_eor_8b();
   test_bsl_16b();
   test_bsl_8b();
   test_bit_16b();
   test_bit_8b();
   test_bif_16b();
   test_bif_8b();

   test_ushr_2d_2d_1();
   test_ushr_2d_2d_13();
   test_ushr_2d_2d_63();
   test_sshr_2d_2d_1();
   test_sshr_2d_2d_13();
   test_sshr_2d_2d_63();
#if 0
   test_ushr_4s_4s_1();
   test_ushr_4s_4s_13();
   test_ushr_4s_4s_31();
   test_sshr_4s_4s_1();
   test_sshr_4s_4s_13();
   test_sshr_4s_4s_31();
   test_ushr_2s_2s_1();
   test_ushr_2s_2s_13();
   test_ushr_2s_2s_31();
   test_sshr_2s_2s_1();
   test_sshr_2s_2s_13();
   test_sshr_2s_2s_31();
   test_ushr_8h_8h_1();
   test_ushr_8h_8h_13();
   test_ushr_8h_8h_15();
   test_sshr_8h_8h_1();
   test_sshr_8h_8h_13();
   test_sshr_8h_8h_15();
   test_ushr_4h_4h_1();
   test_ushr_4h_4h_13();
   test_ushr_4h_4h_15();
   test_sshr_4h_4h_1();
   test_sshr_4h_4h_13();
   test_sshr_4h_4h_15();
   test_ushr_16b_16b_1();
   test_ushr_16b_16b_7();
   test_sshr_16b_16b_1();
   test_sshr_16b_16b_7();
   test_ushr_8b_8b_1();
   test_ushr_8b_8b_7();
   test_sshr_8b_8b_1();
   test_sshr_8b_8b_7();
#endif

   printf("{U,S}SHLL{,2} (MISSING h_b and s_h versions)\n");
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
   printf("{U,S}SHLL{,2} (MISSING h_b and s_h versions)\n");

   test_xtn_2s_2d();
   test_xtn2_4s_2d();
   test_xtn_4h_4s();
   test_xtn2_8h_4s();
   printf("XTN{,2} (MISSING b_h versions)\n");

   printf("DUP (element, vector) MISSING\n");
   printf("DUP (general, vector) MISSING\n");
   printf("{S,U}MOV MISSING\n");

   test_INS_general();

   return 0;
}
