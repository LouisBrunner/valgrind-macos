
/* To compile:
   aarch64-linux-gnu-gcc -march=armv8.1-a -Wall -g -O0 -o atomics_v8.1 \
                         none/tests/arm64/atomics_v8.1.c
*/

#include <stdio.h>
#include <malloc.h>
#include <stdint.h>
#include <string.h>

typedef  unsigned char           UChar;
typedef  unsigned int            UInt;
typedef  signed int              Int;
typedef  unsigned short int      UShort;
typedef  unsigned long long int  ULong;
typedef  signed long long int    Long;


#define CHECK(name, check_expr, size) \
static void name ## _check_ ## size(int ## size ## _t mval, int ## size ## _t rval, int ## size ## _t mval_after) \
{ \
   if ((int ## size ## _t)mval_after != ((int ## size ## _t)mval check_expr (int ## size ## _t)rval)) \
      printf("FAIL: mem after != mem before %s rs ", #check_expr); \
}

CHECK(add, +, 64);
CHECK(add, +, 16);
CHECK(add, +, 8);
CHECK(clr, & ~, 64);
CHECK(eor, ^, 64);

#define ATOMIC_TEST1(instruction, base_addr, mem_val_, rs_, chkfunc, size) \
{ \
   ULong rs = (ULong)rs_; \
   ULong mem_val = (ULong)mem_val_; \
   \
   ULong mem_val_after, rt; \
   mem_val_after = rt = 0ULL; \
   printf("%s :: rs %016llx rt %016llx rn mem %016llx\n", \
          instruction, rs, rt, mem_val); \
   \
   __asm__ __volatile__( \
      "mov x5, %2;" \
      "mov x13, %3;" \
      "str x13, [x5, #0];" \
      "mov x11, %4;" \
      instruction ";" \
      "ldr %0, [x5, #0];" \
      "mov %1, x12;" \
      : "=&r" (mem_val_after), "=&r" (rt) \
      : "r" (base_addr), "r" (mem_val) , "r" (rs) \
      : "x5", "x11", "x12", "x13", "memory" \
   ); \
   printf("%s :: rs %016llx rt %016llx rn mem %016llx  ", \
          instruction, rs, rt, mem_val_after); \
   chkfunc ## _check_ ## size(mem_val, rs, mem_val_after); \
   if (rt != mem_val) \
      printf("FAIL: rt != mem before"); \
   printf("\n\n"); \
}

#define ATOMIC_TEST_CAS(instruction, rsz, base_addr, mem_val_, rs_, rt_, dsz) \
{ \
   ULong rs = (ULong)rs_; \
   ULong rt = (ULong)rt_; \
   ULong mem_val = (ULong)mem_val_; \
   \
   ULong mem_val_after; \
   mem_val_after = 0ULL; \
   \
   int pad = (strcmp(#rsz, "w") == 0) ? 8 : 16; \
   printf("%s :: rs %0*llx rt %0*llx rn mem %0*llx\n", \
          instruction, pad, rs, pad, rt, pad, mem_val); \
   \
   Int swap = (rs == mem_val) ? 1 : 0; \
   __asm__ __volatile__( \
      "mov " #rsz "5, %" #rsz "1;" \
      "mov " #rsz "13, %" #rsz "2;" \
      "str " #rsz "13, [x5, #0];" \
      "mov " #rsz "11, %" #rsz "3;" \
      "mov " #rsz "12, %" #rsz "4;" \
      instruction ";" \
      "ldr %" #rsz "0, [x5, #0];" \
      : "=&r" (mem_val_after) \
      : "r" (base_addr), "r" (mem_val), "r" (rs), "r" (rt) \
      : #rsz "5", #rsz "11", #rsz "12", #rsz "13", "memory" \
   ); \
   printf("%s :: rs %0*llx rt %0*llx rn mem %0*llx  ", \
          instruction, pad, rs, pad, rt, pad, mem_val_after); \
   if (swap == 1) { \
      if ((mem_val_after & dsz) != (rt & dsz)) \
         printf("FAIL: swapped but mem after != rt"); \
   } \
   else { \
      if ((mem_val_after & dsz) != (mem_val & dsz)) \
         printf("FAIL: no swap but mem after != mem before"); \
   } \
   printf("\n\n"); \
}

#define ATOMIC_TEST_CASP(instruction, rsz, base_addr, mem_val0_, mem_val1_, rs0_, rs1_, rt0_, rt1_) \
{ \
   ULong rs0 = (ULong)rs0_; \
   ULong rs1 = (ULong)rs1_; \
   ULong rt0 = (ULong)rt0_; \
   ULong rt1 = (ULong)rt1_; \
   ULong mem_val0 = (ULong)mem_val0_; \
   ULong mem_val1 = (ULong)mem_val1_; \
   \
   ULong mem_val_after0, mem_val_after1; \
   mem_val_after0 = mem_val_after1 = 0ULL; \
   \
   int pad = (strcmp(#rsz, "w") == 0) ? 8 : 16; \
   printf("%s :: rs %0*llx %0*llx rt %0*llx %0*llx rn mem %0*llx %0*llx\n", \
          instruction, pad, rs0, pad, rs1, pad, rt0, pad, rt1, pad, mem_val0, pad, mem_val1); \
   \
   Int swap = (rs0 == mem_val0 && rs1 == mem_val1) ? 1 : 0; \
   __asm__ __volatile__( \
      "mov " #rsz "5, %" #rsz "2;" \
      "mov " #rsz "13, %" #rsz "3;" \
      "mov " #rsz "14, %" #rsz "4;" \
      "stp " #rsz "13, " #rsz "14, [x5, #0];" \
      "mov " #rsz "8, %" #rsz "5;" \
      "mov " #rsz "9, %" #rsz "6;" \
      "mov " #rsz "10, %" #rsz "7;" \
      "mov " #rsz "11, %" #rsz "8;" \
      instruction ";" \
      "ldp %" #rsz "0, %" #rsz "1, [x5, #0];" \
      : "=&r" (mem_val_after0), "=&r" (mem_val_after1) \
      : "r" (base_addr), "r" (mem_val0), "r" (mem_val1), "r" (rs0), "r" (rs1), "r" (rt0), "r" (rt1) \
      : #rsz "5", #rsz "8", #rsz "9", #rsz "10", #rsz "11", #rsz "13", #rsz "14", "memory" \
   ); \
   printf("%s :: rs %0*llx %0*llx rt %0*llx %0*llx rn mem %0*llx %0*llx", \
          instruction, pad, rs0, pad, rs1, pad, rt0, pad, rt1, pad, mem_val_after0, pad, mem_val_after1); \
   if (swap == 1) { \
      if (mem_val_after0 != rt0 || mem_val_after1 != rt1 ) \
         printf("FAIL: swapped but mem after != rt"); \
   } \
   else { \
      if (mem_val_after0 != mem_val0 || mem_val_after1 != mem_val1) \
         printf("FAIL: no swap but mem after != mem before"); \
   } \
   printf("\n\n"); \
}


// Test patterns
#define ALL5s_64  0x5555555555555555ULL
#define MOST5s_64 0x5555555555555554ULL
#define ALLas_64  0xAAAAAAAAAAAAAAAAULL
#define MOSTas_64 0xAAAAAAAAAAAAAAA8ULL
#define ALLfs_64  0xFFFFFFFFFFFFFFFFULL
#define MOSTfs_64 0xFFFFFFFFFFFFFFFEULL
#define UP_64     0x0123456789ABCDEFULL
#define DOWN_64   0xFEDCBA9876543210ULL
#define PI_64     0x3141592653589793ULL
#define E_64      0x2718281828459045ULL

#define ALL5s_32  0x55555555ULL
#define MOST5s_32 0x55555554ULL
#define ALLas_32  0xAAAAAAAAULL
#define MOSTas_32 0xAAAAAAA8ULL
#define ALLfs_32  0xFFFFFFFFULL
#define MOSTfs_32 0xFFFFFFFEULL
#define UP_32     0x01234567ULL
#define DOWN_32   0xFEDCBA98ULL
#define PI_32     0x31415926ULL
#define E_32      0x27182818ULL

#define ALL5s_16 0x5555ULL
#define ALLas_16 0xAAAAULL
#define ALLfs_16 0xFFFFULL
#define UP_16    0x0123ULL
#define DOWN_16  0xFEDCULL
#define PI_16    0x3141ULL
#define E_16     0x2718ULL

#define ALL5s_8 0x55ULL
#define ALLas_8 0xAAULL
#define ALLfs_8 0xFFULL
#define UP_8    0x01ULL
#define DOWN_8  0xFEULL
#define PI_8    0x31ULL
#define E_8     0x27ULL

static __attribute((noinline)) void test_atomics ( void )
{
   ULong *mem = (ULong *)malloc(sizeof(ULong));

   printf("LDADD <Xs>, <Xt>, [<Xn|SP>]\n");
   ATOMIC_TEST1("ldadd x11, x12, [x5]", mem, 0, 0, add, 64);
   ATOMIC_TEST1("ldadd x11, x12, [x5]", mem, 1, 1, add, 64);
   ATOMIC_TEST1("ldadd x11, x12, [x5]", mem, 1, -1, add, 64);
   ATOMIC_TEST1("ldadd x11, x12, [x5]", mem, -1, 1, add, 64);
   ATOMIC_TEST1("ldadd x11, x12, [x5]", mem, -1, -1, add, 64);
   ATOMIC_TEST1("ldadd x11, x12, [x5]", mem, 0, UP_64, add, 64);
   ATOMIC_TEST1("ldadd x11, x12, [x5]", mem, UP_64, 0, add, 64);
   ATOMIC_TEST1("ldadd x11, x12, [x5]", mem, UP_64, UP_64, add, 64);
   ATOMIC_TEST1("ldadd x11, x12, [x5]", mem, 0, DOWN_64, add, 64);
   ATOMIC_TEST1("ldadd x11, x12, [x5]", mem, DOWN_64, 0, add, 64);
   ATOMIC_TEST1("ldadd x11, x12, [x5]", mem, DOWN_64, DOWN_64, add, 64);
   ATOMIC_TEST1("ldadd x11, x12, [x5]", mem, 0, ALL5s_64, add, 64);
   ATOMIC_TEST1("ldadd x11, x12, [x5]", mem, ALL5s_64, 0, add, 64);
   ATOMIC_TEST1("ldadd x11, x12, [x5]", mem, ALL5s_64, ALL5s_64, add, 64);
   ATOMIC_TEST1("ldadd x11, x12, [x5]", mem, 0, ALLas_64, add, 64);
   ATOMIC_TEST1("ldadd x11, x12, [x5]", mem, ALLas_64, 0, add, 64);
   ATOMIC_TEST1("ldadd x11, x12, [x5]", mem, ALLas_64, ALLas_64, add, 64);
   ATOMIC_TEST1("ldadd x11, x12, [x5]", mem, 0, PI_64, add, 64);
   ATOMIC_TEST1("ldadd x11, x12, [x5]", mem, PI_64, 0, add, 64);
   ATOMIC_TEST1("ldadd x11, x12, [x5]", mem, PI_64, PI_64, add, 64);
   ATOMIC_TEST1("ldadd x11, x12, [x5]", mem, 0, E_64, add, 64);
   ATOMIC_TEST1("ldadd x11, x12, [x5]", mem, E_64, 0, add, 64);
   ATOMIC_TEST1("ldadd x11, x12, [x5]", mem, E_64, E_64, add, 64);

   printf("LDADDA <Xs>, <Xt>, [<Xn|SP>]\n");
   ATOMIC_TEST1("ldadda x11, x12, [x5]", mem, 0, 0, add, 64);
   ATOMIC_TEST1("ldadda x11, x12, [x5]", mem, 1, 1, add, 64);
   ATOMIC_TEST1("ldadda x11, x12, [x5]", mem, 1, -1, add, 64);
   ATOMIC_TEST1("ldadda x11, x12, [x5]", mem, -1, 1, add, 64);
   ATOMIC_TEST1("ldadda x11, x12, [x5]", mem, -1, -1, add, 64);
   ATOMIC_TEST1("ldadda x11, x12, [x5]", mem, 0, UP_64, add, 64);
   ATOMIC_TEST1("ldadda x11, x12, [x5]", mem, UP_64, 0, add, 64);
   ATOMIC_TEST1("ldadda x11, x12, [x5]", mem, UP_64, UP_64, add, 64);
   ATOMIC_TEST1("ldadda x11, x12, [x5]", mem, 0, DOWN_64, add, 64);
   ATOMIC_TEST1("ldadda x11, x12, [x5]", mem, DOWN_64, 0, add, 64);
   ATOMIC_TEST1("ldadda x11, x12, [x5]", mem, DOWN_64, DOWN_64, add, 64);
   ATOMIC_TEST1("ldadda x11, x12, [x5]", mem, 0, ALL5s_64, add, 64);
   ATOMIC_TEST1("ldadda x11, x12, [x5]", mem, ALL5s_64, 0, add, 64);
   ATOMIC_TEST1("ldadda x11, x12, [x5]", mem, ALL5s_64, ALL5s_64, add, 64);
   ATOMIC_TEST1("ldadda x11, x12, [x5]", mem, 0, ALLas_64, add, 64);
   ATOMIC_TEST1("ldadda x11, x12, [x5]", mem, ALLas_64, 0, add, 64);
   ATOMIC_TEST1("ldadda x11, x12, [x5]", mem, ALLas_64, ALLas_64, add, 64);
   ATOMIC_TEST1("ldadda x11, x12, [x5]", mem, 0, PI_64, add, 64);
   ATOMIC_TEST1("ldadda x11, x12, [x5]", mem, PI_64, 0, add, 64);
   ATOMIC_TEST1("ldadda x11, x12, [x5]", mem, PI_64, PI_64, add, 64);
   ATOMIC_TEST1("ldadda x11, x12, [x5]", mem, 0, E_64, add, 64);
   ATOMIC_TEST1("ldadda x11, x12, [x5]", mem, E_64, 0, add, 64);
   ATOMIC_TEST1("ldadda x11, x12, [x5]", mem, E_64, E_64, add, 64);

   printf("LDADDL <Ws>, <Wt>, [<Xn|SP>]\n");
   ATOMIC_TEST1("ldaddl x11, x12, [x5]", mem, 0, 0, add, 64);
   ATOMIC_TEST1("ldaddl x11, x12, [x5]", mem, 1, 1, add, 64);
   ATOMIC_TEST1("ldaddl x11, x12, [x5]", mem, 1, -1, add, 64);
   ATOMIC_TEST1("ldaddl x11, x12, [x5]", mem, -1, 1, add, 64);
   ATOMIC_TEST1("ldaddl x11, x12, [x5]", mem, -1, -1, add, 64);
   ATOMIC_TEST1("ldaddl x11, x12, [x5]", mem, 0, UP_64, add, 64);
   ATOMIC_TEST1("ldaddl x11, x12, [x5]", mem, UP_64, 0, add, 64);
   ATOMIC_TEST1("ldaddl x11, x12, [x5]", mem, UP_64, UP_64, add, 64);
   ATOMIC_TEST1("ldaddl x11, x12, [x5]", mem, 0, DOWN_64, add, 64);
   ATOMIC_TEST1("ldaddl x11, x12, [x5]", mem, DOWN_64, 0, add, 64);
   ATOMIC_TEST1("ldaddl x11, x12, [x5]", mem, DOWN_64, DOWN_64, add, 64);
   ATOMIC_TEST1("ldaddl x11, x12, [x5]", mem, 0, ALL5s_64, add, 64);
   ATOMIC_TEST1("ldaddl x11, x12, [x5]", mem, ALL5s_64, 0, add, 64);
   ATOMIC_TEST1("ldaddl x11, x12, [x5]", mem, ALL5s_64, ALL5s_64, add, 64);
   ATOMIC_TEST1("ldaddl x11, x12, [x5]", mem, 0, ALLas_64, add, 64);
   ATOMIC_TEST1("ldaddl x11, x12, [x5]", mem, ALLas_64, 0, add, 64);
   ATOMIC_TEST1("ldaddl x11, x12, [x5]", mem, ALLas_64, ALLas_64, add, 64);
   ATOMIC_TEST1("ldaddl x11, x12, [x5]", mem, 0, PI_64, add, 64);
   ATOMIC_TEST1("ldaddl x11, x12, [x5]", mem, PI_64, 0, add, 64);
   ATOMIC_TEST1("ldaddl x11, x12, [x5]", mem, PI_64, PI_64, add, 64);
   ATOMIC_TEST1("ldaddl x11, x12, [x5]", mem, 0, E_64, add, 64);
   ATOMIC_TEST1("ldaddl x11, x12, [x5]", mem, E_64, 0, add, 64);
   ATOMIC_TEST1("ldaddl x11, x12, [x5]", mem, E_64, E_64, add, 64);

   printf("LDADDAL <Xs>, <Xt>, [<Xn|SP>]\n");
   ATOMIC_TEST1("ldaddal x11, x12, [x5]", mem, 0, 0, add, 64);
   ATOMIC_TEST1("ldaddal x11, x12, [x5]", mem, 1, 1, add, 64);
   ATOMIC_TEST1("ldaddal x11, x12, [x5]", mem, 1, -1, add, 64);
   ATOMIC_TEST1("ldaddal x11, x12, [x5]", mem, -1, 1, add, 64);
   ATOMIC_TEST1("ldaddal x11, x12, [x5]", mem, -1, -1, add, 64);
   ATOMIC_TEST1("ldaddal x11, x12, [x5]", mem, 0, UP_64, add, 64);
   ATOMIC_TEST1("ldaddal x11, x12, [x5]", mem, UP_64, 0, add, 64);
   ATOMIC_TEST1("ldaddal x11, x12, [x5]", mem, UP_64, UP_64, add, 64);
   ATOMIC_TEST1("ldaddal x11, x12, [x5]", mem, 0, DOWN_64, add, 64);
   ATOMIC_TEST1("ldaddal x11, x12, [x5]", mem, DOWN_64, 0, add, 64);
   ATOMIC_TEST1("ldaddal x11, x12, [x5]", mem, DOWN_64, DOWN_64, add, 64);
   ATOMIC_TEST1("ldaddal x11, x12, [x5]", mem, 0, ALL5s_64, add, 64);
   ATOMIC_TEST1("ldaddal x11, x12, [x5]", mem, ALL5s_64, 0, add, 64);
   ATOMIC_TEST1("ldaddal x11, x12, [x5]", mem, ALL5s_64, ALL5s_64, add, 64);
   ATOMIC_TEST1("ldaddal x11, x12, [x5]", mem, 0, ALLas_64, add, 64);
   ATOMIC_TEST1("ldaddal x11, x12, [x5]", mem, ALLas_64, 0, add, 64);
   ATOMIC_TEST1("ldaddal x11, x12, [x5]", mem, ALLas_64, ALLas_64, add, 64);
   ATOMIC_TEST1("ldaddal x11, x12, [x5]", mem, 0, PI_64, add, 64);
   ATOMIC_TEST1("ldaddal x11, x12, [x5]", mem, PI_64, 0, add, 64);
   ATOMIC_TEST1("ldaddal x11, x12, [x5]", mem, PI_64, PI_64, add, 64);
   ATOMIC_TEST1("ldaddal x11, x12, [x5]", mem, 0, E_64, add, 64);
   ATOMIC_TEST1("ldaddal x11, x12, [x5]", mem, E_64, 0, add, 64);
   ATOMIC_TEST1("ldaddal x11, x12, [x5]", mem, E_64, E_64, add, 64);

   printf("LDADDH <Ws>, <Wt>, [<Xn|SP>]\n");
   ATOMIC_TEST1("ldaddh w11, w12, [x5]", mem, 0, 0, add, 16);
   ATOMIC_TEST1("ldaddh w11, w12, [x5]", mem, 0, 1, add, 16);
   ATOMIC_TEST1("ldaddh w11, w12, [x5]", mem, 1, 0, add, 16);
   ATOMIC_TEST1("ldaddh w11, w12, [x5]", mem, 1, 1, add, 16);
   ATOMIC_TEST1("ldaddh w11, w12, [x5]", mem, 1, ALLfs_16, add, 16);
   ATOMIC_TEST1("ldaddh w11, w12, [x5]", mem, ALLfs_16, 1, add, 16);
   ATOMIC_TEST1("ldaddh w11, w12, [x5]", mem, ALLfs_16, ALLfs_16, add, 16);
   ATOMIC_TEST1("ldaddh w11, w12, [x5]", mem, 0, UP_16, add, 16);
   ATOMIC_TEST1("ldaddh w11, w12, [x5]", mem, UP_16, 0, add, 16);
   ATOMIC_TEST1("ldaddh w11, w12, [x5]", mem, UP_16, UP_16, add, 16);
   ATOMIC_TEST1("ldaddh w11, w12, [x5]", mem, 0, DOWN_16, add, 16);
   ATOMIC_TEST1("ldaddh w11, w12, [x5]", mem, DOWN_16, 0, add, 16);
   ATOMIC_TEST1("ldaddh w11, w12, [x5]", mem, DOWN_16, DOWN_16, add, 16);
   ATOMIC_TEST1("ldaddh w11, w12, [x5]", mem, 0, ALL5s_16, add, 16);
   ATOMIC_TEST1("ldaddh w11, w12, [x5]", mem, ALL5s_16, 0, add, 16);
   ATOMIC_TEST1("ldaddh w11, w12, [x5]", mem, 0, ALLas_16, add, 16);
   ATOMIC_TEST1("ldaddh w11, w12, [x5]", mem, ALLas_16, 0, add, 16);
   ATOMIC_TEST1("ldaddh w11, w12, [x5]", mem, 0, PI_16, add, 16);
   ATOMIC_TEST1("ldaddh w11, w12, [x5]", mem, PI_16, 0, add, 16);
   ATOMIC_TEST1("ldaddh w11, w12, [x5]", mem, PI_16, PI_16, add, 16);
   ATOMIC_TEST1("ldaddh w11, w12, [x5]", mem, 0, E_16, add, 16);
   ATOMIC_TEST1("ldaddh w11, w12, [x5]", mem, E_16, 0, add, 16);
   ATOMIC_TEST1("ldaddh w11, w12, [x5]", mem, E_16, E_16, add, 16);

   printf("LDADDAH <Ws>, <Wt>, [<Xn|SP>]\n");
   ATOMIC_TEST1("ldaddah w11, w12, [x5]", mem, 0, 0, add, 16);
   ATOMIC_TEST1("ldaddah w11, w12, [x5]", mem, 0, 1, add, 16);
   ATOMIC_TEST1("ldaddah w11, w12, [x5]", mem, 1, 0, add, 16);
   ATOMIC_TEST1("ldaddah w11, w12, [x5]", mem, 1, 1, add, 16);
   ATOMIC_TEST1("ldaddah w11, w12, [x5]", mem, 1, ALLfs_16, add, 16);
   ATOMIC_TEST1("ldaddah w11, w12, [x5]", mem, ALLfs_16, 1, add, 16);
   ATOMIC_TEST1("ldaddah w11, w12, [x5]", mem, ALLfs_16, ALLfs_16, add, 16);
   ATOMIC_TEST1("ldaddah w11, w12, [x5]", mem, 0, UP_16, add, 16);
   ATOMIC_TEST1("ldaddah w11, w12, [x5]", mem, UP_16, 0, add, 16);
   ATOMIC_TEST1("ldaddah w11, w12, [x5]", mem, UP_16, UP_16, add, 16);
   ATOMIC_TEST1("ldaddah w11, w12, [x5]", mem, 0, DOWN_16, add, 16);
   ATOMIC_TEST1("ldaddah w11, w12, [x5]", mem, DOWN_16, 0, add, 16);
   ATOMIC_TEST1("ldaddah w11, w12, [x5]", mem, DOWN_16, DOWN_16, add, 16);
   ATOMIC_TEST1("ldaddah w11, w12, [x5]", mem, 0, ALL5s_16, add, 16);
   ATOMIC_TEST1("ldaddah w11, w12, [x5]", mem, ALL5s_16, 0, add, 16);
   ATOMIC_TEST1("ldaddah w11, w12, [x5]", mem, 0, ALLas_16, add, 16);
   ATOMIC_TEST1("ldaddah w11, w12, [x5]", mem, ALLas_16, 0, add, 16);
   ATOMIC_TEST1("ldaddah w11, w12, [x5]", mem, 0, PI_16, add, 16);
   ATOMIC_TEST1("ldaddah w11, w12, [x5]", mem, PI_16, 0, add, 16);
   ATOMIC_TEST1("ldaddah w11, w12, [x5]", mem, PI_16, PI_16, add, 16);
   ATOMIC_TEST1("ldaddah w11, w12, [x5]", mem, 0, E_16, add, 16);
   ATOMIC_TEST1("ldaddah w11, w12, [x5]", mem, E_16, 0, add, 16);
   ATOMIC_TEST1("ldaddah w11, w12, [x5]", mem, E_16, E_16, add, 16);

   printf("LDADDALH <Ws>, <Wt>, [<Xn|SP>]\n");
   ATOMIC_TEST1("ldaddalh w11, w12, [x5]", mem, 0, 0, add, 16);
   ATOMIC_TEST1("ldaddalh w11, w12, [x5]", mem, 0, 1, add, 16);
   ATOMIC_TEST1("ldaddalh w11, w12, [x5]", mem, 1, 0, add, 16);
   ATOMIC_TEST1("ldaddalh w11, w12, [x5]", mem, 1, 1, add, 16);
   ATOMIC_TEST1("ldaddalh w11, w12, [x5]", mem, 1, ALLfs_16, add, 16);
   ATOMIC_TEST1("ldaddalh w11, w12, [x5]", mem, ALLfs_16, 1, add, 16);
   ATOMIC_TEST1("ldaddalh w11, w12, [x5]", mem, ALLfs_16, ALLfs_16, add, 16);
   ATOMIC_TEST1("ldaddalh w11, w12, [x5]", mem, 0, UP_16, add, 16);
   ATOMIC_TEST1("ldaddalh w11, w12, [x5]", mem, UP_16, 0, add, 16);
   ATOMIC_TEST1("ldaddalh w11, w12, [x5]", mem, UP_16, UP_16, add, 16);
   ATOMIC_TEST1("ldaddalh w11, w12, [x5]", mem, 0, DOWN_16, add, 16);
   ATOMIC_TEST1("ldaddalh w11, w12, [x5]", mem, DOWN_16, 0, add, 16);
   ATOMIC_TEST1("ldaddalh w11, w12, [x5]", mem, DOWN_16, DOWN_16, add, 16);
   ATOMIC_TEST1("ldaddalh w11, w12, [x5]", mem, 0, ALL5s_16, add, 16);
   ATOMIC_TEST1("ldaddalh w11, w12, [x5]", mem, ALL5s_16, 0, add, 16);
   ATOMIC_TEST1("ldaddalh w11, w12, [x5]", mem, 0, ALLas_16, add, 16);
   ATOMIC_TEST1("ldaddalh w11, w12, [x5]", mem, ALLas_16, 0, add, 16);
   ATOMIC_TEST1("ldaddalh w11, w12, [x5]", mem, 0, PI_16, add, 16);
   ATOMIC_TEST1("ldaddalh w11, w12, [x5]", mem, PI_16, 0, add, 16);
   ATOMIC_TEST1("ldaddalh w11, w12, [x5]", mem, PI_16, PI_16, add, 16);
   ATOMIC_TEST1("ldaddalh w11, w12, [x5]", mem, 0, E_16, add, 16);
   ATOMIC_TEST1("ldaddalh w11, w12, [x5]", mem, E_16, 0, add, 16);
   ATOMIC_TEST1("ldaddalh w11, w12, [x5]", mem, E_16, E_16, add, 16);

   printf("LDADDLH <Ws>, <Wt>, [<Xn|SP>]\n");
   ATOMIC_TEST1("ldaddlh w11, w12, [x5]", mem, 0, 0, add, 16);
   ATOMIC_TEST1("ldaddlh w11, w12, [x5]", mem, 0, 1, add, 16);
   ATOMIC_TEST1("ldaddlh w11, w12, [x5]", mem, 1, 0, add, 16);
   ATOMIC_TEST1("ldaddlh w11, w12, [x5]", mem, 1, 1, add, 16);
   ATOMIC_TEST1("ldaddlh w11, w12, [x5]", mem, 1, ALLfs_16, add, 16);
   ATOMIC_TEST1("ldaddlh w11, w12, [x5]", mem, ALLfs_16, 1, add, 16);
   ATOMIC_TEST1("ldaddlh w11, w12, [x5]", mem, ALLfs_16, ALLfs_16, add, 16);
   ATOMIC_TEST1("ldaddlh w11, w12, [x5]", mem, 0, UP_16, add, 16);
   ATOMIC_TEST1("ldaddlh w11, w12, [x5]", mem, UP_16, 0, add, 16);
   ATOMIC_TEST1("ldaddlh w11, w12, [x5]", mem, UP_16, UP_16, add, 16);
   ATOMIC_TEST1("ldaddlh w11, w12, [x5]", mem, 0, DOWN_16, add, 16);
   ATOMIC_TEST1("ldaddlh w11, w12, [x5]", mem, DOWN_16, 0, add, 16);
   ATOMIC_TEST1("ldaddlh w11, w12, [x5]", mem, DOWN_16, DOWN_16, add, 16);
   ATOMIC_TEST1("ldaddlh w11, w12, [x5]", mem, 0, ALL5s_16, add, 16);
   ATOMIC_TEST1("ldaddlh w11, w12, [x5]", mem, ALL5s_16, 0, add, 16);
   ATOMIC_TEST1("ldaddlh w11, w12, [x5]", mem, 0, ALLas_16, add, 16);
   ATOMIC_TEST1("ldaddlh w11, w12, [x5]", mem, ALLas_16, 0, add, 16);
   ATOMIC_TEST1("ldaddlh w11, w12, [x5]", mem, 0, PI_16, add, 16);
   ATOMIC_TEST1("ldaddlh w11, w12, [x5]", mem, PI_16, 0, add, 16);
   ATOMIC_TEST1("ldaddlh w11, w12, [x5]", mem, PI_16, PI_16, add, 16);
   ATOMIC_TEST1("ldaddlh w11, w12, [x5]", mem, 0, E_16, add, 16);
   ATOMIC_TEST1("ldaddlh w11, w12, [x5]", mem, E_16, 0, add, 16);
   ATOMIC_TEST1("ldaddlh w11, w12, [x5]", mem, E_16, E_16, add, 16);

   printf("LDADDB <Ws>, <Wt>, [<Xn|SP>]\n");
   ATOMIC_TEST1("ldaddb w11, w12, [x5]", mem, 0, 0, add, 8);
   ATOMIC_TEST1("ldaddb w11, w12, [x5]", mem, 0, 1, add, 8);
   ATOMIC_TEST1("ldaddb w11, w12, [x5]", mem, 1, 0, add, 8);
   ATOMIC_TEST1("ldaddb w11, w12, [x5]", mem, 1, 1, add, 8);
   ATOMIC_TEST1("ldaddb w11, w12, [x5]", mem, 1, ALLfs_8, add, 8);
   ATOMIC_TEST1("ldaddb w11, w12, [x5]", mem, ALLfs_8, 1, add, 8);
   ATOMIC_TEST1("ldaddb w11, w12, [x5]", mem, ALLfs_8, ALLfs_8, add, 8);
   ATOMIC_TEST1("ldaddb w11, w12, [x5]", mem, 0, UP_8, add, 8);
   ATOMIC_TEST1("ldaddb w11, w12, [x5]", mem, UP_8, 0, add, 8);
   ATOMIC_TEST1("ldaddb w11, w12, [x5]", mem, UP_8, UP_8, add, 8);
   ATOMIC_TEST1("ldaddb w11, w12, [x5]", mem, 0, DOWN_8, add, 8);
   ATOMIC_TEST1("ldaddb w11, w12, [x5]", mem, DOWN_8, 0, add, 8);
   ATOMIC_TEST1("ldaddb w11, w12, [x5]", mem, DOWN_8, DOWN_8, add, 8);
   ATOMIC_TEST1("ldaddb w11, w12, [x5]", mem, 0, ALL5s_8, add, 8);
   ATOMIC_TEST1("ldaddb w11, w12, [x5]", mem, ALL5s_8, 0, add, 8);
   ATOMIC_TEST1("ldaddb w11, w12, [x5]", mem, 0, ALLas_8, add, 8);
   ATOMIC_TEST1("ldaddb w11, w12, [x5]", mem, ALLas_8, 0, add, 8);
   ATOMIC_TEST1("ldaddb w11, w12, [x5]", mem, 0, PI_8, add, 8);
   ATOMIC_TEST1("ldaddb w11, w12, [x5]", mem, PI_8, 0, add, 8);
   ATOMIC_TEST1("ldaddb w11, w12, [x5]", mem, PI_8, PI_8, add, 8);
   ATOMIC_TEST1("ldaddb w11, w12, [x5]", mem, 0, E_8, add, 8);
   ATOMIC_TEST1("ldaddb w11, w12, [x5]", mem, E_8, 0, add, 8);
   ATOMIC_TEST1("ldaddb w11, w12, [x5]", mem, E_8, E_8, add, 8);

   printf("LDADDAB <Ws>, <Wt>, [<Xn|SP>]\n");
   ATOMIC_TEST1("ldaddab w11, w12, [x5]", mem, 0, 0, add, 8);
   ATOMIC_TEST1("ldaddab w11, w12, [x5]", mem, 0, 1, add, 8);
   ATOMIC_TEST1("ldaddab w11, w12, [x5]", mem, 1, 0, add, 8);
   ATOMIC_TEST1("ldaddab w11, w12, [x5]", mem, 1, 1, add, 8);
   ATOMIC_TEST1("ldaddab w11, w12, [x5]", mem, 1, ALLfs_8, add, 8);
   ATOMIC_TEST1("ldaddab w11, w12, [x5]", mem, ALLfs_8, 1, add, 8);
   ATOMIC_TEST1("ldaddab w11, w12, [x5]", mem, ALLfs_8, ALLfs_8, add, 8);
   ATOMIC_TEST1("ldaddab w11, w12, [x5]", mem, 0, UP_8, add, 8);
   ATOMIC_TEST1("ldaddab w11, w12, [x5]", mem, UP_8, 0, add, 8);
   ATOMIC_TEST1("ldaddab w11, w12, [x5]", mem, UP_8, UP_8, add, 8);
   ATOMIC_TEST1("ldaddab w11, w12, [x5]", mem, 0, DOWN_8, add, 8);
   ATOMIC_TEST1("ldaddab w11, w12, [x5]", mem, DOWN_8, 0, add, 8);
   ATOMIC_TEST1("ldaddab w11, w12, [x5]", mem, DOWN_8, DOWN_8, add, 8);
   ATOMIC_TEST1("ldaddab w11, w12, [x5]", mem, 0, ALL5s_8, add, 8);
   ATOMIC_TEST1("ldaddab w11, w12, [x5]", mem, ALL5s_8, 0, add, 8);
   ATOMIC_TEST1("ldaddab w11, w12, [x5]", mem, 0, ALLas_8, add, 8);
   ATOMIC_TEST1("ldaddab w11, w12, [x5]", mem, ALLas_8, 0, add, 8);
   ATOMIC_TEST1("ldaddab w11, w12, [x5]", mem, 0, PI_8, add, 8);
   ATOMIC_TEST1("ldaddab w11, w12, [x5]", mem, PI_8, 0, add, 8);
   ATOMIC_TEST1("ldaddab w11, w12, [x5]", mem, PI_8, PI_8, add, 8);
   ATOMIC_TEST1("ldaddab w11, w12, [x5]", mem, 0, E_8, add, 8);
   ATOMIC_TEST1("ldaddab w11, w12, [x5]", mem, E_8, 0, add, 8);
   ATOMIC_TEST1("ldaddab w11, w12, [x5]", mem, E_8, E_8, add, 8);

   printf("LDADDALB <Ws>, <Wt>, [<Xn|SP>]\n");
   ATOMIC_TEST1("ldaddalb w11, w12, [x5]", mem, 0, 0, add, 8);
   ATOMIC_TEST1("ldaddalb w11, w12, [x5]", mem, 0, 1, add, 8);
   ATOMIC_TEST1("ldaddalb w11, w12, [x5]", mem, 1, 0, add, 8);
   ATOMIC_TEST1("ldaddalb w11, w12, [x5]", mem, 1, 1, add, 8);
   ATOMIC_TEST1("ldaddalb w11, w12, [x5]", mem, 1, ALLfs_8, add, 8);
   ATOMIC_TEST1("ldaddalb w11, w12, [x5]", mem, ALLfs_8, 1, add, 8);
   ATOMIC_TEST1("ldaddalb w11, w12, [x5]", mem, ALLfs_8, ALLfs_8, add, 8);
   ATOMIC_TEST1("ldaddalb w11, w12, [x5]", mem, 0, UP_8, add, 8);
   ATOMIC_TEST1("ldaddalb w11, w12, [x5]", mem, UP_8, 0, add, 8);
   ATOMIC_TEST1("ldaddalb w11, w12, [x5]", mem, UP_8, UP_8, add, 8);
   ATOMIC_TEST1("ldaddalb w11, w12, [x5]", mem, 0, DOWN_8, add, 8);
   ATOMIC_TEST1("ldaddalb w11, w12, [x5]", mem, DOWN_8, 0, add, 8);
   ATOMIC_TEST1("ldaddalb w11, w12, [x5]", mem, DOWN_8, DOWN_8, add, 8);
   ATOMIC_TEST1("ldaddalb w11, w12, [x5]", mem, 0, ALL5s_8, add, 8);
   ATOMIC_TEST1("ldaddalb w11, w12, [x5]", mem, ALL5s_8, 0, add, 8);
   ATOMIC_TEST1("ldaddalb w11, w12, [x5]", mem, 0, ALLas_8, add, 8);
   ATOMIC_TEST1("ldaddalb w11, w12, [x5]", mem, ALLas_8, 0, add, 8);
   ATOMIC_TEST1("ldaddalb w11, w12, [x5]", mem, 0, PI_8, add, 8);
   ATOMIC_TEST1("ldaddalb w11, w12, [x5]", mem, PI_8, 0, add, 8);
   ATOMIC_TEST1("ldaddalb w11, w12, [x5]", mem, PI_8, PI_8, add, 8);
   ATOMIC_TEST1("ldaddalb w11, w12, [x5]", mem, 0, E_8, add, 8);
   ATOMIC_TEST1("ldaddalb w11, w12, [x5]", mem, E_8, 0, add, 8);
   ATOMIC_TEST1("ldaddalb w11, w12, [x5]", mem, E_8, E_8, add, 8);

   printf("LDADDLB <Ws>, <Wt>, [<Xn|SP>]\n");
   ATOMIC_TEST1("ldaddlb w11, w12, [x5]", mem, 0, 0, add, 8);
   ATOMIC_TEST1("ldaddlb w11, w12, [x5]", mem, 0, 1, add, 8);
   ATOMIC_TEST1("ldaddlb w11, w12, [x5]", mem, 1, 0, add, 8);
   ATOMIC_TEST1("ldaddlb w11, w12, [x5]", mem, 1, 1, add, 8);
   ATOMIC_TEST1("ldaddlb w11, w12, [x5]", mem, 1, ALLfs_8, add, 8);
   ATOMIC_TEST1("ldaddlb w11, w12, [x5]", mem, ALLfs_8, 1, add, 8);
   ATOMIC_TEST1("ldaddlb w11, w12, [x5]", mem, ALLfs_8, ALLfs_8, add, 8);
   ATOMIC_TEST1("ldaddlb w11, w12, [x5]", mem, 0, UP_8, add, 8);
   ATOMIC_TEST1("ldaddlb w11, w12, [x5]", mem, UP_8, 0, add, 8);
   ATOMIC_TEST1("ldaddlb w11, w12, [x5]", mem, UP_8, UP_8, add, 8);
   ATOMIC_TEST1("ldaddlb w11, w12, [x5]", mem, 0, DOWN_8, add, 8);
   ATOMIC_TEST1("ldaddlb w11, w12, [x5]", mem, DOWN_8, 0, add, 8);
   ATOMIC_TEST1("ldaddlb w11, w12, [x5]", mem, DOWN_8, DOWN_8, add, 8);
   ATOMIC_TEST1("ldaddlb w11, w12, [x5]", mem, 0, ALL5s_8, add, 8);
   ATOMIC_TEST1("ldaddlb w11, w12, [x5]", mem, ALL5s_8, 0, add, 8);
   ATOMIC_TEST1("ldaddlb w11, w12, [x5]", mem, 0, ALLas_8, add, 8);
   ATOMIC_TEST1("ldaddlb w11, w12, [x5]", mem, ALLas_8, 0, add, 8);
   ATOMIC_TEST1("ldaddlb w11, w12, [x5]", mem, 0, PI_8, add, 8);
   ATOMIC_TEST1("ldaddlb w11, w12, [x5]", mem, PI_8, 0, add, 8);
   ATOMIC_TEST1("ldaddlb w11, w12, [x5]", mem, PI_8, PI_8, add, 8);
   ATOMIC_TEST1("ldaddlb w11, w12, [x5]", mem, 0, E_8, add, 8);
   ATOMIC_TEST1("ldaddlb w11, w12, [x5]", mem, E_8, 0, add, 8);
   ATOMIC_TEST1("ldaddlb w11, w12, [x5]", mem, E_8, E_8, add, 8);

   printf("LDCLR <Xs>, <Xt>, [<Xn|SP>]\n");
   ATOMIC_TEST1("ldclr x11, x12, [x5]", mem, 0, 0, clr, 64);
   ATOMIC_TEST1("ldclr x11, x12, [x5]", mem, 1, 1, clr, 64);
   ATOMIC_TEST1("ldclr x11, x12, [x5]", mem, 1, -1, clr, 64);
   ATOMIC_TEST1("ldclr x11, x12, [x5]", mem, -1, 1, clr, 64);
   ATOMIC_TEST1("ldclr x11, x12, [x5]", mem, -1, -1, clr, 64);
   ATOMIC_TEST1("ldclr x11, x12, [x5]", mem, 0, UP_64, clr, 64);
   ATOMIC_TEST1("ldclr x11, x12, [x5]", mem, UP_64, 0, clr, 64);
   ATOMIC_TEST1("ldclr x11, x12, [x5]", mem, UP_64, UP_64, clr, 64);
   ATOMIC_TEST1("ldclr x11, x12, [x5]", mem, 0, DOWN_64, clr, 64);
   ATOMIC_TEST1("ldclr x11, x12, [x5]", mem, DOWN_64, 0, clr, 64);
   ATOMIC_TEST1("ldclr x11, x12, [x5]", mem, DOWN_64, DOWN_64, clr, 64);
   ATOMIC_TEST1("ldclr x11, x12, [x5]", mem, 0, ALL5s_64, clr, 64);
   ATOMIC_TEST1("ldclr x11, x12, [x5]", mem, ALL5s_64, 0, clr, 64);
   ATOMIC_TEST1("ldclr x11, x12, [x5]", mem, ALL5s_64, ALL5s_64, clr, 64);
   ATOMIC_TEST1("ldclr x11, x12, [x5]", mem, 0, ALLas_64, clr, 64);
   ATOMIC_TEST1("ldclr x11, x12, [x5]", mem, ALLas_64, 0, clr, 64);
   ATOMIC_TEST1("ldclr x11, x12, [x5]", mem, ALLas_64, ALLas_64, clr, 64);
   ATOMIC_TEST1("ldclr x11, x12, [x5]", mem, 0, PI_64, clr, 64);
   ATOMIC_TEST1("ldclr x11, x12, [x5]", mem, PI_64, 0, clr, 64);
   ATOMIC_TEST1("ldclr x11, x12, [x5]", mem, PI_64, PI_64, clr, 64);
   ATOMIC_TEST1("ldclr x11, x12, [x5]", mem, 0, E_64, clr, 64);
   ATOMIC_TEST1("ldclr x11, x12, [x5]", mem, E_64, 0, clr, 64);
   ATOMIC_TEST1("ldclr x11, x12, [x5]", mem, E_64, E_64, clr, 64);
   // TODO: LDCLRA, LDCLRAL, LDCLRL
   // LDCLRB, LDCLRAB, LDCLRALB, LDCLRLB
   // LDCLRH, LDCLRAH, LDCLRALH, LDCLRLH

   printf("LDEOR <Xs>, <Xt>, [<Xn|SP>]\n");
   ATOMIC_TEST1("ldeor x11, x12, [x5]", mem, 0, 0, eor, 64);
   ATOMIC_TEST1("ldeor x11, x12, [x5]", mem, 1, 1, eor, 64);
   ATOMIC_TEST1("ldeor x11, x12, [x5]", mem, 1, -1, eor, 64);
   ATOMIC_TEST1("ldeor x11, x12, [x5]", mem, -1, 1, eor, 64);
   ATOMIC_TEST1("ldeor x11, x12, [x5]", mem, -1, -1, eor, 64);
   ATOMIC_TEST1("ldeor x11, x12, [x5]", mem, 0, UP_64, eor, 64);
   ATOMIC_TEST1("ldeor x11, x12, [x5]", mem, UP_64, 0, eor, 64);
   ATOMIC_TEST1("ldeor x11, x12, [x5]", mem, UP_64, UP_64, eor, 64);
   ATOMIC_TEST1("ldeor x11, x12, [x5]", mem, 0, DOWN_64, eor, 64);
   ATOMIC_TEST1("ldeor x11, x12, [x5]", mem, DOWN_64, 0, eor, 64);
   ATOMIC_TEST1("ldeor x11, x12, [x5]", mem, DOWN_64, DOWN_64, eor, 64);
   ATOMIC_TEST1("ldeor x11, x12, [x5]", mem, 0, ALL5s_64, eor, 64);
   ATOMIC_TEST1("ldeor x11, x12, [x5]", mem, ALL5s_64, 0, eor, 64);
   ATOMIC_TEST1("ldeor x11, x12, [x5]", mem, ALL5s_64, ALL5s_64, eor, 64);
   ATOMIC_TEST1("ldeor x11, x12, [x5]", mem, 0, ALLas_64, eor, 64);
   ATOMIC_TEST1("ldeor x11, x12, [x5]", mem, ALLas_64, 0, eor, 64);
   ATOMIC_TEST1("ldeor x11, x12, [x5]", mem, ALLas_64, ALLas_64, eor, 64);
   ATOMIC_TEST1("ldeor x11, x12, [x5]", mem, 0, PI_64, eor, 64);
   ATOMIC_TEST1("ldeor x11, x12, [x5]", mem, PI_64, 0, eor, 64);
   ATOMIC_TEST1("ldeor x11, x12, [x5]", mem, PI_64, PI_64, eor, 64);
   ATOMIC_TEST1("ldeor x11, x12, [x5]", mem, 0, E_64, eor, 64);
   ATOMIC_TEST1("ldeor x11, x12, [x5]", mem, E_64, 0, eor, 64);
   ATOMIC_TEST1("ldeor x11, x12, [x5]", mem, E_64, E_64, eor, 64);
   // TODO: LDEORA, LDEORAL, LDEORL
   // LDEORB, LDEORAB, LDEORALB, LDEORLB
   // LDEORH, LDEORAH, LDEORALH, LDEORLH

   printf("CAS <Ws>, <Wt>, [<Xn|SP>]\n");

   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, 0, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, 0, 1, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, 1, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, 1, 1, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 1, 0, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 1, 0, 1, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 1, 1, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 1, 1, 1, 0xFFFFFFFF);

   printf("Combinations of ALL5s_32 and all other patterns\n");

   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, ALL5s_32, ALL5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, ALL5s_32, 0, ALL5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, ALL5s_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, ALL5s_32, MOST5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, ALL5s_32, 0, MOST5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, MOST5s_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, ALL5s_32, ALLas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, ALL5s_32, 0, ALLas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, ALLas_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, ALL5s_32, MOSTas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, ALL5s_32, 0, MOSTas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, MOSTas_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, ALL5s_32, ALLfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, ALL5s_32, 0, ALLfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, ALLfs_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, ALL5s_32, MOSTfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, ALL5s_32, 0, MOSTfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, MOSTfs_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, ALL5s_32, UP_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, ALL5s_32, 0, UP_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, UP_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, ALL5s_32, DOWN_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, ALL5s_32, 0, DOWN_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, DOWN_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, ALL5s_32, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, ALL5s_32, 0, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, 0, 0xFFFFFFFF);

   printf("Combinations of MOST5s_32 and all other patterns\n");

   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, MOST5s_32, ALL5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, MOST5s_32, 0, ALL5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, ALL5s_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, MOST5s_32, MOST5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, MOST5s_32, 0, MOST5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, MOST5s_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, MOST5s_32, ALLas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, MOST5s_32, 0, ALLas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, ALLas_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, MOST5s_32, MOSTas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, MOST5s_32, 0, MOSTas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, MOSTas_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, MOST5s_32, ALLfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, MOST5s_32, 0, ALLfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, ALLfs_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, MOST5s_32, MOSTfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, MOST5s_32, 0, MOSTfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, MOSTfs_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, MOST5s_32, UP_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, MOST5s_32, 0, UP_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, UP_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, MOST5s_32, DOWN_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, MOST5s_32, 0, DOWN_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, DOWN_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, MOST5s_32, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, MOST5s_32, 0, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, 0, 0xFFFFFFFF);

   printf("Combinations of ALLas_32 and all other patterns\n");

   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, ALLas_32, ALL5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, ALLas_32, 0, ALL5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, ALL5s_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, ALLas_32, MOST5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, ALLas_32, 0, MOST5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, MOST5s_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, ALLas_32, ALLas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, ALLas_32, 0, ALLas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, ALLas_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, ALLas_32, MOSTas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, ALLas_32, 0, MOSTas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, MOSTas_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, ALLas_32, ALLfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, ALLas_32, 0, ALLfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, ALLfs_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, ALLas_32, MOSTfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, ALLas_32, 0, MOSTfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, MOSTfs_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, ALLas_32, UP_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, ALLas_32, 0, UP_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, UP_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, ALLas_32, DOWN_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, ALLas_32, 0, DOWN_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, DOWN_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, ALLas_32, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, ALLas_32, 0, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, 0, 0xFFFFFFFF);

   printf("Combinations of MOSTas_32 and all other patterns\n");

   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, MOSTas_32, ALL5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, MOSTas_32, 0, ALL5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, ALL5s_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, MOSTas_32, MOST5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, MOSTas_32, 0, MOST5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, MOST5s_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, MOSTas_32, ALLas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, MOSTas_32, 0, ALLas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, ALLas_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, MOSTas_32, MOSTas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, MOSTas_32, 0, MOSTas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, MOSTas_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, MOSTas_32, ALLfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, MOSTas_32, 0, ALLfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, ALLfs_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, MOSTas_32, MOSTfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, MOSTas_32, 0, MOSTfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, MOSTfs_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, MOSTas_32, UP_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, MOSTas_32, 0, UP_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, UP_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, MOSTas_32, DOWN_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, MOSTas_32, 0, DOWN_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, DOWN_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, MOSTas_32, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, MOSTas_32, 0, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, 0, 0xFFFFFFFF);

   printf("Combinations of ALLfs_32 and all other patterns\n");

   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, ALLfs_32, ALL5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, ALLfs_32, 0, ALL5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, ALL5s_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, ALLfs_32, MOST5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, ALLfs_32, 0, MOST5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, MOST5s_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, ALLfs_32, ALLas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, ALLfs_32, 0, ALLas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, ALLas_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, ALLfs_32, MOSTas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, ALLfs_32, 0, MOSTas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, MOSTas_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, ALLfs_32, ALLfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, ALLfs_32, 0, ALLfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, ALLfs_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, ALLfs_32, MOSTfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, ALLfs_32, 0, MOSTfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, MOSTfs_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, ALLfs_32, UP_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, ALLfs_32, 0, UP_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, UP_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, ALLfs_32, DOWN_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, ALLfs_32, 0, DOWN_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, DOWN_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, ALLfs_32, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, ALLfs_32, 0, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, 0, 0xFFFFFFFF);

   printf("Combinations of MOSTfs_32 and all other patterns\n");

   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, MOSTfs_32, ALL5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, MOSTfs_32, 0, ALL5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, ALL5s_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, MOSTfs_32, MOST5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, MOSTfs_32, 0, MOST5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, MOST5s_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, MOSTfs_32, ALLas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, MOSTfs_32, 0, ALLas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, ALLas_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, MOSTfs_32, MOSTas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, MOSTfs_32, 0, MOSTas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, MOSTas_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, MOSTfs_32, ALLfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, MOSTfs_32, 0, ALLfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, ALLfs_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, MOSTfs_32, MOSTfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, MOSTfs_32, 0, MOSTfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, MOSTfs_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, MOSTfs_32, UP_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, MOSTfs_32, 0, UP_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, UP_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, MOSTfs_32, DOWN_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, MOSTfs_32, 0, DOWN_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, DOWN_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, MOSTfs_32, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, MOSTfs_32, 0, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, 0, 0xFFFFFFFF);

   printf("Combinations of UP_32 and all other patterns\n");

   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, UP_32, ALL5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, UP_32, 0, ALL5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, UP_32, UP_32, ALL5s_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, UP_32, MOST5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, UP_32, 0, MOST5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, UP_32, UP_32, MOST5s_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, UP_32, ALLas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, UP_32, 0, ALLas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, UP_32, UP_32, ALLas_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, UP_32, MOSTas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, UP_32, 0, MOSTas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, UP_32, UP_32, MOSTas_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, UP_32, ALLfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, UP_32, 0, ALLfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, UP_32, UP_32, ALLfs_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, UP_32, MOSTfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, UP_32, 0, MOSTfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, UP_32, UP_32, MOSTfs_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, UP_32, UP_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, UP_32, 0, UP_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, UP_32, UP_32, UP_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, UP_32, DOWN_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, UP_32, 0, DOWN_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, UP_32, UP_32, DOWN_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, UP_32, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, UP_32, 0, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, UP_32, UP_32, 0, 0xFFFFFFFF);

   printf("Combinations of DOWN_32 and all other patterns\n");

   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, DOWN_32, ALL5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, DOWN_32, 0, ALL5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, ALL5s_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, DOWN_32, MOST5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, DOWN_32, 0, MOST5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, MOST5s_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, DOWN_32, ALLas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, DOWN_32, 0, ALLas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, ALLas_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, DOWN_32, MOSTas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, DOWN_32, 0, MOSTas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, MOSTas_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, DOWN_32, ALLfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, DOWN_32, 0, ALLfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, ALLfs_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, DOWN_32, MOSTfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, DOWN_32, 0, MOSTfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, MOSTfs_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, DOWN_32, UP_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, DOWN_32, 0, UP_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, UP_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, DOWN_32, DOWN_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, DOWN_32, 0, DOWN_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, DOWN_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, 0, DOWN_32, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, DOWN_32, 0, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("cas w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, 0, 0xFFFFFFFF);

   printf("CASA <Ws>, <Wt>, [<Xn|SP>]\n\n");

   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, 0, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, 0, 1, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, 1, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, 1, 1, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 1, 0, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 1, 0, 1, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 1, 1, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 1, 1, 1, 0xFFFFFFFF);

   printf("Combinations of ALL5s_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, ALL5s_32, ALL5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, ALL5s_32, 0, ALL5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, ALL5s_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, ALL5s_32, MOST5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, ALL5s_32, 0, MOST5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, MOST5s_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, ALL5s_32, ALLas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, ALL5s_32, 0, ALLas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, ALLas_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, ALL5s_32, MOSTas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, ALL5s_32, 0, MOSTas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, MOSTas_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, ALL5s_32, ALLfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, ALL5s_32, 0, ALLfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, ALLfs_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, ALL5s_32, MOSTfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, ALL5s_32, 0, MOSTfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, MOSTfs_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, ALL5s_32, UP_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, ALL5s_32, 0, UP_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, UP_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, ALL5s_32, DOWN_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, ALL5s_32, 0, DOWN_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, DOWN_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, ALL5s_32, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, ALL5s_32, 0, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, 0, 0xFFFFFFFF);

   printf("Combinations of MOST5s_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, MOST5s_32, ALL5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, MOST5s_32, 0, ALL5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, ALL5s_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, MOST5s_32, MOST5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, MOST5s_32, 0, MOST5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, MOST5s_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, MOST5s_32, ALLas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, MOST5s_32, 0, ALLas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, ALLas_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, MOST5s_32, MOSTas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, MOST5s_32, 0, MOSTas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, MOSTas_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, MOST5s_32, ALLfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, MOST5s_32, 0, ALLfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, ALLfs_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, MOST5s_32, MOSTfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, MOST5s_32, 0, MOSTfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, MOSTfs_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, MOST5s_32, UP_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, MOST5s_32, 0, UP_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, UP_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, MOST5s_32, DOWN_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, MOST5s_32, 0, DOWN_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, DOWN_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, MOST5s_32, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, MOST5s_32, 0, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, 0, 0xFFFFFFFF);

   printf("Combinations of ALLas_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, ALLas_32, ALL5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, ALLas_32, 0, ALL5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, ALL5s_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, ALLas_32, MOST5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, ALLas_32, 0, MOST5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, MOST5s_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, ALLas_32, ALLas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, ALLas_32, 0, ALLas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, ALLas_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, ALLas_32, MOSTas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, ALLas_32, 0, MOSTas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, MOSTas_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, ALLas_32, ALLfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, ALLas_32, 0, ALLfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, ALLfs_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, ALLas_32, MOSTfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, ALLas_32, 0, MOSTfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, MOSTfs_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, ALLas_32, UP_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, ALLas_32, 0, UP_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, UP_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, ALLas_32, DOWN_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, ALLas_32, 0, DOWN_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, DOWN_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, ALLas_32, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, ALLas_32, 0, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, 0, 0xFFFFFFFF);

   printf("Combinations of MOSTas_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, MOSTas_32, ALL5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, MOSTas_32, 0, ALL5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, ALL5s_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, MOSTas_32, MOST5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, MOSTas_32, 0, MOST5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, MOST5s_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, MOSTas_32, ALLas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, MOSTas_32, 0, ALLas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, ALLas_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, MOSTas_32, MOSTas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, MOSTas_32, 0, MOSTas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, MOSTas_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, MOSTas_32, ALLfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, MOSTas_32, 0, ALLfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, ALLfs_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, MOSTas_32, MOSTfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, MOSTas_32, 0, MOSTfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, MOSTfs_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, MOSTas_32, UP_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, MOSTas_32, 0, UP_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, UP_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, MOSTas_32, DOWN_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, MOSTas_32, 0, DOWN_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, DOWN_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, MOSTas_32, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, MOSTas_32, 0, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, 0, 0xFFFFFFFF);

   printf("Combinations of ALLfs_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, ALLfs_32, ALL5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, ALLfs_32, 0, ALL5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, ALL5s_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, ALLfs_32, MOST5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, ALLfs_32, 0, MOST5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, MOST5s_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, ALLfs_32, ALLas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, ALLfs_32, 0, ALLas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, ALLas_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, ALLfs_32, MOSTas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, ALLfs_32, 0, MOSTas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, MOSTas_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, ALLfs_32, ALLfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, ALLfs_32, 0, ALLfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, ALLfs_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, ALLfs_32, MOSTfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, ALLfs_32, 0, MOSTfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, MOSTfs_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, ALLfs_32, UP_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, ALLfs_32, 0, UP_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, UP_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, ALLfs_32, DOWN_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, ALLfs_32, 0, DOWN_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, DOWN_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, ALLfs_32, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, ALLfs_32, 0, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, 0, 0xFFFFFFFF);

   printf("Combinations of MOSTfs_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, MOSTfs_32, ALL5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, MOSTfs_32, 0, ALL5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, ALL5s_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, MOSTfs_32, MOST5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, MOSTfs_32, 0, MOST5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, MOST5s_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, MOSTfs_32, ALLas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, MOSTfs_32, 0, ALLas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, ALLas_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, MOSTfs_32, MOSTas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, MOSTfs_32, 0, MOSTas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, MOSTas_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, MOSTfs_32, ALLfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, MOSTfs_32, 0, ALLfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, ALLfs_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, MOSTfs_32, MOSTfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, MOSTfs_32, 0, MOSTfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, MOSTfs_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, MOSTfs_32, UP_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, MOSTfs_32, 0, UP_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, UP_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, MOSTfs_32, DOWN_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, MOSTfs_32, 0, DOWN_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, DOWN_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, MOSTfs_32, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, MOSTfs_32, 0, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, 0, 0xFFFFFFFF);

   printf("Combinations of UP_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, UP_32, ALL5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, UP_32, 0, ALL5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, UP_32, UP_32, ALL5s_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, UP_32, MOST5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, UP_32, 0, MOST5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, UP_32, UP_32, MOST5s_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, UP_32, ALLas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, UP_32, 0, ALLas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, UP_32, UP_32, ALLas_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, UP_32, MOSTas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, UP_32, 0, MOSTas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, UP_32, UP_32, MOSTas_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, UP_32, ALLfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, UP_32, 0, ALLfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, UP_32, UP_32, ALLfs_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, UP_32, MOSTfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, UP_32, 0, MOSTfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, UP_32, UP_32, MOSTfs_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, UP_32, UP_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, UP_32, 0, UP_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, UP_32, UP_32, UP_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, UP_32, DOWN_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, UP_32, 0, DOWN_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, UP_32, UP_32, DOWN_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, UP_32, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, UP_32, 0, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, UP_32, UP_32, 0, 0xFFFFFFFF);

   printf("Combinations of DOWN_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, DOWN_32, ALL5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, DOWN_32, 0, ALL5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, ALL5s_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, DOWN_32, MOST5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, DOWN_32, 0, MOST5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, MOST5s_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, DOWN_32, ALLas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, DOWN_32, 0, ALLas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, ALLas_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, DOWN_32, MOSTas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, DOWN_32, 0, MOSTas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, MOSTas_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, DOWN_32, ALLfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, DOWN_32, 0, ALLfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, ALLfs_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, DOWN_32, MOSTfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, DOWN_32, 0, MOSTfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, MOSTfs_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, DOWN_32, UP_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, DOWN_32, 0, UP_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, UP_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, DOWN_32, DOWN_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, DOWN_32, 0, DOWN_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, DOWN_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, 0, DOWN_32, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, DOWN_32, 0, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casa w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, 0, 0xFFFFFFFF);

   printf("CASAL <Ws>, <Wt>, [<Xn|SP>]\n\n");

   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, 0, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, 0, 1, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, 1, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, 1, 1, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 1, 0, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 1, 0, 1, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 1, 1, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 1, 1, 1, 0xFFFFFFFF);

   printf("Combinations of ALL5s_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, ALL5s_32, ALL5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, ALL5s_32, 0, ALL5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, ALL5s_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, ALL5s_32, MOST5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, ALL5s_32, 0, MOST5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, MOST5s_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, ALL5s_32, ALLas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, ALL5s_32, 0, ALLas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, ALLas_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, ALL5s_32, MOSTas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, ALL5s_32, 0, MOSTas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, MOSTas_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, ALL5s_32, ALLfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, ALL5s_32, 0, ALLfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, ALLfs_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, ALL5s_32, MOSTfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, ALL5s_32, 0, MOSTfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, MOSTfs_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, ALL5s_32, UP_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, ALL5s_32, 0, UP_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, UP_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, ALL5s_32, DOWN_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, ALL5s_32, 0, DOWN_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, DOWN_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, ALL5s_32, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, ALL5s_32, 0, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, 0, 0xFFFFFFFF);

   printf("Combinations of MOST5s_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, MOST5s_32, ALL5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, MOST5s_32, 0, ALL5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, ALL5s_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, MOST5s_32, MOST5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, MOST5s_32, 0, MOST5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, MOST5s_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, MOST5s_32, ALLas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, MOST5s_32, 0, ALLas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, ALLas_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, MOST5s_32, MOSTas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, MOST5s_32, 0, MOSTas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, MOSTas_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, MOST5s_32, ALLfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, MOST5s_32, 0, ALLfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, ALLfs_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, MOST5s_32, MOSTfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, MOST5s_32, 0, MOSTfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, MOSTfs_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, MOST5s_32, UP_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, MOST5s_32, 0, UP_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, UP_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, MOST5s_32, DOWN_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, MOST5s_32, 0, DOWN_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, DOWN_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, MOST5s_32, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, MOST5s_32, 0, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, 0, 0xFFFFFFFF);

   printf("Combinations of ALLas_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, ALLas_32, ALL5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, ALLas_32, 0, ALL5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, ALL5s_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, ALLas_32, MOST5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, ALLas_32, 0, MOST5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, MOST5s_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, ALLas_32, ALLas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, ALLas_32, 0, ALLas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, ALLas_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, ALLas_32, MOSTas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, ALLas_32, 0, MOSTas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, MOSTas_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, ALLas_32, ALLfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, ALLas_32, 0, ALLfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, ALLfs_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, ALLas_32, MOSTfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, ALLas_32, 0, MOSTfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, MOSTfs_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, ALLas_32, UP_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, ALLas_32, 0, UP_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, UP_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, ALLas_32, DOWN_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, ALLas_32, 0, DOWN_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, DOWN_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, ALLas_32, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, ALLas_32, 0, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, 0, 0xFFFFFFFF);

   printf("Combinations of MOSTas_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, MOSTas_32, ALL5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, MOSTas_32, 0, ALL5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, ALL5s_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, MOSTas_32, MOST5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, MOSTas_32, 0, MOST5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, MOST5s_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, MOSTas_32, ALLas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, MOSTas_32, 0, ALLas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, ALLas_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, MOSTas_32, MOSTas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, MOSTas_32, 0, MOSTas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, MOSTas_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, MOSTas_32, ALLfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, MOSTas_32, 0, ALLfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, ALLfs_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, MOSTas_32, MOSTfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, MOSTas_32, 0, MOSTfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, MOSTfs_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, MOSTas_32, UP_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, MOSTas_32, 0, UP_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, UP_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, MOSTas_32, DOWN_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, MOSTas_32, 0, DOWN_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, DOWN_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, MOSTas_32, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, MOSTas_32, 0, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, 0, 0xFFFFFFFF);

   printf("Combinations of ALLfs_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, ALLfs_32, ALL5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, ALLfs_32, 0, ALL5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, ALL5s_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, ALLfs_32, MOST5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, ALLfs_32, 0, MOST5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, MOST5s_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, ALLfs_32, ALLas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, ALLfs_32, 0, ALLas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, ALLas_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, ALLfs_32, MOSTas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, ALLfs_32, 0, MOSTas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, MOSTas_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, ALLfs_32, ALLfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, ALLfs_32, 0, ALLfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, ALLfs_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, ALLfs_32, MOSTfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, ALLfs_32, 0, MOSTfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, MOSTfs_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, ALLfs_32, UP_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, ALLfs_32, 0, UP_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, UP_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, ALLfs_32, DOWN_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, ALLfs_32, 0, DOWN_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, DOWN_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, ALLfs_32, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, ALLfs_32, 0, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, 0, 0xFFFFFFFF);

   printf("Combinations of MOSTfs_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, MOSTfs_32, ALL5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, MOSTfs_32, 0, ALL5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, ALL5s_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, MOSTfs_32, MOST5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, MOSTfs_32, 0, MOST5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, MOST5s_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, MOSTfs_32, ALLas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, MOSTfs_32, 0, ALLas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, ALLas_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, MOSTfs_32, MOSTas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, MOSTfs_32, 0, MOSTas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, MOSTas_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, MOSTfs_32, ALLfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, MOSTfs_32, 0, ALLfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, ALLfs_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, MOSTfs_32, MOSTfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, MOSTfs_32, 0, MOSTfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, MOSTfs_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, MOSTfs_32, UP_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, MOSTfs_32, 0, UP_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, UP_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, MOSTfs_32, DOWN_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, MOSTfs_32, 0, DOWN_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, DOWN_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, MOSTfs_32, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, MOSTfs_32, 0, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, 0, 0xFFFFFFFF);

   printf("Combinations of UP_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, UP_32, ALL5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, UP_32, 0, ALL5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, UP_32, UP_32, ALL5s_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, UP_32, MOST5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, UP_32, 0, MOST5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, UP_32, UP_32, MOST5s_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, UP_32, ALLas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, UP_32, 0, ALLas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, UP_32, UP_32, ALLas_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, UP_32, MOSTas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, UP_32, 0, MOSTas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, UP_32, UP_32, MOSTas_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, UP_32, ALLfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, UP_32, 0, ALLfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, UP_32, UP_32, ALLfs_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, UP_32, MOSTfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, UP_32, 0, MOSTfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, UP_32, UP_32, MOSTfs_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, UP_32, UP_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, UP_32, 0, UP_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, UP_32, UP_32, UP_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, UP_32, DOWN_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, UP_32, 0, DOWN_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, UP_32, UP_32, DOWN_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, UP_32, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, UP_32, 0, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, UP_32, UP_32, 0, 0xFFFFFFFF);

   printf("Combinations of DOWN_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, DOWN_32, ALL5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, DOWN_32, 0, ALL5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, ALL5s_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, DOWN_32, MOST5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, DOWN_32, 0, MOST5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, MOST5s_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, DOWN_32, ALLas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, DOWN_32, 0, ALLas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, ALLas_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, DOWN_32, MOSTas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, DOWN_32, 0, MOSTas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, MOSTas_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, DOWN_32, ALLfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, DOWN_32, 0, ALLfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, ALLfs_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, DOWN_32, MOSTfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, DOWN_32, 0, MOSTfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, MOSTfs_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, DOWN_32, UP_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, DOWN_32, 0, UP_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, UP_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, DOWN_32, DOWN_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, DOWN_32, 0, DOWN_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, DOWN_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, 0, DOWN_32, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, DOWN_32, 0, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casal w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, 0, 0xFFFFFFFF);

   printf("CASL <Ws>, <Wt>, [<Xn|SP>]\n\n");

   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, 0, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, 0, 1, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, 1, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, 1, 1, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 1, 0, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 1, 0, 1, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 1, 1, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 1, 1, 1, 0xFFFFFFFF);

   printf("Combinations of ALL5s_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, ALL5s_32, ALL5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, ALL5s_32, 0, ALL5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, ALL5s_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, ALL5s_32, MOST5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, ALL5s_32, 0, MOST5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, MOST5s_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, ALL5s_32, ALLas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, ALL5s_32, 0, ALLas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, ALLas_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, ALL5s_32, MOSTas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, ALL5s_32, 0, MOSTas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, MOSTas_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, ALL5s_32, ALLfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, ALL5s_32, 0, ALLfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, ALLfs_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, ALL5s_32, MOSTfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, ALL5s_32, 0, MOSTfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, MOSTfs_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, ALL5s_32, UP_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, ALL5s_32, 0, UP_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, UP_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, ALL5s_32, DOWN_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, ALL5s_32, 0, DOWN_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, DOWN_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, ALL5s_32, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, ALL5s_32, 0, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, 0, 0xFFFFFFFF);

   printf("Combinations of MOST5s_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, MOST5s_32, ALL5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, MOST5s_32, 0, ALL5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, ALL5s_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, MOST5s_32, MOST5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, MOST5s_32, 0, MOST5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, MOST5s_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, MOST5s_32, ALLas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, MOST5s_32, 0, ALLas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, ALLas_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, MOST5s_32, MOSTas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, MOST5s_32, 0, MOSTas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, MOSTas_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, MOST5s_32, ALLfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, MOST5s_32, 0, ALLfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, ALLfs_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, MOST5s_32, MOSTfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, MOST5s_32, 0, MOSTfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, MOSTfs_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, MOST5s_32, UP_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, MOST5s_32, 0, UP_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, UP_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, MOST5s_32, DOWN_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, MOST5s_32, 0, DOWN_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, DOWN_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, MOST5s_32, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, MOST5s_32, 0, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, 0, 0xFFFFFFFF);

   printf("Combinations of ALLas_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, ALLas_32, ALL5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, ALLas_32, 0, ALL5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, ALL5s_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, ALLas_32, MOST5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, ALLas_32, 0, MOST5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, MOST5s_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, ALLas_32, ALLas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, ALLas_32, 0, ALLas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, ALLas_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, ALLas_32, MOSTas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, ALLas_32, 0, MOSTas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, MOSTas_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, ALLas_32, ALLfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, ALLas_32, 0, ALLfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, ALLfs_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, ALLas_32, MOSTfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, ALLas_32, 0, MOSTfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, MOSTfs_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, ALLas_32, UP_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, ALLas_32, 0, UP_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, UP_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, ALLas_32, DOWN_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, ALLas_32, 0, DOWN_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, DOWN_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, ALLas_32, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, ALLas_32, 0, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, 0, 0xFFFFFFFF);

   printf("Combinations of MOSTas_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, MOSTas_32, ALL5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, MOSTas_32, 0, ALL5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, ALL5s_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, MOSTas_32, MOST5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, MOSTas_32, 0, MOST5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, MOST5s_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, MOSTas_32, ALLas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, MOSTas_32, 0, ALLas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, ALLas_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, MOSTas_32, MOSTas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, MOSTas_32, 0, MOSTas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, MOSTas_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, MOSTas_32, ALLfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, MOSTas_32, 0, ALLfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, ALLfs_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, MOSTas_32, MOSTfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, MOSTas_32, 0, MOSTfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, MOSTfs_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, MOSTas_32, UP_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, MOSTas_32, 0, UP_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, UP_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, MOSTas_32, DOWN_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, MOSTas_32, 0, DOWN_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, DOWN_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, MOSTas_32, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, MOSTas_32, 0, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, 0, 0xFFFFFFFF);

   printf("Combinations of ALLfs_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, ALLfs_32, ALL5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, ALLfs_32, 0, ALL5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, ALL5s_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, ALLfs_32, MOST5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, ALLfs_32, 0, MOST5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, MOST5s_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, ALLfs_32, ALLas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, ALLfs_32, 0, ALLas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, ALLas_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, ALLfs_32, MOSTas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, ALLfs_32, 0, MOSTas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, MOSTas_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, ALLfs_32, ALLfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, ALLfs_32, 0, ALLfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, ALLfs_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, ALLfs_32, MOSTfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, ALLfs_32, 0, MOSTfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, MOSTfs_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, ALLfs_32, UP_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, ALLfs_32, 0, UP_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, UP_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, ALLfs_32, DOWN_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, ALLfs_32, 0, DOWN_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, DOWN_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, ALLfs_32, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, ALLfs_32, 0, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, 0, 0xFFFFFFFF);

   printf("Combinations of MOSTfs_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, MOSTfs_32, ALL5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, MOSTfs_32, 0, ALL5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, ALL5s_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, MOSTfs_32, MOST5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, MOSTfs_32, 0, MOST5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, MOST5s_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, MOSTfs_32, ALLas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, MOSTfs_32, 0, ALLas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, ALLas_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, MOSTfs_32, MOSTas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, MOSTfs_32, 0, MOSTas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, MOSTas_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, MOSTfs_32, ALLfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, MOSTfs_32, 0, ALLfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, ALLfs_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, MOSTfs_32, MOSTfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, MOSTfs_32, 0, MOSTfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, MOSTfs_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, MOSTfs_32, UP_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, MOSTfs_32, 0, UP_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, UP_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, MOSTfs_32, DOWN_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, MOSTfs_32, 0, DOWN_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, DOWN_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, MOSTfs_32, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, MOSTfs_32, 0, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, 0, 0xFFFFFFFF);

   printf("Combinations of UP_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, UP_32, ALL5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, UP_32, 0, ALL5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, UP_32, UP_32, ALL5s_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, UP_32, MOST5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, UP_32, 0, MOST5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, UP_32, UP_32, MOST5s_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, UP_32, ALLas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, UP_32, 0, ALLas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, UP_32, UP_32, ALLas_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, UP_32, MOSTas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, UP_32, 0, MOSTas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, UP_32, UP_32, MOSTas_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, UP_32, ALLfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, UP_32, 0, ALLfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, UP_32, UP_32, ALLfs_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, UP_32, MOSTfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, UP_32, 0, MOSTfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, UP_32, UP_32, MOSTfs_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, UP_32, UP_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, UP_32, 0, UP_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, UP_32, UP_32, UP_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, UP_32, DOWN_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, UP_32, 0, DOWN_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, UP_32, UP_32, DOWN_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, UP_32, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, UP_32, 0, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, UP_32, UP_32, 0, 0xFFFFFFFF);

   printf("Combinations of DOWN_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, DOWN_32, ALL5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, DOWN_32, 0, ALL5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, ALL5s_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, DOWN_32, MOST5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, DOWN_32, 0, MOST5s_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, MOST5s_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, DOWN_32, ALLas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, DOWN_32, 0, ALLas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, ALLas_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, DOWN_32, MOSTas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, DOWN_32, 0, MOSTas_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, MOSTas_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, DOWN_32, ALLfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, DOWN_32, 0, ALLfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, ALLfs_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, DOWN_32, MOSTfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, DOWN_32, 0, MOSTfs_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, MOSTfs_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, DOWN_32, UP_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, DOWN_32, 0, UP_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, UP_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, DOWN_32, DOWN_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, DOWN_32, 0, DOWN_32, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, DOWN_32, 0xFFFFFFFF);

   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, 0, DOWN_32, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, DOWN_32, 0, 0, 0xFFFFFFFF);
   ATOMIC_TEST_CAS("casl w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, 0, 0xFFFFFFFF);

   printf("CAS <Xs>, <Xt>, [<Xn|SP>]\n\n");

   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, 0, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, 0, 1, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, 1, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, 1, 1, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 1, 0, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 1, 0, 1, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 1, 1, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 1, 1, 1, 0xFFFFFFFFFFFFFFFF);

   printf("Combinations of ALL5s_32 and all other patterns\n");

   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, ALL5s_32, ALL5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, ALL5s_32, 0, ALL5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, ALL5s_32, ALL5s_32, ALL5s_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, ALL5s_32, MOST5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, ALL5s_32, 0, MOST5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, ALL5s_32, ALL5s_32, MOST5s_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, ALL5s_32, ALLas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, ALL5s_32, 0, ALLas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, ALL5s_32, ALL5s_32, ALLas_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, ALL5s_32, MOSTas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, ALL5s_32, 0, MOSTas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, ALL5s_32, ALL5s_32, MOSTas_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, ALL5s_32, ALLfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, ALL5s_32, 0, ALLfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, ALL5s_32, ALL5s_32, ALLfs_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, ALL5s_32, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, ALL5s_32, 0, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, ALL5s_32, ALL5s_32, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, ALL5s_32, UP_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, ALL5s_32, 0, UP_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, ALL5s_32, ALL5s_32, UP_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, ALL5s_32, DOWN_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, ALL5s_32, 0, DOWN_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, ALL5s_32, ALL5s_32, DOWN_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, ALL5s_32, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, ALL5s_32, 0, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, ALL5s_32, ALL5s_32, 0, 0xFFFFFFFFFFFFFFFF);

   printf("Combinations of MOST5s_32 and all other patterns\n");

   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, MOST5s_32, ALL5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, MOST5s_32, 0, ALL5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, MOST5s_32, MOST5s_32, ALL5s_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, MOST5s_32, MOST5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, MOST5s_32, 0, MOST5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, MOST5s_32, MOST5s_32, MOST5s_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, MOST5s_32, ALLas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, MOST5s_32, 0, ALLas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, MOST5s_32, MOST5s_32, ALLas_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, MOST5s_32, MOSTas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, MOST5s_32, 0, MOSTas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, MOST5s_32, MOST5s_32, MOSTas_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, MOST5s_32, ALLfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, MOST5s_32, 0, ALLfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, MOST5s_32, MOST5s_32, ALLfs_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, MOST5s_32, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, MOST5s_32, 0, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, MOST5s_32, MOST5s_32, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, MOST5s_32, UP_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, MOST5s_32, 0, UP_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, MOST5s_32, MOST5s_32, UP_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, MOST5s_32, DOWN_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, MOST5s_32, 0, DOWN_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, MOST5s_32, MOST5s_32, DOWN_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, MOST5s_32, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, MOST5s_32, 0, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, MOST5s_32, MOST5s_32, 0, 0xFFFFFFFFFFFFFFFF);

   printf("Combinations of ALLas_32 and all other patterns\n");

   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, ALLas_32, ALL5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, ALLas_32, 0, ALL5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, ALLas_32, ALLas_32, ALL5s_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, ALLas_32, MOST5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, ALLas_32, 0, MOST5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, ALLas_32, ALLas_32, MOST5s_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, ALLas_32, ALLas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, ALLas_32, 0, ALLas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, ALLas_32, ALLas_32, ALLas_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, ALLas_32, MOSTas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, ALLas_32, 0, MOSTas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, ALLas_32, ALLas_32, MOSTas_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, ALLas_32, ALLfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, ALLas_32, 0, ALLfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, ALLas_32, ALLas_32, ALLfs_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, ALLas_32, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, ALLas_32, 0, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, ALLas_32, ALLas_32, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, ALLas_32, UP_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, ALLas_32, 0, UP_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, ALLas_32, ALLas_32, UP_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, ALLas_32, DOWN_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, ALLas_32, 0, DOWN_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, ALLas_32, ALLas_32, DOWN_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, ALLas_32, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, ALLas_32, 0, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, ALLas_32, ALLas_32, 0, 0xFFFFFFFFFFFFFFFF);

   printf("Combinations of MOSTas_32 and all other patterns\n");

   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, MOSTas_32, ALL5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, MOSTas_32, 0, ALL5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, MOSTas_32, MOSTas_32, ALL5s_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, MOSTas_32, MOST5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, MOSTas_32, 0, MOST5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, MOSTas_32, MOSTas_32, MOST5s_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, MOSTas_32, ALLas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, MOSTas_32, 0, ALLas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, MOSTas_32, MOSTas_32, ALLas_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, MOSTas_32, MOSTas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, MOSTas_32, 0, MOSTas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, MOSTas_32, MOSTas_32, MOSTas_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, MOSTas_32, ALLfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, MOSTas_32, 0, ALLfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, MOSTas_32, MOSTas_32, ALLfs_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, MOSTas_32, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, MOSTas_32, 0, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, MOSTas_32, MOSTas_32, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, MOSTas_32, UP_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, MOSTas_32, 0, UP_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, MOSTas_32, MOSTas_32, UP_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, MOSTas_32, DOWN_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, MOSTas_32, 0, DOWN_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, MOSTas_32, MOSTas_32, DOWN_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, MOSTas_32, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, MOSTas_32, 0, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, MOSTas_32, MOSTas_32, 0, 0xFFFFFFFFFFFFFFFF);

   printf("Combinations of ALLfs_32 and all other patterns\n");

   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, ALLfs_32, ALL5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, ALLfs_32, 0, ALL5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, ALLfs_32, ALLfs_32, ALL5s_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, ALLfs_32, MOST5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, ALLfs_32, 0, MOST5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, ALLfs_32, ALLfs_32, MOST5s_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, ALLfs_32, ALLas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, ALLfs_32, 0, ALLas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, ALLfs_32, ALLfs_32, ALLas_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, ALLfs_32, MOSTas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, ALLfs_32, 0, MOSTas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, ALLfs_32, ALLfs_32, MOSTas_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, ALLfs_32, ALLfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, ALLfs_32, 0, ALLfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, ALLfs_32, ALLfs_32, ALLfs_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, ALLfs_32, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, ALLfs_32, 0, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, ALLfs_32, ALLfs_32, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, ALLfs_32, UP_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, ALLfs_32, 0, UP_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, ALLfs_32, ALLfs_32, UP_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, ALLfs_32, DOWN_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, ALLfs_32, 0, DOWN_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, ALLfs_32, ALLfs_32, DOWN_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, ALLfs_32, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, ALLfs_32, 0, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, ALLfs_32, ALLfs_32, 0, 0xFFFFFFFFFFFFFFFF);

   printf("Combinations of MOSTfs_32 and all other patterns\n");

   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, MOSTfs_32, ALL5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, MOSTfs_32, 0, ALL5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, MOSTfs_32, MOSTfs_32, ALL5s_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, MOSTfs_32, MOST5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, MOSTfs_32, 0, MOST5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, MOSTfs_32, MOSTfs_32, MOST5s_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, MOSTfs_32, ALLas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, MOSTfs_32, 0, ALLas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, MOSTfs_32, MOSTfs_32, ALLas_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, MOSTfs_32, MOSTas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, MOSTfs_32, 0, MOSTas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, MOSTfs_32, MOSTfs_32, MOSTas_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, MOSTfs_32, ALLfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, MOSTfs_32, 0, ALLfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, MOSTfs_32, MOSTfs_32, ALLfs_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, MOSTfs_32, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, MOSTfs_32, 0, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, MOSTfs_32, MOSTfs_32, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, MOSTfs_32, UP_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, MOSTfs_32, 0, UP_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, MOSTfs_32, MOSTfs_32, UP_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, MOSTfs_32, DOWN_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, MOSTfs_32, 0, DOWN_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, MOSTfs_32, MOSTfs_32, DOWN_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, MOSTfs_32, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, MOSTfs_32, 0, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, MOSTfs_32, MOSTfs_32, 0, 0xFFFFFFFFFFFFFFFF);

   printf("Combinations of UP_32 and all other patterns\n");

   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, UP_32, ALL5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, UP_32, 0, ALL5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, UP_32, UP_32, ALL5s_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, UP_32, MOST5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, UP_32, 0, MOST5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, UP_32, UP_32, MOST5s_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, UP_32, ALLas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, UP_32, 0, ALLas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, UP_32, UP_32, ALLas_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, UP_32, MOSTas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, UP_32, 0, MOSTas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, UP_32, UP_32, MOSTas_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, UP_32, ALLfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, UP_32, 0, ALLfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, UP_32, UP_32, ALLfs_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, UP_32, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, UP_32, 0, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, UP_32, UP_32, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, UP_32, UP_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, UP_32, 0, UP_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, UP_32, UP_32, UP_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, UP_32, DOWN_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, UP_32, 0, DOWN_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, UP_32, UP_32, DOWN_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, UP_32, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, UP_32, 0, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, UP_32, UP_32, 0, 0xFFFFFFFFFFFFFFFF);

   printf("Combinations of DOWN_32 and all other patterns\n");

   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, DOWN_32, ALL5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, DOWN_32, 0, ALL5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, DOWN_32, DOWN_32, ALL5s_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, DOWN_32, MOST5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, DOWN_32, 0, MOST5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, DOWN_32, DOWN_32, MOST5s_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, DOWN_32, ALLas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, DOWN_32, 0, ALLas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, DOWN_32, DOWN_32, ALLas_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, DOWN_32, MOSTas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, DOWN_32, 0, MOSTas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, DOWN_32, DOWN_32, MOSTas_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, DOWN_32, ALLfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, DOWN_32, 0, ALLfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, DOWN_32, DOWN_32, ALLfs_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, DOWN_32, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, DOWN_32, 0, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, DOWN_32, DOWN_32, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, DOWN_32, UP_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, DOWN_32, 0, UP_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, DOWN_32, DOWN_32, UP_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, DOWN_32, DOWN_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, DOWN_32, 0, DOWN_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, DOWN_32, DOWN_32, DOWN_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, 0, DOWN_32, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, DOWN_32, 0, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("cas x11, x12, [x5]", x, mem, DOWN_32, DOWN_32, 0, 0xFFFFFFFFFFFFFFFF);

   printf("CASA <Xs>, <Xt>, [<Xn|SP>]\n\n");

   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, 0, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, 0, 1, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, 1, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, 1, 1, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 1, 0, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 1, 0, 1, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 1, 1, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 1, 1, 1, 0xFFFFFFFFFFFFFFFF);

   printf("Combinations of ALL5s_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, ALL5s_32, ALL5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, ALL5s_32, 0, ALL5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, ALL5s_32, ALL5s_32, ALL5s_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, ALL5s_32, MOST5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, ALL5s_32, 0, MOST5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, ALL5s_32, ALL5s_32, MOST5s_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, ALL5s_32, ALLas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, ALL5s_32, 0, ALLas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, ALL5s_32, ALL5s_32, ALLas_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, ALL5s_32, MOSTas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, ALL5s_32, 0, MOSTas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, ALL5s_32, ALL5s_32, MOSTas_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, ALL5s_32, ALLfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, ALL5s_32, 0, ALLfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, ALL5s_32, ALL5s_32, ALLfs_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, ALL5s_32, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, ALL5s_32, 0, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, ALL5s_32, ALL5s_32, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, ALL5s_32, UP_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, ALL5s_32, 0, UP_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, ALL5s_32, ALL5s_32, UP_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, ALL5s_32, DOWN_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, ALL5s_32, 0, DOWN_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, ALL5s_32, ALL5s_32, DOWN_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, ALL5s_32, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, ALL5s_32, 0, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, ALL5s_32, ALL5s_32, 0, 0xFFFFFFFFFFFFFFFF);

   printf("Combinations of MOST5s_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, MOST5s_32, ALL5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, MOST5s_32, 0, ALL5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, MOST5s_32, MOST5s_32, ALL5s_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, MOST5s_32, MOST5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, MOST5s_32, 0, MOST5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, MOST5s_32, MOST5s_32, MOST5s_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, MOST5s_32, ALLas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, MOST5s_32, 0, ALLas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, MOST5s_32, MOST5s_32, ALLas_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, MOST5s_32, MOSTas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, MOST5s_32, 0, MOSTas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, MOST5s_32, MOST5s_32, MOSTas_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, MOST5s_32, ALLfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, MOST5s_32, 0, ALLfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, MOST5s_32, MOST5s_32, ALLfs_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, MOST5s_32, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, MOST5s_32, 0, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, MOST5s_32, MOST5s_32, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, MOST5s_32, UP_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, MOST5s_32, 0, UP_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, MOST5s_32, MOST5s_32, UP_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, MOST5s_32, DOWN_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, MOST5s_32, 0, DOWN_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, MOST5s_32, MOST5s_32, DOWN_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, MOST5s_32, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, MOST5s_32, 0, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, MOST5s_32, MOST5s_32, 0, 0xFFFFFFFFFFFFFFFF);

   printf("Combinations of ALLas_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, ALLas_32, ALL5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, ALLas_32, 0, ALL5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, ALLas_32, ALLas_32, ALL5s_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, ALLas_32, MOST5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, ALLas_32, 0, MOST5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, ALLas_32, ALLas_32, MOST5s_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, ALLas_32, ALLas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, ALLas_32, 0, ALLas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, ALLas_32, ALLas_32, ALLas_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, ALLas_32, MOSTas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, ALLas_32, 0, MOSTas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, ALLas_32, ALLas_32, MOSTas_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, ALLas_32, ALLfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, ALLas_32, 0, ALLfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, ALLas_32, ALLas_32, ALLfs_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, ALLas_32, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, ALLas_32, 0, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, ALLas_32, ALLas_32, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, ALLas_32, UP_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, ALLas_32, 0, UP_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, ALLas_32, ALLas_32, UP_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, ALLas_32, DOWN_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, ALLas_32, 0, DOWN_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, ALLas_32, ALLas_32, DOWN_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, ALLas_32, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, ALLas_32, 0, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, ALLas_32, ALLas_32, 0, 0xFFFFFFFFFFFFFFFF);

   printf("Combinations of MOSTas_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, MOSTas_32, ALL5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, MOSTas_32, 0, ALL5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, MOSTas_32, MOSTas_32, ALL5s_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, MOSTas_32, MOST5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, MOSTas_32, 0, MOST5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, MOSTas_32, MOSTas_32, MOST5s_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, MOSTas_32, ALLas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, MOSTas_32, 0, ALLas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, MOSTas_32, MOSTas_32, ALLas_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, MOSTas_32, MOSTas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, MOSTas_32, 0, MOSTas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, MOSTas_32, MOSTas_32, MOSTas_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, MOSTas_32, ALLfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, MOSTas_32, 0, ALLfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, MOSTas_32, MOSTas_32, ALLfs_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, MOSTas_32, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, MOSTas_32, 0, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, MOSTas_32, MOSTas_32, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, MOSTas_32, UP_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, MOSTas_32, 0, UP_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, MOSTas_32, MOSTas_32, UP_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, MOSTas_32, DOWN_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, MOSTas_32, 0, DOWN_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, MOSTas_32, MOSTas_32, DOWN_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, MOSTas_32, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, MOSTas_32, 0, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, MOSTas_32, MOSTas_32, 0, 0xFFFFFFFFFFFFFFFF);

   printf("Combinations of ALLfs_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, ALLfs_32, ALL5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, ALLfs_32, 0, ALL5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, ALLfs_32, ALLfs_32, ALL5s_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, ALLfs_32, MOST5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, ALLfs_32, 0, MOST5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, ALLfs_32, ALLfs_32, MOST5s_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, ALLfs_32, ALLas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, ALLfs_32, 0, ALLas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, ALLfs_32, ALLfs_32, ALLas_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, ALLfs_32, MOSTas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, ALLfs_32, 0, MOSTas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, ALLfs_32, ALLfs_32, MOSTas_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, ALLfs_32, ALLfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, ALLfs_32, 0, ALLfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, ALLfs_32, ALLfs_32, ALLfs_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, ALLfs_32, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, ALLfs_32, 0, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, ALLfs_32, ALLfs_32, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, ALLfs_32, UP_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, ALLfs_32, 0, UP_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, ALLfs_32, ALLfs_32, UP_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, ALLfs_32, DOWN_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, ALLfs_32, 0, DOWN_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, ALLfs_32, ALLfs_32, DOWN_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, ALLfs_32, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, ALLfs_32, 0, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, ALLfs_32, ALLfs_32, 0, 0xFFFFFFFFFFFFFFFF);

   printf("Combinations of MOSTfs_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, MOSTfs_32, ALL5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, MOSTfs_32, 0, ALL5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, MOSTfs_32, MOSTfs_32, ALL5s_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, MOSTfs_32, MOST5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, MOSTfs_32, 0, MOST5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, MOSTfs_32, MOSTfs_32, MOST5s_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, MOSTfs_32, ALLas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, MOSTfs_32, 0, ALLas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, MOSTfs_32, MOSTfs_32, ALLas_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, MOSTfs_32, MOSTas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, MOSTfs_32, 0, MOSTas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, MOSTfs_32, MOSTfs_32, MOSTas_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, MOSTfs_32, ALLfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, MOSTfs_32, 0, ALLfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, MOSTfs_32, MOSTfs_32, ALLfs_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, MOSTfs_32, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, MOSTfs_32, 0, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, MOSTfs_32, MOSTfs_32, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, MOSTfs_32, UP_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, MOSTfs_32, 0, UP_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, MOSTfs_32, MOSTfs_32, UP_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, MOSTfs_32, DOWN_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, MOSTfs_32, 0, DOWN_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, MOSTfs_32, MOSTfs_32, DOWN_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, MOSTfs_32, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, MOSTfs_32, 0, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, MOSTfs_32, MOSTfs_32, 0, 0xFFFFFFFFFFFFFFFF);

   printf("Combinations of UP_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, UP_32, ALL5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, UP_32, 0, ALL5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, UP_32, UP_32, ALL5s_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, UP_32, MOST5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, UP_32, 0, MOST5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, UP_32, UP_32, MOST5s_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, UP_32, ALLas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, UP_32, 0, ALLas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, UP_32, UP_32, ALLas_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, UP_32, MOSTas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, UP_32, 0, MOSTas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, UP_32, UP_32, MOSTas_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, UP_32, ALLfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, UP_32, 0, ALLfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, UP_32, UP_32, ALLfs_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, UP_32, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, UP_32, 0, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, UP_32, UP_32, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, UP_32, UP_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, UP_32, 0, UP_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, UP_32, UP_32, UP_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, UP_32, DOWN_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, UP_32, 0, DOWN_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, UP_32, UP_32, DOWN_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, UP_32, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, UP_32, 0, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, UP_32, UP_32, 0, 0xFFFFFFFFFFFFFFFF);

   printf("Combinations of DOWN_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, DOWN_32, ALL5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, DOWN_32, 0, ALL5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, DOWN_32, DOWN_32, ALL5s_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, DOWN_32, MOST5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, DOWN_32, 0, MOST5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, DOWN_32, DOWN_32, MOST5s_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, DOWN_32, ALLas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, DOWN_32, 0, ALLas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, DOWN_32, DOWN_32, ALLas_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, DOWN_32, MOSTas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, DOWN_32, 0, MOSTas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, DOWN_32, DOWN_32, MOSTas_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, DOWN_32, ALLfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, DOWN_32, 0, ALLfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, DOWN_32, DOWN_32, ALLfs_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, DOWN_32, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, DOWN_32, 0, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, DOWN_32, DOWN_32, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, DOWN_32, UP_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, DOWN_32, 0, UP_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, DOWN_32, DOWN_32, UP_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, DOWN_32, DOWN_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, DOWN_32, 0, DOWN_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, DOWN_32, DOWN_32, DOWN_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, 0, DOWN_32, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, DOWN_32, 0, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casa x11, x12, [x5]", x, mem, DOWN_32, DOWN_32, 0, 0xFFFFFFFFFFFFFFFF);

   printf("CASAL <Xs>, <Xt>, [<Xn|SP>]\n\n");

   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, 0, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, 0, 1, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, 1, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, 1, 1, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 1, 0, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 1, 0, 1, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 1, 1, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 1, 1, 1, 0xFFFFFFFFFFFFFFFF);

   printf("Combinations of ALL5s_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, ALL5s_32, ALL5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, ALL5s_32, 0, ALL5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, ALL5s_32, ALL5s_32, ALL5s_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, ALL5s_32, MOST5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, ALL5s_32, 0, MOST5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, ALL5s_32, ALL5s_32, MOST5s_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, ALL5s_32, ALLas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, ALL5s_32, 0, ALLas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, ALL5s_32, ALL5s_32, ALLas_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, ALL5s_32, MOSTas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, ALL5s_32, 0, MOSTas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, ALL5s_32, ALL5s_32, MOSTas_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, ALL5s_32, ALLfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, ALL5s_32, 0, ALLfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, ALL5s_32, ALL5s_32, ALLfs_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, ALL5s_32, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, ALL5s_32, 0, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, ALL5s_32, ALL5s_32, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, ALL5s_32, UP_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, ALL5s_32, 0, UP_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, ALL5s_32, ALL5s_32, UP_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, ALL5s_32, DOWN_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, ALL5s_32, 0, DOWN_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, ALL5s_32, ALL5s_32, DOWN_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, ALL5s_32, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, ALL5s_32, 0, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, ALL5s_32, ALL5s_32, 0, 0xFFFFFFFFFFFFFFFF);

   printf("Combinations of MOST5s_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, MOST5s_32, ALL5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, MOST5s_32, 0, ALL5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, MOST5s_32, MOST5s_32, ALL5s_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, MOST5s_32, MOST5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, MOST5s_32, 0, MOST5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, MOST5s_32, MOST5s_32, MOST5s_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, MOST5s_32, ALLas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, MOST5s_32, 0, ALLas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, MOST5s_32, MOST5s_32, ALLas_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, MOST5s_32, MOSTas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, MOST5s_32, 0, MOSTas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, MOST5s_32, MOST5s_32, MOSTas_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, MOST5s_32, ALLfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, MOST5s_32, 0, ALLfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, MOST5s_32, MOST5s_32, ALLfs_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, MOST5s_32, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, MOST5s_32, 0, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, MOST5s_32, MOST5s_32, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, MOST5s_32, UP_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, MOST5s_32, 0, UP_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, MOST5s_32, MOST5s_32, UP_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, MOST5s_32, DOWN_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, MOST5s_32, 0, DOWN_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, MOST5s_32, MOST5s_32, DOWN_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, MOST5s_32, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, MOST5s_32, 0, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, MOST5s_32, MOST5s_32, 0, 0xFFFFFFFFFFFFFFFF);

   printf("Combinations of ALLas_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, ALLas_32, ALL5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, ALLas_32, 0, ALL5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, ALLas_32, ALLas_32, ALL5s_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, ALLas_32, MOST5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, ALLas_32, 0, MOST5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, ALLas_32, ALLas_32, MOST5s_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, ALLas_32, ALLas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, ALLas_32, 0, ALLas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, ALLas_32, ALLas_32, ALLas_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, ALLas_32, MOSTas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, ALLas_32, 0, MOSTas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, ALLas_32, ALLas_32, MOSTas_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, ALLas_32, ALLfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, ALLas_32, 0, ALLfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, ALLas_32, ALLas_32, ALLfs_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, ALLas_32, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, ALLas_32, 0, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, ALLas_32, ALLas_32, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, ALLas_32, UP_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, ALLas_32, 0, UP_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, ALLas_32, ALLas_32, UP_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, ALLas_32, DOWN_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, ALLas_32, 0, DOWN_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, ALLas_32, ALLas_32, DOWN_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, ALLas_32, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, ALLas_32, 0, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, ALLas_32, ALLas_32, 0, 0xFFFFFFFFFFFFFFFF);

   printf("Combinations of MOSTas_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, MOSTas_32, ALL5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, MOSTas_32, 0, ALL5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, MOSTas_32, MOSTas_32, ALL5s_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, MOSTas_32, MOST5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, MOSTas_32, 0, MOST5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, MOSTas_32, MOSTas_32, MOST5s_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, MOSTas_32, ALLas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, MOSTas_32, 0, ALLas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, MOSTas_32, MOSTas_32, ALLas_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, MOSTas_32, MOSTas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, MOSTas_32, 0, MOSTas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, MOSTas_32, MOSTas_32, MOSTas_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, MOSTas_32, ALLfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, MOSTas_32, 0, ALLfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, MOSTas_32, MOSTas_32, ALLfs_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, MOSTas_32, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, MOSTas_32, 0, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, MOSTas_32, MOSTas_32, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, MOSTas_32, UP_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, MOSTas_32, 0, UP_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, MOSTas_32, MOSTas_32, UP_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, MOSTas_32, DOWN_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, MOSTas_32, 0, DOWN_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, MOSTas_32, MOSTas_32, DOWN_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, MOSTas_32, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, MOSTas_32, 0, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, MOSTas_32, MOSTas_32, 0, 0xFFFFFFFFFFFFFFFF);

   printf("Combinations of ALLfs_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, ALLfs_32, ALL5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, ALLfs_32, 0, ALL5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, ALLfs_32, ALLfs_32, ALL5s_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, ALLfs_32, MOST5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, ALLfs_32, 0, MOST5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, ALLfs_32, ALLfs_32, MOST5s_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, ALLfs_32, ALLas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, ALLfs_32, 0, ALLas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, ALLfs_32, ALLfs_32, ALLas_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, ALLfs_32, MOSTas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, ALLfs_32, 0, MOSTas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, ALLfs_32, ALLfs_32, MOSTas_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, ALLfs_32, ALLfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, ALLfs_32, 0, ALLfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, ALLfs_32, ALLfs_32, ALLfs_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, ALLfs_32, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, ALLfs_32, 0, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, ALLfs_32, ALLfs_32, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, ALLfs_32, UP_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, ALLfs_32, 0, UP_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, ALLfs_32, ALLfs_32, UP_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, ALLfs_32, DOWN_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, ALLfs_32, 0, DOWN_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, ALLfs_32, ALLfs_32, DOWN_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, ALLfs_32, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, ALLfs_32, 0, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, ALLfs_32, ALLfs_32, 0, 0xFFFFFFFFFFFFFFFF);

   printf("Combinations of MOSTfs_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, MOSTfs_32, ALL5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, MOSTfs_32, 0, ALL5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, MOSTfs_32, MOSTfs_32, ALL5s_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, MOSTfs_32, MOST5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, MOSTfs_32, 0, MOST5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, MOSTfs_32, MOSTfs_32, MOST5s_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, MOSTfs_32, ALLas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, MOSTfs_32, 0, ALLas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, MOSTfs_32, MOSTfs_32, ALLas_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, MOSTfs_32, MOSTas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, MOSTfs_32, 0, MOSTas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, MOSTfs_32, MOSTfs_32, MOSTas_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, MOSTfs_32, ALLfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, MOSTfs_32, 0, ALLfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, MOSTfs_32, MOSTfs_32, ALLfs_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, MOSTfs_32, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, MOSTfs_32, 0, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, MOSTfs_32, MOSTfs_32, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, MOSTfs_32, UP_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, MOSTfs_32, 0, UP_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, MOSTfs_32, MOSTfs_32, UP_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, MOSTfs_32, DOWN_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, MOSTfs_32, 0, DOWN_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, MOSTfs_32, MOSTfs_32, DOWN_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, MOSTfs_32, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, MOSTfs_32, 0, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, MOSTfs_32, MOSTfs_32, 0, 0xFFFFFFFFFFFFFFFF);

   printf("Combinations of UP_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, UP_32, ALL5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, UP_32, 0, ALL5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, UP_32, UP_32, ALL5s_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, UP_32, MOST5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, UP_32, 0, MOST5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, UP_32, UP_32, MOST5s_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, UP_32, ALLas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, UP_32, 0, ALLas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, UP_32, UP_32, ALLas_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, UP_32, MOSTas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, UP_32, 0, MOSTas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, UP_32, UP_32, MOSTas_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, UP_32, ALLfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, UP_32, 0, ALLfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, UP_32, UP_32, ALLfs_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, UP_32, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, UP_32, 0, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, UP_32, UP_32, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, UP_32, UP_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, UP_32, 0, UP_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, UP_32, UP_32, UP_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, UP_32, DOWN_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, UP_32, 0, DOWN_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, UP_32, UP_32, DOWN_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, UP_32, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, UP_32, 0, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, UP_32, UP_32, 0, 0xFFFFFFFFFFFFFFFF);

   printf("Combinations of DOWN_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, DOWN_32, ALL5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, DOWN_32, 0, ALL5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, DOWN_32, DOWN_32, ALL5s_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, DOWN_32, MOST5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, DOWN_32, 0, MOST5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, DOWN_32, DOWN_32, MOST5s_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, DOWN_32, ALLas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, DOWN_32, 0, ALLas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, DOWN_32, DOWN_32, ALLas_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, DOWN_32, MOSTas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, DOWN_32, 0, MOSTas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, DOWN_32, DOWN_32, MOSTas_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, DOWN_32, ALLfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, DOWN_32, 0, ALLfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, DOWN_32, DOWN_32, ALLfs_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, DOWN_32, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, DOWN_32, 0, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, DOWN_32, DOWN_32, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, DOWN_32, UP_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, DOWN_32, 0, UP_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, DOWN_32, DOWN_32, UP_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, DOWN_32, DOWN_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, DOWN_32, 0, DOWN_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, DOWN_32, DOWN_32, DOWN_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, 0, DOWN_32, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, DOWN_32, 0, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casal x11, x12, [x5]", x, mem, DOWN_32, DOWN_32, 0, 0xFFFFFFFFFFFFFFFF);

   printf("CASL <Xs>, <Xt>, [<Xn|SP>]\n\n");

   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, 0, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, 0, 1, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, 1, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, 1, 1, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 1, 0, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 1, 0, 1, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 1, 1, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 1, 1, 1, 0xFFFFFFFFFFFFFFFF);

   printf("Combinations of ALL5s_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, ALL5s_32, ALL5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, ALL5s_32, 0, ALL5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, ALL5s_32, ALL5s_32, ALL5s_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, ALL5s_32, MOST5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, ALL5s_32, 0, MOST5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, ALL5s_32, ALL5s_32, MOST5s_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, ALL5s_32, ALLas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, ALL5s_32, 0, ALLas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, ALL5s_32, ALL5s_32, ALLas_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, ALL5s_32, MOSTas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, ALL5s_32, 0, MOSTas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, ALL5s_32, ALL5s_32, MOSTas_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, ALL5s_32, ALLfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, ALL5s_32, 0, ALLfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, ALL5s_32, ALL5s_32, ALLfs_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, ALL5s_32, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, ALL5s_32, 0, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, ALL5s_32, ALL5s_32, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, ALL5s_32, UP_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, ALL5s_32, 0, UP_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, ALL5s_32, ALL5s_32, UP_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, ALL5s_32, DOWN_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, ALL5s_32, 0, DOWN_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, ALL5s_32, ALL5s_32, DOWN_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, ALL5s_32, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, ALL5s_32, 0, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, ALL5s_32, ALL5s_32, 0, 0xFFFFFFFFFFFFFFFF);

   printf("Combinations of MOST5s_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, MOST5s_32, ALL5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, MOST5s_32, 0, ALL5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, MOST5s_32, MOST5s_32, ALL5s_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, MOST5s_32, MOST5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, MOST5s_32, 0, MOST5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, MOST5s_32, MOST5s_32, MOST5s_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, MOST5s_32, ALLas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, MOST5s_32, 0, ALLas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, MOST5s_32, MOST5s_32, ALLas_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, MOST5s_32, MOSTas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, MOST5s_32, 0, MOSTas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, MOST5s_32, MOST5s_32, MOSTas_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, MOST5s_32, ALLfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, MOST5s_32, 0, ALLfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, MOST5s_32, MOST5s_32, ALLfs_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, MOST5s_32, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, MOST5s_32, 0, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, MOST5s_32, MOST5s_32, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, MOST5s_32, UP_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, MOST5s_32, 0, UP_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, MOST5s_32, MOST5s_32, UP_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, MOST5s_32, DOWN_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, MOST5s_32, 0, DOWN_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, MOST5s_32, MOST5s_32, DOWN_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, MOST5s_32, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, MOST5s_32, 0, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, MOST5s_32, MOST5s_32, 0, 0xFFFFFFFFFFFFFFFF);

   printf("Combinations of ALLas_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, ALLas_32, ALL5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, ALLas_32, 0, ALL5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, ALLas_32, ALLas_32, ALL5s_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, ALLas_32, MOST5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, ALLas_32, 0, MOST5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, ALLas_32, ALLas_32, MOST5s_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, ALLas_32, ALLas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, ALLas_32, 0, ALLas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, ALLas_32, ALLas_32, ALLas_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, ALLas_32, MOSTas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, ALLas_32, 0, MOSTas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, ALLas_32, ALLas_32, MOSTas_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, ALLas_32, ALLfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, ALLas_32, 0, ALLfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, ALLas_32, ALLas_32, ALLfs_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, ALLas_32, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, ALLas_32, 0, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, ALLas_32, ALLas_32, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, ALLas_32, UP_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, ALLas_32, 0, UP_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, ALLas_32, ALLas_32, UP_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, ALLas_32, DOWN_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, ALLas_32, 0, DOWN_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, ALLas_32, ALLas_32, DOWN_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, ALLas_32, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, ALLas_32, 0, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, ALLas_32, ALLas_32, 0, 0xFFFFFFFFFFFFFFFF);

   printf("Combinations of MOSTas_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, MOSTas_32, ALL5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, MOSTas_32, 0, ALL5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, MOSTas_32, MOSTas_32, ALL5s_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, MOSTas_32, MOST5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, MOSTas_32, 0, MOST5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, MOSTas_32, MOSTas_32, MOST5s_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, MOSTas_32, ALLas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, MOSTas_32, 0, ALLas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, MOSTas_32, MOSTas_32, ALLas_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, MOSTas_32, MOSTas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, MOSTas_32, 0, MOSTas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, MOSTas_32, MOSTas_32, MOSTas_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, MOSTas_32, ALLfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, MOSTas_32, 0, ALLfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, MOSTas_32, MOSTas_32, ALLfs_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, MOSTas_32, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, MOSTas_32, 0, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, MOSTas_32, MOSTas_32, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, MOSTas_32, UP_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, MOSTas_32, 0, UP_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, MOSTas_32, MOSTas_32, UP_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, MOSTas_32, DOWN_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, MOSTas_32, 0, DOWN_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, MOSTas_32, MOSTas_32, DOWN_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, MOSTas_32, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, MOSTas_32, 0, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, MOSTas_32, MOSTas_32, 0, 0xFFFFFFFFFFFFFFFF);

   printf("Combinations of ALLfs_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, ALLfs_32, ALL5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, ALLfs_32, 0, ALL5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, ALLfs_32, ALLfs_32, ALL5s_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, ALLfs_32, MOST5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, ALLfs_32, 0, MOST5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, ALLfs_32, ALLfs_32, MOST5s_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, ALLfs_32, ALLas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, ALLfs_32, 0, ALLas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, ALLfs_32, ALLfs_32, ALLas_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, ALLfs_32, MOSTas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, ALLfs_32, 0, MOSTas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, ALLfs_32, ALLfs_32, MOSTas_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, ALLfs_32, ALLfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, ALLfs_32, 0, ALLfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, ALLfs_32, ALLfs_32, ALLfs_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, ALLfs_32, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, ALLfs_32, 0, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, ALLfs_32, ALLfs_32, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, ALLfs_32, UP_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, ALLfs_32, 0, UP_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, ALLfs_32, ALLfs_32, UP_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, ALLfs_32, DOWN_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, ALLfs_32, 0, DOWN_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, ALLfs_32, ALLfs_32, DOWN_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, ALLfs_32, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, ALLfs_32, 0, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, ALLfs_32, ALLfs_32, 0, 0xFFFFFFFFFFFFFFFF);

   printf("Combinations of MOSTfs_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, MOSTfs_32, ALL5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, MOSTfs_32, 0, ALL5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, MOSTfs_32, MOSTfs_32, ALL5s_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, MOSTfs_32, MOST5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, MOSTfs_32, 0, MOST5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, MOSTfs_32, MOSTfs_32, MOST5s_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, MOSTfs_32, ALLas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, MOSTfs_32, 0, ALLas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, MOSTfs_32, MOSTfs_32, ALLas_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, MOSTfs_32, MOSTas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, MOSTfs_32, 0, MOSTas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, MOSTfs_32, MOSTfs_32, MOSTas_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, MOSTfs_32, ALLfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, MOSTfs_32, 0, ALLfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, MOSTfs_32, MOSTfs_32, ALLfs_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, MOSTfs_32, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, MOSTfs_32, 0, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, MOSTfs_32, MOSTfs_32, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, MOSTfs_32, UP_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, MOSTfs_32, 0, UP_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, MOSTfs_32, MOSTfs_32, UP_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, MOSTfs_32, DOWN_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, MOSTfs_32, 0, DOWN_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, MOSTfs_32, MOSTfs_32, DOWN_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, MOSTfs_32, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, MOSTfs_32, 0, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, MOSTfs_32, MOSTfs_32, 0, 0xFFFFFFFFFFFFFFFF);

   printf("Combinations of UP_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, UP_32, ALL5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, UP_32, 0, ALL5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, UP_32, UP_32, ALL5s_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, UP_32, MOST5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, UP_32, 0, MOST5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, UP_32, UP_32, MOST5s_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, UP_32, ALLas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, UP_32, 0, ALLas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, UP_32, UP_32, ALLas_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, UP_32, MOSTas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, UP_32, 0, MOSTas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, UP_32, UP_32, MOSTas_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, UP_32, ALLfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, UP_32, 0, ALLfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, UP_32, UP_32, ALLfs_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, UP_32, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, UP_32, 0, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, UP_32, UP_32, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, UP_32, UP_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, UP_32, 0, UP_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, UP_32, UP_32, UP_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, UP_32, DOWN_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, UP_32, 0, DOWN_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, UP_32, UP_32, DOWN_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, UP_32, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, UP_32, 0, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, UP_32, UP_32, 0, 0xFFFFFFFFFFFFFFFF);

   printf("Combinations of DOWN_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, DOWN_32, ALL5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, DOWN_32, 0, ALL5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, DOWN_32, DOWN_32, ALL5s_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, DOWN_32, MOST5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, DOWN_32, 0, MOST5s_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, DOWN_32, DOWN_32, MOST5s_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, DOWN_32, ALLas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, DOWN_32, 0, ALLas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, DOWN_32, DOWN_32, ALLas_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, DOWN_32, MOSTas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, DOWN_32, 0, MOSTas_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, DOWN_32, DOWN_32, MOSTas_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, DOWN_32, ALLfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, DOWN_32, 0, ALLfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, DOWN_32, DOWN_32, ALLfs_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, DOWN_32, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, DOWN_32, 0, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, DOWN_32, DOWN_32, MOSTfs_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, DOWN_32, UP_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, DOWN_32, 0, UP_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, DOWN_32, DOWN_32, UP_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, DOWN_32, DOWN_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, DOWN_32, 0, DOWN_32, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, DOWN_32, DOWN_32, DOWN_32, 0xFFFFFFFFFFFFFFFF);

   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, 0, DOWN_32, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, DOWN_32, 0, 0, 0xFFFFFFFFFFFFFFFF);
   ATOMIC_TEST_CAS("casl x11, x12, [x5]", x, mem, DOWN_32, DOWN_32, 0, 0xFFFFFFFFFFFFFFFF);

   printf("CASB <Ws>, <Wt>, [<Xn|SP>]\n\n");

   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, 0, 0, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, 0, 1, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, 1, 0, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, 1, 1, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 1, 0, 0, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 1, 0, 1, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 1, 1, 0, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 1, 1, 1, 0xFF);

   printf("Combinations of ALL5s_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, ALL5s_32, ALL5s_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, ALL5s_32, 0, ALL5s_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, ALL5s_32, 0xFF);

   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, ALL5s_32, MOST5s_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, ALL5s_32, 0, MOST5s_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, MOST5s_32, 0xFF);

   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, ALL5s_32, ALLas_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, ALL5s_32, 0, ALLas_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, ALLas_32, 0xFF);

   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, ALL5s_32, MOSTas_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, ALL5s_32, 0, MOSTas_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, MOSTas_32, 0xFF);

   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, ALL5s_32, ALLfs_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, ALL5s_32, 0, ALLfs_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, ALLfs_32, 0xFF);

   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, ALL5s_32, MOSTfs_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, ALL5s_32, 0, MOSTfs_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, MOSTfs_32, 0xFF);

   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, ALL5s_32, UP_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, ALL5s_32, 0, UP_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, UP_32, 0xFF);

   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, ALL5s_32, DOWN_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, ALL5s_32, 0, DOWN_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, DOWN_32, 0xFF);

   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, ALL5s_32, 0, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, ALL5s_32, 0, 0, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, 0, 0xFF);

   printf("Combinations of MOST5s_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, MOST5s_32, ALL5s_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, MOST5s_32, 0, ALL5s_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, ALL5s_32, 0xFF);

   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, MOST5s_32, MOST5s_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, MOST5s_32, 0, MOST5s_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, MOST5s_32, 0xFF);

   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, MOST5s_32, ALLas_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, MOST5s_32, 0, ALLas_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, ALLas_32, 0xFF);

   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, MOST5s_32, MOSTas_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, MOST5s_32, 0, MOSTas_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, MOSTas_32, 0xFF);

   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, MOST5s_32, ALLfs_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, MOST5s_32, 0, ALLfs_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, ALLfs_32, 0xFF);

   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, MOST5s_32, MOSTfs_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, MOST5s_32, 0, MOSTfs_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, MOSTfs_32, 0xFF);

   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, MOST5s_32, UP_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, MOST5s_32, 0, UP_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, UP_32, 0xFF);

   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, MOST5s_32, DOWN_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, MOST5s_32, 0, DOWN_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, DOWN_32, 0xFF);

   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, MOST5s_32, 0, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, MOST5s_32, 0, 0, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, 0, 0xFF);

   printf("Combinations of ALLas_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, ALLas_32, ALL5s_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, ALLas_32, 0, ALL5s_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, ALL5s_32, 0xFF);

   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, ALLas_32, MOST5s_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, ALLas_32, 0, MOST5s_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, MOST5s_32, 0xFF);

   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, ALLas_32, ALLas_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, ALLas_32, 0, ALLas_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, ALLas_32, 0xFF);

   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, ALLas_32, MOSTas_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, ALLas_32, 0, MOSTas_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, MOSTas_32, 0xFF);

   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, ALLas_32, ALLfs_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, ALLas_32, 0, ALLfs_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, ALLfs_32, 0xFF);

   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, ALLas_32, MOSTfs_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, ALLas_32, 0, MOSTfs_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, MOSTfs_32, 0xFF);

   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, ALLas_32, UP_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, ALLas_32, 0, UP_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, UP_32, 0xFF);

   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, ALLas_32, DOWN_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, ALLas_32, 0, DOWN_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, DOWN_32, 0xFF);

   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, ALLas_32, 0, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, ALLas_32, 0, 0, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, 0, 0xFF);

   printf("Combinations of MOSTas_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, MOSTas_32, ALL5s_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, MOSTas_32, 0, ALL5s_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, ALL5s_32, 0xFF);

   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, MOSTas_32, MOST5s_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, MOSTas_32, 0, MOST5s_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, MOST5s_32, 0xFF);

   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, MOSTas_32, ALLas_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, MOSTas_32, 0, ALLas_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, ALLas_32, 0xFF);

   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, MOSTas_32, MOSTas_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, MOSTas_32, 0, MOSTas_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, MOSTas_32, 0xFF);

   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, MOSTas_32, ALLfs_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, MOSTas_32, 0, ALLfs_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, ALLfs_32, 0xFF);

   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, MOSTas_32, MOSTfs_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, MOSTas_32, 0, MOSTfs_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, MOSTfs_32, 0xFF);

   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, MOSTas_32, UP_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, MOSTas_32, 0, UP_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, UP_32, 0xFF);

   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, MOSTas_32, DOWN_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, MOSTas_32, 0, DOWN_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, DOWN_32, 0xFF);

   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, MOSTas_32, 0, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, MOSTas_32, 0, 0, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, 0, 0xFF);

   printf("Combinations of ALLfs_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, ALLfs_32, ALL5s_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, ALLfs_32, 0, ALL5s_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, ALL5s_32, 0xFF);

   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, ALLfs_32, MOST5s_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, ALLfs_32, 0, MOST5s_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, MOST5s_32, 0xFF);

   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, ALLfs_32, ALLas_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, ALLfs_32, 0, ALLas_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, ALLas_32, 0xFF);

   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, ALLfs_32, MOSTas_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, ALLfs_32, 0, MOSTas_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, MOSTas_32, 0xFF);

   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, ALLfs_32, ALLfs_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, ALLfs_32, 0, ALLfs_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, ALLfs_32, 0xFF);

   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, ALLfs_32, MOSTfs_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, ALLfs_32, 0, MOSTfs_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, MOSTfs_32, 0xFF);

   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, ALLfs_32, UP_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, ALLfs_32, 0, UP_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, UP_32, 0xFF);

   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, ALLfs_32, DOWN_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, ALLfs_32, 0, DOWN_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, DOWN_32, 0xFF);

   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, ALLfs_32, 0, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, ALLfs_32, 0, 0, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, 0, 0xFF);

   printf("Combinations of MOSTfs_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, MOSTfs_32, ALL5s_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, MOSTfs_32, 0, ALL5s_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, ALL5s_32, 0xFF);

   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, MOSTfs_32, MOST5s_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, MOSTfs_32, 0, MOST5s_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, MOST5s_32, 0xFF);

   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, MOSTfs_32, ALLas_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, MOSTfs_32, 0, ALLas_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, ALLas_32, 0xFF);

   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, MOSTfs_32, MOSTas_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, MOSTfs_32, 0, MOSTas_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, MOSTas_32, 0xFF);

   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, MOSTfs_32, ALLfs_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, MOSTfs_32, 0, ALLfs_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, ALLfs_32, 0xFF);

   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, MOSTfs_32, MOSTfs_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, MOSTfs_32, 0, MOSTfs_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, MOSTfs_32, 0xFF);

   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, MOSTfs_32, UP_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, MOSTfs_32, 0, UP_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, UP_32, 0xFF);

   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, MOSTfs_32, DOWN_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, MOSTfs_32, 0, DOWN_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, DOWN_32, 0xFF);

   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, MOSTfs_32, 0, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, MOSTfs_32, 0, 0, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, 0, 0xFF);

   printf("Combinations of UP_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, UP_32, ALL5s_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, UP_32, 0, ALL5s_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, UP_32, UP_32, ALL5s_32, 0xFF);

   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, UP_32, MOST5s_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, UP_32, 0, MOST5s_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, UP_32, UP_32, MOST5s_32, 0xFF);

   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, UP_32, ALLas_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, UP_32, 0, ALLas_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, UP_32, UP_32, ALLas_32, 0xFF);

   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, UP_32, MOSTas_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, UP_32, 0, MOSTas_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, UP_32, UP_32, MOSTas_32, 0xFF);

   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, UP_32, ALLfs_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, UP_32, 0, ALLfs_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, UP_32, UP_32, ALLfs_32, 0xFF);

   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, UP_32, MOSTfs_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, UP_32, 0, MOSTfs_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, UP_32, UP_32, MOSTfs_32, 0xFF);

   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, UP_32, UP_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, UP_32, 0, UP_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, UP_32, UP_32, UP_32, 0xFF);

   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, UP_32, DOWN_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, UP_32, 0, DOWN_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, UP_32, UP_32, DOWN_32, 0xFF);

   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, UP_32, 0, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, UP_32, 0, 0, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, UP_32, UP_32, 0, 0xFF);

   printf("Combinations of DOWN_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, DOWN_32, ALL5s_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, DOWN_32, 0, ALL5s_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, ALL5s_32, 0xFF);

   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, DOWN_32, MOST5s_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, DOWN_32, 0, MOST5s_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, MOST5s_32, 0xFF);

   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, DOWN_32, ALLas_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, DOWN_32, 0, ALLas_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, ALLas_32, 0xFF);

   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, DOWN_32, MOSTas_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, DOWN_32, 0, MOSTas_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, MOSTas_32, 0xFF);

   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, DOWN_32, ALLfs_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, DOWN_32, 0, ALLfs_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, ALLfs_32, 0xFF);

   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, DOWN_32, MOSTfs_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, DOWN_32, 0, MOSTfs_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, MOSTfs_32, 0xFF);

   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, DOWN_32, UP_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, DOWN_32, 0, UP_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, UP_32, 0xFF);

   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, DOWN_32, DOWN_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, DOWN_32, 0, DOWN_32, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, DOWN_32, 0xFF);

   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, 0, DOWN_32, 0, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, DOWN_32, 0, 0, 0xFF);
   ATOMIC_TEST_CAS("casb w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, 0, 0xFF);

   printf("CASAB <Ws>, <Wt>, [<Xn|SP>]\n\n");

   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, 0, 0, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, 0, 1, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, 1, 0, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, 1, 1, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 1, 0, 0, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 1, 0, 1, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 1, 1, 0, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 1, 1, 1, 0xFF);

   printf("Combinations of ALL5s_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, ALL5s_32, ALL5s_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, ALL5s_32, 0, ALL5s_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, ALL5s_32, 0xFF);

   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, ALL5s_32, MOST5s_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, ALL5s_32, 0, MOST5s_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, MOST5s_32, 0xFF);

   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, ALL5s_32, ALLas_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, ALL5s_32, 0, ALLas_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, ALLas_32, 0xFF);

   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, ALL5s_32, MOSTas_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, ALL5s_32, 0, MOSTas_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, MOSTas_32, 0xFF);

   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, ALL5s_32, ALLfs_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, ALL5s_32, 0, ALLfs_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, ALLfs_32, 0xFF);

   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, ALL5s_32, MOSTfs_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, ALL5s_32, 0, MOSTfs_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, MOSTfs_32, 0xFF);

   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, ALL5s_32, UP_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, ALL5s_32, 0, UP_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, UP_32, 0xFF);

   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, ALL5s_32, DOWN_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, ALL5s_32, 0, DOWN_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, DOWN_32, 0xFF);

   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, ALL5s_32, 0, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, ALL5s_32, 0, 0, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, 0, 0xFF);

   printf("Combinations of MOST5s_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, MOST5s_32, ALL5s_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, MOST5s_32, 0, ALL5s_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, ALL5s_32, 0xFF);

   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, MOST5s_32, MOST5s_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, MOST5s_32, 0, MOST5s_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, MOST5s_32, 0xFF);

   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, MOST5s_32, ALLas_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, MOST5s_32, 0, ALLas_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, ALLas_32, 0xFF);

   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, MOST5s_32, MOSTas_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, MOST5s_32, 0, MOSTas_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, MOSTas_32, 0xFF);

   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, MOST5s_32, ALLfs_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, MOST5s_32, 0, ALLfs_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, ALLfs_32, 0xFF);

   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, MOST5s_32, MOSTfs_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, MOST5s_32, 0, MOSTfs_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, MOSTfs_32, 0xFF);

   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, MOST5s_32, UP_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, MOST5s_32, 0, UP_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, UP_32, 0xFF);

   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, MOST5s_32, DOWN_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, MOST5s_32, 0, DOWN_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, DOWN_32, 0xFF);

   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, MOST5s_32, 0, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, MOST5s_32, 0, 0, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, 0, 0xFF);

   printf("Combinations of ALLas_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, ALLas_32, ALL5s_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, ALLas_32, 0, ALL5s_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, ALL5s_32, 0xFF);

   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, ALLas_32, MOST5s_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, ALLas_32, 0, MOST5s_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, MOST5s_32, 0xFF);

   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, ALLas_32, ALLas_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, ALLas_32, 0, ALLas_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, ALLas_32, 0xFF);

   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, ALLas_32, MOSTas_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, ALLas_32, 0, MOSTas_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, MOSTas_32, 0xFF);

   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, ALLas_32, ALLfs_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, ALLas_32, 0, ALLfs_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, ALLfs_32, 0xFF);

   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, ALLas_32, MOSTfs_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, ALLas_32, 0, MOSTfs_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, MOSTfs_32, 0xFF);

   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, ALLas_32, UP_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, ALLas_32, 0, UP_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, UP_32, 0xFF);

   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, ALLas_32, DOWN_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, ALLas_32, 0, DOWN_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, DOWN_32, 0xFF);

   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, ALLas_32, 0, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, ALLas_32, 0, 0, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, 0, 0xFF);

   printf("Combinations of MOSTas_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, MOSTas_32, ALL5s_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, MOSTas_32, 0, ALL5s_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, ALL5s_32, 0xFF);

   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, MOSTas_32, MOST5s_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, MOSTas_32, 0, MOST5s_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, MOST5s_32, 0xFF);

   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, MOSTas_32, ALLas_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, MOSTas_32, 0, ALLas_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, ALLas_32, 0xFF);

   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, MOSTas_32, MOSTas_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, MOSTas_32, 0, MOSTas_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, MOSTas_32, 0xFF);

   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, MOSTas_32, ALLfs_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, MOSTas_32, 0, ALLfs_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, ALLfs_32, 0xFF);

   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, MOSTas_32, MOSTfs_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, MOSTas_32, 0, MOSTfs_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, MOSTfs_32, 0xFF);

   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, MOSTas_32, UP_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, MOSTas_32, 0, UP_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, UP_32, 0xFF);

   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, MOSTas_32, DOWN_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, MOSTas_32, 0, DOWN_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, DOWN_32, 0xFF);

   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, MOSTas_32, 0, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, MOSTas_32, 0, 0, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, 0, 0xFF);

   printf("Combinations of ALLfs_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, ALLfs_32, ALL5s_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, ALLfs_32, 0, ALL5s_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, ALL5s_32, 0xFF);

   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, ALLfs_32, MOST5s_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, ALLfs_32, 0, MOST5s_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, MOST5s_32, 0xFF);

   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, ALLfs_32, ALLas_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, ALLfs_32, 0, ALLas_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, ALLas_32, 0xFF);

   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, ALLfs_32, MOSTas_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, ALLfs_32, 0, MOSTas_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, MOSTas_32, 0xFF);

   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, ALLfs_32, ALLfs_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, ALLfs_32, 0, ALLfs_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, ALLfs_32, 0xFF);

   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, ALLfs_32, MOSTfs_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, ALLfs_32, 0, MOSTfs_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, MOSTfs_32, 0xFF);

   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, ALLfs_32, UP_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, ALLfs_32, 0, UP_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, UP_32, 0xFF);

   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, ALLfs_32, DOWN_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, ALLfs_32, 0, DOWN_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, DOWN_32, 0xFF);

   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, ALLfs_32, 0, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, ALLfs_32, 0, 0, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, 0, 0xFF);

   printf("Combinations of MOSTfs_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, MOSTfs_32, ALL5s_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, MOSTfs_32, 0, ALL5s_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, ALL5s_32, 0xFF);

   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, MOSTfs_32, MOST5s_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, MOSTfs_32, 0, MOST5s_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, MOST5s_32, 0xFF);

   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, MOSTfs_32, ALLas_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, MOSTfs_32, 0, ALLas_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, ALLas_32, 0xFF);

   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, MOSTfs_32, MOSTas_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, MOSTfs_32, 0, MOSTas_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, MOSTas_32, 0xFF);

   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, MOSTfs_32, ALLfs_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, MOSTfs_32, 0, ALLfs_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, ALLfs_32, 0xFF);

   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, MOSTfs_32, MOSTfs_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, MOSTfs_32, 0, MOSTfs_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, MOSTfs_32, 0xFF);

   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, MOSTfs_32, UP_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, MOSTfs_32, 0, UP_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, UP_32, 0xFF);

   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, MOSTfs_32, DOWN_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, MOSTfs_32, 0, DOWN_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, DOWN_32, 0xFF);

   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, MOSTfs_32, 0, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, MOSTfs_32, 0, 0, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, 0, 0xFF);

   printf("Combinations of UP_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, UP_32, ALL5s_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, UP_32, 0, ALL5s_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, UP_32, UP_32, ALL5s_32, 0xFF);

   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, UP_32, MOST5s_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, UP_32, 0, MOST5s_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, UP_32, UP_32, MOST5s_32, 0xFF);

   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, UP_32, ALLas_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, UP_32, 0, ALLas_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, UP_32, UP_32, ALLas_32, 0xFF);

   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, UP_32, MOSTas_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, UP_32, 0, MOSTas_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, UP_32, UP_32, MOSTas_32, 0xFF);

   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, UP_32, ALLfs_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, UP_32, 0, ALLfs_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, UP_32, UP_32, ALLfs_32, 0xFF);

   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, UP_32, MOSTfs_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, UP_32, 0, MOSTfs_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, UP_32, UP_32, MOSTfs_32, 0xFF);

   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, UP_32, UP_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, UP_32, 0, UP_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, UP_32, UP_32, UP_32, 0xFF);

   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, UP_32, DOWN_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, UP_32, 0, DOWN_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, UP_32, UP_32, DOWN_32, 0xFF);

   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, UP_32, 0, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, UP_32, 0, 0, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, UP_32, UP_32, 0, 0xFF);

   printf("Combinations of DOWN_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, DOWN_32, ALL5s_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, DOWN_32, 0, ALL5s_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, ALL5s_32, 0xFF);

   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, DOWN_32, MOST5s_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, DOWN_32, 0, MOST5s_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, MOST5s_32, 0xFF);

   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, DOWN_32, ALLas_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, DOWN_32, 0, ALLas_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, ALLas_32, 0xFF);

   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, DOWN_32, MOSTas_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, DOWN_32, 0, MOSTas_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, MOSTas_32, 0xFF);

   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, DOWN_32, ALLfs_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, DOWN_32, 0, ALLfs_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, ALLfs_32, 0xFF);

   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, DOWN_32, MOSTfs_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, DOWN_32, 0, MOSTfs_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, MOSTfs_32, 0xFF);

   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, DOWN_32, UP_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, DOWN_32, 0, UP_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, UP_32, 0xFF);

   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, DOWN_32, DOWN_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, DOWN_32, 0, DOWN_32, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, DOWN_32, 0xFF);

   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, 0, DOWN_32, 0, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, DOWN_32, 0, 0, 0xFF);
   ATOMIC_TEST_CAS("casab w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, 0, 0xFF);

   printf("CASALB <Ws>, <Wt>, [<Xn|SP>]\n\n");

   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, 0, 0, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, 0, 1, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, 1, 0, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, 1, 1, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 1, 0, 0, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 1, 0, 1, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 1, 1, 0, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 1, 1, 1, 0xFF);

   printf("Combinations of ALL5s_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, ALL5s_32, ALL5s_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, ALL5s_32, 0, ALL5s_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, ALL5s_32, 0xFF);

   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, ALL5s_32, MOST5s_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, ALL5s_32, 0, MOST5s_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, MOST5s_32, 0xFF);

   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, ALL5s_32, ALLas_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, ALL5s_32, 0, ALLas_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, ALLas_32, 0xFF);

   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, ALL5s_32, MOSTas_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, ALL5s_32, 0, MOSTas_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, MOSTas_32, 0xFF);

   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, ALL5s_32, ALLfs_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, ALL5s_32, 0, ALLfs_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, ALLfs_32, 0xFF);

   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, ALL5s_32, MOSTfs_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, ALL5s_32, 0, MOSTfs_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, MOSTfs_32, 0xFF);

   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, ALL5s_32, UP_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, ALL5s_32, 0, UP_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, UP_32, 0xFF);

   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, ALL5s_32, DOWN_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, ALL5s_32, 0, DOWN_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, DOWN_32, 0xFF);

   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, ALL5s_32, 0, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, ALL5s_32, 0, 0, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, 0, 0xFF);

   printf("Combinations of MOST5s_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, MOST5s_32, ALL5s_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, MOST5s_32, 0, ALL5s_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, ALL5s_32, 0xFF);

   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, MOST5s_32, MOST5s_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, MOST5s_32, 0, MOST5s_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, MOST5s_32, 0xFF);

   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, MOST5s_32, ALLas_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, MOST5s_32, 0, ALLas_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, ALLas_32, 0xFF);

   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, MOST5s_32, MOSTas_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, MOST5s_32, 0, MOSTas_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, MOSTas_32, 0xFF);

   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, MOST5s_32, ALLfs_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, MOST5s_32, 0, ALLfs_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, ALLfs_32, 0xFF);

   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, MOST5s_32, MOSTfs_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, MOST5s_32, 0, MOSTfs_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, MOSTfs_32, 0xFF);

   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, MOST5s_32, UP_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, MOST5s_32, 0, UP_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, UP_32, 0xFF);

   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, MOST5s_32, DOWN_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, MOST5s_32, 0, DOWN_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, DOWN_32, 0xFF);

   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, MOST5s_32, 0, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, MOST5s_32, 0, 0, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, 0, 0xFF);

   printf("Combinations of ALLas_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, ALLas_32, ALL5s_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, ALLas_32, 0, ALL5s_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, ALL5s_32, 0xFF);

   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, ALLas_32, MOST5s_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, ALLas_32, 0, MOST5s_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, MOST5s_32, 0xFF);

   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, ALLas_32, ALLas_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, ALLas_32, 0, ALLas_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, ALLas_32, 0xFF);

   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, ALLas_32, MOSTas_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, ALLas_32, 0, MOSTas_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, MOSTas_32, 0xFF);

   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, ALLas_32, ALLfs_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, ALLas_32, 0, ALLfs_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, ALLfs_32, 0xFF);

   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, ALLas_32, MOSTfs_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, ALLas_32, 0, MOSTfs_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, MOSTfs_32, 0xFF);

   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, ALLas_32, UP_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, ALLas_32, 0, UP_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, UP_32, 0xFF);

   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, ALLas_32, DOWN_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, ALLas_32, 0, DOWN_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, DOWN_32, 0xFF);

   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, ALLas_32, 0, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, ALLas_32, 0, 0, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, 0, 0xFF);

   printf("Combinations of MOSTas_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, MOSTas_32, ALL5s_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, MOSTas_32, 0, ALL5s_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, ALL5s_32, 0xFF);

   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, MOSTas_32, MOST5s_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, MOSTas_32, 0, MOST5s_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, MOST5s_32, 0xFF);

   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, MOSTas_32, ALLas_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, MOSTas_32, 0, ALLas_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, ALLas_32, 0xFF);

   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, MOSTas_32, MOSTas_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, MOSTas_32, 0, MOSTas_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, MOSTas_32, 0xFF);

   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, MOSTas_32, ALLfs_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, MOSTas_32, 0, ALLfs_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, ALLfs_32, 0xFF);

   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, MOSTas_32, MOSTfs_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, MOSTas_32, 0, MOSTfs_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, MOSTfs_32, 0xFF);

   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, MOSTas_32, UP_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, MOSTas_32, 0, UP_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, UP_32, 0xFF);

   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, MOSTas_32, DOWN_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, MOSTas_32, 0, DOWN_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, DOWN_32, 0xFF);

   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, MOSTas_32, 0, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, MOSTas_32, 0, 0, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, 0, 0xFF);

   printf("Combinations of ALLfs_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, ALLfs_32, ALL5s_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, ALLfs_32, 0, ALL5s_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, ALL5s_32, 0xFF);

   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, ALLfs_32, MOST5s_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, ALLfs_32, 0, MOST5s_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, MOST5s_32, 0xFF);

   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, ALLfs_32, ALLas_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, ALLfs_32, 0, ALLas_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, ALLas_32, 0xFF);

   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, ALLfs_32, MOSTas_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, ALLfs_32, 0, MOSTas_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, MOSTas_32, 0xFF);

   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, ALLfs_32, ALLfs_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, ALLfs_32, 0, ALLfs_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, ALLfs_32, 0xFF);

   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, ALLfs_32, MOSTfs_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, ALLfs_32, 0, MOSTfs_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, MOSTfs_32, 0xFF);

   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, ALLfs_32, UP_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, ALLfs_32, 0, UP_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, UP_32, 0xFF);

   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, ALLfs_32, DOWN_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, ALLfs_32, 0, DOWN_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, DOWN_32, 0xFF);

   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, ALLfs_32, 0, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, ALLfs_32, 0, 0, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, 0, 0xFF);

   printf("Combinations of MOSTfs_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, MOSTfs_32, ALL5s_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, MOSTfs_32, 0, ALL5s_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, ALL5s_32, 0xFF);

   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, MOSTfs_32, MOST5s_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, MOSTfs_32, 0, MOST5s_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, MOST5s_32, 0xFF);

   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, MOSTfs_32, ALLas_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, MOSTfs_32, 0, ALLas_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, ALLas_32, 0xFF);

   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, MOSTfs_32, MOSTas_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, MOSTfs_32, 0, MOSTas_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, MOSTas_32, 0xFF);

   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, MOSTfs_32, ALLfs_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, MOSTfs_32, 0, ALLfs_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, ALLfs_32, 0xFF);

   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, MOSTfs_32, MOSTfs_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, MOSTfs_32, 0, MOSTfs_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, MOSTfs_32, 0xFF);

   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, MOSTfs_32, UP_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, MOSTfs_32, 0, UP_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, UP_32, 0xFF);

   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, MOSTfs_32, DOWN_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, MOSTfs_32, 0, DOWN_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, DOWN_32, 0xFF);

   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, MOSTfs_32, 0, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, MOSTfs_32, 0, 0, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, 0, 0xFF);

   printf("Combinations of UP_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, UP_32, ALL5s_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, UP_32, 0, ALL5s_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, UP_32, UP_32, ALL5s_32, 0xFF);

   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, UP_32, MOST5s_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, UP_32, 0, MOST5s_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, UP_32, UP_32, MOST5s_32, 0xFF);

   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, UP_32, ALLas_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, UP_32, 0, ALLas_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, UP_32, UP_32, ALLas_32, 0xFF);

   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, UP_32, MOSTas_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, UP_32, 0, MOSTas_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, UP_32, UP_32, MOSTas_32, 0xFF);

   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, UP_32, ALLfs_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, UP_32, 0, ALLfs_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, UP_32, UP_32, ALLfs_32, 0xFF);

   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, UP_32, MOSTfs_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, UP_32, 0, MOSTfs_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, UP_32, UP_32, MOSTfs_32, 0xFF);

   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, UP_32, UP_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, UP_32, 0, UP_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, UP_32, UP_32, UP_32, 0xFF);

   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, UP_32, DOWN_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, UP_32, 0, DOWN_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, UP_32, UP_32, DOWN_32, 0xFF);

   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, UP_32, 0, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, UP_32, 0, 0, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, UP_32, UP_32, 0, 0xFF);

   printf("Combinations of DOWN_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, DOWN_32, ALL5s_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, DOWN_32, 0, ALL5s_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, ALL5s_32, 0xFF);

   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, DOWN_32, MOST5s_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, DOWN_32, 0, MOST5s_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, MOST5s_32, 0xFF);

   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, DOWN_32, ALLas_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, DOWN_32, 0, ALLas_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, ALLas_32, 0xFF);

   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, DOWN_32, MOSTas_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, DOWN_32, 0, MOSTas_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, MOSTas_32, 0xFF);

   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, DOWN_32, ALLfs_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, DOWN_32, 0, ALLfs_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, ALLfs_32, 0xFF);

   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, DOWN_32, MOSTfs_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, DOWN_32, 0, MOSTfs_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, MOSTfs_32, 0xFF);

   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, DOWN_32, UP_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, DOWN_32, 0, UP_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, UP_32, 0xFF);

   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, DOWN_32, DOWN_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, DOWN_32, 0, DOWN_32, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, DOWN_32, 0xFF);

   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, 0, DOWN_32, 0, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, DOWN_32, 0, 0, 0xFF);
   ATOMIC_TEST_CAS("casalb w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, 0, 0xFF);

   printf("CASLB <Ws>, <Wt>, [<Xn|SP>]\n\n");

   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, 0, 0, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, 0, 1, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, 1, 0, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, 1, 1, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 1, 0, 0, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 1, 0, 1, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 1, 1, 0, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 1, 1, 1, 0xFF);

   printf("Combinations of ALL5s_32 and all other patterns\n");

   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, ALL5s_32, ALL5s_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, ALL5s_32, 0, ALL5s_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, ALL5s_32, 0xFF);

   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, ALL5s_32, MOST5s_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, ALL5s_32, 0, MOST5s_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, MOST5s_32, 0xFF);

   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, ALL5s_32, ALLas_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, ALL5s_32, 0, ALLas_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, ALLas_32, 0xFF);

   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, ALL5s_32, MOSTas_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, ALL5s_32, 0, MOSTas_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, MOSTas_32, 0xFF);

   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, ALL5s_32, ALLfs_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, ALL5s_32, 0, ALLfs_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, ALLfs_32, 0xFF);

   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, ALL5s_32, MOSTfs_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, ALL5s_32, 0, MOSTfs_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, MOSTfs_32, 0xFF);

   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, ALL5s_32, UP_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, ALL5s_32, 0, UP_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, UP_32, 0xFF);

   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, ALL5s_32, DOWN_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, ALL5s_32, 0, DOWN_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, DOWN_32, 0xFF);

   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, ALL5s_32, 0, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, ALL5s_32, 0, 0, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, 0, 0xFF);

   printf("Combinations of MOST5s_32 and all other patterns\n");

   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, MOST5s_32, ALL5s_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, MOST5s_32, 0, ALL5s_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, ALL5s_32, 0xFF);

   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, MOST5s_32, MOST5s_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, MOST5s_32, 0, MOST5s_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, MOST5s_32, 0xFF);

   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, MOST5s_32, ALLas_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, MOST5s_32, 0, ALLas_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, ALLas_32, 0xFF);

   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, MOST5s_32, MOSTas_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, MOST5s_32, 0, MOSTas_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, MOSTas_32, 0xFF);

   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, MOST5s_32, ALLfs_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, MOST5s_32, 0, ALLfs_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, ALLfs_32, 0xFF);

   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, MOST5s_32, MOSTfs_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, MOST5s_32, 0, MOSTfs_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, MOSTfs_32, 0xFF);

   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, MOST5s_32, UP_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, MOST5s_32, 0, UP_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, UP_32, 0xFF);

   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, MOST5s_32, DOWN_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, MOST5s_32, 0, DOWN_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, DOWN_32, 0xFF);

   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, MOST5s_32, 0, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, MOST5s_32, 0, 0, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, 0, 0xFF);

   printf("Combinations of ALLas_32 and all other patterns\n");

   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, ALLas_32, ALL5s_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, ALLas_32, 0, ALL5s_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, ALL5s_32, 0xFF);

   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, ALLas_32, MOST5s_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, ALLas_32, 0, MOST5s_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, MOST5s_32, 0xFF);

   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, ALLas_32, ALLas_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, ALLas_32, 0, ALLas_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, ALLas_32, 0xFF);

   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, ALLas_32, MOSTas_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, ALLas_32, 0, MOSTas_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, MOSTas_32, 0xFF);

   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, ALLas_32, ALLfs_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, ALLas_32, 0, ALLfs_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, ALLfs_32, 0xFF);

   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, ALLas_32, MOSTfs_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, ALLas_32, 0, MOSTfs_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, MOSTfs_32, 0xFF);

   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, ALLas_32, UP_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, ALLas_32, 0, UP_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, UP_32, 0xFF);

   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, ALLas_32, DOWN_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, ALLas_32, 0, DOWN_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, DOWN_32, 0xFF);

   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, ALLas_32, 0, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, ALLas_32, 0, 0, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, 0, 0xFF);

   printf("Combinations of MOSTas_32 and all other patterns\n");

   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, MOSTas_32, ALL5s_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, MOSTas_32, 0, ALL5s_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, ALL5s_32, 0xFF);

   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, MOSTas_32, MOST5s_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, MOSTas_32, 0, MOST5s_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, MOST5s_32, 0xFF);

   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, MOSTas_32, ALLas_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, MOSTas_32, 0, ALLas_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, ALLas_32, 0xFF);

   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, MOSTas_32, MOSTas_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, MOSTas_32, 0, MOSTas_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, MOSTas_32, 0xFF);

   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, MOSTas_32, ALLfs_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, MOSTas_32, 0, ALLfs_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, ALLfs_32, 0xFF);

   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, MOSTas_32, MOSTfs_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, MOSTas_32, 0, MOSTfs_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, MOSTfs_32, 0xFF);

   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, MOSTas_32, UP_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, MOSTas_32, 0, UP_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, UP_32, 0xFF);

   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, MOSTas_32, DOWN_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, MOSTas_32, 0, DOWN_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, DOWN_32, 0xFF);

   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, MOSTas_32, 0, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, MOSTas_32, 0, 0, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, 0, 0xFF);

   printf("Combinations of ALLfs_32 and all other patterns\n");

   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, ALLfs_32, ALL5s_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, ALLfs_32, 0, ALL5s_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, ALL5s_32, 0xFF);

   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, ALLfs_32, MOST5s_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, ALLfs_32, 0, MOST5s_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, MOST5s_32, 0xFF);

   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, ALLfs_32, ALLas_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, ALLfs_32, 0, ALLas_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, ALLas_32, 0xFF);

   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, ALLfs_32, MOSTas_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, ALLfs_32, 0, MOSTas_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, MOSTas_32, 0xFF);

   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, ALLfs_32, ALLfs_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, ALLfs_32, 0, ALLfs_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, ALLfs_32, 0xFF);

   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, ALLfs_32, MOSTfs_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, ALLfs_32, 0, MOSTfs_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, MOSTfs_32, 0xFF);

   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, ALLfs_32, UP_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, ALLfs_32, 0, UP_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, UP_32, 0xFF);

   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, ALLfs_32, DOWN_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, ALLfs_32, 0, DOWN_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, DOWN_32, 0xFF);

   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, ALLfs_32, 0, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, ALLfs_32, 0, 0, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, 0, 0xFF);

   printf("Combinations of MOSTfs_32 and all other patterns\n");

   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, MOSTfs_32, ALL5s_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, MOSTfs_32, 0, ALL5s_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, ALL5s_32, 0xFF);

   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, MOSTfs_32, MOST5s_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, MOSTfs_32, 0, MOST5s_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, MOST5s_32, 0xFF);

   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, MOSTfs_32, ALLas_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, MOSTfs_32, 0, ALLas_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, ALLas_32, 0xFF);

   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, MOSTfs_32, MOSTas_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, MOSTfs_32, 0, MOSTas_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, MOSTas_32, 0xFF);

   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, MOSTfs_32, ALLfs_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, MOSTfs_32, 0, ALLfs_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, ALLfs_32, 0xFF);

   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, MOSTfs_32, MOSTfs_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, MOSTfs_32, 0, MOSTfs_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, MOSTfs_32, 0xFF);

   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, MOSTfs_32, UP_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, MOSTfs_32, 0, UP_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, UP_32, 0xFF);

   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, MOSTfs_32, DOWN_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, MOSTfs_32, 0, DOWN_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, DOWN_32, 0xFF);

   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, MOSTfs_32, 0, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, MOSTfs_32, 0, 0, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, 0, 0xFF);

   printf("Combinations of UP_32 and all other patterns\n");

   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, UP_32, ALL5s_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, UP_32, 0, ALL5s_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, UP_32, UP_32, ALL5s_32, 0xFF);

   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, UP_32, MOST5s_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, UP_32, 0, MOST5s_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, UP_32, UP_32, MOST5s_32, 0xFF);

   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, UP_32, ALLas_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, UP_32, 0, ALLas_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, UP_32, UP_32, ALLas_32, 0xFF);

   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, UP_32, MOSTas_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, UP_32, 0, MOSTas_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, UP_32, UP_32, MOSTas_32, 0xFF);

   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, UP_32, ALLfs_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, UP_32, 0, ALLfs_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, UP_32, UP_32, ALLfs_32, 0xFF);

   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, UP_32, MOSTfs_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, UP_32, 0, MOSTfs_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, UP_32, UP_32, MOSTfs_32, 0xFF);

   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, UP_32, UP_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, UP_32, 0, UP_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, UP_32, UP_32, UP_32, 0xFF);

   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, UP_32, DOWN_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, UP_32, 0, DOWN_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, UP_32, UP_32, DOWN_32, 0xFF);

   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, UP_32, 0, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, UP_32, 0, 0, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, UP_32, UP_32, 0, 0xFF);

   printf("Combinations of DOWN_32 and all other patterns\n");

   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, DOWN_32, ALL5s_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, DOWN_32, 0, ALL5s_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, ALL5s_32, 0xFF);

   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, DOWN_32, MOST5s_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, DOWN_32, 0, MOST5s_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, MOST5s_32, 0xFF);

   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, DOWN_32, ALLas_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, DOWN_32, 0, ALLas_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, ALLas_32, 0xFF);

   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, DOWN_32, MOSTas_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, DOWN_32, 0, MOSTas_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, MOSTas_32, 0xFF);

   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, DOWN_32, ALLfs_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, DOWN_32, 0, ALLfs_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, ALLfs_32, 0xFF);

   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, DOWN_32, MOSTfs_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, DOWN_32, 0, MOSTfs_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, MOSTfs_32, 0xFF);

   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, DOWN_32, UP_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, DOWN_32, 0, UP_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, UP_32, 0xFF);

   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, DOWN_32, DOWN_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, DOWN_32, 0, DOWN_32, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, DOWN_32, 0xFF);

   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, 0, DOWN_32, 0, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, DOWN_32, 0, 0, 0xFF);
   ATOMIC_TEST_CAS("caslb w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, 0, 0xFF);

   printf("CASH <Ws>, <Wt>, [<Xn|SP>]\n\n");

   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, 0, 0, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, 0, 1, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, 1, 0, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, 1, 1, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 1, 0, 0, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 1, 0, 1, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 1, 1, 0, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 1, 1, 1, 0xFFFF);

   printf("Combinations of ALL5s_32 and all other patterns\n");

   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, ALL5s_32, ALL5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, ALL5s_32, 0, ALL5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, ALL5s_32, 0xFFFF);

   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, ALL5s_32, MOST5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, ALL5s_32, 0, MOST5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, MOST5s_32, 0xFFFF);

   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, ALL5s_32, ALLas_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, ALL5s_32, 0, ALLas_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, ALLas_32, 0xFFFF);

   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, ALL5s_32, MOSTas_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, ALL5s_32, 0, MOSTas_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, MOSTas_32, 0xFFFF);

   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, ALL5s_32, ALLfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, ALL5s_32, 0, ALLfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, ALLfs_32, 0xFFFF);

   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, ALL5s_32, MOSTfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, ALL5s_32, 0, MOSTfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, MOSTfs_32, 0xFFFF);

   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, ALL5s_32, UP_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, ALL5s_32, 0, UP_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, UP_32, 0xFFFF);

   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, ALL5s_32, DOWN_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, ALL5s_32, 0, DOWN_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, DOWN_32, 0xFFFF);

   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, ALL5s_32, 0, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, ALL5s_32, 0, 0, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, 0, 0xFFFF);

   printf("Combinations of MOST5s_32 and all other patterns\n");

   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, MOST5s_32, ALL5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, MOST5s_32, 0, ALL5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, ALL5s_32, 0xFFFF);

   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, MOST5s_32, MOST5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, MOST5s_32, 0, MOST5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, MOST5s_32, 0xFFFF);

   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, MOST5s_32, ALLas_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, MOST5s_32, 0, ALLas_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, ALLas_32, 0xFFFF);

   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, MOST5s_32, MOSTas_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, MOST5s_32, 0, MOSTas_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, MOSTas_32, 0xFFFF);

   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, MOST5s_32, ALLfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, MOST5s_32, 0, ALLfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, ALLfs_32, 0xFFFF);

   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, MOST5s_32, MOSTfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, MOST5s_32, 0, MOSTfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, MOSTfs_32, 0xFFFF);

   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, MOST5s_32, UP_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, MOST5s_32, 0, UP_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, UP_32, 0xFFFF);

   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, MOST5s_32, DOWN_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, MOST5s_32, 0, DOWN_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, DOWN_32, 0xFFFF);

   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, MOST5s_32, 0, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, MOST5s_32, 0, 0, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, 0, 0xFFFF);

   printf("Combinations of ALLas_32 and all other patterns\n");

   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, ALLas_32, ALL5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, ALLas_32, 0, ALL5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, ALL5s_32, 0xFFFF);

   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, ALLas_32, MOST5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, ALLas_32, 0, MOST5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, MOST5s_32, 0xFFFF);

   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, ALLas_32, ALLas_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, ALLas_32, 0, ALLas_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, ALLas_32, 0xFFFF);

   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, ALLas_32, MOSTas_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, ALLas_32, 0, MOSTas_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, MOSTas_32, 0xFFFF);

   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, ALLas_32, ALLfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, ALLas_32, 0, ALLfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, ALLfs_32, 0xFFFF);

   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, ALLas_32, MOSTfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, ALLas_32, 0, MOSTfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, MOSTfs_32, 0xFFFF);

   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, ALLas_32, UP_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, ALLas_32, 0, UP_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, UP_32, 0xFFFF);

   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, ALLas_32, DOWN_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, ALLas_32, 0, DOWN_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, DOWN_32, 0xFFFF);

   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, ALLas_32, 0, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, ALLas_32, 0, 0, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, 0, 0xFFFF);

   printf("Combinations of MOSTas_32 and all other patterns\n");

   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, MOSTas_32, ALL5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, MOSTas_32, 0, ALL5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, ALL5s_32, 0xFFFF);

   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, MOSTas_32, MOST5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, MOSTas_32, 0, MOST5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, MOST5s_32, 0xFFFF);

   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, MOSTas_32, ALLas_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, MOSTas_32, 0, ALLas_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, ALLas_32, 0xFFFF);

   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, MOSTas_32, MOSTas_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, MOSTas_32, 0, MOSTas_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, MOSTas_32, 0xFFFF);

   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, MOSTas_32, ALLfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, MOSTas_32, 0, ALLfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, ALLfs_32, 0xFFFF);

   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, MOSTas_32, MOSTfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, MOSTas_32, 0, MOSTfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, MOSTfs_32, 0xFFFF);

   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, MOSTas_32, UP_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, MOSTas_32, 0, UP_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, UP_32, 0xFFFF);

   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, MOSTas_32, DOWN_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, MOSTas_32, 0, DOWN_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, DOWN_32, 0xFFFF);

   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, MOSTas_32, 0, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, MOSTas_32, 0, 0, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, 0, 0xFFFF);

   printf("Combinations of ALLfs_32 and all other patterns\n");

   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, ALLfs_32, ALL5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, ALLfs_32, 0, ALL5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, ALL5s_32, 0xFFFF);

   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, ALLfs_32, MOST5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, ALLfs_32, 0, MOST5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, MOST5s_32, 0xFFFF);

   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, ALLfs_32, ALLas_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, ALLfs_32, 0, ALLas_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, ALLas_32, 0xFFFF);

   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, ALLfs_32, MOSTas_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, ALLfs_32, 0, MOSTas_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, MOSTas_32, 0xFFFF);

   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, ALLfs_32, ALLfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, ALLfs_32, 0, ALLfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, ALLfs_32, 0xFFFF);

   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, ALLfs_32, MOSTfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, ALLfs_32, 0, MOSTfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, MOSTfs_32, 0xFFFF);

   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, ALLfs_32, UP_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, ALLfs_32, 0, UP_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, UP_32, 0xFFFF);

   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, ALLfs_32, DOWN_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, ALLfs_32, 0, DOWN_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, DOWN_32, 0xFFFF);

   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, ALLfs_32, 0, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, ALLfs_32, 0, 0, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, 0, 0xFFFF);

   printf("Combinations of MOSTfs_32 and all other patterns\n");

   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, MOSTfs_32, ALL5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, MOSTfs_32, 0, ALL5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, ALL5s_32, 0xFFFF);

   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, MOSTfs_32, MOST5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, MOSTfs_32, 0, MOST5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, MOST5s_32, 0xFFFF);

   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, MOSTfs_32, ALLas_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, MOSTfs_32, 0, ALLas_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, ALLas_32, 0xFFFF);

   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, MOSTfs_32, MOSTas_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, MOSTfs_32, 0, MOSTas_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, MOSTas_32, 0xFFFF);

   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, MOSTfs_32, ALLfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, MOSTfs_32, 0, ALLfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, ALLfs_32, 0xFFFF);

   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, MOSTfs_32, MOSTfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, MOSTfs_32, 0, MOSTfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, MOSTfs_32, 0xFFFF);

   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, MOSTfs_32, UP_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, MOSTfs_32, 0, UP_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, UP_32, 0xFFFF);

   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, MOSTfs_32, DOWN_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, MOSTfs_32, 0, DOWN_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, DOWN_32, 0xFFFF);

   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, MOSTfs_32, 0, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, MOSTfs_32, 0, 0, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, 0, 0xFFFF);

   printf("Combinations of UP_32 and all other patterns\n");

   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, UP_32, ALL5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, UP_32, 0, ALL5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, UP_32, UP_32, ALL5s_32, 0xFFFF);

   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, UP_32, MOST5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, UP_32, 0, MOST5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, UP_32, UP_32, MOST5s_32, 0xFFFF);

   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, UP_32, ALLas_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, UP_32, 0, ALLas_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, UP_32, UP_32, ALLas_32, 0xFFFF);

   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, UP_32, MOSTas_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, UP_32, 0, MOSTas_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, UP_32, UP_32, MOSTas_32, 0xFFFF);

   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, UP_32, ALLfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, UP_32, 0, ALLfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, UP_32, UP_32, ALLfs_32, 0xFFFF);

   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, UP_32, MOSTfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, UP_32, 0, MOSTfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, UP_32, UP_32, MOSTfs_32, 0xFFFF);

   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, UP_32, UP_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, UP_32, 0, UP_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, UP_32, UP_32, UP_32, 0xFFFF);

   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, UP_32, DOWN_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, UP_32, 0, DOWN_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, UP_32, UP_32, DOWN_32, 0xFFFF);

   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, UP_32, 0, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, UP_32, 0, 0, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, UP_32, UP_32, 0, 0xFFFF);

   printf("Combinations of DOWN_32 and all other patterns\n");

   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, DOWN_32, ALL5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, DOWN_32, 0, ALL5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, ALL5s_32, 0xFFFF);

   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, DOWN_32, MOST5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, DOWN_32, 0, MOST5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, MOST5s_32, 0xFFFF);

   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, DOWN_32, ALLas_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, DOWN_32, 0, ALLas_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, ALLas_32, 0xFFFF);

   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, DOWN_32, MOSTas_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, DOWN_32, 0, MOSTas_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, MOSTas_32, 0xFFFF);

   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, DOWN_32, ALLfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, DOWN_32, 0, ALLfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, ALLfs_32, 0xFFFF);

   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, DOWN_32, MOSTfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, DOWN_32, 0, MOSTfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, MOSTfs_32, 0xFFFF);

   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, DOWN_32, UP_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, DOWN_32, 0, UP_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, UP_32, 0xFFFF);

   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, DOWN_32, DOWN_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, DOWN_32, 0, DOWN_32, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, DOWN_32, 0xFFFF);

   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, 0, DOWN_32, 0, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, DOWN_32, 0, 0, 0xFFFF);
   ATOMIC_TEST_CAS("cash w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, 0, 0xFFFF);

   printf("CASAH <Ws>, <Wt>, [<Xn|SP>]\n\n");

   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, 0, 0, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, 0, 1, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, 1, 0, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, 1, 1, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 1, 0, 0, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 1, 0, 1, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 1, 1, 0, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 1, 1, 1, 0xFFFF);

   printf("Combinations of ALL5s_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, ALL5s_32, ALL5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, ALL5s_32, 0, ALL5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, ALL5s_32, 0xFFFF);

   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, ALL5s_32, MOST5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, ALL5s_32, 0, MOST5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, MOST5s_32, 0xFFFF);

   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, ALL5s_32, ALLas_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, ALL5s_32, 0, ALLas_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, ALLas_32, 0xFFFF);

   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, ALL5s_32, MOSTas_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, ALL5s_32, 0, MOSTas_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, MOSTas_32, 0xFFFF);

   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, ALL5s_32, ALLfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, ALL5s_32, 0, ALLfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, ALLfs_32, 0xFFFF);

   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, ALL5s_32, MOSTfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, ALL5s_32, 0, MOSTfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, MOSTfs_32, 0xFFFF);

   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, ALL5s_32, UP_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, ALL5s_32, 0, UP_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, UP_32, 0xFFFF);

   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, ALL5s_32, DOWN_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, ALL5s_32, 0, DOWN_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, DOWN_32, 0xFFFF);

   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, ALL5s_32, 0, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, ALL5s_32, 0, 0, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, 0, 0xFFFF);

   printf("Combinations of MOST5s_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, MOST5s_32, ALL5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, MOST5s_32, 0, ALL5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, ALL5s_32, 0xFFFF);

   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, MOST5s_32, MOST5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, MOST5s_32, 0, MOST5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, MOST5s_32, 0xFFFF);

   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, MOST5s_32, ALLas_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, MOST5s_32, 0, ALLas_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, ALLas_32, 0xFFFF);

   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, MOST5s_32, MOSTas_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, MOST5s_32, 0, MOSTas_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, MOSTas_32, 0xFFFF);

   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, MOST5s_32, ALLfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, MOST5s_32, 0, ALLfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, ALLfs_32, 0xFFFF);

   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, MOST5s_32, MOSTfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, MOST5s_32, 0, MOSTfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, MOSTfs_32, 0xFFFF);

   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, MOST5s_32, UP_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, MOST5s_32, 0, UP_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, UP_32, 0xFFFF);

   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, MOST5s_32, DOWN_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, MOST5s_32, 0, DOWN_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, DOWN_32, 0xFFFF);

   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, MOST5s_32, 0, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, MOST5s_32, 0, 0, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, 0, 0xFFFF);

   printf("Combinations of ALLas_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, ALLas_32, ALL5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, ALLas_32, 0, ALL5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, ALL5s_32, 0xFFFF);

   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, ALLas_32, MOST5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, ALLas_32, 0, MOST5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, MOST5s_32, 0xFFFF);

   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, ALLas_32, ALLas_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, ALLas_32, 0, ALLas_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, ALLas_32, 0xFFFF);

   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, ALLas_32, MOSTas_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, ALLas_32, 0, MOSTas_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, MOSTas_32, 0xFFFF);

   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, ALLas_32, ALLfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, ALLas_32, 0, ALLfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, ALLfs_32, 0xFFFF);

   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, ALLas_32, MOSTfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, ALLas_32, 0, MOSTfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, MOSTfs_32, 0xFFFF);

   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, ALLas_32, UP_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, ALLas_32, 0, UP_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, UP_32, 0xFFFF);

   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, ALLas_32, DOWN_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, ALLas_32, 0, DOWN_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, DOWN_32, 0xFFFF);

   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, ALLas_32, 0, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, ALLas_32, 0, 0, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, 0, 0xFFFF);

   printf("Combinations of MOSTas_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, MOSTas_32, ALL5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, MOSTas_32, 0, ALL5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, ALL5s_32, 0xFFFF);

   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, MOSTas_32, MOST5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, MOSTas_32, 0, MOST5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, MOST5s_32, 0xFFFF);

   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, MOSTas_32, ALLas_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, MOSTas_32, 0, ALLas_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, ALLas_32, 0xFFFF);

   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, MOSTas_32, MOSTas_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, MOSTas_32, 0, MOSTas_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, MOSTas_32, 0xFFFF);

   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, MOSTas_32, ALLfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, MOSTas_32, 0, ALLfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, ALLfs_32, 0xFFFF);

   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, MOSTas_32, MOSTfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, MOSTas_32, 0, MOSTfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, MOSTfs_32, 0xFFFF);

   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, MOSTas_32, UP_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, MOSTas_32, 0, UP_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, UP_32, 0xFFFF);

   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, MOSTas_32, DOWN_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, MOSTas_32, 0, DOWN_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, DOWN_32, 0xFFFF);

   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, MOSTas_32, 0, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, MOSTas_32, 0, 0, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, 0, 0xFFFF);

   printf("Combinations of ALLfs_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, ALLfs_32, ALL5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, ALLfs_32, 0, ALL5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, ALL5s_32, 0xFFFF);

   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, ALLfs_32, MOST5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, ALLfs_32, 0, MOST5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, MOST5s_32, 0xFFFF);

   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, ALLfs_32, ALLas_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, ALLfs_32, 0, ALLas_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, ALLas_32, 0xFFFF);

   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, ALLfs_32, MOSTas_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, ALLfs_32, 0, MOSTas_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, MOSTas_32, 0xFFFF);

   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, ALLfs_32, ALLfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, ALLfs_32, 0, ALLfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, ALLfs_32, 0xFFFF);

   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, ALLfs_32, MOSTfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, ALLfs_32, 0, MOSTfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, MOSTfs_32, 0xFFFF);

   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, ALLfs_32, UP_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, ALLfs_32, 0, UP_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, UP_32, 0xFFFF);

   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, ALLfs_32, DOWN_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, ALLfs_32, 0, DOWN_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, DOWN_32, 0xFFFF);

   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, ALLfs_32, 0, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, ALLfs_32, 0, 0, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, 0, 0xFFFF);

   printf("Combinations of MOSTfs_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, MOSTfs_32, ALL5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, MOSTfs_32, 0, ALL5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, ALL5s_32, 0xFFFF);

   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, MOSTfs_32, MOST5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, MOSTfs_32, 0, MOST5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, MOST5s_32, 0xFFFF);

   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, MOSTfs_32, ALLas_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, MOSTfs_32, 0, ALLas_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, ALLas_32, 0xFFFF);

   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, MOSTfs_32, MOSTas_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, MOSTfs_32, 0, MOSTas_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, MOSTas_32, 0xFFFF);

   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, MOSTfs_32, ALLfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, MOSTfs_32, 0, ALLfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, ALLfs_32, 0xFFFF);

   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, MOSTfs_32, MOSTfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, MOSTfs_32, 0, MOSTfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, MOSTfs_32, 0xFFFF);

   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, MOSTfs_32, UP_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, MOSTfs_32, 0, UP_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, UP_32, 0xFFFF);

   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, MOSTfs_32, DOWN_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, MOSTfs_32, 0, DOWN_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, DOWN_32, 0xFFFF);

   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, MOSTfs_32, 0, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, MOSTfs_32, 0, 0, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, 0, 0xFFFF);

   printf("Combinations of UP_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, UP_32, ALL5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, UP_32, 0, ALL5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, UP_32, UP_32, ALL5s_32, 0xFFFF);

   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, UP_32, MOST5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, UP_32, 0, MOST5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, UP_32, UP_32, MOST5s_32, 0xFFFF);

   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, UP_32, ALLas_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, UP_32, 0, ALLas_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, UP_32, UP_32, ALLas_32, 0xFFFF);

   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, UP_32, MOSTas_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, UP_32, 0, MOSTas_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, UP_32, UP_32, MOSTas_32, 0xFFFF);

   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, UP_32, ALLfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, UP_32, 0, ALLfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, UP_32, UP_32, ALLfs_32, 0xFFFF);

   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, UP_32, MOSTfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, UP_32, 0, MOSTfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, UP_32, UP_32, MOSTfs_32, 0xFFFF);

   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, UP_32, UP_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, UP_32, 0, UP_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, UP_32, UP_32, UP_32, 0xFFFF);

   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, UP_32, DOWN_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, UP_32, 0, DOWN_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, UP_32, UP_32, DOWN_32, 0xFFFF);

   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, UP_32, 0, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, UP_32, 0, 0, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, UP_32, UP_32, 0, 0xFFFF);

   printf("Combinations of DOWN_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, DOWN_32, ALL5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, DOWN_32, 0, ALL5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, ALL5s_32, 0xFFFF);

   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, DOWN_32, MOST5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, DOWN_32, 0, MOST5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, MOST5s_32, 0xFFFF);

   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, DOWN_32, ALLas_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, DOWN_32, 0, ALLas_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, ALLas_32, 0xFFFF);

   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, DOWN_32, MOSTas_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, DOWN_32, 0, MOSTas_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, MOSTas_32, 0xFFFF);

   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, DOWN_32, ALLfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, DOWN_32, 0, ALLfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, ALLfs_32, 0xFFFF);

   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, DOWN_32, MOSTfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, DOWN_32, 0, MOSTfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, MOSTfs_32, 0xFFFF);

   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, DOWN_32, UP_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, DOWN_32, 0, UP_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, UP_32, 0xFFFF);

   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, DOWN_32, DOWN_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, DOWN_32, 0, DOWN_32, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, DOWN_32, 0xFFFF);

   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, 0, DOWN_32, 0, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, DOWN_32, 0, 0, 0xFFFF);
   ATOMIC_TEST_CAS("casah w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, 0, 0xFFFF);

   printf("CASALH <Ws>, <Wt>, [<Xn|SP>]\n\n");

   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, 0, 0, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, 0, 1, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, 1, 0, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, 1, 1, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 1, 0, 0, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 1, 0, 1, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 1, 1, 0, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 1, 1, 1, 0xFFFF);

   printf("Combinations of ALL5s_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, ALL5s_32, ALL5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, ALL5s_32, 0, ALL5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, ALL5s_32, 0xFFFF);

   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, ALL5s_32, MOST5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, ALL5s_32, 0, MOST5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, MOST5s_32, 0xFFFF);

   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, ALL5s_32, ALLas_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, ALL5s_32, 0, ALLas_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, ALLas_32, 0xFFFF);

   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, ALL5s_32, MOSTas_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, ALL5s_32, 0, MOSTas_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, MOSTas_32, 0xFFFF);

   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, ALL5s_32, ALLfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, ALL5s_32, 0, ALLfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, ALLfs_32, 0xFFFF);

   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, ALL5s_32, MOSTfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, ALL5s_32, 0, MOSTfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, MOSTfs_32, 0xFFFF);

   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, ALL5s_32, UP_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, ALL5s_32, 0, UP_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, UP_32, 0xFFFF);

   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, ALL5s_32, DOWN_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, ALL5s_32, 0, DOWN_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, DOWN_32, 0xFFFF);

   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, ALL5s_32, 0, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, ALL5s_32, 0, 0, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, 0, 0xFFFF);

   printf("Combinations of MOST5s_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, MOST5s_32, ALL5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, MOST5s_32, 0, ALL5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, ALL5s_32, 0xFFFF);

   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, MOST5s_32, MOST5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, MOST5s_32, 0, MOST5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, MOST5s_32, 0xFFFF);

   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, MOST5s_32, ALLas_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, MOST5s_32, 0, ALLas_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, ALLas_32, 0xFFFF);

   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, MOST5s_32, MOSTas_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, MOST5s_32, 0, MOSTas_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, MOSTas_32, 0xFFFF);

   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, MOST5s_32, ALLfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, MOST5s_32, 0, ALLfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, ALLfs_32, 0xFFFF);

   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, MOST5s_32, MOSTfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, MOST5s_32, 0, MOSTfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, MOSTfs_32, 0xFFFF);

   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, MOST5s_32, UP_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, MOST5s_32, 0, UP_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, UP_32, 0xFFFF);

   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, MOST5s_32, DOWN_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, MOST5s_32, 0, DOWN_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, DOWN_32, 0xFFFF);

   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, MOST5s_32, 0, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, MOST5s_32, 0, 0, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, 0, 0xFFFF);

   printf("Combinations of ALLas_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, ALLas_32, ALL5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, ALLas_32, 0, ALL5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, ALL5s_32, 0xFFFF);

   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, ALLas_32, MOST5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, ALLas_32, 0, MOST5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, MOST5s_32, 0xFFFF);

   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, ALLas_32, ALLas_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, ALLas_32, 0, ALLas_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, ALLas_32, 0xFFFF);

   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, ALLas_32, MOSTas_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, ALLas_32, 0, MOSTas_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, MOSTas_32, 0xFFFF);

   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, ALLas_32, ALLfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, ALLas_32, 0, ALLfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, ALLfs_32, 0xFFFF);

   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, ALLas_32, MOSTfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, ALLas_32, 0, MOSTfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, MOSTfs_32, 0xFFFF);

   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, ALLas_32, UP_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, ALLas_32, 0, UP_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, UP_32, 0xFFFF);

   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, ALLas_32, DOWN_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, ALLas_32, 0, DOWN_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, DOWN_32, 0xFFFF);

   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, ALLas_32, 0, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, ALLas_32, 0, 0, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, 0, 0xFFFF);

   printf("Combinations of MOSTas_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, MOSTas_32, ALL5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, MOSTas_32, 0, ALL5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, ALL5s_32, 0xFFFF);

   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, MOSTas_32, MOST5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, MOSTas_32, 0, MOST5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, MOST5s_32, 0xFFFF);

   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, MOSTas_32, ALLas_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, MOSTas_32, 0, ALLas_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, ALLas_32, 0xFFFF);

   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, MOSTas_32, MOSTas_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, MOSTas_32, 0, MOSTas_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, MOSTas_32, 0xFFFF);

   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, MOSTas_32, ALLfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, MOSTas_32, 0, ALLfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, ALLfs_32, 0xFFFF);

   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, MOSTas_32, MOSTfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, MOSTas_32, 0, MOSTfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, MOSTfs_32, 0xFFFF);

   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, MOSTas_32, UP_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, MOSTas_32, 0, UP_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, UP_32, 0xFFFF);

   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, MOSTas_32, DOWN_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, MOSTas_32, 0, DOWN_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, DOWN_32, 0xFFFF);

   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, MOSTas_32, 0, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, MOSTas_32, 0, 0, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, 0, 0xFFFF);

   printf("Combinations of ALLfs_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, ALLfs_32, ALL5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, ALLfs_32, 0, ALL5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, ALL5s_32, 0xFFFF);

   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, ALLfs_32, MOST5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, ALLfs_32, 0, MOST5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, MOST5s_32, 0xFFFF);

   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, ALLfs_32, ALLas_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, ALLfs_32, 0, ALLas_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, ALLas_32, 0xFFFF);

   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, ALLfs_32, MOSTas_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, ALLfs_32, 0, MOSTas_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, MOSTas_32, 0xFFFF);

   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, ALLfs_32, ALLfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, ALLfs_32, 0, ALLfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, ALLfs_32, 0xFFFF);

   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, ALLfs_32, MOSTfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, ALLfs_32, 0, MOSTfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, MOSTfs_32, 0xFFFF);

   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, ALLfs_32, UP_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, ALLfs_32, 0, UP_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, UP_32, 0xFFFF);

   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, ALLfs_32, DOWN_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, ALLfs_32, 0, DOWN_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, DOWN_32, 0xFFFF);

   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, ALLfs_32, 0, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, ALLfs_32, 0, 0, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, 0, 0xFFFF);

   printf("Combinations of MOSTfs_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, MOSTfs_32, ALL5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, MOSTfs_32, 0, ALL5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, ALL5s_32, 0xFFFF);

   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, MOSTfs_32, MOST5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, MOSTfs_32, 0, MOST5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, MOST5s_32, 0xFFFF);

   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, MOSTfs_32, ALLas_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, MOSTfs_32, 0, ALLas_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, ALLas_32, 0xFFFF);

   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, MOSTfs_32, MOSTas_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, MOSTfs_32, 0, MOSTas_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, MOSTas_32, 0xFFFF);

   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, MOSTfs_32, ALLfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, MOSTfs_32, 0, ALLfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, ALLfs_32, 0xFFFF);

   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, MOSTfs_32, MOSTfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, MOSTfs_32, 0, MOSTfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, MOSTfs_32, 0xFFFF);

   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, MOSTfs_32, UP_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, MOSTfs_32, 0, UP_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, UP_32, 0xFFFF);

   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, MOSTfs_32, DOWN_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, MOSTfs_32, 0, DOWN_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, DOWN_32, 0xFFFF);

   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, MOSTfs_32, 0, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, MOSTfs_32, 0, 0, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, 0, 0xFFFF);

   printf("Combinations of UP_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, UP_32, ALL5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, UP_32, 0, ALL5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, UP_32, UP_32, ALL5s_32, 0xFFFF);

   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, UP_32, MOST5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, UP_32, 0, MOST5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, UP_32, UP_32, MOST5s_32, 0xFFFF);

   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, UP_32, ALLas_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, UP_32, 0, ALLas_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, UP_32, UP_32, ALLas_32, 0xFFFF);

   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, UP_32, MOSTas_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, UP_32, 0, MOSTas_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, UP_32, UP_32, MOSTas_32, 0xFFFF);

   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, UP_32, ALLfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, UP_32, 0, ALLfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, UP_32, UP_32, ALLfs_32, 0xFFFF);

   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, UP_32, MOSTfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, UP_32, 0, MOSTfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, UP_32, UP_32, MOSTfs_32, 0xFFFF);

   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, UP_32, UP_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, UP_32, 0, UP_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, UP_32, UP_32, UP_32, 0xFFFF);

   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, UP_32, DOWN_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, UP_32, 0, DOWN_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, UP_32, UP_32, DOWN_32, 0xFFFF);

   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, UP_32, 0, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, UP_32, 0, 0, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, UP_32, UP_32, 0, 0xFFFF);

   printf("Combinations of DOWN_32 and all other patterns\n");

   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, DOWN_32, ALL5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, DOWN_32, 0, ALL5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, ALL5s_32, 0xFFFF);

   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, DOWN_32, MOST5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, DOWN_32, 0, MOST5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, MOST5s_32, 0xFFFF);

   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, DOWN_32, ALLas_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, DOWN_32, 0, ALLas_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, ALLas_32, 0xFFFF);

   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, DOWN_32, MOSTas_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, DOWN_32, 0, MOSTas_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, MOSTas_32, 0xFFFF);

   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, DOWN_32, ALLfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, DOWN_32, 0, ALLfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, ALLfs_32, 0xFFFF);

   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, DOWN_32, MOSTfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, DOWN_32, 0, MOSTfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, MOSTfs_32, 0xFFFF);

   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, DOWN_32, UP_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, DOWN_32, 0, UP_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, UP_32, 0xFFFF);

   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, DOWN_32, DOWN_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, DOWN_32, 0, DOWN_32, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, DOWN_32, 0xFFFF);

   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, 0, DOWN_32, 0, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, DOWN_32, 0, 0, 0xFFFF);
   ATOMIC_TEST_CAS("casalh w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, 0, 0xFFFF);

   printf("CASLH <Ws>, <Wt>, [<Xn|SP>]\n\n");

   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, 0, 0, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, 0, 1, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, 1, 0, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, 1, 1, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 1, 0, 0, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 1, 0, 1, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 1, 1, 0, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 1, 1, 1, 0xFFFF);

   printf("Combinations of ALL5s_32 and all other patterns\n");

   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, ALL5s_32, ALL5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, ALL5s_32, 0, ALL5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, ALL5s_32, 0xFFFF);

   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, ALL5s_32, MOST5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, ALL5s_32, 0, MOST5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, MOST5s_32, 0xFFFF);

   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, ALL5s_32, ALLas_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, ALL5s_32, 0, ALLas_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, ALLas_32, 0xFFFF);

   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, ALL5s_32, MOSTas_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, ALL5s_32, 0, MOSTas_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, MOSTas_32, 0xFFFF);

   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, ALL5s_32, ALLfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, ALL5s_32, 0, ALLfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, ALLfs_32, 0xFFFF);

   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, ALL5s_32, MOSTfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, ALL5s_32, 0, MOSTfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, MOSTfs_32, 0xFFFF);

   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, ALL5s_32, UP_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, ALL5s_32, 0, UP_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, UP_32, 0xFFFF);

   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, ALL5s_32, DOWN_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, ALL5s_32, 0, DOWN_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, DOWN_32, 0xFFFF);

   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, ALL5s_32, 0, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, ALL5s_32, 0, 0, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, ALL5s_32, ALL5s_32, 0, 0xFFFF);

   printf("Combinations of MOST5s_32 and all other patterns\n");

   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, MOST5s_32, ALL5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, MOST5s_32, 0, ALL5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, ALL5s_32, 0xFFFF);

   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, MOST5s_32, MOST5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, MOST5s_32, 0, MOST5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, MOST5s_32, 0xFFFF);

   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, MOST5s_32, ALLas_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, MOST5s_32, 0, ALLas_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, ALLas_32, 0xFFFF);

   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, MOST5s_32, MOSTas_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, MOST5s_32, 0, MOSTas_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, MOSTas_32, 0xFFFF);

   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, MOST5s_32, ALLfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, MOST5s_32, 0, ALLfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, ALLfs_32, 0xFFFF);

   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, MOST5s_32, MOSTfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, MOST5s_32, 0, MOSTfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, MOSTfs_32, 0xFFFF);

   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, MOST5s_32, UP_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, MOST5s_32, 0, UP_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, UP_32, 0xFFFF);

   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, MOST5s_32, DOWN_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, MOST5s_32, 0, DOWN_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, DOWN_32, 0xFFFF);

   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, MOST5s_32, 0, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, MOST5s_32, 0, 0, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, MOST5s_32, MOST5s_32, 0, 0xFFFF);

   printf("Combinations of ALLas_32 and all other patterns\n");

   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, ALLas_32, ALL5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, ALLas_32, 0, ALL5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, ALL5s_32, 0xFFFF);

   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, ALLas_32, MOST5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, ALLas_32, 0, MOST5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, MOST5s_32, 0xFFFF);

   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, ALLas_32, ALLas_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, ALLas_32, 0, ALLas_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, ALLas_32, 0xFFFF);

   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, ALLas_32, MOSTas_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, ALLas_32, 0, MOSTas_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, MOSTas_32, 0xFFFF);

   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, ALLas_32, ALLfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, ALLas_32, 0, ALLfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, ALLfs_32, 0xFFFF);

   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, ALLas_32, MOSTfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, ALLas_32, 0, MOSTfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, MOSTfs_32, 0xFFFF);

   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, ALLas_32, UP_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, ALLas_32, 0, UP_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, UP_32, 0xFFFF);

   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, ALLas_32, DOWN_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, ALLas_32, 0, DOWN_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, DOWN_32, 0xFFFF);

   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, ALLas_32, 0, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, ALLas_32, 0, 0, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, ALLas_32, ALLas_32, 0, 0xFFFF);

   printf("Combinations of MOSTas_32 and all other patterns\n");

   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, MOSTas_32, ALL5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, MOSTas_32, 0, ALL5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, ALL5s_32, 0xFFFF);

   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, MOSTas_32, MOST5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, MOSTas_32, 0, MOST5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, MOST5s_32, 0xFFFF);

   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, MOSTas_32, ALLas_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, MOSTas_32, 0, ALLas_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, ALLas_32, 0xFFFF);

   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, MOSTas_32, MOSTas_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, MOSTas_32, 0, MOSTas_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, MOSTas_32, 0xFFFF);

   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, MOSTas_32, ALLfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, MOSTas_32, 0, ALLfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, ALLfs_32, 0xFFFF);

   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, MOSTas_32, MOSTfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, MOSTas_32, 0, MOSTfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, MOSTfs_32, 0xFFFF);

   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, MOSTas_32, UP_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, MOSTas_32, 0, UP_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, UP_32, 0xFFFF);

   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, MOSTas_32, DOWN_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, MOSTas_32, 0, DOWN_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, DOWN_32, 0xFFFF);

   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, MOSTas_32, 0, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, MOSTas_32, 0, 0, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, MOSTas_32, MOSTas_32, 0, 0xFFFF);

   printf("Combinations of ALLfs_32 and all other patterns\n");

   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, ALLfs_32, ALL5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, ALLfs_32, 0, ALL5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, ALL5s_32, 0xFFFF);

   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, ALLfs_32, MOST5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, ALLfs_32, 0, MOST5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, MOST5s_32, 0xFFFF);

   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, ALLfs_32, ALLas_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, ALLfs_32, 0, ALLas_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, ALLas_32, 0xFFFF);

   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, ALLfs_32, MOSTas_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, ALLfs_32, 0, MOSTas_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, MOSTas_32, 0xFFFF);

   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, ALLfs_32, ALLfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, ALLfs_32, 0, ALLfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, ALLfs_32, 0xFFFF);

   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, ALLfs_32, MOSTfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, ALLfs_32, 0, MOSTfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, MOSTfs_32, 0xFFFF);

   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, ALLfs_32, UP_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, ALLfs_32, 0, UP_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, UP_32, 0xFFFF);

   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, ALLfs_32, DOWN_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, ALLfs_32, 0, DOWN_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, DOWN_32, 0xFFFF);

   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, ALLfs_32, 0, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, ALLfs_32, 0, 0, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, ALLfs_32, ALLfs_32, 0, 0xFFFF);

   printf("Combinations of MOSTfs_32 and all other patterns\n");

   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, MOSTfs_32, ALL5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, MOSTfs_32, 0, ALL5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, ALL5s_32, 0xFFFF);

   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, MOSTfs_32, MOST5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, MOSTfs_32, 0, MOST5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, MOST5s_32, 0xFFFF);

   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, MOSTfs_32, ALLas_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, MOSTfs_32, 0, ALLas_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, ALLas_32, 0xFFFF);

   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, MOSTfs_32, MOSTas_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, MOSTfs_32, 0, MOSTas_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, MOSTas_32, 0xFFFF);

   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, MOSTfs_32, ALLfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, MOSTfs_32, 0, ALLfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, ALLfs_32, 0xFFFF);

   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, MOSTfs_32, MOSTfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, MOSTfs_32, 0, MOSTfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, MOSTfs_32, 0xFFFF);

   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, MOSTfs_32, UP_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, MOSTfs_32, 0, UP_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, UP_32, 0xFFFF);

   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, MOSTfs_32, DOWN_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, MOSTfs_32, 0, DOWN_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, DOWN_32, 0xFFFF);

   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, MOSTfs_32, 0, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, MOSTfs_32, 0, 0, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, MOSTfs_32, MOSTfs_32, 0, 0xFFFF);

   printf("Combinations of UP_32 and all other patterns\n");

   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, UP_32, ALL5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, UP_32, 0, ALL5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, UP_32, UP_32, ALL5s_32, 0xFFFF);

   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, UP_32, MOST5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, UP_32, 0, MOST5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, UP_32, UP_32, MOST5s_32, 0xFFFF);

   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, UP_32, ALLas_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, UP_32, 0, ALLas_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, UP_32, UP_32, ALLas_32, 0xFFFF);

   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, UP_32, MOSTas_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, UP_32, 0, MOSTas_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, UP_32, UP_32, MOSTas_32, 0xFFFF);

   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, UP_32, ALLfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, UP_32, 0, ALLfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, UP_32, UP_32, ALLfs_32, 0xFFFF);

   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, UP_32, MOSTfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, UP_32, 0, MOSTfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, UP_32, UP_32, MOSTfs_32, 0xFFFF);

   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, UP_32, UP_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, UP_32, 0, UP_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, UP_32, UP_32, UP_32, 0xFFFF);

   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, UP_32, DOWN_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, UP_32, 0, DOWN_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, UP_32, UP_32, DOWN_32, 0xFFFF);

   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, UP_32, 0, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, UP_32, 0, 0, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, UP_32, UP_32, 0, 0xFFFF);

   printf("Combinations of DOWN_32 and all other patterns\n");

   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, DOWN_32, ALL5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, DOWN_32, 0, ALL5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, ALL5s_32, 0xFFFF);

   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, DOWN_32, MOST5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, DOWN_32, 0, MOST5s_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, MOST5s_32, 0xFFFF);

   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, DOWN_32, ALLas_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, DOWN_32, 0, ALLas_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, ALLas_32, 0xFFFF);

   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, DOWN_32, MOSTas_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, DOWN_32, 0, MOSTas_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, MOSTas_32, 0xFFFF);

   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, DOWN_32, ALLfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, DOWN_32, 0, ALLfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, ALLfs_32, 0xFFFF);

   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, DOWN_32, MOSTfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, DOWN_32, 0, MOSTfs_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, MOSTfs_32, 0xFFFF);

   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, DOWN_32, UP_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, DOWN_32, 0, UP_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, UP_32, 0xFFFF);

   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, DOWN_32, DOWN_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, DOWN_32, 0, DOWN_32, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, DOWN_32, 0xFFFF);

   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, 0, DOWN_32, 0, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, DOWN_32, 0, 0, 0xFFFF);
   ATOMIC_TEST_CAS("caslh w11, w12, [x5]", w, mem, DOWN_32, DOWN_32, 0, 0xFFFF);

   free(mem);

   ULong *memp = (ULong *)malloc(sizeof(ULong) * 2);

   printf("CASP <Ws>, <W(s+1)>, <Wt>, <W(t+1)>, [<Xn|SP>{,#0}]\n\n");

   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, 0, 0, 0, 0, 0, 0);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, 0, 0, 0, 0, 1, 1);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, 0, 0, 1, 1, 0, 0);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, 0, 0, 1, 1, 1, 1);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, 1, 1, 0, 0, 0, 0);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, 1, 1, 0, 0, 1, 1);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, 1, 1, 1, 1, 0, 0);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, 1, 1, 1, 1, 1, 1);

   printf("Combinations of ALL5s_32 and all other patterns\n");

   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALL5s_32, ALL5s_32, ALL5s_32, ALL5s_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, ALL5s_32, ALL5s_32, 0, 0, ALL5s_32, ALL5s_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, ALL5s_32, ALL5s_32, ALL5s_32, ALL5s_32, ALL5s_32, ALL5s_32);

   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALL5s_32, ALL5s_32, ALLas_32, ALLas_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, ALL5s_32, ALL5s_32, 0, 0, ALLas_32, ALLas_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, ALL5s_32, ALL5s_32, ALL5s_32, ALL5s_32, ALLas_32, ALLas_32);

   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALL5s_32, ALL5s_32, ALLfs_32, ALLfs_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, ALL5s_32, ALL5s_32, 0, 0, ALLfs_32, ALLfs_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, ALL5s_32, ALL5s_32, ALL5s_32, ALL5s_32, ALLfs_32, ALLfs_32);

   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALL5s_32, ALL5s_32, UP_32, UP_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, ALL5s_32, ALL5s_32, 0, 0, UP_32, UP_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, ALL5s_32, ALL5s_32, ALL5s_32, ALL5s_32, UP_32, UP_32);

   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALL5s_32, ALL5s_32, DOWN_32, DOWN_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, ALL5s_32, ALL5s_32, 0, 0, DOWN_32, DOWN_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, ALL5s_32, ALL5s_32, ALL5s_32, ALL5s_32, DOWN_32, DOWN_32);

   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALL5s_32, ALL5s_32, PI_32, PI_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, ALL5s_32, ALL5s_32, 0, 0, PI_32, PI_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, ALL5s_32, ALL5s_32, ALL5s_32, ALL5s_32, PI_32, PI_32);

   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALL5s_32, ALL5s_32, E_32, E_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, ALL5s_32, ALL5s_32, 0, 0, E_32, E_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, ALL5s_32, ALL5s_32, ALL5s_32, ALL5s_32, E_32, E_32);

   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALL5s_32, ALL5s_32, 0, 0);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, ALL5s_32, ALL5s_32, 0, 0, 0, 0);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, ALL5s_32, ALL5s_32, ALL5s_32, ALL5s_32, 0, 0);

   printf("Combinations of ALLas_32 and all other patterns\n");

   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALLas_32, ALLas_32, ALL5s_32, ALL5s_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, ALLas_32, ALLas_32, 0, 0, ALL5s_32, ALL5s_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, ALLas_32, ALLas_32, ALLas_32, ALLas_32, ALL5s_32, ALL5s_32);

   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALLas_32, ALLas_32, ALLas_32, ALLas_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, ALLas_32, ALLas_32, 0, 0, ALLas_32, ALLas_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, ALLas_32, ALLas_32, ALLas_32, ALLas_32, ALLas_32, ALLas_32);

   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALLas_32, ALLas_32, ALLfs_32, ALLfs_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, ALLas_32, ALLas_32, 0, 0, ALLfs_32, ALLfs_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, ALLas_32, ALLas_32, ALLas_32, ALLas_32, ALLfs_32, ALLfs_32);

   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALLas_32, ALLas_32, UP_32, UP_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, ALLas_32, ALLas_32, 0, 0, UP_32, UP_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, ALLas_32, ALLas_32, ALLas_32, ALLas_32, UP_32, UP_32);

   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALLas_32, ALLas_32, DOWN_32, DOWN_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, ALLas_32, ALLas_32, 0, 0, DOWN_32, DOWN_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, ALLas_32, ALLas_32, ALLas_32, ALLas_32, DOWN_32, DOWN_32);

   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALLas_32, ALLas_32, PI_32, PI_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, ALLas_32, ALLas_32, 0, 0, PI_32, PI_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, ALLas_32, ALLas_32, ALLas_32, ALLas_32, PI_32, PI_32);

   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALLas_32, ALLas_32, E_32, E_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, ALLas_32, ALLas_32, 0, 0, E_32, E_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, ALLas_32, ALLas_32, ALLas_32, ALLas_32, E_32, E_32);

   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALLas_32, ALLas_32, 0, 0);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, ALLas_32, ALLas_32, 0, 0, 0, 0);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, ALLas_32, ALLas_32, ALLas_32, ALLas_32, 0, 0);

   printf("Combinations of ALLfs_32 and all other patterns\n");

   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALLfs_32, ALLfs_32, ALL5s_32, ALL5s_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, ALLfs_32, ALLfs_32, 0, 0, ALL5s_32, ALL5s_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, ALLfs_32, ALLfs_32, ALLfs_32, ALLfs_32, ALL5s_32, ALL5s_32);

   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALLfs_32, ALLfs_32, ALLas_32, ALLas_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, ALLfs_32, ALLfs_32, 0, 0, ALLas_32, ALLas_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, ALLfs_32, ALLfs_32, ALLfs_32, ALLfs_32, ALLas_32, ALLas_32);

   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALLfs_32, ALLfs_32, ALLfs_32, ALLfs_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, ALLfs_32, ALLfs_32, 0, 0, ALLfs_32, ALLfs_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, ALLfs_32, ALLfs_32, ALLfs_32, ALLfs_32, ALLfs_32, ALLfs_32);

   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALLfs_32, ALLfs_32, UP_32, UP_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, ALLfs_32, ALLfs_32, 0, 0, UP_32, UP_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, ALLfs_32, ALLfs_32, ALLfs_32, ALLfs_32, UP_32, UP_32);

   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALLfs_32, ALLfs_32, DOWN_32, DOWN_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, ALLfs_32, ALLfs_32, 0, 0, DOWN_32, DOWN_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, ALLfs_32, ALLfs_32, ALLfs_32, ALLfs_32, DOWN_32, DOWN_32);

   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALLfs_32, ALLfs_32, PI_32, PI_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, ALLfs_32, ALLfs_32, 0, 0, PI_32, PI_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, ALLfs_32, ALLfs_32, ALLfs_32, ALLfs_32, PI_32, PI_32);

   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALLfs_32, ALLfs_32, E_32, E_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, ALLfs_32, ALLfs_32, 0, 0, E_32, E_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, ALLfs_32, ALLfs_32, ALLfs_32, ALLfs_32, E_32, E_32);

   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALLfs_32, ALLfs_32, 0, 0);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, ALLfs_32, ALLfs_32, 0, 0, 0, 0);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, ALLfs_32, ALLfs_32, ALLfs_32, ALLfs_32, 0, 0);

   printf("Combinations of UP_32 and all other patterns\n");

   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, 0, 0, UP_32, UP_32, ALL5s_32, ALL5s_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, UP_32, UP_32, 0, 0, ALL5s_32, ALL5s_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, UP_32, UP_32, UP_32, UP_32, ALL5s_32, ALL5s_32);

   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, 0, 0, UP_32, UP_32, ALLas_32, ALLas_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, UP_32, UP_32, 0, 0, ALLas_32, ALLas_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, UP_32, UP_32, UP_32, UP_32, ALLas_32, ALLas_32);

   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, 0, 0, UP_32, UP_32, ALLfs_32, ALLfs_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, UP_32, UP_32, 0, 0, ALLfs_32, ALLfs_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, UP_32, UP_32, UP_32, UP_32, ALLfs_32, ALLfs_32);

   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, 0, 0, UP_32, UP_32, UP_32, UP_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, UP_32, UP_32, 0, 0, UP_32, UP_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, UP_32, UP_32, UP_32, UP_32, UP_32, UP_32);

   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, 0, 0, UP_32, UP_32, DOWN_32, DOWN_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, UP_32, UP_32, 0, 0, DOWN_32, DOWN_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, UP_32, UP_32, UP_32, UP_32, DOWN_32, DOWN_32);

   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, 0, 0, UP_32, UP_32, PI_32, PI_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, UP_32, UP_32, 0, 0, PI_32, PI_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, UP_32, UP_32, UP_32, UP_32, PI_32, PI_32);

   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, 0, 0, UP_32, UP_32, E_32, E_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, UP_32, UP_32, 0, 0, E_32, E_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, UP_32, UP_32, UP_32, UP_32, E_32, E_32);

   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, 0, 0, UP_32, UP_32, 0, 0);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, UP_32, UP_32, 0, 0, 0, 0);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, UP_32, UP_32, UP_32, UP_32, 0, 0);

   printf("Combinations of DOWN_32 and all other patterns\n");

   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, 0, 0, DOWN_32, DOWN_32, ALL5s_32, ALL5s_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, DOWN_32, DOWN_32, 0, 0, ALL5s_32, ALL5s_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, DOWN_32, DOWN_32, DOWN_32, DOWN_32, ALL5s_32, ALL5s_32);

   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, 0, 0, DOWN_32, DOWN_32, ALLas_32, ALLas_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, DOWN_32, DOWN_32, 0, 0, ALLas_32, ALLas_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, DOWN_32, DOWN_32, DOWN_32, DOWN_32, ALLas_32, ALLas_32);

   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, 0, 0, DOWN_32, DOWN_32, ALLfs_32, ALLfs_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, DOWN_32, DOWN_32, 0, 0, ALLfs_32, ALLfs_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, DOWN_32, DOWN_32, DOWN_32, DOWN_32, ALLfs_32, ALLfs_32);

   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, 0, 0, DOWN_32, DOWN_32, UP_32, UP_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, DOWN_32, DOWN_32, 0, 0, UP_32, UP_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, DOWN_32, DOWN_32, DOWN_32, DOWN_32, UP_32, UP_32);

   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, 0, 0, DOWN_32, DOWN_32, DOWN_32, DOWN_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, DOWN_32, DOWN_32, 0, 0, DOWN_32, DOWN_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, DOWN_32, DOWN_32, DOWN_32, DOWN_32, DOWN_32, DOWN_32);

   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, 0, 0, DOWN_32, DOWN_32, PI_32, PI_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, DOWN_32, DOWN_32, 0, 0, PI_32, PI_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, DOWN_32, DOWN_32, DOWN_32, DOWN_32, PI_32, PI_32);

   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, 0, 0, DOWN_32, DOWN_32, E_32, E_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, DOWN_32, DOWN_32, 0, 0, E_32, E_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, DOWN_32, DOWN_32, DOWN_32, DOWN_32, E_32, E_32);

   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, 0, 0, DOWN_32, DOWN_32, 0, 0);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, DOWN_32, DOWN_32, 0, 0, 0, 0);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, DOWN_32, DOWN_32, DOWN_32, DOWN_32, 0, 0);

   printf("Combinations of PI_32 and all other patterns\n");

   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, 0, 0, PI_32, PI_32, ALL5s_32, ALL5s_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, PI_32, PI_32, 0, 0, ALL5s_32, ALL5s_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, PI_32, PI_32, PI_32, PI_32, ALL5s_32, ALL5s_32);

   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, 0, 0, PI_32, PI_32, ALLas_32, ALLas_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, PI_32, PI_32, 0, 0, ALLas_32, ALLas_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, PI_32, PI_32, PI_32, PI_32, ALLas_32, ALLas_32);

   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, 0, 0, PI_32, PI_32, ALLfs_32, ALLfs_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, PI_32, PI_32, 0, 0, ALLfs_32, ALLfs_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, PI_32, PI_32, PI_32, PI_32, ALLfs_32, ALLfs_32);

   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, 0, 0, PI_32, PI_32, UP_32, UP_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, PI_32, PI_32, 0, 0, UP_32, UP_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, PI_32, PI_32, PI_32, PI_32, UP_32, UP_32);

   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, 0, 0, PI_32, PI_32, DOWN_32, DOWN_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, PI_32, PI_32, 0, 0, DOWN_32, DOWN_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, PI_32, PI_32, PI_32, PI_32, DOWN_32, DOWN_32);

   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, 0, 0, PI_32, PI_32, PI_32, PI_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, PI_32, PI_32, 0, 0, PI_32, PI_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, PI_32, PI_32, PI_32, PI_32, PI_32, PI_32);

   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, 0, 0, PI_32, PI_32, E_32, E_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, PI_32, PI_32, 0, 0, E_32, E_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, PI_32, PI_32, PI_32, PI_32, E_32, E_32);

   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, 0, 0, PI_32, PI_32, 0, 0);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, PI_32, PI_32, 0, 0, 0, 0);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, PI_32, PI_32, PI_32, PI_32, 0, 0);

   printf("Combinations of E_32 and all other patterns\n");

   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, 0, 0, E_32, E_32, ALL5s_32, ALL5s_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, E_32, E_32, 0, 0, ALL5s_32, ALL5s_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, E_32, E_32, E_32, E_32, ALL5s_32, ALL5s_32);

   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, 0, 0, E_32, E_32, ALLas_32, ALLas_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, E_32, E_32, 0, 0, ALLas_32, ALLas_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, E_32, E_32, E_32, E_32, ALLas_32, ALLas_32);

   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, 0, 0, E_32, E_32, ALLfs_32, ALLfs_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, E_32, E_32, 0, 0, ALLfs_32, ALLfs_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, E_32, E_32, E_32, E_32, ALLfs_32, ALLfs_32);

   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, 0, 0, E_32, E_32, UP_32, UP_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, E_32, E_32, 0, 0, UP_32, UP_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, E_32, E_32, E_32, E_32, UP_32, UP_32);

   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, 0, 0, E_32, E_32, DOWN_32, DOWN_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, E_32, E_32, 0, 0, DOWN_32, DOWN_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, E_32, E_32, E_32, E_32, DOWN_32, DOWN_32);

   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, 0, 0, E_32, E_32, PI_32, PI_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, E_32, E_32, 0, 0, PI_32, PI_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, E_32, E_32, E_32, E_32, PI_32, PI_32);

   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, 0, 0, E_32, E_32, E_32, E_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, E_32, E_32, 0, 0, E_32, E_32);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, E_32, E_32, E_32, E_32, E_32, E_32);

   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, 0, 0, E_32, E_32, 0, 0);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, E_32, E_32, 0, 0, 0, 0);
   ATOMIC_TEST_CASP("casp w8, w9, w10, w11, [x5]", w, memp, E_32, E_32, E_32, E_32, 0, 0);

   printf("CASPA <Ws>, <W(s+1)>, <Wt>, <W(t+1)>, [<Xn|SP>{,#0}]\n\n");

   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, 0, 0, 0, 0, 0, 0);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, 0, 0, 0, 0, 1, 1);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, 0, 0, 1, 1, 0, 0);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, 0, 0, 1, 1, 1, 1);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, 1, 1, 0, 0, 0, 0);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, 1, 1, 0, 0, 1, 1);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, 1, 1, 1, 1, 0, 0);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, 1, 1, 1, 1, 1, 1);

   printf("Combinations of ALL5s_32 and all other patterns\n");

   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALL5s_32, ALL5s_32, ALL5s_32, ALL5s_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, ALL5s_32, ALL5s_32, 0, 0, ALL5s_32, ALL5s_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, ALL5s_32, ALL5s_32, ALL5s_32, ALL5s_32, ALL5s_32, ALL5s_32);

   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALL5s_32, ALL5s_32, ALLas_32, ALLas_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, ALL5s_32, ALL5s_32, 0, 0, ALLas_32, ALLas_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, ALL5s_32, ALL5s_32, ALL5s_32, ALL5s_32, ALLas_32, ALLas_32);

   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALL5s_32, ALL5s_32, ALLfs_32, ALLfs_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, ALL5s_32, ALL5s_32, 0, 0, ALLfs_32, ALLfs_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, ALL5s_32, ALL5s_32, ALL5s_32, ALL5s_32, ALLfs_32, ALLfs_32);

   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALL5s_32, ALL5s_32, UP_32, UP_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, ALL5s_32, ALL5s_32, 0, 0, UP_32, UP_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, ALL5s_32, ALL5s_32, ALL5s_32, ALL5s_32, UP_32, UP_32);

   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALL5s_32, ALL5s_32, DOWN_32, DOWN_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, ALL5s_32, ALL5s_32, 0, 0, DOWN_32, DOWN_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, ALL5s_32, ALL5s_32, ALL5s_32, ALL5s_32, DOWN_32, DOWN_32);

   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALL5s_32, ALL5s_32, PI_32, PI_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, ALL5s_32, ALL5s_32, 0, 0, PI_32, PI_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, ALL5s_32, ALL5s_32, ALL5s_32, ALL5s_32, PI_32, PI_32);

   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALL5s_32, ALL5s_32, E_32, E_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, ALL5s_32, ALL5s_32, 0, 0, E_32, E_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, ALL5s_32, ALL5s_32, ALL5s_32, ALL5s_32, E_32, E_32);

   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALL5s_32, ALL5s_32, 0, 0);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, ALL5s_32, ALL5s_32, 0, 0, 0, 0);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, ALL5s_32, ALL5s_32, ALL5s_32, ALL5s_32, 0, 0);

   printf("Combinations of ALLas_32 and all other patterns\n");

   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALLas_32, ALLas_32, ALL5s_32, ALL5s_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, ALLas_32, ALLas_32, 0, 0, ALL5s_32, ALL5s_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, ALLas_32, ALLas_32, ALLas_32, ALLas_32, ALL5s_32, ALL5s_32);

   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALLas_32, ALLas_32, ALLas_32, ALLas_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, ALLas_32, ALLas_32, 0, 0, ALLas_32, ALLas_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, ALLas_32, ALLas_32, ALLas_32, ALLas_32, ALLas_32, ALLas_32);

   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALLas_32, ALLas_32, ALLfs_32, ALLfs_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, ALLas_32, ALLas_32, 0, 0, ALLfs_32, ALLfs_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, ALLas_32, ALLas_32, ALLas_32, ALLas_32, ALLfs_32, ALLfs_32);

   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALLas_32, ALLas_32, UP_32, UP_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, ALLas_32, ALLas_32, 0, 0, UP_32, UP_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, ALLas_32, ALLas_32, ALLas_32, ALLas_32, UP_32, UP_32);

   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALLas_32, ALLas_32, DOWN_32, DOWN_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, ALLas_32, ALLas_32, 0, 0, DOWN_32, DOWN_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, ALLas_32, ALLas_32, ALLas_32, ALLas_32, DOWN_32, DOWN_32);

   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALLas_32, ALLas_32, PI_32, PI_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, ALLas_32, ALLas_32, 0, 0, PI_32, PI_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, ALLas_32, ALLas_32, ALLas_32, ALLas_32, PI_32, PI_32);

   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALLas_32, ALLas_32, E_32, E_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, ALLas_32, ALLas_32, 0, 0, E_32, E_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, ALLas_32, ALLas_32, ALLas_32, ALLas_32, E_32, E_32);

   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALLas_32, ALLas_32, 0, 0);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, ALLas_32, ALLas_32, 0, 0, 0, 0);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, ALLas_32, ALLas_32, ALLas_32, ALLas_32, 0, 0);

   printf("Combinations of ALLfs_32 and all other patterns\n");

   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALLfs_32, ALLfs_32, ALL5s_32, ALL5s_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, ALLfs_32, ALLfs_32, 0, 0, ALL5s_32, ALL5s_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, ALLfs_32, ALLfs_32, ALLfs_32, ALLfs_32, ALL5s_32, ALL5s_32);

   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALLfs_32, ALLfs_32, ALLas_32, ALLas_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, ALLfs_32, ALLfs_32, 0, 0, ALLas_32, ALLas_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, ALLfs_32, ALLfs_32, ALLfs_32, ALLfs_32, ALLas_32, ALLas_32);

   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALLfs_32, ALLfs_32, ALLfs_32, ALLfs_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, ALLfs_32, ALLfs_32, 0, 0, ALLfs_32, ALLfs_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, ALLfs_32, ALLfs_32, ALLfs_32, ALLfs_32, ALLfs_32, ALLfs_32);

   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALLfs_32, ALLfs_32, UP_32, UP_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, ALLfs_32, ALLfs_32, 0, 0, UP_32, UP_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, ALLfs_32, ALLfs_32, ALLfs_32, ALLfs_32, UP_32, UP_32);

   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALLfs_32, ALLfs_32, DOWN_32, DOWN_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, ALLfs_32, ALLfs_32, 0, 0, DOWN_32, DOWN_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, ALLfs_32, ALLfs_32, ALLfs_32, ALLfs_32, DOWN_32, DOWN_32);

   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALLfs_32, ALLfs_32, PI_32, PI_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, ALLfs_32, ALLfs_32, 0, 0, PI_32, PI_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, ALLfs_32, ALLfs_32, ALLfs_32, ALLfs_32, PI_32, PI_32);

   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALLfs_32, ALLfs_32, E_32, E_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, ALLfs_32, ALLfs_32, 0, 0, E_32, E_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, ALLfs_32, ALLfs_32, ALLfs_32, ALLfs_32, E_32, E_32);

   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALLfs_32, ALLfs_32, 0, 0);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, ALLfs_32, ALLfs_32, 0, 0, 0, 0);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, ALLfs_32, ALLfs_32, ALLfs_32, ALLfs_32, 0, 0);

   printf("Combinations of UP_32 and all other patterns\n");

   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, 0, 0, UP_32, UP_32, ALL5s_32, ALL5s_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, UP_32, UP_32, 0, 0, ALL5s_32, ALL5s_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, UP_32, UP_32, UP_32, UP_32, ALL5s_32, ALL5s_32);

   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, 0, 0, UP_32, UP_32, ALLas_32, ALLas_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, UP_32, UP_32, 0, 0, ALLas_32, ALLas_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, UP_32, UP_32, UP_32, UP_32, ALLas_32, ALLas_32);

   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, 0, 0, UP_32, UP_32, ALLfs_32, ALLfs_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, UP_32, UP_32, 0, 0, ALLfs_32, ALLfs_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, UP_32, UP_32, UP_32, UP_32, ALLfs_32, ALLfs_32);

   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, 0, 0, UP_32, UP_32, UP_32, UP_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, UP_32, UP_32, 0, 0, UP_32, UP_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, UP_32, UP_32, UP_32, UP_32, UP_32, UP_32);

   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, 0, 0, UP_32, UP_32, DOWN_32, DOWN_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, UP_32, UP_32, 0, 0, DOWN_32, DOWN_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, UP_32, UP_32, UP_32, UP_32, DOWN_32, DOWN_32);

   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, 0, 0, UP_32, UP_32, PI_32, PI_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, UP_32, UP_32, 0, 0, PI_32, PI_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, UP_32, UP_32, UP_32, UP_32, PI_32, PI_32);

   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, 0, 0, UP_32, UP_32, E_32, E_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, UP_32, UP_32, 0, 0, E_32, E_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, UP_32, UP_32, UP_32, UP_32, E_32, E_32);

   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, 0, 0, UP_32, UP_32, 0, 0);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, UP_32, UP_32, 0, 0, 0, 0);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, UP_32, UP_32, UP_32, UP_32, 0, 0);

   printf("Combinations of DOWN_32 and all other patterns\n");

   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, 0, 0, DOWN_32, DOWN_32, ALL5s_32, ALL5s_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, DOWN_32, DOWN_32, 0, 0, ALL5s_32, ALL5s_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, DOWN_32, DOWN_32, DOWN_32, DOWN_32, ALL5s_32, ALL5s_32);

   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, 0, 0, DOWN_32, DOWN_32, ALLas_32, ALLas_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, DOWN_32, DOWN_32, 0, 0, ALLas_32, ALLas_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, DOWN_32, DOWN_32, DOWN_32, DOWN_32, ALLas_32, ALLas_32);

   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, 0, 0, DOWN_32, DOWN_32, ALLfs_32, ALLfs_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, DOWN_32, DOWN_32, 0, 0, ALLfs_32, ALLfs_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, DOWN_32, DOWN_32, DOWN_32, DOWN_32, ALLfs_32, ALLfs_32);

   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, 0, 0, DOWN_32, DOWN_32, UP_32, UP_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, DOWN_32, DOWN_32, 0, 0, UP_32, UP_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, DOWN_32, DOWN_32, DOWN_32, DOWN_32, UP_32, UP_32);

   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, 0, 0, DOWN_32, DOWN_32, DOWN_32, DOWN_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, DOWN_32, DOWN_32, 0, 0, DOWN_32, DOWN_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, DOWN_32, DOWN_32, DOWN_32, DOWN_32, DOWN_32, DOWN_32);

   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, 0, 0, DOWN_32, DOWN_32, PI_32, PI_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, DOWN_32, DOWN_32, 0, 0, PI_32, PI_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, DOWN_32, DOWN_32, DOWN_32, DOWN_32, PI_32, PI_32);

   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, 0, 0, DOWN_32, DOWN_32, E_32, E_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, DOWN_32, DOWN_32, 0, 0, E_32, E_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, DOWN_32, DOWN_32, DOWN_32, DOWN_32, E_32, E_32);

   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, 0, 0, DOWN_32, DOWN_32, 0, 0);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, DOWN_32, DOWN_32, 0, 0, 0, 0);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, DOWN_32, DOWN_32, DOWN_32, DOWN_32, 0, 0);

   printf("Combinations of PI_32 and all other patterns\n");

   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, 0, 0, PI_32, PI_32, ALL5s_32, ALL5s_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, PI_32, PI_32, 0, 0, ALL5s_32, ALL5s_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, PI_32, PI_32, PI_32, PI_32, ALL5s_32, ALL5s_32);

   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, 0, 0, PI_32, PI_32, ALLas_32, ALLas_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, PI_32, PI_32, 0, 0, ALLas_32, ALLas_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, PI_32, PI_32, PI_32, PI_32, ALLas_32, ALLas_32);

   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, 0, 0, PI_32, PI_32, ALLfs_32, ALLfs_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, PI_32, PI_32, 0, 0, ALLfs_32, ALLfs_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, PI_32, PI_32, PI_32, PI_32, ALLfs_32, ALLfs_32);

   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, 0, 0, PI_32, PI_32, UP_32, UP_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, PI_32, PI_32, 0, 0, UP_32, UP_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, PI_32, PI_32, PI_32, PI_32, UP_32, UP_32);

   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, 0, 0, PI_32, PI_32, DOWN_32, DOWN_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, PI_32, PI_32, 0, 0, DOWN_32, DOWN_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, PI_32, PI_32, PI_32, PI_32, DOWN_32, DOWN_32);

   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, 0, 0, PI_32, PI_32, PI_32, PI_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, PI_32, PI_32, 0, 0, PI_32, PI_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, PI_32, PI_32, PI_32, PI_32, PI_32, PI_32);

   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, 0, 0, PI_32, PI_32, E_32, E_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, PI_32, PI_32, 0, 0, E_32, E_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, PI_32, PI_32, PI_32, PI_32, E_32, E_32);

   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, 0, 0, PI_32, PI_32, 0, 0);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, PI_32, PI_32, 0, 0, 0, 0);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, PI_32, PI_32, PI_32, PI_32, 0, 0);

   printf("Combinations of E_32 and all other patterns\n");

   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, 0, 0, E_32, E_32, ALL5s_32, ALL5s_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, E_32, E_32, 0, 0, ALL5s_32, ALL5s_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, E_32, E_32, E_32, E_32, ALL5s_32, ALL5s_32);

   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, 0, 0, E_32, E_32, ALLas_32, ALLas_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, E_32, E_32, 0, 0, ALLas_32, ALLas_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, E_32, E_32, E_32, E_32, ALLas_32, ALLas_32);

   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, 0, 0, E_32, E_32, ALLfs_32, ALLfs_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, E_32, E_32, 0, 0, ALLfs_32, ALLfs_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, E_32, E_32, E_32, E_32, ALLfs_32, ALLfs_32);

   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, 0, 0, E_32, E_32, UP_32, UP_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, E_32, E_32, 0, 0, UP_32, UP_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, E_32, E_32, E_32, E_32, UP_32, UP_32);

   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, 0, 0, E_32, E_32, DOWN_32, DOWN_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, E_32, E_32, 0, 0, DOWN_32, DOWN_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, E_32, E_32, E_32, E_32, DOWN_32, DOWN_32);

   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, 0, 0, E_32, E_32, PI_32, PI_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, E_32, E_32, 0, 0, PI_32, PI_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, E_32, E_32, E_32, E_32, PI_32, PI_32);

   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, 0, 0, E_32, E_32, E_32, E_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, E_32, E_32, 0, 0, E_32, E_32);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, E_32, E_32, E_32, E_32, E_32, E_32);

   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, 0, 0, E_32, E_32, 0, 0);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, E_32, E_32, 0, 0, 0, 0);
   ATOMIC_TEST_CASP("caspa w8, w9, w10, w11, [x5]", w, memp, E_32, E_32, E_32, E_32, 0, 0);

   printf("CASPAL <Ws>, <W(s+1)>, <Wt>, <W(t+1)>, [<Xn|SP>{,#0}]\n\n");

   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, 0, 0, 0, 0, 0, 0);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, 0, 0, 0, 0, 1, 1);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, 0, 0, 1, 1, 0, 0);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, 0, 0, 1, 1, 1, 1);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, 1, 1, 0, 0, 0, 0);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, 1, 1, 0, 0, 1, 1);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, 1, 1, 1, 1, 0, 0);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, 1, 1, 1, 1, 1, 1);

   printf("Combinations of ALL5s_32 and all other patterns\n");

   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALL5s_32, ALL5s_32, ALL5s_32, ALL5s_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, ALL5s_32, ALL5s_32, 0, 0, ALL5s_32, ALL5s_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, ALL5s_32, ALL5s_32, ALL5s_32, ALL5s_32, ALL5s_32, ALL5s_32);

   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALL5s_32, ALL5s_32, ALLas_32, ALLas_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, ALL5s_32, ALL5s_32, 0, 0, ALLas_32, ALLas_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, ALL5s_32, ALL5s_32, ALL5s_32, ALL5s_32, ALLas_32, ALLas_32);

   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALL5s_32, ALL5s_32, ALLfs_32, ALLfs_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, ALL5s_32, ALL5s_32, 0, 0, ALLfs_32, ALLfs_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, ALL5s_32, ALL5s_32, ALL5s_32, ALL5s_32, ALLfs_32, ALLfs_32);

   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALL5s_32, ALL5s_32, UP_32, UP_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, ALL5s_32, ALL5s_32, 0, 0, UP_32, UP_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, ALL5s_32, ALL5s_32, ALL5s_32, ALL5s_32, UP_32, UP_32);

   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALL5s_32, ALL5s_32, DOWN_32, DOWN_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, ALL5s_32, ALL5s_32, 0, 0, DOWN_32, DOWN_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, ALL5s_32, ALL5s_32, ALL5s_32, ALL5s_32, DOWN_32, DOWN_32);

   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALL5s_32, ALL5s_32, PI_32, PI_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, ALL5s_32, ALL5s_32, 0, 0, PI_32, PI_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, ALL5s_32, ALL5s_32, ALL5s_32, ALL5s_32, PI_32, PI_32);

   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALL5s_32, ALL5s_32, E_32, E_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, ALL5s_32, ALL5s_32, 0, 0, E_32, E_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, ALL5s_32, ALL5s_32, ALL5s_32, ALL5s_32, E_32, E_32);

   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALL5s_32, ALL5s_32, 0, 0);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, ALL5s_32, ALL5s_32, 0, 0, 0, 0);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, ALL5s_32, ALL5s_32, ALL5s_32, ALL5s_32, 0, 0);

   printf("Combinations of ALLas_32 and all other patterns\n");

   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALLas_32, ALLas_32, ALL5s_32, ALL5s_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, ALLas_32, ALLas_32, 0, 0, ALL5s_32, ALL5s_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, ALLas_32, ALLas_32, ALLas_32, ALLas_32, ALL5s_32, ALL5s_32);

   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALLas_32, ALLas_32, ALLas_32, ALLas_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, ALLas_32, ALLas_32, 0, 0, ALLas_32, ALLas_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, ALLas_32, ALLas_32, ALLas_32, ALLas_32, ALLas_32, ALLas_32);

   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALLas_32, ALLas_32, ALLfs_32, ALLfs_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, ALLas_32, ALLas_32, 0, 0, ALLfs_32, ALLfs_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, ALLas_32, ALLas_32, ALLas_32, ALLas_32, ALLfs_32, ALLfs_32);

   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALLas_32, ALLas_32, UP_32, UP_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, ALLas_32, ALLas_32, 0, 0, UP_32, UP_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, ALLas_32, ALLas_32, ALLas_32, ALLas_32, UP_32, UP_32);

   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALLas_32, ALLas_32, DOWN_32, DOWN_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, ALLas_32, ALLas_32, 0, 0, DOWN_32, DOWN_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, ALLas_32, ALLas_32, ALLas_32, ALLas_32, DOWN_32, DOWN_32);

   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALLas_32, ALLas_32, PI_32, PI_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, ALLas_32, ALLas_32, 0, 0, PI_32, PI_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, ALLas_32, ALLas_32, ALLas_32, ALLas_32, PI_32, PI_32);

   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALLas_32, ALLas_32, E_32, E_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, ALLas_32, ALLas_32, 0, 0, E_32, E_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, ALLas_32, ALLas_32, ALLas_32, ALLas_32, E_32, E_32);

   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALLas_32, ALLas_32, 0, 0);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, ALLas_32, ALLas_32, 0, 0, 0, 0);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, ALLas_32, ALLas_32, ALLas_32, ALLas_32, 0, 0);

   printf("Combinations of ALLfs_32 and all other patterns\n");

   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALLfs_32, ALLfs_32, ALL5s_32, ALL5s_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, ALLfs_32, ALLfs_32, 0, 0, ALL5s_32, ALL5s_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, ALLfs_32, ALLfs_32, ALLfs_32, ALLfs_32, ALL5s_32, ALL5s_32);

   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALLfs_32, ALLfs_32, ALLas_32, ALLas_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, ALLfs_32, ALLfs_32, 0, 0, ALLas_32, ALLas_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, ALLfs_32, ALLfs_32, ALLfs_32, ALLfs_32, ALLas_32, ALLas_32);

   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALLfs_32, ALLfs_32, ALLfs_32, ALLfs_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, ALLfs_32, ALLfs_32, 0, 0, ALLfs_32, ALLfs_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, ALLfs_32, ALLfs_32, ALLfs_32, ALLfs_32, ALLfs_32, ALLfs_32);

   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALLfs_32, ALLfs_32, UP_32, UP_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, ALLfs_32, ALLfs_32, 0, 0, UP_32, UP_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, ALLfs_32, ALLfs_32, ALLfs_32, ALLfs_32, UP_32, UP_32);

   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALLfs_32, ALLfs_32, DOWN_32, DOWN_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, ALLfs_32, ALLfs_32, 0, 0, DOWN_32, DOWN_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, ALLfs_32, ALLfs_32, ALLfs_32, ALLfs_32, DOWN_32, DOWN_32);

   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALLfs_32, ALLfs_32, PI_32, PI_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, ALLfs_32, ALLfs_32, 0, 0, PI_32, PI_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, ALLfs_32, ALLfs_32, ALLfs_32, ALLfs_32, PI_32, PI_32);

   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALLfs_32, ALLfs_32, E_32, E_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, ALLfs_32, ALLfs_32, 0, 0, E_32, E_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, ALLfs_32, ALLfs_32, ALLfs_32, ALLfs_32, E_32, E_32);

   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALLfs_32, ALLfs_32, 0, 0);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, ALLfs_32, ALLfs_32, 0, 0, 0, 0);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, ALLfs_32, ALLfs_32, ALLfs_32, ALLfs_32, 0, 0);

   printf("Combinations of UP_32 and all other patterns\n");

   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, 0, 0, UP_32, UP_32, ALL5s_32, ALL5s_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, UP_32, UP_32, 0, 0, ALL5s_32, ALL5s_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, UP_32, UP_32, UP_32, UP_32, ALL5s_32, ALL5s_32);

   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, 0, 0, UP_32, UP_32, ALLas_32, ALLas_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, UP_32, UP_32, 0, 0, ALLas_32, ALLas_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, UP_32, UP_32, UP_32, UP_32, ALLas_32, ALLas_32);

   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, 0, 0, UP_32, UP_32, ALLfs_32, ALLfs_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, UP_32, UP_32, 0, 0, ALLfs_32, ALLfs_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, UP_32, UP_32, UP_32, UP_32, ALLfs_32, ALLfs_32);

   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, 0, 0, UP_32, UP_32, UP_32, UP_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, UP_32, UP_32, 0, 0, UP_32, UP_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, UP_32, UP_32, UP_32, UP_32, UP_32, UP_32);

   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, 0, 0, UP_32, UP_32, DOWN_32, DOWN_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, UP_32, UP_32, 0, 0, DOWN_32, DOWN_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, UP_32, UP_32, UP_32, UP_32, DOWN_32, DOWN_32);

   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, 0, 0, UP_32, UP_32, PI_32, PI_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, UP_32, UP_32, 0, 0, PI_32, PI_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, UP_32, UP_32, UP_32, UP_32, PI_32, PI_32);

   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, 0, 0, UP_32, UP_32, E_32, E_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, UP_32, UP_32, 0, 0, E_32, E_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, UP_32, UP_32, UP_32, UP_32, E_32, E_32);

   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, 0, 0, UP_32, UP_32, 0, 0);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, UP_32, UP_32, 0, 0, 0, 0);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, UP_32, UP_32, UP_32, UP_32, 0, 0);

   printf("Combinations of DOWN_32 and all other patterns\n");

   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, 0, 0, DOWN_32, DOWN_32, ALL5s_32, ALL5s_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, DOWN_32, DOWN_32, 0, 0, ALL5s_32, ALL5s_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, DOWN_32, DOWN_32, DOWN_32, DOWN_32, ALL5s_32, ALL5s_32);

   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, 0, 0, DOWN_32, DOWN_32, ALLas_32, ALLas_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, DOWN_32, DOWN_32, 0, 0, ALLas_32, ALLas_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, DOWN_32, DOWN_32, DOWN_32, DOWN_32, ALLas_32, ALLas_32);

   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, 0, 0, DOWN_32, DOWN_32, ALLfs_32, ALLfs_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, DOWN_32, DOWN_32, 0, 0, ALLfs_32, ALLfs_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, DOWN_32, DOWN_32, DOWN_32, DOWN_32, ALLfs_32, ALLfs_32);

   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, 0, 0, DOWN_32, DOWN_32, UP_32, UP_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, DOWN_32, DOWN_32, 0, 0, UP_32, UP_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, DOWN_32, DOWN_32, DOWN_32, DOWN_32, UP_32, UP_32);

   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, 0, 0, DOWN_32, DOWN_32, DOWN_32, DOWN_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, DOWN_32, DOWN_32, 0, 0, DOWN_32, DOWN_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, DOWN_32, DOWN_32, DOWN_32, DOWN_32, DOWN_32, DOWN_32);

   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, 0, 0, DOWN_32, DOWN_32, PI_32, PI_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, DOWN_32, DOWN_32, 0, 0, PI_32, PI_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, DOWN_32, DOWN_32, DOWN_32, DOWN_32, PI_32, PI_32);

   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, 0, 0, DOWN_32, DOWN_32, E_32, E_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, DOWN_32, DOWN_32, 0, 0, E_32, E_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, DOWN_32, DOWN_32, DOWN_32, DOWN_32, E_32, E_32);

   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, 0, 0, DOWN_32, DOWN_32, 0, 0);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, DOWN_32, DOWN_32, 0, 0, 0, 0);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, DOWN_32, DOWN_32, DOWN_32, DOWN_32, 0, 0);

   printf("Combinations of PI_32 and all other patterns\n");

   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, 0, 0, PI_32, PI_32, ALL5s_32, ALL5s_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, PI_32, PI_32, 0, 0, ALL5s_32, ALL5s_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, PI_32, PI_32, PI_32, PI_32, ALL5s_32, ALL5s_32);

   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, 0, 0, PI_32, PI_32, ALLas_32, ALLas_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, PI_32, PI_32, 0, 0, ALLas_32, ALLas_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, PI_32, PI_32, PI_32, PI_32, ALLas_32, ALLas_32);

   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, 0, 0, PI_32, PI_32, ALLfs_32, ALLfs_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, PI_32, PI_32, 0, 0, ALLfs_32, ALLfs_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, PI_32, PI_32, PI_32, PI_32, ALLfs_32, ALLfs_32);

   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, 0, 0, PI_32, PI_32, UP_32, UP_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, PI_32, PI_32, 0, 0, UP_32, UP_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, PI_32, PI_32, PI_32, PI_32, UP_32, UP_32);

   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, 0, 0, PI_32, PI_32, DOWN_32, DOWN_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, PI_32, PI_32, 0, 0, DOWN_32, DOWN_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, PI_32, PI_32, PI_32, PI_32, DOWN_32, DOWN_32);

   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, 0, 0, PI_32, PI_32, PI_32, PI_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, PI_32, PI_32, 0, 0, PI_32, PI_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, PI_32, PI_32, PI_32, PI_32, PI_32, PI_32);

   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, 0, 0, PI_32, PI_32, E_32, E_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, PI_32, PI_32, 0, 0, E_32, E_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, PI_32, PI_32, PI_32, PI_32, E_32, E_32);

   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, 0, 0, PI_32, PI_32, 0, 0);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, PI_32, PI_32, 0, 0, 0, 0);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, PI_32, PI_32, PI_32, PI_32, 0, 0);

   printf("Combinations of E_32 and all other patterns\n");

   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, 0, 0, E_32, E_32, ALL5s_32, ALL5s_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, E_32, E_32, 0, 0, ALL5s_32, ALL5s_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, E_32, E_32, E_32, E_32, ALL5s_32, ALL5s_32);

   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, 0, 0, E_32, E_32, ALLas_32, ALLas_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, E_32, E_32, 0, 0, ALLas_32, ALLas_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, E_32, E_32, E_32, E_32, ALLas_32, ALLas_32);

   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, 0, 0, E_32, E_32, ALLfs_32, ALLfs_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, E_32, E_32, 0, 0, ALLfs_32, ALLfs_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, E_32, E_32, E_32, E_32, ALLfs_32, ALLfs_32);

   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, 0, 0, E_32, E_32, UP_32, UP_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, E_32, E_32, 0, 0, UP_32, UP_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, E_32, E_32, E_32, E_32, UP_32, UP_32);

   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, 0, 0, E_32, E_32, DOWN_32, DOWN_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, E_32, E_32, 0, 0, DOWN_32, DOWN_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, E_32, E_32, E_32, E_32, DOWN_32, DOWN_32);

   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, 0, 0, E_32, E_32, PI_32, PI_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, E_32, E_32, 0, 0, PI_32, PI_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, E_32, E_32, E_32, E_32, PI_32, PI_32);

   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, 0, 0, E_32, E_32, E_32, E_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, E_32, E_32, 0, 0, E_32, E_32);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, E_32, E_32, E_32, E_32, E_32, E_32);

   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, 0, 0, E_32, E_32, 0, 0);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, E_32, E_32, 0, 0, 0, 0);
   ATOMIC_TEST_CASP("caspal w8, w9, w10, w11, [x5]", w, memp, E_32, E_32, E_32, E_32, 0, 0);

   printf("CASPL <Ws>, <W(s+1)>, <Wt>, <W(t+1)>, [<Xn|SP>{,#0}]\n\n");

   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, 0, 0, 0, 0, 0, 0);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, 0, 0, 0, 0, 1, 1);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, 0, 0, 1, 1, 0, 0);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, 0, 0, 1, 1, 1, 1);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, 1, 1, 0, 0, 0, 0);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, 1, 1, 0, 0, 1, 1);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, 1, 1, 1, 1, 0, 0);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, 1, 1, 1, 1, 1, 1);

   printf("Combinations of ALL5s_32 and all other patterns\n");

   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALL5s_32, ALL5s_32, ALL5s_32, ALL5s_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, ALL5s_32, ALL5s_32, 0, 0, ALL5s_32, ALL5s_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, ALL5s_32, ALL5s_32, ALL5s_32, ALL5s_32, ALL5s_32, ALL5s_32);

   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALL5s_32, ALL5s_32, ALLas_32, ALLas_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, ALL5s_32, ALL5s_32, 0, 0, ALLas_32, ALLas_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, ALL5s_32, ALL5s_32, ALL5s_32, ALL5s_32, ALLas_32, ALLas_32);

   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALL5s_32, ALL5s_32, ALLfs_32, ALLfs_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, ALL5s_32, ALL5s_32, 0, 0, ALLfs_32, ALLfs_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, ALL5s_32, ALL5s_32, ALL5s_32, ALL5s_32, ALLfs_32, ALLfs_32);

   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALL5s_32, ALL5s_32, UP_32, UP_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, ALL5s_32, ALL5s_32, 0, 0, UP_32, UP_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, ALL5s_32, ALL5s_32, ALL5s_32, ALL5s_32, UP_32, UP_32);

   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALL5s_32, ALL5s_32, DOWN_32, DOWN_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, ALL5s_32, ALL5s_32, 0, 0, DOWN_32, DOWN_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, ALL5s_32, ALL5s_32, ALL5s_32, ALL5s_32, DOWN_32, DOWN_32);

   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALL5s_32, ALL5s_32, PI_32, PI_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, ALL5s_32, ALL5s_32, 0, 0, PI_32, PI_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, ALL5s_32, ALL5s_32, ALL5s_32, ALL5s_32, PI_32, PI_32);

   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALL5s_32, ALL5s_32, E_32, E_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, ALL5s_32, ALL5s_32, 0, 0, E_32, E_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, ALL5s_32, ALL5s_32, ALL5s_32, ALL5s_32, E_32, E_32);

   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALL5s_32, ALL5s_32, 0, 0);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, ALL5s_32, ALL5s_32, 0, 0, 0, 0);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, ALL5s_32, ALL5s_32, ALL5s_32, ALL5s_32, 0, 0);

   printf("Combinations of ALLas_32 and all other patterns\n");

   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALLas_32, ALLas_32, ALL5s_32, ALL5s_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, ALLas_32, ALLas_32, 0, 0, ALL5s_32, ALL5s_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, ALLas_32, ALLas_32, ALLas_32, ALLas_32, ALL5s_32, ALL5s_32);

   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALLas_32, ALLas_32, ALLas_32, ALLas_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, ALLas_32, ALLas_32, 0, 0, ALLas_32, ALLas_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, ALLas_32, ALLas_32, ALLas_32, ALLas_32, ALLas_32, ALLas_32);

   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALLas_32, ALLas_32, ALLfs_32, ALLfs_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, ALLas_32, ALLas_32, 0, 0, ALLfs_32, ALLfs_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, ALLas_32, ALLas_32, ALLas_32, ALLas_32, ALLfs_32, ALLfs_32);

   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALLas_32, ALLas_32, UP_32, UP_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, ALLas_32, ALLas_32, 0, 0, UP_32, UP_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, ALLas_32, ALLas_32, ALLas_32, ALLas_32, UP_32, UP_32);

   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALLas_32, ALLas_32, DOWN_32, DOWN_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, ALLas_32, ALLas_32, 0, 0, DOWN_32, DOWN_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, ALLas_32, ALLas_32, ALLas_32, ALLas_32, DOWN_32, DOWN_32);

   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALLas_32, ALLas_32, PI_32, PI_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, ALLas_32, ALLas_32, 0, 0, PI_32, PI_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, ALLas_32, ALLas_32, ALLas_32, ALLas_32, PI_32, PI_32);

   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALLas_32, ALLas_32, E_32, E_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, ALLas_32, ALLas_32, 0, 0, E_32, E_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, ALLas_32, ALLas_32, ALLas_32, ALLas_32, E_32, E_32);

   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALLas_32, ALLas_32, 0, 0);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, ALLas_32, ALLas_32, 0, 0, 0, 0);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, ALLas_32, ALLas_32, ALLas_32, ALLas_32, 0, 0);

   printf("Combinations of ALLfs_32 and all other patterns\n");

   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALLfs_32, ALLfs_32, ALL5s_32, ALL5s_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, ALLfs_32, ALLfs_32, 0, 0, ALL5s_32, ALL5s_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, ALLfs_32, ALLfs_32, ALLfs_32, ALLfs_32, ALL5s_32, ALL5s_32);

   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALLfs_32, ALLfs_32, ALLas_32, ALLas_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, ALLfs_32, ALLfs_32, 0, 0, ALLas_32, ALLas_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, ALLfs_32, ALLfs_32, ALLfs_32, ALLfs_32, ALLas_32, ALLas_32);

   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALLfs_32, ALLfs_32, ALLfs_32, ALLfs_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, ALLfs_32, ALLfs_32, 0, 0, ALLfs_32, ALLfs_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, ALLfs_32, ALLfs_32, ALLfs_32, ALLfs_32, ALLfs_32, ALLfs_32);

   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALLfs_32, ALLfs_32, UP_32, UP_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, ALLfs_32, ALLfs_32, 0, 0, UP_32, UP_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, ALLfs_32, ALLfs_32, ALLfs_32, ALLfs_32, UP_32, UP_32);

   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALLfs_32, ALLfs_32, DOWN_32, DOWN_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, ALLfs_32, ALLfs_32, 0, 0, DOWN_32, DOWN_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, ALLfs_32, ALLfs_32, ALLfs_32, ALLfs_32, DOWN_32, DOWN_32);

   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALLfs_32, ALLfs_32, PI_32, PI_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, ALLfs_32, ALLfs_32, 0, 0, PI_32, PI_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, ALLfs_32, ALLfs_32, ALLfs_32, ALLfs_32, PI_32, PI_32);

   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALLfs_32, ALLfs_32, E_32, E_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, ALLfs_32, ALLfs_32, 0, 0, E_32, E_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, ALLfs_32, ALLfs_32, ALLfs_32, ALLfs_32, E_32, E_32);

   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, 0, 0, ALLfs_32, ALLfs_32, 0, 0);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, ALLfs_32, ALLfs_32, 0, 0, 0, 0);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, ALLfs_32, ALLfs_32, ALLfs_32, ALLfs_32, 0, 0);

   printf("Combinations of UP_32 and all other patterns\n");

   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, 0, 0, UP_32, UP_32, ALL5s_32, ALL5s_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, UP_32, UP_32, 0, 0, ALL5s_32, ALL5s_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, UP_32, UP_32, UP_32, UP_32, ALL5s_32, ALL5s_32);

   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, 0, 0, UP_32, UP_32, ALLas_32, ALLas_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, UP_32, UP_32, 0, 0, ALLas_32, ALLas_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, UP_32, UP_32, UP_32, UP_32, ALLas_32, ALLas_32);

   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, 0, 0, UP_32, UP_32, ALLfs_32, ALLfs_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, UP_32, UP_32, 0, 0, ALLfs_32, ALLfs_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, UP_32, UP_32, UP_32, UP_32, ALLfs_32, ALLfs_32);

   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, 0, 0, UP_32, UP_32, UP_32, UP_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, UP_32, UP_32, 0, 0, UP_32, UP_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, UP_32, UP_32, UP_32, UP_32, UP_32, UP_32);

   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, 0, 0, UP_32, UP_32, DOWN_32, DOWN_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, UP_32, UP_32, 0, 0, DOWN_32, DOWN_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, UP_32, UP_32, UP_32, UP_32, DOWN_32, DOWN_32);

   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, 0, 0, UP_32, UP_32, PI_32, PI_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, UP_32, UP_32, 0, 0, PI_32, PI_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, UP_32, UP_32, UP_32, UP_32, PI_32, PI_32);

   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, 0, 0, UP_32, UP_32, E_32, E_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, UP_32, UP_32, 0, 0, E_32, E_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, UP_32, UP_32, UP_32, UP_32, E_32, E_32);

   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, 0, 0, UP_32, UP_32, 0, 0);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, UP_32, UP_32, 0, 0, 0, 0);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, UP_32, UP_32, UP_32, UP_32, 0, 0);

   printf("Combinations of DOWN_32 and all other patterns\n");

   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, 0, 0, DOWN_32, DOWN_32, ALL5s_32, ALL5s_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, DOWN_32, DOWN_32, 0, 0, ALL5s_32, ALL5s_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, DOWN_32, DOWN_32, DOWN_32, DOWN_32, ALL5s_32, ALL5s_32);

   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, 0, 0, DOWN_32, DOWN_32, ALLas_32, ALLas_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, DOWN_32, DOWN_32, 0, 0, ALLas_32, ALLas_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, DOWN_32, DOWN_32, DOWN_32, DOWN_32, ALLas_32, ALLas_32);

   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, 0, 0, DOWN_32, DOWN_32, ALLfs_32, ALLfs_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, DOWN_32, DOWN_32, 0, 0, ALLfs_32, ALLfs_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, DOWN_32, DOWN_32, DOWN_32, DOWN_32, ALLfs_32, ALLfs_32);

   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, 0, 0, DOWN_32, DOWN_32, UP_32, UP_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, DOWN_32, DOWN_32, 0, 0, UP_32, UP_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, DOWN_32, DOWN_32, DOWN_32, DOWN_32, UP_32, UP_32);

   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, 0, 0, DOWN_32, DOWN_32, DOWN_32, DOWN_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, DOWN_32, DOWN_32, 0, 0, DOWN_32, DOWN_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, DOWN_32, DOWN_32, DOWN_32, DOWN_32, DOWN_32, DOWN_32);

   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, 0, 0, DOWN_32, DOWN_32, PI_32, PI_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, DOWN_32, DOWN_32, 0, 0, PI_32, PI_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, DOWN_32, DOWN_32, DOWN_32, DOWN_32, PI_32, PI_32);

   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, 0, 0, DOWN_32, DOWN_32, E_32, E_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, DOWN_32, DOWN_32, 0, 0, E_32, E_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, DOWN_32, DOWN_32, DOWN_32, DOWN_32, E_32, E_32);

   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, 0, 0, DOWN_32, DOWN_32, 0, 0);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, DOWN_32, DOWN_32, 0, 0, 0, 0);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, DOWN_32, DOWN_32, DOWN_32, DOWN_32, 0, 0);

   printf("Combinations of PI_32 and all other patterns\n");

   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, 0, 0, PI_32, PI_32, ALL5s_32, ALL5s_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, PI_32, PI_32, 0, 0, ALL5s_32, ALL5s_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, PI_32, PI_32, PI_32, PI_32, ALL5s_32, ALL5s_32);

   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, 0, 0, PI_32, PI_32, ALLas_32, ALLas_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, PI_32, PI_32, 0, 0, ALLas_32, ALLas_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, PI_32, PI_32, PI_32, PI_32, ALLas_32, ALLas_32);

   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, 0, 0, PI_32, PI_32, ALLfs_32, ALLfs_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, PI_32, PI_32, 0, 0, ALLfs_32, ALLfs_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, PI_32, PI_32, PI_32, PI_32, ALLfs_32, ALLfs_32);

   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, 0, 0, PI_32, PI_32, UP_32, UP_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, PI_32, PI_32, 0, 0, UP_32, UP_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, PI_32, PI_32, PI_32, PI_32, UP_32, UP_32);

   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, 0, 0, PI_32, PI_32, DOWN_32, DOWN_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, PI_32, PI_32, 0, 0, DOWN_32, DOWN_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, PI_32, PI_32, PI_32, PI_32, DOWN_32, DOWN_32);

   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, 0, 0, PI_32, PI_32, PI_32, PI_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, PI_32, PI_32, 0, 0, PI_32, PI_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, PI_32, PI_32, PI_32, PI_32, PI_32, PI_32);

   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, 0, 0, PI_32, PI_32, E_32, E_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, PI_32, PI_32, 0, 0, E_32, E_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, PI_32, PI_32, PI_32, PI_32, E_32, E_32);

   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, 0, 0, PI_32, PI_32, 0, 0);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, PI_32, PI_32, 0, 0, 0, 0);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, PI_32, PI_32, PI_32, PI_32, 0, 0);

   printf("Combinations of E_32 and all other patterns\n");

   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, 0, 0, E_32, E_32, ALL5s_32, ALL5s_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, E_32, E_32, 0, 0, ALL5s_32, ALL5s_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, E_32, E_32, E_32, E_32, ALL5s_32, ALL5s_32);

   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, 0, 0, E_32, E_32, ALLas_32, ALLas_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, E_32, E_32, 0, 0, ALLas_32, ALLas_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, E_32, E_32, E_32, E_32, ALLas_32, ALLas_32);

   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, 0, 0, E_32, E_32, ALLfs_32, ALLfs_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, E_32, E_32, 0, 0, ALLfs_32, ALLfs_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, E_32, E_32, E_32, E_32, ALLfs_32, ALLfs_32);

   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, 0, 0, E_32, E_32, UP_32, UP_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, E_32, E_32, 0, 0, UP_32, UP_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, E_32, E_32, E_32, E_32, UP_32, UP_32);

   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, 0, 0, E_32, E_32, DOWN_32, DOWN_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, E_32, E_32, 0, 0, DOWN_32, DOWN_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, E_32, E_32, E_32, E_32, DOWN_32, DOWN_32);

   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, 0, 0, E_32, E_32, PI_32, PI_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, E_32, E_32, 0, 0, PI_32, PI_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, E_32, E_32, E_32, E_32, PI_32, PI_32);

   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, 0, 0, E_32, E_32, E_32, E_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, E_32, E_32, 0, 0, E_32, E_32);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, E_32, E_32, E_32, E_32, E_32, E_32);

   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, 0, 0, E_32, E_32, 0, 0);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, E_32, E_32, 0, 0, 0, 0);
   ATOMIC_TEST_CASP("caspl w8, w9, w10, w11, [x5]", w, memp, E_32, E_32, E_32, E_32, 0, 0);

   printf("CASP <Xs>, <X(s+1)>, <Xt>, <X(t+1)>, [<Xn|SP>{,#0}]\n\n");

   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, 0, 0, 0, 0, 0, 0);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, 0, 0, 0, 0, 1, 1);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, 0, 0, 1, 1, 0, 0);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, 0, 0, 1, 1, 1, 1);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, 1, 1, 0, 0, 0, 0);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, 1, 1, 0, 0, 1, 1);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, 1, 1, 1, 1, 0, 0);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, 1, 1, 1, 1, 1, 1);

   printf("Combinations of ALL5s_64 and all other patterns\n");

   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALL5s_64, ALL5s_64, ALL5s_64, ALL5s_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, ALL5s_64, ALL5s_64, 0, 0, ALL5s_64, ALL5s_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, ALL5s_64, ALL5s_64, ALL5s_64, ALL5s_64, ALL5s_64, ALL5s_64);

   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALL5s_64, ALL5s_64, ALLas_64, ALLas_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, ALL5s_64, ALL5s_64, 0, 0, ALLas_64, ALLas_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, ALL5s_64, ALL5s_64, ALL5s_64, ALL5s_64, ALLas_64, ALLas_64);

   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALL5s_64, ALL5s_64, ALLfs_64, ALLfs_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, ALL5s_64, ALL5s_64, 0, 0, ALLfs_64, ALLfs_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, ALL5s_64, ALL5s_64, ALL5s_64, ALL5s_64, ALLfs_64, ALLfs_64);

   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALL5s_64, ALL5s_64, UP_64, UP_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, ALL5s_64, ALL5s_64, 0, 0, UP_64, UP_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, ALL5s_64, ALL5s_64, ALL5s_64, ALL5s_64, UP_64, UP_64);

   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALL5s_64, ALL5s_64, DOWN_64, DOWN_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, ALL5s_64, ALL5s_64, 0, 0, DOWN_64, DOWN_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, ALL5s_64, ALL5s_64, ALL5s_64, ALL5s_64, DOWN_64, DOWN_64);

   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALL5s_64, ALL5s_64, PI_64, PI_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, ALL5s_64, ALL5s_64, 0, 0, PI_64, PI_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, ALL5s_64, ALL5s_64, ALL5s_64, ALL5s_64, PI_64, PI_64);

   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALL5s_64, ALL5s_64, E_64, E_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, ALL5s_64, ALL5s_64, 0, 0, E_64, E_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, ALL5s_64, ALL5s_64, ALL5s_64, ALL5s_64, E_64, E_64);

   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALL5s_64, ALL5s_64, 0, 0);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, ALL5s_64, ALL5s_64, 0, 0, 0, 0);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, ALL5s_64, ALL5s_64, ALL5s_64, ALL5s_64, 0, 0);

   printf("Combinations of ALLas_64 and all other patterns\n");

   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALLas_64, ALLas_64, ALL5s_64, ALL5s_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, ALLas_64, ALLas_64, 0, 0, ALL5s_64, ALL5s_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, ALLas_64, ALLas_64, ALLas_64, ALLas_64, ALL5s_64, ALL5s_64);

   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALLas_64, ALLas_64, ALLas_64, ALLas_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, ALLas_64, ALLas_64, 0, 0, ALLas_64, ALLas_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, ALLas_64, ALLas_64, ALLas_64, ALLas_64, ALLas_64, ALLas_64);

   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALLas_64, ALLas_64, ALLfs_64, ALLfs_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, ALLas_64, ALLas_64, 0, 0, ALLfs_64, ALLfs_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, ALLas_64, ALLas_64, ALLas_64, ALLas_64, ALLfs_64, ALLfs_64);

   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALLas_64, ALLas_64, UP_64, UP_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, ALLas_64, ALLas_64, 0, 0, UP_64, UP_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, ALLas_64, ALLas_64, ALLas_64, ALLas_64, UP_64, UP_64);

   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALLas_64, ALLas_64, DOWN_64, DOWN_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, ALLas_64, ALLas_64, 0, 0, DOWN_64, DOWN_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, ALLas_64, ALLas_64, ALLas_64, ALLas_64, DOWN_64, DOWN_64);

   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALLas_64, ALLas_64, PI_64, PI_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, ALLas_64, ALLas_64, 0, 0, PI_64, PI_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, ALLas_64, ALLas_64, ALLas_64, ALLas_64, PI_64, PI_64);

   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALLas_64, ALLas_64, E_64, E_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, ALLas_64, ALLas_64, 0, 0, E_64, E_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, ALLas_64, ALLas_64, ALLas_64, ALLas_64, E_64, E_64);

   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALLas_64, ALLas_64, 0, 0);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, ALLas_64, ALLas_64, 0, 0, 0, 0);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, ALLas_64, ALLas_64, ALLas_64, ALLas_64, 0, 0);

   printf("Combinations of ALLfs_64 and all other patterns\n");

   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALLfs_64, ALLfs_64, ALL5s_64, ALL5s_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, ALLfs_64, ALLfs_64, 0, 0, ALL5s_64, ALL5s_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, ALLfs_64, ALLfs_64, ALLfs_64, ALLfs_64, ALL5s_64, ALL5s_64);

   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALLfs_64, ALLfs_64, ALLas_64, ALLas_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, ALLfs_64, ALLfs_64, 0, 0, ALLas_64, ALLas_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, ALLfs_64, ALLfs_64, ALLfs_64, ALLfs_64, ALLas_64, ALLas_64);

   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALLfs_64, ALLfs_64, ALLfs_64, ALLfs_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, ALLfs_64, ALLfs_64, 0, 0, ALLfs_64, ALLfs_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, ALLfs_64, ALLfs_64, ALLfs_64, ALLfs_64, ALLfs_64, ALLfs_64);

   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALLfs_64, ALLfs_64, UP_64, UP_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, ALLfs_64, ALLfs_64, 0, 0, UP_64, UP_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, ALLfs_64, ALLfs_64, ALLfs_64, ALLfs_64, UP_64, UP_64);

   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALLfs_64, ALLfs_64, DOWN_64, DOWN_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, ALLfs_64, ALLfs_64, 0, 0, DOWN_64, DOWN_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, ALLfs_64, ALLfs_64, ALLfs_64, ALLfs_64, DOWN_64, DOWN_64);

   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALLfs_64, ALLfs_64, PI_64, PI_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, ALLfs_64, ALLfs_64, 0, 0, PI_64, PI_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, ALLfs_64, ALLfs_64, ALLfs_64, ALLfs_64, PI_64, PI_64);

   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALLfs_64, ALLfs_64, E_64, E_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, ALLfs_64, ALLfs_64, 0, 0, E_64, E_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, ALLfs_64, ALLfs_64, ALLfs_64, ALLfs_64, E_64, E_64);

   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALLfs_64, ALLfs_64, 0, 0);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, ALLfs_64, ALLfs_64, 0, 0, 0, 0);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, ALLfs_64, ALLfs_64, ALLfs_64, ALLfs_64, 0, 0);

   printf("Combinations of UP_64 and all other patterns\n");

   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, 0, 0, UP_64, UP_64, ALL5s_64, ALL5s_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, UP_64, UP_64, 0, 0, ALL5s_64, ALL5s_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, UP_64, UP_64, UP_64, UP_64, ALL5s_64, ALL5s_64);

   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, 0, 0, UP_64, UP_64, ALLas_64, ALLas_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, UP_64, UP_64, 0, 0, ALLas_64, ALLas_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, UP_64, UP_64, UP_64, UP_64, ALLas_64, ALLas_64);

   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, 0, 0, UP_64, UP_64, ALLfs_64, ALLfs_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, UP_64, UP_64, 0, 0, ALLfs_64, ALLfs_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, UP_64, UP_64, UP_64, UP_64, ALLfs_64, ALLfs_64);

   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, 0, 0, UP_64, UP_64, UP_64, UP_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, UP_64, UP_64, 0, 0, UP_64, UP_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, UP_64, UP_64, UP_64, UP_64, UP_64, UP_64);

   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, 0, 0, UP_64, UP_64, DOWN_64, DOWN_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, UP_64, UP_64, 0, 0, DOWN_64, DOWN_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, UP_64, UP_64, UP_64, UP_64, DOWN_64, DOWN_64);

   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, 0, 0, UP_64, UP_64, PI_64, PI_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, UP_64, UP_64, 0, 0, PI_64, PI_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, UP_64, UP_64, UP_64, UP_64, PI_64, PI_64);

   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, 0, 0, UP_64, UP_64, E_64, E_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, UP_64, UP_64, 0, 0, E_64, E_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, UP_64, UP_64, UP_64, UP_64, E_64, E_64);

   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, 0, 0, UP_64, UP_64, 0, 0);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, UP_64, UP_64, 0, 0, 0, 0);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, UP_64, UP_64, UP_64, UP_64, 0, 0);

   printf("Combinations of DOWN_64 and all other patterns\n");

   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, 0, 0, DOWN_64, DOWN_64, ALL5s_64, ALL5s_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, DOWN_64, DOWN_64, 0, 0, ALL5s_64, ALL5s_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, DOWN_64, DOWN_64, DOWN_64, DOWN_64, ALL5s_64, ALL5s_64);

   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, 0, 0, DOWN_64, DOWN_64, ALLas_64, ALLas_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, DOWN_64, DOWN_64, 0, 0, ALLas_64, ALLas_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, DOWN_64, DOWN_64, DOWN_64, DOWN_64, ALLas_64, ALLas_64);

   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, 0, 0, DOWN_64, DOWN_64, ALLfs_64, ALLfs_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, DOWN_64, DOWN_64, 0, 0, ALLfs_64, ALLfs_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, DOWN_64, DOWN_64, DOWN_64, DOWN_64, ALLfs_64, ALLfs_64);

   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, 0, 0, DOWN_64, DOWN_64, UP_64, UP_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, DOWN_64, DOWN_64, 0, 0, UP_64, UP_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, DOWN_64, DOWN_64, DOWN_64, DOWN_64, UP_64, UP_64);

   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, 0, 0, DOWN_64, DOWN_64, DOWN_64, DOWN_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, DOWN_64, DOWN_64, 0, 0, DOWN_64, DOWN_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, DOWN_64, DOWN_64, DOWN_64, DOWN_64, DOWN_64, DOWN_64);

   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, 0, 0, DOWN_64, DOWN_64, PI_64, PI_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, DOWN_64, DOWN_64, 0, 0, PI_64, PI_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, DOWN_64, DOWN_64, DOWN_64, DOWN_64, PI_64, PI_64);

   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, 0, 0, DOWN_64, DOWN_64, E_64, E_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, DOWN_64, DOWN_64, 0, 0, E_64, E_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, DOWN_64, DOWN_64, DOWN_64, DOWN_64, E_64, E_64);

   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, 0, 0, DOWN_64, DOWN_64, 0, 0);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, DOWN_64, DOWN_64, 0, 0, 0, 0);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, DOWN_64, DOWN_64, DOWN_64, DOWN_64, 0, 0);

   printf("Combinations of PI_64 and all other patterns\n");

   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, 0, 0, PI_64, PI_64, ALL5s_64, ALL5s_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, PI_64, PI_64, 0, 0, ALL5s_64, ALL5s_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, PI_64, PI_64, PI_64, PI_64, ALL5s_64, ALL5s_64);

   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, 0, 0, PI_64, PI_64, ALLas_64, ALLas_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, PI_64, PI_64, 0, 0, ALLas_64, ALLas_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, PI_64, PI_64, PI_64, PI_64, ALLas_64, ALLas_64);

   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, 0, 0, PI_64, PI_64, ALLfs_64, ALLfs_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, PI_64, PI_64, 0, 0, ALLfs_64, ALLfs_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, PI_64, PI_64, PI_64, PI_64, ALLfs_64, ALLfs_64);

   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, 0, 0, PI_64, PI_64, UP_64, UP_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, PI_64, PI_64, 0, 0, UP_64, UP_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, PI_64, PI_64, PI_64, PI_64, UP_64, UP_64);

   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, 0, 0, PI_64, PI_64, DOWN_64, DOWN_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, PI_64, PI_64, 0, 0, DOWN_64, DOWN_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, PI_64, PI_64, PI_64, PI_64, DOWN_64, DOWN_64);

   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, 0, 0, PI_64, PI_64, PI_64, PI_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, PI_64, PI_64, 0, 0, PI_64, PI_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, PI_64, PI_64, PI_64, PI_64, PI_64, PI_64);

   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, 0, 0, PI_64, PI_64, E_64, E_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, PI_64, PI_64, 0, 0, E_64, E_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, PI_64, PI_64, PI_64, PI_64, E_64, E_64);

   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, 0, 0, PI_64, PI_64, 0, 0);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, PI_64, PI_64, 0, 0, 0, 0);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, PI_64, PI_64, PI_64, PI_64, 0, 0);

   printf("Combinations of E_64 and all other patterns\n");

   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, 0, 0, E_64, E_64, ALL5s_64, ALL5s_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, E_64, E_64, 0, 0, ALL5s_64, ALL5s_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, E_64, E_64, E_64, E_64, ALL5s_64, ALL5s_64);

   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, 0, 0, E_64, E_64, ALLas_64, ALLas_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, E_64, E_64, 0, 0, ALLas_64, ALLas_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, E_64, E_64, E_64, E_64, ALLas_64, ALLas_64);

   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, 0, 0, E_64, E_64, ALLfs_64, ALLfs_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, E_64, E_64, 0, 0, ALLfs_64, ALLfs_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, E_64, E_64, E_64, E_64, ALLfs_64, ALLfs_64);

   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, 0, 0, E_64, E_64, UP_64, UP_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, E_64, E_64, 0, 0, UP_64, UP_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, E_64, E_64, E_64, E_64, UP_64, UP_64);

   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, 0, 0, E_64, E_64, DOWN_64, DOWN_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, E_64, E_64, 0, 0, DOWN_64, DOWN_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, E_64, E_64, E_64, E_64, DOWN_64, DOWN_64);

   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, 0, 0, E_64, E_64, PI_64, PI_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, E_64, E_64, 0, 0, PI_64, PI_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, E_64, E_64, E_64, E_64, PI_64, PI_64);

   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, 0, 0, E_64, E_64, E_64, E_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, E_64, E_64, 0, 0, E_64, E_64);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, E_64, E_64, E_64, E_64, E_64, E_64);

   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, 0, 0, E_64, E_64, 0, 0);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, E_64, E_64, 0, 0, 0, 0);
   ATOMIC_TEST_CASP("casp x8, x9, x10, x11, [x5]", x, memp, E_64, E_64, E_64, E_64, 0, 0);

   printf("CASPA <Xs>, <X(s+1)>, <Xt>, <X(t+1)>, [<Xn|SP>{,#0}]\n\n");

   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, 0, 0, 0, 0, 0, 0);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, 0, 0, 0, 0, 1, 1);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, 0, 0, 1, 1, 0, 0);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, 0, 0, 1, 1, 1, 1);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, 1, 1, 0, 0, 0, 0);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, 1, 1, 0, 0, 1, 1);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, 1, 1, 1, 1, 0, 0);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, 1, 1, 1, 1, 1, 1);

   printf("Combinations of ALL5s_64 and all other patterns\n");

   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALL5s_64, ALL5s_64, ALL5s_64, ALL5s_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, ALL5s_64, ALL5s_64, 0, 0, ALL5s_64, ALL5s_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, ALL5s_64, ALL5s_64, ALL5s_64, ALL5s_64, ALL5s_64, ALL5s_64);

   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALL5s_64, ALL5s_64, ALLas_64, ALLas_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, ALL5s_64, ALL5s_64, 0, 0, ALLas_64, ALLas_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, ALL5s_64, ALL5s_64, ALL5s_64, ALL5s_64, ALLas_64, ALLas_64);

   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALL5s_64, ALL5s_64, ALLfs_64, ALLfs_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, ALL5s_64, ALL5s_64, 0, 0, ALLfs_64, ALLfs_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, ALL5s_64, ALL5s_64, ALL5s_64, ALL5s_64, ALLfs_64, ALLfs_64);

   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALL5s_64, ALL5s_64, UP_64, UP_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, ALL5s_64, ALL5s_64, 0, 0, UP_64, UP_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, ALL5s_64, ALL5s_64, ALL5s_64, ALL5s_64, UP_64, UP_64);

   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALL5s_64, ALL5s_64, DOWN_64, DOWN_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, ALL5s_64, ALL5s_64, 0, 0, DOWN_64, DOWN_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, ALL5s_64, ALL5s_64, ALL5s_64, ALL5s_64, DOWN_64, DOWN_64);

   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALL5s_64, ALL5s_64, PI_64, PI_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, ALL5s_64, ALL5s_64, 0, 0, PI_64, PI_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, ALL5s_64, ALL5s_64, ALL5s_64, ALL5s_64, PI_64, PI_64);

   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALL5s_64, ALL5s_64, E_64, E_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, ALL5s_64, ALL5s_64, 0, 0, E_64, E_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, ALL5s_64, ALL5s_64, ALL5s_64, ALL5s_64, E_64, E_64);

   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALL5s_64, ALL5s_64, 0, 0);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, ALL5s_64, ALL5s_64, 0, 0, 0, 0);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, ALL5s_64, ALL5s_64, ALL5s_64, ALL5s_64, 0, 0);

   printf("Combinations of ALLas_64 and all other patterns\n");

   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALLas_64, ALLas_64, ALL5s_64, ALL5s_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, ALLas_64, ALLas_64, 0, 0, ALL5s_64, ALL5s_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, ALLas_64, ALLas_64, ALLas_64, ALLas_64, ALL5s_64, ALL5s_64);

   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALLas_64, ALLas_64, ALLas_64, ALLas_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, ALLas_64, ALLas_64, 0, 0, ALLas_64, ALLas_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, ALLas_64, ALLas_64, ALLas_64, ALLas_64, ALLas_64, ALLas_64);

   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALLas_64, ALLas_64, ALLfs_64, ALLfs_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, ALLas_64, ALLas_64, 0, 0, ALLfs_64, ALLfs_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, ALLas_64, ALLas_64, ALLas_64, ALLas_64, ALLfs_64, ALLfs_64);

   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALLas_64, ALLas_64, UP_64, UP_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, ALLas_64, ALLas_64, 0, 0, UP_64, UP_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, ALLas_64, ALLas_64, ALLas_64, ALLas_64, UP_64, UP_64);

   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALLas_64, ALLas_64, DOWN_64, DOWN_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, ALLas_64, ALLas_64, 0, 0, DOWN_64, DOWN_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, ALLas_64, ALLas_64, ALLas_64, ALLas_64, DOWN_64, DOWN_64);

   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALLas_64, ALLas_64, PI_64, PI_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, ALLas_64, ALLas_64, 0, 0, PI_64, PI_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, ALLas_64, ALLas_64, ALLas_64, ALLas_64, PI_64, PI_64);

   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALLas_64, ALLas_64, E_64, E_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, ALLas_64, ALLas_64, 0, 0, E_64, E_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, ALLas_64, ALLas_64, ALLas_64, ALLas_64, E_64, E_64);

   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALLas_64, ALLas_64, 0, 0);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, ALLas_64, ALLas_64, 0, 0, 0, 0);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, ALLas_64, ALLas_64, ALLas_64, ALLas_64, 0, 0);

   printf("Combinations of ALLfs_64 and all other patterns\n");

   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALLfs_64, ALLfs_64, ALL5s_64, ALL5s_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, ALLfs_64, ALLfs_64, 0, 0, ALL5s_64, ALL5s_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, ALLfs_64, ALLfs_64, ALLfs_64, ALLfs_64, ALL5s_64, ALL5s_64);

   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALLfs_64, ALLfs_64, ALLas_64, ALLas_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, ALLfs_64, ALLfs_64, 0, 0, ALLas_64, ALLas_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, ALLfs_64, ALLfs_64, ALLfs_64, ALLfs_64, ALLas_64, ALLas_64);

   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALLfs_64, ALLfs_64, ALLfs_64, ALLfs_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, ALLfs_64, ALLfs_64, 0, 0, ALLfs_64, ALLfs_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, ALLfs_64, ALLfs_64, ALLfs_64, ALLfs_64, ALLfs_64, ALLfs_64);

   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALLfs_64, ALLfs_64, UP_64, UP_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, ALLfs_64, ALLfs_64, 0, 0, UP_64, UP_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, ALLfs_64, ALLfs_64, ALLfs_64, ALLfs_64, UP_64, UP_64);

   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALLfs_64, ALLfs_64, DOWN_64, DOWN_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, ALLfs_64, ALLfs_64, 0, 0, DOWN_64, DOWN_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, ALLfs_64, ALLfs_64, ALLfs_64, ALLfs_64, DOWN_64, DOWN_64);

   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALLfs_64, ALLfs_64, PI_64, PI_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, ALLfs_64, ALLfs_64, 0, 0, PI_64, PI_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, ALLfs_64, ALLfs_64, ALLfs_64, ALLfs_64, PI_64, PI_64);

   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALLfs_64, ALLfs_64, E_64, E_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, ALLfs_64, ALLfs_64, 0, 0, E_64, E_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, ALLfs_64, ALLfs_64, ALLfs_64, ALLfs_64, E_64, E_64);

   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALLfs_64, ALLfs_64, 0, 0);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, ALLfs_64, ALLfs_64, 0, 0, 0, 0);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, ALLfs_64, ALLfs_64, ALLfs_64, ALLfs_64, 0, 0);

   printf("Combinations of UP_64 and all other patterns\n");

   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, 0, 0, UP_64, UP_64, ALL5s_64, ALL5s_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, UP_64, UP_64, 0, 0, ALL5s_64, ALL5s_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, UP_64, UP_64, UP_64, UP_64, ALL5s_64, ALL5s_64);

   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, 0, 0, UP_64, UP_64, ALLas_64, ALLas_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, UP_64, UP_64, 0, 0, ALLas_64, ALLas_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, UP_64, UP_64, UP_64, UP_64, ALLas_64, ALLas_64);

   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, 0, 0, UP_64, UP_64, ALLfs_64, ALLfs_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, UP_64, UP_64, 0, 0, ALLfs_64, ALLfs_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, UP_64, UP_64, UP_64, UP_64, ALLfs_64, ALLfs_64);

   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, 0, 0, UP_64, UP_64, UP_64, UP_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, UP_64, UP_64, 0, 0, UP_64, UP_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, UP_64, UP_64, UP_64, UP_64, UP_64, UP_64);

   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, 0, 0, UP_64, UP_64, DOWN_64, DOWN_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, UP_64, UP_64, 0, 0, DOWN_64, DOWN_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, UP_64, UP_64, UP_64, UP_64, DOWN_64, DOWN_64);

   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, 0, 0, UP_64, UP_64, PI_64, PI_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, UP_64, UP_64, 0, 0, PI_64, PI_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, UP_64, UP_64, UP_64, UP_64, PI_64, PI_64);

   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, 0, 0, UP_64, UP_64, E_64, E_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, UP_64, UP_64, 0, 0, E_64, E_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, UP_64, UP_64, UP_64, UP_64, E_64, E_64);

   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, 0, 0, UP_64, UP_64, 0, 0);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, UP_64, UP_64, 0, 0, 0, 0);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, UP_64, UP_64, UP_64, UP_64, 0, 0);

   printf("Combinations of DOWN_64 and all other patterns\n");

   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, 0, 0, DOWN_64, DOWN_64, ALL5s_64, ALL5s_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, DOWN_64, DOWN_64, 0, 0, ALL5s_64, ALL5s_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, DOWN_64, DOWN_64, DOWN_64, DOWN_64, ALL5s_64, ALL5s_64);

   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, 0, 0, DOWN_64, DOWN_64, ALLas_64, ALLas_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, DOWN_64, DOWN_64, 0, 0, ALLas_64, ALLas_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, DOWN_64, DOWN_64, DOWN_64, DOWN_64, ALLas_64, ALLas_64);

   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, 0, 0, DOWN_64, DOWN_64, ALLfs_64, ALLfs_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, DOWN_64, DOWN_64, 0, 0, ALLfs_64, ALLfs_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, DOWN_64, DOWN_64, DOWN_64, DOWN_64, ALLfs_64, ALLfs_64);

   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, 0, 0, DOWN_64, DOWN_64, UP_64, UP_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, DOWN_64, DOWN_64, 0, 0, UP_64, UP_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, DOWN_64, DOWN_64, DOWN_64, DOWN_64, UP_64, UP_64);

   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, 0, 0, DOWN_64, DOWN_64, DOWN_64, DOWN_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, DOWN_64, DOWN_64, 0, 0, DOWN_64, DOWN_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, DOWN_64, DOWN_64, DOWN_64, DOWN_64, DOWN_64, DOWN_64);

   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, 0, 0, DOWN_64, DOWN_64, PI_64, PI_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, DOWN_64, DOWN_64, 0, 0, PI_64, PI_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, DOWN_64, DOWN_64, DOWN_64, DOWN_64, PI_64, PI_64);

   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, 0, 0, DOWN_64, DOWN_64, E_64, E_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, DOWN_64, DOWN_64, 0, 0, E_64, E_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, DOWN_64, DOWN_64, DOWN_64, DOWN_64, E_64, E_64);

   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, 0, 0, DOWN_64, DOWN_64, 0, 0);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, DOWN_64, DOWN_64, 0, 0, 0, 0);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, DOWN_64, DOWN_64, DOWN_64, DOWN_64, 0, 0);

   printf("Combinations of PI_64 and all other patterns\n");

   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, 0, 0, PI_64, PI_64, ALL5s_64, ALL5s_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, PI_64, PI_64, 0, 0, ALL5s_64, ALL5s_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, PI_64, PI_64, PI_64, PI_64, ALL5s_64, ALL5s_64);

   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, 0, 0, PI_64, PI_64, ALLas_64, ALLas_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, PI_64, PI_64, 0, 0, ALLas_64, ALLas_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, PI_64, PI_64, PI_64, PI_64, ALLas_64, ALLas_64);

   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, 0, 0, PI_64, PI_64, ALLfs_64, ALLfs_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, PI_64, PI_64, 0, 0, ALLfs_64, ALLfs_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, PI_64, PI_64, PI_64, PI_64, ALLfs_64, ALLfs_64);

   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, 0, 0, PI_64, PI_64, UP_64, UP_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, PI_64, PI_64, 0, 0, UP_64, UP_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, PI_64, PI_64, PI_64, PI_64, UP_64, UP_64);

   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, 0, 0, PI_64, PI_64, DOWN_64, DOWN_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, PI_64, PI_64, 0, 0, DOWN_64, DOWN_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, PI_64, PI_64, PI_64, PI_64, DOWN_64, DOWN_64);

   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, 0, 0, PI_64, PI_64, PI_64, PI_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, PI_64, PI_64, 0, 0, PI_64, PI_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, PI_64, PI_64, PI_64, PI_64, PI_64, PI_64);

   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, 0, 0, PI_64, PI_64, E_64, E_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, PI_64, PI_64, 0, 0, E_64, E_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, PI_64, PI_64, PI_64, PI_64, E_64, E_64);

   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, 0, 0, PI_64, PI_64, 0, 0);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, PI_64, PI_64, 0, 0, 0, 0);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, PI_64, PI_64, PI_64, PI_64, 0, 0);

   printf("Combinations of E_64 and all other patterns\n");

   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, 0, 0, E_64, E_64, ALL5s_64, ALL5s_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, E_64, E_64, 0, 0, ALL5s_64, ALL5s_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, E_64, E_64, E_64, E_64, ALL5s_64, ALL5s_64);

   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, 0, 0, E_64, E_64, ALLas_64, ALLas_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, E_64, E_64, 0, 0, ALLas_64, ALLas_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, E_64, E_64, E_64, E_64, ALLas_64, ALLas_64);

   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, 0, 0, E_64, E_64, ALLfs_64, ALLfs_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, E_64, E_64, 0, 0, ALLfs_64, ALLfs_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, E_64, E_64, E_64, E_64, ALLfs_64, ALLfs_64);

   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, 0, 0, E_64, E_64, UP_64, UP_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, E_64, E_64, 0, 0, UP_64, UP_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, E_64, E_64, E_64, E_64, UP_64, UP_64);

   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, 0, 0, E_64, E_64, DOWN_64, DOWN_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, E_64, E_64, 0, 0, DOWN_64, DOWN_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, E_64, E_64, E_64, E_64, DOWN_64, DOWN_64);

   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, 0, 0, E_64, E_64, PI_64, PI_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, E_64, E_64, 0, 0, PI_64, PI_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, E_64, E_64, E_64, E_64, PI_64, PI_64);

   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, 0, 0, E_64, E_64, E_64, E_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, E_64, E_64, 0, 0, E_64, E_64);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, E_64, E_64, E_64, E_64, E_64, E_64);

   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, 0, 0, E_64, E_64, 0, 0);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, E_64, E_64, 0, 0, 0, 0);
   ATOMIC_TEST_CASP("caspa x8, x9, x10, x11, [x5]", x, memp, E_64, E_64, E_64, E_64, 0, 0);

   printf("CASPAL <Xs>, <X(s+1)>, <Xt>, <X(t+1)>, [<Xn|SP>{,#0}]\n\n");

   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, 0, 0, 0, 0, 0, 0);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, 0, 0, 0, 0, 1, 1);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, 0, 0, 1, 1, 0, 0);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, 0, 0, 1, 1, 1, 1);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, 1, 1, 0, 0, 0, 0);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, 1, 1, 0, 0, 1, 1);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, 1, 1, 1, 1, 0, 0);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, 1, 1, 1, 1, 1, 1);

   printf("Combinations of ALL5s_64 and all other patterns\n");

   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALL5s_64, ALL5s_64, ALL5s_64, ALL5s_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, ALL5s_64, ALL5s_64, 0, 0, ALL5s_64, ALL5s_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, ALL5s_64, ALL5s_64, ALL5s_64, ALL5s_64, ALL5s_64, ALL5s_64);

   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALL5s_64, ALL5s_64, ALLas_64, ALLas_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, ALL5s_64, ALL5s_64, 0, 0, ALLas_64, ALLas_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, ALL5s_64, ALL5s_64, ALL5s_64, ALL5s_64, ALLas_64, ALLas_64);

   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALL5s_64, ALL5s_64, ALLfs_64, ALLfs_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, ALL5s_64, ALL5s_64, 0, 0, ALLfs_64, ALLfs_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, ALL5s_64, ALL5s_64, ALL5s_64, ALL5s_64, ALLfs_64, ALLfs_64);

   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALL5s_64, ALL5s_64, UP_64, UP_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, ALL5s_64, ALL5s_64, 0, 0, UP_64, UP_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, ALL5s_64, ALL5s_64, ALL5s_64, ALL5s_64, UP_64, UP_64);

   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALL5s_64, ALL5s_64, DOWN_64, DOWN_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, ALL5s_64, ALL5s_64, 0, 0, DOWN_64, DOWN_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, ALL5s_64, ALL5s_64, ALL5s_64, ALL5s_64, DOWN_64, DOWN_64);

   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALL5s_64, ALL5s_64, PI_64, PI_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, ALL5s_64, ALL5s_64, 0, 0, PI_64, PI_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, ALL5s_64, ALL5s_64, ALL5s_64, ALL5s_64, PI_64, PI_64);

   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALL5s_64, ALL5s_64, E_64, E_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, ALL5s_64, ALL5s_64, 0, 0, E_64, E_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, ALL5s_64, ALL5s_64, ALL5s_64, ALL5s_64, E_64, E_64);

   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALL5s_64, ALL5s_64, 0, 0);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, ALL5s_64, ALL5s_64, 0, 0, 0, 0);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, ALL5s_64, ALL5s_64, ALL5s_64, ALL5s_64, 0, 0);

   printf("Combinations of ALLas_64 and all other patterns\n");

   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALLas_64, ALLas_64, ALL5s_64, ALL5s_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, ALLas_64, ALLas_64, 0, 0, ALL5s_64, ALL5s_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, ALLas_64, ALLas_64, ALLas_64, ALLas_64, ALL5s_64, ALL5s_64);

   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALLas_64, ALLas_64, ALLas_64, ALLas_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, ALLas_64, ALLas_64, 0, 0, ALLas_64, ALLas_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, ALLas_64, ALLas_64, ALLas_64, ALLas_64, ALLas_64, ALLas_64);

   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALLas_64, ALLas_64, ALLfs_64, ALLfs_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, ALLas_64, ALLas_64, 0, 0, ALLfs_64, ALLfs_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, ALLas_64, ALLas_64, ALLas_64, ALLas_64, ALLfs_64, ALLfs_64);

   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALLas_64, ALLas_64, UP_64, UP_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, ALLas_64, ALLas_64, 0, 0, UP_64, UP_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, ALLas_64, ALLas_64, ALLas_64, ALLas_64, UP_64, UP_64);

   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALLas_64, ALLas_64, DOWN_64, DOWN_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, ALLas_64, ALLas_64, 0, 0, DOWN_64, DOWN_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, ALLas_64, ALLas_64, ALLas_64, ALLas_64, DOWN_64, DOWN_64);

   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALLas_64, ALLas_64, PI_64, PI_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, ALLas_64, ALLas_64, 0, 0, PI_64, PI_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, ALLas_64, ALLas_64, ALLas_64, ALLas_64, PI_64, PI_64);

   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALLas_64, ALLas_64, E_64, E_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, ALLas_64, ALLas_64, 0, 0, E_64, E_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, ALLas_64, ALLas_64, ALLas_64, ALLas_64, E_64, E_64);

   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALLas_64, ALLas_64, 0, 0);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, ALLas_64, ALLas_64, 0, 0, 0, 0);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, ALLas_64, ALLas_64, ALLas_64, ALLas_64, 0, 0);

   printf("Combinations of ALLfs_64 and all other patterns\n");

   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALLfs_64, ALLfs_64, ALL5s_64, ALL5s_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, ALLfs_64, ALLfs_64, 0, 0, ALL5s_64, ALL5s_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, ALLfs_64, ALLfs_64, ALLfs_64, ALLfs_64, ALL5s_64, ALL5s_64);

   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALLfs_64, ALLfs_64, ALLas_64, ALLas_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, ALLfs_64, ALLfs_64, 0, 0, ALLas_64, ALLas_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, ALLfs_64, ALLfs_64, ALLfs_64, ALLfs_64, ALLas_64, ALLas_64);

   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALLfs_64, ALLfs_64, ALLfs_64, ALLfs_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, ALLfs_64, ALLfs_64, 0, 0, ALLfs_64, ALLfs_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, ALLfs_64, ALLfs_64, ALLfs_64, ALLfs_64, ALLfs_64, ALLfs_64);

   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALLfs_64, ALLfs_64, UP_64, UP_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, ALLfs_64, ALLfs_64, 0, 0, UP_64, UP_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, ALLfs_64, ALLfs_64, ALLfs_64, ALLfs_64, UP_64, UP_64);

   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALLfs_64, ALLfs_64, DOWN_64, DOWN_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, ALLfs_64, ALLfs_64, 0, 0, DOWN_64, DOWN_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, ALLfs_64, ALLfs_64, ALLfs_64, ALLfs_64, DOWN_64, DOWN_64);

   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALLfs_64, ALLfs_64, PI_64, PI_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, ALLfs_64, ALLfs_64, 0, 0, PI_64, PI_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, ALLfs_64, ALLfs_64, ALLfs_64, ALLfs_64, PI_64, PI_64);

   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALLfs_64, ALLfs_64, E_64, E_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, ALLfs_64, ALLfs_64, 0, 0, E_64, E_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, ALLfs_64, ALLfs_64, ALLfs_64, ALLfs_64, E_64, E_64);

   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALLfs_64, ALLfs_64, 0, 0);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, ALLfs_64, ALLfs_64, 0, 0, 0, 0);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, ALLfs_64, ALLfs_64, ALLfs_64, ALLfs_64, 0, 0);

   printf("Combinations of UP_64 and all other patterns\n");

   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, 0, 0, UP_64, UP_64, ALL5s_64, ALL5s_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, UP_64, UP_64, 0, 0, ALL5s_64, ALL5s_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, UP_64, UP_64, UP_64, UP_64, ALL5s_64, ALL5s_64);

   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, 0, 0, UP_64, UP_64, ALLas_64, ALLas_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, UP_64, UP_64, 0, 0, ALLas_64, ALLas_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, UP_64, UP_64, UP_64, UP_64, ALLas_64, ALLas_64);

   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, 0, 0, UP_64, UP_64, ALLfs_64, ALLfs_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, UP_64, UP_64, 0, 0, ALLfs_64, ALLfs_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, UP_64, UP_64, UP_64, UP_64, ALLfs_64, ALLfs_64);

   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, 0, 0, UP_64, UP_64, UP_64, UP_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, UP_64, UP_64, 0, 0, UP_64, UP_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, UP_64, UP_64, UP_64, UP_64, UP_64, UP_64);

   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, 0, 0, UP_64, UP_64, DOWN_64, DOWN_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, UP_64, UP_64, 0, 0, DOWN_64, DOWN_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, UP_64, UP_64, UP_64, UP_64, DOWN_64, DOWN_64);

   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, 0, 0, UP_64, UP_64, PI_64, PI_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, UP_64, UP_64, 0, 0, PI_64, PI_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, UP_64, UP_64, UP_64, UP_64, PI_64, PI_64);

   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, 0, 0, UP_64, UP_64, E_64, E_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, UP_64, UP_64, 0, 0, E_64, E_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, UP_64, UP_64, UP_64, UP_64, E_64, E_64);

   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, 0, 0, UP_64, UP_64, 0, 0);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, UP_64, UP_64, 0, 0, 0, 0);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, UP_64, UP_64, UP_64, UP_64, 0, 0);

   printf("Combinations of DOWN_64 and all other patterns\n");

   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, 0, 0, DOWN_64, DOWN_64, ALL5s_64, ALL5s_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, DOWN_64, DOWN_64, 0, 0, ALL5s_64, ALL5s_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, DOWN_64, DOWN_64, DOWN_64, DOWN_64, ALL5s_64, ALL5s_64);

   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, 0, 0, DOWN_64, DOWN_64, ALLas_64, ALLas_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, DOWN_64, DOWN_64, 0, 0, ALLas_64, ALLas_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, DOWN_64, DOWN_64, DOWN_64, DOWN_64, ALLas_64, ALLas_64);

   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, 0, 0, DOWN_64, DOWN_64, ALLfs_64, ALLfs_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, DOWN_64, DOWN_64, 0, 0, ALLfs_64, ALLfs_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, DOWN_64, DOWN_64, DOWN_64, DOWN_64, ALLfs_64, ALLfs_64);

   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, 0, 0, DOWN_64, DOWN_64, UP_64, UP_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, DOWN_64, DOWN_64, 0, 0, UP_64, UP_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, DOWN_64, DOWN_64, DOWN_64, DOWN_64, UP_64, UP_64);

   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, 0, 0, DOWN_64, DOWN_64, DOWN_64, DOWN_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, DOWN_64, DOWN_64, 0, 0, DOWN_64, DOWN_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, DOWN_64, DOWN_64, DOWN_64, DOWN_64, DOWN_64, DOWN_64);

   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, 0, 0, DOWN_64, DOWN_64, PI_64, PI_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, DOWN_64, DOWN_64, 0, 0, PI_64, PI_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, DOWN_64, DOWN_64, DOWN_64, DOWN_64, PI_64, PI_64);

   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, 0, 0, DOWN_64, DOWN_64, E_64, E_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, DOWN_64, DOWN_64, 0, 0, E_64, E_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, DOWN_64, DOWN_64, DOWN_64, DOWN_64, E_64, E_64);

   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, 0, 0, DOWN_64, DOWN_64, 0, 0);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, DOWN_64, DOWN_64, 0, 0, 0, 0);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, DOWN_64, DOWN_64, DOWN_64, DOWN_64, 0, 0);

   printf("Combinations of PI_64 and all other patterns\n");

   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, 0, 0, PI_64, PI_64, ALL5s_64, ALL5s_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, PI_64, PI_64, 0, 0, ALL5s_64, ALL5s_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, PI_64, PI_64, PI_64, PI_64, ALL5s_64, ALL5s_64);

   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, 0, 0, PI_64, PI_64, ALLas_64, ALLas_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, PI_64, PI_64, 0, 0, ALLas_64, ALLas_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, PI_64, PI_64, PI_64, PI_64, ALLas_64, ALLas_64);

   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, 0, 0, PI_64, PI_64, ALLfs_64, ALLfs_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, PI_64, PI_64, 0, 0, ALLfs_64, ALLfs_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, PI_64, PI_64, PI_64, PI_64, ALLfs_64, ALLfs_64);

   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, 0, 0, PI_64, PI_64, UP_64, UP_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, PI_64, PI_64, 0, 0, UP_64, UP_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, PI_64, PI_64, PI_64, PI_64, UP_64, UP_64);

   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, 0, 0, PI_64, PI_64, DOWN_64, DOWN_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, PI_64, PI_64, 0, 0, DOWN_64, DOWN_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, PI_64, PI_64, PI_64, PI_64, DOWN_64, DOWN_64);

   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, 0, 0, PI_64, PI_64, PI_64, PI_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, PI_64, PI_64, 0, 0, PI_64, PI_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, PI_64, PI_64, PI_64, PI_64, PI_64, PI_64);

   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, 0, 0, PI_64, PI_64, E_64, E_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, PI_64, PI_64, 0, 0, E_64, E_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, PI_64, PI_64, PI_64, PI_64, E_64, E_64);

   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, 0, 0, PI_64, PI_64, 0, 0);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, PI_64, PI_64, 0, 0, 0, 0);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, PI_64, PI_64, PI_64, PI_64, 0, 0);

   printf("Combinations of E_64 and all other patterns\n");

   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, 0, 0, E_64, E_64, ALL5s_64, ALL5s_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, E_64, E_64, 0, 0, ALL5s_64, ALL5s_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, E_64, E_64, E_64, E_64, ALL5s_64, ALL5s_64);

   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, 0, 0, E_64, E_64, ALLas_64, ALLas_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, E_64, E_64, 0, 0, ALLas_64, ALLas_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, E_64, E_64, E_64, E_64, ALLas_64, ALLas_64);

   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, 0, 0, E_64, E_64, ALLfs_64, ALLfs_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, E_64, E_64, 0, 0, ALLfs_64, ALLfs_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, E_64, E_64, E_64, E_64, ALLfs_64, ALLfs_64);

   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, 0, 0, E_64, E_64, UP_64, UP_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, E_64, E_64, 0, 0, UP_64, UP_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, E_64, E_64, E_64, E_64, UP_64, UP_64);

   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, 0, 0, E_64, E_64, DOWN_64, DOWN_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, E_64, E_64, 0, 0, DOWN_64, DOWN_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, E_64, E_64, E_64, E_64, DOWN_64, DOWN_64);

   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, 0, 0, E_64, E_64, PI_64, PI_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, E_64, E_64, 0, 0, PI_64, PI_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, E_64, E_64, E_64, E_64, PI_64, PI_64);

   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, 0, 0, E_64, E_64, E_64, E_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, E_64, E_64, 0, 0, E_64, E_64);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, E_64, E_64, E_64, E_64, E_64, E_64);

   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, 0, 0, E_64, E_64, 0, 0);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, E_64, E_64, 0, 0, 0, 0);
   ATOMIC_TEST_CASP("caspal x8, x9, x10, x11, [x5]", x, memp, E_64, E_64, E_64, E_64, 0, 0);

   printf("CASPL <Xs>, <X(s+1)>, <Xt>, <X(t+1)>, [<Xn|SP>{,#0}]\n\n");

   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, 0, 0, 0, 0, 0, 0);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, 0, 0, 0, 0, 1, 1);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, 0, 0, 1, 1, 0, 0);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, 0, 0, 1, 1, 1, 1);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, 1, 1, 0, 0, 0, 0);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, 1, 1, 0, 0, 1, 1);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, 1, 1, 1, 1, 0, 0);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, 1, 1, 1, 1, 1, 1);

   printf("Combinations of ALL5s_64 and all other patterns\n");

   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALL5s_64, ALL5s_64, ALL5s_64, ALL5s_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, ALL5s_64, ALL5s_64, 0, 0, ALL5s_64, ALL5s_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, ALL5s_64, ALL5s_64, ALL5s_64, ALL5s_64, ALL5s_64, ALL5s_64);

   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALL5s_64, ALL5s_64, ALLas_64, ALLas_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, ALL5s_64, ALL5s_64, 0, 0, ALLas_64, ALLas_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, ALL5s_64, ALL5s_64, ALL5s_64, ALL5s_64, ALLas_64, ALLas_64);

   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALL5s_64, ALL5s_64, ALLfs_64, ALLfs_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, ALL5s_64, ALL5s_64, 0, 0, ALLfs_64, ALLfs_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, ALL5s_64, ALL5s_64, ALL5s_64, ALL5s_64, ALLfs_64, ALLfs_64);

   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALL5s_64, ALL5s_64, UP_64, UP_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, ALL5s_64, ALL5s_64, 0, 0, UP_64, UP_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, ALL5s_64, ALL5s_64, ALL5s_64, ALL5s_64, UP_64, UP_64);

   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALL5s_64, ALL5s_64, DOWN_64, DOWN_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, ALL5s_64, ALL5s_64, 0, 0, DOWN_64, DOWN_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, ALL5s_64, ALL5s_64, ALL5s_64, ALL5s_64, DOWN_64, DOWN_64);

   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALL5s_64, ALL5s_64, PI_64, PI_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, ALL5s_64, ALL5s_64, 0, 0, PI_64, PI_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, ALL5s_64, ALL5s_64, ALL5s_64, ALL5s_64, PI_64, PI_64);

   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALL5s_64, ALL5s_64, E_64, E_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, ALL5s_64, ALL5s_64, 0, 0, E_64, E_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, ALL5s_64, ALL5s_64, ALL5s_64, ALL5s_64, E_64, E_64);

   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALL5s_64, ALL5s_64, 0, 0);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, ALL5s_64, ALL5s_64, 0, 0, 0, 0);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, ALL5s_64, ALL5s_64, ALL5s_64, ALL5s_64, 0, 0);

   printf("Combinations of ALLas_64 and all other patterns\n");

   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALLas_64, ALLas_64, ALL5s_64, ALL5s_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, ALLas_64, ALLas_64, 0, 0, ALL5s_64, ALL5s_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, ALLas_64, ALLas_64, ALLas_64, ALLas_64, ALL5s_64, ALL5s_64);

   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALLas_64, ALLas_64, ALLas_64, ALLas_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, ALLas_64, ALLas_64, 0, 0, ALLas_64, ALLas_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, ALLas_64, ALLas_64, ALLas_64, ALLas_64, ALLas_64, ALLas_64);

   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALLas_64, ALLas_64, ALLfs_64, ALLfs_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, ALLas_64, ALLas_64, 0, 0, ALLfs_64, ALLfs_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, ALLas_64, ALLas_64, ALLas_64, ALLas_64, ALLfs_64, ALLfs_64);

   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALLas_64, ALLas_64, UP_64, UP_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, ALLas_64, ALLas_64, 0, 0, UP_64, UP_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, ALLas_64, ALLas_64, ALLas_64, ALLas_64, UP_64, UP_64);

   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALLas_64, ALLas_64, DOWN_64, DOWN_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, ALLas_64, ALLas_64, 0, 0, DOWN_64, DOWN_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, ALLas_64, ALLas_64, ALLas_64, ALLas_64, DOWN_64, DOWN_64);

   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALLas_64, ALLas_64, PI_64, PI_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, ALLas_64, ALLas_64, 0, 0, PI_64, PI_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, ALLas_64, ALLas_64, ALLas_64, ALLas_64, PI_64, PI_64);

   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALLas_64, ALLas_64, E_64, E_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, ALLas_64, ALLas_64, 0, 0, E_64, E_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, ALLas_64, ALLas_64, ALLas_64, ALLas_64, E_64, E_64);

   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALLas_64, ALLas_64, 0, 0);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, ALLas_64, ALLas_64, 0, 0, 0, 0);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, ALLas_64, ALLas_64, ALLas_64, ALLas_64, 0, 0);

   printf("Combinations of ALLfs_64 and all other patterns\n");

   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALLfs_64, ALLfs_64, ALL5s_64, ALL5s_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, ALLfs_64, ALLfs_64, 0, 0, ALL5s_64, ALL5s_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, ALLfs_64, ALLfs_64, ALLfs_64, ALLfs_64, ALL5s_64, ALL5s_64);

   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALLfs_64, ALLfs_64, ALLas_64, ALLas_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, ALLfs_64, ALLfs_64, 0, 0, ALLas_64, ALLas_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, ALLfs_64, ALLfs_64, ALLfs_64, ALLfs_64, ALLas_64, ALLas_64);

   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALLfs_64, ALLfs_64, ALLfs_64, ALLfs_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, ALLfs_64, ALLfs_64, 0, 0, ALLfs_64, ALLfs_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, ALLfs_64, ALLfs_64, ALLfs_64, ALLfs_64, ALLfs_64, ALLfs_64);

   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALLfs_64, ALLfs_64, UP_64, UP_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, ALLfs_64, ALLfs_64, 0, 0, UP_64, UP_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, ALLfs_64, ALLfs_64, ALLfs_64, ALLfs_64, UP_64, UP_64);

   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALLfs_64, ALLfs_64, DOWN_64, DOWN_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, ALLfs_64, ALLfs_64, 0, 0, DOWN_64, DOWN_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, ALLfs_64, ALLfs_64, ALLfs_64, ALLfs_64, DOWN_64, DOWN_64);

   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALLfs_64, ALLfs_64, PI_64, PI_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, ALLfs_64, ALLfs_64, 0, 0, PI_64, PI_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, ALLfs_64, ALLfs_64, ALLfs_64, ALLfs_64, PI_64, PI_64);

   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALLfs_64, ALLfs_64, E_64, E_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, ALLfs_64, ALLfs_64, 0, 0, E_64, E_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, ALLfs_64, ALLfs_64, ALLfs_64, ALLfs_64, E_64, E_64);

   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, 0, 0, ALLfs_64, ALLfs_64, 0, 0);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, ALLfs_64, ALLfs_64, 0, 0, 0, 0);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, ALLfs_64, ALLfs_64, ALLfs_64, ALLfs_64, 0, 0);

   printf("Combinations of UP_64 and all other patterns\n");

   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, 0, 0, UP_64, UP_64, ALL5s_64, ALL5s_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, UP_64, UP_64, 0, 0, ALL5s_64, ALL5s_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, UP_64, UP_64, UP_64, UP_64, ALL5s_64, ALL5s_64);

   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, 0, 0, UP_64, UP_64, ALLas_64, ALLas_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, UP_64, UP_64, 0, 0, ALLas_64, ALLas_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, UP_64, UP_64, UP_64, UP_64, ALLas_64, ALLas_64);

   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, 0, 0, UP_64, UP_64, ALLfs_64, ALLfs_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, UP_64, UP_64, 0, 0, ALLfs_64, ALLfs_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, UP_64, UP_64, UP_64, UP_64, ALLfs_64, ALLfs_64);

   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, 0, 0, UP_64, UP_64, UP_64, UP_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, UP_64, UP_64, 0, 0, UP_64, UP_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, UP_64, UP_64, UP_64, UP_64, UP_64, UP_64);

   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, 0, 0, UP_64, UP_64, DOWN_64, DOWN_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, UP_64, UP_64, 0, 0, DOWN_64, DOWN_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, UP_64, UP_64, UP_64, UP_64, DOWN_64, DOWN_64);

   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, 0, 0, UP_64, UP_64, PI_64, PI_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, UP_64, UP_64, 0, 0, PI_64, PI_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, UP_64, UP_64, UP_64, UP_64, PI_64, PI_64);

   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, 0, 0, UP_64, UP_64, E_64, E_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, UP_64, UP_64, 0, 0, E_64, E_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, UP_64, UP_64, UP_64, UP_64, E_64, E_64);

   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, 0, 0, UP_64, UP_64, 0, 0);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, UP_64, UP_64, 0, 0, 0, 0);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, UP_64, UP_64, UP_64, UP_64, 0, 0);

   printf("Combinations of DOWN_64 and all other patterns\n");

   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, 0, 0, DOWN_64, DOWN_64, ALL5s_64, ALL5s_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, DOWN_64, DOWN_64, 0, 0, ALL5s_64, ALL5s_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, DOWN_64, DOWN_64, DOWN_64, DOWN_64, ALL5s_64, ALL5s_64);

   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, 0, 0, DOWN_64, DOWN_64, ALLas_64, ALLas_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, DOWN_64, DOWN_64, 0, 0, ALLas_64, ALLas_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, DOWN_64, DOWN_64, DOWN_64, DOWN_64, ALLas_64, ALLas_64);

   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, 0, 0, DOWN_64, DOWN_64, ALLfs_64, ALLfs_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, DOWN_64, DOWN_64, 0, 0, ALLfs_64, ALLfs_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, DOWN_64, DOWN_64, DOWN_64, DOWN_64, ALLfs_64, ALLfs_64);

   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, 0, 0, DOWN_64, DOWN_64, UP_64, UP_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, DOWN_64, DOWN_64, 0, 0, UP_64, UP_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, DOWN_64, DOWN_64, DOWN_64, DOWN_64, UP_64, UP_64);

   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, 0, 0, DOWN_64, DOWN_64, DOWN_64, DOWN_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, DOWN_64, DOWN_64, 0, 0, DOWN_64, DOWN_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, DOWN_64, DOWN_64, DOWN_64, DOWN_64, DOWN_64, DOWN_64);

   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, 0, 0, DOWN_64, DOWN_64, PI_64, PI_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, DOWN_64, DOWN_64, 0, 0, PI_64, PI_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, DOWN_64, DOWN_64, DOWN_64, DOWN_64, PI_64, PI_64);

   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, 0, 0, DOWN_64, DOWN_64, E_64, E_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, DOWN_64, DOWN_64, 0, 0, E_64, E_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, DOWN_64, DOWN_64, DOWN_64, DOWN_64, E_64, E_64);

   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, 0, 0, DOWN_64, DOWN_64, 0, 0);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, DOWN_64, DOWN_64, 0, 0, 0, 0);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, DOWN_64, DOWN_64, DOWN_64, DOWN_64, 0, 0);

   printf("Combinations of PI_64 and all other patterns\n");

   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, 0, 0, PI_64, PI_64, ALL5s_64, ALL5s_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, PI_64, PI_64, 0, 0, ALL5s_64, ALL5s_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, PI_64, PI_64, PI_64, PI_64, ALL5s_64, ALL5s_64);

   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, 0, 0, PI_64, PI_64, ALLas_64, ALLas_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, PI_64, PI_64, 0, 0, ALLas_64, ALLas_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, PI_64, PI_64, PI_64, PI_64, ALLas_64, ALLas_64);

   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, 0, 0, PI_64, PI_64, ALLfs_64, ALLfs_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, PI_64, PI_64, 0, 0, ALLfs_64, ALLfs_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, PI_64, PI_64, PI_64, PI_64, ALLfs_64, ALLfs_64);

   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, 0, 0, PI_64, PI_64, UP_64, UP_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, PI_64, PI_64, 0, 0, UP_64, UP_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, PI_64, PI_64, PI_64, PI_64, UP_64, UP_64);

   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, 0, 0, PI_64, PI_64, DOWN_64, DOWN_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, PI_64, PI_64, 0, 0, DOWN_64, DOWN_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, PI_64, PI_64, PI_64, PI_64, DOWN_64, DOWN_64);

   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, 0, 0, PI_64, PI_64, PI_64, PI_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, PI_64, PI_64, 0, 0, PI_64, PI_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, PI_64, PI_64, PI_64, PI_64, PI_64, PI_64);

   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, 0, 0, PI_64, PI_64, E_64, E_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, PI_64, PI_64, 0, 0, E_64, E_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, PI_64, PI_64, PI_64, PI_64, E_64, E_64);

   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, 0, 0, PI_64, PI_64, 0, 0);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, PI_64, PI_64, 0, 0, 0, 0);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, PI_64, PI_64, PI_64, PI_64, 0, 0);

   printf("Combinations of E_64 and all other patterns\n");

   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, 0, 0, E_64, E_64, ALL5s_64, ALL5s_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, E_64, E_64, 0, 0, ALL5s_64, ALL5s_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, E_64, E_64, E_64, E_64, ALL5s_64, ALL5s_64);

   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, 0, 0, E_64, E_64, ALLas_64, ALLas_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, E_64, E_64, 0, 0, ALLas_64, ALLas_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, E_64, E_64, E_64, E_64, ALLas_64, ALLas_64);

   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, 0, 0, E_64, E_64, ALLfs_64, ALLfs_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, E_64, E_64, 0, 0, ALLfs_64, ALLfs_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, E_64, E_64, E_64, E_64, ALLfs_64, ALLfs_64);

   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, 0, 0, E_64, E_64, UP_64, UP_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, E_64, E_64, 0, 0, UP_64, UP_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, E_64, E_64, E_64, E_64, UP_64, UP_64);

   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, 0, 0, E_64, E_64, DOWN_64, DOWN_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, E_64, E_64, 0, 0, DOWN_64, DOWN_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, E_64, E_64, E_64, E_64, DOWN_64, DOWN_64);

   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, 0, 0, E_64, E_64, PI_64, PI_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, E_64, E_64, 0, 0, PI_64, PI_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, E_64, E_64, E_64, E_64, PI_64, PI_64);

   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, 0, 0, E_64, E_64, E_64, E_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, E_64, E_64, 0, 0, E_64, E_64);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, E_64, E_64, E_64, E_64, E_64, E_64);

   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, 0, 0, E_64, E_64, 0, 0);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, E_64, E_64, 0, 0, 0, 0);
   ATOMIC_TEST_CASP("caspl x8, x9, x10, x11, [x5]", x, memp, E_64, E_64, E_64, E_64, 0, 0);

   free(memp);
}

int main ( void )
{
   test_atomics();
   return 0;
}
