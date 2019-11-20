
/* To compile:
   aarch64-linux-gnu-gcc -march=armv8.1-a -Wall -g -O0 -o atomics_v8.1 \
                         none/tests/arm64/atomics_v8.1.c
*/

#include <stdio.h>
#include <malloc.h>
#include <stdint.h>

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

// Test patterns
#define ALL5s_64 0x5555555555555555ULL
#define ALLas_64 0xAAAAAAAAAAAAAAAAULL
#define ALLfs_64 0xFFFFFFFFFFFFFFFFULL
#define UP_64    0x0123456789ABCDEFULL
#define DOWN_64  0xFEDCBA9876543210ULL
#define PI_64    0x3141592653589793ULL
#define E_64     0x2718281828459045ULL

#define ALL5s_32 0x55555555ULL
#define ALLas_32 0xAAAAAAAAULL
#define ALLfs_32 0xFFFFFFFFULL
#define UP_32    0x01234567ULL
#define DOWN_32  0xFEDCBA98ULL
#define PI_32    0x31415926ULL
#define E_32     0x27182818ULL

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

   free(mem);
}

int main ( void )
{
   test_atomics();
   return 0;
}
