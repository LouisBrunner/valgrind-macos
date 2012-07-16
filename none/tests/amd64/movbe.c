
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <malloc.h>

typedef  unsigned char           UChar;
typedef  unsigned int            UInt;
typedef  unsigned long int       UWord;
typedef  unsigned long long int  ULong;


typedef  struct { UChar cs[40]; }  Block;

void showBlock ( char* msg, Block* b )
{
   int i;
   printf("  %s ", msg);
   for (i = 0; i < 40; i++) 
      printf("%02x", (UInt)b->cs[i]);
   printf("\n");
}

UChar randUChar ( void )
{
   static UInt seed = 80021;
   seed = 1103515245 * seed + 12345;
   return (seed >> 17) & 0xFF;
}

void randBlock ( Block* b )
{
   int i;
   UChar* p = (UChar*)b;
   for (i = 0; i < sizeof(Block); i++)
      p[i] = randUChar();
}

/* Generate a function test_NAME, that tests the given insn.
   The insn may only mention (%rax) and r9. */

#define GEN_test_Monly(_name, _mem_form)   \
    \
    __attribute__ ((noinline)) static void test_##_name ( void )   \
    { \
       Block* b = memalign(32, sizeof(Block)); \
       randBlock(b); \
       printf("%s\n", #_name); \
       showBlock("before", b); \
       __asm__ __volatile__( \
          "leaq      16(%0),%%rax"  "\n\t" \
          "movq      24(%0),%%r9"   "\n\t" \
          _mem_form  "\n\t" \
          "movq      %%r9, 32(%0)"  "\n\t" \
          : /*OUT*/  \
          : /*IN*/"r"(b) \
          : /*TRASH*/"r9","rax","memory","cc" \
       ); \
       showBlock("after ", b); \
       printf("\n"); \
       free(b); \
    }

GEN_test_Monly( MOVBE_RtoM_64, "movbe %%r9, 1(%%rax)")
GEN_test_Monly( MOVBE_RtoM_32, "movbe %%r9d,1(%%rax)")
GEN_test_Monly( MOVBE_RtoM_16, "movbe %%r9w,1(%%rax)")

GEN_test_Monly( MOVBE_MtoR_64, "movbe 1(%%rax), %%r9")
GEN_test_Monly( MOVBE_MtoR_32, "movbe 1(%%rax), %%r9d")
GEN_test_Monly( MOVBE_MtoR_16, "movbe 1(%%rax), %%r9w")

int main ( void )
{
   test_MOVBE_RtoM_64();
   test_MOVBE_RtoM_32();
   test_MOVBE_RtoM_16();
   test_MOVBE_MtoR_64();
   test_MOVBE_MtoR_32();
   test_MOVBE_MtoR_16();
   return 0;
}
