
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
   The insn may only mention (%eax) and esi. */

#define GEN_test_Monly(_name, _mem_form)   \
    \
    __attribute__ ((noinline)) static void test_##_name ( void )   \
    { \
       Block* b = memalign(32, sizeof(Block)); \
       randBlock(b); \
       printf("%s\n", #_name); \
       showBlock("before", b); \
       __asm__ __volatile__( \
          "leal      16(%0),%%eax"  "\n\t" \
          "movl      24(%0),%%esi"   "\n\t" \
          _mem_form  "\n\t" \
          "movl      %%esi, 32(%0)"  "\n\t" \
          : /*OUT*/  \
          : /*IN*/"r"(b) \
          : /*TRASH*/"esi","eax","memory","cc" \
       ); \
       showBlock("after ", b); \
       printf("\n"); \
       free(b); \
    }

GEN_test_Monly( MOVBE_RtoM_32, "movbel %%esi,1(%%eax)")
GEN_test_Monly( MOVBE_RtoM_16, "movbew %%si,1(%%eax)")

GEN_test_Monly( MOVBE_MtoR_32, "movbel 1(%%eax), %%esi")
GEN_test_Monly( MOVBE_MtoR_16, "movbew 1(%%eax), %%si")

int main ( void )
{
   test_MOVBE_RtoM_32();
   test_MOVBE_RtoM_16();
   test_MOVBE_MtoR_32();
   test_MOVBE_MtoR_16();
   return 0;
}
