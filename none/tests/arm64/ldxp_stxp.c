
/* Note, this is only a basic smoke test of LD{A}XP and ST{L}XP.  Their
   atomicity properties are tested by memcheck/tests/atomic_incs.c. */

#include <stdio.h>
#include <stdlib.h>
#include <malloc.h>
#include <assert.h>

typedef  unsigned int            UInt;
typedef  unsigned long long int  ULong;


void initBlock ( ULong* block )
{
   block[0] = 0x0001020304050607ULL;
   block[1] = 0x1011121314151617ULL;
   block[2] = 0x2021222324252627ULL;
   block[3] = 0x3031323334353637ULL;
   block[4] = 0x4041424344454647ULL;
   block[5] = 0x5051525354555657ULL;
}

void printBlock ( const char* who,
                  ULong* block, ULong rt1contents, ULong rt2contents,
                  UInt zeroIfSuccess )
{
   printf("Block %s (%s)\n", who, zeroIfSuccess == 0 ? "success" : "FAILURE" );
   for (int i = 0; i < 6; i++) {
      printf("0x%016llx\n", block[i]);
   }
   printf("0x%016llx rt1contents\n", rt1contents);
   printf("0x%016llx rt2contents\n", rt2contents);
   printf("\n");
}

int main ( void )
{
   ULong* block = memalign(16, 6 * sizeof(ULong));
   assert(block);

   ULong rt1in, rt2in, rt1out, rt2out;
   UInt scRes;

   // Do ldxp then stxp with x-registers
   initBlock(block);
   rt1in  = 0x5555666677778888ULL;
   rt2in  = 0xAAAA9999BBBB0000ULL;
   rt1out = 0x1111222233334444ULL;
   rt2out = 0xFFFFEEEEDDDDCCCCULL;
   scRes  = 0x55555555;
   __asm__ __volatile__(
      "ldxp %1, %2, [%5]"       "\n\t"
      "stxp %w0, %3, %4, [%5]"  "\n\t"
      : /*OUT*/
        "=&r"(scRes),  // %0
        "=&r"(rt1out), // %1
        "=&r"(rt2out)  // %2
      : /*IN*/
        "r"(rt1in),    // %3
        "r"(rt2in),    // %4
        "r"(&block[2]) // %5
      : /*TRASH*/
        "memory","cc"
   );
   printBlock("after ldxp/stxp 2x64-bit", block, rt1out, rt2out, scRes);

   // Do ldxp then stxp with w-registers
   initBlock(block);
   rt1in  = 0x5555666677778888ULL;
   rt2in  = 0xAAAA9999BBBB0000ULL;
   rt1out = 0x1111222233334444ULL;
   rt2out = 0xFFFFEEEEDDDDCCCCULL;
   scRes  = 0x55555555;
   __asm__ __volatile__(
      "ldxp %w1, %w2, [%5]"       "\n\t"
      "stxp %w0, %w3, %w4, [%5]"  "\n\t"
      : /*OUT*/
        "=&r"(scRes),  // %0
        "=&r"(rt1out), // %1
        "=&r"(rt2out)  // %2
      : /*IN*/
        "r"(rt1in),    // %3
        "r"(rt2in),    // %4
        "r"(&block[2]) // %5
      : /*TRASH*/
        "memory","cc"
   );
   printBlock("after ldxp/stxp 2x32-bit", block, rt1out, rt2out, scRes);

   free(block);
   return 0;
}
