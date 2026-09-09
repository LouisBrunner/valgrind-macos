
/* Tests e-vs-i or i-vs-m aspects for pcmp{e,i}str{i,m}.  Does not
   check the core arithmetic in any detail.  */

#include <string.h>
#include <stdio.h>
#include <assert.h>

typedef  unsigned char  V128[16];
typedef  unsigned int   UInt;
typedef  signed int     Int;
typedef  unsigned char  UChar;
typedef  UChar          Bool;
#define False ((Bool)0)
#define True  ((Bool)1)

void show_V128 ( V128* vec )
{
   Int i;
   for (i = 15; i >= 0; i--)
      printf("%02x", (UInt)( (*vec)[i] ));
}

void expand ( V128* dst, char* summary )
{
   Int i;
   assert( strlen(summary) == 16 );
   for (i = 0; i < 16; i++) {
      UChar xx = 0;
      UChar x = summary[15-i];
      if      (x >= '0' && x <= '9') { xx = x - '0'; }
      else if (x >= 'A' && x <= 'F') { xx = x - 'A' + 10; }
      else if (x >= 'a' && x <= 'f') { xx = x - 'a' + 10; }
      else assert(0);

      assert(xx < 16);
      xx = (xx << 4) | xx;
      assert(xx < 256);
      (*dst)[i] = xx;
   }
}

#define one_subtest(name, imm8b6) \
   memset(blockC, 0x55, 64); \
   memcpy(blockC + 0,  &argL,  16); \
   memcpy(blockC + 16, &argR,  16); \
   memcpy(blockC + 32, &edxIN, 4); \
   memcpy(blockC + 36, &eaxIN, 4); \
   memcpy(blockC + 40, &edxIN, 4); \
   __asm__ __volatile__( \
      "movupd     0(%0), %%xmm2"           "\n\t" \
      "movupd     16(%0), %%xmm7"         "\n\t" \
      "movl       32(%0), %%edx"           "\n\t" \
      "movl       36(%0), %%eax"           "\n\t" \
      "movupd     40(%0), %%xmm0"          "\n\t" \
      "movl       56(%0), %%ecx"            "\n\t" \
      "pcmp" #name " $0x" #imm8b6 ", %%xmm2, %%xmm7"  "\n\t" \
      "movupd     %%xmm0, 40(%0)"          "\n\t" \
      "movl       %%ecx, 56(%0)"            "\n\t" \
      "pushfl"                             "\n\t" \
      "popl       %%edi"                   "\n\t" \
      "movl       %%edi, 60(%0)"           "\n\t" \
      : /*out*/  \
      : /*in*/"r"(blockC)  \
      : /*trash*/"memory","cc","xmm2","xmm7","xmm0","edx","eax","ecx","edi" \
   ); \
   printf("  " #name " $0x" #imm8b6 ":  "); \
   printf("    xmm0 "); \
   show_V128( (V128*)(blockC+40) ); \
   printf("  ecx %08x  flags %08x\n", (UInt)block[14], block[15] & 0x8D5);

void one_test ( char* summL, UInt edxIN, char* summR, UInt eaxIN )
{
   V128 argL, argR;
   expand( &argL, summL );
   expand( &argR, summR );
   printf("\n");
   printf("edx %016x  argL ", edxIN);
   show_V128(&argL);
   printf("  eax %016x  argR ", eaxIN);
   show_V128(&argR);
   printf("\n");

   UInt block[ 4/*in:argL*/          // 0   0
               + 4/*in:argR*/        // 4   16
               + 1/*in:edx*/         // 8   32
               + 1/*in:eax*/         // 9   36
               + 4/*inout:xmm0*/     // 10  40
               + 1/*inout:ecx*/      // 14  56
               + 1/*out:rflags*/ ];  // 15  60
   assert(sizeof(block) == 64);

   UChar* blockC = (UChar*)&block[0];

   one_subtest(istri, 4A)
   one_subtest(istri, 0A)
   one_subtest(istrm, 4A)
   one_subtest(istrm, 0A)
   one_subtest(estri, 4A)
   one_subtest(estri, 0A)
   one_subtest(estrm, 4A)
   one_subtest(estrm, 0A)

}

int main ( void )
{
   one_test("aaaaaaaaaaaaaaaa", 0, "aaaaaaaa0aaaaaaa", 0 );
   one_test("0000000000000000", 0, "aaaaaaaa0aaaaaaa", 0 );

   one_test("aaaaaaaaaaaaaaaa", 0, "aaaaaaaaaaaaaaaa", 0 );
   one_test("aaaaaaaaaaaaaaaa", 5, "aaaaaaaaaaaaaaaa", 0 );
   one_test("aaaaaaaaaaaaaaaa", 0, "aaaaaaaaaaaaaaaa", 6 );

   one_test("aaaaaaaaaaaaaaaa", 5, "aaaaaaaaaaaaaaaa", 6 );
   one_test("aaaaaaaaaaaaaaaa", 5, "aaaaaaaaaaaaaaaa", 15 );
   one_test("aaaaaaaaaaaaaaaa", 5, "aaaaaaaaaaaaaaaa", 16 );
   one_test("aaaaaaaaaaaaaaaa", 5, "aaaaaaaaaaaaaaaa", 17 );

   one_test("aaaaaaaaaaaaaaaa", 5, "aaaaaaaaaaaaaaaa", -6 );
   one_test("aaaaaaaaaaaaaaaa", 5, "aaaaaaaaaaaaaaaa", -15 );
   one_test("aaaaaaaaaaaaaaaa", 5, "aaaaaaaaaaaaaaaa", -16 );
   one_test("aaaaaaaaaaaaaaaa", 5, "aaaaaaaaaaaaaaaa", -17 );

   one_test("aaaaaaaaaaaaaaaa", 5,  "aaaaaaaaaaaaaaaa", 6 );
   one_test("aaaaaaaaaaaaaaaa", 15, "aaaaaaaaaaaaaaaa", 6 );
   one_test("aaaaaaaaaaaaaaaa", 16, "aaaaaaaaaaaaaaaa", 6 );
   one_test("aaaaaaaaaaaaaaaa", 17, "aaaaaaaaaaaaaaaa", 6 );

   one_test("aaaaaaaaaaaaaaaa", -5,  "aaaaaaaaaaaaaaaa", 6 );
   one_test("aaaaaaaaaaaaaaaa", -15, "aaaaaaaaaaaaaaaa", 6 );
   one_test("aaaaaaaaaaaaaaaa", -16, "aaaaaaaaaaaaaaaa", 6 );
   one_test("aaaaaaaaaaaaaaaa", -17, "aaaaaaaaaaaaaaaa", 6 );

   return 0;
}
