// This module does unit testing of m_libcbase.

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>

#include "pub_tool_basics.h"  /* UInt et al, needed for pub_tool_vki.h */
#include "pub_tool_vki.h"
#include "m_libcbase.c"

/* On PPC, MIPS and ARM64 Linux VKI_PAGE_SIZE is a variable, not a macro. */
#if defined(VGP_ppc32_linux) || defined(VGP_ppc64be_linux) \
    || defined(VGP_ppc64le_linux)
unsigned long VKI_PAGE_SIZE  = 1UL << 12;
#elif defined(VGP_arm64_linux)
unsigned long VKI_PAGE_SIZE  = 1UL << 16;
#elif defined(VGP_mips32_linux) || defined(VGP_mips64_linux) \
    || defined (VGP_nanomips_linux)
#include <unistd.h>
unsigned long VKI_PAGE_SIZE;
#endif

/* Provide a stub to not have to pull in m_debuglog.c */
void VG_(debugLog) ( Int level, const HChar* modulename,
                                const HChar* format, ... )
{
   va_list args;
   va_start(args, format);
   fprintf(stderr, "debuglog: %s: ", modulename);
   vfprintf(stderr, format, args);
   va_end(args);
}

/* Provide a stub to not have to pull in m_libcassert.c */
void VG_(exit_now)( Int status )
{
   exit(status);
}


#define  CHECK(x) \
   if (!(x)) { fprintf(stderr, "failure: %s:%d\n", __FILE__, __LINE__); }


void test_VG_STREQ(void)
{
   CHECK( ! VG_STREQ(NULL,    NULL) );  // Nb: strcmp() considers these equal
   CHECK( ! VG_STREQ(NULL,    "ab") );  // Nb: strcmp() seg faults on this
   CHECK( ! VG_STREQ("ab",    NULL) );  // Nb: strcmp() seg faults on this
   CHECK( ! VG_STREQ("",      "a")  );
   CHECK( ! VG_STREQ("a",     "")   );
   CHECK( ! VG_STREQ("abc",   "abcd"));
   CHECK( ! VG_STREQ("abcd",  "abc") );
   CHECK( ! VG_STREQ("Abcd",  "abcd"));
   CHECK( ! VG_STREQ("abcd",  "Abcd"));

   CHECK( VG_STREQ("",     "") );
   CHECK( VG_STREQ("a",    "a") );
   CHECK( VG_STREQ("abcd", "abcd") );
}

void test_VG_STREQN(void)
{
   CHECK( ! VG_STREQN(0, NULL,    NULL) );
   CHECK( ! VG_STREQN(5, NULL,    NULL) );
   CHECK( ! VG_STREQN(0, NULL,    "ab") );
   CHECK( ! VG_STREQN(5, NULL,    "ab") );
   CHECK( ! VG_STREQN(0, "ab",    NULL) );
   CHECK( ! VG_STREQN(1, "",      "a")  );
   CHECK( ! VG_STREQN(1, "a",     "")   );
   CHECK( ! VG_STREQN(4, "abc",   "abcd"));
   CHECK( ! VG_STREQN(4, "abcd",  "abc") );
   CHECK( ! VG_STREQN(1, "Abcd",  "abcd"));
   CHECK( ! VG_STREQN(4, "Abcd",  "abcd"));
   CHECK( ! VG_STREQN(4, "abcd",  "abce"));
   CHECK( ! VG_STREQN(9, "abcd",  "abce"));

   CHECK( VG_STREQN(0, "",     "") );
   CHECK( VG_STREQN(1, "",     "") );
   CHECK( VG_STREQN(0, "a",    "a") );
   CHECK( VG_STREQN(1, "a",    "a") );
   CHECK( VG_STREQN(2, "a",    "a") );
   CHECK( VG_STREQN(9, "a",    "a") );
   CHECK( VG_STREQN(1, "ab",   "ac"));
   CHECK( VG_STREQN(3, "abcd", "abce"));
}

void test_VG_IS_XYZ_ALIGNED(void)
{
   CHECK(   VG_IS_2_ALIGNED(0x0) );
   CHECK( ! VG_IS_2_ALIGNED(0x1) );
   CHECK(   VG_IS_2_ALIGNED(0x2) );
   CHECK( ! VG_IS_2_ALIGNED(0x3) );
   CHECK(   VG_IS_2_ALIGNED(0x4) );
   CHECK( ! VG_IS_2_ALIGNED(0x5) );
   CHECK(   VG_IS_2_ALIGNED(0x6) );
   CHECK( ! VG_IS_2_ALIGNED(0x7) );
   CHECK(   VG_IS_2_ALIGNED(0x8) );
   CHECK( ! VG_IS_2_ALIGNED(0x9) );
   CHECK(   VG_IS_2_ALIGNED(0xa) );
   CHECK( ! VG_IS_2_ALIGNED(0xb) );
   CHECK(   VG_IS_2_ALIGNED(0xc) );
   CHECK( ! VG_IS_2_ALIGNED(0xd) );
   CHECK(   VG_IS_2_ALIGNED(0xe) );
   CHECK( ! VG_IS_2_ALIGNED(0xf) );

   CHECK(   VG_IS_4_ALIGNED(0x0) );
   CHECK( ! VG_IS_4_ALIGNED(0x1) );
   CHECK( ! VG_IS_4_ALIGNED(0x2) );
   CHECK( ! VG_IS_4_ALIGNED(0x3) );
   CHECK(   VG_IS_4_ALIGNED(0x4) );
   CHECK( ! VG_IS_4_ALIGNED(0x5) );
   CHECK( ! VG_IS_4_ALIGNED(0x6) );
   CHECK( ! VG_IS_4_ALIGNED(0x7) );
   CHECK(   VG_IS_4_ALIGNED(0x8) );
   CHECK( ! VG_IS_4_ALIGNED(0x9) );
   CHECK( ! VG_IS_4_ALIGNED(0xa) );
   CHECK( ! VG_IS_4_ALIGNED(0xb) );
   CHECK(   VG_IS_4_ALIGNED(0xc) );
   CHECK( ! VG_IS_4_ALIGNED(0xd) );
   CHECK( ! VG_IS_4_ALIGNED(0xe) );
   CHECK( ! VG_IS_4_ALIGNED(0xf) );

   CHECK(   VG_IS_8_ALIGNED(0x0) );
   CHECK( ! VG_IS_8_ALIGNED(0x1) );
   CHECK( ! VG_IS_8_ALIGNED(0x2) );
   CHECK( ! VG_IS_8_ALIGNED(0x3) );
   CHECK( ! VG_IS_8_ALIGNED(0x4) );
   CHECK( ! VG_IS_8_ALIGNED(0x5) );
   CHECK( ! VG_IS_8_ALIGNED(0x6) );
   CHECK( ! VG_IS_8_ALIGNED(0x7) );
   CHECK(   VG_IS_8_ALIGNED(0x8) );
   CHECK( ! VG_IS_8_ALIGNED(0x9) );
   CHECK( ! VG_IS_8_ALIGNED(0xa) );
   CHECK( ! VG_IS_8_ALIGNED(0xb) );
   CHECK( ! VG_IS_8_ALIGNED(0xc) );
   CHECK( ! VG_IS_8_ALIGNED(0xd) );
   CHECK( ! VG_IS_8_ALIGNED(0xe) );
   CHECK( ! VG_IS_8_ALIGNED(0xf) );

   CHECK(   VG_IS_16_ALIGNED(0x0) );
   CHECK( ! VG_IS_16_ALIGNED(0x1) );
   CHECK( ! VG_IS_16_ALIGNED(0x2) );
   CHECK( ! VG_IS_16_ALIGNED(0x3) );
   CHECK( ! VG_IS_16_ALIGNED(0x4) );
   CHECK( ! VG_IS_16_ALIGNED(0x5) );
   CHECK( ! VG_IS_16_ALIGNED(0x6) );
   CHECK( ! VG_IS_16_ALIGNED(0x7) );
   CHECK( ! VG_IS_16_ALIGNED(0x8) );
   CHECK( ! VG_IS_16_ALIGNED(0x9) );
   CHECK( ! VG_IS_16_ALIGNED(0xa) );
   CHECK( ! VG_IS_16_ALIGNED(0xb) );
   CHECK( ! VG_IS_16_ALIGNED(0xc) );
   CHECK( ! VG_IS_16_ALIGNED(0xd) );
   CHECK( ! VG_IS_16_ALIGNED(0xe) );
   CHECK( ! VG_IS_16_ALIGNED(0xf) );

   CHECK(   VG_IS_WORD_ALIGNED(0x0) );
   CHECK( ! VG_IS_WORD_ALIGNED(0x1) );
   CHECK( ! VG_IS_WORD_ALIGNED(0x2) );
   CHECK( ! VG_IS_WORD_ALIGNED(0x3) );
   // 0x4 case below  
   CHECK( ! VG_IS_WORD_ALIGNED(0x5) );
   CHECK( ! VG_IS_WORD_ALIGNED(0x6) );
   CHECK( ! VG_IS_WORD_ALIGNED(0x7) );
   CHECK(   VG_IS_WORD_ALIGNED(0x8) );
   CHECK( ! VG_IS_WORD_ALIGNED(0x9) );
   CHECK( ! VG_IS_WORD_ALIGNED(0xa) );
   CHECK( ! VG_IS_WORD_ALIGNED(0xb) );
   // 0xc case below  
   CHECK( ! VG_IS_WORD_ALIGNED(0xd) );
   CHECK( ! VG_IS_WORD_ALIGNED(0xe) );
   CHECK( ! VG_IS_WORD_ALIGNED(0xf) );
   if        (4 == sizeof(void*)) {
      CHECK(   VG_IS_WORD_ALIGNED(0x4) );
      CHECK(   VG_IS_WORD_ALIGNED(0xc) );
   } else if (8 == sizeof(void*)) {
      CHECK( ! VG_IS_WORD_ALIGNED(0x4) );
      CHECK( ! VG_IS_WORD_ALIGNED(0xc) );
   } else {
      assert(0);
   }

   CHECK(   VG_IS_PAGE_ALIGNED(0x0) );
   CHECK( ! VG_IS_PAGE_ALIGNED(0x1) );
   CHECK( ! VG_IS_PAGE_ALIGNED(0x2) );
   CHECK( ! VG_IS_PAGE_ALIGNED(0x3) );
   CHECK( ! VG_IS_PAGE_ALIGNED(0x4) );
   CHECK( ! VG_IS_PAGE_ALIGNED(VKI_PAGE_SIZE-1) );
   CHECK(   VG_IS_PAGE_ALIGNED(VKI_PAGE_SIZE  ) );
   CHECK( ! VG_IS_PAGE_ALIGNED(VKI_PAGE_SIZE+1) );
}

void test_VG_ROUND_et_al()
{
   CHECK( 0 == VG_ROUNDDN(0, 1) );
   CHECK( 1 == VG_ROUNDDN(1, 1) );
   CHECK( 2 == VG_ROUNDDN(2, 1) );
   CHECK( 3 == VG_ROUNDDN(3, 1) );
   CHECK( 4 == VG_ROUNDDN(4, 1) );
   CHECK( 5 == VG_ROUNDDN(5, 1) );
   CHECK( 6 == VG_ROUNDDN(6, 1) );
   CHECK( 7 == VG_ROUNDDN(7, 1) );

   CHECK( 0 == VG_ROUNDUP(0, 1) );
   CHECK( 1 == VG_ROUNDUP(1, 1) );
   CHECK( 2 == VG_ROUNDUP(2, 1) );
   CHECK( 3 == VG_ROUNDUP(3, 1) );
   CHECK( 4 == VG_ROUNDUP(4, 1) );
   CHECK( 5 == VG_ROUNDUP(5, 1) );
   CHECK( 6 == VG_ROUNDUP(6, 1) );
   CHECK( 7 == VG_ROUNDUP(7, 1) );

   CHECK( 0 == VG_ROUNDDN(0, 2) );
   CHECK( 0 == VG_ROUNDDN(1, 2) );
   CHECK( 2 == VG_ROUNDDN(2, 2) );
   CHECK( 2 == VG_ROUNDDN(3, 2) );
   CHECK( 4 == VG_ROUNDDN(4, 2) );
   CHECK( 4 == VG_ROUNDDN(5, 2) );
   CHECK( 6 == VG_ROUNDDN(6, 2) );
   CHECK( 6 == VG_ROUNDDN(7, 2) );

   CHECK( 0 == VG_ROUNDUP(0, 2) );
   CHECK( 2 == VG_ROUNDUP(1, 2) );
   CHECK( 2 == VG_ROUNDUP(2, 2) );
   CHECK( 4 == VG_ROUNDUP(3, 2) );
   CHECK( 4 == VG_ROUNDUP(4, 2) );
   CHECK( 6 == VG_ROUNDUP(5, 2) );
   CHECK( 6 == VG_ROUNDUP(6, 2) );
   CHECK( 8 == VG_ROUNDUP(7, 2) );

   CHECK( 0 == VG_ROUNDDN(0, 4) );
   CHECK( 0 == VG_ROUNDDN(1, 4) );
   CHECK( 0 == VG_ROUNDDN(2, 4) );
   CHECK( 0 == VG_ROUNDDN(3, 4) );
   CHECK( 4 == VG_ROUNDDN(4, 4) );
   CHECK( 4 == VG_ROUNDDN(5, 4) );
   CHECK( 4 == VG_ROUNDDN(6, 4) );
   CHECK( 4 == VG_ROUNDDN(7, 4) );

   CHECK( 0 == VG_ROUNDUP(0, 4) );
   CHECK( 4 == VG_ROUNDUP(1, 4) );
   CHECK( 4 == VG_ROUNDUP(2, 4) );
   CHECK( 4 == VG_ROUNDUP(3, 4) );
   CHECK( 4 == VG_ROUNDUP(4, 4) );
   CHECK( 8 == VG_ROUNDUP(5, 4) );
   CHECK( 8 == VG_ROUNDUP(6, 4) );
   CHECK( 8 == VG_ROUNDUP(7, 4) );

   CHECK( 0 == VG_ROUNDDN(0, 8) );
   CHECK( 0 == VG_ROUNDDN(1, 8) );
   CHECK( 0 == VG_ROUNDDN(2, 8) );
   CHECK( 0 == VG_ROUNDDN(3, 8) );
   CHECK( 0 == VG_ROUNDDN(4, 8) );
   CHECK( 0 == VG_ROUNDDN(5, 8) );
   CHECK( 0 == VG_ROUNDDN(6, 8) );
   CHECK( 0 == VG_ROUNDDN(7, 8) );

   CHECK( 0 == VG_ROUNDUP(0, 8) );
   CHECK( 8 == VG_ROUNDUP(1, 8) );
   CHECK( 8 == VG_ROUNDUP(2, 8) );
   CHECK( 8 == VG_ROUNDUP(3, 8) );
   CHECK( 8 == VG_ROUNDUP(4, 8) );
   CHECK( 8 == VG_ROUNDUP(5, 8) );
   CHECK( 8 == VG_ROUNDUP(6, 8) );
   CHECK( 8 == VG_ROUNDUP(7, 8) );

   CHECK( 0             == VG_PGROUNDDN(0) );
   CHECK( 0             == VG_PGROUNDDN(1) );
   CHECK( 0             == VG_PGROUNDDN(2) );
   CHECK( 0             == VG_PGROUNDDN(3) );
   CHECK( 0             == VG_PGROUNDDN(4) );
   CHECK( 0             == VG_PGROUNDDN(VKI_PAGE_SIZE-1) );
   CHECK( VKI_PAGE_SIZE == VG_PGROUNDDN(VKI_PAGE_SIZE  ) );
   CHECK( VKI_PAGE_SIZE == VG_PGROUNDDN(VKI_PAGE_SIZE+1) );

   CHECK( 0               == VG_PGROUNDUP(0) );
   CHECK( VKI_PAGE_SIZE   == VG_PGROUNDUP(1) );
   CHECK( VKI_PAGE_SIZE   == VG_PGROUNDUP(2) );
   CHECK( VKI_PAGE_SIZE   == VG_PGROUNDUP(3) );
   CHECK( VKI_PAGE_SIZE   == VG_PGROUNDUP(4) );
   CHECK( VKI_PAGE_SIZE   == VG_PGROUNDUP(VKI_PAGE_SIZE-1) );
   CHECK( VKI_PAGE_SIZE   == VG_PGROUNDUP(VKI_PAGE_SIZE  ) );
   CHECK( VKI_PAGE_SIZE*2 == VG_PGROUNDUP(VKI_PAGE_SIZE+1) );
}

void test_isspace(void)
{
   CHECK(   VG_(isspace)(' ') );
   CHECK(   VG_(isspace)('\n') );
   CHECK(   VG_(isspace)('\t') );
   CHECK( ! VG_(isspace)('3') );
   CHECK( ! VG_(isspace)('x') );
}

void test_isdigit(void)
{
   CHECK(   VG_(isdigit)('0') );
   CHECK(   VG_(isdigit)('1') );
   CHECK(   VG_(isdigit)('5') );
   CHECK(   VG_(isdigit)('9') );
   CHECK( ! VG_(isdigit)('a') );
   CHECK( ! VG_(isdigit)('!') );
}
 
void test_is_dec_digit()
{
   Long x;
   CHECK( is_dec_digit('0', &x) && 0 == x );
   CHECK( is_dec_digit('1', &x) && 1 == x );
   CHECK( is_dec_digit('9', &x) && 9 == x );
}

void test_is_hex_digit()
{
   Long x;
   CHECK( is_hex_digit('0', &x) &&  0 == x );
   CHECK( is_hex_digit('1', &x) &&  1 == x );
   CHECK( is_hex_digit('9', &x) &&  9 == x );
   CHECK( is_hex_digit('a', &x) && 10 == x );
   CHECK( is_hex_digit('f', &x) && 15 == x );
   CHECK( is_hex_digit('A', &x) && 10 == x );
   CHECK( is_hex_digit('F', &x) && 15 == x );
}

void test_strtoll_and_strtod(void)
{
   // For VG_(strtoll*)()
   typedef struct {
      HChar* str;        // The string to convert.
      Long   res;        // The result.
      HChar  endptr_val; // The char one past the end of the converted text.
   } StrtollInputs;

   // VG_(strtoll10)()
   {
      StrtollInputs a[] = {
         // If there's no number at the head of the string, return 0, and
         // make 'endptr' point to the start of the string.
         { .str = "",      .res = 0, .endptr_val = '\0' },
         { .str = " \n\t", .res = 0, .endptr_val = ' '  },
         { .str = "one",   .res = 0, .endptr_val = 'o'  },
         { .str = "\ntwo", .res = 0, .endptr_val = '\n' },

         // Successful conversion.  Leading whitespace is ignored.  A single
         // '-' or '+' is accepted.
         { .str =  "0",            .res =       0, .endptr_val = '\0' },
         { .str = "+0",            .res =       0, .endptr_val = '\0' },
         { .str = "-0",            .res =       0, .endptr_val = '\0' },
         { .str =  "1",            .res =       1, .endptr_val = '\0' },
         { .str = "+1",            .res =       1, .endptr_val = '\0' },
         { .str = "-1",            .res =      -1, .endptr_val = '\0' },
         { .str = "12",            .res =      12, .endptr_val = '\0' },
         { .str = "-567",          .res =    -567, .endptr_val = '\0' },
         { .str = "1234567",       .res = 1234567, .endptr_val = '\0' },
         { .str = "007",           .res =       7, .endptr_val = '\0' },
         { .str = "   +42",        .res =      42, .endptr_val = '\0' },
         { .str = "\n\t\r\v  -56", .res =     -56, .endptr_val = '\0' },
         { .str = "123xyz",        .res =     123, .endptr_val = 'x'  },
         { .str = " -123abc",      .res =    -123, .endptr_val = 'a'  },

         // Whitespace after the +/- is not allowed;  conversion fails.
         { .str = "+ 1",   .res =  0, .endptr_val = '+' },
         { .str = "-\n1",  .res =  0, .endptr_val = '-' },
      };

      // Nb: We test the results against strtoll() as well.
      int i;
      for (i = 0; i < (sizeof(a) / sizeof(StrtollInputs)); i++) {
         HChar* endptr1;
         HChar* endptr2;
         Long      res1 = VG_(strtoll10)(a[i].str, &endptr1);
         long long res2 =     strtoll   (a[i].str, &endptr2, 10);
         //printf("res1 = %lld, *endptr1 = '%c'\n", res1, *endptr1);
         //printf("res2 = %lld, *endptr2 = '%c'\n", res2, *endptr2);
         CHECK(a[i].res == res1 && a[i].endptr_val == *endptr1);
         CHECK(res2     == res1 && *endptr2        == *endptr1);
      }
   }

   // VG_(strtoll16)()
   {
      StrtollInputs a[] = {
         // If there's no number at the head of the string, return 0, and
         // make 'endptr' point to the start of the string.
         { .str = "",      .res = 0, .endptr_val = '\0' },
         { .str = " \n\t", .res = 0, .endptr_val = ' '  },
         { .str = "one",   .res = 0, .endptr_val = 'o'  },
         { .str = "\ntwo", .res = 0, .endptr_val = '\n' },

         // Successful conversion.  Leading whitespace is ignored.  A single
         // '-' or '+' is accepted.  "0X" and "0x" are also allowed at the
         // front, but if no digits follow, just the "0" is converted.
         { .str =   "0",           .res =        0, .endptr_val = '\0' },
         { .str = "0x0",           .res =        0, .endptr_val = '\0' },
         { .str = "0X0",           .res =        0, .endptr_val = '\0' },
         { .str = "0x",            .res =        0, .endptr_val = 'x'  },
         { .str = "0Xg",           .res =        0, .endptr_val = 'X'  },
         { .str =   "0",           .res =        0, .endptr_val = '\0' },
         { .str =  "+0",           .res =        0, .endptr_val = '\0' },
         { .str =  "-0",           .res =        0, .endptr_val = '\0' },
         { .str =   "1",           .res =        1, .endptr_val = '\0' },
         { .str =  "+1",           .res =        1, .endptr_val = '\0' },
         { .str =  "-1",           .res =       -1, .endptr_val = '\0' },
         { .str =  "1a",           .res =       26, .endptr_val = '\0' },
         { .str = "-5F7",          .res =    -1527, .endptr_val = '\0' },
         { .str = "0x1234567",     .res = 19088743, .endptr_val = '\0' },
         { .str = "007",           .res =        7, .endptr_val = '\0' },
         { .str = "0X00ABCD",      .res =    43981, .endptr_val = '\0' },
         { .str = "   +AbC",       .res =     2748, .endptr_val = '\0' },
         { .str = "   -0xAbC",     .res =    -2748, .endptr_val = '\0' },
         { .str = "   -0xxx",      .res =        0, .endptr_val = 'x'  },
         { .str = "\n\t\r\v  -56", .res =      -86, .endptr_val = '\0' },
         { .str = "123xyz",        .res =      291, .endptr_val = 'x'  },
         { .str = " -123defghi",   .res = -1195503, .endptr_val = 'g'  },

         // Whitespace after the +/- is not allowed;  conversion fails.
         { .str = "+ 1",    .res =  0, .endptr_val = '+' },
         { .str = "-\n0x1", .res =  0, .endptr_val = '-' },
      };

      // Nb: We test the results against strtoll() as well.
      int i;
      for (i = 0; i < (sizeof(a) / sizeof(StrtollInputs)); i++) {
         HChar* endptr1;
         HChar* endptr2;
         Long      res1 = VG_(strtoll16)(a[i].str, &endptr1);
         long long res2 =     strtoll   (a[i].str, &endptr2, 16);
         //printf("  res1 = %lld, *endptr1 = '%c'\n", res1, *endptr1);
         //printf("  res2 = %lld, *endptr2 = '%c'\n", res2, *endptr2);
         CHECK(a[i].res == res1 && a[i].endptr_val == *endptr1);
         CHECK(res2     == res1 && *endptr2        == *endptr1);
      }
   }

   // VG_(strtod)()
   // XXX: todo
}

void test_log2(void)
{
   CHECK( -1 == VG_(log2)(0) );
   CHECK(  0 == VG_(log2)(1) );
   CHECK(  1 == VG_(log2)(2) );
   CHECK( -1 == VG_(log2)(3) );
   CHECK(  2 == VG_(log2)(4) );
   CHECK( -1 == VG_(log2)(5) );
   CHECK( -1 == VG_(log2)(6) );
   CHECK( -1 == VG_(log2)(7) );
   CHECK(  3 == VG_(log2)(8) );

   CHECK( -1 == VG_(log2)( 15) );
   CHECK(  4 == VG_(log2)( 16) );
   CHECK( -1 == VG_(log2)( 17) );

   CHECK( -1 == VG_(log2)( 63) );
   CHECK(  6 == VG_(log2)( 64) );
   CHECK( -1 == VG_(log2)( 65) );

   CHECK( -1 == VG_(log2)(255) );
   CHECK(  8 == VG_(log2)(256) );
   CHECK( -1 == VG_(log2)(257) );

   CHECK( -1 == VG_(log2)(65535) );
   CHECK( 16 == VG_(log2)(65536) );
   CHECK( -1 == VG_(log2)(65537) );

   CHECK( -1 == VG_(log2)(16777215) );
   CHECK( 24 == VG_(log2)(16777216) );
   CHECK( -1 == VG_(log2)(16777217) );

   CHECK( -1 == VG_(log2)(2147483647U) );
   CHECK( 31 == VG_(log2)(2147483648U) );
   CHECK( -1 == VG_(log2)(2147483649U) );

   CHECK( -1 == VG_(log2)(4294967295U) );    // Max UInt
}

void test_random(void)
{
   // Hmm, it's really hard to unit test a pseudo-random number generator.
   // So no testing here, sorry.
}

//-----------------------------------------------------------------------
// main
//-----------------------------------------------------------------------

int main(void)
{
#if defined(VGP_mips32_linux) || defined(VGP_mips64_linux)
   VKI_PAGE_SIZE = sysconf(_SC_PAGESIZE);
#endif
   // Nb: the order of the tests is based on the order of the code in
   // m_libcbase.c, except that macros defined in pub_tool_libcbase.h are
   // tested first.
 
   //--------------------------------------------------------------------
   // pub_tool_libcbase.h macros
   //--------------------------------------------------------------------
   test_VG_STREQ();
   test_VG_STREQN();
   test_VG_IS_XYZ_ALIGNED();
   test_VG_ROUND_et_al();

   //--------------------------------------------------------------------
   // Char functions
   //--------------------------------------------------------------------
   test_isspace();
   test_isdigit();

   //--------------------------------------------------------------------
   // String-to-number functions
   //--------------------------------------------------------------------
   test_is_dec_digit();
   test_is_hex_digit();
   test_strtoll_and_strtod();

   //--------------------------------------------------------------------
   // String functions
   //--------------------------------------------------------------------
   // XXX: more todo: VG_(str_*)

   //--------------------------------------------------------------------
   // Mem functions
   //--------------------------------------------------------------------
   // XXX: todo: VG_(mem*)

   //--------------------------------------------------------------------
   // Miscellaneous functions
   //--------------------------------------------------------------------
   // XXX: todo: VG_(ssort)
   test_log2();
   test_random();
 
   return 0;
}

