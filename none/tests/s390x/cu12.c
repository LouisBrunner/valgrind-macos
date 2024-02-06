#include <stdint.h>
#include <inttypes.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "opcodes.h"

#ifndef M3
#define M3 0
#endif

/* The abstracted result of an CU12 insn */
typedef struct {
   uint64_t addr1;  // target
   uint64_t len1;
   uint64_t addr2;  // source
   uint64_t len2;
   uint32_t cc;
} cu12_t;

/* Define various input buffers. */

/* 1-byte UTF-8 character */
uint8_t pattern1[] = {
   0x00, 0x01, 0x02, 0x03
};

/* 2-byte UTF-8 character */
uint8_t pattern2[] = {
   0xc2, 0x80,
   0xc2, 0x81,
   0xc2, 0x82,
   0xc2, 0x83,
};

/* 3-byte UTF-8 character */
uint8_t pattern3[] = {
   0xe1, 0x80, 0x80,
   0xe1, 0x80, 0x81,
   0xe1, 0x80, 0x82,
   0xe1, 0x80, 0x83,
};

/* 4-byte UTF-8 character */
uint8_t pattern4[] = {
   0xf4, 0x80, 0x80, 0x80,
   0xf4, 0x80, 0x80, 0x81,
   0xf4, 0x80, 0x80, 0x82,
   0xf4, 0x80, 0x80, 0x83,
};


/* Mixed bytes */
uint8_t mixed[] = {
   0x01,                    // 1 byte
   0xc3, 0x80,              // 2 bytes
   0x12,                    // 1 byte
   0xe1, 0x90, 0x93,        // 3 bytes
   0x23,                    // 1 byte
   0xf4, 0x80, 0x90, 0x8a,  // 4 bytes
   0x34,                    // 1 byte
   0xc4, 0x8c,              // 2 bytes
   0xe1, 0x91, 0x94,        // 3 bytes
   0xc5, 0x8a,              // 2 bytes
   0xf4, 0x80, 0x90, 0x8a,  // 4 bytes
   0xc5, 0x8a,              // 2 bytes
   0xe1, 0x91, 0x94,        // 3 bytes
   0xf4, 0x80, 0x90, 0x8a,  // 4 bytes
   0xe1, 0x91, 0x94,        // 3 bytes
};

/* This is the buffer for the converted bytes. */
uint16_t buff[1000];  /* Large so we con'don't have to worry about it */


static cu12_t
do_cu12(uint16_t *dst, uint64_t dst_len, uint8_t *src, uint64_t src_len)
{
   int cc = 42;
   cu12_t regs;

   /* build up the register pairs */
   register uint8_t  *source     asm("4") = src;
   register uint64_t  source_len asm("5") = src_len;
   register uint16_t *dest       asm("2") = dst;
   register uint64_t  dest_len   asm("3") = dst_len;

   asm volatile(
                CU12(M3,2,4)
                "ipm %2\n\t"
                "srl %2,28\n\t"
                : "+d"(dest), "+d"(source), "=d"(cc),
                  "+d"(source_len), "+d"(dest_len)
                :
                : "memory", "cc");

   /* Capture register contents at end of cu12 */
   regs.addr1 = (uint64_t)dest;
   regs.len1  = dest_len;
   regs.addr2 = (uint64_t)source;
   regs.len2  = source_len;
   regs.cc = cc;
   
   return regs;
}

void
run_test(uint16_t *dst, uint64_t dst_len, uint8_t *src, uint64_t src_len)
{
   int i;
   cu12_t result;

   printf("UTF8:  ");
   if (src_len == 0) 
      printf(" <none>");
   else {
      for(i = 0; i < src_len; ++i)
         printf(" %02x", src[i]);
   }
   printf("\n");
      
   result = do_cu12(dst, dst_len, src, src_len);

   // Write out the converted byte, if any
   printf("UTF16: ");
   if (dst_len - result.len1 == 0)
      printf(" <none>");
   else {
      uint64_t num_bytes = dst_len - result.len1;

      /* The number of bytes that were written must be divisible by 2 */
      if (num_bytes % 2 != 0)
         fprintf(stderr, "*** number of bytes is not a multiple of 2\n");

      for (i = 0; i < num_bytes / 2; i++) {
         printf(" %04x", dst[i]);
      }
   }
   printf("\n");

   printf("  cc = %d\n", result.cc);
   if (dst != NULL)
      printf("  dst address difference: %"PRId64, result.addr1 - (uint64_t)dst);
   printf("  dst len: %"PRId64"\n", result.len1);

   if (src != NULL)
      printf("  src address difference: %"PRId64, result.addr2 - (uint64_t)src);
   printf("  src len: %"PRId64"\n", result.len2);
}

// Test conversion of a one-byte character
void convert_1_byte(void)
{
   int i;

   printf("===== Conversion of a one-byte character =====\n");

   printf("\n----- Valid characters -----\n");
   uint8_t valid[] = { 
      0x00, 0x7f,              // corner cases
      0x01, 0x10, 0x7e, 0x5d   // misc 
   };
   run_test(buff, sizeof buff, valid, sizeof valid);

   // As conversion stops upon encountering an invalid character, we
   // need to test each invalid character separately, to make sure it
   // is recognized as invalid.
   
   printf("\n----- Invalid characters -----\n");
   uint8_t always_invalid[] = {
      0x80, 0xbf,              // corner cases
      0xf8, 0xff,              // corner cases
      0x81, 0xbe, 0x95, 0xab   // misc
   };
   for (i = 0; i < sizeof always_invalid; ++i) {
      uint8_t invalid_char[1];
      invalid_char[0] = always_invalid[i];
      run_test(buff, sizeof buff, invalid_char, sizeof invalid_char);
   }

   // In case of m3 == 0 we get cc=0 indicating exhaustion of source
   printf("\n----- Invalid characters if m3 == 1 -----\n");
   uint8_t invalid_if_m3[] = {  // contains all such invalid characters
      0xc0, 0xc1,
      0xf5, 0xf6, 0xf7
   };
   for (i = 0; i < sizeof invalid_if_m3; ++i) {
      uint8_t invalid_char[1];
      invalid_char[0] = invalid_if_m3[i];
      run_test(buff, sizeof buff, invalid_char, sizeof invalid_char);
   }

   printf("\n----- 1st char valid, 2nd char invalid -----\n");
   uint8_t valid_invalid[] = {
      0x10, // valid
      0xaa  // invalid
   };
   run_test(buff, sizeof buff, valid_invalid, sizeof valid_invalid);
}

// Test conversion of a two-byte character
void convert_2_bytes(void)
{
   int i;

   printf("\n===== Conversion of a two-byte character =====\n");

   printf("\n----- Valid characters -----\n");
   uint8_t valid[] = { 
      0xc2, 0x80,             // corner case
      0xc2, 0xbf,             // corner case
      0xdf, 0x80,             // corner case
      0xdf, 0xbf,             // corner case
      0xc3, 0xbe, 0xda, 0xbc  // misc 
   };
   run_test(buff, sizeof buff, valid, sizeof valid);

   printf("\n----- Valid characters if m3 == 0 -----\n");
   // First char is 0xc0 or 0xc1
   uint8_t valid_if_not_m3[] = {
      0xc0, 0x80,
      0xc0, 0xbf,
      0xc1, 0x80,
      0xc0, 0xbf
   };
   run_test(buff, sizeof buff, valid_if_not_m3, sizeof valid_if_not_m3);

   // Test for invalid two-byte characters where the 1st byte is valid
   // The 2nd byte is invalid if not in range 0x80..0xbf, inclusive

   // As conversion stops upon encountering an invalid character, we
   // need to test each invalid character separately, to make sure it
   // is recognized as invalid.
   
   printf("\n----- Invalid characters if m3 == 1 -----\n");
   uint8_t always_invalid[] = {
      0xc2, 0x00,
      0xc2, 0x7f,
      0xc2, 0xc0,
      0xc2, 0xff
   };
   for (i = 0; i < sizeof always_invalid; i += 2) {
      uint8_t invalid_char[2];
      invalid_char[0] = always_invalid[i];
      invalid_char[1] = always_invalid[i+1];
      run_test(buff, sizeof buff, invalid_char, sizeof invalid_char);
   }

   /* Nb: for a two-byte character we need not test the case where 
      invalidity of the character (cc=2) takes precedence over exhaustion
      of the 1st operand (cc=1). Invalidity of the character has already
      been tested when testing the 1st byte. */

   printf("\n----- 1st char valid, 2nd char invalid -----\n");
   uint8_t valid_invalid[] = {
      0xc3, 0x81, // valid
      0xc4, 0x00  // invalid
   };
   run_test(buff, sizeof buff, valid_invalid, sizeof valid_invalid);
}

// Test conversion of a three-byte character
void
convert_3_bytes(void)
{
   int i;

   printf("\n===== Conversion of a three-byte character =====\n");

   /* Exhaustively test the 1st byte E0 - EF, and the interval boundaries for
      the 2nd and 3rd bytes */
   printf("\n----- Valid characters -----\n");
   uint8_t e0[] = { 
      0xe0, 0xa0, 0x80,
      0xe0, 0xbf, 0x80,
      0xe0, 0xa0, 0xbf,
      0xe0, 0xbf, 0xbf,
      0xe0, 0xaa, 0xbb,   // random  e0 .. ..
   };
   run_test(buff, sizeof buff, e0, sizeof e0);

   uint8_t ed[] = { 
      0xed, 0x80, 0x80,
      0xed, 0x9f, 0x80,
      0xed, 0x80, 0xbf,
      0xed, 0x9f, 0xbf,
      0xed, 0x8a, 0xbb,   // random  ed .. ..
   };
   run_test(buff, sizeof buff, ed, sizeof ed);

   for (i = 0; i <= 0xf; ++i) {
      uint8_t exxx_1[3] = { 0x0, 0x80, 0x80 };
      uint8_t exxx_2[3] = { 0x0, 0xbf, 0x80 };
      uint8_t exxx_3[3] = { 0x0, 0x80, 0xbf };
      uint8_t exxx_4[3] = { 0x0, 0xbf, 0xbf };

      if (i == 0x00) continue;   // special case e0
      if (i == 0x0d) continue;   // special case ed

      exxx_1[0] = 0xe0 | i;
      exxx_2[0] = 0xe0 | i;
      exxx_3[0] = 0xe0 | i;
      exxx_4[0] = 0xe0 | i;
      run_test(buff, sizeof buff, exxx_1, sizeof exxx_1);
      run_test(buff, sizeof buff, exxx_2, sizeof exxx_2);
      run_test(buff, sizeof buff, exxx_3, sizeof exxx_3);
      run_test(buff, sizeof buff, exxx_4, sizeof exxx_4);
   };

   printf("\n----- Invalid characters (2nd byte is invalid) -----\n");
   // Test for invalid three-byte characters where the 1st byte is valid
   // The 2nd byte is invalid.

   // As conversion stops upon encountering an invalid character, we
   // need to test each invalid character separately, to make sure it
   // is recognized as invalid.

   e0[0] = 0xe0;  // valid
   e0[1] = 0x9f;  // invalid  because outside [0xa0 .. 0xbf]
   e0[2] = 0x80;  // valid
   run_test(buff, sizeof buff, e0, sizeof e0);
   e0[1] = 0xc0;  // invalid  because outside [0xa0 .. 0xbf]
   run_test(buff, sizeof buff, e0, sizeof e0);

   ed[0] = 0xed;  // valid
   ed[1] = 0x7f;  // invalid  because outside [0x80 .. 0x9f]
   ed[2] = 0x80;  // valid
   run_test(buff, sizeof buff, ed, sizeof ed);
   ed[1] = 0xa0;  // invalid  because outside [0x80 .. 0x9f]
   run_test(buff, sizeof buff, ed, sizeof ed);

   for (i = 0; i <= 0xf; ++i) {
      uint8_t exxx_1[3] = { 0x0, 0x7f, 0x80 };
      uint8_t exxx_2[3] = { 0x0, 0xc0, 0x80 };

      if (i == 0x00) continue;   // special case e0
      if (i == 0x0d) continue;   // special case ed

      exxx_1[0] = 0xe0 | i;
      exxx_2[0] = 0xe0 | i;
      run_test(buff, sizeof buff, exxx_1, sizeof exxx_1);
      run_test(buff, sizeof buff, exxx_2, sizeof exxx_2);
   };

   printf("\n----- Invalid characters (3rd byte is invalid) -----\n");
   // For all 1st bytes 0xe0 .. 0xef the 3rd bytes must be in [0x80 .. 0xbf]
   // No need to special case 0xe0 and 0xed
   for (i = 0; i <= 0xf; ++i) {
      uint8_t exxx_1[3] = { 0x0, 0xab, 0x7f };
      uint8_t exxx_2[3] = { 0x0, 0xab, 0xc0 };

      exxx_1[0] = 0xe0 | i;
      exxx_2[0] = 0xe0 | i;
      run_test(buff, sizeof buff, exxx_1, sizeof exxx_1);
      run_test(buff, sizeof buff, exxx_2, sizeof exxx_2);
   };

   printf("\n----- Invalid 2nd char AND output exhausted -----\n");
   /* The character is invalid in its 2nd byte AND the output buffer is 
      exhausted (2 bytes are needed) */
   uint8_t pat1[] = {
      0xe0, 0x00, 0x80
   };
   run_test(buff, 1, pat1, 3);

   printf("\n----- Invalid 3rd char AND output exhausted -----\n");
   /* The character is invalid in its 3rd byte AND the output buffer is 
      exhausted (2 bytes are needed) */
   uint8_t pat2[] = {
      0xe4, 0x84, 0x00
   };
   run_test(buff, 1, pat2, 3);

   printf("\n----- 1st char valid, 2nd char invalid -----\n");
   uint8_t valid_invalid[] = {
      0xe1, 0x90, 0x90, // valid
      0xe1, 0x00, 0x90  // invalid
   };
   run_test(buff, sizeof buff, valid_invalid, sizeof valid_invalid);
}

// Test conversion of a four-byte character
void
convert_4_bytes(void)
{
   int i, j;

   printf("\n===== Conversion of a four-byte character =====\n");

   printf("\n----- Valid characters -----\n");
   for (i = 0; i <= 4; ++i) {
      uint8_t valid[4];

      valid[0] = 0xf0 | i;

      for (j = 0; j <= 1; ++j) {
         // Byte 2
         if (i == 0) {
            valid[1] = j == 0 ? 0x90 : 0xbf;    // 0xf0
         } else if (i == 4) {
            valid[1] = j == 0 ? 0x80 : 0x8f;    // 0xf4
         } else {
            valid[1] = j == 0 ? 0x80 : 0xbf;    // 0xf1 .. 0xf3
         }
         // Byte 3 and byte 4 have same interval 0x80 .. 0xbf
         valid[2] = 0x80;
         valid[3] = 0x80;
         run_test(buff, sizeof buff, valid, sizeof valid);
         valid[2] = 0x80;
         valid[3] = 0xbf;
         run_test(buff, sizeof buff, valid, sizeof valid);
         valid[2] = 0xbf;
         valid[3] = 0x80;
         run_test(buff, sizeof buff, valid, sizeof valid);
         valid[2] = 0xbf;
         valid[3] = 0xbf;
         run_test(buff, sizeof buff, valid, sizeof valid);
      }
   }

   printf("\n----- Valid characters if m3 == 0 -----\n");
   // First char is 0xf5 .. 0xf7
   uint8_t valid_if_not_m3[] = {
      0xf5, 0x00, 0x00, 0x00,
      0xf6, 0x11, 0x22, 0x33,
      0xf7, 0x44, 0x55, 0x66,
   };
   run_test(buff, sizeof buff, valid_if_not_m3, sizeof valid_if_not_m3);

   // As conversion stops upon encountering an invalid character, we
   // need to test each invalid character separately, to make sure it
   // is recognized as invalid.

   printf("\n----- Invalid characters (2nd byte is invalid) -----\n");
   // Test for invalid four-byte characters where the 2nd byte is invalid.
   // All other bytes are valid
   uint8_t f0[4], f4[4];

   f0[0] = 0xf0;  // valid
   f0[1] = 0x8f;  // invalid  because outside [0x90 .. 0xbf]
   f0[2] = 0x80;  // valid
   f0[3] = 0x80;  // valid
   run_test(buff, sizeof buff, f0, sizeof f0);
   f0[1] = 0xc0;  // invalid  because outside [0x90 .. 0xbf]
   run_test(buff, sizeof buff, f0, sizeof f0);

   f4[0] = 0xf4;  // valid
   f4[1] = 0x7f;  // invalid  because outside [0x80 .. 0x8f]
   f4[2] = 0x80;  // valid
   f4[3] = 0x80;  // valid
   run_test(buff, sizeof buff, f4, sizeof f4);
   f4[1] = 0x90;  // invalid  because outside [0x80 .. 0x9f]
   run_test(buff, sizeof buff, f4, sizeof f4);

   for (i = 0; i <= 0x4; ++i) {
      uint8_t fxxx_1[4] = { 0x0, 0x7f, 0x80, 0x80 };
      uint8_t fxxx_2[4] = { 0x0, 0xc0, 0x80, 0x80 };

      if (i == 0) continue;   // special case f0
      if (i == 4) continue;   // special case f4

      fxxx_1[0] = 0xf0 | i;
      fxxx_2[0] = 0xf0 | i;
      run_test(buff, sizeof buff, fxxx_1, sizeof fxxx_1);
      run_test(buff, sizeof buff, fxxx_2, sizeof fxxx_2);
   };

   printf("\n----- Invalid characters (3rd byte is invalid) -----\n");
   // Test for invalid four-byte characters where the 3rd byte is invalid.
   // All other bytes are valid
   for (i = 0; i <= 0x4; ++i) {
      uint8_t fxxx[4] = { 0x0, 0x0, 0x0, 0x80 };

      fxxx[0] = 0xf0 | i;
      fxxx[1] = (i == 0) ? 0x94 : 0x84;
      fxxx[2] = 0x7f;
      run_test(buff, sizeof buff, fxxx, sizeof fxxx);
      fxxx[2] = 0xc0;
      run_test(buff, sizeof buff, fxxx, sizeof fxxx);
   };

   printf("\n----- Invalid characters (4th byte is invalid) -----\n");
   // Test for invalid four-byte characters where the 3rd byte is invalid.
   // All other bytes are valid
   for (i = 0; i <= 0x4; ++i) {
      uint8_t fxxx[4] = { 0x0, 0x0, 0x80, 0x0 };

      fxxx[0] = 0xf0 | i;
      fxxx[1] = (i == 0) ? 0x94 : 0x84;
      fxxx[3] = 0x7f;
      run_test(buff, sizeof buff, fxxx, sizeof fxxx);
      fxxx[3] = 0xc0;
      run_test(buff, sizeof buff, fxxx, sizeof fxxx);
   };

   printf("\n----- Invalid 2nd char AND output exhausted -----\n");
   /* The character is invalid in its 2nd byte AND the output buffer is 
      exhausted (4 bytes are needed) */
   uint8_t pat1[] = {
      0xf0, 0x00, 0x80, 0x80
   };
   run_test(buff, 1, pat1, 4);

   printf("\n----- Invalid 3rd char AND output exhausted -----\n");
   /* The character is invalid in its 3rd byte AND the output buffer is 
      exhausted (4 bytes are needed) */
   uint8_t pat2[] = {
      0xf0, 0xaa, 0x00, 0x80
   };
   run_test(buff, 3, pat2, 4);

   printf("\n----- Invalid 4th char AND output exhausted -----\n");
   /* The character is invalid in its 4th byte AND the output buffer is 
      exhausted (4 bytes are needed) */
   uint8_t pat3[] = {
      0xf0, 0xaa, 0xaa, 0x00
   };
   run_test(buff, 3, pat3, 4);

   printf("\n----- 1st char valid, 2nd char invalid -----\n");
   uint8_t valid_invalid[] = {
      0xf0, 0xaa, 0xaa, 0xaa, // valid
      0xf0, 0x00, 0x00, 0x00  // invalid
   };
   run_test(buff, sizeof buff, valid_invalid, sizeof valid_invalid);
}


int main()
{
   convert_1_byte();
   convert_2_bytes();
   convert_3_bytes();
   convert_4_bytes();

   /* Length == 0, no memory should be read or written */
   printf("\n------------- test1 ----------------\n");
   run_test(NULL, 0, NULL, 0);

   /* Test exhaustion of source length (source bytes are valid) */
   printf("\n------------- test2.1 ----------------\n");

   /* No character will be written to BUFF, i.e. loop in jitted code
      is not iterated */
   run_test(buff, sizeof buff, NULL,     0);
   run_test(buff, sizeof buff, pattern1, 0);
   run_test(buff, sizeof buff, pattern2, 0);
   run_test(buff, sizeof buff, pattern2, 1);
   run_test(buff, sizeof buff, pattern3, 0);
   run_test(buff, sizeof buff, pattern3, 1);
   run_test(buff, sizeof buff, pattern3, 2);
   run_test(buff, sizeof buff, pattern4, 0);
   run_test(buff, sizeof buff, pattern4, 1);
   run_test(buff, sizeof buff, pattern4, 2);
   run_test(buff, sizeof buff, pattern4, 3);

   printf("\n------------- test2.2 ----------------\n");
   /* At least one character will be written to BUFF, i.e. loop in jitted
      code is iterated */
   run_test(buff, sizeof buff, pattern1, 2);
   run_test(buff, sizeof buff, pattern2, 5);
   run_test(buff, sizeof buff, pattern3, 6);
   run_test(buff, sizeof buff, pattern4, 9);

   /* Test exhaustion of destination length (source bytes are valid) */
   printf("\n------------- test3.1 ----------------\n");

   /* No character will be written to BUFF, i.e. loop in jitted code
      is not iterated */

   /* Want to write 2 or 4 bytes at a time */
   run_test(NULL, 0, pattern1, sizeof pattern1);  // 2-byte result
   run_test(NULL, 0, pattern2, sizeof pattern2);  // 2-byte result
   run_test(NULL, 1, pattern2, sizeof pattern2);  // 2-byte result
   run_test(NULL, 0, pattern3, sizeof pattern3);  // 2-byte result
   run_test(NULL, 1, pattern3, sizeof pattern3);  // 2-byte result
   run_test(NULL, 0, pattern4, sizeof pattern4);  // 4-byte result
   run_test(NULL, 1, pattern4, sizeof pattern4);  // 4-byte result
   run_test(NULL, 2, pattern4, sizeof pattern4);  // 4-byte result
   run_test(NULL, 3, pattern4, sizeof pattern4);  // 4-byte result

   printf("\n------------- test3.2 ----------------\n");
   /* At least one character will be written to BUFF, i.e. loop in jitted
      code is iterated */
   run_test(buff, 4, pattern1, sizeof pattern1);
   run_test(buff, 5, pattern1, sizeof pattern1);
   run_test(buff, 6, pattern1, sizeof pattern1);
   run_test(buff, 7, pattern1, sizeof pattern1);

   /* Convert buffer with mixed characters */
   printf("\n------------- test4 ----------------\n");
   run_test(buff, sizeof buff, mixed, sizeof mixed);

   return 0;
}
