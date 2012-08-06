#include <stdint.h>
#include <inttypes.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <assert.h>
#include "opcodes.h"

/* The abstracted result of an CU41 insn */
typedef struct {
   uint64_t addr1;  // target
   uint64_t len1;
   uint64_t addr2;  // source
   uint64_t len2;
   uint32_t cc;
} cu41_t;

/* Define various input buffers. */

/* 0000 to 00ff:  Result is 1 byte for each uint32_t */
uint32_t pattern1[] = {
   0x0000, 0x007f,    /* corner cases */
   0x0001, 0x007e, 0x0030, 0x005e /* misc */
};

/* 0080 to 07ff: Result is 2 bytes for each uint32_t */
uint32_t pattern2[] = {
   0x0080, 0x07ff,    /* corner cases */
   0x0081, 0x07fe, 0x100, 0x333, 0x555, 0x6aa  /* misc */
};

/* 0800 to d7ff: Result is 3 bytes for each uint32_t */
/* dc00 to ffff: Result is 3 bytes for each uint32_t */
uint32_t pattern3[] = {
   0x0800, 0xd7ff,    /* corner cases */
   0xdc00, 0xffff,    /* corner cases */
   0xdc01, 0xfffe, 0xdea0, 0xd00d, 0xe555  /* misc */
};

/* 10000 to 10ffff: Result is 4 bytes for each uint32_t */
uint32_t pattern4[] = {
   0x10000, 0x10ffff,    /* corner cases */
   0x10001, 0x10fffe, 0x12345, 0x23456, 0xfedcb  /* misc */
};

/* Invalid UTF-32 character */
uint32_t invalid[] = {
   0x0000d800, 0x0000dbff,   /* corner cases */
   0x00110000, 0xffffffff,   /* corner cases */
   0x0000daad, 0x0000d901, 0x0000d8ff, /* misc */
   0x00110011, 0x01000000, 0x10000000, 0xdeadbeef  /* misc */
};

/* Mixed bytes */
uint32_t mixed[] = {
   0x00000078 /* 1 byte  */,
   0x00000111 /* 2 bytes */,
   0x00001234 /* 3 bytes */,
   0x00040404 /* 4 bytes */,
};

/* This is the buffer for the converted bytes. */
uint8_t buff[1000];  /* Large so we con'don't have to worry about it */

void write_and_check(uint32_t *, unsigned, unsigned);


static cu41_t
do_cu41(uint8_t *dst, uint64_t dst_len, uint32_t *src, uint64_t src_len)
{
   int cc = 42;
   cu41_t regs;

   /* build up the register pairs */
   register uint32_t *source     asm("4") = src;
   register uint64_t  source_len asm("5") = src_len;
   register uint8_t  *dest       asm("2") = dst;
   register uint64_t  dest_len   asm("3") = dst_len;

   asm volatile(
                CU41(2,4)
                "ipm %2\n\t"
                "srl %2,28\n\t"
                : "+d"(dest), "+d"(source), "=d"(cc),
                  "+d"(source_len), "+d"(dest_len)
                :
                : "memory", "cc");

   /* Capture register contents at end of cu41 */
   regs.addr1 = (uint64_t)dest;
   regs.len1  = dest_len;
   regs.addr2 = (uint64_t)source;
   regs.len2  = source_len;
   regs.cc = cc;
   
   return regs;
}

void
run_test(uint8_t *dst, uint64_t dst_len, uint32_t *src, uint64_t src_len)
{
   int i;
   cu41_t result;

   result = do_cu41(dst, dst_len, src, src_len);

   // Write out the converted values, if any
   printf("UTF8: ");
   if (dst_len - result.len1 == 0)
      printf(" <none>");
   else
      for (i = 0; i < dst_len - result.len1; ++i) {
         printf(" %02x", dst[i]);
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

int main()
{
   int i;

   /* Length == 0, no memory should be read or written */
   printf("\n------------- test1 ----------------\n");
   run_test(NULL, 0, NULL, 0);

   /* Test exhaustion of source length (source bytes are valid) */
   printf("\n------------- test2.1 ----------------\n");

   /* No character will be written to BUFF, i.e. loop in jitted code
      is not iterated */
   run_test(buff, sizeof buff, NULL,     0);
   run_test(buff, sizeof buff, NULL,     1);
   run_test(buff, sizeof buff, NULL,     2);
   run_test(buff, sizeof buff, NULL,     3);
   run_test(buff, sizeof buff, pattern1, 0);
   run_test(buff, sizeof buff, pattern1, 1);
   run_test(buff, sizeof buff, pattern1, 2);
   run_test(buff, sizeof buff, pattern1, 3);

   printf("\n------------- test2.2 ----------------\n");
   /* At least one character will be written to BUFF, i.e. loop in jitted
      code is iterated */
   run_test(buff, sizeof buff, pattern1, 4);  /* 1 utf32 -> 1 1-byte utf8 */
   run_test(buff, sizeof buff, pattern2, 10); /* 2 utf32 -> 2 2-byte utf8 */
   run_test(buff, sizeof buff, pattern3, 5);  /* 1 utf32 -> 1 3-byte utf8 */
   run_test(buff, sizeof buff, pattern4, 21); /* 5 utf32 -> 5 4-byte utf8 */

   /* Test exhaustion of destination length (source bytes are valid) */
   printf("\n------------- test3.1 ----------------\n");

   /* No character will be written to BUFF, i.e. loop in jitted code
      is not iterated */

   /* Want to write at least 1 byte */
   run_test(NULL, 0, pattern1, sizeof pattern1);

   /* Want to write at least 2 bytes */
   run_test(NULL, 0, pattern2, sizeof pattern2);
   run_test(NULL, 1, pattern2, sizeof pattern2);

   /* Want to write at least 3 bytes */
   run_test(NULL, 0, pattern3, sizeof pattern3);
   run_test(NULL, 1, pattern3, sizeof pattern3);

   /* Want to write at least 4 bytes */
   run_test(NULL, 0, pattern4, sizeof pattern4);
   run_test(NULL, 1, pattern4, sizeof pattern4);
   run_test(NULL, 2, pattern4, sizeof pattern4);
   run_test(NULL, 3, pattern4, sizeof pattern4);

   /* When both operands are exhausted, cc=0 takes precedence.
      (test1 tests this for len == 0) */
   printf("\n------------- test4 ----------------\n");
   run_test(buff, 2, pattern1, 8);

   /* Input contains invalid characters */

   // As conversion stops upon encountering an invalid character, we
   // need to test each invalid character separately, to make sure it
   // is recognized as invalid.

   printf("\n------------- test5 ----------------\n");
   for (i = 0; i < sizeof invalid / 4; ++i) {
      run_test(buff, sizeof buff, invalid + i, 4);
   }
   run_test(buff, 0, invalid, sizeof invalid);  // cc = 2
   run_test(buff, 100, invalid, sizeof invalid);

   /* Convert all pattern buffers */
   printf("\n------------- test6 ----------------\n");
   run_test(buff, sizeof buff, pattern1, sizeof pattern1);
   run_test(buff, sizeof buff, pattern2, sizeof pattern2);
   run_test(buff, sizeof buff, pattern3, sizeof pattern3);
   run_test(buff, sizeof buff, pattern4, sizeof pattern4);
   run_test(buff, sizeof buff, mixed,    sizeof mixed);

   /* Make sure we only write the exact number of bytes (and not more) */

   /* Write 1 byte */
   printf("\n------------- test7.0 ----------------\n");
   write_and_check(pattern1 + 2, 4, 1);

   /* Write 2 bytes */
   printf("\n------------- test7.1 ----------------\n");
   write_and_check(pattern2 + 3, 4, 2);

   /* Write 3 bytes */
   printf("\n------------- test7.2 ----------------\n");
   write_and_check(pattern3 + 6, 4, 3);

   /* Write 4 bytes */
   printf("\n------------- test7.3 ----------------\n");
   write_and_check(pattern4 + 5, 4, 4);

   return 0;
}


void
write_and_check_aux(uint32_t *input, unsigned num_input_bytes,
                    unsigned num_expected_output_bytes,
                    unsigned fill_byte)
{
   int num_errors, i;

   /* Fill output buffer with FILL_BYTE */
   memset(buff, fill_byte, sizeof buff);

   /* Execute cu41 */
   run_test(buff, sizeof buff, input, num_input_bytes);

   /* Make sure the rest of the buffer is unmodified.  */
   num_errors = 0;
   for (i = num_expected_output_bytes; i < sizeof buff; ++i)
      if (((unsigned char *)buff)[i] != fill_byte) ++num_errors;
   if (num_errors)
      fprintf(stderr, "*** wrote more than %d bytes\n",
              num_expected_output_bytes);
}

void
write_and_check(uint32_t *input, unsigned num_input_bytes,
                unsigned num_expected_output_bytes)
{
   write_and_check_aux(input, num_input_bytes, num_expected_output_bytes, 0x0);

   /* Run again with different fill pattern to make sure we did not write
      an extra 0x0 byte */
   write_and_check_aux(input, num_input_bytes, num_expected_output_bytes, 0xFF);
}
