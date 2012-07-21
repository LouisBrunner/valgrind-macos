#include <stdint.h>
#include <inttypes.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "opcodes.h"

#ifndef M3
#define M3 0
#endif

/* The abstracted result of an CU24 insn */
typedef struct {
   uint64_t addr1;  // target
   uint64_t len1;
   uint64_t addr2;  // source
   uint64_t len2;
   uint32_t cc;
} cu24_t;

/* Define various input buffers. */

/* Single UTF-16 value */
uint16_t pattern1[] = {
   0x0000, 0xd7ff,    /* [0000 ... d7ff]  corner cases */
   0xdc00, 0xffff,    /* [dc00 ... ffff]  corner cases */
   0x0047, 0x0156, 0x1245, 0xa021, 0xfffe /* misc */
};

/* UTF-16 surrogate pair */
uint16_t pattern2[] = {
   0xd800, 0xdc00,    /* left  corner case */
   0xdbff, 0xdfff,    /* right corner case */
   0xdada, 0xdddd, 0xdeaf, 0xdcdc  /* misc */
};

/* Invalid low surrogate */
uint16_t invalid[] = { 0xd801, 0x0098 };

/* Mixed bytes */
uint16_t mixed[] = {
   0x0078,
   0x0200,
   0xffff,
   0xd800, 0xdc01,
   0xde00, 0xdd00,
   0xc0c0
};

/* This is the buffer for the converted bytes. */
uint32_t buff[1000];  /* Large so we con'don't have to worry about it */


static cu24_t
do_cu24(uint32_t *dst, uint64_t dst_len, uint16_t *src, uint64_t src_len)
{
   int cc = 42;
   cu24_t regs;

   /* build up the register pairs */
   register uint16_t *source     asm("4") = src;
   register uint64_t  source_len asm("5") = src_len;
   register uint32_t *dest       asm("2") = dst;
   register uint64_t  dest_len   asm("3") = dst_len;

   asm volatile(
                CU24(M3,2,4)
                "ipm %2\n\t"
                "srl %2,28\n\t"
                : "+d"(dest), "+d"(source), "=d"(cc),
                  "+d"(source_len), "+d"(dest_len)
                :
                : "memory", "cc");

   /* Capture register contents at end of cu24 */
   regs.addr1 = (uint64_t)dest;
   regs.len1  = dest_len;
   regs.addr2 = (uint64_t)source;
   regs.len2  = source_len;
   regs.cc = cc;
   
   return regs;
}

void
run_test(uint32_t *dst, uint64_t dst_len, uint16_t *src, uint64_t src_len)
{
   int i;
   cu24_t result;

   result = do_cu24(dst, dst_len, src, src_len);

   // Write out the converted byte, if any
   printf("UTF32: ");
   if (dst_len - result.len1 == 0)
      printf(" <none>");
   else {
      uint64_t num_bytes = dst_len - result.len1;

      /* The number of bytes that were written must be divisible by 4 */
      if (num_bytes % 4 != 0)
         fprintf(stderr, "*** number of bytes is not a multiple of 4\n");

      for (i = 0; i < num_bytes / 4; i++) {
         printf(" %02x", dst[i]);
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

int main()
{
   /* Length == 0, no memory should be read or written */
   printf("\n------------- test1 ----------------\n");
   run_test(NULL, 0, NULL, 0);

   /* Test exhaustion of source length (source bytes are valid) */
   printf("\n------------- test2.1 ----------------\n");

   /* No character will be written to BUFF, i.e. loop in jitted code
      is not iterated */
   run_test(buff, sizeof buff, NULL,     1);
   run_test(buff, sizeof buff, pattern1, 1);
   run_test(buff, sizeof buff, pattern2, 1);
   run_test(buff, sizeof buff, pattern2, 2);
   run_test(buff, sizeof buff, pattern2, 3);

   printf("\n------------- test2.2 ----------------\n");
   /* At least one character will be written to BUFF, i.e. loop in jitted
      code is iterated */
   run_test(buff, sizeof buff, pattern1, 3);
   run_test(buff, sizeof buff, pattern1, 5);
   run_test(buff, sizeof buff, pattern2, 2);
   run_test(buff, sizeof buff, pattern2, 5);
   run_test(buff, sizeof buff, pattern2, 7);

   /* Test exhaustion of destination length (source bytes are valid) */
   printf("\n------------- test3.1 ----------------\n");

   /* No character will be written to BUFF, i.e. loop in jitted code
      is not iterated */

   /* Want to write 4 bytes at a time */
   run_test(NULL, 0, pattern1, sizeof pattern1);
   run_test(NULL, 1, pattern1, sizeof pattern1);
   run_test(NULL, 2, pattern1, sizeof pattern1);
   run_test(NULL, 3, pattern1, sizeof pattern1);

   printf("\n------------- test3.2 ----------------\n");
   /* At least one character will be written to BUFF, i.e. loop in jitted
      code is iterated */
   run_test(buff, 4, pattern1, sizeof pattern1);
   run_test(buff, 5, pattern1, sizeof pattern1);
   run_test(buff, 6, pattern1, sizeof pattern1);
   run_test(buff, 7, pattern1, sizeof pattern1);

   /* When both operands are exhausted, cc=0 takes precedence.
      (test1 tests this for len == 0) */
   printf("\n------------- test4 ----------------\n");
   run_test(buff, 4, pattern1, 2);   // no iteration
   run_test(buff, 8, pattern1, 4);   // iteration

   /* Input has invalid low surrogate. */
   printf("\n------------- test5 ----------------\n");
   run_test(buff, sizeof buff, invalid, sizeof invalid);
   run_test(buff, 0, invalid, sizeof invalid);

   /* Convert all pattern buffers */
   printf("\n------------- test6 ----------------\n");
   run_test(buff, sizeof buff, pattern1, sizeof pattern1);
   run_test(buff, sizeof buff, pattern2, sizeof pattern2);

   return 0;
}
