#include <stdint.h>
#include <inttypes.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "../../../none/tests/s390x/opcodes.h"

/* Define various input buffers. */

/* U+0000 to U+d7ff:  Result is 2 bytes for each uint32_t
   U+dc00 to U+ffff:  Result is 2 bytes for each uint32_t */
uint32_t pattern2[] = {
   0x0000, 0xd7ff,    /* corner cases */
   0xdc00, 0xffff,    /* corner cases */
   0xabba, 0xf00d, 0xd00f, 0x1234 /* misc */
};

/* U+00010000 to U+0010ffff:  Result is 4 bytes for each uint32_t */
uint32_t pattern4[] = {
   0x00010000, 0x0010ffff,    /* corner cases */
   0x00010123, 0x00023456, 0x000789ab, 0x00100000  /* misc */
};

static void
do_cu42(uint16_t *dst, uint64_t dst_len, uint32_t *src, uint64_t src_len)
{
   /* build up the register pairs */
   register uint32_t *source     asm("4") = src;
   register uint64_t  source_len asm("5") = src_len;
   register uint16_t *dest       asm("2") = dst;
   register uint64_t  dest_len   asm("3") = dst_len;

   asm volatile(
                CU42(2,4)
                : "+d"(dest), "+d"(source), "+d"(source_len), "+d"(dest_len)
                :
                : "memory", "cc");
}

int main()
{
   /*------------------------------------------------------------*/
   /* Write to a too small buffer                                */
   /*------------------------------------------------------------*/

   /* Write 2 bytes into buffer of length 1 */
   do_cu42(malloc(1), 10, pattern2, 4);             // complaint (2 bytes)

   /* Write 2 bytes into buffer of length 2 */
   do_cu42(malloc(2), 10, pattern2, 4);             // no complaint

   /* Write 4 bytes into buffer of length 1 */
   do_cu42(malloc(1), 10, pattern4, 4);             // complaint (4 bytes)

   /* Write 4 bytes into buffer of length 2 */
   do_cu42(malloc(2), 10, pattern4, 4);             // complaint (4 bytes)

   /* Write 4 bytes into buffer of length 3 */
   do_cu42(malloc(3), 10, pattern4, 4);             // complaint (4 bytes)

   /* Write 4 bytes into buffer of length 4 */
   do_cu42(malloc(4), 10, pattern4, 4);             // no complaint

   /*------------------------------------------------------------*/
   /* Read uninitialised data                                    */
   /*------------------------------------------------------------*/
   uint16_t buf[100];
   uint8_t *input;

   /* Input buffer is completely uninitialised */
   input = malloc(10);
   do_cu42(buf, sizeof buf, (void *)input, 4);         // complaint
   
   /* Read 4 bytes from input buffer. First byte is uninitialised */
   input = malloc(10);
   input[1] = input[2] = input[3] = 0x0;
   do_cu42(buf, sizeof buf, (void *)input, 4);          // complaint

   /* Read 4 bytes from input buffer. Second byte is uninitialised */
   input = malloc(10);
   input[0] = input[2] = input[3] = 0x0;
   do_cu42(buf, sizeof buf, (void *)input, 4);          // complaint
   
   /* Read 4 bytes from input buffer. Third byte is uninitialised */
   input = malloc(10);
   input[0] = input[1] = input[3] = 0x0;
   do_cu42(buf, sizeof buf, (void *)input, 4);          // complaint
   
   /* Read 4 bytes from input buffer. Fourth byte is uninitialised */
   input = malloc(10);
   input[0] = input[1] = input[2] = 0x0;
   do_cu42(buf, sizeof buf, (void *)input, 4);          // complaint
   
   /* Read 4 bytes from input buffer. All bytes are initialised */
   input = malloc(10);
   memset(input, 0, 4);
   do_cu42(buf, sizeof buf, (void *)input, 4);          // no complaint

   /* Read 8 bytes from input buffer. This iterates once. In the 1st
      iteration all input bytes are initialised in the 2nd iteration all
      input bytes are uninitialised. */
   input = malloc(10);
   memset(input, 0, 4);
   do_cu42(buf, sizeof buf, (void *)input, 8);          // complaint
   
   
   /* Write to NULL */
   //   do_cu42(NULL, 10, pattern1, sizeof pattern1);    // complaint

   return 0;
}
