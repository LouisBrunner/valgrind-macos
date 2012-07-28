#include <stdint.h>
#include <inttypes.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "../../../none/tests/s390x/opcodes.h"

/* Define various input buffers. */

/* U+0000 to U+007f:  Result is 1 byte for each uint16_t */
uint16_t pattern1[] = {
   0x0000, 0x007f,    /* corner cases */
   0x0047, 0x0056, 0x0045, 0x0021, 0x007b, 0x003a /* misc */
};

/* U+0080 to U+07ff:  Result is 2 bytes for each uint16_t */
uint16_t pattern2[] = {
   0x0080, 0x07ff,    /* corner cases */
   0x07df, 0x008f, 0x0100, 0x017f, 0x052f, 0x0600, 0x06ff /* misc */
};

/* U+0800 to U+d7ff:  Result is 3 bytes for each uint16_t
   U+dc00 to U+ffff:  Result is 3 bytes for each uint16_t */
uint16_t pattern3[] = {
   0x0800, 0xd7ff,    /* corner cases */
   0xdc00, 0xffff,    /* corner cases */
   0x083f, 0x1a21, 0x1b10, 0x2200, 0x225e, 0x22c9, 0xe001  /* misc */
};

/* U+d800 to U+dbff:  Result is 4 bytes for each uint16_t pair */
uint16_t pattern4[] = {
   0xd800, 0xdc00,    /* left  corner case */
   0xdbff, 0xdfff,    /* right corner case */
   0xdada, 0xdddd, 0xdeaf, 0xdcdc  /* misc */
};


void
do_cu21(uint8_t *dst, uint64_t dst_len, uint16_t *src, uint64_t src_len)
{
   /* build up the register pairs */
   register uint16_t *source     asm("4") = src;
   register uint64_t  source_len asm("5") = src_len;
   register uint8_t  *dest       asm("2") = dst;
   register uint64_t  dest_len   asm("3") = dst_len;

   asm volatile(
                CU21(0,2,4)
                : "+d"(dest), "+d"(source), "+d"(source_len), "+d"(dest_len)
                :
                : "memory", "cc");
   return;
}

int main()
{
   /*------------------------------------------------------------*/
   /* Write to a too small buffer                                */
   /*------------------------------------------------------------*/

   /* Write 2 bytes into buffer of length 1 */
   do_cu21(malloc(1), 10, pattern2, 2);             // complaint (2 bytes)

   /* Write 2 bytes into buffer of length 2 */
   do_cu21(malloc(2), 10, pattern2, 2);             // no complaint

   /* Write 3 bytes into buffer of length 1 */
   do_cu21(malloc(1), 10, pattern3, 2);             // 2 complaints (3 = 2+1)

   /* Write 3 bytes into buffer of length 2 */
   do_cu21(malloc(2), 10, pattern3, 2);             // complaint (1 byte)

   /* Write 3 bytes into buffer of length 3 */
   do_cu21(malloc(3), 10, pattern3, 2);             // no complaint

   /* Write 4 bytes into buffer of length 1 */
   do_cu21(malloc(1), 10, pattern4, 4);             // complaint (4 bytes)

   /* Write 4 bytes into buffer of length 2 */
   do_cu21(malloc(2), 10, pattern4, 4);             // complaint (4 bytes)

   /* Write 4 bytes into buffer of length 3 */
   do_cu21(malloc(3), 10, pattern4, 4);             // complaint (4 bytes)

   /* Write 4 bytes into buffer of length 4 */
   do_cu21(malloc(4), 10, pattern4, 4);             // no complaint

   /*------------------------------------------------------------*/
   /* Read uninitialised data                                    */
   /*------------------------------------------------------------*/
   uint8_t *input = malloc(10);

   /* Input buffer is completely uninitialised */
   do_cu21(malloc(4), 4, (void *)input, 2);         // complaint
   
   /* Read 2 bytes from input buffer. First byte is uninitialised */
   input = malloc(10);
   input[1] = 0x0;
   do_cu21(malloc(4), 4, (void *)input, 2);          // complaint

   /* Read 2 bytes from input buffer. Second byte is uninitialised */
   input = malloc(10);
   input[0] = 0x0;
   do_cu21(malloc(4), 4, (void *)input, 2);          // complaint
   
   /* Read 2 bytes from input buffer. All bytes are initialised */
   input = malloc(10);
   input[0] = input[1] = 0x0;
   do_cu21(malloc(4), 4, (void *)input, 2);          // no complaint
   
   /* Read 4 bytes from input buffer. This iterates once. In the 1st
      iteration all input bytes are initialised in the 2nd iteration all
      input bytes are uninitialised. */
   input = malloc(10);
   input[0] = input[1] = 0x0;
   do_cu21(malloc(4), 4, (void *)input, 4);          // complaint
   
   /* Write to NULL */
   //   do_cu21(NULL, 10, pattern1, sizeof pattern1);    // complaint

   return 0;
}
