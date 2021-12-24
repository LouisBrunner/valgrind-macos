/* The copy, paste, cpabort are ISA 3.0 instructions.  However, the memory
   to memory copy is only supported on ISA 3.1 era machines.

   The following test does a memory to memory copy test, an out of order
   paste test, and a copy paste abort test.  This test is only supported
   ISA 3.1 systems.  */

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <malloc.h>

#ifdef HAS_ISA_3_1
#include <altivec.h>

/* return CR0 in least significant bits */
#define GET_CR0(_lval)                                  \
   __asm__ __volatile__ ("mfcr %0"  : "=b"(_lval) );    \
   __asm__ __volatile__ ("srawi %0,%1,28" : "=r" (_lval) : "r" (_lval));

void test_copy (uint8_t *reg)
{
  __asm__ __volatile__ (".machine push; .machine power9; " \
                        "copy 0,%0; hwsync; .machine pop;" : : "r" (reg));
}

void test_paste (uint8_t *reg)
{
  __asm__ __volatile__ (".machine push; .machine power9; " \
                        "paste. 0,%0; hwsync; .machine pop;" : : "r" (reg));
}

void test_cpabort (void)
{
  __asm__ __volatile__ (".machine push; .machine power9; " \
                        "cpabort; hwsync; .machine pop;");
}

#define NUM_ELEMENTS 128
#define SUCCESS 1
#define FAILURE 2
#define DEBUG 0
#define PASTE_ERROR 0
#endif

int main()
{
#ifdef HAS_ISA_3_1
   int i;
   unsigned int cc_value;
   int result = SUCCESS;

   /* buffers must be 128 bytes long and aligned to 128 bytes */
   uint8_t src_buffer[NUM_ELEMENTS] __attribute__ ((aligned (128)));
   uint8_t dst_buffer[NUM_ELEMENTS] __attribute__ ((aligned (128)));

   for (i=0; i<NUM_ELEMENTS; i++) {
     src_buffer[i] = 10 + i;
     dst_buffer[i] = i;
   }

#if DEBUG
   printf("Initial contents of src/dst buffer\n");
   for (i=0; i<NUM_ELEMENTS; i++) {
      printf("src_buffer[%d] = 0x%x  ", i, src_buffer[i]);
      printf("dst_buffer[%d] = 0x%x\n", i, dst_buffer[i]);
   }
#endif

    /* Test correct copy past sequence. */
   test_copy (src_buffer);
   test_paste (dst_buffer);
   GET_CR0(cc_value);

#if DEBUG
   printf("CR0 = 0x%x\n", cc_value);
#endif

#if DEBUG
   printf("AFTER COPY/PASTE Contents of src/dst buffer\n");
   for (i=0; i<NUM_ELEMENTS; i++) {
      printf("src_buffer[%d] = 0x%x  ", i, src_buffer[i]);
      printf("dst_buffer[%d] = 0x%x\n", i, dst_buffer[i]);
   }
#endif

   /* Check if result matches initial values. */
   for (i=0; i<NUM_ELEMENTS; i++) {
      if (src_buffer[i] != dst_buffer[i]) {
         result = FAILURE;
      printf("ERROR: dst_buffer[%d] = 0x%x\n", i, dst_buffer[i]);
      printf("    not equal to src_buffer[%d] = 0x%x\n", i, src_buffer[i]);
      }
   }

   if (cc_value == PASTE_ERROR) {
      result = FAILURE;
      printf("ERROR: result code wrong for correct copy/paste sequence.\n");
   }

   /* Test out of order paste.  */
   for (i=0; i<NUM_ELEMENTS; i++) {
     dst_buffer[i] = i;
   }

   test_paste (dst_buffer);
   GET_CR0(cc_value);

   if (cc_value != PASTE_ERROR) {
      result = FAILURE;
      printf("ERROR: out of order past result 0x%x is wrong.\n", cc_value);
   }

   /* Issue cpabort instruction just to see if we get a segmentation fault or
      other catastrophic failure.  No specific result to check for failure.  */
   test_cpabort ();

   if (result == SUCCESS)
      printf("SUCCESS.\n");
   else
      printf("FAILURE.\n");

#else
  printf("HAS_ISA_3_1 not detected.\n");
#endif
   return 0;
}
