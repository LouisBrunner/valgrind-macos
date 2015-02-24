#include <stdio.h>
#include "opcodes.h"

/* Test that emulation warnings appear when the floating point
   extension facility is not installed and the opcode specifies
   a rounding mode other than 0. */
int
main(void)
{
   printf("before\n");
   __asm__ volatile ( CEGBRA(1,0,0,0) : : : "cc", "memory");
   __asm__ volatile ( CEFBRA(3,0,0,0) : : : "cc", "memory");
   __asm__ volatile ( CDGBRA(4,0,0,0) : : : "cc", "memory");

   /* Note: an emulation warning is expected for the following
      insn but none is given. The reason is that at most 3 warnings
      of a given kind will be issued - and we already had three. */
   __asm__ volatile ( CEFBRA(5,0,0,0) : : : "cc", "memory");

   printf("after\n");
   return 0;
}
