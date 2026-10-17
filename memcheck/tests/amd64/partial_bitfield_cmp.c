/* Unsigned ordered comparisons of partially defined words (bug 523434).
   clang tests a bitfield that shares its storage word with never-written
   bits, e.g. (w >> 48) != 0, as the unsigned compare w >=u 1<<48.  The
   constant has 48 trailing zero bits, so the undefined low bits of w cannot
   change the result and Memcheck must not complain.  When they can change
   it, it still must. */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

int main ( void )
{
   /* Undefined memory.  Each case clears the high bits with an AND, which
      leaves them defined, and keeps the low bits undefined. */
   char* junk = malloc(16);
   assert(junk);

   printf("\nNo complain 64-bit jae\n");
   __asm__ __volatile__(
      "movq   0(%0), %%rax\n\t"
      "movabsq $0x0000ffffffffffff, %%r8\n\t"
      "andq   %%r8, %%rax\n\t"           /* low 48 bits undefined */
      "movabsq $0x0001000000000000, %%r8\n\t"
      "cmpq   %%r8, %%rax\n\t"
      "jae    1f\n"
      "1:\n\t"
      : : "r"(junk) : "r8", "rax", "cc"
   );

   printf("\nNo complain 64-bit ja\n");
   __asm__ __volatile__(
      "movq   0(%0), %%rax\n\t"
      "movabsq $0x0000ffffffffffff, %%r8\n\t"
      "andq   %%r8, %%rax\n\t"
      "movabsq $0x0001000000000000, %%r8\n\t"
      "cmpq   %%r8, %%rax\n\t"
      "ja     1f\n"
      "1:\n\t"
      : : "r"(junk) : "r8", "rax", "cc"
   );

   printf("\nComplain 64-bit jae\n");
   __asm__ __volatile__(
      "movq   0(%0), %%rax\n\t"
      "movabsq $0x0000ffffffffffff, %%r8\n\t"
      "andq   %%r8, %%rax\n\t"
      "cmpq   $0x1000, %%rax\n\t"        /* undefined bits decide this */
      "jae    1f\n"
      "1:\n\t"
      : : "r"(junk) : "r8", "rax", "cc"
   );

   printf("\nNo complain 32-bit jae\n");
   __asm__ __volatile__(
      "movl   8(%0), %%eax\n\t"
      "andl   $0x0000ffff, %%eax\n\t"    /* low 16 bits undefined */
      /* VEX turns cmpl $0x10000 + jae into a shift and an equality test,
         which would not reach CmpLT32U. */
      "cmpl   $0x00030000, %%eax\n\t"
      "jae    1f\n"
      "1:\n\t"
      : : "r"(junk) : "rax", "cc"
   );

   printf("\nNo complain 32-bit ja\n");
   __asm__ __volatile__(
      "movl   8(%0), %%eax\n\t"
      "andl   $0x0000ffff, %%eax\n\t"
      "cmpl   $0x00010000, %%eax\n\t"
      "ja     1f\n"
      "1:\n\t"
      : : "r"(junk) : "rax", "cc"
   );

   printf("\nComplain 32-bit jae\n");
   __asm__ __volatile__(
      "movl   8(%0), %%eax\n\t"
      "andl   $0x0000ffff, %%eax\n\t"
      "cmpl   $0x100, %%eax\n\t"
      "jae    1f\n"
      "1:\n\t"
      : : "r"(junk) : "rax", "cc"
   );

   free(junk);
   return 0;
}
