
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#define JZ_NEXT ".byte 0x74,0x00"  /* jz the-next-insn */

int main ( void )
{
   char* junk = malloc(48);
   assert(junk);


   /* --- INTEGER --- */

   printf("\nComplain int64\n");
   __asm__ __volatile__(
      "movq   0(%0), %%rax\n\t"
      "movq   8(%0), %%r8\n\t"
      "xorq   %%r8, %%rax\n\t"
      JZ_NEXT
      : : "r"(junk) : "r8", "rax", "cc"
   );

   printf("\nNo complain int64\n");
   __asm__ __volatile__(
      "movq   0(%0), %%rax\n\t"
      "movq   8(%0), %%r8\n\t"
      "xorq   %%rax, %%rax\n\t"
      JZ_NEXT
      : : "r"(junk) : "r8", "rax", "cc"
   );


   /* --- MMX --- */

   printf("\nComplain mmx\n");
   __asm__ __volatile__(
      "emms\n\t"
      "movq   0(%0), %%mm0\n\t"
      "movq   8(%0), %%mm7\n\t"
      "pxor   %%mm7, %%mm0\n\t"
      "movq   %%mm0, 16(%0)\n\t"
      "cmpq   $0,16(%0)\n\t"
      JZ_NEXT
      : : "r"(junk) : "mm7", "mm0", "cc", "memory"
   );

   printf("\nNo complain mmx\n");
   __asm__ __volatile__(
      "emms\n\t"
      "movq   0(%0), %%mm0\n\t"
      "movq   8(%0), %%mm7\n\t"
      "pxor   %%mm0, %%mm0\n\t"
      "movq   %%mm0, 16(%0)\n\t"
      "cmpq   $0,16(%0)\n\t"
      JZ_NEXT
      : : "r"(junk) : "mm7", "mm0", "cc", "memory"
   );


   /* --- SSE1 --- */

   printf("\nComplain sse xorps\n");
   __asm__ __volatile__(
      "movups   0(%0),  %%xmm0\n\t"
      "movups   16(%0), %%xmm8\n\t"
      "xorps    %%xmm8, %%xmm0\n\t"
      "movups   %%xmm0, 32(%0)\n\t"
      "movq 32(%0), %%rax\n\t"
      "addq 40(%0), %%rax\n\t"
      JZ_NEXT
      : : "r"(junk) : "rax", "xmm8", "xmm0", "cc", "memory"
   );

   printf("\nNo complain sse xorps\n");
   __asm__ __volatile__(
      "movups   0(%0),  %%xmm0\n\t"
      "movups   16(%0), %%xmm8\n\t"
      "xorps    %%xmm0, %%xmm0\n\t"
      "movups   %%xmm0, 32(%0)\n\t"
      "movq 32(%0), %%rax\n\t"
      "addq 40(%0), %%rax\n\t"
      JZ_NEXT
      : : "r"(junk) : "rax", "xmm8", "xmm0", "cc", "memory"
   );


   /* --- SSE2 --- */

   printf("\nComplain sse2 pxor\n");
   __asm__ __volatile__(
      "movups   0(%0),  %%xmm0\n\t"
      "movups   16(%0), %%xmm8\n\t"
      "pxor     %%xmm8, %%xmm0\n\t"
      "movups   %%xmm0, 32(%0)\n\t"
      "movq 32(%0), %%rax\n\t"
      "addq 40(%0), %%rax\n\t"
      JZ_NEXT
      : : "r"(junk) : "rax", "xmm8", "xmm0", "cc", "memory"
   );

   printf("\nNo complain sse2 pxor\n");
   __asm__ __volatile__(
      "movups   0(%0),  %%xmm0\n\t"
      "movups   16(%0), %%xmm8\n\t"
      "pxor     %%xmm0, %%xmm0\n\t"
      "movups   %%xmm0, 32(%0)\n\t"
      "movq 32(%0), %%rax\n\t"
      "addq 40(%0), %%rax\n\t"
      JZ_NEXT
      : : "r"(junk) : "rax", "xmm8", "xmm0", "cc", "memory"
   );


   printf("\nComplain sse2 xorpd\n");
   __asm__ __volatile__(
      "movups   0(%0),  %%xmm0\n\t"
      "movups   16(%0), %%xmm8\n\t"
      "xorpd    %%xmm8, %%xmm0\n\t"
      "movups   %%xmm0, 32(%0)\n\t"
      "movq 32(%0), %%rax\n\t"
      "addq 40(%0), %%rax\n\t"
      JZ_NEXT
      : : "r"(junk) : "rax", "xmm8", "xmm0", "cc", "memory"
   );

   printf("\nNo complain sse2 xorpd\n");
   __asm__ __volatile__(
      "movups   0(%0),  %%xmm0\n\t"
      "movups   16(%0), %%xmm8\n\t"
      "xorpd    %%xmm0, %%xmm0\n\t"
      "movups   %%xmm0, 32(%0)\n\t"
      "movq 32(%0), %%rax\n\t"
      "addq 40(%0), %%rax\n\t"
      JZ_NEXT
      : : "r"(junk) : "rax", "xmm8", "xmm0", "cc", "memory"
   );


   free(junk);
   return 0;
}
