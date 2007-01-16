
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#define JZ_NEXT ".byte 0x74,0x00"  /* jz the-next-insn */

int main ( void )
{
   char* junk = malloc(48);
   assert(junk);


   /* --- INTEGER --- */

   printf("\nComplain int32\n");
   __asm__ __volatile__(
      "movl   0(%0), %%eax\n\t"
      "movl   8(%0), %%edi\n\t"
      "xorl   %%edi, %%eax\n\t"
      JZ_NEXT
      : : "r"(junk) : "edi", "eax", "cc"
   );

   printf("\nNo complain int32\n");
   __asm__ __volatile__(
      "movl   0(%0), %%eax\n\t"
      "movl   8(%0), %%edi\n\t"
      "xorl   %%eax, %%eax\n\t"
      JZ_NEXT
      : : "r"(junk) : "edi", "eax", "cc"
   );


   /* --- MMX --- */

   printf("\nComplain mmx\n");
   __asm__ __volatile__(
      "emms\n\t"
      "movq   0(%0), %%mm0\n\t"
      "movq   8(%0), %%mm7\n\t"
      "pxor   %%mm7, %%mm0\n\t"
      "movq   %%mm0, 16(%0)\n\t"
      "movl   16(%0), %%esi\n\t"
      "addl   20(%0), %%esi\n\t"
      JZ_NEXT
      : : "r"(junk) : "esi", "mm7", "mm0", "cc", "memory"
   );

   printf("\nNo complain mmx\n");
   __asm__ __volatile__(
      "emms\n\t"
      "movq   0(%0), %%mm0\n\t"
      "movq   8(%0), %%mm7\n\t"
      "pxor   %%mm0, %%mm0\n\t"
      "movq   %%mm0, 16(%0)\n\t"
      "movl   16(%0), %%esi\n\t"
      "addl   20(%0), %%esi\n\t"
      JZ_NEXT
      : : "r"(junk) : "esi", "mm7", "mm0", "cc", "memory"
   );


   /* --- SSE1 --- */

   printf("\nComplain sse xorps\n");
   __asm__ __volatile__(
      "movups   0(%0),  %%xmm0\n\t"
      "movups   16(%0), %%xmm7\n\t"
      "xorps    %%xmm7, %%xmm0\n\t"
      "movups   %%xmm0, 32(%0)\n\t"
      "movl 32(%0), %%esi\n\t"
      "addl 36(%0), %%esi\n\t"
      "addl 40(%0), %%esi\n\t"
      "addl 44(%0), %%esi\n\t"
      JZ_NEXT
      : : "r"(junk) : "esi", "xmm7", "xmm0", "cc", "memory"
   );

   printf("\nNo complain sse xorps\n");
   __asm__ __volatile__(
      "movups   0(%0),  %%xmm0\n\t"
      "movups   16(%0), %%xmm7\n\t"
      "xorps    %%xmm0, %%xmm0\n\t"
      "movups   %%xmm0, 32(%0)\n\t"
      "movl 32(%0), %%esi\n\t"
      "addl 36(%0), %%esi\n\t"
      "addl 40(%0), %%esi\n\t"
      "addl 44(%0), %%esi\n\t"
      JZ_NEXT
      : : "r"(junk) : "esi", "xmm7", "xmm0", "cc", "memory"
   );


   /* --- SSE2 --- */
#if 0
   printf("\nComplain sse2 pxor\n");
   __asm__ __volatile__(
      "movups   0(%0),  %%xmm0\n\t"
      "movups   16(%0), %%xmm7\n\t"
      "pxor     %%xmm7, %%xmm0\n\t"
      "movups   %%xmm0, 32(%0)\n\t"
      "movl 32(%0), %%esi\n\t"
      "addl 36(%0), %%esi\n\t"
      "addl 40(%0), %%esi\n\t"
      "addl 44(%0), %%esi\n\t"
      JZ_NEXT
      : : "r"(junk) : "esi", "xmm7", "xmm0", "cc", "memory"
   );

   printf("\nNo complain sse2 pxor\n");
   __asm__ __volatile__(
      "movups   0(%0),  %%xmm0\n\t"
      "movups   16(%0), %%xmm7\n\t"
      "pxor     %%xmm0, %%xmm0\n\t"
      "movups   %%xmm0, 32(%0)\n\t"
      "movl 32(%0), %%esi\n\t"
      "addl 36(%0), %%esi\n\t"
      "addl 40(%0), %%esi\n\t"
      "addl 44(%0), %%esi\n\t"
      JZ_NEXT
      : : "r"(junk) : "esi", "xmm7", "xmm0", "cc", "memory"
   );


   printf("\nComplain sse2 xorpd\n");
   __asm__ __volatile__(
      "movups   0(%0),  %%xmm0\n\t"
      "movups   16(%0), %%xmm7\n\t"
      "xorpd    %%xmm7, %%xmm0\n\t"
      "movups   %%xmm0, 32(%0)\n\t"
      "movl 32(%0), %%esi\n\t"
      "addl 36(%0), %%esi\n\t"
      "addl 40(%0), %%esi\n\t"
      "addl 44(%0), %%esi\n\t"
      JZ_NEXT
      : : "r"(junk) : "esi", "xmm7", "xmm0", "cc", "memory"
   );

   printf("\nNo complain sse2 xorpd\n");
   __asm__ __volatile__(
      "movups   0(%0),  %%xmm0\n\t"
      "movups   16(%0), %%xmm7\n\t"
      "xorpd    %%xmm0, %%xmm0\n\t"
      "movups   %%xmm0, 32(%0)\n\t"
      "movl 32(%0), %%esi\n\t"
      "addl 36(%0), %%esi\n\t"
      "addl 40(%0), %%esi\n\t"
      "addl 44(%0), %%esi\n\t"
      JZ_NEXT
      : : "r"(junk) : "esi", "xmm7", "xmm0", "cc", "memory"
   );
#endif

   free(junk);
   return 0;
}
