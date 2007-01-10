
#include <stdio.h>
#include <stdlib.h>

const unsigned int vec0[4]
   = { 0x12345678, 0x11223344, 0x55667788, 0x87654321 };

const unsigned int vec1[4]
   = { 0xABCDEF01, 0xAABBCCDD, 0xEEFF0011, 0x10FEDCBA };

/* set up the FP and SSE state, and then dump it. */
void do_fxsave ( void* p )
{
   asm __volatile__("finit");
   asm __volatile__("fldpi");
   asm __volatile__("fld1");
   asm __volatile__("fldln2");
   asm __volatile__("fldlg2");
   asm __volatile__("fld %st(3)");
   asm __volatile__("fld %st(3)");
   asm __volatile__("fld1");
   asm __volatile__("movups (%0), %%xmm0" : : "r"(&vec0[0]) : "xmm0" );
   asm __volatile__("movups (%0), %%xmm1" : : "r"(&vec1[0]) : "xmm1" );
   asm __volatile__("xorps  %xmm2, %xmm2");
   asm __volatile__("movaps %xmm0, %xmm3");
   asm __volatile__("movaps %xmm1, %xmm4");
   asm __volatile__("movaps %xmm2, %xmm5");
   asm __volatile__("movaps %xmm0, %xmm6");
   asm __volatile__("movaps %xmm1, %xmm7");
   asm __volatile__("movaps %xmm1, %xmm8");
   asm __volatile__("movaps %xmm2, %xmm9");
   asm __volatile__("movaps %xmm0, %xmm10");
   asm __volatile__("movaps %xmm1, %xmm11");
   asm __volatile__("movaps %xmm1, %xmm12");
   asm __volatile__("movaps %xmm2, %xmm13");
   asm __volatile__("movaps %xmm0, %xmm14");
   asm __volatile__("movaps %xmm1, %xmm15");
   asm __volatile__("fxsave (%0)" : : "r" (p) : "memory" );
}

int isFPLsbs ( int i )
{
   int q;
   q = 32; if (i == q || i == q+1) return 1;
   q = 48; if (i == q || i == q+1) return 1;
   q = 64; if (i == q || i == q+1) return 1;
   q = 80; if (i == q || i == q+1) return 1;
   q = 96; if (i == q || i == q+1) return 1;
   q = 112; if (i == q || i == q+1) return 1;
   q = 128; if (i == q || i == q+1) return 1;
   q = 144; if (i == q || i == q+1) return 1;
   return 0;
}

int main ( int argc, char** argv )
{
   int i, j;
   unsigned char* buf = malloc(512);
   int xx = 1; /* argc > 1;
   printf("Re-run with any arg to suppress least-significant\n"
          "   16 bits of FP numbers\n");
	       */
   for (i = 0; i < 512; i++)
      buf[i] = 0x55;

   do_fxsave(buf);
   for (j = 0; j < 512; j++) {
      i = (j & 0xFFF0) + (15 - (j & 0xF));
      if ((j % 16) == 0)
         printf("%3d   ", j);
      if (xx && isFPLsbs(i))
	 printf("xx ");
      else
         printf("%02x ", buf[i]);
      if (j > 0 && ((j % 16) == 15))
          printf("\n");
   }
   return 0;
}
