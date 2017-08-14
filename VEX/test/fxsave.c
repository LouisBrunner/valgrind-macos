
#include <stdio.h>
#include <stdlib.h>
#include <malloc.h>
#include <string.h>

const unsigned int vec0[4]
   = { 0x12345678, 0x11223344, 0x55667788, 0x87654321 };

const unsigned int vec1[4]
   = { 0xABCDEF01, 0xAABBCCDD, 0xEEFF0011, 0x10FEDCBA };

const unsigned int vecZ[4]
   = { 0, 0, 0, 0 };

void do_fxsave ( void* p ) {
   asm __volatile__("fxsave (%0)" : : "r" (p) : "memory" );
}

void do_fxrstor ( void* p ) {
   asm __volatile__("fxrstor (%0)" : : "r" (p) : "memory" );
}

void do_zeroise ( void )
{
   asm __volatile__("finit");
   asm __volatile__(
    "fldz\n\t"
    "fldz\n\t"
    "fldz\n\t"
    "fldz\n\t"
    "fldz\n\t"
    "fldz\n\t"
    "fldz\n\t"
    "fldz\n\t"
    "finit\n");
   asm __volatile__("movups vecZ, %xmm0");
   asm __volatile__("movups vecZ, %xmm1");
   asm __volatile__("movups vecZ, %xmm2");
   asm __volatile__("movups vecZ, %xmm3");
   asm __volatile__("movups vecZ, %xmm4");
   asm __volatile__("movups vecZ, %xmm5");
   asm __volatile__("movups vecZ, %xmm6");
   asm __volatile__("movups vecZ, %xmm7");
   asm __volatile__(
      "pushl $0\n\t"
      "ldmxcsr 0(%esp)\n\t"
      "addl $4,%esp\n");
}

/* set up the FP and SSE state, and then dump it. */
void do_setup_then_fxsave ( void* p )
{
   asm __volatile__("finit");
   asm __volatile__("fldpi");
   asm __volatile__("fld1");
   asm __volatile__("fldln2");
   asm __volatile__("fldlg2");
   asm __volatile__("fld %st(3)");
   asm __volatile__("fld %st(3)");
   asm __volatile__("movups vec0, %xmm0");
   asm __volatile__("movups vec1, %xmm1");
   asm __volatile__("xorps %xmm2, %xmm2");
   asm __volatile__("movaps %xmm2, %xmm3");
   asm __volatile__("movaps %xmm2, %xmm4");
   asm __volatile__("movaps %xmm2, %xmm5");
   asm __volatile__("movaps %xmm2, %xmm6");
   asm __volatile__("movaps %xmm1, %xmm7");
   asm __volatile__("xorps %xmm0, %xmm7");
   do_fxsave (p);
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

void show ( unsigned char* buf, int xx )
{
   int i;
   for (i = 0; i < 512; i++) {
      if ((i % 16) == 0)
         printf("%3d   ", i);
      if (xx && isFPLsbs(i))
	 printf("xx ");
      else
         printf("%02x ", buf[i]);
      if (i > 0 && ((i % 16) == 15))
          printf("\n");
   }
}


int main ( int argc, char** argv )
{
   unsigned char* buf1 = memalign(16,512);
   unsigned char* buf2 = memalign(16,512);
   unsigned char* buf3 = memalign(16,512);
   int xx = argc > 1;
   printf("Re-run with any arg to suppress least-significant\n"
          "   16 bits of FP numbers\n");
   memset(buf1, 0x55, 512);
   memset(buf2, 0x55, 512);
   memset(buf3, 0x55, 512);

   /* Load up x87/xmm state and dump it. */
   do_setup_then_fxsave(buf1);
   printf("\nBEFORE\n");
   show(buf1, xx);

   /* Zeroise x87/xmm state and dump it, to show that the
      regs have been cleared out. */
   do_zeroise();
   do_fxsave(buf2);
   printf("\nZEROED\n");
   show(buf2, xx);

   /* Reload x87/xmm state from buf1 and dump it in buf3. */
   do_fxrstor(buf1);
   do_fxsave(buf3);
   printf("\nRESTORED\n");
   show(buf3, xx);

   free(buf1); free(buf2); free(buf3);

   return 0;
}
