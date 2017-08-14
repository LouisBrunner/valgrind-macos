
#include <stdio.h>
#include <stdlib.h>

void do_fsave ( void* p )
{
   asm __volatile__("fninit");
   asm __volatile__("fldpi");
   asm __volatile__("fld1");
   asm __volatile__("fldln2");
   asm __volatile__("fsave (%0)" : : "r" (p) : "memory" );
}

int isFPLsbs ( int i )
{
   int q;
   q = 0; if (i == q || i == q+1) return 1;
   q = 10; if (i == q || i == q+1) return 1;
   q = 20; if (i == q || i == q+1) return 1;
   q = 30; if (i == q || i == q+1) return 1;
   q = 40; if (i == q || i == q+1) return 1;
   q = 50; if (i == q || i == q+1) return 1;
   q = 60; if (i == q || i == q+1) return 1;
   q = 70; if (i == q || i == q+1) return 1;
   return 0;
}

void show_fpustate ( unsigned char* buf, int hide64to80 )
{
   int i;
   printf("  0   ");
   for (i = 0; i < 14; i++)
      printf("%02x ", buf[i]);
   printf("\n");

   printf(" 14   ");
   for (i = 14; i < 28; i++)
      printf("%02x ", buf[i]);
   printf("\n");

   for (i = 0; i < 80; i++) {
      if ((i % 10) == 0)
         printf("%3d   ", i+28);
      if (hide64to80 && isFPLsbs(i))
	 printf("xx ");
      else
         printf("%02x ", buf[i+28]);
      if (i > 0 && ((i % 10) == 9))
          printf("\n");
   }
}

int main ( int argc, char** argv )
{
   int i;
   unsigned char* buf = malloc(108);
   int xx = argc > 1;
   printf("Re-run with any arg to suppress least-significant\n"
          "   16 bits of FP numbers\n");
   for (i = 0; i < 108; i++)
      buf[i] = 0xAA;

   /* dump FPU state in buf, and show it. */
   do_fsave(buf);
   show_fpustate( buf, xx );

   return 0;
}
