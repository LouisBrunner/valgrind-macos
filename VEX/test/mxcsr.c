
#include <stdio.h>

void mxcsr_default ( void )
{
  asm(" pushl $0x1F80 ; ldmxcsr (%esp) ; addl $4, %esp");
}

void mxcsr_exns ( void )
{
  asm(" pushl $0x1F00 ; ldmxcsr (%esp) ; addl $4, %esp");
}

/* PIII doesn't have DAZ, so this segfaults (!) on PIII. */a
void mxcsr_daz ( void )
{
  asm(" pushl $0x1FC0 ; ldmxcsr (%esp) ; addl $4, %esp");
}

void mxcsr_fz ( void )
{
  asm(" pushl $0x9F80 ; ldmxcsr (%esp) ; addl $4, %esp");
}


int main ( void )
{
   printf("default\n");
   mxcsr_default();
   printf("\n");

   printf("exns\n");
   mxcsr_exns();
   printf("\n");

   printf("daz\n");
   mxcsr_daz();
   printf("\n");

   printf("fz\n");
   mxcsr_fz();
   printf("\n");

   return 0;
}
