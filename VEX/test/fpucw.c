
#include <stdio.h>

void fldcw_default ( void )
{
  asm(" pushw $0x037F ; fldcw (%esp) ; addl $2, %esp");
}

void fldcw_exns ( void )
{
  asm(" pushw $0x037E ; fldcw (%esp) ; addl $2, %esp");
}

void fldcw_precision ( void )
{
  asm(" pushw $0x007F ; fldcw (%esp) ; addl $2, %esp");
}

void fldcw_rounding ( void )
{
  asm(" pushw $0x077F ; fldcw (%esp) ; addl $2, %esp");
}

int main ( void )
{
   printf("default\n");
   fldcw_default();
   printf("\n");

   printf("exns\n");
   fldcw_exns();
   printf("\n");

   printf("precision\n");
   fldcw_precision();
   printf("\n");

   printf("rounding\n");
   fldcw_rounding();
   printf("\n");

   return 0;
}
