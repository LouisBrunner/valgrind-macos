
#include <stdio.h>
#include <stdlib.h>

void do_fld1 ( void* p )
{
   asm __volatile__("fninit");
   asm __volatile__("fld1");
   asm __volatile__("fstpl (%0)" : : "r" (p) : "memory" );
}

void do_fldl2t ( void* p )
{
   asm __volatile__("fninit");
   asm __volatile__("fldl2t");
   asm __volatile__("fstpl (%0)" : : "r" (p) : "memory" );
}

void do_fldl2e ( void* p )
{
   asm __volatile__("fninit");
   asm __volatile__("fldl2e");
   asm __volatile__("fstpl (%0)" : : "r" (p) : "memory" );
}

void do_fldpi ( void* p )
{
   asm __volatile__("fninit");
   asm __volatile__("fldpi");
   asm __volatile__("fstpl (%0)" : : "r" (p) : "memory" );
}

void do_fldlg2 ( void* p )
{
   asm __volatile__("fninit");
   asm __volatile__("fldlg2");
   asm __volatile__("fstpl (%0)" : : "r" (p) : "memory" );
}

void do_fldln2 ( void* p )
{
   asm __volatile__("fninit");
   asm __volatile__("fldln2");
   asm __volatile__("fstpl (%0)" : : "r" (p) : "memory" );
}

void do_fldz ( void* p )
{
   asm __volatile__("fninit");
   asm __volatile__("fldz");
   asm __volatile__("fstpl (%0)" : : "r" (p) : "memory" );
}

typedef  unsigned char  UChar;

void foo ( void (*f)(void*), char* name )
{
  int i;
  UChar* b = malloc(8);
  f(b);
  printf("%s IRConst_F64i(0x", name);
  for (i = 7; i >= 0; i--)
    printf("%02x", (int)b[i]);
  printf(")\n");
}

int main ( void )
{
  foo( do_fld1,   "fld1  ");
  foo( do_fldl2t, "fldl2t");
  foo( do_fldl2e, "fldl2e");
  foo( do_fldpi,  "fldpi ");
  foo( do_fldlg2, "fldlg2");
  foo( do_fldln2, "fldln2");
  foo( do_fldz,   "fldz  ");
  return 0;
}
