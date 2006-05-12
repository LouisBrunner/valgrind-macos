
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

unsigned int do32 ( unsigned int x )
{
  unsigned int* y = malloc(sizeof(unsigned int));
  unsigned int* z = malloc(sizeof(unsigned int));
  unsigned int t;
  assert(y);
  assert(z);
  y[0] = x;
  __asm__ __volatile__(
     "pushl %0\n\t"
     "pushl %1\n\t"
     "popl %%ebx\n\t"
     "popl %%eax\n\t"
     "pushl 0(%%eax)\n\t"
     "popl 0(%%ebx)"
     : /*OUT*/
     : /*IN*/ "r"(y), "r"(z)
     : /*TRASH*/ "memory", "eax", "ebx"
  );
  t = z[0];
  free(y);
  free(z);
  return t;
}

unsigned short do16 ( unsigned short x )
{
  unsigned short* y = malloc(sizeof(unsigned short));
  unsigned short* z = malloc(sizeof(unsigned short));
  unsigned short t;
  assert(y);
  assert(z);
  y[0] = x;
  __asm__ __volatile__(
     "pushl %0\n\t"
     "pushl %1\n\t"
     "popl %%ebx\n\t"
     "popl %%eax\n\t"
     "pushw 0(%%eax)\n\t"
     "popw 0(%%ebx)"
     : /*OUT*/
     : /*IN*/ "r"(y), "r"(z)
     : /*TRASH*/ "memory", "eax", "ebx"
  );
  t = z[0];
  free(y);
  free(z);
  return t;
}


int main ( void )
{
   printf("do32: 0x%08X\n", do32(0xCafeBabe) );
   printf("do16: 0x%08X\n", (unsigned int)do16(0xfeBa) );
   return 0;
}
