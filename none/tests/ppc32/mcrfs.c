
#include <stdio.h>

typedef unsigned int UInt;

void set_fpscr ( UInt x )
{
   UInt d[2];
   d[0] = 0;
   d[1] = x;
   __asm__ __volatile__(
      "lfd %%f0,0(%0)\n\t"
      "mtfsf 255,%%f0"
      : /*out*/
      : /*in*/ "b"(&d[0])
      : /*trash*/ "fr0", "cc", "memory"
   );

}

int main ( void )
{
  int i;
  UInt r;
  for (i = 0; i < 16; i++) {
    set_fpscr(i);
    __asm__ __volatile__(
       "li %0,0\n\t"
       "mtcr %0\n\t"
       "mcrfs 1,7\n\t"
       "mfcr %0"
       : /*out*/ "=b"(r)
       : /*in*/ 
       : /*trash*/ "cc" 
    );
    printf("0x%x -> 0x%08x\n", i, r);
  }
  for (i = 0; i < 16; i++) {
    set_fpscr(i);
    __asm__ __volatile__(
       "li %0,-1\n\t"
       "mtcr %0\n\t"
       "mcrfs 1,7\n\t"
       "mfcr %0"
       : /*out*/ "=b"(r)
       : /*in*/ 
       : /*trash*/ "cc" 
    );
    printf("0x%x -> 0x%08x\n", i, r);
  }
  return 0;
}
