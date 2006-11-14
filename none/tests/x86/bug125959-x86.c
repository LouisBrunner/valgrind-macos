
#include <stdio.h>

unsigned char buf[256]; 

static int lookup ( int i )
{
  int block[3];
  block[0] = (int)&buf[0];
  block[1] = i;
  block[2] = 0;
  __asm__ __volatile__(
     "movl %0,%%esi\n\t"
     "movl 0(%%esi),%%ebx\n\t"
     "movl 4(%%esi),%%eax\n\t"
     "xlat\n\t"
     "movl %%eax,8(%%esi)\n\t"
     : : /*in*/"r"(block) : "esi", "ebx", "eax", "memory", "cc"
  );
  return block[2];
}

int main(void) 
{ 
  int i, j;
   for (i = 0; i < 256; i++) 
    buf[i] = (unsigned char) i;

   j = 0;
   for (i = 0; i < 130; i++) {
     printf("%08x\n", lookup(j));
     j += 0x01000001;
   }

   return 0; 
}
