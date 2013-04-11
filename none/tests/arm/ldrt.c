
// This should be compiled as Thumb code, since currently V only
// handles the T1 encoding of ldrt.

#include <stdio.h>
#include <malloc.h>

typedef unsigned int UInt;

__attribute__((noinline)) UInt do_ldrt_imm_132 ( unsigned char* p )
{
  UInt res;
  __asm__ __volatile__(
     "mov r5, %1 ; ldrt r6, [r5, #132] ; mov %0, r6"
      : "=r"(res) : "r"(p) : "r5", "r6"
  );
  return res;
}

int main ( void )
{
  UInt i;
  unsigned char* b = malloc(256);
  for (i = 0; i < 256; i++) b[i] = (unsigned char)i;
  UInt r = do_ldrt_imm_132(b);
  free(b);
  printf("result is 0x%08x (should be 0x%08x)\n", r, 0x87868584);
  return 0;
}
