
// This should be compiled as Thumb code, since currently V only
// handles the T1 encoding of ldrt.  This all assumes that we are
// in a little-endian world.

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

__attribute__((noinline)) void do_strt_imm_132 ( unsigned char* p, UInt val )
{
  __asm__ __volatile__(
     "mov r5, %0 ; mov r6, %1 ; strt r6, [r5, #132]"
      : : "r"(p), "r"(val) : "r5", "r6", "memory"
  );
}

int main ( void )
{
  UInt i;
  unsigned char* b = malloc(256);
  for (i = 0; i < 256; i++) b[i] = (unsigned char)i;
  UInt r = do_ldrt_imm_132(b);
  free(b);
  printf("result is 0x%08x (should be 0x%08x)\n", r, 0x87868584);

  UInt val = (200 << 0) | (150 << 8) | (100 << 16) | (10 << 24);
  unsigned char* c = malloc(256);
  for (i = 0; i < 256; i++) c[i] = (unsigned char)i;
  do_strt_imm_132(c, val);
  printf("result is %u %u %u %u %u %u (should be %u %u %u %u %u %u)\n",
         c[131], c[132], c[133], c[134], c[135], c[136],
         131, 200, 150, 100, 10, 136);
  free(c);
  return 0;
}
