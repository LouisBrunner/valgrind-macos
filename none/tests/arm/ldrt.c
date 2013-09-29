
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

__attribute__((noinline)) void do_strbt_imm_132 ( unsigned char* p, UInt* val )
{
  __asm__ __volatile__(
     "mov r5, %0 ; ldr r6, [r5] ; mov r5, %1; strbt r6, [r5, #132]"
      : : "r"(val), "r"(p) : "r5", "r6", "memory"
  );
}

__attribute__((noinline)) UInt do_ldrht_imm_1 (unsigned char* val)
{
  UInt res;
  __asm__ __volatile__(
      "mov r4, %1 ; ldrht r5, [r4, #1]; mov %0, r5"
      : "=r"(res) : "r"(val) : "r4", "r5"
  );
  return res;
}

__attribute__((noinline)) void do_ldrsht_imm_1 (UInt* res)
{
  __asm__ __volatile__(
     "mov r4, %1 ; ldrsht r5, [r4, #1] ; str r5, [r4, #0]"
     : "+r"(res) : : "r4", "r5", "memory"
  );
}

__attribute__((noinline)) void do_strht_imm_132 ( unsigned char* p, UInt* val )
{
  __asm__ __volatile__(
     "mov r5, %0 ; ldr r6, [r5] ; mov r5, %1; strht r6, [r5, #132]"
      : : "r"(val), "r"(p) : "r5", "r6", "memory"
  );
}

__attribute__((noinline)) UInt do_ldrbt_imm_2 (unsigned char* val)
{
  UInt res;
  __asm__ __volatile__(
     "mov r4, %1 ; ldrbt r5, [r4, #2]; mov %0, r5"
     : "=r"(res) : "r"(val) : "r4", "r5"
  );
  return res;
}

__attribute__((noinline)) UInt do_ldrsbt_imm_2 (unsigned char* val)
{
  UInt res;
  __asm__ __volatile__(
     "mov r4, %1 ; ldrsbt r5, [r4, #2]; mov %0, r5"
     : "=r"(res) : "r"(val) : "r4", "r5"
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

  UInt val = (200 << 0) | (150 << 8) | (100 << 16) | (10 << 24);
  unsigned char* c = malloc(256);
  for (i = 0; i < 256; i++) c[i] = (unsigned char)i;
  do_strt_imm_132(c, val);
  printf("result is %u %u %u %u %u %u (should be %u %u %u %u %u %u)\n",
         c[131], c[132], c[133], c[134], c[135], c[136],
         131, 200, 150, 100, 10, 136);
  free(c);

  UInt val_bt = 0b11111111;
  unsigned char* d = malloc(256);
  for (i = 0; i < 256; i++) d[i] = (unsigned char)i;
  do_strbt_imm_132(d, &val_bt);
  printf("result is %u %u %u (should be %u %u %u)\n",
         d[131], d[132], d[133], 131, 255, 133);
  free(d);

  UInt val_ht = 0xFFFF;
  unsigned char* e = malloc(256);
  for (i = 0; i < 256; i++) e[i] = (unsigned char)i;
  do_strht_imm_132(e, &val_ht);
  printf("result is %u %u %u %u (should be %u %u %u %u)\n",
         e[131], e[132], e[133], e[134], 131, 255, 255, 134);
  free(e);

  UInt val_ldrbt = (200 << 0) | (150 << 8) | (137 << 16) | (10 << 24);
  printf("result is %u (should be %u)\n",
         do_ldrbt_imm_2((unsigned char*)&val_ldrbt), 137);

  UInt val_ldrsbt = (200 << 0) | (150 << 8) | (254 << 16) | (10 << 24);
  printf("result is %u (should be %llu)\n",
         do_ldrsbt_imm_2((unsigned char*)&val_ldrsbt), 4294967294ULL);


  UInt val_ldrht = 0xABFEFD8D;
  printf("result is %u (should be %u)\n",
         do_ldrht_imm_1((unsigned char*)(&val_ldrht)), 65277);

  UInt val_ldrsht = 0x00BADFAA;
  do_ldrsht_imm_1(&val_ldrsht);
  printf("result is 0x%x (should be 0x%x)\n", val_ldrsht, 0xFFFFBADF);

  return 0;
}
