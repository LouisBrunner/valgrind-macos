// This should be compiled as ARM code.

#include <stdio.h>
#include <malloc.h>

typedef unsigned int UInt;

__attribute__((noinline)) UInt do_ldrt_A1_imm_132 ( unsigned char* p )
{
  UInt res;
  __asm__ __volatile__(
  // Twice do ldrt for testing post-indexed.
     "mov r5, %1 ; ldrt r6, [r5], #+132 ; ldrt r6, [r5], #132 ; mov %0, r6"
      : "=r"(res) : "r"(p) : "r5", "r6"
  );
  return res;
}

__attribute__((noinline)) UInt do_ldrt_A2_imm_132 ( unsigned char* p )
{
  UInt res;
  __asm__ __volatile__(
  // Twice ldrt for testing post-indexed.
     "mov r5, %1 ; mov r6, #33; ldrt r7, [r5], +r6, lsl #2 ; ldrt r7, [r5], +r6, lsl #2 ; mov %0, r7"
      : "=r"(res) : "r"(p) : "r5", "r6", "r7"
  );
  return res;
}

__attribute__((noinline)) UInt do_ldrbt_A1_imm_2 (unsigned char* val)
{
  UInt res;
  __asm__ __volatile__(
  // Twice ldrbt for testing post-indexed.
     "mov r4, %1 ; ldrbt r5, [r4], #+2; ldrbt r5, [r4], #+2; mov %0, r5"
     : "=r"(res) : "r"(val) : "r4", "r5"
  );
  return res;
}

__attribute__((noinline)) UInt do_ldrbt_A2_imm_2 (unsigned char* val)
{
  UInt res;
  __asm__ __volatile__(
  // Twice ldrbt for testing post-indexed.
     "mov r4, %1 ; mov r5, #4; ldrbt r6, [r4], +r5, lsr #2 ; ldrbt r6, [r4], +r5, lsr #2; mov %0, r6"
     : "=r"(res) : "r"(val) : "r4", "r5", "r6"
  );
  return res;
}

__attribute__((noinline)) UInt do_ldrht_A1_imm_m2 (unsigned char* val)
{
  UInt res;
  __asm__ __volatile__(
  // Twice ldrht for testing post-indexed.
     "mov r4, %1 ; ldrht r5, [r4], #-2 ; ldrht r5, [r4], #-2 ; mov %0, r5"
     : "=r"(res) : "r"(val) : "r4", "r5"
  );
  return res;
}

__attribute__((noinline)) UInt do_ldrht_A2_imm_m2 (unsigned char* val)
{
  UInt res;
  __asm__ __volatile__(
  // Twice ldrht for testing post-indexed.
     "mov r4, %1 ; mov r5, #2 ; ldrht r6, [r4], -r5 ; ldrht r6, [r4], -r5 ; mov %0, r6"
     : "=r"(res) : "r"(val) : "r4", "r5", "r6"
  );
  return res;
}

__attribute__((noinline)) UInt do_ldrsht_A1_imm_m2 (unsigned char* val)
{
  UInt res;
  __asm__ __volatile__(
  // Twice ldrsht for testing post-indexed.
     "mov r4, %1 ; ldrsht r5, [r4], #-2 ; ldrsht r5, [r4], #-2 ; mov %0, r5"
     : "=r"(res) : "r"(val) : "r4", "r5"
  );
  return res;
}

__attribute__((noinline)) UInt do_ldrsht_A2_imm_1 (unsigned char* val)
{
  UInt res;
  __asm__ __volatile__(
  // Twice ldrsht for testing post-indexed.
     "mov r4, %1 ; mov r5, #1 ; ldrsht r6, [r4], r5 ; ldrsht r6, [r4], r5 ; mov %0, r6"
     : "=r"(res) : "r"(val) : "r4", "r5", "r6"
  );
  return res;
}

__attribute__((noinline)) UInt do_ldrsbt_A1_imm_1 (unsigned char* val)
{
  UInt res;
  __asm__ __volatile__(
  // Twice ldrsbt for testing post-indexed.
     "mov r4, %1 ; ldrsbt r5, [r4], #1 ; ldrsbt r5, [r4], #1 ; mov %0, r5"
     : "=r"(res) : "r"(val) : "r4", "r5"
  );
  return res;
}

__attribute__((noinline)) UInt do_ldrsbt_A2_imm_1 (unsigned char* val)
{
  UInt res;
  __asm__ __volatile__(
  // Twice ldrsbt for testing post-indexed.
     "mov r4, %1 ; mov r5, #1 ; ldrsbt r6, [r4], r5 ; ldrsbt r6, [r4], r5 ; mov %0, r6"
     : "=r"(res) : "r"(val) : "r4", "r5", "r6"
  );
  return res;
}

__attribute__((noinline)) void do_strbt_A1_imm_1 (unsigned char* val)
{
  __asm__ __volatile__(
  // Twice strbt for testing post-indexed.
     "mov r4, %0 ; mov r5, #0xCD ; strbt r5, [r4], #1 ; strbt r5, [r4], #1"
     : : "r"(val) : "r4", "r5"
  );
}

__attribute__((noinline)) void do_strbt_A2_imm_1 (unsigned char* val)
{
  __asm__ __volatile__(
  // Twice strbt for testing post-indexed.
     "mov r4, %0 ; mov r5, #1 ; mov r6, #0xCD ; strbt r6, [r4], r5, LSL #1 ; strbt r6, [r4], r5"
     : : "r"(val) : "r4", "r5", "r6"
  );
}

__attribute__((noinline)) void do_strht_A1_imm_1 (unsigned char* val)
{
  __asm__ __volatile__(
  // Twice strht for testing post-indexed.
     "mov r4, %0 ; ldr r5, =0xABCD ; strht r5, [r4], #1 ; strht r5, [r4], #1"
     : : "r"(val) : "r4", "r5"
  );
}

__attribute__((noinline)) void do_strht_A2_imm_1 (unsigned char* val)
{
  __asm__ __volatile__(
  // Twice strht for testing post-indexed.
     "mov r4, %0 ; mov r5, #1 ; ldr r6, =0xDCBA ; strht r6, [r4], r5  ; strht r6, [r4], r5"
     : : "r"(val) : "r4", "r5", "r6"
  );
}

__attribute__((noinline)) void do_strt_A1_imm_4 (unsigned char* val)
{
  __asm__ __volatile__(
  // Twice strt for testing post-indexed.
     "mov r4, %0 ; ldr r5, =0xABCDEFFF ; strt r5, [r4], #4  ; ldr r5, =0x01234567 ; strt r5, [r4], #0"
     : : "r"(val) : "r4", "r5"
  );
}

__attribute__((noinline)) void do_strt_A2_imm_4 (unsigned char* val)
{
  __asm__ __volatile__(
  // Twice strt for testing post-indexed.
     "mov r4, %0 ; mov r5, #2 ; ldr r6, =0xFFFEDCBA ; strt r6, [r4], -r5, LSL #1  ; mov r5, #0 ; ldr r6, =0x76543210 ; strt r6, [r4], +r5"
     : : "r"(val) : "r4", "r5", "r6"
  );
}

int main ( void )
{
  UInt i;
  unsigned char* b = malloc(256);
  for (i = 0; i < 256; i++) b[i] = (unsigned char)i;
  UInt r = do_ldrt_A1_imm_132(b);
  free(b);
  printf("result is 0x%08x (should be 0x%08x)\n", r, 0x87868584);

  UInt j;
  unsigned char* c = malloc(256);
  for (j = 0; j < 256; j++) c[j] = (unsigned char)j;
  UInt k = do_ldrt_A2_imm_132(c);
  free(c);
  printf("result is 0x%08x (should be 0x%08x)\n", k, 0x87868584);

  UInt val_ldrbt = (200 << 0) | (150 << 8) | (137 << 16) | (10 << 24);
  printf("result is %u (should be %u)\n", do_ldrbt_A1_imm_2((unsigned char*)&val_ldrbt), 137);

  UInt val_ldrbt_A2 = (200 << 0) | (150 << 8) | (137 << 16) | (10 << 24);
  printf("result is %u (should be %u)\n", do_ldrbt_A2_imm_2((unsigned char*)&val_ldrbt_A2), 150);

  UInt val_ldrht_A1 = (0xC8 << 0) | (0xF6 << 8) | (0xBB << 16) | (0x0A << 24);
  printf("result is %u (should be %u)\n", do_ldrht_A1_imm_m2((unsigned char*)&val_ldrht_A1 + 2), 0x0000F6C8);

  UInt val_ldrht_A2 = (0xC8 << 0) | (0xF7 << 8) | (0xBB << 16) | (0x0A << 24);
  printf("result is %u (should be %u)\n", do_ldrht_A2_imm_m2((unsigned char*)&val_ldrht_A2 + 2), 0x0000F7C8);

  UInt val_ldrsht_A1 = (0x08 << 0) | (0xFC << 8) | (0x0B << 16) | (0x0A << 24);
  printf("result is %u (should be %u)\n", do_ldrsht_A1_imm_m2((unsigned char*)&val_ldrsht_A1 + 2), 0xFFFFFC08);

  UInt val_ldrsht_A2 = (0xC8 << 0) | (0x0B << 8) | (0xFB << 16) | (0x0A << 24);
  printf("result is %u (should be %u)\n", do_ldrsht_A2_imm_1((unsigned char*)&val_ldrsht_A2), 0xFFFFFB0B);

  UInt val_ldrsbt_A1 = (0xC8 << 0) | (0xF0 << 8) | (0xCD << 16) | (0x0A << 24);
  printf("result is %u (should be %u)\n", do_ldrsbt_A1_imm_1((unsigned char*)&val_ldrsbt_A1), 0xFFFFFFF0);

  UInt val_ldrsbt_A2 = (0xC8 << 0) | (0xF0 << 8) | (0xFD << 16) | (0x0A << 24);
  printf("result is %u (should be %u)\n", do_ldrsbt_A2_imm_1((unsigned char*)&val_ldrsbt_A2 + 1), 0xFFFFFFFD);

  UInt val_strbt_A1 = (0xC8 << 0) | (0xF0 << 8) | (0xFD << 16) | (0x0A << 24);
  do_strbt_A1_imm_1((unsigned char*)&val_strbt_A1);
  printf("result is %u (should be %u)\n", val_strbt_A1, 0x0AFDCDCD);

  UInt val_strbt_A2 = (0xC8 << 0) | (0xF0 << 8) | (0xFD << 16) | (0x0A << 24);
  do_strbt_A2_imm_1((unsigned char*)&val_strbt_A2);
  printf("result is %u (should be %u)\n", val_strbt_A2, 0x0ACDF0CD);

  UInt val_strht_A1 = 0xFFFFFFFF;
  do_strht_A1_imm_1((unsigned char*)&val_strht_A1);
  printf("result is %u (should be %u)\n", val_strht_A1, 0xFFABCDCD);

  UInt val_strht_A2 = 0xFFFFFFFF;
  do_strht_A2_imm_1((unsigned char*)&val_strht_A2);
  printf("result is %u (should be %u)\n", val_strht_A2, 0xFFDCBABA);

  UInt val_strt_A1[2] = {0xFFFFFFFF, 0xFFFFFFFF};
  do_strt_A1_imm_4((unsigned char*)&val_strt_A1[0]);
  printf("result is %u %u (should be %u %u)\n", val_strt_A1[0], val_strt_A1[1], 0xABCDEFFF, 0x01234567);

  UInt val_strt_A2[2] = {0xFFFFFFFF, 0xFFFFFFFF};
  do_strt_A2_imm_4((unsigned char*)&val_strt_A2[1]);
  printf("result is %u %u (should be %u %u)\n", val_strt_A2[0], val_strt_A2[1], 0x76543210, 0xFFFEDCBA);

  return 0;
}
