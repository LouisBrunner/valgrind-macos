#include <assert.h>
#include <stdio.h>

/* Valid values (excluding sign code) */
static const unsigned long valid[] = {
   0x0000000000000000,
   0x0000000000000010,
   0x0000000000000120,
   0x0000000000001230,
   0x0000000000012340,
   0x0000000000123450,
   0x0000000001234560,
   0x0000000012345670,
   0x0000000123456780,
   0x0000001234567890,
   0x0000012345678900,
};

/* Boundary values (excluding sign code) */
static const unsigned long max = 0x0000021474836470;
static const unsigned long min = 0x0000021474836480;

/* Valid sign codes */
static const unsigned sign_code_pos[] = { 0xa, 0xc, 0xe, 0xf };
static const unsigned sign_code_neg[] = { 0xb, 0xd };

#define NUM_EL(x)   (sizeof(x) / sizeof(*(x)))

/* The value pointed to by ADDR is valid including sign code. */
static signed int
dec_to_hex(unsigned long *addr)
{
   long res;
   int res1, res2;

   res = 0;
   asm volatile("cvb %0,0(0,%1)"
                : "+d" (res) : "a" (addr) : "memory");

   // bits [0:31] ought to be unchanged
   // Catch bits that are set but shouldn't be
   assert((res >> 32) == 0);
   res1 = (int)res;

   res = -1;
   asm volatile("cvb %0,0(0,%1)"
                : "+d" (res) : "a" (addr) : "memory");

   // bits [0:31] ought to be unchanged
   // Catch bits that are cleared but shouldn't be
   assert((res >> 32) == -1);
   res2 = (int)(res & 0xffffffff);

   // Successful conversion
   assert(res1 == res2);

   return res1;
}

int main(void)
{
   for (int sign_code = 0xa; sign_code <= 0xf; ++sign_code) {
      printf("Testing in-range values with sign code 0x%x\n", sign_code);
      for (int i = 0; i < NUM_EL(valid); ++i) {
         unsigned long value = valid[i] | sign_code;
         printf("0x%016lx  -->  %d\n", value, dec_to_hex(&value));
      }
   }
   printf("\n");

   printf("Testing max. value 0x%lx\n", max >> 4);
   for (int i = 0; i < NUM_EL(sign_code_pos); ++i) {
      unsigned sign_code = sign_code_pos[i];
      unsigned long value = max | sign_code;
      printf("0x%016lx  -->  %d\n", value, dec_to_hex(&value));
   }
   printf("\n");

   printf("Testing min. value 0x%lx\n", min >> 4);
   for (int i = 0; i < NUM_EL(sign_code_neg); ++i) {
      unsigned sign_code = sign_code_neg[i];
      unsigned long value = min | sign_code;
      printf("0x%016lx  -->  %d\n", value, dec_to_hex(&value));
   }

   /* fixs390: check behaviour for invalid values, out-of-range values and values with invalid sign code */

   return 0;
}
