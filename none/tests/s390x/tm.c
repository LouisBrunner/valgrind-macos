#include <stdio.h>

#define get_cc() \
({ \
   char __cc; \
   /* don't use IPM to better test spechelpers */ \
   asm volatile( "brc 8,1f\n\t" \
                 "brc 4,2f\n\t" \
                 "brc 2,3f\n\t" \
                 "brc 1,4f\n\t" \
                 "mvi %0,4\n\t" \
                 "j   0f\n\t" \
                 "1:  mvi %0,0\n\t" \
                 "j   0f\n\t" \
                 "2:  mvi %0,1\n\t" \
                 "j   0f\n\t" \
                 "3:  mvi %0,2\n\t" \
                 "j   0f\n\t" \
                 "4:  mvi %0,3\n\t" \
                 "j   0f\n\t" \
                 "0:  /* nop */ brc 0,0\n\t" \
                 : "=m" (__cc) : : "memory"); \
	__cc; \
})

void check_cc(int value, int cc)
{
   if (cc != value) {
      printf("wrong cc: ");
      if (value == 0) printf("expected 0  ");
      if (value == 1) printf("expected 1  ");
      if (value == 2) printf("expected 2  ");
      if (value == 3) printf("expected 3  ");
      if (cc == 0) printf("got 0");
      if (cc == 1) printf("got 1");
      if (cc == 2) printf("got 2");
      if (cc == 3) printf("got 3");
   } else {
      printf("OK");
   }
   printf("\n");
}

void tm(void)
{
   unsigned char v;

   /* test #1: value is zero */
   v = 0;
   asm volatile( "tm   %[v],15\n\t" : : [v] "R"(v) : "cc");
   check_cc(0, get_cc());

   /* test #2: mask is zero */
   v = 0xFF;
   asm volatile( "tm   %[v],0\n\t" : : [v] "R"(v) : "cc");
   check_cc(0, get_cc());

   /* test #3: selected bits are 0 */
   v = 0x0F;
   asm volatile( "tm   %[v],0xf0\n\t" : : [v] "R"(v) : "cc");
   check_cc(0, get_cc());

   /* test #4: selected bits are all 1 */
   v = 0xF0;
   asm volatile( "tm   %[v],0x70\n\t" : : [v] "R"(v) : "cc");
   check_cc(3, get_cc());

   /* test #5: selected bits are mixed */
   v = 0x0F;
   asm volatile( "tm   %[v],0x81\n\t" : : [v] "R"(v) : "cc");
   check_cc(1, get_cc());
}

int main(void)
{
   tm();

   return 0;
}
