#include <stdio.h>


typedef union
{
    double  dbl;
    long long bits[10];
} u_double;
u_double condition[2] = { {.bits = { 0x3FF0000000000001ULL, 0x3FF0000000000000ULL,
         0x00000000ffffffffULL, 0x1234563388994400ULL,
         0x0000004412369801ULL, 0x111111eeeeeee220ULL,
         0xAAAAABBBBBCCCDDDULL, 0xaa55cc2266dd2200ULL }}};
const double a[] = { 0.5, 0.0, 0.8, 0.10, 0.128, 5.0, 8.15, 9.456 };



#define TEST(instruction, LID, FD, FS, FDval, condition) \
{                                                        \
   unsigned int result;                                  \
   __asm__ volatile (                                    \
   ".set noreorder"                            "\n\t"    \
   "l.d $"#FD", %1"                            "\n\t"    \
   "l.d $"#FS", %2"                            "\n\t"    \
   "move $v0, $0"                              "\n\t"    \
   instruction " $"#FD", end21"instruction#LID "\n\t"    \
   "addiu $v0, 3"                              "\n\t"    \
   "addiu $v0, 11"                             "\n\t"    \
   "end12"instruction#LID":"                   "\n\t"    \
   "bal r_end"instruction#LID                  "\n\t"    \
   "addiu $v0, 19"                             "\n\t"    \
   "addiu $v0, 20"                             "\n\t"    \
   "end21"instruction#LID":"                   "\n\t"    \
   instruction" $"#FS", end12"instruction#LID  "\n\t"    \
   "addiu $v0, 7"                              "\n\t"    \
   "addiu $v0, 1"                              "\n\t"    \
   "r_end"instruction#LID ":"                  "\n\t"    \
   "move %0, $v0"                              "\n\t"    \
   : "=&r"(result)                                       \
   : "m" (FDval), "m" (condition), "r" (LID)             \
   : #FD, #FS, "$v0", "memory"                           \
   );                                                    \
   printf(instruction":: result: %x\n", result);         \
}

int main() {

#if (__mips_isa_rev>=6)
   printf("bc1eqz\n");
   TEST("bc1eqz",  0,  f0,  f1, a[0], condition[0]);
   TEST("bc1eqz",  1,  f1,  f2, a[0], condition[1]);
   TEST("bc1eqz",  3,  f2,  f3, a[1], condition[1]);
   TEST("bc1eqz",  4,  f3,  f4, a[1], condition[2]);
   TEST("bc1eqz",  6,  f4,  f5, a[2], condition[2]);
   TEST("bc1eqz",  7,  f5,  f6, a[2], condition[3]);
   TEST("bc1eqz",  8,  f6,  f7, a[3], condition[3]);
   TEST("bc1eqz",  9,  f7,  f8, a[3], condition[4]);
   TEST("bc1eqz", 10,  f8,  f9, a[4], condition[4]);
   TEST("bc1eqz", 11,  f9, f10, a[4], condition[5]);
   TEST("bc1eqz", 12, f10, f11, a[5], condition[5]);
   TEST("bc1eqz", 13, f11, f12, a[6], condition[5]);
   TEST("bc1eqz", 14, f12, f13, a[6], condition[6]);
   TEST("bc1eqz", 15, f13, f14, a[6], condition[7]);
   TEST("bc1eqz", 16, f14, f15, a[7], condition[7]);
   TEST("bc1eqz", 17, f15, f16, a[7], condition[8]);
   TEST("bc1eqz", 18, f16, f17, a[8], condition[8]);

   printf("\nbc1nez\n");
   TEST("bc1nez",  0,  f0,  f1, a[0], condition[0]);
   TEST("bc1nez",  1,  f1,  f2, a[0], condition[1]);
   TEST("bc1nez",  3,  f2,  f3, a[1], condition[1]);
   TEST("bc1nez",  4,  f3,  f4, a[1], condition[2]);
   TEST("bc1nez",  6,  f4,  f5, a[2], condition[2]);
   TEST("bc1nez",  7,  f5,  f6, a[2], condition[3]);
   TEST("bc1nez",  8,  f6,  f7, a[3], condition[3]);
   TEST("bc1nez",  9,  f7,  f8, a[3], condition[4]);
   TEST("bc1nez", 10,  f8,  f9, a[4], condition[4]);
   TEST("bc1nez", 11,  f9, f10, a[4], condition[5]);
   TEST("bc1nez", 12, f10, f11, a[5], condition[5]);
   TEST("bc1nez", 13, f11, f12, a[6], condition[5]);
   TEST("bc1nez", 14, f12, f13, a[6], condition[6]);
   TEST("bc1nez", 15, f13, f14, a[6], condition[7]);
   TEST("bc1nez", 16, f14, f15, a[7], condition[7]);
   TEST("bc1nez", 17, f15, f16, a[7], condition[8]);
   TEST("bc1nez", 18, f16, f17, a[8], condition[8]);
#endif

   return 0;
}


