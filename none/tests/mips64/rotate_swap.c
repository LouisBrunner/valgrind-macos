#include <stdio.h>

#define TESTINST_DROTR(instruction, in, SA)        \
{                                                  \
   unsigned long long out;                         \
   __asm__ __volatile__(                           \
      "move        $t0, $zero"     "\n\t"          \
      "move        $t1, %1"        "\n\t"          \
      instruction" $t0, $t1, "#SA  "\n\t"          \
      "move        %0,  $t0"       "\n\t"          \
      : "=r" (out)                                 \
      : "r" (in)                                   \
      : "t0", "t1"                                 \
   );                                              \
   printf("%s :: in 0x%llx, out 0x%llx, SA %d\n",  \
          instruction, (long long) in, out, SA);   \
}

#define TESTINST_DROTRV(instruction, in, SA)       \
{                                                  \
   unsigned long long out;                         \
   __asm__ __volatile__(                           \
      "move        $t0, $zero"     "\n\t"          \
      "move        $t1, %1"        "\n\t"          \
      "move        $t2, %2"        "\n\t"          \
      instruction" $t0, $t1, $t2"  "\n\t"          \
      "move        %0,  $t0"       "\n\t"          \
      : "=r" (out)                                 \
      : "r" (in), "r" (SA)                         \
      : "t0", "t1", "t2"                           \
   );                                              \
   printf("%s :: in 0x%llx, out 0x%llx, SA %d\n",  \
          instruction, (long long) in, out, SA);   \
}

#define TESTINST_DSWAP(instruction, in)       \
{                                             \
   unsigned long long out;                    \
   __asm__ __volatile__(                      \
      "move        $t0, $0"   "\n\t"          \
      "move        $t1, $0"   "\n\t"          \
      "move        $t1, %1"   "\n\t"          \
      instruction" $t0, $t1"  "\n\t"          \
      "move        %0,  $t0"  "\n\t"          \
      : "=r" (out)                            \
      : "r" (in)                              \
      : "t0", "t1"                            \
   );                                         \
   printf("%s :: in 0x%llx, out 0x%llx\n",    \
          instruction, (long long) in, out);  \
}

int main()
{
#if (__mips == 64) && (__mips_isa_rev >= 2)
   printf("--- DROTR ---\n");
   TESTINST_DROTR("drotr", 0x2000ffffffffffff, 16);
   TESTINST_DROTR("drotr", 0xffff0000ffffffff, 16);
   TESTINST_DROTR("drotr", 0x2000ffffffffffff, 8);
   TESTINST_DROTR("drotr", 0x2000ffffffffffff, 4);
   TESTINST_DROTR("drotr", 0x2000ffffffffffff, 5);
   TESTINST_DROTR("drotr", 0x31415927ffffffff, 10);
   TESTINST_DROTR("drotr", 0x2000ffffffffffff, 4);
   TESTINST_DROTR("drotr", 0x2000ffffffffffff, 0);
   TESTINST_DROTR("drotr", 0xeeeeffffffffffff, 16);
   TESTINST_DROTR("drotr", 0x2000ffffffffbbbb, 31);
   TESTINST_DROTR("drotr", 0x2000ffffffffffff, 16);
   TESTINST_DROTR("drotr", 0x2000ffffffffffff, 0);
   TESTINST_DROTR("drotr", 0x7fffffffffffffff, 16);
   TESTINST_DROTR("drotr", 0x2000ffffffffffff, 2);
   TESTINST_DROTR("drotr", 0x2000ffffffffffff, 24);
   TESTINST_DROTR("drotr", 0xfffffff31415927f, 16);
   TESTINST_DROTR("drotr", 0xffffffffffff0008, 3);
   TESTINST_DROTR("drotr", 0xffff0000ffffffff, 16);
   TESTINST_DROTR("drotr", 0xff0000ffffffffff, 16);
   TESTINST_DROTR("drotr", 0xfffffffff0000fff, 16);

   printf("--- DROTR32 ---\n");
   TESTINST_DROTR("drotr32", 0x2000ffffffffffff, 16);
   TESTINST_DROTR("drotr32", 0xffff0000ffffffff, 16);
   TESTINST_DROTR("drotr32", 0x2000ffffffffffff, 8);
   TESTINST_DROTR("drotr32", 0x2000ffffffffffff, 4);
   TESTINST_DROTR("drotr32", 0x2000ffffffffffff, 5);
   TESTINST_DROTR("drotr32", 0x31415927ffffffff, 10);
   TESTINST_DROTR("drotr32", 0x2000ffffffffffff, 4);
   TESTINST_DROTR("drotr32", 0x2000ffffffffffff, 0);
   TESTINST_DROTR("drotr32", 0xeeeeffffffffffff, 16);
   TESTINST_DROTR("drotr32", 0x2000ffffffffffff, 31);
   TESTINST_DROTR("drotr32", 0x2000ffffffffbbbb, 16);
   TESTINST_DROTR("drotr32", 0x2000ffffffffffff, 0);
   TESTINST_DROTR("drotr32", 0x7fffffffffffffff, 16);
   TESTINST_DROTR("drotr32", 0x2000ffffffffffff, 2);
   TESTINST_DROTR("drotr32", 0x2000ffffffffffff, 24);
   TESTINST_DROTR("drotr32", 0xfffffff31415927f, 16);
   TESTINST_DROTR("drotr32", 0xffffffffffff0008, 3);
   TESTINST_DROTR("drotr32", 0xffff0000ffffffff, 16);
   TESTINST_DROTR("drotr32", 0xff0000ffffffffff, 16);
   TESTINST_DROTR("drotr32", 0xfffffffff0000fff, 16);

   printf("--- DROTRV ---\n");
   TESTINST_DROTRV("drotrv", 0x2000ffffffffffff, 16);
   TESTINST_DROTRV("drotrv", 0xffff0000ffffffff, 16);
   TESTINST_DROTRV("drotrv", 0x2000ffffffffffff, 8);
   TESTINST_DROTRV("drotrv", 0x2000ffffffffffff, 4);
   TESTINST_DROTRV("drotrv", 0x2000ffffffffffff, 5);
   TESTINST_DROTRV("drotrv", 0x31415927ffffffff, 10);
   TESTINST_DROTRV("drotrv", 0x2000ffffffffffff, 4);
   TESTINST_DROTRV("drotrv", 0x2000ffffffffffff, 0);
   TESTINST_DROTRV("drotrv", 0xeeeeffffffffffff, 16);
   TESTINST_DROTRV("drotrv", 0x2000ffffffffffff, 31);
   TESTINST_DROTRV("drotrv", 0x2000ffffffffbbbb, 16);
   TESTINST_DROTRV("drotrv", 0x2000ffffffffffff, 0);
   TESTINST_DROTRV("drotrv", 0x7fffffffffffffff, 16);
   TESTINST_DROTRV("drotrv", 0x2000ffffffffffff, 2);
   TESTINST_DROTRV("drotrv", 0x2000ffffffffffff, 24);
   TESTINST_DROTRV("drotrv", 0xfffffff31415927f, 16);
   TESTINST_DROTRV("drotrv", 0xffffffffffff0008, 3);
   TESTINST_DROTRV("drotrv", 0xffff0000ffffffff, 16);
   TESTINST_DROTRV("drotrv", 0xff0000ffffffffff, 16);
   TESTINST_DROTRV("drotrv", 0xfffffffff0000fff, 16);

   printf("--- DSBH ---\n");
   TESTINST_DSWAP("dsbh", 0x2000ffffffffffff);
   TESTINST_DSWAP("dsbh", 0xffff0000ffffffff);
   TESTINST_DSWAP("dsbh", 0x2000ffffffffffff);
   TESTINST_DSWAP("dsbh", 0x2000ffffeeeeffff);
   TESTINST_DSWAP("dsbh", 0x2000ffffffffffff);
   TESTINST_DSWAP("dsbh", 0x31415927ffffffff);
   TESTINST_DSWAP("dsbh", 0x2000ffffffffffff);
   TESTINST_DSWAP("dsbh", 0x2000ffffffccccff);
   TESTINST_DSWAP("dsbh", 0xeeeeffffffffffff);
   TESTINST_DSWAP("dsbh", 0x2000ffff0000ffff);
   TESTINST_DSWAP("dsbh", 0x2000ffffffffbbbb);
   TESTINST_DSWAP("dsbh", 0x2000ffffffffffff);
   TESTINST_DSWAP("dsbh", 0x7fffffff5555ffff);
   TESTINST_DSWAP("dsbh", 0x2000ffffff123123);
   TESTINST_DSWAP("dsbh", 0x2000ffffffffffff);
   TESTINST_DSWAP("dsbh", 0xfffffff31415927f);
   TESTINST_DSWAP("dsbh", 0xffffffffffff0008);
   TESTINST_DSWAP("dsbh", 0xffff0000ffff88ff);
   TESTINST_DSWAP("dsbh", 0xff0000ffffffffff);
   TESTINST_DSWAP("dsbh", 0xfff10ffff0000fff);

   printf("--- DSHD ---\n");
   TESTINST_DSWAP("dshd", 0x2002ffffffffffff);
   TESTINST_DSWAP("dshd", 0xffff0000ffffffff);
   TESTINST_DSWAP("dshd", 0x2000ffffffffffff);
   TESTINST_DSWAP("dshd", 0x2000ffffffddddff);
   TESTINST_DSWAP("dshd", 0x2000ffffffffeeee);
   TESTINST_DSWAP("dshd", 0x31415927ffffffff);
   TESTINST_DSWAP("dshd", 0x2000ffffffffaaaa);
   TESTINST_DSWAP("dshd", 0x2000ffffffbbbbff);
   TESTINST_DSWAP("dshd", 0xeeeeff33ff22ffff);
   TESTINST_DSWAP("dshd", 0x2000ffffffffffff);
   TESTINST_DSWAP("dshd", 0x2000ffffffffbbbb);
   TESTINST_DSWAP("dshd", 0x2000ffffffffffff);
   TESTINST_DSWAP("dshd", 0x7fffffffddddffff);
   TESTINST_DSWAP("dshd", 0x2000ffffffff2222);
   TESTINST_DSWAP("dshd", 0x2000ffffffffffff);
   TESTINST_DSWAP("dshd", 0xfffffff31415927f);
   TESTINST_DSWAP("dshd", 0xffffffffffff0008);
   TESTINST_DSWAP("dshd", 0xffff0000ffffffff);
   TESTINST_DSWAP("dshd", 0xff0000ffffffffff);
   TESTINST_DSWAP("dshd", 0xfffffffff0000fff);

   printf("--- WSBH ---\n");
   TESTINST_DSWAP("wsbh", 0x2000ffffffffffff);
   TESTINST_DSWAP("wsbh", 0xffff0000ffffffff);
   TESTINST_DSWAP("wsbh", 0x2000ffffffffffff);
   TESTINST_DSWAP("wsbh", 0x2000ffffeeeeffff);
   TESTINST_DSWAP("wsbh", 0x2000ffffffffffff);
   TESTINST_DSWAP("wsbh", 0x31415927ffffffff);
   TESTINST_DSWAP("wsbh", 0x2000ffffffffffff);
   TESTINST_DSWAP("wsbh", 0x2000ffffffccccff);
   TESTINST_DSWAP("wsbh", 0xeeeeffffffffffff);
   TESTINST_DSWAP("wsbh", 0x2000ffff0000ffff);
   TESTINST_DSWAP("wsbh", 0x2000ffffffffbbbb);
   TESTINST_DSWAP("wsbh", 0x2000ffffffffffff);
   TESTINST_DSWAP("wsbh", 0x7fffffff5555ffff);
   TESTINST_DSWAP("wsbh", 0x2000ffffff123123);
   TESTINST_DSWAP("wsbh", 0x2000ffffffffffff);
   TESTINST_DSWAP("wsbh", 0xfffffff31415927f);
   TESTINST_DSWAP("wsbh", 0xffffffffffff0008);
   TESTINST_DSWAP("wsbh", 0xffff0000ffff88ff);
   TESTINST_DSWAP("wsbh", 0xff0000ffffffffff);
   TESTINST_DSWAP("wsbh", 0xfff10ffff0000fff);
#else
   printf("This test is testing mips64r2 instructions.\n");
#endif

   return 0;
}
