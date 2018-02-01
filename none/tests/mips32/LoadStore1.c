#include <stdio.h>

unsigned int mem[] = {
   0x121f1e1f, 0, 3, -1,
   0x232f2e2f, 0x242c2b2b, 0x252a2e2b, 0x262d2d2a,
   0x3f343f3e, 0x3e353d3c, 0x363a3c3b, 0x3b373b3a,
   0x454f4e45, 0x4e464d46, 0x474d474c, 0x4a484a4c
};

unsigned int mem1[] = {
   0, 0, 0, 0,
   0, 0, 0, 0,
   0xffffffff, 0, 0, 0,
   0, 0, 0, 0
};

unsigned int mem2[] = {
0x0000e680, 0x00010700, 0x0000e7dc, 0x0000b0d0,
0x2ab05fd0, 0x0000b6a0, 0x0000be80, 0x0000de10,
0x0000df20, 0x2ab05fe0, 0x0000dfd0, 0x00010300
};

// sb $t0, 0($t1)
#define TESTINST1(instruction, RTval, offset, RT, RS) \
{                                                     \
   unsigned int out;                                  \
   __asm__ volatile(                                  \
      "move $" #RS", %1                         \n\t" \
      "li $" #RT", " #RTval"                    \n\t" \
      instruction "                             \n\t" \
      "lw %0, "#offset"($"#RS")                 \n\t" \
      : "=&r" (out)                                   \
      : "r" (mem1), "r" (RTval)                       \
      : #RS, #RT, "memory"                            \
   );                                                 \
   printf("%s :: RTval: 0x%x, out: 0x%x\n",           \
          instruction, RTval, out);                   \
   out = 0;                                           \
   __asm__ volatile(                                  \
      "move $" #RS", %1                         \n\t" \
      "li $" #RT", " #RTval "                   \n\t" \
      instruction "                             \n\t" \
      "lw %0, "#offset"($"#RS")                 \n\t" \
      : "=&r" (out)                                   \
      : "r" (mem), "r" (RTval)                        \
      : #RS, #RT, "memory"                            \
   );                                                 \
   printf("%s :: RTval: 0x%x, out: 0x%x\n",           \
          instruction, RTval, out);                   \
}

// swl $t0, 3($t1)
// swr $t0, 0($t1)
#define TESTINSTsw(RTval, offset, RT, RS)             \
{                                                     \
   unsigned int out;                                  \
   __asm__ volatile(                                  \
      "move $" #RS", %1\n\t"                          \
      "addiu $"#RS", $"#RS", "#offset"          \n\t" \
      "li $" #RT", " #RTval"                    \n\t" \
      "swl $t0, 3($t1)                          \n\t" \
      "swr $t0, 0($t1)                          \n\t" \
      "lw %0, 0($"#RS")                         \n\t" \
      : "=&r" (out)                                   \
      : "r" (mem2), "r" (RTval)                       \
      : #RT, #RS, "memory"                            \
   );                                                 \
   printf("swl $t0, 3($t1)\nswr $t0, 0($t1)\n"        \
          " :: RTval: 0x%x, out: 0x%x\n",             \
          RTval, out);                                \
}

void ppMem(unsigned int* m, int len)
{
   int i;
   printf("MEM1:\n");
   for (i = 0; i < len; i=i+4)
   {
      printf("0x%x, 0x%x, 0x%x, 0x%x\n", m[i], m[i+1], m[i+2], m[i+3]);
      m[i] = 0;
      m[i+1] = 0;
      m[i+2] = 0;
      m[i+3] = 0;
      if (i == 2) 
      {
         m[i] = 0xffffffff;
         m[i+1] = 0;
         m[i+2] = 0;
         m[i+3] = 0;
      }
   }
}

void ppMem1(unsigned int* m, int len)
{
   int i;
   printf("MEM:\n");
   for (i = 0; i < len; i=i+4)
   {
      printf("0x%x, 0x%x, 0x%x, 0x%x\n", m[i], m[i+1], m[i+2], m[i+3]);
   }
   m[0] = 0x121f1e1f;
   m[1] = 0;
   m[2] = 3;
   m[3] = -1;
   m[4] = 0x232f2e2f;
   m[5] = 0x242c2b2b;
   m[6] = 0x252a2e2b;
   m[7] = 0x262d2d2a;
   m[8] = 0x3f343f3e;
   m[9] = 0x3e353d3c;
   m[10] = 0x363a3c3b;
   m[11] = 0x3b373b3a;
   m[12] = 0x454f4e45;
   m[13] = 0x4e464d46;
   m[14] = 0x474d474c;
   m[15] = 0x4a484a4c;
}

void ppMem0(unsigned int* m, int len)
{
   int i;
   printf("MEM:\n");
   for (i = 0; i < len; i=i+4)
   {
      printf("0x%x, 0x%x, 0x%x, 0x%x\n", m[i], m[i+1], m[i+2], m[i+3]);
   }

   m[0] = 0x0000e680;
   m[1] = 0x00010700;
   m[2] = 0x0000e7dc;
   m[3] = 0x0000b0d0;
   m[4] = 0x2ab05fd0;
   m[5] = 0x0000b6a0;
   m[6] = 0x0000be80;
   m[7] = 0x0000de10;
   m[8] = 0x0000df20;
   m[9] = 0x2ab05fe0;
   m[10] = 0x0000dfd0;
   m[11] = 0x00010300;
}

int main()
{
   printf("sb\n");
   TESTINST1("sb $t0, 3($t1)", 0, 3, t0, t1);
   TESTINST1("sb $t0, 5($t1)", 0x31415927, 5, t0, t1);
   TESTINST1("sb $t0, 7($t1)", 0x7fffffff, 7, t0, t1);
   TESTINST1("sb $t0, 1($t1)", 0x80000000, 1, t0, t1);
   TESTINST1("sb $t0, 2($t1)", 0x80000000, 2, t0, t1);
   TESTINST1("sb $t0, 6($t1)", 0x7fffffff, 6, t0, t1);
   TESTINST1("sb $t0, 11($t1)", 0x7fffffff, 11, t0, t1);
   TESTINST1("sb $t0, 8($t1)", -1, 8, t0, t1);
   TESTINST1("sb $t0, 0($t1)", 0x31415927, 0, t0, t1);
   TESTINST1("sb $t0, 0($t1)", 0x0dd00000, 0, t0, t1);
   TESTINST1("sb $t0, 0($t1)", 655, 0, t0, t1);
   TESTINST1("sb $t0, 0($t1)", -655, 0, t0, t1);
   TESTINST1("sb $t0, 0($t1)", 15, 0, t0, t1);
   TESTINST1("sb $t0, 0($t1)", 1, 0, t0, t1);
   TESTINST1("sb $t0, 0($t1)", 53, 0, t0, t1);
   TESTINST1("sb $t0, 2($t1)", 0xffffffff, 2, t0, t1);
   TESTINST1("sb $t0, 2($t1)", 0xffffffff, 2, t0, t1);
   TESTINST1("sb $t0, 31($t1)", 0xffffffff, 31, t0, t1);
   TESTINST1("sb $t0, 35($t1)", 0xffffffff, 35, t0, t1);
   TESTINST1("sb $t0, 41($t1)", 0x31415927, 41, t0, t1);
   TESTINST1("sb $t0, 42($t1)", 0x7fffffff, 42, t0, t1);
   TESTINST1("sb $t0, 45($t1)", 0x80000000, 45, t0, t1);
   TESTINST1("sb $t0, 51($t1)", 655, 51, t0, t1);
   ppMem(mem1, 16);
   ppMem1(mem, 16);

   printf("sh\n");
   TESTINST1("sh $t0, 1($t1)", 0, 1, t0, t1);
   TESTINST1("sh $t0, 3($t1)", 0x31415927, 3, t0, t1);
   TESTINST1("sh $t0, 5($t1)", 0x7fffffff, 5, t0, t1);
   TESTINST1("sh $t0, 7($t1)", 0x80000000, 7, t0, t1);
   TESTINST1("sh $t0, 9($t1)", 0x80000000, 9, t0, t1);
   TESTINST1("sh $t0, 6($t1)", 0x7fffffff, 6, t0, t1);
   TESTINST1("sh $t0, 11($t1)", 0x7fffffff, 11, t0, t1);
   TESTINST1("sh $t0, 8($t1)", -1, 8, t0, t1);
   TESTINST1("sh $t0, 0($t1)", 0x31415927, 0, t0, t1);
   TESTINST1("sh $t0, 0($t1)", 0x0dd00000, 0, t0, t1);
   TESTINST1("sh $t0, 0($t1)", 655, 0, t0, t1);
   TESTINST1("sh $t0, 0($t1)", -655, 0, t0, t1);
   TESTINST1("sh $t0, 0($t1)", 15, 0, t0, t1);
   TESTINST1("sh $t0, 0($t1)", 1, 0, t0, t1);
   TESTINST1("sh $t0, 0($t1)", 53, 0, t0, t1);
   TESTINST1("sh $t0, 2($t1)", 0xffffffff, 2, t0, t1);
   TESTINST1("sh $t0, 2($t1)", 0xffffffff, 2, t0, t1);
   TESTINST1("sh $t0, 31($t1)", 0xffffffff, 31, t0, t1);
   TESTINST1("sh $t0, 35($t1)", 0xffffffff, 35, t0, t1);
   TESTINST1("sh $t0, 41($t1)", 0x31415927, 41, t0, t1);
   TESTINST1("sh $t0, 47($t1)", 0x7fffffff, 47, t0, t1);
   TESTINST1("sh $t0, 49($t1)", 0x80000000, 49, t0, t1);
   TESTINST1("sh $t0, 51($t1)", 655, 51, t0, t1);
   ppMem(mem1, 16);
   ppMem1(mem, 16);

   printf("sw\n");
   TESTINST1("sw $t0, 1($t1)", 0, 1, t0, t1);
   TESTINST1("sw $t0, 3($t1)", 0x31415927, 3, t0, t1);
   TESTINST1("sw $t0, 5($t1)", 0x7fffffff, 5, t0, t1);
   TESTINST1("sw $t0, 7($t1)", 0x80000000, 7, t0, t1);
   TESTINST1("sw $t0, 9($t1)", 0x80000000, 9, t0, t1);
   TESTINST1("sw $t0, 6($t1)", 0x7fffffff, 6, t0, t1);
   TESTINST1("sw $t0, 15($t1)", 0x7fffffff, 11, t0, t1);
   TESTINST1("sw $t0, 8($t1)", -1, 8, t0, t1);
   TESTINST1("sw $t0, 0($t1)", 0x31415927, 0, t0, t1);
   TESTINST1("sw $t0, 0($t1)", 0x0dd00000, 0, t0, t1);
   TESTINST1("sw $t0, 0($t1)", 655, 0, t0, t1);
   TESTINST1("sw $t0, 0($t1)", -655, 0, t0, t1);
   TESTINST1("sw $t0, 0($t1)", 15, 0, t0, t1);
   TESTINST1("sw $t0, 0($t1)", 1, 0, t0, t1);
   TESTINST1("sw $t0, 0($t1)", 53, 0, t0, t1);
   TESTINST1("sw $t0, 2($t1)", 0xffffffff, 2, t0, t1);
   TESTINST1("sw $t0, 2($t1)", 0xffffffff, 2, t0, t1);
   TESTINST1("sw $t0, 31($t1)", 0xffffffff, 31, t0, t1);
   TESTINST1("sw $t0, 37($t1)", 0xffffffff, 37, t0, t1);
   TESTINST1("sw $t0, 49($t1)", 0x31415927, 49, t0, t1);
   TESTINST1("sw $t0, 41($t1)", 0x7fffffff, 41, t0, t1);
   TESTINST1("sw $t0, 43($t1)", 0x80000000, 43, t0, t1);
   TESTINST1("sw $t0, 51($t1)", 655, 51, t0, t1);
   ppMem(mem1, 16);
   ppMem1(mem, 16);

#if (__mips_isa_rev < 6)
   printf("swl\n");
   TESTINST1("swl $t0, 1($t1)", 0, 1, t0, t1);
   TESTINST1("swl $t0, 3($t1)", 0x31415927, 3, t0, t1);
   TESTINST1("swl $t0, 5($t1)", 0x7fffffff, 5, t0, t1);
   TESTINST1("swl $t0, 7($t1)", 0x80000000, 7, t0, t1);
   TESTINST1("swl $t0, 9($t1)", 0x80000000, 9, t0, t1);
   TESTINST1("swl $t0, 6($t1)", 0x7fffffff, 6, t0, t1);
   TESTINST1("swl $t0, 11($t1)", 0x7fffffff, 11, t0, t1);
   TESTINST1("swl $t0, 8($t1)", -1, 8, t0, t1);
   TESTINST1("swl $t0, 0($t1)", 0x31415927, 0, t0, t1);
   TESTINST1("swl $t0, 0($t1)", 0x0dd00000, 0, t0, t1);
   TESTINST1("swl $t0, 0($t1)", 655, 0, t0, t1);
   TESTINST1("swl $t0, 0($t1)", -655, 0, t0, t1);
   TESTINST1("swl $t0, 0($t1)", 15, 0, t0, t1);
   TESTINST1("swl $t0, 0($t1)", 1, 0, t0, t1);
   TESTINST1("swl $t0, 0($t1)", 53, 0, t0, t1);
   TESTINST1("swl $t0, 2($t1)", 0xffffffff, 2, t0, t1);
   TESTINST1("swl $t0, 2($t1)", 0xffffffff, 2, t0, t1);
   TESTINST1("swl $t0, 33($t1)", 0xffffffff, 33, t0, t1);
   TESTINST1("swl $t0, 35($t1)", 0xffffffff, 35, t0, t1);
   TESTINST1("swl $t0, 41($t1)", 0x31415927, 41, t0, t1);
   TESTINST1("swl $t0, 45($t1)", 0x7fffffff, 45, t0, t1);
   TESTINST1("swl $t0, 49($t1)", 0x80000000, 49, t0, t1);
   TESTINST1("swl $t0, 51($t1)", 655, 51, t0, t1);
   ppMem(mem1, 16);
   ppMem1(mem, 16);

   printf("swr\n");
   TESTINST1("swr $t0, 1($t1)", 0, 1, t0, t1);
   TESTINST1("swr $t0, 3($t1)", 0x31415927, 3, t0, t1);
   TESTINST1("swr $t0, 5($t1)", 0x7fffffff, 5, t0, t1);
   TESTINST1("swr $t0, 7($t1)", 0x80000000, 7, t0, t1);
   TESTINST1("swr $t0, 9($t1)", 0x80000000, 9, t0, t1);
   TESTINST1("swr $t0, 6($t1)", 0x7fffffff, 6, t0, t1);
   TESTINST1("swr $t0, 11($t1)", 0x7fffffff, 11, t0, t1);
   TESTINST1("swr $t0, 8($t1)", -1, 8, t0, t1);
   TESTINST1("swr $t0, 0($t1)", 0x31415927, 0, t0, t1);
   TESTINST1("swr $t0, 0($t1)", 0x0dd00000, 0, t0, t1);
   TESTINST1("swr $t0, 0($t1)", 655, 0, t0, t1);
   TESTINST1("swr $t0, 0($t1)", -655, 0, t0, t1);
   TESTINST1("swr $t0, 0($t1)", 15, 0, t0, t1);
   TESTINST1("swr $t0, 0($t1)", 1, 0, t0, t1);
   TESTINST1("swr $t0, 0($t1)", 53, 0, t0, t1);
   TESTINST1("swr $t0, 2($t1)", 0xffffffff, 2, t0, t1);
   TESTINST1("swr $t0, 2($t1)", 0xffffffff, 2, t0, t1);
   TESTINST1("swr $t0, 31($t1)", 0xffffffff, 31, t0, t1);
   TESTINST1("swr $t0, 33($t1)", 0xffffffff, 33, t0, t1);
   TESTINST1("swr $t0, 45($t1)", 0x31415927, 45, t0, t1);
   TESTINST1("swr $t0, 47($t1)", 0x7fffffff, 47, t0, t1);
   TESTINST1("swr $t0, 49($t1)", 0x80000000, 49, t0, t1);
   TESTINST1("swr $t0, 51($t1)", 655, 51, t0, t1);
   ppMem(mem1, 16);
   ppMem1(mem, 16);

   printf("ulw\n");
   TESTINST1("ulw $t0, 0($t1)", 0, 0, t0, t1);
   TESTINST1("ulw $t0, 0($t1)", 0x31415927, 0, t0, t1);
   TESTINST1("ulw $t0, 0($t1)", 0x7fffffff, 0, t0, t1);
   TESTINST1("ulw $t0, 0($t1)", 0x80000000, 0, t0, t1);
   TESTINST1("ulw $t0, 2($t1)", 0x80000000, 2, t0, t1);
   TESTINST1("ulw $t0, 6($t1)", 0x7fffffff, 6, t0, t1);
   TESTINST1("ulw $t0, 10($t1)", 0x7fffffff, 10, t0, t1);
   TESTINST1("ulw $t0, 8($t1)", -1, 8, t0, t1);
   TESTINST1("ulw $t0, 0($t1)", 0x31415927, 0, t0, t1);
   TESTINST1("ulw $t0, 0($t1)", 0x0dd00000, 0, t0, t1);
   TESTINST1("ulw $t0, 0($t1)", 655, 0, t0, t1);
   TESTINST1("ulw $t0, 0($t1)", -655, 0, t0, t1);
   TESTINST1("ulw $t0, 0($t1)", 15, 0, t0, t1);
   TESTINST1("ulw $t0, 0($t1)", 1, 0, t0, t1);
   TESTINST1("ulw $t0, 0($t1)", 53, 0, t0, t1);
   TESTINST1("ulw $t0, 2($t1)", 0xffffffff, 2, t0, t1);
   TESTINST1("ulw $t0, 2($t1)", 0xffffffff, 2, t0, t1);
   TESTINST1("ulw $t0, 32($t1)", 0xffffffff, 32, t0, t1);
   TESTINST1("ulw $t0, 36($t1)", 0xffffffff, 36, t0, t1);
   TESTINST1("ulw $t0, 40($t1)", 0x31415927, 40, t0, t1);
   TESTINST1("ulw $t0, 44($t1)", 0x7fffffff, 44, t0, t1);
   TESTINST1("ulw $t0, 48($t1)", 0x80000000, 48, t0, t1);
   TESTINST1("ulw $t0, 52($t1)", 655, 52, t0, t1);
   ppMem(mem1, 16);
   ppMem1(mem, 16);

   printf("usw\n");
   TESTINST1("usw $t0, 0($t1)", 0, 0, t0, t1);
   TESTINST1("usw $t0, 0($t1)", 0x31415927, 0, t0, t1);
   TESTINST1("usw $t0, 0($t1)", 0x7fffffff, 0, t0, t1);
   TESTINST1("usw $t0, 0($t1)", 0x80000000, 0, t0, t1);
   TESTINST1("usw $t0, 2($t1)", 0x80000000, 2, t0, t1);
   TESTINST1("usw $t0, 6($t1)", 0x7fffffff, 6, t0, t1);
   TESTINST1("usw $t0, 10($t1)", 0x7fffffff, 10, t0, t1);
   TESTINST1("usw $t0, 8($t1)", -1, 8, t0, t1);
   TESTINST1("usw $t0, 0($t1)", 0x31415927, 0, t0, t1);
   TESTINST1("usw $t0, 0($t1)", 0x0dd00000, 0, t0, t1);
   TESTINST1("usw $t0, 0($t1)", 655, 0, t0, t1);
   TESTINST1("usw $t0, 0($t1)", -655, 0, t0, t1);
   TESTINST1("usw $t0, 0($t1)", 15, 0, t0, t1);
   TESTINST1("usw $t0, 0($t1)", 1, 0, t0, t1);
   TESTINST1("usw $t0, 0($t1)", 53, 0, t0, t1);
   TESTINST1("usw $t0, 2($t1)", 0xffffffff, 2, t0, t1);
   TESTINST1("usw $t0, 2($t1)", 0xffffffff, 2, t0, t1);
   TESTINST1("usw $t0, 32($t1)", 0xffffffff, 32, t0, t1);
   TESTINST1("usw $t0, 36($t1)", 0xffffffff, 36, t0, t1);
   TESTINST1("usw $t0, 40($t1)", 0x31415927, 40, t0, t1);
   TESTINST1("usw $t0, 44($t1)", 0x7fffffff, 44, t0, t1);
   TESTINST1("usw $t0, 48($t1)", 0x80000000, 48, t0, t1);
   TESTINST1("usw $t0, 52($t1)", 655, 52, t0, t1);
   ppMem(mem1, 16);
   ppMem1(mem, 16);

   printf("swl $t0, 3($t0)\nswr $t0, 0($t0)\n");
   TESTINSTsw(0x4853000, 0, t0, t1);
   ppMem0(mem2, 12);
   TESTINSTsw(0x4853000, 4, t0, t1);
   ppMem0(mem2, 12);
   TESTINSTsw(0x4863700, 8, t0, t1);
   ppMem0(mem2, 12);
   TESTINSTsw(0x48aedd0, 12, t0, t1);
   ppMem0(mem2, 12);
   TESTINSTsw(0x2aaee700, 16, t0, t1);
   ppMem0(mem2, 12);
   TESTINSTsw(0x2aaee7ff, 20, t0, t1);
   ppMem0(mem2, 12);
   TESTINSTsw(0x2aaeffff, 24, t0, t1);
   ppMem0(mem2, 12);
   TESTINSTsw(0x4863700, 28, t0, t1);
   ppMem0(mem2, 12);
   TESTINSTsw(0x2aaee700, 32, t0, t1);
   ppMem0(mem2, 12);
#endif
   return 0;
}

