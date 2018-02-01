#include <stdio.h>

#define TESTINST1(instruction, RSval, RTval, RD, RS, RT) \
{ \
   unsigned int out; \
   __asm__ volatile( \
      "li   $" #RD ", 0\n\t"  \
      "move $" #RS ", %1\n\t" \
      "move $" #RT ", %2\n\t" \
      instruction "\n\t" \
      "move %0, $" #RD "\n\t" \
      : "=&r" (out) \
      : "r" (RSval), "r" (RTval) \
      : #RD, #RS, #RT, "cc", "memory" \
        ); \
        printf("%s :: rd 0x%08x rs 0x%08x, rt 0x%08x\n", \
        instruction, out, RSval, RTval); \
}

#define TESTINST2(instruction, RSval, imm, RT, RS) \
{ \
   unsigned int out; \
   __asm__ volatile( \
      "move $" #RS ", %1\n\t" \
      instruction "\n\t" \
      "move %0, $" #RT "\n\t" \
      : "=&r" (out) \
      : "r" (RSval) \
      : #RT, #RS, "cc", "memory" \
        ); \
        printf("%s :: rt 0x%08x rs 0x%08x, imm 0x%08x\n", \
        instruction, out, RSval, imm); \
}

#define TESTINST3(instruction, RSval, RD, RS) \
{ \
   unsigned int out; \
   __asm__ volatile( \
      "move $" #RS ", %1\n\t" \
      instruction "\n\t" \
      "move %0, $" #RD "\n\t" \
      : "=&r" (out) \
      : "r" (RSval) \
      : #RD, #RS, "cc", "memory" \
        ); \
        printf("%s :: rd 0x%08x rs 0x%08x\n", \
        instruction, out, RSval); \
}

#define TESTINST3a(instruction, RSval, RTval, RS, RT) \
{ \
   unsigned int HI; \
   unsigned int LO; \
   __asm__ volatile( \
      "li $" #RS ", 0x0\n\t" \
      "mthi $" #RS"\n\t" \
      "mtlo $" #RS"\n\t" \
      "move $" #RS ", %2\n\t" \
      "move $" #RT ", %3\n\t" \
      instruction "\n\t" \
      "mfhi %0 \n\t" \
      "mflo %1 \n\t" \
      : "=&r" (HI), "=&r" (LO) \
      : "r" (RSval), "r"(RTval) \
      : #RS, #RT, "cc", "memory" \
        ); \
   printf("%s :: rs 0x%08x rt 0x%08x HI 0x%08x LO 0x%08x \n", \
        instruction, RSval, RTval, HI, LO); \
}

#define TESTINST4(instruction, RTval, RSval, RT, RS, pos, size) \
{ \
   unsigned int out; \
   __asm__ volatile( \
      "move $" #RT ", %1\n\t" \
      "move $" #RS ", %2\n\t" \
      instruction "\n\t" \
      "move %0, $" #RT "\n\t" \
      : "=&r" (out) \
      : "r" (RTval), "r" (RSval) \
      : #RT, #RS, "cc", "memory" \
        ); \
        printf("%s :: rt 0x%08x rs 0x%08x, pos 0x%08x, size 0x%08x\n", \
        instruction, out, RSval, pos, size); \
}

const unsigned int mem[] = {
   0x121f1e1f, 0, 3, -1,
   0x232f2e2f, 0x242c2b2b, 0x252a2e2b, 0x262d2d2a,
   0x3f343f3e, 0x3e353d3c, 0x363a3c3b, 0x3b373b3a,
   0x454f4e45, 0x4e464d46, 0x474d474c, 0x4a484a4c
};

// load $t0, 0($t1)
#define TESTINSN5LOAD(instruction, RTval, offset, RT) \
{ \
    unsigned int out; \
   __asm__ volatile( \
     "move $t1, %1\n\t" \
     "li $t0, " #RTval"\n\t" \
     instruction "\n\t" \
     "move %0, $" #RT "\n\t" \
     : "=&r" (out) \
     : "r" (mem), "r" (RTval) \
     : #RT, "cc", "memory" \
   ); \
   printf("%s :: rt 0x%08x\n", \
          instruction, out); \
}

#define TESTINSN_HILO(RSval) \
{ \
   unsigned int HI; \
   unsigned int LO; \
   __asm__ volatile( \
      "move $t0, %2\n\t" \
      "mthi $t0\n\t" \
      "addiu $t0, $t0, 0xffff\n\t" \
      "mtlo $t0\n\t" \
      "mfhi %0\n\t" \
      "mflo %1\n\t" \
     : "=&r" (HI), "=&r" (LO) \
     : "r" (RSval) \
     : "cc", "memory" \
   ); \
   printf("mfhi mflo :: HI: 0x%x, LO: 0x%x\n", \
          HI, LO); \
}

int main(int argc, char **argv)
{
   printf("ADD\n");
   TESTINST1("add $t0, $t1, $t2", 0, 0, t0, t1, t2);
   TESTINST1("add $t0, $t1, $t2", 0, 1, t0, t1, t2);
   TESTINST1("add $t0, $t1, $t2", 1, 0, t0, t1, t2);
   TESTINST1("add $t0, $t1, $t2", 1, 1, t0, t1, t2);
   TESTINST1("add $t0, $t1, $t2", 0, -1, t0, t1, t2);
   TESTINST1("add $t0, $t1, $t2", 1, -1, t0, t1, t2);
   TESTINST1("add $t0, $t1, $t2", 0x80000000, 0, t0, t1, t2);
   TESTINST1("add $t0, $t1, $t2", 0x31415927, 0x27181728, t0, t1, t2);
   TESTINST1("add $t0, $t1, $t2", 0x31415927, 0x97181728, t0, t1, t2);
   TESTINST1("add $t0, $t1, $t2", -1,         0,          t0, t1, t2);
   TESTINST1("add $t0, $t1, $t2", 0,          0x80000000, t0, t1, t2);
   TESTINST1("add $t0, $t1, $t2", 0x7fffffff, 0x80000000, t0, t1, t2);

#if (__mips_isa_rev < 6)
   printf("ADDI\n");
   TESTINST2("addi $t0, $t1, 0", 0, 0, t0, t1);
   TESTINST2("addi $t0, $t1, 1", 0, 1, t0, t1);
   TESTINST2("addi $t0, $t1, 1", 1, 0, t0, t1);
   TESTINST2("addi $t0, $t1, 1", 1, 1, t0, t1);
   TESTINST2("addi $t0, $t1, -1", 0, -1, t0, t1);
   TESTINST2("addi $t0, $t1, -1", 1, -1, t0, t1);
   TESTINST2("addi $t0, $t1, 0", 0x80000000, 0, t0, t1);
   TESTINST2("addi $t0, $t1, 0", -1,         0,          t0, t1);
   TESTINST2("addi $t0, $t1, 0", 0x80000000, 0,          t0, t1);
#endif

   printf("ADDIU\n");
   TESTINST2("addiu $t0, $t1, 0", 0, 0, t0, t1);
   TESTINST2("addiu $t0, $t1, 1", 0, 1, t0, t1);
   TESTINST2("addiu $t0, $t1, 1", 1, 0, t0, t1);
   TESTINST2("addiu $t0, $t1, 1", 1, 1, t0, t1);
   TESTINST2("addiu $t0, $t1, -1", 0, -1, t0, t1);
   TESTINST2("addiu $t0, $t1, -1", 1, -1, t0, t1);
   TESTINST2("addiu $t0, $t1, 0", 0x80000000, 0, t0, t1);
   TESTINST2("addiu $t0, $t1, 0", -1,         0,          t0, t1);
   TESTINST2("addiu $t0, $t1, 0", 0x80000000, 0,          t0, t1);

   printf("ADDU\n");
   TESTINST1("addu $t0, $t1, $t2", 0, 0, t0, t1, t2);
   TESTINST1("addu $t0, $t1, $t2", 0, 1, t0, t1, t2);
   TESTINST1("addu $t0, $t1, $t2", 1, 0, t0, t1, t2);
   TESTINST1("addu $t0, $t1, $t2", 1, 1, t0, t1, t2);
   TESTINST1("addu $t0, $t1, $t2", 0, -1, t0, t1, t2);
   TESTINST1("addu $t0, $t1, $t2", 1, -1, t0, t1, t2);
   TESTINST1("addu $t0, $t1, $t2", 0x31415927, 0x27181728, t0, t1, t2);
   TESTINST1("addu $t0, $t1, $t2", 0x31415927, 0x97181728, t0, t1, t2);
   TESTINST1("addu $t0, $t1, $t2", 0,          0,          t0, t1, t2);
   TESTINST1("addu $t0, $t1, $t2", 1,          0,          t0, t1, t2);
   TESTINST1("addu $t0, $t1, $t2", 0,          1,          t0, t1, t2);
   TESTINST1("addu $t0, $t1, $t2", -1,         0,          t0, t1, t2);
   TESTINST1("addu $t0, $t1, $t2", 0,          -1,         t0, t1, t2);
   TESTINST1("addu $t0, $t1, $t2", 0,          0x80000000, t0, t1, t2);
   TESTINST1("addu $t0, $t1, $t2", 0x80000000, 0,          t0, t1, t2);
   TESTINST1("addu $t0, $t1, $t2", 0x80000000, 0x80000000, t0, t1, t2);
   TESTINST1("addu $t0, $t1, $t2", 0x7fffffff, 0x80000000, t0, t1, t2);
   TESTINST1("addu $t0, $t1, $t2", 0x80000000, 0x7fffffff, t0, t1, t2);
   TESTINST1("addu $t0, $t1, $t2", 0x7fffffff, 0x7fffffff, t0, t1, t2);

   printf("AND\n");
   TESTINST1("and $t0, $t1, $t2", 0x31415927, 0xffffffff, t0, t1, t2);
   TESTINST1("and $t0, $t1, $t2", 0x31415927, 0xee00ee00, t0, t1, t2);
   TESTINST1("and $t0, $t1, $t2", 0,          255,        t0, t1, t2);
   TESTINST1("and $t0, $t1, $t2", -1,         0,          t0, t1, t2);
   TESTINST1("and $t0, $t1, $t2", 0,          1,          t0, t1, t2);
   TESTINST1("and $t0, $t1, $t2", 0,          0,          t0, t1, t2);
   TESTINST1("and $t0, $t1, $t2", 0x80000000, -1,         t0, t1, t2);
   TESTINST1("and $t0, $t1, $t2", 0x80000000, 0x80000000, t0, t1, t2);
   TESTINST1("and $t0, $t1, $t2", 0x7fffffff, 0,          t0, t1, t2);
   TESTINST1("and $t0, $t1, $t2", 0x80000000, 0x80000000, t0, t1, t2);
   TESTINST1("and $t0, $t1, $t2", 0x7fffffff, 0x80000000, t0, t1, t2);
   TESTINST1("and $t0, $t1, $t2", 0x80000000, 0xff000000, t0, t1, t2);
   TESTINST1("and $t0, $t1, $t2", 0x7fffffff, 0x0dd00000, t0, t1, t2);
   TESTINST1("and $t0, $t1, $t2", 0x31415927, 0xffffffff, t0, t1, t2);
   TESTINST1("and $t0, $t1, $t2", 0x31415927, 0xee00ee00, t0, t1, t2);
   TESTINST1("and $t0, $t1, $t2", 0,          255,        t0, t1, t2);
   TESTINST1("and $t0, $t1, $t2", 1,          0,          t0, t1, t2);
   TESTINST1("and $t0, $t1, $t2", 0,          1,          t0, t1, t2);
   TESTINST1("and $t0, $t1, $t2", -1,         0,          t0, t1, t2);
   TESTINST1("and $t0, $t1, $t2", 0,          -1,         t0, t1, t2);
   TESTINST1("and $t0, $t1, $t2", 0,          0x80000000, t0, t1, t2);
   TESTINST1("and $t0, $t1, $t2", 0x80000000, 0,          t0, t1, t2);
   TESTINST1("and $t0, $t1, $t2", 0x80000000, 0x80000000, t0, t1, t2);
   TESTINST1("and $t0, $t1, $t2", 0x7fffffff, 0x80000000, t0, t1, t2);
   TESTINST1("and $t0, $t1, $t2", 0x80000000, 0xff000000, t0, t1, t2);
   TESTINST1("and $t0, $t1, $t2", 0x7fffffff, 0x0dd00000, t0, t1, t2);

   printf("ANDI\n");
   TESTINST2("andi $t0, $t1, 1", 0, 1, t0, t1);
   TESTINST2("andi $t0, $t1, 0", 1, 0, t0, t1);
   TESTINST2("andi $t0, $t1, 1", 1, 1, t0, t1);
   TESTINST2("andi $t0, $t1, 1", 0x7fffffff, 0, t0, t1);
   TESTINST2("andi $t0, $t1, 0", 0x80000000, 0, t0, t1);
   TESTINST2("andi $t0, $t1, 0x3145", 0xffffffff, 0x3145, t0, t1);

   printf("CLO\n");
   TESTINST3("clo  $t0, $t1", 0, t0, t1);
   TESTINST3("clo  $t0, $t1", 1, t0, t1);
   TESTINST3("clo  $t0, $t1", 0x10, t0, t1);
   TESTINST3("clo  $t0, $t1", 0xffffffff, t0, t1);

   printf("CLZ\n");
   TESTINST3("clz  $t0, $t1", 0, t0, t1);
   TESTINST3("clz  $t0, $t1", 1, t0, t1);
   TESTINST3("clz  $t0, $t1", 0x10, t0, t1);
   TESTINST3("clz  $t0, $t1", 0xffffffff, t0, t1);

#if (__mips_isa_rev < 6)
   printf("DIV\n");
   TESTINST3a("div  $t0, $t1", 0x6, 0x2, t0, t1);
   TESTINST3a("div  $t0, $t1", 0x7fffffff, 0x7fffffff, t0, t1);
   TESTINST3a("div  $t0, $t1", 0xffffffff, 0x1, t0, t1);
   TESTINST3a("div  $t0, $t1", 0x1, 0xffffffff, t0, t1);
   TESTINST3a("div  $t0, $t1", 0x2, 0x6, t0, t1);

   printf("DIVU\n");
   TESTINST3a("divu  $t0, $t1", 0x6, 0x2, t0, t1);
   TESTINST3a("divu  $t0, $t1", 0x7fffffff, 0x7fffffff, t0, t1);
   TESTINST3a("divu  $t0, $t1", 0xffffffff, 0x1, t0, t1);
   TESTINST3a("divu  $t0, $t1", 0x1, 0xffffffff, t0, t1);
   TESTINST3a("divu  $t0, $t1", 0x2, 0x6, t0, t1);
   TESTINST3a("divu  $t0, $t1", 0x0, 0x2, t0, t1);
#endif

#if (__mips==32) && (__mips_isa_rev>=2)
   printf("EXT\n");
   TESTINST4("ext $t0, $t1, 0, 1",  0x0,        0x0,        t0, t1, 0, 1);
   TESTINST4("ext $t0, $t1, 0, 1",  0x0,        0xffffffff, t0, t1, 0, 1);
   TESTINST4("ext $t0, $t1, 0, 1",  0x0,        0x98765432, t0, t1, 0, 1);
   TESTINST4("ext $t0, $t1, 0, 1",  0x0,        0xff865421, t0, t1, 0, 1);
   TESTINST4("ext $t0, $t1, 0, 1",  0xffffffff, 0x0,        t0, t1, 0, 1);
   TESTINST4("ext $t0, $t1, 0, 1",  0xffffffff, 0xffffffff, t0, t1, 0, 1);
   TESTINST4("ext $t0, $t1, 0, 1",  0xffffffff, 0x98765432, t0, t1, 0, 1);
   TESTINST4("ext $t0, $t1, 0, 1",  0xffffffff, 0xff865421, t0, t1, 0, 1);
   TESTINST4("ext $t0, $t1, 0, 1",  0x98765432, 0x0,        t0, t1, 0, 1);
   TESTINST4("ext $t0, $t1, 0, 1",  0x98765432, 0xffffffff, t0, t1, 0, 1);
   TESTINST4("ext $t0, $t1, 0, 1",  0x98765432, 0x98765432, t0, t1, 0, 1);
   TESTINST4("ext $t0, $t1, 0, 1",  0x98765432, 0xff865421, t0, t1, 0, 1);
   TESTINST4("ext $t0, $t1, 0, 1",  0xff865421, 0x0,        t0, t1, 0, 1);
   TESTINST4("ext $t0, $t1, 0, 1",  0xff865421, 0xffffffff, t0, t1, 0, 1);
   TESTINST4("ext $t0, $t1, 0, 1",  0xff865421, 0x98765432, t0, t1, 0, 1);
   TESTINST4("ext $t0, $t1, 0, 1",  0xff865421, 0xff865421, t0, t1, 0, 1);
   TESTINST4("ext $t0, $t1, 0, 4",  0x0,        0x0,        t0, t1, 0, 4);
   TESTINST4("ext $t0, $t1, 0, 4",  0x0,        0xffffffff, t0, t1, 0, 4);
   TESTINST4("ext $t0, $t1, 0, 4",  0x0,        0x98765432, t0, t1, 0, 4);
   TESTINST4("ext $t0, $t1, 0, 4",  0x0,        0xff865421, t0, t1, 0, 4);
   TESTINST4("ext $t0, $t1, 0, 4",  0xffffffff, 0x0,        t0, t1, 0, 4);
   TESTINST4("ext $t0, $t1, 0, 4",  0xffffffff, 0xffffffff, t0, t1, 0, 4);
   TESTINST4("ext $t0, $t1, 0, 4",  0xffffffff, 0x98765432, t0, t1, 0, 4);
   TESTINST4("ext $t0, $t1, 0, 4",  0xffffffff, 0xff865421, t0, t1, 0, 4);
   TESTINST4("ext $t0, $t1, 0, 4",  0x98765432, 0x0,        t0, t1, 0, 4);
   TESTINST4("ext $t0, $t1, 0, 4",  0x98765432, 0xffffffff, t0, t1, 0, 4);
   TESTINST4("ext $t0, $t1, 0, 4",  0x98765432, 0x98765432, t0, t1, 0, 4);
   TESTINST4("ext $t0, $t1, 0, 4",  0x98765432, 0xff865421, t0, t1, 0, 4);
   TESTINST4("ext $t0, $t1, 0, 4",  0xff865421, 0x0,        t0, t1, 0, 4);
   TESTINST4("ext $t0, $t1, 0, 4",  0xff865421, 0xffffffff, t0, t1, 0, 4);
   TESTINST4("ext $t0, $t1, 0, 4",  0xff865421, 0x98765432, t0, t1, 0, 4);
   TESTINST4("ext $t0, $t1, 0, 4",  0xff865421, 0xff865421, t0, t1, 0, 4);
   TESTINST4("ext $t0, $t1, 0, 16", 0x0,        0x0,        t0, t1, 0, 16);
   TESTINST4("ext $t0, $t1, 0, 16", 0x0,        0xffffffff, t0, t1, 0, 16);
   TESTINST4("ext $t0, $t1, 0, 16", 0x0,        0x98765432, t0, t1, 0, 16);
   TESTINST4("ext $t0, $t1, 0, 16", 0x0,        0xff865421, t0, t1, 0, 16);
   TESTINST4("ext $t0, $t1, 0, 16", 0xffffffff, 0x0,        t0, t1, 0, 16);
   TESTINST4("ext $t0, $t1, 0, 16", 0xffffffff, 0xffffffff, t0, t1, 0, 16);
   TESTINST4("ext $t0, $t1, 0, 16", 0xffffffff, 0x98765432, t0, t1, 0, 16);
   TESTINST4("ext $t0, $t1, 0, 16", 0xffffffff, 0xff865421, t0, t1, 0, 16);
   TESTINST4("ext $t0, $t1, 0, 16", 0x98765432, 0x0,        t0, t1, 0, 16);
   TESTINST4("ext $t0, $t1, 0, 16", 0x98765432, 0xffffffff, t0, t1, 0, 16);
   TESTINST4("ext $t0, $t1, 0, 16", 0x98765432, 0x98765432, t0, t1, 0, 16);
   TESTINST4("ext $t0, $t1, 0, 16", 0x98765432, 0xff865421, t0, t1, 0, 16);
   TESTINST4("ext $t0, $t1, 0, 16", 0xff865421, 0x0,        t0, t1, 0, 16);
   TESTINST4("ext $t0, $t1, 0, 16", 0xff865421, 0xffffffff, t0, t1, 0, 16);
   TESTINST4("ext $t0, $t1, 0, 16", 0xff865421, 0x98765432, t0, t1, 0, 16);
   TESTINST4("ext $t0, $t1, 0, 16", 0xff865421, 0xff865421, t0, t1, 0, 16);
   TESTINST4("ext $t0, $t1, 0, 32", 0x0,        0x0,        t0, t1, 0, 32);
   TESTINST4("ext $t0, $t1, 0, 32", 0x0,        0xffffffff, t0, t1, 0, 32);
   TESTINST4("ext $t0, $t1, 0, 32", 0x0,        0x98765432, t0, t1, 0, 32);
   TESTINST4("ext $t0, $t1, 0, 32", 0x0,        0xff865421, t0, t1, 0, 32);
   TESTINST4("ext $t0, $t1, 0, 32", 0xffffffff, 0x0,        t0, t1, 0, 32);
   TESTINST4("ext $t0, $t1, 0, 32", 0xffffffff, 0xffffffff, t0, t1, 0, 32);
   TESTINST4("ext $t0, $t1, 0, 32", 0xffffffff, 0x98765432, t0, t1, 0, 32);
   TESTINST4("ext $t0, $t1, 0, 32", 0xffffffff, 0xff865421, t0, t1, 0, 32);
   TESTINST4("ext $t0, $t1, 0, 32", 0x98765432, 0x0,        t0, t1, 0, 32);
   TESTINST4("ext $t0, $t1, 0, 32", 0x98765432, 0xffffffff, t0, t1, 0, 32);
   TESTINST4("ext $t0, $t1, 0, 32", 0x98765432, 0x98765432, t0, t1, 0, 32);
   TESTINST4("ext $t0, $t1, 0, 32", 0x98765432, 0xff865421, t0, t1, 0, 32);
   TESTINST4("ext $t0, $t1, 0, 32", 0xff865421, 0x0,        t0, t1, 0, 32);
   TESTINST4("ext $t0, $t1, 0, 32", 0xff865421, 0xffffffff, t0, t1, 0, 32);
   TESTINST4("ext $t0, $t1, 0, 32", 0xff865421, 0x98765432, t0, t1, 0, 32);
   TESTINST4("ext $t0, $t1, 0, 32", 0xff865421, 0xff865421, t0, t1, 0, 32);

   TESTINST4("ext $t0, $t1, 4, 1",  0x0,        0x0,        t0, t1, 4, 1);
   TESTINST4("ext $t0, $t1, 4, 1",  0x0,        0xffffffff, t0, t1, 4, 1);
   TESTINST4("ext $t0, $t1, 4, 1",  0x0,        0x98765432, t0, t1, 4, 1);
   TESTINST4("ext $t0, $t1, 4, 1",  0x0,        0xff865421, t0, t1, 4, 1);
   TESTINST4("ext $t0, $t1, 4, 1",  0xffffffff, 0x0,        t0, t1, 4, 1);
   TESTINST4("ext $t0, $t1, 4, 1",  0xffffffff, 0xffffffff, t0, t1, 4, 1);
   TESTINST4("ext $t0, $t1, 4, 1",  0xffffffff, 0x98765432, t0, t1, 4, 1);
   TESTINST4("ext $t0, $t1, 4, 1",  0xffffffff, 0xff865421, t0, t1, 4, 1);
   TESTINST4("ext $t0, $t1, 4, 1",  0x98765432, 0x0,        t0, t1, 4, 1);
   TESTINST4("ext $t0, $t1, 4, 1",  0x98765432, 0xffffffff, t0, t1, 4, 1);
   TESTINST4("ext $t0, $t1, 4, 1",  0x98765432, 0x98765432, t0, t1, 4, 1);
   TESTINST4("ext $t0, $t1, 4, 1",  0x98765432, 0xff865421, t0, t1, 4, 1);
   TESTINST4("ext $t0, $t1, 4, 1",  0xff865421, 0x0,        t0, t1, 4, 1);
   TESTINST4("ext $t0, $t1, 4, 1",  0xff865421, 0xffffffff, t0, t1, 4, 1);
   TESTINST4("ext $t0, $t1, 4, 1",  0xff865421, 0x98765432, t0, t1, 4, 1);
   TESTINST4("ext $t0, $t1, 4, 1",  0xff865421, 0xff865421, t0, t1, 4, 1);
   TESTINST4("ext $t0, $t1, 4, 4",  0x0,        0x0,        t0, t1, 4, 4);
   TESTINST4("ext $t0, $t1, 4, 4",  0x0,        0xffffffff, t0, t1, 4, 4);
   TESTINST4("ext $t0, $t1, 4, 4",  0x0,        0x98765432, t0, t1, 4, 4);
   TESTINST4("ext $t0, $t1, 4, 4",  0x0,        0xff865421, t0, t1, 4, 4);
   TESTINST4("ext $t0, $t1, 4, 4",  0xffffffff, 0x0,        t0, t1, 4, 4);
   TESTINST4("ext $t0, $t1, 4, 4",  0xffffffff, 0xffffffff, t0, t1, 4, 4);
   TESTINST4("ext $t0, $t1, 4, 4",  0xffffffff, 0x98765432, t0, t1, 4, 4);
   TESTINST4("ext $t0, $t1, 4, 4",  0xffffffff, 0xff865421, t0, t1, 4, 4);
   TESTINST4("ext $t0, $t1, 4, 4",  0x98765432, 0x0,        t0, t1, 4, 4);
   TESTINST4("ext $t0, $t1, 4, 4",  0x98765432, 0xffffffff, t0, t1, 4, 4);
   TESTINST4("ext $t0, $t1, 4, 4",  0x98765432, 0x98765432, t0, t1, 4, 4);
   TESTINST4("ext $t0, $t1, 4, 4",  0x98765432, 0xff865421, t0, t1, 4, 4);
   TESTINST4("ext $t0, $t1, 4, 4",  0xff865421, 0x0,        t0, t1, 4, 4);
   TESTINST4("ext $t0, $t1, 4, 4",  0xff865421, 0xffffffff, t0, t1, 4, 4);
   TESTINST4("ext $t0, $t1, 4, 4",  0xff865421, 0x98765432, t0, t1, 4, 4);
   TESTINST4("ext $t0, $t1, 4, 4",  0xff865421, 0xff865421, t0, t1, 4, 4);
   TESTINST4("ext $t0, $t1, 4, 16", 0x0,        0x0,        t0, t1, 4, 16);
   TESTINST4("ext $t0, $t1, 4, 16", 0x0,        0xffffffff, t0, t1, 4, 16);
   TESTINST4("ext $t0, $t1, 4, 16", 0x0,        0x98765432, t0, t1, 4, 16);
   TESTINST4("ext $t0, $t1, 4, 16", 0x0,        0xff865421, t0, t1, 4, 16);
   TESTINST4("ext $t0, $t1, 4, 16", 0xffffffff, 0x0,        t0, t1, 4, 16);
   TESTINST4("ext $t0, $t1, 4, 16", 0xffffffff, 0xffffffff, t0, t1, 4, 16);
   TESTINST4("ext $t0, $t1, 4, 16", 0xffffffff, 0x98765432, t0, t1, 4, 16);
   TESTINST4("ext $t0, $t1, 4, 16", 0xffffffff, 0xff865421, t0, t1, 4, 16);
   TESTINST4("ext $t0, $t1, 4, 16", 0x98765432, 0x0,        t0, t1, 4, 16);
   TESTINST4("ext $t0, $t1, 4, 16", 0x98765432, 0xffffffff, t0, t1, 4, 16);
   TESTINST4("ext $t0, $t1, 4, 16", 0x98765432, 0x98765432, t0, t1, 4, 16);
   TESTINST4("ext $t0, $t1, 4, 16", 0x98765432, 0xff865421, t0, t1, 4, 16);
   TESTINST4("ext $t0, $t1, 4, 16", 0xff865421, 0x0,        t0, t1, 4, 16);
   TESTINST4("ext $t0, $t1, 4, 16", 0xff865421, 0xffffffff, t0, t1, 4, 16);
   TESTINST4("ext $t0, $t1, 4, 16", 0xff865421, 0x98765432, t0, t1, 4, 16);
   TESTINST4("ext $t0, $t1, 4, 16", 0xff865421, 0xff865421, t0, t1, 4, 16);
   TESTINST4("ext $t0, $t1, 4, 28", 0x0,        0x0,        t0, t1, 4, 28);
   TESTINST4("ext $t0, $t1, 4, 28", 0x0,        0xffffffff, t0, t1, 4, 28);
   TESTINST4("ext $t0, $t1, 4, 28", 0x0,        0x98765432, t0, t1, 4, 28);
   TESTINST4("ext $t0, $t1, 4, 28", 0x0,        0xff865421, t0, t1, 4, 28);
   TESTINST4("ext $t0, $t1, 4, 28", 0xffffffff, 0x0,        t0, t1, 4, 28);
   TESTINST4("ext $t0, $t1, 4, 28", 0xffffffff, 0xffffffff, t0, t1, 4, 28);
   TESTINST4("ext $t0, $t1, 4, 28", 0xffffffff, 0x98765432, t0, t1, 4, 28);
   TESTINST4("ext $t0, $t1, 4, 28", 0xffffffff, 0xff865421, t0, t1, 4, 28);
   TESTINST4("ext $t0, $t1, 4, 28", 0x98765432, 0x0,        t0, t1, 4, 28);
   TESTINST4("ext $t0, $t1, 4, 28", 0x98765432, 0xffffffff, t0, t1, 4, 28);
   TESTINST4("ext $t0, $t1, 4, 28", 0x98765432, 0x98765432, t0, t1, 4, 28);
   TESTINST4("ext $t0, $t1, 4, 28", 0x98765432, 0xff865421, t0, t1, 4, 28);
   TESTINST4("ext $t0, $t1, 4, 28", 0xff865421, 0x0,        t0, t1, 4, 28);
   TESTINST4("ext $t0, $t1, 4, 28", 0xff865421, 0xffffffff, t0, t1, 4, 28);
   TESTINST4("ext $t0, $t1, 4, 28", 0xff865421, 0x98765432, t0, t1, 4, 28);
   TESTINST4("ext $t0, $t1, 4, 28", 0xff865421, 0xff865421, t0, t1, 4, 28);

   TESTINST4("ext $t0, $t1, 16, 1",  0x0,        0x0,        t0, t1, 1, 16);
   TESTINST4("ext $t0, $t1, 16, 1",  0x0,        0xffffffff, t0, t1, 1, 16);
   TESTINST4("ext $t0, $t1, 16, 1",  0x0,        0x98765432, t0, t1, 1, 16);
   TESTINST4("ext $t0, $t1, 16, 1",  0x0,        0xff865421, t0, t1, 1, 16);
   TESTINST4("ext $t0, $t1, 16, 1",  0xffffffff, 0x0,        t0, t1, 1, 16);
   TESTINST4("ext $t0, $t1, 16, 1",  0xffffffff, 0xffffffff, t0, t1, 1, 16);
   TESTINST4("ext $t0, $t1, 16, 1",  0xffffffff, 0x98765432, t0, t1, 1, 16);
   TESTINST4("ext $t0, $t1, 16, 1",  0xffffffff, 0xff865421, t0, t1, 1, 16);
   TESTINST4("ext $t0, $t1, 16, 1",  0x98765432, 0x0,        t0, t1, 1, 16);
   TESTINST4("ext $t0, $t1, 16, 1",  0x98765432, 0xffffffff, t0, t1, 1, 16);
   TESTINST4("ext $t0, $t1, 16, 1",  0x98765432, 0x98765432, t0, t1, 1, 16);
   TESTINST4("ext $t0, $t1, 16, 1",  0x98765432, 0xff865421, t0, t1, 1, 16);
   TESTINST4("ext $t0, $t1, 16, 1",  0xff865421, 0x0,        t0, t1, 1, 16);
   TESTINST4("ext $t0, $t1, 16, 1",  0xff865421, 0xffffffff, t0, t1, 1, 16);
   TESTINST4("ext $t0, $t1, 16, 1",  0xff865421, 0x98765432, t0, t1, 1, 16);
   TESTINST4("ext $t0, $t1, 16, 1",  0xff865421, 0xff865421, t0, t1, 1, 16);
   TESTINST4("ext $t0, $t1, 16, 4",  0x0,        0x0,        t0, t1, 16, 4);
   TESTINST4("ext $t0, $t1, 16, 4",  0x0,        0xffffffff, t0, t1, 16, 4);
   TESTINST4("ext $t0, $t1, 16, 4",  0x0,        0x98765432, t0, t1, 16, 4);
   TESTINST4("ext $t0, $t1, 16, 4",  0x0,        0xff865421, t0, t1, 16, 4);
   TESTINST4("ext $t0, $t1, 16, 4",  0xffffffff, 0x0,        t0, t1, 16, 4);
   TESTINST4("ext $t0, $t1, 16, 4",  0xffffffff, 0xffffffff, t0, t1, 16, 4);
   TESTINST4("ext $t0, $t1, 16, 4",  0xffffffff, 0x98765432, t0, t1, 16, 4);
   TESTINST4("ext $t0, $t1, 16, 4",  0xffffffff, 0xff865421, t0, t1, 16, 4);
   TESTINST4("ext $t0, $t1, 16, 4",  0x98765432, 0x0,        t0, t1, 16, 4);
   TESTINST4("ext $t0, $t1, 16, 4",  0x98765432, 0xffffffff, t0, t1, 16, 4);
   TESTINST4("ext $t0, $t1, 16, 4",  0x98765432, 0x98765432, t0, t1, 16, 4);
   TESTINST4("ext $t0, $t1, 16, 4",  0x98765432, 0xff865421, t0, t1, 16, 4);
   TESTINST4("ext $t0, $t1, 16, 4",  0xff865421, 0x0,        t0, t1, 16, 4);
   TESTINST4("ext $t0, $t1, 16, 4",  0xff865421, 0xffffffff, t0, t1, 16, 4);
   TESTINST4("ext $t0, $t1, 16, 4",  0xff865421, 0x98765432, t0, t1, 16, 4);
   TESTINST4("ext $t0, $t1, 16, 4",  0xff865421, 0xff865421, t0, t1, 16, 4);
   TESTINST4("ext $t0, $t1, 16, 16", 0x0,        0x0,        t0, t1, 16, 16);
   TESTINST4("ext $t0, $t1, 16, 16", 0x0,        0xffffffff, t0, t1, 16, 16);
   TESTINST4("ext $t0, $t1, 16, 16", 0x0,        0x98765432, t0, t1, 16, 16);
   TESTINST4("ext $t0, $t1, 16, 16", 0x0,        0xff865421, t0, t1, 16, 16);
   TESTINST4("ext $t0, $t1, 16, 16", 0xffffffff, 0x0,        t0, t1, 16, 16);
   TESTINST4("ext $t0, $t1, 16, 16", 0xffffffff, 0xffffffff, t0, t1, 16, 16);
   TESTINST4("ext $t0, $t1, 16, 16", 0xffffffff, 0x98765432, t0, t1, 16, 16);
   TESTINST4("ext $t0, $t1, 16, 16", 0xffffffff, 0xff865421, t0, t1, 16, 16);
   TESTINST4("ext $t0, $t1, 16, 16", 0x98765432, 0x0,        t0, t1, 16, 16);
   TESTINST4("ext $t0, $t1, 16, 16", 0x98765432, 0xffffffff, t0, t1, 16, 16);
   TESTINST4("ext $t0, $t1, 16, 16", 0x98765432, 0x98765432, t0, t1, 16, 16);
   TESTINST4("ext $t0, $t1, 16, 16", 0x98765432, 0xff865421, t0, t1, 16, 16);
   TESTINST4("ext $t0, $t1, 16, 16", 0xff865421, 0x0,        t0, t1, 16, 16);
   TESTINST4("ext $t0, $t1, 16, 16", 0xff865421, 0xffffffff, t0, t1, 16, 16);
   TESTINST4("ext $t0, $t1, 16, 16", 0xff865421, 0x98765432, t0, t1, 16, 16);
   TESTINST4("ext $t0, $t1, 16, 16", 0xff865421, 0xff865421, t0, t1, 16, 16);

   TESTINST4("ext $t0, $t1, 31, 1", 0x0,        0x0,        t0, t1, 31, 1);
   TESTINST4("ext $t0, $t1, 31, 1", 0x0,        0xffffffff, t0, t1, 31, 1);
   TESTINST4("ext $t0, $t1, 31, 1", 0x0,        0x98765432, t0, t1, 31, 1);
   TESTINST4("ext $t0, $t1, 31, 1", 0x0,        0xff865421, t0, t1, 31, 1);
   TESTINST4("ext $t0, $t1, 31, 1", 0xffffffff, 0x0,        t0, t1, 31, 1);
   TESTINST4("ext $t0, $t1, 31, 1", 0xffffffff, 0xffffffff, t0, t1, 31, 1);
   TESTINST4("ext $t0, $t1, 31, 1", 0xffffffff, 0x98765432, t0, t1, 31, 1);
   TESTINST4("ext $t0, $t1, 31, 1", 0xffffffff, 0xff865421, t0, t1, 31, 1);
   TESTINST4("ext $t0, $t1, 31, 1", 0x98765432, 0x0,        t0, t1, 31, 1);
   TESTINST4("ext $t0, $t1, 31, 1", 0x98765432, 0xffffffff, t0, t1, 31, 1);
   TESTINST4("ext $t0, $t1, 31, 1", 0x98765432, 0x98765432, t0, t1, 31, 1);
   TESTINST4("ext $t0, $t1, 31, 1", 0x98765432, 0xff865421, t0, t1, 31, 1);
   TESTINST4("ext $t0, $t1, 31, 1", 0xff865421, 0x0,        t0, t1, 31, 1);
   TESTINST4("ext $t0, $t1, 31, 1", 0xff865421, 0xffffffff, t0, t1, 31, 1);
   TESTINST4("ext $t0, $t1, 31, 1", 0xff865421, 0x98765432, t0, t1, 31, 1);
   TESTINST4("ext $t0, $t1, 31, 1", 0xff865421, 0xff865421, t0, t1, 31, 1);

   printf("INS\n");
   TESTINST4("ins $t0, $t1, 0, 1",  0x0,        0x0,        t0, t1, 0, 1);
   TESTINST4("ins $t0, $t1, 0, 1",  0x0,        0xffffffff, t0, t1, 0, 1);
   TESTINST4("ins $t0, $t1, 0, 1",  0x0,        0x98765432, t0, t1, 0, 1);
   TESTINST4("ins $t0, $t1, 0, 1",  0x0,        0xff865421, t0, t1, 0, 1);
   TESTINST4("ins $t0, $t1, 0, 1",  0xffffffff, 0x0,        t0, t1, 0, 1);
   TESTINST4("ins $t0, $t1, 0, 1",  0xffffffff, 0xffffffff, t0, t1, 0, 1);
   TESTINST4("ins $t0, $t1, 0, 1",  0xffffffff, 0x98765432, t0, t1, 0, 1);
   TESTINST4("ins $t0, $t1, 0, 1",  0xffffffff, 0xff865421, t0, t1, 0, 1);
   TESTINST4("ins $t0, $t1, 0, 1",  0x98765432, 0x0,        t0, t1, 0, 1);
   TESTINST4("ins $t0, $t1, 0, 1",  0x98765432, 0xffffffff, t0, t1, 0, 1);
   TESTINST4("ins $t0, $t1, 0, 1",  0x98765432, 0x98765432, t0, t1, 0, 1);
   TESTINST4("ins $t0, $t1, 0, 1",  0x98765432, 0xff865421, t0, t1, 0, 1);
   TESTINST4("ins $t0, $t1, 0, 1",  0xff865421, 0x0,        t0, t1, 0, 1);
   TESTINST4("ins $t0, $t1, 0, 1",  0xff865421, 0xffffffff, t0, t1, 0, 1);
   TESTINST4("ins $t0, $t1, 0, 1",  0xff865421, 0x98765432, t0, t1, 0, 1);
   TESTINST4("ins $t0, $t1, 0, 1",  0xff865421, 0xff865421, t0, t1, 0, 1);
   TESTINST4("ins $t0, $t1, 0, 4",  0x0,        0x0,        t0, t1, 0, 4);
   TESTINST4("ins $t0, $t1, 0, 4",  0x0,        0xffffffff, t0, t1, 0, 4);
   TESTINST4("ins $t0, $t1, 0, 4",  0x0,        0x98765432, t0, t1, 0, 4);
   TESTINST4("ins $t0, $t1, 0, 4",  0x0,        0xff865421, t0, t1, 0, 4);
   TESTINST4("ins $t0, $t1, 0, 4",  0xffffffff, 0x0,        t0, t1, 0, 4);
   TESTINST4("ins $t0, $t1, 0, 4",  0xffffffff, 0xffffffff, t0, t1, 0, 4);
   TESTINST4("ins $t0, $t1, 0, 4",  0xffffffff, 0x98765432, t0, t1, 0, 4);
   TESTINST4("ins $t0, $t1, 0, 4",  0xffffffff, 0xff865421, t0, t1, 0, 4);
   TESTINST4("ins $t0, $t1, 0, 4",  0x98765432, 0x0,        t0, t1, 0, 4);
   TESTINST4("ins $t0, $t1, 0, 4",  0x98765432, 0xffffffff, t0, t1, 0, 4);
   TESTINST4("ins $t0, $t1, 0, 4",  0x98765432, 0x98765432, t0, t1, 0, 4);
   TESTINST4("ins $t0, $t1, 0, 4",  0x98765432, 0xff865421, t0, t1, 0, 4);
   TESTINST4("ins $t0, $t1, 0, 4",  0xff865421, 0x0,        t0, t1, 0, 4);
   TESTINST4("ins $t0, $t1, 0, 4",  0xff865421, 0xffffffff, t0, t1, 0, 4);
   TESTINST4("ins $t0, $t1, 0, 4",  0xff865421, 0x98765432, t0, t1, 0, 4);
   TESTINST4("ins $t0, $t1, 0, 4",  0xff865421, 0xff865421, t0, t1, 0, 4);
   TESTINST4("ins $t0, $t1, 0, 16", 0x0,        0x0,        t0, t1, 0, 16);
   TESTINST4("ins $t0, $t1, 0, 16", 0x0,        0xffffffff, t0, t1, 0, 16);
   TESTINST4("ins $t0, $t1, 0, 16", 0x0,        0x98765432, t0, t1, 0, 16);
   TESTINST4("ins $t0, $t1, 0, 16", 0x0,        0xff865421, t0, t1, 0, 16);
   TESTINST4("ins $t0, $t1, 0, 16", 0xffffffff, 0x0,        t0, t1, 0, 16);
   TESTINST4("ins $t0, $t1, 0, 16", 0xffffffff, 0xffffffff, t0, t1, 0, 16);
   TESTINST4("ins $t0, $t1, 0, 16", 0xffffffff, 0x98765432, t0, t1, 0, 16);
   TESTINST4("ins $t0, $t1, 0, 16", 0xffffffff, 0xff865421, t0, t1, 0, 16);
   TESTINST4("ins $t0, $t1, 0, 16", 0x98765432, 0x0,        t0, t1, 0, 16);
   TESTINST4("ins $t0, $t1, 0, 16", 0x98765432, 0xffffffff, t0, t1, 0, 16);
   TESTINST4("ins $t0, $t1, 0, 16", 0x98765432, 0x98765432, t0, t1, 0, 16);
   TESTINST4("ins $t0, $t1, 0, 16", 0x98765432, 0xff865421, t0, t1, 0, 16);
   TESTINST4("ins $t0, $t1, 0, 16", 0xff865421, 0x0,        t0, t1, 0, 16);
   TESTINST4("ins $t0, $t1, 0, 16", 0xff865421, 0xffffffff, t0, t1, 0, 16);
   TESTINST4("ins $t0, $t1, 0, 16", 0xff865421, 0x98765432, t0, t1, 0, 16);
   TESTINST4("ins $t0, $t1, 0, 16", 0xff865421, 0xff865421, t0, t1, 0, 16);
   TESTINST4("ins $t0, $t1, 0, 32", 0x0,        0x0,        t0, t1, 0, 32);
   TESTINST4("ins $t0, $t1, 0, 32", 0x0,        0xffffffff, t0, t1, 0, 32);
   TESTINST4("ins $t0, $t1, 0, 32", 0x0,        0x98765432, t0, t1, 0, 32);
   TESTINST4("ins $t0, $t1, 0, 32", 0x0,        0xff865421, t0, t1, 0, 32);
   TESTINST4("ins $t0, $t1, 0, 32", 0xffffffff, 0x0,        t0, t1, 0, 32);
   TESTINST4("ins $t0, $t1, 0, 32", 0xffffffff, 0xffffffff, t0, t1, 0, 32);
   TESTINST4("ins $t0, $t1, 0, 32", 0xffffffff, 0x98765432, t0, t1, 0, 32);
   TESTINST4("ins $t0, $t1, 0, 32", 0xffffffff, 0xff865421, t0, t1, 0, 32);
   TESTINST4("ins $t0, $t1, 0, 32", 0x98765432, 0x0,        t0, t1, 0, 32);
   TESTINST4("ins $t0, $t1, 0, 32", 0x98765432, 0xffffffff, t0, t1, 0, 32);
   TESTINST4("ins $t0, $t1, 0, 32", 0x98765432, 0x98765432, t0, t1, 0, 32);
   TESTINST4("ins $t0, $t1, 0, 32", 0x98765432, 0xff865421, t0, t1, 0, 32);
   TESTINST4("ins $t0, $t1, 0, 32", 0xff865421, 0x0,        t0, t1, 0, 32);
   TESTINST4("ins $t0, $t1, 0, 32", 0xff865421, 0xffffffff, t0, t1, 0, 32);
   TESTINST4("ins $t0, $t1, 0, 32", 0xff865421, 0x98765432, t0, t1, 0, 32);
   TESTINST4("ins $t0, $t1, 0, 32", 0xff865421, 0xff865421, t0, t1, 0, 32);

   TESTINST4("ins $t0, $t1, 4, 1",  0x0,        0x0,        t0, t1, 4, 1);
   TESTINST4("ins $t0, $t1, 4, 1",  0x0,        0xffffffff, t0, t1, 4, 1);
   TESTINST4("ins $t0, $t1, 4, 1",  0x0,        0x98765432, t0, t1, 4, 1);
   TESTINST4("ins $t0, $t1, 4, 1",  0x0,        0xff865421, t0, t1, 4, 1);
   TESTINST4("ins $t0, $t1, 4, 1",  0xffffffff, 0x0,        t0, t1, 4, 1);
   TESTINST4("ins $t0, $t1, 4, 1",  0xffffffff, 0xffffffff, t0, t1, 4, 1);
   TESTINST4("ins $t0, $t1, 4, 1",  0xffffffff, 0x98765432, t0, t1, 4, 1);
   TESTINST4("ins $t0, $t1, 4, 1",  0xffffffff, 0xff865421, t0, t1, 4, 1);
   TESTINST4("ins $t0, $t1, 4, 1",  0x98765432, 0x0,        t0, t1, 4, 1);
   TESTINST4("ins $t0, $t1, 4, 1",  0x98765432, 0xffffffff, t0, t1, 4, 1);
   TESTINST4("ins $t0, $t1, 4, 1",  0x98765432, 0x98765432, t0, t1, 4, 1);
   TESTINST4("ins $t0, $t1, 4, 1",  0x98765432, 0xff865421, t0, t1, 4, 1);
   TESTINST4("ins $t0, $t1, 4, 1",  0xff865421, 0x0,        t0, t1, 4, 1);
   TESTINST4("ins $t0, $t1, 4, 1",  0xff865421, 0xffffffff, t0, t1, 4, 1);
   TESTINST4("ins $t0, $t1, 4, 1",  0xff865421, 0x98765432, t0, t1, 4, 1);
   TESTINST4("ins $t0, $t1, 4, 1",  0xff865421, 0xff865421, t0, t1, 4, 1);
   TESTINST4("ins $t0, $t1, 4, 4",  0x0,        0x0,        t0, t1, 4, 4);
   TESTINST4("ins $t0, $t1, 4, 4",  0x0,        0xffffffff, t0, t1, 4, 4);
   TESTINST4("ins $t0, $t1, 4, 4",  0x0,        0x98765432, t0, t1, 4, 4);
   TESTINST4("ins $t0, $t1, 4, 4",  0x0,        0xff865421, t0, t1, 4, 4);
   TESTINST4("ins $t0, $t1, 4, 4",  0xffffffff, 0x0,        t0, t1, 4, 4);
   TESTINST4("ins $t0, $t1, 4, 4",  0xffffffff, 0xffffffff, t0, t1, 4, 4);
   TESTINST4("ins $t0, $t1, 4, 4",  0xffffffff, 0x98765432, t0, t1, 4, 4);
   TESTINST4("ins $t0, $t1, 4, 4",  0xffffffff, 0xff865421, t0, t1, 4, 4);
   TESTINST4("ins $t0, $t1, 4, 4",  0x98765432, 0x0,        t0, t1, 4, 4);
   TESTINST4("ins $t0, $t1, 4, 4",  0x98765432, 0xffffffff, t0, t1, 4, 4);
   TESTINST4("ins $t0, $t1, 4, 4",  0x98765432, 0x98765432, t0, t1, 4, 4);
   TESTINST4("ins $t0, $t1, 4, 4",  0x98765432, 0xff865421, t0, t1, 4, 4);
   TESTINST4("ins $t0, $t1, 4, 4",  0xff865421, 0x0,        t0, t1, 4, 4);
   TESTINST4("ins $t0, $t1, 4, 4",  0xff865421, 0xffffffff, t0, t1, 4, 4);
   TESTINST4("ins $t0, $t1, 4, 4",  0xff865421, 0x98765432, t0, t1, 4, 4);
   TESTINST4("ins $t0, $t1, 4, 4",  0xff865421, 0xff865421, t0, t1, 4, 4);
   TESTINST4("ins $t0, $t1, 4, 16", 0x0,        0x0,        t0, t1, 4, 16);
   TESTINST4("ins $t0, $t1, 4, 16", 0x0,        0xffffffff, t0, t1, 4, 16);
   TESTINST4("ins $t0, $t1, 4, 16", 0x0,        0x98765432, t0, t1, 4, 16);
   TESTINST4("ins $t0, $t1, 4, 16", 0x0,        0xff865421, t0, t1, 4, 16);
   TESTINST4("ins $t0, $t1, 4, 16", 0xffffffff, 0x0,        t0, t1, 4, 16);
   TESTINST4("ins $t0, $t1, 4, 16", 0xffffffff, 0xffffffff, t0, t1, 4, 16);
   TESTINST4("ins $t0, $t1, 4, 16", 0xffffffff, 0x98765432, t0, t1, 4, 16);
   TESTINST4("ins $t0, $t1, 4, 16", 0xffffffff, 0xff865421, t0, t1, 4, 16);
   TESTINST4("ins $t0, $t1, 4, 16", 0x98765432, 0x0,        t0, t1, 4, 16);
   TESTINST4("ins $t0, $t1, 4, 16", 0x98765432, 0xffffffff, t0, t1, 4, 16);
   TESTINST4("ins $t0, $t1, 4, 16", 0x98765432, 0x98765432, t0, t1, 4, 16);
   TESTINST4("ins $t0, $t1, 4, 16", 0x98765432, 0xff865421, t0, t1, 4, 16);
   TESTINST4("ins $t0, $t1, 4, 16", 0xff865421, 0x0,        t0, t1, 4, 16);
   TESTINST4("ins $t0, $t1, 4, 16", 0xff865421, 0xffffffff, t0, t1, 4, 16);
   TESTINST4("ins $t0, $t1, 4, 16", 0xff865421, 0x98765432, t0, t1, 4, 16);
   TESTINST4("ins $t0, $t1, 4, 16", 0xff865421, 0xff865421, t0, t1, 4, 16);
   TESTINST4("ins $t0, $t1, 4, 28", 0x0,        0x0,        t0, t1, 4, 28);
   TESTINST4("ins $t0, $t1, 4, 28", 0x0,        0xffffffff, t0, t1, 4, 28);
   TESTINST4("ins $t0, $t1, 4, 28", 0x0,        0x98765432, t0, t1, 4, 28);
   TESTINST4("ins $t0, $t1, 4, 28", 0x0,        0xff865421, t0, t1, 4, 28);
   TESTINST4("ins $t0, $t1, 4, 28", 0xffffffff, 0x0,        t0, t1, 4, 28);
   TESTINST4("ins $t0, $t1, 4, 28", 0xffffffff, 0xffffffff, t0, t1, 4, 28);
   TESTINST4("ins $t0, $t1, 4, 28", 0xffffffff, 0x98765432, t0, t1, 4, 28);
   TESTINST4("ins $t0, $t1, 4, 28", 0xffffffff, 0xff865421, t0, t1, 4, 28);
   TESTINST4("ins $t0, $t1, 4, 28", 0x98765432, 0x0,        t0, t1, 4, 28);
   TESTINST4("ins $t0, $t1, 4, 28", 0x98765432, 0xffffffff, t0, t1, 4, 28);
   TESTINST4("ins $t0, $t1, 4, 28", 0x98765432, 0x98765432, t0, t1, 4, 28);
   TESTINST4("ins $t0, $t1, 4, 28", 0x98765432, 0xff865421, t0, t1, 4, 28);
   TESTINST4("ins $t0, $t1, 4, 28", 0xff865421, 0x0,        t0, t1, 4, 28);
   TESTINST4("ins $t0, $t1, 4, 28", 0xff865421, 0xffffffff, t0, t1, 4, 28);
   TESTINST4("ins $t0, $t1, 4, 28", 0xff865421, 0x98765432, t0, t1, 4, 28);
   TESTINST4("ins $t0, $t1, 4, 28", 0xff865421, 0xff865421, t0, t1, 4, 28);

   TESTINST4("ins $t0, $t1, 16, 1",  0x0,        0x0,        t0, t1, 1, 16);
   TESTINST4("ins $t0, $t1, 16, 1",  0x0,        0xffffffff, t0, t1, 1, 16);
   TESTINST4("ins $t0, $t1, 16, 1",  0x0,        0x98765432, t0, t1, 1, 16);
   TESTINST4("ins $t0, $t1, 16, 1",  0x0,        0xff865421, t0, t1, 1, 16);
   TESTINST4("ins $t0, $t1, 16, 1",  0xffffffff, 0x0,        t0, t1, 1, 16);
   TESTINST4("ins $t0, $t1, 16, 1",  0xffffffff, 0xffffffff, t0, t1, 1, 16);
   TESTINST4("ins $t0, $t1, 16, 1",  0xffffffff, 0x98765432, t0, t1, 1, 16);
   TESTINST4("ins $t0, $t1, 16, 1",  0xffffffff, 0xff865421, t0, t1, 1, 16);
   TESTINST4("ins $t0, $t1, 16, 1",  0x98765432, 0x0,        t0, t1, 1, 16);
   TESTINST4("ins $t0, $t1, 16, 1",  0x98765432, 0xffffffff, t0, t1, 1, 16);
   TESTINST4("ins $t0, $t1, 16, 1",  0x98765432, 0x98765432, t0, t1, 1, 16);
   TESTINST4("ins $t0, $t1, 16, 1",  0x98765432, 0xff865421, t0, t1, 1, 16);
   TESTINST4("ins $t0, $t1, 16, 1",  0xff865421, 0x0,        t0, t1, 1, 16);
   TESTINST4("ins $t0, $t1, 16, 1",  0xff865421, 0xffffffff, t0, t1, 1, 16);
   TESTINST4("ins $t0, $t1, 16, 1",  0xff865421, 0x98765432, t0, t1, 1, 16);
   TESTINST4("ins $t0, $t1, 16, 1",  0xff865421, 0xff865421, t0, t1, 1, 16);
   TESTINST4("ins $t0, $t1, 16, 4",  0x0,        0x0,        t0, t1, 16, 4);
   TESTINST4("ins $t0, $t1, 16, 4",  0x0,        0xffffffff, t0, t1, 16, 4);
   TESTINST4("ins $t0, $t1, 16, 4",  0x0,        0x98765432, t0, t1, 16, 4);
   TESTINST4("ins $t0, $t1, 16, 4",  0x0,        0xff865421, t0, t1, 16, 4);
   TESTINST4("ins $t0, $t1, 16, 4",  0xffffffff, 0x0,        t0, t1, 16, 4);
   TESTINST4("ins $t0, $t1, 16, 4",  0xffffffff, 0xffffffff, t0, t1, 16, 4);
   TESTINST4("ins $t0, $t1, 16, 4",  0xffffffff, 0x98765432, t0, t1, 16, 4);
   TESTINST4("ins $t0, $t1, 16, 4",  0xffffffff, 0xff865421, t0, t1, 16, 4);
   TESTINST4("ins $t0, $t1, 16, 4",  0x98765432, 0x0,        t0, t1, 16, 4);
   TESTINST4("ins $t0, $t1, 16, 4",  0x98765432, 0xffffffff, t0, t1, 16, 4);
   TESTINST4("ins $t0, $t1, 16, 4",  0x98765432, 0x98765432, t0, t1, 16, 4);
   TESTINST4("ins $t0, $t1, 16, 4",  0x98765432, 0xff865421, t0, t1, 16, 4);
   TESTINST4("ins $t0, $t1, 16, 4",  0xff865421, 0x0,        t0, t1, 16, 4);
   TESTINST4("ins $t0, $t1, 16, 4",  0xff865421, 0xffffffff, t0, t1, 16, 4);
   TESTINST4("ins $t0, $t1, 16, 4",  0xff865421, 0x98765432, t0, t1, 16, 4);
   TESTINST4("ins $t0, $t1, 16, 4",  0xff865421, 0xff865421, t0, t1, 16, 4);
   TESTINST4("ins $t0, $t1, 16, 16", 0x0,        0x0,        t0, t1, 16, 16);
   TESTINST4("ins $t0, $t1, 16, 16", 0x0,        0xffffffff, t0, t1, 16, 16);
   TESTINST4("ins $t0, $t1, 16, 16", 0x0,        0x98765432, t0, t1, 16, 16);
   TESTINST4("ins $t0, $t1, 16, 16", 0x0,        0xff865421, t0, t1, 16, 16);
   TESTINST4("ins $t0, $t1, 16, 16", 0xffffffff, 0x0,        t0, t1, 16, 16);
   TESTINST4("ins $t0, $t1, 16, 16", 0xffffffff, 0xffffffff, t0, t1, 16, 16);
   TESTINST4("ins $t0, $t1, 16, 16", 0xffffffff, 0x98765432, t0, t1, 16, 16);
   TESTINST4("ins $t0, $t1, 16, 16", 0xffffffff, 0xff865421, t0, t1, 16, 16);
   TESTINST4("ins $t0, $t1, 16, 16", 0x98765432, 0x0,        t0, t1, 16, 16);
   TESTINST4("ins $t0, $t1, 16, 16", 0x98765432, 0xffffffff, t0, t1, 16, 16);
   TESTINST4("ins $t0, $t1, 16, 16", 0x98765432, 0x98765432, t0, t1, 16, 16);
   TESTINST4("ins $t0, $t1, 16, 16", 0x98765432, 0xff865421, t0, t1, 16, 16);
   TESTINST4("ins $t0, $t1, 16, 16", 0xff865421, 0x0,        t0, t1, 16, 16);
   TESTINST4("ins $t0, $t1, 16, 16", 0xff865421, 0xffffffff, t0, t1, 16, 16);
   TESTINST4("ins $t0, $t1, 16, 16", 0xff865421, 0x98765432, t0, t1, 16, 16);
   TESTINST4("ins $t0, $t1, 16, 16", 0xff865421, 0xff865421, t0, t1, 16, 16);

   TESTINST4("ins $t0, $t1, 31, 1", 0x0,        0x0,        t0, t1, 31, 1);
   TESTINST4("ins $t0, $t1, 31, 1", 0x0,        0xffffffff, t0, t1, 31, 1);
   TESTINST4("ins $t0, $t1, 31, 1", 0x0,        0x98765432, t0, t1, 31, 1);
   TESTINST4("ins $t0, $t1, 31, 1", 0x0,        0xff865421, t0, t1, 31, 1);
   TESTINST4("ins $t0, $t1, 31, 1", 0xffffffff, 0x0,        t0, t1, 31, 1);
   TESTINST4("ins $t0, $t1, 31, 1", 0xffffffff, 0xffffffff, t0, t1, 31, 1);
   TESTINST4("ins $t0, $t1, 31, 1", 0xffffffff, 0x98765432, t0, t1, 31, 1);
   TESTINST4("ins $t0, $t1, 31, 1", 0xffffffff, 0xff865421, t0, t1, 31, 1);
   TESTINST4("ins $t0, $t1, 31, 1", 0x98765432, 0x0,        t0, t1, 31, 1);
   TESTINST4("ins $t0, $t1, 31, 1", 0x98765432, 0xffffffff, t0, t1, 31, 1);
   TESTINST4("ins $t0, $t1, 31, 1", 0x98765432, 0x98765432, t0, t1, 31, 1);
   TESTINST4("ins $t0, $t1, 31, 1", 0x98765432, 0xff865421, t0, t1, 31, 1);
   TESTINST4("ins $t0, $t1, 31, 1", 0xff865421, 0x0,        t0, t1, 31, 1);
   TESTINST4("ins $t0, $t1, 31, 1", 0xff865421, 0xffffffff, t0, t1, 31, 1);
   TESTINST4("ins $t0, $t1, 31, 1", 0xff865421, 0x98765432, t0, t1, 31, 1);
   TESTINST4("ins $t0, $t1, 31, 1", 0xff865421, 0xff865421, t0, t1, 31, 1);
#endif

   printf("LB\n");
   TESTINSN5LOAD("lb $t0, 0($t1)", 0, 0, t0);
   TESTINSN5LOAD("lb $t0, 4($t1)", 0, 4, t0);
   TESTINSN5LOAD("lb $t0, 8($t1)", 0, 8, t0);
   TESTINSN5LOAD("lb $t0, 12($t1)", 0, 12, t0);
   TESTINSN5LOAD("lb $t0, 16($t1)", 0, 16, t0);
   TESTINSN5LOAD("lb $t0, 20($t1)", 0, 20, t0);
   TESTINSN5LOAD("lb $t0, 24($t1)", 0, 24, t0);
   TESTINSN5LOAD("lb $t0, 28($t1)", 0, 28, t0);
   TESTINSN5LOAD("lb $t0, 32($t1)", 0, 32, t0);
   TESTINSN5LOAD("lb $t0, 36($t1)", 0, 36, t0);
   TESTINSN5LOAD("lb $t0, 40($t1)", 0, 40, t0);
   TESTINSN5LOAD("lb $t0, 44($t1)", 0, 44, t0);
   TESTINSN5LOAD("lb $t0, 48($t1)", 0, 48, t0);
   TESTINSN5LOAD("lb $t0, 52($t1)", 0, 52, t0);
   TESTINSN5LOAD("lb $t0, 56($t1)", 0, 56, t0);
   TESTINSN5LOAD("lb $t0, 60($t1)", 0, 60, t0);
   TESTINSN5LOAD("lb $t0, 1($t1)", 0, 1, t0);
   TESTINSN5LOAD("lb $t0, 2($t1)", 0, 2, t0);
   TESTINSN5LOAD("lb $t0, 6($t1)", 0, 6, t0);
   TESTINSN5LOAD("lb $t0, 10($t1)", 0, 10, t0);
   TESTINSN5LOAD("lb $t0, 14($t1)", 0, 14, t0);
   TESTINSN5LOAD("lb $t0, 18($t1)", 0, 18, t0);
   TESTINSN5LOAD("lb $t0, 22($t1)", 0, 22, t0);
   TESTINSN5LOAD("lb $t0, 26($t1)", 0, 26, t0);
   TESTINSN5LOAD("lb $t0, 30($t1)", 0, 30, t0);
   TESTINSN5LOAD("lb $t0, 34($t1)", 0, 34, t0);
   TESTINSN5LOAD("lb $t0, 38($t1)", 0, 38, t0);

   printf("LBU\n");
   TESTINSN5LOAD("lbu $t0, 0($t1)", 0, 0, t0);
   TESTINSN5LOAD("lbu $t0, 4($t1)", 0, 4, t0);
   TESTINSN5LOAD("lbu $t0, 8($t1)", 0, 8, t0);
   TESTINSN5LOAD("lbu $t0, 12($t1)", 0, 12, t0);
   TESTINSN5LOAD("lbu $t0, 16($t1)", 0, 16, t0);
   TESTINSN5LOAD("lbu $t0, 20($t1)", 0, 20, t0);
   TESTINSN5LOAD("lbu $t0, 24($t1)", 0, 24, t0);
   TESTINSN5LOAD("lbu $t0, 28($t1)", 0, 28, t0);
   TESTINSN5LOAD("lbu $t0, 32($t1)", 0, 32, t0);
   TESTINSN5LOAD("lbu $t0, 36($t1)", 0, 36, t0);
   TESTINSN5LOAD("lbu $t0, 40($t1)", 0, 40, t0);
   TESTINSN5LOAD("lbu $t0, 44($t1)", 0, 44, t0);
   TESTINSN5LOAD("lbu $t0, 48($t1)", 0, 48, t0);
   TESTINSN5LOAD("lbu $t0, 52($t1)", 0, 52, t0);
   TESTINSN5LOAD("lbu $t0, 56($t1)", 0, 56, t0);
   TESTINSN5LOAD("lbu $t0, 60($t1)", 0, 60, t0);
   TESTINSN5LOAD("lbu $t0, 1($t1)", 0, 1, t0);
   TESTINSN5LOAD("lbu $t0, 2($t1)", 0, 2, t0);
   TESTINSN5LOAD("lbu $t0, 6($t1)", 0, 6, t0);
   TESTINSN5LOAD("lbu $t0, 10($t1)", 0, 10, t0);
   TESTINSN5LOAD("lbu $t0, 14($t1)", 0, 14, t0);
   TESTINSN5LOAD("lbu $t0, 18($t1)", 0, 18, t0);
   TESTINSN5LOAD("lbu $t0, 22($t1)", 0, 22, t0);
   TESTINSN5LOAD("lbu $t0, 26($t1)", 0, 26, t0);
   TESTINSN5LOAD("lbu $t0, 30($t1)", 0, 30, t0);
   TESTINSN5LOAD("lbu $t0, 34($t1)", 0, 34, t0);
   TESTINSN5LOAD("lbu $t0, 38($t1)", 0, 38, t0);

   printf("LH\n");
   TESTINSN5LOAD("lh $t0, 0($t1)", 0, 0, t0);
   TESTINSN5LOAD("lh $t0, 4($t1)", 0, 4, t0);
   TESTINSN5LOAD("lh $t0, 8($t1)", 0, 8, t0);
   TESTINSN5LOAD("lh $t0, 12($t1)", 0, 12, t0);
   TESTINSN5LOAD("lh $t0, 16($t1)", 0, 16, t0);
   TESTINSN5LOAD("lh $t0, 20($t1)", 0, 20, t0);
   TESTINSN5LOAD("lh $t0, 24($t1)", 0, 24, t0);
   TESTINSN5LOAD("lh $t0, 28($t1)", 0, 28, t0);
   TESTINSN5LOAD("lh $t0, 32($t1)", 0, 32, t0);
   TESTINSN5LOAD("lh $t0, 36($t1)", 0, 36, t0);
   TESTINSN5LOAD("lh $t0, 40($t1)", 0, 40, t0);
   TESTINSN5LOAD("lh $t0, 44($t1)", 0, 44, t0);
   TESTINSN5LOAD("lh $t0, 48($t1)", 0, 48, t0);
   TESTINSN5LOAD("lh $t0, 52($t1)", 0, 52, t0);
   TESTINSN5LOAD("lh $t0, 56($t1)", 0, 56, t0);
   TESTINSN5LOAD("lh $t0, 60($t1)", 0, 60, t0);
   TESTINSN5LOAD("lh $t0, 62($t1)", 0, 62, t0);
   TESTINSN5LOAD("lh $t0, 2($t1)", 0, 2, t0);
   TESTINSN5LOAD("lh $t0, 6($t1)", 0, 6, t0);
   TESTINSN5LOAD("lh $t0, 10($t1)", 0, 10, t0);
   TESTINSN5LOAD("lh $t0, 14($t1)", 0, 14, t0);
   TESTINSN5LOAD("lh $t0, 18($t1)", 0, 18, t0);
   TESTINSN5LOAD("lh $t0, 22($t1)", 0, 22, t0);
   TESTINSN5LOAD("lh $t0, 26($t1)", 0, 26, t0);
   TESTINSN5LOAD("lh $t0, 30($t1)", 0, 30, t0);
   TESTINSN5LOAD("lh $t0, 34($t1)", 0, 34, t0);
   TESTINSN5LOAD("lh $t0, 38($t1)", 0, 38, t0);

   printf("LHU\n");
   TESTINSN5LOAD("lhu $t0, 0($t1)", 0, 0, t0);
   TESTINSN5LOAD("lhu $t0, 4($t1)", 0, 4, t0);
   TESTINSN5LOAD("lhu $t0, 8($t1)", 0, 8, t0);
   TESTINSN5LOAD("lhu $t0, 12($t1)", 0, 12, t0);
   TESTINSN5LOAD("lhu $t0, 16($t1)", 0, 16, t0);
   TESTINSN5LOAD("lhu $t0, 20($t1)", 0, 20, t0);
   TESTINSN5LOAD("lhu $t0, 24($t1)", 0, 24, t0);
   TESTINSN5LOAD("lhu $t0, 28($t1)", 0, 28, t0);
   TESTINSN5LOAD("lhu $t0, 32($t1)", 0, 32, t0);
   TESTINSN5LOAD("lhu $t0, 36($t1)", 0, 36, t0);
   TESTINSN5LOAD("lhu $t0, 40($t1)", 0, 40, t0);
   TESTINSN5LOAD("lhu $t0, 44($t1)", 0, 44, t0);
   TESTINSN5LOAD("lhu $t0, 48($t1)", 0, 48, t0);
   TESTINSN5LOAD("lhu $t0, 52($t1)", 0, 52, t0);
   TESTINSN5LOAD("lhu $t0, 56($t1)", 0, 56, t0);
   TESTINSN5LOAD("lhu $t0, 60($t1)", 0, 60, t0);
   TESTINSN5LOAD("lhu $t0, 62($t1)", 0, 62, t0);
   TESTINSN5LOAD("lhu $t0, 2($t1)", 0, 2, t0);
   TESTINSN5LOAD("lhu $t0, 6($t1)", 0, 6, t0);
   TESTINSN5LOAD("lhu $t0, 10($t1)", 0, 10, t0);
   TESTINSN5LOAD("lhu $t0, 14($t1)", 0, 14, t0);
   TESTINSN5LOAD("lhu $t0, 18($t1)", 0, 18, t0);
   TESTINSN5LOAD("lhu $t0, 22($t1)", 0, 22, t0);
   TESTINSN5LOAD("lhu $t0, 26($t1)", 0, 26, t0);
   TESTINSN5LOAD("lhu $t0, 30($t1)", 0, 30, t0);
   TESTINSN5LOAD("lhu $t0, 34($t1)", 0, 34, t0);
   TESTINSN5LOAD("lhu $t0, 38($t1)", 0, 38, t0);

   printf("LUI\n");
   TESTINST3("lui  $t0, 0xffff", 0xffff, t0, t1);
   TESTINST3("lui  $t0, 0xff00", 0xff00, t0, t1);
   TESTINST3("lui  $t0, 0xff", 0xff, t0, t1);
   TESTINST3("lui  $t0, 0x0", 0x0, t0, t1);
   TESTINST3("lui  $t0, 0x5", 0x5, t0, t1);
   TESTINST3("lui  $t0, 0x387", 0x387, t0, t1);

   printf("LW\n");
   TESTINSN5LOAD("lw $t0, 0($t1)", 0, 0, t0);
   TESTINSN5LOAD("lw $t0, 4($t1)", 0, 4, t0);
   TESTINSN5LOAD("lw $t0, 8($t1)", 0, 8, t0);
   TESTINSN5LOAD("lw $t0, 12($t1)", 0, 12, t0);
   TESTINSN5LOAD("lw $t0, 16($t1)", 0, 16, t0);
   TESTINSN5LOAD("lw $t0, 20($t1)", 0, 20, t0);
   TESTINSN5LOAD("lw $t0, 24($t1)", 0, 24, t0);
   TESTINSN5LOAD("lw $t0, 28($t1)", 0, 28, t0);
   TESTINSN5LOAD("lw $t0, 32($t1)", 0, 32, t0);
   TESTINSN5LOAD("lw $t0, 36($t1)", 0, 36, t0);
   TESTINSN5LOAD("lw $t0, 40($t1)", 0, 40, t0);
   TESTINSN5LOAD("lw $t0, 44($t1)", 0, 44, t0);
   TESTINSN5LOAD("lw $t0, 48($t1)", 0, 48, t0);
   TESTINSN5LOAD("lw $t0, 52($t1)", 0, 52, t0);
   TESTINSN5LOAD("lw $t0, 56($t1)", 0, 56, t0);
   TESTINSN5LOAD("lw $t0, 60($t1)", 0, 60, t0);
   TESTINSN5LOAD("lw $t0, 2($t1)", 0, 2, t0);
   TESTINSN5LOAD("lw $t0, 6($t1)", 0, 6, t0);
   TESTINSN5LOAD("lw $t0, 10($t1)", 0, 10, t0);
   TESTINSN5LOAD("lw $t0, 14($t1)", 0, 14, t0);
   TESTINSN5LOAD("lw $t0, 18($t1)", 0, 18, t0);
   TESTINSN5LOAD("lw $t0, 22($t1)", 0, 22, t0);
   TESTINSN5LOAD("lw $t0, 26($t1)", 0, 26, t0);
   TESTINSN5LOAD("lw $t0, 30($t1)", 0, 30, t0);
   TESTINSN5LOAD("lw $t0, 34($t1)", 0, 34, t0);
   TESTINSN5LOAD("lw $t0, 38($t1)", 0, 38, t0);

#if (__mips_isa_rev < 6)
   printf("LWL\n");
   TESTINSN5LOAD("lwl $t0, 3($t1)", 0, 3, t0);
   TESTINSN5LOAD("lwl $t0, 6($t1)", 0, 6, t0);
   TESTINSN5LOAD("lwl $t0, 9($t1)", 0, 9, t0);
   TESTINSN5LOAD("lwl $t0, 12($t1)", 0, 12, t0);
   TESTINSN5LOAD("lwl $t0, 15($t1)", 0, 15, t0);
   TESTINSN5LOAD("lwl $t0, 18($t1)", 0, 18, t0);
   TESTINSN5LOAD("lwl $t0, 21($t1)", 0, 21, t0);
   TESTINSN5LOAD("lwl $t0, 24($t1)", 0, 24, t0);
   TESTINSN5LOAD("lwl $t0, 27($t1)", 0, 27, t0);
   TESTINSN5LOAD("lwl $t0, 30($t1)", 0, 30, t0);
   TESTINSN5LOAD("lwl $t0, 33($t1)", 0, 33, t0);
   TESTINSN5LOAD("lwl $t0, 36($t1)", 0, 36, t0);
   TESTINSN5LOAD("lwl $t0, 39($t1)", 0, 39, t0);
   TESTINSN5LOAD("lwl $t0, 42($t1)", 0, 42, t0);
   TESTINSN5LOAD("lwl $t0, 45($t1)", 0, 45, t0);
   TESTINSN5LOAD("lwl $t0, 48($t1)", 0, 48, t0);
   TESTINSN5LOAD("lwl $t0, 51($t1)", 0, 51, t0);
   TESTINSN5LOAD("lwl $t0, 54($t1)", 0, 54, t0);
   TESTINSN5LOAD("lwl $t0, 57($t1)", 0, 57, t0);
   TESTINSN5LOAD("lwl $t0, 60($t1)", 0, 60, t0);

   printf("LWR\n");
   TESTINSN5LOAD("lwr $t0, 3($t1)", 0, 0, t0);
   TESTINSN5LOAD("lwr $t0, 6($t1)", 0, 4, t0);
   TESTINSN5LOAD("lwr $t0, 9($t1)", 0, 8, t0);
   TESTINSN5LOAD("lwr $t0, 12($t1)", 0, 12, t0);
   TESTINSN5LOAD("lwr $t0, 15($t1)", 0, 16, t0);
   TESTINSN5LOAD("lwr $t0, 18($t1)", 0, 20, t0);
   TESTINSN5LOAD("lwr $t0, 21($t1)", 0, 24, t0);
   TESTINSN5LOAD("lwr $t0, 24($t1)", 0, 28, t0);
   TESTINSN5LOAD("lwr $t0, 27($t1)", 0, 32, t0);
   TESTINSN5LOAD("lwr $t0, 30($t1)", 0, 36, t0);
   TESTINSN5LOAD("lwr $t0, 33($t1)", 0, 40, t0);
   TESTINSN5LOAD("lwr $t0, 36($t1)", 0, 44, t0);
   TESTINSN5LOAD("lwr $t0, 39($t1)", 0, 48, t0);
   TESTINSN5LOAD("lwr $t0, 42($t1)", 0, 52, t0);
   TESTINSN5LOAD("lwr $t0, 45($t1)", 0, 56, t0);
   TESTINSN5LOAD("lwr $t0, 48($t1)", 0, 60, t0);
   TESTINSN5LOAD("lwr $t0, 51($t1)", 0, 64, t0);
   TESTINSN5LOAD("lwr $t0, 54($t1)", 0, 2, t0);
   TESTINSN5LOAD("lwr $t0, 57($t1)", 0, 6, t0);
   TESTINSN5LOAD("lwr $t0, 60($t1)", 0, 10, t0);

   printf("MADD\n");
   TESTINST3a("madd  $t0, $t1", 0x6, 0x2, t0, t1);
   TESTINST3a("madd  $t0, $t1", 0x55, 0x28, t0, t1);
   TESTINST3a("madd  $t0, $t1", 0x18, 0xfff, t0, t1);
   TESTINST3a("madd  $t0, $t1", 0x7fffffff, 0x7fffffff, t0, t1);
   TESTINST3a("madd  $t0, $t1", 0xffffffff, 0x1, t0, t1);
   TESTINST3a("madd  $t0, $t1", 0x1, 0xffffffff, t0, t1);
   TESTINST3a("madd  $t0, $t1", 0x2, 0x6, t0, t1);
   TESTINST3a("madd  $t0, $t1", 0x356, 0x555, t0, t1);

   printf("MADDU\n");
   TESTINST3a("maddu  $t0, $t1", 0x6, 0x2, t0, t1);
   TESTINST3a("maddu  $t0, $t1", 0x55, 0x28, t0, t1);
   TESTINST3a("maddu  $t0, $t1", 0x18, 0xfff, t0, t1);
   TESTINST3a("maddu  $t0, $t1", 0x7fffffff, 0x7fffffff, t0, t1);
   TESTINST3a("maddu  $t0, $t1", 0xffffffff, 0x1, t0, t1);
   TESTINST3a("maddu  $t0, $t1", 0x1, 0xffffffff, t0, t1);
   TESTINST3a("maddu  $t0, $t1", 0x2, 0x6, t0, t1);
   TESTINST3a("maddu  $t0, $t1", 0x356, 0x555, t0, t1);

   printf("MOVN\n");
   TESTINST1("movn $t0, $t1, $t2", 0x31415927, 0,          t0, t1, t2);
   TESTINST1("movn $t0, $t1, $t2", 0x31415927, 1,          t0, t1, t2);
   TESTINST1("movn $t0, $t1, $t2", 0,          255,        t0, t1, t2);
   TESTINST1("movn $t0, $t1, $t2", -1,         0,          t0, t1, t2);
   TESTINST1("movn $t0, $t1, $t2", 0,          1,          t0, t1, t2);
   TESTINST1("movn $t0, $t1, $t2", 0,          0,          t0, t1, t2);
   TESTINST1("movn $t0, $t1, $t2", 0x80000000, -1,         t0, t1, t2);
   TESTINST1("movn $t0, $t1, $t2", 0x80000000, 1,          t0, t1, t2);
   TESTINST1("movn $t0, $t1, $t2", 0x7fffffff, 1,          t0, t1, t2);
   TESTINST1("movn $t0, $t1, $t2", 0x80000000, 0,          t0, t1, t2);
   TESTINST1("movn $t0, $t1, $t2", 0x7fffffff, 0x80000000, t0, t1, t2);
   TESTINST1("movn $t0, $t1, $t2", 0x80000000, 1,          t0, t1, t2);
   TESTINST1("movn $t0, $t1, $t2", 0x7fffffff, 0,          t0, t1, t2);
   TESTINST1("movn $t0, $t1, $t2", 0x31415927, 0,          t0, t1, t2);
   TESTINST1("movn $t0, $t1, $t2", 0x31415927, 0xee00ee00, t0, t1, t2);
   TESTINST1("movn $t0, $t1, $t2", 0,          1,          t0, t1, t2);
   TESTINST1("movn $t0, $t1, $t2", 1,          0,          t0, t1, t2);
   TESTINST1("movn $t0, $t1, $t2", 0,          1,          t0, t1, t2);
   TESTINST1("movn $t0, $t1, $t2", -1,         0,          t0, t1, t2);
   TESTINST1("movn $t0, $t1, $t2", 0,          -1,         t0, t1, t2);
   TESTINST1("movn $t0, $t1, $t2", 0,          0x80000000, t0, t1, t2);
   TESTINST1("movn $t0, $t1, $t2", 0x80000000, 1,          t0, t1, t2);
   TESTINST1("movn $t0, $t1, $t2", 0x80000000, 0,          t0, t1, t2);
   TESTINST1("movn $t0, $t1, $t2", 0x7fffffff, 0x80000000, t0, t1, t2);
   TESTINST1("movn $t0, $t1, $t2", 0x80000000, 1,          t0, t1, t2);
   TESTINST1("movn $t0, $t1, $t2", 0x7fffffff, 0,          t0, t1, t2);

   printf("MOVZ\n");
   TESTINST1("movz $t0, $t1, $t2", 0x31415927, 0,          t0, t1, t2);
   TESTINST1("movz $t0, $t1, $t2", 0x31415927, 1,          t0, t1, t2);
   TESTINST1("movz $t0, $t1, $t2", 0,          255,        t0, t1, t2);
   TESTINST1("movz $t0, $t1, $t2", -1,         0,          t0, t1, t2);
   TESTINST1("movz $t0, $t1, $t2", 0,          1,          t0, t1, t2);
   TESTINST1("movz $t0, $t1, $t2", 0,          0,          t0, t1, t2);
   TESTINST1("movz $t0, $t1, $t2", 0x80000000, -1,         t0, t1, t2);
   TESTINST1("movz $t0, $t1, $t2", 0x80000000, 1,          t0, t1, t2);
   TESTINST1("movz $t0, $t1, $t2", 0x7fffffff, 1,          t0, t1, t2);
   TESTINST1("movz $t0, $t1, $t2", 0x80000000, 0,          t0, t1, t2);
   TESTINST1("movz $t0, $t1, $t2", 0x7fffffff, 0x80000000, t0, t1, t2);
   TESTINST1("movz $t0, $t1, $t2", 0x80000000, 1,          t0, t1, t2);
   TESTINST1("movz $t0, $t1, $t2", 0x7fffffff, 0,          t0, t1, t2);
   TESTINST1("movz $t0, $t1, $t2", 0x31415927, 0,          t0, t1, t2);
   TESTINST1("movz $t0, $t1, $t2", 0x31415927, 0xee00ee00, t0, t1, t2);
   TESTINST1("movz $t0, $t1, $t2", 0,          1,          t0, t1, t2);
   TESTINST1("movz $t0, $t1, $t2", 1,          0,          t0, t1, t2);
   TESTINST1("movz $t0, $t1, $t2", 0,          1,          t0, t1, t2);
   TESTINST1("movz $t0, $t1, $t2", -1,         0,          t0, t1, t2);
   TESTINST1("movz $t0, $t1, $t2", 0,          -1,         t0, t1, t2);
   TESTINST1("movz $t0, $t1, $t2", 0,          0x80000000, t0, t1, t2);
   TESTINST1("movz $t0, $t1, $t2", 0x80000000, 1,          t0, t1, t2);
   TESTINST1("movz $t0, $t1, $t2", 0x80000000, 0,          t0, t1, t2);
   TESTINST1("movz $t0, $t1, $t2", 0x7fffffff, 0x80000000, t0, t1, t2);
   TESTINST1("movz $t0, $t1, $t2", 0x80000000, 1,          t0, t1, t2);
   TESTINST1("movz $t0, $t1, $t2", 0x7fffffff, 0,          t0, t1, t2);

   printf("MSUB\n");
   TESTINST3a("msub  $t0, $t1", 0x6, 0x2, t0, t1);
   TESTINST3a("msub  $t0, $t1", 0x55, 0x28, t0, t1);
   TESTINST3a("msub  $t0, $t1", 0x18, 0xfff, t0, t1);
   TESTINST3a("msub  $t0, $t1", 0x7fffffff, 0x7fffffff, t0, t1);
   TESTINST3a("msub  $t0, $t1", 0xffffffff, 0x1, t0, t1);
   TESTINST3a("msub  $t0, $t1", 0x1, 0xffffffff, t0, t1);
   TESTINST3a("msub  $t0, $t1", 0x2, 0x6, t0, t1);
   TESTINST3a("msub  $t0, $t1", 0x356, 0x555, t0, t1);

   printf("MSUBU\n");
   TESTINST3a("msubu  $t0, $t1", 0x31415927, 0xffffffff, t0, t1);
   TESTINST3a("msubu  $t0, $t1", 0x31415927, 0xee00ee00, t0, t1);
   TESTINST3a("msubu  $t0, $t1", 0,          255,        t0, t1);
   TESTINST3a("msubu  $t0, $t1", -1,         0,          t0, t1);
   TESTINST3a("msubu  $t0, $t1", 0,          1,          t0, t1);
   TESTINST3a("msubu  $t0, $t1", 0,          0,          t0, t1);
   TESTINST3a("msubu  $t0, $t1", 0x80000000, -1,         t0, t1);
   TESTINST3a("msubu  $t0, $t1", 0x80000000, 0x80000000, t0, t1);
   TESTINST3a("msubu  $t0, $t1", 0x7fffffff, 0,          t0, t1);
   TESTINST3a("msubu  $t0, $t1", 0x80000000, 0x80000000, t0, t1);
   TESTINST3a("msubu  $t0, $t1", 0x7fffffff, 0x80000000, t0, t1);
   TESTINST3a("msubu  $t0, $t1", 0x80000000, 0xff000000, t0, t1);
   TESTINST3a("msubu  $t0, $t1", 0x7fffffff, 0x0dd00000, t0, t1);
   TESTINST3a("msubu  $t0, $t1", 0x31415927, 0xffffffff, t0, t1);
   TESTINST3a("msubu  $t0, $t1", 0x31415927, 0xee00ee00, t0, t1);
   TESTINST3a("msubu  $t0, $t1", 0,          255,        t0, t1);
   TESTINST3a("msubu  $t0, $t1", 1,          0,          t0, t1);
   TESTINST3a("msubu  $t0, $t1", 0,          1,          t0, t1);
   TESTINST3a("msubu  $t0, $t1", -1,         0,          t0, t1);
   TESTINST3a("msubu  $t0, $t1", 0,          -1,         t0, t1);
   TESTINST3a("msubu  $t0, $t1", 0,          0x80000000, t0, t1);
   TESTINST3a("msubu  $t0, $t1", 0x80000000, 0,          t0, t1);
   TESTINST3a("msubu  $t0, $t1", 0x80000000, 0x80000000, t0, t1);
   TESTINST3a("msubu  $t0, $t1", 0x7fffffff, 0x80000000, t0, t1);
   TESTINST3a("msubu  $t0, $t1", 0x80000000, 0xff000000, t0, t1);
   TESTINST3a("msubu  $t0, $t1", 0x7fffffff, 0x0dd00000, t0, t1);
   TESTINST3a("msubu  $t0, $t1", 0xffffffff, 0,          t0, t1);
   TESTINST3a("msubu  $t0, $t1", 0,          0xffffffff, t0, t1);
   TESTINST3a("msubu  $t0, $t1", 0xffffffff, 0xffffffff, t0, t1);
   TESTINST3a("msubu  $t0, $t1", 0x7fffffff, 0x7fffffff, t0, t1);
   TESTINST3a("msubu  $t0, $t1", 0x0000ffff, 0x0000ffff, t0, t1);
#endif

   printf("MUL\n");
   TESTINST1("mul $t0, $t1, $t2", 0x31415927, 0xffffffff, t0, t1, t2);
   TESTINST1("mul $t0, $t1, $t2", 0x31415927, 0xee00ee00, t0, t1, t2);
   TESTINST1("mul $t0, $t1, $t2", 0,          255,        t0, t1, t2);
   TESTINST1("mul $t0, $t1, $t2", -1,         0,          t0, t1, t2);
   TESTINST1("mul $t0, $t1, $t2", 0,          1,          t0, t1, t2);
   TESTINST1("mul $t0, $t1, $t2", 0,          0,          t0, t1, t2);
   TESTINST1("mul $t0, $t1, $t2", 0x80000000, -1,         t0, t1, t2);
   TESTINST1("mul $t0, $t1, $t2", 0x80000000, 0x80000000, t0, t1, t2);
   TESTINST1("mul $t0, $t1, $t2", 0x7fffffff, 0,          t0, t1, t2);
   TESTINST1("mul $t0, $t1, $t2", 0x80000000, 0x80000000, t0, t1, t2);
   TESTINST1("mul $t0, $t1, $t2", 0x7fffffff, 0x80000000, t0, t1, t2);
   TESTINST1("mul $t0, $t1, $t2", 0x80000000, 0xff000000, t0, t1, t2);
   TESTINST1("mul $t0, $t1, $t2", 0x7fffffff, 0x0dd00000, t0, t1, t2);
   TESTINST1("mul $t0, $t1, $t2", 0x31415927, 0xffffffff, t0, t1, t2);
   TESTINST1("mul $t0, $t1, $t2", 0x31415927, 0xee00ee00, t0, t1, t2);
   TESTINST1("mul $t0, $t1, $t2", 0,          255,        t0, t1, t2);
   TESTINST1("mul $t0, $t1, $t2", 1,          0,          t0, t1, t2);
   TESTINST1("mul $t0, $t1, $t2", 0,          1,          t0, t1, t2);
   TESTINST1("mul $t0, $t1, $t2", -1,         0,          t0, t1, t2);
   TESTINST1("mul $t0, $t1, $t2", 0,          -1,         t0, t1, t2);
   TESTINST1("mul $t0, $t1, $t2", 0,          0x80000000, t0, t1, t2);
   TESTINST1("mul $t0, $t1, $t2", 0x80000000, 0,          t0, t1, t2);
   TESTINST1("mul $t0, $t1, $t2", 0x80000000, 0x80000000, t0, t1, t2);
   TESTINST1("mul $t0, $t1, $t2", 0x7fffffff, 0x80000000, t0, t1, t2);
   TESTINST1("mul $t0, $t1, $t2", 0x80000000, 0xff000000, t0, t1, t2);
   TESTINST1("mul $t0, $t1, $t2", 0x7fffffff, 0x0dd00000, t0, t1, t2);
   TESTINST1("mul $t0, $t1, $t2", 0xffffffff, 0,          t0, t1, t2);
   TESTINST1("mul $t0, $t1, $t2", 0,          0xffffffff, t0, t1, t2);
   TESTINST1("mul $t0, $t1, $t2", 0xffffffff, 0xffffffff, t0, t1, t2);
   TESTINST1("mul $t0, $t1, $t2", 0x7fffffff, 0x7fffffff, t0, t1, t2);
   TESTINST1("mul $t0, $t1, $t2", 0x0000ffff, 0x0000ffff, t0, t1, t2);

#if (__mips_isa_rev < 6)
   printf("MULT\n");
   TESTINST3a("mult  $t0, $t1", 0x31415927, 0xffffffff, t0, t1);
   TESTINST3a("mult  $t0, $t1", 0x31415927, 0xee00ee00, t0, t1);
   TESTINST3a("mult  $t0, $t1", 0,          255,        t0, t1);
   TESTINST3a("mult  $t0, $t1", -1,         0,          t0, t1);
   TESTINST3a("mult  $t0, $t1", 0,          1,          t0, t1);
   TESTINST3a("mult  $t0, $t1", 0,          0,          t0, t1);
   TESTINST3a("mult  $t0, $t1", 0x80000000, -1,         t0, t1);
   TESTINST3a("mult  $t0, $t1", 0x80000000, 0x80000000, t0, t1);
   TESTINST3a("mult  $t0, $t1", 0x7fffffff, 0,          t0, t1);
   TESTINST3a("mult  $t0, $t1", 0x80000000, 0x80000000, t0, t1);
   TESTINST3a("mult  $t0, $t1", 0x7fffffff, 0x80000000, t0, t1);
   TESTINST3a("mult  $t0, $t1", 0x80000000, 0xff000000, t0, t1);
   TESTINST3a("mult  $t0, $t1", 0x7fffffff, 0x0dd00000, t0, t1);
   TESTINST3a("mult  $t0, $t1", 0x31415927, 0xffffffff, t0, t1);
   TESTINST3a("mult  $t0, $t1", 0x31415927, 0xee00ee00, t0, t1);
   TESTINST3a("mult  $t0, $t1", 0,          255,        t0, t1);
   TESTINST3a("mult  $t0, $t1", 1,          0,          t0, t1);
   TESTINST3a("mult  $t0, $t1", 0,          1,          t0, t1);
   TESTINST3a("mult  $t0, $t1", -1,         0,          t0, t1);
   TESTINST3a("mult  $t0, $t1", 0,          -1,         t0, t1);
   TESTINST3a("mult  $t0, $t1", 0,          0x80000000, t0, t1);
   TESTINST3a("mult  $t0, $t1", 0x80000000, 0,          t0, t1);
   TESTINST3a("mult  $t0, $t1", 0x80000000, 0x80000000, t0, t1);
   TESTINST3a("mult  $t0, $t1", 0x7fffffff, 0x80000000, t0, t1);
   TESTINST3a("mult  $t0, $t1", 0x80000000, 0xff000000, t0, t1);
   TESTINST3a("mult  $t0, $t1", 0x7fffffff, 0x0dd00000, t0, t1);
   TESTINST3a("mult  $t0, $t1", 0xffffffff, 0,          t0, t1);
   TESTINST3a("mult  $t0, $t1", 0,          0xffffffff, t0, t1);
   TESTINST3a("mult  $t0, $t1", 0xffffffff, 0xffffffff, t0, t1);
   TESTINST3a("mult  $t0, $t1", 0x7fffffff, 0x7fffffff, t0, t1);
   TESTINST3a("mult  $t0, $t1", 0x0000ffff, 0x0000ffff, t0, t1);

   printf("MULTU\n");
   TESTINST3a("multu  $t0, $t1", 0x31415927, 0xffffffff, t0, t1);
   TESTINST3a("multu  $t0, $t1", 0x31415927, 0xee00ee00, t0, t1);
   TESTINST3a("multu  $t0, $t1", 0,          255,        t0, t1);
   TESTINST3a("multu  $t0, $t1", -1,         0,          t0, t1);
   TESTINST3a("multu  $t0, $t1", 0,          1,          t0, t1);
   TESTINST3a("multu  $t0, $t1", 0,          0,          t0, t1);
   TESTINST3a("multu  $t0, $t1", 0x80000000, -1,         t0, t1);
   TESTINST3a("multu  $t0, $t1", 0x80000000, 0x80000000, t0, t1);
   TESTINST3a("multu  $t0, $t1", 0x7fffffff, 0,          t0, t1);
   TESTINST3a("multu  $t0, $t1", 0x80000000, 0x80000000, t0, t1);
   TESTINST3a("multu  $t0, $t1", 0x7fffffff, 0x80000000, t0, t1);
   TESTINST3a("multu  $t0, $t1", 0x80000000, 0xff000000, t0, t1);
   TESTINST3a("multu  $t0, $t1", 0x7fffffff, 0x0dd00000, t0, t1);
   TESTINST3a("multu  $t0, $t1", 0x31415927, 0xffffffff, t0, t1);
   TESTINST3a("multu  $t0, $t1", 0x31415927, 0xee00ee00, t0, t1);
   TESTINST3a("multu  $t0, $t1", 0,          255,        t0, t1);
   TESTINST3a("multu  $t0, $t1", 1,          0,          t0, t1);
   TESTINST3a("multu  $t0, $t1", 0,          1,          t0, t1);
   TESTINST3a("multu  $t0, $t1", -1,         0,          t0, t1);
   TESTINST3a("multu  $t0, $t1", 0,          -1,         t0, t1);
   TESTINST3a("multu  $t0, $t1", 0,          0x80000000, t0, t1);
   TESTINST3a("multu  $t0, $t1", 0x80000000, 0,          t0, t1);
   TESTINST3a("multu  $t0, $t1", 0x80000000, 0x80000000, t0, t1);
   TESTINST3a("multu  $t0, $t1", 0x7fffffff, 0x80000000, t0, t1);
   TESTINST3a("multu  $t0, $t1", 0x80000000, 0xff000000, t0, t1);
   TESTINST3a("multu  $t0, $t1", 0x7fffffff, 0x0dd00000, t0, t1);
   TESTINST3a("multu  $t0, $t1", 0xffffffff, 0,          t0, t1);
   TESTINST3a("multu  $t0, $t1", 0,          0xffffffff, t0, t1);
   TESTINST3a("multu  $t0, $t1", 0xffffffff, 0xffffffff, t0, t1);
   TESTINST3a("multu  $t0, $t1", 0x7fffffff, 0x7fffffff, t0, t1);
   TESTINST3a("multu  $t0, $t1", 0x0000ffff, 0x0000ffff, t0, t1);
#endif

   printf("NOR\n");
   TESTINST1("nor $t0, $t1, $t2", 0x31415927, 0xffffffff, t0, t1, t2);
   TESTINST1("nor $t0, $t1, $t2", 0x31415927, 0xee00ee00, t0, t1, t2);
   TESTINST1("nor $t0, $t1, $t2", 0,          255,        t0, t1, t2);
   TESTINST1("nor $t0, $t1, $t2", -1,         0,          t0, t1, t2);
   TESTINST1("nor $t0, $t1, $t2", 0,          1,          t0, t1, t2);
   TESTINST1("nor $t0, $t1, $t2", 0,          0,          t0, t1, t2);
   TESTINST1("nor $t0, $t1, $t2", 0x80000000, -1,         t0, t1, t2);
   TESTINST1("nor $t0, $t1, $t2", 0x80000000, 0x80000000, t0, t1, t2);
   TESTINST1("nor $t0, $t1, $t2", 0x7fffffff, 0,          t0, t1, t2);
   TESTINST1("nor $t0, $t1, $t2", 0x80000000, 0x80000000, t0, t1, t2);
   TESTINST1("nor $t0, $t1, $t2", 0x7fffffff, 0x80000000, t0, t1, t2);
   TESTINST1("nor $t0, $t1, $t2", 0x80000000, 0xff000000, t0, t1, t2);
   TESTINST1("nor $t0, $t1, $t2", 0x7fffffff, 0x0dd00000, t0, t1, t2);
   TESTINST1("nor $t0, $t1, $t2", 0x31415927, 0xffffffff, t0, t1, t2);
   TESTINST1("nor $t0, $t1, $t2", 0x31415927, 0xee00ee00, t0, t1, t2);
   TESTINST1("nor $t0, $t1, $t2", 0,          255,        t0, t1, t2);
   TESTINST1("nor $t0, $t1, $t2", 1,          0,          t0, t1, t2);
   TESTINST1("nor $t0, $t1, $t2", 0,          1,          t0, t1, t2);
   TESTINST1("nor $t0, $t1, $t2", -1,         0,          t0, t1, t2);
   TESTINST1("nor $t0, $t1, $t2", 0,          -1,         t0, t1, t2);
   TESTINST1("nor $t0, $t1, $t2", 0,          0x80000000, t0, t1, t2);
   TESTINST1("nor $t0, $t1, $t2", 0x80000000, 0,          t0, t1, t2);
   TESTINST1("nor $t0, $t1, $t2", 0x80000000, 0x80000000, t0, t1, t2);
   TESTINST1("nor $t0, $t1, $t2", 0x7fffffff, 0x80000000, t0, t1, t2);
   TESTINST1("nor $t0, $t1, $t2", 0x80000000, 0xff000000, t0, t1, t2);
   TESTINST1("nor $t0, $t1, $t2", 0x7fffffff, 0x0dd00000, t0, t1, t2);
   TESTINST1("nor $t0, $t1, $t2", 0xffffffff, 0,          t0, t1, t2);
   TESTINST1("nor $t0, $t1, $t2", 0,          0xffffffff, t0, t1, t2);
   TESTINST1("nor $t0, $t1, $t2", 0xffffffff, 0xffffffff, t0, t1, t2);
   TESTINST1("nor $t0, $t1, $t2", 0x7fffffff, 0x7fffffff, t0, t1, t2);
   TESTINST1("nor $t0, $t1, $t2", 0x0000ffff, 0x0000ffff, t0, t1, t2);

#if (__mips==32) && (__mips_isa_rev>=2)
   printf("WSBH\n");
   TESTINST3("wsbh  $t0, $t1", 0x2, t0, t1);
   TESTINST3("wsbh  $t0, $t1", 0x28, t0, t1);
   TESTINST3("wsbh  $t0, $t1", -258, t0, t1);
   TESTINST3("wsbh  $t0, $t1", 0x7fffffff, t0, t1);
   TESTINST3("wsbh  $t0, $t1", -11, t0, t1);
   TESTINST3("wsbh  $t0, $t1", 0xffffffff, t0, t1);
   TESTINST3("wsbh  $t0, $t1", 0x16, t0, t1);
   TESTINST3("wsbh  $t0, $t1", -1, t0, t1);
#endif

   printf("NOT\n");
   TESTINST3("not  $t0, $t1", 0x2, t0, t1);
   TESTINST3("not  $t0, $t1", 0x28, t0, t1);
   TESTINST3("not  $t0, $t1", -258, t0, t1);
   TESTINST3("not  $t0, $t1", 0x7fffffff, t0, t1);
   TESTINST3("not  $t0, $t1", -11, t0, t1);
   TESTINST3("not  $t0, $t1", 0xffffffff, t0, t1);
   TESTINST3("not  $t0, $t1", 0x16, t0, t1);
   TESTINST3("not  $t0, $t1", -1, t0, t1);

   printf("NEGU\n");
   TESTINST3("negu  $t0, $t1", 0x2, t0, t1);
   TESTINST3("negu  $t0, $t1", 0x28, t0, t1);
   TESTINST3("negu  $t0, $t1", -258, t0, t1);
   TESTINST3("negu  $t0, $t1", 0x7fffffff, t0, t1);
   TESTINST3("negu  $t0, $t1", -11, t0, t1);
   TESTINST3("negu  $t0, $t1", 0xffffffff, t0, t1);
   TESTINST3("negu  $t0, $t1", 0x16, t0, t1);
   TESTINST3("negu  $t0, $t1", -1, t0, t1);

   printf("OR\n");
   TESTINST1("or $t0, $t1, $t2", 0x31415927, 0xffffffff, t0, t1, t2);
   TESTINST1("or $t0, $t1, $t2", 0x31415927, 0xee00ee00, t0, t1, t2);
   TESTINST1("or $t0, $t1, $t2", 0,          255,        t0, t1, t2);
   TESTINST1("or $t0, $t1, $t2", -1,         0,          t0, t1, t2);
   TESTINST1("or $t0, $t1, $t2", 0,          1,          t0, t1, t2);
   TESTINST1("or $t0, $t1, $t2", 0,          0,          t0, t1, t2);
   TESTINST1("or $t0, $t1, $t2", 0x80000000, -1,         t0, t1, t2);
   TESTINST1("or $t0, $t1, $t2", 0x80000000, 0x80000000, t0, t1, t2);
   TESTINST1("or $t0, $t1, $t2", 0x7fffffff, 0,          t0, t1, t2);
   TESTINST1("or $t0, $t1, $t2", 0x80000000, 0x80000000, t0, t1, t2);
   TESTINST1("or $t0, $t1, $t2", 0x7fffffff, 0x80000000, t0, t1, t2);
   TESTINST1("or $t0, $t1, $t2", 0x80000000, 0xff000000, t0, t1, t2);
   TESTINST1("or $t0, $t1, $t2", 0x7fffffff, 0x0dd00000, t0, t1, t2);
   TESTINST1("or $t0, $t1, $t2", 0x31415927, 0xffffffff, t0, t1, t2);
   TESTINST1("or $t0, $t1, $t2", 0x31415927, 0xee00ee00, t0, t1, t2);
   TESTINST1("or $t0, $t1, $t2", 0,          255,        t0, t1, t2);
   TESTINST1("or $t0, $t1, $t2", 1,          0,          t0, t1, t2);
   TESTINST1("or $t0, $t1, $t2", 0,          1,          t0, t1, t2);
   TESTINST1("or $t0, $t1, $t2", -1,         0,          t0, t1, t2);
   TESTINST1("or $t0, $t1, $t2", 0,          -1,         t0, t1, t2);
   TESTINST1("or $t0, $t1, $t2", 0,          0x80000000, t0, t1, t2);
   TESTINST1("or $t0, $t1, $t2", 0x80000000, 0,          t0, t1, t2);
   TESTINST1("or $t0, $t1, $t2", 0x80000000, 0x80000000, t0, t1, t2);
   TESTINST1("or $t0, $t1, $t2", 0x7fffffff, 0x80000000, t0, t1, t2);
   TESTINST1("or $t0, $t1, $t2", 0x80000000, 0xff000000, t0, t1, t2);
   TESTINST1("or $t0, $t1, $t2", 0x7fffffff, 0x0dd00000, t0, t1, t2);
   TESTINST1("or $t0, $t1, $t2", 0xffffffff, 0,          t0, t1, t2);
   TESTINST1("or $t0, $t1, $t2", 0,          0xffffffff, t0, t1, t2);
   TESTINST1("or $t0, $t1, $t2", 0xffffffff, 0xffffffff, t0, t1, t2);
   TESTINST1("or $t0, $t1, $t2", 0x7fffffff, 0x7fffffff, t0, t1, t2);
   TESTINST1("or $t0, $t1, $t2", 0x0000ffff, 0x0000ffff, t0, t1, t2);

   printf("ORI\n");
   TESTINST2("ori $t0, $t1, 0xffff", 0x31415927, 0xffff, t0, t1);
   TESTINST2("ori $t0, $t1, 0xee00", 0x31415927, 0xee00, t0, t1);
   TESTINST2("ori $t0, $t1, 255", 0,          255,        t0, t1);
   TESTINST2("ori $t0, $t1, 0", -1,         0,          t0, t1);
   TESTINST2("ori $t0, $t1, 1", 0,          1,          t0, t1);
   TESTINST2("ori $t0, $t1, 0", 0,          0,          t0, t1);
   TESTINST2("ori $t0, $t1, 0x8000", 0x80000000, 0x8000, t0, t1);
   TESTINST2("ori $t0, $t1, 0", 0x7fffffff, 0,          t0, t1);
   TESTINST2("ori $t0, $t1, 0x8000", 0x80000000, 0x8000, t0, t1);
   TESTINST2("ori $t0, $t1, 0x8000", 0x7fffffff, 0x8000, t0, t1);
   TESTINST2("ori $t0, $t1, 0xff00", 0x80000000, 0xff00, t0, t1);
   TESTINST2("ori $t0, $t1, 0x0dd0", 0x7fffffff, 0x0dd0, t0, t1);
   TESTINST2("ori $t0, $t1, 0xffff", 0x31415927, 0xffff, t0, t1);
   TESTINST2("ori $t0, $t1, 0xee00", 0x31415927, 0xee00, t0, t1);
   TESTINST2("ori $t0, $t1, 255", 0,          255,        t0, t1);
   TESTINST2("ori $t0, $t1, 0", 1,          0,          t0, t1);
   TESTINST2("ori $t0, $t1, 1", 0,          1,          t0, t1);
   TESTINST2("ori $t0, $t1, 0", -1,         0,          t0, t1);
   TESTINST2("ori $t0, $t1, 0x8000", 0,          0x8000, t0, t1);
   TESTINST2("ori $t0, $t1, 0", 0x8000, 0,          t0, t1);
   TESTINST2("ori $t0, $t1, 0x8000", 0x80000000, 0x8000, t0, t1);
   TESTINST2("ori $t0, $t1, 0x8000", 0x7fffffff, 0x8000, t0, t1);
   TESTINST2("ori $t0, $t1, 0xff00", 0x80000000, 0xff00, t0, t1);
   TESTINST2("ori $t0, $t1, 0x0dd0", 0x7fffffff, 0x0dd0, t0, t1);
   TESTINST2("ori $t0, $t1, 0", 0xffff, 0,          t0, t1);
   TESTINST2("ori $t0, $t1, 0xffff", 0,          0xffff, t0, t1);
   TESTINST2("ori $t0, $t1, 0xffff", 0xffffffff, 0xffff, t0, t1);
   TESTINST2("ori $t0, $t1, 0x7fff", 0x7fffffff, 0x7fff, t0, t1);
   TESTINST2("ori $t0, $t1, 0x0000", 0x0000ffff, 0x0000, t0, t1);

#if (__mips==32) && (__mips_isa_rev>=2)
   printf("ROTR\n");
   TESTINST2("rotr $t0, $t1, 0x00000000", 0x31415927, 0x00000000, t0, t1);
   TESTINST2("rotr $t0, $t1, 0x00000001", 0x31415927, 0x00000001, t0, t1);
   TESTINST2("rotr $t0, $t1, 0x00000002", 0x31415927, 0x00000002, t0, t1);
   TESTINST2("rotr $t0, $t1, 0x0000000F", 0x31415927, 0x0000000F, t0, t1);
   TESTINST2("rotr $t0, $t1, 0x00000010", 0x31415927, 0x00000010, t0, t1);
   TESTINST2("rotr $t0, $t1, 0x0000001F", 0x31415927, 0x0000001F, t0, t1);
   TESTINST2("rotr $t0, $t1, 0x0000001A", 0x31415927, 0x0000001A, t0, t1);
   TESTINST2("rotr $t0, $t1, 0x00000007", 0x31415927, 0x00000007, t0, t1);
   TESTINST2("rotr $t0, $t1, 0x00000000", 0x00088000, 0x00000000, t0, t1);
   TESTINST2("rotr $t0, $t1, 0x00000001", 0x00088000, 0x00000001, t0, t1);
   TESTINST2("rotr $t0, $t1, 31", 0x00088000, 31, t0, t1);
   TESTINST2("rotr $t0, $t1, 16", 0x00010000, 16, t0, t1);
   TESTINST2("rotr $t0, $t1, 17", 0x00010000, 17, t0, t1);
   TESTINST2("rotr $t0, $t1, 18", 0x00010000, 18, t0, t1);
   TESTINST2("rotr $t0, $t1, 0", 0, 0, t0, t1);
   TESTINST2("rotr $t0, $t1, 0x1F", 0xFFFF, 0x1F, t0, t1);
#endif

#if (__mips==32) && (__mips_isa_rev>=2)
   printf("ROTRV\n");
   TESTINST1("rotrv $t0, $t1, $t2", 0x31415927, 0xffffffff, t0, t1, t2);
   TESTINST1("rotrv $t0, $t1, $t2", 0x31415927, 0xee00ee00, t0, t1, t2);
   TESTINST1("rotrv $t0, $t1, $t2", 0,          255,        t0, t1, t2);
   TESTINST1("rotrv $t0, $t1, $t2", -1,         0,          t0, t1, t2);
   TESTINST1("rotrv $t0, $t1, $t2", 0,          1,          t0, t1, t2);
   TESTINST1("rotrv $t0, $t1, $t2", 0,          0,          t0, t1, t2);
   TESTINST1("rotrv $t0, $t1, $t2", 0x80000000, -1,         t0, t1, t2);
   TESTINST1("rotrv $t0, $t1, $t2", 0x80000000, 0x80000000, t0, t1, t2);
   TESTINST1("rotrv $t0, $t1, $t2", 0x7fffffff, 0,          t0, t1, t2);
   TESTINST1("rotrv $t0, $t1, $t2", 0x80000000, 0x80000000, t0, t1, t2);
   TESTINST1("rotrv $t0, $t1, $t2", 0x7fffffff, 0x80000000, t0, t1, t2);
   TESTINST1("rotrv $t0, $t1, $t2", 0x80000000, 0xff000000, t0, t1, t2);
   TESTINST1("rotrv $t0, $t1, $t2", 0x7fffffff, 0x0dd00000, t0, t1, t2);
   TESTINST1("rotrv $t0, $t1, $t2", 0x31415927, 0xffffffff, t0, t1, t2);
   TESTINST1("rotrv $t0, $t1, $t2", 0x31415927, 0xee00ee00, t0, t1, t2);
   TESTINST1("rotrv $t0, $t1, $t2", 0,          255,        t0, t1, t2);
   TESTINST1("rotrv $t0, $t1, $t2", 1,          0,          t0, t1, t2);
   TESTINST1("rotrv $t0, $t1, $t2", 0,          1,          t0, t1, t2);
   TESTINST1("rotrv $t0, $t1, $t2", -1,         0,          t0, t1, t2);
   TESTINST1("rotrv $t0, $t1, $t2", 0,          -1,         t0, t1, t2);
   TESTINST1("rotrv $t0, $t1, $t2", 0,          0x80000000, t0, t1, t2);
   TESTINST1("rotrv $t0, $t1, $t2", 0x80000000, 0,          t0, t1, t2);
   TESTINST1("rotrv $t0, $t1, $t2", 0x80000000, 0x80000000, t0, t1, t2);
   TESTINST1("rotrv $t0, $t1, $t2", 0x7fffffff, 0x80000000, t0, t1, t2);
   TESTINST1("rotrv $t0, $t1, $t2", 0x80000000, 0xff000000, t0, t1, t2);
   TESTINST1("rotrv $t0, $t1, $t2", 0x7fffffff, 0x0dd00000, t0, t1, t2);
   TESTINST1("rotrv $t0, $t1, $t2", 0xffffffff, 0,          t0, t1, t2);
   TESTINST1("rotrv $t0, $t1, $t2", 0,          0xffffffff, t0, t1, t2);
   TESTINST1("rotrv $t0, $t1, $t2", 0xffffffff, 0xffffffff, t0, t1, t2);
   TESTINST1("rotrv $t0, $t1, $t2", 0x7fffffff, 0x7fffffff, t0, t1, t2);
   TESTINST1("rotrv $t0, $t1, $t2", 0x0000ffff, 0x0000ffff, t0, t1, t2);
   TESTINST1("rotrv $t0, $t1, $t2", 0x31415927, 0x00000000, t0, t1, t2);
   TESTINST1("rotrv $t0, $t1, $t2", 0x31415927, 0x00000001, t0, t1, t2);
   TESTINST1("rotrv $t0, $t1, $t2", 0x31415927, 0x00000002, t0, t1, t2);
   TESTINST1("rotrv $t0, $t1, $t2", 0x31415927, 0x0000000F, t0, t1, t2);
   TESTINST1("rotrv $t0, $t1, $t2", 0x31415927, 0x00000010, t0, t1, t2);
   TESTINST1("rotrv $t0, $t1, $t2", 0x31415927, 0x0000001F, t0, t1, t2);
   TESTINST1("rotrv $t0, $t1, $t2", 0x31415927, 0x00000020, t0, t1, t2);
   TESTINST1("rotrv $t0, $t1, $t2", 0x31415927, 0x00000021, t0, t1, t2);
   TESTINST1("rotrv $t0, $t1, $t2", 0x00088000, 0x00000000, t0, t1, t2);
   TESTINST1("rotrv $t0, $t1, $t2", 0x00088000, 0x00000001, t0, t1, t2);
   TESTINST1("rotrv $t0, $t1, $t2", 0x00088000, 31, t0, t1, t2);
   TESTINST1("rotrv $t0, $t1, $t2", 0x00010000, 16, t0, t1, t2);
   TESTINST1("rotrv $t0, $t1, $t2", 0x00010000, 17, t0, t1, t2);
   TESTINST1("rotrv $t0, $t1, $t2", 0x00010000, 18, t0, t1, t2);
   TESTINST1("rotrv $t0, $t1, $t2", 0, 0, t0, t1, t2);
   TESTINST1("rotrv $t0, $t1, $t2", 0xffff, 0xffff, t0, t1, t2);

   printf("SEB\n");
   TESTINST3("seb  $t0, $t1", 0x2, t0, t1);
   TESTINST3("seb  $t0, $t1", 0x28, t0, t1);
   TESTINST3("seb  $t0, $t1", -258, t0, t1);
   TESTINST3("seb  $t0, $t1", 0x7fffffff, t0, t1);
   TESTINST3("seb  $t0, $t1", -11, t0, t1);
   TESTINST3("seb  $t0, $t1", 0xffffffff, t0, t1);
   TESTINST3("seb  $t0, $t1", 0x16, t0, t1);
   TESTINST3("seb  $t0, $t1", -1, t0, t1);

   printf("SEH\n");
   TESTINST3("seh  $t0, $t1", 0x2, t0, t1);
   TESTINST3("seh  $t0, $t1", 0x28, t0, t1);
   TESTINST3("seh  $t0, $t1", -258, t0, t1);
   TESTINST3("seh  $t0, $t1", 0x7fffffff, t0, t1);
   TESTINST3("seh  $t0, $t1", -11, t0, t1);
   TESTINST3("seh  $t0, $t1", 0xffffffff, t0, t1);
   TESTINST3("seh  $t0, $t1", 0x16, t0, t1);
   TESTINST3("seh  $t0, $t1", -1, t0, t1);
#endif

   printf("SLL\n");
   TESTINST2("sll $t0, $t1, 0x00000000", 0x31415927, 0x00000000, t0, t1);
   TESTINST2("sll $t0, $t1, 0x00000001", 0x31415927, 0x00000001, t0, t1);
   TESTINST2("sll $t0, $t1, 0x00000002", 0x31415927, 0x00000002, t0, t1);
   TESTINST2("sll $t0, $t1, 0x0000000F", 0x31415927, 0x0000000F, t0, t1);
   TESTINST2("sll $t0, $t1, 0x00000010", 0x31415927, 0x00000010, t0, t1);
   TESTINST2("sll $t0, $t1, 0x0000001F", 0x31415927, 0x0000001F, t0, t1);
   TESTINST2("sll $t0, $t1, 0x00000009", 0x31415927, 0x00000009, t0, t1);
   TESTINST2("sll $t0, $t1, 0x0000000A", 0x31415927, 0x0000000A, t0, t1);
   TESTINST2("sll $t0, $t1, 0x00000000", 0x00088000, 0x00000000, t0, t1);
   TESTINST2("sll $t0, $t1, 0x00000001", 0x00088000, 0x00000001, t0, t1);
   TESTINST2("sll $t0, $t1, 31", 0x00088000, 31, t0, t1);
   TESTINST2("sll $t0, $t1, 16", 0x00010000, 16, t0, t1);
   TESTINST2("sll $t0, $t1, 17", 0x00010000, 17, t0, t1);
   TESTINST2("sll $t0, $t1, 18", 0x00010000, 18, t0, t1);
   TESTINST2("sll $t0, $t1, 0", 0, 0, t0, t1);

   printf("SLLV\n");
   TESTINST1("sllv $t0, $t1, $t2", 0x31415927, 0xffffffff, t0, t1, t2);
   TESTINST1("sllv $t0, $t1, $t2", 0x31415927, 0xee00ee00, t0, t1, t2);
   TESTINST1("sllv $t0, $t1, $t2", 0,          255,        t0, t1, t2);
   TESTINST1("sllv $t0, $t1, $t2", -1,         0,          t0, t1, t2);
   TESTINST1("sllv $t0, $t1, $t2", 0,          1,          t0, t1, t2);
   TESTINST1("sllv $t0, $t1, $t2", 0,          0,          t0, t1, t2);
   TESTINST1("sllv $t0, $t1, $t2", 0x80000000, -1,         t0, t1, t2);
   TESTINST1("sllv $t0, $t1, $t2", 0x80000000, 0x80000000, t0, t1, t2);
   TESTINST1("sllv $t0, $t1, $t2", 0x7fffffff, 0,          t0, t1, t2);
   TESTINST1("sllv $t0, $t1, $t2", 0x80000000, 0x80000000, t0, t1, t2);
   TESTINST1("sllv $t0, $t1, $t2", 0x7fffffff, 0x80000000, t0, t1, t2);
   TESTINST1("sllv $t0, $t1, $t2", 0x80000000, 0xff000000, t0, t1, t2);
   TESTINST1("sllv $t0, $t1, $t2", 0x7fffffff, 0x0dd00000, t0, t1, t2);
   TESTINST1("sllv $t0, $t1, $t2", 0x31415927, 0xffffffff, t0, t1, t2);
   TESTINST1("sllv $t0, $t1, $t2", 0x31415927, 0xee00ee00, t0, t1, t2);
   TESTINST1("sllv $t0, $t1, $t2", 0,          255,        t0, t1, t2);
   TESTINST1("sllv $t0, $t1, $t2", 1,          0,          t0, t1, t2);
   TESTINST1("sllv $t0, $t1, $t2", 0,          1,          t0, t1, t2);
   TESTINST1("sllv $t0, $t1, $t2", -1,         0,          t0, t1, t2);
   TESTINST1("sllv $t0, $t1, $t2", 0,          -1,         t0, t1, t2);
   TESTINST1("sllv $t0, $t1, $t2", 0,          0x80000000, t0, t1, t2);
   TESTINST1("sllv $t0, $t1, $t2", 0x80000000, 0,          t0, t1, t2);
   TESTINST1("sllv $t0, $t1, $t2", 0x80000000, 0x80000000, t0, t1, t2);
   TESTINST1("sllv $t0, $t1, $t2", 0x7fffffff, 0x80000000, t0, t1, t2);
   TESTINST1("sllv $t0, $t1, $t2", 0x80000000, 0xff000000, t0, t1, t2);
   TESTINST1("sllv $t0, $t1, $t2", 0x7fffffff, 0x0dd00000, t0, t1, t2);
   TESTINST1("sllv $t0, $t1, $t2", 0xffffffff, 0,          t0, t1, t2);
   TESTINST1("sllv $t0, $t1, $t2", 0,          0xffffffff, t0, t1, t2);
   TESTINST1("sllv $t0, $t1, $t2", 0xffffffff, 0xffffffff, t0, t1, t2);
   TESTINST1("sllv $t0, $t1, $t2", 0x7fffffff, 0x7fffffff, t0, t1, t2);
   TESTINST1("sllv $t0, $t1, $t2", 0x0000ffff, 0x0000ffff, t0, t1, t2);
   TESTINST1("sllv $t0, $t1, $t2", 0x31415927, 0x00000000, t0, t1, t2);
   TESTINST1("sllv $t0, $t1, $t2", 0x31415927, 0x00000001, t0, t1, t2);
   TESTINST1("sllv $t0, $t1, $t2", 0x31415927, 0x00000002, t0, t1, t2);
   TESTINST1("sllv $t0, $t1, $t2", 0x31415927, 0x0000000F, t0, t1, t2);
   TESTINST1("sllv $t0, $t1, $t2", 0x31415927, 0x00000010, t0, t1, t2);
   TESTINST1("sllv $t0, $t1, $t2", 0x31415927, 0x0000001F, t0, t1, t2);
   TESTINST1("sllv $t0, $t1, $t2", 0x31415927, 0x00000020, t0, t1, t2);
   TESTINST1("sllv $t0, $t1, $t2", 0x31415927, 0x00000021, t0, t1, t2);
   TESTINST1("sllv $t0, $t1, $t2", 0x00088000, 0x00000000, t0, t1, t2);
   TESTINST1("sllv $t0, $t1, $t2", 0x00088000, 0x00000001, t0, t1, t2);
   TESTINST1("sllv $t0, $t1, $t2", 0x00088000, 31, t0, t1, t2);
   TESTINST1("sllv $t0, $t1, $t2", 0x00010000, 16, t0, t1, t2);
   TESTINST1("sllv $t0, $t1, $t2", 0x00010000, 17, t0, t1, t2);
   TESTINST1("sllv $t0, $t1, $t2", 0x00010000, 18, t0, t1, t2);
   TESTINST1("sllv $t0, $t1, $t2", 0, 0, t0, t1, t2);
   TESTINST1("sllv $t0, $t1, $t2", 0xffff, 0xffff, t0, t1, t2);

   printf("SLT\n");
   TESTINST1("slt $t0, $t1, $t2", 0x31415927, 0xffffffff, t0, t1, t2);
   TESTINST1("slt $t0, $t1, $t2", 0x31415927, 0xee00ee00, t0, t1, t2);
   TESTINST1("slt $t0, $t1, $t2", 0,          255,        t0, t1, t2);
   TESTINST1("slt $t0, $t1, $t2", -1,         0,          t0, t1, t2);
   TESTINST1("slt $t0, $t1, $t2", 0,          1,          t0, t1, t2);
   TESTINST1("slt $t0, $t1, $t2", 0,          0,          t0, t1, t2);
   TESTINST1("slt $t0, $t1, $t2", 0x80000000, -1,         t0, t1, t2);
   TESTINST1("slt $t0, $t1, $t2", 0x80000000, 0x80000000, t0, t1, t2);
   TESTINST1("slt $t0, $t1, $t2", 0x7fffffff, 0,          t0, t1, t2);
   TESTINST1("slt $t0, $t1, $t2", 0x80000000, 0x80000000, t0, t1, t2);
   TESTINST1("slt $t0, $t1, $t2", 0x7fffffff, 0x80000000, t0, t1, t2);
   TESTINST1("slt $t0, $t1, $t2", 0x80000000, 0xff000000, t0, t1, t2);
   TESTINST1("slt $t0, $t1, $t2", 0x7fffffff, 0x0dd00000, t0, t1, t2);
   TESTINST1("slt $t0, $t1, $t2", 0x31415927, 0xffffffff, t0, t1, t2);
   TESTINST1("slt $t0, $t1, $t2", 0x31415927, 0xee00ee00, t0, t1, t2);
   TESTINST1("slt $t0, $t1, $t2", 0,          255,        t0, t1, t2);
   TESTINST1("slt $t0, $t1, $t2", 1,          0,          t0, t1, t2);
   TESTINST1("slt $t0, $t1, $t2", 0,          1,          t0, t1, t2);
   TESTINST1("slt $t0, $t1, $t2", -1,         0,          t0, t1, t2);
   TESTINST1("slt $t0, $t1, $t2", 0,          -1,         t0, t1, t2);
   TESTINST1("slt $t0, $t1, $t2", 0,          0x80000000, t0, t1, t2);
   TESTINST1("slt $t0, $t1, $t2", 0x80000000, 0,          t0, t1, t2);
   TESTINST1("slt $t0, $t1, $t2", 0x80000000, 0x80000000, t0, t1, t2);
   TESTINST1("slt $t0, $t1, $t2", 0x7fffffff, 0x80000000, t0, t1, t2);
   TESTINST1("slt $t0, $t1, $t2", 0x80000000, 0xff000000, t0, t1, t2);
   TESTINST1("slt $t0, $t1, $t2", 0x7fffffff, 0x0dd00000, t0, t1, t2);
   TESTINST1("slt $t0, $t1, $t2", 0xffffffff, 0,          t0, t1, t2);
   TESTINST1("slt $t0, $t1, $t2", 0,          0xffffffff, t0, t1, t2);
   TESTINST1("slt $t0, $t1, $t2", 0xffffffff, 0xffffffff, t0, t1, t2);
   TESTINST1("slt $t0, $t1, $t2", 0x7fffffff, 0x7fffffff, t0, t1, t2);
   TESTINST1("slt $t0, $t1, $t2", 0x0000ffff, 0x0000ffff, t0, t1, t2);
   TESTINST1("slt $t0, $t1, $t2", 0xffffffff, 0,          t0, t1, t2);
   TESTINST1("slt $t0, $t1, $t2", 0,          0xffffffff, t0, t1, t2);
   TESTINST1("slt $t0, $t1, $t2", 0xffffffff, 0xffffffff, t0, t1, t2);
   TESTINST1("slt $t0, $t1, $t2", 0x7fffffff, 0x7fffffff, t0, t1, t2);
   TESTINST1("slt $t0, $t1, $t2", 0x0000ffff, 0x0000ffff, t0, t1, t2);
   TESTINST1("slt $t0, $t1, $t2", 0x31415927, 0x00000000, t0, t1, t2);
   TESTINST1("slt $t0, $t1, $t2", 0x31415927, 0x00000001, t0, t1, t2);
   TESTINST1("slt $t0, $t1, $t2", 0x31415927, 0x00000002, t0, t1, t2);
   TESTINST1("slt $t0, $t1, $t2", 0x31415927, 0x0000000F, t0, t1, t2);
   TESTINST1("slt $t0, $t1, $t2", 0x31415927, 0x00000010, t0, t1, t2);
   TESTINST1("slt $t0, $t1, $t2", 0x31415927, 0x0000001F, t0, t1, t2);
   TESTINST1("slt $t0, $t1, $t2", 0x31415927, 0x00000020, t0, t1, t2);
   TESTINST1("slt $t0, $t1, $t2", 0x31415927, 0x00000021, t0, t1, t2);
   TESTINST1("slt $t0, $t1, $t2", 0x00088000, 0x00000000, t0, t1, t2);
   TESTINST1("slt $t0, $t1, $t2", 0x00088000, 0x00000001, t0, t1, t2);
   TESTINST1("slt $t0, $t1, $t2", 0x00088000, 31, t0, t1, t2);
   TESTINST1("slt $t0, $t1, $t2", 0x00010000, 16, t0, t1, t2);
   TESTINST1("slt $t0, $t1, $t2", 0x00010000, 17, t0, t1, t2);
   TESTINST1("slt $t0, $t1, $t2", 0x00010000, 18, t0, t1, t2);
   TESTINST1("slt $t0, $t1, $t2", 0, 0, t0, t1, t2);
   TESTINST1("slt $t0, $t1, $t2", 0xffff, 0xffff, t0, t1, t2);

   printf("SLTI\n");
   TESTINST2("slti $t0, $t1, 0x00000000", 0x00000001, 0x31415927, t0, t1);
   TESTINST2("slti $t0, $t1, 0x00000001", 0x31415927, 0x00000001, t0, t1);
   TESTINST2("slti $t0, $t1, 0x00000002", 0x31415927, 0x00000002, t0, t1);
   TESTINST2("slti $t0, $t1, 0x0000000F", 0x31415927, 0x0000000F, t0, t1);
   TESTINST2("slti $t0, $t1, 0x00000010", 0x00000010, 0x00000010, t0, t1);
   TESTINST2("slti $t0, $t1, 0x0000001F", 0x00000010, 0x31415927, t0, t1);
   TESTINST2("slti $t0, $t1, 0x00000009", 0x31415927, 0x00000009, t0, t1);
   TESTINST2("slti $t0, $t1, 0x0000000A", 0x31415927, 0x0000000A, t0, t1);
   TESTINST2("slti $t0, $t1, 0x00000000", 0x00088000, 0x0000000A, t0, t1);
   TESTINST2("slti $t0, $t1, 0x00000001", 0x00000000, 0x00000001, t0, t1);
   TESTINST2("slti $t0, $t1, 31", 0x00088000, 31, t0, t1);
   TESTINST2("slti $t0, $t1, 16", 0x00010000, 16, t0, t1);
   TESTINST2("slti $t0, $t1, 17", 0x00010000, 17, t0, t1);
   TESTINST2("slti $t0, $t1, 18", 0x00010000, 18, t0, t1);
   TESTINST2("slti $t0, $t1, 0", 0, 0, t0, t1);

   printf("SLTIU\n");
   TESTINST2("sltiu $t0, $t1, 0x00000000", 0x00000001, 0x31415927, t0, t1);
   TESTINST2("sltiu $t0, $t1, 0x00000001", 0x31415927, 0x00000001, t0, t1);
   TESTINST2("sltiu $t0, $t1, 0x00000002", 0x31415927, 0x00000002, t0, t1);
   TESTINST2("sltiu $t0, $t1, 0x0000000F", 0x31415927, 0x0000000F, t0, t1);
   TESTINST2("sltiu $t0, $t1, 0x00000010", 0x00000010, 0x00000010, t0, t1);
   TESTINST2("sltiu $t0, $t1, 0x0000001F", 0x00000010, 0x31415927, t0, t1);
   TESTINST2("sltiu $t0, $t1, 0x00000009", 0x31415927, 0x00000009, t0, t1);
   TESTINST2("sltiu $t0, $t1, 0x0000000A", 0x31415927, 0x0000000A, t0, t1);
   TESTINST2("sltiu $t0, $t1, 0x00000000", 0x00088000, 0x0000000A, t0, t1);
   TESTINST2("sltiu $t0, $t1, 0x00000001", 0x00000000, 0x00000001, t0, t1);
   TESTINST2("sltiu $t0, $t1, 31", 0x00088000, 31, t0, t1);
   TESTINST2("sltiu $t0, $t1, 16", 0x00010000, 16, t0, t1);
   TESTINST2("sltiu $t0, $t1, 17", 0x00010000, 17, t0, t1);
   TESTINST2("sltiu $t0, $t1, 18", 0x00010000, 18, t0, t1);
   TESTINST2("sltiu $t0, $t1, 0", 0, 0, t0, t1);

   printf("SLTU\n");
   TESTINST1("sltu $t0, $t1, $t2", 0x31415927, 0xffffffff, t0, t1, t2);
   TESTINST1("sltu $t0, $t1, $t2", 0x31415927, 0xee00ee00, t0, t1, t2);
   TESTINST1("sltu $t0, $t1, $t2", 0,          255,        t0, t1, t2);
   TESTINST1("sltu $t0, $t1, $t2", -1,         0,          t0, t1, t2);
   TESTINST1("sltu $t0, $t1, $t2", 0,          1,          t0, t1, t2);
   TESTINST1("sltu $t0, $t1, $t2", 0,          0,          t0, t1, t2);
   TESTINST1("sltu $t0, $t1, $t2", 0x80000000, -1,         t0, t1, t2);
   TESTINST1("sltu $t0, $t1, $t2", 0x80000000, 0x80000000, t0, t1, t2);
   TESTINST1("sltu $t0, $t1, $t2", 0x7fffffff, 0,          t0, t1, t2);
   TESTINST1("sltu $t0, $t1, $t2", 0x80000000, 0x80000000, t0, t1, t2);
   TESTINST1("sltu $t0, $t1, $t2", 0x7fffffff, 0x80000000, t0, t1, t2);
   TESTINST1("sltu $t0, $t1, $t2", 0x80000000, 0xff000000, t0, t1, t2);
   TESTINST1("sltu $t0, $t1, $t2", 0x7fffffff, 0x0dd00000, t0, t1, t2);
   TESTINST1("sltu $t0, $t1, $t2", 0x31415927, 0xffffffff, t0, t1, t2);
   TESTINST1("sltu $t0, $t1, $t2", 0x31415927, 0xee00ee00, t0, t1, t2);
   TESTINST1("sltu $t0, $t1, $t2", 0,          255,        t0, t1, t2);
   TESTINST1("sltu $t0, $t1, $t2", 1,          0,          t0, t1, t2);
   TESTINST1("sltu $t0, $t1, $t2", 0,          1,          t0, t1, t2);
   TESTINST1("sltu $t0, $t1, $t2", -1,         0,          t0, t1, t2);
   TESTINST1("sltu $t0, $t1, $t2", 0,          -1,         t0, t1, t2);
   TESTINST1("sltu $t0, $t1, $t2", 0,          0x80000000, t0, t1, t2);
   TESTINST1("sltu $t0, $t1, $t2", 0x80000000, 0,          t0, t1, t2);
   TESTINST1("sltu $t0, $t1, $t2", 0x80000000, 0x80000000, t0, t1, t2);
   TESTINST1("sltu $t0, $t1, $t2", 0x7fffffff, 0x80000000, t0, t1, t2);
   TESTINST1("sltu $t0, $t1, $t2", 0x80000000, 0xff000000, t0, t1, t2);
   TESTINST1("sltu $t0, $t1, $t2", 0x7fffffff, 0x0dd00000, t0, t1, t2);
   TESTINST1("sltu $t0, $t1, $t2", 0xffffffff, 0,          t0, t1, t2);
   TESTINST1("sltu $t0, $t1, $t2", 0,          0xffffffff, t0, t1, t2);
   TESTINST1("sltu $t0, $t1, $t2", 0xffffffff, 0xffffffff, t0, t1, t2);
   TESTINST1("sltu $t0, $t1, $t2", 0x7fffffff, 0x7fffffff, t0, t1, t2);
   TESTINST1("sltu $t0, $t1, $t2", 0x0000ffff, 0x0000ffff, t0, t1, t2);
   TESTINST1("sltu $t0, $t1, $t2", 0xffffffff, 0,          t0, t1, t2);
   TESTINST1("sltu $t0, $t1, $t2", 0,          0xffffffff, t0, t1, t2);
   TESTINST1("sltu $t0, $t1, $t2", 0xffffffff, 0xffffffff, t0, t1, t2);
   TESTINST1("sltu $t0, $t1, $t2", 0x7fffffff, 0x7fffffff, t0, t1, t2);
   TESTINST1("sltu $t0, $t1, $t2", 0x0000ffff, 0x0000ffff, t0, t1, t2);
   TESTINST1("sltu $t0, $t1, $t2", 0x31415927, 0x00000000, t0, t1, t2);
   TESTINST1("sltu $t0, $t1, $t2", 0x31415927, 0x00000001, t0, t1, t2);
   TESTINST1("sltu $t0, $t1, $t2", 0x31415927, 0x00000002, t0, t1, t2);
   TESTINST1("sltu $t0, $t1, $t2", 0x31415927, 0x0000000F, t0, t1, t2);
   TESTINST1("sltu $t0, $t1, $t2", 0x31415927, 0x00000010, t0, t1, t2);
   TESTINST1("sltu $t0, $t1, $t2", 0x31415927, 0x0000001F, t0, t1, t2);
   TESTINST1("sltu $t0, $t1, $t2", 0x31415927, 0x00000020, t0, t1, t2);
   TESTINST1("sltu $t0, $t1, $t2", 0x31415927, 0x00000021, t0, t1, t2);
   TESTINST1("sltu $t0, $t1, $t2", 0x00088000, 0x00000000, t0, t1, t2);
   TESTINST1("sltu $t0, $t1, $t2", 0x00088000, 0x00000001, t0, t1, t2);
   TESTINST1("sltu $t0, $t1, $t2", 0x00088000, 31, t0, t1, t2);
   TESTINST1("sltu $t0, $t1, $t2", 0x00010000, 16, t0, t1, t2);
   TESTINST1("sltu $t0, $t1, $t2", 0x00010000, 17, t0, t1, t2);
   TESTINST1("sltu $t0, $t1, $t2", 0x00010000, 18, t0, t1, t2);
   TESTINST1("sltu $t0, $t1, $t2", 0, 0, t0, t1, t2);
   TESTINST1("sltu $t0, $t1, $t2", 0xffff, 0xffff, t0, t1, t2);

   printf("SRA\n");
   TESTINST2("sra $t0, $t1, 0x00000000", 0x00000001, 0x31415927, t0, t1);
   TESTINST2("sra $t0, $t1, 0x00000001", 0x31415927, 0x00000001, t0, t1);
   TESTINST2("sra $t0, $t1, 0x00000002", 0x31415927, 0x00000002, t0, t1);
   TESTINST2("sra $t0, $t1, 0x0000000F", 0x31415927, 0x0000000F, t0, t1);
   TESTINST2("sra $t0, $t1, 0x00000010", 0x00000010, 0x00000010, t0, t1);
   TESTINST2("sra $t0, $t1, 0x0000001F", 0x00000010, 0x31415927, t0, t1);
   TESTINST2("sra $t0, $t1, 0x00000009", 0x31415927, 0x00000009, t0, t1);
   TESTINST2("sra $t0, $t1, 0x0000000A", 0x31415927, 0x0000000A, t0, t1);
   TESTINST2("sra $t0, $t1, 0x00000000", 0x00088000, 0x0000000A, t0, t1);
   TESTINST2("sra $t0, $t1, 0x00000001", 0x00000000, 0x00000001, t0, t1);
   TESTINST2("sra $t0, $t1, 31", 0x00088000, 31, t0, t1);
   TESTINST2("sra $t0, $t1, 16", 0x00010000, 16, t0, t1);
   TESTINST2("sra $t0, $t1, 17", 0x00010000, 17, t0, t1);
   TESTINST2("sra $t0, $t1, 18", 0x00010000, 18, t0, t1);
   TESTINST2("sra $t0, $t1, 0", 0, 0, t0, t1);

   printf("SRAV\n");
   TESTINST1("srav $t0, $t1, $t2", 0x31415927, 0xffffffff, t0, t1, t2);
   TESTINST1("srav $t0, $t1, $t2", 0x31415927, 0xee00ee00, t0, t1, t2);
   TESTINST1("srav $t0, $t1, $t2", 0,          255,        t0, t1, t2);
   TESTINST1("srav $t0, $t1, $t2", -1,         0,          t0, t1, t2);
   TESTINST1("srav $t0, $t1, $t2", 0,          1,          t0, t1, t2);
   TESTINST1("srav $t0, $t1, $t2", 0,          0,          t0, t1, t2);
   TESTINST1("srav $t0, $t1, $t2", 0x80000000, -1,         t0, t1, t2);
   TESTINST1("srav $t0, $t1, $t2", 0x80000000, 0x80000000, t0, t1, t2);
   TESTINST1("srav $t0, $t1, $t2", 0x7fffffff, 0,          t0, t1, t2);
   TESTINST1("srav $t0, $t1, $t2", 0x80000000, 0x80000000, t0, t1, t2);
   TESTINST1("srav $t0, $t1, $t2", 0x7fffffff, 0x80000000, t0, t1, t2);
   TESTINST1("srav $t0, $t1, $t2", 0x80000000, 0xff000000, t0, t1, t2);
   TESTINST1("srav $t0, $t1, $t2", 0x7fffffff, 0x0dd00000, t0, t1, t2);
   TESTINST1("srav $t0, $t1, $t2", 0x31415927, 0xffffffff, t0, t1, t2);
   TESTINST1("srav $t0, $t1, $t2", 0x31415927, 0xee00ee00, t0, t1, t2);
   TESTINST1("srav $t0, $t1, $t2", 0,          255,        t0, t1, t2);
   TESTINST1("srav $t0, $t1, $t2", 1,          0,          t0, t1, t2);
   TESTINST1("srav $t0, $t1, $t2", 0,          1,          t0, t1, t2);
   TESTINST1("srav $t0, $t1, $t2", -1,         0,          t0, t1, t2);
   TESTINST1("srav $t0, $t1, $t2", 0,          -1,         t0, t1, t2);
   TESTINST1("srav $t0, $t1, $t2", 0,          0x80000000, t0, t1, t2);
   TESTINST1("srav $t0, $t1, $t2", 0x80000000, 0,          t0, t1, t2);
   TESTINST1("srav $t0, $t1, $t2", 0x80000000, 0x80000000, t0, t1, t2);
   TESTINST1("srav $t0, $t1, $t2", 0x7fffffff, 0x80000000, t0, t1, t2);
   TESTINST1("srav $t0, $t1, $t2", 0x80000000, 0xff000000, t0, t1, t2);
   TESTINST1("srav $t0, $t1, $t2", 0x7fffffff, 0x0dd00000, t0, t1, t2);
   TESTINST1("srav $t0, $t1, $t2", 0xffffffff, 0,          t0, t1, t2);
   TESTINST1("srav $t0, $t1, $t2", 0,          0xffffffff, t0, t1, t2);
   TESTINST1("srav $t0, $t1, $t2", 0xffffffff, 0xffffffff, t0, t1, t2);
   TESTINST1("srav $t0, $t1, $t2", 0x7fffffff, 0x7fffffff, t0, t1, t2);
   TESTINST1("srav $t0, $t1, $t2", 0x0000ffff, 0x0000ffff, t0, t1, t2);
   TESTINST1("srav $t0, $t1, $t2", 0xffffffff, 0,          t0, t1, t2);
   TESTINST1("srav $t0, $t1, $t2", 0,          0xffffffff, t0, t1, t2);
   TESTINST1("srav $t0, $t1, $t2", 0xffffffff, 0xffffffff, t0, t1, t2);
   TESTINST1("srav $t0, $t1, $t2", 0x7fffffff, 0x7fffffff, t0, t1, t2);
   TESTINST1("srav $t0, $t1, $t2", 0x0000ffff, 0x0000ffff, t0, t1, t2);
   TESTINST1("srav $t0, $t1, $t2", 0x31415927, 0x00000000, t0, t1, t2);
   TESTINST1("srav $t0, $t1, $t2", 0x31415927, 0x00000001, t0, t1, t2);
   TESTINST1("srav $t0, $t1, $t2", 0x31415927, 0x00000002, t0, t1, t2);
   TESTINST1("srav $t0, $t1, $t2", 0x31415927, 0x0000000F, t0, t1, t2);
   TESTINST1("srav $t0, $t1, $t2", 0x31415927, 0x00000010, t0, t1, t2);
   TESTINST1("srav $t0, $t1, $t2", 0x31415927, 0x0000001F, t0, t1, t2);
   TESTINST1("srav $t0, $t1, $t2", 0x31415927, 0x00000020, t0, t1, t2);
   TESTINST1("srav $t0, $t1, $t2", 0x31415927, 0x00000021, t0, t1, t2);
   TESTINST1("srav $t0, $t1, $t2", 0x00088000, 0x00000000, t0, t1, t2);
   TESTINST1("srav $t0, $t1, $t2", 0x00088000, 0x00000001, t0, t1, t2);
   TESTINST1("srav $t0, $t1, $t2", 0x00088000, 31, t0, t1, t2);
   TESTINST1("srav $t0, $t1, $t2", 0x00010000, 16, t0, t1, t2);
   TESTINST1("srav $t0, $t1, $t2", 0x00010000, 17, t0, t1, t2);
   TESTINST1("srav $t0, $t1, $t2", 0x00010000, 18, t0, t1, t2);
   TESTINST1("srav $t0, $t1, $t2", 0, 0, t0, t1, t2);
   TESTINST1("srav $t0, $t1, $t2", 0xffff, 0xffff, t0, t1, t2);

   printf("SRL\n");
   TESTINST2("srl $t0, $t1, 0x00000000", 0x00000001, 0x31415927, t0, t1);
   TESTINST2("srl $t0, $t1, 0x00000001", 0x31415927, 0x00000001, t0, t1);
   TESTINST2("srl $t0, $t1, 0x00000002", 0x31415927, 0x00000002, t0, t1);
   TESTINST2("srl $t0, $t1, 0x0000000F", 0x31415927, 0x0000000F, t0, t1);
   TESTINST2("srl $t0, $t1, 0x00000010", 0x00000010, 0x00000010, t0, t1);
   TESTINST2("srl $t0, $t1, 0x0000001F", 0x00000010, 0x31415927, t0, t1);
   TESTINST2("srl $t0, $t1, 0x00000009", 0x31415927, 0x00000009, t0, t1);
   TESTINST2("srl $t0, $t1, 0x0000000A", 0x31415927, 0x0000000A, t0, t1);
   TESTINST2("srl $t0, $t1, 0x00000000", 0x00088000, 0x0000000A, t0, t1);
   TESTINST2("srl $t0, $t1, 0x00000001", 0x00000000, 0x00000001, t0, t1);
   TESTINST2("srl $t0, $t1, 31", 0x00088000, 31, t0, t1);
   TESTINST2("srl $t0, $t1, 16", 0x00010000, 16, t0, t1);
   TESTINST2("srl $t0, $t1, 17", 0x00010000, 17, t0, t1);
   TESTINST2("srl $t0, $t1, 18", 0x00010000, 18, t0, t1);
   TESTINST2("srl $t0, $t1, 0", 0, 0, t0, t1);

   printf("SRLV\n");
   TESTINST1("srlv $t0, $t1, $t2", 0x31415927, 0xffffffff, t0, t1, t2);
   TESTINST1("srlv $t0, $t1, $t2", 0x31415927, 0xee00ee00, t0, t1, t2);
   TESTINST1("srlv $t0, $t1, $t2", 0,          255,        t0, t1, t2);
   TESTINST1("srlv $t0, $t1, $t2", -1,         0,          t0, t1, t2);
   TESTINST1("srlv $t0, $t1, $t2", 0,          1,          t0, t1, t2);
   TESTINST1("srlv $t0, $t1, $t2", 0,          0,          t0, t1, t2);
   TESTINST1("srlv $t0, $t1, $t2", 0x80000000, -1,         t0, t1, t2);
   TESTINST1("srlv $t0, $t1, $t2", 0x80000000, 0x80000000, t0, t1, t2);
   TESTINST1("srlv $t0, $t1, $t2", 0x7fffffff, 0,          t0, t1, t2);
   TESTINST1("srlv $t0, $t1, $t2", 0x80000000, 0x80000000, t0, t1, t2);
   TESTINST1("srlv $t0, $t1, $t2", 0x7fffffff, 0x80000000, t0, t1, t2);
   TESTINST1("srlv $t0, $t1, $t2", 0x80000000, 0xff000000, t0, t1, t2);
   TESTINST1("srlv $t0, $t1, $t2", 0x7fffffff, 0x0dd00000, t0, t1, t2);
   TESTINST1("srlv $t0, $t1, $t2", 0x31415927, 0xffffffff, t0, t1, t2);
   TESTINST1("srlv $t0, $t1, $t2", 0x31415927, 0xee00ee00, t0, t1, t2);
   TESTINST1("srlv $t0, $t1, $t2", 0,          255,        t0, t1, t2);
   TESTINST1("srlv $t0, $t1, $t2", 1,          0,          t0, t1, t2);
   TESTINST1("srlv $t0, $t1, $t2", 0,          1,          t0, t1, t2);
   TESTINST1("srlv $t0, $t1, $t2", -1,         0,          t0, t1, t2);
   TESTINST1("srlv $t0, $t1, $t2", 0,          -1,         t0, t1, t2);
   TESTINST1("srlv $t0, $t1, $t2", 0,          0x80000000, t0, t1, t2);
   TESTINST1("srlv $t0, $t1, $t2", 0x80000000, 0,          t0, t1, t2);
   TESTINST1("srlv $t0, $t1, $t2", 0x80000000, 0x80000000, t0, t1, t2);
   TESTINST1("srlv $t0, $t1, $t2", 0x7fffffff, 0x80000000, t0, t1, t2);
   TESTINST1("srlv $t0, $t1, $t2", 0x80000000, 0xff000000, t0, t1, t2);
   TESTINST1("srlv $t0, $t1, $t2", 0x7fffffff, 0x0dd00000, t0, t1, t2);
   TESTINST1("srlv $t0, $t1, $t2", 0xffffffff, 0,          t0, t1, t2);
   TESTINST1("srlv $t0, $t1, $t2", 0,          0xffffffff, t0, t1, t2);
   TESTINST1("srlv $t0, $t1, $t2", 0xffffffff, 0xffffffff, t0, t1, t2);
   TESTINST1("srlv $t0, $t1, $t2", 0x7fffffff, 0x7fffffff, t0, t1, t2);
   TESTINST1("srlv $t0, $t1, $t2", 0x0000ffff, 0x0000ffff, t0, t1, t2);
   TESTINST1("srlv $t0, $t1, $t2", 0xffffffff, 0,          t0, t1, t2);
   TESTINST1("srlv $t0, $t1, $t2", 0,          0xffffffff, t0, t1, t2);
   TESTINST1("srlv $t0, $t1, $t2", 0xffffffff, 0xffffffff, t0, t1, t2);
   TESTINST1("srlv $t0, $t1, $t2", 0x7fffffff, 0x7fffffff, t0, t1, t2);
   TESTINST1("srlv $t0, $t1, $t2", 0x0000ffff, 0x0000ffff, t0, t1, t2);
   TESTINST1("srlv $t0, $t1, $t2", 0x31415927, 0x00000000, t0, t1, t2);
   TESTINST1("srlv $t0, $t1, $t2", 0x31415927, 0x00000001, t0, t1, t2);
   TESTINST1("srlv $t0, $t1, $t2", 0x31415927, 0x00000002, t0, t1, t2);
   TESTINST1("srlv $t0, $t1, $t2", 0x31415927, 0x0000000F, t0, t1, t2);
   TESTINST1("srlv $t0, $t1, $t2", 0x31415927, 0x00000010, t0, t1, t2);
   TESTINST1("srlv $t0, $t1, $t2", 0x31415927, 0x0000001F, t0, t1, t2);
   TESTINST1("srlv $t0, $t1, $t2", 0x31415927, 0x00000020, t0, t1, t2);
   TESTINST1("srlv $t0, $t1, $t2", 0x31415927, 0x00000021, t0, t1, t2);
   TESTINST1("srlv $t0, $t1, $t2", 0x00088000, 0x00000000, t0, t1, t2);
   TESTINST1("srlv $t0, $t1, $t2", 0x00088000, 0x00000001, t0, t1, t2);
   TESTINST1("srlv $t0, $t1, $t2", 0x00088000, 31, t0, t1, t2);
   TESTINST1("srlv $t0, $t1, $t2", 0x00010000, 16, t0, t1, t2);
   TESTINST1("srlv $t0, $t1, $t2", 0x00010000, 17, t0, t1, t2);
   TESTINST1("srlv $t0, $t1, $t2", 0x00010000, 18, t0, t1, t2);
   TESTINST1("srlv $t0, $t1, $t2", 0, 0, t0, t1, t2);
   TESTINST1("srlv $t0, $t1, $t2", 0xffff, 0xffff, t0, t1, t2);

   printf("SUBU\n");
   TESTINST1("subu $t0, $t1, $t2", 0x31415927, 0x00000001, t0, t1, t2);
   TESTINST1("subu $t0, $t1, $t2", 0x31415927, 0x00001110, t0, t1, t2);
   TESTINST1("subu $t0, $t1, $t2", 0,          255,        t0, t1, t2);
   TESTINST1("subu $t0, $t1, $t2", -1,         0,          t0, t1, t2);
   TESTINST1("subu $t0, $t1, $t2", 0,          1,          t0, t1, t2);
   TESTINST1("subu $t0, $t1, $t2", 0,          0,          t0, t1, t2);
   TESTINST1("subu $t0, $t1, $t2", 0x80000000, -1,         t0, t1, t2);
   TESTINST1("subu $t0, $t1, $t2", 0x80000000, 0x80000000, t0, t1, t2);
   TESTINST1("subu $t0, $t1, $t2", 0x7fffffff, 0,          t0, t1, t2);
   TESTINST1("subu $t0, $t1, $t2", 0x80000000, 0x80000000, t0, t1, t2);
   TESTINST1("subu $t0, $t1, $t2", 0x7fffffff, 0x80000000, t0, t1, t2);
   TESTINST1("subu $t0, $t1, $t2", 0x80000000, 0xff000000, t0, t1, t2);
   TESTINST1("subu $t0, $t1, $t2", 0x7fffffff, 0x0dd00000, t0, t1, t2);
   TESTINST1("subu $t0, $t1, $t2", 0x31415927, 0xffffffff, t0, t1, t2);
   TESTINST1("subu $t0, $t1, $t2", 0x31415927, 0xee00ee00, t0, t1, t2);
   TESTINST1("subu $t0, $t1, $t2", 0,          255,        t0, t1, t2);
   TESTINST1("subu $t0, $t1, $t2", 1,          0,          t0, t1, t2);
   TESTINST1("subu $t0, $t1, $t2", 0,          1,          t0, t1, t2);
   TESTINST1("subu $t0, $t1, $t2", -1,         0,          t0, t1, t2);
   TESTINST1("subu $t0, $t1, $t2", 0,          -1,         t0, t1, t2);
   TESTINST1("subu $t0, $t1, $t2", 0,          0x80000000, t0, t1, t2);
   TESTINST1("subu $t0, $t1, $t2", 0x80000000, 0,          t0, t1, t2);
   TESTINST1("subu $t0, $t1, $t2", 0x80000000, 0x80000000, t0, t1, t2);
   TESTINST1("subu $t0, $t1, $t2", 0x7fffffff, 0x80000000, t0, t1, t2);
   TESTINST1("subu $t0, $t1, $t2", 0x80000000, 0xff000000, t0, t1, t2);
   TESTINST1("subu $t0, $t1, $t2", 0x7fffffff, 0x0dd00000, t0, t1, t2);
   TESTINST1("subu $t0, $t1, $t2", 0xffffffff, 0,          t0, t1, t2);
   TESTINST1("subu $t0, $t1, $t2", 0,          0xffffffff, t0, t1, t2);
   TESTINST1("subu $t0, $t1, $t2", 0xffffffff, 0xffffffff, t0, t1, t2);
   TESTINST1("subu $t0, $t1, $t2", 0x7fffffff, 0x7fffffff, t0, t1, t2);
   TESTINST1("subu $t0, $t1, $t2", 0x0000ffff, 0x0000ffff, t0, t1, t2);
   TESTINST1("subu $t0, $t1, $t2", 0xffffffff, 0,          t0, t1, t2);
   TESTINST1("subu $t0, $t1, $t2", 0,          0xffffffff, t0, t1, t2);
   TESTINST1("subu $t0, $t1, $t2", 0xffffffff, 0xffffffff, t0, t1, t2);
   TESTINST1("subu $t0, $t1, $t2", 0x7fffffff, 0x7fffffff, t0, t1, t2);
   TESTINST1("subu $t0, $t1, $t2", 0x0000ffff, 0x0000ffff, t0, t1, t2);
   TESTINST1("subu $t0, $t1, $t2", 0x31415927, 0x00000000, t0, t1, t2);
   TESTINST1("subu $t0, $t1, $t2", 0x31415927, 0x00000001, t0, t1, t2);
   TESTINST1("subu $t0, $t1, $t2", 0x31415927, 0x00000002, t0, t1, t2);
   TESTINST1("subu $t0, $t1, $t2", 0x31415927, 0x0000000F, t0, t1, t2);
   TESTINST1("subu $t0, $t1, $t2", 0x31415927, 0x00000010, t0, t1, t2);
   TESTINST1("subu $t0, $t1, $t2", 0x31415927, 0x0000001F, t0, t1, t2);
   TESTINST1("subu $t0, $t1, $t2", 0x31415927, 0x00000020, t0, t1, t2);
   TESTINST1("subu $t0, $t1, $t2", 0x31415927, 0x00000021, t0, t1, t2);
   TESTINST1("subu $t0, $t1, $t2", 0x00088000, 0x00000000, t0, t1, t2);
   TESTINST1("subu $t0, $t1, $t2", 0x00088000, 0x00000001, t0, t1, t2);
   TESTINST1("subu $t0, $t1, $t2", 0x00088000, 31, t0, t1, t2);
   TESTINST1("subu $t0, $t1, $t2", 0x00010000, 16, t0, t1, t2);
   TESTINST1("subu $t0, $t1, $t2", 0x00010000, 17, t0, t1, t2);
   TESTINST1("subu $t0, $t1, $t2", 0x00010000, 18, t0, t1, t2);
   TESTINST1("subu $t0, $t1, $t2", 0, 0, t0, t1, t2);
   TESTINST1("subu $t0, $t1, $t2", 0xffff, 0xffff, t0, t1, t2);

   printf("SUB\n");
   TESTINST1("subu $t0, $t1, $t2", 0x31415927, 0xffffffff, t0, t1, t2);
   TESTINST1("subu $t0, $t1, $t2", 0x31415927, 0x27181728, t0, t1, t2);
   TESTINST1("subu $t0, $t1, $t2", 0x31415927, 0x97181728, t0, t1, t2);
   TESTINST1("subu $t0, $t1, $t2", 0,          0,          t0, t1, t2);
   TESTINST1("subu $t0, $t1, $t2", 1,          0,          t0, t1, t2);
   TESTINST1("subu $t0, $t1, $t2", 0,          1,          t0, t1, t2);
   TESTINST1("subu $t0, $t1, $t2", -1,         0,          t0, t1, t2);
   TESTINST1("subu $t0, $t1, $t2", 0,          -1,         t0, t1, t2);
   TESTINST1("subu $t0, $t1, $t2", 0,          0x80000000, t0, t1, t2);
   TESTINST1("subu $t0, $t1, $t2", 0x80000000, 0,          t0, t1, t2);
   TESTINST1("subu $t0, $t1, $t2", 0x80000000, 0x80000000, t0, t1, t2);
   TESTINST1("subu $t0, $t1, $t2", 0x7fffffff, 0x80000000, t0, t1, t2);
   TESTINST1("subu $t0, $t1, $t2", 0x80000000, 0x7fffffff, t0, t1, t2);
   TESTINST1("subu $t0, $t1, $t2", 0x7fffffff, 0x7fffffff, t0, t1, t2);

   printf("XOR\n");
   TESTINST1("xor $t0, $t1, $t2", 0x31415927, 0xffffffff, t0, t1, t2);
   TESTINST1("xor $t0, $t1, $t2", 0x31415927, 0xee00ee00, t0, t1, t2);
   TESTINST1("xor $t0, $t1, $t2", 0,          255,        t0, t1, t2);
   TESTINST1("xor $t0, $t1, $t2", -1,         0,          t0, t1, t2);
   TESTINST1("xor $t0, $t1, $t2", 0,          1,          t0, t1, t2);
   TESTINST1("xor $t0, $t1, $t2", 0,          0,          t0, t1, t2);
   TESTINST1("xor $t0, $t1, $t2", 0x80000000, -1,         t0, t1, t2);
   TESTINST1("xor $t0, $t1, $t2", 0x80000000, 0x80000000, t0, t1, t2);
   TESTINST1("xor $t0, $t1, $t2", 0x7fffffff, 0,          t0, t1, t2);
   TESTINST1("xor $t0, $t1, $t2", 0x80000000, 0x80000000, t0, t1, t2);
   TESTINST1("xor $t0, $t1, $t2", 0x7fffffff, 0x80000000, t0, t1, t2);
   TESTINST1("xor $t0, $t1, $t2", 0x80000000, 0xff000000, t0, t1, t2);
   TESTINST1("xor $t0, $t1, $t2", 0x7fffffff, 0x0dd00000, t0, t1, t2);
   TESTINST1("xor $t0, $t1, $t2", 0x31415927, 0xffffffff, t0, t1, t2);
   TESTINST1("xor $t0, $t1, $t2", 0x31415927, 0xee00ee00, t0, t1, t2);
   TESTINST1("xor $t0, $t1, $t2", 0,          255,        t0, t1, t2);
   TESTINST1("xor $t0, $t1, $t2", 1,          0,          t0, t1, t2);
   TESTINST1("xor $t0, $t1, $t2", 0,          1,          t0, t1, t2);
   TESTINST1("xor $t0, $t1, $t2", -1,         0,          t0, t1, t2);
   TESTINST1("xor $t0, $t1, $t2", 0,          -1,         t0, t1, t2);
   TESTINST1("xor $t0, $t1, $t2", 0,          0x80000000, t0, t1, t2);
   TESTINST1("xor $t0, $t1, $t2", 0x80000000, 0,          t0, t1, t2);
   TESTINST1("xor $t0, $t1, $t2", 0x80000000, 0x80000000, t0, t1, t2);
   TESTINST1("xor $t0, $t1, $t2", 0x7fffffff, 0x80000000, t0, t1, t2);
   TESTINST1("xor $t0, $t1, $t2", 0x80000000, 0xff000000, t0, t1, t2);
   TESTINST1("xor $t0, $t1, $t2", 0x7fffffff, 0x0dd00000, t0, t1, t2);
   TESTINST1("xor $t0, $t1, $t2", 0xffffffff, 0,          t0, t1, t2);
   TESTINST1("xor $t0, $t1, $t2", 0,          0xffffffff, t0, t1, t2);
   TESTINST1("xor $t0, $t1, $t2", 0xffffffff, 0xffffffff, t0, t1, t2);
   TESTINST1("xor $t0, $t1, $t2", 0x7fffffff, 0x7fffffff, t0, t1, t2);
   TESTINST1("xor $t0, $t1, $t2", 0x0000ffff, 0x0000ffff, t0, t1, t2);

   printf("XORI\n");
   TESTINST2("xori $t0, $t1, 0xffff", 0x31415927, 0xffff, t0, t1);
   TESTINST2("xori $t0, $t1, 0xee00", 0x31415927, 0xee00, t0, t1);
   TESTINST2("xori $t0, $t1, 255", 0,          255,        t0, t1);
   TESTINST2("xori $t0, $t1, 0", -1,         0,          t0, t1);
   TESTINST2("xori $t0, $t1, 1", 0,          1,          t0, t1);
   TESTINST2("xori $t0, $t1, 0", 0,          0,          t0, t1);
   TESTINST2("xori $t0, $t1, 0x8000", 0x80000000, 0x8000, t0, t1);
   TESTINST2("xori $t0, $t1, 0", 0x7fffffff, 0,          t0, t1);
   TESTINST2("xori $t0, $t1, 0x8000", 0x80000000, 0x8000, t0, t1);
   TESTINST2("xori $t0, $t1, 0x8000", 0x7fffffff, 0x8000, t0, t1);
   TESTINST2("xori $t0, $t1, 0xff00", 0x80000000, 0xff00, t0, t1);
   TESTINST2("xori $t0, $t1, 0x0dd0", 0x7fffffff, 0x0dd0, t0, t1);
   TESTINST2("xori $t0, $t1, 0xffff", 0x31415927, 0xffff, t0, t1);
   TESTINST2("xori $t0, $t1, 0xee00", 0x31415927, 0xee00, t0, t1);
   TESTINST2("xori $t0, $t1, 255", 0,          255,        t0, t1);
   TESTINST2("xori $t0, $t1, 0", 1,          0,          t0, t1);
   TESTINST2("xori $t0, $t1, 1", 0,          1,          t0, t1);
   TESTINST2("xori $t0, $t1, 0", -1,         0,          t0, t1);
   TESTINST2("xori $t0, $t1, 0x8000", 0,          0x8000, t0, t1);
   TESTINST2("xori $t0, $t1, 0", 0x8000, 0,          t0, t1);
   TESTINST2("xori $t0, $t1, 0x8000", 0x80000000, 0x8000, t0, t1);
   TESTINST2("xori $t0, $t1, 0x8000", 0x7fffffff, 0x8000, t0, t1);
   TESTINST2("xori $t0, $t1, 0xff00", 0x80000000, 0xff00, t0, t1);
   TESTINST2("xori $t0, $t1, 0x0dd0", 0x7fffffff, 0x0dd0, t0, t1);
   TESTINST2("xori $t0, $t1, 0", 0xffff, 0,          t0, t1);
   TESTINST2("xori $t0, $t1, 0xffff", 0,          0xffff, t0, t1);
   TESTINST2("xori $t0, $t1, 0xffff", 0xffffffff, 0xffff, t0, t1);
   TESTINST2("xori $t0, $t1, 0x7fff", 0x7fffffff, 0x7fff, t0, t1);
   TESTINST2("xori $t0, $t1, 0x0000", 0x0000ffff, 0x0000, t0, t1);

#if (__mips_isa_rev < 6)
   printf("MFHI MFLO\n");
   TESTINSN_HILO(0x31415927);
   TESTINSN_HILO(0);
   TESTINSN_HILO(-1);
   TESTINSN_HILO(0xffffffff);
   TESTINSN_HILO(0x8000);
   TESTINSN_HILO(0x80000000);
   TESTINSN_HILO(0x0000ffff);
   TESTINSN_HILO(0x7fff);
   TESTINSN_HILO(0x0dd0);
   TESTINSN_HILO(0xff00);
#endif

   return 0;
}
