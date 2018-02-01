#include <stdio.h>

#define TESTINST1(RSval, RD) \
{ \
   unsigned int out = 0; \
   __asm__ volatile( \
      ".set push \n\t" \
      ".set noreorder \n\t" \
      "move $" #RD ", %1\n\t" \
      "b end"#RSval"\n\t" \
      "nop\n\t" \
      "addiu $" #RD ", $" #RD", 5\n\t" \
      "end"#RSval":\n\t" \
      "addiu $" #RD ", $" #RD", 1\n\t" \
      "move %0, $" #RD "\n\t" \
      ".set pop \n\t" \
      : "=&r" (out) \
      : "r" (RSval) \
      : #RD, "memory" \
        ); \
        printf("B :: %d, RSval: %d\n", \
        out, RSval); \
}

#define TESTINST2(RSval, RD) \
{ \
   unsigned int out = 0; \
   __asm__ volatile( \
      ".set push \n\t" \
      ".set noreorder \n\t" \
      "move $" #RD ", %1\n\t" \
      "b end12"#RSval"\n\t" \
      "addiu $" #RD ", $" #RD", 3\n\t" \
      "addiu $" #RD ", $" #RD", 5\n\t" \
      "end12"#RSval":\n\t" \
      "addiu $" #RD ", $" #RD", 3\n\t" \
      "move %0, $" #RD "\n\t" \
      ".set pop \n\t" \
      : "=&r" (out) \
      : "r" (RSval) \
      : #RD, "memory" \
        ); \
        printf("B :: %d, RSval: %d\n", \
        out, RSval); \
}

#define TESTINST3(RSval, RD) \
{ \
   unsigned int out = 0; \
   __asm__ volatile( \
      ".set push \n\t" \
      ".set noreorder \n\t" \
      "move $" #RD ", %1\n\t" \
      "bal end21"#RSval"\n\t" \
      "nop\n\t" \
      "addiu $" #RD ", $" #RD", 5\n\t" \
      "b r_end"#RSval"\n\t" \
      "nop\n\t" \
      "end21"#RSval":\n\t" \
      "addiu $" #RD ", $" #RD", 1\n\t" \
      "jr $ra\n\t"  \
      "r_end"#RSval":\n\t" \
      "move %0, $" #RD "\n\t" \
      ".set pop \n\t" \
      : "=&r" (out) \
      : "r" (RSval) \
      : #RD, "memory" \
        ); \
        printf("B BAL JR :: %d, RSval: %d\n", \
        out, RSval); \
}

#define TESTINST3j(RSval, RD) \
{ \
   unsigned int out = 0; \
   __asm__ volatile( \
      ".set push \n\t" \
      ".set noreorder \n\t" \
      "move $" #RD ", %1\n\t" \
      "la $t0, end31"#RSval"\n\t" \
      "jal $t0\n\t" \
      "nop\n\t" \
      "addiu $" #RD ", $" #RD", 5\n\t" \
      "la $t0, r_end11"#RSval"\n\t" \
      "j $t0\n\t" \
      "nop\n\t" \
      "end31"#RSval":\n\t" \
      "addiu $" #RD ", $" #RD", 1\n\t" \
      "jr $ra\n\t"  \
      "r_end11"#RSval":\n\t" \
      "move %0, $" #RD "\n\t" \
      ".set pop \n\t" \
      : "=&r" (out) \
      : "r" (RSval) \
      : #RD, "t0", "memory" \
        ); \
        printf("J JAL JR :: %d, RSval: %d\n", \
        out, RSval); \
}

#define TESTINST3ja(RSval, RD) \
{ \
   unsigned int out = 0; \
   __asm__ volatile( \
      ".set push \n\t" \
      ".set noreorder \n\t" \
      "move $" #RD ", %1\n\t" \
      "la $t0, end41"#RSval"\n\t" \
      "jalr $t1, $t0\n\t" \
      "nop\n\t" \
      "addiu $" #RD ", $" #RD", 5\n\t" \
      "la $t0, r_end21"#RSval"\n\t" \
      "j $t0\n\t" \
      "nop\n\t" \
      "end41"#RSval":\n\t" \
      "addiu $" #RD ", $" #RD", 1\n\t" \
      "jr $t1\n\t"  \
      "r_end21"#RSval":\n\t" \
      "move %0, $" #RD "\n\t" \
      ".set pop \n\t" \
      : "=&r" (out) \
      : "r" (RSval) \
      : #RD, "t0", "t1", "memory" \
        ); \
        printf("J JALR JR :: %d, RSval: %d\n", \
        out, RSval); \
}

#define TESTINST4(instruction, RDval, RSval, RTval, RD, RS, RT) \
{ \
   unsigned int out = 0; \
   __asm__ volatile( \
      ".set push \n\t" \
      ".set noreorder \n\t" \
      "move $" #RS ", %1\n\t" \
      "move $" #RT ", %2\n\t" \
      "move $" #RD ", %3\n\t" \
      instruction" $" #RS ", $" #RT ", end"instruction#RDval"\n\t" \
      "nop\n\t" \
      "addiu $" #RD ", $" #RD", 5\n\t" \
      "end"instruction#RDval":\n\t" \
      "addiu $" #RD ", $" #RD", 1\n\t" \
      "move %0, $" #RD "\n\t" \
      ".set pop \n\t" \
      : "=&r" (out) \
      : "r" (RSval), "r" (RTval), "r" (RDval) \
      : #RD, #RS, #RT, "memory" \
        ); \
        printf(instruction" :: %d, RSval: %d, RTval: %d\n", \
        out, RSval, RTval); \
}

#define TESTINST5(instruction, RDval, RSval, RD, RS) \
{ \
   unsigned int out = 0; \
   __asm__ volatile( \
      ".set push \n\t" \
      ".set noreorder \n\t" \
      "move $" #RS ", %1\n\t" \
      "move $" #RD ", %2\n\t" \
      instruction" $" #RS ", end"instruction#RDval"\n\t" \
      "nop\n\t" \
      "addiu $" #RD ", $" #RD", 5\n\t" \
      "end"instruction#RDval":\n\t" \
      "addiu $" #RD ", $" #RD", 1\n\t" \
      "move %0, $" #RD "\n\t" \
      ".set pop \n\t" \
      : "=&r" (out) \
      : "r" (RSval), "r" (RDval) \
      : #RD, #RS, "memory" \
        ); \
        printf(instruction" :: %d, RSval: %d\n", \
        out, RSval); \
}

#define TESTINST6(instruction, RDval, RSval, RD, RS) \
{ \
   unsigned int out = 0; \
   __asm__ volatile( \
      ".set push \n\t" \
      ".set noreorder \n\t" \
      "move $" #RD ", %2\n\t" \
      "move $" #RS ", %1\n\t" \
      instruction" $" #RS ", end21"instruction#RDval"\n\t" \
      "nop\n\t" \
      "addiu $" #RD ", $" #RD", 5\n\t" \
      "b r_end"instruction#RDval"\n\t" \
      "nop\n\t" \
      "end21"instruction#RDval":\n\t" \
      "addiu $" #RD ", $" #RD", 1\n\t" \
      "jr $ra\n\t"  \
      "r_end"instruction#RDval":\n\t" \
      "move %0, $" #RD "\n\t" \
      ".set pop \n\t" \
      : "=&r" (out) \
      : "r" (RSval), "r" (RDval) \
      : #RD, #RS, "memory" \
        ); \
        printf(instruction" :: %d, RSval: %d\n", \
        out, RSval); \
}

#define TESTINST4l(instruction, RDval, RSval, RTval, RD, RS, RT) \
{ \
   unsigned int out = 0; \
   __asm__ volatile( \
      ".set push \n\t" \
      ".set noreorder \n\t" \
      "move $" #RS ", %1\n\t" \
      "move $" #RT ", %2\n\t" \
      "move $" #RD ", %3\n\t" \
      instruction" $" #RS ", $" #RT ", end"instruction#RDval"\n\t" \
      "addiu $" #RD ", $" #RD", 3\n\t" \
      "addiu $" #RD ", $" #RD", 5\n\t" \
      "end"instruction#RDval":\n\t" \
      "addiu $" #RD ", $" #RD", 1\n\t" \
      "move %0, $" #RD "\n\t" \
      ".set pop \n\t" \
      : "=&r" (out) \
      : "r" (RSval), "r" (RTval), "r" (RDval) \
      : #RD, #RS, #RT, "memory" \
        ); \
        printf(instruction" :: %d, RSval: %d, RTval: %d\n", \
        out, RSval, RTval); \
}

#define TESTINST5l(instruction, RDval, RSval, RD, RS) \
{ \
   unsigned int out = 0; \
   __asm__ volatile( \
      ".set push \n\t" \
      ".set noreorder \n\t" \
      "move $" #RS ", %1\n\t" \
      "move $" #RD ", %2\n\t" \
      instruction" $" #RS ", end"instruction#RDval"\n\t" \
      "addiu $" #RD ", $" #RD", 3\n\t" \
      "addiu $" #RD ", $" #RD", 5\n\t" \
      "end"instruction#RDval":\n\t" \
      "addiu $" #RD ", $" #RD", 1\n\t" \
      "move %0, $" #RD "\n\t" \
      ".set pop \n\t" \
      : "=&r" (out) \
      : "r" (RSval), "r" (RDval) \
      : #RD, #RS, "memory" \
        ); \
        printf(instruction" :: %d, RSval: %d\n", \
        out, RSval); \
}

#define TESTINST6l(instruction, RDval, RSval, RD, RS) \
{ \
   unsigned int out = 0; \
   __asm__ volatile( \
      ".set push \n\t" \
      ".set noreorder \n\t" \
      "move $" #RD ", %2\n\t" \
      "move $" #RS ", %1\n\t" \
      instruction" $" #RS ", end21"instruction#RDval"\n\t" \
      "addiu $" #RD ", $" #RD", 3\n\t" \
      "addiu $" #RD ", $" #RD", 5\n\t" \
      "b r_end"instruction#RDval"\n\t" \
      "nop\n\t" \
      "end21"instruction#RDval":\n\t" \
      "addiu $" #RD ", $" #RD", 1\n\t" \
      "jr $ra\n\t"  \
      "r_end"instruction#RDval":\n\t" \
      "move %0, $" #RD "\n\t" \
      ".set pop \n\t" \
      : "=&r" (out) \
      : "r" (RSval), "r" (RDval) \
      : #RD, #RS, "memory" \
        ); \
        printf(instruction" :: %d, RSval: %d\n", \
        out, RSval); \
}

int main()
{
   printf("b\n");
   TESTINST1(0, v0);
   TESTINST1(1, v1);
   TESTINST1(2, a0);
   TESTINST1(3, a1);
   TESTINST1(4, a2);
   TESTINST1(5, a3);
   TESTINST1(6, t0);
   TESTINST1(7, t1);
   TESTINST1(8, t2);
   TESTINST1(9, t3);
   TESTINST1(10, t4);
   TESTINST1(11, t5);
   TESTINST1(12, t6);
   TESTINST1(13, t7);
   TESTINST1(14, s0);
   TESTINST1(15, s1);
   TESTINST1(16, s2);
   TESTINST1(17, s3);
   TESTINST1(18, s4);
   TESTINST1(19, s5);
   TESTINST1(20, s6);
   TESTINST1(21, s7);
   TESTINST1(22, t8);
   TESTINST1(23, t9);

   printf("b\n");
   TESTINST2(0, v0);
   TESTINST2(1, v1);
   TESTINST2(2, a0);
   TESTINST2(3, a1);
   TESTINST2(4, a2);
   TESTINST2(5, a3);
   TESTINST2(6, t0);
   TESTINST2(7, t1);
   TESTINST2(8, t2);
   TESTINST2(9, t3);
   TESTINST2(10, t4);
   TESTINST2(11, t5);
   TESTINST2(12, t6);
   TESTINST2(13, t7);
   TESTINST2(14, s0);
   TESTINST2(15, s1);
   TESTINST2(16, s2);
   TESTINST2(17, s3);
   TESTINST2(18, s4);
   TESTINST2(19, s5);
   TESTINST2(20, s6);
   TESTINST2(21, s7);
   TESTINST2(22, t8);
   TESTINST2(23, t9);

   printf("b, bal, jr\n");
   TESTINST3(0, v0);
   TESTINST3(1, v1);
   TESTINST3(2, a0);
   TESTINST3(3, a1);
   TESTINST3(4, a2);
   TESTINST3(5, a3);
   TESTINST3(6, t0);
   TESTINST3(7, t1);
   TESTINST3(8, t2);
   TESTINST3(9, t3);
   TESTINST3(10, t4);
   TESTINST3(11, t5);
   TESTINST3(12, t6);
   TESTINST3(13, t7);
   TESTINST3(14, s0);
   TESTINST3(15, s1);
   TESTINST3(16, s2);
   TESTINST3(17, s3);
   TESTINST3(18, s4);
   TESTINST3(19, s5);
   TESTINST3(20, s6);
   TESTINST3(21, s7);
   TESTINST3(22, t8);
   TESTINST3(23, t9);

   printf("beq\n");
   TESTINST4("beq", 0, 0, 1, v0, v1, a0);
   TESTINST4("beq", 1, 1, 1, v1, a0, a1);
   TESTINST4("beq", 2, 0xffffffff, 0xffffffff, a0, a1, a2);
   TESTINST4("beq", 3, 0xffffffff, 0xfffffffe, a1, a2, a3);
   TESTINST4("beq", 4, 0xfffffffe, 0xffffffff, a2, t0, t1);
   TESTINST4("beq", 5, 0xffffffff, 0xffffffff, a3, t0, t1);
   TESTINST4("beq", 6, 0x5, 0x5, t0, t1, t2);
   TESTINST4("beq", 7, -3, -4, t1, t2, t3);
   TESTINST4("beq", 8, 125, 125, t2, t3, t4);
   TESTINST4("beq", 9, 0x80000000, 0x80000000, t3, t4, t5);
   TESTINST4("beq", 10, 0xffffffff, 0x80000000, t4, t5, t6);
   TESTINST4("beq", 11, 0x256, 0x256, t5, t6, t7);
   TESTINST4("beq", 12, 0x55, 0x55, t6, t7, s0);
   TESTINST4("beq", 13, 0xfff, 0xdd, s0, s1, s2);
   TESTINST4("beq", 14, -1, 0x5, v0, t9, t8);
   TESTINST4("beq", 15, -1, -1, t9, t8, a3);

   printf("bne\n");
   TESTINST4("bne", 0, 0, 1, v0, v1, a0);
   TESTINST4("bne", 1, 1, 1, v1, a0, a1);
   TESTINST4("bne", 2, 0xffffffff, 0xffffffff, a0, a1, a2);
   TESTINST4("bne", 3, 0xffffffff, 0xfffffffe, a1, a2, a3);
   TESTINST4("bne", 4, 0xfffffffe, 0xffffffff, a2, t0, t1);
   TESTINST4("bne", 5, 0xffffffff, 0xffffffff, a3, t0, t1);
   TESTINST4("bne", 6, 0x5, 0x5, t0, t1, t2);
   TESTINST4("bne", 7, -3, -4, t1, t2, t3);
   TESTINST4("bne", 8, 125, 125, t2, t3, t4);
   TESTINST4("bne", 9, 0x80000000, 0x80000000, t3, t4, t5);
   TESTINST4("bne", 10, 0xffffffff, 0x80000000, t4, t5, t6);
   TESTINST4("bne", 11, 0x256, 0x256, t5, t6, t7);
   TESTINST4("bne", 12, 0x55, 0x55, t6, t7, s0);
   TESTINST4("bne", 13, 0xfff, 0xdd, s0, s1, s2);
   TESTINST4("bne", 14, -1, 0x5, v0, t9, t8);
   TESTINST4("bne", 15, -1, -1, t9, t8, a3);

   printf("BEQZ\n");
   TESTINST5("beqz", 0, 0, v0, v1);
   TESTINST5("beqz", 1, 1, v1, a0);
   TESTINST5("beqz", 2, 0xffffffff, a0, a1);
   TESTINST5("beqz", 3, 0xffffffff, a1, a2);
   TESTINST5("beqz", 4, 0xfffffffe, a2, t0);
   TESTINST5("beqz", 5, 0xffffffff, a3, t0);
   TESTINST5("beqz", 6, 0x5, t0, t1);
   TESTINST5("beqz", 7, -3, t1, t2);
   TESTINST5("beqz", 8, 125, t2, t3);
   TESTINST5("beqz", 9, 0x80000000, t3, t4);
   TESTINST5("beqz", 10, 0xffffffff, t4, t5);
   TESTINST5("beqz", 11, 0x256, t5, t6);
   TESTINST5("beqz", 12, 0x55, t6, t7);
   TESTINST5("beqz", 13, 0xfff, s0, s1);
   TESTINST5("beqz", 14, -1, v0, t9);
   TESTINST5("beqz", 15, -1, t9, t8);

   printf("BGEZ\n");
   TESTINST5("bgez", 0, 0, v0, v1);
   TESTINST5("bgez", 1, 1, v1, a0);
   TESTINST5("bgez", 2, 0xffffffff, a0, a1);
   TESTINST5("bgez", 3, 0xffffffff, a1, a2);
   TESTINST5("bgez", 4, 0xfffffffe, a2, t0);
   TESTINST5("bgez", 5, 0xffffffff, a3, t0);
   TESTINST5("bgez", 6, 0x5, t0, t1);
   TESTINST5("bgez", 7, -3, t1, t2);
   TESTINST5("bgez", 8, 125, t2, t3);
   TESTINST5("bgez", 9, 0x80000000, t3, t4);
   TESTINST5("bgez", 10, 0xffffffff, t4, t5);
   TESTINST5("bgez", 11, 0x256, t5, t6);
   TESTINST5("bgez", 12, 0x55, t6, t7);
   TESTINST5("bgez", 13, 0xfff, s0, s1);
   TESTINST5("bgez", 14, -1, v0, t9);
   TESTINST5("bgez", 15, -1, t9, t8);

   printf("BGTZ\n");
   TESTINST5("bgtz", 0, 0, v0, v1);
   TESTINST5("bgtz", 1, 1, v1, a0);
   TESTINST5("bgtz", 2, 0xffffffff, a0, a1);
   TESTINST5("bgtz", 3, 0xffffffff, a1, a2);
   TESTINST5("bgtz", 4, 0xfffffffe, a2, t0);
   TESTINST5("bgtz", 5, 0xffffffff, a3, t0);
   TESTINST5("bgtz", 6, 0x5, t0, t1);
   TESTINST5("bgtz", 7, -3, t1, t2);
   TESTINST5("bgtz", 8, 125, t2, t3);
   TESTINST5("bgtz", 9, 0x80000000, t3, t4);
   TESTINST5("bgtz", 10, 0xffffffff, t4, t5);
   TESTINST5("bgtz", 11, 0x256, t5, t6);
   TESTINST5("bgtz", 12, 0x55, t6, t7);
   TESTINST5("bgtz", 13, 0xfff, s0, s1);
   TESTINST5("bgtz", 14, -1, v0, t9);
   TESTINST5("bgtz", 15, -1, t9, t8);

   printf("BLEZ\n");
   TESTINST5("blez", 0, 0, v0, v1);
   TESTINST5("blez", 1, 1, v1, a0);
   TESTINST5("blez", 2, 0xffffffff, a0, a1);
   TESTINST5("blez", 3, 0xffffffff, a1, a2);
   TESTINST5("blez", 4, 0xfffffffe, a2, t0);
   TESTINST5("blez", 5, 0xffffffff, a3, t0);
   TESTINST5("blez", 6, 0x5, t0, t1);
   TESTINST5("blez", 7, -3, t1, t2);
   TESTINST5("blez", 8, 125, t2, t3);
   TESTINST5("blez", 9, 0x80000000, t3, t4);
   TESTINST5("blez", 10, 0xffffffff, t4, t5);
   TESTINST5("blez", 11, 0x256, t5, t6);
   TESTINST5("blez", 12, 0x55, t6, t7);
   TESTINST5("blez", 13, 0xfff, s0, s1);
   TESTINST5("blez", 14, -1, v0, t9);
   TESTINST5("blez", 15, -1, t9, t8);

   printf("BLTZ\n");
   TESTINST5("bltz", 0, 0, v0, v1);
   TESTINST5("bltz", 1, 1, v1, a0);
   TESTINST5("bltz", 2, 0xffffffff, a0, a1);
   TESTINST5("bltz", 3, 0xffffffff, a1, a2);
   TESTINST5("bltz", 4, 0xfffffffe, a2, t0);
   TESTINST5("bltz", 5, 0xffffffff, a3, t0);
   TESTINST5("bltz", 6, 0x5, t0, t1);
   TESTINST5("bltz", 7, -3, t1, t2);
   TESTINST5("bltz", 8, 125, t2, t3);
   TESTINST5("bltz", 9, 0x80000000, t3, t4);
   TESTINST5("bltz", 10, 0xffffffff, t4, t5);
   TESTINST5("bltz", 11, 0x256, t5, t6);
   TESTINST5("bltz", 12, 0x55, t6, t7);
   TESTINST5("bltz", 13, 0xfff, s0, s1);
   TESTINST5("bltz", 14, -1, v0, t9);
   TESTINST5("bltz", 15, -1, t9, t8);

#if (__mips_isa_rev < 6)
   printf("BGEZAL\n");
   TESTINST6("bgezal", 0, 0, v0, v1);
   TESTINST6("bgezal", 1, 1, v1, a0);
   TESTINST6("bgezal", 2, 0xffffffff, a0, a1);
   TESTINST6("bgezal", 3, 0xffffffff, a1, a2);
   TESTINST6("bgezal", 4, 0xfffffffe, a2, t0);
   TESTINST6("bgezal", 5, 0xffffffff, a3, t0);
   TESTINST6("bgezal", 6, 0x5, t0, t1);
   TESTINST6("bgezal", 7, -3, t1, t2);
   TESTINST6("bgezal", 8, 125, t2, t3);
   TESTINST6("bgezal", 9, 0x80000000, t3, t4);
   TESTINST6("bgezal", 10, 0xffffffff, t4, t5);
   TESTINST6("bgezal", 11, 0x256, t5, t6);
   TESTINST6("bgezal", 12, 0x55, t6, t7);
   TESTINST6("bgezal", 13, 0xfff, s0, s1);
   TESTINST6("bgezal", 14, -1, v0, t9);
   TESTINST6("bgezal", 15, -1, t9, t8);

   printf("BLTZAL\n");
   TESTINST6("bltzal", 0, 0, v0, v1);
   TESTINST6("bltzal", 1, 1, v1, a0);
   TESTINST6("bltzal", 2, 0xffffffff, a0, a1);
   TESTINST6("bltzal", 3, 0xffffffff, a1, a2);
   TESTINST6("bltzal", 4, 0xfffffffe, a2, t0);
   TESTINST6("bltzal", 5, 0xffffffff, a3, t0);
   TESTINST6("bltzal", 6, 0x5, t0, t1);
   TESTINST6("bltzal", 7, -3, t1, t2);
   TESTINST6("bltzal", 8, 125, t2, t3);
   TESTINST6("bltzal", 9, 0x80000000, t3, t4);
   TESTINST6("bltzal", 10, 0xffffffff, t4, t5);
   TESTINST6("bltzal", 11, 0x256, t5, t6);
   TESTINST6("bltzal", 12, 0x55, t6, t7);
   TESTINST6("bltzal", 13, 0xfff, s0, s1);
   TESTINST6("bltzal", 14, -1, v0, t9);
   TESTINST6("bltzal", 15, -1, t9, t8);
#endif

   printf("BNEZ\n");
   TESTINST5("bnez", 0, 0, v0, v1);
   TESTINST5("bnez", 1, 1, v1, a0);
   TESTINST5("bnez", 2, 0xffffffff, a0, a1);
   TESTINST5("bnez", 3, 0xffffffff, a1, a2);
   TESTINST5("bnez", 4, 0xfffffffe, a2, t0);
   TESTINST5("bnez", 5, 0xffffffff, a3, t0);
   TESTINST5("bnez", 6, 0x5, t0, t1);
   TESTINST5("bnez", 7, -3, t1, t2);
   TESTINST5("bnez", 8, 125, t2, t3);
   TESTINST5("bnez", 9, 0x80000000, t3, t4);
   TESTINST5("bnez", 10, 0xffffffff, t4, t5);
   TESTINST5("bnez", 11, 0x256, t5, t6);
   TESTINST5("bnez", 12, 0x55, t6, t7);
   TESTINST5("bnez", 13, 0xfff, s0, s1);
   TESTINST5("bnez", 14, -1, v0, t9);
   TESTINST5("bnez", 15, -1, t9, t8);

#if (__mips_isa_rev < 6)
   printf("beql\n");
   TESTINST4l("beql", 0, 0, 1, v0, v1, a0);
   TESTINST4l("beql", 1, 1, 1, v1, a0, a1);
   TESTINST4l("beql", 2, 0xffffffff, 0xffffffff, a0, a1, a2);
   TESTINST4l("beql", 3, 0xffffffff, 0xfffffffe, a1, a2, a3);
   TESTINST4l("beql", 4, 0xfffffffe, 0xffffffff, a2, t0, t1);
   TESTINST4l("beql", 5, 0xffffffff, 0xffffffff, a3, t0, t1);
   TESTINST4l("beql", 6, 0x5, 0x5, t0, t1, t2);
   TESTINST4l("beql", 7, -3, -4, t1, t2, t3);
   TESTINST4l("beql", 8, 125, 125, t2, t3, t4);
   TESTINST4l("beql", 9, 0x80000000, 0x80000000, t3, t4, t5);
   TESTINST4l("beql", 10, 0xffffffff, 0x80000000, t4, t5, t6);
   TESTINST4l("beql", 11, 0x256, 0x256, t5, t6, t7);
   TESTINST4l("beql", 12, 0x55, 0x55, t6, t7, s0);
   TESTINST4l("beql", 13, 0xfff, 0xdd, s0, s1, s2);
   TESTINST4l("beql", 14, -1, 0x5, v0, t9, t8);
   TESTINST4l("beql", 15, -1, -1, t9, t8, a3);

   printf("BGEZALL\n");
   TESTINST5l("bgezall", 0, 0, v0, v1);
   TESTINST5l("bgezall", 1, 1, v1, a0);
   TESTINST5l("bgezall", 2, 0xffffffff, a0, a1);
   TESTINST5l("bgezall", 3, 0xffffffff, a1, a2);
   TESTINST5l("bgezall", 4, 0xfffffffe, a2, t0);
   TESTINST5l("bgezall", 5, 0xffffffff, a3, t0);
   TESTINST5l("bgezall", 6, 0x5, t0, t1);
   TESTINST5l("bgezall", 7, -3, t1, t2);
   TESTINST5l("bgezall", 8, 125, t2, t3);
   TESTINST5l("bgezall", 9, 0x80000000, t3, t4);
   TESTINST5l("bgezall", 10, 0xffffffff, t4, t5);
   TESTINST5l("bgezall", 11, 0x256, t5, t6);
   TESTINST5l("bgezall", 12, 0x55, t6, t7);
   TESTINST5l("bgezall", 13, 0xfff, s0, s1);
   TESTINST5l("bgezall", 14, -1, v0, t9);
   TESTINST5l("bgezall", 15, -1, t9, t8);

   printf("BGEZL\n");
   TESTINST5l("bgezl", 0, 0, v0, v1);
   TESTINST5l("bgezl", 1, 1, v1, a0);
   TESTINST5l("bgezl", 2, 0xffffffff, a0, a1);
   TESTINST5l("bgezl", 3, 0xffffffff, a1, a2);
   TESTINST5l("bgezl", 4, 0xfffffffe, a2, t0);
   TESTINST5l("bgezl", 5, 0xffffffff, a3, t0);
   TESTINST5l("bgezl", 6, 0x5, t0, t1);
   TESTINST5l("bgezl", 7, -3, t1, t2);
   TESTINST5l("bgezl", 8, 125, t2, t3);
   TESTINST5l("bgezl", 9, 0x80000000, t3, t4);
   TESTINST5l("bgezl", 10, 0xffffffff, t4, t5);
   TESTINST5l("bgezl", 11, 0x256, t5, t6);
   TESTINST5l("bgezl", 12, 0x55, t6, t7);
   TESTINST5l("bgezl", 13, 0xfff, s0, s1);
   TESTINST5l("bgezl", 14, -1, v0, t9);
   TESTINST5l("bgezl", 15, -1, t9, t8);

   printf("BGTZL\n");
   TESTINST5l("bgtzl", 0, 0, v0, v1);
   TESTINST5l("bgtzl", 1, 1, v1, a0);
   TESTINST5l("bgtzl", 2, 0xffffffff, a0, a1);
   TESTINST5l("bgtzl", 3, 0xffffffff, a1, a2);
   TESTINST5l("bgtzl", 4, 0xfffffffe, a2, t0);
   TESTINST5l("bgtzl", 5, 0xffffffff, a3, t0);
   TESTINST5l("bgtzl", 6, 0x5, t0, t1);
   TESTINST5l("bgtzl", 7, -3, t1, t2);
   TESTINST5l("bgtzl", 8, 125, t2, t3);
   TESTINST5l("bgtzl", 9, 0x80000000, t3, t4);
   TESTINST5l("bgtzl", 10, 0xffffffff, t4, t5);
   TESTINST5l("bgtzl", 11, 0x256, t5, t6);
   TESTINST5l("bgtzl", 12, 0x55, t6, t7);
   TESTINST5l("bgtzl", 13, 0xfff, s0, s1);
   TESTINST5l("bgtzl", 14, -1, v0, t9);
   TESTINST5l("bgtzl", 15, -1, t9, t8);

   printf("BLEZL\n");
   TESTINST5l("blezl", 0, 0, v0, v1);
   TESTINST5l("blezl", 1, 1, v1, a0);
   TESTINST5l("blezl", 2, 0xffffffff, a0, a1);
   TESTINST5l("blezl", 3, 0xffffffff, a1, a2);
   TESTINST5l("blezl", 4, 0xfffffffe, a2, t0);
   TESTINST5l("blezl", 5, 0xffffffff, a3, t0);
   TESTINST5l("blezl", 6, 0x5, t0, t1);
   TESTINST5l("blezl", 7, -3, t1, t2);
   TESTINST5l("blezl", 8, 125, t2, t3);
   TESTINST5l("blezl", 9, 0x80000000, t3, t4);
   TESTINST5l("blezl", 10, 0xffffffff, t4, t5);
   TESTINST5l("blezl", 11, 0x256, t5, t6);
   TESTINST5l("blezl", 12, 0x55, t6, t7);
   TESTINST5l("blezl", 13, 0xfff, s0, s1);
   TESTINST5l("blezl", 14, -1, v0, t9);
   TESTINST5l("blezl", 15, -1, t9, t8);

   printf("BGEZALL\n");
   TESTINST6l("bgezall", 0, 0, v0, v1);
   TESTINST6l("bgezall", 1, 1, v1, a0);
   TESTINST6l("bgezall", 2, 0xffffffff, a0, a1);
   TESTINST6l("bgezall", 3, 0xffffffff, a1, a2);
   TESTINST6l("bgezall", 4, 0xfffffffe, a2, t0);
   TESTINST6l("bgezall", 5, 0xffffffff, a3, t0);
   TESTINST6l("bgezall", 6, 0x5, t0, t1);
   TESTINST6l("bgezall", 7, -3, t1, t2);
   TESTINST6l("bgezall", 8, 125, t2, t3);
   TESTINST6l("bgezall", 9, 0x80000000, t3, t4);
   TESTINST6l("bgezall", 10, 0xffffffff, t4, t5);
   TESTINST6l("bgezall", 11, 0x256, t5, t6);
   TESTINST6l("bgezall", 12, 0x55, t6, t7);
   TESTINST6l("bgezall", 13, 0xfff, s0, s1);
   TESTINST6l("bgezall", 14, -1, v0, t9);
   TESTINST6l("bgezall", 15, -1, t9, t8);

   printf("BLTZL\n");
   TESTINST5l("bltzl", 0, 0, v0, v1);
   TESTINST5l("bltzl", 1, 1, v1, a0);
   TESTINST5l("bltzl", 2, 0xffffffff, a0, a1);
   TESTINST5l("bltzl", 3, 0xffffffff, a1, a2);
   TESTINST5l("bltzl", 4, 0xfffffffe, a2, t0);
   TESTINST5l("bltzl", 5, 0xffffffff, a3, t0);
   TESTINST5l("bltzl", 6, 0x5, t0, t1);
   TESTINST5l("bltzl", 7, -3, t1, t2);
   TESTINST5l("bltzl", 8, 125, t2, t3);
   TESTINST5l("bltzl", 9, 0x80000000, t3, t4);
   TESTINST5l("bltzl", 10, 0xffffffff, t4, t5);
   TESTINST5l("bltzl", 11, 0x256, t5, t6);
   TESTINST5l("bltzl", 12, 0x55, t6, t7);
   TESTINST5l("bltzl", 13, 0xfff, s0, s1);
   TESTINST5l("bltzl", 14, -1, v0, t9);
   TESTINST5l("bltzl", 15, -1, t9, t8);

   printf("BNEL\n");
   TESTINST4l("bnel", 0, 0, 1, v0, v1, a0);
   TESTINST4l("bnel", 1, 1, 1, v1, a0, a1);
   TESTINST4l("bnel", 2, 0xffffffff, 0xffffffff, a0, a1, a2);
   TESTINST4l("bnel", 3, 0xffffffff, 0xfffffffe, a1, a2, a3);
   TESTINST4l("bnel", 4, 0xfffffffe, 0xffffffff, a2, t0, t1);
   TESTINST4l("bnel", 5, 0xffffffff, 0xffffffff, a3, t0, t1);
   TESTINST4l("bnel", 6, 0x5, 0x5, t0, t1, t2);
   TESTINST4l("bnel", 7, -3, -4, t1, t2, t3);
   TESTINST4l("bnel", 8, 125, 125, t2, t3, t4);
   TESTINST4l("bnel", 9, 0x80000000, 0x80000000, t3, t4, t5);
   TESTINST4l("bnel", 10, 0xffffffff, 0x80000000, t4, t5, t6);
   TESTINST4l("bnel", 11, 0x256, 0x256, t5, t6, t7);
   TESTINST4l("bnel", 12, 0x55, 0x55, t6, t7, s0);
   TESTINST4l("bnel", 13, 0xfff, 0xdd, s0, s1, s2);
   TESTINST4l("bnel", 14, -1, 0x5, v0, t9, t8);
   TESTINST4l("bnel", 15, -1, -1, t9, t8, a3);
#endif

   printf("j, jal, jr\n");
   TESTINST3j(0, v0);
   TESTINST3j(1, v1);
   TESTINST3j(2, a0);
   TESTINST3j(3, a1);
   TESTINST3j(4, a2);
   TESTINST3j(5, a3);
   TESTINST3j(6, a0);
   TESTINST3j(7, t1);
   TESTINST3j(8, t2);
   TESTINST3j(9, t3);
   TESTINST3j(10, t4);
   TESTINST3j(11, t5);
   TESTINST3j(12, t6);
   TESTINST3j(13, t7);
   TESTINST3j(14, s0);
   TESTINST3j(15, s1);
   TESTINST3j(16, s2);
   TESTINST3j(17, s3);
   TESTINST3j(18, s4);
   TESTINST3j(19, s5);
   TESTINST3j(20, s6);
   TESTINST3j(21, s7);
   TESTINST3j(22, t8);
   TESTINST3j(23, t9);

   printf("j, jalr, jr\n");
   TESTINST3ja(0, v0);
   TESTINST3ja(1, v1);
   TESTINST3ja(2, a0);
   TESTINST3ja(3, a1);
   TESTINST3ja(4, a2);
   TESTINST3ja(5, a3);
   TESTINST3ja(6, a0);
   TESTINST3ja(7, a3);
   TESTINST3ja(8, t2);
   TESTINST3ja(9, t3);
   TESTINST3ja(10, t4);
   TESTINST3ja(11, t5);
   TESTINST3ja(12, t6);
   TESTINST3ja(13, t7);
   TESTINST3ja(14, s0);
   TESTINST3ja(15, s1);
   TESTINST3ja(16, s2);
   TESTINST3ja(17, s3);
   TESTINST3ja(18, s4);
   TESTINST3ja(19, s5);
   TESTINST3ja(20, s6);
   TESTINST3ja(21, s7);
   TESTINST3ja(22, t8);
   TESTINST3ja(23, t9);

   return 0;
}
