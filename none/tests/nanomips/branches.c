#include <stdio.h>

#define TESTINST1(RSval, RD) \
{ \
   unsigned int out = 0; \
   __asm__ volatile( \
      ".set push \n\t" \
      ".set noreorder \n\t" \
      "move $" #RD ", %1\n\t" \
      "bc end1"#RSval"\n\t" \
      "addiu $" #RD ", $" #RD", 5\n\t" \
      "end1"#RSval":\n\t" \
      "addiu $" #RD ", $" #RD", 1\n\t" \
      "move %0, $" #RD "\n\t" \
      ".set pop \n\t" \
      : "=&r" (out) \
      : "r" (RSval) \
      : #RD, "memory" \
        ); \
        printf("BC :: %d, RSval: %d\n", \
        out, RSval); \
}

#define TESTINST2(RSval, RD) \
{ \
   unsigned int out = 0; \
   __asm__ volatile( \
      ".set push \n\t" \
      ".set noreorder \n\t" \
      "move $" #RD ", %1\n\t" \
      "bc end2"#RSval"\n\t" \
      "addiu $" #RD ", $" #RD", 3\n\t" \
      "addiu $" #RD ", $" #RD", 5\n\t" \
      "end2"#RSval":\n\t" \
      "addiu $" #RD ", $" #RD", 3\n\t" \
      "move %0, $" #RD "\n\t" \
      ".set pop \n\t" \
      : "=&r" (out) \
      : "r" (RSval) \
      : #RD, "memory" \
        ); \
        printf("bc :: %d, RSval: %d\n", \
        out, RSval); \
}

#define TESTINST3(RSval, RD) \
{ \
   unsigned int out = 0; \
   __asm__ volatile( \
      ".set push \n\t" \
      ".set noreorder \n\t" \
      "move $" #RD ", %1\n\t" \
      "balc end3"#RSval"\n\t" \
      "addiu $" #RD ", $" #RD", 5\n\t" \
      "bc r_end3"#RSval"\n\t" \
      "end3"#RSval":\n\t" \
      "addiu $" #RD ", $" #RD", 1\n\t" \
      "jrc $ra\n\t"  \
      "r_end3"#RSval":\n\t" \
      "move %0, $" #RD "\n\t" \
      ".set pop \n\t" \
      : "=&r" (out) \
      : "r" (RSval) \
      : #RD, "memory" \
        ); \
        printf("bc balc jrc :: %d, RSval: %d\n", \
        out, RSval); \
}


#define TESTINST3j(RSval, RD) \
{ \
   unsigned int out = 0; \
   __asm__ volatile( \
      ".set push \n\t" \
      ".set noreorder \n\t" \
      "move $" #RD ", %1\n\t" \
      "la $t0, end3j"#RSval"\n\t" \
      "jalrc $t1, $t0\n\t" \
      "addiu $" #RD ", $" #RD", 5\n\t" \
      "la $t0, r_end3j"#RSval"\n\t" \
      "jrc $t0\n\t" \
      "end3j"#RSval":\n\t" \
      "addiu $" #RD ", $" #RD", 1\n\t" \
      "jrc $t1\n\t"  \
      "r_end3j"#RSval":\n\t" \
      "move %0, $" #RD "\n\t" \
      ".set pop \n\t" \
      : "=&r" (out) \
      : "r" (RSval) \
      : #RD, "t0", "t1", "memory" \
        ); \
        printf("jalrc jrc :: %d, RSval: %d\n", \
        out, RSval); \
}


#define TESTINST3b(RSval, RS, RT) \
{ \
   unsigned int out = 0; \
   __asm__ volatile( \
      ".set push \n\t" \
      ".set noreorder \n\t" \
      "move $t0, %1\n\t" \
      "li $" #RS", 5\n\t" \
      "balrsc $" #RT ", $" #RS "\n\t" \
      "addiu[32] $t0, $t0, 5\n\t" \
      "la $t1, end3b"#RSval"\n\t" \
      "jrc $t1 \n\t" \
      "jrc $" #RT " \n\t"\
      "end3b"#RSval":\n\t" \
      "addiu $t0, $t0, 1\n\t" \
      "move %0, $t0\n\t" \
      ".set pop \n\t" \
      : "=&r" (out) \
      : "r" (RSval) \
      : #RS, #RT, "t0", "memory" \
        ); \
        printf("balrsc :: %d, RSval: %d\n", \
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

#define TESTINST5i(instruction, RDval, RSval, immVal, RD, RS) \
{ \
   unsigned int out = 0; \
   __asm__ volatile( \
      ".set push \n\t" \
      ".set noreorder \n\t" \
      "move $" #RS ", %1\n\t" \
      "move $" #RD ", %2\n\t" \
      instruction" $" #RS "," #immVal ", end"instruction#RDval"\n\t" \
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

#define TESTINST6(RSval, RD) \
{ \
   unsigned int out = 0; \
   __asm__ volatile( \
      ".set push \n\t" \
      ".set noreorder \n\t" \
      "move $" #RD ", %1\n\t" \
      "move $t0, $" #RD "\n\t" \
      "addiu $t1, $zero, -7\n\t" \
      "end60"#RSval":\n\t" \
      "beqzc $t0, end6"#RSval"\n\t" \
      "addiu $t0, $t0, -1 \n\t" \
      "addiu[32] $" #RD ", $" #RD", 5\n\t" \
      "brsc $t1\n\t" \
      "bc end60"#RSval" \n\t"\
      "end6"#RSval":\n\t" \
      "addiu $" #RD ", $" #RD", 1\n\t" \
      "move %0, $" #RD "\n\t" \
      ".set pop \n\t" \
      : "=&r" (out) \
      : "r" (RSval) \
      : #RD, "t0", "t1", "memory" \
        ); \
        printf("brsc :: %d, RSval: %d\n", \
        out, RSval); \
}

int main()
{
   printf("---bc---\n");
   TESTINST1(0, t4);
   TESTINST1(1, t5);
   TESTINST1(2, a0);
   TESTINST1(3, a1);
   TESTINST1(4, a2);
   TESTINST1(5, a3);
   TESTINST1(6, a4);
   TESTINST1(7, a5);
   TESTINST1(8, a6);
   TESTINST1(9, a7);
   TESTINST1(10, t0);
   TESTINST1(11, t1);
   TESTINST1(12, t2);
   TESTINST1(13, t3);
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

   printf("---bc---\n");
   TESTINST2(0, t4);
   TESTINST2(1, t5);
   TESTINST2(2, a0);
   TESTINST2(3, a1);
   TESTINST2(4, a2);
   TESTINST2(5, a3);
   TESTINST2(6, a4);
   TESTINST2(7, a5);
   TESTINST2(8, a6);
   TESTINST2(9, a7);
   TESTINST2(10, t0);
   TESTINST2(11, t1);
   TESTINST2(12, t2);
   TESTINST2(13, t3);
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

   printf("---bc, balc, jrc---\n");
   TESTINST3(0, t4);
   TESTINST3(1, t5);
   TESTINST3(2, a0);
   TESTINST3(3, a1);
   TESTINST3(4, a2);
   TESTINST3(5, a3);
   TESTINST3(6, a4);
   TESTINST3(7, a5);
   TESTINST3(8, a6);
   TESTINST3(9, a7);
   TESTINST3(10, t0);
   TESTINST3(11, t1);
   TESTINST3(12, t2);
   TESTINST3(13, t3);
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

   printf("---balrsc---\n");
   TESTINST3b(0, t4, t5);
   TESTINST3b(1, t5, a0);
   TESTINST3b(2, a0, a1);
   TESTINST3b(3, a1, a2);
   TESTINST3b(4, a2, a3);
   TESTINST3b(5, a3, a4);
   TESTINST3b(6, a4, a5);
   TESTINST3b(7, a5, a6);
   TESTINST3b(8, a6, a7);
   TESTINST3b(9, a7, t2);
   TESTINST3b(10, t2, t3);
   TESTINST3b(11, t3, s0);
   TESTINST3b(12, s0, s1);
   TESTINST3b(13, s1, s2);
   TESTINST3b(14, s2, s3);
   TESTINST3b(15, s3, s4);
   TESTINST3b(16, s4, s5);
   TESTINST3b(17, s5, s6);
   TESTINST3b(18, s6, s7);
   TESTINST3b(19, s7, t8);
   TESTINST3b(20, t8, t9);
   TESTINST3b(21, t9, t4);

   printf("---bbeqzc---\n");
   TESTINST5i("bbeqzc", 0, 0, 0, t4, t5);
   TESTINST5i("bbeqzc", 1, 1, 1, t5, a0);
   TESTINST5i("bbeqzc", 2, 0xFFFF, 5, a0, a1);
   TESTINST5i("bbeqzc", 3, 0xFFCF, 5, a1, a2);
   TESTINST5i("bbeqzc", 4, 0xFFFF0000, 16 , a2, a3);
   TESTINST5i("bbeqzc", 5, 0xFFFC0000, 16, a3, t4);
   TESTINST5i("bbeqzc", 6, 0x0, 30, a4, a5);
   TESTINST5i("bbeqzc", 7, 0xF0000000, 30, a6, a7);
   TESTINST5i("bbeqzc", 8, 125, 8, t0, t1);
   TESTINST5i("bbeqzc", 9, 125, 9, t1, t2);
   TESTINST5i("bbeqzc", 10, 0xFFFFFFFF, 0x10, t2, t3);
   TESTINST5i("bbeqzc", 11, 0xFFFFFFFF, 0x15, t3, t4);
   TESTINST5i("bbeqzc", 12, 0x55, 2, t4, t5);
   TESTINST5i("bbeqzc", 13, 0x10, 3, t5, s0);
   TESTINST5i("bbeqzc", 14, -1, 0, s0, s1);
   TESTINST5i("bbeqzc", 15, -1, 5, s1, s2);

   printf("---bbnezc---\n");
   TESTINST5i("bbnezc", 0, 0, 0, t4, t5);
   TESTINST5i("bbnezc", 1, 1, 1, t5, a0);
   TESTINST5i("bbnezc", 2, 0xFFFF, 5, a0, a1);
   TESTINST5i("bbnezc", 3, 0xFFCF, 5, a1, a2);
   TESTINST5i("bbnezc", 4, 0xFFFF0000, 16 , a2, a3);
   TESTINST5i("bbnezc", 5, 0xFFFC0000, 16, a3, t4);
   TESTINST5i("bbnezc", 6, 0x0, 30, a4, a5);
   TESTINST5i("bbnezc", 7, 0xF0000000, 30, a6, a7);
   TESTINST5i("bbnezc", 8, 125, 8, t0, t1);
   TESTINST5i("bbnezc", 9, 125, 9, t1, t2);
   TESTINST5i("bbnezc", 10, 0xFFFFFFFF, 0x10, t2, t3);
   TESTINST5i("bbnezc", 11, 0xFFFFFFFF, 0x15, t3, t4);
   TESTINST5i("bbnezc", 12, 0x55, 2, t4, t5);
   TESTINST5i("bbnezc", 13, 0x10, 3, t5, s0);
   TESTINST5i("bbnezc", 14, -1, 0, s0, s1);
   TESTINST5i("bbnezc", 15, -1, 5, s1, s2);

   printf("---jalrc jrc---\n");
   TESTINST3j(0, t4);
   TESTINST3j(1, t5);
   TESTINST3j(2, a0);
   TESTINST3j(3, a1);
   TESTINST3j(4, a2);
   TESTINST3j(5, a3);
   TESTINST3j(6, a4);
   TESTINST3j(7, a5);
   TESTINST3j(8, a6);
   TESTINST3j(9, a7);
   TESTINST3j(10, a0);
   TESTINST3j(11, a1);
   TESTINST3j(12, t2);
   TESTINST3j(13, t3);
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

   printf("---beqc---\n");
   TESTINST4("beqc", 0, 0, 1, t4, t5, a0);
   TESTINST4("beqc", 1, 1, 1, t5, a0, a1);
   TESTINST4("beqc", 2, 0xffffffff, 0xffffffff, a0, a1, a2);
   TESTINST4("beqc", 3, 0xffffffff, 0xfffffffe, a1, a2, a3);
   TESTINST4("beqc", 4, 0xfffffffe, 0xffffffff, a2, a3, a4);
   TESTINST4("beqc", 5, 0xffffffff, 0xffffffff, a3, a4, a5);
   TESTINST4("beqc", 6, 0x5, 0x5, a4, a5, a6);
   TESTINST4("beqc", 7, -3, -4, a5, a6, a7);
   TESTINST4("beqc", 8, 125, 125, a6, a7, t0);
   TESTINST4("beqc", 9, 0x80000000, 0x80000000, t0, t1, t2);
   TESTINST4("beqc", 10, 0xffffffff, 0x80000000, t1, t2, t3);
   TESTINST4("beqc", 11, 0x256, 0x256, t4, t5, t8);
   TESTINST4("beqc", 12, 0x55, 0x55, t5, t8, t9);
   TESTINST4("beqc", 13, 0xfff, 0xdd, t8, t9, s0);
   TESTINST4("beqc", 14, -1, 0x5, t9, s0, s1);
   TESTINST4("beqc", 15, -1, -1, s0, s1, s2);

   printf("---beqic---\n");
   TESTINST5i("beqic", 0, 0, 0, t4, t5);
   TESTINST5i("beqic", 1, 1, 1, t5, a0);
   TESTINST5i("beqic", 2, 0x7f, 0x7f, a0, a1);
   TESTINST5i("beqic", 3, 0x7f, 0x7e, a1, a2);
   TESTINST5i("beqic", 4, 0x7e, 0x7f, a2, a3);
   TESTINST5i("beqic", 5, 0x70, 0x71, a3, t4);
   TESTINST5i("beqic", 6, 0x5,0x4, a4, a5);
   TESTINST5i("beqic", 7, -3, 0x7E, a6, a7);
   TESTINST5i("beqic", 8, 125, 124, t0, t1);
   TESTINST5i("beqic", 9, 0x0, 0x1, t1, t2);
   TESTINST5i("beqic", 10, 0xf, 0x10, t2, t3);
   TESTINST5i("beqic", 11, 0x26, 0x25, t3, t4);
   TESTINST5i("beqic", 12, 0x55, 0x54, t4, t5);
   TESTINST5i("beqic", 13, 0x10, 0xf, t5, s0);
   TESTINST5i("beqic", 14, -1, 0, s0, s1);
   TESTINST5i("beqic", 15, -1, 0x7E, s1, s2);

   printf("---beqzc---\n");
   TESTINST5("beqzc", 0, 0, t4, t5);
   TESTINST5("beqzc", 1, 1, t5, a0);
   TESTINST5("beqzc", 2, 0xffffffff, a0, a1);
   TESTINST5("beqzc", 3, 0xffffffff, a1, a2);
   TESTINST5("beqzc", 4, 0xfffffffe, a2, a3);
   TESTINST5("beqzc", 5, 0xffffffff, a3, t4);
   TESTINST5("beqzc", 6, 0x5, a4, a5);
   TESTINST5("beqzc", 7, -3, a6, a7);
   TESTINST5("beqzc", 8, 125, t0, t1);
   TESTINST5("beqzc", 9, 0x80000000, t1, t2);
   TESTINST5("beqzc", 10, 0xffffffff, t2, t3);
   TESTINST5("beqzc", 11, 0x256, t3, t4);
   TESTINST5("beqzc", 12, 0x55, t4, t5);
   TESTINST5("beqzc", 13, 0xfff, t5, s0);
   TESTINST5("beqzc", 14, -1, s0, s1);
   TESTINST5("beqzc", 15, -1, s1, s2);

   printf("---bgec---\n");
   TESTINST4("bgec", 0, 0, 1, t4, t5, a0);
   TESTINST4("bgec", 1, 1, 1, t5, a0, a1);
   TESTINST4("bgec", 2, 0xffffffff, 0xffffffff, a0, a1, a2);
   TESTINST4("bgec", 3, 0xffffffff, 0xfffffffe, a1, a2, a3);
   TESTINST4("bgec", 4, 0xfffffffe, 0xffffffff, a2, t0, t1);
   TESTINST4("bgec", 5, 0xffffffff, 0xffffffff, a3, t0, t1);
   TESTINST4("bgec", 6, 0x5, 0x5, t0, t1, t2);
   TESTINST4("bgec", 7, -3, -4, t1, t2, t3);
   TESTINST4("bgec", 8, 125, 125, t2, t3, t4);
   TESTINST4("bgec", 9, 0x80000000, 0x80000000, t3, t4, t5);
   TESTINST4("bgec", 10, 0xffffffff, 0x80000000, t4, t5, a5);
   TESTINST4("bgec", 11, 0x256, 0x256, t5, a5, a6);
   TESTINST4("bgec", 12, 0x55, 0x55, a5, a6, s0);
   TESTINST4("bgec", 13, 0xfff, 0xdd, s0, s1, s2);
   TESTINST4("bgec", 14, -1, 0x5, t4, t9, t8);
   TESTINST4("bgec", 15, -1, -1, t9, t8, a3);

   printf("---bgeic---\n");
   TESTINST5i("bgeic", 0, 0, 0, t4, t5);
   TESTINST5i("bgeic", 1, 1, 1, t5, a0);
   TESTINST5i("bgeic", 2, 0x7f, 0x7f, a0, a1);
   TESTINST5i("bgeic", 3, 0x7f, 0x7e, a1, a2);
   TESTINST5i("bgeic", 4, 0x7e, 0x7f, a2, a3);
   TESTINST5i("bgeic", 5, 0x70, 0x71, a3, t4);
   TESTINST5i("bgeic", 6, 0x5,0x4, a4, a5);
   TESTINST5i("bgeic", 7, -3, 0x7E, a6, a7);
   TESTINST5i("bgeic", 8, 125, 124, t0, t1);
   TESTINST5i("bgeic", 9, 0x0, 0x1, t1, t2);
   TESTINST5i("bgeic", 10, 0xf, 0x10, t2, t3);
   TESTINST5i("bgeic", 11, 0x26, 0x25, t3, t4);
   TESTINST5i("bgeic", 12, 0x55, 0x54, t4, t5);
   TESTINST5i("bgeic", 13, 0x10, 0xf, t5, s0);
   TESTINST5i("bgeic", 14, -1, 0, s0, s1);
   TESTINST5i("bgeic", 15, -1, 0x7E, s1, s2);

   printf("---bgeiuc---\n");
   TESTINST5i("bgeiuc", 0, 0, 0, t4, t5);
   TESTINST5i("bgeiuc", 1, 1, 1, t5, a0);
   TESTINST5i("bgeiuc", 2, 0x7f, 0x7f, a0, a1);
   TESTINST5i("bgeiuc", 3, 0x7f, 0x7e, a1, a2);
   TESTINST5i("bgeiuc", 4, 0x7e, 0x7f, a2, a3);
   TESTINST5i("bgeiuc", 5, 0x70, 0x71, a3, t4);
   TESTINST5i("bgeiuc", 6, 0x5,0x4, a4, a5);
   TESTINST5i("bgeiuc", 7, -3, 0x7E, a6, a7);
   TESTINST5i("bgeiuc", 8, 125, 124, t0, t1);
   TESTINST5i("bgeiuc", 9, 0x0, 0x1, t1, t2);
   TESTINST5i("bgeiuc", 10, 0xf, 0x10, t2, t3);
   TESTINST5i("bgeiuc", 11, 0x26, 0x25, t3, t4);
   TESTINST5i("bgeiuc", 12, 0x55, 0x54, t4, t5);
   TESTINST5i("bgeiuc", 13, 0x10, 0xf, t5, s0);
   TESTINST5i("bgeiuc", 14, -1, 0, s0, s1);
   TESTINST5i("bgeiuc", 15, -1, 0x7E, s1, s2);

   printf("---bgeuc---\n");
   TESTINST4("bgeuc", 0, 0, 1, t4, t5, a0);
   TESTINST4("bgeuc", 1, 1, 1, t5, a0, a1);
   TESTINST4("bgeuc", 2, 0xffffffff, 0xffffffff, a0, a1, a2);
   TESTINST4("bgeuc", 3, 0xffffffff, 0xfffffffe, a1, a2, a3);
   TESTINST4("bgeuc", 4, 0xfffffffe, 0xffffffff, a2, t0, t1);
   TESTINST4("bgeuc", 5, 0xffffffff, 0xffffffff, a3, t0, t1);
   TESTINST4("bgeuc", 6, 0x5, 0x5, t0, t1, t2);
   TESTINST4("bgeuc", 7, -3, -4, t1, t2, t3);
   TESTINST4("bgeuc", 8, 125, 125, t2, t3, t4);
   TESTINST4("bgeuc", 9, 0x80000000, 0x80000000, t3, t4, t5);
   TESTINST4("bgeuc", 10, 0xffffffff, 0x80000000, t4, t5, a5);
   TESTINST4("bgeuc", 11, 0x256, 0x256, t5, a5, a6);
   TESTINST4("bgeuc", 12, 0x55, 0x55, a5, a6, s0);
   TESTINST4("bgeuc", 13, 0xfff, 0xdd, s0, s1, s2);
   TESTINST4("bgeuc", 14, -1, 0x5, t4, t9, t8);
   TESTINST4("bgeuc", 15, -1, -1, t9, t8, a3);

   printf("---bltc---\n");
   TESTINST4("bltc", 0, 0, 1, t4, t5, a0);
   TESTINST4("bltc", 1, 1, 1, t5, a0, a1);
   TESTINST4("bltc", 2, 0xffffffff, 0xffffffff, a0, a1, a2);
   TESTINST4("bltc", 3, 0xffffffff, 0xfffffffe, a1, a2, a3);
   TESTINST4("bltc", 4, 0xfffffffe, 0xffffffff, a2, t0, t1);
   TESTINST4("bltc", 5, 0xffffffff, 0xffffffff, a3, t0, t1);
   TESTINST4("bltc", 6, 0x5, 0x5, t0, t1, t2);
   TESTINST4("bltc", 7, -3, -4, t1, t2, t3);
   TESTINST4("bltc", 8, 125, 125, t2, t3, t4);
   TESTINST4("bltc", 9, 0x80000000, 0x80000000, t3, t4, t5);
   TESTINST4("bltc", 10, 0xffffffff, 0x80000000, t4, t5, a5);
   TESTINST4("bltc", 11, 0x256, 0x256, t5, a5, a6);
   TESTINST4("bltc", 12, 0x55, 0x55, a5, a6, s0);
   TESTINST4("bltc", 13, 0xfff, 0xdd, s0, s1, s2);
   TESTINST4("bltc", 14, -1, 0x5, t4, t9, t8);
   TESTINST4("bltc", 15, -1, -1, t9, t8, a3);

   printf("---bltic---\n");
   TESTINST5i("bltic", 0, 0, 0, t4, t5);
   TESTINST5i("bltic", 1, 1, 1, t5, a0);
   TESTINST5i("bltic", 2, 0x7f, 0x7f, a0, a1);
   TESTINST5i("bltic", 3, 0x7f, 0x7e, a1, a2);
   TESTINST5i("bltic", 4, 0x7e, 0x7f, a2, a3);
   TESTINST5i("bltic", 5, 0x70, 0x71, a3, t4);
   TESTINST5i("bltic", 6, 0x5,0x4, a4, a5);
   TESTINST5i("bltic", 7, -3, 0x7E, a6, a7);
   TESTINST5i("bltic", 8, 125, 124, t0, t1);
   TESTINST5i("bltic", 9, 0x0, 0x1, t1, t2);
   TESTINST5i("bltic", 10, 0xf, 0x10, t2, t3);
   TESTINST5i("bltic", 11, 0x26, 0x25, t3, t4);
   TESTINST5i("bltic", 12, 0x55, 0x54, t4, t5);
   TESTINST5i("bltic", 13, 0x10, 0xf, t5, s0);
   TESTINST5i("bltic", 14, -1, 0, s0, s1);
   TESTINST5i("bltic", 15, -1, 0x7E, s1, s2);

   printf("---bltiuc---\n");
   TESTINST5i("bltiuc", 0, 0, 0, t4, t5);
   TESTINST5i("bltiuc", 1, 1, 1, t5, a0);
   TESTINST5i("bltiuc", 2, 0x7f, 0x7f, a0, a1);
   TESTINST5i("bltiuc", 3, 0x7f, 0x7e, a1, a2);
   TESTINST5i("bltiuc", 4, 0x7e, 0x7f, a2, a3);
   TESTINST5i("bltiuc", 5, 0x70, 0x71, a3, t4);
   TESTINST5i("bltiuc", 6, 0x5,0x4, a4, a5);
   TESTINST5i("bltiuc", 7, -3, 0x7E, a6, a7);
   TESTINST5i("bltiuc", 8, 125, 124, t0, t1);
   TESTINST5i("bltiuc", 9, 0x0, 0x1, t1, t2);
   TESTINST5i("bltiuc", 10, 0xf, 0x10, t2, t3);
   TESTINST5i("bltiuc", 11, 0x26, 0x25, t3, t4);
   TESTINST5i("bltiuc", 12, 0x55, 0x54, t4, t5);
   TESTINST5i("bltiuc", 13, 0x10, 0xf, t5, s0);
   TESTINST5i("bltiuc", 14, -1, 0, s0, s1);
   TESTINST5i("bltiuc", 15, -1, 0x7E, s1, s2);

   printf("---bltuc---\n");
   TESTINST4("bltuc", 0, 0, 1, t4, t5, a0);
   TESTINST4("bltuc", 1, 1, 1, t5, a0, a1);
   TESTINST4("bltuc", 2, 0xffffffff, 0xffffffff, a0, a1, a2);
   TESTINST4("bltuc", 3, 0xffffffff, 0xfffffffe, a1, a2, a3);
   TESTINST4("bltuc", 4, 0xfffffffe, 0xffffffff, a2, t0, t1);
   TESTINST4("bltuc", 5, 0xffffffff, 0xffffffff, a3, t0, t1);
   TESTINST4("bltuc", 6, 0x5, 0x5, t0, t1, t2);
   TESTINST4("bltuc", 7, -3, -4, t1, t2, t3);
   TESTINST4("bltuc", 8, 125, 125, t2, t3, t4);
   TESTINST4("bltuc", 9, 0x80000000, 0x80000000, t3, t4, t5);
   TESTINST4("bltuc", 10, 0xffffffff, 0x80000000, t4, t5, a5);
   TESTINST4("bltuc", 11, 0x256, 0x256, t5, a5, a6);
   TESTINST4("bltuc", 12, 0x55, 0x55, a5, a6, s0);
   TESTINST4("bltuc", 13, 0xfff, 0xdd, s0, s1, s2);
   TESTINST4("bltuc", 14, -1, 0x5, t4, t9, t8);
   TESTINST4("bltuc", 15, -1, -1, t9, t8, a3);

   printf("---bnec---\n");
   TESTINST4("bnec", 0, 0, 1, t4, t5, a0);
   TESTINST4("bnec", 1, 1, 1, t5, a0, a1);
   TESTINST4("bnec", 2, 0xffffffff, 0xffffffff, a0, a1, a2);
   TESTINST4("bnec", 3, 0xffffffff, 0xfffffffe, a1, a2, a3);
   TESTINST4("bnec", 4, 0xfffffffe, 0xffffffff, a2, a3, a4);
   TESTINST4("bnec", 5, 0xffffffff, 0xffffffff, a3, a4, a5);
   TESTINST4("bnec", 6, 0x5, 0x5, a4, a5, a6);
   TESTINST4("bnec", 7, -3, -4, a5, a6, a7);
   TESTINST4("bnec", 8, 125, 125, a6, a7, t0);
   TESTINST4("bnec", 9, 0x80000000, 0x80000000, t0, t1, t2);
   TESTINST4("bnec", 10, 0xffffffff, 0x80000000, t1, t2, t3);
   TESTINST4("bnec", 11, 0x256, 0x256, t4, t5, t8);
   TESTINST4("bnec", 12, 0x55, 0x55, t5, t8, t9);
   TESTINST4("bnec", 13, 0xfff, 0xdd, t8, t9, s0);
   TESTINST4("bnec", 14, -1, 0x5, t9, s0, s1);
   TESTINST4("bnec", 15, -1, -1, s0, s1, s2);

   printf("---bneic---\n");
   TESTINST5i("bneic", 0, 0, 0, t4, t5);
   TESTINST5i("bneic", 1, 1, 1, t5, a0);
   TESTINST5i("bneic", 2, 0x7f, 0x7f, a0, a1);
   TESTINST5i("bneic", 3, 0x7f, 0x7e, a1, a2);
   TESTINST5i("bneic", 4, 0x7e, 0x7f, a2, a3);
   TESTINST5i("bneic", 5, 0x70, 0x71, a3, t4);
   TESTINST5i("bneic", 6, 0x5,0x4, a4, a5);
   TESTINST5i("bneic", 7, -3, 0x7E, a6, a7);
   TESTINST5i("bneic", 8, 125, 124, t0, t1);
   TESTINST5i("bneic", 9, 0x0, 0x1, t1, t2);
   TESTINST5i("bneic", 10, 0xf, 0x10, t2, t3);
   TESTINST5i("bneic", 11, 0x26, 0x25, t3, t4);
   TESTINST5i("bneic", 12, 0x55, 0x54, t4, t5);
   TESTINST5i("bneic", 13, 0x10, 0xf, t5, s0);
   TESTINST5i("bneic", 14, -1, 0, s0, s1);
   TESTINST5i("bneic", 15, -1, 0x7E, s1, s2);

   printf("---bnezc---\n");
   TESTINST5("bnezc", 0, 0, t4, t5);
   TESTINST5("bnezc", 1, 1, t5, a0);
   TESTINST5("bnezc", 2, 0xffffffff, a0, a1);
   TESTINST5("bnezc", 3, 0xffffffff, a1, a2);
   TESTINST5("bnezc", 4, 0xfffffffe, a2, a3);
   TESTINST5("bnezc", 5, 0xffffffff, a3, t4);
   TESTINST5("bnezc", 6, 0x5, a4, a5);
   TESTINST5("bnezc", 7, -3, a6, a7);
   TESTINST5("bnezc", 8, 125, t0, t1);
   TESTINST5("bnezc", 9, 0x80000000, t1, t2);
   TESTINST5("bnezc", 10, 0xffffffff, t2, t3);
   TESTINST5("bnezc", 11, 0x256, t3, t4);
   TESTINST5("bnezc", 12, 0x55, t4, t5);
   TESTINST5("bnezc", 13, 0xfff, t5, s0);
   TESTINST5("bnezc", 14, -1, s0, s1);
   TESTINST5("bnezc", 15, -1, s1, s2);

   printf("---brsc---\n");
   TESTINST6(0, a0);
   TESTINST6(1, a1);
   TESTINST6(2, a2);
   TESTINST6(3, a3);
   TESTINST6(4, a4);
   TESTINST6(5, a5);
   TESTINST6(6, a6);
   TESTINST6(7, a7);
   TESTINST6(8, t4);
   TESTINST6(9, t5);
   TESTINST6(10, s0);
   TESTINST6(11, s1);
   TESTINST6(12, s2);
   TESTINST6(13, s3);
   TESTINST6(14, t2);
   TESTINST6(15, t3);

   printf("---bgezc---\n");
   TESTINST5("bgezc", 0, 0, t4, t5);
   TESTINST5("bgezc", 1, 1, t5, a0);
   TESTINST5("bgezc", 2, 0xffffffff, a0, a1);
   TESTINST5("bgezc", 3, 0xffffffff, a1, a2);
   TESTINST5("bgezc", 4, 0xfffffffe, a2, a3);
   TESTINST5("bgezc", 5, 0xffffffff, a3, t4);
   TESTINST5("bgezc", 6, 0x5, a4, a5);
   TESTINST5("bgezc", 7, -3, a6, a7);
   TESTINST5("bgezc", 8, 125, t0, t1);
   TESTINST5("bgezc", 9, 0x80000000, t1, t2);
   TESTINST5("bgezc", 10, 0xffffffff, t2, t3);
   TESTINST5("bgezc", 11, 0x256, t3, t4);
   TESTINST5("bgezc", 12, 0x55, t4, t5);
   TESTINST5("bgezc", 13, 0xfff, t5, s0);
   TESTINST5("bgezc", 14, -1, s0, s1);
   TESTINST5("bgezc", 15, -1, s1, s2);

   printf("---bgtzc---\n");
   TESTINST5("bgtzc", 0, 0, t4, t5);
   TESTINST5("bgtzc", 1, 1, t5, a0);
   TESTINST5("bgtzc", 2, 0xffffffff, a0, a1);
   TESTINST5("bgtzc", 3, 0xffffffff, a1, a2);
   TESTINST5("bgtzc", 4, 0xfffffffe, a2, a3);
   TESTINST5("bgtzc", 5, 0xffffffff, a3, t4);
   TESTINST5("bgtzc", 6, 0x5, a4, a5);
   TESTINST5("bgtzc", 7, -3, a6, a7);
   TESTINST5("bgtzc", 8, 125, t0, t1);
   TESTINST5("bgtzc", 9, 0x80000000, t1, t2);
   TESTINST5("bgtzc", 10, 0xffffffff, t2, t3);
   TESTINST5("bgtzc", 11, 0x256, t3, t4);
   TESTINST5("bgtzc", 12, 0x55, t4, t5);
   TESTINST5("bgtzc", 13, 0xfff, t5, s0);
   TESTINST5("bgtzc", 14, -1, s0, s1);
   TESTINST5("bgtzc", 15, -1, s1, s2);

   printf("---blezc---\n");
   TESTINST5("blezc", 0, 0, t4, t5);
   TESTINST5("blezc", 1, 1, t5, a0);
   TESTINST5("blezc", 2, 0xffffffff, a0, a1);
   TESTINST5("blezc", 3, 0xffffffff, a1, a2);
   TESTINST5("blezc", 4, 0xfffffffe, a2, a3);
   TESTINST5("blezc", 5, 0xffffffff, a3, t4);
   TESTINST5("blezc", 6, 0x5, a4, a5);
   TESTINST5("blezc", 7, -3, a6, a7);
   TESTINST5("blezc", 8, 125, t0, t1);
   TESTINST5("blezc", 9, 0x80000000, t1, t2);
   TESTINST5("blezc", 10, 0xffffffff, t2, t3);
   TESTINST5("blezc", 11, 0x256, t3, t4);
   TESTINST5("blezc", 12, 0x55, t4, t5);
   TESTINST5("blezc", 13, 0xfff, t5, s0);
   TESTINST5("blezc", 14, -1, s0, s1);
   TESTINST5("blezc", 15, -1, s1, s2);

   printf("---bltzc---\n");
   TESTINST5("bltzc", 0, 0, t4, t5);
   TESTINST5("bltzc", 1, 1, t5, a0);
   TESTINST5("bltzc", 2, 0xffffffff, a0, a1);
   TESTINST5("bltzc", 3, 0xffffffff, a1, a2);
   TESTINST5("bltzc", 4, 0xfffffffe, a2, a3);
   TESTINST5("bltzc", 5, 0xffffffff, a3, t4);
   TESTINST5("bltzc", 6, 0x5, a4, a5);
   TESTINST5("bltzc", 7, -3, a6, a7);
   TESTINST5("bltzc", 8, 125, t0, t1);
   TESTINST5("bltzc", 9, 0x80000000, t1, t2);
   TESTINST5("bltzc", 10, 0xffffffff, t2, t3);
   TESTINST5("bltzc", 11, 0x256, t3, t4);
   TESTINST5("bltzc", 12, 0x55, t4, t5);
   TESTINST5("bltzc", 13, 0xfff, t5, s0);
   TESTINST5("bltzc", 14, -1, s0, s1);
   TESTINST5("bltzc", 15, -1, s1, s2);

   return 0;
}
