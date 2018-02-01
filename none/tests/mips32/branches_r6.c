#include <stdio.h>


#if (__mips == 64)
#define LOAD_ADDRESS "dla"
#else
#define LOAD_ADDRESS "la"
#endif

#define TESTINST1(label, instruction,TID, RD) \
{ \
   unsigned int out = 0; \
   __asm__ volatile( \
      ".set push \n\t" \
      ".set noreorder \n\t" \
      "bal end31"#TID"\n\t" \
      "move $" #RD ", $0 \n\t" \
      "end12"#TID":\n\t" \
      "addiu $" #RD ", $" #RD", -1\n\t" \
      "end13"#TID":\n\t" \
      "addiu $" #RD ", $" #RD", -1\n\t" \
      "end14"#TID":\n\t" \
      "addiu $" #RD ", $" #RD", -1\n\t" \
      "end15"#TID":\n\t" \
      "addiu $" #RD ", $" #RD", -1\n\t" \
      "end16"#TID":\n\t" \
      "addiu $" #RD ", $" #RD", -1\n\t" \
      "bal r_end"#TID "\n\t" \
      "nop \n\t" \
      "end31"#TID":\n\t" \
      instruction " " label #TID "\n\t" \
      "end21"#TID":\n\t" \
      "addiu $" #RD ", $" #RD", 1\n\t" \
      "end22"#TID":\n\t" \
      "addiu $" #RD ", $" #RD", 1\n\t" \
      "end23"#TID":\n\t" \
      "addiu $" #RD ", $" #RD", 1\n\t" \
      "end24"#TID":\n\t" \
      "addiu $" #RD ", $" #RD", 1\n\t" \
      "end25"#TID":\n\t" \
      "addiu $" #RD ", $" #RD", 1\n\t" \
      "r_end"#TID":\n\t" \
      "move %0, $" #RD "\n\t" \
      ".set pop \n\t" \
      : "=&r" (out) \
      :\
      : #RD, "cc", "memory" \
        ); \
        printf(instruction" :: %x, RSval: %x\n", \
        out, TID); \
}

#define TESTINST2(instruction, RDval, RSval, RD, RS) \
{ \
   unsigned int out = 0; \
   __asm__ volatile( \
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
      : "=&r" (out) \
      : "r" (RSval), "r" (RDval) \
      : #RD, #RS, "cc", "memory" \
        ); \
        printf(instruction" :: %x, RSval: %x\n", \
        out, RSval); \
}

#define TESTINST3(instruction, RDval, RSval, RD, RS) \
{ \
   unsigned int out = 0; \
   __asm__ volatile( \
      "move $" #RS ", %1\n\t" \
      "move $" #RD ", %2\n\t" \
      instruction" $" #RS ", end"instruction#RDval"\n\t" \
      "nop\n\t" \
      "addiu $" #RD ", $" #RD", 5\n\t" \
      "end"instruction#RDval":\n\t" \
      "addiu $" #RD ", $" #RD", 1\n\t" \
      "move %0, $" #RD "\n\t" \
      : "=&r" (out) \
      : "r" (RSval), "r" (RDval) \
      : #RD, #RS, "cc", "memory" \
        ); \
        printf(instruction" :: %x, RSval: %x\n", \
        out, RSval); \
}

#define TESTINST4(instruction, RDval, RSval, RTval, RD, RS, RT) \
{ \
   unsigned int out = 0; \
   __asm__ volatile( \
      "move $" #RS ", %1\n\t" \
      "move $" #RT ", %2\n\t" \
      "move $" #RD ", %3\n\t" \
      instruction" $" #RS ", $" #RT ", end"instruction#RDval"\n\t" \
      "nop\n\t" \
      "addiu $" #RD ", $" #RD", 5\n\t" \
      "end"instruction#RDval":\n\t" \
      "addiu $" #RD ", $" #RD", 1\n\t" \
      "move %0, $" #RD "\n\t" \
      : "=&r" (out) \
      : "r" (RSval), "r" (RTval), "r" (RDval) \
      : #RD, #RS, #RT, "cc", "memory" \
        ); \
        printf(instruction" :: %x, RSval: %x, RTval: %x\n", \
        out, RSval, RTval); \
}

#define TESTINST3ja(instruction, RSval, RD) \
{ \
   unsigned int out = 0; \
   unsigned int out1 = 0; \
   unsigned int out2 = 0; \
   __asm__ volatile( \
      "move $" #RD ", %3\n\t" \
      LOAD_ADDRESS " $t0, r_end"instruction#RSval"\n\t" \
      LOAD_ADDRESS " $t1, end"instruction#RSval "\n\t" \
      instruction " $t0, "#RSval"\n\t" \
      "end"instruction#RSval":\n\t" \
      "addiu $" #RD ", $" #RD", 100\n\t" \
      "nop \n\t" \
      "r_end"instruction#RSval":\n\t" \
      "addiu $" #RD ", $" #RD", 1\n\t" \
      "move %0, $" #RD "\n\t" \
      "move %1, $t1 \n\t" \
      "move %2, $ra \n\t" \
      : "=&r" (out), "=&r" (out1), "=&r" (out2) \
      : "r" (RSval) \
      : #RD, "t0", "t1", "ra", "cc", "memory" \
        ); \
        printf(instruction ":: %x, RSval: %x, $t1 == $ra: %x\n", \
        out, RSval, (out1 == out2)); \
}

int main() {

#if (__mips_isa_rev>=6)
   printf("balc\n");
   TESTINST1("end12", "balc", 0, v0);
   TESTINST1("end13", "balc", 1, v0);
   TESTINST1("end14", "balc", 2, v0);
   TESTINST1("end15", "balc", 3, v0);
   TESTINST1("end16", "balc", 4, v0);
   TESTINST1("end21", "balc", 5, v0);
   TESTINST1("end22", "balc", 6, v0);
   TESTINST1("end23", "balc", 7, v0);
   TESTINST1("end24", "balc", 8, v0);
   TESTINST1("end25", "balc", 9, v0);

   printf("bc\n");
   TESTINST1("end12", "bc", 10, v0);
   TESTINST1("end13", "bc", 11, v0);
   TESTINST1("end14", "bc", 12, v0);
   TESTINST1("end15", "bc", 13, v0);
   TESTINST1("end16", "bc", 14, v0);
   TESTINST1("end21", "bc", 15, v0);
   TESTINST1("end22", "bc", 16, v0);
   TESTINST1("end23", "bc", 17, v0);
   TESTINST1("end24", "bc", 18, v0);
   TESTINST1("end25", "bc", 19, v0);

   printf("bgezalc\n");
   TESTINST2("bgezalc", 0, 0, v0, v1);
   TESTINST2("bgezalc", 1, 1, v1, a0);
   TESTINST2("bgezalc", 2, 0xffffffff, a0, a1);
   TESTINST2("bgezalc", 3, 0xffffffff, a1, a2);
   TESTINST2("bgezalc", 4, 0xfffffffe, a2, t0);
   TESTINST2("bgezalc", 5, 0xffffffff, a3, t0);
   TESTINST2("bgezalc", 6, 0x5, t0, t1);
   TESTINST2("bgezalc", 7, -3, t1, t2);
   TESTINST2("bgezalc", 8, 125, t2, t3);
   TESTINST2("bgezalc", 9, 0x80000000, t3, 12);
   TESTINST2("bgezalc", 10, 0xffffffff, 12, 13);
   TESTINST2("bgezalc", 11, 0x256, 13, 14);
   TESTINST2("bgezalc", 12, 0x55, 14, 15);
   TESTINST2("bgezalc", 13, 0xfff, s0, s1);
   TESTINST2("bgezalc", 14, -1, v0, t9);
   TESTINST2("bgezalc", 15, -1, t9, t8);

   printf("bgtzalc\n");
   TESTINST2("bgtzalc", 0, 0, v0, v1);
   TESTINST2("bgtzalc", 1, 1, v1, a0);
   TESTINST2("bgtzalc", 2, 0xffffffff, a0, a1);
   TESTINST2("bgtzalc", 3, 0xffffffff, a1, a2);
   TESTINST2("bgtzalc", 4, 0xfffffffe, a2, t0);
   TESTINST2("bgtzalc", 5, 0xffffffff, a3, t0);
   TESTINST2("bgtzalc", 6, 0x5, t0, t1);
   TESTINST2("bgtzalc", 7, -3, t1, t2);
   TESTINST2("bgtzalc", 8, 125, t2, t3);
   TESTINST2("bgtzalc", 9, 0x80000000, t3, 12);
   TESTINST2("bgtzalc", 10, 0xffffffff, 12, 13);
   TESTINST2("bgtzalc", 11, 0x256, 13, 14);
   TESTINST2("bgtzalc", 12, 0x55, 14, 15);
   TESTINST2("bgtzalc", 13, 0xfff, s0, s1);
   TESTINST2("bgtzalc", 14, -1, v0, t9);
   TESTINST2("bgtzalc", 15, -1, t9, t8);

   printf("blezalc\n");
   TESTINST2("blezalc", 0, 0, v0, v1);
   TESTINST2("blezalc", 1, 1, v1, a0);
   TESTINST2("blezalc", 2, 0xffffffff, a0, a1);
   TESTINST2("blezalc", 3, 0xffffffff, a1, a2);
   TESTINST2("blezalc", 4, 0xfffffffe, a2, t0);
   TESTINST2("blezalc", 5, 0xffffffff, a3, t0);
   TESTINST2("blezalc", 6, 0x5, t0, t1);
   TESTINST2("blezalc", 7, -3, t1, t2);
   TESTINST2("blezalc", 8, 125, t2, t3);
   TESTINST2("blezalc", 9, 0x80000000, t3, 12);
   TESTINST2("blezalc", 10, 0xffffffff, 12, 13);
   TESTINST2("blezalc", 11, 0x256, 13, 14);
   TESTINST2("blezalc", 12, 0x55, 14, 15);
   TESTINST2("blezalc", 13, 0xfff, s0, s1);
   TESTINST2("blezalc", 14, -1, v0, t9);
   TESTINST2("blezalc", 15, -1, t9, t8);

   printf("bltzalc\n");
   TESTINST2("bltzalc", 0, 0, v0, v1);
   TESTINST2("bltzalc", 1, 1, v1, a0);
   TESTINST2("bltzalc", 2, 0xffffffff, a0, a1);
   TESTINST2("bltzalc", 3, 0xffffffff, a1, a2);
   TESTINST2("bltzalc", 4, 0xfffffffe, a2, t0);
   TESTINST2("bltzalc", 5, 0xffffffff, a3, t0);
   TESTINST2("bltzalc", 6, 0x5, t0, t1);
   TESTINST2("bltzalc", 7, -3, t1, t2);
   TESTINST2("bltzalc", 8, 125, t2, t3);
   TESTINST2("bltzalc", 9, 0x80000000, t3, 12);
   TESTINST2("bltzalc", 10, 0xffffffff, 12, 13);
   TESTINST2("bltzalc", 11, 0x256, 13, 14);
   TESTINST2("bltzalc", 12, 0x55, 14, 15);
   TESTINST2("bltzalc", 13, 0xfff, s0, s1);
   TESTINST2("bltzalc", 14, -1, v0, t9);
   TESTINST2("bltzalc", 15, -1, t9, t8);

   printf("beqzalc\n");
   TESTINST2("beqzalc", 0, 0, v0, v1);
   TESTINST2("beqzalc", 1, 1, v1, a0);
   TESTINST2("beqzalc", 2, 0xffffffff, a0, a1);
   TESTINST2("beqzalc", 3, 0xffffffff, a1, a2);
   TESTINST2("beqzalc", 4, 0xfffffffe, a2, t0);
   TESTINST2("beqzalc", 5, 0xffffffff, a3, t0);
   TESTINST2("beqzalc", 6, 0x5, t0, t1);
   TESTINST2("beqzalc", 7, -3, t1, t2);
   TESTINST2("beqzalc", 8, 125, t2, t3);
   TESTINST2("beqzalc", 9, 0x80000000, t3, 12);
   TESTINST2("beqzalc", 10, 0xffffffff, 12, 13);
   TESTINST2("beqzalc", 11, 0x256, 13, 14);
   TESTINST2("beqzalc", 12, 0x55, 14, 15);
   TESTINST2("beqzalc", 13, 0xfff, s0, s1);
   TESTINST2("beqzalc", 14, -1, v0, t9);
   TESTINST2("beqzalc", 15, -1, t9, t8);

   printf("bnezalc\n");
   TESTINST2("bnezalc", 0, 0, v0, v1);
   TESTINST2("bnezalc", 1, 1, v1, a0);
   TESTINST2("bnezalc", 2, 0xffffffff, a0, a1);
   TESTINST2("bnezalc", 3, 0xffffffff, a1, a2);
   TESTINST2("bnezalc", 4, 0xfffffffe, a2, t0);
   TESTINST2("bnezalc", 5, 0xffffffff, a3, t0);
   TESTINST2("bnezalc", 6, 0x5, t0, t1);
   TESTINST2("bnezalc", 7, -3, t1, t2);
   TESTINST2("bnezalc", 8, 125, t2, t3);
   TESTINST2("bnezalc", 9, 0x80000000, t3, 12);
   TESTINST2("bnezalc", 10, 0xffffffff, 12, 13);
   TESTINST2("bnezalc", 11, 0x256, 13, 14);
   TESTINST2("bnezalc", 12, 0x55, 14, 15);
   TESTINST2("bnezalc", 13, 0xfff, s0, s1);
   TESTINST2("bnezalc", 14, -1, v0, t9);
   TESTINST2("bnezalc", 15, -1, t9, t8);

   printf("blezc\n");
   TESTINST3("blezc", 0, 0, v0, v1);
   TESTINST3("blezc", 1, 1, v1, a0);
   TESTINST3("blezc", 2, 0xffffffff, a0, a1);
   TESTINST3("blezc", 3, 0xffffffff, a1, a2);
   TESTINST3("blezc", 4, 0xfffffffe, a2, t0);
   TESTINST3("blezc", 5, 0xffffffff, a3, t0);
   TESTINST3("blezc", 6, 0x5, t0, t1);
   TESTINST3("blezc", 7, -3, t1, t2);
   TESTINST3("blezc", 8, 125, t2, t3);
   TESTINST3("blezc", 9, 0x80000000, t3, 12);
   TESTINST3("blezc", 10, 0xffffffff, 12, 13);
   TESTINST3("blezc", 11, 0x256, 13, 14);
   TESTINST3("blezc", 12, 0x55, 14, 15);
   TESTINST3("blezc", 13, 0xfff, s0, s1);
   TESTINST3("blezc", 14, -1, v0, t9);
   TESTINST3("blezc", 15, -1, t9, t8);

   printf("bgezc\n");
   TESTINST3("bgezc", 0, 0, v0, v1);
   TESTINST3("bgezc", 1, 1, v1, a0);
   TESTINST3("bgezc", 2, 0xffffffff, a0, a1);
   TESTINST3("bgezc", 3, 0xffffffff, a1, a2);
   TESTINST3("bgezc", 4, 0xfffffffe, a2, t0);
   TESTINST3("bgezc", 5, 0xffffffff, a3, t0);
   TESTINST3("bgezc", 6, 0x5, t0, t1);
   TESTINST3("bgezc", 7, -3, t1, t2);
   TESTINST3("bgezc", 8, 125, t2, t3);
   TESTINST3("bgezc", 9, 0x80000000, t3, 12);
   TESTINST3("bgezc", 10, 0xffffffff, 12, 13);
   TESTINST3("bgezc", 11, 0x256, 13, 14);
   TESTINST3("bgezc", 12, 0x55, 14, 15);
   TESTINST3("bgezc", 13, 0xfff, s0, s1);
   TESTINST3("bgezc", 14, -1, v0, t9);
   TESTINST3("bgezc", 15, -1, t9, t8);

   printf("bgtzc\n");
   TESTINST3("bgtzc", 0, 0, v0, v1);
   TESTINST3("bgtzc", 1, 1, v1, a0);
   TESTINST3("bgtzc", 2, 0xffffffff, a0, a1);
   TESTINST3("bgtzc", 3, 0xffffffff, a1, a2);
   TESTINST3("bgtzc", 4, 0xfffffffe, a2, t0);
   TESTINST3("bgtzc", 5, 0xffffffff, a3, t0);
   TESTINST3("bgtzc", 6, 0x5, t0, t1);
   TESTINST3("bgtzc", 7, -3, t1, t2);
   TESTINST3("bgtzc", 8, 125, t2, t3);
   TESTINST3("bgtzc", 9, 0x80000000, t3, 12);
   TESTINST3("bgtzc", 10, 0xffffffff, 12, 13);
   TESTINST3("bgtzc", 11, 0x256, 13, 14);
   TESTINST3("bgtzc", 12, 0x55, 14, 15);
   TESTINST3("bgtzc", 13, 0xfff, s0, s1);
   TESTINST3("bgtzc", 14, -1, v0, t9);
   TESTINST3("bgtzc", 15, -1, t9, t8);

   printf("bgec\n");
   TESTINST4("bgec", 0, 0, 1, v0, v1, a0);
   TESTINST4("bgec", 1, 1, 1, v1, a0, a1);
   TESTINST4("bgec", 2, 0xffffffff, 0xffffffff, a0, a1, a2);
   TESTINST4("bgec", 3, 0xffffffff, 0xfffffffe, a1, a2, a3);
   TESTINST4("bgec", 4, 0xfffffffe, 0xffffffff, a2, t0, t1);
   TESTINST4("bgec", 5, 0xffffffff, 0xffffffff, a3, t0, t1);
   TESTINST4("bgec", 6, 0x5, 0x5, t0, t1, t2);
   TESTINST4("bgec", 7, -3, -4, t1, t2, t3);
   TESTINST4("bgec", 8, 125, 125, t2, t3, 12);
   TESTINST4("bgec", 9, 0x80000000, 0x80000000, t3, 12, 13);
   TESTINST4("bgec", 10, 0xffffffff, 0x80000000, 12, 13, 14);
   TESTINST4("bgec", 11, 0x256, 0x256, 13, 14, 15);
   TESTINST4("bgec", 12, 0x55, 0x55, 14, 15, s0);
   TESTINST4("bgec", 13, 0xfff, 0xdd, s0, s1, s2);
   TESTINST4("bgec", 14, -1, 0x5, v0, t9, t8);
   TESTINST4("bgec", 15, -1, -1, t9, t8, a3);

   printf("bltc\n");
   TESTINST4("bltc", 0, 0, 1, v0, v1, a0);
   TESTINST4("bltc", 1, 1, 1, v1, a0, a1);
   TESTINST4("bltc", 2, 0xffffffff, 0xffffffff, a0, a1, a2);
   TESTINST4("bltc", 3, 0xffffffff, 0xfffffffe, a1, a2, a3);
   TESTINST4("bltc", 4, 0xfffffffe, 0xffffffff, a2, t0, t1);
   TESTINST4("bltc", 5, 0xffffffff, 0xffffffff, a3, t0, t1);
   TESTINST4("bltc", 6, 0x5, 0x5, t0, t1, t2);
   TESTINST4("bltc", 7, -3, -4, t1, t2, t3);
   TESTINST4("bltc", 8, 125, 125, t2, t3, 12);
   TESTINST4("bltc", 9, 0x80000000, 0x80000000, t3, 12, 13);
   TESTINST4("bltc", 10, 0xffffffff, 0x80000000, 12, 13, 14);
   TESTINST4("bltc", 11, 0x256, 0x256, 13, 14, 15);
   TESTINST4("bltc", 12, 0x55, 0x55, 14, 15, s0);
   TESTINST4("bltc", 13, 0xfff, 0xdd, s0, s1, s2);
   TESTINST4("bltc", 14, -1, 0x5, v0, t9, t8);
   TESTINST4("bltc", 15, -1, -1, t9, t8, a3);

   printf("bltzc\n");
   TESTINST3("bltzc", 0, 0, v0, v1);
   TESTINST3("bltzc", 1, 1, v1, a0);
   TESTINST3("bltzc", 2, 0xffffffff, a0, a1);
   TESTINST3("bltzc", 3, 0xffffffff, a1, a2);
   TESTINST3("bltzc", 4, 0xfffffffe, a2, t0);
   TESTINST3("bltzc", 5, 0xffffffff, a3, t0);
   TESTINST3("bltzc", 6, 0x5, t0, t1);
   TESTINST3("bltzc", 7, -3, t1, t2);
   TESTINST3("bltzc", 8, 125, t2, t3);
   TESTINST3("bltzc", 9, 0x80000000, t3, 12);
   TESTINST3("bltzc", 10, 0xffffffff, 12, 13);
   TESTINST3("bltzc", 11, 0x256, 13, 14);
   TESTINST3("bltzc", 12, 0x55, 14, 15);
   TESTINST3("bltzc", 13, 0xfff, s0, s1);
   TESTINST3("bltzc", 14, -1, v0, t9);
   TESTINST3("bltzc", 15, -1, t9, t8);

   printf("bgeuc\n");
   TESTINST4("bgeuc", 0, 0, 1, v0, v1, a0);
   TESTINST4("bgeuc", 1, 1, 1, v1, a0, a1);
   TESTINST4("bgeuc", 2, 0xffffffff, 0xffffffff, a0, a1, a2);
   TESTINST4("bgeuc", 3, 0xffffffff, 0xfffffffe, a1, a2, a3);
   TESTINST4("bgeuc", 4, 0xfffffffe, 0xffffffff, a2, t0, t1);
   TESTINST4("bgeuc", 5, 0xffffffff, 0xffffffff, a3, t0, t1);
   TESTINST4("bgeuc", 6, 0x5, 0x5, t0, t1, t2);
   TESTINST4("bgeuc", 7, -3, -4, t1, t2, t3);
   TESTINST4("bgeuc", 8, 125, 125, t2, t3, 12);
   TESTINST4("bgeuc", 9, 0x80000000, 0x80000000, t3, 12, 13);
   TESTINST4("bgeuc", 10, 0xffffffff, 0x80000000, 12, 13, 14);
   TESTINST4("bgeuc", 11, 0x256, 0x256, 13, 14, 15);
   TESTINST4("bgeuc", 12, 0x55, 0x55, 14, 15, s0);
   TESTINST4("bgeuc", 13, 0xfff, 0xdd, s0, s1, s2);
   TESTINST4("bgeuc", 14, -1, 0x5, v0, t9, t8);
   TESTINST4("bgeuc", 15, -1, -1, t9, t8, a3);

   printf("bltuc\n");
   TESTINST4("bltuc", 0, 0, 1, v0, v1, a0);
   TESTINST4("bltuc", 1, 1, 1, v1, a0, a1);
   TESTINST4("bltuc", 2, 0xffffffff, 0xffffffff, a0, a1, a2);
   TESTINST4("bltuc", 3, 0xffffffff, 0xfffffffe, a1, a2, a3);
   TESTINST4("bltuc", 4, 0xfffffffe, 0xffffffff, a2, t0, t1);
   TESTINST4("bltuc", 5, 0xffffffff, 0xffffffff, a3, t0, t1);
   TESTINST4("bltuc", 6, 0x5, 0x5, t0, t1, t2);
   TESTINST4("bltuc", 7, -3, -4, t1, t2, t3);
   TESTINST4("bltuc", 8, 125, 125, t2, t3, 12);
   TESTINST4("bltuc", 9, 0x80000000, 0x80000000, t3, 12, 13);
   TESTINST4("bltuc", 10, 0xffffffff, 0x80000000, 12, 13, 14);
   TESTINST4("bltuc", 11, 0x256, 0x256, 13, 14, 15);
   TESTINST4("bltuc", 12, 0x55, 0x55, 14, 15, s0);
   TESTINST4("bltuc", 13, 0xfff, 0xdd, s0, s1, s2);
   TESTINST4("bltuc", 14, -1, 0x5, v0, t9, t8);
   TESTINST4("bltuc", 15, -1, -1, t9, t8, a3);

   printf("beqc\n");
   TESTINST4("beqc", 0, 0, 1, v0, v1, a0);
   TESTINST4("beqc", 1, 1, 1, v1, a0, a1);
   TESTINST4("beqc", 2, 0xffffffff, 0xffffffff, a0, a1, a2);
   TESTINST4("beqc", 3, 0xffffffff, 0xfffffffe, a1, a2, a3);
   TESTINST4("beqc", 4, 0xfffffffe, 0xffffffff, a2, t0, t1);
   TESTINST4("beqc", 5, 0xffffffff, 0xffffffff, a3, t0, t1);
   TESTINST4("beqc", 6, 0x5, 0x5, t0, t1, t2);
   TESTINST4("beqc", 7, -3, -4, t1, t2, t3);
   TESTINST4("beqc", 8, 125, 125, t2, t3, 12);
   TESTINST4("beqc", 9, 0x80000000, 0x80000000, t3, 12, 13);
   TESTINST4("beqc", 10, 0xffffffff, 0x80000000, 12, 13, 14);
   TESTINST4("beqc", 11, 0x256, 0x256, 13, 14, 15);
   TESTINST4("beqc", 12, 0x55, 0x55, 14, 15, s0);
   TESTINST4("beqc", 13, 0xfff, 0xdd, s0, s1, s2);
   TESTINST4("beqc", 14, -1, 0x5, v0, t9, t8);
   TESTINST4("beqc", 15, -1, -1, t9, t8, a3);

   printf("bnec\n");
   TESTINST4("bnec", 0, 0, 1, v0, v1, a0);
   TESTINST4("bnec", 1, 1, 1, v1, a0, a1);
   TESTINST4("bnec", 2, 0xffffffff, 0xffffffff, a0, a1, a2);
   TESTINST4("bnec", 3, 0xffffffff, 0xfffffffe, a1, a2, a3);
   TESTINST4("bnec", 4, 0xfffffffe, 0xffffffff, a2, t0, t1);
   TESTINST4("bnec", 5, 0xffffffff, 0xffffffff, a3, t0, t1);
   TESTINST4("bnec", 6, 0x5, 0x5, t0, t1, t2);
   TESTINST4("bnec", 7, -3, -4, t1, t2, t3);
   TESTINST4("bnec", 8, 125, 125, t2, t3, 12);
   TESTINST4("bnec", 9, 0x80000000, 0x80000000, t3, 12, 13);
   TESTINST4("bnec", 10, 0xffffffff, 0x80000000, 12, 13, 14);
   TESTINST4("bnec", 11, 0x256, 0x256, 13, 14, 15);
   TESTINST4("bnec", 12, 0x55, 0x55, 14, 15, s0);
   TESTINST4("bnec", 13, 0xfff, 0xdd, s0, s1, s2);
   TESTINST4("bnec", 14, -1, 0x5, v0, t9, t8);
   TESTINST4("bnec", 15, -1, -1, t9, t8, a3);

   printf("beqzc\n");
   TESTINST3("beqzc", 0, 0, v0, v1);
   TESTINST3("beqzc", 1, 1, v1, a0);
   TESTINST3("beqzc", 2, 0xffffffff, a0, a1);
   TESTINST3("beqzc", 3, 0xffffffff, a1, a2);
   TESTINST3("beqzc", 4, 0xfffffffe, a2, t0);
   TESTINST3("beqzc", 5, 0xffffffff, a3, t0);
   TESTINST3("beqzc", 6, 0x5, t0, t1);
   TESTINST3("beqzc", 7, -3, t1, t2);
   TESTINST3("beqzc", 8, 125, t2, t3);
   TESTINST3("beqzc", 9, 0x80000000, t3, 12);
   TESTINST3("beqzc", 10, 0xffffffff, 12, 13);
   TESTINST3("beqzc", 11, 0x256, 13, 14);
   TESTINST3("beqzc", 12, 0x55, 14, 15);
   TESTINST3("beqzc", 13, 0xfff, s0, s1);
   TESTINST3("beqzc", 14, -1, v0, t9);
   TESTINST3("beqzc", 15, -1, t9, t8);

   printf("bnezc\n");
   TESTINST3("bnezc", 0, 0, v0, v1);
   TESTINST3("bnezc", 1, 1, v1, a0);
   TESTINST3("bnezc", 2, 0xffffffff, a0, a1);
   TESTINST3("bnezc", 3, 0xffffffff, a1, a2);
   TESTINST3("bnezc", 4, 0xfffffffe, a2, t0);
   TESTINST3("bnezc", 5, 0xffffffff, a3, t0);
   TESTINST3("bnezc", 6, 0x5, t0, t1);
   TESTINST3("bnezc", 7, -3, t1, t2);
   TESTINST3("bnezc", 8, 125, t2, t3);
   TESTINST3("bnezc", 9, 0x80000000, t3, 12);
   TESTINST3("bnezc", 10, 0xffffffff, 12, 13);
   TESTINST3("bnezc", 11, 0x256, 13, 14);
   TESTINST3("bnezc", 12, 0x55, 14, 15);
   TESTINST3("bnezc", 13, 0xfff, s0, s1);
   TESTINST3("bnezc", 14, -1, v0, t9);
   TESTINST3("bnezc", 15, -1, t9, t8);

   printf("bovc\n");
   TESTINST4("bovc", 0, 0, 1, v0, v1, a0);
   TESTINST4("bovc", 1, 1, 1, v1, a0, a1);
   TESTINST4("bovc", 2, 0xffffffff, 0xffffffff, a0, a1, a2);
   TESTINST4("bovc", 3, 0xffffffff, 0xfffffffe, a1, a2, a3);
   TESTINST4("bovc", 4, 0xfffffffe, 0xffffffff, a2, t0, t1);
   TESTINST4("bovc", 5, 0xffffffff, 0xffffffff, a3, t0, t1);
   TESTINST4("bovc", 6, 0x5, 0x5, t0, t1, t2);
   TESTINST4("bovc", 7, -3, -4, t1, t2, t3);
   TESTINST4("bovc", 8, 125, 125, t2, t3, 12);
   TESTINST4("bovc", 9, 0x80000000, 0x80000000, t3, 12, 13);
   TESTINST4("bovc", 10, 0xffffffff, 0x80000000, 12, 13, 14);
   TESTINST4("bovc", 11, 0x256, 0x256, 13, 14, 15);
   TESTINST4("bovc", 12, 0x55, 0x55, 14, 15, s0);
   TESTINST4("bovc", 13, 0xfff, 0xdd, s0, s1, s2);
   TESTINST4("bovc", 14, -1, 0x5, v0, t9, t8);
   TESTINST4("bovc", 15, -1, -1, t9, t8, a3);

   printf("bnvc\n");
   TESTINST4("bnvc", 0, 0, 1, v0, v1, a0);
   TESTINST4("bnvc", 1, 1, 1, v1, a0, a1);
   TESTINST4("bnvc", 2, 0xffffffff, 0xffffffff, a0, a1, a2);
   TESTINST4("bnvc", 3, 0xffffffff, 0xfffffffe, a1, a2, a3);
   TESTINST4("bnvc", 4, 0xfffffffe, 0xffffffff, a2, t0, t1);
   TESTINST4("bnvc", 5, 0xffffffff, 0xffffffff, a3, t0, t1);
   TESTINST4("bnvc", 6, 0x5, 0x5, t0, t1, t2);
   TESTINST4("bnvc", 7, -3, -4, t1, t2, t3);
   TESTINST4("bnvc", 8, 125, 125, t2, t3, 12);
   TESTINST4("bnvc", 9, 0x80000000, 0x80000000, t3, 12, 13);
   TESTINST4("bnvc", 10, 0xffffffff, 0x80000000, 12, 13, 14);
   TESTINST4("bnvc", 11, 0x256, 0x256, 13, 14, 15);
   TESTINST4("bnvc", 12, 0x55, 0x55, 14, 15, s0);
   TESTINST4("bnvc", 13, 0xfff, 0xdd, s0, s1, s2);
   TESTINST4("bnvc", 14, -1, 0x5, v0, t9, t8);
   TESTINST4("bnvc", 15, -1, -1, t9, t8, a3);

   printf("jialc\n");
   TESTINST3ja("jialc",  0, v0);
   TESTINST3ja("jialc",  4, v0);
   TESTINST3ja("jialc",  8, v0);
   TESTINST3ja("jialc", 16, v0);
   TESTINST3ja("jialc", 32, v0);

   printf("jic\n");
   TESTINST3ja("jic",  0, v0);
   TESTINST3ja("jic",  4, v0);
   TESTINST3ja("jic",  8, v0);
   TESTINST3ja("jic", 16, v0);
   TESTINST3ja("jic", 32, v0);
   TESTINST3ja("jic", 64, v0);
#endif
   return 0;
}

