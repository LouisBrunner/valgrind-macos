#include <stdio.h>

unsigned long long mem[] = { 0x3FF0000000000001ULL, 0x3FF0000000000000ULL,
         0x00000000ffffffffULL, 0x1234563388994400ULL,
         0x0000004412369801ULL, 0x111111eeeeeee220ULL,
         0xAAAAABBBBBCCCDDDULL, 0xaa55cc2266dd2200ULL,
         0x3FF0045698720001ULL, 0x3FF0000000000000ULL,
         0x00000000ffffffffULL, 0x0007200059762458ULL, 
         0xaa55c200abcdefabULL, 0x852369741afedbc6ULL };



#define TEST1(instruction, RD, RS, RT, RSval, RTval) \
{ \
   unsigned long long result; \
   __asm__ volatile (  \
  "move $"#RS", %1\n\t"  \
  "move $"#RT", %2\n\t"  \
  instruction" $"#RD", $"#RS", $"#RT", 1\n\t"  \
  "move %0, $"#RD"\n\t"  \
   : "=&r"(result)  \
   : "r"(RSval), "r"(RTval)  \
   : #RD, #RS, #RT, "memory"  \
  );  \
   printf(instruction":: %llx\n", result);  \
}

#define TEST2(instruction, RS, RT, RSval, RTval) \
{ \
   unsigned long long result; \
   __asm__ volatile (  \
  "move $"#RS", %1\n\t"  \
  "move $"#RT", %2\n\t"  \
  instruction" $"#RS", $"#RT", 1\n\t"  \
  "move %0, $"#RS"\n\t"  \
   : "=&r"(result)  \
   : "r"(RSval), "r"(RTval)  \
   : #RS, #RT, "memory"  \
  );  \
   printf(instruction":: %llx\n", result);  \
}

#define TEST3(instruction, RS, RSval) \
{ \
   unsigned long long result; \
   __asm__ volatile (  \
  "move $"#RS", %1\n\t"  \
  instruction" $"#RS", $"#RS", 1\n\t"  \
  "move %0, $"#RS"\n\t"  \
   : "=&r"(result)  \
   : "r"(RSval) \
   : #RS, "memory"  \
  );  \
   printf(instruction":: %llx\n", result);  \
}

#define TEST4(instruction, RS, RT, RSval, RTval) \
{ \
   unsigned long long result; \
   __asm__ volatile (  \
  "move $"#RS", %1\n\t"  \
  "move $"#RT", %2\n\t"  \
  instruction" $"#RS", $"#RT" \n\t"  \
  "move %0, $"#RS"\n\t"  \
   : "=&r"(result)  \
   : "r"(RSval), "r"(RTval)  \
   : #RS, #RT, "memory"  \
  );  \
   printf(instruction":: %llx\n", result);  \
}

#define TEST5(instruction, RD, RS, RT, RSval, RTval) \
{ \
   unsigned long long result; \
   __asm__ volatile (  \
  "move $"#RS", %1\n\t"  \
  "move $"#RT", %2\n\t"  \
  instruction" $"#RD", $"#RS", $"#RT" \n\t"  \
  "move %0, $"#RD"\n\t"  \
   : "=&r"(result)  \
   : "r"(RSval), "r"(RTval)  \
   : #RD, #RS, #RT, "memory"  \
  );  \
   printf(instruction":: %llx\n", result);  \
}

#define TEST6(instruction, RSval, RD)  \
{  \
   unsigned long long out = 0; \
   __asm__ __volatile__( \
      ".set push \n\t" \
      ".set noreorder \n\t" \
      "lbl"instruction#RSval ": \n\t" \
      "or $0, $0, $0 \n\t" \
      "and $0, $0, $0 \n\t" \
      instruction" $"#RD ", lbl"instruction#RSval "\n\t" \
      "move %0, $"#RD "\n\t" \
      ".set pop \n\t" \
      : "=r" (out) \
      :  \
      : "t0", "t1" \
   ); \
   printf("%s :: out: 0x%llx\n", instruction, out); \
}


int main() {

#if (__mips_isa_rev>=6)
   printf("dalign\n");
   TEST1("dalign" , t0, t1, t2,  mem[0],  0);
   TEST1("dalign" , t1, t2, t3,  mem[1],  1);
   TEST1("dalign" , t2, t3, v0,  mem[2],  2);
   TEST1("dalign" , t3, v0, v1,  mem[3],  3);
   TEST1("dalign" , v0, v1, a0,  mem[4],  4);
   TEST1("dalign" , v1, a0, a1,  mem[5],  5);
   TEST1("dalign" , a0, a1, a2,  mem[6],  6);
   TEST1("dalign" , a1, a2, a3,  mem[7],  7);
   TEST1("dalign" , s0, s1, s2,  mem[8],  8);
   TEST1("dalign" , s1, s2, s3,  mem[9],  9);
   TEST1("dalign" , s2, s3, s4, mem[10], 10);
   TEST1("dalign" , s3, s4, s5, mem[11], 11);
   TEST1("dalign" , s4, s5, s6, mem[12], 12);
   TEST1("dalign" , s5, s6, s7, mem[13], 13);

   printf("\ndaui\n");
   TEST2("daui" , t0, t1,  mem[0],  0);
   TEST2("daui" , t1, t2,  mem[1],  1);
   TEST2("daui" , t2, t3,  mem[2],  2);
   TEST2("daui" , t3, v0,  mem[3],  3);
   TEST2("daui" , v0, v1,  mem[4],  4);
   TEST2("daui" , v1, a0,  mem[5],  5);
   TEST2("daui" , a0, a1,  mem[6],  6);
   TEST2("daui" , a1, a2,  mem[7],  7);
   TEST2("daui" , a2, a3,  mem[8],  8);
   TEST2("daui" , s0, s1,  mem[9],  9);
   TEST2("daui" , s2, s3,  mem[10], 10);
   TEST2("daui" , s3, s4,  mem[11], 11);
   TEST2("daui" , s4, s5,  mem[12], 12);
   TEST2("daui" , s5, s6,  mem[13], 13);

   printf("\ndahi\n");
   TEST3("dahi" , t0,  mem[0]);
   TEST3("dahi" , t1,  mem[1]);
   TEST3("dahi" , t2,  mem[2]);
   TEST3("dahi" , t3,  mem[3]);
   TEST3("dahi" , v0,  mem[4]);
   TEST3("dahi" , v1,  mem[5]);
   TEST3("dahi" , a0,  mem[6]);
   TEST3("dahi" , a1,  mem[7]);
   TEST3("dahi" , a2,  mem[8]);
   TEST3("dahi" , a3,  mem[9]);
   TEST3("dahi" , s0,  mem[10]);
   TEST3("dahi" , s1,  mem[11]);
   TEST3("dahi" , s2,  mem[12]);
   TEST3("dahi" , s3,  mem[13]);

   printf("\ndati\n");
   TEST3("dati" , t0,  mem[0]);
   TEST3("dati" , t1,  mem[1]);
   TEST3("dati" , t2,  mem[2]);
   TEST3("dati" , t3,  mem[3]);
   TEST3("dati" , v0,  mem[4]);
   TEST3("dati" , v1,  mem[5]);
   TEST3("dati" , a0,  mem[6]);
   TEST3("dati" , a1,  mem[7]);
   TEST3("dati" , a2,  mem[8]);
   TEST3("dati" , a3,  mem[9]);
   TEST3("dati" , s0,  mem[10]);
   TEST3("dati" , s1,  mem[11]);
   TEST3("dati" , s2,  mem[12]);
   TEST3("dati" , s3,  mem[13]);

   printf("\ndbitswap\n");
   TEST4("dbitswap" , t0, t1,  mem[0],  0);
   TEST4("dbitswap" , t1, t2,  mem[1],  1);
   TEST4("dbitswap" , t2, t3,  mem[2],  2);
   TEST4("dbitswap" , t3, v0,  mem[3],  3);
   TEST4("dbitswap" , v0, v1,  mem[4],  4);
   TEST4("dbitswap" , v1, a0,  mem[5],  5);
   TEST4("dbitswap" , a0, a1,  mem[6],  6);
   TEST4("dbitswap" , a1, a2,  mem[7],  7);
   TEST4("dbitswap" , a2, a3,  mem[8],  8);
   TEST4("dbitswap" , a3, s0,  mem[9],  9);
   TEST4("dbitswap" , s0, s1,  mem[10], 10);
   TEST4("dbitswap" , s1, s2,  mem[11], 11);
   TEST4("dbitswap" , s2, s3,  mem[12], 12);
   TEST4("dbitswap" , s3, s4,  mem[13], 13);

   printf("\nddiv\n");
   TEST5("ddiv" , t0, t1, t2,  mem[0],  mem[1]);
   TEST5("ddiv" , t1, t2, t3,  mem[1],  mem[2]);
   TEST5("ddiv" , t2, t3, v0,  mem[2],  mem[3]);
   TEST5("ddiv" , t3, v0, v1,  mem[3],  mem[4]);
   TEST5("ddiv" , v0, v1, a0,  mem[4],  mem[5]);
   TEST5("ddiv" , v1, a0, a1,  mem[5],  mem[6]);
   TEST5("ddiv" , a0, a1, a2,  mem[6],  mem[7]);
   TEST5("ddiv" , a1, a2, a3,  mem[7],  mem[8]);
   TEST5("ddiv" , a2, a3, s0,  mem[8],  mem[9]);
   TEST5("ddiv" , a3, s0, s1,  mem[9],  mem[10]);
   TEST5("ddiv" , s0, s1, s2,  mem[10], mem[11]);
   TEST5("ddiv" , s2, s3, s4,  mem[11], mem[12]);
   TEST5("ddiv" , s3, s4, s5,  mem[12], mem[13]);
   TEST5("ddiv" , s4, s5, s6,  mem[13], mem[0]);

   printf("\ndmod\n");
   TEST5("dmod" , t0, t1, t2,  mem[0],  mem[1]);
   TEST5("dmod" , t1, t2, t3,  mem[1],  mem[2]);
   TEST5("dmod" , t2, t3, v0,  mem[2],  mem[3]);
   TEST5("dmod" , t3, v0, v1,  mem[3],  mem[4]);
   TEST5("dmod" , v0, v1, a0,  mem[4],  mem[5]);
   TEST5("dmod" , v1, a0, a1,  mem[5],  mem[6]);
   TEST5("dmod" , a0, a1, a2,  mem[6],  mem[7]);
   TEST5("dmod" , a1, a2, a3,  mem[7],  mem[8]);
   TEST5("dmod" , a2, a3, s0,  mem[8],  mem[9]);
   TEST5("dmod" , a3, s0, s1,  mem[9],  mem[10]);
   TEST5("dmod" , s0, s1, s2,  mem[10], mem[11]);
   TEST5("dmod" , s2, s3, s4,  mem[11], mem[12]);
   TEST5("dmod" , s3, s4, s5,  mem[12], mem[13]);
   TEST5("dmod" , s4, s5, s6,  mem[13], mem[0]);

   printf("\nddivu\n");
   TEST5("ddivu" , t0, t1, t2,  mem[0],  mem[1]);
   TEST5("ddivu" , t1, t2, t3,  mem[1],  mem[2]);
   TEST5("ddivu" , t2, t3, v0,  mem[2],  mem[3]);
   TEST5("ddivu" , t3, v0, v1,  mem[3],  mem[4]);
   TEST5("ddivu" , v0, v1, a0,  mem[4],  mem[5]);
   TEST5("ddivu" , v1, a0, a1,  mem[5],  mem[6]);
   TEST5("ddivu" , a0, a1, a2,  mem[6],  mem[7]);
   TEST5("ddivu" , a1, a2, a3,  mem[7],  mem[8]);
   TEST5("ddivu" , a2, a3, s0,  mem[8],  mem[9]);
   TEST5("ddivu" , a3, s0, s1,  mem[9],  mem[10]);
   TEST5("ddivu" , s0, s1, s2,  mem[10], mem[11]);
   TEST5("ddivu" , s2, s3, s4,  mem[11], mem[12]);
   TEST5("ddivu" , s3, s4, s5,  mem[12], mem[13]);
   TEST5("ddivu" , s4, s5, s6,  mem[13], mem[0]);

   printf("\ndmodu\n");
   TEST5("dmodu" , t0, t1, t2,  mem[0],  mem[1]);
   TEST5("dmodu" , t1, t2, t3,  mem[1],  mem[2]);
   TEST5("dmodu" , t2, t3, v0,  mem[2],  mem[3]);
   TEST5("dmodu" , t3, v0, v1,  mem[3],  mem[4]);
   TEST5("dmodu" , v0, v1, a0,  mem[4],  mem[5]);
   TEST5("dmodu" , v1, a0, a1,  mem[5],  mem[6]);
   TEST5("dmodu" , a0, a1, a2,  mem[6],  mem[7]);
   TEST5("dmodu" , a1, a2, a3,  mem[7],  mem[8]);
   TEST5("dmodu" , a2, a3, s0,  mem[8],  mem[9]);
   TEST5("dmodu" , a3, s0, s1,  mem[9],  mem[10]);
   TEST5("dmodu" , s0, s1, s2,  mem[10], mem[11]);
   TEST5("dmodu" , s2, s3, s4,  mem[11], mem[12]);
   TEST5("dmodu" , s3, s4, s5,  mem[12], mem[13]);
   TEST5("dmodu" , s4, s5, s6,  mem[13], mem[0]);

   printf("\ndlsa\n");
   TEST1("dlsa" , t0, t1, t2,  mem[0],  0);
   TEST1("dlsa" , t1, t2, t3,  mem[1],  1);
   TEST1("dlsa" , t2, t3, v0,  mem[2],  2);
   TEST1("dlsa" , t3, v0, v1,  mem[3],  3);
   TEST1("dlsa" , v0, v1, a0,  mem[4],  4);
   TEST1("dlsa" , v1, a0, a1,  mem[5],  5);
   TEST1("dlsa" , a0, a1, a2,  mem[6],  6);
   TEST1("dlsa" , a1, a2, a3,  mem[7],  7);
   TEST1("dlsa" , s0, s1, s2,  mem[8],  8);
   TEST1("dlsa" , s1, s2, s3,  mem[9],  9);
   TEST1("dlsa" , s2, s3, s4, mem[10], 10);
   TEST1("dlsa" , s3, s4, s5, mem[11], 11);
   TEST1("dlsa" , s4, s5, s6, mem[12], 12);
   TEST1("dlsa" , s5, s6, s7, mem[13], 13);

   printf("\ndmul\n");
   TEST5("dmul" , t0, t1, t2,  mem[0],  mem[1]);
   TEST5("dmul" , t1, t2, t3,  mem[1],  mem[2]);
   TEST5("dmul" , t2, t3, v0,  mem[2],  mem[3]);
   TEST5("dmul" , t3, v0, v1,  mem[3],  mem[4]);
   TEST5("dmul" , v0, v1, a0,  mem[4],  mem[5]);
   TEST5("dmul" , v1, a0, a1,  mem[5],  mem[6]);
   TEST5("dmul" , a0, a1, a2,  mem[6],  mem[7]);
   TEST5("dmul" , a1, a2, a3,  mem[7],  mem[8]);
   TEST5("dmul" , a2, a3, s0,  mem[8],  mem[9]);
   TEST5("dmul" , a3, s0, s1,  mem[9],  mem[10]);
   TEST5("dmul" , s0, s1, s2,  mem[10], mem[11]);
   TEST5("dmul" , s2, s3, s4,  mem[11], mem[12]);
   TEST5("dmul" , s3, s4, s5,  mem[12], mem[13]);
   TEST5("dmul" , s4, s5, s6,  mem[13], mem[0]);

   printf("\ndmuh\n");
   TEST5("dmuh" , t0, t1, t2,  mem[0],  mem[1]);
   TEST5("dmuh" , t1, t2, t3,  mem[1],  mem[2]);
   TEST5("dmuh" , t2, t3, v0,  mem[2],  mem[3]);
   TEST5("dmuh" , t3, v0, v1,  mem[3],  mem[4]);
   TEST5("dmuh" , v0, v1, a0,  mem[4],  mem[5]);
   TEST5("dmuh" , v1, a0, a1,  mem[5],  mem[6]);
   TEST5("dmuh" , a0, a1, a2,  mem[6],  mem[7]);
   TEST5("dmuh" , a1, a2, a3,  mem[7],  mem[8]);
   TEST5("dmuh" , a2, a3, s0,  mem[8],  mem[9]);
   TEST5("dmuh" , a3, s0, s1,  mem[9],  mem[10]);
   TEST5("dmuh" , s0, s1, s2,  mem[10], mem[11]);
   TEST5("dmuh" , s2, s3, s4,  mem[11], mem[12]);
   TEST5("dmuh" , s3, s4, s5,  mem[12], mem[13]);
   TEST5("dmuh" , s4, s5, s6,  mem[13], mem[0]);

   printf("\ndmulu\n");
   TEST5("dmulu" , t0, t1, t2,  mem[0],  mem[1]);
   TEST5("dmulu" , t1, t2, t3,  mem[1],  mem[2]);
   TEST5("dmulu" , t2, t3, v0,  mem[2],  mem[3]);
   TEST5("dmulu" , t3, v0, v1,  mem[3],  mem[4]);
   TEST5("dmulu" , v0, v1, a0,  mem[4],  mem[5]);
   TEST5("dmulu" , v1, a0, a1,  mem[5],  mem[6]);
   TEST5("dmulu" , a0, a1, a2,  mem[6],  mem[7]);
   TEST5("dmulu" , a1, a2, a3,  mem[7],  mem[8]);
   TEST5("dmulu" , a2, a3, s0,  mem[8],  mem[9]);
   TEST5("dmulu" , a3, s0, s1,  mem[9],  mem[10]);
   TEST5("dmulu" , s0, s1, s2,  mem[10], mem[11]);
   TEST5("dmulu" , s2, s3, s4,  mem[11], mem[12]);
   TEST5("dmulu" , s3, s4, s5,  mem[12], mem[13]);
   TEST5("dmulu" , s4, s5, s6,  mem[13], mem[0]);

   printf("\ndmuhu\n");
   TEST5("dmuhu" , t0, t1, t2,  mem[0],  mem[1]);
   TEST5("dmuhu" , t1, t2, t3,  mem[1],  mem[2]);
   TEST5("dmuhu" , t2, t3, v0,  mem[2],  mem[3]);
   TEST5("dmuhu" , t3, v0, v1,  mem[3],  mem[4]);
   TEST5("dmuhu" , v0, v1, a0,  mem[4],  mem[5]);
   TEST5("dmuhu" , v1, a0, a1,  mem[5],  mem[6]);
   TEST5("dmuhu" , a0, a1, a2,  mem[6],  mem[7]);
   TEST5("dmuhu" , a1, a2, a3,  mem[7],  mem[8]);
   TEST5("dmuhu" , a2, a3, s0,  mem[8],  mem[9]);
   TEST5("dmuhu" , a3, s0, s1,  mem[9],  mem[10]);
   TEST5("dmuhu" , s0, s1, s2,  mem[10], mem[11]);
   TEST5("dmuhu" , s2, s3, s4,  mem[11], mem[12]);
   TEST5("dmuhu" , s3, s4, s5,  mem[12], mem[13]);
   TEST5("dmuhu" , s4, s5, s6,  mem[13], mem[0]);

   printf("\nldpc\n");
   TEST6("ldpc", 0, v0);
   TEST6("ldpc", 4, v1);
   TEST6("ldpc", 16, a0);
   TEST6("ldpc", 64, a1);
   TEST6("ldpc", 256, a3);
   TEST6("ldpc", 1024, t0);
   TEST6("ldpc", 4096, t1);
   TEST6("ldpc", 16384, t2);

   printf("\nlwupc\n");
   TEST6("lwupc", 0, v0);
   TEST6("lwupc", 4, v1);
   TEST6("lwupc", 16, a0);
   TEST6("lwupc", 64, a1);
   TEST6("lwupc", 256, a3);
   TEST6("lwupc", 1024, t0);
   TEST6("lwupc", 4096, t1);
   TEST6("lwupc", 16384, t2);
#endif
}

