#include <stdio.h>



#define TESTINST1(instruction, RSval, RD)                      \
{                                                              \
   unsigned int out;                                           \
   unsigned int out1;                                          \
   __asm__ volatile(                                           \
      ".set push \n\t"                                         \
      ".set noreorder \n\t"                                    \
      "jal end"instruction#RSval "\n\t"                        \
      "nop \n\t"                                               \
      "end"instruction#RSval ": \n\t"                          \
      instruction " $" #RD ", " #RSval " \n\t"                 \
      "move %0, $" #RD "\n\t"                                  \
      "move %1, $ra \n\t"                                      \
      ".set pop \n\t"                                          \
      : "=&r" (out), "=&r" (out1)                              \
      : "r" (RSval)                                            \
      : #RD, "ra", "cc", "memory"                              \
        );                                                     \
        printf(instruction" :: out - ra %x, RSval 0x%08x\n",   \
         out - out1, RSval);                                   \
}

#define TESTINST2(instruction, RSval, RD)                      \
{                                                              \
   unsigned int out;                                           \
   unsigned int out1;                                          \
   __asm__ volatile(                                           \
      ".set push \n\t"                                         \
      ".set noreorder \n\t"                                    \
      "jal end"instruction#RSval "\n\t"                        \
      "nop \n\t"                                               \
      "end"instruction#RSval ": \n\t"                          \
      instruction " $" #RD ", " #RSval " \n\t"                 \
      "move %0, $" #RD "\n\t"                                  \
      "move %1, $ra \n\t"                                      \
      ".set pop \n\t"                                          \
      : "=&r" (out), "=&r" (out1)                              \
      : "r" (RSval)                                            \
      : #RD, "ra", "cc", "memory"                              \
        );                                                     \
        printf(instruction" :: out - ra %x, RSval 0x%08x\n",   \
         out - (out1 & ~0xffffffff), RSval);                   \
}

#define TESTINST3(instruction, RSval, RD)                      \
{                                                              \
   unsigned int  out = 0;                                      \
   __asm__ __volatile__(                                       \
      ".set push \n\t"                                         \
      ".set noreorder \n\t"                                    \
      "lbl"instruction#RSval ": \n\t"                          \
      "or $0, $0, $0 \n\t"                                     \
      "and $0, $0, $0 \n\t"                                    \
      instruction" $"#RD ", lbl"instruction#RSval "\n\t"       \
      "move %0, $"#RD "\n\t"                                   \
      ".set pop \n\t"                                          \
      : "=r" (out)                                             \
      :                                                        \
      : "t0", "t1"                                             \
   );                                                          \
   printf("%s :: out: 0x%x\n", instruction, out);              \
}

int main() {
#if (__mips_isa_rev>=6)
   printf("addiupc\n");
   TESTINST1("addiupc",     0, v0);
   TESTINST1("addiupc",     4, v1);
   TESTINST1("addiupc",    16, a0);
   TESTINST1("addiupc",    64, a1);
   TESTINST1("addiupc",   256, a3);
   TESTINST1("addiupc",  1024, t0);
   TESTINST1("addiupc",  4096, t1);
   TESTINST1("addiupc", 16384, t2);

   printf("\naluipc\n");
   TESTINST2("aluipc",     0, v0);
   TESTINST2("aluipc",     4, v1);
   TESTINST2("aluipc",    16, a0);
   TESTINST2("aluipc",    64, a1);
   TESTINST2("aluipc",   256, a3);
   TESTINST2("aluipc",  1024, t0);
   TESTINST2("aluipc",  4096, t1);
   TESTINST2("aluipc", 16384, t2);

   printf("\nauipc\n");
   TESTINST1("auipc",     0, v0);
   TESTINST1("auipc",     4, v1);
   TESTINST1("auipc",    16, a0);
   TESTINST1("auipc",    64, a1);
   TESTINST1("auipc",   256, a3);
   TESTINST1("auipc",  1024, t0);
   TESTINST1("auipc",  4096, t1);
   TESTINST1("auipc", 16384, t2);

   printf("\nlwpc\n");
   TESTINST3("lwpc",     0, v0);
   TESTINST3("lwpc",     4, v1);
   TESTINST3("lwpc",    16, a0);
   TESTINST3("lwpc",    64, a1);
   TESTINST3("lwpc",   256, a3);
   TESTINST3("lwpc",  1024, t0);
   TESTINST3("lwpc",  4096, t1);
   TESTINST3("lwpc", 16384, t2);

#endif

   return 0;
}

