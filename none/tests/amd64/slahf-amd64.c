
#include <stdio.h>

typedef unsigned long long int ULong;

extern ULong lahf_1 ( void );
asm("\n"
".text\n"
"lahf_1:\n"
"\tpushq $0\n"
"\tpopfq\n"
"\tmovabsq $0xFFFFFFFFFFFFFFFF, %rax\n"
"\tmovabsq $0x8765432112345678, %rdx\n"
"\tsubq %rax, %rdx\n"
"\t.byte 0x9F\n" /* lahf */
"\tret\n"
".previous\n"
);

extern ULong lahf_0 ( void );
asm("\n"
".text\n"
"lahf_0:\n"
"\tpushq $0\n"
"\tpopfq\n"
"\tmovabsq $0x0, %rax\n"
"\tmovabsq $0x8765432112345678, %rdx\n"
"\tsubq %rax, %rdx\n"
"\t.byte 0x9F\n" /* lahf */
"\tret\n"
".previous\n"
);

extern ULong sahf_then_lahf ( ULong );
asm("\n"
".text\n"
"sahf_then_lahf:\n"
"\tmovq %rdi, %rax\n"
"\t.byte 0x9E\n" /* sahf */
"\tmovabsq $0, %rax\n"
"\t.byte 0x9F\n" /* lahf */
"\tret\n"
".previous\n"
);

int main ( void )
{
   ULong i;
   printf("0x%llx\n", lahf_0());
   printf("0x%llx\n", lahf_1());
   for (i = 0; i < 255; i++) {
      ULong x = sahf_then_lahf(i << 8);
      printf("%llx -> %llx\n", i, x);
   }
   return 0;
}
