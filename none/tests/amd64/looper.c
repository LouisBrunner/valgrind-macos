
#include <stdio.h>

long long int arg = 0;
long long int res = 0;

extern void loop_plain ( void );
asm("\n"
".text\n"
".globl loop_plain\n"
"loop_plain:\n"
"\tpushq %rcx\n"
"\tmovq $999, %rax\n"
"\tmovq arg, %rcx\n"

".Lmn123plain:\n"
"\tdecq %rax\n"
"\tdecq %rax\n"
"\tdecq %rax\n"
"\tloop .Lmn123plain\n"

"\tmovq %rax, res\n"
"\tpopq %rcx\n"
"\tret\n"
);

extern void loop_ne ( void );
asm("\n"
".text\n"
".globl loop_ne\n"
"loop_ne:\n"
"\tpushq %rcx\n"
"\tmovq $999, %rax\n"
"\tmovq arg, %rcx\n"

".Lmn123ne:\n"
"\tdecq %rax\n"
"\tdecq %rax\n"
"\tdecq %rax\n"
"\tloopne .Lmn123ne\n"

"\tmovq %rax, res\n"
"\tpopq %rcx\n"
"\tret\n"
);

extern void loop_e ( void );
asm("\n"
".text\n"
".globl loop_e\n"
"loop_e:\n"
"\tpushq %rcx\n"
"\tmovq $999, %rax\n"
"\tmovq arg, %rcx\n"

".Lmn123e:\n"
"\tdecq %rax\n"
"\tdecq %rax\n"
"\tdecq %rax\n"
/* invert the Z flag */
"\tpushfq\n"
"\txorq $64, 0(%rsp)\n"
"\tpopfq\n"
"\tloope .Lmn123e\n"

"\tmovq %rax, res\n"
"\tpopq %rcx\n"
"\tret\n"
);

int main ( void )
{
   res = 0; arg = 10;  loop_plain(); printf("res = %lld\n", res);

   res = 0; arg = 10;  loop_ne();    printf("res = %lld\n", res);
   res = 0; arg = 500; loop_ne();    printf("res = %lld\n", res);

   res = 0; arg = 10;  loop_e();     printf("res = %lld\n", res);
   res = 0; arg = 500; loop_e();     printf("res = %lld\n", res);

   return 0;
}
