
#include <stdio.h>

int arg = 0;
int res = 0;

extern void loop_plain ( void );
asm("\n"
".text\n"
".globl loop_plain\n"
"loop_plain:\n"
"\tpushl %ecx\n"
"\tmovl $999, %eax\n"
"\tmovl arg, %ecx\n"

".Lmn123plain:\n"
"\tdecl %eax\n"
"\tdecl %eax\n"
"\tdecl %eax\n"
"\tloop .Lmn123plain\n"

"\tmovl %eax, res\n"
"\tpopl %ecx\n"
"\tret\n"
);

extern void loop_ne ( void );
asm("\n"
".text\n"
".globl loop_ne\n"
"loop_ne:\n"
"\tpushl %ecx\n"
"\tmovl $999, %eax\n"
"\tmovl arg, %ecx\n"

".Lmn123ne:\n"
"\tdecl %eax\n"
"\tdecl %eax\n"
"\tdecl %eax\n"
"\tloopne .Lmn123ne\n"

"\tmovl %eax, res\n"
"\tpopl %ecx\n"
"\tret\n"
);

extern void loop_e ( void );
asm("\n"
".text\n"
".globl loop_e\n"
"loop_e:\n"
"\tpushl %ecx\n"
"\tmovl $999, %eax\n"
"\tmovl arg, %ecx\n"

".Lmn123e:\n"
"\tdecl %eax\n"
"\tdecl %eax\n"
"\tdecl %eax\n"
/* invert the Z flag */
"\tpushfl\n"
"\txorl $64, 0(%esp)\n"
"\tpopfl\n"
"\tloope .Lmn123e\n"

"\tmovl %eax, res\n"
"\tpopl %ecx\n"
"\tret\n"
);

int main ( void )
{
   res = 0; arg = 10;  loop_plain(); printf("res = %d\n", res);

   res = 0; arg = 10;  loop_ne();    printf("res = %d\n", res);
   res = 0; arg = 500; loop_ne();    printf("res = %d\n", res);

   res = 0; arg = 10;  loop_e();    printf("res = %d\n", res);
   res = 0; arg = 500; loop_e();    printf("res = %d\n", res);

   return 0;
}
