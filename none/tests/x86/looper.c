#include "tests/asm.h"
#include <stdio.h>

int arg = 0;
int res = 0;

extern void loop_plain ( void );
asm("\n"
".text\n"
VG_SYM(loop_plain) ":\n"
"\tpushl %ecx\n"
"\tmovl $999, %eax\n"
"\tmovl " VG_SYM(arg) ", %ecx\n"

"0:\n"
"\tdecl %eax\n"
"\tdecl %eax\n"
"\tdecl %eax\n"
"\tloop 0b\n"

"\tmovl %eax, " VG_SYM(res) "\n"
"\tpopl %ecx\n"
"\tret\n"
".previous\n"
);

extern void loop_ne ( void );
asm("\n"
".text\n"
VG_SYM(loop_ne) ":\n"
"\tpushl %ecx\n"
"\tmovl $999, %eax\n"
"\tmovl " VG_SYM(arg) ", %ecx\n"

"0:\n"
"\tdecl %eax\n"
"\tdecl %eax\n"
"\tdecl %eax\n"
"\tloopne 0b\n"

"\tmovl %eax, " VG_SYM(res) "\n"
"\tpopl %ecx\n"
"\tret\n"
".previous\n"
);

extern void loop_e ( void );
asm("\n"
".text\n"
VG_SYM(loop_e) ":\n"
"\tpushl %ecx\n"
"\tmovl $999, %eax\n"
"\tmovl " VG_SYM(arg) ", %ecx\n"

"0:\n"
"\tdecl %eax\n"
"\tdecl %eax\n"
"\tdecl %eax\n"
/* invert the Z flag */
"\tpushfl\n"
"\txorl $64, 0(%esp)\n"
"\tpopfl\n"
"\tloope 0b\n"

"\tmovl %eax, " VG_SYM(res) "\n"
"\tpopl %ecx\n"
"\tret\n"
".previous\n"
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
