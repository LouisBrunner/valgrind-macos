#include "tests/asm.h"
#include <stdio.h>

extern int foo_1 ( void );
asm("\n"
".text\n"
VG_SYM(foo_1) ":\n"
"\tpushl $0\n"
"\tpopfl\n"
"\tmovl $0xFFFFFFFF, %eax\n"
"\tmovl $0x12345678, %edx\n"
"\tsubl %eax, %edx\n"
"\tlahf\n"
"\tret\n"
".previous\n"
);

extern int foo_0 ( void );
asm("\n"
".text\n"
VG_SYM(foo_0) ":\n"
"\tpushl $0\n"
"\tpopfl\n"
"\tmovl $0x0, %eax\n"
"\tmovl $0x12345678, %edx\n"
"\tsubl %eax, %edx\n"
"\tlahf\n"
"\tret\n"
".previous\n"
);

int main ( void )
{
  printf("0x%x\n", foo_0());
  printf("0x%x\n", foo_1());
  return 0;
}
