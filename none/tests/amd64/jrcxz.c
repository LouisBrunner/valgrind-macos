
#include <stdio.h>

typedef unsigned long long int ULong;

ULong arg, res;

extern void foo ( void );
asm("\n"
"foo:\n"
"\tpushq %rcx\n"

"\tmovq $0, %rax\n"
"\tmovq arg, %rcx\n"

"Lagain:\n"
"\taddq $177, %rax\n"
"\tdecq %rcx\n"
"\tjrcxz Lout\n"
"\tjmp Lagain\n"

"Lout:\n"
"\tmovq %rax, res\n"

"\tpopq %rcx\n"
"\tret\n"
);

int main ( void )
{
  arg = 100;
  res = 0;
  foo();
  printf("%lld\n", res);
  return 0;
}
