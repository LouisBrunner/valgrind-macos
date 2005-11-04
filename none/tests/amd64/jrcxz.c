
#include <stdio.h>

typedef unsigned long long int ULong;
typedef unsigned           int UInt;

ULong arg64, res64;

extern void foo64 ( void );
asm("\n"
"foo64:\n"
"\tpushq %rcx\n"

"\tmovq $0, %rax\n"
"\tmovq arg64, %rcx\n"

"Lagain64:\n"
"\taddq $177, %rax\n"
"\tdecq %rcx\n"
"\tjrcxz Lout64\n"
"\tjmp Lagain64\n"

"Lout64:\n"
"\tmovq %rax, res64\n"

"\tpopq %rcx\n"
"\tret\n"
);


UInt arg32, res32;

extern void foo32 ( void );
asm("\n"
"foo32:\n"
"\tpushq %rcx\n"

"\tmovq $0, %rax\n"
"\tmovl arg32, %ecx\n"

"Lagain32:\n"
"\taddq $177, %rax\n"
"\tdecl %ecx\n"
"\tjecxz Lout32\n"
"\tjmp Lagain32\n"

"Lout32:\n"
"\tmovl %eax, res32\n"

"\tpopq %rcx\n"
"\tret\n"
);



int main ( void )
{
  arg64 = 100;
  res64 = 0;
  foo64();
  printf("%lld\n", res64);

  arg32 = 1234;
  res32 = 0;
  foo32();
  printf("%d\n", res32);

  return 0;
}
