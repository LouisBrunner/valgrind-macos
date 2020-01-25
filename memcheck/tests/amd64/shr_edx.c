#include <stdio.h>
#include <stdlib.h>
typedef unsigned long long int ULong;

ULong data;
ULong result;


extern void shrl32_with_0x10 ( void );
asm("\n"
".text\n"
"shrl32_with_0x10:\n"
"\tpushq %rdx\n"
"\tmovq data, %rdx\n"
"\tshr $0x10, %edx\n"
"\tjne shrl32_with_0x10_jump\n"
"\tshrl32_with_0x10_cont:\n"
"\tmovq %rdx, result\n"
"\tpopq %rdx\n"
"\tret\n"
"\tshrl32_with_0x10_jump:\n"
"\tmov $0xdeaddead, %edx\n"
"\tjmp shrl32_with_0x10_cont\n"
".previous\n"
);


int main ( void )
{
  char *p;

  printf("\nshrl 0x10 with unitialised bits\n");
  ULong *notinitialised = malloc(sizeof(ULong)); // Not initialised data.
  data = *notinitialised;
  p = (char*) &data;
  p[0] = 0x11;
  // p[1] = 0x22;
  p[2] = 0x33;
  p[3] = 0x44;

  shrl32_with_0x10();

  printf("non zero jump on p[2..3] 0x%016llx\n", result);

  data = *notinitialised;
  p = (char*) &data;
  p[0] = 0x00;
  // p[1] = 0x00;
  p[2] = 0x00;
  p[3] = 0x00;

  shrl32_with_0x10();

  printf("zero jump on p[2..3] 0x%016llx\n", result);
  return 0;
}
