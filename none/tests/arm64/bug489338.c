/*
cat fp-valgrind-test.c
gcc -g -O2 fp-valgrind-test.c -o fp-valgrind-test
./fp-valgrind-test
valgrind ./fp-valgrind-test
gdb -q --args ./fp-valgrind-test
disassemble main
q
*/

#include <stdio.h>
#include <math.h>

double value(int s)
{
  switch (s) {
    case 0:  return -322.500001; break;
    case 1:  return -322.5;       break;
    case 2:  return -322.499999; break;
    case 3:  return  322.499999; break;
    case 4:  return  322.5;       break;
    default: return  322.500001; break;
  }
}

int main()
{
  for (int i = 0; i < 6; i++) {
    volatile double a = value(i);
    int b = (int)round(a);
    printf("i=%d a=%f a=0x%llx b=%d\n", i, a, *(long long unsigned int*)&a, b);
  }
}
