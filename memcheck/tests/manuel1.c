#include <stdio.h>

int main ()
{
  int x;

  printf ("x = %d\n", x==0xCAFEBABE ? 99 : 88);

  return 0;
}
