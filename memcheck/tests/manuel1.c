#include <stdio.h>

int main ()
{
  int x;

  printf ("x = %d\n", x==0xDEADBEEF ? 99 : 88);

  return 0;
}
