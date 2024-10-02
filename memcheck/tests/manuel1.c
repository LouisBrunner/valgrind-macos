#include <stdio.h>

int main ()
{
  int x;

  if (x==0xCAFEBABE)
  {
    printf ("x = %d\n", 99);
  }
  else
  {
    printf ("x = %d\n", 88);
  }

  return 0;
}
