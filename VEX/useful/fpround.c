
#include <stdio.h>

int main ( void )
{
  double d = 0.0;
  int i;
  for (i = 0; i < 30; i++) {
    printf("i = %d,  d = %f,  (int)d = %d\n", i, d, (int)d);
    d += 0.11;
  }
  return 0;
}
