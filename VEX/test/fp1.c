
#include <stdio.h>

double a[10];

int main ( void ) 
{
  int i;
  double s;
  for (i = 0; i < 10; i++)
    a[i] = 11.11 * i;
  s = 0.0;
  for (i = 0; i < 10; i++)
    s += a[i];
  printf("result = %f\n", s);
  return 0;
}
