
#include <stdio.h>

double qq ( void )
{int i;
  long double a = 0.0, b = 0.0, c = 0.0, d = 0.0, e = 0.0, f = 0.0, g = 0.0, h = 0.0;
  for (i = 0; i < 10; i++) {
    a += 1.1; b += 1.2; c += 1.3; d += 1.4; e += 1.5; f += 1.6;
    g += 1.7; h += 1.8001;
  }
  return a+b+c+d+e+f+g+h;
}

int main ( void )
{
  double r = qq();
  printf("answer is %f %d\n", r, (int)r );
  return 0;
}
