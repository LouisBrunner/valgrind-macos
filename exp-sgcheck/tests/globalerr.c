
#include <stdio.h>

short a[7];
static short b[7];

int main ( void )
{
  int i;
  short sum;
  for (i = 0; i < 7+1; i++) {
     sum += a[i] * b[i];
  }
  return 1 & ((unsigned int)sum / 1000000);
}
