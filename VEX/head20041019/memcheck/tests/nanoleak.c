
#include <stdlib.h>

int main ( void )
{
  volatile int* a = malloc(1000);
  a[0] = 0;
  return a[0];
}
