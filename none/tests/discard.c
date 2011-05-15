
#include <stdio.h>
#include "../../include/valgrind.h"




int fooble ( void )
{
  int x, y;
  y = 0;
  for (x = 0; x < 100; x++) {
    if ((x % 3) == 0) y += x; else y++;
  }
  return y;
}

void someother ( void )
{
}

int main ( void )
{
  printf("fooble-1() = %d\n", fooble() );
  (void)VALGRIND_DISCARD_TRANSLATIONS( (char*)(&fooble), 
          ((char*)(&someother)) - ((char*)(&fooble)) );
  printf("fooble-2() = %d\n", fooble() );
  return 0;
}

