
#include <stdlib.h>

void ddd ( char* x )
{
  free(x);
  free(x);
}

void ccc (char* x)
{
  ddd(x);
}

void bbb (char* x)
{
  ccc(x);
}

void aaa (char* x)
{
  bbb(x);
}

int main ( void )
{
   char* x = malloc(10);
   aaa(x);
   return 0;
}
