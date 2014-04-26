
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

void ok_to_suppress_double_free_from_this_fun(char* y)
{
  aaa(y);
}

int main ( int argc, char*argv[] )
{
   char* x = malloc(10);
   char* y = malloc(10);
   if (argc > 1)
      ok_to_suppress_double_free_from_this_fun(y);
   aaa(x);
   return 0;
}
