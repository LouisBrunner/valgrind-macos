
#include <stdio.h>

extern void test_printer ( int );

int main ( int argc, char** argv )
{
  int i;
  printf("begin ...\n");
  for (i = 0; i < 4; i++)
    test_printer(i);
  printf("end.\n");
  return 0;
}
