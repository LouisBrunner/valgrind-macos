#include "../include/valgrind.h"
#include <stdlib.h>
#include <stdio.h>
int main ( int argc, char** argv )
{
  if (argc > 1) {
     char option[200];
     sprintf(option, "--suppressions=%s",  argv[1]);
     VALGRIND_CLO_CHANGE(option);
  }
  volatile int* a = malloc(1000);
  a[0] = 0;
  return a[0];
}
