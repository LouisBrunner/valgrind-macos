#include <stdio.h>
#include <string.h>

int main(int argc, char *argv[])
{
  char strval[100];
  int len = sprintf(strval, "%.7G", 10.);
  if(memchr(strval, '.', len))
     printf("IF len %d\n", len);
  else
     printf("ELSE len %d\n", len);
}

