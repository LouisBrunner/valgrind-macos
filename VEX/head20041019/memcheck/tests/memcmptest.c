
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

char* s1;
char* s2;

int main ( void )
{
  s1 = malloc(10); strcpy(s1,"fooble");
  s2 = malloc(10); strcpy(s2,"fooble");
  if (memcmp(s1, s2, 8) != 0)
    printf("different\n");
  else
    printf("same (?!)\n");
  return 0;
}

	
