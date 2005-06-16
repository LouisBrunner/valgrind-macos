#include <string.h>
#include <stdio.h>
#include <stdlib.h>

// An issue here is that in glibc memcmp() and bcmp() are aliases.  Valgrind
// chooses the shorter name -- bcmp -- and reports that in the error
// message, even though memcmp() was called.  This is hard to avoid.
char *s1, *s2;
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

	
