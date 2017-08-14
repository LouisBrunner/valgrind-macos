
#include <stdio.h>

int main ( void )
{
double d =  7.25063790881233303e-303;
 int i;

 for (i = 0; i < 26; i++) {
			    printf("%.16e\n", d);
			    d *= 0.1012198489248992489422;
 }
  return 0;
}

