
/* Anyone know what this is supposed to test?  I don't
   (JRS 2003-07-07) -- which ain't good considering I
   probably created it :)
*/

#include <stdio.h>
#include <stdlib.h>

int main ( void )
{
   int i;
   char* p = malloc(1);
   for (i = 2; i < 50; i++) {
      p = realloc(p, i);
      p[i-1] = 'z';
   }
   return 0;
}
