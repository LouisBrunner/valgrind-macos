
/* Test Heimdall's ability to spot writes to code which has been
   translated, and discard the out-of-date translations.

   CORRECT output is

      in p 0
      in q 1
      in p 2
      in q 3
      in p 4
      in q 5
      in p 6
      in q 7
      in p 8
      in q 9

  WRONG output (if you fail to spot code-writes to code[0 .. 4]) is

      in p 0
      in p 1
      in p 2
      in p 3
      in p 4
      in p 5
      in p 6
      in p 7
      in p 8
      in p 9
*/

#include <stdio.h>
#include "valgrind.h"

typedef unsigned int Addr;
typedef unsigned char UChar;

void q ( int n )
{
   printf("in q %d\n", n);
}

void p ( int n )
{
   printf("in p %d\n", n);
}

UChar code[10];

/* Make `code' be JMP-32 dest */
void set_dest ( Addr dest )
{
   unsigned int delta;
   delta = dest - ((Addr)(&code[0]));
   delta -= 5;
   
   code[0] = 0xE9;   /* JMP d32 */
   code[1] = (delta & 0xFF);
   code[2] = ((delta >> 8) & 0xFF);
   code[3] = ((delta >> 16) & 0xFF);
   code[4] = ((delta >> 24) & 0xFF);

   /* XXX this should be automatic */
   VALGRIND_DISCARD_TRANSLATIONS(code, sizeof(code));
}

int main ( void )
{
   int i;
   for (i = 0; i < 10; i += 2) {
      set_dest ( (Addr)&p );
      (  (void (*)(int)) (&code[0])  ) (i);
      set_dest ( (Addr)&q );
      (  (void (*)(int)) (&code[0])  ) (i+1);
   }
   return 0;
}
