
/* Test Valgrind's ability to spot writes to code which has been
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

static UChar code[10];

/* Make `code' be PUSHL $dest ; ret */
// This forces the branch onwards to be indirect, so vex can't chase it
void set_dest ( Addr dest )
{
   code[0] = 0x68; /* PUSH imm32 */
   code[1] = (dest & 0xFF);
   code[2] = ((dest >> 8) & 0xFF);
   code[3] = ((dest >> 16) & 0xFF);
   code[4] = ((dest >> 24) & 0xFF);
   code[5] = 0xC3;
}

/* Calling aa gets eventually to the function residing in code[0..].
   This indirection is necessary to defeat Vex's basic-block chasing
   optimisation.  That will merge up to three basic blocks into the
   same IR superblock, which causes the test to succeed when it
   shouldn't if main calls code[] directly.  */

// force an indirect branch to code[0], so vex can't chase it
__attribute__((noinline))
void dd ( int x, void (*f)(int) ) { f(x); }

__attribute__((noinline))
void cc ( int x ) { dd(x, (void(*)(int)) &code[0]); }

__attribute__((noinline))
void bb ( int x ) { cc(x); }

__attribute__((noinline))
void aa ( int x ) { bb(x); }

__attribute__((noinline))
void diversion ( void ) { }

int main ( void )
{
   int i;
   for (i = 0; i < 10; i += 2) {
      set_dest ( (Addr)&p );
      //      diversion();
      aa(i);
      set_dest ( (Addr)&q );
      //      diversion();
      aa(i+1);
   }
   return 0;
}
