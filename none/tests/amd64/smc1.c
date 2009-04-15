
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
#include <assert.h>
#include "tests/sys_mman.h"

typedef unsigned long long int Addr;
typedef unsigned char UChar;

void q ( int n )
{
   printf("in q %d\n", n);
}

void p ( int n )
{
   printf("in p %d\n", n);
}

// Unlike on x86, data areas aren't executable; have to put
// code on the heap therefore
static UChar* code;

/* Make `code' be  movabsq $dest, %rax ; pushq %rax ; ret */
// This forces the branch onwards to be indirect, so vex can't chase it
void set_dest ( Addr dest )
{
   assert(sizeof(Addr) == 8);

   /* movabsq $imm64, %rax */
   code[0] = 0x48;
   code[1] = 0xB8;
   code[2] = (dest & 0xFF);
   code[3] = ((dest >>  8) & 0xFF);
   code[4] = ((dest >> 16) & 0xFF);
   code[5] = ((dest >> 24) & 0xFF);
   code[6] = ((dest >> 32) & 0xFF);
   code[7] = ((dest >> 40) & 0xFF);
   code[8] = ((dest >> 48) & 0xFF);
   code[9] = ((dest >> 56) & 0xFF);

   /* pushq %rax */
   code[10] = 0x50;

   /* ret */
   code[11] = 0xC3;
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
   code = mmap(NULL, 20, PROT_READ|PROT_WRITE|PROT_EXEC,
               MAP_PRIVATE|MAP_ANONYMOUS, -1, 0);
   assert(code != MAP_FAILED);
   for (i = 0; i < 10; i += 2) {
      set_dest ( (Addr)&p );
      //      diversion();
      aa(i);
      set_dest ( (Addr)&q );
      //      diversion();
      aa(i+1);
   }
   munmap(code, 20);
   return 0;
}
