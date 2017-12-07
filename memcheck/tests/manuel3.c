#include <stdio.h>
#include <stdlib.h>
int foo, bar;
#define CBAR do { __asm__ __volatile__("":::"cc","memory"); } while (0)
int* gcc_cant_inline_me ( int );
int main ()
{
  int *x, y;

  x = (int *) malloc (sizeof (int));

  y = *x == 173;

  if (gcc_cant_inline_me(y) == &foo) { CBAR; } else { CBAR; }

  return 0;
}

/* must be AFTER main */
__attribute__((noinline)) int* gcc_cant_inline_me ( int n )
{
   if (n == 1)
      return &foo; /* foo! */
   else
      return &bar; /* bar! */
}


