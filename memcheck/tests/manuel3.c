#include <stdio.h>
#include <stdlib.h>

int gcc_cant_inline_me ( int );

int main ()
{
  int *x, y;

  x = (int *) malloc (sizeof (int));

  y = *x == 173;

  if (gcc_cant_inline_me(y)) { } 

  return 0;
}

/* must be AFTER main */
int gcc_cant_inline_me ( int n )
{
   if (n == 42) 
      return 1; /* forty-two, dudes! */
   else
      return 0; /* some other number, dudes! */
}


