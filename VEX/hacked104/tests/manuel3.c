#include <stdio.h>
#include <malloc.h>

int main ()
{
  int *x, y;

  x = (int *) malloc (sizeof (int));

  y = *x == 173;

  printf ("x = %d\n", y);
}
