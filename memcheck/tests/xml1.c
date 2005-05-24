
#include <stdlib.h>
#include <stdio.h>


int frame3 ( void )
{
  int *a = malloc(10 * sizeof(int));

  // bad address;
  int n = a[10];

  // undefined condition
  if (a[5] == 42) {
    printf("hello from frame3().  The answer is 42.\n");
  } else {
    printf("hello from frame3().  The answer is not 42.\n");
  }

  // undefined address (careful ..)
  n = a[  a[0] & 7  ];

  // invalid free, the second time
  free(a);
  free(a);

  // more invalid frees
  free(&n);

  // leak ..
  a = malloc(99 * sizeof(int));

  // pass garbage to the exit syscall
  return n;
}

int frame2 ( void )
{
  return frame3() - 1;
}

int frame1 ( void )
{
  return frame2() + 1;
}

int main ( void )
{
  return frame1() - 1;
}
