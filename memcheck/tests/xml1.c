
#include <stdlib.h>
#include <stdio.h>

static void* return_arg(void* p);
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
  free(return_arg(&n));

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

/*
 * The only purpose of the function below is to make sure that gcc 4.4.x does
 * not print the following warning during the compilation of this test program:
 * warning: attempt to free a non-heap object
 */
static void* return_arg(void* p)
{
   return p;
}

