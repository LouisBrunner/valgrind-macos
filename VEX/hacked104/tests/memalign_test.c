
#include <stdlib.h>
#include <stdio.h>

int main ( void )
{
  void* a[10];
  int i;
  for (i = 0; i < 10; i++) {
    a[i] = valloc(11111 * (i+1));
    //    printf("I acquire %p\n", a[i]);
  }
  for (i = 0; i < 10; i++) {
    //    printf("I release %p\n", a[i]);
    free(a[i]);
  }
  free(a[9]);
  return 0;
}
