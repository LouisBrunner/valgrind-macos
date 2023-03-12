#include <stdlib.h>

int main()
{
  int* fpointer = (int*)malloc(10);
  delete fpointer;          // should give warning (or two if sized delete is used)
  fpointer = (int*)malloc(10);
  delete [] fpointer;       // should give warning
  fpointer = (int*)malloc(10);
  free (fpointer);          // should work!

  int* nvec = new int[10];
  delete nvec;              // should give a warning (or two if sized delete is used)
  nvec = new int[10];
  free (nvec);              // should give a warning
  nvec = new int[10];
  delete [] nvec;           // should work!

  int* n = new int;
  delete [] n;              // should give a warning
  n = new int;
  free(n);                  // should give a warning
  n = new int;
  delete n;                 // should work!

  return 0;
}
