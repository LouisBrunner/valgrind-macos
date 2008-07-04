/** Simple regression test triggering the C++ operators new and delete. */

#include <stdio.h>

int main(int argc, char** argv)
{
  int zero = 0;
  int* p = new int;
  int* q = new int[733];
  delete[] q;
  delete p;

  q = new int[zero];
  delete q;

  fprintf(stderr, "Success.\n");

  return 0;
}
