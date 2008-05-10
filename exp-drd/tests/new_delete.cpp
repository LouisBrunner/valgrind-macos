/** Simple regression test triggering the C++ operators new and delete. */

#include <stdio.h>

int main(int argc, char** argv)
{
  int* p = new int;
  int* q = new int[733];
  delete[] q;
  delete p;

  fprintf(stderr, "Success.\n");

  return 0;
}
