#include <assert.h>
#include <malloc.h>
#include <stdio.h>
#include <stdlib.h>


int main(int argc, char** argv)
{
  struct mallinfo mi;
  int allocated, allocated1, allocated2;
  void* p;

  fprintf(stderr,
          "Test of mallinfo(). May not trigger any assertion failures.\n");

  mi = mallinfo();
  assert(mi.arena == mi.uordblks + mi.fordblks);
  allocated1 = mi.arena + mi.hblkhd;
  allocated = 128*4096;
  p = malloc(allocated);
  mi = mallinfo();
  assert(mi.arena == mi.uordblks + mi.fordblks);
  allocated2 = mi.arena + mi.hblkhd;
  assert(allocated2 - allocated1 >= allocated);
  free(p);

  return 0;
}
