#include <stdlib.h>
#include <stdio.h>
#include "pub_tool_basics.h"

int main(void)
{
   // The n*size multiplication overflows in this example.  The only sensible
   // thing to do is return NULL, but old versions of Valgrind didn't (they
   // often ground to a halt trying to allocate an enormous (but not as
   // enormous as asked-for!) block.  See bug 149878.
   int* x;
#if VG_WORDSIZE == 8
   size_t szB = 0x1000000010000001ULL;
#else
   size_t szB = 0x10000001UL;
#endif
   x = calloc(szB, 0x10);
   fprintf(stderr, "x = %#lx\n", (long)x);
   return 0;
}
