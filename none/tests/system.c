#include <stdlib.h>

// This wasn't working at one point, because glibc used a variant of clone()
// to implement system(), which Valgrind didn't accept.

int main(void)
{
   system("../../tests/true");
   return 0;
}
