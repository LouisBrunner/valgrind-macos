#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// This program returns 0 if executing on ppc64 big endian; otherwise returns 1

int main(void)
{
#if defined(VGP_ppc64be_linux)
   return 0;
#else
   return 1;
#endif
}
