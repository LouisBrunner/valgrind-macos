#define _GNU_SOURCE

#include "../../memcheck.h"
#include "scalar.h"

int main(void)
{
   // uninitialised, but we know px[0] is 0x0
   long* px  = malloc(sizeof(long));
   long  x0  = px[0];
   long  res;

   // __NR_openat2 337
   GO(__NR_openat2, "4s 2m");
   SY(__NR_openat2, x0, x0+1, x0+1, x0); FAIL;
}

