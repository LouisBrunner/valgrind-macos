#include "scalar.h"

int main(void)
{
   // uninitialised, but we know px[0] is 0x0
   long* px  = malloc(sizeof(long));
   long  x0  = px[0];
   int   res;

   // All __NR_xxx numbers are taken from x86
   
   // __NR_exit_group 252
   GO(__NR_exit_group, "1s 0m");
   SY(__NR_exit_group, x0);

   return(0);
}

