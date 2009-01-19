#include "scalar.h"

int main(void)
{
   int res;
   
   // All __NR_xxx numbers are taken from x86
   
   // __NR_fork 2 --> arch/sys_fork()
   GO(__NR_fork, "0e");
   SY(__NR_fork);

   return(0);
}

