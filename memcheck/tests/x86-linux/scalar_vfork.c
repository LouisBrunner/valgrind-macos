#include "scalar.h"

int main(void)
{
   int res __attribute__((unused));
   
   // All __NR_xxx numbers are taken from x86
   
   // __NR_vfork 190 --> arch/sys_fork()  [we can't use sys_vfork()]
   GO(__NR_vfork, "0e");
   SY(__NR_vfork);

   return(0);
}

