#include "scalar.h"

int main(void)
{
   // All __NR_xxx numbers are taken from x86
   
   // __NR_exit_group 252
   GO(__NR_exit_group, "1s 0m");
   SY(__NR_exit_group);

   return(0);
}

