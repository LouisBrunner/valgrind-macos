#include "scalar.h"

int main(void)
{
   int res;
   
   // __NR_vfork --> __NR_fork  [we can't use vfork()]
   GO(__NR_vfork, 66, "0e");
   SY(__NR_vfork);

   return(0);
}

