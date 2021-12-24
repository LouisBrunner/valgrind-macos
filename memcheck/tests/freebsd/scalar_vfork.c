#include "scalar.h"

int main(void)
{
   /* SYS_vfork                    66 */
   GO(SYS_vfork, "0s 0m");
   SY(SYS_vfork);

   return(0);
}

