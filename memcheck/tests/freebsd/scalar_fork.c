#include "scalar.h"

int main(void)
{
   /* SYS_fork                    2 */
   GO(SYS_fork, "0s 0m");
   SY(SYS_fork);

   return(0);
}

