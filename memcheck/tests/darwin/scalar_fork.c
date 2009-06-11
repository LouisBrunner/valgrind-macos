#include "scalar.h"

int main(void)
{
   int res;
   
   GO(__NR_fork, 2, "0e");
   SY(__NR_fork);

   return(0);
}

