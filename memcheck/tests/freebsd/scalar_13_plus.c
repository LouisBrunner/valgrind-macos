#include "scalar.h"

int main(void)
{
   long *px = malloc(2*sizeof(long));
   x0 = px[0];

   
   /* SYS___specialfd                    574 */
   GO(SYS___specialfd, "3s 1m");
   SY(SYS___specialfd, x0+0xf000, x0+1, x0+10); FAIL;

   return(0);
}

