#include "scalar.h"

int main(void)
{
   long *px = malloc(2*sizeof(long));
   x0 = px[0];

   /* SYS___realpathat                   574 */
   GO(SYS___realpathat, " 5s 2m");
   SY(SYS___realpathat, x0+0xffff, x0, x0, x0+100, x0+2); FAIL;
   
   /* SYS___specialfd                    577 */
   GO(SYS___specialfd, "3s 1m");
   SY(SYS___specialfd, x0+0xf000, x0+1, x0+10); FAIL;

   return(0);
}

