#include "scalar.h"

int main(void)
{
   long* px  = malloc(sizeof(long));
   x0  = px[0];

   /* SYS_abort2                  2 */
   GO(SYS_abort2, "3s 2m");
   SY(SYS_abort2, x0+1, x0+2, x0+1); FAIL;

   return(0);
}

