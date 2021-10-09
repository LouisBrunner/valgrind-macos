#include "scalar.h"

int main(void)
{
   long* px  = malloc(sizeof(long));
   x0  = px[0];

   /* SYS_thr_exit                2 */
   GO(SYS_thr_exit, "1s 1m");
   SY(SYS_thr_exit, x0+1); FAIL;

   return(0);
}

