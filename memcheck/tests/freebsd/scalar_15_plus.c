#include "scalar.h"
#include <sys/mman.h>

int main(void)
{
   long *px = malloc(2*sizeof(long));
   x0 = px[0];

   /* SYS_kqueuex                        583 */
   GO(SYS_kqueuex, " 1s 0m");
   SY(SYS_kqueuex, x0+123); FAIL;

   /* SYS_membarrier                     584 */
   GO(SYS_membarrier, " 3s 0m");
   SY(SYS_membarrier, x0+123, x0+456, x0+789); FAIL;

   /* SYS_timerfd_create                 585 */
   GO(SYS_timerfd_create, " 2s 0m");
   SY(SYS_timerfd_create, x0+123, x0+23456); FAIL;

   /* SYS_timerfd_gettime                586 */
   GO(SYS_timerfd_gettime, " 2s 1m");
   SY(SYS_timerfd_gettime, x0+100, x0); FAIL;

   /* SYS_timerfd_settime                587 */
   GO(SYS_timerfd_settime, "3s 2m");
   SY(SYS_timerfd_settime, x0+321, x0, x0+10); FAIL;
   
   return(0);
}

