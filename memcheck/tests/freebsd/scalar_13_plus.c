#include "scalar.h"
#include <sys/mman.h>

int main(void)
{
   long *px = malloc(2*sizeof(long));
   x0 = px[0];

   /* SYS_shm_open2                      571 */
   GO(SYS_shm_open2, " 5s 2m");
   SY(SYS_shm_open2, x0+0xf00c, x0+1, x0+2, x0+3, x0+4); FAIL;
   
   GO(SYS_shm_open2, " 5s 1m");
   SY(SYS_shm_open2, x0+SHM_ANON, x0+1, x0+2, x0+3, x0+4); FAIL;

   /* SYS___realpathat                   574 */
   GO(SYS___realpathat, " 5s 2m");
   SY(SYS___realpathat, x0+0xffff, x0, x0, x0+100, x0+2); FAIL;

   /* SYS_close_range                    575 */
   GO(SYS_close_range, "3s 0m");
   SY(SYS_close_range, x0+5, x0+10, x0); SUCC;
   
   /* SYS___specialfd                    577 */
   GO(SYS___specialfd, "3s 1m");
   SY(SYS___specialfd, x0+0xf000, x0+1, x0+10); FAIL;
 
   /* SYS_aio_writev                     578 */
   GO(SYS_aio_writev, "1s 1m");
   SY(SYS_aio_writev, x0+1); FAIL;
   
   /* SYS_aio_readv                      579 */
   GO(SYS_aio_readv, "1s 1m");
   SY(SYS_aio_readv, x0+1); FAIL;

   return(0);
}

