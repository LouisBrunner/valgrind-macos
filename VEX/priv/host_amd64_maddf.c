
/*---------------------------------------------------------------*/
/*--- begin                                host_amd64_maddf.c ---*/
/*---------------------------------------------------------------*/

/*
   Compute x * y + z as ternary operation with intrinsics.
*/


#include "libvex_basictypes.h"
#include "host_amd64_maddf.h"

#if defined(VGA_amd64)
void VEX_REGPARM(3)
     h_amd64_calc_MAddF32_fma4 ( /*OUT*/Float* res,
                            Float* argX, Float* argY, Float* argZ )
{
   __asm__ ("vfmaddss %3,%2,%1,%0;" :
            "=x"(*res): "x"(*argX),"x"(*argY), "x"(*argZ));
   return ;
}

void VEX_REGPARM(3)
     h_amd64_calc_MAddF64_fma4 ( /*OUT*/Double* res,
                            Double* argX, Double* argY, Double* argZ )
{
   __asm__ ("vfmaddsd %3,%2,%1,%0;" :
            "=x"(*res): "x"(*argX),"x"(*argY), "x"(*argZ));
   return;
}
#endif
/*---------------------------------------------------------------*/
/*--- end                                   host_amd64_maddf.c --*/
/*---------------------------------------------------------------*/
