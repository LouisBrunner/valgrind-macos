
/*---------------------------------------------------------------*/
/*--- begin                                host_amd64_maddf.h ---*/
/*---------------------------------------------------------------*/

/*
   Compute x * y + z as ternary operation with intrinsics
*/

/* Generic helper functions for doing FMA, i.e. compute x * y + z
   as ternary operation.
   These are purely back-end entities and cannot be seen/referenced
   from IR. */

#ifndef __VEX_HOST_AMD64_MADDF_H
#define __VEX_HOST_AMD64_MADDF_H

#include "libvex_basictypes.h"

#if defined(VGA_amd64)
extern VEX_REGPARM(3)
       void h_amd64_calc_MAddF32_fma4 ( /*OUT*/Float*, Float*, Float*, Float* );

extern VEX_REGPARM(3)
       void h_amd64_calc_MAddF64_fma4 ( /*OUT*/Double*, Double*, Double*,
                                        Double* );
#endif
#endif /* ndef __VEX_HOST_AMD64_MADDF_H */

/*---------------------------------------------------------------*/
/*--- end                                   host_amd64_maddf.h --*/
/*---------------------------------------------------------------*/
