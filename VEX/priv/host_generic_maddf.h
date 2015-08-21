
/*---------------------------------------------------------------*/
/*--- begin                              host_generic_maddf.h ---*/
/*---------------------------------------------------------------*/

/* 
   Compute x * y + z as ternary operation.
   Copyright (C) 2010-2015 Free Software Foundation, Inc.
   This file is part of the GNU C Library.
   Contributed by Jakub Jelinek <jakub@redhat.com>, 2010.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.
*/

/* Generic helper functions for doing FMA, i.e. compute x * y + z
   as ternary operation.
   These are purely back-end entities and cannot be seen/referenced
   from IR. */

#ifndef __VEX_HOST_GENERIC_MADDF_H
#define __VEX_HOST_GENERIC_MADDF_H

#include "libvex_basictypes.h"

extern VEX_REGPARM(3)
       void h_generic_calc_MAddF32 ( /*OUT*/Float*, Float*, Float*, Float* );

extern VEX_REGPARM(3)
       void h_generic_calc_MAddF64 ( /*OUT*/Double*, Double*, Double*,
                                     Double* );

#endif /* ndef __VEX_HOST_GENERIC_MADDF_H */

/*---------------------------------------------------------------*/
/*--- end                                 host_generic_maddf.h --*/
/*---------------------------------------------------------------*/
