
/*---------------------------------------------------------------*/
/*--- begin                             host_generic_simd256.h ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2012-2017 OpenWorks GbR
      info@open-works.net

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.
*/

/* Generic helper functions for doing 256-bit SIMD arithmetic in cases
   where the instruction selectors cannot generate code in-line.
   These are purely back-end entities and cannot be seen/referenced
   as clean helper functions from IR.

   These will get called from generated code and therefore should be
   well behaved -- no floating point or mmx insns, just straight
   integer code.

   Each function implements the correspondingly-named IR primop.
*/

#ifndef __VEX_HOST_GENERIC_SIMD256_H
#define __VEX_HOST_GENERIC_SIMD256_H

#include "libvex_basictypes.h"

extern VEX_REGPARM(3)
       void h_generic_calc_Perm32x8   ( /*OUT*/V256*, V256*, V256* );

#endif /* ndef __VEX_HOST_GENERIC_SIMD256_H */

/*---------------------------------------------------------------*/
/*--- end                              host_generic_simd256.h ---*/
/*---------------------------------------------------------------*/
