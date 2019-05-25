
/*---------------------------------------------------------------*/
/*--- begin                            host_generic_simd256.c ---*/
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
   from IR. */

#include "libvex_basictypes.h"
#include "host_generic_simd256.h"


void VEX_REGPARM(3)
     h_generic_calc_Perm32x8 ( /*OUT*/V256* res,
                               V256* argL, V256* argR )
{
   res->w32[0] = argL->w32[ argR->w32[0] & 7 ];
   res->w32[1] = argL->w32[ argR->w32[1] & 7 ];
   res->w32[2] = argL->w32[ argR->w32[2] & 7 ];
   res->w32[3] = argL->w32[ argR->w32[3] & 7 ];
   res->w32[4] = argL->w32[ argR->w32[4] & 7 ];
   res->w32[5] = argL->w32[ argR->w32[5] & 7 ];
   res->w32[6] = argL->w32[ argR->w32[6] & 7 ];
   res->w32[7] = argL->w32[ argR->w32[7] & 7 ];
}


/*---------------------------------------------------------------*/
/*--- end                              host_generic_simd256.c ---*/
/*---------------------------------------------------------------*/
