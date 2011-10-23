
/*---------------------------------------------------------------*/
/*--- begin                               guest_generic_x87.h ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2004-2011 OpenWorks LLP
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
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
   02110-1301, USA.

   The GNU General Public License is contained in the file COPYING.

   Neither the names of the U.S. Department of Energy nor the
   University of California nor the names of its contributors may be
   used to endorse or promote products derived from this software
   without prior written permission.
*/

/* This file contains functions for doing some x87-specific
   operations.  Both the amd64 and x86 front ends (guests) indirectly
   call these functions via guest helper calls.  By putting them here,
   code duplication is avoided.  Some of these functions are tricky
   and hard to verify, so there is much to be said for only having one
   copy thereof.
*/

#ifndef __VEX_GUEST_GENERIC_X87_H
#define __VEX_GUEST_GENERIC_X87_H

#include "libvex_basictypes.h"


/* Convert an IEEE754 double (64-bit) into an x87 extended double
   (80-bit), mimicing the hardware fairly closely.  Both numbers are
   stored little-endian.  Limitations, all of which could be fixed,
   given some level of hassle:

   * Identity of NaNs is not preserved.

   See comments in the code for more details.
*/
extern
void convert_f64le_to_f80le ( /*IN*/UChar* f64, /*OUT*/UChar* f80 );


/* Convert an x87 extended double (80-bit) into an IEEE 754 double
   (64-bit), mimicking the hardware fairly closely.  Both numbers are
   stored little-endian.  Limitations, both of which could be fixed,
   given some level of hassle:

   * Rounding following truncation could be a bit better.

   * Identity of NaNs is not preserved.

   See comments in the code for more details.
*/
extern
void convert_f80le_to_f64le ( /*IN*/UChar* f80, /*OUT*/UChar* f64 );


/* Layout of the real x87 state. */
typedef
   struct {
      UShort env[14];
      UChar  reg[80];
   }
   Fpu_State;

/* Offsets, in 16-bit ints, into the FPU environment (env) area. */
#define FP_ENV_CTRL   0
#define FP_ENV_STAT   2
#define FP_ENV_TAG    4
#define FP_ENV_IP     6 /* and 7 */
#define FP_ENV_CS     8
#define FP_ENV_OPOFF  10 /* and 11 */
#define FP_ENV_OPSEL  12
#define FP_REG(ii)    (10*(7-(ii)))


/* Do the computations for x86/amd64 FXTRACT.  Called directly from
   generated code.  CLEAN HELPER. */
extern ULong x86amd64g_calculate_FXTRACT ( ULong arg, HWord getExp );

/* Compute result and new OSZACP flags for all PCMP{E,I}STR{I,M}
   variants.  See bigger comment on implementation of this function
   for details on call/return conventions. */
extern Bool compute_PCMPxSTRx ( /*OUT*/V128* resV,
                                /*OUT*/UInt* resOSZACP,
                                V128* argLV,  V128* argRV,
                                UInt zmaskL, UInt zmaskR,
                                UInt imm8,   Bool isxSTRM );

#endif /* ndef __VEX_GUEST_GENERIC_X87_H */

/*---------------------------------------------------------------*/
/*--- end                                 guest_generic_x87.h ---*/
/*---------------------------------------------------------------*/
