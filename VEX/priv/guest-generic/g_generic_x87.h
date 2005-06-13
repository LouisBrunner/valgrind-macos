
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (guest-generic/g_generic_x87.h) is            ---*/
/*--- Copyright (c) 2005 OpenWorks LLP.  All rights reserved. ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of LibVEX, a library for dynamic binary
   instrumentation and translation.

   Copyright (C) 2004-2005 OpenWorks, LLP.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; Version 2 dated June 1991 of the
   license.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE, or liability
   for damages.  See the GNU General Public License for more details.

   Neither the names of the U.S. Department of Energy nor the
   University of California nor the names of its contributors may be
   used to endorse or promote products derived from this software
   without prior written permission.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
   USA.
*/

/* This file contains functions for doing some x87-specific
   operations.  Both the amd64 and x86 front ends (guests) indirectly
   call these functions via guest helper calls.  By putting them here,
   code duplication is avoided.  Some of these functions are tricky
   and hard to verify, so there is much to be said for only having one
   copy thereof.
*/

#ifndef __G_GENERIC_X87_H
#define __G_GENERIC_X87_H

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



#endif /* ndef __G_GENERIC_X87_H */

/*---------------------------------------------------------------*/
/*--- end                       guest-generic/h_generic_x87.h ---*/
/*---------------------------------------------------------------*/
