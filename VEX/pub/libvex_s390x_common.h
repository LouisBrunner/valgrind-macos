/* -*- mode: C; c-basic-offset: 3; -*- */

/*--------------------------------------------------------------------*/
/*--- Common defs for s390x                  libvex_s390x_common.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright IBM Corp. 2010-2017

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.
*/

#ifndef __LIBVEX_PUB_S390X_H
#define __LIBVEX_PUB_S390X_H

/* This file includes definitions for s390.

   It must be suitable for inclusion in assembler source files. */


/*--------------------------------------------------------------*/
/*--- Dedicated registers                                    ---*/
/*--------------------------------------------------------------*/

#define S390_REGNO_RETURN_VALUE         2
#define S390_REGNO_TCHAIN_SCRATCH      12
#define S390_REGNO_GUEST_STATE_POINTER 13
#define S390_REGNO_LINK_REGISTER       14
#define S390_REGNO_STACK_POINTER       15


/*--------------------------------------------------------------*/
/*--- Offsets in the stack frame allocated by the dispatcher ---*/
/*--------------------------------------------------------------*/

/* Dispatcher will save 8 FPRs at offsets 160 + 0 ... 160 + 56 */

/* Where the dispatcher saves the r2 contents. */
#define S390_OFFSET_SAVED_R2 160+72

/* Where valgrind's FPC register is saved. */
#define S390_OFFSET_SAVED_FPC_V 160+64

/* Size of frame allocated by VG_(disp_run_translations)
   Need size for
       8 FPRs
     + 1 GPR  (SAVED_R2)
     + 1 FPC  (SAVED_FPC_V)

   Additionally, we need a standard frame for helper functions being called
   from client code. (See figure 1-16 in zSeries ABI) */
#define S390_INNERLOOP_FRAME_SIZE ((8+1+1)*8 + 160)


/*--------------------------------------------------------------*/
/*--- Extensions                                             ---*/
/*--------------------------------------------------------------*/

/* The extension ID is stored in the low 16 bits of the guest_SYSNO pseudo
   register. */
#define S390_EXT_ID_NBITS 16

#define S390_EXT_PRNO    1
#define S390_EXT_NNPA    2
#define S390_EXT_DFLT    3
#define S390_EXT_STFLE   4
#define S390_EXT_KM      5
#define S390_EXT_KMC     6
#define S390_EXT_KIMD    7
#define S390_EXT_KLMD    8
#define S390_EXT_KMAC    9
#define S390_EXT_PCC    10
#define S390_EXT_KMCTR  11
#define S390_EXT_KMO    12
#define S390_EXT_KMF    13
#define S390_EXT_KMA    14
#define S390_EXT_KDSA   15

/*--------------------------------------------------------------*/
/*--- Miscellaneous                                          ---*/
/*--------------------------------------------------------------*/

/* Number of arguments that can be passed in registers */
#define S390_NUM_GPRPARMS 5

/* Number of double words needed to store all facility bits. */
#define S390_NUM_FACILITY_DW 4

#endif /* __LIBVEX_PUB_S390X_H */

/*--------------------------------------------------------------------*/
/*--- end                                    libvex_s390x_common.h ---*/
/*--------------------------------------------------------------------*/
