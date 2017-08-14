/* -*- mode: C; c-basic-offset: 3; -*- */

/*--------------------------------------------------------------------*/
/*--- Common defs for s390x                  libvex_s390x_common.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright IBM Corp. 2010-2012

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

/* Where the dispatcher saves the r2 contents. */
#define S390_OFFSET_SAVED_R2 160+96

/* Where client's FPC register is saved. */
#define S390_OFFSET_SAVED_FPC_C 160+88

/* Where valgrind's FPC register is saved. */
#define S390_OFFSET_SAVED_FPC_V 160+80

/* Where client code will save the link register before calling a helper. */
#define S390_OFFSET_SAVED_LR 160+72

/* Size of frame allocated by VG_(disp_run_translations)
   Need size for
       8 FPRs
     + 2 GPRs (SAVED_LR, and SAVED_R2)
     + 2 FPCs (SAVED_FPC_C and SAVED_FPC_V).

   Additionally, we need a standard frame for helper functions being called
   from client code. (See figure 1-16 in zSeries ABI) */
#define S390_INNERLOOP_FRAME_SIZE ((8+2+2)*8 + 160)


/*--------------------------------------------------------------*/
/*--- Miscellaneous                                          ---*/
/*--------------------------------------------------------------*/

/* Number of arguments that can be passed in registers */
#define S390_NUM_GPRPARMS 5

/* Number of double words needed to store all facility bits. */
#define S390_NUM_FACILITY_DW 2

#endif /* __LIBVEX_PUB_S390X_H */

/*--------------------------------------------------------------------*/
/*--- end                                    libvex_s390x_common.h ---*/
/*--------------------------------------------------------------------*/
