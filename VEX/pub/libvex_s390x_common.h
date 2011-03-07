
/*--------------------------------------------------------------------*/
/*--- Common defs for s390x                  libvex_s390x_common.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright IBM Corp. 2010-2011

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

/* -*- mode: C; c-basic-offset: 3; -*- */

#ifndef __LIBVEX_PUB_S390X_H
#define __LIBVEX_PUB_S390X_H

/* This file includes definitions for s390.

   It must be suitable for inclusion in assembler source files. */


/*--------------------------------------------------------------*/
/*--- Dedicated registers                                    ---*/
/*--------------------------------------------------------------*/

#define S390_REGNO_RETURN_VALUE         2
#define S390_REGNO_DISPATCH_CTR        12   /* Holds VG_(dispatch_ctr) */
#define S390_REGNO_GUEST_STATE_POINTER 13
#define S390_REGNO_LINK_REGISTER       14
#define S390_REGNO_STACK_POINTER       15


/*--------------------------------------------------------------*/
/*--- Offsets in the stack frame allocated by the dispatcher ---*/
/*--------------------------------------------------------------*/

/* Where client's FPC register is saved. */
#define S390_OFFSET_SAVED_FPC_C 160+88

/* Where valgrind's FPC register is saved. */
#define S390_OFFSET_SAVED_FPC_V 160+80

/* Where client code will save the link register before calling a helper. */
#define S390_OFFSET_SAVED_LR 160+72

/* Location of saved guest state pointer */
#define S390_OFFSET_SAVED_GSP 160+64

/* Size of frame allocated by VG_(run_innerloop)
   Need size for
       8 FPRs
     + 2 GPRs (SAVED_GSP and SAVED_LR)
     + 2 FPCs (SAVED_FPC_C and SAVED_FPC_V).

   Additionally, we need a standard frame for helper functions being called
   from client code. (See figure 1-16 in zSeries ABI) */
#define S390_INNERLOOP_FRAME_SIZE ((8+2+2)*8 + 160)


/*--------------------------------------------------------------*/
/*--- Miscellaneous                                          ---*/
/*--------------------------------------------------------------*/

/* Number of arguments that can be passed in registers */
#define S390_NUM_GPRPARMS 5

#endif /* __LIBVEX_PUB_S390X_H */

/*--------------------------------------------------------------------*/
/*--- end                                    libvex_s390x_common.h ---*/
/*--------------------------------------------------------------------*/
