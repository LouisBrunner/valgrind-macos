/* -*- mode: C; c-basic-offset: 3; -*- */

/*---------------------------------------------------------------*/
/*--- begin                              libvex_guest_s390x.h ---*/
/*---------------------------------------------------------------*/

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

#ifndef __LIBVEX_PUB_GUEST_S390X_H
#define __LIBVEX_PUB_GUEST_S390X_H

#include "libvex_basictypes.h"
#include "libvex_emwarn.h"

/*------------------------------------------------------------*/
/*--- Vex's representation of the s390 CPU state.          ---*/
/*------------------------------------------------------------*/

typedef struct {

/*------------------------------------------------------------*/
/*--- ar registers                                         ---*/
/*------------------------------------------------------------*/

   /*    0 */  UInt guest_a0;
   /*    4 */  UInt guest_a1;
   /*    8 */  UInt guest_a2;
   /*   12 */  UInt guest_a3;
   /*   16 */  UInt guest_a4;
   /*   20 */  UInt guest_a5;
   /*   24 */  UInt guest_a6;
   /*   28 */  UInt guest_a7;
   /*   32 */  UInt guest_a8;
   /*   36 */  UInt guest_a9;
   /*   40 */  UInt guest_a10;
   /*   44 */  UInt guest_a11;
   /*   48 */  UInt guest_a12;
   /*   52 */  UInt guest_a13;
   /*   56 */  UInt guest_a14;
   /*   60 */  UInt guest_a15;

/*------------------------------------------------------------*/
/*--- fpr registers                                        ---*/
/*------------------------------------------------------------*/

   /*   64 */  ULong guest_f0;
   /*   72 */  ULong guest_f1;
   /*   80 */  ULong guest_f2;
   /*   88 */  ULong guest_f3;
   /*   96 */  ULong guest_f4;
   /*  104 */  ULong guest_f5;
   /*  112 */  ULong guest_f6;
   /*  120 */  ULong guest_f7;
   /*  128 */  ULong guest_f8;
   /*  136 */  ULong guest_f9;
   /*  144 */  ULong guest_f10;
   /*  152 */  ULong guest_f11;
   /*  160 */  ULong guest_f12;
   /*  168 */  ULong guest_f13;
   /*  176 */  ULong guest_f14;
   /*  184 */  ULong guest_f15;

/*------------------------------------------------------------*/
/*--- gpr registers                                        ---*/
/*------------------------------------------------------------*/

   /*  192 */  ULong guest_r0;
   /*  200 */  ULong guest_r1;
   /*  208 */  ULong guest_r2;
   /*  216 */  ULong guest_r3;
   /*  224 */  ULong guest_r4;
   /*  232 */  ULong guest_r5;
   /*  240 */  ULong guest_r6;
   /*  248 */  ULong guest_r7;
   /*  256 */  ULong guest_r8;
   /*  264 */  ULong guest_r9;
   /*  272 */  ULong guest_r10;
   /*  280 */  ULong guest_r11;
   /*  288 */  ULong guest_r12;
   /*  296 */  ULong guest_r13;
   /*  304 */  ULong guest_r14;
   /*  312 */  ULong guest_r15;

/*------------------------------------------------------------*/
/*--- S390 miscellaneous registers                         ---*/
/*------------------------------------------------------------*/

   /*  320 */  ULong guest_counter;
   /*  328 */  UInt guest_fpc;
   /*  332 */  UChar unused[4]; /* 4-byte hole to get 8-byte alignment */
   /*  336 */  ULong guest_IA;

/*------------------------------------------------------------*/
/*--- S390 pseudo registers                                ---*/
/*------------------------------------------------------------*/

   /*  344 */  ULong guest_SYSNO;

/*------------------------------------------------------------*/
/*--- 4-word thunk used to calculate the condition code    ---*/
/*------------------------------------------------------------*/

   /*  352 */  ULong guest_CC_OP;
   /*  360 */  ULong guest_CC_DEP1;
   /*  368 */  ULong guest_CC_DEP2;
   /*  376 */  ULong guest_CC_NDEP;

/*------------------------------------------------------------*/
/*--- Pseudo registers. Required by all architectures      ---*/
/*------------------------------------------------------------*/

   /* See comments at bottom of libvex.h */
   /*  384 */  ULong guest_NRADDR;
   /*  392 */  ULong guest_TISTART;
   /*  400 */  ULong guest_TILEN;

   /* Used when backing up to restart a syscall that has
      been interrupted by a signal. See also comment in
      libvex_ir.h */
   /*  408 */  ULong guest_IP_AT_SYSCALL;

   /* Emulation warnings; see comments in libvex_emwarn.h */
   /*  416 */  UInt guest_EMWARN;

/*------------------------------------------------------------*/
/*--- Force alignment to 16 bytes                          ---*/
/*------------------------------------------------------------*/
   /*  420 */  UChar padding[12];

   /*  432 */  /* This is the size of the guest state */
} VexGuestS390XState;


/*------------------------------------------------------------*/
/*--- Function prototypes                                  ---*/
/*------------------------------------------------------------*/

void LibVEX_GuestS390X_initialise(VexGuestS390XState *);

/*------------------------------------------------------------*/
/*--- Dedicated registers                                  ---*/
/*------------------------------------------------------------*/

#define guest_LR guest_r14  /* Link register */
#define guest_SP guest_r15  /* Stack pointer */
#define guest_FP guest_r11  /* Frame pointer */

/*---------------------------------------------------------------*/
/*--- end                                libvex_guest_s390x.h ---*/
/*---------------------------------------------------------------*/

#endif /* __LIBVEX_PUB_GUEST_S390X_H */
