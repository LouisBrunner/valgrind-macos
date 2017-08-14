
/*---------------------------------------------------------------*/
/*--- begin                             libvex_guest_mips64.h ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2010-2015 RT-RK
      mips-valgrind@rt-rk.com

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

#ifndef __LIBVEX_PUB_GUEST_MIPS64_H
#define __LIBVEX_PUB_GUEST_MIPS64_H

#include "libvex_basictypes.h"
#include "libvex_emnote.h"


/*---------------------------------------------------------------*/
/*--- Vex's representation of the MIPS64 CPU state.           ---*/
/*---------------------------------------------------------------*/

typedef
   struct {
      /* CPU Registers */
      /*   0 */ ULong guest_r0;   /* Hardwired to 0 */
      /*   8 */ ULong guest_r1;   /* Assembler temporary */
      /*   16 */ ULong guest_r2;  /* Values for function returns ...*/
      /*   24 */ ULong guest_r3;  /* ...and expression evaluation */
      /*   32 */ ULong guest_r4;  /* Function arguments */
      /*   40 */ ULong guest_r5;
      /*   48 */ ULong guest_r6;
      /*   56 */ ULong guest_r7;
      /*   64 */ ULong guest_r8;
      /*   72 */ ULong guest_r9;
      /*   80 */ ULong guest_r10;
      /*   88 */ ULong guest_r11;
      /*   96 */ ULong guest_r12;  /* Temporaries */
      /*   104 */ ULong guest_r13;
      /*   112 */ ULong guest_r14;
      /*   120 */ ULong guest_r15;
      /*   128 */ ULong guest_r16;  /* Saved temporaries */
      /*   136 */ ULong guest_r17;
      /*   144 */ ULong guest_r18;
      /*   152 */ ULong guest_r19;
      /*   160 */ ULong guest_r20;
      /*   168 */ ULong guest_r21;
      /*   176 */ ULong guest_r22;
      /*   184 */ ULong guest_r23;
      /*   192 */ ULong guest_r24;  /* Temporaries */
      /*   200 */ ULong guest_r25;
      /*   208 */ ULong guest_r26;  /* Reserved for OS kernel */
      /*   216 */ ULong guest_r27;
      /*   224 */ ULong guest_r28;  /* Global pointer */
      /*   232 */ ULong guest_r29;  /* Stack pointer */
      /*   240 */ ULong guest_r30;  /* Frame pointer */
      /*   248 */ ULong guest_r31;  /* Return address */
      /*   256 */ ULong guest_PC;   /* Program counter */
      /*   264 */ ULong guest_HI;   /* Multiply and divide reg higher result */
      /*   272 */ ULong guest_LO;   /* Multiply and divide reg lower result */

      /* FPU Registers */
      /*   280 */ ULong guest_f0;   /* Floting point gen purpose registers */
      /*   288 */ ULong guest_f1;
      /*   296 */ ULong guest_f2;
      /*   304 */ ULong guest_f3;
      /*   312 */ ULong guest_f4;
      /*   320 */ ULong guest_f5;
      /*   328 */ ULong guest_f6;
      /*   336 */ ULong guest_f7;
      /*   344 */ ULong guest_f8;
      /*   352 */ ULong guest_f9;
      /*   360 */ ULong guest_f10;
      /*   368 */ ULong guest_f11;
      /*   376 */ ULong guest_f12;
      /*   384 */ ULong guest_f13;
      /*   392 */ ULong guest_f14;
      /*   400 */ ULong guest_f15;
      /*   408 */ ULong guest_f16;
      /*   416 */ ULong guest_f17;
      /*   424 */ ULong guest_f18;
      /*   432 */ ULong guest_f19;
      /*   440 */ ULong guest_f20;
      /*   448 */ ULong guest_f21;
      /*   456 */ ULong guest_f22;
      /*   464 */ ULong guest_f23;
      /*   472 */ ULong guest_f24;
      /*   480 */ ULong guest_f25;
      /*   488 */ ULong guest_f26;
      /*   496 */ ULong guest_f27;
      /*   504 */ ULong guest_f28;
      /*   512 */ ULong guest_f29;
      /*   520 */ ULong guest_f30;
      /*   528 */ ULong guest_f31;

      /*   536 */ UInt guest_FIR;
      /*   540 */ UInt guest_FCCR;
      /*   544 */ UInt guest_FEXR;
      /*   548 */ UInt guest_FENR;
      /*   552 */ UInt guest_FCSR;

      /* TLS pointer for the thread. It's read-only in user space. On Linux it
         is set in user space by various thread-related syscalls.
         User Local Register.
         This register provides read access to the coprocessor 0
         UserLocal register, if it is implemented. In some operating
         environments, the UserLocal register is a pointer to a thread-specific
         storage block.
       */
        ULong guest_ULR;         /* 560 */

      /* Emulation notes */
        UInt guest_EMNOTE;       /* 568 */

      /* For clflush: record start and length of area to invalidate */
        ULong guest_CMSTART;     /* 576 */
        ULong guest_CMLEN;       /* 584 */

        ULong guest_NRADDR;      /* 592 */

        ULong host_EvC_FAILADDR; /* 600 */
        UInt host_EvC_COUNTER;   /* 608 */
        UInt guest_COND;         /* 612 */
        UInt padding[2];
} VexGuestMIPS64State;

/*---------------------------------------------------------------*/
/*--- Utility functions for MIPS64 guest stuff.               ---*/
/*---------------------------------------------------------------*/

/* ALL THE FOLLOWING ARE VISIBLE TO LIBRARY CLIENT */

/* Initialise all guest MIPS64 state. */

extern
void LibVEX_GuestMIPS64_initialise ( /*OUT*/VexGuestMIPS64State* vex_state );

#endif /* ndef __LIBVEX_PUB_GUEST_MIPS64_H */

/*---------------------------------------------------------------*/
/*---                                   libvex_guest_mips64.h ---*/
/*---------------------------------------------------------------*/

