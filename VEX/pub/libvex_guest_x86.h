
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (libvex_guest_x86.h) is                       ---*/
/*--- Copyright (c) 2004 OpenWorks LLP.  All rights reserved. ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of LibVEX, a library for dynamic binary
   instrumentation and translation.

   Copyright (C) 2004 OpenWorks, LLP.

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

#ifndef __LIBVEX_PUB_GUEST_X86_H
#define __LIBVEX_PUB_GUEST_X86_H

#include "libvex_basictypes.h"
#include "libvex_emwarn.h"


/*---------------------------------------------------------------*/
/*--- Vex's representation of the x86 CPU state.              ---*/
/*---------------------------------------------------------------*/

/* The integer parts should be pretty straightforward. */

/* Hmm, subregisters.  The simulated state is stored in memory in the
   host's byte ordering, so we can't say here what the offsets of %ax,
   %al, %ah etc are since that depends on the host's byte ordering,
   which we don't know. */

/* FPU.  For now, just simulate 8 64-bit registers, their tags, and
   the reg-stack top pointer, of which only the least significant
   three bits are relevant.

   The model is:
     F0 .. F7 are the 8 registers.  FTOP[2:0] contains the 
     index of the current 'stack top' -- pretty meaningless, but
     still.  FTOP is a 32-bit value.  FTOP[31:3] can be anything
     (not guaranteed to be zero).

     When a value is pushed onto the stack, ftop is first replaced by 
     (ftop-1) & 7, and then F[ftop] is assigned the value.

     When a value is popped off the stack, the value is read from
     F[ftop], and then ftop is replaced by (ftop+1) & 7.

     In general, a reference to a register ST(i) actually references
     F[ (ftop+i) & 7 ].

   FTAG0 .. FTAG0+7 are the tags.  Each is a byte, zero means empty,
   non-zero means non-empty.

   The general rule appears to be that a read or modify of a register
   gets a stack underflow fault if the register is empty.  A write of
   a register (only a write, not a modify) gets a stack overflow fault
   if the register is full.  Note that "over" vs "under" is pretty
   meaningless since the FP stack pointer can move around arbitrarily,
   so it's really just two different kinds of exceptions:
   register-empty and register full.

   Naturally Intel (in its infinite wisdom) has seen fit to throw in
   some ad-hoc inconsistencies to the fault-generation rules of the
   above para, just to complicate everything.  Known inconsistencies:

   * fxam can read a register in any state without taking an underflow
     fault.

   * fst from st(0) to st(i) does not take an overflow fault even if the
     destination is already full.

   FPROUND[1:0] is the FPU's notional rounding mode, encoded as per
   the IRRoundingMode type (see libvex_ir.h).  This just happens to be
   the Intel encoding.  Note carefully, the rounding mode is only
   observed on float-to-int conversions, and on float-to-float
   rounding, but not for general float-to-float operations, which are
   always rounded-to-nearest.

   Loads/stores of the FPU control word are faked accordingly -- on
   loads, everything except the rounding mode is ignored, and on
   stores, you get a vanilla control world (0x037F) with the rounding
   mode patched in.  Hence the only values you can get are 0x037F,
   0x077F, 0x0B7F or 0x0F7F.  Vex will emit an emulation warning if
   you try and load a control word which either (1) unmasks FP
   exceptions, or (2) changes the default (80-bit) precision.

   FC3210 contains the C3, C2, C1 and C0 bits in the same place they
   are in the FPU's status word.  (bits 14, 10, 9, 8 respectively).
   All other bits should be zero.  The relevant mask to select just
   those bits is 0x4700.  To select C3, C2 and C0 only, the mask is
   0x4500.  

   SSEROUND[1:0] is the SSE unit's notional rounding mode, encoded as
   per the IRRoundingMode type.  As with the FPU control word, the
   rounding mode is the only part of %MXCSR that Vex observes.  On
   storing %MXCSR, you will get a vanilla word (0x1F80) with the
   rounding mode patched in.  Hence the only values you will get are
   0x1F80, 0x3F80, 0x5F80 or 0x7F80.  Vex will emit an emulation
   warning if you try and load a control word which either (1) unmasks
   any exceptions, (2) sets FZ (flush-to-zero) to 1, or (3) sets DAZ
   (denormals-are-zeroes) to 1. */

typedef
   struct {
      UInt  guest_EAX;         /* 0 */
      UInt  guest_ECX;
      UInt  guest_EDX;
      UInt  guest_EBX;
      UInt  guest_ESP;
      UInt  guest_EBP;
      UInt  guest_ESI;
      UInt  guest_EDI;         /* 28 */
      /* 4-word thunk used to calculate O S Z A C P flags. */
      UInt  guest_CC_OP;       /* 32 */
      UInt  guest_CC_DEP1;
      UInt  guest_CC_DEP2;
      UInt  guest_CC_NDEP;     /* 44 */
      /* The D flag is stored here, encoded as either -1 or +1 */
      UInt  guest_DFLAG;       /* 48 */
      /* Bit 21 (ID) of eflags stored here, as either 0 or 1. */
      UInt  guest_IDFLAG;      /* 52 */
      /* EIP */
      UInt  guest_EIP;         /* 56 */
      /* FPU */
      UInt  guest_FTOP;        /* 60 */
      ULong guest_FPREG[8];    /* 64 */
      UChar guest_FPTAG[8];   /* 128 */
      UInt  guest_FPROUND;    /* 136 */
      UInt  guest_FC3210;     /* 140 */
      /* SSE */
      UInt  guest_SSEROUND;   /* 144 */
      U128  guest_XMM0;       /* 148 */
      U128  guest_XMM1;
      U128  guest_XMM2;
      U128  guest_XMM3;
      U128  guest_XMM4;
      U128  guest_XMM5;
      U128  guest_XMM6;
      U128  guest_XMM7;
      /* Segment registers. */
      UShort guest_CS;
      UShort guest_DS;
      UShort guest_ES;
      UShort guest_FS;
      UShort guest_GS;
      UShort guest_SS;
      /* Emulation warnings */
      UInt   guest_EMWARN;
      /* Padding to make it have an 8-aligned size */
      UInt   padding;
   }
   VexGuestX86State;



/*---------------------------------------------------------------*/
/*--- Utility functions for x86 guest stuff.                  ---*/
/*---------------------------------------------------------------*/


/* ALL THE FOLLOWING ARE VISIBLE TO LIBRARY CLIENT */


/* Initialise all guest x86 state.  The FPU is put in default mode. */
extern
void LibVEX_GuestX86_initialise ( /*OUT*/VexGuestX86State* vex_state );


/* Convert a saved x87 FPU image (as created by fsave) and write it
   into the supplied VexGuestX86State structure.  The non-FP parts of
   said structure are left unchanged.  May return an emulation warning
   value.
*/
extern 
VexEmWarn
     LibVEX_GuestX86_put_x87 ( /*IN*/UChar* x87_state, 
                               /*OUT*/VexGuestX86State* vex_state );

/* Extract from the supplied VexGuestX86State structure, an x87 FPU
   image. */
extern 
void LibVEX_GuestX86_get_x87 ( /*IN*/VexGuestX86State* vex_state, 
                               /*OUT*/UChar* x87_state );


/* Given a 32-bit word containing native x86 %eflags values, set the
   eflag-related fields in the supplied VexGuestX86State accordingly.
   All other fields are left unchanged.  */

extern
void LibVEX_GuestX86_put_eflags ( UInt eflags_native,
                                  /*OUT*/VexGuestX86State* vex_state );

/* Extract from the supplied VexGuestX86State structure the
   corresponding native %eflags value. */

extern 
UInt LibVEX_GuestX86_get_eflags ( /*IN*/VexGuestX86State* vex_state );


#endif /* ndef __LIBVEX_PUB_GUEST_X86_H */

/*---------------------------------------------------------------*/
/*---                                      libvex_guest_x86.h ---*/
/*---------------------------------------------------------------*/
