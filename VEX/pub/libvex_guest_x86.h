
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (libvex_guest_x86.h) is                       ---*/
/*--- Copyright (c) 2004 OpenWorks LLP.  All rights reserved. ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

#ifndef __LIBVEX_PUB_GUEST_X86_H
#define __LIBVEX_PUB_GUEST_X86_H

#include "libvex_basictypes.h"

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

   FPUCW[15:0] is the FPU's control word.  FPUCW[31:16] is unused.

   FC3210 contains the C3, C2, C1 and C0 bits in the same place they
   are in the FPU's status word.  (bits 14, 10, 9, 8 respectively).
   All other bits should be zero.  The relevant mask to select just
   those bits is 0x4700.  To select C3, C2 and C0 only, the mask is
   0x4500.
*/

typedef
   struct {
      UInt  guest_EAX;
      UInt  guest_ECX;
      UInt  guest_EDX;
      UInt  guest_EBX;
      UInt  guest_ESP;
      UInt  guest_EBP;
      UInt  guest_ESI;
      UInt  guest_EDI;
      /* 3-word thunk used to calculate O S Z A C P flags. */
      UInt  guest_CC_OP;
      UInt  guest_CC_SRC;
      UInt  guest_CC_DST;
      /* The D flag is stored here, as either -1 or +1 */
      UInt  guest_DFLAG;
      /* EIP */
      UInt  guest_EIP;
      /* FPU */
      UInt  guest_FTOP;
      ULong guest_FPREG[8];
      UChar guest_FPTAG[8];
      UInt  guest_FPUCW;
      UInt  guest_FC3210;
   }
   VexGuestX86State;


/*---------------------------------------------------------------*/
/*--- Utility functions for x86 guest stuff.                  ---*/
/*---------------------------------------------------------------*/

/* Convert a saved x87 FPU image (as created by fsave) and write it
   into the supplied VexGuestX86State structure.  The non-FP parts of
   said structure are left unchanged.  
*/
extern 
void x87_to_vex ( /*IN*/UChar* x87_state, 
                  /*OUT*/VexGuestX86State* vex_state );

/* Extract from the supplied VexGuestX86State structure, an x87 FPU
   image. */
extern 
void vex_to_x87 ( /*IN*/VexGuestX86State* vex_state, 
                  /*OUT*/UChar* x87_state );


/* Given a 32-bit word containing native x86 %eflags values, set the
   eflag-related fields in the supplied VexGuestX86State accordingly.
   All other fields are left unchanged.  */

extern
void eflags_to_vex ( UInt eflags_native,
                     /*OUT*/VexGuestX86State* vex_state );

/* Extract from the supplied VexGuestX86State structure the
   corresponding native %eflags value. */

extern 
UInt vex_to_eflags ( /*IN*/VexGuestX86State* vex_state );


#endif /* ndef __LIBVEX_PUB_GUEST_X86_H */

/*---------------------------------------------------------------*/
/*---                                      libvex_guest_x86.h ---*/
/*---------------------------------------------------------------*/
