
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (libvex_guest_ppc.h) is                       ---*/
/*--- Copyright (c) 2005 OpenWorks LLP.  All rights reserved. ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of LibVEX, a library for dynamic binary
   instrumentation and translation.

   Copyright (C) 2005 OpenWorks, LLP.

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

#ifndef __LIBVEX_PUB_GUEST_PPC_H
#define __LIBVEX_PUB_GUEST_PPC_H

#include "libvex_basictypes.h"
#include "libvex_emwarn.h"


/*---------------------------------------------------------------*/
/*--- Vex's representation of the PPC CPU state (32 bit)      ---*/
/*---------------------------------------------------------------*/

typedef
   struct {
      // General Purpose Registers
      UInt guest_GPR0;
      UInt guest_GPR1;
      UInt guest_GPR2;
      UInt guest_GPR3;
      UInt guest_GPR4;
      UInt guest_GPR5;
      UInt guest_GPR6;
      UInt guest_GPR7;
      UInt guest_GPR8;
      UInt guest_GPR9;
      UInt guest_GPR10;
      UInt guest_GPR11;
      UInt guest_GPR12;
      UInt guest_GPR13;
      UInt guest_GPR14;
      UInt guest_GPR15;
      UInt guest_GPR16;
      UInt guest_GPR17;
      UInt guest_GPR18;
      UInt guest_GPR19;
      UInt guest_GPR20;
      UInt guest_GPR21;
      UInt guest_GPR22;
      UInt guest_GPR23;
      UInt guest_GPR24;
      UInt guest_GPR25;
      UInt guest_GPR26;
      UInt guest_GPR27;
      UInt guest_GPR28;
      UInt guest_GPR29;
      UInt guest_GPR30;
      UInt guest_GPR31;

      UInt guest_CIA;    // Current Instruction Address (no architecturally visible register)
      UInt guest_LR;     // Link Register
      UInt guest_CTR;    // Count Register

      /* CR0: Use last result for delayed calc of CR0[0,1,2] (neg,pos,zero)
        (bit3 is just a redundant copy of XER_SO) */
      UInt guest_Result; // Result of last op

      // CR1: Used for FP - don't need yet.
      // CR2:7: Used for 'compare' instructions
      UChar guest_CR2;
      UChar guest_CR3;
      UChar guest_CR4;
      UChar guest_CR5;
      UChar guest_CR6;
      UChar guest_CR7;

      /* XER */
      UChar guest_XER_SO;  // Summary Overflow
      UChar guest_XER_OV;  // Overflow
      UChar guest_XER_CA;  // Carry
//    UChar guest_XER_ByteCount;

      /* Emulation warnings */
      UInt guest_EMWARN;

      /* Padding to make it have an 8-aligned size */
      UChar padding_1b;
      UChar padding_1b;
      UChar padding_1b;
      UInt  padding_4b;
   }
   VexGuestPPCState;


/*---------------------------------------------------------------*/
/*--- Utility functions for PPC guest stuff.                  ---*/
/*---------------------------------------------------------------*/

/* ALL THE FOLLOWING ARE VISIBLE TO LIBRARY CLIENT */

/* Initialise all guest PPC state. */

extern
void LibVEX_GuestPPC_initialise ( /*OUT*/VexGuestPPCState* vex_state );

/* Calculate the PPC flag state from the saved data. */

extern
UInt LibVEX_GuestPPC_get_flags ( /*IN*/VexGuestPPCState* vex_state );


#endif /* ndef __LIBVEX_PUB_GUEST_PPC_H */


/*---------------------------------------------------------------*/
/*---                                      libvex_guest_ppc.h ---*/
/*---------------------------------------------------------------*/
