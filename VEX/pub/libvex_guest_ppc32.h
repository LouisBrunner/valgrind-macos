
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (libvex_guest_ppc32.h) is                     ---*/
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

#ifndef __LIBVEX_PUB_GUEST_PPC32_H
#define __LIBVEX_PUB_GUEST_PPC32_H

#include "libvex_basictypes.h"
#include "libvex_emwarn.h"


/*---------------------------------------------------------------*/
/*--- Vex's representation of the PPC32 CPU state             ---*/
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

      UInt guest_CIA;    // Current Instruction Address (no arch visible register)
//      UInt guest_NIA;    // Next Instruction Address (no arch visible register)
      UInt guest_LR;     // Link Register
      UInt guest_CTR;    // Count Register

      /* thunk used to calculate CR0 flags. */
      UChar guest_CC_OP;    // boolean: 0=> dep1=result 1=> dep1=flags
      UInt  guest_CC_DEP1;  // Result of last op | flags
      UChar guest_CC_DEP2;  // XER_SO

      // CR1:7: Used for 'compare' ops (bits 0:4 not used - thunk used for this)
      UInt guest_CR1to7;

      /* XER */
      UChar guest_XER_SO;  // Summary Overflow
      UChar guest_XER_OV;  // Overflow
      UChar guest_XER_CA;  // Carry
      UChar guest_XER_BC;  // Byte Count

      /* Emulation warnings */
      UInt guest_EMWARN;

      /* Padding to make it have an 8-aligned size */
//      UChar padding_1b1;
//      UChar padding_1b2;
//      UChar padding_1b3;
//      UInt  padding_4b;
   }
   VexGuestPPC32State;


/*---------------------------------------------------------------*/
/*--- Utility functions for PPC32 guest stuff.                ---*/
/*---------------------------------------------------------------*/

/* ALL THE FOLLOWING ARE VISIBLE TO LIBRARY CLIENT */

/* Initialise all guest PPC32 state. */

extern
void LibVEX_GuestPPC32_initialise ( /*OUT*/VexGuestPPC32State* vex_state );

/* Calculate the PPC32 flag state from the saved data. */

extern
UInt LibVEX_GuestPPC32_get_flags ( /*IN*/VexGuestPPC32State* vex_state );


#endif /* ndef __LIBVEX_PUB_GUEST_PPC32_H */


/*---------------------------------------------------------------*/
/*---                                    libvex_guest_ppc32.h ---*/
/*---------------------------------------------------------------*/
