
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (libvex_guest_arm.h) is                       ---*/
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

#ifndef __LIBVEX_PUB_GUEST_ARM_H
#define __LIBVEX_PUB_GUEST_ARM_H

#include "libvex_basictypes.h"
#include "libvex_emwarn.h"


/*---------------------------------------------------------------*/
/*--- Vex's representation of the ARM CPU state.              ---*/
/*---------------------------------------------------------------*/

/* R13 traditionally used as the stack pointer ? */

typedef
   struct {
      UInt  guest_R0;
      UInt  guest_R1;
      UInt  guest_R2;
      UInt  guest_R3;
      UInt  guest_R4;
      UInt  guest_R5;
      UInt  guest_R6;
      UInt  guest_R7;
      UInt  guest_R8;
      UInt  guest_R9;
      UInt  guest_R10;
      UInt  guest_R11;
      UInt  guest_R12;

      /* aka the stack pointer */
      UInt  guest_R13;

      /* aka the link register */
      UInt  guest_R14; 

      /* Program counter. */
      UInt  guest_R15;

      /* System call number copied in here from swi insn literal
         field. */
      UInt  guest_SYSCALLNO;

      /* 3-word thunk used to calculate N(sign) Z(zero) C(carry,
         unsigned overflow) and V(signed overflow) flags. */
      UInt  guest_CC_OP;
      UInt  guest_CC_DEP1;
      UInt  guest_CC_DEP2;

      /* Emulation warnings */
      UInt   guest_EMWARN;

      /* Padding to make it have an 8-aligned size */
      UInt   padding;
   }
   VexGuestARMState;


/*---------------------------------------------------------------*/
/*--- Utility functions for ARM guest stuff.                  ---*/
/*---------------------------------------------------------------*/

/* ALL THE FOLLOWING ARE VISIBLE TO LIBRARY CLIENT */

/* Initialise all guest ARM state. */

extern
void LibVEX_GuestARM_initialise ( /*OUT*/VexGuestARMState* vex_state );

/* Calculate the ARM flag state from the saved data. */

extern
UInt LibVEX_GuestARM_get_flags ( /*IN*/VexGuestARMState* vex_state );


#endif /* ndef __LIBVEX_PUB_GUEST_ARM_H */


/*---------------------------------------------------------------*/
/*---                                      libvex_guest_arm.h ---*/
/*---------------------------------------------------------------*/
