
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (libvex_guest_amd64.h) is                     ---*/
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

#ifndef __LIBVEX_PUB_GUEST_AMD64_H
#define __LIBVEX_PUB_GUEST_AMD64_H

#include "libvex_basictypes.h"
#include "libvex_emwarn.h"


/*---------------------------------------------------------------*/
/*--- Vex's representation of the AMD64 CPU state.            ---*/
/*---------------------------------------------------------------*/

typedef
   struct {
      /*   0 */ ULong  guest_RAX;
      /*   8 */ ULong  guest_RCX;
      /*  16 */ ULong  guest_RDX;
      /*  24 */ ULong  guest_RBX;
      /*  32 */ ULong  guest_RSP;
      /*  40 */ ULong  guest_RBP;
      /*  48 */ ULong  guest_RSI;
      /*  56 */ ULong  guest_RDI;
      /*  64 */ ULong  guest_R8;
      /*  72 */ ULong  guest_R9;
      /*  80 */ ULong  guest_R10;
      /*  88 */ ULong  guest_R11;
      /*  96 */ ULong  guest_R12;
      /* 104 */ ULong  guest_R13;
      /* 112 */ ULong  guest_R14;
      /* 120 */ ULong  guest_R15;
      /* 4-word thunk used to calculate O S Z A C P flags. */
      /* 128 */ ULong  guest_CC_OP;
      /* 136 */ ULong  guest_CC_DEP1;
      /* 144 */ ULong  guest_CC_DEP2;
      /* 152 */ ULong  guest_CC_NDEP;
      /* The D flag is stored here, encoded as either -1 or +1 */
      /* 160 */ ULong  guest_DFLAG;       /* 48 */
      /* RIP */
      /* 168 */ ULong  guest_RIP;
      /* Probably a lot more stuff too. 
         D,ID flags
         16  128-bit SSE registers
         all the old x87 FPU gunk
         segment registers
      */

      /* Bit 21 (ID) of eflags stored here, as either 0 or 1. */
      ULong guest_IDFLAG;

      /* HACK to make tls on amd64-linux work.  %fs only ever seems to
         hold zero, and so guest_FS_ZERO holds the 64-bit offset
         associated with a %fs value of zero. */
      ULong guest_FS_ZERO;

      /* Emulation warnings */
      UInt   guest_EMWARN;
      /* Padding to make it have an 8-aligned size */
      UInt   padding;
   }
   VexGuestAMD64State;



/*---------------------------------------------------------------*/
/*--- Utility functions for amd64 guest stuff.                ---*/
/*---------------------------------------------------------------*/

/* ALL THE FOLLOWING ARE VISIBLE TO LIBRARY CLIENT */

/* Initialise all guest amd64 state.  The FPU is put in default
   mode. */
extern
void LibVEX_GuestAMD64_initialise ( /*OUT*/VexGuestAMD64State* vex_state );


/* Extract from the supplied VexGuestAMD64State structure the
   corresponding native %rflags value. */
extern 
ULong LibVEX_GuestAMD64_get_rflags ( /*IN*/VexGuestAMD64State* vex_state );


#if 0
/* Convert a saved x87 FPU image (as created by fsave) and write it
   into the supplied VexGuestX86State structure.  The non-FP parts of
   said structure are left unchanged.  
*/
extern 
void LibVEX_GuestX86_put_x87 ( /*IN*/UChar* x87_state, 
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

#endif /* 0 */

#endif /* ndef __LIBVEX_PUB_GUEST_AMD64_H */

/*---------------------------------------------------------------*/
/*---                                    libvex_guest_amd64.h ---*/
/*---------------------------------------------------------------*/
