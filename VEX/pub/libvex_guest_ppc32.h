
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
      /* General Purpose Registers */
      /*   0 */ UInt guest_GPR0;
      /*   4 */ UInt guest_GPR1;
      /*   8 */ UInt guest_GPR2;
      /*  12 */ UInt guest_GPR3;
      /*  16 */ UInt guest_GPR4;
      /*  20 */ UInt guest_GPR5;
      /*  24 */ UInt guest_GPR6;
      /*  28 */ UInt guest_GPR7;
      /*  32 */ UInt guest_GPR8;
      /*  36 */ UInt guest_GPR9;
      /*  40 */ UInt guest_GPR10;
      /*  44 */ UInt guest_GPR11;
      /*  48 */ UInt guest_GPR12;
      /*  52 */ UInt guest_GPR13;
      /*  56 */ UInt guest_GPR14;
      /*  60 */ UInt guest_GPR15;
      /*  64 */ UInt guest_GPR16;
      /*  68 */ UInt guest_GPR17;
      /*  72 */ UInt guest_GPR18;
      /*  76 */ UInt guest_GPR19;
      /*  80 */ UInt guest_GPR20;
      /*  84 */ UInt guest_GPR21;
      /*  88 */ UInt guest_GPR22;
      /*  92 */ UInt guest_GPR23;
      /*  96 */ UInt guest_GPR24;
      /* 100 */ UInt guest_GPR25;
      /* 104 */ UInt guest_GPR26;
      /* 108 */ UInt guest_GPR27;
      /* 112 */ UInt guest_GPR28;
      /* 116 */ UInt guest_GPR29;
      /* 120 */ UInt guest_GPR30;
      /* 124 */ UInt guest_GPR31;

      // Floating Point Registers
      /* 128 */ ULong guest_FPR0;
      /* 136 */ ULong guest_FPR1;
      /* 144 */ ULong guest_FPR2;
      /* 152 */ ULong guest_FPR3;
      /* 160 */ ULong guest_FPR4;
      /* 168 */ ULong guest_FPR5;
      /* 176 */ ULong guest_FPR6;
      /* 184 */ ULong guest_FPR7;
      /* 192 */ ULong guest_FPR8;
      /* 200 */ ULong guest_FPR9;
      /* 208 */ ULong guest_FPR10;
      /* 216 */ ULong guest_FPR11;
      /* 224 */ ULong guest_FPR12;
      /* 232 */ ULong guest_FPR13;
      /* 240 */ ULong guest_FPR14;
      /* 248 */ ULong guest_FPR15;
      /* 256 */ ULong guest_FPR16;
      /* 264 */ ULong guest_FPR17;
      /* 272 */ ULong guest_FPR18;
      /* 280 */ ULong guest_FPR19;
      /* 288 */ ULong guest_FPR20;
      /* 296 */ ULong guest_FPR21;
      /* 304 */ ULong guest_FPR22;
      /* 312 */ ULong guest_FPR23;
      /* 320 */ ULong guest_FPR24;
      /* 328 */ ULong guest_FPR25;
      /* 336 */ ULong guest_FPR26;
      /* 344 */ ULong guest_FPR27;
      /* 352 */ ULong guest_FPR28;
      /* 360 */ ULong guest_FPR29;
      /* 368 */ ULong guest_FPR30;
      /* 376 */ ULong guest_FPR31;

      // Vector Registers
      /* 384 */ U128 guest_VR0;
      /* 400 */ U128 guest_VR1;
      /* 416 */ U128 guest_VR2;
      /* 432 */ U128 guest_VR3;
      /* 448 */ U128 guest_VR4;
      /* 464 */ U128 guest_VR5;
      /* 480 */ U128 guest_VR6;
      /* 496 */ U128 guest_VR7;
      /* 512 */ U128 guest_VR8;
      /* 528 */ U128 guest_VR9;
      /* 544 */ U128 guest_VR10;
      /* 560 */ U128 guest_VR11;
      /* 576 */ U128 guest_VR12;
      /* 592 */ U128 guest_VR13;
      /* 608 */ U128 guest_VR14;
      /* 624 */ U128 guest_VR15;
      /* 640 */ U128 guest_VR16;
      /* 656 */ U128 guest_VR17;
      /* 672 */ U128 guest_VR18;
      /* 688 */ U128 guest_VR19;
      /* 704 */ U128 guest_VR20;
      /* 720 */ U128 guest_VR21;
      /* 736 */ U128 guest_VR22;
      /* 752 */ U128 guest_VR23;
      /* 768 */ U128 guest_VR24;
      /* 784 */ U128 guest_VR25;
      /* 800 */ U128 guest_VR26;
      /* 816 */ U128 guest_VR27;
      /* 832 */ U128 guest_VR28;
      /* 848 */ U128 guest_VR29;
      /* 864 */ U128 guest_VR30;
      /* 880 */ U128 guest_VR31;

      /* 896 */ UInt guest_CIA;    // IP (no arch visible register)
      /* 900 */ UInt guest_LR;     // Link Register
      /* 904 */ UInt guest_CTR;    // Count Register

      /* CR[7]: thunk used to calculate these flags. */
      /* 908 */ UInt guest_CC_OP;
      /* 912 */ UInt guest_CC_DEP1;
      /* 916 */ UInt guest_CC_DEP2;

      // CR[0:6]: Used for 'compare' ops
      /* 920 */ UInt guest_CR0to6;

      /* FP Status & Control Register fields */
      /* 924 */ UInt guest_FPROUND; // FP Rounding Mode

      /* 928 */ UInt guest_XER;     // XER Register

      /* Vector Save/Restore Register */
      /* 932 */ UInt guest_VRSAVE;

      /* Vector Status and Control Register */
      /* 936 */ UInt guest_VSCR;

      /* Emulation warnings */
      /* 940 */ UInt guest_EMWARN;

      /* For icbi: record start and length of area to invalidate */
      /* 944 */ UInt guest_TISTART;
      /* 948 */ UInt guest_TILEN;

      /* Padding to make it have an 8-aligned size */
      /* UInt  padding; */
   }
   VexGuestPPC32State;


/*---------------------------------------------------------------*/
/*--- Utility functions for PPC32 guest stuff.                ---*/
/*---------------------------------------------------------------*/

/* ALL THE FOLLOWING ARE VISIBLE TO LIBRARY CLIENT */

/* Initialise all guest PPC32 state. */

extern
void LibVEX_GuestPPC32_initialise ( /*OUT*/VexGuestPPC32State* vex_state );

/* Write the given native %cr7 value to the supplied
   VexGuestPPC32State structure */
extern
void LibVEX_GuestPPC32_put_cr7 ( UInt cr7_native,
                                 /*OUT*/VexGuestPPC32State* vex_state );

/* Ditto, but for entire %cr */
extern
void LibVEX_GuestPPC32_put_cr ( UInt cr_native,
                                /*OUT*/VexGuestPPC32State* vex_state );

/* Extract from the supplied VexGuestPPC32State structure the
   corresponding native %cr7 value. */
extern
UInt LibVEX_GuestPPC32_get_cr7 ( /*IN*/VexGuestPPC32State* vex_state );

/* Ditto, but for entire %cr */
extern
UInt LibVEX_GuestPPC32_get_cr ( /*IN*/VexGuestPPC32State* vex_state );

#endif /* ndef __LIBVEX_PUB_GUEST_PPC32_H */


/*---------------------------------------------------------------*/
/*---                                    libvex_guest_ppc32.h ---*/
/*---------------------------------------------------------------*/
