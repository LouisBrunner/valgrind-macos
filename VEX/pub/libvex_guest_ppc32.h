
/*---------------------------------------------------------------*/
/*--- begin                              libvex_guest_ppc32.h ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2004-2010 OpenWorks LLP
      info@open-works.net

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

#ifndef __LIBVEX_PUB_GUEST_PPC32_H
#define __LIBVEX_PUB_GUEST_PPC32_H

#include "libvex_basictypes.h"
#include "libvex_emwarn.h"


/*---------------------------------------------------------------*/
/*--- Vex's representation of the PPC32 CPU state             ---*/
/*---------------------------------------------------------------*/

#define VEX_GUEST_PPC32_REDIR_STACK_SIZE (16/*entries*/ * 2/*words per entry*/)

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
      // IMPORTANT: the user of libvex must place the guest state so as
      // to ensure that guest_VR{0..31}, and any shadows thereof, are
      // 16-aligned.
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

      /* XER pieces */
      /* 908 */ UChar guest_XER_SO; /* in lsb */
      /* 909 */ UChar guest_XER_OV; /* in lsb */
      /* 910 */ UChar guest_XER_CA; /* in lsb */
      /* 911 */ UChar guest_XER_BC; /* all bits */

      /* CR pieces */
      /* 912 */ UChar guest_CR0_321; /* in [3:1] */
      /* 913 */ UChar guest_CR0_0;   /* in lsb */
      /* 914 */ UChar guest_CR1_321; /* in [3:1] */
      /* 915 */ UChar guest_CR1_0;   /* in lsb */
      /* 916 */ UChar guest_CR2_321; /* in [3:1] */
      /* 917 */ UChar guest_CR2_0;   /* in lsb */
      /* 918 */ UChar guest_CR3_321; /* in [3:1] */
      /* 919 */ UChar guest_CR3_0;   /* in lsb */
      /* 920 */ UChar guest_CR4_321; /* in [3:1] */
      /* 921 */ UChar guest_CR4_0;   /* in lsb */
      /* 922 */ UChar guest_CR5_321; /* in [3:1] */
      /* 923 */ UChar guest_CR5_0;   /* in lsb */
      /* 924 */ UChar guest_CR6_321; /* in [3:1] */
      /* 925 */ UChar guest_CR6_0;   /* in lsb */
      /* 926 */ UChar guest_CR7_321; /* in [3:1] */
      /* 927 */ UChar guest_CR7_0;   /* in lsb */

      /* FP Status & Control Register fields */
      /* 928 */ UInt guest_FPROUND; // FP Rounding Mode

      /* Vector Save/Restore Register */
      /* 932 */ UInt guest_VRSAVE;

      /* Vector Status and Control Register */
      /* 936 */ UInt guest_VSCR;

      /* Emulation warnings */
      /* 940 */ UInt guest_EMWARN;

      /* For icbi: record start and length of area to invalidate */
      /* 944 */ UInt guest_TISTART;
      /* 948 */ UInt guest_TILEN;

      /* Used to record the unredirected guest address at the start of
         a translation whose start has been redirected.  By reading
         this pseudo-register shortly afterwards, the translation can
         find out what the corresponding no-redirection address was.
         Note, this is only set for wrap-style redirects, not for
         replace-style ones. */
      /* 952 */ UInt guest_NRADDR;
      /* 956 */ UInt guest_NRADDR_GPR2; /* needed by aix */

     /* A grows-upwards stack for hidden saves/restores of LR and R2
        needed for function interception and wrapping on ppc32-aix5.
        A horrible hack.  REDIR_SP points to the highest live entry,
        and so starts at -1. */
      /* 960 */ UInt guest_REDIR_SP;
      /* 964 */ UInt guest_REDIR_STACK[VEX_GUEST_PPC32_REDIR_STACK_SIZE];

      /* Needed for AIX (but mandated for all guest architectures):
         CIA at the last SC insn.  Used when backing up to restart a
         syscall that has been interrupted by a signal. */
      /* ??? */ UInt guest_IP_AT_SYSCALL; 

      /* SPRG3, which AIUI is readonly in user space.  Needed for
         threading on AIX. */
      /* ??? */ UInt guest_SPRG3_RO;

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


/* Write the given native %CR value to the supplied VexGuestPPC32State
   structure. */
extern
void LibVEX_GuestPPC32_put_CR ( UInt cr_native,
                                /*OUT*/VexGuestPPC32State* vex_state );

/* Extract from the supplied VexGuestPPC32State structure the
   corresponding native %CR value. */
extern
UInt LibVEX_GuestPPC32_get_CR ( /*IN*/VexGuestPPC32State* vex_state );


/* Write the given native %XER value to the supplied VexGuestPPC32State
   structure. */
extern
void LibVEX_GuestPPC32_put_XER ( UInt xer_native,
                                 /*OUT*/VexGuestPPC32State* vex_state );

/* Extract from the supplied VexGuestPPC32State structure the
   corresponding native %XER value. */
extern
UInt LibVEX_GuestPPC32_get_XER ( /*IN*/VexGuestPPC32State* vex_state );

#endif /* ndef __LIBVEX_PUB_GUEST_PPC32_H */


/*---------------------------------------------------------------*/
/*---                                    libvex_guest_ppc32.h ---*/
/*---------------------------------------------------------------*/
