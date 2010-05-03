
/*---------------------------------------------------------------*/
/*--- begin                              libvex_guest_ppc64.h ---*/
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

#ifndef __LIBVEX_PUB_GUEST_PPC64_H
#define __LIBVEX_PUB_GUEST_PPC64_H

#include "libvex_basictypes.h"
#include "libvex_emwarn.h"

/*
    volatile ==  caller-saved (not preserved across function calls)
non-volatile ==  callee-saved (preserved across function calls)

r0        Volatile register used in function prologs
r1        Stack frame pointer
r2        TOC pointer
r3        Volatile parameter and return value register
r4-r10    Volatile registers used for function parameters
r11       Volatile register used in calls by pointer and as an
          environment pointer for languages which require one
r12       Volatile register used for exception handling and glink code
r13       Reserved for use as system thread ID
r14-r31   Nonvolatile registers used for local variables

f0        Volatile scratch register
f1-f4     Volatile floating point parameter and return value registers
f5-f13    Volatile floating point parameter registers
f14-f31   Nonvolatile registers

LR        Link register (volatile)
CTR       Loop counter register (volatile)
XER       Fixed point exception register (volatile)
FPSCR     Floating point status and control register (volatile)

CR0-CR1   Volatile condition code register fields
CR2-CR4   Nonvolatile condition code register fields
CR5-CR7   Volatile condition code register fields

On processors with the VMX feature.

v0-v1     Volatile scratch registers
v2-v13    Volatile vector parameters registers
v14-v19   Volatile scratch registers
v20-v31   Non-volatile registers
vrsave    Non-volatile 32-bit register
*/


/*---------------------------------------------------------------*/
/*--- Vex's representation of the PPC64 CPU state             ---*/
/*---------------------------------------------------------------*/

#define VEX_GUEST_PPC64_REDIR_STACK_SIZE (16/*entries*/ * 2/*words per entry*/)

typedef
   struct {
      /* General Purpose Registers */
      /*   0 */ ULong guest_GPR0;
      /*   8 */ ULong guest_GPR1;
      /*  16 */ ULong guest_GPR2;
      /*  24 */ ULong guest_GPR3;
      /*  32 */ ULong guest_GPR4;
      /*  40 */ ULong guest_GPR5;
      /*  48 */ ULong guest_GPR6;
      /*  56 */ ULong guest_GPR7;
      /*  64 */ ULong guest_GPR8;
      /*  72 */ ULong guest_GPR9;
      /*  80 */ ULong guest_GPR10;
      /*  88 */ ULong guest_GPR11;
      /*  96 */ ULong guest_GPR12;
      /* 104 */ ULong guest_GPR13;
      /* 112 */ ULong guest_GPR14;
      /* 120 */ ULong guest_GPR15;
      /* 128 */ ULong guest_GPR16;
      /* 136 */ ULong guest_GPR17;
      /* 144 */ ULong guest_GPR18;
      /* 152 */ ULong guest_GPR19;
      /* 160 */ ULong guest_GPR20;
      /* 168 */ ULong guest_GPR21;
      /* 176 */ ULong guest_GPR22;
      /* 184 */ ULong guest_GPR23;
      /* 192 */ ULong guest_GPR24;
      /* 200 */ ULong guest_GPR25;
      /* 208 */ ULong guest_GPR26;
      /* 216 */ ULong guest_GPR27;
      /* 224 */ ULong guest_GPR28;
      /* 232 */ ULong guest_GPR29;
      /* 240 */ ULong guest_GPR30;
      /* 248 */ ULong guest_GPR31;

      // Floating Point Registers
      /* 256 */ ULong guest_FPR0;
      /* 264 */ ULong guest_FPR1;
      /* 272 */ ULong guest_FPR2;
      /* 280 */ ULong guest_FPR3;
      /* 288 */ ULong guest_FPR4;
      /* 296 */ ULong guest_FPR5;
      /* 304 */ ULong guest_FPR6;
      /* 312 */ ULong guest_FPR7;
      /* 320 */ ULong guest_FPR8;
      /* 328 */ ULong guest_FPR9;
      /* 336 */ ULong guest_FPR10;
      /* 344 */ ULong guest_FPR11;
      /* 352 */ ULong guest_FPR12;
      /* 360 */ ULong guest_FPR13;
      /* 368 */ ULong guest_FPR14;
      /* 376 */ ULong guest_FPR15;
      /* 384 */ ULong guest_FPR16;
      /* 392 */ ULong guest_FPR17;
      /* 400 */ ULong guest_FPR18;
      /* 408 */ ULong guest_FPR19;
      /* 416 */ ULong guest_FPR20;
      /* 424 */ ULong guest_FPR21;
      /* 432 */ ULong guest_FPR22;
      /* 440 */ ULong guest_FPR23;
      /* 448 */ ULong guest_FPR24;
      /* 456 */ ULong guest_FPR25;
      /* 464 */ ULong guest_FPR26;
      /* 472 */ ULong guest_FPR27;
      /* 480 */ ULong guest_FPR28;
      /* 488 */ ULong guest_FPR29;
      /* 496 */ ULong guest_FPR30;
      /* 504 */ ULong guest_FPR31;

      // Vector Registers
      // IMPORTANT: the user of libvex must place the guest state so as
      // to ensure that guest_VR{0..31}, and any shadows thereof, are
      // 16-aligned.
      /*  512 */ U128 guest_VR0;
      /*  528 */ U128 guest_VR1;
      /*  544 */ U128 guest_VR2;
      /*  560 */ U128 guest_VR3;
      /*  576 */ U128 guest_VR4;
      /*  592 */ U128 guest_VR5;
      /*  608 */ U128 guest_VR6;
      /*  624 */ U128 guest_VR7;
      /*  640 */ U128 guest_VR8;
      /*  656 */ U128 guest_VR9;
      /*  672 */ U128 guest_VR10;
      /*  688 */ U128 guest_VR11;
      /*  704 */ U128 guest_VR12;
      /*  720 */ U128 guest_VR13;
      /*  736 */ U128 guest_VR14;
      /*  752 */ U128 guest_VR15;
      /*  768 */ U128 guest_VR16;
      /*  784 */ U128 guest_VR17;
      /*  800 */ U128 guest_VR18;
      /*  816 */ U128 guest_VR19;
      /*  832 */ U128 guest_VR20;
      /*  848 */ U128 guest_VR21;
      /*  864 */ U128 guest_VR22;
      /*  880 */ U128 guest_VR23;
      /*  896 */ U128 guest_VR24;
      /*  912 */ U128 guest_VR25;
      /*  928 */ U128 guest_VR26;
      /*  944 */ U128 guest_VR27;
      /*  960 */ U128 guest_VR28;
      /*  976 */ U128 guest_VR29;
      /*  992 */ U128 guest_VR30;
      /* 1008 */ U128 guest_VR31;

      /* 1024 */ ULong guest_CIA;    // IP (no arch visible register)
      /* 1032 */ ULong guest_LR;     // Link Register
      /* 1040 */ ULong guest_CTR;    // Count Register

      /* XER pieces */
      /* 1048 */ UChar guest_XER_SO; /* in lsb */
      /* 1049 */ UChar guest_XER_OV; /* in lsb */
      /* 1050 */ UChar guest_XER_CA; /* in lsb */
      /* 1051 */ UChar guest_XER_BC; /* all bits */

      /* CR pieces */
      /* 1052 */ UChar guest_CR0_321; /* in [3:1] */
      /* 1053 */ UChar guest_CR0_0;   /* in lsb */
      /* 1054 */ UChar guest_CR1_321; /* in [3:1] */
      /* 1055 */ UChar guest_CR1_0;   /* in lsb */
      /* 1056 */ UChar guest_CR2_321; /* in [3:1] */
      /* 1057 */ UChar guest_CR2_0;   /* in lsb */
      /* 1058 */ UChar guest_CR3_321; /* in [3:1] */
      /* 1059 */ UChar guest_CR3_0;   /* in lsb */
      /* 1060 */ UChar guest_CR4_321; /* in [3:1] */
      /* 1061 */ UChar guest_CR4_0;   /* in lsb */
      /* 1062 */ UChar guest_CR5_321; /* in [3:1] */
      /* 1063 */ UChar guest_CR5_0;   /* in lsb */
      /* 1064 */ UChar guest_CR6_321; /* in [3:1] */
      /* 1065 */ UChar guest_CR6_0;   /* in lsb */
      /* 1066 */ UChar guest_CR7_321; /* in [3:1] */
      /* 1067 */ UChar guest_CR7_0;   /* in lsb */

      /* FP Status & Control Register fields */
      /* 1068 */ UInt guest_FPROUND; // FP Rounding Mode

      /* Vector Save/Restore Register */
      /* 1072 */ UInt guest_VRSAVE;

      /* Vector Status and Control Register */
      /* 1076 */ UInt guest_VSCR;

      /* Emulation warnings */
      /* 1080 */ UInt guest_EMWARN;

      /* gcc adds 4 bytes padding here: pre-empt it. */
      /* 1084 */ UInt  padding;

      /* For icbi: record start and length of area to invalidate */
      /* 1088 */ ULong guest_TISTART;
      /* 1096 */ ULong guest_TILEN;

      /* Used to record the unredirected guest address at the start of
         a translation whose start has been redirected.  By reading
         this pseudo-register shortly afterwards, the translation can
         find out what the corresponding no-redirection address was.
         Note, this is only set for wrap-style redirects, not for
         replace-style ones. */
      /* 1104 */ ULong guest_NRADDR;
      /* 1112 */ ULong guest_NRADDR_GPR2;

     /* A grows-upwards stack for hidden saves/restores of LR and R2
        needed for function interception and wrapping on ppc64-linux.
        A horrible hack.  REDIR_SP points to the highest live entry,
        and so starts at -1. */
      /* 1120 */ ULong guest_REDIR_SP;
      /* 1128 */ ULong guest_REDIR_STACK[VEX_GUEST_PPC64_REDIR_STACK_SIZE];

      /* Needed for AIX: CIA at the last SC insn.  Used when backing up
         to restart a syscall that has been interrupted by a signal. */
      /* 1384 */ ULong guest_IP_AT_SYSCALL; 

      /* SPRG3, which AIUI is readonly in user space.  Needed for
         threading on AIX. */
      /* ???? */ ULong guest_SPRG3_RO;

      /* Padding to make it have an 8-aligned size */
      /* ???? */ ULong padding2;
   }
   VexGuestPPC64State;


/*---------------------------------------------------------------*/
/*--- Utility functions for PPC64 guest stuff.                ---*/
/*---------------------------------------------------------------*/

/* ALL THE FOLLOWING ARE VISIBLE TO LIBRARY CLIENT */

/* Initialise all guest PPC64 state. */
extern
void LibVEX_GuestPPC64_initialise ( /*OUT*/VexGuestPPC64State* vex_state );


/* Write the given native %CR value to the supplied VexGuestPPC64State
   structure.  Note, %CR is 32-bits even for ppc64. */
extern
void LibVEX_GuestPPC64_put_CR ( UInt cr_native,
                                /*OUT*/VexGuestPPC64State* vex_state );

/* Extract from the supplied VexGuestPPC64State structure the
   corresponding native %CR value.  Note, %CR is 32-bits even for
   ppc64. */
extern
UInt LibVEX_GuestPPC64_get_CR ( /*IN*/VexGuestPPC64State* vex_state );


/* Write the given native %XER value to the supplied
   VexGuestPPC64State structure.  Note, %XER is 32-bits even for
   ppc64. */
extern
void LibVEX_GuestPPC64_put_XER ( UInt xer_native,
                                 /*OUT*/VexGuestPPC64State* vex_state );

/* Extract from the supplied VexGuestPPC64State structure the
   corresponding native %XER value.  Note, %CR is 32-bits even for
   ppc64. */
extern
UInt LibVEX_GuestPPC64_get_XER ( /*IN*/VexGuestPPC64State* vex_state );

#endif /* ndef __LIBVEX_PUB_GUEST_PPC64_H */


/*---------------------------------------------------------------*/
/*---                                    libvex_guest_ppc64.h ---*/
/*---------------------------------------------------------------*/
