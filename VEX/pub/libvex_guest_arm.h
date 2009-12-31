
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (libvex_guest_arm.h) is                       ---*/
/*--- Copyright (C) OpenWorks LLP.  All rights reserved.      ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of LibVEX, a library for dynamic binary
   instrumentation and translation.

   Copyright (C) 2004-2009 OpenWorks LLP.  All rights reserved.

   This library is made available under a dual licensing scheme.

   If you link LibVEX against other code all of which is itself
   licensed under the GNU General Public License, version 2 dated June
   1991 ("GPL v2"), then you may use LibVEX under the terms of the GPL
   v2, as appearing in the file LICENSE.GPL.  If the file LICENSE.GPL
   is missing, you can obtain a copy of the GPL v2 from the Free
   Software Foundation Inc., 51 Franklin St, Fifth Floor, Boston, MA
   02110-1301, USA.

   For any other uses of LibVEX, you must first obtain a commercial
   license from OpenWorks LLP.  Please contact info@open-works.co.uk
   for information about commercial licensing.

   This software is provided by OpenWorks LLP "as is" and any express
   or implied warranties, including, but not limited to, the implied
   warranties of merchantability and fitness for a particular purpose
   are disclaimed.  In no event shall OpenWorks LLP be liable for any
   direct, indirect, incidental, special, exemplary, or consequential
   damages (including, but not limited to, procurement of substitute
   goods or services; loss of use, data, or profits; or business
   interruption) however caused and on any theory of liability,
   whether in contract, strict liability, or tort (including
   negligence or otherwise) arising in any way out of the use of this
   software, even if advised of the possibility of such damage.
*/

#ifndef __LIBVEX_PUB_GUEST_ARM_H
#define __LIBVEX_PUB_GUEST_ARM_H

#include "libvex_basictypes.h"
#include "libvex_emwarn.h"


/*---------------------------------------------------------------*/
/*--- Vex's representation of the ARM CPU state.              ---*/
/*---------------------------------------------------------------*/

typedef
   struct {
      /* 0 */
      UInt guest_R0;
      UInt guest_R1;
      UInt guest_R2;
      UInt guest_R3;
      UInt guest_R4;
      UInt guest_R5;
      UInt guest_R6;
      UInt guest_R7;
      UInt guest_R8;
      UInt guest_R9;
      UInt guest_R10;
      UInt guest_R11;
      UInt guest_R12;
      UInt guest_R13;     /* stack pointer */
      UInt guest_R14;     /* link register */
      UInt guest_R15;     /* program counter */

      /* 4-word thunk used to calculate N(sign) Z(zero) C(carry,
         unsigned overflow) and V(signed overflow) flags. */
      /* 64 */
      UInt guest_CC_OP;
      UInt guest_CC_DEP1;
      UInt guest_CC_DEP2;
      UInt guest_CC_NDEP;

      /* Various pseudo-regs mandated by Vex or Valgrind. */
      /* Emulation warnings */
      UInt guest_EMWARN;

      /* For clflush: record start and length of area to invalidate */
      UInt guest_TISTART;
      UInt guest_TILEN;

      /* Used to record the unredirected guest address at the start of
         a translation whose start has been redirected.  By reading
         this pseudo-register shortly afterwards, the translation can
         find out what the corresponding no-redirection address was.
         Note, this is only set for wrap-style redirects, not for
         replace-style ones. */
      UInt guest_NRADDR;

      /* Needed for Darwin (but mandated for all guest architectures):
         program counter at the last syscall insn (int 0x80/81/82,
         sysenter, syscall, svc).  Used when backing up to restart a
         syscall that has been interrupted by a signal. */
      /* 96 */
      UInt guest_IP_AT_SYSCALL;

      /* VFP state.  D0 .. D15 must be 8-aligned. */
      /* 104 -- I guess there's 4 bytes of padding just prior to this? */
      ULong guest_D0;
      ULong guest_D1;
      ULong guest_D2;
      ULong guest_D3;
      ULong guest_D4;
      ULong guest_D5;
      ULong guest_D6;
      ULong guest_D7;
      ULong guest_D8;
      ULong guest_D9;
      ULong guest_D10;
      ULong guest_D11;
      ULong guest_D12;
      ULong guest_D13;
      ULong guest_D14;
      ULong guest_D15;
      UInt  guest_FPSCR;

      /* Not a town in Cornwall, but instead the TPIDRURO, on of the
         Thread ID registers present in CP15 (the system control
         coprocessor), register set "c13", register 3 (the User
         Read-only Thread ID Register).  arm-linux apparently uses it
         to hold the TLS pointer for the thread.  It's read-only in
         user space.  On Linux it is set in user space by various
         thread-related syscalls. */
      UInt guest_TPIDRURO;

      /* Padding to make it have an 16-aligned size */
      /* UInt padding1; */
      /* UInt padding2; */
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
UInt LibVEX_GuestARM_get_cpsr ( /*IN*/VexGuestARMState* vex_state );


#endif /* ndef __LIBVEX_PUB_GUEST_ARM_H */


/*---------------------------------------------------------------*/
/*---                                      libvex_guest_arm.h ---*/
/*---------------------------------------------------------------*/
