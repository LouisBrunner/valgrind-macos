
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (libvex_guest_arm.h) is                       ---*/
/*--- Copyright (C) OpenWorks LLP.  All rights reserved.      ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of LibVEX, a library for dynamic binary
   instrumentation and translation.

   Copyright (C) 2004-2008 OpenWorks LLP.  All rights reserved.

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

   Neither the names of the U.S. Department of Energy nor the
   University of California nor the names of its contributors may be
   used to endorse or promote products derived from this software
   without prior written permission.
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
