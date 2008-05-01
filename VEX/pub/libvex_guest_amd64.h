
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (libvex_guest_amd64.h) is                     ---*/
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

#ifndef __LIBVEX_PUB_GUEST_AMD64_H
#define __LIBVEX_PUB_GUEST_AMD64_H

#include "libvex_basictypes.h"
#include "libvex_emwarn.h"


/*---------------------------------------------------------------*/
/*--- Vex's representation of the AMD64 CPU state.            ---*/
/*---------------------------------------------------------------*/

/* See detailed comments at the top of libvex_guest_x86.h for
   further info.  This representation closely follows the
   x86 representation.
*/


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
      /* 160 */ ULong  guest_DFLAG;
      /* 168 */ ULong  guest_RIP;
      /* Probably a lot more stuff too. 
         D,ID flags
         16  128-bit SSE registers
         all the old x87 FPU gunk
         segment registers
      */

      /* Bit 21 (ID) of eflags stored here, as either 0 or 1. */
      /* 176 */ ULong guest_IDFLAG;

      /* HACK to make tls on amd64-linux work.  %fs only ever seems to
         hold zero, and so guest_FS_ZERO holds the 64-bit offset
         associated with a %fs value of zero. */
      /* 184 */ ULong guest_FS_ZERO;

      /* XMM registers */
      /* 192 */ULong guest_SSEROUND;
      /* 200 */U128  guest_XMM0;
      U128  guest_XMM1;
      U128  guest_XMM2;
      U128  guest_XMM3;
      U128  guest_XMM4;
      U128  guest_XMM5;
      U128  guest_XMM6;
      U128  guest_XMM7;
      U128  guest_XMM8;
      U128  guest_XMM9;
      U128  guest_XMM10;
      U128  guest_XMM11;
      U128  guest_XMM12;
      U128  guest_XMM13;
      U128  guest_XMM14;
      U128  guest_XMM15;

      /* FPU */
      /* Note.  Setting guest_FTOP to be ULong messes up the
         delicately-balanced PutI/GetI optimisation machinery.
         Therefore best to leave it as a UInt. */
      /* 456 */UInt  guest_FTOP;
      ULong guest_FPREG[8];
      /* 528 */ UChar guest_FPTAG[8];
      /* 536 */ ULong guest_FPROUND;
      /* 544 */ ULong guest_FC3210;

      /* Emulation warnings */
      /* 552 */ UInt  guest_EMWARN;

      /* Translation-invalidation area description.  Not used on amd64
         (there is no invalidate-icache insn), but needed so as to
         allow users of the library to uniformly assume that the guest
         state contains these two fields -- otherwise there is
         compilation breakage.  On amd64, these two fields are set to
         zero by LibVEX_GuestAMD64_initialise and then should be
         ignored forever thereafter. */
      ULong guest_TISTART;
      ULong guest_TILEN;

      /* Used to record the unredirected guest address at the start of
         a translation whose start has been redirected.  By reading
         this pseudo-register shortly afterwards, the translation can
         find out what the corresponding no-redirection address was.
         Note, this is only set for wrap-style redirects, not for
         replace-style ones. */
      ULong guest_NRADDR;

      /* Padding to make it have an 16-aligned size */
      ULong padding;
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
