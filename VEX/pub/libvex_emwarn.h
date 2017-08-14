
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (libvex_emwarn.h) is                          ---*/
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

#ifndef __LIBVEX_EMWARN_H
#define __LIBVEX_EMWARN_H


/* VEX can sometimes generate code which returns to the dispatcher
   with the guest state pointer set to VEX_TRC_JMP_EMWARN.  This means
   that VEX is trying to warn Valgrind that it is doing imprecise
   emulation in some sense.  The guest's pseudo-register
   "guest_EMWARN" will hold a value of type VexEmWarn, which describes
   the nature of the warning.  Currently the limitations that are
   warned about apply primarily to floating point support.

   All guest states should have a 32-bit (UInt) guest_EMWARN pseudo-
   register, that emulation warnings can be written in to.

   Note that guest_EMWARN only carries a valid value at the jump
   marked as VEX_TRC_JMP_EMWARN.  You can't assume it will continue to
   carry a valid value from any amount of time after the jump.
*/

typedef
   enum {
      /* no warning indicated */
      EmWarn_NONE=0,

      /* unmasking x87 FP exceptions is not supported */
      EmWarn_X86_x87exns,

      /* change of x87 FP precision away from 64-bit (mantissa) */
      EmWarn_X86_x87precision,

      /* unmasking SSE FP exceptions is not supported */
      EmWarn_X86_sseExns,
      
      /* setting mxcsr.fz is not supported */
      EmWarn_X86_fz,
      
      /* setting mxcsr.daz is not supported */
      EmWarn_X86_daz,

      /* settings to %eflags.ac (alignment check) are noted but ignored */
      EmWarn_X86_acFlag,
      
      /* unmasking PPC32/64 FP exceptions is not supported */
      EmWarn_PPCexns,

      /* overflow/underflow of the PPC64 _REDIR stack (ppc64 only) */
      EmWarn_PPC64_redir_overflow,
      EmWarn_PPC64_redir_underflow,

      EmWarn_NUMBER
   }
   VexEmWarn;


/* Produces a short string describing the warning. */
extern HChar* LibVEX_EmWarn_string ( VexEmWarn );


#endif /* ndef __LIBVEX_EMWARN_H */

/*---------------------------------------------------------------*/
/*---                                         libvex_emwarn.h ---*/
/*---------------------------------------------------------------*/
