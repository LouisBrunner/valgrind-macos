
/*---------------------------------------------------------------*/
/*--- begin                                   libvex_emwarn.h ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2004-2012 OpenWorks LLP
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
