
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (libvex_emwarn.h) is                          ---*/
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
      
      EmWarn_NUMBER
   }
   VexEmWarn;


/* Produces a short string describing the warning. */
extern HChar* LibVEX_EmWarn_string ( VexEmWarn );


#endif /* ndef __LIBVEX_EMWARN_H */

/*---------------------------------------------------------------*/
/*---                                         libvex_emwarn.h ---*/
/*---------------------------------------------------------------*/
