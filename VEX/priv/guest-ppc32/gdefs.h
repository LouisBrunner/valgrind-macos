
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (guest-ppc32/gdefs.h) is                      ---*/
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

/* Only to be used within the guest-ppc32 directory. */


#ifndef __LIBVEX_GUEST_PPC32_DEFS_H
#define __LIBVEX_GUEST_PPC32_DEFS_H


/*---------------------------------------------------------*/
/*--- ppc32 to IR conversion                            ---*/
/*---------------------------------------------------------*/

/* Convert one ppc32 insn to IR.  See the type DisOneInstrFn in
   bb_to_IR.h. */
extern
DisResult disInstr_PPC32 ( IRBB*        irbb,
                           Bool         put_IP,
                           Bool         (*resteerOkFn) ( Addr64 ),
                           UChar*       guest_code,
                           Long         delta,
                           Addr64       guest_IP,
                           VexArchInfo* archinfo,
                           Bool         host_bigendian );

/* Used by the optimiser to specialise calls to helpers. */
extern
IRExpr* guest_ppc32_spechelper ( HChar* function_name,
                                 IRExpr** args );

/* Describes to the optimser which part of the guest state require
   precise memory exceptions.  This is logically part of the guest
   state description. */
extern 
Bool guest_ppc32_state_requires_precise_mem_exns ( Int, Int );

extern
VexGuestLayout ppc32Guest_layout;


/* FP Rounding mode - different encoding to IR */
typedef
   enum {
      PPC32rm_NEAREST = 0,
      PPC32rm_NegINF  = 1,
      PPC32rm_PosINF  = 2,
      PPC32rm_ZERO    = 3
   } PPC32RoundingMode;

/* Floating point comparison values - different encoding to IR */
typedef
   enum {
      PPC32cr_LT = 0x8,
      PPC32cr_GT = 0x4,
      PPC32cr_EQ = 0x2,
      PPC32cr_UN = 0x1
   }
   PPC32CmpF64Result;


/*---------------------------------------------------------*/
/*--- ppc32 guest helpers                                 ---*/
/*---------------------------------------------------------*/

/* --- CLEAN HELPERS --- */

// Calculate XER flags
extern 
UInt ppc32g_calculate_xer_ov  ( UInt op, 
                                UInt res, UInt argL, UInt argR );

extern 
UInt ppc32g_calculate_xer_ca  ( UInt op, 
                                UInt res, UInt argL, UInt argR, 
                                UInt old_ca );

/*
  Enumeration for xer_ca/ov calculation helper functions
*/
enum {
   PPC32G_FLAG_OP_ADD=0,   // addc[o], addic
   PPC32G_FLAG_OP_ADDE,    // adde[o], addme[o], addze[o]
   PPC32G_FLAG_OP_DIVW,    // divwo
   PPC32G_FLAG_OP_DIVWU,   // divwuo
   PPC32G_FLAG_OP_MULLW,   // mullwo
   PPC32G_FLAG_OP_NEG,     // nego
   PPC32G_FLAG_OP_SUBF,    // subfo
   PPC32G_FLAG_OP_SUBFC,   // subfc[o]
   PPC32G_FLAG_OP_SUBFE,   // subfe[o], subfme[o], subfze[o]
   PPC32G_FLAG_OP_SUBFI,   // subfic
   PPC32G_FLAG_OP_SRAW,    // sraw
   PPC32G_FLAG_OP_SRAWI,   // srawi
   
   PPC32G_FLAG_OP_NUMBER
};


#endif /* ndef __LIBVEX_GUEST_PPC32_DEFS_H */

/*---------------------------------------------------------------*/
/*--- end                                 guest-ppc32/gdefs.h ---*/
/*---------------------------------------------------------------*/
