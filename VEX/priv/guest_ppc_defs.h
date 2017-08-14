
/*---------------------------------------------------------------*/
/*--- begin                                  guest_ppc_defs.h ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2004-2015 OpenWorks LLP
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

/* Only to be used within the guest-ppc directory. */


#ifndef __VEX_GUEST_PPC_DEFS_H
#define __VEX_GUEST_PPC_DEFS_H

#include "libvex_basictypes.h"
#include "libvex_guest_ppc32.h"         // VexGuestPPC32State
#include "libvex_guest_ppc64.h"         // VexGuestPPC64State
#include "guest_generic_bb_to_IR.h"     // DisResult

/*---------------------------------------------------------*/
/*--- ppc to IR conversion                              ---*/
/*---------------------------------------------------------*/

/* Convert one ppc insn to IR.  See the type DisOneInstrFn in
   bb_to_IR.h. */
extern
DisResult disInstr_PPC ( IRSB*        irbb,
                         Bool         (*resteerOkFn) ( void*, Addr ),
                         Bool         resteerCisOk,
                         void*        callback_opaque,
                         const UChar* guest_code,
                         Long         delta,
                         Addr         guest_IP,
                         VexArch      guest_arch,
                         const VexArchInfo* archinfo,
                         const VexAbiInfo*  abiinfo,
                         VexEndness   host_endness,
                         Bool         sigill_diag );

/* Used by the optimiser to specialise calls to helpers. */
extern
IRExpr* guest_ppc32_spechelper ( const HChar* function_name,
                                 IRExpr** args,
                                 IRStmt** precedingStmts,
                                 Int      n_precedingStmts );

extern
IRExpr* guest_ppc64_spechelper ( const HChar* function_name,
                                 IRExpr** args,
                                 IRStmt** precedingStmts,
                                 Int      n_precedingStmts );

/* Describes to the optimser which part of the guest state require
   precise memory exceptions.  This is logically part of the guest
   state description. */
extern 
Bool guest_ppc32_state_requires_precise_mem_exns ( Int, Int,
                                                   VexRegisterUpdates );

extern 
Bool guest_ppc64_state_requires_precise_mem_exns ( Int, Int,
                                                   VexRegisterUpdates );

extern
VexGuestLayout ppc32Guest_layout;

extern
VexGuestLayout ppc64Guest_layout;


/* FP Rounding mode - different encoding to IR */
typedef
   enum {
      PPCrm_NEAREST = 0,
      PPCrm_NegINF  = 1,
      PPCrm_PosINF  = 2,
      PPCrm_ZERO    = 3
   } PPCRoundingMode;

/* Floating point comparison values - different encoding to IR */
typedef
   enum {
      PPCcr_LT = 0x8,
      PPCcr_GT = 0x4,
      PPCcr_EQ = 0x2,
      PPCcr_UN = 0x1
   }
   PPCCmpF64Result;

/*
  Enumeration for xer_ca/ov calculation helper functions
*/
enum {
   /* 0  */ PPCG_FLAG_OP_ADD=0,   // addc[o], addic
   /* 1  */ PPCG_FLAG_OP_ADDE,    // adde[o], addme[o], addze[o]
   /* 2  */ PPCG_FLAG_OP_DIVW,    // divwo
   /* 3  */ PPCG_FLAG_OP_DIVWU,   // divwuo
   /* 4  */ PPCG_FLAG_OP_MULLW,   // mullwo
   /* 5  */ PPCG_FLAG_OP_NEG,     // nego
   /* 6  */ PPCG_FLAG_OP_SUBF,    // subfo
   /* 7  */ PPCG_FLAG_OP_SUBFC,   // subfc[o]
   /* 8  */ PPCG_FLAG_OP_SUBFE,   // subfe[o], subfme[o], subfze[o]
   /* 9  */ PPCG_FLAG_OP_SUBFI,   // subfic
   /* 10 */ PPCG_FLAG_OP_SRAW,    // sraw
   /* 11 */ PPCG_FLAG_OP_SRAWI,   // srawi
   /* 12 */ PPCG_FLAG_OP_SRAD,    // srad
   /* 13 */ PPCG_FLAG_OP_SRADI,   // sradi
   /* 14 */ PPCG_FLAG_OP_DIVDE,   // divdeo
   /* 15 */ PPCG_FLAG_OP_DIVWEU,  // divweuo
   /* 16 */ PPCG_FLAG_OP_DIVWE,   // divweo
   /* 17 */ PPCG_FLAG_OP_DIVDEU,  // divdeuo
   /* 18 */ PPCG_FLAG_OP_MULLD,   // mulldo
   PPCG_FLAG_OP_NUMBER
};


/*---------------------------------------------------------*/
/*--- ppc guest helpers                                 ---*/
/*---------------------------------------------------------*/

/* --- CLEAN HELPERS --- */

extern ULong is_BCDstring128_helper( ULong Signed, ULong hi64, ULong low64 );
extern ULong increment_BCDstring32_helper( ULong Signed,
                                           ULong bcd_string, ULong carry_in );
extern ULong convert_to_zoned_helper( ULong src_hi, ULong src_low,
                                      ULong upper_byte,
                                      ULong return_upper );
extern ULong convert_to_national_helper( ULong src, ULong return_upper );
extern ULong convert_from_zoned_helper( ULong src_hi, ULong src_low );
extern ULong convert_from_national_helper( ULong src_hi, ULong src_low );


/* --- DIRTY HELPERS --- */

extern ULong ppcg_dirtyhelper_MFTB ( void );

extern UInt ppc32g_dirtyhelper_MFSPR_268_269 ( UInt );

extern UInt ppc32g_dirtyhelper_MFSPR_287 ( void );

extern void ppc32g_dirtyhelper_LVS ( VexGuestPPC32State* gst,
                                     UInt vD_idx, UInt sh,
                                     UInt shift_right );

extern void ppc64g_dirtyhelper_LVS ( VexGuestPPC64State* gst,
                                     UInt vD_idx, UInt sh,
                                     UInt shift_right,
                                     UInt endness );

#endif /* ndef __VEX_GUEST_PPC_DEFS_H */

/*---------------------------------------------------------------*/
/*--- end                                    guest_ppc_defs.h ---*/
/*---------------------------------------------------------------*/
