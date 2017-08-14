
/*---------------------------------------------------------------*/
/*--- begin                                guest_arm64_defs.h ---*/
/*---------------------------------------------------------------*/
/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2013-2017 OpenWorks
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
*/

#ifndef __VEX_GUEST_ARM64_DEFS_H
#define __VEX_GUEST_ARM64_DEFS_H

#include "libvex_basictypes.h"
#include "guest_generic_bb_to_IR.h"     // DisResult

/*---------------------------------------------------------*/
/*--- arm64 to IR conversion                            ---*/
/*---------------------------------------------------------*/

/* Convert one ARM64 insn to IR.  See the type DisOneInstrFn in
   guest_generic_bb_to_IR.h. */
extern
DisResult disInstr_ARM64 ( IRSB*        irbb,
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
IRExpr* guest_arm64_spechelper ( const HChar* function_name,
                                 IRExpr** args,
                                 IRStmt** precedingStmts,
                                 Int      n_precedingStmts );

/* Describes to the optimser which part of the guest state require
   precise memory exceptions.  This is logically part of the guest
   state description. */
extern 
Bool guest_arm64_state_requires_precise_mem_exns ( Int, Int,
                                                   VexRegisterUpdates );

extern
VexGuestLayout arm64Guest_layout;


/*---------------------------------------------------------*/
/*--- arm64 guest helpers                               ---*/
/*---------------------------------------------------------*/

/* --- CLEAN HELPERS --- */

/* Calculate NZCV from the supplied thunk components, in the positions
   they appear in the CPSR, viz bits 31:28 for N Z C V respectively.
   Returned bits 63:32 and 27:0 are zero. */
extern 
ULong arm64g_calculate_flags_nzcv ( ULong cc_op, ULong cc_dep1,
                                    ULong cc_dep2, ULong cc_dep3 );

/* Calculate the C flag from the thunk components, in the lowest bit
   of the word (bit 0). */
extern
ULong arm64g_calculate_flag_c ( ULong cc_op, ULong cc_dep1,
                                ULong cc_dep2, ULong cc_dep3 );

//ZZ /* Calculate the V flag from the thunk components, in the lowest bit
//ZZ    of the word (bit 0). */
//ZZ extern 
//ZZ UInt armg_calculate_flag_v ( UInt cc_op, UInt cc_dep1,
//ZZ                              UInt cc_dep2, UInt cc_dep3 );
//ZZ 
/* Calculate the specified condition from the thunk components, in the
   lowest bit of the word (bit 0). */
extern 
ULong arm64g_calculate_condition ( /* ARM64Condcode << 4 | cc_op */
                                   ULong cond_n_op ,
                                   ULong cc_dep1,
                                   ULong cc_dep2, ULong cc_dep3 );

//ZZ /* Calculate the QC flag from the thunk components, in the lowest bit
//ZZ    of the word (bit 0). */
//ZZ extern 
//ZZ UInt armg_calculate_flag_qc ( UInt resL1, UInt resL2,
//ZZ                               UInt resR1, UInt resR2 );

extern ULong arm64g_calc_crc32b ( ULong acc, ULong bits );
extern ULong arm64g_calc_crc32h ( ULong acc, ULong bits );
extern ULong arm64g_calc_crc32w ( ULong acc, ULong bits );
extern ULong arm64g_calc_crc32x ( ULong acc, ULong bits );

extern ULong arm64g_calc_crc32cb ( ULong acc, ULong bits );
extern ULong arm64g_calc_crc32ch ( ULong acc, ULong bits );
extern ULong arm64g_calc_crc32cw ( ULong acc, ULong bits );
extern ULong arm64g_calc_crc32cx ( ULong acc, ULong bits );

/* --- DIRTY HELPERS --- */

extern ULong arm64g_dirtyhelper_MRS_CNTVCT_EL0 ( void );

extern ULong arm64g_dirtyhelper_MRS_CNTFRQ_EL0 ( void );

extern void  arm64g_dirtyhelper_PMULLQ ( /*OUT*/V128* res,
                                         ULong arg1, ULong arg2 );

extern void  arm64g_dirtyhelper_AESE ( /*OUT*/V128* res,
                                       ULong argHi, ULong argLo );
extern void  arm64g_dirtyhelper_AESD ( /*OUT*/V128* res,
                                       ULong argHi, ULong argLo );
extern void  arm64g_dirtyhelper_AESMC  ( /*OUT*/V128* res,
                                         ULong argHi, ULong argLo );
extern void  arm64g_dirtyhelper_AESIMC ( /*OUT*/V128* res,
                                         ULong argHi, ULong argLo );

extern
void arm64g_dirtyhelper_SHA1C ( /*OUT*/V128* res, ULong dHi, ULong dLo,
                                ULong nHi, ULong nLo, ULong mHi, ULong mLo );
extern
void arm64g_dirtyhelper_SHA1H ( /*OUT*/V128* res,
                                ULong nHi, ULong nLo );
extern
void arm64g_dirtyhelper_SHA1M ( /*OUT*/V128* res, ULong dHi, ULong dLo,
                                ULong nHi, ULong nLo, ULong mHi, ULong mLo );
extern
void arm64g_dirtyhelper_SHA1P ( /*OUT*/V128* res, ULong dHi, ULong dLo,
                                ULong nHi, ULong nLo, ULong mHi, ULong mLo );
extern
void arm64g_dirtyhelper_SHA1SU0 ( /*OUT*/V128* res, ULong dHi, ULong dLo,
                                  ULong nHi, ULong nLo, ULong mHi, ULong mLo );
extern
void arm64g_dirtyhelper_SHA1SU1 ( /*OUT*/V128* res, ULong dHi, ULong dLo,
                                  ULong nHi, ULong nLo );
extern
void arm64g_dirtyhelper_SHA256H2 ( /*OUT*/V128* res, ULong dHi, ULong dLo,
                                   ULong nHi, ULong nLo, ULong mHi, ULong mLo );
extern
void arm64g_dirtyhelper_SHA256H ( /*OUT*/V128* res, ULong dHi, ULong dLo,
                                  ULong nHi, ULong nLo, ULong mHi, ULong mLo );
extern
void arm64g_dirtyhelper_SHA256SU0 ( /*OUT*/V128* res, ULong dHi, ULong dLo,
                                    ULong nHi, ULong nLo );
extern
void arm64g_dirtyhelper_SHA256SU1 ( /*OUT*/V128* res, ULong dHi, ULong dLo,
                                    ULong nHi, ULong nLo,
                                    ULong mHi, ULong mLo );


/*---------------------------------------------------------*/
/*--- Condition code stuff                              ---*/
/*---------------------------------------------------------*/

/* Flag masks.  Defines positions of flag bits in the NZCV
   register. */
#define ARM64G_CC_SHIFT_N  31
#define ARM64G_CC_SHIFT_Z  30
#define ARM64G_CC_SHIFT_C  29
#define ARM64G_CC_SHIFT_V  28
//ZZ #define ARMG_CC_SHIFT_Q  27
//ZZ 
//ZZ #define ARMG_CC_MASK_N    (1 << ARMG_CC_SHIFT_N)
//ZZ #define ARMG_CC_MASK_Z    (1 << ARMG_CC_SHIFT_Z)
//ZZ #define ARMG_CC_MASK_C    (1 << ARMG_CC_SHIFT_C)
//ZZ #define ARMG_CC_MASK_V    (1 << ARMG_CC_SHIFT_V)
//ZZ #define ARMG_CC_MASK_Q    (1 << ARMG_CC_SHIFT_Q)

/* Flag thunk descriptors.  A four-word thunk is used to record
   details of the most recent flag-setting operation, so NZCV can
   be computed later if needed.

   The four words are:

      CC_OP, which describes the operation.

      CC_DEP1, CC_DEP2, CC_NDEP.  These are arguments to the
         operation.  We want set up the mcx_masks in flag helper calls
         involving these fields so that Memcheck "believes" that the
         resulting flags are data-dependent on both CC_DEP1 and
         CC_DEP2.  Hence the name DEP.

   When building the thunk, it is always necessary to write words into
   CC_DEP1/2 and NDEP, even if those args are not used given the CC_OP
   field.  This is important because otherwise Memcheck could give
   false positives as it does not understand the relationship between
   the CC_OP field and CC_DEP1/2/NDEP, and so believes that the
   definedness of the stored flags always depends on all 3 DEP values.

   A summary of the field usages is:

   OP                DEP1              DEP2              DEP3
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   OP_COPY           curr_NZCV:28x0    unused            unused
   OP_ADD32          argL              argR              unused
   OP_ADD64          argL              argR              unused
   OP_SUB32          argL              argR              unused
   OP_SUB64          argL              argR              unused
   OP_ADC32          argL              argR              63x0:old_C
   OP_ADC64          argL              argR              63x0:old_C
   OP_SBC32          argL              argR              63x0:old_C
   OP_SBC64          argL              argR              63x0:old_C
   OP_LOGIC32        result            unused            unused
   OP_LOGIC64        result            unused            unused
//ZZ    OP_MUL            result            unused            30x0:old_C:old_V
//ZZ    OP_MULL           resLO32           resHI32           30x0:old_C:old_V
//ZZ */

enum {
   ARM64G_CC_OP_COPY=0,   /* DEP1 = NZCV in 31:28, DEP2 = 0, DEP3 = 0
                             just copy DEP1 to output */

   ARM64G_CC_OP_ADD32,    /* DEP1 = argL (Rn), DEP2 = argR (shifter_op),
                             DEP3 = 0 */

   ARM64G_CC_OP_ADD64,    /* DEP1 = argL (Rn), DEP2 = argR (shifter_op),
                             DEP3 = 0 */

   ARM64G_CC_OP_SUB32,    /* DEP1 = argL (Rn), DEP2 = argR (shifter_op),
                             DEP3 = 0 */

   ARM64G_CC_OP_SUB64,    /* DEP1 = argL (Rn), DEP2 = argR (shifter_op),
                             DEP3 = 0 */

   ARM64G_CC_OP_ADC32,    /* DEP1 = argL (Rn), DEP2 = arg2 (shifter_op),
                             DEP3 = oldC (in LSB) */

   ARM64G_CC_OP_ADC64,    /* DEP1 = argL (Rn), DEP2 = arg2 (shifter_op),
                             DEP3 = oldC (in LSB) */

   ARM64G_CC_OP_SBC32,    /* DEP1 = argL (Rn), DEP2 = arg2 (shifter_op),
                             DEP3 = oldC (in LSB) */

   ARM64G_CC_OP_SBC64,    /* DEP1 = argL (Rn), DEP2 = arg2 (shifter_op),
                             DEP3 = oldC (in LSB) */

   ARM64G_CC_OP_LOGIC32,  /* DEP1 = result, DEP2 = 0, DEP3 = 0 */
   ARM64G_CC_OP_LOGIC64,  /* DEP1 = result, DEP2 = 0, DEP3 = 0 */

//ZZ    ARMG_CC_OP_MUL,     /* DEP1 = result, DEP2 = 0, DEP3 = oldC:old_V
//ZZ                           (in bits 1:0) */
//ZZ 
//ZZ    ARMG_CC_OP_MULL,    /* DEP1 = resLO32, DEP2 = resHI32, DEP3 = oldC:old_V
//ZZ                           (in bits 1:0) */

   ARM64G_CC_OP_NUMBER
};

/* XXXX because of the calling conventions for
   arm64g_calculate_condition, all these OP values MUST be in the range
   0 .. 15 only (viz, 4-bits). */



/* Defines conditions which we can ask for */

typedef
   enum {
      ARM64CondEQ = 0,  /* equal                         : Z=1 */
      ARM64CondNE = 1,  /* not equal                     : Z=0 */

      ARM64CondCS = 2,  /* >=u (higher or same) (aka HS) : C=1 */
      ARM64CondCC = 3,  /* <u  (lower)          (aka LO) : C=0 */

      ARM64CondMI = 4,  /* minus (negative)              : N=1 */
      ARM64CondPL = 5,  /* plus (zero or +ve)            : N=0 */

      ARM64CondVS = 6,  /* overflow                      : V=1 */
      ARM64CondVC = 7,  /* no overflow                   : V=0 */

      ARM64CondHI = 8,  /* >u   (higher)                 : C=1 && Z=0 */
      ARM64CondLS = 9,  /* <=u  (lower or same)          : C=0 || Z=1 */

      ARM64CondGE = 10, /* >=s (signed greater or equal) : N=V */
      ARM64CondLT = 11, /* <s  (signed less than)        : N!=V */

      ARM64CondGT = 12, /* >s  (signed greater)          : Z=0 && N=V */
      ARM64CondLE = 13, /* <=s (signed less or equal)    : Z=1 || N!=V */

      ARM64CondAL = 14, /* always (unconditional)        : 1 */
      ARM64CondNV = 15  /* always (unconditional)        : 1 */
   }
   ARM64Condcode;

#endif /* ndef __VEX_GUEST_ARM64_DEFS_H */

/*---------------------------------------------------------------*/
/*--- end                                  guest_arm64_defs.h ---*/
/*---------------------------------------------------------------*/
