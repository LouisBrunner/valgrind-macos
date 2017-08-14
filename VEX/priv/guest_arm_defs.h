
/*---------------------------------------------------------------*/
/*--- begin                                  guest_arm_defs.h ---*/
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
*/

/* Only to be used within the guest-arm directory. */

#ifndef __VEX_GUEST_ARM_DEFS_H
#define __VEX_GUEST_ARM_DEFS_H

#include "libvex_basictypes.h"
#include "guest_generic_bb_to_IR.h"     // DisResult

/*---------------------------------------------------------*/
/*--- arm to IR conversion                              ---*/
/*---------------------------------------------------------*/

/* Convert one ARM insn to IR.  See the type DisOneInstrFn in
   bb_to_IR.h. */
extern
DisResult disInstr_ARM ( IRSB*        irbb,
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
IRExpr* guest_arm_spechelper ( const HChar* function_name,
                               IRExpr** args,
                               IRStmt** precedingStmts,
                               Int      n_precedingStmts );

/* Describes to the optimser which part of the guest state require
   precise memory exceptions.  This is logically part of the guest
   state description. */
extern 
Bool guest_arm_state_requires_precise_mem_exns ( Int, Int,
                                                 VexRegisterUpdates );

extern
VexGuestLayout armGuest_layout;


/*---------------------------------------------------------*/
/*--- arm guest helpers                                 ---*/
/*---------------------------------------------------------*/

/* --- CLEAN HELPERS --- */

/* Calculate NZCV from the supplied thunk components, in the positions
   they appear in the CPSR, viz bits 31:28 for N Z V C respectively.
   Returned bits 27:0 are zero. */
extern 
UInt armg_calculate_flags_nzcv ( UInt cc_op, UInt cc_dep1,
                                 UInt cc_dep2, UInt cc_dep3 );

/* Calculate the C flag from the thunk components, in the lowest bit
   of the word (bit 0). */
extern 
UInt armg_calculate_flag_c ( UInt cc_op, UInt cc_dep1,
                             UInt cc_dep2, UInt cc_dep3 );

/* Calculate the V flag from the thunk components, in the lowest bit
   of the word (bit 0). */
extern 
UInt armg_calculate_flag_v ( UInt cc_op, UInt cc_dep1,
                             UInt cc_dep2, UInt cc_dep3 );

/* Calculate the specified condition from the thunk components, in the
   lowest bit of the word (bit 0). */
extern 
UInt armg_calculate_condition ( UInt cond_n_op /* ARMCondcode << 4 | cc_op */,
                                UInt cc_dep1,
                                UInt cc_dep2, UInt cc_dep3 );

/* Calculate the QC flag from the thunk components, in the lowest bit
   of the word (bit 0). */
extern 
UInt armg_calculate_flag_qc ( UInt resL1, UInt resL2,
                              UInt resR1, UInt resR2 );


/*---------------------------------------------------------*/
/*--- Condition code stuff                              ---*/
/*---------------------------------------------------------*/

/* Flags masks.  Defines positions of flags bits in the CPSR. */
#define ARMG_CC_SHIFT_N  31
#define ARMG_CC_SHIFT_Z  30
#define ARMG_CC_SHIFT_C  29
#define ARMG_CC_SHIFT_V  28
#define ARMG_CC_SHIFT_Q  27

#define ARMG_CC_MASK_N    (1 << ARMG_CC_SHIFT_N)
#define ARMG_CC_MASK_Z    (1 << ARMG_CC_SHIFT_Z)
#define ARMG_CC_MASK_C    (1 << ARMG_CC_SHIFT_C)
#define ARMG_CC_MASK_V    (1 << ARMG_CC_SHIFT_V)
#define ARMG_CC_MASK_Q    (1 << ARMG_CC_SHIFT_Q)

/* Flag thunk descriptors.  A four-word thunk is used to record
   details of the most recent flag-setting operation, so NZCV can
   be computed later if needed.

   The four words are:

      CC_OP, which describes the operation.

      CC_DEP1, CC_DEP2, CC_DEP3.  These are arguments to the
         operation.  We want set up the mcx_masks in flag helper calls
         involving these fields so that Memcheck "believes" that the
         resulting flags are data-dependent on both CC_DEP1 and
         CC_DEP2.  Hence the name DEP.

   When building the thunk, it is always necessary to write words into
   CC_DEP1/2/3, even if those args are not used given the
   CC_OP field.  This is important because otherwise Memcheck could
   give false positives as it does not understand the relationship
   between the CC_OP field and CC_DEP1/2/3, and so believes
   that the definedness of the stored flags always depends on
   all 3 DEP values.

   Fields carrying only 1 or 2 bits of useful information (old_C,
   shifter_co, old_V, oldC:oldV) must have their top 31 or 30 bits
   (respectively) zero.  The text "31x0:" or "30x0:" denotes this.

   A summary of the field usages is:

   OP                DEP1              DEP2              DEP3
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   OP_COPY           curr_NZCV:28x0    unused            unused
   OP_ADD            argL              argR              unused
   OP_SUB            argL              argR              unused
   OP_ADC            argL              argR              31x0:old_C
   OP_SBB            argL              argR              31x0:old_C
   OP_LOGIC          result            31x0:shifter_co   31x0:old_V
   OP_MUL            result            unused            30x0:old_C:old_V
   OP_MULL           resLO32           resHI32           30x0:old_C:old_V
*/

enum {
   ARMG_CC_OP_COPY=0,  /* DEP1 = NZCV in 31:28, DEP2 = 0, DEP3 = 0
                          just copy DEP1 to output */

   ARMG_CC_OP_ADD,     /* DEP1 = argL (Rn), DEP2 = argR (shifter_op),
                          DEP3 = 0 */

   ARMG_CC_OP_SUB,     /* DEP1 = argL (Rn), DEP2 = argR (shifter_op),
                          DEP3 = 0 */

   ARMG_CC_OP_ADC,     /* DEP1 = argL (Rn), DEP2 = arg2 (shifter_op),
                          DEP3 = oldC (in LSB) */

   ARMG_CC_OP_SBB,     /* DEP1 = argL (Rn), DEP2 = arg2 (shifter_op),
                          DEP3 = oldC (in LSB) */

   ARMG_CC_OP_LOGIC,   /* DEP1 = result, DEP2 = shifter_carry_out (in LSB),
                          DEP3 = old V flag (in LSB) */

   ARMG_CC_OP_MUL,     /* DEP1 = result, DEP2 = 0, DEP3 = oldC:old_V
                          (in bits 1:0) */

   ARMG_CC_OP_MULL,    /* DEP1 = resLO32, DEP2 = resHI32, DEP3 = oldC:old_V
                          (in bits 1:0) */

   ARMG_CC_OP_NUMBER
};

/* XXXX because of the calling conventions for
   armg_calculate_condition, all this OP values MUST be in the range
   0 .. 15 only (viz, 4-bits). */



/* Defines conditions which we can ask for (ARM ARM 2e page A3-6) */

typedef
   enum {
      ARMCondEQ     = 0,  /* equal                         : Z=1 */
      ARMCondNE     = 1,  /* not equal                     : Z=0 */

      ARMCondHS     = 2,  /* >=u (higher or same)          : C=1 */
      ARMCondLO     = 3,  /* <u  (lower)                   : C=0 */

      ARMCondMI     = 4,  /* minus (negative)              : N=1 */
      ARMCondPL     = 5,  /* plus (zero or +ve)            : N=0 */

      ARMCondVS     = 6,  /* overflow                      : V=1 */
      ARMCondVC     = 7,  /* no overflow                   : V=0 */

      ARMCondHI     = 8,  /* >u   (higher)                 : C=1 && Z=0 */
      ARMCondLS     = 9,  /* <=u  (lower or same)          : C=0 || Z=1 */

      ARMCondGE     = 10, /* >=s (signed greater or equal) : N=V */
      ARMCondLT     = 11, /* <s  (signed less than)        : N!=V */

      ARMCondGT     = 12, /* >s  (signed greater)          : Z=0 && N=V */
      ARMCondLE     = 13, /* <=s (signed less or equal)    : Z=1 || N!=V */

      ARMCondAL     = 14, /* always (unconditional)        : 1 */
      ARMCondNV     = 15  /* never (unconditional):        : 0 */
      /* NB: ARM have deprecated the use of the NV condition code.
         You are now supposed to use MOV R0,R0 as a noop rather than
         MOVNV R0,R0 as was previously recommended.  Future processors
         may have the NV condition code reused to do other things.  */
   }
   ARMCondcode;

#endif /* ndef __VEX_GUEST_ARM_DEFS_H */

/*---------------------------------------------------------------*/
/*--- end                                    guest_arm_defs.h ---*/
/*---------------------------------------------------------------*/
