
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (guest_arm_defs.h) is                         ---*/
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

/* Only to be used within the guest-arm directory. */

#ifndef __VEX_GUEST_ARM_DEFS_H
#define __VEX_GUEST_ARM_DEFS_H


/*---------------------------------------------------------*/
/*--- arm to IR conversion                              ---*/
/*---------------------------------------------------------*/

/* Convert one ARM insn to IR.  See the type DisOneInstrFn in
   bb_to_IR.h. */
extern
DisResult disInstr_ARM ( IRSB*        irbb,
                         Bool         put_IP,
                         Bool         (*resteerOkFn) ( void*, Addr64 ),
                         void*        callback_opaque,
                         UChar*       guest_code,
                         Long         delta,
                         Addr64       guest_IP,
                         VexArch      guest_arch,
                         VexArchInfo* archinfo,
                         VexAbiInfo*  abiinfo,
                         Bool         host_bigendian );

/* Used by the optimiser to specialise calls to helpers. */
extern
IRExpr* guest_arm_spechelper ( HChar* function_name,
                               IRExpr** args );

/* Describes to the optimser which part of the guest state require
   precise memory exceptions.  This is logically part of the guest
   state description. */
extern 
Bool guest_arm_state_requires_precise_mem_exns ( Int, Int );

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


/*---------------------------------------------------------*/
/*--- Condition code stuff                              ---*/
/*---------------------------------------------------------*/

/* Flags masks.  Defines positions of flags bits in the CPSR. */
#define ARMG_CC_SHIFT_N  31
#define ARMG_CC_SHIFT_Z  30
#define ARMG_CC_SHIFT_C  29
#define ARMG_CC_SHIFT_V  28

#define ARMG_CC_MASK_N    (1 << ARMG_CC_SHIFT_N)
#define ARMG_CC_MASK_Z    (1 << ARMG_CC_SHIFT_Z)
#define ARMG_CC_MASK_C    (1 << ARMG_CC_SHIFT_C)
#define ARMG_CC_MASK_V    (1 << ARMG_CC_SHIFT_V)

/* Flag thunk descriptors.  A four-word thunk is used to record
   details of the most recent flag-setting operation, so the flags can
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

   A summary of the field usages is:

   OP                DEP1              DEP2              DEP3
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   OP_COPY           current NZCV      unused            unused
   OP_ADD            argL              argR              unused
   OP_SUB            argL              argR              unused
   OP_ADC            argL              argR              old_C
   OP_SBB            argL              argR              old_C
   OP_LOGIC          result            shifter_co        old_V
   OP_MUL            result            unused            old_C:old_V
   OP_MULL           resLO32           resHI32           old_C:old_V
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
