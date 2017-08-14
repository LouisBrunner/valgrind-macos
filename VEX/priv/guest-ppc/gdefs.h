
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (guest-ppc/gdefs.h) is                        ---*/
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

/* Only to be used within the guest-ppc directory. */


#ifndef __LIBVEX_GUEST_PPC_DEFS_H
#define __LIBVEX_GUEST_PPC_DEFS_H


/*---------------------------------------------------------*/
/*--- ppc to IR conversion                              ---*/
/*---------------------------------------------------------*/

/* Convert one ppc insn to IR.  See the type DisOneInstrFn in
   bb_to_IR.h. */
extern
DisResult disInstr_PPC ( IRSB*        irbb,
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
IRExpr* guest_ppc32_spechelper ( HChar* function_name,
                                 IRExpr** args );

extern
IRExpr* guest_ppc64_spechelper ( HChar* function_name,
                                 IRExpr** args );

/* Describes to the optimser which part of the guest state require
   precise memory exceptions.  This is logically part of the guest
   state description. */
extern 
Bool guest_ppc32_state_requires_precise_mem_exns ( Int, Int );

extern 
Bool guest_ppc64_state_requires_precise_mem_exns ( Int, Int );

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
   PPCG_FLAG_OP_NUMBER
};


/*---------------------------------------------------------*/
/*--- ppc guest helpers                                 ---*/
/*---------------------------------------------------------*/

/* --- CLEAN HELPERS --- */

/* none, right now */

/* --- DIRTY HELPERS --- */

extern ULong ppcg_dirtyhelper_MFTB ( void );

extern UInt ppc32g_dirtyhelper_MFSPR_268_269 ( UInt );

extern void ppc32g_dirtyhelper_LVS ( VexGuestPPC32State* gst,
                                     UInt vD_idx, UInt sh,
                                     UInt shift_right );

extern void ppc64g_dirtyhelper_LVS ( VexGuestPPC64State* gst,
                                     UInt vD_idx, UInt sh,
                                     UInt shift_right );

#endif /* ndef __LIBVEX_GUEST_PPC_DEFS_H */

/*---------------------------------------------------------------*/
/*--- end                                   guest-ppc/gdefs.h ---*/
/*---------------------------------------------------------------*/
