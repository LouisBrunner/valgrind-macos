
/*--------------------------------------------------------------------*/
/*--- begin                               guest_generic_bb_to_IR.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2004-2017 OpenWorks LLP
      info@open-works.net

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.

   Neither the names of the U.S. Department of Energy nor the
   University of California nor the names of its contributors may be
   used to endorse or promote products derived from this software
   without prior written permission.
*/

#ifndef __VEX_GUEST_GENERIC_BB_TO_IR_H
#define __VEX_GUEST_GENERIC_BB_TO_IR_H

#include "libvex_basictypes.h"
#include "libvex_ir.h"              // IRJumpKind
#include "libvex.h"                 // VexArch

/* This defines stuff needed by the guest insn disassemblers.
   It's a bit circular; is imported by
   - the guest-specific toIR.c files (guest-{x86,amd64,ppc,arm}/toIR.c)
   - the generic disassembly driver (bb_to_IR.c)
   - vex_main.c
*/


/* ---------------------------------------------------------------
   Result of disassembling an instruction
   --------------------------------------------------------------- */

/* The results of disassembling an instruction.  There are three possible
   outcomes.  For Dis_StopHere, the disassembler _must_ terminate the BB.  For
   Dis_Continue, we may at our option either disassemble the next insn, or
   terminate the BB; but in the latter case we must set the bb's ->next field
   to point to the next instruction.  */

typedef

   struct {

      /* The disassembled insn has this length.  Must always be
         set. */
      UInt len;

      /* What happens next?
         Dis_StopHere:  this insn terminates the BB; we must stop.
         Dis_Continue:  we can optionally continue into the next insn
      */
      enum { Dis_StopHere=0x10, Dis_Continue } whatNext;

      /* Any other hints that we should feed back to the disassembler?
         Dis_HintNone:     no hint
         Dis_HintVerbose:  this insn potentially generates a lot of code
      */
      enum { Dis_HintNone=0x20, Dis_HintVerbose } hint;

      /* For whatNext==Dis_StopHere, we need to end the block and create a
         transfer to whatever the NIA is.  That will have presumably
         been set by the IR generated for this insn.  So we need to
         know the jump kind to use.  Should Ijk_INVALID in other Dis_
         cases. */
      IRJumpKind jk_StopHere;

   }

   DisResult;


/* ---------------------------------------------------------------
   The type of a function which disassembles one instruction.
   --------------------------------------------------------------- */

/* A function of this type (DisOneInstrFn) disassembles an instruction
   located at host address &guest_code[delta], whose guest IP is
   guest_IP (this may be entirely unrelated to where the insn is
   actually located in the host's address space.).  The returned
   DisResult.len field carries its size.

   The resulting IR is added to the end of irsb.
*/

typedef

   DisResult (*DisOneInstrFn) ( 

      /* This is the IRSB to which the resulting IR is to be appended. */
      /*OUT*/ IRSB*        irsb,

      /* Where is the guest code? */
      /*IN*/  const UChar* guest_code,

      /* Where is the actual insn?  Note: it's at &guest_code[delta] */
      /*IN*/  Long         delta,

      /* What is the guest IP of the insn? */
      /*IN*/  Addr         guest_IP,

      /* Info about the guest architecture */
      /*IN*/  VexArch      guest_arch,
      /*IN*/  const VexArchInfo* archinfo,

      /* ABI info for both guest and host */
      /*IN*/  const VexAbiInfo*  abiinfo,

      /* The endianness of the host */
      /*IN*/  VexEndness   host_endness,

      /* Should diagnostics be printed for illegal instructions? */
      /*IN*/  Bool         sigill_diag

   );


/* ---------------------------------------------------------------
   Top-level BB to IR conversion fn.
   --------------------------------------------------------------- */

/* See detailed comment in guest_generic_bb_to_IR.c. */
extern
IRSB* bb_to_IR ( 
         /*OUT*/VexGuestExtents* vge,
         /*OUT*/UInt*            n_sc_extents,
         /*OUT*/UInt*            n_guest_instrs, /* stats only */
         /*OUT*/UShort*          n_uncond_in_trace, /* stats only */
         /*OUT*/UShort*          n_cond_in_trace, /* stats only */
         /*MOD*/VexRegisterUpdates* pxControl,
         /*IN*/ void*            callback_opaque,
         /*IN*/ DisOneInstrFn    dis_instr_fn,
         /*IN*/ const UChar*     guest_code,
         /*IN*/ Addr             guest_IP_bbstart,
         /*IN*/ Bool             (*chase_into_ok)(void*,Addr),
         /*IN*/ VexEndness       host_endness,
         /*IN*/ Bool             sigill_diag,
         /*IN*/ VexArch          arch_guest,
         /*IN*/ const VexArchInfo* archinfo_guest,
         /*IN*/ const VexAbiInfo*  abiinfo_both,
         /*IN*/ IRType           guest_word_type,
         /*IN*/ UInt             (*needs_self_check)
                                    (void*, /*MB_MOD*/VexRegisterUpdates*,
                                            const VexGuestExtents*),
         /*IN*/ Bool             (*preamble_function)(void*,IRSB*),
         /*IN*/ Int              offB_GUEST_CMSTART,
         /*IN*/ Int              offB_GUEST_CMLEN,
         /*IN*/ Int              offB_GUEST_IP,
         /*IN*/ Int              szB_GUEST_IP
      );


#endif /* ndef __VEX_GUEST_GENERIC_BB_TO_IR_H */

/*--------------------------------------------------------------------*/
/*--- end                                 guest_generic_bb_to_IR.h ---*/
/*--------------------------------------------------------------------*/
