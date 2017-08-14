
/*--------------------------------------------------------------------*/
/*---                                                              ---*/
/*--- This file (guest-generic/bb_to_IR.h) is                      ---*/
/*--- Copyright (C) OpenWorks LLP.  All rights reserved.           ---*/
/*---                                                              ---*/
/*--------------------------------------------------------------------*/

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

#ifndef __LIBVEX_GENERIC_BB_TO_IR_H
#define __LIBVEX_GENERIC_BB_TO_IR_H


/* This defines stuff needed by the guest insn disassemblers.
   It's a bit circular; is imported by
   - the guest-specific toIR.c files (guest-{x86,amd64,ppc,arm}/toIR.c)
   - the generic disassembly driver (bb_to_IR.c)
   - vex_main.c
*/


/* ---------------------------------------------------------------
   Result of disassembling an instruction
   --------------------------------------------------------------- */

/* The results of disassembling an instruction.  There are three
   possible outcomes.  For Dis_Resteer, the disassembler _must_
   continue at the specified address.  For Dis_StopHere, the
   disassembler _must_ terminate the BB.  For Dis_Continue, we may at
   our option either disassemble the next insn, or terminate the BB;
   but in the latter case we must set the bb's ->next field to point
   to the next instruction.  */

typedef

   struct {

      /* The disassembled insn has this length.  Must always be
         set. */
      Int len;

      /* What happens next?
         Dis_StopHere:  this insn terminates the BB; we must stop.
         Dis_Continue:  we can optionally continue into the next insn
         Dis_Resteer:   followed a branch; continue at the spec'd addr
      */
      enum { Dis_StopHere, Dis_Continue, Dis_Resteer } whatNext;

      /* For Dis_Resteer, this is the guest address we should continue
         at.  Otherwise ignored (should be zero). */
      Addr64 continueAt;

   }

   DisResult;


/* ---------------------------------------------------------------
   The type of a function which disassembles one instruction.
   C's function-type syntax is really astonishing bizarre.
   --------------------------------------------------------------- */

/* A function of this type (DisOneInstrFn) disassembles an instruction
   located at host address &guest_code[delta], whose guest IP is
   guest_IP (this may be entirely unrelated to where the insn is
   actually located in the host's address space.).  The returned
   DisResult.len field carries its size.  If the returned
   DisResult.whatNext field is Dis_Resteer then DisResult.continueAt
   should hold the guest IP of the next insn to disassemble.

   disInstr is not permitted to return Dis_Resteer if resteerOkFn,
   when applied to the address which it wishes to resteer into,
   returns False.  

   The resulting IR is added to the end of irbb.
*/

typedef

   DisResult (*DisOneInstrFn) ( 

      /* This is the IRSB to which the resulting IR is to be appended. */
      /*OUT*/ IRSB*        irbb,

      /* Do we need to generate IR to set the guest IP for this insn,
         or not? */
      /*IN*/  Bool         put_IP,

      /* Return True iff resteering to the given addr is allowed */
      /*IN*/  Bool         (*resteerOkFn) ( /*opaque*/void*, Addr64 ),

      /* Vex-opaque data passed to all caller (valgrind) supplied
         callbacks. */
      /*IN*/  void*        callback_opaque,

      /* Where is the guest code? */
      /*IN*/  UChar*       guest_code,

      /* Where is the actual insn?  Note: it's at &guest_code[delta] */
      /*IN*/  Long         delta,

      /* What is the guest IP of the insn? */
      /*IN*/  Addr64       guest_IP,

      /* Info about the guest architecture */
      /*IN*/  VexArch      guest_arch,
      /*IN*/  VexArchInfo* archinfo,

      /* ABI info for both guest and host */
      /*IN*/  VexAbiInfo*  abiinfo,

      /* Is the host bigendian? */
      /*IN*/  Bool         host_bigendian

   );


/* ---------------------------------------------------------------
   Top-level BB to IR conversion fn.
   --------------------------------------------------------------- */

/* See detailed comment in bb_to_IR.c. */
extern
IRSB* bb_to_IR ( /*OUT*/VexGuestExtents* vge,
                 /*IN*/ void*            closure_opaque,
                 /*IN*/ DisOneInstrFn    dis_instr_fn,
                 /*IN*/ UChar*           guest_code,
                 /*IN*/ Addr64           guest_IP_bbstart,
                 /*IN*/ Bool             (*chase_into_ok)(void*,Addr64),
                 /*IN*/ Bool             host_bigendian,
                 /*IN*/ VexArch          arch_guest,
                 /*IN*/ VexArchInfo*     archinfo_guest,
                 /*IN*/ VexAbiInfo*      abiinfo_both,
                 /*IN*/ IRType           guest_word_type,
                 /*IN*/ Bool             do_self_check,
                 /*IN*/ Bool             (*preamble_function)(void*,IRSB*),
                 /*IN*/ Int              offB_TISTART,
                 /*IN*/ Int              offB_TILEN );


#endif /* ndef GENERIC_BB_TO_IR_H */

/*--------------------------------------------------------------------*/
/*--- end                                 guest-generic/bb_to_IR.h ---*/
/*--------------------------------------------------------------------*/
