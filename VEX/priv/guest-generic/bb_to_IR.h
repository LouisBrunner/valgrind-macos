
/*--------------------------------------------------------------------*/
/*---                                                              ---*/
/*--- This file (guest-generic/bb_to_IR.h) is                      ---*/
/*--- Copyright (c) OpenWorks LLP.  All rights reserved.           ---*/
/*---                                                              ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of LibVEX, a library for dynamic binary
   instrumentation and translation.

   Copyright (C) 2004-2005 OpenWorks LLP.

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

#ifndef __LIBVEX_GENERIC_BB_TO_IR_H
#define __LIBVEX_GENERIC_BB_TO_IR_H


/* This defines stuff needed by the guest insn disassemblers.
   It's a bit circular; is imported by
   - the guest-specific toIR.c files (guest-{x86,amd64,ppc32,arm}/toIR.c)
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

      /* This is the IRBB to which the resulting IR is to be appended. */
      /*OUT*/ IRBB*        irbb,

      /* Do we need to generate IR to set the guest IP for this insn,
         or not? */
      /*IN*/  Bool         put_IP,

      /* Return True iff resteering to the given addr is allowed */
      /*IN*/  Bool         (*resteerOkFn) ( Addr64 ),

      /* Where is the guest code? */
      /*IN*/  UChar*       guest_code,

      /* Where is the actual insn?  Note: it's at &guest_code[delta] */
      /*IN*/  Long         delta,

      /* What is the guest IP of the insn? */
      /*IN*/  Addr64       guest_IP,

      /* Info about the guest architecture */
      /*IN*/  VexArchInfo* archinfo,

      /* Is the host bigendian? */
      /*IN*/  Bool         host_bigendian

   );


/* ---------------------------------------------------------------
   Top-level BB to IR conversion fn.
   --------------------------------------------------------------- */

/* See detailed comment in bb_to_IR.c. */
extern
IRBB* bb_to_IR ( /*OUT*/VexGuestExtents* vge,
                 /*IN*/ DisOneInstrFn    dis_instr_fn,
                 /*IN*/ UChar*           guest_code,
                 /*IN*/ Addr64           guest_IP_bbstart,
                 /*IN*/ Bool             (*chase_into_ok)(Addr64),
                 /*IN*/ Bool             host_bigendian,
                 /*IN*/ VexArchInfo*     archinfo_guest,
                 /*IN*/ IRType           guest_word_type,
                 /*IN*/ Bool             do_self_check,
                 /*IN*/ Int              offB_TISTART,
                 /*IN*/ Int              offB_TILEN );


#endif /* ndef GENERIC_BB_TO_IR_H */

/*--------------------------------------------------------------------*/
/*--- end                                 guest-generic/bb_to_IR.h ---*/
/*--------------------------------------------------------------------*/
