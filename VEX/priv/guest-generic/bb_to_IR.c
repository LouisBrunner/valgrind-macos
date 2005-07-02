
/*--------------------------------------------------------------------*/
/*---                                                              ---*/
/*--- This file (guest-generic/bb_to_IR.c) is                      ---*/
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

#include "libvex_basictypes.h"
#include "libvex_ir.h"
#include "libvex.h"
#include "main/vex_util.h"
#include "main/vex_globals.h"
#include "guest-generic/bb_to_IR.h"


/* Disassemble a complete basic block, starting at guest_IP_start, 
   returning a new IRBB.  The disassembler may chase across basic
   block boundaries if it wishes and if chase_into_ok allows it.
   The precise guest address ranges from which code has been taken
   are written into vge.  guest_IP_start is taken to be the IP in
   the guest's address space corresponding to the instruction at
   &guest_code[0].  

   dis_instr_fn is the arch-specific fn to disassemble on function; it
   is this that does the real work.
*/

static Bool const_False ( Addr64 a ) { return False; }

IRBB* bb_to_IR ( /*OUT*/VexGuestExtents* vge,
                 /*IN*/ DisOneInstrFn    dis_instr_fn,
                 /*IN*/ UChar*           guest_code,
                 /*IN*/ Addr64           guest_IP_bbstart,
                 /*IN*/ Bool             (*chase_into_ok)(Addr64),
                 /*IN*/ Bool             host_bigendian,
                 /*IN*/ VexArchInfo*     archinfo_guest,
                 /*IN*/ IRType           guest_word_type )
{
   Long       delta;
   Int        i, n_instrs, first_stmt_idx;
   Bool       resteerOK, need_to_put_IP, debug_print;
   DisResult  dres;
   IRStmt*    imark;
   static Int n_resteers = 0;
   Int        d_resteers = 0;
   IRBB*      irbb;
   Addr64     guest_IP_curr_instr;

   Bool (*resteerOKfn)(Addr64) = NULL;

   debug_print = toBool(vex_traceflags & VEX_TRACE_FE);

   /* check sanity .. */
   vassert(vex_control.guest_max_insns >= 1);
   vassert(vex_control.guest_max_insns < 500);
   vassert(vex_control.guest_chase_thresh >= 0);
   vassert(vex_control.guest_chase_thresh < vex_control.guest_max_insns);
   vassert(guest_word_type == Ity_I32 || guest_word_type == Ity_I64);

   /* Start a new, empty extent. */
   vge->n_used  = 1;
   vge->base[0] = guest_IP_bbstart;
   vge->len[0]  = 0;

   /* And a new IR BB to dump the result into. */
   irbb = emptyIRBB();

   /* Delta keeps track of how far along the guest_code array we have
      so far gone. */
   delta    = 0;
   n_instrs = 0;

   while (True) {
      vassert(n_instrs < vex_control.guest_max_insns);

      /* Regardless of what chase_into_ok says, is chasing permissible
         at all right now?  Set resteerOKfn accordingly. */
      resteerOK 
         = toBool(
              n_instrs < vex_control.guest_chase_thresh
              /* we can't afford to have a resteer once we're on the
                 last extent slot. */
              && vge->n_used < 3
           );

      resteerOKfn
         = resteerOK ? chase_into_ok : const_False;

      /* This is the IP of the instruction we're just about to deal
         with. */
      guest_IP_curr_instr = guest_IP_bbstart + delta;

      /* This is the irbb statement array index of the first stmt in
         this insn.  That will always be the instruction-mark
         descriptor. */
      first_stmt_idx = irbb->stmts_used;

      /* Add an instruction-mark statement.  We won't know until after
         disassembling the instruction how long it instruction is, so
         just put in a zero length and we'll fix it up later. */
      addStmtToIRBB( irbb, IRStmt_IMark( guest_IP_curr_instr, 0 ));

      /* for the first insn, the dispatch loop will have set
         %IP, but for all the others we have to do it ourselves. */
      need_to_put_IP = toBool(n_instrs > 0);

      /* Finally, actually disassemble an instruction. */
      dres = dis_instr_fn ( irbb,
                            need_to_put_IP,
                            resteerOKfn,
                            guest_code,
                            delta,
                            guest_IP_curr_instr,
                            archinfo_guest,
                            host_bigendian );

      /* stay sane ... */
      vassert(dres.whatNext == Dis_StopHere
              || dres.whatNext == Dis_Continue
              || dres.whatNext == Dis_Resteer);
      vassert(dres.len >= 0 && dres.len <= 18);
      if (dres.whatNext != Dis_Resteer)
         vassert(dres.continueAt == 0);

      /* Fill in the insn-mark length field. */
      vassert(first_stmt_idx >= 0 && first_stmt_idx < irbb->stmts_used);
      imark = irbb->stmts[first_stmt_idx];
      vassert(imark);
      vassert(imark->tag == Ist_IMark);
      vassert(imark->Ist.IMark.len == 0);
      imark->Ist.IMark.len = toUInt(dres.len);

      /* Print the resulting IR, if needed. */
      if (vex_traceflags & VEX_TRACE_FE) {
         for (i = first_stmt_idx; i < irbb->stmts_used; i++) {
            vex_printf("              ");
            ppIRStmt(irbb->stmts[i]);
            vex_printf("\n");
         }
      }

      /* If dis_instr_fn terminated the BB at this point, check it
	 also filled in the irbb->next field. */
      if (dres.whatNext == Dis_StopHere) {
         vassert(irbb->next != NULL);
         if (debug_print) {
            vex_printf("              ");
            vex_printf( "goto {");
            ppIRJumpKind(irbb->jumpkind);
            vex_printf( "} ");
            ppIRExpr( irbb->next );
            vex_printf( "\n");
         }
      }

      /* Update the VexGuestExtents we are constructing. */
      /* If vex_control.guest_max_insns is required to be < 500 and
	 each insn is at max 15 bytes long, this limit of 10000 then
	 seems reasonable since the max possible extent length will be
	 500 * 15 == 7500. */
      vassert(vge->len[vge->n_used-1] < 10000);
      vge->len[vge->n_used-1] 
         = toUShort(toUInt( vge->len[vge->n_used-1] + dres.len ));
      n_instrs++;
      if (debug_print) 
         vex_printf("\n");

      /* Advance delta (inconspicuous but very important :-) */
      delta += (Long)dres.len;

      switch (dres.whatNext) {
         case Dis_Continue:
            vassert(irbb->next == NULL);
            if (n_instrs < vex_control.guest_max_insns) {
               /* keep going */
            } else {
               /* We have to stop. */
               irbb->next 
                  = IRExpr_Const(
                       guest_word_type == Ity_I32
                          ? IRConst_U32(toUInt(guest_IP_bbstart+delta))
                          : IRConst_U64(guest_IP_bbstart+delta)
                    );
               return irbb;
            }
            break;
         case Dis_StopHere:
            vassert(irbb->next != NULL);
            return irbb;
         case Dis_Resteer:
            /* Check that we actually allowed a resteer .. */
            vassert(resteerOK);
            vassert(irbb->next == NULL);
            /* figure out a new delta to continue at. */
            vassert(resteerOKfn(dres.continueAt));
            delta = dres.continueAt - guest_IP_bbstart;
            /* we now have to start a new extent slot. */
	    vge->n_used++;
	    vassert(vge->n_used <= 3);
            vge->base[vge->n_used-1] = dres.continueAt;
            vge->len[vge->n_used-1] = 0;
            n_resteers++;
            d_resteers++;
            if (0 && (n_resteers & 0xFF) == 0)
            vex_printf("resteer[%d,%d] to 0x%llx (delta = %lld)\n",
                       n_resteers, d_resteers,
                       dres.continueAt, delta);
            break;
         default:
            vpanic("bb_to_IR");
      }
   }
}



/*--------------------------------------------------------------------*/
/*--- end                                 guest-generic/bb_to_IR.c ---*/
/*--------------------------------------------------------------------*/
