
/*--------------------------------------------------------------------*/
/*--- begin                               guest_generic_bb_to_IR.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2004-2010 OpenWorks LLP
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

#include "libvex_basictypes.h"
#include "libvex_ir.h"
#include "libvex.h"
#include "main_util.h"
#include "main_globals.h"
#include "guest_generic_bb_to_IR.h"


/* Forwards .. */
VEX_REGPARM(2)
static UInt genericg_compute_checksum_4al ( HWord first_w32, HWord n_w32s );
VEX_REGPARM(1)
static UInt genericg_compute_checksum_4al_1 ( HWord first_w32 );
VEX_REGPARM(1)
static UInt genericg_compute_checksum_4al_2 ( HWord first_w32 );
VEX_REGPARM(1)
static UInt genericg_compute_checksum_4al_3 ( HWord first_w32 );
VEX_REGPARM(1)
static UInt genericg_compute_checksum_4al_4 ( HWord first_w32 );
VEX_REGPARM(1)
static UInt genericg_compute_checksum_4al_5 ( HWord first_w32 );
VEX_REGPARM(1)
static UInt genericg_compute_checksum_4al_6 ( HWord first_w32 );
VEX_REGPARM(1)
static UInt genericg_compute_checksum_4al_7 ( HWord first_w32 );
VEX_REGPARM(1)
static UInt genericg_compute_checksum_4al_8 ( HWord first_w32 );
VEX_REGPARM(1)
static UInt genericg_compute_checksum_4al_9 ( HWord first_w32 );
VEX_REGPARM(1)
static UInt genericg_compute_checksum_4al_10 ( HWord first_w32 );
VEX_REGPARM(1)
static UInt genericg_compute_checksum_4al_11 ( HWord first_w32 );
VEX_REGPARM(1)
static UInt genericg_compute_checksum_4al_12 ( HWord first_w32 );

VEX_REGPARM(2)
static ULong genericg_compute_checksum_8al ( HWord first_w64, HWord n_w64s );
VEX_REGPARM(1)
static ULong genericg_compute_checksum_8al_1 ( HWord first_w64 );
VEX_REGPARM(1)
static ULong genericg_compute_checksum_8al_2 ( HWord first_w64 );
VEX_REGPARM(1)
static ULong genericg_compute_checksum_8al_3 ( HWord first_w64 );
VEX_REGPARM(1)
static ULong genericg_compute_checksum_8al_4 ( HWord first_w64 );
VEX_REGPARM(1)
static ULong genericg_compute_checksum_8al_5 ( HWord first_w64 );
VEX_REGPARM(1)
static ULong genericg_compute_checksum_8al_6 ( HWord first_w64 );
VEX_REGPARM(1)
static ULong genericg_compute_checksum_8al_7 ( HWord first_w64 );
VEX_REGPARM(1)
static ULong genericg_compute_checksum_8al_8 ( HWord first_w64 );
VEX_REGPARM(1)
static ULong genericg_compute_checksum_8al_9 ( HWord first_w64 );
VEX_REGPARM(1)
static ULong genericg_compute_checksum_8al_10 ( HWord first_w64 );
VEX_REGPARM(1)
static ULong genericg_compute_checksum_8al_11 ( HWord first_w64 );
VEX_REGPARM(1)
static ULong genericg_compute_checksum_8al_12 ( HWord first_w64 );

/* Small helpers */
static Bool const_False ( void* callback_opaque, Addr64 a ) { 
   return False; 
}

/* Disassemble a complete basic block, starting at guest_IP_start, 
   returning a new IRSB.  The disassembler may chase across basic
   block boundaries if it wishes and if chase_into_ok allows it.
   The precise guest address ranges from which code has been taken
   are written into vge.  guest_IP_bbstart is taken to be the IP in
   the guest's address space corresponding to the instruction at
   &guest_code[0].  

   dis_instr_fn is the arch-specific fn to disassemble on function; it
   is this that does the real work.

   do_self_check indicates that the caller needs a self-checking
   translation.

   preamble_function is a callback which allows the caller to add
   its own IR preamble (following the self-check, if any).  May be
   NULL.  If non-NULL, the IRSB under construction is handed to 
   this function, which presumably adds IR statements to it.  The
   callback may optionally complete the block and direct bb_to_IR
   not to disassemble any instructions into it; this is indicated
   by the callback returning True.

   offB_TIADDR and offB_TILEN are the offsets of guest_TIADDR and
   guest_TILEN.  Since this routine has to work for any guest state,
   without knowing what it is, those offsets have to passed in.

   callback_opaque is a caller-supplied pointer to data which the
   callbacks may want to see.  Vex has no idea what it is.
   (In fact it's a VgInstrumentClosure.)
*/

IRSB* bb_to_IR ( /*OUT*/VexGuestExtents* vge,
                 /*IN*/ void*            callback_opaque,
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
                 /*IN*/ Int              offB_TILEN )
{
   Long       delta;
   Int        i, n_instrs, first_stmt_idx;
   Bool       resteerOK, need_to_put_IP, debug_print;
   DisResult  dres;
   IRStmt*    imark;
   static Int n_resteers = 0;
   Int        d_resteers = 0;
   Int        selfcheck_idx = 0;
   IRSB*      irsb;
   Addr64     guest_IP_curr_instr;
   IRConst*   guest_IP_bbstart_IRConst = NULL;
   Int        n_cond_resteers_allowed = 2;

   Bool (*resteerOKfn)(void*,Addr64) = NULL;

   debug_print = toBool(vex_traceflags & VEX_TRACE_FE);

   /* Note: for adler32 to work without % operation for the self
      check, need to limit length of stuff it scans to 5552 bytes.
      Therefore limiting the max bb len to 100 insns seems generously
      conservative. */

   /* check sanity .. */
   vassert(sizeof(HWord) == sizeof(void*));
   vassert(vex_control.guest_max_insns >= 1);
   vassert(vex_control.guest_max_insns < 100);
   vassert(vex_control.guest_chase_thresh >= 0);
   vassert(vex_control.guest_chase_thresh < vex_control.guest_max_insns);
   vassert(guest_word_type == Ity_I32 || guest_word_type == Ity_I64);

   /* Start a new, empty extent. */
   vge->n_used  = 1;
   vge->base[0] = guest_IP_bbstart;
   vge->len[0]  = 0;

   /* And a new IR superblock to dump the result into. */
   irsb = emptyIRSB();

   /* Delta keeps track of how far along the guest_code array we have
      so far gone. */
   delta    = 0;
   n_instrs = 0;

   /* Guest addresses as IRConsts.  Used in the two self-checks
      generated. */
   if (do_self_check) {
      guest_IP_bbstart_IRConst
         = guest_word_type==Ity_I32 
              ? IRConst_U32(toUInt(guest_IP_bbstart))
              : IRConst_U64(guest_IP_bbstart);
   }

   /* If asked to make a self-checking translation, leave 15 spaces in
      which to put the check statements (up to 3 extents, and 5 stmts
      required for each).  We'll fill them in later when we know the
      extents and checksums of the areas to check. */
   if (do_self_check) {
      selfcheck_idx = irsb->stmts_used;
      for (i = 0; i < 3 * 5; i++)
         addStmtToIRSB( irsb, IRStmt_NoOp() );
   }

   /* If the caller supplied a function to add its own preamble, use
      it now. */
   if (preamble_function) {
      Bool stopNow = preamble_function( callback_opaque, irsb );
      if (stopNow) {
         /* The callback has completed the IR block without any guest
            insns being disassembled into it, so just return it at
            this point, even if a self-check was requested - as there
            is nothing to self-check.  The five self-check no-ops will
            still be in place, but they are harmless. */
         return irsb;
      }
   }

   /* Process instructions. */
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

      /* n_cond_resteers_allowed keeps track of whether we're still
         allowing dis_instr_fn to chase conditional branches.  It
         starts (at 2) and gets decremented each time dis_instr_fn
         tells us it has chased a conditional branch.  We then
         decrement it, and use it to tell later calls to dis_instr_fn
         whether or not it is allowed to chase conditional
         branches. */
      vassert(n_cond_resteers_allowed >= 0 && n_cond_resteers_allowed <= 2);

      /* This is the IP of the instruction we're just about to deal
         with. */
      guest_IP_curr_instr = guest_IP_bbstart + delta;

      /* This is the irsb statement array index of the first stmt in
         this insn.  That will always be the instruction-mark
         descriptor. */
      first_stmt_idx = irsb->stmts_used;

      /* Add an instruction-mark statement.  We won't know until after
         disassembling the instruction how long it instruction is, so
         just put in a zero length and we'll fix it up later.

         On ARM, the least significant bit of the instr address
         distinguishes ARM vs Thumb instructions.  All instructions
         actually start on at least 2-aligned addresses.  So we need
         to ignore the bottom bit of the insn address when forming the
         IMark.  For more details of this convention, see comments on
         definition of guest_R15 in libvex_guest_arm.h. */
      addStmtToIRSB( irsb,
                     IRStmt_IMark( arch_guest == VexArchARM
                                      ? (guest_IP_curr_instr & ~(Addr64)1)
                                      : guest_IP_curr_instr,
                                   0
                     )
      );

      /* for the first insn, the dispatch loop will have set
         %IP, but for all the others we have to do it ourselves. */
      need_to_put_IP = toBool(n_instrs > 0);

      /* Finally, actually disassemble an instruction. */
      dres = dis_instr_fn ( irsb,
                            need_to_put_IP,
                            resteerOKfn,
                            toBool(n_cond_resteers_allowed > 0),
                            callback_opaque,
                            guest_code,
                            delta,
                            guest_IP_curr_instr,
                            arch_guest,
                            archinfo_guest,
                            abiinfo_both,
                            host_bigendian );

      /* stay sane ... */
      vassert(dres.whatNext == Dis_StopHere
              || dres.whatNext == Dis_Continue
              || dres.whatNext == Dis_ResteerU
              || dres.whatNext == Dis_ResteerC);
      /* ... disassembled insn length is sane ... */
      vassert(dres.len >= 0 && dres.len <= 20);
      /* ... continueAt is zero if no resteer requested ... */
      if (dres.whatNext != Dis_ResteerU && dres.whatNext != Dis_ResteerC)
         vassert(dres.continueAt == 0);
      /* ... if we disallowed conditional resteers, check that one
             didn't actually happen anyway ... */
      if (n_cond_resteers_allowed == 0)
         vassert(dres.whatNext != Dis_ResteerC);

      /* Fill in the insn-mark length field. */
      vassert(first_stmt_idx >= 0 && first_stmt_idx < irsb->stmts_used);
      imark = irsb->stmts[first_stmt_idx];
      vassert(imark);
      vassert(imark->tag == Ist_IMark);
      vassert(imark->Ist.IMark.len == 0);
      imark->Ist.IMark.len = toUInt(dres.len);

      /* Print the resulting IR, if needed. */
      if (vex_traceflags & VEX_TRACE_FE) {
         for (i = first_stmt_idx; i < irsb->stmts_used; i++) {
            vex_printf("              ");
            ppIRStmt(irsb->stmts[i]);
            vex_printf("\n");
         }
      }

      /* If dis_instr_fn terminated the BB at this point, check it
         also filled in the irsb->next field. */
      if (dres.whatNext == Dis_StopHere) {
         vassert(irsb->next != NULL);
         if (debug_print) {
            vex_printf("              ");
            vex_printf( "goto {");
            ppIRJumpKind(irsb->jumpkind);
            vex_printf( "} ");
            ppIRExpr( irsb->next );
            vex_printf( "\n");
         }
      }

      /* Update the VexGuestExtents we are constructing. */
      /* If vex_control.guest_max_insns is required to be < 100 and
         each insn is at max 20 bytes long, this limit of 5000 then
         seems reasonable since the max possible extent length will be
         100 * 20 == 2000. */
      vassert(vge->len[vge->n_used-1] < 5000);
      vge->len[vge->n_used-1] 
         = toUShort(toUInt( vge->len[vge->n_used-1] + dres.len ));
      n_instrs++;
      if (debug_print) 
         vex_printf("\n");

      /* Advance delta (inconspicuous but very important :-) */
      delta += (Long)dres.len;

      switch (dres.whatNext) {
         case Dis_Continue:
            vassert(irsb->next == NULL);
            if (n_instrs < vex_control.guest_max_insns) {
               /* keep going */
            } else {
               /* We have to stop. */
               irsb->next 
                  = IRExpr_Const(
                       guest_word_type == Ity_I32
                          ? IRConst_U32(toUInt(guest_IP_bbstart+delta))
                          : IRConst_U64(guest_IP_bbstart+delta)
                    );
               goto done;
            }
            break;
         case Dis_StopHere:
            vassert(irsb->next != NULL);
            goto done;
         case Dis_ResteerU:
         case Dis_ResteerC:
            /* Check that we actually allowed a resteer .. */
            vassert(resteerOK);
            vassert(irsb->next == NULL);
            if (dres.whatNext == Dis_ResteerC) {
               vassert(n_cond_resteers_allowed > 0);
               n_cond_resteers_allowed--;
            }
            /* figure out a new delta to continue at. */
            vassert(resteerOKfn(callback_opaque,dres.continueAt));
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
   /*NOTREACHED*/
   vassert(0);

  done:
   /* We're done.  The only thing that might need attending to is that
      a self-checking preamble may need to be created.

      The scheme is to compute a rather crude checksum of the code
      we're making a translation of, and add to the IR a call to a
      helper routine which recomputes the checksum every time the
      translation is run, and requests a retranslation if it doesn't
      match.  This is obviously very expensive and considerable
      efforts are made to speed it up:

      * the checksum is computed from all the 32-bit words that
        overlap the translated code.  That means it could depend on up
        to 3 bytes before and 3 bytes after which aren't part of the
        translated area, and so if those change then we'll
        unnecessarily have to discard and retranslate.  This seems
        like a pretty remote possibility and it seems as if the
        benefit of not having to deal with the ends of the range at
        byte precision far outweigh any possible extra translations
        needed.

      * there's a generic routine and 12 specialised cases, which
        handle the cases of 1 through 12-word lengths respectively.
        They seem to cover about 90% of the cases that occur in
        practice.
   */
   if (do_self_check) {

      Addr64   base2check;
      UInt     len2check;
      HWord    expectedhW;
      IRTemp   tistart_tmp, tilen_tmp;
      HWord    VEX_REGPARM(2) (*fn_generic)(HWord, HWord);
      HWord    VEX_REGPARM(1) (*fn_spec)(HWord);
      HChar*   nm_generic;
      HChar*   nm_spec;
      HWord    fn_generic_entry = 0;
      HWord    fn_spec_entry = 0;
      UInt     host_word_szB = sizeof(HWord);
      IRType   host_word_type = Ity_INVALID;

      if (host_word_szB == 4) host_word_type = Ity_I32;
      if (host_word_szB == 8) host_word_type = Ity_I64;
      vassert(host_word_type != Ity_INVALID);

      vassert(vge->n_used >= 1 && vge->n_used <= 3);
      for (i = 0; i < vge->n_used; i++) {

         /* the extent we're generating a check for */
         base2check = vge->base[i];
         len2check  = vge->len[i];

         /* stay sane */
         vassert(len2check >= 0 && len2check < 1000/*arbitrary*/);

         /* Skip the check if the translation involved zero bytes */
         if (len2check == 0)
            continue;

         HWord first_hW = ((HWord)base2check)
                          & ~(HWord)(host_word_szB-1);
         HWord last_hW  = (((HWord)base2check) + len2check - 1)
                          & ~(HWord)(host_word_szB-1);
         vassert(first_hW <= last_hW);
         HWord hW_diff = last_hW - first_hW;
         vassert(0 == (hW_diff & (host_word_szB-1)));
         HWord hWs_to_check = (hW_diff + host_word_szB) / host_word_szB;
         vassert(hWs_to_check > 0
                 && hWs_to_check < 1004/*arbitrary*/ / host_word_szB);

         /* vex_printf("%lx %lx  %ld\n", first_w32, last_w32, w32s_to_check); */

         if (host_word_szB == 8) {
            fn_generic =  (VEX_REGPARM(2) HWord(*)(HWord, HWord))
                          genericg_compute_checksum_8al;
            nm_generic = "genericg_compute_checksum_8al";
         } else {
            fn_generic =  (VEX_REGPARM(2) HWord(*)(HWord, HWord))
                          genericg_compute_checksum_4al;
            nm_generic = "genericg_compute_checksum_4al";
         }

         fn_spec = NULL;
         nm_spec = NULL;

         if (host_word_szB == 8) {
            HChar* nm = NULL;
            ULong  VEX_REGPARM(1) (*fn)(HWord)  = NULL;
            switch (hWs_to_check) {
               case 1:  fn =  genericg_compute_checksum_8al_1;
                        nm = "genericg_compute_checksum_8al_1"; break;
               case 2:  fn =  genericg_compute_checksum_8al_2;
                        nm = "genericg_compute_checksum_8al_2"; break;
               case 3:  fn =  genericg_compute_checksum_8al_3;
                        nm = "genericg_compute_checksum_8al_3"; break;
               case 4:  fn =  genericg_compute_checksum_8al_4;
                        nm = "genericg_compute_checksum_8al_4"; break;
               case 5:  fn =  genericg_compute_checksum_8al_5;
                        nm = "genericg_compute_checksum_8al_5"; break;
               case 6:  fn =  genericg_compute_checksum_8al_6;
                        nm = "genericg_compute_checksum_8al_6"; break;
               case 7:  fn =  genericg_compute_checksum_8al_7;
                        nm = "genericg_compute_checksum_8al_7"; break;
               case 8:  fn =  genericg_compute_checksum_8al_8;
                        nm = "genericg_compute_checksum_8al_8"; break;
               case 9:  fn =  genericg_compute_checksum_8al_9;
                        nm = "genericg_compute_checksum_8al_9"; break;
               case 10: fn =  genericg_compute_checksum_8al_10;
                        nm = "genericg_compute_checksum_8al_10"; break;
               case 11: fn =  genericg_compute_checksum_8al_11;
                        nm = "genericg_compute_checksum_8al_11"; break;
               case 12: fn =  genericg_compute_checksum_8al_12;
                        nm = "genericg_compute_checksum_8al_12"; break;
               default: break;
            }
            fn_spec = (VEX_REGPARM(1) HWord(*)(HWord)) fn;
            nm_spec = nm;
         } else {
            HChar* nm = NULL;
            UInt   VEX_REGPARM(1) (*fn)(HWord) = NULL;
            switch (hWs_to_check) {
               case 1:  fn =  genericg_compute_checksum_4al_1;
                        nm = "genericg_compute_checksum_4al_1"; break;
               case 2:  fn =  genericg_compute_checksum_4al_2;
                        nm = "genericg_compute_checksum_4al_2"; break;
               case 3:  fn =  genericg_compute_checksum_4al_3;
                        nm = "genericg_compute_checksum_4al_3"; break;
               case 4:  fn =  genericg_compute_checksum_4al_4;
                        nm = "genericg_compute_checksum_4al_4"; break;
               case 5:  fn =  genericg_compute_checksum_4al_5;
                        nm = "genericg_compute_checksum_4al_5"; break;
               case 6:  fn =  genericg_compute_checksum_4al_6;
                        nm = "genericg_compute_checksum_4al_6"; break;
               case 7:  fn =  genericg_compute_checksum_4al_7;
                        nm = "genericg_compute_checksum_4al_7"; break;
               case 8:  fn =  genericg_compute_checksum_4al_8;
                        nm = "genericg_compute_checksum_4al_8"; break;
               case 9:  fn =  genericg_compute_checksum_4al_9;
                        nm = "genericg_compute_checksum_4al_9"; break;
               case 10: fn =  genericg_compute_checksum_4al_10;
                        nm = "genericg_compute_checksum_4al_10"; break;
               case 11: fn =  genericg_compute_checksum_4al_11;
                        nm = "genericg_compute_checksum_4al_11"; break;
               case 12: fn =  genericg_compute_checksum_4al_12;
                        nm = "genericg_compute_checksum_4al_12"; break;
               default: break;
            }
            fn_spec = (VEX_REGPARM(1) HWord(*)(HWord))fn;
            nm_spec = nm;
         }

         expectedhW = fn_generic( first_hW, hWs_to_check );
         /* If we got a specialised version, check it produces the same
            result as the generic version! */
         if (fn_spec) {
            vassert(nm_spec);
            vassert(expectedhW == fn_spec( first_hW ));
         } else {
            vassert(!nm_spec);
         }

         /* Set TISTART and TILEN.  These will describe to the despatcher
            the area of guest code to invalidate should we exit with a
            self-check failure. */

         tistart_tmp = newIRTemp(irsb->tyenv, guest_word_type);
         tilen_tmp   = newIRTemp(irsb->tyenv, guest_word_type);

         IRConst* base2check_IRConst
            = guest_word_type==Ity_I32 ? IRConst_U32(toUInt(base2check))
                                       : IRConst_U64(base2check);
         IRConst* len2check_IRConst
            = guest_word_type==Ity_I32 ? IRConst_U32(len2check)
                                       : IRConst_U64(len2check);

         irsb->stmts[selfcheck_idx + i * 5 + 0]
            = IRStmt_WrTmp(tistart_tmp, IRExpr_Const(base2check_IRConst) );

         irsb->stmts[selfcheck_idx + i * 5 + 1]
            = IRStmt_WrTmp(tilen_tmp, IRExpr_Const(len2check_IRConst) );

         irsb->stmts[selfcheck_idx + i * 5 + 2]
            = IRStmt_Put( offB_TISTART, IRExpr_RdTmp(tistart_tmp) );

         irsb->stmts[selfcheck_idx + i * 5 + 3]
            = IRStmt_Put( offB_TILEN, IRExpr_RdTmp(tilen_tmp) );

         /* Generate the entry point descriptors */
         if (abiinfo_both->host_ppc_calls_use_fndescrs) {
            HWord* descr = (HWord*)fn_generic;
            fn_generic_entry = descr[0];
            if (fn_spec) {
               descr = (HWord*)fn_spec;
               fn_spec_entry = descr[0];
            } else {
               fn_spec_entry = (HWord)NULL;
            }
         } else {
            fn_generic_entry = (HWord)fn_generic;
            if (fn_spec) {
               fn_spec_entry = (HWord)fn_spec;
            } else {
               fn_spec_entry = (HWord)NULL;
            }
         }

         IRExpr* callexpr = NULL;
         if (fn_spec) {
            callexpr = mkIRExprCCall( 
                          host_word_type, 1/*regparms*/, 
                          nm_spec, (void*)fn_spec_entry,
                          mkIRExprVec_1(
                             mkIRExpr_HWord( (HWord)first_hW )
                          )
                       );
         } else {
            callexpr = mkIRExprCCall( 
                          host_word_type, 2/*regparms*/, 
                          nm_generic, (void*)fn_generic_entry,
                          mkIRExprVec_2(
                             mkIRExpr_HWord( (HWord)first_hW ),
                             mkIRExpr_HWord( (HWord)hWs_to_check )
                          )
                       );
         }

         irsb->stmts[selfcheck_idx + i * 5 + 4]
            = IRStmt_Exit( 
                 IRExpr_Binop( 
                    host_word_type==Ity_I64 ? Iop_CmpNE64 : Iop_CmpNE32,
                    callexpr,
                       host_word_type==Ity_I64
                          ? IRExpr_Const(IRConst_U64(expectedhW))
                          : IRExpr_Const(IRConst_U32(expectedhW))
                 ),
                 Ijk_TInval,
                 /* Where we must restart if there's a failure: at the
                    first extent, regardless of which extent the
                    failure actually happened in. */
                 guest_IP_bbstart_IRConst
              );
      }
   }

   return irsb;
}


/*-------------------------------------------------------------
  A support routine for doing self-checking translations. 
  -------------------------------------------------------------*/

/* CLEAN HELPER */
/* CALLED FROM GENERATED CODE */

/* Compute a checksum of host memory at [addr .. addr+len-1], as fast
   as possible.  All _4al versions assume that the supplied address is
   4 aligned.  All length values are in 4-byte chunks.  These fns
   arecalled once for every use of a self-checking translation, so
   they needs to be as fast as possible. */

/* --- 32-bit versions, used only on 32-bit hosts --- */

static inline UInt ROL32 ( UInt w, Int n ) {
   w = (w << n) | (w >> (32-n));
   return w;
}

VEX_REGPARM(2)
static UInt genericg_compute_checksum_4al ( HWord first_w32, HWord n_w32s )
{
   UInt  sum1 = 0, sum2 = 0;
   UInt* p = (UInt*)first_w32;
   /* unrolled */
   while (n_w32s >= 4) {
      UInt  w;
      w = p[0];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
      w = p[1];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
      w = p[2];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
      w = p[3];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
      p += 4;
      n_w32s -= 4;
      sum1 ^= sum2;
   }
   while (n_w32s >= 1) {
      UInt  w;
      w = p[0];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
      p += 1;
      n_w32s -= 1;
      sum1 ^= sum2;
   }
   return sum1 + sum2;
}

/* Specialised versions of the above function */

VEX_REGPARM(1)
static UInt genericg_compute_checksum_4al_1 ( HWord first_w32 )
{
   UInt  sum1 = 0, sum2 = 0;
   UInt* p = (UInt*)first_w32;
   UInt  w;
   w = p[0];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   sum1 ^= sum2;
   return sum1 + sum2;
}

VEX_REGPARM(1)
static UInt genericg_compute_checksum_4al_2 ( HWord first_w32 )
{
   UInt  sum1 = 0, sum2 = 0;
   UInt* p = (UInt*)first_w32;
   UInt  w;
   w = p[0];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   sum1 ^= sum2;
   w = p[1];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   sum1 ^= sum2;
   return sum1 + sum2;
}

VEX_REGPARM(1)
static UInt genericg_compute_checksum_4al_3 ( HWord first_w32 )
{
   UInt  sum1 = 0, sum2 = 0;
   UInt* p = (UInt*)first_w32;
   UInt  w;
   w = p[0];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   sum1 ^= sum2;
   w = p[1];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   sum1 ^= sum2;
   w = p[2];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   sum1 ^= sum2;
   return sum1 + sum2;
}

VEX_REGPARM(1)
static UInt genericg_compute_checksum_4al_4 ( HWord first_w32 )
{
   UInt  sum1 = 0, sum2 = 0;
   UInt* p = (UInt*)first_w32;
   UInt  w;
   w = p[0];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   w = p[1];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   w = p[2];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   w = p[3];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   sum1 ^= sum2;
   return sum1 + sum2;
}

VEX_REGPARM(1)
static UInt genericg_compute_checksum_4al_5 ( HWord first_w32 )
{
   UInt  sum1 = 0, sum2 = 0;
   UInt* p = (UInt*)first_w32;
   UInt  w;
   w = p[0];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   w = p[1];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   w = p[2];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   w = p[3];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   sum1 ^= sum2;
   w = p[4];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   sum1 ^= sum2;
   return sum1 + sum2;
}

VEX_REGPARM(1)
static UInt genericg_compute_checksum_4al_6 ( HWord first_w32 )
{
   UInt  sum1 = 0, sum2 = 0;
   UInt* p = (UInt*)first_w32;
   UInt  w;
   w = p[0];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   w = p[1];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   w = p[2];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   w = p[3];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   sum1 ^= sum2;
   w = p[4];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   sum1 ^= sum2;
   w = p[5];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   sum1 ^= sum2;
   return sum1 + sum2;
}

VEX_REGPARM(1)
static UInt genericg_compute_checksum_4al_7 ( HWord first_w32 )
{
   UInt  sum1 = 0, sum2 = 0;
   UInt* p = (UInt*)first_w32;
   UInt  w;
   w = p[0];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   w = p[1];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   w = p[2];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   w = p[3];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   sum1 ^= sum2;
   w = p[4];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   sum1 ^= sum2;
   w = p[5];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   sum1 ^= sum2;
   w = p[6];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   sum1 ^= sum2;
   return sum1 + sum2;
}

VEX_REGPARM(1)
static UInt genericg_compute_checksum_4al_8 ( HWord first_w32 )
{
   UInt  sum1 = 0, sum2 = 0;
   UInt* p = (UInt*)first_w32;
   UInt  w;
   w = p[0];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   w = p[1];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   w = p[2];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   w = p[3];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   sum1 ^= sum2;
   w = p[4];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   w = p[5];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   w = p[6];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   w = p[7];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   sum1 ^= sum2;
   return sum1 + sum2;
}

VEX_REGPARM(1)
static UInt genericg_compute_checksum_4al_9 ( HWord first_w32 )
{
   UInt  sum1 = 0, sum2 = 0;
   UInt* p = (UInt*)first_w32;
   UInt  w;
   w = p[0];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   w = p[1];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   w = p[2];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   w = p[3];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   sum1 ^= sum2;
   w = p[4];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   w = p[5];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   w = p[6];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   w = p[7];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   sum1 ^= sum2;
   w = p[8];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   sum1 ^= sum2;
   return sum1 + sum2;
}

VEX_REGPARM(1)
static UInt genericg_compute_checksum_4al_10 ( HWord first_w32 )
{
   UInt  sum1 = 0, sum2 = 0;
   UInt* p = (UInt*)first_w32;
   UInt  w;
   w = p[0];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   w = p[1];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   w = p[2];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   w = p[3];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   sum1 ^= sum2;
   w = p[4];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   w = p[5];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   w = p[6];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   w = p[7];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   sum1 ^= sum2;
   w = p[8];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   sum1 ^= sum2;
   w = p[9];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   sum1 ^= sum2;
   return sum1 + sum2;
}

VEX_REGPARM(1)
static UInt genericg_compute_checksum_4al_11 ( HWord first_w32 )
{
   UInt  sum1 = 0, sum2 = 0;
   UInt* p = (UInt*)first_w32;
   UInt  w;
   w = p[0];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   w = p[1];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   w = p[2];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   w = p[3];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   sum1 ^= sum2;
   w = p[4];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   w = p[5];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   w = p[6];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   w = p[7];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   sum1 ^= sum2;
   w = p[8];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   sum1 ^= sum2;
   w = p[9];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   sum1 ^= sum2;
   w = p[10]; sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   sum1 ^= sum2;
   return sum1 + sum2;
}

VEX_REGPARM(1)
static UInt genericg_compute_checksum_4al_12 ( HWord first_w32 )
{
   UInt  sum1 = 0, sum2 = 0;
   UInt* p = (UInt*)first_w32;
   UInt  w;
   w = p[0];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   w = p[1];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   w = p[2];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   w = p[3];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   sum1 ^= sum2;
   w = p[4];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   w = p[5];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   w = p[6];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   w = p[7];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   sum1 ^= sum2;
   w = p[8];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   w = p[9];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   w = p[10]; sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   w = p[11]; sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
   sum1 ^= sum2;
   return sum1 + sum2;
}


/* --- 64-bit versions, used only on 64-bit hosts --- */

static inline ULong ROL64 ( ULong w, Int n ) {
   w = (w << n) | (w >> (64-n));
   return w;
}

VEX_REGPARM(2)
static ULong genericg_compute_checksum_8al ( HWord first_w64, HWord n_w64s )
{
   ULong  sum1 = 0, sum2 = 0;
   ULong* p = (ULong*)first_w64;
   /* unrolled */
   while (n_w64s >= 4) {
      ULong  w;
      w = p[0];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
      w = p[1];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
      w = p[2];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
      w = p[3];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
      p += 4;
      n_w64s -= 4;
      sum1 ^= sum2;
   }
   while (n_w64s >= 1) {
      ULong  w;
      w = p[0];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
      p += 1;
      n_w64s -= 1;
      sum1 ^= sum2;
   }
   return sum1 + sum2;
}

/* Specialised versions of the above function */

VEX_REGPARM(1)
static ULong genericg_compute_checksum_8al_1 ( HWord first_w64 )
{
   ULong  sum1 = 0, sum2 = 0;
   ULong* p = (ULong*)first_w64;
   ULong  w;
   w = p[0];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   sum1 ^= sum2;
   return sum1 + sum2;
}

VEX_REGPARM(1)
static ULong genericg_compute_checksum_8al_2 ( HWord first_w64 )
{
   ULong  sum1 = 0, sum2 = 0;
   ULong* p = (ULong*)first_w64;
   ULong  w;
   w = p[0];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   sum1 ^= sum2;
   w = p[1];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   sum1 ^= sum2;
   return sum1 + sum2;
}

VEX_REGPARM(1)
static ULong genericg_compute_checksum_8al_3 ( HWord first_w64 )
{
   ULong  sum1 = 0, sum2 = 0;
   ULong* p = (ULong*)first_w64;
   ULong  w;
   w = p[0];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   sum1 ^= sum2;
   w = p[1];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   sum1 ^= sum2;
   w = p[2];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   sum1 ^= sum2;
   return sum1 + sum2;
}

VEX_REGPARM(1)
static ULong genericg_compute_checksum_8al_4 ( HWord first_w64 )
{
   ULong  sum1 = 0, sum2 = 0;
   ULong* p = (ULong*)first_w64;
   ULong  w;
   w = p[0];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   w = p[1];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   w = p[2];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   w = p[3];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   sum1 ^= sum2;
   return sum1 + sum2;
}

VEX_REGPARM(1)
static ULong genericg_compute_checksum_8al_5 ( HWord first_w64 )
{
   ULong  sum1 = 0, sum2 = 0;
   ULong* p = (ULong*)first_w64;
   ULong  w;
   w = p[0];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   w = p[1];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   w = p[2];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   w = p[3];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   sum1 ^= sum2;
   w = p[4];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   sum1 ^= sum2;
   return sum1 + sum2;
}

VEX_REGPARM(1)
static ULong genericg_compute_checksum_8al_6 ( HWord first_w64 )
{
   ULong  sum1 = 0, sum2 = 0;
   ULong* p = (ULong*)first_w64;
   ULong  w;
   w = p[0];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   w = p[1];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   w = p[2];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   w = p[3];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   sum1 ^= sum2;
   w = p[4];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   sum1 ^= sum2;
   w = p[5];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   sum1 ^= sum2;
   return sum1 + sum2;
}

VEX_REGPARM(1)
static ULong genericg_compute_checksum_8al_7 ( HWord first_w64 )
{
   ULong  sum1 = 0, sum2 = 0;
   ULong* p = (ULong*)first_w64;
   ULong  w;
   w = p[0];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   w = p[1];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   w = p[2];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   w = p[3];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   sum1 ^= sum2;
   w = p[4];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   sum1 ^= sum2;
   w = p[5];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   sum1 ^= sum2;
   w = p[6];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   sum1 ^= sum2;
   return sum1 + sum2;
}

VEX_REGPARM(1)
static ULong genericg_compute_checksum_8al_8 ( HWord first_w64 )
{
   ULong  sum1 = 0, sum2 = 0;
   ULong* p = (ULong*)first_w64;
   ULong  w;
   w = p[0];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   w = p[1];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   w = p[2];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   w = p[3];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   sum1 ^= sum2;
   w = p[4];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   w = p[5];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   w = p[6];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   w = p[7];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   sum1 ^= sum2;
   return sum1 + sum2;
}

VEX_REGPARM(1)
static ULong genericg_compute_checksum_8al_9 ( HWord first_w64 )
{
   ULong  sum1 = 0, sum2 = 0;
   ULong* p = (ULong*)first_w64;
   ULong  w;
   w = p[0];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   w = p[1];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   w = p[2];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   w = p[3];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   sum1 ^= sum2;
   w = p[4];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   w = p[5];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   w = p[6];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   w = p[7];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   sum1 ^= sum2;
   w = p[8];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   sum1 ^= sum2;
   return sum1 + sum2;
}

VEX_REGPARM(1)
static ULong genericg_compute_checksum_8al_10 ( HWord first_w64 )
{
   ULong  sum1 = 0, sum2 = 0;
   ULong* p = (ULong*)first_w64;
   ULong  w;
   w = p[0];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   w = p[1];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   w = p[2];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   w = p[3];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   sum1 ^= sum2;
   w = p[4];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   w = p[5];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   w = p[6];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   w = p[7];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   sum1 ^= sum2;
   w = p[8];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   sum1 ^= sum2;
   w = p[9];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   sum1 ^= sum2;
   return sum1 + sum2;
}

VEX_REGPARM(1)
static ULong genericg_compute_checksum_8al_11 ( HWord first_w64 )
{
   ULong  sum1 = 0, sum2 = 0;
   ULong* p = (ULong*)first_w64;
   ULong  w;
   w = p[0];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   w = p[1];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   w = p[2];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   w = p[3];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   sum1 ^= sum2;
   w = p[4];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   w = p[5];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   w = p[6];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   w = p[7];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   sum1 ^= sum2;
   w = p[8];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   sum1 ^= sum2;
   w = p[9];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   sum1 ^= sum2;
   w = p[10]; sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   sum1 ^= sum2;
   return sum1 + sum2;
}

VEX_REGPARM(1)
static ULong genericg_compute_checksum_8al_12 ( HWord first_w64 )
{
   ULong  sum1 = 0, sum2 = 0;
   ULong* p = (ULong*)first_w64;
   ULong  w;
   w = p[0];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   w = p[1];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   w = p[2];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   w = p[3];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   sum1 ^= sum2;
   w = p[4];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   w = p[5];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   w = p[6];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   w = p[7];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   sum1 ^= sum2;
   w = p[8];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   w = p[9];  sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   w = p[10]; sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   w = p[11]; sum1 = ROL64(sum1 ^ w, 63);  sum2 += w;
   sum1 ^= sum2;
   return sum1 + sum2;
}

/*--------------------------------------------------------------------*/
/*--- end                                 guest_generic_bb_to_IR.c ---*/
/*--------------------------------------------------------------------*/
