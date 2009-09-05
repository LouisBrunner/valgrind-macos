
/*--------------------------------------------------------------------*/
/*---                                                              ---*/
/*--- This file (guest_generic_bb_to_IR.c) is                      ---*/
/*--- Copyright (C) OpenWorks LLP.  All rights reserved.           ---*/
/*---                                                              ---*/
/*--------------------------------------------------------------------*/

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
__attribute__((regparm(2)))
static UInt genericg_compute_checksum_4al_4plus ( HWord addr, HWord len );
__attribute__((regparm(2)))
static UInt genericg_compute_checksum_generic ( HWord addr, HWord len );

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

   /* If asked to make a self-checking translation, leave 5 spaces
      in which to put the check statements.  We'll fill them in later
      when we know the length and adler32 of the area to check. */
   if (do_self_check) {
      selfcheck_idx = irsb->stmts_used;
      addStmtToIRSB( irsb, IRStmt_NoOp() );
      addStmtToIRSB( irsb, IRStmt_NoOp() );
      addStmtToIRSB( irsb, IRStmt_NoOp() );
      addStmtToIRSB( irsb, IRStmt_NoOp() );
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
              /* If making self-checking translations, don't chase
                 .. it makes the checks too complicated.  We only want
                 to scan just one sequence of bytes in the check, not
                 a whole bunch. */
              && !do_self_check
              /* we can't afford to have a resteer once we're on the
                 last extent slot. */
              && vge->n_used < 3
           );

      resteerOKfn
         = resteerOK ? chase_into_ok : const_False;

      /* This is the IP of the instruction we're just about to deal
         with. */
      guest_IP_curr_instr = guest_IP_bbstart + delta;

      /* This is the irsb statement array index of the first stmt in
         this insn.  That will always be the instruction-mark
         descriptor. */
      first_stmt_idx = irsb->stmts_used;

      /* Add an instruction-mark statement.  We won't know until after
         disassembling the instruction how long it instruction is, so
         just put in a zero length and we'll fix it up later. */
      addStmtToIRSB( irsb, IRStmt_IMark( guest_IP_curr_instr, 0 ));

      /* for the first insn, the dispatch loop will have set
         %IP, but for all the others we have to do it ourselves. */
      need_to_put_IP = toBool(n_instrs > 0);

      /* Finally, actually disassemble an instruction. */
      dres = dis_instr_fn ( irsb,
                            need_to_put_IP,
                            resteerOKfn,
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
              || dres.whatNext == Dis_Resteer);
      vassert(dres.len >= 0 && dres.len <= 20);
      if (dres.whatNext != Dis_Resteer)
         vassert(dres.continueAt == 0);

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
         case Dis_Resteer:
            /* Check that we actually allowed a resteer .. */
            vassert(resteerOK);
            vassert(irsb->next == NULL);
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
      a self-checking preamble may need to be created. */
   if (do_self_check) {

      UInt     len2check, expected32;
      IRTemp   tistart_tmp, tilen_tmp;
      UInt     (*checksum_fn)(HWord, HWord) __attribute__((regparm(2)));
      HWord    checksum_fn_entry;

      vassert(vge->n_used == 1);
      len2check = vge->len[0];

      /* stay sane */
      vassert(len2check >= 0 && len2check < 1000/*arbitrary*/);

      if (len2check >= 4 && 0 == (((HWord)guest_code) & 3)) {
         checksum_fn = genericg_compute_checksum_4al_4plus;
      } else {
         checksum_fn = genericg_compute_checksum_generic;
      }

      expected32 = checksum_fn( (HWord)guest_code, len2check );

      /* Set TISTART and TILEN.  These will describe to the despatcher
         the area of guest code to invalidate should we exit with a
         self-check failure. */

      tistart_tmp = newIRTemp(irsb->tyenv, guest_word_type);
      tilen_tmp   = newIRTemp(irsb->tyenv, guest_word_type);

      irsb->stmts[selfcheck_idx+0]
         = IRStmt_WrTmp(tistart_tmp, IRExpr_Const(guest_IP_bbstart_IRConst) );

      irsb->stmts[selfcheck_idx+1]
         = IRStmt_WrTmp(tilen_tmp,
                        guest_word_type==Ity_I32 
                           ? IRExpr_Const(IRConst_U32(len2check)) 
                           : IRExpr_Const(IRConst_U64(len2check))
           );

      irsb->stmts[selfcheck_idx+2]
         = IRStmt_Put( offB_TISTART, IRExpr_RdTmp(tistart_tmp) );

      irsb->stmts[selfcheck_idx+3]
         = IRStmt_Put( offB_TILEN, IRExpr_RdTmp(tilen_tmp) );

      if (abiinfo_both->host_ppc_calls_use_fndescrs) {
         HWord* fndescr = (HWord*)checksum_fn;
         checksum_fn_entry = fndescr[0];
      } else {
         checksum_fn_entry = (HWord)checksum_fn;
      }

      irsb->stmts[selfcheck_idx+4]
         = IRStmt_Exit( 
              IRExpr_Binop( 
                 Iop_CmpNE32, 
                 mkIRExprCCall( 
                    Ity_I32, 
                    2/*regparms*/, 
                    checksum_fn == genericg_compute_checksum_4al_4plus
                       ? "genericg_compute_checksum_4al_4plus"
                       : "genericg_compute_checksum_generic",
                    (void*)checksum_fn_entry,
                    mkIRExprVec_2( 
                       mkIRExpr_HWord( (HWord)guest_code ), 
                       mkIRExpr_HWord( (HWord)len2check )
                    )
                 ),
                 IRExpr_Const(IRConst_U32(expected32))
              ),
              Ijk_TInval,
              guest_IP_bbstart_IRConst
           );
   }

   return irsb;
}


/*-------------------------------------------------------------
  A support routine for doing self-checking translations. 
  -------------------------------------------------------------*/

/* CLEAN HELPER */
/* CALLED FROM GENERATED CODE */

/* Compute a checksum of host memory at [addr .. addr+len-1], as fast
   as possible.  The _4al_4plus version is assured that the request is
   for 4-aligned memory and for a block of 4 or more long, whilst the
   _generic version must be able to handle any alignment, and lengths
   down to zero too.  This fn is called once for every use of a
   self-checking translation, so it needs to be as fast as
   possible. */

static inline UInt ROL32 ( UInt w, Int n ) {
   w = (w << n) | (w >> (32-n));
   return w;
}

__attribute((regparm(2)))
static UInt genericg_compute_checksum_generic ( HWord addr, HWord len )
{
   UInt sum1 = 0, sum2 = 0;
   /* pull up to 4-alignment */
   while ((addr & 3) != 0 && len >= 1) {
      UChar* p = (UChar*)addr;
      sum1 = (sum1 << 8) | (UInt)p[0];
      addr++;
      len--;
   }
   /* vectorised + unrolled */
   while (len >= 16) {
      UInt* p = (UInt*)addr;
      UInt  w;
      w = p[0];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
      w = p[1];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
      w = p[2];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
      w = p[3];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
      addr += 16;
      len  -= 16;
      sum1 ^= sum2;
   }
   /* vectorised fixup */
   while (len >= 4) {
      UInt* p = (UInt*)addr;
      UInt  w = p[0];
      sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
      addr += 4;
      len  -= 4;
      sum1 ^= sum2;
   }
   /* scalar fixup */
   while (len >= 1) {
      UChar* p = (UChar*)addr;
      UInt   w = (UInt)p[0];
      sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
      addr++;
      len--;
   }
   return sum1 + sum2;
}

__attribute((regparm(2)))
static UInt genericg_compute_checksum_4al_4plus ( HWord addr, HWord len )
{
   UInt sum1 = 0, sum2 = 0;
   /* vassert(0 == (addr & 3)); */
   /* vassert(len >= 4); */
   /* vectorised + unrolled */
   while (len >= 16) {
      UInt* p = (UInt*)addr;
      UInt  w;
      w = p[0];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
      w = p[1];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
      w = p[2];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
      w = p[3];  sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
      addr += 16;
      len  -= 16;
      sum1 ^= sum2;
   }
   /* vectorised fixup */
   while (len >= 4) {
      UInt* p = (UInt*)addr;
      UInt  w = p[0];
      sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
      addr += 4;
      len  -= 4;
      sum1 ^= sum2;
   }
   /* scalar fixup */
   while (len >= 1) {
      UChar* p = (UChar*)addr;
      UInt   w = (UInt)p[0];
      sum1 = ROL32(sum1 ^ w, 31);  sum2 += w;
      addr++;
      len--;
   }
   return sum1 + sum2;
}

/*--------------------------------------------------------------------*/
/*--- end                                 guest_generic_bb_to_IR.c ---*/
/*--------------------------------------------------------------------*/
