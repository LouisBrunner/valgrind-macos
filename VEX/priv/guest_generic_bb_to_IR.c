
/*--------------------------------------------------------------------*/
/*--- begin                               guest_generic_bb_to_IR.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2004-2017 OpenWorks LLP
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
   along with this program; if not, see <http://www.gnu.org/licenses/>.

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
#include "ir_opt.h"


/*--------------------------------------------------------------*/
/*--- Forwards for fns called by self-checking translations  ---*/
/*--------------------------------------------------------------*/

/* Forwards .. */
VEX_REGPARM(2) static UInt genericg_compute_checksum_4al ( HWord first_w32,
                                                           HWord n_w32s );
VEX_REGPARM(1) static UInt genericg_compute_checksum_4al_1 ( HWord first_w32 );
VEX_REGPARM(1) static UInt genericg_compute_checksum_4al_2 ( HWord first_w32 );
VEX_REGPARM(1) static UInt genericg_compute_checksum_4al_3 ( HWord first_w32 );
VEX_REGPARM(1) static UInt genericg_compute_checksum_4al_4 ( HWord first_w32 );
VEX_REGPARM(1) static UInt genericg_compute_checksum_4al_5 ( HWord first_w32 );
VEX_REGPARM(1) static UInt genericg_compute_checksum_4al_6 ( HWord first_w32 );
VEX_REGPARM(1) static UInt genericg_compute_checksum_4al_7 ( HWord first_w32 );
VEX_REGPARM(1) static UInt genericg_compute_checksum_4al_8 ( HWord first_w32 );
VEX_REGPARM(1) static UInt genericg_compute_checksum_4al_9 ( HWord first_w32 );
VEX_REGPARM(1) static UInt genericg_compute_checksum_4al_10 ( HWord first_w32 );
VEX_REGPARM(1) static UInt genericg_compute_checksum_4al_11 ( HWord first_w32 );
VEX_REGPARM(1) static UInt genericg_compute_checksum_4al_12 ( HWord first_w32 );

VEX_REGPARM(2) static ULong genericg_compute_checksum_8al ( HWord first_w64,
                                                            HWord n_w64s );
VEX_REGPARM(1) static ULong genericg_compute_checksum_8al_1 ( HWord first_w64 );
VEX_REGPARM(1) static ULong genericg_compute_checksum_8al_2 ( HWord first_w64 );
VEX_REGPARM(1) static ULong genericg_compute_checksum_8al_3 ( HWord first_w64 );
VEX_REGPARM(1) static ULong genericg_compute_checksum_8al_4 ( HWord first_w64 );
VEX_REGPARM(1) static ULong genericg_compute_checksum_8al_5 ( HWord first_w64 );
VEX_REGPARM(1) static ULong genericg_compute_checksum_8al_6 ( HWord first_w64 );
VEX_REGPARM(1) static ULong genericg_compute_checksum_8al_7 ( HWord first_w64 );
VEX_REGPARM(1) static ULong genericg_compute_checksum_8al_8 ( HWord first_w64 );
VEX_REGPARM(1) static ULong genericg_compute_checksum_8al_9 ( HWord first_w64 );
VEX_REGPARM(1) static ULong genericg_compute_checksum_8al_10 ( HWord first_w64 );
VEX_REGPARM(1) static ULong genericg_compute_checksum_8al_11 ( HWord first_w64 );
VEX_REGPARM(1) static ULong genericg_compute_checksum_8al_12 ( HWord first_w64 );


/*--------------------------------------------------------------*/
/*--- Creation of self-check IR                              ---*/
/*--------------------------------------------------------------*/

static void create_self_checks_as_needed(
               /*MOD*/ IRSB*                irsb,
               /*OUT*/ UInt*                n_sc_extents,
               /*MOD*/ VexRegisterUpdates*  pxControl,
               /*MOD*/ void*                callback_opaque,
               /*IN*/  UInt (*needs_self_check)
                               (void*, /*MB_MOD*/VexRegisterUpdates*,
                                const VexGuestExtents*),
               const   VexGuestExtents*     vge,
               const   VexAbiInfo*          abiinfo_both,
               const   IRType               guest_word_type,
               const   Int                  selfcheck_idx,
               /*IN*/  Int                  offB_GUEST_CMSTART,
               /*IN*/  Int                  offB_GUEST_CMLEN,
               /*IN*/  Int                  offB_GUEST_IP,
               const   Addr                 guest_IP_sbstart
            )
{
   /* The scheme is to compute a rather crude checksum of the code
      we're making a translation of, and add to the IR a call to a
      helper routine which recomputes the checksum every time the
      translation is run, and requests a retranslation if it doesn't
      match.  This is obviously very expensive and considerable
      efforts are made to speed it up:

      * the checksum is computed from all the naturally aligned
        host-sized words that overlap the translated code.  That means
        it could depend on up to 7 bytes before and 7 bytes after
        which aren't part of the translated area, and so if those
        change then we'll unnecessarily have to discard and
        retranslate.  This seems like a pretty remote possibility and
        it seems as if the benefit of not having to deal with the ends
        of the range at byte precision far outweigh any possible extra
        translations needed.

      * there's a generic routine and 12 specialised cases, which
        handle the cases of 1 through 12-word lengths respectively.
        They seem to cover about 90% of the cases that occur in
        practice.

      We ask the caller, via needs_self_check, which of the 3 vge
      extents needs a check, and only generate check code for those
      that do.
   */
   {
      Addr     base2check;
      UInt     len2check;
      HWord    expectedhW;
      IRTemp   tistart_tmp, tilen_tmp;
      HWord    VEX_REGPARM(2) (*fn_generic)(HWord, HWord);
      HWord    VEX_REGPARM(1) (*fn_spec)(HWord);
      const HChar* nm_generic;
      const HChar* nm_spec;
      HWord    fn_generic_entry = 0;
      HWord    fn_spec_entry = 0;
      UInt     host_word_szB = sizeof(HWord);
      IRType   host_word_type = Ity_INVALID;

      UInt extents_needing_check
         = needs_self_check(callback_opaque, pxControl, vge);

      if (host_word_szB == 4) host_word_type = Ity_I32;
      if (host_word_szB == 8) host_word_type = Ity_I64;
      vassert(host_word_type != Ity_INVALID);

      vassert(vge->n_used >= 1 && vge->n_used <= 3);

      /* Caller shouldn't claim that nonexistent extents need a
         check. */
      vassert((extents_needing_check >> vge->n_used) == 0);

      /* Guest addresses as IRConsts.  Used in self-checks to specify the
         restart-after-discard point. */
      IRConst* guest_IP_sbstart_IRConst
                  = guest_word_type==Ity_I32 
                       ? IRConst_U32(toUInt(guest_IP_sbstart))
                       : IRConst_U64(guest_IP_sbstart);

      const Int n_extent_slots = sizeof(vge->base) / sizeof(vge->base[0]);
      vassert(n_extent_slots == 3);

      vassert(selfcheck_idx + (n_extent_slots - 1) * 5 + 4 < irsb->stmts_used);

      for (Int i = 0; i < vge->n_used; i++) {
         /* Do we need to generate a check for this extent? */
         if ((extents_needing_check & (1 << i)) == 0)
            continue;

         /* Tell the caller */
         (*n_sc_extents)++;

         /* the extent we're generating a check for */
         base2check = vge->base[i];
         len2check  = vge->len[i];

         /* stay sane */
         vassert(len2check >= 0 && len2check < 2000/*arbitrary*/);

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
                 && hWs_to_check < 2004/*arbitrary*/ / host_word_szB);

         /* vex_printf("%lx %lx  %ld\n", first_hW, last_hW, hWs_to_check); */

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
            const HChar* nm = NULL;
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
            const HChar* nm = NULL;
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

         /* Set CMSTART and CMLEN.  These will describe to the despatcher
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

         IRStmt** stmt0 = &irsb->stmts[selfcheck_idx + i * 5 + 0];
         IRStmt** stmt1 = &irsb->stmts[selfcheck_idx + i * 5 + 1];
         IRStmt** stmt2 = &irsb->stmts[selfcheck_idx + i * 5 + 2];
         IRStmt** stmt3 = &irsb->stmts[selfcheck_idx + i * 5 + 3];
         IRStmt** stmt4 = &irsb->stmts[selfcheck_idx + i * 5 + 4];
         vassert((*stmt0)->tag == Ist_NoOp);
         vassert((*stmt1)->tag == Ist_NoOp);
         vassert((*stmt2)->tag == Ist_NoOp);
         vassert((*stmt3)->tag == Ist_NoOp);
         vassert((*stmt4)->tag == Ist_NoOp);

         *stmt0 = IRStmt_WrTmp(tistart_tmp, IRExpr_Const(base2check_IRConst) );
         *stmt1 = IRStmt_WrTmp(tilen_tmp, IRExpr_Const(len2check_IRConst) );
         *stmt2 = IRStmt_Put( offB_GUEST_CMSTART, IRExpr_RdTmp(tistart_tmp) );
         *stmt3 = IRStmt_Put( offB_GUEST_CMLEN, IRExpr_RdTmp(tilen_tmp) );

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

         *stmt4
            = IRStmt_Exit( 
                 IRExpr_Binop( 
                    host_word_type==Ity_I64 ? Iop_CmpNE64 : Iop_CmpNE32,
                    callexpr,
                       host_word_type==Ity_I64
                          ? IRExpr_Const(IRConst_U64(expectedhW))
                          : IRExpr_Const(IRConst_U32(expectedhW))
                 ),
                 Ijk_InvalICache,
                 /* Where we must restart if there's a failure: at the
                    first extent, regardless of which extent the
                    failure actually happened in. */
                 guest_IP_sbstart_IRConst,
                 offB_GUEST_IP
              );
      } /* for (i = 0; i < vge->n_used; i++) */

      for (Int i = vge->n_used;
           i < sizeof(vge->base) / sizeof(vge->base[0]); i++) {
         IRStmt* stmt0 = irsb->stmts[selfcheck_idx + i * 5 + 0];
         IRStmt* stmt1 = irsb->stmts[selfcheck_idx + i * 5 + 1];
         IRStmt* stmt2 = irsb->stmts[selfcheck_idx + i * 5 + 2];
         IRStmt* stmt3 = irsb->stmts[selfcheck_idx + i * 5 + 3];
         IRStmt* stmt4 = irsb->stmts[selfcheck_idx + i * 5 + 4];
         vassert(stmt0->tag == Ist_NoOp);
         vassert(stmt1->tag == Ist_NoOp);
         vassert(stmt2->tag == Ist_NoOp);
         vassert(stmt3->tag == Ist_NoOp);
         vassert(stmt4->tag == Ist_NoOp);
      }
   }
}


/*--------------------------------------------------------------*/
/*--- To do with guarding (conditionalisation) of IRStmts    ---*/
/*--------------------------------------------------------------*/

// Is it possible to guard |e|?  Meaning, is it safe (exception-free) to compute
// |e| and ignore the result?  Since |e| is by definition otherwise
// side-effect-free, we don't have to ask about any other effects caused by
// first computing |e| and then ignoring the result.
static Bool expr_is_guardable ( const IRExpr* e )
{
   switch (e->tag) {
      case Iex_Load:
         return False;
      case Iex_Unop:
         return !primopMightTrap(e->Iex.Unop.op);
      case Iex_Binop:
         return !primopMightTrap(e->Iex.Binop.op);
      case Iex_ITE:
      case Iex_CCall:
      case Iex_Get:
         return True;
      default:
         vex_printf("\n"); ppIRExpr(e); vex_printf("\n");
         vpanic("expr_is_guardable: unhandled expr");
   }
}

// Is it possible to guard |st|?  Meaning, is it possible to replace |st| by
// some other sequence of IRStmts which have the same effect on the architected
// state when the guard is true, but when it is false, have no effect on the
// architected state and are guaranteed not to cause any exceptions?
//
// Note that this isn't as aggressive as it could be: it sometimes returns False
// in cases where |st| is actually guardable.  This routine must coordinate
// closely with add_guarded_stmt_to_end_of below, in the sense that that routine
// must be able to handle any |st| for which this routine returns True.
static Bool stmt_is_guardable ( const IRStmt* st )
{
   switch (st->tag) {
      case Ist_IMark:
      case Ist_Put:
         return True;
      case Ist_Store: // definitely not
      case Ist_CAS: // definitely not
      case Ist_Exit: // We could in fact spec this, if required
         return False;
      case Ist_WrTmp:
         return expr_is_guardable(st->Ist.WrTmp.data);
      default:
         vex_printf("\n"); ppIRStmt(st); vex_printf("\n");
         vpanic("stmt_is_guardable: unhandled stmt");
   }
}

// Are all stmts (but not the end dst value) in |bb| guardable, per
// stmt_is_guardable?
static Bool block_is_guardable ( const IRSB* bb )
{
   Int i = bb->stmts_used;
   vassert(i >= 2); // Must have at least: IMark, side Exit (at the end)
   i--;
   vassert(bb->stmts[i]->tag == Ist_Exit);
   i--;
   for (; i >= 0; i--) {
      if (!stmt_is_guardable(bb->stmts[i]))
         return False;
   }
   return True;
}

// Guard |st| with |guard| and add it to |bb|.  This must be able to handle any
// |st| for which stmt_is_guardable returns True.
static void add_guarded_stmt_to_end_of ( /*MOD*/IRSB* bb,
                                         /*IN*/ IRStmt* st, IRTemp guard )
{
   switch (st->tag) {
      case Ist_IMark:
      case Ist_WrTmp:
         addStmtToIRSB(bb, st);
         break;
      case Ist_Put: {
         // Put(offs, e) ==> Put(offs, ITE(guard, e, Get(offs, sizeof(e))))
         // Which when flattened out is:
         //   t1 = Get(offs, sizeof(e))
         //   t2 = ITE(guard, e, t2)
         //   Put(offs, t2)
         Int offset = st->Ist.Put.offset;
         IRExpr* e = st->Ist.Put.data;
         IRType ty = typeOfIRExpr(bb->tyenv, e);
         IRTemp t1 = newIRTemp(bb->tyenv, ty);
         IRTemp t2 = newIRTemp(bb->tyenv, ty);
         addStmtToIRSB(bb, IRStmt_WrTmp(t1, IRExpr_Get(offset, ty)));
         addStmtToIRSB(bb, IRStmt_WrTmp(t2, IRExpr_ITE(IRExpr_RdTmp(guard),
                                                       e, IRExpr_RdTmp(t1))));
         addStmtToIRSB(bb, IRStmt_Put(offset, IRExpr_RdTmp(t2)));
         break;
      }
      case Ist_Exit: {
         // Exit(xguard, dst, jk, offsIP)
         // ==> t1 = And1(xguard, guard)
         //     Exit(t1, dst, jk, offsIP)
         IRExpr* xguard = st->Ist.Exit.guard;
         IRTemp t1 = newIRTemp(bb->tyenv, Ity_I1);
         addStmtToIRSB(bb, IRStmt_WrTmp(t1, IRExpr_Binop(Iop_And1, xguard,
                                                         IRExpr_RdTmp(guard))));
         addStmtToIRSB(bb, IRStmt_Exit(IRExpr_RdTmp(t1), st->Ist.Exit.jk,
                                       st->Ist.Exit.dst, st->Ist.Exit.offsIP));
         break;
      }
      default:
         vex_printf("\n"); ppIRStmt(st); vex_printf("\n");
         vpanic("add_guarded_stmt_to_end_of: unhandled stmt");
   }
}


/*--------------------------------------------------------------*/
/*--- Analysis of block ends                                 ---*/
/*--------------------------------------------------------------*/

typedef
   enum {
      Be_Other=1,  // Block end isn't of interest to us
      Be_Uncond,   // Unconditional branch to known destination, unassisted
      Be_Cond      // Conditional branch to known destinations, unassisted
   }
   BlockEndTag;

typedef
   struct {
      BlockEndTag tag;
      union {
         struct {
         } Other;
         struct {
            Long delta;
         } Uncond;
         struct {
            IRTemp condSX;
            Long   deltaSX;
            Long   deltaFT;
         } Cond;
      } Be;
   }
   BlockEnd;

static void ppBlockEnd ( const BlockEnd* be )
{
   switch (be->tag) {
      case Be_Other:
         vex_printf("Other");
         break;
      case Be_Uncond:
         vex_printf("Uncond{delta=%lld}", be->Be.Uncond.delta);
         break;
      case Be_Cond:
         vex_printf("Cond{condSX=");
         ppIRTemp(be->Be.Cond.condSX);
         vex_printf(", deltaSX=%lld, deltaFT=%lld}",
                    be->Be.Cond.deltaSX, be->Be.Cond.deltaFT);
         break;
      default:
         vassert(0);
   }
}

// Return True if |be| definitely does not jump to |delta|.  In case of
// doubt, returns False. 
static Bool definitely_does_not_jump_to_delta ( const BlockEnd* be, Long delta )
{
   switch (be->tag) {
      case Be_Other:
         return False;
      case Be_Uncond:
         return be->Be.Uncond.delta != delta;
      case Be_Cond:
         return be->Be.Cond.deltaSX != delta && be->Be.Cond.deltaFT != delta;
      default:
         vassert(0);
   }
}

static Addr irconst_to_Addr ( const IRConst* con, const IRType guest_word_type )
{
   switch (con->tag) {
      case Ico_U32:
         vassert(guest_word_type == Ity_I32);
         return con->Ico.U32;
      case Ico_U64:
         vassert(guest_word_type == Ity_I64);
         return con->Ico.U64;
      default:
         vassert(0);
   }
}

static Bool irconst_to_maybe_delta ( /*OUT*/Long* delta,
                                     const IRConst* known_dst,
                                     const Addr guest_IP_sbstart,
                                     const IRType guest_word_type,
                                     Bool (*chase_into_ok)(void*,Addr),
                                     void* callback_opaque )
{
   vassert(typeOfIRConst(known_dst) == guest_word_type);

   *delta = 0;

   // Extract the destination guest address.
   Addr dst_ga = irconst_to_Addr(known_dst, guest_word_type);

   // Check we're allowed to chase into it.
   if (!chase_into_ok(callback_opaque, dst_ga))
      return False;

   Addr delta_as_Addr = dst_ga - guest_IP_sbstart;
   // Either |delta_as_Addr| is a 64-bit value, in which case copy it directly
   // to |delta|, or it's a 32 bit value, in which case sign extend it.
   *delta = sizeof(Addr) == 8 ? (Long)delta_as_Addr : (Long)(Int)delta_as_Addr;
   return True;
}

static Bool any_overlap ( Int start1, Int len1, Int start2, Int len2 )
{
   vassert(len1 > 0 && len2 > 0);
   vassert(start1 >= 0 && start2 >= 0);
   if (start1 + len1 <= start2) return False;
   if (start2 + len2 <= start1) return False;
   return True;
}

/* Scan |stmts|, starting at |scan_start| and working backwards, to detect the
   case where there are no IRStmt_Exits before we find the IMark.  In other
   words, it scans backwards through some prefix of an instruction's IR to see
   if there is an exit there.

   It also checks for explicit PUTs to the PC, via Ist_Put, Ist_PutI or
   Ist_Dirty.  I suspect this is ridiculous overkill, but is here for safety. */
static Bool insn_has_no_other_exits_or_PUTs_to_PC (
               IRStmt** const stmts, Int scan_start,
               Int offB_GUEST_IP, Int szB_GUEST_IP,
               const IRTypeEnv* tyenv
            )
{
   Bool found_exit = False;
   Bool found_PUT_to_PC = False;
   Int i = scan_start;
   while (True) {
      if (i < 0)
         break;
      const IRStmt* st = stmts[i];
      if (st->tag == Ist_IMark) {
         // We're back at the start of the insn.  Stop searching.
         break;
      }
      if (st->tag == Ist_Exit) {
         found_exit = True;
         break;
      }
      if (st->tag == Ist_Put) {
         Int offB = st->Ist.Put.offset;
         Int szB  = sizeofIRType(typeOfIRExpr(tyenv, st->Ist.Put.data));
         if (any_overlap(offB, szB, offB_GUEST_IP, szB_GUEST_IP)) {
            found_PUT_to_PC = True;
            break;
         }
      }
      if (st->tag == Ist_PutI) {
         const IRPutI* details = st->Ist.PutI.details;
         const IRRegArray* descr = details->descr;
         Int offB = descr->base;
         Int szB  = descr->nElems * sizeofIRType(descr->elemTy);
         if (any_overlap(offB, szB, offB_GUEST_IP, szB_GUEST_IP)) {
            found_PUT_to_PC = True;
            break;
         }
      }
      if (st->tag == Ist_Dirty) {
         vassert(!found_PUT_to_PC);
         const IRDirty* details = st->Ist.Dirty.details;
         for (Int j = 0; j < details->nFxState; j++) {
            const IREffect fx   = details->fxState[j].fx;
            const Int offset    = details->fxState[j].offset;
            const Int size      = details->fxState[j].size;
            const Int nRepeats  = details->fxState[j].nRepeats;
            const Int repeatLen = details->fxState[j].repeatLen;
            if (fx == Ifx_Write || fx == Ifx_Modify) {
               for (Int k = 0; k < nRepeats; k++) {
                  Int offB = offset + k * repeatLen;
                  Int szB  = size;
                  if (any_overlap(offB, szB, offB_GUEST_IP, szB_GUEST_IP)) {
                     found_PUT_to_PC = True;
                  }
               }
            }
         }
         if (found_PUT_to_PC) {
            break;
         }
      }
      i--;
   }
   // We expect IR for all instructions to start with an IMark.
   vassert(i >= 0);
   return !found_exit && !found_PUT_to_PC;
}

static void analyse_block_end ( /*OUT*/BlockEnd* be, const IRSB* irsb,
                                const Addr guest_IP_sbstart,
                                const IRType guest_word_type,
                                Bool (*chase_into_ok)(void*,Addr),
                                void* callback_opaque,
                                Int   offB_GUEST_IP,
                                Int   szB_GUEST_IP,
                                Bool  debug_print )
{
   vex_bzero(be, sizeof(*be));

   // -- Conditional branch to known destination
   /* In short, detect the following end form:
         ------ IMark(0x4002009, 2, 0) ------
         // Zero or more non-exit statements
         if (t14) { PUT(184) = 0x4002040:I64; exit-Boring } 
         PUT(184) = 0x400200B:I64; exit-Boring
      Checks:
        - Both transfers are 'boring'
        - Both dsts are constants
        - The cond is non-constant (an IRExpr_Tmp)
        - There are no other exits in this instruction
        - The client allows chasing into both destinations
   */
   if (irsb->jumpkind == Ijk_Boring && irsb->stmts_used >= 2) {
      const IRStmt* maybe_exit = irsb->stmts[irsb->stmts_used - 1];
      if (maybe_exit->tag == Ist_Exit
          && maybe_exit->Ist.Exit.guard->tag == Iex_RdTmp
          && maybe_exit->Ist.Exit.jk == Ijk_Boring
          && irsb->next->tag == Iex_Const
          && insn_has_no_other_exits_or_PUTs_to_PC(
                irsb->stmts, irsb->stmts_used - 2,
                offB_GUEST_IP, szB_GUEST_IP, irsb->tyenv)) {
         vassert(maybe_exit->Ist.Exit.offsIP == irsb->offsIP);
         IRConst* dst_SX   = maybe_exit->Ist.Exit.dst;
         IRConst* dst_FT   = irsb->next->Iex.Const.con;
         IRTemp   cond_SX  = maybe_exit->Ist.Exit.guard->Iex.RdTmp.tmp;
         Long     delta_SX = 0;
         Long     delta_FT = 0;
         Bool ok_SX
            = irconst_to_maybe_delta(&delta_SX, dst_SX,
                                     guest_IP_sbstart, guest_word_type,
                                     chase_into_ok, callback_opaque);
         Bool ok_FT
            = irconst_to_maybe_delta(&delta_FT, dst_FT,
                                     guest_IP_sbstart, guest_word_type,
                                     chase_into_ok, callback_opaque);
         if (ok_SX && ok_FT) {
            be->tag = Be_Cond;
            be->Be.Cond.condSX = cond_SX;
            be->Be.Cond.deltaSX = delta_SX;
            be->Be.Cond.deltaFT = delta_FT;
            goto out;
         }
      }
   }

   // -- Unconditional branch/call to known destination
   /* Four checks:
      - The transfer is 'boring' or 'call', so that no assistance is needed
      - The dst is a constant (known at jit time)
      - There are no other exits in this instruction.  In other words, the
        transfer is unconditional.
      - The client allows chasing into the destination.
   */
   if ((irsb->jumpkind == Ijk_Boring || irsb->jumpkind == Ijk_Call)
       && irsb->next->tag == Iex_Const) {
      if (insn_has_no_other_exits_or_PUTs_to_PC(
             irsb->stmts, irsb->stmts_used - 1,
             offB_GUEST_IP, szB_GUEST_IP, irsb->tyenv)) {
         // We've got the right pattern.  Check whether we can chase into the
         // destination, and if so convert that to a delta value.
         const IRConst* known_dst = irsb->next->Iex.Const.con;
         Long delta = 0;
         // This call also checks the type of the dst addr, and that the client
         // allows chasing into it.
         Bool ok = irconst_to_maybe_delta(&delta, known_dst,
                                          guest_IP_sbstart, guest_word_type,
                                          chase_into_ok, callback_opaque);
         if (ok) {
            be->tag = Be_Uncond;
            be->Be.Uncond.delta = delta;
            goto out;
         }
      }
   }

   // Not identified as anything of interest to us.
   be->tag = Be_Other;

  out:
   if (debug_print) {
      vex_printf("\nBlockEnd: ");
      ppBlockEnd(be);
      vex_printf("\n");
   }
}


/*--------------------------------------------------------------*/
/*--- Disassembly of basic (not super) blocks                ---*/
/*--------------------------------------------------------------*/

/* Disassemble instructions, starting at |&guest_code[delta_IN]|, into |irbb|,
   and terminate the block properly.  At most |n_instrs_allowed_IN| may be
   disassembled, and this function may choose to disassemble fewer.

   Also do minimal simplifications on the resulting block, so as to convert the
   end of the block into something that |analyse_block_end| can reliably
   recognise.

   |irbb| will both be modified, and replaced by a new, simplified version,
   which is returned.
*/
static IRSB* disassemble_basic_block_till_stop(
                /*OUT*/ Int*    n_instrs,        // #instrs actually used
                /*OUT*/ Bool*   is_verbose_seen, // did we get a 'verbose' hint?
                /*OUT*/ Addr*   extent_base,     // VexGuestExtents[..].base
                /*OUT*/ UShort* extent_len,      // VexGuestExtents[..].len
                /*MOD*/ IRSB*          irbb,
                const   Long           delta_IN,
                const   Int            n_instrs_allowed_IN,
                const   Addr           guest_IP_sbstart,
                const   VexEndness     host_endness,
                const   Bool           sigill_diag,
                const   VexArch        arch_guest,
                const   VexArchInfo*   archinfo_guest,
                const   VexAbiInfo*    abiinfo_both,
                const   IRType         guest_word_type,
                const   Bool           debug_print,
                const   DisOneInstrFn  dis_instr_fn,
                const   UChar*         guest_code,
                const   Int            offB_GUEST_IP
             )
{
   /* This is the max instrs we allow in the block.  It starts off at
      |n_instrs_allowed_IN| but we may choose to reduce it in the case where the
      instruction disassembler returns an 'is verbose' hint.  This is so as to
      ensure that the JIT doesn't run out of space.  See bug 375839 for a
      motivating example. */

   /* Process instructions. */
   Long delta = delta_IN;
   Int n_instrs_allowed = n_instrs_allowed_IN;

   *n_instrs        = 0;
   *is_verbose_seen = False;
   *extent_base     = guest_IP_sbstart + delta;
   *extent_len      = 0;

   while (True) {
      vassert(*n_instrs < n_instrs_allowed);

      /* This is the IP of the instruction we're just about to deal
         with. */
      Addr guest_IP_curr_instr = guest_IP_sbstart + delta;

      /* This is the irbb statement array index of the first stmt in
         this insn.  That will always be the instruction-mark
         descriptor. */
      Int first_stmt_idx = irbb->stmts_used;

      /* Add an instruction-mark statement.  We won't know until after
         disassembling the instruction how long it instruction is, so
         just put in a zero length and we'll fix it up later.

         On ARM, the least significant bit of the instr address
         distinguishes ARM vs Thumb instructions.  All instructions
         actually start on at least 2-aligned addresses.  So we need
         to ignore the bottom bit of the insn address when forming the
         IMark's address field, but put that bottom bit in the delta
         field, so that comparisons against guest_R15T for Thumb can
         be done correctly.  By inspecting the delta field,
         instruction processors can determine whether the instruction
         was originally Thumb or ARM.  For more details of this
         convention, see comments on definition of guest_R15T in
         libvex_guest_arm.h. */
      if (arch_guest == VexArchARM && (guest_IP_curr_instr & 1)) {
         /* Thumb insn => mask out the T bit, but put it in delta */
         addStmtToIRSB( irbb,
                        IRStmt_IMark(guest_IP_curr_instr & ~(Addr)1,
                                     0, /* len */
                                     1  /* delta */
                        )
         );
      } else {
         /* All other targets: store IP as-is, and set delta to zero. */
         addStmtToIRSB( irbb,
                        IRStmt_IMark(guest_IP_curr_instr,
                                     0, /* len */
                                     0  /* delta */
                        )
         );
      }

      if (debug_print && *n_instrs > 0)
         vex_printf("\n");

      /* Finally, actually disassemble an instruction. */
      vassert(irbb->next == NULL);
      DisResult dres 
         = dis_instr_fn ( irbb, guest_code, delta, guest_IP_curr_instr,
                          arch_guest, archinfo_guest, abiinfo_both,
                          host_endness, sigill_diag );

      /* stay sane ... */
      vassert(dres.whatNext == Dis_StopHere || dres.whatNext == Dis_Continue);
      /* ... disassembled insn length is sane ... */
      vassert(dres.len >= 0 && dres.len <= 24);

      /* If the disassembly function passed us a hint, take note of it. */
      if (LIKELY(dres.hint == Dis_HintNone)) {
         /* Do nothing */
      } else {
         vassert(dres.hint == Dis_HintVerbose);
         /* The current insn is known to be verbose.  Lower the max insns limit
            if necessary so as to avoid running the JIT out of space in the
            event that we've encountered the start of a long sequence of them.
            This is expected to be a very rare event.  In any case the remaining
            limit (in the default setting, 30 insns) is still so high that most
            blocks will terminate anyway before then.  So this is very unlikely
            to give a perf hit in practice.  See bug 375839 for the motivating
            example. */
         if (!(*is_verbose_seen)) {
            *is_verbose_seen = True;
            // Halve the number of allowed insns, but only above 2
            if (n_instrs_allowed > 2) {
               n_instrs_allowed = ((n_instrs_allowed - 2) / 2) + 2;
               //vassert(*n_instrs <= n_instrs_allowed);
            }
         }
      }

      /* Fill in the insn-mark length field. */
      vassert(first_stmt_idx >= 0 && first_stmt_idx < irbb->stmts_used);
      IRStmt* imark = irbb->stmts[first_stmt_idx];
      vassert(imark);
      vassert(imark->tag == Ist_IMark);
      vassert(imark->Ist.IMark.len == 0);
      imark->Ist.IMark.len = dres.len;

      /* Print the resulting IR, if needed. */
      if (vex_traceflags & VEX_TRACE_FE) {
         for (Int i = first_stmt_idx; i < irbb->stmts_used; i++) {
            vex_printf("              ");
            ppIRStmt(irbb->stmts[i]);
            vex_printf("\n");
         }
      }

      /* Individual insn disassembly may not mess with irbb->next.
         This function is the only place where it can be set. */
      vassert(irbb->next == NULL);
      vassert(irbb->jumpkind == Ijk_Boring);
      vassert(irbb->offsIP == 0);

      /* Individual insn disassembly must finish the IR for each
         instruction with an assignment to the guest PC. */
      vassert(first_stmt_idx < irbb->stmts_used);
      /* it follows that irbb->stmts_used must be > 0 */
      { IRStmt* st = irbb->stmts[irbb->stmts_used-1];
        vassert(st);
        vassert(st->tag == Ist_Put);
        vassert(st->Ist.Put.offset == offB_GUEST_IP);
        /* Really we should also check that the type of the Put'd data
           == guest_word_type, but that's a bit expensive. */
      }

      /* Update the extents entry that we are constructing. */
      /* If vex_control.guest_max_insns is required to be < 100 and
         each insn is at max 20 bytes long, this limit of 5000 then
         seems reasonable since the max possible extent length will be
         100 * 20 == 2000. */
      vassert(*extent_len < 5000);
      (*extent_len) += dres.len;
      (*n_instrs)++;

      /* Advance delta (inconspicuous but very important :-) */
      delta += (Long)dres.len;

      Bool stopNow = False;
      switch (dres.whatNext) {
         case Dis_Continue:
            vassert(dres.jk_StopHere == Ijk_INVALID);
            if (*n_instrs >= n_instrs_allowed) {
               /* We have to stop.  See comment above re irbb field
                  settings here. */
               irbb->next = IRExpr_Get(offB_GUEST_IP, guest_word_type);
               /* irbb->jumpkind must already by Ijk_Boring */
               irbb->offsIP = offB_GUEST_IP;
               stopNow = True;
            }
            break;
         case Dis_StopHere:
            vassert(dres.jk_StopHere != Ijk_INVALID);
            /* See comment above re irbb field settings here. */
            irbb->next = IRExpr_Get(offB_GUEST_IP, guest_word_type);
            irbb->jumpkind = dres.jk_StopHere;
            irbb->offsIP = offB_GUEST_IP;
            stopNow = True;
            break;
         default:
            vpanic("bb_to_IR");
      }

      if (stopNow)
         break;
   } /* while (True) */

   /* irbb->next must now be set, since we've finished the block.
      Print it if necessary.*/
   vassert(irbb->next != NULL);
   if (debug_print) {
      vex_printf("              ");
      vex_printf( "PUT(%d) = ", irbb->offsIP);
      ppIRExpr( irbb->next );
      vex_printf( "; exit-");
      ppIRJumpKind(irbb->jumpkind);
      vex_printf( "\n");
      vex_printf( "\n");
   }

   /* And clean it up. */
   irbb = do_minimal_initial_iropt_BB ( irbb );
   if (debug_print) {
      ppIRSB(irbb);
   }

   return irbb;
}


/*--------------------------------------------------------------*/
/*--- Disassembly of traces: helper functions                ---*/
/*--------------------------------------------------------------*/

// Swap the side exit and fall through exit for |bb|.  Update |be| so as to be
// consistent.
static void swap_sx_and_ft ( /*MOD*/IRSB* bb, /*MOD*/BlockEnd* be )
{
   vassert(be->tag == Be_Cond);
   vassert(bb->stmts_used >= 2); // Must have at least: IMark, Exit
   IRStmt* exit = bb->stmts[bb->stmts_used - 1];
   vassert(exit->tag == Ist_Exit);
   vassert(exit->Ist.Exit.guard->tag == Iex_RdTmp);
   vassert(exit->Ist.Exit.guard->Iex.RdTmp.tmp == be->Be.Cond.condSX);
   vassert(bb->next->tag == Iex_Const);
   vassert(bb->jumpkind == Ijk_Boring);
   // We need to insert a new stmt, just before the exit, that computes 'Not1'
   // of the guard condition.  Replace |bb->stmts[bb->stmts_used - 1]| by the
   // new stmt, and then place |exit| immediately after it.
   IRTemp invertedGuard = newIRTemp(bb->tyenv, Ity_I1);
   bb->stmts[bb->stmts_used - 1]
      = IRStmt_WrTmp(invertedGuard,
                     IRExpr_Unop(Iop_Not1, IRExpr_RdTmp(exit->Ist.Exit.guard
                                                            ->Iex.RdTmp.tmp)));
   exit->Ist.Exit.guard->Iex.RdTmp.tmp = invertedGuard;
   addStmtToIRSB(bb, exit);

   // Swap the actual destination constants.
   { IRConst* tmp = exit->Ist.Exit.dst;
     exit->Ist.Exit.dst = bb->next->Iex.Const.con;
     bb->next->Iex.Const.con = tmp;
   }

   // And update |be|.
   { be->Be.Cond.condSX = invertedGuard;
     Long tmp = be->Be.Cond.deltaSX;
     be->Be.Cond.deltaSX = be->Be.Cond.deltaFT;
     be->Be.Cond.deltaFT = tmp;
   }
}


static void update_instr_budget( /*MOD*/Int*  instrs_avail,
                                 /*MOD*/Bool* verbose_mode,
                                 const  Int   bb_instrs_used,
                                 const  Bool  bb_verbose_seen )
{
   if (0)
      vex_printf("UIB: verbose_mode %d, instrs_avail %d, "
                 "bb_instrs_used %d, bb_verbose_seen %d\n",
                 *verbose_mode ? 1 : 0, *instrs_avail,
                 bb_instrs_used, bb_verbose_seen ? 1 : 0);

   vassert(bb_instrs_used <= *instrs_avail);

   if (bb_verbose_seen && !(*verbose_mode)) {
      *verbose_mode = True;
      // Adjust *instrs_avail so that, when it becomes zero, we haven't used
      // more than 50% of vex_control.guest_max_instrs.
      if (bb_instrs_used > vex_control.guest_max_insns / 2) {
         *instrs_avail = 0;
      } else {
         *instrs_avail = vex_control.guest_max_insns / 2;
      }
      vassert(*instrs_avail >= 0);
   }

   // Subtract bb_instrs_used from *instrs_avail, clamping at 0 if necessary.
   if (bb_instrs_used > *instrs_avail) {
      *instrs_avail = 0;
   } else {
      *instrs_avail -= bb_instrs_used;
   }

   vassert(*instrs_avail >= 0);
}

// Add the extent [base, +len) to |vge|.  Asserts if |vge| is already full.
// As an optimisation only, tries to also merge the new extent with the
// previous one, if possible.
static void add_extent ( /*MOD*/VexGuestExtents* vge, Addr base, UShort len )
{
   const UInt limit = sizeof(vge->base) / sizeof(vge->base[0]);
   vassert(limit == 3);
   const UInt i = vge->n_used;
   vassert(i < limit);
   vge->n_used++;
   vge->base[i] = base;
   vge->len[i] = len;
   // Try to merge with the previous extent
   if (i > 0 
       && (((UInt)vge->len[i-1]) + ((UInt)len))
            < 200*25 /* say, 200 insns of size 25 bytes, absolute worst case */
       && vge->base[i-1] + vge->len[i-1] == base) {
      vge->len[i-1] += len;
      vge->n_used--;
      //vex_printf("MERGE\n");
   }
}


/*--------------------------------------------------------------*/
/*--- Disassembly of traces: main function                   ---*/
/*--------------------------------------------------------------*/

/* Disassemble a complete basic block, starting at guest_IP_start, 
   returning a new IRSB.  The disassembler may chase across basic
   block boundaries if it wishes and if chase_into_ok allows it.
   The precise guest address ranges from which code has been taken
   are written into vge.  guest_IP_sbstart is taken to be the IP in
   the guest's address space corresponding to the instruction at
   &guest_code[0].  

   dis_instr_fn is the arch-specific fn to disassemble on function; it
   is this that does the real work.

   needs_self_check is a callback used to ask the caller which of the
   extents, if any, a self check is required for.  The returned value
   is a bitmask with a 1 in position i indicating that the i'th extent
   needs a check.  Since there can be at most 3 extents, the returned
   values must be between 0 and 7.

   The number of extents which did get a self check (0 to 3) is put in
   n_sc_extents.  The caller already knows this because it told us
   which extents to add checks for, via the needs_self_check callback,
   but we ship the number back out here for the caller's convenience.

   preamble_function is a callback which allows the caller to add
   its own IR preamble (following the self-check, if any).  May be
   NULL.  If non-NULL, the IRSB under construction is handed to 
   this function, which presumably adds IR statements to it.  The
   callback may optionally complete the block and direct bb_to_IR
   not to disassemble any instructions into it; this is indicated
   by the callback returning True.

   offB_CMADDR and offB_CMLEN are the offsets of guest_CMADDR and
   guest_CMLEN.  Since this routine has to work for any guest state,
   without knowing what it is, those offsets have to passed in.

   callback_opaque is a caller-supplied pointer to data which the
   callbacks may want to see.  Vex has no idea what it is.
   (In fact it's a VgInstrumentClosure.)
*/

/* Regarding IP updating.  dis_instr_fn (that does the guest specific
   work of disassembling an individual instruction) must finish the
   resulting IR with "PUT(guest_IP) = ".  Hence in all cases it must
   state the next instruction address.

   If the block is to be ended at that point, then this routine
   (bb_to_IR) will set up the next/jumpkind/offsIP fields so as to
   make a transfer (of the right kind) to "GET(guest_IP)".  Hence if
   dis_instr_fn generates incorrect IP updates we will see it
   immediately (due to jumping to the wrong next guest address).

   However it is also necessary to set this up so it can be optimised
   nicely.  The IRSB exit is defined to update the guest IP, so that
   chaining works -- since the chain_me stubs expect the chain-to
   address to be in the guest state.  Hence what the IRSB next fields
   will contain initially is (implicitly)

   PUT(guest_IP) [implicitly] = GET(guest_IP) [explicit expr on ::next]

   which looks pretty strange at first.  Eg so unconditional branch
   to some address 0x123456 looks like this:

   PUT(guest_IP) = 0x123456;  // dis_instr_fn generates this
   // the exit
   PUT(guest_IP) [implicitly] = GET(guest_IP); exit-Boring

   after redundant-GET and -PUT removal by iropt, we get what we want:

   // the exit
   PUT(guest_IP) [implicitly] = 0x123456; exit-Boring

   This makes the IRSB-end case the same as the side-exit case: update
   IP, then transfer.  There is no redundancy of representation for
   the destination, and we use the destination specified by
   dis_instr_fn, so any errors it makes show up sooner.
*/
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
         /*IN*/ Addr             guest_IP_sbstart,
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
      )
{
   Bool debug_print = toBool(vex_traceflags & VEX_TRACE_FE);

   /* check sanity .. */
   vassert(sizeof(HWord) == sizeof(void*));
   vassert(vex_control.guest_max_insns >= 1);
   vassert(vex_control.guest_max_insns <= 100);
   vassert(vex_control.guest_chase == False || vex_control.guest_chase == True);
   vassert(guest_word_type == Ity_I32 || guest_word_type == Ity_I64);

   if (guest_word_type == Ity_I32) {
      vassert(szB_GUEST_IP == 4);
      vassert((offB_GUEST_IP % 4) == 0);
   } else {
      vassert(szB_GUEST_IP == 8);
      vassert((offB_GUEST_IP % 8) == 0);
   }

   /* Initialise all return-by-ref state. */
   vge->n_used     = 0;
   *n_sc_extents   = 0;
   *n_guest_instrs = 0;
   *n_uncond_in_trace = 0;
   *n_cond_in_trace   = 0;

   /* And a new IR superblock to dump the result into. */
   IRSB* irsb = emptyIRSB();

   /* Leave 15 spaces in which to put the check statements for a self
      checking translation (up to 3 extents, and 5 stmts required for
      each).  We won't know until later the extents and checksums of
      the areas, if any, that need to be checked. */
   IRStmt* nop = IRStmt_NoOp();
   Int selfcheck_idx = irsb->stmts_used;
   for (Int i = 0; i < 3 * 5; i++)
      addStmtToIRSB( irsb, nop );

   /* If the caller supplied a function to add its own preamble, use
      it now. */
   if (preamble_function) {
      Bool stopNow = preamble_function( callback_opaque, irsb );
      if (stopNow) {
         /* The callback has completed the IR block without any guest
            insns being disassembled into it, so just return it at
            this point, even if a self-check was requested - as there
            is nothing to self-check.  The 15 self-check no-ops will
            still be in place, but they are harmless. */
         vge->n_used  = 1;
         vge->base[0] = guest_IP_sbstart;
         vge->len[0]  = 0;
         return irsb;
      }
   }

   /* Running state:
         irsb          the SB we are incrementally constructing
         vge           associated extents for irsb
         instrs_used   instrs incorporated in irsb so far
         instrs_avail  number of instrs we have space for
         verbose_mode  did we see an 'is verbose' hint at some point?
   */
   Int  instrs_used  = 0;
   Int  instrs_avail = vex_control.guest_max_insns;
   Bool verbose_mode = False;

   /* Disassemble the initial block until we have to stop. */
   {
      Int    ib_instrs_used  = 0;
      Bool   ib_verbose_seen = False;
      Addr   ib_base         = 0;
      UShort ib_len          = 0;
      irsb = disassemble_basic_block_till_stop(
                /*OUT*/ &ib_instrs_used, &ib_verbose_seen, &ib_base, &ib_len,
                /*MOD*/ irsb,
                /*IN*/  0/*delta for the first block in the trace*/,
                instrs_avail, guest_IP_sbstart, host_endness, sigill_diag,
                arch_guest, archinfo_guest, abiinfo_both, guest_word_type,
                debug_print, dis_instr_fn, guest_code, offB_GUEST_IP
             );
      vassert(ib_instrs_used <= instrs_avail);

      // Update instrs_used, extents, budget.
      instrs_used += ib_instrs_used;
      add_extent(vge, ib_base, ib_len);
      update_instr_budget(&instrs_avail, &verbose_mode,
                          ib_instrs_used, ib_verbose_seen);
   }

   /* Now, see if we can extend the initial block. */
   while (True) {
      const Int n_extent_slots = sizeof(vge->base) / sizeof(vge->base[0]);
      vassert(n_extent_slots == 3);

      // Reasons to give up immediately:
      // User or tool asked us not to chase
      if (!vex_control.guest_chase)
         break;

      // Out of extent slots
      vassert(vge->n_used <= n_extent_slots);
      if (vge->n_used == n_extent_slots)
         break;

      // Almost out of available instructions
      vassert(instrs_avail >= 0);
      if (instrs_avail < 3)
         break;

      // Try for an extend.  What kind we do depends on how the current trace
      // ends.
      BlockEnd irsb_be;
      analyse_block_end(&irsb_be, irsb, guest_IP_sbstart, guest_word_type,
                        chase_into_ok, callback_opaque,
                        offB_GUEST_IP, szB_GUEST_IP, debug_print);

      // Try for an extend based on an unconditional branch or call to a known
      // destination.
      if (irsb_be.tag == Be_Uncond) {
         if (debug_print) {
            vex_printf("\n-+-+ Unconditional follow (ext# %d) to 0x%llx "
                       "-+-+\n\n",
                       (Int)vge->n_used,
                       (ULong)((Long)guest_IP_sbstart+ irsb_be.Be.Uncond.delta));
         }
         Int    bb_instrs_used  = 0;
         Bool   bb_verbose_seen = False;
         Addr   bb_base         = 0;
         UShort bb_len          = 0;
         IRSB*  bb 
            = disassemble_basic_block_till_stop(
                 /*OUT*/ &bb_instrs_used, &bb_verbose_seen, &bb_base, &bb_len,
                 /*MOD*/ emptyIRSB(),
                 /*IN*/  irsb_be.Be.Uncond.delta,
                 instrs_avail, guest_IP_sbstart, host_endness, sigill_diag,
                 arch_guest, archinfo_guest, abiinfo_both, guest_word_type,
                 debug_print, dis_instr_fn, guest_code, offB_GUEST_IP
              );
         vassert(bb_instrs_used <= instrs_avail);

         /* Now we have to append 'bb' to 'irsb'. */
         concatenate_irsbs(irsb, bb);

         // Update instrs_used, extents, budget.
         instrs_used += bb_instrs_used;
         add_extent(vge, bb_base, bb_len);
         update_instr_budget(&instrs_avail, &verbose_mode,
                             bb_instrs_used, bb_verbose_seen);
         *n_uncond_in_trace += 1;
      } // if (be.tag == Be_Uncond)
   
      // Try for an extend based on a conditional branch, specifically in the
      // hope of identifying and recovering, an "A && B" condition spread across
      // two basic blocks.
      if (irsb_be.tag == Be_Cond) {
         if (debug_print) {
            vex_printf("\n-+-+ (ext# %d) Considering cbranch to"
                       " SX=0x%llx FT=0x%llx -+-+\n\n",
                       (Int)vge->n_used,
                       (ULong)((Long)guest_IP_sbstart+ irsb_be.Be.Cond.deltaSX),
                       (ULong)((Long)guest_IP_sbstart+ irsb_be.Be.Cond.deltaFT));
         }
         const Int instrs_avail_spec = 3;

         if (debug_print) {
            vex_printf("-+-+ SPEC side exit -+-+\n\n");
         }
         Int    sx_instrs_used  = 0;
         Bool   sx_verbose_seen = False;
         Addr   sx_base         = 0;
         UShort sx_len          = 0;
         IRSB*  sx_bb
            = disassemble_basic_block_till_stop(
                 /*OUT*/ &sx_instrs_used, &sx_verbose_seen, &sx_base, &sx_len,
                 /*MOD*/ emptyIRSB(),
                 /*IN*/  irsb_be.Be.Cond.deltaSX,
                 instrs_avail_spec, guest_IP_sbstart, host_endness, sigill_diag,
                 arch_guest, archinfo_guest, abiinfo_both, guest_word_type,
                 debug_print, dis_instr_fn, guest_code, offB_GUEST_IP
              );
         vassert(sx_instrs_used <= instrs_avail_spec);
         BlockEnd sx_be;
         analyse_block_end(&sx_be, sx_bb, guest_IP_sbstart, guest_word_type,
                           chase_into_ok, callback_opaque,
                           offB_GUEST_IP, szB_GUEST_IP, debug_print);

         if (debug_print) {
            vex_printf("\n-+-+ SPEC fall through -+-+\n\n");
         }
         Int    ft_instrs_used  = 0;
         Bool   ft_verbose_seen = False;
         Addr   ft_base         = 0;
         UShort ft_len          = 0;
         IRSB*  ft_bb
            = disassemble_basic_block_till_stop(
                 /*OUT*/ &ft_instrs_used, &ft_verbose_seen, &ft_base, &ft_len,
                 /*MOD*/ emptyIRSB(),
                 /*IN*/  irsb_be.Be.Cond.deltaFT,
                 instrs_avail_spec, guest_IP_sbstart, host_endness, sigill_diag,
                 arch_guest, archinfo_guest, abiinfo_both, guest_word_type,
                 debug_print, dis_instr_fn, guest_code, offB_GUEST_IP
              );
         vassert(ft_instrs_used <= instrs_avail_spec);
         BlockEnd ft_be;
         analyse_block_end(&ft_be, ft_bb, guest_IP_sbstart, guest_word_type,
                           chase_into_ok, callback_opaque,
                           offB_GUEST_IP, szB_GUEST_IP, debug_print);

         /* In order for the transformation to be remotely valid, we need:
            - At least one of the sx_bb or ft_bb to be have a Be_Cond end.
            - sx_bb and ft_bb definitely don't form a loop.
         */
         Bool ok = sx_be.tag == Be_Cond || ft_be.tag == Be_Cond;
         if (ok) {
            ok = definitely_does_not_jump_to_delta(&sx_be,
                                                   irsb_be.Be.Cond.deltaFT)
                 || definitely_does_not_jump_to_delta(&ft_be,
                                                      irsb_be.Be.Cond.deltaSX);
         }

         // Check for other mutancy:
         // irsb ft == sx, or the same for ft itself or sx itself
         if (ok) {
            if (irsb_be.Be.Cond.deltaSX == irsb_be.Be.Cond.deltaFT
                || (sx_be.tag == Be_Cond
                    && sx_be.Be.Cond.deltaSX == sx_be.Be.Cond.deltaFT)
                || (ft_be.tag == Be_Cond
                    && ft_be.Be.Cond.deltaSX == ft_be.Be.Cond.deltaFT)) {
               ok = False;
            }
         }

         /* Now let's see if any of our four cases actually holds (viz, is this
            really an && idiom? */
         UInt idiom = 4;
         if (ok) {
            vassert(irsb_be.tag == Be_Cond);
            UInt iom1 = 4/*invalid*/;
            if (sx_be.tag == Be_Cond) {
               /**/ if (sx_be.Be.Cond.deltaFT == irsb_be.Be.Cond.deltaFT)
                       iom1 = 0;
               else if (sx_be.Be.Cond.deltaSX == irsb_be.Be.Cond.deltaFT)
                       iom1 = 1;
            }
            UInt iom2 = 4/*invalid*/;
            if (ft_be.tag == Be_Cond) {
               /**/ if (ft_be.Be.Cond.deltaFT == irsb_be.Be.Cond.deltaSX)
                       iom2 = 2;
               else if (ft_be.Be.Cond.deltaSX == irsb_be.Be.Cond.deltaSX)
                       iom2 = 3;
            }

            /* We should only have identified at most one of the four idioms. */
            vassert(iom1 == 4 || iom2 == 4);
            idiom = (iom1 < 4) ? iom1 : (iom2 < 4 ? iom2 : 4);
            if (idiom == 4) {
               ok = False;
               if (debug_print) {
                  vex_printf("\n-+-+ &&-idiom not recognised, "
                             "giving up. -+-+\n\n");
               }
            }
         }

         if (ok) {
            vassert(idiom < 4);
            // "Normalise" the data so as to ensure we only have one of the four
            // idioms to transform.
            if (idiom == 2 || idiom == 3) {
               swap_sx_and_ft(irsb, &irsb_be);
#              define SWAP(_ty, _aa, _bb) \
                  do { _ty _tmp = _aa; _aa = _bb; _bb = _tmp; } while (0)
               SWAP(Int,      sx_instrs_used,  ft_instrs_used);
               SWAP(Bool,     sx_verbose_seen, ft_verbose_seen);
               SWAP(Addr,     sx_base,         ft_base);
               SWAP(UShort,   sx_len,          ft_len);
               SWAP(IRSB*,    sx_bb,           ft_bb);
               SWAP(BlockEnd, sx_be,           ft_be);
#              undef SWAP
            }
            if (idiom == 1 || idiom == 3) {
               swap_sx_and_ft(sx_bb, &sx_be);
            }
            vassert(sx_be.tag == Be_Cond);
            vassert(sx_be.Be.Cond.deltaFT == irsb_be.Be.Cond.deltaFT);

            if (debug_print) {
               vex_printf("\n-+-+ After normalisation (idiom=%u) -+-+\n", idiom);
               vex_printf("\n-+-+ IRSB -+-+\n");
               ppIRSB(irsb);
               ppBlockEnd(&irsb_be);
               vex_printf("\n\n-+-+ SX -+-+\n");
               ppIRSB(sx_bb);
               ppBlockEnd(&sx_be);
               vex_printf("\n");
            }
            // Finally, check the sx block actually is guardable.
            ok = block_is_guardable(sx_bb);
            if (!ok && debug_print) {
               vex_printf("\n-+-+ SX not guardable, giving up. -+-+\n\n");
            }
         }

         if (ok) {
            if (0 || debug_print) {
               vex_printf("\n-+-+ DOING &&-TRANSFORM -+-+\n");
            }
            // Finally really actually do the transformation.
            // 0. remove the last Exit on irsb.
            // 1. Add irsb->tyenv->types_used to all the tmps in sx_bb,
            //    by calling deltaIRStmt on all stmts.
            // 2. Guard all stmts in sx_bb on irsb_be.Be.Cond.condSX,
            //    **including** the last stmt (which must be an Exit).  It's
            //    here that the And1 is generated.
            // 3. Copy all guarded stmts to the end of irsb.
            vassert(irsb->stmts_used >= 2);
            irsb->stmts_used--;
            Int delta = irsb->tyenv->types_used;

            // Append sx_bb's tyenv to irsb's
            for (Int i = 0; i < sx_bb->tyenv->types_used; i++) {
               (void)newIRTemp(irsb->tyenv, sx_bb->tyenv->types[i]);
            }

            for (Int i = 0; i < sx_bb->stmts_used; i++) {
               IRStmt* st = deepCopyIRStmt(sx_bb->stmts[i]);
               deltaIRStmt(st, delta);
               add_guarded_stmt_to_end_of(irsb, st, irsb_be.Be.Cond.condSX);
            }

            if (debug_print) {
               vex_printf("\n-+-+ FINAL RESULT -+-+\n\n");
               ppIRSB(irsb);
               vex_printf("\n");
            }

            // Update instrs_used, extents, budget.
            instrs_used += sx_instrs_used;
            add_extent(vge, sx_base, sx_len);
            update_instr_budget(&instrs_avail, &verbose_mode,
                                sx_instrs_used, sx_verbose_seen);
            *n_cond_in_trace += 1;
         }
         break;
      } // if (be.tag == Be_Cond)

      // We don't know any other way to extend the block.  Give up.
      else {
         break;
      }

   } // while (True)

   /* We're almost done.  The only thing that might need attending to is that
      a self-checking preamble may need to be created.  If so it gets placed
      in the 15 slots reserved above. */
   create_self_checks_as_needed(
      irsb, n_sc_extents, pxControl, callback_opaque, needs_self_check,
      vge, abiinfo_both, guest_word_type, selfcheck_idx, offB_GUEST_CMSTART,
      offB_GUEST_CMLEN, offB_GUEST_IP, guest_IP_sbstart
   );

   *n_guest_instrs = instrs_used;
   return irsb;
}


/*--------------------------------------------------------------*/
/*--- Functions called by self-checking transations          ---*/
/*--------------------------------------------------------------*/

/* All of these are CLEAN HELPERs */
/* All of these are CALLED FROM GENERATED CODE */

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
