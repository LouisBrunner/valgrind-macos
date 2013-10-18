
/*--------------------------------------------------------------------*/
/*--- Interface to LibVEX_Translate, and the SP-update pass        ---*/
/*---                                                m_translate.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2013 Julian Seward 
      jseward@acm.org

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
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307, USA.

   The GNU General Public License is contained in the file COPYING.
*/

#include "pub_core_basics.h"
#include "pub_core_vki.h"
#include "pub_core_aspacemgr.h"

#include "pub_core_machine.h"    // VG_(fnptr_to_fnentry)
                                 // VG_(get_SP)
                                 // VG_(machine_get_VexArchInfo)
#include "pub_core_libcbase.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcprint.h"
#include "pub_core_options.h"

#include "pub_core_debuginfo.h"  // VG_(get_fnname_w_offset)
#include "pub_core_redir.h"      // VG_(redir_do_lookup)

#include "pub_core_signals.h"    // VG_(synth_fault_{perms,mapping}
#include "pub_core_stacks.h"     // VG_(unknown_SP_update*)()
#include "pub_core_tooliface.h"  // VG_(tdict)

#include "pub_core_translate.h"
#include "pub_core_transtab.h"
#include "pub_core_dispatch.h" // VG_(run_innerloop__dispatch_{un}profiled)
                               // VG_(run_a_noredir_translation__return_point)

#include "pub_core_libcsetjmp.h"   // to keep _threadstate.h happy
#include "pub_core_threadstate.h"  // VexGuestArchState
#include "pub_core_trampoline.h"   // VG_(ppctoc_magic_redirect_return_stub)

#include "pub_core_execontext.h"  // VG_(make_depth_1_ExeContext_from_Addr)

#include "pub_core_gdbserver.h"   // VG_(tool_instrument_then_gdbserver_if_needed)

#include "libvex_emnote.h"        // For PPC, EmWarn_PPC64_redir_underflow

/*------------------------------------------------------------*/
/*--- Stats                                                ---*/
/*------------------------------------------------------------*/

static UInt n_SP_updates_fast            = 0;
static UInt n_SP_updates_generic_known   = 0;
static UInt n_SP_updates_generic_unknown = 0;

void VG_(print_translation_stats) ( void )
{
   HChar buf[7];
   UInt n_SP_updates = n_SP_updates_fast + n_SP_updates_generic_known
                                         + n_SP_updates_generic_unknown;
   VG_(percentify)(n_SP_updates_fast, n_SP_updates, 1, 6, buf);
   VG_(message)(Vg_DebugMsg,
      "translate:            fast SP updates identified: %'u (%s)\n",
      n_SP_updates_fast, buf );

   VG_(percentify)(n_SP_updates_generic_known, n_SP_updates, 1, 6, buf);
   VG_(message)(Vg_DebugMsg,
      "translate:   generic_known SP updates identified: %'u (%s)\n",
      n_SP_updates_generic_known, buf );

   VG_(percentify)(n_SP_updates_generic_unknown, n_SP_updates, 1, 6, buf);
   VG_(message)(Vg_DebugMsg,
      "translate: generic_unknown SP updates identified: %'u (%s)\n",
      n_SP_updates_generic_unknown, buf );
}

/*------------------------------------------------------------*/
/*--- %SP-update pass                                      ---*/
/*------------------------------------------------------------*/

static Bool need_to_handle_SP_assignment(void)
{
   return ( VG_(tdict).track_new_mem_stack_4   ||
            VG_(tdict).track_die_mem_stack_4   ||
            VG_(tdict).track_new_mem_stack_8   ||
            VG_(tdict).track_die_mem_stack_8   ||
            VG_(tdict).track_new_mem_stack_12  ||
            VG_(tdict).track_die_mem_stack_12  ||
            VG_(tdict).track_new_mem_stack_16  ||
            VG_(tdict).track_die_mem_stack_16  ||
            VG_(tdict).track_new_mem_stack_32  ||
            VG_(tdict).track_die_mem_stack_32  ||
            VG_(tdict).track_new_mem_stack_112 ||
            VG_(tdict).track_die_mem_stack_112 ||
            VG_(tdict).track_new_mem_stack_128 ||
            VG_(tdict).track_die_mem_stack_128 ||
            VG_(tdict).track_new_mem_stack_144 ||
            VG_(tdict).track_die_mem_stack_144 ||
            VG_(tdict).track_new_mem_stack_160 ||
            VG_(tdict).track_die_mem_stack_160 ||
            VG_(tdict).track_new_mem_stack     ||
            VG_(tdict).track_die_mem_stack     );
}

// - The SP aliases are held in an array which is used as a circular buffer.
//   This misses very few constant updates of SP (ie. < 0.1%) while using a
//   small, constant structure that will also never fill up and cause
//   execution to abort.
// - Unused slots have a .temp value of 'IRTemp_INVALID'.
// - 'next_SP_alias_slot' is the index where the next alias will be stored.
// - If the buffer fills, we circle around and start over-writing
//   non-IRTemp_INVALID values.  This is rare, and the overwriting of a
//   value that would have subsequently be used is even rarer.
// - Every slot below next_SP_alias_slot holds a non-IRTemp_INVALID value.
//   The rest either all won't (if we haven't yet circled around) or all
//   will (if we have circled around).

typedef 
   struct {
      IRTemp temp;
      Long   delta;
   }
   SP_Alias;

// With 32 slots the buffer fills very rarely -- eg. once in a run of GCC.
// And I've tested with smaller values and the wrap-around case works ok.
#define N_ALIASES    32
static SP_Alias SP_aliases[N_ALIASES];
static Int      next_SP_alias_slot = 0;

static void clear_SP_aliases(void)
{
   Int i;
   for (i = 0; i < N_ALIASES; i++) {
      SP_aliases[i].temp  = IRTemp_INVALID;
      SP_aliases[i].delta = 0;
   }
   next_SP_alias_slot = 0;
}

static void add_SP_alias(IRTemp temp, Long delta)
{
   vg_assert(temp != IRTemp_INVALID);
   SP_aliases[ next_SP_alias_slot ].temp  = temp;
   SP_aliases[ next_SP_alias_slot ].delta = delta;
   next_SP_alias_slot++;
   if (N_ALIASES == next_SP_alias_slot) next_SP_alias_slot = 0;
}

static Bool get_SP_delta(IRTemp temp, Long* delta)
{
   Int i;      // i must be signed!
   vg_assert(IRTemp_INVALID != temp);
   // Search backwards between current buffer position and the start.
   for (i = next_SP_alias_slot-1; i >= 0; i--) {
      if (temp == SP_aliases[i].temp) {
         *delta = SP_aliases[i].delta;
         return True;
      }
   }
   // Search backwards between the end and the current buffer position.
   for (i = N_ALIASES-1; i >= next_SP_alias_slot; i--) {
      if (temp == SP_aliases[i].temp) {
         *delta = SP_aliases[i].delta;
         return True;
      }
   }
   return False;
}

static void update_SP_aliases(Long delta)
{
   Int i;
   for (i = 0; i < N_ALIASES; i++) {
      if (SP_aliases[i].temp == IRTemp_INVALID) {
         return;
      }
      SP_aliases[i].delta += delta;
   }
}

/* Given a guest IP, get an origin tag for a 1-element stack trace,
   and wrap it up in an IR atom that can be passed as the origin-tag
   value for a stack-adjustment helper function. */
static IRExpr* mk_ecu_Expr ( Addr64 guest_IP )
{
   UInt ecu;
   ExeContext* ec
      = VG_(make_depth_1_ExeContext_from_Addr)( (Addr)guest_IP );
   vg_assert(ec);
   ecu = VG_(get_ECU_from_ExeContext)( ec );
   vg_assert(VG_(is_plausible_ECU)(ecu));
   /* This is always safe to do, since ecu is only 32 bits, and
      HWord is 32 or 64. */
   return mkIRExpr_HWord( (HWord)ecu );
}

/* When gdbserver is activated, the translation of a block must
   first be done by the tool function, then followed by a pass
   which (if needed) instruments the code for gdbserver.
*/
static
IRSB* tool_instrument_then_gdbserver_if_needed ( VgCallbackClosure* closureV,
                                                 IRSB*              sb_in, 
                                                 VexGuestLayout*    layout, 
                                                 VexGuestExtents*   vge,
                                                 VexArchInfo*       vai,
                                                 IRType             gWordTy, 
                                                 IRType             hWordTy )
{
   return VG_(instrument_for_gdbserver_if_needed)
      (VG_(tdict).tool_instrument (closureV,
                                   sb_in,
                                   layout,
                                   vge,
                                   vai,
                                   gWordTy,
                                   hWordTy),
       layout,
       vge,
       gWordTy,
       hWordTy);                                   
}

/* For tools that want to know about SP changes, this pass adds
   in the appropriate hooks.  We have to do it after the tool's
   instrumentation, so the tool doesn't have to worry about the C calls
   it adds in, and we must do it before register allocation because
   spilled temps make it much harder to work out the SP deltas.
   This it is done with Vex's "second instrumentation" pass.

   Basically, we look for GET(SP)/PUT(SP) pairs and track constant
   increments/decrements of SP between them.  (This requires tracking one or
   more "aliases", which are not exact aliases but instead are tempregs
   whose value is equal to the SP's plus or minus a known constant.)
   If all the changes to SP leading up to a PUT(SP) are by known, small
   constants, we can do a specific call to eg. new_mem_stack_4, otherwise
   we fall back to the case that handles an unknown SP change.

   There is some extra complexity to deal correctly with updates to
   only parts of SP.  Bizarre, but it has been known to happen.
*/
static
IRSB* vg_SP_update_pass ( void*             closureV,
                          IRSB*             sb_in, 
                          VexGuestLayout*   layout, 
                          VexGuestExtents*  vge,
                          VexArchInfo*      vai,
                          IRType            gWordTy, 
                          IRType            hWordTy )
{
   Int         i, j, k, minoff_ST, maxoff_ST, sizeof_SP, offset_SP;
   Int         first_SP, last_SP, first_Put, last_Put;
   IRDirty     *dcall, *d;
   IRStmt*     st;
   IRExpr*     e;
   IRRegArray* descr;
   IRType      typeof_SP;
   Long        delta, con;

   /* Set up stuff for tracking the guest IP */
   Bool   curr_IP_known = False;
   Addr64 curr_IP       = 0;

   /* Set up BB */
   IRSB* bb     = emptyIRSB();
   bb->tyenv    = deepCopyIRTypeEnv(sb_in->tyenv);
   bb->next     = deepCopyIRExpr(sb_in->next);
   bb->jumpkind = sb_in->jumpkind;
   bb->offsIP   = sb_in->offsIP;

   delta = 0;

   sizeof_SP = layout->sizeof_SP;
   offset_SP = layout->offset_SP;
   typeof_SP = sizeof_SP==4 ? Ity_I32 : Ity_I64;
   vg_assert(sizeof_SP == 4 || sizeof_SP == 8);

   /* --- Start of #defines --- */

#  define IS_ADD(op) (sizeof_SP==4 ? ((op)==Iop_Add32) : ((op)==Iop_Add64))
#  define IS_SUB(op) (sizeof_SP==4 ? ((op)==Iop_Sub32) : ((op)==Iop_Sub64))

#  define IS_ADD_OR_SUB(op) (IS_ADD(op) || IS_SUB(op))

#  define GET_CONST(con)                                                \
       (sizeof_SP==4 ? (Long)(Int)(con->Ico.U32)                        \
                     : (Long)(con->Ico.U64))

#  define DO_NEW(syze, tmpp)                                            \
      do {                                                              \
         Bool vanilla, w_ecu;                                           \
         vg_assert(curr_IP_known);                                      \
         vanilla = NULL != VG_(tdict).track_new_mem_stack_##syze;       \
         w_ecu   = NULL != VG_(tdict).track_new_mem_stack_##syze##_w_ECU; \
         vg_assert(!(vanilla && w_ecu)); /* can't have both */          \
         if (!(vanilla || w_ecu))                                       \
            goto generic;                                               \
                                                                        \
         /* I don't know if it's really necessary to say that the */    \
         /* call reads the stack pointer.  But anyway, we do. */        \
         if (w_ecu) {                                                   \
            dcall = unsafeIRDirty_0_N(                                  \
                       2/*regparms*/,                                   \
                       "track_new_mem_stack_" #syze "_w_ECU",           \
                       VG_(fnptr_to_fnentry)(                           \
                          VG_(tdict).track_new_mem_stack_##syze##_w_ECU ), \
                       mkIRExprVec_2(IRExpr_RdTmp(tmpp),                \
                                     mk_ecu_Expr(curr_IP))              \
                    );                                                  \
         } else {                                                       \
            dcall = unsafeIRDirty_0_N(                                  \
                       1/*regparms*/,                                   \
                       "track_new_mem_stack_" #syze ,                   \
                       VG_(fnptr_to_fnentry)(                           \
                          VG_(tdict).track_new_mem_stack_##syze ),      \
                       mkIRExprVec_1(IRExpr_RdTmp(tmpp))                \
                    );                                                  \
         }                                                              \
         dcall->nFxState = 1;                                           \
         dcall->fxState[0].fx     = Ifx_Read;                           \
         dcall->fxState[0].offset = layout->offset_SP;                  \
         dcall->fxState[0].size   = layout->sizeof_SP;                  \
         dcall->fxState[0].nRepeats  = 0;                               \
         dcall->fxState[0].repeatLen = 0;                               \
                                                                        \
         addStmtToIRSB( bb, IRStmt_Dirty(dcall) );                      \
                                                                        \
         tl_assert(syze > 0);                                           \
         update_SP_aliases(syze);                                       \
                                                                        \
         n_SP_updates_fast++;                                           \
                                                                        \
      } while (0)

#  define DO_DIE(syze, tmpp)                                            \
      do {                                                              \
         if (!VG_(tdict).track_die_mem_stack_##syze)                    \
            goto generic;                                               \
                                                                        \
         /* I don't know if it's really necessary to say that the */    \
         /* call reads the stack pointer.  But anyway, we do. */        \
         dcall = unsafeIRDirty_0_N(                                     \
                    1/*regparms*/,                                      \
                    "track_die_mem_stack_" #syze,                       \
                    VG_(fnptr_to_fnentry)(                              \
                       VG_(tdict).track_die_mem_stack_##syze ),         \
                    mkIRExprVec_1(IRExpr_RdTmp(tmpp))                   \
                 );                                                     \
         dcall->nFxState = 1;                                           \
         dcall->fxState[0].fx     = Ifx_Read;                           \
         dcall->fxState[0].offset = layout->offset_SP;                  \
         dcall->fxState[0].size   = layout->sizeof_SP;                  \
         dcall->fxState[0].nRepeats  = 0;                               \
         dcall->fxState[0].repeatLen = 0;                               \
                                                                        \
         addStmtToIRSB( bb, IRStmt_Dirty(dcall) );                      \
                                                                        \
         tl_assert(syze > 0);                                           \
         update_SP_aliases(-(syze));                                    \
                                                                        \
         n_SP_updates_fast++;                                           \
                                                                        \
      } while (0)

   /* --- End of #defines --- */

   clear_SP_aliases();

   for (i = 0; i <  sb_in->stmts_used; i++) {

      st = sb_in->stmts[i];

      if (st->tag == Ist_IMark) {
         curr_IP_known = True;
         curr_IP       = st->Ist.IMark.addr;
      }

      /* t = Get(sp):   curr = t, delta = 0 */
      if (st->tag != Ist_WrTmp) goto case2;
      e = st->Ist.WrTmp.data;
      if (e->tag != Iex_Get)              goto case2;
      if (e->Iex.Get.offset != offset_SP) goto case2;
      if (e->Iex.Get.ty != typeof_SP)     goto case2;
      vg_assert( typeOfIRTemp(bb->tyenv, st->Ist.WrTmp.tmp) == typeof_SP );
      add_SP_alias(st->Ist.WrTmp.tmp, 0);
      addStmtToIRSB( bb, st );
      continue;

     case2:
      /* t' = curr +/- const:   curr = t',  delta +=/-= const */
      if (st->tag != Ist_WrTmp) goto case3;
      e = st->Ist.WrTmp.data;
      if (e->tag != Iex_Binop) goto case3;
      if (e->Iex.Binop.arg1->tag != Iex_RdTmp) goto case3;
      if (!get_SP_delta(e->Iex.Binop.arg1->Iex.RdTmp.tmp, &delta)) goto case3;
      if (e->Iex.Binop.arg2->tag != Iex_Const) goto case3;
      if (!IS_ADD_OR_SUB(e->Iex.Binop.op)) goto case3;
      con = GET_CONST(e->Iex.Binop.arg2->Iex.Const.con);
      vg_assert( typeOfIRTemp(bb->tyenv, st->Ist.WrTmp.tmp) == typeof_SP );
      if (IS_ADD(e->Iex.Binop.op)) {
         add_SP_alias(st->Ist.WrTmp.tmp, delta + con);
      } else {
         add_SP_alias(st->Ist.WrTmp.tmp, delta - con);
      }
      addStmtToIRSB( bb, st );
      continue;

     case3:
      /* t' = curr:   curr = t' */
      if (st->tag != Ist_WrTmp) goto case4;
      e = st->Ist.WrTmp.data;
      if (e->tag != Iex_RdTmp) goto case4;
      if (!get_SP_delta(e->Iex.RdTmp.tmp, &delta)) goto case4;
      vg_assert( typeOfIRTemp(bb->tyenv, st->Ist.WrTmp.tmp) == typeof_SP );
      add_SP_alias(st->Ist.WrTmp.tmp, delta);
      addStmtToIRSB( bb, st );
      continue;

     case4:
      /* Put(sp) = curr */
      /* More generally, we must correctly handle a Put which writes
         any part of SP, not just the case where all of SP is
         written. */
      if (st->tag != Ist_Put) goto case5;
      first_SP  = offset_SP;
      last_SP   = first_SP + sizeof_SP - 1;
      first_Put = st->Ist.Put.offset;
      last_Put  = first_Put
                  + sizeofIRType( typeOfIRExpr( bb->tyenv, st->Ist.Put.data ))
                  - 1;
      vg_assert(first_SP <= last_SP);
      vg_assert(first_Put <= last_Put);

      if (last_Put < first_SP || last_SP < first_Put)
         goto case5; /* no overlap */

      if (st->Ist.Put.data->tag == Iex_RdTmp
          && get_SP_delta(st->Ist.Put.data->Iex.RdTmp.tmp, &delta)) {
         IRTemp tttmp = st->Ist.Put.data->Iex.RdTmp.tmp;
         /* Why should the following assertion hold?  Because any
            alias added by put_SP_alias must be of a temporary which
            has the same type as typeof_SP, and whose value is a Get
            at exactly offset_SP of size typeof_SP.  Each call to
            put_SP_alias is immediately preceded by an assertion that
            we are putting in a binding for a correctly-typed
            temporary. */
         vg_assert( typeOfIRTemp(bb->tyenv, tttmp) == typeof_SP );
         /* From the same type-and-offset-correctness argument, if 
            we found a useable alias, it must for an "exact" write of SP. */
         vg_assert(first_SP == first_Put);
         vg_assert(last_SP == last_Put);
         switch (delta) {
            case    0:                      addStmtToIRSB(bb,st); continue;
            case    4: DO_DIE(  4,  tttmp); addStmtToIRSB(bb,st); continue;
            case   -4: DO_NEW(  4,  tttmp); addStmtToIRSB(bb,st); continue;
            case    8: DO_DIE(  8,  tttmp); addStmtToIRSB(bb,st); continue;
            case   -8: DO_NEW(  8,  tttmp); addStmtToIRSB(bb,st); continue;
            case   12: DO_DIE(  12, tttmp); addStmtToIRSB(bb,st); continue;
            case  -12: DO_NEW(  12, tttmp); addStmtToIRSB(bb,st); continue;
            case   16: DO_DIE(  16, tttmp); addStmtToIRSB(bb,st); continue;
            case  -16: DO_NEW(  16, tttmp); addStmtToIRSB(bb,st); continue;
            case   32: DO_DIE(  32, tttmp); addStmtToIRSB(bb,st); continue;
            case  -32: DO_NEW(  32, tttmp); addStmtToIRSB(bb,st); continue;
            case  112: DO_DIE( 112, tttmp); addStmtToIRSB(bb,st); continue;
            case -112: DO_NEW( 112, tttmp); addStmtToIRSB(bb,st); continue;
            case  128: DO_DIE( 128, tttmp); addStmtToIRSB(bb,st); continue;
            case -128: DO_NEW( 128, tttmp); addStmtToIRSB(bb,st); continue;
            case  144: DO_DIE( 144, tttmp); addStmtToIRSB(bb,st); continue;
            case -144: DO_NEW( 144, tttmp); addStmtToIRSB(bb,st); continue;
            case  160: DO_DIE( 160, tttmp); addStmtToIRSB(bb,st); continue;
            case -160: DO_NEW( 160, tttmp); addStmtToIRSB(bb,st); continue;
            default:  
               /* common values for ppc64: 144 128 160 112 176 */
               n_SP_updates_generic_known++;
               goto generic;
         }
      } else {
         /* Deal with an unknown update to SP.  We're here because
            either:
            (1) the Put does not exactly cover SP; it is a partial update.
                Highly unlikely, but has been known to happen for 16-bit
                Windows apps running on Wine, doing 16-bit adjustments to
                %sp.
            (2) the Put does exactly cover SP, but we are unable to
                determine how the value relates to the old SP.  In any 
                case, we cannot assume that the Put.data value is a tmp;
                we must assume it can be anything allowed in flat IR (tmp
                or const).
         */
         IRTemp  old_SP;
         n_SP_updates_generic_unknown++;

         // Nb: if all is well, this generic case will typically be
         // called something like every 1000th SP update.  If it's more than
         // that, the above code may be missing some cases.
        generic:
         /* Pass both the old and new SP values to this helper.  Also,
            pass an origin tag, even if it isn't needed. */
         old_SP = newIRTemp(bb->tyenv, typeof_SP);
         addStmtToIRSB( 
            bb,
            IRStmt_WrTmp( old_SP, IRExpr_Get(offset_SP, typeof_SP) ) 
         );

         /* Now we know what the old value of SP is.  But knowing the new
            value is a bit tricky if there is a partial write. */
         if (first_Put == first_SP && last_Put == last_SP) {
            /* The common case, an exact write to SP.  So st->Ist.Put.data
               does hold the new value; simple. */
            vg_assert(curr_IP_known);
            if (NULL != VG_(tdict).track_new_mem_stack_w_ECU)
               dcall = unsafeIRDirty_0_N( 
                          3/*regparms*/, 
                          "VG_(unknown_SP_update_w_ECU)", 
                          VG_(fnptr_to_fnentry)( &VG_(unknown_SP_update_w_ECU) ),
                          mkIRExprVec_3( IRExpr_RdTmp(old_SP), st->Ist.Put.data,
                                         mk_ecu_Expr(curr_IP) ) 
                       );
            else
               dcall = unsafeIRDirty_0_N( 
                          2/*regparms*/, 
                          "VG_(unknown_SP_update)", 
                          VG_(fnptr_to_fnentry)( &VG_(unknown_SP_update) ),
                          mkIRExprVec_2( IRExpr_RdTmp(old_SP), st->Ist.Put.data )
                       );

            addStmtToIRSB( bb, IRStmt_Dirty(dcall) );
            /* don't forget the original assignment */
            addStmtToIRSB( bb, st );
         } else {
            /* We have a partial update to SP.  We need to know what
               the new SP will be, and hand that to the helper call,
               but when the helper call happens, SP must hold the
               value it had before the update.  Tricky.
               Therefore use the following kludge:
               1. do the partial SP update (Put)
               2. Get the new SP value into a tmp, new_SP
               3. Put old_SP
               4. Call the helper
               5. Put new_SP
            */
            IRTemp new_SP;
            /* 1 */
            addStmtToIRSB( bb, st );
            /* 2 */
            new_SP = newIRTemp(bb->tyenv, typeof_SP);
            addStmtToIRSB( 
               bb,
               IRStmt_WrTmp( new_SP, IRExpr_Get(offset_SP, typeof_SP) ) 
            );
            /* 3 */
            addStmtToIRSB( bb, IRStmt_Put(offset_SP, IRExpr_RdTmp(old_SP) ));
            /* 4 */
            vg_assert(curr_IP_known);
            if (NULL != VG_(tdict).track_new_mem_stack_w_ECU)
               dcall = unsafeIRDirty_0_N( 
                          3/*regparms*/, 
                          "VG_(unknown_SP_update_w_ECU)", 
                          VG_(fnptr_to_fnentry)( &VG_(unknown_SP_update_w_ECU) ),
                          mkIRExprVec_3( IRExpr_RdTmp(old_SP),
                                         IRExpr_RdTmp(new_SP), 
                                         mk_ecu_Expr(curr_IP) )
                       );
            else
               dcall = unsafeIRDirty_0_N( 
                          2/*regparms*/, 
                          "VG_(unknown_SP_update)", 
                          VG_(fnptr_to_fnentry)( &VG_(unknown_SP_update) ),
                          mkIRExprVec_2( IRExpr_RdTmp(old_SP),
                                         IRExpr_RdTmp(new_SP) )
                       );
            addStmtToIRSB( bb, IRStmt_Dirty(dcall) );
            /* 5 */
            addStmtToIRSB( bb, IRStmt_Put(offset_SP, IRExpr_RdTmp(new_SP) ));
         }

         /* Forget what we already know. */
         clear_SP_aliases();

         /* If this is a Put of a tmp that exactly updates SP,
            start tracking aliases against this tmp. */

         if (first_Put == first_SP && last_Put == last_SP
             && st->Ist.Put.data->tag == Iex_RdTmp) {
            vg_assert( typeOfIRTemp(bb->tyenv, st->Ist.Put.data->Iex.RdTmp.tmp)
                       == typeof_SP );
            add_SP_alias(st->Ist.Put.data->Iex.RdTmp.tmp, 0);
         }
         continue;
      }

     case5:
      /* PutI or Dirty call which overlaps SP: complain.  We can't
         deal with SP changing in weird ways (well, we can, but not at
         this time of night).  */
      if (st->tag == Ist_PutI) {
         descr = st->Ist.PutI.details->descr;
         minoff_ST = descr->base;
         maxoff_ST = descr->base 
                     + descr->nElems * sizeofIRType(descr->elemTy) - 1;
         if (!(offset_SP > maxoff_ST
               || (offset_SP + sizeof_SP - 1) < minoff_ST))
            goto complain;
      }
      if (st->tag == Ist_Dirty) {
         d = st->Ist.Dirty.details;
         for (j = 0; j < d->nFxState; j++) {
            if (d->fxState[j].fx == Ifx_Read || d->fxState[j].fx == Ifx_None)
               continue;
            /* Enumerate the described state segments */
            for (k = 0; k < 1 + d->fxState[j].nRepeats; k++) {
               minoff_ST = d->fxState[j].offset + k * d->fxState[j].repeatLen;
               maxoff_ST = minoff_ST + d->fxState[j].size - 1;
               if (!(offset_SP > maxoff_ST
                     || (offset_SP + sizeof_SP - 1) < minoff_ST))
                  goto complain;
            }
         }
      }

      /* well, not interesting.  Just copy and keep going. */
      addStmtToIRSB( bb, st );

   } /* for (i = 0; i < sb_in->stmts_used; i++) */

   return bb;

  complain:
   VG_(core_panic)("vg_SP_update_pass: PutI or Dirty which overlaps SP");

#undef IS_ADD
#undef IS_SUB
#undef IS_ADD_OR_SUB
#undef GET_CONST
#undef DO_NEW
#undef DO_DIE
}

/*------------------------------------------------------------*/
/*--- Main entry point for the JITter.                     ---*/
/*------------------------------------------------------------*/

/* Extra comments re self-checking translations and self-modifying
   code.  (JRS 14 Oct 05).

   There are 3 modes:
   (1) no checking: all code assumed to be not self-modifying
   (2) partial: known-problematic situations get a self-check
   (3) full checking: all translations get a self-check

   As currently implemented, the default is (2).  (3) is always safe,
   but very slow.  (1) works mostly, but fails for gcc nested-function
   code which uses trampolines on the stack; this situation is
   detected and handled by (2).

   ----------

   A more robust and transparent solution, which is not currently
   implemented, is a variant of (2): if a translation is made from an
   area which aspacem says does not have 'w' permission, then it can
   be non-self-checking.  Otherwise, it needs a self-check.

   This is complicated by Vex's basic-block chasing.  If a self-check
   is requested, then Vex will not chase over basic block boundaries
   (it's too complex).  However there is still a problem if it chases
   from a non-'w' area into a 'w' area.

   I think the right thing to do is:

   - if a translation request starts in a 'w' area, ask for a
     self-checking translation, and do not allow any chasing (make
     chase_into_ok return False).  Note that the latter is redundant
     in the sense that Vex won't chase anyway in this situation.

   - if a translation request starts in a non-'w' area, do not ask for
     a self-checking translation.  However, do not allow chasing (as
     determined by chase_into_ok) to go into a 'w' area.

   The result of this is that all code inside 'w' areas is self
   checking.

   To complete the trick, there is a caveat: we must watch the
   client's mprotect calls.  If pages are changed from non-'w' to 'w'
   then we should throw away all translations which intersect the
   affected area, so as to force them to be redone with self-checks.

   ----------

   The above outlines the conditions under which bb chasing is allowed
   from a self-modifying-code point of view.  There are other
   situations pertaining to function redirection in which it is
   necessary to disallow chasing, but those fall outside the scope of
   this comment.
*/


/* Vex dumps the final code in here.  Then we can copy it off
   wherever we like. */
/* 60000: should agree with assertion in VG_(add_to_transtab) in
   m_transtab.c. */
#define N_TMPBUF 60000
static UChar tmpbuf[N_TMPBUF];


/* Function pointers we must supply to LibVEX in order that it
   can bomb out and emit messages under Valgrind's control. */
__attribute__ ((noreturn))
static
void failure_exit ( void )
{
   LibVEX_ShowAllocStats();
   VG_(core_panic)("LibVEX called failure_exit().");
}

static
void log_bytes ( HChar* bytes, Int nbytes )
{
  Int i;
  for (i = 0; i < nbytes-3; i += 4)
     VG_(printf)("%c%c%c%c", bytes[i], bytes[i+1], bytes[i+2], bytes[i+3]);
  for (; i < nbytes; i++) 
     VG_(printf)("%c", bytes[i]);
}


/* --------- Various helper functions for translation --------- */

/* Look for reasons to disallow making translations from the given
   segment/addr. */

static Bool translations_allowable_from_seg ( NSegment const* seg, Addr addr )
{
#  if defined(VGA_x86) || defined(VGA_s390x) || defined(VGA_mips32) \
      || defined(VGA_mips64)
   Bool allowR = True;
#  else
   Bool allowR = False;
#  endif
   return seg != NULL
          && (seg->kind == SkAnonC || seg->kind == SkFileC || seg->kind == SkShmC)
          && (seg->hasX 
              || (seg->hasR && (allowR
                                || VG_(has_gdbserver_breakpoint) (addr))));
   /* If GDB/gdbsrv has inserted a breakpoint at addr, assume this is a valid
      location to translate if seg is not executable but is readable.
      This is needed for inferior function calls from GDB: GDB inserts a
      breakpoint on the stack, and expects to regain control before the
      breakpoint instruction at the breakpoint address is really
      executed. For this, the breakpoint instruction must be translated
      so as to have the call to gdbserver executed. */
}


/* Produce a bitmask stating which of the supplied extents needs a
   self-check.  See documentation of
   VexTranslateArgs::needs_self_check for more details about the
   return convention. */

static UInt needs_self_check ( void* closureV,
                               VexGuestExtents* vge )
{
   VgCallbackClosure* closure = (VgCallbackClosure*)closureV;
   UInt i, bitset;

   vg_assert(vge->n_used >= 1 && vge->n_used <= 3);
   bitset = 0;

   for (i = 0; i < vge->n_used; i++) {
      Bool  check = False;
      Addr  addr  = (Addr)vge->base[i];
      SizeT len   = (SizeT)vge->len[i];
      NSegment const* segA = NULL;

#     if defined(VGO_darwin)
      // GrP fixme hack - dyld i386 IMPORT gets rewritten.
      // To really do this correctly, we'd need to flush the 
      // translation cache whenever a segment became +WX.
      segA = VG_(am_find_nsegment)(addr);
      if (segA && segA->hasX && segA->hasW)
         check = True;
#     endif

      if (!check) {
         switch (VG_(clo_smc_check)) {
            case Vg_SmcNone:
               /* never check (except as per Darwin hack above) */
               break;
            case Vg_SmcAll: 
               /* always check */
               check = True;
               break;
            case Vg_SmcStack: {
               /* check if the address is in the same segment as this
                  thread's stack pointer */
               Addr sp = VG_(get_SP)(closure->tid);
               if (!segA) {
                  segA = VG_(am_find_nsegment)(addr);
               }
               NSegment const* segSP = VG_(am_find_nsegment)(sp);
               if (segA && segSP && segA == segSP)
                  check = True;
               break;
            }
            case Vg_SmcAllNonFile: {
               /* check if any part of the extent is not in a
                  file-mapped segment */
               if (!segA) {
                  segA = VG_(am_find_nsegment)(addr);
               }
               if (segA && segA->kind == SkFileC && segA->start <= addr
                   && (len == 0 || addr + len <= segA->end + 1)) {
                  /* in a file-mapped segment; skip the check */
               } else {
                  check = True;
               }
               break;
            }
            default:
               vg_assert(0);
         }
      }

      if (check)
         bitset |= (1 << i);
   }

   return bitset;
}


/* This is a callback passed to LibVEX_Translate.  It stops Vex from
   chasing into function entry points that we wish to redirect.
   Chasing across them obviously defeats the redirect mechanism, with
   bad effects for Memcheck, Helgrind, DRD, Massif, and possibly others.
*/
static Bool chase_into_ok ( void* closureV, Addr64 addr64 )
{
   Addr               addr    = (Addr)addr64;
   NSegment const*    seg     = VG_(am_find_nsegment)(addr);

   /* Work through a list of possibilities why we might not want to
      allow a chase. */

   /* Destination not in a plausible segment? */
   if (!translations_allowable_from_seg(seg, addr))
      goto dontchase;

   /* Destination is redirected? */
   if (addr != VG_(redir_do_lookup)(addr, NULL))
      goto dontchase;

#  if defined(VG_PLAT_USES_PPCTOC)
   /* This needs to be at the start of its own block.  Don't chase. Re
      ULong_to_Ptr, be careful to ensure we only compare 32 bits on a
      32-bit target.*/
   if (ULong_to_Ptr(addr64)
       == (void*)&VG_(ppctoc_magic_redirect_return_stub))
      goto dontchase;
#  endif

   /* overly conservative, but .. don't chase into the distinguished
      address that m_transtab uses as an empty-slot marker for
      VG_(tt_fast). */
   if (addr == TRANSTAB_BOGUS_GUEST_ADDR)
      goto dontchase;

#  if defined(VGA_s390x)
   /* Never chase into an EX instruction. Generating IR for EX causes
      a round-trip through the scheduler including VG_(discard_translations).
      And that's expensive as shown by perf/tinycc.c:
      Chasing into EX increases the number of EX translations from 21 to
      102666 causing a 7x runtime increase for "none" and a 3.2x runtime
      increase for memcheck. */
   if (((UChar *)ULong_to_Ptr(addr))[0] == 0x44 ||   /* EX */
       ((UChar *)ULong_to_Ptr(addr))[0] == 0xC6)     /* EXRL */
     goto dontchase;
#  endif

   /* well, ok then.  go on and chase. */
   return True;

   vg_assert(0);
   /*NOTREACHED*/

  dontchase:
   if (0) VG_(printf)("not chasing into 0x%lx\n", addr);
   return False;
}


/* --------------- helpers for with-TOC platforms --------------- */

/* NOTE: with-TOC platforms are: ppc64-linux. */

static IRExpr* mkU64 ( ULong n ) {
   return IRExpr_Const(IRConst_U64(n));
}
static IRExpr* mkU32 ( UInt n ) {
   return IRExpr_Const(IRConst_U32(n));
}

#if defined(VG_PLAT_USES_PPCTOC)
static IRExpr* mkU8 ( UChar n ) {
   return IRExpr_Const(IRConst_U8(n));
}
static IRExpr* narrowTo32 ( IRTypeEnv* tyenv, IRExpr* e ) {
   if (typeOfIRExpr(tyenv, e) == Ity_I32) {
      return e;
   } else {
      vg_assert(typeOfIRExpr(tyenv, e) == Ity_I64);
      return IRExpr_Unop(Iop_64to32, e);
   }
}

/* Generate code to push word-typed expression 'e' onto this thread's
   redir stack, checking for stack overflow and generating code to
   bomb out if so. */

static void gen_PUSH ( IRSB* bb, IRExpr* e )
{
   IRRegArray* descr;
   IRTemp      t1;
   IRExpr*     one;

#  if defined(VGP_ppc64_linux)
   Int    stack_size       = VEX_GUEST_PPC64_REDIR_STACK_SIZE;
   Int    offB_REDIR_SP    = offsetof(VexGuestPPC64State,guest_REDIR_SP);
   Int    offB_REDIR_STACK = offsetof(VexGuestPPC64State,guest_REDIR_STACK);
   Int    offB_EMNOTE      = offsetof(VexGuestPPC64State,guest_EMNOTE);
   Int    offB_CIA         = offsetof(VexGuestPPC64State,guest_CIA);
   Bool   is64             = True;
   IRType ty_Word          = Ity_I64;
   IROp   op_CmpNE         = Iop_CmpNE64;
   IROp   op_Sar           = Iop_Sar64;
   IROp   op_Sub           = Iop_Sub64;
   IROp   op_Add           = Iop_Add64;
   IRExpr*(*mkU)(ULong)    = mkU64;
   vg_assert(VG_WORDSIZE == 8);
#  else
   Int    stack_size       = VEX_GUEST_PPC32_REDIR_STACK_SIZE;
   Int    offB_REDIR_SP    = offsetof(VexGuestPPC32State,guest_REDIR_SP);
   Int    offB_REDIR_STACK = offsetof(VexGuestPPC32State,guest_REDIR_STACK);
   Int    offB_EMNOTE      = offsetof(VexGuestPPC32State,guest_EMNOTE);
   Int    offB_CIA         = offsetof(VexGuestPPC32State,guest_CIA);
   Bool   is64             = False;
   IRType ty_Word          = Ity_I32;
   IROp   op_CmpNE         = Iop_CmpNE32;
   IROp   op_Sar           = Iop_Sar32;
   IROp   op_Sub           = Iop_Sub32;
   IROp   op_Add           = Iop_Add32;
   IRExpr*(*mkU)(UInt)     = mkU32;
   vg_assert(VG_WORDSIZE == 4);
#  endif

   vg_assert(sizeof(void*) == VG_WORDSIZE);
   vg_assert(sizeof(Word)  == VG_WORDSIZE);
   vg_assert(sizeof(Addr)  == VG_WORDSIZE);

   descr = mkIRRegArray( offB_REDIR_STACK, ty_Word, stack_size );
   t1    = newIRTemp( bb->tyenv, ty_Word );
   one   = mkU(1);

   vg_assert(typeOfIRExpr(bb->tyenv, e) == ty_Word);

   /* t1 = guest_REDIR_SP + 1 */
   addStmtToIRSB(
      bb, 
      IRStmt_WrTmp(
         t1, 
         IRExpr_Binop(op_Add, IRExpr_Get( offB_REDIR_SP, ty_Word ), one)
      )
   );

   /* Bomb out if t1 >=s stack_size, that is, (stack_size-1)-t1 <s 0.
      The destination (0) is a bit bogus but it doesn't matter since
      this is an unrecoverable error and will lead to Valgrind
      shutting down.  _EMNOTE is set regardless - that's harmless
      since is only has a meaning if the exit is taken. */
   addStmtToIRSB(
      bb,
      IRStmt_Put(offB_EMNOTE, mkU32(EmWarn_PPC64_redir_overflow))
   );
   addStmtToIRSB(
      bb,
      IRStmt_Exit(
         IRExpr_Binop(
            op_CmpNE,
            IRExpr_Binop(
               op_Sar,
               IRExpr_Binop(op_Sub,mkU(stack_size-1),IRExpr_RdTmp(t1)),
               mkU8(8 * VG_WORDSIZE - 1)
            ),
            mkU(0)
         ),
         Ijk_EmFail,
         is64 ? IRConst_U64(0) : IRConst_U32(0),
         offB_CIA
      )
   );

   /* guest_REDIR_SP = t1 */
   addStmtToIRSB(bb, IRStmt_Put(offB_REDIR_SP, IRExpr_RdTmp(t1)));

   /* guest_REDIR_STACK[t1+0] = e */
   /* PutI/GetI have I32-typed indexes regardless of guest word size */
   addStmtToIRSB(
      bb, 
      IRStmt_PutI(mkIRPutI(descr, 
                           narrowTo32(bb->tyenv,IRExpr_RdTmp(t1)), 0, e)));
}


/* Generate code to pop a word-sized value from this thread's redir
   stack, binding it to a new temporary, which is returned.  As with
   gen_PUSH, an overflow check is also performed. */

static IRTemp gen_POP ( IRSB* bb )
{
#  if defined(VGP_ppc64_linux)
   Int    stack_size       = VEX_GUEST_PPC64_REDIR_STACK_SIZE;
   Int    offB_REDIR_SP    = offsetof(VexGuestPPC64State,guest_REDIR_SP);
   Int    offB_REDIR_STACK = offsetof(VexGuestPPC64State,guest_REDIR_STACK);
   Int    offB_EMNOTE      = offsetof(VexGuestPPC64State,guest_EMNOTE);
   Int    offB_CIA         = offsetof(VexGuestPPC64State,guest_CIA);
   Bool   is64             = True;
   IRType ty_Word          = Ity_I64;
   IROp   op_CmpNE         = Iop_CmpNE64;
   IROp   op_Sar           = Iop_Sar64;
   IROp   op_Sub           = Iop_Sub64;
   IRExpr*(*mkU)(ULong)    = mkU64;
#  else
   Int    stack_size       = VEX_GUEST_PPC32_REDIR_STACK_SIZE;
   Int    offB_REDIR_SP    = offsetof(VexGuestPPC32State,guest_REDIR_SP);
   Int    offB_REDIR_STACK = offsetof(VexGuestPPC32State,guest_REDIR_STACK);
   Int    offB_EMNOTE      = offsetof(VexGuestPPC32State,guest_EMNOTE);
   Int    offB_CIA         = offsetof(VexGuestPPC32State,guest_CIA);
   Bool   is64             = False;
   IRType ty_Word          = Ity_I32;
   IROp   op_CmpNE         = Iop_CmpNE32;
   IROp   op_Sar           = Iop_Sar32;
   IROp   op_Sub           = Iop_Sub32;
   IRExpr*(*mkU)(UInt)     = mkU32;
#  endif

   IRRegArray* descr = mkIRRegArray( offB_REDIR_STACK, ty_Word, stack_size );
   IRTemp      t1    = newIRTemp( bb->tyenv, ty_Word );
   IRTemp      res   = newIRTemp( bb->tyenv, ty_Word );
   IRExpr*     one   = mkU(1);

   vg_assert(sizeof(void*) == VG_WORDSIZE);
   vg_assert(sizeof(Word)  == VG_WORDSIZE);
   vg_assert(sizeof(Addr)  == VG_WORDSIZE);

   /* t1 = guest_REDIR_SP */
   addStmtToIRSB(
      bb, 
      IRStmt_WrTmp( t1, IRExpr_Get( offB_REDIR_SP, ty_Word ) )
   );

   /* Bomb out if t1 < 0.  Same comments as gen_PUSH apply. */
   addStmtToIRSB(
      bb,
      IRStmt_Put(offB_EMNOTE, mkU32(EmWarn_PPC64_redir_underflow))
   );
   addStmtToIRSB(
      bb,
      IRStmt_Exit(
         IRExpr_Binop(
            op_CmpNE,
            IRExpr_Binop(
               op_Sar,
               IRExpr_RdTmp(t1),
               mkU8(8 * VG_WORDSIZE - 1)
            ),
            mkU(0)
         ),
         Ijk_EmFail,
         is64 ? IRConst_U64(0) : IRConst_U32(0),
         offB_CIA
      )
   );

   /* res = guest_REDIR_STACK[t1+0] */
   /* PutI/GetI have I32-typed indexes regardless of guest word size */
   addStmtToIRSB(
      bb,
      IRStmt_WrTmp(
         res, 
         IRExpr_GetI(descr, narrowTo32(bb->tyenv,IRExpr_RdTmp(t1)), 0)
      )
   );

   /* guest_REDIR_SP = t1-1 */
   addStmtToIRSB(
      bb, 
      IRStmt_Put(offB_REDIR_SP, IRExpr_Binop(op_Sub, IRExpr_RdTmp(t1), one))
   );

   return res;
}

/* Generate code to push LR and R2 onto this thread's redir stack,
   then set R2 to the new value (which is the TOC pointer to be used
   for the duration of the replacement function, as determined by
   m_debuginfo), and set LR to the magic return stub, so we get to
   intercept the return and restore R2 and L2 to the values saved
   here. */

static void gen_push_and_set_LR_R2 ( IRSB* bb, Addr64 new_R2_value )
{
#  if defined(VGP_ppc64_linux)
   Addr64 bogus_RA  = (Addr64)&VG_(ppctoc_magic_redirect_return_stub);
   Int    offB_GPR2 = offsetof(VexGuestPPC64State,guest_GPR2);
   Int    offB_LR   = offsetof(VexGuestPPC64State,guest_LR);
   gen_PUSH( bb, IRExpr_Get(offB_LR,   Ity_I64) );
   gen_PUSH( bb, IRExpr_Get(offB_GPR2, Ity_I64) );
   addStmtToIRSB( bb, IRStmt_Put( offB_LR,   mkU64( bogus_RA )) );
   addStmtToIRSB( bb, IRStmt_Put( offB_GPR2, mkU64( new_R2_value )) );

#  else
#    error Platform is not TOC-afflicted, fortunately
#  endif
}

static void gen_pop_R2_LR_then_bLR ( IRSB* bb )
{
#  if defined(VGP_ppc64_linux)
   Int    offB_GPR2 = offsetof(VexGuestPPC64State,guest_GPR2);
   Int    offB_LR   = offsetof(VexGuestPPC64State,guest_LR);
   Int    offB_CIA  = offsetof(VexGuestPPC64State,guest_CIA);
   IRTemp old_R2    = newIRTemp( bb->tyenv, Ity_I64 );
   IRTemp old_LR    = newIRTemp( bb->tyenv, Ity_I64 );
   /* Restore R2 */
   old_R2 = gen_POP( bb );
   addStmtToIRSB( bb, IRStmt_Put( offB_GPR2, IRExpr_RdTmp(old_R2)) );
   /* Restore LR */
   old_LR = gen_POP( bb );
   addStmtToIRSB( bb, IRStmt_Put( offB_LR, IRExpr_RdTmp(old_LR)) );
   /* Branch to LR */
   /* re boring, we arrived here precisely because a wrapped fn did a
      blr (hence Ijk_Ret); so we should just mark this jump as Boring,
      else one _Call will have resulted in two _Rets. */
   bb->jumpkind = Ijk_Boring;
   bb->next     = IRExpr_Binop(Iop_And64, IRExpr_RdTmp(old_LR), mkU64(~(3ULL)));
   bb->offsIP   = offB_CIA;
#  else
#    error Platform is not TOC-afflicted, fortunately
#  endif
}

static
Bool mk_preamble__ppctoc_magic_return_stub ( void* closureV, IRSB* bb )
{
   VgCallbackClosure* closure = (VgCallbackClosure*)closureV;
   /* Since we're creating the entire IRSB right here, give it a
      proper IMark, as it won't get one any other way, and cachegrind
      will barf if it doesn't have one (fair enough really). */
   addStmtToIRSB( bb, IRStmt_IMark( closure->readdr, 4, 0 ) );
   /* Generate the magic sequence:
         pop R2 from hidden stack
         pop LR from hidden stack
         goto LR
   */
   gen_pop_R2_LR_then_bLR(bb);
   return True; /* True == this is the entire BB; don't disassemble any
                   real insns into it - just hand it directly to
                   optimiser/instrumenter/backend. */
}
#endif

/* --------------- END helpers for with-TOC platforms --------------- */


/* This is the IR preamble generator used for replacement
   functions.  It adds code to set the guest_NRADDR{_GPR2} to zero
   (technically not necessary, but facilitates detecting mixups in
   which a replacement function has been erroneously declared using
   VG_REPLACE_FUNCTION_Z{U,Z} when instead it should have been written
   using VG_WRAP_FUNCTION_Z{U,Z}).

   On with-TOC platforms the follow hacks are also done: LR and R2 are
   pushed onto a hidden stack, R2 is set to the correct value for the
   replacement function, and LR is set to point at the magic
   return-stub address.  Setting LR causes the return of the
   wrapped/redirected function to lead to our magic return stub, which
   restores LR and R2 from said stack and returns for real.

   VG_(get_StackTrace_wrk) understands that the LR value may point to
   the return stub address, and that in that case it can get the real
   LR value from the hidden stack instead. */
static 
Bool mk_preamble__set_NRADDR_to_zero ( void* closureV, IRSB* bb )
{
   Int nraddr_szB
      = sizeof(((VexGuestArchState*)0)->guest_NRADDR);
   vg_assert(nraddr_szB == 4 || nraddr_szB == 8);
   vg_assert(nraddr_szB == VG_WORDSIZE);
   addStmtToIRSB( 
      bb,
      IRStmt_Put( 
         offsetof(VexGuestArchState,guest_NRADDR),
         nraddr_szB == 8 ? mkU64(0) : mkU32(0)
      )
   );
   // t9 needs to be set to point to the start of the redirected function.
#  if defined(VGP_mips32_linux)
   VgCallbackClosure* closure = (VgCallbackClosure*)closureV;
   Int offB_GPR25 = offsetof(VexGuestMIPS32State, guest_r25);
   addStmtToIRSB(bb, IRStmt_Put(offB_GPR25, mkU32(closure->readdr)));
#  endif
#  if defined(VGP_mips64_linux)
   VgCallbackClosure* closure = (VgCallbackClosure*)closureV;
   Int offB_GPR25 = offsetof(VexGuestMIPS64State, guest_r25);
   addStmtToIRSB(bb, IRStmt_Put(offB_GPR25, mkU64(closure->readdr)));
#  endif
#  if defined(VG_PLAT_USES_PPCTOC)
   { VgCallbackClosure* closure = (VgCallbackClosure*)closureV;
     addStmtToIRSB(
        bb,
        IRStmt_Put(
           offsetof(VexGuestArchState,guest_NRADDR_GPR2),
           VG_WORDSIZE==8 ? mkU64(0) : mkU32(0)
        )
     );
     gen_push_and_set_LR_R2 ( bb, VG_(get_tocptr)( closure->readdr ) );
   }
#  endif
   return False;
}

/* Ditto, except set guest_NRADDR to nraddr (the un-redirected guest
   address).  This is needed for function wrapping - so the wrapper
   can read _NRADDR and find the address of the function being
   wrapped.  On toc-afflicted platforms we must also snarf r2. */
static 
Bool mk_preamble__set_NRADDR_to_nraddr ( void* closureV, IRSB* bb )
{
   VgCallbackClosure* closure = (VgCallbackClosure*)closureV;
   Int nraddr_szB
      = sizeof(((VexGuestArchState*)0)->guest_NRADDR);
   vg_assert(nraddr_szB == 4 || nraddr_szB == 8);
   vg_assert(nraddr_szB == VG_WORDSIZE);
   addStmtToIRSB( 
      bb,
      IRStmt_Put( 
         offsetof(VexGuestArchState,guest_NRADDR),
         nraddr_szB == 8
            ? IRExpr_Const(IRConst_U64( closure->nraddr ))
            : IRExpr_Const(IRConst_U32( (UInt)closure->nraddr ))
      )
   );
   // t9 needs to be set to point to the start of the redirected function.
#  if defined(VGP_mips32_linux)
   Int offB_GPR25 = offsetof(VexGuestMIPS32State, guest_r25);
   addStmtToIRSB(bb, IRStmt_Put(offB_GPR25, mkU32(closure->readdr)));
#  endif
#  if defined(VGP_mips64_linux)
   Int offB_GPR25 = offsetof(VexGuestMIPS64State, guest_r25);
   addStmtToIRSB(bb, IRStmt_Put(offB_GPR25, mkU64(closure->readdr)));
#  endif
#  if defined(VGP_ppc64_linux)
   addStmtToIRSB( 
      bb,
      IRStmt_Put( 
         offsetof(VexGuestArchState,guest_NRADDR_GPR2),
         IRExpr_Get(offsetof(VexGuestArchState,guest_GPR2), 
                    VG_WORDSIZE==8 ? Ity_I64 : Ity_I32)
      )
   );
   gen_push_and_set_LR_R2 ( bb, VG_(get_tocptr)( closure->readdr ) );
#  endif
   return False;
}

/* --- Helpers to do with PPC related stack redzones. --- */

__attribute__((unused))
static Bool const_True ( Addr64 guest_addr )
{
   return True;
}

/* --------------- main translation function --------------- */

/* Note: see comments at top of m_redir.c for the Big Picture on how
   redirections are managed. */

typedef 
   enum {
      /* normal translation, redir neither requested nor inhibited */
      T_Normal, 
      /* redir translation, function-wrap (set _NRADDR) style */
      T_Redir_Wrap,
      /* redir translation, replacement (don't set _NRADDR) style */
      T_Redir_Replace,
      /* a translation in which redir is specifically disallowed */
      T_NoRedir
   }
   T_Kind;

/* Translate the basic block beginning at NRADDR, and add it to the
   translation cache & translation table.  Unless
   DEBUGGING_TRANSLATION is true, in which case the call is being done
   for debugging purposes, so (a) throw away the translation once it
   is made, and (b) produce a load of debugging output.  If
   ALLOW_REDIRECTION is False, do not attempt redirection of NRADDR,
   and also, put the resulting translation into the no-redirect tt/tc
   instead of the normal one.

   TID is the identity of the thread requesting this translation.
*/

Bool VG_(translate) ( ThreadId tid, 
                      Addr64   nraddr,
                      Bool     debugging_translation,
                      Int      debugging_verbosity,
                      ULong    bbs_done,
                      Bool     allow_redirection )
{
   Addr64             addr;
   T_Kind             kind;
   Int                tmpbuf_used, verbosity, i;
   Bool (*preamble_fn)(void*,IRSB*);
   VexArch            vex_arch;
   VexArchInfo        vex_archinfo;
   VexAbiInfo         vex_abiinfo;
   VexGuestExtents    vge;
   VexTranslateArgs   vta;
   VexTranslateResult tres;
   VgCallbackClosure  closure;

   /* Make sure Vex is initialised right. */

   static Bool vex_init_done = False;

   if (!vex_init_done) {
      LibVEX_Init ( &failure_exit, &log_bytes, 
                    1,     /* debug_paranoia */ 
                    False, /* valgrind support */
                    &VG_(clo_vex_control) );
      vex_init_done = True;
   }

   /* Establish the translation kind and actual guest address to
      start from.  Sets (addr,kind). */
   if (allow_redirection) {
      Bool isWrap;
      Addr64 tmp = VG_(redir_do_lookup)( nraddr, &isWrap );
      if (tmp == nraddr) {
         /* no redirection found */
         addr = nraddr;
         kind = T_Normal;
      } else {
         /* found a redirect */
         addr = tmp;
         kind = isWrap ? T_Redir_Wrap : T_Redir_Replace;
      }
   } else {
      addr = nraddr;
      kind = T_NoRedir;
   }

   /* Established: (nraddr, addr, kind) */

   /* Printing redirection info. */

   if ((kind == T_Redir_Wrap || kind == T_Redir_Replace)
       && (VG_(clo_verbosity) >= 2 || VG_(clo_trace_redir))) {
      Bool ok;
      HChar name1[512] = "";
      HChar name2[512] = "";
      name1[0] = name2[0] = 0;
      ok = VG_(get_fnname_w_offset)(nraddr, name1, 512);
      if (!ok) VG_(strcpy)(name1, "???");
      ok = VG_(get_fnname_w_offset)(addr, name2, 512);
      if (!ok) VG_(strcpy)(name2, "???");
      VG_(message)(Vg_DebugMsg, 
                   "REDIR: 0x%llx (%s) redirected to 0x%llx (%s)\n",
                   nraddr, name1,
                   addr, name2 );
   }

   if (!debugging_translation)
      VG_TRACK( pre_mem_read, Vg_CoreTranslate, 
                              tid, "(translator)", addr, 1 );

   /* If doing any code printing, print a basic block start marker */
   if (VG_(clo_trace_flags) || debugging_translation) {
      HChar fnname[512] = "UNKNOWN_FUNCTION";
      VG_(get_fnname_w_offset)(addr, fnname, 512);
      const HChar* objname = "UNKNOWN_OBJECT";
      OffT         objoff  = 0;
      DebugInfo*   di      = VG_(find_DebugInfo)( addr );
      if (di) {
         objname = VG_(DebugInfo_get_filename)(di);
         objoff  = addr - VG_(DebugInfo_get_text_bias)(di);
      }
      vg_assert(objname);
      VG_(printf)(
         "==== SB %d (evchecks %lld) [tid %d] 0x%llx %s %s+0x%llx\n",
         VG_(get_bbs_translated)(), bbs_done, (Int)tid, addr,
         fnname, objname, (ULong)objoff
      );
   }

   /* Are we allowed to translate here? */

   { /* BEGIN new scope specially for 'seg' */
   NSegment const* seg = VG_(am_find_nsegment)(addr);

   if ( (!translations_allowable_from_seg(seg, addr))
        || addr == TRANSTAB_BOGUS_GUEST_ADDR ) {
      if (VG_(clo_trace_signals))
         VG_(message)(Vg_DebugMsg, "translations not allowed here (0x%llx)"
                                   " - throwing SEGV\n", addr);
      /* U R busted, sonny.  Place your hands on your head and step
         away from the orig_addr. */
      /* Code address is bad - deliver a signal instead */
      if (seg != NULL) {
         /* There's some kind of segment at the requested place, but we
            aren't allowed to execute code here. */
         if (debugging_translation)
            VG_(printf)("translations not allowed here (segment not executable)"
                        "(0x%llx)\n", addr);
         else
            VG_(synth_fault_perms)(tid, addr);
      } else {
        /* There is no segment at all; we are attempting to execute in
           the middle of nowhere. */
         if (debugging_translation)
            VG_(printf)("translations not allowed here (no segment)"
                        "(0x%llx)\n", addr);
         else
            VG_(synth_fault_mapping)(tid, addr);
      }
      return False;
   }

   /* True if a debug trans., or if bit N set in VG_(clo_trace_codegen). */
   verbosity = 0;
   if (debugging_translation) {
      verbosity = debugging_verbosity;
   }
   else
   if ( (VG_(clo_trace_flags) > 0
        && VG_(get_bbs_translated)() <= VG_(clo_trace_notabove)
        && VG_(get_bbs_translated)() >= VG_(clo_trace_notbelow) )) {
      verbosity = VG_(clo_trace_flags);
   }

   /* Figure out which preamble-mangling callback to send. */
   preamble_fn = NULL;
   if (kind == T_Redir_Replace)
      preamble_fn = mk_preamble__set_NRADDR_to_zero;
   else 
   if (kind == T_Redir_Wrap)
      preamble_fn = mk_preamble__set_NRADDR_to_nraddr;

#  if defined(VG_PLAT_USES_PPCTOC)
   if (ULong_to_Ptr(nraddr)
       == (void*)&VG_(ppctoc_magic_redirect_return_stub)) {
      /* If entering the special return stub, this means a wrapped or
         redirected function is returning.  Make this translation one
         which restores R2 and LR from the thread's hidden redir
         stack, and branch to the (restored) link register, thereby
         really causing the function to return. */
      vg_assert(kind == T_Normal);
      vg_assert(nraddr == addr);
      preamble_fn = mk_preamble__ppctoc_magic_return_stub;
   }
#  endif

   /* ------ Actually do the translation. ------ */
   tl_assert2(VG_(tdict).tool_instrument,
              "you forgot to set VgToolInterface function 'tool_instrument'");

   /* Get the CPU info established at startup. */
   VG_(machine_get_VexArchInfo)( &vex_arch, &vex_archinfo );

   /* Set up 'abiinfo' structure with stuff Vex needs to know about
      the guest and host ABIs. */

   LibVEX_default_VexAbiInfo( &vex_abiinfo );
   vex_abiinfo.guest_stack_redzone_size = VG_STACK_REDZONE_SZB;

#  if defined(VGP_amd64_linux)
   vex_abiinfo.guest_amd64_assume_fs_is_zero  = True;
#  endif
#  if defined(VGP_amd64_darwin)
   vex_abiinfo.guest_amd64_assume_gs_is_0x60  = True;
#  endif
#  if defined(VGP_ppc32_linux)
   vex_abiinfo.guest_ppc_zap_RZ_at_blr        = False;
   vex_abiinfo.guest_ppc_zap_RZ_at_bl         = NULL;
   vex_abiinfo.host_ppc32_regalign_int64_args = True;
#  endif
#  if defined(VGP_ppc64_linux)
   vex_abiinfo.guest_ppc_zap_RZ_at_blr        = True;
   vex_abiinfo.guest_ppc_zap_RZ_at_bl         = const_True;
   vex_abiinfo.host_ppc_calls_use_fndescrs    = True;
#  endif

   /* Set up closure args. */
   closure.tid    = tid;
   closure.nraddr = nraddr;
   closure.readdr = addr;

   /* Set up args for LibVEX_Translate. */
   vta.arch_guest       = vex_arch;
   vta.archinfo_guest   = vex_archinfo;
   vta.arch_host        = vex_arch;
   vta.archinfo_host    = vex_archinfo;
   vta.abiinfo_both     = vex_abiinfo;
   vta.callback_opaque  = (void*)&closure;
   vta.guest_bytes      = (UChar*)ULong_to_Ptr(addr);
   vta.guest_bytes_addr = (Addr64)addr;
   vta.chase_into_ok    = chase_into_ok;
   vta.guest_extents    = &vge;
   vta.host_bytes       = tmpbuf;
   vta.host_bytes_size  = N_TMPBUF;
   vta.host_bytes_used  = &tmpbuf_used;
   { /* At this point we have to reconcile Vex's view of the
        instrumentation callback - which takes a void* first argument
        - with Valgrind's view, in which the first arg is a
        VgCallbackClosure*.  Hence the following longwinded casts.
        They are entirely legal but longwinded so as to maximise the
        chance of the C typechecker picking up any type snafus. */
     IRSB*(*f)(VgCallbackClosure*,
               IRSB*,VexGuestLayout*,VexGuestExtents*, VexArchInfo*,
               IRType,IRType)
        = VG_(clo_vgdb) != Vg_VgdbNo
             ? tool_instrument_then_gdbserver_if_needed
             : VG_(tdict).tool_instrument;
     IRSB*(*g)(void*,
               IRSB*,VexGuestLayout*,VexGuestExtents*,VexArchInfo*,
               IRType,IRType)
       = (IRSB*(*)(void*,IRSB*,VexGuestLayout*,VexGuestExtents*,
                   VexArchInfo*,IRType,IRType))f;
     vta.instrument1     = g;
   }
   /* No need for type kludgery here. */
   vta.instrument2       = need_to_handle_SP_assignment()
                              ? vg_SP_update_pass
                              : NULL;
   vta.finaltidy         = VG_(needs).final_IR_tidy_pass
                              ? VG_(tdict).tool_final_IR_tidy_pass
                              : NULL;
   vta.needs_self_check  = needs_self_check;
   vta.preamble_function = preamble_fn;
   vta.traceflags        = verbosity;
   vta.sigill_diag       = VG_(clo_sigill_diag);
   vta.addProfInc        = VG_(clo_profyle_sbs) && kind != T_NoRedir;

   /* Set up the dispatch continuation-point info.  If this is a
      no-redir translation then it cannot be chained, and the chain-me
      points are set to NULL to indicate that.  The indir point must
      also be NULL, since we can't allow this translation to do an
      indir transfer -- that would take it back into the main
      translation cache too.

      All this is because no-redir translations live outside the main
      translation cache (in a secondary one) and chaining them would
      involve more adminstrative complexity that isn't worth the
      hassle, because we don't expect them to get used often.  So
      don't bother. */
   if (allow_redirection) {
      vta.disp_cp_chain_me_to_slowEP
         = VG_(fnptr_to_fnentry)( &VG_(disp_cp_chain_me_to_slowEP) );
      vta.disp_cp_chain_me_to_fastEP
         = VG_(fnptr_to_fnentry)( &VG_(disp_cp_chain_me_to_fastEP) );
      vta.disp_cp_xindir
         = VG_(fnptr_to_fnentry)( &VG_(disp_cp_xindir) );
   } else {
      vta.disp_cp_chain_me_to_slowEP = NULL;
      vta.disp_cp_chain_me_to_fastEP = NULL;
      vta.disp_cp_xindir             = NULL;
   }
   /* This doesn't involve chaining and so is always allowable. */
   vta.disp_cp_xassisted
      = VG_(fnptr_to_fnentry)( &VG_(disp_cp_xassisted) );

   /* Sheesh.  Finally, actually _do_ the translation! */
   tres = LibVEX_Translate ( &vta );

   vg_assert(tres.status == VexTransOK);
   vg_assert(tres.n_sc_extents >= 0 && tres.n_sc_extents <= 3);
   vg_assert(tmpbuf_used <= N_TMPBUF);
   vg_assert(tmpbuf_used > 0);

   /* Tell aspacem of all segments that have had translations taken
      from them.  Optimisation: don't re-look up vge.base[0] since seg
      should already point to it. */

   vg_assert( vge.base[0] == (Addr64)addr );
   /* set 'translations taken from this segment' flag */
   VG_(am_set_segment_hasT_if_SkFileC_or_SkAnonC)( seg );
   } /* END new scope specially for 'seg' */

   for (i = 1; i < vge.n_used; i++) {
      NSegment const* seg 
         = VG_(am_find_nsegment)( vge.base[i] );
      /* set 'translations taken from this segment' flag */
      VG_(am_set_segment_hasT_if_SkFileC_or_SkAnonC)( seg );
   }

   /* Copy data at trans_addr into the translation cache. */
   vg_assert(tmpbuf_used > 0 && tmpbuf_used < 65536);

   // If debugging, don't do anything with the translated block;  we
   // only did this for the debugging output produced along the way.
   if (!debugging_translation) {

      if (kind != T_NoRedir) {
          // Put it into the normal TT/TC structures.  This is the
          // normal case.

          // Note that we use nraddr (the non-redirected address), not
          // addr, which might have been changed by the redirection
          VG_(add_to_transtab)( &vge,
                                nraddr,
                                (Addr)(&tmpbuf[0]), 
                                tmpbuf_used,
                                tres.n_sc_extents > 0,
                                tres.offs_profInc,
                                tres.n_guest_instrs,
                                vex_arch );
      } else {
          vg_assert(tres.offs_profInc == -1); /* -1 == unset */
          VG_(add_to_unredir_transtab)( &vge,
                                        nraddr,
                                        (Addr)(&tmpbuf[0]), 
                                        tmpbuf_used );
      }
   }

   return True;
}

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
