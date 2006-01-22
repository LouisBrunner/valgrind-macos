
/*--------------------------------------------------------------------*/
/*--- Interface to LibVEX_Translate, and the SP-update pass        ---*/
/*---                                                m_translate.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2005 Julian Seward 
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
#include "pub_core_stacks.h"     // VG_(unknown_SP_update)()
#include "pub_core_tooliface.h"  // VG_(tdict)

#include "pub_core_translate.h"
#include "pub_core_transtab.h"
#include "pub_core_dispatch.h" // VG_(run_innerloop__dispatch_{un}profiled)
                               // VG_(run_a_noredir_translation__return_point)

#include "pub_core_threadstate.h"  // VexGuestArchState
#include "pub_core_trampoline.h"   // VG_(ppc64_linux_magic_redirect_return_stub)


/*------------------------------------------------------------*/
/*--- Stats                                                ---*/
/*------------------------------------------------------------*/

static UInt n_SP_updates_fast            = 0;
static UInt n_SP_updates_generic_known   = 0;
static UInt n_SP_updates_generic_unknown = 0;

void VG_(print_translation_stats) ( void )
{
   Char buf[6];
   UInt n_SP_updates = n_SP_updates_fast + n_SP_updates_generic_known
                                         + n_SP_updates_generic_unknown;
   VG_(percentify)(n_SP_updates_fast, n_SP_updates, 1, 6, buf);
   VG_(message)(Vg_DebugMsg,
      "translate:            fast SP updates identified: %,u (%s)",
      n_SP_updates_fast, buf );

   VG_(percentify)(n_SP_updates_generic_known, n_SP_updates, 1, 6, buf);
   VG_(message)(Vg_DebugMsg,
      "translate:   generic_known SP updates identified: %,u (%s)",
      n_SP_updates_generic_known, buf );

   VG_(percentify)(n_SP_updates_generic_unknown, n_SP_updates, 1, 6, buf);
   VG_(message)(Vg_DebugMsg,
      "translate: generic_unknown SP updates identified: %,u (%s)",
      n_SP_updates_generic_unknown, buf );
}

/*------------------------------------------------------------*/
/*--- %SP-update pass                                      ---*/
/*------------------------------------------------------------*/

static Bool need_to_handle_SP_assignment(void)
{
   return ( VG_(tdict).track_new_mem_stack_4  ||
            VG_(tdict).track_die_mem_stack_4  ||
            VG_(tdict).track_new_mem_stack_8  ||
            VG_(tdict).track_die_mem_stack_8  ||
            VG_(tdict).track_new_mem_stack_12 ||
            VG_(tdict).track_die_mem_stack_12 ||
            VG_(tdict).track_new_mem_stack_16 ||
            VG_(tdict).track_die_mem_stack_16 ||
            VG_(tdict).track_new_mem_stack_32 ||
            VG_(tdict).track_die_mem_stack_32 ||
            VG_(tdict).track_new_mem_stack    ||
            VG_(tdict).track_die_mem_stack    );
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

static Bool get_SP_delta(IRTemp temp, ULong* delta)
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
*/
static
IRBB* vg_SP_update_pass ( void*             closureV,
                          IRBB*             bb_in, 
                          VexGuestLayout*   layout, 
                          VexGuestExtents*  vge,
                          IRType            gWordTy, 
                          IRType            hWordTy )
{
   Int      i, j, minoff_ST, maxoff_ST, sizeof_SP, offset_SP;
   IRDirty  *dcall, *d;
   IRStmt*  st;
   IRExpr*  e;
   IRArray* descr;
   IRType   typeof_SP;
   Long     delta, con;

   /* Set up BB */
   IRBB* bb     = emptyIRBB();
   bb->tyenv    = dopyIRTypeEnv(bb_in->tyenv);
   bb->next     = dopyIRExpr(bb_in->next);
   bb->jumpkind = bb_in->jumpkind;

   delta = 0;

   sizeof_SP = layout->sizeof_SP;
   offset_SP = layout->offset_SP;
   typeof_SP = sizeof_SP==4 ? Ity_I32 : Ity_I64;
   vg_assert(sizeof_SP == 4 || sizeof_SP == 8);

#  define IS_ADD(op) (sizeof_SP==4 ? ((op)==Iop_Add32) : ((op)==Iop_Add64))
#  define IS_SUB(op) (sizeof_SP==4 ? ((op)==Iop_Sub32) : ((op)==Iop_Sub64))

#  define IS_ADD_OR_SUB(op) (IS_ADD(op) || IS_SUB(op))

#  define GET_CONST(con)                                                \
       (sizeof_SP==4 ? (Long)(Int)(con->Ico.U32)                        \
                     : (Long)(con->Ico.U64))

// XXX: convert this to a function
#  define DO(kind, syze, tmpp)                                          \
      do {                                                              \
         if (!VG_(tdict).track_##kind##_mem_stack_##syze)               \
            goto generic;                                               \
                                                                        \
         /* I don't know if it's really necessary to say that the */    \
         /* call reads the stack pointer.  But anyway, we do. */        \
         dcall = unsafeIRDirty_0_N(                                     \
                    1/*regparms*/,                                      \
                    "track_" #kind "_mem_stack_" #syze,                 \
                    VG_(fnptr_to_fnentry)(                              \
                       VG_(tdict).track_##kind##_mem_stack_##syze ),    \
                    mkIRExprVec_1(IRExpr_Tmp(tmpp))                     \
                 );                                                     \
         dcall->nFxState = 1;                                           \
         dcall->fxState[0].fx     = Ifx_Read;                           \
         dcall->fxState[0].offset = layout->offset_SP;                  \
         dcall->fxState[0].size   = layout->sizeof_SP;                  \
                                                                        \
         addStmtToIRBB( bb, IRStmt_Dirty(dcall) );                      \
                                                                        \
         update_SP_aliases(-delta);                                     \
                                                                        \
         n_SP_updates_fast++;                                           \
                                                                        \
      } while (0)

   clear_SP_aliases();

   for (i = 0; i <  bb_in->stmts_used; i++) {

      st = bb_in->stmts[i];

      /* t = Get(sp):   curr = t, delta = 0 */
      if (st->tag != Ist_Tmp) goto case2;
      e = st->Ist.Tmp.data;
      if (e->tag != Iex_Get)              goto case2;
      if (e->Iex.Get.offset != offset_SP) goto case2;
      if (e->Iex.Get.ty != typeof_SP)     goto case2;
      add_SP_alias(st->Ist.Tmp.tmp, 0);
      addStmtToIRBB( bb, st );
      continue;

     case2:
      /* t' = curr +/- const:   curr = t',  delta +=/-= const */
      if (st->tag != Ist_Tmp) goto case3;
      e = st->Ist.Tmp.data;
      if (e->tag != Iex_Binop) goto case3;
      if (e->Iex.Binop.arg1->tag != Iex_Tmp) goto case3;
      if (!get_SP_delta(e->Iex.Binop.arg1->Iex.Tmp.tmp, &delta)) goto case3;
      if (e->Iex.Binop.arg2->tag != Iex_Const) goto case3;
      if (!IS_ADD_OR_SUB(e->Iex.Binop.op)) goto case3;
      con = GET_CONST(e->Iex.Binop.arg2->Iex.Const.con);
      if (IS_ADD(e->Iex.Binop.op)) {
         add_SP_alias(st->Ist.Tmp.tmp, delta + con);
      } else {
         add_SP_alias(st->Ist.Tmp.tmp, delta - con);
      }
      addStmtToIRBB( bb, st );
      continue;

     case3:
      /* t' = curr:   curr = t' */
      if (st->tag != Ist_Tmp) goto case4;
      e = st->Ist.Tmp.data;
      if (e->tag != Iex_Tmp) goto case4;
      if (!get_SP_delta(e->Iex.Tmp.tmp, &delta)) goto case4;
      add_SP_alias(st->Ist.Tmp.tmp, delta);
      addStmtToIRBB( bb, st );
      continue;

     case4:
      /* Put(sp) = curr */
      if (st->tag != Ist_Put) goto case5;
      if (st->Ist.Put.offset != offset_SP) goto case5;
      if (st->Ist.Put.data->tag != Iex_Tmp) goto case5;
      if (get_SP_delta(st->Ist.Put.data->Iex.Tmp.tmp, &delta)) {
         IRTemp tttmp = st->Ist.Put.data->Iex.Tmp.tmp;
         switch (delta) {
            case   0:                     addStmtToIRBB(bb,st); continue;
            case   4: DO(die, 4,  tttmp); addStmtToIRBB(bb,st); continue;
            case  -4: DO(new, 4,  tttmp); addStmtToIRBB(bb,st); continue;
            case   8: DO(die, 8,  tttmp); addStmtToIRBB(bb,st); continue;
            case  -8: DO(new, 8,  tttmp); addStmtToIRBB(bb,st); continue;
            case  12: DO(die, 12, tttmp); addStmtToIRBB(bb,st); continue;
            case -12: DO(new, 12, tttmp); addStmtToIRBB(bb,st); continue;
            case  16: DO(die, 16, tttmp); addStmtToIRBB(bb,st); continue;
            case -16: DO(new, 16, tttmp); addStmtToIRBB(bb,st); continue;
            case  32: DO(die, 32, tttmp); addStmtToIRBB(bb,st); continue;
            case -32: DO(new, 32, tttmp); addStmtToIRBB(bb,st); continue;
            default:  
               /* common values for ppc64: 144 128 160 112 176 */
               n_SP_updates_generic_known++;
               goto generic;
         }
      } else {
         IRTemp old_SP;
         n_SP_updates_generic_unknown++;

         // Nb: if all is well, this generic case will typically be
         // called something like every 1000th SP update.  If it's more than
         // that, the above code may be missing some cases.
        generic:
         /* Pass both the old and new SP values to this helper. */
         old_SP = newIRTemp(bb->tyenv, typeof_SP);
         addStmtToIRBB( 
            bb,
            IRStmt_Tmp( old_SP, IRExpr_Get(offset_SP, typeof_SP) ) 
         );

         dcall = unsafeIRDirty_0_N( 
                    2/*regparms*/, 
                    "VG_(unknown_SP_update)", 
                    VG_(fnptr_to_fnentry)( &VG_(unknown_SP_update) ),
                    mkIRExprVec_2( IRExpr_Tmp(old_SP), st->Ist.Put.data ) 
                 );
         addStmtToIRBB( bb, IRStmt_Dirty(dcall) );

         addStmtToIRBB( bb, st );

         clear_SP_aliases();
         add_SP_alias(st->Ist.Put.data->Iex.Tmp.tmp, 0);
         continue;
      }

     case5:
      /* PutI or Dirty call which overlaps SP: complain.  We can't
         deal with SP changing in weird ways (well, we can, but not at
         this time of night).  */
      if (st->tag == Ist_PutI) {
         descr = st->Ist.PutI.descr;
         minoff_ST = descr->base;
         maxoff_ST = descr->base + descr->nElems * sizeofIRType(descr->elemTy) - 1;
         if (!(offset_SP > maxoff_ST || (offset_SP + sizeof_SP - 1) < minoff_ST))
            goto complain;
      }
      if (st->tag == Ist_Dirty) {
         d = st->Ist.Dirty.details;
         for (j = 0; j < d->nFxState; j++) {
            minoff_ST = d->fxState[j].offset;
            maxoff_ST = d->fxState[j].offset + d->fxState[j].size - 1;
            if (d->fxState[j].fx == Ifx_Read || d->fxState[j].fx == Ifx_None)
               continue;
            if (!(offset_SP > maxoff_ST || (offset_SP + sizeof_SP - 1) < minoff_ST))
               goto complain;
         }
      }

      /* well, not interesting.  Just copy and keep going. */
      addStmtToIRBB( bb, st );

   } /* for (i = 0; i <  bb_in->stmts_used; i++) */

   return bb;

  complain:
   VG_(core_panic)("vg_SP_update_pass: PutI or Dirty which overlaps SP");

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
#define N_TMPBUF 20000
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
   segment. */

static Bool translations_allowable_from_seg ( NSegment* seg )
{
#  if defined(VGA_x86)
   Bool allowR = True;
#  else
   Bool allowR = False;
#  endif
   return seg != NULL
          && (seg->kind == SkAnonC || seg->kind == SkFileC)
          && (seg->hasX || (seg->hasR && allowR));
}


/* Is a self-check required for a translation of a guest address
   inside segment SEG when requested by thread TID ? */

static Bool self_check_required ( NSegment* seg, ThreadId tid )
{
   switch (VG_(clo_smc_check)) {
      case Vg_SmcNone:  return False;
      case Vg_SmcAll:   return True;
      case Vg_SmcStack: 
         return seg 
                ? (seg->start <= VG_(get_SP)(tid)
                   && VG_(get_SP)(tid)+sizeof(Word)-1 <= seg->end)
                : False;
         break;
      default: 
         vg_assert2(0, "unknown VG_(clo_smc_check) value");
   }
}


/* This is a callback passed to LibVEX_Translate.  It stops Vex from
   chasing into function entry points that we wish to redirect.
   Chasing across them obviously defeats the redirect mechanism, with
   bad effects for Memcheck, Addrcheck, and possibly others.

   Also, we must stop Vex chasing into blocks for which we might want
   to self checking.
*/
static Bool chase_into_ok ( void* closureV, Addr64 addr64 )
{
   Addr               addr    = (Addr)addr64;
   NSegment*          seg     = VG_(am_find_nsegment)(addr);
   VgCallbackClosure* closure = (VgCallbackClosure*)closureV;

   /* Work through a list of possibilities why we might not want to
      allow a chase. */

   /* Destination not in a plausible segment? */
   if (!translations_allowable_from_seg(seg))
      goto dontchase;

   /* Destination requires a self-check? */
   if (self_check_required(seg, closure->tid))
      goto dontchase;

   /* Destination is redirected? */
   if (addr != VG_(redir_do_lookup)(addr, NULL))
      goto dontchase;

#  if defined(VGP_ppc64_linux)
   /* This needs to be at the start of its own block.  Don't chase. */
   if (addr64 == (Addr64)&VG_(ppc64_linux_magic_redirect_return_stub))
      goto dontchase;
#  endif

   /* well, ok then.  go on and chase. */
   return True;

   vg_assert(0);
   /*NOTREACHED*/

  dontchase:
   if (0) VG_(printf)("not chasing into 0x%x\n", addr);
   return False;
}


/* --------------- ppc64-linux specific helpers --------------- */

static IRExpr* mkU64 ( ULong n ) {
   return IRExpr_Const(IRConst_U64(n));
}
static IRExpr* mkU32 ( UInt n ) {
   return IRExpr_Const(IRConst_U32(n));
}
#if defined(VGP_ppc64_linux)
static IRExpr* mkU8 ( UChar n ) {
   return IRExpr_Const(IRConst_U8(n));
}

static void gen_PUSH ( IRBB* bb, IRExpr* e )
{
   Int stack_size       = VEX_GUEST_PPC64_REDIR_STACK_SIZE;
   Int offB_REDIR_SP    = offsetof(VexGuestPPC64State,guest_REDIR_SP);
   Int offB_REDIR_STACK = offsetof(VexGuestPPC64State,guest_REDIR_STACK);
   Int offB_EMWARN      = offsetof(VexGuestPPC64State,guest_EMWARN);

   IRArray* descr = mkIRArray( offB_REDIR_STACK, Ity_I64, stack_size );
   IRTemp   t1    = newIRTemp( bb->tyenv, Ity_I64 );
   IRExpr*  one   = mkU64(1);

   /* t1 = guest_REDIR_SP + 1 */
   addStmtToIRBB(
      bb, 
      IRStmt_Tmp(
         t1, 
         IRExpr_Binop(Iop_Add64, IRExpr_Get( offB_REDIR_SP, Ity_I64 ), one)
      )
   );

   /* Bomb out if t1 >=s stack_size, that is, (stack_size-1)-t1 <s 0.
      The destination (0) is a bit bogus but it doesn't matter since
      this is an unrecoverable error and will lead to Valgrind
      shutting down.  _EMWARN is set regardless - that's harmless
      since is only has a meaning if the exit is taken. */
   addStmtToIRBB(
      bb,
      IRStmt_Put(offB_EMWARN, mkU32(EmWarn_PPC64_redir_overflow))
   );
   addStmtToIRBB(
      bb,
      IRStmt_Exit(
         IRExpr_Binop(
            Iop_CmpNE64,
            IRExpr_Binop(
               Iop_Sar64,
               IRExpr_Binop(Iop_Sub64,mkU64(stack_size-1),IRExpr_Tmp(t1)),
               mkU8(63)
            ),
            mkU64(0)
         ),
         Ijk_EmFail,
         IRConst_U64(0)
      )
   );

   /* guest_REDIR_SP = t1 */
   addStmtToIRBB(bb, IRStmt_Put(offB_REDIR_SP, IRExpr_Tmp(t1)));

   /* guest_REDIR_STACK[t1+0] = e */
   addStmtToIRBB(
      bb, 
      IRStmt_PutI(descr, IRExpr_Unop(Iop_64to32,IRExpr_Tmp(t1)), 0, e)
   );
}

static IRTemp gen_POP ( IRBB* bb )
{
   Int stack_size       = VEX_GUEST_PPC64_REDIR_STACK_SIZE;
   Int offB_REDIR_SP    = offsetof(VexGuestPPC64State,guest_REDIR_SP);
   Int offB_REDIR_STACK = offsetof(VexGuestPPC64State,guest_REDIR_STACK);
   Int offB_EMWARN      = offsetof(VexGuestPPC64State,guest_EMWARN);

   IRArray* descr = mkIRArray( offB_REDIR_STACK, Ity_I64, stack_size );
   IRTemp   t1    = newIRTemp( bb->tyenv, Ity_I64 );
   IRTemp   res   = newIRTemp( bb->tyenv, Ity_I64 );
   IRExpr*  one   = mkU64(1);

   /* t1 = guest_REDIR_SP */
   addStmtToIRBB(
      bb, 
      IRStmt_Tmp( t1, IRExpr_Get( offB_REDIR_SP, Ity_I64 ) )
   );

   /* Bomb out if t1 < 0.  Same comments as gen_PUSH apply. */
   addStmtToIRBB(
      bb,
      IRStmt_Put(offB_EMWARN, mkU32(EmWarn_PPC64_redir_underflow))
   );
   addStmtToIRBB(
      bb,
      IRStmt_Exit(
         IRExpr_Binop(
            Iop_CmpNE64,
            IRExpr_Binop(
               Iop_Sar64,
               IRExpr_Tmp(t1),
               mkU8(63)
            ),
            mkU64(0)
         ),
         Ijk_EmFail,
         IRConst_U64(0)
      )
   );

   /* res = guest_REDIR_STACK[t1+0] */
   addStmtToIRBB(
      bb,
      IRStmt_Tmp(
         res, 
         IRExpr_GetI(descr, IRExpr_Unop(Iop_64to32,IRExpr_Tmp(t1)), 0)
      )
   );

   /* guest_REDIR_SP = t1-1 */
   addStmtToIRBB(
      bb, 
      IRStmt_Put(offB_REDIR_SP, IRExpr_Binop(Iop_Sub64, IRExpr_Tmp(t1), one))
   );

   return res;
}

static void gen_push_and_set_LR_R2 ( IRBB* bb, Addr64 new_R2_value )
{
   Addr64 bogus_RA = (Addr64)&VG_(ppc64_linux_magic_redirect_return_stub);
   Int offB_GPR2 = offsetof(VexGuestPPC64State,guest_GPR2);
   Int offB_LR   = offsetof(VexGuestPPC64State,guest_LR);
   gen_PUSH( bb, IRExpr_Get(offB_LR,   Ity_I64) );
   gen_PUSH( bb, IRExpr_Get(offB_GPR2, Ity_I64) );
   addStmtToIRBB( bb, IRStmt_Put( offB_LR,   mkU64( bogus_RA )) );
   addStmtToIRBB( bb, IRStmt_Put( offB_GPR2, mkU64( new_R2_value )) );
}

static void gen_pop_R2_LR_then_bLR ( IRBB* bb )
{
   Int offB_GPR2 = offsetof(VexGuestPPC64State,guest_GPR2);
   Int offB_LR   = offsetof(VexGuestPPC64State,guest_LR);
   IRTemp old_R2 = newIRTemp( bb->tyenv, Ity_I64 );
   IRTemp old_LR = newIRTemp( bb->tyenv, Ity_I64 );
   /* Restore R2 */
   old_R2 = gen_POP( bb );
   addStmtToIRBB( bb, IRStmt_Put( offB_GPR2, IRExpr_Tmp(old_R2)) );
   /* Restore LR */
   old_LR = gen_POP( bb );
   addStmtToIRBB( bb, IRStmt_Put( offB_LR, IRExpr_Tmp(old_LR)) );
   /* Branch to LR */
   /* re boring, we arrived here precisely because a wrapped fn did a
      blr (hence Ijk_Ret); so we should just mark this jump as Boring,
      else one _Call will have resulted in to _Rets. */
   bb->jumpkind = Ijk_Boring;
   bb->next = IRExpr_Binop(Iop_And64, IRExpr_Tmp(old_LR), mkU64(~(3ULL)));
}

static
Bool mk_preamble__ppc64_magic_return_stub ( void* closureV, IRBB* bb )
{
   VgCallbackClosure* closure = (VgCallbackClosure*)closureV;
   /* Since we're creating the entire IRBB right here, give it a
      proper IMark, as it won't get one any other way, and cachegrind
      will barf if it doesn't have one (fair enough really). */
   addStmtToIRBB( bb, IRStmt_IMark( closure->readdr, 4 ) );
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

/* --------------- END ppc64-linux specific helpers --------------- */

/* This is an the IR preamble generators used for replacement
   functions.  It adds code to set the guest_NRADDR{_GPR2} to zero
   (technically not necessary, but facilitates detecting mixups in
   which a replacement function has been erroneously declared using
   VG_REPLACE_FUNCTION_Z{U,Z} when instead it should have been written
   using VG_WRAP_FUNCTION_Z{U,Z}).

   On ppc64-linux the follow hacks are also done: LR and R2 are pushed
   onto a hidden stack, sets R2 to the correct value for the
   replacement function, and sets LR to point at the magic return-stub
   address.  Setting LR causes the return of the wrapped/redirected
   function to lead to our magic return stub, which restores LR and R2
   from said stack and returns for real.

   VG_(get_StackTrace2) understands that the LR value may point to the
   return stub address, and that in that case it can get the real LR
   value from the hidden stack instead. */
static 
Bool mk_preamble__set_NRADDR_to_zero ( void* closureV, IRBB* bb )
{
   Int nraddr_szB
      = sizeof(((VexGuestArchState*)0)->guest_NRADDR);
   vg_assert(nraddr_szB == 4 || nraddr_szB == 8);
   addStmtToIRBB( 
      bb,
      IRStmt_Put( 
         offsetof(VexGuestArchState,guest_NRADDR),
         nraddr_szB == 8 ? mkU64(0) : mkU32(0)
      )
   );
#  if defined(VGP_ppc64_linux)
   { VgCallbackClosure* closure = (VgCallbackClosure*)closureV;
     addStmtToIRBB(
        bb,
        IRStmt_Put(
           offsetof(VexGuestArchState,guest_NRADDR_GPR2),
           mkU64(0)
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
   wrapped. */
static 
Bool mk_preamble__set_NRADDR_to_nraddr ( void* closureV, IRBB* bb )
{
   VgCallbackClosure* closure = (VgCallbackClosure*)closureV;
   Int nraddr_szB
      = sizeof(((VexGuestArchState*)0)->guest_NRADDR);
   vg_assert(nraddr_szB == 4 || nraddr_szB == 8);
   addStmtToIRBB( 
      bb,
      IRStmt_Put( 
         offsetof(VexGuestArchState,guest_NRADDR),
         nraddr_szB == 8
            ? IRExpr_Const(IRConst_U64( closure->nraddr ))
            : IRExpr_Const(IRConst_U32( (UInt)closure->nraddr ))
      )
   );
#  if defined(VGP_ppc64_linux)
   addStmtToIRBB( 
      bb,
      IRStmt_Put( 
         offsetof(VexGuestArchState,guest_NRADDR_GPR2),
         IRExpr_Get(offsetof(VexGuestArchState,guest_GPR2), 
                    Ity_I64)
      )
   );
   gen_push_and_set_LR_R2 ( bb, VG_(get_tocptr)( closure->readdr ) );
#  endif
   return False;
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
   Bool               notrace_until_done, do_self_check;
   UInt               notrace_until_limit = 0;
   NSegment*          seg;
   Bool (*preamble_fn)(void*,IRBB*);
   VexArch            vex_arch;
   VexArchInfo        vex_archinfo;
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
      Char name1[64] = "";
      Char name2[64] = "";
      name1[0] = name2[0] = 0;
      ok = VG_(get_fnname_w_offset)(nraddr, name1, 64);
      if (!ok) VG_(strcpy)(name1, "???");
      ok = VG_(get_fnname_w_offset)(addr, name2, 64);
      if (!ok) VG_(strcpy)(name2, "???");
      VG_(message)(Vg_DebugMsg, 
                   "REDIR: 0x%llx (%s) redirected to 0x%llx (%s)",
                   nraddr, name1,
                   addr, name2 );
   }

   /* If codegen tracing, don't start tracing until
      notrace_until_limit blocks have gone by.  This avoids printing
      huge amounts of useless junk when all we want to see is the last
      few blocks translated prior to a failure.  Set
      notrace_until_limit to be the number of translations to be made
      before --trace-codegen= style printing takes effect. */
   notrace_until_done
      = VG_(get_bbs_translated)() >= notrace_until_limit;

   if (!debugging_translation)
      VG_TRACK( pre_mem_read, Vg_CoreTranslate, 
                              tid, "(translator)", addr, 1 );

   /* If doing any code printing, print a basic block start marker */
   if (VG_(clo_trace_flags) || debugging_translation) {
      Char fnname[64] = "";
      VG_(get_fnname_w_offset)(addr, fnname, 64);
      VG_(printf)(
              "==== BB %d %s(0x%llx) BBs exec'd %lld ====\n",
              VG_(get_bbs_translated)(), fnname, addr, 
              bbs_done);
   }

   /* Are we allowed to translate here? */

   seg = VG_(am_find_nsegment)(addr);

   if (!translations_allowable_from_seg(seg)) {
      /* U R busted, sonny.  Place your hands on your head and step
         away from the orig_addr. */
      /* Code address is bad - deliver a signal instead */
      if (seg != NULL) {
         /* There's some kind of segment at the requested place, but we
            aren't allowed to execute code here. */
         VG_(synth_fault_perms)(tid, addr);
      } else {
        /* There is no segment at all; we are attempting to execute in
           the middle of nowhere. */
         VG_(synth_fault_mapping)(tid, addr);
      }
      return False;
   }

   /* Do we want a self-checking translation? */
   do_self_check = self_check_required( seg, tid );

   /* True if a debug trans., or if bit N set in VG_(clo_trace_codegen). */
   verbosity = 0;
   if (debugging_translation) {
      verbosity = debugging_verbosity;
   }
   else
   if ( (VG_(clo_trace_flags) > 0
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
#  if defined(VGP_ppc64_linux)
   if (nraddr == (Addr64)&VG_(ppc64_linux_magic_redirect_return_stub)) {
      /* If entering the special return stub, this means a wrapped or
         redirected function is returning.  Make this translation one
         which restores R2 and LR from the thread's hidden redir
         stack, and branch to the (restored) link register, thereby
         really causing the function to return. */
      vg_assert(kind == T_Normal);
      vg_assert(nraddr == addr);
      preamble_fn = mk_preamble__ppc64_magic_return_stub;
    }
#  endif

   /* ------ Actually do the translation. ------ */
   tl_assert2(VG_(tdict).tool_instrument,
              "you forgot to set VgToolInterface function 'tool_instrument'");

   /* Get the CPU info established at startup. */
   VG_(machine_get_VexArchInfo)( &vex_arch, &vex_archinfo );

   /* Set up closure args. */
   closure.tid    = tid;
   closure.nraddr = nraddr;
   closure.readdr = addr;

   /* Set up args for LibVEX_Translate. */
   vta.arch_guest       = vex_arch;
   vta.archinfo_guest   = vex_archinfo;
   vta.arch_host        = vex_arch;
   vta.archinfo_host    = vex_archinfo;
   vta.guest_bytes      = (UChar*)ULong_to_Ptr(addr);
   vta.guest_bytes_addr = (Addr64)addr;
   vta.callback_opaque  = (void*)&closure;
   vta.chase_into_ok    = chase_into_ok;
   vta.preamble_function = preamble_fn;
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
     IRBB*(*f)(VgCallbackClosure*,
               IRBB*,VexGuestLayout*,VexGuestExtents*,
               IRType,IRType)
       = VG_(tdict).tool_instrument;
     IRBB*(*g)(void*,
               IRBB*,VexGuestLayout*,VexGuestExtents*,
               IRType,IRType)
       = (IRBB*(*)(void*,IRBB*,VexGuestLayout*,VexGuestExtents*,IRType,IRType))f;
     vta.instrument1    = g;
   }
   /* No need for type kludgery here. */
   vta.instrument2      = need_to_handle_SP_assignment()
                             ? vg_SP_update_pass
                             : NULL;
   vta.do_self_check    = do_self_check;
   vta.traceflags       = verbosity;

   /* Set up the dispatch-return info.  For archs without a link
      register, vex generates a jump back to the specified dispatch
      address.  Else, it just generates a branch-to-LR. */
#  if defined(VGA_x86) || defined(VGA_amd64)
   vta.dispatch 
      = (!allow_redirection)
        ? /* It's a no-redir translation.  Will be run with the nonstandard
           dispatcher VG_(run_a_noredir_translation)
           and so needs a nonstandard return point. */
          (void*) &VG_(run_a_noredir_translation__return_point)

        : /* normal translation.  Uses VG_(run_innerloop).  Return
             point depends on whether we're profiling bbs or not. */
          VG_(clo_profile_flags) > 0
          ? (void*) &VG_(run_innerloop__dispatch_profiled)
          : (void*) &VG_(run_innerloop__dispatch_unprofiled);
#  elif defined(VGA_ppc32) || defined(VGA_ppc64)
   vta.dispatch = NULL;
#  else
#    error "Unknown arch"
#  endif

   /* Sheesh.  Finally, actually _do_ the translation! */
   tres = LibVEX_Translate ( &vta );

   vg_assert(tres == VexTransOK);
   vg_assert(tmpbuf_used <= N_TMPBUF);
   vg_assert(tmpbuf_used > 0);

   /* Tell aspacem of all segments that have had translations taken
      from them.  Optimisation: don't re-look up vge.base[0] since seg
      should already point to it. */

   vg_assert( vge.base[0] == (Addr64)addr );
   if (seg->kind == SkFileC || seg->kind == SkAnonC)
      seg->hasT = True; /* has cached code */

   for (i = 1; i < vge.n_used; i++) {
      seg = VG_(am_find_nsegment)( vge.base[i] );
      if (seg->kind == SkFileC || seg->kind == SkAnonC)
         seg->hasT = True; /* has cached code */
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
                                do_self_check );
      } else {
          VG_(add_to_unredir_transtab)( &vge,
                                        nraddr,
                                        (Addr)(&tmpbuf[0]), 
                                        tmpbuf_used,
                                        do_self_check );
      }
   }

   return True;
}

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
