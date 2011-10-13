/* -*- mode: C; c-basic-offset: 3; indent-tabs-mode: nil; -*- */
/*
  This file is part of drd, a thread error detector.

  Copyright (C) 2006-2011 Bart Van Assche <bvanassche@acm.org>.

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


#include "drd_bitmap.h"
#include "drd_thread_bitmap.h"
#include "drd_vc.h"            /* DRD_(vc_snprint)() */

/* Include several source files here in order to allow the compiler to */
/* do more inlining.                                                   */
#include "drd_bitmap.c"
#include "drd_load_store.h"
#include "drd_segment.c"
#include "drd_thread.c"
#include "drd_vc.c"
#include "libvex_guest_offsets.h"


/* STACK_POINTER_OFFSET: VEX register offset for the stack pointer register. */
#if defined(VGA_x86)
#define STACK_POINTER_OFFSET OFFSET_x86_ESP
#elif defined(VGA_amd64)
#define STACK_POINTER_OFFSET OFFSET_amd64_RSP
#elif defined(VGA_ppc32)
#define STACK_POINTER_OFFSET OFFSET_ppc32_GPR1
#elif defined(VGA_ppc64)
#define STACK_POINTER_OFFSET OFFSET_ppc64_GPR1
#elif defined(VGA_arm)
#define STACK_POINTER_OFFSET OFFSET_arm_R13
#elif defined(VGA_s390x)
#define STACK_POINTER_OFFSET OFFSET_s390x_r15
#else
#error Unknown architecture.
#endif


/* Local variables. */

static Bool s_check_stack_accesses = False;
static Bool s_first_race_only      = False;


/* Function definitions. */

Bool DRD_(get_check_stack_accesses)()
{
   return s_check_stack_accesses;
}

void DRD_(set_check_stack_accesses)(const Bool c)
{
   tl_assert(c == False || c == True);
   s_check_stack_accesses = c;
}

Bool DRD_(get_first_race_only)()
{
   return s_first_race_only;
}

void DRD_(set_first_race_only)(const Bool fro)
{
   tl_assert(fro == False || fro == True);
   s_first_race_only = fro;
}

void DRD_(trace_mem_access)(const Addr addr, const SizeT size,
                            const BmAccessTypeT access_type)
{
   if (DRD_(is_any_traced)(addr, addr + size))
   {
      char* vc;

      vc = DRD_(vc_aprint)(DRD_(thread_get_vc)(DRD_(thread_get_running_tid)()));
      DRD_(trace_msg_w_bt)("%s 0x%lx size %ld (thread %d / vc %s)",
                           access_type == eLoad ? "load "
                           : access_type == eStore ? "store"
                           : access_type == eStart ? "start"
                           : access_type == eEnd ? "end  " : "????",
                           addr, size, DRD_(thread_get_running_tid)(), vc);
      VG_(free)(vc);
      tl_assert(DRD_(DrdThreadIdToVgThreadId)(DRD_(thread_get_running_tid)())
                == VG_(get_running_tid)());
   }
}

static VG_REGPARM(2) void drd_trace_mem_load(const Addr addr, const SizeT size)
{
   return DRD_(trace_mem_access)(addr, size, eLoad);
}

static VG_REGPARM(2) void drd_trace_mem_store(const Addr addr,const SizeT size)
{
   return DRD_(trace_mem_access)(addr, size, eStore);
}

static void drd_report_race(const Addr addr, const SizeT size,
                            const BmAccessTypeT access_type)
{
   DataRaceErrInfo drei;

   drei.tid  = DRD_(thread_get_running_tid)();
   drei.addr = addr;
   drei.size = size;
   drei.access_type = access_type;
   VG_(maybe_record_error)(VG_(get_running_tid)(),
                           DataRaceErr,
                           VG_(get_IP)(VG_(get_running_tid)()),
                           "Conflicting access",
                           &drei);

   if (s_first_race_only)
   {
      DRD_(start_suppression)(addr, addr + size, "first race only");
   }
}

VG_REGPARM(2) void DRD_(trace_load)(Addr addr, SizeT size)
{
#ifdef ENABLE_DRD_CONSISTENCY_CHECKS
   /* The assert below has been commented out because of performance reasons.*/
   tl_assert(DRD_(thread_get_running_tid)()
             == DRD_(VgThreadIdToDrdThreadId)(VG_(get_running_tid())));
#endif

   if (DRD_(running_thread_is_recording_loads)()
       && (s_check_stack_accesses
           || ! DRD_(thread_address_on_stack)(addr))
       && bm_access_load_triggers_conflict(addr, addr + size)
       && ! DRD_(is_suppressed)(addr, addr + size))
   {
      drd_report_race(addr, size, eLoad);
   }
}

static VG_REGPARM(1) void drd_trace_load_1(Addr addr)
{
   if (DRD_(running_thread_is_recording_loads)()
       && (s_check_stack_accesses
           || ! DRD_(thread_address_on_stack)(addr))
       && bm_access_load_1_triggers_conflict(addr)
       && ! DRD_(is_suppressed)(addr, addr + 1))
   {
      drd_report_race(addr, 1, eLoad);
   }
}

static VG_REGPARM(1) void drd_trace_load_2(Addr addr)
{
   if (DRD_(running_thread_is_recording_loads)()
       && (s_check_stack_accesses
           || ! DRD_(thread_address_on_stack)(addr))
       && bm_access_load_2_triggers_conflict(addr)
       && ! DRD_(is_suppressed)(addr, addr + 2))
   {
      drd_report_race(addr, 2, eLoad);
   }
}

static VG_REGPARM(1) void drd_trace_load_4(Addr addr)
{
   if (DRD_(running_thread_is_recording_loads)()
       && (s_check_stack_accesses
           || ! DRD_(thread_address_on_stack)(addr))
       && bm_access_load_4_triggers_conflict(addr)
       && ! DRD_(is_suppressed)(addr, addr + 4))
   {
      drd_report_race(addr, 4, eLoad);
   }
}

static VG_REGPARM(1) void drd_trace_load_8(Addr addr)
{
   if (DRD_(running_thread_is_recording_loads)()
       && (s_check_stack_accesses
           || ! DRD_(thread_address_on_stack)(addr))
       && bm_access_load_8_triggers_conflict(addr)
       && ! DRD_(is_suppressed)(addr, addr + 8))
   {
      drd_report_race(addr, 8, eLoad);
   }
}

VG_REGPARM(2) void DRD_(trace_store)(Addr addr, SizeT size)
{
#ifdef ENABLE_DRD_CONSISTENCY_CHECKS
   /* The assert below has been commented out because of performance reasons.*/
   tl_assert(DRD_(thread_get_running_tid)()
             == DRD_(VgThreadIdToDrdThreadId)(VG_(get_running_tid())));
#endif

   if (DRD_(running_thread_is_recording_stores)()
       && (s_check_stack_accesses
           || ! DRD_(thread_address_on_stack)(addr))
       && bm_access_store_triggers_conflict(addr, addr + size)
       && ! DRD_(is_suppressed)(addr, addr + size))
   {
      drd_report_race(addr, size, eStore);
   }
}

static VG_REGPARM(1) void drd_trace_store_1(Addr addr)
{
   if (DRD_(running_thread_is_recording_stores)()
       && (s_check_stack_accesses
           || ! DRD_(thread_address_on_stack)(addr))
       && bm_access_store_1_triggers_conflict(addr)
       && ! DRD_(is_suppressed)(addr, addr + 1))
   {
      drd_report_race(addr, 1, eStore);
   }
}

static VG_REGPARM(1) void drd_trace_store_2(Addr addr)
{
   if (DRD_(running_thread_is_recording_stores)()
       && (s_check_stack_accesses
           || ! DRD_(thread_address_on_stack)(addr))
       && bm_access_store_2_triggers_conflict(addr)
       && ! DRD_(is_suppressed)(addr, addr + 2))
   {
      drd_report_race(addr, 2, eStore);
   }
}

static VG_REGPARM(1) void drd_trace_store_4(Addr addr)
{
   if (DRD_(running_thread_is_recording_stores)()
       && (s_check_stack_accesses
           || ! DRD_(thread_address_on_stack)(addr))
       && bm_access_store_4_triggers_conflict(addr)
       && ! DRD_(is_suppressed)(addr, addr + 4))
   {
      drd_report_race(addr, 4, eStore);
   }
}

static VG_REGPARM(1) void drd_trace_store_8(Addr addr)
{
   if (DRD_(running_thread_is_recording_stores)()
       && (s_check_stack_accesses
           || ! DRD_(thread_address_on_stack)(addr))
       && bm_access_store_8_triggers_conflict(addr)
       && ! DRD_(is_suppressed)(addr, addr + 8))
   {
      drd_report_race(addr, 8, eStore);
   }
}

/**
 * Return true if and only if addr_expr matches the pattern (SP) or
 * <offset>(SP).
 */
static Bool is_stack_access(IRSB* const bb, IRExpr* const addr_expr)
{
   Bool result = False;

   if (addr_expr->tag == Iex_RdTmp)
   {
      int i;
      for (i = 0; i < bb->stmts_size; i++)
      {
         if (bb->stmts[i]
             && bb->stmts[i]->tag == Ist_WrTmp
             && bb->stmts[i]->Ist.WrTmp.tmp == addr_expr->Iex.RdTmp.tmp)
         {
            IRExpr* e = bb->stmts[i]->Ist.WrTmp.data;
            if (e->tag == Iex_Get && e->Iex.Get.offset == STACK_POINTER_OFFSET)
            {
               result = True;
            }

            //ppIRExpr(e);
            //VG_(printf)(" (%s)\n", result ? "True" : "False");
            break;
         }
      }
   }
   return result;
}

static void instrument_load(IRSB* const bb,
                            IRExpr* const addr_expr,
                            const HWord size)
{
   IRExpr* size_expr;
   IRExpr** argv;
   IRDirty* di;

   if (UNLIKELY(DRD_(any_address_is_traced)()))
   {
      addStmtToIRSB(bb,
         IRStmt_Dirty(
            unsafeIRDirty_0_N(/*regparms*/2,
                              "drd_trace_load",
                              VG_(fnptr_to_fnentry)
                              (drd_trace_mem_load),
                              mkIRExprVec_2(addr_expr,
                                            mkIRExpr_HWord(size)))));
   }

   if (! s_check_stack_accesses && is_stack_access(bb, addr_expr))
      return;

   switch (size)
   {
   case 1:
      argv = mkIRExprVec_1(addr_expr);
      di = unsafeIRDirty_0_N(/*regparms*/1,
                             "drd_trace_load_1",
                             VG_(fnptr_to_fnentry)(drd_trace_load_1),
                             argv);
      break;
   case 2:
      argv = mkIRExprVec_1(addr_expr);
      di = unsafeIRDirty_0_N(/*regparms*/1,
                             "drd_trace_load_2",
                             VG_(fnptr_to_fnentry)(drd_trace_load_2),
                             argv);
      break;
   case 4:
      argv = mkIRExprVec_1(addr_expr);
      di = unsafeIRDirty_0_N(/*regparms*/1,
                             "drd_trace_load_4",
                             VG_(fnptr_to_fnentry)(drd_trace_load_4),
                             argv);
      break;
   case 8:
      argv = mkIRExprVec_1(addr_expr);
      di = unsafeIRDirty_0_N(/*regparms*/1,
                             "drd_trace_load_8",
                             VG_(fnptr_to_fnentry)(drd_trace_load_8),
                             argv);
      break;
   default:
      size_expr = mkIRExpr_HWord(size);
      argv = mkIRExprVec_2(addr_expr, size_expr);
      di = unsafeIRDirty_0_N(/*regparms*/2,
                             "drd_trace_load",
                             VG_(fnptr_to_fnentry)(DRD_(trace_load)),
                             argv);
      break;
   }
   addStmtToIRSB(bb, IRStmt_Dirty(di));
}

static void instrument_store(IRSB* const bb,
                             IRExpr* const addr_expr,
                             const HWord size)
{
   IRExpr* size_expr;
   IRExpr** argv;
   IRDirty* di;

   if (UNLIKELY(DRD_(any_address_is_traced)()))
   {
      addStmtToIRSB(bb,
                    IRStmt_Dirty(
                                 unsafeIRDirty_0_N(/*regparms*/2,
                                                   "drd_trace_store",
                                                   VG_(fnptr_to_fnentry)
                                                   (drd_trace_mem_store),
                                                   mkIRExprVec_2(addr_expr,
                                                                 mkIRExpr_HWord(size)))));
   }

   if (! s_check_stack_accesses && is_stack_access(bb, addr_expr))
      return;

   switch (size)
   {
   case 1:
      argv = mkIRExprVec_1(addr_expr);
      di = unsafeIRDirty_0_N(/*regparms*/1,
                             "drd_trace_store_1",
                             VG_(fnptr_to_fnentry)(drd_trace_store_1),
                             argv);
      break;
   case 2:
      argv = mkIRExprVec_1(addr_expr);
      di = unsafeIRDirty_0_N(/*regparms*/1,
                             "drd_trace_store_2",
                             VG_(fnptr_to_fnentry)(drd_trace_store_2),
                             argv);
      break;
   case 4:
      argv = mkIRExprVec_1(addr_expr);
      di = unsafeIRDirty_0_N(/*regparms*/1,
                             "drd_trace_store_4",
                             VG_(fnptr_to_fnentry)(drd_trace_store_4),
                             argv);
      break;
   case 8:
      argv = mkIRExprVec_1(addr_expr);
      di = unsafeIRDirty_0_N(/*regparms*/1,
                             "drd_trace_store_8",
                             VG_(fnptr_to_fnentry)(drd_trace_store_8),
                             argv);
      break;
   default:
      size_expr = mkIRExpr_HWord(size);
      argv = mkIRExprVec_2(addr_expr, size_expr);
      di = unsafeIRDirty_0_N(/*regparms*/2,
                             "drd_trace_store",
                             VG_(fnptr_to_fnentry)(DRD_(trace_store)),
                             argv);
      break;
   }
   addStmtToIRSB(bb, IRStmt_Dirty(di));
}

IRSB* DRD_(instrument)(VgCallbackClosure* const closure,
                       IRSB* const bb_in,
                       VexGuestLayout* const layout,
                       VexGuestExtents* const vge,
                       IRType const gWordTy,
                       IRType const hWordTy)
{
   IRDirty* di;
   Int      i;
   IRSB*    bb;
   IRExpr** argv;
   Bool     instrument = True;

   /* Set up BB */
   bb           = emptyIRSB();
   bb->tyenv    = deepCopyIRTypeEnv(bb_in->tyenv);
   bb->next     = deepCopyIRExpr(bb_in->next);
   bb->jumpkind = bb_in->jumpkind;

   for (i = 0; i < bb_in->stmts_used; i++)
   {
      IRStmt* const st = bb_in->stmts[i];
      tl_assert(st);
      tl_assert(isFlatIRStmt(st));

      switch (st->tag)
      {
         /* Note: the code for not instrumenting the code in .plt          */
         /* sections is only necessary on CentOS 3.0 x86 (kernel 2.4.21    */
         /* + glibc 2.3.2 + NPTL 0.60 + binutils 2.14.90.0.4).             */
         /* This is because on this platform dynamic library symbols are   */
         /* relocated in another way than by later binutils versions. The  */
         /* linker e.g. does not generate .got.plt sections on CentOS 3.0. */
      case Ist_IMark:
         instrument = VG_(DebugInfo_sect_kind)(NULL, 0, st->Ist.IMark.addr)
            != Vg_SectPLT;
         addStmtToIRSB(bb, st);
         break;

      case Ist_MBE:
         switch (st->Ist.MBE.event)
         {
         case Imbe_Fence:
            break; /* not interesting */
         default:
            tl_assert(0);
         }
         addStmtToIRSB(bb, st);
         break;

      case Ist_Store:
         if (instrument)
         {
            instrument_store(bb,
                             st->Ist.Store.addr,
                             sizeofIRType(typeOfIRExpr(bb->tyenv,
                                                       st->Ist.Store.data)));
         }
         addStmtToIRSB(bb, st);
         break;

      case Ist_WrTmp:
         if (instrument)
         {
            const IRExpr* const data = st->Ist.WrTmp.data;
            if (data->tag == Iex_Load)
            {
               instrument_load(bb,
                               data->Iex.Load.addr,
                               sizeofIRType(data->Iex.Load.ty));
            }
         }
         addStmtToIRSB(bb, st);
         break;

      case Ist_Dirty:
         if (instrument)
         {
            IRDirty* d = st->Ist.Dirty.details;
            IREffect const mFx = d->mFx;
            switch (mFx) {
            case Ifx_None:
               break;
            case Ifx_Read:
            case Ifx_Write:
            case Ifx_Modify:
               tl_assert(d->mAddr);
               tl_assert(d->mSize > 0);
               argv = mkIRExprVec_2(d->mAddr, mkIRExpr_HWord(d->mSize));
               if (mFx == Ifx_Read || mFx == Ifx_Modify) {
                  di = unsafeIRDirty_0_N(
                          /*regparms*/2,
                          "drd_trace_load",
                          VG_(fnptr_to_fnentry)(DRD_(trace_load)),
                          argv);
                  addStmtToIRSB(bb, IRStmt_Dirty(di));
               }
               if (mFx == Ifx_Write || mFx == Ifx_Modify)
               {
                  di = unsafeIRDirty_0_N(
                          /*regparms*/2,
                          "drd_trace_store",
                          VG_(fnptr_to_fnentry)(DRD_(trace_store)),
                          argv);
                  addStmtToIRSB(bb, IRStmt_Dirty(di));
               }
               break;
            default:
               tl_assert(0);
            }
         }
         addStmtToIRSB(bb, st);
         break;

      case Ist_CAS:
         if (instrument)
         {
            /*
             * Treat compare-and-swap as a read. By handling atomic
             * instructions as read instructions no data races are reported
             * between conflicting atomic operations nor between atomic
             * operations and non-atomic reads. Conflicts between atomic
             * operations and non-atomic write operations are still reported
             * however.
             */
            Int    dataSize;
            IRCAS* cas = st->Ist.CAS.details;
            tl_assert(cas->addr != NULL);
            tl_assert(cas->dataLo != NULL);
            dataSize = sizeofIRType(typeOfIRExpr(bb->tyenv, cas->dataLo));
            if (cas->dataHi != NULL)
               dataSize *= 2; /* since it's a doubleword-CAS */
            instrument_load(bb, cas->addr, dataSize);
         }
         addStmtToIRSB(bb, st);
         break;

      case Ist_LLSC: {
         /* Ignore store-conditionals, and handle load-linked's
            exactly like normal loads. */
         IRType dataTy;
         if (st->Ist.LLSC.storedata == NULL)
         {
            /* LL */
            dataTy = typeOfIRTemp(bb_in->tyenv, st->Ist.LLSC.result);
            if (instrument) {
               instrument_load(bb,
                               st->Ist.LLSC.addr,
                               sizeofIRType(dataTy));
            }
         }
         else
         {
            /* SC */
            /*ignore */
         }
         addStmtToIRSB(bb, st);
         break;
      }

      case Ist_NoOp:
      case Ist_AbiHint:
      case Ist_Put:
      case Ist_PutI:
      case Ist_Exit:
         /* None of these can contain any memory references. */
         addStmtToIRSB(bb, st);
         break;

      default:
         ppIRStmt(st);
         tl_assert(0);
      }
   }

   return bb;
}

