/*
  This file is part of drd, a thread error detector.

  Copyright (C) 2006-2020 Bart Van Assche <bvanassche@acm.org>.

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
#elif defined(VGA_ppc64be) || defined(VGA_ppc64le)
#define STACK_POINTER_OFFSET OFFSET_ppc64_GPR1
#elif defined(VGA_arm)
#define STACK_POINTER_OFFSET OFFSET_arm_R13
#elif defined(VGA_arm64)
#define STACK_POINTER_OFFSET OFFSET_arm64_XSP
#elif defined(VGA_s390x)
#define STACK_POINTER_OFFSET OFFSET_s390x_r15
#elif defined(VGA_mips32) || defined(VGA_nanomips)
#define STACK_POINTER_OFFSET OFFSET_mips32_r29
#elif defined(VGA_mips64)
#define STACK_POINTER_OFFSET OFFSET_mips64_r29
#else
#error Unknown architecture.
#endif


/* Local variables. */

static Bool s_check_stack_accesses = False;
static Bool s_first_race_only      = False;


/* Function definitions. */

Bool DRD_(get_check_stack_accesses)(void)
{
   return s_check_stack_accesses;
}

void DRD_(set_check_stack_accesses)(const Bool c)
{
   tl_assert(c == False || c == True);
   s_check_stack_accesses = c;
}

Bool DRD_(get_first_race_only)(void)
{
   return s_first_race_only;
}

void DRD_(set_first_race_only)(const Bool fro)
{
   tl_assert(fro == False || fro == True);
   s_first_race_only = fro;
}

void DRD_(trace_mem_access)(const Addr addr, const SizeT size,
                            const BmAccessTypeT access_type,
                            const HWord stored_value_hi,
                            const HWord stored_value_lo)
{
   if (DRD_(is_any_traced)(addr, addr + size))
   {
      HChar* vc;

      vc = DRD_(vc_aprint)(DRD_(thread_get_vc)(DRD_(thread_get_running_tid)()));
      if (access_type == eStore && size <= sizeof(HWord)) {
         DRD_(trace_msg_w_bt)("store 0x%lx size %lu val %lu/0x%lx (thread %u /"
                              " vc %s)", addr, size, stored_value_lo,
                              stored_value_lo, DRD_(thread_get_running_tid)(),
                              vc);
      } else if (access_type == eStore && size > sizeof(HWord)) {
         ULong sv;

         tl_assert(sizeof(HWord) == 4);
         sv = ((ULong)stored_value_hi << 32) | stored_value_lo;
         DRD_(trace_msg_w_bt)("store 0x%lx size %lu val %llu/0x%llx (thread %u"
                              " / vc %s)", addr, size, sv, sv,
                              DRD_(thread_get_running_tid)(), vc);
      } else {
         DRD_(trace_msg_w_bt)("%s 0x%lx size %lu (thread %u / vc %s)",
                              access_type == eLoad ? "load "
                              : access_type == eStore ? "store"
                              : access_type == eStart ? "start"
                              : access_type == eEnd ? "end  " : "????",
                              addr, size, DRD_(thread_get_running_tid)(), vc);
      }
      VG_(free)(vc);
      tl_assert(DRD_(DrdThreadIdToVgThreadId)(DRD_(thread_get_running_tid)())
                == VG_(get_running_tid)());
   }
}

static VG_REGPARM(2) void drd_trace_mem_load(const Addr addr, const SizeT size)
{
   return DRD_(trace_mem_access)(addr, size, eLoad, 0, 0);
}

static VG_REGPARM(3) void drd_trace_mem_store(const Addr addr,const SizeT size,
                                              const HWord stored_value_hi,
                                              const HWord stored_value_lo)
{
   return DRD_(trace_mem_access)(addr, size, eStore, stored_value_hi,
                                 stored_value_lo);
}

static void drd_report_race(const Addr addr, const SizeT size,
                            const BmAccessTypeT access_type)
{
   ThreadId vg_tid;

   vg_tid = VG_(get_running_tid)();
   if (!DRD_(get_check_stack_accesses)()
       && DRD_(thread_address_on_any_stack)(addr)) {
#if 0
      GenericErrInfo GEI = {
         .tid = DRD_(thread_get_running_tid)(),
         .addr = addr,
      };
      VG_(maybe_record_error)(vg_tid, GenericErr, VG_(get_IP)(vg_tid),
                              "--check-stack-var=no skips checking stack"
                              " variables shared over threads",
                              &GEI);
#endif
  } else {
      DataRaceErrInfo drei = {
         .tid  = DRD_(thread_get_running_tid)(),
         .addr = addr,
         .size = size,
         .access_type = access_type,
      };
      VG_(maybe_record_error)(vg_tid, DataRaceErr, VG_(get_IP)(vg_tid),
                              "Conflicting access", &drei);

      if (s_first_race_only)
         DRD_(start_suppression)(addr, addr + size, "first race only");
   }
}

VG_REGPARM(2) void DRD_(trace_load)(Addr addr, SizeT size)
{
#ifdef ENABLE_DRD_CONSISTENCY_CHECKS
   /* The assert below has been commented out because of performance reasons.*/
   tl_assert(DRD_(thread_get_running_tid)()
             == DRD_(VgThreadIdToDrdThreadId)(VG_(get_running_tid)()));
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
             == DRD_(VgThreadIdToDrdThreadId)(VG_(get_running_tid)()));
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
           || !DRD_(thread_address_on_stack)(addr))
       && bm_access_store_4_triggers_conflict(addr)
       && !DRD_(is_suppressed)(addr, addr + 4))
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
      for (i = 0; i < bb->stmts_used; i++)
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

static const IROp u_widen_irop[5][9] = {
   [Ity_I1  - Ity_I1] = { [4] = Iop_1Uto32,  [8] = Iop_1Uto64 },
   [Ity_I8  - Ity_I1] = { [4] = Iop_8Uto32,  [8] = Iop_8Uto64 },
   [Ity_I16 - Ity_I1] = { [4] = Iop_16Uto32, [8] = Iop_16Uto64 },
   [Ity_I32 - Ity_I1] = {                    [8] = Iop_32Uto64 },
};

/**
 * Instrument the client code to trace a memory load (--trace-addr).
 */
static IRExpr* instr_trace_mem_load(IRSB* const bb, IRExpr* addr_expr,
                                    const HWord size,
                                    IRExpr* const guard/* NULL => True */)
{
   IRTemp tmp;

   tmp = newIRTemp(bb->tyenv, typeOfIRExpr(bb->tyenv, addr_expr));
   addStmtToIRSB(bb, IRStmt_WrTmp(tmp, addr_expr));
   addr_expr = IRExpr_RdTmp(tmp);
   IRDirty* di
     = unsafeIRDirty_0_N(/*regparms*/2,
                         "drd_trace_mem_load",
                         VG_(fnptr_to_fnentry)
                         (drd_trace_mem_load),
                         mkIRExprVec_2(addr_expr, mkIRExpr_HWord(size)));
   if (guard) di->guard = guard;
   addStmtToIRSB(bb, IRStmt_Dirty(di));

   return addr_expr;
}

/**
 * Instrument the client code to trace a memory store (--trace-addr).
 */
static void instr_trace_mem_store(IRSB* const bb, IRExpr* const addr_expr,
                                  IRExpr* data_expr_hi, IRExpr* data_expr_lo,
                                  IRExpr* const guard/* NULL => True */)
{
   IRType ty_data_expr;
   HWord size;

   tl_assert(sizeof(HWord) == 4 || sizeof(HWord) == 8);
   tl_assert(!data_expr_hi || typeOfIRExpr(bb->tyenv, data_expr_hi) == Ity_I32);

   ty_data_expr = typeOfIRExpr(bb->tyenv, data_expr_lo);
   size = sizeofIRType(ty_data_expr);

#if 0
   // Test code
   if (ty_data_expr == Ity_I32) {
      IRTemp tmp = newIRTemp(bb->tyenv, Ity_F32);
      data_expr_lo = IRExpr_Unop(Iop_ReinterpI32asF32, data_expr_lo);
      addStmtToIRSB(bb, IRStmt_WrTmp(tmp, data_expr_lo));
      data_expr_lo = IRExpr_RdTmp(tmp);
      ty_data_expr = Ity_F32;
   } else if (ty_data_expr == Ity_I64) {
      IRTemp tmp = newIRTemp(bb->tyenv, Ity_F64);
      data_expr_lo = IRExpr_Unop(Iop_ReinterpI64asF64, data_expr_lo);
      addStmtToIRSB(bb, IRStmt_WrTmp(tmp, data_expr_lo));
      data_expr_lo = IRExpr_RdTmp(tmp);
      ty_data_expr = Ity_F64;
   }
#endif

   if (ty_data_expr == Ity_F32) {
      IRTemp tmp = newIRTemp(bb->tyenv, Ity_I32);
      addStmtToIRSB(bb, IRStmt_WrTmp(tmp, IRExpr_Unop(Iop_ReinterpF32asI32,
                                                      data_expr_lo)));
      data_expr_lo = IRExpr_RdTmp(tmp);
      ty_data_expr = Ity_I32;
   } else if (ty_data_expr == Ity_F64) {
      IRTemp tmp = newIRTemp(bb->tyenv, Ity_I64);
      addStmtToIRSB(bb, IRStmt_WrTmp(tmp, IRExpr_Unop(Iop_ReinterpF64asI64,
                                                      data_expr_lo)));
      data_expr_lo = IRExpr_RdTmp(tmp);
      ty_data_expr = Ity_I64;
   }

   if (size == sizeof(HWord)
       && (ty_data_expr == Ity_I32 || ty_data_expr == Ity_I64))
   {
      /* No conversion necessary */
   } else {
      IROp widen_op;

      if (Ity_I1 <= ty_data_expr
          && ty_data_expr
             < Ity_I1 + sizeof(u_widen_irop)/sizeof(u_widen_irop[0]))
      {
         widen_op = u_widen_irop[ty_data_expr - Ity_I1][sizeof(HWord)];
         if (!widen_op)
            widen_op = Iop_INVALID;
      } else {
         widen_op = Iop_INVALID;
      }
      if (widen_op != Iop_INVALID) {
         IRTemp tmp;

         /* Widen the integer expression to a HWord */
         tmp = newIRTemp(bb->tyenv, sizeof(HWord) == 4 ? Ity_I32 : Ity_I64);
         addStmtToIRSB(bb,
                       IRStmt_WrTmp(tmp, IRExpr_Unop(widen_op, data_expr_lo)));
         data_expr_lo = IRExpr_RdTmp(tmp);
      } else if (size > sizeof(HWord) && !data_expr_hi
                 && ty_data_expr == Ity_I64) {
         IRTemp tmp;
         
         tl_assert(sizeof(HWord) == 4);
         tl_assert(size == 8);
         tmp = newIRTemp(bb->tyenv, Ity_I32);
         addStmtToIRSB(bb,
                       IRStmt_WrTmp(tmp,
                                    IRExpr_Unop(Iop_64HIto32, data_expr_lo)));
         data_expr_hi = IRExpr_RdTmp(tmp);
         tmp = newIRTemp(bb->tyenv, Ity_I32);
         addStmtToIRSB(bb, IRStmt_WrTmp(tmp,
                                        IRExpr_Unop(Iop_64to32, data_expr_lo)));
         data_expr_lo = IRExpr_RdTmp(tmp);
      } else {
         data_expr_lo = mkIRExpr_HWord(0);
      }
   }
   IRDirty* di
     = unsafeIRDirty_0_N(/*regparms*/3,
                         "drd_trace_mem_store",
                         VG_(fnptr_to_fnentry)(drd_trace_mem_store),
                         mkIRExprVec_4(addr_expr, mkIRExpr_HWord(size),
                                       data_expr_hi ? data_expr_hi
                                       : mkIRExpr_HWord(0), data_expr_lo));
   if (guard) di->guard = guard;
   addStmtToIRSB(bb, IRStmt_Dirty(di) );
}

static void instrument_load(IRSB* const bb, IRExpr* const addr_expr,
                            const HWord size,
                            IRExpr* const guard/* NULL => True */)
{
   IRExpr* size_expr;
   IRExpr** argv;
   IRDirty* di;

   if (!s_check_stack_accesses && is_stack_access(bb, addr_expr))
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
   if (guard) di->guard = guard;
   addStmtToIRSB(bb, IRStmt_Dirty(di));
}

static void instrument_store(IRSB* const bb, IRExpr* addr_expr,
                             IRExpr* const data_expr,
                             IRExpr* const guard_expr/* NULL => True */)
{
   IRExpr* size_expr;
   IRExpr** argv;
   IRDirty* di;
   HWord size;

   size = sizeofIRType(typeOfIRExpr(bb->tyenv, data_expr));

   if (UNLIKELY(DRD_(any_address_is_traced)())) {
      IRTemp tmp = newIRTemp(bb->tyenv, typeOfIRExpr(bb->tyenv, addr_expr));
      addStmtToIRSB(bb, IRStmt_WrTmp(tmp, addr_expr));
      addr_expr = IRExpr_RdTmp(tmp);
      instr_trace_mem_store(bb, addr_expr, NULL, data_expr, guard_expr);
   }

   if (!s_check_stack_accesses && is_stack_access(bb, addr_expr))
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
   if (guard_expr) di->guard = guard_expr;
   addStmtToIRSB(bb, IRStmt_Dirty(di));
}

IRSB* DRD_(instrument)(VgCallbackClosure* const closure,
                       IRSB* const bb_in,
                       const VexGuestLayout* const layout,
                       const VexGuestExtents* const vge,
                       const VexArchInfo* archinfo_host,
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
   bb->offsIP   = bb_in->offsIP;

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
         instrument = VG_(DebugInfo_sect_kind)(NULL, st->Ist.IMark.addr)
            != Vg_SectPLT;
         addStmtToIRSB(bb, st);
         break;

      case Ist_MBE:
         switch (st->Ist.MBE.event)
         {
         case Imbe_Fence:
            break; /* not interesting to DRD */
         case Imbe_CancelReservation:
            break; /* not interesting to DRD */
         default:
            tl_assert(0);
         }
         addStmtToIRSB(bb, st);
         break;

      case Ist_Store:
         if (instrument)
            instrument_store(bb, st->Ist.Store.addr, st->Ist.Store.data,
                             NULL/* no guard */);
         addStmtToIRSB(bb, st);
         break;

      case Ist_StoreG: {
         IRStoreG* sg   = st->Ist.StoreG.details;
         IRExpr*   data = sg->data;
         IRExpr*   addr = sg->addr;
         if (instrument)
            instrument_store(bb, addr, data, sg->guard);
         addStmtToIRSB(bb, st);
         break;
      }

      case Ist_LoadG: {
         IRLoadG* lg        = st->Ist.LoadG.details;
         IRType   type      = Ity_INVALID; /* loaded type */
         IRType   typeWide  = Ity_INVALID; /* after implicit widening */
         IRExpr*  addr_expr = lg->addr;
         typeOfIRLoadGOp(lg->cvt, &typeWide, &type);
         tl_assert(type != Ity_INVALID);
         if (UNLIKELY(DRD_(any_address_is_traced)())) {
            addr_expr = instr_trace_mem_load(bb, addr_expr,
                                             sizeofIRType(type), lg->guard);
         }
         instrument_load(bb, lg->addr,
                         sizeofIRType(type), lg->guard);
         addStmtToIRSB(bb, st);
         break;
      }

      case Ist_WrTmp:
         if (instrument) {
            const IRExpr* const data = st->Ist.WrTmp.data;
            IRExpr* addr_expr = data->Iex.Load.addr;
            if (data->tag == Iex_Load) {
               if (UNLIKELY(DRD_(any_address_is_traced)())) {
                  addr_expr = instr_trace_mem_load(bb, addr_expr,
                                       sizeofIRType(data->Iex.Load.ty),
                                       NULL/* no guard */);
               }
               instrument_load(bb, addr_expr, sizeofIRType(data->Iex.Load.ty),
                               NULL/* no guard */);
            }
         }
         addStmtToIRSB(bb, st);
         break;

      case Ist_Dirty:
         if (instrument) {
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
         if (instrument) {
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

            if (UNLIKELY(DRD_(any_address_is_traced)()))
               instr_trace_mem_store(bb, cas->addr, cas->dataHi, cas->dataLo,
                                     NULL/* no guard */);

            instrument_load(bb, cas->addr, dataSize, NULL/*no guard*/);
         }
         addStmtToIRSB(bb, st);
         break;

      case Ist_LLSC: {
         /*
          * Ignore store-conditionals (except for tracing), and handle
          * load-linked's exactly like normal loads.
          */
         IRType dataTy;

         if (st->Ist.LLSC.storedata == NULL) {
            /* LL */
            dataTy = typeOfIRTemp(bb_in->tyenv, st->Ist.LLSC.result);
            if (instrument) {
               IRExpr* addr_expr = st->Ist.LLSC.addr;
               if (UNLIKELY(DRD_(any_address_is_traced)()))
                  addr_expr = instr_trace_mem_load(bb, addr_expr,
                                                   sizeofIRType(dataTy),
                                                   NULL /* no guard */);

               instrument_load(bb, addr_expr, sizeofIRType(dataTy),
                               NULL/*no guard*/);
            }
         } else {
            /* SC */
            instr_trace_mem_store(bb, st->Ist.LLSC.addr, NULL,
                                  st->Ist.LLSC.storedata,
                                  NULL/* no guard */);
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

