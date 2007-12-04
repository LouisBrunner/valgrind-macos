/*
  This file is part of drd, a data race detector.

  Copyright (C) 2006-2007 Bart Van Assche
  bart.vanassche@gmail.com

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


#include "pub_drd_bitmap.h"
#include "drd_clientreq.h"
#include "drd_cond.h"
#include "drd_error.h"
#include "drd_malloc_wrappers.h"
#include "drd_mutex.h"
#include "drd_segment.h"
#include "drd_suppression.h"
#include "drd_thread.h"
#include "drd_track.h"
#include "drd_vc.h"
#include "priv_drd_clientreq.h"
#include "pub_core_mallocfree.h"
#include "pub_core_options.h"
#include "pub_tool_vki.h"
#include "pub_tool_basics.h"
#include "pub_tool_debuginfo.h"   // VG_(describe_IP)()
#include "pub_tool_libcassert.h"  // tl_assert()
#include "pub_tool_libcbase.h"    // VG_(strcmp)
#include "pub_tool_libcprint.h"   // VG_(printf)
#include "pub_tool_libcproc.h"
#include "pub_tool_machine.h"
#include "pub_tool_options.h"     // command line options
#include "pub_tool_threadstate.h" // VG_(get_running_tid)
#include "pub_tool_tooliface.h"


// Type definitions.

#if 0
typedef struct 
{
  const Char* const soname;
  const Char* const symbol;
} SuppressedSymbol;
#endif


// Function declarations.

static void drd_start_client_code(const ThreadId tid, const ULong bbs_done);
static void drd_set_running_tid(const ThreadId tid);



// Local variables.

static Bool drd_print_stats = False;
static Bool drd_trace_mem = False;
static Bool drd_trace_fork_join = False;
static Addr drd_trace_address = 0;


//
// Implement the needs_command_line_options for drd.
//

static Bool drd_process_cmd_line_option(Char* arg)
{
   Bool trace_cond        = False;
   Bool trace_mutex       = False;
   Bool trace_segment     = False;
   Bool trace_suppression = False;
   Char* trace_address    = 0;

   VG_BOOL_CLO     (arg, "--drd-stats",         drd_print_stats)
   else VG_BOOL_CLO(arg, "--trace-cond",        trace_cond)
   else VG_BOOL_CLO(arg, "--trace-fork-join",   drd_trace_fork_join)
   else VG_BOOL_CLO(arg, "--trace-mem",         drd_trace_mem)
   else VG_BOOL_CLO(arg, "--trace-mutex",       trace_mutex)
   else VG_BOOL_CLO(arg, "--trace-segment",     trace_segment)
   else VG_BOOL_CLO(arg, "--trace-suppression", trace_suppression)
   else VG_STR_CLO (arg, "--trace-address",     trace_address)
   else
      return False;

   if (trace_address)
      drd_trace_address = VG_(strtoll16)(trace_address, 0);
   if (trace_cond)
      cond_set_trace(trace_cond);
   if (trace_mutex)
      mutex_set_trace(trace_mutex);
   if (trace_segment)
      sg_set_trace(trace_segment);
   if (trace_suppression)
      suppression_set_trace(trace_suppression);

   return True;
}

static void drd_print_usage(void)
{  
   VG_(printf)("    --trace-mem=no|yes Trace all memory accesses to stdout[no]"
               "\n"
               "    --trace-fork-join=no|yes Trace all thread creation and join"
               " activity\n"
               "    --trace-mutex=no|yes Trace all mutex activity\n"
               "    --trace-segment=no|yes Trace segment actions\n"
               );
}

static void drd_print_debug_usage(void)
{  
}


//
// Implements the thread-related core callbacks.
//

static
VG_REGPARM(2) void drd_trace_load(Addr addr, SizeT size)
{
   Segment* sg;

   thread_set_vg_running_tid(VG_(get_running_tid)());

   if (! thread_is_recording(thread_get_running_tid()))
      return;

#if 1
   if (drd_trace_mem || (addr == drd_trace_address))
   {
      VG_(message)(Vg_UserMsg, "load  0x%lx size %ld %s (vg %d / drd %d)",
                   addr,
                   size,
                   thread_get_name(thread_get_running_tid()),
                   VG_(get_running_tid)(),
                   thread_get_running_tid());
      VG_(get_and_pp_StackTrace)(VG_(get_running_tid)(),
                                 VG_(clo_backtrace_size));
      tl_assert(DrdThreadIdToVgThreadId(thread_get_running_tid())
                == VG_(get_running_tid)());
   }
#endif
   sg = thread_get_segment(thread_get_running_tid());
   bm_access_range(sg->bm, addr, size, eLoad);
   if (thread_conflicting_access(addr, size, eLoad))
   {
      DataRaceErrInfo drei;
      drei.tid  = VG_(get_running_tid)();
      drei.addr = addr;
      drei.size = size;
      drei.access_type = eLoad;
      VG_(maybe_record_error)(VG_(get_running_tid)(),
                              DataRaceErr,
                              VG_(get_IP)(VG_(get_running_tid)()),
                              "Conflicting accesses",
                              &drei);
   }
}

static
VG_REGPARM(2) void drd_trace_store(Addr addr, SizeT size)
{
   Segment* sg;

   thread_set_vg_running_tid(VG_(get_running_tid)());

   if (! thread_is_recording(thread_get_running_tid()))
      return;

#if 1
   if (drd_trace_mem || (addr == drd_trace_address))
   {
      VG_(message)(Vg_UserMsg, "store 0x%lx size %ld %s (vg %d / drd %d / off %d)",
                   addr,
                   size,
                   thread_get_name(thread_get_running_tid()),
                   VG_(get_running_tid)(),
                   thread_get_running_tid(),
                   addr - thread_get_stack_min(thread_get_running_tid()));
      VG_(get_and_pp_StackTrace)(VG_(get_running_tid)(),
                                 VG_(clo_backtrace_size));
      tl_assert(DrdThreadIdToVgThreadId(thread_get_running_tid())
                == VG_(get_running_tid)());
   }
#endif
   sg = thread_get_segment(thread_get_running_tid());
   bm_access_range(sg->bm, addr, size, eStore);
   if (thread_conflicting_access(addr, size, eStore))
   {
      DataRaceErrInfo drei;
      drei.tid  = VG_(get_running_tid)();
      drei.addr = addr;
      drei.size = size;
      drei.access_type = eStore;
      VG_(maybe_record_error)(VG_(get_running_tid)(),
                              DataRaceErr,
                              VG_(get_IP)(VG_(get_running_tid)()),
                                    "Conflicting accesses",
                              &drei);
   }
}

static void drd_pre_mem_read(const CorePart part,
                             const ThreadId tid,
                             Char* const s,
                             const Addr a,
                             const SizeT size)
{
   if (size > 0)
   {
      drd_trace_load(a, size);
   }
}

static void drd_post_mem_write(const CorePart part,
                               const ThreadId tid,
                               const Addr a,
                               const SizeT size)
{
   if (size > 0)
   {
      drd_trace_store(a, size);
   }
}

static void drd_start_using_mem(const Addr a1, const Addr a2)
{
   thread_set_vg_running_tid(VG_(get_running_tid)());

   if (a1 <= drd_trace_address && drd_trace_address < a2
       && thread_is_recording(thread_get_running_tid()))
   {
      VG_(message)(Vg_UserMsg, "start 0x%lx size %ld %s (tracing 0x%lx)",
                   a1, a2 - a1, thread_get_name(thread_get_running_tid()),
                   drd_trace_address);
      VG_(get_and_pp_StackTrace)(VG_(get_running_tid)(),
                                 VG_(clo_backtrace_size));
   }
}

static void drd_stop_using_mem(const Addr a1, const Addr a2)
{
   if (a1 <= drd_trace_address && drd_trace_address < a2
       && thread_is_recording(thread_get_running_tid()))
   {
      VG_(message)(Vg_UserMsg, "end   0x%lx size %ld %s (tracing 0x%lx)",
                   a1, a2 - a1, thread_get_name(thread_get_running_tid()),
                   drd_trace_address);
      VG_(get_and_pp_StackTrace)(VG_(get_running_tid)(),
                                 VG_(clo_backtrace_size));
   }
   thread_stop_using_mem(a1, a2);
   mutex_stop_using_mem(a1, a2);
   cond_stop_using_mem(a1, a2);
   drd_suppression_stop_using_mem(a1, a2);
}

static VG_REGPARM(2)
     void drd_make_stack_uninit(const Addr base, const UWord len)
{
#if 0
   VG_(message)(Vg_DebugMsg, "make_stack_uninit(0x%lx, %ld)", base, len);
#endif
   drd_stop_using_mem(base, base + len);
}

/* Called by the core when the stack of a thread grows, to indicate that */
/* the addresses in range [ a, a + len [ may now be used by the client.  */
/* Assumption: stacks grow downward.                                     */
static void drd_start_using_mem_stack(const Addr a, const SizeT len)
{
   thread_set_stack_min(thread_get_running_tid(), a - VG_STACK_REDZONE_SZB);
   drd_start_using_mem(a - VG_STACK_REDZONE_SZB,
                       a - VG_STACK_REDZONE_SZB + len);
}

/* Called by the core when the stack of a thread shrinks, to indicate that */
/* the addresses [ a, a + len [ are no longer accessible for the client.   */
/* Assumption: stacks grow downward.                                       */
static void drd_stop_using_mem_stack(const Addr a, const SizeT len)
{
   thread_set_vg_running_tid(VG_(get_running_tid)());
   thread_set_stack_min(thread_get_running_tid(),
                        a + len - VG_STACK_REDZONE_SZB);
   drd_stop_using_mem(a - VG_STACK_REDZONE_SZB,
                      a + len - VG_STACK_REDZONE_SZB);
}

static void drd_start_using_mem_mmap(Addr a, SizeT len,
                                     Bool rr, Bool ww, Bool xx)
{
   drd_start_using_mem(a, a + len);
}

static void drd_stop_using_mem_munmap(Addr a, SizeT len)
{
   drd_stop_using_mem(a, a + len);
}

static
void drd_pre_thread_create(const ThreadId creator, const ThreadId created)
{
   const DrdThreadId drd_creator = VgThreadIdToDrdThreadId(creator);
   tl_assert(created != VG_INVALID_THREADID);
   thread_pre_create(drd_creator, created);
#if 1
   // Hack: compensation for code missing in coregrind/m_main.c.
   if (created == 1)
   {
      thread_set_running_tid(1, 1);
   }
#endif
   if (IsValidDrdThreadId(drd_creator))
   {
      thread_new_segment(drd_creator);
   }
   if (drd_trace_fork_join)
   {
      VG_(message)(Vg_DebugMsg,
                   "drd_pre_thread_create creator = %d/%d, created = %d",
                   creator, drd_creator, created);
   }
}

/* Called by Valgrind's core before any loads or stores are performed on */
/* the context of thread "created". At startup, this function is called  */
/* with arguments (0,1).                                                 */
static
void drd_post_thread_create(const ThreadId created)
{
   const DrdThreadId drd_created = thread_post_create(created);
   tl_assert(created != VG_INVALID_THREADID);
   if (drd_trace_fork_join)
   {
      VG_(message)(Vg_DebugMsg,
                   "drd_post_thread_create created = %d/%d",
                   created, drd_created);
   }
}

/* Process VG_USERREQ__POST_THREAD_JOIN. This client request is invoked just */
/* after thread drd_joiner joined thread drd_joinee.                         */
void drd_post_thread_join(DrdThreadId drd_joiner, DrdThreadId drd_joinee)
{
   tl_assert(IsValidDrdThreadId(drd_joiner));
   tl_assert(IsValidDrdThreadId(drd_joinee));
   thread_new_segment(drd_joinee);
   thread_combine_vc(drd_joiner, drd_joinee);
   thread_new_segment(drd_joiner);

   if (drd_trace_fork_join)
   {
      char msg[256];
      const ThreadId joiner = DrdThreadIdToVgThreadId(drd_joiner);
      const ThreadId joinee = DrdThreadIdToVgThreadId(drd_joinee);
      VG_(snprintf)(msg, sizeof(msg),
                    "drd_post_thread_join joiner = %d/%d, joinee = %d/%d",
                    joiner, drd_joiner, joinee, drd_joinee);
      if (joiner)
      {
         VG_(snprintf)(msg + VG_(strlen)(msg), sizeof(msg) - VG_(strlen)(msg),
                       ", new vc: ");
         vc_snprint(msg + VG_(strlen)(msg), sizeof(msg) - VG_(strlen)(msg),
                    thread_get_vc(drd_joiner));
      }
      VG_(message)(Vg_DebugMsg, msg);
   }

   thread_delete(drd_joinee);
   mutex_thread_delete(drd_joinee);
}

/* Called after a thread has performed its last memory access. */
static void drd_thread_finished(ThreadId tid)
{
   const DrdThreadId drd_tid = VgThreadIdToDrdThreadId(tid);
   if (drd_trace_fork_join)
   {
      VG_(message)(Vg_DebugMsg,
                   "drd_thread_finished tid = %d/%d%s",
                   tid,
                   drd_tid,
                   thread_get_joinable(drd_tid)
                   ? ""
                   : " (which is a detached thread)");

   }
   thread_finished(drd_tid);
}

void drd_pre_mutex_init(Addr mutex, SizeT size, MutexT mutex_type)
{
   mutex_init(mutex, size, mutex_type);
}

void drd_post_mutex_destroy(Addr mutex, MutexT mutex_type)
{
   struct mutex_info* p;

   p = mutex_get(mutex);
   if (p)
   {
      // TO DO: report an error in case the recursion count is not zero
      // before asserting.
      tl_assert(mutex_get_recursion_count(mutex) == 0);
      mutex_destroy(p);
   }
}

void drd_pre_mutex_lock(const DrdThreadId drd_tid,
                        const Addr mutex,
                        const SizeT size,
                        const MutexT mutex_type)
{
   if (mutex_get(mutex) == 0)
   {
      mutex_init(mutex, size, mutex_type);
   }
}

void drd_post_mutex_lock(const DrdThreadId drd_tid,
                         const Addr mutex,
                         const SizeT size,
                         const MutexT mutex_type)
{
   mutex_lock(mutex, size, mutex_type);
}

void drd_pre_mutex_unlock(const DrdThreadId drd_tid,
                          const Addr mutex,
                          const MutexT mutex_type)
{
   mutex_unlock(mutex, mutex_type);
}

void drd_post_cond_init(Addr cond, SizeT s)
{
   if (cond_get(cond))
   {
      CondErrInfo cei = { .cond = cond };
      VG_(maybe_record_error)(VG_(get_running_tid)(),
                              CondErr,
                              VG_(get_IP)(VG_(get_running_tid)()),
                              "initialized twice",
                              &cei);
   }
   cond_init(cond, s);
}

void drd_pre_cond_destroy(Addr cond)
{
   struct cond_info* cond_p;

   cond_p = cond_get(cond);
   if (cond_p)
   {
      cond_destroy(cond_p);
   }
   else
   {
      CondErrInfo cei = { .cond = cond };
      VG_(maybe_record_error)(VG_(get_running_tid)(),
                              CondErr,
                              VG_(get_IP)(VG_(get_running_tid)()),
                              "destroy requested but not initialized",
                              &cei);
   }
}


//
// Implementation of the tool interface.
//

static
void drd_post_clo_init(void)
{
#  if defined(VGP_x86_linux) || defined(VGP_amd64_linux)
   /* fine */
#  else
   VG_(printf)("\nWARNING: DRD has only been tested on x86-linux and amd64-linux.\n\n");
#  endif
}

static
IRSB* drd_instrument(VgCallbackClosure* const closure,
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
   IRExpr*  addr_expr;
   IRExpr*  size_expr;
   Bool     instrument = True;

   /* Set up BB */
   bb           = emptyIRSB();
   bb->tyenv    = deepCopyIRTypeEnv(bb_in->tyenv);
   bb->next     = deepCopyIRExpr(bb_in->next);
   bb->jumpkind = bb_in->jumpkind;

   for (i = 0; i < bb_in->stmts_used; i++)
   {
      IRStmt* const st = bb_in->stmts[i];
      if (!st || st->tag == Ist_NoOp)
         continue;

      switch (st->tag)
      {
      case Ist_IMark:
         instrument = VG_(seginfo_sect_kind)(st->Ist.IMark.addr) != Vg_SectPLT;
         break;

      case Ist_AbiHint:
         addStmtToIRSB(bb,
            IRStmt_Dirty(
               unsafeIRDirty_0_N(
                  /*regparms*/2,
                  "drd_make_stack_uninit",
                  VG_(fnptr_to_fnentry)(drd_make_stack_uninit),
                  mkIRExprVec_2(st->Ist.AbiHint.base,
                                mkIRExpr_HWord((UInt)st->Ist.AbiHint.len))
                  )
               )
            );
         break;

      case Ist_Store:
         if (instrument)
         {
            addr_expr = st->Ist.Store.addr;
            size_expr = mkIRExpr_HWord( 
                   sizeofIRType(typeOfIRExpr(bb->tyenv, st->Ist.Store.data)));
            argv = mkIRExprVec_2(addr_expr, size_expr);
            di = unsafeIRDirty_0_N(/*regparms*/2, 
                                   "drd_trace_store",
                                   VG_(fnptr_to_fnentry)(drd_trace_store),
                                   argv);
            addStmtToIRSB(bb, IRStmt_Dirty(di));
         }
         addStmtToIRSB(bb, st);
         break;

      case Ist_WrTmp:
         if (instrument)
         {
            const IRExpr* const data = st->Ist.WrTmp.data;
            if (data->tag == Iex_Load)
            {
               addr_expr = data->Iex.Load.addr;
               size_expr = mkIRExpr_HWord(sizeofIRType(data->Iex.Load.ty));
               argv = mkIRExprVec_2(addr_expr, size_expr);
               di = unsafeIRDirty_0_N(/*regparms*/2, 
                                      "drd_trace_load",
                                      VG_(fnptr_to_fnentry)(drd_trace_load),
                                      argv);
               addStmtToIRSB(bb, IRStmt_Dirty(di));
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
                                         VG_(fnptr_to_fnentry)(drd_trace_load),
                                         argv);
                  addStmtToIRSB(bb, IRStmt_Dirty(di));
               }
               if (mFx == Ifx_Write || mFx == Ifx_Modify) {
                  di = unsafeIRDirty_0_N(
                                         /*regparms*/2,
                                         "drd_trace_store",
                                         VG_(fnptr_to_fnentry)(drd_trace_store),
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

      default:
         addStmtToIRSB(bb, st);
         break;
      }
   }

   return bb;
}

static void drd_set_running_tid(const ThreadId vg_tid)
{
   static ThreadId s_last_vg_tid = VG_INVALID_THREADID;
   if (vg_tid != s_last_vg_tid)
   {
      const DrdThreadId drd_tid = VgThreadIdToDrdThreadId(vg_tid);
      tl_assert(drd_tid != DRD_INVALID_THREADID);
      s_last_vg_tid = vg_tid;
      if (drd_trace_fork_join)
      {
         VG_(message)(Vg_DebugMsg,
                      "drd_track_thread_run tid = %d / drd tid %d",
                      vg_tid, drd_tid);
      }
      thread_set_running_tid(vg_tid, drd_tid);
   }
}

static void drd_start_client_code(const ThreadId tid, const ULong bbs_done)
{
   drd_set_running_tid(tid);
}

static
void drd_fini(Int exitcode)
{
   // thread_print_all();
#ifdef OLD_RACE_DETECTION_ALGORITHM
   thread_report_all_races();
#endif
   if (VG_(clo_verbosity) > 1 || drd_print_stats)
   {
      VG_(message)(Vg_DebugMsg,
                   "   thread: %lld context switches"
                   " / %lld updates of the danger set",
                   thread_get_context_switch_count(),
                   thread_get_update_danger_set_count());
#ifdef OLD_RACE_DETECTION_ALGORITHM
      VG_(message)(Vg_DebugMsg,
                   " analysis: %lld data race analysis points",
                   thread_get_report_races_count());
#endif
      VG_(message)(Vg_DebugMsg,
                   " segments: %lld total, %lld max, %lld discard points",
                   sg_get_segments_created_count(),
                   sg_get_max_segments_alive_count(),
                   thread_get_discard_ordered_segments_count());
      VG_(message)(Vg_DebugMsg,
                   "  bitmaps: %lld / %lld bitmaps were allocated"
                   " and %lld / %lld for danger set updates",
                   bm_get_bitmap_creation_count(),
                   bm_get_bitmap2_creation_count(),
                   thread_get_danger_set_bitmap_creation_count(),
                   thread_get_danger_set_bitmap2_creation_count());
      VG_(message)(Vg_DebugMsg,
                   "    mutex: %lld non-recursive lock/unlock events",
                   get_mutex_lock_count());
      drd_print_malloc_stats();
   }
}

static void drd_load_suppression_file(void)
{
   tl_assert(VG_(clo_n_suppressions) < VG_CLO_MAX_SFILES - 1);
   {
      /* If we haven't reached the max number of suppression files, load
         the drd suppression patterns file. */
      static const Char drd_supp[] = "glibc-2.X-drd.supp";
      const Int len = VG_(strlen)(VG_(libdir)) + 1 + sizeof(drd_supp);
      Char* const buf = VG_(arena_malloc)(VG_AR_CORE, len);
      VG_(snprintf)(buf, len, "%s/%s", VG_(libdir), drd_supp);
      VG_(clo_suppressions)[VG_(clo_n_suppressions)] = buf;
      VG_(clo_n_suppressions)++;
   }
}

static
void drd_pre_clo_init(void)
{
   // Basic tool stuff.

   VG_(details_name)            ("exp-drd");
   VG_(details_version)         (NULL);
   VG_(details_description)     ("a data race detector");
   VG_(details_copyright_author)("Copyright (C) 2006-2007, and GNU GPL'd,"
                                 " by Bart Van Assche.");
   VG_(details_bug_reports_to)  (VG_BUGS_TO);

   VG_(basic_tool_funcs)        (drd_post_clo_init,
                                 drd_instrument,
                                 drd_fini);

   // Command line stuff.
   VG_(needs_command_line_options)(drd_process_cmd_line_option,
                                   drd_print_usage,
                                   drd_print_debug_usage);

   drd_register_error_handlers();

   // Core event tracking.
   VG_(track_pre_mem_read)         (drd_pre_mem_read);
   VG_(track_post_mem_write)       (drd_post_mem_write);
   VG_(track_new_mem_stack)        (drd_start_using_mem_stack);
   VG_(track_die_mem_stack)        (drd_stop_using_mem_stack);
   VG_(track_new_mem_mmap)         (drd_start_using_mem_mmap);
   VG_(track_die_mem_munmap)       (drd_stop_using_mem_munmap);
   VG_(track_start_client_code)    (drd_start_client_code);
   VG_(track_pre_thread_ll_create) (drd_pre_thread_create);
   VG_(track_pre_thread_first_insn)(drd_post_thread_create);
   VG_(track_pre_thread_ll_exit)   (drd_thread_finished);

   // Other stuff.
   VG_(needs_data_syms)();

   drd_register_malloc_wrappers(drd_start_using_mem, drd_stop_using_mem);

   drd_clientreq_init();

   drd_suppression_init();

   drd_load_suppression_file();
}


VG_DETERMINE_INTERFACE_VERSION(drd_pre_clo_init)


/*
 * Local variables:
 * c-basic-offset: 3
 * End:
 */
