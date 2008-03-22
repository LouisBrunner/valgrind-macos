/*
  This file is part of drd, a data race detector.

  Copyright (C) 2006-2008 Bart Van Assche
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


#include "drd_barrier.h"
#include "drd_clientreq.h"
#include "drd_clientobj.h"
#include "drd_cond.h"
#include "drd_error.h"
#include "drd_malloc_wrappers.h"
#include "drd_mutex.h"
#include "drd_rwlock.h"
#include "drd_segment.h"
#include "drd_semaphore.h"
#include "drd_suppression.h"
#include "drd_thread.h"
#include "drd_track.h"
#include "drd_vc.h"
#include "priv_drd_clientreq.h"
#include "pub_drd_bitmap.h"
#include "pub_tool_basics.h"
#include "pub_tool_debuginfo.h"   // VG_(describe_IP)()
#include "pub_tool_libcassert.h"  // tl_assert()
#include "pub_tool_libcbase.h"    // VG_(strcmp)
#include "pub_tool_libcprint.h"   // VG_(printf)
#include "pub_tool_vki.h"         // Must be included before pub_tool_libcproc
#include "pub_tool_libcproc.h"
#include "pub_tool_machine.h"
#include "pub_tool_options.h"     // command line options
#include "pub_tool_threadstate.h" // VG_(get_running_tid)()
#include "pub_tool_tooliface.h"


// Function declarations.

static void drd_start_client_code(const ThreadId tid, const ULong bbs_done);


// Local variables.

static Bool drd_print_stats = False;
static Bool drd_trace_fork_join = False;
static Bool drd_trace_mem = False;
static Addr drd_trace_address = 0;


//
// Implement the needs_command_line_options for drd.
//

static Bool drd_process_cmd_line_option(Char* arg)
{
  int segment_merging   = -1;
  int show_confl_seg    = -1;
  int trace_barrier     = -1;
  int trace_clientobj   = -1;
  int trace_cond        = -1;
  int trace_csw         = -1;
  int trace_danger_set  = -1;
  int trace_mutex       = -1;
  int trace_rwlock      = -1;
  int trace_segment     = -1;
  int trace_semaphore   = -1;
  int trace_suppression = -1;
  Char* trace_address   = 0;

  VG_BOOL_CLO     (arg, "--drd-stats",         drd_print_stats)
  else VG_BOOL_CLO(arg, "--segment-merging",   segment_merging)
  else VG_BOOL_CLO(arg, "--show-confl-seg",    show_confl_seg)
  else VG_BOOL_CLO(arg, "--trace-barrier",     trace_barrier)
  else VG_BOOL_CLO(arg, "--trace-clientobj",   trace_clientobj)
  else VG_BOOL_CLO(arg, "--trace-cond",        trace_cond)
  else VG_BOOL_CLO(arg, "--trace-csw",         trace_csw)
  else VG_BOOL_CLO(arg, "--trace-danger-set",  trace_danger_set)
  else VG_BOOL_CLO(arg, "--trace-fork-join",   drd_trace_fork_join)
  else VG_BOOL_CLO(arg, "--trace-mem",         drd_trace_mem)
  else VG_BOOL_CLO(arg, "--trace-mutex",       trace_mutex)
  else VG_BOOL_CLO(arg, "--trace-rwlock",      trace_rwlock)
  else VG_BOOL_CLO(arg, "--trace-segment",     trace_segment)
  else VG_BOOL_CLO(arg, "--trace-semaphore",   trace_semaphore)
  else VG_BOOL_CLO(arg, "--trace-suppression", trace_suppression)
  else VG_STR_CLO (arg, "--trace-address",     trace_address)
  else
    return False;

  if (segment_merging != -1)
    thread_set_segment_merging(segment_merging);
  if (show_confl_seg != -1)
    set_show_conflicting_segments(show_confl_seg);
  if (trace_address)
  {
    drd_trace_address = VG_(strtoll16)(trace_address, 0);
  }
  if (trace_barrier != -1)
    barrier_set_trace(trace_barrier);
  if (trace_clientobj != -1)
    clientobj_set_trace(trace_clientobj);
  if (trace_cond != -1)
    cond_set_trace(trace_cond);
  if (trace_csw != -1)
    thread_trace_context_switches(trace_csw);
  if (trace_danger_set != -1)
    thread_trace_danger_set(trace_danger_set);
  if (trace_mutex != -1)
    mutex_set_trace(trace_mutex);
  if (trace_rwlock != -1)
    rwlock_set_trace(trace_rwlock);
  if (trace_segment != -1)
    sg_set_trace(trace_segment);
  if (trace_semaphore != -1)
    semaphore_set_trace(trace_semaphore);
  if (trace_suppression != -1)
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

static void drd_trace_mem_access(const Addr addr, const SizeT size,
                                 const BmAccessTypeT access_type)
{
  char vc[80];
  vc_snprint(vc, sizeof(vc), thread_get_vc(thread_get_running_tid()));
  VG_(message)(Vg_UserMsg,
               "%s 0x%lx size %ld (vg %d / drd %d / vc %s)",
               access_type == eLoad
               ? "load "
               : access_type == eStore
               ? "store"
               : access_type == eStart
               ? "start"
               : access_type == eEnd
               ? "end  "
               : "????",
               addr,
               size,
               VG_(get_running_tid)(),
               thread_get_running_tid(),
               vc);
  VG_(get_and_pp_StackTrace)(VG_(get_running_tid)(),
                             VG_(clo_backtrace_size));
  tl_assert(DrdThreadIdToVgThreadId(thread_get_running_tid())
            == VG_(get_running_tid)());
}

static void drd_report_race(const Addr addr, const SizeT size,
                            const BmAccessTypeT access_type)
{
  DataRaceErrInfo drei;

  if (drd_is_suppressed(addr, addr + size))
    return;

  drei.tid  = thread_get_running_tid();
  drei.addr = addr;
  drei.size = size;
  drei.access_type = access_type;
  VG_(maybe_record_error)(VG_(get_running_tid)(),
                          DataRaceErr,
                          VG_(get_IP)(VG_(get_running_tid)()),
                          "Conflicting accesses",
                          &drei);
}

static VG_REGPARM(2) void drd_trace_load(Addr addr, SizeT size)
{
  Segment* sg;

#if 0
  /* The assert below has been commented out because of performance reasons.*/
  tl_assert(thread_get_running_tid()
            == VgThreadIdToDrdThreadId(VG_(get_running_tid())));
#endif

  if (! running_thread_is_recording())
    return;

  if (drd_trace_mem || (addr == drd_trace_address))
  {
    drd_trace_mem_access(addr, size, eLoad);
  }
  sg = running_thread_get_segment();
  bm_access_range_load(sg->bm, addr, addr + size);
  if (bm_load_has_conflict_with(thread_get_danger_set(), addr, addr + size))
  {
    drd_report_race(addr, size, eLoad);
  }
}

static VG_REGPARM(1) void drd_trace_load_1(Addr addr)
{
  Segment* sg;

  if (! running_thread_is_recording())
    return;

  if (drd_trace_mem || (addr == drd_trace_address))
  {
    drd_trace_mem_access(addr, 1, eLoad);
  }
  sg = running_thread_get_segment();
  bm_access_load_1(sg->bm, addr);
  if (bm_load_1_has_conflict_with(thread_get_danger_set(), addr))
  {
    drd_report_race(addr, 1, eLoad);
  }
}

static VG_REGPARM(1) void drd_trace_load_2(Addr addr)
{
  Segment* sg;

  if (! running_thread_is_recording())
    return;

  if (drd_trace_mem || (addr == drd_trace_address))
  {
    drd_trace_mem_access(addr, 2, eLoad);
  }
  sg = running_thread_get_segment();
  bm_access_load_2(sg->bm, addr);
  if (bm_load_2_has_conflict_with(thread_get_danger_set(), addr))
  {
    drd_report_race(addr, 2, eLoad);
  }
}

static VG_REGPARM(1) void drd_trace_load_4(Addr addr)
{
  Segment* sg;

  if (! running_thread_is_recording())
    return;

  if (drd_trace_mem || (addr == drd_trace_address))
  {
    drd_trace_mem_access(addr, 4, eLoad);
  }
  sg = running_thread_get_segment();
  bm_access_load_4(sg->bm, addr);
  if (bm_load_4_has_conflict_with(thread_get_danger_set(), addr))
  {
    drd_report_race(addr, 4, eLoad);
  }
}

static VG_REGPARM(1) void drd_trace_load_8(Addr addr)
{
  Segment* sg;

  if (! running_thread_is_recording())
    return;

  if (drd_trace_mem || (addr == drd_trace_address))
  {
    drd_trace_mem_access(addr, 8, eLoad);
  }
  sg = running_thread_get_segment();
  bm_access_load_8(sg->bm, addr);
  if (bm_load_8_has_conflict_with(thread_get_danger_set(), addr))
  {
    drd_report_race(addr, 8, eLoad);
  }
}

static
VG_REGPARM(2) void drd_trace_store(Addr addr, SizeT size)
{
  Segment* sg;

#if 0
  /* The assert below has been commented out because of performance reasons.*/
  tl_assert(thread_get_running_tid()
            == VgThreadIdToDrdThreadId(VG_(get_running_tid())));
#endif

  if (! running_thread_is_recording())
    return;

  if (drd_trace_mem || (addr == drd_trace_address))
  {
    drd_trace_mem_access(addr, size, eStore);
  }
  sg = running_thread_get_segment();
  bm_access_range_store(sg->bm, addr, addr + size);
  if (bm_store_has_conflict_with(thread_get_danger_set(), addr, addr + size))
  {
    drd_report_race(addr, size, eStore);
  }
}

static VG_REGPARM(1) void drd_trace_store_1(Addr addr)
{
  Segment* sg;

  if (! running_thread_is_recording())
    return;

  if (drd_trace_mem || (addr == drd_trace_address))
  {
    drd_trace_mem_access(addr, 1, eStore);
  }
  sg = running_thread_get_segment();
  bm_access_store_1(sg->bm, addr);
  if (bm_store_1_has_conflict_with(thread_get_danger_set(), addr))
  {
    drd_report_race(addr, 1, eStore);
  }
}

static VG_REGPARM(1) void drd_trace_store_2(Addr addr)
{
  Segment* sg;

  if (! running_thread_is_recording())
    return;

  if (drd_trace_mem || (addr == drd_trace_address))
  {
    drd_trace_mem_access(addr, 2, eStore);
  }
  sg = running_thread_get_segment();
  bm_access_store_2(sg->bm, addr);
  if (bm_store_2_has_conflict_with(thread_get_danger_set(), addr))
  {
    drd_report_race(addr, 2, eStore);
  }
}

static VG_REGPARM(1) void drd_trace_store_4(Addr addr)
{
  Segment* sg;

  if (! running_thread_is_recording())
    return;

  if (drd_trace_mem || (addr == drd_trace_address))
  {
    drd_trace_mem_access(addr, 4, eStore);
  }
  sg = running_thread_get_segment();
  bm_access_store_4(sg->bm, addr);
  if (bm_store_4_has_conflict_with(thread_get_danger_set(), addr))
  {
    drd_report_race(addr, 4, eStore);
  }
}

static VG_REGPARM(1) void drd_trace_store_8(Addr addr)
{
  Segment* sg;

  if (! running_thread_is_recording())
    return;

  if (drd_trace_mem || (addr == drd_trace_address))
  {
    drd_trace_mem_access(addr, 8, eStore);
  }
  sg = running_thread_get_segment();
  bm_access_store_8(sg->bm, addr);
  if (bm_store_8_has_conflict_with(thread_get_danger_set(), addr))
  {
    drd_report_race(addr, 8, eStore);
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

static void drd_pre_mem_read_asciiz(const CorePart part,
                                    const ThreadId tid,
                                    Char* const s,
                                    const Addr a)
{
  const char* p = (void*)a;
  SizeT size = 0;

  /* Note: the expression '*p' reads client memory and may crash if the */
  /* client provided an invalid pointer !                               */
  while (*p)
  {
    p++;
    size++;
  }
  // To do: find out what a reasonable upper limit on 'size' is.
  tl_assert(size < 4096);
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
  thread_set_vg_running_tid(VG_(get_running_tid)());
  if (size > 0)
  {
    drd_trace_store(a, size);
  }
}

static void drd_start_using_mem(const Addr a1, const SizeT len)
{
  const Addr a2 = a1 + len;

  tl_assert(a1 < a2);

  if (a1 <= drd_trace_address && drd_trace_address < a2)
  {
    drd_trace_mem_access(a1, len, eStart);
  }
}

static void drd_stop_using_mem(const Addr a1, const SizeT len)
{
  const Addr a2 = a1 + len;

  tl_assert(a1 < a2);

  if (a1 <= drd_trace_address && drd_trace_address < a2)
  {
    drd_trace_mem_access(a1, len, eEnd);
  }
  thread_stop_using_mem(a1, a2);
  clientobj_stop_using_mem(a1, a2);
  drd_suppression_stop_using_mem(a1, a2);
}

static
void drd_start_using_mem_w_perms(const Addr a, const SizeT len,
                                 const Bool rr, const Bool ww, const Bool xx)
{
  thread_set_vg_running_tid(VG_(get_running_tid)());

  drd_start_using_mem(a, len);
}

/* Called by the core when the stack of a thread grows, to indicate that */
/* the addresses in range [ a, a + len [ may now be used by the client.  */
/* Assumption: stacks grow downward.                                     */
static void drd_start_using_mem_stack(const Addr a, const SizeT len)
{
  thread_set_stack_min(thread_get_running_tid(), a - VG_STACK_REDZONE_SZB);
  drd_start_using_mem(a - VG_STACK_REDZONE_SZB, len + VG_STACK_REDZONE_SZB);
}

/* Called by the core when the stack of a thread shrinks, to indicate that */
/* the addresses [ a, a + len [ are no longer accessible for the client.   */
/* Assumption: stacks grow downward.                                       */
static void drd_stop_using_mem_stack(const Addr a, const SizeT len)
{
  thread_set_stack_min(thread_get_running_tid(),
                       a + len - VG_STACK_REDZONE_SZB);
  drd_stop_using_mem(a - VG_STACK_REDZONE_SZB, len + VG_STACK_REDZONE_SZB);
}

static void drd_start_using_mem_stack_signal(const Addr a, const SizeT len)
{
  thread_set_vg_running_tid(VG_(get_running_tid)());
  drd_start_using_mem(a, len);
}

static void drd_stop_using_mem_stack_signal(Addr a, SizeT len)
{
  drd_stop_using_mem(a, len);
}

static
void drd_pre_thread_create(const ThreadId creator, const ThreadId created)
{
  const DrdThreadId drd_creator = VgThreadIdToDrdThreadId(creator);
  tl_assert(created != VG_INVALID_THREADID);
  thread_pre_create(drd_creator, created);
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
  cond_thread_delete(drd_joinee);
  semaphore_thread_delete(drd_joinee);
  barrier_thread_delete(drd_joinee);
}

void drd_trace_addr(const Addr addr)
{
  drd_trace_address = addr;
}

/* Called after a thread has performed its last memory access. */
static void drd_thread_finished(ThreadId vg_tid)
{
  DrdThreadId drd_tid;

  tl_assert(VG_(get_running_tid)() == vg_tid);

  drd_tid = VgThreadIdToDrdThreadId(vg_tid);
  if (drd_trace_fork_join)
  {
    VG_(message)(Vg_DebugMsg,
                 "drd_thread_finished tid = %d/%d%s",
                 vg_tid,
                 drd_tid,
                 thread_get_joinable(drd_tid)
                 ? ""
                 : " (which is a detached thread)");

  }
  drd_stop_using_mem(thread_get_stack_min(drd_tid),
                     thread_get_stack_max(drd_tid)
                     - thread_get_stack_min(drd_tid));
  thread_stop_recording(drd_tid);
  thread_finished(drd_tid);
}

void drd_pre_mutex_init(const Addr mutex, const MutexT mutex_type)
{
  mutex_init(mutex, mutex_type);
}

void drd_post_mutex_destroy(const Addr mutex, const MutexT mutex_type)
{
  mutex_post_destroy(mutex);
}

void drd_pre_mutex_lock(const Addr mutex, const MutexT mutex_type)
{
  mutex_pre_lock(mutex, mutex_type);
}

void drd_post_mutex_lock(const Addr mutex, const Bool took_lock)
{
  mutex_post_lock(mutex, took_lock);
}

void drd_pre_mutex_unlock(const Addr mutex, const MutexT mutex_type)
{
  mutex_unlock(mutex, mutex_type);
}

void drd_pre_cond_init(Addr cond)
{
  cond_pre_init(cond);
}

void drd_post_cond_destroy(Addr cond)
{
  cond_post_destroy(cond);
}

void drd_semaphore_init(const Addr semaphore,
                        const Word pshared, const Word value)
{
  semaphore_init(semaphore, pshared, value);
}

void drd_semaphore_destroy(const Addr semaphore)
{
  semaphore_destroy(semaphore);
}

void drd_semaphore_pre_wait(const DrdThreadId tid, const Addr semaphore)
{
  semaphore_pre_wait(semaphore);
}

void drd_semaphore_post_wait(const DrdThreadId tid, const Addr semaphore,
                             const Bool waited)
{
  semaphore_post_wait(tid, semaphore, waited);
}

void drd_semaphore_pre_post(const DrdThreadId tid, const Addr semaphore)
{
  semaphore_pre_post(tid, semaphore);
}

void drd_semaphore_post_post(const DrdThreadId tid, const Addr semaphore,
                             const Bool waited)
{
  semaphore_post_post(tid, semaphore, waited);
}


void drd_barrier_init(const Addr barrier,
                      const BarrierT barrier_type, const Word count,
                      const Bool reinitialization)
{
  barrier_init(barrier, barrier_type, count, reinitialization);
}

void drd_barrier_destroy(const Addr barrier, const BarrierT barrier_type)
{
  barrier_destroy(barrier, barrier_type);
}

void drd_barrier_pre_wait(const DrdThreadId tid, const Addr barrier,
                          const BarrierT barrier_type)
{
  barrier_pre_wait(tid, barrier, barrier_type);
}

void drd_barrier_post_wait(const DrdThreadId tid, const Addr barrier,
                           const BarrierT barrier_type, const Bool waited)
{
  barrier_post_wait(tid, barrier, barrier_type, waited);
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

static void instrument_load(IRSB* const bb,
                            IRExpr* const addr_expr,
                            const HWord size)
{
  IRExpr* size_expr;
  IRExpr** argv;
  IRDirty* di;

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
                           VG_(fnptr_to_fnentry)(drd_trace_load),
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
                           VG_(fnptr_to_fnentry)(drd_trace_store),
                           argv);
    break;
  }
  addStmtToIRSB(bb, IRStmt_Dirty(di));
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
  Bool     instrument = True;
  Bool     bus_locked = False;

  /* Set up BB */
  bb           = emptyIRSB();
  bb->tyenv    = deepCopyIRTypeEnv(bb_in->tyenv);
  bb->next     = deepCopyIRExpr(bb_in->next);
  bb->jumpkind = bb_in->jumpkind;

  for (i = 0; i < bb_in->stmts_used; i++)
  {
    IRStmt* const st = bb_in->stmts[i];
    tl_assert(st);
    if (st->tag == Ist_NoOp)
      continue;

    switch (st->tag)
    {
    case Ist_IMark:
      instrument = VG_(seginfo_sect_kind)(NULL, 0, st->Ist.IMark.addr)
        != Vg_SectPLT;
      break;

    case Ist_MBE:
      switch (st->Ist.MBE.event)
      {
      case Imbe_Fence:
        break; /* not interesting */
      case Imbe_BusLock:
        tl_assert(! bus_locked);
        bus_locked = True;
        break;
      case Imbe_BusUnlock:
        tl_assert(bus_locked);
        bus_locked = False;
        break;
      default:
        tl_assert(0);
      }
      addStmtToIRSB(bb, st);
      break;

    case Ist_Store:
      if (instrument && ! bus_locked)
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
                                   VG_(fnptr_to_fnentry)(drd_trace_load),
                                   argv);
            addStmtToIRSB(bb, IRStmt_Dirty(di));
          }
          if ((mFx == Ifx_Write || mFx == Ifx_Modify)
              && ! bus_locked)
          {
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

  tl_assert(! bus_locked);

  return bb;
}

static void drd_start_client_code(const ThreadId tid, const ULong bbs_done)
{
  tl_assert(tid == VG_(get_running_tid)());
  thread_set_vg_running_tid(tid);
}

static
void drd_fini(Int exitcode)
{
  // thread_print_all();
  if (VG_(clo_verbosity) > 1 || drd_print_stats)
  {
    VG_(message)(Vg_DebugMsg,
                 "   thread: %lld context switches"
                 " / %lld updates of the danger set",
                 thread_get_context_switch_count(),
                 thread_get_update_danger_set_count());
    VG_(message)(Vg_DebugMsg,
                 " segments: %lld total, %lld max, %lld discard points",
                 sg_get_created_segments_count(),
                 sg_get_max_alive_segments_count(),
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

static
void drd_pre_clo_init(void)
{
  // Basic tool stuff.

  VG_(details_name)            ("exp-drd");
  VG_(details_version)         (NULL);
  VG_(details_description)     ("a data race detector");
  VG_(details_copyright_author)("Copyright (C) 2006-2008, and GNU GPL'd,"
                                " by Bart Van Assche.");
  VG_(details_bug_reports_to)  (VG_BUGS_TO);

  VG_(basic_tool_funcs)        (drd_post_clo_init,
                                drd_instrument,
                                drd_fini);

  // Command line stuff.
  VG_(needs_command_line_options)(drd_process_cmd_line_option,
                                  drd_print_usage,
                                  drd_print_debug_usage);

  // Error handling.
  drd_register_error_handlers();

  // Core event tracking.
  VG_(track_pre_mem_read)         (drd_pre_mem_read);
  VG_(track_pre_mem_read_asciiz)  (drd_pre_mem_read_asciiz);
  VG_(track_post_mem_write)       (drd_post_mem_write);
  VG_(track_new_mem_brk)          (drd_start_using_mem);
  VG_(track_new_mem_mmap)         (drd_start_using_mem_w_perms);
  VG_(track_new_mem_stack)        (drd_start_using_mem_stack);
  VG_(track_new_mem_stack_signal) (drd_start_using_mem_stack_signal);
  VG_(track_new_mem_startup)      (drd_start_using_mem_w_perms);
  VG_(track_die_mem_brk)          (drd_stop_using_mem);
  VG_(track_die_mem_munmap)       (drd_stop_using_mem);
  VG_(track_die_mem_stack)        (drd_stop_using_mem_stack);
  VG_(track_die_mem_stack_signal) (drd_stop_using_mem_stack_signal);
  VG_(track_start_client_code)    (drd_start_client_code);
  VG_(track_pre_thread_ll_create) (drd_pre_thread_create);
  VG_(track_pre_thread_first_insn)(drd_post_thread_create);
  VG_(track_pre_thread_ll_exit)   (drd_thread_finished);

  // Other stuff.
  VG_(needs_var_info)();

  drd_register_malloc_wrappers(drd_start_using_mem, drd_stop_using_mem);

  drd_clientreq_init();

  drd_suppression_init();

  clientobj_init();
}


VG_DETERMINE_INTERFACE_VERSION(drd_pre_clo_init)
