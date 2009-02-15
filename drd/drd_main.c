/*
  This file is part of drd, a data race detector.

  Copyright (C) 2006-2009 Bart Van Assche <bart.vanassche@gmail.com>.

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
#include "drd_clientobj.h"
#include "drd_clientreq.h"
#include "drd_cond.h"
#include "drd_error.h"
#include "drd_load_store.h"
#include "drd_malloc_wrappers.h"
#include "drd_mutex.h"
#include "drd_rwlock.h"
#include "drd_segment.h"
#include "drd_semaphore.h"
#include "drd_suppression.h"
#include "drd_thread.h"
#include "libvex_guest_offsets.h"
#include "pub_drd_bitmap.h"
#include "pub_tool_vki.h"         // Must be included before pub_tool_libcproc
#include "pub_tool_basics.h"
#include "pub_tool_debuginfo.h"   // VG_(describe_IP)()
#include "pub_tool_libcassert.h"  // tl_assert()
#include "pub_tool_libcbase.h"    // VG_(strcmp)
#include "pub_tool_libcprint.h"   // VG_(printf)
#include "pub_tool_libcproc.h"
#include "pub_tool_machine.h"
#include "pub_tool_mallocfree.h"  // VG_(malloc)(), VG_(free)()
#include "pub_tool_options.h"     // command line options
#include "pub_tool_replacemalloc.h"
#include "pub_tool_threadstate.h" // VG_(get_running_tid)()
#include "pub_tool_tooliface.h"


/* Local variables. */

static Bool DRD_(s_print_stats)      = False;
static Bool DRD_(s_var_info)         = False;
static Bool DRD_(s_show_stack_usage) = False;


/**
 * Implement the needs_command_line_options for drd.
 */
static Bool DRD_(process_cmd_line_option)(Char* arg)
{
  int check_stack_accesses   = -1;
  int exclusive_threshold_ms = -1;
  int segment_merging        = -1;
  int shared_threshold_ms    = -1;
  int show_confl_seg         = -1;
  int trace_barrier          = -1;
  int trace_clientobj        = -1;
  int trace_cond             = -1;
  int trace_csw              = -1;
  int trace_fork_join        = -1;
  int trace_conflict_set     = -1;
  int trace_mutex            = -1;
  int trace_rwlock           = -1;
  int trace_segment          = -1;
  int trace_semaphore        = -1;
  int trace_suppression      = -1;
  Char* trace_address        = 0;

  VG_BOOL_CLO     (arg, "--check-stack-var",     check_stack_accesses)
  else VG_BOOL_CLO(arg, "--drd-stats",           DRD_(s_print_stats))
  else VG_BOOL_CLO(arg,"--report-signal-unlocked",s_drd_report_signal_unlocked)
  else VG_BOOL_CLO(arg, "--segment-merging",     segment_merging)
  else VG_BOOL_CLO(arg, "--show-confl-seg",      show_confl_seg)
  else VG_BOOL_CLO(arg, "--show-stack-usage",    DRD_(s_show_stack_usage))
  else VG_BOOL_CLO(arg, "--trace-barrier",       trace_barrier)
  else VG_BOOL_CLO(arg, "--trace-clientobj",     trace_clientobj)
  else VG_BOOL_CLO(arg, "--trace-cond",          trace_cond)
  else VG_BOOL_CLO(arg, "--trace-conflict-set",  trace_conflict_set)
  else VG_BOOL_CLO(arg, "--trace-csw",           trace_csw)
  else VG_BOOL_CLO(arg, "--trace-fork-join",     trace_fork_join)
  else VG_BOOL_CLO(arg, "--trace-mutex",         trace_mutex)
  else VG_BOOL_CLO(arg, "--trace-rwlock",        trace_rwlock)
  else VG_BOOL_CLO(arg, "--trace-segment",       trace_segment)
  else VG_BOOL_CLO(arg, "--trace-semaphore",     trace_semaphore)
  else VG_BOOL_CLO(arg, "--trace-suppr",         trace_suppression)
  else VG_BOOL_CLO(arg, "--var-info",            DRD_(s_var_info))
  else VG_NUM_CLO (arg, "--exclusive-threshold", exclusive_threshold_ms)
  else VG_NUM_CLO (arg, "--shared-threshold",    shared_threshold_ms)
  else VG_STR_CLO (arg, "--trace-addr",          trace_address)
  else
    return VG_(replacement_malloc_process_cmd_line_option)(arg);

  if (check_stack_accesses != -1)
    DRD_(set_check_stack_accesses)(check_stack_accesses);
  if (exclusive_threshold_ms != -1)
  {
    mutex_set_lock_threshold(exclusive_threshold_ms);
    rwlock_set_exclusive_threshold(exclusive_threshold_ms);
  }
  if (shared_threshold_ms != -1)
  {
    rwlock_set_shared_threshold(shared_threshold_ms);
  }
  if (segment_merging != -1)
    thread_set_segment_merging(segment_merging);
  if (show_confl_seg != -1)
    set_show_conflicting_segments(show_confl_seg);
  if (trace_address)
  {
    const Addr addr = VG_(strtoll16)(trace_address, 0);
    DRD_(start_tracing_address_range)(addr, addr + 1);
  }
  if (trace_barrier != -1)
    DRD_(barrier_set_trace)(trace_barrier);
  if (trace_clientobj != -1)
    clientobj_set_trace(trace_clientobj);
  if (trace_cond != -1)
    cond_set_trace(trace_cond);
  if (trace_csw != -1)
    thread_trace_context_switches(trace_csw);
  if (trace_fork_join != -1)
    DRD_(thread_set_trace_fork_join)(trace_fork_join);
  if (trace_conflict_set != -1)
    thread_trace_conflict_set(trace_conflict_set);
  if (trace_mutex != -1)
    mutex_set_trace(trace_mutex);
  if (trace_rwlock != -1)
    rwlock_set_trace(trace_rwlock);
  if (trace_segment != -1)
    DRD_(sg_set_trace)(trace_segment);
  if (trace_semaphore != -1)
    semaphore_set_trace(trace_semaphore);
  if (trace_suppression != -1)
    DRD_(suppression_set_trace)(trace_suppression);

  return True;
}

static void DRD_(print_usage)(void)
{
  VG_(printf)(
"    --check-stack-var=yes|no  Whether or not to report data races on\n"
"                              stack variables [no].\n"
"    --exclusive-threshold=<n> Print an error message if any mutex or\n"
"        writer lock is held longer than the specified time (in milliseconds).\n"
"    --report-signal-unlocked=yes|no Whether to report calls to\n"
"                              pthread_cond_signal() where the mutex associated\n"
"                              with the signal via pthread_cond_wait() is not\n"
"                              locked at the time the signal is sent [yes].\n"
"    --segment-merging=yes|no  Controls segment merging [yes].\n"
"        Segment merging is an algorithm to limit memory usage of the\n"
"        data race detection algorithm. Disabling segment merging may\n"
"        improve the accuracy of the so-called 'other segments' displayed\n"
"        in race reports but can also trigger an out of memory error.\n"
"    --shared-threshold=<n>    Print an error message if a reader lock\n"
"        is held longer than the specified time (in milliseconds).\n"
"    --show-confl-seg=yes|no   Show conflicting segments in race reports [yes].\n"
"    --show-stack-usage=yes|no Print stack usage at thread exit time [no].\n"
"    --var-info=yes|no         Display the names of global, static and\n"
"        stack variables when a race is reported on such a variable. This\n"
"        information is by default not displayed since for big programs\n"
"        reading in all debug information at once may cause an out of\n"
"        memory error [no].\n"
"\n"
"  drd options for monitoring process behavior:\n"
"    --trace-addr=<address>    Trace all load and store activity for the.\n"
"                              specified address [off].\n"
"    --trace-barrier=yes|no    Trace all barrier activity [no].\n"
"    --trace-cond=yes|no       Trace all condition variable activity [no].\n"
"    --trace-fork-join=yes|no  Trace all thread fork/join activity [no].\n"
"    --trace-mutex=yes|no      Trace all mutex activity [no].\n"
"    --trace-rwlock=yes|no     Trace all reader-writer lock activity[no].\n"
"    --trace-semaphore=yes|no  Trace all semaphore activity [no].\n"
              );
   VG_(replacement_malloc_print_usage)();
}

static void DRD_(print_debug_usage)(void)
{  
  VG_(printf)(
"    --drd-stats=yes|no        Print statistics about DRD activity [no].\n"
"    --trace-clientobj=yes|no  Trace all client object activity [no].\n"
"    --trace-csw=yes|no        Trace all scheduler context switches [no].\n"
"    --trace-conflict-set=yes|no Trace all conflict set updates [no].\n"
"    --trace-segment=yes|no    Trace segment actions [no].\n"
"    --trace-suppr=yes|no      Trace all address suppression actions [no].\n"
              );
   VG_(replacement_malloc_print_debug_usage)();
}


//
// Implements the thread-related core callbacks.
//

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

static __inline__
void drd_start_using_mem(const Addr a1, const SizeT len)
{
  tl_assert(a1 < a1 + len);

  if (UNLIKELY(DRD_(any_address_is_traced)()))
  {
    DRD_(trace_mem_access)(a1, len, eStart);
  }
}

static void drd_start_using_mem_w_ecu(const Addr a1,
                                      const SizeT len,
                                      UInt ec_uniq)
{
  drd_start_using_mem(a1, len);
}

static void drd_start_using_mem_w_tid(const Addr a1,
                                      const SizeT len,
                                      ThreadId tid)
{
  drd_start_using_mem(a1, len);
}

static __inline__
void drd_stop_using_mem(const Addr a1, const SizeT len,
                        const Bool is_stack_mem)
{
  const Addr a2 = a1 + len;

  tl_assert(a1 < a2);

  if (UNLIKELY(DRD_(any_address_is_traced)()))
  {
    DRD_(trace_mem_access)(a1, len, eEnd);
  }
  if (! is_stack_mem || DRD_(get_check_stack_accesses)())
  {
    thread_stop_using_mem(a1, a2);
    clientobj_stop_using_mem(a1, a2);
    DRD_(suppression_stop_using_mem)(a1, a2);
  }
}

static __inline__
void drd_stop_using_nonstack_mem(const Addr a1, const SizeT len)
{
  drd_stop_using_mem(a1, len, False);
}

/**
 * Suppress data race reports on all addresses contained in .plt and
 * .got.plt sections inside the address range [ a, a + len [. The data in
 * these sections is modified by _dl_relocate_object() every time a function
 * in a shared library is called for the first time. Since the first call
 * to a function in a shared library can happen from a multithreaded context,
 * such calls can cause conflicting accesses. See also Ulrich Drepper's
 * paper "How to Write Shared Libraries" for more information about relocation
 * (http://people.redhat.com/drepper/dsohowto.pdf).
 */
static void DRD_(suppress_relocation_conflicts)(const Addr a, const SizeT len)
{
  const DebugInfo* di;

#if 0
  VG_(printf)("Evaluating range @ 0x%lx size %ld\n", a, len);
#endif

  for (di = VG_(next_seginfo)(0); di; di = VG_(next_seginfo)(di))
  {
    Addr  avma;
    SizeT size;

    avma = VG_(seginfo_get_plt_avma)(di);
    size = VG_(seginfo_get_plt_size)(di);
    tl_assert((avma && size) || (avma == 0 && size == 0));
    if (size > 0)
    {
#if 0
      VG_(printf)("Suppressing .plt @ 0x%lx size %ld\n", avma, size);
#endif
      tl_assert(VG_(seginfo_sect_kind)(NULL, 0, avma) == Vg_SectPLT);
      DRD_(start_suppression)(avma, avma + size, ".plt");
    }

    avma = VG_(seginfo_get_gotplt_avma)(di);
    size = VG_(seginfo_get_gotplt_size)(di);
    tl_assert((avma && size) || (avma == 0 && size == 0));
    if (size > 0)
    {
#if 0
      VG_(printf)("Suppressing .got.plt @ 0x%lx size %ld\n", avma, size);
#endif
      tl_assert(VG_(seginfo_sect_kind)(NULL, 0, avma) == Vg_SectGOTPLT);
      DRD_(start_suppression)(avma, avma + size, ".gotplt");
    }
  }
}

static
void drd_start_using_mem_w_perms(const Addr a, const SizeT len,
                                 const Bool rr, const Bool ww, const Bool xx,
                                 ULong di_handle)
{
  thread_set_vg_running_tid(VG_(get_running_tid)());

  drd_start_using_mem(a, len);

  DRD_(suppress_relocation_conflicts)(a, len);
}

/* Called by the core when the stack of a thread grows, to indicate that */
/* the addresses in range [ a, a + len [ may now be used by the client.  */
/* Assumption: stacks grow downward.                                     */
static __inline__
void drd_start_using_mem_stack(const Addr a, const SizeT len)
{
  thread_set_stack_min(thread_get_running_tid(), a - VG_STACK_REDZONE_SZB);
  drd_start_using_mem(a - VG_STACK_REDZONE_SZB, 
                      len + VG_STACK_REDZONE_SZB);
}

/* Called by the core when the stack of a thread shrinks, to indicate that */
/* the addresses [ a, a + len [ are no longer accessible for the client.   */
/* Assumption: stacks grow downward.                                       */
static __inline__
void drd_stop_using_mem_stack(const Addr a, const SizeT len)
{
  thread_set_stack_min(thread_get_running_tid(),
                       a + len - VG_STACK_REDZONE_SZB);
  drd_stop_using_mem(a - VG_STACK_REDZONE_SZB, len + VG_STACK_REDZONE_SZB,
                     True);
}

static void drd_start_using_mem_stack_signal(
               const Addr a, const SizeT len,
               ThreadId tid_for_whom_the_signal_frame_is_being_constructed)
{
  thread_set_vg_running_tid(VG_(get_running_tid)());
  drd_start_using_mem(a, len);
}

static void drd_stop_using_mem_stack_signal(Addr a, SizeT len)
{
  drd_stop_using_mem(a, len, True);
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
  if (DRD_(thread_get_trace_fork_join)())
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
void drd_post_thread_create(const ThreadId vg_created)
{
  DrdThreadId drd_created;

  tl_assert(vg_created != VG_INVALID_THREADID);

  drd_created = thread_post_create(vg_created);
  if (DRD_(thread_get_trace_fork_join)())
  {
    VG_(message)(Vg_DebugMsg,
                 "drd_post_thread_create created = %d/%d",
                 vg_created, drd_created);
  }
  if (! DRD_(get_check_stack_accesses)())
  {
    DRD_(start_suppression)(thread_get_stack_max(drd_created)
                            - thread_get_stack_size(drd_created),
                            thread_get_stack_max(drd_created),
                            "stack");
  }
}

/* Called after a thread has performed its last memory access. */
static void drd_thread_finished(ThreadId vg_tid)
{
  DrdThreadId drd_tid;

  tl_assert(VG_(get_running_tid)() == vg_tid);

  drd_tid = VgThreadIdToDrdThreadId(vg_tid);
  if (DRD_(thread_get_trace_fork_join)())
  {
    VG_(message)(Vg_DebugMsg,
                 "drd_thread_finished tid = %d/%d%s",
                 vg_tid,
                 drd_tid,
                 thread_get_joinable(drd_tid)
                 ? ""
                 : " (which is a detached thread)");
  }
  if (DRD_(s_show_stack_usage))
  {
    const SizeT stack_size = thread_get_stack_size(drd_tid);
    const SizeT used_stack
      = thread_get_stack_max(drd_tid) - thread_get_stack_min_min(drd_tid);
    VG_(message)(Vg_UserMsg,
                 "thread %d/%d%s finished and used %ld bytes out of %ld"
                 " on its stack. Margin: %ld bytes.",
                 vg_tid,
                 drd_tid,
                 thread_get_joinable(drd_tid)
                 ? ""
                 : " (which is a detached thread)",
                 used_stack,
                 stack_size,
                 stack_size - used_stack);

  }
  drd_stop_using_mem(thread_get_stack_min(drd_tid),
                     thread_get_stack_max(drd_tid)
                     - thread_get_stack_min(drd_tid),
                     True);
  thread_stop_recording(drd_tid);
  thread_finished(drd_tid);
}

//
// Implementation of the tool interface.
//

static void DRD_(post_clo_init)(void)
{
#  if defined(VGP_x86_linux) || defined(VGP_amd64_linux) \
      || defined(VGP_ppc32_linux) || defined(VGP_ppc64_linux)
  /* fine */
#  else
  VG_(printf)("\nWARNING: DRD has only been tested on Linux.\n\n");
#  endif

  if (DRD_(s_var_info))
  {
    VG_(needs_var_info)();
  }
}

static void drd_start_client_code(const ThreadId tid, const ULong bbs_done)
{
  tl_assert(tid == VG_(get_running_tid)());
  thread_set_vg_running_tid(tid);
}

static void DRD_(fini)(Int exitcode)
{
  // thread_print_all();
  if (VG_(clo_verbosity) > 1 || DRD_(s_print_stats))
  {
    ULong update_conflict_set_count;
    ULong dsnsc;
    ULong dscvc;

    update_conflict_set_count
      = thread_get_update_conflict_set_count(&dsnsc, &dscvc);

    VG_(message)(Vg_UserMsg,
                 "   thread: %lld context switches"
                 " / %lld updates of the conflict set",
                 thread_get_context_switch_count(),
                 update_conflict_set_count);
    VG_(message)(Vg_UserMsg,
                 "           (%lld new sg + %lld combine vc + %lld csw).",
                 dsnsc,
                 dscvc,
                 update_conflict_set_count - dsnsc - dscvc);
    VG_(message)(Vg_UserMsg,
                 " segments: created %lld segments, max %lld alive,"
                 " %lld discard points.",
                 DRD_(sg_get_segments_created_count)(),
                 DRD_(sg_get_max_segments_alive_count)(),
                 thread_get_discard_ordered_segments_count());
    VG_(message)(Vg_UserMsg,
                 "           (%lld m, %lld rw, %lld s, %lld b)",
                 get_mutex_segment_creation_count(),
                 get_rwlock_segment_creation_count(),
                 get_semaphore_segment_creation_count(),
                 DRD_(get_barrier_segment_creation_count)());
    VG_(message)(Vg_UserMsg,
                 "  bitmaps: %lld level 1 / %lld level 2 bitmap refs",
                 bm_get_bitmap_creation_count(),
                 bm_get_bitmap2_node_creation_count());
    VG_(message)(Vg_UserMsg,
                 "           and %lld level 2 bitmaps were allocated.",
                 bm_get_bitmap2_creation_count());
    VG_(message)(Vg_UserMsg,
                 "    mutex: %lld non-recursive lock/unlock events.",
                 get_mutex_lock_count());
    drd_print_malloc_stats();
  }
}

static
void drd_pre_clo_init(void)
{
  // Basic tool stuff.

  VG_(details_name)            ("drd");
  VG_(details_version)         (NULL);
  VG_(details_description)     ("a thread error detector");
  VG_(details_copyright_author)("Copyright (C) 2006-2008, and GNU GPL'd,"
                                " by Bart Van Assche.");
  VG_(details_bug_reports_to)  (VG_BUGS_TO);

  VG_(basic_tool_funcs)        (DRD_(post_clo_init),
                                DRD_(instrument),
                                DRD_(fini));

  // Command line stuff.
  VG_(needs_command_line_options)(DRD_(process_cmd_line_option),
                                  DRD_(print_usage),
                                  DRD_(print_debug_usage));

  // Error handling.
  DRD_(register_error_handlers)();

  // Core event tracking.
  VG_(track_pre_mem_read)         (drd_pre_mem_read);
  VG_(track_pre_mem_read_asciiz)  (drd_pre_mem_read_asciiz);
  VG_(track_post_mem_write)       (drd_post_mem_write);
  VG_(track_new_mem_brk)          (drd_start_using_mem_w_tid);
  VG_(track_new_mem_mmap)         (drd_start_using_mem_w_perms);
  VG_(track_new_mem_stack)        (drd_start_using_mem_stack);
  VG_(track_new_mem_stack_signal) (drd_start_using_mem_stack_signal);
  VG_(track_new_mem_startup)      (drd_start_using_mem_w_perms);
  VG_(track_die_mem_brk)          (drd_stop_using_nonstack_mem);
  VG_(track_die_mem_munmap)       (drd_stop_using_nonstack_mem);
  VG_(track_die_mem_stack)        (drd_stop_using_mem_stack);
  VG_(track_die_mem_stack_signal) (drd_stop_using_mem_stack_signal);
  VG_(track_start_client_code)    (drd_start_client_code);
  VG_(track_pre_thread_ll_create) (drd_pre_thread_create);
  VG_(track_pre_thread_first_insn)(drd_post_thread_create);
  VG_(track_pre_thread_ll_exit)   (drd_thread_finished);

  // Other stuff.
  drd_register_malloc_wrappers(drd_start_using_mem_w_ecu,
                               drd_stop_using_nonstack_mem);

  DRD_(clientreq_init)();

  DRD_(suppression_init)();

  clientobj_init();
}


VG_DETERMINE_INTERFACE_VERSION(drd_pre_clo_init)
