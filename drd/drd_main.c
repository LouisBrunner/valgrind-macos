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


#include "drd_barrier.h"
#include "drd_clientobj.h"
#include "drd_clientreq.h"
#include "drd_cond.h"
#include "drd_error.h"
#include "drd_hb.h"
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
#include "pub_tool_libcassert.h"  // tl_assert()
#include "pub_tool_libcbase.h"    // VG_(strcmp)
#include "pub_tool_libcprint.h"   // VG_(printf)
#include "pub_tool_libcproc.h"    // VG_(getenv)()
#include "pub_tool_machine.h"
#include "pub_tool_mallocfree.h"  // VG_(malloc)(), VG_(free)()
#include "pub_tool_options.h"     // command line options
#include "pub_tool_replacemalloc.h"
#include "pub_tool_threadstate.h" // VG_(get_running_tid)()
#include "pub_tool_tooliface.h"
#include "pub_tool_aspacemgr.h"   // VG_(am_is_valid_for_client)


/* Local variables. */

static Bool s_print_stats;
static Bool s_var_info;
static Bool s_show_stack_usage;
static Bool s_trace_alloc;
static Bool trace_sectsuppr;


/**
 * Implement the needs_command_line_options for drd.
 */
static Bool DRD_(process_cmd_line_option)(const HChar* arg)
{
   Bool check_stack_accesses   = False;
   int join_list_vol           = -1;
   int exclusive_threshold_ms  = -1;
   Bool first_race_only        = False;
   Bool report_signal_unlocked = False;
   Bool segment_merging        = False;
   int segment_merge_interval  = -1;
   int shared_threshold_ms     = -1;
   Bool show_confl_seg         = False;
   Bool trace_barrier          = False;
   Bool trace_clientobj        = False;
   Bool trace_cond             = False;
   Bool trace_csw              = False;
   Bool trace_fork_join        = False;
   Bool trace_hb               = False;
   Bool trace_conflict_set     = False;
   Bool trace_conflict_set_bm  = False;
   Bool trace_mutex            = False;
   Bool trace_rwlock           = False;
   Bool trace_segment          = False;
   Bool trace_semaphore        = False;
   Bool trace_suppression      = False;
   const HChar* trace_address = 0;
   const HChar* ptrace_address= 0;

   if      VG_BOOL_CLO(arg, "--check-stack-var",     check_stack_accesses) {
      DRD_(set_check_stack_accesses)(check_stack_accesses);
   }
   else if VG_INT_CLO (arg, "--join-list-vol",       join_list_vol) {}
   else if VG_BOOL_CLO(arg, "--drd-stats",           s_print_stats) {}
   else if VG_BOOL_CLO(arg, "--first-race-only",     first_race_only) {
      DRD_(set_first_race_only)(first_race_only);
   }
   else if VG_BOOL_CLO(arg, "--free-is-write",       DRD_(g_free_is_write)) {}
   else if VG_BOOL_CLO(arg,"--report-signal-unlocked",report_signal_unlocked) {
      DRD_(cond_set_report_signal_unlocked)(report_signal_unlocked);
   }
   else if VG_BOOL_CLO(arg, "--segment-merging",     segment_merging) {
      DRD_(thread_set_segment_merging)(segment_merging);
   }
   else if VG_INT_CLO (arg, "--segment-merging-interval", segment_merge_interval)
   {}
   else if VG_BOOL_CLO(arg, "--show-confl-seg",      show_confl_seg) {
      DRD_(set_show_conflicting_segments)(show_confl_seg);
   }
   else if VG_BOOL_CLO(arg, "--show-stack-usage",    s_show_stack_usage) {}
   else if VG_BOOL_CLO(arg, "--ignore-thread-creation",
   DRD_(ignore_thread_creation)) {}
   else if VG_BOOL_CLO(arg, "--trace-alloc",         s_trace_alloc) {}
   else if VG_BOOL_CLO(arg, "--trace-barrier",       trace_barrier) {
      DRD_(barrier_set_trace)(trace_barrier);
   }
   else if VG_BOOL_CLO(arg, "--trace-clientobj",     trace_clientobj) {
      DRD_(clientobj_set_trace)(trace_clientobj);
   }
   else if VG_BOOL_CLO(arg, "--trace-cond",          trace_cond) {
      DRD_(cond_set_trace)(trace_cond);
   }
   else if VG_BOOL_CLO(arg, "--trace-conflict-set",  trace_conflict_set) {
      DRD_(thread_trace_conflict_set)(trace_conflict_set);
   }
   else if VG_BOOL_CLO(arg, "--trace-conflict-set-bm", trace_conflict_set_bm){
      DRD_(thread_trace_conflict_set_bm)(trace_conflict_set_bm);
   }
   else if VG_BOOL_CLO(arg, "--trace-csw",           trace_csw) {
      DRD_(thread_trace_context_switches)(trace_csw);
   }
   else if VG_BOOL_CLO(arg, "--trace-fork-join",     trace_fork_join) {
      DRD_(thread_set_trace_fork_join)(trace_fork_join);
   }
   else if VG_BOOL_CLO(arg, "--trace-hb",            trace_hb) {
      DRD_(hb_set_trace)(trace_hb);
   }
   else if VG_BOOL_CLO(arg, "--trace-mutex",         trace_mutex) {
      DRD_(mutex_set_trace)(trace_mutex);
   }
   else if VG_BOOL_CLO(arg, "--trace-rwlock",        trace_rwlock) {
      DRD_(rwlock_set_trace)(trace_rwlock);
   }
   else if VG_BOOL_CLO(arg, "--trace-sectsuppr",     trace_sectsuppr) {}
   else if VG_BOOL_CLO(arg, "--trace-segment",       trace_segment) {
      DRD_(sg_set_trace)(trace_segment);
   }
   else if VG_BOOL_CLO(arg, "--trace-semaphore",     trace_semaphore) {
      DRD_(semaphore_set_trace)(trace_semaphore);
   }
   else if VG_BOOL_CLO(arg, "--trace-suppr",         trace_suppression) {
      DRD_(suppression_set_trace)(trace_suppression);
   }
   else if VG_BOOL_CLO(arg, "--var-info",            s_var_info) {}
   else if VG_BOOL_CLO(arg, "--verify-conflict-set", DRD_(verify_conflict_set))
   {}
   else if VG_INT_CLO (arg, "--exclusive-threshold", exclusive_threshold_ms) {}
   else if VG_STR_CLO (arg, "--ptrace-addr",         ptrace_address) {}
   else if VG_INT_CLO (arg, "--shared-threshold",    shared_threshold_ms)    {}
   else if VG_STR_CLO (arg, "--trace-addr",          trace_address) {}
   else
      return VG_(replacement_malloc_process_cmd_line_option)(arg);

   if (exclusive_threshold_ms != -1)
   {
      DRD_(mutex_set_lock_threshold)(exclusive_threshold_ms);
      DRD_(rwlock_set_exclusive_threshold)(exclusive_threshold_ms);
   }
   if (join_list_vol != -1)
      DRD_(thread_set_join_list_vol)(join_list_vol);
   if (shared_threshold_ms != -1)
   {
      DRD_(rwlock_set_shared_threshold)(shared_threshold_ms);
   }
   if (segment_merge_interval != -1)
      DRD_(thread_set_segment_merge_interval)(segment_merge_interval);
   if (trace_address) {
      const Addr addr = VG_(strtoll16)(trace_address, 0);
      DRD_(start_tracing_address_range)(addr, addr + 1, False);
   }
   if (ptrace_address) {
      HChar *plus = VG_(strchr)(ptrace_address, '+');
      Addr addr, length;
      if (plus)
         *plus = '\0';
      addr = VG_(strtoll16)(ptrace_address, 0);
      length = plus ? VG_(strtoll16)(plus + 1, 0) : 1;
      DRD_(start_tracing_address_range)(addr, addr + length, True);
   }

   return True;
}

static void DRD_(print_usage)(void)
{
   VG_(printf)(
"    --check-stack-var=yes|no  Whether or not to report data races on\n"
"                              stack variables [no].\n"
"    --exclusive-threshold=<n> Print an error message if any mutex or\n"
"                              writer lock is held longer than the specified\n"
"                              time (in milliseconds) [off].\n"
"    --first-race-only=yes|no  Only report the first data race that occurs on\n"
"                              a memory location instead of all races [no].\n"
"    --free-is-write=yes|no    Whether to report races between freeing memory\n"
"                              and subsequent accesses of that memory[no].\n"
"    --join-list-vol=<n>       Number of threads to delay cleanup for [10].\n"
"    --report-signal-unlocked=yes|no Whether to report calls to\n"
"                              pthread_cond_signal() where the mutex associated\n"
"                              with the signal via pthread_cond_wait() is not\n"
"                              locked at the time the signal is sent [yes].\n"
"    --segment-merging=yes|no  Controls segment merging [yes].\n"
"        Segment merging is an algorithm to limit memory usage of the\n"
"        data race detection algorithm. Disabling segment merging may\n"
"        improve the accuracy of the so-called 'other segments' displayed\n"
"        in race reports but can also trigger an out of memory error.\n"
"    --segment-merging-interval=<n> Perform segment merging every time n new\n"
"        segments have been created. Default: %d.\n"
"    --shared-threshold=<n>    Print an error message if a reader lock\n"
"                              is held longer than the specified time (in\n"
"                              milliseconds) [off]\n"
"    --show-confl-seg=yes|no   Show conflicting segments in race reports [yes].\n"
"    --show-stack-usage=yes|no Print stack usage at thread exit time [no].\n"
"    --ignore-thread-creation=yes|no Ignore activities during thread \n"
"                              creation [%s].\n"
"\n"
"  drd options for monitoring process behavior:\n"
"    --ptrace-addr=<address>[+<length>] Trace all load and store activity for\n"
"                              the specified address range and keep doing that\n"
"                              even after the memory at that address has been\n"
"                              freed and reallocated [off].\n"
"    --trace-addr=<address>    Trace all load and store activity for the\n"
"                              specified address [off].\n"
"    --trace-alloc=yes|no      Trace all memory allocations and deallocations\n"
"                              [no].\n"
"    --trace-barrier=yes|no    Trace all barrier activity [no].\n"
"    --trace-cond=yes|no       Trace all condition variable activity [no].\n"
"    --trace-fork-join=yes|no  Trace all thread fork/join activity [no].\n"
"    --trace-hb=yes|no         Trace ANNOTATE_HAPPENS_BEFORE() etc. [no].\n"
"    --trace-mutex=yes|no      Trace all mutex activity [no].\n"
"    --trace-rwlock=yes|no     Trace all reader-writer lock activity[no].\n"
"    --trace-semaphore=yes|no  Trace all semaphore activity [no].\n",
DRD_(thread_get_segment_merge_interval)(),
DRD_(ignore_thread_creation) ? "yes" : "no"
);
}

static void DRD_(print_debug_usage)(void)
{
   VG_(printf)(
"    --drd-stats=yes|no        Print statistics about DRD activity [no].\n"
"    --trace-clientobj=yes|no  Trace all client object activity [no].\n"
"    --trace-csw=yes|no        Trace all scheduler context switches [no].\n"
"    --trace-conflict-set=yes|no Trace all conflict set updates [no].\n"
"    --trace-conflict-set-bm=yes|no Trace all conflict set bitmap\n"
"                              updates [no]. Note: enabling this option\n"
"                              will generate a lot of output !\n"
"    --trace-sectsuppr=yes|no  Trace which the dynamic library sections on\n"
"                              which data race detection is suppressed.\n"
"    --trace-segment=yes|no    Trace segment actions [no].\n"
"    --trace-suppr=yes|no      Trace all address suppression actions [no].\n"
"    --verify-conflict-set=yes|no Verify conflict set consistency [no].\n"
);
}


//
// Implements the thread-related core callbacks.
//

static void drd_pre_mem_read(const CorePart part,
                             const ThreadId tid,
                             const HChar* const s,
                             const Addr a,
                             const SizeT size)
{
   DRD_(thread_set_vg_running_tid)(VG_(get_running_tid)());
   if (size > 0)
   {
      DRD_(trace_load)(a, size);
   }
}

static void drd_pre_mem_read_asciiz(const CorePart part,
                                    const ThreadId tid,
                                    const HChar* const s,
                                    const Addr a)
{
   const HChar* p = (void*)a;
   SizeT size = 0;

   // Don't segfault if the string starts in an obviously stupid
   // place.  Actually we should check the whole string, not just
   // the start address, but that's too much trouble.  At least
   // checking the first byte is better than nothing.  See #255009.
   if (!VG_(am_is_valid_for_client) (a, 1, VKI_PROT_READ))
      return;

   /* Note: the expression '*p' reads client memory and may crash if the */
   /* client provided an invalid pointer !                               */
   while (*p)
   {
      p++;
      size++;
   }
   if (size > 0)
   {
      DRD_(trace_load)(a, size);
   }
}

static void drd_post_mem_write(const CorePart part,
                               const ThreadId tid,
                               const Addr a,
                               const SizeT size)
{
   DRD_(thread_set_vg_running_tid)(VG_(get_running_tid)());
   if (size > 0)
   {
      DRD_(trace_store)(a, size);
   }
}

static __inline__
void drd_start_using_mem(const Addr a1, const SizeT len,
                         const Bool is_stack_mem)
{
   const Addr a2 = a1 + len;

   tl_assert(a1 <= a2);

   if (!is_stack_mem && s_trace_alloc)
      DRD_(trace_msg)("Started using memory range 0x%lx + %lu%s",
                      a1, len, DRD_(running_thread_inside_pthread_create)()
                      ? " (inside pthread_create())" : "");

   if (!is_stack_mem && DRD_(g_free_is_write))
      DRD_(thread_stop_using_mem)(a1, a2);

   if (UNLIKELY(DRD_(any_address_is_traced)()))
   {
      DRD_(trace_mem_access)(a1, len, eStart, 0, 0);
   }

   if (UNLIKELY(DRD_(running_thread_inside_pthread_create)()))
   {
      DRD_(start_suppression)(a1, a2, "pthread_create()");
   }
}

static void drd_start_using_mem_w_ecu(const Addr a1,
                                      const SizeT len,
                                      UInt ec_uniq)
{
   drd_start_using_mem(a1, len, False);
}

static void drd_start_using_mem_w_tid(const Addr a1,
                                      const SizeT len,
                                      ThreadId tid)
{
   drd_start_using_mem(a1, len, False);
}

static __inline__
void drd_stop_using_mem(const Addr a1, const SizeT len,
                        const Bool is_stack_mem)
{
   const Addr a2 = a1 + len;

   tl_assert(a1 <= a2);

   if (UNLIKELY(DRD_(any_address_is_traced)()))
      DRD_(trace_mem_access)(a1, len, eEnd, 0, 0);

   if (!is_stack_mem && s_trace_alloc)
      DRD_(trace_msg)("Stopped using memory range 0x%lx + %lu",
                      a1, len);

   if (!is_stack_mem || DRD_(get_check_stack_accesses)())
   {
      if (is_stack_mem || !DRD_(g_free_is_write))
	 DRD_(thread_stop_using_mem)(a1, a2);
      else if (DRD_(g_free_is_write))
	 DRD_(trace_store)(a1, len);
      DRD_(clientobj_stop_using_mem)(a1, a2);
      DRD_(suppression_stop_using_mem)(a1, a2);
   }
}

static __inline__
void drd_stop_using_nonstack_mem(const Addr a1, const SizeT len)
{
   drd_stop_using_mem(a1, len, False);
}

/**
 * Discard all information DRD has about memory accesses and client objects
 * in the specified address range.
 */
void DRD_(clean_memory)(const Addr a1, const SizeT len)
{
   const Bool is_stack_memory = DRD_(thread_address_on_any_stack)(a1);
   drd_stop_using_mem(a1, len, is_stack_memory);
   drd_start_using_mem(a1, len, is_stack_memory);
}

/**
 * Suppress data race reports on all addresses contained in .plt, .got and
 * .got.plt sections inside the address range [ a, a + len [. The data in
 * these sections is modified by _dl_relocate_object() every time a function
 * in a shared library is called for the first time. Since the first call
 * to a function in a shared library can happen from a multithreaded context,
 * such calls can cause conflicting accesses. See also Ulrich Drepper's
 * paper "How to Write Shared Libraries" for more information about relocation
 * (http://people.redhat.com/drepper/dsohowto.pdf).
 * Note: the contents of the .got section is only modified by the MIPS resolver.
 */
static void DRD_(suppress_relocation_conflicts)(const Addr a, const SizeT len)
{
   const DebugInfo* di;

   if (trace_sectsuppr)
      VG_(dmsg)("Evaluating range @ 0x%lx size %lu\n", a, len);

   for (di = VG_(next_DebugInfo)(0); di; di = VG_(next_DebugInfo)(di)) {
      Addr  avma;
      SizeT size;

      if (trace_sectsuppr)
	 VG_(dmsg)("Examining %s / %s\n", VG_(DebugInfo_get_filename)(di),
		   VG_(DebugInfo_get_soname)(di));

      /*
       * Suppress the race report on the libpthread global variable
       * __pthread_multiple_threads. See also
       * http://bugs.kde.org/show_bug.cgi?id=323905.
       */
      avma = VG_(DebugInfo_get_bss_avma)(di);
      size = VG_(DebugInfo_get_bss_size)(di);
      tl_assert((avma && size) || (avma == 0 && size == 0));
      if (size > 0 &&
          VG_(strcmp)(VG_(DebugInfo_get_soname)(di), "libpthread.so.0") == 0) {
	 if (trace_sectsuppr)
	    VG_(dmsg)("Suppressing .bss @ 0x%lx size %lu\n", avma, size);
         tl_assert(VG_(DebugInfo_sect_kind)(NULL, avma) == Vg_SectBSS);
         DRD_(start_suppression)(avma, avma + size, ".bss");
      }

      avma = VG_(DebugInfo_get_plt_avma)(di);
      size = VG_(DebugInfo_get_plt_size)(di);
      tl_assert((avma && size) || (avma == 0 && size == 0));
      if (size > 0) {
	 if (trace_sectsuppr)
	    VG_(dmsg)("Suppressing .plt @ 0x%lx size %lu\n", avma, size);
         tl_assert(VG_(DebugInfo_sect_kind)(NULL, avma) == Vg_SectPLT);
         DRD_(start_suppression)(avma, avma + size, ".plt");
      }

      avma = VG_(DebugInfo_get_gotplt_avma)(di);
      size = VG_(DebugInfo_get_gotplt_size)(di);
      tl_assert((avma && size) || (avma == 0 && size == 0));
      if (size > 0) {
	 if (trace_sectsuppr)
	    VG_(dmsg)("Suppressing .got.plt @ 0x%lx size %lu\n", avma, size);
         tl_assert(VG_(DebugInfo_sect_kind)(NULL, avma) == Vg_SectGOTPLT);
         DRD_(start_suppression)(avma, avma + size, ".gotplt");
      }

      avma = VG_(DebugInfo_get_got_avma)(di);
      size = VG_(DebugInfo_get_got_size)(di);
      tl_assert((avma && size) || (avma == 0 && size == 0));
      if (size > 0) {
	 if (trace_sectsuppr)
	    VG_(dmsg)("Suppressing .got @ 0x%lx size %lu\n", avma, size);
         tl_assert(VG_(DebugInfo_sect_kind)(NULL, avma) == Vg_SectGOT);
         DRD_(start_suppression)(avma, avma + size, ".got");
      }
   }
}

static
void drd_start_using_mem_w_perms(const Addr a, const SizeT len,
                                 const Bool rr, const Bool ww, const Bool xx,
                                 ULong di_handle)
{
   DRD_(thread_set_vg_running_tid)(VG_(get_running_tid)());

   drd_start_using_mem(a, len, False);

   DRD_(suppress_relocation_conflicts)(a, len);
}

/**
 * Called by the core when the stack of a thread grows, to indicate that
 * the addresses in range [ a, a + len [ may now be used by the client.
 * Assumption: stacks grow downward.
 */
static __inline__
void drd_start_using_mem_stack2(const DrdThreadId tid, const Addr a,
                                const SizeT len)
{
   DRD_(thread_set_stack_min)(tid, a - VG_STACK_REDZONE_SZB);
   drd_start_using_mem(a - VG_STACK_REDZONE_SZB, len + VG_STACK_REDZONE_SZB,
                       True);
}

static __inline__
void drd_start_using_mem_stack(const Addr a, const SizeT len)
{
   drd_start_using_mem_stack2(DRD_(thread_get_running_tid)(), a, len);
}

/**
 * Called by the core when the stack of a thread shrinks, to indicate that
 * the addresses [ a, a + len [ are no longer accessible for the client.
 * Assumption: stacks grow downward.
 */
static __inline__
void drd_stop_using_mem_stack2(const DrdThreadId tid, const Addr a,
                               const SizeT len)
{
   DRD_(thread_set_stack_min)(tid, a + len - VG_STACK_REDZONE_SZB);
   drd_stop_using_mem(a - VG_STACK_REDZONE_SZB, len + VG_STACK_REDZONE_SZB,
                      True);
}

static __inline__
void drd_stop_using_mem_stack(const Addr a, const SizeT len)
{
   drd_stop_using_mem_stack2(DRD_(thread_get_running_tid)(), a, len);
}

static
Bool on_alt_stack(const Addr a)
{
   ThreadId vg_tid;
   Addr alt_min;
   SizeT alt_size;

   vg_tid = VG_(get_running_tid)();
   alt_min = VG_(thread_get_altstack_min)(vg_tid);
   alt_size = VG_(thread_get_altstack_size)(vg_tid);
   return (SizeT)(a - alt_min) < alt_size;
}

static
void drd_start_using_mem_alt_stack(const Addr a, const SizeT len)
{
   if (!on_alt_stack(a))
      drd_start_using_mem_stack(a, len);
}

static
void drd_stop_using_mem_alt_stack(const Addr a, const SizeT len)
{
   if (!on_alt_stack(a))
      drd_stop_using_mem_stack(a, len);
}

/**
 * Callback function invoked by the Valgrind core before a signal is delivered.
 */
static
void drd_pre_deliver_signal(const ThreadId vg_tid, const Int sigNo,
                            const Bool alt_stack)
{
   DrdThreadId drd_tid;

   drd_tid = DRD_(VgThreadIdToDrdThreadId)(vg_tid);
   DRD_(thread_set_on_alt_stack)(drd_tid, alt_stack);
   if (alt_stack)
   {
      /*
       * As soon a signal handler has been invoked on the alternate stack,
       * switch to stack memory handling functions that can handle the
       * alternate stack.
       */
      VG_(track_new_mem_stack)(drd_start_using_mem_alt_stack);
      VG_(track_die_mem_stack)(drd_stop_using_mem_alt_stack);
   }
}

/**
 * Callback function invoked by the Valgrind core after a signal is delivered,
 * at least if the signal handler did not longjmp().
 */
static
void drd_post_deliver_signal(const ThreadId vg_tid, const Int sigNo)
{
   DrdThreadId drd_tid;

   drd_tid = DRD_(VgThreadIdToDrdThreadId)(vg_tid);
   DRD_(thread_set_on_alt_stack)(drd_tid, False);
   if (DRD_(thread_get_threads_on_alt_stack)() == 0)
   {
      VG_(track_new_mem_stack)(drd_start_using_mem_stack);
      VG_(track_die_mem_stack)(drd_stop_using_mem_stack);
   }
}

/**
 * Callback function called by the Valgrind core before a stack area is
 * being used by a signal handler.
 *
 * @param[in] a   Start of address range - VG_STACK_REDZONE_SZB.
 * @param[in] len Address range length + VG_STACK_REDZONE_SZB.
 * @param[in] tid Valgrind thread ID for whom the signal frame is being
 *                constructed.
 */
static void drd_start_using_mem_stack_signal(const Addr a, const SizeT len,
                                             ThreadId tid)
{
   DRD_(thread_set_vg_running_tid)(VG_(get_running_tid)());
   drd_start_using_mem(a + VG_STACK_REDZONE_SZB, len - VG_STACK_REDZONE_SZB,
                       True);
}

static void drd_stop_using_mem_stack_signal(Addr a, SizeT len)
{
   drd_stop_using_mem(a + VG_STACK_REDZONE_SZB, len - VG_STACK_REDZONE_SZB,
                      True);
}

static void drd_register_stack(Addr start, Addr end)
{
   DrdThreadId drd_tid = DRD_(thread_get_running_tid)();

   DRD_(thread_register_stack)(drd_tid, start, end);
}

static
void drd_pre_thread_create(const ThreadId creator, const ThreadId created)
{
   const DrdThreadId drd_creator = DRD_(VgThreadIdToDrdThreadId)(creator);
   tl_assert(created != VG_INVALID_THREADID);
   DRD_(thread_pre_create)(drd_creator, created);
   if (DRD_(IsValidDrdThreadId)(drd_creator))
   {
      DRD_(thread_new_segment)(drd_creator);
   }
   if (DRD_(thread_get_trace_fork_join)())
   {
      DRD_(trace_msg)("drd_pre_thread_create creator = %u, created = %u",
                      drd_creator, created);
   }
}

/**
 * Called by Valgrind's core before any loads or stores are performed on
 * the context of thread "created".
 */
static
void drd_post_thread_create(const ThreadId vg_created)
{
   DrdThreadId drd_created;
   Addr stack_max;

   tl_assert(vg_created != VG_INVALID_THREADID);

   drd_created = DRD_(thread_post_create)(vg_created);

   /* Set up red zone before the code in glibc's clone.S is run. */
   stack_max = DRD_(thread_get_stack_max)(drd_created);
   drd_start_using_mem_stack2(drd_created, stack_max, 0);

   if (DRD_(thread_get_trace_fork_join)())
   {
      DRD_(trace_msg)("drd_post_thread_create created = %u", drd_created);
   }
   if (! DRD_(get_check_stack_accesses)())
   {
      DRD_(start_suppression)(DRD_(thread_get_stack_max)(drd_created)
                              - DRD_(thread_get_stack_size)(drd_created),
                              DRD_(thread_get_stack_max)(drd_created),
                              "stack");
   }
}

/* Called after a thread has performed its last memory access. */
static void drd_thread_finished(ThreadId vg_tid)
{
   DrdThreadId drd_tid;

   /*
    * Ignore if invoked because thread creation failed. See e.g.
    * coregrind/m_syswrap/syswrap-amd64-linux.c
    */
   if (VG_(get_running_tid)() != vg_tid)
      return;

   drd_tid = DRD_(VgThreadIdToDrdThreadId)(vg_tid);
   tl_assert(drd_tid != DRD_INVALID_THREADID);
   if (DRD_(thread_get_trace_fork_join)())
   {
      DRD_(trace_msg)("drd_thread_finished tid = %u%s", drd_tid,
                      DRD_(thread_get_joinable)(drd_tid)
                      ? "" : " (which is a detached thread)");
   }
   if (s_show_stack_usage && !VG_(clo_xml)) {
      const SizeT stack_size = DRD_(thread_get_stack_size)(drd_tid);
      const SizeT used_stack
         = (DRD_(thread_get_stack_max)(drd_tid)
            - DRD_(thread_get_stack_min_min)(drd_tid));
      VG_(message)(Vg_UserMsg,
                   "thread %u%s finished and used %lu bytes out of %lu on its stack. Margin: %ld bytes.\n",
                   drd_tid,
                   DRD_(thread_get_joinable)(drd_tid)
                   ? "" : " (which is a detached thread)",
                   used_stack, stack_size, (long)(stack_size - used_stack));

   }
   drd_stop_using_mem(DRD_(thread_get_stack_min)(drd_tid),
                      DRD_(thread_get_stack_max)(drd_tid)
                      - DRD_(thread_get_stack_min)(drd_tid),
                      True);
   DRD_(thread_set_record_loads)(drd_tid, False);
   DRD_(thread_set_record_stores)(drd_tid, False);
   DRD_(thread_finished)(drd_tid);
}

/*
 * Called immediately after fork for the child process only. 'tid' is the
 * only surviving thread in the child process. Cleans up thread state.
 * See also http://pubs.opengroup.org/onlinepubs/9699919799/functions/pthread_atfork.html for a detailed discussion of using fork() in combination with mutexes.
 */
static
void drd__atfork_child(ThreadId tid)
{
   DRD_(drd_thread_atfork_child)(tid);
}


//
// Implementation of the tool interface.
//

static void DRD_(post_clo_init)(void)
{
#if defined(VGO_linux) || defined(VGO_darwin) || defined(VGO_solaris) || defined(VGO_freebsd)
   /* fine */
#else
   VG_(printf)("\nWARNING: DRD has not yet been tested on this operating system.\n\n");
#  endif

   if (s_var_info)
   {
      VG_(needs_var_info)();
   }
}

static void drd_start_client_code(const ThreadId tid, const ULong bbs_done)
{
   tl_assert(tid == VG_(get_running_tid)());
   DRD_(thread_set_vg_running_tid)(tid);
}

static void DRD_(fini)(Int exitcode)
{
   // thread_print_all();

   if ((VG_(clo_stats) || s_print_stats) && !VG_(clo_xml))
   {
      ULong pu = DRD_(thread_get_update_conflict_set_count)();
      ULong pu_seg_cr = DRD_(thread_get_update_conflict_set_new_sg_count)();
      ULong pu_mtx_cv = DRD_(thread_get_update_conflict_set_sync_count)();
      ULong pu_join   = DRD_(thread_get_update_conflict_set_join_count)();

      VG_(message)(Vg_UserMsg,
                   "   thread: %llu context switches.\n",
                   DRD_(thread_get_context_switch_count)());
      VG_(message)(Vg_UserMsg,
                   "confl set: %llu full updates and %llu partial updates;\n",
                   DRD_(thread_get_compute_conflict_set_count)(),
                   pu);
      VG_(message)(Vg_UserMsg,
                   "           %llu partial updates during segment creation,\n",
                   pu_seg_cr);
      VG_(message)(Vg_UserMsg,
                   "           %llu because of mutex/sema/cond.var. operations,\n",
                   pu_mtx_cv);
      VG_(message)(Vg_UserMsg,
                   "           %llu because of barrier/rwlock operations and\n",
		   pu - pu_seg_cr - pu_mtx_cv - pu_join);
      VG_(message)(Vg_UserMsg,
                   "           %llu partial updates because of thread join"
                   " operations.\n",
                   pu_join);
      VG_(message)(Vg_UserMsg,
                   " segments: created %llu segments, max %llu alive,\n",
                   DRD_(sg_get_segments_created_count)(),
                   DRD_(sg_get_max_segments_alive_count)());
      VG_(message)(Vg_UserMsg,
                   "           %llu discard points and %llu merges.\n",
                   DRD_(thread_get_discard_ordered_segments_count)(),
                   DRD_(sg_get_segment_merge_count)());
      VG_(message)(Vg_UserMsg,
                   "segmnt cr: %llu mutex, %llu rwlock, %llu semaphore and"
                   " %llu barrier.\n",
                   DRD_(get_mutex_segment_creation_count)(),
                   DRD_(get_rwlock_segment_creation_count)(),
                   DRD_(get_semaphore_segment_creation_count)(),
                   DRD_(get_barrier_segment_creation_count)());
      VG_(message)(Vg_UserMsg,
                   "  bitmaps: %llu level one"
                   " and %llu level two bitmaps were allocated.\n",
                   DRD_(bm_get_bitmap_creation_count)(),
                   DRD_(bm_get_bitmap2_creation_count)());
      VG_(message)(Vg_UserMsg,
                   "    mutex: %llu non-recursive lock/unlock events.\n",
                   DRD_(get_mutex_lock_count)());
      DRD_(print_malloc_stats)();
   }

   DRD_(bm_module_cleanup)();
}

static
void drd_pre_clo_init(void)
{
   // Basic tool stuff.
   VG_(details_name)            ("drd");
   VG_(details_version)         (NULL);
   VG_(details_description)     ("a thread error detector");
   VG_(details_copyright_author)("Copyright (C) 2006-2020, and GNU GPL'd,"
                                 " by Bart Van Assche.");
   VG_(details_bug_reports_to)  (VG_BUGS_TO);

   VG_(basic_tool_funcs)        (DRD_(post_clo_init),
                                 DRD_(instrument),
                                 DRD_(fini));

   // Command line stuff.
   VG_(needs_command_line_options)(DRD_(process_cmd_line_option),
                                   DRD_(print_usage),
                                   DRD_(print_debug_usage));
   VG_(needs_xml_output)          ();

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
   VG_(track_register_stack)       (drd_register_stack);
   VG_(track_pre_deliver_signal)   (drd_pre_deliver_signal);
   VG_(track_post_deliver_signal)  (drd_post_deliver_signal);
   VG_(track_start_client_code)    (drd_start_client_code);
   VG_(track_pre_thread_ll_create) (drd_pre_thread_create);
   VG_(track_pre_thread_first_insn)(drd_post_thread_create);
   VG_(track_pre_thread_ll_exit)   (drd_thread_finished);
   VG_(atfork)                     (NULL/*pre*/, NULL/*parent*/,
				    drd__atfork_child/*child*/);

   // Other stuff.
   DRD_(register_malloc_wrappers)(drd_start_using_mem_w_ecu,
                                  drd_stop_using_nonstack_mem);

   DRD_(bm_module_init)();

   DRD_(clientreq_init)();

   DRD_(suppression_init)();

   DRD_(clientobj_init)();

   DRD_(thread_init)();

   {
      HChar* const smi = VG_(getenv)("DRD_SEGMENT_MERGING_INTERVAL");
      if (smi)
         DRD_(thread_set_segment_merge_interval)(VG_(strtoll10)(smi, NULL));
   }

   if (VG_(getenv)("DRD_VERIFY_CONFLICT_SET"))
      DRD_(verify_conflict_set) = True;

}


VG_DETERMINE_INTERFACE_VERSION(drd_pre_clo_init)
