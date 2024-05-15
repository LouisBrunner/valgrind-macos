
/*--------------------------------------------------------------------*/
/*--- Thread scheduling.                               scheduler.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2017 Julian Seward 
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
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.
*/

/* 
   Overview

   Valgrind tries to emulate the kernel's threading as closely as
   possible.  The client does all threading via the normal syscalls
   (on Linux: clone, etc).  Valgrind emulates this by creating exactly
   the same process structure as would be created without Valgrind.
   There are no extra threads.

   The main difference is that Valgrind only allows one client thread
   to run at once.  This is controlled with the CPU Big Lock,
   "the_BigLock".  Any time a thread wants to run client code or
   manipulate any shared state (which is anything other than its own
   ThreadState entry), it must hold the_BigLock.

   When a thread is about to block in a blocking syscall, it releases
   the_BigLock, and re-takes it when it becomes runnable again (either
   because the syscall finished, or we took a signal).

   VG_(scheduler) therefore runs in each thread.  It returns only when
   the thread is exiting, either because it exited itself, or it was
   told to exit by another thread.

   This file is almost entirely OS-independent.  The details of how
   the OS handles threading and signalling are abstracted away and
   implemented elsewhere.  [Some of the functions have worked their
   way back for the moment, until we do an OS port in earnest...]
*/


#include "pub_core_basics.h"
#include "pub_core_debuglog.h"
#include "pub_core_vki.h"
#include "pub_core_vkiscnums.h"  // __NR_sched_yield
#include "pub_core_threadstate.h"
#include "pub_core_clientstate.h"
#include "pub_core_aspacemgr.h"
#include "pub_core_clreq.h"      // for VG_USERREQ__*
#include "pub_core_dispatch.h"
#include "pub_core_errormgr.h"   // For VG_(get_n_errs_found)()
#include "pub_core_extension.h"
#include "pub_core_gdbserver.h"  // for VG_(gdbserver)/VG_(gdbserver_activity)
#include "pub_core_libcbase.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcprint.h"
#include "pub_core_libcproc.h"
#include "pub_core_libcsignal.h"
#if defined(VGO_darwin)
#include "pub_core_mach.h"
#endif
#include "pub_core_machine.h"
#include "pub_core_mallocfree.h"
#include "pub_core_options.h"
#include "pub_core_replacemalloc.h"
#include "pub_core_sbprofile.h"
#include "pub_core_signals.h"
#include "pub_core_stacks.h"
#include "pub_core_stacktrace.h"    // For VG_(get_and_pp_StackTrace)()
#include "pub_core_syscall.h"
#include "pub_core_syswrap.h"
#include "pub_core_tooliface.h"
#include "pub_core_translate.h"     // For VG_(translate)()
#include "pub_core_transtab.h"
#include "pub_core_debuginfo.h"     // VG_(di_notify_pdb_debuginfo)
#include "priv_sched-lock.h"
#include "pub_core_scheduler.h"     // self
#include "pub_core_redir.h"
#include "libvex_emnote.h"          // VexEmNote


/* ---------------------------------------------------------------------
   Types and globals for the scheduler.
   ------------------------------------------------------------------ */

/* ThreadId and ThreadState are defined elsewhere*/

/* If False, a fault is Valgrind-internal (ie, a bug) */
Bool VG_(in_generated_code) = False;

/* 64-bit counter for the number of basic blocks done. */
static ULong bbs_done = 0;

/* Counter to see if vgdb activity is to be verified.
   When nr of bbs done reaches vgdb_next_poll, scheduler will
   poll for gdbserver activity. VG_(force_vgdb_poll) and 
   VG_(disable_vgdb_poll) allows the valgrind core (e.g. m_gdbserver)
   to control when the next poll will be done. */
static ULong vgdb_next_poll;

/* Forwards */
static void do_client_request ( ThreadId tid );
static void scheduler_sanity ( ThreadId tid );
static void mostly_clear_thread_record ( ThreadId tid );

/* Stats. */
static ULong n_scheduling_events_MINOR = 0;
static ULong n_scheduling_events_MAJOR = 0;

/* Stats: number of XIndirs looked up in the fast cache, the number of hits in
   ways 1, 2 and 3, and the number of misses.  The number of hits in way 0 isn't
   recorded because it can be computed from these five numbers. */
static ULong stats__n_xIndirs = 0;
static ULong stats__n_xIndir_hits1 = 0;
static ULong stats__n_xIndir_hits2 = 0;
static ULong stats__n_xIndir_hits3 = 0;
static ULong stats__n_xIndir_misses = 0;

/* And 32-bit temp bins for the above, so that 32-bit platforms don't
   have to do 64 bit incs on the hot path through
   VG_(disp_cp_xindir). */
/*global*/ UInt VG_(stats__n_xIndirs_32) = 0;
/*global*/ UInt VG_(stats__n_xIndir_hits1_32) = 0;
/*global*/ UInt VG_(stats__n_xIndir_hits2_32) = 0;
/*global*/ UInt VG_(stats__n_xIndir_hits3_32) = 0;
/*global*/ UInt VG_(stats__n_xIndir_misses_32) = 0;

/* Sanity checking counts. */
static UInt sanity_fast_count = 0;
static UInt sanity_slow_count = 0;

void VG_(print_scheduler_stats)(void)
{
   VG_(message)(Vg_DebugMsg,
      "scheduler: %'llu event checks.\n", bbs_done );

   const ULong hits0
      = stats__n_xIndirs - stats__n_xIndir_hits1 - stats__n_xIndir_hits2
        - stats__n_xIndir_hits3 - stats__n_xIndir_misses;
   VG_(message)(Vg_DebugMsg,
                "scheduler: %'llu indir transfers, "
                "%'llu misses (1 in %llu) ..\n",
                stats__n_xIndirs, stats__n_xIndir_misses,
                stats__n_xIndirs / (stats__n_xIndir_misses
                                   ? stats__n_xIndir_misses : 1));
   VG_(message)(Vg_DebugMsg,
                "scheduler: .. of which: %'llu hit0, %'llu hit1, "
                "%'llu hit2, %'llu hit3, %'llu missed\n",
                hits0,
                stats__n_xIndir_hits1,
                stats__n_xIndir_hits2,
                stats__n_xIndir_hits3,
                stats__n_xIndir_misses);

   VG_(message)(Vg_DebugMsg,
      "scheduler: %'llu/%'llu major/minor sched events.\n",
      n_scheduling_events_MAJOR, n_scheduling_events_MINOR);
   VG_(message)(Vg_DebugMsg, 
                "   sanity: %u cheap, %u expensive checks.\n",
                sanity_fast_count, sanity_slow_count );
}

/*
 * Mutual exclusion object used to serialize threads.
 */
static struct sched_lock *the_BigLock;


/* ---------------------------------------------------------------------
   Helper functions for the scheduler.
   ------------------------------------------------------------------ */

static void maybe_progress_report ( UInt reporting_interval_seconds )
{
   /* This is when the next report is due, in user cpu milliseconds since
      process start.  This is a global variable so this won't be thread-safe
      if Valgrind is ever made multithreaded.  For now it's fine. */
   static UInt next_report_due_at = 0;

   /* First of all, figure out whether another report is due.  It
      probably isn't. */
   UInt user_ms = VG_(get_user_milliseconds)();
   if (LIKELY(user_ms < next_report_due_at))
      return;

   Bool first_ever_call = next_report_due_at == 0;

   /* A report is due.  First, though, set the time for the next report. */
   next_report_due_at += 1000 * reporting_interval_seconds;

   /* If it's been an excessively long time since the last check, we
      might have gone more than one reporting interval forward.  Guard
      against that. */
   while (next_report_due_at <= user_ms)
      next_report_due_at += 1000 * reporting_interval_seconds;

   /* Also we don't want to report anything on the first call, but we
      have to wait till this point to leave, so that we set up the
      next-call time correctly. */
   if (first_ever_call)
      return;

   /* Print the report. */
   UInt   user_cpu_seconds  = user_ms / 1000;
   UInt   wallclock_seconds = VG_(read_millisecond_timer)() / 1000;
   Double millionEvCs   = ((Double)bbs_done) / 1000000.0;
   Double thousandTIns  = ((Double)VG_(get_bbs_translated)()) / 1000.0;
   Double thousandTOuts = ((Double)VG_(get_bbs_discarded_or_dumped)()) / 1000.0;
   UInt   nThreads      = VG_(count_living_threads)();

   if (VG_(clo_verbosity) > 0) {
      VG_(dmsg)("PROGRESS: U %'us, W %'us, %.1f%% CPU, EvC %.2fM, "
                "TIn %.1fk, TOut %.1fk, #thr %u\n",
                user_cpu_seconds, wallclock_seconds,
                100.0
                   * (Double)(user_cpu_seconds)
                   / (Double)(wallclock_seconds == 0 ? 1 : wallclock_seconds),
                millionEvCs,
                thousandTIns, thousandTOuts, nThreads);
   }
}

static
void print_sched_event ( ThreadId tid, const HChar* what )
{
   VG_(message)(Vg_DebugMsg, "  SCHED[%u]: %s\n", tid, what );
}

/* For showing SB profiles, if the user asks to see them. */
static
void maybe_show_sb_profile ( void )
{
   /* DO NOT MAKE NON-STATIC */
   static ULong bbs_done_lastcheck = 0;
   /* */
   vg_assert(VG_(clo_profyle_interval) > 0);
   Long delta = (Long)(bbs_done - bbs_done_lastcheck);
   vg_assert(delta >= 0);
   if ((ULong)delta >= VG_(clo_profyle_interval)) {
      bbs_done_lastcheck = bbs_done;
      VG_(get_and_show_SB_profile)(bbs_done);
   }
}

static
const HChar* name_of_sched_event ( UInt event )
{
   switch (event) {
      case VEX_TRC_JMP_INVALICACHE:    return "INVALICACHE";
      case VEX_TRC_JMP_FLUSHDCACHE:    return "FLUSHDCACHE";
      case VEX_TRC_JMP_NOREDIR:        return "NOREDIR";
      case VEX_TRC_JMP_SIGILL:         return "SIGILL";
      case VEX_TRC_JMP_SIGTRAP:        return "SIGTRAP";
      case VEX_TRC_JMP_SIGSEGV:        return "SIGSEGV";
      case VEX_TRC_JMP_SIGBUS:         return "SIGBUS";
      case VEX_TRC_JMP_SIGFPE_INTOVF:
      case VEX_TRC_JMP_SIGFPE_INTDIV:  return "SIGFPE";
      case VEX_TRC_JMP_EMWARN:         return "EMWARN";
      case VEX_TRC_JMP_EMFAIL:         return "EMFAIL";
      case VEX_TRC_JMP_CLIENTREQ:      return "CLIENTREQ";
      case VEX_TRC_JMP_YIELD:          return "YIELD";
      case VEX_TRC_JMP_NODECODE:       return "NODECODE";
      case VEX_TRC_JMP_MAPFAIL:        return "MAPFAIL";
      case VEX_TRC_JMP_EXTENSION:      return "EXTENSION";
      case VEX_TRC_JMP_SYS_SYSCALL:    return "SYSCALL";
      case VEX_TRC_JMP_SYS_INT32:      return "INT32";
      case VEX_TRC_JMP_SYS_INT128:     return "INT128";
      case VEX_TRC_JMP_SYS_INT129:     return "INT129";
      case VEX_TRC_JMP_SYS_INT130:     return "INT130";
      case VEX_TRC_JMP_SYS_INT145:     return "INT145";
      case VEX_TRC_JMP_SYS_INT210:     return "INT210";
      case VEX_TRC_JMP_SYS_SYSENTER:   return "SYSENTER";
      case VEX_TRC_JMP_BORING:         return "VEX_BORING";

      case VG_TRC_BORING:              return "VG_BORING";
      case VG_TRC_INNER_FASTMISS:      return "FASTMISS";
      case VG_TRC_INNER_COUNTERZERO:   return "COUNTERZERO";
      case VG_TRC_FAULT_SIGNAL:        return "FAULTSIGNAL";
      case VG_TRC_INVARIANT_FAILED:    return "INVFAILED";
      case VG_TRC_CHAIN_ME_TO_SLOW_EP: return "CHAIN_ME_SLOW";
      case VG_TRC_CHAIN_ME_TO_FAST_EP: return "CHAIN_ME_FAST";
      default:                         return "??UNKNOWN??";
  }
}

/* Allocate a completely empty ThreadState record. */
ThreadId VG_(alloc_ThreadState) ( void )
{
   Int i;
   for (i = 1; i < VG_N_THREADS; i++) {
      if (VG_(threads)[i].status == VgTs_Empty) {
         VG_(threads)[i].status = VgTs_Init;
         VG_(threads)[i].exitreason = VgSrc_None;
         if (VG_(threads)[i].thread_name)
            VG_(free)(VG_(threads)[i].thread_name);
         VG_(threads)[i].thread_name = NULL;
         return i;
      }
   }
   VG_(printf)("Use --max-threads=INT to specify a larger number of threads\n"
               "and rerun valgrind\n");
   VG_(core_panic)("Max number of threads is too low");
   /*NOTREACHED*/
}

/* 
   Mark a thread as Runnable.  This will block until the_BigLock is
   available, so that we get exclusive access to all the shared
   structures and the CPU.  Up until we get the_BigLock, we must not
   touch any shared state.

   When this returns, we'll actually be running.
 */
void VG_(acquire_BigLock)(ThreadId tid, const HChar* who)
{
   ThreadState *tst;

#if 0
   if (VG_(clo_trace_sched)) {
      HChar buf[VG_(strlen)(who) + 30];
      VG_(sprintf)(buf, "waiting for lock (%s)", who);
      print_sched_event(tid, buf);
   }
#endif

   /* First, acquire the_BigLock.  We can't do anything else safely
      prior to this point.  Even doing debug printing prior to this
      point is, technically, wrong. */
   VG_(acquire_BigLock_LL)(NULL);

   tst = VG_(get_ThreadState)(tid);

   vg_assert(tst->status != VgTs_Runnable);
   
   tst->status = VgTs_Runnable;

   if (VG_(running_tid) != VG_INVALID_THREADID)
      VG_(printf)("tid %u found %u running\n", tid, VG_(running_tid));
   vg_assert(VG_(running_tid) == VG_INVALID_THREADID);
   VG_(running_tid) = tid;

   { Addr gsp = VG_(get_SP)(tid);
      if (NULL != VG_(tdict).track_new_mem_stack_w_ECU)
         VG_(unknown_SP_update_w_ECU)(gsp, gsp, 0/*unknown origin*/);
      else
         VG_(unknown_SP_update)(gsp, gsp);
   }

   if (VG_(clo_trace_sched)) {
      HChar buf[VG_(strlen)(who) + 30];
      VG_(sprintf)(buf, " acquired lock (%s)", who);
      print_sched_event(tid, buf);
   }
}

/* 
   Set a thread into a sleeping state, and give up exclusive access to
   the CPU.  On return, the thread must be prepared to block until it
   is ready to run again (generally this means blocking in a syscall,
   but it may mean that we remain in a Runnable state and we're just
   yielding the CPU to another thread).
 */
void VG_(release_BigLock)(ThreadId tid, ThreadStatus sleepstate,
                          const HChar* who)
{
   ThreadState *tst = VG_(get_ThreadState)(tid);

   vg_assert(tst->status == VgTs_Runnable);

   vg_assert(sleepstate == VgTs_WaitSys ||
	     sleepstate == VgTs_Yielding);

   tst->status = sleepstate;

   vg_assert(VG_(running_tid) == tid);
   VG_(running_tid) = VG_INVALID_THREADID;

   if (VG_(clo_trace_sched)) {
      const HChar *status = VG_(name_of_ThreadStatus)(sleepstate);
      HChar buf[VG_(strlen)(who) + VG_(strlen)(status) + 30];
      VG_(sprintf)(buf, "releasing lock (%s) -> %s", who, status);
      print_sched_event(tid, buf);
   }

   /* Release the_BigLock; this will reschedule any runnable
      thread. */
   VG_(release_BigLock_LL)(NULL);
}

static void init_BigLock(void)
{
   vg_assert(!the_BigLock);
   the_BigLock = ML_(create_sched_lock)();
}

static void deinit_BigLock(void)
{
   ML_(destroy_sched_lock)(the_BigLock);
   the_BigLock = NULL;
}

/* See pub_core_scheduler.h for description */
void VG_(acquire_BigLock_LL) ( const HChar* who )
{
   ML_(acquire_sched_lock)(the_BigLock);
}

/* See pub_core_scheduler.h for description */
void VG_(release_BigLock_LL) ( const HChar* who )
{
   ML_(release_sched_lock)(the_BigLock);
}

Bool VG_(owns_BigLock_LL) ( ThreadId tid )
{
   return (ML_(get_sched_lock_owner)(the_BigLock)
           == VG_(threads)[tid].os_state.lwpid);
}


/* Clear out the ThreadState and release the semaphore. Leaves the
   ThreadState in VgTs_Zombie state, so that it doesn't get
   reallocated until the caller is really ready. */
void VG_(exit_thread)(ThreadId tid)
{
   vg_assert(VG_(is_valid_tid)(tid));
   vg_assert(VG_(is_running_thread)(tid));
   vg_assert(VG_(is_exiting)(tid));

   mostly_clear_thread_record(tid);
   VG_(running_tid) = VG_INVALID_THREADID;

   /* There should still be a valid exitreason for this thread */
   vg_assert(VG_(threads)[tid].exitreason != VgSrc_None);

   if (VG_(clo_trace_sched))
      print_sched_event(tid, "release lock in VG_(exit_thread)");

   VG_(release_BigLock_LL)(NULL);
}

/* If 'tid' is blocked in a syscall, send it SIGVGKILL so as to get it
   out of the syscall and onto doing the next thing, whatever that is.
   If it isn't blocked in a syscall, has no effect on the thread. */
void VG_(get_thread_out_of_syscall)(ThreadId tid)
{
   vg_assert(VG_(is_valid_tid)(tid));
   vg_assert(!VG_(is_running_thread)(tid));

   if (VG_(threads)[tid].status == VgTs_WaitSys) {
      if (VG_(clo_trace_signals)) {
         VG_(message)(Vg_DebugMsg,
                      "get_thread_out_of_syscall zaps tid %u lwp %d\n",
                      tid, VG_(threads)[tid].os_state.lwpid);
      }
#     if defined(VGO_darwin)
      {
         // GrP fixme use mach primitives on darwin?
         // GrP fixme thread_abort_safely?
         // GrP fixme race for thread with WaitSys set but not in syscall yet?
         extern kern_return_t thread_abort(mach_port_t);
         thread_abort(VG_(threads)[tid].os_state.lwpid);
      }
#     else
      {
         __attribute__((unused))
         Int r = VG_(tkill)(VG_(threads)[tid].os_state.lwpid, VG_SIGVGKILL);
         /* JRS 2009-Mar-20: should we assert for r==0 (tkill succeeded)?
            I'm really not sure.  Here's a race scenario which argues
            that we shoudn't; but equally I'm not sure the scenario is
            even possible, because of constraints caused by the question
            of who holds the BigLock when.

            Target thread tid does sys_read on a socket and blocks.  This
            function gets called, and we observe correctly that tid's
            status is WaitSys but then for whatever reason this function
            goes very slowly for a while.  Then data arrives from
            wherever, tid's sys_read returns, tid exits.  Then we do
            tkill on tid, but tid no longer exists; tkill returns an
            error code and the assert fails. */
         /* vg_assert(r == 0); */
      }
#     endif
   }
}

/* 
   Yield the CPU for a short time to let some other thread run.
 */
void VG_(vg_yield)(void)
{
   ThreadId tid = VG_(running_tid);

   vg_assert(tid != VG_INVALID_THREADID);
   vg_assert(VG_(threads)[tid].os_state.lwpid == VG_(gettid)());

   VG_(release_BigLock)(tid, VgTs_Yielding, "VG_(vg_yield)");

   /* 
      Tell the kernel we're yielding.
    */
#  if defined(VGO_linux) || defined(VGO_darwin) || defined(VGO_freebsd)
   VG_(do_syscall0)(__NR_sched_yield);
#  elif defined(VGO_solaris)
   VG_(do_syscall0)(__NR_yield);
#  else
#    error Unknown OS
#  endif

   VG_(acquire_BigLock)(tid, "VG_(vg_yield)");
}


/* Set the standard set of blocked signals, used whenever we're not
   running a client syscall. */
static void block_signals(void)
{
   vki_sigset_t mask;

   VG_(sigfillset)(&mask);

   /* Don't block these because they're synchronous */
   VG_(sigdelset)(&mask, VKI_SIGSEGV);
   VG_(sigdelset)(&mask, VKI_SIGBUS);
   VG_(sigdelset)(&mask, VKI_SIGFPE);
   VG_(sigdelset)(&mask, VKI_SIGILL);
   VG_(sigdelset)(&mask, VKI_SIGTRAP);
   VG_(sigdelset)(&mask, VKI_SIGSYS);

   /* Can't block these anyway */
   VG_(sigdelset)(&mask, VKI_SIGSTOP);
   VG_(sigdelset)(&mask, VKI_SIGKILL);

   VG_(sigprocmask)(VKI_SIG_SETMASK, &mask, NULL);
}

static void os_state_clear(ThreadState *tst)
{
   tst->os_state.lwpid       = 0;
   tst->os_state.threadgroup = 0;
   tst->os_state.stk_id = NULL_STK_ID;
#  if defined(VGO_linux)
   /* no other fields to clear */
#  elif defined(VGO_freebsd)
   /* no other fields to clear */
#  elif defined(VGO_darwin)
   tst->os_state.post_mach_trap_fn = NULL;
   tst->os_state.pthread           = 0;
   tst->os_state.func_arg          = 0;
   VG_(memset)(&tst->os_state.child_go, 0, sizeof(tst->os_state.child_go));
   VG_(memset)(&tst->os_state.child_done, 0, sizeof(tst->os_state.child_done));
   tst->os_state.wq_jmpbuf_valid   = False;
   tst->os_state.remote_port       = 0;
   tst->os_state.msgh_id           = 0;
   VG_(memset)(&tst->os_state.mach_args, 0, sizeof(tst->os_state.mach_args));
#  elif defined(VGO_solaris)
#  if defined(VGP_x86_solaris)
   tst->os_state.thrptr = 0;
#  endif
   tst->os_state.ustack = NULL;
   tst->os_state.in_door_return = False;
   tst->os_state.door_return_procedure = 0;
   tst->os_state.oldcontext = NULL;
   tst->os_state.schedctl_data = 0;
   tst->os_state.daemon_thread = False;
#  else
#    error "Unknown OS"
#  endif
}

static void os_state_init(ThreadState *tst)
{
   tst->os_state.valgrind_stack_base    = 0;
   tst->os_state.valgrind_stack_init_SP = 0;
   os_state_clear(tst);
}

static 
void mostly_clear_thread_record ( ThreadId tid )
{
   vki_sigset_t savedmask;

   vg_assert(tid < VG_N_THREADS);
   VG_(cleanup_thread)(&VG_(threads)[tid].arch);
   VG_(threads)[tid].tid = tid;

   /* Leave the thread in Zombie, so that it doesn't get reallocated
      until the caller is finally done with the thread stack. */
   VG_(threads)[tid].status               = VgTs_Zombie;

   VG_(sigemptyset)(&VG_(threads)[tid].sig_mask);
   VG_(sigemptyset)(&VG_(threads)[tid].tmp_sig_mask);

   os_state_clear(&VG_(threads)[tid]);

   /* start with no altstack */
   VG_(threads)[tid].altstack.ss_sp = (void *)0xdeadbeef;
   VG_(threads)[tid].altstack.ss_size = 0;
   VG_(threads)[tid].altstack.ss_flags = VKI_SS_DISABLE;

   VG_(clear_out_queued_signals)(tid, &savedmask);

   VG_(threads)[tid].sched_jmpbuf_valid = False;
}

/*                                                                             
   Called in the child after fork.  If the parent has multiple
   threads, then we've inherited a VG_(threads) array describing them,
   but only the thread which called fork() is actually alive in the
   child.  This functions needs to clean up all those other thread
   structures.
                                                                               
   Whichever tid in the parent which called fork() becomes the                 
   master_tid in the child.  That's because the only living slot in            
   VG_(threads) in the child after fork is VG_(threads)[tid], and it           
   would be too hard to try to re-number the thread and relocate the           
   thread state down to VG_(threads)[1].                                       
                                                                               
   This function also needs to reinitialize the_BigLock, since
   otherwise we may end up sharing its state with the parent, which
   would be deeply confusing.
*/                                          
static void sched_fork_cleanup(ThreadId me)
{
   ThreadId tid;
   vg_assert(VG_(running_tid) == me);

#  if defined(VGO_darwin)
   // GrP fixme hack reset Mach ports
   VG_(mach_init)();
#  endif

   VG_(threads)[me].os_state.lwpid = VG_(gettid)();
   VG_(threads)[me].os_state.threadgroup = VG_(getpid)();

   /* clear out all the unused thread slots */
   for (tid = 1; tid < VG_N_THREADS; tid++) {
      if (tid != me) {
         mostly_clear_thread_record(tid);
         VG_(threads)[tid].status = VgTs_Empty;
         VG_(clear_syscallInfo)(tid);
      }
   }

   /* re-init and take the sema */
   deinit_BigLock();
   init_BigLock();
   VG_(acquire_BigLock_LL)(NULL);
}


/* First phase of initialisation of the scheduler.  Initialise the
   bigLock, zeroise the VG_(threads) structure and decide on the
   ThreadId of the root thread.
*/
ThreadId VG_(scheduler_init_phase1) ( void )
{
   Int i;
   ThreadId tid_main;

   VG_(debugLog)(1,"sched","sched_init_phase1\n");

   if (VG_(clo_fair_sched) != disable_fair_sched
       && !ML_(set_sched_lock_impl)(sched_lock_ticket)
       && VG_(clo_fair_sched) == enable_fair_sched)
   {
      VG_(printf)("Error: fair scheduling is not supported on this system.\n");
      VG_(exit)(1);
   }

   if (VG_(clo_verbosity) > 1) {
      VG_(message)(Vg_DebugMsg,
                   "Scheduler: using %s scheduler lock implementation.\n",
                   ML_(get_sched_lock_name)());
   }

   init_BigLock();

   for (i = 0 /* NB; not 1 */; i < VG_N_THREADS; i++) {
      /* Paranoia .. completely zero it out. */
      VG_(memset)( & VG_(threads)[i], 0, sizeof( VG_(threads)[i] ) );

      VG_(threads)[i].sig_queue = NULL;

      os_state_init(&VG_(threads)[i]);
      mostly_clear_thread_record(i);

      VG_(threads)[i].status                    = VgTs_Empty;
      VG_(threads)[i].client_stack_szB          = 0;
      VG_(threads)[i].client_stack_highest_byte = (Addr)NULL;
      VG_(threads)[i].err_disablement_level     = 0;
      VG_(threads)[i].thread_name               = NULL;
   }

   tid_main = VG_(alloc_ThreadState)();

   /* Bleh.  Unfortunately there are various places in the system that
      assume that the main thread has a ThreadId of 1.
      - Helgrind (possibly)
      - stack overflow message in default_action() in m_signals.c
      - definitely a lot more places
   */
   vg_assert(tid_main == 1);

   return tid_main;
}


/* Second phase of initialisation of the scheduler.  Given the root
   ThreadId computed by first phase of initialisation, fill in stack
   details and acquire bigLock.  Initialise the scheduler.  This is
   called at startup.  The caller subsequently initialises the guest
   state components of this main thread.
*/
void VG_(scheduler_init_phase2) ( ThreadId tid_main,
                                  Addr     clstack_end, 
                                  SizeT    clstack_size )
{
   VG_(debugLog)(1,"sched","sched_init_phase2: tid_main=%u, "
                   "cls_end=0x%lx, cls_sz=%lu\n",
                   tid_main, clstack_end, clstack_size);

   vg_assert(VG_IS_PAGE_ALIGNED(clstack_end+1));
   vg_assert(VG_IS_PAGE_ALIGNED(clstack_size));

   VG_(threads)[tid_main].client_stack_highest_byte 
      = clstack_end;
   VG_(threads)[tid_main].client_stack_szB 
      = clstack_size;

   VG_(atfork)(NULL, NULL, sched_fork_cleanup);
}


/* ---------------------------------------------------------------------
   Helpers for running translations.
   ------------------------------------------------------------------ */

/* Use gcc's built-in setjmp/longjmp.  longjmp must not restore signal
   mask state, but does need to pass "val" through.  jumped must be a
   volatile UWord. */
#define SCHEDSETJMP(tid, jumped, stmt)                            \
   do {                                                           \
      ThreadState * volatile _qq_tst = VG_(get_ThreadState)(tid); \
                                                                  \
      (jumped) = VG_MINIMAL_SETJMP(_qq_tst->sched_jmpbuf);        \
      if ((jumped) == ((UWord)0)) {                               \
         vg_assert(!_qq_tst->sched_jmpbuf_valid);                 \
         _qq_tst->sched_jmpbuf_valid = True;                      \
         stmt;                                                    \
      }	else if (VG_(clo_trace_sched))                           \
         VG_(printf)("SCHEDSETJMP(line %d) tid %u, jumped=%lu\n", \
                     __LINE__, tid, jumped);                      \
      vg_assert(_qq_tst->sched_jmpbuf_valid);                     \
      _qq_tst->sched_jmpbuf_valid = False;                        \
   } while(0)


/* Do various guest state alignment checks prior to running a thread.
   Specifically, check that what we have matches Vex's guest state
   layout requirements.  See libvex.h for details, but in short the
   requirements are: There must be no holes in between the primary
   guest state, its two copies, and the spill area.  In short, all 4
   areas must be aligned on the LibVEX_GUEST_STATE_ALIGN boundary and 
   be placed back-to-back without holes in between. */
static void do_pre_run_checks ( volatile ThreadState* tst )
{
   Addr a_vex     = (Addr) & tst->arch.vex;
   Addr a_vexsh1  = (Addr) & tst->arch.vex_shadow1;
   Addr a_vexsh2  = (Addr) & tst->arch.vex_shadow2;
   Addr a_spill   = (Addr) & tst->arch.vex_spill;
   UInt sz_vex    = (UInt) sizeof tst->arch.vex;
   UInt sz_vexsh1 = (UInt) sizeof tst->arch.vex_shadow1;
   UInt sz_vexsh2 = (UInt) sizeof tst->arch.vex_shadow2;
   UInt sz_spill  = (UInt) sizeof tst->arch.vex_spill;

   if (0)
   VG_(printf)("gst %p %u, sh1 %p %u, "
               "sh2 %p %u, spill %p %u\n",
               (void*)a_vex, sz_vex,
               (void*)a_vexsh1, sz_vexsh1,
               (void*)a_vexsh2, sz_vexsh2,
               (void*)a_spill, sz_spill );

   vg_assert(sz_vex    % LibVEX_GUEST_STATE_ALIGN == 0);
   vg_assert(sz_vexsh1 % LibVEX_GUEST_STATE_ALIGN == 0);
   vg_assert(sz_vexsh2 % LibVEX_GUEST_STATE_ALIGN == 0);
   vg_assert(sz_spill  % LibVEX_GUEST_STATE_ALIGN == 0);

   vg_assert(a_vex    % LibVEX_GUEST_STATE_ALIGN == 0);
   vg_assert(a_vexsh1 % LibVEX_GUEST_STATE_ALIGN == 0);
   vg_assert(a_vexsh2 % LibVEX_GUEST_STATE_ALIGN == 0);
   vg_assert(a_spill  % LibVEX_GUEST_STATE_ALIGN == 0);

   /* Check that the guest state and its two shadows have the same
      size, and that there are no holes in between.  The latter is
      important because Memcheck assumes that it can reliably access
      the shadows by indexing off a pointer to the start of the
      primary guest state area. */
   vg_assert(sz_vex == sz_vexsh1);
   vg_assert(sz_vex == sz_vexsh2);
   vg_assert(a_vex + 1 * sz_vex == a_vexsh1);
   vg_assert(a_vex + 2 * sz_vex == a_vexsh2);
   /* Also check there's no hole between the second shadow area and
      the spill area. */
   vg_assert(sz_spill == LibVEX_N_SPILL_BYTES);
   vg_assert(a_vex + 3 * sz_vex == a_spill);

#  if defined(VGA_x86)
   /* x86 XMM regs must form an array, ie, have no holes in
      between. */
   vg_assert(
      (offsetof(VexGuestX86State,guest_XMM7)
       - offsetof(VexGuestX86State,guest_XMM0))
      == (8/*#regs*/-1) * 16/*bytes per reg*/
   );
   vg_assert(VG_IS_16_ALIGNED(offsetof(VexGuestX86State,guest_XMM0)));
   vg_assert(VG_IS_8_ALIGNED(offsetof(VexGuestX86State,guest_FPREG)));
   vg_assert(8 == offsetof(VexGuestX86State,guest_EAX));
   vg_assert(VG_IS_4_ALIGNED(offsetof(VexGuestX86State,guest_EAX)));
   vg_assert(VG_IS_4_ALIGNED(offsetof(VexGuestX86State,guest_EIP)));
#  endif

#  if defined(VGA_amd64)
   /* amd64 YMM regs must form an array, ie, have no holes in
      between. */
   vg_assert(
      (offsetof(VexGuestAMD64State,guest_YMM16)
       - offsetof(VexGuestAMD64State,guest_YMM0))
      == (17/*#regs*/-1) * 32/*bytes per reg*/
   );
   vg_assert(VG_IS_16_ALIGNED(offsetof(VexGuestAMD64State,guest_YMM0)));
   vg_assert(VG_IS_8_ALIGNED(offsetof(VexGuestAMD64State,guest_FPREG)));
   vg_assert(16 == offsetof(VexGuestAMD64State,guest_RAX));
   vg_assert(VG_IS_8_ALIGNED(offsetof(VexGuestAMD64State,guest_RAX)));
   vg_assert(VG_IS_8_ALIGNED(offsetof(VexGuestAMD64State,guest_RIP)));
#  endif

#  if defined(VGA_ppc32) || defined(VGA_ppc64be) || defined(VGA_ppc64le)
   /* ppc guest_state vector regs must be 16 byte aligned for
      loads/stores.  This is important! */
   vg_assert(VG_IS_16_ALIGNED(& tst->arch.vex.guest_VSR0));
   vg_assert(VG_IS_16_ALIGNED(& tst->arch.vex_shadow1.guest_VSR0));
   vg_assert(VG_IS_16_ALIGNED(& tst->arch.vex_shadow2.guest_VSR0));
   /* be extra paranoid .. */
   vg_assert(VG_IS_16_ALIGNED(& tst->arch.vex.guest_VSR1));
   vg_assert(VG_IS_16_ALIGNED(& tst->arch.vex_shadow1.guest_VSR1));
   vg_assert(VG_IS_16_ALIGNED(& tst->arch.vex_shadow2.guest_VSR1));
#  endif

#  if defined(VGA_arm)
   /* arm guest_state VFP regs must be 8 byte aligned for
      loads/stores.  Let's use 16 just to be on the safe side. */
   vg_assert(VG_IS_16_ALIGNED(& tst->arch.vex.guest_D0));
   vg_assert(VG_IS_16_ALIGNED(& tst->arch.vex_shadow1.guest_D0));
   vg_assert(VG_IS_16_ALIGNED(& tst->arch.vex_shadow2.guest_D0));
   /* be extra paranoid .. */
   vg_assert(VG_IS_8_ALIGNED(& tst->arch.vex.guest_D1));
   vg_assert(VG_IS_8_ALIGNED(& tst->arch.vex_shadow1.guest_D1));
   vg_assert(VG_IS_8_ALIGNED(& tst->arch.vex_shadow2.guest_D1));
#  endif

#  if defined(VGA_arm64)
   vg_assert(VG_IS_8_ALIGNED(& tst->arch.vex.guest_X0));
   vg_assert(VG_IS_8_ALIGNED(& tst->arch.vex_shadow1.guest_X0));
   vg_assert(VG_IS_8_ALIGNED(& tst->arch.vex_shadow2.guest_X0));
   vg_assert(VG_IS_16_ALIGNED(& tst->arch.vex.guest_Q0));
   vg_assert(VG_IS_16_ALIGNED(& tst->arch.vex_shadow1.guest_Q0));
   vg_assert(VG_IS_16_ALIGNED(& tst->arch.vex_shadow2.guest_Q0));
#  endif

#  if defined(VGA_s390x)
   /* no special requirements */
#  endif

#  if defined(VGA_mips32) || defined(VGA_mips64)
   /* no special requirements */
#  endif
}

// NO_VGDB_POLL value ensures vgdb is not polled, while
// VGDB_POLL_ASAP ensures that the next scheduler call
// will cause a poll.
#define NO_VGDB_POLL    0xffffffffffffffffULL
#define VGDB_POLL_ASAP  0x0ULL

void VG_(disable_vgdb_poll) (void )
{
   vgdb_next_poll = NO_VGDB_POLL;
}
void VG_(force_vgdb_poll) ( void )
{
   vgdb_next_poll = VGDB_POLL_ASAP;
}

/* Run the thread tid for a while, and return a VG_TRC_* value
   indicating why VG_(disp_run_translations) stopped, and possibly an
   auxiliary word.  Also, only allow the thread to run for at most
   *dispatchCtrP events.  If (as is the normal case) use_alt_host_addr
   is False, we are running ordinary redir'd translations, and we
   should therefore start by looking up the guest next IP in TT.  If
   it is True then we ignore the guest next IP and just run from
   alt_host_addr, which presumably points at host code for a no-redir
   translation.

   Return results are placed in two_words.  two_words[0] is set to the
   TRC.  In the case where that is VG_TRC_CHAIN_ME_TO_{SLOW,FAST}_EP,
   the address to patch is placed in two_words[1].
*/
static
void run_thread_for_a_while ( /*OUT*/HWord* two_words,
                              /*MOD*/Int*   dispatchCtrP,
                              ThreadId      tid,
                              HWord         alt_host_addr,
                              Bool          use_alt_host_addr )
{
   volatile HWord        jumped         = 0;
   volatile ThreadState* tst            = NULL; /* stop gcc complaining */
   volatile Int          done_this_time = 0;
   volatile HWord        host_code_addr = 0;

   /* Paranoia */
   vg_assert(VG_(is_valid_tid)(tid));
   vg_assert(VG_(is_running_thread)(tid));
   vg_assert(!VG_(is_exiting)(tid));
   vg_assert(*dispatchCtrP > 0);

   tst = VG_(get_ThreadState)(tid);
   do_pre_run_checks( tst );
   /* end Paranoia */

   /* Futz with the XIndir stats counters. */
   vg_assert(VG_(stats__n_xIndirs_32) == 0);
   vg_assert(VG_(stats__n_xIndir_hits1_32) == 0);
   vg_assert(VG_(stats__n_xIndir_hits2_32) == 0);
   vg_assert(VG_(stats__n_xIndir_hits3_32) == 0);
   vg_assert(VG_(stats__n_xIndir_misses_32) == 0);

   /* Clear return area. */
   two_words[0] = two_words[1] = 0;

   /* Figure out where we're starting from. */
   if (use_alt_host_addr) {
      /* unusual case -- no-redir translation */
      host_code_addr = alt_host_addr;
   } else {
      /* normal case -- redir translation */
      Addr host_from_fast_cache = 0;
      Bool found_in_fast_cache
         = VG_(lookupInFastCache)( &host_from_fast_cache,
                                   (Addr)tst->arch.vex.VG_INSTR_PTR );
      if (found_in_fast_cache) {
         host_code_addr = host_from_fast_cache;
      } else {
         Addr res = 0;
         /* not found in VG_(tt_fast). Searching here the transtab
            improves the performance compared to returning directly
            to the scheduler. */
         Bool  found = VG_(search_transtab)(&res, NULL, NULL,
                                            (Addr)tst->arch.vex.VG_INSTR_PTR,
                                            True/*upd cache*/
                                            );
         if (LIKELY(found)) {
            host_code_addr = res;
         } else {
            /* At this point, we know that we intended to start at a
               normal redir translation, but it was not found.  In
               which case we can return now claiming it's not
               findable. */
            two_words[0] = VG_TRC_INNER_FASTMISS; /* hmm, is that right? */
            return;
         }
      }
   }
   /* We have either a no-redir or a redir translation. */
   vg_assert(host_code_addr != 0); /* implausible */

   /* there should be no undealt-with signals */
   //vg_assert(VG_(threads)[tid].siginfo.si_signo == 0);

   /* Set up event counter stuff for the run. */
   tst->arch.vex.host_EvC_COUNTER = *dispatchCtrP;
   tst->arch.vex.host_EvC_FAILADDR
      = (HWord)VG_(fnptr_to_fnentry)( &VG_(disp_cp_evcheck_fail) );

   /* Invalidate any in-flight LL/SC transactions, in the case that we're
      using the fallback LL/SC implementation.  See bugs 344524 and 369459. */
#  if defined(VGP_mips32_linux) || defined(VGP_mips64_linux) \
      || defined(VGP_nanomips_linux)
   tst->arch.vex.guest_LLaddr = (RegWord)(-1);
#  elif defined(VGP_arm64_linux) || defined(VGP_arm64_freebsd)
   tst->arch.vex.guest_LLSC_SIZE = 0;
#  endif

   if (0) {
      vki_sigset_t m;
      Int i, err = VG_(sigprocmask)(VKI_SIG_SETMASK, NULL, &m);
      vg_assert(err == 0);
      VG_(printf)("tid %u: entering code with unblocked signals: ", tid);
      for (i = 1; i <= _VKI_NSIG; i++)
         if (!VG_(sigismember)(&m, i))
            VG_(printf)("%d ", i);
      VG_(printf)("\n");
   }

   /* Set up return-value area. */

   // Tell the tool this thread is about to run client code
   VG_TRACK( start_client_code, tid, bbs_done );

   vg_assert(VG_(in_generated_code) == False);
   VG_(in_generated_code) = True;

   SCHEDSETJMP(
      tid, 
      jumped, 
      VG_(disp_run_translations)( 
         two_words,
         (volatile void*)&tst->arch.vex,
         host_code_addr
      )
   );

   vg_assert(VG_(in_generated_code) == True);
   VG_(in_generated_code) = False;

   if (jumped != (HWord)0) {
      /* We get here if the client took a fault that caused our signal
         handler to longjmp. */
      vg_assert(two_words[0] == 0 && two_words[1] == 0); // correct?
      two_words[0] = VG_TRC_FAULT_SIGNAL;
      two_words[1] = 0;
      block_signals();
   } 

   /* Merge the 32-bit XIndir/miss counters into the 64 bit versions,
      and zero out the 32-bit ones in preparation for the next run of
      generated code. */
   stats__n_xIndirs += (ULong)VG_(stats__n_xIndirs_32);
   VG_(stats__n_xIndirs_32) = 0;
   stats__n_xIndir_hits1 += (ULong)VG_(stats__n_xIndir_hits1_32);
   VG_(stats__n_xIndir_hits1_32) = 0;
   stats__n_xIndir_hits2 += (ULong)VG_(stats__n_xIndir_hits2_32);
   VG_(stats__n_xIndir_hits2_32) = 0;
   stats__n_xIndir_hits3 += (ULong)VG_(stats__n_xIndir_hits3_32);
   VG_(stats__n_xIndir_hits3_32) = 0;
   stats__n_xIndir_misses += (ULong)VG_(stats__n_xIndir_misses_32);
   VG_(stats__n_xIndir_misses_32) = 0;

   /* Inspect the event counter. */
   vg_assert((Int)tst->arch.vex.host_EvC_COUNTER >= -1);
   vg_assert(tst->arch.vex.host_EvC_FAILADDR
             == (HWord)VG_(fnptr_to_fnentry)( &VG_(disp_cp_evcheck_fail)) );

   /* The number of events done this time is the difference between
      the event counter originally and what it is now.  Except -- if
      it has gone negative (to -1) then the transition 0 to -1 doesn't
      correspond to a real executed block, so back it out.  It's like
      this because the event checks decrement the counter first and
      check it for negativeness second, hence the 0 to -1 transition
      causes a bailout and the block it happens in isn't executed. */
   {
     Int dispatchCtrAfterwards = (Int)tst->arch.vex.host_EvC_COUNTER;
     done_this_time = *dispatchCtrP - dispatchCtrAfterwards;
     if (dispatchCtrAfterwards == -1) {
        done_this_time--;
     } else {
        /* If the generated code drives the counter below -1, something
           is seriously wrong. */
        vg_assert(dispatchCtrAfterwards >= 0);
     }
   }

   vg_assert(done_this_time >= 0);
   bbs_done += (ULong)done_this_time;

   *dispatchCtrP -= done_this_time;
   vg_assert(*dispatchCtrP >= 0);

   // Tell the tool this thread has stopped running client code
   VG_TRACK( stop_client_code, tid, bbs_done );

   if (bbs_done >= vgdb_next_poll) {
      if (VG_(clo_vgdb_poll))
         vgdb_next_poll = bbs_done + (ULong)VG_(clo_vgdb_poll);
      else
         /* value was changed due to gdbserver invocation via ptrace */
         vgdb_next_poll = NO_VGDB_POLL;
      if (VG_(gdbserver_activity) (tid))
         VG_(gdbserver) (tid);
   }

   /* TRC value and possible auxiliary patch-address word are already
      in two_words[0] and [1] respectively, as a result of the call to
      VG_(run_innerloop). */
   /* Stay sane .. */
   if (two_words[0] == VG_TRC_CHAIN_ME_TO_SLOW_EP
       || two_words[0] == VG_TRC_CHAIN_ME_TO_FAST_EP) {
      vg_assert(two_words[1] != 0); /* we have a legit patch addr */
   } else {
      vg_assert(two_words[1] == 0); /* nobody messed with it */
   }
}


/* ---------------------------------------------------------------------
   The scheduler proper.
   ------------------------------------------------------------------ */

static void handle_tt_miss ( ThreadId tid )
{
   Bool found;
   Addr ip = VG_(get_IP)(tid);

   /* Trivial event.  Miss in the fast-cache.  Do a full
      lookup for it. */
   found = VG_(search_transtab)( NULL, NULL, NULL,
                                 ip, True/*upd_fast_cache*/ );
   if (UNLIKELY(!found)) {
      /* Not found; we need to request a translation. */
      if (VG_(translate)( tid, ip, /*debug*/False, 0/*not verbose*/, 
                          bbs_done, True/*allow redirection*/ )) {
         found = VG_(search_transtab)( NULL, NULL, NULL,
                                       ip, True ); 
         vg_assert2(found, "handle_tt_miss: missing tt_fast entry");
      
      } else {
	 // If VG_(translate)() fails, it's because it had to throw a
	 // signal because the client jumped to a bad address.  That
	 // means that either a signal has been set up for delivery,
	 // or the thread has been marked for termination.  Either
	 // way, we just need to go back into the scheduler loop.
      }
   }
}

static
void handle_chain_me ( ThreadId tid, void* place_to_chain, Bool toFastEP )
{
   Bool found          = False;
   Addr ip             = VG_(get_IP)(tid);
   SECno to_sNo         = INV_SNO;
   TTEno to_tteNo       = INV_TTE;

   found = VG_(search_transtab)( NULL, &to_sNo, &to_tteNo,
                                 ip, False/*dont_upd_fast_cache*/ );
   if (!found) {
      /* Not found; we need to request a translation. */
      if (VG_(translate)( tid, ip, /*debug*/False, 0/*not verbose*/, 
                          bbs_done, True/*allow redirection*/ )) {
         found = VG_(search_transtab)( NULL, &to_sNo, &to_tteNo,
                                       ip, False ); 
         vg_assert2(found, "handle_chain_me: missing tt_fast entry");
      } else {
	 // If VG_(translate)() fails, it's because it had to throw a
	 // signal because the client jumped to a bad address.  That
	 // means that either a signal has been set up for delivery,
	 // or the thread has been marked for termination.  Either
	 // way, we just need to go back into the scheduler loop.
        return;
      }
   }
   vg_assert(found);
   vg_assert(to_sNo != INV_SNO);
   vg_assert(to_tteNo != INV_TTE);

   /* So, finally we know where to patch through to.  Do the patching
      and update the various admin tables that allow it to be undone
      in the case that the destination block gets deleted. */
   VG_(tt_tc_do_chaining)( place_to_chain,
                           to_sNo, to_tteNo, toFastEP );
}

static void handle_syscall(ThreadId tid, UInt trc)
{
   ThreadState * volatile tst = VG_(get_ThreadState)(tid);
   volatile UWord jumped; 

   /* Syscall may or may not block; either way, it will be
      complete by the time this call returns, and we'll be
      runnable again.  We could take a signal while the
      syscall runs. */

   if (VG_(clo_sanity_level) >= 3) {
      HChar buf[50];    // large enough
      VG_(sprintf)(buf, "(BEFORE SYSCALL, tid %u)", tid);
      Bool ok = VG_(am_do_sync_check)(buf, __FILE__, __LINE__);
      vg_assert(ok);
   }

   SCHEDSETJMP(tid, jumped, VG_(client_syscall)(tid, trc));

   if (VG_(clo_sanity_level) >= 3) {
      HChar buf[50];    // large enough
      VG_(sprintf)(buf, "(AFTER SYSCALL, tid %u)", tid);
      Bool ok = VG_(am_do_sync_check)(buf, __FILE__, __LINE__);
      vg_assert(ok);
   }

   if (!VG_(is_running_thread)(tid))
      VG_(printf)("tid %u not running; VG_(running_tid)=%u, tid %u status %u\n",
		  tid, VG_(running_tid), tid, tst->status);
   vg_assert(VG_(is_running_thread)(tid));
   
   if (jumped != (UWord)0) {
      block_signals();
      VG_(poll_signals)(tid);
   }
}

static void handle_extension(ThreadId tid)
{
   volatile UWord jumped;
   enum ExtensionError err;

   SCHEDSETJMP(tid, jumped, err = VG_(client_extension)(tid));
   vg_assert(VG_(is_running_thread)(tid));

   if (jumped != (UWord)0) {
      block_signals();
      VG_(poll_signals)(tid);
   } else if (err != ExtErr_OK) {
      Addr addr = VG_(get_IP)(tid);
      switch (err) {
      case ExtErr_Illop:
         VG_(synth_sigill)(tid, addr);
         break;
      default:
         VG_(core_panic)("scheduler: bad return code from extension");
      }
   }
}

/* tid just requested a jump to the noredir version of its current
   program counter.  So make up that translation if needed, run it,
   and return the resulting thread return code in two_words[]. */
static
void handle_noredir_jump ( /*OUT*/HWord* two_words,
                           /*MOD*/Int*   dispatchCtrP,
                           ThreadId tid )
{
   /* Clear return area. */
   two_words[0] = two_words[1] = 0;

   Addr  hcode = 0;
   Addr  ip    = VG_(get_IP)(tid);

   Bool  found = VG_(search_unredir_transtab)( &hcode, ip );
   if (!found) {
      /* Not found; we need to request a translation. */
      if (VG_(translate)( tid, ip, /*debug*/False, 0/*not verbose*/, bbs_done,
                          False/*NO REDIRECTION*/ )) {

         found = VG_(search_unredir_transtab)( &hcode, ip );
         vg_assert2(found, "unredir translation missing after creation?!");
      } else {
	 // If VG_(translate)() fails, it's because it had to throw a
	 // signal because the client jumped to a bad address.  That
	 // means that either a signal has been set up for delivery,
	 // or the thread has been marked for termination.  Either
	 // way, we just need to go back into the scheduler loop.
         two_words[0] = VG_TRC_BORING;
         return;
      }

   }

   vg_assert(found);
   vg_assert(hcode != 0);

   /* Otherwise run it and return the resulting VG_TRC_* value. */
   vg_assert(*dispatchCtrP > 0); /* so as to guarantee progress */
   run_thread_for_a_while( two_words, dispatchCtrP, tid,
                           hcode, True/*use hcode*/ );
}


/* 
   Run a thread until it wants to exit.
   
   We assume that the caller has already called VG_(acquire_BigLock) for
   us, so we own the VCPU.  Also, all signals are blocked.
 */
VgSchedReturnCode VG_(scheduler) ( ThreadId tid )
{
   /* Holds the remaining size of this thread's "timeslice". */
   Int dispatch_ctr = 0;

   ThreadState *tst = VG_(get_ThreadState)(tid);
   static Bool vgdb_startup_action_done = False;

   if (VG_(clo_trace_sched))
      print_sched_event(tid, "entering VG_(scheduler)");      

   /* Do vgdb initialization (but once). Only the first (main) task
      starting up will do the below.
      Initialize gdbserver earlier than at the first 
      thread VG_(scheduler) is causing problems:
      * at the end of VG_(scheduler_init_phase2) :
        The main thread is in VgTs_Init state, but in a not yet
        consistent state => the thread cannot be reported to gdb
        (e.g. causes an assert in LibVEX_GuestX86_get_eflags when giving
        back the guest registers to gdb).
      * at end of valgrind_main, just
        before VG_(main_thread_wrapper_NORETURN)(1) :
        The main thread is still in VgTs_Init state but in a
        more advanced state. However, the thread state is not yet
        completely initialized : a.o., the os_state is not yet fully
        set => the thread is then not properly reported to gdb,
        which is then confused (causing e.g. a duplicate thread be
        shown, without thread id).
      * it would be possible to initialize gdbserver "lower" in the
        call stack (e.g. in VG_(main_thread_wrapper_NORETURN)) but
        these are platform dependent and the place at which
        the thread state is completely initialized is not
        specific anymore to the main thread (so a similar "do it only
        once" would be needed).

        => a "once only" initialization here is the best compromise. */
   if (!vgdb_startup_action_done) {
      vg_assert(tid == 1); // it must be the main thread.
      vgdb_startup_action_done = True;
      if (VG_(clo_vgdb) != Vg_VgdbNo) {
         /* If we have to poll, ensures we do an initial poll at first
            scheduler call. Otherwise, ensure no poll (unless interrupted
            by ptrace). */
         if (VG_(clo_vgdb_poll))
            VG_(force_vgdb_poll) ();
         else
            VG_(disable_vgdb_poll) ();

         VG_(gdbserver_prerun_action) (1);
      } else {
         VG_(disable_vgdb_poll) ();
      }
   }

   if (SimHintiS(SimHint_no_nptl_pthread_stackcache, VG_(clo_sim_hints))
       && tid != 1) {
      /* We disable the stack cache the first time we see a thread other
         than the main thread appearing. At this moment, we are sure the pthread
         lib loading is done/variable was initialised by pthread lib/... */
      if (VG_(client__stack_cache_actsize__addr)) {
         if (*VG_(client__stack_cache_actsize__addr) == 0) {
            VG_(debugLog)(1,"sched",
                          "pthread stack cache size disable done"
                          " via kludge\n");
            *VG_(client__stack_cache_actsize__addr) = 1000 * 1000 * 1000;
            /* Set a value big enough to be above the hardcoded maximum stack
               cache size in glibc, small enough to allow a pthread stack size
               to be added without risk of overflow. */
         }
      } else {
          /*
           * glibc 2.34 no longer has stack_cache_actsize as a visible variable
           * so we switch to using the GLIBC_TUNABLES env var. Processing for that
           * is done in initimg-linux.c / setup_client_env  for all glibc
           *
           * If we don't detect stack_cache_actsize we want to be able to tell
           * whether it is an unexpected error or if it is no longer there.
           * In the latter case we don't print a warning.
           */
          Bool print_warning = True;
          if (VG_(client__gnu_get_libc_version_addr) != NULL) {
              const HChar* gnu_libc_version = VG_(client__gnu_get_libc_version_addr)();
              if (gnu_libc_version != NULL) {
                  HChar* glibc_version_tok = VG_(strdup)("scheduler.1", gnu_libc_version);
                  const HChar* str_major = VG_(strtok)(glibc_version_tok, ".");
                  Long major = VG_(strtoll10)(str_major, NULL);
                  const HChar* str_minor = VG_(strtok)(NULL, ".");
                  Long minor = VG_(strtoll10)(str_minor, NULL);
                  if (major >= 2 && minor >= 34) {
                      print_warning = False;
                  }
                  VG_(free)(glibc_version_tok);
              }
          } else {

          }
          if (print_warning) {
              VG_(debugLog)(0,"sched",
                            "WARNING: pthread stack cache cannot be disabled!\n");
          }
          VG_(clo_sim_hints) &= ~SimHint2S(SimHint_no_nptl_pthread_stackcache);
          /* Remove SimHint_no_nptl_pthread_stackcache from VG_(clo_sim_hints)
             to avoid having a msg for all following threads. */
      }
   }

   /* set the proper running signal mask */
   block_signals();
   
   vg_assert(VG_(is_running_thread)(tid));

   dispatch_ctr = VG_(clo_scheduling_quantum);

   while (!VG_(is_exiting)(tid)) {

      vg_assert(dispatch_ctr >= 0);
      if (dispatch_ctr == 0) {

	 /* Our slice is done, so yield the CPU to another thread.  On
            Linux, this doesn't sleep between sleeping and running,
            since that would take too much time. */

	 /* 4 July 06: it seems that a zero-length nsleep is needed to
            cause async thread cancellation (canceller.c) to terminate
            in finite time; else it is in some kind of race/starvation
            situation and completion is arbitrarily delayed (although
            this is not a deadlock).

            Unfortunately these sleeps cause MPI jobs not to terminate
            sometimes (some kind of livelock).  So sleeping once
            every N opportunities appears to work. */

	 /* 3 Aug 06: doing sys__nsleep works but crashes some apps.
            sys_yield also helps the problem, whilst not crashing apps. */

	 VG_(release_BigLock)(tid, VgTs_Yielding, 
                                   "VG_(scheduler):timeslice");
	 /* ------------ now we don't have The Lock ------------ */

	 VG_(acquire_BigLock)(tid, "VG_(scheduler):timeslice");
	 /* ------------ now we do have The Lock ------------ */

	 /* OK, do some relatively expensive housekeeping stuff */
	 scheduler_sanity(tid);
	 VG_(sanity_check_general)(False);

         /* Possibly make a progress report */
         if (UNLIKELY(VG_(clo_progress_interval) > 0)) {
            maybe_progress_report( VG_(clo_progress_interval) );
         }

	 /* Look for any pending signals for this thread, and set them up
	    for delivery */
	 VG_(poll_signals)(tid);

	 if (VG_(is_exiting)(tid))
	    break;		/* poll_signals picked up a fatal signal */

	 /* For stats purposes only. */
	 n_scheduling_events_MAJOR++;

	 /* Figure out how many bbs to ask vg_run_innerloop to do. */
         dispatch_ctr = VG_(clo_scheduling_quantum);

	 /* paranoia ... */
	 vg_assert(tst->tid == tid);
	 vg_assert(tst->os_state.lwpid == VG_(gettid)());
      }

      /* For stats purposes only. */
      n_scheduling_events_MINOR++;

      if (0)
         VG_(message)(Vg_DebugMsg, "thread %u: running for %d bbs\n", 
                                   tid, dispatch_ctr - 1 );

      HWord trc[2]; /* "two_words" */
      run_thread_for_a_while( &trc[0],
                              &dispatch_ctr,
                              tid, 0/*ignored*/, False );

      if (VG_(clo_trace_sched) && VG_(clo_verbosity) > 2) {
         const HChar *name = name_of_sched_event(trc[0]);
         HChar buf[VG_(strlen)(name) + 10];    // large enough
	 VG_(sprintf)(buf, "TRC: %s", name);
	 print_sched_event(tid, buf);
      }

      if (trc[0] == VEX_TRC_JMP_NOREDIR) {
         /* If we got a request to run a no-redir version of
            something, do so now -- handle_noredir_jump just (creates
            and) runs that one translation.  The flip side is that the
            noredir translation can't itself return another noredir
            request -- that would be nonsensical.  It can, however,
            return VG_TRC_BORING, which just means keep going as
            normal. */
         /* Note that the fact that we need to continue with a
            no-redir jump is not recorded anywhere else in this
            thread's state.  So we *must* execute the block right now
            -- we can't fail to execute it and later resume with it,
            because by then we'll have forgotten the fact that it
            should be run as no-redir, but will get run as a normal
            potentially-redir'd, hence screwing up.  This really ought
            to be cleaned up, by noting in the guest state that the
            next block to be executed should be no-redir.  Then we can
            suspend and resume at any point, which isn't the case at
            the moment. */
         /* We can't enter a no-redir translation with the dispatch
            ctr set to zero, for the reasons commented just above --
            we need to force it to execute right now.  So, if the
            dispatch ctr is zero, set it to one.  Note that this would
            have the bad side effect of holding the Big Lock arbitrary
            long should there be an arbitrarily long sequence of
            back-to-back no-redir translations to run.  But we assert
            just below that this translation cannot request another
            no-redir jump, so we should be safe against that. */
         if (dispatch_ctr == 0) {
            dispatch_ctr = 1;
         }
         handle_noredir_jump( &trc[0], 
                              &dispatch_ctr,
                              tid );
         vg_assert(trc[0] != VEX_TRC_JMP_NOREDIR);

         /* This can't be allowed to happen, since it means the block
            didn't execute, and we have no way to resume-as-noredir
            after we get more timeslice.  But I don't think it ever
            can, since handle_noredir_jump will assert if the counter
            is zero on entry. */
         vg_assert(trc[0] != VG_TRC_INNER_COUNTERZERO);
         /* This asserts the same thing. */
         vg_assert(dispatch_ctr >= 0);

         /* A no-redir translation can't return with a chain-me
            request, since chaining in the no-redir cache is too
            complex. */
         vg_assert(trc[0] != VG_TRC_CHAIN_ME_TO_SLOW_EP
                   && trc[0] != VG_TRC_CHAIN_ME_TO_FAST_EP);
      }

      switch (trc[0]) {
      case VEX_TRC_JMP_BORING:
         /* assisted dispatch, no event.  Used by no-redir
            translations to force return to the scheduler. */
      case VG_TRC_BORING:
         /* no special event, just keep going. */
         break;

      case VG_TRC_INNER_FASTMISS:
	 vg_assert(dispatch_ctr >= 0);
	 handle_tt_miss(tid);
	 break;

      case VG_TRC_CHAIN_ME_TO_SLOW_EP: {
         if (0) VG_(printf)("sched: CHAIN_TO_SLOW_EP: %p\n", (void*)trc[1] );
         handle_chain_me(tid, (void*)trc[1], False);
         break;
      }

      case VG_TRC_CHAIN_ME_TO_FAST_EP: {
         if (0) VG_(printf)("sched: CHAIN_TO_FAST_EP: %p\n", (void*)trc[1] );
         handle_chain_me(tid, (void*)trc[1], True);
         break;
      }

      case VEX_TRC_JMP_CLIENTREQ:
	 do_client_request(tid);
	 break;

      case VEX_TRC_JMP_EXTENSION: {
         handle_extension(tid);
         break;
      }

      case VEX_TRC_JMP_SYS_INT128:  /* x86-linux */
      case VEX_TRC_JMP_SYS_INT129:  /* x86-darwin */
      case VEX_TRC_JMP_SYS_INT130:  /* x86-darwin */
      case VEX_TRC_JMP_SYS_INT145:  /* x86-solaris */
      case VEX_TRC_JMP_SYS_INT210:  /* x86-solaris */
      /* amd64-linux, ppc32-linux, amd64-darwin, amd64-solaris */
      case VEX_TRC_JMP_SYS_SYSCALL:
	 handle_syscall(tid, trc[0]);
         if (VG_(clo_sanity_level) >= 3)
	    VG_(sanity_check_general)(True); /* sanity-check every syscall */
	 break;

      case VEX_TRC_JMP_YIELD:
	 /* Explicit yield, because this thread is in a spin-lock
	    or something.  Only let the thread run for a short while
            longer.  Because swapping to another thread is expensive,
            we're prepared to let this thread eat a little more CPU
            before swapping to another.  That means that short term
            spins waiting for hardware to poke memory won't cause a
            thread swap. */
         if (dispatch_ctr > 300)
            dispatch_ctr = 300;
	 break;

      case VG_TRC_INNER_COUNTERZERO:
	 /* Timeslice is out.  Let a new thread be scheduled. */
	 vg_assert(dispatch_ctr == 0);
	 break;

      case VG_TRC_FAULT_SIGNAL:
	 /* Everything should be set up (either we're exiting, or
	    about to start in a signal handler). */
	 break;

      case VEX_TRC_JMP_MAPFAIL:
         /* Failure of arch-specific address translation (x86/amd64
            segment override use) */
         /* jrs 2005 03 11: is this correct? */
         VG_(synth_fault)(tid);
         break;

      case VEX_TRC_JMP_EMWARN: {
         static Int  counts[EmNote_NUMBER];
         static Bool counts_initted = False;
         VexEmNote ew;
         const HChar* what;
         Bool      show;
         Int       q;
         if (!counts_initted) {
            counts_initted = True;
            for (q = 0; q < EmNote_NUMBER; q++)
               counts[q] = 0;
         }
         ew   = (VexEmNote)VG_(threads)[tid].arch.vex.guest_EMNOTE;
         what = (ew < 0 || ew >= EmNote_NUMBER)
                   ? "unknown (?!)"
                   : LibVEX_EmNote_string(ew);
         show = (ew < 0 || ew >= EmNote_NUMBER)
                   ? True
                   : counts[ew]++ < 3;
         if (show && VG_(clo_show_emwarns) && !VG_(clo_xml)) {
            VG_(message)( Vg_UserMsg,
                          "Emulation warning: unsupported action:\n");
            VG_(message)( Vg_UserMsg, "  %s\n", what);
            VG_(get_and_pp_StackTrace)( tid, VG_(clo_backtrace_size) );
         }
         break;
      }

      case VEX_TRC_JMP_EMFAIL: {
         VexEmNote ew;
         const HChar* what;
         ew   = (VexEmNote)VG_(threads)[tid].arch.vex.guest_EMNOTE;
         what = (ew < 0 || ew >= EmNote_NUMBER)
                   ? "unknown (?!)"
                   : LibVEX_EmNote_string(ew);
         VG_(message)( Vg_UserMsg,
                       "Emulation fatal error -- Valgrind cannot continue:\n");
         VG_(message)( Vg_UserMsg, "  %s\n", what);
         VG_(get_and_pp_StackTrace)( tid, VG_(clo_backtrace_size) );
         VG_(message)(Vg_UserMsg, "\n");
         VG_(message)(Vg_UserMsg, "Valgrind has to exit now.  Sorry.\n");
         VG_(message)(Vg_UserMsg, "\n");
         VG_(exit)(1);
         break;
      }

      case VEX_TRC_JMP_SIGILL:
         VG_(synth_sigill)(tid, VG_(get_IP)(tid));
         break;

      case VEX_TRC_JMP_SIGTRAP:
         VG_(synth_sigtrap)(tid);
         break;

      case VEX_TRC_JMP_SIGSEGV:
         VG_(synth_fault)(tid);
         break;

      case VEX_TRC_JMP_SIGBUS:
         VG_(synth_sigbus)(tid);
         break;

      case VEX_TRC_JMP_SIGFPE:
         VG_(synth_sigfpe)(tid, 0);
         break;

      case VEX_TRC_JMP_SIGFPE_INTDIV:
         VG_(synth_sigfpe)(tid, VKI_FPE_INTDIV);
         break;

      case VEX_TRC_JMP_SIGFPE_INTOVF:
         VG_(synth_sigfpe)(tid, VKI_FPE_INTOVF);
         break;

      case VEX_TRC_JMP_NODECODE: {
         Addr addr = VG_(get_IP)(tid);

         if (VG_(clo_sigill_diag)) {
            VG_(umsg)(
               "valgrind: Unrecognised instruction at address %#lx.\n", addr);
            VG_(get_and_pp_StackTrace)(tid, VG_(clo_backtrace_size));
#        define M(a) VG_(umsg)(a "\n");
         M("Your program just tried to execute an instruction that Valgrind" );
         M("did not recognise.  There are two possible reasons for this."    );
         M("1. Your program has a bug and erroneously jumped to a non-code"  );
         M("   location.  If you are running Memcheck and you just saw a"    );
         M("   warning about a bad jump, it's probably your program's fault.");
         M("2. The instruction is legitimate but Valgrind doesn't handle it,");
         M("   i.e. it's Valgrind's fault.  If you think this is the case or");
         M("   you are not sure, please let us know and we'll try to fix it.");
         M("Either way, Valgrind will now raise a SIGILL signal which will"  );
         M("probably kill your program."                                     );
#        undef M
         }
#        if defined(VGA_s390x)
         /* Now that the complaint is out we need to adjust the guest_IA. The
            reason is that -- after raising the exception -- execution will
            continue with the insn that follows the invalid insn. As the first
            2 bits of the invalid insn determine its length in the usual way,
            we can compute the address of the next insn here and adjust the
            guest_IA accordingly. This adjustment is essential and tested by
            none/tests/s390x/op_exception.c (which would loop forever
            otherwise) */
         UChar byte = ((UChar *)addr)[0];
         UInt  insn_length = ((((byte >> 6) + 1) >> 1) + 1) << 1;
         Addr  next_insn_addr = addr + insn_length;
         VG_(set_IP)(tid, next_insn_addr);
#        endif
         VG_(synth_sigill)(tid, addr);
         break;
      }

      case VEX_TRC_JMP_INVALICACHE:
         VG_(discard_translations)(
            (Addr)VG_(threads)[tid].arch.vex.guest_CMSTART,
            VG_(threads)[tid].arch.vex.guest_CMLEN,
            "scheduler(VEX_TRC_JMP_INVALICACHE)"
         );
         if (0)
            VG_(printf)("dump translations done.\n");
         break;

      case VEX_TRC_JMP_FLUSHDCACHE: {
         void* start = (void*)(Addr)VG_(threads)[tid].arch.vex.guest_CMSTART;
         SizeT len   = VG_(threads)[tid].arch.vex.guest_CMLEN;
         VG_(debugLog)(2, "sched", "flush_dcache(%p, %lu)\n", start, len);
         VG_(flush_dcache)(start, len);
         break;
      }

      case VG_TRC_INVARIANT_FAILED:
         /* This typically happens if, after running generated code,
            it is detected that host CPU settings (eg, FPU/Vector
            control words) are not as they should be.  Vex's code
            generation specifies the state such control words should
            be in on entry to Vex-generated code, and they should be
            unchanged on exit from it.  Failure of this assertion
            usually means a bug in Vex's code generation. */
         //{ UInt xx;
         //  __asm__ __volatile__ (
         //     "\t.word 0xEEF12A10\n"  // fmrx r2,fpscr
         //     "\tmov %0, r2" : "=r"(xx) : : "r2" );
         //  VG_(printf)("QQQQ new fpscr = %08x\n", xx);
         //}
         vg_assert2(0, "VG_(scheduler), phase 3: "
                       "run_innerloop detected host "
                       "state invariant failure", trc);

      case VEX_TRC_JMP_SYS_SYSENTER:
         /* Do whatever simulation is appropriate for an x86 sysenter
            instruction.  Note that it is critical to set this thread's
            guest_EIP to point at the code to execute after the
            sysenter, since Vex-generated code will not have set it --
            vex does not know what it should be.  Vex sets the next
            address to zero, so if you don't set guest_EIP, the thread
            will jump to zero afterwards and probably die as a result. */
#        if defined(VGP_x86_linux)
         vg_assert2(0, "VG_(scheduler), phase 3: "
                       "sysenter_x86 on x86-linux is not supported");
#        elif defined(VGP_x86_darwin) || defined(VGP_x86_solaris)
         /* return address in client edx */
         VG_(threads)[tid].arch.vex.guest_EIP
            = VG_(threads)[tid].arch.vex.guest_EDX;
         handle_syscall(tid, trc[0]);
#        else
         vg_assert2(0, "VG_(scheduler), phase 3: "
                       "sysenter_x86 on non-x86 platform?!?!");
#        endif
         break;

      default: 
	 vg_assert2(0, "VG_(scheduler), phase 3: "
                       "unexpected thread return code (%u)", trc[0]);
	 /* NOTREACHED */
	 break;

      } /* switch (trc) */

      if (UNLIKELY(VG_(clo_profyle_sbs)) && VG_(clo_profyle_interval) > 0)
         maybe_show_sb_profile();
   }

   if (VG_(clo_trace_sched))
      print_sched_event(tid, "exiting VG_(scheduler)");

   vg_assert(VG_(is_exiting)(tid));

   return tst->exitreason;
}


void VG_(nuke_all_threads_except) ( ThreadId me, VgSchedReturnCode src )
{
   ThreadId tid;

   vg_assert(VG_(is_running_thread)(me));

   for (tid = 1; tid < VG_N_THREADS; tid++) {
      if (tid == me
          || VG_(threads)[tid].status == VgTs_Empty)
         continue;
      if (0)
         VG_(printf)(
            "VG_(nuke_all_threads_except): nuking tid %u\n", tid);

      VG_(threads)[tid].exitreason = src;
      if (src == VgSrc_FatalSig)
         VG_(threads)[tid].os_state.fatalsig = VKI_SIGKILL;
      VG_(get_thread_out_of_syscall)(tid);
   }
}


/* ---------------------------------------------------------------------
   Specifying shadow register values
   ------------------------------------------------------------------ */

#if defined(VGA_x86)
#  define VG_CLREQ_ARGS       guest_EAX
#  define VG_CLREQ_RET        guest_EDX
#elif defined(VGA_amd64)
#  define VG_CLREQ_ARGS       guest_RAX
#  define VG_CLREQ_RET        guest_RDX
#elif defined(VGA_ppc32) || defined(VGA_ppc64be) || defined(VGA_ppc64le)
#  define VG_CLREQ_ARGS       guest_GPR4
#  define VG_CLREQ_RET        guest_GPR3
#elif defined(VGA_arm)
#  define VG_CLREQ_ARGS       guest_R4
#  define VG_CLREQ_RET        guest_R3
#elif defined(VGA_arm64)
#  define VG_CLREQ_ARGS       guest_X4
#  define VG_CLREQ_RET        guest_X3
#elif defined (VGA_s390x)
#  define VG_CLREQ_ARGS       guest_r2
#  define VG_CLREQ_RET        guest_r3
#elif defined(VGA_mips32) || defined(VGA_mips64) || defined(VGA_nanomips)
#  define VG_CLREQ_ARGS       guest_r12
#  define VG_CLREQ_RET        guest_r11
#else
#  error Unknown arch
#endif

#define CLREQ_ARGS(regs)   ((regs).vex.VG_CLREQ_ARGS)
#define CLREQ_RET(regs)    ((regs).vex.VG_CLREQ_RET)
#define O_CLREQ_RET        (offsetof(VexGuestArchState, VG_CLREQ_RET))

// These macros write a value to a client's thread register, and tell the
// tool that it's happened (if necessary).

#define SET_CLREQ_RETVAL(zztid, zzval) \
   do { CLREQ_RET(VG_(threads)[zztid].arch) = (zzval); \
        VG_TRACK( post_reg_write, \
                  Vg_CoreClientReq, zztid, O_CLREQ_RET, sizeof(UWord)); \
   } while (0)

#define SET_CLCALL_RETVAL(zztid, zzval, f) \
   do { CLREQ_RET(VG_(threads)[zztid].arch) = (zzval); \
        VG_TRACK( post_reg_write_clientcall_return, \
                  zztid, O_CLREQ_RET, sizeof(UWord), f); \
   } while (0)


/* ---------------------------------------------------------------------
   Handle client requests.
   ------------------------------------------------------------------ */

// OS-specific(?) client requests
static Bool os_client_request(ThreadId tid, UWord *args)
{
   Bool handled = True;

   vg_assert(VG_(is_running_thread)(tid));

   switch(args[0]) {
   case VG_USERREQ__FREERES_DONE:
      /* This is equivalent to an exit() syscall, but we don't set the
	 exitcode (since it might already be set) */
      if (0 || VG_(clo_trace_syscalls) || VG_(clo_trace_sched))
         VG_(message)(Vg_DebugMsg, 
                      "__gnu_cxx::__freeres() and __libc_freeres() wrapper "
                      "done; really quitting!\n");
      VG_(threads)[tid].exitreason = VgSrc_ExitThread;
      break;

   default:
      handled = False;
      break;
   }

   return handled;
}


/* Write out a client message, possibly including a back trace. Return
   the number of characters written. In case of XML output, the format
   string as well as any arguments it requires will be XML'ified. 
   I.e. special characters such as the angle brackets will be translated
   into proper escape sequences. */
static
Int print_client_message( ThreadId tid, const HChar *format,
                          va_list *vargsp, Bool include_backtrace)
{
   Int count;

   if (VG_(clo_xml)) {
      /* Translate the format string as follows:
         <  -->  &lt;
         >  -->  &gt;
         &  -->  &amp;
         %s -->  %pS
         Yes, yes, it's simplified but in synch with 
         myvprintf_str_XML_simplistic and VG_(debugLog_vprintf).
      */

      /* Allocate a buffer that is for sure large enough. */
      HChar xml_format[VG_(strlen)(format) * 5 + 1];

      const HChar *p;
      HChar *q = xml_format;

      for (p = format; *p; ++p) {
         switch (*p) {
         case '<': VG_(strcpy)(q, "&lt;");  q += 4; break;
         case '>': VG_(strcpy)(q, "&gt;");  q += 4; break;
         case '&': VG_(strcpy)(q, "&amp;"); q += 5; break;
         case '%':
            /* Careful: make sure %%s stays %%s */
            *q++ = *p++;
            if (*p == 's') {
              *q++ = 'p';
              *q++ = 'S';
            } else {
              *q++ = *p;
            }
            break;

         default:
            *q++ = *p;
            break;
         }
      }
      *q = '\0';

      VG_(printf_xml)( "<clientmsg>\n" );
      VG_(printf_xml)( "  <tid>%u</tid>\n", tid );
      const ThreadState *tst = VG_(get_ThreadState)(tid);
      if (tst->thread_name)
         VG_(printf_xml)("  <threadname>%s</threadname>\n", tst->thread_name);
      VG_(printf_xml)( "  <text>" );
      count = VG_(vprintf_xml)( xml_format, *vargsp );
      VG_(printf_xml)( "  </text>\n" );
   } else {
      count = VG_(vmessage)( Vg_ClientMsg, format, *vargsp );
      VG_(message_flush)();
   }

   if (include_backtrace)
      VG_(get_and_pp_StackTrace)( tid, VG_(clo_backtrace_size) );
   
   if (VG_(clo_xml))
      VG_(printf_xml)( "</clientmsg>\n" );

   return count;
}


/* Do a client request for the thread tid.  After the request, tid may
   or may not still be runnable; if not, the scheduler will have to
   choose a new thread to run.  
*/
static
void do_client_request ( ThreadId tid )
{
   UWord* arg = (UWord*)(Addr)(CLREQ_ARGS(VG_(threads)[tid].arch));
   UWord req_no = arg[0];

   if (0)
      VG_(printf)("req no = 0x%lx, arg = %p\n", req_no, arg);
   switch (req_no) {

      case VG_USERREQ__CLIENT_CALL0: {
         UWord (*f)(ThreadId) = (__typeof__(f))arg[1];
	 if (f == NULL)
	    VG_(message)(Vg_DebugMsg, "VG_USERREQ__CLIENT_CALL0: func=%p\n", f);
	 else
	    SET_CLCALL_RETVAL(tid, f ( tid ), (Addr)f);
         break;
      }
      case VG_USERREQ__CLIENT_CALL1: {
         UWord (*f)(ThreadId, UWord) = (__typeof__(f))arg[1];
	 if (f == NULL)
	    VG_(message)(Vg_DebugMsg, "VG_USERREQ__CLIENT_CALL1: func=%p\n", f);
	 else
	    SET_CLCALL_RETVAL(tid, f ( tid, arg[2] ), (Addr)f );
         break;
      }
      case VG_USERREQ__CLIENT_CALL2: {
         UWord (*f)(ThreadId, UWord, UWord) = (__typeof__(f))arg[1];
	 if (f == NULL)
	    VG_(message)(Vg_DebugMsg, "VG_USERREQ__CLIENT_CALL2: func=%p\n", f);
	 else
	    SET_CLCALL_RETVAL(tid, f ( tid, arg[2], arg[3] ), (Addr)f );
         break;
      }
      case VG_USERREQ__CLIENT_CALL3: {
         UWord (*f)(ThreadId, UWord, UWord, UWord) = (__typeof__(f))arg[1];
	 if (f == NULL)
	    VG_(message)(Vg_DebugMsg, "VG_USERREQ__CLIENT_CALL3: func=%p\n", f);
	 else
	    SET_CLCALL_RETVAL(tid, f ( tid, arg[2], arg[3], arg[4] ), (Addr)f );
         break;
      }

      // Nb: this looks like a circular definition, because it kind of is.
      // See comment in valgrind.h to understand what's going on.
      case VG_USERREQ__RUNNING_ON_VALGRIND:
         SET_CLREQ_RETVAL(tid, RUNNING_ON_VALGRIND+1);
         break;

      case VG_USERREQ__PRINTF: {
         const HChar* format = (HChar *)arg[1];
         /* JRS 2010-Jan-28: this is DEPRECATED; use the
            _VALIST_BY_REF version instead */
         if (sizeof(va_list) != sizeof(UWord))
            goto va_list_casting_error_NORETURN;
         union {
            va_list vargs;
            unsigned long uw;
         } u;
         u.uw = (unsigned long)arg[2];
         Int count = 
            print_client_message( tid, format, &u.vargs,
                                  /* include_backtrace */ False );
         SET_CLREQ_RETVAL( tid, count );
         break;
      }

      case VG_USERREQ__PRINTF_BACKTRACE: {
         const HChar* format = (HChar *)arg[1];
         /* JRS 2010-Jan-28: this is DEPRECATED; use the
            _VALIST_BY_REF version instead */
         if (sizeof(va_list) != sizeof(UWord))
            goto va_list_casting_error_NORETURN;
         union {
            va_list vargs;
            unsigned long uw;
         } u;
         u.uw = (unsigned long)arg[2];
         Int count =
            print_client_message( tid, format, &u.vargs,
                                  /* include_backtrace */ True );
         SET_CLREQ_RETVAL( tid, count );
         break;
      }

      case VG_USERREQ__PRINTF_VALIST_BY_REF: {
         const HChar* format = (HChar *)arg[1];
         va_list* vargsp = (va_list*)arg[2];
         Int count =
            print_client_message( tid, format, vargsp,
                                  /* include_backtrace */ False );

         SET_CLREQ_RETVAL( tid, count );
         break;
      }

      case VG_USERREQ__PRINTF_BACKTRACE_VALIST_BY_REF: {
         const HChar* format = (HChar *)arg[1];
         va_list* vargsp = (va_list*)arg[2];
         Int count =
            print_client_message( tid, format, vargsp,
                                  /* include_backtrace */ True );
         SET_CLREQ_RETVAL( tid, count );
         break;
      }

      case VG_USERREQ__INTERNAL_PRINTF_VALIST_BY_REF: {
         va_list* vargsp = (va_list*)arg[2];
         Int count = 
            VG_(vmessage)( Vg_DebugMsg, (HChar *)arg[1], *vargsp );
         VG_(message_flush)();
         SET_CLREQ_RETVAL( tid, count );
         break;
      }

      case VG_USERREQ__ADD_IFUNC_TARGET: {
         VG_(redir_add_ifunc_target)( arg[1], arg[2] );
         SET_CLREQ_RETVAL( tid, 0);
         break; }

      case VG_USERREQ__STACK_REGISTER: {
         UWord sid = VG_(register_stack)((Addr)arg[1], (Addr)arg[2]);
         SET_CLREQ_RETVAL( tid, sid );
         VG_TRACK(register_stack, (Addr)arg[1], (Addr)arg[2]);
         break; }

      case VG_USERREQ__STACK_DEREGISTER: {
         VG_(deregister_stack)(arg[1]);
         SET_CLREQ_RETVAL( tid, 0 );     /* return value is meaningless */
         break; }

      case VG_USERREQ__STACK_CHANGE: {
         VG_(change_stack)(arg[1], (Addr)arg[2], (Addr)arg[3]);
         SET_CLREQ_RETVAL( tid, 0 );     /* return value is meaningless */
         break; }

      case VG_USERREQ__GET_MALLOCFUNCS: {
	 struct vg_mallocfunc_info *info = (struct vg_mallocfunc_info *)arg[1];

	 info->tl_malloc               = VG_(tdict).tool_malloc;
	 info->tl_calloc               = VG_(tdict).tool_calloc;
	 info->tl_realloc              = VG_(tdict).tool_realloc;
	 info->tl_memalign             = VG_(tdict).tool_memalign;
	 info->tl___builtin_new        = VG_(tdict).tool___builtin_new;
	 info->tl___builtin_new_aligned = VG_(tdict).tool___builtin_new_aligned;
	 info->tl___builtin_vec_new    = VG_(tdict).tool___builtin_vec_new;
	 info->tl___builtin_vec_new_aligned    = VG_(tdict).tool___builtin_vec_new_aligned;
	 info->tl_free                 = VG_(tdict).tool_free;
	 info->tl___builtin_delete     = VG_(tdict).tool___builtin_delete;
	 info->tl___builtin_delete_aligned     = VG_(tdict).tool___builtin_delete_aligned;
	 info->tl___builtin_vec_delete = VG_(tdict).tool___builtin_vec_delete;
	 info->tl___builtin_vec_delete_aligned = VG_(tdict).tool___builtin_vec_delete_aligned;
	 info->tl_malloc_usable_size   = VG_(tdict).tool_malloc_usable_size;

	 info->mallinfo                = VG_(mallinfo);
	 info->clo_trace_malloc        = VG_(clo_trace_malloc);
         info->clo_realloc_zero_bytes_frees    = VG_(clo_realloc_zero_bytes_frees);

         SET_CLREQ_RETVAL( tid, 0 );     /* return value is meaningless */

	 break;
      }

      /* Requests from the client program */

      case VG_USERREQ__DISCARD_TRANSLATIONS:
         if (VG_(clo_verbosity) > 2)
            VG_(printf)( "client request: DISCARD_TRANSLATIONS,"
                         " addr %p,  len %lu\n",
                         (void*)arg[1], arg[2] );

         VG_(discard_translations)( 
            arg[1], arg[2], "scheduler(VG_USERREQ__DISCARD_TRANSLATIONS)" 
         );

         SET_CLREQ_RETVAL( tid, 0 );     /* return value is meaningless */
	 break;

      case VG_USERREQ__INNER_THREADS:
         if (VG_(clo_verbosity) > 2)
            VG_(printf)( "client request: INNER_THREADS,"
                         " addr %p\n",
                         (void*)arg[1] );
         VG_(inner_threads) = (ThreadState*)arg[1];
         SET_CLREQ_RETVAL( tid, 0 );     /* return value is meaningless */
	 break;

      case VG_USERREQ__COUNT_ERRORS:  
         SET_CLREQ_RETVAL( tid, VG_(get_n_errs_found)() );
         break;

      case VG_USERREQ__CLO_CHANGE:
         VG_(process_dynamic_option) (cloD, (HChar *)arg[1]);
         SET_CLREQ_RETVAL( tid, 0 );     /* return value is meaningless */
         break;

      case VG_USERREQ__LOAD_PDB_DEBUGINFO:
         VG_(di_notify_pdb_debuginfo)( arg[1], arg[2], arg[3], arg[4] );
         SET_CLREQ_RETVAL( tid, 0 );     /* return value is meaningless */
         break;

      case VG_USERREQ__MAP_IP_TO_SRCLOC: {
         Addr   ip    = arg[1];
         HChar* buf64 = (HChar*)arg[2];  // points to a HChar [64] array
         const HChar *buf;  // points to a string of unknown size

         VG_(memset)(buf64, 0, 64);
         UInt linenum = 0;

         // Unless the guest would become epoch aware (and would need to
         // describe IP addresses of dlclosed libs), using cur_ep is a
         // reasonable choice.
         const DiEpoch cur_ep = VG_(current_DiEpoch)();

         Bool ok = VG_(get_filename_linenum)(
                      cur_ep, ip, &buf, NULL, &linenum
                   );
         if (ok) {
            /* For backward compatibility truncate the filename to
               49 characters. */
            VG_(strncpy)(buf64, buf, 50);
            buf64[49] = '\0';
            UInt i;
            for (i = 0; i < 50; i++) {
               if (buf64[i] == 0)
                  break;
            }
            VG_(sprintf)(buf64+i, ":%u", linenum);  // safe
         } else {
            buf64[0] = 0;
         }

         SET_CLREQ_RETVAL( tid, 0 ); /* return value is meaningless */
         break;
      }

      case VG_USERREQ__CHANGE_ERR_DISABLEMENT: {
         Word delta = arg[1];
         vg_assert(delta == 1 || delta == -1);
         ThreadState* tst = VG_(get_ThreadState)(tid);
         vg_assert(tst);
         if (delta == 1 && tst->err_disablement_level < 0xFFFFFFFF) {
            tst->err_disablement_level++;
         }
         else
         if (delta == -1 && tst->err_disablement_level > 0) {
            tst->err_disablement_level--;
         }
         SET_CLREQ_RETVAL( tid, 0 ); /* return value is meaningless */
         break;
      }

      case VG_USERREQ__GDB_MONITOR_COMMAND: {
         UWord ret;
         ret = (UWord) VG_(client_monitor_command) ((HChar*)arg[1]);
         SET_CLREQ_RETVAL(tid, ret);
         break;
      }

      case VG_USERREQ__MALLOCLIKE_BLOCK:
      case VG_USERREQ__RESIZEINPLACE_BLOCK:
      case VG_USERREQ__FREELIKE_BLOCK:
         // Ignore them if the addr is NULL;  otherwise pass onto the tool.
         if (!arg[1]) {
            SET_CLREQ_RETVAL( tid, 0 );     /* return value is meaningless */
            break;
         } else {
            goto my_default;
         }

      case VG_USERREQ__VEX_INIT_FOR_IRI:
         LibVEX_InitIRI ( (IRICB *)arg[1] );
         break;

      default:
       my_default:
	 if (os_client_request(tid, arg)) {
	    // do nothing, os_client_request() handled it
         } else if (VG_(needs).client_requests) {
	    UWord ret;

            if (VG_(clo_verbosity) > 2)
               VG_(printf)("client request: code %lx,  addr %p,  len %lu\n",
                           arg[0], (void*)arg[1], arg[2] );

	    if ( VG_TDICT_CALL(tool_handle_client_request, tid, arg, &ret) )
	       SET_CLREQ_RETVAL(tid, ret);
         } else {
	    static Bool whined = False;

	    if (!whined && VG_(clo_verbosity) > 2) {
               // Allow for requests in core, but defined by tools, which
               // have 0 and 0 in their two high bytes.
               HChar c1 = (arg[0] >> 24) & 0xff;
               HChar c2 = (arg[0] >> 16) & 0xff;
               if (c1 == 0) c1 = '_';
               if (c2 == 0) c2 = '_';
	       VG_(message)(Vg_UserMsg, "Warning:\n"
                   "  unhandled client request: 0x%lx (%c%c+0x%lx).  Perhaps\n"
		   "  VG_(needs).client_requests should be set?\n",
			    arg[0], c1, c2, arg[0] & 0xffff);
	       whined = True;
	    }
         }
         break;
   }
   return;

   /*NOTREACHED*/
  va_list_casting_error_NORETURN:
   VG_(umsg)(
      "Valgrind: fatal error - cannot continue: use of the deprecated\n"
      "client requests VG_USERREQ__PRINTF or VG_USERREQ__PRINTF_BACKTRACE\n"
      "on a platform where they cannot be supported.  Please use the\n"
      "equivalent _VALIST_BY_REF versions instead.\n"
      "\n"
      "This is a binary-incompatible change in Valgrind's client request\n"
      "mechanism.  It is unfortunate, but difficult to avoid.  End-users\n"
      "are expected to almost never see this message.  The only case in\n"
      "which you might see this message is if your code uses the macros\n"
      "VALGRIND_PRINTF or VALGRIND_PRINTF_BACKTRACE.  If so, you will need\n"
      "to recompile such code, using the header files from this version of\n"
      "Valgrind, and not any previous version.\n"
      "\n"
      "If you see this message in any other circumstances, it is probably\n"
      "a bug in Valgrind.  In this case, please file a bug report at\n"
      "\n"
      "   http://www.valgrind.org/support/bug_reports.html\n"
      "\n"
      "Will now abort.\n"
   );
   vg_assert(0);
}


/* ---------------------------------------------------------------------
   Sanity checking (permanently engaged)
   ------------------------------------------------------------------ */

/* Internal consistency checks on the sched structures. */
static
void scheduler_sanity ( ThreadId tid )
{
   Bool bad = False;
   Int lwpid = VG_(gettid)();

   if (!VG_(is_running_thread)(tid)) {
      VG_(message)(Vg_DebugMsg,
		   "Thread %u is supposed to be running, "
                   "but doesn't own the_BigLock (owned by %u)\n", 
		   tid, VG_(running_tid));
      bad = True;
   }

   if (lwpid != VG_(threads)[tid].os_state.lwpid) {
      VG_(message)(Vg_DebugMsg,
                   "Thread %u supposed to be in LWP %d, but we're actually %d\n",
                   tid, VG_(threads)[tid].os_state.lwpid, VG_(gettid)());
      bad = True;
   }

   if (lwpid != ML_(get_sched_lock_owner)(the_BigLock)) {
      VG_(message)(Vg_DebugMsg,
                   "Thread (LWPID) %u doesn't own the_BigLock\n",
                   tid);
      bad = True;
   }

   if (0) {
      /* Periodically show the state of all threads, for debugging
         purposes. */
      static UInt lasttime = 0;
      UInt now;
      now = VG_(read_millisecond_timer)();
      if ((!bad) && (lasttime + 4000/*ms*/ <= now)) {
         lasttime = now;
         VG_(printf)("\n------------ Sched State at %d ms ------------\n",
                     (Int)now);
         VG_(show_sched_status)(True,  // host_stacktrace
                                True,  // stack_usage
                                True); // exited_threads);
      }
   }

   /* core_panic also shows the sched status, which is why we don't
      show it above if bad==True. */
   if (bad)
      VG_(core_panic)("scheduler_sanity: failed");
}

void VG_(sanity_check_general) ( Bool force_expensive )
{
   ThreadId tid;

   static UInt next_slow_check_at = 1;
   static UInt slow_check_interval = 25;

   if (VG_(clo_sanity_level) < 1) return;

   /* --- First do all the tests that we can do quickly. ---*/

   sanity_fast_count++;

   /* Check stuff pertaining to the memory check system. */

   /* Check that nobody has spuriously claimed that the first or
      last 16 pages of memory have become accessible [...] */
   if (VG_(needs).sanity_checks) {
      vg_assert(VG_TDICT_CALL(tool_cheap_sanity_check));
   }

   /* --- Now some more expensive checks. ---*/

   /* Once every now and again, check some more expensive stuff.
      Gradually increase the interval between such checks so as not to
      burden long-running programs too much. */
   if ( force_expensive
        || VG_(clo_sanity_level) >= 2
        || (VG_(clo_sanity_level) == 1 
            && sanity_fast_count == next_slow_check_at)) {

      if (0) VG_(printf)("SLOW at %u\n", sanity_fast_count-1);

      next_slow_check_at = sanity_fast_count - 1 + slow_check_interval;
      slow_check_interval++;
      sanity_slow_count++;

      if (VG_(needs).sanity_checks) {
          vg_assert(VG_TDICT_CALL(tool_expensive_sanity_check));
      }

      /* Look for stack overruns.  Visit all threads. */
      for (tid = 1; tid < VG_N_THREADS; tid++) {
	 SizeT    remains;
         VgStack* stack;

	 if (VG_(threads)[tid].status == VgTs_Empty ||
	     VG_(threads)[tid].status == VgTs_Zombie)
	    continue;

         stack 
            = (VgStack*)
              VG_(get_ThreadState)(tid)->os_state.valgrind_stack_base;
         SizeT limit
            = 4096; // Let's say.  Checking more causes lots of L2 misses.
	 remains 
            = VG_(am_get_VgStack_unused_szB)(stack, limit);
	 if (remains < limit)
	    VG_(message)(Vg_DebugMsg, 
                         "WARNING: Thread %u is within %lu bytes "
                         "of running out of valgrind stack!\n"
                         "Valgrind stack size can be increased "
                         "using --valgrind-stacksize=....\n",
		         tid, remains);
      }
   }

   if (VG_(clo_sanity_level) >= 2) {
      /* Check sanity of the low-level memory manager.  Note that bugs
         in the client's code can cause this to fail, so we don't do
         this check unless specially asked for.  And because it's
         potentially very expensive. */
      VG_(sanity_check_malloc_all)();
   }
}

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
