
/*--------------------------------------------------------------------*/
/*--- Thread scheduling.                               scheduler.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2010 Julian Seward 
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
#include "pub_core_vkiscnums.h"    // __NR_sched_yield
#include "pub_core_threadstate.h"
#include "pub_core_aspacemgr.h"
#include "pub_core_clreq.h"         // for VG_USERREQ__*
#include "pub_core_dispatch.h"
#include "pub_core_errormgr.h"      // For VG_(get_n_errs_found)()
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
#include "pub_core_signals.h"
#include "pub_core_stacks.h"
#include "pub_core_stacktrace.h"    // For VG_(get_and_pp_StackTrace)()
#include "pub_core_syscall.h"
#include "pub_core_syswrap.h"
#include "pub_core_tooliface.h"
#include "pub_core_translate.h"     // For VG_(translate)()
#include "pub_core_transtab.h"
#include "pub_core_debuginfo.h"     // VG_(di_notify_pdb_debuginfo)
#include "priv_sema.h"
#include "pub_core_scheduler.h"     // self
#include "pub_core_redir.h"


/* ---------------------------------------------------------------------
   Types and globals for the scheduler.
   ------------------------------------------------------------------ */

/* ThreadId and ThreadState are defined elsewhere*/

/* Defines the thread-scheduling timeslice, in terms of the number of
   basic blocks we attempt to run each thread for.  Smaller values
   give finer interleaving but much increased scheduling overheads. */
#define SCHEDULING_QUANTUM   100000

/* If False, a fault is Valgrind-internal (ie, a bug) */
Bool VG_(in_generated_code) = False;

/* Counts downwards in VG_(run_innerloop). */
UInt VG_(dispatch_ctr);

/* 64-bit counter for the number of basic blocks done. */
static ULong bbs_done = 0;

/* Forwards */
static void do_client_request ( ThreadId tid );
static void scheduler_sanity ( ThreadId tid );
static void mostly_clear_thread_record ( ThreadId tid );

/* Stats. */
static ULong n_scheduling_events_MINOR = 0;
static ULong n_scheduling_events_MAJOR = 0;

/* Sanity checking counts. */
static UInt sanity_fast_count = 0;
static UInt sanity_slow_count = 0;

void VG_(print_scheduler_stats)(void)
{
   VG_(message)(Vg_DebugMsg,
      "scheduler: %'llu jumps (bb entries).\n", bbs_done );
   VG_(message)(Vg_DebugMsg,
      "scheduler: %'llu/%'llu major/minor sched events.\n",
      n_scheduling_events_MAJOR, n_scheduling_events_MINOR);
   VG_(message)(Vg_DebugMsg, 
                "   sanity: %d cheap, %d expensive checks.\n",
                sanity_fast_count, sanity_slow_count );
}

/* CPU semaphore, so that threads can run exclusively */
static vg_sema_t the_BigLock;


/* ---------------------------------------------------------------------
   Helper functions for the scheduler.
   ------------------------------------------------------------------ */

static
void print_sched_event ( ThreadId tid, Char* what )
{
   VG_(message)(Vg_DebugMsg, "  SCHED[%d]: %s\n", tid, what );
}

static
HChar* name_of_sched_event ( UInt event )
{
   switch (event) {
      case VEX_TRC_JMP_SYS_SYSCALL:   return "SYSCALL";
      case VEX_TRC_JMP_SYS_INT32:     return "INT32";
      case VEX_TRC_JMP_SYS_INT128:    return "INT128";
      case VEX_TRC_JMP_SYS_INT129:    return "INT129";
      case VEX_TRC_JMP_SYS_INT130:    return "INT130";
      case VEX_TRC_JMP_SYS_SYSENTER:  return "SYSENTER";
      case VEX_TRC_JMP_CLIENTREQ:     return "CLIENTREQ";
      case VEX_TRC_JMP_YIELD:         return "YIELD";
      case VEX_TRC_JMP_NODECODE:      return "NODECODE";
      case VEX_TRC_JMP_MAPFAIL:       return "MAPFAIL";
      case VEX_TRC_JMP_NOREDIR:       return "NOREDIR";
      case VEX_TRC_JMP_EMWARN:        return "EMWARN";
      case VEX_TRC_JMP_TINVAL:        return "TINVAL";
      case VG_TRC_INVARIANT_FAILED:   return "INVFAILED";
      case VG_TRC_INNER_COUNTERZERO:  return "COUNTERZERO";
      case VG_TRC_INNER_FASTMISS:     return "FASTMISS";
      case VG_TRC_FAULT_SIGNAL:       return "FAULTSIGNAL";
      default:                        return "??UNKNOWN??";
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
         return i;
      }
   }
   VG_(printf)("vg_alloc_ThreadState: no free slots available\n");
   VG_(printf)("Increase VG_N_THREADS, rebuild and try again.\n");
   VG_(core_panic)("VG_N_THREADS is too low");
   /*NOTREACHED*/
}

/* 
   Mark a thread as Runnable.  This will block until the_BigLock is
   available, so that we get exclusive access to all the shared
   structures and the CPU.  Up until we get the_BigLock, we must not
   touch any shared state.

   When this returns, we'll actually be running.
 */
void VG_(acquire_BigLock)(ThreadId tid, HChar* who)
{
   ThreadState *tst;

#if 0
   if (VG_(clo_trace_sched)) {
      HChar buf[100];
      vg_assert(VG_(strlen)(who) <= 100-50);
      VG_(sprintf)(buf, "waiting for lock (%s)", who);
      print_sched_event(tid, buf);
   }
#endif

   /* First, acquire the_BigLock.  We can't do anything else safely
      prior to this point.  Even doing debug printing prior to this
      point is, technically, wrong. */
   ML_(sema_down)(&the_BigLock, False/*not LL*/);

   tst = VG_(get_ThreadState)(tid);

   vg_assert(tst->status != VgTs_Runnable);
   
   tst->status = VgTs_Runnable;

   if (VG_(running_tid) != VG_INVALID_THREADID)
      VG_(printf)("tid %d found %d running\n", tid, VG_(running_tid));
   vg_assert(VG_(running_tid) == VG_INVALID_THREADID);
   VG_(running_tid) = tid;

   { Addr gsp = VG_(get_SP)(tid);
     VG_(unknown_SP_update)(gsp, gsp, 0/*unknown origin*/);
   }

   if (VG_(clo_trace_sched)) {
      HChar buf[150];
      vg_assert(VG_(strlen)(who) <= 150-50);
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
void VG_(release_BigLock)(ThreadId tid, ThreadStatus sleepstate, HChar* who)
{
   ThreadState *tst = VG_(get_ThreadState)(tid);

   vg_assert(tst->status == VgTs_Runnable);

   vg_assert(sleepstate == VgTs_WaitSys ||
	     sleepstate == VgTs_Yielding);

   tst->status = sleepstate;

   vg_assert(VG_(running_tid) == tid);
   VG_(running_tid) = VG_INVALID_THREADID;

   if (VG_(clo_trace_sched)) {
      Char buf[200];
      vg_assert(VG_(strlen)(who) <= 200-100);
      VG_(sprintf)(buf, "releasing lock (%s) -> %s",
                        who, VG_(name_of_ThreadStatus)(sleepstate));
      print_sched_event(tid, buf);
   }

   /* Release the_BigLock; this will reschedule any runnable
      thread. */
   ML_(sema_up)(&the_BigLock, False/*not LL*/);
}

/* See pub_core_scheduler.h for description */
void VG_(acquire_BigLock_LL) ( HChar* who )
{
  ML_(sema_down)(&the_BigLock, True/*LL*/);
}

/* See pub_core_scheduler.h for description */
void VG_(release_BigLock_LL) ( HChar* who )
{
   ML_(sema_up)(&the_BigLock, True/*LL*/);
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

   ML_(sema_up)(&the_BigLock, False/*not LL*/);
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
                      "get_thread_out_of_syscall zaps tid %d lwp %d\n",
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
   VG_(do_syscall0)(__NR_sched_yield);

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

   /* Can't block these anyway */
   VG_(sigdelset)(&mask, VKI_SIGSTOP);
   VG_(sigdelset)(&mask, VKI_SIGKILL);

   VG_(sigprocmask)(VKI_SIG_SETMASK, &mask, NULL);
}

static void os_state_clear(ThreadState *tst)
{
   tst->os_state.lwpid       = 0;
   tst->os_state.threadgroup = 0;
#  if defined(VGO_linux)
   /* no other fields to clear */
#  elif defined(VGO_aix5)
   tst->os_state.cancel_async    = False;
   tst->os_state.cancel_disabled = False;
   tst->os_state.cancel_progress = Canc_NoRequest;
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

   vg_assert(tid >= 0 && tid < VG_N_THREADS);
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
   ML_(sema_deinit)(&the_BigLock);
   ML_(sema_init)(&the_BigLock);
   ML_(sema_down)(&the_BigLock, False/*not LL*/);
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

   ML_(sema_init)(&the_BigLock);

   for (i = 0 /* NB; not 1 */; i < VG_N_THREADS; i++) {
      /* Paranoia .. completely zero it out. */
      VG_(memset)( & VG_(threads)[i], 0, sizeof( VG_(threads)[i] ) );

      VG_(threads)[i].sig_queue = NULL;

      os_state_init(&VG_(threads)[i]);
      mostly_clear_thread_record(i);

      VG_(threads)[i].status                    = VgTs_Empty;
      VG_(threads)[i].client_stack_szB          = 0;
      VG_(threads)[i].client_stack_highest_word = (Addr)NULL;
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
   VG_(debugLog)(1,"sched","sched_init_phase2: tid_main=%d, "
                   "cls_end=0x%lx, cls_sz=%ld\n",
                   tid_main, clstack_end, clstack_size);

   vg_assert(VG_IS_PAGE_ALIGNED(clstack_end+1));
   vg_assert(VG_IS_PAGE_ALIGNED(clstack_size));

   VG_(threads)[tid_main].client_stack_highest_word 
      = clstack_end + 1 - sizeof(UWord);
   VG_(threads)[tid_main].client_stack_szB 
      = clstack_size;

   VG_(atfork)(NULL, NULL, sched_fork_cleanup);
}


/* ---------------------------------------------------------------------
   Helpers for running translations.
   ------------------------------------------------------------------ */

/* Use gcc's built-in setjmp/longjmp.  longjmp must not restore signal
   mask state, but does need to pass "val" through. */
#define SCHEDSETJMP(tid, jumped, stmt)					\
   do {									\
      ThreadState * volatile _qq_tst = VG_(get_ThreadState)(tid);	\
									\
      (jumped) = __builtin_setjmp(_qq_tst->sched_jmpbuf);               \
      if ((jumped) == 0) {						\
	 vg_assert(!_qq_tst->sched_jmpbuf_valid);			\
	 _qq_tst->sched_jmpbuf_valid = True;				\
	 stmt;								\
      }	else if (VG_(clo_trace_sched))					\
	 VG_(printf)("SCHEDSETJMP(line %d) tid %d, jumped=%d\n",        \
                     __LINE__, tid, jumped);                            \
      vg_assert(_qq_tst->sched_jmpbuf_valid);				\
      _qq_tst->sched_jmpbuf_valid = False;				\
   } while(0)


/* Do various guest state alignment checks prior to running a thread.
   Specifically, check that what we have matches Vex's guest state
   layout requirements.  See libvex.h for details, but in short the
   requirements are: There must be no holes in between the primary
   guest state, its two copies, and the spill area.  In short, all 4
   areas must have a 16-aligned size and be 16-aligned, and placed
   back-to-back. */
static void do_pre_run_checks ( ThreadState* tst )
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
   VG_(printf)("gst %p %d, sh1 %p %d, "
               "sh2 %p %d, spill %p %d\n",
               (void*)a_vex, sz_vex,
               (void*)a_vexsh1, sz_vexsh1,
               (void*)a_vexsh2, sz_vexsh2,
               (void*)a_spill, sz_spill );

   vg_assert(VG_IS_16_ALIGNED(sz_vex));
   vg_assert(VG_IS_16_ALIGNED(sz_vexsh1));
   vg_assert(VG_IS_16_ALIGNED(sz_vexsh2));
   vg_assert(VG_IS_16_ALIGNED(sz_spill));

   vg_assert(VG_IS_16_ALIGNED(a_vex));
   vg_assert(VG_IS_16_ALIGNED(a_vexsh1));
   vg_assert(VG_IS_16_ALIGNED(a_vexsh2));
   vg_assert(VG_IS_16_ALIGNED(a_spill));

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

#  if defined(VGA_amd64)
   /* x86/amd64 XMM regs must form an array, ie, have no
      holes in between. */
   vg_assert(
      (offsetof(VexGuestAMD64State,guest_XMM16)
       - offsetof(VexGuestAMD64State,guest_XMM0))
      == (17/*#regs*/-1) * 16/*bytes per reg*/
   );
#  endif

#  if defined(VGA_ppc32) || defined(VGA_ppc64)
   /* ppc guest_state vector regs must be 16 byte aligned for
      loads/stores.  This is important! */
   vg_assert(VG_IS_16_ALIGNED(& tst->arch.vex.guest_VR0));
   vg_assert(VG_IS_16_ALIGNED(& tst->arch.vex_shadow1.guest_VR0));
   vg_assert(VG_IS_16_ALIGNED(& tst->arch.vex_shadow2.guest_VR0));
   /* be extra paranoid .. */
   vg_assert(VG_IS_16_ALIGNED(& tst->arch.vex.guest_VR1));
   vg_assert(VG_IS_16_ALIGNED(& tst->arch.vex_shadow1.guest_VR1));
   vg_assert(VG_IS_16_ALIGNED(& tst->arch.vex_shadow2.guest_VR1));
#  endif

#  if defined(VGA_arm)
   /* arm guest_state VFP regs must be 8 byte aligned for
      loads/stores. */
   vg_assert(VG_IS_8_ALIGNED(& tst->arch.vex.guest_D0));
   vg_assert(VG_IS_8_ALIGNED(& tst->arch.vex_shadow1.guest_D0));
   vg_assert(VG_IS_8_ALIGNED(& tst->arch.vex_shadow2.guest_D0));
   /* be extra paranoid .. */
   vg_assert(VG_IS_8_ALIGNED(& tst->arch.vex.guest_D1));
   vg_assert(VG_IS_8_ALIGNED(& tst->arch.vex_shadow1.guest_D1));
   vg_assert(VG_IS_8_ALIGNED(& tst->arch.vex_shadow2.guest_D1));
#  endif
}


/* Run the thread tid for a while, and return a VG_TRC_* value
   indicating why VG_(run_innerloop) stopped. */
static UInt run_thread_for_a_while ( ThreadId tid )
{
   volatile Int          jumped;
   volatile ThreadState* tst = NULL; /* stop gcc complaining */
   volatile UInt         trc;
   volatile Int          dispatch_ctr_SAVED;
   volatile Int          done_this_time;

   /* Paranoia */
   vg_assert(VG_(is_valid_tid)(tid));
   vg_assert(VG_(is_running_thread)(tid));
   vg_assert(!VG_(is_exiting)(tid));

   tst = VG_(get_ThreadState)(tid);
   do_pre_run_checks( (ThreadState*)tst );
   /* end Paranoia */

   trc = 0;
   dispatch_ctr_SAVED = VG_(dispatch_ctr);

#  if defined(VGP_ppc32_aix5) || defined(VGP_ppc64_aix5)
   /* On AIX, we need to get a plausible value for SPRG3 for this
      thread, since it's used I think as a thread-state pointer.  It
      is presumably set by the kernel for each dispatched thread and
      cannot be changed by user space.  It therefore seems safe enough
      to copy the host's value of it into the guest state at the point
      the thread is dispatched.
      (Later): Hmm, looks like SPRG3 is only used in 32-bit mode.
      Oh well. */
   { UWord host_sprg3;
     __asm__ __volatile__( "mfspr %0,259\n" : "=b"(host_sprg3) );
    VG_(threads)[tid].arch.vex.guest_SPRG3_RO = host_sprg3;
    vg_assert(sizeof(VG_(threads)[tid].arch.vex.guest_SPRG3_RO) == sizeof(void*));
   }
#  endif

   /* there should be no undealt-with signals */
   //vg_assert(VG_(threads)[tid].siginfo.si_signo == 0);

   if (0) {
      vki_sigset_t m;
      Int i, err = VG_(sigprocmask)(VKI_SIG_SETMASK, NULL, &m);
      vg_assert(err == 0);
      VG_(printf)("tid %d: entering code with unblocked signals: ", tid);
      for (i = 1; i <= _VKI_NSIG; i++)
         if (!VG_(sigismember)(&m, i))
            VG_(printf)("%d ", i);
      VG_(printf)("\n");
   }

   // Tell the tool this thread is about to run client code
   VG_TRACK( start_client_code, tid, bbs_done );

   vg_assert(VG_(in_generated_code) == False);
   VG_(in_generated_code) = True;

   SCHEDSETJMP(
      tid, 
      jumped, 
      trc = (UInt)VG_(run_innerloop)( (void*)&tst->arch.vex,
                                      VG_(clo_profile_flags) > 0 ? 1 : 0 )
   );

   vg_assert(VG_(in_generated_code) == True);
   VG_(in_generated_code) = False;

   if (jumped) {
      /* We get here if the client took a fault that caused our signal
         handler to longjmp. */
      vg_assert(trc == 0);
      trc = VG_TRC_FAULT_SIGNAL;
      block_signals();
   } 

   done_this_time = (Int)dispatch_ctr_SAVED - (Int)VG_(dispatch_ctr) - 0;

   vg_assert(done_this_time >= 0);
   bbs_done += (ULong)done_this_time;

   // Tell the tool this thread has stopped running client code
   VG_TRACK( stop_client_code, tid, bbs_done );

   return trc;
}


/* Run a no-redir translation just once, and return the resulting
   VG_TRC_* value. */
static UInt run_noredir_translation ( Addr hcode, ThreadId tid )
{
   volatile Int          jumped;
   volatile ThreadState* tst; 
   volatile UWord        argblock[4];
   volatile UInt         retval;

   /* Paranoia */
   vg_assert(VG_(is_valid_tid)(tid));
   vg_assert(VG_(is_running_thread)(tid));
   vg_assert(!VG_(is_exiting)(tid));

   tst = VG_(get_ThreadState)(tid);
   do_pre_run_checks( (ThreadState*)tst );
   /* end Paranoia */

#  if defined(VGA_ppc32) || defined(VGA_ppc64)
   /* I don't think we need to clear this thread's guest_RESVN here,
      because we can only get here if run_thread_for_a_while() has
      been used immediately before, on this same thread. */
#  endif

   /* There can be 3 outcomes from VG_(run_a_noredir_translation):

      - a signal occurred and the sighandler longjmp'd.  Then both [2]
        and [3] are unchanged - hence zero.

      - translation ran normally, set [2] (next guest IP) and set [3]
        to whatever [1] was beforehand, indicating a normal (boring)
        jump to the next block.

      - translation ran normally, set [2] (next guest IP) and set [3]
        to something different from [1] beforehand, which indicates a
        TRC_ value.
   */
   argblock[0] = (UWord)hcode;
   argblock[1] = (UWord)&VG_(threads)[tid].arch.vex;
   argblock[2] = 0; /* next guest IP is written here */
   argblock[3] = 0; /* guest state ptr afterwards is written here */

   // Tell the tool this thread is about to run client code
   VG_TRACK( start_client_code, tid, bbs_done );

   vg_assert(VG_(in_generated_code) == False);
   VG_(in_generated_code) = True;

   SCHEDSETJMP(
      tid, 
      jumped, 
      VG_(run_a_noredir_translation)( &argblock[0] )
   );

   VG_(in_generated_code) = False;

   if (jumped) {
      /* We get here if the client took a fault that caused our signal
         handler to longjmp. */
      vg_assert(argblock[2] == 0); /* next guest IP was not written */
      vg_assert(argblock[3] == 0); /* trc was not written */
      block_signals();
      retval = VG_TRC_FAULT_SIGNAL;
   } else {
      /* store away the guest program counter */
      VG_(set_IP)( tid, argblock[2] );
      if (argblock[3] == argblock[1])
         /* the guest state pointer afterwards was unchanged */
         retval = VG_TRC_BORING;
      else
         retval = (UInt)argblock[3];
   }

   bbs_done++;

   // Tell the tool this thread has stopped running client code
   VG_TRACK( stop_client_code, tid, bbs_done );

   return retval;
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
   found = VG_(search_transtab)( NULL, ip, True/*upd_fast_cache*/ );
   if (UNLIKELY(!found)) {
      /* Not found; we need to request a translation. */
      if (VG_(translate)( tid, ip, /*debug*/False, 0/*not verbose*/, 
                          bbs_done, True/*allow redirection*/ )) {
	 found = VG_(search_transtab)( NULL, ip, True ); 
         vg_assert2(found, "VG_TRC_INNER_FASTMISS: missing tt_fast entry");
      
      } else {
	 // If VG_(translate)() fails, it's because it had to throw a
	 // signal because the client jumped to a bad address.  That
	 // means that either a signal has been set up for delivery,
	 // or the thread has been marked for termination.  Either
	 // way, we just need to go back into the scheduler loop.
      }
   }
}

static void handle_syscall(ThreadId tid, UInt trc)
{
   ThreadState * volatile tst = VG_(get_ThreadState)(tid);
   Bool jumped; 

   /* Syscall may or may not block; either way, it will be
      complete by the time this call returns, and we'll be
      runnable again.  We could take a signal while the
      syscall runs. */

   if (VG_(clo_sanity_level >= 3))
      VG_(am_do_sync_check)("(BEFORE SYSCALL)",__FILE__,__LINE__);

   SCHEDSETJMP(tid, jumped, VG_(client_syscall)(tid, trc));

   if (VG_(clo_sanity_level >= 3))
      VG_(am_do_sync_check)("(AFTER SYSCALL)",__FILE__,__LINE__);

   if (!VG_(is_running_thread)(tid))
      VG_(printf)("tid %d not running; VG_(running_tid)=%d, tid %d status %d\n",
		  tid, VG_(running_tid), tid, tst->status);
   vg_assert(VG_(is_running_thread)(tid));
   
   if (jumped) {
      block_signals();
      VG_(poll_signals)(tid);
   }
}

/* tid just requested a jump to the noredir version of its current
   program counter.  So make up that translation if needed, run it,
   and return the resulting thread return code. */
static UInt/*trc*/ handle_noredir_jump ( ThreadId tid )
{
   AddrH hcode = 0;
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
         return VG_TRC_BORING;
      }

   }

   vg_assert(found);
   vg_assert(hcode != 0);

   /* Otherwise run it and return the resulting VG_TRC_* value. */ 
   return run_noredir_translation( hcode, tid );
}


/* 
   Run a thread until it wants to exit.
   
   We assume that the caller has already called VG_(acquire_BigLock) for
   us, so we own the VCPU.  Also, all signals are blocked.
 */
VgSchedReturnCode VG_(scheduler) ( ThreadId tid )
{
   UInt     trc;
   ThreadState *tst = VG_(get_ThreadState)(tid);

   if (VG_(clo_trace_sched))
      print_sched_event(tid, "entering VG_(scheduler)");      

   /* set the proper running signal mask */
   block_signals();
   
   vg_assert(VG_(is_running_thread)(tid));

   VG_(dispatch_ctr) = SCHEDULING_QUANTUM + 1;

   while (!VG_(is_exiting)(tid)) {

      if (VG_(dispatch_ctr) == 1) {

#        if defined(VGP_ppc32_aix5) || defined(VGP_ppc64_aix5)
         /* Note: count runnable threads before dropping The Lock. */
         Int rt = VG_(count_runnable_threads)();
#        endif

	 /* Our slice is done, so yield the CPU to another thread.  On
            Linux, this doesn't sleep between sleeping and running,
            since that would take too much time.  On AIX, we have to
            prod the scheduler to get it consider other threads; not
            doing so appears to cause very long delays before other
            runnable threads get rescheduled. */

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

#        if defined(VGP_ppc32_aix5) || defined(VGP_ppc64_aix5)
         { static Int ctr=0;
           vg_assert(__NR_AIX5__nsleep != __NR_AIX5_UNKNOWN);
           vg_assert(__NR_AIX5_yield   != __NR_AIX5_UNKNOWN);
           if (1 && rt > 0 && ((++ctr % 3) == 0)) { 
              //struct vki_timespec ts;
              //ts.tv_sec = 0;
              //ts.tv_nsec = 0*1000*1000;
              //VG_(do_syscall2)(__NR_AIX5__nsleep, (UWord)&ts, (UWord)NULL);
	      VG_(do_syscall0)(__NR_AIX5_yield);
           }
         }
#        endif

	 VG_(acquire_BigLock)(tid, "VG_(scheduler):timeslice");
	 /* ------------ now we do have The Lock ------------ */

	 /* OK, do some relatively expensive housekeeping stuff */
	 scheduler_sanity(tid);
	 VG_(sanity_check_general)(False);

	 /* Look for any pending signals for this thread, and set them up
	    for delivery */
	 VG_(poll_signals)(tid);

	 if (VG_(is_exiting)(tid))
	    break;		/* poll_signals picked up a fatal signal */

	 /* For stats purposes only. */
	 n_scheduling_events_MAJOR++;

	 /* Figure out how many bbs to ask vg_run_innerloop to do.  Note
	    that it decrements the counter before testing it for zero, so
	    that if tst->dispatch_ctr is set to N you get at most N-1
	    iterations.  Also this means that tst->dispatch_ctr must
	    exceed zero before entering the innerloop.  Also also, the
	    decrement is done before the bb is actually run, so you
	    always get at least one decrement even if nothing happens. */
         VG_(dispatch_ctr) = SCHEDULING_QUANTUM + 1;

	 /* paranoia ... */
	 vg_assert(tst->tid == tid);
	 vg_assert(tst->os_state.lwpid == VG_(gettid)());
      }

      /* For stats purposes only. */
      n_scheduling_events_MINOR++;

      if (0)
         VG_(message)(Vg_DebugMsg, "thread %d: running for %d bbs\n", 
                                   tid, VG_(dispatch_ctr) - 1 );

      trc = run_thread_for_a_while ( tid );

      if (VG_(clo_trace_sched) && VG_(clo_verbosity) > 2) {
	 Char buf[50];
	 VG_(sprintf)(buf, "TRC: %s", name_of_sched_event(trc));
	 print_sched_event(tid, buf);
      }

      if (trc == VEX_TRC_JMP_NOREDIR) {
         /* If we got a request to run a no-redir version of
            something, do so now -- handle_noredir_jump just (creates
            and) runs that one translation.  The flip side is that the
            noredir translation can't itself return another noredir
            request -- that would be nonsensical.  It can, however,
            return VG_TRC_BORING, which just means keep going as
            normal. */
         trc = handle_noredir_jump(tid);
         vg_assert(trc != VEX_TRC_JMP_NOREDIR);
      }

      switch (trc) {
      case VG_TRC_BORING:
         /* no special event, just keep going. */
         break;

      case VG_TRC_INNER_FASTMISS:
	 vg_assert(VG_(dispatch_ctr) > 1);
	 handle_tt_miss(tid);
	 break;
	    
      case VEX_TRC_JMP_CLIENTREQ:
	 do_client_request(tid);
	 break;

      case VEX_TRC_JMP_SYS_INT128:  /* x86-linux */
      case VEX_TRC_JMP_SYS_INT129:  /* x86-darwin */
      case VEX_TRC_JMP_SYS_INT130:  /* x86-darwin */
      case VEX_TRC_JMP_SYS_SYSCALL: /* amd64-linux, ppc32-linux, amd64-darwin */
	 handle_syscall(tid, trc);
	 if (VG_(clo_sanity_level) > 2)
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
	 if (VG_(dispatch_ctr) > 2000) 
            VG_(dispatch_ctr) = 2000;
	 break;

      case VG_TRC_INNER_COUNTERZERO:
	 /* Timeslice is out.  Let a new thread be scheduled. */
	 vg_assert(VG_(dispatch_ctr) == 1);
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
         static Int  counts[EmWarn_NUMBER];
         static Bool counts_initted = False;
         VexEmWarn ew;
         HChar*    what;
         Bool      show;
         Int       q;
         if (!counts_initted) {
            counts_initted = True;
            for (q = 0; q < EmWarn_NUMBER; q++)
               counts[q] = 0;
         }
         ew   = (VexEmWarn)VG_(threads)[tid].arch.vex.guest_EMWARN;
         what = (ew < 0 || ew >= EmWarn_NUMBER)
                   ? "unknown (?!)"
                   : LibVEX_EmWarn_string(ew);
         show = (ew < 0 || ew >= EmWarn_NUMBER)
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
         VexEmWarn ew;
         HChar*    what;
         ew   = (VexEmWarn)VG_(threads)[tid].arch.vex.guest_EMWARN;
         what = (ew < 0 || ew >= EmWarn_NUMBER)
                   ? "unknown (?!)"
                   : LibVEX_EmWarn_string(ew);
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

      case VEX_TRC_JMP_SIGTRAP:
         VG_(synth_sigtrap)(tid);
         break;

      case VEX_TRC_JMP_SIGSEGV:
         VG_(synth_fault)(tid);
         break;

      case VEX_TRC_JMP_SIGBUS:
         VG_(synth_sigbus)(tid);
         break;

      case VEX_TRC_JMP_NODECODE:
         VG_(umsg)(
            "valgrind: Unrecognised instruction at address %#lx.\n",
            VG_(get_IP)(tid));
         VG_(get_and_pp_StackTrace)(tid, 50);
#define M(a) VG_(umsg)(a "\n");
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
#undef M
         VG_(synth_sigill)(tid, VG_(get_IP)(tid));
         break;

      case VEX_TRC_JMP_TINVAL:
         VG_(discard_translations)(
            (Addr64)VG_(threads)[tid].arch.vex.guest_TISTART,
            VG_(threads)[tid].arch.vex.guest_TILEN,
            "scheduler(VEX_TRC_JMP_TINVAL)"
         );
         if (0)
            VG_(printf)("dump translations done.\n");
         break;

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
#        elif defined(VGP_x86_darwin)
         /* return address in client edx */
         VG_(threads)[tid].arch.vex.guest_EIP
            = VG_(threads)[tid].arch.vex.guest_EDX;
         handle_syscall(tid, trc);
#        else
         vg_assert2(0, "VG_(scheduler), phase 3: "
                       "sysenter_x86 on non-x86 platform?!?!");
#        endif
         break;

      default: 
	 vg_assert2(0, "VG_(scheduler), phase 3: "
                       "unexpected thread return code (%u)", trc);
	 /* NOTREACHED */
	 break;

      } /* switch (trc) */
   }

   if (VG_(clo_trace_sched))
      print_sched_event(tid, "exiting VG_(scheduler)");

   vg_assert(VG_(is_exiting)(tid));

   return tst->exitreason;
}


/* 
   This causes all threads to forceably exit.  They aren't actually
   dead by the time this returns; you need to call
   VG_(reap_threads)() to wait for them.
 */
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
            "VG_(nuke_all_threads_except): nuking tid %d\n", tid);

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
#elif defined(VGA_ppc32) || defined(VGA_ppc64)
#  define VG_CLREQ_ARGS       guest_GPR4
#  define VG_CLREQ_RET        guest_GPR3
#elif defined(VGA_arm)
#  define VG_CLREQ_ARGS       guest_R4
#  define VG_CLREQ_RET        guest_R3
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
   case VG_USERREQ__LIBC_FREERES_DONE:
      /* This is equivalent to an exit() syscall, but we don't set the
	 exitcode (since it might already be set) */
      if (0 || VG_(clo_trace_syscalls) || VG_(clo_trace_sched))
         VG_(message)(Vg_DebugMsg, 
                      "__libc_freeres() done; really quitting!\n");
      VG_(threads)[tid].exitreason = VgSrc_ExitThread;
      break;

   default:
      handled = False;
      break;
   }

   return handled;
}


/* Do a client request for the thread tid.  After the request, tid may
   or may not still be runnable; if not, the scheduler will have to
   choose a new thread to run.  
*/
static
void do_client_request ( ThreadId tid )
{
   UWord* arg = (UWord*)(CLREQ_ARGS(VG_(threads)[tid].arch));
   UWord req_no = arg[0];

   if (0)
      VG_(printf)("req no = 0x%llx, arg = %p\n", (ULong)req_no, arg);
   switch (req_no) {

      case VG_USERREQ__CLIENT_CALL0: {
         UWord (*f)(ThreadId) = (void*)arg[1];
	 if (f == NULL)
	    VG_(message)(Vg_DebugMsg, "VG_USERREQ__CLIENT_CALL0: func=%p\n", f);
	 else
	    SET_CLCALL_RETVAL(tid, f ( tid ), (Addr)f);
         break;
      }
      case VG_USERREQ__CLIENT_CALL1: {
         UWord (*f)(ThreadId, UWord) = (void*)arg[1];
	 if (f == NULL)
	    VG_(message)(Vg_DebugMsg, "VG_USERREQ__CLIENT_CALL1: func=%p\n", f);
	 else
	    SET_CLCALL_RETVAL(tid, f ( tid, arg[2] ), (Addr)f );
         break;
      }
      case VG_USERREQ__CLIENT_CALL2: {
         UWord (*f)(ThreadId, UWord, UWord) = (void*)arg[1];
	 if (f == NULL)
	    VG_(message)(Vg_DebugMsg, "VG_USERREQ__CLIENT_CALL2: func=%p\n", f);
	 else
	    SET_CLCALL_RETVAL(tid, f ( tid, arg[2], arg[3] ), (Addr)f );
         break;
      }
      case VG_USERREQ__CLIENT_CALL3: {
         UWord (*f)(ThreadId, UWord, UWord, UWord) = (void*)arg[1];
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
            VG_(vmessage)( Vg_ClientMsg, (char *)arg[1], u.vargs );
         VG_(message_flush)();
         SET_CLREQ_RETVAL( tid, count );
         break;
      }

      case VG_USERREQ__PRINTF_BACKTRACE: {
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
            VG_(vmessage)( Vg_ClientMsg, (char *)arg[1], u.vargs );
         VG_(message_flush)();
         VG_(get_and_pp_StackTrace)( tid, VG_(clo_backtrace_size) );
         SET_CLREQ_RETVAL( tid, count );
         break;
      }

      case VG_USERREQ__PRINTF_VALIST_BY_REF: {
         va_list* vargsp = (va_list*)arg[2];
         Int count = 
            VG_(vmessage)( Vg_ClientMsg, (char *)arg[1], *vargsp );
         VG_(message_flush)();
         SET_CLREQ_RETVAL( tid, count );
         break;
      }

      case VG_USERREQ__PRINTF_BACKTRACE_VALIST_BY_REF: {
         va_list* vargsp = (va_list*)arg[2];
         Int count =
            VG_(vmessage)( Vg_ClientMsg, (char *)arg[1], *vargsp );
         VG_(message_flush)();
         VG_(get_and_pp_StackTrace)( tid, VG_(clo_backtrace_size) );
         SET_CLREQ_RETVAL( tid, count );
         break;
      }

      case VG_USERREQ__INTERNAL_PRINTF_VALIST_BY_REF: {
         va_list* vargsp = (va_list*)arg[2];
         Int count = 
            VG_(vmessage)( Vg_DebugMsg, (char *)arg[1], *vargsp );
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
	 info->tl___builtin_vec_new    = VG_(tdict).tool___builtin_vec_new;
	 info->tl_free                 = VG_(tdict).tool_free;
	 info->tl___builtin_delete     = VG_(tdict).tool___builtin_delete;
	 info->tl___builtin_vec_delete = VG_(tdict).tool___builtin_vec_delete;
         info->tl_malloc_usable_size   = VG_(tdict).tool_malloc_usable_size;

	 info->mallinfo                = VG_(mallinfo);
	 info->clo_trace_malloc        = VG_(clo_trace_malloc);

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

      case VG_USERREQ__COUNT_ERRORS:  
         SET_CLREQ_RETVAL( tid, VG_(get_n_errs_found)() );
         break;

      case VG_USERREQ__LOAD_PDB_DEBUGINFO:
         VG_(di_notify_pdb_debuginfo)( arg[1], arg[2], arg[3], arg[4] );
         SET_CLREQ_RETVAL( tid, 0 );     /* return value is meaningless */
         break;

      case VG_USERREQ__MAP_IP_TO_SRCLOC: {
         Addr   ip    = arg[1];
         UChar* buf64 = (UChar*)arg[2];

         VG_(memset)(buf64, 0, 64);
         UInt linenum = 0;
         Bool ok = VG_(get_filename_linenum)(
                      ip, &buf64[0], 50, NULL, 0, NULL, &linenum
                   );
         if (ok) {
            /* Find the terminating zero in the first 50 bytes. */
            UInt i;
            for (i = 0; i < 50; i++) {
               if (buf64[i] == 0)
                  break;
            }
            /* We must find a zero somewhere in 0 .. 49.  Else
               VG_(get_filename_linenum) is not properly zero
               terminating. */
            vg_assert(i < 50);
            VG_(sprintf)(&buf64[i], ":%u", linenum);
         } else {
            buf64[0] = 0;
         }

         SET_CLREQ_RETVAL( tid, 0 ); /* return value is meaningless */
         break;
      }

      case VG_USERREQ__MALLOCLIKE_BLOCK:
      case VG_USERREQ__FREELIKE_BLOCK:
         // Ignore them if the addr is NULL;  otherwise pass onto the tool.
         if (!arg[1]) {
            SET_CLREQ_RETVAL( tid, 0 );     /* return value is meaningless */
            break;
         } else {
            goto my_default;
         }

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
               Char c1 = (arg[0] >> 24) & 0xff;
               Char c2 = (arg[0] >> 16) & 0xff;
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
      "If you see this mesage in any other circumstances, it is probably\n"
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
   static UInt lasttime = 0;
   UInt now;
   Int lwpid = VG_(gettid)();

   if (!VG_(is_running_thread)(tid)) {
      VG_(message)(Vg_DebugMsg,
		   "Thread %d is supposed to be running, "
                   "but doesn't own the_BigLock (owned by %d)\n", 
		   tid, VG_(running_tid));
      bad = True;
   }

   if (lwpid != VG_(threads)[tid].os_state.lwpid) {
      VG_(message)(Vg_DebugMsg,
                   "Thread %d supposed to be in LWP %d, but we're actually %d\n",
                   tid, VG_(threads)[tid].os_state.lwpid, VG_(gettid)());
      bad = True;
   }

#if !defined(VGO_darwin)
   // GrP fixme
   if (lwpid != the_BigLock.owner_lwpid) {
      VG_(message)(Vg_DebugMsg,
                   "Thread (LWPID) %d doesn't own the_BigLock\n",
                   tid);
      bad = True;
   }
#endif

   /* Periodically show the state of all threads, for debugging
      purposes. */
   now = VG_(read_millisecond_timer)();
   if (0 && (!bad) && (lasttime + 4000/*ms*/ <= now)) {
      lasttime = now;
      VG_(printf)("\n------------ Sched State at %d ms ------------\n",
                  (Int)now);
      VG_(show_sched_status)();
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
        || VG_(clo_sanity_level) > 1
        || (VG_(clo_sanity_level) == 1 
            && sanity_fast_count == next_slow_check_at)) {

      if (0) VG_(printf)("SLOW at %d\n", sanity_fast_count-1);

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
                         "WARNING: Thread %d is within %ld bytes "
                         "of running out of stack!\n",
		         tid, remains);
      }
   }

   if (VG_(clo_sanity_level) > 1) {
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
