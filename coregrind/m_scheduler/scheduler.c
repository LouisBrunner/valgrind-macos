
/*--------------------------------------------------------------------*/
/*--- Thread scheduling.                               scheduler.c ---*/
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

/* 
   Overview

   Valgrind tries to emulate the kernel's threading as closely as
   possible.  The client does all threading via the normal syscalls
   (on Linux: clone, etc).  Valgrind emulates this by creating exactly
   the same process structure as would be created without Valgrind.
   There are no extra threads.

   The main difference is that Valgrind only allows one client thread
   to run at once.  This is controlled with the VCPU semaphore,
   "run_sema".  Any time a thread wants to run client code or
   manipulate any shared state (which is anything other than its own
   ThreadState entry), it must hold the run_sema.

   When a thread is about to block in a blocking syscall, it releases
   run_sema, and re-takes it when it becomes runnable again (either
   because the syscall finished, or we took a signal).

   VG_(scheduler) therefore runs in each thread.  It returns only when
   the thread is exiting, either because it exited itself, or it was
   told to exit by another thread.

   This file is almost entirely OS-independent.  The details of how
   the OS handles threading and signalling are abstracted away and
   implemented elsewhere.  [Some of the functions have worked their
   way back for the moment, until we do an OS port in earnest...]
 */

#include "valgrind.h"   // for VG_USERREQ__*
#include "coregrind.h"  // for VG_USERREQ__*

#include "pub_core_basics.h"
#include "pub_core_threadstate.h"
#include "pub_core_aspacemgr.h"
#include "pub_core_dispatch.h"
#include "pub_core_errormgr.h"      // For VG_(get_n_errs_found)()
#include "pub_core_libcbase.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcprint.h"
#include "pub_core_libcproc.h"
#include "pub_core_libcsignal.h"
#include "pub_core_machine.h"
#include "pub_core_mallocfree.h"
#include "pub_core_options.h"
#include "pub_core_profile.h"
#include "pub_core_replacemalloc.h"
#include "pub_core_scheduler.h"
#include "pub_core_signals.h"
#include "pub_core_stacks.h"
#include "pub_core_stacktrace.h"    // For VG_(get_and_pp_StackTrace)()
#include "pub_core_syscall.h"
#include "pub_core_syswrap.h"
#include "pub_core_tooliface.h"
#include "pub_core_translate.h"     // For VG_(translate)()
#include "pub_core_transtab.h"
#include "vki_unistd.h"
#include "priv_sema.h"

/* ---------------------------------------------------------------------
   Types and globals for the scheduler.
   ------------------------------------------------------------------ */

/* ThreadId and ThreadState are defined elsewhere*/

/* Defines the thread-scheduling timeslice, in terms of the number of
   basic blocks we attempt to run each thread for.  Smaller values
   give finer interleaving but much increased scheduling overheads. */
#define SCHEDULING_QUANTUM   50000

/* If true, a fault is Valgrind-internal (ie, a bug) */
Bool VG_(my_fault) = True;

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
      "scheduler: %,llu jumps (bb entries).", bbs_done );
   VG_(message)(Vg_DebugMsg,
      "scheduler: %,llu/%,llu major/minor sched events.", 
      n_scheduling_events_MAJOR, n_scheduling_events_MINOR);
   VG_(message)(Vg_DebugMsg, 
                "   sanity: %d cheap, %d expensive checks.",
                sanity_fast_count, sanity_slow_count );
}

/* CPU semaphore, so that threads can run exclusively */
static vg_sema_t run_sema;


/* ---------------------------------------------------------------------
   Helper functions for the scheduler.
   ------------------------------------------------------------------ */

static
void print_sched_event ( ThreadId tid, Char* what )
{
   VG_(message)(Vg_DebugMsg, "  SCHED[%d]: %s", tid, what );
}

static
HChar* name_of_sched_event ( UInt event )
{
   switch (event) {
      case VEX_TRC_JMP_SYSCALL:       return "SYSCALL";
      case VEX_TRC_JMP_CLIENTREQ:     return "CLIENTREQ";
      case VEX_TRC_JMP_YIELD:         return "YIELD";
      case VEX_TRC_JMP_NODECODE:      return "NODECODE";
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
   Mark a thread as Runnable.  This will block until the run_sema is
   available, so that we get exclusive access to all the shared
   structures and the CPU.  Up until we get the sema, we must not
   touch any shared state.

   When this returns, we'll actually be running.
 */
void VG_(set_running)(ThreadId tid)
{
   ThreadState *tst = VG_(get_ThreadState)(tid);

   vg_assert(tst->status != VgTs_Runnable);
   
   tst->status = VgTs_Runnable;
   
   ML_(sema_down)(&run_sema);
   if (VG_(running_tid) != VG_INVALID_THREADID)
      VG_(printf)("tid %d found %d running\n", tid, VG_(running_tid));
   vg_assert(VG_(running_tid) == VG_INVALID_THREADID);
   VG_(running_tid) = tid;

   if (VG_(clo_trace_sched))
      print_sched_event(tid, "now running");

   // While thre modeling is disable, issue thread_run events here
   // VG_(tm_thread_switchto)(tid);
   VG_TRACK( thread_run, tid );
}

/* 
   Set a thread into a sleeping state, and give up exclusive access to
   the CPU.  On return, the thread must be prepared to block until it
   is ready to run again (generally this means blocking in a syscall,
   but it may mean that we remain in a Runnable state and we're just
   yielding the CPU to another thread).
 */
void VG_(set_sleeping)(ThreadId tid, ThreadStatus sleepstate)
{
   ThreadState *tst = VG_(get_ThreadState)(tid);

   vg_assert(tst->status == VgTs_Runnable);

   vg_assert(sleepstate == VgTs_WaitSys ||
	     sleepstate == VgTs_Yielding);

   tst->status = sleepstate;

   vg_assert(VG_(running_tid) == tid);
   VG_(running_tid) = VG_INVALID_THREADID;

   /* Release the run_sema; this will reschedule any runnable
      thread. */
   ML_(sema_up)(&run_sema);

   if (VG_(clo_trace_sched)) {
      Char buf[50];
      VG_(sprintf)(buf, "now sleeping in state %s", 
                        VG_(name_of_ThreadStatus)(sleepstate));
      print_sched_event(tid, buf);
   }
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

   ML_(sema_up)(&run_sema);
}

/* Kill a thread.  This interrupts whatever a thread is doing, and
   makes it exit ASAP.  This does not set the exitreason or
   exitcode. */
void VG_(kill_thread)(ThreadId tid)
{
   vg_assert(VG_(is_valid_tid)(tid));
   vg_assert(!VG_(is_running_thread)(tid));
   vg_assert(VG_(is_exiting)(tid));

   if (VG_(threads)[tid].status == VgTs_WaitSys) {
      if (VG_(clo_trace_signals))
	 VG_(message)(Vg_DebugMsg, "kill_thread zaps tid %d lwp %d",
		      tid, VG_(threads)[tid].os_state.lwpid);
      VG_(tkill)(VG_(threads)[tid].os_state.lwpid, VG_SIGVGKILL);
   }
}

/* 
   Yield the CPU for a short time to let some other thread run.
 */
void VG_(vg_yield)(void)
{
   struct vki_timespec ts = { 0, 1 };
   ThreadId tid = VG_(running_tid);

   vg_assert(tid != VG_INVALID_THREADID);
   vg_assert(VG_(threads)[tid].os_state.lwpid == VG_(gettid)());

   VG_(set_sleeping)(tid, VgTs_Yielding);

   //VG_(printf)("tid %d yielding EIP=%p\n", tid, VG_(threads)[tid].arch.m_eip);

   /* 
      Tell the kernel we're yielding.
    */
   if (1)
      VG_(do_syscall0)(__NR_sched_yield);
   else
      VG_(nanosleep)(&ts);

   VG_(set_running)(tid);
}


/* Set the standard set of blocked signals, used wheneever we're not
   running a client syscall. */
static void block_signals(ThreadId tid)
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

/* Use libc setjmp/longjmp.  longjmp must not restore signal mask
   state, but does need to pass "val" through. */
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
	 VG_(printf)("SCHEDSETJMP(line %d) tid %d, jumped=%d\n", __LINE__, tid, jumped); \
      vg_assert(_qq_tst->sched_jmpbuf_valid);				\
      _qq_tst->sched_jmpbuf_valid = False;				\
   } while(0)

/* Run the thread tid for a while, and return a VG_TRC_* value to the
   scheduler indicating what happened. */
static
UInt run_thread_for_a_while ( ThreadId tid )
{
   volatile Bool jumped;
   volatile ThreadState *tst = VG_(get_ThreadState)(tid);

   volatile UInt trc = 0;
   volatile Int  dispatch_ctr_SAVED = VG_(dispatch_ctr);
   volatile Int  done_this_time;

   /* For paranoia purposes only */
   volatile Addr a_vex    = (Addr) & VG_(threads)[tid].arch.vex;
   volatile Addr a_vexsh  = (Addr) & VG_(threads)[tid].arch.vex_shadow;
   volatile Addr a_spill  = (Addr) & VG_(threads)[tid].arch.vex_spill;
   volatile UInt sz_vex   = (UInt) sizeof VG_(threads)[tid].arch.vex;
   volatile UInt sz_vexsh = (UInt) sizeof VG_(threads)[tid].arch.vex_shadow;
   volatile UInt sz_spill = (UInt) sizeof VG_(threads)[tid].arch.vex_spill;

   /* Paranoia */
   vg_assert(VG_(is_valid_tid)(tid));
   vg_assert(VG_(is_valid_tid)(tid));
   vg_assert(VG_(is_running_thread)(tid));
   vg_assert(!VG_(is_exiting)(tid));

   /* Even more paranoia.  Check that what we have matches
      Vex's guest state layout requirements. */
   if (0)
   VG_(printf)("%p %d %p %d %p %d\n",
               (void*)a_vex, sz_vex, (void*)a_vexsh, sz_vexsh,
               (void*)a_spill, sz_spill );

   vg_assert(VG_IS_8_ALIGNED(sz_vex));
   vg_assert(VG_IS_8_ALIGNED(sz_vexsh));
   vg_assert(VG_IS_16_ALIGNED(sz_spill));

   vg_assert(VG_IS_4_ALIGNED(a_vex));
   vg_assert(VG_IS_4_ALIGNED(a_vexsh));
   vg_assert(VG_IS_4_ALIGNED(a_spill));

   vg_assert(sz_vex == sz_vexsh);
   vg_assert(a_vex + sz_vex == a_vexsh);

   vg_assert(sz_spill == LibVEX_N_SPILL_BYTES);
   vg_assert(a_vex + 2 * sz_vex == a_spill);

   VGP_PUSHCC(VgpRun);

#  if defined(VGA_ppc32)
   /* This is necessary due to the hacky way vex models reservations
      on ppc.  It's really quite incorrect for each thread to have its
      own reservation flag/address, since it's really something that
      all threads share (that's the whole point).  But having shared
      guest state is something we can't model with Vex.  However, as
      per PaulM's 2.4.0ppc, the reservation is modelled using a
      reservation flag which is cleared at each context switch.  So it
      is indeed possible to get away with a per thread-reservation if
      the thread's reservation is cleared before running it.

      This should be abstractified and lifted out.
   */
   { Int i;
     /* Clear any existing reservation.  Be paranoid and clear them all. */
     for (i = 0; i < VG_N_THREADS; i++)
        VG_(threads)[i].arch.vex.guest_RESVN = 0;
   }

   /* ppc guest_state vector regs must be 16byte aligned for loads/stores */
   vg_assert(VG_IS_16_ALIGNED(VG_(threads)[tid].arch.vex.guest_VR0));
   vg_assert(VG_IS_16_ALIGNED(VG_(threads)[tid].arch.vex_shadow.guest_VR0));
#  endif   

   /* there should be no undealt-with signals */
   //vg_assert(VG_(threads)[tid].siginfo.si_signo == 0);

   //VG_(printf)("running EIP = %p ESP=%p\n", VG_(threads)[tid].arch.m_eip, VG_(threads)[tid].arch.m_esp);

   vg_assert(VG_(my_fault));
   VG_(my_fault) = False;

   SCHEDSETJMP(tid, jumped, 
                    trc = (UInt)VG_(run_innerloop)( (void*)&tst->arch.vex ));

   //nextEIP = tst->arch.m_eip;
   //if (nextEIP >= VG_(client_end))
   //   VG_(printf)("trc=%d jump to %p from %p\n",
   //		  trc, nextEIP, EIP);
   
   VG_(my_fault) = True;

   if (jumped) {
      /* We get here if the client took a fault, which caused our
         signal handler to longjmp. */
      vg_assert(trc == 0);
      trc = VG_TRC_FAULT_SIGNAL;
      block_signals(tid);
   } 

   done_this_time = (Int)dispatch_ctr_SAVED - (Int)VG_(dispatch_ctr) - 0;

   vg_assert(done_this_time >= 0);
   bbs_done += (ULong)done_this_time;

   VGP_POPCC(VgpRun);
   return trc;
}


static void os_state_clear(ThreadState *tst)
{
   tst->os_state.lwpid       = 0;
   tst->os_state.threadgroup = 0;
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
   threads, then we've inhereted a VG_(threads) array describing them,         
   but only the thread which called fork() is actually alive in the            
   child.  This functions needs to clean up all those other thread             
   structures.                                                                 
                                                                               
   Whichever tid in the parent which called fork() becomes the                 
   master_tid in the child.  That's because the only living slot in            
   VG_(threads) in the child after fork is VG_(threads)[tid], and it           
   would be too hard to try to re-number the thread and relocate the           
   thread state down to VG_(threads)[1].                                       
                                                                               
   This function also needs to reinitialize the run_sema, since                
   otherwise we may end up sharing its state with the parent, which            
   would be deeply confusing.                                                  
*/                                          
static void sched_fork_cleanup(ThreadId me)
{
   ThreadId tid;
   vg_assert(VG_(running_tid) == me);

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
   ML_(sema_deinit)(&run_sema);
   ML_(sema_init)(&run_sema);
   ML_(sema_down)(&run_sema);
}


/* Initialise the scheduler.  Create a single "main" thread ready to
   run, with special ThreadId of one.  This is called at startup.  The
   caller subsequently initialises the guest state components of this
   main thread, thread 1.  
*/
void VG_(scheduler_init) ( Addr clstack_end, SizeT clstack_size )
{
   Int i;
   ThreadId tid_main;

   vg_assert(VG_IS_PAGE_ALIGNED(clstack_end+1));
   vg_assert(VG_IS_PAGE_ALIGNED(clstack_size));

   ML_(sema_init)(&run_sema);

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

   VG_(threads)[tid_main].client_stack_highest_word 
      = clstack_end + 1 - sizeof(UWord);
   VG_(threads)[tid_main].client_stack_szB 
      = clstack_size;

   VG_(atfork_child)(sched_fork_cleanup);
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
   if (!found) {
      /* Not found; we need to request a translation. */
      if (VG_(translate)( tid, ip, /*debug*/False, 0/*not verbose*/, bbs_done )) {
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

static void handle_syscall(ThreadId tid)
{
   ThreadState *tst = VG_(get_ThreadState)(tid);
   Bool jumped; 

   /* Syscall may or may not block; either way, it will be
      complete by the time this call returns, and we'll be
      runnable again.  We could take a signal while the
      syscall runs. */

   if (VG_(clo_sanity_level >= 3))
      VG_(am_do_sync_check)("(BEFORE SYSCALL)",__FILE__,__LINE__);

   SCHEDSETJMP(tid, jumped, VG_(client_syscall)(tid));

   if (VG_(clo_sanity_level >= 3))
      VG_(am_do_sync_check)("(AFTER SYSCALL)",__FILE__,__LINE__);

   if (!VG_(is_running_thread)(tid))
      VG_(printf)("tid %d not running; VG_(running_tid)=%d, tid %d status %d\n",
		  tid, VG_(running_tid), tid, tst->status);
   vg_assert(VG_(is_running_thread)(tid));
   
   if (jumped) {
      block_signals(tid);
      VG_(poll_signals)(tid);
   }
}

/* 
   Run a thread until it wants to exit.
   
   We assume that the caller has already called VG_(set_running) for
   us, so we own the VCPU.  Also, all signals are blocked.
 */
VgSchedReturnCode VG_(scheduler) ( ThreadId tid )
{
   UInt     trc;
   ThreadState *tst = VG_(get_ThreadState)(tid);

   if (VG_(clo_trace_sched))
      print_sched_event(tid, "entering VG_(scheduler)");      

   VGP_PUSHCC(VgpSched);

   /* set the proper running signal mask */
   block_signals(tid);
   
   vg_assert(VG_(is_running_thread)(tid));

   VG_(dispatch_ctr) = SCHEDULING_QUANTUM + 1;

   while(!VG_(is_exiting)(tid)) {
      if (VG_(dispatch_ctr) == 1) {
	 /* Our slice is done, so yield the CPU to another thread.  This
	    doesn't sleep between sleeping and running, since that would
	    take too much time.  */
	 VG_(set_sleeping)(tid, VgTs_Yielding);
	 /* nothing */
	 VG_(set_running)(tid);

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
	 VG_(message)(Vg_DebugMsg, "thread %d: running for %d bbs", 
		      tid, VG_(dispatch_ctr) - 1 );

      trc = run_thread_for_a_while ( tid );

      if (VG_(clo_trace_sched) && VG_(clo_verbosity) > 2) {
	 Char buf[50];
	 VG_(sprintf)(buf, "TRC: %s", name_of_sched_event(trc));
	 print_sched_event(tid, buf);
      }

      switch(trc) {
      case VG_TRC_INNER_FASTMISS:
	 vg_assert(VG_(dispatch_ctr) > 1);
	 handle_tt_miss(tid);
	 break;
	    
      case VEX_TRC_JMP_CLIENTREQ:
	 do_client_request(tid);
	 break;
	    
      case VEX_TRC_JMP_SYSCALL:
	 handle_syscall(tid);
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
	 if (VG_(dispatch_ctr) > 100) 
            VG_(dispatch_ctr) = 100;
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
         if (show && VG_(clo_show_emwarns)) {
            VG_(message)( Vg_UserMsg,
                          "Emulation warning: unsupported action:");
            VG_(message)( Vg_UserMsg, "  %s", what);
            VG_(get_and_pp_StackTrace)( tid, VG_(clo_backtrace_size) );
         }
         break;
      }

      case VEX_TRC_JMP_NODECODE:
#define M(a) VG_(message)(Vg_UserMsg, a);
   M("Your program just tried to execute an instruction that Valgrind" );
   M("did not recognise.  There are two possible reasons for this."    );
   M("1. Your program has a bug and erroneously jumped to a non-code"  );
   M("   location.  If you are running Memcheck and you just saw a"    );
   M("   warning about a bad jump, it's probably your program's fault.");
   M("2. The instruction is legitimate but Valgrind doesn't handle it,");
   M("   i.e. it's Valgrind's fault.  If you think this is the case or");
   M("   you are not sure, please let us know."                        );
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
         vg_assert2(0, "VG_(scheduler), phase 3: "
                       "run_innerloop detected host "
                       "state invariant failure", trc);

      case VEX_TRC_JMP_SYSENTER_X86:
         /* Do whatever simulation is appropriate for an x86 sysenter
            instruction.  Note that it is critical to set this thread's
            guest_EIP to point at the code to execute after the
            sysenter, since Vex-generated code will not have set it --
            vex does not know what it should be.  Vex sets the next
            address to zero, so if you don't guest_EIP, the thread will
            jump to zero afterwards and probably die as a result. */
#        if defined(VGA_x86)
         //FIXME: VG_(threads)[tid].arch.vex.guest_EIP = ....
         //handle_sysenter_x86(tid);
         vg_assert2(0, "VG_(scheduler), phase 3: "
                       "sysenter_x86 on not yet implemented");
#        else
         vg_assert2(0, "VG_(scheduler), phase 3: "
                       "sysenter_x86 on non-x86 platform?!?!");
#        endif

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

   VGP_POPCC(VgpSched);

   //if (VG_(clo_model_pthreads))
   //   VG_(tm_thread_exit)(tid);
   
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
      VG_(kill_thread)(tid);
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
#elif defined(VGA_ppc32)
#  define VG_CLREQ_ARGS       guest_GPR4
#  define VG_CLREQ_RET        guest_GPR3
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
		      "__libc_freeres() done; really quitting!");
      VG_(threads)[tid].exitreason = VgSrc_ExitSyscall;
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
	    VG_(message)(Vg_DebugMsg, "VG_USERREQ__CLIENT_CALL0: func=%p", f);
	 else
	    SET_CLCALL_RETVAL(tid, f ( tid ), (Addr)f);
         break;
      }
      case VG_USERREQ__CLIENT_CALL1: {
         UWord (*f)(ThreadId, UWord) = (void*)arg[1];
	 if (f == NULL)
	    VG_(message)(Vg_DebugMsg, "VG_USERREQ__CLIENT_CALL1: func=%p", f);
	 else
	    SET_CLCALL_RETVAL(tid, f ( tid, arg[2] ), (Addr)f );
         break;
      }
      case VG_USERREQ__CLIENT_CALL2: {
         UWord (*f)(ThreadId, UWord, UWord) = (void*)arg[1];
	 if (f == NULL)
	    VG_(message)(Vg_DebugMsg, "VG_USERREQ__CLIENT_CALL2: func=%p", f);
	 else
	    SET_CLCALL_RETVAL(tid, f ( tid, arg[2], arg[3] ), (Addr)f );
         break;
      }
      case VG_USERREQ__CLIENT_CALL3: {
         UWord (*f)(ThreadId, UWord, UWord, UWord) = (void*)arg[1];
	 if (f == NULL)
	    VG_(message)(Vg_DebugMsg, "VG_USERREQ__CLIENT_CALL3: func=%p", f);
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
         int count = 
            VG_(vmessage)( Vg_ClientMsg, (char *)arg[1], (void*)arg[2] );
            SET_CLREQ_RETVAL( tid, count );
         break; }

      case VG_USERREQ__INTERNAL_PRINTF: {
         int count = 
            VG_(vmessage)( Vg_DebugMsg, (char *)arg[1], (void*)arg[2] );
            SET_CLREQ_RETVAL( tid, count );
         break; }

      case VG_USERREQ__PRINTF_BACKTRACE: {
         int count =
            VG_(vmessage)( Vg_ClientMsg, (char *)arg[1], (void*)arg[2] );
            VG_(get_and_pp_StackTrace)( tid, VG_(clo_backtrace_size) );
            SET_CLREQ_RETVAL( tid, count );
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

	 info->arena_payload_szB       = VG_(arena_payload_szB);
	 info->mallinfo                = VG_(mallinfo);
	 info->clo_trace_malloc        = VG_(clo_trace_malloc);

         SET_CLREQ_RETVAL( tid, 0 );     /* return value is meaningless */

	 break;
      }

      /* Requests from the client program */

      case VG_USERREQ__DISCARD_TRANSLATIONS:
         if (VG_(clo_verbosity) > 2)
            VG_(printf)( "client request: DISCARD_TRANSLATIONS,"
                         " addr %p,  len %d\n",
                         (void*)arg[1], arg[2] );

         VG_(discard_translations)( 
            arg[1], arg[2], "scheduler(VG_USERREQ__DISCARD_TRANSLATIONS)" 
         );

         SET_CLREQ_RETVAL( tid, 0 );     /* return value is meaningless */
	 break;

      case VG_USERREQ__COUNT_ERRORS:  
         SET_CLREQ_RETVAL( tid, VG_(get_n_errs_found)() );
         break;

      default:
	 if (os_client_request(tid, arg)) {
	    // do nothing, os_client_request() handled it
         } else if (VG_(needs).client_requests) {
	    UWord ret;

            if (VG_(clo_verbosity) > 2)
               VG_(printf)("client request: code %x,  addr %p,  len %d\n",
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
                   "  unhandled client request: 0x%x (%c%c+0x%x).  Perhaps\n" 
		   "  VG_(needs).client_requests should be set?",
			    arg[0], c1, c2, arg[0] & 0xffff);
	       whined = True;
	    }
         }
         break;
   }
}


/* ---------------------------------------------------------------------
   Sanity checking (permanently engaged)
   ------------------------------------------------------------------ */

/* Internal consistency checks on the sched structures. */
static
void scheduler_sanity ( ThreadId tid )
{
   Bool bad = False;

   if (!VG_(is_running_thread)(tid)) {
      VG_(message)(Vg_DebugMsg,
		   "Thread %d is supposed to be running, but doesn't own run_sema (owned by %d)\n", 
		   tid, VG_(running_tid));
      bad = True;
   }

   if (VG_(gettid)() != VG_(threads)[tid].os_state.lwpid) {
      VG_(message)(Vg_DebugMsg,
                   "Thread %d supposed to be in LWP %d, but we're actually %d\n",
                   tid, VG_(threads)[tid].os_state.lwpid, VG_(gettid)());
      bad = True;
   }
}

void VG_(sanity_check_general) ( Bool force_expensive )
{
   ThreadId tid;

   VGP_PUSHCC(VgpCoreCheapSanity);

   if (VG_(clo_sanity_level) < 1) return;

   /* --- First do all the tests that we can do quickly. ---*/

   sanity_fast_count++;

   /* Check stuff pertaining to the memory check system. */

   /* Check that nobody has spuriously claimed that the first or
      last 16 pages of memory have become accessible [...] */
   if (VG_(needs).sanity_checks) {
      VGP_PUSHCC(VgpToolCheapSanity);
      vg_assert(VG_TDICT_CALL(tool_cheap_sanity_check));
      VGP_POPCC(VgpToolCheapSanity);
   }

   /* --- Now some more expensive checks. ---*/

   /* Once every 25 times, check some more expensive stuff. */
   if ( force_expensive
     || VG_(clo_sanity_level) > 1
     || (VG_(clo_sanity_level) == 1 && (sanity_fast_count % 25) == 0)) {

      VGP_PUSHCC(VgpCoreExpensiveSanity);
      sanity_slow_count++;

      if (VG_(needs).sanity_checks) {
          VGP_PUSHCC(VgpToolExpensiveSanity);
          vg_assert(VG_TDICT_CALL(tool_expensive_sanity_check));
          VGP_POPCC(VgpToolExpensiveSanity);
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
	 remains 
            = VG_(am_get_VgStack_unused_szB)(stack);
	 if (remains < VKI_PAGE_SIZE)
	    VG_(message)(Vg_DebugMsg, 
                         "WARNING: Thread %d is within %d bytes "
                         "of running out of stack!",
		         tid, remains);
      }

      VGP_POPCC(VgpCoreExpensiveSanity);
   }

   if (VG_(clo_sanity_level) > 1) {
      VGP_PUSHCC(VgpCoreExpensiveSanity);
      /* Check sanity of the low-level memory manager.  Note that bugs
         in the client's code can cause this to fail, so we don't do
         this check unless specially asked for.  And because it's
         potentially very expensive. */
      VG_(sanity_check_malloc_all)();
      VGP_POPCC(VgpCoreExpensiveSanity);
   }
   VGP_POPCC(VgpCoreCheapSanity);
}

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
