
/*--------------------------------------------------------------------*/
/*--- Thread scheduling.                            vg_scheduler.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, an extensible x86 protected-mode
   emulator for monitoring program execution on x86-Unixes.

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
   implemented elsewhere.
 */

#include "valgrind.h" /* for VG_USERREQ__RUNNING_ON_VALGRIND and
                             VG_USERREQ__DISCARD_TRANSLATIONS, and others */
#include "core.h"

#include "pub_core_errormgr.h"
#include "pub_core_stacktrace.h"
#include "pub_core_syscalls.h"

/* ---------------------------------------------------------------------
   Types and globals for the scheduler.
   ------------------------------------------------------------------ */

/* ThreadId and ThreadState are defined in core.h. */

/* Defines the thread-scheduling timeslice, in terms of the number of
   basic blocks we attempt to run each thread for.  Smaller values
   give finer interleaving but much increased scheduling overheads. */
#define SCHEDULING_QUANTUM   50000

/* Globals.  A statically allocated array of threads.  NOTE: [0] is
   never used, to simplify the simulation of initialisers for
   LinuxThreads. */
ThreadState VG_(threads)[VG_N_THREADS];

/* If true, a fault is Valgrind-internal (ie, a bug) */
Bool VG_(my_fault) = True;

/* Forwards */
static void do_client_request ( ThreadId tid );
static void scheduler_sanity ( ThreadId tid );
static void mostly_clear_thread_record ( ThreadId tid );
static const HChar *name_of_thread_state ( ThreadStatus );

/* Stats. */
static UInt n_scheduling_events_MINOR = 0;
static UInt n_scheduling_events_MAJOR = 0;


void VG_(print_scheduler_stats)(void)
{
   VG_(message)(Vg_DebugMsg,
      "           %d/%d major/minor sched events.", 
      n_scheduling_events_MAJOR, n_scheduling_events_MINOR);
}

/* CPU semaphore, so that threads can run exclusively */
static vg_sema_t run_sema;
static ThreadId running_tid = VG_INVALID_THREADID;


/* ---------------------------------------------------------------------
   Helper functions for the scheduler.
   ------------------------------------------------------------------ */

__inline__
Bool VG_(is_valid_tid) ( ThreadId tid )
{
   /* tid is unsigned, hence no < 0 test. */
   if (tid == 0) return False;
   if (tid >= VG_N_THREADS) return False;
   if (VG_(threads)[tid].status == VgTs_Empty) return False;
   return True;
}


__inline__
static Bool is_valid_or_empty_tid ( ThreadId tid )
{
   /* tid is unsigned, hence no < 0 test. */
   if (tid == 0) return False;
   if (tid >= VG_N_THREADS) return False;
   return True;
}


/* For constructing error messages only: try and identify a thread
   whose stack satisfies the predicate p, or return VG_INVALID_THREADID
   if none do.
*/
ThreadId VG_(first_matching_thread_stack)
              ( Bool (*p) ( Addr stack_min, Addr stack_max, void* d ),
                void* d )
{
   ThreadId tid;

   for (tid = 1; tid < VG_N_THREADS; tid++) {
      if (VG_(threads)[tid].status == VgTs_Empty) continue;

      if ( p ( STACK_PTR(VG_(threads)[tid].arch),
               VG_(threads)[tid].client_stack_highest_word, d ) )
         return tid;
   }
   return VG_INVALID_THREADID;
}
 
void VG_(mark_from_registers)(void (*mark_addr)(Addr))
{
   ThreadId tid;

   for(tid = 1; tid < VG_N_THREADS; tid++) {
      if (!VG_(is_valid_tid)(tid))
	 continue;
      VGA_(mark_from_registers)(tid, mark_addr);
   }
}

/* Print the scheduler status. */
void VG_(pp_sched_status) ( void )
{
   Int i; 
   VG_(printf)("\nsched status:\n"); 
   VG_(printf)("  running_tid=%d\n", running_tid);
   for (i = 1; i < VG_N_THREADS; i++) {
      if (VG_(threads)[i].status == VgTs_Empty) continue;
      VG_(printf)("\nThread %d: status = %s\n", i, name_of_thread_state(VG_(threads)[i].status));
      VG_(get_and_pp_StackTrace)( i, VG_(clo_backtrace_size) );
   }
   VG_(printf)("\n");
}

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

static
const HChar* name_of_thread_state ( ThreadStatus state )
{
   switch (state) {
   case VgTs_Empty:     return "VgTs_Empty";
   case VgTs_Init:      return "VgTs_Init";
   case VgTs_Runnable:  return "VgTs_Runnable";
   case VgTs_WaitSys:   return "VgTs_WaitSys";
   case VgTs_Yielding:  return "VgTs_Yielding";
   case VgTs_Zombie:    return "VgTs_Zombie";
   default:             return "VgTs_???";
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

ThreadState *VG_(get_ThreadState)(ThreadId tid)
{
   vg_assert(tid >= 0 && tid < VG_N_THREADS);
   return &VG_(threads)[tid];
}

/* Given an LWP id (ie, real kernel thread id), find the corresponding
   ThreadId */
ThreadId VG_(get_lwp_tid)(Int lwp)
{
   ThreadId tid;
   
   for(tid = 1; tid <= VG_N_THREADS; tid++)
      if (VG_(threads)[tid].status != VgTs_Empty && VG_(threads)[tid].os_state.lwpid == lwp)
	 return tid;

   return VG_INVALID_THREADID;
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
   
   VGO_(sema_down)(&run_sema);
   if (running_tid != VG_INVALID_THREADID)
      VG_(printf)("tid %d found %d running\n", tid, running_tid);
   vg_assert(running_tid == VG_INVALID_THREADID);
   running_tid = tid;

   if (VG_(clo_trace_sched))
      print_sched_event(tid, "now running");
}

ThreadId VG_(get_running_tid)(void)
{
   return running_tid;
}

Bool VG_(is_running_thread)(ThreadId tid)
{
   ThreadState *tst = VG_(get_ThreadState)(tid);

   return 
//      tst->os_state.lwpid == VG_(gettid)() &&	/* check we're this tid */
      running_tid == tid	           &&	/* and that we've got the lock */
      tst->status == VgTs_Runnable;		/* and we're runnable */
}

/* Return the number of non-dead Threads */
Int VG_(count_living_threads)(void)
{
   Int count = 0;
   ThreadId tid;

   for(tid = 1; tid < VG_N_THREADS; tid++)
      if (VG_(threads)[tid].status != VgTs_Empty &&
	  VG_(threads)[tid].status != VgTs_Zombie)
	 count++;

   return count;
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

   vg_assert(running_tid == tid);
   running_tid = VG_INVALID_THREADID;

   /* Release the run_sema; this will reschedule any runnable
      thread. */
   VGO_(sema_up)(&run_sema);

   if (VG_(clo_trace_sched)) {
      Char buf[50];
      VG_(sprintf)(buf, "now sleeping in state %s", name_of_thread_state(sleepstate));
      print_sched_event(tid, buf);
   }
}

/* Return true if the thread is still alive but in the process of
   exiting. */
inline Bool VG_(is_exiting)(ThreadId tid)
{
   vg_assert(VG_(is_valid_tid)(tid));
   return VG_(threads)[tid].exitreason != VgSrc_None;
}

/* Clear out the ThreadState and release the semaphore. Leaves the
   ThreadState in VgTs_Zombie state, so that it doesn't get
   reallocated until the caller is really ready. */
void VG_(exit_thread)(ThreadId tid)
{
   vg_assert(VG_(is_valid_tid)(tid));
   vg_assert(VG_(is_running_thread)(tid));
   vg_assert(VG_(is_exiting)(tid));

   VGA_(cleanup_thread)( &VG_(threads)[tid].arch );

   mostly_clear_thread_record(tid);
   running_tid = VG_INVALID_THREADID;

   /* There should still be a valid exitreason for this thread */
   vg_assert(VG_(threads)[tid].exitreason != VgSrc_None);

   VGO_(sema_up)(&run_sema);
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
      VG_(tkill)(VG_(threads)[tid].os_state.lwpid, VKI_SIGVGKILL);
   }
}

/* 
   Yield the CPU for a short time to let some other thread run.
 */
void VG_(vg_yield)(void)
{
   struct vki_timespec ts = { 0, 1 };
   ThreadId tid = running_tid;

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

   VG_(poll_signals)(tid);	/* something might have happened */
}


void VG_(resume_scheduler)(ThreadId tid)
{
   ThreadState *tst = VG_(get_ThreadState)(tid);

   vg_assert(tst->os_state.lwpid == VG_(gettid)());

   if (tst->sched_jmpbuf_valid) {
      /* Can't continue; must longjmp back to the scheduler and thus
         enter the sighandler immediately. */
   
      VGP_LONGJMP(tst->sched_jmpbuf, True);
   }
}

#define SCHEDSETJMP(tid, jumped, stmt)					\
   do {									\
      ThreadState * volatile _qq_tst = VG_(get_ThreadState)(tid);	\
									\
      (jumped) = VGP_SETJMP(_qq_tst->sched_jmpbuf);                     \
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
      VG_(block_signals)(tid);
   } 

   done_this_time = (Int)dispatch_ctr_SAVED - (Int)VG_(dispatch_ctr) - 0;

   vg_assert(done_this_time >= 0);
   VG_(bbs_done) += (ULong)done_this_time;

   VGP_POPCC(VgpRun);
   return trc;
}


static 
void mostly_clear_thread_record ( ThreadId tid )
{
   vki_sigset_t savedmask;

   vg_assert(tid >= 0 && tid < VG_N_THREADS);
   VGA_(cleanup_thread)(&VG_(threads)[tid].arch);
   VG_(threads)[tid].tid = tid;

   /* Leave the thread in Zombie, so that it doesn't get reallocated
      until the caller is finally done with the thread stack. */
   VG_(threads)[tid].status               = VgTs_Zombie;

   VG_(threads)[tid].syscallno = -1;

   VG_(sigemptyset)(&VG_(threads)[tid].sig_mask);
   VG_(sigemptyset)(&VG_(threads)[tid].tmp_sig_mask);

   VGA_(os_state_clear)(&VG_(threads)[tid]);

   /* start with no altstack */
   VG_(threads)[tid].altstack.ss_sp = (void *)0xdeadbeef;
   VG_(threads)[tid].altstack.ss_size = 0;
   VG_(threads)[tid].altstack.ss_flags = VKI_SS_DISABLE;

   /* clear out queued signals */
   VG_(block_all_host_signals)(&savedmask);
   if (VG_(threads)[tid].sig_queue != NULL) {
      VG_(arena_free)(VG_AR_CORE, VG_(threads)[tid].sig_queue);
      VG_(threads)[tid].sig_queue = NULL;
   }
   VG_(restore_all_host_signals)(&savedmask);

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
   vg_assert(running_tid == me);

   VG_(master_tid) = me;

   VG_(threads)[me].os_state.lwpid = VG_(gettid)();
   VG_(threads)[me].os_state.threadgroup = VG_(getpid)();

   /* clear out all the unused thread slots */
   for (tid = 1; tid < VG_N_THREADS; tid++) {
      if (tid != me) {
         mostly_clear_thread_record(tid);
	 VG_(threads)[tid].status = VgTs_Empty;
      }
   }

   /* re-init and take the sema */
   VGO_(sema_deinit)(&run_sema);
   VGO_(sema_init)(&run_sema);
   VGO_(sema_down)(&run_sema);
}


/* Initialise the scheduler.  Create a single "main" thread ready to
   run, with special ThreadId of one.  This is called at startup.  The
   caller subsequently initialises the guest state components of this
   main thread, thread 1.  
*/
void VG_(scheduler_init) ( void )
{
   Int i;
   ThreadId tid_main;

   VGO_(sema_init)(&run_sema);

   for (i = 0 /* NB; not 1 */; i < VG_N_THREADS; i++) {
      VG_(threads)[i].sig_queue            = NULL;

      VGA_(os_state_init)(&VG_(threads)[i]);
      mostly_clear_thread_record(i);

      VG_(threads)[i].status                    = VgTs_Empty;
      VG_(threads)[i].client_stack_szB          = 0;
      VG_(threads)[i].client_stack_highest_word = (Addr)NULL;
   }

   tid_main = VG_(alloc_ThreadState)();

   VG_(master_tid) = tid_main;

   /* Initial thread's stack is the original process stack */
   VG_(threads)[tid_main].client_stack_highest_word 
                                            = VG_(clstk_end) - sizeof(UWord);
   VG_(threads)[tid_main].client_stack_szB  = VG_(client_rlimit_stack).rlim_cur;

   VG_(atfork)(NULL, NULL, sched_fork_cleanup);
}


/* ---------------------------------------------------------------------
   The scheduler proper.
   ------------------------------------------------------------------ */

static void handle_tt_miss ( ThreadId tid )
{
   Bool found;
   Addr ip = INSTR_PTR(VG_(threads)[tid].arch);

   /* Trivial event.  Miss in the fast-cache.  Do a full
      lookup for it. */
   found = VG_(search_transtab)( NULL,
                                 ip, True/*upd_fast_cache*/ );
   if (!found) {
      /* Not found; we need to request a translation. */
      if (VG_(translate)( tid, ip, /*debug*/False, 0/*not verbose*/ )) {
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
   SCHEDSETJMP(tid, jumped, VG_(client_syscall)(tid));

   if (!VG_(is_running_thread)(tid))
      VG_(printf)("tid %d not running; running_tid=%d, tid %d status %d\n",
		  tid, running_tid, tid, tst->status);
   vg_assert(VG_(is_running_thread)(tid));
   
   if (jumped) {
      VG_(block_signals)(tid);
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
   VG_(block_signals)(tid);
   
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
	 //VG_(tm_thread_switchto)(tid);

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
         VG_(synth_sigill)(tid, INSTR_PTR(VG_(threads)[tid].arch));
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

   VGP_POPCC(VgpSched);

   //if (VG_(clo_model_pthreads))
   //   VG_(tm_thread_exit)(tid);
   
   return tst->exitreason;
}


/* 
   This causes all threads to forceably exit.  They aren't actually
   dead by the time this returns; you need to call
   VGA_(reap_threads)() to wait for them.
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
      VG_(kill_thread)(tid);
   }
}


/* ---------------------------------------------------------------------
   Specifying shadow register values
   ------------------------------------------------------------------ */

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

void VG_(set_shadow_regs_area) ( ThreadId tid, OffT offset, SizeT size,
                                 const UChar* area )
{
   ThreadState* tst;

   vg_assert(VG_(is_valid_tid)(tid));
   tst = & VG_(threads)[tid];

   // Bounds check
   vg_assert(0 <= offset && offset < sizeof(VexGuestArchState));
   vg_assert(offset + size <= sizeof(VexGuestArchState));

   VG_(memcpy)( (void*)(((Addr)(&tst->arch.vex_shadow)) + offset), area, size);
}

void VG_(get_shadow_regs_area) ( ThreadId tid, OffT offset, SizeT size,
                                 UChar* area )
{
   ThreadState* tst;

   vg_assert(VG_(is_valid_tid)(tid));
   tst = & VG_(threads)[tid];

   // Bounds check
   vg_assert(0 <= offset && offset < sizeof(VexGuestArchState));
   vg_assert(offset + size <= sizeof(VexGuestArchState));

   VG_(memcpy)( area, (void*)(((Addr)&(tst->arch.vex_shadow)) + offset), size);
}

/* ---------------------------------------------------------------------
   Handle client requests.
   ------------------------------------------------------------------ */

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

      case VG_USERREQ__GET_MALLOCFUNCS: {
	 struct vg_mallocfunc_info *info = (struct vg_mallocfunc_info *)arg[1];

	 info->tl_malloc               = VG_(tdict).malloc_malloc;
	 info->tl_calloc               = VG_(tdict).malloc_calloc;
	 info->tl_realloc              = VG_(tdict).malloc_realloc;
	 info->tl_memalign             = VG_(tdict).malloc_memalign;
	 info->tl___builtin_new        = VG_(tdict).malloc___builtin_new;
	 info->tl___builtin_vec_new    = VG_(tdict).malloc___builtin_vec_new;
	 info->tl_free                 = VG_(tdict).malloc_free;
	 info->tl___builtin_delete     = VG_(tdict).malloc___builtin_delete;
	 info->tl___builtin_vec_delete = VG_(tdict).malloc___builtin_vec_delete;

	 info->arena_payload_szB       = VG_(arena_payload_szB);
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

         VG_(discard_translations)( arg[1], arg[2] );

         SET_CLREQ_RETVAL( tid, 0 );     /* return value is meaningless */
	 break;

      case VG_USERREQ__COUNT_ERRORS:  
         SET_CLREQ_RETVAL( tid, VG_(get_n_errs_found)() );
         break;

      /* Obsolete requests: print a warning in case there's an old
	 libpthread.so still hanging around. */
      case VG_USERREQ__APPLY_IN_NEW_THREAD:
      case VG_USERREQ__QUIT:
      case VG_USERREQ__WAIT_JOINER:
      case VG_USERREQ__PTHREAD_JOIN:
      case VG_USERREQ__SET_CANCELSTATE:
      case VG_USERREQ__SET_CANCELTYPE:
      case VG_USERREQ__TESTCANCEL:
      case VG_USERREQ__SET_CANCELPEND:
      case VG_USERREQ__SET_OR_GET_DETACH:
      case VG_USERREQ__PTHREAD_GET_THREADID:
      case VG_USERREQ__PTHREAD_MUTEX_LOCK:
      case VG_USERREQ__PTHREAD_MUTEX_TIMEDLOCK:
      case VG_USERREQ__PTHREAD_MUTEX_TRYLOCK:
      case VG_USERREQ__PTHREAD_MUTEX_UNLOCK:
      case VG_USERREQ__PTHREAD_COND_WAIT:
      case VG_USERREQ__PTHREAD_COND_TIMEDWAIT:
      case VG_USERREQ__PTHREAD_COND_SIGNAL:
      case VG_USERREQ__PTHREAD_COND_BROADCAST:
      case VG_USERREQ__PTHREAD_KEY_CREATE:
      case VG_USERREQ__PTHREAD_KEY_DELETE:
      case VG_USERREQ__PTHREAD_SETSPECIFIC_PTR:
      case VG_USERREQ__PTHREAD_GETSPECIFIC_PTR:
      case VG_USERREQ__READ_MILLISECOND_TIMER:
      case VG_USERREQ__PTHREAD_SIGMASK:
      case VG_USERREQ__SIGWAIT:
      case VG_USERREQ__PTHREAD_KILL:
      case VG_USERREQ__PTHREAD_YIELD:
      case VG_USERREQ__PTHREAD_KEY_VALIDATE:
      case VG_USERREQ__CLEANUP_PUSH:
      case VG_USERREQ__CLEANUP_POP:
      case VG_USERREQ__GET_KEY_D_AND_S:
      case VG_USERREQ__NUKE_OTHER_THREADS:
      case VG_USERREQ__GET_N_SIGS_RETURNED:
      case VG_USERREQ__SET_FHSTACK_USED:
      case VG_USERREQ__GET_FHSTACK_USED:
      case VG_USERREQ__SET_FHSTACK_ENTRY:
      case VG_USERREQ__GET_FHSTACK_ENTRY:
      case VG_USERREQ__GET_SIGRT_MIN:
      case VG_USERREQ__GET_SIGRT_MAX:
      case VG_USERREQ__ALLOC_RTSIG:
      case VG_USERREQ__MALLOC:
      case VG_USERREQ__FREE:
	 VG_(message)(Vg_UserMsg, "It looks like you've got an old libpthread.so* ");
	 VG_(message)(Vg_UserMsg, "installed in \"%s\".", VG_(libdir));
	 VG_(message)(Vg_UserMsg, "Please delete it and try again.");
	 VG_(exit)(99);
         break;

      default:
	 if (VGA_(client_request)(tid, arg)) {
	    /* architecture handled the client request */
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
		   "  VG_(needs).client_requests should be set?\n",
			    arg[0], c1, c2, arg[0] & 0xffff);
	       whined = True;
	    }
         }
         break;
   }
}


/* ---------------------------------------------------------------------
   Sanity checking.
   ------------------------------------------------------------------ */

/* Internal consistency checks on the sched structures. */
static
void scheduler_sanity ( ThreadId tid )
{
   Bool bad = False;

   if (!VG_(is_running_thread)(tid)) {
      VG_(message)(Vg_DebugMsg,
		   "Thread %d is supposed to be running, but doesn't own run_sema (owned by %d)\n", 
		   tid, running_tid);
      bad = True;
   }

   if (VG_(gettid)() != VG_(threads)[tid].os_state.lwpid) {
      VG_(message)(Vg_DebugMsg,
                   "Thread %d supposed to be in LWP %d, but we're actually %d\n",
                   tid, VG_(threads)[tid].os_state.lwpid, VG_(gettid)());
      bad = True;
   }
}


/*--------------------------------------------------------------------*/
/*--- end                                           vg_scheduler.c ---*/
/*--------------------------------------------------------------------*/
