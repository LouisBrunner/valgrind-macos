
/*--------------------------------------------------------------------*/
/*--- Platform-specific syscalls stuff.       syswrap-ppc64-aix5.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2006-2010 OpenWorks LLP
      info@open-works.co.uk

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

   Neither the names of the U.S. Department of Energy nor the
   University of California nor the names of its contributors may be
   used to endorse or promote products derived from this software
   without prior written permission.
*/

#if defined(VGP_ppc64_aix5)

#include "pub_core_basics.h"
#include "pub_core_vki.h"
#include "pub_core_vkiscnums.h"
#include "pub_core_threadstate.h"
#include "pub_core_debuglog.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcprint.h"
#include "pub_core_libcproc.h"
#include "pub_core_options.h"
#include "pub_core_scheduler.h"
#include "pub_core_sigframe.h"      // For VG_(sigframe_destroy)()
#include "pub_core_signals.h"
#include "pub_core_syscall.h"
#include "pub_core_syswrap.h"
#include "pub_core_tooliface.h"

#include "priv_types_n_macros.h"
#include "priv_syswrap-aix5.h"      /* for decls of aix5-common wrappers */
#include "priv_syswrap-main.h"


/* --------- HACKS --------- */
/* XXXXXXXXXXXX these HACKS are copies of stuff in syswrap-linux.c;
   check for duplication. */
/* HACK: is in syswrap-generic.c, but that doesn't get build on AIX. */
/* Dump out a summary, and a more detailed list, of open file descriptors. */
void VG_(show_open_fds) ( void )
{
  I_die_here;
}
static Bool i_am_the_only_thread ( void )
{
   Int c = VG_(count_living_threads)();
   vg_assert(c >= 1); /* stay sane */
   return c == 1;
}
void VG_(reap_threads)(ThreadId self)
{
   while (!i_am_the_only_thread()) {
      /* Let other thread(s) run */
      VG_(vg_yield)();
      VG_(poll_signals)(self);
   }
   vg_assert(i_am_the_only_thread());
}
void VG_(init_preopened_fds) ( void )
{
   I_die_here;
}


// Run a thread from beginning to end and return the thread's
// scheduler-return-code.
static VgSchedReturnCode thread_wrapper(Word /*ThreadId*/ tidW)
{
   VgSchedReturnCode ret;
   ThreadId     tid = (ThreadId)tidW;
   ThreadState* tst = VG_(get_ThreadState)(tid);

   VG_(debugLog)(1, "syswrap-aix64",
                    "thread_wrapper(tid=%lld): entry\n",
                    (ULong)tidW);

   vg_assert(tst->status == VgTs_Init);

   /* make sure we get the CPU lock before doing anything significant */
   VG_(acquire_BigLock)(tid, "thread_wrapper(starting new thread)");

   if (0)
      VG_(printf)("thread tid %d started: stack = %p\n",
                  tid, &tid);

   VG_TRACK( pre_thread_first_insn, tid );

   tst->os_state.lwpid = VG_(gettid)();
   tst->os_state.threadgroup = VG_(getpid)();

   /* Thread created with all signals blocked; scheduler will set the
      appropriate mask */
   ret = VG_(scheduler)(tid);

   vg_assert(VG_(is_exiting)(tid));

   vg_assert(tst->status == VgTs_Runnable);
   vg_assert(VG_(is_running_thread)(tid));

   VG_(debugLog)(1, "syswrap-aix64",
                    "thread_wrapper(tid=%lld): exit\n",
                    (ULong)tidW);

   /* Return to caller, still holding the lock. */
   return ret;
}


/* Run a thread all the way to the end, then do appropriate exit actions
   (this is the last-one-out-turn-off-the-lights bit).  */
static void run_a_thread_NORETURN ( Word tidW )
{
   ThreadId          tid = (ThreadId)tidW;
   VgSchedReturnCode src;
   Int               c;

   VG_(debugLog)(1, "syswrap-aix64",
                    "run_a_thread_NORETURN(tid=%lld): pre-thread_wrapper\n",
                    (ULong)tidW);

   /* Run the thread all the way through. */
   src = thread_wrapper(tid);

   VG_(debugLog)(1, "syswrap-aix64",
                    "run_a_thread_NORETURN(tid=%lld): post-thread_wrapper\n",
                    (ULong)tidW);

   c = VG_(count_living_threads)();
   vg_assert(c >= 1); /* stay sane */

   vg_assert(src == VgSrc_ExitThread 
             || src == VgSrc_ExitProcess
             || src == VgSrc_FatalSig);

   if (c == 1 || src == VgSrc_ExitProcess) {

      VG_(debugLog)(1, "syswrap-aix64",
                       "run_a_thread_NORETURN(tid=%lld): "
                          "exit process (%d threads remaining)\n",
                          (ULong)tidW, c);

      /* We are the last one standing.  Keep hold of the lock and
         carry on to show final tool results, then exit the entire system.
         Use the continuation pointer set at startup in m_main. */
      ( * VG_(address_of_m_main_shutdown_actions_NORETURN) ) (tid, src);

   } else {

      ThreadState *tst;

      VG_(debugLog)(1, "syswrap-aix64",
                       "run_a_thread_NORETURN(tid=%lld): "
                          "not last one standing\n",
                          (ULong)tidW);

      /* OK, thread is dead, but others still exist.  Just exit. */
      vg_assert(c >= 2);
      tst = VG_(get_ThreadState)(tid);

      /* This releases the run lock */
      VG_(exit_thread)(tid);
      vg_assert(tst->status == VgTs_Zombie);

      /* We have to use this sequence to terminate the thread to
         prevent a subtle race.  If VG_(exit_thread)() had left the
         ThreadState as Empty, then it could have been reallocated,
         reusing the stack while we're doing these last cleanups.
         Instead, VG_(exit_thread) leaves it as Zombie to prevent
         reallocation.  We need to make sure we don't touch the stack
         between marking it Empty and exiting.  Hence the
         assembler. */
      { ULong block[4];
        vg_assert(sizeof(tst->status == 8));
        vg_assert(__NR_AIX5_thread_terminate
                  != __NR_AIX5_UNKNOWN);
        block[0] = (ULong)VgTs_Empty;
        block[1] = (ULong) & (tst->status);
        block[2] = (ULong) tst->os_state.exitcode;
        block[3] = __NR_AIX5_thread_terminate;
        asm volatile (
          "mr 29,%0\n\t"           /* r29 = &block[0] */
          "ld 20, 0(29)\n\t"       /* r20 = VgTs_Empty */
          "ld 21, 8(29)\n\t"       /* r21 = & (tst->status) */
          "ld 22, 16(29)\n\t"      /* r22 = tst->os_state.exitcode */
          "ld 23, 24(29)\n\t"      /* r23 = __NR_exit */
          /* after this point we can't safely use the stack. */
          "std 20, 0(21)\n\t"      /* tst->status = VgTs_Empty */
          "mr 2,23\n\t"            /* r2 = __NR_exit */
          "mr 3,22\n\t"            /* set r3 = tst->os_state.exitcode */
          /* set up for syscall */
          "crorc 6,6,6\n\t"
          ".long 0x48000005\n\t"   /* "bl here+4" */
          "mflr 29\n\t"
          "addi 29,29,16\n\t"
          "mtlr 29\n\t"
          "sc\n\t"                 /* exit(tst->os_state.exitcode) */
          :
          : "b" (&block[0])
          : "lr", "memory", "r2", "r3", "r20", "r21", "r22", "r23", "r29"
        );
      }

      VG_(core_panic)("Thread exit failed?\n");
   }

   /*NOTREACHED*/
   vg_assert(0);
}


static Word start_thread_NORETURN ( void* arg )
{
   ThreadState* tst = (ThreadState*)arg;
   ThreadId     tid = tst->tid;

   run_a_thread_NORETURN ( (Word)tid );
   /*NOTREACHED*/
   vg_assert(0);
}


/* Call f(arg1), but first switch stacks, using 'stack' as the new
   stack.  f itself needs to never return. */
__attribute__((noreturn))
static
void call_on_new_stack_0_1_NORETURN ( Addr stack,
                                      void (*f_NORETURN)(Word),
                                      Word arg1 )
{
   UWord* fdescr = (UWord*)f_NORETURN;
   volatile UWord block[5];
   block[0] = fdescr[0];  /* nia */
   block[1] = stack;      /* r1 */
   block[2] = fdescr[1];  /* r2 */
   block[3] = arg1;       /* r3 */
   block[4] = fdescr[2];  /* r11 */
   __asm__ __volatile__(
      "mr  4,%0\n\t" /* r4 = block */
      "ld 1, 8(4)\n\t"
      "ld 2, 16(4)\n\t"
      "ld 3, 24(4)\n\t"
      "ld 11,32(4)\n\t"
      "ld 4, 0(4)\n\t"
      "mtctr 4\n\t"
      "bctr\n"
      : /*out*/ : /*in*/ "b"(&block[0]) 
   );
   /*NOTREACHED*/
   __asm__ __volatile__("trap");
   while (1) {} /* convince gcc that this really doesn't return */
}


/* Allocate a stack for the main thread, and run it all the way to the
   end.  Although we already have a working VgStack
   (VG_(interim_stack)) it's better to allocate a new one, so that
   overflow detection works uniformly for all threads.
*/
void VG_(main_thread_wrapper_NORETURN)(ThreadId tid)
{
   Addr sp;
   VG_(debugLog)(1, "syswrap-aix64",
                    "entering VG_(main_thread_wrapper_NORETURN)\n");

   sp = ML_(allocstack)(tid);

   /* If we can't even allocate the first thread's stack, we're hosed.
      Give up. */
   vg_assert2(sp != 0, "Cannot allocate main thread's stack.");

   /* shouldn't be any other threads around yet */
   vg_assert( VG_(count_living_threads)() == 1 );

   /* make a stack frame */
   sp -= 16;
   sp &= ~0xF;
   *(UWord *)sp = 0;

   call_on_new_stack_0_1_NORETURN(
      (Addr)sp,               /* stack */
      run_a_thread_NORETURN,  /* fn to call */
      (Word)tid               /* arg to give it */
   );

   /*NOTREACHED*/
   vg_assert(0);
}

/* --------- end HACKS --------- */


/* ---------------------------------------------------------------------
   More thread stuff
   ------------------------------------------------------------------ */

void VG_(cleanup_thread) ( ThreadArchState* arch )
{
}  


/* ---------------------------------------------------------------------
   PRE/POST wrappers for ppc64/AIX5-specific syscalls
   ------------------------------------------------------------------ */

/* --- !!! --- EXTERNAL HEADERS start --- !!! --- */
#include <sys/thread.h>
/* --- !!! --- EXTERNAL HEADERS end --- !!! --- */


/* Add prototypes for the wrappers declared here, so that gcc doesn't
   harass us for not having prototypes.  Really this is a kludge --
   the right thing to do is to make these wrappers 'static' since they
   aren't visible outside this file, but that requires even more macro
   magic. */

#define PRE(name)       DEFN_PRE_TEMPLATE(ppc64_aix5, name)
#define POST(name)      DEFN_POST_TEMPLATE(ppc64_aix5, name)

DECL_TEMPLATE(ppc64_aix5, sys__clock_gettime);
DECL_TEMPLATE(ppc64_aix5, sys__fp_fpscrx64_);
DECL_TEMPLATE(ppc64_aix5, sys_kload);
DECL_TEMPLATE(ppc64_aix5, sys_kunload64);
DECL_TEMPLATE(ppc64_aix5, sys_thread_setstate);
DECL_TEMPLATE(ppc64_aix5, sys_FAKE_SIGRETURN);


PRE(sys__clock_gettime)
{
   /* Seems like ARG2 points at a destination buffer? */
   /* _clock_gettime (UNDOCUMENTED) ( 0, 0xA, 0x2FF21808 ) */
   PRINT("_clock_gettime (UNDOCUMENTED) ( %ld, %#lx, %#lx )", ARG1, ARG2, ARG3 );
   PRE_REG_READ3(int, "_clock_gettime", int, arg1, int, arg2, void*, arg3);
   PRE_MEM_WRITE( "_clock_gettime(dst)", ARG2, sizeof(struct timespec) );
}
POST(sys__clock_gettime)
{
   vg_assert(SUCCESS);
   POST_MEM_WRITE( ARG2, sizeof(struct timespec) );
}

PRE(sys__fp_fpscrx64_)
{
   PRINT("_fp_fpscrx64_ (BOGUS HANDLER)");
}

PRE(sys_kload)
{
   PRINT("kload (UNDOCUMENTED)( %#lx(%s), %ld, %ld )", 
         ARG1,(Char*)ARG1, ARG2, ARG3 );
   PRE_REG_READ3(void*, "kload", char*, name, long, arg2, char*, arg3);
}
POST(sys_kload)
{
   vg_assert(SUCCESS);
   if (0) VG_(printf)("kload result = %#lx\n", RES);
   if (RES)
      POST_MEM_WRITE( RES, 64 );
   ML_(aix5_rescan_procmap_after_load_or_unload)();
}

PRE(sys_kunload64)
{
   PRINT("kunload64 (UNDOCUMENTED)( %#lx, %ld, %ld, %#lx )", 
         ARG1, ARG2, ARG3, ARG4 );
   PRE_REG_READ4(long, "kunload64",
                 void*, arg1, long, arg2, long, arg3, void*, arg4);
}
POST(sys_kunload64)
{
   vg_assert(SUCCESS);
   ML_(aix5_rescan_procmap_after_load_or_unload)();
}

PRE(sys_thread_setstate)
{
   UWord          dst_lwpid = (UWord)ARG1;
   struct tstate* ats_new   = (struct tstate*)ARG2;
   struct tstate* ats_old   = (struct tstate*)ARG3;
   ThreadId       dst_tid   = VG_INVALID_THREADID;
   ThreadState*   dst_ts    = NULL;
   Int i;

   /* Arrgh.  We MUST retain the lock during this syscall.  Reason is
      that this is sometimes used for asynchronous thread cancellation
      (nuking other threads).  If we don't have the lock during the
      syscall, then it's possible that the thread we're nuking might
      get the lock before it gets killed off, and so we can never
      re-acquire the lock after this syscall, and the system
      deadlocks. */

   /* 10 July 06: above comment is a misdiagnosis.  It appears that
      for thread cancellation (that is, with ->flags == TSTATE_INTR)
      the target thread is has its PC changed by the the kernel to
      something else, possibly to pthread_exit(), so that it can run
      its cancellation handlers and exit.  Currently is unknown how
      the kernel knows what to set the target thread's PC to.  I did
      establish that all the other data passed in the struct is not
      relevant: when ->flags == TSTATE_INTR, all the other words can
      be set to 0x0 or 0xFFFFFFFF and the syscall still works.  So the
      address is not passed like that.  Also I looked at args to
      thread_setmystate_fast, which is used when a thread sets its
      cancellation state, but none of those are code addresses.

      Also, it's ok for the kernel to simply change the target
      thread's PC to something else for async thread cancellation, but
      for deferred cancellation something else is needed, and I can't
      see how that would work either.

      Anyway, net result is, target thread ends up not running on the
      simulator (not dead), which is why it's necessary to hold onto
      the lock at this point. */

   /* 30 July 06: added kludge to intercept attempts to cancel another
      thread and instead just force that thread to run
      pthread_exit(PTHREAD_CANCELED).  This allows V to keep
      control. */

   PRINT("thread_setstate (BOGUS HANDLER) "
         "( %ld, %p,%p )", dst_lwpid, ats_new, ats_old);
   if (1 && VG_(clo_trace_syscalls) && ats_new)
      ML_(aix5debugstuff_show_tstate)((Addr)ats_new, 
                                      "thread_setstate (NEW)");

   /* Intercept and handle ourselves any attempts to cancel 
      another thread (including this one). */

   if (ats_new && (!ats_old) && ats_new->flags == TSTATE_INTR) {
      dst_ts = NULL;
      if (VG_(clo_trace_syscalls))
         VG_(printf)("(INTR for lwpid %ld)", dst_lwpid);
      dst_tid = VG_INVALID_THREADID;
      for (i = 0; i < VG_N_THREADS; i++) {
         dst_ts = VG_(get_ThreadState)(i);
         if ((dst_ts->status == VgTs_Runnable 
              || dst_ts->status == VgTs_Yielding
              || dst_ts->status == VgTs_WaitSys)
             && dst_ts->os_state.lwpid == dst_lwpid) {
            dst_tid = i;
            break;
         }
      }
      if (VG_(clo_trace_syscalls)) {
         if (dst_tid == VG_INVALID_THREADID)
            VG_(printf)("(== unknown tid)");
         else 
            VG_(printf)("(== tid %d)", (Int)dst_tid);
      }
      if (dst_tid != VG_INVALID_THREADID) {
         /* A cancel has been requested for ctid.  If the target
            thread has cancellation enabled, honour it right now.  If
            not, mark the thread as having a cancellation request, so
            that if it later enables cancellation then the
            cancellation will take effect. */
         vg_assert(dst_ts);
         if (dst_ts->os_state.cancel_progress == Canc_NoRequest) {
            if (dst_ts->os_state.cancel_disabled) {
               if (VG_(clo_trace_syscalls))
                  VG_(printf)("(target has cancel disabled"
                              "; request lodged)");
               dst_ts->os_state.cancel_progress = Canc_Requested;
            } else {
               if (VG_(clo_trace_syscalls))
                  VG_(printf)("(forcing target into pthread_exit)");
               dst_ts->os_state.cancel_progress = Canc_Actioned;
               Bool ok = ML_(aix5_force_thread_into_pthread_exit)(dst_tid);
               if (!ok) {
                  /* now at serious risk of deadlock/livelock.  Give up
                     rather than continue. */
                  ML_(aix5_set_threadstate_for_emergency_exit)
                     (tid, "pthread_cancel(case2-64): "
                           "cannot find pthread_exit; aborting");
                  SET_STATUS_Success(0);
                  return;
               }
            }
         }
         SET_STATUS_Success(0);
         return;
      }
   }

   /* Well, it's not a cancellation request.  Maybe it is the
      initialisation of a previously created thread? */

   if (ats_new && !ats_old) {
      dst_tid = VG_INVALID_THREADID;
      for (i = 0; i < VG_N_THREADS; i++) {
         dst_ts = VG_(get_ThreadState)(i);
         if (dst_ts->status == VgTs_Init 
             && dst_ts->os_state.lwpid == dst_lwpid) {
            dst_tid = i;
            break;
         }
      }
      if (dst_tid != VG_INVALID_THREADID) {
         /* Found the associated child */
         if (VG_(clo_trace_syscalls)) 
            VG_(printf)("(initialised child tid %d)", (Int)dst_tid);
         dst_ts = VG_(get_ThreadState)(dst_tid);
         UWord* stack = (UWord*)ML_(allocstack)(dst_tid);
         /* XXX TODO: check allocstack failure */

         /* copy the specified child register state into the guest
            slot (we need that context to run on the simulated CPU,
            not the real one) and put pointers to our own
            run-the-simulator function into what we'll hand off to the
            kernel instead. */

         /* The guest thread is to start running whatever context
            this syscall showed up with. */
         dst_ts->arch.vex.guest_GPR0  = ats_new->mst.gpr[0];
         dst_ts->arch.vex.guest_GPR1  = ats_new->mst.gpr[1]; /* sp */
         dst_ts->arch.vex.guest_GPR2  = ats_new->mst.gpr[2]; /* toc */
         dst_ts->arch.vex.guest_GPR3  = ats_new->mst.gpr[3]; /* initarg */
         dst_ts->arch.vex.guest_GPR4  = ats_new->mst.gpr[4];
         dst_ts->arch.vex.guest_GPR5  = ats_new->mst.gpr[5];
         dst_ts->arch.vex.guest_GPR6  = ats_new->mst.gpr[6];
         dst_ts->arch.vex.guest_GPR7  = ats_new->mst.gpr[7];
         dst_ts->arch.vex.guest_GPR8  = ats_new->mst.gpr[8];
         dst_ts->arch.vex.guest_GPR9  = ats_new->mst.gpr[9];
         dst_ts->arch.vex.guest_GPR10 = ats_new->mst.gpr[10];
         dst_ts->arch.vex.guest_GPR11 = ats_new->mst.gpr[11]; /* ?? */
         dst_ts->arch.vex.guest_GPR12 = ats_new->mst.gpr[12];
         dst_ts->arch.vex.guest_GPR13 = ats_new->mst.gpr[13];
         dst_ts->arch.vex.guest_GPR14 = ats_new->mst.gpr[14];
         dst_ts->arch.vex.guest_GPR15 = ats_new->mst.gpr[15];
         dst_ts->arch.vex.guest_GPR16 = ats_new->mst.gpr[16];
         dst_ts->arch.vex.guest_GPR17 = ats_new->mst.gpr[17];
         dst_ts->arch.vex.guest_GPR18 = ats_new->mst.gpr[18];
         dst_ts->arch.vex.guest_GPR19 = ats_new->mst.gpr[19];
         dst_ts->arch.vex.guest_GPR20 = ats_new->mst.gpr[20];
         dst_ts->arch.vex.guest_GPR21 = ats_new->mst.gpr[21];
         dst_ts->arch.vex.guest_GPR22 = ats_new->mst.gpr[22];
         dst_ts->arch.vex.guest_GPR23 = ats_new->mst.gpr[23];
         dst_ts->arch.vex.guest_GPR24 = ats_new->mst.gpr[24];
         dst_ts->arch.vex.guest_GPR25 = ats_new->mst.gpr[25];
         dst_ts->arch.vex.guest_GPR26 = ats_new->mst.gpr[26];
         dst_ts->arch.vex.guest_GPR27 = ats_new->mst.gpr[27];
         dst_ts->arch.vex.guest_GPR28 = ats_new->mst.gpr[28];
         dst_ts->arch.vex.guest_GPR29 = ats_new->mst.gpr[29];
         dst_ts->arch.vex.guest_GPR30 = ats_new->mst.gpr[30];
         dst_ts->arch.vex.guest_GPR31 = ats_new->mst.gpr[31];
         dst_ts->arch.vex.guest_CIA   = ats_new->mst.iar; /* pc */
         dst_ts->arch.vex.guest_LR    = ats_new->mst.lr;
         dst_ts->arch.vex.guest_CTR   = ats_new->mst.ctr;
         LibVEX_GuestPPC64_put_CR( ats_new->mst.cr, &dst_ts->arch.vex );
         LibVEX_GuestPPC64_put_XER( ats_new->mst.xer, &dst_ts->arch.vex );

         /* Record what seems like the highest legitimate stack
            address for this thread, so that the stack unwinder works
            properly.  It seems reasonable to use the R1 value
            supplied here. */
         dst_ts->client_stack_highest_word = dst_ts->arch.vex.guest_GPR1;

         /* The host thread is to start running
            start_thread_NORETURN */
         UWord* wrapper_fdescr = (UWord*) & start_thread_NORETURN;
         ats_new->mst.gpr[1] = (UWord)stack;
         ats_new->mst.gpr[2] = wrapper_fdescr[1];
         ats_new->mst.iar    = wrapper_fdescr[0];
         ats_new->mst.gpr[3] = (UWord)dst_ts;

         /* Set initial cancellation status for the thread. */
         dst_ts->os_state.cancel_async    = False;
         dst_ts->os_state.cancel_disabled = False;
         dst_ts->os_state.cancel_progress = Canc_NoRequest;
      }
   }
}
POST(sys_thread_setstate)
{
   if (ARG3)
      POST_MEM_WRITE( ARG3, sizeof(struct tstate) );
   if (0 && VG_(clo_trace_syscalls) && ARG3)
      ML_(aix5debugstuff_show_tstate)(ARG3, "thread_setstate (OLD)");
}

PRE(sys_FAKE_SIGRETURN)
{
   /* See comments on PRE(sys_rt_sigreturn) in syswrap-amd64-linux.c for
      an explanation of what follows. */
   /* This handles the fake signal-return system call created by
      sigframe-ppc64-aix5.c. */

   PRINT("FAKE_SIGRETURN ( )");

   vg_assert(VG_(is_valid_tid)(tid));
   vg_assert(tid >= 1 && tid < VG_N_THREADS);
   vg_assert(VG_(is_running_thread)(tid));

   /* Remove the signal frame from this thread's (guest) stack,
      in the process restoring the pre-signal guest state. */
   VG_(sigframe_destroy)(tid, True);

   /* Tell the driver not to update the guest state with the "result",
      and set a bogus result to keep it happy. */
   *flags |= SfNoWriteResult;
   SET_STATUS_Success(0);

   /* Check to see if any signals arose as a result of this. */
   *flags |= SfPollAfter;
}


/* ---------------------------------------------------------------------
   The ppc64/AIX5 syscall table
   ------------------------------------------------------------------ */

typedef
   struct {
      UInt* pSysNo;
      SyscallTableEntry wrappers;
   }
   AIX5SCTabEntry;

#undef PLAXY
#undef PLAX_

#define PLAXY(sysno, name)                     \
   { & sysno,                                  \
     { & WRAPPER_PRE_NAME(ppc64_aix5, name),   \
       & WRAPPER_POST_NAME(ppc64_aix5, name) }} 

#define PLAX_(sysno, name)                     \
   { & sysno,                                  \
     { & WRAPPER_PRE_NAME(ppc64_aix5, name),   \
       NULL }} 

static /* but not const */
AIX5SCTabEntry aix5_ppc64_syscall_table[]
= {
    AIXXY(__NR_AIX5___libc_sbrk,        sys___libc_sbrk),
    AIXX_(__NR_AIX5___msleep,           sys___msleep),
    PLAXY(__NR_AIX5__clock_gettime,     sys__clock_gettime),
    AIXX_(__NR_AIX5__exit,              sys__exit),
    PLAX_(__NR_AIX5__fp_fpscrx64_,      sys__fp_fpscrx64_),
    AIXX_(__NR_AIX5__getpid,            sys__getpid),
    AIXXY(__NR_AIX5__nsleep,            sys__nsleep),
    AIXX_(__NR_AIX5__pause,             sys__pause),
    AIXXY(__NR_AIX5__poll,              sys__poll),
    AIXX_(__NR_AIX5__select,            sys__select),
    AIXX_(__NR_AIX5__sem_wait,          sys__sem_wait),
    AIXXY(__NR_AIX5__sigaction,         sys__sigaction),
    AIXX_(__NR_AIX5__thread_self,       sys__thread_self),
    AIXX_(__NR_AIX5_access,             sys_access),
    AIXX_(__NR_AIX5_accessx,            sys_accessx),
    AIXXY(__NR_AIX5_appgetrlimit,       sys_appgetrlimit),
    AIXXY(__NR_AIX5_appgetrusage,       sys_appgetrusage),
    AIXX_(__NR_AIX5_appsetrlimit,       sys_appsetrlimit),
    AIXX_(__NR_AIX5_appulimit,          sys_appulimit),
    AIXX_(__NR_AIX5_bind,               sys_bind),
    AIXX_(__NR_AIX5_chdir,              sys_chdir),
    AIXX_(__NR_AIX5_chmod,              sys_chmod),
    AIXX_(__NR_AIX5_chown,              sys_chown),
    AIXX_(__NR_AIX5_close,              sys_close),
    AIXX_(__NR_AIX5_connext,            sys_connext),
    AIXX_(__NR_AIX5_execve,             sys_execve),
    AIXXY(__NR_AIX5_finfo,              sys_finfo),
    AIXXY(__NR_AIX5_fstatfs,            sys_fstatfs),
    AIXXY(__NR_AIX5_fstatx,             sys_fstatx),
    AIXXY(__NR_AIX5_getdirent,          sys_getdirent),
    AIXXY(__NR_AIX5_getdirent64,        sys_getdirent64),
    AIXXY(__NR_AIX5_getdomainname,      sys_getdomainname),
    AIXX_(__NR_AIX5_getgidx,            sys_getgidx),
    AIXXY(__NR_AIX5_gethostname,        sys_gethostname),
    AIXXY(__NR_AIX5_getpriv,            sys_getpriv),
    AIXXY(__NR_AIX5_getprocs,           sys_getprocs),
    AIXXY(__NR_AIX5_getprocs64,         sys_getprocs), /* XXX: correct? */
    AIXX_(__NR_AIX5_getrpid,            sys_getrpid),
    AIXXY(__NR_AIX5_getsockopt,         sys_getsockopt),
    AIXX_(__NR_AIX5_gettimerid,         sys_gettimerid),
    AIXX_(__NR_AIX5_getuidx,            sys_getuidx),
    AIXXY(__NR_AIX5_incinterval,        sys_incinterval),
    AIXXY(__NR_AIX5_kfcntl,             sys_kfcntl),
    AIXX_(__NR_AIX5_kfork,              sys_kfork),
    AIXX_(__NR_AIX5_kill,               sys_kill),
    AIXXY(__NR_AIX5_kioctl,             sys_kioctl),
    PLAXY(__NR_AIX5_kload,              sys_kload),
    AIXX_(__NR_AIX5_klseek,             sys_klseek),
    AIXXY(__NR_AIX5_kread,              sys_kread),
    AIXXY(__NR_AIX5_kreadv,             sys_kreadv),
    AIXX_(__NR_AIX5_kthread_ctl,        sys_kthread_ctl),
    AIXX_(__NR_AIX5_ktruncate,          sys_ktruncate),
    PLAXY(__NR_AIX5_kunload64,          sys_kunload64),
    AIXXY(__NR_AIX5_kwaitpid,           sys_kwaitpid),
    AIXX_(__NR_AIX5_kwrite,             sys_kwrite),
    AIXX_(__NR_AIX5_kwritev,            sys_kwritev),
    AIXX_(__NR_AIX5_lseek,              sys_lseek),
    AIXX_(__NR_AIX5_mkdir,              sys_mkdir),
    AIXXY(__NR_AIX5_mmap,               sys_mmap),
    AIXXY(__NR_AIX5_mntctl,             sys_mntctl),
    AIXXY(__NR_AIX5_mprotect,           sys_mprotect),
    AIXXY(__NR_AIX5_munmap,             sys_munmap),
    AIXXY(__NR_AIX5_ngetpeername,       sys_ngetpeername),
    AIXXY(__NR_AIX5_ngetsockname,       sys_ngetsockname),
    AIXXY(__NR_AIX5_nrecvfrom,          sys_nrecvfrom),
    AIXX_(__NR_AIX5_nrecvmsg,           sys_nrecvmsg),
    AIXX_(__NR_AIX5_open,               sys_open),
    AIXXY(__NR_AIX5_pipe,               sys_pipe),
    AIXX_(__NR_AIX5_privcheck,          sys_privcheck),
    AIXX_(__NR_AIX5_rename,             sys_rename),
    AIXXY(__NR_AIX5_sbrk,               sys_sbrk),
    AIXXY(__NR_AIX5_sem_init,           sys_sem_init),
    AIXXY(__NR_AIX5_sem_post,           sys_sem_post),
    AIXX_(__NR_AIX5_send,               sys_send),
    AIXX_(__NR_AIX5_setgid,             sys_setgid),
    AIXX_(__NR_AIX5_setsockopt,         sys_setsockopt),
    AIXX_(__NR_AIX5_setuid,             sys_setuid),
    AIXXY(__NR_AIX5_shmat,              sys_shmat),
    AIXXY(__NR_AIX5_shmctl,             sys_shmctl),
    AIXXY(__NR_AIX5_shmdt,              sys_shmdt),
    AIXX_(__NR_AIX5_shmget,             sys_shmget),
    AIXX_(__NR_AIX5_shutdown,           sys_shutdown),
    AIXX_(__NR_AIX5_sigcleanup,         sys_sigcleanup),
    AIXXY(__NR_AIX5_sigprocmask,        sys_sigprocmask),
    AIXXY(__NR_AIX5_sys_parm,           sys_sys_parm),
    AIXXY(__NR_AIX5_sysconfig,          sys_sysconfig),
    AIXX_(__NR_AIX5_socket,             sys_socket),
    AIXXY(__NR_AIX5_statx,              sys_statx),
    AIXXY(__NR_AIX5_thread_create,      sys_thread_create),
    AIXX_(__NR_AIX5_thread_init,        sys_thread_init),
    AIXX_(__NR_AIX5_thread_kill,        sys_thread_kill),
    AIXXY(__NR_AIX5_thread_setmystate,  sys_thread_setmystate),
    AIXX_(__NR_AIX5_thread_setmystate_fast, sys_thread_setmystate_fast),
    PLAXY(__NR_AIX5_thread_setstate,    sys_thread_setstate),
    AIXX_(__NR_AIX5_thread_terminate_unlock, sys_thread_terminate_unlock),
    AIXX_(__NR_AIX5_thread_tsleep,      sys_thread_tsleep),
    AIXX_(__NR_AIX5_thread_twakeup,     sys_thread_twakeup),
    AIXX_(__NR_AIX5_thread_unlock,      sys_thread_unlock),
    AIXX_(__NR_AIX5_thread_waitlock_,   sys_thread_waitlock_),
    AIXXY(__NR_AIX5_times,              sys_times),
    AIXXY(__NR_AIX5_uname,              sys_uname),
    AIXX_(__NR_AIX5_unlink,             sys_unlink),
    AIXX_(__NR_AIX5_utimes,             sys_utimes),
    AIXXY(__NR_AIX5_vmgetinfo,          sys_vmgetinfo),
    AIXX_(__NR_AIX5_yield,              sys_yield),
    PLAX_(__NR_AIX5_FAKE_SIGRETURN,     sys_FAKE_SIGRETURN)
  };

SyscallTableEntry* ML_(get_ppc64_aix5_syscall_entry) ( UInt sysno )
{
   Int            i;
   AIX5SCTabEntry tmp;

   const Int tab_size = sizeof(aix5_ppc64_syscall_table) 
                        / sizeof(aix5_ppc64_syscall_table[0]);

   for (i = 0; i < tab_size; i++)
      if (sysno == *(aix5_ppc64_syscall_table[i].pSysNo))
         break;

   vg_assert(i >= 0 && i <= tab_size);
   if (i == tab_size)
      return NULL; /* can't find a wrapper */

   /* Move found one a bit closer to the front, so as to
      make future searches cheaper. */
   if (i > 0) {
      tmp = aix5_ppc64_syscall_table[i-1];
      aix5_ppc64_syscall_table[i-1] = aix5_ppc64_syscall_table[i];
      aix5_ppc64_syscall_table[i] = tmp;
      i--;
   }

   vg_assert(i >= 0 && i < tab_size);
   return &aix5_ppc64_syscall_table[i].wrappers;
}

#endif // defined(VGP_ppc64_aix5)

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
