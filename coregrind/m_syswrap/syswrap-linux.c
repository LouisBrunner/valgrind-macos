
/*--------------------------------------------------------------------*/
/*--- Linux-specific syscalls, etc.                syswrap-linux.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2007 Nicholas Nethercote
      njn@valgrind.org

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

#include "pub_core_basics.h"
#include "pub_core_vki.h"
#include "pub_core_vkiscnums.h"
#include "pub_core_threadstate.h"
#include "pub_core_aspacemgr.h"
#include "pub_core_debuginfo.h"    // VG_(di_notify_*)
#include "pub_core_transtab.h"     // VG_(discard_translations)
#include "pub_core_xarray.h"
#include "pub_core_clientstate.h"
#include "pub_core_debuglog.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcfile.h"
#include "pub_core_libcprint.h"
#include "pub_core_libcproc.h"
#include "pub_core_libcsignal.h"
#include "pub_core_mallocfree.h"
#include "pub_core_tooliface.h"
#include "pub_core_options.h"
#include "pub_core_scheduler.h"
#include "pub_core_signals.h"
#include "pub_core_syscall.h"
#include "pub_core_syswrap.h"

#include "priv_types_n_macros.h"
#include "priv_syswrap-generic.h"
#include "priv_syswrap-linux.h"


// Run a thread from beginning to end and return the thread's
// scheduler-return-code.
static VgSchedReturnCode thread_wrapper(Word /*ThreadId*/ tidW)
{
   VgSchedReturnCode ret;
   ThreadId     tid = (ThreadId)tidW;
   ThreadState* tst = VG_(get_ThreadState)(tid);

   VG_(debugLog)(1, "syswrap-linux", 
                    "thread_wrapper(tid=%lld): entry\n", 
                    (ULong)tidW);

   vg_assert(tst->status == VgTs_Init);

   /* make sure we get the CPU lock before doing anything significant */
   VG_(acquire_BigLock)(tid, "thread_wrapper(starting new thread)");

   if (0)
      VG_(printf)("thread tid %d started: stack = %p\n",
		  tid, &tid);

   VG_TRACK ( post_thread_create, tst->os_state.parent, tid );

   tst->os_state.lwpid = VG_(gettid)();
   tst->os_state.threadgroup = VG_(getpid)();

   /* Thread created with all signals blocked; scheduler will set the
      appropriate mask */

   ret = VG_(scheduler)(tid);

   vg_assert(VG_(is_exiting)(tid));
   
   vg_assert(tst->status == VgTs_Runnable);
   vg_assert(VG_(is_running_thread)(tid));

   VG_(debugLog)(1, "syswrap-linux", 
                    "thread_wrapper(tid=%lld): exit\n", 
                    (ULong)tidW);

   /* Return to caller, still holding the lock. */
   return ret;
}


/* ---------------------------------------------------------------------
   clone-related stuff
   ------------------------------------------------------------------ */

/* Run a thread all the way to the end, then do appropriate exit actions
   (this is the last-one-out-turn-off-the-lights bit).  */
static void run_a_thread_NORETURN ( Word tidW )
{
   ThreadId          tid = (ThreadId)tidW;
   VgSchedReturnCode src;
   Int               c;

   VG_(debugLog)(1, "syswrap-linux", 
                    "run_a_thread_NORETURN(tid=%lld): pre-thread_wrapper\n",
                    (ULong)tidW);

   /* Run the thread all the way through. */
   src = thread_wrapper(tid);  

   VG_(debugLog)(1, "syswrap-linux", 
                    "run_a_thread_NORETURN(tid=%lld): post-thread_wrapper\n",
                    (ULong)tidW);

   c = VG_(count_living_threads)();
   vg_assert(c >= 1); /* stay sane */

   if (c == 1) {

      VG_(debugLog)(1, "syswrap-linux", 
                       "run_a_thread_NORETURN(tid=%lld): "
                          "last one standing\n",
                          (ULong)tidW);

      /* We are the last one standing.  Keep hold of the lock and
         carry on to show final tool results, then exit the entire system. 
         Use the continuation pointer set at startup in m_main. */
      ( * VG_(address_of_m_main_shutdown_actions_NORETURN) ) (tid, src);

   } else {

      ThreadState *tst;

      VG_(debugLog)(1, "syswrap-linux", 
                       "run_a_thread_NORETURN(tid=%lld): "
                          "not last one standing\n",
                          (ULong)tidW);

      /* OK, thread is dead, but others still exist.  Just exit. */
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
#if defined(VGP_x86_linux)
      asm volatile (
         "movl	%1, %0\n"	/* set tst->status = VgTs_Empty */
         "movl	%2, %%eax\n"    /* set %eax = __NR_exit */
         "movl	%3, %%ebx\n"    /* set %ebx = tst->os_state.exitcode */
         "int	$0x80\n"	/* exit(tst->os_state.exitcode) */
         : "=m" (tst->status)
         : "n" (VgTs_Empty), "n" (__NR_exit), "m" (tst->os_state.exitcode));
#elif defined(VGP_amd64_linux)
      asm volatile (
         "movl	%1, %0\n"	/* set tst->status = VgTs_Empty */
         "movq	%2, %%rax\n"    /* set %rax = __NR_exit */
         "movq	%3, %%rdi\n"    /* set %rdi = tst->os_state.exitcode */
         "syscall\n"		/* exit(tst->os_state.exitcode) */
         : "=m" (tst->status)
         : "n" (VgTs_Empty), "n" (__NR_exit), "m" (tst->os_state.exitcode));
#elif defined(VGP_ppc32_linux) || defined(VGP_ppc64_linux)
      { UInt vgts_empty = (UInt)VgTs_Empty;
        asm volatile (
          "stw %1,%0\n\t"          /* set tst->status = VgTs_Empty */
          "li  0,%2\n\t"           /* set r0 = __NR_exit */
          "lwz 3,%3\n\t"           /* set r3 = tst->os_state.exitcode */
          "sc\n\t"                 /* exit(tst->os_state.exitcode) */
          : "=m" (tst->status)
          : "r" (vgts_empty), "n" (__NR_exit), "m" (tst->os_state.exitcode));
      }
#else
# error Unknown platform
#endif

      VG_(core_panic)("Thread exit failed?\n");
   }

   /*NOTREACHED*/
   vg_assert(0);
}

Word ML_(start_thread_NORETURN) ( void* arg )
{
   ThreadState* tst = (ThreadState*)arg;
   ThreadId     tid = tst->tid;

   run_a_thread_NORETURN ( (Word)tid );
   /*NOTREACHED*/
   vg_assert(0);
}

/* Allocate a stack for this thread, if it doesn't already have one.
   They're allocated lazily, and never freed.  Returns the initial stack
   pointer value to use, or 0 if allocation failed. */
Addr ML_(allocstack)(ThreadId tid)
{
   ThreadState* tst = VG_(get_ThreadState)(tid);
   VgStack*     stack;
   Addr         initial_SP;

   /* Either the stack_base and stack_init_SP are both zero (in which
      case a stack hasn't been allocated) or they are both non-zero,
      in which case it has. */

   if (tst->os_state.valgrind_stack_base == 0)
      vg_assert(tst->os_state.valgrind_stack_init_SP == 0);

   if (tst->os_state.valgrind_stack_base != 0)
      vg_assert(tst->os_state.valgrind_stack_init_SP != 0);

   /* If no stack is present, allocate one. */

   if (tst->os_state.valgrind_stack_base == 0) {
      stack = VG_(am_alloc_VgStack)( &initial_SP );
      if (stack) {
         tst->os_state.valgrind_stack_base    = (Addr)stack;
         tst->os_state.valgrind_stack_init_SP = initial_SP;
      }
   }

   if (0)
      VG_(printf)( "stack for tid %d at %p; init_SP=%p\n",
                   tid, 
                   (void*)tst->os_state.valgrind_stack_base, 
                   (void*)tst->os_state.valgrind_stack_init_SP );
                  
   return tst->os_state.valgrind_stack_init_SP;
}

/* Allocate a stack for the main thread, and run it all the way to the
   end.  Although we already have a working VgStack
   (VG_(interim_stack)) it's better to allocate a new one, so that
   overflow detection works uniformly for all threads.
*/
void VG_(main_thread_wrapper_NORETURN)(ThreadId tid)
{
   Addr sp;
   VG_(debugLog)(1, "syswrap-linux", 
                    "entering VG_(main_thread_wrapper_NORETURN)\n");

   sp = ML_(allocstack)(tid);

#if defined(VGP_ppc32_linux)
   /* make a stack frame */
   sp -= 16;
   sp &= ~0xF;
   *(UWord *)sp = 0;
#elif defined(VGP_ppc64_linux)
   /* make a stack frame */
   sp -= 112;
   sp &= ~((Addr)0xF);
   *(UWord *)sp = 0;
#endif

   /* If we can't even allocate the first thread's stack, we're hosed.
      Give up. */
   vg_assert2(sp != 0, "Cannot allocate main thread's stack.");

   /* shouldn't be any other threads around yet */
   vg_assert( VG_(count_living_threads)() == 1 );

   ML_(call_on_new_stack_0_1)( 
      (Addr)sp,               /* stack */
      0,                      /* bogus return address */
      run_a_thread_NORETURN,  /* fn to call */
      (Word)tid               /* arg to give it */
   );

   /*NOTREACHED*/
   vg_assert(0);
}


/* Do a clone which is really a fork() */
SysRes ML_(do_fork_clone) ( ThreadId tid, UInt flags,
                            Int* parent_tidptr, Int* child_tidptr )
{
   vki_sigset_t fork_saved_mask;
   vki_sigset_t mask;
   SysRes       res;

   if (flags & (VKI_CLONE_SETTLS | VKI_CLONE_FS | VKI_CLONE_VM 
                | VKI_CLONE_FILES | VKI_CLONE_VFORK))
      return VG_(mk_SysRes_Error)( VKI_EINVAL );

   /* Block all signals during fork, so that we can fix things up in
      the child without being interrupted. */
   VG_(sigfillset)(&mask);
   VG_(sigprocmask)(VKI_SIG_SETMASK, &mask, &fork_saved_mask);

   /* Since this is the fork() form of clone, we don't need all that
      VG_(clone) stuff */
#if defined(VGP_x86_linux) || defined(VGP_ppc32_linux) || defined(VGP_ppc64_linux)
   res = VG_(do_syscall5)( __NR_clone, flags, 
                           (UWord)NULL, (UWord)parent_tidptr, 
                           (UWord)NULL, (UWord)child_tidptr );
#elif defined(VGP_amd64_linux)
   /* note that the last two arguments are the opposite way round to x86 and
      ppc32 as the amd64 kernel expects the arguments in a different order */
   res = VG_(do_syscall5)( __NR_clone, flags, 
                           (UWord)NULL, (UWord)parent_tidptr, 
                           (UWord)child_tidptr, (UWord)NULL );
#else
# error Unknown platform
#endif

   if (!res.isError && res.res == 0) {
      /* child */
      VG_(do_atfork_child)(tid);

      /* restore signal mask */
      VG_(sigprocmask)(VKI_SIG_SETMASK, &fork_saved_mask, NULL);
   } 
   else 
   if (!res.isError && res.res > 0) {
      /* parent */
      if (VG_(clo_trace_syscalls))
	  VG_(printf)("   clone(fork): process %d created child %d\n", 
                      VG_(getpid)(), res.res);

      /* restore signal mask */
      VG_(sigprocmask)(VKI_SIG_SETMASK, &fork_saved_mask, NULL);
   }

   return res;
}


/* ---------------------------------------------------------------------
   PRE/POST wrappers for arch-generic, Linux-specific syscalls
   ------------------------------------------------------------------ */

// Nb: See the comment above the generic PRE/POST wrappers in
// m_syswrap/syswrap-generic.c for notes about how they work.

#define PRE(name)       DEFN_PRE_TEMPLATE(linux, name)
#define POST(name)      DEFN_POST_TEMPLATE(linux, name)

// Combine two 32-bit values into a 64-bit value
#define LOHI64(lo,hi)   ( (lo) | ((ULong)(hi) << 32) )

/* ---------------------------------------------------------------------
   *mount wrappers
   ------------------------------------------------------------------ */

PRE(sys_mount)
{
   // Nb: depending on 'flags', the 'type' and 'data' args may be ignored.
   // We are conservative and check everything, except the memory pointed to
   // by 'data'.
   *flags |= SfMayBlock;
   PRINT( "sys_mount( %p, %p, %p, %p, %p )" ,ARG1,ARG2,ARG3,ARG4,ARG5);
   PRE_REG_READ5(long, "mount",
                 char *, source, char *, target, char *, type,
                 unsigned long, flags, void *, data);
   PRE_MEM_RASCIIZ( "mount(source)", ARG1);
   PRE_MEM_RASCIIZ( "mount(target)", ARG2);
   PRE_MEM_RASCIIZ( "mount(type)", ARG3);
}

PRE(sys_oldumount)
{
   PRINT("sys_oldumount( %p )", ARG1);
   PRE_REG_READ1(long, "umount", char *, path);
   PRE_MEM_RASCIIZ( "umount(path)", ARG1);
}

PRE(sys_umount)
{
   PRINT("sys_umount( %p, %d )", ARG1, ARG2);
   PRE_REG_READ2(long, "umount2", char *, path, int, flags);
   PRE_MEM_RASCIIZ( "umount2(path)", ARG1);
}

/* ---------------------------------------------------------------------
   16- and 32-bit uid/gid wrappers
   ------------------------------------------------------------------ */

PRE(sys_setfsuid16)
{
   PRINT("sys_setfsuid16 ( %d )", ARG1);
   PRE_REG_READ1(long, "setfsuid16", vki_old_uid_t, uid);
}

PRE(sys_setfsuid)
{
   PRINT("sys_setfsuid ( %d )", ARG1);
   PRE_REG_READ1(long, "setfsuid", vki_uid_t, uid);
}

PRE(sys_setfsgid16)
{
   PRINT("sys_setfsgid16 ( %d )", ARG1);
   PRE_REG_READ1(long, "setfsgid16", vki_old_gid_t, gid);
}

PRE(sys_setfsgid)
{
   PRINT("sys_setfsgid ( %d )", ARG1);
   PRE_REG_READ1(long, "setfsgid", vki_gid_t, gid);
}

PRE(sys_setresuid16)
{
   PRINT("sys_setresuid16 ( %d, %d, %d )", ARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "setresuid16",
                 vki_old_uid_t, ruid, vki_old_uid_t, euid, vki_old_uid_t, suid);
}

PRE(sys_setresuid)
{
   PRINT("sys_setresuid ( %d, %d, %d )", ARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "setresuid",
                 vki_uid_t, ruid, vki_uid_t, euid, vki_uid_t, suid);
}

PRE(sys_getresuid16)
{
   PRINT("sys_getresuid16 ( %p, %p, %p )", ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "getresuid16",
                 vki_old_uid_t *, ruid, vki_old_uid_t *, euid,
                 vki_old_uid_t *, suid);
   PRE_MEM_WRITE( "getresuid16(ruid)", ARG1, sizeof(vki_old_uid_t) );
   PRE_MEM_WRITE( "getresuid16(euid)", ARG2, sizeof(vki_old_uid_t) );
   PRE_MEM_WRITE( "getresuid16(suid)", ARG3, sizeof(vki_old_uid_t) );
}
POST(sys_getresuid16)
{
   vg_assert(SUCCESS);
   if (RES == 0) {
      POST_MEM_WRITE( ARG1, sizeof(vki_old_uid_t) );
      POST_MEM_WRITE( ARG2, sizeof(vki_old_uid_t) );
      POST_MEM_WRITE( ARG3, sizeof(vki_old_uid_t) );
   }
}

PRE(sys_getresuid)
{
   PRINT("sys_getresuid ( %p, %p, %p )", ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "getresuid", 
                 vki_uid_t *, ruid, vki_uid_t *, euid, vki_uid_t *, suid);
   PRE_MEM_WRITE( "getresuid(ruid)", ARG1, sizeof(vki_uid_t) );
   PRE_MEM_WRITE( "getresuid(euid)", ARG2, sizeof(vki_uid_t) );
   PRE_MEM_WRITE( "getresuid(suid)", ARG3, sizeof(vki_uid_t) );
}
POST(sys_getresuid)
{
   vg_assert(SUCCESS);
   if (RES == 0) {
      POST_MEM_WRITE( ARG1, sizeof(vki_uid_t) );
      POST_MEM_WRITE( ARG2, sizeof(vki_uid_t) );
      POST_MEM_WRITE( ARG3, sizeof(vki_uid_t) );
   }
}

PRE(sys_setresgid16)
{
   PRINT("sys_setresgid16 ( %d, %d, %d )", ARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "setresgid16",
                 vki_old_gid_t, rgid, 
                 vki_old_gid_t, egid, vki_old_gid_t, sgid);
}

PRE(sys_setresgid)
{
   PRINT("sys_setresgid ( %d, %d, %d )", ARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "setresgid",
                 vki_gid_t, rgid, vki_gid_t, egid, vki_gid_t, sgid);
}

PRE(sys_getresgid16)
{
   PRINT("sys_getresgid16 ( %p, %p, %p )", ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "getresgid16",
                 vki_old_gid_t *, rgid, vki_old_gid_t *, egid,
                 vki_old_gid_t *, sgid);
   PRE_MEM_WRITE( "getresgid16(rgid)", ARG1, sizeof(vki_old_gid_t) );
   PRE_MEM_WRITE( "getresgid16(egid)", ARG2, sizeof(vki_old_gid_t) );
   PRE_MEM_WRITE( "getresgid16(sgid)", ARG3, sizeof(vki_old_gid_t) );
}
POST(sys_getresgid16)
{
   vg_assert(SUCCESS);
   if (RES == 0) {
      POST_MEM_WRITE( ARG1, sizeof(vki_old_gid_t) );
      POST_MEM_WRITE( ARG2, sizeof(vki_old_gid_t) );
      POST_MEM_WRITE( ARG3, sizeof(vki_old_gid_t) );
   }
}

PRE(sys_getresgid)
{
   PRINT("sys_getresgid ( %p, %p, %p )", ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "getresgid", 
                 vki_gid_t *, rgid, vki_gid_t *, egid, vki_gid_t *, sgid);
   PRE_MEM_WRITE( "getresgid(rgid)", ARG1, sizeof(vki_gid_t) );
   PRE_MEM_WRITE( "getresgid(egid)", ARG2, sizeof(vki_gid_t) );
   PRE_MEM_WRITE( "getresgid(sgid)", ARG3, sizeof(vki_gid_t) );
}
POST(sys_getresgid)
{
   vg_assert(SUCCESS);
   if (RES == 0) {
      POST_MEM_WRITE( ARG1, sizeof(vki_gid_t) );
      POST_MEM_WRITE( ARG2, sizeof(vki_gid_t) );
      POST_MEM_WRITE( ARG3, sizeof(vki_gid_t) );
   }
}

/* ---------------------------------------------------------------------
   miscellaneous wrappers
   ------------------------------------------------------------------ */

PRE(sys_exit_group)
{
   ThreadId     t;
   ThreadState* tst;

   PRINT("exit_group( %d )", ARG1);
   PRE_REG_READ1(void, "exit_group", int, exit_code);

   tst = VG_(get_ThreadState)(tid);

   /* A little complex; find all the threads with the same threadgroup
      as this one (including this one), and mark them to exit */
   for (t = 1; t < VG_N_THREADS; t++) {
      if ( /* not alive */
           VG_(threads)[t].status == VgTs_Empty 
           ||
	   /* not our group */
           VG_(threads)[t].os_state.threadgroup != tst->os_state.threadgroup
         )
         continue;

      VG_(threads)[t].exitreason = VgSrc_ExitThread;
      VG_(threads)[t].os_state.exitcode = ARG1;

      if (t != tid)
	 VG_(get_thread_out_of_syscall)(t); /* unblock it, if blocked */
   }

   /* We have to claim the syscall already succeeded. */
   SET_STATUS_Success(0);
}

PRE(sys_llseek)
{
   PRINT("sys_llseek ( %d, 0x%x, 0x%x, %p, %d )", ARG1,ARG2,ARG3,ARG4,ARG5);
   PRE_REG_READ5(long, "llseek",
                 unsigned int, fd, unsigned long, offset_high,
                 unsigned long, offset_low, vki_loff_t *, result,
                 unsigned int, whence);
   PRE_MEM_WRITE( "llseek(result)", ARG4, sizeof(vki_loff_t));
}
POST(sys_llseek)
{
   vg_assert(SUCCESS);
   if (RES == 0)
      POST_MEM_WRITE( ARG4, sizeof(vki_loff_t) );
}

//zz PRE(sys_adjtimex, 0)
//zz {
//zz    struct vki_timex *tx = (struct vki_timex *)ARG1;
//zz    PRINT("sys_adjtimex ( %p )", ARG1);
//zz    PRE_REG_READ1(long, "adjtimex", struct timex *, buf);
//zz    PRE_MEM_READ( "adjtimex(timex->modes)", ARG1, sizeof(tx->modes));
//zz 
#if 0 //zz  (avoiding warnings about multi-line comments)
zz #define ADJX(bit,field) 				\
zz    if (tx->modes & bit)					\
zz       PRE_MEM_READ( "adjtimex(timex->"#field")",	\
zz 		    (Addr)&tx->field, sizeof(tx->field))
#endif
//zz    ADJX(ADJ_FREQUENCY, freq);
//zz    ADJX(ADJ_MAXERROR, maxerror);
//zz    ADJX(ADJ_ESTERROR, esterror);
//zz    ADJX(ADJ_STATUS, status);
//zz    ADJX(ADJ_TIMECONST, constant);
//zz    ADJX(ADJ_TICK, tick);
//zz #undef ADJX
//zz    
//zz    PRE_MEM_WRITE( "adjtimex(timex)", ARG1, sizeof(struct vki_timex));
//zz }
//zz 
//zz POST(sys_adjtimex)
//zz {
//zz    POST_MEM_WRITE( ARG1, sizeof(struct vki_timex) );
//zz }

PRE(sys_ioperm)
{
   PRINT("sys_ioperm ( %d, %d, %d )", ARG1, ARG2, ARG3 );
   PRE_REG_READ3(long, "ioperm",
                 unsigned long, from, unsigned long, num, int, turn_on);
}

PRE(sys_syslog)
{
   *flags |= SfMayBlock;
   PRINT("sys_syslog (%d, %p, %d)", ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "syslog", int, type, char *, bufp, int, len);
   switch (ARG1) {
   // The kernel uses magic numbers here, rather than named constants,
   // therefore so do we.
   case 2: case 3: case 4:
      PRE_MEM_WRITE( "syslog(bufp)", ARG2, ARG3);
      break;
   default: 
      break;
   }
}
POST(sys_syslog)
{
   switch (ARG1) {
   case 2: case 3: case 4:
      POST_MEM_WRITE( ARG2, ARG3 );
      break;
   default:
      break;
   }
}

PRE(sys_vhangup)
{
   PRINT("sys_vhangup ( )");
   PRE_REG_READ0(long, "vhangup");
}

PRE(sys_sysinfo)
{
   PRINT("sys_sysinfo ( %p )",ARG1);
   PRE_REG_READ1(long, "sysinfo", struct sysinfo *, info);
   PRE_MEM_WRITE( "sysinfo(info)", ARG1, sizeof(struct vki_sysinfo) );
}
POST(sys_sysinfo)
{
   POST_MEM_WRITE( ARG1, sizeof(struct vki_sysinfo) );
}

PRE(sys_personality)
{
   PRINT("sys_personality ( %llu )", (ULong)ARG1);
   PRE_REG_READ1(long, "personality", vki_u_long, persona);
}

PRE(sys_sysctl)
{
   struct __vki_sysctl_args *args;
   PRINT("sys_sysctl ( %p )", ARG1 );
   args = (struct __vki_sysctl_args *)ARG1;
   PRE_REG_READ1(long, "sysctl", struct __sysctl_args *, args);
   PRE_MEM_WRITE( "sysctl(args)", ARG1, sizeof(struct __vki_sysctl_args) );
   if (!VG_(am_is_valid_for_client)(ARG1, sizeof(struct __vki_sysctl_args), 
                                          VKI_PROT_READ)) {
      SET_STATUS_Failure( VKI_EFAULT );
      return;
   }

   PRE_MEM_READ("sysctl(name)", (Addr)args->name, args->nlen * sizeof(*args->name));
   if (args->newval != NULL)
      PRE_MEM_READ("sysctl(newval)", (Addr)args->newval, args->newlen);
   if (args->oldlenp != NULL) {
      PRE_MEM_READ("sysctl(oldlenp)", (Addr)args->oldlenp, sizeof(*args->oldlenp));
      PRE_MEM_WRITE("sysctl(oldval)", (Addr)args->oldval, *args->oldlenp);
   }
}
POST(sys_sysctl)
{
   struct __vki_sysctl_args *args;
   args = (struct __vki_sysctl_args *)ARG1;
   if (args->oldlenp != NULL) {
      POST_MEM_WRITE((Addr)args->oldlenp, sizeof(*args->oldlenp));
      POST_MEM_WRITE((Addr)args->oldval, 1 + *args->oldlenp);
   }
}

PRE(sys_prctl)
{
   *flags |= SfMayBlock;
   PRINT( "sys_prctl ( %d, %d, %d, %d, %d )", ARG1, ARG2, ARG3, ARG4, ARG5 );
   switch (ARG1) {
   case VKI_PR_SET_PDEATHSIG:
      PRE_REG_READ2(int, "prctl", int, option, int, signal);
      break;
   case VKI_PR_GET_PDEATHSIG:
      PRE_REG_READ2(int, "prctl", int, option, int *, signal);
      PRE_MEM_WRITE("prctl(get-death-signal)", ARG2, sizeof(Int));
      break;
   case VKI_PR_GET_DUMPABLE:
      PRE_REG_READ1(int, "prctl", int, option);
      break;
   case VKI_PR_SET_DUMPABLE:
      PRE_REG_READ2(int, "prctl", int, option, int, dump);
      break;
   case VKI_PR_GET_UNALIGN:
      PRE_REG_READ2(int, "prctl", int, option, int *, value);
      PRE_MEM_WRITE("prctl(get-unalign)", ARG2, sizeof(Int));
      break;
   case VKI_PR_SET_UNALIGN:
      PRE_REG_READ2(int, "prctl", int, option, int, value);
      break;
   case VKI_PR_GET_KEEPCAPS:
      PRE_REG_READ1(int, "prctl", int, option);
      break;
   case VKI_PR_SET_KEEPCAPS:
      PRE_REG_READ2(int, "prctl", int, option, int, keepcaps);
      break;
   case VKI_PR_GET_FPEMU:
      PRE_REG_READ2(int, "prctl", int, option, int *, value);
      PRE_MEM_WRITE("prctl(get-fpemu)", ARG2, sizeof(Int));
      break;
   case VKI_PR_SET_FPEMU:
      PRE_REG_READ2(int, "prctl", int, option, int, value);
      break;
   case VKI_PR_GET_FPEXC:
      PRE_REG_READ2(int, "prctl", int, option, int *, value);
      PRE_MEM_WRITE("prctl(get-fpexc)", ARG2, sizeof(Int));
      break;
   case VKI_PR_SET_FPEXC:
      PRE_REG_READ2(int, "prctl", int, option, int, value);
      break;
   case VKI_PR_GET_TIMING:
      PRE_REG_READ1(int, "prctl", int, option);
      break;
   case VKI_PR_SET_TIMING:
      PRE_REG_READ2(int, "prctl", int, option, int, timing);
      break;
   case VKI_PR_SET_NAME:
      PRE_REG_READ2(int, "prctl", int, option, char *, name);
      PRE_MEM_RASCIIZ("prctl(set-name)", ARG2);
      break;
   case VKI_PR_GET_NAME:
      PRE_REG_READ2(int, "prctl", int, option, char *, name);
      PRE_MEM_WRITE("prctl(get-name)", ARG2, VKI_TASK_COMM_LEN);
      break;
   case VKI_PR_GET_ENDIAN:
      PRE_REG_READ2(int, "prctl", int, option, int *, value);
      PRE_MEM_WRITE("prctl(get-endian)", ARG2, sizeof(Int));
      break;
   case VKI_PR_SET_ENDIAN:
      PRE_REG_READ2(int, "prctl", int, option, int, value);
      break;
   default:
      PRE_REG_READ5(long, "prctl",
                    int, option, unsigned long, arg2, unsigned long, arg3,
                    unsigned long, arg4, unsigned long, arg5);
      break;
   }
}
POST(sys_prctl)
{
   switch (ARG1) {
   case VKI_PR_GET_PDEATHSIG:
      POST_MEM_WRITE(ARG2, sizeof(Int));
      break;
   case VKI_PR_GET_UNALIGN:
      POST_MEM_WRITE(ARG2, sizeof(Int));
      break;
   case VKI_PR_GET_FPEMU:
      POST_MEM_WRITE(ARG2, sizeof(Int));
      break;
   case VKI_PR_GET_FPEXC:
      POST_MEM_WRITE(ARG2, sizeof(Int));
      break;
   case VKI_PR_GET_NAME:
      POST_MEM_WRITE(ARG2, VKI_TASK_COMM_LEN);
      break;
   case VKI_PR_GET_ENDIAN:
      POST_MEM_WRITE(ARG2, sizeof(Int));
      break;
   }
}

PRE(sys_sendfile)
{
   *flags |= SfMayBlock;
   PRINT("sys_sendfile ( %d, %d, %p, %lu )", ARG1,ARG2,ARG3,ARG4);
   PRE_REG_READ4(ssize_t, "sendfile",
                 int, out_fd, int, in_fd, vki_off_t *, offset,
                 vki_size_t, count);
   if (ARG3 != 0)
      PRE_MEM_WRITE( "sendfile(offset)", ARG3, sizeof(vki_off_t) );
}
POST(sys_sendfile)
{
   if (ARG3 != 0 ) {
      POST_MEM_WRITE( ARG3, sizeof( vki_off_t ) );
   }
}

PRE(sys_sendfile64)
{
   *flags |= SfMayBlock;
   PRINT("sendfile64 ( %d, %d, %p, %lu )",ARG1,ARG2,ARG3,ARG4);
   PRE_REG_READ4(ssize_t, "sendfile64",
                 int, out_fd, int, in_fd, vki_loff_t *, offset,
                 vki_size_t, count);
   if (ARG3 != 0)
      PRE_MEM_WRITE( "sendfile64(offset)", ARG3, sizeof(vki_loff_t) );
}
POST(sys_sendfile64)
{
   if (ARG3 != 0 ) {
      POST_MEM_WRITE( ARG3, sizeof(vki_loff_t) );
   }
}

PRE(sys_futex)
{
   /* 
      arg    param                              used by ops

      ARG1 - u32 *futex				all
      ARG2 - int op
      ARG3 - int val				WAIT,WAKE,FD,REQUEUE,CMP_REQUEUE
      ARG4 - struct timespec *utime		WAIT:time*	REQUEUE,CMP_REQUEUE:val2
      ARG5 - u32 *uaddr2			REQUEUE,CMP_REQUEUE
      ARG6 - int val3				CMP_REQUEUE
    */
   PRINT("sys_futex ( %p, %d, %d, %p, %p )", ARG1,ARG2,ARG3,ARG4,ARG5);
   switch(ARG2) {
   case VKI_FUTEX_CMP_REQUEUE:
      PRE_REG_READ6(long, "futex", 
                    vki_u32 *, futex, int, op, int, val,
                    struct timespec *, utime, vki_u32 *, uaddr2, int, val3);
      break;
   case VKI_FUTEX_REQUEUE:
      PRE_REG_READ5(long, "futex", 
                    vki_u32 *, futex, int, op, int, val,
                    struct timespec *, utime, vki_u32 *, uaddr2);
      break;
   case VKI_FUTEX_WAIT:
      PRE_REG_READ4(long, "futex", 
                    vki_u32 *, futex, int, op, int, val,
                    struct timespec *, utime);
      break;
   case VKI_FUTEX_WAKE:
   case VKI_FUTEX_FD:
      PRE_REG_READ3(long, "futex", 
                    vki_u32 *, futex, int, op, int, val);
      break;
   default:
      PRE_REG_READ2(long, "futex", vki_u32 *, futex, int, op);
      break;
   }

   PRE_MEM_READ( "futex(futex)", ARG1, sizeof(Int) );

   *flags |= SfMayBlock;

   switch(ARG2) {
   case VKI_FUTEX_WAIT:
      if (ARG4 != 0)
	 PRE_MEM_READ( "futex(timeout)", ARG4, sizeof(struct vki_timespec) );
      break;

   case VKI_FUTEX_REQUEUE:
   case VKI_FUTEX_CMP_REQUEUE:
      PRE_MEM_READ( "futex(futex2)", ARG5, sizeof(Int) );
      break;

   case VKI_FUTEX_WAKE:
   case VKI_FUTEX_FD:
      /* no additional pointers */
      break;

   default:
      SET_STATUS_Failure( VKI_ENOSYS );   // some futex function we don't understand
      break;
   }
}
POST(sys_futex)
{
   vg_assert(SUCCESS);
   POST_MEM_WRITE( ARG1, sizeof(int) );
   if (ARG2 == VKI_FUTEX_FD) {
      if (!ML_(fd_allowed)(RES, "futex", tid, True)) {
         VG_(close)(RES);
         SET_STATUS_Failure( VKI_EMFILE );
      } else {
         if (VG_(clo_track_fds))
            ML_(record_fd_open_nameless)(tid, RES);
      }
   }
}

PRE(sys_set_robust_list)
{
   PRINT("sys_set_robust_list ( %p, %d )", ARG1,ARG2);
   PRE_REG_READ2(long, "set_robust_list", 
                 struct vki_robust_list_head *, head, vki_size_t, len);

   /* Just check the robust_list_head structure is readable - don't
      try and chase the list as the kernel will only read it when
      the thread exits so the current contents is irrelevant. */
   if (ARG1 != 0)
      PRE_MEM_READ("set_robust_list(head)", ARG1, ARG2);
}

PRE(sys_get_robust_list)
{
   PRINT("sys_get_robust_list ( %d, %p, %d )", ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "get_robust_list",
                 int, pid,
                 struct vki_robust_list_head **, head_ptr,
                 vki_size_t *, len_ptr);
   PRE_MEM_WRITE("get_robust_list(head_ptr)",
                 ARG2, sizeof(struct vki_robust_list_head *));
   PRE_MEM_WRITE("get_robust_list(len_ptr)",
                 ARG3, sizeof(struct vki_size_t *));
}
POST(sys_get_robust_list)
{
   POST_MEM_WRITE(ARG2, sizeof(struct vki_robust_list_head *));
   POST_MEM_WRITE(ARG3, sizeof(struct vki_size_t *));
}

PRE(sys_pselect6)
{
   *flags |= SfMayBlock;
   PRINT("sys_pselect6 ( %d, %p, %p, %p, %p, %p )", ARG1,ARG2,ARG3,ARG4,ARG5,ARG6);
   PRE_REG_READ6(long, "pselect6",
                 int, n, vki_fd_set *, readfds, vki_fd_set *, writefds,
                 vki_fd_set *, exceptfds, struct vki_timeval *, timeout,
                 void *, sig);
   // XXX: this possibly understates how much memory is read.
   if (ARG2 != 0)
      PRE_MEM_READ( "pselect6(readfds)",   
		     ARG2, ARG1/8 /* __FD_SETSIZE/8 */ );
   if (ARG3 != 0)
      PRE_MEM_READ( "pselect6(writefds)",  
		     ARG3, ARG1/8 /* __FD_SETSIZE/8 */ );
   if (ARG4 != 0)
      PRE_MEM_READ( "pselect6(exceptfds)", 
		     ARG4, ARG1/8 /* __FD_SETSIZE/8 */ );
   if (ARG5 != 0)
      PRE_MEM_READ( "pselect6(timeout)", ARG5, sizeof(struct vki_timeval) );
   if (ARG6 != 0)
      PRE_MEM_READ( "pselect6(sig)", ARG6, sizeof(void *)+sizeof(vki_size_t) );
}

PRE(sys_ppoll)
{
   UInt i;
   struct vki_pollfd* ufds = (struct vki_pollfd *)ARG1;
   *flags |= SfMayBlock;
   PRINT("sys_ppoll ( %p, %d, %p, %p, %llu )\n", ARG1,ARG2,ARG3,ARG4,ARG5);
   PRE_REG_READ5(long, "ppoll",
                 struct vki_pollfd *, ufds, unsigned int, nfds,
                 struct vki_timespec *, tsp, vki_sigset_t *, sigmask,
                 vki_size_t, sigsetsize);

   for (i = 0; i < ARG2; i++) {
      PRE_MEM_READ( "ppoll(ufds.fd)",
                    (Addr)(&ufds[i].fd), sizeof(ufds[i].fd) );
      PRE_MEM_READ( "ppoll(ufds.events)",
                    (Addr)(&ufds[i].events), sizeof(ufds[i].events) );
      PRE_MEM_WRITE( "ppoll(ufd.reventss)",
                     (Addr)(&ufds[i].revents), sizeof(ufds[i].revents) );
   }

   if (ARG3)
      PRE_MEM_READ( "ppoll(tsp)", ARG3, sizeof(struct vki_timespec) );
   if (ARG4)
      PRE_MEM_READ( "ppoll(sigmask)", ARG4, sizeof(vki_sigset_t) );
}

POST(sys_ppoll)
{
   if (RES > 0) {
      UInt i;
      struct vki_pollfd* ufds = (struct vki_pollfd *)ARG1;
      for (i = 0; i < ARG2; i++)
	 POST_MEM_WRITE( (Addr)(&ufds[i].revents), sizeof(ufds[i].revents) );
   }
}


/* ---------------------------------------------------------------------
   epoll_* wrappers
   ------------------------------------------------------------------ */

PRE(sys_epoll_create)
{
   PRINT("sys_epoll_create ( %d )", ARG1);
   PRE_REG_READ1(long, "epoll_create", int, size);
}
POST(sys_epoll_create)
{
   vg_assert(SUCCESS);
   if (!ML_(fd_allowed)(RES, "epoll_create", tid, True)) {
      VG_(close)(RES);
      SET_STATUS_Failure( VKI_EMFILE );
   } else {
      if (VG_(clo_track_fds))
         ML_(record_fd_open_nameless) (tid, RES);
   }
}

PRE(sys_epoll_ctl)
{
   static const HChar* epoll_ctl_s[3] = {
      "EPOLL_CTL_ADD",
      "EPOLL_CTL_DEL",
      "EPOLL_CTL_MOD"
   };
   PRINT("sys_epoll_ctl ( %d, %s, %d, %p )", 
         ARG1, ( ARG2<3 ? epoll_ctl_s[ARG2] : "?" ), ARG3, ARG4);
   PRE_REG_READ4(long, "epoll_ctl",
                 int, epfd, int, op, int, fd, struct vki_epoll_event *, event);
   if (ARG2 != VKI_EPOLL_CTL_DEL)
      PRE_MEM_READ( "epoll_ctl(event)", ARG4, sizeof(struct vki_epoll_event) );
}

PRE(sys_epoll_wait)
{
   *flags |= SfMayBlock;
   PRINT("sys_epoll_wait ( %d, %p, %d, %d )", ARG1, ARG2, ARG3, ARG4);
   PRE_REG_READ4(long, "epoll_wait",
                 int, epfd, struct vki_epoll_event *, events,
                 int, maxevents, int, timeout);
   PRE_MEM_WRITE( "epoll_wait(events)", ARG2, sizeof(struct vki_epoll_event)*ARG3);
}
POST(sys_epoll_wait)
{
   vg_assert(SUCCESS);
   if (RES > 0)
      POST_MEM_WRITE( ARG2, sizeof(struct vki_epoll_event)*RES ) ;
}

/* ---------------------------------------------------------------------
   tid-related wrappers
   ------------------------------------------------------------------ */

PRE(sys_gettid)
{
   PRINT("sys_gettid ()");
   PRE_REG_READ0(long, "gettid");
}

PRE(sys_set_tid_address)
{
   PRINT("sys_set_tid_address ( %p )", ARG1);
   PRE_REG_READ1(long, "set_tid_address", int *, tidptr);
}

PRE(sys_tkill)
{
   PRINT("sys_tgkill ( %d, %d )", ARG1,ARG2);
   PRE_REG_READ2(long, "tkill", int, tid, int, sig);
   if (!ML_(client_signal_OK)(ARG2)) {
      SET_STATUS_Failure( VKI_EINVAL );
      return;
   }
   
   /* Check to see if this kill gave us a pending signal */
   *flags |= SfPollAfter;

   if (VG_(clo_trace_signals))
      VG_(message)(Vg_DebugMsg, "tkill: sending signal %d to pid %d",
		   ARG2, ARG1);

   /* If we're sending SIGKILL, check to see if the target is one of
      our threads and handle it specially. */
   if (ARG2 == VKI_SIGKILL && ML_(do_sigkill)(ARG1, -1)) {
      SET_STATUS_Success(0);
      return;
   }

   /* Ask to handle this syscall via the slow route, since that's the
      only one that sets tst->status to VgTs_WaitSys.  If the result
      of doing the syscall is an immediate run of
      async_signalhandler() in m_signals, then we need the thread to
      be properly tidied away.  I have the impression the previous
      version of this wrapper worked on x86/amd64 only because the
      kernel did not immediately deliver the async signal to this
      thread (on ppc it did, which broke the assertion re tst->status
      at the top of async_signalhandler()). */
   *flags |= SfMayBlock;
}
POST(sys_tkill)
{
   if (VG_(clo_trace_signals))
      VG_(message)(Vg_DebugMsg, "tkill: sent signal %d to pid %d",
                   ARG2, ARG1);
}

PRE(sys_tgkill)
{
   PRINT("sys_tgkill ( %d, %d, %d )", ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "tgkill", int, tgid, int, tid, int, sig);
   if (!ML_(client_signal_OK)(ARG3)) {
      SET_STATUS_Failure( VKI_EINVAL );
      return;
   }
   
   /* Check to see if this kill gave us a pending signal */
   *flags |= SfPollAfter;

   if (VG_(clo_trace_signals))
      VG_(message)(Vg_DebugMsg, "tgkill: sending signal %d to pid %d/%d",
		   ARG3, ARG1, ARG2);

   /* If we're sending SIGKILL, check to see if the target is one of
      our threads and handle it specially. */
   if (ARG3 == VKI_SIGKILL && ML_(do_sigkill)(ARG2, ARG1)) {
      SET_STATUS_Success(0);
      return;
   }

   /* Ask to handle this syscall via the slow route, since that's the
      only one that sets tst->status to VgTs_WaitSys.  If the result
      of doing the syscall is an immediate run of
      async_signalhandler() in m_signals, then we need the thread to
      be properly tidied away.  I have the impression the previous
      version of this wrapper worked on x86/amd64 only because the
      kernel did not immediately deliver the async signal to this
      thread (on ppc it did, which broke the assertion re tst->status
      at the top of async_signalhandler()). */
   *flags |= SfMayBlock;
}
POST(sys_tgkill)
{
   if (VG_(clo_trace_signals))
      VG_(message)(Vg_DebugMsg, "tgkill: sent signal %d to pid %d/%d",
                   ARG3, ARG1, ARG2);
}

/* ---------------------------------------------------------------------
   fadvise64* wrappers
   ------------------------------------------------------------------ */

PRE(sys_fadvise64)
{
   PRINT("sys_fadvise64 ( %d, %lld, %lu, %d )",
         ARG1, LOHI64(ARG2,ARG3), ARG4, ARG5);
   PRE_REG_READ5(long, "fadvise64",
                 int, fd, vki_u32, offset_low, vki_u32, offset_high,
                 vki_size_t, len, int, advice);
}

PRE(sys_fadvise64_64)
{
   PRINT("sys_fadvise64_64 ( %d, %lld, %lld, %d )",
         ARG1, LOHI64(ARG2,ARG3), LOHI64(ARG4,ARG5), ARG6);
   PRE_REG_READ6(long, "fadvise64_64",
                 int, fd, vki_u32, offset_low, vki_u32, offset_high,
                 vki_u32, len_low, vki_u32, len_high, int, advice);
}

/* ---------------------------------------------------------------------
   io_* wrappers
   ------------------------------------------------------------------ */

// Nb: this wrapper has to pad/unpad memory around the syscall itself,
// and this allows us to control exactly the code that gets run while
// the padding is in place.

PRE(sys_io_setup)
{
   PRINT("sys_io_setup ( %u, %p )", ARG1,ARG2);
   PRE_REG_READ2(long, "io_setup",
                 unsigned, nr_events, vki_aio_context_t *, ctxp);
   PRE_MEM_WRITE( "io_setup(ctxp)", ARG2, sizeof(vki_aio_context_t) );
}

POST(sys_io_setup)
{
   SizeT size;
   struct vki_aio_ring *r;
           
   size = VG_PGROUNDUP(sizeof(struct vki_aio_ring) +
                       ARG1*sizeof(struct vki_io_event));
   r = *(struct vki_aio_ring **)ARG2;
   vg_assert(ML_(valid_client_addr)((Addr)r, size, tid, "io_setup"));

   ML_(notify_aspacem_and_tool_of_mmap)( (Addr)r, size,
                                         VKI_PROT_READ | VKI_PROT_WRITE,
                                         VKI_MAP_ANONYMOUS, -1, 0 );

   POST_MEM_WRITE( ARG2, sizeof(vki_aio_context_t) );
}

// Nb: This wrapper is "Special" because we need 'size' to do the unmap
// after the syscall.  We must get 'size' from the aio_ring structure,
// before the syscall, while the aio_ring structure still exists.  (And we
// know that we must look at the aio_ring structure because Tom inspected the
// kernel and glibc sources to see what they do, yuk.)
//
// XXX This segment can be implicitly unmapped when aio
// file-descriptors are closed...
PRE(sys_io_destroy)
{
   struct vki_aio_ring *r;
   SizeT size;
      
   PRINT("sys_io_destroy ( %llu )", (ULong)ARG1);
   PRE_REG_READ1(long, "io_destroy", vki_aio_context_t, ctx);

   // If we are going to seg fault (due to a bogus ARG1) do it as late as
   // possible...
   r = (struct vki_aio_ring *)ARG1;
   size = VG_PGROUNDUP(sizeof(struct vki_aio_ring) + 
                       r->nr*sizeof(struct vki_io_event));

   SET_STATUS_from_SysRes( VG_(do_syscall1)(SYSNO, ARG1) );

   if (SUCCESS && RES == 0) { 
      Bool d = VG_(am_notify_munmap)( ARG1, size );
      VG_TRACK( die_mem_munmap, ARG1, size );
      if (d)
         VG_(discard_translations)( (Addr64)ARG1, (ULong)size, 
                                    "PRE(sys_io_destroy)" );
   }  
}  

PRE(sys_io_getevents)
{
   *flags |= SfMayBlock;
   PRINT("sys_io_getevents ( %llu, %lld, %lld, %p, %p )",
         (ULong)ARG1,(Long)ARG2,(Long)ARG3,ARG4,ARG5);
   PRE_REG_READ5(long, "io_getevents",
                 vki_aio_context_t, ctx_id, long, min_nr, long, nr,
                 struct io_event *, events,
                 struct timespec *, timeout);
   if (ARG3 > 0)
      PRE_MEM_WRITE( "io_getevents(events)",
                     ARG4, sizeof(struct vki_io_event)*ARG3 );
   if (ARG5 != 0)
      PRE_MEM_READ( "io_getevents(timeout)",
                    ARG5, sizeof(struct vki_timespec));
}
POST(sys_io_getevents)
{
   Int i;
   vg_assert(SUCCESS);
   if (RES > 0) {
      POST_MEM_WRITE( ARG4, sizeof(struct vki_io_event)*RES );
      for (i = 0; i < RES; i++) {
         const struct vki_io_event *vev = ((struct vki_io_event *)ARG4) + i;
         const struct vki_iocb *cb = (struct vki_iocb *)(Addr)vev->obj;

         switch (cb->aio_lio_opcode) {
         case VKI_IOCB_CMD_PREAD:
            if (vev->result > 0)
               POST_MEM_WRITE( cb->aio_buf, vev->result );
            break;
            
         case VKI_IOCB_CMD_PWRITE:
            break;
           
         default:
            VG_(message)(Vg_DebugMsg,
                        "Warning: unhandled io_getevents opcode: %u\n",
                        cb->aio_lio_opcode);
            break;
         }
      }
   }
}

PRE(sys_io_submit)
{
   Int i;

   PRINT("sys_io_submit ( %llu, %ld, %p )", (ULong)ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "io_submit",
                 vki_aio_context_t, ctx_id, long, nr,
                 struct iocb **, iocbpp);
   PRE_MEM_READ( "io_submit(iocbpp)", ARG3, ARG2*sizeof(struct vki_iocb *) );
   if (ARG3 != 0) {
      for (i = 0; i < ARG2; i++) {
         struct vki_iocb *cb = ((struct vki_iocb **)ARG3)[i];
         PRE_MEM_READ( "io_submit(iocb)", (Addr)cb, sizeof(struct vki_iocb) );
         switch (cb->aio_lio_opcode) {
         case VKI_IOCB_CMD_PREAD:
            PRE_MEM_WRITE( "io_submit(PREAD)", cb->aio_buf, cb->aio_nbytes );
            break;

         case VKI_IOCB_CMD_PWRITE:
            PRE_MEM_READ( "io_submit(PWRITE)", cb->aio_buf, cb->aio_nbytes );
            break;
           
         default:
            VG_(message)(Vg_DebugMsg,"Warning: unhandled io_submit opcode: %u\n",
                         cb->aio_lio_opcode);
            break;
         }
      }
   }
}

PRE(sys_io_cancel)
{
   PRINT("sys_io_cancel ( %llu, %p, %p )", (ULong)ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "io_cancel",
                 vki_aio_context_t, ctx_id, struct iocb *, iocb,
                 struct io_event *, result);
   PRE_MEM_READ( "io_cancel(iocb)", ARG2, sizeof(struct vki_iocb) );
   PRE_MEM_WRITE( "io_cancel(result)", ARG3, sizeof(struct vki_io_event) );
}
POST(sys_io_cancel)
{
   POST_MEM_WRITE( ARG3, sizeof(struct vki_io_event) );
}

/* ---------------------------------------------------------------------
   *_mempolicy wrappers
   ------------------------------------------------------------------ */

PRE(sys_mbind)
{
   PRINT("sys_mbind ( %p, %lu, %d, %p, %lu, %u )", ARG1,ARG2,ARG3,ARG4,ARG5,ARG6);
   PRE_REG_READ6(long, "mbind",
                 unsigned long, start, unsigned long, len,
                 unsigned long, policy, unsigned long *, nodemask,
                 unsigned long, maxnode, unsigned, flags);
   if (ARG1 != 0)
      PRE_MEM_READ( "mbind(nodemask)", ARG4,
                    VG_ROUNDUP( ARG5, sizeof(UWord) ) / sizeof(UWord) );
}

PRE(sys_set_mempolicy)
{
   PRINT("sys_set_mempolicy ( %d, %p, %d )", ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "set_mempolicy",
                 int, policy, unsigned long *, nodemask,
                 unsigned long, maxnode);
   PRE_MEM_READ( "set_mempolicy(nodemask)", ARG2,
                 VG_ROUNDUP( ARG3, sizeof(UWord) ) / sizeof(UWord) );
}

PRE(sys_get_mempolicy)
{
   PRINT("sys_get_mempolicy ( %p, %p, %d, %p, %x )", ARG1,ARG2,ARG3,ARG4,ARG5);
   PRE_REG_READ5(long, "get_mempolicy",
                 int *, policy, unsigned long *, nodemask,
                 unsigned long, maxnode, unsigned long, addr,
                 unsigned long, flags);
   if (ARG1 != 0)
      PRE_MEM_WRITE( "get_mempolicy(policy)", ARG1, sizeof(Int) );
   if (ARG2 != 0)
      PRE_MEM_WRITE( "get_mempolicy(nodemask)", ARG2,
                     VG_ROUNDUP( ARG3, sizeof(UWord) * 8 ) / sizeof(UWord) );
}
POST(sys_get_mempolicy)
{
   if (ARG1 != 0)
      POST_MEM_WRITE( ARG1, sizeof(Int) );
   if (ARG2 != 0)
      POST_MEM_WRITE( ARG2, VG_ROUNDUP( ARG3, sizeof(UWord) * 8 ) / sizeof(UWord) );
}

/* ---------------------------------------------------------------------
   inotify_* wrappers
   ------------------------------------------------------------------ */

PRE(sys_inotify_init)
{
   PRINT("sys_inotify_init ( )");
   PRE_REG_READ0(long, "inotify_init");
}
POST(sys_inotify_init)
{
   vg_assert(SUCCESS);
   if (!ML_(fd_allowed)(RES, "inotify_init", tid, True)) {
      VG_(close)(RES);
      SET_STATUS_Failure( VKI_EMFILE );
   } else {
      if (VG_(clo_track_fds))
         ML_(record_fd_open_nameless) (tid, RES);
   }
}

PRE(sys_inotify_add_watch)
{
   PRINT( "sys_inotify_add_watch ( %d, %p, %x )", ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "inotify_add_watch", int, fd, char *, path, int, mask);
   PRE_MEM_RASCIIZ( "inotify_add_watch(path)", ARG2 );
}

PRE(sys_inotify_rm_watch)
{
   PRINT( "sys_inotify_rm_watch ( %d, %x )", ARG1,ARG2);
   PRE_REG_READ2(long, "inotify_rm_watch", int, fd, int, wd);
}

/* ---------------------------------------------------------------------
   mq_* wrappers
   ------------------------------------------------------------------ */

PRE(sys_mq_open)
{
   PRINT("sys_mq_open( %p(%s), %d, %lld, %p )",
         ARG1,ARG1,ARG2,(ULong)ARG3,ARG4);
   PRE_REG_READ4(long, "mq_open",
                 const char *, name, int, oflag, vki_mode_t, mode,
                 struct mq_attr *, attr);
   PRE_MEM_RASCIIZ( "mq_open(name)", ARG1 );
   if ((ARG2 & VKI_O_CREAT) != 0 && ARG4 != 0) {
      const struct vki_mq_attr *attr = (struct vki_mq_attr *)ARG4;
      PRE_MEM_READ( "mq_open(attr->mq_maxmsg)",
                     (Addr)&attr->mq_maxmsg, sizeof(attr->mq_maxmsg) );
      PRE_MEM_READ( "mq_open(attr->mq_msgsize)",
                     (Addr)&attr->mq_msgsize, sizeof(attr->mq_msgsize) );
   }
}
POST(sys_mq_open)
{
   vg_assert(SUCCESS);
   if (!ML_(fd_allowed)(RES, "mq_open", tid, True)) {
      VG_(close)(RES);
      SET_STATUS_Failure( VKI_EMFILE );
   } else {
      if (VG_(clo_track_fds))
         ML_(record_fd_open_with_given_name)(tid, RES, (Char*)ARG1);
   }
}

PRE(sys_mq_unlink)
{
   PRINT("sys_mq_unlink ( %p(%s) )", ARG1,ARG1);
   PRE_REG_READ1(long, "mq_unlink", const char *, name);
   PRE_MEM_RASCIIZ( "mq_unlink(name)", ARG1 );
}

PRE(sys_mq_timedsend)
{
   *flags |= SfMayBlock;
   PRINT("sys_mq_timedsend ( %d, %p, %llu, %d, %p )",
         ARG1,ARG2,(ULong)ARG3,ARG4,ARG5);
   PRE_REG_READ5(long, "mq_timedsend",
                 vki_mqd_t, mqdes, const char *, msg_ptr, vki_size_t, msg_len,
                 unsigned int, msg_prio, const struct timespec *, abs_timeout);
   if (!ML_(fd_allowed)(ARG1, "mq_timedsend", tid, False)) {
      SET_STATUS_Failure( VKI_EBADF );
   } else {
      PRE_MEM_READ( "mq_timedsend(msg_ptr)", ARG2, ARG3 );
      if (ARG5 != 0)
         PRE_MEM_READ( "mq_timedsend(abs_timeout)", ARG5,
                        sizeof(struct vki_timespec) );
   }
}

PRE(sys_mq_timedreceive)
{
   *flags |= SfMayBlock;
   PRINT("sys_mq_timedreceive( %d, %p, %llu, %p, %p )",
         ARG1,ARG2,(ULong)ARG3,ARG4,ARG5);
   PRE_REG_READ5(ssize_t, "mq_timedreceive",
                 vki_mqd_t, mqdes, char *, msg_ptr, vki_size_t, msg_len,
                 unsigned int *, msg_prio,
                 const struct timespec *, abs_timeout);
   if (!ML_(fd_allowed)(ARG1, "mq_timedreceive", tid, False)) {
      SET_STATUS_Failure( VKI_EBADF );
   } else {
      PRE_MEM_WRITE( "mq_timedreceive(msg_ptr)", ARG2, ARG3 );
      if (ARG4 != 0)
         PRE_MEM_WRITE( "mq_timedreceive(msg_prio)",
                        ARG4, sizeof(unsigned int) );
      if (ARG5 != 0)
         PRE_MEM_READ( "mq_timedreceive(abs_timeout)",
                        ARG5, sizeof(struct vki_timespec) );
   }
}
POST(sys_mq_timedreceive)
{
   POST_MEM_WRITE( ARG2, ARG3 );
   if (ARG4 != 0)
      POST_MEM_WRITE( ARG4, sizeof(unsigned int) );
}

PRE(sys_mq_notify)
{
   PRINT("sys_mq_notify( %d, %p )", ARG1,ARG2 );
   PRE_REG_READ2(long, "mq_notify",
                 vki_mqd_t, mqdes, const struct sigevent *, notification);
   if (!ML_(fd_allowed)(ARG1, "mq_notify", tid, False))
      SET_STATUS_Failure( VKI_EBADF );
   else if (ARG2 != 0)
      PRE_MEM_READ( "mq_notify(notification)",
                    ARG2, sizeof(struct vki_sigevent) );
}

PRE(sys_mq_getsetattr)
{
   PRINT("sys_mq_getsetattr( %d, %p, %p )", ARG1,ARG2,ARG3 );
   PRE_REG_READ3(long, "mq_getsetattr",
                 vki_mqd_t, mqdes, const struct mq_attr *, mqstat,
                 struct mq_attr *, omqstat);
   if (!ML_(fd_allowed)(ARG1, "mq_getsetattr", tid, False)) {
      SET_STATUS_Failure( VKI_EBADF );
   } else {
      if (ARG2 != 0) {
         const struct vki_mq_attr *attr = (struct vki_mq_attr *)ARG2;
         PRE_MEM_READ( "mq_getsetattr(mqstat->mq_flags)",
                        (Addr)&attr->mq_flags, sizeof(attr->mq_flags) );
      }
      if (ARG3 != 0)
         PRE_MEM_WRITE( "mq_getsetattr(omqstat)", ARG3,
                        sizeof(struct vki_mq_attr) );
   }   
}
POST(sys_mq_getsetattr)
{
   if (ARG3 != 0)
      POST_MEM_WRITE( ARG3, sizeof(struct vki_mq_attr) );
}

/* ---------------------------------------------------------------------
   clock_* wrappers
   ------------------------------------------------------------------ */

PRE(sys_clock_settime)
{
   PRINT("sys_clock_settime( %d, %p )", ARG1,ARG2);
   PRE_REG_READ2(long, "clock_settime", 
                 vki_clockid_t, clk_id, const struct timespec *, tp);
   PRE_MEM_READ( "clock_settime(tp)", ARG2, sizeof(struct vki_timespec) );
}

PRE(sys_clock_gettime)
{
   PRINT("sys_clock_gettime( %d, %p )" , ARG1,ARG2);
   PRE_REG_READ2(long, "clock_gettime", 
                 vki_clockid_t, clk_id, struct timespec *, tp);
   PRE_MEM_WRITE( "clock_gettime(tp)", ARG2, sizeof(struct vki_timespec) );
}
POST(sys_clock_gettime)
{
   POST_MEM_WRITE( ARG2, sizeof(struct vki_timespec) );
}

PRE(sys_clock_getres)
{
   PRINT("sys_clock_getres( %d, %p )" , ARG1,ARG2);
   // Nb: we can't use "RES" as the param name because that's a macro
   // defined above!
   PRE_REG_READ2(long, "clock_getres", 
                 vki_clockid_t, clk_id, struct timespec *, res);
   if (ARG2 != 0)
      PRE_MEM_WRITE( "clock_getres(res)", ARG2, sizeof(struct vki_timespec) );
}
POST(sys_clock_getres)
{
   if (ARG2 != 0)
      POST_MEM_WRITE( ARG2, sizeof(struct vki_timespec) );
}

PRE(sys_clock_nanosleep)
{
   *flags |= SfMayBlock|SfPostOnFail;
   PRINT("sys_clock_nanosleep( %d, %d, %p, %p )", ARG1,ARG2,ARG3,ARG4);
   PRE_REG_READ4(int32_t, "clock_nanosleep",
                 vki_clockid_t, clkid, int, flags,
                 const struct timespec *, rqtp, struct timespec *, rmtp);
   PRE_MEM_READ( "clock_nanosleep(rqtp)", ARG3, sizeof(struct vki_timespec) );
   if (ARG4 != 0)
      PRE_MEM_WRITE( "clock_nanosleep(rmtp)", ARG4, sizeof(struct vki_timespec) );
}
POST(sys_clock_nanosleep)
{
   if (ARG4 != 0 && FAILURE && ERR == VKI_EINTR)
      POST_MEM_WRITE( ARG4, sizeof(struct vki_timespec) );
}

/* ---------------------------------------------------------------------
   timer_* wrappers
   ------------------------------------------------------------------ */

PRE(sys_timer_create)
{
   PRINT("sys_timer_create( %d, %p, %p )", ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "timer_create",
                 vki_clockid_t, clockid, struct sigevent *, evp,
                 vki_timer_t *, timerid);
   if (ARG2 != 0)
      PRE_MEM_READ( "timer_create(evp)", ARG2, sizeof(struct vki_sigevent) );
   PRE_MEM_WRITE( "timer_create(timerid)", ARG3, sizeof(vki_timer_t) );
}
POST(sys_timer_create)
{
   POST_MEM_WRITE( ARG3, sizeof(vki_timer_t) );
}

PRE(sys_timer_settime)
{
   PRINT("sys_timer_settime( %lld, %d, %p, %p )", (ULong)ARG1,ARG2,ARG3,ARG4);
   PRE_REG_READ4(long, "timer_settime", 
                 vki_timer_t, timerid, int, flags,
                 const struct itimerspec *, value,
                 struct itimerspec *, ovalue);
   PRE_MEM_READ( "timer_settime(value)", ARG3,
                  sizeof(struct vki_itimerspec) );
   if (ARG4 != 0)
       PRE_MEM_WRITE( "timer_settime(ovalue)", ARG4,
                      sizeof(struct vki_itimerspec) );
}
POST(sys_timer_settime)
{
   if (ARG4 != 0)
      POST_MEM_WRITE( ARG4, sizeof(struct vki_itimerspec) );
}

PRE(sys_timer_gettime)
{
   PRINT("sys_timer_gettime( %lld, %p )", (ULong)ARG1,ARG2);
   PRE_REG_READ2(long, "timer_gettime", 
                 vki_timer_t, timerid, struct itimerspec *, value);
   PRE_MEM_WRITE( "timer_gettime(value)", ARG2,
                  sizeof(struct vki_itimerspec));
}
POST(sys_timer_gettime)
{
   POST_MEM_WRITE( ARG2, sizeof(struct vki_itimerspec) );
}

PRE(sys_timer_getoverrun)
{
   PRINT("sys_timer_getoverrun( %p )", ARG1);
   PRE_REG_READ1(long, "timer_getoverrun", vki_timer_t, timerid);
}

PRE(sys_timer_delete)
{
   PRINT("sys_timer_delete( %p )", ARG1);
   PRE_REG_READ1(long, "timer_delete", vki_timer_t, timerid);
}

/* ---------------------------------------------------------------------
   capabilities wrappers
   ------------------------------------------------------------------ */

PRE(sys_capget)
{
   PRINT("sys_capget ( %p, %p )", ARG1, ARG2 );
   PRE_REG_READ2(long, "capget", 
                 vki_cap_user_header_t, header, vki_cap_user_data_t, data);
   PRE_MEM_READ( "capget(header)", ARG1, 
                  sizeof(struct __vki_user_cap_header_struct) );
   PRE_MEM_WRITE( "capget(data)", ARG2, 
                  sizeof(struct __vki_user_cap_data_struct) );
}
POST(sys_capget)
{
   if (ARG2 != (Addr)NULL)
      POST_MEM_WRITE( ARG2, sizeof(struct __vki_user_cap_data_struct) );
}

PRE(sys_capset)
{
   PRINT("sys_capset ( %p, %p )", ARG1, ARG2 );
   PRE_REG_READ2(long, "capset", 
                 vki_cap_user_header_t, header,
                 const vki_cap_user_data_t, data);
   PRE_MEM_READ( "capset(header)", 
                  ARG1, sizeof(struct __vki_user_cap_header_struct) );
   PRE_MEM_READ( "capset(data)", 
                  ARG2, sizeof(struct __vki_user_cap_data_struct) );
}

/* ---------------------------------------------------------------------
   16-bit uid/gid/groups wrappers
   ------------------------------------------------------------------ */

PRE(sys_getuid16)
{
   PRINT("sys_getuid16 ( )");
   PRE_REG_READ0(long, "getuid16");
}

PRE(sys_setuid16)
{
   PRINT("sys_setuid16 ( %d )", ARG1);
   PRE_REG_READ1(long, "setuid16", vki_old_uid_t, uid);
}

PRE(sys_getgid16)
{
   PRINT("sys_getgid16 ( )");
   PRE_REG_READ0(long, "getgid16");
}

PRE(sys_setgid16)
{
   PRINT("sys_setgid16 ( %d )", ARG1);
   PRE_REG_READ1(long, "setgid16", vki_old_gid_t, gid);
}

PRE(sys_geteuid16)
{
   PRINT("sys_geteuid16 ( )");
   PRE_REG_READ0(long, "geteuid16");
}

PRE(sys_getegid16)
{
   PRINT("sys_getegid16 ( )");
   PRE_REG_READ0(long, "getegid16");
}

PRE(sys_setreuid16)
{
   PRINT("setreuid16 ( 0x%x, 0x%x )", ARG1, ARG2);
   PRE_REG_READ2(long, "setreuid16", vki_old_uid_t, ruid, vki_old_uid_t, euid);
}

PRE(sys_setregid16)
{
   PRINT("sys_setregid16 ( %d, %d )", ARG1, ARG2);
   PRE_REG_READ2(long, "setregid16", vki_old_gid_t, rgid, vki_old_gid_t, egid);
}

PRE(sys_getgroups16)
{
   PRINT("sys_getgroups16 ( %d, %p )", ARG1, ARG2);
   PRE_REG_READ2(long, "getgroups16", int, size, vki_old_gid_t *, list);
   if (ARG1 > 0)
      PRE_MEM_WRITE( "getgroups16(list)", ARG2, ARG1 * sizeof(vki_old_gid_t) );
}
POST(sys_getgroups16)
{
   vg_assert(SUCCESS);
   if (ARG1 > 0 && RES > 0)
      POST_MEM_WRITE( ARG2, RES * sizeof(vki_old_gid_t) );
}

PRE(sys_setgroups16)
{
   PRINT("sys_setgroups16 ( %llu, %p )", (ULong)ARG1, ARG2);
   PRE_REG_READ2(long, "setgroups16", int, size, vki_old_gid_t *, list);
   if (ARG1 > 0)
      PRE_MEM_READ( "setgroups16(list)", ARG2, ARG1 * sizeof(vki_old_gid_t) );
}

/* ---------------------------------------------------------------------
   *chown16 wrappers
   ------------------------------------------------------------------ */

PRE(sys_chown16)
{
   PRINT("sys_chown16 ( %p, 0x%x, 0x%x )", ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "chown16",
                 const char *, path,
                 vki_old_uid_t, owner, vki_old_gid_t, group);
   PRE_MEM_RASCIIZ( "chown16(path)", ARG1 );
}

PRE(sys_fchown16)
{
   PRINT("sys_fchown16 ( %d, %d, %d )", ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "fchown16",
                 unsigned int, fd, vki_old_uid_t, owner, vki_old_gid_t, group);
}

/* ---------------------------------------------------------------------
   *xattr wrappers
   ------------------------------------------------------------------ */

PRE(sys_setxattr)
{
   *flags |= SfMayBlock;
   PRINT("sys_setxattr ( %p, %p, %p, %llu, %d )",
         ARG1, ARG2, ARG3, (ULong)ARG4, ARG5);
   PRE_REG_READ5(long, "setxattr",
                 char *, path, char *, name,
                 void *, value, vki_size_t, size, int, flags);
   PRE_MEM_RASCIIZ( "setxattr(path)", ARG1 );
   PRE_MEM_RASCIIZ( "setxattr(name)", ARG2 );
   PRE_MEM_READ( "setxattr(value)", ARG3, ARG4 );
}

PRE(sys_lsetxattr)
{
   *flags |= SfMayBlock;
   PRINT("sys_lsetxattr ( %p, %p, %p, %llu, %d )",
         ARG1, ARG2, ARG3, (ULong)ARG4, ARG5);
   PRE_REG_READ5(long, "lsetxattr",
                 char *, path, char *, name,
                 void *, value, vki_size_t, size, int, flags);
   PRE_MEM_RASCIIZ( "lsetxattr(path)", ARG1 );
   PRE_MEM_RASCIIZ( "lsetxattr(name)", ARG2 );
   PRE_MEM_READ( "lsetxattr(value)", ARG3, ARG4 );
}

PRE(sys_fsetxattr)
{
   *flags |= SfMayBlock;
   PRINT("sys_fsetxattr ( %d, %p, %p, %llu, %d )",
         ARG1, ARG2, ARG3, (ULong)ARG4, ARG5);
   PRE_REG_READ5(long, "fsetxattr",
                 int, fd, char *, name, void *, value,
                 vki_size_t, size, int, flags);
   PRE_MEM_RASCIIZ( "fsetxattr(name)", ARG2 );
   PRE_MEM_READ( "fsetxattr(value)", ARG3, ARG4 );
}

PRE(sys_getxattr)
{
   *flags |= SfMayBlock;
   PRINT("sys_getxattr ( %p, %p, %p, %llu )", ARG1,ARG2,ARG3, (ULong)ARG4);
   PRE_REG_READ4(ssize_t, "getxattr",
                 char *, path, char *, name, void *, value, vki_size_t, size);
   PRE_MEM_RASCIIZ( "getxattr(path)", ARG1 );
   PRE_MEM_RASCIIZ( "getxattr(name)", ARG2 );
   PRE_MEM_WRITE( "getxattr(value)", ARG3, ARG4 );
}
POST(sys_getxattr)
{
   vg_assert(SUCCESS);
   if (RES > 0 && ARG3 != (Addr)NULL) {
      POST_MEM_WRITE( ARG3, RES );
   }
}

PRE(sys_lgetxattr)
{
   *flags |= SfMayBlock;
   PRINT("sys_lgetxattr ( %p, %p, %p, %llu )", ARG1,ARG2,ARG3, (ULong)ARG4);
   PRE_REG_READ4(ssize_t, "lgetxattr",
                 char *, path, char *, name, void *, value, vki_size_t, size);
   PRE_MEM_RASCIIZ( "lgetxattr(path)", ARG1 );
   PRE_MEM_RASCIIZ( "lgetxattr(name)", ARG2 );
   PRE_MEM_WRITE( "lgetxattr(value)", ARG3, ARG4 );
}
POST(sys_lgetxattr)
{
   vg_assert(SUCCESS);
   if (RES > 0 && ARG3 != (Addr)NULL) {
      POST_MEM_WRITE( ARG3, RES );
   }
}

PRE(sys_fgetxattr)
{
   *flags |= SfMayBlock;
   PRINT("sys_fgetxattr ( %d, %p, %p, %llu )", ARG1, ARG2, ARG3, (ULong)ARG4);
   PRE_REG_READ4(ssize_t, "fgetxattr",
                 int, fd, char *, name, void *, value, vki_size_t, size);
   PRE_MEM_RASCIIZ( "fgetxattr(name)", ARG2 );
   PRE_MEM_WRITE( "fgetxattr(value)", ARG3, ARG4 );
}
POST(sys_fgetxattr)
{
   if (RES > 0 && ARG3 != (Addr)NULL)
      POST_MEM_WRITE( ARG3, RES );
}

PRE(sys_listxattr)
{
   *flags |= SfMayBlock;
   PRINT("sys_listxattr ( %p, %p, %llu )", ARG1, ARG2, (ULong)ARG3);
   PRE_REG_READ3(ssize_t, "listxattr",
                 char *, path, char *, list, vki_size_t, size);
   PRE_MEM_RASCIIZ( "listxattr(path)", ARG1 );
   PRE_MEM_WRITE( "listxattr(list)", ARG2, ARG3 );
}
POST(sys_listxattr)
{
   if (RES > 0 && ARG2 != (Addr)NULL)
      POST_MEM_WRITE( ARG2, RES );
}

PRE(sys_llistxattr)
{
   *flags |= SfMayBlock;
   PRINT("sys_llistxattr ( %p, %p, %llu )", ARG1, ARG2, (ULong)ARG3);
   PRE_REG_READ3(ssize_t, "llistxattr",
                 char *, path, char *, list, vki_size_t, size);
   PRE_MEM_RASCIIZ( "llistxattr(path)", ARG1 );
   PRE_MEM_WRITE( "llistxattr(list)", ARG2, ARG3 );
}
POST(sys_llistxattr)
{
   if (RES > 0 && ARG2 != (Addr)NULL)
      POST_MEM_WRITE( ARG2, RES );
}

PRE(sys_flistxattr)
{
   *flags |= SfMayBlock;
   PRINT("sys_flistxattr ( %d, %p, %llu )", ARG1, ARG2, (ULong)ARG3);
   PRE_REG_READ3(ssize_t, "flistxattr",
                 int, fd, char *, list, vki_size_t, size);
   PRE_MEM_WRITE( "flistxattr(list)", ARG2, ARG3 );
}
POST(sys_flistxattr)
{
   if (RES > 0 && ARG2 != (Addr)NULL)
      POST_MEM_WRITE( ARG2, RES );
}

PRE(sys_removexattr)
{
   *flags |= SfMayBlock;
   PRINT("sys_removexattr ( %p, %p )", ARG1, ARG2);
   PRE_REG_READ2(long, "removexattr", char *, path, char *, name);
   PRE_MEM_RASCIIZ( "removexattr(path)", ARG1 );
   PRE_MEM_RASCIIZ( "removexattr(name)", ARG2 );
}

PRE(sys_lremovexattr)
{
   *flags |= SfMayBlock;
   PRINT("sys_lremovexattr ( %p, %p )", ARG1, ARG2);
   PRE_REG_READ2(long, "lremovexattr", char *, path, char *, name);
   PRE_MEM_RASCIIZ( "lremovexattr(path)", ARG1 );
   PRE_MEM_RASCIIZ( "lremovexattr(name)", ARG2 );
}

PRE(sys_fremovexattr)
{
   *flags |= SfMayBlock;
   PRINT("sys_fremovexattr ( %d, %p )", ARG1, ARG2);
   PRE_REG_READ2(long, "fremovexattr", int, fd, char *, name);
   PRE_MEM_RASCIIZ( "fremovexattr(name)", ARG2 );
}

/* ---------------------------------------------------------------------
   sched_* wrappers
   ------------------------------------------------------------------ */

PRE(sys_sched_setparam)
{
   PRINT("sched_setparam ( %d, %p )", ARG1, ARG2 );
   PRE_REG_READ2(long, "sched_setparam", 
                 vki_pid_t, pid, struct sched_param *, p);
   PRE_MEM_READ( "sched_setparam(p)", ARG2, sizeof(struct vki_sched_param) );
}
POST(sys_sched_setparam)
{
   POST_MEM_WRITE( ARG2, sizeof(struct vki_sched_param) );
}

PRE(sys_sched_getparam)
{
   PRINT("sched_getparam ( %d, %p )", ARG1, ARG2 );
   PRE_REG_READ2(long, "sched_getparam", 
                 vki_pid_t, pid, struct sched_param *, p);
   PRE_MEM_WRITE( "sched_getparam(p)", ARG2, sizeof(struct vki_sched_param) );
}
POST(sys_sched_getparam)
{
   POST_MEM_WRITE( ARG2, sizeof(struct vki_sched_param) );
}

PRE(sys_sched_getscheduler)
{
   PRINT("sys_sched_getscheduler ( %d )", ARG1);
   PRE_REG_READ1(long, "sched_getscheduler", vki_pid_t, pid);
}

PRE(sys_sched_setscheduler)
{
   PRINT("sys_sched_setscheduler ( %d, %d, %p )", ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "sched_setscheduler", 
                 vki_pid_t, pid, int, policy, struct sched_param *, p);
   if (ARG3 != 0)
      PRE_MEM_READ( "sched_setscheduler(p)", 
		    ARG3, sizeof(struct vki_sched_param));
}

PRE(sys_sched_yield)
{
   *flags |= SfMayBlock;
   PRINT("sched_yield()");
   PRE_REG_READ0(long, "sys_sched_yield");
}

PRE(sys_sched_get_priority_max)
{
   PRINT("sched_get_priority_max ( %d )", ARG1);
   PRE_REG_READ1(long, "sched_get_priority_max", int, policy);
}

PRE(sys_sched_get_priority_min)
{
   PRINT("sched_get_priority_min ( %d )", ARG1);
   PRE_REG_READ1(long, "sched_get_priority_min", int, policy);
}

PRE(sys_sched_setaffinity)
{
   PRINT("sched_setaffinity ( %d, %d, %p )", ARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "sched_setaffinity", 
                 vki_pid_t, pid, unsigned int, len, unsigned long *, mask);
   PRE_MEM_READ( "sched_setaffinity(mask)", ARG3, ARG2);
}

PRE(sys_sched_getaffinity)
{
   PRINT("sched_getaffinity ( %d, %d, %p )", ARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "sched_getaffinity", 
                 vki_pid_t, pid, unsigned int, len, unsigned long *, mask);
   PRE_MEM_WRITE( "sched_getaffinity(mask)", ARG3, ARG2);
}
POST(sys_sched_getaffinity)
{
   POST_MEM_WRITE(ARG3, ARG2);
}

/* ---------------------------------------------------------------------
   miscellaneous wrappers
   ------------------------------------------------------------------ */

PRE(sys_munlockall)
{
   *flags |= SfMayBlock;
   PRINT("sys_munlockall ( )");
   PRE_REG_READ0(long, "munlockall");
}

// This has different signatures for different platforms.
//
//  x86:   int  sys_pipe(unsigned long __user *fildes);
//  AMD64: long sys_pipe(int *fildes);
//  ppc32: int  sys_pipe(int __user *fildes);
//  ppc64: int  sys_pipe(int __user *fildes);
//
// The type of the argument is most important, and it is an array of 32 bit
// values in all cases.  (The return type differs across platforms, but it
// is not used.)  So we use 'int' as its type.  This fixed bug #113230 which
// was caused by using an array of 'unsigned long's, which didn't work on
// AMD64.
PRE(sys_pipe)
{
   PRINT("sys_pipe ( %p )", ARG1);
   PRE_REG_READ1(int, "pipe", int *, filedes);
   PRE_MEM_WRITE( "pipe(filedes)", ARG1, 2*sizeof(int) );
}
POST(sys_pipe)
{
   Int *p = (Int *)ARG1;

   if (!ML_(fd_allowed)(p[0], "pipe", tid, True) ||
       !ML_(fd_allowed)(p[1], "pipe", tid, True)) {
      VG_(close)(p[0]);
      VG_(close)(p[1]);
      SET_STATUS_Failure( VKI_EMFILE );
   } else {
      POST_MEM_WRITE( ARG1, 2*sizeof(int) );
      if (VG_(clo_track_fds)) {
         ML_(record_fd_open_nameless)(tid, p[0]);
         ML_(record_fd_open_nameless)(tid, p[1]);
      }
   }
}

PRE(sys_quotactl)
{
   PRINT("sys_quotactl (0x%x, %p, 0x%x, 0x%x )", ARG1,ARG2,ARG3, ARG4);
   PRE_REG_READ4(long, "quotactl",
                 unsigned int, cmd, const char *, special, vki_qid_t, id,
                 void *, addr);
   PRE_MEM_RASCIIZ( "quotactl(special)", ARG2 );
}

PRE(sys_waitid)
{
   *flags |= SfMayBlock;
   PRINT("sys_waitid( %d, %d, %p, %d, %p )", ARG1,ARG2,ARG3,ARG4,ARG5);
   PRE_REG_READ5(int32_t, "sys_waitid",
                 int, which, vki_pid_t, pid, struct vki_siginfo *, infop,
                 int, options, struct vki_rusage *, ru);
   PRE_MEM_WRITE( "waitid(infop)", ARG3, sizeof(struct vki_siginfo) );
   if (ARG5 != 0)
      PRE_MEM_WRITE( "waitid(ru)", ARG5, sizeof(struct vki_rusage) );
}
POST(sys_waitid)
{
   POST_MEM_WRITE( ARG3, sizeof(struct vki_siginfo) );
   if (ARG5 != 0)
      POST_MEM_WRITE( ARG5, sizeof(struct vki_rusage) );
}

/* ---------------------------------------------------------------------
   utime wrapper
   ------------------------------------------------------------------ */

PRE(sys_utime)
{
   *flags |= SfMayBlock;
   PRINT("sys_utime ( %p, %p )", ARG1,ARG2);
   PRE_REG_READ2(long, "utime", char *, filename, struct utimbuf *, buf);
   PRE_MEM_RASCIIZ( "utime(filename)", ARG1 );
   if (ARG2 != 0)
      PRE_MEM_READ( "utime(buf)", ARG2, sizeof(struct vki_utimbuf) );
}

/* ---------------------------------------------------------------------
   lseek wrapper
   ------------------------------------------------------------------ */

PRE(sys_lseek)
{
   PRINT("sys_lseek ( %d, %d, %d )", ARG1,ARG2,ARG3);
   PRE_REG_READ3(vki_off_t, "lseek",
                 unsigned int, fd, vki_off_t, offset, unsigned int, whence);
}

/* ---------------------------------------------------------------------
   sig* wrappers
   ------------------------------------------------------------------ */

PRE(sys_sigpending)
{
   PRINT( "sys_sigpending ( %p )", ARG1 );
   PRE_REG_READ1(long, "sigpending", vki_old_sigset_t *, set);
   PRE_MEM_WRITE( "sigpending(set)", ARG1, sizeof(vki_old_sigset_t));
}
POST(sys_sigpending)
{
   POST_MEM_WRITE( ARG1, sizeof(vki_old_sigset_t) ) ;
}

// This syscall is not used on amd64/Linux -- it only provides
// sys_rt_sigprocmask, which uses sigset_t rather than old_sigset_t.
// This wrapper is only suitable for 32-bit architectures.
// (XXX: so how is it that PRE(sys_sigpending) above doesn't need
// conditional compilation like this?)
#if defined(VGP_x86_linux) || defined(VGP_ppc32_linux)
PRE(sys_sigprocmask)
{
   vki_old_sigset_t* set;
   vki_old_sigset_t* oldset;
   vki_sigset_t bigger_set;
   vki_sigset_t bigger_oldset;

   PRINT("sys_sigprocmask ( %d, %p, %p )",ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "sigprocmask", 
                 int, how, vki_old_sigset_t *, set, vki_old_sigset_t *, oldset);
   if (ARG2 != 0)
      PRE_MEM_READ( "sigprocmask(set)", ARG2, sizeof(vki_old_sigset_t));
   if (ARG3 != 0)
      PRE_MEM_WRITE( "sigprocmask(oldset)", ARG3, sizeof(vki_old_sigset_t));

   // Nb: We must convert the smaller vki_old_sigset_t params into bigger
   // vki_sigset_t params.
   set    = (vki_old_sigset_t*)ARG2;
   oldset = (vki_old_sigset_t*)ARG3;

   VG_(memset)(&bigger_set,    0, sizeof(vki_sigset_t));
   VG_(memset)(&bigger_oldset, 0, sizeof(vki_sigset_t));
   if (set)
      bigger_set.sig[0] = *(vki_old_sigset_t*)set;

   SET_STATUS_from_SysRes(
      VG_(do_sys_sigprocmask) ( tid, ARG1 /*how*/, 
                                set ? &bigger_set    : NULL,
                             oldset ? &bigger_oldset : NULL)
   );

   if (oldset)
      *oldset = bigger_oldset.sig[0];

   if (SUCCESS)
      *flags |= SfPollAfter;
}
POST(sys_sigprocmask)
{
   vg_assert(SUCCESS);
   if (RES == 0 && ARG3 != 0)
      POST_MEM_WRITE( ARG3, sizeof(vki_old_sigset_t));
}
#endif

/* ---------------------------------------------------------------------
   rt_sig* wrappers
   ------------------------------------------------------------------ */

PRE(sys_rt_sigaction)
{
   PRINT("sys_rt_sigaction ( %d, %p, %p, %d )", ARG1,ARG2,ARG3,ARG4);
   PRE_REG_READ4(long, "rt_sigaction",
                 int, signum, const struct sigaction *, act,
                 struct sigaction *, oldact, vki_size_t, sigsetsize);

   if (ARG2 != 0) {
      struct vki_sigaction *sa = (struct vki_sigaction *)ARG2;
      PRE_MEM_READ( "rt_sigaction(act->sa_handler)", (Addr)&sa->ksa_handler, sizeof(sa->ksa_handler));
      PRE_MEM_READ( "rt_sigaction(act->sa_mask)", (Addr)&sa->sa_mask, sizeof(sa->sa_mask));
      PRE_MEM_READ( "rt_sigaction(act->sa_flags)", (Addr)&sa->sa_flags, sizeof(sa->sa_flags));
      if (sa->sa_flags & VKI_SA_RESTORER)
         PRE_MEM_READ( "rt_sigaction(act->sa_restorer)", (Addr)&sa->sa_restorer, sizeof(sa->sa_restorer));
   }
   if (ARG3 != 0)
      PRE_MEM_WRITE( "rt_sigaction(oldact)", ARG3, sizeof(struct vki_sigaction));

   // XXX: doesn't seem right to be calling do_sys_sigaction for
   // sys_rt_sigaction... perhaps this function should be renamed
   // VG_(do_sys_rt_sigaction)()  --njn

   SET_STATUS_from_SysRes(
      VG_(do_sys_sigaction)(ARG1, (const struct vki_sigaction *)ARG2,
                            (struct vki_sigaction *)ARG3)
   );
}
POST(sys_rt_sigaction)
{
   vg_assert(SUCCESS);
   if (RES == 0 && ARG3 != 0)
      POST_MEM_WRITE( ARG3, sizeof(struct vki_sigaction));
}

PRE(sys_rt_sigprocmask)
{
   PRINT("sys_rt_sigprocmask ( %d, %p, %p, %llu )",ARG1,ARG2,ARG3,(ULong)ARG4);
   PRE_REG_READ4(long, "rt_sigprocmask", 
                 int, how, vki_sigset_t *, set, vki_sigset_t *, oldset,
                 vki_size_t, sigsetsize);
   if (ARG2 != 0)
      PRE_MEM_READ( "rt_sigprocmask(set)", ARG2, sizeof(vki_sigset_t));
   if (ARG3 != 0)
      PRE_MEM_WRITE( "rt_sigprocmask(oldset)", ARG3, sizeof(vki_sigset_t));

   // Like the kernel, we fail if the sigsetsize is not exactly what we expect.
   if (sizeof(vki_sigset_t) != ARG4)
      SET_STATUS_Failure( VKI_EMFILE );
   else {
      SET_STATUS_from_SysRes( 
                  VG_(do_sys_sigprocmask) ( tid, ARG1 /*how*/, 
                                            (vki_sigset_t*) ARG2,
                                            (vki_sigset_t*) ARG3 )
      );
   }

   if (SUCCESS)
      *flags |= SfPollAfter;
}
POST(sys_rt_sigprocmask)
{
   vg_assert(SUCCESS);
   if (RES == 0 && ARG3 != 0)
      POST_MEM_WRITE( ARG3, sizeof(vki_sigset_t));
}

PRE(sys_rt_sigpending)
{
   PRINT( "sys_rt_sigpending ( %p )", ARG1 );
   PRE_REG_READ2(long, "rt_sigpending", 
                 vki_sigset_t *, set, vki_size_t, sigsetsize);
   PRE_MEM_WRITE( "rt_sigpending(set)", ARG1, sizeof(vki_sigset_t));
}
POST(sys_rt_sigpending)
{
   POST_MEM_WRITE( ARG1, sizeof(vki_sigset_t) ) ;
}

PRE(sys_rt_sigtimedwait)
{
   *flags |= SfMayBlock;
   PRINT("sys_rt_sigtimedwait ( %p, %p, %p, %lld )",
         ARG1,ARG2,ARG3,(ULong)ARG4);
   PRE_REG_READ4(long, "rt_sigtimedwait", 
                 const vki_sigset_t *, set, vki_siginfo_t *, info,
                 const struct timespec *, timeout, vki_size_t, sigsetsize);
   if (ARG1 != 0) 
      PRE_MEM_READ(  "rt_sigtimedwait(set)",  ARG1, sizeof(vki_sigset_t));
   if (ARG2 != 0)
      PRE_MEM_WRITE( "rt_sigtimedwait(info)", ARG2, sizeof(vki_siginfo_t) );
   if (ARG3 != 0)
      PRE_MEM_READ( "rt_sigtimedwait(timeout)",
                    ARG3, sizeof(struct vki_timespec) );
}
POST(sys_rt_sigtimedwait)
{
   if (ARG2 != 0)
      POST_MEM_WRITE( ARG2, sizeof(vki_siginfo_t) );
}

PRE(sys_rt_sigqueueinfo)
{
   PRINT("sys_rt_sigqueueinfo(%d, %d, %p)", ARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "rt_sigqueueinfo", 
                 int, pid, int, sig, vki_siginfo_t *, uinfo);
   if (ARG2 != 0)
      PRE_MEM_READ( "rt_sigqueueinfo(uinfo)", ARG3, sizeof(vki_siginfo_t) );
}
POST(sys_rt_sigqueueinfo)
{
   if (!ML_(client_signal_OK)(ARG2))
      SET_STATUS_Failure( VKI_EINVAL );
}

// XXX: x86-specific?  The kernel prototypes for the different archs are
//      hard to decipher.
PRE(sys_rt_sigsuspend)
{
   /* The C library interface to sigsuspend just takes a pointer to
      a signal mask but this system call has two arguments - a pointer
      to the mask and the number of bytes used by it. The kernel insists
      on the size being equal to sizeof(sigset_t) however and will just
      return EINVAL if it isn't.
    */
   *flags |= SfMayBlock;
   PRINT("sys_rt_sigsuspend ( %p, %d )", ARG1,ARG2 );
   PRE_REG_READ2(int, "rt_sigsuspend", vki_sigset_t *, mask, vki_size_t, size)
   if (ARG1 != (Addr)NULL) {
      PRE_MEM_READ( "rt_sigsuspend(mask)", ARG1, sizeof(vki_sigset_t) );
   }
}

/* ---------------------------------------------------------------------
   linux msg* wrapper helpers
   ------------------------------------------------------------------ */

void
ML_(linux_PRE_sys_msgsnd) ( ThreadId tid,
                            UWord arg0, UWord arg1, UWord arg2, UWord arg3 )
{
   /* int msgsnd(int msqid, struct msgbuf *msgp, size_t msgsz, int msgflg); */
   struct vki_msgbuf *msgp = (struct vki_msgbuf *)arg1;
   PRE_MEM_READ( "msgsnd(msgp->mtype)", (Addr)&msgp->mtype, sizeof(msgp->mtype) );
   PRE_MEM_READ( "msgsnd(msgp->mtext)", (Addr)&msgp->mtext, arg2 );
}

void
ML_(linux_PRE_sys_msgrcv) ( ThreadId tid,
                            UWord arg0, UWord arg1, UWord arg2,
                            UWord arg3, UWord arg4 )
{
   /* ssize_t msgrcv(int msqid, struct msgbuf *msgp, size_t msgsz,
                     long msgtyp, int msgflg); */
   struct vki_msgbuf *msgp = (struct vki_msgbuf *)arg1;
   PRE_MEM_WRITE( "msgrcv(msgp->mtype)", (Addr)&msgp->mtype, sizeof(msgp->mtype) );
   PRE_MEM_WRITE( "msgrcv(msgp->mtext)", (Addr)&msgp->mtext, arg2 );
}
void
ML_(linux_POST_sys_msgrcv) ( ThreadId tid,
                             UWord res,
                             UWord arg0, UWord arg1, UWord arg2,
                             UWord arg3, UWord arg4 )
{
   struct vki_msgbuf *msgp = (struct vki_msgbuf *)arg1;
   POST_MEM_WRITE( (Addr)&msgp->mtype, sizeof(msgp->mtype) );
   POST_MEM_WRITE( (Addr)&msgp->mtext, res );
}

void
ML_(linux_PRE_sys_msgctl) ( ThreadId tid,
                            UWord arg0, UWord arg1, UWord arg2 )
{
   /* int msgctl(int msqid, int cmd, struct msqid_ds *buf); */
   switch (arg1 /* cmd */) {
   case VKI_IPC_INFO:
   case VKI_MSG_INFO:
   case VKI_IPC_INFO|VKI_IPC_64:
   case VKI_MSG_INFO|VKI_IPC_64:
      PRE_MEM_WRITE( "msgctl(IPC_INFO, buf)",
                     arg2, sizeof(struct vki_msginfo) );
      break;
   case VKI_IPC_STAT:
   case VKI_MSG_STAT:
      PRE_MEM_WRITE( "msgctl(IPC_STAT, buf)",
                     arg2, sizeof(struct vki_msqid_ds) );
      break;
   case VKI_IPC_STAT|VKI_IPC_64:
   case VKI_MSG_STAT|VKI_IPC_64:
      PRE_MEM_WRITE( "msgctl(IPC_STAT, arg.buf)",
                     arg2, sizeof(struct vki_msqid64_ds) );
      break;
   case VKI_IPC_SET:
      PRE_MEM_READ( "msgctl(IPC_SET, arg.buf)",
                    arg2, sizeof(struct vki_msqid_ds) );
      break;
   case VKI_IPC_SET|VKI_IPC_64:
      PRE_MEM_READ( "msgctl(IPC_SET, arg.buf)",
                    arg2, sizeof(struct vki_msqid64_ds) );
      break;
   }
}
void
ML_(linux_POST_sys_msgctl) ( ThreadId tid,
                             UWord res,
                             UWord arg0, UWord arg1, UWord arg2 )
{
   switch (arg1 /* cmd */) {
   case VKI_IPC_INFO:
   case VKI_MSG_INFO:
   case VKI_IPC_INFO|VKI_IPC_64:
   case VKI_MSG_INFO|VKI_IPC_64:
      POST_MEM_WRITE( arg2, sizeof(struct vki_msginfo) );
      break;
   case VKI_IPC_STAT:
   case VKI_MSG_STAT:
      POST_MEM_WRITE( arg2, sizeof(struct vki_msqid_ds) );
      break;
   case VKI_IPC_STAT|VKI_IPC_64:
   case VKI_MSG_STAT|VKI_IPC_64:
      POST_MEM_WRITE( arg2, sizeof(struct vki_msqid64_ds) );
      break;
   }
}

/* ---------------------------------------------------------------------
   *at wrappers
   ------------------------------------------------------------------ */

PRE(sys_openat)
{
   HChar  name[30];
   SysRes sres;

   if (ARG3 & VKI_O_CREAT) {
      // 4-arg version
      PRINT("sys_openat ( %d, %p(%s), %d, %d )",ARG1,ARG2,ARG2,ARG3,ARG4);
      PRE_REG_READ4(long, "openat",
                    int, dfd, const char *, filename, int, flags, int, mode);
   } else {
      // 3-arg version
      PRINT("sys_openat ( %d, %p(%s), %d )",ARG1,ARG2,ARG2,ARG3);
      PRE_REG_READ3(long, "openat",
                    int, dfd, const char *, filename, int, flags);
   }

   if (ARG1 != VKI_AT_FDCWD && !ML_(fd_allowed)(ARG1, "openat", tid, False))
      SET_STATUS_Failure( VKI_EBADF );
   else
      PRE_MEM_RASCIIZ( "openat(filename)", ARG2 );

   /* Handle the case where the open is of /proc/self/cmdline or
      /proc/<pid>/cmdline, and just give it a copy of the fd for the
      fake file we cooked up at startup (in m_main).  Also, seek the
      cloned fd back to the start. */

   VG_(sprintf)(name, "/proc/%d/cmdline", VG_(getpid)());
   if (ML_(safe_to_deref)( (void*)ARG2, 1 )
       && (VG_(strcmp)((Char *)ARG2, name) == 0 
           || VG_(strcmp)((Char *)ARG2, "/proc/self/cmdline") == 0)) {
      sres = VG_(dup)( VG_(cl_cmdline_fd) );
      SET_STATUS_from_SysRes( sres );
      if (!sres.isError) {
         OffT off = VG_(lseek)( sres.res, 0, VKI_SEEK_SET );
         if (off < 0)
            SET_STATUS_Failure( VKI_EMFILE );
      }
      return;
   }

   /* Otherwise handle normally */
   *flags |= SfMayBlock;
}

POST(sys_openat)
{
   vg_assert(SUCCESS);
   if (!ML_(fd_allowed)(RES, "openat", tid, True)) {
      VG_(close)(RES);
      SET_STATUS_Failure( VKI_EMFILE );
   } else {
      if (VG_(clo_track_fds))
         ML_(record_fd_open_with_given_name)(tid, RES, (Char*)ARG2);
   }
}

PRE(sys_mkdirat)
{
   *flags |= SfMayBlock;
   PRINT("sys_mkdirat ( %d, %p(%s), %d )", ARG1,ARG2,ARG2,ARG3);
   PRE_REG_READ3(long, "mkdirat",
                 int, dfd, const char *, pathname, int, mode);
   PRE_MEM_RASCIIZ( "mkdirat(pathname)", ARG2 );
}

PRE(sys_mknodat)
{
   PRINT("sys_mknodat ( %d, %p(%s), 0x%x, 0x%x )", ARG1,ARG2,ARG2,ARG3,ARG4 );
   PRE_REG_READ4(long, "mknodat",
                 int, dfd, const char *, pathname, int, mode, unsigned, dev);
   PRE_MEM_RASCIIZ( "mknodat(pathname)", ARG2 );
}

PRE(sys_fchownat)
{
   PRINT("sys_fchownat ( %d, %p(%s), 0x%x, 0x%x )", ARG1,ARG2,ARG2,ARG3,ARG4);
   PRE_REG_READ4(long, "fchownat",
                 int, dfd, const char *, path,
                 vki_uid_t, owner, vki_gid_t, group);
   PRE_MEM_RASCIIZ( "fchownat(path)", ARG2 );
}

PRE(sys_futimesat)
{
   PRINT("sys_futimesat ( %d, %p(%s), %p )", ARG1,ARG2,ARG2,ARG3);
   PRE_REG_READ3(long, "futimesat",
                 int, dfd, char *, filename, struct timeval *, tvp);
   PRE_MEM_RASCIIZ( "futimesat(filename)", ARG2 );
   if (ARG3 != 0)
      PRE_MEM_READ( "futimesat(tvp)", ARG3, sizeof(struct vki_timeval) );
}

PRE(sys_newfstatat)
{
   PRINT("sys_newfstatat ( %d, %p(%s), %p )", ARG1,ARG2,ARG2,ARG3);
   PRE_REG_READ3(long, "fstatat",
                 int, dfd, char *, file_name, struct stat *, buf);
   PRE_MEM_RASCIIZ( "fstatat(file_name)", ARG2 );
   PRE_MEM_WRITE( "fstatat(buf)", ARG3, sizeof(struct vki_stat) );
}

POST(sys_newfstatat)
{
   POST_MEM_WRITE( ARG3, sizeof(struct vki_stat) );
}

PRE(sys_unlinkat)
{
   *flags |= SfMayBlock;
   PRINT("sys_unlinkat ( %d, %p(%s) )", ARG1,ARG2,ARG2);
   PRE_REG_READ2(long, "unlinkat", int, dfd, const char *, pathname);
   PRE_MEM_RASCIIZ( "unlinkat(pathname)", ARG2 );
}

PRE(sys_renameat)
{
   PRINT("sys_renameat ( %d, %p(%s), %d, %p(%s) )", ARG1,ARG2,ARG2,ARG3,ARG4,ARG4);
   PRE_REG_READ4(long, "renameat",
                 int, olddfd, const char *, oldpath,
                 int, newdfd, const char *, newpath);
   PRE_MEM_RASCIIZ( "renameat(oldpath)", ARG2 );
   PRE_MEM_RASCIIZ( "renameat(newpath)", ARG4 );
}

PRE(sys_linkat)
{
   *flags |= SfMayBlock;
   PRINT("sys_linkat ( %d, %p(%s), %d, %p(%s), %d )",ARG1,ARG2,ARG2,ARG3,ARG4,ARG4,ARG5);
   PRE_REG_READ5(long, "linkat",
                 int, olddfd, const char *, oldpath,
                 int, newdfd, const char *, newpath,
                 int, flags);
   PRE_MEM_RASCIIZ( "linkat(oldpath)", ARG2);
   PRE_MEM_RASCIIZ( "linkat(newpath)", ARG4);
}

PRE(sys_symlinkat)
{
   *flags |= SfMayBlock;
   PRINT("sys_symlinkat ( %p(%s), %d, %p(%s) )",ARG1,ARG1,ARG2,ARG3,ARG3);
   PRE_REG_READ3(long, "symlinkat",
                 const char *, oldpath, int, newdfd, const char *, newpath);
   PRE_MEM_RASCIIZ( "symlinkat(oldpath)", ARG1 );
   PRE_MEM_RASCIIZ( "symlinkat(newpath)", ARG3 );
}

PRE(sys_readlinkat)
{
   HChar name[25];
   Word  saved = SYSNO;

   PRINT("sys_readlinkat ( %d, %p(%s), %p, %llu )", ARG1,ARG2,ARG2,ARG3,(ULong)ARG4);
   PRE_REG_READ4(long, "readlinkat",
                 int, dfd, const char *, path, char *, buf, int, bufsiz);
   PRE_MEM_RASCIIZ( "readlinkat(path)", ARG2 );
   PRE_MEM_WRITE( "readlinkat(buf)", ARG3,ARG4 );

   /*
    * Handle the case where readlinkat is looking at /proc/self/exe or
    * /proc/<pid>/exe.
    */
   VG_(sprintf)(name, "/proc/%d/exe", VG_(getpid)());
   if (ML_(safe_to_deref)((void*)ARG2, 1)
       && (VG_(strcmp)((Char *)ARG2, name) == 0 
           || VG_(strcmp)((Char *)ARG2, "/proc/self/exe") == 0)) {
      VG_(sprintf)(name, "/proc/self/fd/%d", VG_(cl_exec_fd));
      SET_STATUS_from_SysRes( VG_(do_syscall4)(saved, ARG1, (UWord)name, 
                                                      ARG3, ARG4));
   } else {
      /* Normal case */
      SET_STATUS_from_SysRes( VG_(do_syscall4)(saved, ARG1, ARG2, ARG3, ARG4));
   }

   if (SUCCESS && RES > 0)
      POST_MEM_WRITE( ARG3, RES );
}

PRE(sys_fchmodat)
{
   PRINT("sys_fchmodat ( %d, %p(%s), %d )", ARG1,ARG2,ARG2,ARG3);
   PRE_REG_READ3(long, "fchmodat",
                 int, dfd, const char *, path, vki_mode_t, mode);
   PRE_MEM_RASCIIZ( "fchmodat(path)", ARG2 );
}

PRE(sys_faccessat)
{
   PRINT("sys_faccessat ( %d, %p(%s), %d )", ARG1,ARG2,ARG2,ARG3);
   PRE_REG_READ3(long, "faccessat",
                 int, dfd, const char *, pathname, int, mode);
   PRE_MEM_RASCIIZ( "faccessat(pathname)", ARG2 );
}

/* ---------------------------------------------------------------------
   key retention service wrappers
   ------------------------------------------------------------------ */

PRE(sys_request_key)
{
   PRINT("sys_request_key ( %p(%s), %p(%s), %p(%s), %d )",
         ARG1,ARG1,ARG2,ARG2,ARG3,ARG3,ARG4);
   PRE_REG_READ4(long, "request_key",
                 const char *, type, const char *, description, 
                 const char *, callout_info, vki_key_serial_t, keyring);
   PRE_MEM_RASCIIZ( "request_key(type)", ARG1);
   PRE_MEM_RASCIIZ( "request_key(description)", ARG2);
   if (ARG3 != (UWord)NULL)
      PRE_MEM_RASCIIZ( "request_key(callout_info)", ARG3);
}

PRE(sys_add_key)
{
   PRINT("sys_add_key ( %p(%s), %p(%s), %p, %d, %d )",
         ARG1,ARG1,ARG2,ARG2,ARG3,ARG4,ARG5);
   PRE_REG_READ5(long, "add_key",
                 const char *, type, const char *, description,
                 const void *, payload, vki_size_t, plen, 
                 vki_key_serial_t, keyring);
   PRE_MEM_RASCIIZ( "add_key(type)", ARG1);
   PRE_MEM_RASCIIZ( "add_key(description)", ARG2);
   if (ARG3 != (UWord)NULL)
      PRE_MEM_READ( "request_key(payload)", ARG3, ARG4);
}

PRE(sys_keyctl)
{
   switch (ARG1 /* option */) {
   case VKI_KEYCTL_GET_KEYRING_ID:
      PRINT("sys_keyctl ( KEYCTL_GET_KEYRING_ID, %d, %d )", ARG2,ARG3);
      PRE_REG_READ3(long, "keyctl(KEYCTL_GET_KEYRING_ID)",
                    int, option, vki_key_serial_t, id, int, create);
      break;
   case VKI_KEYCTL_JOIN_SESSION_KEYRING:
      PRINT("sys_keyctl ( KEYCTL_JOIN_SESSION_KEYRING, %p(%s) )", ARG2,ARG2);
      PRE_REG_READ2(long, "keyctl(KEYCTL_JOIN_SESSION_KEYRING)",
                    int, option, const char *, name);
      if (ARG2 != (UWord)NULL)
         PRE_MEM_RASCIIZ("keyctl(KEYCTL_JOIN_SESSION_KEYRING, name)", ARG2);
      break;
   case VKI_KEYCTL_UPDATE:
      PRINT("sys_keyctl ( KEYCTL_UPDATE, %d, %p, %d )", ARG2,ARG3,ARG4);
      PRE_REG_READ4(long, "keyctl(KEYCTL_UPDATE)",
                    int, option, vki_key_serial_t, key,
                    const void *, payload, vki_size_t, plen);
      if (ARG3 != (UWord)NULL)
         PRE_MEM_READ("keyctl(KEYCTL_UPDATE, payload)", ARG3, ARG4);
      break;
   case VKI_KEYCTL_REVOKE:
      PRINT("sys_keyctl ( KEYCTL_REVOKE, %d )", ARG2);
      PRE_REG_READ2(long, "keyctl(KEYCTL_REVOKE)",
                    int, option, vki_key_serial_t, id);
      break;
   case VKI_KEYCTL_CHOWN:
      PRINT("sys_keyctl ( KEYCTL_CHOWN, %d, %d, %d )", ARG2,ARG3,ARG4);
      PRE_REG_READ4(long, "keyctl(KEYCTL_CHOWN)",
                    int, option, vki_key_serial_t, id,
                    vki_uid_t, uid, vki_gid_t, gid);
      break;
   case VKI_KEYCTL_SETPERM:
      PRINT("sys_keyctl ( KEYCTL_SETPERM, %d, %d )", ARG2,ARG3);
      PRE_REG_READ3(long, "keyctl(KEYCTL_SETPERM)",
                    int, option, vki_key_serial_t, id, vki_key_perm_t, perm);
      break;
   case VKI_KEYCTL_DESCRIBE:
      PRINT("sys_keyctl ( KEYCTL_DESCRIBE, %d, %p, %d )", ARG2,ARG3,ARG4);
      PRE_REG_READ4(long, "keyctl(KEYCTL_DESCRIBE)",
                    int, option, vki_key_serial_t, id,
                    char *, buffer, vki_size_t, buflen);
      if (ARG3 != (UWord)NULL)
         PRE_MEM_WRITE("keyctl(KEYCTL_DESCRIBE, buffer)", ARG3, ARG4);
      break;
   case VKI_KEYCTL_CLEAR:
      PRINT("sys_keyctl ( KEYCTL_CLEAR, %d )", ARG2);
      PRE_REG_READ2(long, "keyctl(KEYCTL_CLEAR)",
                    int, option, vki_key_serial_t, keyring);
      break;
   case VKI_KEYCTL_LINK:
      PRINT("sys_keyctl ( KEYCTL_LINK, %d, %d )", ARG2,ARG3);
      PRE_REG_READ3(long, "keyctl(KEYCTL_LINK)", int, option,
                    vki_key_serial_t, keyring, vki_key_serial_t, key);
      break;
   case VKI_KEYCTL_UNLINK:
      PRINT("sys_keyctl ( KEYCTL_UNLINK, %d, %d )", ARG2,ARG3);
      PRE_REG_READ3(long, "keyctl(KEYCTL_UNLINK)", int, option,
                    vki_key_serial_t, keyring, vki_key_serial_t, key);
      break;
   case VKI_KEYCTL_SEARCH:
      PRINT("sys_keyctl ( KEYCTL_SEARCH, %d, %p(%s), %p(%s), %d )",
            ARG2,ARG3,ARG3,ARG4,ARG4,ARG5);
      PRE_REG_READ5(long, "keyctl(KEYCTL_SEARCH)",
                    int, option, vki_key_serial_t, keyring, 
                    const char *, type, const char *, description,
                    vki_key_serial_t, destring);
      PRE_MEM_RASCIIZ("sys_keyctl(KEYCTL_SEARCH, type)", ARG3);
      PRE_MEM_RASCIIZ("sys_keyctl(KEYCTL_SEARCH, description)", ARG4);
      break;
   case VKI_KEYCTL_READ:
      PRINT("sys_keyctl ( KEYCTL_READ, %d, %p, %d )", ARG2,ARG3,ARG4);
      PRE_REG_READ4(long, "keyctl(KEYCTL_READ)",
                    int, option, vki_key_serial_t, keyring, 
                    char *, buffer, vki_size_t, buflen);
      if (ARG3 != (UWord)NULL)
         PRE_MEM_WRITE("keyctl(KEYCTL_READ, buffer)", ARG3, ARG4);
      break;
   case VKI_KEYCTL_INSTANTIATE:
      PRINT("sys_keyctl ( KEYCTL_INSTANTIATE, %d, %p, %d, %d )",
            ARG2,ARG3,ARG4,ARG5);
      PRE_REG_READ5(long, "keyctl(KEYCTL_INSTANTIATE)",
                    int, option, vki_key_serial_t, key, 
                    char *, payload, vki_size_t, plen,
                    vki_key_serial_t, keyring);
      if (ARG3 != (UWord)NULL)
         PRE_MEM_READ("keyctl(KEYCTL_INSTANTIATE, payload)", ARG3, ARG4);
      break;
   case VKI_KEYCTL_NEGATE:
      PRINT("sys_keyctl ( KEYCTL_NEGATE, %d, %u, %d )", ARG2,ARG3,ARG4);
      PRE_REG_READ4(long, "keyctl(KEYCTL_NEGATE)",
                    int, option, vki_key_serial_t, key, 
                    unsigned, timeout, vki_key_serial_t, keyring);
      break;
   case VKI_KEYCTL_SET_REQKEY_KEYRING:
      PRINT("sys_keyctl ( KEYCTL_SET_REQKEY_KEYRING, %d )", ARG2);
      PRE_REG_READ2(long, "keyctl(KEYCTL_SET_REQKEY_KEYRING)",
                    int, option, int, reqkey_defl);
      break;
   case VKI_KEYCTL_SET_TIMEOUT:
      PRINT("sys_keyctl ( KEYCTL_SET_TIMEOUT, %d, %d )", ARG2,ARG3);
      PRE_REG_READ3(long, "keyctl(KEYCTL_SET_TIMEOUT)",
                    int, option, vki_key_serial_t, key, unsigned, timeout);
      break;
   case VKI_KEYCTL_ASSUME_AUTHORITY:
      PRINT("sys_keyctl ( KEYCTL_ASSUME_AUTHORITY, %d )", ARG2);
      PRE_REG_READ2(long, "keyctl(KEYCTL_ASSUME_AUTHORITY)",
                    int, option, vki_key_serial_t, key);
      break;
   default:
      PRINT("sys_keyctl ( %d ) ", ARG1);
      PRE_REG_READ1(long, "keyctl", int, option);
      break;
   }
}

POST(sys_keyctl)
{
   vg_assert(SUCCESS);
   switch (ARG1 /* option */) {
   case VKI_KEYCTL_DESCRIBE:
   case VKI_KEYCTL_READ:
      if (RES > ARG4)
         POST_MEM_WRITE(ARG3, ARG4);
      else
         POST_MEM_WRITE(ARG3, RES);
      break;
   default:
      break;
   }
}

/* ---------------------------------------------------------------------
   ioprio_ wrappers
   ------------------------------------------------------------------ */

/* _syscall3(int, ioprio_set, int, which, int, who, int, ioprio); */

PRE(sys_ioprio_set)
{
   PRINT("sys_ioprio_set ( %ld, %ld, %ld )", ARG1,ARG2,ARG3);
   PRE_REG_READ3(int, "ioprio_set", int, which, int, who, int, ioprio);
}


#undef PRE
#undef POST

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/


