
/*--------------------------------------------------------------------*/
/*--- Linux-specific syscalls, etc.                syswrap-linux.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2010 Nicholas Nethercote
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

#if defined(VGO_linux)

#include "pub_core_basics.h"
#include "pub_core_vki.h"
#include "pub_core_vkiscnums.h"
#include "pub_core_libcsetjmp.h"   // to keep _threadstate.h happy
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

   VG_TRACK(pre_thread_first_insn, tid);

   tst->os_state.lwpid = VG_(gettid)();
   /* Set the threadgroup for real.  This overwrites the provisional
      value set in do_clone() syswrap-*-linux.c.  See comments in
      do_clone for background, also #226116. */
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

   // Tell the tool this thread is exiting
   VG_TRACK( pre_thread_ll_exit, tid );

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
#elif defined(VGP_arm_linux)
      asm volatile (
         "str  %1, %0\n"      /* set tst->status = VgTs_Empty */
         "mov  r7, %2\n"      /* set %r7 = __NR_exit */
         "ldr  r0, %3\n"      /* set %r0 = tst->os_state.exitcode */
         "svc  0x00000000\n"  /* exit(tst->os_state.exitcode) */
         : "=m" (tst->status)
         : "r" (VgTs_Empty), "n" (__NR_exit), "m" (tst->os_state.exitcode));
#elif defined(VGP_s390x_linux)
      asm volatile (
         "st   %1, %0\n"        /* set tst->status = VgTs_Empty */
         "lg   2, %3\n"         /* set r2 = tst->os_state.exitcode */
         "svc %2\n"             /* exit(tst->os_state.exitcode) */
         : "=m" (tst->status)
         : "d" (VgTs_Empty), "n" (__NR_exit), "m" (tst->os_state.exitcode)
         : "2");
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
#elif defined(VGP_s390x_linux)
   /* make a stack frame */
   sp -= 160;
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

   VG_(do_atfork_pre)(tid);

   /* Since this is the fork() form of clone, we don't need all that
      VG_(clone) stuff */
#if defined(VGP_x86_linux) \
    || defined(VGP_ppc32_linux) || defined(VGP_ppc64_linux) \
    || defined(VGP_arm_linux)
   res = VG_(do_syscall5)( __NR_clone, flags, 
                           (UWord)NULL, (UWord)parent_tidptr, 
                           (UWord)NULL, (UWord)child_tidptr );
#elif defined(VGP_amd64_linux)
   /* note that the last two arguments are the opposite way round to x86 and
      ppc32 as the amd64 kernel expects the arguments in a different order */
   res = VG_(do_syscall5)( __NR_clone, flags, 
                           (UWord)NULL, (UWord)parent_tidptr, 
                           (UWord)child_tidptr, (UWord)NULL );
#elif defined(VGP_s390x_linux)
   /* Note that s390 has the stack first and then the flags */
   res = VG_(do_syscall4)( __NR_clone, (UWord) NULL, flags,
                          (UWord)parent_tidptr, (UWord)child_tidptr);
#else
# error Unknown platform
#endif

   if (!sr_isError(res) && sr_Res(res) == 0) {
      /* child */
      VG_(do_atfork_child)(tid);

      /* restore signal mask */
      VG_(sigprocmask)(VKI_SIG_SETMASK, &fork_saved_mask, NULL);

      /* If --child-silent-after-fork=yes was specified, set the
         output file descriptors to 'impossible' values.  This is
         noticed by send_bytes_to_logging_sink in m_libcprint.c, which
         duly stops writing any further output. */
      if (VG_(clo_child_silent_after_fork)) {
         if (!VG_(log_output_sink).is_socket)
            VG_(log_output_sink).fd = -1;
         if (!VG_(xml_output_sink).is_socket)
            VG_(xml_output_sink).fd = -1;
      }
   } 
   else 
   if (!sr_isError(res) && sr_Res(res) > 0) {
      /* parent */
      VG_(do_atfork_parent)(tid);

      if (VG_(clo_trace_syscalls))
	  VG_(printf)("   clone(fork): process %d created child %ld\n",
                      VG_(getpid)(), sr_Res(res));

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

// Macros to support 64-bit syscall args split into two 32 bit values
#define LOHI64(lo,hi)   ( ((ULong)(lo)) | (((ULong)(hi)) << 32) )
#if defined(VG_LITTLEENDIAN)
#define MERGE64(lo,hi)   ( ((ULong)(lo)) | (((ULong)(hi)) << 32) )
#define MERGE64_FIRST(name) name##_low
#define MERGE64_SECOND(name) name##_high
#elif defined(VG_BIGENDIAN)
#define MERGE64(hi,lo)   ( ((ULong)(lo)) | (((ULong)(hi)) << 32) )
#define MERGE64_FIRST(name) name##_high
#define MERGE64_SECOND(name) name##_low
#else
#error Unknown endianness
#endif

/* ---------------------------------------------------------------------
   *mount wrappers
   ------------------------------------------------------------------ */

PRE(sys_mount)
{
   // Nb: depending on 'flags', the 'type' and 'data' args may be ignored.
   // We are conservative and check everything, except the memory pointed to
   // by 'data'.
   *flags |= SfMayBlock;
   PRINT("sys_mount( %#lx(%s), %#lx(%s), %#lx(%s), %#lx, %#lx )",
         ARG1,(Char*)ARG1, ARG2,(Char*)ARG2, ARG3,(Char*)ARG3, ARG4, ARG5);
   PRE_REG_READ5(long, "mount",
                 char *, source, char *, target, char *, type,
                 unsigned long, flags, void *, data);
   if (ARG1)
      PRE_MEM_RASCIIZ( "mount(source)", ARG1);
   PRE_MEM_RASCIIZ( "mount(target)", ARG2);
   PRE_MEM_RASCIIZ( "mount(type)", ARG3);
}

PRE(sys_oldumount)
{
   PRINT("sys_oldumount( %#lx )", ARG1);
   PRE_REG_READ1(long, "umount", char *, path);
   PRE_MEM_RASCIIZ( "umount(path)", ARG1);
}

PRE(sys_umount)
{
   PRINT("sys_umount( %#lx, %ld )", ARG1, ARG2);
   PRE_REG_READ2(long, "umount2", char *, path, int, flags);
   PRE_MEM_RASCIIZ( "umount2(path)", ARG1);
}

/* ---------------------------------------------------------------------
   16- and 32-bit uid/gid wrappers
   ------------------------------------------------------------------ */

PRE(sys_setfsuid16)
{
   PRINT("sys_setfsuid16 ( %ld )", ARG1);
   PRE_REG_READ1(long, "setfsuid16", vki_old_uid_t, uid);
}

PRE(sys_setfsuid)
{
   PRINT("sys_setfsuid ( %ld )", ARG1);
   PRE_REG_READ1(long, "setfsuid", vki_uid_t, uid);
}

PRE(sys_setfsgid16)
{
   PRINT("sys_setfsgid16 ( %ld )", ARG1);
   PRE_REG_READ1(long, "setfsgid16", vki_old_gid_t, gid);
}

PRE(sys_setfsgid)
{
   PRINT("sys_setfsgid ( %ld )", ARG1);
   PRE_REG_READ1(long, "setfsgid", vki_gid_t, gid);
}

PRE(sys_setresuid16)
{
   PRINT("sys_setresuid16 ( %ld, %ld, %ld )", ARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "setresuid16",
                 vki_old_uid_t, ruid, vki_old_uid_t, euid, vki_old_uid_t, suid);
}

PRE(sys_setresuid)
{
   PRINT("sys_setresuid ( %ld, %ld, %ld )", ARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "setresuid",
                 vki_uid_t, ruid, vki_uid_t, euid, vki_uid_t, suid);
}

PRE(sys_getresuid16)
{
   PRINT("sys_getresuid16 ( %#lx, %#lx, %#lx )", ARG1,ARG2,ARG3);
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
   PRINT("sys_getresuid ( %#lx, %#lx, %#lx )", ARG1,ARG2,ARG3);
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
   PRINT("sys_setresgid16 ( %ld, %ld, %ld )", ARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "setresgid16",
                 vki_old_gid_t, rgid, 
                 vki_old_gid_t, egid, vki_old_gid_t, sgid);
}

PRE(sys_setresgid)
{
   PRINT("sys_setresgid ( %ld, %ld, %ld )", ARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "setresgid",
                 vki_gid_t, rgid, vki_gid_t, egid, vki_gid_t, sgid);
}

PRE(sys_getresgid16)
{
   PRINT("sys_getresgid16 ( %#lx, %#lx, %#lx )", ARG1,ARG2,ARG3);
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
   PRINT("sys_getresgid ( %#lx, %#lx, %#lx )", ARG1,ARG2,ARG3);
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

   PRINT("exit_group( %ld )", ARG1);
   PRE_REG_READ1(void, "exit_group", int, status);

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
   PRINT("sys_llseek ( %ld, 0x%lx, 0x%lx, %#lx, %ld )", ARG1,ARG2,ARG3,ARG4,ARG5);
   PRE_REG_READ5(long, "llseek",
                 unsigned int, fd, unsigned long, offset_high,
                 unsigned long, offset_low, vki_loff_t *, result,
                 unsigned int, whence);
   if (!ML_(fd_allowed)(ARG1, "llseek", tid, False))
      SET_STATUS_Failure( VKI_EBADF );
   else
      PRE_MEM_WRITE( "llseek(result)", ARG4, sizeof(vki_loff_t));
}
POST(sys_llseek)
{
   vg_assert(SUCCESS);
   if (RES == 0)
      POST_MEM_WRITE( ARG4, sizeof(vki_loff_t) );
}

PRE(sys_adjtimex)
{
   struct vki_timex *tx = (struct vki_timex *)ARG1;
   PRINT("sys_adjtimex ( %#lx )", ARG1);
   PRE_REG_READ1(long, "adjtimex", struct timex *, buf);
   PRE_MEM_READ( "adjtimex(timex->modes)", ARG1, sizeof(tx->modes));

#define ADJX(bits,field) 				\
   if (tx->modes & (bits))                              \
      PRE_MEM_READ( "adjtimex(timex->"#field")",	\
		    (Addr)&tx->field, sizeof(tx->field))

   if (tx->modes & VKI_ADJ_ADJTIME) {
      if (!(tx->modes & VKI_ADJ_OFFSET_READONLY))
         PRE_MEM_READ( "adjtimex(timex->offset)", (Addr)&tx->offset, sizeof(tx->offset));
   } else {
      ADJX(VKI_ADJ_OFFSET, offset);
      ADJX(VKI_ADJ_FREQUENCY, freq);
      ADJX(VKI_ADJ_MAXERROR, maxerror);
      ADJX(VKI_ADJ_ESTERROR, esterror);
      ADJX(VKI_ADJ_STATUS, status);
      ADJX(VKI_ADJ_TIMECONST|VKI_ADJ_TAI, constant);
      ADJX(VKI_ADJ_TICK, tick);
   }
#undef ADJX

   PRE_MEM_WRITE( "adjtimex(timex)", ARG1, sizeof(struct vki_timex));
}

POST(sys_adjtimex)
{
   POST_MEM_WRITE( ARG1, sizeof(struct vki_timex) );
}

PRE(sys_ioperm)
{
   PRINT("sys_ioperm ( %ld, %ld, %ld )", ARG1, ARG2, ARG3 );
   PRE_REG_READ3(long, "ioperm",
                 unsigned long, from, unsigned long, num, int, turn_on);
}

PRE(sys_syslog)
{
   *flags |= SfMayBlock;
   PRINT("sys_syslog (%ld, %#lx, %ld)", ARG1,ARG2,ARG3);
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
   PRINT("sys_sysinfo ( %#lx )",ARG1);
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
   PRINT("sys_sysctl ( %#lx )", ARG1 );
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
   PRINT( "sys_prctl ( %ld, %ld, %ld, %ld, %ld )", ARG1, ARG2, ARG3, ARG4, ARG5 );
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
   PRINT("sys_sendfile ( %ld, %ld, %#lx, %lu )", ARG1,ARG2,ARG3,ARG4);
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
   PRINT("sendfile64 ( %ld, %ld, %#lx, %lu )",ARG1,ARG2,ARG3,ARG4);
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
   PRINT("sys_futex ( %#lx, %ld, %ld, %#lx, %#lx )", ARG1,ARG2,ARG3,ARG4,ARG5);
   switch(ARG2 & ~(VKI_FUTEX_PRIVATE_FLAG|VKI_FUTEX_CLOCK_REALTIME)) {
   case VKI_FUTEX_CMP_REQUEUE:
   case VKI_FUTEX_WAKE_OP:
   case VKI_FUTEX_CMP_REQUEUE_PI:
      PRE_REG_READ6(long, "futex", 
                    vki_u32 *, futex, int, op, int, val,
                    struct timespec *, utime, vki_u32 *, uaddr2, int, val3);
      break;
   case VKI_FUTEX_REQUEUE:
   case VKI_FUTEX_WAIT_REQUEUE_PI:
      PRE_REG_READ5(long, "futex", 
                    vki_u32 *, futex, int, op, int, val,
                    struct timespec *, utime, vki_u32 *, uaddr2);
      break;
   case VKI_FUTEX_WAIT_BITSET:
      PRE_REG_READ6(long, "futex", 
                    vki_u32 *, futex, int, op, int, val,
                    struct timespec *, utime, int, dummy, int, val3);
      break;
   case VKI_FUTEX_WAKE_BITSET:
      PRE_REG_READ6(long, "futex", 
                    vki_u32 *, futex, int, op, int, val,
                    int, dummy, int, dummy2, int, val3);
      break;
   case VKI_FUTEX_WAIT:
   case VKI_FUTEX_LOCK_PI:
      PRE_REG_READ4(long, "futex", 
                    vki_u32 *, futex, int, op, int, val,
                    struct timespec *, utime);
      break;
   case VKI_FUTEX_WAKE:
   case VKI_FUTEX_FD:
   case VKI_FUTEX_TRYLOCK_PI:
      PRE_REG_READ3(long, "futex", 
                    vki_u32 *, futex, int, op, int, val);
      break;
   case VKI_FUTEX_UNLOCK_PI:
   default:
      PRE_REG_READ2(long, "futex", vki_u32 *, futex, int, op);
      break;
   }

   *flags |= SfMayBlock;

   switch(ARG2 & ~(VKI_FUTEX_PRIVATE_FLAG|VKI_FUTEX_CLOCK_REALTIME)) {
   case VKI_FUTEX_WAIT:
   case VKI_FUTEX_LOCK_PI:
   case VKI_FUTEX_WAIT_BITSET:
   case VKI_FUTEX_WAIT_REQUEUE_PI:
      PRE_MEM_READ( "futex(futex)", ARG1, sizeof(Int) );
      if (ARG4 != 0)
	 PRE_MEM_READ( "futex(timeout)", ARG4, sizeof(struct vki_timespec) );
      break;

   case VKI_FUTEX_REQUEUE:
   case VKI_FUTEX_CMP_REQUEUE:
   case VKI_FUTEX_CMP_REQUEUE_PI:
   case VKI_FUTEX_WAKE_OP:
      PRE_MEM_READ( "futex(futex)", ARG1, sizeof(Int) );
      PRE_MEM_READ( "futex(futex2)", ARG5, sizeof(Int) );
      break;

   case VKI_FUTEX_FD:
   case VKI_FUTEX_TRYLOCK_PI:
   case VKI_FUTEX_UNLOCK_PI:
      PRE_MEM_READ( "futex(futex)", ARG1, sizeof(Int) );
     break;

   case VKI_FUTEX_WAKE:
   case VKI_FUTEX_WAKE_BITSET:
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
   PRINT("sys_set_robust_list ( %#lx, %ld )", ARG1,ARG2);
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
   PRINT("sys_get_robust_list ( %ld, %#lx, %ld )", ARG1,ARG2,ARG3);
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
   PRINT("sys_pselect6 ( %ld, %#lx, %#lx, %#lx, %#lx, %#lx )", ARG1,ARG2,ARG3,ARG4,ARG5,ARG6);
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
   PRINT("sys_ppoll ( %#lx, %ld, %#lx, %#lx, %llu )\n", ARG1,ARG2,ARG3,ARG4,(ULong)ARG5);
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
   PRINT("sys_epoll_create ( %ld )", ARG1);
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

PRE(sys_epoll_create1)
{
   PRINT("sys_epoll_create1 ( %ld )", ARG1);
   PRE_REG_READ1(long, "epoll_create1", int, flags);
}
POST(sys_epoll_create1)
{
   vg_assert(SUCCESS);
   if (!ML_(fd_allowed)(RES, "epoll_create1", tid, True)) {
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
   PRINT("sys_epoll_ctl ( %ld, %s, %ld, %#lx )",
         ARG1, ( ARG2<3 ? epoll_ctl_s[ARG2] : "?" ), ARG3, ARG4);
   PRE_REG_READ4(long, "epoll_ctl",
                 int, epfd, int, op, int, fd, struct vki_epoll_event *, event);
   if (ARG2 != VKI_EPOLL_CTL_DEL)
      PRE_MEM_READ( "epoll_ctl(event)", ARG4, sizeof(struct vki_epoll_event) );
}

PRE(sys_epoll_wait)
{
   *flags |= SfMayBlock;
   PRINT("sys_epoll_wait ( %ld, %#lx, %ld, %ld )", ARG1, ARG2, ARG3, ARG4);
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

PRE(sys_epoll_pwait)
{
   *flags |= SfMayBlock;
   PRINT("sys_epoll_pwait ( %ld, %#lx, %ld, %ld, %#lx, %llu )", ARG1,ARG2,ARG3,ARG4,ARG5,(ULong)ARG6);
   PRE_REG_READ6(long, "epoll_pwait",
                 int, epfd, struct vki_epoll_event *, events,
                 int, maxevents, int, timeout, vki_sigset_t *, sigmask,
                 vki_size_t, sigsetsize);
   PRE_MEM_WRITE( "epoll_pwait(events)", ARG2, sizeof(struct vki_epoll_event)*ARG3);
   if (ARG4)
      PRE_MEM_READ( "epoll_pwait(sigmask)", ARG5, sizeof(vki_sigset_t) );
}
POST(sys_epoll_pwait)
{
   vg_assert(SUCCESS);
   if (RES > 0)
      POST_MEM_WRITE( ARG2, sizeof(struct vki_epoll_event)*RES ) ;
}

PRE(sys_eventfd)
{
   PRINT("sys_eventfd ( %lu )", ARG1);
   PRE_REG_READ1(long, "sys_eventfd", unsigned int, count);
}
POST(sys_eventfd)
{
   if (!ML_(fd_allowed)(RES, "eventfd", tid, True)) {
      VG_(close)(RES);
      SET_STATUS_Failure( VKI_EMFILE );
   } else {
      if (VG_(clo_track_fds))
         ML_(record_fd_open_nameless) (tid, RES);
   }
}

PRE(sys_eventfd2)
{
   PRINT("sys_eventfd2 ( %lu, %ld )", ARG1,ARG2);
   PRE_REG_READ2(long, "sys_eventfd2", unsigned int, count, int, flags);
}
POST(sys_eventfd2)
{
   if (!ML_(fd_allowed)(RES, "eventfd2", tid, True)) {
      VG_(close)(RES);
      SET_STATUS_Failure( VKI_EMFILE );
   } else {
      if (VG_(clo_track_fds))
         ML_(record_fd_open_nameless) (tid, RES);
   }
}

PRE(sys_fallocate)
{
   *flags |= SfMayBlock;
#if VG_WORDSIZE == 4
   PRINT("sys_fallocate ( %ld, %ld, %lld, %lld )",
         ARG1, ARG2, MERGE64(ARG3,ARG4), MERGE64(ARG5,ARG6));
   PRE_REG_READ6(long, "fallocate",
                 int, fd, int, mode,
                 unsigned, MERGE64_FIRST(offset), unsigned, MERGE64_SECOND(offset),
                 unsigned, MERGE64_FIRST(len), unsigned, MERGE64_SECOND(len));
#elif VG_WORDSIZE == 8
   PRINT("sys_fallocate ( %ld, %ld, %lld, %lld )",
         ARG1, ARG2, (Long)ARG3, (Long)ARG4);
   PRE_REG_READ4(long, "fallocate",
                 int, fd, int, mode, vki_loff_t, offset, vki_loff_t, len);
#else
#  error Unexpected word size
#endif
   if (!ML_(fd_allowed)(ARG1, "fallocate", tid, False))
      SET_STATUS_Failure( VKI_EBADF );
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
   PRINT("sys_set_tid_address ( %#lx )", ARG1);
   PRE_REG_READ1(long, "set_tid_address", int *, tidptr);
}

PRE(sys_tkill)
{
   PRINT("sys_tgkill ( %ld, %ld )", ARG1,ARG2);
   PRE_REG_READ2(long, "tkill", int, tid, int, sig);
   if (!ML_(client_signal_OK)(ARG2)) {
      SET_STATUS_Failure( VKI_EINVAL );
      return;
   }
   
   /* Check to see if this kill gave us a pending signal */
   *flags |= SfPollAfter;

   if (VG_(clo_trace_signals))
      VG_(message)(Vg_DebugMsg, "tkill: sending signal %ld to pid %ld\n",
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
      VG_(message)(Vg_DebugMsg, "tkill: sent signal %ld to pid %ld\n",
                   ARG2, ARG1);
}

PRE(sys_tgkill)
{
   PRINT("sys_tgkill ( %ld, %ld, %ld )", ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "tgkill", int, tgid, int, tid, int, sig);
   if (!ML_(client_signal_OK)(ARG3)) {
      SET_STATUS_Failure( VKI_EINVAL );
      return;
   }
   
   /* Check to see if this kill gave us a pending signal */
   *flags |= SfPollAfter;

   if (VG_(clo_trace_signals))
      VG_(message)(Vg_DebugMsg,
                   "tgkill: sending signal %ld to pid %ld/%ld\n",
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
      VG_(message)(Vg_DebugMsg,
                   "tgkill: sent signal %ld to pid %ld/%ld\n",
                   ARG3, ARG1, ARG2);
}

/* ---------------------------------------------------------------------
   fadvise64* wrappers
   ------------------------------------------------------------------ */

PRE(sys_fadvise64)
{
   PRINT("sys_fadvise64 ( %ld, %lld, %lu, %ld )",
         ARG1, MERGE64(ARG2,ARG3), ARG4, ARG5);
   PRE_REG_READ5(long, "fadvise64",
                 int, fd, vki_u32, MERGE64_FIRST(offset), vki_u32, MERGE64_SECOND(offset),
                 vki_size_t, len, int, advice);
}

PRE(sys_fadvise64_64)
{
   PRINT("sys_fadvise64_64 ( %ld, %lld, %lld, %ld )",
         ARG1, MERGE64(ARG2,ARG3), MERGE64(ARG4,ARG5), ARG6);
   PRE_REG_READ6(long, "fadvise64_64",
                 int, fd, vki_u32, MERGE64_FIRST(offset), vki_u32, MERGE64_SECOND(offset),
                 vki_u32, MERGE64_FIRST(len), vki_u32, MERGE64_SECOND(len), int, advice);
}

/* ---------------------------------------------------------------------
   io_* wrappers
   ------------------------------------------------------------------ */

// Nb: this wrapper has to pad/unpad memory around the syscall itself,
// and this allows us to control exactly the code that gets run while
// the padding is in place.

PRE(sys_io_setup)
{
   PRINT("sys_io_setup ( %lu, %#lx )", ARG1,ARG2);
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

   ML_(notify_core_and_tool_of_mmap)( (Addr)r, size,
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
   SizeT size = 0;
      
   PRINT("sys_io_destroy ( %llu )", (ULong)ARG1);
   PRE_REG_READ1(long, "io_destroy", vki_aio_context_t, ctx);

   // If we are going to seg fault (due to a bogus ARG1) do it as late as
   // possible...
   if (ML_(safe_to_deref)( (void*)ARG1, sizeof(struct vki_aio_ring))) {
      struct vki_aio_ring *r = (struct vki_aio_ring *)ARG1;
      size = VG_PGROUNDUP(sizeof(struct vki_aio_ring) + 
                          r->nr*sizeof(struct vki_io_event));
   }

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
   PRINT("sys_io_getevents ( %llu, %lld, %lld, %#lx, %#lx )",
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

         case VKI_IOCB_CMD_FSYNC:
            break;

         case VKI_IOCB_CMD_FDSYNC:
            break;

         case VKI_IOCB_CMD_PREADV:
	     if (vev->result > 0) {
                  struct vki_iovec * vec = (struct vki_iovec *)(Addr)cb->aio_buf;
                  Int remains = vev->result;
                  Int j;

                  for (j = 0; j < cb->aio_nbytes; j++) {
                       Int nReadThisBuf = vec[j].iov_len;
                       if (nReadThisBuf > remains) nReadThisBuf = remains;
                       POST_MEM_WRITE( (Addr)vec[j].iov_base, nReadThisBuf );
                       remains -= nReadThisBuf;
                       if (remains < 0) VG_(core_panic)("io_getevents(PREADV): remains < 0");
                  }
	     }
             break;

         case VKI_IOCB_CMD_PWRITEV:
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
   Int i, j;

   PRINT("sys_io_submit ( %llu, %ld, %#lx )", (ULong)ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "io_submit",
                 vki_aio_context_t, ctx_id, long, nr,
                 struct iocb **, iocbpp);
   PRE_MEM_READ( "io_submit(iocbpp)", ARG3, ARG2*sizeof(struct vki_iocb *) );
   if (ARG3 != 0) {
      for (i = 0; i < ARG2; i++) {
         struct vki_iocb *cb = ((struct vki_iocb **)ARG3)[i];
         struct vki_iovec *iov;

         PRE_MEM_READ( "io_submit(iocb)", (Addr)cb, sizeof(struct vki_iocb) );
         switch (cb->aio_lio_opcode) {
         case VKI_IOCB_CMD_PREAD:
            PRE_MEM_WRITE( "io_submit(PREAD)", cb->aio_buf, cb->aio_nbytes );
            break;

         case VKI_IOCB_CMD_PWRITE:
            PRE_MEM_READ( "io_submit(PWRITE)", cb->aio_buf, cb->aio_nbytes );
            break;

         case VKI_IOCB_CMD_FSYNC:
            break;

         case VKI_IOCB_CMD_FDSYNC:
            break;

         case VKI_IOCB_CMD_PREADV:
            iov = (struct vki_iovec *)(Addr)cb->aio_buf;
            PRE_MEM_READ( "io_submit(PREADV)", cb->aio_buf, cb->aio_nbytes * sizeof(struct vki_iovec) );
            for (j = 0; j < cb->aio_nbytes; j++)
                PRE_MEM_WRITE( "io_submit(PREADV(iov[i]))", (Addr)iov[j].iov_base, iov[j].iov_len );
            break;

         case VKI_IOCB_CMD_PWRITEV:
            iov = (struct vki_iovec *)(Addr)cb->aio_buf;
            PRE_MEM_READ( "io_submit(PWRITEV)", cb->aio_buf, cb->aio_nbytes * sizeof(struct vki_iovec) );
            for (j = 0; j < cb->aio_nbytes; j++)
                PRE_MEM_READ( "io_submit(PWRITEV(iov[i]))", (Addr)iov[j].iov_base, iov[j].iov_len );
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
   PRINT("sys_io_cancel ( %llu, %#lx, %#lx )", (ULong)ARG1,ARG2,ARG3);
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
   PRINT("sys_mbind ( %#lx, %lu, %ld, %#lx, %lu, %lu )", ARG1,ARG2,ARG3,ARG4,ARG5,ARG6);
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
   PRINT("sys_set_mempolicy ( %ld, %#lx, %ld )", ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "set_mempolicy",
                 int, policy, unsigned long *, nodemask,
                 unsigned long, maxnode);
   PRE_MEM_READ( "set_mempolicy(nodemask)", ARG2,
                 VG_ROUNDUP( ARG3, sizeof(UWord) ) / sizeof(UWord) );
}

PRE(sys_get_mempolicy)
{
   PRINT("sys_get_mempolicy ( %#lx, %#lx, %ld, %#lx, %lx )", ARG1,ARG2,ARG3,ARG4,ARG5);
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

PRE(sys_inotify_init1)
{
   PRINT("sys_inotify_init ( %ld )", ARG1);
   PRE_REG_READ1(long, "inotify_init", int, flag);
}

POST(sys_inotify_init1)
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
   PRINT( "sys_inotify_add_watch ( %ld, %#lx, %lx )", ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "inotify_add_watch", int, fd, char *, path, int, mask);
   PRE_MEM_RASCIIZ( "inotify_add_watch(path)", ARG2 );
}

PRE(sys_inotify_rm_watch)
{
   PRINT( "sys_inotify_rm_watch ( %ld, %lx )", ARG1,ARG2);
   PRE_REG_READ2(long, "inotify_rm_watch", int, fd, int, wd);
}

/* ---------------------------------------------------------------------
   mq_* wrappers
   ------------------------------------------------------------------ */

PRE(sys_mq_open)
{
   PRINT("sys_mq_open( %#lx(%s), %ld, %lld, %#lx )",
         ARG1,(char*)ARG1,ARG2,(ULong)ARG3,ARG4);
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
   PRINT("sys_mq_unlink ( %#lx(%s) )", ARG1,(char*)ARG1);
   PRE_REG_READ1(long, "mq_unlink", const char *, name);
   PRE_MEM_RASCIIZ( "mq_unlink(name)", ARG1 );
}

PRE(sys_mq_timedsend)
{
   *flags |= SfMayBlock;
   PRINT("sys_mq_timedsend ( %ld, %#lx, %llu, %ld, %#lx )",
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
   PRINT("sys_mq_timedreceive( %ld, %#lx, %llu, %#lx, %#lx )",
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
   POST_MEM_WRITE( ARG2, RES );
   if (ARG4 != 0)
      POST_MEM_WRITE( ARG4, sizeof(unsigned int) );
}

PRE(sys_mq_notify)
{
   PRINT("sys_mq_notify( %ld, %#lx )", ARG1,ARG2 );
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
   PRINT("sys_mq_getsetattr( %ld, %#lx, %#lx )", ARG1,ARG2,ARG3 );
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
   PRINT("sys_clock_settime( %ld, %#lx )", ARG1,ARG2);
   PRE_REG_READ2(long, "clock_settime", 
                 vki_clockid_t, clk_id, const struct timespec *, tp);
   PRE_MEM_READ( "clock_settime(tp)", ARG2, sizeof(struct vki_timespec) );
}

PRE(sys_clock_gettime)
{
   PRINT("sys_clock_gettime( %ld, %#lx )" , ARG1,ARG2);
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
   PRINT("sys_clock_getres( %ld, %#lx )" , ARG1,ARG2);
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
   PRINT("sys_clock_nanosleep( %ld, %ld, %#lx, %#lx )", ARG1,ARG2,ARG3,ARG4);
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
   PRINT("sys_timer_create( %ld, %#lx, %#lx )", ARG1,ARG2,ARG3);
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
   PRINT("sys_timer_settime( %lld, %ld, %#lx, %#lx )", (ULong)ARG1,ARG2,ARG3,ARG4);
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
   PRINT("sys_timer_gettime( %lld, %#lx )", (ULong)ARG1,ARG2);
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
   PRINT("sys_timer_getoverrun( %#lx )", ARG1);
   PRE_REG_READ1(long, "timer_getoverrun", vki_timer_t, timerid);
}

PRE(sys_timer_delete)
{
   PRINT("sys_timer_delete( %#lx )", ARG1);
   PRE_REG_READ1(long, "timer_delete", vki_timer_t, timerid);
}

/* ---------------------------------------------------------------------
   timerfd* wrappers
   See also http://lwn.net/Articles/260172/ for an overview.
   See also /usr/src/linux/fs/timerfd.c for the implementation.
   ------------------------------------------------------------------ */

/* Returns True if running on 2.6.22, else False (or False if
   cannot be determined). */
static Bool linux_kernel_2_6_22(void)
{
   static Int result = -1;
   Int fd, read;
   HChar release[64];
   SysRes res;

   if (result == -1) {
      res = VG_(open)("/proc/sys/kernel/osrelease", 0, 0);
      if (sr_isError(res))
         return False;
      fd = sr_Res(res);
      read = VG_(read)(fd, release, sizeof(release) - 1);
      vg_assert(read >= 0);
      release[read] = 0;
      VG_(close)(fd);
      //VG_(printf)("kernel release = %s\n", release);
      result = (VG_(strncmp)(release, "2.6.22", 6) == 0
                && (release[6] < '0' || release[6] > '9'));
   }
   vg_assert(result == 0 || result == 1);
   return result == 1;
}

PRE(sys_timerfd_create)
{
   if (linux_kernel_2_6_22()) {
      /* 2.6.22 kernel: timerfd system call. */
      PRINT("sys_timerfd ( %ld, %ld, %#lx )", ARG1, ARG2, ARG3);
      PRE_REG_READ3(long, "sys_timerfd",
                    int, fd, int, clockid, const struct itimerspec *, tmr);
      PRE_MEM_READ("timerfd(tmr)", ARG3,
                   sizeof(struct vki_itimerspec) );
      if ((Word)ARG1 != -1L && !ML_(fd_allowed)(ARG1, "timerfd", tid, False))
         SET_STATUS_Failure( VKI_EBADF );
   } else {
      /* 2.6.24 and later kernels: timerfd_create system call. */
      PRINT("sys_timerfd_create (%ld, %ld )", ARG1, ARG2);
      PRE_REG_READ2(long, "timerfd_create", int, clockid, int, flags);
   }
}
POST(sys_timerfd_create)
{
   if (linux_kernel_2_6_22())
   {
      /* 2.6.22 kernel: timerfd system call. */
      if (!ML_(fd_allowed)(RES, "timerfd", tid, True)) {
         VG_(close)(RES);
         SET_STATUS_Failure( VKI_EMFILE );
      } else {
         if (VG_(clo_track_fds))
            ML_(record_fd_open_nameless) (tid, RES);
      }
   }
   else
   {
      /* 2.6.24 and later kernels: timerfd_create system call. */
      if (!ML_(fd_allowed)(RES, "timerfd_create", tid, True)) {
         VG_(close)(RES);
         SET_STATUS_Failure( VKI_EMFILE );
      } else {
         if (VG_(clo_track_fds))
            ML_(record_fd_open_nameless) (tid, RES);
      }
   }
}

PRE(sys_timerfd_gettime)
{
   PRINT("sys_timerfd_gettime ( %ld, %#lx )", ARG1, ARG2);
   PRE_REG_READ2(long, "timerfd_gettime",
                 int, ufd,
                 struct vki_itimerspec*, otmr);
   if (!ML_(fd_allowed)(ARG1, "timerfd_gettime", tid, False))
      SET_STATUS_Failure(VKI_EBADF);
   else
      PRE_MEM_WRITE("timerfd_gettime(result)",
                    ARG2, sizeof(struct vki_itimerspec));
}
POST(sys_timerfd_gettime)
{
   if (RES == 0)
      POST_MEM_WRITE(ARG2, sizeof(struct vki_itimerspec));
}

PRE(sys_timerfd_settime)
{
   PRINT("sys_timerfd_settime ( %ld, %ld, %#lx, %#lx )", ARG1, ARG2, ARG3, ARG4);
   PRE_REG_READ4(long, "timerfd_settime",
                 int, ufd,
                 int, flags,
                 const struct vki_itimerspec*, utmr,
                 struct vki_itimerspec*, otmr);
   if (!ML_(fd_allowed)(ARG1, "timerfd_settime", tid, False))
      SET_STATUS_Failure(VKI_EBADF);
   else
   {
      PRE_MEM_READ("timerfd_settime(result)",
                   ARG3, sizeof(struct vki_itimerspec));
      if (ARG4)
      {
         PRE_MEM_WRITE("timerfd_settime(result)",
                       ARG4, sizeof(struct vki_itimerspec));
      }
   }
}
POST(sys_timerfd_settime)
{
   if (RES == 0 && ARG4 != 0)
      POST_MEM_WRITE(ARG4, sizeof(struct vki_itimerspec));
}

/* ---------------------------------------------------------------------
   capabilities wrappers
   ------------------------------------------------------------------ */

PRE(sys_capget)
{
   PRINT("sys_capget ( %#lx, %#lx )", ARG1, ARG2 );
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
   PRINT("sys_capset ( %#lx, %#lx )", ARG1, ARG2 );
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
   PRINT("sys_setuid16 ( %ld )", ARG1);
   PRE_REG_READ1(long, "setuid16", vki_old_uid_t, uid);
}

PRE(sys_getgid16)
{
   PRINT("sys_getgid16 ( )");
   PRE_REG_READ0(long, "getgid16");
}

PRE(sys_setgid16)
{
   PRINT("sys_setgid16 ( %ld )", ARG1);
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
   PRINT("setreuid16 ( 0x%lx, 0x%lx )", ARG1, ARG2);
   PRE_REG_READ2(long, "setreuid16", vki_old_uid_t, ruid, vki_old_uid_t, euid);
}

PRE(sys_setregid16)
{
   PRINT("sys_setregid16 ( %ld, %ld )", ARG1, ARG2);
   PRE_REG_READ2(long, "setregid16", vki_old_gid_t, rgid, vki_old_gid_t, egid);
}

PRE(sys_getgroups16)
{
   PRINT("sys_getgroups16 ( %ld, %#lx )", ARG1, ARG2);
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
   PRINT("sys_setgroups16 ( %llu, %#lx )", (ULong)ARG1, ARG2);
   PRE_REG_READ2(long, "setgroups16", int, size, vki_old_gid_t *, list);
   if (ARG1 > 0)
      PRE_MEM_READ( "setgroups16(list)", ARG2, ARG1 * sizeof(vki_old_gid_t) );
}

/* ---------------------------------------------------------------------
   *chown16 wrappers
   ------------------------------------------------------------------ */

PRE(sys_chown16)
{
   PRINT("sys_chown16 ( %#lx, 0x%lx, 0x%lx )", ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "chown16",
                 const char *, path,
                 vki_old_uid_t, owner, vki_old_gid_t, group);
   PRE_MEM_RASCIIZ( "chown16(path)", ARG1 );
}

PRE(sys_fchown16)
{
   PRINT("sys_fchown16 ( %ld, %ld, %ld )", ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "fchown16",
                 unsigned int, fd, vki_old_uid_t, owner, vki_old_gid_t, group);
}

/* ---------------------------------------------------------------------
   *xattr wrappers
   ------------------------------------------------------------------ */

PRE(sys_setxattr)
{
   *flags |= SfMayBlock;
   PRINT("sys_setxattr ( %#lx, %#lx, %#lx, %llu, %ld )",
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
   PRINT("sys_lsetxattr ( %#lx, %#lx, %#lx, %llu, %ld )",
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
   PRINT("sys_fsetxattr ( %ld, %#lx, %#lx, %llu, %ld )",
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
   PRINT("sys_getxattr ( %#lx, %#lx, %#lx, %llu )", ARG1,ARG2,ARG3, (ULong)ARG4);
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
   PRINT("sys_lgetxattr ( %#lx, %#lx, %#lx, %llu )", ARG1,ARG2,ARG3, (ULong)ARG4);
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
   PRINT("sys_fgetxattr ( %ld, %#lx, %#lx, %llu )", ARG1, ARG2, ARG3, (ULong)ARG4);
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
   PRINT("sys_listxattr ( %#lx, %#lx, %llu )", ARG1, ARG2, (ULong)ARG3);
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
   PRINT("sys_llistxattr ( %#lx, %#lx, %llu )", ARG1, ARG2, (ULong)ARG3);
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
   PRINT("sys_flistxattr ( %ld, %#lx, %llu )", ARG1, ARG2, (ULong)ARG3);
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
   PRINT("sys_removexattr ( %#lx, %#lx )", ARG1, ARG2);
   PRE_REG_READ2(long, "removexattr", char *, path, char *, name);
   PRE_MEM_RASCIIZ( "removexattr(path)", ARG1 );
   PRE_MEM_RASCIIZ( "removexattr(name)", ARG2 );
}

PRE(sys_lremovexattr)
{
   *flags |= SfMayBlock;
   PRINT("sys_lremovexattr ( %#lx, %#lx )", ARG1, ARG2);
   PRE_REG_READ2(long, "lremovexattr", char *, path, char *, name);
   PRE_MEM_RASCIIZ( "lremovexattr(path)", ARG1 );
   PRE_MEM_RASCIIZ( "lremovexattr(name)", ARG2 );
}

PRE(sys_fremovexattr)
{
   *flags |= SfMayBlock;
   PRINT("sys_fremovexattr ( %ld, %#lx )", ARG1, ARG2);
   PRE_REG_READ2(long, "fremovexattr", int, fd, char *, name);
   PRE_MEM_RASCIIZ( "fremovexattr(name)", ARG2 );
}

/* ---------------------------------------------------------------------
   sched_* wrappers
   ------------------------------------------------------------------ */

PRE(sys_sched_setparam)
{
   PRINT("sched_setparam ( %ld, %#lx )", ARG1, ARG2 );
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
   PRINT("sched_getparam ( %ld, %#lx )", ARG1, ARG2 );
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
   PRINT("sys_sched_getscheduler ( %ld )", ARG1);
   PRE_REG_READ1(long, "sched_getscheduler", vki_pid_t, pid);
}

PRE(sys_sched_setscheduler)
{
   PRINT("sys_sched_setscheduler ( %ld, %ld, %#lx )", ARG1,ARG2,ARG3);
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
   PRINT("sched_get_priority_max ( %ld )", ARG1);
   PRE_REG_READ1(long, "sched_get_priority_max", int, policy);
}

PRE(sys_sched_get_priority_min)
{
   PRINT("sched_get_priority_min ( %ld )", ARG1);
   PRE_REG_READ1(long, "sched_get_priority_min", int, policy);
}

PRE(sys_sched_rr_get_interval)
{
   PRINT("sys_sched_rr_get_interval ( %ld, %#lx )", ARG1, ARG2);
   PRE_REG_READ2(int, "sched_rr_get_interval",
                 vki_pid_t, pid,
                 struct vki_timespec *, tp);
   PRE_MEM_WRITE("sched_rr_get_interval(timespec)",
                 ARG2, sizeof(struct vki_timespec));
}

POST(sys_sched_rr_get_interval)
{
   POST_MEM_WRITE(ARG2, sizeof(struct vki_timespec));
}

PRE(sys_sched_setaffinity)
{
   PRINT("sched_setaffinity ( %ld, %ld, %#lx )", ARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "sched_setaffinity", 
                 vki_pid_t, pid, unsigned int, len, unsigned long *, mask);
   PRE_MEM_READ( "sched_setaffinity(mask)", ARG3, ARG2);
}

PRE(sys_sched_getaffinity)
{
   PRINT("sched_getaffinity ( %ld, %ld, %#lx )", ARG1, ARG2, ARG3);
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
   PRINT("sys_pipe ( %#lx )", ARG1);
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

/* pipe2 (a kernel 2.6.twentysomething invention) is like pipe, except
   there's a second arg containing flags to be applied to the new file
   descriptors.  It hardly seems worth the effort to factor out the
   duplicated code, hence: */
PRE(sys_pipe2)
{
   PRINT("sys_pipe2 ( %#lx, %#lx )", ARG1, ARG2);
   PRE_REG_READ2(int, "pipe", int *, filedes, long, flags);
   PRE_MEM_WRITE( "pipe2(filedes)", ARG1, 2*sizeof(int) );
}
POST(sys_pipe2)
{
   Int *p = (Int *)ARG1;
   if (!ML_(fd_allowed)(p[0], "pipe2", tid, True) ||
       !ML_(fd_allowed)(p[1], "pipe2", tid, True)) {
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

PRE(sys_dup3)
{
   PRINT("sys_dup3 ( %ld, %ld, %ld )", ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "dup3", unsigned int, oldfd, unsigned int, newfd, int, flags);
   if (!ML_(fd_allowed)(ARG2, "dup3", tid, True))
      SET_STATUS_Failure( VKI_EBADF );
}

POST(sys_dup3)
{
   vg_assert(SUCCESS);
   if (VG_(clo_track_fds))
      ML_(record_fd_open_named)(tid, RES);
}

PRE(sys_quotactl)
{
   PRINT("sys_quotactl (0x%lx, %#lx, 0x%lx, 0x%lx )", ARG1,ARG2,ARG3, ARG4);
   PRE_REG_READ4(long, "quotactl",
                 unsigned int, cmd, const char *, special, vki_qid_t, id,
                 void *, addr);
   PRE_MEM_RASCIIZ( "quotactl(special)", ARG2 );
}

PRE(sys_waitid)
{
   *flags |= SfMayBlock;
   PRINT("sys_waitid( %ld, %ld, %#lx, %ld, %#lx )", ARG1,ARG2,ARG3,ARG4,ARG5);
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

PRE(sys_sync_file_range)
{
   *flags |= SfMayBlock;
#if VG_WORDSIZE == 4
   PRINT("sys_sync_file_range ( %ld, %lld, %lld, %ld )",
         ARG1,MERGE64(ARG2,ARG3),MERGE64(ARG4,ARG5),ARG6);
   PRE_REG_READ6(long, "sync_file_range",
                 int, fd,
                 unsigned, MERGE64_FIRST(offset), unsigned, MERGE64_SECOND(offset),
                 unsigned, MERGE64_FIRST(nbytes), unsigned, MERGE64_SECOND(nbytes),
                 unsigned int, flags);
#elif VG_WORDSIZE == 8
   PRINT("sys_sync_file_range ( %ld, %lld, %lld, %ld )",
         ARG1,(Long)ARG2,(Long)ARG3,ARG4);
   PRE_REG_READ4(long, "sync_file_range",
                 int, fd, vki_loff_t, offset, vki_loff_t, nbytes,
                 unsigned int, flags);
#else
#  error Unexpected word size
#endif
   if (!ML_(fd_allowed)(ARG1, "sync_file_range", tid, False))
      SET_STATUS_Failure( VKI_EBADF );
}

PRE(sys_sync_file_range2)
{
   *flags |= SfMayBlock;
#if VG_WORDSIZE == 4
   PRINT("sys_sync_file_range2 ( %ld, %ld, %lld, %lld )",
         ARG1,ARG2,MERGE64(ARG3,ARG4),MERGE64(ARG5,ARG6));
   PRE_REG_READ6(long, "sync_file_range2",
                 int, fd, unsigned int, flags,
                 unsigned, MERGE64_FIRST(offset), unsigned, MERGE64_SECOND(offset),
                 unsigned, MERGE64_FIRST(nbytes), unsigned, MERGE64_SECOND(nbytes));
#elif VG_WORDSIZE == 8
   PRINT("sys_sync_file_range2 ( %ld, %ld, %lld, %lld )",
         ARG1,ARG2,(Long)ARG3,(Long)ARG4);
   PRE_REG_READ4(long, "sync_file_range2",
                 int, fd, unsigned int, flags,
                 vki_loff_t, offset, vki_loff_t, nbytes);
#else
#  error Unexpected word size
#endif
   if (!ML_(fd_allowed)(ARG1, "sync_file_range2", tid, False))
      SET_STATUS_Failure( VKI_EBADF );
}

PRE(sys_stime)
{
   PRINT("sys_stime ( %#lx )", ARG1);
   PRE_REG_READ1(int, "stime", vki_time_t*, t);
   PRE_MEM_READ( "stime(t)", ARG1, sizeof(vki_time_t) );
}

PRE(sys_perf_counter_open)
{
   PRINT("sys_perf_counter_open ( %#lx, %ld, %ld, %ld, %ld )",
         ARG1,ARG2,ARG3,ARG4,ARG5);
   PRE_REG_READ5(long, "perf_counter_open",
                 struct vki_perf_counter_attr *, attr,
                 vki_pid_t, pid, int, cpu, int, group_fd,
                 unsigned long, flags);
   PRE_MEM_READ( "perf_counter_open(attr)",
                 ARG1, sizeof(struct vki_perf_counter_attr) );
}

POST(sys_perf_counter_open)
{
   vg_assert(SUCCESS);
   if (!ML_(fd_allowed)(RES, "perf_counter_open", tid, True)) {
      VG_(close)(RES);
      SET_STATUS_Failure( VKI_EMFILE );
   } else {
      if (VG_(clo_track_fds))
         ML_(record_fd_open_nameless)(tid, RES);
   }
}

PRE(sys_getcpu)
{
   PRINT("sys_getcpu ( %#lx, %#lx, %#lx )" , ARG1,ARG2,ARG3);
   PRE_REG_READ3(int, "getcpu", 
                 unsigned *, cpu, unsigned *, node, struct vki_getcpu_cache *, tcache);
   if (ARG1 != 0)
      PRE_MEM_WRITE( "getcpu(cpu)", ARG1, sizeof(unsigned) );
   if (ARG2 != 0)
      PRE_MEM_WRITE( "getcpu(node)", ARG2, sizeof(unsigned) );
   if (ARG3 != 0)
      PRE_MEM_WRITE( "getcpu(tcache)", ARG3, sizeof(struct vki_getcpu_cache) );
}

POST(sys_getcpu)
{
   if (ARG1 != 0)
      POST_MEM_WRITE( ARG1, sizeof(unsigned) );
   if (ARG2 != 0)
      POST_MEM_WRITE( ARG2, sizeof(unsigned) );
   if (ARG3 != 0)
      POST_MEM_WRITE( ARG3, sizeof(struct vki_getcpu_cache) );
}

/* ---------------------------------------------------------------------
   utime wrapper
   ------------------------------------------------------------------ */

PRE(sys_utime)
{
   *flags |= SfMayBlock;
   PRINT("sys_utime ( %#lx, %#lx )", ARG1,ARG2);
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
   PRINT("sys_lseek ( %ld, %ld, %ld )", ARG1,ARG2,ARG3);
   PRE_REG_READ3(vki_off_t, "lseek",
                 unsigned int, fd, vki_off_t, offset, unsigned int, whence);
}

/* ---------------------------------------------------------------------
   readahead wrapper
   ------------------------------------------------------------------ */

PRE(sys_readahead)
{
   *flags |= SfMayBlock;
#if VG_WORDSIZE == 4
   PRINT("sys_readahead ( %ld, %lld, %ld )", ARG1, MERGE64(ARG2,ARG3), ARG4);
   PRE_REG_READ4(vki_off_t, "readahead",
                 int, fd, unsigned, MERGE64_FIRST(offset),
                 unsigned, MERGE64_SECOND(offset), vki_size_t, count);
#elif VG_WORDSIZE == 8
   PRINT("sys_readahead ( %ld, %lld, %ld )", ARG1, (Long)ARG2, ARG3);
   PRE_REG_READ3(vki_off_t, "readahead",
                 int, fd, vki_loff_t, offset, vki_size_t, count);
#else
#  error Unexpected word size
#endif
   if (!ML_(fd_allowed)(ARG1, "readahead", tid, False))
      SET_STATUS_Failure( VKI_EBADF );
}

/* ---------------------------------------------------------------------
   sig* wrappers
   ------------------------------------------------------------------ */

PRE(sys_sigpending)
{
   PRINT( "sys_sigpending ( %#lx )", ARG1 );
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

   PRINT("sys_sigprocmask ( %ld, %#lx, %#lx )",ARG1,ARG2,ARG3);
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

PRE(sys_signalfd)
{
   PRINT("sys_signalfd ( %d, %#lx, %llu )", (Int)ARG1,ARG2,(ULong)ARG3);
   PRE_REG_READ3(long, "sys_signalfd",
                 int, fd, vki_sigset_t *, sigmask, vki_size_t, sigsetsize);
   PRE_MEM_READ( "signalfd(sigmask)", ARG2, sizeof(vki_sigset_t) );
   if ((int)ARG1 != -1 && !ML_(fd_allowed)(ARG1, "signalfd", tid, False))
      SET_STATUS_Failure( VKI_EBADF );
}
POST(sys_signalfd)
{
   if (!ML_(fd_allowed)(RES, "signalfd", tid, True)) {
      VG_(close)(RES);
      SET_STATUS_Failure( VKI_EMFILE );
   } else {
      if (VG_(clo_track_fds))
         ML_(record_fd_open_nameless) (tid, RES);
   }
}

PRE(sys_signalfd4)
{
   PRINT("sys_signalfd4 ( %d, %#lx, %llu, %ld )", (Int)ARG1,ARG2,(ULong)ARG3,ARG4);
   PRE_REG_READ4(long, "sys_signalfd4",
                 int, fd, vki_sigset_t *, sigmask, vki_size_t, sigsetsize, int, flags);
   PRE_MEM_READ( "signalfd(sigmask)", ARG2, sizeof(vki_sigset_t) );
   if ((int)ARG1 != -1 && !ML_(fd_allowed)(ARG1, "signalfd", tid, False))
      SET_STATUS_Failure( VKI_EBADF );
}
POST(sys_signalfd4)
{
   if (!ML_(fd_allowed)(RES, "signalfd4", tid, True)) {
      VG_(close)(RES);
      SET_STATUS_Failure( VKI_EMFILE );
   } else {
      if (VG_(clo_track_fds))
         ML_(record_fd_open_nameless) (tid, RES);
   }
}


/* ---------------------------------------------------------------------
   rt_sig* wrappers
   ------------------------------------------------------------------ */

PRE(sys_rt_sigaction)
{
   PRINT("sys_rt_sigaction ( %ld, %#lx, %#lx, %ld )", ARG1,ARG2,ARG3,ARG4);
   PRE_REG_READ4(long, "rt_sigaction",
                 int, signum, const struct sigaction *, act,
                 struct sigaction *, oldact, vki_size_t, sigsetsize);

   if (ARG2 != 0) {
      vki_sigaction_toK_t *sa = (vki_sigaction_toK_t *)ARG2;
      PRE_MEM_READ( "rt_sigaction(act->sa_handler)", (Addr)&sa->ksa_handler, sizeof(sa->ksa_handler));
      PRE_MEM_READ( "rt_sigaction(act->sa_mask)", (Addr)&sa->sa_mask, sizeof(sa->sa_mask));
      PRE_MEM_READ( "rt_sigaction(act->sa_flags)", (Addr)&sa->sa_flags, sizeof(sa->sa_flags));
      if (sa->sa_flags & VKI_SA_RESTORER)
         PRE_MEM_READ( "rt_sigaction(act->sa_restorer)", (Addr)&sa->sa_restorer, sizeof(sa->sa_restorer));
   }
   if (ARG3 != 0)
      PRE_MEM_WRITE( "rt_sigaction(oldact)", ARG3, sizeof(vki_sigaction_fromK_t));

   // XXX: doesn't seem right to be calling do_sys_sigaction for
   // sys_rt_sigaction... perhaps this function should be renamed
   // VG_(do_sys_rt_sigaction)()  --njn

   SET_STATUS_from_SysRes(
      VG_(do_sys_sigaction)(ARG1, (const vki_sigaction_toK_t *)ARG2,
                            (vki_sigaction_fromK_t *)ARG3)
   );
}
POST(sys_rt_sigaction)
{
   vg_assert(SUCCESS);
   if (RES == 0 && ARG3 != 0)
      POST_MEM_WRITE( ARG3, sizeof(vki_sigaction_fromK_t));
}

PRE(sys_rt_sigprocmask)
{
   PRINT("sys_rt_sigprocmask ( %ld, %#lx, %#lx, %llu )",ARG1,ARG2,ARG3,(ULong)ARG4);
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
   PRINT( "sys_rt_sigpending ( %#lx )", ARG1 );
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
   PRINT("sys_rt_sigtimedwait ( %#lx, %#lx, %#lx, %lld )",
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
   PRINT("sys_rt_sigqueueinfo(%ld, %ld, %#lx)", ARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "rt_sigqueueinfo", 
                 int, pid, int, sig, vki_siginfo_t *, uinfo);
   if (ARG2 != 0)
      PRE_MEM_READ( "rt_sigqueueinfo(uinfo)", ARG3, VKI_SI_MAX_SIZE );
}
POST(sys_rt_sigqueueinfo)
{
   if (!ML_(client_signal_OK)(ARG2))
      SET_STATUS_Failure( VKI_EINVAL );
}

PRE(sys_rt_tgsigqueueinfo)
{
   PRINT("sys_rt_tgsigqueueinfo(%ld, %ld, %ld, %#lx)", ARG1, ARG2, ARG3, ARG4);
   PRE_REG_READ4(long, "rt_tgsigqueueinfo",
                 int, tgid, int, pid, int, sig, vki_siginfo_t *, uinfo);
   if (ARG3 != 0)
      PRE_MEM_READ( "rt_tgsigqueueinfo(uinfo)", ARG4, VKI_SI_MAX_SIZE );
}

POST(sys_rt_tgsigqueueinfo)
{
   if (!ML_(client_signal_OK)(ARG3))
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
   PRINT("sys_rt_sigsuspend ( %#lx, %ld )", ARG1,ARG2 );
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
      PRINT("sys_openat ( %ld, %#lx(%s), %ld, %ld )",ARG1,ARG2,(char*)ARG2,ARG3,ARG4);
      PRE_REG_READ4(long, "openat",
                    int, dfd, const char *, filename, int, flags, int, mode);
   } else {
      // 3-arg version
      PRINT("sys_openat ( %ld, %#lx(%s), %ld )",ARG1,ARG2,(char*)ARG2,ARG3);
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
      if (!sr_isError(sres)) {
         OffT off = VG_(lseek)( sr_Res(sres), 0, VKI_SEEK_SET );
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
   PRINT("sys_mkdirat ( %ld, %#lx(%s), %ld )", ARG1,ARG2,(char*)ARG2,ARG3);
   PRE_REG_READ3(long, "mkdirat",
                 int, dfd, const char *, pathname, int, mode);
   PRE_MEM_RASCIIZ( "mkdirat(pathname)", ARG2 );
}

PRE(sys_mknodat)
{
  PRINT("sys_mknodat ( %ld, %#lx(%s), 0x%lx, 0x%lx )", ARG1,ARG2,(char*)ARG2,ARG3,ARG4 );
   PRE_REG_READ4(long, "mknodat",
                 int, dfd, const char *, pathname, int, mode, unsigned, dev);
   PRE_MEM_RASCIIZ( "mknodat(pathname)", ARG2 );
}

PRE(sys_fchownat)
{
   PRINT("sys_fchownat ( %ld, %#lx(%s), 0x%lx, 0x%lx )", ARG1,ARG2,(char*)ARG2,ARG3,ARG4);
   PRE_REG_READ4(long, "fchownat",
                 int, dfd, const char *, path,
                 vki_uid_t, owner, vki_gid_t, group);
   PRE_MEM_RASCIIZ( "fchownat(path)", ARG2 );
}

PRE(sys_futimesat)
{
   PRINT("sys_futimesat ( %ld, %#lx(%s), %#lx )", ARG1,ARG2,(char*)ARG2,ARG3);
   PRE_REG_READ3(long, "futimesat",
                 int, dfd, char *, filename, struct timeval *, tvp);
   if (ARG2 != 0)
      PRE_MEM_RASCIIZ( "futimesat(filename)", ARG2 );
   if (ARG3 != 0)
      PRE_MEM_READ( "futimesat(tvp)", ARG3, 2 * sizeof(struct vki_timeval) );
}

PRE(sys_utimensat)
{
   PRINT("sys_utimensat ( %ld, %#lx(%s), %#lx, 0x%lx )", ARG1,ARG2,(char*)ARG2,ARG3,ARG4);
   PRE_REG_READ4(long, "utimensat",
                 int, dfd, char *, filename, struct timespec *, utimes, int, flags);
   if (ARG2 != 0)
      PRE_MEM_RASCIIZ( "utimensat(filename)", ARG2 );
   if (ARG3 != 0)
      PRE_MEM_READ( "utimensat(tvp)", ARG3, 2 * sizeof(struct vki_timespec) );
}

PRE(sys_newfstatat)
{
   PRINT("sys_newfstatat ( %ld, %#lx(%s), %#lx )", ARG1,ARG2,(char*)ARG2,ARG3);
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
   PRINT("sys_unlinkat ( %ld, %#lx(%s) )", ARG1,ARG2,(char*)ARG2);
   PRE_REG_READ2(long, "unlinkat", int, dfd, const char *, pathname);
   PRE_MEM_RASCIIZ( "unlinkat(pathname)", ARG2 );
}

PRE(sys_renameat)
{
   PRINT("sys_renameat ( %ld, %#lx(%s), %ld, %#lx(%s) )", ARG1,ARG2,(char*)ARG2,ARG3,ARG4,(char*)ARG4);
   PRE_REG_READ4(long, "renameat",
                 int, olddfd, const char *, oldpath,
                 int, newdfd, const char *, newpath);
   PRE_MEM_RASCIIZ( "renameat(oldpath)", ARG2 );
   PRE_MEM_RASCIIZ( "renameat(newpath)", ARG4 );
}

PRE(sys_linkat)
{
   *flags |= SfMayBlock;
   PRINT("sys_linkat ( %ld, %#lx(%s), %ld, %#lx(%s), %ld )",ARG1,ARG2,(char*)ARG2,ARG3,ARG4,(char*)ARG4,ARG5);
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
   PRINT("sys_symlinkat ( %#lx(%s), %ld, %#lx(%s) )",ARG1,(char*)ARG1,ARG2,ARG3,(char*)ARG3);
   PRE_REG_READ3(long, "symlinkat",
                 const char *, oldpath, int, newdfd, const char *, newpath);
   PRE_MEM_RASCIIZ( "symlinkat(oldpath)", ARG1 );
   PRE_MEM_RASCIIZ( "symlinkat(newpath)", ARG3 );
}

PRE(sys_readlinkat)
{
   HChar name[25];
   Word  saved = SYSNO;

   PRINT("sys_readlinkat ( %ld, %#lx(%s), %#lx, %llu )", ARG1,ARG2,(char*)ARG2,ARG3,(ULong)ARG4);
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
   PRINT("sys_fchmodat ( %ld, %#lx(%s), %ld )", ARG1,ARG2,(char*)ARG2,ARG3);
   PRE_REG_READ3(long, "fchmodat",
                 int, dfd, const char *, path, vki_mode_t, mode);
   PRE_MEM_RASCIIZ( "fchmodat(path)", ARG2 );
}

PRE(sys_faccessat)
{
   PRINT("sys_faccessat ( %ld, %#lx(%s), %ld )", ARG1,ARG2,(char*)ARG2,ARG3);
   PRE_REG_READ3(long, "faccessat",
                 int, dfd, const char *, pathname, int, mode);
   PRE_MEM_RASCIIZ( "faccessat(pathname)", ARG2 );
}

/* ---------------------------------------------------------------------
   p{read,write}v wrappers
   ------------------------------------------------------------------ */

PRE(sys_preadv)
{
   Int i;
   struct vki_iovec * vec;
   *flags |= SfMayBlock;
#if VG_WORDSIZE == 4
   /* Note that the offset argument here is in lo+hi order on both
      big and little endian platforms... */
   PRINT("sys_preadv ( %ld, %#lx, %llu, %lld )",ARG1,ARG2,(ULong)ARG3,LOHI64(ARG4,ARG5));
   PRE_REG_READ5(ssize_t, "preadv",
                 unsigned long, fd, const struct iovec *, vector,
                 unsigned long, count, vki_u32, offset_low,
                 vki_u32, offset_high);
#elif VG_WORDSIZE == 8
   PRINT("sys_preadv ( %ld, %#lx, %llu, %lld )",ARG1,ARG2,(ULong)ARG3,(Long)ARG4);
   PRE_REG_READ4(ssize_t, "preadv",
                 unsigned long, fd, const struct iovec *, vector,
                 unsigned long, count, Word, offset);
#else
#  error Unexpected word size
#endif
   if (!ML_(fd_allowed)(ARG1, "preadv", tid, False)) {
      SET_STATUS_Failure( VKI_EBADF );
   } else {
      PRE_MEM_READ( "preadv(vector)", ARG2, ARG3 * sizeof(struct vki_iovec) );

      if (ARG2 != 0) {
         /* ToDo: don't do any of the following if the vector is invalid */
         vec = (struct vki_iovec *)ARG2;
         for (i = 0; i < (Int)ARG3; i++)
            PRE_MEM_WRITE( "preadv(vector[...])",
                           (Addr)vec[i].iov_base, vec[i].iov_len );
      }
   }
}

POST(sys_preadv)
{
   vg_assert(SUCCESS);
   if (RES > 0) {
      Int i;
      struct vki_iovec * vec = (struct vki_iovec *)ARG2;
      Int remains = RES;

      /* RES holds the number of bytes read. */
      for (i = 0; i < (Int)ARG3; i++) {
	 Int nReadThisBuf = vec[i].iov_len;
	 if (nReadThisBuf > remains) nReadThisBuf = remains;
	 POST_MEM_WRITE( (Addr)vec[i].iov_base, nReadThisBuf );
	 remains -= nReadThisBuf;
	 if (remains < 0) VG_(core_panic)("preadv: remains < 0");
      }
   }
}

PRE(sys_pwritev)
{
   Int i;
   struct vki_iovec * vec;
   *flags |= SfMayBlock;
#if VG_WORDSIZE == 4
   /* Note that the offset argument here is in lo+hi order on both
      big and little endian platforms... */
   PRINT("sys_pwritev ( %ld, %#lx, %llu, %lld )",ARG1,ARG2,(ULong)ARG3,LOHI64(ARG4,ARG5));
   PRE_REG_READ5(ssize_t, "pwritev",
                 unsigned long, fd, const struct iovec *, vector,
                 unsigned long, count, vki_u32, offset_low,
                 vki_u32, offset_high);
#elif VG_WORDSIZE == 8
   PRINT("sys_pwritev ( %ld, %#lx, %llu, %lld )",ARG1,ARG2,(ULong)ARG3,(Long)ARG4);
   PRE_REG_READ4(ssize_t, "pwritev",
                 unsigned long, fd, const struct iovec *, vector,
                 unsigned long, count, Word, offset);
#else
#  error Unexpected word size
#endif
   if (!ML_(fd_allowed)(ARG1, "pwritev", tid, False)) {
      SET_STATUS_Failure( VKI_EBADF );
   } else {
      PRE_MEM_READ( "pwritev(vector)", 
		     ARG2, ARG3 * sizeof(struct vki_iovec) );
      if (ARG2 != 0) {
         /* ToDo: don't do any of the following if the vector is invalid */
         vec = (struct vki_iovec *)ARG2;
         for (i = 0; i < (Int)ARG3; i++)
            PRE_MEM_READ( "pwritev(vector[...])",
                           (Addr)vec[i].iov_base, vec[i].iov_len );
      }
   }
}

/* ---------------------------------------------------------------------
   key retention service wrappers
   ------------------------------------------------------------------ */

PRE(sys_request_key)
{
   PRINT("sys_request_key ( %#lx(%s), %#lx(%s), %#lx(%s), %ld )",
         ARG1,(char*)ARG1,ARG2,(char*)ARG2,ARG3,(char*)ARG3,ARG4);
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
   PRINT("sys_add_key ( %#lx(%s), %#lx(%s), %#lx, %ld, %ld )",
         ARG1,(char*)ARG1,ARG2,(char*)ARG2,ARG3,ARG4,ARG5);
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
      PRINT("sys_keyctl ( KEYCTL_GET_KEYRING_ID, %ld, %ld )", ARG2,ARG3);
      PRE_REG_READ3(long, "keyctl(KEYCTL_GET_KEYRING_ID)",
                    int, option, vki_key_serial_t, id, int, create);
      break;
   case VKI_KEYCTL_JOIN_SESSION_KEYRING:
      PRINT("sys_keyctl ( KEYCTL_JOIN_SESSION_KEYRING, %#lx(%s) )", ARG2,(char*)ARG2);
      PRE_REG_READ2(long, "keyctl(KEYCTL_JOIN_SESSION_KEYRING)",
                    int, option, const char *, name);
      if (ARG2 != (UWord)NULL)
         PRE_MEM_RASCIIZ("keyctl(KEYCTL_JOIN_SESSION_KEYRING, name)", ARG2);
      break;
   case VKI_KEYCTL_UPDATE:
      PRINT("sys_keyctl ( KEYCTL_UPDATE, %ld, %#lx, %ld )", ARG2,ARG3,ARG4);
      PRE_REG_READ4(long, "keyctl(KEYCTL_UPDATE)",
                    int, option, vki_key_serial_t, key,
                    const void *, payload, vki_size_t, plen);
      if (ARG3 != (UWord)NULL)
         PRE_MEM_READ("keyctl(KEYCTL_UPDATE, payload)", ARG3, ARG4);
      break;
   case VKI_KEYCTL_REVOKE:
      PRINT("sys_keyctl ( KEYCTL_REVOKE, %ld )", ARG2);
      PRE_REG_READ2(long, "keyctl(KEYCTL_REVOKE)",
                    int, option, vki_key_serial_t, id);
      break;
   case VKI_KEYCTL_CHOWN:
      PRINT("sys_keyctl ( KEYCTL_CHOWN, %ld, %ld, %ld )", ARG2,ARG3,ARG4);
      PRE_REG_READ4(long, "keyctl(KEYCTL_CHOWN)",
                    int, option, vki_key_serial_t, id,
                    vki_uid_t, uid, vki_gid_t, gid);
      break;
   case VKI_KEYCTL_SETPERM:
      PRINT("sys_keyctl ( KEYCTL_SETPERM, %ld, %ld )", ARG2,ARG3);
      PRE_REG_READ3(long, "keyctl(KEYCTL_SETPERM)",
                    int, option, vki_key_serial_t, id, vki_key_perm_t, perm);
      break;
   case VKI_KEYCTL_DESCRIBE:
      PRINT("sys_keyctl ( KEYCTL_DESCRIBE, %ld, %#lx, %ld )", ARG2,ARG3,ARG4);
      PRE_REG_READ4(long, "keyctl(KEYCTL_DESCRIBE)",
                    int, option, vki_key_serial_t, id,
                    char *, buffer, vki_size_t, buflen);
      if (ARG3 != (UWord)NULL)
         PRE_MEM_WRITE("keyctl(KEYCTL_DESCRIBE, buffer)", ARG3, ARG4);
      break;
   case VKI_KEYCTL_CLEAR:
      PRINT("sys_keyctl ( KEYCTL_CLEAR, %ld )", ARG2);
      PRE_REG_READ2(long, "keyctl(KEYCTL_CLEAR)",
                    int, option, vki_key_serial_t, keyring);
      break;
   case VKI_KEYCTL_LINK:
      PRINT("sys_keyctl ( KEYCTL_LINK, %ld, %ld )", ARG2,ARG3);
      PRE_REG_READ3(long, "keyctl(KEYCTL_LINK)", int, option,
                    vki_key_serial_t, keyring, vki_key_serial_t, key);
      break;
   case VKI_KEYCTL_UNLINK:
      PRINT("sys_keyctl ( KEYCTL_UNLINK, %ld, %ld )", ARG2,ARG3);
      PRE_REG_READ3(long, "keyctl(KEYCTL_UNLINK)", int, option,
                    vki_key_serial_t, keyring, vki_key_serial_t, key);
      break;
   case VKI_KEYCTL_SEARCH:
      PRINT("sys_keyctl ( KEYCTL_SEARCH, %ld, %#lx(%s), %#lx(%s), %ld )",
            ARG2,ARG3,(char*)ARG3,ARG4,(char*)ARG4,ARG5);
      PRE_REG_READ5(long, "keyctl(KEYCTL_SEARCH)",
                    int, option, vki_key_serial_t, keyring, 
                    const char *, type, const char *, description,
                    vki_key_serial_t, destring);
      PRE_MEM_RASCIIZ("sys_keyctl(KEYCTL_SEARCH, type)", ARG3);
      PRE_MEM_RASCIIZ("sys_keyctl(KEYCTL_SEARCH, description)", ARG4);
      break;
   case VKI_KEYCTL_READ:
      PRINT("sys_keyctl ( KEYCTL_READ, %ld, %#lx, %ld )", ARG2,ARG3,ARG4);
      PRE_REG_READ4(long, "keyctl(KEYCTL_READ)",
                    int, option, vki_key_serial_t, keyring, 
                    char *, buffer, vki_size_t, buflen);
      if (ARG3 != (UWord)NULL)
         PRE_MEM_WRITE("keyctl(KEYCTL_READ, buffer)", ARG3, ARG4);
      break;
   case VKI_KEYCTL_INSTANTIATE:
      PRINT("sys_keyctl ( KEYCTL_INSTANTIATE, %ld, %#lx, %ld, %ld )",
            ARG2,ARG3,ARG4,ARG5);
      PRE_REG_READ5(long, "keyctl(KEYCTL_INSTANTIATE)",
                    int, option, vki_key_serial_t, key, 
                    char *, payload, vki_size_t, plen,
                    vki_key_serial_t, keyring);
      if (ARG3 != (UWord)NULL)
         PRE_MEM_READ("keyctl(KEYCTL_INSTANTIATE, payload)", ARG3, ARG4);
      break;
   case VKI_KEYCTL_NEGATE:
      PRINT("sys_keyctl ( KEYCTL_NEGATE, %ld, %lu, %ld )", ARG2,ARG3,ARG4);
      PRE_REG_READ4(long, "keyctl(KEYCTL_NEGATE)",
                    int, option, vki_key_serial_t, key, 
                    unsigned, timeout, vki_key_serial_t, keyring);
      break;
   case VKI_KEYCTL_SET_REQKEY_KEYRING:
      PRINT("sys_keyctl ( KEYCTL_SET_REQKEY_KEYRING, %ld )", ARG2);
      PRE_REG_READ2(long, "keyctl(KEYCTL_SET_REQKEY_KEYRING)",
                    int, option, int, reqkey_defl);
      break;
   case VKI_KEYCTL_SET_TIMEOUT:
      PRINT("sys_keyctl ( KEYCTL_SET_TIMEOUT, %ld, %ld )", ARG2,ARG3);
      PRE_REG_READ3(long, "keyctl(KEYCTL_SET_TIMEOUT)",
                    int, option, vki_key_serial_t, key, unsigned, timeout);
      break;
   case VKI_KEYCTL_ASSUME_AUTHORITY:
      PRINT("sys_keyctl ( KEYCTL_ASSUME_AUTHORITY, %ld )", ARG2);
      PRE_REG_READ2(long, "keyctl(KEYCTL_ASSUME_AUTHORITY)",
                    int, option, vki_key_serial_t, key);
      break;
   default:
      PRINT("sys_keyctl ( %ld ) ", ARG1);
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

PRE(sys_ioprio_set)
{
   PRINT("sys_ioprio_set ( %ld, %ld, %ld )", ARG1,ARG2,ARG3);
   PRE_REG_READ3(int, "ioprio_set", int, which, int, who, int, ioprio);
}

PRE(sys_ioprio_get)
{
   PRINT("sys_ioprio_get ( %ld, %ld )", ARG1,ARG2);
   PRE_REG_READ2(int, "ioprio_get", int, which, int, who);
}

/* ---------------------------------------------------------------------
   _module wrappers
   ------------------------------------------------------------------ */

PRE(sys_init_module)
{
   *flags |= SfMayBlock;
   PRINT("sys_init_module ( %#lx, %llu, %#lx(\"%s\") )",
         ARG1, (ULong)ARG2, ARG3, (char*)ARG3);
   PRE_REG_READ3(long, "init_module",
                 void *, umod, unsigned long, len, const char *, uargs);
   PRE_MEM_READ( "init_module(umod)", ARG1, ARG2 );
   PRE_MEM_RASCIIZ( "init_module(uargs)", ARG3 );
}

PRE(sys_delete_module)
{
   *flags |= SfMayBlock;
   PRINT("sys_delete_module ( %#lx(\"%s\"), 0x%lx )", ARG1,(char*)ARG1, ARG2);
   PRE_REG_READ2(long, "delete_module",
                 const char *, name_user, unsigned int, flags);
   PRE_MEM_RASCIIZ("delete_module(name_user)", ARG1);
}

/* ---------------------------------------------------------------------
   splice wrappers
   ------------------------------------------------------------------ */

PRE(sys_splice)
{
   *flags |= SfMayBlock;
   PRINT("sys_splice ( %ld, %#lx, %ld, %#lx, %ld, %ld )",
         ARG1,ARG2,ARG3,ARG4,ARG5,ARG6);
   PRE_REG_READ6(int32_t, "splice",
                 int, fd_in, vki_loff_t *, off_in,
                 int, fd_out, vki_loff_t *, off_out,
                 vki_size_t, len, unsigned int, flags);
   if (!ML_(fd_allowed)(ARG1, "splice(fd_in)", tid, False) ||
       !ML_(fd_allowed)(ARG3, "splice(fd_out)", tid, False)) {
      SET_STATUS_Failure( VKI_EBADF );
   } else {
      if (ARG2 != 0)
         PRE_MEM_READ( "splice(off_in)", ARG2, sizeof(vki_loff_t));
      if (ARG4 != 0)
         PRE_MEM_READ( "splice(off_out)", ARG4, sizeof(vki_loff_t));
   }
}

/* ---------------------------------------------------------------------
   oprofile-related wrappers
   ------------------------------------------------------------------ */

#if defined(VGP_x86_linux)
PRE(sys_lookup_dcookie)
{
   PRINT("sys_lookup_dcookie (0x%llx, %#lx, %ld)",
         MERGE64(ARG1,ARG2), ARG3, ARG4);
   PRE_REG_READ4(long, "lookup_dcookie",
                 vki_u32, MERGE64_FIRST(cookie), vki_u32, MERGE64_SECOND(cookie),
                 char *, buf, vki_size_t, len);
   PRE_MEM_WRITE( "lookup_dcookie(buf)", ARG3, ARG4);
}
POST(sys_lookup_dcookie)
{
   vg_assert(SUCCESS);
   if (ARG3 != (Addr)NULL)
      POST_MEM_WRITE( ARG3, RES);
}
#endif

#if defined(VGP_amd64_linux) || defined(VGP_s390x_linux)
PRE(sys_lookup_dcookie)
{
   *flags |= SfMayBlock;
   PRINT("sys_lookup_dcookie ( %llu, %#lx, %llu )",
	 (ULong)ARG1, ARG2, (ULong)ARG3);
   PRE_REG_READ3(int, "lookup_dcookie",
                 unsigned long long, cookie, char *, buf, vki_size_t, len);

   PRE_MEM_WRITE( "sys_lookup_dcookie(buf)", ARG2, ARG3 );
}

POST(sys_lookup_dcookie)
{
   vg_assert(SUCCESS);
   if (ARG2 != (Addr)NULL)
     POST_MEM_WRITE( ARG2, RES );
}
#endif

/* ---------------------------------------------------------------------
   fcntl wrappers
   ------------------------------------------------------------------ */

PRE(sys_fcntl)
{
   switch (ARG2) {
   // These ones ignore ARG3.
   case VKI_F_GETFD:
   case VKI_F_GETFL:
   case VKI_F_GETOWN:
   case VKI_F_GETSIG:
   case VKI_F_GETLEASE:
      PRINT("sys_fcntl ( %ld, %ld )", ARG1,ARG2);
      PRE_REG_READ2(long, "fcntl", unsigned int, fd, unsigned int, cmd);
      break;

   // These ones use ARG3 as "arg".
   case VKI_F_DUPFD:
   case VKI_F_DUPFD_CLOEXEC:
   case VKI_F_SETFD:
   case VKI_F_SETFL:
   case VKI_F_SETLEASE:
   case VKI_F_NOTIFY:
   case VKI_F_SETOWN:
   case VKI_F_SETSIG:
      PRINT("sys_fcntl[ARG3=='arg'] ( %ld, %ld, %ld )", ARG1,ARG2,ARG3);
      PRE_REG_READ3(long, "fcntl",
                    unsigned int, fd, unsigned int, cmd, unsigned long, arg);
      break;

   // These ones use ARG3 as "lock".
   case VKI_F_GETLK:
   case VKI_F_SETLK:
   case VKI_F_SETLKW:
#  if defined(VGP_x86_linux)
   case VKI_F_GETLK64:
   case VKI_F_SETLK64:
   case VKI_F_SETLKW64:
#  endif
      PRINT("sys_fcntl[ARG3=='lock'] ( %ld, %ld, %#lx )", ARG1,ARG2,ARG3);
      PRE_REG_READ3(long, "fcntl",
                    unsigned int, fd, unsigned int, cmd,
                    struct flock64 *, lock);
      break;

   default:
      PRINT("sys_fcntl[UNKNOWN] ( %ld, %ld, %ld )", ARG1,ARG2,ARG3);
      I_die_here;
      break;
   }

#  if defined(VGP_x86_linux)
   if (ARG2 == VKI_F_SETLKW || ARG2 == VKI_F_SETLKW64)
#  else
   if (ARG2 == VKI_F_SETLKW)
#  endif
      *flags |= SfMayBlock;
}

POST(sys_fcntl)
{
   vg_assert(SUCCESS);
   if (ARG2 == VKI_F_DUPFD) {
      if (!ML_(fd_allowed)(RES, "fcntl(DUPFD)", tid, True)) {
         VG_(close)(RES);
         SET_STATUS_Failure( VKI_EMFILE );
      } else {
         if (VG_(clo_track_fds))
            ML_(record_fd_open_named)(tid, RES);
      }
   }
   else if (ARG2 == VKI_F_DUPFD_CLOEXEC) {
      if (!ML_(fd_allowed)(RES, "fcntl(DUPFD_CLOEXEC)", tid, True)) {
         VG_(close)(RES);
         SET_STATUS_Failure( VKI_EMFILE );
      } else {
         if (VG_(clo_track_fds))
            ML_(record_fd_open_named)(tid, RES);
      }
   }
}

// XXX: wrapper only suitable for 32-bit systems
PRE(sys_fcntl64)
{
   switch (ARG2) {
   // These ones ignore ARG3.
   case VKI_F_GETFD:
   case VKI_F_GETFL:
   case VKI_F_GETOWN:
   case VKI_F_SETOWN:
   case VKI_F_GETSIG:
   case VKI_F_SETSIG:
   case VKI_F_GETLEASE:
      PRINT("sys_fcntl64 ( %ld, %ld )", ARG1,ARG2);
      PRE_REG_READ2(long, "fcntl64", unsigned int, fd, unsigned int, cmd);
      break;

   // These ones use ARG3 as "arg".
   case VKI_F_DUPFD:
   case VKI_F_DUPFD_CLOEXEC:
   case VKI_F_SETFD:
   case VKI_F_SETFL:
   case VKI_F_SETLEASE:
   case VKI_F_NOTIFY:
      PRINT("sys_fcntl64[ARG3=='arg'] ( %ld, %ld, %ld )", ARG1,ARG2,ARG3);
      PRE_REG_READ3(long, "fcntl64",
                    unsigned int, fd, unsigned int, cmd, unsigned long, arg);
      break;

   // These ones use ARG3 as "lock".
   case VKI_F_GETLK:
   case VKI_F_SETLK:
   case VKI_F_SETLKW:
#  if defined(VGP_x86_linux)
   case VKI_F_GETLK64:
   case VKI_F_SETLK64:
   case VKI_F_SETLKW64:
#  endif
      PRINT("sys_fcntl64[ARG3=='lock'] ( %ld, %ld, %#lx )", ARG1,ARG2,ARG3);
      PRE_REG_READ3(long, "fcntl64",
                    unsigned int, fd, unsigned int, cmd,
                    struct flock64 *, lock);
      break;
   }
   
#  if defined(VGP_x86_linux)
   if (ARG2 == VKI_F_SETLKW || ARG2 == VKI_F_SETLKW64)
#  else
   if (ARG2 == VKI_F_SETLKW)
#  endif
      *flags |= SfMayBlock;
}

POST(sys_fcntl64)
{
   vg_assert(SUCCESS);
   if (ARG2 == VKI_F_DUPFD) {
      if (!ML_(fd_allowed)(RES, "fcntl64(DUPFD)", tid, True)) {
         VG_(close)(RES);
         SET_STATUS_Failure( VKI_EMFILE );
      } else {
         if (VG_(clo_track_fds))
            ML_(record_fd_open_named)(tid, RES);
      }
   }
   else if (ARG2 == VKI_F_DUPFD_CLOEXEC) {
      if (!ML_(fd_allowed)(RES, "fcntl64(DUPFD_CLOEXEC)", tid, True)) {
         VG_(close)(RES);
         SET_STATUS_Failure( VKI_EMFILE );
      } else {
         if (VG_(clo_track_fds))
            ML_(record_fd_open_named)(tid, RES);
      }
   }
}

/* ---------------------------------------------------------------------
   ioctl wrappers
   ------------------------------------------------------------------ */

PRE(sys_ioctl)
{
   *flags |= SfMayBlock;

   // We first handle the ones that don't use ARG3 (even as a
   // scalar/non-pointer argument).
   switch (ARG2 /* request */) {

      /* linux/soundcard interface (ALSA) */
   case VKI_SNDRV_PCM_IOCTL_HW_FREE:
   case VKI_SNDRV_PCM_IOCTL_HWSYNC:
   case VKI_SNDRV_PCM_IOCTL_PREPARE:
   case VKI_SNDRV_PCM_IOCTL_RESET:
   case VKI_SNDRV_PCM_IOCTL_START:
   case VKI_SNDRV_PCM_IOCTL_DROP:
   case VKI_SNDRV_PCM_IOCTL_DRAIN:
   case VKI_SNDRV_PCM_IOCTL_RESUME:
   case VKI_SNDRV_PCM_IOCTL_XRUN:
   case VKI_SNDRV_PCM_IOCTL_UNLINK:
   case VKI_SNDRV_TIMER_IOCTL_START:
   case VKI_SNDRV_TIMER_IOCTL_STOP:
   case VKI_SNDRV_TIMER_IOCTL_CONTINUE:
   case VKI_SNDRV_TIMER_IOCTL_PAUSE:
      PRINT("sys_ioctl ( %ld, 0x%lx )",ARG1,ARG2);
      PRE_REG_READ2(long, "ioctl",
                    unsigned int, fd, unsigned int, request);
      return;

   default:
      PRINT("sys_ioctl ( %ld, 0x%lx, 0x%lx )",ARG1,ARG2,ARG3);
      PRE_REG_READ3(long, "ioctl",
                    unsigned int, fd, unsigned int, request, unsigned long, arg);
      break;
   }

   // We now handle those that do look at ARG3 (and unknown ones fall into
   // this category).  Nb: some of these may well belong in the
   // doesn't-use-ARG3 switch above.
   switch (ARG2 /* request */) {
   case VKI_TCSETS:
   case VKI_TCSETSW:
   case VKI_TCSETSF:
      PRE_MEM_READ( "ioctl(TCSET{S,SW,SF})", ARG3, sizeof(struct vki_termios) );
      break; 
   case VKI_TCGETS:
      PRE_MEM_WRITE( "ioctl(TCGETS)", ARG3, sizeof(struct vki_termios) );
      break;
   case VKI_TCSETA:
   case VKI_TCSETAW:
   case VKI_TCSETAF:
      PRE_MEM_READ( "ioctl(TCSET{A,AW,AF})", ARG3, sizeof(struct vki_termio) );
      break;
   case VKI_TCGETA:
      PRE_MEM_WRITE( "ioctl(TCGETA)", ARG3, sizeof(struct vki_termio) );
      break;
   case VKI_TCSBRK:
   case VKI_TCXONC:
   case VKI_TCSBRKP:
   case VKI_TCFLSH:
      /* These just take an int by value */
      break;
   case VKI_TIOCGWINSZ:
      PRE_MEM_WRITE( "ioctl(TIOCGWINSZ)", ARG3, sizeof(struct vki_winsize) );
      break;
   case VKI_TIOCSWINSZ:
      PRE_MEM_READ( "ioctl(TIOCSWINSZ)",  ARG3, sizeof(struct vki_winsize) );
      break;
   case VKI_TIOCMBIS:
      PRE_MEM_READ( "ioctl(TIOCMBIS)",    ARG3, sizeof(unsigned int) );
      break;
   case VKI_TIOCMBIC:
      PRE_MEM_READ( "ioctl(TIOCMBIC)",    ARG3, sizeof(unsigned int) );
      break;
   case VKI_TIOCMSET:
      PRE_MEM_READ( "ioctl(TIOCMSET)",    ARG3, sizeof(unsigned int) );
      break;
   case VKI_TIOCMGET:
      PRE_MEM_WRITE( "ioctl(TIOCMGET)",   ARG3, sizeof(unsigned int) );
      break;
   case VKI_TIOCLINUX:
      PRE_MEM_READ( "ioctl(TIOCLINUX)",   ARG3, sizeof(char *) );
      if (*(char *)ARG3 == 11) {
	 PRE_MEM_READ( "ioctl(TIOCLINUX, 11)", ARG3, 2 * sizeof(char *) );
      }
      break;
   case VKI_TIOCGPGRP:
      /* Get process group ID for foreground processing group. */
      PRE_MEM_WRITE( "ioctl(TIOCGPGRP)", ARG3, sizeof(vki_pid_t) );
      break;
   case VKI_TIOCSPGRP:
      /* Set a process group ID? */
      PRE_MEM_WRITE( "ioctl(TIOCGPGRP)", ARG3, sizeof(vki_pid_t) );
      break;
   case VKI_TIOCGPTN: /* Get Pty Number (of pty-mux device) */
      PRE_MEM_WRITE( "ioctl(TIOCGPTN)", ARG3, sizeof(int) );
      break;
   case VKI_TIOCSCTTY:
      /* Just takes an int value.  */
      break;
   case VKI_TIOCSPTLCK: /* Lock/unlock Pty */
      PRE_MEM_READ( "ioctl(TIOCSPTLCK)", ARG3, sizeof(int) );
      break;
   case VKI_FIONBIO:
      PRE_MEM_READ( "ioctl(FIONBIO)",    ARG3, sizeof(int) );
      break;
   case VKI_FIOASYNC:
      PRE_MEM_READ( "ioctl(FIOASYNC)",   ARG3, sizeof(int) );
      break;
   case VKI_FIONREAD:                /* identical to SIOCINQ */
      PRE_MEM_WRITE( "ioctl(FIONREAD)",  ARG3, sizeof(int) );
      break;

   case VKI_TIOCSERGETLSR:
      PRE_MEM_WRITE( "ioctl(TIOCSERGETLSR)", ARG3, sizeof(int) );
      break;
   case VKI_TIOCGICOUNT:
      PRE_MEM_WRITE( "ioctl(TIOCGICOUNT)", ARG3,
                     sizeof(struct vki_serial_icounter_struct) );
      break;

   case VKI_SG_SET_COMMAND_Q:
      PRE_MEM_READ( "ioctl(SG_SET_COMMAND_Q)", ARG3, sizeof(int) );
      break;
   case VKI_SG_IO:
      PRE_MEM_WRITE( "ioctl(SG_IO)", ARG3, sizeof(vki_sg_io_hdr_t) );
      break;
   case VKI_SG_GET_SCSI_ID:
      PRE_MEM_WRITE( "ioctl(SG_GET_SCSI_ID)", ARG3, sizeof(vki_sg_scsi_id_t) );
      break;
   case VKI_SG_SET_RESERVED_SIZE:
      PRE_MEM_READ( "ioctl(SG_SET_RESERVED_SIZE)", ARG3, sizeof(int) );
      break;
   case VKI_SG_SET_TIMEOUT:
      PRE_MEM_READ( "ioctl(SG_SET_TIMEOUT)", ARG3, sizeof(int) );
      break;
   case VKI_SG_GET_RESERVED_SIZE:
      PRE_MEM_WRITE( "ioctl(SG_GET_RESERVED_SIZE)", ARG3, sizeof(int) );
      break;
   case VKI_SG_GET_TIMEOUT:
      break;
   case VKI_SG_GET_VERSION_NUM:
      PRE_MEM_WRITE(  "ioctl(SG_GET_VERSION_NUM)",  ARG3, sizeof(int) );
      break;
   case VKI_SG_EMULATED_HOST: /* 0x2203 */
      PRE_MEM_WRITE( "ioctl(SG_EMULATED_HOST)",    ARG3, sizeof(int) );
      break;
   case VKI_SG_GET_SG_TABLESIZE: /* 0x227f */
      PRE_MEM_WRITE( "ioctl(SG_GET_SG_TABLESIZE)", ARG3, sizeof(int) );
      break;

   case VKI_IIOCGETCPS:
      PRE_MEM_WRITE( "ioctl(IIOCGETCPS)", ARG3,
		     VKI_ISDN_MAX_CHANNELS * 2 * sizeof(unsigned long) );
      break;
   case VKI_IIOCNETGPN:
      PRE_MEM_READ( "ioctl(IIOCNETGPN)",
		     (Addr)&((vki_isdn_net_ioctl_phone *)ARG3)->name,
		     sizeof(((vki_isdn_net_ioctl_phone *)ARG3)->name) );
      PRE_MEM_WRITE( "ioctl(IIOCNETGPN)", ARG3,
		     sizeof(vki_isdn_net_ioctl_phone) );
      break;

      /* These all use struct ifreq AFAIK */
   case VKI_SIOCGIFINDEX:        /* get iface index              */
      PRE_MEM_RASCIIZ( "ioctl(SIOCGIFINDEX)",
                     (Addr)((struct vki_ifreq *)ARG3)->vki_ifr_name );
      PRE_MEM_WRITE( "ioctl(SIOCGIFINDEX)", ARG3, sizeof(struct vki_ifreq));
      break;
   case VKI_SIOCGIFFLAGS:        /* get flags                    */
      PRE_MEM_RASCIIZ( "ioctl(SIOCGIFFLAGS)",
                     (Addr)((struct vki_ifreq *)ARG3)->vki_ifr_name );
      PRE_MEM_WRITE( "ioctl(SIOCGIFFLAGS)", ARG3, sizeof(struct vki_ifreq));
      break;
   case VKI_SIOCGIFHWADDR:       /* Get hardware address         */
      PRE_MEM_RASCIIZ( "ioctl(SIOCGIFHWADDR)",
                     (Addr)((struct vki_ifreq *)ARG3)->vki_ifr_name );
      PRE_MEM_WRITE( "ioctl(SIOCGIFHWADDR)", ARG3, sizeof(struct vki_ifreq));
      break;
   case VKI_SIOCGIFMTU:          /* get MTU size                 */
      PRE_MEM_RASCIIZ( "ioctl(SIOCGIFMTU)",
                     (Addr)((struct vki_ifreq *)ARG3)->vki_ifr_name );
      PRE_MEM_WRITE( "ioctl(SIOCGIFMTU)", ARG3, sizeof(struct vki_ifreq));
      break;
   case VKI_SIOCGIFADDR:         /* get PA address               */
      PRE_MEM_RASCIIZ( "ioctl(SIOCGIFADDR)",
                     (Addr)((struct vki_ifreq *)ARG3)->vki_ifr_name );
      PRE_MEM_WRITE( "ioctl(SIOCGIFADDR)", ARG3, sizeof(struct vki_ifreq));
      break;
   case VKI_SIOCGIFNETMASK:      /* get network PA mask          */
      PRE_MEM_RASCIIZ( "ioctl(SIOCGIFNETMASK)",
                     (Addr)((struct vki_ifreq *)ARG3)->vki_ifr_name );
      PRE_MEM_WRITE( "ioctl(SIOCGIFNETMASK)", ARG3, sizeof(struct vki_ifreq));
      break;
   case VKI_SIOCGIFMETRIC:       /* get metric                   */
      PRE_MEM_RASCIIZ( "ioctl(SIOCGIFMETRIC)",
                     (Addr)((struct vki_ifreq *)ARG3)->vki_ifr_name );
      PRE_MEM_WRITE( "ioctl(SIOCGIFMETRIC)", ARG3, sizeof(struct vki_ifreq));
      break;
   case VKI_SIOCGIFMAP:          /* Get device parameters        */
      PRE_MEM_RASCIIZ( "ioctl(SIOCGIFMAP)",
                     (Addr)((struct vki_ifreq *)ARG3)->vki_ifr_name );
      PRE_MEM_WRITE( "ioctl(SIOCGIFMAP)", ARG3, sizeof(struct vki_ifreq));
      break;
   case VKI_SIOCGIFTXQLEN:       /* Get the tx queue length      */
      PRE_MEM_RASCIIZ( "ioctl(SIOCGIFTXQLEN)",
                     (Addr)((struct vki_ifreq *)ARG3)->vki_ifr_name );
      PRE_MEM_WRITE( "ioctl(SIOCGIFTXQLEN)", ARG3, sizeof(struct vki_ifreq));
      break;
   case VKI_SIOCGIFDSTADDR:      /* get remote PA address        */
      PRE_MEM_RASCIIZ( "ioctl(SIOCGIFDSTADDR)",
                     (Addr)((struct vki_ifreq *)ARG3)->vki_ifr_name );
      PRE_MEM_WRITE( "ioctl(SIOCGIFDSTADDR)", ARG3, sizeof(struct vki_ifreq));
      break;
   case VKI_SIOCGIFBRDADDR:      /* get broadcast PA address     */
      PRE_MEM_RASCIIZ( "ioctl(SIOCGIFBRDADDR)",
                     (Addr)((struct vki_ifreq *)ARG3)->vki_ifr_name );
      PRE_MEM_WRITE( "ioctl(SIOCGIFBRDADDR)", ARG3, sizeof(struct vki_ifreq));
      break;
   case VKI_SIOCGIFNAME:         /* get iface name               */
      PRE_MEM_READ( "ioctl(SIOCGIFNAME)",
                     (Addr)&((struct vki_ifreq *)ARG3)->vki_ifr_ifindex,
                     sizeof(((struct vki_ifreq *)ARG3)->vki_ifr_ifindex) );
      PRE_MEM_WRITE( "ioctl(SIOCGIFNAME)", ARG3, sizeof(struct vki_ifreq));
      break;
   case VKI_SIOCGMIIPHY:         /* get hardware entry           */
      PRE_MEM_RASCIIZ( "ioctl(SIOCGIFMIIPHY)",
                     (Addr)((struct vki_ifreq *)ARG3)->vki_ifr_name );
      PRE_MEM_WRITE( "ioctl(SIOCGIFMIIPHY)", ARG3, sizeof(struct vki_ifreq));
      break;
   case VKI_SIOCGMIIREG:         /* get hardware entry registers */
      PRE_MEM_RASCIIZ( "ioctl(SIOCGIFMIIREG)",
                     (Addr)((struct vki_ifreq *)ARG3)->vki_ifr_name );
      PRE_MEM_READ( "ioctl(SIOCGIFMIIREG)",
                     (Addr)&((struct vki_mii_ioctl_data *)&((struct vki_ifreq *)ARG3)->vki_ifr_data)->phy_id,
                     sizeof(((struct vki_mii_ioctl_data *)&((struct vki_ifreq *)ARG3)->vki_ifr_data)->phy_id) );
      PRE_MEM_READ( "ioctl(SIOCGIFMIIREG)",
                     (Addr)&((struct vki_mii_ioctl_data *)&((struct vki_ifreq *)ARG3)->vki_ifr_data)->reg_num,
                     sizeof(((struct vki_mii_ioctl_data *)&((struct vki_ifreq *)ARG3)->vki_ifr_data)->reg_num) );
      PRE_MEM_WRITE( "ioctl(SIOCGIFMIIREG)", ARG3, 
		     sizeof(struct vki_ifreq));
      break;
   case VKI_SIOCGIFCONF:         /* get iface list               */
      /* WAS:
	 PRE_MEM_WRITE( "ioctl(SIOCGIFCONF)", ARG3, sizeof(struct ifconf));
	 KERNEL_DO_SYSCALL(tid,RES);
	 if (!VG_(is_kerror)(RES) && RES == 0)
	 POST_MEM_WRITE(ARG3, sizeof(struct ifconf));
      */
      PRE_MEM_READ( "ioctl(SIOCGIFCONF)",
                    (Addr)&((struct vki_ifconf *)ARG3)->ifc_len,
                    sizeof(((struct vki_ifconf *)ARG3)->ifc_len));
      PRE_MEM_READ( "ioctl(SIOCGIFCONF)",
                    (Addr)&((struct vki_ifconf *)ARG3)->vki_ifc_buf,
                    sizeof(((struct vki_ifconf *)ARG3)->vki_ifc_buf));
      if ( ARG3 ) {
	 // TODO len must be readable and writable
	 // buf pointer only needs to be readable
	 struct vki_ifconf *ifc = (struct vki_ifconf *) ARG3;
	 PRE_MEM_WRITE( "ioctl(SIOCGIFCONF).ifc_buf",
			(Addr)(ifc->vki_ifc_buf), ifc->ifc_len );
      }
      break;
   case VKI_SIOCGSTAMP:
      PRE_MEM_WRITE( "ioctl(SIOCGSTAMP)", ARG3, sizeof(struct vki_timeval));
      break;
   case VKI_SIOCGSTAMPNS:
      PRE_MEM_WRITE( "ioctl(SIOCGSTAMPNS)", ARG3, sizeof(struct vki_timespec));
      break;
      /* SIOCOUTQ is an ioctl that, when called on a socket, returns
	 the number of bytes currently in that socket's send buffer.
	 It writes this value as an int to the memory location
	 indicated by the third argument of ioctl(2). */
   case VKI_SIOCOUTQ:
      PRE_MEM_WRITE( "ioctl(SIOCOUTQ)", ARG3, sizeof(int));
      break;
   case VKI_SIOCGRARP:           /* get RARP table entry         */
   case VKI_SIOCGARP:            /* get ARP table entry          */
      PRE_MEM_WRITE( "ioctl(SIOCGARP)", ARG3, sizeof(struct vki_arpreq));
      break;
                    
   case VKI_SIOCSIFFLAGS:        /* set flags                    */
      PRE_MEM_RASCIIZ( "ioctl(SIOCSIFFLAGS)",
                     (Addr)((struct vki_ifreq *)ARG3)->vki_ifr_name );
      PRE_MEM_READ( "ioctl(SIOCSIFFLAGS)",
                     (Addr)&((struct vki_ifreq *)ARG3)->vki_ifr_flags,
                     sizeof(((struct vki_ifreq *)ARG3)->vki_ifr_flags) );
      break;
   case VKI_SIOCSIFMAP:          /* Set device parameters        */
      PRE_MEM_RASCIIZ( "ioctl(SIOCSIFMAP)",
                     (Addr)((struct vki_ifreq *)ARG3)->vki_ifr_name );
      PRE_MEM_READ( "ioctl(SIOCSIFMAP)",
                     (Addr)&((struct vki_ifreq *)ARG3)->ifr_map,
                     sizeof(((struct vki_ifreq *)ARG3)->ifr_map) );
      break;
   case VKI_SIOCSIFTXQLEN:       /* Set the tx queue length      */
      PRE_MEM_RASCIIZ( "ioctl(SIOCSIFTXQLEN)",
                     (Addr)((struct vki_ifreq *)ARG3)->vki_ifr_name );
      PRE_MEM_READ( "ioctl(SIOCSIFTXQLEN)",
                     (Addr)&((struct vki_ifreq *)ARG3)->ifr_qlen,
                     sizeof(((struct vki_ifreq *)ARG3)->ifr_qlen) );
      break;
   case VKI_SIOCSIFADDR:         /* set PA address               */
   case VKI_SIOCSIFDSTADDR:      /* set remote PA address        */
   case VKI_SIOCSIFBRDADDR:      /* set broadcast PA address     */
   case VKI_SIOCSIFNETMASK:      /* set network PA mask          */
      PRE_MEM_RASCIIZ( "ioctl(SIOCSIF*ADDR)",
                     (Addr)((struct vki_ifreq *)ARG3)->vki_ifr_name );
      PRE_MEM_READ( "ioctl(SIOCSIF*ADDR)",
                     (Addr)&((struct vki_ifreq *)ARG3)->ifr_addr,
                     sizeof(((struct vki_ifreq *)ARG3)->ifr_addr) );
      break;
   case VKI_SIOCSIFMETRIC:       /* set metric                   */
      PRE_MEM_RASCIIZ( "ioctl(SIOCSIFMETRIC)",
                     (Addr)((struct vki_ifreq *)ARG3)->vki_ifr_name );
      PRE_MEM_READ( "ioctl(SIOCSIFMETRIC)",
                     (Addr)&((struct vki_ifreq *)ARG3)->vki_ifr_metric,
                     sizeof(((struct vki_ifreq *)ARG3)->vki_ifr_metric) );
      break;
   case VKI_SIOCSIFMTU:          /* set MTU size                 */
      PRE_MEM_RASCIIZ( "ioctl(SIOCSIFMTU)",
                     (Addr)((struct vki_ifreq *)ARG3)->vki_ifr_name );
      PRE_MEM_READ( "ioctl(SIOCSIFMTU)",
                     (Addr)&((struct vki_ifreq *)ARG3)->vki_ifr_mtu,
                     sizeof(((struct vki_ifreq *)ARG3)->vki_ifr_mtu) );
      break;
   case VKI_SIOCSIFHWADDR:       /* set hardware address         */
      PRE_MEM_RASCIIZ( "ioctl(SIOCSIFHWADDR)",
                     (Addr)((struct vki_ifreq *)ARG3)->vki_ifr_name );
      PRE_MEM_READ( "ioctl(SIOCSIFHWADDR)",
                     (Addr)&((struct vki_ifreq *)ARG3)->ifr_hwaddr,
                     sizeof(((struct vki_ifreq *)ARG3)->ifr_hwaddr) );
      break;
   case VKI_SIOCSMIIREG:         /* set hardware entry registers */
      PRE_MEM_RASCIIZ( "ioctl(SIOCSMIIREG)",
                     (Addr)((struct vki_ifreq *)ARG3)->vki_ifr_name );
      PRE_MEM_READ( "ioctl(SIOCSMIIREG)",
                     (Addr)&((struct vki_mii_ioctl_data *)&((struct vki_ifreq *)ARG3)->vki_ifr_data)->phy_id,
                     sizeof(((struct vki_mii_ioctl_data *)&((struct vki_ifreq *)ARG3)->vki_ifr_data)->phy_id) );
      PRE_MEM_READ( "ioctl(SIOCSMIIREG)",
                     (Addr)&((struct vki_mii_ioctl_data *)&((struct vki_ifreq *)ARG3)->vki_ifr_data)->reg_num,
                     sizeof(((struct vki_mii_ioctl_data *)&((struct vki_ifreq *)ARG3)->vki_ifr_data)->reg_num) );
      PRE_MEM_READ( "ioctl(SIOCSMIIREG)",
                     (Addr)&((struct vki_mii_ioctl_data *)&((struct vki_ifreq *)ARG3)->vki_ifr_data)->val_in,
                     sizeof(((struct vki_mii_ioctl_data *)&((struct vki_ifreq *)ARG3)->vki_ifr_data)->val_in) );
      break;
      /* Routing table calls.  */
   case VKI_SIOCADDRT:           /* add routing table entry      */
   case VKI_SIOCDELRT:           /* delete routing table entry   */
      PRE_MEM_READ( "ioctl(SIOCADDRT/DELRT)", ARG3, 
		    sizeof(struct vki_rtentry));
      break;

      /* RARP cache control calls. */
   case VKI_SIOCDRARP:           /* delete RARP table entry      */
   case VKI_SIOCSRARP:           /* set RARP table entry         */
      /* ARP cache control calls. */
   case VKI_SIOCSARP:            /* set ARP table entry          */
   case VKI_SIOCDARP:            /* delete ARP table entry       */
      PRE_MEM_READ( "ioctl(SIOCSIFFLAGS)", ARG3, sizeof(struct vki_ifreq));
      break;

   case VKI_SIOCGPGRP:
      PRE_MEM_WRITE( "ioctl(SIOCGPGRP)", ARG3, sizeof(int) );
      break;
   case VKI_SIOCSPGRP:
      PRE_MEM_READ( "ioctl(SIOCSPGRP)", ARG3, sizeof(int) );
      //tst->sys_flags &= ~SfMayBlock;
      break;

      /* linux/soundcard interface (OSS) */
   case VKI_SNDCTL_SEQ_GETOUTCOUNT:
   case VKI_SNDCTL_SEQ_GETINCOUNT:
   case VKI_SNDCTL_SEQ_PERCMODE:
   case VKI_SNDCTL_SEQ_TESTMIDI:
   case VKI_SNDCTL_SEQ_RESETSAMPLES:
   case VKI_SNDCTL_SEQ_NRSYNTHS:
   case VKI_SNDCTL_SEQ_NRMIDIS:
   case VKI_SNDCTL_SEQ_GETTIME:
   case VKI_SNDCTL_DSP_GETBLKSIZE:
   case VKI_SNDCTL_DSP_GETFMTS:
   case VKI_SNDCTL_DSP_GETTRIGGER:
   case VKI_SNDCTL_DSP_GETODELAY:
   case VKI_SNDCTL_DSP_GETSPDIF:
   case VKI_SNDCTL_DSP_GETCAPS:
   case VKI_SOUND_PCM_READ_RATE:
   case VKI_SOUND_PCM_READ_CHANNELS:
   case VKI_SOUND_PCM_READ_BITS:
   case VKI_SOUND_PCM_READ_FILTER:
      PRE_MEM_WRITE( "ioctl(SNDCTL_XXX|SOUND_XXX (SIOR, int))", 
		     ARG3, sizeof(int));
      break;
   case VKI_SNDCTL_SEQ_CTRLRATE:
   case VKI_SNDCTL_DSP_SPEED:
   case VKI_SNDCTL_DSP_STEREO:
   case VKI_SNDCTL_DSP_CHANNELS:
   case VKI_SOUND_PCM_WRITE_FILTER:
   case VKI_SNDCTL_DSP_SUBDIVIDE:
   case VKI_SNDCTL_DSP_SETFRAGMENT:
   case VKI_SNDCTL_DSP_SETFMT:
   case VKI_SNDCTL_DSP_GETCHANNELMASK:
   case VKI_SNDCTL_DSP_BIND_CHANNEL:
   case VKI_SNDCTL_TMR_TIMEBASE:
   case VKI_SNDCTL_TMR_TEMPO:
   case VKI_SNDCTL_TMR_SOURCE:
   case VKI_SNDCTL_MIDI_PRETIME:
   case VKI_SNDCTL_MIDI_MPUMODE:
      PRE_MEM_READ( "ioctl(SNDCTL_XXX|SOUND_XXX (SIOWR, int))", 
		     ARG3, sizeof(int));
      PRE_MEM_WRITE( "ioctl(SNDCTL_XXX|SOUND_XXX (SIOWR, int))", 
		     ARG3, sizeof(int));
      break;
   case VKI_SNDCTL_DSP_GETOSPACE:
   case VKI_SNDCTL_DSP_GETISPACE:
      PRE_MEM_WRITE( "ioctl(SNDCTL_XXX|SOUND_XXX (SIOR, audio_buf_info))",
                     ARG3, sizeof(vki_audio_buf_info));
      break;
   case VKI_SNDCTL_DSP_NONBLOCK:
      break;
   case VKI_SNDCTL_DSP_SETTRIGGER:
      PRE_MEM_READ( "ioctl(SNDCTL_XXX|SOUND_XXX (SIOW, int))", 
		     ARG3, sizeof(int));
      break;

   case VKI_SNDCTL_DSP_POST:
   case VKI_SNDCTL_DSP_RESET:
   case VKI_SNDCTL_DSP_SYNC:
   case VKI_SNDCTL_DSP_SETSYNCRO:
   case VKI_SNDCTL_DSP_SETDUPLEX:
      break;

      /* linux/soundcard interface (ALSA) */
   case VKI_SNDRV_PCM_IOCTL_PAUSE:
   case VKI_SNDRV_PCM_IOCTL_LINK:
      /* these just take an int by value */
      break;

      /* Real Time Clock (/dev/rtc) ioctls */
   case VKI_RTC_UIE_ON:
   case VKI_RTC_UIE_OFF:
   case VKI_RTC_AIE_ON:
   case VKI_RTC_AIE_OFF:
   case VKI_RTC_PIE_ON:
   case VKI_RTC_PIE_OFF:
   case VKI_RTC_IRQP_SET:
      break;
   case VKI_RTC_RD_TIME:
   case VKI_RTC_ALM_READ:
      PRE_MEM_WRITE( "ioctl(RTC_RD_TIME/ALM_READ)", 
		     ARG3, sizeof(struct vki_rtc_time));
      break;
   case VKI_RTC_ALM_SET:
      PRE_MEM_READ( "ioctl(RTC_ALM_SET)", ARG3, sizeof(struct vki_rtc_time));
      break;
   case VKI_RTC_IRQP_READ:
      PRE_MEM_WRITE( "ioctl(RTC_IRQP_READ)", ARG3, sizeof(unsigned long));
      break;

      /* Block devices */
   case VKI_BLKROSET:
      PRE_MEM_READ( "ioctl(BLKROSET)", ARG3, sizeof(int));
      break;
   case VKI_BLKROGET:
      PRE_MEM_WRITE( "ioctl(BLKROGET)", ARG3, sizeof(int));
      break;
   case VKI_BLKGETSIZE:
      PRE_MEM_WRITE( "ioctl(BLKGETSIZE)", ARG3, sizeof(unsigned long));
      break;
   case VKI_BLKRASET:
      break;
   case VKI_BLKRAGET:
      PRE_MEM_WRITE( "ioctl(BLKRAGET)", ARG3, sizeof(long));
      break;
   case VKI_BLKFRASET:
      break;
   case VKI_BLKFRAGET:
      PRE_MEM_WRITE( "ioctl(BLKFRAGET)", ARG3, sizeof(long));
      break;
   case VKI_BLKSECTGET:
      PRE_MEM_WRITE( "ioctl(BLKSECTGET)", ARG3, sizeof(unsigned short));
      break;
   case VKI_BLKSSZGET:
      PRE_MEM_WRITE( "ioctl(BLKSSZGET)", ARG3, sizeof(int));
      break;
   case VKI_BLKBSZGET:
      PRE_MEM_WRITE( "ioctl(BLKBSZGET)", ARG3, sizeof(int));
      break;
   case VKI_BLKBSZSET:
      PRE_MEM_READ( "ioctl(BLKBSZSET)", ARG3, sizeof(int));
      break;
   case VKI_BLKGETSIZE64:
      PRE_MEM_WRITE( "ioctl(BLKGETSIZE64)", ARG3, sizeof(unsigned long long));
      break;

      /* Hard disks */
   case VKI_HDIO_GETGEO: /* 0x0301 */
      PRE_MEM_WRITE( "ioctl(HDIO_GETGEO)", ARG3, sizeof(struct vki_hd_geometry));
      break;
   case VKI_HDIO_GET_DMA: /* 0x030b */
      PRE_MEM_WRITE( "ioctl(HDIO_GET_DMA)", ARG3, sizeof(long));
      break;
   case VKI_HDIO_GET_IDENTITY: /* 0x030d */
      PRE_MEM_WRITE( "ioctl(HDIO_GET_IDENTITY)", ARG3,
                     VKI_SIZEOF_STRUCT_HD_DRIVEID );
      break;

      /* CD ROM stuff (??)  */
   case VKI_CDROM_GET_MCN:
      PRE_MEM_READ( "ioctl(CDROM_GET_MCN)", ARG3,
                    sizeof(struct vki_cdrom_mcn) );
      break;
   case VKI_CDROM_SEND_PACKET:
      PRE_MEM_READ( "ioctl(CDROM_SEND_PACKET)", ARG3,
                    sizeof(struct vki_cdrom_generic_command));
      break;
   case VKI_CDROMSUBCHNL:
      PRE_MEM_READ( "ioctl(CDROMSUBCHNL (cdsc_format, char))",
		    (Addr) &(((struct vki_cdrom_subchnl*) ARG3)->cdsc_format),
		    sizeof(((struct vki_cdrom_subchnl*) ARG3)->cdsc_format));
      PRE_MEM_WRITE( "ioctl(CDROMSUBCHNL)", ARG3, 
		     sizeof(struct vki_cdrom_subchnl));
      break;
   case VKI_CDROMREADMODE2:
      PRE_MEM_READ( "ioctl(CDROMREADMODE2)", ARG3, VKI_CD_FRAMESIZE_RAW0 );
      break;
   case VKI_CDROMREADTOCHDR:
      PRE_MEM_WRITE( "ioctl(CDROMREADTOCHDR)", ARG3, 
		     sizeof(struct vki_cdrom_tochdr));
      break;
   case VKI_CDROMREADTOCENTRY:
      PRE_MEM_READ( "ioctl(CDROMREADTOCENTRY (cdte_format, char))",
		    (Addr) &(((struct vki_cdrom_tocentry*) ARG3)->cdte_format),
		    sizeof(((struct vki_cdrom_tocentry*) ARG3)->cdte_format));
      PRE_MEM_READ( "ioctl(CDROMREADTOCENTRY (cdte_track, char))",
		    (Addr) &(((struct vki_cdrom_tocentry*) ARG3)->cdte_track), 
		    sizeof(((struct vki_cdrom_tocentry*) ARG3)->cdte_track));
      PRE_MEM_WRITE( "ioctl(CDROMREADTOCENTRY)", ARG3, 
		     sizeof(struct vki_cdrom_tocentry));
      break;
   case VKI_CDROMMULTISESSION: /* 0x5310 */
      PRE_MEM_WRITE( "ioctl(CDROMMULTISESSION)", ARG3,
		     sizeof(struct vki_cdrom_multisession));
      break;
   case VKI_CDROMVOLREAD: /* 0x5313 */
      PRE_MEM_WRITE( "ioctl(CDROMVOLREAD)", ARG3,
		     sizeof(struct vki_cdrom_volctrl));
      break;
   case VKI_CDROMREADRAW: /* 0x5314 */
      PRE_MEM_READ( "ioctl(CDROMREADRAW)", ARG3, sizeof(struct vki_cdrom_msf));
      PRE_MEM_WRITE( "ioctl(CDROMREADRAW)", ARG3, VKI_CD_FRAMESIZE_RAW);
      break;
   case VKI_CDROMREADAUDIO: /* 0x530e */
      PRE_MEM_READ( "ioctl(CDROMREADAUDIO)", ARG3,
		     sizeof (struct vki_cdrom_read_audio));
      if ( ARG3 ) {
         /* ToDo: don't do any of the following if the structure is invalid */
         struct vki_cdrom_read_audio *cra = (struct vki_cdrom_read_audio *) ARG3;
	 PRE_MEM_WRITE( "ioctl(CDROMREADAUDIO).buf",
	                (Addr)(cra->buf), cra->nframes * VKI_CD_FRAMESIZE_RAW);
      }
      break;      
   case VKI_CDROMPLAYMSF:
      PRE_MEM_READ( "ioctl(CDROMPLAYMSF)", ARG3, sizeof(struct vki_cdrom_msf));
      break;
      /* The following two are probably bogus (should check args
	 for readability).  JRS 20021117 */
   case VKI_CDROM_DRIVE_STATUS: /* 0x5326 */
   case VKI_CDROM_CLEAR_OPTIONS: /* 0x5321 */
      break;

   case VKI_FIGETBSZ:
      PRE_MEM_WRITE( "ioctl(FIGETBSZ)", ARG3, sizeof(unsigned long));
      break;
   case VKI_FIBMAP:
      PRE_MEM_READ( "ioctl(FIBMAP)", ARG3, sizeof(int));
      break;

   case VKI_FBIOGET_VSCREENINFO: /* 0x4600 */
      PRE_MEM_WRITE( "ioctl(FBIOGET_VSCREENINFO)", ARG3,
                     sizeof(struct vki_fb_var_screeninfo));
      break;
   case VKI_FBIOGET_FSCREENINFO: /* 0x4602 */
      PRE_MEM_WRITE( "ioctl(FBIOGET_FSCREENINFO)", ARG3,
                     sizeof(struct vki_fb_fix_screeninfo));
      break;

   case VKI_PPCLAIM:
   case VKI_PPEXCL:
   case VKI_PPYIELD:
   case VKI_PPRELEASE:
      break;
   case VKI_PPSETMODE:
      PRE_MEM_READ( "ioctl(PPSETMODE)",   ARG3, sizeof(int) );
      break;
   case VKI_PPGETMODE:
      PRE_MEM_WRITE( "ioctl(PPGETMODE)",  ARG3, sizeof(int) );
      break;
   case VKI_PPSETPHASE:
      PRE_MEM_READ(  "ioctl(PPSETPHASE)", ARG3, sizeof(int) );
      break;
   case VKI_PPGETPHASE:
      PRE_MEM_WRITE( "ioctl(PPGETPHASE)", ARG3, sizeof(int) );
      break;
   case VKI_PPGETMODES:
      PRE_MEM_WRITE( "ioctl(PPGETMODES)", ARG3, sizeof(unsigned int) );
      break;
   case VKI_PPSETFLAGS:
      PRE_MEM_READ(  "ioctl(PPSETFLAGS)", ARG3, sizeof(int) );
      break;
   case VKI_PPGETFLAGS:
      PRE_MEM_WRITE( "ioctl(PPGETFLAGS)", ARG3, sizeof(int) );
      break;
   case VKI_PPRSTATUS:
      PRE_MEM_WRITE( "ioctl(PPRSTATUS)",  ARG3, sizeof(unsigned char) );
      break;
   case VKI_PPRDATA:
      PRE_MEM_WRITE( "ioctl(PPRDATA)",    ARG3, sizeof(unsigned char) );
      break;
   case VKI_PPRCONTROL:
      PRE_MEM_WRITE( "ioctl(PPRCONTROL)", ARG3, sizeof(unsigned char) );
      break;
   case VKI_PPWDATA:
      PRE_MEM_READ(  "ioctl(PPWDATA)",    ARG3, sizeof(unsigned char) );
      break;
   case VKI_PPWCONTROL:
      PRE_MEM_READ(  "ioctl(PPWCONTROL)", ARG3, sizeof(unsigned char) );
      break;
   case VKI_PPFCONTROL:
      PRE_MEM_READ(  "ioctl(PPFCONTROL)", ARG3, 2 * sizeof(unsigned char) );
      break;
   case VKI_PPDATADIR:
      PRE_MEM_READ(  "ioctl(PPDATADIR)",  ARG3, sizeof(int) );
      break;
   case VKI_PPNEGOT:
      PRE_MEM_READ(  "ioctl(PPNEGOT)",    ARG3, sizeof(int) );
      break;
   case VKI_PPWCTLONIRQ:
      PRE_MEM_READ(  "ioctl(PPWCTLONIRQ)",ARG3, sizeof(unsigned char) );
      break;
   case VKI_PPCLRIRQ:
      PRE_MEM_WRITE( "ioctl(PPCLRIRQ)",   ARG3, sizeof(int) );
      break;
   case VKI_PPSETTIME:
      PRE_MEM_READ(  "ioctl(PPSETTIME)",  ARG3, sizeof(struct vki_timeval) );
      break;
   case VKI_PPGETTIME:
      PRE_MEM_WRITE( "ioctl(PPGETTIME)",  ARG3, sizeof(struct vki_timeval) );
      break;

   case VKI_GIO_FONT:
      PRE_MEM_WRITE( "ioctl(GIO_FONT)", ARG3, 32 * 256 );
      break;
   case VKI_PIO_FONT:
      PRE_MEM_READ( "ioctl(PIO_FONT)", ARG3, 32 * 256 );
      break;

   case VKI_GIO_FONTX:
      PRE_MEM_READ( "ioctl(GIO_FONTX)", ARG3, sizeof(struct vki_consolefontdesc) );
      if ( ARG3 ) {
         /* ToDo: don't do any of the following if the structure is invalid */
         struct vki_consolefontdesc *cfd = (struct vki_consolefontdesc *)ARG3;
         PRE_MEM_WRITE( "ioctl(GIO_FONTX).chardata", (Addr)cfd->chardata,
                        32 * cfd->charcount );
      }
      break;
   case VKI_PIO_FONTX:
      PRE_MEM_READ( "ioctl(PIO_FONTX)", ARG3, sizeof(struct vki_consolefontdesc) );
      if ( ARG3 ) {
         /* ToDo: don't do any of the following if the structure is invalid */
         struct vki_consolefontdesc *cfd = (struct vki_consolefontdesc *)ARG3;
         PRE_MEM_READ( "ioctl(PIO_FONTX).chardata", (Addr)cfd->chardata,
                       32 * cfd->charcount );
      }
      break;

   case VKI_PIO_FONTRESET:
      break;

   case VKI_GIO_CMAP:
      PRE_MEM_WRITE( "ioctl(GIO_CMAP)", ARG3, 16 * 3 );
      break;
   case VKI_PIO_CMAP:
      PRE_MEM_READ( "ioctl(PIO_CMAP)", ARG3, 16 * 3 );
      break;

   case VKI_KIOCSOUND:
   case VKI_KDMKTONE:
      break;

   case VKI_KDGETLED:
      PRE_MEM_WRITE( "ioctl(KDGETLED)", ARG3, sizeof(char) );
      break;
   case VKI_KDSETLED:
      break;

   case VKI_KDGKBTYPE:
      PRE_MEM_WRITE( "ioctl(KDGKBTYPE)", ARG3, sizeof(char) );
      break;

   case VKI_KDADDIO:
   case VKI_KDDELIO:
   case VKI_KDENABIO:
   case VKI_KDDISABIO:
      break;

   case VKI_KDSETMODE:
      break;
   case VKI_KDGETMODE:
      PRE_MEM_WRITE( "ioctl(KDGETMODE)", ARG3, sizeof(int) );
      break;

   case VKI_KDMAPDISP:
   case VKI_KDUNMAPDISP:
      break;

   case VKI_GIO_SCRNMAP:
      PRE_MEM_WRITE( "ioctl(GIO_SCRNMAP)", ARG3, VKI_E_TABSZ );
      break;
   case VKI_PIO_SCRNMAP:
      PRE_MEM_READ( "ioctl(PIO_SCRNMAP)", ARG3, VKI_E_TABSZ  );
      break;
   case VKI_GIO_UNISCRNMAP:
      PRE_MEM_WRITE( "ioctl(GIO_UNISCRNMAP)", ARG3,
                     VKI_E_TABSZ * sizeof(unsigned short) );
      break;
   case VKI_PIO_UNISCRNMAP:
      PRE_MEM_READ( "ioctl(PIO_UNISCRNMAP)", ARG3,
                    VKI_E_TABSZ * sizeof(unsigned short) );
      break;

   case VKI_GIO_UNIMAP:
      if ( ARG3 ) {
         struct vki_unimapdesc *desc = (struct vki_unimapdesc *) ARG3;
         PRE_MEM_READ( "ioctl(GIO_UNIMAP)", (Addr)&desc->entry_ct,
                       sizeof(unsigned short));
         PRE_MEM_READ( "ioctl(GIO_UNIMAP)", (Addr)&desc->entries,
                       sizeof(struct vki_unipair *));
         PRE_MEM_WRITE( "ioctl(GIO_UNIMAP).entries", (Addr)desc->entries,
                        desc->entry_ct * sizeof(struct vki_unipair));
      }
      break;
   case VKI_PIO_UNIMAP:
      if ( ARG3 ) {
         struct vki_unimapdesc *desc = (struct vki_unimapdesc *) ARG3;
         PRE_MEM_READ( "ioctl(GIO_UNIMAP)", (Addr)&desc->entry_ct,
                       sizeof(unsigned short) );
         PRE_MEM_READ( "ioctl(GIO_UNIMAP)", (Addr)&desc->entries,
                       sizeof(struct vki_unipair *) );
         PRE_MEM_READ( "ioctl(PIO_UNIMAP).entries", (Addr)desc->entries,
                       desc->entry_ct * sizeof(struct vki_unipair) );
      }
      break;
   case VKI_PIO_UNIMAPCLR:
      PRE_MEM_READ( "ioctl(GIO_UNIMAP)", ARG3, sizeof(struct vki_unimapinit));
      break;

   case VKI_KDGKBMODE:
      PRE_MEM_WRITE( "ioctl(KDGKBMODE)", ARG3, sizeof(int) );
      break;
   case VKI_KDSKBMODE:
      break;
      
   case VKI_KDGKBMETA:
      PRE_MEM_WRITE( "ioctl(KDGKBMETA)", ARG3, sizeof(int) );
      break;
   case VKI_KDSKBMETA:
      break;
      
   case VKI_KDGKBLED:
      PRE_MEM_WRITE( "ioctl(KDGKBLED)", ARG3, sizeof(char) );
      break;
   case VKI_KDSKBLED:
      break;
      
   case VKI_KDGKBENT:
      PRE_MEM_READ( "ioctl(KDGKBENT).kb_table",
                    (Addr)&((struct vki_kbentry *)ARG3)->kb_table,
                    sizeof(((struct vki_kbentry *)ARG3)->kb_table) );
      PRE_MEM_READ( "ioctl(KDGKBENT).kb_index",
                    (Addr)&((struct vki_kbentry *)ARG3)->kb_index,
                    sizeof(((struct vki_kbentry *)ARG3)->kb_index) );
      PRE_MEM_WRITE( "ioctl(KDGKBENT).kb_value",
		     (Addr)&((struct vki_kbentry *)ARG3)->kb_value,
		     sizeof(((struct vki_kbentry *)ARG3)->kb_value) );
      break;
   case VKI_KDSKBENT:
      PRE_MEM_READ( "ioctl(KDSKBENT).kb_table",
                    (Addr)&((struct vki_kbentry *)ARG3)->kb_table,
                    sizeof(((struct vki_kbentry *)ARG3)->kb_table) );
      PRE_MEM_READ( "ioctl(KDSKBENT).kb_index",
                    (Addr)&((struct vki_kbentry *)ARG3)->kb_index,
                    sizeof(((struct vki_kbentry *)ARG3)->kb_index) );
      PRE_MEM_READ( "ioctl(KDSKBENT).kb_value",
                    (Addr)&((struct vki_kbentry *)ARG3)->kb_value,
                    sizeof(((struct vki_kbentry *)ARG3)->kb_value) );
      break;
      
   case VKI_KDGKBSENT:
      PRE_MEM_READ( "ioctl(KDGKBSENT).kb_func",
                    (Addr)&((struct vki_kbsentry *)ARG3)->kb_func,
                    sizeof(((struct vki_kbsentry *)ARG3)->kb_func) );
      PRE_MEM_WRITE( "ioctl(KDGKSENT).kb_string",
		     (Addr)((struct vki_kbsentry *)ARG3)->kb_string,
		     sizeof(((struct vki_kbsentry *)ARG3)->kb_string) );
      break;
   case VKI_KDSKBSENT:
      PRE_MEM_READ( "ioctl(KDSKBSENT).kb_func",
                    (Addr)&((struct vki_kbsentry *)ARG3)->kb_func,
                    sizeof(((struct vki_kbsentry *)ARG3)->kb_func) );
      PRE_MEM_RASCIIZ( "ioctl(KDSKBSENT).kb_string",
                       (Addr)((struct vki_kbsentry *)ARG3)->kb_string );
      break;
      
   case VKI_KDGKBDIACR:
      PRE_MEM_WRITE( "ioctl(KDGKBDIACR)", ARG3, sizeof(struct vki_kbdiacrs) );
      break;
   case VKI_KDSKBDIACR:
      PRE_MEM_READ( "ioctl(KDSKBDIACR)", ARG3, sizeof(struct vki_kbdiacrs) );
      break;
      
   case VKI_KDGETKEYCODE:
      PRE_MEM_READ( "ioctl(KDGETKEYCODE).scancode",
                    (Addr)&((struct vki_kbkeycode *)ARG3)->scancode,
                    sizeof(((struct vki_kbkeycode *)ARG3)->scancode) );
      PRE_MEM_WRITE( "ioctl(KDGETKEYCODE).keycode",
		     (Addr)((struct vki_kbkeycode *)ARG3)->keycode,
		     sizeof(((struct vki_kbkeycode *)ARG3)->keycode) );
      break;
   case VKI_KDSETKEYCODE:
      PRE_MEM_READ( "ioctl(KDSETKEYCODE).scancode",
                    (Addr)&((struct vki_kbkeycode *)ARG3)->scancode,
                    sizeof(((struct vki_kbkeycode *)ARG3)->scancode) );
      PRE_MEM_READ( "ioctl(KDSETKEYCODE).keycode",
                    (Addr)((struct vki_kbkeycode *)ARG3)->keycode,
                    sizeof(((struct vki_kbkeycode *)ARG3)->keycode) );
      break;
      
   case VKI_KDSIGACCEPT:
      break;

   case VKI_KDKBDREP:
      PRE_MEM_READ( "ioctl(KBKBDREP)", ARG3, sizeof(struct vki_kbd_repeat) );
      break;

   case VKI_KDFONTOP:
      if ( ARG3 ) {
         struct vki_console_font_op *op = (struct vki_console_font_op *) ARG3;
         PRE_MEM_READ( "ioctl(KDFONTOP)", (Addr)op,
                       sizeof(struct vki_console_font_op) );
         switch ( op->op ) {
            case VKI_KD_FONT_OP_SET:
               PRE_MEM_READ( "ioctl(KDFONTOP,KD_FONT_OP_SET).data",
                             (Addr)op->data,
                             (op->width + 7) / 8 * 32 * op->charcount );
               break;
            case VKI_KD_FONT_OP_GET:
               if ( op->data )
                  PRE_MEM_WRITE( "ioctl(KDFONTOP,KD_FONT_OP_GET).data",
                                 (Addr)op->data,
                                 (op->width + 7) / 8 * 32 * op->charcount );
               break;
            case VKI_KD_FONT_OP_SET_DEFAULT:
               if ( op->data )
                  PRE_MEM_RASCIIZ( "ioctl(KDFONTOP,KD_FONT_OP_SET_DEFAULT).data",
                                   (Addr)op->data );
               break;
            case VKI_KD_FONT_OP_COPY:
               break;
         }
      }
      break;

   case VKI_VT_OPENQRY:
      PRE_MEM_WRITE( "ioctl(VT_OPENQRY)", ARG3, sizeof(int) );
      break;
   case VKI_VT_GETMODE:
      PRE_MEM_WRITE( "ioctl(VT_GETMODE)", ARG3, sizeof(struct vki_vt_mode) );
      break;
   case VKI_VT_SETMODE:
      PRE_MEM_READ( "ioctl(VT_SETMODE)", ARG3, sizeof(struct vki_vt_mode) );
      break;
   case VKI_VT_GETSTATE:
      PRE_MEM_WRITE( "ioctl(VT_GETSTATE).v_active",
                     (Addr) &(((struct vki_vt_stat*) ARG3)->v_active),
                     sizeof(((struct vki_vt_stat*) ARG3)->v_active));
      PRE_MEM_WRITE( "ioctl(VT_GETSTATE).v_state",
                     (Addr) &(((struct vki_vt_stat*) ARG3)->v_state),
                     sizeof(((struct vki_vt_stat*) ARG3)->v_state));
      break;
   case VKI_VT_RELDISP:
   case VKI_VT_ACTIVATE:
   case VKI_VT_WAITACTIVE:
   case VKI_VT_DISALLOCATE:
      break;
   case VKI_VT_RESIZE:
      PRE_MEM_READ( "ioctl(VT_RESIZE)", ARG3, sizeof(struct vki_vt_sizes) );
      break;
   case VKI_VT_RESIZEX:
      PRE_MEM_READ( "ioctl(VT_RESIZEX)", ARG3, sizeof(struct vki_vt_consize) );
      break;
   case VKI_VT_LOCKSWITCH:
   case VKI_VT_UNLOCKSWITCH:
      break;

   case VKI_USBDEVFS_CONTROL:
      if ( ARG3 ) {
         struct vki_usbdevfs_ctrltransfer *vkuc = (struct vki_usbdevfs_ctrltransfer *)ARG3;
         PRE_MEM_READ( "ioctl(USBDEVFS_CONTROL).bRequestType", (Addr)&vkuc->bRequestType, sizeof(vkuc->bRequestType));
         PRE_MEM_READ( "ioctl(USBDEVFS_CONTROL).bRequest", (Addr)&vkuc->bRequest, sizeof(vkuc->bRequest));
         PRE_MEM_READ( "ioctl(USBDEVFS_CONTROL).wValue", (Addr)&vkuc->wValue, sizeof(vkuc->wValue));
         PRE_MEM_READ( "ioctl(USBDEVFS_CONTROL).wIndex", (Addr)&vkuc->wIndex, sizeof(vkuc->wIndex));
         PRE_MEM_READ( "ioctl(USBDEVFS_CONTROL).wLength", (Addr)&vkuc->wLength, sizeof(vkuc->wLength));
         PRE_MEM_READ( "ioctl(USBDEVFS_CONTROL).timeout", (Addr)&vkuc->timeout, sizeof(vkuc->timeout));
         if (vkuc->bRequestType & 0x80)
            PRE_MEM_WRITE( "ioctl(USBDEVFS_CONTROL).data", (Addr)vkuc->data, vkuc->wLength);
         else
            PRE_MEM_READ( "ioctl(USBDEVFS_CONTROL).data", (Addr)vkuc->data, vkuc->wLength);
      }
      break;
   case VKI_USBDEVFS_BULK:
      if ( ARG3 ) {
         struct vki_usbdevfs_bulktransfer *vkub = (struct vki_usbdevfs_bulktransfer *)ARG3;
         PRE_MEM_READ( "ioctl(USBDEVFS_BULK)", ARG3, sizeof(struct vki_usbdevfs_bulktransfer));
         if (vkub->ep & 0x80)
            PRE_MEM_WRITE( "ioctl(USBDEVFS_BULK).data", (Addr)vkub->data, vkub->len);
         else
            PRE_MEM_READ( "ioctl(USBDEVFS_BULK).data", (Addr)vkub->data, vkub->len);
         break;
      }
   case VKI_USBDEVFS_GETDRIVER:
      if ( ARG3 ) {
         struct vki_usbdevfs_getdriver *vkugd = (struct vki_usbdevfs_getdriver *) ARG3;
         PRE_MEM_WRITE( "ioctl(USBDEVFS_GETDRIVER)", (Addr)&vkugd->driver, sizeof(vkugd->driver));
         break;
      }
   case VKI_USBDEVFS_SUBMITURB:
      if ( ARG3 ) {
         struct vki_usbdevfs_urb *vkuu = (struct vki_usbdevfs_urb *)ARG3;

         /* Not the whole struct needs to be initialized */
         PRE_MEM_READ( "ioctl(USBDEVFS_SUBMITURB).endpoint", (Addr)&vkuu->endpoint, sizeof(vkuu->endpoint));
         PRE_MEM_READ( "ioctl(USBDEVFS_SUBMITURB).type", (Addr)&vkuu->type, sizeof(vkuu->type));
         PRE_MEM_READ( "ioctl(USBDEVFS_SUBMITURB).flags", (Addr)&vkuu->flags, sizeof(vkuu->flags));
         PRE_MEM_READ( "ioctl(USBDEVFS_SUBMITURB).buffer", (Addr)&vkuu->buffer, sizeof(vkuu->buffer));
         PRE_MEM_READ( "ioctl(USBDEVFS_SUBMITURB).signr", (Addr)&vkuu->signr, sizeof(vkuu->signr));
         PRE_MEM_WRITE( "ioctl(USBDEVFS_SUBMITURB).status", (Addr)&vkuu->status, sizeof(vkuu->status));
         if (vkuu->type == VKI_USBDEVFS_URB_TYPE_CONTROL) {
            struct vki_usbdevfs_setuppacket *vkusp = (struct vki_usbdevfs_setuppacket *)vkuu->buffer;
            PRE_MEM_READ( "ioctl(USBDEVFS_SUBMITURB).buffer_length", (Addr)&vkuu->buffer_length, sizeof(vkuu->buffer_length));
            PRE_MEM_READ( "ioctl(USBDEVFS_SUBMITURB).buffer.setup_packet", (Addr)vkusp, sizeof(*vkusp));
            if (vkusp->bRequestType & 0x80)
               PRE_MEM_WRITE( "ioctl(USBDEVFS_SUBMITURB).buffer.data", (Addr)(vkusp+1), vkuu->buffer_length - sizeof(*vkusp));
            else
               PRE_MEM_READ( "ioctl(USBDEVFS_SUBMITURB).buffer.data", (Addr)(vkusp+1), vkuu->buffer_length - sizeof(*vkusp));
            PRE_MEM_WRITE( "ioctl(USBDEVFS_SUBMITURB).actual_length", (Addr)&vkuu->actual_length, sizeof(vkuu->actual_length));
         } else if (vkuu->type == VKI_USBDEVFS_URB_TYPE_ISO) {
            int total_length = 0;
            int i;
            PRE_MEM_READ( "ioctl(USBDEVFS_SUBMITURB).number_of_packets", (Addr)&vkuu->number_of_packets, sizeof(vkuu->number_of_packets));
            for(i=0; i<vkuu->number_of_packets; i++) {
               PRE_MEM_READ( "ioctl(USBDEVFS_SUBMITURB).iso_frame_desc[].length", (Addr)&vkuu->iso_frame_desc[i].length, sizeof(vkuu->iso_frame_desc[i].length));
               PRE_MEM_WRITE( "ioctl(USBDEVFS_SUBMITURB).iso_frame_desc[].actual_length", (Addr)&vkuu->iso_frame_desc[i].actual_length, sizeof(vkuu->iso_frame_desc[i].actual_length));
               PRE_MEM_WRITE( "ioctl(USBDEVFS_SUBMITURB).iso_frame_desc[].status", (Addr)&vkuu->iso_frame_desc[i].status, sizeof(vkuu->iso_frame_desc[i].status));
               total_length += vkuu->iso_frame_desc[i].length;
            }
            if (vkuu->endpoint & 0x80)
               PRE_MEM_WRITE( "ioctl(USBDEVFS_SUBMITURB).buffer", (Addr)vkuu->buffer, total_length);
            else
               PRE_MEM_READ( "ioctl(USBDEVFS_SUBMITURB).buffer", (Addr)vkuu->buffer, total_length);
            PRE_MEM_WRITE( "ioctl(USBDEVFS_SUBMITURB).error_count", (Addr)&vkuu->error_count, sizeof(vkuu->error_count));
         } else {
            PRE_MEM_READ( "ioctl(USBDEVFS_SUBMITURB).buffer_length", (Addr)&vkuu->buffer_length, sizeof(vkuu->buffer_length));
            if (vkuu->endpoint & 0x80)
               PRE_MEM_WRITE( "ioctl(USBDEVFS_SUBMITURB).buffer", (Addr)vkuu->buffer, vkuu->buffer_length);
            else
               PRE_MEM_READ( "ioctl(USBDEVFS_SUBMITURB).buffer", (Addr)vkuu->buffer, vkuu->buffer_length);
            PRE_MEM_WRITE( "ioctl(USBDEVFS_SUBMITURB).actual_length", (Addr)&vkuu->actual_length, sizeof(vkuu->actual_length));
         }
         break;
      }
   case VKI_USBDEVFS_DISCARDURB:
      break;
   case VKI_USBDEVFS_REAPURB:
      if ( ARG3 ) {
         PRE_MEM_WRITE( "ioctl(USBDEVFS_REAPURB)", ARG3, sizeof(struct vki_usbdevfs_urb **));
         break;
      }
   case VKI_USBDEVFS_REAPURBNDELAY:
      if ( ARG3 ) {
         PRE_MEM_WRITE( "ioctl(USBDEVFS_REAPURBNDELAY)", ARG3, sizeof(struct vki_usbdevfs_urb **));
         break;
      }
   case VKI_USBDEVFS_CONNECTINFO:
      PRE_MEM_WRITE( "ioctl(USBDEVFS_CONNECTINFO)", ARG3, sizeof(struct vki_usbdevfs_connectinfo));
      break;
   case VKI_USBDEVFS_IOCTL:
      if ( ARG3 ) {
         struct vki_usbdevfs_ioctl *vkui = (struct vki_usbdevfs_ioctl *)ARG3;
         UInt dir2, size2;
         PRE_MEM_READ("ioctl(USBDEVFS_IOCTL)", (Addr)vkui, sizeof(struct vki_usbdevfs_ioctl));
         dir2  = _VKI_IOC_DIR(vkui->ioctl_code);
         size2 = _VKI_IOC_SIZE(vkui->ioctl_code);
         if (size2 > 0) {
            if (dir2 & _VKI_IOC_WRITE)
               PRE_MEM_READ("ioctl(USBDEVFS_IOCTL).dataWrite", (Addr)vkui->data, size2);
            else if (dir2 & _VKI_IOC_READ)
               PRE_MEM_WRITE("ioctl(USBDEVFS_IOCTL).dataRead", (Addr)vkui->data, size2);
         }
      }
      break;
   case VKI_USBDEVFS_RESET:
      break;

      /* I2C (/dev/i2c-*) ioctls */
   case VKI_I2C_SLAVE:
   case VKI_I2C_SLAVE_FORCE:
   case VKI_I2C_TENBIT:
   case VKI_I2C_PEC:
      break;
   case VKI_I2C_FUNCS:
      PRE_MEM_WRITE( "ioctl(I2C_FUNCS)", ARG3, sizeof(unsigned long) );
      break;

      /* Wireless extensions ioctls */
   case VKI_SIOCSIWCOMMIT:
   case VKI_SIOCSIWNWID:
   case VKI_SIOCSIWFREQ:
   case VKI_SIOCSIWMODE:
   case VKI_SIOCSIWSENS:
   case VKI_SIOCSIWRANGE:
   case VKI_SIOCSIWPRIV:
   case VKI_SIOCSIWSTATS:
   case VKI_SIOCSIWSPY:
   case VKI_SIOCSIWTHRSPY:
   case VKI_SIOCSIWAP:
   case VKI_SIOCSIWSCAN:
   case VKI_SIOCSIWESSID:
   case VKI_SIOCSIWRATE:
   case VKI_SIOCSIWNICKN:
   case VKI_SIOCSIWRTS:
   case VKI_SIOCSIWFRAG:
   case VKI_SIOCSIWTXPOW:
   case VKI_SIOCSIWRETRY:
   case VKI_SIOCSIWENCODE:
   case VKI_SIOCSIWPOWER:
   case VKI_SIOCSIWGENIE:
   case VKI_SIOCSIWMLME:
   case VKI_SIOCSIWAUTH:
   case VKI_SIOCSIWENCODEEXT:
   case VKI_SIOCSIWPMKSA:
      break;
   case VKI_SIOCGIWNAME:
      if (ARG3) {
         PRE_MEM_WRITE("ioctl(SIOCGIWNAME)",
                       (Addr)((struct vki_iwreq *)ARG3)->u.name,
                       sizeof(((struct vki_iwreq *)ARG3)->u.name));
      }
      break;
   case VKI_SIOCGIWNWID:
   case VKI_SIOCGIWSENS:
   case VKI_SIOCGIWRATE:
   case VKI_SIOCGIWRTS:
   case VKI_SIOCGIWFRAG:
   case VKI_SIOCGIWTXPOW:
   case VKI_SIOCGIWRETRY:
   case VKI_SIOCGIWPOWER:
   case VKI_SIOCGIWAUTH:
      if (ARG3) {
         PRE_MEM_WRITE("ioctl(SIOCGIW[NWID|SENS|RATE|RTS|FRAG|TXPOW|"
                       "RETRY|PARAM|AUTH])",
                       (Addr)&((struct vki_iwreq *)ARG3)->u.nwid,
                       sizeof(struct vki_iw_param));
      }
      break;
   case VKI_SIOCGIWFREQ:
      if (ARG3) {
         PRE_MEM_WRITE("ioctl(SIOCGIWFREQ",
                       (Addr)&((struct vki_iwreq *)ARG3)->u.freq,
                       sizeof(struct vki_iw_freq));
      }
      break;
   case VKI_SIOCGIWMODE:
      if (ARG3) {
         PRE_MEM_WRITE("ioctl(SIOCGIWMODE",
                       (Addr)&((struct vki_iwreq *)ARG3)->u.mode,
                       sizeof(__vki_u32));
      }
      break;
   case VKI_SIOCGIWRANGE:
   case VKI_SIOCGIWPRIV:
   case VKI_SIOCGIWSTATS:
   case VKI_SIOCGIWSPY:
   case VKI_SIOCGIWTHRSPY:
   case VKI_SIOCGIWAPLIST:
   case VKI_SIOCGIWSCAN:
   case VKI_SIOCGIWESSID:
   case VKI_SIOCGIWNICKN:
   case VKI_SIOCGIWENCODE:
   case VKI_SIOCGIWGENIE:
   case VKI_SIOCGIWENCODEEXT:
      if (ARG3) {
         struct vki_iw_point* point;
         point = &((struct vki_iwreq *)ARG3)->u.data;
         PRE_MEM_WRITE("ioctl(SIOCGIW[RANGE|PRIV|STATS|SPY|THRSPY|"
                       "APLIST|SCAN|ESSID|NICKN|ENCODE|GENIE|ENCODEEXT])",
                       (Addr)point->pointer, point->length);
      }
      break;
   case VKI_SIOCGIWAP:
      if (ARG3) {
         PRE_MEM_WRITE("ioctl(SIOCGIWAP)",
                       (Addr)&((struct vki_iwreq *)ARG3)->u.ap_addr,
                       sizeof(struct vki_sockaddr));
      }
      break;

   default:
      /* EVIOC* are variable length and return size written on success */
      switch (ARG2 & ~(_VKI_IOC_SIZEMASK << _VKI_IOC_SIZESHIFT)) {
      case VKI_EVIOCGNAME(0):
      case VKI_EVIOCGPHYS(0):
      case VKI_EVIOCGUNIQ(0):
      case VKI_EVIOCGKEY(0):
      case VKI_EVIOCGLED(0):
      case VKI_EVIOCGSND(0):
      case VKI_EVIOCGSW(0):
      case VKI_EVIOCGBIT(VKI_EV_SYN,0):
      case VKI_EVIOCGBIT(VKI_EV_KEY,0):
      case VKI_EVIOCGBIT(VKI_EV_REL,0):
      case VKI_EVIOCGBIT(VKI_EV_ABS,0):
      case VKI_EVIOCGBIT(VKI_EV_MSC,0):
      case VKI_EVIOCGBIT(VKI_EV_SW,0):
      case VKI_EVIOCGBIT(VKI_EV_LED,0):
      case VKI_EVIOCGBIT(VKI_EV_SND,0):
      case VKI_EVIOCGBIT(VKI_EV_REP,0):
      case VKI_EVIOCGBIT(VKI_EV_FF,0):
      case VKI_EVIOCGBIT(VKI_EV_PWR,0):
      case VKI_EVIOCGBIT(VKI_EV_FF_STATUS,0):
         PRE_MEM_WRITE("ioctl(EVIO*)", ARG3, _VKI_IOC_SIZE(ARG2));
         break;
      default:
         ML_(PRE_unknown_ioctl)(tid, ARG2, ARG3);
         break;
      }
      break;
   }   
}

POST(sys_ioctl)
{
   vg_assert(SUCCESS);
   switch (ARG2 /* request */) {
   case VKI_TCSETS:
   case VKI_TCSETSW:
   case VKI_TCSETSF:
      break; 
   case VKI_TCGETS:
      POST_MEM_WRITE( ARG3, sizeof(struct vki_termios) );
      break;
   case VKI_TCSETA:
   case VKI_TCSETAW:
   case VKI_TCSETAF:
      break;
   case VKI_TCGETA:
      POST_MEM_WRITE( ARG3, sizeof(struct vki_termio) );
      break;
   case VKI_TCSBRK:
   case VKI_TCXONC:
   case VKI_TCSBRKP:
   case VKI_TCFLSH:
      break;
   case VKI_TIOCGWINSZ:
      POST_MEM_WRITE( ARG3, sizeof(struct vki_winsize) );
      break;
   case VKI_TIOCSWINSZ:
   case VKI_TIOCMBIS:
   case VKI_TIOCMBIC:
   case VKI_TIOCMSET:
      break;
   case VKI_TIOCMGET:
      POST_MEM_WRITE( ARG3, sizeof(unsigned int) );
      break;
   case VKI_TIOCLINUX:
      POST_MEM_WRITE( ARG3, sizeof(char *) );
      break;
   case VKI_TIOCGPGRP:
      /* Get process group ID for foreground processing group. */
      POST_MEM_WRITE( ARG3, sizeof(vki_pid_t) );
      break;
   case VKI_TIOCSPGRP:
      /* Set a process group ID? */
      POST_MEM_WRITE( ARG3, sizeof(vki_pid_t) );
      break;
   case VKI_TIOCGPTN: /* Get Pty Number (of pty-mux device) */
      POST_MEM_WRITE( ARG3, sizeof(int));
      break;
   case VKI_TIOCSCTTY:
      break;
   case VKI_TIOCSPTLCK: /* Lock/unlock Pty */
      break;
   case VKI_FIONBIO:
      break;
   case VKI_FIOASYNC:
      break;
   case VKI_FIONREAD:                /* identical to SIOCINQ */
      POST_MEM_WRITE( ARG3, sizeof(int) );
      break;

   case VKI_TIOCSERGETLSR:
      POST_MEM_WRITE( ARG3, sizeof(int) );
      break;
   case VKI_TIOCGICOUNT:
      POST_MEM_WRITE( ARG3, sizeof(struct vki_serial_icounter_struct) );
      break;

   case VKI_SG_SET_COMMAND_Q:
      break;
   case VKI_SG_IO:
      POST_MEM_WRITE(ARG3, sizeof(vki_sg_io_hdr_t));
      break;
   case VKI_SG_GET_SCSI_ID:
      POST_MEM_WRITE(ARG3, sizeof(vki_sg_scsi_id_t));
      break;
   case VKI_SG_SET_RESERVED_SIZE:
      break;
   case VKI_SG_SET_TIMEOUT:
      break;
   case VKI_SG_GET_RESERVED_SIZE:
      POST_MEM_WRITE(ARG3, sizeof(int));
      break;
   case VKI_SG_GET_TIMEOUT:
      break;
   case VKI_SG_GET_VERSION_NUM:
      POST_MEM_WRITE(ARG3, sizeof(int));
      break;
   case VKI_SG_EMULATED_HOST:
      POST_MEM_WRITE(ARG3, sizeof(int));
      break;
   case VKI_SG_GET_SG_TABLESIZE:
      POST_MEM_WRITE(ARG3, sizeof(int));
      break;      

   case VKI_IIOCGETCPS:
      POST_MEM_WRITE( ARG3, VKI_ISDN_MAX_CHANNELS * 2 * sizeof(unsigned long) );
      break;
   case VKI_IIOCNETGPN:
      POST_MEM_WRITE( ARG3, sizeof(vki_isdn_net_ioctl_phone) );
      break;

      /* These all use struct ifreq AFAIK */
   case VKI_SIOCGIFINDEX:        /* get iface index              */
      POST_MEM_WRITE( (Addr)&((struct vki_ifreq *)ARG3)->vki_ifr_ifindex,
                      sizeof(((struct vki_ifreq *)ARG3)->vki_ifr_ifindex) );
      break;
   case VKI_SIOCGIFFLAGS:        /* get flags                    */
      POST_MEM_WRITE( (Addr)&((struct vki_ifreq *)ARG3)->vki_ifr_flags,
                      sizeof(((struct vki_ifreq *)ARG3)->vki_ifr_flags) );
      break;
   case VKI_SIOCGIFHWADDR:       /* Get hardware address         */
      POST_MEM_WRITE( (Addr)&((struct vki_ifreq *)ARG3)->ifr_hwaddr,
                      sizeof(((struct vki_ifreq *)ARG3)->ifr_hwaddr) );
      break;
   case VKI_SIOCGIFMTU:          /* get MTU size                 */
      POST_MEM_WRITE( (Addr)&((struct vki_ifreq *)ARG3)->vki_ifr_mtu,
                      sizeof(((struct vki_ifreq *)ARG3)->vki_ifr_mtu) );
      break;
   case VKI_SIOCGIFADDR:         /* get PA address               */
   case VKI_SIOCGIFDSTADDR:      /* get remote PA address        */
   case VKI_SIOCGIFBRDADDR:      /* get broadcast PA address     */
   case VKI_SIOCGIFNETMASK:      /* get network PA mask          */
      POST_MEM_WRITE(
                (Addr)&((struct vki_ifreq *)ARG3)->ifr_addr,
                sizeof(((struct vki_ifreq *)ARG3)->ifr_addr) );
      break;
   case VKI_SIOCGIFMETRIC:       /* get metric                   */
      POST_MEM_WRITE(
                (Addr)&((struct vki_ifreq *)ARG3)->vki_ifr_metric,
                sizeof(((struct vki_ifreq *)ARG3)->vki_ifr_metric) );
      break;
   case VKI_SIOCGIFMAP:          /* Get device parameters        */
      POST_MEM_WRITE(
                (Addr)&((struct vki_ifreq *)ARG3)->ifr_map,
                sizeof(((struct vki_ifreq *)ARG3)->ifr_map) );
      break;
     break;
   case VKI_SIOCGIFTXQLEN:       /* Get the tx queue length      */
      POST_MEM_WRITE(
                (Addr)&((struct vki_ifreq *)ARG3)->ifr_qlen,
                sizeof(((struct vki_ifreq *)ARG3)->ifr_qlen) );
      break;
   case VKI_SIOCGIFNAME:         /* get iface name               */
      POST_MEM_WRITE(
                (Addr)&((struct vki_ifreq *)ARG3)->vki_ifr_name,
                sizeof(((struct vki_ifreq *)ARG3)->vki_ifr_name) );
      break;
   case VKI_SIOCGMIIPHY:         /* get hardware entry           */
      POST_MEM_WRITE(
                (Addr)&((struct vki_mii_ioctl_data *)&((struct vki_ifreq *)ARG3)->vki_ifr_data)->phy_id,
                sizeof(((struct vki_mii_ioctl_data *)&((struct vki_ifreq *)ARG3)->vki_ifr_data)->phy_id) );
      break;
   case VKI_SIOCGMIIREG:         /* get hardware entry registers */
      POST_MEM_WRITE(
                (Addr)&((struct vki_mii_ioctl_data *)&((struct vki_ifreq *)ARG3)->vki_ifr_data)->val_out,
                sizeof(((struct vki_mii_ioctl_data *)&((struct vki_ifreq *)ARG3)->vki_ifr_data)->val_out) );
      break;
   case VKI_SIOCGIFCONF:         /* get iface list               */
      /* WAS:
	 PRE_MEM_WRITE("ioctl(SIOCGIFCONF)", ARG3, sizeof(struct ifconf));
	 KERNEL_DO_SYSCALL(tid,RES);
	 if (!VG_(is_kerror)(RES) && RES == 0)
	 POST_MEM_WRITE(ARG3, sizeof(struct ifconf));
      */
      if (RES == 0 && ARG3 ) {
	 struct vki_ifconf *ifc = (struct vki_ifconf *) ARG3;
	 if (ifc->vki_ifc_buf != NULL)
	    POST_MEM_WRITE( (Addr)(ifc->vki_ifc_buf), ifc->ifc_len );
      }
      break;
   case VKI_SIOCGSTAMP:
      POST_MEM_WRITE( ARG3, sizeof(struct vki_timeval) );
      break;
   case VKI_SIOCGSTAMPNS:
      POST_MEM_WRITE( ARG3, sizeof(struct vki_timespec) );
      break;
      /* SIOCOUTQ is an ioctl that, when called on a socket, returns
	 the number of bytes currently in that socket's send buffer.
	 It writes this value as an int to the memory location
	 indicated by the third argument of ioctl(2). */
   case VKI_SIOCOUTQ:
      POST_MEM_WRITE(ARG3, sizeof(int));
      break;
   case VKI_SIOCGRARP:           /* get RARP table entry         */
   case VKI_SIOCGARP:            /* get ARP table entry          */
      POST_MEM_WRITE(ARG3, sizeof(struct vki_arpreq));
      break;
                    
   case VKI_SIOCSIFFLAGS:        /* set flags                    */
   case VKI_SIOCSIFMAP:          /* Set device parameters        */
   case VKI_SIOCSIFTXQLEN:       /* Set the tx queue length      */
   case VKI_SIOCSIFDSTADDR:      /* set remote PA address        */
   case VKI_SIOCSIFBRDADDR:      /* set broadcast PA address     */
   case VKI_SIOCSIFNETMASK:      /* set network PA mask          */
   case VKI_SIOCSIFMETRIC:       /* set metric                   */
   case VKI_SIOCSIFADDR:         /* set PA address               */
   case VKI_SIOCSIFMTU:          /* set MTU size                 */
   case VKI_SIOCSIFHWADDR:       /* set hardware address         */
   case VKI_SIOCSMIIREG:         /* set hardware entry registers */
      break;
      /* Routing table calls.  */
   case VKI_SIOCADDRT:           /* add routing table entry      */
   case VKI_SIOCDELRT:           /* delete routing table entry   */
      break;

      /* RARP cache control calls. */
   case VKI_SIOCDRARP:           /* delete RARP table entry      */
   case VKI_SIOCSRARP:           /* set RARP table entry         */
      /* ARP cache control calls. */
   case VKI_SIOCSARP:            /* set ARP table entry          */
   case VKI_SIOCDARP:            /* delete ARP table entry       */
      break;

   case VKI_SIOCGPGRP:
      POST_MEM_WRITE(ARG3, sizeof(int));
      break;
   case VKI_SIOCSPGRP:
      break;

      /* linux/soundcard interface (OSS) */
   case VKI_SNDCTL_SEQ_GETOUTCOUNT:
   case VKI_SNDCTL_SEQ_GETINCOUNT:
   case VKI_SNDCTL_SEQ_PERCMODE:
   case VKI_SNDCTL_SEQ_TESTMIDI:
   case VKI_SNDCTL_SEQ_RESETSAMPLES:
   case VKI_SNDCTL_SEQ_NRSYNTHS:
   case VKI_SNDCTL_SEQ_NRMIDIS:
   case VKI_SNDCTL_SEQ_GETTIME:
   case VKI_SNDCTL_DSP_GETBLKSIZE:
   case VKI_SNDCTL_DSP_GETFMTS:
   case VKI_SNDCTL_DSP_SETFMT:
   case VKI_SNDCTL_DSP_GETTRIGGER:
   case VKI_SNDCTL_DSP_GETODELAY:
   case VKI_SNDCTL_DSP_GETSPDIF:
   case VKI_SNDCTL_DSP_GETCAPS:
   case VKI_SOUND_PCM_READ_RATE:
   case VKI_SOUND_PCM_READ_CHANNELS:
   case VKI_SOUND_PCM_READ_BITS:
   case VKI_SOUND_PCM_READ_FILTER:
      POST_MEM_WRITE(ARG3, sizeof(int));
      break;
   case VKI_SNDCTL_SEQ_CTRLRATE:
   case VKI_SNDCTL_DSP_SPEED:
   case VKI_SNDCTL_DSP_STEREO:
   case VKI_SNDCTL_DSP_CHANNELS:
   case VKI_SOUND_PCM_WRITE_FILTER:
   case VKI_SNDCTL_DSP_SUBDIVIDE:
   case VKI_SNDCTL_DSP_SETFRAGMENT:
   case VKI_SNDCTL_DSP_GETCHANNELMASK:
   case VKI_SNDCTL_DSP_BIND_CHANNEL:
   case VKI_SNDCTL_TMR_TIMEBASE:
   case VKI_SNDCTL_TMR_TEMPO:
   case VKI_SNDCTL_TMR_SOURCE:
   case VKI_SNDCTL_MIDI_PRETIME:
   case VKI_SNDCTL_MIDI_MPUMODE:
      break;
   case VKI_SNDCTL_DSP_GETOSPACE:
   case VKI_SNDCTL_DSP_GETISPACE:
      POST_MEM_WRITE(ARG3, sizeof(vki_audio_buf_info));
      break;
   case VKI_SNDCTL_DSP_NONBLOCK:
      break;
   case VKI_SNDCTL_DSP_SETTRIGGER:
      break;

   case VKI_SNDCTL_DSP_POST:
   case VKI_SNDCTL_DSP_RESET:
   case VKI_SNDCTL_DSP_SYNC:
   case VKI_SNDCTL_DSP_SETSYNCRO:
   case VKI_SNDCTL_DSP_SETDUPLEX:
      break;

      /* linux/soundcard interface (ALSA) */
   case VKI_SNDRV_PCM_IOCTL_HW_FREE:
   case VKI_SNDRV_PCM_IOCTL_HWSYNC:
   case VKI_SNDRV_PCM_IOCTL_PREPARE:
   case VKI_SNDRV_PCM_IOCTL_RESET:
   case VKI_SNDRV_PCM_IOCTL_START:
   case VKI_SNDRV_PCM_IOCTL_DROP:
   case VKI_SNDRV_PCM_IOCTL_DRAIN:
   case VKI_SNDRV_PCM_IOCTL_RESUME:
   case VKI_SNDRV_PCM_IOCTL_XRUN:
   case VKI_SNDRV_PCM_IOCTL_UNLINK:
   case VKI_SNDRV_TIMER_IOCTL_START:
   case VKI_SNDRV_TIMER_IOCTL_STOP:
   case VKI_SNDRV_TIMER_IOCTL_CONTINUE:
   case VKI_SNDRV_TIMER_IOCTL_PAUSE:
      break;

      /* Real Time Clock (/dev/rtc) ioctls */
   case VKI_RTC_UIE_ON:
   case VKI_RTC_UIE_OFF:
   case VKI_RTC_AIE_ON:
   case VKI_RTC_AIE_OFF:
   case VKI_RTC_PIE_ON:
   case VKI_RTC_PIE_OFF:
   case VKI_RTC_IRQP_SET:
      break;
   case VKI_RTC_RD_TIME:
   case VKI_RTC_ALM_READ:
      POST_MEM_WRITE(ARG3, sizeof(struct vki_rtc_time));
      break;
   case VKI_RTC_ALM_SET:
      break;
   case VKI_RTC_IRQP_READ:
      POST_MEM_WRITE(ARG3, sizeof(unsigned long));
      break;

      /* Block devices */
   case VKI_BLKROSET:
      break;
   case VKI_BLKROGET:
      POST_MEM_WRITE(ARG3, sizeof(int));
      break;
   case VKI_BLKGETSIZE:
      POST_MEM_WRITE(ARG3, sizeof(unsigned long));
      break;
   case VKI_BLKRASET:
      break;
   case VKI_BLKRAGET:
      POST_MEM_WRITE(ARG3, sizeof(long));
      break;
   case VKI_BLKFRASET:
      break;
   case VKI_BLKFRAGET:
      POST_MEM_WRITE(ARG3, sizeof(long));
      break;
   case VKI_BLKSECTGET:
      POST_MEM_WRITE(ARG3, sizeof(unsigned short));
      break;
   case VKI_BLKSSZGET:
      POST_MEM_WRITE(ARG3, sizeof(int));
      break;
   case VKI_BLKBSZGET:
      POST_MEM_WRITE(ARG3, sizeof(int));
      break;
   case VKI_BLKBSZSET:
      break;
   case VKI_BLKGETSIZE64:
      POST_MEM_WRITE(ARG3, sizeof(unsigned long long));
      break;

      /* Hard disks */
   case VKI_HDIO_GETGEO: /* 0x0301 */
      POST_MEM_WRITE(ARG3, sizeof(struct vki_hd_geometry));
      break;
   case VKI_HDIO_GET_DMA: /* 0x030b */
      POST_MEM_WRITE(ARG3, sizeof(long));
      break;
   case VKI_HDIO_GET_IDENTITY: /* 0x030d */
      POST_MEM_WRITE(ARG3, VKI_SIZEOF_STRUCT_HD_DRIVEID );
      break;

      /* CD ROM stuff (??)  */
   case VKI_CDROMSUBCHNL:
      POST_MEM_WRITE(ARG3, sizeof(struct vki_cdrom_subchnl));
      break;
   case VKI_CDROMREADTOCHDR:
      POST_MEM_WRITE(ARG3, sizeof(struct vki_cdrom_tochdr));
      break;
   case VKI_CDROMREADTOCENTRY:
      POST_MEM_WRITE(ARG3, sizeof(struct vki_cdrom_tocentry));
      break;
   case VKI_CDROMMULTISESSION:
      POST_MEM_WRITE(ARG3, sizeof(struct vki_cdrom_multisession));
      break;
   case VKI_CDROMVOLREAD:
      POST_MEM_WRITE(ARG3, sizeof(struct vki_cdrom_volctrl));
      break;
   case VKI_CDROMREADRAW:
      POST_MEM_WRITE(ARG3, VKI_CD_FRAMESIZE_RAW);
      break;
   case VKI_CDROMREADAUDIO:
   {
      struct vki_cdrom_read_audio *cra = (struct vki_cdrom_read_audio *) ARG3;
      POST_MEM_WRITE( (Addr)(cra->buf), cra->nframes * VKI_CD_FRAMESIZE_RAW);
      break;
   }
      
   case VKI_CDROMPLAYMSF:
      break;
      /* The following two are probably bogus (should check args
	 for readability).  JRS 20021117 */
   case VKI_CDROM_DRIVE_STATUS: /* 0x5326 */
   case VKI_CDROM_CLEAR_OPTIONS: /* 0x5321 */
      break;

   case VKI_FIGETBSZ:
      POST_MEM_WRITE(ARG3, sizeof(unsigned long));
      break;
   case VKI_FIBMAP:
      POST_MEM_WRITE(ARG3, sizeof(int));
      break;

   case VKI_FBIOGET_VSCREENINFO: //0x4600
      POST_MEM_WRITE(ARG3, sizeof(struct vki_fb_var_screeninfo));
      break;
   case VKI_FBIOGET_FSCREENINFO: //0x4602
      POST_MEM_WRITE(ARG3, sizeof(struct vki_fb_fix_screeninfo));
      break;

   case VKI_PPCLAIM:
   case VKI_PPEXCL:
   case VKI_PPYIELD:
   case VKI_PPRELEASE:
   case VKI_PPSETMODE:
   case VKI_PPSETPHASE:
   case VKI_PPSETFLAGS:
   case VKI_PPWDATA:
   case VKI_PPWCONTROL:
   case VKI_PPFCONTROL:
   case VKI_PPDATADIR:
   case VKI_PPNEGOT:
   case VKI_PPWCTLONIRQ:
   case VKI_PPSETTIME:
      break;
   case VKI_PPGETMODE:
      POST_MEM_WRITE( ARG3, sizeof(int) );
      break;
   case VKI_PPGETPHASE:
      POST_MEM_WRITE( ARG3, sizeof(int) );
      break;
   case VKI_PPGETMODES:
      POST_MEM_WRITE( ARG3, sizeof(unsigned int) );
      break;
   case VKI_PPGETFLAGS:
      POST_MEM_WRITE( ARG3, sizeof(int) );
      break;
   case VKI_PPRSTATUS:
      POST_MEM_WRITE( ARG3, sizeof(unsigned char) );
      break;
   case VKI_PPRDATA:
      POST_MEM_WRITE( ARG3, sizeof(unsigned char) );
      break;
   case VKI_PPRCONTROL:
      POST_MEM_WRITE( ARG3, sizeof(unsigned char) );
      break;
   case VKI_PPCLRIRQ:
      POST_MEM_WRITE( ARG3, sizeof(int) );
      break;
   case VKI_PPGETTIME:
      POST_MEM_WRITE( ARG3, sizeof(struct vki_timeval) );
      break;

   case VKI_GIO_FONT:
      POST_MEM_WRITE( ARG3, 32 * 256 );
      break;
   case VKI_PIO_FONT:
      break;

   case VKI_GIO_FONTX:
      POST_MEM_WRITE( (Addr)((struct vki_consolefontdesc *)ARG3)->chardata,
                      32 * ((struct vki_consolefontdesc *)ARG3)->charcount );
      break;
   case VKI_PIO_FONTX:
      break;

   case VKI_PIO_FONTRESET:
      break;

   case VKI_GIO_CMAP:
      POST_MEM_WRITE( ARG3, 16 * 3 );
      break;
   case VKI_PIO_CMAP:
      break;

   case VKI_KIOCSOUND:
   case VKI_KDMKTONE:
      break;

   case VKI_KDGETLED:
      POST_MEM_WRITE( ARG3, sizeof(char) );
      break;
   case VKI_KDSETLED:
      break;

   case VKI_KDGKBTYPE:
      POST_MEM_WRITE( ARG3, sizeof(char) );
      break;

   case VKI_KDADDIO:
   case VKI_KDDELIO:
   case VKI_KDENABIO:
   case VKI_KDDISABIO:
      break;

   case VKI_KDSETMODE:
      break;
   case VKI_KDGETMODE:
      POST_MEM_WRITE( ARG3, sizeof(int) );
      break;

   case VKI_KDMAPDISP:
   case VKI_KDUNMAPDISP:
      break;

   case VKI_GIO_SCRNMAP:
      POST_MEM_WRITE( ARG3, VKI_E_TABSZ );
      break;
   case VKI_PIO_SCRNMAP:
      break;
   case VKI_GIO_UNISCRNMAP:
      POST_MEM_WRITE( ARG3, VKI_E_TABSZ * sizeof(unsigned short) );
      break;
   case VKI_PIO_UNISCRNMAP:
      break;

   case VKI_GIO_UNIMAP:
      if ( ARG3 ) {
         struct vki_unimapdesc *desc = (struct vki_unimapdesc *) ARG3;
         POST_MEM_WRITE( (Addr)&desc->entry_ct, sizeof(desc->entry_ct));
         POST_MEM_WRITE( (Addr)desc->entries,
      	                 desc->entry_ct * sizeof(struct vki_unipair) );
      }
      break;
   case VKI_PIO_UNIMAP:
      break;
   case VKI_PIO_UNIMAPCLR:
      break;

   case VKI_KDGKBMODE:
      POST_MEM_WRITE( ARG3, sizeof(int) );
      break;
   case VKI_KDSKBMODE:
      break;
      
   case VKI_KDGKBMETA:
      POST_MEM_WRITE( ARG3, sizeof(int) );
      break;
   case VKI_KDSKBMETA:
      break;
      
   case VKI_KDGKBLED:
      POST_MEM_WRITE( ARG3, sizeof(char) );
      break;
   case VKI_KDSKBLED:
      break;
      
   case VKI_KDGKBENT:
      POST_MEM_WRITE( (Addr)&((struct vki_kbentry *)ARG3)->kb_value,
                      sizeof(((struct vki_kbentry *)ARG3)->kb_value) );
      break;
   case VKI_KDSKBENT:
      break;
      
   case VKI_KDGKBSENT:
      POST_MEM_WRITE( (Addr)((struct vki_kbsentry *)ARG3)->kb_string,
                      sizeof(((struct vki_kbsentry *)ARG3)->kb_string) );
      break;
   case VKI_KDSKBSENT:
      break;
      
   case VKI_KDGKBDIACR:
      POST_MEM_WRITE( ARG3, sizeof(struct vki_kbdiacrs) );
      break;
   case VKI_KDSKBDIACR:
      break;
      
   case VKI_KDGETKEYCODE:
      POST_MEM_WRITE( (Addr)((struct vki_kbkeycode *)ARG3)->keycode,
                      sizeof(((struct vki_kbkeycode *)ARG3)->keycode) );
      break;
   case VKI_KDSETKEYCODE:
      break;
      
   case VKI_KDSIGACCEPT:
      break;

   case VKI_KDKBDREP:
      break;

   case VKI_KDFONTOP:
      if ( ARG3 ) {
         struct vki_console_font_op *op = (struct vki_console_font_op *) ARG3;
         switch ( op->op ) {
            case VKI_KD_FONT_OP_SET:
               break;
            case VKI_KD_FONT_OP_GET:
               if ( op->data )
                  POST_MEM_WRITE( (Addr) op->data,
                                  (op->width + 7) / 8 * 32 * op->charcount );
               break;
            case VKI_KD_FONT_OP_SET_DEFAULT:
               break;
            case VKI_KD_FONT_OP_COPY:
               break;
         }
         POST_MEM_WRITE( (Addr) op, sizeof(*op));
      }
      break;

   case VKI_VT_OPENQRY:
      POST_MEM_WRITE( ARG3, sizeof(int) );
      break;
   case VKI_VT_GETMODE:
      POST_MEM_WRITE( ARG3, sizeof(struct vki_vt_mode) );
      break;
   case VKI_VT_SETMODE:
      break;
   case VKI_VT_GETSTATE:
      POST_MEM_WRITE( (Addr) &(((struct vki_vt_stat*) ARG3)->v_active),
                      sizeof(((struct vki_vt_stat*) ARG3)->v_active) );
      POST_MEM_WRITE( (Addr) &(((struct vki_vt_stat*) ARG3)->v_state),
                      sizeof(((struct vki_vt_stat*) ARG3)->v_state) );
      break;
   case VKI_VT_RELDISP:
   case VKI_VT_ACTIVATE:
   case VKI_VT_WAITACTIVE:
   case VKI_VT_DISALLOCATE:
      break;
   case VKI_VT_RESIZE:
      break;
   case VKI_VT_RESIZEX:
      break;
   case VKI_VT_LOCKSWITCH:
   case VKI_VT_UNLOCKSWITCH:
      break;

   case VKI_USBDEVFS_CONTROL:
      if ( ARG3 ) {
         struct vki_usbdevfs_ctrltransfer *vkuc = (struct vki_usbdevfs_ctrltransfer *)ARG3;
         if (vkuc->bRequestType & 0x80)
            POST_MEM_WRITE((Addr)vkuc->data, RES);
         break;
      }
   case VKI_USBDEVFS_BULK:
      if ( ARG3 ) {
         struct vki_usbdevfs_bulktransfer *vkub = (struct vki_usbdevfs_bulktransfer *)ARG3;
         if (vkub->ep & 0x80)
            POST_MEM_WRITE((Addr)vkub->data, RES);
         break;
      }
   case VKI_USBDEVFS_GETDRIVER:
      if ( ARG3 ) {
         struct vki_usbdevfs_getdriver *vkugd = (struct vki_usbdevfs_getdriver *)ARG3;
         POST_MEM_WRITE((Addr)&vkugd->driver, sizeof(vkugd->driver));
         break;
      }
   case VKI_USBDEVFS_REAPURB:
   case VKI_USBDEVFS_REAPURBNDELAY:
      if ( ARG3 ) {
         struct vki_usbdevfs_urb **vkuu = (struct vki_usbdevfs_urb**)ARG3;
         POST_MEM_WRITE((Addr)vkuu, sizeof(*vkuu));
         if (!*vkuu)
            break;
         POST_MEM_WRITE((Addr) &((*vkuu)->status),sizeof((*vkuu)->status));
         if ((*vkuu)->type == VKI_USBDEVFS_URB_TYPE_CONTROL) {
            struct vki_usbdevfs_setuppacket *vkusp = (struct vki_usbdevfs_setuppacket *)(*vkuu)->buffer;
            if (vkusp->bRequestType & 0x80)
               POST_MEM_WRITE((Addr)(vkusp+1), (*vkuu)->buffer_length - sizeof(*vkusp));
            POST_MEM_WRITE((Addr)&(*vkuu)->actual_length, sizeof((*vkuu)->actual_length));
         } else if ((*vkuu)->type == VKI_USBDEVFS_URB_TYPE_ISO) {
            char *bp = (*vkuu)->buffer;
            int i;
            for(i=0; i<(*vkuu)->number_of_packets; i++) {
               POST_MEM_WRITE((Addr)&(*vkuu)->iso_frame_desc[i].actual_length, sizeof((*vkuu)->iso_frame_desc[i].actual_length));
               POST_MEM_WRITE((Addr)&(*vkuu)->iso_frame_desc[i].status, sizeof((*vkuu)->iso_frame_desc[i].status));
               if ((*vkuu)->endpoint & 0x80)
                  POST_MEM_WRITE((Addr)bp, (*vkuu)->iso_frame_desc[i].actual_length);
               bp += (*vkuu)->iso_frame_desc[i].length; // FIXME: or actual_length??
            }
            POST_MEM_WRITE((Addr)&(*vkuu)->error_count, sizeof((*vkuu)->error_count));
         } else {
            if ((*vkuu)->endpoint & 0x80)
               POST_MEM_WRITE((Addr)(*vkuu)->buffer, (*vkuu)->actual_length);
            POST_MEM_WRITE((Addr)&(*vkuu)->actual_length, sizeof((*vkuu)->actual_length));
         }
         break;
      }
   case VKI_USBDEVFS_CONNECTINFO:
      POST_MEM_WRITE(ARG3, sizeof(struct vki_usbdevfs_connectinfo));
      break;
   case VKI_USBDEVFS_IOCTL:
      if ( ARG3 ) {
         struct vki_usbdevfs_ioctl *vkui = (struct vki_usbdevfs_ioctl *)ARG3;
         UInt dir2, size2;
         dir2  = _VKI_IOC_DIR(vkui->ioctl_code);
         size2 = _VKI_IOC_SIZE(vkui->ioctl_code);
         if (size2 > 0) {
            if (dir2 & _VKI_IOC_READ) 
               POST_MEM_WRITE((Addr)vkui->data, size2);
         }
      }
      break;

      /* I2C (/dev/i2c-*) ioctls */
   case VKI_I2C_SLAVE:
   case VKI_I2C_SLAVE_FORCE:
   case VKI_I2C_TENBIT:
   case VKI_I2C_PEC:
      break;
   case VKI_I2C_FUNCS:
      POST_MEM_WRITE( ARG3, sizeof(unsigned long) );
      break;

      /* Wireless extensions ioctls */
   case VKI_SIOCSIWCOMMIT:
   case VKI_SIOCSIWNWID:
   case VKI_SIOCSIWFREQ:
   case VKI_SIOCSIWMODE:
   case VKI_SIOCSIWSENS:
   case VKI_SIOCSIWRANGE:
   case VKI_SIOCSIWPRIV:
   case VKI_SIOCSIWSTATS:
   case VKI_SIOCSIWSPY:
   case VKI_SIOCSIWTHRSPY:
   case VKI_SIOCSIWAP:
   case VKI_SIOCSIWSCAN:
   case VKI_SIOCSIWESSID:
   case VKI_SIOCSIWRATE:
   case VKI_SIOCSIWNICKN:
   case VKI_SIOCSIWRTS:
   case VKI_SIOCSIWFRAG:
   case VKI_SIOCSIWTXPOW:
   case VKI_SIOCSIWRETRY:
   case VKI_SIOCSIWENCODE:
   case VKI_SIOCSIWPOWER:
   case VKI_SIOCSIWGENIE:
   case VKI_SIOCSIWMLME:
   case VKI_SIOCSIWAUTH:
   case VKI_SIOCSIWENCODEEXT:
   case VKI_SIOCSIWPMKSA:
      break;
   case VKI_SIOCGIWNAME:
      if (ARG3) {
         POST_MEM_WRITE((Addr)((struct vki_iwreq *)ARG3)->u.name,
                        sizeof(((struct vki_iwreq *)ARG3)->u.name));
      }
      break;
   case VKI_SIOCGIWNWID:
   case VKI_SIOCGIWSENS:
   case VKI_SIOCGIWRATE:
   case VKI_SIOCGIWRTS:
   case VKI_SIOCGIWFRAG:
   case VKI_SIOCGIWTXPOW:
   case VKI_SIOCGIWRETRY:
   case VKI_SIOCGIWPOWER:
   case VKI_SIOCGIWAUTH:
      if (ARG3) {
         POST_MEM_WRITE((Addr)&((struct vki_iwreq *)ARG3)->u.param,
                        sizeof(struct vki_iw_param));
      }
      break;
   case VKI_SIOCGIWFREQ:
      if (ARG3) {
         POST_MEM_WRITE((Addr)&((struct vki_iwreq *)ARG3)->u.freq,
                        sizeof(struct vki_iw_freq));
      }
      break;
   case VKI_SIOCGIWMODE:
      if (ARG3) {
         POST_MEM_WRITE((Addr)&((struct vki_iwreq *)ARG3)->u.mode,
                       sizeof(__vki_u32));
      }
      break;
   case VKI_SIOCGIWRANGE:
   case VKI_SIOCGIWPRIV:
   case VKI_SIOCGIWSTATS:
   case VKI_SIOCGIWSPY:
   case VKI_SIOCGIWTHRSPY:
   case VKI_SIOCGIWAPLIST:
   case VKI_SIOCGIWSCAN:
   case VKI_SIOCGIWESSID:
   case VKI_SIOCGIWNICKN:
   case VKI_SIOCGIWENCODE:
   case VKI_SIOCGIWGENIE:
   case VKI_SIOCGIWENCODEEXT:
      if (ARG3) {
         struct vki_iw_point* point;
         point = &((struct vki_iwreq *)ARG3)->u.data;
         POST_MEM_WRITE((Addr)point->pointer, point->length);
      }
      break;
   case VKI_SIOCGIWAP:
      if (ARG3) {
         POST_MEM_WRITE((Addr)&((struct vki_iwreq *)ARG3)->u.ap_addr,
                        sizeof(struct vki_sockaddr));
      }
      break;

   default:
      /* EVIOC* are variable length and return size written on success */
      switch (ARG2 & ~(_VKI_IOC_SIZEMASK << _VKI_IOC_SIZESHIFT)) {
      case VKI_EVIOCGNAME(0):
      case VKI_EVIOCGPHYS(0):
      case VKI_EVIOCGUNIQ(0):
      case VKI_EVIOCGKEY(0):
      case VKI_EVIOCGLED(0):
      case VKI_EVIOCGSND(0):
      case VKI_EVIOCGSW(0):
      case VKI_EVIOCGBIT(VKI_EV_SYN,0):
      case VKI_EVIOCGBIT(VKI_EV_KEY,0):
      case VKI_EVIOCGBIT(VKI_EV_REL,0):
      case VKI_EVIOCGBIT(VKI_EV_ABS,0):
      case VKI_EVIOCGBIT(VKI_EV_MSC,0):
      case VKI_EVIOCGBIT(VKI_EV_SW,0):
      case VKI_EVIOCGBIT(VKI_EV_LED,0):
      case VKI_EVIOCGBIT(VKI_EV_SND,0):
      case VKI_EVIOCGBIT(VKI_EV_REP,0):
      case VKI_EVIOCGBIT(VKI_EV_FF,0):
      case VKI_EVIOCGBIT(VKI_EV_PWR,0):
      case VKI_EVIOCGBIT(VKI_EV_FF_STATUS,0):
         if (RES > 0)
            POST_MEM_WRITE(ARG3, RES);
         break;
      default:
         ML_(POST_unknown_ioctl)(tid, RES, ARG2, ARG3);
         break;
      }
      break;
   }
}

/* ---------------------------------------------------------------------
   socketcall wrapper helpers
   ------------------------------------------------------------------ */

void 
ML_(linux_PRE_sys_getsockopt) ( ThreadId tid, 
                                UWord arg0, UWord arg1, UWord arg2,
                                UWord arg3, UWord arg4 )
{
   /* int getsockopt(int s, int level, int optname, 
                     void *optval, socklen_t *optlen); */
   Addr optval_p = arg3;
   Addr optlen_p = arg4;
   /* vg_assert(sizeof(socklen_t) == sizeof(UInt)); */
   if (optval_p != (Addr)NULL) {
      ML_(buf_and_len_pre_check) ( tid, optval_p, optlen_p,
                                   "socketcall.getsockopt(optval)",
                                   "socketcall.getsockopt(optlen)" );
      if (arg1 == VKI_SOL_SCTP &&
          (arg2 == VKI_SCTP_GET_PEER_ADDRS || 
           arg2 == VKI_SCTP_GET_LOCAL_ADDRS))
      {
         struct vki_sctp_getaddrs *ga = (struct vki_sctp_getaddrs*)arg3;
         int address_bytes = sizeof(struct vki_sockaddr_in6) * ga->addr_num;
         PRE_MEM_WRITE( "socketcall.getsockopt(optval.addrs)",
                        (Addr)ga->addrs, address_bytes );
      }
   }
}

void 
ML_(linux_POST_sys_getsockopt) ( ThreadId tid,
                                 SysRes res,
                                 UWord arg0, UWord arg1, UWord arg2,
                                 UWord arg3, UWord arg4 )
{
   Addr optval_p = arg3;
   Addr optlen_p = arg4;
   vg_assert(!sr_isError(res)); /* guaranteed by caller */
   if (optval_p != (Addr)NULL) {
      ML_(buf_and_len_post_check) ( tid, res, optval_p, optlen_p,
                                    "socketcall.getsockopt(optlen_out)" );
      if (arg1 == VKI_SOL_SCTP &&
          (arg2 == VKI_SCTP_GET_PEER_ADDRS ||
           arg2 == VKI_SCTP_GET_LOCAL_ADDRS))
      {
         struct vki_sctp_getaddrs *ga = (struct vki_sctp_getaddrs*)arg3;    
         struct vki_sockaddr *a = ga->addrs;
         int i;
         for (i = 0; i < ga->addr_num; i++) {
            int sl = 0;
            if (a->sa_family == VKI_AF_INET)
               sl = sizeof(struct vki_sockaddr_in);
            else if (a->sa_family == VKI_AF_INET6)
               sl = sizeof(struct vki_sockaddr_in6);
            else {
               VG_(message)(Vg_UserMsg, "Warning: getsockopt: unhandled "
                                        "address type %d\n", a->sa_family);
            }
            a = (struct vki_sockaddr*)((char*)a + sl);
         }
         POST_MEM_WRITE( (Addr)ga->addrs, (char*)a - (char*)ga->addrs );    
      }
   }
}

#undef PRE
#undef POST

#endif // defined(VGO_linux)

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
