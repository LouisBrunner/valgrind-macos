
/*--------------------------------------------------------------------*/
/*--- Linux-specific syscalls, etc.                syswrap-linux.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2015 Nicholas Nethercote
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
#include "pub_core_machine.h"      // VG_(get_SP)
#include "pub_core_mallocfree.h"
#include "pub_core_tooliface.h"
#include "pub_core_options.h"
#include "pub_core_scheduler.h"
#include "pub_core_signals.h"
#include "pub_core_syscall.h"
#include "pub_core_syswrap.h"
#include "pub_core_inner.h"
#if defined(ENABLE_INNER_CLIENT_REQUEST)
#include "pub_core_clreq.h"
#endif

#include "priv_types_n_macros.h"
#include "priv_syswrap-generic.h"
#include "priv_syswrap-linux.h"
#include "priv_syswrap-xen.h"

// Run a thread from beginning to end and return the thread's
// scheduler-return-code.
static VgSchedReturnCode thread_wrapper(Word /*ThreadId*/ tidW)
{
   VgSchedReturnCode ret;
   ThreadId     tid = (ThreadId)tidW;
   ThreadState* tst = VG_(get_ThreadState)(tid);

   VG_(debugLog)(1, "syswrap-linux", 
                    "thread_wrapper(tid=%u): entry\n", 
                    tid);

   vg_assert(tst->status == VgTs_Init);

   /* make sure we get the CPU lock before doing anything significant */
   VG_(acquire_BigLock)(tid, "thread_wrapper(starting new thread)");

   if (0)
      VG_(printf)("thread tid %u started: stack = %p\n",
		  tid, (void *)&tid);

   /* Make sure error reporting is enabled in the new thread. */
   tst->err_disablement_level = 0;

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
                    "thread_wrapper(tid=%u): exit, schedreturncode %s\n", 
                    tid, VG_(name_of_VgSchedReturnCode)(ret));

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
   ThreadState*      tst;
#ifdef ENABLE_INNER_CLIENT_REQUEST
   Int               registered_vgstack_id;
#endif

   VG_(debugLog)(1, "syswrap-linux", 
                    "run_a_thread_NORETURN(tid=%u): pre-thread_wrapper\n",
                    tid);

   tst = VG_(get_ThreadState)(tid);
   vg_assert(tst);

   /* An thread has two stacks:
      * the simulated stack (used by the synthetic cpu. Guest process
        is using this stack).
      * the valgrind stack (used by the real cpu. Valgrind code is running
        on this stack).
      When Valgrind runs as an inner, it must signals that its (real) stack
      is the stack to use by the outer to e.g. do stacktraces.
   */
   INNER_REQUEST
      (registered_vgstack_id 
       = VALGRIND_STACK_REGISTER (tst->os_state.valgrind_stack_base,
                                  tst->os_state.valgrind_stack_init_SP));
   
   /* Run the thread all the way through. */
   src = thread_wrapper(tid);  

   VG_(debugLog)(1, "syswrap-linux", 
                    "run_a_thread_NORETURN(tid=%u): post-thread_wrapper\n",
                    tid);

   c = VG_(count_living_threads)();
   vg_assert(c >= 1); /* stay sane */

   // Tell the tool this thread is exiting
   VG_TRACK( pre_thread_ll_exit, tid );

   /* If the thread is exiting with errors disabled, complain loudly;
      doing so is bad (does the user know this has happened?)  Also,
      in all cases, be paranoid and clear the flag anyway so that the
      thread slot is safe in this respect if later reallocated.  This
      should be unnecessary since the flag should be cleared when the
      slot is reallocated, in thread_wrapper(). */
   if (tst->err_disablement_level > 0) {
      VG_(umsg)(
         "WARNING: exiting thread has error reporting disabled.\n"
         "WARNING: possibly as a result of some mistake in the use\n"
         "WARNING: of the VALGRIND_DISABLE_ERROR_REPORTING macros.\n"
      );
      VG_(debugLog)(
         1, "syswrap-linux", 
            "run_a_thread_NORETURN(tid=%u): "
            "WARNING: exiting thread has err_disablement_level = %u\n",
            tid, tst->err_disablement_level
      );
   }
   tst->err_disablement_level = 0;

   if (c == 1) {

      VG_(debugLog)(1, "syswrap-linux", 
                       "run_a_thread_NORETURN(tid=%u): "
                          "last one standing\n",
                          tid);

      /* We are the last one standing.  Keep hold of the lock and
         carry on to show final tool results, then exit the entire system. 
         Use the continuation pointer set at startup in m_main. */
      ( * VG_(address_of_m_main_shutdown_actions_NORETURN) ) (tid, src);
   } else {

      VG_(debugLog)(1, "syswrap-linux", 
                       "run_a_thread_NORETURN(tid=%u): "
                          "not last one standing\n",
                          tid);

      /* OK, thread is dead, but others still exist.  Just exit. */

      /* This releases the run lock */
      VG_(exit_thread)(tid);
      vg_assert(tst->status == VgTs_Zombie);
      vg_assert(sizeof(tst->status) == 4);
      vg_assert(sizeof(tst->os_state.exitcode) == sizeof(Word));

      INNER_REQUEST (VALGRIND_STACK_DEREGISTER (registered_vgstack_id));

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
         "pushl %%ebx\n"
         "movl	%1, %0\n"	/* set tst->status = VgTs_Empty */
         "movl	%2, %%eax\n"    /* set %eax = __NR_exit */
         "movl	%3, %%ebx\n"    /* set %ebx = tst->os_state.exitcode */
         "int	$0x80\n"	/* exit(tst->os_state.exitcode) */
	 "popl %%ebx\n"
         : "=m" (tst->status)
         : "n" (VgTs_Empty), "n" (__NR_exit), "m" (tst->os_state.exitcode)
         : "eax"
      );
#elif defined(VGP_amd64_linux)
      asm volatile (
         "movl	%1, %0\n"	/* set tst->status = VgTs_Empty */
         "movq	%2, %%rax\n"    /* set %rax = __NR_exit */
         "movq	%3, %%rdi\n"    /* set %rdi = tst->os_state.exitcode */
         "syscall\n"		/* exit(tst->os_state.exitcode) */
         : "=m" (tst->status)
         : "n" (VgTs_Empty), "n" (__NR_exit), "m" (tst->os_state.exitcode)
         : "rax", "rdi"
      );
#elif defined(VGP_ppc32_linux) || defined(VGP_ppc64be_linux) \
      || defined(VGP_ppc64le_linux)
      { UInt vgts_empty = (UInt)VgTs_Empty;
        asm volatile (
          "stw %1,%0\n\t"          /* set tst->status = VgTs_Empty */
          "li  0,%2\n\t"           /* set r0 = __NR_exit */
          "lwz 3,%3\n\t"           /* set r3 = tst->os_state.exitcode */
          "sc\n\t"                 /* exit(tst->os_state.exitcode) */
          : "=m" (tst->status)
          : "r" (vgts_empty), "n" (__NR_exit), "m" (tst->os_state.exitcode)
          : "r0", "r3"
        );
      }
#elif defined(VGP_arm_linux)
      asm volatile (
         "str  %1, %0\n"      /* set tst->status = VgTs_Empty */
         "mov  r7, %2\n"      /* set %r7 = __NR_exit */
         "ldr  r0, %3\n"      /* set %r0 = tst->os_state.exitcode */
         "svc  0x00000000\n"  /* exit(tst->os_state.exitcode) */
         : "=m" (tst->status)
         : "r" (VgTs_Empty), "n" (__NR_exit), "m" (tst->os_state.exitcode)
         : "r0", "r7"
      );
#elif defined(VGP_arm64_linux)
      asm volatile (
         "str  %w1, %0\n"     /* set tst->status = VgTs_Empty (32-bit store) */
         "mov  x8,  %2\n"     /* set %r7 = __NR_exit */
         "ldr  x0,  %3\n"     /* set %r0 = tst->os_state.exitcode */
         "svc  0x00000000\n"  /* exit(tst->os_state.exitcode) */
         : "=m" (tst->status)
         : "r" (VgTs_Empty), "n" (__NR_exit), "m" (tst->os_state.exitcode)
         : "r0", "r7"
      );
#elif defined(VGP_s390x_linux)
      asm volatile (
         "st   %1, %0\n"        /* set tst->status = VgTs_Empty */
         "lg   2, %3\n"         /* set r2 = tst->os_state.exitcode */
         "svc %2\n"             /* exit(tst->os_state.exitcode) */
         : "=m" (tst->status)
         : "d" (VgTs_Empty), "n" (__NR_exit), "m" (tst->os_state.exitcode)
         : "2"
      );
#elif defined(VGP_mips32_linux) || defined(VGP_mips64_linux)
      asm volatile (
         "sw   %1, %0\n\t"     /* set tst->status = VgTs_Empty */
         "li   $2, %2\n\t"     /* set v0 = __NR_exit */
         "lw   $4, %3\n\t"     /* set a0 = tst->os_state.exitcode */
         "syscall\n\t"         /* exit(tst->os_state.exitcode) */
         "nop"
         : "=m" (tst->status)
         : "r" (VgTs_Empty), "n" (__NR_exit), "m" (tst->os_state.exitcode)
         : "cc", "memory" , "v0", "a0"
      );
#elif defined(VGP_tilegx_linux)
      asm volatile (
         "st4    %0,  %1\n"      /* set tst->status = VgTs_Empty */
         "moveli r10, %2\n"      /* set r10 = __NR_exit */
         "move   r0,  %3\n"      /* set  r0 = tst->os_state.exitcode */
         "swint1\n"              /* exit(tst->os_state.exitcode) */
         : "=m" (tst->status)
         : "r" (VgTs_Empty), "n" (__NR_exit), "r" (tst->os_state.exitcode)
         : "r0", "r1", "r2", "r3", "r4", "r5");
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
      VG_(printf)( "stack for tid %u at %p; init_SP=%p\n",
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
#if defined(ENABLE_INNER_CLIENT_REQUEST)
   {
      // we must register the main thread stack before the call
      // to ML_(call_on_new_stack_0_1), otherwise the outer valgrind
      // reports 'write error' on the non registered stack.
      ThreadState* tst = VG_(get_ThreadState)(tid);
      INNER_REQUEST
         ((void) 
          VALGRIND_STACK_REGISTER (tst->os_state.valgrind_stack_base,
                                   tst->os_state.valgrind_stack_init_SP));
   }
#endif

#if defined(VGP_ppc32_linux)
   /* make a stack frame */
   sp -= 16;
   sp &= ~0xF;
   *(UWord *)sp = 0;
#elif defined(VGP_ppc64be_linux) || defined(VGP_ppc64le_linux)
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
    || defined(VGP_ppc32_linux) \
    || defined(VGP_ppc64be_linux) || defined(VGP_ppc64le_linux)	\
    || defined(VGP_arm_linux) || defined(VGP_mips32_linux) \
    || defined(VGP_mips64_linux) || defined(VGP_arm64_linux)
   res = VG_(do_syscall5)( __NR_clone, flags, 
                           (UWord)NULL, (UWord)parent_tidptr, 
                           (UWord)NULL, (UWord)child_tidptr );
#elif defined(VGP_amd64_linux) || defined(VGP_tilegx_linux)
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
	  VG_(printf)("   clone(fork): process %d created child %lu\n",
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
         ARG1,(HChar*)ARG1, ARG2,(HChar*)ARG2, ARG3,(HChar*)ARG3, ARG4, ARG5);
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
   PRINT("sys_umount( %#lx, %ld )", ARG1, SARG2);
   PRE_REG_READ2(long, "umount2", char *, path, int, flags);
   PRE_MEM_RASCIIZ( "umount2(path)", ARG1);
}

/* Not actually wrapped by GLibc but does things with the system
 * mounts so it is put here.
 */
PRE(sys_pivot_root)
{
   PRINT("sys_pivot_root ( %s %s )", (HChar*)ARG1, (HChar*)ARG2);
   PRE_REG_READ2(int, "pivot_root", char *, new_root, char *, old_root);
   PRE_MEM_RASCIIZ( "pivot_root(new_root)", ARG1);
   PRE_MEM_RASCIIZ( "pivot_root(old_root)", ARG2);
}


/* ---------------------------------------------------------------------
   16- and 32-bit uid/gid wrappers
   ------------------------------------------------------------------ */

PRE(sys_setfsuid16)
{
   PRINT("sys_setfsuid16 ( %lu )", ARG1);
   PRE_REG_READ1(long, "setfsuid16", vki_old_uid_t, uid);
}

PRE(sys_setfsuid)
{
   PRINT("sys_setfsuid ( %lu )", ARG1);
   PRE_REG_READ1(long, "setfsuid", vki_uid_t, uid);
}

PRE(sys_setfsgid16)
{
   PRINT("sys_setfsgid16 ( %lu )", ARG1);
   PRE_REG_READ1(long, "setfsgid16", vki_old_gid_t, gid);
}

PRE(sys_setfsgid)
{
   PRINT("sys_setfsgid ( %lu )", ARG1);
   PRE_REG_READ1(long, "setfsgid", vki_gid_t, gid);
}

PRE(sys_setresuid16)
{
   PRINT("sys_setresuid16 ( %lu, %lu, %lu )", ARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "setresuid16",
                 vki_old_uid_t, ruid, vki_old_uid_t, euid, vki_old_uid_t, suid);
}

PRE(sys_setresuid)
{
   PRINT("sys_setresuid ( %lu, %lu, %lu )", ARG1, ARG2, ARG3);
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
   PRINT("sys_setresgid16 ( %lu, %lu, %lu )", ARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "setresgid16",
                 vki_old_gid_t, rgid, 
                 vki_old_gid_t, egid, vki_old_gid_t, sgid);
}

PRE(sys_setresgid)
{
   PRINT("sys_setresgid ( %lu, %lu, %lu )", ARG1, ARG2, ARG3);
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

   PRINT("exit_group( %ld )", SARG1);
   PRE_REG_READ1(void, "exit_group", int, status);

   tst = VG_(get_ThreadState)(tid);
   /* A little complex; find all the threads with the same threadgroup
      as this one (including this one), and mark them to exit */
   /* It is unclear how one can get a threadgroup in this process which
      is not the threadgroup of the calling thread:
      The assignments to threadgroups are:
        = 0; /// scheduler.c os_state_clear
        = getpid(); /// scheduler.c in child after fork
        = getpid(); /// this file, in thread_wrapper
        = ptst->os_state.threadgroup; /// syswrap-*-linux.c,
                           copying the thread group of the thread doing clone
      So, the only case where the threadgroup might be different to the getpid
      value is in the child, just after fork. But then the fork syscall is
      still going on, the forked thread has had no chance yet to make this
      syscall. */
   for (t = 1; t < VG_N_THREADS; t++) {
      if ( /* not alive */
           VG_(threads)[t].status == VgTs_Empty 
           ||
	   /* not our group */
           VG_(threads)[t].os_state.threadgroup != tst->os_state.threadgroup
         )
         continue;
      /* Assign the exit code, VG_(nuke_all_threads_except) will assign
         the exitreason. */
      VG_(threads)[t].os_state.exitcode = ARG1;
   }

   /* Indicate in all other threads that the process is exiting.
      Then wait using VG_(reap_threads) for these threads to disappear.
      
      Can this give a deadlock if another thread is calling exit in parallel
      and would then wait for this thread to disappear ?
      The answer is no:
      Other threads are either blocked in a syscall or have yielded the CPU.
      
      A thread that has yielded the CPU is trying to get the big lock in
      VG_(scheduler). This thread will get the CPU thanks to the call
      to VG_(reap_threads). The scheduler will then check for signals,
      kill the process if this is a fatal signal, and otherwise prepare
      the thread for handling this signal. After this preparation, if
      the thread status is VG_(is_exiting), the scheduler exits the thread.
      So, a thread that has yielded the CPU does not have a chance to
      call exit => no deadlock for this thread.
      
      VG_(nuke_all_threads_except) will send the VG_SIGVGKILL signal
      to all threads blocked in a syscall.
      The syscall will be interrupted, and the control will go to the
      scheduler. The scheduler will then return, as the thread is in
      exiting state. */

   VG_(nuke_all_threads_except)( tid, VgSrc_ExitProcess );
   VG_(reap_threads)(tid);
   VG_(threads)[tid].exitreason = VgSrc_ExitThread;
   /* we do assign VgSrc_ExitThread and not VgSrc_ExitProcess, as this thread
      is the thread calling exit_group and so its registers must be considered
      as not reachable. See pub_tool_machine.h VG_(apply_to_GP_regs). */

   /* We have to claim the syscall already succeeded. */
   SET_STATUS_Success(0);
}

PRE(sys_llseek)
{
   PRINT("sys_llseek ( %lu, 0x%lx, 0x%lx, %#lx, %lu )", ARG1,ARG2,ARG3,ARG4,ARG5);
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

PRE(sys_clock_adjtime)
{
   struct vki_timex *tx = (struct vki_timex *)ARG2;
   PRINT("sys_clock_adjtime ( %ld, %#lx )", SARG1,ARG2);
   PRE_REG_READ2(long, "clock_adjtime", vki_clockid_t, id, struct timex *, buf);
   PRE_MEM_READ( "clock_adjtime(timex->modes)", ARG2, sizeof(tx->modes));

#define ADJX(bits,field)                                \
   if (tx->modes & (bits))                              \
      PRE_MEM_READ( "clock_adjtime(timex->"#field")",   \
                    (Addr)&tx->field, sizeof(tx->field))

   if (tx->modes & VKI_ADJ_ADJTIME) {
      if (!(tx->modes & VKI_ADJ_OFFSET_READONLY))
         PRE_MEM_READ( "clock_adjtime(timex->offset)", (Addr)&tx->offset, sizeof(tx->offset));
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

   PRE_MEM_WRITE( "adjtimex(timex)", ARG2, sizeof(struct vki_timex));
}

POST(sys_clock_adjtime)
{
   POST_MEM_WRITE( ARG2, sizeof(struct vki_timex) );
}

PRE(sys_ioperm)
{
   PRINT("sys_ioperm ( %lu, %lu, %ld )", ARG1, ARG2, SARG3 );
   PRE_REG_READ3(long, "ioperm",
                 unsigned long, from, unsigned long, num, int, turn_on);
}

PRE(sys_syslog)
{
   *flags |= SfMayBlock;
   PRINT("sys_syslog (%ld, %#lx, %ld)", SARG1, ARG2, SARG3);
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
   PRINT( "sys_prctl ( %ld, %ld, %ld, %ld, %ld )", SARG1, SARG2, SARG3, SARG4, SARG5 );
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
   case VKI_PR_SET_PTRACER:
      PRE_REG_READ2(int, "prctl", int, option, int, ptracer_process_ID);
      break;
   case VKI_PR_SET_SECCOMP:
      /* This is a bit feeble in that it uses |option| before checking
         it, but at least both sides of the conditional check it. */
      if (ARG2 == VKI_SECCOMP_MODE_FILTER) {
         PRE_REG_READ3(int, "prctl", int, option, int, mode, char*, filter);
         if (ARG3) {
            /* Should check that ARG3 points at a valid struct sock_fprog.
               Sounds complex; hence be lame. */
            PRE_MEM_READ( "prctl(PR_SET_SECCOMP, SECCOMP_MODE_FILTER, filter)",
                          ARG3, 1 );
         }
      } else {
         PRE_REG_READ2(int, "prctl", int, option, int, mode);
      }
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
   case VKI_PR_SET_NAME:
      {
         const HChar* new_name = (const HChar*) ARG2;
         if (new_name) {    // Paranoia
            ThreadState* tst = VG_(get_ThreadState)(tid);
            SizeT new_len = VG_(strlen)(new_name);

            /* Don't bother reusing the memory. This is a rare event. */
            tst->thread_name =
              VG_(realloc)("syswrap.prctl", tst->thread_name, new_len + 1);
            VG_(strcpy)(tst->thread_name, new_name);
         }
      }
      break;
   }
}

PRE(sys_sendfile)
{
   *flags |= SfMayBlock;
   PRINT("sys_sendfile ( %ld, %ld, %#lx, %lu )", SARG1,SARG2,ARG3,ARG4);
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
   PRINT("sendfile64 ( %ld, %ld, %#lx, %lu )",SARG1,SARG2,ARG3,ARG4);
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
   PRINT("sys_futex ( %#lx, %ld, %ld, %#lx, %#lx )", ARG1,SARG2,SARG3,ARG4,ARG5);
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
      /* Check that the address at least begins in client-accessible area. */
      if (!VG_(am_is_valid_for_client)( ARG1, 1, VKI_PROT_READ )) {
            SET_STATUS_Failure( VKI_EFAULT );
            return;
      }
      if (*(vki_u32 *)ARG1 != ARG3) {
         PRE_REG_READ5(long, "futex",
                       vki_u32 *, futex, int, op, int, val,
                       struct timespec *, utime, int, dummy);
      } else {
         PRE_REG_READ6(long, "futex",
                       vki_u32 *, futex, int, op, int, val,
                       struct timespec *, utime, int, dummy, int, val3);
      }
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
   PRINT("sys_set_robust_list ( %#lx, %lu )", ARG1,ARG2);
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
   PRINT("sys_get_robust_list ( %ld, %#lx, %#lx )", SARG1,ARG2,ARG3);
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
   PRINT("sys_pselect6 ( %ld, %#lx, %#lx, %#lx, %#lx, %#lx )",
         SARG1, ARG2, ARG3, ARG4, ARG5, ARG6);
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
   PRINT("sys_ppoll ( %#lx, %lu, %#lx, %#lx, %lu )\n", ARG1,ARG2,ARG3,ARG4,ARG5);
   PRE_REG_READ5(long, "ppoll",
                 struct vki_pollfd *, ufds, unsigned int, nfds,
                 struct vki_timespec *, tsp, vki_sigset_t *, sigmask,
                 vki_size_t, sigsetsize);

   for (i = 0; i < ARG2; i++) {
      PRE_MEM_READ( "ppoll(ufds.fd)",
                    (Addr)(&ufds[i].fd), sizeof(ufds[i].fd) );
      PRE_MEM_READ( "ppoll(ufds.events)",
                    (Addr)(&ufds[i].events), sizeof(ufds[i].events) );
      PRE_MEM_WRITE( "ppoll(ufds.revents)",
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
   PRINT("sys_epoll_create ( %ld )", SARG1);
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
   PRINT("sys_epoll_create1 ( %ld )", SARG1);
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
         SARG1, ( ARG2<3 ? epoll_ctl_s[ARG2] : "?" ), SARG3, ARG4);
   PRE_REG_READ4(long, "epoll_ctl",
                 int, epfd, int, op, int, fd, struct vki_epoll_event *, event);
   if (ARG2 != VKI_EPOLL_CTL_DEL)
      PRE_MEM_READ( "epoll_ctl(event)", ARG4, sizeof(struct vki_epoll_event) );
}

PRE(sys_epoll_wait)
{
   *flags |= SfMayBlock;
   PRINT("sys_epoll_wait ( %ld, %#lx, %ld, %ld )", SARG1, ARG2, SARG3, SARG4);
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
   PRINT("sys_epoll_pwait ( %ld, %#lx, %ld, %ld, %#lx, %lu )",
         SARG1, ARG2, SARG3, SARG4, ARG5, ARG6);
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
   PRINT("sys_eventfd2 ( %lu, %ld )", ARG1, SARG2);
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
         SARG1, SARG2, (Long)MERGE64(ARG3,ARG4), (Long)MERGE64(ARG5,ARG6));
   PRE_REG_READ6(long, "fallocate",
                 int, fd, int, mode,
                 unsigned, MERGE64_FIRST(offset), unsigned, MERGE64_SECOND(offset),
                 unsigned, MERGE64_FIRST(len), unsigned, MERGE64_SECOND(len));
#elif VG_WORDSIZE == 8
   PRINT("sys_fallocate ( %ld, %ld, %ld, %ld )",
         SARG1, SARG2, SARG3, SARG4);
   PRE_REG_READ4(long, "fallocate",
                 int, fd, int, mode, vki_loff_t, offset, vki_loff_t, len);
#else
#  error Unexpected word size
#endif
   if (!ML_(fd_allowed)(ARG1, "fallocate", tid, False))
      SET_STATUS_Failure( VKI_EBADF );
}

PRE(sys_prlimit64)
{
   PRINT("sys_prlimit64 ( %ld, %lu, %#lx, %#lx )", SARG1,ARG2,ARG3,ARG4);
   PRE_REG_READ4(long, "prlimit64",
                 vki_pid_t, pid, unsigned int, resource,
                 const struct rlimit64 *, new_rlim,
                 struct rlimit64 *, old_rlim);
   if (ARG3)
      PRE_MEM_READ( "rlimit64(new_rlim)", ARG3, sizeof(struct vki_rlimit64) );
   if (ARG4)
      PRE_MEM_WRITE( "rlimit64(old_rlim)", ARG4, sizeof(struct vki_rlimit64) );

   if (ARG3 &&
       ((struct vki_rlimit64 *)ARG3)->rlim_cur > ((struct vki_rlimit64 *)ARG3)->rlim_max) {
      SET_STATUS_Failure( VKI_EINVAL );
   }
   else if (ARG1 == 0 || ARG1 == VG_(getpid)()) {
      switch (ARG2) {
      case VKI_RLIMIT_NOFILE:
         SET_STATUS_Success( 0 );
         if (ARG4) {
            ((struct vki_rlimit64 *)ARG4)->rlim_cur = VG_(fd_soft_limit);
            ((struct vki_rlimit64 *)ARG4)->rlim_max = VG_(fd_hard_limit);
         }
         if (ARG3) {
            if (((struct vki_rlimit64 *)ARG3)->rlim_cur > VG_(fd_hard_limit) ||
                ((struct vki_rlimit64 *)ARG3)->rlim_max != VG_(fd_hard_limit)) {
               SET_STATUS_Failure( VKI_EPERM );
            }
            else {
               VG_(fd_soft_limit) = ((struct vki_rlimit64 *)ARG3)->rlim_cur;
            }
         }
         break;

      case VKI_RLIMIT_DATA:
         SET_STATUS_Success( 0 );
         if (ARG4) {
            ((struct vki_rlimit64 *)ARG4)->rlim_cur = VG_(client_rlimit_data).rlim_cur;
            ((struct vki_rlimit64 *)ARG4)->rlim_max = VG_(client_rlimit_data).rlim_max;
         }
         if (ARG3) {
            if (((struct vki_rlimit64 *)ARG3)->rlim_cur > VG_(client_rlimit_data).rlim_max ||
                ((struct vki_rlimit64 *)ARG3)->rlim_max > VG_(client_rlimit_data).rlim_max) {
               SET_STATUS_Failure( VKI_EPERM );
            }
            else {
               VG_(client_rlimit_data).rlim_cur = ((struct vki_rlimit64 *)ARG3)->rlim_cur;
               VG_(client_rlimit_data).rlim_max = ((struct vki_rlimit64 *)ARG3)->rlim_max;
            }
         }
         break;

      case VKI_RLIMIT_STACK:
         SET_STATUS_Success( 0 );
         if (ARG4) {
            ((struct vki_rlimit64 *)ARG4)->rlim_cur = VG_(client_rlimit_stack).rlim_cur;
            ((struct vki_rlimit64 *)ARG4)->rlim_max = VG_(client_rlimit_stack).rlim_max;
         }
         if (ARG3) {
            if (((struct vki_rlimit64 *)ARG3)->rlim_cur > VG_(client_rlimit_stack).rlim_max ||
                ((struct vki_rlimit64 *)ARG3)->rlim_max > VG_(client_rlimit_stack).rlim_max) {
               SET_STATUS_Failure( VKI_EPERM );
            }
            else {
               VG_(threads)[tid].client_stack_szB = ((struct vki_rlimit64 *)ARG3)->rlim_cur;
               VG_(client_rlimit_stack).rlim_cur = ((struct vki_rlimit64 *)ARG3)->rlim_cur;
               VG_(client_rlimit_stack).rlim_max = ((struct vki_rlimit64 *)ARG3)->rlim_max;
           }
         }
         break;
      }
   }
}

POST(sys_prlimit64)
{
   if (ARG4)
      POST_MEM_WRITE( ARG4, sizeof(struct vki_rlimit64) );
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
   PRINT("sys_tgkill ( %ld, %ld )", SARG1, SARG2);
   PRE_REG_READ2(long, "tkill", int, tid, int, sig);
   if (!ML_(client_signal_OK)(ARG2)) {
      SET_STATUS_Failure( VKI_EINVAL );
      return;
   }
   
   /* Check to see if this kill gave us a pending signal */
   *flags |= SfPollAfter;

   if (VG_(clo_trace_signals))
      VG_(message)(Vg_DebugMsg, "tkill: sending signal %ld to pid %ld\n",
		   SARG2, SARG1);

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
                   SARG2, SARG1);
}

PRE(sys_tgkill)
{
   PRINT("sys_tgkill ( %ld, %ld, %ld )", SARG1, SARG2, SARG3);
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
		   SARG3, SARG1, SARG2);

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
                   SARG3, SARG1, SARG2);
}

/* ---------------------------------------------------------------------
   fadvise64* wrappers
   ------------------------------------------------------------------ */

PRE(sys_fadvise64)
{
   PRINT("sys_fadvise64 ( %ld, %llu, %lu, %ld )",
         SARG1, MERGE64(ARG2,ARG3), ARG4, SARG5);
   PRE_REG_READ5(long, "fadvise64",
                 int, fd, vki_u32, MERGE64_FIRST(offset), vki_u32, MERGE64_SECOND(offset),
                 vki_size_t, len, int, advice);
}

PRE(sys_fadvise64_64)
{
   PRINT("sys_fadvise64_64 ( %ld, %llu, %llu, %ld )",
         SARG1, MERGE64(ARG2,ARG3), MERGE64(ARG4,ARG5), SARG6);
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
        VG_(discard_translations)( (Addr)ARG1, (ULong)size, 
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

   PRINT("sys_io_submit ( %lu, %ld, %#lx )", ARG1, SARG2, ARG3);
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
   PRINT("sys_mbind ( %#lx, %lu, %lu, %#lx, %lu, %lu )", ARG1,ARG2,ARG3,ARG4,ARG5,ARG6);
   PRE_REG_READ6(long, "mbind",
                 unsigned long, start, unsigned long, len,
                 unsigned long, policy, unsigned long *, nodemask,
                 unsigned long, maxnode, unsigned, flags);
   if (ARG1 != 0)
      PRE_MEM_READ( "mbind(nodemask)", ARG4,
                    VG_ROUNDUP( ARG5-1, sizeof(UWord) * 8 ) / 8 );
}

PRE(sys_set_mempolicy)
{
   PRINT("sys_set_mempolicy ( %ld, %#lx, %lu )", SARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "set_mempolicy",
                 int, policy, unsigned long *, nodemask,
                 unsigned long, maxnode);
   PRE_MEM_READ( "set_mempolicy(nodemask)", ARG2,
                 VG_ROUNDUP( ARG3-1, sizeof(UWord) * 8 ) / 8 );
}

PRE(sys_get_mempolicy)
{
   PRINT("sys_get_mempolicy ( %#lx, %#lx, %lu, %#lx, %lx )", ARG1,ARG2,ARG3,ARG4,ARG5);
   PRE_REG_READ5(long, "get_mempolicy",
                 int *, policy, unsigned long *, nodemask,
                 unsigned long, maxnode, unsigned long, addr,
                 unsigned long, flags);
   if (ARG1 != 0)
      PRE_MEM_WRITE( "get_mempolicy(policy)", ARG1, sizeof(Int) );
   if (ARG2 != 0)
      PRE_MEM_WRITE( "get_mempolicy(nodemask)", ARG2,
                     VG_ROUNDUP( ARG3-1, sizeof(UWord) * 8 ) / 8 );
}
POST(sys_get_mempolicy)
{
   if (ARG1 != 0)
      POST_MEM_WRITE( ARG1, sizeof(Int) );
   if (ARG2 != 0)
      POST_MEM_WRITE( ARG2, VG_ROUNDUP( ARG3-1, sizeof(UWord) * 8 ) / 8 );
}

/* ---------------------------------------------------------------------
   fanotify_* wrappers
   ------------------------------------------------------------------ */

PRE(sys_fanotify_init)
{
   PRINT("sys_fanotify_init ( %lu, %lu )", ARG1,ARG2);
   PRE_REG_READ2(long, "fanotify_init",
                 unsigned int, flags, unsigned int, event_f_flags);
}

POST(sys_fanotify_init)
{
   vg_assert(SUCCESS);
   if (!ML_(fd_allowed)(RES, "fanotify_init", tid, True)) {
      VG_(close)(RES);
      SET_STATUS_Failure( VKI_EMFILE );
   } else {
      if (VG_(clo_track_fds))
         ML_(record_fd_open_nameless) (tid, RES);
   }
}

PRE(sys_fanotify_mark)
{
#if VG_WORDSIZE == 4
   PRINT( "sys_fanotify_mark ( %ld, %lu, %llu, %ld, %#lx(%s))", 
          SARG1, ARG2, MERGE64(ARG3,ARG4), SARG5, ARG6, (HChar *)ARG6);
   PRE_REG_READ6(long, "sys_fanotify_mark", 
                 int, fanotify_fd, unsigned int, flags,
                 __vki_u32, mask0, __vki_u32, mask1,
                 int, dfd, const char *, pathname);
   if (ARG6)
      PRE_MEM_RASCIIZ( "fanotify_mark(path)", ARG6);
#elif VG_WORDSIZE == 8
   PRINT( "sys_fanotify_mark ( %ld, %lu, %lu, %ld, %#lx(%s))", 
          SARG1, ARG2, ARG3, SARG4, ARG5, (HChar *)ARG5);
   PRE_REG_READ5(long, "sys_fanotify_mark", 
                 int, fanotify_fd, unsigned int, flags,
                 __vki_u64, mask,
                 int, dfd, const char *, pathname);
   if (ARG5)
      PRE_MEM_RASCIIZ( "fanotify_mark(path)", ARG5);
#else
#  error Unexpected word size
#endif
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
   PRINT("sys_inotify_init ( %ld )", SARG1);
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
   PRINT( "sys_inotify_add_watch ( %ld, %#lx, %lx )", SARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "inotify_add_watch", int, fd, char *, path, int, mask);
   PRE_MEM_RASCIIZ( "inotify_add_watch(path)", ARG2 );
}

PRE(sys_inotify_rm_watch)
{
   PRINT( "sys_inotify_rm_watch ( %ld, %lx )", SARG1, ARG2);
   PRE_REG_READ2(long, "inotify_rm_watch", int, fd, int, wd);
}

/* ---------------------------------------------------------------------
   mq_* wrappers
   ------------------------------------------------------------------ */

PRE(sys_mq_open)
{
   PRINT("sys_mq_open( %#lx(%s), %ld, %lu, %#lx )",
         ARG1, (HChar*)ARG1, SARG2, ARG3, ARG4);
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
         ML_(record_fd_open_with_given_name)(tid, RES, (HChar*)ARG1);
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
   PRINT("sys_mq_timedsend ( %ld, %#lx, %lu, %lu, %#lx )",
         SARG1,ARG2,ARG3,ARG4,ARG5);
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
   PRINT("sys_mq_timedreceive( %ld, %#lx, %lu, %#lx, %#lx )",
         SARG1,ARG2,ARG3,ARG4,ARG5);
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
   PRINT("sys_mq_notify( %ld, %#lx )", SARG1, ARG2 );
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
   PRINT("sys_mq_getsetattr( %ld, %#lx, %#lx )", SARG1,ARG2,ARG3 );
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
   PRINT("sys_clock_settime( %ld, %#lx )", SARG1, ARG2);
   PRE_REG_READ2(long, "clock_settime", 
                 vki_clockid_t, clk_id, const struct timespec *, tp);
   PRE_MEM_READ( "clock_settime(tp)", ARG2, sizeof(struct vki_timespec) );
}

PRE(sys_clock_gettime)
{
   PRINT("sys_clock_gettime( %ld, %#lx )" , SARG1, ARG2);
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
   PRINT("sys_clock_getres( %ld, %#lx )" , SARG1, ARG2);
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
   PRINT("sys_clock_nanosleep( %ld, %ld, %#lx, %#lx )",
         SARG1, SARG2, ARG3, ARG4);
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
   PRINT("sys_timer_create( %ld, %#lx, %#lx )", SARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "timer_create",
                 vki_clockid_t, clockid, struct sigevent *, evp,
                 vki_timer_t *, timerid);
   if (ARG2 != 0) {
      struct vki_sigevent *evp = (struct vki_sigevent *) ARG2;
      PRE_MEM_READ( "timer_create(evp.sigev_value)", (Addr)&evp->sigev_value,
                    sizeof(vki_sigval_t) );
      PRE_MEM_READ( "timer_create(evp.sigev_signo)", (Addr)&evp->sigev_signo,
                    sizeof(int) );
      PRE_MEM_READ( "timer_create(evp.sigev_notify)", (Addr)&evp->sigev_notify,
                    sizeof(int) );
      if (ML_(safe_to_deref)(&evp->sigev_notify, sizeof(int))
          && (evp->sigev_notify & VKI_SIGEV_THREAD_ID) != 0)
         PRE_MEM_READ( "timer_create(evp.sigev_notify_thread_id)",
                       (Addr)&evp->vki_sigev_notify_thread_id, sizeof(int) );
   }
   PRE_MEM_WRITE( "timer_create(timerid)", ARG3, sizeof(vki_timer_t) );
}
POST(sys_timer_create)
{
   POST_MEM_WRITE( ARG3, sizeof(vki_timer_t) );
}

PRE(sys_timer_settime)
{
   PRINT("sys_timer_settime( %ld, %ld, %#lx, %#lx )", SARG1,SARG2,ARG3,ARG4);
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
   PRINT("sys_timer_gettime( %ld, %#lx )", SARG1, ARG2);
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
   HChar release[64];   // large enough
   SysRes res;

   if (result == -1) {
      res = VG_(open)("/proc/sys/kernel/osrelease", 0, 0);
      if (sr_isError(res))
         return False;
      fd = sr_Res(res);
      read = VG_(read)(fd, release, sizeof(release) - 1);
      if (read < 0)
         return False;
      release[read] = 0;
      VG_(close)(fd);
      //VG_(printf)("kernel release = %s\n", release);
      result = VG_(strncmp)(release, "2.6.22", 6) == 0
               && ! VG_(isdigit)(release[6]);
   }
   vg_assert(result == 0 || result == 1);
   return result == 1;
}

PRE(sys_timerfd_create)
{
   if (linux_kernel_2_6_22()) {
      /* 2.6.22 kernel: timerfd system call. */
      PRINT("sys_timerfd ( %ld, %ld, %#lx )", SARG1, SARG2, ARG3);
      PRE_REG_READ3(long, "sys_timerfd",
                    int, fd, int, clockid, const struct itimerspec *, tmr);
      PRE_MEM_READ("timerfd(tmr)", ARG3,
                   sizeof(struct vki_itimerspec) );
      if ((Word)ARG1 != -1L && !ML_(fd_allowed)(ARG1, "timerfd", tid, False))
         SET_STATUS_Failure( VKI_EBADF );
   } else {
      /* 2.6.24 and later kernels: timerfd_create system call. */
      PRINT("sys_timerfd_create (%ld, %ld )", SARG1, SARG2);
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
   PRINT("sys_timerfd_gettime ( %ld, %#lx )", SARG1, ARG2);
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
   PRINT("sys_timerfd_settime ( %ld, %ld, %#lx, %#lx )",
         SARG1, SARG2, ARG3, ARG4);
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
   if (ARG2 != (Addr)NULL)
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
   PRINT("sys_setuid16 ( %lu )", ARG1);
   PRE_REG_READ1(long, "setuid16", vki_old_uid_t, uid);
}

PRE(sys_getgid16)
{
   PRINT("sys_getgid16 ( )");
   PRE_REG_READ0(long, "getgid16");
}

PRE(sys_setgid16)
{
   PRINT("sys_setgid16 ( %lu )", ARG1);
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
   PRINT("sys_setregid16 ( %lu, %lu )", ARG1, ARG2);
   PRE_REG_READ2(long, "setregid16", vki_old_gid_t, rgid, vki_old_gid_t, egid);
}

PRE(sys_getgroups16)
{
   PRINT("sys_getgroups16 ( %ld, %#lx )", SARG1, ARG2);
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
   PRINT("sys_fchown16 ( %lu, %lu, %lu )", ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "fchown16",
                 unsigned int, fd, vki_old_uid_t, owner, vki_old_gid_t, group);
}

/* ---------------------------------------------------------------------
   *xattr wrappers
   ------------------------------------------------------------------ */

PRE(sys_setxattr)
{
   *flags |= SfMayBlock;
   PRINT("sys_setxattr ( %#lx, %#lx, %#lx, %lu, %ld )",
         ARG1, ARG2, ARG3, ARG4, SARG5);
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
   PRINT("sys_lsetxattr ( %#lx, %#lx, %#lx, %lu, %ld )",
         ARG1, ARG2, ARG3, ARG4, SARG5);
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
   PRINT("sys_fsetxattr ( %ld, %#lx, %#lx, %lu, %ld )",
         SARG1, ARG2, ARG3, ARG4, SARG5);
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
   PRINT("sys_fgetxattr ( %ld, %#lx, %#lx, %lu )", SARG1, ARG2, ARG3, ARG4);
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
   PRINT("sys_flistxattr ( %ld, %#lx, %lu )", SARG1, ARG2, ARG3);
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
   PRINT("sys_fremovexattr ( %ld, %#lx )", SARG1, ARG2);
   PRE_REG_READ2(long, "fremovexattr", int, fd, char *, name);
   PRE_MEM_RASCIIZ( "fremovexattr(name)", ARG2 );
}

/* ---------------------------------------------------------------------
   sched_* wrappers
   ------------------------------------------------------------------ */

PRE(sys_sched_setparam)
{
   PRINT("sched_setparam ( %ld, %#lx )", SARG1, ARG2 );
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
   PRINT("sched_getparam ( %ld, %#lx )", SARG1, ARG2 );
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
   PRINT("sys_sched_getscheduler ( %ld )", SARG1);
   PRE_REG_READ1(long, "sched_getscheduler", vki_pid_t, pid);
}

PRE(sys_sched_setscheduler)
{
   PRINT("sys_sched_setscheduler ( %ld, %ld, %#lx )", SARG1, SARG2, ARG3);
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
   PRINT("sched_get_priority_max ( %ld )", SARG1);
   PRE_REG_READ1(long, "sched_get_priority_max", int, policy);
}

PRE(sys_sched_get_priority_min)
{
   PRINT("sched_get_priority_min ( %ld )", SARG1);
   PRE_REG_READ1(long, "sched_get_priority_min", int, policy);
}

PRE(sys_sched_rr_get_interval)
{
   PRINT("sys_sched_rr_get_interval ( %ld, %#lx )", SARG1, ARG2);
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
   PRINT("sched_setaffinity ( %ld, %lu, %#lx )", SARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "sched_setaffinity", 
                 vki_pid_t, pid, unsigned int, len, unsigned long *, mask);
   PRE_MEM_READ( "sched_setaffinity(mask)", ARG3, ARG2);
}

PRE(sys_sched_getaffinity)
{
   PRINT("sched_getaffinity ( %ld, %lu, %#lx )", SARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "sched_getaffinity", 
                 vki_pid_t, pid, unsigned int, len, unsigned long *, mask);
   PRE_MEM_WRITE( "sched_getaffinity(mask)", ARG3, ARG2);
}
POST(sys_sched_getaffinity)
{
   POST_MEM_WRITE(ARG3, ARG2);
}

PRE(sys_unshare)
{
   PRINT("sys_unshare ( %#lx )", ARG1);
   PRE_REG_READ1(int, "unshare", unsigned long, flags);
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
   PRINT("sys_dup3 ( %lu, %lu, %#lx )", ARG1, ARG2, ARG3);
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
   PRINT("sys_waitid( %ld, %ld, %#lx, %ld, %#lx )",
         SARG1, SARG2, ARG3, SARG4, ARG5);
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
   PRINT("sys_sync_file_range ( %ld, %lld, %lld, %#lx )",
         SARG1, (Long)MERGE64(ARG2,ARG3), (Long)MERGE64(ARG4,ARG5),ARG6);
   PRE_REG_READ6(long, "sync_file_range",
                 int, fd,
                 unsigned, MERGE64_FIRST(offset), unsigned, MERGE64_SECOND(offset),
                 unsigned, MERGE64_FIRST(nbytes), unsigned, MERGE64_SECOND(nbytes),
                 unsigned int, flags);
#elif VG_WORDSIZE == 8
   PRINT("sys_sync_file_range ( %ld, %ld, %ld, %#lx )",
         SARG1, SARG2, SARG3, ARG4);
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
   PRINT("sys_sync_file_range2 ( %ld, %lu, %lld, %lld )",
         SARG1, ARG2, (Long)MERGE64(ARG3,ARG4), (Long)MERGE64(ARG5,ARG6));
   PRE_REG_READ6(long, "sync_file_range2",
                 int, fd, unsigned int, flags,
                 unsigned, MERGE64_FIRST(offset), unsigned, MERGE64_SECOND(offset),
                 unsigned, MERGE64_FIRST(nbytes), unsigned, MERGE64_SECOND(nbytes));
#elif VG_WORDSIZE == 8
   PRINT("sys_sync_file_range2 ( %ld, %lu, %ld, %ld )",
         SARG1, ARG2, SARG3, SARG4);
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

PRE(sys_perf_event_open)
{
   struct vki_perf_event_attr *attr;
   PRINT("sys_perf_event_open ( %#lx, %ld, %ld, %ld, %#lx )",
         ARG1, SARG2, SARG3, SARG4, ARG5);
   PRE_REG_READ5(long, "perf_event_open",
                 struct vki_perf_event_attr *, attr,
                 vki_pid_t, pid, int, cpu, int, group_fd,
                 unsigned long, flags);
   attr = (struct vki_perf_event_attr *)ARG1;
   PRE_MEM_READ( "perf_event_open(attr->size)",
                 (Addr)&attr->size, sizeof(attr->size) );
   PRE_MEM_READ( "perf_event_open(attr)",
                 (Addr)attr, attr->size );
}

POST(sys_perf_event_open)
{
   vg_assert(SUCCESS);
   if (!ML_(fd_allowed)(RES, "perf_event_open", tid, True)) {
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

PRE(sys_move_pages)
{
   PRINT("sys_move_pages ( %ld, %lu, %#lx, %#lx, %#lx, %#lx )",
         SARG1, ARG2, ARG3, ARG4, ARG5, ARG6);
   PRE_REG_READ6(int, "move_pages",
                 vki_pid_t, pid, unsigned long, nr_pages, const void **, pages,
                 const int *, nodes, int *, status, int, flags);
   PRE_MEM_READ("move_pages(pages)", ARG3, ARG2 * sizeof(void *));
   if (ARG4)
      PRE_MEM_READ("move_pages(nodes)", ARG4, ARG2 * sizeof(int));
   PRE_MEM_WRITE("move_pages(status)", ARG5, ARG2 * sizeof(int));
}

POST(sys_move_pages)
{
   POST_MEM_WRITE(ARG5, ARG2 * sizeof(int));
}

PRE(sys_getrandom)
{
   PRINT("sys_getrandom ( %#lx, %lu, %lu )" , ARG1, ARG2, ARG3);
   PRE_REG_READ3(int, "getrandom",
                 char *, buf, vki_size_t, count, unsigned int, flags);
   PRE_MEM_WRITE( "getrandom(cpu)", ARG1, ARG2 );
}

POST(sys_getrandom)
{
   POST_MEM_WRITE( ARG1, ARG2 );
}

PRE(sys_memfd_create)
{
   PRINT("sys_memfd_create ( %#lx, %lu )" , ARG1, ARG2);
   PRE_REG_READ2(int, "memfd_create",
                 char *, uname, unsigned int, flags);
   PRE_MEM_RASCIIZ( "memfd_create(uname)", ARG1 );
}

POST(sys_memfd_create)
{
   vg_assert(SUCCESS);
   if (!ML_(fd_allowed)(RES, "memfd_create", tid, True)) {
      VG_(close)(RES);
      SET_STATUS_Failure( VKI_EMFILE );
   } else {
      if (VG_(clo_track_fds))
         ML_(record_fd_open_nameless)(tid, RES);
   }
}

PRE(sys_syncfs)
{
   *flags |= SfMayBlock;
   PRINT("sys_syncfs ( %lu )", ARG1);
   PRE_REG_READ1(long, "syncfs", unsigned int, fd);
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
   PRINT("sys_lseek ( %lu, %ld, %lu )", ARG1, SARG2, ARG3);
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
   PRINT("sys_readahead ( %ld, %lld, %lu )",
         SARG1, (Long)MERGE64(ARG2,ARG3), ARG4);
   PRE_REG_READ4(vki_off_t, "readahead",
                 int, fd, unsigned, MERGE64_FIRST(offset),
                 unsigned, MERGE64_SECOND(offset), vki_size_t, count);
#elif VG_WORDSIZE == 8
   PRINT("sys_readahead ( %ld, %ld, %lu )", SARG1, SARG2, ARG3);
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
#if defined(VGP_x86_linux) || defined(VGP_ppc32_linux) \
    || defined(VGP_arm_linux) || defined(VGP_mips32_linux)
PRE(sys_sigprocmask)
{
   vki_old_sigset_t* set;
   vki_old_sigset_t* oldset;
   vki_sigset_t bigger_set;
   vki_sigset_t bigger_oldset;

   PRINT("sys_sigprocmask ( %ld, %#lx, %#lx )", SARG1, ARG2, ARG3);
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

/* Convert from non-RT to RT sigset_t's */
static 
void convert_sigset_to_rt(const vki_old_sigset_t *oldset, vki_sigset_t *set)
{
   VG_(sigemptyset)(set);
   set->sig[0] = *oldset;
}
PRE(sys_sigaction)
{
   vki_sigaction_toK_t   new, *newp;
   vki_sigaction_fromK_t old, *oldp;

   PRINT("sys_sigaction ( %ld, %#lx, %#lx )",  SARG1, ARG2, ARG3);
   PRE_REG_READ3(int, "sigaction",
                 int, signum, const struct old_sigaction *, act,
                 struct old_sigaction *, oldact);

   newp = oldp = NULL;

   if (ARG2 != 0) {
      struct vki_old_sigaction *sa = (struct vki_old_sigaction *)ARG2;
      PRE_MEM_READ( "sigaction(act->sa_handler)", (Addr)&sa->ksa_handler, sizeof(sa->ksa_handler));
      PRE_MEM_READ( "sigaction(act->sa_mask)", (Addr)&sa->sa_mask, sizeof(sa->sa_mask));
      PRE_MEM_READ( "sigaction(act->sa_flags)", (Addr)&sa->sa_flags, sizeof(sa->sa_flags));
      if (ML_(safe_to_deref)(sa,sizeof(sa)) 
          && (sa->sa_flags & VKI_SA_RESTORER))
         PRE_MEM_READ( "sigaction(act->sa_restorer)", (Addr)&sa->sa_restorer, sizeof(sa->sa_restorer));
   }

   if (ARG3 != 0) {
      PRE_MEM_WRITE( "sigaction(oldact)", ARG3, sizeof(struct vki_old_sigaction));
      oldp = &old;
   }

   if (ARG2 != 0) {
      struct vki_old_sigaction *oldnew = (struct vki_old_sigaction *)ARG2;

      new.ksa_handler = oldnew->ksa_handler;
      new.sa_flags = oldnew->sa_flags;
      new.sa_restorer = oldnew->sa_restorer;
      convert_sigset_to_rt(&oldnew->sa_mask, &new.sa_mask);
      newp = &new;
   }

   SET_STATUS_from_SysRes( VG_(do_sys_sigaction)(ARG1, newp, oldp) );

   if (ARG3 != 0 && SUCCESS && RES == 0) {
      struct vki_old_sigaction *oldold = (struct vki_old_sigaction *)ARG3;

      oldold->ksa_handler = oldp->ksa_handler;
      oldold->sa_flags = oldp->sa_flags;
      oldold->sa_restorer = oldp->sa_restorer;
      oldold->sa_mask = oldp->sa_mask.sig[0];
   }
}
POST(sys_sigaction)
{
   vg_assert(SUCCESS);
   if (RES == 0 && ARG3 != 0)
      POST_MEM_WRITE( ARG3, sizeof(struct vki_old_sigaction));
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
   PRINT("sys_signalfd4 ( %ld, %#lx, %lu, %ld )", SARG1, ARG2, ARG3, SARG4);
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
   PRINT("sys_rt_sigaction ( %ld, %#lx, %#lx, %lu )", SARG1, ARG2, ARG3, ARG4);
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
   PRINT("sys_rt_sigprocmask ( %ld, %#lx, %#lx, %lu )",
         SARG1, ARG2, ARG3, ARG4);
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
   PRINT("sys_rt_sigtimedwait ( %#lx, %#lx, %#lx, %lu )",
         ARG1, ARG2, ARG3, ARG4);
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
   PRINT("sys_rt_sigqueueinfo(%ld, %ld, %#lx)", SARG1, SARG2, ARG3);
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
   PRINT("sys_rt_tgsigqueueinfo(%ld, %ld, %ld, %#lx)",
         SARG1, SARG2, SARG3, ARG4);
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
   PRINT("sys_rt_sigsuspend ( %#lx, %lu )", ARG1, ARG2 );
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
   Generic handler for sys_ipc
   Depending on the platform, some syscalls (e.g. semctl, semop, ...)
   are either direct system calls, or are all implemented via sys_ipc.
   ------------------------------------------------------------------ */
#ifdef __NR_ipc
static Addr deref_Addr ( ThreadId tid, Addr a, const HChar* s )
{
   Addr* a_p = (Addr*)a;
   PRE_MEM_READ( s, (Addr)a_p, sizeof(Addr) );
   return *a_p;
}

static Bool semctl_cmd_has_4args (UWord cmd)
{
   switch (cmd & ~VKI_IPC_64)
   {
   case VKI_IPC_INFO:
   case VKI_SEM_INFO:
   case VKI_IPC_STAT:
   case VKI_SEM_STAT:
   case VKI_IPC_SET:
   case VKI_GETALL:
   case VKI_SETALL:
      return True;
   default:
      return False;
   }
}

PRE(sys_ipc)
{
   PRINT("sys_ipc ( %lu, %ld, %ld, %ld, %#lx, %ld )",
         ARG1, SARG2, SARG3, SARG4, ARG5, SARG6);

   switch (ARG1 /* call */) {
   case VKI_SEMOP:
      PRE_REG_READ5(int, "ipc",
                    vki_uint, call, int, first, int, second, int, third,
                    void *, ptr);
      ML_(generic_PRE_sys_semop)( tid, ARG2, ARG5, ARG3 );
      *flags |= SfMayBlock;
      break;
   case VKI_SEMGET:
      PRE_REG_READ4(int, "ipc",
                    vki_uint, call, int, first, int, second, int, third);
      break;
   case VKI_SEMCTL:
   {
      PRE_REG_READ5(int, "ipc",
                    vki_uint, call, int, first, int, second, int, third,
                    void *, ptr);
      UWord arg;
      if (semctl_cmd_has_4args(ARG4))
         arg = deref_Addr( tid, ARG5, "semctl(arg)" );
      else
         arg = 0;
      ML_(generic_PRE_sys_semctl)( tid, ARG2, ARG3, ARG4, arg );
      break;
   }
   case VKI_SEMTIMEDOP:
      PRE_REG_READ6(int, "ipc",
                    vki_uint, call, int, first, int, second, int, third,
                    void *, ptr, long, fifth);
      ML_(generic_PRE_sys_semtimedop)( tid, ARG2, ARG5, ARG3, ARG6 );
      *flags |= SfMayBlock;
      break;
   case VKI_MSGSND:
      PRE_REG_READ5(int, "ipc",
                    vki_uint, call, int, first, int, second, int, third,
                    void *, ptr);
      ML_(linux_PRE_sys_msgsnd)( tid, ARG2, ARG5, ARG3, ARG4 );
      if ((ARG4 & VKI_IPC_NOWAIT) == 0)
         *flags |= SfMayBlock;
      break;
   case VKI_MSGRCV:
   {
      PRE_REG_READ5(int, "ipc",
                    vki_uint, call, int, first, int, second, int, third,
                    void *, ptr);
      Addr msgp;
      Word msgtyp;
 
      msgp = deref_Addr( tid, (Addr) (&((struct vki_ipc_kludge *)ARG5)->msgp),
                         "msgrcv(msgp)" );
      msgtyp = deref_Addr( tid, 
                           (Addr) (&((struct vki_ipc_kludge *)ARG5)->msgtyp),
                           "msgrcv(msgp)" );

      ML_(linux_PRE_sys_msgrcv)( tid, ARG2, msgp, ARG3, msgtyp, ARG4 );

      if ((ARG4 & VKI_IPC_NOWAIT) == 0)
         *flags |= SfMayBlock;
      break;
   }
   case VKI_MSGGET:
      PRE_REG_READ3(int, "ipc", vki_uint, call, int, first, int, second);
      break;
   case VKI_MSGCTL:
      PRE_REG_READ5(int, "ipc",
                    vki_uint, call, int, first, int, second, int, third,
                    void *, ptr);
      ML_(linux_PRE_sys_msgctl)( tid, ARG2, ARG3, ARG5 );
      break;
   case VKI_SHMAT:
   {
      PRE_REG_READ5(int, "ipc",
                    vki_uint, call, int, first, int, second, int, third,
                    void *, ptr);
      UWord w;
      PRE_MEM_WRITE( "shmat(raddr)", ARG4, sizeof(Addr) );
      w = ML_(generic_PRE_sys_shmat)( tid, ARG2, ARG5, ARG3 );
      if (w == 0)
         SET_STATUS_Failure( VKI_EINVAL );
      else
         ARG5 = w;
      break;
   }
   case VKI_SHMDT:
      PRE_REG_READ5(int, "ipc",
                    vki_uint, call, int, first, int, second, int, third,
                    void *, ptr);
      if (!ML_(generic_PRE_sys_shmdt)(tid, ARG5))
	 SET_STATUS_Failure( VKI_EINVAL );
      break;
   case VKI_SHMGET:
      PRE_REG_READ4(int, "ipc",
                    vki_uint, call, int, first, int, second, int, third);
      if (ARG4 & VKI_SHM_HUGETLB) {
         static Bool warning_given = False;
         ARG4 &= ~VKI_SHM_HUGETLB;
         if (!warning_given) {
            warning_given = True;
            VG_(umsg)(
               "WARNING: valgrind ignores shmget(shmflg) SHM_HUGETLB\n");
         }
      }
      break;
   case VKI_SHMCTL: /* IPCOP_shmctl */
      PRE_REG_READ5(int, "ipc",
                    vki_uint, call, int, first, int, second, int, third,
                    void *, ptr);
      ML_(generic_PRE_sys_shmctl)( tid, ARG2, ARG3, ARG5 );
      break;
   default:
      VG_(message)(Vg_DebugMsg, "FATAL: unhandled syscall(ipc) %lu\n", ARG1 );
      VG_(core_panic)("... bye!\n");
      break; /*NOTREACHED*/
   }
}

POST(sys_ipc)
{
   vg_assert(SUCCESS);
   switch (ARG1 /* call */) {
   case VKI_SEMOP:
   case VKI_SEMGET:
      break;
   case VKI_SEMCTL:
   {
      UWord arg;
      if (semctl_cmd_has_4args(ARG4))
         arg = deref_Addr( tid, ARG5, "semctl(arg)" );
      else
         arg = 0;
      ML_(generic_POST_sys_semctl)( tid, RES, ARG2, ARG3, ARG4, arg );
      break;
   }
   case VKI_SEMTIMEDOP:
   case VKI_MSGSND:
      break;
   case VKI_MSGRCV:
   {
      Addr msgp;
      Word msgtyp;

      msgp = deref_Addr( tid,
			 (Addr) (&((struct vki_ipc_kludge *)ARG5)->msgp),
			 "msgrcv(msgp)" );
      msgtyp = deref_Addr( tid,
			   (Addr) (&((struct vki_ipc_kludge *)ARG5)->msgtyp),
			   "msgrcv(msgp)" );

      ML_(linux_POST_sys_msgrcv)( tid, RES, ARG2, msgp, ARG3, msgtyp, ARG4 );
      break;
   }
   case VKI_MSGGET:
      break;
   case VKI_MSGCTL:
      ML_(linux_POST_sys_msgctl)( tid, RES, ARG2, ARG3, ARG5 );
      break;
   case VKI_SHMAT:
   {
      Addr addr;

      /* force readability. before the syscall it is
       * indeed uninitialized, as can be seen in
       * glibc/sysdeps/unix/sysv/linux/shmat.c */
      POST_MEM_WRITE( ARG4, sizeof( Addr ) );

      addr = deref_Addr ( tid, ARG4, "shmat(addr)" );
      ML_(generic_POST_sys_shmat)( tid, addr, ARG2, ARG5, ARG3 );
      break;
   }
   case VKI_SHMDT:
      ML_(generic_POST_sys_shmdt)( tid, RES, ARG5 );
      break;
   case VKI_SHMGET:
      break;
   case VKI_SHMCTL:
      ML_(generic_POST_sys_shmctl)( tid, RES, ARG2, ARG3, ARG5 );
      break;
   default:
      VG_(message)(Vg_DebugMsg,
		   "FATAL: unhandled syscall(ipc) %lu\n",
		   ARG1 );
      VG_(core_panic)("... bye!\n");
      break; /*NOTREACHED*/
   }
}
#endif

PRE(sys_semget)
{
   PRINT("sys_semget ( %ld, %ld, %ld )", SARG1, SARG2, SARG3);
   PRE_REG_READ3(long, "semget", vki_key_t, key, int, nsems, int, semflg);
}

PRE(sys_semop)
{
   *flags |= SfMayBlock;
   PRINT("sys_semop ( %ld, %#lx, %lu )", SARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "semop",
                 int, semid, struct sembuf *, sops, unsigned, nsoops);
   ML_(generic_PRE_sys_semop)(tid, ARG1,ARG2,ARG3);
}

PRE(sys_semctl)
{
   switch (ARG3 & ~VKI_IPC_64) {
   case VKI_IPC_INFO:
   case VKI_SEM_INFO:
      PRINT("sys_semctl ( %ld, %ld, %ld, %#lx )", SARG1, SARG2, SARG3, ARG4);
      PRE_REG_READ4(long, "semctl",
                    int, semid, int, semnum, int, cmd, struct seminfo *, arg);
      break;
   case VKI_IPC_STAT:
   case VKI_SEM_STAT:
   case VKI_IPC_SET:
      PRINT("sys_semctl ( %ld, %ld, %ld, %#lx )", SARG1, SARG2, SARG3, ARG4);
      PRE_REG_READ4(long, "semctl",
                    int, semid, int, semnum, int, cmd, struct semid_ds *, arg);
      break;
   case VKI_GETALL:
   case VKI_SETALL:
      PRINT("sys_semctl ( %ld, %ld, %ld, %#lx )", SARG1, SARG2, SARG3, ARG4);
      PRE_REG_READ4(long, "semctl",
                    int, semid, int, semnum, int, cmd, unsigned short *, arg);
      break;
   default:
      PRINT("sys_semctl ( %ld, %ld, %ld )", SARG1, SARG2, SARG3);
      PRE_REG_READ3(long, "semctl",
                    int, semid, int, semnum, int, cmd);
      break;
   }
#ifdef VGP_amd64_linux
   ML_(generic_PRE_sys_semctl)(tid, ARG1,ARG2,ARG3|VKI_IPC_64,ARG4);
#else
   ML_(generic_PRE_sys_semctl)(tid, ARG1,ARG2,ARG3,ARG4);
#endif
}

POST(sys_semctl)
{
#ifdef VGP_amd64_linux
   ML_(generic_POST_sys_semctl)(tid, RES,ARG1,ARG2,ARG3|VKI_IPC_64,ARG4);
#else
   ML_(generic_POST_sys_semctl)(tid, RES,ARG1,ARG2,ARG3,ARG4);
#endif
}

PRE(sys_semtimedop)
{
   *flags |= SfMayBlock;
   PRINT("sys_semtimedop ( %ld, %#lx, %lu, %#lx )", SARG1, ARG2, ARG3, ARG4);
   PRE_REG_READ4(long, "semtimedop",
                 int, semid, struct sembuf *, sops, unsigned, nsoops,
                 struct timespec *, timeout);
   ML_(generic_PRE_sys_semtimedop)(tid, ARG1,ARG2,ARG3,ARG4);
}

PRE(sys_msgget)
{
   PRINT("sys_msgget ( %ld, %ld )", SARG1, SARG2);
   PRE_REG_READ2(long, "msgget", vki_key_t, key, int, msgflg);
}

PRE(sys_msgsnd)
{
   PRINT("sys_msgsnd ( %ld, %#lx, %lu, %ld )", SARG1, ARG2, ARG3, SARG4);
   PRE_REG_READ4(long, "msgsnd",
                 int, msqid, struct msgbuf *, msgp, vki_size_t, msgsz, int, msgflg);
   ML_(linux_PRE_sys_msgsnd)(tid, ARG1,ARG2,ARG3,ARG4);
   if ((ARG4 & VKI_IPC_NOWAIT) == 0)
      *flags |= SfMayBlock;
}

PRE(sys_msgrcv)
{
   PRINT("sys_msgrcv ( %ld, %#lx, %lu, %ld, %ld )",
         SARG1, ARG2, ARG3, SARG4, SARG5);
   PRE_REG_READ5(long, "msgrcv",
                 int, msqid, struct msgbuf *, msgp, vki_size_t, msgsz,
                 long, msgytp, int, msgflg);
   ML_(linux_PRE_sys_msgrcv)(tid, ARG1,ARG2,ARG3,ARG4,ARG5);
   if ((ARG5 & VKI_IPC_NOWAIT) == 0)
      *flags |= SfMayBlock;
}
POST(sys_msgrcv)
{
   ML_(linux_POST_sys_msgrcv)(tid, RES,ARG1,ARG2,ARG3,ARG4,ARG5);
}

PRE(sys_msgctl)
{
   PRINT("sys_msgctl ( %ld, %ld, %#lx )", SARG1, SARG2, ARG3);
   PRE_REG_READ3(long, "msgctl",
                 int, msqid, int, cmd, struct msqid_ds *, buf);
   ML_(linux_PRE_sys_msgctl)(tid, ARG1,ARG2,ARG3);
}

POST(sys_msgctl)
{
   ML_(linux_POST_sys_msgctl)(tid, RES,ARG1,ARG2,ARG3);
}

PRE(sys_shmget)
{
   PRINT("sys_shmget ( %ld, %lu, %ld )", SARG1, ARG2, SARG3);
   PRE_REG_READ3(long, "shmget", vki_key_t, key, vki_size_t, size, int, shmflg);
   if (ARG3 & VKI_SHM_HUGETLB) {
      static Bool warning_given = False;
      ARG3 &= ~VKI_SHM_HUGETLB;
      if (!warning_given) {
         warning_given = True;
         VG_(umsg)(
            "WARNING: valgrind ignores shmget(shmflg) SHM_HUGETLB\n");
      }
   }
}

PRE(wrap_sys_shmat)
{
   UWord arg2tmp;
   PRINT("wrap_sys_shmat ( %ld, %#lx, %ld )", SARG1, ARG2, SARG3);
   PRE_REG_READ3(long, "shmat",
                 int, shmid, const void *, shmaddr, int, shmflg);
#if defined(VGP_arm_linux)
   /* Round the attach address down to an VKI_SHMLBA boundary if the
      client requested rounding.  See #222545.  This is necessary only
      on arm-linux because VKI_SHMLBA is 4 * VKI_PAGE size; on all
      other linux targets it is the same as the page size. */
   if (ARG3 & VKI_SHM_RND)
      ARG2 = VG_ROUNDDN(ARG2, VKI_SHMLBA);
#endif
   arg2tmp = ML_(generic_PRE_sys_shmat)(tid, ARG1,ARG2,ARG3);
   if (arg2tmp == 0)
      SET_STATUS_Failure( VKI_EINVAL );
   else
      ARG2 = arg2tmp;  // used in POST
}

POST(wrap_sys_shmat)
{
   ML_(generic_POST_sys_shmat)(tid, RES,ARG1,ARG2,ARG3);
}

PRE(sys_shmdt)
{
   PRINT("sys_shmdt ( %#lx )",ARG1);
   PRE_REG_READ1(long, "shmdt", const void *, shmaddr);
   if (!ML_(generic_PRE_sys_shmdt)(tid, ARG1))
      SET_STATUS_Failure( VKI_EINVAL );
}

POST(sys_shmdt)
{
   ML_(generic_POST_sys_shmdt)(tid, RES,ARG1);
}

PRE(sys_shmctl)
{
   PRINT("sys_shmctl ( %ld, %ld, %#lx )", SARG1, SARG2, ARG3);
   PRE_REG_READ3(long, "shmctl",
                 int, shmid, int, cmd, struct shmid_ds *, buf);
#ifdef VGP_amd64_linux
   ML_(generic_PRE_sys_shmctl)(tid, ARG1,ARG2|VKI_IPC_64,ARG3);
#else
   ML_(generic_PRE_sys_shmctl)(tid, ARG1,ARG2,ARG3);
#endif
}

POST(sys_shmctl)
{
#ifdef VGP_amd64_linux
   ML_(generic_POST_sys_shmctl)(tid, RES,ARG1,ARG2|VKI_IPC_64,ARG3);
#else
   ML_(generic_POST_sys_shmctl)(tid, RES,ARG1,ARG2,ARG3);
#endif
}


/* ---------------------------------------------------------------------
   Generic handler for sys_socketcall
   Depending on the platform, some socket related syscalls (e.g. socketpair,
   socket, bind, ...)
   are either direct system calls, or are all implemented via sys_socketcall.
   ------------------------------------------------------------------ */
#ifdef __NR_socketcall
PRE(sys_socketcall)
{
#  define ARG2_0  (((UWord*)ARG2)[0])
#  define ARG2_1  (((UWord*)ARG2)[1])
#  define ARG2_2  (((UWord*)ARG2)[2])
#  define ARG2_3  (((UWord*)ARG2)[3])
#  define ARG2_4  (((UWord*)ARG2)[4])
#  define ARG2_5  (((UWord*)ARG2)[5])

// call PRE_MEM_READ and check for EFAULT result.
#define PRE_MEM_READ_ef(msg, arg, size)                         \
   {                                                            \
      PRE_MEM_READ( msg, arg, size);                            \
      if (!ML_(valid_client_addr)(arg, size, tid, NULL)) {      \
         SET_STATUS_Failure( VKI_EFAULT );                      \
         break;                                                 \
      }                                                         \
   }

   *flags |= SfMayBlock;
   PRINT("sys_socketcall ( %ld, %#lx )", SARG1, ARG2);
   PRE_REG_READ2(long, "socketcall", int, call, unsigned long *, args);

   switch (ARG1 /* request */) {

   case VKI_SYS_SOCKETPAIR:
      /* int socketpair(int d, int type, int protocol, int sv[2]); */
      PRE_MEM_READ_ef( "socketcall.socketpair(args)", ARG2, 4*sizeof(Addr) );
      ML_(generic_PRE_sys_socketpair)( tid, ARG2_0, ARG2_1, ARG2_2, ARG2_3 );
      break;

   case VKI_SYS_SOCKET:
      /* int socket(int domain, int type, int protocol); */
      PRE_MEM_READ_ef( "socketcall.socket(args)", ARG2, 3*sizeof(Addr) );
      break;

   case VKI_SYS_BIND:
      /* int bind(int sockfd, struct sockaddr *my_addr, 
                  int addrlen); */
      PRE_MEM_READ_ef( "socketcall.bind(args)", ARG2, 3*sizeof(Addr) );
      ML_(generic_PRE_sys_bind)( tid, ARG2_0, ARG2_1, ARG2_2 );
      break;
               
   case VKI_SYS_LISTEN:
      /* int listen(int s, int backlog); */
      PRE_MEM_READ_ef( "socketcall.listen(args)", ARG2, 2*sizeof(Addr) );
      break;

   case VKI_SYS_ACCEPT:
      /* int accept(int s, struct sockaddr *addr, int *addrlen); */
      PRE_MEM_READ_ef( "socketcall.accept(args)", ARG2, 3*sizeof(Addr) );
      ML_(generic_PRE_sys_accept)( tid, ARG2_0, ARG2_1, ARG2_2 );
      break;

   case VKI_SYS_ACCEPT4:
      /* int accept4(int s, struct sockaddr *addr, int *addrlen, int flags); */
      PRE_MEM_READ_ef( "socketcall.accept4(args)", ARG2, 4*sizeof(Addr) );
      ML_(generic_PRE_sys_accept)( tid, ARG2_0, ARG2_1, ARG2_2 );
      break;

   case VKI_SYS_SENDTO:
      /* int sendto(int s, const void *msg, int len, 
                    unsigned int flags, 
                    const struct sockaddr *to, int tolen); */
      PRE_MEM_READ_ef( "socketcall.sendto(args)", ARG2, 6*sizeof(Addr) );
      ML_(generic_PRE_sys_sendto)( tid, ARG2_0, ARG2_1, ARG2_2, 
                                   ARG2_3, ARG2_4, ARG2_5 );
      break;

   case VKI_SYS_SEND:
      /* int send(int s, const void *msg, size_t len, int flags); */
      PRE_MEM_READ_ef( "socketcall.send(args)", ARG2, 4*sizeof(Addr) );
      ML_(generic_PRE_sys_send)( tid, ARG2_0, ARG2_1, ARG2_2 );
      break;

   case VKI_SYS_RECVFROM:
      /* int recvfrom(int s, void *buf, int len, unsigned int flags,
         struct sockaddr *from, int *fromlen); */
      PRE_MEM_READ_ef( "socketcall.recvfrom(args)", ARG2, 6*sizeof(Addr) );
      ML_(generic_PRE_sys_recvfrom)( tid, ARG2_0, ARG2_1, ARG2_2, 
                                     ARG2_3, ARG2_4, ARG2_5 );
      break;
   
   case VKI_SYS_RECV:
      /* int recv(int s, void *buf, int len, unsigned int flags); */
      /* man 2 recv says:
         The  recv call is normally used only on a connected socket
         (see connect(2)) and is identical to recvfrom with a  NULL
         from parameter.
      */
      PRE_MEM_READ_ef( "socketcall.recv(args)", ARG2, 4*sizeof(Addr) );
      ML_(generic_PRE_sys_recv)( tid, ARG2_0, ARG2_1, ARG2_2 );
      break;

   case VKI_SYS_CONNECT:
      /* int connect(int sockfd, 
                     struct sockaddr *serv_addr, int addrlen ); */
      PRE_MEM_READ_ef( "socketcall.connect(args)", ARG2, 3*sizeof(Addr) );
      ML_(generic_PRE_sys_connect)( tid, ARG2_0, ARG2_1, ARG2_2 );
      break;

   case VKI_SYS_SETSOCKOPT:
      /* int setsockopt(int s, int level, int optname, 
                        const void *optval, int optlen); */
      PRE_MEM_READ_ef( "socketcall.setsockopt(args)", ARG2, 5*sizeof(Addr) );
      ML_(linux_PRE_sys_setsockopt)( tid, ARG2_0, ARG2_1, ARG2_2, 
                                     ARG2_3, ARG2_4 );
      break;

   case VKI_SYS_GETSOCKOPT:
      /* int getsockopt(int s, int level, int optname, 
                        void *optval, socklen_t *optlen); */
      PRE_MEM_READ_ef( "socketcall.getsockopt(args)", ARG2, 5*sizeof(Addr) );
      ML_(linux_PRE_sys_getsockopt)( tid, ARG2_0, ARG2_1, ARG2_2, 
                                     ARG2_3, ARG2_4 );
      break;

   case VKI_SYS_GETSOCKNAME:
      /* int getsockname(int s, struct sockaddr* name, int* namelen) */
      PRE_MEM_READ_ef( "socketcall.getsockname(args)", ARG2, 3*sizeof(Addr) );
      ML_(generic_PRE_sys_getsockname)( tid, ARG2_0, ARG2_1, ARG2_2 );
      break;

   case VKI_SYS_GETPEERNAME:
      /* int getpeername(int s, struct sockaddr* name, int* namelen) */
      PRE_MEM_READ_ef( "socketcall.getpeername(args)", ARG2, 3*sizeof(Addr) );
      ML_(generic_PRE_sys_getpeername)( tid, ARG2_0, ARG2_1, ARG2_2 );
      break;

   case VKI_SYS_SHUTDOWN:
      /* int shutdown(int s, int how); */
      PRE_MEM_READ_ef( "socketcall.shutdown(args)", ARG2, 2*sizeof(Addr) );
      break;

   case VKI_SYS_SENDMSG:
      /* int sendmsg(int s, const struct msghdr *msg, int flags); */
      PRE_MEM_READ_ef( "socketcall.sendmsg(args)", ARG2, 3*sizeof(Addr) );
      ML_(generic_PRE_sys_sendmsg)( tid, "msg", (struct vki_msghdr *)ARG2_1 );
      break;
      
   case VKI_SYS_RECVMSG:
      /* int recvmsg(int s, struct msghdr *msg, int flags); */
      PRE_MEM_READ_ef("socketcall.recvmsg(args)", ARG2, 3*sizeof(Addr) );
      ML_(generic_PRE_sys_recvmsg)( tid, "msg", (struct vki_msghdr *)ARG2_1 );
      break;

   case VKI_SYS_RECVMMSG:
      /* int recvmmsg(int s, struct mmsghdr *mmsg, int vlen, int flags,
                      struct timespec *timeout); */
      PRE_MEM_READ_ef("socketcall.recvmmsg(args)", ARG2, 5*sizeof(Addr) );
      ML_(linux_PRE_sys_recvmmsg)( tid, ARG2_0, ARG2_1, ARG2_2, ARG2_3,
                                   ARG2_4 );
      break;

   case VKI_SYS_SENDMMSG:
      /* int sendmmsg(int s, struct mmsghdr *mmsg, int vlen, int flags); */
      PRE_MEM_READ_ef("socketcall.sendmmsg(args)", ARG2, 4*sizeof(Addr) );
      ML_(linux_PRE_sys_sendmmsg)( tid, ARG2_0, ARG2_1, ARG2_2, ARG2_3 );
      break;

   default:
      VG_(message)(Vg_DebugMsg,"Warning: unhandled socketcall 0x%lx\n",ARG1);
      SET_STATUS_Failure( VKI_EINVAL );
      break;
   }
#  undef ARG2_0
#  undef ARG2_1
#  undef ARG2_2
#  undef ARG2_3
#  undef ARG2_4
#  undef ARG2_5
}

POST(sys_socketcall)
{
#  define ARG2_0  (((UWord*)ARG2)[0])
#  define ARG2_1  (((UWord*)ARG2)[1])
#  define ARG2_2  (((UWord*)ARG2)[2])
#  define ARG2_3  (((UWord*)ARG2)[3])
#  define ARG2_4  (((UWord*)ARG2)[4])
#  define ARG2_5  (((UWord*)ARG2)[5])

   SysRes r;
   vg_assert(SUCCESS);
   switch (ARG1 /* request */) {

   case VKI_SYS_SOCKETPAIR:
      r = ML_(generic_POST_sys_socketpair)( 
             tid, VG_(mk_SysRes_Success)(RES), 
             ARG2_0, ARG2_1, ARG2_2, ARG2_3 
          );
      SET_STATUS_from_SysRes(r);
      break;

   case VKI_SYS_SOCKET:
      r = ML_(generic_POST_sys_socket)( tid, VG_(mk_SysRes_Success)(RES) );
      SET_STATUS_from_SysRes(r);
      break;

   case VKI_SYS_BIND:
      /* int bind(int sockfd, struct sockaddr *my_addr, 
			int addrlen); */
      break;
               
   case VKI_SYS_LISTEN:
      /* int listen(int s, int backlog); */
      break;

   case VKI_SYS_ACCEPT:
   case VKI_SYS_ACCEPT4:
      /* int accept(int s, struct sockaddr *addr, int *addrlen); */
      /* int accept4(int s, struct sockaddr *addr, int *addrlen, int flags); */
     r = ML_(generic_POST_sys_accept)( tid, VG_(mk_SysRes_Success)(RES), 
                                            ARG2_0, ARG2_1, ARG2_2 );
     SET_STATUS_from_SysRes(r);
     break;

   case VKI_SYS_SENDTO:
      break;

   case VKI_SYS_SEND:
      break;

   case VKI_SYS_RECVFROM:
      ML_(generic_POST_sys_recvfrom)( tid, VG_(mk_SysRes_Success)(RES),
                                           ARG2_0, ARG2_1, ARG2_2,
                                           ARG2_3, ARG2_4, ARG2_5 );
      break;

   case VKI_SYS_RECV:
      ML_(generic_POST_sys_recv)( tid, RES, ARG2_0, ARG2_1, ARG2_2 );
      break;

   case VKI_SYS_CONNECT:
      break;

   case VKI_SYS_SETSOCKOPT:
      break;

   case VKI_SYS_GETSOCKOPT:
      ML_(linux_POST_sys_getsockopt)( tid, VG_(mk_SysRes_Success)(RES),
                                      ARG2_0, ARG2_1, 
                                      ARG2_2, ARG2_3, ARG2_4 );
      break;

   case VKI_SYS_GETSOCKNAME:
      ML_(generic_POST_sys_getsockname)( tid, VG_(mk_SysRes_Success)(RES),
                                              ARG2_0, ARG2_1, ARG2_2 );
      break;

   case VKI_SYS_GETPEERNAME:
      ML_(generic_POST_sys_getpeername)( tid, VG_(mk_SysRes_Success)(RES), 
                                              ARG2_0, ARG2_1, ARG2_2 );
      break;

   case VKI_SYS_SHUTDOWN:
      break;

   case VKI_SYS_SENDMSG:
      break;

   case VKI_SYS_RECVMSG:
      ML_(generic_POST_sys_recvmsg)( tid, "msg", (struct vki_msghdr *)ARG2_1, RES );
      break;

   case VKI_SYS_RECVMMSG:
      ML_(linux_POST_sys_recvmmsg)( tid, RES,
                                    ARG2_0, ARG2_1, ARG2_2, ARG2_3, ARG2_4 );
      break;

   case VKI_SYS_SENDMMSG:
      ML_(linux_POST_sys_sendmmsg)( tid, RES, ARG2_0, ARG2_1, ARG2_2, ARG2_3 );
      break;

   default:
      VG_(message)(Vg_DebugMsg,"FATAL: unhandled socketcall 0x%lx\n",ARG1);
      VG_(core_panic)("... bye!\n");
      break; /*NOTREACHED*/
   }
#  undef ARG2_0
#  undef ARG2_1
#  undef ARG2_2
#  undef ARG2_3
#  undef ARG2_4
#  undef ARG2_5
}
#endif

PRE(sys_socket)
{
   PRINT("sys_socket ( %ld, %ld, %ld )", SARG1, SARG2, SARG3);
   PRE_REG_READ3(long, "socket", int, domain, int, type, int, protocol);
}
POST(sys_socket)
{
   SysRes r;
   vg_assert(SUCCESS);
   r = ML_(generic_POST_sys_socket)(tid, VG_(mk_SysRes_Success)(RES));
   SET_STATUS_from_SysRes(r);
}

PRE(sys_setsockopt)
{
   PRINT("sys_setsockopt ( %ld, %ld, %ld, %#lx, %lu )",
         SARG1, SARG2, SARG3, ARG4, ARG5);
   PRE_REG_READ5(long, "setsockopt",
                 int, s, int, level, int, optname,
                 const void *, optval, unsigned, optlen); // socklen_t
   ML_(linux_PRE_sys_setsockopt)(tid, ARG1,ARG2,ARG3,ARG4,ARG5);
}

PRE(sys_getsockopt)
{
   PRINT("sys_getsockopt ( %ld, %ld, %ld, %#lx, %ld )",
         SARG1, SARG2, SARG3, ARG4, SARG5);
   PRE_REG_READ5(long, "getsockopt",
                 int, s, int, level, int, optname,
                 void *, optval, int, *optlen);
   ML_(linux_PRE_sys_getsockopt)(tid, ARG1,ARG2,ARG3,ARG4,ARG5);
}
POST(sys_getsockopt)
{
   vg_assert(SUCCESS);
   ML_(linux_POST_sys_getsockopt)(tid, VG_(mk_SysRes_Success)(RES),
                                       ARG1,ARG2,ARG3,ARG4,ARG5);
}

PRE(sys_connect)
{
   *flags |= SfMayBlock;
   PRINT("sys_connect ( %ld, %#lx, %ld )", SARG1, ARG2, SARG3);
   PRE_REG_READ3(long, "connect",
                 int, sockfd, struct sockaddr *, serv_addr, int, addrlen);
   ML_(generic_PRE_sys_connect)(tid, ARG1,ARG2,ARG3);
}

PRE(sys_accept)
{
   *flags |= SfMayBlock;
   PRINT("sys_accept ( %ld, %#lx, %#lx )", SARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "accept",
                 int, s, struct sockaddr *, addr, int *, addrlen);
   ML_(generic_PRE_sys_accept)(tid, ARG1,ARG2,ARG3);
}
POST(sys_accept)
{
   SysRes r;
   vg_assert(SUCCESS);
   r = ML_(generic_POST_sys_accept)(tid, VG_(mk_SysRes_Success)(RES),
                                         ARG1,ARG2,ARG3);
   SET_STATUS_from_SysRes(r);
}

PRE(sys_accept4)
{
   *flags |= SfMayBlock;
   PRINT("sys_accept4 ( %ld, %#lx, %#lx, %ld )", SARG1, ARG2, ARG3, SARG4);
   PRE_REG_READ4(long, "accept4",
                 int, s, struct sockaddr *, addr, int *, addrlen, int, flags);
   ML_(generic_PRE_sys_accept)(tid, ARG1,ARG2,ARG3);
}
POST(sys_accept4)
{
   SysRes r;
   vg_assert(SUCCESS);
   r = ML_(generic_POST_sys_accept)(tid, VG_(mk_SysRes_Success)(RES),
                                         ARG1,ARG2,ARG3);
   SET_STATUS_from_SysRes(r);
}

PRE(sys_send)
{
   *flags |= SfMayBlock;
   PRINT("sys_send ( %ld, %#lx, %lu, %#lx )", SARG1, ARG2, ARG3, ARG4);
   PRE_REG_READ4(long, "send",
                 int, s, const void *, msg, vki_size_t, len, 
                 int, flags);

   ML_(generic_PRE_sys_send)( tid, ARG1, ARG2, ARG3 );
}

PRE(sys_sendto)
{
   *flags |= SfMayBlock;
   PRINT("sys_sendto ( %ld, %#lx, %lu, %lu, %#lx, %ld )",
         SARG1, ARG2, ARG3, ARG4, ARG5, SARG6);
   PRE_REG_READ6(long, "sendto",
                 int, s, const void *, msg, vki_size_t, len, 
                 unsigned int, flags, 
                 const struct sockaddr *, to, int, tolen);
   ML_(generic_PRE_sys_sendto)(tid, ARG1,ARG2,ARG3,ARG4,ARG5,ARG6);
}

PRE (sys_recv) 
{
  *flags |= SfMayBlock;
  PRINT ("sys_recv ( %ld, %#lx, %lu, %lu )", SARG1, ARG2, ARG3, ARG4);
  PRE_REG_READ4 (long, "recv", int, s, void *, buf, vki_size_t, len,
                 unsigned int, flags);
  ML_ (generic_PRE_sys_recv) (tid, ARG1, ARG2, ARG3);
} 

POST (sys_recv) 
{
  ML_ (generic_POST_sys_recv) (tid, RES, ARG1, ARG2, ARG3);
} 

PRE(sys_recvfrom)
{
   *flags |= SfMayBlock;
   PRINT("sys_recvfrom ( %ld, %#lx, %lu, %lu, %#lx, %#lx )",
         SARG1, ARG2, ARG3, ARG4, ARG5, ARG6);
   PRE_REG_READ6(long, "recvfrom",
                 int, s, void *, buf, vki_size_t, len, unsigned int, flags,
                 struct sockaddr *, from, int *, fromlen);
   ML_(generic_PRE_sys_recvfrom)(tid, ARG1,ARG2,ARG3,ARG4,ARG5,ARG6);
}
POST(sys_recvfrom)
{
   vg_assert(SUCCESS);
   ML_(generic_POST_sys_recvfrom)(tid, VG_(mk_SysRes_Success)(RES),
                                       ARG1,ARG2,ARG3,ARG4,ARG5,ARG6);
}

PRE(sys_sendmsg)
{
   *flags |= SfMayBlock;
   PRINT("sys_sendmsg ( %ld, %#lx, %lu )", SARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "sendmsg",
                 int, s, const struct msghdr *, msg, unsigned int, flags);
   ML_(generic_PRE_sys_sendmsg)(tid, "msg", (struct vki_msghdr *)ARG2);
}

PRE(sys_recvmsg)
{
   *flags |= SfMayBlock;
   PRINT("sys_recvmsg ( %ld, %#lx, %lu )", SARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "recvmsg", int, s, struct msghdr *, msg,
                 unsigned int, flags);
   ML_(generic_PRE_sys_recvmsg)(tid, "msg", (struct vki_msghdr *)ARG2);
}
POST(sys_recvmsg)
{
   ML_(generic_POST_sys_recvmsg)(tid, "msg", (struct vki_msghdr *)ARG2, RES);
}

PRE(sys_shutdown)
{
   *flags |= SfMayBlock;
   PRINT("sys_shutdown ( %ld, %ld )", SARG1, SARG2);
   PRE_REG_READ2(int, "shutdown", int, s, int, how);
}

PRE(sys_bind)
{
   PRINT("sys_bind ( %ld, %#lx, %ld )", SARG1, ARG2, SARG3);
   PRE_REG_READ3(long, "bind",
                 int, sockfd, struct sockaddr *, my_addr, int, addrlen);
   ML_(generic_PRE_sys_bind)(tid, ARG1,ARG2,ARG3);
}

PRE(sys_listen)
{
   PRINT("sys_listen ( %ld, %ld )", SARG1, SARG2);
   PRE_REG_READ2(long, "listen", int, s, int, backlog);
}

PRE(sys_getsockname)
{
   PRINT("sys_getsockname ( %ld, %#lx, %#lx )", SARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "getsockname",
                 int, s, struct sockaddr *, name, int *, namelen);
   ML_(generic_PRE_sys_getsockname)(tid, ARG1,ARG2,ARG3);
}
POST(sys_getsockname)
{
   vg_assert(SUCCESS);
   ML_(generic_POST_sys_getsockname)(tid, VG_(mk_SysRes_Success)(RES),
                                          ARG1,ARG2,ARG3);
}

PRE(sys_getpeername)
{
   PRINT("sys_getpeername ( %ld, %#lx, %#lx )", SARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "getpeername",
                 int, s, struct sockaddr *, name, int *, namelen);
   ML_(generic_PRE_sys_getpeername)(tid, ARG1,ARG2,ARG3);
}
POST(sys_getpeername)
{
   vg_assert(SUCCESS);
   ML_(generic_POST_sys_getpeername)(tid, VG_(mk_SysRes_Success)(RES),
                                          ARG1,ARG2,ARG3);
}

PRE(sys_socketpair)
{
   PRINT("sys_socketpair ( %ld, %ld, %ld, %#lx )", SARG1, SARG2, SARG3, ARG4);
   PRE_REG_READ4(long, "socketpair",
                 int, d, int, type, int, protocol, int*, sv);
   ML_(generic_PRE_sys_socketpair)(tid, ARG1,ARG2,ARG3,ARG4);
}
POST(sys_socketpair)
{
   vg_assert(SUCCESS);
   ML_(generic_POST_sys_socketpair)(tid, VG_(mk_SysRes_Success)(RES),
                                         ARG1,ARG2,ARG3,ARG4);
}


/* ---------------------------------------------------------------------
   *at wrappers
   ------------------------------------------------------------------ */

PRE(sys_openat)
{
   HChar  name[30];   // large enough
   SysRes sres;

   if (ARG3 & VKI_O_CREAT) {
      // 4-arg version
      PRINT("sys_openat ( %ld, %#lx(%s), %ld, %ld )",
            SARG1, ARG2, (HChar*)ARG2, SARG3, SARG4);
      PRE_REG_READ4(long, "openat",
                    int, dfd, const char *, filename, int, flags, int, mode);
   } else {
      // 3-arg version
      PRINT("sys_openat ( %ld, %#lx(%s), %ld )",
            SARG1, ARG2, (HChar*)ARG2, SARG3);
      PRE_REG_READ3(long, "openat",
                    int, dfd, const char *, filename, int, flags);
   }

   PRE_MEM_RASCIIZ( "openat(filename)", ARG2 );

   /* For absolute filenames, dfd is ignored.  If dfd is AT_FDCWD,
      filename is relative to cwd.  When comparing dfd against AT_FDCWD,
      be sure only to compare the bottom 32 bits. */
   if (ML_(safe_to_deref)( (void*)ARG2, 1 )
       && *(Char *)ARG2 != '/'
       && ((Int)ARG1) != ((Int)VKI_AT_FDCWD)
       && !ML_(fd_allowed)(ARG1, "openat", tid, False))
      SET_STATUS_Failure( VKI_EBADF );

   /* Handle the case where the open is of /proc/self/cmdline or
      /proc/<pid>/cmdline, and just give it a copy of the fd for the
      fake file we cooked up at startup (in m_main).  Also, seek the
      cloned fd back to the start. */

   VG_(sprintf)(name, "/proc/%d/cmdline", VG_(getpid)());
   if (ML_(safe_to_deref)( (void*)ARG2, 1 )
       && (VG_(strcmp)((HChar *)ARG2, name) == 0 
           || VG_(strcmp)((HChar *)ARG2, "/proc/self/cmdline") == 0)) {
      sres = VG_(dup)( VG_(cl_cmdline_fd) );
      SET_STATUS_from_SysRes( sres );
      if (!sr_isError(sres)) {
         OffT off = VG_(lseek)( sr_Res(sres), 0, VKI_SEEK_SET );
         if (off < 0)
            SET_STATUS_Failure( VKI_EMFILE );
      }
      return;
   }

   /* Do the same for /proc/self/auxv or /proc/<pid>/auxv case. */

   VG_(sprintf)(name, "/proc/%d/auxv", VG_(getpid)());
   if (ML_(safe_to_deref)( (void*)ARG2, 1 )
       && (VG_(strcmp)((HChar *)ARG2, name) == 0 
           || VG_(strcmp)((HChar *)ARG2, "/proc/self/auxv") == 0)) {
      sres = VG_(dup)( VG_(cl_auxv_fd) );
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
         ML_(record_fd_open_with_given_name)(tid, RES, (HChar*)ARG2);
   }
}

PRE(sys_mkdirat)
{
   *flags |= SfMayBlock;
   PRINT("sys_mkdirat ( %ld, %#lx(%s), %ld )",
         SARG1, ARG2, (HChar*)ARG2, SARG3);
   PRE_REG_READ3(long, "mkdirat",
                 int, dfd, const char *, pathname, int, mode);
   PRE_MEM_RASCIIZ( "mkdirat(pathname)", ARG2 );
}

PRE(sys_mknodat)
{
   PRINT("sys_mknodat ( %ld, %#lx(%s), 0x%lx, 0x%lx )",
         SARG1, ARG2, (HChar*)ARG2, ARG3, ARG4 );
   PRE_REG_READ4(long, "mknodat",
                 int, dfd, const char *, pathname, int, mode, unsigned, dev);
   PRE_MEM_RASCIIZ( "mknodat(pathname)", ARG2 );
}

PRE(sys_fchownat)
{
   PRINT("sys_fchownat ( %ld, %#lx(%s), 0x%lx, 0x%lx )",
         SARG1, ARG2, (HChar*)ARG2, ARG3, ARG4);
   PRE_REG_READ4(long, "fchownat",
                 int, dfd, const char *, path,
                 vki_uid_t, owner, vki_gid_t, group);
   PRE_MEM_RASCIIZ( "fchownat(path)", ARG2 );
}

PRE(sys_futimesat)
{
   PRINT("sys_futimesat ( %ld, %#lx(%s), %#lx )",
         SARG1, ARG2, (HChar*)ARG2, ARG3);
   PRE_REG_READ3(long, "futimesat",
                 int, dfd, char *, filename, struct timeval *, tvp);
   if (ARG2 != 0)
      PRE_MEM_RASCIIZ( "futimesat(filename)", ARG2 );
   if (ARG3 != 0)
      PRE_MEM_READ( "futimesat(tvp)", ARG3, 2 * sizeof(struct vki_timeval) );
}

PRE(sys_utimensat)
{
   PRINT("sys_utimensat ( %ld, %#lx(%s), %#lx, 0x%lx )",
         SARG1, ARG2, (HChar*)ARG2, ARG3, ARG4);
   PRE_REG_READ4(long, "utimensat",
                 int, dfd, char *, filename, struct timespec *, utimes, int, flags);
   if (ARG2 != 0)
      PRE_MEM_RASCIIZ( "utimensat(filename)", ARG2 );
   if (ARG3 != 0)
      PRE_MEM_READ( "utimensat(tvp)", ARG3, 2 * sizeof(struct vki_timespec) );
}

PRE(sys_newfstatat)
{
   FUSE_COMPATIBLE_MAY_BLOCK();
   PRINT("sys_newfstatat ( %ld, %#lx(%s), %#lx )",
         SARG1, ARG2, (HChar*)ARG2, ARG3);
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
   PRINT("sys_unlinkat ( %ld, %#lx(%s) )", SARG1, ARG2, (HChar*)ARG2);
   PRE_REG_READ2(long, "unlinkat", int, dfd, const char *, pathname);
   PRE_MEM_RASCIIZ( "unlinkat(pathname)", ARG2 );
}

PRE(sys_renameat)
{
   PRINT("sys_renameat ( %ld, %#lx(%s), %ld, %#lx(%s) )",
         SARG1, ARG2, (HChar*)ARG2, SARG3, ARG4, (HChar*)ARG4);
   PRE_REG_READ4(long, "renameat",
                 int, olddfd, const char *, oldpath,
                 int, newdfd, const char *, newpath);
   PRE_MEM_RASCIIZ( "renameat(oldpath)", ARG2 );
   PRE_MEM_RASCIIZ( "renameat(newpath)", ARG4 );
}

PRE(sys_linkat)
{
   *flags |= SfMayBlock;
   PRINT("sys_linkat ( %ld, %#lx(%s), %ld, %#lx(%s), %ld )",
         SARG1, ARG2, (HChar*)ARG2, SARG3, ARG4, (HChar*)ARG4, SARG5);
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
   PRINT("sys_symlinkat ( %#lx(%s), %ld, %#lx(%s) )",
         ARG1, (HChar*)ARG1, SARG2, ARG3, (HChar*)ARG3);
   PRE_REG_READ3(long, "symlinkat",
                 const char *, oldpath, int, newdfd, const char *, newpath);
   PRE_MEM_RASCIIZ( "symlinkat(oldpath)", ARG1 );
   PRE_MEM_RASCIIZ( "symlinkat(newpath)", ARG3 );
}

PRE(sys_readlinkat)
{
   HChar name[30];       // large enough
   Word  saved = SYSNO;

   PRINT("sys_readlinkat ( %ld, %#lx(%s), %#lx, %lu )",
         SARG1, ARG2, (HChar*)ARG2, ARG3, ARG4);
   PRE_REG_READ4(long, "readlinkat",
                 int, dfd, const char *, path, char *, buf, vki_size_t, bufsiz);
   PRE_MEM_RASCIIZ( "readlinkat(path)", ARG2 );
   PRE_MEM_WRITE( "readlinkat(buf)", ARG3,ARG4 );

   /*
    * Handle the case where readlinkat is looking at /proc/self/exe or
    * /proc/<pid>/exe.
    */
   VG_(sprintf)(name, "/proc/%d/exe", VG_(getpid)());
   if (ML_(safe_to_deref)((void*)ARG2, 1)
       && (VG_(strcmp)((HChar *)ARG2, name) == 0 
           || VG_(strcmp)((HChar *)ARG2, "/proc/self/exe") == 0)) {
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
   PRINT("sys_fchmodat ( %ld, %#lx(%s), %lu )",
         SARG1, ARG2, (HChar*)ARG2, ARG3);
   PRE_REG_READ3(long, "fchmodat",
                 int, dfd, const char *, path, vki_mode_t, mode);
   PRE_MEM_RASCIIZ( "fchmodat(path)", ARG2 );
}

PRE(sys_faccessat)
{
   PRINT("sys_faccessat ( %ld, %#lx(%s), %ld )",
         SARG1, ARG2, (HChar*)ARG2, SARG3);
   PRE_REG_READ3(long, "faccessat",
                 int, dfd, const char *, pathname, int, mode);
   PRE_MEM_RASCIIZ( "faccessat(pathname)", ARG2 );
}

PRE(sys_name_to_handle_at)
{
   PRINT("sys_name_to_handle_at ( %ld, %#lx(%s), %#lx, %#lx, %ld )",
         SARG1, ARG2, (HChar*)ARG2, ARG3, ARG4, SARG5);
   PRE_REG_READ5(int, "name_to_handle_at",
                 int, dfd, const char *, name,
                 struct vki_file_handle *, handle,
                 int *, mnt_id, int, flag);
   PRE_MEM_RASCIIZ( "name_to_handle_at(name)", ARG2 );
   if (ML_(safe_to_deref)( (void*)ARG3, sizeof(struct vki_file_handle))) {
      struct vki_file_handle *fh = (struct vki_file_handle *)ARG3;
      PRE_MEM_READ( "name_to_handle_at(handle)", (Addr)&fh->handle_bytes, sizeof(fh->handle_bytes) );
      PRE_MEM_WRITE( "name_to_handle_at(handle)", (Addr)fh, sizeof(struct vki_file_handle) + fh->handle_bytes );
   }
   PRE_MEM_WRITE( "name_to_handle_at(mnt_id)", ARG4, sizeof(int) );
}

POST(sys_name_to_handle_at)
{
   struct vki_file_handle *fh = (struct vki_file_handle *)ARG3;
   POST_MEM_WRITE( ARG3, sizeof(struct vki_file_handle) + fh->handle_bytes );
   POST_MEM_WRITE( ARG4, sizeof(int) );
}

PRE(sys_open_by_handle_at)
{
   *flags |= SfMayBlock;
   PRINT("sys_open_by_handle_at ( %ld, %#lx, %ld )", SARG1, ARG2, SARG3);
   PRE_REG_READ3(int, "open_by_handle_at",
                 int, mountdirfd,
                 struct vki_file_handle *, handle,
                 int, flags);
   PRE_MEM_READ( "open_by_handle_at(handle)", ARG2, sizeof(struct vki_file_handle) + ((struct vki_file_handle*)ARG2)->handle_bytes );
}

POST(sys_open_by_handle_at)
{
   vg_assert(SUCCESS);
   if (!ML_(fd_allowed)(RES, "open_by_handle_at", tid, True)) {
      VG_(close)(RES);
      SET_STATUS_Failure( VKI_EMFILE );
   } else {
      if (VG_(clo_track_fds))
         ML_(record_fd_open_with_given_name)(tid, RES, (HChar*)ARG2);
   }
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
   PRINT("sys_preadv ( %lu, %#lx, %lu, %lld )",
         ARG1, ARG2, ARG3, (Long)LOHI64(ARG4,ARG5));
   PRE_REG_READ5(ssize_t, "preadv",
                 unsigned long, fd, const struct iovec *, vector,
                 unsigned long, count, vki_u32, offset_low,
                 vki_u32, offset_high);
#elif VG_WORDSIZE == 8
   PRINT("sys_preadv ( %lu, %#lx, %lu, %ld )", ARG1, ARG2, ARG3, SARG4);
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
   PRINT("sys_pwritev ( %lu, %#lx, %lu, %lld )",
         ARG1, ARG2, ARG3, (Long)LOHI64(ARG4,ARG5));
   PRE_REG_READ5(ssize_t, "pwritev",
                 unsigned long, fd, const struct iovec *, vector,
                 unsigned long, count, vki_u32, offset_low,
                 vki_u32, offset_high);
#elif VG_WORDSIZE == 8
   PRINT("sys_pwritev ( %lu, %#lx, %lu, %ld )", ARG1, ARG2, ARG3, SARG4);
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
   process_vm_{read,write}v wrappers
   ------------------------------------------------------------------ */

PRE(sys_process_vm_readv)
{
   PRINT("sys_process_vm_readv ( %ld, %#lx, %lu, %#lx, %lu, %lu )",
         SARG1, ARG2, ARG3, ARG4, ARG5, ARG6);
   PRE_REG_READ6(ssize_t, "process_vm_readv",
                 vki_pid_t, pid,
                 const struct iovec *, lvec,
                 unsigned long, liovcnt,
                 const struct iovec *, rvec,
                 unsigned long, riovcnt,
                 unsigned long, flags);
   PRE_MEM_READ( "process_vm_readv(lvec)",
                 ARG2, ARG3 * sizeof(struct vki_iovec) );
   PRE_MEM_READ( "process_vm_readv(rvec)",
                 ARG4, ARG5 * sizeof(struct vki_iovec) );
   if (ARG2 != 0) {
      /* TODO: Don't do any of the following if lvec is invalid */
      const struct vki_iovec *vec = (const struct vki_iovec *)ARG2;
      UInt i;
      for (i = 0; i < ARG3; i++)
         PRE_MEM_WRITE( "process_vm_readv(lvec[...])",
                        (Addr)vec[i].iov_base, vec[i].iov_len );
   }
}

POST(sys_process_vm_readv)
{
   const struct vki_iovec *vec = (const struct vki_iovec *)ARG2;
   UInt remains = RES;
   UInt i;
   for (i = 0; i < ARG3; i++) {
      UInt nReadThisBuf = vec[i].iov_len <= remains ?
                          vec[i].iov_len : remains;
      POST_MEM_WRITE( (Addr)vec[i].iov_base, nReadThisBuf );
      remains -= nReadThisBuf;
   }
}

PRE(sys_process_vm_writev)
{
   PRINT("sys_process_vm_writev ( %ld, %#lx, %lu, %#lx, %lu, %lu )",
         SARG1, ARG2, ARG3, ARG4, ARG5, ARG6);
   PRE_REG_READ6(ssize_t, "process_vm_writev",
                 vki_pid_t, pid,
                 const struct iovec *, lvec,
                 unsigned long, liovcnt,
                 const struct iovec *, rvec,
                 unsigned long, riovcnt,
                 unsigned long, flags);
   PRE_MEM_READ( "process_vm_writev(lvec)",
                 ARG2, ARG3 * sizeof(struct vki_iovec) );
   PRE_MEM_READ( "process_vm_writev(rvec)",
                 ARG4, ARG5 * sizeof(struct vki_iovec) );
   if (ARG2 != 0) {
      /* TODO: Don't do any of the following if lvec is invalid */
      const struct vki_iovec *vec = (const struct vki_iovec *)ARG2;
      UInt i;
      for (i = 0; i < ARG3; i++)
         PRE_MEM_READ( "process_vm_writev(lvec[...])",
                       (Addr)vec[i].iov_base, vec[i].iov_len );
   }
}

/* ---------------------------------------------------------------------
   {send,recv}mmsg wrappers
   ------------------------------------------------------------------ */

PRE(sys_sendmmsg)
{
   *flags |= SfMayBlock;
   PRINT("sys_sendmmsg ( %ld, %#lx, %ld, %ld )", SARG1, ARG2, SARG3, SARG4);
   PRE_REG_READ4(long, "sendmmsg",
                 int, s, const struct mmsghdr *, mmsg, int, vlen, int, flags);
   ML_(linux_PRE_sys_sendmmsg)(tid, ARG1,ARG2,ARG3,ARG4);
}

POST(sys_sendmmsg)
{
   ML_(linux_POST_sys_sendmmsg) (tid, RES, ARG1,ARG2,ARG3,ARG4);
}

PRE(sys_recvmmsg)
{
   *flags |= SfMayBlock;
   PRINT("sys_recvmmsg ( %ld, %#lx, %ld, %ld, %#lx )",
         SARG1, ARG2, SARG3, SARG4, ARG5);
   PRE_REG_READ5(long, "recvmmsg",
                 int, s, struct mmsghdr *, mmsg, int, vlen,
                 int, flags, struct timespec *, timeout);
   ML_(linux_PRE_sys_recvmmsg)(tid, ARG1,ARG2,ARG3,ARG4,ARG5);
}

POST(sys_recvmmsg)
{
   ML_(linux_POST_sys_recvmmsg) (tid, RES, ARG1,ARG2,ARG3,ARG4,ARG5);
}

/* ---------------------------------------------------------------------
   key retention service wrappers
   ------------------------------------------------------------------ */

PRE(sys_request_key)
{
   PRINT("sys_request_key ( %#lx(%s), %#lx(%s), %#lx(%s), %ld )",
         ARG1, (HChar*)ARG1, ARG2, (HChar*)ARG2, ARG3, (HChar*)ARG3, SARG4);
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
   PRINT("sys_add_key ( %#lx(%s), %#lx(%s), %#lx, %lu, %ld )",
         ARG1, (HChar*)ARG1, ARG2, (HChar*)ARG2, ARG3, ARG4, SARG5);
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
      PRINT("sys_keyctl ( KEYCTL_GET_KEYRING_ID, %ld, %ld )", SARG2, SARG3);
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
      PRINT("sys_keyctl ( KEYCTL_UPDATE, %ld, %#lx, %lu )", SARG2, ARG3, ARG4);
      PRE_REG_READ4(long, "keyctl(KEYCTL_UPDATE)",
                    int, option, vki_key_serial_t, key,
                    const void *, payload, vki_size_t, plen);
      if (ARG3 != (UWord)NULL)
         PRE_MEM_READ("keyctl(KEYCTL_UPDATE, payload)", ARG3, ARG4);
      break;
   case VKI_KEYCTL_REVOKE:
      PRINT("sys_keyctl ( KEYCTL_REVOKE, %ld )", SARG2);
      PRE_REG_READ2(long, "keyctl(KEYCTL_REVOKE)",
                    int, option, vki_key_serial_t, id);
      break;
   case VKI_KEYCTL_CHOWN:
      PRINT("sys_keyctl ( KEYCTL_CHOWN, %ld, %lu, %lu )", SARG2, ARG3, ARG4);
      PRE_REG_READ4(long, "keyctl(KEYCTL_CHOWN)",
                    int, option, vki_key_serial_t, id,
                    vki_uid_t, uid, vki_gid_t, gid);
      break;
   case VKI_KEYCTL_SETPERM:
      PRINT("sys_keyctl ( KEYCTL_SETPERM, %ld, %lu )", SARG2, ARG3);
      PRE_REG_READ3(long, "keyctl(KEYCTL_SETPERM)",
                    int, option, vki_key_serial_t, id, vki_key_perm_t, perm);
      break;
   case VKI_KEYCTL_DESCRIBE:
      PRINT("sys_keyctl ( KEYCTL_DESCRIBE, %ld, %#lx, %lu )", SARG2, ARG3, ARG4);
      PRE_REG_READ4(long, "keyctl(KEYCTL_DESCRIBE)",
                    int, option, vki_key_serial_t, id,
                    char *, buffer, vki_size_t, buflen);
      if (ARG3 != (UWord)NULL)
         PRE_MEM_WRITE("keyctl(KEYCTL_DESCRIBE, buffer)", ARG3, ARG4);
      break;
   case VKI_KEYCTL_CLEAR:
      PRINT("sys_keyctl ( KEYCTL_CLEAR, %ld )", SARG2);
      PRE_REG_READ2(long, "keyctl(KEYCTL_CLEAR)",
                    int, option, vki_key_serial_t, keyring);
      break;
   case VKI_KEYCTL_LINK:
      PRINT("sys_keyctl ( KEYCTL_LINK, %ld, %ld )", SARG2, SARG3);
      PRE_REG_READ3(long, "keyctl(KEYCTL_LINK)", int, option,
                    vki_key_serial_t, keyring, vki_key_serial_t, key);
      break;
   case VKI_KEYCTL_UNLINK:
      PRINT("sys_keyctl ( KEYCTL_UNLINK, %ld, %ld )", SARG2, SARG3);
      PRE_REG_READ3(long, "keyctl(KEYCTL_UNLINK)", int, option,
                    vki_key_serial_t, keyring, vki_key_serial_t, key);
      break;
   case VKI_KEYCTL_SEARCH:
      PRINT("sys_keyctl ( KEYCTL_SEARCH, %ld, %#lx(%s), %#lx(%s), %ld )",
            SARG2, ARG3, (HChar*)ARG3, ARG4, (HChar*)ARG4, SARG5);
      PRE_REG_READ5(long, "keyctl(KEYCTL_SEARCH)",
                    int, option, vki_key_serial_t, keyring, 
                    const char *, type, const char *, description,
                    vki_key_serial_t, destring);
      PRE_MEM_RASCIIZ("sys_keyctl(KEYCTL_SEARCH, type)", ARG3);
      PRE_MEM_RASCIIZ("sys_keyctl(KEYCTL_SEARCH, description)", ARG4);
      break;
   case VKI_KEYCTL_READ:
      PRINT("sys_keyctl ( KEYCTL_READ, %ld, %#lx, %lu )", SARG2, ARG3, ARG4);
      PRE_REG_READ4(long, "keyctl(KEYCTL_READ)",
                    int, option, vki_key_serial_t, keyring, 
                    char *, buffer, vki_size_t, buflen);
      if (ARG3 != (UWord)NULL)
         PRE_MEM_WRITE("keyctl(KEYCTL_READ, buffer)", ARG3, ARG4);
      break;
   case VKI_KEYCTL_INSTANTIATE:
      PRINT("sys_keyctl ( KEYCTL_INSTANTIATE, %ld, %#lx, %lu, %ld )",
            SARG2, ARG3, ARG4, SARG5);
      PRE_REG_READ5(long, "keyctl(KEYCTL_INSTANTIATE)",
                    int, option, vki_key_serial_t, key, 
                    char *, payload, vki_size_t, plen,
                    vki_key_serial_t, keyring);
      if (ARG3 != (UWord)NULL)
         PRE_MEM_READ("keyctl(KEYCTL_INSTANTIATE, payload)", ARG3, ARG4);
      break;
   case VKI_KEYCTL_NEGATE:
      PRINT("sys_keyctl ( KEYCTL_NEGATE, %ld, %lu, %ld )", SARG2, ARG3, SARG4);
      PRE_REG_READ4(long, "keyctl(KEYCTL_NEGATE)",
                    int, option, vki_key_serial_t, key, 
                    unsigned, timeout, vki_key_serial_t, keyring);
      break;
   case VKI_KEYCTL_SET_REQKEY_KEYRING:
      PRINT("sys_keyctl ( KEYCTL_SET_REQKEY_KEYRING, %ld )", SARG2);
      PRE_REG_READ2(long, "keyctl(KEYCTL_SET_REQKEY_KEYRING)",
                    int, option, int, reqkey_defl);
      break;
   case VKI_KEYCTL_SET_TIMEOUT:
      PRINT("sys_keyctl ( KEYCTL_SET_TIMEOUT, %ld, %lu )", SARG2, ARG3);
      PRE_REG_READ3(long, "keyctl(KEYCTL_SET_TIMEOUT)",
                    int, option, vki_key_serial_t, key, unsigned, timeout);
      break;
   case VKI_KEYCTL_ASSUME_AUTHORITY:
      PRINT("sys_keyctl ( KEYCTL_ASSUME_AUTHORITY, %ld )", SARG2);
      PRE_REG_READ2(long, "keyctl(KEYCTL_ASSUME_AUTHORITY)",
                    int, option, vki_key_serial_t, key);
      break;
   default:
      PRINT("sys_keyctl ( %ld ) ", SARG1);
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
   PRINT("sys_ioprio_set ( %ld, %ld, %ld )", SARG1, SARG2, SARG3);
   PRE_REG_READ3(int, "ioprio_set", int, which, int, who, int, ioprio);
}

PRE(sys_ioprio_get)
{
   PRINT("sys_ioprio_get ( %ld, %ld )", SARG1, SARG2);
   PRE_REG_READ2(int, "ioprio_get", int, which, int, who);
}

/* ---------------------------------------------------------------------
   _module wrappers
   ------------------------------------------------------------------ */

PRE(sys_init_module)
{
   *flags |= SfMayBlock;
   PRINT("sys_init_module ( %#lx, %lu, %#lx(\"%s\") )",
         ARG1, ARG2, ARG3, (HChar*)ARG3);
   PRE_REG_READ3(long, "init_module",
                 void *, umod, unsigned long, len, const char *, uargs);
   PRE_MEM_READ( "init_module(umod)", ARG1, ARG2 );
   PRE_MEM_RASCIIZ( "init_module(uargs)", ARG3 );
}

PRE(sys_delete_module)
{
   *flags |= SfMayBlock;
   PRINT("sys_delete_module ( %#lx(\"%s\"), 0x%lx )", ARG1, (HChar*)ARG1, ARG2);
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
   PRINT("sys_splice ( %ld, %#lx, %ld, %#lx, %lu, %#lx )",
         SARG1, ARG2, SARG3, ARG4, ARG5, ARG6);
   PRE_REG_READ6(vki_ssize_t, "splice",
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

PRE(sys_tee)
{
   *flags |= SfMayBlock;
   PRINT("sys_tree ( %ld, %ld, %lu, %#lx )", SARG1, SARG2, ARG3, ARG4);
   PRE_REG_READ4(vki_ssize_t, "tee",
                 int, fd_in, int, fd_out,
                 vki_size_t, len, unsigned int, flags);
   if (!ML_(fd_allowed)(ARG1, "tee(fd_in)", tid, False) ||
       !ML_(fd_allowed)(ARG2, "tee(fd_out)", tid, False)) {
      SET_STATUS_Failure( VKI_EBADF );
   }
}

PRE(sys_vmsplice)
{
   Int fdfl;
   *flags |= SfMayBlock;
   PRINT("sys_vmsplice ( %ld, %#lx, %lu, %lu )", SARG1, ARG2, ARG3, ARG4);
   PRE_REG_READ4(vki_ssize_t, "splice",
                 int, fd, struct vki_iovec *, iov,
                 unsigned long, nr_segs, unsigned int, flags);
   if (!ML_(fd_allowed)(ARG1, "vmsplice(fd)", tid, False)) {
      SET_STATUS_Failure( VKI_EBADF );
   } else if ((fdfl = VG_(fcntl)(ARG1, VKI_F_GETFL, 0)) < 0) {
      SET_STATUS_Failure( VKI_EBADF );
   } else {
      const struct vki_iovec *iov;
      PRE_MEM_READ( "vmsplice(iov)", ARG2, sizeof(struct vki_iovec) * ARG3 );
      for (iov = (struct vki_iovec *)ARG2;
           iov < (struct vki_iovec *)ARG2 + ARG3; iov++) 
      {
         if ((fdfl & VKI_O_ACCMODE) == VKI_O_RDONLY)
            PRE_MEM_WRITE( "vmsplice(iov[...])", (Addr)iov->iov_base, iov->iov_len );
         else
            PRE_MEM_READ( "vmsplice(iov[...])", (Addr)iov->iov_base, iov->iov_len );
      }
   }
}

POST(sys_vmsplice)
{
   vg_assert(SUCCESS);
   if (RES > 0) {
      Int fdfl = VG_(fcntl)(ARG1, VKI_F_GETFL, 0);
      vg_assert(fdfl >= 0);
      if ((fdfl & VKI_O_ACCMODE) == VKI_O_RDONLY)
      {
         const struct vki_iovec *iov;
         for (iov = (struct vki_iovec *)ARG2;
              iov < (struct vki_iovec *)ARG2 + ARG3; iov++) 
         {
            POST_MEM_WRITE( (Addr)iov->iov_base, iov->iov_len );
         }
      }
   }
}

/* ---------------------------------------------------------------------
   oprofile-related wrappers
   ------------------------------------------------------------------ */

#if defined(VGP_x86_linux)
PRE(sys_lookup_dcookie)
{
   PRINT("sys_lookup_dcookie (0x%llx, %#lx, %lu)",
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

#if defined(VGP_amd64_linux) || defined(VGP_s390x_linux)        \
      || defined(VGP_tilegx_linux)
PRE(sys_lookup_dcookie)
{
   *flags |= SfMayBlock;
   PRINT("sys_lookup_dcookie ( %lu, %#lx, %lu )", ARG1, ARG2, ARG3);
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
   case VKI_F_GETPIPE_SZ:
      PRINT("sys_fcntl ( %lu, %lu )", ARG1, ARG2);
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
   case VKI_F_SETPIPE_SZ:
      PRINT("sys_fcntl[ARG3=='arg'] ( %lu, %lu, %lu )", ARG1, ARG2, ARG3);
      PRE_REG_READ3(long, "fcntl",
                    unsigned int, fd, unsigned int, cmd, unsigned long, arg);
      break;

   // These ones use ARG3 as "lock".
   case VKI_F_GETLK:
   case VKI_F_SETLK:
   case VKI_F_SETLKW:
#  if defined(VGP_x86_linux) || defined(VGP_mips64_linux)
   case VKI_F_GETLK64:
   case VKI_F_SETLK64:
   case VKI_F_SETLKW64:
#  endif
   case VKI_F_OFD_GETLK:
   case VKI_F_OFD_SETLK:
   case VKI_F_OFD_SETLKW:
      PRINT("sys_fcntl[ARG3=='lock'] ( %lu, %lu, %#lx )", ARG1, ARG2, ARG3);
      PRE_REG_READ3(long, "fcntl",
                    unsigned int, fd, unsigned int, cmd,
                    struct flock64 *, lock);
      break;

   case VKI_F_SETOWN_EX:
      PRINT("sys_fcntl[F_SETOWN_EX] ( %lu, %lu, %lu )", ARG1, ARG2, ARG3);
      PRE_REG_READ3(long, "fcntl",
                    unsigned int, fd, unsigned int, cmd,
                    struct vki_f_owner_ex *, arg);
      PRE_MEM_READ("fcntl(F_SETOWN_EX)", ARG3, sizeof(struct vki_f_owner_ex));
      break;

   case VKI_F_GETOWN_EX:
      PRINT("sys_fcntl[F_GETOWN_EX] ( %lu, %lu, %lu )", ARG1, ARG2, ARG3);
      PRE_REG_READ3(long, "fcntl",
                    unsigned int, fd, unsigned int, cmd,
                    struct vki_f_owner_ex *, arg);
      PRE_MEM_WRITE("fcntl(F_GETOWN_EX)", ARG3, sizeof(struct vki_f_owner_ex));
      break;

   default:
      PRINT("sys_fcntl[UNKNOWN] ( %lu, %lu, %lu )", ARG1, ARG2, ARG3);
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
   } else if (ARG2 == VKI_F_GETOWN_EX) {
      POST_MEM_WRITE(ARG3, sizeof(struct vki_f_owner_ex));
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
      PRINT("sys_fcntl64 ( %lu, %lu )", ARG1, ARG2);
      PRE_REG_READ2(long, "fcntl64", unsigned int, fd, unsigned int, cmd);
      break;

   // These ones use ARG3 as "arg".
   case VKI_F_DUPFD:
   case VKI_F_DUPFD_CLOEXEC:
   case VKI_F_SETFD:
   case VKI_F_SETFL:
   case VKI_F_SETLEASE:
   case VKI_F_NOTIFY:
      PRINT("sys_fcntl64[ARG3=='arg'] ( %lu, %lu, %lu )", ARG1, ARG2, ARG3);
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
   case VKI_F_OFD_GETLK:
   case VKI_F_OFD_SETLK:
   case VKI_F_OFD_SETLKW:
      PRINT("sys_fcntl64[ARG3=='lock'] ( %lu, %lu, %#lx )", ARG1, ARG2, ARG3);
      PRE_REG_READ3(long, "fcntl64",
                    unsigned int, fd, unsigned int, cmd,
                    struct flock64 *, lock);
      break;

   case VKI_F_SETOWN_EX:
      PRINT("sys_fcntl[F_SETOWN_EX] ( %lu, %lu, %lu )", ARG1, ARG2, ARG3);
      PRE_REG_READ3(long, "fcntl",
                    unsigned int, fd, unsigned int, cmd,
                    struct vki_f_owner_ex *, arg);
      PRE_MEM_READ("fcntl(F_SETOWN_EX)", ARG3, sizeof(struct vki_f_owner_ex));
      break;

   case VKI_F_GETOWN_EX:
      PRINT("sys_fcntl[F_GETOWN_EX] ( %lu, %lu, %lu )", ARG1, ARG2, ARG3);
      PRE_REG_READ3(long, "fcntl",
                    unsigned int, fd, unsigned int, cmd,
                    struct vki_f_owner_ex *, arg);
      PRE_MEM_WRITE("fcntl(F_GETOWN_EX)", ARG3, sizeof(struct vki_f_owner_ex));
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
   } else if (ARG2 == VKI_F_GETOWN_EX) {
      POST_MEM_WRITE(ARG3, sizeof(struct vki_f_owner_ex));
   }
}

/* ---------------------------------------------------------------------
   ioctl wrappers
   ------------------------------------------------------------------ */

PRE(sys_ioctl)
{
   *flags |= SfMayBlock;

   ARG2 = (UInt)ARG2;

   // We first handle the ones that don't use ARG3 (even as a
   // scalar/non-pointer argument).
   switch (ARG2 /* request */) {

      /* asm-generic/ioctls.h */
   case VKI_FIOCLEX:
   case VKI_FIONCLEX:
   case VKI_TIOCNOTTY:

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

      /* SCSI no operand */
   case VKI_SCSI_IOCTL_DOORLOCK:
   case VKI_SCSI_IOCTL_DOORUNLOCK:
   
   /* CDROM stuff. */
   case VKI_CDROM_DISC_STATUS:

   /* KVM ioctls that dont check for a numeric value as parameter */
   case VKI_KVM_S390_ENABLE_SIE:
   case VKI_KVM_CREATE_IRQCHIP:
   case VKI_KVM_S390_INITIAL_RESET:
   case VKI_KVM_KVMCLOCK_CTRL:

   /* vhost without parameter */
   case VKI_VHOST_SET_OWNER:
   case VKI_VHOST_RESET_OWNER:

   /* User input device creation */
   case VKI_UI_DEV_CREATE:
   case VKI_UI_DEV_DESTROY:

   /* InfiniBand */
   case VKI_IB_USER_MAD_ENABLE_PKEY:

   /* Lustre */
   case VKI_LL_IOC_GROUP_LOCK:
   case VKI_LL_IOC_GROUP_UNLOCK:

   /* V4L2 */
   case VKI_V4L2_LOG_STATUS:

   /* DVB */
   case VKI_DMX_STOP:
      PRINT("sys_ioctl ( %lu, 0x%lx )", ARG1, ARG2);
      PRE_REG_READ2(long, "ioctl",
                    unsigned int, fd, unsigned int, request);
      return;

   default:
      PRINT("sys_ioctl ( %lu, 0x%lx, 0x%lx )", ARG1, ARG2, ARG3);
      PRE_REG_READ3(long, "ioctl",
                    unsigned int, fd, unsigned int, request, unsigned long, arg);
      break;
   }

   // We now handle those that do look at ARG3 (and unknown ones fall into
   // this category).  Nb: some of these may well belong in the
   // doesn't-use-ARG3 switch above.
   switch (ARG2 /* request */) {

   case VKI_ION_IOC_ALLOC: {
      struct vki_ion_allocation_data* data
         = (struct vki_ion_allocation_data*)ARG3;
      PRE_FIELD_READ ("ioctl(ION_IOC_ALLOC).len",          data->len);
      PRE_FIELD_READ ("ioctl(ION_IOC_ALLOC).align",        data->align);
      PRE_FIELD_READ ("ioctl(ION_IOC_ALLOC).heap_id_mask", data->heap_id_mask);
      PRE_FIELD_READ ("ioctl(ION_IOC_ALLOC).flags",        data->flags);
      PRE_FIELD_WRITE("ioctl(ION_IOC_ALLOC).handle",       data->handle);
      break;
   }
   case VKI_ION_IOC_MAP: {
      struct vki_ion_fd_data* data = (struct vki_ion_fd_data*)ARG3;
      PRE_FIELD_READ ("ioctl(ION_IOC_MAP).handle", data->handle);
      PRE_FIELD_WRITE("ioctl(ION_IOC_MAP).fd",     data->fd);
      break;
   }
   case VKI_ION_IOC_IMPORT: {
      struct vki_ion_fd_data* data = (struct vki_ion_fd_data*)ARG3;
      PRE_FIELD_READ ("ioctl(ION_IOC_IMPORT).fd",     data->fd);
      PRE_FIELD_WRITE("ioctl(ION_IOC_IMPORT).handle", data->handle);
      break;
   }

   case VKI_SYNC_IOC_MERGE: {
      struct vki_sync_merge_data* data = (struct vki_sync_merge_data*)ARG3;
      PRE_FIELD_READ ("ioctl(SYNC_IOC_MERGE).fd2",   data->fd2);
      PRE_MEM_RASCIIZ("ioctl(SYNC_IOC_MERGE).name",  (Addr)(&data->name[0]));
      PRE_FIELD_WRITE("ioctl(SYNC_IOC_MERGE).fence", data->fence);
      break;
   }

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
   case VKI_TIOCSIG:
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
   case VKI_FIOQSIZE:
      PRE_MEM_WRITE( "ioctl(FIOQSIZE)",  ARG3, sizeof(vki_loff_t) );
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
      PRE_MEM_READ( "ioctl(SG_IO)", ARG3, sizeof(vki_sg_io_hdr_t) );
      {
         vki_sg_io_hdr_t *sgio = (vki_sg_io_hdr_t*)ARG3;
         PRE_MEM_READ( "ioctl(SG_IO)", (Addr)sgio->cmdp, sgio->cmd_len );
         if ( sgio->dxfer_direction == VKI_SG_DXFER_TO_DEV ||
              sgio->dxfer_direction == VKI_SG_DXFER_TO_FROM_DEV ) {
            PRE_MEM_READ( "ioctl(SG_IO)", (Addr)sgio->dxferp, sgio->dxfer_len );
         }
      }
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
   case VKI_SIOCETHTOOL: {       /* ethtool(8) interface         */
      struct vki_ifreq *ir = (struct vki_ifreq *)ARG3;
      PRE_MEM_READ( "ioctl(SIOCETHTOOL)", (Addr)ir, sizeof(struct vki_ifreq) );
      PRE_MEM_RASCIIZ( "ioctl(SIOCETHTOOL)", (Addr)ir->vki_ifr_name );
      PRE_MEM_READ( "ioctl(SIOCETHTOOL)", (Addr)ir->vki_ifr_data, sizeof(vki_u32) );
      PRINT("SIOCETHTOOL( 0x%x )", *(vki_u32 *)ir->vki_ifr_data );
      switch ( *(vki_u32 *)ir->vki_ifr_data ) {
      case VKI_ETHTOOL_GSET:
         PRE_MEM_WRITE( "ioctl(SIOCETHTOOL,GSET)",
                        (Addr)ir->vki_ifr_data, sizeof(struct vki_ethtool_cmd) );
         break;
      case VKI_ETHTOOL_SSET:
         PRE_MEM_READ( "ioctl(SIOCETHTOOL,SSET)",
                       (Addr)ir->vki_ifr_data, sizeof(struct vki_ethtool_cmd) );
         break;
      case VKI_ETHTOOL_GDRVINFO:
         PRE_MEM_WRITE( "ioctl(SIOCETHTOOL,GDRVINFO)",
                        (Addr)ir->vki_ifr_data, sizeof(struct vki_ethtool_drvinfo) );
         break;
      case VKI_ETHTOOL_GREGS:
         PRE_MEM_READ( "ioctl(SIOCETHTOOL,GREGS)",
                       (Addr)ir->vki_ifr_data, sizeof(struct vki_ethtool_regs) );
         PRE_MEM_WRITE( "ioctl(SIOCETHTOOL,GREGS)",
                        (Addr)((struct vki_ethtool_regs *)ir->vki_ifr_data)->data,
                        ((struct vki_ethtool_regs *)ir->vki_ifr_data)->len );
         break;
      case VKI_ETHTOOL_GWOL:
         PRE_MEM_WRITE( "ioctl(SIOCETHTOOL,GWOL)",
                        (Addr)ir->vki_ifr_data, sizeof(struct vki_ethtool_wolinfo) );
         break;
      case VKI_ETHTOOL_SWOL:
         PRE_MEM_READ( "ioctl(SIOCETHTOOL,SWOL)",
                       (Addr)ir->vki_ifr_data, sizeof(struct vki_ethtool_wolinfo) );
         break;
      case VKI_ETHTOOL_GMSGLVL:
      case VKI_ETHTOOL_GLINK:
      case VKI_ETHTOOL_GRXCSUM:
      case VKI_ETHTOOL_GSG:
      case VKI_ETHTOOL_GTSO:
      case VKI_ETHTOOL_GUFO:
      case VKI_ETHTOOL_GGSO:
      case VKI_ETHTOOL_GFLAGS:
      case VKI_ETHTOOL_GGRO:
         PRE_MEM_WRITE( "ioctl(SIOCETHTOOL,Gvalue)",
                        (Addr)ir->vki_ifr_data, sizeof(struct vki_ethtool_value) );
         break;
      case VKI_ETHTOOL_SMSGLVL:
      case VKI_ETHTOOL_SRXCSUM:
      case VKI_ETHTOOL_SSG:
      case VKI_ETHTOOL_STSO:
      case VKI_ETHTOOL_SUFO:
      case VKI_ETHTOOL_SGSO:
      case VKI_ETHTOOL_SFLAGS:
      case VKI_ETHTOOL_SGRO:
         PRE_MEM_READ( "ioctl(SIOCETHTOOL,Svalue)",
                       (Addr)ir->vki_ifr_data, sizeof(struct vki_ethtool_value) );
         break;
      case VKI_ETHTOOL_NWAY_RST:
         break;
      case VKI_ETHTOOL_GRINGPARAM:
         PRE_MEM_WRITE( "ioctl(SIOCETHTOOL,GRINGPARAM)",
                        (Addr)ir->vki_ifr_data, sizeof(struct vki_ethtool_ringparam) );
         break;
      case VKI_ETHTOOL_SRINGPARAM:
         PRE_MEM_READ( "ioctl(SIOCETHTOOL,SRINGPARAM)",
                       (Addr)ir->vki_ifr_data, sizeof(struct vki_ethtool_ringparam) );
         break;
      case VKI_ETHTOOL_TEST:
         PRE_MEM_READ( "ioctl(SIOCETHTOOL,TEST)",
                       (Addr)ir->vki_ifr_data, sizeof(struct vki_ethtool_test) );
         PRE_MEM_WRITE( "ioctl(SIOCETHTOOL,TEST)",
                        (Addr)((struct vki_ethtool_test *)ir->vki_ifr_data)->data,
                        ((struct vki_ethtool_test *)ir->vki_ifr_data)->len * sizeof(__vki_u64) );
         break;
      case VKI_ETHTOOL_PHYS_ID:
         break;
      case VKI_ETHTOOL_GPERMADDR:
         PRE_MEM_READ( "ioctl(SIOCETHTOOL,GPERMADDR)",
                       (Addr)ir->vki_ifr_data, sizeof(struct vki_ethtool_perm_addr) );
         PRE_MEM_WRITE( "ioctl(SIOCETHTOOL,GPERMADDR)",
                        (Addr)((struct vki_ethtool_perm_addr *)ir->vki_ifr_data)->data,
                        ((struct vki_ethtool_perm_addr *)ir->vki_ifr_data)->size );
         break;
      case VKI_ETHTOOL_RESET:
         break;
      case VKI_ETHTOOL_GSSET_INFO:
         PRE_MEM_READ( "ioctl(SIOCETHTOOL,GSSET_INFO)",
                       (Addr)ir->vki_ifr_data, sizeof(struct vki_ethtool_sset_info) );
         PRE_MEM_WRITE( "ioctl(SIOCETHTOOL,GSSET_INFO)",
                        (Addr)((struct vki_ethtool_sset_info *)ir->vki_ifr_data)->data,
                        __builtin_popcountll(((struct vki_ethtool_sset_info *)ir->vki_ifr_data)->sset_mask) * sizeof(__vki_u32) );
         break;
      case VKI_ETHTOOL_GFEATURES:
         PRE_MEM_READ( "ioctl(SIOCETHTOOL,GFEATURES)",
                       (Addr)ir->vki_ifr_data, sizeof(struct vki_ethtool_gfeatures) );
         PRE_MEM_WRITE( "ioctl(SIOCETHTOOL,GFEATURES)",
                        (Addr)((struct vki_ethtool_gfeatures *)ir->vki_ifr_data)->features,
                        ((struct vki_ethtool_gfeatures *)ir->vki_ifr_data)->size * sizeof(struct vki_ethtool_get_features_block) );
         break;
      case VKI_ETHTOOL_SFEATURES:
         PRE_MEM_READ( "ioctl(SIOCETHTOOL,SFEATURES)",
                       (Addr)ir->vki_ifr_data, sizeof(struct vki_ethtool_sfeatures) );
         PRE_MEM_READ( "ioctl(SIOCETHTOOL,SFEATURES)",
                       (Addr)((struct vki_ethtool_sfeatures *)ir->vki_ifr_data)->features,
                       ((struct vki_ethtool_sfeatures *)ir->vki_ifr_data)->size * sizeof(struct vki_ethtool_set_features_block) );
         break;
      case VKI_ETHTOOL_GCHANNELS:
         PRE_MEM_WRITE( "ioctl(SIOCETHTOOL,GCHANNELS)",
                        (Addr)ir->vki_ifr_data, sizeof(struct vki_ethtool_channels) );
         break;
      case VKI_ETHTOOL_SCHANNELS:
         PRE_MEM_READ( "ioctl(SIOCETHTOOL,SCHANNELS)",
                       (Addr)ir->vki_ifr_data, sizeof(struct vki_ethtool_channels) );
         break;
      case VKI_ETHTOOL_GET_TS_INFO:
         PRE_MEM_WRITE( "ioctl(SIOCETHTOOL,GET_TS_INFO)",
                        (Addr)ir->vki_ifr_data, sizeof(struct vki_ethtool_ts_info) );
         break;
      }
      break;
   }
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
   case VKI_SIOCSHWTSTAMP:       /* Set hardware time stamping   */
      PRE_MEM_RASCIIZ( "ioctl(SIOCSHWTSTAMP)",
                     (Addr)((struct vki_ifreq *)ARG3)->vki_ifr_name );
      PRE_MEM_READ( "ioctl(SIOCSHWTSTAMP)",
                     (Addr)((struct vki_ifreq *)ARG3)->vki_ifr_data,
                     sizeof(struct vki_hwtstamp_config) );
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

      /* tun/tap related ioctls */
   case VKI_TUNSETNOCSUM:
   case VKI_TUNSETDEBUG:
      break;
   case VKI_TUNSETIFF:
      PRE_MEM_RASCIIZ( "ioctl(TUNSETIFF)",
                     (Addr)((struct vki_ifreq *)ARG3)->vki_ifr_name );
      PRE_MEM_READ( "ioctl(TUNSETIFF)",
                     (Addr)&((struct vki_ifreq *)ARG3)->vki_ifr_flags,
                     sizeof(((struct vki_ifreq *)ARG3)->vki_ifr_flags) );
      PRE_MEM_WRITE( "ioctl(TUNSETIFF)", ARG3, sizeof(struct vki_ifreq) );
      break;
   case VKI_TUNSETPERSIST:
   case VKI_TUNSETOWNER:
   case VKI_TUNSETLINK:
   case VKI_TUNSETGROUP:
      break;
   case VKI_TUNGETFEATURES:
      PRE_MEM_WRITE( "ioctl(TUNGETFEATURES)", ARG3, sizeof(unsigned int) );
      break;
   case VKI_TUNSETOFFLOAD:
      break;
   case VKI_TUNGETIFF:
      PRE_MEM_WRITE( "ioctl(TUNGETIFF)", ARG3, sizeof(struct vki_ifreq) );
      break;
   case VKI_TUNGETSNDBUF:
      PRE_MEM_WRITE( "ioctl(TUNGETSNDBUF)", ARG3, sizeof(int) );
      break;
   case VKI_TUNSETSNDBUF:
      PRE_MEM_READ( "ioctl(TUNSETSNDBUF)", ARG3, sizeof(int) );
      break;
   case VKI_TUNGETVNETHDRSZ:
      PRE_MEM_WRITE( "ioctl(TUNGETVNETHDRSZ)", ARG3, sizeof(int) );
      break;
   case VKI_TUNSETVNETHDRSZ:
      PRE_MEM_READ( "ioctl(TUNSETVNETHDRSZ)", ARG3, sizeof(int) );
      break;
   case VKI_TUNSETQUEUE:
      PRE_MEM_READ( "ioctl(TUNSETQUEUE)",
                     (Addr)&((struct vki_ifreq *)ARG3)->vki_ifr_flags,
                     sizeof(((struct vki_ifreq *)ARG3)->vki_ifr_flags) );
      break;
   case VKI_TUNSETIFINDEX:
      PRE_MEM_READ( "ioctl(TUNSETIFINDEX)", ARG3, sizeof(unsigned int));
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

    case VKI_SIOCATMARK:
      PRE_MEM_READ( "ioctl(SIOCATMARK)", ARG3, sizeof(int) );
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
   case VKI_SNDRV_CTL_IOCTL_PVERSION:
      PRE_MEM_WRITE( "ioctl(SNDRV_CTL_IOCTL_PVERSION)", (Addr)ARG3, sizeof(int) );
      break;
   case VKI_SNDRV_CTL_IOCTL_CARD_INFO:
      PRE_MEM_WRITE( "ioctl(SNDRV_CTL_IOCTL_CARD_INFO)", (Addr)ARG3, sizeof(struct vki_snd_ctl_card_info) );
      break;
   case VKI_SNDRV_CTL_IOCTL_ELEM_LIST: {
      struct vki_snd_ctl_elem_list *data = (struct vki_snd_ctl_elem_list *)ARG3;
      PRE_MEM_READ( "ioctl(SNDRV_CTL_IOCTL_ELEM_LIST)", (Addr)&data->offset, sizeof(data->offset) );
      PRE_MEM_READ( "ioctl(SNDRV_CTL_IOCTL_ELEM_LIST)", (Addr)&data->space, sizeof(data->space) );
      PRE_MEM_WRITE( "ioctl(SNDRV_CTL_IOCTL_ELEM_LIST)", (Addr)&data->used, sizeof(data->used) );
      PRE_MEM_WRITE( "ioctl(SNDRV_CTL_IOCTL_ELEM_LIST)", (Addr)&data->count, sizeof(data->count) );
      PRE_MEM_READ( "ioctl(SNDRV_CTL_IOCTL_ELEM_LIST)", (Addr)&data->pids, sizeof(data->pids) );
      if (data->pids) {
         PRE_MEM_WRITE( "ioctl(SNDRV_CTL_IOCTL_ELEM_LIST)", (Addr)data->pids, sizeof(struct vki_snd_ctl_elem_id) * data->space );
      }
      break;
   }
   case VKI_SNDRV_CTL_IOCTL_TLV_READ: {
      struct vki_snd_ctl_tlv *data = (struct vki_snd_ctl_tlv *)ARG3;
      PRE_MEM_READ( "ioctl(SNDRV_CTL_IOCTL_TLV_READ)", (Addr)&data->numid, sizeof(data->numid) );
      PRE_MEM_READ( "ioctl(SNDRV_CTL_IOCTL_TLV_READ)", (Addr)&data->length, sizeof(data->length) );
      PRE_MEM_WRITE( "ioctl(SNDRV_CTL_IOCTL_TLV_READ)", (Addr)data->tlv, data->length );
      break;
   }
   case VKI_SNDRV_CTL_IOCTL_TLV_WRITE:
   case VKI_SNDRV_CTL_IOCTL_TLV_COMMAND: {
      struct vki_snd_ctl_tlv *data = (struct vki_snd_ctl_tlv *)ARG3;
      PRE_MEM_READ( "ioctl(SNDRV_CTL_IOCTL_TLV_WRITE)", (Addr)&data->numid, sizeof(data->numid) );
      PRE_MEM_READ( "ioctl(SNDRV_CTL_IOCTL_TLV_WRITE)", (Addr)&data->length, sizeof(data->length) );
      PRE_MEM_READ( "ioctl(SNDRV_CTL_IOCTL_TLV_WRITE)", (Addr)data->tlv, data->length );
      break;
   }

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
   case VKI_BLKPBSZGET:
      PRE_MEM_WRITE( "ioctl(BLKPBSZGET)", ARG3, sizeof(int));
      break;
   case VKI_BLKDISCARDZEROES:
      PRE_MEM_WRITE( "ioctl(BLKDISCARDZEROES)", ARG3, sizeof(vki_uint));
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

      /* SCSI */
   case VKI_SCSI_IOCTL_GET_IDLUN: /* 0x5382 */
      PRE_MEM_WRITE( "ioctl(SCSI_IOCTL_GET_IDLUN)", ARG3, sizeof(struct vki_scsi_idlun));
      break;
   case VKI_SCSI_IOCTL_GET_BUS_NUMBER: /* 0x5386 */
      PRE_MEM_WRITE( "ioctl(SCSI_IOCTL_GET_BUS_NUMBER)", ARG3, sizeof(int));
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
   case VKI_CDROM_GET_CAPABILITY: /* 0x5331 */
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
   case VKI_FBIOPUT_VSCREENINFO:
      PRE_MEM_READ( "ioctl(FBIOPUT_VSCREENINFO)", ARG3,
                    sizeof(struct vki_fb_var_screeninfo));
      break;
   case VKI_FBIOGET_FSCREENINFO: /* 0x4602 */
      PRE_MEM_WRITE( "ioctl(FBIOGET_FSCREENINFO)", ARG3,
                     sizeof(struct vki_fb_fix_screeninfo));
      break;
   case VKI_FBIOPAN_DISPLAY:
      PRE_MEM_READ( "ioctl(FBIOPAN_DISPLAY)", ARG3,
                    sizeof(struct vki_fb_var_screeninfo));

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
      }
      break;
   case VKI_USBDEVFS_GETDRIVER:
      if ( ARG3 ) {
         struct vki_usbdevfs_getdriver *vkugd = (struct vki_usbdevfs_getdriver *) ARG3;
         PRE_MEM_WRITE( "ioctl(USBDEVFS_GETDRIVER)", (Addr)&vkugd->driver, sizeof(vkugd->driver));
      }
      break;
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
      }
      break;
   case VKI_USBDEVFS_DISCARDURB:
      break;
   case VKI_USBDEVFS_REAPURB:
      if ( ARG3 ) {
         PRE_MEM_WRITE( "ioctl(USBDEVFS_REAPURB)", ARG3, sizeof(struct vki_usbdevfs_urb **));
      }
      break;
   case VKI_USBDEVFS_REAPURBNDELAY:
      if ( ARG3 ) {
         PRE_MEM_WRITE( "ioctl(USBDEVFS_REAPURBNDELAY)", ARG3, sizeof(struct vki_usbdevfs_urb **));
      }
      break;
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
   case VKI_I2C_RDWR:
      if ( ARG3 ) {
          struct vki_i2c_rdwr_ioctl_data *vkui = (struct vki_i2c_rdwr_ioctl_data *)ARG3;
          UInt i;
          PRE_MEM_READ("ioctl(I2C_RDWR)", (Addr)vkui, sizeof(struct vki_i2c_rdwr_ioctl_data));
          for (i=0; i < vkui->nmsgs; i++) {
              struct vki_i2c_msg *msg = vkui->msgs + i;
              PRE_MEM_READ("ioctl(I2C_RDWR).msgs", (Addr)msg, sizeof(struct vki_i2c_msg));
              if (msg->flags & VKI_I2C_M_RD) 
                  PRE_MEM_WRITE("ioctl(I2C_RDWR).msgs.buf", (Addr)msg->buf, msg->len);
              else
                  PRE_MEM_READ("ioctl(I2C_RDWR).msgs.buf", (Addr)msg->buf, msg->len);
          }
      }
      break;
   case VKI_I2C_SMBUS:
       if ( ARG3 ) {
            struct vki_i2c_smbus_ioctl_data *vkis
               = (struct vki_i2c_smbus_ioctl_data *) ARG3;
            PRE_MEM_READ("ioctl(VKI_I2C_SMBUS).i2c_smbus_ioctl_data.read_write",
                         (Addr)&vkis->read_write, sizeof(vkis->read_write));
            PRE_MEM_READ("ioctl(VKI_I2C_SMBUS).i2c_smbus_ioctl_data.size",
                         (Addr)&vkis->size, sizeof(vkis->size));
            PRE_MEM_READ("ioctl(VKI_I2C_SMBUS).i2c_smbus_ioctl_data.command",
                         (Addr)&vkis->command, sizeof(vkis->command));
            /* i2c_smbus_write_quick hides its value in read_write, so
               this variable can have a different meaning */
            /* to make matters worse i2c_smbus_write_byte stores its
               value in command */
            if ( ! ((vkis->size == VKI_I2C_SMBUS_QUICK) ||
                 ((vkis->size == VKI_I2C_SMBUS_BYTE)
                  && (vkis->read_write == VKI_I2C_SMBUS_WRITE))))  {
                    /* the rest uses the byte array to store the data,
                       some the first byte for size */
                    UInt size;
                    switch(vkis->size) {
                        case VKI_I2C_SMBUS_BYTE_DATA:
                            size = 1;
                            break;
                        case VKI_I2C_SMBUS_WORD_DATA:
                        case VKI_I2C_SMBUS_PROC_CALL:
                            size = 2;
                            break;
                        case VKI_I2C_SMBUS_BLOCK_DATA:
                        case VKI_I2C_SMBUS_I2C_BLOCK_BROKEN:
                        case VKI_I2C_SMBUS_BLOCK_PROC_CALL:
                        case VKI_I2C_SMBUS_I2C_BLOCK_DATA:
                            size = 1 + vkis->data->block[0];
                            break;
                        default:
                            size = 0;
                    }

                    if ((vkis->read_write == VKI_I2C_SMBUS_READ)
                        || (vkis->size == VKI_I2C_SMBUS_PROC_CALL)
                        || (vkis->size == VKI_I2C_SMBUS_BLOCK_PROC_CALL))
                        PRE_MEM_WRITE("ioctl(VKI_I2C_SMBUS)"
                                      ".i2c_smbus_ioctl_data.data",
                                      (Addr)&vkis->data->block[0], size);
                    else
                        PRE_MEM_READ("ioctl(VKI_I2C_SMBUS)."
                                     "i2c_smbus_ioctl_data.data",
                                     (Addr)&vkis->data->block[0], size);
            }
       }
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

  /* User input device creation */
  case VKI_UI_SET_EVBIT:
  case VKI_UI_SET_KEYBIT:
  case VKI_UI_SET_RELBIT:
  case VKI_UI_SET_ABSBIT:
  case VKI_UI_SET_MSCBIT:
  case VKI_UI_SET_LEDBIT:
  case VKI_UI_SET_SNDBIT:
  case VKI_UI_SET_FFBIT:
  case VKI_UI_SET_SWBIT:
  case VKI_UI_SET_PROPBIT:
      /* These just take an int by value */
      break;

#  if defined(VGPV_arm_linux_android) || defined(VGPV_x86_linux_android) \
      || defined(VGPV_mips32_linux_android)
   /* ashmem */
   case VKI_ASHMEM_GET_SIZE:
   case VKI_ASHMEM_SET_SIZE:
   case VKI_ASHMEM_GET_PROT_MASK:
   case VKI_ASHMEM_SET_PROT_MASK:
   case VKI_ASHMEM_GET_PIN_STATUS:
   case VKI_ASHMEM_PURGE_ALL_CACHES:
       break;
   case VKI_ASHMEM_GET_NAME:
       PRE_MEM_WRITE( "ioctl(ASHMEM_SET_NAME)", ARG3, VKI_ASHMEM_NAME_LEN );
       break;
   case VKI_ASHMEM_SET_NAME:
       PRE_MEM_RASCIIZ( "ioctl(ASHMEM_SET_NAME)", ARG3);
       break;
   case VKI_ASHMEM_PIN:
   case VKI_ASHMEM_UNPIN:
       PRE_MEM_READ( "ioctl(ASHMEM_PIN|ASHMEM_UNPIN)",
                     ARG3, sizeof(struct vki_ashmem_pin) );
       break;

   /* binder */
   case VKI_BINDER_WRITE_READ:
       if (ARG3) {
           struct vki_binder_write_read* bwr
              = (struct vki_binder_write_read*)ARG3;

           PRE_FIELD_READ("ioctl(BINDER_WRITE_READ).write_buffer",
                          bwr->write_buffer);
           PRE_FIELD_READ("ioctl(BINDER_WRITE_READ).write_size",
                          bwr->write_size);
           PRE_FIELD_READ("ioctl(BINDER_WRITE_READ).write_consumed",
                          bwr->write_consumed);
           PRE_FIELD_READ("ioctl(BINDER_WRITE_READ).read_buffer",
                          bwr->read_buffer);
           PRE_FIELD_READ("ioctl(BINDER_WRITE_READ).read_size",
                          bwr->read_size);
           PRE_FIELD_READ("ioctl(BINDER_WRITE_READ).read_consumed",
                          bwr->read_consumed);

           PRE_FIELD_WRITE("ioctl(BINDER_WRITE_READ).write_consumed",
                           bwr->write_consumed);
           PRE_FIELD_WRITE("ioctl(BINDER_WRITE_READ).read_consumed",
                           bwr->read_consumed);

           if (bwr->read_size)
               PRE_MEM_WRITE("ioctl(BINDER_WRITE_READ).read_buffer[]",
                             (Addr)bwr->read_buffer, bwr->read_size);
           if (bwr->write_size)
               PRE_MEM_READ("ioctl(BINDER_WRITE_READ).write_buffer[]",
                            (Addr)bwr->write_buffer, bwr->write_size);
       }
       break;

   case VKI_BINDER_SET_IDLE_TIMEOUT:
   case VKI_BINDER_SET_MAX_THREADS:
   case VKI_BINDER_SET_IDLE_PRIORITY:
   case VKI_BINDER_SET_CONTEXT_MGR:
   case VKI_BINDER_THREAD_EXIT:
       break;
   case VKI_BINDER_VERSION:
       if (ARG3) {
           struct vki_binder_version* bv = (struct vki_binder_version*)ARG3;
           PRE_FIELD_WRITE("ioctl(BINDER_VERSION)", bv->protocol_version);
       }
       break;
#  endif /* defined(VGPV_*_linux_android) */

   case VKI_HCIGETDEVLIST:
      if (ARG3) {
         struct vki_hci_dev_list_req* dlr = (struct vki_hci_dev_list_req*)ARG3;
         PRE_MEM_READ("ioctl(HCIGETDEVLIST)",
                      (Addr)ARG3, sizeof(struct vki_hci_dev_list_req));
         PRE_MEM_WRITE("ioctl(HCIGETDEVLIST)",
                       (Addr)ARG3 + sizeof(struct vki_hci_dev_list_req),
                       dlr->dev_num * sizeof(struct vki_hci_dev_req));
      }
      break;
      
   case VKI_HCIINQUIRY:
      if (ARG3) {
         struct vki_hci_inquiry_req* ir = (struct vki_hci_inquiry_req*)ARG3;
         PRE_MEM_READ("ioctl(HCIINQUIRY)",
                      (Addr)ARG3, sizeof(struct vki_hci_inquiry_req));
         PRE_MEM_WRITE("ioctl(HCIINQUIRY)",
                       (Addr)ARG3 + sizeof(struct vki_hci_inquiry_req),
                       ir->num_rsp * sizeof(struct vki_inquiry_info));
      }
      break;

   case VKI_DRM_IOCTL_VERSION:
      if (ARG3) {
         struct vki_drm_version *data = (struct vki_drm_version *)ARG3;
	 PRE_MEM_WRITE("ioctl(DRM_VERSION).version_major", (Addr)&data->version_major, sizeof(data->version_major));
         PRE_MEM_WRITE("ioctl(DRM_VERSION).version_minor", (Addr)&data->version_minor, sizeof(data->version_minor));
         PRE_MEM_WRITE("ioctl(DRM_VERSION).version_patchlevel", (Addr)&data->version_patchlevel, sizeof(data->version_patchlevel));
         PRE_MEM_READ("ioctl(DRM_VERSION).name_len", (Addr)&data->name_len, sizeof(data->name_len));
         PRE_MEM_READ("ioctl(DRM_VERSION).name", (Addr)&data->name, sizeof(data->name));
         PRE_MEM_WRITE("ioctl(DRM_VERSION).name", (Addr)data->name, data->name_len);
         PRE_MEM_READ("ioctl(DRM_VERSION).date_len", (Addr)&data->date_len, sizeof(data->date_len));
         PRE_MEM_READ("ioctl(DRM_VERSION).date", (Addr)&data->date, sizeof(data->date));
         PRE_MEM_WRITE("ioctl(DRM_VERSION).date", (Addr)data->date, data->date_len);
         PRE_MEM_READ("ioctl(DRM_VERSION).desc_len", (Addr)&data->desc_len, sizeof(data->desc_len));
         PRE_MEM_READ("ioctl(DRM_VERSION).desc", (Addr)&data->desc, sizeof(data->desc));
         PRE_MEM_WRITE("ioctl(DRM_VERSION).desc", (Addr)data->desc, data->desc_len);
      }
      break;
   case VKI_DRM_IOCTL_GET_UNIQUE:
      if (ARG3) {
         struct vki_drm_unique *data = (struct vki_drm_unique *)ARG3;
	 PRE_MEM_READ("ioctl(DRM_GET_UNIQUE).unique_len", (Addr)&data->unique_len, sizeof(data->unique_len));
	 PRE_MEM_READ("ioctl(DRM_GET_UNIQUE).unique", (Addr)&data->unique, sizeof(data->unique));
	 PRE_MEM_WRITE("ioctl(DRM_GET_UNIQUE).unique", (Addr)data->unique, data->unique_len);
      }
      break;
   case VKI_DRM_IOCTL_GET_MAGIC:
      if (ARG3) {
         struct vki_drm_auth *data = (struct vki_drm_auth *)ARG3;
         PRE_MEM_WRITE("ioctl(DRM_GET_MAGIC).magic", (Addr)&data->magic, sizeof(data->magic));
      }
      break;
   case VKI_DRM_IOCTL_WAIT_VBLANK:
      if (ARG3) {
         union vki_drm_wait_vblank *data = (union vki_drm_wait_vblank *)ARG3;
	 PRE_MEM_READ("ioctl(DRM_WAIT_VBLANK).request.type", (Addr)&data->request.type, sizeof(data->request.type));
	 PRE_MEM_READ("ioctl(DRM_WAIT_VBLANK).request.sequence", (Addr)&data->request.sequence, sizeof(data->request.sequence));
	 /* XXX: It seems request.signal isn't used */
         PRE_MEM_WRITE("ioctl(DRM_WAIT_VBLANK).reply", (Addr)&data->reply, sizeof(data->reply));
      }
      break;
   case VKI_DRM_IOCTL_GEM_CLOSE:
      if (ARG3) {
         struct vki_drm_gem_close *data = (struct vki_drm_gem_close *)ARG3;
	 PRE_MEM_READ("ioctl(DRM_GEM_CLOSE).handle", (Addr)&data->handle, sizeof(data->handle));
      }
      break;
   case VKI_DRM_IOCTL_GEM_FLINK:
      if (ARG3) {
         struct vki_drm_gem_flink *data = (struct vki_drm_gem_flink *)ARG3;
	 PRE_MEM_READ("ioctl(DRM_GEM_FLINK).handle", (Addr)&data->handle, sizeof(data->handle));
         PRE_MEM_WRITE("ioctl(DRM_GEM_FLINK).name", (Addr)&data->name, sizeof(data->name));
      }
      break;
   case VKI_DRM_IOCTL_GEM_OPEN:
      if (ARG3) {
         struct vki_drm_gem_open *data = (struct vki_drm_gem_open *)ARG3;
	 PRE_MEM_READ("ioctl(DRM_GEM_OPEN).name", (Addr)&data->name, sizeof(data->name));
	 PRE_MEM_WRITE("ioctl(DRM_GEM_OPEN).handle", (Addr)&data->handle, sizeof(data->handle));
	 PRE_MEM_WRITE("ioctl(DRM_GEM_OPEN).size", (Addr)&data->size, sizeof(data->size));
      }
      break;
   case VKI_DRM_IOCTL_I915_GETPARAM:
      if (ARG3) {
         vki_drm_i915_getparam_t *data = (vki_drm_i915_getparam_t *)ARG3;
	 PRE_MEM_READ("ioctl(DRM_I915_GETPARAM).param", (Addr)&data->param, sizeof(data->param));
	 PRE_MEM_WRITE("ioctl(DRM_I915_GETPARAM).value", (Addr)data->value, sizeof(int));
      }
      break;
   case VKI_DRM_IOCTL_I915_GEM_BUSY:
      if (ARG3) {
         struct vki_drm_i915_gem_busy *data = (struct vki_drm_i915_gem_busy *)ARG3;
	 PRE_MEM_READ("ioctl(DRM_I915_GEM_BUSY).handle", (Addr)&data->handle, sizeof(data->handle));
         PRE_MEM_WRITE("ioctl(DRM_I915_GEM_BUSY).busy", (Addr)&data->busy, sizeof(data->busy));
      }
      break;
   case VKI_DRM_IOCTL_I915_GEM_CREATE:
      if (ARG3) {
         struct vki_drm_i915_gem_create *data = (struct vki_drm_i915_gem_create *)ARG3;
	 PRE_MEM_READ("ioctl(DRM_I915_GEM_CREATE).size", (Addr)&data->size, sizeof(data->size));
	 PRE_MEM_WRITE("ioctl(DRM_I915_GEM_CREATE).handle", (Addr)&data->handle, sizeof(data->handle));
      }
      break;
   case VKI_DRM_IOCTL_I915_GEM_PREAD:
      if (ARG3) {
         struct vki_drm_i915_gem_pread *data = (struct vki_drm_i915_gem_pread *)ARG3;
	 PRE_MEM_READ("ioctl(DRM_I915_GEM_PREAD).handle", (Addr)&data->handle, sizeof(data->handle));
	 PRE_MEM_READ("ioctl(DRM_I915_GEM_PREAD).offset", (Addr)&data->offset, sizeof(data->offset));
	 PRE_MEM_READ("ioctl(DRM_I915_GEM_PREAD).size", (Addr)&data->size, sizeof(data->size));
	 PRE_MEM_READ("ioctl(DRM_I915_GEM_PREAD).data_ptr", (Addr)&data->data_ptr, sizeof(data->data_ptr));
	 PRE_MEM_WRITE("ioctl(DRM_I915_GEM_PREAD).data_ptr", (Addr)data->data_ptr, data->size);
      }
      break;
   case VKI_DRM_IOCTL_I915_GEM_PWRITE:
      if (ARG3) {
         struct vki_drm_i915_gem_pwrite *data = (struct vki_drm_i915_gem_pwrite *)ARG3;
	 PRE_MEM_READ("ioctl(DRM_I915_GEM_PWRITE).handle", (Addr)&data->handle, sizeof(data->handle));
	 PRE_MEM_READ("ioctl(DRM_I915_GEM_PWRITE).offset", (Addr)&data->offset, sizeof(data->offset));
	 PRE_MEM_READ("ioctl(DRM_I915_GEM_PWRITE).size", (Addr)&data->size, sizeof(data->size));
	 PRE_MEM_READ("ioctl(DRM_I915_GEM_PWRITE).data_ptr", (Addr)&data->data_ptr, sizeof(data->data_ptr));
	 /* PRE_MEM_READ("ioctl(DRM_I915_GEM_PWRITE).data_ptr", (Addr)data->data_ptr, data->size);
	  * NB: the buffer is allowed to contain any amount of uninitialized data (e.g.
	  * interleaved vertex attributes may have a wide stride with uninitialized data between
	  * consecutive vertices) */
      }
      break;
   case VKI_DRM_IOCTL_I915_GEM_MMAP_GTT:
      if (ARG3) {
         struct vki_drm_i915_gem_mmap_gtt *data = (struct vki_drm_i915_gem_mmap_gtt *)ARG3;
	 PRE_MEM_READ("ioctl(DRM_I915_GEM_MMAP_GTT).handle", (Addr)&data->handle, sizeof(data->handle));
         PRE_MEM_WRITE("ioctl(DRM_I915_GEM_MMAP_GTT).offset", (Addr)&data->offset, sizeof(data->offset));
      }
      break;
   case VKI_DRM_IOCTL_I915_GEM_SET_DOMAIN:
      if (ARG3) {
         struct vki_drm_i915_gem_set_domain *data = (struct vki_drm_i915_gem_set_domain *)ARG3;
	 PRE_MEM_READ("ioctl(DRM_I915_GEM_SET_DOMAIN).handle", (Addr)&data->handle, sizeof(data->handle));
	 PRE_MEM_READ("ioctl(DRM_I915_GEM_SET_DOMAIN).read_domains", (Addr)&data->read_domains, sizeof(data->read_domains));
	 PRE_MEM_READ("ioctl(DRM_I915_GEM_SET_DOMAIN).write_domain", (Addr)&data->write_domain, sizeof(data->write_domain));
      }
      break;
   case VKI_DRM_IOCTL_I915_GEM_SET_TILING:
      if (ARG3) {
         struct vki_drm_i915_gem_set_tiling *data = (struct vki_drm_i915_gem_set_tiling *)ARG3;
	 PRE_MEM_READ("ioctl(DRM_I915_GEM_SET_TILING).handle", (Addr)&data->handle, sizeof(data->handle));
         PRE_MEM_READ("ioctl(DRM_I915_GEM_SET_TILING).tiling_mode", (Addr)&data->tiling_mode, sizeof(data->tiling_mode));
         PRE_MEM_READ("ioctl(DRM_I915_GEM_SET_TILING).stride", (Addr)&data->stride, sizeof(data->stride));
         PRE_MEM_WRITE("ioctl(DRM_I915_GEM_SET_TILING).swizzle_mode", (Addr)&data->swizzle_mode, sizeof(data->swizzle_mode));
      }
      break;
   case VKI_DRM_IOCTL_I915_GEM_GET_TILING:
      if (ARG3) {
         struct vki_drm_i915_gem_get_tiling *data = (struct vki_drm_i915_gem_get_tiling *)ARG3;
	 PRE_MEM_READ("ioctl(DRM_I915_GEM_GET_TILING).handle", (Addr)&data->handle, sizeof(data->handle));
	 PRE_MEM_WRITE("ioctl(DRM_I915_GEM_GET_TILING).tiling_mode", (Addr)&data->tiling_mode, sizeof(data->tiling_mode));
         PRE_MEM_WRITE("ioctl(DRM_I915_GEM_GET_TILING).swizzle_mode", (Addr)&data->swizzle_mode, sizeof(data->swizzle_mode));
      }
      break;
   case VKI_DRM_IOCTL_I915_GEM_GET_APERTURE:
      if (ARG3) {
         struct vki_drm_i915_gem_get_aperture *data = (struct vki_drm_i915_gem_get_aperture *)ARG3;
         PRE_MEM_WRITE("ioctl(DRM_I915_GEM_GET_APERTURE).aper_size", (Addr)&data->aper_size, sizeof(data->aper_size));
         PRE_MEM_WRITE("ioctl(DRM_I915_GEM_GET_APERTURE).aper_available_size", (Addr)&data->aper_available_size, sizeof(data->aper_available_size));
      }
      break;

   /* KVM ioctls that check for a numeric value as parameter */
   case VKI_KVM_GET_API_VERSION:
   case VKI_KVM_CREATE_VM:
   case VKI_KVM_GET_VCPU_MMAP_SIZE:
   case VKI_KVM_CHECK_EXTENSION:
   case VKI_KVM_SET_TSS_ADDR:
   case VKI_KVM_CREATE_VCPU:
   case VKI_KVM_RUN:
      break;

   case VKI_KVM_S390_MEM_OP: {
      struct vki_kvm_s390_mem_op *args =
         (struct vki_kvm_s390_mem_op *)(ARG3);
      PRE_MEM_READ("ioctl(KVM_S390_MEM_OP)", ARG3,
                   sizeof(struct vki_kvm_s390_mem_op));
      if (args->flags & VKI_KVM_S390_MEMOP_F_CHECK_ONLY)
         break;
      if (args->op == VKI_KVM_S390_MEMOP_LOGICAL_READ)
         PRE_MEM_WRITE("ioctl(KVM_S390_MEM_OP).buf", (Addr)args->buf, args->size);
      if (args->op == VKI_KVM_S390_MEMOP_LOGICAL_WRITE)
         PRE_MEM_READ("ioctl(KVM_S390_MEM_OP).buf", (Addr)args->buf, args->size);
      }
      break;


#ifdef ENABLE_XEN
   case VKI_XEN_IOCTL_PRIVCMD_HYPERCALL: {
      SyscallArgs harrghs;
      struct vki_xen_privcmd_hypercall *args =
         (struct vki_xen_privcmd_hypercall *)(ARG3);

      if (!args)
         break;

      VG_(memset)(&harrghs, 0, sizeof(harrghs));
      harrghs.sysno = args->op;
      harrghs.arg1 = args->arg[0];
      harrghs.arg2 = args->arg[1];
      harrghs.arg3 = args->arg[2];
      harrghs.arg4 = args->arg[3];
      harrghs.arg5 = args->arg[4];
      harrghs.arg6 = harrghs.arg7 = harrghs.arg8 = 0;

      WRAPPER_PRE_NAME(xen, hypercall) (tid, layout, &harrghs, status, flags);

      /* HACK. arg8 is used to return the number of hypercall
       * arguments actually consumed! */
      PRE_MEM_READ("hypercall", ARG3, sizeof(args->op) +
                   ( sizeof(args->arg[0]) * harrghs.arg8 ) );

      break;
   }

   case VKI_XEN_IOCTL_PRIVCMD_MMAP: {
       struct vki_xen_privcmd_mmap *args =
           (struct vki_xen_privcmd_mmap *)(ARG3);
       PRE_MEM_READ("VKI_XEN_IOCTL_PRIVCMD_MMAP(num)",
                    (Addr)&args->num, sizeof(args->num));
       PRE_MEM_READ("VKI_XEN_IOCTL_PRIVCMD_MMAP(dom)",
                    (Addr)&args->dom, sizeof(args->dom));
       PRE_MEM_READ("VKI_XEN_IOCTL_PRIVCMD_MMAP(entry)",
                    (Addr)args->entry, sizeof(*(args->entry)) * args->num);
      break;
   }
   case VKI_XEN_IOCTL_PRIVCMD_MMAPBATCH: {
       struct vki_xen_privcmd_mmapbatch *args =
           (struct vki_xen_privcmd_mmapbatch *)(ARG3);
       PRE_MEM_READ("VKI_XEN_IOCTL_PRIVCMD_MMAPBATCH(num)",
                    (Addr)&args->num, sizeof(args->num));
       PRE_MEM_READ("VKI_XEN_IOCTL_PRIVCMD_MMAPBATCH(dom)",
                    (Addr)&args->dom, sizeof(args->dom));
       PRE_MEM_READ("VKI_XEN_IOCTL_PRIVCMD_MMAPBATCH(addr)",
                    (Addr)&args->addr, sizeof(args->addr));
       PRE_MEM_READ("VKI_XEN_IOCTL_PRIVCMD_MMAPBATCH(arr)",
                    (Addr)args->arr, sizeof(*(args->arr)) * args->num);
      break;
   }
   case VKI_XEN_IOCTL_PRIVCMD_MMAPBATCH_V2: {
       struct vki_xen_privcmd_mmapbatch_v2 *args =
           (struct vki_xen_privcmd_mmapbatch_v2 *)(ARG3);
       PRE_MEM_READ("VKI_XEN_IOCTL_PRIVCMD_MMAPBATCH_V2(num)",
                    (Addr)&args->num, sizeof(args->num));
       PRE_MEM_READ("VKI_XEN_IOCTL_PRIVCMD_MMAPBATCH_V2(dom)",
                    (Addr)&args->dom, sizeof(args->dom));
       PRE_MEM_READ("VKI_XEN_IOCTL_PRIVCMD_MMAPBATCH_V2(addr)",
                    (Addr)&args->addr, sizeof(args->addr));
       PRE_MEM_READ("VKI_XEN_IOCTL_PRIVCMD_MMAPBATCH_V2(arr)",
                    (Addr)args->arr, sizeof(*(args->arr)) * args->num);
      break;
   }

   case VKI_XEN_IOCTL_EVTCHN_BIND_VIRQ: {
         struct vki_xen_ioctl_evtchn_bind_virq *args =
            (struct vki_xen_ioctl_evtchn_bind_virq *)(ARG3);
         PRE_MEM_READ("VKI_XEN_IOCTL_EVTCHN_BIND_VIRQ(virq)",
                 (Addr)&args->virq, sizeof(args->virq));
      }
      break;
   case VKI_XEN_IOCTL_EVTCHN_BIND_INTERDOMAIN: {
         struct vki_xen_ioctl_evtchn_bind_interdomain *args =
            (struct vki_xen_ioctl_evtchn_bind_interdomain *)(ARG3);
         PRE_MEM_READ("VKI_XEN_IOCTL_EVTCHN_BIND_INTERDOMAIN(remote_domain)",
                 (Addr)&args->remote_domain, sizeof(args->remote_domain));
         PRE_MEM_READ("VKI_XEN_IOCTL_EVTCHN_BIND_INTERDOMAIN(remote_port)",
                 (Addr)&args->remote_port, sizeof(args->remote_port));
      }
      break;
   case VKI_XEN_IOCTL_EVTCHN_BIND_UNBOUND_PORT: {
         struct vki_xen_ioctl_evtchn_bind_unbound_port *args =
            (struct vki_xen_ioctl_evtchn_bind_unbound_port *)(ARG3);
         PRE_MEM_READ("VKI_XEN_IOCTL_EVTCHN_BIND_UNBOUND_PORT(remote_domain)",
                 (Addr)&args->remote_domain, sizeof(args->remote_domain));
      }
      break;
   case VKI_XEN_IOCTL_EVTCHN_UNBIND: {
         struct vki_xen_ioctl_evtchn_unbind *args =
            (struct vki_xen_ioctl_evtchn_unbind *)(ARG3);
         PRE_MEM_READ("VKI_XEN_IOCTL_EVTCHN_UNBIND(port)",
                 (Addr)&args->port, sizeof(args->port));
      }
      break;
   case VKI_XEN_IOCTL_EVTCHN_NOTIFY: {
         struct vki_xen_ioctl_evtchn_notify *args =
            (struct vki_xen_ioctl_evtchn_notify*)(ARG3);
         PRE_MEM_READ("VKI_XEN_IOCTL_EVTCHN_notify(port)",
                 (Addr)&args->port, sizeof(args->port));
      }
      break;
   case VKI_XEN_IOCTL_EVTCHN_RESET:
      /* No input*/
      break;
#endif

   /* Lustre */
   case VKI_OBD_IOC_FID2PATH: {
      struct vki_getinfo_fid2path *gf = (struct vki_getinfo_fid2path *)ARG3;
      PRE_MEM_READ("VKI_OBD_IOC_FID2PATH(args)", ARG3, sizeof(struct vki_getinfo_fid2path));
      PRE_FIELD_WRITE("VKI_OBD_IOC_FID2PATH(args).gf_recno", gf->gf_recno);
      PRE_FIELD_WRITE("VKI_OBD_IOC_FID2PATH(args).gf_linkno", gf->gf_linkno);
      PRE_MEM_WRITE("VKI_OBD_IOC_FID2PATH(args)", (Addr)gf->gf_path, gf->gf_pathlen);
      break;
   }

   case VKI_LL_IOC_PATH2FID:
      PRE_MEM_WRITE("ioctl(VKI_LL_IOC_PATH2FID)", ARG3, sizeof(struct vki_lu_fid));
      break;

   case VKI_LL_IOC_GETPARENT: {
      struct vki_getparent *gp = (struct vki_getparent *)ARG3;
      PRE_FIELD_READ("ioctl(VKI_LL_IOC_GETPARENT).gp_linkno", gp->gp_linkno);
      PRE_FIELD_READ("ioctl(VKI_LL_IOC_GETPARENT).gp_name_size", gp->gp_name_size);
      PRE_FIELD_WRITE("ioctl(VKI_LL_IOC_GETPARENT).gp_fid", gp->gp_fid);
      PRE_MEM_WRITE("ioctl(VKI_LL_IOC_GETPARENT).gp_name", (Addr)gp->gp_name, gp->gp_name_size);
      break;
   }

   /* V4L2 */
   case VKI_V4L2_QUERYCAP: {
      struct vki_v4l2_capability *data = (struct vki_v4l2_capability *)ARG3;
      PRE_MEM_WRITE("ioctl(VKI_V4L2_QUERYCAP)", (Addr)data, sizeof(*data));
      break;
   }
   case VKI_V4L2_ENUM_FMT: {
      struct vki_v4l2_fmtdesc *data = (struct vki_v4l2_fmtdesc *)ARG3;
      PRE_FIELD_READ("ioctl(VKI_V4L2_ENUM_FMT).index", data->index);
      PRE_FIELD_READ("ioctl(VKI_V4L2_ENUM_FMT).type", data->type);
      PRE_FIELD_WRITE("ioctl(VKI_V4L2_ENUM_FMT).flags", data->flags);
      PRE_FIELD_WRITE("ioctl(VKI_V4L2_ENUM_FMT).description", data->description);
      PRE_FIELD_WRITE("ioctl(VKI_V4L2_ENUM_FMT).pixelformat", data->pixelformat);
      PRE_FIELD_WRITE("ioctl(VKI_V4L2_ENUM_FMT).reserved", data->reserved);
      break;
   }
   case VKI_V4L2_G_FMT: {
      struct vki_v4l2_format *data = (struct vki_v4l2_format *)ARG3;
      PRE_FIELD_READ("ioctl(VKI_V4L2_G_FMT).type", data->type);
      switch (data->type) {
      case VKI_V4L2_BUF_TYPE_VIDEO_CAPTURE:
      case VKI_V4L2_BUF_TYPE_VIDEO_OUTPUT:
         PRE_FIELD_READ("ioctl(VKI_V4L2_G_FMT).fmt.pix.priv", data->fmt.pix.priv);
         PRE_FIELD_WRITE("ioctl(VKI_V4L2_G_FMT).fmt.pix", data->fmt.pix);
         PRE_MEM_READ("ioctl(VKI_V4L2_G_FMT)",
               (Addr)&data->type + sizeof(data->type) + sizeof(data->fmt.pix),
               sizeof(*data) - sizeof(data->type) - sizeof(data->fmt.pix));
         break;
      case VKI_V4L2_BUF_TYPE_VBI_CAPTURE:
      case VKI_V4L2_BUF_TYPE_VBI_OUTPUT:
         PRE_FIELD_WRITE("ioctl(VKI_V4L2_G_FMT).fmt.vbi", data->fmt.vbi);
         break;
      case VKI_V4L2_BUF_TYPE_SLICED_VBI_CAPTURE:
      case VKI_V4L2_BUF_TYPE_SLICED_VBI_OUTPUT:
         PRE_FIELD_WRITE("ioctl(VKI_V4L2_G_FMT).fmt.sliced", data->fmt.sliced);
         break;
      case VKI_V4L2_BUF_TYPE_VIDEO_OVERLAY:
      case VKI_V4L2_BUF_TYPE_VIDEO_OUTPUT_OVERLAY:
         PRE_FIELD_READ("ioctl(VKI_V4L2_G_FMT).fmt.win.clips", data->fmt.win.clips);
         PRE_FIELD_READ("ioctl(VKI_V4L2_G_FMT).fmt.win.bitmap", data->fmt.win.bitmap);
         PRE_FIELD_READ("ioctl(VKI_V4L2_G_FMT).fmt.win.clipcount", data->fmt.win.clipcount);
         if (data->fmt.win.clipcount && data->fmt.win.clips)
            PRE_MEM_WRITE("ioctl(VKI_V4L2_G_FMT).fmt.win.clips[]",
                  (Addr)data->fmt.win.clips,
                  data->fmt.win.clipcount * sizeof(data->fmt.win.clips[0]));
         PRE_FIELD_WRITE("ioctl(VKI_V4L2_G_FMT).fmt.win.clipcount", data->fmt.win.clipcount);
         PRE_FIELD_WRITE("ioctl(VKI_V4L2_G_FMT).fmt.win.w", data->fmt.win.w);
         PRE_FIELD_WRITE("ioctl(VKI_V4L2_G_FMT).fmt.win.field", data->fmt.win.field);
         PRE_FIELD_WRITE("ioctl(VKI_V4L2_G_FMT).fmt.win.chromakey", data->fmt.win.chromakey);
         PRE_FIELD_WRITE("ioctl(VKI_V4L2_G_FMT).fmt.win.global_alpha", data->fmt.win.global_alpha);
         break;
      case VKI_V4L2_BUF_TYPE_VIDEO_CAPTURE_MPLANE:
      case VKI_V4L2_BUF_TYPE_VIDEO_OUTPUT_MPLANE:
         PRE_FIELD_WRITE("ioctl(VKI_V4L2_G_FMT).fmt.pix_mp", data->fmt.pix_mp);
         break;
      case VKI_V4L2_BUF_TYPE_SDR_CAPTURE:
         PRE_FIELD_WRITE("ioctl(VKI_V4L2_G_FMT).fmt.sdr", data->fmt.sdr);
         break;
      }
      break;
   }
   case VKI_V4L2_S_FMT: {
      struct vki_v4l2_format *data = (struct vki_v4l2_format *)ARG3;
      PRE_FIELD_READ("ioctl(VKI_V4L2_S_FMT).type", data->type);
      switch (data->type) {
      case VKI_V4L2_BUF_TYPE_VIDEO_CAPTURE:
      case VKI_V4L2_BUF_TYPE_VIDEO_OUTPUT:
         PRE_MEM_READ("ioctl(VKI_V4L2_S_FMT)",
               (Addr)&data->type + sizeof(data->type),
               sizeof(*data) - sizeof(data->type));
         break;
      case VKI_V4L2_BUF_TYPE_VBI_CAPTURE:
      case VKI_V4L2_BUF_TYPE_VBI_OUTPUT:
         PRE_FIELD_READ("ioctl(VKI_V4L2_S_FMT).fmt.vbi", data->fmt.vbi);
         break;
      case VKI_V4L2_BUF_TYPE_SLICED_VBI_CAPTURE:
      case VKI_V4L2_BUF_TYPE_SLICED_VBI_OUTPUT:
         PRE_FIELD_READ("ioctl(VKI_V4L2_S_FMT).fmt.sliced", data->fmt.sliced);
         break;
      case VKI_V4L2_BUF_TYPE_VIDEO_OVERLAY:
      case VKI_V4L2_BUF_TYPE_VIDEO_OUTPUT_OVERLAY:
         PRE_FIELD_READ("ioctl(VKI_V4L2_S_FMT).fmt.win", data->fmt.win);
         if (data->fmt.win.clipcount && data->fmt.win.clips)
            PRE_MEM_READ("ioctl(VKI_V4L2_S_FMT).fmt.win.clips[]",
                  (Addr)data->fmt.win.clips,
                  data->fmt.win.clipcount * sizeof(data->fmt.win.clips[0]));
         if (data->fmt.win.bitmap)
            PRE_MEM_READ("ioctl(VKI_V4L2_S_FMT).fmt.win.bitmap[]",
                  (Addr)data->fmt.win.bitmap,
                  data->fmt.win.w.height * ((data->fmt.win.w.width + 7) / 8));
         break;
      case VKI_V4L2_BUF_TYPE_VIDEO_CAPTURE_MPLANE:
      case VKI_V4L2_BUF_TYPE_VIDEO_OUTPUT_MPLANE:
         PRE_FIELD_READ("ioctl(VKI_V4L2_S_FMT).fmt.pix_mp", data->fmt.pix_mp);
         break;
      case VKI_V4L2_BUF_TYPE_SDR_CAPTURE:
         PRE_FIELD_READ("ioctl(VKI_V4L2_S_FMT).fmt.sdr", data->fmt.sdr);
         break;
      }
      break;
   }
   case VKI_V4L2_TRY_FMT: {
      struct vki_v4l2_format *data = (struct vki_v4l2_format *)ARG3;
      PRE_FIELD_READ("ioctl(VKI_V4L2_TRY_FMT).type", data->type);
      switch (data->type) {
      case VKI_V4L2_BUF_TYPE_VIDEO_CAPTURE:
      case VKI_V4L2_BUF_TYPE_VIDEO_OUTPUT:
         PRE_MEM_READ("ioctl(VKI_V4L2_TRY_FMT)",
               (Addr)&data->type + sizeof(data->type),
               sizeof(*data) - sizeof(data->type));
         break;
      case VKI_V4L2_BUF_TYPE_VBI_CAPTURE:
      case VKI_V4L2_BUF_TYPE_VBI_OUTPUT:
         PRE_FIELD_READ("ioctl(VKI_V4L2_TRY_FMT).fmt.vbi", data->fmt.vbi);
         break;
      case VKI_V4L2_BUF_TYPE_SLICED_VBI_CAPTURE:
      case VKI_V4L2_BUF_TYPE_SLICED_VBI_OUTPUT:
         PRE_FIELD_READ("ioctl(VKI_V4L2_TRY_FMT).fmt.sliced", data->fmt.sliced);
         break;
      case VKI_V4L2_BUF_TYPE_VIDEO_OVERLAY:
      case VKI_V4L2_BUF_TYPE_VIDEO_OUTPUT_OVERLAY:
         PRE_FIELD_READ("ioctl(VKI_V4L2_TRY_FMT).fmt.win", data->fmt.win);
         if (data->fmt.win.clipcount && data->fmt.win.clips)
            PRE_MEM_READ("ioctl(VKI_V4L2_TRY_FMT).fmt.win.clips[]",
                  (Addr)data->fmt.win.clips,
                  data->fmt.win.clipcount * sizeof(data->fmt.win.clips[0]));
         if (data->fmt.win.bitmap)
            PRE_MEM_READ("ioctl(VKI_V4L2_TRY_FMT).fmt.win.bitmap[]",
                  (Addr)data->fmt.win.bitmap,
                  data->fmt.win.w.height * ((data->fmt.win.w.width + 7) / 8));
         break;
      case VKI_V4L2_BUF_TYPE_VIDEO_CAPTURE_MPLANE:
      case VKI_V4L2_BUF_TYPE_VIDEO_OUTPUT_MPLANE:
         PRE_FIELD_READ("ioctl(VKI_V4L2_TRY_FMT).fmt.pix_mp", data->fmt.pix_mp);
         break;
      case VKI_V4L2_BUF_TYPE_SDR_CAPTURE:
         PRE_FIELD_READ("ioctl(VKI_V4L2_TRY_FMT).fmt.sdr", data->fmt.sdr);
         break;
      }
      break;
   }
   case VKI_V4L2_REQBUFS: {
      struct vki_v4l2_requestbuffers *data = (struct vki_v4l2_requestbuffers *)ARG3;
      PRE_MEM_READ("ioctl(VKI_V4L2_REQBUFS)", (Addr)data, sizeof(*data));
      break;
   }
   case VKI_V4L2_QUERYBUF: {
      struct vki_v4l2_buffer *data = (struct vki_v4l2_buffer *)ARG3;
      PRE_FIELD_READ("ioctl(VKI_V4L2_QUERYBUF).type", data->type);
      PRE_FIELD_READ("ioctl(VKI_V4L2_QUERYBUF).index", data->index);
      PRE_FIELD_READ("ioctl(VKI_V4L2_QUERYBUF).reserved", data->reserved);
      PRE_FIELD_READ("ioctl(VKI_V4L2_QUERYBUF).reserved2", data->reserved2);
      if (data->type == VKI_V4L2_BUF_TYPE_VIDEO_CAPTURE_MPLANE ||
            data->type == VKI_V4L2_BUF_TYPE_VIDEO_OUTPUT_MPLANE) {
         unsigned i;

         PRE_FIELD_READ("ioctl(VKI_V4L2_QUERYBUF).length", data->length);
         PRE_FIELD_READ("ioctl(VKI_V4L2_QUERYBUF).m.planes", data->m.planes);
         for (i = 0; i < data->length; i++) {
            PRE_FIELD_WRITE("ioctl(VKI_V4L2_QUERYBUF).m.planes[].bytesused", data->m.planes[i].bytesused);
            PRE_FIELD_WRITE("ioctl(VKI_V4L2_QUERYBUF).m.planes[].length", data->m.planes[i].length);
            PRE_FIELD_WRITE("ioctl(VKI_V4L2_QUERYBUF).m.planes[].m", data->m.planes[i].m);
            PRE_FIELD_WRITE("ioctl(VKI_V4L2_QUERYBUF).m.planes[].data_offset", data->m.planes[i].data_offset);
            PRE_FIELD_WRITE("ioctl(VKI_V4L2_QUERYBUF).m.planes[].reserved", data->m.planes[i].reserved);
         }
      } else {
         PRE_FIELD_WRITE("ioctl(VKI_V4L2_QUERYBUF).m", data->m);
         PRE_FIELD_WRITE("ioctl(VKI_V4L2_QUERYBUF).length", data->length);
      }
      PRE_FIELD_WRITE("ioctl(VKI_V4L2_QUERYBUF).bytesused", data->bytesused);
      PRE_FIELD_WRITE("ioctl(VKI_V4L2_QUERYBUF).flags", data->flags);
      PRE_FIELD_WRITE("ioctl(VKI_V4L2_QUERYBUF).field", data->field);
      PRE_FIELD_WRITE("ioctl(VKI_V4L2_QUERYBUF).timestamp", data->timestamp);
      PRE_FIELD_WRITE("ioctl(VKI_V4L2_QUERYBUF).timecode", data->timecode);
      PRE_FIELD_WRITE("ioctl(VKI_V4L2_QUERYBUF).sequence", data->sequence);
      PRE_FIELD_WRITE("ioctl(VKI_V4L2_QUERYBUF).memory", data->memory);
      PRE_FIELD_WRITE("ioctl(VKI_V4L2_QUERYBUF).sequence", data->sequence);
      break;
   }
   case VKI_V4L2_G_FBUF: {
      struct vki_v4l2_framebuffer *data = (struct vki_v4l2_framebuffer *)ARG3;
      PRE_MEM_WRITE("ioctl(VKI_V4L2_G_FBUF)", (Addr)data, sizeof(*data));
      break;
   }
   case VKI_V4L2_S_FBUF: {
      struct vki_v4l2_framebuffer *data = (struct vki_v4l2_framebuffer *)ARG3;
      PRE_FIELD_WRITE("ioctl(VKI_V4L2_S_FBUF).capability", data->capability);
      PRE_FIELD_READ("ioctl(VKI_V4L2_S_FBUF).flags", data->flags);
      PRE_FIELD_READ("ioctl(VKI_V4L2_S_FBUF).base", data->base);
      PRE_FIELD_READ("ioctl(VKI_V4L2_S_FBUF).fmt", data->fmt);
      break;
   }
   case VKI_V4L2_OVERLAY: {
      int *data = (int *)ARG3;
      PRE_MEM_READ("ioctl(VKI_V4L2_OVERLAY)", (Addr)data, sizeof(*data));
      break;
   }
   case VKI_V4L2_QBUF: {
      struct vki_v4l2_buffer *data = (struct vki_v4l2_buffer *)ARG3;
      int is_output = data->type == VKI_V4L2_BUF_TYPE_VIDEO_OUTPUT ||
         data->type == VKI_V4L2_BUF_TYPE_VIDEO_OUTPUT_MPLANE ||
         data->type == VKI_V4L2_BUF_TYPE_VBI_OUTPUT ||
         data->type == VKI_V4L2_BUF_TYPE_SLICED_VBI_OUTPUT;

      PRE_FIELD_READ("ioctl(VKI_V4L2_QBUF).type", data->type);
      PRE_FIELD_READ("ioctl(VKI_V4L2_QBUF).index", data->index);
      PRE_FIELD_READ("ioctl(VKI_V4L2_QBUF).flags", data->flags);
      PRE_FIELD_READ("ioctl(VKI_V4L2_QBUF).memory", data->memory);
      PRE_FIELD_READ("ioctl(VKI_V4L2_QBUF).reserved", data->reserved);
      PRE_FIELD_READ("ioctl(VKI_V4L2_QBUF).reserved2", data->reserved2);
      if (is_output) {
         PRE_FIELD_READ("ioctl(VKI_V4L2_QBUF).bytesused", data->bytesused);
         PRE_FIELD_READ("ioctl(VKI_V4L2_QBUF).field", data->field);
      }
      if (data->type == VKI_V4L2_BUF_TYPE_VIDEO_CAPTURE_MPLANE ||
            data->type == VKI_V4L2_BUF_TYPE_VIDEO_OUTPUT_MPLANE) {
         unsigned i;

         PRE_FIELD_READ("ioctl(VKI_V4L2_QBUF).length", data->length);
         PRE_FIELD_READ("ioctl(VKI_V4L2_QBUF).m.planes", data->m.planes);
         for (i = 0; i < data->length; i++) {
            if (is_output) {
               PRE_FIELD_READ("ioctl(VKI_V4L2_QBUF).m.planes[].bytesused", data->m.planes[i].bytesused);
               PRE_FIELD_READ("ioctl(VKI_V4L2_QBUF).m.planes[].data_offset", data->m.planes[i].data_offset);
            }
            if (data->memory == VKI_V4L2_MEMORY_MMAP)
               PRE_FIELD_WRITE("ioctl(VKI_V4L2_QBUF).m.planes[].m", data->m.planes[i].m);
            else if (data->memory == VKI_V4L2_MEMORY_DMABUF)
               PRE_FIELD_READ("ioctl(VKI_V4L2_QBUF).m.planes[].m.fd", data->m.planes[i].m.fd);
            else
               PRE_FIELD_READ("ioctl(VKI_V4L2_QBUF).m.planes[].m", data->m.planes[i].m);
            PRE_FIELD_READ("ioctl(VKI_V4L2_QBUF).m.planes[].reserved", data->m.planes[i].reserved);
         }
      } else {
         if (data->memory == VKI_V4L2_MEMORY_MMAP)
            PRE_FIELD_WRITE("ioctl(VKI_V4L2_QBUF).m", data->m);
         else if (data->memory == VKI_V4L2_MEMORY_DMABUF)
            PRE_FIELD_READ("ioctl(VKI_V4L2_QBUF).m.fd", data->m.fd);
         else
            PRE_FIELD_READ("ioctl(VKI_V4L2_QBUF).m", data->m);
         if (is_output) {
            PRE_FIELD_READ("ioctl(VKI_V4L2_QBUF).bytesused", data->bytesused);
            PRE_FIELD_READ("ioctl(VKI_V4L2_QBUF).field", data->field);
         }
      }
      if (is_output && (data->flags & VKI_V4L2_BUF_FLAG_TIMESTAMP_MASK) == VKI_V4L2_BUF_FLAG_TIMESTAMP_COPY) {
         PRE_FIELD_READ("ioctl(VKI_V4L2_QBUF).timestamp", data->timestamp);
         PRE_FIELD_READ("ioctl(VKI_V4L2_QBUF).timecode", data->timecode);
      }
      break;
   }
   case VKI_V4L2_EXPBUF: {
      struct vki_v4l2_exportbuffer *data = (struct vki_v4l2_exportbuffer *)ARG3;
      PRE_FIELD_READ("ioctl(VKI_V4L2_EXPBUF).type", data->type);
      PRE_FIELD_READ("ioctl(VKI_V4L2_EXPBUF).index", data->index);
      PRE_FIELD_READ("ioctl(VKI_V4L2_EXPBUF).plane", data->plane);
      PRE_FIELD_READ("ioctl(VKI_V4L2_EXPBUF).flags", data->flags);
      PRE_FIELD_WRITE("ioctl(VKI_V4L2_EXPBUF).fd", data->fd);
      PRE_FIELD_READ("ioctl(VKI_V4L2_EXPBUF).reserved", data->reserved);
      break;
   }
   case VKI_V4L2_DQBUF: {
      struct vki_v4l2_buffer *data = (struct vki_v4l2_buffer *)ARG3;
      PRE_FIELD_READ("ioctl(VKI_V4L2_DQBUF).type", data->type);
      PRE_FIELD_WRITE("ioctl(VKI_V4L2_DQBUF).index", data->index);
      PRE_FIELD_READ("ioctl(VKI_V4L2_DQBUF).memory", data->memory);
      PRE_FIELD_READ("ioctl(VKI_V4L2_DQBUF).reserved", data->reserved);
      PRE_FIELD_READ("ioctl(VKI_V4L2_DQBUF).reserved2", data->reserved2);
      PRE_FIELD_WRITE("ioctl(VKI_V4L2_DQBUF).bytesused", data->bytesused);
      PRE_FIELD_WRITE("ioctl(VKI_V4L2_DQBUF).field", data->field);
      if (data->type == VKI_V4L2_BUF_TYPE_VIDEO_CAPTURE_MPLANE ||
            data->type == VKI_V4L2_BUF_TYPE_VIDEO_OUTPUT_MPLANE) {
         unsigned i;

         PRE_FIELD_READ("ioctl(VKI_V4L2_DQBUF).length", data->length);
         PRE_FIELD_READ("ioctl(VKI_V4L2_DQBUF).m.planes", data->m.planes);
         for (i = 0; i < data->length; i++) {
            PRE_FIELD_WRITE("ioctl(VKI_V4L2_DQBUF).m.planes[].bytesused", data->m.planes[i].bytesused);
            PRE_FIELD_WRITE("ioctl(VKI_V4L2_DQBUF).m.planes[].data_offset", data->m.planes[i].data_offset);
            PRE_FIELD_WRITE("ioctl(VKI_V4L2_DQBUF).m.planes[].length", data->m.planes[i].length);
            PRE_FIELD_WRITE("ioctl(VKI_V4L2_DQBUF).m.planes[].m", data->m.planes[i].m);
            PRE_FIELD_READ("ioctl(VKI_V4L2_DQBUF).m.planes[].reserved", data->m.planes[i].reserved);
         }
      } else {
         PRE_FIELD_WRITE("ioctl(VKI_V4L2_DQBUF).m", data->m);
         PRE_FIELD_WRITE("ioctl(VKI_V4L2_DQBUF).length", data->length);
         PRE_FIELD_WRITE("ioctl(VKI_V4L2_DQBUF).bytesused", data->bytesused);
         PRE_FIELD_WRITE("ioctl(VKI_V4L2_DQBUF).field", data->field);
      }
      PRE_FIELD_WRITE("ioctl(VKI_V4L2_DQBUF).timestamp", data->timestamp);
      PRE_FIELD_WRITE("ioctl(VKI_V4L2_DQBUF).timecode", data->timecode);
      PRE_FIELD_WRITE("ioctl(VKI_V4L2_DQBUF).sequence", data->sequence);
      break;
   }
   case VKI_V4L2_STREAMON: {
      int *data = (int *)ARG3;
      PRE_MEM_READ("ioctl(VKI_V4L2_STREAMON)", (Addr)data, sizeof(*data));
      break;
   }
   case VKI_V4L2_STREAMOFF: {
      int *data = (int *)ARG3;
      PRE_MEM_READ("ioctl(VKI_V4L2_STREAMOFF)", (Addr)data, sizeof(*data));
      break;
   }
   case VKI_V4L2_G_PARM: {
      struct vki_v4l2_streamparm *data = (struct vki_v4l2_streamparm *)ARG3;
      int is_output = data->type == VKI_V4L2_BUF_TYPE_VIDEO_OUTPUT ||
         data->type == VKI_V4L2_BUF_TYPE_VIDEO_OUTPUT_MPLANE ||
         data->type == VKI_V4L2_BUF_TYPE_VBI_OUTPUT ||
         data->type == VKI_V4L2_BUF_TYPE_SLICED_VBI_OUTPUT;

      PRE_FIELD_READ("ioctl(VKI_V4L2_G_PARM).type", data->type);
      if (is_output) {
         PRE_MEM_WRITE("ioctl(VKI_V4L2_G_PARM)", (Addr)&data->parm.output,
            sizeof(data->parm.output) - sizeof(data->parm.output.reserved));
         PRE_FIELD_READ("ioctl(VKI_V4L2_G_PARM).parm.output.reserved", data->parm.output.reserved);
      } else {
         PRE_MEM_WRITE("ioctl(VKI_V4L2_G_PARM)", (Addr)&data->parm.capture,
            sizeof(data->parm.capture) - sizeof(data->parm.capture.reserved));
         PRE_FIELD_READ("ioctl(VKI_V4L2_G_PARM).parm.capture.reserved", data->parm.capture.reserved);
      }
      break;
   }
   case VKI_V4L2_S_PARM: {
      struct vki_v4l2_streamparm *data = (struct vki_v4l2_streamparm *)ARG3;
      int is_output = data->type == VKI_V4L2_BUF_TYPE_VIDEO_OUTPUT ||
         data->type == VKI_V4L2_BUF_TYPE_VIDEO_OUTPUT_MPLANE ||
         data->type == VKI_V4L2_BUF_TYPE_VBI_OUTPUT ||
         data->type == VKI_V4L2_BUF_TYPE_SLICED_VBI_OUTPUT;

      PRE_FIELD_READ("ioctl(VKI_V4L2_S_PARM).type", data->type);
      if (is_output)
         PRE_FIELD_READ("ioctl(VKI_V4L2_S_PARM).parm.output", data->parm.output);
      else
         PRE_FIELD_READ("ioctl(VKI_V4L2_S_PARM).parm.capture", data->parm.capture);
      break;
   }
   case VKI_V4L2_G_STD: {
      vki_v4l2_std_id *data = (vki_v4l2_std_id *)ARG3;
      PRE_MEM_WRITE("ioctl(VKI_V4L2_G_STD)", (Addr)data, sizeof(*data));
      break;
   }
   case VKI_V4L2_S_STD: {
      vki_v4l2_std_id *data = (vki_v4l2_std_id *)ARG3;
      PRE_MEM_READ("ioctl(VKI_V4L2_S_STD)", (Addr)data, sizeof(*data));
      break;
   }
   case VKI_V4L2_ENUMSTD: {
      struct vki_v4l2_standard *data = (struct vki_v4l2_standard *)ARG3;
      PRE_FIELD_READ("ioctl(VKI_V4L2_ENUMSTD).index", data->index);
      PRE_MEM_WRITE("ioctl(VKI_V4L2_ENUMSTD)", (Addr)&data->id, sizeof(*data) - sizeof(data->index));
      break;
   }
   case VKI_V4L2_ENUMINPUT: {
      struct vki_v4l2_input *data = (struct vki_v4l2_input *)ARG3;
      PRE_FIELD_READ("ioctl(VKI_V4L2_ENUMINPUT).index", data->index);
      PRE_MEM_WRITE("ioctl(VKI_V4L2_ENUMINPUT)", (Addr)data->name, sizeof(*data) - sizeof(data->index));
      break;
   }
   case VKI_V4L2_G_CTRL: {
      struct vki_v4l2_control *data = (struct vki_v4l2_control *)ARG3;
      PRE_FIELD_READ("ioctl(VKI_V4L2_G_CTRL).id", data->id);
      PRE_FIELD_WRITE("ioctl(VKI_V4L2_G_CTRL).value", data->value);
      break;
   }
   case VKI_V4L2_S_CTRL: {
      struct vki_v4l2_control *data = (struct vki_v4l2_control *)ARG3;
      PRE_MEM_READ("ioctl(VKI_V4L2_S_CTRL)", (Addr)data, sizeof(*data));
      break;
   }
   case VKI_V4L2_G_TUNER: {
      struct vki_v4l2_tuner *data = (struct vki_v4l2_tuner *)ARG3;
      PRE_FIELD_READ("ioctl(VKI_V4L2_G_TUNER).index", data->index);
      PRE_FIELD_READ("ioctl(VKI_V4L2_G_TUNER).reserved", data->reserved);
      PRE_MEM_WRITE("ioctl(VKI_V4L2_G_TUNER)", (Addr)data->name,
            sizeof(*data) - sizeof(data->index) - sizeof(data->reserved));
      break;
   }
   case VKI_V4L2_S_TUNER: {
      struct vki_v4l2_tuner *data = (struct vki_v4l2_tuner *)ARG3;
      PRE_FIELD_READ("ioctl(VKI_V4L2_S_TUNER).index", data->index);
      PRE_FIELD_READ("ioctl(VKI_V4L2_S_TUNER).audmode", data->audmode);
      PRE_FIELD_READ("ioctl(VKI_V4L2_S_TUNER).reserved", data->reserved);
      break;
   }
   case VKI_V4L2_G_AUDIO: {
      struct vki_v4l2_audio *data = (struct vki_v4l2_audio *)ARG3;
      PRE_MEM_WRITE("ioctl(VKI_V4L2_G_AUDIO)", (Addr)data,
            sizeof(*data) - sizeof(data->reserved));
      PRE_FIELD_READ("ioctl(VKI_V4L2_G_AUDIO).reserved", data->reserved);
      break;
   }
   case VKI_V4L2_S_AUDIO: {
      struct vki_v4l2_audio *data = (struct vki_v4l2_audio *)ARG3;
      PRE_FIELD_READ("ioctl(VKI_V4L2_S_AUDIO).index", data->index);
      PRE_FIELD_READ("ioctl(VKI_V4L2_S_AUDIO).mode", data->mode);
      PRE_FIELD_READ("ioctl(VKI_V4L2_S_AUDIO).reserved", data->reserved);
      break;
   }
   case VKI_V4L2_QUERYCTRL: {
      struct vki_v4l2_queryctrl *data = (struct vki_v4l2_queryctrl *)ARG3;
      PRE_FIELD_READ("ioctl(VKI_V4L2_QUERYCTRL).id", data->id);
      PRE_MEM_WRITE("ioctl(VKI_V4L2_QUERYCTRL)", (Addr)&data->type,
            sizeof(*data) - sizeof(data->id));
      break;
   }
   case VKI_V4L2_QUERYMENU: {
      struct vki_v4l2_querymenu *data = (struct vki_v4l2_querymenu *)ARG3;
      PRE_FIELD_READ("ioctl(VKI_V4L2_QUERYMENU).id", data->id);
      PRE_FIELD_READ("ioctl(VKI_V4L2_QUERYMENU).index", data->index);
      PRE_MEM_WRITE("ioctl(VKI_V4L2_QUERYMENU)", (Addr)data->name,
            sizeof(*data) - sizeof(data->id) - sizeof(data->index));
      break;
   }
   case VKI_V4L2_G_INPUT: {
      int *data = (int *)ARG3;
      PRE_MEM_WRITE("ioctl(VKI_V4L2_G_INPUT)", (Addr)data, sizeof(*data));
      break;
   }
   case VKI_V4L2_S_INPUT: {
      int *data = (int *)ARG3;
      PRE_MEM_READ("ioctl(VKI_V4L2_S_INPUT)", (Addr)data, sizeof(*data));
      break;
   }
   case VKI_V4L2_G_EDID: {
      struct vki_v4l2_edid *data = (struct vki_v4l2_edid *)ARG3;
      PRE_MEM_READ("ioctl(VKI_V4L2_G_EDID)", (Addr)data, sizeof(*data));
      if (data->blocks && data->edid)
         PRE_MEM_WRITE("ioctl(VKI_V4L2_G_EDID)", (Addr)data->edid, data->blocks * 128);
      break;
   }
   case VKI_V4L2_S_EDID: {
      struct vki_v4l2_edid *data = (struct vki_v4l2_edid *)ARG3;
      PRE_MEM_READ("ioctl(VKI_V4L2_S_EDID)", (Addr)data, sizeof(*data));
      if (data->blocks && data->edid)
         PRE_MEM_READ("ioctl(VKI_V4L2_S_EDID)", (Addr)data->edid, data->blocks * 128);
      break;
   }
   case VKI_V4L2_G_OUTPUT: {
      int *data = (int *)ARG3;
      PRE_MEM_WRITE("ioctl(VKI_V4L2_G_OUTPUT)", (Addr)data, sizeof(*data));
      break;
   }
   case VKI_V4L2_S_OUTPUT: {
      int *data = (int *)ARG3;
      PRE_MEM_READ("ioctl(VKI_V4L2_S_OUTPUT)", (Addr)data, sizeof(*data));
      break;
   }
   case VKI_V4L2_ENUMOUTPUT: {
      struct vki_v4l2_output *data = (struct vki_v4l2_output *)ARG3;
      PRE_FIELD_READ("ioctl(VKI_V4L2_ENUMOUTPUT).index", data->index);
      PRE_MEM_WRITE("ioctl(VKI_V4L2_ENUMOUTPUT)", (Addr)data->name, sizeof(*data) - sizeof(data->index));
      break;
   }
   case VKI_V4L2_G_AUDOUT: {
      struct vki_v4l2_audioout *data = (struct vki_v4l2_audioout *)ARG3;
      PRE_MEM_WRITE("ioctl(VKI_V4L2_G_AUDOUT)", (Addr)data,
            sizeof(*data) - sizeof(data->reserved));
      PRE_FIELD_READ("ioctl(VKI_V4L2_G_AUDOUT).reserved", data->reserved);
      break;
   }
   case VKI_V4L2_S_AUDOUT: {
      struct vki_v4l2_audioout *data = (struct vki_v4l2_audioout *)ARG3;
      PRE_FIELD_READ("ioctl(VKI_V4L2_S_AUDOUT).index", data->index);
      PRE_FIELD_READ("ioctl(VKI_V4L2_S_AUDOUT).reserved", data->reserved);
      PRE_FIELD_READ("ioctl(VKI_V4L2_S_AUDOUT).mode", data->mode);
      break;
   }
   case VKI_V4L2_G_MODULATOR: {
      struct vki_v4l2_modulator *data = (struct vki_v4l2_modulator *)ARG3;
      PRE_FIELD_READ("ioctl(VKI_V4L2_G_MODULATOR).index", data->index);
      PRE_FIELD_READ("ioctl(VKI_V4L2_G_MODULATOR).reserved", data->reserved);
      PRE_MEM_WRITE("ioctl(VKI_V4L2_G_MODULATOR)", (Addr)data->name,
            sizeof(*data) - sizeof(data->index) - sizeof(data->reserved));
      break;
   }
   case VKI_V4L2_S_MODULATOR: {
      struct vki_v4l2_modulator *data = (struct vki_v4l2_modulator *)ARG3;
      PRE_FIELD_READ("ioctl(VKI_V4L2_S_MODULATOR).index", data->index);
      PRE_FIELD_READ("ioctl(VKI_V4L2_S_MODULATOR).txsubchans", data->txsubchans);
      PRE_FIELD_READ("ioctl(VKI_V4L2_S_MODULATOR).reserved", data->reserved);
      break;
   }
   case VKI_V4L2_G_FREQUENCY: {
      struct vki_v4l2_frequency *data = (struct vki_v4l2_frequency *)ARG3;
      PRE_FIELD_READ("ioctl(VKI_V4L2_G_FREQUENCY).tuner", data->tuner);
      PRE_FIELD_READ("ioctl(VKI_V4L2_G_FREQUENCY).reserved", data->reserved);
      PRE_FIELD_WRITE("ioctl(VKI_V4L2_G_FREQUENCY).type", data->type);
      PRE_FIELD_WRITE("ioctl(VKI_V4L2_G_FREQUENCY).frequency", data->frequency);
      break;
   }
   case VKI_V4L2_S_FREQUENCY: {
      struct vki_v4l2_frequency *data = (struct vki_v4l2_frequency *)ARG3;
      PRE_MEM_READ("ioctl(VKI_V4L2_S_FREQUENCY)", (Addr)data, sizeof(*data));
      break;
   }
   case VKI_V4L2_CROPCAP: {
      struct vki_v4l2_cropcap *data = (struct vki_v4l2_cropcap *)ARG3;
      PRE_FIELD_READ("ioctl(VKI_V4L2_CROPCAP)", data->type);
      PRE_MEM_WRITE("ioctl(VKI_V4L2_CROPCAP)", (Addr)&data->bounds, sizeof(*data) - sizeof(data->type));
      break;
   }
   case VKI_V4L2_G_CROP: {
      struct vki_v4l2_crop *data = (struct vki_v4l2_crop *)ARG3;
      PRE_FIELD_READ("ioctl(VKI_V4L2_G_CROP).type", data->type);
      PRE_FIELD_WRITE("ioctl(VKI_V4L2_G_CROP).c", data->c);
      break;
   }
   case VKI_V4L2_S_CROP: {
      struct vki_v4l2_crop *data = (struct vki_v4l2_crop *)ARG3;
      PRE_MEM_READ("ioctl(VKI_V4L2_S_CROP)", (Addr)data, sizeof(*data));
      break;
   }
   case VKI_V4L2_G_JPEGCOMP: {
      struct vki_v4l2_jpegcompression *data = (struct vki_v4l2_jpegcompression *)ARG3;
      PRE_MEM_WRITE("ioctl(VKI_V4L2_G_JPEGCOMP)", (Addr)data, sizeof(*data));
      break;
   }
   case VKI_V4L2_S_JPEGCOMP: {
      struct vki_v4l2_jpegcompression *data = (struct vki_v4l2_jpegcompression *)ARG3;
      PRE_MEM_READ("ioctl(VKI_V4L2_S_JPEGCOMP)", (Addr)data, sizeof(*data));
      break;
   }
   case VKI_V4L2_QUERYSTD: {
      vki_v4l2_std_id *data = (vki_v4l2_std_id *)ARG3;
      PRE_MEM_WRITE("ioctl(VKI_V4L2_QUERYSTD)", (Addr)data, sizeof(*data));
      break;
   }
   case VKI_V4L2_ENUMAUDIO: {
      struct vki_v4l2_audio *data = (struct vki_v4l2_audio *)ARG3;
      PRE_FIELD_READ("ioctl(VKI_V4L2_ENUMAUDIO).index", data->index);
      PRE_FIELD_READ("ioctl(VKI_V4L2_ENUMAUDIO).reserved", data->reserved);
      PRE_MEM_WRITE("ioctl(VKI_V4L2_ENUMAUDIO)", (Addr)data->name,
            sizeof(*data) - sizeof(data->index) - sizeof(data->reserved));
      break;
   }
   case VKI_V4L2_ENUMAUDOUT: {
      struct vki_v4l2_audioout *data = (struct vki_v4l2_audioout *)ARG3;
      PRE_FIELD_READ("ioctl(VKI_V4L2_ENUMAUDOUT).index", data->index);
      PRE_FIELD_READ("ioctl(VKI_V4L2_ENUMAUDOUT).reserved", data->reserved);
      PRE_MEM_WRITE("ioctl(VKI_V4L2_ENUMAUDOUT)", (Addr)data->name,
            sizeof(*data) - sizeof(data->index) - sizeof(data->reserved));
      break;
   }
   case VKI_V4L2_G_PRIORITY: {
      __vki_u32 *data = (__vki_u32 *)ARG3;
      PRE_MEM_WRITE("ioctl(VKI_V4L2_G_PRIORITY)", (Addr)data, sizeof(*data));
      break;
   }
   case VKI_V4L2_S_PRIORITY: {
      __vki_u32 *data = (__vki_u32 *)ARG3;
      PRE_MEM_READ("ioctl(VKI_V4L2_S_PRIORITY)", (Addr)data, sizeof(*data));
      break;
   }
   case VKI_V4L2_G_SLICED_VBI_CAP: {
      struct vki_v4l2_sliced_vbi_cap *data = (struct vki_v4l2_sliced_vbi_cap *)ARG3;
      PRE_FIELD_READ("ioctl(VKI_V4L2_G_SLICED_VBI_CAP).type", data->type);
      PRE_FIELD_READ("ioctl(VKI_V4L2_G_SLICED_VBI_CAP).reserved", data->reserved);
      PRE_MEM_WRITE("ioctl(VKI_V4L2_G_SLICED_VBI_CAP)", (Addr)data,
            sizeof(*data) - sizeof(data->type) - sizeof(data->reserved));
      break;
   }
   case VKI_V4L2_G_EXT_CTRLS: {
      struct vki_v4l2_ext_controls *data = (struct vki_v4l2_ext_controls *)ARG3;
      PRE_FIELD_READ("ioctl(VKI_V4L2_G_EXT_CTRLS).ctrl_class", data->ctrl_class);
      PRE_FIELD_READ("ioctl(VKI_V4L2_G_EXT_CTRLS).count", data->count);
      if (data->count) {
         unsigned i;

         PRE_FIELD_READ("ioctl(VKI_V4L2_G_EXT_CTRLS).controls", data->controls);
         for (i = 0; i < data->count; i++) {
            PRE_FIELD_READ("ioctl(VKI_V4L2_G_EXT_CTRLS).controls[].id", data->controls[i].id);
            PRE_FIELD_READ("ioctl(VKI_V4L2_G_EXT_CTRLS).controls[].size", data->controls[i].size);
            PRE_FIELD_READ("ioctl(VKI_V4L2_G_EXT_CTRLS).controls[].reserved2", data->controls[i].reserved2);
            if (data->controls[i].size) {
               PRE_FIELD_READ("ioctl(VKI_V4L2_G_EXT_CTRLS).controls[].ptr", data->controls[i].ptr);
               PRE_MEM_WRITE("ioctl(VKI_V4L2_G_EXT_CTRLS).controls[].ptr[]",
                     (Addr)data->controls[i].ptr, data->controls[i].size);
            } else {
               PRE_FIELD_WRITE("ioctl(VKI_V4L2_G_EXT_CTRLS).controls[].value64",
                     data->controls[i].value64);
            }
         }
      }
      PRE_FIELD_WRITE("ioctl(VKI_V4L2_G_EXT_CTRLS).error_idx", data->error_idx);
      PRE_FIELD_READ("ioctl(VKI_V4L2_G_EXT_CTRLS).reserved", data->reserved);
      break;
   }
   case VKI_V4L2_S_EXT_CTRLS: {
      struct vki_v4l2_ext_controls *data = (struct vki_v4l2_ext_controls *)ARG3;
      PRE_FIELD_READ("ioctl(VKI_V4L2_S_EXT_CTRLS).ctrl_class", data->ctrl_class);
      PRE_FIELD_READ("ioctl(VKI_V4L2_S_EXT_CTRLS).count", data->count);
      if (data->count) {
         unsigned i;

         PRE_FIELD_READ("ioctl(VKI_V4L2_S_EXT_CTRLS).controls", data->controls);
         PRE_MEM_READ("ioctl(VKI_V4L2_S_EXT_CTRLS)", (Addr)data->controls,
               data->count * sizeof(data->controls[0]));
         for (i = 0; i < data->count; i++) {
            if (data->controls[i].size) {
               PRE_MEM_READ("ioctl(VKI_V4L2_S_EXT_CTRLS).controls[].ptr[]",
                     (Addr)data->controls[i].ptr, data->controls[i].size);
            }
         }
      }
      PRE_FIELD_WRITE("ioctl(VKI_V4L2_S_EXT_CTRLS).error_idx", data->error_idx);
      PRE_FIELD_READ("ioctl(VKI_V4L2_S_EXT_CTRLS).reserved", data->reserved);
      break;
   }
   case VKI_V4L2_TRY_EXT_CTRLS: {
      struct vki_v4l2_ext_controls *data = (struct vki_v4l2_ext_controls *)ARG3;
      PRE_FIELD_READ("ioctl(VKI_V4L2_TRY_EXT_CTRLS).ctrl_class", data->ctrl_class);
      PRE_FIELD_READ("ioctl(VKI_V4L2_TRY_EXT_CTRLS).count", data->count);
      if (data->count) {
         unsigned i;

         PRE_FIELD_READ("ioctl(VKI_V4L2_TRY_EXT_CTRLS).controls", data->controls);
         PRE_MEM_READ("ioctl(VKI_V4L2_TRY_EXT_CTRLS)", (Addr)data->controls,
               data->count * sizeof(data->controls[0]));
         for (i = 0; i < data->count; i++) {
            if (data->controls[i].size) {
               PRE_MEM_READ("ioctl(VKI_V4L2_TRY_EXT_CTRLS).controls[].ptr[]",
                     (Addr)data->controls[i].ptr, data->controls[i].size);
            }
         }
      }
      PRE_FIELD_WRITE("ioctl(VKI_V4L2_TRY_EXT_CTRLS).error_idx", data->error_idx);
      PRE_FIELD_READ("ioctl(VKI_V4L2_TRY_EXT_CTRLS).reserved", data->reserved);
      break;
   }
   case VKI_V4L2_ENUM_FRAMESIZES: {
      struct vki_v4l2_frmsizeenum *data = (struct vki_v4l2_frmsizeenum *)ARG3;
      PRE_FIELD_READ("ioctl(VKI_V4L2_ENUM_FRAMESIZES).index", data->index);
      PRE_FIELD_READ("ioctl(VKI_V4L2_ENUM_FRAMESIZES).pixel_format", data->pixel_format);
      PRE_FIELD_READ("ioctl(VKI_V4L2_ENUM_FRAMESIZES).reserved", data->reserved);
      PRE_FIELD_WRITE("ioctl(VKI_V4L2_ENUM_FRAMESIZES).type", data->type);
      PRE_FIELD_WRITE("ioctl(VKI_V4L2_ENUM_FRAMESIZES).stepwise", data->stepwise);
      break;
   }
   case VKI_V4L2_ENUM_FRAMEINTERVALS: {
      struct vki_v4l2_frmivalenum *data = (struct vki_v4l2_frmivalenum *)ARG3;
      PRE_FIELD_READ("ioctl(VKI_V4L2_ENUM_FRAMEINTERVALS).index", data->index);
      PRE_FIELD_READ("ioctl(VKI_V4L2_ENUM_FRAMEINTERVALS).pixel_format", data->pixel_format);
      PRE_FIELD_READ("ioctl(VKI_V4L2_ENUM_FRAMEINTERVALS).width", data->width);
      PRE_FIELD_READ("ioctl(VKI_V4L2_ENUM_FRAMEINTERVALS).height", data->height);
      PRE_FIELD_READ("ioctl(VKI_V4L2_ENUM_FRAMEINTERVALS).reserved", data->reserved);
      PRE_FIELD_WRITE("ioctl(VKI_V4L2_ENUM_FRAMEINTERVALS).type", data->type);
      PRE_FIELD_WRITE("ioctl(VKI_V4L2_ENUM_FRAMEINTERVALS).stepwise", data->stepwise);
      break;
   }
   case VKI_V4L2_G_ENC_INDEX: {
      struct vki_v4l2_enc_idx *data = (struct vki_v4l2_enc_idx *)ARG3;
      PRE_MEM_WRITE("ioctl(VKI_V4L2_G_ENC_INDEX)", (Addr)data, sizeof(*data));
      break;
   }
   case VKI_V4L2_ENCODER_CMD: {
      struct vki_v4l2_encoder_cmd *data = (struct vki_v4l2_encoder_cmd *)ARG3;
      PRE_MEM_READ("ioctl(VKI_V4L2_ENCODER_CMD)", (Addr)data, sizeof(*data));
      break;
   }
   case VKI_V4L2_TRY_ENCODER_CMD: {
      struct vki_v4l2_encoder_cmd *data = (struct vki_v4l2_encoder_cmd *)ARG3;
      PRE_MEM_READ("ioctl(VKI_V4L2_TRY_ENCODER_CMD)", (Addr)data, sizeof(*data));
      break;
   }
   case VKI_V4L2_DBG_S_REGISTER: {
      struct vki_v4l2_dbg_register *data = (struct vki_v4l2_dbg_register *)ARG3;
      PRE_FIELD_READ("ioctl(VKI_V4L2_DBG_S_REGISTER).match.type", data->match.type);
      PRE_FIELD_READ("ioctl(VKI_V4L2_DBG_S_REGISTER).match.addr", data->match.addr);
      PRE_FIELD_READ("ioctl(VKI_V4L2_DBG_S_REGISTER).reg", data->reg);
      PRE_FIELD_READ("ioctl(VKI_V4L2_DBG_S_REGISTER).val", data->val);
      PRE_FIELD_WRITE("ioctl(VKI_V4L2_DBG_S_REGISTER).size", data->size);
      break;
   }
   case VKI_V4L2_DBG_G_REGISTER: {
      struct vki_v4l2_dbg_register *data = (struct vki_v4l2_dbg_register *)ARG3;
      PRE_FIELD_READ("ioctl(VKI_V4L2_DBG_G_REGISTER).match.type", data->match.type);
      PRE_FIELD_READ("ioctl(VKI_V4L2_DBG_G_REGISTER).match.addr", data->match.addr);
      PRE_FIELD_READ("ioctl(VKI_V4L2_DBG_G_REGISTER).reg", data->reg);
      PRE_FIELD_WRITE("ioctl(VKI_V4L2_DBG_G_REGISTER).val", data->val);
      PRE_FIELD_WRITE("ioctl(VKI_V4L2_DBG_G_REGISTER).size", data->size);
      break;
   }
   case VKI_V4L2_S_HW_FREQ_SEEK: {
      struct vki_v4l2_hw_freq_seek *data = (struct vki_v4l2_hw_freq_seek *)ARG3;
      PRE_MEM_READ("ioctl(VKI_V4L2_S_HW_FREQ_SEEK)", (Addr)data, sizeof(*data));
      break;
   }
   case VKI_V4L2_S_DV_TIMINGS: {
      struct vki_v4l2_dv_timings *data = (struct vki_v4l2_dv_timings *)ARG3;
      PRE_FIELD_READ("ioctl(VKI_V4L2_S_DV_TIMINGS).type", data->type);
      PRE_FIELD_READ("ioctl(VKI_V4L2_S_DV_TIMINGS).bt", data->bt);
      break;
   }
   case VKI_V4L2_G_DV_TIMINGS: {
      struct vki_v4l2_dv_timings *data = (struct vki_v4l2_dv_timings *)ARG3;
      PRE_MEM_WRITE("ioctl(VKI_V4L2_G_DV_TIMINGS)", (Addr)data, sizeof(*data));
      break;
   }
   case VKI_V4L2_DQEVENT: {
      struct vki_v4l2_event *data = (struct vki_v4l2_event *)ARG3;
      PRE_MEM_WRITE("ioctl(VKI_V4L2_DQEVENT)", (Addr)data, sizeof(*data));
      break;
   }
   case VKI_V4L2_SUBSCRIBE_EVENT: {
      struct vki_v4l2_event_subscription *data = (struct vki_v4l2_event_subscription *)ARG3;
      PRE_MEM_READ("ioctl(VKI_V4L2_SUBSCRIBE_EVENT)", (Addr)data, sizeof(*data));
      break;
   }
   case VKI_V4L2_UNSUBSCRIBE_EVENT: {
      struct vki_v4l2_event_subscription *data = (struct vki_v4l2_event_subscription *)ARG3;
      PRE_MEM_READ("ioctl(VKI_V4L2_UNSUBSCRIBE_EVENT)", (Addr)data, sizeof(*data));
      break;
   }
   case VKI_V4L2_CREATE_BUFS: {
      struct vki_v4l2_create_buffers *data = (struct vki_v4l2_create_buffers *)ARG3;
      struct vki_v4l2_format *fmt = &data->format;
      PRE_FIELD_WRITE("ioctl(VKI_V4L2_CREATE_BUFS).index", data->index);
      PRE_FIELD_READ("ioctl(VKI_V4L2_CREATE_BUFS).count", data->count);
      PRE_FIELD_READ("ioctl(VKI_V4L2_CREATE_BUFS).memory", data->memory);
      PRE_FIELD_READ("ioctl(VKI_V4L2_CREATE_BUFS).reserved", data->reserved);
      PRE_FIELD_READ("ioctl(VKI_V4L2_CREATE_BUFS).format.type", fmt->type);
      switch (fmt->type) {
      case VKI_V4L2_BUF_TYPE_VIDEO_CAPTURE:
      case VKI_V4L2_BUF_TYPE_VIDEO_OUTPUT:
         PRE_FIELD_READ("ioctl(VKI_V4L2_CREATE_BUFS).format.pix", fmt->fmt.raw_data);
         break;
      case VKI_V4L2_BUF_TYPE_VBI_CAPTURE:
      case VKI_V4L2_BUF_TYPE_VBI_OUTPUT:
         PRE_FIELD_READ("ioctl(VKI_V4L2_CREATE_BUFS).format.vbi", fmt->fmt.vbi);
         break;
      case VKI_V4L2_BUF_TYPE_SLICED_VBI_CAPTURE:
      case VKI_V4L2_BUF_TYPE_SLICED_VBI_OUTPUT:
         PRE_FIELD_READ("ioctl(VKI_V4L2_CREATE_BUFS).format.sliced", fmt->fmt.sliced);
         break;
      case VKI_V4L2_BUF_TYPE_VIDEO_OVERLAY:
      case VKI_V4L2_BUF_TYPE_VIDEO_OUTPUT_OVERLAY:
         PRE_FIELD_READ("ioctl(VKI_V4L2_CREATE_BUFS).format.win", fmt->fmt.win);
         break;
      case VKI_V4L2_BUF_TYPE_VIDEO_CAPTURE_MPLANE:
      case VKI_V4L2_BUF_TYPE_VIDEO_OUTPUT_MPLANE:
         PRE_FIELD_READ("ioctl(VKI_V4L2_CREATE_BUFS).format.pix_mp", fmt->fmt.pix_mp);
         break;
      case VKI_V4L2_BUF_TYPE_SDR_CAPTURE:
         PRE_FIELD_READ("ioctl(VKI_V4L2_CREATE_BUFS).format.sdr", fmt->fmt.sdr);
         break;
      }
      break;
   }
   case VKI_V4L2_PREPARE_BUF: {
      struct vki_v4l2_buffer *data = (struct vki_v4l2_buffer *)ARG3;
      PRE_FIELD_READ("ioctl(VKI_V4L2_PREPARE_BUF).index", data->index);
      PRE_FIELD_READ("ioctl(VKI_V4L2_PREPARE_BUF).type", data->type);
      PRE_FIELD_READ("ioctl(VKI_V4L2_PREPARE_BUF).memory", data->memory);
      PRE_FIELD_READ("ioctl(VKI_V4L2_PREPARE_BUF).reserved", data->reserved);
      PRE_FIELD_READ("ioctl(VKI_V4L2_PREPARE_BUF).reserved2", data->reserved2);
      if (data->type == VKI_V4L2_BUF_TYPE_VIDEO_CAPTURE_MPLANE ||
            data->type == VKI_V4L2_BUF_TYPE_VIDEO_OUTPUT_MPLANE) {
         unsigned i;

         PRE_FIELD_READ("ioctl(VKI_V4L2_PREPARE_BUF).length", data->length);
         PRE_FIELD_READ("ioctl(VKI_V4L2_PREPARE_BUF).m.planes", data->m.planes);
         for (i = 0; i < data->length; i++) {
            PRE_FIELD_READ("ioctl(VKI_V4L2_PREPARE_BUF).m.planes[].reserved", data->m.planes[i].reserved);
         }
      }
      break;
   }
   case VKI_V4L2_G_SELECTION: {
      struct vki_v4l2_selection *data = (struct vki_v4l2_selection *)ARG3;
      PRE_FIELD_READ("ioctl(VKI_V4L2_G_SELECTION).type", data->type);
      PRE_FIELD_READ("ioctl(VKI_V4L2_G_SELECTION).target", data->target);
      PRE_FIELD_READ("ioctl(VKI_V4L2_G_SELECTION).flags", data->flags);
      PRE_FIELD_READ("ioctl(VKI_V4L2_G_SELECTION).reserved", data->reserved);
      PRE_FIELD_WRITE("ioctl(VKI_V4L2_G_SELECTION).r", data->r);
      break;
   }
   case VKI_V4L2_S_SELECTION: {
      struct vki_v4l2_selection *data = (struct vki_v4l2_selection *)ARG3;
      PRE_MEM_READ("ioctl(VKI_V4L2_S_SELECTION)", (Addr)data, sizeof(*data));
      break;
   }
   case VKI_V4L2_DECODER_CMD: {
      struct vki_v4l2_decoder_cmd *data = (struct vki_v4l2_decoder_cmd *)ARG3;
      PRE_MEM_READ("ioctl(VKI_V4L2_DECODER_CMD)", (Addr)data, sizeof(*data));
      break;
   }
   case VKI_V4L2_TRY_DECODER_CMD: {
      struct vki_v4l2_decoder_cmd *data = (struct vki_v4l2_decoder_cmd *)ARG3;
      PRE_MEM_READ("ioctl(VKI_V4L2_TRY_DECODER_CMD)", (Addr)data, sizeof(*data));
      break;
   }
   case VKI_V4L2_ENUM_DV_TIMINGS: {
      struct vki_v4l2_enum_dv_timings *data = (struct vki_v4l2_enum_dv_timings *)ARG3;
      PRE_FIELD_READ("ioctl(VKI_V4L2_ENUM_DV_TIMINGS).index", data->index);
      PRE_FIELD_READ("ioctl(VKI_V4L2_ENUM_DV_TIMINGS).pad", data->pad);
      PRE_FIELD_READ("ioctl(VKI_V4L2_ENUM_DV_TIMINGS).reserved", data->reserved);
      PRE_FIELD_WRITE("ioctl(VKI_V4L2_ENUM_DV_TIMINGS).timings", data->timings);
      break;
   }
   case VKI_V4L2_QUERY_DV_TIMINGS: {
      struct vki_v4l2_dv_timings *data = (struct vki_v4l2_dv_timings *)ARG3;
      PRE_MEM_WRITE("ioctl(VKI_V4L2_QUERY_DV_TIMINGS)", (Addr)data, sizeof(*data));
      break;
   }
   case VKI_V4L2_DV_TIMINGS_CAP: {
      struct vki_v4l2_dv_timings_cap *data = (struct vki_v4l2_dv_timings_cap *)ARG3;
      PRE_MEM_WRITE("ioctl(VKI_V4L2_DV_TIMINGS_CAP)", (Addr)data, sizeof(*data));
      break;
   }
   case VKI_V4L2_ENUM_FREQ_BANDS: {
      struct vki_v4l2_frequency_band *data = (struct vki_v4l2_frequency_band *)ARG3;
      PRE_FIELD_READ("ioctl(VKI_V4L2_ENUM_FREQ_BANDS).tuner", data->tuner);
      PRE_FIELD_READ("ioctl(VKI_V4L2_ENUM_FREQ_BANDS).type", data->type);
      PRE_FIELD_READ("ioctl(VKI_V4L2_ENUM_FREQ_BANDS).index", data->index);
      PRE_FIELD_READ("ioctl(VKI_V4L2_ENUM_FREQ_BANDS).reserved", data->reserved);
      PRE_FIELD_WRITE("ioctl(VKI_V4L2_ENUM_FREQ_BANDS).capability", data->capability);
      PRE_FIELD_WRITE("ioctl(VKI_V4L2_ENUM_FREQ_BANDS).rangelow", data->rangelow);
      PRE_FIELD_WRITE("ioctl(VKI_V4L2_ENUM_FREQ_BANDS).rangehigh", data->rangehigh);
      PRE_FIELD_WRITE("ioctl(VKI_V4L2_ENUM_FREQ_BANDS).modulation", data->modulation);
      break;
   }
   case VKI_V4L2_DBG_G_CHIP_INFO: {
      struct vki_v4l2_dbg_chip_info *data = (struct vki_v4l2_dbg_chip_info *)ARG3;
      PRE_FIELD_READ("ioctl(VKI_V4L2_DBG_G_CHIP_INFO).match.type", data->match.type);
      PRE_FIELD_READ("ioctl(VKI_V4L2_DBG_G_CHIP_INFO).match.addr", data->match.addr);
      PRE_FIELD_WRITE("ioctl(VKI_V4L2_DBG_G_CHIP_INFO).name", data->name);
      PRE_FIELD_WRITE("ioctl(VKI_V4L2_DBG_G_CHIP_INFO).flags", data->flags);
      PRE_FIELD_READ("ioctl(VKI_V4L2_DBG_G_CHIP_INFO).reserved", data->reserved);
      break;
   }
   case VKI_V4L2_QUERY_EXT_CTRL: {
      struct vki_v4l2_query_ext_ctrl *data = (struct vki_v4l2_query_ext_ctrl *)ARG3;
      PRE_FIELD_READ("ioctl(VKI_V4L2_QUERY_EXT_CTRL).id", data->id);
      PRE_FIELD_READ("ioctl(VKI_V4L2_QUERY_EXT_CTRL).reserved", data->reserved);
      PRE_MEM_WRITE("ioctl(VKI_V4L2_QUERY_EXT_CTRL)", (Addr)&data->type,
            sizeof(*data) - sizeof(data->id) - sizeof(data->reserved));
      break;
   }
   case VKI_V4L2_SUBDEV_G_FMT: {
      struct vki_v4l2_subdev_format *data = (struct vki_v4l2_subdev_format *)ARG3;
      PRE_FIELD_READ("ioctl(VKI_V4L2_SUBDEV_G_FMT).pad", data->pad);
      PRE_FIELD_READ("ioctl(VKI_V4L2_SUBDEV_G_FMT).which", data->which);
      PRE_FIELD_READ("ioctl(VKI_V4L2_SUBDEV_G_FMT).reserved", data->reserved);
      PRE_FIELD_WRITE("ioctl(VKI_V4L2_SUBDEV_G_FMT).format", data->format);
      break;
   }
   case VKI_V4L2_SUBDEV_S_FMT: {
      struct vki_v4l2_subdev_format *data = (struct vki_v4l2_subdev_format *)ARG3;
      PRE_MEM_READ("ioctl(VKI_V4L2_SUBDEV_S_FMT)", (Addr)data, sizeof(*data));
      break;
   }
   case VKI_V4L2_SUBDEV_G_FRAME_INTERVAL: {
      struct vki_v4l2_subdev_frame_interval *data = (struct vki_v4l2_subdev_frame_interval *)ARG3;
      PRE_FIELD_READ("ioctl(VKI_V4L2_SUBDEV_G_FRAME_SIZE).pad", data->pad);
      PRE_FIELD_READ("ioctl(VKI_V4L2_SUBDEV_G_FRAME_SIZE).reserved", data->reserved);
      PRE_FIELD_WRITE("ioctl(VKI_V4L2_SUBDEV_G_FRAME_SIZE).interval", data->interval);
      break;
   }
   case VKI_V4L2_SUBDEV_S_FRAME_INTERVAL: {
      struct vki_v4l2_subdev_frame_interval *data = (struct vki_v4l2_subdev_frame_interval *)ARG3;
      PRE_MEM_READ("ioctl(VKI_V4L2_SUBDEV_S_FRAME_INTERVAL)", (Addr)data, sizeof(*data));
      break;
   }
   case VKI_V4L2_SUBDEV_ENUM_MBUS_CODE: {
      struct vki_v4l2_subdev_mbus_code_enum *data = (struct vki_v4l2_subdev_mbus_code_enum *)ARG3;
      PRE_FIELD_READ("ioctl(VKI_V4L2_SUBDEV_ENUM_MBUS_CODE).index", data->index);
      PRE_FIELD_READ("ioctl(VKI_V4L2_SUBDEV_ENUM_MBUS_CODE).pad", data->pad);
      PRE_FIELD_WRITE("ioctl(VKI_V4L2_SUBDEV_ENUM_MBUS_CODE).code", data->code);
      PRE_FIELD_READ("ioctl(VKI_V4L2_SUBDEV_ENUM_MBUS_CODE).which", data->which);
      PRE_FIELD_READ("ioctl(VKI_V4L2_SUBDEV_ENUM_MBUS_CODE).reserved", data->reserved);
      break;
   }
   case VKI_V4L2_SUBDEV_ENUM_FRAME_SIZE: {
      struct vki_v4l2_subdev_frame_size_enum *data = (struct vki_v4l2_subdev_frame_size_enum *)ARG3;
      PRE_FIELD_READ("ioctl(VKI_V4L2_SUBDEV_ENUM_FRAME_SIZE).index", data->index);
      PRE_FIELD_READ("ioctl(VKI_V4L2_SUBDEV_ENUM_FRAME_SIZE).pad", data->pad);
      PRE_FIELD_READ("ioctl(VKI_V4L2_SUBDEV_ENUM_FRAME_SIZE).code", data->code);
      PRE_FIELD_READ("ioctl(VKI_V4L2_SUBDEV_ENUM_FRAME_SIZE).which", data->which);
      PRE_FIELD_READ("ioctl(VKI_V4L2_SUBDEV_ENUM_FRAME_SIZE).reserved", data->reserved);
      PRE_FIELD_WRITE("ioctl(VKI_V4L2_SUBDEV_ENUM_FRAME_SIZE).min_width", data->min_width);
      PRE_FIELD_WRITE("ioctl(VKI_V4L2_SUBDEV_ENUM_FRAME_SIZE).min_height", data->min_height);
      PRE_FIELD_WRITE("ioctl(VKI_V4L2_SUBDEV_ENUM_FRAME_SIZE).max_width", data->max_width);
      PRE_FIELD_WRITE("ioctl(VKI_V4L2_SUBDEV_ENUM_FRAME_SIZE).max_height", data->max_height);
      break;
   }
   case VKI_V4L2_SUBDEV_ENUM_FRAME_INTERVAL: {
      struct vki_v4l2_subdev_frame_interval_enum *data = (struct vki_v4l2_subdev_frame_interval_enum *)ARG3;
      PRE_FIELD_READ("ioctl(VKI_V4L2_SUBDEV_ENUM_FRAME_INTERVAL).index", data->index);
      PRE_FIELD_READ("ioctl(VKI_V4L2_SUBDEV_ENUM_FRAME_INTERVAL).pad", data->pad);
      PRE_FIELD_READ("ioctl(VKI_V4L2_SUBDEV_ENUM_FRAME_INTERVAL).code", data->code);
      PRE_FIELD_READ("ioctl(VKI_V4L2_SUBDEV_ENUM_FRAME_INTERVAL).width", data->width);
      PRE_FIELD_READ("ioctl(VKI_V4L2_SUBDEV_ENUM_FRAME_INTERVAL).height", data->height);
      PRE_FIELD_READ("ioctl(VKI_V4L2_SUBDEV_ENUM_FRAME_INTERVAL).which", data->which);
      PRE_FIELD_READ("ioctl(VKI_V4L2_SUBDEV_ENUM_FRAME_INTERVAL).reserved", data->reserved);
      PRE_FIELD_WRITE("ioctl(VKI_V4L2_SUBDEV_ENUM_FRAME_INTERVAL).interval", data->interval);
      break;
   }
   case VKI_V4L2_SUBDEV_G_CROP: {
      struct vki_v4l2_subdev_crop *data = (struct vki_v4l2_subdev_crop *)ARG3;
      PRE_FIELD_READ("ioctl(VKI_V4L2_SUBDEV_G_CROP).pad", data->pad);
      PRE_FIELD_READ("ioctl(VKI_V4L2_SUBDEV_G_CROP).which", data->which);
      PRE_FIELD_READ("ioctl(VKI_V4L2_SUBDEV_G_CROP).reserved", data->reserved);
      PRE_FIELD_WRITE("ioctl(VKI_V4L2_SUBDEV_G_CROP).rect", data->rect);
      break;
   }
   case VKI_V4L2_SUBDEV_S_CROP: {
      struct vki_v4l2_subdev_crop *data = (struct vki_v4l2_subdev_crop *)ARG3;
      PRE_MEM_READ("ioctl(VKI_V4L2_SUBDEV_S_CROP)", (Addr)data, sizeof(*data));
      break;
   }
   case VKI_V4L2_SUBDEV_G_SELECTION: {
      struct vki_v4l2_subdev_selection *data = (struct vki_v4l2_subdev_selection *)ARG3;
      PRE_FIELD_READ("ioctl(VKI_V4L2_SUBDEV_G_SELECTION).pad", data->pad);
      PRE_FIELD_READ("ioctl(VKI_V4L2_SUBDEV_G_SELECTION).which", data->which);
      PRE_FIELD_READ("ioctl(VKI_V4L2_SUBDEV_G_SELECTION).target", data->target);
      PRE_FIELD_READ("ioctl(VKI_V4L2_SUBDEV_G_SELECTION).flags", data->flags);
      PRE_FIELD_READ("ioctl(VKI_V4L2_SUBDEV_G_SELECTION).reserved", data->reserved);
      PRE_FIELD_WRITE("ioctl(VKI_V4L2_SUBDEV_G_SELECTION).r", data->r);
      break;
   }
   case VKI_V4L2_SUBDEV_S_SELECTION: {
      struct vki_v4l2_subdev_selection *data = (struct vki_v4l2_subdev_selection *)ARG3;
      PRE_MEM_READ("ioctl(VKI_V4L2_SUBDEV_S_SELECTION)", (Addr)data, sizeof(*data));
      break;
   }
   case VKI_MEDIA_IOC_DEVICE_INFO: {
      struct vki_media_device_info *data = (struct vki_media_device_info *)ARG3;
      PRE_FIELD_READ("ioctl(VKI_MEDIA_IOC_DEVICE_INFO).reserved", data->reserved);
      PRE_MEM_WRITE("ioctl(VKI_MEDIA_IOC_DEVICE_INFO)",
            (Addr)data, sizeof(*data) - sizeof(data->reserved));
      break;
   }
   case VKI_MEDIA_IOC_ENUM_ENTITIES: {
      struct vki_media_entity_desc *data = (struct vki_media_entity_desc *)ARG3;
      PRE_FIELD_READ("ioctl(VKI_MEDIA_IOC_ENUM_ENTITIES).id", data->id);
      PRE_MEM_WRITE("ioctl(VKI_MEDIA_IOC_ENUM_ENTITIES)",
            (Addr)data->name, sizeof(*data) - sizeof(data->id));
      break;
   }
   case VKI_MEDIA_IOC_ENUM_LINKS: {
      struct vki_media_links_enum *data = (struct vki_media_links_enum *)ARG3;
      PRE_MEM_READ("ioctl(VKI_MEDIA_IOC_ENUM_LINKS)", (Addr)data, sizeof(*data));
      break;
   }
   case VKI_MEDIA_IOC_SETUP_LINK: {
      struct vki_media_link_desc *data = (struct vki_media_link_desc *)ARG3;
      PRE_MEM_READ("ioctl(VKI_MEDIA_IOC_SETUP_LINK)", (Addr)data, sizeof(*data));
      break;
   }

   /* Serial */
   case VKI_TIOCGSERIAL: {
      struct vki_serial_struct *data = (struct vki_serial_struct *)ARG3;
      PRE_MEM_WRITE("ioctl(VKI_TIOCGSERIAL)", (Addr)data, sizeof(*data));
      break;
   }
   case VKI_TIOCSSERIAL: {
      struct vki_serial_struct *data = (struct vki_serial_struct *)ARG3;
      PRE_MEM_READ("ioctl(VKI_TIOCSSERIAL)", (Addr)data, sizeof(*data));
      break;
   }

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

   ARG2 = (UInt)ARG2;

   /* --- BEGIN special IOCTL handlers for specific Android hardware --- */

   /* BEGIN undocumented ioctls for PowerVR SGX 540 (the GPU on Nexus S) */
   if (KernelVariantiS(KernelVariant_android_gpu_sgx5xx,
                       VG_(clo_kernel_variant))) {

      if (ARG2 >= 0xC01C6700 && ARG2 <= 0xC01C67FF && ARG3 >= 0x1000) {
         /* What's going on here: there appear to be a bunch of ioctls
            of the form 0xC01C67xx which are undocumented, and if
            unhandled give rise to a vast number of false positives in
            Memcheck.

            The "normal" interpretation of an ioctl of this form would
            be that the 3rd arg is a pointer to an area of size 0x1C
            (28 bytes) which is filled in by the kernel.  Hence you
            might think that "POST_MEM_WRITE(ARG3, 28)" would fix it.
            But it doesn't.

            It requires POST_MEM_WRITE(ARG3, 256) to silence them.
            One interpretation of this is that ARG3 really does point
            to a 28 byte struct, but inside that are pointers to other
            areas also filled in by the kernel.  If these happen to be
            allocated just back up the stack then the 256 byte paint
            might cover them too, somewhat indiscriminately.

            By printing out ARG3 and also the 28 bytes that it points
            at, it's possible to guess that the 7 word structure has
            this form

              0            1    2    3        4    5        6           
              ioctl-number 0x1C ptr1 ptr1size ptr2 ptr2size aBitMask

            Unfortunately that doesn't seem to work for some reason,
            so stay with the blunt-instrument approach for the time
            being.
         */
         if (1) {
            /* blunt-instrument approach */
            POST_MEM_WRITE(ARG3, 256);
         } else {
            /* be a bit more sophisticated */
            POST_MEM_WRITE(ARG3, 28);
            UInt* word = (UInt*)ARG3;
            if (word && word[2] && word[3] < 0x200/*stay sane*/)
               POST_MEM_WRITE(word[2], word[3]); // "ptr1"
            if (word && word[4] && word[5] < 0x200/*stay sane*/)
               POST_MEM_WRITE(word[4], word[5]); // "ptr2"
         }
         goto post_sys_ioctl__out;
      }
   }
   /* END undocumented ioctls for PowerVR SGX 540 (the GPU on Nexus S) */

   /* BEGIN undocumented ioctls for Qualcomm Adreno 3xx */
   if (KernelVariantiS(KernelVariant_android_gpu_adreno3xx,
                       VG_(clo_kernel_variant))) {
     if (ARG2 == 0xC00C0902) {
         POST_MEM_WRITE(ARG3, 24); // 16 is not enough
         goto post_sys_ioctl__out;
     }
   }
   /* END undocumented ioctls for Qualcomm Adreno 3xx */

   /* --- END special IOCTL handlers for specific Android hardware --- */

   /* --- normal handling --- */
   switch (ARG2 /* request */) {

   /* The Linux kernel "ion" memory allocator, used on Android.  Note:
      this is pretty poor given that there's no pre-handling to check
      that writable areas are addressable. */
   case VKI_ION_IOC_ALLOC: {
      struct vki_ion_allocation_data* data
         = (struct vki_ion_allocation_data*)ARG3;
      POST_FIELD_WRITE(data->handle);
      break;
   }
   case VKI_ION_IOC_MAP: {
      struct vki_ion_fd_data* data = (struct vki_ion_fd_data*)ARG3;
      POST_FIELD_WRITE(data->fd);
      break;
   }
   case VKI_ION_IOC_FREE: // is this necessary?
      POST_MEM_WRITE(ARG3, sizeof(struct vki_ion_handle_data));
      break;
   case VKI_ION_IOC_SHARE:
      break;
   case VKI_ION_IOC_IMPORT: {
      struct vki_ion_fd_data* data = (struct vki_ion_fd_data*)ARG3;
      POST_FIELD_WRITE(data->handle);
      break;
   }
   case VKI_ION_IOC_SYNC:
      break;
   case VKI_ION_IOC_CUSTOM: // is this necessary?
      POST_MEM_WRITE(ARG3, sizeof(struct vki_ion_custom_data));
      break;

   case VKI_SYNC_IOC_MERGE: {
      struct vki_sync_merge_data* data = (struct vki_sync_merge_data*)ARG3;
      POST_FIELD_WRITE(data->fence);
      break;
   }

   case VKI_TCSETS:
   case VKI_TCSETSW:
   case VKI_TCSETSF:
   case VKI_IB_USER_MAD_ENABLE_PKEY:
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
   case VKI_TIOCSIG:
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
   case VKI_FIONCLEX:
      break;
   case VKI_FIOCLEX:
      break;
   case VKI_TIOCNOTTY:
      break;
   case VKI_FIOASYNC:
      break;
   case VKI_FIONREAD:                /* identical to SIOCINQ */
      POST_MEM_WRITE( ARG3, sizeof(int) );
      break;
   case VKI_FIOQSIZE:
      POST_MEM_WRITE( ARG3, sizeof(vki_loff_t) );
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
      {
         vki_sg_io_hdr_t *sgio = (vki_sg_io_hdr_t*)ARG3;
         if ( sgio->sbp ) {
            POST_MEM_WRITE( (Addr)sgio->sbp, sgio->sb_len_wr );
         }
         if ( sgio->dxfer_direction == VKI_SG_DXFER_FROM_DEV ||
              sgio->dxfer_direction == VKI_SG_DXFER_TO_FROM_DEV ) {
            int transferred = sgio->dxfer_len - sgio->resid;
            POST_MEM_WRITE( (Addr)sgio->dxferp, transferred );
         }
      }
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
   case VKI_SIOCETHTOOL: {       /* ethtool(8) interface         */
      struct vki_ifreq *ir = (struct vki_ifreq *)ARG3;
      switch ( *(vki_u32 *)ir->vki_ifr_data ) {
      case VKI_ETHTOOL_GSET:
         POST_MEM_WRITE( (Addr)ir->vki_ifr_data, sizeof(struct vki_ethtool_cmd));
         break;
      case VKI_ETHTOOL_SSET:
         break;
      case VKI_ETHTOOL_GDRVINFO:
         POST_MEM_WRITE( (Addr)ir->vki_ifr_data, sizeof(struct vki_ethtool_drvinfo) );
         break;
      case VKI_ETHTOOL_GREGS:
         POST_MEM_WRITE( (Addr)((struct vki_ethtool_regs *)ir->vki_ifr_data)->data,
                         ((struct vki_ethtool_regs *)ir->vki_ifr_data)->len );
         break;
      case VKI_ETHTOOL_GWOL:
         POST_MEM_WRITE( (Addr)ir->vki_ifr_data, sizeof(struct vki_ethtool_wolinfo) );
         break;
      case VKI_ETHTOOL_SWOL:
         break;
      case VKI_ETHTOOL_GMSGLVL:
      case VKI_ETHTOOL_GLINK:
      case VKI_ETHTOOL_GRXCSUM:
      case VKI_ETHTOOL_GSG:
      case VKI_ETHTOOL_GTSO:
      case VKI_ETHTOOL_GUFO:
      case VKI_ETHTOOL_GGSO:
      case VKI_ETHTOOL_GFLAGS:
      case VKI_ETHTOOL_GGRO:
         POST_MEM_WRITE( (Addr)ir->vki_ifr_data, sizeof(struct vki_ethtool_value));
         break;
      case VKI_ETHTOOL_SMSGLVL:
      case VKI_ETHTOOL_SRXCSUM:
      case VKI_ETHTOOL_SSG:
      case VKI_ETHTOOL_STSO:
      case VKI_ETHTOOL_SUFO:
      case VKI_ETHTOOL_SGSO:
      case VKI_ETHTOOL_SFLAGS:
      case VKI_ETHTOOL_SGRO:
         break;
      case VKI_ETHTOOL_NWAY_RST:
         break;
      case VKI_ETHTOOL_GRINGPARAM:
         POST_MEM_WRITE( (Addr)ir->vki_ifr_data, sizeof(struct vki_ethtool_ringparam));
         break;
      case VKI_ETHTOOL_SRINGPARAM:
         break;
      case VKI_ETHTOOL_TEST:
         POST_MEM_WRITE( (Addr)((struct vki_ethtool_test *)ir->vki_ifr_data)->data,
                         ((struct vki_ethtool_test *)ir->vki_ifr_data)->len * sizeof(__vki_u64) );
         break;
      case VKI_ETHTOOL_PHYS_ID:
         break;
      case VKI_ETHTOOL_GPERMADDR:
         POST_MEM_WRITE( (Addr)((struct vki_ethtool_perm_addr *)ir->vki_ifr_data)->data,
                         ((struct vki_ethtool_perm_addr *)ir->vki_ifr_data)->size );
         break;
      case VKI_ETHTOOL_RESET:
         break;
      case VKI_ETHTOOL_GSSET_INFO:
         POST_MEM_WRITE( (Addr)((struct vki_ethtool_sset_info *)ir->vki_ifr_data)->data,
                        __builtin_popcountll(((struct vki_ethtool_sset_info *)ir->vki_ifr_data)->sset_mask) * sizeof(__vki_u32) );
         break;
      case VKI_ETHTOOL_GFEATURES:
         POST_MEM_WRITE( (Addr)((struct vki_ethtool_gfeatures *)ir->vki_ifr_data)->features,
                         ((struct vki_ethtool_gfeatures *)ir->vki_ifr_data)->size * sizeof(struct vki_ethtool_get_features_block) );
         break;
      case VKI_ETHTOOL_SFEATURES:
         break;
      case VKI_ETHTOOL_GCHANNELS:
         POST_MEM_WRITE( (Addr)ir->vki_ifr_data, sizeof(struct vki_ethtool_channels) );
         break;
      case VKI_ETHTOOL_SCHANNELS:
         break;
      case VKI_ETHTOOL_GET_TS_INFO:
         POST_MEM_WRITE( (Addr)ir->vki_ifr_data, sizeof(struct vki_ethtool_ts_info) );
         break;
      }
      break;
   }
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

      /* tun/tap related ioctls */
   case VKI_TUNSETIFF:
      POST_MEM_WRITE( (Addr)&((struct vki_ifreq *)ARG3)->vki_ifr_name,
                      sizeof(((struct vki_ifreq *)ARG3)->vki_ifr_name) );
      break;
   case VKI_TUNGETFEATURES:
      POST_MEM_WRITE( ARG3, sizeof(unsigned int) );
      break;
   case VKI_TUNGETIFF:
      POST_MEM_WRITE( (Addr)&((struct vki_ifreq *)ARG3)->vki_ifr_name,
                      sizeof(((struct vki_ifreq *)ARG3)->vki_ifr_name) );
      POST_MEM_WRITE( (Addr)&((struct vki_ifreq *)ARG3)->vki_ifr_flags,
                      sizeof(((struct vki_ifreq *)ARG3)->vki_ifr_flags) );
      break;
   case VKI_TUNGETSNDBUF:
      POST_MEM_WRITE( ARG3, sizeof(int) );
      break;
   case VKI_TUNGETVNETHDRSZ:
      POST_MEM_WRITE( ARG3, sizeof(int) );
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
   case VKI_SIOCSHWTSTAMP:       /* Set hardware time stamping   */
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

   case VKI_SIOCATMARK:
      POST_MEM_WRITE(ARG3, sizeof(int));
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
   case VKI_SNDRV_CTL_IOCTL_PVERSION: {
      POST_MEM_WRITE( (Addr)ARG3, sizeof(int) );
      break;
   }
   case VKI_SNDRV_CTL_IOCTL_CARD_INFO:
      POST_MEM_WRITE( (Addr)ARG3, sizeof(struct vki_snd_ctl_card_info) );
      break;
   case VKI_SNDRV_CTL_IOCTL_ELEM_LIST: {
      struct vki_snd_ctl_elem_list *data = (struct vki_snd_ctl_elem_list *)ARG3;
      POST_MEM_WRITE( (Addr)&data->used, sizeof(data->used) );
      POST_MEM_WRITE( (Addr)&data->count, sizeof(data->count) );
      if (data->pids) {
         POST_MEM_WRITE( (Addr)data->pids, sizeof(struct vki_snd_ctl_elem_id) * data->used );
      }
      break;
   }
   case VKI_SNDRV_CTL_IOCTL_TLV_READ: {
      struct vki_snd_ctl_tlv *data = (struct vki_snd_ctl_tlv *)ARG3;
      POST_MEM_WRITE( (Addr)data->tlv, data->length );
      break;
   }
   case VKI_SNDRV_CTL_IOCTL_TLV_WRITE:
   case VKI_SNDRV_CTL_IOCTL_TLV_COMMAND:
      break;

      /* SCSI no operand */
   case VKI_SCSI_IOCTL_DOORLOCK:
   case VKI_SCSI_IOCTL_DOORUNLOCK:
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
   case VKI_BLKPBSZGET:
      POST_MEM_WRITE(ARG3, sizeof(int));
      break;
   case VKI_BLKDISCARDZEROES:
      POST_MEM_WRITE(ARG3, sizeof(vki_uint));
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

      /* SCSI */
   case VKI_SCSI_IOCTL_GET_IDLUN: /* 0x5382 */
      POST_MEM_WRITE(ARG3, sizeof(struct vki_scsi_idlun));
      break;
   case VKI_SCSI_IOCTL_GET_BUS_NUMBER: /* 0x5386 */
      POST_MEM_WRITE(ARG3, sizeof(int));
      break;

      /* CD ROM stuff (??)  */
   case VKI_CDROM_DISC_STATUS:
      break;
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
   case VKI_CDROM_GET_CAPABILITY: /* 0x5331 */
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
      }
      break;
   case VKI_USBDEVFS_BULK:
      if ( ARG3 ) {
         struct vki_usbdevfs_bulktransfer *vkub = (struct vki_usbdevfs_bulktransfer *)ARG3;
         if (vkub->ep & 0x80)
            POST_MEM_WRITE((Addr)vkub->data, RES);
      }
      break;
   case VKI_USBDEVFS_GETDRIVER:
      if ( ARG3 ) {
         struct vki_usbdevfs_getdriver *vkugd = (struct vki_usbdevfs_getdriver *)ARG3;
         POST_MEM_WRITE((Addr)&vkugd->driver, sizeof(vkugd->driver));
      }
      break;
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
      }
      break;
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
   case VKI_I2C_RDWR:
      if ( ARG3 ) {
          struct vki_i2c_rdwr_ioctl_data *vkui = (struct vki_i2c_rdwr_ioctl_data *)ARG3;
          UInt i;
          for (i=0; i < vkui->nmsgs; i++) {
              struct vki_i2c_msg *msg = vkui->msgs + i;
              if (msg->flags & VKI_I2C_M_RD) 
                  POST_MEM_WRITE((Addr)msg->buf, msg->len);
          }
      }
      break;
   case VKI_I2C_SMBUS:
       if ( ARG3 ) {
            struct vki_i2c_smbus_ioctl_data *vkis
               = (struct vki_i2c_smbus_ioctl_data *) ARG3;
            /* i2c_smbus_write_quick hides its value in read_write, so
               this variable can have a different meaning */
            if ((vkis->read_write == VKI_I2C_SMBUS_READ)
                || (vkis->size == VKI_I2C_SMBUS_PROC_CALL)
                || (vkis->size == VKI_I2C_SMBUS_BLOCK_PROC_CALL)) {
                if ( ! (vkis->size == VKI_I2C_SMBUS_QUICK)) {
                    UInt size;
                    switch(vkis->size) {
                        case VKI_I2C_SMBUS_BYTE:
                        case VKI_I2C_SMBUS_BYTE_DATA:
                            size = 1;
                            break;
                        case VKI_I2C_SMBUS_WORD_DATA:
                        case VKI_I2C_SMBUS_PROC_CALL:
                            size = 2;
                            break;
                        case VKI_I2C_SMBUS_BLOCK_DATA:
                        case VKI_I2C_SMBUS_I2C_BLOCK_BROKEN:
                        case VKI_I2C_SMBUS_BLOCK_PROC_CALL:
                        case VKI_I2C_SMBUS_I2C_BLOCK_DATA:
                            size = 1 + vkis->data->block[0];
                            break;
                        default:
                            size = 0;
                    }
                    POST_MEM_WRITE((Addr)&vkis->data->block[0], size);
                }
            }
       }
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

#  if defined(VGPV_arm_linux_android) || defined(VGPV_x86_linux_android) \
      || defined(VGPV_mips32_linux_android)
   /* ashmem */
   case VKI_ASHMEM_GET_SIZE:
   case VKI_ASHMEM_SET_SIZE:
   case VKI_ASHMEM_GET_PROT_MASK:
   case VKI_ASHMEM_SET_PROT_MASK:
   case VKI_ASHMEM_GET_PIN_STATUS:
   case VKI_ASHMEM_PURGE_ALL_CACHES:
   case VKI_ASHMEM_SET_NAME:
   case VKI_ASHMEM_PIN:
   case VKI_ASHMEM_UNPIN:
       break;
   case VKI_ASHMEM_GET_NAME:
       POST_MEM_WRITE( ARG3, VKI_ASHMEM_NAME_LEN );
       break;

   /* binder */
   case VKI_BINDER_WRITE_READ:
       if (ARG3) {
           struct vki_binder_write_read* bwr
              = (struct vki_binder_write_read*)ARG3;
           POST_FIELD_WRITE(bwr->write_consumed);
           POST_FIELD_WRITE(bwr->read_consumed);

           if (bwr->read_size)
               POST_MEM_WRITE((Addr)bwr->read_buffer, bwr->read_consumed);
       }
       break;

   case VKI_BINDER_SET_IDLE_TIMEOUT:
   case VKI_BINDER_SET_MAX_THREADS:
   case VKI_BINDER_SET_IDLE_PRIORITY:
   case VKI_BINDER_SET_CONTEXT_MGR:
   case VKI_BINDER_THREAD_EXIT:
       break;
   case VKI_BINDER_VERSION:
       if (ARG3) {
           struct vki_binder_version* bv = (struct vki_binder_version*)ARG3;
           POST_FIELD_WRITE(bv->protocol_version);
       }
       break;
#  endif /* defined(VGPV_*_linux_android) */

   case VKI_HCIGETDEVLIST:
      if (ARG3) {
        struct vki_hci_dev_list_req* dlr = (struct vki_hci_dev_list_req*)ARG3;
        POST_MEM_WRITE((Addr)ARG3 + sizeof(struct vki_hci_dev_list_req),
                       dlr->dev_num * sizeof(struct vki_hci_dev_req));
      }
      break;

   case VKI_HCIINQUIRY:
      if (ARG3) {
        struct vki_hci_inquiry_req* ir = (struct vki_hci_inquiry_req*)ARG3;
        POST_MEM_WRITE((Addr)ARG3 + sizeof(struct vki_hci_inquiry_req),
                       ir->num_rsp * sizeof(struct vki_inquiry_info));
      }
      break;

   case VKI_DRM_IOCTL_VERSION:
      if (ARG3) {
         struct vki_drm_version *data = (struct vki_drm_version *)ARG3;
	 POST_MEM_WRITE((Addr)&data->version_major, sizeof(data->version_major));
         POST_MEM_WRITE((Addr)&data->version_minor, sizeof(data->version_minor));
         POST_MEM_WRITE((Addr)&data->version_patchlevel, sizeof(data->version_patchlevel));
         POST_MEM_WRITE((Addr)&data->name_len, sizeof(data->name_len));
         POST_MEM_WRITE((Addr)data->name, data->name_len);
         POST_MEM_WRITE((Addr)&data->date_len, sizeof(data->date_len));
         POST_MEM_WRITE((Addr)data->date, data->date_len);
         POST_MEM_WRITE((Addr)&data->desc_len, sizeof(data->desc_len));
         POST_MEM_WRITE((Addr)data->desc, data->desc_len);
      }
      break;
   case VKI_DRM_IOCTL_GET_UNIQUE:
      if (ARG3) {
         struct vki_drm_unique *data = (struct vki_drm_unique *)ARG3;
	 POST_MEM_WRITE((Addr)data->unique, sizeof(data->unique_len));
      }
      break;
   case VKI_DRM_IOCTL_GET_MAGIC:
      if (ARG3) {
         struct vki_drm_auth *data = (struct vki_drm_auth *)ARG3;
         POST_MEM_WRITE((Addr)&data->magic, sizeof(data->magic));
      }
      break;
   case VKI_DRM_IOCTL_WAIT_VBLANK:
      if (ARG3) {
         union vki_drm_wait_vblank *data = (union vki_drm_wait_vblank *)ARG3;
         POST_MEM_WRITE((Addr)&data->reply, sizeof(data->reply));
      }
      break;
   case VKI_DRM_IOCTL_GEM_FLINK:
      if (ARG3) {
         struct vki_drm_gem_flink *data = (struct vki_drm_gem_flink *)ARG3;
         POST_MEM_WRITE((Addr)&data->name, sizeof(data->name));
      }
      break;
   case VKI_DRM_IOCTL_GEM_OPEN:
      if (ARG3) {
         struct vki_drm_gem_open *data = (struct vki_drm_gem_open *)ARG3;
	 POST_MEM_WRITE((Addr)&data->handle, sizeof(data->handle));
	 POST_MEM_WRITE((Addr)&data->size, sizeof(data->size));
      }
      break;
   case VKI_DRM_IOCTL_I915_GETPARAM:
      if (ARG3) {
         vki_drm_i915_getparam_t *data = (vki_drm_i915_getparam_t *)ARG3;
	 POST_MEM_WRITE((Addr)data->value, sizeof(int));
      }
      break;
   case VKI_DRM_IOCTL_I915_GEM_BUSY:
      if (ARG3) {
         struct vki_drm_i915_gem_busy *data = (struct vki_drm_i915_gem_busy *)ARG3;
         POST_MEM_WRITE((Addr)&data->busy, sizeof(data->busy));
      }
      break;
   case VKI_DRM_IOCTL_I915_GEM_CREATE:
      if (ARG3) {
         struct vki_drm_i915_gem_create *data = (struct vki_drm_i915_gem_create *)ARG3;
	 POST_MEM_WRITE((Addr)&data->handle, sizeof(data->handle));
      }
      break;
   case VKI_DRM_IOCTL_I915_GEM_PREAD:
      if (ARG3) {
         struct vki_drm_i915_gem_pread *data = (struct vki_drm_i915_gem_pread *)ARG3;
	 POST_MEM_WRITE((Addr)data->data_ptr, data->size);
      }
      break;
   case VKI_DRM_IOCTL_I915_GEM_MMAP_GTT:
      if (ARG3) {
         struct vki_drm_i915_gem_mmap_gtt *data = (struct vki_drm_i915_gem_mmap_gtt *)ARG3;
         POST_MEM_WRITE((Addr)&data->offset, sizeof(data->offset));
      }
      break;
   case VKI_DRM_IOCTL_I915_GEM_SET_TILING:
      if (ARG3) {
         struct vki_drm_i915_gem_set_tiling *data = (struct vki_drm_i915_gem_set_tiling *)ARG3;
         POST_MEM_WRITE((Addr)&data->tiling_mode, sizeof(data->tiling_mode));
         POST_MEM_WRITE((Addr)&data->stride, sizeof(data->stride));
         POST_MEM_WRITE((Addr)&data->swizzle_mode, sizeof(data->swizzle_mode));
      }
      break;
   case VKI_DRM_IOCTL_I915_GEM_GET_TILING:
      if (ARG3) {
         struct vki_drm_i915_gem_get_tiling *data = (struct vki_drm_i915_gem_get_tiling *)ARG3;
	 POST_MEM_WRITE((Addr)&data->tiling_mode, sizeof(data->tiling_mode));
         POST_MEM_WRITE((Addr)&data->swizzle_mode, sizeof(data->swizzle_mode));
      }
      break;
   case VKI_DRM_IOCTL_I915_GEM_GET_APERTURE:
      if (ARG3) {
         struct vki_drm_i915_gem_get_aperture *data = (struct vki_drm_i915_gem_get_aperture *)ARG3;
         POST_MEM_WRITE((Addr)&data->aper_size, sizeof(data->aper_size));
         POST_MEM_WRITE((Addr)&data->aper_available_size, sizeof(data->aper_available_size));
      }
      break;

   /* KVM ioctls that only write the system call return value */
   case VKI_KVM_GET_API_VERSION:
   case VKI_KVM_CREATE_VM:
   case VKI_KVM_CHECK_EXTENSION:
   case VKI_KVM_GET_VCPU_MMAP_SIZE:
   case VKI_KVM_S390_ENABLE_SIE:
   case VKI_KVM_CREATE_VCPU:
   case VKI_KVM_SET_TSS_ADDR:
   case VKI_KVM_CREATE_IRQCHIP:
   case VKI_KVM_RUN:
   case VKI_KVM_S390_INITIAL_RESET:
   case VKI_KVM_KVMCLOCK_CTRL:
      break;

   case VKI_KVM_S390_MEM_OP: {
      struct vki_kvm_s390_mem_op *args =
         (struct vki_kvm_s390_mem_op *)(ARG3);
      if (args->flags & VKI_KVM_S390_MEMOP_F_CHECK_ONLY)
         break;
      if (args->op == VKI_KVM_S390_MEMOP_LOGICAL_READ)
         POST_MEM_WRITE((Addr)args->buf, args->size);
      }
      break;

#ifdef ENABLE_XEN
   case VKI_XEN_IOCTL_PRIVCMD_HYPERCALL: {
       SyscallArgs harrghs;
       struct vki_xen_privcmd_hypercall *args =
          (struct vki_xen_privcmd_hypercall *)(ARG3);

       if (!args)
          break;

       VG_(memset)(&harrghs, 0, sizeof(harrghs));
       harrghs.sysno = args->op;
       harrghs.arg1 = args->arg[0];
       harrghs.arg2 = args->arg[1];
       harrghs.arg3 = args->arg[2];
       harrghs.arg4 = args->arg[3];
       harrghs.arg5 = args->arg[4];
       harrghs.arg6 = harrghs.arg7 = harrghs.arg8 = 0;

       WRAPPER_POST_NAME(xen, hypercall) (tid, &harrghs, status);
      }
      break;

   case VKI_XEN_IOCTL_PRIVCMD_MMAP:
      break;
   case VKI_XEN_IOCTL_PRIVCMD_MMAPBATCH: {
       struct vki_xen_privcmd_mmapbatch *args =
           (struct vki_xen_privcmd_mmapbatch *)(ARG3);
       POST_MEM_WRITE((Addr)args->arr, sizeof(*(args->arr)) * args->num);
      }
      break;
   case VKI_XEN_IOCTL_PRIVCMD_MMAPBATCH_V2: {
       struct vki_xen_privcmd_mmapbatch_v2 *args =
           (struct vki_xen_privcmd_mmapbatch_v2 *)(ARG3);
       POST_MEM_WRITE((Addr)args->err, sizeof(*(args->err)) * args->num);
      }
      break;

   case VKI_XEN_IOCTL_EVTCHN_BIND_VIRQ:
   case VKI_XEN_IOCTL_EVTCHN_BIND_INTERDOMAIN:
   case VKI_XEN_IOCTL_EVTCHN_BIND_UNBOUND_PORT:
   case VKI_XEN_IOCTL_EVTCHN_UNBIND:
   case VKI_XEN_IOCTL_EVTCHN_NOTIFY:
   case VKI_XEN_IOCTL_EVTCHN_RESET:
      /* No output */
      break;
#endif

   /* Lustre */
   case VKI_OBD_IOC_FID2PATH: {
       struct vki_getinfo_fid2path *args = (void *)(ARG3);
       POST_FIELD_WRITE(args->gf_recno);
       POST_FIELD_WRITE(args->gf_linkno);
       POST_MEM_WRITE((Addr)args->gf_path, VG_(strlen)(args->gf_path)+1);
       break;
      }

   case VKI_LL_IOC_PATH2FID:
       POST_MEM_WRITE(ARG3, sizeof(struct vki_lu_fid));
      break;

   case VKI_LL_IOC_GETPARENT: {
       struct vki_getparent *gp = (struct vki_getparent *)ARG3;
       POST_FIELD_WRITE(gp->gp_fid);
       POST_MEM_WRITE((Addr)gp->gp_name, VG_(strlen)(gp->gp_name)+1);
       break;
   }

   /* V4L2 */
   case VKI_V4L2_S_FMT:
   case VKI_V4L2_TRY_FMT:
   case VKI_V4L2_REQBUFS:
   case VKI_V4L2_OVERLAY:
   case VKI_V4L2_STREAMON:
   case VKI_V4L2_STREAMOFF:
   case VKI_V4L2_S_PARM:
   case VKI_V4L2_S_STD:
   case VKI_V4L2_S_FREQUENCY:
   case VKI_V4L2_S_CTRL:
   case VKI_V4L2_S_TUNER:
   case VKI_V4L2_S_AUDIO:
   case VKI_V4L2_S_INPUT:
   case VKI_V4L2_S_EDID:
   case VKI_V4L2_S_OUTPUT:
   case VKI_V4L2_S_AUDOUT:
   case VKI_V4L2_S_MODULATOR:
   case VKI_V4L2_S_JPEGCOMP:
   case VKI_V4L2_S_CROP:
   case VKI_V4L2_S_PRIORITY:
   case VKI_V4L2_S_HW_FREQ_SEEK:
   case VKI_V4L2_S_DV_TIMINGS:
   case VKI_V4L2_SUBSCRIBE_EVENT:
   case VKI_V4L2_UNSUBSCRIBE_EVENT:
   case VKI_V4L2_PREPARE_BUF:
      break;
   case VKI_V4L2_QUERYCAP: {
      struct vki_v4l2_capability *data = (struct vki_v4l2_capability *)ARG3;
      POST_MEM_WRITE((Addr)data, sizeof(*data));
      break;
   }
   case VKI_V4L2_ENUM_FMT: {
      struct vki_v4l2_fmtdesc *data = (struct vki_v4l2_fmtdesc *)ARG3;
      POST_FIELD_WRITE(data->flags);
      POST_FIELD_WRITE(data->description);
      POST_FIELD_WRITE(data->pixelformat);
      POST_FIELD_WRITE(data->reserved);
      break;
   }
   case VKI_V4L2_G_FMT: {
      struct vki_v4l2_format *data = (struct vki_v4l2_format *)ARG3;
      switch (data->type) {
      case VKI_V4L2_BUF_TYPE_VIDEO_CAPTURE:
      case VKI_V4L2_BUF_TYPE_VIDEO_OUTPUT:
         POST_FIELD_WRITE(data->fmt.pix);
         break;
      case VKI_V4L2_BUF_TYPE_VBI_CAPTURE:
      case VKI_V4L2_BUF_TYPE_VBI_OUTPUT:
         POST_FIELD_WRITE(data->fmt.vbi);
         break;
      case VKI_V4L2_BUF_TYPE_SLICED_VBI_CAPTURE:
      case VKI_V4L2_BUF_TYPE_SLICED_VBI_OUTPUT:
         POST_FIELD_WRITE(data->fmt.sliced);
         break;
      case VKI_V4L2_BUF_TYPE_VIDEO_OVERLAY:
      case VKI_V4L2_BUF_TYPE_VIDEO_OUTPUT_OVERLAY:
         POST_FIELD_WRITE(data->fmt.win);
         break;
      case VKI_V4L2_BUF_TYPE_VIDEO_CAPTURE_MPLANE:
      case VKI_V4L2_BUF_TYPE_VIDEO_OUTPUT_MPLANE:
         POST_FIELD_WRITE(data->fmt.pix_mp);
         break;
      case VKI_V4L2_BUF_TYPE_SDR_CAPTURE:
         POST_FIELD_WRITE(data->fmt.sdr);
         break;
      }
      break;
   }
   case VKI_V4L2_QUERYBUF: {
      struct vki_v4l2_buffer *data = (struct vki_v4l2_buffer *)ARG3;
      if (data->type == VKI_V4L2_BUF_TYPE_VIDEO_CAPTURE_MPLANE ||
            data->type == VKI_V4L2_BUF_TYPE_VIDEO_OUTPUT_MPLANE) {
         unsigned i;

         for (i = 0; i < data->length; i++) {
            POST_FIELD_WRITE(data->m.planes[i].bytesused);
            POST_FIELD_WRITE(data->m.planes[i].length);
            POST_FIELD_WRITE(data->m.planes[i].m);
            POST_FIELD_WRITE(data->m.planes[i].data_offset);
            POST_FIELD_WRITE(data->m.planes[i].reserved);
         }
      } else {
         POST_FIELD_WRITE(data->m);
         POST_FIELD_WRITE(data->length);
      }
      POST_FIELD_WRITE(data->bytesused);
      POST_FIELD_WRITE(data->flags);
      POST_FIELD_WRITE(data->field);
      POST_FIELD_WRITE(data->timestamp);
      POST_FIELD_WRITE(data->timecode);
      POST_FIELD_WRITE(data->sequence);
      POST_FIELD_WRITE(data->memory);
      POST_FIELD_WRITE(data->sequence);
      break;
   }
   case VKI_V4L2_G_FBUF: {
      struct vki_v4l2_framebuffer *data = (struct vki_v4l2_framebuffer *)ARG3;
      POST_MEM_WRITE((Addr)data, sizeof(*data));
      break;
   }
   case VKI_V4L2_S_FBUF: {
      struct vki_v4l2_framebuffer *data = (struct vki_v4l2_framebuffer *)ARG3;
      POST_FIELD_WRITE(data->capability);
      POST_FIELD_WRITE(data->flags);
      POST_FIELD_WRITE(data->fmt);
      break;
   }
   case VKI_V4L2_QBUF: {
      struct vki_v4l2_buffer *data = (struct vki_v4l2_buffer *)ARG3;

      if (data->type == VKI_V4L2_BUF_TYPE_VIDEO_CAPTURE_MPLANE ||
            data->type == VKI_V4L2_BUF_TYPE_VIDEO_OUTPUT_MPLANE) {
         unsigned i;

         for (i = 0; i < data->length; i++) {
            POST_FIELD_WRITE(data->m.planes[i].length);
            if (data->memory == VKI_V4L2_MEMORY_MMAP)
               POST_FIELD_WRITE(data->m.planes[i].m);
         }
      } else {
         if (data->memory == VKI_V4L2_MEMORY_MMAP)
            POST_FIELD_WRITE(data->m);
         POST_FIELD_WRITE(data->length);
      }
      break;
   }
   case VKI_V4L2_EXPBUF: {
      struct vki_v4l2_exportbuffer *data = (struct vki_v4l2_exportbuffer *)ARG3;
      POST_FIELD_WRITE(data->fd);
      break;
   }
   case VKI_V4L2_DQBUF: {
      struct vki_v4l2_buffer *data = (struct vki_v4l2_buffer *)ARG3;
      POST_FIELD_WRITE(data->index);
      POST_FIELD_WRITE(data->bytesused);
      POST_FIELD_WRITE(data->field);
      if (data->type == VKI_V4L2_BUF_TYPE_VIDEO_CAPTURE_MPLANE ||
            data->type == VKI_V4L2_BUF_TYPE_VIDEO_OUTPUT_MPLANE) {
         unsigned i;

         for (i = 0; i < data->length; i++) {
            POST_FIELD_WRITE(data->m.planes[i].bytesused);
            POST_FIELD_WRITE(data->m.planes[i].data_offset);
            POST_FIELD_WRITE(data->m.planes[i].length);
            POST_FIELD_WRITE(data->m.planes[i].m);
         }
      } else {
         POST_FIELD_WRITE(data->m);
         POST_FIELD_WRITE(data->length);
         POST_FIELD_WRITE(data->bytesused);
         POST_FIELD_WRITE(data->field);
      }
      POST_FIELD_WRITE(data->timestamp);
      POST_FIELD_WRITE(data->timecode);
      POST_FIELD_WRITE(data->sequence);
      break;
   }
   case VKI_V4L2_G_PARM: {
      struct vki_v4l2_streamparm *data = (struct vki_v4l2_streamparm *)ARG3;
      int is_output = data->type == VKI_V4L2_BUF_TYPE_VIDEO_OUTPUT ||
         data->type == VKI_V4L2_BUF_TYPE_VIDEO_OUTPUT_MPLANE ||
         data->type == VKI_V4L2_BUF_TYPE_VBI_OUTPUT ||
         data->type == VKI_V4L2_BUF_TYPE_SLICED_VBI_OUTPUT;

      if (is_output)
        POST_MEM_WRITE((Addr)&data->parm.output,
            sizeof(data->parm.output) - sizeof(data->parm.output.reserved));
      else
        POST_MEM_WRITE((Addr)&data->parm.capture,
            sizeof(data->parm.capture) - sizeof(data->parm.capture.reserved));
      break;
   }
   case VKI_V4L2_G_STD: {
      vki_v4l2_std_id *data = (vki_v4l2_std_id *)ARG3;
      POST_MEM_WRITE((Addr)data, sizeof(*data));
      break;
   }
   case VKI_V4L2_ENUMSTD: {
      struct vki_v4l2_standard *data = (struct vki_v4l2_standard *)ARG3;
      POST_MEM_WRITE((Addr)&data->id, sizeof(*data) - sizeof(data->index));
      break;
   }
   case VKI_V4L2_ENUMINPUT: {
      struct vki_v4l2_input *data = (struct vki_v4l2_input *)ARG3;
      POST_MEM_WRITE((Addr)data->name, sizeof(*data) - sizeof(data->index));
      break;
   }
   case VKI_V4L2_G_CTRL: {
      struct vki_v4l2_control *data = (struct vki_v4l2_control *)ARG3;
      POST_FIELD_WRITE(data->value);
      break;
   }
   case VKI_V4L2_G_TUNER: {
      struct vki_v4l2_tuner *data = (struct vki_v4l2_tuner *)ARG3;
      POST_MEM_WRITE((Addr)data->name,
            sizeof(*data) - sizeof(data->index) - sizeof(data->reserved));
      break;
   }
   case VKI_V4L2_G_AUDIO: {
      struct vki_v4l2_audio *data = (struct vki_v4l2_audio *)ARG3;
      POST_MEM_WRITE((Addr)data,
            sizeof(*data) - sizeof(data->reserved));
      break;
   }
   case VKI_V4L2_QUERYCTRL: {
      struct vki_v4l2_queryctrl *data = (struct vki_v4l2_queryctrl *)ARG3;
      POST_MEM_WRITE((Addr)&data->type,
            sizeof(*data) - sizeof(data->id));
      break;
   }
   case VKI_V4L2_QUERYMENU: {
      struct vki_v4l2_querymenu *data = (struct vki_v4l2_querymenu *)ARG3;
      POST_MEM_WRITE((Addr)data->name,
            sizeof(*data) - sizeof(data->id) - sizeof(data->index));
      break;
   }
   case VKI_V4L2_G_INPUT: {
      int *data = (int *)ARG3;
      POST_MEM_WRITE((Addr)data, sizeof(*data));
      break;
   }
   case VKI_V4L2_G_EDID: {
      struct vki_v4l2_edid *data = (struct vki_v4l2_edid *)ARG3;
      if (data->blocks && data->edid)
         POST_MEM_WRITE((Addr)data->edid, data->blocks * 128);
      break;
   }
   case VKI_V4L2_G_OUTPUT: {
      int *data = (int *)ARG3;
      POST_MEM_WRITE((Addr)data, sizeof(*data));
      break;
   }
   case VKI_V4L2_ENUMOUTPUT: {
      struct vki_v4l2_output *data = (struct vki_v4l2_output *)ARG3;
      POST_MEM_WRITE((Addr)data->name, sizeof(*data) - sizeof(data->index));
      break;
   }
   case VKI_V4L2_G_AUDOUT: {
      struct vki_v4l2_audioout *data = (struct vki_v4l2_audioout *)ARG3;
      POST_MEM_WRITE((Addr)data,
            sizeof(*data) - sizeof(data->reserved));
      break;
   }
   case VKI_V4L2_G_MODULATOR: {
      struct vki_v4l2_modulator *data = (struct vki_v4l2_modulator *)ARG3;
      POST_MEM_WRITE((Addr)data->name,
            sizeof(*data) - sizeof(data->index) - sizeof(data->reserved));
      break;
   }
   case VKI_V4L2_G_FREQUENCY: {
      struct vki_v4l2_frequency *data = (struct vki_v4l2_frequency *)ARG3;
      POST_FIELD_WRITE(data->type);
      POST_FIELD_WRITE(data->frequency);
      break;
   }
   case VKI_V4L2_CROPCAP: {
      struct vki_v4l2_cropcap *data = (struct vki_v4l2_cropcap *)ARG3;
      POST_MEM_WRITE((Addr)&data->bounds, sizeof(*data) - sizeof(data->type));
      break;
   }
   case VKI_V4L2_G_CROP: {
      struct vki_v4l2_crop *data = (struct vki_v4l2_crop *)ARG3;
      POST_FIELD_WRITE(data->c);
      break;
   }
   case VKI_V4L2_G_JPEGCOMP: {
      struct vki_v4l2_jpegcompression *data = (struct vki_v4l2_jpegcompression *)ARG3;
      POST_MEM_WRITE((Addr)data, sizeof(*data));
      break;
   }
   case VKI_V4L2_QUERYSTD: {
      vki_v4l2_std_id *data = (vki_v4l2_std_id *)ARG3;
      POST_MEM_WRITE((Addr)data, sizeof(*data));
      break;
   }
   case VKI_V4L2_ENUMAUDIO: {
      struct vki_v4l2_audio *data = (struct vki_v4l2_audio *)ARG3;
      POST_MEM_WRITE((Addr)data->name,
            sizeof(*data) - sizeof(data->index) - sizeof(data->reserved));
      break;
   }
   case VKI_V4L2_ENUMAUDOUT: {
      struct vki_v4l2_audioout *data = (struct vki_v4l2_audioout *)ARG3;
      POST_MEM_WRITE((Addr)data->name,
            sizeof(*data) - sizeof(data->index) - sizeof(data->reserved));
      break;
   }
   case VKI_V4L2_G_PRIORITY: {
      __vki_u32 *data = (__vki_u32 *)ARG3;
      POST_MEM_WRITE((Addr)data, sizeof(*data));
      break;
   }
   case VKI_V4L2_G_SLICED_VBI_CAP: {
      struct vki_v4l2_sliced_vbi_cap *data = (struct vki_v4l2_sliced_vbi_cap *)ARG3;
      POST_MEM_WRITE((Addr)data,
            sizeof(*data) - sizeof(data->type) - sizeof(data->reserved));
      break;
   }
   case VKI_V4L2_G_EXT_CTRLS: {
      struct vki_v4l2_ext_controls *data = (struct vki_v4l2_ext_controls *)ARG3;
      if (data->count) {
         unsigned i;

         for (i = 0; i < data->count; i++) {
            if (data->controls[i].size)
               POST_MEM_WRITE((Addr)data->controls[i].ptr, data->controls[i].size);
            else
               POST_FIELD_WRITE(data->controls[i].value64);
         }
      }
      POST_FIELD_WRITE(data->error_idx);
      break;
   }
   case VKI_V4L2_S_EXT_CTRLS: {
      struct vki_v4l2_ext_controls *data = (struct vki_v4l2_ext_controls *)ARG3;
      POST_FIELD_WRITE(data->error_idx);
      break;
   }
   case VKI_V4L2_TRY_EXT_CTRLS: {
      struct vki_v4l2_ext_controls *data = (struct vki_v4l2_ext_controls *)ARG3;
      POST_FIELD_WRITE(data->error_idx);
      break;
   }
   case VKI_V4L2_ENUM_FRAMESIZES: {
      struct vki_v4l2_frmsizeenum *data = (struct vki_v4l2_frmsizeenum *)ARG3;
      POST_FIELD_WRITE(data->type);
      POST_FIELD_WRITE(data->stepwise);
      break;
   }
   case VKI_V4L2_ENUM_FRAMEINTERVALS: {
      struct vki_v4l2_frmivalenum *data = (struct vki_v4l2_frmivalenum *)ARG3;
      POST_FIELD_WRITE(data->type);
      POST_FIELD_WRITE(data->stepwise);
      break;
   }
   case VKI_V4L2_G_ENC_INDEX: {
      struct vki_v4l2_enc_idx *data = (struct vki_v4l2_enc_idx *)ARG3;
      POST_MEM_WRITE((Addr)data, sizeof(*data));
      break;
   }
   case VKI_V4L2_ENCODER_CMD: {
      struct vki_v4l2_encoder_cmd *data = (struct vki_v4l2_encoder_cmd *)ARG3;
      POST_FIELD_WRITE(data->flags);
      break;
   }
   case VKI_V4L2_TRY_ENCODER_CMD: {
      struct vki_v4l2_encoder_cmd *data = (struct vki_v4l2_encoder_cmd *)ARG3;
      POST_FIELD_WRITE(data->flags);
      break;
   }
   case VKI_V4L2_DBG_S_REGISTER: {
      struct vki_v4l2_dbg_register *data = (struct vki_v4l2_dbg_register *)ARG3;
      POST_FIELD_WRITE(data->size);
      break;
   }
   case VKI_V4L2_DBG_G_REGISTER: {
      struct vki_v4l2_dbg_register *data = (struct vki_v4l2_dbg_register *)ARG3;
      POST_FIELD_WRITE(data->val);
      POST_FIELD_WRITE(data->size);
      break;
   }
   case VKI_V4L2_G_DV_TIMINGS: {
      struct vki_v4l2_dv_timings *data = (struct vki_v4l2_dv_timings *)ARG3;
      POST_MEM_WRITE((Addr)data, sizeof(*data));
      break;
   }
   case VKI_V4L2_DQEVENT: {
      struct vki_v4l2_event *data = (struct vki_v4l2_event *)ARG3;
      POST_MEM_WRITE((Addr)data, sizeof(*data));
      break;
   }
   case VKI_V4L2_CREATE_BUFS: {
      struct vki_v4l2_create_buffers *data = (struct vki_v4l2_create_buffers *)ARG3;
      POST_FIELD_WRITE(data->index);
      break;
   }
   case VKI_V4L2_G_SELECTION: {
      struct vki_v4l2_selection *data = (struct vki_v4l2_selection *)ARG3;
      POST_FIELD_WRITE(data->r);
      break;
   }
   case VKI_V4L2_S_SELECTION: {
      struct vki_v4l2_selection *data = (struct vki_v4l2_selection *)ARG3;
      POST_FIELD_WRITE(data->r);
      break;
   }
   case VKI_V4L2_DECODER_CMD: {
      struct vki_v4l2_decoder_cmd *data = (struct vki_v4l2_decoder_cmd *)ARG3;
      POST_FIELD_WRITE(data->flags);
      break;
   }
   case VKI_V4L2_TRY_DECODER_CMD: {
      struct vki_v4l2_decoder_cmd *data = (struct vki_v4l2_decoder_cmd *)ARG3;
      POST_FIELD_WRITE(data->flags);
      break;
   }
   case VKI_V4L2_ENUM_DV_TIMINGS: {
      struct vki_v4l2_enum_dv_timings *data = (struct vki_v4l2_enum_dv_timings *)ARG3;
      POST_FIELD_WRITE(data->timings);
      break;
   }
   case VKI_V4L2_QUERY_DV_TIMINGS: {
      struct vki_v4l2_dv_timings *data = (struct vki_v4l2_dv_timings *)ARG3;
      POST_MEM_WRITE((Addr)data, sizeof(*data));
      break;
   }
   case VKI_V4L2_DV_TIMINGS_CAP: {
      struct vki_v4l2_dv_timings_cap *data = (struct vki_v4l2_dv_timings_cap *)ARG3;
      POST_MEM_WRITE((Addr)data, sizeof(*data));
      break;
   }
   case VKI_V4L2_ENUM_FREQ_BANDS: {
      struct vki_v4l2_frequency_band *data = (struct vki_v4l2_frequency_band *)ARG3;
      POST_FIELD_WRITE(data->capability);
      POST_FIELD_WRITE(data->rangelow);
      POST_FIELD_WRITE(data->rangehigh);
      POST_FIELD_WRITE(data->modulation);
      break;
   }
   case VKI_V4L2_DBG_G_CHIP_INFO: {
      struct vki_v4l2_dbg_chip_info *data = (struct vki_v4l2_dbg_chip_info *)ARG3;
      POST_FIELD_WRITE(data->name);
      POST_FIELD_WRITE(data->flags);
      break;
   }
   case VKI_V4L2_QUERY_EXT_CTRL: {
      struct vki_v4l2_query_ext_ctrl *data = (struct vki_v4l2_query_ext_ctrl *)ARG3;
      POST_MEM_WRITE((Addr)&data->type,
            sizeof(*data) - sizeof(data->id) - sizeof(data->reserved));
      break;
   }

   case VKI_V4L2_SUBDEV_S_FMT:
   case VKI_V4L2_SUBDEV_S_FRAME_INTERVAL:
   case VKI_V4L2_SUBDEV_S_CROP:
   case VKI_V4L2_SUBDEV_S_SELECTION:
      break;

   case VKI_V4L2_SUBDEV_G_FMT: {
      struct vki_v4l2_subdev_format *data = (struct vki_v4l2_subdev_format *)ARG3;
      POST_FIELD_WRITE(data->format);
      break;
   }
   case VKI_V4L2_SUBDEV_G_FRAME_INTERVAL: {
      struct vki_v4l2_subdev_frame_interval *data = (struct vki_v4l2_subdev_frame_interval *)ARG3;
      POST_FIELD_WRITE(data->interval);
      break;
   }
   case VKI_V4L2_SUBDEV_ENUM_MBUS_CODE: {
      struct vki_v4l2_subdev_mbus_code_enum *data = (struct vki_v4l2_subdev_mbus_code_enum *)ARG3;
      POST_FIELD_WRITE(data->code);
      break;
   }
   case VKI_V4L2_SUBDEV_ENUM_FRAME_SIZE: {
      struct vki_v4l2_subdev_frame_size_enum *data = (struct vki_v4l2_subdev_frame_size_enum *)ARG3;
      POST_FIELD_WRITE(data->min_width);
      POST_FIELD_WRITE(data->min_height);
      POST_FIELD_WRITE(data->max_width);
      POST_FIELD_WRITE(data->max_height);
      break;
   }
   case VKI_V4L2_SUBDEV_ENUM_FRAME_INTERVAL: {
      struct vki_v4l2_subdev_frame_interval_enum *data = (struct vki_v4l2_subdev_frame_interval_enum *)ARG3;
      POST_FIELD_WRITE(data->interval);
      break;
   }
   case VKI_V4L2_SUBDEV_G_CROP: {
      struct vki_v4l2_subdev_crop *data = (struct vki_v4l2_subdev_crop *)ARG3;
      POST_FIELD_WRITE(data->rect);
      break;
   }
   case VKI_V4L2_SUBDEV_G_SELECTION: {
      struct vki_v4l2_subdev_selection *data = (struct vki_v4l2_subdev_selection *)ARG3;
      POST_FIELD_WRITE(data->r);
      break;
   }
   case VKI_MEDIA_IOC_DEVICE_INFO: {
      struct vki_media_device_info *data = (struct vki_media_device_info *)ARG3;
      POST_MEM_WRITE((Addr)data, sizeof(*data) - sizeof(data->reserved));
      break;
   }
   case VKI_MEDIA_IOC_ENUM_ENTITIES: {
      struct vki_media_entity_desc *data = (struct vki_media_entity_desc *)ARG3;
      POST_MEM_WRITE((Addr)data->name, sizeof(*data) - sizeof(data->id));
      break;
   }
   case VKI_MEDIA_IOC_ENUM_LINKS:
      /*
       * This ioctl does write to the provided pointers, but it's not
       * possible to deduce the size of the array those pointers point to.
       */
      break;
   case VKI_MEDIA_IOC_SETUP_LINK:
      break;

   /* Serial */
   case VKI_TIOCGSERIAL: {
      struct vki_serial_struct *data = (struct vki_serial_struct *)ARG3;
      POST_MEM_WRITE((Addr)data, sizeof(*data));
      break;
   }
   case VKI_TIOCSSERIAL:
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

  post_sys_ioctl__out:
   {} /* keep C compilers happy */
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

void 
ML_(linux_PRE_sys_setsockopt) ( ThreadId tid, 
                                UWord arg0, UWord arg1, UWord arg2,
                                UWord arg3, UWord arg4 )
{
   /* int setsockopt(int s, int level, int optname, 
                     const void *optval, socklen_t optlen); */
   Addr optval_p = arg3;
   if (optval_p != (Addr)NULL) {
      /*
       * OK, let's handle at least some setsockopt levels and options
       * ourselves, so we don't get false claims of references to
       * uninitialized memory (such as padding in structures) and *do*
       * check what pointers in the argument point to.
       */
      if (arg1 == VKI_SOL_SOCKET && arg2 == VKI_SO_ATTACH_FILTER)
      {
         struct vki_sock_fprog *fp = (struct vki_sock_fprog *)optval_p;

         /*
          * struct sock_fprog has a 16-bit count of instructions,
          * followed by a pointer to an array of those instructions.
          * There's padding between those two elements.
          *
          * So that we don't bogusly complain about the padding bytes,
          * we just report that we read len and and filter.
          *
          * We then make sure that what filter points to is valid.
          */
         PRE_MEM_READ( "setsockopt(SOL_SOCKET, SO_ATTACH_FILTER, &optval.len)",
                       (Addr)&fp->len, sizeof(fp->len) );
         PRE_MEM_READ( "setsockopt(SOL_SOCKET, SO_ATTACH_FILTER, &optval.filter)",
                       (Addr)&fp->filter, sizeof(fp->filter) );

         /* len * sizeof (*filter) */
         if (fp->filter != NULL)
         {
            PRE_MEM_READ( "setsockopt(SOL_SOCKET, SO_ATTACH_FILTER, optval.filter)",
                          (Addr)(fp->filter),
                          fp->len * sizeof(*fp->filter) );
         }
      }
      else
      {
         PRE_MEM_READ( "socketcall.setsockopt(optval)",
                       arg3, /* optval */
                       arg4  /* optlen */ );
      }
   }
}

void
ML_(linux_PRE_sys_recvmmsg) ( ThreadId tid,
                              UWord arg1, UWord arg2, UWord arg3,
                              UWord arg4, UWord arg5 )
{
   struct vki_mmsghdr *mmsg = (struct vki_mmsghdr *)arg2;
   HChar name[40];     // large enough
   UInt i;
   for (i = 0; i < arg3; i++) {
      VG_(sprintf)(name, "mmsg[%u].msg_hdr", i);
      ML_(generic_PRE_sys_recvmsg)(tid, name, &mmsg[i].msg_hdr);
      VG_(sprintf)(name, "recvmmsg(mmsg[%u].msg_len)", i);
      PRE_MEM_WRITE( name, (Addr)&mmsg[i].msg_len, sizeof(mmsg[i].msg_len) );
   }
   if (arg5)
      PRE_MEM_READ( "recvmmsg(timeout)", arg5, sizeof(struct vki_timespec) );
}

void
ML_(linux_POST_sys_recvmmsg) (ThreadId tid, UWord res,
                              UWord arg1, UWord arg2, UWord arg3,
                              UWord arg4, UWord arg5 )
{
   if (res > 0) {
      struct vki_mmsghdr *mmsg = (struct vki_mmsghdr *)arg2;
      HChar name[32];    // large enough
      UInt i;
      for (i = 0; i < res; i++) {
         VG_(sprintf)(name, "mmsg[%u].msg_hdr", i);
         ML_(generic_POST_sys_recvmsg)(tid, name, &mmsg[i].msg_hdr, mmsg[i].msg_len);
         POST_MEM_WRITE( (Addr)&mmsg[i].msg_len, sizeof(mmsg[i].msg_len) );
      }
   }
}

void
ML_(linux_PRE_sys_sendmmsg) ( ThreadId tid,
                              UWord arg1, UWord arg2, UWord arg3, UWord arg4 )
{
   struct vki_mmsghdr *mmsg = (struct vki_mmsghdr *)arg2;
   HChar name[40];     // large enough
   UInt i;
   for (i = 0; i < arg3; i++) {
      VG_(sprintf)(name, "mmsg[%u].msg_hdr", i);
      ML_(generic_PRE_sys_sendmsg)(tid, name, &mmsg[i].msg_hdr);
      VG_(sprintf)(name, "sendmmsg(mmsg[%u].msg_len)", i);
      PRE_MEM_WRITE( name, (Addr)&mmsg[i].msg_len, sizeof(mmsg[i].msg_len) );
   }
}

void
ML_(linux_POST_sys_sendmmsg) (ThreadId tid, UWord res,
                              UWord arg1, UWord arg2, UWord arg3, UWord arg4 )
{
   if (res > 0) {
      struct vki_mmsghdr *mmsg = (struct vki_mmsghdr *)arg2;
      UInt i;
      for (i = 0; i < res; i++) {
         POST_MEM_WRITE( (Addr)&mmsg[i].msg_len, sizeof(mmsg[i].msg_len) );
      }
   }
}

/* ---------------------------------------------------------------------
   ptrace wrapper helpers
   ------------------------------------------------------------------ */

void
ML_(linux_PRE_getregset) ( ThreadId tid, long arg3, long arg4 )
{
   struct vki_iovec *iov = (struct vki_iovec *) arg4;

   PRE_MEM_READ("ptrace(getregset iovec->iov_base)",
		(unsigned long) &iov->iov_base, sizeof(iov->iov_base));
   PRE_MEM_READ("ptrace(getregset iovec->iov_len)",
		(unsigned long) &iov->iov_len, sizeof(iov->iov_len));
   PRE_MEM_WRITE("ptrace(getregset *(iovec->iov_base))",
		 (unsigned long) iov->iov_base, iov->iov_len);
}

void
ML_(linux_PRE_setregset) ( ThreadId tid, long arg3, long arg4 )
{
   struct vki_iovec *iov = (struct vki_iovec *) arg4;

   PRE_MEM_READ("ptrace(setregset iovec->iov_base)",
		(unsigned long) &iov->iov_base, sizeof(iov->iov_base));
   PRE_MEM_READ("ptrace(setregset iovec->iov_len)",
		(unsigned long) &iov->iov_len, sizeof(iov->iov_len));
   PRE_MEM_READ("ptrace(setregset *(iovec->iov_base))",
		(unsigned long) iov->iov_base, iov->iov_len);
}

void
ML_(linux_POST_getregset) ( ThreadId tid, long arg3, long arg4 )
{
   struct vki_iovec *iov = (struct vki_iovec *) arg4;

   /* XXX: The actual amount of data written by the kernel might be
      less than iov_len, depending on the regset (arg3). */
   POST_MEM_WRITE((unsigned long) iov->iov_base, iov->iov_len);
}

PRE(sys_kcmp)
{
   PRINT("kcmp ( %ld, %ld, %ld, %lu, %lu )", SARG1, SARG2, SARG3, ARG4, ARG5);
   switch (ARG3) {
      case VKI_KCMP_VM: case VKI_KCMP_FILES: case VKI_KCMP_FS:
      case VKI_KCMP_SIGHAND: case VKI_KCMP_IO: case VKI_KCMP_SYSVSEM:
         /* Most of the comparison types don't look at |idx1| or
            |idx2|. */
         PRE_REG_READ3(long, "kcmp",
                       vki_pid_t, pid1, vki_pid_t, pid2, int, type);
         break;
      case VKI_KCMP_FILE:
      default:
         PRE_REG_READ5(long, "kcmp",
                       vki_pid_t, pid1, vki_pid_t, pid2, int, type,
                       unsigned long, idx1, unsigned long, idx2);
         break;
   }
}

#undef PRE
#undef POST

#endif // defined(VGO_linux)

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
