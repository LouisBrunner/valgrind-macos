/*--------------------------------------------------------------------*/
/*--- FreeBSD-specific syscalls, etc.            syswrap-freebsd.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2008 Nicholas Nethercote
      njn@valgrind.org
   Copyright (C) 2018-2021 Paul Floyd
      pjfloyd@wanadoo.fr

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

#if defined(VGO_freebsd)

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
#include "pub_core_machine.h"
#include "pub_core_mallocfree.h"
#include "pub_core_tooliface.h"
#include "pub_core_options.h"
#include "pub_core_scheduler.h"
#include "pub_core_signals.h"
#include "pub_core_stacks.h"
#include "pub_core_syscall.h"
#include "pub_core_syswrap.h"
#include "pub_core_inner.h"
#include "pub_core_pathscan.h"
#include "pub_core_oset.h"
#if defined(ENABLE_INNER_CLIENT_REQUEST)
#include "pub_core_clreq.h"
#endif

#include "priv_types_n_macros.h"
#include "priv_syswrap-generic.h"
#include "priv_syswrap-main.h"
#include "priv_syswrap-freebsd.h"

static Bool capabiltyMode = False;

Bool VG_(get_capability_mode)(void)
{
   return capabiltyMode;
}


// Run a thread from beginning to end and return the thread's
// scheduler-return-code.
static VgSchedReturnCode thread_wrapper(Word /*ThreadId*/ tidW)
{
   VgSchedReturnCode ret;
   ThreadId     tid = (ThreadId)tidW;
   Int          lwpid = VG_(gettid)();
   ThreadState* tst = VG_(get_ThreadState)(tid);

   VG_(debugLog)(1, "syswrap-freebsd",
                 "thread_wrapper(tid=%u,lwpid=%d): entry\n",
                 tid, lwpid);

   vg_assert(tst->status == VgTs_Init);

   /* make sure we get the CPU lock before doing anything significant */
   VG_(acquire_BigLock)(tid, "thread_wrapper(starting new thread)");

   if (0) {
      VG_(printf)("thread tid %u started: stack = %p\n",
                  tid, (void*)&tid);
   }

   /* Make sure error reporting is enabled in the new thread. */
   tst->err_disablement_level = 0;

   VG_TRACK(pre_thread_first_insn, tid);

   tst->os_state.lwpid = lwpid;
   /* Set the threadgroup for real.  This overwrites the provisional value set
      in do_clone().  See comments in do_clone for background, also #226116. */
   tst->os_state.threadgroup = VG_(getpid)();

   /* Thread created with all signals blocked; scheduler will set the
      appropriate mask */

   ret = VG_(scheduler)(tid);

   vg_assert(VG_(is_exiting)(tid));

   vg_assert(tst->status == VgTs_Runnable);
   vg_assert(VG_(is_running_thread)(tid));

   VG_(debugLog)(1, "syswrap-freebsd",
                 "thread_wrapper(tid=%u,lwpid=%d): exit, schedreturncode %s\n",
                 tid, lwpid, VG_(name_of_VgSchedReturnCode)(ret));

   /* Return to caller, still holding the lock. */
   return ret;
}


/* ---------------------------------------------------------------------
   clone-related stuff
   ------------------------------------------------------------------ */

/* Run a thread all the way to the end, then do appropriate exit actions
 *   (this is the last-one-out-turn-off-the-lights bit).
 *
 * This is marked as __attribute__((noreturn)). That has the effect of
 * making clang++ no longer emit the function prologue and epilogue
 * to save the base pointer.
 *
 * As far as I can tell clang -O2 does not include -fomit-frame-pointer
 * However, since from here on the saved base pointer values are
 * junk tools like FreeBSD pstack that only rely on base pointer
 * walking will not work. FreeBSD bstack does work, based on GDB and
 * reading debuginfo.
 *
 * If you really need a working base pointer modify Makefile.all.am
 * and add -fno-omit-frame-pointer to AM_CFLAGS_BASE.
 */
__attribute__((noreturn))
static void run_a_thread_NORETURN ( Word tidW )
{
   ThreadId          tid = (ThreadId)tidW;
   VgSchedReturnCode src;
   Int               c;
   ThreadState*      tst;
#ifdef ENABLE_INNER_CLIENT_REQUEST
   Int               registered_vgstack_id;
#endif

   VG_(debugLog)(1, "syswrap-freebsd",
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

   VG_(debugLog)(1, "syswrap-freebsd",
                 "run_a_thread_NORETURN(tid=%u): post-thread_wrapper\n",
                 tid);

   c = VG_(count_living_threads)();
   vg_assert(c >= 1); /* stay sane */

   /* Deregister thread's stack. */
   if (tst->os_state.stk_id != NULL_STK_ID) {
      VG_(deregister_stack)(tst->os_state.stk_id);
   }

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
         1, "syswrap-freebsd",
         "run_a_thread_NORETURN(tid=%u): "
         "WARNING: exiting thread has err_disablement_level = %u\n",
         tid, tst->err_disablement_level
      );
   }
   tst->err_disablement_level = 0;

   if (c == 1) {

      VG_(debugLog)(1, "syswrap-freebsd",
                    "run_a_thread_NORETURN(tid=%u): "
                    "last one standing\n",
                    tid);

      /* We are the last one standing.  Keep hold of the lock and
         carry on to show final tool results, then exit the entire system.
         Use the continuation pointer set at startup in m_main. */
      ( * VG_(address_of_m_main_shutdown_actions_NORETURN) ) (tid, src);
   } else {

      VG_(debugLog)(1, "syswrap-freebsd",
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
#if defined(VGP_x86_freebsd)    /* FreeBSD has args on the stack */
      __asm__ volatile (
         "movl    %1, %0\n"    /* set tst->status = VgTs_Empty */
         "movl    %2, %%eax\n"    /* set %eax = __NR_thr_exit */
         "movl    %3, %%ebx\n"    /* set %ebx = tst->os_state.exitcode */
         "pushl   %%ebx\n"    /* arg on stack */
         "pushl   %%ebx\n"    /* fake return address */
         "int     $0x80\n"    /* thr_exit(tst->os_state.exitcode) */
         "popl    %%ebx\n"    /* fake return address */
         "popl    %%ebx\n"    /* arg off stack */
         : "=m" (tst->status)
         : "n" (VgTs_Empty), "n" (__NR_thr_exit), "m" (tst->os_state.exitcode)
         : "eax", "ebx"
      );
#elif defined(VGP_amd64_freebsd)
      __asm__ volatile (
         "movl   %1, %0\n"    /* set tst->status = VgTs_Empty */
         "movq   %2, %%rax\n"    /* set %rax = __NR_thr_exit */
         "movq   %3, %%rdi\n"    /* set %rdi = tst->os_state.exitcode */
         "pushq  %%rdi\n"    /* fake return address */
         "syscall\n"        /* thr_exit(tst->os_state.exitcode) */
         "popq   %%rdi\n"    /* fake return address */
         : "=m" (tst->status)
         : "n" (VgTs_Empty), "n" (__NR_thr_exit), "m" (tst->os_state.exitcode)
         : "rax", "rdi"
      );
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

   if (tst->os_state.valgrind_stack_base == 0) {
      vg_assert(tst->os_state.valgrind_stack_init_SP == 0);
   }

   if (tst->os_state.valgrind_stack_base != 0) {
      vg_assert(tst->os_state.valgrind_stack_init_SP != 0);
   }

   /* If no stack is present, allocate one. */

   if (tst->os_state.valgrind_stack_base == 0) {
      stack = VG_(am_alloc_VgStack)( &initial_SP );
      if (stack) {
         tst->os_state.valgrind_stack_base    = (Addr)stack;
         tst->os_state.valgrind_stack_init_SP = initial_SP;
      }
   }

   if (0) {
      VG_(printf)( "stack for tid %u at %p; init_SP=%p\n",
                   tid,
                   (void*)tst->os_state.valgrind_stack_base,
                   (void*)tst->os_state.valgrind_stack_init_SP );
   }

   return tst->os_state.valgrind_stack_init_SP;
}

/* Allocate a stack for the main thread, and run it all the way to the
   end.  Although we already have a working VgStack
   (VG_(interim_stack)) it's better to allocate a new one, so that
   overflow detection works uniformly for all threads.
*/
__attribute__((noreturn))
void VG_(main_thread_wrapper_NORETURN)(ThreadId tid)
{
   Addr sp;
   VG_(debugLog)(1, "syswrap-freebsd",
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

   /* If we can't even allocate the first thread's stack, we're hosed.
      Give up. */
   vg_assert2(sp != 0, "%s", "Cannot allocate main thread's stack.");

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


/* Do a fork() */
SysRes ML_(do_fork) ( ThreadId tid )
{
   vki_sigset_t fork_saved_mask;
   vki_sigset_t mask;
   SysRes       res;

   /* Block all signals during fork, so that we can fix things up in
      the child without being interrupted. */
   VG_(sigfillset)(&mask);
   VG_(sigprocmask)(VKI_SIG_SETMASK, &mask, &fork_saved_mask);

   VG_(do_atfork_pre)(tid);

   res = VG_(do_syscall0)( __NR_fork );

   if (!sr_isError(res)) {
      if (sr_Res(res) == 0) {
         /* child */
         VG_(do_atfork_child)(tid);

         /* restore signal mask */
         VG_(sigprocmask)(VKI_SIG_SETMASK, &fork_saved_mask, NULL);

      } else {
         /* parent */
         VG_(do_atfork_parent)(tid);

         if (VG_(clo_trace_syscalls)) {
            VG_(printf)("   clone(fork): process %d created child %lu\n",
                        VG_(getpid)(), sr_Res(res));
         }

         /* restore signal mask */
         VG_(sigprocmask)(VKI_SIG_SETMASK, &fork_saved_mask, NULL);
      }
   }

   return res;
}

static Addr ML_(make_safe_mask) ( const HChar* malloc_message, Addr mask_pointer )
{
   vki_sigset_t* new_mask;
   const vki_sigset_t* old_mask = (vki_sigset_t *)mask_pointer;

   if (!ML_(safe_to_deref)(old_mask, sizeof(vki_sigset_t))) {
      new_mask = (vki_sigset_t*)1; /* Something recognisable to POST() hook. */
   } else {
      new_mask = VG_(malloc)(malloc_message, sizeof(vki_sigset_t));
      *new_mask = *old_mask;
      VG_(sanitize_client_sigmask)(new_mask);
   }

   return (Addr)new_mask;
}

static void ML_(free_safe_mask) ( Addr mask_pointer )
{
   if (mask_pointer != 0 && mask_pointer != 1) {
      VG_(free)((vki_sigset_t *) mask_pointer);
   }
}


/* ---------------------------------------------------------------------
   PRE/POST wrappers for arch-generic, FreeBSD-specific syscalls
   ------------------------------------------------------------------ */

// Nb: See the comment above the generic PRE/POST wrappers in
// m_syswrap/syswrap-generic.c for notes about how they work.

#define PRE(name)       DEFN_PRE_TEMPLATE(freebsd, name)
#define POST(name)      DEFN_POST_TEMPLATE(freebsd, name)

/* On FreeBSD, if any thread calls exit(2), then they are all shut down, pretty
 * much like linux's exit_group().
 */
// SYS_exit 1
// void exit(int status);
PRE(sys_exit)
{
   ThreadId     t;

   PRINT("exit( %" FMT_REGWORD "u )", ARG1);
   PRE_REG_READ1(void, "exit", int, status);

   /* Mark all threads (including this one) to exit. */
   for (t = 1; t < VG_N_THREADS; t++) {
      if ( /* not alive */ VG_(threads)[t].status == VgTs_Empty ) {
         continue;
      }

      //VG_(threads)[t].exitreason = VgSrc_ExitThread;
      VG_(threads)[t].os_state.exitcode = ARG1;

      // if (t != tid)
      // VG_(get_thread_out_of_syscall)(t);    /* unblock it, if blocked */
   }

   VG_(nuke_all_threads_except)( tid, VgSrc_ExitProcess );
   VG_(reap_threads)(tid);
   VG_(threads)[tid].exitreason = VgSrc_ExitThread;

   /* We have to claim the syscall already succeeded. */
   SET_STATUS_Success(0);
}

// SYS_fork 2
// pid_t fork(void);
PRE(sys_fork)
{
   PRINT("%s", "sys_fork ()");
   PRE_REG_READ0(pid_t, "fork");

   SET_STATUS_from_SysRes( ML_(do_fork)(tid) );
   if (SUCCESS) {
      /* Thread creation was successful; let the child have the chance
         to run */
      *flags |= SfYieldAfter;
   }
}

// SYS_read 3
// generic

// SYS_write   4
// generic

// SYS_open 5
// generic

// SYS_close   6
// generic

// SYS_wait4   7
// generic

// SYS_link 9
// generic

// SYS_unlink  10
// generic

// SYS_chdir   12

// SYS_fchdir  13
// generic

// SYS_freebsd11_mknod  14
// generic

// SYS_chmod   15
// generic

// SYS_chown   16
// generic

// SYS_break   17
// generic

// SYS_getpid  20
// generic

// SYS_mount   21
// int mount(const char *type, const char *dir, int flags, void *data);
PRE(sys_mount)
{
   // Nb: depending on 'flags', the 'type' and 'data' args may be ignored.
   // We are conservative and check everything, except the memory pointed to
   // by 'data'.
   *flags |= SfMayBlock;
   PRINT( "sys_mount( %#" FMT_REGWORD "x, %#" FMT_REGWORD "x, %" FMT_REGWORD "u, %#" FMT_REGWORD "x )",ARG1,ARG2,ARG3,ARG4);
   PRE_REG_READ4(int, "mount",
                 const char *, type, char *, dir, int, flags,
                 void *, data);
   PRE_MEM_RASCIIZ( "mount(type)", ARG1);
   PRE_MEM_RASCIIZ( "mount(path)", ARG2);
}

//  SYS_unmount   22
// int unmount(const char *dir, int flags);
PRE(sys_unmount)
{
   PRINT("sys_umount( %#" FMT_REGWORD "x, %" FMT_REGWORD "u )", ARG1, ARG2);
   PRE_REG_READ2(int, "unmount", const char *, dir, int, flags);
   PRE_MEM_RASCIIZ( "unmount(path)", ARG1);
}

// SYS_setuid  23
// generic

// SYS_getuid  24
// generic

// SYS_geteuid 25
// generic

// SYS_ptrace  26
// int ptrace(int request, pid_t pid, caddr_t addr, int data);
PRE(sys_ptrace)
{
   struct vki_ptrace_io_desc *io_desc;
   PRINT("sys_ptrace ( %" FMT_REGWORD "u, %" FMT_REGWORD "u, 0x%" FMT_REGWORD "x, %" FMT_REGWORD "u)", ARG1, ARG2, ARG3, ARG4);

   PRE_REG_READ4(int, "ptrace", int, request, pid_t, pid, caddr_t, addr, int, data);

   switch (ARG1) {
   case VKI_PTRACE_TRACEME:
   case VKI_PTRACE_READ_I:
   case VKI_PTRACE_READ_D:
   case VKI_PTRACE_WRITE_I:
   case VKI_PTRACE_WRITE_D:
      break;

   case VKI_PTRACE_IO:
      PRE_MEM_READ("ptrace", ARG3, sizeof(struct vki_ptrace_io_desc));
      io_desc = (struct vki_ptrace_io_desc *)ARG3;
      switch (io_desc->piod_op) {
      case VKI_PIOD_READ_D:
      case VKI_PIOD_READ_I:
         PRE_MEM_WRITE( "ptrace", (UWord)io_desc->piod_addr, io_desc->piod_len);
         break;
      case VKI_PIOD_WRITE_D:
      case VKI_PIOD_WRITE_I:
         PRE_MEM_READ( "ptrace", (UWord)io_desc->piod_addr, io_desc->piod_len);
         break;
      }
      break;

   case VKI_PTRACE_CONTINUE:
   case VKI_PTRACE_STEP:
   case VKI_PTRACE_KILL:
   case VKI_PTRACE_ATTACH:
   case VKI_PTRACE_DETACH:
      break;

   case VKI_PTRACE_GETREGS:
      PRE_MEM_WRITE("ptrace", ARG3, sizeof(struct vki_user_regs_struct));
      break;

   case VKI_PTRACE_SETREGS:
      PRE_MEM_READ("ptrace", ARG3, sizeof(struct vki_user_regs_struct));
      break;

   case VKI_PTRACE_GETFPREGS:
      PRE_MEM_WRITE("ptrace", ARG3, sizeof(struct vki_fpreg));
      break;

   case VKI_PTRACE_SETFPREGS:
      PRE_MEM_READ("ptrace", ARG3, sizeof(struct vki_fpreg));
      break;

   case VKI_PTRACE_GETDBREGS:
      PRE_MEM_WRITE("ptrace", ARG3, sizeof(struct vki_dbreg));
      break;

   case VKI_PTRACE_SETDBREGS:
      PRE_MEM_READ("ptrace", ARG3, sizeof(struct vki_dbreg));
      break;

   case VKI_PTRACE_LWPINFO:
      PRE_MEM_WRITE("ptrace", ARG3, sizeof(struct vki_ptrace_lwpinfo));
      break;

   case VKI_PTRACE_GETNUMLWPS:
      break;

   case VKI_PTRACE_GETLWPLIST:
      PRE_MEM_WRITE( "ptrace", ARG3, sizeof(vki_lwpid_t) * ARG4);
      break;

   case VKI_PTRACE_SETSTEP:
   case VKI_PTRACE_CLEARSTEP:
   case VKI_PTRACE_SUSPEND:
   case VKI_PTRACE_RESUME:
   case VKI_PTRACE_TO_SCE:
   case VKI_PTRACE_TO_SCX:
   case VKI_PTRACE_SYSCALL:
   case VKI_PTRACE_VM_TIMESTAMP:
      break;
   case VKI_PTRACE_VM_ENTRY:
      PRE_MEM_WRITE( "ptrace", ARG3, sizeof(struct vki_ptrace_vm_entry));
      break;
   }
}

POST(sys_ptrace)
{
   struct vki_ptrace_io_desc *io_desc;

   switch (ARG1) {
   case VKI_PTRACE_TRACEME:
   case VKI_PTRACE_READ_I:
   case VKI_PTRACE_READ_D:
   case VKI_PTRACE_WRITE_I:
   case VKI_PTRACE_WRITE_D:
      break;

   case VKI_PTRACE_IO:
      io_desc = (struct vki_ptrace_io_desc *)ARG3;
      switch (io_desc->piod_op) {
      case VKI_PIOD_READ_D:
      case VKI_PIOD_READ_I:
         if ((Word)RES != -1) {
            POST_MEM_WRITE((UWord)io_desc->piod_addr, io_desc->piod_len);
         }
         break;
      case VKI_PIOD_WRITE_D:
      case VKI_PIOD_WRITE_I:
         break;
      }
      break;

   case VKI_PTRACE_CONTINUE:
   case VKI_PTRACE_STEP:
   case VKI_PTRACE_KILL:
   case VKI_PTRACE_ATTACH:
   case VKI_PTRACE_DETACH:
      break;

   case VKI_PTRACE_GETREGS:
      if ((Word)RES != -1) {
         POST_MEM_WRITE(ARG3, sizeof(struct vki_user_regs_struct));
      }
      break;

   case VKI_PTRACE_SETREGS:
      break;

   case VKI_PTRACE_GETFPREGS:
      if ((Word)RES != -1) {
         POST_MEM_WRITE(ARG3, sizeof(struct vki_fpreg));
      }
      break;

   case VKI_PTRACE_SETFPREGS:
      break;

   case VKI_PTRACE_GETDBREGS:
      if ((Word)RES != -1) {
         POST_MEM_WRITE(ARG3, sizeof(struct vki_dbreg));
      }
      break;

   case VKI_PTRACE_SETDBREGS:
      break;

   case VKI_PTRACE_LWPINFO:
      if ((Word)RES != -1) {
         POST_MEM_WRITE(ARG3, sizeof(struct vki_ptrace_lwpinfo));
      }
      break;

   case VKI_PTRACE_GETNUMLWPS:
      break;

   case VKI_PTRACE_GETLWPLIST:
      if ((Word)RES != -1) {
         POST_MEM_WRITE(ARG3, sizeof(vki_lwpid_t) * RES);
      }
      break;

   case VKI_PTRACE_SETSTEP:
   case VKI_PTRACE_CLEARSTEP:
   case VKI_PTRACE_SUSPEND:
   case VKI_PTRACE_RESUME:
   case VKI_PTRACE_TO_SCE:
   case VKI_PTRACE_TO_SCX:
   case VKI_PTRACE_SYSCALL:
   case VKI_PTRACE_VM_TIMESTAMP:
      break;

   case VKI_PTRACE_VM_ENTRY:
      if ((Word)RES != -1) {
         POST_MEM_WRITE(ARG3, sizeof(struct vki_ptrace_vm_entry));
      }
      break;
   }
}

// SYS_recvmsg 27
// ssize_t recvmsg(int s, struct msghdr *msg, int flags);
PRE(sys_recvmsg)
{
   *flags |= SfMayBlock;
   PRINT("sys_recvmsg ( %" FMT_REGWORD "d, %#" FMT_REGWORD "x, %" FMT_REGWORD "d )",SARG1,ARG2,SARG3);
   PRE_REG_READ3(vki_ssize_t, "recvmsg", int, s, struct msghdr *, msg, int, flags);
   ML_(generic_PRE_sys_recvmsg)(tid, "recvmsg", (struct vki_msghdr *)ARG2);
}

POST(sys_recvmsg)
{

   ML_(generic_POST_sys_recvmsg)(tid, "recvmsg", (struct vki_msghdr *)ARG2, RES);
}

// SYS_sendmsg 28
// ssize_t sendmsg(int s, const struct msghdr *msg, int flags);
PRE(sys_sendmsg)
{
   *flags |= SfMayBlock;
   PRINT("sys_sendmsg ( %" FMT_REGWORD "u, %#" FMT_REGWORD "x, %" FMT_REGWORD "u )",ARG1,ARG2,ARG3);
   PRE_REG_READ3(ssize_t, "sendmsg",
                 int, s, const struct msghdr *, msg, int, flags);
   ML_(generic_PRE_sys_sendmsg)(tid, "sendmsg", (struct vki_msghdr *)ARG2);
}

// SYS_recvfrom   29
// ssize_t recvfrom(int s, void *buf, size_t len, int flags,
//                  struct sockaddr * restrict from, socklen_t * restrict fromlen);
PRE(sys_recvfrom)
{
   *flags |= SfMayBlock;
   PRINT("sys_recvfrom ( %" FMT_REGWORD "d, %#" FMT_REGWORD "x, %" FMT_REGWORD "u, %" FMT_REGWORD "d, %#" FMT_REGWORD "x, %#" FMT_REGWORD "x )",SARG1,ARG2,ARG3,SARG4,ARG5,ARG6);
   PRE_REG_READ6(ssize_t, "recvfrom",
                 int, s, void *, buf, size_t, len, int, flags,
                 struct sockaddr *, from, int *, fromlen);
   ML_(generic_PRE_sys_recvfrom)(tid, ARG1,ARG2,ARG3,ARG4,ARG5,ARG6);
}

POST(sys_recvfrom)
{
   vg_assert(SUCCESS);
   ML_(generic_POST_sys_recvfrom)(tid, VG_(mk_SysRes_Success)(RES),
                                  ARG1,ARG2,ARG3,ARG4,ARG5,ARG6);
}

// SYS_accept  30
// int accept(int s, struct sockaddr * restrict addr,
//            socklen_t * restrict addrlen);
PRE(sys_accept)
{
   *flags |= SfMayBlock;
   PRINT("sys_accept ( %" FMT_REGWORD "u, %#" FMT_REGWORD "x, %" FMT_REGWORD "u )",ARG1,ARG2,ARG3);
   PRE_REG_READ3(int, "accept",
                 int, s, struct sockaddr *, addr, int, *addrlen);
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

// SYS_getpeername   31
// int getpeername(int s, struct sockaddr * restrict name,
//                 socklen_t * restrict namelen);
PRE(sys_getpeername)
{
   PRINT("sys_getpeername ( %" FMT_REGWORD "u, %#" FMT_REGWORD "x, %#" FMT_REGWORD "x )",ARG1,ARG2,ARG3);
   PRE_REG_READ3(int, "getpeername",
                 int, s, struct sockaddr *, name, socklen_t *, namelen);
   ML_(generic_PRE_sys_getpeername)(tid, ARG1,ARG2,ARG3);
}

POST(sys_getpeername)
{
   vg_assert(SUCCESS);
   ML_(generic_POST_sys_getpeername)(tid, VG_(mk_SysRes_Success)(RES),
                                     ARG1,ARG2,ARG3);
}

// SYS_getsockname   32
// int getsockname(int s, struct sockaddr * restrict name,
//                 socklen_t * restrict namelen);
PRE(sys_getsockname)
{
   PRINT("sys_getsockname ( %" FMT_REGWORD "d, %#" FMT_REGWORD "x, %#" FMT_REGWORD "x )",SARG1,ARG2,ARG3);
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

// SYS_access  33
// generic

// SYS_chflags 34
// int chflags(const char *path, unsigned long flags)
PRE(sys_chflags)
{
   PRINT("sys_chflags ( %#" FMT_REGWORD "x(%s), 0x%" FMT_REGWORD "x )", ARG1,(char *)ARG1,ARG2);
   PRE_REG_READ2(int, "chflags",
                 const char *, path, unsigned long, flags);
   PRE_MEM_RASCIIZ( "chflags(path)", ARG1 );
}

// SYS_fchflags   35
// int  fchflags(int fd, unsigned long flags);
PRE(sys_fchflags)
{
   PRINT("sys_fchflags ( %" FMT_REGWORD "u, %" FMT_REGWORD "u )", ARG1,ARG2);
   PRE_REG_READ2(int, "fchflags", int, fd, unsigned long, flags);
}

// SYS_sync 36
// generic

// SYS_kill 37
// generic

// SYS_getppid 39
// generic

// SYS_dup  41
// generic

// Pipe on freebsd doesn't have args, and uses dual returns!
// SYS_freebsd10_pipe   42
// int pipe(void);
PRE(sys_pipe)
{
   PRINT("%s", "sys_pipe ()");
}

POST(sys_pipe)
{
   if (!ML_(fd_allowed)(RES, "pipe", tid, True) ||
         !ML_(fd_allowed)(RESHI, "pipe", tid, True)) {
      VG_(close)(RES);
      VG_(close)(RESHI);
      SET_STATUS_Failure( VKI_EMFILE );
   } else {
      if (VG_(clo_track_fds)) {
         ML_(record_fd_open_nameless)(tid, RES);
         ML_(record_fd_open_nameless)(tid, RESHI);
      }
   }
}

// SYS_getegid 43
// generic

// SYS_profil  44
// generic

// SYS_ktrace  45
// generic

// SYS_getgid  47
// generic

// SYS_getlogin   49
// syscall.master refers to namelen and namebuf for the argument names
// man getlogin has just getlogin(void) but also
// int getlogin_r(char *name, int len);
// so let's go with those names
PRE(sys_getlogin)
{
   PRINT("sys_getlogin ( %#" FMT_REGWORD "x, %" FMT_REGWORD "u )",ARG1,ARG2);
   PRE_REG_READ2(int, "getlogin", char *, buf, u_int, len);
   PRE_MEM_WRITE( "getlogin(name)", ARG1, ARG2 );
}

POST(sys_getlogin)
{
   POST_MEM_WRITE(ARG1, ARG2 );
}

// SYS_setlogin   50
// int setlogin(const char *name);
PRE(sys_setlogin)
{
   PRINT("sys_setlogin ( %#" FMT_REGWORD "x )",ARG1);
   PRE_REG_READ1(long, "setlogin", char *, buf);
   PRE_MEM_RASCIIZ( "setlogin(buf)", ARG1 );
}

// SYS_acct 51
// generic

// SYS_sigaltstack   53
// generic

// SYS_ioctl   54
// int ioctl(int fd, unsigned long request, ...);
PRE(sys_ioctl)
{
   *flags |= SfMayBlock;
   // @todo PJF presumably the presence of ARG3 depends on ARG2
   PRINT("sys_ioctl ( %" FMT_REGWORD "u, 0x%" FMT_REGWORD "x, %#" FMT_REGWORD "x )",ARG1,ARG2,ARG3);
   PRE_REG_READ3(int, "ioctl",
                 int, fd, unsigned long, request, unsigned long, arg);

   switch (ARG2) {
   case VKI_FIODGNAME: {
      struct vki_fiodgname_arg* data = (struct vki_fiodgname_arg*)(Addr)ARG3;
      PRE_FIELD_READ("ioctl(FIODGNAME).len", data->len);
      PRE_FIELD_READ("ioctl(FIODGNAME).buf", data->buf);
      PRE_MEM_WRITE("ioctl(FIODGNAME).buf", (Addr)data->buf, data->len);
      break;
   }
   default:
      ML_(PRE_unknown_ioctl)(tid, ARG2, ARG3);
      break;
   }

   // The block below is from Ryan Stone
   // https://bitbucket.org/rysto32/valgrind-freebsd/commits/5323c22be9f6c71a00e842c3ddfa1fa8a7feb279
   // however it drags in hundreds of lines of headers into vki-freebsd.h.
   // How stable are these structures? -> maintainability is a concern
   // Also there are no testcases for this.
   // Hence #if 0
#if 0
   /* Handle specific ioctls which pass structures which may have pointers to other
      buffers */
   switch (ARG2 /* request */) {
   case VKI_SIOCGIFMEDIA:
      if (ARG3) {
         struct vki_ifmediareq* imr = (struct vki_ifmediareq*)ARG3;
         if (imr->ifm_ulist) {
            PRE_MEM_WRITE("ioctl(SIOCGIFMEDIA).ifm_ulist",
                          (Addr)(imr->ifm_ulist), imr->ifm_count * sizeof(int));
         }
      }
      break;

   case VKI_PCIOCGETCONF:
      if (ARG3) {
         struct vki_pci_conf_io* pci = (struct vki_pci_conf_io*)ARG3;
         PRE_MEM_READ("ioctl(PCIOCGETCONF).patterns",
                      (Addr)(pci->patterns), pci->pat_buf_len);
         PRE_MEM_WRITE("ioctl(PCIOCGETCONF).matches",
                       (Addr)(pci->matches), pci->match_buf_len);
      }
      break;

   case VKI_CAMIOCOMMAND:
      if (ARG3) {
         union vki_ccb* ccb = (union vki_ccb*)ARG3;
         if (ccb->ccb_h.func_code == VKI_XPT_DEV_MATCH) {
            PRE_MEM_WRITE("ioctl(CAMIOCOMMAND:XPT_DEV_MATCH).matches",
                          (Addr)(ccb->cdm.matches), ccb->cdm.match_buf_len);
         } else if (ccb->ccb_h.func_code == VKI_XPT_SCSI_IO) {
            struct vki_ccb_scsiio* scsiio = (struct vki_ccb_scsiio*)ccb;
            if (scsiio->dxfer_len) {
               if ((scsiio->ccb_h.flags & VKI_CAM_DIR_MASK) == VKI_CAM_DIR_IN) {
                  PRE_MEM_WRITE("ioctl(CAMIOCOMMAND:XPT_SCSI_IO).data_ptr",
                                (Addr)(scsiio->data_ptr), scsiio->dxfer_len);
               } else if ((scsiio->ccb_h.flags & VKI_CAM_DIR_MASK) == VKI_CAM_DIR_OUT) {
                  PRE_MEM_READ("ioctl(CAMIOCOMMAND:XPT_SCSI_IO).data_ptr",
                               (Addr)(scsiio->data_ptr), scsiio->dxfer_len);
               }
            }
         } else if (ccb->ccb_h.func_code == VKI_XPT_GDEV_TYPE ||
                    ccb->ccb_h.func_code == VKI_XPT_PATH_INQ ||
                    ccb->ccb_h.func_code == VKI_XPT_GET_TRAN_SETTINGS) {
            // do nothing
         } else {
            VG_(message)(Vg_UserMsg,
                         "Warning: unhandled ioctl CAMIOCOMMAND function 0x%lx\n",
                         ccb->ccb_h.func_code);
         }
      }
      break;
   }
#endif
}

POST(sys_ioctl)
{
   switch (ARG2) {
   case VKI_FIODGNAME: {
      struct vki_fiodgname_arg* data = (struct vki_fiodgname_arg*)(Addr)ARG3;
      POST_MEM_WRITE((Addr)data->buf, data->len);
      break;
   }
   default:
      ML_(POST_unknown_ioctl)(tid, RES, ARG2, ARG3);
      break;
   }

#if 0
   /* Handle specific ioctls which pass structures which may have pointers to other
      buffers */
   switch (ARG2 /* request */) {
   case VKI_SIOCGIFMEDIA:
      if (ARG3) {
         struct vki_ifmediareq* imr = (struct vki_ifmediareq*)ARG3;
         if (imr->ifm_ulist) {
            POST_MEM_WRITE((Addr)(imr->ifm_ulist), imr->ifm_count * sizeof(int));
         }
      }
      break;

   case VKI_PCIOCGETCONF:
      if (ARG3) {
         struct vki_pci_conf_io* pci = (struct vki_pci_conf_io*)ARG3;
         POST_MEM_WRITE((Addr)(pci->matches), pci->num_matches * sizeof(struct vki_pci_conf));
      }
      break;

   case VKI_CAMIOCOMMAND:
      if (ARG3) {
         union vki_ccb* ccb = (union vki_ccb*)ARG3;
         if (ccb->ccb_h.func_code == VKI_XPT_DEV_MATCH) {
            POST_MEM_WRITE((Addr)(ccb->cdm.matches), ccb->cdm.num_matches*sizeof(struct vki_dev_match_result));
         } else if (ccb->ccb_h.func_code == VKI_XPT_SCSI_IO) {
            struct vki_ccb_scsiio* scsiio = (struct vki_ccb_scsiio*)ccb;
            if (scsiio->dxfer_len) {
               if ((scsiio->ccb_h.flags & VKI_CAM_DIR_MASK) == VKI_CAM_DIR_IN) {
                  POST_MEM_WRITE((Addr)(scsiio->data_ptr), scsiio->dxfer_len);
               }
            }
         }
      }
      break;
   }
#endif
}

// SYS_reboot  55
// int reboot(int howto);
PRE(sys_reboot)
{
   PRINT("sys_reboot ( %" FMT_REGWORD "d )", SARG1);
   PRE_REG_READ1(int, "reboot", int, howto);
}

// SYS_revoke  56
// int revoke(const char *path);
PRE(sys_revoke)
{
   PRINT("sys_revoke ( %#" FMT_REGWORD "x(%s) )", ARG1, (char*)ARG1);
   PRE_REG_READ1(long, "revoke", const char *, path);
   PRE_MEM_RASCIIZ( "revoke(path)", ARG1);
}

// SYS_symlink 57
// generic

static void do_readlink(const HChar* path, HChar *buf, SizeT bufsize, SyscallStatus* status, Bool* curproc_file)
{
   HChar name[30];
   VG_(sprintf)(name, "/proc/%d/file", VG_(getpid)());
   if (ML_(safe_to_deref)(path, 1)
         && (VG_(strcmp)(path, name) == 0
             || VG_(strcmp)(path, "/proc/curproc/file") == 0)) {
      vg_assert(VG_(resolved_exename));
      Int len = VG_(snprintf)(buf, bufsize, "%s",  VG_(resolved_exename));
      SET_STATUS_Success(len);
      *curproc_file = True;
   }
}

// SYS_readlink   58
// ssize_t  readlink(const char *restrict path, char *restrict buf, size_t bufsiz);
PRE(sys_readlink)
{
   FUSE_COMPATIBLE_MAY_BLOCK();
   Word saved = SYSNO;
   Bool curproc_file = False;

   PRINT("sys_readlink ( %#" FMT_REGWORD "x(%s), %#" FMT_REGWORD "x, %llu )",
         ARG1, (char*)(Addr)ARG1, ARG2, (ULong)ARG3);
   PRE_REG_READ3(long, "readlink",
                 const char *, path, char *, buf, int, bufsiz);
   PRE_MEM_RASCIIZ( "readlink(path)", ARG1 );
   PRE_MEM_WRITE( "readlink(buf)", ARG2,ARG3 );

   if (VG_(have_slash_proc) == True)
   {
      /*
       * Handle the case where readlink is looking at /proc/curproc/file or
       * /proc/<pid>/file
       */
      do_readlink((const HChar *)ARG1, (HChar *)ARG2, (SizeT)ARG3, status, &curproc_file);
   }

   if (!curproc_file) {
      /* Normal case */
      SET_STATUS_from_SysRes( VG_(do_syscall3)(saved, ARG1, ARG2, ARG3));
   }
   if (SUCCESS && RES > 0) {
      POST_MEM_WRITE( ARG2, RES );
   }
}

// SYS_execve  59
// generic

// SYS_umask   60
// generic

// SYS_chroot  61
// generic

// SYS_msync   65
// generic

// SYS_vfork 66
// pid_t vfork(void);
PRE(sys_vfork)
{
   PRINT("%s", "sys_vfork ()");
   PRE_REG_READ0(pid_t, "vfork");

   /* Pretend vfork == fork. Not true, but will have to do. */
   SET_STATUS_from_SysRes( ML_(do_fork)(tid) );
   if (SUCCESS) {
      /* Thread creation was successful; let the child have the chance
         to run */
      *flags |= SfYieldAfter;
   }
}

// SYS_sbrk 69
// void * sbrk(intptr_t incr);
PRE(sys_sbrk)
{
   PRINT("sys_sbrk ( %#" FMT_REGWORD "x )",ARG1);
   PRE_REG_READ1(void*, "sbrk", vki_intptr_t, incr);
}

// SYS_freebsd11_vadvise   72
// @todo maybe

// SYS_munmap  73
// generic

// SYS_mprotect   74
// generic

// SYS_madvise 75
// generic

// SYS_mincore 78
// generic

// SYS_getgroups  79
// generic

// SYS_setgroups  80
// generic

// SYS_getpgrp 81
// generic

// SYS_setpgid 82
// generic

// SYS_setitimer  83
// generic

// SYS_swapon  85
// int swapon(const char *special);
PRE(sys_swapon)
{
   PRINT("sys_swapon ( %#" FMT_REGWORD "x(%s) )", ARG1,(char*)ARG1);
   PRE_REG_READ1(int, "swapon", const char*, special );
   PRE_MEM_RASCIIZ( "swapon(special)", ARG1 );
}

// SYS_getitimer  86
// generic

// SYS_getdtablesize 89
// int getdtablesize(void);
PRE(sys_getdtablesize)
{
   PRINT("%s", "sys_getdtablesize ( )");
   PRE_REG_READ0(long, "getdtablesize");
}

// SYS_dup2 90
// generic

// SYS_fcntl   92
// int fcntl(int fd, int cmd, ...);
PRE(sys_fcntl)
{
   switch (ARG2) {
   // These ones ignore ARG3.
   case VKI_F_GETFD:
   case VKI_F_GETFL:
   case VKI_F_GETOWN:
   case VKI_F_GET_SEALS:
   case VKI_F_ISUNIONSTACK:
      PRINT("sys_fcntl ( %" FMT_REGWORD "d, %" FMT_REGWORD "d )", SARG1,SARG2);
      PRE_REG_READ2(int, "fcntl", int, fd, int, cmd);
      break;

   // These ones use ARG3 as "arg".
   case VKI_F_DUPFD:
   case VKI_F_DUPFD_CLOEXEC:
   case VKI_F_SETFD:
   case VKI_F_SETFL:
   case VKI_F_SETOWN:
   case VKI_F_READAHEAD:
   case VKI_F_RDAHEAD:
   case VKI_F_ADD_SEALS:
      PRINT("sys_fcntl[ARG3=='arg'] ( %" FMT_REGWORD "d, %" FMT_REGWORD "d, %" FMT_REGWORD "d )", SARG1,SARG2,SARG3);
      PRE_REG_READ3(int, "fcntl",
                    int, fd, int, cmd, int, arg);
      break;

   // These ones use ARG3 as "lock" - obsolete.
   case VKI_F_OSETLKW:
      *flags |= SfMayBlock;
   /* FALLTHROUGH */
   case VKI_F_OGETLK:
   case VKI_F_OSETLK:
      PRINT("sys_fcntl[ARG3=='lock'] ( %" FMT_REGWORD "u, %" FMT_REGWORD "u, %#" FMT_REGWORD "x )", ARG1,ARG2,ARG3);
      PRE_REG_READ3(int, "fcntl",
                    int, fd, int, cmd,
                    struct oflock *, lock);
      break;

   // This one uses ARG3 as "oldd" and ARG4 as "newd".
   case VKI_F_DUP2FD:
   case VKI_F_DUP2FD_CLOEXEC:
      PRINT("sys_fcntl[ARG3=='oldd', ARG4=='newd'] ( %" FMT_REGWORD "u, %" FMT_REGWORD "u, %" FMT_REGWORD "u, %" FMT_REGWORD "u )",
            ARG1,ARG2,ARG3,ARG4);
      PRE_REG_READ4(int, "fcntl",
                    int, fd, int, cmd,
                    unsigned long, oldd, unsigned long, newd);
      break;

   // These ones use ARG3 as "lock".
   case VKI_F_SETLKW:
      *flags |= SfMayBlock;
   /* FALLTHROUGH */
   case VKI_F_GETLK:
   case VKI_F_SETLK:
   case VKI_F_SETLK_REMOTE:
      PRINT("sys_fcntl[ARG3=='lock'] ( %" FMT_REGWORD "u, %" FMT_REGWORD "u, %#" FMT_REGWORD "x )", ARG1,ARG2,ARG3);
      PRE_REG_READ3(int, "fcntl",
                    int, fd, int, cmd,
                    struct flock *, lock);
      break;
   case VKI_F_KINFO:
      PRINT("sys_fcntl[ARG3=='kinfo_file'] ( %" FMT_REGWORD "u, %" FMT_REGWORD "u, %#" FMT_REGWORD "x )", ARG1,ARG2,ARG3);
      PRE_REG_READ3(int, "fcntl",
                    int, fd, int, cmd,
                    struct vki_kinfo_file *, kinfo);
      if (ARG3) {
         struct vki_kinfo_file* p_kinfo_file = (struct vki_kinfo_file*)ARG3;
         PRE_MEM_WRITE("fcntl(ARG3=='kinfo_file)", ARG3, p_kinfo_file->vki_kf_structsize);
      }
      break;

   default:
      PRINT("sys_fcntl[UNKNOWN] ( %lu, %lu, %lu )", ARG1,ARG2,ARG3);
      I_die_here;
   }
}

POST(sys_fcntl)
{
   vg_assert(SUCCESS);
   if (ARG2 == VKI_F_DUPFD) {
      if (!ML_(fd_allowed)(RES, "fcntl(DUPFD)", tid, True)) {
         VG_(close)(RES);
         SET_STATUS_Failure( VKI_EMFILE );
      } else {
         if (VG_(clo_track_fds)) {
            ML_(record_fd_open_named)(tid, RES);
         }
      }
   } else if (ARG2 == VKI_F_DUPFD_CLOEXEC) {
      if (!ML_(fd_allowed)(RES, "fcntl(DUPFD_CLOEXEC)", tid, True)) {
         VG_(close)(RES);
         SET_STATUS_Failure( VKI_EMFILE );
      } else {
         if (VG_(clo_track_fds)) {
            ML_(record_fd_open_named)(tid, RES);
         }
      }
   }
}

// SYS_select  93
// generic

// SYS_fsync   95
// generic

// SYS_setpriority   9
// generic

// SYS_socket  97
// int socket(int domain, int type, int protocol);
PRE(sys_socket)
{
   PRINT("sys_socket ( %" FMT_REGWORD "d, %" FMT_REGWORD "d, %" FMT_REGWORD "d )",SARG1,SARG2,SARG3);
   PRE_REG_READ3(int, "socket", int, domain, int, type, int, protocol);
}

POST(sys_socket)
{
   SysRes r;
   vg_assert(SUCCESS);
   r = ML_(generic_POST_sys_socket)(tid, VG_(mk_SysRes_Success)(RES));
   SET_STATUS_from_SysRes(r);
}

// SYS_connect 98
// int connect(int s, const struct sockaddr *name, socklen_t namelen);
PRE(sys_connect)
{
   *flags |= SfMayBlock;
   PRINT("sys_connect ( %" FMT_REGWORD "u, %#" FMT_REGWORD "x, %" FMT_REGWORD "u )",ARG1,ARG2,ARG3);
   PRE_REG_READ3(int, "connect",
                 int, s, const struct sockaddr *, name, int, namelen);
   ML_(generic_PRE_sys_connect)(tid, ARG1,ARG2,ARG3);
}

// SYS_getpriority   100
// generic

// SYS_bind 104
// int bind(int s, const struct sockaddr *addr, socklen_t addrlen);
PRE(sys_bind)
{
   PRINT("sys_bind ( %" FMT_REGWORD "u, %#" FMT_REGWORD "x, %" FMT_REGWORD "u )",ARG1,ARG2,ARG3);
   PRE_REG_READ3(int, "bind",
                 int, s, struct sockaddr *, addr, int, addrlen);
   ML_(generic_PRE_sys_bind)(tid, ARG1,ARG2,ARG3);
}

// SYS_setsockopt 105
// int setsockopt(int s, int level, int optname, const void *optval,
//                socklen_t optlen);
PRE(sys_setsockopt)
{
   PRINT("sys_setsockopt ( %" FMT_REGWORD "d, %" FMT_REGWORD "d, %" FMT_REGWORD "d, %#" FMT_REGWORD "x, %" FMT_REGWORD "u )",SARG1,SARG2,SARG3,ARG4,ARG5);
   PRE_REG_READ5(int, "setsockopt",
                 int, s, int, level, int, optname,
                 const void *, optval, vki_socklen_t, optlen);
   ML_(generic_PRE_sys_setsockopt)(tid, ARG1,ARG2,ARG3,ARG4,ARG5);
}

// SYS_listen  106
// int listen(int s, int backlog);
PRE(sys_listen)
{
   PRINT("sys_listen ( %" FMT_REGWORD "d, %" FMT_REGWORD "d )",SARG1,SARG2);
   PRE_REG_READ2(int, "listen", int, s, int, backlog);
}

//SYS_gettimeofday   116
// generic

// SYS_getrusage  117
// generic

// SYS_getsockopt 118
// int getsockopt(int s, int level, int optname, void * restrict optval,
//                socklen_t * restrict optlen);
PRE(sys_getsockopt)
{
   Addr optval_p = ARG4;
   Addr optlen_p = ARG5;
   PRINT("sys_getsockopt ( %" FMT_REGWORD "u, %" FMT_REGWORD "u, %" FMT_REGWORD "u, %#" FMT_REGWORD "x, %#" FMT_REGWORD "x )",ARG1,ARG2,ARG3,ARG4,ARG5);
   PRE_REG_READ5(int, "getsockopt",
                 int, s, int, level, int, optname,
                 void *, optval, int, *optlen);
   if (optval_p != (Addr)NULL) {
      ML_(buf_and_len_pre_check) ( tid, optval_p, optlen_p,
                                   "getsockopt(optval)",
                                   "getsockopt(optlen)" );
   }
}

POST(sys_getsockopt)
{
   Addr optval_p = ARG4;
   Addr optlen_p = ARG5;
   vg_assert(SUCCESS);
   if (optval_p != (Addr)NULL) {
      ML_(buf_and_len_post_check) ( tid, VG_(mk_SysRes_Success)(RES),
                                    optval_p, optlen_p,
                                    "getsockopt(optlen_out)" );
   }
}

// SYS_readv   120
// generic

// SYS_writev  121
// generic

// SYS_settimeofday  122
// generic

// SYS_fchown  123
// generic

// SYS_fchmod  124
// generic

// SYS_setreuid   126
// generic

// SYS_setregid   127
// generic

// SYS_rename  128
// generic

// SYS_flock   131
// generic

// SYS_mkfifo  132
// int mkfifo(const char *path, mode_t mode);
PRE(sys_mkfifo)
{
   PRINT("sys_mkfifo ( %#" FMT_REGWORD "x(%s), 0x%" FMT_REGWORD "x, 0x%" FMT_REGWORD "x )", ARG1, (char *)ARG1, ARG2, ARG3 );
   PRE_REG_READ2(int, "mkfifo", const char *, path, int, mode);
   PRE_MEM_RASCIIZ( "mkfifo(path)", ARG1 );
}

// SYS_sendto  133
// ssize_t sendto(int s, const void *msg, size_t len, int flags,
//                const struct sockaddr *to, socklen_t tolen);
PRE(sys_sendto)
{
   *flags |= SfMayBlock;
   PRINT("sys_sendto ( %" FMT_REGWORD "u, %#" FMT_REGWORD "x, %" FMT_REGWORD "u, %" FMT_REGWORD "u, %#" FMT_REGWORD "x, %" FMT_REGWORD "u )",ARG1,ARG2,ARG3,ARG4,ARG5,ARG6);
   PRE_REG_READ6(ssize_t, "sendto",
                 int, s, const void *, msg, int, len,
                 int, flags,
                 const struct sockaddr *, to, socklen_t, tolen);
   ML_(generic_PRE_sys_sendto)(tid, ARG1,ARG2,ARG3,ARG4,ARG5,ARG6);
}

// SYS_shutdown   134
// int shutdown(int s, int how);
PRE(sys_shutdown)
{
   *flags |= SfMayBlock;
   PRINT("sys_shutdown ( %" FMT_REGWORD "u, %" FMT_REGWORD "u )",ARG1,ARG2);
   PRE_REG_READ2(int, "shutdown", int, s, int, how);
}

// SYS_socketpair 135
// int socketpair(int domain, int type, int protocol, int *sv);
PRE(sys_socketpair)
{
   PRINT("sys_socketpair ( %" FMT_REGWORD "u, %" FMT_REGWORD "u, %" FMT_REGWORD "u, %#" FMT_REGWORD "x )",ARG1,ARG2,ARG3,ARG4);
   PRE_REG_READ4(int, "socketpair",
                 int, domain, int, type, int, protocol, int *, sv);
   ML_(generic_PRE_sys_socketpair)(tid, ARG1,ARG2,ARG3,ARG4);
}

POST(sys_socketpair)
{
   vg_assert(SUCCESS);
   ML_(generic_POST_sys_socketpair)(tid, VG_(mk_SysRes_Success)(RES),
                                    ARG1,ARG2,ARG3,ARG4);
}

// SYS_mkdir   136
// generic

// SYS_rmdir   137
// generic

// SYS_utimes  138
// generic

// SYS_adjtime 140
// int adjtime(const struct timeval *delta, struct timeval *olddelta);
PRE(sys_adjtime)
{
   PRINT("sys_adjtime ( %#" FMT_REGWORD "x, %#" FMT_REGWORD "x )",ARG1,ARG2);
   PRE_REG_READ2(int, "adjtime",
                 const struct vki_timeval *, delta, struct vki_timeval *, olddelta);
   PRE_MEM_READ("adjtime(delta)", ARG1, sizeof(struct vki_timeval));
   if (ARG2) {
      PRE_MEM_WRITE("adjtime(olddelta)", ARG1, sizeof(struct vki_timeval));
   }
}

POST(sys_adjtime)
{
   if (ARG2) {
      POST_MEM_WRITE(ARG1, sizeof(struct vki_timeval));
   }
}

// SYS_setsid  147
// generic

// SYS_quotactl   148
/* int quotactl(const char *path, int cmd, int id, void *addr); */
PRE(sys_quotactl)
{
   PRINT("sys_quotactl ( %#" FMT_REGWORD "x, %" FMT_REGWORD "u, %" FMT_REGWORD "u, %#" FMT_REGWORD "x )", ARG1,ARG2,ARG3, ARG4);
   switch (ARG2) {
   case VKI_Q_QUOTAON:
   case VKI_Q_SETQUOTA:
   case VKI_Q_SETUSE:

   case VKI_Q_GETQUOTASIZE:
      PRE_REG_READ4(int, "quotactl",
                    const char *, path, int, cmd, int, id,
                    void *, addr);
      PRE_MEM_RASCIIZ( "quotactl(path)", ARG1 );
      break;
   case VKI_Q_GETQUOTA:
      if (VG_(tdict).track_pre_reg_read) {
         \
         PRRSN;
         PRA1("quotactl",const char*,path);
         PRA2("quotactl",int,cmd);
         PRA4("quotactl",void*,addr);
      }
      break;
   case VKI_Q_QUOTAOFF:
   case VKI_Q_SYNC:
      PRE_REG_READ2(int, "quotactl",
                    const char *, path, int, cmd);
      break;
   default:
      break;
   }
}

// SYS_nlm_syscall   154
// syscall.master says ; 154 is initialised by the NLM code, if present.
// @todo

// SYS_nfssvc  155
// int nfssvc(int flags, void *argstructp);
// lengthy manpage, at least 3 types of struct that argstructp can point to
// @todo

// SYS_lgetfh  160
// int lgetfh(const char *path, fhandle_t *fhp);
PRE(sys_lgetfh)
{
   PRINT("sys_lgetfh ( %#" FMT_REGWORD "x, %" FMT_REGWORD "x ", ARG1, ARG2);
   PRE_REG_READ2(int, "lgetfh", const char*, path, vki_fhandle_t*, fhp);
   PRE_MEM_RASCIIZ( "lgetfh(path)", ARG1 );
   PRE_MEM_WRITE("lgetfh(fhp)", ARG2, sizeof(vki_fhandle_t));
}

POST(sys_lgetfh)
{
   POST_MEM_WRITE(ARG2, sizeof(vki_fhandle_t));
}

// SYS_getfh   161
// int getfh(const char *path, fhandle_t *fhp);
PRE(sys_getfh)
{
   PRINT("sys_getfh ( %#" FMT_REGWORD "x, %" FMT_REGWORD "x ", ARG1, ARG2);
   PRE_REG_READ2(int, "getfh", const char*, path, vki_fhandle_t*, fhp);
   PRE_MEM_RASCIIZ( "getfh(path)", ARG1 );
   PRE_MEM_WRITE("getfh(fhp)", ARG2, sizeof(vki_fhandle_t));
}

POST(sys_getfh)
{
   POST_MEM_WRITE(ARG2, sizeof(vki_fhandle_t));
}

#if (FREEBSD_VERS <= FREEBSD_10)
// 162
// int getdomainname(char *domainname, int len);
PRE(sys_freebsd4_getdomainname)
{
   PRINT("sys_freebsd4_getdomainname ( %#" FMT_REGWORD "x, %" FMT_REGWORD "u )",ARG1,ARG2);
   PRE_REG_READ2(int, "getdomainname",
                 char *, domainname, int, len);
   PRE_MEM_WRITE( "getdomainname(domainname)", ARG1, ARG2 );
}

POST(sys_freebsd4_getdomainname)
{
   if (ARG1 != 0) {
      POST_MEM_WRITE( ARG1, ARG2 );
   }
}

// 163
// int setdomainname(char *domainname, int len);
PRE(sys_freebsd4_setdomainname)
{
   PRINT("sys_freebsd4_setdomainname ( %#" FMT_REGWORD "x )",ARG1);
   PRE_REG_READ2(int, "setdomainname", char *, domainname, int, len);
   PRE_MEM_RASCIIZ( "setdomainname(domainname)", ARG1 );
}

// 164
// int uname(struct utsname *name);
PRE(sys_freebsd4_uname)
{
   PRINT("sys_freebsd4_uname ( %#" FMT_REGWORD "x )", ARG1);
   PRE_REG_READ1(int, "uname", struct utsname *, name);
   PRE_MEM_WRITE( "uname(name)", ARG1, sizeof(struct vki_utsname) );
}

POST(sys_freebsd4_uname)
{
   if (ARG1 != 0) {
      POST_MEM_WRITE( ARG1, sizeof(struct vki_utsname) );
   }
}
#endif

// SYS_sysarch 165
// x86/amd64

// SYS_rtprio  166
PRE(sys_rtprio)
{
   PRINT( "sys_rtprio ( %" FMT_REGWORD "u, %" FMT_REGWORD "u, %#" FMT_REGWORD "x )", ARG1, ARG2, ARG3 );
   PRE_REG_READ3(int, "rtprio",
                 int, function, pid_t, pid, struct rtprio *, rtp);
   if (ARG1 == VKI_RTP_SET) {
      PRE_MEM_READ( "rtprio(rtp#set)", ARG3, sizeof(struct vki_rtprio));
   } else if (ARG1 == VKI_RTP_LOOKUP) {
      PRE_MEM_WRITE( "rtprio(rtp#lookup)", ARG3, sizeof(struct vki_rtprio));
   } else {
      /* PHK ?? */
   }
}

POST(sys_rtprio)
{
   if (ARG1 == VKI_RTP_LOOKUP && RES == 0) {
      POST_MEM_WRITE( ARG3, sizeof(struct vki_rtprio));
   }
}

// freebsd6_pread 173 FREEBSD_VERS <= 10
// x86/amd64

// freebsd6_pwrite 174 FREEBSD_VERS <= 10
// x86/amd64

// SYS_setfib  175
// int setfib(int fib);
PRE(sys_setfib)
{
   PRINT("sys_setfib ( %" FMT_REGWORD "d )", SARG1);
   PRE_REG_READ1(int, "setfib", int, fib);
}

// SYS_ntp_adjtime   176
// int ntp_adjtime(struct timex *);
// @todo

// SYS_setgid  181
// generic

// SYS_setegid 182
// int setegid(gid_t egid);
PRE(sys_setegid)
{
   PRINT("sys_setegid ( %" FMT_REGWORD "u )", ARG1);
   PRE_REG_READ1(int, "setegid", vki_gid_t, gid);
}

// SYS_seteuid 183
// int seteuid(uid_t euid);
PRE(sys_seteuid)
{
   PRINT("sys_seteuid ( %" FMT_REGWORD "u )", ARG1);
   PRE_REG_READ1(long, "seteuid", vki_uid_t, uid);
}


#if (FREEBSD_VERS >= FREEBSD_12)

// SYS_freebsd11_stat   188
// int stat(char *path, struct freebsd11_stat *sb);
PRE(sys_freebsd11_stat)
{
   PRINT("sys_freebsd11_stat ( %#" FMT_REGWORD "x(%s), %#" FMT_REGWORD "x )",ARG1,(char *)ARG1,ARG2);
   PRE_REG_READ2(int, "stat", char *, path, struct freebsd11_stat *, sb);
   PRE_MEM_RASCIIZ( "stat(path)", ARG1 );
   PRE_MEM_WRITE( "stat(sb)", ARG2, sizeof(struct vki_freebsd11_stat) );
}

POST(sys_freebsd11_stat)
{
   POST_MEM_WRITE( ARG2, sizeof(struct vki_freebsd11_stat) );
}

// SYS_freebsd11_fstat  189
// int fstat(int fd, struct stat *sb);
PRE(sys_freebsd11_fstat)
{
   PRINT("sys_freebsd11_fstat ( %" FMT_REGWORD "d, %#" FMT_REGWORD "x )",SARG1,ARG2);
   PRE_REG_READ2(int, "fstat", int, fd, struct stat *, sb);
   PRE_MEM_WRITE( "fstat(sb)", ARG2, sizeof(struct vki_freebsd11_stat) );
}

POST(sys_freebsd11_fstat)
{
   POST_MEM_WRITE( ARG2, sizeof(struct vki_freebsd11_stat) );
}

// SYS_freebsd11_lstat  190
// int lstat(const char * restrict path, struct stat * restrict sb);
PRE(sys_freebsd11_lstat)
{
   PRINT("sys_freebsd11_lstat ( %#" FMT_REGWORD "x(%s), %#" FMT_REGWORD "x )",ARG1,(char *)ARG1,ARG2);
   PRE_REG_READ2(sb, "lstat", const char *, path, struct freebsd11_stat *, sb);
   PRE_MEM_RASCIIZ( "lstat(path)", ARG1 );
   PRE_MEM_WRITE( "lstat(sb)", ARG2, sizeof(struct vki_freebsd11_stat) );
}

POST(sys_freebsd11_lstat)
{
   vg_assert(SUCCESS);
   if (RES == 0) {
      POST_MEM_WRITE( ARG2, sizeof(struct vki_freebsd11_stat) );
   }
}

#else

PRE(sys_stat)
{
   PRINT("sys_stat ( %#" FMT_REGWORD "x(%s), %#" FMT_REGWORD "x )",ARG1,(char *)ARG1,ARG2);
   PRE_REG_READ2(int, "stat", char *, path, struct stat *, sb);
   PRE_MEM_RASCIIZ( "stat(path)", ARG1 );
   PRE_MEM_WRITE( "stat(sb)", ARG2, sizeof(struct vki_freebsd11_stat) );
}

POST(sys_stat)
{
   POST_MEM_WRITE( ARG2, sizeof(struct vki_freebsd11_stat) );
}


PRE(sys_fstat)
{
   PRINT("sys_fstat ( %" FMT_REGWORD "d, %#" FMT_REGWORD "x )",SARG1,ARG2);
   PRE_REG_READ2(int, "fstat", int, fd, struct stat *, sb);
   PRE_MEM_WRITE( "fstat(sb)", ARG2, sizeof(struct vki_freebsd11_stat) );
}

POST(sys_fstat)
{
   POST_MEM_WRITE( ARG2, sizeof(struct vki_freebsd11_stat) );
}

PRE(sys_lstat)
{
   PRINT("sys_lstat ( %#" FMT_REGWORD "x(%s), %#" FMT_REGWORD "x )",ARG1,(char *)ARG1,ARG2);
   PRE_REG_READ2(int, "lstat", const char *, path, struct stat *, sb);
   PRE_MEM_RASCIIZ( "lstat(path)", ARG1 );
   PRE_MEM_WRITE( "lstat(sb)", ARG2, sizeof(struct vki_freebsd11_stat) );
}

POST(sys_lstat)
{
   vg_assert(SUCCESS);
   if (RES == 0) {
      POST_MEM_WRITE( ARG2, sizeof(struct vki_freebsd11_stat) );
   }
}

#endif

// SYS_pathconf   191
// long pathconf(const char *path, int name);
PRE(sys_pathconf)
{
   PRINT("sys_pathconf ( %#" FMT_REGWORD "x(%s), %" FMT_REGWORD "u )",ARG1,(char *)ARG1,ARG2);
   PRE_REG_READ2(long, "pathconf", char *, path, int, name);
   PRE_MEM_RASCIIZ( "pathconf(path)", ARG1 );
}

// SYS_fpathconf  192
// long fpathconf(int fd, int name);
PRE(sys_fpathconf)
{
   PRINT("sys_fpathconf ( %" FMT_REGWORD "u, %" FMT_REGWORD "u )",ARG1,ARG2);
   PRE_REG_READ2(long, "fpathconf", int, fd, int, name);
}

// SYS_getrlimit  194
// generic

// SYS_setrlimit  195
// generic


// SYS_freebsd11_getdirentries   196
// int getdirentries(int fd, char *buf, int nbytes, long *basep);
#if (FREEBSD_VERS >= FREEBSD_12)
PRE(sys_freebsd11_getdirentries)
{
   *flags |= SfMayBlock;
   PRINT("sys_freebsd11_getdirentries ( %" FMT_REGWORD "u, %#" FMT_REGWORD "x, %" FMT_REGWORD "u )", ARG1,ARG2,ARG3);
   PRE_REG_READ4(int, "getdirentries",
                 int, fd, char *, buf,
                 int, nbytes,
                 long *, basep);
   PRE_MEM_WRITE( "getdirentries(buf)", ARG2, ARG3 );
   if (ARG4) {
      PRE_MEM_WRITE( "getdirentries(basep)", ARG4, sizeof(long) );
   }
}

POST(sys_freebsd11_getdirentries)
{
   vg_assert(SUCCESS);
   if (RES > 0) {
      POST_MEM_WRITE( ARG2, RES );
      if ( ARG4 != 0 ) {
         POST_MEM_WRITE( ARG4, sizeof (long));
      }
   }
}
#else
PRE(sys_getdirentries)
{
   *flags |= SfMayBlock;
   PRINT("sys_getdirentries ( %" FMT_REGWORD "u, %#" FMT_REGWORD "x, %" FMT_REGWORD "u )", ARG1,ARG2,ARG3);
   PRE_REG_READ4(int, "getdirentries",
                 int, fd, char *, buf,
                 int, nbytes,
                 long *, basep);
   PRE_MEM_WRITE( "getdirentries(buf)", ARG2, ARG3 );
   if (ARG4)
      PRE_MEM_WRITE( "getdirentries(basep)", ARG4, sizeof(long) );
}

POST(sys_getdirentries)
{
   vg_assert(SUCCESS);
   if (RES > 0) {
      POST_MEM_WRITE( ARG2, RES );
      if ( ARG4 != 0 )
         POST_MEM_WRITE( ARG4, sizeof (long));
   }
}
#endif

// SYS_freebsd6_mmap 197
// amd64 / x86


// SYS___syscall  198
// special handling

// freebsd6_lseek 199 FREEBSD_VERS <= 10
// x86/amd64

// freebsd6_truncate 200 FREEBSD_VERS <= 10
// x86/amd64

// freebsd6_ftruncate 201 FREEBSD_VERS <= 10
// x86/amd64

static Bool sysctl_kern_ps_strings(SizeT* out, SizeT* outlen)
{
   Word tmp = -1;
   const struct auxv *cauxv;

   for (cauxv = (struct auxv*)VG_(client_auxv); cauxv->a_type != VKI_AT_NULL; cauxv++) {
      if (cauxv->a_type == VKI_AT_PS_STRINGS) {
         tmp = (Word)cauxv->u.a_ptr;

         *out = tmp;
         *outlen = sizeof(size_t);
         return True;
      }
   }
   return False;
}

static void sysctl_kern_usrstack(SizeT* out, SizeT* outlen)
{
   *out = VG_(get_usrstack)();
   *outlen = sizeof(ULong);
}

static Bool sysctl_kern_proc_pathname(HChar *out, SizeT *len)
{
   const HChar *exe_name = VG_(resolved_exename);

   if (!len) {
      return False;
   }

   if (!out) {
      HChar tmp[VKI_PATH_MAX];
      if (!VG_(realpath)(exe_name, tmp)) {
         return False;
      }
      *len = VG_(strlen)(tmp)+1;
      return True;
   }

   if (!VG_(realpath)(exe_name, out)) {
      return False;
   }

   *len = VG_(strlen)(out)+1;
   return True;
}

// SYS___sysctl   202
/* int __sysctl(int *name, u_int namelen, void *oldp, size_t *oldlenp, void *newp, size_t newlen); */
/*               ARG1        ARG2          ARG3         ARG4           ARG5        ARG6 */
PRE(sys___sysctl)
{
   PRINT("sys_sysctl ( %#" FMT_REGWORD "x, %" FMT_REGWORD "d, %#" FMT_REGWORD "x, %#" FMT_REGWORD "x, %#" FMT_REGWORD "x, %" FMT_REGWORD "u )", ARG1,SARG2,ARG3,ARG4,ARG5,ARG6 );

   int* name = (int*)ARG1;
   if (ML_(safe_to_deref)(name, sizeof(int))) {
      PRINT("\nmib[0]: ");
      if (SARG2 >= 1) {
         switch (name[0]) {
         case 0: // CTL_UNSPEC
            PRINT("unspec");
            break;
         case 1: // CTL_KERN
            PRINT("kern");
            break;
         case 2: // CTL_VM
            PRINT("vm");
            break;
         case 3: // CTL_VFS
            PRINT("vfs");
            break;
         case 4: // CTL_NET
            PRINT("net");
            break;
         case 5: // CTL_DEBUG
            PRINT("debug");
            break;
         case 6: // CTL_HW
            PRINT("hw");
            break;
         case 7: // CTL_MACHDEP
            PRINT("machdep");
            break;
         case 8: // CTL _USER
            PRINT("user");
            break;
         case 9: //CTL_P1003_1B
            PRINT("p1003_b1b");
            break;
         default:
            PRINT("unrecognized (%d)", ((int*)ARG1)[0]);
            break;
         }
      }
      if (SARG2 >= 2 && ML_(safe_to_deref)(name, 2*sizeof(int))) {
         PRINT(" mib[1]: %d\n", name[1]);
      }
   }

   /*
    * Special handling cases
    *
    * 1. kern.usrstack
    *    This sysctl returns the address of the bottom of the user stack
    *    (that is the highest user stack address, since the stack grows
    *    downwards). Without any special handling this would return the
    *    address of the host userstack. We have created a stack for the
    *    guest (in aspacemgr) and that is the one that we want the guest
    *    to see. Aspacemgr is setup in m_main.c with the adresses and sizes
    *    saved to file static variables in that file, so we call
    *    VG_(get_usrstack)() to retrieve them from there.
    */
   if (SARG2 == 2 && ML_(safe_to_deref)(name, 2*sizeof(int))) {
      if (name[0] == 1 && name[1] == 33) {
         // kern.usrstack
         sysctl_kern_usrstack((SizeT*)ARG3, (SizeT*)ARG4);
         SET_STATUS_Success(0);
      }
   }

   /*
    * 2. kern.ps_strings
    */
   if (SARG2 == 2 && ML_(safe_to_deref)(name, 2*sizeof(int))) {
      if (name[0] == 1 && name[1] == 32) {
         if (sysctl_kern_ps_strings((SizeT*)ARG3, (SizeT*)ARG4)) {
           SET_STATUS_Success(0);
         }
      }
   }

   /*
    * 3. kern.proc.pathname
    */
   if (SARG2 == 4 && ML_(safe_to_deref)(name, 4*sizeof(int))) {
      if (name[0] == 1 && name[1] == 14 && name[2] == 12) {
         vki_pid_t pid = (vki_pid_t)name[3];
         if (pid == -1 || pid == VG_(getpid)()) {
            sysctl_kern_proc_pathname((HChar *)ARG3, (SizeT *)ARG4);
            SET_STATUS_Success(0);
         }
      }
   }

   PRE_REG_READ6(int, "__sysctl", int *, name, vki_u_int32_t, namelen, void *, oldp,
                 vki_size_t *, oldlenp, void *, newp, vki_size_t, newlen);

   // read number of ints specified in ARG2 from mem pointed to by ARG1
   PRE_MEM_READ("sysctl(name)", (Addr)ARG1, ARG2 * sizeof(int));

   // if 'newp' is not NULL can read namelen bytes from that address
   if (ARG5 != (UWord)NULL) {
      PRE_MEM_READ("sysctl(newp)", (Addr)ARG5, ARG6);
   }

   // there are two scenarios for oldlenp/oldp
   // 1. oldval is NULL and oldlenp is non-NULL
   //    this is a query of oldlenp so oldlenp will be written
   // 2. Both are non-NULL
   //    this is a query of oldp, oldlenp will be read and oldp will
   //    be written
   //
   // More thoughts on this
   // if say oldp is a string buffer
   // oldlenp will point to the length of the buffer
   //
   // but on return does oldlenp also get updated?

   // is oldlenp is not NULL, can write
   if (ARG4 != (UWord)NULL) {
      if (ARG3 != (UWord)NULL) {
         // case 2 above
         PRE_MEM_READ("sysctl(oldlenp)", (Addr)ARG4, sizeof(vki_size_t));
         PRE_MEM_WRITE("sysctl(oldlenp)", (Addr)ARG4, sizeof(vki_size_t));
         if (ML_(safe_to_deref)((void*)(Addr)ARG4, sizeof(vki_size_t))) {
            PRE_MEM_WRITE("sysctl(oldp)", (Addr)ARG3, *(vki_size_t *)ARG4);
         } else {
            VG_(dmsg)("Warning: Bad oldlenp address %p in sysctl\n",
                      (void *)(Addr)ARG4);
            SET_STATUS_Failure ( VKI_EFAULT );
         }
      } else {
         // case 1 above
         PRE_MEM_WRITE("sysctl(oldlenp)", (Addr)ARG4, sizeof(vki_size_t));
      }
   }
}

POST(sys___sysctl)
{
   if (ARG4 != (UWord)NULL) {
      if (ARG3 != (UWord)NULL) {
         POST_MEM_WRITE((Addr)ARG4, sizeof(vki_size_t));
         POST_MEM_WRITE((Addr)ARG3, *(vki_size_t *)ARG4);
      } else {
         POST_MEM_WRITE((Addr)ARG4, sizeof(vki_size_t));
      }
   }
}

// SYS_mlock   203
// generic

// SYS_munlock 204
// generic

// SYS_undelete   205
// int undelete(const char *path);
PRE(sys_undelete)
{
   *flags |= SfMayBlock;
   PRINT("sys_undelete ( %#" FMT_REGWORD "x(%s) )", ARG1,(char *)ARG1);
   PRE_REG_READ1(int, "undelete", const char *, path);
   PRE_MEM_RASCIIZ( "undelete(path)", ARG1 );
}

// SYS_futimes 206
// int futimes(int fd, const struct timeval *times);
PRE(sys_futimes)
{
   PRINT("sys_lutimes ( %" FMT_REGWORD "u, %#" FMT_REGWORD "x )", ARG1,ARG2);
   PRE_REG_READ2(long, "futimes", int, fd, struct timeval *, times);
   if (ARG2 != 0) {
      PRE_MEM_READ( "futimes(times)", ARG2, sizeof(struct vki_timeval) );
   }
}

// SYS_getpgid 207
// generic

// SYS_poll 209
// generic

// SYS_freebsd7___semctl   220
// int semctl(int semid, int semnum, int cmd, ...);
PRE(sys_freebsd7___semctl)
{
   switch (ARG3) {
   case VKI_IPC_INFO:
   case VKI_SEM_INFO:
      PRINT("sys_semctl ( %" FMT_REGWORD "u, %" FMT_REGWORD "u, %" FMT_REGWORD "u, %#" FMT_REGWORD "x )",ARG1,ARG2,ARG3,ARG4);
      PRE_REG_READ4(int, "semctl",
                    int, semid, int, semnum, int, cmd, struct seminfo *, arg);
      break;
   case VKI_IPC_STAT:
   case VKI_SEM_STAT:
   case VKI_IPC_SET:
      PRINT("sys_semctl ( %" FMT_REGWORD "u, %" FMT_REGWORD "u, %" FMT_REGWORD "u, %#" FMT_REGWORD "x )",ARG1,ARG2,ARG3,ARG4);
      PRE_REG_READ4(int, "semctl",
                    int, semid, int, semnum, int, cmd, struct vki_semid_ds_old *, arg);
      break;
   case VKI_GETALL:
   case VKI_SETALL:
      PRINT("sys_semctl ( %" FMT_REGWORD "u, %" FMT_REGWORD "u, %" FMT_REGWORD "u, %#" FMT_REGWORD "x )",ARG1,ARG2,ARG3,ARG4);
      PRE_REG_READ4(int, "semctl",
                    int, semid, int, semnum, int, cmd, unsigned short *, arg);
      break;
   default:
      PRINT("sys_semctl ( %" FMT_REGWORD "u, %" FMT_REGWORD "u, %" FMT_REGWORD "u )",ARG1,ARG2,ARG3);
      PRE_REG_READ3(long, "semctl",
                    int, semid, int, semnum, int, cmd);
      break;
   }
   ML_(generic_PRE_sys_semctl)(tid, ARG1,ARG2,ARG3,ARG4);
}

POST(sys_freebsd7___semctl)
{
   ML_(generic_POST_sys_semctl)(tid, RES,ARG1,ARG2,ARG3,ARG4);
}

// SYS_semget  221
// int semget(key_t key, int nsems, int flag);
PRE(sys_semget)
{
   PRINT("sys_semget ( %" FMT_REGWORD "u, %" FMT_REGWORD "u, %" FMT_REGWORD "u )",ARG1,ARG2,ARG3);
   PRE_REG_READ3(int, "semget", vki_key_t, key, int, nsems, int, flag);
}

// SYS_semop   222
// int semop(int semid, struct sembuf *array, size_t nops);
PRE(sys_semop)
{
   *flags |= SfMayBlock;
   PRINT("sys_semop ( %" FMT_REGWORD "u, %#" FMT_REGWORD "x, %" FMT_REGWORD "u )",ARG1,ARG2,ARG3);
   PRE_REG_READ3(int, "semop",
                 int, semid, struct sembuf *, array, unsigned, nops);
   ML_(generic_PRE_sys_semop)(tid, ARG1,ARG2,ARG3);
}

// SYS_freebsd7_msgctl  224
// int msgctl(int msqid, int cmd, struct msqid_ds_old *buf);
PRE(sys_freebsd7_msgctl)
{
   PRINT("sys_freebsd7_msgctl ( %" FMT_REGWORD "d, %" FMT_REGWORD "d, %#" FMT_REGWORD "x )", SARG1,SARG2,ARG3 );

   PRE_REG_READ3(int, "msgctl", int, msqid, int, cmd, struct msqid_ds_old *, buf);

   switch (ARG2 /* cmd */) {
   case VKI_IPC_STAT:
      PRE_MEM_WRITE( "msgctl(IPC_STAT, buf)",
                     ARG3, sizeof(struct vki_msqid_ds_old) );
      break;
   case VKI_IPC_SET:
      PRE_MEM_READ( "msgctl(IPC_SET, buf)",
                    ARG3, sizeof(struct vki_msqid_ds_old) );
      break;
   }
}

POST(sys_freebsd7_msgctl)
{
   switch (ARG2 /* cmd */) {
   case VKI_IPC_STAT:
      POST_MEM_WRITE( ARG3, sizeof(struct vki_msqid_ds_old) );
      break;
   }
}

// SYS_msgget  225
// int msgget(key_t key, int msgflg);
PRE(sys_msgget)
{
   PRINT("sys_msgget ( %" FMT_REGWORD"d, %" FMT_REGWORD"d )",SARG1,SARG2);
   PRE_REG_READ2(int, "msgget", key_t, key, int, msgflg);
}

// SYS_msgsnd  226
// int msgsnd(int msqid, struct msgbuf *msgp, size_t msgsz, int msgflg);
PRE(sys_msgsnd)
{
   PRINT("sys_msgsnd ( %" FMT_REGWORD "d, %#" FMT_REGWORD "x, %" FMT_REGWORD "d, %" FMT_REGWORD "d )", SARG1,ARG2,SARG3,SARG4 );
   PRE_REG_READ4(int, "msgsnd", int, msqid, struct msgbuf *, msgp, size_t, msgsz, int, msgflg);
   struct vki_msgbuf *msgp = (struct vki_msgbuf *)ARG2;
   PRE_MEM_READ( "msgsnd(msgp->mtype)", (Addr)&msgp->mtype, sizeof(msgp->mtype) );
   PRE_MEM_READ( "msgsnd(msgp->mtext)", (Addr)&msgp->mtext, ARG3 );
}
// SYS_msgrcv  227
// ssize_t msgrcv(int msqid, struct msgbuf *msgp, size_t msgsz, long msgtyp, int msgflg);
PRE(sys_msgrcv)
{
   *flags |= SfMayBlock;

   PRINT("sys_msgrcv ( %" FMT_REGWORD "d, %#" FMT_REGWORD "x, %" FMT_REGWORD "u, %" FMT_REGWORD "d, %" FMT_REGWORD "d )", SARG1,ARG2,ARG3,SARG4,SARG5 );
   PRE_REG_READ5(ssize_t, "msgrcv", int, msqid, struct msgbuf *, msgp, size_t, msgsz,
                 long, msgtyp, int, msgflg);
   struct vki_msgbuf *msgp = (struct vki_msgbuf *)ARG2;
   PRE_MEM_WRITE( "msgrcv(msgp->mtype)", (Addr)&msgp->mtype, sizeof(msgp->mtype) );
   PRE_MEM_WRITE( "msgrcv(msgp->mtext)", (Addr)&msgp->mtext, ARG3 );
}

POST(sys_msgrcv)
{
   struct vki_msgbuf *msgp = (struct vki_msgbuf *)ARG2;
   POST_MEM_WRITE( (Addr)&msgp->mtype, sizeof(msgp->mtype) );
   POST_MEM_WRITE( (Addr)&msgp->mtext, RES );
}

// SYS_shmat   228
// void * shmat(int shmid, const void *addr, int flag);
PRE(sys_shmat)
{
   UWord arg2tmp;
   PRINT("sys_shmat ( %" FMT_REGWORD "u, %#" FMT_REGWORD "x, %" FMT_REGWORD "u )",ARG1,ARG2,ARG3);
   PRE_REG_READ3(void *, "shmat",
                 int, shmid, const void *, addr, int, flag);
   arg2tmp = ML_(generic_PRE_sys_shmat)(tid, ARG1,ARG2,ARG3);
   if (arg2tmp == 0) {
      SET_STATUS_Failure( VKI_EINVAL );
   } else {
      ARG2 = arg2tmp;
   }
}

POST(sys_shmat)
{
   ML_(generic_POST_sys_shmat)(tid, RES,ARG1,ARG2,ARG3);
}

// SYS_freebsd7_shmctl  229
// int shmctl(int shmid, int cmd, struct shmid_ds *buf);
PRE(sys_freebsd7_shmctl)
{
   PRINT("sys_freebsd7_shmctl ( %" FMT_REGWORD "d, %" FMT_REGWORD "d, %#" FMT_REGWORD "x )",SARG1,SARG2,ARG3);
   PRE_REG_READ3(int, "shmctl",
                 int, shmid, int, cmd, struct vki_shmid_ds_old *, buf);
   switch (ARG2 /* cmd */) {
   case VKI_IPC_STAT:
      PRE_MEM_WRITE( "shmctl7(IPC_STAT, buf)",
                     ARG3, sizeof(struct vki_shmid_ds_old) );
      break;
   case VKI_IPC_SET:
      PRE_MEM_READ( "shmctl7(IPC_SET, buf)",
                    ARG3, sizeof(struct vki_shmid_ds_old) );
      break;
   }
}

POST(sys_freebsd7_shmctl)
{
   if (ARG2 == VKI_IPC_STAT) {
      POST_MEM_WRITE( ARG3, sizeof(struct vki_shmid_ds_old) );
   }
}

// SYS_shmdt   230
// int shmdt(const void *addr);
PRE(sys_shmdt)
{
   PRINT("sys_shmdt ( %#" FMT_REGWORD "x )",ARG1);
   PRE_REG_READ1(int, "shmdt", const void *, addr);
   if (!ML_(generic_PRE_sys_shmdt)(tid, ARG1)) {
      SET_STATUS_Failure( VKI_EINVAL );
   }
}

POST(sys_shmdt)
{
   ML_(generic_POST_sys_shmdt)(tid, RES,ARG1);
}

// SYS_shmget  231
// int shmget(key_t key, size_t size, int flag);
PRE(sys_shmget)
{
   PRINT("sys_shmget ( %" FMT_REGWORD "u, %" FMT_REGWORD "u, %" FMT_REGWORD "u )",ARG1,ARG2,ARG3);
   PRE_REG_READ3(int, "shmget", vki_key_t, key, vki_size_t, size, int, flag);
}


// SYS_clock_gettime 232
// int clock_gettime(clockid_t clock_id, struct timespec *tp);
PRE(sys_clock_gettime)
{
   PRINT("sys_clock_gettime( %" FMT_REGWORD "u, %#" FMT_REGWORD "x )", ARG1,ARG2);
   PRE_REG_READ2(int, "clock_gettime",
                 vki_clockid_t, clk_id, struct timespec *, tp);
   PRE_MEM_WRITE( "clock_gettime(tp)", ARG2, sizeof(struct vki_timespec) );
}

POST(sys_clock_gettime)
{
   POST_MEM_WRITE( ARG2, sizeof(struct vki_timespec) );
}

// SYS_clock_settime 233
// int clock_settime(clockid_t clock_id, const struct timespec *tp);
PRE(sys_clock_settime)
{
   PRINT("sys_clock_settime( %" FMT_REGWORD "u, %#" FMT_REGWORD "x )", ARG1,ARG2);
   PRE_REG_READ2(int, "clock_settime",
                 vki_clockid_t, clk_id, const struct timespec *, tp);
   PRE_MEM_READ( "clock_settime(tp)", ARG2, sizeof(struct vki_timespec) );
}

// SYS_clock_getres  234
// int clock_getres(clockid_t clock_id, struct timespec *tp);
PRE(sys_clock_getres)
{
   PRINT("sys_clock_getres( %" FMT_REGWORD "u, %#" FMT_REGWORD "x )", ARG1,ARG2);
   // Nb: we can't use "RES" as the param name because that's a macro
   // defined above!
   PRE_REG_READ2(int, "clock_getres",
                 vki_clockid_t, clock_id, struct timespec *, tp);
   if (ARG2 != 0) {
      PRE_MEM_WRITE( "clock_getres(tp)", ARG2, sizeof(struct vki_timespec) );
   }
}

POST(sys_clock_getres)
{
   if (ARG2 != 0) {
      POST_MEM_WRITE( ARG2, sizeof(struct vki_timespec) );
   }
}

// SYS_ktimer_create 235
// int      timer_create(clockid_t clockid, struct sigevent *restrict evp,
//                       timer_t *restrict timerid);
PRE(sys_timer_create)
{
   PRINT("sys_timer_create( %" FMT_REGWORD "d, %#" FMT_REGWORD "x, %#" FMT_REGWORD "x )", SARG1,ARG2,ARG3);
   PRE_REG_READ3(int, "timer_create",
                 vki_clockid_t, clockid, struct sigevent *, evp,
                 vki_timer_t *, timerid);
   if (ARG2 != 0) {
      PRE_MEM_READ( "timer_create(evp)", ARG2, sizeof(struct vki_sigevent) );
   }
   PRE_MEM_WRITE( "timer_create(timerid)", ARG3, sizeof(vki_timer_t) );
}

POST(sys_timer_create)
{
   POST_MEM_WRITE( ARG3, sizeof(vki_timer_t) );
}

// SYS_ktimer_delete 236
// int timer_delete(timer_t timerid);
PRE(sys_timer_delete)
{
   PRINT("sys_timer_delete( %#" FMT_REGWORD "x )", ARG1);
   PRE_REG_READ1(long, "timer_delete", vki_timer_t, timerid);
}

// SYS_ktimer_settime   237
// int timer_settime(timer_t timerid, int flags,
//                   const struct itimerspec *restrict value,
//                   struct itimerspec *restrict ovalue);
PRE(sys_timer_settime)
{
   PRINT("sys_timer_settime( %#" FMT_REGWORD "x, %" FMT_REGWORD "d, %#" FMT_REGWORD "x, %#" FMT_REGWORD "x )", ARG1,SARG2,ARG3,ARG4);
   PRE_REG_READ4(int, "timer_settime",
                 vki_timer_t, timerid, int, flags,
                 const struct itimerspec *, value,
                 struct itimerspec *, ovalue);
   PRE_MEM_READ( "timer_settime(value)", ARG3,
                 sizeof(struct vki_itimerspec) );
   if (ARG4 != 0) {
      PRE_MEM_WRITE( "timer_settime(ovalue)", ARG4,
                     sizeof(struct vki_itimerspec) );
   }
}

POST(sys_timer_settime)
{
   if (ARG4 != 0) {
      POST_MEM_WRITE( ARG4, sizeof(struct vki_itimerspec) );
   }
}

// SYS_ktimer_gettime   238
// int timer_gettime(timer_t timerid, struct itimerspec *value);
PRE(sys_timer_gettime)
{
   PRINT("sys_timer_gettime( %#" FMT_REGWORD "x, %#" FMT_REGWORD "x )", ARG1,ARG2);
   PRE_REG_READ2(long, "timer_gettime",
                 vki_timer_t, timerid, struct itimerspec *, value);
   PRE_MEM_WRITE( "timer_gettime(value)", ARG2,
                  sizeof(struct vki_itimerspec));
}

POST(sys_timer_gettime)
{
   POST_MEM_WRITE( ARG2, sizeof(struct vki_itimerspec) );
}

// SYS_ktimer_getoverrun   239
// int timer_getoverrun(timer_t timerid);
PRE(sys_timer_getoverrun)
{
   PRINT("sys_timer_getoverrun( %#" FMT_REGWORD "x )", ARG1);
   PRE_REG_READ1(int, "timer_getoverrun", vki_timer_t, timerid);
}

// SYS_nanosleep  240
// generic

// SYS_ffclock_getcounter  241
// int ffclock_getcounter(ffcounter *ffcount);
// @todo

// SYS_ffclock_setestimate 242
// int ffclock_setestimate(struct ffclock_estimate *cest);
// @todo

// SYS_ffclock_getestimate 243
// int ffclock_getestimate(struct ffclock_estimate *cest);
// @todo

// SYS_clock_nanosleep 244
// int clock_nanosleep(clockid_t clock_id, int flags,
//                     const struct timespec *rqtp, struct timespec *rmtp);
PRE(sys_clock_nanosleep)
{
   *flags |= SfMayBlock|SfPostOnFail;
   PRINT("sys_clock_nanosleep ( %" FMT_REGWORD "d, %" FMT_REGWORD "d, %#" FMT_REGWORD "x, %#" FMT_REGWORD "x )",
         SARG1, SARG2, ARG3, ARG4);
   PRE_REG_READ4(int, "clock_nanosleep", clockid_t, clock_id, int, flags,
                 const struct timespec *, rqtp, struct timespec *, rmtp);
   PRE_MEM_READ("clock_nanosleep(rqtp)", ARG1, sizeof(struct vki_timespec));
   if (ARG2 != 0) {
      PRE_MEM_WRITE( "clock_nanosleep(rmtp)", ARG2, sizeof(struct vki_timespec) );
   }
}

POST(sys_clock_nanosleep)
{
   if (ARG2 != 0) {
      POST_MEM_WRITE( ARG2, sizeof(struct vki_timespec) );
   }
}

// SYS_clock_getcpuclockid2   247
// x86/amd64

POST(sys_clock_getcpuclockid2)
{
   POST_MEM_WRITE(ARG3, sizeof(vki_clockid_t));
}


// SYS_ntp_gettime   248
// int ntp_gettime(struct ntptimeval *);
// @todo

// SYS_minherit   250
// int minherit(void *addr, size_t len, int inherit);
PRE(sys_minherit)
{
   PRINT("sys_minherit( %#" FMT_REGWORD "x, %" FMT_REGWORD "u, %" FMT_REGWORD "u )", ARG1,ARG2,ARG3);
   PRE_REG_READ3(int, "minherit",
                 void *, addr, vki_size_t, len, int, inherit);
   if (ARG2 != 0) {
      PRE_MEM_WRITE( "minherit(addr)", ARG1,ARG2 );
   }
}

POST(sys_minherit)
{
   if (ARG2 != 0) {
      POST_MEM_WRITE( ARG1, ARG2 );
   }
}

// SYS_rfork   251
// x86/amd64 not functional

// SYS_issetugid  253
// int issetugid(void);
PRE(sys_issetugid)
{
   PRINT("%s", "sys_issetugid ()");
   PRE_REG_READ0(long, "issetugid");
}

// SYS_lchown  254
// generic

// We must record the iocb for each aio_read() in a table so that when
// aio_return() is called we can mark the memory written asynchronously by
// aio_read() as having been written.  We don't have to do this for
// aio_write().  See bug 197227 for more details.
static OSet* iocb_table = NULL;
static Bool aio_init_done = False;

static void aio_init(void)
{
   iocb_table = VG_(OSetWord_Create)(VG_(malloc), "syswrap.aio", VG_(free));
   aio_init_done = True;
}

// and the same thing for vector reads
static OSet* iocbv_table = NULL;
static Bool aiov_init_done = False;

static void aiov_init(void)
{
   iocbv_table = VG_(OSetWord_Create)(VG_(malloc), "syswrap.aiov", VG_(free));
   aiov_init_done = True;
}


// SYS_aio_read   255
// int aio_read(struct aiocb *iocb);
PRE(sys_aio_read)
{
   PRINT("sys_aio_read ( %#" FMT_REGWORD "x )", ARG1);
   PRE_REG_READ1(int, "aio_read", struct vki_aiocb *, iocb);
   PRE_MEM_READ("aio_read(iocb)", ARG1, sizeof(struct vki_aiocb));
   if (ML_(safe_to_deref)((struct vki_aiocb *)ARG1, sizeof(struct vki_aiocb))) {
      struct vki_aiocb *iocb = (struct vki_aiocb *)ARG1;
      if (!ML_(fd_allowed)(iocb->aio_fildes, "aio_read", tid, False)) {
         SET_STATUS_Failure(VKI_EBADF);
      } else {
         PRE_MEM_WRITE("aio_read(aiocbp->aio_buf)",
                       (Addr)iocb->aio_buf, iocb->aio_nbytes);
         // @todo PJF there is a difference between FreeBSD and
         // Darwin here. On Darwin, if aio_buf is NULL the syscall
         // will fail, on FreeBSD it doesn't fail.
      }
   } else {
      SET_STATUS_Failure(VKI_EINVAL);
   }
}

POST(sys_aio_read)
{
   struct vki_aiocb* iocb = (struct vki_aiocb*)ARG1;

   if (iocb->aio_buf) {
      if (!aio_init_done) {
         aio_init();
      }
      // see also POST(sys_aio_readv)
      if (!VG_(OSetWord_Contains)(iocb_table, (UWord)iocb)) {
         VG_(OSetWord_Insert)(iocb_table, (UWord)iocb);
      } else {
         // @todo PJF this warns without callstack
         VG_(dmsg)("Warning: Duplicate control block %p in aio_read\n",
                   (void *)(Addr)ARG1);
         VG_(dmsg)("Warning: Ensure 'aio_return' is called when 'aio_read' has completed\n");
      }
   }
}

// SYS_aio_write  256
// int aio_write(struct aiocb *iocb);
PRE(sys_aio_write)
{
   PRINT("sys_aio_write ( %#" FMT_REGWORD "x )", ARG1);
   PRE_REG_READ1(int, "aio_write", struct vki_aiocb *, iocb);
   PRE_MEM_READ("aio_write(iocb)", ARG1, sizeof(struct vki_aiocb));
   if (ML_(safe_to_deref)((struct vki_aiocb *)ARG1, sizeof(struct vki_aiocb))) {
      struct vki_aiocb *iocb = (struct vki_aiocb *)ARG1;
      if (!ML_(fd_allowed)(iocb->aio_fildes, "aio_write", tid, False)) {
         SET_STATUS_Failure( VKI_EBADF );
      } else {
         PRE_MEM_READ("aio_write(iocb->aio_buf)",
                      (Addr)iocb->aio_buf, iocb->aio_nbytes);
         // @todo PJF there is a difference between FreeBSD and
         // Darwin here. On Darwin, if aio_buf is NULL the syscall
         // will fail, on FreeBSD it doesn't fail.
      }
   } else {
      SET_STATUS_Failure(VKI_EINVAL);
   }
}

// SYS_lio_listio 257
// int lio_listio(int mode, struct aiocb * const list[], int nent,
//                struct sigevent *sig);
PRE(sys_lio_listio)
{
   PRINT("sys_lio_listio ( %" FMT_REGWORD "d, %#" FMT_REGWORD "x, %" FMT_REGWORD "d, %#" FMT_REGWORD "x )",
         SARG1, ARG2, SARG3, ARG4);
   PRE_REG_READ4(int, "lio_listio", int, mode, struct aiocb * const *, list, int, nent,
                 struct sigevent *,sig);
   PRE_MEM_READ("lio_listio(list)", ARG2, ARG3*sizeof(struct vki_aiocb *));
   // loop check elements
   if (ML_(safe_to_deref)((struct vki_aiocb **)ARG2, ARG3*sizeof(struct vki_aiocb *))) {
      struct vki_aiocb** list = (struct vki_aiocb **)ARG2;
      for (int i = 0; i < (int)ARG3; ++i) {
         if (list[i]) {
            PRE_MEM_READ("lio_listio(list[?])", (Addr)list[i], ARG3*sizeof(struct vki_aiocb));
         }
         // @todo
         // figure out what gets read/written
         // when list[i]->aio_lio_opcode == VKI_LIO_READ and
         // when list[i]->aio_lio_opcode == VKI_LIO_WRITE
         //if (ML_(safe_to_deref)(list[i], ARG3*sizeof(struct vki_aiocb))) {
         //}
      }
   }

   if (ARG1 & VKI_LIO_WAIT) {
      *flags |= SfMayBlock;
   }

   if (ARG4 && (ARG1 == VKI_LIO_NOWAIT)) {
      PRE_MEM_READ("lio_listio(sig)", ARG4, sizeof(struct vki_sigevent));
   }
}

// SYS_freebsd11_getdents  272
// generic

// SYS_lchmod  274
// int lchmod(const char *path, mode_t mode);
PRE(sys_lchmod)
{
   PRINT("sys_lchmod ( %#" FMT_REGWORD "x(%s), %" FMT_REGWORD "u )", ARG1,(char *)ARG1,ARG2);
   PRE_REG_READ2(int, "lchmod", const char *, path, vki_mode_t, mode);
   PRE_MEM_RASCIIZ( "lchmod(path)", ARG1 );
}

// SYS_lutimes 276
// int lutimes(const char *path, const struct timeval *times);
PRE(sys_lutimes)
{
   PRINT("sys_lutimes ( %#" FMT_REGWORD "x(%s), %#" FMT_REGWORD "x )", ARG1,(char *)ARG1,ARG2);
   PRE_REG_READ2(int, "lutimes", char *, path, struct timeval *, times);
   PRE_MEM_RASCIIZ( "lutimes(path)", ARG1 );
   if (ARG2 != 0) {
      PRE_MEM_READ( "lutimes(times)", ARG2, sizeof(struct vki_timeval) );
   }
}

// SYS_freebsd11_nstat  278
// @todo, maybe

// SYS_freebsd11_nfstat 279
// @todo, maybe

// SYS_freebsd11_nlstat 280
// @todo, maybe

// SYS_preadv  289
// amd64 / x86

// SYS_pwritev 290
// amd64 / x86

// SYS_fhopen  298
// int fhopen(const fhandle_t *fhp, int flags);
PRE(sys_fhopen)
{
   PRINT("sys_open ( %#" FMT_REGWORD "x, %" FMT_REGWORD "u )",ARG1,ARG2);
   PRE_REG_READ2(int, "fhopen",
                 struct fhandle_t *, fhp, int, flags);
   PRE_MEM_READ( "fhopen(fhp)", ARG1, sizeof(struct vki_fhandle) );

   /* Otherwise handle normally */
   *flags |= SfMayBlock;
}

POST(sys_fhopen)
{
   vg_assert(SUCCESS);
   if (!ML_(fd_allowed)(RES, "fhopen", tid, True)) {
      VG_(close)(RES);
      SET_STATUS_Failure( VKI_EMFILE );
   } else {
      if (VG_(clo_track_fds)) {
         ML_(record_fd_open_nameless)(tid, RES);
      }
   }
}

// SYS_freebsd11_fhstat 299
// int fhstat(const fhandle_t *fhp, struct stat *sb);
#if (FREEBSD_VERS >= FREEBSD_12)
PRE(sys_freebsd11_fhstat)
{
   PRINT("sys_freebsd11_fhstat ( %#" FMT_REGWORD "x, %#" FMT_REGWORD "x )",ARG1,ARG2);
   PRE_REG_READ2(int, "fhstat", struct fhandle *, fhp, struct freebd11_stat *, sb);
   PRE_MEM_READ( "fhstat(fhp)", ARG1, sizeof(struct vki_fhandle) );
   PRE_MEM_WRITE( "fhstat(sb)", ARG2, sizeof(struct vki_freebsd11_stat) );
}

POST(sys_freebsd11_fhstat)
{
   POST_MEM_WRITE( ARG2, sizeof(struct vki_freebsd11_stat) );
}
#else
PRE(sys_fhstat)
{
   PRINT("sys_fhstat ( %#" FMT_REGWORD "x, %#" FMT_REGWORD "x )",ARG1,ARG2);
   PRE_REG_READ2(int, "fhstat", struct fhandle *, fhp, struct stat *, sb);
   PRE_MEM_READ( "fhstat(fhp)", ARG1, sizeof(struct vki_fhandle) );
   PRE_MEM_WRITE( "fhstat(sb)", ARG2, sizeof(struct vki_freebsd11_stat) );
}

POST(sys_fhstat)
{
   POST_MEM_WRITE( ARG2, sizeof(struct vki_freebsd11_stat) );
}

#endif

// SYS_modnext 300
// int modnext(int modid);
PRE(sys_modnext)
{
   PRINT("sys_modnext ( %" FMT_REGWORD "d )",SARG1);
   PRE_REG_READ1(int, "modnext", int, modid);
}

// SYS_modstat 301
// int modstat(int modid, struct module_stat *stat);
PRE(sys_modstat)
{
   PRINT("sys_modstat ( %" FMT_REGWORD "u, %#" FMT_REGWORD "x )",ARG1,ARG2);
   PRE_REG_READ2(int, "modstat", int, modid, struct module_stat *, buf);
   PRE_MEM_WRITE( "modstat(stat)", ARG2, sizeof(struct vki_module_stat) );
}

POST(sys_modstat)
{
   POST_MEM_WRITE( ARG2, sizeof(struct vki_module_stat) );
}

// SYS_modfnext   302
// int modfnext(int modid);
PRE(sys_modfnext)
{
   PRINT("sys_modfnext ( %" FMT_REGWORD "d )",SARG1);
   PRE_REG_READ1(int, "modfnext", int, modid);
}

// SYS_modfind 303
// int modfind(const char *modname);
PRE(sys_modfind)
{
   PRINT("sys_modfind ( %#" FMT_REGWORD "x )",ARG1);
   PRE_REG_READ1(long, "modfind", char *, modname);
   PRE_MEM_RASCIIZ( "modfind(modname)", ARG1 );
}

// SYS_kldload 304
// int kldload(const char *file);
PRE(sys_kldload)
{
   PRINT("sys_kldload ( %#" FMT_REGWORD "x(%s) )", ARG1, (char *)ARG1);
   PRE_REG_READ1(int, "kldload", const char *, "file");
   PRE_MEM_RASCIIZ( "kldload(file)", ARG1 );
}

// SYS_kldunload  305
// int kldunload(int fileid);
PRE(sys_kldunload)
{
   PRINT("sys_kldunload ( %" FMT_REGWORD "u )", ARG1);
   PRE_REG_READ1(int, "kldunload", int, "fileid");
}

// SYS_kldfind 306
// int kldfind(const char *file);
PRE(sys_kldfind)
{
   PRINT("sys_kldfind ( %#" FMT_REGWORD "x(%s) )", ARG1, (char *)ARG1);
   PRE_REG_READ1(int, "kldfind", const char *, file);
   PRE_MEM_RASCIIZ( "kldfind(file)", ARG1 );
}

// SYS_kldnext 307
// int kldnext(int fileid);
PRE(sys_kldnext)
{
   PRINT("sys_kldnext ( %" FMT_REGWORD "u )", ARG1);
   PRE_REG_READ1(int, "kldnext", int, fileid);
}

// SYS_kldstat 308
// int kldstat(int fileid, struct kld_file_stat *stat);
PRE(sys_kldstat)
{
   PRINT("sys_kldstat ( %" FMT_REGWORD "d, %#" FMT_REGWORD "x )", SARG1, ARG2);
   PRE_REG_READ2(int, "kldstat", int, fileid, struct kld_file_stat*, stat);
   PRE_MEM_WRITE("kldstat(stat)", ARG2, sizeof(struct vki_kld_file_stat));
}

POST(sys_kldstat)
{
   POST_MEM_WRITE(ARG2, sizeof(struct vki_kld_file_stat));
}

// SYS_kldfirstmod   309
// int kldfirstmod(int fileid);
PRE(sys_kldfirstmod)
{
   PRINT("sys_kldfirstmod ( %" FMT_REGWORD "u )", ARG1);
   PRE_REG_READ1(int, "kldfirstmod", int, fileid);
}

// SYS_setresuid  311
// int setresuid(uid_t *ruid, uid_t *euid, uid_t *suid);
PRE(sys_setresuid)
{
   PRINT("sys_setresuid ( %" FMT_REGWORD "u, %" FMT_REGWORD "u, %" FMT_REGWORD "u )", ARG1, ARG2, ARG3);
   PRE_REG_READ3(int, "setresuid",
                 vki_uid_t, ruid, vki_uid_t, euid, vki_uid_t, suid);
}

// SYS_setresgid  312
// int setresgid(gid_t rgid, gid_t egid, gid_t sgid);
PRE(sys_setresgid)
{
   PRINT("sys_setresgid ( %" FMT_REGWORD "u, %" FMT_REGWORD "u, %" FMT_REGWORD "u )", ARG1, ARG2, ARG3);
   PRE_REG_READ3(int, "setresgid",
                 vki_gid_t, rgid, vki_gid_t, egid, vki_gid_t, sgid);
}

// SYS_aio_return 314
// ssize_t aio_return(struct aiocb *iocb);
PRE(sys_aio_return)
{
   PRINT("sys_aio_return ( %#" FMT_REGWORD "x )", ARG1);
   PRE_REG_READ1(ssize_t, "aio_return", struct aiocb *, iocb);
   PRE_MEM_READ("aio_return(iocb)", ARG1, sizeof(struct vki_aiocb));
   // read or write?
   if (ML_(safe_to_deref)((struct vki_aiocb *)ARG1, sizeof(struct vki_aiocb))) {
      SET_STATUS_from_SysRes(VG_(do_syscall1)(SYSNO, ARG1));
      /*if (SUCCESS)*/ {
         struct vki_aiocb* iocb = (struct vki_aiocb*)ARG1;
         if (!aio_init_done) {
            aio_init();
         }
         if (!aiov_init_done) {
            aiov_init();
         }

         // for the happy path aio_return is supposed to be called
         // after the io has completed (as determined by aio_error,
         // aio_suspend or a signal).

         // but what if the aio_read failed or hasn't completed?
         // we want to remove the read from the iocb(v)_table
         // in the case of aio_read failing
         // if the read hasn't completed that's a user error
         // I don't know if it's possible to recover in that case
         // the iocb will have been removed from the table
         // so if the user does recover and call aio_return
         // 'correctly' we won't do the POST_MEM_WRITE
         // I don't think that we can tell apart a failing
         // read from a premature aio_return

         // check if it was a plain read
         if (VG_(OSetWord_Remove)(iocb_table, (UWord)iocb) && SUCCESS) {
            POST_MEM_WRITE((Addr)iocb->aio_buf, iocb->aio_nbytes);
         }
         if (VG_(OSetWord_Remove)(iocbv_table, (UWord)iocb) && SUCCESS) {
            SizeT vec_count = (SizeT)iocb->aio_nbytes;
            // assume that id the read succeded p_iovec is accessible
            volatile struct vki_iovec* p_iovec  = (volatile struct vki_iovec*)iocb->aio_buf;
            for (SizeT i = 0U; i < vec_count; ++i) {
               POST_MEM_WRITE((Addr)p_iovec[i].iov_base, p_iovec[i].iov_len);
            }
         }
      }
   } else {
      SET_STATUS_Failure(VKI_EINVAL);
   }
}

// SYS_aio_suspend   315
// int aio_suspend(const struct aiocb *const iocbs[], int niocb,
//                 const struct timespec *timeout);
PRE(sys_aio_suspend)
{
   PRINT("sys_aio_suspend ( %#" FMT_REGWORD "x )", ARG1);
   PRE_REG_READ3(int, "aio_suspend", const struct aiocb * const *, iocbs, int, nbiocb, const struct timespec*, timeout);
   if (ARG2 > 0) {
      PRE_MEM_READ("aio_suspend(iocbs)", ARG1, ARG2*sizeof(struct vki_aiocb*));
   }
   if (ARG3) {
      PRE_MEM_READ("aio_suspend(timeout)", ARG3, sizeof(struct vki_timespec));
   }
}

// SYS_aio_cancel 316
// int aio_cancel(int fildes, struct aiocb *iocb);
PRE(sys_aio_cancel)
{
   PRINT("sys_aio_cancel ( %" FMT_REGWORD "d, %#" FMT_REGWORD "x )", SARG1, ARG2);
   PRE_REG_READ2(int, "aio_cancel", int, fildes, struct iocb *, iocb);
   if (ARG2) {
      PRE_MEM_READ("aio_cancel(iocb)", ARG2, sizeof(struct vki_aiocb));
   }
   if (!ML_(fd_allowed)(ARG1, "aio_cancel", tid, False)) {
      SET_STATUS_Failure(VKI_EBADF);
   } else {
      if (ARG2) {
         if (ML_(safe_to_deref)((struct vki_aiocb *)ARG2, sizeof(struct vki_aiocb))) {
            // struct vki_aiocb *iocb = (struct vki_aiocb *)ARG2;
            // @todo PJF cancel only requests associated with
            // fildes and iocb
            // Do I need to remove pending reads from iocb(v)_table
            // or should the user always call aio_return even after
            // aio_cancel?
         } else {
            SET_STATUS_Failure(VKI_EINVAL);
         }
      } else {
         // @todo PJF cancel all requests associated with fildes, see above
      }
   }
}

// SYS_aio_error  317
// int aio_error(const struct aiocb *iocb);
PRE(sys_aio_error)
{
   PRINT("sys_aio_error ( %#" FMT_REGWORD "x )", ARG1);
   PRE_REG_READ1(ssize_t, "aio_error", struct aiocb *, iocb);
   PRE_MEM_READ("aio_error(iocb)", ARG1, sizeof(struct vki_aiocb));
}

// SYS_yield   321
int yield(void);
PRE(sys_yield)
{
   *flags |= SfMayBlock;
   PRINT("%s", "yield()");
   PRE_REG_READ0(long, "yield");
}

// SYS_mlockall   324
// generic

// SYS_munlockall 325
// int munlockall(void);
PRE(sys_munlockall)
{
   *flags |= SfMayBlock;
   PRINT("%s", "sys_munlockall ( )");
   PRE_REG_READ0(int, "munlockall");
}

// SYS___getcwd   326
// int __getcwd(char *buf, size_t buflen);
PRE(sys___getcwd)
{
   PRINT("sys___getcwd ( %#" FMT_REGWORD "x, %" FMT_REGWORD "u )", ARG1,ARG2);
   PRE_REG_READ2(long, "__getcwd", char *, buf, unsigned int, buflen);
   PRE_MEM_WRITE( "__getcwd(buf)", ARG1, ARG2 );
}

POST(sys___getcwd)
{
   vg_assert(SUCCESS);
   if (RES == 0) {
      // QQQ it is unclear if this is legal or not, but the
      // QQQ kernel just wrote it there...
      // QQQ Why oh why didn't phk return the length from __getcwd()?
      UInt len = VG_(strlen) ( (char *)ARG1 ) + 1;
      POST_MEM_WRITE( ARG1, len );
   }
}

//SYS_sched_setparam 327
// int sched_setparam(pid_t pid, const struct sched_param *param);
PRE(sys_sched_setparam)
{
   PRINT("sched_setparam ( %" FMT_REGWORD "d, %#" FMT_REGWORD "x )", SARG1, ARG2 );
   PRE_REG_READ2(int, "sched_setparam",
                 vki_pid_t, pid, struct sched_param *, param);
   PRE_MEM_READ( "sched_setparam(param)", ARG2, sizeof(struct vki_sched_param) );
}

POST(sys_sched_setparam)
{
   POST_MEM_WRITE( ARG2, sizeof(struct vki_sched_param) );
}

// SYS_sched_getparam   328
// int sched_getparam(pid_t pid, struct sched_param *param);
PRE(sys_sched_getparam)
{
   PRINT("sched_getparam ( %" FMT_REGWORD "d, %#" FMT_REGWORD "x )", SARG1, ARG2 );
   PRE_REG_READ2(int, "sched_getparam",
                 vki_pid_t, pid, struct sched_param *, param);
   PRE_MEM_WRITE( "sched_getparam(param)", ARG2, sizeof(struct vki_sched_param) );
}

POST(sys_sched_getparam)
{
   POST_MEM_WRITE( ARG2, sizeof(struct vki_sched_param) );
}

// SYS_sched_setscheduler  329
// int sched_setscheduler(pid_t pid, int policy,
//                        const struct sched_param *param);
PRE(sys_sched_setscheduler)
{
   PRINT("sys_sched_setscheduler ( %" FMT_REGWORD "d, %" FMT_REGWORD "d, %#" FMT_REGWORD "x )", SARG1,SARG2,ARG3);
   PRE_REG_READ3(int, "sched_setscheduler",
                 vki_pid_t, pid, int, policy, struct sched_param *, param);
   if (ARG3 != 0) {
      PRE_MEM_READ("sched_setscheduler(param)",
                   ARG3, sizeof(struct vki_sched_param));
   }
}

// SYS_sched_getscheduler  330
// int sched_getscheduler(pid_t pid);
PRE(sys_sched_getscheduler)
{
   PRINT("sys_sched_getscheduler ( %" FMT_REGWORD "d )", SARG1);
   PRE_REG_READ1(int, "sched_getscheduler", vki_pid_t, pid);
}

// SYS_sched_yield   331
// int sched_yield(void);
PRE(sys_sched_yield)
{
   *flags |= SfMayBlock;
   PRINT("sched_yield()");
   PRE_REG_READ0(int, "sched_yield");
}

// SYS_sched_get_priority_max 332
// int sched_get_priority_max(int policy);
PRE(sys_sched_get_priority_max)
{
   PRINT("sched_get_priority_max ( %" FMT_REGWORD "u )", ARG1);
   PRE_REG_READ1(long, "sched_get_priority_max", int, policy);
}

// SYS_sched_get_priority_min 333
// int sched_get_priority_min(int policy);
PRE(sys_sched_get_priority_min)
{
   PRINT("sched_get_priority_min ( %" FMT_REGWORD "u )", ARG1);
   PRE_REG_READ1(long, "sched_get_priority_min", int, policy);
}

// SYS_sched_rr_get_interval  334
// int sched_rr_get_interval(pid_t pid, struct timespec *interval);
PRE(sys_sched_rr_get_interval)
{
   PRINT("sys_sched_rr_get_interval ( %" FMT_REGWORD "d, %#" FMT_REGWORD "x )", SARG1, ARG2);
   PRE_REG_READ2(int, "sched_rr_get_interval", vki_pid_t, pid, struct vki_timespec *,interval);
   PRE_MEM_WRITE("sys_sched_rr_get_interval(interval)", ARG2, sizeof(struct vki_timespec));
}

POST(sys_sched_rr_get_interval)
{
   POST_MEM_WRITE(ARG2, sizeof(struct vki_timespec));
}

// SYS_utrace  335
// int utrace(const void *addr, size_t len);
PRE(sys_utrace)
{
   PRINT("sys_utrace ( %#" FMT_REGWORD "x, %" FMT_REGWORD "u )", ARG1, ARG2);
   PRE_REG_READ2(int, "utrace", const void *, addr, vki_size_t, len);
   PRE_MEM_READ( "utrace(addr)", ARG2, ARG3 );
}

// SYS_kldsym  337
// int kldsym(int fileid, int cmd, void *data);
PRE(sys_kldsym)
{
   PRINT("sys_kldsym ( %" FMT_REGWORD "u, %" FMT_REGWORD "u, %#" FMT_REGWORD "x )", ARG1,ARG2,ARG3 );
   PRE_REG_READ3(int, "kldsym", int, fileid, int, cmd, void*, data);
   PRE_MEM_READ( "kldsym(data)", ARG3, sizeof(struct vki_kld_sym_lookup) );
   struct vki_kld_sym_lookup *kslp = (struct vki_kld_sym_lookup *)ARG3;
   if (ML_(safe_to_deref)(kslp, sizeof(struct vki_kld_sym_lookup))) {
      PRE_MEM_RASCIIZ( "kldsym(data.symname)", (Addr)kslp->symname );
   }
}

POST(sys_kldsym)
{
   struct vki_kld_sym_lookup *kslp = (struct vki_kld_sym_lookup *)ARG3;
   POST_MEM_WRITE( (Addr)&kslp->symvalue, sizeof(kslp->symvalue) );
   POST_MEM_WRITE( (Addr)&kslp->symsize, sizeof(kslp->symsize) );
}

// SYS_jail 338
// int jail(struct jail *jail);
PRE(sys_jail)
{
   PRINT("sys_jail ( %#" FMT_REGWORD "x )", ARG1);
   PRE_REG_READ1(int, "jail", struct jail *, jail);
   PRE_MEM_READ( "jail(jail)", ARG1, sizeof(struct vki_jail) );
}

// SYS_nnpfs_syscall 338
// @todo

// SYS_sigprocmask   340
// int sigprocmask(int how, const sigset_t * restrict set,
//                 sigset_t * restrict oset);
PRE(sys_sigprocmask)
{
   PRINT("sys_sigprocmask ( %" FMT_REGWORD "u, %#" FMT_REGWORD "x, %#" FMT_REGWORD "x )",ARG1,ARG2,ARG3);
   PRE_REG_READ3(int, "sigprocmask",
                 int, how, vki_sigset_t *, set, vki_sigset_t *, oset);
   if (ARG2 != 0) {
      PRE_MEM_READ( "sigprocmask(set)", ARG2, sizeof(vki_sigset_t));
   }
   if (ARG3 != 0) {
      PRE_MEM_WRITE( "sigprocmask(oset)", ARG3, sizeof(vki_sigset_t));
   }

   if (ARG2 != 0  &&
         !ML_(safe_to_deref)((void *)(Addr)ARG2, sizeof(vki_sigset_t))) {
      VG_(dmsg)("Warning: Bad set handler address %p in sigprocmask\n",
                (void *)(Addr)ARG2);
      SET_STATUS_Failure ( VKI_EFAULT );
   } else if (ARG3 != 0 &&
              !ML_(safe_to_deref)((void *)(Addr)ARG3, sizeof(vki_sigset_t))) {
      VG_(dmsg)("Warning: Bad oldset address %p in sigprocmask\n",
                (void *)(Addr)ARG3);
      SET_STATUS_Failure ( VKI_EFAULT );
   } else {
      SET_STATUS_from_SysRes(VG_(do_sys_sigprocmask)(tid, ARG1 /*how*/,
                             (vki_sigset_t*)(Addr)ARG2,
                             (vki_sigset_t*)(Addr)ARG3));
   }

   if (SUCCESS) {
      *flags |= SfPollAfter;
   }
}

POST(sys_sigprocmask)
{
   vg_assert(SUCCESS);
   if (RES == 0 && ARG3 != 0) {
      POST_MEM_WRITE( ARG3, sizeof(vki_sigset_t));
   }
}

// SYS_sigsuspend 341
// int sigsuspend(const sigset_t *sigmask);
PRE(sys_sigsuspend)
{
   *flags |= SfMayBlock;
   PRINT("sys_sigsuspend ( %#" FMT_REGWORD "x )", ARG1 );
   PRE_REG_READ1(int, "sigsuspend", const vki_sigset_t *, sigmask);
   PRE_MEM_READ( "sigsuspend(sigmask)", ARG1, sizeof(vki_sigset_t) );
   if (ARG1) {
      ARG1 = ML_(make_safe_mask)("syswrap.sigsuspend.1", (Addr)ARG1);
   }
}

POST(sys_sigsuspend)
{
   ML_(free_safe_mask) ( (Addr)ARG1 );
}

// SYS_sigpending 343
// int sigpending(sigset_t *set);
PRE(sys_sigpending)
{
   PRINT( "sys_sigpending ( %#" FMT_REGWORD "x )", ARG1 );
   PRE_REG_READ1(int, "sigpending", vki_sigset_t *, set);
   PRE_MEM_WRITE( "sigpending(set)", ARG1, sizeof(vki_sigset_t));
}

POST(sys_sigpending)
{
   POST_MEM_WRITE( ARG1, sizeof(vki_sigset_t) ) ;
}


// SYS_sigtimedwait  345
// int sigtimedwait(const sigset_t *restrict set, siginfo_t *restrict info,
//                  const struct timespec *restrict timeout);
PRE(sys_sigtimedwait)
{
   *flags |= SfMayBlock;
   PRINT("sys_sigtimedwait ( %#" FMT_REGWORD "x, %#" FMT_REGWORD "x, %#" FMT_REGWORD "x )",
         ARG1,ARG2,ARG3);
   PRE_REG_READ3(int, "sigtimedwait",
                 const vki_sigset_t *, set, vki_siginfo_t *, info,
                 const struct timespec *, timeout);
   if (ARG1 != 0) {
      PRE_MEM_READ(  "sigtimedwait(set)",  ARG1, sizeof(vki_sigset_t));
   }
   if (ARG2 != 0) {
      PRE_MEM_WRITE( "sigtimedwait(info)", ARG2, sizeof(vki_siginfo_t) );
   }
   if (ARG3 != 0) {
      PRE_MEM_READ( "sigtimedwait(timeout)",
                    ARG3, sizeof(struct vki_timespec) );
   }
}

POST(sys_sigtimedwait)
{
   if (ARG2 != 0) {
      POST_MEM_WRITE( ARG2, sizeof(vki_siginfo_t) );
   }
}

// SYS_sigwaitinfo   346
// int sigwaitinfo(const sigset_t * restrict set, siginfo_t * restrict info);
PRE(sys_sigwaitinfo)
{
   *flags |= SfMayBlock;
   PRINT("sys_sigwaitinfo ( %#" FMT_REGWORD "x, %#" FMT_REGWORD "x )",
         ARG1,ARG2);
   PRE_REG_READ2(int, "sigwaitinfo",
                 const vki_sigset_t *, set, vki_siginfo_t *, info);
   if (ARG1 != 0) {
      PRE_MEM_READ(  "sigwaitinfo(set)",  ARG1, sizeof(vki_sigset_t));
   }
   if (ARG2 != 0) {
      PRE_MEM_WRITE( "sigwaitinfo(info)", ARG2, sizeof(vki_siginfo_t) );
   }
}

POST(sys_sigwaitinfo)
{
   if (ARG2 != 0) {
      POST_MEM_WRITE( ARG2, sizeof(vki_siginfo_t) );
   }
}

// SYS___acl_get_file   347
// int __acl_get_file(const char *path, acl_type_t type, struct acl *aclp);
PRE(sys___acl_get_file)
{
   PRINT("sys___acl_get_file ( %#" FMT_REGWORD "x(%s), %" FMT_REGWORD "u, %#" FMT_REGWORD "x )", ARG1,(char *)ARG1,ARG2,ARG3);
   PRE_REG_READ3(int, "acl_get_file",
                 const char *, path, int, type, struct vki_acl *, aclp);
   PRE_MEM_RASCIIZ("acl_get_file(path", ARG1);
   PRE_MEM_WRITE( "acl_get_file(aclp)", ARG3, sizeof(struct vki_acl) );
}

POST(sys___acl_get_file)
{
   vg_assert(SUCCESS);
   if (RES == 0) {
      POST_MEM_WRITE( ARG3, sizeof(struct vki_acl) );
   }
}

// SYS___acl_set_file   348
// int __acl_set_file(const char *path, acl_type_t type, struct acl *aclp);
PRE(sys___acl_set_file)
{
   PRINT("sys___acl_set_file ( %#" FMT_REGWORD "x(%s), %" FMT_REGWORD "u, %#" FMT_REGWORD "x )", ARG1,(char *)ARG1,ARG2,ARG3);
   PRE_REG_READ3(int, "acl_set_file",
                 const char *, path, int, type, struct vki_acl *, aclp);
   PRE_MEM_RASCIIZ("acl_set_file(path", ARG1);
   PRE_MEM_READ("acl_set_file(aclp)", ARG3, sizeof(struct vki_acl) );
}

// SYS___acl_get_fd  349
// int __acl_get_fd(int filedes, acl_type_t type, struct acl *aclp);
PRE(sys___acl_get_fd)
{
   PRINT("sys___acl_get_fd ( %" FMT_REGWORD "u, %" FMT_REGWORD "u, %#" FMT_REGWORD "x )", ARG1,ARG2,ARG3);
   PRE_REG_READ3(int, "acl_get_fd",
                 int, fd, int, type, struct vki_acl *, aclp);
   PRE_MEM_WRITE( "acl_get_file(aclp)", ARG3, sizeof(struct vki_acl) );
}

POST(sys___acl_get_fd)
{
   vg_assert(SUCCESS);
   if (RES == 0) {
      POST_MEM_WRITE( ARG3, sizeof(struct vki_acl) );
   }
}

// SYS___acl_set_fd  350
// int __acl_set_fd(int filedes, acl_type_t type, struct acl *aclp);
PRE(sys___acl_set_fd)
{
   PRINT("sys___acl_set_fd ( %" FMT_REGWORD "u, %" FMT_REGWORD "u, %#" FMT_REGWORD "x )", ARG1,ARG2,ARG3);
   PRE_REG_READ3(int, "acl_set_fd",
                 int, filedes, int, type, struct vki_acl *, aclp);
   PRE_MEM_READ( "acl_get_file(aclp)", ARG3, sizeof(struct vki_acl) );
}

// SYS___acl_delete_file   351
// int __acl_delete_file(const char *path, acl_type_t type);
PRE(sys___acl_delete_file)
{
   PRINT("sys___acl_delete_file ( %#" FMT_REGWORD "x(%s), %" FMT_REGWORD "u )", ARG1,(char *)ARG1,ARG2);
   PRE_MEM_RASCIIZ("acl_set_file(path", ARG1);
   PRE_REG_READ2(int, "acl_delete_file",
                 const char *, path, int, type);
}
// SYS___acl_delete_fd  352
// int __acl_delete_fd(int filedes, acl_type_t type);
PRE(sys___acl_delete_fd)
{
   PRINT("sys___acl_delete_fd ( %" FMT_REGWORD "u, %" FMT_REGWORD "u )", ARG1,ARG2);
   PRE_REG_READ2(int, "acl_delete_fd",
                 int, filedes, int, acltype);
}

// SYS___acl_aclcheck_file 353
// int __acl_aclcheck_file(const char *path, acl_type_t type, struct acl *aclp);
PRE(sys___acl_aclcheck_file)
{
   PRINT("sys___acl_aclcheck_file ( %#" FMT_REGWORD "x(%s), %" FMT_REGWORD "u, %#" FMT_REGWORD "x )", ARG1,(char *)ARG1,ARG2,ARG3);
   PRE_REG_READ3(int, "acl_aclcheck_file",
                 const char *, path, int, type, struct vki_acl *, aclp);
   PRE_MEM_RASCIIZ("acl_set_file(path", ARG1);
   PRE_MEM_READ( "acl_aclcheck_file(aclp)", ARG3, sizeof(struct vki_acl) );
}

// SYS___acl_aclcheck_fd   354
// int __acl_aclcheck_fd(int filedes, acl_type_t type, struct acl *aclp);
PRE(sys___acl_aclcheck_fd)
{
   PRINT("sys___acl_aclcheck_fd ( %" FMT_REGWORD "u, %" FMT_REGWORD "u, %#" FMT_REGWORD "x )", ARG1,ARG2,ARG3);
   PRE_REG_READ3(int, "acl_aclcheck_fd",
                 int, fd, int, type, struct vki_acl *, aclp);
   PRE_MEM_READ( "acl_aclcheck_fd(aclp)", ARG3, sizeof(struct vki_acl) );
}

// SYS_extattrctl 355
// no manpage?
// syscalls.master: int extattrctl(_In_z_ const char *path, int cmd, _In_z_opt_ const char *filename, int attrnamespace, _In_z_ const char *attrname);
PRE(sys_extattrctl)
{
   PRINT("sys_extattrctl ( %#" FMT_REGWORD "x, %" FMT_REGWORD "d, %#" FMT_REGWORD "x, %" FMT_REGWORD "d, %#" FMT_REGWORD "x )", ARG1,SARG2,ARG3,SARG4,ARG5);
   PRE_REG_READ5(ssize_t, "extattrctl",
                 const char *, path, int, cmd, const char *, filename, int, attrnamespace, const char *, attrname);
   PRE_MEM_RASCIIZ("extattrctl(path)", ARG1);
   PRE_MEM_RASCIIZ("extattrctl(filename)", ARG3);
   PRE_MEM_RASCIIZ("extattrctl(attrname)", ARG5);
}

// SYS_extattr_set_file 356
// ssize_t extattr_set_file(const char *path, int attrnamespace,
//                          const char *attrname, const void *data, size_t nbytes);
PRE(sys_extattr_set_file)
{
   PRINT("sys_extattr_set_file ( %#" FMT_REGWORD "x, %" FMT_REGWORD "d, %#" FMT_REGWORD "x, %#" FMT_REGWORD "x, %" FMT_REGWORD "u )", ARG1,SARG2,ARG3,ARG4,ARG5);
   PRE_REG_READ5(ssize_t, "extattr_set_file",
                 const char *, path, int, attrnamespace, const char *, attrname, const void *, data, size_t, nbytes);
   PRE_MEM_RASCIIZ("extattr_set_file(path)", ARG1);
   PRE_MEM_RASCIIZ("extattr_set_file(attrname)", ARG3);
   PRE_MEM_READ("extattr_set_file(data)", ARG4, ARG5);
}

// SYS_extattr_get_file 357
// ssize_t extattr_get_file(const char *path, int attrnamespace,
//                          const char *attrname, void *data, size_t nbytes);
PRE(sys_extattr_get_file)
{
   PRINT("sys_extattr_get_file ( %#" FMT_REGWORD "x, %" FMT_REGWORD "d, %#" FMT_REGWORD "x, %#" FMT_REGWORD "x, %" FMT_REGWORD "u )", ARG1,SARG2,ARG3,ARG4,ARG5);
   PRE_REG_READ5(ssize_t, "extattr_get_file",
                 const char *, path, int, attrnamespace, const char *, attrname, void *, data, size_t, nbytes);
   PRE_MEM_RASCIIZ("extattr_get_file(path)", ARG1);
   PRE_MEM_RASCIIZ("extattr_get_file(attrname)", ARG3);
   if (ARG4) {
      PRE_MEM_WRITE("extattr_get_file(data)", ARG4, ARG5);
   }
}

POST(sys_extattr_get_file)
{
   if (ARG4) {
      POST_MEM_WRITE(ARG4, ARG5);
   }
}

// SYS_extattr_delete_file 358
// int extattr_delete_file(const char *path, int attrnamespace,
//                         const char *attrname);
PRE(sys_extattr_delete_file)
{
   PRINT("sys_extattr_delete_file ( %#" FMT_REGWORD "x, %" FMT_REGWORD "d, %#" FMT_REGWORD "x )", ARG1,SARG2,ARG3);
   PRE_REG_READ3(ssize_t, "extattr_delete_file",
                 const char *, path, int, attrnamespace, const char *, attrname);
   PRE_MEM_RASCIIZ("extattr_delete_file(path)", ARG1);
   PRE_MEM_RASCIIZ("extattr_delete_file(attrname)", ARG3);
}

// SYS_aio_waitcomplete 359
// ssize_t aio_waitcomplete(struct aiocb **iocbp, struct timespec *timeout);
PRE(sys_aio_waitcomplete)
{
   *flags |= SfMayBlock;
   PRINT("sys_aio_waitcomplete ( %#" FMT_REGWORD "x, %#" FMT_REGWORD "x )", ARG1,ARG2);
   PRE_REG_READ2(ssize_t, "aio_waitcomplete", struct aiocb **, iocbp, struct timespec *, timeout);
   if (ARG2) {
      PRE_MEM_READ("aio_waitcomplete(timeout", ARG2, sizeof(struct vki_timespec));
   }
   PRE_MEM_WRITE( "aio_waitcomplete(iocbp)", ARG1, sizeof(struct aiocb *));
}

POST(sys_aio_waitcomplete)
{
   POST_MEM_WRITE(ARG1, sizeof(struct aiocb *));
}

// SYS_getresuid  360
// int getresuid(uid_t *ruid, uid_t *euid, uid_t *suid);
PRE(sys_getresuid)
{
   PRINT("sys_getresuid ( %#" FMT_REGWORD "x, %#" FMT_REGWORD "x, %#" FMT_REGWORD "x )", ARG1,ARG2,ARG3);
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

// SYS_getresgid  361
// int getresgid(gid_t *rgid, gid_t *egid, gid_t *sgid);
PRE(sys_getresgid)
{
   PRINT("sys_getresgid ( %#" FMT_REGWORD "x, %#" FMT_REGWORD "x, %#" FMT_REGWORD "x )", ARG1,ARG2,ARG3);
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

// SYS_kqueue  362
// int kqueue(void);
PRE(sys_kqueue)
{
   PRINT("%s", "sys_kqueue(void)");
   PRE_REG_READ0(int, "kqueue");
}

POST(sys_kqueue)
{
   if (!ML_(fd_allowed)(RES, "kqueue", tid, True)) {
      VG_(close)(RES);
      SET_STATUS_Failure( VKI_EMFILE );
   } else {
      if (VG_(clo_track_fds)) {
         ML_(record_fd_open_nameless)(tid, RES);
      }
   }
}

// SYS_freebsd11_kevent 363
// int kevent(int kq, const struct kevent *changelist, int nchanges,
//            struct kevent *eventlist, int nevents,
//            const struct timespec *timeout);
#if (FREEBSD_VERS >= FREEBSD_12)
PRE(sys_freebsd11_kevent)
{
   PRINT("sys_freebsd11_kevent ( %" FMT_REGWORD "u, %#" FMT_REGWORD "x, %" FMT_REGWORD "u, %#" FMT_REGWORD "x, %" FMT_REGWORD "u, %#" FMT_REGWORD "x )\n", ARG1,ARG2,ARG3,ARG4,ARG5,ARG6);
   PRE_REG_READ6(int, "kevent",
                 int, fd, const struct vki_kevent_freebsd11 *, changelist, int, nchanges,
                 struct vki_kevent_freebsd11 *, eventlist, int, nevents,
                 struct timespec *, timeout);
   if (ARG2 != 0 && ARG3 != 0) {
      PRE_MEM_READ( "kevent(changelist)", ARG2, sizeof(struct vki_kevent_freebsd11)*ARG3 );
   }
   if (ARG4 != 0 && ARG5 != 0) {
      PRE_MEM_WRITE( "kevent(eventlist)", ARG4, sizeof(struct vki_kevent_freebsd11)*ARG5);
   }
   if (ARG5 != 0) {
      *flags |= SfMayBlock;
   }
   if (ARG6 != 0) {
      PRE_MEM_READ( "kevent(timeout)",
                    ARG6, sizeof(struct vki_timespec));
   }
}

POST(sys_freebsd11_kevent)
{
   vg_assert(SUCCESS);
   if ((Word)RES != -1) {
      if (ARG4 != 0) {
         POST_MEM_WRITE( ARG4, sizeof(struct vki_kevent_freebsd11)*RES) ;
      }
   }
}
#else
PRE(sys_kevent)
{
   *flags |= SfMayBlock;
   PRINT("sys_kevent ( %" FMT_REGWORD "u, %#" FMT_REGWORD "x, %" FMT_REGWORD "u, %#" FMT_REGWORD "x, %" FMT_REGWORD "u, %#" FMT_REGWORD "x )\n", ARG1,ARG2,ARG3,ARG4,ARG5,ARG6);
   PRE_REG_READ6(int, "kevent",
                 int, fd, struct vki_kevent_freebsd11 *, changelist, int, nchanges,
                 struct vki_kevent_freebsd11 *, eventlist, int, nevents,
                 struct timespec *, timeout);
   if (ARG2 != 0 && ARG3 != 0)
      PRE_MEM_READ( "kevent(changelist)", ARG2, sizeof(struct vki_kevent_freebsd11)*ARG3 );
   if (ARG4 != 0 && ARG5 != 0)
      PRE_MEM_WRITE( "kevent(eventlist)", ARG4, sizeof(struct vki_kevent_freebsd11)*ARG5);
   if (ARG6 != 0)
      PRE_MEM_READ( "kevent(timeout)",
                    ARG6, sizeof(struct vki_timespec));
}

POST(sys_kevent)
{
   vg_assert(SUCCESS);
   if ((Word)RES != -1) {
      if (ARG4 != 0)
         POST_MEM_WRITE( ARG4, sizeof(struct vki_kevent_freebsd11)*RES) ;
   }
}
#endif

// SYS_extattr_set_fd   371
// ssize_t extattr_set_fd(int fd, int attrnamespace, const char *attrname,
//                        const void *data, size_t nbytes);
PRE(sys_extattr_set_fd)
{
   PRINT("sys_extattr_set_fd ( %" FMT_REGWORD "d, %" FMT_REGWORD "d, %#" FMT_REGWORD "x, %#" FMT_REGWORD "x, %" FMT_REGWORD "u )", SARG1,SARG2,ARG3,ARG4,ARG5);
   PRE_REG_READ5(int, "extattr_set_fd", int, fd, int, attrnamespace, const char *,attrname, const void *,data, size_t, nbytes);
   PRE_MEM_RASCIIZ( "extattr_set_fd(attrname)", ARG3 );
   PRE_MEM_READ("extattr_set_fd(data)", ARG4, ARG5);
}

// SYS_extattr_get_fd   372
// ssize_t extattr_get_fd(int fd, int attrnamespace, const char *attrname,
//                        void *data, size_t nbytes);
PRE(sys_extattr_get_fd)
{
   PRINT("sys_extattr_get_fd ( %" FMT_REGWORD "d, %" FMT_REGWORD "d, %#" FMT_REGWORD "x, %#" FMT_REGWORD "x, %" FMT_REGWORD "u )", SARG1,SARG2,ARG3,ARG4,ARG5);
   PRE_REG_READ5(int, "extattr_get_fd", int, fd, int, attrnamespace, const char *,attrname, const void *,data, size_t, nbytes);
   PRE_MEM_RASCIIZ( "extattr_get_fd(attrname)", ARG3 );
   PRE_MEM_WRITE("extattr_get_fd(data)", ARG4, ARG5);
}

POST(sys_extattr_get_fd)
{
   POST_MEM_WRITE(ARG4, ARG5);
}

// SYS_extattr_delete_fd   373
// int extattr_delete_fd(int fd, int attrnamespace, const char *attrname);
PRE(sys_extattr_delete_fd)
{
   PRINT("sys_extattr_delete_fd ( %" FMT_REGWORD "d, %" FMT_REGWORD "d, %#" FMT_REGWORD "x )", SARG1,SARG2,ARG3);
   PRE_REG_READ3(int, "extattr_delete_fd", int, fd, int, attrnamespace, const char *,attrname);
   PRE_MEM_RASCIIZ( "extattr_delete_fd(attrname)", ARG3 );
}

// SYS___setugid  374
// no manpage?
// syscalls.master: int __setugid(int flag);
PRE(sys___setugid)
{
   PRINT("sys___setugid ( %" FMT_REGWORD "d )", SARG1);
   PRE_REG_READ1(int, "__setugid", int, flag);
}

// SYS_eaccess 376
// int eaccess(const char *path, int mode);
PRE(sys_eaccess)
{
   PRINT("sys_eaccess ( %#" FMT_REGWORD "x(%s), %" FMT_REGWORD "u )", ARG1,(char*)ARG1,ARG2);
   PRE_REG_READ2(int, "eaccess", const char *, path, int, mode);
   PRE_MEM_RASCIIZ( "eaccess(path)", ARG1 );
}

// SYS_afs3_syscall  377
// @todo

// SYS_nmount  378
// int nmount(struct iovec *iov, u_int niov, int flags);
PRE(sys_nmount)
{
   PRINT("sys_nmount ( %#" FMT_REGWORD "x, %" FMT_REGWORD "u, %" FMT_REGWORD "d )", ARG1, ARG2, SARG3);
   PRE_REG_READ3(int, "nmount", struct iovec *, iov, u_int, niov, int, flags);
   PRE_MEM_READ( "nmount(pathname)", ARG1, ARG2*sizeof(struct vki_iovec) );
}

// SYS___mac_get_proc   384
// @todo

// SYS___mac_set_proc   385
// @todo

// SYS___mac_get_fd  386
// @todo

// SYS___mac_get_file   387
// @todo

// SYS___mac_set_fd  388
// @todo

// SYS___mac_set_file   389
// @todo

// SYS_kenv 390
// int kenv(int action, const char *name, char *value, int len);
PRE(sys_kenv)
{
   PRINT("sys_kenv ( %" FMT_REGWORD "u, %#" FMT_REGWORD "x, %#" FMT_REGWORD "x, %" FMT_REGWORD "u )", ARG1,ARG2,ARG3,ARG4);
   PRE_REG_READ4(int, "kenv",
                 int, action, const char *, name, char *, value, int, len);
   switch (ARG1) {
   case VKI_KENV_GET:
   case VKI_KENV_SET:
   case VKI_KENV_UNSET:
      PRE_MEM_RASCIIZ("kenv(name)", ARG2);
   /* FALLTHROUGH */
   case VKI_KENV_DUMP:
      break;
   default:
      VG_(dmsg)("Warning: Bad action %" FMT_REGWORD "u in kenv\n", ARG1);
   }
}

POST(sys_kenv)
{
   if (SUCCESS) {
      switch (ARG1) {
      case VKI_KENV_GET:
         POST_MEM_WRITE(ARG3, ARG4);
         break;
      case VKI_KENV_DUMP:
         if (ARG3 != (Addr)NULL) {
            POST_MEM_WRITE(ARG3, ARG4);
         }
         break;
      }
   }
}

// SYS_lchflags   391
// int lchflags(const char *path, unsigned long flags);
PRE(sys_lchflags)
{
   PRINT("sys_lchflags ( %#" FMT_REGWORD "x(%s), 0x%" FMT_REGWORD "x )", ARG1,(char *)ARG1,ARG2);
   PRE_REG_READ2(int, "lchflags",
                 const char *, path, unsigned long, flags);
   PRE_MEM_RASCIIZ( "lchflags(path)", ARG1 );
}

// SYS_uuidgen 392
// int uuidgen(struct uuid *store, int count);
PRE(sys_uuidgen)
{
   PRINT("sys_uuidgen ( %#" FMT_REGWORD "x, %" FMT_REGWORD "u )", ARG1,ARG2);
   PRE_REG_READ2(int, "uuidgen",
                 struct vki_uuid *, store, int, count);
   PRE_MEM_WRITE( "uuidgen(store)", ARG1, ARG2 * sizeof(struct vki_uuid));
}

POST(sys_uuidgen)
{
   if (SUCCESS) {
      POST_MEM_WRITE( ARG1, ARG2 * sizeof(struct vki_uuid) );
   }
}

// SYS_sendfile   393
// x86/amd64

// SYS_mac_syscall   394
// @todo

#if (FREEBSD_VERS >= FREEBSD_12)

// SYS_freebsd11_getfsstat 395
// int getfsstat(struct freebsd11_statfs *buf, long bufsize, int mode);

PRE(sys_freebsd11_getfsstat)
{
   PRINT("sys_freebsd11_getfsstat ( %#" FMT_REGWORD "x, %" FMT_REGWORD "u, %" FMT_REGWORD "u )",ARG1,ARG2,ARG3);
   PRE_REG_READ3(int, "getfsstat", struct vki_freebsd11_statfs *, buf, long, bufsize, int, mode);
   PRE_MEM_WRITE( "getfsstat(buf)", ARG1, ARG2 );
}

POST(sys_freebsd11_getfsstat)
{
   vg_assert(SUCCESS);
   if ((Word)RES != -1) {
      POST_MEM_WRITE( ARG1, RES * sizeof(struct vki_freebsd11_statfs) );
   }
}

// SYS_freebsd11_statfs 396
// int statfs(const char *path, struct statfs *buf);
PRE(sys_freebsd11_statfs)
{
   PRINT("sys_statfs ( %#" FMT_REGWORD "x(%s), %#" FMT_REGWORD "x )",ARG1,(char *)ARG1,ARG2);
   PRE_REG_READ2(int, "statfs", const char *, path, struct statfs *, buf);
   PRE_MEM_RASCIIZ( "statfs(path)", ARG1 );
   PRE_MEM_WRITE( "statfs(buf)", ARG2, sizeof(struct vki_freebsd11_statfs) );
}

POST(sys_freebsd11_statfs)
{
   POST_MEM_WRITE( ARG2, sizeof(struct vki_freebsd11_statfs) );
}

// SYS_freebsd11_fstatfs   397
// int fstatfs(int fd, struct statfs *buf);
PRE(sys_freebsd11_fstatfs)
{
   PRINT("sys_fstatfs ( %" FMT_REGWORD "u, %#" FMT_REGWORD "x )",ARG1,ARG2);
   PRE_REG_READ2(int, "fstatfs",
                 unsigned int, fd, struct statfs *, buf);
   PRE_MEM_WRITE( "fstatfs(buf)", ARG2, sizeof(struct vki_freebsd11_statfs) );
}

POST(sys_freebsd11_fstatfs)
{
   POST_MEM_WRITE( ARG2, sizeof(struct vki_freebsd11_statfs) );
}

// SYS_freebsd11_fhstatfs  398
// int fhstatfs(const fhandle_t *fhp, struct statfs *buf);
PRE(sys_freebsd11_fhstatfs)
{
   PRINT("sys_fhstatfs ( %#" FMT_REGWORD "x, %#" FMT_REGWORD "x )",ARG1,ARG2);
   PRE_REG_READ2(int, "fhstatfs",
                 struct fhandle *, fhp, struct statfs *, buf);
   PRE_MEM_READ( "fhstatfs(fhp)", ARG1, sizeof(struct vki_fhandle) );
   PRE_MEM_WRITE( "fhstatfs(buf)", ARG2, sizeof(struct vki_freebsd11_statfs) );
}

POST(sys_freebsd11_fhstatfs)
{
   POST_MEM_WRITE( ARG2, sizeof(struct vki_freebsd11_statfs) );
}


#else

PRE(sys_getfsstat)
{
   PRINT("sys_getfsstat ( %#" FMT_REGWORD "x, %" FMT_REGWORD "u, %" FMT_REGWORD "u )",ARG1,ARG2,ARG3);
   PRE_REG_READ3(int, "getfsstat", struct vki_freebsd11_statfs *, buf, long, bufsize, int, mode);
   PRE_MEM_WRITE( "getfsstat(buf)", ARG1, ARG2 );
}

POST(sys_getfsstat)
{
   vg_assert(SUCCESS);
   if ((Word)RES != -1) {
      POST_MEM_WRITE( ARG1, RES * sizeof(struct vki_freebsd11_statfs) );
   }
}

PRE(sys_statfs)
{
   PRINT("sys_statfs ( %#" FMT_REGWORD "x(%s), %#" FMT_REGWORD "x )",ARG1,(char *)ARG1,ARG2);
   PRE_REG_READ2(int, "statfs", const char *, path, struct statfs *, buf);
   PRE_MEM_RASCIIZ( "statfs(path)", ARG1 );
   PRE_MEM_WRITE( "statfs(buf)", ARG2, sizeof(struct vki_freebsd11_statfs) );
}

POST(sys_statfs)
{
   POST_MEM_WRITE( ARG2, sizeof(struct vki_freebsd11_statfs) );
}

PRE(sys_fstatfs)
{
   PRINT("sys_fstatfs ( %" FMT_REGWORD "u, %#" FMT_REGWORD "x )",ARG1,ARG2);
   PRE_REG_READ2(int, "fstatfs",
                 unsigned int, fd, struct statfs *, buf);
   PRE_MEM_WRITE( "fstatfs(buf)", ARG2, sizeof(struct vki_freebsd11_statfs) );
}

POST(sys_fstatfs)
{
   POST_MEM_WRITE( ARG2, sizeof(struct vki_freebsd11_statfs) );
}

PRE(sys_fhstatfs)
{
   PRINT("sys_fhstatfs ( %#" FMT_REGWORD "x, %#" FMT_REGWORD "x )",ARG1,ARG2);
   PRE_REG_READ2(int, "fhstatfs",
                 struct fhandle *, fhp, struct statfs *, buf);
   PRE_MEM_READ( "fhstatfs(fhp)", ARG1, sizeof(struct vki_fhandle) );
   PRE_MEM_WRITE( "fhstatfs(buf)", ARG2, sizeof(struct vki_freebsd11_statfs) );
}

POST(sys_fhstatfs)
{
   POST_MEM_WRITE( ARG2, sizeof(struct vki_freebsd11_statfs) );
}


#endif

// SYS_ksem_close 400
// @todo

// SYS_ksem_post  401
// @todo

// SYS_ksem_wait  402
// @todo

// SYS_ksem_trywait  403
// @todo

// SYS_ksem_init  404
// @todo

// SYS_ksem_open  405
// @todo

// SYS_ksem_unlink   406
// @todo

// SYS_ksem_getvalue 407
// @todo

// SYS_ksem_destroy  408
// @todo

// SYS___mac_get_pid 409
// @todo

// SYS___mac_get_link   410
// @todo

// SYS___mac_set_link   411
// @todo

// SYS_extattr_set_link 412
// ssize_t extattr_set_link(const char *path, int attrnamespace,
//                          const char *attrname, const void *data, size_t nbytes);
PRE(sys_extattr_set_link)
{
   PRINT("sys_extattr_set_link ( %#" FMT_REGWORD "x, %" FMT_REGWORD "d, %#" FMT_REGWORD "x, %#" FMT_REGWORD "x, %" FMT_REGWORD "u )", ARG1,SARG2,ARG3,ARG4,ARG5);
   PRE_REG_READ5(ssize_t, "extattr_set_link",
                 const char *, path, int, attrnamespace, const char *, attrname, const void *, data, size_t, nbytes);
   PRE_MEM_RASCIIZ("extattr_set_link(path)", ARG1);
   PRE_MEM_RASCIIZ("extattr_set_link(attrname)", ARG3);
   PRE_MEM_READ("extattr_set_link(data)", ARG4, ARG5);
}

// SYS_extattr_get_link 413
// ssize_t extattr_get_link(const char *path, int attrnamespace,
//                          const char *attrname, void *data, size_t nbytes);
PRE(sys_extattr_get_link)
{
   PRINT("sys_extattr_get_link ( %#" FMT_REGWORD "x, %" FMT_REGWORD "d, %#" FMT_REGWORD "x, %#" FMT_REGWORD "x, %" FMT_REGWORD "u )", ARG1,SARG2,ARG3,ARG4,ARG5);
   PRE_REG_READ5(ssize_t, "extattr_get_link",
                 const char *, path, int, attrnamespace, const char *, attrname, void *, data, size_t, nbytes);
   PRE_MEM_RASCIIZ("extattr_get_link(path)", ARG1);
   PRE_MEM_RASCIIZ("extattr_get_link(attrname)", ARG3);
   if (ARG4) {
      PRE_MEM_WRITE("extattr_get_link(data)", ARG4, ARG5);
   }
}

POST(sys_extattr_get_link)
{
   if (ARG4) {
      POST_MEM_WRITE(ARG4, ARG5);
   }
}

// SYS_extattr_delete_link 414
// int extattr_delete_link(const char *path, int attrnamespace,
//                         const char *attrname);
PRE(sys_extattr_delete_link)
{
   PRINT("sys_extattr_delete_link ( %#" FMT_REGWORD "x, %" FMT_REGWORD "d, %#" FMT_REGWORD "x )", ARG1,SARG2,ARG3);
   PRE_REG_READ3(ssize_t, "extattr_delete_link",
                 const char *, path, int, attrnamespace, const char *, attrname);
   PRE_MEM_RASCIIZ("extattr_delete_link(path)", ARG1);
   PRE_MEM_RASCIIZ("extattr_delete_link(attrname)", ARG3);
}

// SYS___mac_execve  415
// @todo

// SYS_sigaction  416
//int sigaction(int sig, const struct sigaction * restrict act,
//              struct sigaction * restrict oact);
PRE(sys_sigaction)
{
   vki_sigaction_toK_t   new;
   vki_sigaction_toK_t   *newp;
   vki_sigaction_fromK_t old;
   vki_sigaction_fromK_t *oldp;

   PRINT("sys_sigaction ( %" FMT_REGWORD "d, %#" FMT_REGWORD "x, %#" FMT_REGWORD "x )",
         SARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "sigaction",
                 int, sign, const struct sigaction *, act,
                 struct sigaction *, oact);

   newp = oldp = NULL;

   if (ARG2 != 0) {
      struct vki_sigaction *sa = (struct vki_sigaction *)ARG2;
      PRE_MEM_READ( "sigaction(act->sa_handler)", (Addr)&sa->ksa_handler, sizeof(sa->ksa_handler));
      PRE_MEM_READ( "sigaction(act->sa_mask)", (Addr)&sa->sa_mask, sizeof(sa->sa_mask));
      PRE_MEM_READ( "sigaction(act->sa_flags)", (Addr)&sa->sa_flags, sizeof(sa->sa_flags));
   }

   if (ARG3 != 0) {
      PRE_MEM_WRITE( "sigaction(oact)", ARG3, sizeof(struct vki_sigaction));
      oldp = &old;
   }

   if (ARG2 != 0
         && ! ML_(safe_to_deref)((void *)(Addr)ARG2,
                                 sizeof(struct vki_sigaction))) {
      VG_(umsg)("Warning: bad act handler address %p in sigaction()\n",
                (void *)(Addr)ARG2);
      SET_STATUS_Failure ( VKI_EFAULT );
   } else if ((ARG3 != 0
               && ! ML_(safe_to_deref)((void *)(Addr)ARG3,
                                       sizeof(struct vki_sigaction)))) {
      VG_(umsg)("Warning: bad oact handler address %p in sigaction()\n",
                (void *)(Addr)ARG3);
      SET_STATUS_Failure ( VKI_EFAULT );
   } else {
      if (ARG2 != 0) {
         struct vki_sigaction *oldnew =
            (struct vki_sigaction *)(Addr)ARG2;

         new.ksa_handler = oldnew->ksa_handler;
         new.sa_flags = oldnew->sa_flags;
         new.sa_mask = oldnew->sa_mask;
         newp = &new;
      }

      SET_STATUS_from_SysRes( VG_(do_sys_sigaction)(ARG1, newp, oldp) );

      if (ARG3 != 0 && SUCCESS && RES == 0) {
         struct vki_sigaction *oldold =
            (struct vki_sigaction *)(Addr)ARG3;

         oldold->ksa_handler = oldp->ksa_handler;
         oldold->sa_flags = oldp->sa_flags;
         oldold->sa_mask = oldp->sa_mask;
      }
   }
}

POST(sys_sigaction)
{
   vg_assert(SUCCESS);
   if (RES == 0 && ARG3 != 0) {
      POST_MEM_WRITE( ARG3, sizeof(struct vki_sigaction));
   }
}

// SYS_sigreturn  417
// x86/amd64

// SYS_getcontext 421
// SYS_setcontext 422
// SYS_swapcontext   423
// PRE in x86/amd64

POST(sys_getcontext)
{
   POST_MEM_WRITE( ARG1, sizeof(struct vki_ucontext) );
}

POST(sys_swapcontext)
{
   if (SUCCESS) {
      POST_MEM_WRITE( ARG1, sizeof(struct vki_ucontext) );
   }
}

#if (FREEBSD_VERS >= FREEBSD_13_1)
// SYS_freebsd13_swapoff 424
// int swapoff(const char *special);
PRE(sys_freebsd13_swapoff)
{
   PRINT("sys_freebsd13_swapoff ( %#" FMT_REGWORD "x(%s) )", ARG1,(char *)ARG1);
   PRE_REG_READ1(int, "swapoff", const char *, special);
   PRE_MEM_RASCIIZ( "swapoff(special)", ARG1 );
}
#else
// SYS_swapoff 424
// int swapoff(const char *special);
PRE(sys_swapoff)
{
   PRINT("sys_swapoff ( %#" FMT_REGWORD "x(%s) )", ARG1,(char *)ARG1);
   PRE_REG_READ1(int, "swapoff", const char *, special);
   PRE_MEM_RASCIIZ( "swapoff(special)", ARG1 );
}
#endif

// SYS___acl_get_link   425
// int __acl_get_link(const char *path, acl_type_t type, struct acl *aclp);
PRE(sys___acl_get_link)
{
   PRINT("sys___acl_get_link ( %#" FMT_REGWORD "x(%s), %" FMT_REGWORD "u, %#" FMT_REGWORD "x )", ARG1,(char *)ARG1,ARG2,ARG3);
   PRE_REG_READ3(int, "__acl_get_link",
                 const char *, path, int, acltype, struct vki_acl *, aclp);
   PRE_MEM_RASCIIZ( "__acl_get_link(path)", ARG1 );
   PRE_MEM_WRITE( "__acl_get_link(aclp)", ARG3, sizeof(struct vki_acl) );
}

POST(sys___acl_get_link)
{
   vg_assert(SUCCESS);
   if (RES == 0) {
      POST_MEM_WRITE( ARG3, sizeof(struct vki_acl) );
   }
}

// SYS___acl_set_link   426
// int __acl_set_link(const char *path, acl_type_t type, struct acl *aclp);
PRE(sys___acl_set_link)
{
   PRINT("sys___acl_set_link ( %#" FMT_REGWORD "x(%s), %" FMT_REGWORD "u, %#" FMT_REGWORD "x )", ARG1,(char *)ARG1,ARG2,ARG3);
   PRE_REG_READ3(int, "__acl_set_link",
                 const char *, path, int, acltype, struct vki_acl *, aclp);
   PRE_MEM_RASCIIZ( "__acl_set_link(path)", ARG1 );
   PRE_MEM_READ( "__acl_set_link(aclp)", ARG3, sizeof(struct vki_acl) );
}
// SYS___acl_delete_link   427
// int __acl_delete_link(const char *path, acl_type_t type);
PRE(sys___acl_delete_link)
{
   PRINT("sys___acl_delete_link ( %#" FMT_REGWORD "x(%s), %" FMT_REGWORD "u )", ARG1,(char *)ARG1,ARG2);
   PRE_MEM_RASCIIZ( "__acl_delete_link(path)", ARG1 );
   PRE_REG_READ2(int, "__acl_delete_link",
                 const char *, path, int, acltype);
}

// SYS___acl_aclcheck_link 428
// int __acl_aclcheck_link(const char *path, acl_type_t type, struct acl *aclp);
PRE(sys___acl_aclcheck_link)
{
   PRINT("sys___acl_aclcheck_link ( %#" FMT_REGWORD "x(%s), %" FMT_REGWORD "u, %#" FMT_REGWORD "x )", ARG1,(char *)ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "__acl_aclcheck_link",
                 const char *, path, int, acltype, struct vki_acl *, aclp);
   PRE_MEM_RASCIIZ( "__acl_check_link(path)", ARG1 );
   PRE_MEM_READ( "__acl_aclcheck_link(aclp)", ARG3, sizeof(struct vki_acl) );
}

// SYS_sigwait 429
// int sigwait(const sigset_t * restrict set, int * restrict sig);
PRE(sys_sigwait)
{
   *flags |= SfMayBlock;
   PRINT("sys_sigwait ( %#" FMT_REGWORD "x, %#" FMT_REGWORD "x )",
         ARG1,ARG2);
   PRE_REG_READ2(int, "sigwait",
                 const vki_sigset_t *, set, int *, sig);
   if (ARG1 != 0) {
      PRE_MEM_READ(  "sigwait(set)",  ARG1, sizeof(vki_sigset_t));
   }
   if (ARG2 != 0) {
      PRE_MEM_WRITE( "sigwait(sig)", ARG2, sizeof(int));
   }
}

POST(sys_sigwait)
{
   if (ARG2 != 0) {
      POST_MEM_WRITE( ARG2, sizeof(int));
   }
}

// SYS_thr_create 430
// no manpage?
// syscalls.master: int thr_create(_In_ ucontext_t *ctx, _Out_ long *id, int flags );
PRE(sys_thr_create)
{
   PRINT( "sys_thr_create ( %#" FMT_REGWORD "x, %#" FMT_REGWORD "x, %" FMT_REGWORD "d )", ARG1, ARG2, SARG3 );
   PRE_REG_READ3(int, "thr_create", /*ucontext_t*/void *, ctx, long *, id, int, flags );

   VG_(message)(Vg_UserMsg, "thr_create() not implemented");
   VG_(unimplemented)("Valgrind does not support thr_create().");

   SET_STATUS_Failure(VKI_ENOSYS);
}

// SYS_thr_exit   431
// void thr_exit(long *state);
PRE(sys_thr_exit)
{
   ThreadState *tst;

   PRINT( "sys_thr_exit ( %#" FMT_REGWORD "x )", ARG1 );
   PRE_REG_READ1(void, "thr_exit", long *, state);

   if (ARG1) {
      PRE_MEM_WRITE( "thr_exit(state)", ARG1, sizeof(long) );
   }

   tst = VG_(get_ThreadState)(tid);
   tst->exitreason = VgSrc_ExitThread;
   tst->os_state.exitcode = ARG1;
   SET_STATUS_Success(0);
}

// SYS_thr_self   432
// int thr_self(long *id);
PRE(sys_thr_self)
{
   PRINT( "sys_thr_self ( %#" FMT_REGWORD "x )", ARG1 );
   PRE_REG_READ1(int, "thr_self", long *, id);
   PRE_MEM_WRITE( "thr_self()", ARG1, sizeof(long));
}

POST(sys_thr_self)
{
   POST_MEM_WRITE( ARG1, sizeof(long));
}

// SYS_thr_kill   433
// int thr_kill(long id, int sig);
PRE(sys_thr_kill)
{
   PRINT("sys_thr_kill ( %" FMT_REGWORD "u, %" FMT_REGWORD "u )", ARG1,ARG2);
   PRE_REG_READ2(long, "thr_kill", long, id, int, sig);
   if (!ML_(client_signal_OK)(ARG2)) {
      SET_STATUS_Failure( VKI_EINVAL );
      return;
   }

   /* Check to see if this kill gave us a pending signal */
   *flags |= SfPollAfter;

   if (VG_(clo_trace_signals))  {
      VG_(message)(Vg_DebugMsg, "thr_kill: sending signal %lu to tid %lu\n",
                   ARG2, ARG1);
   }

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

POST(sys_thr_kill)
{
   if (VG_(clo_trace_signals)) {
      VG_(message)(Vg_DebugMsg, "thr_kill: sent signal %lu to tid %lu\n",
                   ARG2, ARG1);
   }
}

#if (FREEBSD_VERS <= FREEBSD_10)
// SYS__umtx_lock 434
PRE(sys__umtx_lock)
{
   PRINT( "sys__umtx_lock ( %#" FMT_REGWORD "x )", ARG1);
   PRE_REG_READ1(long, "_umtx_lock", struct vki_umtx *, umtx);
   PRE_MEM_READ( "_umtx_lock(mtx)", ARG1, sizeof(struct vki_umtx) );
   PRE_MEM_WRITE( "_umtx_lock(mtx)", ARG1, sizeof(struct vki_umtx) );
}

POST(sys__umtx_lock)
{
   if (SUCCESS) {
      POST_MEM_WRITE(ARG1, sizeof(struct vki_umtx));
   }
}

// SYS__umtx_unlock 434
PRE(sys__umtx_unlock)
{
   PRINT( "sys__umtx_unlock ( %#" FMT_REGWORD "x )", ARG1);
   PRE_REG_READ1(long, "_umtx_unlock", struct vki_umtx *, umtx);
   PRE_MEM_READ( "_umtx_unlock(mtx)", ARG1, sizeof(struct vki_umtx) );
   PRE_MEM_WRITE( "_umtx_unlock(mtx)", ARG1, sizeof(struct vki_umtx) );
}

POST(sys__umtx_unlock)
{
   if (SUCCESS) {
      POST_MEM_WRITE(ARG1, sizeof(struct vki_umtx));
   }
}
#endif

// SYS_jail_attach   436
// int jail_attach(int jid);
PRE(sys_jail_attach)
{
   PRINT("sys_jail_attach ( %" FMT_REGWORD "d )", SARG1);
   PRE_REG_READ1(int, "jail_attach", int, jid);
}

// SYS_extattr_list_fd  437
// ssize_t extattr_list_fd(int fd, int attrnamespace, void *data, size_t nbytes);
PRE(sys_extattr_list_fd)
{
   PRINT("extattr_list_fd ( %" FMT_REGWORD "d, %" FMT_REGWORD "d, %#" FMT_REGWORD "x, %" FMT_REGWORD "u )", SARG1, SARG2, ARG3, ARG4);
   PRE_REG_READ4(ssize_t, "extattr_list_fd", int, id, int, attrnamespace, void *,data, size_t, nbytes);
   PRE_MEM_WRITE("extattr_list_fd(data)", ARG3, ARG4);
}

POST(sys_extattr_list_fd)
{
   POST_MEM_WRITE(ARG3, ARG4);
}

// SYS_extattr_list_file   438
// ssize_t extattr_list_file(const char *path, int attrnamespace, void *data,
//                           size_t nbytes);
PRE(sys_extattr_list_file)
{
   PRINT("extattr_list_file ( %#" FMT_REGWORD "x, %" FMT_REGWORD "d, %#" FMT_REGWORD "x, %" FMT_REGWORD "u )", ARG1, SARG2, ARG3, ARG4);
   PRE_REG_READ4(ssize_t, "extattr_list_file", const char *, path, int, attrnamespace, void *,data, size_t, nbytes);
   PRE_MEM_RASCIIZ("extattr_list_file(path)", ARG1);
   PRE_MEM_WRITE("extattr_list_file(data)", ARG3, ARG4);
}

POST(sys_extattr_list_file)
{
   POST_MEM_WRITE(ARG3, ARG4);
}

// SYS_extattr_list_link   439
// ssize_t extattr_get_link(const char *path, int attrnamespace,
//                          const char *attrname, void *data, size_t nbytes);
PRE(sys_extattr_list_link)
{
   PRINT("extattr_list_link ( %#" FMT_REGWORD "x, %" FMT_REGWORD "d, %#" FMT_REGWORD "x, %" FMT_REGWORD "u )", ARG1, SARG2, ARG3, ARG4);
   PRE_REG_READ4(ssize_t, "extattr_list_link", const char *, path, int, attrnamespace, void *,data, size_t, nbytes);
   PRE_MEM_RASCIIZ("extattr_list_link(path)", ARG1);
   PRE_MEM_WRITE("extattr_list_link(data)", ARG3, ARG4);
}

POST(sys_extattr_list_link)
{
   POST_MEM_WRITE(ARG3, ARG4);
}

// SYS_ksem_timedwait   441
// @todo

// SYS_thr_suspend   442
// int thr_suspend(struct timespec *timeout);
PRE(sys_thr_suspend)
{
   PRINT("sys_thr_suspend ( %#" FMT_REGWORD "x )", ARG1);
   PRE_REG_READ1(int, "thr_suspend", struct timespec *, timeout);
   PRE_MEM_READ("thr_suspend(timeout)", ARG1, sizeof(struct vki_timespec));

   VG_(message)(Vg_UserMsg, "thr_supend() not implemented");
   VG_(unimplemented)("Valgrind does not support thr_suspend().");

   SET_STATUS_Failure(VKI_ENOSYS);
}

// SYS_thr_wake   443
// int thr_wake(long id);
PRE(sys_thr_wake)
{
   PRINT("sys_thr_wake ( %" FMT_REGWORD "d )", SARG1);
   PRE_REG_READ1(long, "thr_wake", long, id);
   /*
      if (VG_(is_valid_tid)(ARG1)) {
         VG_(threads)[ARG1].status = VgTs_Runnable;
      } else {
         SET_STATUS_Failure( VKI_ESRCH );
      }
   */
}

// SYS_kldunloadf 444
// int kldunloadf(int fileid, int flags);
PRE(sys_kldunloadf)
{
   PRINT("sys_kldunloadf ( %" FMT_REGWORD "d, %" FMT_REGWORD "d )", SARG1, SARG2);
   PRE_REG_READ2(int, "kldunloadf", int, fileid, int, flags);
}

// SYS_audit   445
// int audit(const char *record, u_int length);
// @todo

// SYS_auditon 446
// int auditon(int cmd, void *data, u_int length);
// @todo

// SYS_getauid 447
// int getauid(au_id_t *auid);
// @todo

// SYS_setauid 448
// int setauid(au_id_t *auid);
// @todo

// SYS_getaudit   449
//  int getaudit(auditinfo_t *auditinfo);
// @todo

// SYS_setaudit   450
// int setaudit(auditinfo_t *auditinfo);
// @todo

// SYS_getaudit_addr 451
// int getaudit_addr(auditinfo_addr_t *auditinfo_addr, u_int length);
// @todo

// SYS_setaudit_addr 452
// int setaudit_addr(auditinfo_addr_t *auditinfo_addr, u_int length);
// @todo

// SYS_auditctl   453
// @todo

// SYS__umtx_op   454
// int _umtx_op(void *obj, int op, u_long val, void *uaddr, void *uaddr2);
PRE(sys__umtx_op)
{
   /* 5 args are always passed through.  The last two can vary, but
      they're always pointers.  They may not be used though. */
   switch(ARG2) {
   case VKI_UMTX_OP_LOCK:
      // marked as COMPAT10
      PRINT( "sys__umtx_op ( %#" FMT_REGWORD "x, LOCK, %" FMT_REGWORD "u, %#" FMT_REGWORD "x, %#" FMT_REGWORD "x)", ARG1, ARG3, ARG4, ARG5);
      PRE_REG_READ5(long, "_umtx_op_lock",
                    struct umtx *, obj, int, op, unsigned long, id,
                    size_t, timeout_size, struct vki_timespec *, timeout);
      PRE_MEM_READ( "_umtx_op_lock(mtx)", ARG1, sizeof(struct vki_umtx) );
      if (ARG5) {
         PRE_MEM_READ( "_umtx_op_lock(timespec)", ARG5, ARG4 );
      }
      PRE_MEM_WRITE( "_umtx_op_lock(mtx)", ARG1, sizeof(struct vki_umtx) );
      *flags |= SfMayBlock;
      break;
   case VKI_UMTX_OP_UNLOCK:
      // marked as COMPAT10
      PRINT( "sys__umtx_op ( %#" FMT_REGWORD "x, UNLOCK, %" FMT_REGWORD "u)", ARG1, ARG3);
      PRE_REG_READ3(long, "_umtx_op_unlock",
                    struct umtx *, obj, int, op, unsigned long, id);
      PRE_MEM_READ( "_umtx_op_unlock(mtx)", ARG1, sizeof(struct vki_umtx) );
      PRE_MEM_WRITE( "_umtx_op_unlock(mtx)", ARG1, sizeof(struct vki_umtx) );
      break;
   case VKI_UMTX_OP_WAIT:
      PRINT( "sys__umtx_op ( %#" FMT_REGWORD "x, WAIT, %" FMT_REGWORD "u, %#" FMT_REGWORD "x, %#" FMT_REGWORD "x)", ARG1, ARG3, ARG4, ARG5);
      PRE_REG_READ5(long, "_umtx_op_wait",
                    long *, obj, int, op, unsigned long, val,
                    size_t, timeout_size, struct vki_timespec *, timeout);
      if (ARG1) {
         PRE_MEM_READ( "_umtx_op_wait(val)", ARG1, sizeof(long) );
         if (*(long*)ARG1 == (long)ARG3) {
            *flags |= SfMayBlock;
         }
      }

      if (ARG5) {
         PRE_MEM_READ( "_umtx_op_wait(timeout)", ARG5, ARG4 );
      }

      break;
   case VKI_UMTX_OP_WAKE:
      PRINT( "sys__umtx_op ( %#" FMT_REGWORD "x, WAKE, %" FMT_REGWORD "u)", ARG1, ARG3);
      PRE_REG_READ3(long, "_umtx_op_wake",
                    vki_uintptr_t *, obj, int, op, int, val);
      // PJF I don't think that the value of obj gets read, the address is being used as a key
      //PRE_MEM_READ("_umtx_op_wake(obj)", ARG1, sizeof(vki_uintptr_t));
      break;
   case VKI_UMTX_OP_MUTEX_TRYLOCK:
      PRINT( "sys__umtx_op ( %#" FMT_REGWORD "x, MUTEX_TRYLOCK, %" FMT_REGWORD "u, %#" FMT_REGWORD "x, %#" FMT_REGWORD "x)", ARG1, ARG3, ARG4, ARG5);
      PRE_REG_READ2(long, "_umtx_op_mutex_trylock", struct umutex *, obj, int, op);
      PRE_MEM_READ( "_umtx_op_mutex_trylock(mutex)", ARG1, sizeof(struct vki_umutex) );
      PRE_MEM_WRITE( "_umtx_op_mutex_trylock(mutex)", ARG1, sizeof(struct vki_umutex) );
      /* not too sure about the restart here
       * it's hard to test as if the mutex is locked this returns EBUSY
       * so there is only a small window where the syscall could be interrupted */
      *flags |= SfMayBlock | SfKernelRestart;
      break;
   case VKI_UMTX_OP_MUTEX_LOCK:
      // called by pthread_mutex_lock
      // when the atribute UMUTEX_PRIO_PROTECT or UMUTEX_PRIO_INHERIT is set
      PRINT( "sys__umtx_op ( %#" FMT_REGWORD "x, MUTEX_LOCK, %" FMT_REGWORD "u, %#" FMT_REGWORD "x, %#" FMT_REGWORD "x)", ARG1, ARG3, ARG4, ARG5);
      PRE_REG_READ5(long, "_umtx_op_mutex_lock",
                    struct umutex *, obj, int, op, unsigned long, noid,
                    size_t, timeout_size, struct vki_timespec *, timeout);
      PRE_MEM_READ( "_umtx_op_mutex_lock(mutex)", ARG1, sizeof(struct vki_umutex) );
      if (ARG5) {
         PRE_MEM_READ( "_umtx_op_mutex_lock(timespec)", ARG5, ARG4 );
      } else {
         *flags |= SfKernelRestart;
      }
      PRE_MEM_WRITE( "_umtx_op_mutex_lock(mutex)", ARG1, sizeof(struct vki_umutex) );
      *flags |= SfMayBlock;
      break;
   case VKI_UMTX_OP_MUTEX_UNLOCK:
      PRINT( "sys__umtx_op ( %#" FMT_REGWORD "x, MUTEX_UNLOCK)", ARG1);
      PRE_REG_READ2(long, "_umtx_op_mutex_unlock",
                    struct umutex *, obj, int, op);
      PRE_MEM_READ( "_umtx_op_mutex_unlock(mutex)", ARG1, sizeof(struct vki_umutex) );
      PRE_MEM_WRITE( "_umtx_op_mutex_unlock(mutex)", ARG1, sizeof(struct vki_umutex) );
      break;
   case VKI_UMTX_OP_SET_CEILING:
      PRINT( "sys__umtx_op ( %#" FMT_REGWORD "x, SET_CEILING, %" FMT_REGWORD "u, %#" FMT_REGWORD "x)", ARG1, ARG3, ARG4);
      PRE_REG_READ4(long, "_umtx_op_set_ceiling",
                    struct umutex *, obj, int, op, unsigned int, ceiling,
                    unsigned int *, old_ceiling);
      PRE_MEM_READ( "_umtx_op_set_ceiling(mutex)", ARG1, sizeof(struct vki_umutex) );
      PRE_MEM_WRITE( "_umtx_op_set_ceiling(mutex)", ARG1, sizeof(struct vki_umutex) );
      if (ARG4) {
         PRE_MEM_WRITE( "_umtx_op_set_ceiling(old_ceiling)", ARG4, sizeof(vki_uint32_t) );
      }
      break;
   case VKI_UMTX_OP_CV_WAIT:
      PRINT( "sys__umtx_op ( %#" FMT_REGWORD "x, CV_WAIT, %" FMT_REGWORD "u, %#" FMT_REGWORD "x, %#" FMT_REGWORD "x)", ARG1, ARG3, ARG4, ARG5);
      PRE_REG_READ5(long, "_umtx_op_cv_wait",
                    struct ucond *, obj, int, op, unsigned long, wflags,
                    struct umutex *, umtx, struct vki_timespec *, timeout);
      PRE_MEM_READ( "_umtx_op_cv_wait(cond)", ARG1, sizeof(struct vki_ucond) );
      PRE_MEM_WRITE( "_umtx_op_cv_wait(cond)", ARG1, sizeof(struct vki_ucond) );
      PRE_MEM_READ( "_umtx_op_cv_wait(mutex)", ARG4, sizeof(struct vki_umutex) );
      PRE_MEM_WRITE( "_umtx_op_cv_wait(mutex)", ARG4, sizeof(struct vki_umutex) );
      if (ARG5) {
         PRE_MEM_READ( "_umtx_op_cv_wait(timespec)", ARG5, sizeof(struct vki_timespec) );
      }
      *flags |= SfMayBlock;
      break;
   case VKI_UMTX_OP_CV_SIGNAL:
      PRINT( "sys__umtx_op ( %#" FMT_REGWORD "x, CV_SIGNAL)", ARG1);
      PRE_REG_READ2(long, "_umtx_op_cv_signal",
                    struct ucond *, obj, int, op);
      PRE_MEM_READ( "_umtx_op_cv_signal(cond)", ARG1, sizeof(struct vki_ucond) );
      PRE_MEM_WRITE( "_umtx_op_cv_signal(cond)", ARG1, sizeof(struct vki_ucond) );
      break;
   case VKI_UMTX_OP_CV_BROADCAST:
      PRINT( "sys__umtx_op ( %#" FMT_REGWORD "x, CV_BROADCAST, %" FMT_REGWORD "u, %#" FMT_REGWORD "x, %#" FMT_REGWORD "x)", ARG1, ARG3, ARG4, ARG5);
      PRE_REG_READ2(long, "_umtx_op_cv_broadcast",
                    struct ucond *, obj, int, op);
      PRE_MEM_READ( "_umtx_op_cv_broadcast(cond)", ARG1, sizeof(struct vki_ucond) );
      PRE_MEM_WRITE( "_umtx_op_cv_broadcast(cond)", ARG1, sizeof(struct vki_ucond) );
      break;
   case VKI_UMTX_OP_WAIT_UINT:
      PRINT( "sys__umtx_op ( %#" FMT_REGWORD "x, CV_WAIT_UINT, %" FMT_REGWORD "u, %#" FMT_REGWORD "x, %#" FMT_REGWORD "x)", ARG1, ARG3, ARG4, ARG5);
      PRE_REG_READ5(long, "_umtx_op_wait_uint",
                    int *, obj, int, op, unsigned long, id,
                    size_t, timeout_wait, struct vki_timespec *, timeout);
      PRE_MEM_READ( "_umtx_op_wait(uint)", ARG1, sizeof(int) );
      if (ARG5) {
         PRE_MEM_READ( "_umtx_op_wait(timespec)", ARG5, ARG4 );
      }
      *flags |= SfMayBlock;
      break;
   case VKI_UMTX_OP_RW_RDLOCK:
      PRINT( "sys__umtx_op ( %#" FMT_REGWORD "x, RW_RDLOCK, %" FMT_REGWORD "u, %#" FMT_REGWORD "x, %#" FMT_REGWORD "x)", ARG1, ARG3, ARG4, ARG5);
      PRE_REG_READ5(long, "_umtx_op_rw_rdlock",
                    struct urwlock *, obj, int, op, unsigned long, noid,
                    void *, zero, struct vki_timespec *, timeout);
      PRE_MEM_READ( "_umtx_op_rw_rdlock(rw)", ARG1, sizeof(struct vki_urwlock) );
      PRE_MEM_WRITE( "_umtx_op_rw_rdlock(rw)", ARG1, sizeof(struct vki_urwlock) );
      *flags |= SfMayBlock;
      break;
   case VKI_UMTX_OP_RW_WRLOCK:
      PRINT( "sys__umtx_op ( %#" FMT_REGWORD "x, RW_WRLOCK, %" FMT_REGWORD "u, %#" FMT_REGWORD "x, %#" FMT_REGWORD "x)", ARG1, ARG3, ARG4, ARG5);
      PRE_REG_READ5(long, "_umtx_op_rw_wrlock",
                    struct urwlock *, obj, int, op, unsigned long, noid,
                    void *, zero, struct vki_timespec *, timeout);
      PRE_MEM_READ( "_umtx_op_rw_wrlock(rw)", ARG1, sizeof(struct vki_urwlock) );
      PRE_MEM_WRITE( "_umtx_op_rw_wrlock(rw)", ARG1, sizeof(struct vki_urwlock) );
      *flags |= SfMayBlock;
      break;
   case VKI_UMTX_OP_RW_UNLOCK:
      PRINT( "sys__umtx_op ( %#" FMT_REGWORD "x, RW_UNLOCK, %" FMT_REGWORD "u, %#" FMT_REGWORD "x, %#" FMT_REGWORD "x)", ARG1, ARG3, ARG4, ARG5);
      PRE_REG_READ2(long, "_umtx_op_rw_unlock",
                    struct urwlock *, obj, int, op);
      PRE_MEM_READ( "_umtx_op_rw_unlock(rw)", ARG1, sizeof(struct vki_urwlock) );
      PRE_MEM_WRITE( "_umtx_op_rw_unlock(rw)", ARG1, sizeof(struct vki_urwlock) );
      break;
   case VKI_UMTX_OP_WAIT_UINT_PRIVATE:
      PRINT( "sys__umtx_op ( %#" FMT_REGWORD "x, CV_WAIT_UINT_PRIVATE, %" FMT_REGWORD "u, %#" FMT_REGWORD "x, %#" FMT_REGWORD "x)", ARG1, ARG3, ARG4, ARG5);
      PRE_REG_READ5(long, "_umtx_op_wait_uint_private",
                    int *, obj, int, op, unsigned long, id,
                    size_t, timeout_size, struct vki_timespec *, timeout);
      PRE_MEM_READ( "_umtx_op_wait_private(uint)", ARG1, sizeof(int) );
      if (ARG5) {
         PRE_MEM_READ( "_umtx_op_wait_private(umtx_time)", ARG5, ARG4 );
      }
      *flags |= SfMayBlock;
      break;
   case VKI_UMTX_OP_WAKE_PRIVATE:
      PRINT( "sys__umtx_op ( %#" FMT_REGWORD "x, CV_WAKE_PRIVATE, %" FMT_REGWORD "u)", ARG1, ARG3);
      PRE_REG_READ3(long, "_umtx_op_wake_private",
                    vki_uintptr_t *, obj, int, op, int, val);
      // PJF like OP_WAKE contents of obj not read
      //PRE_MEM_READ("_umtx_op_wake_private(obj)", ARG1, sizeof(vki_uintptr_t));
      break;
   case VKI_UMTX_OP_MUTEX_WAIT:
      // pthread_mutex_lock without prio flags
      // does not need to be restarted
      PRINT( "sys__umtx_op ( %#" FMT_REGWORD "x, MUTEX_WAIT, %" FMT_REGWORD "u, %#" FMT_REGWORD "x, %#" FMT_REGWORD "x)", ARG1, ARG3, ARG4, ARG5);
      PRE_REG_READ2(long, "_umtx_op_mutex_wait",
                    struct umutex *, obj, int, op);
      PRE_MEM_READ( "_umtx_op_mutex_wait(mutex)", ARG1, sizeof(struct vki_umutex) );
      PRE_MEM_WRITE( "_umtx_op_mutex_wait(mutex)", ARG1, sizeof(struct vki_umutex) );
      *flags |= SfMayBlock;
      break;
   case VKI_UMTX_OP_MUTEX_WAKE:
      // marked as deprecated
      PRINT( "sys__umtx_op ( %#" FMT_REGWORD "x, MUTEX_WAKE, %" FMT_REGWORD "u, %#" FMT_REGWORD "x, %#" FMT_REGWORD "x)", ARG1, ARG3, ARG4, ARG5);
      PRE_REG_READ2(long, "_umtx_op_mutex_wake",
                    struct umutex *, obj, int, op);
      PRE_MEM_READ( "_umtx_op_mutex_wake(mutex)", ARG1, sizeof(struct vki_umutex) );
      PRE_MEM_WRITE( "_umtx_op_mutex_wake(mutex)", ARG1, sizeof(struct vki_umutex) );
      break;
   case VKI_UMTX_OP_SEM_WAIT:
      // marked as deprecated
      PRINT( "sys__umtx_op ( %#" FMT_REGWORD "x, SEM_WAIT, %" FMT_REGWORD "u, %#" FMT_REGWORD "x, %#" FMT_REGWORD "x)", ARG1, ARG3, ARG4, ARG5);
      PRE_REG_READ5(long, "_umtx_op_sem_wait",
                    struct usem *, obj, int, op, unsigned long, id,
                    size_t, timeout_size, struct vki_timespec *, timeout);
      PRE_MEM_READ( "_umtx_op_sem_wait(usem)", ARG1, sizeof(struct vki_usem) );
      PRE_MEM_WRITE( "_umtx_op_sem_wait(usem)", ARG1, sizeof(struct vki_usem) );
      if (ARG5) {
         PRE_MEM_READ( "_umtx_op_sem_wait(umtx_time)", ARG5, ARG4 );
      }
      *flags |= SfMayBlock;
      break;
   case VKI_UMTX_OP_SEM_WAKE:
      // marked as deprecated
      PRINT( "sys__umtx_op ( %#" FMT_REGWORD "x, SEM_WAKE, %" FMT_REGWORD "u, %#" FMT_REGWORD "x, %#" FMT_REGWORD "x)", ARG1, ARG3, ARG4, ARG5);
      PRE_REG_READ2(long, "_umtx_op_sem_wake",
                    struct umutex *, obj, int, op);
      PRE_MEM_READ( "_umtx_op_sem_wake(mutex)", ARG1, sizeof(struct vki_usem) );
      PRE_MEM_WRITE( "_umtx_op_sem_wake(mutex)", ARG1, sizeof(struct vki_usem) );
      break;
   case VKI_UMTX_OP_NWAKE_PRIVATE:
      PRINT( "sys__umtx_op ( %#" FMT_REGWORD "x, NWAKE_PRIVATE, %" FMT_REGWORD "u, %#" FMT_REGWORD "x, %#" FMT_REGWORD "x)", ARG1, ARG3, ARG4, ARG5);
      PRE_REG_READ3(long, "_umtx_op_nwake_private",
                    struct umutex *, obj, int, op, int, count);
      PRE_MEM_READ( "_umtx_op_nwake_private(mtxs)", ARG1, ARG3 * sizeof(void *) );
      PRE_MEM_WRITE( "_umtx_op_mutex_wake(mtxs)", ARG1, sizeof(struct vki_umutex) );
      break;
   case VKI_UMTX_OP_MUTEX_WAKE2:
      PRINT( "sys__umtx_op ( %#" FMT_REGWORD "x, MUTEX_WAKE2, %" FMT_REGWORD "u, %#" FMT_REGWORD "x, %#" FMT_REGWORD "x)", ARG1, ARG3, ARG4, ARG5);
      PRE_REG_READ3(long, "_umtx_op_mutex_wake2",
                    struct umutex *, obj, int, op, unsigned long, flags);
      PRE_MEM_READ( "_umtx_op_mutex_wake(mutex)", ARG1, sizeof(struct vki_umutex) );
      PRE_MEM_WRITE( "_umtx_op_mutex_wake(mutex)", ARG1, sizeof(struct vki_umutex) );
      break;
   case VKI_UMTX_OP_SEM2_WAIT:
      PRINT( "sys__umtx_op ( %#" FMT_REGWORD "x, SEM2_WAIT, %" FMT_REGWORD "u, %#" FMT_REGWORD "x, %#" FMT_REGWORD "x)", ARG1, ARG3, ARG4, ARG5);
      PRE_REG_READ3(long, "_umtx_op_sem2_wake",
                    struct _usem2 *, obj, int, op, unsigned long, flags);
      PRE_MEM_READ( "_umtx_op_sem2_wait(mutex)", ARG1, sizeof(struct vki_usem2) );
      PRE_MEM_WRITE( "_umtx_op_sem2_wait(mutex)", ARG1, sizeof(struct vki_usem2) );
      *flags |= SfMayBlock;
      break;
   case VKI_UMTX_OP_SEM2_WAKE:
      PRINT( "sys__umtx_op ( %#" FMT_REGWORD "x, SEM2_WAKE, %" FMT_REGWORD "u, %#" FMT_REGWORD "x, %#" FMT_REGWORD "x)", ARG1, ARG3, ARG4, ARG5);
      PRE_REG_READ3(long, "_umtx_op_sem2_wake",
                    struct _usem2 *, obj, int, op, unsigned long, flags);
      PRE_MEM_READ( "_umtx_op_sem2_wait(mutex)", ARG1, sizeof(struct vki_usem2) );
      PRE_MEM_WRITE( "_umtx_op_sem2_wait(mutex)", ARG1, sizeof(struct vki_usem2) );
      break;
   case VKI_UMTX_OP_SHM:
      PRINT( "sys__umtx_op ( %#" FMT_REGWORD "x, SHM, %" FMT_REGWORD "u, %#" FMT_REGWORD "x, %#" FMT_REGWORD "x)", ARG1, ARG3, ARG4, ARG5);
      PRE_REG_READ4(long, "_umtx_op_shm",
                    void *, obj, int, op, unsigned long, val, void*, uaddr);
      break;
   case VKI_UMTX_OP_ROBUST_LISTS:
      // strangely the obj pointer ARG1 isn't used, for instance lin libc
      // libthr/thread/thr_mutex.c:      _umtx_op(NULL, UMTX_OP_ROBUST_LISTS, sizeof(rb), &rb, NULL);
      // val (ARG3) ought to be the same as sizeof(struct vki_umtx_robust_lists_params)
      // strangely the kernel returns EINVAL if size is larger than sizeof(struct vki_umtx_robust_lists_params)
      // (which seems relatively harmless)
      // but not if it is smaller (definitely dangerous, probably an overrun)
      if (ARG3 < sizeof(struct vki_umtx_robust_lists_params)) {
         VG_(umsg)("WARNING: _umtx_op_tobust_lists size is smaller than sizeof(struct umtx_robust_lists_params).\n");
      }
      PRINT( "sys__umtx_op ( %#" FMT_REGWORD "x, ROBUST_LISTS, %" FMT_REGWORD "u, %#" FMT_REGWORD "x, %#" FMT_REGWORD "x)", ARG1, ARG3, ARG4, ARG5);
      PRE_REG_READ4(long, "_umtx_op_robust_lists",
                    void*, obj, int, op, unsigned long, val, struct umtx_robust_lists*, uaddr);
      PRE_MEM_READ( "_umtx_op_robust_lists(robust_lists)", ARG4, ARG3 );
      break;
#if (FREEBSD_VERS >= FREEBSD_13_3)
   case VKI_UMTX_OP_GET_MIN_TIMEOUT:
      PRINT( "sys__umtx_op ( GET_MIN_TIMEOUT, %#" FMT_REGWORD "x)", ARG4);
      // bit of a pain just reads args 2 and 4
      if (VG_(tdict).track_pre_reg_read) {
            PRRSN;
            PRA2("_umtx_op_get_min_timeout",int,op);
            PRA4("_umtx_op_get_min_timeout",long int*,timeout);
      }
      PRE_MEM_WRITE( "_umtx_op_get_min_timout(uaddr)", ARG4, sizeof(long int) );
      break;
   case VKI_UMTX_OP_SET_MIN_TIMEOUT:
      PRINT( "sys__umtx_op ( SET_MIN_TIMEOUT, %" FMT_REGWORD "u)", ARG3);
      // bit of a pain just reads args 2 and 3
      if (VG_(tdict).track_pre_reg_read) {
            PRRSN;
            PRA2("_umtx_op_set_min_timeout",int,op);
            PRA3("_umtx_op_set_min_timeout",unsigned long,timeout);
      }
      break;
#endif
   default:
      VG_(umsg)("WARNING: _umtx_op unsupported value.\n");
      PRINT( "sys__umtx_op ( %#" FMT_REGWORD "x, %" FMT_REGWORD "u(UNKNOWN), %" FMT_REGWORD "u, %#" FMT_REGWORD "x, %#" FMT_REGWORD "x )", ARG1, ARG2, ARG3, ARG4, ARG5);
      break;
   }
}

POST(sys__umtx_op)
{
   switch(ARG2) {
   case VKI_UMTX_OP_LOCK:
   case VKI_UMTX_OP_UNLOCK:
      if (SUCCESS) {
         POST_MEM_WRITE( ARG1, sizeof(struct vki_umtx) );
      }
      break;
   case VKI_UMTX_OP_WAIT:
   case VKI_UMTX_OP_WAKE:
   case VKI_UMTX_OP_WAIT_UINT:
   case VKI_UMTX_OP_WAIT_UINT_PRIVATE:
   case VKI_UMTX_OP_WAKE_PRIVATE:
      break;
   case VKI_UMTX_OP_MUTEX_TRYLOCK:
   case VKI_UMTX_OP_MUTEX_LOCK:
   case VKI_UMTX_OP_MUTEX_UNLOCK:
   case VKI_UMTX_OP_MUTEX_WAIT:        /* Sets/clears contested bits */
   case VKI_UMTX_OP_MUTEX_WAKE:        /* Sets/clears contested bits */
      if (SUCCESS) {
         POST_MEM_WRITE( ARG1, sizeof(vki_uintptr_t) );
      }
      break;
   case VKI_UMTX_OP_SET_CEILING:
      if (SUCCESS) {
         POST_MEM_WRITE( ARG1, sizeof(struct vki_umutex) );
         if (ARG4) {
            POST_MEM_WRITE( ARG4, sizeof(vki_uint32_t) );
         }
      }
      break;
   case VKI_UMTX_OP_CV_WAIT:
      if (SUCCESS) {
         POST_MEM_WRITE( ARG1, sizeof(struct vki_ucond) );
         POST_MEM_WRITE( ARG4, sizeof(struct vki_umutex) );
      }
      break;
   case VKI_UMTX_OP_CV_SIGNAL:
   case VKI_UMTX_OP_CV_BROADCAST:
      if (SUCCESS) {
         POST_MEM_WRITE( ARG1, sizeof(struct vki_ucond) );
      }
      break;
   case VKI_UMTX_OP_RW_RDLOCK:
   case VKI_UMTX_OP_RW_WRLOCK:
   case VKI_UMTX_OP_RW_UNLOCK:
      if (SUCCESS) {
         POST_MEM_WRITE( ARG1, sizeof(struct vki_urwlock) );
      }
      break;
   case VKI_UMTX_OP_SEM2_WAIT:
   case VKI_UMTX_OP_SEM2_WAKE:
      if (SUCCESS) {
         POST_MEM_WRITE( ARG1, sizeof(struct vki_usem2) );
      }
      break;
   case VKI_UMTX_OP_SHM:
   case VKI_UMTX_OP_ROBUST_LISTS:
      break;
#if (FREEBSD_VERS >= FREEBSD_13_3)
   case VKI_UMTX_OP_GET_MIN_TIMEOUT:
      POST_MEM_WRITE( ARG4, sizeof(long int) );
      break;
   case VKI_UMTX_OP_SET_MIN_TIMEOUT:
      break;
#endif
   default:
      break;
   }
}

// SYS_thr_new 455
// x86/amd64

// SYS_sigqueue   456
// int sigqueue(pid_t pid, int signo, const union sigval value);
PRE(sys_sigqueue)
{
   PRINT("sys_sigqueue ( %" FMT_REGWORD "d, %" FMT_REGWORD "d, %#" FMT_REGWORD "x )",
         SARG1,SARG2,ARG3);
   PRE_REG_READ3(int, "sigqueue", vki_pid_t, pid, int, signo, const union vki_sigval, value);
}

// SYS_kmq_open   457
// mqd_t mq_open(const char *name, int oflag, ...);
// int kmq_open(_In_z_ const char *path, int flags, mode_t mode, _In_opt_ const struct mq_attr *attr);
PRE(sys_kmq_open)
{
   if (ARG2 & VKI_O_CREAT) {
      PRINT("sys_kmq_open( %#" FMT_REGWORD "x(%s), %" FMT_REGWORD "u, %hu, %#" FMT_REGWORD "x )",
            ARG1,(char *)ARG1,ARG2,(vki_mode_t)ARG3,ARG4);
      PRE_REG_READ4(long, "mq_open",
                    const char *, name, int, oflag, vki_mode_t, mode,
                    struct mq_attr *, attr);
   } else {
      PRINT("sys_kmq_open( %#" FMT_REGWORD "x(%s), %" FMT_REGWORD "u, %hu)",
            ARG1,(char *)ARG1,ARG2,(vki_mode_t)ARG3);
      PRE_REG_READ3(long, "mq_open",
                    const char *, name, int, oflag, vki_mode_t, mode);
   }
   PRE_MEM_RASCIIZ( "mq_open(name)", ARG1 );
   if (ARG2 & VKI_O_CREAT) {
      PRE_MEM_READ("mq_open(attr)", ARG4, sizeof(struct vki_mq_attr));
      if (ML_(safe_to_deref)((struct vki_mq_attr *)ARG4, sizeof(struct vki_mq_attr))) {
         const struct vki_mq_attr *attr = (struct vki_mq_attr *)ARG4;
         PRE_MEM_READ("mq_open(attr->mq_maxmsg)",
                      (Addr)&attr->mq_maxmsg, sizeof(attr->mq_maxmsg) );
         PRE_MEM_READ("mq_open(attr->mq_msgsize)",
                      (Addr)&attr->mq_msgsize, sizeof(attr->mq_msgsize) );
      }
   }
}

POST(sys_kmq_open)
{
   vg_assert(SUCCESS);
   if (!ML_(fd_allowed)(RES, "mq_open", tid, True)) {
      VG_(close)(RES);
      SET_STATUS_Failure( VKI_EMFILE );
   } else {
      if (VG_(clo_track_fds)) {
         ML_(record_fd_open_with_given_name)(tid, RES, (const HChar*)ARG1);
      }
   }
}

// SYS_kmq_setattr   458
// int mq_setattr(mqd_t mqdes, const struct mq_attr *restrict mqstat,
//                struct mq_attr *restrict omqstat);
PRE(sys_kmq_setattr)
{
   PRINT("sys_kmq_getattr( %" FMT_REGWORD "u, %#" FMT_REGWORD "x, %#" FMT_REGWORD "x )", ARG1,ARG2,ARG3 );
   PRE_REG_READ3(int, "mq_setattr",
                 vki_mqd_t, mqdes, const struct mq_attr *, mqstat,
                 struct mq_attr *, omqstat);
   if (!ML_(fd_allowed)(ARG1, "mq_getattr", tid, False)) {
      SET_STATUS_Failure( VKI_EBADF );
   } else {
      if (ML_(safe_to_deref)((struct vki_mq_attr *)ARG2, sizeof(struct vki_mq_attr))) {
         const struct vki_mq_attr *attr = (struct vki_mq_attr *)ARG2;
         PRE_MEM_READ( "mq_setattr(mqstat->mq_flags)",
                       (Addr)&attr->mq_flags, sizeof(attr->mq_flags) );
      }
      PRE_MEM_WRITE( "mq_setattr(omqstat)", ARG3,
                     sizeof(struct vki_mq_attr) );
   }
}

// SYS_kmq_timedreceive 459
// ssize_t mq_timedreceive(mqd_t mqdes, char *msg_ptr, size_t msg_len,
//                         unsigned *msg_prio, const struct timespec *abs_timeout);
PRE(sys_kmq_timedreceive)
{
   *flags |= SfMayBlock;
   PRINT("sys_kmq_timedreceive( %" FMT_REGWORD "u, %#" FMT_REGWORD "x, %llu, %#" FMT_REGWORD "x, %#" FMT_REGWORD "x )",
         ARG1,ARG2,(ULong)ARG3,ARG4,ARG5);
   PRE_REG_READ5(ssize_t, "mq_timedreceive",
                 vki_mqd_t, mqdes, char *, msg_ptr, vki_size_t, msg_len,
                 unsigned int *, msg_prio,
                 const struct timespec *, abs_timeout);
   if (!ML_(fd_allowed)(ARG1, "mq_timedreceive", tid, False)) {
      SET_STATUS_Failure( VKI_EBADF );
   } else {
      PRE_MEM_WRITE( "mq_timedreceive(msg_ptr)", ARG2, ARG3 );
      if (ARG4 != 0) {
         PRE_MEM_WRITE( "mq_timedreceive(msg_prio)",
                        ARG4, sizeof(unsigned int) );
      }
      if (ARG5 != 0) {
         PRE_MEM_READ( "mq_timedreceive(abs_timeout)",
                       ARG5, sizeof(struct vki_timespec) );
      }
   }
}

POST(sys_kmq_timedreceive)
{
   POST_MEM_WRITE( ARG2, ARG3 );
   if (ARG4 != 0) {
      POST_MEM_WRITE( ARG4, sizeof(unsigned int) );
   }
}

// SYS_kmq_timedsend 460
// int mq_timedsend(mqd_t mqdes, const char *msg_ptr, size_t msg_len,
//                  unsigned msg_prio, const struct timespec *abs_timeout);
PRE(sys_kmq_timedsend)
{
   *flags |= SfMayBlock;
   PRINT("sys_kmq_timedsend ( %" FMT_REGWORD "u, %#" FMT_REGWORD "x, %llu, %" FMT_REGWORD "u, %#" FMT_REGWORD "x )",
         ARG1,ARG2,(ULong)ARG3,ARG4,ARG5);
   PRE_REG_READ5(int, "mq_timedsend",
                 vki_mqd_t, mqdes, const char *, msg_ptr, vki_size_t, msg_len,
                 unsigned int, msg_prio, const struct timespec *, abs_timeout);
   if (!ML_(fd_allowed)(ARG1, "mq_timedsend", tid, False)) {
      SET_STATUS_Failure( VKI_EBADF );
   } else {
      PRE_MEM_READ( "mq_timedsend(msg_ptr)", ARG2, ARG3 );
      if (ARG5 != 0) {
         PRE_MEM_READ( "mq_timedsend(abs_timeout)", ARG5,
                       sizeof(struct vki_timespec) );
      }
   }
}

// SYS_kmq_notify 461
// int mq_notify(mqd_t mqdes, const struct sigevent *notification);
PRE(sys_kmq_notify)
{
   PRINT("sys_kmq_notify( %" FMT_REGWORD "u, %#" FMT_REGWORD "x )", ARG1,ARG2 );
   PRE_REG_READ2(int, "mq_notify",
                 vki_mqd_t, mqdes, const struct sigevent *, notification);
   if (!ML_(fd_allowed)(ARG1, "mq_notify", tid, False)) {
      SET_STATUS_Failure( VKI_EBADF );
   }
   else if (ARG2 != 0) {
      PRE_MEM_READ( "mq_notify(notification)",
                    ARG2, sizeof(struct vki_sigevent) );
   }
}

// SYS_kmq_unlink 462
// int kmq_unlink(const char *path);
PRE(sys_kmq_unlink)
{
   PRINT("sys_kmq_unlink ( %#" FMT_REGWORD "x(%s) )", ARG1,(char *)ARG1);
   PRE_REG_READ1(int, "mq_unlink", const char *, name);
   PRE_MEM_RASCIIZ( "mq_unlink(name)", ARG1 );
}

// SYS_abort2  463
// void abort2(const char *why, int nargs, void **args);
PRE(sys_abort2)
{
   PRINT( "sys_abort2 ( %#" FMT_REGWORD "x, %" FMT_REGWORD "d, %#" FMT_REGWORD "x )", ARG1, SARG2, ARG3 );
   PRE_REG_READ3(void, "abort2", const char *, why, int, nargs, void **, args);
   // max length of 'why' is 128
   PRE_MEM_RASCIIZ( "abort2(why)", ARG2);
   // max val for nargs is 16
   PRE_MEM_READ("abort2(args", ARG3, ARG2*sizeof(void*));
}

// SYS_thr_set_name  464
// int thr_set_name(long id, const char *name);
PRE(sys_thr_set_name)
{
   PRINT( "sys_thr_set_name ( %" FMT_REGWORD "u, %#" FMT_REGWORD "x )", ARG1, ARG2 );
   PRE_REG_READ2(int, "thr_set_name", long, id, const char *, name);
   PRE_MEM_RASCIIZ( "thr_set_name(name)", ARG2);

   if (ML_(safe_to_deref)((void*)ARG2, 1)) {
      const HChar* new_name = (const HChar*) (Addr)ARG2;
      ThreadState* tst = VG_(get_ThreadState)(tid);
      SizeT new_len = VG_(strnlen)(new_name, VKI_MAXCOMLEN+1);
      tst->thread_name = VG_(realloc)("syswrap.thr_set_name", tst->thread_name, new_len + 1);
      VG_(strlcpy)(tst->thread_name, new_name, new_len + 1);
   }
}

// SYS_aio_fsync  465
// int aio_fsync(int op, struct aiocb *iocb);
PRE(sys_aio_fsync)
{
   PRINT("aio_fsync ( %" FMT_REGWORD "d, %#" FMT_REGWORD "x )", SARG1,ARG2);
   PRE_REG_READ2(int, "aio_fsync", int, op, struct vki_aiocb *, iocb);
   PRE_MEM_READ( "aio_fsync(iocb)", ARG2, sizeof(struct vki_aiocb) );
}

// SYS_rtprio_thread 466
// int rtprio_thread(int function, lwpid_t lwpid, struct rtprio *rtp);
PRE(sys_rtprio_thread)
{
   PRINT( "sys_rtprio_thread ( %" FMT_REGWORD "u, %" FMT_REGWORD "u, %#" FMT_REGWORD "x )", ARG1, ARG2, ARG3 );
   PRE_REG_READ3(int, "rtprio_thread",
                 int, function, __vki_lwpid_t, lwpid, struct vki_rtprio *, rtp);
   if (ARG1 == VKI_RTP_SET) {
      PRE_MEM_READ( "rtprio_thread(rtp#set)", ARG3, sizeof(struct vki_rtprio));
   } else if (ARG1 == VKI_RTP_LOOKUP) {
      PRE_MEM_WRITE( "rtprio_thread(rtp#lookup)", ARG3, sizeof(struct vki_rtprio));
   } else {
      /* PHK ?? */
   }
}

POST(sys_rtprio_thread)
{
   if (ARG1 == VKI_RTP_LOOKUP && RES == 0) {
      POST_MEM_WRITE( ARG3, sizeof(struct vki_rtprio));
   }
}

// SYS_sctp_peeloff  471
// int sctp_peeloff(int s, sctp_assoc_t id);
// @todo


// SYS_sctp_generic_sendmsg   472
// int sctp_generic_sendmsg(int s, void *msg, int msglen, struct sockaddr *to,
//                          socklen_t len, struct sctp_sndrcvinfo *sinfo, int flags);
//
// Not called directly from libc
PRE(sys_sctp_generic_sendmsg)
{
   *flags |= SfMayBlock;
   PRINT("sys_sctp_generic_sendmsg ( %" FMT_REGWORD "d, %#" FMT_REGWORD "x, %" FMT_REGWORD "d, %#" FMT_REGWORD "x, %" FMT_REGWORD "d, %#" FMT_REGWORD "x, %" FMT_REGWORD "d )",SARG1,ARG2,SARG3,ARG4,SARG5,ARG6,SARG7);
   PRE_REG_READ7(ssize_t, "sctp_generic_sendmsg",
                 int, s, void *, msg, int, msglen,
                 struct sockaddr *, to, socklen_t, len,
                 struct sctp_sndrcvinfo *, sinfo, int, flags);

   PRE_MEM_READ( "sctp_generic_sendmsg(msg)", ARG2, ARG3);

   ML_(pre_mem_read_sockaddr) (tid, "sctp_generic_sendmsg(to)", (struct vki_sockaddr *)ARG4, ARG5);

   if (ARG6 != (Addr)NULL) {
      PRE_MEM_READ( "sctp_generic_sendmsg(sinfo)", ARG6, sizeof(struct vki_sctp_sndrcvinfo));
   }
}

// SYS_sctp_generic_sendmsg_iov  473
// int sctp_generic_sendmsg_iov(int s, struct iovec *iov, int iovlen,
//                              struct sockaddr *to, struct sctp_sndrcvinfo *sinfo, int flags);
// @todo

// SYS_sctp_generic_recvmsg   474
// int sctp_generic_recvmsg(int s, struct iovec *iov, int iovlen,
//                          struct sockaddr *from, socklen_t *fromlen,
//                          struct sctp_sndrcvinfo *sinfo, int *msgflags);
//
// Not called directly from libc
PRE(sys_sctp_generic_recvmsg)
{
   *flags |= SfMayBlock;
   PRINT("sys_sctp_generic_recvmsg ( %" FMT_REGWORD "d, %#" FMT_REGWORD "x, %" FMT_REGWORD "d, %#" FMT_REGWORD "x, %#" FMT_REGWORD "x, %#" FMT_REGWORD "x, %#" FMT_REGWORD "x )",SARG1,ARG2,SARG3,ARG4,ARG5,ARG6,ARG7);
   PRE_REG_READ7(ssize_t, "sctp_generic_recvmsg",
                 int, s, struct iovec *, iov, int, iovlen,
                 struct sockaddr *, from, socklen_t *, fromlen,
                 struct sctp_sndrcvinfo *, sinfo, int *, msgflags);

   // in the sctp_recvmsg libc wrapper this is always 1
   if ((Int)ARG3 > 0) {
      PRE_MEM_READ( "sctp_generic_recvmsg(iov)", ARG2, ARG3 * sizeof(struct vki_iovec) );
   }
   if (ML_(safe_to_deref)((const void*)ARG2, ARG3 * sizeof(struct vki_iovec))) {
      struct vki_iovec* iovec = (struct vki_iovec*)ARG2;
      PRE_MEM_WRITE("sctp_generic_recvmsg(iov.iov_base)", (Addr)iovec->iov_base, iovec->iov_len);
   }

   if (ARG4 != (Addr)NULL) {
      ML_(buf_and_len_pre_check) (tid, ARG4, ARG5,
                        "sctp_generic_recvmsg(from)",
                        "sctp_generic_recvmsg(fromlen_in)");
   }

   if (ARG6 != (Addr)NULL) {
      PRE_MEM_WRITE("sctp_generic_recvmsg(sinfo)", ARG6, sizeof(struct vki_sctp_sndrcvinfo));
   }

   if (ARG7 != (Addr)NULL) {
      PRE_MEM_WRITE("sctp_generic_recvmsg(msgflags)", ARG7, sizeof(int));
   }
}

POST(sys_sctp_generic_recvmsg)
{
   vg_assert(SUCCESS);
   struct vki_iovec* iovec = (struct vki_iovec*)ARG2;
   POST_MEM_WRITE((Addr)iovec->iov_base, iovec->iov_len);

   POST_MEM_WRITE( ARG2, ARG3*sizeof(struct vki_iovec) );

   if (ARG4 != (Addr)NULL) {
      ML_(buf_and_len_post_check) (tid, VG_(mk_SysRes_Success)(RES), ARG4, ARG5,
              "sctp_generic_recvmsg(fromlen_out)");
   }

   if (ARG6 != (Addr)NULL) {
      POST_MEM_WRITE(ARG6, sizeof(struct vki_sctp_sndrcvinfo));
   }

   if (ARG7 != (Addr)NULL) {
      POST_MEM_WRITE(ARG7, sizeof(int));
   }
}

// SYS_pread   475
// x86/amd64

// SYS_pwrite  476
// x86/amd64

// SYS_mmap 477
// x86/amd64

// SYS_lseek   478
// x86/amd64

//SYS_truncate 479
// x86/amd64

// SYS_ftruncate  480
// x86/amd64

// SYS_thr_kill2  481
// int thr_kill2(pid_t pid, long id, int sig);
PRE(sys_thr_kill2)
{
   PRINT("sys_thr_kill2 ( %" FMT_REGWORD "u, %" FMT_REGWORD "u, %" FMT_REGWORD "u )", ARG1,ARG2,ARG3);
   PRE_REG_READ3(int, "thr_kill2", pid_t, pid, long, tid, int, sig);
   if (!ML_(client_signal_OK)(ARG3)) {
      SET_STATUS_Failure( VKI_EINVAL );
      return;
   }

   /* Check to see if this kill gave us a pending signal */
   *flags |= SfPollAfter;

   if (VG_(clo_trace_signals)) {
      VG_(message)(Vg_DebugMsg, "thr_kill2: sending signal %lu to pid %lu/%lu\n",
                   ARG3, ARG1, ARG2);
   }

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

POST(sys_thr_kill2)
{
   if (VG_(clo_trace_signals)) {
      VG_(message)(Vg_DebugMsg, "thr_kill2: sent signal %lu to pid %lu/%lu\n",
                   ARG3, ARG1, ARG2);
   }
}

// SYS_shm_open   482
// int shm_open(const char *path, int flags, mode_t mode);
PRE(sys_shm_open)
{
   PRE_REG_READ3(int, "shm_open",
                 const char *, path, int, flags, vki_mode_t, mode);
   if (ARG1 == VKI_SHM_ANON) {
      PRINT("sys_shm_open(%#" FMT_REGWORD "x(SHM_ANON), %" FMT_REGWORD "u, %hu)", ARG1, ARG2, (vki_mode_t)ARG3);
   } else {
      PRINT("sys_shm_open(%#" FMT_REGWORD "x(%s), %" FMT_REGWORD "u, %hu)", ARG1, (HChar *)ARG1, ARG2, (vki_mode_t)ARG3);
      PRE_MEM_RASCIIZ( "shm_open(path)", ARG1 );
   }
   *flags |= SfMayBlock;
}

POST(sys_shm_open)
{
   vg_assert(SUCCESS);
   if (!ML_(fd_allowed)(RES, "shm_open", tid, True)) {
      VG_(close)(RES);
      SET_STATUS_Failure( VKI_EMFILE );
   } else {
      if (VG_(clo_track_fds)) {
         ML_(record_fd_open_with_given_name)(tid, RES, (HChar*)ARG1);
      }
   }
}

// SYS_shm_unlink 483
// int shm_unlink(const char *path);
PRE(sys_shm_unlink)
{
   PRINT("sys_shm_unlink(%#" FMT_REGWORD "x(%s))", ARG1, (char *)ARG1);
   PRE_REG_READ1(int, "shm_unlink",
                 const char *, path);

   PRE_MEM_RASCIIZ( "shm_unlink(path)", ARG1 );

   *flags |= SfMayBlock;
}

// SYS_cpuset  484
// int cpuset(cpusetid_t *setid);
PRE(sys_cpuset)
{
   PRINT("sys_cpuset ( %#" FMT_REGWORD "x )", ARG1);
   PRE_REG_READ1(int, "cpuset", vki_cpusetid_t *, setid);
   PRE_MEM_WRITE("cpuset(setid)", ARG1, sizeof(vki_cpusetid_t));
}

POST(sys_cpuset)
{
   POST_MEM_WRITE(ARG1, sizeof(vki_cpusetid_t));
}

// SYS_cpuset_setid  485
// amd64 / x86

// SYS_cpuset_getid  486
// amd64 / x86

// SYS_cpuset_getaffinity  487
// amd64 / x86

// SYS_cpuset_setaffinity  488
// amd64 / x86

// SYS_faccessat  489
// int faccessat(int fd, const char *path, int mode, int flag);
PRE(sys_faccessat)
{
   PRINT("sys_faccessat ( %" FMT_REGWORD "u, %#" FMT_REGWORD "x(%s), %" FMT_REGWORD "u )", ARG1,ARG2,(char*)ARG2,ARG3);
   PRE_REG_READ3(int, "faccessat",
                 int, fd, const char *, path, int, flag);
   PRE_MEM_RASCIIZ( "faccessat(path)", ARG2 );
}

// SYS_fchmodat   490
// int fchmodat(int fd, const char *path, mode_t mode, int flag);
PRE(sys_fchmodat)
{
   PRINT("sys_fchmodat ( %" FMT_REGWORD "u, %#" FMT_REGWORD "x(%s), %" FMT_REGWORD "u )", ARG1,ARG2,(char*)ARG2,ARG3);
   PRE_REG_READ4(int, "fchmodat",
                 int, fd, const char *, path, vki_mode_t, mode, int, flag);
   PRE_MEM_RASCIIZ( "fchmodat(path)", ARG2 );
}

// SYS_fchownat   491
// int fchownat(int fd, const char *path, uid_t owner, gid_t group, int flag);
PRE(sys_fchownat)
{
   PRINT("sys_fchownat ( %" FMT_REGWORD "u, %#" FMT_REGWORD "x(%s), 0x%" FMT_REGWORD "x, 0x%" FMT_REGWORD "x, %" FMT_REGWORD "d )",
         ARG1,ARG2,(char*)ARG2,ARG3,ARG4, SARG5);
   PRE_REG_READ5(int, "fchownat",
                 int, fd, const char *, path,
                 vki_uid_t, owner, vki_gid_t, group, int, flag);
   PRE_MEM_RASCIIZ( "fchownat(path)", ARG2 );
}

// SYS_fexecve 492
// int fexecve(int fd, char *const argv[], char *const envp[]);
PRE(sys_fexecve)
{
   PRINT("sys_fexecve ( %" FMT_REGWORD "d, %#" FMT_REGWORD "x, %#" FMT_REGWORD "x )",
         SARG1,ARG2,ARG3);
   PRE_REG_READ3(int, "fexecve",
                 int, fd, char * const *, argv,
                 char * const *, envp);

   if (!ML_(fd_allowed)(ARG1, "fexecve", tid, False)) {
      SET_STATUS_Failure(VKI_EBADF);
      return;
   }

   const HChar *fname;

   if (VG_(resolve_filename)(ARG1, &fname) == False) {
      SET_STATUS_Failure(VKI_ENOENT);
      return;
   }

   struct vg_stat stats;
   if (VG_(fstat)(ARG1, &stats) != 0) {
      SET_STATUS_Failure(VKI_EACCES);
      return;
   }

   Int openFlags;

   if (VG_(resolve_filemode)(ARG1, &openFlags) == False) {
      SET_STATUS_Failure(VKI_ENOENT);
      return;
   }

   /*
    * openFlags is in kernel FFLAGS format
    * (see /usr/include/sys/fcntl.h)
    * which alllows us to tell if RDONLY is set
    *
    */

   Bool isScript = False;

   SysRes res;
   res = VG_(open)(fname, VKI_O_RDONLY,
                   VKI_S_IRUSR|VKI_S_IRGRP|VKI_S_IROTH);
   if (sr_isError(res)) {
      SET_STATUS_Failure(VKI_ENOENT);
      return;
   }

   char buf[2];
   VG_(read)((Int)sr_Res(res), buf, 2);
   VG_(close)((Int)sr_Res(res));
   if (buf[0] == '#' && buf[1] == '!') {
      isScript = True;
   }

   if (isScript) {
      if (!(openFlags & VKI_FREAD)) {
         SET_STATUS_Failure(VKI_EACCES);
         return;
      }
   } else {
      if (!((openFlags & VKI_O_EXEC) ||
            (stats.mode & (VKI_S_IXUSR|VKI_S_IXGRP|VKI_S_IXOTH)))) {
         SET_STATUS_Failure(VKI_EACCES);
         return;
      }
   }

   Addr arg_2 = (Addr)ARG2;
   Addr arg_3 = (Addr)ARG3;

   handle_pre_sys_execve(tid, status, (Addr)fname, arg_2, arg_3, FEXECVE, False);
}

// SYS_freebsd11_fstatat   493
// int fstatat(int fd, const char *path, struct stat *sb, int flag);
#if (FREEBSD_VERS >= FREEBSD_12)
PRE(sys_freebsd11_fstatat)
{
   PRINT("sys_freebsd11_fstatat ( %" FMT_REGWORD "u, %#" FMT_REGWORD "x(%s), %#" FMT_REGWORD "x )", ARG1,ARG2,(char*)ARG2,ARG3);
   PRE_REG_READ4(int, "fstatat",
                 int, fd, const char *, path, struct freebsd11_stat *, buf, int, flag);
   PRE_MEM_RASCIIZ( "fstatat(path)", ARG2 );
   PRE_MEM_WRITE( "fstatat(sb)", ARG3, sizeof(struct vki_freebsd11_stat) );
}

POST(sys_freebsd11_fstatat)
{
   POST_MEM_WRITE( ARG3, sizeof(struct vki_freebsd11_stat) );
}
#else
PRE(sys_fstatat)
{
   PRINT("sys_fstatat ( %" FMT_REGWORD "u, %#" FMT_REGWORD "x(%s), %#" FMT_REGWORD "x )", ARG1,ARG2,(char*)ARG2,ARG3);
   PRE_REG_READ4(int, "fstatat",
                 int, fd, const char *, path, struct stat *, buf, int, flag);
   PRE_MEM_RASCIIZ( "fstatat(path)", ARG2 );
   PRE_MEM_WRITE( "fstatat(sb)", ARG3, sizeof(struct vki_freebsd11_stat) );
}

POST(sys_fstatat)
{
   POST_MEM_WRITE( ARG3, sizeof(struct vki_freebsd11_stat) );
}
#endif

// SYS_futimesat  494
// int futimesat(int fd, const char *path, const struct timeval times[2]);
PRE(sys_futimesat)
{
   PRINT("sys_futimesat ( %" FMT_REGWORD "u, %#" FMT_REGWORD "x(%s), %#" FMT_REGWORD "x )", ARG1,ARG2,(char*)ARG2,ARG3);
   PRE_REG_READ3(int, "futimesat",
                 int, fd, const char *, path, struct timeval *, times);
   if (ARG2 != 0) {
      PRE_MEM_RASCIIZ( "futimesat(path)", ARG2 );
   }
   if (ARG3 != 0) {
      PRE_MEM_READ( "futimesat(times)", ARG3, 2 * sizeof(struct vki_timeval) );
   }
}

// SYS_linkat  495
// int linkat(int fd1, const char *name1, int fd2, const char *name2, int flag);
PRE(sys_linkat)
{
   *flags |= SfMayBlock;
   PRINT("sys_linkat ( %" FMT_REGWORD "u, %#" FMT_REGWORD "x(%s), %" FMT_REGWORD "u, %#" FMT_REGWORD "x(%s), %" FMT_REGWORD "u )",ARG1,ARG2,(char*)ARG2,ARG3,ARG4,(char*)ARG4,ARG5);
   PRE_REG_READ5(int, "linkat",
                 int, fd1, const char *, name1,
                 int, fd2, const char *, name2,
                 int, flag);
   PRE_MEM_RASCIIZ( "linkat(name1)", ARG2);
   PRE_MEM_RASCIIZ( "linkat(name2)", ARG4);
}

// SYS_mkdirat 496
// int mkdirat(int fd, const char *path, mode_t mode);
PRE(sys_mkdirat)
{
   *flags |= SfMayBlock;
   PRINT("sys_mkdirat ( %" FMT_REGWORD "u, %#" FMT_REGWORD "x(%s), %" FMT_REGWORD "u )", ARG1,ARG2,(char*)ARG2,ARG3);
   PRE_REG_READ3(int, "mkdirat",
                 int, fd, const char *, path, int, mode);
   PRE_MEM_RASCIIZ( "mkdirat(path)", ARG2 );
}

// SYS_mkfifoat   497
// int mkfifoat(int fd, const char *path, mode_t mode);
PRE(sys_mkfifoat)
{
   PRINT("sys_mkfifoat ( %" FMT_REGWORD "d, %#" FMT_REGWORD "x(%s), 0x%" FMT_REGWORD "x )",
         SARG1,ARG2,(HChar*)ARG2,ARG3 );
   PRE_REG_READ3(int, "mkfifoat",
                 int, fd, const char *, path, vki_mode_t, mode);
   PRE_MEM_RASCIIZ( "mkfifoat(path)", ARG2 );
}

// SYS_freebsd11_mknodat   498
// int mknodat(int fd, const char *path, mode_t mode, dev_t dev);
#if (FREEBSD_VERS >= FREEBSD_12)
PRE(sys_freebsd11_mknodat)
{
   PRINT("sys_freebsd11_mknodat ( %" FMT_REGWORD "u, %#" FMT_REGWORD "x(%s), 0x%" FMT_REGWORD "x, 0x%" FMT_REGWORD "x )", ARG1,ARG2,(char*)ARG2,ARG3,ARG4 );
   PRE_REG_READ4(long, "mknodat",
                 int, dfd, const char *, pathname, int, mode, unsigned, dev);
   PRE_MEM_RASCIIZ( "mknodat(pathname)", ARG2 );
}
#else
PRE(sys_mknodat)
{
   PRINT("sys_mknodat ( %" FMT_REGWORD "u, %#" FMT_REGWORD "x(%s), 0x%" FMT_REGWORD "x, 0x%" FMT_REGWORD "x )", ARG1,ARG2,(char*)ARG2,ARG3,ARG4 );
   PRE_REG_READ4(long, "mknodat",
                 int, dfd, const char *, pathname, int, mode, unsigned, dev);
   PRE_MEM_RASCIIZ( "mknodat(pathname)", ARG2 );
}
#endif

// SYS_openat  499
// int openat(int fd, const char *path, int flags, ...);
PRE(sys_openat)
{

   if (ARG3 & VKI_O_CREAT) {
      // 4-arg version
      PRINT("sys_openat ( %" FMT_REGWORD "u, %#" FMT_REGWORD "x(%s), %" FMT_REGWORD "u, %" FMT_REGWORD "u )",ARG1,ARG2,(char*)ARG2,ARG3,ARG4);
      PRE_REG_READ4(int, "openat",
                    int, fd, const char *, path, int, flags, vki_mode_t, mode);
   } else {
      // 3-arg version
      PRINT("sys_openat ( %" FMT_REGWORD "u, %#" FMT_REGWORD "x(%s), %" FMT_REGWORD "u )",ARG1,ARG2,(char*)ARG2,ARG3);
      PRE_REG_READ3(int, "openat",
                    int, fd, const char *, path, int, flags);
   }

   if (ARG1 != (unsigned)VKI_AT_FDCWD && !ML_(fd_allowed)(ARG1, "openat", tid, False)) {
      SET_STATUS_Failure( VKI_EBADF );
   } else {
      PRE_MEM_RASCIIZ( "openat(path)", ARG2 );
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
      if (VG_(clo_track_fds)) {
         ML_(record_fd_open_with_given_name)(tid, RES, (HChar*)ARG2);
      }
   }
}

// SYS_readlinkat 500
// ssize_t readlinkat(int fd, const char *restrict path, char *restrict buf,
//                    size_t bufsize);
PRE(sys_readlinkat)
{
   Word  saved = SYSNO;
   Bool curproc_file = False;

   PRINT("sys_readlinkat ( %" FMT_REGWORD "u, %#" FMT_REGWORD "x(%s), %#" FMT_REGWORD "x, %llu )", ARG1,ARG2,(char*)ARG2,ARG3,(ULong)ARG4);
   PRE_REG_READ4(ssize_t, "readlinkat",
                 int, fd, const char *, path, char *, buf, int, bufsize);
   PRE_MEM_RASCIIZ( "readlinkat(path)", ARG2 );
   PRE_MEM_WRITE( "readlinkat(buf)", ARG3,ARG4 );

   if (VG_(have_slash_proc) == True && (Int)ARG1 == VKI_AT_FDCWD) {
      /*
       * Handle the case where readlinkat is looking at /proc/curproc/file or
       * /proc/<pid>/file.
       */
      do_readlink((const HChar *)ARG2, (HChar *)ARG3, (SizeT)ARG4, status, &curproc_file);
   }

   // @todo PJF there is still the case where fd refers to /proc or /proc/pid
   // or /proc/curproc and path is relative pid/file, curptoc/file or just file

   if (!curproc_file) {
      /* Normal case */
      SET_STATUS_from_SysRes( VG_(do_syscall4)(saved, ARG1, ARG2, ARG3, ARG4));
   }
   if (SUCCESS && RES > 0) {
      POST_MEM_WRITE( ARG3, RES );
   }
}

POST(sys_readlinkat)
{
   POST_MEM_WRITE( ARG3, RES );
}

// SYS_renameat   501
// int renameat(int fromfd, const char *from, int tofd, const char *to);
PRE(sys_renameat)
{
   PRINT("sys_renameat ( %" FMT_REGWORD "u, %#" FMT_REGWORD "x(%s), %" FMT_REGWORD "u, %#" FMT_REGWORD "x(%s) )", ARG1,ARG2,(char*)ARG2,ARG3,ARG4,(char*)ARG4);
   PRE_REG_READ4(int, "renameat",
                 int, fromfd, const char *, from,
                 int, tofd, const char *, to);
   PRE_MEM_RASCIIZ( "renameat(oldpath)", ARG2 );
   PRE_MEM_RASCIIZ( "renameat(newpath)", ARG4 );
}

// SYS_symlinkat  502
// int symlinkat(const char *name1, int fd, const char *name2);
PRE(sys_symlinkat)
{
   *flags |= SfMayBlock;
   PRINT("sys_symlinkat ( %#" FMT_REGWORD "x(%s), %" FMT_REGWORD "u, %#" FMT_REGWORD "x(%s) )",ARG1,(char*)ARG1,ARG2,ARG3,(char*)ARG3);
   PRE_REG_READ3(int, "symlinkat",
                 const char *, name1, int, fd, const char *, name2);
   PRE_MEM_RASCIIZ( "symlinkat(name1)", ARG1 );
   PRE_MEM_RASCIIZ( "symlinkat(name2)", ARG3 );
}

// SYS_unlinkat   503
// int unlinkat(int fd, const char *path, int flag);
PRE(sys_unlinkat)
{
   *flags |= SfMayBlock;
   PRINT("sys_unlinkat ( %" FMT_REGWORD "u, %#" FMT_REGWORD "x(%s), %" FMT_REGWORD "u ",
         ARG1, ARG2, (char*)ARG2, ARG3);
   PRE_REG_READ3(int, "unlinkat", int, fd, const char *, path, int, flag);
   PRE_MEM_RASCIIZ( "unlinkat(path)", ARG2 );
}

// SYS_posix_openpt  504
// int posix_openpt(int oflag);
PRE(sys_posix_openpt)
{
   PRINT("sys_posix_openpt ( %" FMT_REGWORD "d )", SARG1);
   PRE_REG_READ1(int, "posix_openpt", int, oflag);
}

// SYS_gssd_syscall  505
// @todo
// see https://www.freebsd.org/cgi/man.cgi?format=html&query=gssapi(3)
// syscalls.master says ; 505 is initialised by the kgssapi code, if present.

// SYS_jail_get   506
// int jail_get(struct iovec *iov, u_int niov, int flags);
PRE(sys_jail_get)
{
   PRINT("sys_jail_get ( %#" FMT_REGWORD "x, %" FMT_REGWORD "u, %" FMT_REGWORD "u )", ARG1, ARG2, ARG3);
   PRE_REG_READ3(int, "jail_get", struct vki_iovec *, iov, unsigned int,
                 niov, int, flags);
   PRE_MEM_READ("jail_get(iov)", ARG1, ARG2 * sizeof(struct vki_iovec));
}

// SYS_jail_set   507
// int jail_set(struct iovec *iov, u_int niov, int flags);
PRE(sys_jail_set)
{
   PRINT("sys_jail_set ( %#" FMT_REGWORD "x, %" FMT_REGWORD "u, %" FMT_REGWORD "u )", ARG1, ARG2, ARG3);
   PRE_REG_READ3(int, "jail_set", struct vki_iovec *, iov, unsigned int,
                 niov, int, flags);
   PRE_MEM_READ("jail_set(iovs)", ARG1, ARG2 * sizeof(struct vki_iovec));
}

// SYS_jail_remove   508
// int jail_remove(int jid);
PRE(sys_jail_remove)
{
   PRINT("sys_jail_remove ( %" FMT_REGWORD "d )", SARG1);
   PRE_REG_READ1(int, "jail_remove", int, jid);
}

// SYS_closefrom  509
// void closefrom(int lowfd);
PRE(sys_closefrom)
{
   PRINT("sys_closefrom ( %" FMT_REGWORD "dx  )", SARG1);
   PRE_REG_READ1(int, "closefrom", int, lowfd);

   /*
    * Can't pass this on to the kernel otherwise it will close
    * all of the host files like the log
    */

   for (int i = ARG1; i < VG_(fd_soft_limit); ++i) {
      VG_(close)(i);
   }

   SET_STATUS_Success(0);
}

// SYS___semctl   510
// int semctl(int semid, int semnum, int cmd, ...);
// int __semctl(int semid, int semnum, int cmd, _Inout_ union semun *arg);
PRE(sys___semctl)
{
   switch (ARG3) {
   case VKI_IPC_INFO:
   case VKI_SEM_INFO:
      PRINT("sys_semctl ( %" FMT_REGWORD "u, %" FMT_REGWORD "u, %" FMT_REGWORD "u, %#" FMT_REGWORD "x )",ARG1,ARG2,ARG3,ARG4);
      PRE_REG_READ4(int, "semctl",
                    int, semid, int, semnum, int, cmd, struct seminfo *, arg);
      break;
   case VKI_IPC_STAT:
   case VKI_SEM_STAT:
   case VKI_IPC_SET:
      PRINT("sys_semctl ( %" FMT_REGWORD "u, %" FMT_REGWORD "u, %" FMT_REGWORD "u, %#" FMT_REGWORD "x )",ARG1,ARG2,ARG3,ARG4);
      PRE_REG_READ4(long, "semctl",
                    int, semid, int, semnum, int, cmd, struct semid_ds *, arg);
      break;
   case VKI_GETALL:
   case VKI_SETALL:
      PRINT("sys_semctl ( %" FMT_REGWORD "u, %" FMT_REGWORD "u, %" FMT_REGWORD "u, %#" FMT_REGWORD "x )",ARG1,ARG2,ARG3,ARG4);
      PRE_REG_READ4(long, "semctl",
                    int, semid, int, semnum, int, cmd, unsigned short *, arg);
      break;
   default:
      PRINT("sys_semctl ( %" FMT_REGWORD "u, %" FMT_REGWORD "u, %" FMT_REGWORD "u )",ARG1,ARG2,ARG3);
      PRE_REG_READ3(long, "semctl",
                    int, semid, int, semnum, int, cmd);
      break;
   }
   ML_(generic_PRE_sys_semctl)(tid, ARG1,ARG2,ARG3,ARG4);
}

POST(sys___semctl)
{
   ML_(generic_POST_sys_semctl)(tid, RES,ARG1,ARG2,ARG3,ARG4);
}

// SYS_msgctl  511
// int msgctl(int msqid, int cmd, struct msqid_ds *buf);
PRE(sys_msgctl)
{
   PRINT("sys_msgctl ( %" FMT_REGWORD "d, %" FMT_REGWORD "d, %#" FMT_REGWORD "x )", SARG1,SARG2,ARG3 );

   PRE_REG_READ3(int, "msgctl", int, msqid, int, cmd, struct msqid_ds *, buf);

   switch (ARG2 /* cmd */) {
   case VKI_IPC_STAT:
      PRE_MEM_WRITE( "msgctl(IPC_STAT, buf)",
                     ARG3, sizeof(struct vki_msqid_ds) );
      break;
   case VKI_IPC_SET:
      PRE_MEM_READ( "msgctl(IPC_SET, buf)",
                    ARG3, sizeof(struct vki_msqid_ds) );
      break;
   }
}

POST(sys_msgctl)
{
   switch (ARG2 /* cmd */) {
   case VKI_IPC_STAT:
      POST_MEM_WRITE( ARG3, sizeof(struct vki_msqid_ds) );
      break;
   }
}


// SYS_shmctl  512
// int shmctl(int shmid, int cmd, struct shmid_ds *buf);
PRE(sys_shmctl)
{
   PRINT("sys_shmctl ( %" FMT_REGWORD "d, %" FMT_REGWORD "d, %#" FMT_REGWORD "x )",SARG1,SARG2,ARG3);
   PRE_REG_READ3(int, "shmctl",
                 int, shmid, int, cmd, struct vki_shmid_ds *, buf);
   switch (ARG2 /* cmd */) {
   case VKI_IPC_STAT:
      PRE_MEM_WRITE( "shmctl(IPC_STAT, buf)",
                     ARG3, sizeof(struct vki_shmid_ds) );
      break;
   case VKI_IPC_SET:
      PRE_MEM_READ( "shmctl(IPC_SET, buf)",
                    ARG3, sizeof(struct vki_shmid_ds) );
      break;
   }
}

POST(sys_shmctl)
{
   if (ARG2 == VKI_IPC_STAT) {
      POST_MEM_WRITE( ARG3, sizeof(struct vki_shmid_ds) );
   }
}

// SYS_lpathconf  513
// long lpathconf(const char *path, int name);
PRE(sys_lpathconf)
{
   PRINT("sys_lpathconf ( %#" FMT_REGWORD "x, %" FMT_REGWORD "d)", ARG1, SARG2);
   PRE_REG_READ2(long, "lpathconf", const char *, path, int, name);
   PRE_MEM_RASCIIZ("lpathconf(path)", ARG1);
}

// SYS___cap_rights_get 515
// note extra 1st argument for the internal function which is not present
// in the public interface
// int __cap_rights_get(int version, int fd, cap_rights_t *rights);
PRE(sys_cap_rights_get)
{
   PRINT("sys_cap_rights_get ( %" FMT_REGWORD "d, %" FMT_REGWORD "d, %#" FMT_REGWORD "x )", SARG1, SARG2, ARG3);
   PRE_REG_READ3(long, "cap_rights_get", int, version, int, fd, vki_cap_rights_t*, rights);
   PRE_MEM_WRITE("cap_rights_get(rights)", ARG3, sizeof(vki_cap_rights_t));
}

POST(sys_cap_rights_get)
{
   POST_MEM_WRITE(ARG2, sizeof(vki_cap_rights_t));
}

// SYS_cap_enter  516
// int cap_enter(void);
PRE(sys_cap_enter)
{
   PRINT("%s", "sys_cap_enter ( )");
   PRE_REG_READ0(int, "cap_enter");
   static Bool warning_given = False;
   if (!warning_given) {
      warning_given = True;
      capabiltyMode = True;
      VG_(umsg)(
         "WARNING: Valgrind may not operate correctly in capability mode.\n"
         "         Please consider disabling capability by using the RUNNING_ON_VALGRIND mechanism.\n"
         "         See http://valgrind.org/docs/manual/manual-core-adv.html#manual-core-adv.clientreq\n");
   }
   /* now complete loading debuginfo since it is not allowed after entering cap mode */
   VG_(load_all_debuginfo)();
}

// SYS_cap_getmode   517
// int cap_getmode(u_int *modep);
PRE(sys_cap_getmode)
{
   PRINT("sys_cap_getmode ( %#" FMT_REGWORD "x )", ARG1);
   PRE_REG_READ1(int, "cap_getmode", u_int*, modep);
   PRE_MEM_WRITE("cap_getmode(modep)", ARG1, sizeof(u_int));
}

POST(sys_cap_getmode)
{
   POST_MEM_WRITE(ARG1, sizeof(u_int));
}

static vki_sigset_t pdfork_saved_mask;

// SYS_pdfork  518
// pid_t pdfork(int *fdp, int flags);
PRE(sys_pdfork)
{
   Bool is_child;
   Int child_pid;
   vki_sigset_t mask;

   PRINT("sys_pdfork ( %#" FMT_REGWORD "x, %#" FMT_REGWORD "x )", ARG1, ARG2);
   PRE_REG_READ2(pid_t, "pdfork", int*, fdp, int, flags);

   /* Block all signals during fork, so that we can fix things up in
      the child without being interrupted. */
   VG_(sigfillset)(&mask);
   VG_(sigprocmask)(VKI_SIG_SETMASK, &mask, &pdfork_saved_mask);

   VG_(do_atfork_pre)(tid);

   SET_STATUS_from_SysRes( VG_(do_syscall2)(__NR_pdfork, ARG1, ARG2) );

   if (!SUCCESS) {
      return;
   }

   // RES is 0 for child, non-0 (the child's PID) for parent.
   is_child = ( RES == 0 ? True : False );
   child_pid = ( is_child ? -1 : (Int)RES );

   if (is_child) {
      VG_(do_atfork_child)(tid);

      /* restore signal mask */
      VG_(sigprocmask)(VKI_SIG_SETMASK, &pdfork_saved_mask, NULL);
   } else {
      VG_(do_atfork_parent)(tid);

      PRINT("   fork: process %d created child %d\n", VG_(getpid)(), child_pid);

      /* restore signal mask */
      VG_(sigprocmask)(VKI_SIG_SETMASK, &pdfork_saved_mask, NULL);
   }

   if (ARG1) {
      PRE_MEM_WRITE( "pdfork(fdp)", ARG1, sizeof(int) );
   }
}

POST(sys_pdfork)
{
   if (ARG1) {
      POST_MEM_WRITE( ARG1, sizeof(int) );
   }
}

// pdkill   519
//int pdkill(int fd, int signum)
PRE(sys_pdkill)
{
   PRINT("sys_pdkill ( %" FMT_REGWORD "u, %" FMT_REGWORD "d )", ARG1, SARG2);
   PRE_REG_READ2(int, "pdkill", int, fd, int, signum);

   if (!ML_(client_signal_OK)(ARG2)) {
      SET_STATUS_Failure( VKI_EINVAL );
      return;
   }

   /* Ther was some code here to check if the kill is to this process
    *
    * But it was totally wrong
    *
    * It was calling ML_(do_sigkill)(Int pid, Int tgid)
    *
    * With a file descriptor
    *
    * Fortunately this will never match a real process otherwise
    * it might have accidentally killed us.
    *
    * For a start we need the pid, obtained with pdgetpid
    * Next ML_(do_sigkill) doesn't map to FreeBSD. It takes a
    * pid (lwpid) and a tgid (threadgroup)
    *
    * On FreeBSD lwpid is the tid and threadgroup is the pid
    * The kill functions operate on pids, not tids.
    *
    * One last thing, I don't see how pdkill could do a self
    * kill 9. It neads an fd which implied pdfork whichimplies
    * that the fd/pid are for a child process
    */

   SET_STATUS_from_SysRes(VG_(do_syscall2)(SYSNO, ARG1, ARG2));

   if (VG_(clo_trace_signals)) {
      VG_(message)(Vg_DebugMsg, "pdkill: sent signal %ld to fd %ld\n",
                   SARG2, SARG1);
   }

   /* This kill might have given us a pending signal.  Ask for a check once
      the syscall is done. */
   *flags |= SfPollAfter;

}

// SYS_pdgetpid   520
// int pdgetpid(int fd, pid_t *pidp);
PRE(sys_pdgetpid)
{
   PRINT("pdgetpid ( %" FMT_REGWORD "d, %#lx )", SARG1, ARG2);
   PRE_REG_READ2(int, "pdgetpid",
                 int, fd, pid_t*, pidp);
   PRE_MEM_WRITE( "pdgetpid(pidp))", ARG2, sizeof(vki_pid_t) );
}

POST(sys_pdgetpid)
{
   POST_MEM_WRITE( ARG2, sizeof(vki_pid_t) );
}

// SYS_pselect 522

// int pselect(int nfds, fd_set * restrict readfds, fd_set * restrict writefds,
//             fd_set * restrict exceptfds,
//             const struct timespec * restrict timeout,
//             const sigset_t * restrict newsigmask);
PRE(sys_pselect)
{
   *flags |= SfMayBlock | SfPostOnFail;
   PRINT("sys_pselect ( %ld, %#" FMT_REGWORD "x, %#" FMT_REGWORD "x, %#"
         FMT_REGWORD "x, %#" FMT_REGWORD "x, %#" FMT_REGWORD "x )",
         SARG1, ARG2, ARG3, ARG4, ARG5, ARG6);
   PRE_REG_READ6(int, "pselect",
                 int, nfds, vki_fd_set *, readfds, vki_fd_set *, writefds,
                 vki_fd_set *, exceptfds, struct vki_timespec *, timeout,
                 const sigset_t *, newsigmask);
   // XXX: this possibly understates how much memory is read.
   if (ARG2 != 0) {
      PRE_MEM_READ( "pselect(readfds)",
                    ARG2, ARG1/8 /* __FD_SETSIZE/8 */ );
   }
   if (ARG3 != 0) {
      PRE_MEM_READ( "pselect(writefds)",
                    ARG3, ARG1/8 /* __FD_SETSIZE/8 */ );
   }
   if (ARG4 != 0) {
      PRE_MEM_READ( "pselect(exceptfds)",
                    ARG4, ARG1/8 /* __FD_SETSIZE/8 */ );
   }
   if (ARG5 != 0) {
      PRE_MEM_READ( "pselect(timeout)", ARG5, sizeof(struct vki_timeval) );
   }

   if (ARG6 != 0) {
      PRE_MEM_READ( "pselect(sig)", ARG6, sizeof(vki_sigset_t) );
      ARG6 = ML_(make_safe_mask)("syswrap.pselect.1", (Addr)ARG6);
   }
}

POST(sys_pselect)
{
   ML_(free_safe_mask) ( (Addr)ARG6 );
}

// SYS_getloginclass 523
// int getloginclass(char *name, size_t len);
PRE(sys_getloginclass)
{
   PRINT("sys_getloginclass ( %#" FMT_REGWORD "x, %" FMT_REGWORD "u  )", ARG1, ARG2);
   PRE_REG_READ2(int, "getloginclass", char *, name, size_t, len);
   // The buffer should be at least MAXLOGNAME bytes in length.
   PRE_MEM_WRITE("getloginclass(name)", ARG1, ARG2);
}

POST(sys_getloginclass)
{
   POST_MEM_WRITE(ARG1, ARG2);
}

// SYS_setloginclass 524
// int setloginclass(const char *name);
PRE(sys_setloginclass)
{
   PRINT("sys_setloginclass ( %#" FMT_REGWORD "x(%s) )", ARG1, (HChar*)ARG1);
   PRE_REG_READ1(int, "setloginclass", const char *, name);
   PRE_MEM_RASCIIZ("rctl_setloginclass(name)", ARG1);
}

// SYS_rctl_get_racct   525
// int rctl_get_racct(const char *inbufp, size_t inbuflen, char *outbufp,
//                    size_t outbuflen);
PRE(sys_rctl_get_racct)
{
   PRINT("sys_rctl_get_racct ( %#" FMT_REGWORD "x, %" FMT_REGWORD "u, %#" FMT_REGWORD "xd, %" FMT_REGWORD "u )", ARG1, ARG2, ARG3, ARG4);
   PRE_REG_READ4(int, "rctl_get_racct", const char *, inbufp, size_t, inbuflen, char *, outbufp,
                 size_t, outbuflen);
   PRE_MEM_READ("rctl_get_racct(inbufp)", ARG1, ARG2);
   PRE_MEM_WRITE("rctl_get_racct(outbufp)", ARG3, ARG4);
}

POST(sys_rctl_get_racct)
{
   POST_MEM_WRITE(ARG3, ARG4);
}

// SYS_rctl_get_rules   526
// int rctl_get_rules(const char *inbufp, size_t inbuflen, char *outbufp,
//                    size_t outbuflen);
PRE(sys_rctl_get_rules)
{
   PRINT("sys_rctl_get_rules ( %#" FMT_REGWORD "xd, %" FMT_REGWORD "u, %#" FMT_REGWORD "xd, %" FMT_REGWORD "u )", ARG1, ARG2, ARG3, ARG4);
   PRE_REG_READ4(int, "rctl_get_rules", const char *, inbufp, size_t, inbuflen, char *, outbufp,
                 size_t, outbuflen);
   PRE_MEM_READ("rctl_get_rules(inbufp)", ARG1, ARG2);
   PRE_MEM_WRITE("rctl_get_rules(outbufp)", ARG3, ARG4);
}

POST(sys_rctl_get_rules)
{
   POST_MEM_WRITE(ARG3, ARG4);
}

// SYS_rctl_get_limits  527
// int rctl_get_limits(const char *inbufp, size_t inbuflen, char *outbufp,
//                     size_t outbuflen);
PRE(sys_rctl_get_limits)
{
   PRINT("sys_rctl_get_limits ( %#" FMT_REGWORD "xd, %" FMT_REGWORD "u, %#" FMT_REGWORD "xd, %" FMT_REGWORD "u )", ARG1, ARG2, ARG3, ARG4);
   PRE_REG_READ4(int, "rctl_get_limits", const char *, inbufp, size_t, inbuflen, char *, outbufp,
                 size_t, outbuflen);
   PRE_MEM_READ("rctl_get_limits(inbufp)", ARG1, ARG2);
   PRE_MEM_WRITE("rctl_get_limits(outbufp)", ARG3, ARG4);
}

POST(sys_rctl_get_limits)
{
   POST_MEM_WRITE(ARG3, ARG4);
}

// SYS_rctl_add_rule 528
// int rctl_add_rule(const char *inbufp, size_t inbuflen, char *outbufp,
//                   size_t outbuflen);
PRE(sys_rctl_add_rule)
{
   PRINT("sys_rctl_add_rule ( %#" FMT_REGWORD "xd, %" FMT_REGWORD "u, %#" FMT_REGWORD "xd, %" FMT_REGWORD "u )", ARG1, ARG2, ARG3, ARG4);
   PRE_REG_READ2(int, "rctl_add_rule", const char *, inbufp, size_t, inbuflen);
   PRE_MEM_READ("rctl_add_rule(inbufp)", ARG1, ARG2);
   // man page says
   // The outbufp and outbuflen arguments are unused
   //PRE_MEM_WRITE("rctl_add_rule(outbufp)", ARG3, ARG4);
}

POST(sys_rctl_add_rule)
{
   //POST_MEM_WRITE(ARG3, ARG4);
}

// SYS_rctl_remove_rule 529
// int rctl_remove_rule(const char *inbufp, size_t inbuflen, char *outbufp,
//          size_t outbuflen);
PRE(sys_rctl_remove_rule)
{
   PRINT("sys_rctl_remove_rule ( %#" FMT_REGWORD "xd, %" FMT_REGWORD "u, %#" FMT_REGWORD "xd, %" FMT_REGWORD "u )", ARG1, ARG2, ARG3, ARG4);
   PRE_REG_READ2(int, "rctl_remove_rule", const char *, inbufp, size_t, inbuflen);
   PRE_MEM_READ("rctl_remove_rule(inbufp)", ARG1, ARG2);
   // man page says
   // The outbufp and outbuflen arguments are unused
   //PRE_MEM_WRITE("rctl_remove_rule(outbufp)", ARG3, ARG4);
}

POST(sys_rctl_remove_rule)
{
   //POST_MEM_WRITE(ARG3, ARG4);
}

// SYS_posix_fallocate  530
// x86/amd64

// SYS_posix_fadvise 531
// x86/amd64

// SYS_wait6   532
// amd64 / x86

// SYS_cap_rights_limit 533
//int cap_rights_limit(int fd, const cap_rights_t *rights);
PRE(sys_cap_rights_limit)
{
   PRINT("sys_cap_rights_limit ( %" FMT_REGWORD "d, %#" FMT_REGWORD "x )", SARG1, ARG2);
   PRE_REG_READ2(int, "cap_rights_limit",
                 int, fd, const cap_rights_t *, rights);
   PRE_MEM_READ( "cap_rights_limit(rights)", ARG2, sizeof(struct vki_cap_rights) );
}

// SYS_cap_ioctls_limit 534
// int cap_ioctls_limit(int fd, const unsigned long *cmds, size_t ncmds);
PRE(sys_cap_ioctls_limit)
{
   PRINT("cap_ioctls_limit ( %" FMT_REGWORD "u, %#" FMT_REGWORD "x, %" FMT_REGWORD "u )", ARG1, ARG2, ARG3);
   PRE_REG_READ3(int, "cap_ioctls_limit",
                 int, fd, unsigned long*, rights, vki_size_t, ncmds);
   // "can be up to 256" taking that to not be inclusive
   if (ARG3 < 256 ) {
      PRE_MEM_READ( "cap_ioctls_limit(cmds))", ARG2, ARG3*sizeof(unsigned long) );
   }
   // else fail?
}

// SYS_cap_ioctls_get   535
// int cap_ioctls_get(int fd, unsigned long *cmds, size_t maxcmds);
PRE(sys_cap_ioctls_get)
{
   PRINT("sys_cap_ioctls_get ( %" FMT_REGWORD "d, %#" FMT_REGWORD "x, %" FMT_REGWORD "u )", SARG1, ARG2, ARG3);
   PRE_REG_READ3(int, "cap_ioctls_get", int, fd, unsigned long *, cmds, size_t, maxcmds);
   if (ARG3 < 256) {
      PRE_MEM_WRITE("cap_ioctls_get(cmds)", ARG2, ARG3*sizeof(unsigned long));
   }
}

POST(sys_cap_ioctls_get)
{
   if (ARG3 < 256) {
      POST_MEM_WRITE(ARG2, ARG3*sizeof(unsigned long));
   }
}


// SYS_cap_fcntls_limit 536
//int cap_fcntls_limit(int fd, uint32_t fcntlrights);
PRE(sys_cap_fcntls_limit)
{
   PRINT("cap_fcntls_limit ( %" FMT_REGWORD "d, %" FMT_REGWORD "u )", SARG1, ARG2);
   PRE_REG_READ2(long, "cap_fcntls_limit",
                 int, fd, vki_uint32_t, fcntlrights);
}

// SYS_cap_fcntls_get   537
// int cap_fcntls_get(int fd, uint32_t *fcntlrightsp);
PRE(sys_cap_fcntls_get)
{
   PRINT("sys_cap_fcntls_get ( %" FMT_REGWORD "d, %#" FMT_REGWORD "x )", SARG1, ARG2);
   PRE_REG_READ2(int, "cap_fcntls_get", int, fd, uint32_t *, fcntlrightsp);
   PRE_MEM_WRITE("cap_fcntls_get(fcntlrightsp)", ARG2, sizeof(uint32_t));
}

POST(sys_cap_fcntls_get)
{
   POST_MEM_WRITE(ARG2, sizeof(uint32_t));
}

// SYS_bindat  538
// int bindat(int fd, int s, const struct sockaddr *addr, socklen_t addrlen);
PRE(sys_bindat)
{
   PRINT("sys_bindat ( %" FMT_REGWORD "d, %" FMT_REGWORD "dx, %#" FMT_REGWORD "x, %" FMT_REGWORD "u )",
         SARG1, SARG2, ARG3, ARG4);
   PRE_REG_READ4(int, "bindat", int, fd, int, s, const struct vki_sockaddr *, name, vki_socklen_t, namelen);
   PRE_MEM_READ("bindat(name)", ARG3, ARG4);
}

// SYS_connectat  539
// int connectat(int fd, int s, const struct sockaddr *name, socklen_t namelen);
PRE(sys_connectat)
{
   PRINT("sys_connectat ( %" FMT_REGWORD "d, %" FMT_REGWORD "dx, %#" FMT_REGWORD "x, %" FMT_REGWORD "u )",
         SARG1, SARG2, ARG3, ARG4);
   PRE_REG_READ4(int, "connectat", int, fd, int, s, const struct vki_sockaddr *, name, vki_socklen_t, namelen);
   PRE_MEM_READ("connectat(name)", ARG3, ARG4);
}

// SYS_chflagsat  540
// int chflagsat(int fd, const char *path, unsigned long flags, int atflag);
PRE(sys_chflagsat)
{
   PRINT("sys_chglagsat ( %" FMT_REGWORD "d, %#" FMT_REGWORD "x, %" FMT_REGWORD "u, %" FMT_REGWORD "d )",
         SARG1, ARG2, ARG3, SARG4);
   PRE_REG_READ4(int, "chflagsat", int, fd, const char *, path, unsigned long, flags, int, atflag);
   PRE_MEM_RASCIIZ("chflagsat(path)", ARG2);
}

// SYS_accept4 541
// int accept4(int s, struct sockaddr * restrict addr,
//             socklen_t * restrict addrlen, int flags);
PRE(sys_accept4)
{
   *flags |= SfMayBlock;
   PRINT("sys_accept4 ( %" FMT_REGWORD "u, %#" FMT_REGWORD "x, %" FMT_REGWORD "u, %" FMT_REGWORD "u)",ARG1,ARG2,ARG3,ARG4);
   PRE_REG_READ4(int, "accept4",
                 int, s, struct sockaddr *, addr, int, *addrlen, int, flags);
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

// SYS_pipe2   542
// int pipe2(int fildes[2], int flags);
PRE(sys_pipe2)
{
   PRINT("sys_pipe2 ( %#" FMT_REGWORD "x, %" FMT_REGWORD "u )", ARG1, ARG2);
   PRE_REG_READ2(int, "pipe2",
                 int *, fildes, int, flags);
   PRE_MEM_WRITE("pipe2(fildes)", ARG1, 2 * sizeof(int));

}

POST(sys_pipe2)
{
   int *fildes;

   if (RES != 0) {
      return;
   }

   POST_MEM_WRITE(ARG1, 2 * sizeof(int));
   fildes = (int *)ARG1;

   if (!ML_(fd_allowed)(fildes[0], "pipe2", tid, True) ||
         !ML_(fd_allowed)(fildes[1], "pipe2", tid, True)) {
      VG_(close)(fildes[0]);
      VG_(close)(fildes[1]);
      SET_STATUS_Failure( VKI_EMFILE );
   } else if (VG_(clo_track_fds)) {
      ML_(record_fd_open_nameless)(tid, fildes[0]);
      ML_(record_fd_open_nameless)(tid, fildes[1]);
   }
}

// SYS_aio_mlock  543
// int aio_mlock(struct aiocb *iocb);
PRE(sys_aio_mlock)
{
   PRINT("sys_aio_mlock ( %#" FMT_REGWORD "x )", ARG1);
   PRE_REG_READ1(int, "aio_mlock", struct vki_aiocb *, iocb);
   PRE_MEM_READ("aio_mlock(iocb", ARG1, sizeof(struct vki_aiocb));
   // this locks memory into RAM, don't think that we need to do
   // anything extra
}

// SYS_procctl 544
// amd64 / x86

// SYS_ppoll   545
// int ppoll(struct pollfd fds[], nfds_t nfds,
//           const struct timespec * restrict timeout,
//           const sigset_t * restrict newsigmask);
PRE(sys_ppoll)
{
   PRINT("sys_ppoll ( %#" FMT_REGWORD "x, %" FMT_REGWORD "u, %#" FMT_REGWORD
         "x, %#" FMT_REGWORD "x )",
         ARG1, ARG2, ARG3, ARG4);
   UInt i;
   struct vki_pollfd* fds = (struct vki_pollfd *)(Addr)ARG1;
   *flags |= SfMayBlock | SfPostOnFail;
   PRE_REG_READ4(long, "ppoll",
                 struct vki_pollfd *, fds, unsigned int, nfds,
                 struct vki_timespec *, timeout, vki_sigset_t *, newsigmask);

   for (i = 0; i < ARG2; i++) {
      PRE_MEM_READ( "ppoll(fds.fd)",
                    (Addr)(&fds[i].fd), sizeof(fds[i].fd) );
      if (ML_(safe_to_deref)(&fds[i].fd, sizeof(fds[i].fd)) && fds[i].fd >= 0) {
         PRE_MEM_READ( "ppoll(fds.events)",
                       (Addr)(&fds[i].events), sizeof(fds[i].events) );
      }
      PRE_MEM_WRITE( "ppoll(fds.revents)",
                     (Addr)(&fds[i].revents), sizeof(fds[i].revents) );
   }

   if (ARG3) {
      PRE_MEM_READ( "ppoll(timeout)", ARG3,
                    sizeof(struct vki_timespec) );
   }
   if (ARG4) {
      PRE_MEM_READ( "ppoll(newsigmask)", ARG4, sizeof(vki_sigset_t));
      ARG4 = ML_(make_safe_mask)("syswrap.ppoll.1", (Addr)ARG4);
   }
}

POST(sys_ppoll)
{
   if (SUCCESS && ((Word)RES != -1)) {
      UInt i;
      struct vki_pollfd* ufds = (struct vki_pollfd *)(Addr)ARG1;
      for (i = 0; i < ARG2; i++) {
         POST_MEM_WRITE( (Addr)(&ufds[i].revents), sizeof(ufds[i].revents) );
      }
   }
   ML_(free_safe_mask) ( (Addr)ARG4 );
}

// SYS_futimens   546
// int futimens(int fd, const struct timespec times[2]);
PRE(sys_futimens)
{
   PRINT("sys_futimens ( %" FMT_REGWORD "d, %#" FMT_REGWORD "x )", SARG1, ARG2);
   PRE_REG_READ2(int, "futimens", int, fd, const struct timespec *, times);
   PRE_MEM_READ("futimens(times)", ARG2, 2*sizeof(struct vki_timespec));
}

// SYS_utimensat  547
// int utimensat(int fd, const char *path, const struct timespec times[2],
//               int flag);
PRE(sys_utimensat)
{
   PRINT("sys_utimensat ( %" FMT_REGWORD "d, %#" FMT_REGWORD "x, %#" FMT_REGWORD "x, %" FMT_REGWORD "d )",
         SARG1, ARG2, ARG3, SARG4);
   PRE_REG_READ4(int, "utimensat", int, fd, const char *,path, const struct timespec *, times,
                 int, flag);
   PRE_MEM_RASCIIZ("utimensat(path)", ARG2);
   PRE_MEM_READ("utimensat(times)", ARG3, 2*sizeof(struct vki_timespec));
}

// SYS_fdatasync  550
// int fdatasync(int fd);
PRE(sys_fdatasync)
{
   PRINT("sys_fdatasync ( %" FMT_REGWORD "d )",SARG1);
   PRE_REG_READ1(int, "fdatasync", int, fd);
}

#if (FREEBSD_VERS >= FREEBSD_12)
// SYS_fstat   551
// int fstat(int fd, struct stat *sb);
PRE(sys_fstat)
{
   PRINT("sys_fstat ( %" FMT_REGWORD "d, %#" FMT_REGWORD "x )",SARG1,ARG2);
   PRE_REG_READ2(int, "fstat", int, fd, struct stat *, sb);
   PRE_MEM_WRITE( "fstat(sb)", ARG2, sizeof(struct vki_stat) );
}

POST(sys_fstat)
{
   POST_MEM_WRITE( ARG2, sizeof(struct vki_stat) );
}

// SYS_fstatat 552
// int fstatat(int fd, const char *path, struct stat *sb, int flag);
PRE(sys_fstatat)
{
   PRINT("sys_fstatat ( %" FMT_REGWORD "d, %#" FMT_REGWORD "x(%s), %#" FMT_REGWORD "x, %" FMT_REGWORD "d )", SARG1,ARG2,(char*)ARG2,ARG3,SARG4);
   PRE_REG_READ4(int, "fstatat",
                 int, fd, const char *, path, struct stat *, sb, int, flag);
   PRE_MEM_RASCIIZ( "fstatat(path)", ARG2 );
   PRE_MEM_WRITE( "fstatat(sb)", ARG3, sizeof(struct vki_stat) );
}

POST(sys_fstatat)
{
   POST_MEM_WRITE( ARG3, sizeof(struct vki_stat) );
}
// SYS_fhstat  553
// int fhstat(const fhandle_t *fhp, struct stat *sb);
PRE(sys_fhstat)
{
   PRINT("sys_fhstat ( %#" FMT_REGWORD "x, %#" FMT_REGWORD "x )",ARG1,ARG2);
   PRE_REG_READ2(long, "fhstat", const vki_fhandle_t *, fhp, struct stat *, sb);
   PRE_MEM_READ( "fhstat(fhp)", ARG1, sizeof(struct vki_fhandle) );
   PRE_MEM_WRITE( "fhstat(sb)", ARG2, sizeof(struct vki_stat) );
}

POST(sys_fhstat)
{
   POST_MEM_WRITE( ARG2, sizeof(struct vki_stat) );
}

// SYS_getdirentries 554
// ssize_t getdirentries(int fd, char *buf, size_t nbytes, off_t *basep);
PRE(sys_getdirentries)
{
   *flags |= SfMayBlock;
   PRINT("sys_getdirentries ( %" FMT_REGWORD "d, %#" FMT_REGWORD "x, %" FMT_REGWORD "u, %#" FMT_REGWORD "x )", SARG1,ARG2,ARG3,ARG4);
   PRE_REG_READ4(ssize_t, "getdirentries",
                 int, fd, char *, buf,
                 size_t, nbytes,
                 off_t *, basep);
   PRE_MEM_WRITE( "getdirentries(buf)", ARG2, ARG3 );
   if (ARG4) {
      PRE_MEM_WRITE("getdirentries(basep)", ARG4, sizeof (vki_off_t));
   }
}

POST(sys_getdirentries)
{
   vg_assert(SUCCESS);
   if (RES > 0) {
      POST_MEM_WRITE( ARG2, RES );
      if ( ARG4 != 0 ) {
         POST_MEM_WRITE( ARG4, sizeof (vki_off_t));
      }
   }
}

// SYS_statfs  555
// int statfs(const char *path, struct statfs *buf);
PRE(sys_statfs)
{
   PRINT("sys_statfs ( %#" FMT_REGWORD "x(%s), %#" FMT_REGWORD "x )",ARG1,(char *)ARG1,ARG2);
   PRE_REG_READ2(int, "statfs", const char *, path, struct statfs *, buf);
   PRE_MEM_RASCIIZ( "statfs(path)", ARG1 );
   PRE_MEM_WRITE( "statfs(buf)", ARG2, sizeof(struct vki_statfs) );
}

POST(sys_statfs)
{
   POST_MEM_WRITE( ARG2, sizeof(struct vki_statfs) );
}

// SYS_fstatfs 556
// int fstatfs(int fd, struct statfs *buf);
PRE(sys_fstatfs)
{
   PRINT("sys_fstatfs ( %" FMT_REGWORD "d, %#" FMT_REGWORD "x )",SARG1,ARG2);
   PRE_REG_READ2(int, "fstatfs",
                 int, fd, struct vki_statfs *, buf);
   PRE_MEM_WRITE( "fstatfs(buf)", ARG2, sizeof(struct vki_statfs) );
}

POST(sys_fstatfs)
{
   POST_MEM_WRITE( ARG2, sizeof(struct vki_statfs) );
}

// SYS_getfsstat  557
// int getfsstat(struct statfs *buf, long bufsize, int mode);
PRE(sys_getfsstat)
{
   PRINT("sys_getfsstat ( %#" FMT_REGWORD "x, %" FMT_REGWORD "u, %" FMT_REGWORD "u )",ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "getfsstat", struct vki_statfs *, buf, long, len, int, flags);
   PRE_MEM_WRITE( "getfsstat(buf)", ARG1, ARG2 );
}

POST(sys_getfsstat)
{
   vg_assert(SUCCESS);
   if ((Word)RES != -1) {
      POST_MEM_WRITE( ARG1, RES * sizeof(struct vki_statfs) );
   }
}

// SYS_fhstatfs   558
// int fhstatfs(const fhandle_t *fhp, struct statfs *buf);
PRE(sys_fhstatfs)
{
   PRINT("sys_fhstatfs ( %#" FMT_REGWORD "x, %#" FMT_REGWORD "x )",ARG1,ARG2);
   PRE_REG_READ2(long, "fhstatfs",
                 struct fhandle *, fhp, struct statfs *, buf);
   PRE_MEM_READ( "fhstatfs(fhp)", ARG1, sizeof(struct vki_fhandle) );
   PRE_MEM_WRITE( "fhstatfs(buf)", ARG2, sizeof(struct vki_statfs) );
}

POST(sys_fhstatfs)
{
   POST_MEM_WRITE( ARG2, sizeof(struct vki_statfs) );
}

// SYS_mknodat 559
// x86 / amd64

// SYS_kevent  560
// int kevent(int kq, const struct kevent *changelist, int nchanges,
//            struct kevent *eventlist, int nevents,
//            const struct timespec *timeout);
PRE(sys_kevent)
{
   PRINT("sys_kevent ( %" FMT_REGWORD "u, %#" FMT_REGWORD "x, %" FMT_REGWORD "u, %#" FMT_REGWORD "x, %" FMT_REGWORD "u, %#" FMT_REGWORD "x )\n", ARG1,ARG2,ARG3,ARG4,ARG5,ARG6);
   PRE_REG_READ6(int, "kevent",
                 int, kq, struct vki_kevent *, changelist, int, nchanges,
                 struct vki_kevent *, eventlist, int, nevents,
                 struct timespec *, timeout);
   if (ARG2 != 0 && ARG3 != 0) {
      PRE_MEM_READ( "kevent(changelist)", ARG2, sizeof(struct vki_kevent)*ARG3 );
   }
   if (ARG4 != 0 && ARG5 != 0) {
      PRE_MEM_WRITE( "kevent(eventlist)", ARG4, sizeof(struct vki_kevent)*ARG5);
   }
   if (ARG5 != 0) {
      *flags |= SfMayBlock;
   }
   if (ARG6 != 0) {
      PRE_MEM_READ( "kevent(timeout)",
                    ARG6, sizeof(struct vki_timespec));
   }
}

POST(sys_kevent)
{
   vg_assert(SUCCESS);
   if ((Word)RES != -1) {
      if (ARG4 != 0) {
         POST_MEM_WRITE( ARG4, sizeof(struct vki_kevent)*RES) ;
      }
   }
}

// SYS_cpuset_getdomain 561
// x86 / amd64

// SYS_cpuset_setdomain 562
// x86 / amd64

// SYS_getrandom  563
// ssize_t  getrandom(void *buf, size_t buflen, unsigned int flags);
PRE(sys_getrandom)
{
   PRINT("sys_getrandom ( %#" FMT_REGWORD "x, %" FMT_REGWORD "u, %" FMT_REGWORD "u )", ARG1, ARG2, ARG3);
   PRE_REG_READ3(ssize_t, "getrandom",
                 void *, buf, vki_size_t, buflen, unsigned int, flags);
   PRE_MEM_WRITE( "getrandom(buf)", ARG1, ARG2 );
   if ((ARG3 & VKI_GRND_NONBLOCK) == 0) {
      *flags |= SfMayBlock;
   }
}

POST(sys_getrandom)
{
   POST_MEM_WRITE( ARG1, ARG2 );
}

// SYS_getfhat 564
// int getfhat(int fd, const char *path, fhandle_t *fhp, int flag);
PRE(sys_getfhat)
{
   PRINT("sys_getfhat ( %" FMT_REGWORD "d, %#" FMT_REGWORD "x, %" FMT_REGWORD "x, %" FMT_REGWORD "d ", SARG1, ARG2, ARG3, SARG4);
   PRE_REG_READ4(int, "getfhat", int, fd, const char*, path, vki_fhandle_t*, fhp, int, flag);
   PRE_MEM_RASCIIZ( "getfhat(path)", ARG2 );
   PRE_MEM_WRITE("getfhat(fhp)", ARG3, sizeof(vki_fhandle_t));
}

POST(sys_getfhat)
{
   POST_MEM_WRITE(ARG3, sizeof(vki_fhandle_t));
}

// SYS_fhlink  565
// int fhlink(fhandle_t *fhp, const char *to);
PRE(sys_fhlink)
{
   PRINT("sys_fhlink ( %#" FMT_REGWORD "x, %#" FMT_REGWORD "x )", ARG1, ARG2);
   PRE_REG_READ2(int, "fhlink", vki_fhandle_t *, fhp, const char *, to);
   PRE_MEM_READ( "fhlink(fhp)", ARG1, sizeof(vki_fhandle_t));
   PRE_MEM_RASCIIZ("fhlink(buf)", ARG2);
}

// SYS_fhlinkat   566
// int fhlinkat(fhandle_t *fhp, int tofd, const char *to);
PRE(sys_fhlinkat)
{
   PRINT("sys_fhlinkat ( %#" FMT_REGWORD "x, %" FMT_REGWORD "d, %#" FMT_REGWORD "xu ", ARG1, SARG2, ARG3);
   PRE_REG_READ3(int, "fhlinkat", vki_fhandle_t *, fhp, int, tofd, const char *, to);
   PRE_MEM_READ( "fhlinkat(fhp)", ARG1, sizeof(vki_fhandle_t));
   PRE_MEM_RASCIIZ("fhreadlink(to)", ARG3);
}

// SYS_fhreadlink 567
// int fhreadlink(fhandle_t *fhp, char *buf, size_t bufsize);
PRE(sys_fhreadlink)
{
   PRINT("sys_fhreadlink ( %#" FMT_REGWORD "x, %" FMT_REGWORD "x, %" FMT_REGWORD "u ", ARG1, ARG2, ARG3);
   PRE_REG_READ3(int, "fhreadlink", vki_fhandle_t *, fhp, char *, buf, size_t, bufsize);
   PRE_MEM_READ( "fhreadlink(fhp)", ARG1, sizeof(vki_fhandle_t));
   PRE_MEM_WRITE("fhreadlink(buf)", ARG2, ARG3);
}

POST(sys_fhreadlink)
{
   POST_MEM_WRITE(ARG2, ARG3);
}

#endif

#if (FREEBSD_VERS >= FREEBSD_12_2)

// SYS_unlinkat   568
// int funlinkat(int dfd, const char *path, int fd, int flag);
PRE(sys_funlinkat)
{
   *flags |= SfMayBlock;
   PRINT("sys_funlinkat ( %" FMT_REGWORD "d, %#" FMT_REGWORD "x(%s), %" FMT_REGWORD "u, %" FMT_REGWORD"u )",
         SARG1, ARG2, (char*)ARG2, ARG4, ARG5);
   PRE_REG_READ4(int, "funlinkat", int, dfd, const char *, path, int, fd, int, flag);
   PRE_MEM_RASCIIZ( "funlinkat(path)", ARG2 );
}

// SYS_copy_file_range 569
// ssize_t copy_file_range(int infd, off_t *inoffp, int outfd, off_t *outoffp,
//                         size_t len, unsigned int flags);
PRE(sys_copy_file_range)
{
   PRINT("sys_copy_file_range (%" FMT_REGWORD"d, %#" FMT_REGWORD "x, %" FMT_REGWORD "d, %#" FMT_REGWORD "x(%s), %" FMT_REGWORD "d, %" FMT_REGWORD "d)",
         SARG1, ARG2, SARG3, ARG4, (char*)ARG4, SARG5, SARG6);

   PRE_REG_READ6(vki_ssize_t, "copy_file_range",
                 int, "infd",
                 vki_off_t *, "inoffp",
                 int, "outfd",
                 vki_off_t *, "outoffp",
                 vki_size_t, "len",
                 unsigned int, "flags");

   /* File descriptors are "specially" tracked by valgrind.
      valgrind itself uses some, so make sure someone didn't
      put in one of our own...  */
   if (!ML_(fd_allowed)(ARG1, "copy_file_range(infd)", tid, False) ||
       !ML_(fd_allowed)(ARG3, "copy_file_range(infd)", tid, False)) {
      SET_STATUS_Failure( VKI_EBADF );
   } else {
      /* Now see if the offsets are defined. PRE_MEM_READ will
         double check it can dereference them. */
      if (ARG2 != 0) {
         PRE_MEM_READ( "copy_file_range(inoffp)", ARG2, sizeof(vki_off_t));
      }
      if (ARG4 != 0) {
         PRE_MEM_READ( "copy_file_range(outoffp)", ARG4, sizeof(vki_off_t));
      }
   }
}


// SYS___sysctlbyname 570
// int sysctlbyname(const char *name, void *oldp, size_t *oldlenp,
//                  const void *newp, size_t newlen);
// syscalls.master:
// int __sysctlbyname(_In_reads_(namelen) const char *name, size_t namelen,
//                    _Out_writes_bytes_opt_(*oldlenp) void *old,
//                    _Inout_opt_ size_t *oldlenp, _In_reads_bytes_opt_(newlen) void *new,
//                    size_t newlen );
PRE(sys___sysctlbyname)
{
   // this is very much like SYS___sysctl, instead of having an OID with length
   // here threre is an ascii string with length
   // @todo PJF factor out the common functionality of the two
   PRINT("sys___sysctlbyname ( %#" FMT_REGWORD "x(%s), %#" FMT_REGWORD "x, %#" FMT_REGWORD "x, %#" FMT_REGWORD "x, %" FMT_REGWORD "u )", ARG1,(const char*)ARG1,ARG2,ARG3,ARG4,ARG5 );
   PRE_REG_READ6(int, "__sysctlbyname", const char *, name, vki_size_t, namelen,
                 void *, oldp, vki_size_t *, oldlenp,
                 void *, newp, vki_size_t, newlen);


   const char* name = (const char*)ARG1;
   if (ML_(safe_to_deref)(name, sizeof("kern.ps_strings")) &&
       VG_(strcmp)(name, "kern.ps_strings") == 0) {
      if (sysctl_kern_ps_strings((SizeT*)ARG3, (SizeT*)ARG4)) {
         SET_STATUS_Success(0);
      }
   }

   if (ML_(safe_to_deref)(name, sizeof("kern.usrstack")) &&
      VG_(strcmp)(name, "kern.usrstack") == 0) {
      sysctl_kern_usrstack((SizeT*)ARG3, (SizeT*)ARG4);
      SET_STATUS_Success(0);
   }

   // kern.proc.pathname doesn't seem to be handled
   // makes sense as the pid is variable and using
   // a MIB is easier than generating a string

   // read number of ints specified in ARG2 from mem pointed to by ARG1
   PRE_MEM_READ("__sysctlbyname(name)", (Addr)ARG1, ARG2 * sizeof(int));

   // if 'newp' is not NULL can read namelen bytes from that addess
   if (ARG5 != (UWord)NULL) {
      PRE_MEM_READ("__sysctlbyname(newp)", (Addr)ARG5, ARG6);
   }

   // there are two scenarios for oldlenp/oldp
   // 1. oldval is NULL and oldlenp is non-NULL
   //    this is a query of oldlenp so oldlenp will be written
   // 2. Both are non-NULL
   //    this is a query of oldp, oldlenp will be read and oldp will
   //    be written

   // is oldlenp is not NULL, can write
   if (ARG4 != (UWord)NULL) {
      if (ARG3 != (UWord)NULL) {
         // case 2 above
         PRE_MEM_READ("__sysctlbyname(oldlenp)", (Addr)ARG4, sizeof(vki_size_t));
         if (ML_(safe_to_deref)((void*)(Addr)ARG4, sizeof(vki_size_t))) {
            PRE_MEM_WRITE("__sysctlbyname(oldp)", (Addr)ARG3, *(vki_size_t *)ARG4);
         } else {
            VG_(dmsg)("Warning: Bad oldlenp address %p in sysctlbyname\n",
                      (void *)(Addr)ARG4);
            SET_STATUS_Failure ( VKI_EFAULT );
         }
      } else {
         // case 1 above
         PRE_MEM_WRITE("__sysctlbyname(oldlenp)", (Addr)ARG4, sizeof(vki_size_t));
      }
   }
}

POST(sys___sysctlbyname)
{
   if (ARG4 != (UWord)NULL) {
      if (ARG3 != (UWord)NULL) {
         //POST_MEM_WRITE((Addr)ARG4, sizeof(vki_size_t));
         POST_MEM_WRITE((Addr)ARG3, *(vki_size_t *)ARG4);
      } else {
         POST_MEM_WRITE((Addr)ARG4, sizeof(vki_size_t));
      }
   }
}

#endif // (FREEBSD_VERS >= FREEBSD_12_2)

#if (FREEBSD_VERS >= FREEBSD_13_0)

// SYS_shm_open2   571
// from syscalls.master
// int shm_open2(_In_z_ const char *path,
//               int flags,
//               mode_t mode,
//               int shmflags,
//               _In_z_ const char *name);
PRE(sys_shm_open2)
{
   PRE_REG_READ5(int, "shm_open2",
                 const char *, path, int, flags, vki_mode_t, mode, int, shmflags, const char*, name);
   if (ARG1 == VKI_SHM_ANON) {
      PRINT("sys_shm_open2(%#" FMT_REGWORD "x(SHM_ANON), %" FMT_REGWORD "u, %hu, %d, %#" FMT_REGWORD "x(%s))",
            ARG1, ARG2, (vki_mode_t)ARG3, (Int)ARG4, ARG5, (HChar*)ARG5);
   } else {
      PRINT("sys_shm_open2(%#" FMT_REGWORD "x(%s), %" FMT_REGWORD "u, %hu, %d, %#" FMT_REGWORD "x(%s))",
            ARG1, (HChar *)ARG1, ARG2, (vki_mode_t)ARG3, (Int)ARG4, ARG5, (HChar*)ARG5);
      PRE_MEM_RASCIIZ( "shm_open2(path)", ARG1 );
   }

   if (ARG5) {
      PRE_MEM_RASCIIZ( "shm_open2(name)", ARG5 );
   }
   *flags |= SfMayBlock;
}

POST(sys_shm_open2)
{
   vg_assert(SUCCESS);
   if (!ML_(fd_allowed)(RES, "shm_open2", tid, True)) {
      VG_(close)(RES);
      SET_STATUS_Failure( VKI_EMFILE );
   } else {
      if (VG_(clo_track_fds)) {
         ML_(record_fd_open_with_given_name)(tid, RES, (HChar*)ARG1);
      }
   }
}

// SYS_sigfastblock 573
// int sigfastblock(int cmd, void *ptr);
PRE(sys_sigfastblock)
{
   PRINT("sys_sigfastblock ( %" FMT_REGWORD "d, %#" FMT_REGWORD "x )", SARG1, ARG2);
   PRE_REG_READ2(int, "sigfasblock", int, cmd, void*, ptr);
}

// SYS___realpathat 574
// from syscalls.master
//         int __realpathat(int fd,
//         _In_z_ const char *path,
//         _Out_writes_z_(size) char *buf,
//         size_t size,
//         int flags)
PRE(sys___realpathat)
{
   PRINT("sys___realpathat ( %" FMT_REGWORD "d, %#" FMT_REGWORD "x(%s), %#" FMT_REGWORD "x, %" FMT_REGWORD "u %" FMT_REGWORD "d )",
         SARG1,ARG2,(const char*)ARG2,ARG3,ARG4,SARG5 );
   PRE_REG_READ5(int, "__realpathat", int, fd, const char *, path,
                 char *, buf, vki_size_t, size, int, flags);
   PRE_MEM_RASCIIZ("__realpathat(path)", (Addr)ARG2);
   PRE_MEM_WRITE("__realpathat(buf)", (Addr)ARG3, ARG4);
}

POST(sys___realpathat)
{
   POST_MEM_WRITE((Addr)ARG3, ARG4);
}

#endif

#if (FREEBSD_VERS >= FREEBSD_12_2)

// SYS_sys_close_range   575
// int close_range(close_range(u_int lowfd, u_int highfd, int flags);
PRE(sys_close_range)
{
   SysRes res = VG_(mk_SysRes_Success)(0);
   unsigned int lowfd = ARG1;
   unsigned int fd_counter; // will count from lowfd to highfd
   unsigned int highfd = ARG2;

   /* on linux the may lock if futexes are used
    * there is a lock in the kernel but I assume it's just
    * a spinlock */
   PRINT("sys_close_range ( %" FMT_REGWORD "u, %" FMT_REGWORD "u, %"
         FMT_REGWORD "d )", ARG1, ARG2, SARG3);
   PRE_REG_READ3(int, "close_range",
                 unsigned int, lowfd, unsigned int, highfd,
                 int, flags);

   if (lowfd > highfd) {
      SET_STATUS_Failure( VKI_EINVAL );
      return;
   }

   if (highfd >= VG_(fd_hard_limit))
      highfd = VG_(fd_hard_limit) - 1;

   if (lowfd > highfd) {
      SET_STATUS_Success ( 0 );
      return;
   }

   fd_counter = lowfd;
   do {
      if (fd_counter > highfd
          || (fd_counter == 2U/*stderr*/ && VG_(debugLog_getLevel)() > 0)
          || fd_counter == VG_(log_output_sink).fd
          || fd_counter == VG_(xml_output_sink).fd) {
         /* Split the range if it contains a file descriptor we're not
          * supposed to close. */
         if (fd_counter - 1 >= lowfd) {
            res = VG_(do_syscall3)(__NR_close_range, (UWord)lowfd, (UWord)fd_counter - 1, ARG3 );
         }
         lowfd = fd_counter + 1;
      }
   } while (fd_counter++ <= highfd);

   /* If it failed along the way, it's presumably the flags being wrong. */
   SET_STATUS_from_SysRes (res);
}

POST(sys_close_range)
{
   unsigned int fd;
   unsigned int last = ARG2;

   if (!VG_(clo_track_fds)
       || (ARG3 & VKI_CLOSE_RANGE_CLOEXEC) != 0)
      return;

   if (last >= VG_(fd_hard_limit))
      last = VG_(fd_hard_limit) - 1;

   for (fd = ARG1; fd <= last; fd++)
      if ((fd != 2/*stderr*/ || VG_(debugLog_getLevel)() == 0)
          && fd != VG_(log_output_sink).fd
          && fd != VG_(xml_output_sink).fd)
         ML_(record_fd_close)(fd);
}
#endif

#if (FREEBSD_VERS >= FREEBSD_13_0)

// SYS___specialfd 577
// syscalls.master
// int __specialfd(int type,
//                _In_reads_bytes_(len) const void *req,
//                 size_t len);
PRE(sys___specialfd)
{
   PRINT("sys___specialfd ( %" FMT_REGWORD "d, %#" FMT_REGWORD "x(%s), %" FMT_REGWORD "u )",
         SARG1,ARG2,(const char*)ARG2,ARG3 );
   PRE_REG_READ3(int, "__specialfd", int, type, const void *, req, vki_size_t, len);
   PRE_MEM_READ("__specialfd(req)", (Addr)ARG2, ARG3);
}

// SYS_aio_writev 578
// int aio_writev(struct aiocb *iocb);
PRE(sys_aio_writev)
{
   PRINT("sys_aio_writev ( %#" FMT_REGWORD "x )", ARG1);
   PRE_REG_READ1(int, "aio_writev", struct vki_aiocb *, iocb);
   PRE_MEM_READ("aio_writev(iocb)", ARG1, sizeof(struct vki_aiocb));
   if (ML_(safe_to_deref)((struct vki_aiocb *)ARG1, sizeof(struct vki_aiocb))) {
      struct vki_aiocb *iocb = (struct vki_aiocb *)ARG1;
      if (!ML_(fd_allowed)(iocb->aio_fildes, "aio_writev", tid, False)) {
         SET_STATUS_Failure( VKI_EBADF );
      } else {
         // aio_writev() gathers the data from the iocb->aio_iovcnt buffers specified
         // by the members of the iocb->aio_iov array
         // FreeBSD headers #define define this to aio_iovcnt
         SizeT vec_count = (SizeT)iocb->aio_nbytes;
#if defined(__clang__)
#pragma clang diagnostic push
         // yes, I know it is volatile
#pragma clang diagnostic ignored "-Wcast-qual"
#endif
         struct vki_iovec* p_iovec  = (struct vki_iovec*)iocb->aio_buf;
#if defined(__clang__)
#pragma clang diagnostic pop
#endif
         PRE_MEM_READ("aio_writev(iocb->aio_iov)", (Addr)p_iovec, vec_count*sizeof(struct vki_iovec));
         // and this to aio_iov

         if (ML_(safe_to_deref)(p_iovec, vec_count*sizeof(struct vki_iovec))) {
            for (SizeT i = 0U; i < vec_count; ++i) {
               PRE_MEM_READ("aio_writev(iocb->iov[...])",
                            (Addr)p_iovec[i].iov_base, p_iovec[i].iov_len);
            }
         }
      }
   } else {
      SET_STATUS_Failure(VKI_EINVAL);
   }
}

// SYS_aio_readv 579
// int aio_readv(struct aiocb *iocb);
PRE(sys_aio_readv)
{
   PRINT("sys_aio_readv ( %#" FMT_REGWORD "x )", ARG1);
   PRE_REG_READ1(int, "aio_readv", struct vki_aiocb *, iocb);
   PRE_MEM_READ("aio_readv(iocb)", ARG1, sizeof(struct vki_aiocb));
   if (ML_(safe_to_deref)((struct vki_aiocb *)ARG1, sizeof(struct vki_aiocb))) {
      struct vki_aiocb *iocb = (struct vki_aiocb *)ARG1;
      if (!ML_(fd_allowed)(iocb->aio_fildes, "aio_readv", tid, False)) {
         SET_STATUS_Failure( VKI_EBADF );
      } else {
         SizeT vec_count = (SizeT)iocb->aio_nbytes;
#if defined(__clang__)
#pragma clang diagnostic push
         // yes, I know it is volatile
#pragma clang diagnostic ignored "-Wcast-qual"
#endif
         struct vki_iovec* p_iovec  = (struct vki_iovec*)iocb->aio_buf;
#if defined(__clang__)
#pragma clang diagnostic pop
#endif
         PRE_MEM_READ("aio_readv(iocb->aio_iov)", (Addr)p_iovec,  vec_count*sizeof(struct vki_iovec));
         if (ML_(safe_to_deref)(p_iovec, vec_count*sizeof(struct vki_iovec))) {
            for (SizeT i = 0U; i < vec_count; ++i) {
               PRE_MEM_WRITE("aio_writev(iocb->aio_iov[...])",
                            (Addr)p_iovec[i].iov_base, p_iovec[i].iov_len);
            }
         }
      }
   } else {
      SET_STATUS_Failure(VKI_EINVAL);
   }
}

POST(sys_aio_readv)
{
   struct vki_aiocb* iocbv = (struct vki_aiocb*)ARG1;
   if (iocbv->aio_buf) {
      if (!aiov_init_done) {
         aiov_init();
      }

      if (!VG_(OSetWord_Contains)(iocbv_table, (UWord)iocbv)) {
         VG_(OSetWord_Insert)(iocbv_table, (UWord)iocbv);
      } else {
         // @todo PJF this warns without callstack
         VG_(dmsg)("Warning: Duplicate control block %p in aio_readv\n",
                   (void *)(Addr)ARG1);
         VG_(dmsg)("Warning: Ensure 'aio_return' is called when 'aio_readv' has completed\n");
      }
   }
}

#endif // (FREEBSD_VERS >= FREEBSD_13_0)

#if (FREEBSD_VERS >= FREEBSD_13_1)

#if (FREEBSD_VERS >= FREEBSD_14)
// SYS_fspacectl 580
// int fspacectl(int fd, int cmd, const struct spacectl_range *rqsr, int flags,
//     struct spacectl_range *rmsr);
PRE(sys_fspacectl)
{
   PRINT("fspacectl ( %" FMT_REGWORD "d, %" FMT_REGWORD "d, %#" FMT_REGWORD "x, %" FMT_REGWORD "d, %#" FMT_REGWORD "x )", SARG1, SARG2, ARG3, SARG4, ARG5);
   PRE_REG_READ5(int, "fspacectl", int, fd, int, cmd, const struct spacectl_range *, rqsr, int, flags, struct spacectl_range *, rmsr);
   PRE_MEM_READ("fspacectl(rqsr)", (Addr)ARG3, sizeof(struct vki_spacectl_range));
   if (ARG5) {
      PRE_MEM_WRITE("fspacectl(rmsr)", (Addr)ARG5, sizeof(struct vki_spacectl_range));
   }
}

POST(sys_fspacectl)
{
   if (ARG5) {
      POST_MEM_WRITE((Addr)ARG5, sizeof(struct vki_spacectl_range));
   }
}
#endif

// SYS_swapoff 582
// int swapoff(const char *special, u_int flags);
PRE(sys_swapoff)
{
   PRINT("sys_swapoff(%#" FMT_REGWORD "x(%s), %" FMT_REGWORD "u)", ARG1,(char *)ARG1, ARG2);
   PRE_REG_READ2(int, "swapoff", const char *, special, u_int, flags);
   PRE_MEM_RASCIIZ( "swapoff(special)", ARG1 );
}

#endif

#if (FREEBSD_VERS >= FREEBSD_15) || (FREEBSD_VERS >= FREEBSD_13_3)

// SYS_kqueuex 583
// int kqueuex(u_int flags);
PRE(sys_kqueuex)
{
   PRINT("sys_kqueuex(%#" FMT_REGWORD "x)", ARG1);
   PRE_REG_READ1(int, "kqueuex", u_int, flags);
}

POST(sys_kqueuex)
{
   if (!ML_(fd_allowed)(RES, "kqueuex", tid, True)) {
      VG_(close)(RES);
      SET_STATUS_Failure(VKI_EMFILE);
   } else {
      if (VG_(clo_track_fds)) {
         ML_(record_fd_open_nameless)(tid, RES);
      }
   }
}

// SYS_membarrier 584
// syscalls.master
// int membarrier(int cmd, unsigned flags, int cpu_id);
PRE(sys_membarrier)
{
   // cmd is signed int but the constants in the headers
   // are hex so print in hex
   PRINT("sys_membarrier(%#" FMT_REGWORD "x, %#" FMT_REGWORD "x, %" FMT_REGWORD "d)",
         ARG1, ARG2, SARG3);
   PRE_REG_READ3(int, "membarrier", int, cmd, unsigned, flags, int, cpu_id);
}

#endif

#if (FREEBSD_VERS >= FREEBSD_15)

// SYS_timerfd_create 585
// int timerfd_create(int clockid, int flags);
PRE(sys_timerfd_create)
{
   PRINT("sys_timerfd_create (%ld, %ld )", SARG1, SARG2);
   PRE_REG_READ2(int, "timerfd_create", int, clockid, int, flags);
}

POST(sys_timerfd_create)
{
   if (!ML_(fd_allowed)(RES, "timerfd_create", tid, True)) {
      VG_(close)(RES);
      SET_STATUS_Failure( VKI_EMFILE );
   } else {
      if (VG_(clo_track_fds))
         ML_(record_fd_open_nameless) (tid, RES);
   }
}

// SYS_timerfd_gettime 586
// int timerfd_gettime(int fd, struct itimerspec *curr_value);
PRE(sys_timerfd_gettime)
{
   PRINT("sys_timerfd_gettime ( %ld, %#" FMT_REGWORD "x )", SARG1, ARG2);
   PRE_REG_READ2(int, "timerfd_gettime",
                 int, fd,
                 struct vki_itimerspec*, curr_value);
   if (!ML_(fd_allowed)(ARG1, "timerfd_gettime", tid, False))
      SET_STATUS_Failure(VKI_EBADF);
   else
      PRE_MEM_WRITE("timerfd_gettime(curr_value)",
                    ARG2, sizeof(struct vki_itimerspec));
}

POST(sys_timerfd_gettime)
{
   if (RES == 0)
      POST_MEM_WRITE(ARG2, sizeof(struct vki_itimerspec));
}

// SYS_timerfd_gettime 587
// int timerfd_settime(int fd, int flags, const struct itimerspec *new_value,
//                     struct itimerspec *old_value);
PRE(sys_timerfd_settime)
{
   PRINT("sys_timerfd_settime(%" FMT_REGWORD "d, %" FMT_REGWORD "d, %#" FMT_REGWORD "x, %#"
         FMT_REGWORD "x )", SARG1, SARG2, ARG3, ARG4);
   PRE_REG_READ4(int, "timerfd_settime",
                 int, fd,
                 int, flags,
                 const struct vki_itimerspec*, new_value,
                 struct vki_itimerspec*, old_value);
   if (!ML_(fd_allowed)(ARG1, "timerfd_settime", tid, False))
      SET_STATUS_Failure(VKI_EBADF);
   else
   {
      PRE_MEM_READ("timerfd_settime(new_value)",
                   ARG3, sizeof(struct vki_itimerspec));
      if (ARG4)
      {
         PRE_MEM_WRITE("timerfd_settime(old_value)",
                       ARG4, sizeof(struct vki_itimerspec));
      }
   }
}

POST(sys_timerfd_settime)
{
   if (RES == 0 && ARG4 != 0) {
      POST_MEM_WRITE(ARG4, sizeof(struct vki_itimerspec));
   }
}
#endif

#undef PRE
#undef POST

const SyscallTableEntry ML_(syscall_table)[] = {
   // syscall (handled specially)                       // 0
   BSDX_(__NR_exit,             sys_exit),              // 1
   BSDX_(__NR_fork,             sys_fork),              // 2
   GENXY(__NR_read,             sys_read),              // 3

   GENX_(__NR_write,            sys_write),             // 4
   GENXY(__NR_open,             sys_open),              // 5
   GENXY(__NR_close,            sys_close),             // 6
   GENXY(__NR_wait4,            sys_wait4),             // 7

   // 4.3 creat                                            8
   GENX_(__NR_link,             sys_link),              // 9
   GENX_(__NR_unlink,           sys_unlink),            // 10
   // obsol execv                                          11

   GENX_(__NR_chdir,            sys_chdir),             // 12
   GENX_(__NR_fchdir,           sys_fchdir),            // 13
   GENX_(__NR_freebsd11_mknod,  sys_mknod),             // 14
   GENX_(__NR_chmod,            sys_chmod),             // 15

   GENX_(__NR_chown,            sys_chown),             // 16
   GENX_(__NR_break,            sys_brk),               // 17
   // freebsd 4 getfsstat                                  18
   // 4.3 lseek                                            19

   GENX_(__NR_getpid,           sys_getpid),            // 20
   BSDX_(__NR_mount,            sys_mount),             // 21
   BSDX_(__NR_unmount,          sys_unmount),           // 22
   GENX_(__NR_setuid,           sys_setuid),            // 23

   GENX_(__NR_getuid,           sys_getuid),            // 24
   GENX_(__NR_geteuid,          sys_geteuid),           // 25
   BSDXY(__NR_ptrace,           sys_ptrace),            // 26
   BSDXY(__NR_recvmsg,          sys_recvmsg),           // 27

   BSDX_(__NR_sendmsg,          sys_sendmsg),           // 28
   BSDXY(__NR_recvfrom,         sys_recvfrom),          // 29
   BSDXY(__NR_accept,           sys_accept),            // 30
   BSDXY(__NR_getpeername,      sys_getpeername),       // 31

   BSDXY(__NR_getsockname,      sys_getsockname),       // 32
   GENX_(__NR_access,           sys_access),            // 33
   BSDX_(__NR_chflags,          sys_chflags),           // 34
   BSDX_(__NR_fchflags,         sys_fchflags),          // 35

   GENX_(__NR_sync,             sys_sync),              // 36
   GENX_(__NR_kill,             sys_kill),              // 37
   // 4.3 stat                                             38
   GENX_(__NR_getppid,          sys_getppid),           // 39

   // 4.3 lstat                                            40
   GENXY(__NR_dup,              sys_dup),               // 41

   BSDXY(__NR_freebsd10_pipe,   sys_pipe),              // 42
   GENX_(__NR_getegid,          sys_getegid),           // 43

   GENX_(__NR_profil,           sys_ni_syscall),        // 44
   GENX_(__NR_ktrace,           sys_ni_syscall),        // 45
   // 4.3 sigaction                                        46
   GENX_(__NR_getgid,           sys_getgid),            // 47

   // 4.3 sigaction (int sigset)                           48
   BSDXY(__NR_getlogin,         sys_getlogin),          // 49
   BSDX_(__NR_setlogin,         sys_setlogin),          // 50
   GENX_(__NR_acct,             sys_acct),              // 51

   // 4.3 sigpending                                       52
   GENXY(__NR_sigaltstack,      sys_sigaltstack),       // 53
   BSDXY(__NR_ioctl,            sys_ioctl),             // 54
   BSDX_(__NR_reboot,           sys_reboot),            // 55

   BSDX_(__NR_revoke,           sys_revoke),            // 56
   GENX_(__NR_symlink,          sys_symlink),           // 57
   BSDX_(__NR_readlink,         sys_readlink),          // 58
   GENX_(__NR_execve,           sys_execve),            // 59

   GENX_(__NR_umask,            sys_umask),             // 60
   GENX_(__NR_chroot,           sys_chroot),            // 61
   // 4.3 fstat                                            62
   // 4.3 getgerninfo                                      63

   // 4.3 getpagesize                                      64
   GENX_(__NR_msync,            sys_msync),             // 65
   BSDX_(__NR_vfork,            sys_vfork),             // 66
   // obsol vread                                          67

   // obsol vwrite                                         68
   BSDX_(__NR_sbrk,             sys_sbrk),              // 69
   // not implemented in OS sstk                           70
   // 4.3 mmap                                             71

   // freebsd11 vadvise                                    72
   GENXY(__NR_munmap,           sys_munmap),            // 73
   GENXY(__NR_mprotect,         sys_mprotect),          // 74
   GENX_(__NR_madvise,          sys_madvise),           // 75

   // obsol vhangup                                        76
   // obsol vlimit                                         77
   GENXY(__NR_mincore,          sys_mincore),           // 78
   GENXY(__NR_getgroups,        sys_getgroups),         // 79

   GENX_(__NR_setgroups,        sys_setgroups),         // 80
   GENX_(__NR_getpgrp,          sys_getpgrp),           // 81
   GENX_(__NR_setpgid,          sys_setpgid),           // 82
   GENXY(__NR_setitimer,        sys_setitimer),         // 83

   // 4.3 wait                                             84
   BSDX_(__NR_swapon,           sys_swapon),            // 85
   GENXY(__NR_getitimer,        sys_getitimer),         // 86
   // 4.3 gethostname                                      87

   // 4.3 sethostname                                      88
   BSDX_(__NR_getdtablesize,    sys_getdtablesize),     // 89
   GENXY(__NR_dup2,             sys_dup2),              // 90

   BSDXY(__NR_fcntl,            sys_fcntl),             // 92
   GENX_(__NR_select,           sys_select),            // 93
   GENX_(__NR_fsync,            sys_fsync),             // 95

   GENX_(__NR_setpriority,      sys_setpriority),       // 96
   BSDXY(__NR_socket,           sys_socket),            // 97
   BSDX_(__NR_connect,          sys_connect),           // 98
   // 4.3 accept                                           99

   GENX_(__NR_getpriority,      sys_getpriority),       // 100
   // 4.3 send                                             101
   // 4.3 recv                                             102
   // 4.3 sigreturn                                        103

   BSDX_(__NR_bind,             sys_bind),              // 104
   BSDX_(__NR_setsockopt,       sys_setsockopt),        // 105
   BSDX_(__NR_listen,           sys_listen),            // 106
   // obsol vtimes                                         107

   // 4.3 sigvec                                           108
   // 4.3 sigblock                                         109
   // 4.3 sigsetmask                                       110
   // 4.3 sigsuspend                                       111

   // 4.3 sigstack                                         112
   // 4.3 recvmsg                                          113
   // 4.3 sendmsg                                          114
   // 4.3 vtrace                                           115

   GENXY(__NR_gettimeofday,     sys_gettimeofday),      // 116
   GENXY(__NR_getrusage,        sys_getrusage),         // 117
   BSDXY(__NR_getsockopt,       sys_getsockopt),        // 118

   GENXY(__NR_readv,            sys_readv),             // 120
   GENX_(__NR_writev,           sys_writev),            // 121
   GENX_(__NR_settimeofday,     sys_settimeofday),      // 122
   GENX_(__NR_fchown,           sys_fchown),            // 123

   GENX_(__NR_fchmod,           sys_fchmod),            // 124
   // 4.3 recvfrom                                         125
   GENX_(__NR_setreuid,         sys_setreuid),          // 126
   GENX_(__NR_setregid,         sys_setregid),          // 127

   GENX_(__NR_rename,           sys_rename),            // 128
   // 4.3 truncate                                         129
   // 4.3 ftruncate                                        130
   GENX_(__NR_flock,            sys_flock),             // 131

   BSDX_(__NR_mkfifo,           sys_mkfifo),            // 132
   BSDX_(__NR_sendto,           sys_sendto),            // 133
   BSDX_(__NR_shutdown,         sys_shutdown),          // 134
   BSDXY(__NR_socketpair,       sys_socketpair),        // 135

   GENX_(__NR_mkdir,            sys_mkdir),             // 136
   GENX_(__NR_rmdir,            sys_rmdir),             // 137
   GENX_(__NR_utimes,           sys_utimes),            // 138
   // 4.2 sigreturn                                        139

   BSDXY(__NR_adjtime,          sys_adjtime),           // 140
   // 4.3 getpeername                                      141
   // 4.3 gethostid                                        142
   // 4.3 sethostid                                        143

   // 4.3 getrlimit`                                       144
   // 4.3 setrlimit                                        145
   // 4.3 killpg                                           146
   GENX_(__NR_setsid,           sys_setsid),            // 147

   BSDX_(__NR_quotactl,         sys_quotactl),          // 148
   // 4.3 quota                                            149
   // 4.3 getsockname                                      150
   // bsd/os sem_lock                                      151

   // bsd/os sem_wakeup                                    152
   // bsd/os asyncdaemon                                   153

   // no idea what the following syscall does
   // unimp SYS_nlm_syscall                                154

   // a somewhat complicated NFS API
   // takes a flag and a void* that can point to one of
   // three different types of struct depending on the flag
   // unimp SYS_nfssvc                                     155

   // 4.3 getdirentries                                    156
   // freebsd 4 statfs                                     157
   // freebsd 4 fstatfs                                    158

   BSDXY(__NR_lgetfh,           sys_lgetfh),            // 160
   BSDXY(__NR_getfh,            sys_getfh),             // 161
#if (FREEBSD_VERS <= FREEBSD_10)
   BSDXY(__NR_freebsd4_getdomainname, sys_freebsd4_getdomainname), // 162
   BSDX_(__NR_freebsd4_setdomainname, sys_freebsd4_setdomainname), // 163
   BSDXY(__NR_freebsd4_uname,   sys_freebsd4_uname),    // 164
#endif
   BSDXY(__NR_sysarch,          sys_sysarch),           // 165
   BSDXY(__NR_rtprio,           sys_rtprio),            // 166

   // the following 3 seem only to be defines in a header
   // semsys                                               169
   // msgsys                                               170
   // shmsys                                               171

#if (FREEBSD_VERS <= FREEBSD_10)
   BSDXY(__NR_freebsd6_pread,   sys_freebsd6_pread),    // 173
   BSDX_(__NR_freebsd6_pwrite,  sys_freebsd6_pwrite),   // 174
#endif
   BSDX_(__NR_setfib,           sys_setfib),            // 175

   // @todo PJF this exists on Darwin and Solaris as well
   // and it isn't implememented on either
   // looking at the manpage there is a rather fearsome
   // timex struct with a mixture of ro and rw fields
   // BSDXY(__NR_ntp_adjtime,   sys_ntp_adjtime),       // 176

   // bsd/os sfork                                         177
   // bsd/os getdescriptor                                 178
   // bsd/os setdescriptor                                 179

   GENX_(__NR_setgid,           sys_setgid),            // 181
   BSDX_(__NR_setegid,          sys_setegid),           // 182
   BSDX_(__NR_seteuid,          sys_seteuid),           // 183

   // obs lfs_bmapv                                        184
   // obs lfs_markv                                        185
   // obs lfs_segclean                                     186
   // obs lfs_segwait                                      187

#if (FREEBSD_VERS >= FREEBSD_12)
   BSDXY(__NR_freebsd11_stat,   sys_freebsd11_stat),    // 188
   BSDXY(__NR_freebsd11_fstat,  sys_freebsd11_fstat),   // 189
   BSDXY(__NR_freebsd11_lstat,  sys_freebsd11_lstat),   // 190
#else
   BSDXY(__NR_stat,             sys_stat),              // 188
   BSDXY(__NR_fstat,            sys_fstat),             // 189
   BSDXY(__NR_lstat,            sys_lstat),             // 190
#endif
   BSDX_(__NR_pathconf,         sys_pathconf),          // 191
   BSDX_(__NR_fpathconf,        sys_fpathconf),         // 192
   GENXY(__NR_getrlimit,        sys_getrlimit),         // 194
   GENX_(__NR_setrlimit,        sys_setrlimit),         // 195
#if (FREEBSD_VERS >= FREEBSD_12)
   BSDXY(__NR_freebsd11_getdirentries, sys_freebsd11_getdirentries), // 196
#else
   BSDXY(__NR_getdirentries,    sys_getdirentries),     // 196
#endif
#if (FREEBSD_VERS <= FREEBSD_10)
   BSDX_(__NR_freebsd6_mmap,    sys_freebsd6_mmap),     // 197
#endif
   // __syscall (handled specially)                     // 198
#if (FREEBSD_VERS <= FREEBSD_10)
   BSDX_(__NR_freebsd6_lseek,   sys_freebsd6_lseek),   // 199
   BSDX_(__NR_freebsd6_truncate, sys_freebsd6_truncate), // 200
   BSDX_(__NR_freebsd6_ftruncate, sys_freebsd6_ftruncate), // 201
#endif
   BSDXY(__NR___sysctl,         sys___sysctl),          // 202
   GENX_(__NR_mlock,            sys_mlock),             // 203

   GENX_(__NR_munlock,          sys_munlock),           // 204
   BSDX_(__NR_undelete,         sys_undelete),          // 205
   BSDX_(__NR_futimes,          sys_futimes),           // 206
   GENX_(__NR_getpgid,          sys_getpgid),           // 207

   // netbsd newreboot                                     208
   GENXY(__NR_poll,             sys_poll),              // 209

   BSDXY(__NR_freebsd7___semctl, sys_freebsd7___semctl), // 220
   BSDX_(__NR_semget,           sys_semget),            // 221
   BSDX_(__NR_semop,            sys_semop),             // 222
   // obs semconfig                                        223

   BSDXY(__NR_freebsd7_msgctl,  sys_freebsd7_msgctl),   // 224
   BSDX_(__NR_msgget,           sys_msgget),            // 225
   BSDX_(__NR_msgsnd,           sys_msgsnd),            // 226
   BSDXY(__NR_msgrcv,           sys_msgrcv),            // 227

   BSDXY(__NR_shmat,            sys_shmat),             // 228
   BSDXY(__NR_freebsd7_shmctl,  sys_freebsd7_shmctl),   // 229
   BSDXY(__NR_shmdt,            sys_shmdt),             // 230
   BSDX_(__NR_shmget,           sys_shmget),            // 231

   BSDXY(__NR_clock_gettime,    sys_clock_gettime),     // 232
   BSDX_(__NR_clock_settime,    sys_clock_settime),     // 233
   BSDXY(__NR_clock_getres,     sys_clock_getres),      // 234
   BSDXY(__NR_ktimer_create,    sys_timer_create),      // 235
   BSDX_(__NR_ktimer_delete,    sys_timer_delete),      // 236
   BSDXY(__NR_ktimer_settime,   sys_timer_settime),     // 237
   BSDXY(__NR_ktimer_gettime,   sys_timer_gettime),     // 238
   BSDX_(__NR_ktimer_getoverrun, sys_timer_getoverrun), // 239

   GENXY(__NR_nanosleep,        sys_nanosleep),         // 240
   // unimpl SYS_ffclock_getcounter                        241
   // unimpl SYS_ffclock_setestimate                       242
   // unimpl SYS_ffclock_getestimate                       243

   BSDXY(__NR_clock_nanosleep,  sys_clock_nanosleep),   // 244
   BSDXY(__NR_clock_getcpuclockid2, sys_clock_getcpuclockid2), // 247

   // unimpl SYS_ntp_gettime                               248
   BSDXY(__NR_minherit,         sys_minherit),          // 250
   BSDX_(__NR_rfork,            sys_rfork),             // 251

   // openbsd_poll                                      // 252
   BSDX_(__NR_issetugid,        sys_issetugid),         // 253
   GENX_(__NR_lchown,           sys_lchown),            // 254
   BSDXY(__NR_aio_read,         sys_aio_read),          // 255
   BSDX_(__NR_aio_write,        sys_aio_write),         // 256
   BSDX_(__NR_lio_listio,       sys_lio_listio),        // 257

   GENXY(__NR_freebsd11_getdents, sys_getdents),        // 272
   BSDX_(__NR_lchmod,           sys_lchmod),            // 274
   // netbsd_lchown                                     // 275

   BSDX_(__NR_lutimes,          sys_lutimes),           // 276
   // netbsd msync                                         277
   // unimpl SYS_freebsd11_nstat                           278
   // unimpl SYS_freebsd11_nfstat                          279

   // unimpl SYS_freebsd11_nlstat                          280

   BSDXY(__NR_preadv,           sys_preadv),            // 289
   BSDX_(__NR_pwritev,          sys_pwritev),           // 290

   // freebsd 4 fhstatfs                                   297
   BSDXY(__NR_fhopen,           sys_fhopen),            // 298
#if (FREEBSD_VERS >= FREEBSD_12)
   BSDXY(__NR_freebsd11_fhstat, sys_freebsd11_fhstat),  // 299
#else
   BSDXY(__NR_fhstat,           sys_fhstat),            // 299
#endif

   BSDX_(__NR_modnext,          sys_modnext),           // 300
   BSDXY(__NR_modstat,          sys_modstat),           // 301
   BSDX_(__NR_modfnext,         sys_modfnext),          // 302
   BSDX_(__NR_modfind,          sys_modfind),           // 303

   BSDX_(__NR_kldload,          sys_kldload),           // 304
   BSDX_(__NR_kldunload,        sys_kldunload),         // 305
   BSDX_(__NR_kldfind,          sys_kldfind),           // 306
   BSDX_(__NR_kldnext,          sys_kldnext),           // 307

   BSDXY(__NR_kldstat,          sys_kldstat),           // 308
   BSDX_(__NR_kldfirstmod,      sys_kldfirstmod),       // 309
   GENX_(__NR_getsid,           sys_getsid),            // 310
   BSDX_(__NR_setresuid,        sys_setresuid),         // 311

   BSDX_(__NR_setresgid,        sys_setresgid),         // 312
   // obsol signanosleep                                   313
   BSDX_(__NR_aio_return,       sys_aio_return),        // 314
   BSDX_(__NR_aio_suspend,      sys_aio_suspend),       // 315

   BSDX_(__NR_aio_cancel,       sys_aio_cancel),        // 316
   BSDX_(__NR_aio_error,        sys_aio_error),         // 317
   // freebsd 6 aio_read                                   318
   // freebsd 6 aio_write                                  319
   // freebsd 6 lio_listio                                 320
   BSDX_(__NR_yield,            sys_yield),             // 321
   // obs thr_sleep                                        322
   // obs thr_wakeup                                       323

   GENX_(__NR_mlockall,         sys_mlockall),          // 324
   BSDX_(__NR_munlockall,       sys_munlockall),        // 325
   BSDXY(__NR___getcwd,         sys___getcwd),          // 326
   BSDX_(__NR_sched_setparam,   sys_sched_setparam),    // 327
   BSDXY(__NR_sched_getparam,   sys_sched_getparam),    // 328
   BSDX_(__NR_sched_setscheduler, sys_sched_setscheduler), // 329
   BSDX_(__NR_sched_getscheduler, sys_sched_getscheduler), // 330
   BSDX_(__NR_sched_yield,      sys_sched_yield),       // 331

   BSDX_(__NR_sched_get_priority_max, sys_sched_get_priority_max), // 332
   BSDX_(__NR_sched_get_priority_min, sys_sched_get_priority_min), // 333
   BSDXY(__NR_sched_rr_get_interval, sys_sched_rr_get_interval), // 334
   BSDX_(__NR_utrace,           sys_utrace),            // 335

   // freebsd 4 sendfile                                   336
   BSDXY(__NR_kldsym,           sys_kldsym),            // 337
   BSDX_(__NR_jail,             sys_jail),              // 338
   // unimpl SYS_nnpfs_syscall                             339

   BSDXY(__NR_sigprocmask,      sys_sigprocmask),       // 340
   BSDXY(__NR_sigsuspend,       sys_sigsuspend),        // 341
   // freebsd 4 sigaction                                  342
   BSDXY(__NR_sigpending,       sys_sigpending),        // 343

   // freebsd 4 sigreturn                                  344
   BSDXY(__NR_sigtimedwait,     sys_sigtimedwait),      // 345
   BSDXY(__NR_sigwaitinfo,      sys_sigwaitinfo),       // 346
   BSDXY(__NR___acl_get_file,   sys___acl_get_file),    // 347

   BSDX_(__NR___acl_set_file,   sys___acl_set_file),    // 348
   BSDXY(__NR___acl_get_fd,     sys___acl_get_fd),      // 349
   BSDX_(__NR___acl_set_fd,     sys___acl_set_fd),      // 350
   BSDX_(__NR___acl_delete_file, sys___acl_delete_file), // 351

   BSDX_(__NR___acl_delete_fd,  sys___acl_delete_fd),   // 352
   BSDX_(__NR___acl_aclcheck_file, sys___acl_aclcheck_file), // 353
   BSDX_(__NR___acl_aclcheck_fd, sys___acl_aclcheck_fd), // 354
   BSDX_(__NR_extattrctl,       sys_extattrctl),        // 355
   BSDX_(__NR_extattr_set_file, sys_extattr_set_file),  // 356
   BSDXY(__NR_extattr_get_file, sys_extattr_get_file),  // 357
   BSDX_(__NR_extattr_delete_file, sys_extattr_delete_file), // 358
   BSDXY(__NR_aio_waitcomplete, sys_aio_waitcomplete),  // 359

   BSDXY(__NR_getresuid,        sys_getresuid),         // 360
   BSDXY(__NR_getresgid,        sys_getresgid),         // 361
   BSDXY(__NR_kqueue,           sys_kqueue),            // 362
#if (FREEBSD_VERS >= FREEBSD_12)
   BSDXY(__NR_freebsd11_kevent, sys_freebsd11_kevent),  // 363
#else
   BSDXY(__NR_kevent,           sys_kevent),            // 363
#endif
   // obs __cap_get_proc                                   364
   // obs __cap_set_proc                                   365
   // obs __cap_get_fd                                     366
   // obs __cap_get_file                                   367
   // obs __cap_set_fd                                     368
   // obs __cap_set_file                                   369

   BSDX_(__NR_extattr_set_fd,   sys_extattr_set_fd),    // 371
   BSDXY(__NR_extattr_get_fd,   sys_extattr_get_fd),    // 372
   BSDX_(__NR_extattr_delete_fd, sys_extattr_delete_fd), // 373
   BSDX_(__NR___setugid,        sys___setugid),         // 374
   // obs nfsclnt                                          375

   BSDX_(__NR_eaccess,          sys_eaccess),           // 376
   // unimpl afs3_syscall                                  377
   BSDX_(__NR_nmount,           sys_nmount),           //  378
   // obs kse_exit                                         379
   // obs kse_wakeup                                       380
   // obs kse_create                                       381
   // obs kse_thr_interrupt                                382
   // obs kse_release                                      383

   // unimpl __mac_get_proc                                384
   // unimpl __mac_set_proc                                385
   // unimpl __mac_get_fd                                  386
   // unimpl __mac_get_file                                387
   // unimpl __mac_set_fd                                  388
   // unimpl __mac_set_file                                389
   BSDXY(__NR_kenv,             sys_kenv),              // 390
   BSDX_(__NR_lchflags,         sys_lchflags),          // 391

   BSDXY(__NR_uuidgen,          sys_uuidgen),           // 392
   BSDXY(__NR_sendfile,         sys_sendfile),          // 393
   // unimpl mac_syscall                                   394

#if (FREEBSD_VERS >= FREEBSD_12)
   BSDXY(__NR_freebsd11_getfsstat, sys_freebsd11_getfsstat), // 395
   BSDXY(__NR_freebsd11_statfs, sys_statfs),            // 396
   BSDXY(__NR_freebsd11_fstatfs, sys_fstatfs),          // 397
   BSDXY(__NR_freebsd11_fhstatfs, sys_fhstatfs),        // 398
#else
   BSDXY(__NR_getfsstat,        sys_getfsstat),         // 395
   BSDXY(__NR_statfs,           sys_statfs),            // 396
   BSDXY(__NR_fstatfs,          sys_fstatfs),           // 397
   BSDXY(__NR_fhstatfs,         sys_fhstatfs),          // 398
#endif

   // unimpl ksem_close                                    400
   // unimpl ksem_post                                     401
   // unimpl ksem_wait                                     402
   // unimpl ksem_trywait                                  403

   // unimpl ksem_init                                     404
   // unimpl ksem_open                                     405
   // unimpl ksem_unlink                                   406
   // unimpl ksem_getvalue                                 407

   // unimpl ksem_destroy                                  408
   // unimpl __mac_get_pid                                 409
   // unimpl __mac_get_link                                410
   // unimpl __mac_set_link                                411

   BSDX_(__NR_extattr_set_link, sys_extattr_set_link),  // 412
   BSDXY(__NR_extattr_get_link, sys_extattr_get_link),  // 413
   BSDX_(__NR_extattr_delete_link, sys_extattr_delete_link), // 414
   // unimpl __mac_execve                                  415

   BSDXY(__NR_sigaction,        sys_sigaction),         // 416
   BSDX_(__NR_sigreturn,        sys_sigreturn),         // 417

   BSDXY(__NR_getcontext,       sys_getcontext),        // 421
   BSDX_(__NR_setcontext,       sys_setcontext),        // 422
   BSDXY(__NR_swapcontext,      sys_swapcontext),       // 423

#if (FREEBSD_VERS >= FREEBSD_13_1)
   BSDX_(__NR_freebsd13_swapoff, sys_freebsd13_swapoff), // 424
#else
   BSDX_(__NR_swapoff,          sys_swapoff),           // 424
#endif
   BSDXY(__NR___acl_get_link,   sys___acl_get_link),    // 425
   BSDX_(__NR___acl_set_link,   sys___acl_set_link),    // 426
   BSDX_(__NR___acl_delete_link, sys___acl_delete_link), // 427

   BSDX_(__NR___acl_aclcheck_link, sys___acl_aclcheck_link), // 428
   BSDXY(__NR_sigwait,          sys_sigwait),           // 429
   BSDX_(__NR_thr_create,       sys_thr_create),        // 430
   BSDX_(__NR_thr_exit,         sys_thr_exit),          // 431

   BSDXY(__NR_thr_self,         sys_thr_self),          // 432
   BSDXY(__NR_thr_kill,         sys_thr_kill),          // 433
#if (FREEBSD_VERS <= FREEBSD_10)
   BSDXY(__NR__umtx_lock,       sys__umtx_lock),        // 434
   BSDXY(__NR__umtx_unlock,     sys__umtx_unlock),      // 435
#endif

   BSDX_(__NR_jail_attach,      sys_jail_attach),       // 436
   BSDXY(__NR_extattr_list_fd,  sys_extattr_list_fd),   // 437
   BSDXY(__NR_extattr_list_file, sys_extattr_list_file), // 438
   BSDXY(__NR_extattr_list_link, sys_extattr_list_link), // 439

   // obs kse_switchin                                     440
   // unimpl ksem_timedwait                                441
   BSDX_(__NR_thr_suspend,      sys_thr_suspend),       // 442
   BSDX_(__NR_thr_wake,         sys_thr_wake),          // 443
   BSDX_(__NR_kldunloadf,       sys_kldunloadf),        // 444
   // unimpl audit                                         445
   // unimpl auditon                                       446
   // unimpl getauid                                       447

   // unimpl setauid                                       448
   // unimpl getaudit                                      449
   // unimpl setaudit                                      450
   // unimpl getaudit_addr                                 451
   // unimpl setaudit_addr                                 452
   // unimpl auditctl                                      453
   BSDXY(__NR__umtx_op,         sys__umtx_op),          // 454
   BSDX_(__NR_thr_new,          sys_thr_new),           // 455

   BSDX_(__NR_sigqueue,         sys_sigqueue),          // 456
   BSDXY(__NR_kmq_open,         sys_kmq_open),          // 457
   BSDX_(__NR_kmq_setattr,      sys_kmq_setattr),       // 458
   BSDXY(__NR_kmq_timedreceive, sys_kmq_timedreceive),  // 459

   BSDX_(__NR_kmq_timedsend,    sys_kmq_timedsend),     // 460
   BSDX_(__NR_kmq_notify,       sys_kmq_notify),        // 461
   BSDX_(__NR_kmq_unlink,       sys_kmq_unlink),        // 462
   BSDX_(__NR_abort2,           sys_abort2),            // 463

   BSDX_(__NR_thr_set_name,     sys_thr_set_name),      // 464
   BSDX_(__NR_aio_fsync,        sys_aio_fsync),         // 465
   BSDXY(__NR_rtprio_thread,    sys_rtprio_thread),     // 466

   // unimpl sctp_peeloff                                  471
   BSDX_(__NR_sctp_generic_sendmsg, sys_sctp_generic_sendmsg), // 472
   // unimpl sctp_generic_sendmsg_iov                      473
   BSDXY(__NR_sctp_generic_recvmsg, sys_sctp_generic_recvmsg), // 474
   BSDXY(__NR_pread,            sys_pread),             // 475

   BSDX_(__NR_pwrite,           sys_pwrite),            // 476
   BSDX_(__NR_mmap,             sys_mmap),              // 477
   BSDX_(__NR_lseek,            sys_lseek),             // 478
   BSDX_(__NR_truncate,         sys_truncate),          // 479
   BSDX_(__NR_ftruncate,        sys_ftruncate),         // 480
   BSDXY(__NR_thr_kill2,        sys_thr_kill2),         // 481
   BSDXY(__NR_shm_open,         sys_shm_open),          // 482
   BSDX_(__NR_shm_unlink,       sys_shm_unlink),        // 483

   BSDXY(__NR_cpuset,           sys_cpuset),            // 484
   BSDX_(__NR_cpuset_setid,     sys_cpuset_setid),      // 485
   BSDXY(__NR_cpuset_getid,     sys_cpuset_getid),      // 486

   BSDXY(__NR_cpuset_getaffinity, sys_cpuset_getaffinity), // 487
   BSDX_(__NR_cpuset_setaffinity, sys_cpuset_setaffinity), // 488
   BSDX_(__NR_faccessat,        sys_faccessat),         // 489
   BSDX_(__NR_fchmodat,         sys_fchmodat),          // 490
   BSDX_(__NR_fchownat,         sys_fchownat),          // 491

   BSDX_(__NR_fexecve,          sys_fexecve),           // 492
#if (FREEBSD_VERS >= FREEBSD_12)
   BSDXY(__NR_freebsd11_fstatat, sys_freebsd11_fstatat), // 493
#else
   BSDXY(__NR_fstatat,          sys_fstatat),           // 493
#endif
   BSDX_(__NR_futimesat,        sys_futimesat),         // 494
   BSDX_(__NR_linkat,           sys_linkat),            // 495

   BSDX_(__NR_mkdirat,          sys_mkdirat),           // 496
   BSDX_(__NR_mkfifoat,         sys_mkfifoat),          // 497

#if (FREEBSD_VERS >= FREEBSD_12)
   BSDX_(__NR_freebsd11_mknodat, sys_freebsd11_mknodat), // 498
#else
   BSDX_(__NR_mknodat,          sys_mknodat),           // 498
#endif

   BSDXY(__NR_openat,           sys_openat),            // 499

   BSDXY(__NR_readlinkat,       sys_readlinkat),        // 500
   BSDX_(__NR_renameat,         sys_renameat),          // 501
   BSDX_(__NR_symlinkat,        sys_symlinkat),         // 502
   BSDX_(__NR_unlinkat,         sys_unlinkat),          // 503

   BSDX_(__NR_posix_openpt,     sys_posix_openpt),      // 504
   // unimp gssd_syscall                                   505
   BSDX_(__NR_jail_get,         sys_jail_get),          // 506
   BSDX_(__NR_jail_set,         sys_jail_set),          // 507
   BSDX_(__NR_jail_remove,      sys_jail_remove),       // 508
   BSDX_(__NR_closefrom,        sys_closefrom),         // 509
   BSDXY(__NR___semctl,         sys___semctl),          // 510
   BSDXY(__NR_msgctl,           sys_msgctl),            // 511
   BSDXY(__NR_shmctl,           sys_shmctl),            // 512
   BSDX_(__NR_lpathconf,        sys_lpathconf),         // 513
   /* 514 is obsolete cap_new */
   BSDXY(__NR___cap_rights_get, sys_cap_rights_get),    // 515
   BSDX_(__NR_cap_enter,        sys_cap_enter),         // 516
   BSDXY(__NR_cap_getmode,      sys_cap_getmode),       // 517
   BSDXY(__NR_pdfork,           sys_pdfork),            // 518
   BSDX_(__NR_pdkill,           sys_pdkill),            // 519
   BSDXY(__NR_pdgetpid,         sys_pdgetpid),          // 520
   BSDXY(__NR_pselect,          sys_pselect),           // 522
   BSDXY(__NR_getloginclass,    sys_getloginclass),     // 523
   BSDX_(__NR_setloginclass,    sys_setloginclass),     // 524
   BSDXY(__NR_rctl_get_racct,   sys_rctl_get_racct),    // 525
   BSDXY(__NR_rctl_get_rules,   sys_rctl_get_rules),    // 526
   BSDXY(__NR_rctl_get_limits,  sys_rctl_get_limits),   // 527
   BSDXY(__NR_rctl_add_rule,    sys_rctl_add_rule),     // 528
   BSDXY(__NR_rctl_remove_rule, sys_rctl_remove_rule),  // 529
   BSDX_(__NR_posix_fallocate,  sys_posix_fallocate),   // 530
   BSDX_(__NR_posix_fadvise,    sys_posix_fadvise),     // 531
   BSDXY(__NR_wait6,            sys_wait6),             // 532
   BSDX_(__NR_cap_rights_limit, sys_cap_rights_limit),  // 533
   BSDX_(__NR_cap_ioctls_limit, sys_cap_ioctls_limit),  // 534
   BSDXY(__NR_cap_ioctls_get,   sys_cap_ioctls_get),    // 535
   BSDX_(__NR_cap_fcntls_limit, sys_cap_fcntls_limit),  // 536
   BSDXY(__NR_cap_fcntls_get,   sys_cap_fcntls_get),    // 537
   BSDX_(__NR_bindat,           sys_bindat),            // 538
   BSDX_(__NR_connectat,        sys_connectat),         // 539
   BSDX_(__NR_chflagsat,        sys_chflagsat),         // 540
   BSDXY(__NR_accept4,          sys_accept4),           // 541
   BSDXY(__NR_pipe2,            sys_pipe2),             // 542
   BSDX_(__NR_aio_mlock,        sys_aio_mlock),         // 543
   BSDXY(__NR_procctl,          sys_procctl),           // 544

   // 544 is the highest syscall on FreeBSD 9

#if (FREEBSD_VERS >= FREEBSD_10)

   BSDXY(__NR_ppoll,            sys_ppoll),             // 545
   BSDX_(__NR_futimens,         sys_futimens),          // 546
   BSDX_(__NR_utimensat,        sys_utimensat),         // 547

#endif // FREEBSD_VERS >= FREEBSD_10

#if (FREEBSD_VERS >= FREEBSD_11)

   /* 548 is obsolete numa_getaffinity */
   /* 549 is obsolete numa_setaffinity */
   BSDX_(__NR_fdatasync,        sys_fdatasync),         // 550

#endif // FREEBSD_VERS >= FREEBSD_11

#if (FREEBSD_VERS >= FREEBSD_12)
   BSDXY(__NR_fstat,            sys_fstat),             // 551
   BSDXY(__NR_fstatat,          sys_fstatat),           // 552
   BSDXY(__NR_fhstat,           sys_fhstat),            // 553
   BSDXY(__NR_getdirentries,    sys_getdirentries),     // 554
   BSDXY(__NR_statfs,           sys_statfs),            // 555
   BSDXY(__NR_fstatfs,          sys_fstatfs),           // 556
   BSDXY(__NR_getfsstat,        sys_getfsstat),         // 557
   BSDXY(__NR_fhstatfs,         sys_fhstatfs),          // 558
   BSDX_(__NR_mknodat,          sys_mknodat),           // 559
   BSDXY(__NR_kevent,           sys_kevent),            // 560
   BSDXY(__NR_cpuset_getdomain, sys_cpuset_getdomain),  // 561
   BSDX_(__NR_cpuset_setdomain, sys_cpuset_setdomain),  // 562
   BSDXY(__NR_getrandom,        sys_getrandom),         // 563
   BSDXY(__NR_getfhat,          sys_getfhat),           // 564
   BSDX_(__NR_fhlink,           sys_fhlink),            // 565
   BSDX_(__NR_fhlinkat,         sys_fhlinkat),          // 566
   BSDXY(__NR_fhreadlink,       sys_fhreadlink),        // 567
#endif // FREEBSD_VERS >= FREEBSD_12

#if (FREEBSD_VERS >= FREEBSD_12_2)
   BSDX_(__NR_funlinkat,        sys_funlinkat),         // 568
   BSDX_(__NR_copy_file_range,  sys_copy_file_range),   // 569
   BSDXY(__NR___sysctlbyname,   sys___sysctlbyname),    // 570

#if (FREEBSD_VERS >= FREEBSD_13_0)
   BSDXY(__NR_shm_open2,        sys_shm_open2),         // 571
   // unimpl __NR_shm_rename          572
   BSDX_(__NR_sigfastblock,     sys_sigfastblock),      // 573
   BSDXY( __NR___realpathat,    sys___realpathat),      // 574
#endif
   BSDXY(__NR_close_range,      sys_close_range),       // 575
#endif

#if (FREEBSD_VERS >= FREEBSD_13_0)
   // unimpl __NR_rpctls_syscall      576
   BSDX_(__NR___specialfd,      sys___specialfd),       // 577
   BSDX_(__NR_aio_writev,       sys_aio_writev),        // 578
   BSDXY(__NR_aio_readv,        sys_aio_readv),         // 579
#endif

#if (FREEBSD_VERS >= FREEBSD_13_1)

#if (FREEBSD_VERS >= FREEBSD_14)
   BSDXY(__NR_fspacectl,        sys_fspacectl),        //  580
#endif
   // unimpl __NR_sched_getcpu        581
   BSDX_(__NR_swapoff,          sys_swapoff),           // 582
#endif

#if (FREEBSD_VERS >= FREEBSD_15) || (FREEBSD_VERS >= FREEBSD_13_3)
   BSDXY(__NR_kqueuex,          sys_kqueuex),           // 583
   BSDX_(__NR_membarrier,       sys_membarrier),        // 584
#endif
#if (FREEBSD_VERS >= FREEBSD_15)
   BSDXY(__NR_timerfd_create,   sys_timerfd_create),    // 585
   BSDXY(__NR_timerfd_settime,  sys_timerfd_settime),   // 586
   BSDXY(__NR_timerfd_gettime,  sys_timerfd_gettime),   // 587
#endif


   BSDX_(__NR_fake_sigreturn,   sys_fake_sigreturn),    // 1000, fake sigreturn

};

const SyscallTableEntry* ML_(get_freebsd_syscall_entry) ( UInt sysno )
{
   const UInt syscall_table_size
      = sizeof(ML_(syscall_table)) / sizeof(ML_(syscall_table)[0]);

   /* Is it in the contiguous initial section of the table? */
   if (sysno < syscall_table_size) {
      const SyscallTableEntry* sys = &ML_(syscall_table)[sysno];
      if (sys->before == NULL) {
         return NULL; /* no entry */
      }
      return sys;
   }

   /* Can't find a wrapper */
   return NULL;
}

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/

#endif // defined(VGO_freebsd)
