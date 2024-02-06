
/*--------------------------------------------------------------------*/
/*--- Solaris-specific syscalls, etc.            syswrap-solaris.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2011-2017 Petr Pavlu
      setup@dagobah.cz

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

/* Copyright 2013-2017, Ivo Raisr <ivosh@ivosh.net>. */

/* Copyright 2015-2017, Tomas Jedlicka <jedlickat@gmail.com>. */

/* Copyright 2013, OmniTI Computer Consulting, Inc. All rights reserved. */

#if defined(VGO_solaris)

#include "libvex_guest_offsets.h"
#include "pub_core_basics.h"
#include "pub_core_vki.h"
#include "pub_core_vkiscnums.h"
#include "pub_core_threadstate.h"
#include "pub_core_aspacemgr.h"
#include "pub_core_debuginfo.h"         // VG_(di_notify_*)
#include "pub_core_debuglog.h"
#include "pub_core_clientstate.h"
#include "pub_core_gdbserver.h"
#include "pub_core_inner.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcfile.h"
#include "pub_core_libcprint.h"
#include "pub_core_libcproc.h"
#include "pub_core_libcsignal.h"
#include "pub_core_machine.h"           // VG_(get_SP)
#include "pub_core_mallocfree.h"
#include "pub_core_options.h"
#include "pub_core_tooliface.h"
#include "pub_core_transtab.h"          // VG_(discard_translations)
#include "pub_core_scheduler.h"
#include "pub_core_sigframe.h"
#include "pub_core_signals.h"
#include "pub_core_stacks.h"
#include "pub_core_syscall.h"
#include "pub_core_syswrap.h"
#include "pub_core_ume.h"
#if defined(ENABLE_INNER_CLIENT_REQUEST)
#include "pub_core_clreq.h"
#endif

#include "priv_types_n_macros.h"
#include "priv_syswrap-generic.h"
#include "priv_syswrap-main.h"
#include "priv_syswrap-solaris.h"

/* Return the number of non-dead and daemon threads.
   count_daemon == True:  count daemon threads
   count_daemon == False: count non-daemon threads */
static UInt count_living_daemon_threads(Bool count_daemon)
{
   UInt count = 0;
   for (ThreadId tid = 1; tid < VG_N_THREADS; tid++)
      if (VG_(threads)[tid].status != VgTs_Empty &&
         VG_(threads)[tid].status != VgTs_Zombie &&
         VG_(threads)[tid].os_state.daemon_thread == count_daemon)
         count++;

   return count;
}

/* Note: The following functions (thread_wrapper, run_a_thread_NORETURN,
   ML_(start_thread_NORETURN), ML_(allocstack) and
   VG_(main_thread_wrapper_NORETURN)) are based on the code in
   syswrap-linux.c.  Keep them synchronized! */

/* Run a thread from beginning to end and return the thread's
   scheduler-return-code. */
static VgSchedReturnCode thread_wrapper(Word /*ThreadId*/ tidW)
{
   VgSchedReturnCode ret;
   ThreadId tid = (ThreadId)tidW;
   Int lwpid = VG_(gettid)();
   ThreadState *tst = VG_(get_ThreadState)(tid);

   VG_(debugLog)(1, "syswrap-solaris",
                    "thread_wrapper(tid=%u,lwpid=%d): entry\n",
                    tid, lwpid);

   vg_assert(tst->status == VgTs_Init);

   /* Make sure we get the CPU lock before doing anything significant. */
   VG_(acquire_BigLock)(tid, "thread_wrapper(starting new thread)");

   if (0)
     VG_(printf)("thread tid %u started: stack = %p\n", tid, (void *)&tid);

   /* Make sure error reporting is enabled in the new thread. */
   tst->err_disablement_level = 0;

   if (tid == 1)
      VG_TRACK(pre_thread_first_insn, tid);
   else {
      /* For newly created threads, VG_TRACK(pre_thread_first_insn, tid) is
         invoked later from PRE(sys_getsetcontext)() when setucontext()
         called from _thrp_setup() concludes new thread setup. Invoking it
         here would be way too early - new thread has no stack, yet. */
   }

   tst->os_state.lwpid = lwpid;
   tst->os_state.threadgroup = VG_(getpid)();

   /* Thread created with all signals blocked; scheduler will set the
      appropriate mask. */

   ret = VG_(scheduler)(tid);

   vg_assert(VG_(is_exiting)(tid));

   vg_assert(tst->status == VgTs_Runnable);
   vg_assert(VG_(is_running_thread)(tid));

   VG_(debugLog)(1, "syswrap-solaris",
                    "thread_wrapper(tid=%u,lwpid=%d): exit, schedreturncode %s\n",
                    tid, lwpid, VG_(name_of_VgSchedReturnCode)(ret));

   /* Return to caller, still holding the lock. */
   return ret;
}

/* Run a thread all the way to the end, then do appropriate exit actions
   (this is the last-one-out-turn-off-the-lights bit). */
static void run_a_thread_NORETURN(Word tidW)
{
   ThreadId tid = (ThreadId)tidW;
   VgSchedReturnCode src;
   Int c;
   ThreadState *tst;
#ifdef ENABLE_INNER_CLIENT_REQUEST
   Int registered_vgstack_id;
#endif

   VG_(debugLog)(1, "syswrap-solaris",
                    "run_a_thread_NORETURN(tid=%u): pre-thread_wrapper\n",
                    tid);

   tst = VG_(get_ThreadState)(tid);
   vg_assert(tst);

   /* A thread has two stacks:
      * the simulated stack (used by the synthetic cpu. Guest process
        is using this stack).
      * the valgrind stack (used by the real cpu. Valgrind code is running
        on this stack).
      When Valgrind runs as an inner, it must signal that its (real) stack
      is the stack to use by the outer to e.g. do stacktraces.
   */
   INNER_REQUEST
      (registered_vgstack_id
       = VALGRIND_STACK_REGISTER(tst->os_state.valgrind_stack_base,
                                 tst->os_state.valgrind_stack_init_SP));

   /* Run the thread all the way through. */
   src = thread_wrapper(tid);

   VG_(debugLog)(1, "syswrap-solaris",
                    "run_a_thread_NORETURN(tid=%u): post-thread_wrapper\n",
                    tid);

   c = count_living_daemon_threads(False);
   vg_assert(c >= 1); /* Stay sane. */

   /* Tell the tool that schedctl data belonging to this thread are gone. */
   Addr a = tst->os_state.schedctl_data;
   if (a != 0)
      VG_TRACK(die_mem_munmap, a, sizeof(struct vki_sc_shared));

   /* Deregister thread's stack. */
   if (tst->os_state.stk_id != NULL_STK_ID)
      VG_(deregister_stack)(tst->os_state.stk_id);

   /* Tell the tool this thread is exiting. */
   VG_TRACK(pre_thread_ll_exit, tid);

   /* If the thread is exiting with errors disabled, complain loudly;
      doing so is bad (does the user know this has happened?)  Also, in all
      cases, be paranoid and clear the flag anyway so that the thread slot is
      safe in this respect if later reallocated.  This should be unnecessary
      since the flag should be cleared when the slot is reallocated, in
      thread_wrapper(). */
   if (tst->err_disablement_level > 0) {
      VG_(umsg)(
         "WARNING: exiting thread has error reporting disabled.\n"
         "WARNING: possibly as a result of some mistake in the use\n"
         "WARNING: of the VALGRIND_DISABLE_ERROR_REPORTING macros.\n"
      );
      VG_(debugLog)(
         1, "syswrap-solaris",
            "run_a_thread_NORETURN(tid=%u): "
            "WARNING: exiting thread has err_disablement_level = %u\n",
            tid, tst->err_disablement_level
      );
   }
   tst->err_disablement_level = 0;

   if (c == 1) {
      UInt daemon_threads = count_living_daemon_threads(True);
      if (daemon_threads == 0)
         VG_(debugLog)(1, "syswrap-solaris",
                          "run_a_thread_NORETURN(tid=%u): "
                          "last one standing\n",
                          tid);
      else
         VG_(debugLog)(1, "syswrap-solaris",
                          "run_a_thread_NORETURN(tid=%u): "
                          "last non-daemon thread standing "
                          "[daemon threads=%u]\n",
                          tid, daemon_threads);

      /* We are the last non-daemon thread standing. Keep hold of the lock and
         carry on to show final tool results, then exit the entire system.
         Use the continuation pointer set at startup in m_main. */
      if ((src == VgSrc_ExitThread) && (daemon_threads > 0))
         src = VgSrc_ExitProcess;
      (*VG_(address_of_m_main_shutdown_actions_NORETURN))(tid, src);
   }
   else {
      VG_(debugLog)(1, "syswrap-solaris",
                       "run_a_thread_NORETURN(tid=%u): "
                       "not last one standing\n",
                       tid);

      /* OK, thread is dead, but others still exist.  Just exit. */

      /* This releases the run lock. */
      VG_(exit_thread)(tid);
      vg_assert(tst->status == VgTs_Zombie);
      vg_assert(sizeof(tst->status) == 4);

      INNER_REQUEST(VALGRIND_STACK_DEREGISTER(registered_vgstack_id));

      /* We have to use this sequence to terminate the thread to
         prevent a subtle race.  If VG_(exit_thread)() had left the
         ThreadState as Empty, then it could have been reallocated, reusing
         the stack while we're doing these last cleanups.  Instead,
         VG_(exit_thread) leaves it as Zombie to prevent reallocation.  We
         need to make sure we don't touch the stack between marking it Empty
         and exiting.  Hence the assembler. */
#if defined(VGP_x86_solaris)
      /* Luckily lwp_exit doesn't take any arguments so we don't have to mess
         with the stack. */
      __asm__ __volatile__ (
         "movl  %[EMPTY], %[status]\n"  /* set tst->status = VgTs_Empty */
         "movl  $"VG_STRINGIFY(__NR_lwp_exit)", %%eax\n"
         "int   $0x91\n"                /* lwp_exit() */
         : [status] "=m" (tst->status)
         : [EMPTY] "n" (VgTs_Empty)
         : "eax", "edx", "cc", "memory");
#elif defined(VGP_amd64_solaris)
      __asm__ __volatile__ (
         "movl  %[EMPTY], %[status]\n"  /* set tst->status = VgTs_Empty */
         "movq  $"VG_STRINGIFY(__NR_lwp_exit)", %%rax\n"
         "syscall\n"                    /* lwp_exit() */
         : [status] "=m" (tst->status)
         : [EMPTY] "n" (VgTs_Empty)
         : "rax", "rdx", "cc", "memory");
#else
#  error "Unknown platform"
#endif

      VG_(core_panic)("Thread exit failed?\n");
   }

   /*NOTREACHED*/
   vg_assert(0);
}

Word ML_(start_thread_NORETURN)(void *arg)
{
   ThreadState *tst = (ThreadState*)arg;
   ThreadId tid = tst->tid;

   run_a_thread_NORETURN((Word)tid);
   /*NOTREACHED*/
   vg_assert(0);
}

/* Allocate a stack for this thread, if it doesn't already have one.
   They're allocated lazily, and never freed.  Returns the initial stack
   pointer value to use, or 0 if allocation failed. */
Addr ML_(allocstack)(ThreadId tid)
{
   ThreadState *tst = VG_(get_ThreadState)(tid);
   VgStack *stack;
   Addr initial_SP;

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
         tst->os_state.valgrind_stack_base = (Addr)stack;
         tst->os_state.valgrind_stack_init_SP = initial_SP;
      }
   }

   if (0)
      VG_(printf)("stack for tid %u at %p; init_SP=%p\n",
                  tid,
                  (void*)tst->os_state.valgrind_stack_base,
                  (void*)tst->os_state.valgrind_stack_init_SP);

   return tst->os_state.valgrind_stack_init_SP;
}

/* Allocate a stack for the main thread, and run it all the way to the
   end.  Although we already have a working VgStack (VG_(interim_stack)) it's
   better to allocate a new one, so that overflow detection works uniformly
   for all threads.  Also initialize the GDT (for normal threads, this is done
   in the PRE wrapper of lwp_create). */
void VG_(main_thread_wrapper_NORETURN)(ThreadId tid)
{
   Addr sp;

   VG_(debugLog)(1, "syswrap-solaris",
                    "entering VG_(main_thread_wrapper_NORETURN)\n");

   sp = ML_(allocstack)(tid);
#if defined(ENABLE_INNER_CLIENT_REQUEST)
   {
      // we must register the main thread stack before the call
      // to ML_(call_on_new_stack_0_1), otherwise the outer valgrind
      // reports 'write error' on the non registered stack.
      ThreadState *tst = VG_(get_ThreadState)(tid);
      INNER_REQUEST
         ((void) 
          VALGRIND_STACK_REGISTER(tst->os_state.valgrind_stack_base,
                                  tst->os_state.valgrind_stack_init_SP));
   }
#endif

#if defined(VGP_x86_solaris)
   {
      ThreadState *tst = VG_(get_ThreadState)(tid);
      ML_(setup_gdt)(&tst->arch.vex);
      ML_(update_gdt_lwpgs)(tid);
   }
#elif defined(VGP_amd64_solaris)
   /* Nothing to do. */
#else
#  error "Unknown platform"
#endif

   /* If we can't even allocate the first thread's stack, we're hosed.
      Give up. */
   vg_assert2(sp != 0, "Cannot allocate main thread's stack.");

   /* Shouldn't be any other threads around yet. */
   vg_assert(VG_(count_living_threads)() == 1);

   ML_(call_on_new_stack_0_1)(
      (Addr)sp,               /* stack */
      0,                      /* bogus return address */
      run_a_thread_NORETURN,  /* fn to call */
      (Word)tid               /* arg to give it */
   );

   /*NOTREACHED*/
   vg_assert(0);
}

/* Deallocate the GDT for a thread. */
void VG_(cleanup_thread)(ThreadArchState *arch)
{
#if defined(VGP_x86_solaris)
   ML_(cleanup_gdt)(&arch->vex);
#elif defined(VGP_amd64_solaris)
   /* Nothing to do. */
#else
#  error "Unknown platform"
#endif
}

/*
 * Notify core about spring cleaning of schedctl data pages for all threads
 * in child post-fork handler. Libc will issue new schedctl syscalls for threads
 * in the child when needs arise.
 *
 * See also POST(schedctl) and run_a_thread_NORETURN() when a thread exits.
 */
static void clean_schedctl_data(ThreadId tid)
{
   UInt i;
   for (i = 0; i < VG_N_THREADS; i++) {
      ThreadState *tst = &VG_(threads)[i];
      if (tst->status != VgTs_Empty) {
         Addr a = tst->os_state.schedctl_data;
         if (a != 0) {
            tst->os_state.schedctl_data = 0;
            a = VG_PGROUNDDN(a);
            if (VG_(am_find_anon_segment)(a))
               VG_(am_notify_munmap)(a, VKI_PAGE_SIZE);
         }
      }
   }
}

void VG_(syswrap_init)(void)
{
   VG_(atfork)(NULL, NULL, clean_schedctl_data);
}

/* Changes ownership of a memory mapping shared between kernel and the client
   process. This mapping should have already been pre-arranged during process
   address space initialization happening in kernel. Valgrind on startup created
   a segment for this mapping categorized as Valgrind's owned anonymous.
   Size of this mapping typically varies among Solaris versions but should be
   page aligned.
   If 'once_only' is 'True', it is expected this function is called once only
   and the mapping ownership has not been changed, yet [useful during
   initialization]. If 'False', this function can be called many times but does
   change ownership only upon the first invocation [useful in syscall wrappers].
 */
void VG_(change_mapping_ownership)(Addr addr, Bool once_only)
{
   const NSegment *seg = VG_(am_find_anon_segment)(addr);
   vg_assert(seg != NULL);
   vg_assert(seg->start == addr);
   vg_assert(VG_IS_PAGE_ALIGNED(seg->start));
   vg_assert(VG_IS_PAGE_ALIGNED(seg->end + 1));
   SizeT size = seg->end - seg->start + 1;
   vg_assert(size > 0);

   Bool do_change = False;
   if (once_only) {
      vg_assert(VG_(am_is_valid_for_valgrind)(addr, size, VKI_PROT_READ));
      do_change = True;
   } else {
      if (!VG_(am_is_valid_for_client)(addr, size, VKI_PROT_READ))
         do_change = True;
   }

   if (do_change) {
      Bool change_ownership_OK = VG_(am_change_ownership_v_to_c)(addr, size);
      vg_assert(change_ownership_OK);

      /* Tell the tool about just discovered mapping. */
      VG_TRACK(new_mem_startup,
               addr, size,
               True  /* readable? */,
               False /* writable? */,
               False /* executable? */,
               0     /* di_handle */);
   }
}

/* Calculate the Fletcher-32 checksum of a given buffer. */
UInt ML_(fletcher32)(UShort *buf, SizeT blocks)
{
   UInt sum1 = 0;
   UInt sum2 = 0;
   SizeT i;

   for (i = 0; i < blocks; i++) {
      sum1 = (sum1 + buf[i]) % 0xffff;
      sum2 = (sum2 + sum1) % 0xffff;
   }

   return (sum2 << 16) | sum1;
}

/* Calculate the Fletcher-64 checksum of a given buffer. */
ULong ML_(fletcher64)(UInt *buf, SizeT blocks)
{
   ULong sum1 = 0;
   ULong sum2 = 0;
   SizeT i;

   for (i = 0; i < blocks; i++) {
      sum1 = (sum1 + buf[i]) % 0xffffffff;
      sum2 = (sum2 + sum1) % 0xffffffff;
   }
   return (sum2 << 32) | sum1;
}

/* Save a complete context (VCPU state, sigmask) of a given client thread
   into the vki_ucontext_t structure.  This structure is supposed to be
   allocated in the client memory, a caller must make sure that the memory can
   be dereferenced.  The active tool is informed about the save. */
void VG_(save_context)(ThreadId tid, vki_ucontext_t *uc, CorePart part)
{
   ThreadState *tst = VG_(get_ThreadState)(tid);

   VG_TRACK(pre_mem_write, part, tid, "save_context(uc)", (Addr)uc,
            sizeof(*uc));

   uc->uc_flags = VKI_UC_ALL;
   VG_TRACK(post_mem_write, part, tid, (Addr)&uc->uc_flags,
            sizeof(uc->uc_flags));

   /* Old context */
   uc->uc_link = tst->os_state.oldcontext;
   VG_TRACK(post_mem_write, part, tid, (Addr)&uc->uc_link,
            sizeof(uc->uc_link));

   /* Clear uc->vki_uc_signo.  This slot is used by the signal machinery to
      store a signal number. */
   VKI_UC_SIGNO(uc) = 0;

   /* Sigmask */
   uc->uc_sigmask = tst->sig_mask;
   VG_TRACK(post_mem_write, part, tid, (Addr)&uc->uc_sigmask,
            sizeof(uc->uc_sigmask));

   /* Stack */
   {
      if (tst->os_state.ustack
          && ML_(safe_to_deref)(tst->os_state.ustack, sizeof(vki_stack_t))
          && tst->os_state.ustack->ss_size) {
         /* If ustack points to a valid stack copy it to ucontext. */
         uc->uc_stack = *tst->os_state.ustack;
      }
      else {
         /* Ustack is not valid.  A correct stack has to be figured out
            manually. */
         SysRes res;
         vki_stack_t altstack;

         /* Get information about alternate stack. */
         res = VG_(do_sys_sigaltstack)(tid, NULL, &altstack);
         vg_assert(!sr_isError(res));

         if (altstack.ss_flags == VKI_SS_ONSTACK) {
            /* If the alternate stack is active copy it to ucontext. */
            uc->uc_stack = altstack;
         }
         else {
            /* No information about stack is present, save information about
               current main stack to ucontext.  This branch should be reached
               only by the main thread. */
            ThreadState *tst2 = VG_(get_ThreadState)(1);
            uc->uc_stack.ss_sp = (void*)(tst2->client_stack_highest_byte + 1
                                         - tst2->client_stack_szB);
            uc->uc_stack.ss_size = tst2->client_stack_szB;
            uc->uc_stack.ss_flags = 0;
         }
      }

      VG_TRACK(post_mem_write, part, tid, (Addr)&uc->uc_stack,
               sizeof(uc->uc_stack));
   }

   /* Save the architecture-specific part of the context. */
   ML_(save_machine_context)(tid, uc, part);
}

/* Set a complete context (VCPU state, sigmask) of a given client thread
   according to values passed in the vki_ucontext_t structure.  This structure
   is supposed to be allocated in the client memory, a caller must make sure
   that the memory can be dereferenced.  The active tool is informed about
   what parts of the structure are read.

   This function is a counterpart to VG_(save_context)(). */
void VG_(restore_context)(ThreadId tid, vki_ucontext_t *uc, CorePart part,
                          Bool esp_is_thrptr)
{
   ThreadState *tst = VG_(get_ThreadState)(tid);
   Addr old_esp = VG_(get_SP)(tid);

   VG_TRACK(pre_mem_read, part, tid, "restore_context(uc->uc_flags)",
            (Addr)&uc->uc_flags, sizeof(uc->uc_flags));

   /* Old context */
   VG_TRACK(pre_mem_read, part, tid, "restore_context(uc->uc_link)",
            (Addr)&uc->uc_link, sizeof(uc->uc_link));
   tst->os_state.oldcontext = uc->uc_link;

   /* Sigmask */
   if (uc->uc_flags & VKI_UC_SIGMASK) {
      SysRes res;

      VG_TRACK(pre_mem_read, part, tid, "restore_context(uc->uc_sigmask)",
               (Addr)&uc->uc_sigmask, sizeof(uc->uc_sigmask));
      res = VG_(do_sys_sigprocmask)(tid, VKI_SIG_SETMASK, &uc->uc_sigmask,
                                    NULL);
      /* Setting signal mask should never fail. */
      vg_assert(!sr_isError(res));
   }

   /* Stack */
   if (uc->uc_flags & VKI_UC_STACK) {
      VG_TRACK(pre_mem_read, part, tid, "restore_context(uc->uc_stack)",
               (Addr)&uc->uc_stack, sizeof(uc->uc_stack));

      if (uc->uc_stack.ss_flags == VKI_SS_ONSTACK) {
         /* This seems to be a little bit dangerous but it is what the kernel
            does. */
         if (VG_(clo_trace_signals))
            VG_(dmsg)("restore_context, sigaltstack: tid %u, "
                      "ss %p{%p,sz=%lu,flags=%#x}\n",
                      tid, &uc->uc_stack, uc->uc_stack.ss_sp,
                      (SizeT)uc->uc_stack.ss_size, (UInt)uc->uc_stack.ss_flags);

         tst->altstack.ss_sp = uc->uc_stack.ss_sp;
         tst->altstack.ss_size = uc->uc_stack.ss_size;
         /* Do not copy ss_flags, they are calculated dynamically by
            Valgrind. */
      }

      /* Copyout the new stack. */
      if (tst->os_state.ustack
          && VG_(am_is_valid_for_client)((Addr)tst->os_state.ustack,
                                         sizeof(*tst->os_state.ustack),
                                         VKI_PROT_WRITE)) {
         *tst->os_state.ustack = uc->uc_stack;
         VG_TRACK(post_mem_write, part, tid, (Addr)&tst->os_state.ustack,
                  sizeof(tst->os_state.ustack));
      }
   }

   /* Restore the architecture-specific part of the context. */
   ML_(restore_machine_context)(tid, uc, part, esp_is_thrptr);

   /* If the thread stack is already known, kill the deallocated stack area.
      This is important when returning from a signal handler. */
   if (tst->client_stack_highest_byte && tst->client_stack_szB) {
      Addr end = tst->client_stack_highest_byte;
      Addr start = end + 1 - tst->client_stack_szB;
      Addr new_esp = VG_(get_SP)(tid);

      /* Make sure that the old and new stack pointer are on the same (active)
         stack.  Alternate stack is currently never affected by this code. */
      if (start <= old_esp && old_esp <= end
          && start <= new_esp && new_esp <= end
          && new_esp > old_esp)
         VG_TRACK(die_mem_stack, old_esp - VG_STACK_REDZONE_SZB,
                  (new_esp - old_esp) + VG_STACK_REDZONE_SZB);
   }
}

/* Set a client stack associated with a given thread id according to values
   passed in the vki_stack_t structure. */
static void set_stack(ThreadId tid, vki_stack_t *st)
{
   ThreadState *tst = VG_(get_ThreadState)(tid);
   Addr new_start, new_end;
   SizeT new_size;
   Addr cur_start;
   SizeT cur_size;

   VG_(debugLog)(2, "syswrap-solaris",
                    "set stack: sp=%#lx, size=%#lx.\n",
                    (Addr)st->ss_sp, (SizeT)st->ss_size);

   /* Stay sane. */
   vg_assert(st->ss_flags == 0);

   new_start = (Addr)st->ss_sp;
   new_end = new_start + st->ss_size - 1;
   new_size = st->ss_size;
   cur_start = tst->client_stack_highest_byte + 1
               - tst->client_stack_szB;
   cur_size = tst->client_stack_szB;

   if (new_start == cur_start && new_size == cur_size) {
      /* No change is requested, bail out. */
      return;
   }

   if (tid == 1 && (new_size == 0 || new_size > VG_(clstk_max_size))) {
      /* The main thread requests to use a stack without any size checking, or
         too big stack.  Fallback to the maximum allocated client stack. */

      /* TODO I think it is possible to give up on setting main stack anyway.
         Valgrind knows where it is located and it is already registered as
         VG_(clstk_id). */

      new_size = VG_(clstk_max_size);
      new_end = tst->client_stack_highest_byte;
      new_start = new_end + 1 - new_size;
   }

   if (tst->os_state.stk_id == NULL_STK_ID) {
      /* This thread doesn't have a stack set yet. */
      VG_(debugLog)(2, "syswrap-solaris",
                       "Stack set to %#lx-%#lx (new) for thread %u.\n",
                       new_start, new_end, tid);
      tst->os_state.stk_id = VG_(register_stack)(new_start, new_end);
   } else {
      /* Change a thread stack. */
      VG_(debugLog)(2, "syswrap-solaris",
                       "Stack set to %#lx-%#lx (change) for thread %u.\n",
                       new_start, new_end, tid);
      VG_(change_stack)(tst->os_state.stk_id, new_start, new_end);
   }
   tst->client_stack_highest_byte = new_end;
   tst->client_stack_szB = new_size;
}

/* ---------------------------------------------------------------------
   Door tracking. Used mainly for server side where door_return()
   parameters alone do not contain sufficient information.
   Also used on client side when new door descriptors are passed via
   door_call() in desc_ptr. Not used for tracking door descriptors
   explicitly open()'ed [generic fd tracking is used in that case].
   ------------------------------------------------------------------ */

/* One of these is allocated for each created door. */
typedef struct OpenDoor
{
   Bool server; /* TRUE = server door, FALSE = client door */
   Int fd;      /* The file descriptor. */
   union {
      /* Server side. */
      struct {
         Addr server_procedure;  /* The door server procedure. */
         HChar *pathname;        /* NULL if unknown. */
      };
      /* Client side. */
      struct {
         /* Hook called during PRE door_call()
            to check contents of params->data_ptr. */
         void (*pre_mem_hook)(ThreadId tid, Int fd,
                              void *data_ptr, SizeT data_size);
         /* Hook called during POST door_call()
            to define contents of params->rbuf. */
         void (*post_mem_hook)(ThreadId tid, Int fd,
                               void *rbuf, SizeT rsize);
      };
   };
   struct OpenDoor *next, *prev;
} OpenDoor;

/* List of allocated door fds. */
static OpenDoor *doors_recorded = NULL;
static UInt nr_doors_recorded = 0;

static OpenDoor *door_record_create(void)
{
   OpenDoor *d = VG_(malloc)("syswrap.door_record_create.1", sizeof(OpenDoor));
   d->prev = NULL;
   d->next = doors_recorded;
   if (doors_recorded != NULL)
      doors_recorded->prev = d;
   doors_recorded = d;
   nr_doors_recorded += 1;

   return d;
}

/* Records a server door. */
static void door_record_server(ThreadId tid, Addr server_procedure, Int fd)
{
   OpenDoor *d = doors_recorded;

   while (d != NULL) {
      if ((d->server == TRUE) && (d->server_procedure == server_procedure)) {
         if (d->pathname) {
            VG_(free)(d->pathname);
         }
         break;
      }
      d = d->next;
   }

   if (d == NULL)
      d = door_record_create();
   vg_assert(d != NULL);

   d->server = TRUE;
   d->fd = fd;
   d->server_procedure = server_procedure;
   d->pathname = NULL;
}

/* Records a client door. */
static void door_record_client(ThreadId tid, Int fd,
   void (*pre_mem_hook)(ThreadId tid, Int fd, void *data_ptr, SizeT data_size),
   void (*post_mem_hook)(ThreadId tid, Int fd, void *rbuf, SizeT rsize))
{
   OpenDoor *d = doors_recorded;

   while (d != NULL) {
      if ((d->server == FALSE) && (d->fd == fd))
         break;
      d = d->next;
   }

   if (d == NULL)
      d = door_record_create();
   vg_assert(d != NULL);

   d->server = FALSE;
   d->fd = fd;
   d->pre_mem_hook = pre_mem_hook;
   d->post_mem_hook = post_mem_hook;
}

/* Revokes an open door, be it server side or client side. */
static void door_record_revoke(ThreadId tid, Int fd)
{
   OpenDoor *d = doors_recorded;

   while (d != NULL) {
      if (d->fd == fd) {
         if (d->prev != NULL)
            d->prev->next = d->next;
         else
            doors_recorded = d->next;
         if (d->next != NULL)
            d->next->prev = d->prev;

         if ((d->server == TRUE) && (d->pathname != NULL))
            VG_(free)(d->pathname);
         VG_(free)(d);
         nr_doors_recorded -= 1;
         return;
      }
      d = d->next;
   }
}

/* Attaches a server door to a filename. */
static void door_record_server_fattach(Int fd, HChar *pathname)
{
   OpenDoor *d = doors_recorded;

   while (d != NULL) {
      if (d->fd == fd) {
         vg_assert(d->server == TRUE);

         if (d->pathname != NULL)
            VG_(free)(d->pathname);
         d->pathname = VG_(strdup)("syswrap.door_server_fattach.1", pathname);
         return;
      }
      d = d->next;
   }
}

/* Finds a server door based on server procedure. */
static const OpenDoor *door_find_by_proc(Addr server_procedure)
{
   OpenDoor *d = doors_recorded;

   while (d != NULL) {
      if ((d->server) && (d->server_procedure == server_procedure))
         return d;
      d = d->next;
   }

   return NULL;
}

/* Finds a client door based on fd. */
static const OpenDoor *door_find_by_fd(Int fd)
{
   OpenDoor *d = doors_recorded;

   while (d != NULL) {
      if ((d->server == FALSE) && (d->fd == fd))
         return d;
      d = d->next;
   }

   return NULL;
}

/* ---------------------------------------------------------------------
   PRE/POST wrappers for Solaris-specific syscalls
   ------------------------------------------------------------------ */

#define PRE(name)       DEFN_PRE_TEMPLATE(solaris, name)
#define POST(name)      DEFN_POST_TEMPLATE(solaris, name)

/* prototypes */
DECL_TEMPLATE(solaris, sys_exit);
#if defined(SOLARIS_SPAWN_SYSCALL)
DECL_TEMPLATE(solaris, sys_spawn);
#endif /* SOLARIS_SPAWN_SYSCALL */
#if defined(SOLARIS_OLD_SYSCALLS)
DECL_TEMPLATE(solaris, sys_open);
#endif /* SOLARIS_OLD_SYSCALLS */
DECL_TEMPLATE(solaris, sys_close);
DECL_TEMPLATE(solaris, sys_linkat);
DECL_TEMPLATE(solaris, sys_symlinkat);
DECL_TEMPLATE(solaris, sys_time);
DECL_TEMPLATE(solaris, sys_brk);
DECL_TEMPLATE(solaris, sys_stat);
DECL_TEMPLATE(solaris, sys_lseek);
DECL_TEMPLATE(solaris, sys_mount);
DECL_TEMPLATE(solaris, sys_readlinkat);
DECL_TEMPLATE(solaris, sys_stime);
DECL_TEMPLATE(solaris, sys_fstat);
#if defined(SOLARIS_FREALPATHAT_SYSCALL)
DECL_TEMPLATE(solaris, sys_frealpathat);
#endif /* SOLARIS_FREALPATHAT_SYSCALL */
DECL_TEMPLATE(solaris, sys_stty);
DECL_TEMPLATE(solaris, sys_gtty);
DECL_TEMPLATE(solaris, sys_pgrpsys);
DECL_TEMPLATE(solaris, sys_pipe);
DECL_TEMPLATE(solaris, sys_faccessat);
DECL_TEMPLATE(solaris, sys_mknodat);
DECL_TEMPLATE(solaris, sys_sysi86);
DECL_TEMPLATE(solaris, sys_shmsys);
DECL_TEMPLATE(solaris, sys_semsys);
DECL_TEMPLATE(solaris, sys_ioctl);
DECL_TEMPLATE(solaris, sys_fchownat);
DECL_TEMPLATE(solaris, sys_fdsync);
DECL_TEMPLATE(solaris, sys_execve);
DECL_TEMPLATE(solaris, sys_fcntl);
DECL_TEMPLATE(solaris, sys_renameat);
DECL_TEMPLATE(solaris, sys_unlinkat);
DECL_TEMPLATE(solaris, sys_fstatat);
DECL_TEMPLATE(solaris, sys_openat);
DECL_TEMPLATE(solaris, sys_tasksys);
DECL_TEMPLATE(solaris, sys_getpagesizes);
DECL_TEMPLATE(solaris, sys_lwp_park);
DECL_TEMPLATE(solaris, sys_sendfilev);
#if defined(SOLARIS_LWP_NAME_SYSCALL)
DECL_TEMPLATE(solaris, sys_lwp_name);
#endif /* SOLARIS_LWP_NAME_SYSCALL */
DECL_TEMPLATE(solaris, sys_privsys);
DECL_TEMPLATE(solaris, sys_ucredsys);
DECL_TEMPLATE(solaris, sys_sysfs);
DECL_TEMPLATE(solaris, sys_getmsg);
DECL_TEMPLATE(solaris, sys_putmsg);
DECL_TEMPLATE(solaris, sys_lstat);
DECL_TEMPLATE(solaris, sys_sigprocmask);
DECL_TEMPLATE(solaris, sys_sigsuspend);
DECL_TEMPLATE(solaris, sys_sigaction);
DECL_TEMPLATE(solaris, sys_sigpending);
DECL_TEMPLATE(solaris, sys_getsetcontext);
DECL_TEMPLATE(solaris, sys_fchmodat);
DECL_TEMPLATE(solaris, sys_mkdirat);
DECL_TEMPLATE(solaris, sys_statvfs);
DECL_TEMPLATE(solaris, sys_fstatvfs);
DECL_TEMPLATE(solaris, sys_nfssys);
DECL_TEMPLATE(solaris, sys_waitid);
DECL_TEMPLATE(solaris, sys_sigsendsys);
#if defined(SOLARIS_UTIMESYS_SYSCALL)
DECL_TEMPLATE(solaris, sys_utimesys);
#endif /* SOLARIS_UTIMESYS_SYSCALL */
#if defined(SOLARIS_UTIMENSAT_SYSCALL)
DECL_TEMPLATE(solaris, sys_utimensat);
#endif /* SOLARIS_UTIMENSAT_SYSCALL */
DECL_TEMPLATE(solaris, sys_sigresend);
DECL_TEMPLATE(solaris, sys_priocntlsys);
DECL_TEMPLATE(solaris, sys_pathconf);
DECL_TEMPLATE(solaris, sys_mmap);
#if defined(SOLARIS_UUIDSYS_SYSCALL)
DECL_TEMPLATE(solaris, sys_uuidsys);
#endif /* SOLARIS_UUIDSYS_SYSCALL */
DECL_TEMPLATE(solaris, sys_mmapobj);
DECL_TEMPLATE(solaris, sys_memcntl);
DECL_TEMPLATE(solaris, sys_getpmsg);
DECL_TEMPLATE(solaris, sys_putpmsg);
#if defined(SOLARIS_OLD_SYSCALLS)
DECL_TEMPLATE(solaris, sys_rename);
#endif /* SOLARIS_OLD_SYSCALLS */
DECL_TEMPLATE(solaris, sys_uname);
DECL_TEMPLATE(solaris, sys_setegid);
DECL_TEMPLATE(solaris, sys_sysconfig);
DECL_TEMPLATE(solaris, sys_systeminfo);
DECL_TEMPLATE(solaris, sys_seteuid);
DECL_TEMPLATE(solaris, sys_forksys);
#if defined(SOLARIS_GETRANDOM_SYSCALL)
DECL_TEMPLATE(solaris, sys_getrandom);
#endif /* SOLARIS_GETRANDOM_SYSCALL */
DECL_TEMPLATE(solaris, sys_sigtimedwait);
DECL_TEMPLATE(solaris, sys_yield);
DECL_TEMPLATE(solaris, sys_lwp_sema_post);
DECL_TEMPLATE(solaris, sys_lwp_sema_trywait);
DECL_TEMPLATE(solaris, sys_lwp_detach);
DECL_TEMPLATE(solaris, sys_modctl);
DECL_TEMPLATE(solaris, sys_fchroot);
#if defined(SOLARIS_SYSTEM_STATS_SYSCALL)
DECL_TEMPLATE(solaris, sys_system_stats);
#endif /* SOLARIS_SYSTEM_STATS_SYSCALL */
DECL_TEMPLATE(solaris, sys_gettimeofday);
DECL_TEMPLATE(solaris, sys_lwp_create);
DECL_TEMPLATE(solaris, sys_lwp_exit);
DECL_TEMPLATE(solaris, sys_lwp_suspend);
DECL_TEMPLATE(solaris, sys_lwp_continue);
#if defined(SOLARIS_LWP_SIGQUEUE_SYSCALL)
DECL_TEMPLATE(solaris, sys_lwp_sigqueue);
#else
DECL_TEMPLATE(solaris, sys_lwp_kill);
#endif /* SOLARIS_LWP_SIGQUEUE_SYSCALL */
DECL_TEMPLATE(solaris, sys_lwp_self);
DECL_TEMPLATE(solaris, sys_lwp_sigmask);
DECL_TEMPLATE(solaris, sys_lwp_private);
DECL_TEMPLATE(solaris, sys_lwp_wait);
DECL_TEMPLATE(solaris, sys_lwp_mutex_wakeup);
DECL_TEMPLATE(solaris, sys_lwp_cond_wait);
DECL_TEMPLATE(solaris, sys_lwp_cond_signal);
DECL_TEMPLATE(solaris, sys_lwp_cond_broadcast);
DECL_TEMPLATE(solaris, sys_pread);
DECL_TEMPLATE(solaris, sys_pwrite);
DECL_TEMPLATE(solaris, sys_lgrpsys);
DECL_TEMPLATE(solaris, sys_rusagesys);
DECL_TEMPLATE(solaris, sys_port);
DECL_TEMPLATE(solaris, sys_pollsys);
DECL_TEMPLATE(solaris, sys_labelsys);
DECL_TEMPLATE(solaris, sys_acl);
DECL_TEMPLATE(solaris, sys_auditsys);
DECL_TEMPLATE(solaris, sys_p_online);
DECL_TEMPLATE(solaris, sys_sigqueue);
DECL_TEMPLATE(solaris, sys_clock_gettime);
DECL_TEMPLATE(solaris, sys_clock_settime);
DECL_TEMPLATE(solaris, sys_clock_getres);
DECL_TEMPLATE(solaris, sys_timer_create);
DECL_TEMPLATE(solaris, sys_timer_delete);
DECL_TEMPLATE(solaris, sys_timer_settime);
DECL_TEMPLATE(solaris, sys_timer_gettime);
DECL_TEMPLATE(solaris, sys_timer_getoverrun);
DECL_TEMPLATE(solaris, sys_facl);
DECL_TEMPLATE(solaris, sys_door);
DECL_TEMPLATE(solaris, sys_schedctl);
DECL_TEMPLATE(solaris, sys_pset);
DECL_TEMPLATE(solaris, sys_resolvepath);
DECL_TEMPLATE(solaris, sys_lwp_mutex_timedlock);
DECL_TEMPLATE(solaris, sys_lwp_rwlock_sys);
DECL_TEMPLATE(solaris, sys_lwp_sema_timedwait);
DECL_TEMPLATE(solaris, sys_zone);
DECL_TEMPLATE(solaris, sys_getcwd);
DECL_TEMPLATE(solaris, sys_so_socket);
DECL_TEMPLATE(solaris, sys_so_socketpair);
DECL_TEMPLATE(solaris, sys_bind);
DECL_TEMPLATE(solaris, sys_listen);
DECL_TEMPLATE(solaris, sys_accept);
DECL_TEMPLATE(solaris, sys_connect);
DECL_TEMPLATE(solaris, sys_shutdown);
DECL_TEMPLATE(solaris, sys_recv);
DECL_TEMPLATE(solaris, sys_recvfrom);
DECL_TEMPLATE(solaris, sys_recvmsg);
DECL_TEMPLATE(solaris, sys_send);
DECL_TEMPLATE(solaris, sys_sendmsg);
DECL_TEMPLATE(solaris, sys_sendto);
DECL_TEMPLATE(solaris, sys_getpeername);
DECL_TEMPLATE(solaris, sys_getsockname);
DECL_TEMPLATE(solaris, sys_getsockopt);
DECL_TEMPLATE(solaris, sys_setsockopt);
DECL_TEMPLATE(solaris, sys_lwp_mutex_unlock);
DECL_TEMPLATE(solaris, sys_lwp_mutex_register);
DECL_TEMPLATE(solaris, sys_uucopy);
DECL_TEMPLATE(solaris, sys_umount2);

DECL_TEMPLATE(solaris, fast_gethrtime);
DECL_TEMPLATE(solaris, fast_gethrvtime);
DECL_TEMPLATE(solaris, fast_gethrestime);
DECL_TEMPLATE(solaris, fast_getlgrp);
#if defined(SOLARIS_GETHRT_FASTTRAP)
DECL_TEMPLATE(solaris, fast_gethrt);
#endif /* SOLARIS_GETHRT_FASTTRAP */
#if defined(SOLARIS_GETZONEOFFSET_FASTTRAP)
DECL_TEMPLATE(solaris, fast_getzoneoffset);
#endif /* SOLARIS_GETZONEOFFSET_FASTTRAP */

/* implementation */
PRE(sys_exit)
{
   /* void exit(int status); */
   ThreadId t;

   PRINT("sys_exit( %ld )", SARG1);
   PRE_REG_READ1(void, "exit", int, status);

   for (t = 1; t < VG_N_THREADS; t++) {
      if (VG_(threads)[t].status == VgTs_Empty)
         continue;

      /* Assign the exit code, VG_(nuke_all_threads_except) will assign
         the exitreason. */
      VG_(threads)[t].os_state.exitcode = ARG1;
   }

   /* Indicate in all other threads that the process is exiting.
      Then wait using VG_(reap_threads) for these threads to disappear.
      See comments in syswrap-linux.c, PRE(sys_exit_group) wrapper,
      for reasoning why this cannot give a deadlock. */
   VG_(nuke_all_threads_except)(tid, VgSrc_ExitProcess);
   VG_(reap_threads)(tid);
   VG_(threads)[tid].exitreason = VgSrc_ExitThread;
   /* We do assign VgSrc_ExitThread and not VgSrc_ExitProcess, as this thread
      is the thread calling exit_group and so its registers must be considered
      as not reachable. See pub_tool_machine.h VG_(apply_to_GP_regs). */

   /* We have to claim the syscall already succeeded. */
   SET_STATUS_Success(0);
}

#if defined(SOLARIS_SPAWN_SYSCALL)
static Bool spawn_pre_check_kfa(ThreadId tid, SyscallStatus *status,
                                vki_kfile_attr_t *kfa)
{
   PRE_FIELD_READ("spawn(attrs->kfa_size)", kfa->kfa_size);
   PRE_FIELD_READ("spawn(attrs->kfa_type)", kfa->kfa_type);

   if (ML_(safe_to_deref)(kfa, kfa->kfa_size)) {
      switch (kfa->kfa_type) {
      case VKI_FA_DUP2:
         PRE_FIELD_READ("spawn(attrs->kfa_filedes)", kfa->kfa_filedes);
         PRE_FIELD_READ("spawn(attrs->kfa_newfiledes)", kfa->kfa_newfiledes);
         if (!ML_(fd_allowed)(kfa->kfa_filedes, "spawn(dup2)", tid, False) ||
             !ML_(fd_allowed)(kfa->kfa_newfiledes, "spawn(dup2)", tid, False)) {
            SET_STATUS_Failure(VKI_EBADF);
            return False;
         }
         break;
      case VKI_FA_CLOSE:
         PRE_FIELD_READ("spawn(attrs->kfa_filedes)", kfa->kfa_filedes);
         /* If doing -d style logging (which is to fd = 2 = stderr),
            don't allow that filedes to be closed. See ML_(fd_allowed)(). */
         if (!ML_(fd_allowed)(kfa->kfa_filedes, "spawn(close)", tid, False) ||
             (kfa->kfa_filedes == 2 && VG_(debugLog_getLevel)() > 0)) {
            SET_STATUS_Failure(VKI_EBADF);
            return False;
         }
         break;
      case VKI_FA_CLOSEFROM:
         /* :TODO: All file descriptors greater than or equal to
            kfa->kfa_filedes would have to be checked. */
         VG_(unimplemented)("Support for spawn() with file attribute type "
                            "FA_CLOSEFROM.");
         break;
      case VKI_FA_OPEN:
         PRE_FIELD_READ("spawn(attrs->kfa_filedes)", kfa->kfa_filedes);
         PRE_FIELD_READ("spawn(attrs->kfa_oflag)", kfa->kfa_oflag);
         PRE_FIELD_READ("spawn(attrs->kfa_mode)", kfa->kfa_mode);
         if (!ML_(fd_allowed)(kfa->kfa_filedes, "spawn(open)", tid, False)) {
            SET_STATUS_Failure(VKI_EBADF);
            return False;
         }
         /* fallthrough */
      case VKI_FA_CHDIR:
         PRE_FIELD_READ("spawn(attrs->kfa_pathsize)", kfa->kfa_pathsize);
         if (kfa->kfa_pathsize != 0) {
            PRE_MEM_RASCIIZ("spawn(attrs->kfa_data)", (Addr) kfa->kfa_data);
         }
         break;
      default:
         VG_(unimplemented)("Support for spawn() with file attribute type %u.",
                            kfa->kfa_type);
      }
   }

   return True;
}

PRE(sys_spawn)
{
   /* int spawn(char *path, void *attrs, size_t attrsize,
                char *argenv, size_t aesize); */
   PRINT("sys_spawn ( %#lx(%s), %#lx, %lu, %#lx, %lu )",
         ARG1, (HChar *) ARG1, ARG2, ARG3, ARG4, ARG5);
   PRE_REG_READ5(long, "spawn", const char *, path, void *, attrs,
                 size_t, attrsize, char *, argenv, size_t, aesize);

   /* First check input arguments. */
   PRE_MEM_RASCIIZ("spawn(path)", ARG1);
   if (ARG3 > 0) {
      /*  --- vki_kspawn_attr_t --
          | ksa_version          |
          | ksa_size             |
          | ksa_attr_off         |  -----| (only if != 0)
          | ksa_attr_size        |       |
          | ksa_path_off         |  =====|====| (only if != 0)
          | ksa_path_size        |       |    |
          | ksa_shell_off        |  -----|----|----| (only if != 0)
          | ksa_shell_size       |       |    |    |
          | ksa_data[0]          |       |    |    |
          ------------------------       |    |    |
          | vki_spawn_attr_t     |  <----|    |    |
          ------------------------            |    |
          | path                 |  <---------|    |
          ------------------------                 |
          | shell                |  <---------------
          ------------------------
          | file actions         |  (not included in ksa_size, only in ARG3)
          ------------------------

          ksa_size = sizeof(vki_kspawn_attr_t) + ksa_attr_size + ksa_path_size +
                     ksa_shell_size
          attrs_size (ARG3) = ksa_size + file actions size */

      vki_kspawn_attr_t *attrs = (vki_kspawn_attr_t *) ARG2;
      PRE_FIELD_READ("spawn(attrs->ksa_version)", attrs->ksa_version);
      PRE_FIELD_READ("spawn(attrs->ksa_size)", attrs->ksa_size);
      PRE_FIELD_READ("spawn(attrs->ksa_attr_off)", attrs->ksa_attr_off);
      PRE_FIELD_READ("spawn(attrs->ksa_path_off)", attrs->ksa_path_off);
      PRE_FIELD_READ("spawn(attrs->ksa_shell_off)", attrs->ksa_shell_off);

      if (ML_(safe_to_deref)(attrs, sizeof(vki_kspawn_attr_t))) {
         if (attrs->ksa_version != VKI_SPAWN_VERSION) {
            VG_(unimplemented)("Support for spawn() with attributes "
                               "version %u.", attrs->ksa_version);
         }

         if (attrs->ksa_attr_off != 0) {
            PRE_FIELD_READ("spawn(attrs->ksa_attr_size)", attrs->ksa_attr_size);
            vki_spawn_attr_t *sap =
                (vki_spawn_attr_t *) ((Addr) attrs + attrs->ksa_attr_off);
            PRE_MEM_READ("spawn(attrs->ksa_attr)",
                         (Addr) sap, attrs->ksa_attr_size);
            if (ML_(safe_to_deref)(sap, sizeof(vki_spawn_attr_t))) {
               if (sap->sa_psflags & VKI_POSIX_SPAWN_SETVAMASK_NP) {
                  VG_(unimplemented)("Support for spawn() with attributes flag "
                                     "including POSIX_SPAWN_SETVAMASK_NP.");
               }
               /* paranoia */
               Int rem = sap->sa_psflags & ~(
                  VKI_POSIX_SPAWN_RESETIDS      | VKI_POSIX_SPAWN_SETPGROUP |
                  VKI_POSIX_SPAWN_SETSIGDEF     | VKI_POSIX_SPAWN_SETSIGMASK |
                  VKI_POSIX_SPAWN_SETSCHEDPARAM | VKI_POSIX_SPAWN_SETSCHEDULER |
                  VKI_POSIX_SPAWN_SETSID_NP     | VKI_POSIX_SPAWN_SETVAMASK_NP |
                  VKI_POSIX_SPAWN_SETSIGIGN_NP  | VKI_POSIX_SPAWN_NOSIGCHLD_NP |
                  VKI_POSIX_SPAWN_WAITPID_NP    | VKI_POSIX_SPAWN_NOEXECERR_NP);
               if (rem != 0) {
                  VG_(unimplemented)("Support for spawn() with attributes flag "
                                     "%#x.", sap->sa_psflags);
               }
            }
         }

         if (attrs->ksa_path_off != 0) {
            PRE_FIELD_READ("spawn(attrs->ksa_path_size)", attrs->ksa_path_size);
            PRE_MEM_RASCIIZ("spawn(attrs->ksa_path)",
                            (Addr) attrs + attrs->ksa_path_off);
         }

         if (attrs->ksa_shell_off != 0) {
            PRE_FIELD_READ("spawn(attrs->ksa_shell_size)",
                           attrs->ksa_shell_size);
            PRE_MEM_RASCIIZ("spawn(attrs->ksa_shell)",
                            (Addr) attrs + attrs->ksa_shell_off);
         }

         vki_kfile_attr_t *kfa = (vki_kfile_attr_t *) (ARG2 + attrs->ksa_size);
         while ((Addr) kfa < ARG2 + ARG3) {
            if (spawn_pre_check_kfa(tid, status, kfa) == False) {
               return;
            }
            kfa = (vki_kfile_attr_t *) ((Addr) kfa + kfa->kfa_size);
         }
      }
   }
   PRE_MEM_READ("spawn(argenv)", ARG4, ARG5);

   /* Check that the name at least begins in client-accessible storage. */
   if ((ARG1 == 0) || !ML_(safe_to_deref)((HChar *) ARG1, 1)) {
      SET_STATUS_Failure(VKI_EFAULT);
      return;
   }

   /* Check that attrs reside in client-accessible storage. */
   if (ARG2 != 0) {
      if (!VG_(am_is_valid_for_client)(ARG2, ARG3, VKI_PROT_READ)) {
         SET_STATUS_Failure(VKI_EFAULT);
         return;
      }
   }

   /* Check that the argenv reside in client-accessible storage.
      Solaris disallows to perform spawn() without any arguments & environment
      variables specified. */
   if ((ARG4 == 0) /* obviously bogus */ ||
       !VG_(am_is_valid_for_client)(ARG4, ARG5, VKI_PROT_READ)) {
      SET_STATUS_Failure(VKI_EFAULT);
      return;
   }

   /* Copy existing attrs or create empty minimal ones. */
   vki_kspawn_attr_t *attrs;
   SizeT attrs_size;
   if (ARG2 == 0) {
      /* minimalistic kspawn_attr_t + spawn_attr_t */
      attrs_size = sizeof(vki_kspawn_attr_t) + sizeof(vki_spawn_attr_t);
      attrs = VG_(calloc)("syswrap.spawn.1", 1, attrs_size);
      attrs->ksa_version = VKI_SPAWN_VERSION;
      attrs->ksa_size = attrs_size;
      attrs->ksa_attr_off = sizeof(vki_kspawn_attr_t);
      attrs->ksa_attr_size = sizeof(vki_spawn_attr_t);
   } else if (((vki_kspawn_attr_t *) ARG2)->ksa_attr_off == 0) {
      /* existing kspawn_attr_t but missing spawn_attr_t */
      attrs_size = ARG3 + sizeof(vki_spawn_attr_t);
      attrs = VG_(calloc)("syswrap.spawn.2", 1, attrs_size);
      VG_(memcpy)(attrs, (void *) ARG2, sizeof(vki_kspawn_attr_t));
      SizeT file_actions_size = ARG3 - attrs->ksa_size;
      attrs->ksa_size += sizeof(vki_spawn_attr_t);
      attrs->ksa_attr_off = sizeof(vki_kspawn_attr_t);
      attrs->ksa_attr_size = sizeof(vki_spawn_attr_t);
      if (attrs->ksa_path_off != 0) {
         VG_(memcpy)((HChar *) attrs + attrs->ksa_path_off +
                     sizeof(vki_spawn_attr_t), (HChar *) ARG2 +
                     attrs->ksa_path_off, attrs->ksa_path_size);
         attrs->ksa_path_off += sizeof(vki_spawn_attr_t);
      }
      if (attrs->ksa_shell_off != 0) {
         VG_(memcpy)((HChar *) attrs + attrs->ksa_shell_off +
                     sizeof(vki_spawn_attr_t), (HChar *) ARG2 +
                     attrs->ksa_shell_off, attrs->ksa_shell_size);
         attrs->ksa_shell_off += sizeof(vki_spawn_attr_t);
      }
      if (file_actions_size > 0) {
         VG_(memcpy)((HChar *) attrs + attrs_size - file_actions_size,
                     (HChar *) ARG2 + ARG3 - file_actions_size,
                     file_actions_size);
      }
   } else {
      /* existing kspawn_attr_t + spawn_attr_t */
      attrs_size = ARG3;
      attrs = VG_(malloc)("syswrap.spawn.3", attrs_size);
      VG_(memcpy)(attrs, (void *) ARG2, attrs_size);
   }
   vki_spawn_attr_t *spa = (vki_spawn_attr_t *) ((HChar *) attrs +
                                                 attrs->ksa_attr_off);

   /* Convert argv and envp parts of argenv into their separate XArray's.
      Duplicate strings because argv and envp will be then modified. */
   XArray *argv = VG_(newXA)(VG_(malloc), "syswrap.spawn.4",
                             VG_(free), sizeof(HChar *));
   XArray *envp = VG_(newXA)(VG_(malloc), "syswrap.spawn.5",
                             VG_(free), sizeof(HChar *));

   HChar *argenv = (HChar *) ARG4;
   XArray *current_xa = argv;
   while ((Addr) argenv < ARG4 + ARG5) {
      if (*argenv == '\0') {
         argenv += 1;
         if (current_xa == argv) {
            current_xa = envp;
            if ((*argenv == '\0') && ((Addr) argenv == ARG4 + ARG5 - 1)) {
               /* envp part is empty, it contained only {NULL}. */
               break;
            }
         } else {
            if ((Addr) argenv != ARG4 + ARG5) {
               if (VG_(clo_trace_syscalls))
                  VG_(debugLog)(3, "syswrap-solaris", "spawn: bogus argenv\n");
               SET_STATUS_Failure(VKI_EINVAL);
               goto exit;
            }
            break;
         }
      }

      if (*argenv != '\1') {
         if (VG_(clo_trace_syscalls))
            VG_(debugLog)(3, "syswrap-solaris", "spawn: bogus argenv\n");
         SET_STATUS_Failure(VKI_EINVAL);
         goto exit;
      }
      argenv += 1;

      HChar *duplicate = VG_(strdup)("syswrap.spawn.6", argenv);
      VG_(addToXA)(current_xa, &duplicate);
      argenv += VG_(strlen)(argenv) + 1;
   }

   /* Debug-only printing. */
   if (0) {
      VG_(printf)("\nARG1 = %#lx(%s)\n", ARG1, (HChar *) ARG1);
      VG_(printf)("ARG4 (argv) = ");
      for (Word i = 0; i < VG_(sizeXA)(argv); i++) {
         VG_(printf)("%s ", *(HChar **) VG_(indexXA)(argv, i));
      }

      VG_(printf)("\nARG4 (envp) = ");
      for (Word i = 0; i < VG_(sizeXA)(envp); i++) {
         VG_(printf)("%s ", *(HChar **) VG_(indexXA)(envp, i));
      }
      VG_(printf)("\n");
   }

   /* Decide whether or not we want to trace the spawned child.
      Omit the executable name itself from child_argv. */
   const HChar **child_argv = VG_(malloc)("syswrap.spawn.7",
                                     (VG_(sizeXA)(argv) - 1) * sizeof(HChar *));
   for (Word i = 1; i < VG_(sizeXA)(argv); i++) {
      child_argv[i - 1] = *(HChar **) VG_(indexXA)(argv, i);
   }
   Bool trace_this_child = VG_(should_we_trace_this_child)((HChar *) ARG1,
                                                           child_argv);
   VG_(free)(child_argv);

   /* If we're tracing the child, and the launcher name looks bogus (possibly
      because launcher.c couldn't figure it out, see comments therein) then we
      have no option but to fail. */
   if (trace_this_child &&
       (!VG_(name_of_launcher) || VG_(name_of_launcher)[0] != '/')) {
      SET_STATUS_Failure(VKI_ECHILD); /* "No child processes." */
      goto exit;
   }

   /* Set up the child's exe path. */
   const HChar *path = (const HChar *) ARG1;
   const HChar *launcher_basename = NULL;
   if (trace_this_child) {
      /* We want to exec the launcher. */
      path = VG_(name_of_launcher);
      vg_assert(path != NULL);

      launcher_basename = VG_(strrchr)(path, '/');
      if ((launcher_basename == NULL) || (launcher_basename[1] == '\0')) {
         launcher_basename = path;  /* hmm, tres dubious */
      } else {
         launcher_basename++;
      }
   }

   /* Set up the child's environment.

      Remove the valgrind-specific stuff from the environment so the child
      doesn't get vgpreload_core.so, vgpreload_<tool>.so, etc. This is done
      unconditionally, since if we are tracing the child, the child valgrind
      will set up the appropriate client environment.

      Then, if tracing the child, set VALGRIND_LIB for it. */
   HChar **child_envp = VG_(calloc)("syswrap.spawn.8",
                                    VG_(sizeXA)(envp) + 1, sizeof(HChar *));
   for (Word i = 0; i < VG_(sizeXA)(envp); i++) {
      child_envp[i] = *(HChar **) VG_(indexXA)(envp, i);
   }
   VG_(env_remove_valgrind_env_stuff)(child_envp, /* ro_strings */ False,
                                      VG_(free));

   /* Stuff was removed from child_envp, reflect that in envp XArray. */
   VG_(dropTailXA)(envp, VG_(sizeXA)(envp));
   for (UInt i = 0; child_envp[i] != NULL; i++) {
      VG_(addToXA)(envp, &child_envp[i]);
   }
   VG_(free)(child_envp);

   if (trace_this_child) {
      /* Set VALGRIND_LIB in envp. */
      SizeT len = VG_(strlen)(VALGRIND_LIB) + VG_(strlen)(VG_(libdir)) + 2;
      HChar *valstr = VG_(malloc)("syswrap.spawn.9", len);
      VG_(sprintf)(valstr, "%s=%s", VALGRIND_LIB, VG_(libdir));
      VG_(addToXA)(envp, &valstr);
   }

   /* Set up the child's args. If not tracing it, they are left untouched.
      Otherwise, they are:

      [launcher_basename] ++ VG_(args_for_valgrind) ++ [ARG1] ++ ARG4[1..],

      except that the first VG_(args_for_valgrind_noexecpass) args are
      omitted. */
   if (trace_this_child) {
      vg_assert(VG_(args_for_valgrind) != NULL);
      vg_assert(VG_(args_for_valgrind_noexecpass) >= 0);
      vg_assert(VG_(args_for_valgrind_noexecpass)
                   <= VG_(sizeXA)(VG_(args_for_valgrind)));

      /* So what args will there be? Bear with me... */
      /* ... launcher basename, ... */
      HChar *duplicate = VG_(strdup)("syswrap.spawn.10", launcher_basename);
      VG_(insertIndexXA)(argv, 0, &duplicate);

      /* ... Valgrind's args, ... */
      UInt v_args = VG_(sizeXA)(VG_(args_for_valgrind));
      v_args -= VG_(args_for_valgrind_noexecpass);
      for (Word i = VG_(args_for_valgrind_noexecpass);
           i < VG_(sizeXA)(VG_(args_for_valgrind)); i++) {
         duplicate = VG_(strdup)("syswrap.spawn.11",
                           *(HChar **) VG_(indexXA)(VG_(args_for_valgrind), i));
         VG_(insertIndexXA)(argv, 1 + i, &duplicate);
      }

      /* ... name of client executable, ... */
      duplicate = VG_(strdup)("syswrap.spawn.12", (HChar *) ARG1);
      VG_(insertIndexXA)(argv, 1 + v_args, &duplicate);

      /* ... and args for client executable (without [0]). */
      duplicate = *(HChar **) VG_(indexXA)(argv, 1 + v_args + 1);
      VG_(free)(duplicate);
      VG_(removeIndexXA)(argv, 1 + v_args + 1);
   }

   /* Debug-only printing. */
   if (0) {
      VG_(printf)("\npath = %s\n", path);
      VG_(printf)("argv = ");
      for (Word i = 0; i < VG_(sizeXA)(argv); i++) {
         VG_(printf)("%s ", *(HChar **) VG_(indexXA)(argv, i));
      }

      VG_(printf)("\nenvp = ");
      for (Word i = 0; i < VG_(sizeXA)(envp); i++) {
         VG_(printf)("%s ", *(HChar **) VG_(indexXA)(envp, i));
      }
      VG_(printf)("\n");
   }

   /* Set the signal state up for spawned child.

      Signals set to be caught are equivalent to signals set to the default
      action, from the child's perspective.

      Therefore query SCSS and prepare default (DFL) and ignore (IGN) signal
      sets. Then combine these sets with those passed from client, if flags
      POSIX_SPAWN_SETSIGDEF, or POSIX_SPAWN_SETSIGIGN_NP have been specified.
   */
   vki_sigset_t sig_default;
   vki_sigset_t sig_ignore;
   VG_(sigemptyset)(&sig_default);
   VG_(sigemptyset)(&sig_ignore);
   for (Int i = 1; i < VG_(max_signal); i++) {
      vki_sigaction_fromK_t sa;
      VG_(do_sys_sigaction)(i, NULL, &sa); /* query SCSS */
      if (sa.sa_handler == VKI_SIG_IGN) {
         VG_(sigaddset)(&sig_ignore, i);
      } else {
         VG_(sigaddset)(&sig_default, i);
      }
   }

   if (spa->sa_psflags & VKI_POSIX_SPAWN_SETSIGDEF) {
      VG_(sigaddset_from_set)(&spa->sa_sigdefault, &sig_default);
   } else {
      spa->sa_psflags |= VKI_POSIX_SPAWN_SETSIGDEF;
      spa->sa_sigdefault = sig_default;
   }

   if (spa->sa_psflags & VKI_POSIX_SPAWN_SETSIGIGN_NP) {
      VG_(sigaddset_from_set)(&spa->sa_sigignore, &sig_ignore);
   } else {
      spa->sa_psflags |= VKI_POSIX_SPAWN_SETSIGIGN_NP;
      spa->sa_sigignore = sig_ignore;
   }

   /* Set the signal mask for spawned child.

      Analogous to signal handlers: query SCSS for blocked signals mask
      and combine this mask with that passed from client, if flag
      POSIX_SPAWN_SETSIGMASK has been specified. */
   vki_sigset_t *sigmask = &VG_(get_ThreadState)(tid)->sig_mask;
   if (spa->sa_psflags & VKI_POSIX_SPAWN_SETSIGMASK) {
      VG_(sigaddset_from_set)(&spa->sa_sigmask, sigmask);
   } else {
      spa->sa_psflags |= VKI_POSIX_SPAWN_SETSIGMASK;
      spa->sa_sigmask = *sigmask;
   }

   /* Lastly, reconstruct argenv from argv + envp. */
   SizeT argenv_size = 1 + 1;
   for (Word i = 0; i < VG_(sizeXA)(argv); i++) {
      argenv_size += VG_(strlen)(*(HChar **) VG_(indexXA)(argv, i)) + 2;
   }
   for (Word i = 0; i < VG_(sizeXA)(envp); i++) {
      argenv_size += VG_(strlen)(*(HChar **) VG_(indexXA)(envp, i)) + 2;
   }

   argenv = VG_(malloc)("syswrap.spawn.13", argenv_size);
   HChar *current = argenv;
#define COPY_CHAR_TO_ARGENV(dst, character) \
   do {                                     \
      *(dst) = character;                   \
      (dst) += 1;                           \
   } while (0)
#define COPY_STRING_TO_ARGENV(dst, src)       \
   do {                                       \
      COPY_CHAR_TO_ARGENV(dst, '\1');         \
      SizeT src_len = VG_(strlen)((src)) + 1; \
      VG_(memcpy)((dst), (src), src_len);     \
      (dst) += src_len;                       \
   } while (0)

   for (Word i = 0; i < VG_(sizeXA)(argv); i++) {
      COPY_STRING_TO_ARGENV(current, *(HChar **) VG_(indexXA)(argv, i));
   }
   COPY_CHAR_TO_ARGENV(current, '\0');
   for (Word i = 0; i < VG_(sizeXA)(envp); i++) {
      COPY_STRING_TO_ARGENV(current, *(HChar **) VG_(indexXA)(envp, i));
   }
   COPY_CHAR_TO_ARGENV(current, '\0');
   vg_assert(current == argenv + argenv_size);
#undef COPY_CHAR_TO_ARGENV
#undef COPY_STRING_TOARGENV

   /* Actual spawn() syscall. */
   SysRes res = VG_(do_syscall5)(__NR_spawn, (UWord) path, (UWord) attrs,
                                 attrs_size, (UWord) argenv, argenv_size);
   SET_STATUS_from_SysRes(res);
   VG_(free)(argenv);

   if (SUCCESS) {
      PRINT("   spawn: process %d spawned child %ld\n", VG_(getpid)(), RES);
   }

exit:
   VG_(free)(attrs);
   for (Word i = 0; i < VG_(sizeXA)(argv); i++) {
      VG_(free)(*(HChar **) VG_(indexXA)(argv, i));
   }
   for (Word i = 0; i < VG_(sizeXA)(envp); i++) {
      VG_(free)(*(HChar **) VG_(indexXA)(envp, i));
   }
   VG_(deleteXA)(argv);
   VG_(deleteXA)(envp);
}
#endif /* SOLARIS_SPAWN_SYSCALL */

/* Handles the case where the open is of /proc/self/psinfo or
   /proc/<pid>/psinfo. Fetch fresh contents into psinfo_t,
   fake fname, psargs, argc and argv. Write the structure to the fake
   file we cooked up at startup (in m_main) and give out a copy of this
   fd. Also seek the cloned fd back to the start. */
static Bool handle_psinfo_open(SyscallStatus *status,
                               Bool use_openat,
                               const HChar *filename,
                               Int arg1, UWord arg3, UWord arg4)
{
   if (!ML_(safe_to_deref)((const void *) filename, 1))
      return False;

   HChar name[VKI_PATH_MAX];    // large enough
   VG_(sprintf)(name, "/proc/%d/psinfo", VG_(getpid)());

   if (!VG_STREQ(filename, name) && !VG_STREQ(filename, "/proc/self/psinfo"))
      return False;

   /* Use original arguments to open() or openat(). */
   SysRes sres;
#if defined(SOLARIS_OLD_SYSCALLS)
   if (use_openat)
      sres = VG_(do_syscall4)(SYS_openat, arg1, (UWord) filename,
                              arg3, arg4);
   else
      sres = VG_(do_syscall3)(SYS_open, (UWord) filename, arg3, arg4);
#else
   vg_assert(use_openat == True);
   sres = VG_(do_syscall4)(SYS_openat, arg1, (UWord) filename,
                           arg3, arg4);
#endif /* SOLARIS_OLD_SYSCALLS */

   if (sr_isError(sres)) {
      SET_STATUS_from_SysRes(sres);
      return True;
   }
   Int fd = sr_Res(sres);

   vki_psinfo_t psinfo;
   sres = VG_(do_syscall3)(SYS_read, fd, (UWord) &psinfo, sizeof(psinfo));
   if (sr_isError(sres)) {
      SET_STATUS_from_SysRes(sres);
      VG_(close)(fd);
      return True;
   }
   if (sr_Res(sres) != sizeof(psinfo)) {
      SET_STATUS_Failure(VKI_ENODATA);
      VG_(close)(fd);
      return True;
   }

   VG_(close)(fd);

   VG_(client_fname)(psinfo.pr_fname, sizeof(psinfo.pr_fname), True);
   VG_(client_cmd_and_args)(psinfo.pr_psargs, sizeof(psinfo.pr_psargs));

   Addr *ptr = (Addr *) VG_(get_initial_client_SP)();
   psinfo.pr_argc = *ptr++;
   psinfo.pr_argv = (Addr) ptr;

   sres = VG_(do_syscall4)(SYS_pwrite, VG_(cl_psinfo_fd),
                           (UWord) &psinfo, sizeof(psinfo), 0);
   if (sr_isError(sres)) {
      SET_STATUS_from_SysRes(sres);
      return True;
   }

   sres = VG_(dup)(VG_(cl_psinfo_fd));
   SET_STATUS_from_SysRes(sres);
   if (!sr_isError(sres)) {
      OffT off = VG_(lseek)(sr_Res(sres), 0, VKI_SEEK_SET);
      if (off < 0)
         SET_STATUS_Failure(VKI_EMFILE);
   }

   return True;
}

#if defined(SOLARIS_PROC_CMDLINE)
/* Handles the case where the open is of /proc/self/cmdline or
   /proc/<pid>/cmdline. Just give it a copy of VG_(cl_cmdline_fd) for the
   fake file we cooked up at startup (in m_main).  Also, seek the
   cloned fd back to the start. */
static Bool handle_cmdline_open(SyscallStatus *status, const HChar *filename)
{
   if (!ML_(safe_to_deref)((const void *) filename, 1))
      return False;

   HChar name[VKI_PATH_MAX];    // large enough
   VG_(sprintf)(name, "/proc/%d/cmdline", VG_(getpid)());

   if (!VG_STREQ(filename, name) && !VG_STREQ(filename, "/proc/self/cmdline"))
      return False;

   SysRes sres = VG_(dup)(VG_(cl_cmdline_fd));
   SET_STATUS_from_SysRes(sres);
   if (!sr_isError(sres)) {
      OffT off = VG_(lseek)(sr_Res(sres), 0, VKI_SEEK_SET);
      if (off < 0)
         SET_STATUS_Failure(VKI_EMFILE);
   }

   return True;
}
#endif /* SOLARIS_PROC_CMDLINE */


#if defined(SOLARIS_OLD_SYSCALLS)
PRE(sys_open)
{
   /* int open(const char *filename, int flags);
      int open(const char *filename, int flags, mode_t mode); */

   if (ARG2 & VKI_O_CREAT) {
      /* 3-arg version */
      PRINT("sys_open ( %#lx(%s), %ld, %lu )", ARG1, (HChar *) ARG1,
            SARG2, ARG3);
      PRE_REG_READ3(long, "open", const char *, filename,
                    int, flags, vki_mode_t, mode);
   } else {
      /* 2-arg version */
      PRINT("sys_open ( %#lx(%s), %ld )", ARG1, (HChar *) ARG1, SARG2);
      PRE_REG_READ2(long, "open", const char *, filename, int, flags);
   }

   PRE_MEM_RASCIIZ("open(filename)", ARG1);

   if (ML_(handle_auxv_open)(status, (const HChar*)ARG1, ARG2))
      return;

   if (handle_psinfo_open(status, False /*use_openat*/, (const HChar*)ARG1, 0,
                          ARG2, ARG3))
      return;

   *flags |= SfMayBlock;
}

POST(sys_open)
{
   if (!ML_(fd_allowed)(RES, "open", tid, True)) {
      VG_(close)(RES);
      SET_STATUS_Failure(VKI_EMFILE);
   } else if (VG_(clo_track_fds))
      ML_(record_fd_open_with_given_name)(tid, RES, (HChar *) ARG1);
}
#endif /* SOLARIS_OLD_SYSCALLS */

PRE(sys_close)
{
   WRAPPER_PRE_NAME(generic, sys_close)(tid, layout, arrghs, status,
                                        flags);
}

POST(sys_close)
{
   WRAPPER_POST_NAME(generic, sys_close)(tid, arrghs, status);
   door_record_revoke(tid, ARG1);
   /* Possibly an explicitly open'ed client door fd was just closed.
      Generic sys_close wrapper calls this only if VG_(clo_track_fds) = True. */
   if (!VG_(clo_track_fds))
      ML_(record_fd_close)(ARG1);
}

PRE(sys_linkat)
{
   /* int linkat(int fd1, const char *path1, int fd2,
                 const char *path2, int flag);
    */

   /* Interpret the first and third arguments as 32-bit values even on 64-bit
      architecture. This is different from Linux, for example, where glibc
      sign-extends them. */
   Int fd1 = (Int) ARG1;
   Int fd2 = (Int) ARG3;

   PRINT("sys_linkat ( %d, %#lx(%s), %d, %#lx(%s), %ld )",
         fd1, ARG2, (HChar *) ARG2, fd2, ARG4, (HChar *) ARG4, SARG5);
   PRE_REG_READ5(long, "linkat", int, fd1, const char *, path1,
                 int, fd2, const char *, path2, int, flags);
   PRE_MEM_RASCIIZ("linkat(path1)", ARG2);
   PRE_MEM_RASCIIZ("linkat(path2)", ARG4);

   /* Be strict but ignore fd1/fd2 for absolute path1/path2. */
   if (fd1 != VKI_AT_FDCWD
       && ML_(safe_to_deref)((void *) ARG2, 1)
       && ((HChar *) ARG2)[0] != '/'
       && !ML_(fd_allowed)(fd1, "linkat", tid, False)) {
      SET_STATUS_Failure(VKI_EBADF);
   }
   if (fd2 != VKI_AT_FDCWD
       && ML_(safe_to_deref)((void *) ARG4, 1)
       && ((HChar *) ARG4)[0] != '/'
       && !ML_(fd_allowed)(fd2, "linkat", tid, False)) {
      SET_STATUS_Failure(VKI_EBADF);
   }

   *flags |= SfMayBlock;
}

PRE(sys_symlinkat)
{
   /* int symlinkat(const char *path1, int fd, const char *path2); */

   /* Interpret the second argument as 32-bit value even on 64-bit architecture.
      This is different from Linux, for example, where glibc sign-extends it. */
   Int fd = (Int) ARG2;

   PRINT("sys_symlinkat ( %#lx(%s), %d, %#lx(%s) )",
         ARG1, (HChar *) ARG1, fd, ARG3, (HChar *) ARG3);
   PRE_REG_READ3(long, "symlinkat", const char *, path1, int, fd,
                 const char *, path2);
   PRE_MEM_RASCIIZ("symlinkat(path1)", ARG1);
   PRE_MEM_RASCIIZ("symlinkat(path2)", ARG3);

   /* Be strict but ignore fd for absolute path2. */
   if (fd != VKI_AT_FDCWD
       && ML_(safe_to_deref)((void *) ARG3, 1)
       && ((HChar *) ARG3)[0] != '/'
       && !ML_(fd_allowed)(fd, "symlinkat", tid, False))
      SET_STATUS_Failure(VKI_EBADF);

   *flags |= SfMayBlock;
}

PRE(sys_time)
{
   /* time_t time(); */
   PRINT("sys_time ( )");
   PRE_REG_READ0(long, "time");
}

/* Data segment for brk (heap). It is an expandable anonymous mapping
   abutting a 1-page reservation. The data segment starts at VG_(brk_base)
   and runs up to VG_(brk_limit). None of these two values have to be
   page-aligned.
   Initial data segment is established (see initimg-solaris.c for rationale):
   - directly during client program image initialization,
   - or on demand when the executed program is the runtime linker itself,
     after it has loaded its target dynamic executable (see PRE(sys_mmapobj)),
     or when the first brk() syscall is made.

   Notable facts:
   - VG_(brk_base) is not page aligned; does not move
   - VG_(brk_limit) moves between [VG_(brk_base), data segment end]
   - data segment end is always page aligned
   - right after data segment end is 1-page reservation

            |      heap           | 1 page
     +------+------+--------------+-------+
     | BSS  | anon |   anon       | resvn |
     +------+------+--------------+-------+

            ^      ^        ^    ^
            |      |        |    |
            |      |        |    data segment end
            |      |        VG_(brk_limit) -- no alignment constraint
            |      brk_base_pgup -- page aligned
            VG_(brk_base) -- not page aligned -- does not move

   Because VG_(brk_base) is not page-aligned and is initially located within
   pre-established BSS (data) segment, special care has to be taken in the code
   below to handle this feature.

   Reservation segment is used to protect the data segment merging with
   a pre-existing segment. This should be no problem because address space
   manager ensures that requests for client address space are satisfied from
   the highest available addresses. However when memory is low, data segment
   can meet with mmap'ed objects and the reservation segment separates these.
   The page that contains VG_(brk_base) is already allocated by the program's
   loaded data segment. The brk syscall wrapper handles this special case. */

static Bool brk_segment_established = False;

/* Establishes initial data segment for brk (heap). */
Bool VG_(setup_client_dataseg)(void)
{
   /* Segment size is initially at least 1 MB and at most 8 MB. */
   SizeT m1 = 1024 * 1024;
   SizeT m8 = 8 * m1;
   SizeT initial_size = VG_(client_rlimit_data).rlim_cur;
   VG_(debugLog)(1, "syswrap-solaris", "Setup client data (brk) segment "
                                       "at %#lx\n", VG_(brk_base));
   if (initial_size < m1)
      initial_size = m1;
   if (initial_size > m8)
      initial_size = m8;
   initial_size = VG_PGROUNDUP(initial_size);

   Addr anon_start = VG_PGROUNDUP(VG_(brk_base));
   SizeT anon_size = VG_PGROUNDUP(initial_size);
   Addr resvn_start = anon_start + anon_size;
   SizeT resvn_size = VKI_PAGE_SIZE;

   vg_assert(VG_IS_PAGE_ALIGNED(anon_size));
   vg_assert(VG_IS_PAGE_ALIGNED(resvn_size));
   vg_assert(VG_IS_PAGE_ALIGNED(anon_start));
   vg_assert(VG_IS_PAGE_ALIGNED(resvn_start));
   vg_assert(VG_(brk_base) == VG_(brk_limit));

   /* Find the loaded data segment and remember its protection. */
   const NSegment *seg = VG_(am_find_nsegment)(VG_(brk_base) - 1);
   vg_assert(seg != NULL);
   UInt prot = (seg->hasR ? VKI_PROT_READ : 0)
             | (seg->hasW ? VKI_PROT_WRITE : 0)
             | (seg->hasX ? VKI_PROT_EXEC : 0);

   /* Try to create the data segment and associated reservation where
      VG_(brk_base) says. */
   Bool ok = VG_(am_create_reservation)(resvn_start, resvn_size, SmLower,
                                        anon_size);
   if (!ok) {
      /* That didn't work, we're hosed. */
      return False;
   }

   /* Map the data segment. */
   SysRes sres = VG_(am_mmap_anon_fixed_client)(anon_start, anon_size, prot);
   vg_assert(!sr_isError(sres));
   vg_assert(sr_Res(sres) == anon_start);

   brk_segment_established = True;
   return True;
}

/* Tell the tool about the client data segment and then kill it which will
   make it initially inaccessible/unaddressable. */
void VG_(track_client_dataseg)(ThreadId tid)
{
   const NSegment *seg = VG_(am_find_nsegment)(VG_PGROUNDUP(VG_(brk_base)));
   vg_assert(seg != NULL);
   vg_assert(seg->kind == SkAnonC);

   VG_TRACK(new_mem_brk, VG_(brk_base), seg->end + 1 - VG_(brk_base), tid);
   VG_TRACK(die_mem_brk, VG_(brk_base), seg->end + 1 - VG_(brk_base));
}

static void PRINTF_CHECK(1, 2)
possibly_complain_brk(const HChar *format, ...)
{
   static Bool alreadyComplained = False;
   if (!alreadyComplained) {
      alreadyComplained = True;
      if (VG_(clo_verbosity) > 0) {
         va_list vargs;
         va_start(vargs, format);
         VG_(vmessage)(Vg_UserMsg, format, vargs);
         va_end(vargs);
         VG_(umsg)("(See section Limitations in the user manual.)\n");
         VG_(umsg)("NOTE: further instances of this message will not be "
                   "shown.\n");
      }
   }
}

PRE(sys_brk)
{
   /* unsigned long brk(caddr_t end_data_segment); */
   /* The Solaris kernel returns 0 on success.
      In addition to this, brk(0) returns current data segment end.  This is
      very different from the Linux kernel, for example. */

   Addr old_brk_limit = VG_(brk_limit);
   /* If VG_(brk_base) is page-aligned then old_brk_base_pgup is equal to
      VG_(brk_base). */
   Addr old_brk_base_pgup = VG_PGROUNDUP(VG_(brk_base));
   Addr new_brk = ARG1;
   const NSegment *seg, *seg2;

   PRINT("sys_brk ( %#lx )", ARG1);
   PRE_REG_READ1(unsigned long, "brk", vki_caddr_t, end_data_segment);

   if (new_brk == 0) {
      /* brk(0) - specific to Solaris 11 only. */
      SET_STATUS_Success(old_brk_limit);
      return;
   }

   /* Handle some trivial cases. */
   if (new_brk == old_brk_limit) {
      SET_STATUS_Success(0);
      return;
   }
   if (new_brk < VG_(brk_base)) {
      /* Clearly impossible. */
      SET_STATUS_Failure(VKI_ENOMEM);
      return;
   }
   if (new_brk - VG_(brk_base) > VG_(client_rlimit_data).rlim_cur) {
      SET_STATUS_Failure(VKI_ENOMEM);
      return;
   }

   /* The brk base and limit must have been already set. */
   vg_assert(VG_(brk_base) != -1);
   vg_assert(VG_(brk_limit) != -1);

   if (!brk_segment_established) {
      /* Stay sane (because there should have been no brk activity yet). */
      vg_assert(VG_(brk_base) == VG_(brk_limit));

      if (!VG_(setup_client_dataseg)()) {
         possibly_complain_brk("Cannot map memory to initialize brk segment in "
                               "thread #%u at %#lx\n", tid, VG_(brk_base));
         SET_STATUS_Failure(VKI_ENOMEM);
         return;
      }

      VG_(track_client_dataseg)(tid);
   }

   if (new_brk < old_brk_limit) {
      /* Shrinking the data segment.  Be lazy and don't munmap the excess
         area. */
      if (old_brk_limit > old_brk_base_pgup) {
         /* Calculate new local brk (=MAX(new_brk, old_brk_base_pgup)). */
         Addr new_brk_local;
         if (new_brk < old_brk_base_pgup)
            new_brk_local = old_brk_base_pgup;
         else
            new_brk_local = new_brk;

         /* Find a segment at the beginning and at the end of the shrinked
            range. */
         seg = VG_(am_find_nsegment)(new_brk_local);
         seg2 = VG_(am_find_nsegment)(old_brk_limit - 1);
         vg_assert(seg);
         vg_assert(seg->kind == SkAnonC);
         vg_assert(seg2);
         vg_assert(seg == seg2);

         /* Discard any translations and zero-out the area. */
         if (seg->hasT)
            VG_(discard_translations)(new_brk_local,
                                      old_brk_limit - new_brk_local,
                                      "do_brk(shrink)");
        /* Since we're being lazy and not unmapping pages, we have to zero out
           the area, so that if the area later comes back into circulation, it
           will be filled with zeroes, as if it really had been unmapped and
           later remapped.  Be a bit paranoid and try hard to ensure we're not
           going to segfault by doing the write - check that segment is
           writable. */
         if (seg->hasW)
            VG_(memset)((void*)new_brk_local, 0, old_brk_limit - new_brk_local);
      }

      /* Fixup code if the VG_(brk_base) is not page-aligned. */
      if (new_brk < old_brk_base_pgup) {
         /* Calculate old local brk (=MIN(old_brk_limit, old_brk_base_up)). */
         Addr old_brk_local;
         if (old_brk_limit < old_brk_base_pgup)
            old_brk_local = old_brk_limit;
         else
            old_brk_local = old_brk_base_pgup;

         /* Find a segment at the beginning and at the end of the shrinked
            range. */
         seg = VG_(am_find_nsegment)(new_brk);
         seg2 = VG_(am_find_nsegment)(old_brk_local - 1);
         vg_assert(seg);
         vg_assert(seg2);
         vg_assert(seg == seg2);

         /* Discard any translations and zero-out the area. */
         if (seg->hasT)
            VG_(discard_translations)(new_brk, old_brk_local - new_brk,
                                      "do_brk(shrink)");
         if (seg->hasW)
            VG_(memset)((void*)new_brk, 0, old_brk_local - new_brk);
      }

      /* We are done, update VG_(brk_limit), tell the tool about the changes,
         and leave. */
      VG_(brk_limit) = new_brk;
      VG_TRACK(die_mem_brk, new_brk, old_brk_limit - new_brk);
      SET_STATUS_Success(0);
      return;
   }

   /* We are expanding the brk segment. */

   /* Fixup code if the VG_(brk_base) is not page-aligned. */
   if (old_brk_limit < old_brk_base_pgup) {
      /* Calculate new local brk (=MIN(new_brk, old_brk_base_pgup)). */
      Addr new_brk_local;
      if (new_brk < old_brk_base_pgup)
         new_brk_local = new_brk;
      else
         new_brk_local = old_brk_base_pgup;

      /* Find a segment at the beginning and at the end of the expanded
         range. */
      seg = VG_(am_find_nsegment)(old_brk_limit);
      seg2 = VG_(am_find_nsegment)(new_brk_local - 1);
      vg_assert(seg);
      vg_assert(seg2);
      vg_assert(seg == seg2);

      /* Nothing else to do. */
   }

   if (new_brk > old_brk_base_pgup) {
      /* Calculate old local brk (=MAX(old_brk_limit, old_brk_base_pgup)). */
      Addr old_brk_local;
      if (old_brk_limit < old_brk_base_pgup)
         old_brk_local = old_brk_base_pgup;
      else
         old_brk_local = old_brk_limit;

      /* Find a segment at the beginning of the expanded range. */
      if (old_brk_local > old_brk_base_pgup)
         seg = VG_(am_find_nsegment)(old_brk_local - 1);
      else
         seg = VG_(am_find_nsegment)(old_brk_local);
      vg_assert(seg);
      vg_assert(seg->kind == SkAnonC);

      /* Find the 1-page reservation segment. */
      seg2 = VG_(am_next_nsegment)(seg, True/*forwards*/);
      vg_assert(seg2);
      vg_assert(seg2->kind == SkResvn);
      vg_assert(seg->end + 1 == seg2->start);
      vg_assert(seg2->end - seg2->start + 1 == VKI_PAGE_SIZE);

      if (new_brk <= seg2->start) {
         /* Still fits within the existing anon segment, nothing to do. */
      } else {
         /* Data segment limit was already checked. */
         Addr anon_start = seg->end + 1;
         Addr resvn_start = VG_PGROUNDUP(new_brk);
         SizeT anon_size = resvn_start - anon_start;
         SizeT resvn_size = VKI_PAGE_SIZE;
         SysRes sres;

         vg_assert(VG_IS_PAGE_ALIGNED(anon_size));
         vg_assert(VG_IS_PAGE_ALIGNED(resvn_size));
         vg_assert(VG_IS_PAGE_ALIGNED(anon_start));
         vg_assert(VG_IS_PAGE_ALIGNED(resvn_start));
         vg_assert(anon_size > 0);

         /* Address space manager checks for free address space for us;
            reservation would not be otherwise created. */
         Bool ok = VG_(am_create_reservation)(resvn_start, resvn_size, SmLower,
                                              anon_size);
         if (!ok) {
            possibly_complain_brk("brk segment overflow in thread #%u: can not "
                                  "grow to %#lx\n", tid, new_brk);
            SET_STATUS_Failure(VKI_ENOMEM);
            return;
         }

         /* Establish protection from the existing segment. */
         UInt prot = (seg->hasR ? VKI_PROT_READ : 0)
                     | (seg->hasW ? VKI_PROT_WRITE : 0)
                     | (seg->hasX ? VKI_PROT_EXEC : 0);

         /* Address space manager will merge old and new data segments. */
         sres = VG_(am_mmap_anon_fixed_client)(anon_start, anon_size, prot);
         if (sr_isError(sres)) {
            possibly_complain_brk("Cannot map memory to grow brk segment in "
                                  "thread #%u to %#lx\n", tid, new_brk);
            SET_STATUS_Failure(VKI_ENOMEM);
            return;
         }
         vg_assert(sr_Res(sres) == anon_start);

         seg = VG_(am_find_nsegment)(old_brk_base_pgup);
         seg2 = VG_(am_find_nsegment)(VG_PGROUNDUP(new_brk) - 1);
         vg_assert(seg);
         vg_assert(seg2);
         vg_assert(seg == seg2);
         vg_assert(new_brk <= seg->end + 1);
      }
   }

   /* We are done, update VG_(brk_limit), tell the tool about the changes, and
      leave. */
   VG_(brk_limit) = new_brk;
   VG_TRACK(new_mem_brk, old_brk_limit, new_brk - old_brk_limit, tid);
   SET_STATUS_Success(0);
}

PRE(sys_stat)
{
   /* int stat(const char *path, struct stat *buf); */
   /* Note: We could use here the sys_newstat generic wrapper, but the 'new'
      in its name is rather confusing in the Solaris context, thus we provide
      our own wrapper. */
   PRINT("sys_stat ( %#lx(%s), %#lx )", ARG1, (HChar *) ARG1, ARG2);
   PRE_REG_READ2(long, "stat", const char *, path, struct stat *, buf);

   PRE_MEM_RASCIIZ("stat(path)", ARG1);
   PRE_MEM_WRITE("stat(buf)", ARG2, sizeof(struct vki_stat));
}

POST(sys_stat)
{
   POST_MEM_WRITE(ARG2, sizeof(struct vki_stat));
}

PRE(sys_lseek)
{
   /* off_t lseek(int fildes, off_t offset, int whence); */
   PRINT("sys_lseek ( %ld, %ld, %ld )", SARG1, SARG2, SARG3);
   PRE_REG_READ3(long, "lseek", int, fildes, vki_off_t, offset, int, whence);

   /* Stay sane. */
   if (!ML_(fd_allowed)(ARG1, "lseek", tid, False))
      SET_STATUS_Failure(VKI_EBADF);
}

PRE(sys_mount)
{
   /* int mount(const char *spec, const char *dir, int mflag, char *fstype,
                char *dataptr, int datalen, char *optptr, int optlen); */
   *flags |= SfMayBlock;
   if (ARG3 & VKI_MS_OPTIONSTR) {
      /* 8-argument mount */
      PRINT("sys_mount ( %#lx(%s), %#lx(%s), %ld, %#lx(%s), %#lx, %lu, "
            "%#lx(%s), %ld )", ARG1, (HChar *) ARG1, ARG2, (HChar *) ARG2, SARG3,
            ARG4, (HChar *) ARG4, ARG5, ARG6, ARG7, (HChar *) ARG7, SARG8);
      PRE_REG_READ8(long, "mount", const char *, spec, const char *, dir,
                    int, mflag, char *, fstype, char *, dataptr, int, datalen,
                    char *, optptr, int, optlen);
   }
   else if (ARG3 & VKI_MS_DATA) {
      /* 6-argument mount */
      PRINT("sys_mount ( %#lx(%s), %#lx(%s), %ld, %#lx(%s), %#lx, %ld )",
            ARG1, (HChar *) ARG1, ARG2, (HChar *) ARG2, SARG3, ARG4,
            (HChar *) ARG4, ARG5, SARG6);
      PRE_REG_READ6(long, "mount", const char *, spec, const char *, dir,
                    int, mflag, char *, fstype, char *, dataptr,
                    int, datalen);
   }
   else {
      /* 4-argument mount */
      PRINT("sys_mount ( %#lx(%s), %#lx(%s), %ld, %#lx(%s) )", ARG1,
            (HChar *) ARG1, ARG2, (HChar *) ARG2, SARG3, ARG4, (HChar *) ARG4);
      PRE_REG_READ4(long, "mount", const char *, spec, const char *, dir,
                    int, mflag, char *, fstype);
   }
   if (ARG1)
      PRE_MEM_RASCIIZ("mount(spec)", ARG1);
   PRE_MEM_RASCIIZ("mount(dir)", ARG2);
   if (ARG4 && ARG4 >= 256) {
      /* If ARG4 < 256, then it's an index to a fs table in the kernel. */
      PRE_MEM_RASCIIZ("mount(fstype)", ARG4);
   }
   if (ARG3 & (VKI_MS_DATA | VKI_MS_OPTIONSTR)) {
      if (ARG5)
         PRE_MEM_READ("mount(dataptr)", ARG5, ARG6);
      if ((ARG3 & VKI_MS_OPTIONSTR) && ARG7) {
         /* in/out buffer */
         PRE_MEM_RASCIIZ("mount(optptr)", ARG7);
         PRE_MEM_WRITE("mount(optptr)", ARG7, ARG8);
      }
   }
}

POST(sys_mount)
{
   if (ARG3 & VKI_MS_OPTIONSTR) {
      POST_MEM_WRITE(ARG7, VG_(strlen)((HChar*)ARG7) + 1);
   } else if (ARG3 & VKI_MS_DATA) {
      if ((ARG2) &&
          (ARG3 & MS_NOMNTTAB) &&
          (VG_STREQ((HChar *) ARG4, "namefs")) &&
          (ARG6 == sizeof(struct vki_namefd)) &&
          ML_(safe_to_deref)((void *) ARG5, ARG6)) {
         /* Most likely an fattach() call for a door file descriptor. */
         door_record_server_fattach(((struct vki_namefd *) ARG5)->fd,
                                    (HChar *) ARG2);
      }
   }
}

PRE(sys_readlinkat)
{
   /* ssize_t readlinkat(int dfd, const char *path, char *buf,
                         size_t bufsiz); */
   HChar name[30];    // large enough
   Word saved = SYSNO;

   /* Interpret the first argument as 32-bit value even on 64-bit architecture.
      This is different from Linux, for example, where glibc sign-extends it. */
   Int dfd = (Int) ARG1;

   PRINT("sys_readlinkat ( %d, %#lx(%s), %#lx, %ld )", dfd, ARG2,
         (HChar *) ARG2, ARG3, SARG4);
   PRE_REG_READ4(long, "readlinkat", int, dfd, const char *, path,
                 char *, buf, int, bufsiz);
   PRE_MEM_RASCIIZ("readlinkat(path)", ARG2);
   PRE_MEM_WRITE("readlinkat(buf)", ARG3, ARG4);

   /* Be strict but ignore dfd for absolute path. */
   if (dfd != VKI_AT_FDCWD
       && ML_(safe_to_deref)((void *) ARG2, 1)
       && ((HChar *) ARG2)[0] != '/'
       && !ML_(fd_allowed)(dfd, "readlinkat", tid, False)) {
      SET_STATUS_Failure(VKI_EBADF);
      return;
   }

   /* Handle the case where readlinkat is looking at /proc/self/path/a.out or
      /proc/<pid>/path/a.out. */
   VG_(sprintf)(name, "/proc/%d/path/a.out", VG_(getpid)());
   if (ML_(safe_to_deref)((void*)ARG2, 1) &&
       (!VG_(strcmp)((HChar*)ARG2, name) ||
        !VG_(strcmp)((HChar*)ARG2, "/proc/self/path/a.out"))) {
      VG_(sprintf)(name, "/proc/self/path/%d", VG_(cl_exec_fd));
      SET_STATUS_from_SysRes(VG_(do_syscall4)(saved, dfd, (UWord)name, ARG3,
                                              ARG4));
   }
}

POST(sys_readlinkat)
{
   POST_MEM_WRITE(ARG3, RES);
}

PRE(sys_stime)
{
   /* Kernel: int stime(time_t time); */
   PRINT("sys_stime ( %lu )", ARG1);
   PRE_REG_READ1(long, "stime", vki_time_t, time);
}

PRE(sys_fstat)
{
   /* int fstat(int fildes, struct stat *buf); */
   /* Note: We could use here the sys_newfstat generic wrapper, but the 'new'
      in its name is rather confusing in the Solaris context, thus we provide
      our own wrapper. */
   PRINT("sys_fstat ( %ld, %#lx )", SARG1, ARG2);
   PRE_REG_READ2(long, "fstat", int, fildes, struct stat *, buf);
   PRE_MEM_WRITE("fstat(buf)", ARG2, sizeof(struct vki_stat));

   /* Be strict. */
   if (!ML_(fd_allowed)(ARG1, "fstat", tid, False))
      SET_STATUS_Failure(VKI_EBADF);
}

POST(sys_fstat)
{
   POST_MEM_WRITE(ARG2, sizeof(struct vki_stat));
}

#if defined(SOLARIS_FREALPATHAT_SYSCALL)
PRE(sys_frealpathat)
{
   /* int frealpathat(int fd, char *path, char *buf, size_t buflen); */

   /* Interpret the first argument as 32-bit value even on 64-bit architecture.
      This is different from Linux, for example, where glibc sign-extends it. */
   Int fd = (Int) ARG1;

   PRINT("sys_frealpathat ( %d, %#lx(%s), %#lx, %lu )",
         fd, ARG2, (HChar *) ARG2, ARG3, ARG4);
   PRE_REG_READ4(long, "frealpathat", int, fd, char *, path,
                 char *, buf, vki_size_t, buflen);
   PRE_MEM_RASCIIZ("frealpathat(path)", ARG2);
   PRE_MEM_WRITE("frealpathat(buf)", ARG3, ARG4);

   /* Be strict but ignore fd for absolute path. */
   if (fd != VKI_AT_FDCWD
       && ML_(safe_to_deref)((void *) ARG2, 1)
       && ((HChar *) ARG2)[0] != '/'
       && !ML_(fd_allowed)(fd, "frealpathat", tid, False))
      SET_STATUS_Failure(VKI_EBADF);
}

POST(sys_frealpathat)
{
   POST_MEM_WRITE(ARG3, VG_(strlen)((HChar *) ARG3) + 1);
}
#endif /* SOLARIS_FREALPATHAT_SYSCALL */

PRE(sys_stty)
{
   /* int stty(int fd, const struct sgttyb *tty); */
   PRINT("sys_stty ( %ld, %#lx )", SARG1, ARG2);
   PRE_REG_READ2(long, "stty", int, fd,
                 const struct vki_sgttyb *, tty);
   PRE_MEM_READ("stty(tty)", ARG2, sizeof(struct vki_sgttyb));

   /* Be strict. */
   if (!ML_(fd_allowed)(ARG1, "stty", tid, False))
      SET_STATUS_Failure(VKI_EBADF);
}

PRE(sys_gtty)
{
   /* int gtty(int fd, struct sgttyb *tty); */
   PRINT("sys_gtty ( %ld, %#lx )", SARG1, ARG2);
   PRE_REG_READ2(long, "gtty", int, fd, struct vki_sgttyb *, tty);
   PRE_MEM_WRITE("gtty(tty)", ARG2, sizeof(struct vki_sgttyb));

   /* Be strict. */
   if (!ML_(fd_allowed)(ARG1, "gtty", tid, False))
      SET_STATUS_Failure(VKI_EBADF);
}

POST(sys_gtty)
{
   POST_MEM_WRITE(ARG2, sizeof(struct vki_sgttyb));
}

PRE(sys_pgrpsys)
{
   /* Kernel: int setpgrp(int flag, int pid, int pgid); */
   switch (ARG1 /*flag*/) {
   case 0:
      /* Libc: pid_t getpgrp(void); */
      PRINT("sys_pgrpsys ( %ld )", SARG1);
      PRE_REG_READ1(long, SC2("pgrpsys", "getpgrp"), int, flag);
      break;
   case 1:
      /* Libc: pid_t setpgrp(void); */
      PRINT("sys_pgrpsys ( %ld )", SARG1);
      PRE_REG_READ1(long, SC2("pgrpsys", "setpgrp"), int, flag);
      break;
   case 2:
      /* Libc: pid_t getsid(pid_t pid); */
      PRINT("sys_pgrpsys ( %ld, %ld )", SARG1, SARG2);
      PRE_REG_READ2(long, SC2("pgrpsys", "getsid"), int, flag,
                    vki_pid_t, pid);
      break;
   case 3:
      /* Libc: pid_t setsid(void); */
      PRINT("sys_pgrpsys ( %ld )", SARG1);
      PRE_REG_READ1(long, SC2("pgrpsys", "setsid"), int, flag);
      break;
   case 4:
      /* Libc: pid_t getpgid(pid_t pid); */
      PRINT("sys_pgrpsys ( %ld, %ld )", SARG1, SARG2);
      PRE_REG_READ2(long, SC2("pgrpsys", "getpgid"), int, flag,
                    vki_pid_t, pid);
      break;
   case 5:
      /* Libc: int setpgid(pid_t pid, pid_t pgid); */
      PRINT("sys_pgrpsys ( %ld, %ld, %ld )", SARG1, SARG2, SARG3);
      PRE_REG_READ3(long, SC2("pgrpsys", "setpgid"), int, flag,
                    vki_pid_t, pid, vki_pid_t, pgid);
      break;
   default:
      VG_(unimplemented)("Syswrap of the pgrpsys call with flag %ld.", SARG1);
      /*NOTREACHED*/
      break;
   }
}

PRE(sys_pipe)
{
#if defined(SOLARIS_NEW_PIPE_SYSCALL)
   /* int pipe(int fildes[2], int flags); */
   PRINT("sys_pipe ( %#lx, %ld )", ARG1, SARG2);
   PRE_REG_READ2(long, "pipe", int *, fildes, int, flags);
   PRE_MEM_WRITE("pipe(fildes)", ARG1, 2 * sizeof(int));
#else
   /* longlong_t pipe(); */
   PRINT("sys_pipe ( )");
   PRE_REG_READ0(long, "pipe");
#endif /* SOLARIS_NEW_PIPE_SYSCALL */
}

POST(sys_pipe)
{
   Int p0, p1;

#if defined(SOLARIS_NEW_PIPE_SYSCALL)
   int *fds = (int*)ARG1;
   p0 = fds[0];
   p1 = fds[1];
   POST_MEM_WRITE(ARG1, 2 * sizeof(int));
#else
   p0 = RES;
   p1 = RESHI;
#endif /* SOLARIS_NEW_PIPE_SYSCALL */

   if (!ML_(fd_allowed)(p0, "pipe", tid, True) ||
       !ML_(fd_allowed)(p1, "pipe", tid, True)) {
      VG_(close)(p0);
      VG_(close)(p1);
      SET_STATUS_Failure(VKI_EMFILE);
   }
   else if (VG_(clo_track_fds)) {
      ML_(record_fd_open_nameless)(tid, p0);
      ML_(record_fd_open_nameless)(tid, p1);
   }
}

PRE(sys_faccessat)
{
   /* int faccessat(int fd, const char *path, int amode, int flag); */

   /* Interpret the first argument as 32-bit value even on 64-bit architecture.
      This is different from Linux, for example, where glibc sign-extends it. */
   Int fd = (Int) ARG1;

   PRINT("sys_faccessat ( %d, %#lx(%s), %ld, %ld )", fd, ARG2,
         (HChar *) ARG2, SARG3, SARG4);
   PRE_REG_READ4(long, "faccessat", int, fd, const char *, path,
                 int, amode, int, flag);
   PRE_MEM_RASCIIZ("faccessat(path)", ARG2);

   /* Be strict but ignore fd for absolute path. */
   if (fd != VKI_AT_FDCWD
       && ML_(safe_to_deref)((void *) ARG2, 1)
       && ((HChar *) ARG2)[0] != '/'
       && !ML_(fd_allowed)(fd, "faccessat", tid, False))
      SET_STATUS_Failure(VKI_EBADF);
}

PRE(sys_mknodat)
{
   /* int mknodat(int fd, char *fname, mode_t fmode, dev_t dev); */

   /* Interpret the first argument as 32-bit value even on 64-bit architecture.
      This is different from Linux, for example, where glibc sign-extends it. */
   Int fd = (Int) ARG1;

   PRINT("sys_mknodat ( %d, %#lx(%s), %ld, %ld )", fd, ARG2,
         (HChar *) ARG2, SARG3, SARG4);
   PRE_REG_READ4(long, "mknodat", int, fd, const char *, fname,
                 vki_mode_t, fmode, vki_dev_t, dev);
   PRE_MEM_RASCIIZ("mknodat(fname)", ARG2);

   /* Be strict but ignore fd for absolute path. */
   if (fd != VKI_AT_FDCWD
       && ML_(safe_to_deref)((void *) ARG2, 1)
       && ((HChar *) ARG2)[0] != '/'
       && !ML_(fd_allowed)(fd, "mknodat", tid, False))
      SET_STATUS_Failure(VKI_EBADF);

   *flags |= SfMayBlock;
}

POST(sys_mknodat)
{
   if (!ML_(fd_allowed)(RES, "mknodat", tid, True)) {
      VG_(close)(RES);
      SET_STATUS_Failure(VKI_EMFILE);
   } else if (VG_(clo_track_fds))
      ML_(record_fd_open_with_given_name)(tid, RES, (HChar *) ARG2);
}

PRE(sys_sysi86)
{
   /* int sysi86(int cmd, uintptr_t arg1, uintptr_t arg2, uintptr_t arg3); */
   PRINT("sys_sysi86 ( %ld, %#lx, %#lx, %#lx )", SARG1, ARG2, ARG3, ARG4);
   PRE_REG_READ4(long, "sysi86", int, cmd, uintptr_t, arg1, uintptr_t, arg2,
                 uintptr_t, arg3);

   switch (ARG1 /*cmd*/) {
   case VKI_SI86FPSTART:
      PRE_MEM_WRITE("sysi86(fp_hw)", ARG2, sizeof(vki_uint_t));
      /* ARG3 is a desired x87 FCW value, ARG4 is a desired SSE MXCSR value.
         They are passed to the kernel but V will change them later anyway
         (this is a general Valgrind limitation described in the official
         documentation). */
      break;
   default:
      VG_(unimplemented)("Syswrap of the sysi86 call with cmd %ld.", SARG1);
      /*NOTREACHED*/
      break;
   }
}

POST(sys_sysi86)
{
   switch (ARG1 /*cmd*/) {
   case VKI_SI86FPSTART:
      POST_MEM_WRITE(ARG2, sizeof(vki_uint_t));
      break;
   default:
      vg_assert(0);
      break;
   }
}

PRE(sys_shmsys)
{
   /* Kernel: uintptr_t shmsys(int opcode, uintptr_t a0, uintptr_t a1,
                               uintptr_t a2, uintptr_t a3);
    */
   *flags |= SfMayBlock;

   switch (ARG1 /*opcode*/) {
   case VKI_SHMAT:
      /* Libc: void *shmat(int shmid, const void *shmaddr, int shmflg); */
      PRINT("sys_shmsys ( %ld, %ld, %#lx, %ld )",
            SARG1, SARG2, ARG3, SARG4);
      PRE_REG_READ4(long, SC2("shmsys", "shmat"), int, opcode,
                    int, shmid, const void *, shmaddr, int, shmflg);

      UWord addr = ML_(generic_PRE_sys_shmat)(tid, ARG2, ARG3, ARG4);
      if (addr == 0)
         SET_STATUS_Failure(VKI_EINVAL);
      else
         ARG3 = addr;
      break;

   case VKI_SHMCTL:
      /* Libc: int shmctl(int shmid, int cmd, struct shmid_ds *buf); */
      switch (ARG3 /* cmd */) {
      case VKI_SHM_LOCK:
         PRINT("sys_shmsys ( %ld, %ld, %ld )", SARG1, SARG2, SARG3);
         PRE_REG_READ3(long, SC3("shmsys", "shmctl", "lock"),
                       int, opcode, int, shmid, int, cmd);
         break;
      case VKI_SHM_UNLOCK:
         PRINT("sys_shmsys ( %ld, %ld, %ld )", SARG1, SARG2, SARG3);
         PRE_REG_READ3(long, SC3("shmsys", "shmctl", "unlock"),
                       int, opcode, int, shmid, int, cmd);
         break;
      case VKI_IPC_RMID:
         PRINT("sys_shmsys ( %ld, %ld, %ld )", SARG1, SARG2, SARG3);
         PRE_REG_READ3(long, SC3("shmsys", "shmctl", "rmid"),
                       int, opcode, int, shmid, int, cmd);
         break;
      case VKI_IPC_SET:
         PRINT("sys_shmsys ( %ld, %ld, %ld, %#lx )",
               SARG1, SARG2, SARG3, ARG4);
         PRE_REG_READ4(long, SC3("shmsys", "shmctl", "set"),
                       int, opcode, int, shmid, int, cmd,
                       struct vki_shmid_ds *, buf);

         struct vki_shmid_ds *buf = (struct vki_shmid_ds *) ARG4;
         PRE_FIELD_READ("shmsys(shmctl, ipc_set, buf->shm_perm.uid)",
                        buf->shm_perm.uid);
         PRE_FIELD_READ("shmsys(shmctl, ipc_set, buf->shm_perm.gid)",
                        buf->shm_perm.gid);
         PRE_FIELD_READ("shmsys(shmctl, ipc_set, buf->shm_perm.mode)",
                        buf->shm_perm.mode);
         break;
      case VKI_IPC_STAT:
         PRINT("sys_shmsys ( %ld, %ld, %ld, %#lx )",
               SARG1, SARG2, SARG3, ARG4);
         PRE_REG_READ4(long, SC3("shmsys", "shmctl", "stat"),
                       int, opcode, int, shmid, int, cmd,
                       struct vki_shmid_ds *, buf);
         PRE_MEM_WRITE("shmsys(shmctl, ipc_stat, buf)", ARG4,
                       sizeof(struct vki_shmid_ds));
        break;
      case VKI_IPC_SET64:
         PRINT("sys_shmsys ( %ld, %ld, %ld, %#lx )",
               SARG1, SARG2, SARG3, ARG4);
         PRE_REG_READ4(long, SC3("shmsys", "shmctl", "set64"),
                       int, opcode, int, shmid, int, cmd,
                       struct vki_shmid_ds64 *, buf);

         struct vki_shmid_ds64 *buf64 = (struct vki_shmid_ds64 *) ARG4;
         PRE_FIELD_READ("shmsys(shmctl, ipc_set64, "
                        "buf->shmx_perm.ipcx_uid)",
                        buf64->shmx_perm.ipcx_uid);
         PRE_FIELD_READ("shmsys(shmctl, ipc_set64, "
                        "buf->shmx_perm.ipcx_gid)",
                        buf64->shmx_perm.ipcx_gid);
         PRE_FIELD_READ("shmsys(shmctl, ipc_set64, "
                        "buf->shmx_perm.ipcx_mode)",
                        buf64->shmx_perm.ipcx_mode);
         break;
      case VKI_IPC_STAT64:
         PRINT("sys_shmsys ( %ld, %ld, %ld, %#lx )",
               SARG1, SARG2, SARG3, ARG4);
         PRE_REG_READ4(long, SC3("shmsys", "shmctl", "stat64"),
                       int, opcode, int, shmid, int, cmd,
                       struct vki_shmid_ds64 *, buf);
         PRE_MEM_WRITE("shmsys(shmctl, ipc_stat64, buf)", ARG4,
                       sizeof(struct vki_shmid_ds64));
         break;
#if defined(SOLARIS_SHM_NEW)
      case VKI_IPC_XSTAT64:
         PRINT("sys_shmsys ( %ld, %ld, %ld, %#lx )",
               SARG1, SARG2, SARG3, ARG4);
         PRE_REG_READ4(long, SC3("shmsys", "shmctl", "xstat64"),
                       int, opcode, int, shmid, int, cmd,
                       struct vki_shmid_ds64 *, buf);
         PRE_MEM_WRITE("shmsys(shmctl, ipc_xstat64, buf)", ARG4,
                       sizeof(struct vki_shmid_xds64));
         break;
#endif /* SOLARIS_SHM_NEW */
      default:
         VG_(unimplemented)("Syswrap of the shmsys(shmctl) call with "
                            "cmd %ld.", SARG3);
         /*NOTREACHED*/
         break;
      }
      break;

   case VKI_SHMDT:
      /* Libc: int shmdt(const void *shmaddr); */
      PRINT("sys_shmsys ( %ld, %#lx )", SARG1, ARG2);
      PRE_REG_READ2(long, SC2("shmsys", "shmdt"), int, opcode,
                    const void *, shmaddr);

      if (!ML_(generic_PRE_sys_shmdt)(tid, ARG2))
	 SET_STATUS_Failure(VKI_EINVAL);
      break;

   case VKI_SHMGET:
      /* Libc: int shmget(key_t key, size_t size, int shmflg); */
      PRINT("sys_shmsys ( %ld, %ld, %lu, %lu )",
            SARG1, SARG2, ARG3, ARG4);
      PRE_REG_READ4(long, SC2("shmsys", "shmget"), int, opcode,
                    vki_key_t, key, vki_size_t, size, int, shmflg);
      break;

   case VKI_SHMIDS:
      /* Libc: int shmids(int *buf, uint_t nids, uint_t *pnids); */
      PRINT("sys_shmsys ( %ld, %#lx, %lu, %#lx )",
            SARG1, ARG2, ARG3, ARG4);
      PRE_REG_READ4(long, SC2("shmsys", "shmids"), int, opcode,
                    int *, buf, vki_uint_t, nids, vki_uint_t *, pnids);

      PRE_MEM_WRITE("shmsys(shmids, buf)", ARG2, ARG3 * sizeof(int *));
      PRE_MEM_WRITE("shmsys(shmids, pnids)", ARG4, sizeof(vki_uint_t));
      break;

#if defined(SOLARIS_SHM_NEW)
   case VKI_SHMADV:
      /* Libc: int shmadv(int shmid, uint_t cmd, uint_t *advice); */
      PRINT("sys_shmsys ( %ld, %ld, %lu, %ld )",
            SARG1, SARG2, ARG3, ARG4);
      PRE_REG_READ4(long, SC2("shmsys", "shmadv"), int, opcode,
                    int, shmid, vki_uint_t, cmd, vki_uint_t *, advice);

      switch (ARG3 /*cmd*/) {
      case VKI_SHM_ADV_GET:
         PRE_MEM_WRITE("shmsys(shmadv, advice)", ARG4,
                       sizeof(vki_uint_t));
         break;
      case VKI_SHM_ADV_SET:
         PRE_MEM_READ("shmsys(shmadv, advice)", ARG4,
                       sizeof(vki_uint_t));
         break;
      default:
         VG_(unimplemented)("Syswrap of the shmsys(shmadv) call with "
                            "cmd %lu.", ARG3);
         /*NOTREACHED*/
         break;
      }
      break;

   case VKI_SHMGET_OSM:
      /* Libc: int shmget_osm(key_t key, size_t size, int shmflg,
                              size_t granule_sz);
       */
      PRINT("sys_shmsys ( %ld, %ld, %lu, %ld, %lu )",
            SARG1, SARG2, ARG3, SARG4, ARG5);
      PRE_REG_READ5(long, SC2("shmsys", "shmget_osm"), int, opcode,
                    vki_key_t, key, vki_size_t, size, int, shmflg,
                    vki_size_t, granule_sz);
      break;
#endif /* SOLARIS_SHM_NEW */

   default:
      VG_(unimplemented)("Syswrap of the shmsys call with opcode %ld.",
                         SARG1);
      /*NOTREACHED*/
      break;
   }
}

POST(sys_shmsys)
{
   switch (ARG1 /*opcode*/) {
   case VKI_SHMAT:
      ML_(generic_POST_sys_shmat)(tid, RES, ARG2, ARG3, ARG4);
      break;

   case VKI_SHMCTL:
      switch (ARG3 /*cmd*/) {
      case VKI_SHM_LOCK:
      case VKI_SHM_UNLOCK:
      case VKI_IPC_RMID:
      case VKI_IPC_SET:
         break;
      case VKI_IPC_STAT:
         POST_MEM_WRITE(ARG4, sizeof(struct vki_shmid_ds));
         break;
      case VKI_IPC_SET64:
         break;
      case VKI_IPC_STAT64:
         POST_MEM_WRITE(ARG4, sizeof(struct vki_shmid_ds64));
         break;
#if defined(SOLARIS_SHM_NEW)
      case VKI_IPC_XSTAT64:
         POST_MEM_WRITE(ARG4, sizeof(struct vki_shmid_xds64));
         break;
#endif /* SOLARIS_SHM_NEW */
      default:
         vg_assert(0);
         break;
      }
      break;

   case VKI_SHMDT:
      ML_(generic_POST_sys_shmdt)(tid, RES, ARG2);
      break;

   case VKI_SHMGET:
      break;

   case VKI_SHMIDS:
      {
         POST_MEM_WRITE(ARG4, sizeof(vki_uint_t));

         uint_t *pnids = (vki_uint_t *) ARG4;
         if (*pnids <= ARG3)
            POST_MEM_WRITE(ARG2, *pnids * sizeof(int *));
      }
      break;

#if defined(SOLARIS_SHM_NEW)
   case VKI_SHMADV:
      switch (ARG3 /*cmd*/) {
      case VKI_SHM_ADV_GET:
         POST_MEM_WRITE(ARG4, sizeof(vki_uint_t));
         break;
      case VKI_SHM_ADV_SET:
         break;
      default:
         vg_assert(0);
         break;
      }
      break;

   case VKI_SHMGET_OSM:
      break;
#endif /* SOLARIS_SHM_NEW */

   default:
      vg_assert(0);
      break;
   }
}

PRE(sys_semsys)
{
   /* Kernel: int semsys(int opcode, uintptr_t a1, uintptr_t a2, uintptr_t a3,
                         uintptr_t a4);
    */
   *flags |= SfMayBlock;

   switch (ARG1 /*opcode*/) {
   case VKI_SEMCTL:
      /* Libc: int semctl(int semid, int semnum, int cmd...); */
      switch (ARG4) {
         case VKI_IPC_STAT:
            PRINT("sys_semsys ( %ld, %ld, %ld, %ld, %#lx )",
                  SARG1, SARG2, SARG3, SARG4, ARG5);
            PRE_REG_READ5(long, SC3("semsys", "semctl", "stat"), int, opcode,
                          int, semid, int, semnum, int, cmd,
                          struct vki_semid_ds *, arg);
            break;
         case VKI_IPC_SET:
            PRINT("sys_semsys ( %ld, %ld, %ld, %ld, %#lx )",
                  SARG1, SARG2, SARG3, SARG4, ARG5);
            PRE_REG_READ5(long, SC3("semsys", "semctl", "set"), int, opcode,
                          int, semid, int, semnum, int, cmd,
                          struct vki_semid_ds *, arg);
            break;
         case VKI_IPC_STAT64:
            PRINT("sys_semsys ( %ld, %ld, %ld, %ld, %#lx )",
                  SARG1, SARG2, SARG3, SARG4, ARG5);
            PRE_REG_READ5(long, SC3("semsys", "semctl", "stat64"), int, opcode,
                          int, semid, int, semnum, int, cmd,
                          struct vki_semid64_ds *, arg);
            break;
         case VKI_IPC_SET64:
            PRINT("sys_semsys ( %ld, %ld, %ld, %ld, %#lx )",
                  SARG1, SARG2, SARG3, SARG4, ARG5);
            PRE_REG_READ5(long, SC3("semsys", "semctl", "set64"), int, opcode,
                          int, semid, int, semnum, int, cmd,
                          struct vki_semid64_ds *, arg);
            break;
         case VKI_IPC_RMID:
            PRINT("sys_semsys ( %ld, %ld, %ld )", SARG1, SARG3, SARG4);
            PRE_REG_READ3(long, SC3("semsys", "semctl", "rmid"), int, opcode,
                          int, semid, int, cmd);
            break;
         case VKI_GETALL:
            PRINT("sys_semsys ( %ld, %ld, %ld, %#lx )",
                  SARG1, SARG2, SARG4, ARG5);
            PRE_REG_READ4(long, SC3("semsys", "semctl", "getall"), int, opcode,
                          int, semid, int, cmd, ushort_t *, arg);
            break;
         case VKI_SETALL:
            PRINT("sys_semsys ( %ld, %ld, %ld, %#lx )",
                  SARG1, SARG2, SARG4, ARG5);
            PRE_REG_READ4(long, SC3("semsys", "semctl", "setall"), int, opcode,
                          int, semid, int, cmd, ushort_t *, arg);
            break;
         case VKI_GETVAL:
            PRINT("sys_semsys ( %ld, %ld, %ld, %ld )",
                  SARG1, SARG2, SARG3, SARG4);
            PRE_REG_READ4(long, SC3("semsys", "semctl", "getval"), int, opcode,
                          int, semid, int, semnum, int, cmd);
            break;
         case VKI_SETVAL:
            PRINT("sys_semsys ( %ld, %ld, %ld, %ld, %#lx )",
                  SARG1, SARG2, SARG3, SARG4, ARG5);
            PRE_REG_READ5(long, SC3("semsys", "semctl", "setval"), int, opcode,
                          int, semid, int, semnum, int, cmd,
                          union vki_semun *, arg);
            break;
         case VKI_GETPID:
            PRINT("sys_semsys ( %ld, %ld, %ld, %ld )",
                  SARG1, SARG2, SARG3, SARG4);
            PRE_REG_READ4(long, SC3("semsys", "semctl", "getpid"), int, opcode,
                          int, semid, int, semnum, int, cmd);
            break;
         case VKI_GETNCNT:
            PRINT("sys_semsys ( %ld, %ld, %ld, %ld )",
                  SARG1, SARG2, SARG3, SARG4);
            PRE_REG_READ4(long, SC3("semsys", "semctl", "getncnt"),
                          int, opcode, int, semid, int, semnum, int, cmd);
            break;
         case VKI_GETZCNT:
            PRINT("sys_semsys ( %ld, %ld, %ld, %ld )",
                  SARG1, SARG2, SARG3, SARG4);
            PRE_REG_READ4(long, SC3("semsys", "semctl", "getzcnt"),
                          int, opcode, int, semid, int, semnum, int, cmd);
            break;
         default:
            VG_(unimplemented)("Syswrap of the semsys(semctl) call "
                               "with cmd %ld.", SARG4);
            /*NOTREACHED*/
            break;
      }
      ML_(generic_PRE_sys_semctl)(tid, ARG2, ARG3, ARG4, ARG5);
      break;
   case VKI_SEMGET:
      /* Libc: int semget(key_t key, int nsems, int semflg); */
      PRINT("sys_semsys ( %ld, %ld, %ld, %ld )", SARG1, SARG2, SARG3, SARG4);
      PRE_REG_READ4(long, SC2("semsys", "semget"), int, opcode,
                    vki_key_t, key, int, nsems, int, semflg);
      break;
   case VKI_SEMOP:
      /* Libc: int semop(int semid, struct sembuf *sops, size_t nsops); */
      PRINT("sys_semsys ( %ld, %ld, %#lx, %lu )", SARG1, SARG2, ARG3, ARG4);
      PRE_REG_READ4(long, SC2("semsys", "semop"), int, opcode, int, semid,
                    struct vki_sembuf *, sops, vki_size_t, nsops);
      ML_(generic_PRE_sys_semop)(tid, ARG2, ARG3, ARG4);
      break;
   case VKI_SEMIDS:
      /* Libc: int semids(int *buf, uint_t nids, uint_t *pnids); */
      PRINT("sys_semsys ( %ld, %#lx, %lu, %#lx )", SARG1, ARG2, ARG3, ARG4);
      PRE_REG_READ4(long, SC2("semsys", "semids"), int, opcode, int *, buf,
                   vki_uint_t, nids, vki_uint_t *, pnids);

      PRE_MEM_WRITE("semsys(semids, buf)", ARG2, ARG3 * sizeof(int *));
      PRE_MEM_WRITE("semsys(semids, pnids)", ARG4, sizeof(vki_uint_t));
      break;
   case VKI_SEMTIMEDOP:
      /* Libc: int semtimedop(int semid, struct sembuf *sops, size_t nsops,
                              const struct timespec *timeout);
       */
      PRINT("sys_semsys ( %ld, %ld, %#lx, %lu, %#lx )", SARG1, SARG2, ARG3,
            ARG4, ARG5);
      PRE_REG_READ5(long, SC2("semsys", "semtimedop"), int, opcode,
                    int, semid, struct vki_sembuf *, sops, vki_size_t, nsops,
                    struct vki_timespec *, timeout);
      ML_(generic_PRE_sys_semtimedop)(tid, ARG2, ARG3, ARG4, ARG5);
      break;
   default:
      VG_(unimplemented)("Syswrap of the semsys call with opcode %ld.", SARG1);
      /*NOTREACHED*/
      break;
   }
}

POST(sys_semsys)
{
   switch (ARG1 /*opcode*/) {
   case VKI_SEMCTL:
      ML_(generic_POST_sys_semctl)(tid, RES, ARG2, ARG3, ARG4, ARG5);
      break;
   case VKI_SEMGET:
   case VKI_SEMOP:
      break;
   case VKI_SEMIDS:
      {
         POST_MEM_WRITE(ARG4, sizeof(vki_uint_t));

         uint_t *pnids = (uint_t *)ARG4;
         if (*pnids <= ARG3)
            POST_MEM_WRITE(ARG2, *pnids * sizeof(int *));
      }
      break;
   case VKI_SEMTIMEDOP:
      break;
   default:
      vg_assert(0);
      break;
   }
}

/* ---------------------------------------------------------------------
   ioctl wrappers
   ------------------------------------------------------------------ */

PRE(sys_ioctl)
{
   /* int ioctl(int fildes, int request, ...); */
   *flags |= SfMayBlock;

   /* Prevent sign extending the switch case values to 64-bits on 64-bits
      architectures. */
   Int cmd = (Int) ARG2;

   switch (cmd /*request*/) {
      /* Handle 2-arg specially here (they do not use ARG3 at all). */
   case VKI_DINFOIDENT:
   case VKI_TIOCNOTTY:
   case VKI_TIOCSCTTY:
      PRINT("sys_ioctl ( %ld, %#lx )", SARG1, ARG2);
      PRE_REG_READ2(long, "ioctl", int, fd, int, request);
      break;
      /* And now come the 3-arg ones. */
   default:
      PRINT("sys_ioctl ( %ld, %#lx, %#lx )", SARG1, ARG2, ARG3);
      PRE_REG_READ3(long, "ioctl", int, fd, int, request, intptr_t, arg);
      break;
   }

   switch (cmd /*request*/) {
   /* pools */
   case VKI_POOL_STATUSQ:
      PRE_MEM_WRITE("ioctl(POOL_STATUSQ)", ARG3, sizeof(vki_pool_status_t));
      break;

   /* mntio */
   case VKI_MNTIOC_GETEXTMNTENT:
      {
         PRE_MEM_READ("ioctl(MNTIOC_GETEXTMNTENT)",
                      ARG3, sizeof(struct vki_mntentbuf));

         struct vki_mntentbuf *embuf = (struct vki_mntentbuf *) ARG3;
         if (ML_(safe_to_deref(embuf, sizeof(*embuf)))) {
            PRE_MEM_WRITE("ioctl(MNTIOC_GETEXTMNTENT, embuf->mbuf_emp)",
                          (Addr) embuf->mbuf_emp, sizeof(struct vki_extmnttab));
            PRE_MEM_WRITE("ioctl(MNTIOC_GETEXTMNTENT, embuf->mbuf_buf)",
                          (Addr) embuf->mbuf_buf, embuf->mbuf_bufsize);
         }
      }
      break;

   case VKI_MNTIOC_GETMNTANY:
      {
         PRE_MEM_READ("ioctl(MNTIOC_GETMNTANY)",
                      ARG3, sizeof(struct vki_mntentbuf));

         struct vki_mntentbuf *embuf = (struct vki_mntentbuf *) ARG3;
         if (ML_(safe_to_deref(embuf, sizeof(*embuf)))) {
            PRE_MEM_READ("ioctl(MNTIOC_GETMNTANY, embuf->mbuf_emp)",
                         (Addr) embuf->mbuf_emp, sizeof(struct vki_mnttab));
            PRE_MEM_WRITE("ioctl(MNTIOC_GETMNTANY, embuf->mbuf_buf)",
                          (Addr) embuf->mbuf_buf, embuf->mbuf_bufsize);

            struct vki_mnttab *mnt = (struct vki_mnttab *) embuf->mbuf_emp;
            if (ML_(safe_to_deref(mnt, sizeof(struct vki_mnttab)))) {
               if (mnt->mnt_special != NULL)
                  PRE_MEM_RASCIIZ("ioctl(MNTIOC_GETMNTANY, mnt->mnt_special)",
                                  (Addr) mnt->mnt_special);
               if (mnt->mnt_mountp != NULL)
                  PRE_MEM_RASCIIZ("ioctl(MNTIOC_GETMNTANY, mnt->mnt_mountp)",
                                  (Addr) mnt->mnt_mountp);
               if (mnt->mnt_fstype != NULL)
                  PRE_MEM_RASCIIZ("ioctl(MNTIOC_GETMNTANY, mnt->mnt_fstype)",
                                  (Addr) mnt->mnt_fstype);
               if (mnt->mnt_mntopts != NULL)
                  PRE_MEM_RASCIIZ("ioctl(MNTIOC_GETMNTANY, mnt->mnt_mntopts)",
                                  (Addr) mnt->mnt_mntopts);
               if (mnt->mnt_time != NULL)
                  PRE_MEM_RASCIIZ("ioctl(MNTIOC_GETMNTANY, mnt->mnt_time)",
                                  (Addr) mnt->mnt_time);
            }
         }
      }
      break;

   /* termio/termios */
   case VKI_TCGETA:
      PRE_MEM_WRITE("ioctl(TCGETA)", ARG3, sizeof(struct vki_termio));
      break;
   case VKI_TCGETS:
      PRE_MEM_WRITE("ioctl(TCGETS)", ARG3, sizeof(struct vki_termios));
      break;
   case VKI_TCSETS:
      PRE_MEM_READ("ioctl(TCSETS)", ARG3, sizeof(struct vki_termios));
      break;
   case VKI_TCSETSW:
      PRE_MEM_READ("ioctl(TCSETSW)", ARG3, sizeof(struct vki_termios));
      break;
   case VKI_TCSETSF:
      PRE_MEM_READ("ioctl(TCSETSF)", ARG3, sizeof(struct vki_termios));
      break;
   case VKI_TIOCGWINSZ:
      PRE_MEM_WRITE("ioctl(TIOCGWINSZ)", ARG3, sizeof(struct vki_winsize));
      break;
   case VKI_TIOCSWINSZ:
      PRE_MEM_READ("ioctl(TIOCSWINSZ)", ARG3, sizeof(struct vki_winsize));
      break;
   case VKI_TIOCGPGRP:
      PRE_MEM_WRITE("ioctl(TIOCGPGRP)", ARG3, sizeof(vki_pid_t));
      break;
   case VKI_TIOCSPGRP:
      PRE_MEM_READ("ioctl(TIOCSPGRP)", ARG3, sizeof(vki_pid_t));
      break;
   case VKI_TIOCGSID:
      PRE_MEM_WRITE("ioctl(TIOCGSID)", ARG3, sizeof(vki_pid_t));
      break;
   case VKI_TIOCNOTTY:
   case VKI_TIOCSCTTY:
      break;

   /* STREAMS */
   case VKI_I_PUSH:
      PRE_MEM_RASCIIZ("ioctl(I_PUSH)", ARG3);
      break;
   case VKI_I_FLUSH:
      break;
   case VKI_I_STR:
      {
         PRE_MEM_READ("ioctl(I_STR)", ARG3, sizeof(struct vki_strioctl));

         struct vki_strioctl *p = (struct vki_strioctl *) ARG3;
         if (ML_(safe_to_deref(p, sizeof(*p)))) {
            if ((p->ic_dp != NULL) && (p->ic_len > 0)) {
               PRE_MEM_READ("ioctl(I_STR, strioctl->ic_dp)",
                            (Addr) p->ic_dp, p->ic_len);
            }
         }
      }
      break;
   case VKI_I_FIND:
      PRE_MEM_RASCIIZ("ioctl(I_FIND)", ARG3);
      break;
   case VKI_I_PEEK:
      {
         /* Try hard not to mark strpeek->*buf.len members as being read. */
         struct vki_strpeek *p = (struct vki_strpeek*)ARG3;

         PRE_FIELD_READ("ioctl(I_PEEK, strpeek->ctlbuf.maxlen)",
                        p->ctlbuf.maxlen);
         PRE_FIELD_WRITE("ioctl(I_PEEK, strpeek->ctlbuf.len)",
                         p->ctlbuf.len);
         PRE_FIELD_READ("ioctl(I_PEEK, strpeek->ctlbuf.buf)",
                        p->ctlbuf.buf);
         PRE_FIELD_READ("ioctl(I_PEEK, strpeek->databuf.maxlen)",
                        p->databuf.maxlen);
         PRE_FIELD_WRITE("ioctl(I_PEEK, strpeek->databuf.len)",
                         p->databuf.len);
         PRE_FIELD_READ("ioctl(I_PEEK, strpeek->databuf.buf)",
                        p->databuf.buf);
         PRE_FIELD_READ("ioctl(I_PEEK, strpeek->flags)", p->flags);
         /*PRE_FIELD_WRITE("ioctl(I_PEEK, strpeek->flags)", p->flags);*/

         if (ML_(safe_to_deref(p, sizeof(*p)))) {
            if (p->ctlbuf.buf && p->ctlbuf.maxlen > 0)
               PRE_MEM_WRITE("ioctl(I_PEEK, strpeek->ctlbuf.buf)",
                             (Addr)p->ctlbuf.buf, p->ctlbuf.maxlen);
            if (p->databuf.buf && p->databuf.maxlen > 0)
               PRE_MEM_WRITE("ioctl(I_PEEK, strpeek->databuf.buf)",
                             (Addr)p->databuf.buf, p->databuf.maxlen);
         }
      }
      break;
   case VKI_I_CANPUT:
      break;

   /* sockio */
   case VKI_SIOCGIFCONF:
      {
         struct vki_ifconf *p = (struct vki_ifconf *) ARG3;
         PRE_FIELD_READ("ioctl(SIOCGIFCONF, ifconf->ifc_len)", p->ifc_len);
         PRE_FIELD_READ("ioctl(SIOCGIFCONF, ifconf->ifc_buf)", p->ifc_buf);
         if (ML_(safe_to_deref)(p, sizeof(*p))) {
            if ((p->ifc_buf != NULL) && (p->ifc_len > 0))
               PRE_MEM_WRITE("ioctl(SIOCGIFCONF, ifconf->ifc_buf)",
                             (Addr) p->ifc_buf, p->ifc_len);
         }
         /* ifc_len gets also written to during SIOCGIFCONF ioctl. */
      }
      break;
   case VKI_SIOCGIFFLAGS:
      {
         struct vki_ifreq *p = (struct vki_ifreq *) ARG3;
         PRE_FIELD_READ("ioctl(SIOCGIFFLAGS, ifreq->ifr_name)", p->ifr_name);
         PRE_FIELD_WRITE("ioctl(SIOCGIFFLAGS, ifreq->ifr_flags)", p->ifr_flags);
      }
      break;
   case VKI_SIOCGIFNETMASK:
      {
         struct vki_ifreq *p = (struct vki_ifreq *) ARG3;
         PRE_FIELD_READ("ioctl(SIOCGIFFLAGS, ifreq->ifr_name)", p->ifr_name);
         PRE_FIELD_WRITE("ioctl(SIOCGIFFLAGS, ifreq->ifr_addr)", p->ifr_addr);
      }
      break;
   case VKI_SIOCGIFNUM:
      PRE_MEM_WRITE("ioctl(SIOCGIFNUM)", ARG3, sizeof(int));
      break;
   case VKI_SIOCGLIFBRDADDR:
      {
         struct vki_lifreq *p = (struct vki_lifreq *) ARG3;
         PRE_FIELD_READ("ioctl(SIOCGLIFBRDADDR, lifreq->lifr_name)",
                        p->lifr_name);
         PRE_FIELD_WRITE("ioctl(SIOCGLIFBRDADDR, lifreq->lifr_addr)",
                         p->lifr_addr);
      }
      break;
   case VKI_SIOCGLIFCONF:
      {
         struct vki_lifconf *p = (struct vki_lifconf *) ARG3;
         PRE_FIELD_READ("ioctl(SIOCGLIFCONF, lifconf->lifc_len)", p->lifc_len);
         PRE_FIELD_READ("ioctl(SIOCGLIFCONF, lifconf->lifc_buf)", p->lifc_buf);
         PRE_FIELD_READ("ioctl(SIOCGLIFCONF, lifconf->lifc_family)",
                        p->lifc_family);
         PRE_FIELD_READ("ioctl(SIOCGLIFCONF, lifconf->lifc_flags)",
                        p->lifc_flags);
         if (ML_(safe_to_deref)(p, sizeof(*p))) {
            if ((p->lifc_buf != NULL) && (p->lifc_len > 0))
               PRE_MEM_WRITE("ioctl(SIOCGLIFCONF, lifconf->lifc_buf)",
                             (Addr) p->lifc_buf, p->lifc_len);
         }
         /* lifc_len gets also written to during SIOCGLIFCONF ioctl. */
      }
      break;
   case VKI_SIOCGLIFFLAGS:
      {
         struct vki_lifreq *p = (struct vki_lifreq *) ARG3;
         PRE_FIELD_READ("ioctl(SIOCGLIFFLAGS, lifreq->lifr_name)",
                        p->lifr_name);
         PRE_FIELD_WRITE("ioctl(SIOCGLIFFLAGS, lifreq->lifr_flags)",
                         p->lifr_flags);
      }
      break;
   case VKI_SIOCGLIFNETMASK:
      {
         struct vki_lifreq *p = (struct vki_lifreq *) ARG3;
         PRE_FIELD_READ("ioctl(SIOCGLIFNETMASK, lifreq->lifr_name)",
                        p->lifr_name);
         PRE_FIELD_WRITE("ioctl(SIOCGLIFNETMASK, lifreq->lifr_addr)",
                         p->lifr_addr);
      }
      break;
   case VKI_SIOCGLIFNUM:
      {
         struct vki_lifnum *p = (struct vki_lifnum *) ARG3;
         PRE_FIELD_READ("ioctl(SIOCGLIFNUM, lifn->lifn_family)",
                        p->lifn_family);
         PRE_FIELD_READ("ioctl(SIOCGLIFNUM, lifn->lifn_flags)",
                        p->lifn_flags);
         PRE_FIELD_WRITE("ioctl(SIOCGLIFNUM, lifn->lifn_count)",
                         p->lifn_count);
      }
      break;

   /* filio */
   case VKI_FIOSETOWN:
      PRE_MEM_READ("ioctl(FIOSETOWN)", ARG3, sizeof(vki_pid_t));
      break;
   case VKI_FIOGETOWN:
      PRE_MEM_WRITE("ioctl(FIOGETOWN)", ARG3, sizeof(vki_pid_t));
      break;

   /* CRYPTO */
   case VKI_CRYPTO_GET_PROVIDER_LIST:
      {
         vki_crypto_get_provider_list_t *pl =
            (vki_crypto_get_provider_list_t *) ARG3;
         PRE_FIELD_READ("ioctl(CRYPTO_GET_PROVIDER_LIST, pl->pl_count)",
                        pl->pl_count);

         if (ML_(safe_to_deref)(pl, sizeof(*pl))) {
            PRE_MEM_WRITE("ioctl(CRYPTO_GET_PROVIDER_LIST)", ARG3,
                          MAX(1, pl->pl_count) *
                          sizeof(vki_crypto_get_provider_list_t));
         }
         /* Save the requested count to unused ARG4 below,
            when we know pre-handler succeeded.
          */
      }
      break; 

   /* dtrace */
   case VKI_DTRACEHIOC_REMOVE:
      break;
   case VKI_DTRACEHIOC_ADDDOF:
      {
         vki_dof_helper_t *dh = (vki_dof_helper_t *) ARG3;
         PRE_MEM_RASCIIZ("ioctl(DTRACEHIOC_ADDDOF, dh->dofhp_mod)",
                         (Addr) dh->dofhp_mod);
         PRE_FIELD_READ("ioctl(DTRACEHIOC_ADDDOF, dh->dofhp_addr",
                        dh->dofhp_addr);
         PRE_FIELD_READ("ioctl(DTRACEHIOC_ADDDOF, dh->dofhp_dof",
                        dh->dofhp_dof);
      }
      break;

   /* devinfo */
   case VKI_DINFOUSRLD:
      /* We should do PRE_MEM_WRITE here but the question is for how many? */
      break;
   case VKI_DINFOIDENT:
      break;

   default:
      ML_(PRE_unknown_ioctl)(tid, ARG2, ARG3);
      break;
   }

   /* Be strict. */
   if (!ML_(fd_allowed)(ARG1, "ioctl", tid, False)) {
      SET_STATUS_Failure(VKI_EBADF);
   } else if (ARG2 == VKI_CRYPTO_GET_PROVIDER_LIST) {
      /* Save the requested count to unused ARG4 now. */
      ARG4 = ARG3;
   }
}

POST(sys_ioctl)
{
   /* Prevent sign extending the switch case values to 64-bits on 64-bits
      architectures. */
   Int cmd = (Int) ARG2;

   switch (cmd /*request*/) {
   /* pools */
   case VKI_POOL_STATUSQ:
      POST_MEM_WRITE(ARG3, sizeof(vki_pool_status_t));
      break;

   /* mntio */
   case VKI_MNTIOC_GETEXTMNTENT:
      {
         struct vki_mntentbuf *embuf = (struct vki_mntentbuf *) ARG3;
         struct vki_extmnttab *mnt = (struct vki_extmnttab *) embuf->mbuf_emp;

         POST_MEM_WRITE((Addr) mnt, sizeof(struct vki_extmnttab));
         if (mnt != NULL) {
            if (mnt->mnt_special != NULL)
               POST_MEM_WRITE((Addr) mnt->mnt_special,
                              VG_(strlen)(mnt->mnt_special) + 1);
            if (mnt->mnt_mountp != NULL)
               POST_MEM_WRITE((Addr) mnt->mnt_mountp,
                              VG_(strlen)(mnt->mnt_mountp) + 1);
            if (mnt->mnt_fstype != NULL)
               POST_MEM_WRITE((Addr) mnt->mnt_fstype,
                              VG_(strlen)(mnt->mnt_fstype) + 1);
            if (mnt->mnt_mntopts != NULL)
               POST_MEM_WRITE((Addr) mnt->mnt_mntopts,
                              VG_(strlen)(mnt->mnt_mntopts) + 1);
            if (mnt->mnt_time != NULL)
               POST_MEM_WRITE((Addr) mnt->mnt_time,
                              VG_(strlen)(mnt->mnt_time) + 1);
         }
      }
      break;

   case VKI_MNTIOC_GETMNTANY:
      {
         struct vki_mntentbuf *embuf = (struct vki_mntentbuf *) ARG3;
         struct vki_mnttab *mnt = (struct vki_mnttab *) embuf->mbuf_emp;

         POST_MEM_WRITE((Addr) mnt, sizeof(struct vki_mnttab));
         if (mnt != NULL) {
            if (mnt->mnt_special != NULL)
               POST_MEM_WRITE((Addr) mnt->mnt_special,
                              VG_(strlen)(mnt->mnt_special) + 1);
            if (mnt->mnt_mountp != NULL)
               POST_MEM_WRITE((Addr) mnt->mnt_mountp,
                              VG_(strlen)(mnt->mnt_mountp) + 1);
            if (mnt->mnt_fstype != NULL)
               POST_MEM_WRITE((Addr) mnt->mnt_fstype,
                              VG_(strlen)(mnt->mnt_fstype) + 1);
            if (mnt->mnt_mntopts != NULL)
               POST_MEM_WRITE((Addr) mnt->mnt_mntopts,
                              VG_(strlen)(mnt->mnt_mntopts) + 1);
            if (mnt->mnt_time != NULL)
               POST_MEM_WRITE((Addr) mnt->mnt_time,
                              VG_(strlen)(mnt->mnt_time) + 1);
         }
      }
      break;

   /* termio/termios */
   case VKI_TCGETA:
      POST_MEM_WRITE(ARG3, sizeof(struct vki_termio));
      break;
   case VKI_TCGETS:
      POST_MEM_WRITE(ARG3, sizeof(struct vki_termios));
      break;
   case VKI_TCSETS:
      break;
   case VKI_TCSETSW:
      break;
   case VKI_TCSETSF:
      break;
   case VKI_TIOCGWINSZ:
      POST_MEM_WRITE(ARG3, sizeof(struct vki_winsize));
      break;
   case VKI_TIOCSWINSZ:
      break;
   case VKI_TIOCGPGRP:
      POST_MEM_WRITE(ARG3, sizeof(vki_pid_t));
      break;
   case VKI_TIOCSPGRP:
      break;
   case VKI_TIOCGSID:
      POST_MEM_WRITE(ARG3, sizeof(vki_pid_t));
      break;
   case VKI_TIOCNOTTY:
   case VKI_TIOCSCTTY:
      break;

   /* STREAMS */
   case VKI_I_PUSH:
      break;
   case VKI_I_FLUSH:
      break;
   case VKI_I_STR:
      {
         struct vki_strioctl *p = (struct vki_strioctl *) ARG3;

         POST_FIELD_WRITE(p->ic_len);
         if ((p->ic_dp != NULL) && (p->ic_len > 0))
            POST_MEM_WRITE((Addr) p->ic_dp, p->ic_len);
      }
      break;
   case VKI_I_FIND:
      break;
   case VKI_I_PEEK:
      {
         struct vki_strpeek *p = (struct vki_strpeek*)ARG3;

         POST_FIELD_WRITE(p->ctlbuf.len);
         POST_FIELD_WRITE(p->databuf.len);
         POST_FIELD_WRITE(p->flags);

         if (p->ctlbuf.buf && p->ctlbuf.len > 0)
            POST_MEM_WRITE((Addr)p->ctlbuf.buf, p->ctlbuf.len);
         if (p->databuf.buf && p->databuf.len > 0)
            POST_MEM_WRITE((Addr)p->databuf.buf, p->databuf.len);
      }
      break;
   case VKI_I_CANPUT:
      break;

   /* sockio */
   case VKI_SIOCGIFCONF:
      {
         struct vki_ifconf *p = (struct vki_ifconf *) ARG3;
         POST_FIELD_WRITE(p->ifc_len);
         POST_FIELD_WRITE(p->ifc_req);
         if ((p->ifc_req != NULL) && (p->ifc_len > 0))
            POST_MEM_WRITE((Addr) p->ifc_req, p->ifc_len);
      }
      break;
   case VKI_SIOCGIFFLAGS:
      {
         struct vki_ifreq *p = (struct vki_ifreq *) ARG3;
         POST_FIELD_WRITE(p->ifr_flags);
      }
      break;
   case VKI_SIOCGIFNETMASK:
      {
         struct vki_ifreq *p = (struct vki_ifreq *) ARG3;
         POST_FIELD_WRITE(p->ifr_addr);
      }
      break;
   case VKI_SIOCGIFNUM:
      POST_MEM_WRITE(ARG3, sizeof(int));
      break;
   case VKI_SIOCGLIFBRDADDR:
      {
         struct vki_lifreq *p = (struct vki_lifreq *) ARG3;
         POST_FIELD_WRITE(p->lifr_addr);
      }
      break;
   case VKI_SIOCGLIFCONF:
      {
         struct vki_lifconf *p = (struct vki_lifconf *) ARG3;
         POST_FIELD_WRITE(p->lifc_len);
         POST_FIELD_WRITE(p->lifc_req);
         if ((p->lifc_req != NULL) && (p->lifc_len > 0))
            POST_MEM_WRITE((Addr) p->lifc_req, p->lifc_len);
      }
      break;
   case VKI_SIOCGLIFFLAGS:
      {
         struct vki_lifreq *p = (struct vki_lifreq *) ARG3;
         POST_FIELD_WRITE(p->lifr_flags);
      }
      break;
   case VKI_SIOCGLIFNETMASK:
      {
         struct vki_lifreq *p = (struct vki_lifreq *) ARG3;
         POST_FIELD_WRITE(p->lifr_addr);
      }
      break;
   case VKI_SIOCGLIFNUM:
      {
         struct vki_lifnum *p = (struct vki_lifnum *) ARG3;
         POST_FIELD_WRITE(p->lifn_count);
      }
      break;  

   /* filio */
   case VKI_FIOSETOWN:
      break;
   case VKI_FIOGETOWN:
      POST_MEM_WRITE(ARG3, sizeof(vki_pid_t));
      break;

   /* CRYPTO */
   case VKI_CRYPTO_GET_PROVIDER_LIST:
      {
         vki_crypto_get_provider_list_t *pl =
            (vki_crypto_get_provider_list_t *) ARG3;

         POST_FIELD_WRITE(pl->pl_count);
         POST_FIELD_WRITE(pl->pl_return_value);

         if ((ARG4 > 0) && (pl->pl_return_value == VKI_CRYPTO_SUCCESS))
            POST_MEM_WRITE((Addr) pl->pl_list, pl->pl_count *
                           sizeof(vki_crypto_provider_entry_t));
      }
      break;

   /* dtrace */
   case VKI_DTRACEHIOC_REMOVE:
   case VKI_DTRACEHIOC_ADDDOF:
      break;

   /* devinfo */
   case VKI_DINFOUSRLD:
      POST_MEM_WRITE(ARG3, RES);
      break;
   case VKI_DINFOIDENT:
      break;

   default:
      /* Not really anything to do since ioctl direction hints are hardly used
         on Solaris. */
      break;
   }
}

PRE(sys_fchownat)
{
   /* int fchownat(int fd, const char *path, uid_t owner, gid_t group,
                   int flag); */

   /* Interpret the first argument as 32-bit value even on 64-bit architecture.
      This is different from Linux, for example, where glibc sign-extends it. */
   Int fd = (Int) ARG1;

   PRINT("sys_fchownat ( %d, %#lx(%s), %ld, %ld, %lu )", fd,
         ARG2, (HChar *) ARG2, SARG3, SARG4, ARG5);
   PRE_REG_READ5(long, "fchownat", int, fd, const char *, path,
                 vki_uid_t, owner, vki_gid_t, group, int, flag);

   if (ARG2)
      PRE_MEM_RASCIIZ("fchownat(path)", ARG2);

   /* Be strict but ignore fd for absolute path. */
   if (fd != VKI_AT_FDCWD
       && ML_(safe_to_deref)((void *) ARG2, 1)
       && ((HChar *) ARG2)[0] != '/'
       && !ML_(fd_allowed)(fd, "fchownat", tid, False))
      SET_STATUS_Failure(VKI_EBADF);
}

PRE(sys_fdsync)
{
   /* int fdsync(int fd, int flag); */
   PRINT("sys_fdsync ( %ld, %ld )", SARG1, SARG2);
   PRE_REG_READ2(long, "fdsync", int, fd, int, flag);

   /* Be strict. */
   if (!ML_(fd_allowed)(ARG1, "fdsync", tid, False))
      SET_STATUS_Failure(VKI_EBADF);
}

PRE(sys_execve)
{
   Int i, j;
   Addr arg_2_check;
   const char* str2 = "execve(argv)";
   const char* str3 = "execve(argv[0])";
   const char* str4 = "execve(argv[i])";
   /* This is a Solaris specific version of the generic pre-execve wrapper. */

#if defined(SOLARIS_EXECVE_SYSCALL_TAKES_FLAGS)
   /* int execve(uintptr_t file, const char **argv, const char **envp,
                 int flags); */
   PRINT("sys_execve ( %#lx, %#lx, %#lx, %ld )", ARG1, ARG2, ARG3, SARG4);
   PRE_REG_READ4(long, "execve", uintptr_t, file, const char **, argv,
                 const char **, envp, int, flags);

#else

   /* int execve(const char *fname, const char **argv, const char **envp); */
   PRINT("sys_execve ( %#lx(%s), %#lx, %#lx )",
         ARG1, (HChar *) ARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "execve", const char *, file, const char **, argv,
                 const char **, envp);
#endif /* SOLARIS_EXECVE_SYSCALL_TAKES_FLAGS */

   Bool ARG1_is_fd = False;
#if defined(SOLARIS_EXECVE_SYSCALL_TAKES_FLAGS)
   if (ARG4 & VKI_EXEC_DESCRIPTOR) {
      ARG1_is_fd = True;
   }
#endif /* SOLARIS_EXECVE_SYSCALL_TAKES_FLAGS */

   if (ARG1_is_fd == False)
      PRE_MEM_RASCIIZ("execve(filename)", ARG1);
   
    /* Erk.  If the exec fails, then the following will have made a mess of
      things which makes it hard for us to continue.  The right thing to do is
      piece everything together again in POST(execve), but that's close to
      impossible.  Instead, we make an effort to check that the execve will
      work before actually doing it. */

   const HChar *fname = (const HChar *) ARG1;
   if (ARG1_is_fd) {
      if (!ML_(fd_allowed)(ARG1, "execve", tid, False)) {
         SET_STATUS_Failure(VKI_EBADF);
         return;
      }

      if (VG_(resolve_filename)(ARG1, &fname) == False) {
         SET_STATUS_Failure(VKI_EBADF);
         return;
      }

      struct vg_stat stats;
      if (VG_(fstat)(ARG1, &stats) != 0) {
         SET_STATUS_Failure(VKI_EBADF);
         return;
      }

      if (stats.nlink > 1)
         VG_(unimplemented)("Syswrap of execve where fd points to a hardlink.");
   }

   arg_2_check = (Addr)ARG2;

   /* argv[] should not be NULL and valid.  */
   PRE_MEM_READ(str2, arg_2_check, sizeof(Addr));

   /* argv[0] should not be NULL and valid.  */
   if (ML_(safe_to_deref)((HChar **) (Addr)arg_2_check, sizeof(HChar *))) {
      Addr argv0 = *(Addr*)arg_2_check;
      PRE_MEM_RASCIIZ( str3, argv0 );
      /* The rest of argv can be NULL or a valid string pointer.  */
      if (VG_(am_is_valid_for_client)(arg_2_check, sizeof(HChar), VKI_PROT_READ)) {
         arg_2_check += sizeof(HChar*);
         ML_(pre_argv_envp)( arg_2_check, tid, str2, str4 );
       }
   } else {
      SET_STATUS_Failure(VKI_EFAULT);
      return;
    }

   if (ARG3 != 0) {
      /* At least the terminating NULL must be addressable. */
      if (!ML_(safe_to_deref)((HChar **) (Addr)ARG3, sizeof(HChar *))) {
         SET_STATUS_Failure(VKI_EFAULT);
         return;
      }
      ML_(pre_argv_envp)( ARG3, tid, "execve(envp)", "execve(envp[i])" );
   }

   /* Check that the name at least begins in client-accessible storage. */
   if (ARG1_is_fd == False) {
      if ((fname == NULL) || !ML_(safe_to_deref)(fname, 1)) {
         SET_STATUS_Failure(VKI_EFAULT);
         return;
      }
   }

   /* Check that the args at least begin in client-accessible storage.
      Solaris disallows to perform the exec without any arguments specified.
    */
   if (!ARG2 /* obviously bogus */ ||
       !VG_(am_is_valid_for_client)(ARG2, 1, VKI_PROT_READ)) {
      SET_STATUS_Failure(VKI_EFAULT);
      return;
   }

   /* Debug-only printing. */
   if (0) {
      VG_(printf)("ARG1 = %#lx(%s)\n", ARG1, fname);
      if (ARG2) {
         Int q;
         HChar** vec = (HChar**)ARG2;

         VG_(printf)("ARG2 = ");
         for (q = 0; vec[q]; q++)
            VG_(printf)("%p(%s) ", vec[q], vec[q]);
         VG_(printf)("\n");
      }
      else
         VG_(printf)("ARG2 = null\n");
   }

   /* Decide whether or not we want to follow along. */
   /* Make 'child_argv' be a pointer to the child's arg vector (skipping the
      exe name) */
   const HChar **child_argv = (const HChar **) ARG2;
   if (child_argv[0] == NULL)
      child_argv = NULL;
   Bool trace_this_child = VG_(should_we_trace_this_child)(fname, child_argv);

   /* Do the important checks:  it is a file, is executable, permissions are
      ok, etc.  We allow setuid executables to run only in the case when
      we are not simulating them, that is, they to be run natively. */
   Bool setuid_allowed = trace_this_child ? False : True;
   SysRes res = VG_(pre_exec_check)(fname, NULL, setuid_allowed);
   if (sr_isError(res)) {
      SET_STATUS_Failure(sr_Err(res));
      return;
   }

   /* If we're tracing the child, and the launcher name looks bogus (possibly
      because launcher.c couldn't figure it out, see comments therein) then we
      have no option but to fail. */
   if (trace_this_child &&
       (!VG_(name_of_launcher) || VG_(name_of_launcher)[0] != '/')) {
      SET_STATUS_Failure(VKI_ECHILD); /* "No child processes." */
      return;
   }

   /* After this point, we can't recover if the execve fails. */
   VG_(debugLog)(1, "syswrap", "Exec of %s\n", fname);

   /* Terminate gdbserver if it is active. */
   if (VG_(clo_vgdb) != Vg_VgdbNo) {
      /* If the child will not be traced, we need to terminate gdbserver to
         cleanup the gdbserver resources (e.g. the FIFO files). If child will
         be traced, we also terminate gdbserver: the new Valgrind will start a
         fresh gdbserver after exec. */
      VG_(gdbserver)(0);
   }

   /* Resistance is futile.  Nuke all other threads.  POSIX mandates this.
      (Really, nuke them all, since the new process will make its own new
      thread.) */
   VG_(nuke_all_threads_except)(tid, VgSrc_ExitThread);
   VG_(reap_threads)(tid);

   /* Set up the child's exe path. */
   const HChar *path = fname;
   const HChar *launcher_basename = NULL;
   if (trace_this_child) {
      /* We want to exec the launcher.  Get its pre-remembered path. */
      path = VG_(name_of_launcher);
      /* VG_(name_of_launcher) should have been acquired by m_main at
         startup. */
      vg_assert(path);

      launcher_basename = VG_(strrchr)(path, '/');
      if (!launcher_basename || launcher_basename[1] == '\0')
         launcher_basename = path;  /* hmm, tres dubious */
      else
         launcher_basename++;
   }

   /* Set up the child's environment.

      Remove the valgrind-specific stuff from the environment so the child
      doesn't get vgpreload_core.so, vgpreload_<tool>.so, etc.  This is done
      unconditionally, since if we are tracing the child, the child valgrind
      will set up the appropriate client environment.  Nb: we make a copy of
      the environment before trying to mangle it as it might be in read-only
      memory (bug #101881).

      Then, if tracing the child, set VALGRIND_LIB for it. */
   HChar **envp = NULL;
   if (ARG3 != 0) {
      envp = VG_(env_clone)((HChar**)ARG3);
      vg_assert(envp != NULL);
      VG_(env_remove_valgrind_env_stuff)(envp, True /*ro_strings*/, NULL);
   }

   if (trace_this_child) {
      /* Set VALGRIND_LIB in ARG3 (the environment). */
      VG_(env_setenv)( &envp, VALGRIND_LIB, VG_(libdir));
   }

   /* Set up the child's args.  If not tracing it, they are simply ARG2.
      Otherwise, they are:

      [launcher_basename] ++ VG_(args_for_valgrind) ++ [ARG1] ++ ARG2[1..],

      except that the first VG_(args_for_valgrind_noexecpass) args are
      omitted. */
   HChar **argv = NULL;
   if (!trace_this_child)
      argv = (HChar **) ARG2;
   else {
      Int tot_args;

      vg_assert(VG_(args_for_valgrind));
      vg_assert(VG_(args_for_valgrind_noexecpass) >= 0);
      vg_assert(VG_(args_for_valgrind_noexecpass)
                   <= VG_(sizeXA)(VG_(args_for_valgrind)));

      /* How many args in total will there be? */
      /* launcher basename */
      tot_args = 1;
      /* V's args */
      tot_args += VG_(sizeXA)(VG_(args_for_valgrind));
      tot_args -= VG_(args_for_valgrind_noexecpass);
      /* name of client exe */
      tot_args++;
      /* args for client exe, skipping [0] */
      HChar **arg2copy = (HChar **) ARG2;
      if (arg2copy[0] != NULL)
         for (i = 1; arg2copy[i]; i++)
            tot_args++;
      /* allocate */
      argv = VG_(malloc)("syswrap.exec.5", (tot_args + 1) * sizeof(HChar*));
      /* copy */
      j = 0;
      argv[j++] = CONST_CAST(HChar *, launcher_basename);
      for (i = 0; i < VG_(sizeXA)(VG_(args_for_valgrind)); i++) {
         if (i < VG_(args_for_valgrind_noexecpass))
            continue;
         argv[j++] = *(HChar**)VG_(indexXA)(VG_(args_for_valgrind), i);
      }
      argv[j++] = CONST_CAST(HChar *, fname);
      if (arg2copy[0] != NULL)
         for (i = 1; arg2copy[i]; i++)
            argv[j++] = arg2copy[i];
      argv[j++] = NULL;
      /* check */
      vg_assert(j == tot_args + 1);
   }

   /* Set the signal state up for exec.

      We need to set the real signal state to make sure the exec'd process
      gets SIG_IGN properly.

      Also set our real sigmask to match the client's sigmask so that the
      exec'd child will get the right mask.  First we need to clear out any
      pending signals so they they don't get delivered, which would confuse
      things.

      XXX This is a bug - the signals should remain pending, and be delivered
      to the new process after exec.  There's also a race-condition, since if
      someone delivers us a signal between the sigprocmask and the execve,
      we'll still get the signal. Oh well.
   */
   {
      vki_sigset_t allsigs;
      vki_siginfo_t info;

      /* What this loop does: it queries SCSS (the signal state that the
         client _thinks_ the kernel is in) by calling VG_(do_sys_sigaction),
         and modifies the real kernel signal state accordingly. */
      for (i = 1; i < VG_(max_signal); i++) {
         vki_sigaction_fromK_t sa_f;
         vki_sigaction_toK_t   sa_t;
         VG_(do_sys_sigaction)(i, NULL, &sa_f);
         VG_(convert_sigaction_fromK_to_toK)(&sa_f, &sa_t);
         VG_(sigaction)(i, &sa_t, NULL);
      }

      VG_(sigfillset)(&allsigs);
      while (VG_(sigtimedwait_zero)(&allsigs, &info) > 0)
         ;

      ThreadState *tst = VG_(get_ThreadState)(tid);
      VG_(sigprocmask)(VKI_SIG_SETMASK, &tst->sig_mask, NULL);
   }

   /* Debug-only printing. */
   if (0) {
      HChar **cpp;
      VG_(printf)("exec: %s\n", path);
      for (cpp = argv; cpp && *cpp; cpp++)
         VG_(printf)("argv: %s\n", *cpp);
      if (0)
         for (cpp = envp; cpp && *cpp; cpp++)
            VG_(printf)("env: %s\n", *cpp);
   }

#if defined(SOLARIS_EXECVE_SYSCALL_TAKES_FLAGS)
   res = VG_(do_syscall4)(__NR_execve, (UWord) path, (UWord) argv,
                          (UWord) envp, ARG4 & ~VKI_EXEC_DESCRIPTOR);
#else
   res = VG_(do_syscall3)(__NR_execve, (UWord) path, (UWord) argv,
                          (UWord) envp);
#endif /* SOLARIS_EXECVE_SYSCALL_TAKES_FLAGS */
   SET_STATUS_from_SysRes(res);

   /* If we got here, then the execve failed.  We've already made way too much
      of a mess to continue, so we have to abort. */
   vg_assert(FAILURE);
#if defined(SOLARIS_EXECVE_SYSCALL_TAKES_FLAGS)
   if (ARG1_is_fd)
      VG_(message)(Vg_UserMsg, "execve(%ld, %#lx, %#lx, %lu) failed, "
                   "errno %ld\n", SARG1, ARG2, ARG3, ARG4, ERR);
   else
      VG_(message)(Vg_UserMsg, "execve(%#lx(%s), %#lx, %#lx, %ld) failed, errno"
                   " %lu\n", ARG1, (HChar *) ARG1, ARG2, ARG3, SARG4, ERR);
#else
   VG_(message)(Vg_UserMsg, "execve(%#lx(%s), %#lx, %#lx) failed, errno %lu\n",
                ARG1, (HChar *) ARG1, ARG2, ARG3, ERR);
#endif /* SOLARIS_EXECVE_SYSCALL_TAKES_FLAGS */
   VG_(message)(Vg_UserMsg, "EXEC FAILED: I can't recover from "
                            "execve() failing, so I'm dying.\n");
   VG_(message)(Vg_UserMsg, "Add more stringent tests in PRE(sys_execve), "
                            "or work out how to recover.\n");
   VG_(exit)(101);
   /*NOTREACHED*/
}

static void pre_mem_read_flock(ThreadId tid, struct vki_flock *lock)
{
   PRE_FIELD_READ("fcntl(lock->l_type)", lock->l_type);
   PRE_FIELD_READ("fcntl(lock->l_whence)", lock->l_whence);
   PRE_FIELD_READ("fcntl(lock->l_start)", lock->l_start);
   PRE_FIELD_READ("fcntl(lock->l_len)", lock->l_len);
}

#if defined(VGP_x86_solaris)
static void pre_mem_read_flock64(ThreadId tid, struct vki_flock64 *lock)
{
   PRE_FIELD_READ("fcntl(lock->l_type)", lock->l_type);
   PRE_FIELD_READ("fcntl(lock->l_whence)", lock->l_whence);
   PRE_FIELD_READ("fcntl(lock->l_start)", lock->l_start);
   PRE_FIELD_READ("fcntl(lock->l_len)", lock->l_len);
}
#endif /* VGP_x86_solaris */

PRE(sys_fcntl)
{
   /* int fcntl(int fildes, int cmd, ...); */

   switch (ARG2 /*cmd*/) {
   /* These ones ignore ARG3. */
   case VKI_F_GETFD:
   case VKI_F_GETFL:
   case VKI_F_GETXFL:
      PRINT("sys_fcntl ( %ld, %ld )", SARG1, SARG2);
      PRE_REG_READ2(long, "fcntl", int, fildes, int, cmd);
      break;

   /* These ones use ARG3 as "arg". */
   case VKI_F_DUPFD:
   case VKI_F_DUPFD_CLOEXEC:
   case VKI_F_SETFD:
   case VKI_F_SETFL:
   case VKI_F_DUP2FD:
   case VKI_F_BADFD:
      PRINT("sys_fcntl ( %ld, %ld, %ld )", SARG1, SARG2, SARG3);
      PRE_REG_READ3(long, "fcntl", int, fildes, int, cmd, int, arg);
      /* Check if a client program isn't going to poison any of V's output
         fds. */
      if (ARG2 == VKI_F_DUP2FD &&
          !ML_(fd_allowed)(ARG3, "fcntl(F_DUP2FD)", tid, False)) {
         SET_STATUS_Failure(VKI_EBADF);
         return;
      }
      break;

   /* These ones use ARG3 as "native lock" (input only). */
   case VKI_F_SETLK:
   case VKI_F_SETLKW:
   case VKI_F_ALLOCSP:
   case VKI_F_FREESP:
   case VKI_F_SETLK_NBMAND:
      PRINT("sys_fcntl ( %ld, %ld, %#lx )", SARG1, SARG2, ARG3);
      PRE_REG_READ3(long, "fcntl", int, fildes, int, cmd,
                    struct flock *, lock);
      pre_mem_read_flock(tid, (struct vki_flock*)ARG3);
      break;

   /* This one uses ARG3 as "native lock" (input&output). */
   case VKI_F_GETLK:
      PRINT("sys_fcntl ( %ld, %ld, %#lx )", SARG1, SARG2, ARG3);
      PRE_REG_READ3(long, "fcntl", int, fildes, int, cmd,
                    struct flock *, lock);
      pre_mem_read_flock(tid, (struct vki_flock*)ARG3);
      PRE_MEM_WRITE("fcntl(lock)", ARG3, sizeof(struct vki_flock));
      break;

#if defined(VGP_x86_solaris)
   /* These ones use ARG3 as "transitional 64b lock" (input only). */
   case VKI_F_SETLK64:
   case VKI_F_SETLKW64:
   case VKI_F_ALLOCSP64:
   case VKI_F_FREESP64:
   case VKI_F_SETLK64_NBMAND:
      PRINT("sys_fcntl ( %ld, %ld, %#lx )", SARG1, SARG2, ARG3);
      PRE_REG_READ3(long, "fcntl", int, fildes, int, cmd,
                    struct flock64 *, lock);
      pre_mem_read_flock64(tid, (struct vki_flock64*)ARG3);
      break;

   /* This one uses ARG3 as "transitional 64b lock" (input&output). */
   case VKI_F_GETLK64:
      PRINT("sys_fcntl ( %ld, %ld, %#lx )", SARG1, SARG2, ARG3);
      PRE_REG_READ3(long, "fcntl", int, fildes, int, cmd,
                    struct flock64 *, lock);
      pre_mem_read_flock64(tid, (struct vki_flock64*)ARG3);
      PRE_MEM_WRITE("fcntl(lock)", ARG3, sizeof(struct vki_flock64));
      break;
#endif /* VGP_x86_solaris */

   /* These ones use ARG3 as "fshare". */
   case VKI_F_SHARE:
   case VKI_F_UNSHARE:
   case VKI_F_SHARE_NBMAND:
      PRINT("sys_fcntl[ARG3=='fshare'] ( %ld, %ld, %#lx )", SARG1, SARG2, ARG3);
      PRE_REG_READ3(long, "fcntl", int, fildes, int, cmd,
                    struct fshare *, sh);
      PRE_MEM_READ("fcntl(fshare)", ARG3, sizeof(struct vki_fshare));
      break;

   default:
      VG_(unimplemented)("Syswrap of the fcntl call with cmd %ld.", SARG2);
      /*NOTREACHED*/
      break;
   }

   if (ARG2 == VKI_F_SETLKW
#if defined(VGP_x86_solaris)
       || ARG2 == VKI_F_SETLKW64
#endif /* VGP_x86_solaris */
       )
      *flags |= SfMayBlock;

   /* Be strict. */
   if (!ML_(fd_allowed)(ARG1, "fcntl", tid, False))
      SET_STATUS_Failure(VKI_EBADF);
}

POST(sys_fcntl)
{
   switch (ARG2 /*cmd*/) {
   case VKI_F_DUPFD:
      if (!ML_(fd_allowed)(RES, "fcntl(F_DUPFD)", tid, True)) {
         VG_(close)(RES);
         SET_STATUS_Failure(VKI_EMFILE);
      } else if (VG_(clo_track_fds))
         ML_(record_fd_open_named)(tid, RES);
      break;

   case VKI_F_DUPFD_CLOEXEC:
      if (!ML_(fd_allowed)(RES, "fcntl(F_DUPFD_CLOEXEC)", tid, True)) {
         VG_(close)(RES);
         SET_STATUS_Failure(VKI_EMFILE);
      } else if (VG_(clo_track_fds))
         ML_(record_fd_open_named)(tid, RES);
      break;

   case VKI_F_DUP2FD:
      if (!ML_(fd_allowed)(RES, "fcntl(F_DUP2FD)", tid, True)) {
         VG_(close)(RES);
         SET_STATUS_Failure(VKI_EMFILE);
      } else if (VG_(clo_track_fds))
         ML_(record_fd_open_named)(tid, RES);
      break;

   /* This one uses ARG3 as "native lock" (input&output). */
   case VKI_F_GETLK:
      POST_MEM_WRITE(ARG3, sizeof(struct vki_flock));
      break;

#if defined(VGP_x86_solaris)
   /* This one uses ARG3 as "transitional 64b lock" (input&output). */
   case VKI_F_GETLK64:
      POST_MEM_WRITE(ARG3, sizeof(struct vki_flock64));
      break;
#endif /* VGP_x86_solaris */

   default:
      break;
   }
}

PRE(sys_renameat)
{
   /* int renameat(int fromfd, const char *old, int tofd, const char *new); */

   /* Interpret the first and third arguments as 32-bit values even on 64-bit
      architecture. This is different from Linux, for example, where glibc
      sign-extends them. */
   Int fromfd = (Int) ARG1;
   Int tofd = (Int) ARG3;

   *flags |= SfMayBlock;
   PRINT("sys_renameat ( %d, %#lx(%s), %d, %#lx(%s) )", fromfd,
         ARG2, (HChar *) ARG2, tofd, ARG4, (HChar *) ARG4);
   PRE_REG_READ4(long, "renameat", int, fromfd, const char *, old,
                 int, tofd, const char *, new);

   PRE_MEM_RASCIIZ("renameat(old)", ARG2);
   PRE_MEM_RASCIIZ("renameat(new)", ARG4);

   /* Be strict but ignore fromfd/tofd for absolute old/new. */
   if (fromfd != VKI_AT_FDCWD
       && ML_(safe_to_deref)((void *) ARG2, 1)
       && ((HChar *) ARG2)[0] != '/'
       && !ML_(fd_allowed)(fromfd, "renameat", tid, False)) {
      SET_STATUS_Failure(VKI_EBADF);
   }
   if (tofd != VKI_AT_FDCWD
       && ML_(safe_to_deref)((void *) ARG4, 1)
       && ((HChar *) ARG4)[0] != '/'
       && !ML_(fd_allowed)(tofd, "renameat", tid, False)) {
      SET_STATUS_Failure(VKI_EBADF);
   }
}

PRE(sys_unlinkat)
{
   /* int unlinkat(int dirfd, const char *pathname, int flags); */

   /* Interpret the first argument as 32-bit value even on 64-bit architecture.
      This is different from Linux, for example, where glibc sign-extends it. */
   Int dfd = (Int) ARG1;

   *flags |= SfMayBlock;
   PRINT("sys_unlinkat ( %d, %#lx(%s), %ld )", dfd, ARG2, (HChar *) ARG2,
         SARG3);
   PRE_REG_READ3(long, "unlinkat", int, dirfd, const char *, pathname,
                 int, flags);
   PRE_MEM_RASCIIZ("unlinkat(pathname)", ARG2);

   /* Be strict but ignore dfd for absolute pathname. */
   if (dfd != VKI_AT_FDCWD
       && ML_(safe_to_deref)((void *) ARG2, 1)
       && ((HChar *) ARG2)[0] != '/'
       && !ML_(fd_allowed)(dfd, "unlinkat", tid, False))
      SET_STATUS_Failure(VKI_EBADF);
}

PRE(sys_fstatat)
{
   /* int fstatat(int fildes, const char *path, struct stat *buf,
                    int flag); */

   /* Interpret the first argument as 32-bit value even on 64-bit architecture.
      This is different from Linux, for example, where glibc sign-extends it. */
   Int fd = (Int) ARG1;

   PRINT("sys_fstatat ( %d, %#lx(%s), %#lx, %ld )", fd, ARG2,
         (HChar *) ARG2, ARG3, SARG4);
   PRE_REG_READ4(long, "fstatat", int, fildes, const char *, path,
                 struct stat *, buf, int, flag);
   if (ARG2) {
      /* Only test ARG2 if it isn't NULL.  The kernel treats the NULL-case as
         fstat(fildes, buf). */
      PRE_MEM_RASCIIZ("fstatat(path)", ARG2);
   }
   PRE_MEM_WRITE("fstatat(buf)", ARG3, sizeof(struct vki_stat));

   /* Be strict but ignore fildes for absolute path. */
   if (fd != VKI_AT_FDCWD
       && ML_(safe_to_deref)((void *) ARG2, 1)
       && ((HChar *) ARG2)[0] != '/'
       && !ML_(fd_allowed)(fd, "fstatat", tid, False))
      SET_STATUS_Failure(VKI_EBADF);
}

POST(sys_fstatat)
{
   POST_MEM_WRITE(ARG3, sizeof(struct vki_stat));
}

PRE(sys_openat)
{
   /* int openat(int fildes, const char *filename, int flags);
      int openat(int fildes, const char *filename, int flags, mode_t mode); */

   /* Interpret the first argument as 32-bit value even on 64-bit architecture.
      This is different from Linux, for example, where glibc sign-extends it. */
   Int fd = (Int) ARG1;

   if (ARG3 & VKI_O_CREAT) {
      /* 4-arg version */
      PRINT("sys_openat ( %d, %#lx(%s), %ld, %ld )", fd, ARG2, (HChar *) ARG2,
            SARG3, SARG4);
      PRE_REG_READ4(long, "openat", int, fildes, const char *, filename,
                    int, flags, vki_mode_t, mode);
   }
   else {
      /* 3-arg version */
      PRINT("sys_openat ( %d, %#lx(%s), %ld )", fd, ARG2, (HChar *) ARG2,
            SARG3);
      PRE_REG_READ3(long, "openat", int, fildes, const char *, filename,
                    int, flags);
   }

   PRE_MEM_RASCIIZ("openat(filename)", ARG2);

   /* Be strict but ignore fildes for absolute pathname. */
   if (fd != VKI_AT_FDCWD
       && ML_(safe_to_deref)((void *) ARG2, 1)
       && ((HChar *) ARG2)[0] != '/'
       && !ML_(fd_allowed)(fd, "openat", tid, False)) {
      SET_STATUS_Failure(VKI_EBADF);
      return;
   }

   if (ML_(handle_auxv_open)(status, (const HChar *) ARG2, ARG3))
      return;

   if (handle_psinfo_open(status, True /*use_openat*/, (const HChar *) ARG2,
                          fd, ARG3, ARG4))
      return;

#if defined(SOLARIS_PROC_CMDLINE)
   if (handle_cmdline_open(status, (const HChar *) ARG2))
      return;
#endif /* SOLARIS_PROC_CMDLINE */

   *flags |= SfMayBlock;
}

POST(sys_openat)
{
   if (!ML_(fd_allowed)(RES, "openat", tid, True)) {
      VG_(close)(RES);
      SET_STATUS_Failure(VKI_EMFILE);
   }
   else if (VG_(clo_track_fds))
      ML_(record_fd_open_with_given_name)(tid, RES, (HChar*)ARG2);
}

PRE(sys_tasksys)
{
   /* Kernel: long tasksys(int code, projid_t projid, uint_t flags,
                           void *projidbuf, size_t pbufsz);
    */
   switch (ARG1 /*code*/) {
   case 0:
      /* Libc: taskid_t settaskid(projid_t project, uint_t flags); */
      PRINT("sys_tasksys ( %ld, %ld, %lu )", SARG1, SARG2, ARG3);
      PRE_REG_READ3(long, SC2("tasksys", "settaskid"), int, code,
                    vki_projid_t, projid, vki_uint_t, flags);
      break;
   case 1:
      /* Libc: taskid_t gettaskid(void); */
      PRINT("sys_tasksys ( %ld )", SARG1);
      PRE_REG_READ1(long, SC2("tasksys", "gettaskid"), int, code);
      break;
   case 2:
      /* Libc: projid_t getprojid(void); */
      PRINT("sys_tasksys ( %ld )", SARG1);
      PRE_REG_READ1(long, SC2("tasksys", "getprojid"), int, code);
      break;
   case 3:
      /* Libproject: size_t projlist(id_t *idbuf, size_t idbufsz); */
      PRINT("sys_tasksys ( %ld, %#lx, %lu )", SARG1, ARG4, ARG5);
      PRE_REG_READ3(long, SC2("tasksys", "projlist"), int, code,
                    vki_id_t *, idbuf, vki_size_t, idbufsz);
      PRE_MEM_WRITE("tasksys(idbuf)", ARG4, ARG5);
      break;
   default:
      VG_(unimplemented)("Syswrap of the tasksys call with code %ld.", SARG1);
      /*NOTREACHED*/
      break;
   }
}

POST(sys_tasksys)
{
   switch (ARG1 /*code*/) {
   case 0:
   case 1:
   case 2:
      break;
   case 3:
      if ((ARG4 != 0) && (ARG5 != 0))
         POST_MEM_WRITE(ARG4, MIN(RES, ARG5));
      break;
   default:
      vg_assert(0);
      break;
   }
}

PRE(sys_lwp_park)
{
   /* Kernel: int lwp_park(int which, uintptr_t arg1, uintptr_t arg2);
    */
   *flags |= SfMayBlock;
   switch (ARG1 /*which*/) {
   case 0:
      /* Libc: int lwp_park(timespec_t *timeout, id_t lwpid); */
      PRINT("sys_lwp_park ( %ld, %#lx, %ld )", SARG1, ARG2, SARG3);
      PRE_REG_READ3(long, SC2("lwp_park", "lwp_park"), int, which,
                    timespec_t *, timeout, vki_id_t, lwpid);
      if (ARG2) {
         PRE_MEM_READ("lwp_park(timeout)", ARG2, sizeof(vki_timespec_t));
         /*PRE_MEM_WRITE("lwp_park(timeout)", ARG2,
                         sizeof(vki_timespec_t));*/
      }
      break;
   case 1:
      /* Libc: int lwp_unpark(id_t lwpid); */
      PRINT("sys_lwp_park ( %ld, %ld )", SARG1, SARG2);
      PRE_REG_READ2(long, SC2("lwp_park", "lwp_unpark"), int, which,
                    vki_id_t, lwpid);
      break;
   case 2:
      /* Libc: int lwp_unpark_all(id_t *lwpid, int nids); */
      PRINT("sys_lwp_park ( %ld, %#lx, %ld )", SARG1, ARG2, SARG3);
      PRE_REG_READ3(long, SC2("lwp_park", "lwp_unpark_all"), int, which,
                    id_t *, lwpid, int, nids);
      PRE_MEM_READ("lwp_park(lwpid)", ARG2, ARG3 * sizeof(vki_id_t));
      break;
   default:
      VG_(unimplemented)("Syswrap of the lwp_park call with which %ld.", SARG1);
      /*NOTREACHED*/
      break;
   }
}

POST(sys_lwp_park)
{
   switch (ARG1 /*which*/) {
   case 0:
      if (ARG2)
         POST_MEM_WRITE(ARG2, sizeof(vki_timespec_t));
      break;
   case 1:
   case 2:
      break;
   default:
      vg_assert(0);
      break;
   }
}

PRE(sys_sendfilev)
{
   /* Kernel: ssize_t sendfilev(int opcode, int fd,
                                const struct sendfilevec *vec,
                                int sfvcnt, size_t *xferred);
    */
   PRINT("sys_sendfilev ( %ld, %ld, %#lx, %ld, %#lx )",
         SARG1, SARG2, ARG3, SARG4, ARG5);

   switch (ARG1 /*opcode*/) {
   case VKI_SENDFILEV:
      {
         PRE_REG_READ5(long, "sendfilev", int, opcode, int, fd,
                       const struct vki_sendfilevec *, vec,
                       int, sfvcnt, vki_size_t *, xferred);

         PRE_MEM_READ("sendfilev(vec)", ARG3,
                      ARG4 * sizeof(struct vki_sendfilevec));
         PRE_MEM_WRITE("sendfilev(xferred)", ARG5, sizeof(vki_size_t));

         struct vki_sendfilevec *vec = (struct vki_sendfilevec *) ARG3;
         if (ML_(safe_to_deref)(vec, ARG4 *
                                sizeof(struct vki_sendfilevec))) {
            UInt i;
            for (i = 0; i < ARG4; i++) {
               HChar desc[35];    // large enough
               if (vec[i].sfv_fd == VKI_SFV_FD_SELF) {
                  VG_(snprintf)(desc, sizeof(desc),
                                "sendfilev(vec[%u].sfv_off", i);
                  PRE_MEM_READ(desc, vec[i].sfv_off, vec[i].sfv_len);
               } else {
                  VG_(snprintf)(desc, sizeof(desc),
                                "sendfilev(vec[%u].sfv_fd)", i);
                  if (!ML_(fd_allowed)(vec[i].sfv_fd, desc, tid, False))
                     SET_STATUS_Failure(VKI_EBADF);
               }
            }
         }
      }
      break;
   case VKI_SENDFILEV64:
      {
         PRE_REG_READ5(long, "sendfilev", int, opcode, int, fd,
                       const struct vki_sendfilevec64 *, vec,
                       int, sfvcnt, vki_size_t *, xferred);

         PRE_MEM_READ("sendfilev(vec)", ARG3,
                      ARG4 * sizeof(struct vki_sendfilevec64));
         PRE_MEM_WRITE("sendfilev(xferred)", ARG5, sizeof(vki_size_t));

         struct vki_sendfilevec64 *vec64 =
            (struct vki_sendfilevec64 *) ARG3;
         if (ML_(safe_to_deref)(vec64, ARG4 *
                                sizeof(struct vki_sendfilevec64))) {
            UInt i;
            for (i = 0; i < ARG4; i++) {
               HChar desc[35];    // large enough
               if (vec64[i].sfv_fd == VKI_SFV_FD_SELF) {
                  VG_(snprintf)(desc, sizeof(desc),
                                "sendfilev(vec[%u].sfv_off", i);
                  PRE_MEM_READ(desc, vec64[i].sfv_off, vec64[i].sfv_len);
               } else {
                  VG_(snprintf)(desc, sizeof(desc),
                                "sendfilev(vec[%u].sfv_fd)", i);
                  if (!ML_(fd_allowed)(vec64[i].sfv_fd, desc,
                                       tid, False))
                     SET_STATUS_Failure(VKI_EBADF);
               }
            }
         }
      }
      break;
   default:
      VG_(unimplemented)("Syswrap of the sendfilev call with "
                         "opcode %ld.", SARG1);
      /*NOTREACHED*/
      break;
   }

   /* Be strict. */
   if (!ML_(fd_allowed)(ARG2, "sendfilev(fd)", tid, False))
      SET_STATUS_Failure(VKI_EBADF);

   *flags |= SfMayBlock;
}

POST(sys_sendfilev)
{
   POST_MEM_WRITE(ARG5, sizeof(vki_size_t));
}

#if defined(SOLARIS_LWP_NAME_SYSCALL)
PRE(sys_lwp_name)
{
   /* int lwp_name(int opcode, id_t lwpid, char *name, size_t len); */
   PRINT("sys_lwp_name ( %ld, %ld, %#lx, %lu )", SARG1, SARG2, ARG3, ARG4);

   switch (ARG1 /*opcode*/) {
   case 0:
      /* lwp_setname */
      PRE_REG_READ3(long, "lwp_name", int, opcode, vki_id_t, lwpid,
                    char *, name);
      PRE_MEM_RASCIIZ("lwp_name(name)", ARG3);
      break;
   case 1:
      /* lwp_getname */
      PRE_REG_READ4(long, "lwp_name", int, opcode, vki_id_t, lwpid,
                    char *, name, vki_size_t, len);
      PRE_MEM_WRITE("lwp_name(name)", ARG3, ARG4);
      break;
   default:
      VG_(unimplemented)("Syswrap of the lwp_name call with opcode %ld.", SARG1);
      /*NOTREACHED*/
      break;
   }
}

POST(sys_lwp_name)
{
   switch (ARG1 /*opcode*/) {
   case 0:
      if (ARG3) { // Paranoia
         const HChar *new_name = (const HChar *) ARG3;
         ThreadState *tst = VG_(get_ThreadState)(tid);
         SizeT new_len = VG_(strlen)(new_name);

         /* Don't bother reusing the memory. This is a rare event. */
         tst->thread_name = VG_(realloc)("syswrap.lwp_name", tst->thread_name,
                                         new_len + 1);
         VG_(strcpy)(tst->thread_name, new_name);
      }
      break;
   case 1:
      POST_MEM_WRITE(ARG3, VG_(strlen)((HChar *) ARG3) + 1);
      break;
   default:
      vg_assert(0);
      break;
   }
}
#endif /* SOLARIS_LWP_NAME_SYSCALL */

PRE(sys_privsys)
{
   /* Kernel: int privsys(int code, priv_op_t op, priv_ptype_t type,
                          void *buf, size_t bufsize, int itype);
    */
   switch (ARG1 /*code*/) {
   case VKI_PRIVSYS_SETPPRIV:
      /* Libc: int setppriv(priv_op_t op, priv_ptype_t type,
                            const priv_set_t *pset);
       */
      PRINT("sys_privsys ( %ld, %ld, %ld, %#lx, %lu )", SARG1, SARG2, SARG3,
            ARG4, ARG5);
      PRE_REG_READ5(long, SC2("privsys", "setppriv"), int, code,
                    vki_priv_op_t, op, vki_priv_ptype_t, type,
                    const priv_set_t *, pset, vki_size_t, bufsize);
      PRE_MEM_READ("privsys(pset)", ARG4, ARG5);
      break;
   case VKI_PRIVSYS_GETPPRIV:
      /* Libc: int getppriv(priv_ptype_t type, priv_set_t *pset);
               priv_set_t *pset -> void *buf
       */
      PRINT("sys_privsys ( %ld, %ld, %ld, %#lx, %lu )", SARG1, SARG2, SARG3,
            ARG4, ARG5);
      PRE_REG_READ5(long, SC2("privsys", "getppriv"), int, code,
            vki_priv_op_t, op, vki_priv_ptype_t, type, priv_set_t *, pset,
            vki_size_t, bufsize);
      PRE_MEM_WRITE("privsys(pset)", ARG4, ARG5);
      break;
   case VKI_PRIVSYS_GETIMPLINFO:
      /* Libc: int getprivinfo(priv_impl_info_t *buf, size_t bufsize);
               priv_impl_info_t *buf -> void *buf
       */
      PRINT("sys_privsys ( %ld, %ld, %ld, %#lx, %lu )", SARG1, SARG2, SARG3,
            ARG4, ARG5);
      PRE_REG_READ5(long, SC2("privsys", "getprivinfo"), int, code,
            vki_priv_op_t, op, vki_priv_ptype_t, type,
            priv_impl_info_t *, buf, vki_size_t, bufsize);
      PRE_MEM_WRITE("privsys(buf)", ARG4, ARG5);
      break;
   case VKI_PRIVSYS_SETPFLAGS:
      /* Libc: int setpflags(uint_t flag, uint_t val);
               uint_t flag -> priv_op_t op
               uint_t val -> priv_ptype_t type
       */
      PRINT("sys_privsys ( %ld, %lu, %lu )", SARG1, ARG2, ARG3);
      PRE_REG_READ3(long, SC2("privsys", "setpflags"), int, code,
                    vki_uint_t, flag, vki_uint_t, val);
      break;
   case VKI_PRIVSYS_GETPFLAGS:
      /* Libc: uint_t getpflags(uint_t flag);
               uint_t flag -> priv_op_t op
       */
      PRINT("sys_privsys ( %ld, %lu )", SARG1, ARG2);
      PRE_REG_READ2(long, SC2("privsys", "setpflags"), int, code,
                    vki_uint_t, flag);
      break;
   case VKI_PRIVSYS_ISSETUGID:
      /* Libc: int issetugid(void); */
      PRINT("sys_privsys ( %ld )", SARG1);
      PRE_REG_READ1(long, SC2("privsys", "issetugid"), int, code);
      break;
   case VKI_PRIVSYS_PFEXEC_REG:
      /* Libc: int register_pfexec(int did);
               int did -> priv_op_t op
       */
      PRINT("sys_privsys ( %ld, %ld )", SARG1, SARG2);
      PRE_REG_READ2(long, SC2("privsys", "register_pfexec"), int, code,
                    int, did);
      break;
   case VKI_PRIVSYS_PFEXEC_UNREG:
      /* Libc: int unregister_pfexec(int did); */
      PRINT("sys_privsys ( %ld, %ld )", SARG1, SARG2);
      PRE_REG_READ2(long, SC2("privsys", "unregister_pfexec"), int, code,
                    int, did);
      break;
   default:
      VG_(unimplemented)("Syswrap of the privsys call with code %ld.", SARG1);
      /*NOTREACHED*/
      break;
   }

   /* Be strict. */
   if ((ARG1 == VKI_PRIVSYS_PFEXEC_REG ||
        ARG1 == VKI_PRIVSYS_PFEXEC_UNREG) &&
       !ML_(fd_allowed)(ARG2, "privsys", tid, False))
      SET_STATUS_Failure(VKI_EBADF);
}

POST(sys_privsys)
{
   switch (ARG1 /*code*/) {
   case VKI_PRIVSYS_SETPPRIV:
      break;
   case VKI_PRIVSYS_GETPPRIV:
      POST_MEM_WRITE(ARG4, sizeof(vki_priv_set_t));
      break;
   case VKI_PRIVSYS_GETIMPLINFO:
      /* The kernel copy outs data of size min(bufsize, privinfosize).
         Unfortunately, it does not seem to be possible to easily obtain the
         privinfosize value.  The code below optimistically marks all ARG5
         bytes (aka bufsize) as written by the kernel. */
      POST_MEM_WRITE(ARG4, ARG5);
      break;
   case VKI_PRIVSYS_SETPFLAGS:
   case VKI_PRIVSYS_GETPFLAGS:
   case VKI_PRIVSYS_ISSETUGID:
   case VKI_PRIVSYS_PFEXEC_REG:
   case VKI_PRIVSYS_PFEXEC_UNREG:
      break;
   default:
      vg_assert(0);
      break;
   }
}

PRE(sys_ucredsys)
{
   /* Kernel: int ucredsys(int code, int obj, void *buf); */
   PRINT("sys_ucredsys ( %ld, %ld, %#lx )", SARG1, SARG2, ARG3);

   switch (ARG1 /*code*/) {
   case VKI_UCREDSYS_UCREDGET:
      /* Libc: ucred_t *ucred_get(pid_t pid); */
      PRE_REG_READ3(long, SC2("ucredsys", "ucredget"), int, code,
                    vki_pid_t, pid, vki_ucred_t *, buf);
      PRE_MEM_WRITE("ucredsys(buf)", ARG3, sizeof(vki_ucred_t));
      break;

   case VKI_UCREDSYS_GETPEERUCRED:
      /* Libc: int getpeerucred(int fd, ucred_t **ucred); */
      PRE_REG_READ3(long, SC2("ucredsys", "getpeerucred"), int, code,
                    int, fd, vki_ucred_t *, buf);
      PRE_MEM_WRITE("ucredsys(buf)", ARG3, sizeof(vki_ucred_t));

      /* Be strict. */
      if (!ML_(fd_allowed)(ARG2, "ucredsys", tid, False))
         SET_STATUS_Failure(VKI_EBADF);
      break;

   default:
      VG_(unimplemented)("Syswrap of the ucredsys call with code %ld.", SARG1);
      /*NOTREACHED*/
      break;
   }
}

POST(sys_ucredsys)
{
   switch (ARG1 /*code*/) {
   case VKI_UCREDSYS_UCREDGET:
   case VKI_UCREDSYS_GETPEERUCRED:
      vg_assert(ARG3 != 0);
      POST_MEM_WRITE(ARG3, ((vki_ucred_t *) ARG3)->uc_size);
      break;

   default:
      vg_assert(0);
      break;
   }
}

PRE(sys_sysfs)
{
   /* Kernel: int sysfs(int opcode, long a1, long a2); */
   PRINT("sys_sysfs ( %ld, %ld, %ld )", SARG1, SARG2, SARG3);

   switch (ARG1 /*opcode*/) {
   case VKI_GETFSIND:
      /* Libc: int sysfs(int opcode, const char *fsname); */
      PRE_REG_READ2(long, SC2("sysfs", "getfsind"), int, opcode,
                    const char *, fsname);
      PRE_MEM_RASCIIZ("sysfs(fsname)", ARG2);
      break;
   case VKI_GETFSTYP:
      /* Libc: int sysfs(int opcode, int fs_index, char *buf); */
      PRE_REG_READ3(long, SC2("sysfs", "getfstyp"), int, opcode,
                    int, fs_index, char *, buf);
      PRE_MEM_WRITE("sysfs(buf)", ARG3, VKI_FSTYPSZ + 1);
      break;
   case VKI_GETNFSTYP:
      /* Libc: int sysfs(int opcode); */
      PRE_REG_READ1(long, SC2("sysfs", "getnfstyp"), int, opcode);
      break;
   default:
      VG_(unimplemented)("Syswrap of the sysfs call with opcode %ld.", SARG1);
      /*NOTREACHED*/
      break;
   }
}

POST(sys_sysfs)
{
   switch (ARG1 /*opcode*/) {
   case VKI_GETFSIND:
   case VKI_GETNFSTYP:
      break;
   case VKI_GETFSTYP:
      POST_MEM_WRITE(ARG3, VG_(strlen)((HChar *) ARG3) + 1);
      break;
   default:
      vg_assert(0);
      break;
   }
}


PRE(sys_getmsg)
{
   /* int getmsg(int fildes, struct strbuf *ctlptr, struct strbuf *dataptr,
                 int *flagsp); */
   struct vki_strbuf *ctrlptr = (struct vki_strbuf *)ARG2;
   struct vki_strbuf *dataptr = (struct vki_strbuf *)ARG3;
   *flags |= SfMayBlock;
   PRINT("sys_getmsg ( %ld, %#lx, %#lx, %#lx )", SARG1, ARG2, ARG3, ARG4);
   PRE_REG_READ4(long, "getmsg", int, fildes, struct vki_strbuf *, ctlptr,
                 struct vki_strbuf *, dataptr, int *, flagsp);
   if (ctrlptr) {
      PRE_FIELD_READ("getmsg(ctrlptr->maxlen)", ctrlptr->maxlen);
      PRE_FIELD_WRITE("getmsg(ctrlptr->len)", ctrlptr->len);
      PRE_FIELD_READ("getmsg(ctrlptr->buf)", ctrlptr->buf);
      if (ML_(safe_to_deref)((void*)ARG2, sizeof(struct vki_strbuf))
          && ctrlptr->maxlen > 0)
         PRE_MEM_WRITE("getmsg(ctrlptr->buf)", (Addr)ctrlptr->buf,
                       ctrlptr->maxlen);
   }
   if (dataptr) {
      PRE_FIELD_READ("getmsg(dataptr->maxlen)", dataptr->maxlen);
      PRE_FIELD_WRITE("getmsg(dataptr->len)", dataptr->len);
      PRE_FIELD_READ("getmsg(dataptr->buf)", dataptr->buf);
      if (ML_(safe_to_deref)((void*)ARG3, sizeof(struct vki_strbuf))
          && dataptr->maxlen > 0)
         PRE_MEM_WRITE("getmsg(dataptr->buf)", (Addr)dataptr->buf,
                       dataptr->maxlen);
   }
   PRE_MEM_READ("getmsg(flagsp)", ARG4, sizeof(int));
   /*PRE_MEM_WRITE("getmsg(flagsp)", ARG4, sizeof(int));*/

   /* Be strict. */
   if (!ML_(fd_allowed)(ARG1, "getmsg", tid, False))
      SET_STATUS_Failure(VKI_EBADF);
}

POST(sys_getmsg)
{
   struct vki_strbuf *ctrlptr = (struct vki_strbuf *)ARG2;
   struct vki_strbuf *dataptr = (struct vki_strbuf *)ARG3;

   if (ctrlptr && ctrlptr->len > 0)
      POST_MEM_WRITE((Addr)ctrlptr->buf, ctrlptr->len);
   if (dataptr && dataptr->len > 0)
      POST_MEM_WRITE((Addr)dataptr->buf, dataptr->len);
   POST_MEM_WRITE(ARG4, sizeof(int));
}

PRE(sys_putmsg)
{
   /* int putmsg(int fildes, struct strbuf *ctlptr, struct strbuf *dataptr,
                 int flags); */
   struct vki_strbuf *ctrlptr = (struct vki_strbuf *)ARG2;
   struct vki_strbuf *dataptr = (struct vki_strbuf *)ARG3;
   *flags |= SfMayBlock;
   PRINT("sys_putmsg ( %ld, %#lx, %#lx, %ld )", SARG1, ARG2, ARG3, SARG4);
   PRE_REG_READ4(long, "putmsg", int, fildes, struct vki_strbuf *, ctrlptr,
                 struct vki_strbuf *, dataptr, int, flags);
   if (ctrlptr) {
      PRE_FIELD_READ("putmsg(ctrlptr->len)", ctrlptr->len);
      PRE_FIELD_READ("putmsg(ctrlptr->buf)", ctrlptr->buf);
      if (ML_(safe_to_deref)((void*)ARG2, sizeof(struct vki_strbuf))
          && ctrlptr->len > 0)
         PRE_MEM_READ("putmsg(ctrlptr->buf)", (Addr)ctrlptr->buf,
                      ctrlptr->len);
   }
   if (dataptr) {
      PRE_FIELD_READ("putmsg(dataptr->len)", dataptr->len);
      PRE_FIELD_READ("putmsg(dataptr->buf)", dataptr->buf);
      if (ML_(safe_to_deref)((void*)ARG3, sizeof(struct vki_strbuf))
          && dataptr->len > 0)
         PRE_MEM_READ("putmsg(dataptr->buf)", (Addr)dataptr->buf,
                      dataptr->len);
   }

   /* Be strict. */
   if (!ML_(fd_allowed)(ARG1, "putmsg", tid, False))
      SET_STATUS_Failure(VKI_EBADF);
}

PRE(sys_lstat)
{
   /* int lstat(const char *path, struct stat *buf); */
   /* Note: We could use here the sys_newlstat generic wrapper, but the 'new'
      in its name is rather confusing in the Solaris context, thus we provide
      our own wrapper. */
   PRINT("sys_lstat ( %#lx(%s), %#lx )", ARG1, (HChar *) ARG1, ARG2);
   PRE_REG_READ2(long, "lstat", const char *, path, struct stat *, buf);

   PRE_MEM_RASCIIZ("lstat(path)", ARG1);
   PRE_MEM_WRITE("lstat(buf)", ARG2, sizeof(struct vki_stat));
}

POST(sys_lstat)
{
   POST_MEM_WRITE(ARG2, sizeof(struct vki_stat));
}

PRE(sys_sigprocmask)
{
   /* int sigprocmask(int how, const sigset_t *set, sigset_t *oset); */
   PRINT("sys_sigprocmask ( %ld, %#lx, %#lx )", SARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "sigprocmask",
                 int, how, vki_sigset_t *, set, vki_sigset_t *, oset);
   if (ARG2)
      PRE_MEM_READ("sigprocmask(set)", ARG2, sizeof(vki_sigset_t));
   if (ARG3)
      PRE_MEM_WRITE("sigprocmask(oset)", ARG3, sizeof(vki_sigset_t));

   /* Be safe. */
   if (ARG2 && !ML_(safe_to_deref((void*)ARG2, sizeof(vki_sigset_t)))) {
      SET_STATUS_Failure(VKI_EFAULT);
   }
   if (ARG3 && !ML_(safe_to_deref((void*)ARG3, sizeof(vki_sigset_t)))) {
      SET_STATUS_Failure(VKI_EFAULT);
   }

   if (!FAILURE)
      SET_STATUS_from_SysRes(
         VG_(do_sys_sigprocmask)(tid, ARG1 /*how*/, (vki_sigset_t*)ARG2,
                                 (vki_sigset_t*)ARG3)
      );

   if (SUCCESS)
      *flags |= SfPollAfter;
}

POST(sys_sigprocmask)
{
   if (ARG3)
      POST_MEM_WRITE(ARG3, sizeof(vki_sigset_t));
}

PRE(sys_sigsuspend)
{
   *flags |= SfMayBlock;

   /* int sigsuspend(const sigset_t *set); */
   PRINT("sys_sigsuspend ( %#lx )", ARG1);
   PRE_REG_READ1(long, "sigsuspend", vki_sigset_t *, set);
   PRE_MEM_READ("sigsuspend(set)", ARG1, sizeof(vki_sigset_t));

   /* Be safe. */
   if (ARG1 && ML_(safe_to_deref((void *) ARG1, sizeof(vki_sigset_t)))) {
      VG_(sigdelset)((vki_sigset_t *) ARG1, VG_SIGVGKILL); 
      /* We cannot mask VG_SIGVGKILL, as otherwise this thread would not
         be killable by VG_(nuke_all_threads_except).
         We thus silently ignore the user request to mask this signal.
         Note that this is similar to what is done for e.g.
         sigprocmask (see m_signals.c calculate_SKSS_from_SCSS).  */
   }
}

PRE(sys_sigaction)
{
   /* int sigaction(int signal, const struct sigaction *act,
                    struct sigaction *oact); */
   PRINT("sys_sigaction ( %ld, %#lx, %#lx )", SARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "sigaction", int, signal,
                 const struct sigaction *, act, struct sigaction *, oact);

   /* Note that on Solaris, vki_sigaction_toK_t and vki_sigaction_fromK_t are
      both typedefs of 'struct sigaction'. */

   if (ARG2) {
      vki_sigaction_toK_t *sa = (vki_sigaction_toK_t*)ARG2;
      PRE_FIELD_READ("sigaction(act->sa_flags)", sa->sa_flags);
      PRE_FIELD_READ("sigaction(act->sa_handler)", sa->ksa_handler);
      PRE_FIELD_READ("sigaction(act->sa_mask)", sa->sa_mask);
   }
   if (ARG3)
      PRE_MEM_WRITE("sigaction(oact)", ARG3, sizeof(vki_sigaction_fromK_t));

   /* Be safe. */
   if (ARG2 && !ML_(safe_to_deref((void*)ARG2,
                                  sizeof(vki_sigaction_toK_t)))) {
      SET_STATUS_Failure(VKI_EFAULT);
   }
   if (ARG3 && !ML_(safe_to_deref((void*)ARG3,
                                   sizeof(vki_sigaction_fromK_t)))) {
      SET_STATUS_Failure(VKI_EFAULT);
   }

   if (!FAILURE)
      SET_STATUS_from_SysRes(
         VG_(do_sys_sigaction)(ARG1, (const vki_sigaction_toK_t*)ARG2,
                              (vki_sigaction_fromK_t*)ARG3));
}

POST(sys_sigaction)
{
   if (ARG3)
      POST_MEM_WRITE(ARG3, sizeof(vki_sigaction_fromK_t));
}

PRE(sys_sigpending)
{
   /* int sigpending(int flag, sigset_t *setp); */
   PRINT("sys_sigpending ( %ld, %#lx )", SARG1, ARG2);
   PRE_REG_READ2(long, "sigpending", int, flag, sigset_t *, setp);
   PRE_MEM_WRITE("sigpending(setp)", ARG2, sizeof(vki_sigset_t));
}

POST(sys_sigpending)
{
   POST_MEM_WRITE(ARG2, sizeof(vki_sigset_t));
}

PRE(sys_getsetcontext)
{
   /* Kernel: int getsetcontext(int flag, void *arg) */
   ThreadState *tst = VG_(get_ThreadState)(tid);
   PRINT("sys_getsetcontext ( %ld, %#lx )", SARG1, ARG2);
   switch (ARG1 /*flag*/) {
   case VKI_GETCONTEXT:
      /* Libc: int getcontext(ucontext_t *ucp); */
      PRE_REG_READ2(long, SC2("getsetcontext", "getcontext"), int, flag,
                    ucontext_t *, ucp);
      PRE_MEM_WRITE("getsetcontext(ucp)", ARG2, sizeof(vki_ucontext_t));

      if (!ML_(safe_to_deref((void*)ARG2, sizeof(vki_ucontext_t)))) {
         SET_STATUS_Failure(VKI_EFAULT);
         return;
      }
      VG_(save_context)(tid, (vki_ucontext_t*)ARG2, Vg_CoreSysCall);
      SET_STATUS_Success(0);
      break;
   case VKI_SETCONTEXT:
      /* Libc: int setcontext(const ucontext_t *ucp); */
      PRE_REG_READ2(long, SC2("getsetcontext", "setcontext"), int, flag,
                    const ucontext_t *, ucp);

      if (!ARG2) {
         /* Setting NULL context causes thread exit. */
         tst->exitreason = VgSrc_ExitThread;
         tst->os_state.exitcode = 0;
         SET_STATUS_Success(0);
         return;
      }

      if (!ML_(safe_to_deref((void*)ARG2, sizeof(vki_ucontext_t)))) {
         SET_STATUS_Failure(VKI_EFAULT);
         return;
      }

      VG_(restore_context)(tid, (vki_ucontext_t*)ARG2,
                           Vg_CoreSysCall, False/*esp_is_thrptr*/);
      /* Tell the driver not to update the guest state with the "result". */
      *flags |= SfNoWriteResult;
      /* Check to see if any signals arose as a result of this. */
      *flags |= SfPollAfter;

      /* Check if this is a possible return from a signal handler. */
      VG_(sigframe_return)(tid, (vki_ucontext_t*)ARG2);

      SET_STATUS_Success(0);
      break;
   case VKI_GETUSTACK:
      /* Libc: int getustack(stack_t **spp); */
      PRE_REG_READ2(long, SC2("getsetcontext", "getustack"), int, flag,
                    stack_t **, spp);
      PRE_MEM_WRITE("getsetcontext(spp)", ARG2, sizeof(vki_stack_t*));

      if (!ML_(safe_to_deref((void*)ARG2, sizeof(vki_stack_t*)))) {
         SET_STATUS_Failure(VKI_EFAULT);
         return;
      }

      *(vki_stack_t**)ARG2 = tst->os_state.ustack;
      POST_MEM_WRITE(ARG2, sizeof(vki_stack_t*));
      SET_STATUS_Success(0);
      break;
   case VKI_SETUSTACK:
      {
         /* Libc: int setustack(stack_t *sp); */
         PRE_REG_READ2(long, SC2("getsetcontext", "setustack"), int, flag,
                       stack_t *, sp);

         /* The kernel does not read the stack data instantly but it can read
            them later so it is better to make sure the data are defined. */
         PRE_MEM_READ("getsetcontext_setustack(sp)", ARG2, sizeof(vki_stack_t));

         if (!ML_(safe_to_deref((void*)ARG2, sizeof(vki_stack_t)))) {
            SET_STATUS_Failure(VKI_EFAULT);
            return;
         }

         vki_stack_t *old_stack = tst->os_state.ustack;
         tst->os_state.ustack = (vki_stack_t*)ARG2;

         /* The thread is setting the ustack pointer.  It is a good time to get
            information about its stack. */
         if (tst->os_state.ustack->ss_flags == 0) {
            /* If the sanity check of ss_flags passed set the stack. */
            set_stack(tid, tst->os_state.ustack);

            if ((old_stack == NULL) && (tid > 1)) {
               /* New thread creation is now completed. Inform the tool. */
               VG_TRACK(pre_thread_first_insn, tid);
            }
         }

         SET_STATUS_Success(0);
      }
      break;
   default:
      VG_(unimplemented)("Syswrap of the context call with flag %ld.", SARG1);
      /*NOTREACHED*/
      break;
   }
}

PRE(sys_fchmodat)
{
   /* int fchmodat(int fd, const char *path, mode_t mode, int flag); */

   /* Interpret the first argument as 32-bit value even on 64-bit architecture.
      This is different from Linux, for example, where glibc sign-extends it. */
   Int fd = (Int) ARG1;

   PRINT("sys_fchmodat ( %d, %#lx(%s), %ld, %ld )",
         fd, ARG2, (HChar *) ARG2, SARG3, SARG4);
   PRE_REG_READ4(long, "fchmodat",
                 int, fd, const char *, path, vki_mode_t, mode, int, flag);

   if (ARG2)
      PRE_MEM_RASCIIZ("fchmodat(path)", ARG2);

   /* Be strict but ignore fd for absolute path. */
   if (fd != VKI_AT_FDCWD
       && ML_(safe_to_deref)((void *) ARG2, 1)
       && ((HChar *) ARG2)[0] != '/'
       && !ML_(fd_allowed)(fd, "fchmodat", tid, False))
      SET_STATUS_Failure(VKI_EBADF);
}

PRE(sys_mkdirat)
{
   /* int mkdirat(int fd, const char *path, mode_t mode); */

   /* Interpret the first argument as 32-bit value even on 64-bit architecture.
      This is different from Linux, for example, where glibc sign-extends it. */
   Int fd = (Int) ARG1;

   *flags |= SfMayBlock;
   PRINT("sys_mkdirat ( %d, %#lx(%s), %ld )", fd, ARG2, (HChar *) ARG2, SARG3);
   PRE_REG_READ3(long, "mkdirat", int, fd, const char *, path,
                 vki_mode_t, mode);
   PRE_MEM_RASCIIZ("mkdirat(path)", ARG2);

   /* Be strict but ignore fd for absolute path. */
   if (fd != VKI_AT_FDCWD
       && ML_(safe_to_deref)((void *) ARG2, 1)
       && ((HChar *) ARG2)[0] != '/'
       && !ML_(fd_allowed)(fd, "mkdirat", tid, False))
      SET_STATUS_Failure(VKI_EBADF);
}

static void do_statvfs_post(struct vki_statvfs *stats, ThreadId tid)
{
   POST_FIELD_WRITE(stats->f_bsize);
   POST_FIELD_WRITE(stats->f_frsize);
   POST_FIELD_WRITE(stats->f_blocks);
   POST_FIELD_WRITE(stats->f_bfree);
   POST_FIELD_WRITE(stats->f_bavail);
   POST_FIELD_WRITE(stats->f_files);
   POST_FIELD_WRITE(stats->f_ffree);
   POST_FIELD_WRITE(stats->f_favail);
   POST_FIELD_WRITE(stats->f_fsid);
   POST_MEM_WRITE((Addr) stats->f_basetype, VG_(strlen)(stats->f_basetype) + 1);
   POST_FIELD_WRITE(stats->f_flag);
   POST_FIELD_WRITE(stats->f_namemax);
   POST_MEM_WRITE((Addr) stats->f_fstr, VG_(strlen)(stats->f_fstr) + 1);
}

PRE(sys_statvfs)
{
   /* int statvfs(const char *path, struct statvfs *buf); */
   *flags |= SfMayBlock;
   PRINT("sys_statvfs ( %#lx(%s), %#lx )", ARG1, (HChar *) ARG1, ARG2);
   PRE_REG_READ2(long, "statvfs", const char *, path,
                 struct vki_statvfs *, buf);
   PRE_MEM_RASCIIZ("statvfs(path)", ARG1);
   PRE_MEM_WRITE("statvfs(buf)", ARG2, sizeof(struct vki_statvfs));
}

POST(sys_statvfs)
{
   do_statvfs_post((struct vki_statvfs *) ARG2, tid);
}

PRE(sys_fstatvfs)
{
   /* int fstatvfs(int fd, struct statvfs *buf); */
   *flags |= SfMayBlock;
   PRINT("sys_fstatvfs ( %ld, %#lx )", SARG1, ARG2);
   PRE_REG_READ2(long, "fstatvfs", int, fd, struct vki_statvfs *, buf);
   PRE_MEM_WRITE("fstatvfs(buf)", ARG2, sizeof(struct vki_statvfs));

   /* Be strict. */
   if (!ML_(fd_allowed)(ARG1, "fstatvfs", tid, False))
      SET_STATUS_Failure(VKI_EBADF);
}

POST(sys_fstatvfs)
{
   do_statvfs_post((struct vki_statvfs *) ARG2, tid);
}

PRE(sys_nfssys)
{
   /* int nfssys(enum nfssys_op opcode, void *arg); */
   *flags |= SfMayBlock;
   PRINT("sys_nfssys ( %ld, %#lx )", SARG1, ARG2);

   switch (ARG1 /*opcode*/) {
   case VKI_NFS_REVAUTH:
      PRE_REG_READ2(long, SC2("nfssys", "nfs_revauth"), int, opcode,
                    struct vki_nfs_revauth_args *, args);
      PRE_MEM_READ("nfssys(arg)", ARG2,
                   sizeof(struct vki_nfs_revauth_args));
      break;
   default:
      VG_(unimplemented)("Syswrap of the nfssys call with opcode %ld.", SARG1);
      /*NOTREACHED*/
      break;
   }
}

POST(sys_nfssys)
{
   switch (ARG1 /*opcode*/) {
   case VKI_NFS_REVAUTH:
      break;
   default:
      vg_assert(0);
      break;
   }
}

PRE(sys_waitid)
{
   /* int waitid(idtype_t idtype, id_t id, siginfo_t *infop, int options); */
   *flags |= SfMayBlock;
   PRINT("sys_waitid( %ld, %ld, %#lx, %ld )", SARG1, SARG2, ARG3, SARG4);
   PRE_REG_READ4(long, "waitid", vki_idtype_t, idtype, vki_id_t, id,
                 siginfo_t *, infop, int, options);
   PRE_MEM_WRITE("waitid(infop)", ARG3, sizeof(vki_siginfo_t));
}

POST(sys_waitid)
{
   POST_MEM_WRITE(ARG3, sizeof(vki_siginfo_t));
}

PRE(sys_sigsendsys)
{
   /* int sigsendsys(procset_t *psp, int sig); */
   PRINT("sys_sigsendsys( %#lx, %ld )", ARG1, SARG2);
   PRE_REG_READ2(long, "sigsendsys", vki_procset_t *, psp, int, signal);
   PRE_MEM_READ("sigsendsys(psp)", ARG1, sizeof(vki_procset_t));

   if (!ML_(client_signal_OK)(ARG1)) {
      SET_STATUS_Failure(VKI_EINVAL);
   }
   if (!ML_(safe_to_deref)((void *) ARG1, sizeof(vki_procset_t))) {
      SET_STATUS_Failure(VKI_EFAULT);
   }
   
   /* Exit early if there are problems. */
   if (FAILURE)
      return;

   vki_procset_t *psp = (vki_procset_t *) ARG1;
   switch (psp->p_op) {
      case VKI_POP_AND:
         break;
      default:
         VG_(unimplemented)("Syswrap of the sigsendsys call with op %u.",
                            psp->p_op);
   }

   UInt pid;
   if ((psp->p_lidtype == VKI_P_PID) && (psp->p_ridtype == VKI_P_ALL)) {
      pid = psp->p_lid;
   } else if ((psp->p_lidtype == VKI_P_ALL) && (psp->p_ridtype == VKI_P_PID)) {
      pid = psp->p_rid;
   } else {
      VG_(unimplemented)("Syswrap of the sigsendsys call with lidtype %u and"
                         "ridtype %u.", psp->p_lidtype, psp->p_ridtype);
   }

   if (VG_(clo_trace_signals))
      VG_(message)(Vg_DebugMsg, "sigsendsys: sending signal to process %u\n",
                   pid);

   /* Handle SIGKILL specially. */
   if (ARG2 == VKI_SIGKILL && ML_(do_sigkill)(pid, -1)) {
      SET_STATUS_Success(0);
      return;
   }

   /* Check to see if this gave us a pending signal. */
   *flags |= SfPollAfter;
}

#if defined(SOLARIS_UTIMESYS_SYSCALL)
PRE(sys_utimesys)
{
   /* Kernel: int utimesys(int code, uintptr_t arg1, uintptr_t arg2,
                           uintptr_t arg3, uintptr_t arg4);
    */

   switch (ARG1 /*code*/) {
   case 0:
      /* Libc: int futimens(int fd, const timespec_t times[2]); */
      PRINT("sys_utimesys ( %ld, %ld, %#lx )", SARG1, SARG2, ARG3);
      PRE_REG_READ3(long, "utimesys", int, code, int, fd,
                    const vki_timespec_t *, times);
      if (ARG3)
         PRE_MEM_READ("utimesys(times)", ARG3, 2 * sizeof(vki_timespec_t));

      /* Be strict. */
      if (!ML_(fd_allowed)(ARG2, "utimesys", tid, False))
         SET_STATUS_Failure(VKI_EBADF);
      break;
   case 1:
      {
         /* Libc: int utimensat(int fd, const char *path,
                                const timespec_t times[2], int flag);
          */

         /* Interpret the second argument as 32-bit value even on 64-bit
            architecture. This is different from Linux, for example, where glibc
            sign-extends it. */
         Int fd = (Int) ARG2;

         PRINT("sys_utimesys ( %ld, %d, %#lx(%s), %#lx, %ld )",
               SARG1, fd, ARG3, (HChar *) ARG3, ARG4, SARG5);
         PRE_REG_READ5(long, "utimesys", int, code, int, fd, const char *, path,
                       const vki_timespec_t *, times, int, flag);
         if (ARG3)
            PRE_MEM_RASCIIZ("utimesys(path)", ARG3);
         if (ARG4)
            PRE_MEM_READ("utimesys(times)", ARG4, 2 * sizeof(vki_timespec_t));

         /* Be strict but ignore fd for absolute path. */
         if (fd != VKI_AT_FDCWD
             && ML_(safe_to_deref)((void *) ARG3, 1)
             && ((HChar *) ARG3)[0] != '/'
             && !ML_(fd_allowed)(fd, "utimesys", tid, False))
            SET_STATUS_Failure(VKI_EBADF);
         break;
      }
   default:
      VG_(unimplemented)("Syswrap of the utimesys call with code %ld.", SARG1);
      /*NOTREACHED*/
      break;
   }
}
#endif /* SOLARIS_UTIMESYS_SYSCALL */

#if defined(SOLARIS_UTIMENSAT_SYSCALL)
PRE(sys_utimensat)
{
   /* int utimensat(int fd, const char *path, const timespec_t times[2],
                    int flag);
    */

   /* Interpret the first argument as 32-bit value even on 64-bit architecture.
      This is different from Linux, for example, where glibc sign-extends it. */
   Int fd = (Int) ARG1;

   PRINT("sys_utimensat ( %d, %#lx(%s), %#lx, %ld )",
         fd, ARG2, (HChar *) ARG2, ARG3, SARG4);
   PRE_REG_READ4(long, "utimensat", int, fd, const char *, path,
                 const vki_timespec_t *, times, int, flag);
   if (ARG2)
      PRE_MEM_RASCIIZ("utimensat(path)", ARG2);
   if (ARG3)
      PRE_MEM_READ("utimensat(times)", ARG3, 2 * sizeof(vki_timespec_t));

   /* Be strict but ignore fd for absolute path. */
   if (fd != VKI_AT_FDCWD
       && ML_(safe_to_deref)((void *) ARG2, 1)
       && ((HChar *) ARG2)[0] != '/'
       && !ML_(fd_allowed)(fd, "utimensat", tid, False))
      SET_STATUS_Failure(VKI_EBADF);
}
#endif /* SOLARIS_UTIMENSAT_SYSCALL */

PRE(sys_sigresend)
{
   /* int sigresend(int signal, siginfo_t *siginfo, sigset_t *mask); */
   /* Sends a signal to the calling thread, the mask parameter specifies a new
      signal mask. */

   /* Static (const) mask accessible from outside of this function. */
   static vki_sigset_t block_all;

   PRINT("sys_sigresend( %ld, %#lx, %#lx )", SARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "sigresend", int, signal, vki_siginfo_t *, siginfo,
                 vki_sigset_t *, mask);

   if (ARG2)
      PRE_MEM_READ("sigresend(siginfo)", ARG2, sizeof(vki_siginfo_t));
   PRE_MEM_WRITE("sigresend(mask)", ARG3, sizeof(vki_sigset_t));

   /* Check the signal and mask. */
   if (!ML_(client_signal_OK)(ARG1)) {
      SET_STATUS_Failure(VKI_EINVAL);
   }
   if (!ML_(safe_to_deref)((void*)ARG3, sizeof(vki_sigset_t))) {
      SET_STATUS_Failure(VKI_EFAULT);
   }
   
   /* Exit early if there are problems. */
   if (FAILURE)
      return;

   /* Save the requested mask to unused ARG4. */
   ARG4 = ARG3;

   /* Fake the requested sigmask with a block-all mask.  If the syscall
      succeeds then we will block "all" signals for a few instructions (in
      syscall-x86-solaris.S) but the correct mask will be almost instantly set
      again by a call to sigprocmask (also in syscall-x86-solaris.S).  If the
      syscall fails then the mask is not changed, so everything is ok too. */
   VG_(sigfillset)(&block_all);
   ARG3 = (UWord)&block_all;

   /* Check to see if this gave us a pending signal. */
   *flags |= SfPollAfter;

   if (VG_(clo_trace_signals))
      VG_(message)(Vg_DebugMsg, "sigresend: resending signal %lu\n", ARG1);

   /* Handle SIGKILL specially. */
   if (ARG1 == VKI_SIGKILL && ML_(do_sigkill)(tid, -1)) {
      SET_STATUS_Success(0);
      return;
   }

   /* Ask to handle this syscall via the slow route, since that's the only one
      that sets tst->status to VgTs_WaitSys.  If the result of doing the
      syscall is an immediate run of async_signalhandler() in m_signals.c,
      then we need the thread to be properly tidied away. */
   *flags |= SfMayBlock;
}

POST(sys_sigresend)
{
   /* The syscall succeeded, set the requested mask. */
   VG_(do_sys_sigprocmask)(tid, VKI_SIG_SETMASK, (vki_sigset_t*)ARG4, NULL);

   if (VG_(clo_trace_signals))
      VG_(message)(Vg_DebugMsg, "sigresend: resent signal %lu\n", ARG1);
}

static void mem_priocntlsys_parm_ok(ThreadId tid, Bool pre, Bool reade,
                                    vki_pc_vaparm_t *parm)
{
   if (reade)
      return;

   if (pre)
      PRE_FIELD_WRITE("priocntlsys(parm)", parm->pc_parm);
   else
      POST_FIELD_WRITE(parm->pc_parm);
}

static void mem_priocntlsys_parm(ThreadId tid, Bool pre, Bool reade,
                                 const HChar *clname,
                                 vki_pc_vaparm_t *parm)
{
   /* This function is used to handle the PC_SETXPARMS and PC_GETXPARMS
      parameters.  In the case of PC_SETXPARMS, the code below merely checks
      if all parameters are scalar, PRE_MEM_READ() for these parameters is
      already done by the PC_SETXPARMS handler in PRE(sys_priocntlsys).

      A caller of this function is responsible for checking that clname and
      &parm->key can be dereferenced. */

   if (VG_STREQ(clname, "RT")) {
      switch (parm->pc_key) {
      case VKI_RT_KY_PRI:
      case VKI_RT_KY_TQSECS:
      case VKI_RT_KY_TQNSECS:
      case VKI_RT_KY_TQSIG:
         /* Scalar values that are stored directly in pc_parm. */
         mem_priocntlsys_parm_ok(tid, pre, reade, parm);
         return;
      }
   }
   else if (VG_STREQ(clname, "TS")) {
      switch (parm->pc_key) {
      case VKI_TS_KY_UPRILIM:
      case VKI_TS_KY_UPRI:
         /* Scalar values that are stored directly in pc_parm. */
         mem_priocntlsys_parm_ok(tid, pre, reade, parm);
         return;
      }
   }
   else if (VG_STREQ(clname, "IA")) {
      switch (parm->pc_key) {
      case VKI_IA_KY_UPRILIM:
      case VKI_IA_KY_UPRI:
      case VKI_IA_KY_MODE:
         /* Scalar values that are stored directly in pc_parm. */
         mem_priocntlsys_parm_ok(tid, pre, reade, parm);
         return;
      }
   }
   else if (VG_STREQ(clname, "FSS")) {
      switch (parm->pc_key) {
      case VKI_FSS_KY_UPRILIM:
      case VKI_FSS_KY_UPRI:
         /* Scalar values that are stored directly in pc_parm. */
         mem_priocntlsys_parm_ok(tid, pre, reade, parm);
         return;
      }
   }
   else if (VG_STREQ(clname, "FX")) {
      switch (parm->pc_key) {
      case VKI_FX_KY_UPRILIM:
      case VKI_FX_KY_UPRI:
      case VKI_FX_KY_TQSECS:
      case VKI_FX_KY_TQNSECS:
         /* Scalar values that are stored directly in pc_parm. */
         mem_priocntlsys_parm_ok(tid, pre, reade, parm);
         return;
      }
   }
   else {
      /* Unknown class. */
      VG_(unimplemented)("Syswrap of the priocntlsys call where clname=%s.",
                         clname);
      /*NOTREACHED*/
   }

   /* The class is known but pc_key is unknown. */
   VG_(unimplemented)("Syswrap of the priocntlsys call where clname=%s "
                      "and pc_key=%d.", clname, parm->pc_key);
   /*NOTREACHED*/
}

PRE(sys_priocntlsys)
{
   /* long priocntlsys(int pc_version, procset_t *psp, int cmd, caddr_t arg,
                       caddr_t arg2); */

   if (ARG1 != 1) {
      /* Only the first version of priocntlsys is supported by the code below.
       */
      VG_(unimplemented)("Syswrap of the priocntlsys where pc_version=%lu.",
                         ARG1);
      /*NOTREACHED*/
   }

   PRINT("sys_priocntlsys ( %ld, %#lx, %ld, %#lx, %#lx )", SARG1, ARG2, SARG3,
         ARG4, ARG5);
   PRE_REG_READ5(long, "priocntlsys", int, pc_version, procset_t *, psp,
                 int, cmd, void *, arg, void *, arg2);

   switch (ARG3 /*cmd*/) {
   case VKI_PC_GETCID:
      if (ARG4) {
         vki_pcinfo_t *info = (vki_pcinfo_t*)ARG4;
         PRE_MEM_RASCIIZ("priocntlsys(clname)", (Addr)info->pc_clname);
         /* The next line says that the complete pcinfo_t structure can be
            written, but this actually isn't true for pc_clname which is
            always only read. */
         PRE_MEM_WRITE("priocntlsys(pcinfo)", ARG4, sizeof(vki_pcinfo_t));
      }
      break;
   case VKI_PC_GETCLINFO:
      if (ARG4) {
         vki_pcinfo_t *info = (vki_pcinfo_t*)ARG4;
         PRE_FIELD_READ("priocntlsys(cid)", info->pc_cid);
         /* The next line says that the complete pcinfo_t structure can be
            written, but this actually isn't true for pc_cid which is
            always only read. */
         PRE_MEM_WRITE("priocntlsys(pcinfo)", ARG4, sizeof(vki_pcinfo_t));
      }
      break;
   case VKI_PC_SETPARMS:
      PRE_MEM_READ("priocntlsys(psp)", ARG2, sizeof(vki_procset_t));
      /* The next line says that the complete pcparms_t structure is read
         which is never actually true (we are too pessimistic here).
         Unfortunately we can't do better because we don't know what
         process class is involved. */
      PRE_MEM_READ("priocntlsys(parms)", ARG4, sizeof(vki_pcparms_t));
      break;
   case VKI_PC_GETPARMS:
      PRE_MEM_READ("priocntlsys(psp)", ARG2, sizeof(vki_procset_t));
      PRE_MEM_WRITE("priocntlsys(parms)", ARG4, sizeof(vki_pcparms_t));
      break;
   case VKI_PC_GETPRIRANGE:
      {
         vki_pcpri_t *pcpri = (vki_pcpri_t*)ARG4;
         PRE_FIELD_READ("priocntlsys(cid)", pcpri->pc_cid);
      }
      PRE_MEM_WRITE("priocntlsys(pri)", ARG4, sizeof(vki_pcpri_t));
      break;
   case VKI_PC_DONICE:
      PRE_MEM_READ("priocntlsys(psp)", ARG2, sizeof(vki_procset_t));
      {
         vki_pcnice_t *nicee = (vki_pcnice_t*)ARG4;
         PRE_FIELD_READ("priocntlsys(op)", nicee->pc_op);
         if (ML_(safe_to_deref)(&nicee->pc_op, sizeof(nicee->pc_op))) {
            switch (nicee->pc_op) {
            case VKI_PC_GETNICE:
               PRE_FIELD_WRITE("priocntlsys(val)", nicee->pc_val);
               break;
            case VKI_PC_SETNICE:
               PRE_FIELD_READ("priocntlsys(val)", nicee->pc_val);
               break;
            default:
               VG_(unimplemented)("Syswrap of the priocntlsys call where "
                                  "cmd=PC_DONICE and pc_op=%d", nicee->pc_op);
               /*NOTREACHED*/
               break;
            }
         }
      }
      break;
   case VKI_PC_SETXPARMS:
      PRE_MEM_READ("priocntlsys(psp)", ARG2, sizeof(vki_procset_t));
      PRE_MEM_RASCIIZ("priocntlsys(clname)", ARG4);
      if (ARG5) {
         vki_pc_vaparms_t *parms = (vki_pc_vaparms_t*)ARG5;
         PRE_FIELD_READ("priocntlsys(vaparmscnt)", parms->pc_vaparmscnt);
         if (ML_(safe_to_deref)(&parms->pc_vaparmscnt,
                                sizeof(parms->pc_vaparmscnt))) {
            vki_uint_t i;
            PRE_MEM_READ("priocntlsys(parms)", (Addr)parms->pc_parms,
                         parms->pc_vaparmscnt * sizeof(parms->pc_parms[0]));
            for (i = 0; i < parms->pc_vaparmscnt; i++) {
               vki_pc_vaparm_t *parm = &parms->pc_parms[i];
               if (ML_(safe_to_deref)(parm, sizeof(*parm)) &&
                   ML_(safe_to_deref)((void*)ARG4, 1))
                  mem_priocntlsys_parm(tid, True /*pre*/, True /*read*/,
                                       (HChar*)ARG4, parm);
            }
         }
      }
      break;
   case VKI_PC_GETXPARMS:
      PRE_MEM_READ("priocntlsys(psp)", ARG2, sizeof(vki_procset_t));
      if (ARG4)
         PRE_MEM_RASCIIZ("priocntlsys(clname)", ARG4);
      if (ARG5) {
         vki_pc_vaparms_t *parms = (vki_pc_vaparms_t*)ARG5;
         PRE_FIELD_READ("priocntlsys(vaparmscnt)", parms->pc_vaparmscnt);
         if (ML_(safe_to_deref)(&parms->pc_vaparmscnt,
                                sizeof(parms->pc_vaparmscnt))) {
            vki_uint_t i;
            for (i = 0; i < parms->pc_vaparmscnt; i++) {
               vki_pc_vaparm_t *parm = &parms->pc_parms[i];
               PRE_MEM_READ("priocntlsys(parms)", (Addr)&parm->pc_key,
                            parms->pc_vaparmscnt * sizeof(parm->pc_key));
               if (ML_(safe_to_deref)(&parm->pc_key,
                                      sizeof(parm->pc_key))) {
                  /* First handle PC_KY_CLNAME, then class specific keys.
                     Note that PC_KY_CLNAME can be used only with
                     ARG4==NULL && parms->pc_vaparmscnt==1.  We are not so
                     strict here and handle this special case as a regular
                     one which makes the code simpler. */
                  if (parm->pc_key == VKI_PC_KY_CLNAME)
                     PRE_MEM_WRITE("priocntlsys(clname)", parm->pc_parm,
                                   VKI_PC_CLNMSZ);
                  else if (ARG4 && ML_(safe_to_deref)((void*)ARG4, 1))
                     mem_priocntlsys_parm(tid, True /*pre*/,
                                          False /*read*/, (HChar*)ARG4,
                                          parm);
               }
            }
         }
      }
      break;
   case VKI_PC_SETDFLCL:
      PRE_MEM_RASCIIZ("priocntlsys(clname)", ARG4);
      break;
   case VKI_PC_GETDFLCL:
      if (ARG4) {
         /* GETDFLCL writes to the ARG4 buffer only if ARG4 isn't NULL.  Also
            note that if ARG4 is NULL then the syscall succeeds. */
         PRE_MEM_WRITE("priocntlsys(clname)", ARG4, VKI_PC_CLNMSZ);
      }
      break;
   case VKI_PC_DOPRIO:
      PRE_MEM_READ("priocntlsys(psp)", ARG2, sizeof(vki_procset_t));
      {
         vki_pcprio_t *prio = (vki_pcprio_t*)ARG4;
         PRE_FIELD_READ("priocntlsys(op)", prio->pc_op);
         if (ML_(safe_to_deref)(&prio->pc_op, sizeof(prio->pc_op))) {
            switch (prio->pc_op) {
            case VKI_PC_GETPRIO:
               PRE_FIELD_WRITE("priocntlsys(cid)", prio->pc_cid);
               PRE_FIELD_WRITE("priocntlsys(val)", prio->pc_val);
               break;
            case VKI_PC_SETPRIO:
               PRE_FIELD_READ("priocntlsys(cid)", prio->pc_cid);
               PRE_FIELD_READ("priocntlsys(val)", prio->pc_val);
               break;
            default:
               VG_(unimplemented)("Syswrap of the priocntlsys call where "
                                  "cmd=PC_DOPRIO and pc_op=%d", prio->pc_op);
               /*NOTREACHED*/
               break;
            }
         }
      }
      break;
   case VKI_PC_ADMIN:
   default:
      VG_(unimplemented)("Syswrap of the priocntlsys call with cmd %ld.", SARG3);
      /*NOTREACHED*/
      break;
   }
}

static void post_mem_write_priocntlsys_clinfo(ThreadId tid,
                                              const HChar *clname, Addr clinfo)
{
   if (VG_STREQ(clname, "RT"))
      POST_MEM_WRITE(clinfo, sizeof(vki_rtinfo_t));
   else if (VG_STREQ(clname, "TS"))
      POST_MEM_WRITE(clinfo, sizeof(vki_tsinfo_t));
   else if (VG_STREQ(clname, "IA"))
      POST_MEM_WRITE(clinfo, sizeof(vki_iainfo_t));
   else if (VG_STREQ(clname, "FSS"))
      POST_MEM_WRITE(clinfo, sizeof(vki_fssinfo_t));
   else if (VG_STREQ(clname, "FX"))
      POST_MEM_WRITE(clinfo, sizeof(vki_fxinfo_t));
   else if (VG_STREQ(clname, "SDC")) {
      /* Relax. */
   }
   else {
      VG_(unimplemented)("Syswrap of the priocntlsys call where clname=%s.",
                         clname);
      /*NOTREACHED*/
   }
}

POST(sys_priocntlsys)
{
   switch (ARG3 /*cmd*/) {
   case VKI_PC_GETCID:
      if (ARG4) {
         vki_pcinfo_t *info = (vki_pcinfo_t*)ARG4;
         POST_FIELD_WRITE(info->pc_cid);
         post_mem_write_priocntlsys_clinfo(tid, info->pc_clname,
                                           (Addr)&info->pc_clinfo);
      }
      break;
   case VKI_PC_GETCLINFO:
      if (ARG4) {
         vki_pcinfo_t *info = (vki_pcinfo_t*)ARG4;
         POST_MEM_WRITE((Addr)info->pc_clname,
                        VG_(strlen)((HChar*)info->pc_clname) + 1);
         post_mem_write_priocntlsys_clinfo(tid, info->pc_clname,
                                           (Addr)&info->pc_clinfo);
      }
      break;
   case VKI_PC_SETPARMS:
      /* Relax. */
      break;
   case VKI_PC_GETPARMS:
      /* The next line says that the complete pcparms_t structure is
         written which is never actually true (we are too optimistic here).
         Unfortunately we can't do better because we don't know what
         process class is involved. */
      POST_MEM_WRITE(ARG4, sizeof(vki_pcparms_t));
      break;
   case VKI_PC_GETPRIRANGE:
      POST_MEM_WRITE(ARG4, sizeof(vki_pcpri_t));
      break;
   case VKI_PC_DONICE:
      {
         vki_pcnice_t *nicee = (vki_pcnice_t*)ARG4;
         if (nicee->pc_op == VKI_PC_GETNICE)
            POST_FIELD_WRITE(nicee->pc_val);
      }
      break;
   case VKI_PC_SETXPARMS:
      /* Relax. */
      break;
   case VKI_PC_GETXPARMS:
      {
         vki_pc_vaparms_t *parms = (vki_pc_vaparms_t*)ARG5;
         vki_uint_t i;
         for (i = 0; i < parms->pc_vaparmscnt; i++) {
            vki_pc_vaparm_t *parm = &parms->pc_parms[i];
            if (parm->pc_key == VKI_PC_KY_CLNAME)
               POST_MEM_WRITE(parm->pc_parm,
                              VG_(strlen)((HChar*)(Addr)parm->pc_parm) + 1);
            else if (ARG4)
               mem_priocntlsys_parm(tid, False /*pre*/, False /*read*/,
                                    (HChar*)ARG4, parm);
         }
      }
      break;
   case VKI_PC_SETDFLCL:
      /* Relax. */
      break;
   case VKI_PC_GETDFLCL:
      if (ARG4)
         POST_MEM_WRITE(ARG4, VG_(strlen)((HChar*)ARG4) + 1);
      break;
   case VKI_PC_DOPRIO:
      {
         vki_pcprio_t *prio = (vki_pcprio_t*)ARG4;
         if (prio->pc_op == VKI_PC_GETPRIO) {
            POST_FIELD_WRITE(prio->pc_cid);
            POST_FIELD_WRITE(prio->pc_val);
         }
      }
      break;
   case VKI_PC_ADMIN:
   default:
      vg_assert(0);
      break;
   }
}

PRE(sys_pathconf)
{
   /* long pathconf(const char *path, int name); */
   PRINT("sys_pathconf ( %#lx(%s), %ld )", ARG1, (HChar *) ARG1, SARG2);
   PRE_REG_READ2(long, "pathconf", const char *, path, int, name);
   PRE_MEM_RASCIIZ("pathconf(path)", ARG1);
}

PRE(sys_mmap)
{
   /* void *mmap(void *addr, size_t len, int prot, int flags,
                 int fildes, off_t off); */
   SysRes r;
   OffT offset;

   /* Stay sane. */
   vg_assert(VKI_PAGE_SIZE == 4096);
   vg_assert(sizeof(offset) == sizeof(ARG6));

   PRINT("sys_mmap ( %#lx, %#lx, %#lx, %#lx, %ld, %#lx )",
         ARG1, ARG2, ARG3, ARG4, SARG5, ARG6);
   PRE_REG_READ6(long, "mmap", void *, start, vki_size_t, length,
                 int, prot, int, flags, int, fd, vki_off_t, offset);

   /* Make sure that if off < 0 then it's passed correctly to the generic mmap
      wraper. */
   offset = *(OffT*)&ARG6;

   r = ML_(generic_PRE_sys_mmap)(tid, ARG1, ARG2, ARG3, ARG4, ARG5, offset);
   SET_STATUS_from_SysRes(r);
}

#if defined(SOLARIS_UUIDSYS_SYSCALL)
PRE(sys_uuidsys)
{
   /* int uuidsys(struct uuid *uuid); */
   PRINT("sys_uuidsys ( %#lx )", ARG1);
   PRE_REG_READ1(long, "uuidsys", struct vki_uuid *, uuid);
   PRE_MEM_WRITE("uuidsys(uuid)", ARG1, sizeof(struct vki_uuid));
}

POST(sys_uuidsys)
{
   POST_MEM_WRITE(ARG1, sizeof(struct vki_uuid));
}
#endif /* SOLARIS_UUIDSYS_SYSCALL */

/* Syscall mmapobj emulation. Processes ELF program headers
   and maps them into correct place in memory. Not an easy task, though.
   ELF program header of PT_LOAD/PT_SUNWBSS type specifies:
   o p_vaddr  - actually a memory offset
   o p_memsz  - total segment size, including text, data and BSS
   o p_filesz - file-based segment size mapping (includes only text and data);
                p_memsz - p_filesz is the size of BSS
   o p_offset - offset into the ELF file where the file-based mapping starts
 
   Several problematic areas to cover here:
   1. p_offset can contain a value which is not page-aligned. In that case
      we mmap a part of the file prior to p_offset to make the start address
      page-aligned.
   2. Partially unused page after the file-based mapping must be zeroed.
   3. The first mapping is flagged with MR_HDR_ELF and needs to contain
      the ELF header. This information is used and verified by the dynamic
      linker (ld.so.1). */
static SysRes mmapobj_process_phdrs(ThreadId tid, Int fd,
                                    vki_mmapobj_result_t *storage,
                                    vki_uint_t *elements,
                                    const VKI_ESZ(Ehdr) *ehdr,
                                    const VKI_ESZ(Phdr) *phdrs)
{
#define ADVANCE_PHDR(ehdr, phdr) \
   (const VKI_ESZ(Phdr) *) ((const HChar *) (phdr) + (ehdr)->e_phentsize)

   SysRes res;
   Int i;
   Int first_segment_idx = -1;
   UInt idx;
   UInt segments = 0; /* loadable segments */
   Addr start_addr = 0;
   Addr end_addr = 0;
   Addr elfbrk = 0;
   SizeT max_align = VKI_PAGE_SIZE;

   /* 1. First pass over phdrs - determine number, span and max alignment. */
   const VKI_ESZ(Phdr) *phdr = phdrs;
   for (idx = 0; idx < ehdr->e_phnum; idx++, phdr = ADVANCE_PHDR(ehdr, phdr)) {
      /* Skip this header if no memory is requested. */
      if (phdr->p_memsz == 0)
         continue;

      if ((phdr->p_type == VKI_PT_LOAD) || (phdr->p_type == VKI_PT_SUNWBSS)) {
         Off64T offset = 0;

         if (VG_(clo_trace_syscalls))
            VG_(debugLog)(2, "syswrap-solaris", "mmapobj_process_phdrs: "
                             "program header #%u: addr=%#lx type=%#lx "
                             "prot=%#lx memsz=%#lx filesz=%#lx file "
                             "offset=%#lx\n", idx, phdr->p_vaddr,
                             (UWord) phdr->p_type, (UWord) phdr->p_flags,
                             phdr->p_memsz, phdr->p_filesz, phdr->p_offset);

         if (segments == 0) {
            first_segment_idx = idx;

            if (phdr->p_filesz == 0) {
               VG_(unimplemented)("Syswrap of the mmapobj call with the first "
                                  "loadable ELF program header specifying "
                                  "p_filesz == 0");
              /*NOTREACHED*/
              return res;
            }

            /* Address of the first segment must be either NULL or within the
               first page. */
            if ((ehdr->e_type == VKI_ET_DYN) &&
                ((phdr->p_vaddr & VKI_PAGEMASK) != 0)) {
               if (VG_(clo_trace_syscalls))
                  VG_(debugLog)(3, "syswrap-solaris", "mmapobj_process_phdrs: "
                                   "ELF program header #%u does not land on "
                                   "the first page (vaddr=%#lx)\n", idx,
                                   phdr->p_vaddr);
               return VG_(mk_SysRes_Error)(VKI_ENOTSUP);
            }

            start_addr = phdr->p_vaddr;
            /* The first segment is mapped from the beginning of the file (to
               include also the ELF header), so include this memory as well.
               Later on we flag this mapping with MR_HDR_ELF. */
            offset = phdr->p_offset;
         }

         if (phdr->p_align > 1) {
            if ((phdr->p_vaddr % phdr->p_align) !=
                (phdr->p_offset % phdr->p_align)) {
               if (VG_(clo_trace_syscalls))
                  VG_(debugLog)(3, "syswrap-solaris", "mmapobj_process_phdrs: "
                                   "ELF program header #%u does not have "
                                   "congruent offset and vaddr (vaddr=%#lx "
                                   "file offset=%#lx align=%#lx)\n", idx,
                                   phdr->p_vaddr, phdr->p_offset,
                                   phdr->p_align);
               return VG_(mk_SysRes_Error)(VKI_ENOTSUP);
            }
         }

         if (phdr->p_vaddr < end_addr) {
            if (VG_(clo_trace_syscalls))
               VG_(debugLog)(3, "syswrap-solaris", "mmapobj_process_phdrs: "
                                "ELF program header #%u specifies overlaping "
                                "address (vaddr=%#lx end_addr=%#lx)\n",
                                idx, phdr->p_vaddr, end_addr);
            return VG_(mk_SysRes_Error)(VKI_ENOTSUP);
         }

         end_addr = elfbrk = phdr->p_vaddr + phdr->p_memsz + offset;
         end_addr = VG_PGROUNDUP(end_addr);
         if (phdr->p_align > max_align) {
            max_align = phdr->p_align;
         }

         segments += 1;
      }
   }

   /* Alignment check - it should be power of two. */
   if ((max_align & (max_align - 1)) != 0) {
      if (VG_(clo_trace_syscalls))
         VG_(debugLog)(3, "syswrap-solaris", "mmapobj_process_phdrs: alignment "
                          "is not a power of 2 (%#lx)\n", max_align);
      return VG_(mk_SysRes_Error)(VKI_ENOTSUP);
   }
   vg_assert(max_align >= VKI_PAGE_SIZE);

#if defined(VGP_x86_solaris)
   if (max_align > VKI_UINT_MAX) {
      if (VG_(clo_trace_syscalls))
         VG_(debugLog)(3, "syswrap-solaris", "mmapobj_process_phdrs: alignment "
                          "for 32-bit ELF is >32-bits (%#lx)\n", max_align);
      return VG_(mk_SysRes_Error)(VKI_ENOTSUP);
   }
#endif /* VGP_x86_solaris */

   if (segments == 0) {
      if (VG_(clo_trace_syscalls))
         VG_(debugLog)(3, "syswrap-solaris", "mmapobj_process_phdrs: nothing "
                          "to map (0 loadable segments)");
      return VG_(mk_SysRes_Error)(VKI_ENOTSUP);
   }

   vg_assert(end_addr >= start_addr);
   SizeT span = end_addr - start_addr;
   if (span == 0) {
      if (VG_(clo_trace_syscalls))
         VG_(debugLog)(3, "syswrap-solaris", "mmapobj_process_phdrs: nothing "
                          "to map (%u loadable segments spanning 0 bytes)\n",
                          segments);
      return VG_(mk_SysRes_Error)(VKI_ENOTSUP);
   }
   vg_assert(first_segment_idx >= 0);

   if (segments > *elements) {
      if (VG_(clo_trace_syscalls))
         VG_(debugLog)(3, "syswrap-solaris", "mmapobj_process_phdrs: too many "
                          "segments (%u)\n", segments);
      return VG_(mk_SysRes_Error)(VKI_E2BIG);
   }

   if (VG_(clo_trace_syscalls))
      VG_(debugLog)(2, "syswrap-solaris", "mmapobj_process_phdrs: there "
                       "are %u loadable segments spanning %#lx bytes; max "
                       "align is %#lx\n", segments, span, max_align);

   /* Now get the aspacemgr oraculum advisory.
      Later on we mmap file-based and BSS mappings into this address space area
      as required and leave the holes unmapped. */
   if (ehdr->e_type == VKI_ET_DYN) {
      MapRequest mreq = {MAlign, max_align, span};
      Bool ok;
      start_addr = VG_(am_get_advisory)(&mreq, True /* forClient */, &ok);
      if (!ok) {
         if (VG_(clo_trace_syscalls))
            VG_(debugLog)(3, "syswrap-solaris", "mmapobj_process_phdrs: "
                             "failed to reserve address space of %#lx bytes "
                             "with alignment %#lx\n", span, max_align);
         return VG_(mk_SysRes_Error)(VKI_ENOMEM);
      }
      vg_assert(VG_ROUNDUP(start_addr, max_align) == start_addr);

      if (VG_(clo_trace_syscalls))
         VG_(debugLog)(2, "syswrap-solaris", "PRE(sys_mmapobj): address space "
                          "reserved at: vaddr=%#lx size=%#lx\n",
                          start_addr, span);
   } else {
      vg_assert(ehdr->e_type == VKI_ET_EXEC);
      /* ET_EXEC uses fixed mappings. Will be checked when processing phdrs. */
   }

   /* This is an utterly ugly hack, the aspacemgr assumes that only one
      segment is added at the time. However we add here multiple segments so
      AM_SANITY_CHECK inside the aspacemgr can easily fail. We want to
      prevent that thus we disable these checks. The scheduler will check the
      aspacemgr sanity after the syscall. */
   UInt sanity_level = VG_(clo_sanity_level);
   VG_(clo_sanity_level) = 1;

   /* 2. Second pass over phdrs - map the program headers and fill in
         the mmapobj_result_t array. */
   phdr = phdrs;
   *elements = 0;
   for (idx = 0; idx < ehdr->e_phnum; idx++, phdr = ADVANCE_PHDR(ehdr, phdr)) {
      /* Skip this header if no memory is requested. */
      if (phdr->p_memsz == 0)
         continue;

      if ((phdr->p_type == VKI_PT_LOAD) || (phdr->p_type == VKI_PT_SUNWBSS)) {
         UInt prot = 0;
         if (phdr->p_flags & VKI_PF_R)
            prot |= VKI_PROT_READ;
         if (phdr->p_flags & VKI_PF_W)
            prot |= VKI_PROT_WRITE;
         if (phdr->p_flags & VKI_PF_X)
            prot |= VKI_PROT_EXEC;

         vki_mmapobj_result_t *mrp = &storage[*elements];
         mrp->mr_msize = phdr->p_memsz;
         mrp->mr_fsize = phdr->p_filesz;
         mrp->mr_offset = 0;
         mrp->mr_prot = prot;
         mrp->mr_flags = 0;
         Off64T file_offset = phdr->p_offset;
         if (idx == first_segment_idx) {
            mrp->mr_flags = VKI_MR_HDR_ELF;
            if (ehdr->e_type == VKI_ET_DYN) {
               if (phdr->p_offset > 0) {
                  /* Include the ELF header into the first segment.
                     This means we ignore p_offset from the program header
                     and map from file offset 0. */
                  mrp->mr_msize += phdr->p_offset;
                  mrp->mr_fsize += phdr->p_offset;
                  file_offset = 0;
               }
            } else {
               vg_assert(ehdr->e_type == VKI_ET_EXEC);
               start_addr = phdr->p_vaddr;
            }
         }

         /* p_vaddr is absolute for ET_EXEC, and relative for ET_DYN. */
         mrp->mr_addr = (vki_caddr_t) phdr->p_vaddr;
         if (ehdr->e_type == VKI_ET_DYN) {
            mrp->mr_addr += start_addr;
         }

         SizeT page_offset = (Addr) mrp->mr_addr & VKI_PAGEOFFSET;
         if (page_offset > 0) {
            vg_assert(file_offset >= page_offset);
            /* Mapping address does not start at the beginning of a page.
               Therefore include some bytes before to make it page aligned. */
            mrp->mr_addr -= page_offset;
            mrp->mr_msize += page_offset;
            mrp->mr_offset = page_offset;
            file_offset -= page_offset;
         }
         SizeT file_size = mrp->mr_fsize + mrp->mr_offset;
         if (VG_(clo_trace_syscalls))
            VG_(debugLog)(2, "syswrap-solaris", "mmapobj_process_phdrs: "
                             "mmapobj result #%u: addr=%#lx msize=%#lx "
                             "fsize=%#lx mr_offset=%#lx prot=%#x flags=%#x\n",
                             *elements, (Addr) mrp->mr_addr,
                             (UWord) mrp->mr_msize, (UWord) mrp->mr_fsize,
                             (UWord) mrp->mr_offset, mrp->mr_prot,
                             mrp->mr_flags);

         UInt flags = VKI_MAP_PRIVATE | VKI_MAP_FIXED;
         if ((mrp->mr_prot & (VKI_PROT_WRITE | VKI_PROT_EXEC)) ==
                                                               VKI_PROT_EXEC) {
            flags |= VKI_MAP_TEXT;
         } else {
            flags |= VKI_MAP_INITDATA;
         }

         /* Determine if there will be partially unused page after file-based
            mapping. If so, then we need to zero it explicitly afterwards. */
         Addr mapping_end = (Addr) mrp->mr_addr + file_size;
         SizeT zeroed_size = VG_PGROUNDUP(mapping_end) - mapping_end;
         Bool mprotect_needed = False;
         if ((zeroed_size > 0) && ((prot & VKI_PROT_WRITE) == 0)) {
            prot |= VKI_PROT_WRITE;
            mprotect_needed = True;
         }

         if (ehdr->e_type == VKI_ET_EXEC) {
            /* Now check if the requested address space is available. */
            if (!VG_(am_is_free_or_resvn)((Addr) mrp->mr_addr, mrp->mr_msize)) {
               if (VG_(clo_trace_syscalls))
                  VG_(debugLog)(3, "syswrap-solaris", "mmapobj_process_phdrs: "
                                   "requested segment at %#lx with size of "
                                   "%#lx bytes is not available\n",
                                   (Addr) mrp->mr_addr, (UWord) mrp->mr_msize);
               res = VG_(mk_SysRes_Error)(VKI_EADDRINUSE);
               goto mmap_error;
            }
         }

         if (file_size > 0) {
            res = VG_(am_mmap_file_fixed_client_flags)((Addr) mrp->mr_addr,
                                       file_size, prot, flags, fd, file_offset);
            if (sr_isError(res)) {
               if (VG_(clo_trace_syscalls))
                  VG_(debugLog)(3, "syswrap-solaris", "mmapobj_process_phdrs: "
                                   "mmap failed: addr=%#lx size=%#lx prot=%#x "
                                   "flags=%#x fd=%d file offset=%#llx\n",
                                   (Addr) mrp->mr_addr, file_size,
                                   prot, flags, fd, (unsigned long long)file_offset);
               goto mmap_error;
            }

            VG_(debugLog)(1, "syswrap-solaris", "PRE(sys_mmapobj): new "
                             "segment: vaddr=%#lx size=%#lx prot=%#x "
                             "flags=%#x fd=%d file offset=%#llx\n",
                             (Addr) mrp->mr_addr, file_size, mrp->mr_prot,
                             flags, fd, (unsigned long long)file_offset);
         }

         if (zeroed_size > 0) {
            /* Now zero out the end of partially used page. */
            VG_(memset)((void *) mapping_end, 0, zeroed_size);
            if (mprotect_needed) {
               prot &= ~VKI_PROT_WRITE;
               res = VG_(do_syscall3)(SYS_mprotect, (Addr) mrp->mr_addr,
                                      file_size, prot);
               if (sr_isError(res)) {
                  if (VG_(clo_trace_syscalls))
                     VG_(debugLog)(3, "syswrap-solaris",
                                      "mmapobj_process_phdrs: mprotect failed: "
                                      "addr=%#lx size=%#lx prot=%#x",
                                      (Addr) mrp->mr_addr, file_size, prot);
                  /* Mapping for this segment was already established. */
                  idx += 1;
                  goto mmap_error;
               }
            }
         }

         if (file_size > 0) {
            ML_(notify_core_and_tool_of_mmap)((Addr) mrp->mr_addr, file_size,
                                              prot, flags, fd, file_offset);
         }

         /* Page(s) after the mapping backed up by the file are part of BSS.
            They need to be mmap'ed over with correct flags and will be
            implicitly zeroed. */
         mapping_end = VG_PGROUNDUP(mrp->mr_addr + mrp->mr_msize);
         Addr page_end = VG_PGROUNDUP(mrp->mr_addr + file_size);
         vg_assert(mapping_end >= page_end);
         zeroed_size = mapping_end - page_end;
         if (zeroed_size > 0) {
            flags = VKI_MAP_FIXED | VKI_MAP_PRIVATE | VKI_MAP_ANONYMOUS;
            res = VG_(am_mmap_anon_fixed_client)(page_end, zeroed_size, prot);
            if (sr_isError(res)) {
               if (VG_(clo_trace_syscalls))
                  VG_(debugLog)(3, "syswrap-solaris", "mmapobj_process_phdrs: "
                                   "mmap_anon failed: addr=%#lx size=%#lx "
                                   "prot=%#x\n", page_end, zeroed_size, prot);
               idx += 1; /* mapping for this segment was already established */
               goto mmap_error;
            }

            VG_(debugLog)(1, "syswrap-solaris", "PRE(sys_mmapobj): new "
                             "anonymous segment (BSS): vaddr=%#lx size=%#lx "
                             "prot=%#x\n", page_end, zeroed_size, prot);
            ML_(notify_core_and_tool_of_mmap)(page_end, zeroed_size,
                                              prot, flags, -1, 0);
         }

         VG_(di_notify_mmap)((Addr) mrp->mr_addr, False /*allow_SkFileV*/, fd);

         *elements += 1;
         vg_assert(*elements <= segments);
      }
   }

   if ((ehdr->e_type == VKI_ET_EXEC) && (!brk_segment_established)) {
      vg_assert(VG_(brk_base) == VG_(brk_limit));
      vg_assert(VG_(brk_base) == -1);
      VG_(brk_base) = VG_(brk_limit) = elfbrk;

      if (!VG_(setup_client_dataseg)()) {
         VG_(umsg)("Cannot map memory to initialize brk segment in thread #%u "
                   "at %#lx\n", tid, VG_(brk_base));
         res = VG_(mk_SysRes_Error)(VKI_ENOMEM);
         goto mmap_error;
      }

      VG_(track_client_dataseg)(tid);
   }

   /* Restore VG_(clo_sanity_level). The scheduler will perform the aspacemgr
      sanity check after the syscall. */
   VG_(clo_sanity_level) = sanity_level;

   return VG_(mk_SysRes_Success)(0);

mmap_error:
   for (i = idx - 1; i > 0; i--) {
      Bool discard_translations;
      Addr addr = (Addr) storage[i].mr_addr;

      VG_(am_munmap_client)(&discard_translations, addr, storage[i].mr_msize);
      ML_(notify_core_and_tool_of_munmap)(addr, storage[i].mr_msize);
   }
   *elements = 0;
   return res;

#undef ADVANCE_PHDR
}

static SysRes mmapobj_interpret(ThreadId tid, Int fd,
                                vki_mmapobj_result_t *storage,
                                vki_uint_t *elements)
{
   SysRes res;

   struct vg_stat stats;
   if (VG_(fstat)(fd, &stats) != 0) {
      return VG_(mk_SysRes_Error)(VKI_EBADF);
   }

   if (stats.size < sizeof(VKI_ESZ(Ehdr))) {
      if (VG_(clo_trace_syscalls))
         VG_(debugLog)(3, "syswrap-solaris", "mmapobj_interpret: insufficient "
                          "file size (%lld)\n", stats.size);
      return VG_(mk_SysRes_Error)(VKI_ENOTSUP);
   }

   /* Align the header buffer appropriately. */
   vki_ulong_t lheader[sizeof(VKI_ESZ(Ehdr)) / sizeof(vki_ulong_t) + 1];
   HChar *header = (HChar *) &lheader;

   res = VG_(pread)(fd, header, sizeof(VKI_ESZ(Ehdr)), 0);
   if (sr_isError(res)) {
      if (VG_(clo_trace_syscalls))
         VG_(debugLog)(3, "syswrap-solaris", "mmapobj_interpret: read of ELF "
                          "header failed\n");
      return res;
   } else if (sr_Res(res) != sizeof(VKI_ESZ(Ehdr))) {
      if (VG_(clo_trace_syscalls))
         VG_(debugLog)(3, "syswrap-solaris", "mmapobj_interpret: read of ELF "
                          "header failed - only %lu bytes out of %lu\n",
                          sr_Res(res), (UWord) sizeof(VKI_ESZ(Ehdr)));
      return VG_(mk_SysRes_Error)(VKI_ENOTSUP);
   }

   /* Verify file type is ELF. */
   if ((header[VKI_EI_MAG0] != VKI_ELFMAG0) ||
       (header[VKI_EI_MAG1] != VKI_ELFMAG1) ||
       (header[VKI_EI_MAG2] != VKI_ELFMAG2) ||
       (header[VKI_EI_MAG3] != VKI_ELFMAG3)) {
      if (VG_(clo_trace_syscalls))
         VG_(debugLog)(3, "syswrap-solaris", "mmapobj_interpret: ELF header "
                          "missing magic\n");
      return VG_(mk_SysRes_Error)(VKI_ENOTSUP);
   }

   if (header[VKI_EI_CLASS] != VG_ELF_CLASS) {
      if (VG_(clo_trace_syscalls))
         VG_(debugLog)(3, "syswrap-solaris", "mmapobj_interpret: ELF class "
                          "mismatch (%d vs %d)\n", header[VKI_EI_CLASS],
                          VG_ELF_CLASS);
      return VG_(mk_SysRes_Error)(VKI_ENOTSUP);
   }

   VKI_ESZ(Ehdr) *ehdr = (VKI_ESZ(Ehdr) *) header;
   if ((ehdr->e_type != VKI_ET_EXEC) && (ehdr->e_type != VKI_ET_DYN)) {
      VG_(unimplemented)("Syswrap of the mmapobj call with ELF type %u.",
                         ehdr->e_type);
      /*NOTREACHED*/
      return res;
   }

   if (ehdr->e_phnum == VKI_PN_XNUM) {
      VG_(unimplemented)("Syswrap of the mmapobj call with number of ELF "
                         "program headers == PN_XNUM");
      /*NOTREACHED*/
      return res;
   }

   /* Check alignment. */
#if defined(VGP_x86_solaris)
   if (!VG_IS_4_ALIGNED(ehdr->e_phentsize)) {
#elif defined(VGP_amd64_solaris)
   if (!VG_IS_8_ALIGNED(ehdr->e_phentsize)) {
#else
#  error "Unknown platform"
#endif
      if (VG_(clo_trace_syscalls))
         VG_(debugLog)(3, "syswrap-solaris", "mmapobj_interpret: ELF header "
                          "phentsize not aligned properly (%u)\n",
                          ehdr->e_phentsize);
      return VG_(mk_SysRes_Error)(VKI_ENOTSUP);
   }

   SizeT phdrs_size = ehdr->e_phnum * ehdr->e_phentsize;
   if (phdrs_size == 0) {
      if (VG_(clo_trace_syscalls))
         VG_(debugLog)(3, "syswrap-solaris", "mmapobj_interpret: no ELF "
                          "program headers\n");
      return VG_(mk_SysRes_Error)(VKI_ENOTSUP);
   }

   VKI_ESZ(Phdr) *phdrs = VG_(malloc)("syswrap.mi.1", phdrs_size);
   res = VG_(pread)(fd, phdrs, phdrs_size, ehdr->e_phoff);
   if (sr_isError(res)) {
      if (VG_(clo_trace_syscalls))
         VG_(debugLog)(3, "syswrap-solaris", "mmapobj_interpret: read of ELF "
                          "program headers failed\n");
      VG_(free)(phdrs);
      return VG_(mk_SysRes_Error)(VKI_ENOTSUP);
   } else if (sr_Res(res) != phdrs_size) {
      if (VG_(clo_trace_syscalls))
         VG_(debugLog)(3, "syswrap-solaris", "mmapobj_interpret: read of ELF "
                          "program headers failed - only %lu bytes out of %lu\n",
                          sr_Res(res), phdrs_size);
      VG_(free)(phdrs);
      return VG_(mk_SysRes_Error)(VKI_ENOTSUP);
   }

   if (VG_(clo_trace_syscalls))
      VG_(debugLog)(2, "syswrap-solaris", "mmapobj_interpret: %u ELF "
                       "program headers with total size of %lu bytes\n",
                       ehdr->e_phnum, phdrs_size);

   /* Now process the program headers. */
   res = mmapobj_process_phdrs(tid, fd, storage, elements, ehdr, phdrs);
   VG_(free)(phdrs);
   return res;
}

PRE(sys_mmapobj)
{
   /* int mmapobj(int fd, uint_t flags, mmapobj_result_t *storage,
                  uint_t *elements, void *arg); */
   PRINT("sys_mmapobj ( %ld, %#lx, %#lx, %#lx, %#lx )", SARG1, ARG2, ARG3,
         ARG4, ARG5);
   PRE_REG_READ5(long, "mmapobj", int, fd, vki_uint_t, flags,
                 mmapobj_result_t *, storage, uint_t *, elements,
                 void *, arg);

   PRE_MEM_READ("mmapobj(elements)", ARG4, sizeof(vki_uint_t));
   /*PRE_MEM_WRITE("mmapobj(elements)", ARG4, sizeof(vki_uint_t));*/
   if (ML_(safe_to_deref)((void*)ARG4, sizeof(vki_uint_t))) {
      vki_uint_t *u = (vki_uint_t*)ARG4;
      PRE_MEM_WRITE("mmapobj(storage)", ARG3,
                    *u * sizeof(vki_mmapobj_result_t));
   }

   if (ARG2 & VKI_MMOBJ_PADDING)
      PRE_MEM_READ("mmapobj(arg)", ARG5, sizeof(vki_size_t));

   /* Be strict. */
   if (!ML_(fd_allowed)(ARG1, "mmapobj", tid, False)) {
      SET_STATUS_Failure(VKI_EBADF);
      return;
   }

   /* We cannot advise mmapobj about desired address(es). Unfortunately
      kernel places mappings from mmapobj at the end of process address
      space, defeating memcheck's optimized fast 2-level array algorithm.
      So we need to emulate what mmapobj does in the kernel. */

   /* Sanity check on parameters. */
   if ((ARG2 & ~VKI_MMOBJ_ALL_FLAGS) != 0) {
      SET_STATUS_Failure(VKI_EINVAL);
      return;
   }

   if (!ML_(safe_to_deref)((void *) ARG4, sizeof(vki_uint_t))) {
      SET_STATUS_Failure(VKI_EFAULT);
      return;
   }
   vki_uint_t *elements = (vki_uint_t *) ARG4;

   if (*elements > 0) {
      if (!ML_(safe_to_deref)((void *) ARG3,
                              *elements * sizeof(vki_mmapobj_result_t))) {
         SET_STATUS_Failure(VKI_EFAULT);
         return;
      }
   }

   /* For now, supported is only MMOBJ_INTERPRET and no MMOBJ_PADDING. */
   if (ARG2 != VKI_MMOBJ_INTERPRET) {
      VG_(unimplemented)("Syswrap of the mmapobj call with flags %lu.", ARG2);
      /*NOTREACHED*/
      return;
   }

   SysRes res = mmapobj_interpret(tid, (Int) ARG1,
                                  (vki_mmapobj_result_t *) ARG3, elements);
   SET_STATUS_from_SysRes(res);

   if (!sr_isError(res)) {
      POST_MEM_WRITE(ARG4, sizeof(vki_uint_t));

      UInt idx;
      for (idx = 0; idx < *(vki_uint_t *) ARG4; idx++) {
         vki_mmapobj_result_t *mrp = &((vki_mmapobj_result_t *) ARG3)[idx];
         POST_FIELD_WRITE(mrp->mr_addr);
         POST_FIELD_WRITE(mrp->mr_msize);
         POST_FIELD_WRITE(mrp->mr_fsize);
         POST_FIELD_WRITE(mrp->mr_prot);
         POST_FIELD_WRITE(mrp->mr_flags);
         POST_FIELD_WRITE(mrp->mr_offset);
      }
   }
}

PRE(sys_memcntl)
{
   /* int memcntl(caddr_t addr, size_t len, int cmd, caddr_t arg,
                  int attr, int mask); */
   PRINT("sys_memcntl ( %#lx, %#lx, %ld, %#lx, %#lx, %#lx )", ARG1, ARG2,
         SARG3, ARG4, ARG5, ARG6);
   PRE_REG_READ6(long, "memcntl", void *, addr, vki_size_t, len, int, cmd,
                 void *, arg, int, attr, int, mask);

   if (ARG3 != VKI_MC_LOCKAS && ARG3 != VKI_MC_UNLOCKAS &&
       !ML_(valid_client_addr)(ARG1, ARG2, tid, "memcntl")) {
      /* MC_LOCKAS and MC_UNLOCKAS work on the complete address space thus we
         don't check the address range validity if these commands are
         requested. */
      SET_STATUS_Failure(VKI_ENOMEM);
      return;
   }

   if (ARG3 == VKI_MC_HAT_ADVISE)
      PRE_MEM_READ("memcntl(arg)", ARG4, sizeof(struct vki_memcntl_mha));
}

PRE(sys_getpmsg)
{
   /* int getpmsg(int fildes, struct strbuf *ctlptr, struct strbuf *dataptr,
                  int *bandp, int *flagsp); */
   struct vki_strbuf *ctrlptr = (struct vki_strbuf *)ARG2;
   struct vki_strbuf *dataptr = (struct vki_strbuf *)ARG3;
   *flags |= SfMayBlock;
   PRINT("sys_getpmsg ( %ld, %#lx, %#lx, %#lx, %#lx )", SARG1, ARG2, ARG3,
         ARG4, ARG5);
   PRE_REG_READ5(long, "getpmsg", int, fildes, struct vki_strbuf *, ctlptr,
                 struct vki_strbuf *, dataptr, int *, bandp, int *, flagsp);
   if (ctrlptr) {
      PRE_FIELD_READ("getpmsg(ctrlptr->maxlen)", ctrlptr->maxlen);
      PRE_FIELD_WRITE("getpmsg(ctrlptr->len)", ctrlptr->len);
      PRE_FIELD_READ("getpmsg(ctrlptr->buf)", ctrlptr->buf);
      if (ML_(safe_to_deref)((void*)ARG2, sizeof(struct vki_strbuf))
          && ctrlptr->maxlen > 0)
         PRE_MEM_WRITE("getpmsg(ctrlptr->buf)", (Addr)ctrlptr->buf,
                       ctrlptr->maxlen);
   }
   if (dataptr) {
      PRE_FIELD_READ("getpmsg(dataptr->maxlen)", dataptr->maxlen);
      PRE_FIELD_WRITE("getpmsg(dataptr->len)", dataptr->len);
      PRE_FIELD_READ("getpmsg(dataptr->buf)", dataptr->buf);
      if (ML_(safe_to_deref)((void*)ARG3, sizeof(struct vki_strbuf))
          && dataptr->maxlen > 0)
         PRE_MEM_WRITE("getpmsg(dataptr->buf)", (Addr)dataptr->buf,
                       dataptr->maxlen);
   }
   PRE_MEM_READ("getpmsg(bandp)", ARG4, sizeof(int));
   /*PRE_MEM_WRITE("getpmsg(bandp)", ARG4, sizeof(int));*/
   PRE_MEM_READ("getpmsg(flagsp)", ARG5, sizeof(int));
   /*PRE_MEM_WRITE("getpmsg(flagsp)", ARG5, sizeof(int));*/

   /* Be strict. */
   if (!ML_(fd_allowed)(ARG1, "getpmsg", tid, False))
      SET_STATUS_Failure(VKI_EBADF);
}

POST(sys_getpmsg)
{
   struct vki_strbuf *ctrlptr = (struct vki_strbuf *)ARG2;
   struct vki_strbuf *dataptr = (struct vki_strbuf *)ARG3;

   if (ctrlptr && ctrlptr->len > 0)
      POST_MEM_WRITE((Addr)ctrlptr->buf, ctrlptr->len);
   if (dataptr && dataptr->len > 0)
      POST_MEM_WRITE((Addr)dataptr->buf, dataptr->len);
   POST_MEM_WRITE(ARG4, sizeof(int));
   POST_MEM_WRITE(ARG5, sizeof(int));
}

PRE(sys_putpmsg)
{
   /* int putpmsg(int fildes, const struct strbuf *ctlptr,
                  const struct strbuf *dataptr, int band, int flags); */
   struct vki_strbuf *ctrlptr = (struct vki_strbuf *)ARG2;
   struct vki_strbuf *dataptr = (struct vki_strbuf *)ARG3;
   *flags |= SfMayBlock;
   PRINT("sys_putpmsg ( %ld, %#lx, %#lx, %ld, %ld )", SARG1, ARG2, ARG3, SARG4,
         SARG5);
   PRE_REG_READ5(long, "putpmsg", int, fildes, struct vki_strbuf *, ctrlptr,
                 struct vki_strbuf *, dataptr, int, band, int, flags);
   if (ctrlptr) {
      PRE_FIELD_READ("putpmsg(ctrlptr->len)", ctrlptr->len);
      PRE_FIELD_READ("putpmsg(ctrlptr->buf)", ctrlptr->buf);
      if (ML_(safe_to_deref)((void*)ARG2, sizeof(struct vki_strbuf))
          && ctrlptr->len > 0)
         PRE_MEM_READ("putpmsg(ctrlptr->buf)", (Addr)ctrlptr->buf,
                      ctrlptr->len);
   }
   if (dataptr) {
      PRE_FIELD_READ("putpmsg(dataptr->len)", dataptr->len);
      PRE_FIELD_READ("putpmsg(dataptr->buf)", dataptr->buf);
      if (ML_(safe_to_deref)((void*)ARG3, sizeof(struct vki_strbuf))
          && dataptr->len > 0)
         PRE_MEM_READ("putpmsg(dataptr->buf)", (Addr)dataptr->buf,
                      dataptr->len);
   }

   /* Be strict. */
   if (!ML_(fd_allowed)(ARG1, "putpmsg", tid, False))
      SET_STATUS_Failure(VKI_EBADF);
}

#if defined(SOLARIS_OLD_SYSCALLS)
PRE(sys_rename)
{
   /* int rename(const char *from, const char *to); */

   *flags |= SfMayBlock;
   PRINT("sys_rename ( %#lx(%s), %#lx(%s) )",
         ARG1, (HChar *) ARG1, ARG2, (HChar *) ARG2);
   PRE_REG_READ2(long, "rename", const char *, from, const char *, to);

   PRE_MEM_RASCIIZ("rename(from)", ARG1);
   PRE_MEM_RASCIIZ("rename(to)", ARG2);
}
#endif /* SOLARIS_OLD_SYSCALLS */

PRE(sys_uname)
{
   /* int uname(struct utsname *name); */
   PRINT("sys_uname ( %#lx )", ARG1);
   PRE_REG_READ1(long, "uname", struct vki_utsname *, name);
   PRE_MEM_WRITE("uname(name)", ARG1, sizeof(struct vki_utsname));
}

POST(sys_uname)
{
   struct vki_utsname *name = (struct vki_utsname *) ARG1;
   POST_MEM_WRITE((Addr) name->sysname, VG_(strlen)(name->sysname) + 1);
   POST_MEM_WRITE((Addr) name->nodename, VG_(strlen)(name->nodename) + 1);
   POST_MEM_WRITE((Addr) name->release, VG_(strlen)(name->release) + 1);
   POST_MEM_WRITE((Addr) name->version, VG_(strlen)(name->version) + 1);
   POST_MEM_WRITE((Addr) name->machine, VG_(strlen)(name->machine) + 1);
}

PRE(sys_setegid)
{
   /* int setegid(gid_t egid); */
   PRINT("sys_setegid ( %ld )", SARG1);
   PRE_REG_READ1(long, "setegid", vki_gid_t, egid);
}

PRE(sys_sysconfig)
{
   /* long sysconf(int name); */
   PRINT("sys_sysconfig ( %ld )", SARG1);
   PRE_REG_READ1(long, "sysconf", int, name);

   if (ARG1 == VKI_CONFIG_OPEN_FILES)
      SET_STATUS_Success(VG_(fd_soft_limit));
}

PRE(sys_systeminfo)
{
   /* int sysinfo(int command, char *buf, long count); */
   PRINT("sys_systeminfo ( %ld, %#lx, %ld )", SARG1, ARG2, SARG3);
   PRE_REG_READ3(long, "sysinfo", int, command, char *, buf, long, count);

   switch (ARG1 /*command*/) {
   case VKI_SI_SYSNAME:
   case VKI_SI_HOSTNAME:
   case VKI_SI_RELEASE:
   case VKI_SI_VERSION:
   case VKI_SI_MACHINE:
   case VKI_SI_ARCHITECTURE:
   case VKI_SI_HW_SERIAL:
   case VKI_SI_HW_PROVIDER:
   case VKI_SI_SRPC_DOMAIN:
   case VKI_SI_PLATFORM:
   case VKI_SI_ISALIST:
   case VKI_SI_DHCP_CACHE:
   case VKI_SI_ARCHITECTURE_32:
   case VKI_SI_ARCHITECTURE_64:
   case VKI_SI_ARCHITECTURE_K:
   case VKI_SI_ARCHITECTURE_NATIVE:
      PRE_MEM_WRITE("sysinfo(buf)", ARG2, ARG3);
      break;

   case VKI_SI_SET_HOSTNAME:
   case VKI_SI_SET_SRCP_DOMAIN:
      PRE_MEM_RASCIIZ("sysinfo(buf)", ARG2);
      break;

   default:
      VG_(unimplemented)("Syswrap of the sysinfo call with command %ld.", SARG1);
      /*NOTREACHED*/
      break;
   }
}

POST(sys_systeminfo)
{
   if (ARG1 != VKI_SI_SET_HOSTNAME && ARG1 != VKI_SI_SET_SRCP_DOMAIN)
      POST_MEM_WRITE(ARG2, MIN(RES, ARG3));
}

PRE(sys_seteuid)
{
   /* int seteuid(uid_t euid); */
   PRINT("sys_seteuid ( %ld )", SARG1);
   PRE_REG_READ1(long, "seteuid", vki_uid_t, euid);
}

PRE(sys_forksys)
{
   /* int64_t forksys(int subcode, int flags); */
   Int fds[2];
   Int res;
   PRINT("sys_forksys ( %ld, %ld )", SARG1, SARG2);
   PRE_REG_READ2(long, "forksys", int, subcode, int, flags);

   if (ARG1 == 1) {
      /* Support for forkall() requires changes to the big lock processing
         which are not yet implemented. */
      VG_(unimplemented)("Support for forkall().");
      /*NOTREACHED*/
      return;
   }

   if (ARG1 != 0 && ARG1 != 2) {
      VG_(unimplemented)("Syswrap of the forksys call where subcode=%ld.",
                         SARG1);
      /*NOTREACHED*/
   }

   if (ARG1 == 2) {
      /* vfork() is requested. Translate it to a normal fork() but work around
         a problem with posix_spawn() which relies on the real vfork()
         behaviour. See a description in vg_preloaded.c for details. */
      res = VG_(pipe)(fds);
      vg_assert(res == 0);

      vg_assert(fds[0] != fds[1]);

      /* Move to Valgrind fds and set close-on-exec flag on both of them (done
         by VG_(safe_fd). */
      fds[0] = VG_(safe_fd)(fds[0]);
      fds[1] = VG_(safe_fd)(fds[1]);
      vg_assert(fds[0] != fds[1]);

      vg_assert(VG_(vfork_fildes_addr) != NULL);
      vg_assert(*VG_(vfork_fildes_addr) == -1);
      *VG_(vfork_fildes_addr) = fds[0];
   }

   VG_(do_atfork_pre)(tid);
   SET_STATUS_from_SysRes(VG_(do_syscall2)(__NR_forksys, 0, ARG2));

   if (!SUCCESS) {
      /* vfork */
      if (ARG1 == 2) {
         VG_(close)(fds[0]);
         VG_(close)(fds[1]);
      }

      return;
   }

   if (RESHI) {
      VG_(do_atfork_child)(tid);

      /* vfork */
      if (ARG1 == 2)
         VG_(close)(fds[1]);

#     if defined(SOLARIS_PT_SUNDWTRACE_THRP)
      /* Kernel can map a new page as a scratch space of the DTrace fasttrap
         provider. There is no way we can directly get its address - it's all
         private to the kernel. Fish it the slow way. */
      Addr addr;
      SizeT size;
      UInt prot;
      Bool found = VG_(am_search_for_new_segment)(&addr, &size, &prot);
      if (found) {
         VG_(debugLog)(1, "syswrap-solaris", "PRE(forksys), new segment: "
                       "vaddr=%#lx, size=%#lx, prot=%#x\n", addr, size, prot);
         vg_assert(prot == (VKI_PROT_READ | VKI_PROT_EXEC));
         vg_assert(size == VKI_PAGE_SIZE);
         ML_(notify_core_and_tool_of_mmap)(addr, size, prot, VKI_MAP_ANONYMOUS,
                                           -1, 0);

         /* Note: We don't notify the debuginfo reader about this mapping
            because there is no debug information stored in this segment. */
      }
#     endif /* SOLARIS_PT_SUNDWTRACE_THRP */
   }
   else {
      VG_(do_atfork_parent)(tid);

      /* Print information about the fork. */
      PRINT("   fork: process %d created child %d\n", VG_(getpid)(),
            (Int)RES);

      /* vfork */
      if (ARG1 == 2) {
         /* Wait for the child to finish (exec or exit). */
         UChar w;

         VG_(close)(fds[0]);

         res = VG_(read)(fds[1], &w, 1);
         if (res == 1)
            SET_STATUS_Failure(w);
         VG_(close)(fds[1]);

         *VG_(vfork_fildes_addr) = -1;
      }
   }
}

#if defined(SOLARIS_GETRANDOM_SYSCALL)
PRE(sys_getrandom)
{
   /* int getrandom(void *buf, size_t buflen, uint_t flags); */
   PRINT("sys_getrandom ( %#lx, %lu, %lu )", ARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "getrandom", void *, buf, vki_size_t, buflen,
                 vki_uint_t, flags);
   PRE_MEM_WRITE("getrandom(buf)", ARG1, ARG2);
}

POST(sys_getrandom)
{
   POST_MEM_WRITE(ARG1, RES);
}
#endif /* SOLARIS_GETRANDOM_SYSCALL */

PRE(sys_sigtimedwait)
{
   /* int sigtimedwait(const sigset_t *set, siginfo_t *info,
                       const timespec_t *timeout); */
   *flags |= SfMayBlock;
   PRINT("sys_sigtimedwait ( %#lx, %#lx, %#lx )", ARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "sigtimedwait", vki_sigset_t *, set,
                 vki_siginfo_t *, info, vki_timespec_t *, timeout);
   PRE_MEM_READ("sigtimewait(set)", ARG1, sizeof(vki_sigset_t));
   if (ARG2)
      PRE_MEM_WRITE("sigtimedwait(info)", ARG2, sizeof(vki_siginfo_t));
   if (ARG3)
      PRE_MEM_READ("sigtimedwait(timeout)", ARG3, sizeof(vki_timespec_t));
}

POST(sys_sigtimedwait)
{
   if (ARG2)
      POST_MEM_WRITE(ARG2, sizeof(vki_siginfo_t));
}

PRE(sys_yield)
{
   /* void yield(void); */
   *flags |= SfMayBlock;
   PRINT("sys_yield ( )");
   PRE_REG_READ0(long, "yield");
}

PRE(sys_lwp_sema_post)
{
   /* int lwp_sema_post(lwp_sema_t *sema); */
   vki_lwp_sema_t *sema = (vki_lwp_sema_t*)ARG1;
   *flags |= SfMayBlock;
   PRINT("sys_lwp_sema_post ( %#lx )", ARG1);
   PRE_REG_READ1(long, "lwp_sema_post", lwp_sema_t *, sema);

   PRE_FIELD_READ("lwp_sema_post(sema->type)", sema->vki_sema_type);
   PRE_FIELD_READ("lwp_sema_post(sema->count)", sema->vki_sema_count);
   /*PRE_FIELD_WRITE("lwp_sema_post(sema->count)", sema->vki_sema_count);*/
   PRE_FIELD_READ("lwp_sema_post(sema->waiters)", sema->vki_sema_waiters);
   /*PRE_FIELD_WRITE("lwp_sema_post(sema->waiters)", sema->vki_sema_waiters);*/
}

POST(sys_lwp_sema_post)
{
   vki_lwp_sema_t *sema = (vki_lwp_sema_t*)ARG1;
   POST_FIELD_WRITE(sema->vki_sema_count);
   POST_FIELD_WRITE(sema->vki_sema_waiters);
}

PRE(sys_lwp_sema_trywait)
{
   /* int lwp_sema_trywait(lwp_sema_t *sema); */
   vki_lwp_sema_t *sema = (vki_lwp_sema_t*)ARG1;
   PRINT("sys_lwp_sema_trywait ( %#lx )", ARG1);
   PRE_REG_READ1(long, "lwp_sema_trywait", lwp_sema_t *, sema);

   PRE_FIELD_READ("lwp_sema_trywait(sema->type)", sema->vki_sema_type);
   PRE_FIELD_READ("lwp_sema_trywait(sema->count)", sema->vki_sema_count);
   /*PRE_FIELD_WRITE("lwp_sema_trywait(sema->count)", sema->vki_sema_count);*/
   PRE_FIELD_READ("lwp_sema_trywait(sema->waiters)", sema->vki_sema_waiters);
   /*PRE_FIELD_WRITE("lwp_sema_trywait(sema->waiters)",
     sema->vki_sema_waiters);*/
}

POST(sys_lwp_sema_trywait)
{
   vki_lwp_sema_t *sema = (vki_lwp_sema_t*)ARG1;
   POST_FIELD_WRITE(sema->vki_sema_count);
   POST_FIELD_WRITE(sema->vki_sema_waiters);
}

PRE(sys_lwp_detach)
{
   /* int lwp_detach(id_t lwpid); */
   PRINT("sys_lwp_detach ( %ld )", SARG1);
   PRE_REG_READ1(long, "lwp_detach", vki_id_t, lwpid);
}

PRE(sys_modctl)
{
   /* int modctl(int cmd, uintptr_t a1, uintptr_t a2, uintptr_t a3,
                 uintptr_t a4, uintptr_t a5); */
   *flags |= SfMayBlock;

   switch (ARG1 /*cmd*/) {
   case VKI_MODLOAD:
      /* int modctl_modload(int use_path, char *filename, int *rvp); */
      PRINT("sys_modctl ( %ld, %lu, %#lx(%s), %#lx )",
            SARG1, ARG2, ARG3, (HChar *) ARG3, ARG4);
      PRE_REG_READ4(long, SC2("modctl", "modload"),
                    int, cmd, int, use_path, char *, filename, int *, rvp);
      PRE_MEM_RASCIIZ("modctl(filaneme)", ARG3);
      if (ARG4 != 0) {
         PRE_MEM_WRITE("modctl(rvp)", ARG4, sizeof(int *));
      }
      break;
   case VKI_MODUNLOAD:
      /* int modctl_modunload(modid_t id); */
      PRINT("sys_modctl ( %ld, %ld )", SARG1, SARG2);
      PRE_REG_READ2(long, SC2("modctl", "modunload"),
                    int, cmd, vki_modid_t, id);
      break;
   case VKI_MODINFO: {
      /* int modctl_modinfo(modid_t id, struct modinfo *umodi); */
      PRINT("sys_modctl ( %ld, %ld, %#lx )", SARG1, SARG2, ARG3);
      PRE_REG_READ3(long, SC2("modctl", "modinfo"),
                    int, cmd, vki_modid_t, id, struct modinfo *, umodi);

      struct vki_modinfo *umodi = (struct vki_modinfo *) ARG3;
      PRE_FIELD_READ("modctl(umodi->mi_info)", umodi->mi_info);
      PRE_FIELD_READ("modctl(umodi->mi_id)", umodi->mi_id);
      PRE_FIELD_READ("modctl(umodi->mi_nextid)", umodi->mi_nextid);
      PRE_MEM_WRITE("modctl(umodi)", ARG3, sizeof(struct vki_modinfo));
      break;
   }

#  if defined(SOLARIS_MODCTL_MODNVL)
   case VKI_MODNVL_DEVLINKSYNC:
      /* int modnvl_devlinksync(sysnvl_op_t a1, uintptr_t a2, uintptr_t a3,
                                uintptr_t a4); */
      switch (ARG2 /*op*/) {

#     if defined(HAVE_SYS_SYSNVL_H)
      case VKI_SYSNVL_OP_GET:
         PRE_REG_READ5(long, SC3("modctl", "modnvl_devlinksync", "get"),
                       int, cmd, sysnvl_op_t, a1, char *, bufp,
                       uint64_t *, buflenp, uint64_t *, genp);
#     else
      case VKI_MODCTL_NVL_OP_GET:
         PRE_REG_READ5(long, SC3("modctl", "modnvl_devlinksync", "get"),
                       int, cmd, modctl_nvl_op_t, a1, char *, bufp,
                       uint64_t *, buflenp, uint64_t *, genp);
#     endif /* HAVE_SYS_SYSNVL_H */

         PRINT("sys_modctl ( %ld, %lu, %#lx, %#lx, %#lx )",
               SARG1, ARG2, ARG3, ARG4, ARG5);
         PRE_MEM_WRITE("modctl(buflenp)", ARG4, sizeof(vki_uint64_t));
         if (ML_(safe_to_deref)((vki_uint64_t *) ARG4, sizeof(vki_uint64_t))) {
            if (ARG3 != 0) {
               PRE_MEM_WRITE("modctl(bufp)", ARG3, *(vki_uint64_t *) ARG4);
            }
         }
         if (ARG5 != 0) {
            PRE_MEM_WRITE("modctl(genp)", ARG5, sizeof(vki_uint64_t));
         }
         break;

#     if defined(HAVE_SYS_SYSNVL_H)
      case VKI_SYSNVL_OP_UPDATE:
         PRE_REG_READ4(long, SC3("modctl", "modnvl_devlinksync", "update"),
                       int, cmd, sysnvl_op_t, a1, char *, bufp,
                       uint64_t *, buflenp);
#     else
      case VKI_MODCTL_NVL_OP_UPDATE:
         PRE_REG_READ4(long, SC3("modctl", "modnvl_devlinksync", "update"),
                       int, cmd, modctl_nvl_op_t, a1, char *, bufp,
                       uint64_t *, buflenp);
#     endif /* HAVE_SYS_SYSNVL_H */

         PRINT("sys_modctl ( %ld, %lu, %#lx, %#lx )", SARG1, ARG2, ARG3, ARG4);
         PRE_MEM_READ("modctl(buflenp)", ARG4, sizeof(vki_uint64_t));
         if (ML_(safe_to_deref)((vki_uint64_t *) ARG4, sizeof(vki_uint64_t))) {
            PRE_MEM_READ("modctl(bufp)", ARG3, *(vki_uint64_t *) ARG4);
         }
         break;

      default:
         VG_(unimplemented)("Syswrap of the modctl call with command "
                            "MODNVL_DEVLINKSYNC and op %ld.", ARG2);
         /*NOTREACHED*/
         break;
      }
      break;

   case VKI_MODDEVINFO_CACHE_TS:
      /* int modctl_devinfo_cache_ts(uint64_t *utsp); */
      PRINT("sys_modctl ( %ld, %#lx )", SARG1, ARG2);
      PRE_REG_READ2(long, SC2("modctl", "moddevinfo_cache_ts"),
                    int, cmd, uint64_t *, utsp);
      PRE_MEM_WRITE("modctl(utsp)", ARG2, sizeof(vki_uint64_t));
      break;
#  endif /* SOLARIS_MODCTL_MODNVL */

   default:
      VG_(unimplemented)("Syswrap of the modctl call with command %ld.", SARG1);
      /*NOTREACHED*/
      break;
   }
}

POST(sys_modctl)
{
   switch (ARG1 /*cmd*/) {
   case VKI_MODLOAD:
      if (ARG4 != 0) {
         POST_MEM_WRITE(ARG4, sizeof(int *));
      }
      break;
   case VKI_MODUNLOAD:
      break;
   case VKI_MODINFO:
      POST_MEM_WRITE(ARG3, sizeof(struct vki_modinfo));
      break;
#  if defined(SOLARIS_MODCTL_MODNVL)
   case VKI_MODNVL_DEVLINKSYNC:
      switch (ARG2 /*op*/) {

#     if defined(HAVE_SYS_SYSNVL_H)
      case VKI_SYSNVL_OP_GET:
#     else
      case VKI_MODCTL_NVL_OP_GET:
#     endif /* HAVE_SYS_SYSNVL_H */

         POST_MEM_WRITE(ARG4, sizeof(vki_uint64_t));
         if (ARG3 != 0) {
            POST_MEM_WRITE(ARG3, *(vki_uint64_t *) ARG4);
         }
         if (ARG5 != 0) {
            POST_MEM_WRITE(ARG5, sizeof(vki_uint64_t));
         }
         break;

#     if defined(HAVE_SYS_SYSNVL_H)
      case VKI_SYSNVL_OP_UPDATE:
#     else
      case VKI_MODCTL_NVL_OP_UPDATE:
#     endif /* HAVE_SYS_SYSNVL_H */
         break;

      default:
         vg_assert(0);
         break;
      }
      break;
   case VKI_MODDEVINFO_CACHE_TS:
      POST_MEM_WRITE(ARG2, sizeof(vki_uint64_t));
      break;
#  endif /* SOLARIS_MODCTL_MODNVL */

   default:
      vg_assert(0);
      break;
   }
}

PRE(sys_fchroot)
{
   /* int fchroot(int fd); */
   PRINT("sys_fchroot ( %ld )", SARG1);
   PRE_REG_READ1(long, "fchroot", int, fd);

   /* Be strict. */
   if (!ML_(fd_allowed)(ARG1, "fchroot", tid, False))
      SET_STATUS_Failure(VKI_EBADF);
}

#if defined(SOLARIS_SYSTEM_STATS_SYSCALL)
PRE(sys_system_stats)
{
   /* void system_stats(int flag); */
   PRINT("sys_system_stats ( %ld )", SARG1);
   PRE_REG_READ1(void, "system_stats", int, flag);
}
#endif /* SOLARIS_SYSTEM_STATS_SYSCALL */

PRE(sys_gettimeofday)
{
   /* Kernel: int gettimeofday(struct timeval *tp); */
   PRINT("sys_gettimeofday ( %#lx )", ARG1);
   PRE_REG_READ1(long, "gettimeofday", struct timeval *, tp);
   if (ARG1)
      PRE_timeval_WRITE("gettimeofday(tp)", ARG1);
}

POST(sys_gettimeofday)
{
   if (ARG1)
      POST_timeval_WRITE(ARG1);
}

PRE(sys_lwp_create)
{
   /* int lwp_create(ucontext_t *ucp, int flags, id_t *new_lwp) */

   ThreadId ctid;
   ThreadState *ptst;
   ThreadState *ctst;
   Addr stack;
   SysRes res;
   vki_ucontext_t uc;
   Bool tool_informed = False;

   PRINT("sys_lwp_create ( %#lx, %lu, %#lx )", ARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "lwp_create", ucontext_t *, ucp, int, flags,
                 id_t *, new_lwp);

   if (ARG3 != 0)
      PRE_MEM_WRITE("lwp_create(new_lwp)", ARG3, sizeof(vki_id_t));

   /* If we can't deref ucontext_t then we can't do anything. */
   if (!ML_(safe_to_deref)((void*)ARG1, sizeof(vki_ucontext_t))) {
      SET_STATUS_Failure(VKI_EINVAL);
      return;
   }

   ctid = VG_(alloc_ThreadState)();
   ptst = VG_(get_ThreadState)(tid);
   ctst = VG_(get_ThreadState)(ctid);

   /* Stay sane. */
   vg_assert(VG_(is_running_thread)(tid));
   vg_assert(VG_(is_valid_tid)(ctid));

   stack = ML_(allocstack)(ctid);
   if (!stack) {
      res = VG_(mk_SysRes_Error)(VKI_ENOMEM);
      goto out;
   }

   /* First inherit parent's guest state */
   ctst->arch.vex = ptst->arch.vex;
   ctst->arch.vex_shadow1 = ptst->arch.vex_shadow1;
   ctst->arch.vex_shadow2 = ptst->arch.vex_shadow2;

   /* Set up some values. */
   ctst->os_state.parent = tid;
   ctst->os_state.threadgroup = ptst->os_state.threadgroup;
   ctst->sig_mask = ptst->sig_mask;
   ctst->tmp_sig_mask = ptst->sig_mask;

   /* No stack definition should be currently present.  The stack will be set
      later by libc by a setustack() call (the getsetcontext syscall). */
   ctst->client_stack_highest_byte = 0;
   ctst->client_stack_szB = 0;
   vg_assert(ctst->os_state.stk_id == NULL_STK_ID);

   /* Inform a tool that a new thread is created.  This has to be done before
      any other core->tool event is sent. */
   vg_assert(VG_(owns_BigLock_LL)(tid));
   VG_TRACK(pre_thread_ll_create, tid, ctid);
   tool_informed = True;

#if defined(VGP_x86_solaris)
   /* Set up GDT (this has to be done before calling
      VG_(restore_context)(). */
   ML_(setup_gdt)(&ctst->arch.vex);
#elif defined(VGP_amd64_solaris)
   /* Nothing to do. */
#else
#  error "Unknown platform"
#endif

   /* Now set up the new thread according to ucontext_t. */
   VG_(restore_context)(ctid, (vki_ucontext_t*)ARG1, Vg_CoreSysCall,
                        True/*esp_is_thrptr*/);

   /* Set up V thread (this also tells the kernel to block all signals in the
      thread). */
   ML_(setup_start_thread_context)(ctid, &uc);

   /* Actually create the new thread. */
   res = VG_(do_syscall3)(__NR_lwp_create, (UWord)&uc, ARG2, ARG3);

   if (!sr_isError(res)) {
      if (ARG3 != 0)
         POST_MEM_WRITE(ARG3, sizeof(vki_id_t));
      if (ARG2 & VKI_LWP_DAEMON)
         ctst->os_state.daemon_thread = True;
   }

out:
   if (sr_isError(res)) {
      if (tool_informed) {
         /* Tell a tool the thread exited in a hurry. */
         VG_TRACK(pre_thread_ll_exit, ctid);
      }

      /* lwp_create failed. */
      VG_(cleanup_thread)(&ctst->arch);
      ctst->status = VgTs_Empty;
   }

   SET_STATUS_from_SysRes(res);
}

PRE(sys_lwp_exit)
{
   /* void syslwp_exit(); */
   ThreadState *tst = VG_(get_ThreadState)(tid);
   PRINT("sys_lwp_exit ( )");
   PRE_REG_READ0(long, "lwp_exit");

   /* Set the thread's status to be exiting, then claim that the syscall
      succeeded. */
   tst->exitreason = VgSrc_ExitThread;
   tst->os_state.exitcode = 0;
   SET_STATUS_Success(0);
}

PRE(sys_lwp_suspend)
{
   /* int lwp_suspend(id_t lwpid); */
   ThreadState *tst = VG_(get_ThreadState)(tid);
   PRINT("sys_lwp_suspend ( %ld )", SARG1);
   PRE_REG_READ1(long, "lwp_suspend", vki_id_t, lwpid);

   if (ARG1 == tst->os_state.lwpid) {
      /* Set the SfMayBlock flag only if the currently running thread should
         be suspended. If this flag was used also when suspending other
         threads then it could happen that a thread holding the_BigLock would
         be suspended and Valgrind would hang. */
      *flags |= SfMayBlock;
   }
}

PRE(sys_lwp_continue)
{
   /* int lwp_continue(id_t target_lwp); */
   PRINT("sys_lwp_continue ( %ld )", SARG1);
   PRE_REG_READ1(long, "lwp_continue", vki_id_t, target_lwp);
}

static void
do_lwp_sigqueue(const HChar *syscall_name, UWord target_lwp, UWord signo,
                SyscallStatus *status, UWord *flags)
{
   if (!ML_(client_signal_OK)(signo)) {
      SET_STATUS_Failure(VKI_EINVAL);
      return;
   }

   /* Check to see if this gave us a pending signal. */
   *flags |= SfPollAfter;

   if (VG_(clo_trace_signals))
      VG_(message)(Vg_DebugMsg, "%s: sending signal %lu to thread %lu\n",
                   syscall_name, signo, target_lwp);

   /* If we're sending SIGKILL, check to see if the target is one of our
      threads and handle it specially. */
   if (signo == VKI_SIGKILL && ML_(do_sigkill)(target_lwp, -1)) {
      SET_STATUS_Success(0);
      return;
   }

   /* Ask to handle this syscall via the slow route, since that's the only one
      that sets tst->status to VgTs_WaitSys.  If the result of doing the
      syscall is an immediate run of async_signalhandler() in m_signals.c,
      then we need the thread to be properly tidied away. */
   *flags |= SfMayBlock;
}

#if defined(SOLARIS_LWP_SIGQUEUE_SYSCALL)
#if defined(SOLARIS_LWP_SIGQUEUE_SYSCALL_TAKES_PID)
PRE(sys_lwp_sigqueue)
{
   /* int lwp_sigqueue(pid_t target_pid, id_t target_lwp, int signal,
                       void *value, int si_code, timespec_t *timeout);
    */
   PRINT("sys_lwp_sigqueue ( %ld, %ld, %ld, %#lx, %ld, %#lx )",
         SARG1, SARG2, SARG3, ARG4, SARG5, ARG6);
   PRE_REG_READ6(long, "lwp_sigqueue", vki_pid_t, target_pid,
                 vki_id_t, target_lwp, int, signal, void *, value, int, si_code,
                 vki_timespec_t *, timeout);

   if (ARG6)
      PRE_MEM_READ("lwp_sigqueue(timeout)", ARG6, sizeof(vki_timespec_t));

   if ((ARG1 == 0) || (ARG1 == VG_(getpid)())) {
      do_lwp_sigqueue("lwp_sigqueue", ARG2, ARG3, status, flags);
   } else {
      /* Signal is sent to a different process. */
      if (VG_(clo_trace_signals))
         VG_(message)(Vg_DebugMsg, "lwp_sigqueue: sending signal %ld to "
                      "process %ld, thread %ld\n", SARG3, SARG1, SARG2);
     *flags |= SfMayBlock;
   }
}

POST(sys_lwp_sigqueue)
{
   if (VG_(clo_trace_signals))
      VG_(message)(Vg_DebugMsg, "lwp_sigqueue: sent signal %ld to process %ld, "
                   "thread %ld\n", SARG3, SARG1, SARG2);
}

#else

PRE(sys_lwp_sigqueue)
{
   /* int lwp_sigqueue(id_t target_lwp, int signal, void *value,
                       int si_code, timespec_t *timeout);
    */
   PRINT("sys_lwp_sigqueue ( %ld, %ld, %#lx, %ld, %#lx )",
         SARG1, SARG2, ARG3, SARG4, ARG5);
   PRE_REG_READ5(long, "lwp_sigqueue", vki_id_t, target_lwp, int, signal,
                 void *, value, int, si_code, vki_timespec_t *, timeout);

   if (ARG5)
      PRE_MEM_READ("lwp_sigqueue(timeout)", ARG5, sizeof(vki_timespec_t));

   do_lwp_sigqueue("lwp_sigqueue", ARG1, ARG2, status, flags);
}

POST(sys_lwp_sigqueue)
{
   if (VG_(clo_trace_signals))
      VG_(message)(Vg_DebugMsg, "lwp_sigqueue: sent signal %lu to thread %lu\n",
                   ARG2, ARG1);
}


#endif /* SOLARIS_LWP_SIGQUEUE_SYSCALL_TAKES_PID */

#else

PRE(sys_lwp_kill)
{
   /* int lwp_kill(id_t target_lwp, int signal); */
   PRINT("sys_lwp_kill ( %ld, %ld )", SARG1, SARG2);
   PRE_REG_READ2(long, "lwp_kill", vki_id_t, target_lwp, int, signal);

   do_lwp_sigqueue("lwp_kill", ARG1, ARG2, status, flags);
}

POST(sys_lwp_kill)
{
   if (VG_(clo_trace_signals))
      VG_(message)(Vg_DebugMsg, "lwp_kill: sent signal %lu to thread %lu\n",
                   ARG2, ARG1);
}
#endif /* SOLARIS_LWP_SIGQUEUE_SYSCALL */

PRE(sys_lwp_self)
{
   /* id_t lwp_self(void); */
   PRINT("sys_lwp_self ( )");
   PRE_REG_READ0(long, "lwp_self");
}

PRE(sys_lwp_sigmask)
{
   /* int64_t lwp_sigmask(int how, uint_t bits0, uint_t bits1, uint_t bits2,
                          uint_t bits3); */
   vki_sigset_t sigset;
   PRINT("sys_lwp_sigmask ( %ld, %#lx, %#lx, %#lx, %#lx )", SARG1, ARG2, ARG3,
         ARG4, ARG5);
   PRE_REG_READ5(long, "lwp_sigmask", int, how, vki_uint_t, bits0,
                 vki_uint_t, bits1, vki_uint_t, bits2, vki_uint_t, bits3);

   sigset.__sigbits[0] = ARG2;
   sigset.__sigbits[1] = ARG3;
   sigset.__sigbits[2] = ARG4;
   sigset.__sigbits[3] = ARG5;

   SET_STATUS_from_SysRes(
      VG_(do_sys_sigprocmask)(tid, ARG1 /*how*/, &sigset, NULL)
   );

   if (SUCCESS)
      *flags |= SfPollAfter;
}

PRE(sys_lwp_private)
{
   /* int lwp_private(int cmd, int which, uintptr_t base); */
   ThreadState *tst = VG_(get_ThreadState)(tid);
   Int supported_base, supported_sel;
   PRINT("sys_lwp_private ( %ld, %ld, %#lx )", SARG1, SARG2, ARG3);
   PRE_REG_READ3(long, "lwp_private", int, cmd, int, which,
                 uintptr_t, base);

   /* Note: Only the %gs base is currently supported on x86 and the %fs base
      on amd64.  Support for the %fs base on x86 and for the %gs base on amd64
      should be added.  Anything else is probably a client program error. */
#if defined(VGP_x86_solaris)
   supported_base = VKI_LWP_GSBASE;
   supported_sel = VKI_LWPGS_SEL;
#elif defined(VGP_amd64_solaris)
   supported_base = VKI_LWP_FSBASE;
   supported_sel = 0;
#else
#error "Unknown platform"
#endif
   if (ARG2 != supported_base) {
      VG_(unimplemented)("Syswrap of the lwp_private call where which=%ld.",
                         SARG2);
      /*NOTREACHED*/
   }

   switch (ARG1 /*cmd*/) {
   case VKI_LWP_SETPRIVATE:
#if defined(VGP_x86_solaris)
      tst->os_state.thrptr = ARG3;
      ML_(update_gdt_lwpgs)(tid);
#elif defined(VGP_amd64_solaris)
      tst->arch.vex.guest_FS_CONST = ARG3;
#else
#error "Unknown platform"
#endif
      SET_STATUS_Success(supported_sel);
      break;
   case VKI_LWP_GETPRIVATE:
      {
         int thrptr;
#if defined(VGP_x86_solaris)
         thrptr = tst->os_state.thrptr;
#elif defined(VGP_amd64_solaris)
         thrptr = tst->arch.vex.guest_FS_CONST;
#else
#error "Unknown platform"
#endif

         if (thrptr == 0) {
            SET_STATUS_Failure(VKI_EINVAL);
            return;
         }

#if defined(VGP_x86_solaris)
         if (tst->arch.vex.guest_GS != supported_sel) {
            SET_STATUS_Failure(VKI_EINVAL);
            return;
         }
#elif defined(VGP_amd64_solaris)
         /* Valgrind on amd64 does not allow to change the gs register so
            a check that guest_GS is equal to supported_sel is not needed
            here. */
#else
#error "Unknown platform"
#endif

         PRE_MEM_WRITE("lwp_private(base)", ARG3, sizeof(Addr));
         if (!ML_(safe_to_deref((void*)ARG3, sizeof(Addr)))) {
            SET_STATUS_Failure(VKI_EFAULT);
            return;
         }
         *(Addr*)ARG3 = thrptr;
         POST_MEM_WRITE((Addr)ARG3, sizeof(Addr));
         SET_STATUS_Success(0);
         break;
      }
   default:
      VG_(unimplemented)("Syswrap of the lwp_private call where cmd=%ld.",
                         SARG1);
      /*NOTREACHED*/
      break;
   }
}

PRE(sys_lwp_wait)
{
   /* int lwp_wait(id_t lwpid, id_t *departed); */
   *flags |= SfMayBlock;
   PRINT("sys_lwp_wait ( %ld, %#lx )", SARG1, ARG2);
   PRE_REG_READ2(long, "lwp_wait", vki_id_t, lwpid, vki_id_t *, departed);
   if (ARG2)
      PRE_MEM_WRITE("lwp_wait(departed)", ARG2, sizeof(vki_id_t));
}

POST(sys_lwp_wait)
{
   POST_MEM_WRITE(ARG2, sizeof(vki_id_t));
}

PRE(sys_lwp_mutex_wakeup)
{
   /* int lwp_mutex_wakeup(lwp_mutex_t *lp, int release_all); */
   *flags |= SfMayBlock;
   PRINT("sys_lwp_mutex_wakeup ( %#lx, %ld )", ARG1, SARG2);
   PRE_REG_READ2(long, "lwp_mutex_wakeup", vki_lwp_mutex_t *, lp,
                 int, release_all);
   vki_lwp_mutex_t *lp = (vki_lwp_mutex_t *) ARG1;
   PRE_FIELD_READ("lwp_mutex_wakeup(lp->mutex_type)", lp->vki_mutex_type);
   PRE_FIELD_WRITE("lwp_mutex_wakeup(lp->mutex_waiters)",
                   lp->vki_mutex_waiters);
}

POST(sys_lwp_mutex_wakeup)
{
   vki_lwp_mutex_t *lp = (vki_lwp_mutex_t *) ARG1;
   POST_FIELD_WRITE(lp->vki_mutex_waiters);
}

PRE(sys_lwp_cond_wait)
{
   /* int lwp_cond_wait(lwp_cond_t *cvp, lwp_mutex_t *mp, timespec_t *tsp,
                        int check_park); */
   *flags |= SfMayBlock;
   PRINT("sys_lwp_cond_wait( %#lx, %#lx, %#lx, %ld )", ARG1, ARG2, ARG3, SARG4);
   PRE_REG_READ4(long, "lwp_cond_wait", vki_lwp_cond_t *, cvp,
                 vki_lwp_mutex_t *, mp, vki_timespec_t *, tsp, int, check_part);

   vki_lwp_cond_t *cvp = (vki_lwp_cond_t *) ARG1;
   vki_lwp_mutex_t *mp = (vki_lwp_mutex_t *) ARG2;
   PRE_FIELD_READ("lwp_cond_wait(cvp->type)", cvp->vki_cond_type);
   PRE_FIELD_READ("lwp_cond_wait(cvp->waiters_kernel)",
                  cvp->vki_cond_waiters_kernel);
   PRE_FIELD_READ("lwp_cond_wait(mp->mutex_type)", mp->vki_mutex_type);
   PRE_FIELD_WRITE("lwp_cond_wait(mp->mutex_waiters)", mp->vki_mutex_waiters);
   if (ARG3 != 0)
      PRE_MEM_READ("lwp_cond_wait(tsp)", ARG3, sizeof(vki_timespec_t));
}

POST(sys_lwp_cond_wait)
{
   vki_lwp_cond_t *cvp = (vki_lwp_cond_t *) ARG1;
   vki_lwp_mutex_t *mp = (vki_lwp_mutex_t *) ARG2;
   POST_FIELD_WRITE(cvp->vki_cond_waiters_kernel);
   POST_FIELD_WRITE(mp->vki_mutex_waiters);
   if (ARG3 != 0)
      POST_MEM_WRITE(ARG3, sizeof(vki_timespec_t));
}

PRE(sys_lwp_cond_signal)
{
   /* int lwp_cond_signal(lwp_cond_t *cvp); */
   *flags |= SfMayBlock;
   PRINT("sys_lwp_cond_signal( %#lx )", ARG1);
   PRE_REG_READ1(long, "lwp_cond_signal", vki_lwp_cond_t *, cvp);

   vki_lwp_cond_t *cvp = (vki_lwp_cond_t *) ARG1;
   PRE_FIELD_READ("lwp_cond_signal(cvp->type)", cvp->vki_cond_type);
   PRE_FIELD_READ("lwp_cond_signal(cvp->waiters_kernel)",
                  cvp->vki_cond_waiters_kernel);
}

POST(sys_lwp_cond_signal)
{
   vki_lwp_cond_t *cvp = (vki_lwp_cond_t *) ARG1;
   POST_FIELD_WRITE(cvp->vki_cond_waiters_kernel);
}

PRE(sys_lwp_cond_broadcast)
{
   /* int lwp_cond_broadcast(lwp_cond_t *cvp); */
   *flags |= SfMayBlock;
   PRINT("sys_lwp_cond_broadcast ( %#lx )", ARG1);
   PRE_REG_READ1(long, "lwp_cond_broadcast", vki_lwp_cond_t *, cvp);

   vki_lwp_cond_t *cvp = (vki_lwp_cond_t *) ARG1;
   PRE_FIELD_READ("lwp_cond_broadcast(cvp->type)", cvp->vki_cond_type);
   PRE_FIELD_READ("lwp_cond_broadcast(cvp->waiters_kernel)",
                  cvp->vki_cond_waiters_kernel);
   /*PRE_FIELD_WRITE("lwp_cond_broadcast(cvp->waiters_kernel)",
                     cvp->vki_cond_waiters_kernel);*/
}

POST(sys_lwp_cond_broadcast)
{
   vki_lwp_cond_t *cvp = (vki_lwp_cond_t *) ARG1;
   POST_FIELD_WRITE(cvp->vki_cond_waiters_kernel);
}

PRE(sys_pread)
{
   /* ssize_t pread(int fildes, void *buf, size_t nbyte, off_t offset); */
   *flags |= SfMayBlock;
   PRINT("sys_pread ( %ld, %#lx, %lu, %ld )", SARG1, ARG2, ARG3, SARG4);
   PRE_REG_READ4(long, "pread", int, fildes, void *, buf,
                 vki_size_t, nbyte, vki_off_t, offset);
   PRE_MEM_WRITE("pread(buf)", ARG2, ARG3);

   /* Be strict. */
   if (!ML_(fd_allowed)(ARG1, "pread", tid, False))
      SET_STATUS_Failure(VKI_EBADF);
}

POST(sys_pread)
{
   POST_MEM_WRITE(ARG2, RES);
}

PRE(sys_pwrite)
{
   /* ssize_t pwrite(int fildes, const void *buf, size_t nbyte,
                     off_t offset); */
   *flags |= SfMayBlock;
   PRINT("sys_pwrite ( %ld, %#lx, %lu, %ld )", SARG1, ARG2, ARG3, SARG4);
   PRE_REG_READ4(long, "pwrite", int, fildes, const void *, buf,
                 vki_size_t, nbyte, vki_off_t, offset);
   PRE_MEM_READ("pwrite(buf)", ARG2, ARG3);

   /* Be strict. */
   if (!ML_(fd_allowed)(ARG1, "pwrite", tid, False))
      SET_STATUS_Failure(VKI_EBADF);
}

PRE(sys_getpagesizes)
{
   /* int getpagesizes(int legacy, size_t *buf, int nelem); */
   PRINT("sys_getpagesizes ( %ld, %#lx, %ld )", SARG1, ARG2, SARG3);
   PRE_REG_READ3(long, "getpagesizes", int, legacy, size_t *, buf,
                 int, nelem);
   if (ARG2)
      PRE_MEM_WRITE("getpagesizes(buf)", ARG2, ARG3 * sizeof(vki_size_t));
}

POST(sys_getpagesizes)
{
   if (ARG2)
      POST_MEM_WRITE(ARG2, RES * sizeof(vki_size_t));
}

PRE(sys_lgrpsys)
{
   /* Kernel: int lgrpsys(int subcode, long ia, void *ap); */
   switch (ARG1 /*subcode*/) {
   case VKI_LGRP_SYS_MEMINFO:
      PRINT("sys_lgrpsys ( %ld, %ld, %#lx )", SARG1, SARG2, ARG3);
      PRE_REG_READ3(long, SC2("lgrpsys", "meminfo"), int, subcode,
                    int, addr_count, vki_meminfo_t *, minfo);
      PRE_MEM_READ("lgrpsys(minfo)", ARG3, sizeof(vki_meminfo_t));

      if (ML_(safe_to_deref)((vki_meminfo_t *) ARG3, sizeof(vki_meminfo_t))) {
         vki_meminfo_t *minfo = (vki_meminfo_t *) ARG3;
         PRE_MEM_READ("lgrpsys(minfo->mi_inaddr)",
                      (Addr) minfo->mi_inaddr, SARG2 * sizeof(vki_uint64_t));
         PRE_MEM_READ("lgrpsys(minfo->mi_info_req)", (Addr) minfo->mi_info_req,
                      minfo->mi_info_count * sizeof(vki_uint_t));
         PRE_MEM_WRITE("lgrpsys(minfo->mi_outdata)", (Addr) minfo->mi_outdata,
                       SARG2 * minfo->mi_info_count * sizeof(vki_uint64_t));
         PRE_MEM_WRITE("lgrpsys(minfo->mi_validity)",
                       (Addr) minfo->mi_validity, SARG2 * sizeof(vki_uint_t));
      }
      break;
   case VKI_LGRP_SYS_GENERATION:
      /* Liblgrp: lgrp_gen_t lgrp_generation(lgrp_view_t view); */
      PRINT("sys_lgrpsys ( %ld, %ld )", SARG1, SARG2);
      PRE_REG_READ2(long, SC2("lgrpsys", "generation"), int, subcode,
                    vki_lgrp_view_t, view);
      break;
   case VKI_LGRP_SYS_VERSION:
      /* Liblgrp: int lgrp_version(int version); */
      PRINT("sys_lgrpsys ( %ld, %ld )", SARG1, SARG2);
      PRE_REG_READ2(long, SC2("lgrpsys", "version"), int, subcode,
                    int, version);
      break;
   case VKI_LGRP_SYS_SNAPSHOT:
      /* Liblgrp: int lgrp_snapshot(void *buf, size_t bufsize); */
      PRINT("sys_lgrpsys ( %ld, %lu, %#lx )", SARG1, ARG2, ARG3);
      PRE_REG_READ3(long, SC2("lgrpsys", "snapshot"), int, subcode,
                    vki_size_t, bufsize, void *, buf);
      PRE_MEM_WRITE("lgrpsys(buf)", ARG3, ARG2);
      break;
   default:
      VG_(unimplemented)("Syswrap of the lgrpsys call with subcode %ld.",
                         SARG1);
      /*NOTREACHED*/
      break;
   }
}

POST(sys_lgrpsys)
{
   switch (ARG1 /*subcode*/) {
   case VKI_LGRP_SYS_MEMINFO:
      {
         vki_meminfo_t *minfo = (vki_meminfo_t *) ARG3;
         POST_MEM_WRITE((Addr) minfo->mi_outdata,
                        SARG2 * minfo->mi_info_count * sizeof(vki_uint64_t));
         POST_MEM_WRITE((Addr) minfo->mi_validity, SARG2 * sizeof(vki_uint_t));
      }
      break;
   case VKI_LGRP_SYS_GENERATION:
   case VKI_LGRP_SYS_VERSION:
      break;
   case VKI_LGRP_SYS_SNAPSHOT:
      POST_MEM_WRITE(ARG3, RES);
      break;
   default:
      vg_assert(0);
      break;
   }
}

PRE(sys_rusagesys)
{
   /* Kernel: int rusagesys(int code, void *arg1, void *arg2,
                            void *arg3, void *arg4); */
   switch (ARG1 /*code*/) {
   case VKI__RUSAGESYS_GETRUSAGE:
   case VKI__RUSAGESYS_GETRUSAGE_CHLD:
   case VKI__RUSAGESYS_GETRUSAGE_LWP:
      /* Libc: int getrusage(int who, struct rusage *r_usage); */
      PRINT("sys_rusagesys ( %ld, %#lx )", SARG1, ARG2);
      PRE_REG_READ2(long, SC2("rusagesys", "getrusage"), int, code,
                    struct vki_rusage *, r_usage);
      PRE_MEM_WRITE("rusagesys(r_usage)", ARG2, sizeof(struct vki_rusage));
      break;

   case VKI__RUSAGESYS_GETVMUSAGE:
      /* Libc: int getvmusage(uint_t flags, time_t age,
                              vmusage_t *buf, size_t *nres); */
      PRINT("sys_rusagesys ( %ld, %lu, %ld, %#lx, %#lx )",
            SARG1, ARG2, SARG3, ARG4, ARG5);
      PRE_REG_READ5(long, SC2("rusagesys", "getvmusage"), int, code,
                    vki_uint_t, flags, vki_time_t, age,
                    vki_vmusage_t *, buf, vki_size_t *, nres);
      PRE_MEM_READ("rusagesys(nres)", ARG5, sizeof(vki_size_t));
      /* PRE_MEM_WRITE("rusagesys(nres)", ARG5, sizeof(vki_size_t)); */

      if (ML_(safe_to_deref)((void *) ARG5, sizeof(vki_size_t))) {
         vki_size_t *nres = (vki_size_t *) ARG5;
         PRE_MEM_WRITE("rusagesys(buf)", ARG4,
                       *nres * sizeof(vki_vmusage_t));
      }
      *flags |= SfMayBlock;
      break;

   default:
      VG_(unimplemented)("Syswrap of the rusagesys call with code %ld.", SARG1);
      /*NOTREACHED*/
      break;
   }
}

POST(sys_rusagesys)
{
   switch (ARG1 /*code*/) {
   case VKI__RUSAGESYS_GETRUSAGE:
   case VKI__RUSAGESYS_GETRUSAGE_CHLD:
   case VKI__RUSAGESYS_GETRUSAGE_LWP:
      POST_MEM_WRITE(ARG2, sizeof(struct vki_rusage));
      break;
   case VKI__RUSAGESYS_GETVMUSAGE:
      {
         vki_size_t *nres = (vki_size_t *) ARG5;
         POST_MEM_WRITE(ARG5, sizeof(vki_size_t));
         POST_MEM_WRITE(ARG4, *nres * sizeof(vki_vmusage_t));
      }
      break;
   default:
      vg_assert(0);
      break;
   }
}

PRE(sys_port)
{
   /* Kernel: int64_t portfs(int opcode, uintptr_t a0, uintptr_t a1,
                             uintptr_t a2, uintptr_t a3, uintptr_t a4); */
   Int opcode = ARG1 & VKI_PORT_CODE_MASK;
   *flags |= SfMayBlock;
   switch (opcode) {
   case VKI_PORT_CREATE:
      PRINT("sys_port ( %ld )", SARG1);
      PRE_REG_READ1(long, SC2("port", "create"), int, opcode);
      break;
   case VKI_PORT_ASSOCIATE:
   case VKI_PORT_DISSOCIATE:
      PRINT("sys_port ( %ld, %ld, %ld, %#lx, %ld, %#lx )", SARG1, SARG2, SARG3,
            ARG4, SARG5, ARG6);
      if (opcode == VKI_PORT_ASSOCIATE) {
         PRE_REG_READ6(long, SC2("port", "associate"), int, opcode, int, a0,
                       int, a1, uintptr_t, a2, int, a3, void *, a4);
      }
      else {
         PRE_REG_READ6(long, SC2("port", "dissociate"), int, opcode, int, a0,
                       int, a1, uintptr_t, a2, int, a3, void *, a4);
      }

      switch (ARG3 /*source*/) {
      case VKI_PORT_SOURCE_FD:
         if (!ML_(fd_allowed)(ARG4, "port", tid, False)) {
            SET_STATUS_Failure(VKI_EBADF);
         }
         break;
      case VKI_PORT_SOURCE_FILE:
         {
            struct vki_file_obj *fo = (struct vki_file_obj *)ARG4;
            PRE_MEM_READ("port(file_obj)", ARG4, sizeof(struct vki_file_obj));
            if (ML_(safe_to_deref)(&fo->fo_name, sizeof(fo->fo_name)))
               PRE_MEM_RASCIIZ("port(file_obj->fo_name)", (Addr)fo->fo_name);
         }
         break;
      default:
         VG_(unimplemented)("Syswrap of the port_associate/dissociate call "
                            "type %ld.", SARG3);
         /*NOTREACHED*/
         break;
      }
      break;
   case VKI_PORT_SEND:
      PRINT("sys_port ( %ld, %ld, %ld, %#lx )", SARG1, SARG2, SARG3, ARG4);
      PRE_REG_READ4(long, SC2("port", "send"), int, opcode, int, a0, int, a1,
                    void *, a2);
      break;
   case VKI_PORT_SENDN:
      PRINT("sys_port ( %ld, %#lx, %#lx, %lu, %lx, %#lx)", SARG1, ARG2, ARG3,
            ARG4, ARG5, ARG6);
      PRE_REG_READ6(long, SC2("port", "sendn"), int, opcode, int *, a0,
                    int *, a1, vki_uint_t, a2, int, a3, void *, a4);
      PRE_MEM_READ("port(ports)", ARG2, ARG4 * sizeof(int));
      PRE_MEM_WRITE("port(errors)", ARG3, ARG4 * sizeof(int));
      break;
   case VKI_PORT_GET:
      PRINT("sys_port ( %ld, %ld, %#lx, %ld, %ld, %#lx )", SARG1, SARG2, ARG3,
            SARG4, SARG5, ARG6);
      PRE_REG_READ6(long, SC2("port", "get"), int, opcode, int, a0,
                    port_event_t *, a1, vki_time_t, a2, long, a3,
                    timespec_t *, a4);
      PRE_MEM_WRITE("port(uevp)", ARG3, sizeof(vki_port_event_t));
      break;
   case VKI_PORT_GETN:
      PRINT("sys_port ( %ld, %ld, %#lx, %lu, %lu, %#lx )", SARG1, SARG2, ARG3,
            ARG4, ARG5, ARG6);
      PRE_REG_READ6(long, SC2("port", "getn"), int, opcode, int, a0,
                    port_event_t *, a1, vki_uint_t, a2, vki_uint_t, a3,
                    timespec_t *, a4);
      if (ARG6)
         PRE_MEM_READ("port(timeout)", ARG6, sizeof(vki_timespec_t));
      PRE_MEM_WRITE("port(uevp)", ARG3, ARG4 * sizeof(vki_port_event_t));
      break;
   case VKI_PORT_ALERT:
      PRINT("sys_port ( %ld, %ld, %ld, %ld, %#lx )", SARG1, SARG2, SARG3, SARG4,
            ARG5);
      PRE_REG_READ5(long, SC2("port", "alert"), int, opcode, int, a0, int, a1,
                    int, a2, void *, a3);
      break;
   case VKI_PORT_DISPATCH:
      // FIXME: check order: SARG2, SARG1  or   SARG1, SARG2  ??
      PRINT("sys_port ( %ld, %ld, %ld, %ld, %#lx, %#lx )", SARG2, SARG1, SARG3,
            SARG4, ARG5, ARG6);
      PRE_REG_READ6(long, SC2("port", "dispatch"), int, opcode, int, a0,
                    int, a1, int, a2, uintptr_t, a3, void *, a4);
      break;
   default:
      VG_(unimplemented)("Syswrap of the port call with opcode %ld.", SARG1);
      /*NOTREACHED*/
      break;
   }

   /* Be strict. */
   if ((opcode != VKI_PORT_CREATE && opcode != VKI_PORT_SENDN) &&
       !ML_(fd_allowed)(ARG2, "port", tid, False))
      SET_STATUS_Failure(VKI_EBADF);
}

POST(sys_port)
{
   Int opcode = ARG1 & VKI_PORT_CODE_MASK;
   switch (opcode) {
   case VKI_PORT_CREATE:
      if (!ML_(fd_allowed)(RES, "port", tid, True)) {
         VG_(close)(RES);
         SET_STATUS_Failure(VKI_EMFILE);
      }
      else if (VG_(clo_track_fds))
         ML_(record_fd_open_named)(tid, RES);
      break;
   case VKI_PORT_ASSOCIATE:
   case VKI_PORT_DISSOCIATE:
   case VKI_PORT_SEND:
      break;
   case VKI_PORT_SENDN:
      if (RES != ARG4) {
         /* If there is any error then the whole errors area is written. */
         POST_MEM_WRITE(ARG3, ARG4 * sizeof(int));
      }
      break;
   case VKI_PORT_GET:
      POST_MEM_WRITE(ARG3, sizeof(vki_port_event_t));
      break;
   case VKI_PORT_GETN:
      POST_MEM_WRITE(ARG3, RES * sizeof(vki_port_event_t));
      break;
   case VKI_PORT_ALERT:
   case VKI_PORT_DISPATCH:
      break;
   default:
      VG_(unimplemented)("Syswrap of the port call with opcode %lu.", ARG1);
      /*NOTREACHED*/
      break;
   }
}

PRE(sys_pollsys)
{
   /* int pollsys(pollfd_t *fds, nfds_t nfds, timespec_t *timeout,
                  sigset_t *set); */
   UWord i;
   struct vki_pollfd *ufds = (struct vki_pollfd *)ARG1;

   *flags |= SfMayBlock | SfPostOnFail;

   PRINT("sys_pollsys ( %#lx, %lu, %#lx, %#lx )", ARG1, ARG2, ARG3, ARG4);
   PRE_REG_READ4(long, "poll", pollfd_t *, fds, vki_nfds_t, nfds,
                 timespec_t *, timeout, sigset_t *, set);

   for (i = 0; i < ARG2; i++) {
      vki_pollfd_t *u = &ufds[i];
      PRE_FIELD_READ("poll(ufds.fd)", u->fd);
      if (ML_(safe_to_deref)(&ufds[i].fd, sizeof(ufds[i].fd)) && ufds[i].fd >= 0) {
         PRE_FIELD_READ("poll(ufds.events)", u->events);
      }
      PRE_FIELD_WRITE("poll(ufds.revents)", u->revents);
   }

   if (ARG3)
      PRE_MEM_READ("poll(timeout)", ARG3, sizeof(vki_timespec_t));

   if (ARG4) {
      PRE_MEM_READ("poll(set)", ARG4, sizeof(vki_sigset_t));

      const vki_sigset_t *guest_sigmask = (vki_sigset_t *) ARG4;
      if (!ML_(safe_to_deref)(guest_sigmask, sizeof(vki_sigset_t))) {
         ARG4 = 1; /* Something recognisable to POST() hook. */
      } else {
         vki_sigset_t *vg_sigmask =
            VG_(malloc)("syswrap.pollsys.1", sizeof(vki_sigset_t));
         ARG4 = (Addr) vg_sigmask;
         *vg_sigmask = *guest_sigmask;
         VG_(sanitize_client_sigmask)(vg_sigmask);
      }
   }
}

POST(sys_pollsys)
{
   vg_assert(SUCCESS || FAILURE);

   if (SUCCESS && (RES >= 0)) {
      UWord i;
      vki_pollfd_t *ufds = (vki_pollfd_t*)ARG1;
      for (i = 0; i < ARG2; i++)
         POST_FIELD_WRITE(ufds[i].revents);
   }

   if ((ARG4 != 0) && (ARG4 != 1)) {
      VG_(free)((vki_sigset_t *) ARG4);
   }
}

PRE(sys_labelsys)
{
   /* Kernel: int labelsys(int op, void *a1, void *a2, void *a3,
                           void *a4, void *a5); */

   switch (ARG1 /*op*/) {
   case VKI_TSOL_SYSLABELING:
      /* Libc: int is_system_labeled(void); */
      PRINT("sys_labelsys ( %ld )", SARG1);
      PRE_REG_READ1(long, SC2("labelsys", "syslabeling"), int, op);
      break;

   case VKI_TSOL_TNRH:
      /* Libtsnet: int tnrh(int cmd, tsol_rhent_t *buf); */
      PRINT("sys_labelsys ( %ld, %ld, %#lx )", SARG1, SARG2, ARG3);
      PRE_REG_READ3(long, SC2("labelsys", "tnrh"), int, op, int, cmd,
                    vki_tsol_rhent_t *, buf);
      if (ARG2 != VKI_TNDB_FLUSH)
         PRE_MEM_READ("labelsys(buf)", ARG3, sizeof(vki_tsol_rhent_t));
      break;

   case VKI_TSOL_TNRHTP:
      /* Libtsnet: int tnrhtp(int cmd, tsol_tpent_t *buf); */
      PRINT("sys_labelsys ( %ld, %ld, %#lx )", SARG1, SARG2, ARG3);
      PRE_REG_READ3(long, SC2("labelsys", "tnrhtp"), int, op, int, cmd,
                    vki_tsol_tpent_t *, buf);
      if (ARG2 != VKI_TNDB_FLUSH)
         PRE_MEM_READ("labelsys(buf)", ARG3, sizeof(vki_tsol_tpent_t));
      break;

   case VKI_TSOL_TNMLP:
      /* Libtsnet: int tnmlp(int cmd, tsol_mlpent_t *buf); */
      PRINT("sys_labelsys ( %ld, %ld, %#lx )", SARG1, SARG2, ARG3);
      PRE_REG_READ3(long, SC2("labelsys", "tnmlp"), int, op, int, cmd,
                    vki_tsol_mlpent_t *, buf);
      PRE_MEM_READ("labelsys(buf)", ARG3, sizeof(vki_tsol_mlpent_t));
      break;

   case VKI_TSOL_GETLABEL:
      /* Libtsol: int getlabel(const char *path, bslabel_t *label); */
      PRINT("sys_labelsys ( %ld, %#lx(%s), %#lx )",
            SARG1, ARG2, (HChar *) ARG2, ARG3);
      PRE_REG_READ3(long, SC2("labelsys", "getlabel"), int, op,
                    const char *, path, vki_bslabel_t *, label);
      PRE_MEM_RASCIIZ("labelsys(path)", ARG2);
      PRE_MEM_WRITE("labelsys(label)", ARG3, sizeof(vki_bslabel_t));
      break;

   case VKI_TSOL_FGETLABEL:
      /* Libtsol: int fgetlabel(int fd, bslabel_t *label); */
      PRINT("sys_labelsys ( %ld, %ld, %#lx )", SARG1, SARG2, ARG3);
      PRE_REG_READ3(long, SC2("labelsys", "fgetlabel"), int, op,
                    int, fd, vki_bslabel_t *, label);
      /* Be strict. */
      if (!ML_(fd_allowed)(ARG2, "labelsys(fgetlabel)", tid, False))
         SET_STATUS_Failure(VKI_EBADF);
      PRE_MEM_WRITE("labelsys(label)", ARG3, sizeof(vki_bslabel_t));
      break;

#if defined(SOLARIS_TSOL_CLEARANCE)
   case VKI_TSOL_GETCLEARANCE:
      /* Libtsol: int getclearance(bslabel_t *clearance); */
      PRINT("sys_labelsys ( %ld, %#lx )", SARG1, ARG2);
      PRE_REG_READ2(long, SC2("labelsys", "getclearance"), int, op,
                    vki_bslabel_t *, clearance);
      PRE_MEM_WRITE("labelsys(clearance)", ARG2, sizeof(vki_bslabel_t));
      break;

   case VKI_TSOL_SETCLEARANCE:
      /* Libtsol: int setclearance(bslabel_t *clearance); */
      PRINT("sys_labelsys ( %ld, %#lx )", SARG1, ARG2);
      PRE_REG_READ2(long, SC2("labelsys", "setclearance"), int, op,
                    vki_bslabel_t *, clearance);
      PRE_MEM_READ("labelsys(clearance)", ARG2, sizeof(vki_bslabel_t));
      break;
#endif /* SOLARIS_TSOL_CLEARANCE */

   default:
      VG_(unimplemented)("Syswrap of the labelsys call with op %ld.", SARG1);
      /*NOTREACHED*/
      break;
   }
}

POST(sys_labelsys)
{
   switch (ARG1 /*op*/) {
   case VKI_TSOL_SYSLABELING:
      break;

   case VKI_TSOL_TNRH:
      switch (ARG2 /*cmd*/) {
      case VKI_TNDB_LOAD:
      case VKI_TNDB_DELETE:
      case VKI_TNDB_FLUSH:
         break;
#if defined(SOLARIS_TNDB_GET_TNIP)
      case TNDB_GET_TNIP:
#endif /* SOLARIS_TNDB_GET_TNIP */
      case VKI_TNDB_GET:
         POST_MEM_WRITE(ARG3, sizeof(vki_tsol_rhent_t));
         break;
      default:
         vg_assert(0);
         break;
      }
      break;

   case VKI_TSOL_TNRHTP:
      switch (ARG2 /*cmd*/) {
      case VKI_TNDB_LOAD:
      case VKI_TNDB_DELETE:
      case VKI_TNDB_FLUSH:
         break;
      case VKI_TNDB_GET:
         POST_MEM_WRITE(ARG3, sizeof(vki_tsol_tpent_t));
         break;
      default:
         vg_assert(0);
         break;
      }
      break;

   case VKI_TSOL_TNMLP:
      switch (ARG2 /*cmd*/) {
      case VKI_TNDB_LOAD:
      case VKI_TNDB_DELETE:
      case VKI_TNDB_FLUSH:
         break;
      case VKI_TNDB_GET:
         POST_MEM_WRITE(ARG3, sizeof(vki_tsol_mlpent_t));
         break;
      default:
         vg_assert(0);
         break;
      }
      break;

   case VKI_TSOL_GETLABEL:
   case VKI_TSOL_FGETLABEL:
      POST_MEM_WRITE(ARG3, sizeof(vki_bslabel_t));
      break;

#if defined(SOLARIS_TSOL_CLEARANCE)
   case VKI_TSOL_GETCLEARANCE:
      POST_MEM_WRITE(ARG2, sizeof(vki_bslabel_t));
      break;

   case VKI_TSOL_SETCLEARANCE:
      break;
#endif /* SOLARIS_TSOL_CLEARANCE */

   default:
      vg_assert(0);
      break;
   }
}

PRE(sys_acl)
{
   /* int acl(char *pathp, int cmd, int nentries, void *aclbufp); */
   PRINT("sys_acl ( %#lx(%s), %ld, %ld, %#lx )", ARG1, (HChar *) ARG1, SARG2,
         SARG3, ARG4);

   PRE_REG_READ4(long, "acl", char *, pathp, int, cmd,
                 int, nentries, void *, aclbufp);
   PRE_MEM_RASCIIZ("acl(pathp)", ARG1);

   switch (ARG2 /*cmd*/) {
   case VKI_SETACL:
      if (ARG4)
         PRE_MEM_READ("acl(aclbufp)", ARG4, ARG3 * sizeof(vki_aclent_t));
      break;
   case VKI_GETACL:
      PRE_MEM_WRITE("acl(aclbufp)", ARG4, ARG3 * sizeof(vki_aclent_t));
      break;
   case VKI_GETACLCNT:
      break;
   case VKI_ACE_SETACL:
      if (ARG4)
         PRE_MEM_READ("acl(aclbufp)", ARG4, ARG3 * sizeof(vki_ace_t));
      break;
   case VKI_ACE_GETACL:
      PRE_MEM_WRITE("acl(aclbufp)", ARG4, ARG3 * sizeof(vki_ace_t));
      break;
   case VKI_ACE_GETACLCNT:
      break;
   default:
      VG_(unimplemented)("Syswrap of the acl call with cmd %ld.", SARG2);
      /*NOTREACHED*/
      break;
   }
}

POST(sys_acl)
{
   switch (ARG2 /*cmd*/) {
   case VKI_SETACL:
      break;
   case VKI_GETACL:
      POST_MEM_WRITE(ARG4, ARG3 * sizeof(vki_aclent_t));
      break;
   case VKI_GETACLCNT:
      break;
   case VKI_ACE_SETACL:
      break;
   case VKI_ACE_GETACL:
      POST_MEM_WRITE(ARG4, ARG3 * sizeof(vki_ace_t));
      break;
   case VKI_ACE_GETACLCNT:
      break;
   default:
      vg_assert(0);
      break;
   }
}

PRE(sys_auditsys)
{
   /* Kernel: int auditsys(long code, long a1, long a2, long a3, long a4); */
   switch (ARG1 /*code*/) {
   case VKI_BSM_GETAUID:
      /* Libbsm: int getauid(au_id_t *auid); */
      PRINT("sys_auditsys ( %ld, %#lx )", SARG1, ARG2);
      PRE_REG_READ2(long, SC2("auditsys", "getauid"), long, code,
                    vki_au_id_t *, auid);
      PRE_MEM_WRITE("auditsys(auid)", ARG2, sizeof(vki_au_id_t));
      break;
   case VKI_BSM_SETAUID:
      /* Libbsm: int setauid(au_id_t *auid); */
      PRINT("sys_auditsys ( %ld, %#lx )", SARG1, ARG2);
      PRE_REG_READ2(long, SC2("auditsys", "setauid"), long, code,
                    vki_au_id_t *, auid);
      PRE_MEM_READ("auditsys(auid)", ARG2, sizeof(vki_au_id_t));
      break;
   case VKI_BSM_GETAUDIT:
      /* Libbsm: int getaudit(auditinfo_t *ai); */
      PRINT("sys_auditsys ( %ld, %#lx )", SARG1, ARG2);
      PRE_REG_READ2(long, SC2("auditsys", "getaudit"), long, code,
                    vki_auditinfo_t *, ai);
      PRE_MEM_WRITE("auditsys(ai)", ARG2, sizeof(vki_auditinfo_t));
      break;
   case VKI_BSM_SETAUDIT:
      /* Libbsm: int setaudit(auditinfo_t *ai); */
      PRINT("sys_auditsys ( %ld, %#lx )", SARG1, ARG2);
      PRE_REG_READ2(long, SC2("auditsys", "setaudit"), long, code,
                    vki_auditinfo_t *, ai);
      PRE_MEM_READ("auditsys(ai)", ARG2, sizeof(vki_auditinfo_t));
      break;
   case VKI_BSM_AUDIT:
      /* Libbsm: int audit(void *record, int length); */
      PRINT("sys_auditsys ( %ld, %#lx, %ld )", SARG1, ARG2, SARG3);
      PRE_REG_READ3(long, SC2("auditsys", "audit"), long, code,
                    void *, record, int, length);
      PRE_MEM_READ("auditsys(record)", ARG2, ARG3);
      break;
   case VKI_BSM_AUDITCTL:
      /* Libbsm: int auditon(int cmd, caddr_t data, int length); */
      PRINT("sys_auditsys ( %ld, %ld, %#lx, %ld )",
            SARG1, SARG2, ARG3, SARG4);

      switch (ARG2 /*cmd*/) {
      case VKI_A_GETPOLICY:
         PRE_REG_READ3(long, SC3("auditsys", "auditctl", "getpolicy"),
                       long, code, int, cmd, vki_uint32_t *, policy);
         PRE_MEM_WRITE("auditsys(policy)", ARG3, sizeof(vki_uint32_t));
         break;
      case VKI_A_SETPOLICY:
         PRE_REG_READ3(long, SC3("auditsys", "auditctl", "setpolicy"),
                       long, code, int, cmd, vki_uint32_t *, policy);
         PRE_MEM_READ("auditsys(policy)", ARG3, sizeof(vki_uint32_t));
         break;
      case VKI_A_GETKMASK:
         PRE_REG_READ3(long, SC3("auditsys", "auditctl", "getkmask"),
                       long, code, int, cmd, vki_au_mask_t *, kmask);
         PRE_MEM_WRITE("auditsys(kmask)", ARG3, sizeof(vki_au_mask_t));
         break;
      case VKI_A_SETKMASK:
         PRE_REG_READ3(long, SC3("auditsys", "auditctl", "setkmask"),
                       long, code, int, cmd, vki_au_mask_t *, kmask);
         PRE_MEM_READ("auditsys(kmask)", ARG3, sizeof(vki_au_mask_t));
         break;
      case VKI_A_GETQCTRL:
         PRE_REG_READ3(long, SC3("auditsys", "auditctl", "getqctrl"),
                       long, code, int, cmd,
                       struct vki_au_qctrl *, qctrl);
         PRE_MEM_WRITE("auditsys(qctrl)", ARG3,
                       sizeof(struct vki_au_qctrl));
         break;
      case VKI_A_SETQCTRL:
         PRE_REG_READ3(long, SC3("auditsys", "auditctl", "setqctrl"),
                       long, code, int, cmd,
                       struct vki_au_qctrl *, qctrl);
         PRE_MEM_READ("auditsys(qctrl)", ARG3,
                      sizeof(struct vki_au_qctrl));
         break;
      case VKI_A_GETCWD:
         PRE_REG_READ4(long, SC3("auditsys", "auditctl", "getcwd"),
                       long, code, int, cmd, char *, data, int, length);
         PRE_MEM_WRITE("auditsys(data)", ARG3, ARG4);
         break;
      case VKI_A_GETCAR:
         PRE_REG_READ4(long, SC3("auditsys", "auditctl", "getcar"),
                       long, code, int, cmd, char *, data, int, length);
         PRE_MEM_WRITE("auditsys(data)", ARG3, ARG4);
         break;
#if defined(SOLARIS_AUDITON_STAT)
      case VKI_A_GETSTAT:
         PRE_REG_READ3(long, SC3("auditsys", "auditctl", "getstat"),
                       long, code, int, cmd, vki_au_stat_t *, stats);
         PRE_MEM_WRITE("auditsys(stats)", ARG3, sizeof(vki_au_stat_t));
         break;
      case VKI_A_SETSTAT:
         PRE_REG_READ3(long, SC3("auditsys", "auditctl", "setstat"),
                       long, code, int, cmd, vki_au_stat_t *, stats);
         PRE_MEM_READ("auditsys(stats)", ARG3, sizeof(vki_au_stat_t));
         break;
#endif /* SOLARIS_AUDITON_STAT */
      case VKI_A_SETUMASK:
         PRE_REG_READ3(long, SC3("auditsys", "auditctl", "setumask"),
                       long, code, int, cmd, vki_auditinfo_t *, umask);
         PRE_MEM_READ("auditsys(umask)", ARG3, sizeof(vki_auditinfo_t));
         break;
      case VKI_A_SETSMASK:
         PRE_REG_READ3(long, SC3("auditsys", "auditctl", "setsmask"),
                       long, code, int, cmd, vki_auditinfo_t *, smask);
         PRE_MEM_READ("auditsys(smask)", ARG3, sizeof(vki_auditinfo_t));
         break;
      case VKI_A_GETCOND:
         PRE_REG_READ3(long, SC3("auditsys", "auditctl", "getcond"),
                       long, code, int, cmd, int *, cond);
         PRE_MEM_WRITE("auditsys(cond)", ARG3, sizeof(int));
         break;
      case VKI_A_SETCOND:
         PRE_REG_READ3(long, SC3("auditsys", "auditctl", "setcond"),
                       long, code, int, cmd, int *, state);
         PRE_MEM_READ("auditsys(cond)", ARG3, sizeof(int));
         break;
      case VKI_A_GETCLASS:
         PRE_REG_READ3(long, SC3("auditsys", "auditctl", "getclass"),
                       long, code, int, cmd,
                       vki_au_evclass_map_t *, classmap);

         if (ML_(safe_to_deref((void *) ARG3,
                               sizeof(vki_au_evclass_map_t)))) {
            vki_au_evclass_map_t *classmap =
               (vki_au_evclass_map_t *) ARG3;
            PRE_FIELD_READ("auditsys(classmap.ec_number)",
                           classmap->ec_number);
            PRE_MEM_WRITE("auditsys(classmap)", ARG3,
                          sizeof(vki_au_evclass_map_t));
         }
         break;
      case VKI_A_SETCLASS:
         PRE_REG_READ3(long, SC3("auditsys", "auditctl", "setclass"),
                       long, code, int, cmd,
                       vki_au_evclass_map_t *, classmap);

         if (ML_(safe_to_deref((void *) ARG3, 
                               sizeof(vki_au_evclass_map_t)))) {
            vki_au_evclass_map_t *classmap =
               (vki_au_evclass_map_t *) ARG3;
            PRE_FIELD_READ("auditsys(classmap.ec_number)", 
                           classmap->ec_number);  
            PRE_FIELD_READ("auditsys(classmap.ec_class)", 
                           classmap->ec_class);
         }
         break;
      case VKI_A_GETPINFO:
         PRE_REG_READ3(long, SC3("auditsys", "auditctl", "getpinfo"),
                       long, code, int, cmd,
                       struct vki_auditpinfo *, apinfo);

         if (ML_(safe_to_deref((void *) ARG3,
                               sizeof(struct vki_auditpinfo)))) {
            struct vki_auditpinfo *apinfo =
               (struct vki_auditpinfo *) ARG3;
            PRE_FIELD_READ("auditsys(apinfo.ap_pid)", apinfo->ap_pid);
            PRE_MEM_WRITE("auditsys(apinfo)", ARG3,
                          sizeof(struct vki_auditpinfo));
         }
         break;
      case VKI_A_SETPMASK:
         PRE_REG_READ3(long, SC3("auditsys", "auditctl", "setpmask"),
                       long, code, int, cmd,
                       struct vki_auditpinfo *, apinfo);
         PRE_MEM_WRITE("auditsys(apinfo)", ARG3,
                       sizeof(struct vki_auditpinfo));
         break;
      case VKI_A_GETPINFO_ADDR:
         PRE_REG_READ4(long, SC3("auditsys", "auditctl", "getpinfo_addr"),
                       long, code, int, cmd,
                       struct vki_auditpinfo_addr *, apinfo, int, length);

         if (ML_(safe_to_deref((void *) ARG3,
                               sizeof(struct vki_auditpinfo_addr)))) {
            struct vki_auditpinfo_addr *apinfo_addr =
               (struct vki_auditpinfo_addr *) ARG3;
            PRE_FIELD_READ("auditsys(apinfo_addr.ap_pid)",
                           apinfo_addr->ap_pid);
            PRE_MEM_WRITE("auditsys(apinfo_addr)", ARG3, ARG4);
         }
         break;
      case VKI_A_GETKAUDIT:
         PRE_REG_READ4(long, SC3("auditsys", "auditctl", "getkaudit"),
                       long, code, int, cmd,
                       vki_auditinfo_addr_t *, kaudit, int, length);
         PRE_MEM_WRITE("auditsys(kaudit)", ARG3, ARG4);
         break;
      case VKI_A_SETKAUDIT:
         PRE_REG_READ4(long, SC3("auditsys", "auditctl", "setkaudit"),
                       long, code, int, cmd,
                       vki_auditinfo_addr_t *, kaudit, int, length);
         PRE_MEM_READ("auditsys(kaudit)", ARG3, ARG4);
         break;
      case VKI_A_GETAMASK:
         PRE_REG_READ3(long, SC3("auditsys", "auditctl", "getamask"),
                       long, code, int, cmd, vki_au_mask_t *, amask);
         PRE_MEM_WRITE("auditsys(amask)", ARG3, sizeof(vki_au_mask_t));
         break;
      case VKI_A_SETAMASK:
         PRE_REG_READ3(long, SC3("auditsys", "auditctl", "setamask"),
                       long, code, int, cmd, vki_au_mask_t *, amask);
         PRE_MEM_READ("auditsys(amask)", ARG3, sizeof(vki_au_mask_t));
         break;
      default:
         VG_(unimplemented)("Syswrap of the auditsys(auditctl) call "
                            "with cmd %lu.", ARG2);
         /*NOTREACHED*/
         break;
      }
      break;
   case VKI_BSM_GETAUDIT_ADDR:
      /* Libbsm: int getaudit_addr(auditinfo_addr_t *ai, int len); */
      PRINT("sys_auditsys ( %ld, %#lx, %ld )", SARG1, ARG2, SARG3);
      PRE_REG_READ3(long, SC2("auditsys", "getaudit_addr"), long, code,
                    vki_auditinfo_addr_t *, ai, int, len);
      PRE_MEM_WRITE("auditsys(ai)", ARG2, ARG3);
      break;
   case VKI_BSM_SETAUDIT_ADDR:
      /* Libbsm: int setaudit_addr(auditinfo_addr_t *ai, int len); */
      PRINT("sys_auditsys ( %ld, %#lx, %ld )", SARG1, ARG2, SARG3);
      PRE_REG_READ3(long, SC2("auditsys", "setaudit_addr"), long, code,
                    vki_auditinfo_addr_t *, ai, int, len);
      PRE_MEM_READ("auditsys(ai)", ARG2, ARG3);
      break;
   case VKI_BSM_AUDITDOOR:
      /* Libbsm: int auditdoor(int fd); */
      PRINT("sys_auditsys ( %ld, %ld )", SARG1, SARG2);
      PRE_REG_READ2(long, SC2("auditsys", "door"), long, code, int, fd);

      /* Be strict. */
      if (!ML_(fd_allowed)(ARG2, SC2("auditsys", "door")"(fd)",
                           tid, False))
         SET_STATUS_Failure(VKI_EBADF);
      break;
   default:
      VG_(unimplemented)("Syswrap of the auditsys call with code %lu.", ARG1);
      /*NOTREACHED*/
      break;
   }
}

POST(sys_auditsys)
{
   switch (ARG1 /*code*/) {
   case VKI_BSM_GETAUID:
      POST_MEM_WRITE(ARG2, sizeof(vki_au_id_t));
      break;
   case VKI_BSM_SETAUID:
      break;
   case VKI_BSM_GETAUDIT:
      POST_MEM_WRITE(ARG2, sizeof(vki_auditinfo_t));
      break;
   case VKI_BSM_SETAUDIT:
   case VKI_BSM_AUDIT:
      break;
   case VKI_BSM_AUDITCTL:
      switch (ARG2 /*cmd*/) {
         case VKI_A_GETPOLICY:
            POST_MEM_WRITE(ARG3, sizeof(vki_uint32_t));
            break;
         case VKI_A_SETPOLICY:
            break;
         case VKI_A_GETKMASK:
            POST_MEM_WRITE(ARG3, sizeof(vki_au_mask_t));
            break;
         case VKI_A_SETKMASK:
            break;
         case VKI_A_GETQCTRL:
            POST_MEM_WRITE(ARG3, sizeof(struct vki_au_qctrl));
            break;
         case VKI_A_SETQCTRL:
            break;
         case VKI_A_GETCWD:
         case VKI_A_GETCAR:
            POST_MEM_WRITE(ARG3, VG_(strlen)((HChar *) ARG3) + 1);
            break;
#if defined(SOLARIS_AUDITON_STAT)
         case VKI_A_GETSTAT:
            POST_MEM_WRITE(ARG3, sizeof(vki_au_stat_t));
            break;
         case VKI_A_SETSTAT:
#endif /* SOLARIS_AUDITON_STAT */
         case VKI_A_SETUMASK:
         case VKI_A_SETSMASK:
            break;
         case VKI_A_GETCOND:
            POST_MEM_WRITE(ARG3, sizeof(int));
            break;
         case VKI_A_SETCOND:
            break;
         case VKI_A_GETCLASS:
            POST_MEM_WRITE(ARG3, sizeof(vki_au_evclass_map_t));
            break;
         case VKI_A_SETCLASS:
            break;
         case VKI_A_GETPINFO:
            POST_MEM_WRITE(ARG3, sizeof(struct vki_auditpinfo));
            break;
         case VKI_A_SETPMASK:
            break;
         case VKI_A_GETPINFO_ADDR:
            POST_MEM_WRITE(ARG3, sizeof(struct auditpinfo_addr));
            break;
         case VKI_A_GETKAUDIT:
            POST_MEM_WRITE(ARG3, sizeof(vki_auditinfo_addr_t));
            break;
         case VKI_A_SETKAUDIT:
            break;
         case VKI_A_GETAMASK:
            POST_MEM_WRITE(ARG3, sizeof(vki_au_mask_t));
            break;
         case VKI_A_SETAMASK:
            break;
      }
      break;
   case VKI_BSM_GETAUDIT_ADDR:
      POST_MEM_WRITE(ARG2, sizeof(vki_auditinfo_addr_t));
      break;
   case VKI_BSM_SETAUDIT_ADDR:
      break;
   case VKI_BSM_AUDITDOOR:
      break;
   }
}

PRE(sys_p_online)
{
   /* int p_online(processorid_t processorid, int flag); */
   PRINT("sys_p_online ( %ld, %ld )", SARG1, SARG2);
   PRE_REG_READ2(long, "p_online", vki_processorid_t, processorid, int, flag);
}

PRE(sys_sigqueue)
{
   /* int sigqueue(pid_t pid, int signo, void *value,
                   int si_code, timespec_t *timeout);
    */
   PRINT("sys_sigqueue ( %ld, %ld, %#lx, %ld, %#lx )",
         SARG1, SARG2, ARG3, SARG4, ARG5);
   PRE_REG_READ5(long, "sigqueue", vki_pid_t, pid, int, signo,
                 void *, value, int, si_code,
                 vki_timespec_t *, timeout);

   if (ARG5)
      PRE_MEM_READ("sigqueue(timeout)", ARG5, sizeof(vki_timespec_t));

   if (!ML_(client_signal_OK)(ARG2)) {
      SET_STATUS_Failure(VKI_EINVAL);
      return;
   }

   /* If we're sending SIGKILL, check to see if the target is one of
      our threads and handle it specially. */
   if (ARG2 == VKI_SIGKILL && ML_(do_sigkill)(ARG1, -1)) {
      SET_STATUS_Success(0);
   } else {
      SysRes res = VG_(do_syscall5)(SYSNO, ARG1, ARG2, ARG3, ARG4,
                                    ARG5);
      SET_STATUS_from_SysRes(res);
   }

   if (VG_(clo_trace_signals))
      VG_(message)(Vg_DebugMsg,
                   "sigqueue: signal %ld queued for pid %ld\n",
                   SARG2, SARG1);

   /* Check to see if this gave us a pending signal. */
   *flags |= SfPollAfter;
}

PRE(sys_clock_gettime)
{
   /* int clock_gettime(clockid_t clock_id, struct timespec *tp); */
   PRINT("sys_clock_gettime ( %ld, %#lx )", SARG1, ARG2);
   PRE_REG_READ2(long, "clock_gettime", vki_clockid_t, clock_id,
                 struct timespec *, tp);
   PRE_MEM_WRITE("clock_gettime(tp)", ARG2, sizeof(struct vki_timespec));
}

POST(sys_clock_gettime)
{
   POST_MEM_WRITE(ARG2, sizeof(struct vki_timespec));
}

PRE(sys_clock_settime)
{
   /* int clock_settime(clockid_t clock_id, const struct timespec *tp); */
   PRINT("sys_clock_settime ( %ld, %#lx )", SARG1, ARG2);
   PRE_REG_READ2(long, "clock_settime", vki_clockid_t, clock_id,
                 const struct timespec *, tp);
   PRE_MEM_READ("clock_settime(tp)", ARG2, sizeof(struct vki_timespec));
}

PRE(sys_clock_getres)
{
   /* int clock_getres(clockid_t clock_id, struct timespec *res); */
   PRINT("sys_clock_getres ( %ld, %#lx )", SARG1, ARG2);
   PRE_REG_READ2(long, "clock_getres", vki_clockid_t, clock_id,
                 struct timespec *, res);

   if (ARG2)
      PRE_MEM_WRITE("clock_getres(res)", ARG2, sizeof(struct vki_timespec));
}

POST(sys_clock_getres)
{
   if (ARG2)
      POST_MEM_WRITE(ARG2, sizeof(struct vki_timespec));
}

PRE(sys_timer_create)
{
   /* int timer_create(clockid_t clock_id,
                       struct sigevent *evp, timer_t *timerid);
    */
   PRINT("sys_timer_create ( %ld, %#lx, %#lx )", SARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "timer_create", vki_clockid_t, clock_id,
                 struct vki_sigevent *, evp, vki_timer_t *, timerid);

   if (ARG2) {
      struct vki_sigevent *evp = (struct vki_sigevent *) ARG2;
      PRE_FIELD_READ("timer_create(evp.sigev_notify)", evp->sigev_notify);
      PRE_FIELD_READ("timer_create(evp.sigev_signo)", evp->sigev_signo);
      PRE_FIELD_READ("timer_create(evp.sigev_value.sival_int)",
         evp->sigev_value.sival_int);

      /* Be safe. */
      if (ML_(safe_to_deref(evp, sizeof(struct vki_sigevent)))) {
         if ((evp->sigev_notify == VKI_SIGEV_PORT) ||
             (evp->sigev_notify == VKI_SIGEV_THREAD))
            PRE_MEM_READ("timer_create(evp.sigev_value.sival_ptr)", 
                         (Addr) evp->sigev_value.sival_ptr,
                         sizeof(vki_port_notify_t));
      }
   }

   PRE_MEM_WRITE("timer_create(timerid)", ARG3, sizeof(vki_timer_t));
}

POST(sys_timer_create)
{
   POST_MEM_WRITE(ARG3, sizeof(vki_timer_t));
}

PRE(sys_timer_delete)
{
   /* int timer_delete(timer_t timerid); */
   PRINT("sys_timer_delete ( %ld )", SARG1);
   PRE_REG_READ1(long, "timer_delete", vki_timer_t, timerid);
}

PRE(sys_timer_settime)
{
   /* int timer_settime(timer_t timerid, int flags,
                        const struct itimerspec *value,
                        struct itimerspec *ovalue);
    */
   PRINT("sys_timer_settime ( %ld, %ld, %#lx, %#lx )",
         SARG1, SARG2, ARG3, ARG4);
   PRE_REG_READ4(long, "timer_settime", vki_timer_t, timerid,
                 int, flags, const struct vki_itimerspec *, value,
                 struct vki_itimerspec *, ovalue);
   PRE_MEM_READ("timer_settime(value)",
                ARG3, sizeof(struct vki_itimerspec));
   if (ARG4)
      PRE_MEM_WRITE("timer_settime(ovalue)",
                    ARG4, sizeof(struct vki_itimerspec));
}

POST(sys_timer_settime)
{
   if (ARG4)
      POST_MEM_WRITE(ARG4, sizeof(struct vki_itimerspec));
}

PRE(sys_timer_gettime)
{
   /* int timer_gettime(timer_t timerid, struct itimerspec *value); */
   PRINT("sys_timer_gettime ( %ld, %#lx )", SARG1, ARG2);
   PRE_REG_READ2(long, "timer_gettime", vki_timer_t, timerid,
                 struct vki_itimerspec *, value);
   PRE_MEM_WRITE("timer_gettime(value)",
                 ARG2, sizeof(struct vki_itimerspec));
}

POST(sys_timer_gettime)
{
   POST_MEM_WRITE(ARG2, sizeof(struct vki_itimerspec));
}

PRE(sys_timer_getoverrun)
{
   /* int timer_getoverrun(timer_t timerid); */
   PRINT("sys_timer_getoverrun ( %ld )", SARG1);
   PRE_REG_READ1(long, "timer_getoverrun", vki_timer_t, timerid);
}

PRE(sys_facl)
{
   /* int facl(int fildes, int cmd, int nentries, void *aclbufp); */
   PRINT("sys_facl ( %ld, %ld, %ld, %#lx )", SARG1, SARG2, SARG3, ARG4);

   PRE_REG_READ4(long, "facl", int, fildes, int, cmd,
                 int, nentries, void *, aclbufp);

   switch (ARG2 /*cmd*/) {
   case VKI_SETACL:
      if (ARG4)
         PRE_MEM_READ("facl(aclbufp)", ARG4, sizeof(vki_aclent_t));
      break;
   case VKI_GETACL:
      PRE_MEM_WRITE("facl(aclbufp)", ARG4, ARG3 * sizeof(vki_aclent_t));
      break;
   case VKI_GETACLCNT:
      break;
   case VKI_ACE_SETACL:
      if (ARG4)
         PRE_MEM_READ("facl(aclbufp)", ARG4, sizeof(vki_ace_t));
      break;
   case VKI_ACE_GETACL:
      PRE_MEM_WRITE("facl(aclbufp)", ARG4, ARG3 * sizeof(vki_ace_t));
      break;
   case VKI_ACE_GETACLCNT:
      break;
   default:
      VG_(unimplemented)("Syswrap of the facl call with cmd %ld.", SARG2);
      /*NOTREACHED*/
      break;
   }

   /* Be strict. */
   if (!ML_(fd_allowed)(ARG1, "facl", tid, False))
      SET_STATUS_Failure(VKI_EBADF);
}

POST(sys_facl)
{
   switch (ARG2 /*cmd*/) {
   case VKI_SETACL:
      break;
   case VKI_GETACL:
      POST_MEM_WRITE(ARG4, ARG3 * sizeof(vki_aclent_t));
      break;
   case VKI_GETACLCNT:
      break;
   case VKI_ACE_SETACL:
      break;
   case VKI_ACE_GETACL:
      POST_MEM_WRITE(ARG4, ARG3 * sizeof(vki_ace_t));
      break;
   case VKI_ACE_GETACLCNT:
      break;
   default:
      vg_assert(0);
      break;
   }
}

static Int pre_check_and_close_fds(ThreadId tid, const HChar *name,
                                   vki_door_desc_t *desc_ptr,
                                   vki_uint_t desc_num)
{
   vki_uint_t i;

   /* Verify passed file descriptors. */
   for (i = 0; i < desc_num; i++) {
      vki_door_desc_t *desc = &desc_ptr[i];
      if ((desc->d_attributes & DOOR_DESCRIPTOR) &&
          (desc->d_attributes & DOOR_RELEASE)) {
         Int fd = desc->d_data.d_desc.d_descriptor;

         /* Detect and negate attempts by the client to close Valgrind's fds.
            Also if doing -d style logging (which is to fd = 2 = stderr),
            don't allow that to be closed either. */
         if (!ML_(fd_allowed)(fd, name, tid, False) ||
             (fd == 2 && VG_(debugLog_getLevel)() > 0))
            return VKI_EBADF;
      }
   }

   /* All fds are allowed, record information about the closed ones.

      Note: Recording information about any closed fds should generally happen
      in a post wrapper but it is not possible in this case because door calls
      are "very blocking", if the information was recorded after the syscall
      finishes then it would be out-of-date during the call, i.e. while the
      syscall is blocked in the kernel.  Therefore, we record closed fds for
      this specific syscall in the PRE wrapper.  Unfortunately, this creates
      a problem when the syscall fails, for example, door_call() can fail with
      EBADF or EFAULT and then no fds are released.  If that happens the
      information about opened fds is incorrect.  This should be very rare (I
      hope) and such a condition is also reported in the post wrapper. */
   if (VG_(clo_track_fds)) {
      for (i = 0; i < desc_num; i++) {
         vki_door_desc_t *desc = &desc_ptr[i];
         if ((desc->d_attributes & DOOR_DESCRIPTOR) &&
             (desc->d_attributes & DOOR_RELEASE)) {
            Int fd = desc->d_data.d_desc.d_descriptor;
            ML_(record_fd_close)(fd);
         }
      }
   }

   return 0;
}

static void post_record_fds(ThreadId tid, const HChar *name,
                            vki_door_desc_t *desc_ptr, vki_uint_t desc_num)
{
   vki_uint_t i;

   /* Record returned file descriptors. */
   for (i = 0; i < desc_num; i++) {
      vki_door_desc_t *desc = &desc_ptr[i];
      if (desc->d_attributes & DOOR_DESCRIPTOR) {
         Int fd = desc->d_data.d_desc.d_descriptor;
         if (!ML_(fd_allowed)(fd, name, tid, True)) {
            /* Unfortunately, we cannot recover at this point and have to fail
               hard. */
            VG_(message)(Vg_UserMsg, "The %s syscall returned an unallowed"
                                     "file descriptor %d.\n", name, fd);
            VG_(exit)(101);
         }
         else if (VG_(clo_track_fds))
            ML_(record_fd_open_named)(tid, fd);
      }
   }
}

/* Handles repository door protocol request over client door fd. */
static void repository_door_pre_mem_door_call_hook(ThreadId tid, Int fd,
                                                   void *data_ptr,
                                                   SizeT data_size)
{
   vki_rep_protocol_request_t *p = (vki_rep_protocol_request_t *) data_ptr;
   PRE_FIELD_READ("door_call(\"" VKI_REPOSITORY_DOOR_NAME "\", "
                  "request->rpr_request)", p->rpr_request);

   if (ML_(safe_to_deref)(p, sizeof(vki_rep_protocol_request_t))) {
      switch (p->rpr_request) {
      case VKI_REP_PROTOCOL_CLOSE:
         break;
      case VKI_REP_PROTOCOL_ENTITY_SETUP:
         {
            struct vki_rep_protocol_entity_setup *r =
               (struct vki_rep_protocol_entity_setup *) p;
            PRE_FIELD_READ("door_call(\"" VKI_REPOSITORY_DOOR_NAME "\", "
                           "entity_setup->rpr_entityid)", r->rpr_entityid);
            PRE_FIELD_READ("door_call(\"" VKI_REPOSITORY_DOOR_NAME "\", "
                           "entity_setup->rpr_entitytype)", r->rpr_entitytype);
         }
         break;
      case VKI_REP_PROTOCOL_ENTITY_NAME:
         {
            struct vki_rep_protocol_entity_name *r =
               (struct vki_rep_protocol_entity_name *) p;
            PRE_FIELD_READ("door_call(\"" VKI_REPOSITORY_DOOR_NAME "\", "
                           "entity_name->rpr_entityid)", r->rpr_entityid);
            PRE_FIELD_READ("door_call(\"" VKI_REPOSITORY_DOOR_NAME "\", "
                           "entity_name->rpr_answertype)", r->rpr_answertype);
         }
         break;
#if (SOLARIS_REPCACHE_PROTOCOL_VERSION >= 24) && (SOLARIS_REPCACHE_PROTOCOL_VERSION <= 30)
      case VKI_REP_PROTOCOL_ENTITY_FMRI:
         {
            struct vki_rep_protocol_entity_fmri *r =
               (struct vki_rep_protocol_entity_fmri *) p;
            PRE_FIELD_READ("door_call(\"" VKI_REPOSITORY_DOOR_NAME "\", "
                           "entity_fmri->rpr_entityid)", r->rpr_entityid);
         }
         break;
#endif /* 24 <= SOLARIS_REPCACHE_PROTOCOL_VERSION =< 30 */
#if (SOLARIS_REPCACHE_PROTOCOL_VERSION >= 25)
      case VKI_REP_PROTOCOL_ENTITY_GET_ROOT:
         {
            struct vki_rep_protocol_entity_root *r =
               (struct vki_rep_protocol_entity_root *) p;
            PRE_FIELD_READ("door_call(\"" VKI_REPOSITORY_DOOR_NAME "\", "
                           "entity_root->rpr_entityid)", r->rpr_entityid);
            PRE_FIELD_READ("door_call(\"" VKI_REPOSITORY_DOOR_NAME "\", "
                           "entity_root->rpr_outid)", r->rpr_outid);
         }
         break;
#endif /* SOLARIS_REPCACHE_PROTOCOL_VERSION >= 25 */
      case VKI_REP_PROTOCOL_ENTITY_GET:
         {
            struct vki_rep_protocol_entity_get *r =
               (struct vki_rep_protocol_entity_get *) p;
            PRE_FIELD_READ("door_call(\"" VKI_REPOSITORY_DOOR_NAME "\", "
                           "entity_get->rpr_entityid)", r->rpr_entityid);
            PRE_FIELD_READ("door_call(\"" VKI_REPOSITORY_DOOR_NAME "\", "
                           "entity_get->rpr_object)", r->rpr_object);
         }
         break;
      case VKI_REP_PROTOCOL_ENTITY_GET_CHILD:
#if (SOLARIS_REPCACHE_PROTOCOL_VERSION >= 31)
      case VKI_REP_PROTOCOL_ENTITY_GET_CHILD_COMPOSED:
#endif
         {
            struct vki_rep_protocol_entity_get_child *r =
               (struct vki_rep_protocol_entity_get_child *) p;
            PRE_FIELD_READ("door_call(\"" VKI_REPOSITORY_DOOR_NAME "\", "
                           "entity_get_child->rpr_entityid)", r->rpr_entityid);
            PRE_FIELD_READ("door_call(\"" VKI_REPOSITORY_DOOR_NAME "\", "
                           "entity_get_child->rpr_childid)", r->rpr_childid);
            PRE_MEM_RASCIIZ("door_call(\"" VKI_REPOSITORY_DOOR_NAME "\", "
                            "entity_get_child->rpr_name)", (Addr) r->rpr_name);
         }
         break;
      case VKI_REP_PROTOCOL_ENTITY_GET_PARENT:
         {
            struct vki_rep_protocol_entity_parent *r =
               (struct vki_rep_protocol_entity_parent *) p;
            PRE_FIELD_READ("door_call(\"" VKI_REPOSITORY_DOOR_NAME "\", "
                           "entity_get_parent->rpr_entityid)", r->rpr_entityid);
            PRE_FIELD_READ("door_call(\"" VKI_REPOSITORY_DOOR_NAME "\", "
                           "entity_get_parent->rpr_outid)", r->rpr_outid);
         }
         break;
      case VKI_REP_PROTOCOL_ENTITY_RESET:
         {
            struct vki_rep_protocol_entity_reset *r =
               (struct vki_rep_protocol_entity_reset *) p;
            PRE_FIELD_READ("door_call(\"" VKI_REPOSITORY_DOOR_NAME "\", "
                           "entity_reset->rpr_entityid)", r->rpr_entityid);
         }
         break;
      case VKI_REP_PROTOCOL_ENTITY_TEARDOWN:
         {
            struct vki_rep_protocol_entity_teardown *r =
               (struct vki_rep_protocol_entity_teardown *) p;
            PRE_FIELD_READ("door_call(\"" VKI_REPOSITORY_DOOR_NAME "\", "
                           "entity_teardown->rpr_entityid)", r->rpr_entityid);
         }
         break;
      case VKI_REP_PROTOCOL_ITER_READ:
         {
            struct vki_rep_protocol_iter_read *r =
               (struct vki_rep_protocol_iter_read *) p;
            PRE_FIELD_READ("door_call(\"" VKI_REPOSITORY_DOOR_NAME "\", "
                           "iter_read->rpr_iterid)", r->rpr_iterid);
            PRE_FIELD_READ("door_call(\"" VKI_REPOSITORY_DOOR_NAME "\", "
                           "iter_read->rpr_sequence)", r->rpr_sequence);
            PRE_FIELD_READ("door_call(\"" VKI_REPOSITORY_DOOR_NAME "\", "
                           "iter_read->rpr_entityid)", r->rpr_entityid);
         }
         break;
      case VKI_REP_PROTOCOL_ITER_READ_VALUE:
         {
            struct vki_rep_protocol_iter_read_value *r =
               (struct vki_rep_protocol_iter_read_value *) p;
            PRE_FIELD_READ("door_call(\"" VKI_REPOSITORY_DOOR_NAME "\", "
                           "iter_read_value->rpr_iterid)", r->rpr_iterid);
            PRE_FIELD_READ("door_call(\"" VKI_REPOSITORY_DOOR_NAME "\", "
                           "iter_read_value->rpr_sequence)", r->rpr_sequence);
         }
         break;
      case VKI_REP_PROTOCOL_ITER_RESET:
      case VKI_REP_PROTOCOL_ITER_SETUP:
      case VKI_REP_PROTOCOL_ITER_TEARDOWN:
         {
            struct vki_rep_protocol_iter_request *r =
               (struct vki_rep_protocol_iter_request *) p;
            PRE_FIELD_READ("door_call(\"" VKI_REPOSITORY_DOOR_NAME "\", "
                           "iter_request->rpr_iterid)", r->rpr_iterid);
         }
         break;
      case VKI_REP_PROTOCOL_ITER_START:
         {
            struct vki_rep_protocol_iter_start *r =
               (struct vki_rep_protocol_iter_start *) p;
            PRE_FIELD_READ("door_call(\"" VKI_REPOSITORY_DOOR_NAME "\", "
                           "iter_start->rpr_iterid)", r->rpr_iterid);
            PRE_FIELD_READ("door_call(\"" VKI_REPOSITORY_DOOR_NAME "\", "
                           "iter_start->rpr_entity)", r->rpr_entity);
            PRE_FIELD_READ("door_call(\"" VKI_REPOSITORY_DOOR_NAME "\", "
                           "iter_start->rpr_itertype)", r->rpr_itertype);
            PRE_FIELD_READ("door_call(\"" VKI_REPOSITORY_DOOR_NAME "\", "
                           "iter_start->rpr_flags)", r->rpr_flags);
            PRE_MEM_RASCIIZ("door_call(\"" VKI_REPOSITORY_DOOR_NAME "\", "
                            "iter_start->rpr_pattern)", (Addr) r->rpr_pattern);
         }
         break;
      case VKI_REP_PROTOCOL_PROPERTY_GET_TYPE:
      case VKI_REP_PROTOCOL_PROPERTY_GET_VALUE:
         {
            struct vki_rep_protocol_property_request *r =
               (struct vki_rep_protocol_property_request *) p;
            PRE_FIELD_READ("door_call(\"" VKI_REPOSITORY_DOOR_NAME "\", "
                           "property_request->rpr_entityid)", r->rpr_entityid);
         }
         break;
      default:
         VG_(unimplemented)("Door wrapper of " VKI_REPOSITORY_DOOR_NAME
                            " where rpr_request=%#x.", p->rpr_request);
         /* NOTREACHED */
         break;
      }        
   }
}

/* Handles repository door protocol response over client door fd. */
static void repository_door_post_mem_door_call_hook(ThreadId tid, Int fd,
                                                    void *rbuf, SizeT rsize)
{
   /* :TODO: Ideally we would need to match the response type with the
      previous request because response itself does not contain any
      type identification.
      For now simply make defined whole response buffer. */
   POST_MEM_WRITE((Addr) rbuf, rsize);
}

/* Pre-syscall checks for params->data_ptr contents of a door_call(). */
static void door_call_pre_mem_params_data(ThreadId tid, Int fd,
                                          void *data_ptr, SizeT data_size)
{
   const HChar *pathname;

   /* Get pathname of the door file descriptor, if not already done.
      Needed to dissect door service on the pathname below. */
   if (!VG_(clo_track_fds) && !ML_(fd_recorded)(fd)) {
      ML_(record_fd_open_named)(tid, fd);
   }
   pathname = ML_(find_fd_recorded_by_fd)(fd);

   /* Debug-only printing. */
   if (0) {
      VG_(printf)("PRE(door_call) with fd=%d and filename=%s\n",
                  fd, pathname);
   }

   if (VG_STREQ(pathname, VKI__PATH_KCFD_DOOR)) {
      vki_kcf_door_arg_t *p = (vki_kcf_door_arg_t *) data_ptr;

      PRE_FIELD_READ("door_call(\"" VKI__PATH_KCFD_DOOR "\", "
                     "kcf_door_arg_t->da_version)", p->da_version);
      PRE_FIELD_READ("door_call(\"" VKI__PATH_KCFD_DOOR "\", "
                     "kcf_door_arg_t->da_iskernel)", p->da_iskernel);
      PRE_MEM_RASCIIZ("door_call(\"" VKI__PATH_KCFD_DOOR "\", "
                      "kcf_door_arg_t->da_u.filename)",
                      (Addr) p->vki_da_u.filename);
   } else if (VG_STREQ(pathname, VKI_NAME_SERVICE_DOOR)) {
      vki_nss_pheader_t *p = (vki_nss_pheader_t *) data_ptr;

      PRE_FIELD_READ("door_call(\"" VKI_NAME_SERVICE_DOOR "\", "
                     "nss_pheader->nsc_callnumber)", p->nsc_callnumber);
      if (ML_(safe_to_deref)(p, sizeof(vki_nss_pheader_t))) {
         if ((p->nsc_callnumber & VKI_NSCDV2CATMASK) == VKI_NSCD_CALLCAT_APP) {
            /* request from an application towards nscd */
            PRE_FIELD_READ("door_call(\"" VKI_NAME_SERVICE_DOOR "\", "
                           "nss_pheader->p_version)", p->p_version);
            PRE_FIELD_READ("door_call(\"" VKI_NAME_SERVICE_DOOR "\", "
                           "nss_pheader->dbd_off)", p->dbd_off);
            PRE_FIELD_READ("door_call(\"" VKI_NAME_SERVICE_DOOR "\", "
                           "nss_pheader->dbd_len)", p->dbd_len);
            PRE_FIELD_READ("door_call(\"" VKI_NAME_SERVICE_DOOR "\", "
                           "nss_pheader->key_off)", p->key_off);
            PRE_FIELD_READ("door_call(\"" VKI_NAME_SERVICE_DOOR "\", "
                           "nss_pheader->key_len)", p->key_len);
            PRE_FIELD_READ("door_call(\"" VKI_NAME_SERVICE_DOOR "\", "
                           "nss_pheader->data_off)", p->data_off);
            PRE_FIELD_READ("door_call(\"" VKI_NAME_SERVICE_DOOR "\", "
                           "nss_pheader->data_len)", p->data_len);
            /* Fields ext_off and ext_len are set only sporadically. */
            PRE_FIELD_READ("door_call(\"" VKI_NAME_SERVICE_DOOR "\", "
                           "nss_pheader->pbufsiz)", p->pbufsiz);
            PRE_MEM_WRITE("door_call(\"" VKI_NAME_SERVICE_DOOR "\", pbuf)",
                          (Addr) p, p->pbufsiz);

            if (p->dbd_len > 0) {
               vki_nss_dbd_t *dbd
                  = (vki_nss_dbd_t *) ((HChar *) p + p->dbd_off);

               PRE_MEM_READ("door_call(\"" VKI_NAME_SERVICE_DOOR
                            "\", nss_dbd)", (Addr) dbd, sizeof(vki_nss_dbd_t));
               if (ML_(safe_to_deref)(dbd, sizeof(vki_nss_dbd_t))) {
                  if (dbd->o_name != 0)
                     PRE_MEM_RASCIIZ("door_call(\"" VKI_NAME_SERVICE_DOOR
                                     "\", nss_dbd->o_name)", (Addr) ((HChar *) p
                                     + p->dbd_off + dbd->o_name));
                  if (dbd->o_config_name != 0)
                     PRE_MEM_RASCIIZ("door_call(\"" VKI_NAME_SERVICE_DOOR
                                     "\", nss_dbd->o_config_name)",
                                     (Addr) ((HChar *) p + p->dbd_off
                                     + dbd->o_config_name));
                  if (dbd->o_default_config != 0)
                     PRE_MEM_RASCIIZ("door_call(\"" VKI_NAME_SERVICE_DOOR
                                     "\", nss_dbd->o_default_config)",
                                     (Addr) ((HChar *) p + p->dbd_off +
                                     dbd->o_default_config));
              }
           }

           PRE_MEM_READ("door_call(\"" VKI_NAME_SERVICE_DOOR "\", nss->key)",
                        (Addr) ((HChar *) p + p->key_off), p->key_len);
         } else {
            /* request from a child nscd towards parent nscd */
            VG_(unimplemented)("Door wrapper of child/parent nscd.");
         }
      }
   } else if (VG_STREQ(pathname, VKI_REPOSITORY_DOOR_NAME)) {
      vki_repository_door_request_t *p =	
         (vki_repository_door_request_t *) data_ptr;

      PRE_FIELD_READ("door_call(\"" VKI_REPOSITORY_DOOR_NAME "\", "
                     "request->rdr_version)", p->rdr_version);
      PRE_FIELD_READ("door_call(\"" VKI_REPOSITORY_DOOR_NAME "\", "
                     "request->rdr_request)", p->rdr_request);
      if (ML_(safe_to_deref)(p, sizeof(vki_repository_door_request_t))) {
         if (p->rdr_version == VKI_REPOSITORY_DOOR_VERSION) {
            PRE_FIELD_READ("door_call(\"" VKI_REPOSITORY_DOOR_NAME "\", "
                           "request->rdr_flags)", p->rdr_flags);
            PRE_FIELD_READ("door_call(\"" VKI_REPOSITORY_DOOR_NAME "\", "
                           "request->rdr_debug)", p->rdr_debug);
         } else {
            VG_(unimplemented)("Door wrapper of " VKI_REPOSITORY_DOOR_NAME
                               " where version=%u.", p->rdr_version);
         }
      }
   } else {
      const OpenDoor *open_door = door_find_by_fd(fd);
      if ((open_door != NULL) && (open_door->pre_mem_hook != NULL)) {
         open_door->pre_mem_hook(tid, fd, data_ptr, data_size);
      } else {
         if (SimHintiS(SimHint_lax_doors, VG_(clo_sim_hints))) {
            /*
             * Be very lax about door syscall handling over unrecognized
             * door file descriptors. Does not require that full buffer
             * is initialized when writing. Without this, programs using
             * libdoor(3LIB) functionality with completely proprietary
             * semantics may report large number of false positives.
             */
         } else {
            static Int moans = 3;

            /* generic default */
            if (moans > 0 && !VG_(clo_xml)) {
               moans--;
               VG_(umsg)(
"Warning: noted and generically handled door call\n"
"   on file descriptor %d (filename: %s).\n"
"   This could cause spurious value errors to appear.\n"
"   See README_MISSING_SYSCALL_OR_IOCTL for guidance on writing a proper wrapper.\n"
"   Alternatively you may find '--sim-hints=lax-doors' option useful.\n",
                         fd, pathname);
            }
            PRE_MEM_READ("door_call(params->data_ptr)",
                         (Addr) data_ptr, data_size);
         }
      }
   }
}

/* Post-syscall checks for params->rbuf contents of a door_call(). */
static void door_call_post_mem_params_rbuf(ThreadId tid, Int fd,
                                           void *rbuf, SizeT rsize,
                                           const vki_door_desc_t *desc_ptr,
                                           vki_uint_t desc_num)
{
   const HChar *pathname = ML_(find_fd_recorded_by_fd)(fd);

   /* Debug-only printing. */
   if (0) {
      VG_(printf)("POST(door_call) with fd=%d and filename=%s\n",
                  fd, pathname);
   }

   if (VG_STREQ(pathname, VKI__PATH_KCFD_DOOR)) {
      vki_kcf_door_arg_t *p = (vki_kcf_door_arg_t *) rbuf;

      POST_FIELD_WRITE(p->da_version);
      POST_FIELD_WRITE(p->vki_da_u.result.status);
      POST_MEM_WRITE((Addr) p->vki_da_u.result.signature,
                     p->vki_da_u.result.siglen);
   } else if (VG_STREQ(pathname, VKI_NAME_SERVICE_DOOR)) {
      vki_nss_pheader_t *p = (vki_nss_pheader_t *) rbuf;

      POST_FIELD_WRITE(p->nsc_callnumber);
      if (ML_(safe_to_deref)(p, sizeof(vki_nss_pheader_t))) {
         if ((p->nsc_callnumber & VKI_NSCDV2CATMASK) == VKI_NSCD_CALLCAT_APP) {
            /* response from nscd to an application */
            POST_FIELD_WRITE(p->p_status);
            POST_FIELD_WRITE(p->p_errno);
            POST_FIELD_WRITE(p->p_herrno);
            POST_FIELD_WRITE(p->dbd_off);
            POST_FIELD_WRITE(p->dbd_len);
            POST_FIELD_WRITE(p->key_off);
            POST_FIELD_WRITE(p->key_len);
            POST_FIELD_WRITE(p->data_off);
            POST_FIELD_WRITE(p->data_len);
            POST_FIELD_WRITE(p->ext_off);
            POST_FIELD_WRITE(p->ext_len);
            POST_FIELD_WRITE(p->pbufsiz);

            if (p->pbufsiz <= rsize) {
               if (p->dbd_off < p->pbufsiz - sizeof(vki_nss_pheader_t)) {
                  SizeT len = MIN(p->dbd_len, p->pbufsiz - p->dbd_off);
                  POST_MEM_WRITE((Addr) ((HChar *) p + p->dbd_off), len);
               }

               if (p->key_off < p->pbufsiz - sizeof(vki_nss_pheader_t)) {
                  SizeT len = MIN(p->key_len, p->pbufsiz - p->key_off);
                  POST_MEM_WRITE((Addr) ((HChar *) p + p->key_off), len);
               }

               if (p->data_off < p->pbufsiz - sizeof(vki_nss_pheader_t)) {
                  SizeT len = MIN(p->data_len, p->pbufsiz - p->data_off);
                  POST_MEM_WRITE((Addr) ((HChar *) p + p->data_off), len);
               }

               if (p->ext_off < p->pbufsiz - sizeof(vki_nss_pheader_t)) {
                  SizeT len = MIN(p->ext_len, p->pbufsiz - p->ext_off);
                  POST_MEM_WRITE((Addr) ((HChar *) p + p->ext_off), len);
               }
            }
         } else {
            /* response from parent nscd to a child nscd */
            VG_(unimplemented)("Door wrapper of child/parent nscd.");
         }
      }
   } else if (VG_STREQ(pathname, VKI_REPOSITORY_DOOR_NAME)) {
      POST_FIELD_WRITE(((vki_repository_door_response_t *) rbuf)->rdr_status);
      /* A new client door fd is passed over the global repository door. */
      if ((desc_ptr != NULL) && (desc_num > 0)) {
         if (desc_ptr[0].d_attributes & DOOR_DESCRIPTOR) {
            door_record_client(tid, desc_ptr[0].d_data.d_desc.d_descriptor,
                               repository_door_pre_mem_door_call_hook,
                               repository_door_post_mem_door_call_hook);
         }
      }
   } else {
      const OpenDoor *open_door = door_find_by_fd(fd);
      if ((open_door != NULL) && (open_door->post_mem_hook != NULL)) {
         open_door->post_mem_hook(tid, fd, rbuf, rsize);
      } else {
         /* generic default */
         POST_MEM_WRITE((Addr) rbuf, rsize);
      }
   }
}

/* Pre-syscall checks for data_ptr contents in a door_return(). */
static void door_return_pre_mem_data(ThreadId tid, Addr server_procedure,
                                     void *data_ptr, SizeT data_size)
{
   if ((data_size == 0) || (server_procedure == 0)) {
      /* There is nothing to check. This usually happens during thread's
         first call to door_return(). */
      return;
   }

   /* Get pathname of the door file descriptor based on the
      door server procedure (that's all we have).
      Needed to dissect door service on the pathname below. */
   const OpenDoor *open_door = door_find_by_proc(server_procedure);
   const HChar *pathname = (open_door != NULL) ? open_door->pathname : NULL;
   Int fd = (open_door != NULL) ? open_door->fd : -1;

   /* Debug-only printing. */
   if (0) {
      VG_(printf)("PRE(door_return) with fd=%d and filename=%s "
                  "(nr_doors_recorded=%u)\n",
                  fd, pathname, nr_doors_recorded);
   }

   if (VG_STREQ(pathname, VKI__PATH_KCFD_DOOR)) {
      vki_kcf_door_arg_t *p = (vki_kcf_door_arg_t *) data_ptr;

      PRE_FIELD_READ("door_return(\"" VKI__PATH_KCFD_DOOR "\", "
                     "kcf_door_arg_t->da_version)", p->da_version);
      PRE_FIELD_READ("door_return(\"" VKI__PATH_KCFD_DOOR "\", "
                     "kcf_door_arg_t->da_u.result.status)",
                     p->vki_da_u.result.status);
      PRE_MEM_READ("door_return(\"" VKI__PATH_KCFD_DOOR "\", "
                   "kcf_door_arg_t->da_u.result.signature)",
                   (Addr) p->vki_da_u.result.signature,
                   p->vki_da_u.result.siglen);
   } else if (VG_STREQ(pathname, VKI_NAME_SERVICE_DOOR)) {
      vki_nss_pheader_t *p = (vki_nss_pheader_t *) data_ptr;

      PRE_FIELD_READ("door_return(\"" VKI_NAME_SERVICE_DOOR "\", "
                     "nss_pheader->nsc_callnumber)", p->nsc_callnumber);
      if (ML_(safe_to_deref)(p, sizeof(vki_nss_pheader_t))) {
         if ((p->nsc_callnumber & VKI_NSCDV2CATMASK) == VKI_NSCD_CALLCAT_APP) {
            /* response from nscd to an application */
            PRE_FIELD_READ("door_return(\"" VKI_NAME_SERVICE_DOOR "\", "
                           "nss_pheader->p_status)", p->p_status);
            PRE_FIELD_READ("door_return(\"" VKI_NAME_SERVICE_DOOR "\", "
                           "nss_pheader->p_errno)", p->p_errno);
            PRE_FIELD_READ("door_return(\"" VKI_NAME_SERVICE_DOOR "\", "
                           "nss_pheader->p_herrno)", p->p_herrno);
            PRE_FIELD_READ("door_return(\"" VKI_NAME_SERVICE_DOOR "\", "
                           "nss_pheader->dbd_off)", p->dbd_off);
            PRE_FIELD_READ("door_return(\"" VKI_NAME_SERVICE_DOOR "\", "
                           "nss_pheader->dbd_len)", p->dbd_len);
            PRE_FIELD_READ("door_return(\"" VKI_NAME_SERVICE_DOOR "\", "
                           "nss_pheader->data_off)", p->data_off);
            PRE_FIELD_READ("door_return(\"" VKI_NAME_SERVICE_DOOR "\", "
                           "nss_pheader->data_len)", p->data_len);
            PRE_FIELD_READ("door_return(\"" VKI_NAME_SERVICE_DOOR "\", "
                           "nss_pheader->ext_off)", p->ext_off);
            PRE_FIELD_READ("door_return(\"" VKI_NAME_SERVICE_DOOR "\", "
                           "nss_pheader->ext_len)", p->ext_len);
            PRE_FIELD_READ("door_return(\"" VKI_NAME_SERVICE_DOOR "\", "
                           "nss_pheader->pbufsiz)", p->pbufsiz);
            PRE_MEM_WRITE("door_return(\"" VKI_NAME_SERVICE_DOOR "\", pbuf)",
                          (Addr) p, p->pbufsiz);
            PRE_MEM_READ("door_return(\"" VKI_NAME_SERVICE_DOOR
                         "\", nss->data)",
                         (Addr) ((HChar *) p + p->data_off), p->data_len);
            PRE_MEM_READ("door_return(\"" VKI_NAME_SERVICE_DOOR
                         "\", nss->ext)",
                         (Addr) ((HChar *) p + p->ext_off), p->ext_len);
         } else {
            /* response from parent nscd to a child nscd */
            VG_(unimplemented)("Door wrapper of child/parent nscd.");
         }
      }
   } else if (VG_STREQ(pathname, VKI_REPOSITORY_DOOR_NAME)) {
            VG_(unimplemented)("Door wrapper of " VKI_REPOSITORY_DOOR_NAME);
   } else {
      if (SimHintiS(SimHint_lax_doors, VG_(clo_sim_hints))) {
         /*
          * Be very lax about door syscall handling over unrecognized
          * door file descriptors. Does not require that full buffer
          * is initialized when writing. Without this, programs using
          * libdoor(3LIB) functionality with completely proprietary
          * semantics may report large number of false positives.
          */
      } else {
         static Int moans = 3;

         /* generic default */
         if (moans > 0 && !VG_(clo_xml)) {
            moans--;
            VG_(umsg)(
"Warning: noted and generically handled door return\n"
"   on file descriptor %d (filename: %s).\n"
"   This could cause spurious value errors to appear.\n"
"   See README_MISSING_SYSCALL_OR_IOCTL for guidance on writing a proper wrapper.\n"
"   Alternatively you may find '--sim-hints=lax-doors' option useful.\n",
                   fd, pathname);
         }
         PRE_MEM_READ("door_return(data_ptr)",
                      (Addr) data_ptr, data_size);
      }
   }
}

/* Post-syscall checks for data_ptr contents in a door_return(). */
static void door_return_post_mem_data(ThreadId tid, Addr server_procedure,
                                      void *data_ptr, SizeT data_size)
{
   const OpenDoor *open_door = door_find_by_proc(server_procedure);
   const HChar *pathname = (open_door != NULL) ? open_door->pathname : NULL;

   /* Debug-only printing. */
   if (0) {
      Int fd = (open_door != NULL) ? open_door->fd : -1;
      VG_(printf)("POST(door_return) with fd=%d and filename=%s "
                  "(nr_doors_recorded=%u)\n",
                  fd, pathname, nr_doors_recorded);
   }

   if (VG_STREQ(pathname, VKI__PATH_KCFD_DOOR)) {
      vki_kcf_door_arg_t *p = (vki_kcf_door_arg_t *) data_ptr;

      POST_FIELD_WRITE(p->da_version);
      POST_FIELD_WRITE(p->da_iskernel);
      POST_MEM_WRITE((Addr) p->vki_da_u.filename,
                     VG_(strlen)(p->vki_da_u.filename) + 1);
   } else if (VG_STREQ(pathname, VKI_NAME_SERVICE_DOOR)) {
      vki_nss_pheader_t *p = (vki_nss_pheader_t *) data_ptr;

      POST_FIELD_WRITE(p->nsc_callnumber);
      if (ML_(safe_to_deref)(p, sizeof(vki_nss_pheader_t))) {
         if ((p->nsc_callnumber & VKI_NSCDV2CATMASK) == VKI_NSCD_CALLCAT_APP) {
            /* request from an application towards nscd */
            POST_FIELD_WRITE(p->p_version);
            POST_FIELD_WRITE(p->dbd_off);
            POST_FIELD_WRITE(p->dbd_len);
            POST_FIELD_WRITE(p->key_off);
            POST_FIELD_WRITE(p->key_len);
            POST_FIELD_WRITE(p->data_off);
            POST_FIELD_WRITE(p->data_len);
            POST_FIELD_WRITE(p->ext_off);
            POST_FIELD_WRITE(p->ext_len);
            POST_FIELD_WRITE(p->pbufsiz);

            if (p->dbd_len > 0) {
               vki_nss_dbd_t *dbd
                  = (vki_nss_dbd_t *) ((HChar *) p + p->dbd_off);

               POST_MEM_WRITE((Addr) dbd, sizeof(vki_nss_dbd_t));
               if (ML_(safe_to_deref)(dbd, sizeof(vki_nss_dbd_t))) {
                  SizeT headers_size = sizeof(vki_nss_pheader_t)
                     + sizeof(vki_nss_dbd_t);

                  if (dbd->o_name != 0) {
                     HChar *name = (HChar *) p + p->dbd_off + dbd->o_name;
                     SizeT name_len = VG_(strlen)(name) + 1;
                     if (name_len <= data_size - headers_size)
                        POST_MEM_WRITE((Addr) name, name_len);
                  }
                  if (dbd->o_config_name != 0) {
                     HChar *name = (HChar *) p + p->dbd_off + dbd->o_config_name;
                     SizeT name_len = VG_(strlen)(name) + 1;
                     if (name_len <= data_size - headers_size)
                        POST_MEM_WRITE((Addr) name, name_len);
                  }
                  if (dbd->o_default_config != 0) {
                     HChar *name = (HChar *) p + p->dbd_off
                        + dbd->o_default_config;
                     SizeT name_len = VG_(strlen)(name) + 1;
                     if (name_len <= data_size - headers_size)
                        POST_MEM_WRITE((Addr) name, name_len);
                  }
              }
           }

           if (p->key_len <= data_size - p->key_off)
              POST_MEM_WRITE((Addr) ((HChar *) p + p->key_off), p->key_len);
         } else {
            /* request from a child nscd towards parent nscd */
            VG_(unimplemented)("Door wrapper of child/parent nscd.");
         }
      }
   } else if (VG_STREQ(pathname, VKI_REPOSITORY_DOOR_NAME)) {
            VG_(unimplemented)("Door wrapper of " VKI_REPOSITORY_DOOR_NAME);
   } else {
      /* generic default */
      POST_MEM_WRITE((Addr) data_ptr, data_size);
   }
}

PRE(sys_door)
{
   /* int doorfs(long arg1, long arg2, long arg3, long arg4, long arg5,
                 long subcode); */
   ThreadState *tst = VG_(get_ThreadState)(tid);
   *flags |= SfMayBlock | SfPostOnFail;

   PRINT("sys_door ( %#lx, %#lx, %#lx, %#lx, %#lx, %ld )", ARG1, ARG2, ARG3,
         ARG4, ARG5, SARG6);

   /* Macro PRE_REG_READ6 cannot be simply used because not all ARGs are used
      in door() syscall variants. Note that ARG6 (subcode) is used always. */
#define PRE_REG_READ_SIXTH_ONLY         \
   if (VG_(tdict).track_pre_reg_read) { \
      PRA6("door", long, subcode);      \
   }

   switch (ARG6 /*subcode*/) {
   case VKI_DOOR_CREATE:
      PRE_REG_READ3(long, "door", long, arg1, long, arg2, long, arg3);
      PRE_REG_READ_SIXTH_ONLY;
      /* Note: the first argument to DOOR_CREATE is a server procedure.
         This could lead to a problem if the kernel tries to force the
         execution of this procedure, similarly to how signal handlers are
         executed.   Fortunately, the kernel never does that (for user-space
         server procedures).  The procedure is always executed by the standard
         library. */
      break;
   case VKI_DOOR_REVOKE:
      PRE_REG_READ1(long, "door", long, arg1);
      PRE_REG_READ_SIXTH_ONLY;
      if (!ML_(fd_allowed)(ARG1, "door_revoke", tid, False))
         SET_STATUS_Failure(VKI_EBADF);
      break;
   case VKI_DOOR_INFO:
      PRE_REG_READ2(long, "door", long, arg1, long, arg2);
      PRE_REG_READ_SIXTH_ONLY;
      PRE_MEM_WRITE("door_info(info)", ARG2, sizeof(vki_door_info_t));
      break;
   case VKI_DOOR_CALL:
      {
         PRE_REG_READ2(long, "door", long, arg1, long, arg2);
         PRE_REG_READ_SIXTH_ONLY;

         Int rval = 0;
         vki_door_arg_t *params = (vki_door_arg_t*)ARG2;

         if (!ML_(fd_allowed)(ARG1, "door_call", tid, False))
            rval = VKI_EBADF;

         PRE_FIELD_READ("door_call(params->data_ptr)", params->data_ptr);
         PRE_FIELD_READ("door_call(params->data_size)", params->data_size);
         PRE_FIELD_READ("door_call(params->desc_ptr)", params->desc_ptr);
         PRE_FIELD_READ("door_call(params->desc_num)", params->desc_num);
         PRE_FIELD_READ("door_call(params->rbuf)", params->rbuf);
         PRE_FIELD_READ("door_call(params->rsize)", params->rsize);

         if (ML_(safe_to_deref)(params, sizeof(*params))) {
            if (params->data_ptr)
               door_call_pre_mem_params_data(tid, ARG1, params->data_ptr,
                                             params->data_size);

            if (params->desc_ptr) {
               SizeT desc_size = params->desc_num * sizeof(*params->desc_ptr);
               PRE_MEM_READ("door_call(params->desc_ptr)",
                            (Addr)params->desc_ptr, desc_size);

               /* Do not record information about closed fds if we are going
                  to fail the syscall and so no fds will be closed. */
               if ((rval == 0) &&
                   (ML_(safe_to_deref)(params->desc_ptr, desc_size))) {
                     rval = pre_check_and_close_fds(tid, "door_call",
                                                    params->desc_ptr,
                                                    params->desc_num);
               }
            }

            if (params->rbuf)
               PRE_MEM_WRITE("door_call(params->rbuf)", (Addr)params->rbuf,
                             params->rsize);
         }

         if (rval)
            SET_STATUS_Failure(rval);
      }
      break;
   case VKI_DOOR_BIND:
      PRE_REG_READ1(long, "door", long, arg1);
      PRE_REG_READ_SIXTH_ONLY;
      VG_(unimplemented)("DOOR_BIND");
      break;
   case VKI_DOOR_UNBIND:
      PRE_REG_READ0(long, "door");
      PRE_REG_READ_SIXTH_ONLY;
      VG_(unimplemented)("DOOR_UNBIND");
      break;
   case VKI_DOOR_UNREFSYS:
      PRE_REG_READ0(long, "door");
      PRE_REG_READ_SIXTH_ONLY;
      VG_(unimplemented)("DOOR_UNREFSYS");
      break;
   case VKI_DOOR_UCRED:
      PRE_REG_READ1(long, "door", long, arg1);
      PRE_REG_READ_SIXTH_ONLY;
      VG_(unimplemented)("DOOR_UCRED");
      break;
   case VKI_DOOR_RETURN:
      PRE_REG_READ6(long, "door", long, arg1, long, arg2, long, arg3,
                    long, arg4, long, arg5, long, subcode);

      /* Register %esp/%rsp is read and modified by the syscall. */
      VG_TRACK(pre_reg_read, Vg_CoreSysCall, tid, "door_return(sp)",
               VG_O_STACK_PTR, sizeof(UWord));
      /* Register %ebp/%rbp is not really read by the syscall, it is only
         written by it, but it is hard to determine when it is written so we
         make sure it is always valid prior to making the syscall. */
      VG_TRACK(pre_reg_read, Vg_CoreSysCall, tid, "door_return(bp)",
               VG_O_FRAME_PTR, sizeof(UWord));

      door_return_pre_mem_data(tid, tst->os_state.door_return_procedure,
                               (void *) ARG1, ARG2);

      /* Do not tell the tool where the syscall is going to write the
         resulting data.  It is necessary to skip this check because the data
         area starting at ARG4-ARG5 (of length ARG5) is usually on a client
         thread stack below the stack pointer and therefore it can be marked
         by a tool (for example, Memcheck) as inaccessible.  It is ok to skip
         this check in this case because if there is something wrong with the
         data area then the syscall will fail or the error will be handled by
         POST_MEM_WRITE() in the post wrapper. */
      /*PRE_MEM_WRITE("door_return(sp)", ARG4 - ARG5, ARG5);*/

      if (ARG3) {
         vki_door_return_desc_t *desc_env = (vki_door_return_desc_t*)ARG3;

         PRE_MEM_READ("door_return(desc_env)", ARG3,
                      sizeof(vki_door_return_desc_t));

         if (ML_(safe_to_deref)(desc_env, sizeof(*desc_env)) &&
             desc_env->desc_ptr) {
            Int rval;

            PRE_MEM_READ("door_return(desc_env->desc_ptr)",
                         (Addr)desc_env->desc_ptr,
                         desc_env->desc_num * sizeof(*desc_env->desc_ptr));

            rval = pre_check_and_close_fds(tid, "door_return",
                                           desc_env->desc_ptr,
                                           desc_env->desc_num);
            if (rval)
               SET_STATUS_Failure(rval);
         }
      }
      tst->os_state.in_door_return = True;
      tst->os_state.door_return_procedure = 0;
      break;
   case VKI_DOOR_GETPARAM:
      PRE_REG_READ3(long, "door", long, arg1, long, arg2, long, arg3);
      PRE_REG_READ_SIXTH_ONLY;
      VG_(unimplemented)("DOOR_GETPARAM");
      break;
   case VKI_DOOR_SETPARAM:
      PRE_REG_READ3(long, "door", long, arg1, long, arg2, long, arg3);
      PRE_REG_READ_SIXTH_ONLY;
      if (!ML_(fd_allowed)(ARG1, "door_setparam", tid, False))
         SET_STATUS_Failure(VKI_EBADF);
      break;
   default:
      VG_(unimplemented)("Syswrap of the door call with subcode %ld.", SARG6);
      /*NOTREACHED*/
      break;
   }

#undef PRE_REG_READ_SIXTH_ONLY
}

POST(sys_door)
{
   ThreadState *tst = VG_(get_ThreadState)(tid);

   vg_assert(SUCCESS || FAILURE);

   /* Alter the tst->os_state.in_door_return flag. */
   if (ARG6 == VKI_DOOR_RETURN) {
      vg_assert(tst->os_state.in_door_return == True);
      tst->os_state.in_door_return = False;

      /* Inform the tool that %esp/%rsp and %ebp/%rbp were (potentially)
         modified. */
      VG_TRACK(post_reg_write, Vg_CoreSysCall, tid, VG_O_STACK_PTR,
               sizeof(UWord));
      VG_TRACK(post_reg_write, Vg_CoreSysCall, tid, VG_O_FRAME_PTR,
               sizeof(UWord));
   }
   else
      vg_assert(tst->os_state.in_door_return == False);

   if (FAILURE) {
      if (VG_(clo_track_fds)) {
         /* See the discussion in pre_check_and_close_fds() to understand this
            part. */
         Bool loss = False;
         switch (ARG6 /*subcode*/) {
         case VKI_DOOR_CALL:
            if (ERR == VKI_EFAULT || ERR == VKI_EBADF)
               loss = True;
            break;
         case VKI_DOOR_RETURN:
            if (ERR == VKI_EFAULT || ERR == VKI_EINVAL)
               loss = True;
            break;
         default:
            break;
         }
         if (loss)
            VG_(message)(Vg_UserMsg, "The door call failed with an "
                                     "unexpected error and information "
                                     "about open file descriptors can be "
                                     "now imprecise.\n");
      }

      return;
   }

   vg_assert(SUCCESS);

   switch (ARG6 /*subcode*/) {
   case VKI_DOOR_CREATE:
      door_record_server(tid, ARG1, RES);
      break;
   case VKI_DOOR_REVOKE:
      door_record_revoke(tid, ARG1);
      if (VG_(clo_track_fds))
         ML_(record_fd_close)(ARG1);
      break;
   case VKI_DOOR_INFO:
      POST_MEM_WRITE(ARG2, sizeof(vki_door_info_t));
      break;
   case VKI_DOOR_CALL:
      {
         /* Note that all returned values are stored in the rbuf, i.e.
            data_ptr and desc_ptr points into this buffer. */
         vki_door_arg_t *params = (vki_door_arg_t*)ARG2;

         if (params->rbuf) {
            Addr addr = (Addr)params->rbuf;
            if (!VG_(am_find_anon_segment)(addr)) {
               /* This segment is new and was mapped by the kernel. */
               UInt prot, flags;
               SizeT size;

               prot = VKI_PROT_READ | VKI_PROT_WRITE | VKI_PROT_EXEC;
               flags = VKI_MAP_ANONYMOUS;
               size = VG_PGROUNDUP(params->rsize);

               VG_(debugLog)(1, "syswrap-solaris", "POST(sys_door), "
                                "new segment: vaddr=%#lx, size=%#lx, "
                                "prot=%#x, flags=%#x, fd=%lu, offset=%#llx\n",
                                addr, size, prot, flags, (UWord)-1, (ULong)0);

               ML_(notify_core_and_tool_of_mmap)(addr, size, prot, flags,
                                                 -1, 0);

               /* Note: We don't notify the debuginfo reader about this
                  mapping because there is no debug information stored in
                  this segment. */
            }

            door_call_post_mem_params_rbuf(tid, ARG1, (void *) addr,
                                           params->rsize, params->desc_ptr,
                                           params->desc_num);
         }

         if (params->desc_ptr) {
            POST_MEM_WRITE((Addr)params->desc_ptr,
                           params->desc_num * sizeof(vki_door_desc_t));
            post_record_fds(tid, "door_call", params->desc_ptr,
                            params->desc_num);
         }
      }
      break;
   case VKI_DOOR_BIND:
      break;
   case VKI_DOOR_UNBIND:
      break;
   case VKI_DOOR_UNREFSYS:
      break;
   case VKI_DOOR_UCRED:
      break;
   case VKI_DOOR_RETURN:
      {
         struct vki_door_results *results
            = (struct vki_door_results*)VG_(get_SP)(tid);

         tst->os_state.door_return_procedure = (Addr)results->pc;

         POST_MEM_WRITE((Addr)results, sizeof(*results));
         if (results->data_ptr)
            door_return_post_mem_data(tid,
                                      tst->os_state.door_return_procedure,
                                      results->data_ptr,
                                      results->data_size);
         if (results->desc_ptr) {
            POST_MEM_WRITE((Addr)results->desc_ptr,
                           results->desc_num * sizeof(vki_door_desc_t));
            post_record_fds(tid, "door_return", results->desc_ptr,
                            results->desc_num);
         }

         POST_MEM_WRITE((Addr)results->door_info,
                        sizeof(*results->door_info));
      }
      break;
   case VKI_DOOR_GETPARAM:
      break;
   case VKI_DOOR_SETPARAM:
      break;
   default:
      vg_assert(0);
      break;
   }
}

PRE(sys_schedctl)
{
   /* caddr_t schedctl(void); */
   /* This syscall returns an address that points to struct sc_shared.
      This per-thread structure is used as an interface between the libc and
      the kernel. */
   PRINT("sys_schedctl ( )");
   PRE_REG_READ0(long, "schedctl");
}

POST(sys_schedctl)
{
   Addr a = RES;
   ThreadState *tst = VG_(get_ThreadState)(tid);

   /* Stay sane. */
   vg_assert((tst->os_state.schedctl_data == 0) ||
             (tst->os_state.schedctl_data == a));
   tst->os_state.schedctl_data = a;

   /* Returned address points to a block in a mapped page. */
   if (!VG_(am_find_anon_segment)(a)) {
      Addr page = VG_PGROUNDDN(a);
      UInt prot = VKI_PROT_READ | VKI_PROT_WRITE;
#     if defined(SOLARIS_SCHEDCTL_PAGE_EXEC)
      prot |= VKI_PROT_EXEC;
#     endif /* SOLARIS_SCHEDCTL_PAGE_EXEC */
      UInt flags = VKI_MAP_ANONYMOUS;
      /* The kernel always allocates one page for the sc_shared struct. */
      SizeT size = VKI_PAGE_SIZE;

      VG_(debugLog)(1, "syswrap-solaris", "POST(sys_schedctl), new segment: "
                    "vaddr=%#lx, size=%#lx, prot=%#x, flags=%#x, fd=-1, "
                    "offset=0\n", page, size, prot, flags);

      /* The kernel always places redzone before and after the allocated page.
         Check this assertion now; the tool can later request to allocate
         a Valgrind segment and aspacemgr will place it adjacent. */
      const NSegment *seg = VG_(am_find_nsegment)(page - 1);
      vg_assert(seg == NULL || seg->kind == SkResvn);
      seg = VG_(am_find_nsegment)(page + VKI_PAGE_SIZE);
      vg_assert(seg == NULL || seg->kind == SkResvn);

      /* The address space manager works with whole pages. */
      VG_(am_notify_client_mmap)(page, size, prot, flags, -1, 0);

      /* Note: It isn't needed to notify debuginfo about the new mapping
         because it's only an anonymous mapping. */
      /* Note: schedctl data are cleaned in two places:
         - for the tool when the thread exits
         - for the core in child's post-fork handler clean_schedctl_data(). */
   }

   /* The tool needs per-thread granularity, not whole pages. */
   VG_TRACK(new_mem_mmap, a, sizeof(struct vki_sc_shared), True, True, True, 0);
   POST_MEM_WRITE(a, sizeof(struct vki_sc_shared));
}

PRE(sys_pset)
{
   /* Kernel: int pset(int subcode, long arg1, long arg2, long arg3,
                       long arg4); */
   switch (ARG1 /* subcode */) {
   case VKI_PSET_CREATE:
      /* Libc: int pset_create(psetid_t *newpset); */
      PRINT("sys_pset ( %ld, %#lx )", SARG1, ARG2);
      PRE_REG_READ2(long, SC2("pset", "create"), int, subcode,
                    vki_psetid_t *, newpset);
      PRE_MEM_WRITE("pset(newpset)", ARG2, sizeof(vki_psetid_t));
      break;
   case VKI_PSET_DESTROY:
      /* Libc: int pset_destroy(psetid_t pset); */
      PRINT("sys_pset ( %ld, %ld )", SARG1, SARG2);
      PRE_REG_READ2(long, SC2("pset", "destroy"), int, subcode,
                    vki_psetid_t, pset);
      break;
   case VKI_PSET_ASSIGN:
      /* Libc: int pset_assign(psetid_t pset, processorid_t cpu,
                               psetid_t *opset); */
      PRINT("sys_pset ( %ld, %ld, %ld, %#lx )", SARG1, SARG2, SARG3, ARG4);
      PRE_REG_READ4(long, SC2("pset", "assign"), int, subcode,
                    vki_psetid_t, pset, vki_processorid_t, cpu,
                    vki_psetid_t *, opset);
      if (ARG4 != 0)
         PRE_MEM_WRITE("pset(opset)", ARG4, sizeof(vki_psetid_t));
      break;
   case VKI_PSET_INFO:
      /* Libc: int pset_info(psetid_t pset, int *type, uint_t *numcpus,
                             processorid_t *cpulist); */
      PRINT("sys_pset ( %ld, %ld, %#lx, %#lx, %#lx )", SARG1, SARG2, ARG3,
                                                       ARG4, ARG5);
      PRE_REG_READ5(long, SC2("pset", "info"), int, subcode, vki_psetid_t, pset,
                    int *, type, vki_uint_t *, numcpus,
                    vki_processorid_t *, cpulist);
      if (ARG3 != 0)
         PRE_MEM_WRITE("pset(type)", ARG3, sizeof(int));
      if (ARG4 != 0)
         PRE_MEM_WRITE("pset(numcpus)", ARG4, sizeof(vki_uint_t));
      if ((ARG4 != 0) && (ARG5 != 0)) {
         vki_uint_t *numcpus = (vki_uint_t *) ARG4;
         if (ML_(safe_to_deref(numcpus, sizeof(vki_uint_t)))) {
            PRE_MEM_WRITE("pset(cpulist)", ARG5,
                          *numcpus * sizeof(vki_processorid_t));
            /* If cpulist buffer is not large enough, it will hold only as many
               entries as fit in the buffer. However numcpus will contain the
               real number of cpus which will be greater than originally passed
               in. Stash the original value in unused ARG6. */
            ARG6 = *numcpus;
         }
      }
      break;
   case VKI_PSET_BIND:
      /* Libc: int pset_bind(psetid_t pset, idtype_t idtype, id_t id,
                             psetid_t *opset); */
      PRINT("sys_pset ( %ld, %ld, %ld, %ld, %#lx )", SARG1, SARG2, SARG3,
                                                     SARG4, ARG5);
      PRE_REG_READ5(long, SC2("pset", "bind"), int, subcode, vki_psetid_t, pset,
                    vki_idtype_t, idtype, vki_id_t, id, vki_psetid_t *, opset);
      if (ARG5 != 0)
         PRE_MEM_WRITE("pset(opset)", ARG5, sizeof(vki_psetid_t));
      break;
   case VKI_PSET_BIND_LWP:
      /* Libc: int pset_bind_lwp(psetid_t pset, id_t id, pid_t pid,
                                 psetid_t *opset); */
      PRINT("sys_pset ( %ld, %ld, %ld, %ld, %#lx )", SARG1, SARG2, SARG3,
                                                     SARG4, ARG5);
      PRE_REG_READ5(long, SC2("pset", "bind_lwp"), int, subcode,
                    vki_psetid_t, pset, vki_id_t, id, vki_pid_t, pid,
                    vki_psetid_t *, opset);
      if (ARG5 != 0)
         PRE_MEM_WRITE("pset(opset)", ARG5, sizeof(vki_psetid_t));
      break;
   case VKI_PSET_GETLOADAVG:
      /* Libc: int pset_getloadavg(psetid_t pset, double loadavg[],
                                   int nelem); */
      PRINT("sys_pset ( %ld, %ld, %#lx, %ld )", SARG1, SARG2, ARG3, SARG4);
      PRE_REG_READ4(long, SC2("pset", "getloadavg"), int, subcode,
                    vki_psetid_t, pset, int *, buf, int, nelem);
      if (ARG3 != 0)
         PRE_MEM_WRITE("pset(buf)", ARG3, SARG4 * sizeof(int));
      break;
   case VKI_PSET_LIST:
      /* Libc: int pset_list(psetid_t *psetlist, uint_t *numpsets); */
      PRINT("sys_pset ( %ld, %#lx, %#lx )", SARG1, ARG2, ARG3);
      PRE_REG_READ3(long, SC2("pset", "list"), int, subcode,
                    vki_psetid_t *, psetlist, vki_uint_t *, numpsets);
      if (ARG3 != 0)
         PRE_MEM_WRITE("pset(numpsets)", ARG3, sizeof(vki_uint_t));
      if ((ARG2 != 0) && (ARG3 != 0)) {
         vki_uint_t *numpsets = (vki_uint_t *) ARG3;
         if (ML_(safe_to_deref(numpsets, sizeof(vki_uint_t)))) {
            PRE_MEM_WRITE("pset(psetlist)", ARG2,
                          *numpsets * sizeof(vki_psetid_t));
            /* If psetlist buffer is not large enough, it will hold only as many
               entries as fit in the buffer. However numpsets will contain the
               real number of processor sets which will be greater than
               originally passed in. Stash the original value in unused ARG6. */
            ARG6 = *numpsets;
         }
      }
      break;
#  if defined(SOLARIS_PSET_GET_NAME)
   case VKI_PSET_GET_NAME:
      /* Libc: int pset_get_name(psetid_t psetid, char *buf, uint_t len); */
      PRINT("sys_pset ( %ld, %ld, %#lx, %ld )", SARG1, SARG2, ARG3, SARG4);
      PRE_REG_READ4(long, SC2("pset", "get_name"), int, subcode,
                    vki_psetid_t, pset, char *, buf, vki_uint_t, len);
      PRE_MEM_WRITE("pset(buf)", ARG3, ARG4);
      break;
#  endif /* SOLARIS_PSET_GET_NAME */
   case VKI_PSET_SETATTR:
      /* Libc: int pset_setattr(psetid_t pset, uint_t attr); */
      PRINT("sys_pset ( %ld, %ld, %lu )", SARG1, SARG2, ARG3);
      PRE_REG_READ3(long, SC2("pset", "setattr"), int, subcode,
                    vki_psetid_t, pset, vki_uint_t, attr);
      break;
   case VKI_PSET_GETATTR:
      /* Libc: int pset_getattr(psetid_t pset, uint_t *attr); */
      PRINT("sys_pset ( %ld, %ld, %#lx )", SARG1, SARG2, ARG3);
      PRE_REG_READ3(long, SC2("pset", "getattr"), int, subcode,
                    vki_psetid_t, pset, vki_uint_t *, attr);
      PRE_MEM_WRITE("pset(attr)", ARG3, sizeof(vki_uint_t));
      break;
   case VKI_PSET_ASSIGN_FORCED:
      /* Libc: int pset_assign_forced(psetid_t pset, processorid_t cpu,
                                      psetid_t *opset); */
      PRINT("sys_pset ( %ld, %ld, %ld, %#lx )", SARG1, SARG2, SARG3, ARG4);
      PRE_REG_READ4(long, SC2("pset", "assign_forced"), int, subcode,
                    vki_psetid_t, pset, vki_processorid_t, cpu,
                    vki_psetid_t *, opset);
      if (ARG4 != 0)
         PRE_MEM_WRITE("pset(opset)", ARG4, sizeof(vki_psetid_t));
      break;
   default:
      VG_(unimplemented)("Syswrap of pset syscall with subcode %ld.", SARG1);
      /*NOTREACHED*/
      break;
   }
}

POST(sys_pset)
{
   switch (ARG1 /*subcode*/) {
   case VKI_PSET_CREATE:
      POST_MEM_WRITE(ARG2, sizeof(vki_psetid_t));
      break;
   case VKI_PSET_DESTROY:
      break;
   case VKI_PSET_ASSIGN:
      if (ARG4 != 0)
         POST_MEM_WRITE(ARG4, sizeof(vki_psetid_t));
      break;
   case VKI_PSET_INFO:
      if (ARG3 != 0)
         POST_MEM_WRITE(ARG3, sizeof(int));
      if (ARG4 != 0)
         POST_MEM_WRITE(ARG4, sizeof(vki_uint_t));
      if ((ARG4 != 0) && (ARG5 != 0)) {
         vki_uint_t *numcpus = (vki_uint_t *) ARG4;
         POST_MEM_WRITE(ARG5, MIN(*numcpus, ARG6) * sizeof(vki_processorid_t));
      }
      break;
   case VKI_PSET_BIND:
      if (ARG5 != 0)
         POST_MEM_WRITE(ARG5, sizeof(vki_psetid_t));
      break;
   case VKI_PSET_BIND_LWP:
      if (ARG5 != 0)
         POST_MEM_WRITE(ARG5, sizeof(vki_psetid_t));
      break;
   case VKI_PSET_GETLOADAVG:
      if (ARG3 != 0)
         POST_MEM_WRITE(ARG3, MIN(SARG4, VKI_LOADAVG_NSTATS) * sizeof(int));
      break;
   case VKI_PSET_LIST:
      if (ARG3 != 0)
         POST_MEM_WRITE(ARG3, sizeof(vki_uint_t));
      if ((ARG2 != 0) && (ARG3 != 0)) {
         vki_uint_t *numpsets = (vki_uint_t *) ARG3;
         POST_MEM_WRITE(ARG2, MIN(*numpsets, ARG6) * sizeof(vki_psetid_t));
      }
      break;
#  if defined(SOLARIS_PSET_GET_NAME)
   case VKI_PSET_GET_NAME:
      POST_MEM_WRITE(ARG3, VG_(strlen)((HChar *) ARG3) + 1);
      break;
#  endif /* SOLARIS_PSET_GET_NAME */
   case VKI_PSET_SETATTR:
      break;
   case VKI_PSET_GETATTR:
      POST_MEM_WRITE(ARG3, sizeof(vki_uint_t));
      break;
   case VKI_PSET_ASSIGN_FORCED:
      if (ARG4 != 0)
         POST_MEM_WRITE(ARG4, sizeof(vki_psetid_t));
      break;
   default:
      vg_assert(0);
      break;
   }
}

PRE(sys_resolvepath)
{
   /* int resolvepath(const char *path, char *buf, size_t bufsiz); */
   PRINT("sys_resolvepath ( %#lx(%s), %#lx, %lu )", ARG1, (HChar *) ARG1, ARG2,
         ARG3);
   PRE_REG_READ3(long, "resolvepath", const char *, path, char *, buf,
                 vki_size_t, bufsiz);

   PRE_MEM_RASCIIZ("resolvepath(path)", ARG1);
   PRE_MEM_WRITE("resolvepath(buf)", ARG2, ARG3);
}

POST(sys_resolvepath)
{
   POST_MEM_WRITE(ARG2, RES);
}

PRE(sys_lwp_mutex_timedlock)
{
   /* int lwp_mutex_timedlock(lwp_mutex_t *lp, timespec_t *tsp,
                              uintptr_t owner); */
   vki_lwp_mutex_t *lp = (vki_lwp_mutex_t *)ARG1;
   *flags |= SfMayBlock;
   PRINT("lwp_mutex_timedlock ( %#lx, %#lx, %#lx )", ARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "lwp_mutex_timedlock", lwp_mutex_t *, lp,
                 timespec_t *, tsp, uintptr_t, owner);

   PRE_FIELD_READ("lwp_mutex_timedlock(lp->mutex_flag)", lp->vki_mutex_flag);
   PRE_FIELD_READ("lwp_mutex_timedlock(lp->mutex_type)", lp->vki_mutex_type);
   PRE_FIELD_WRITE("lwp_mutex_timedlock(lp->mutex_owner)",
                   lp->vki_mutex_owner);
   PRE_FIELD_WRITE("lwp_mutex_timedlock(lp->mutex_ownerpid)",
                   lp->vki_mutex_ownerpid);
   PRE_FIELD_READ("lwp_mutex_timedlock(lp->mutex_lockw)", lp->vki_mutex_lockw);
   /*PRE_FIELD_WRITE("lwp_mutex_timedlock(lp->mutex_lockw)",
                     lp->vki_mutex_lockw);*/
   PRE_FIELD_READ("lwp_mutex_timedlock(lp->mutex_waiters)",
                  lp->vki_mutex_waiters);
   /*PRE_FIELD_WRITE("lwp_mutex_timedlock(lp->mutex_waiters)",
                     lp->vki_mutex_waiters);*/
   if (ARG2) {
      PRE_MEM_READ("lwp_mutex_timedlock(tsp)", ARG2, sizeof(vki_timespec_t));
      /*PRE_MEM_WRITE("lwp_mutex_timedlock(tsp)", ARG2,
                      sizeof(vki_timespec_t));*/
   }
}

POST(sys_lwp_mutex_timedlock)
{
   vki_lwp_mutex_t *lp = (vki_lwp_mutex_t *)ARG1;
   POST_FIELD_WRITE(lp->vki_mutex_owner);
   POST_FIELD_WRITE(lp->vki_mutex_ownerpid);
   POST_FIELD_WRITE(lp->vki_mutex_lockw);
   POST_FIELD_WRITE(lp->vki_mutex_waiters);
   if (ARG2)
      POST_MEM_WRITE(ARG2, sizeof(vki_timespec_t));
}

PRE(sys_lwp_rwlock_sys)
{
   /* int lwp_rwlock_sys(int subcode, lwp_rwlock_t *rwlp, timespec_t *tsp); */
   vki_lwp_rwlock_t *rwlp = (vki_lwp_rwlock_t *)ARG2;
   switch (ARG1 /*subcode*/) {
   case 0:
   case 1:
   case 2:
   case 3:
      *flags |= SfMayBlock;
      switch (ARG1 /*subcode*/) {
      case 0:
         PRINT("sys_lwp_rwlock ( %ld, %#lx, %#lx )", SARG1, ARG2, ARG3);
         PRE_REG_READ3(long, SC2("lwp_rwlock", "rdlock"), int, subcode,
                       lwp_rwlock_t *, rwlp, timespec_t *, tsp);
         break;
      case 1:
         PRINT("sys_lwp_rwlock ( %ld, %#lx, %#lx )", SARG1, ARG2, ARG3);
         PRE_REG_READ3(long, SC2("lwp_rwlock", "wrlock"), int, subcode,
                       lwp_rwlock_t *, rwlp, timespec_t *, tsp);
         break;
      case 2:
         PRINT("sys_lwp_rwlock ( %ld, %#lx )", SARG1, ARG2);
         PRE_REG_READ2(long, SC2("lwp_rwlock", "tryrdlock"), int, subcode,
                       lwp_rwlock_t *, rwlp);
         break;
      case 3:
         PRINT("sys_lwp_rwlock ( %ld, %#lx )", SARG1, ARG2);
         PRE_REG_READ2(long, SC2("lwp_rwlock", "trywrlock"), int, subcode,
                       lwp_rwlock_t *, rwlp);
         break;
      default:
         vg_assert(0);
         break;
      }

      PRE_FIELD_READ("lwp_rwlock(rwlp->rwlock_type)", rwlp->vki_rwlock_type);
      PRE_FIELD_READ("lwp_rwlock(rwlp->rwlock_readers)",
                     rwlp->vki_rwlock_readers);
      /*PRE_FIELD_WRITE("lwp_rwlock(rwlp->rwlock_readers)",
                        rwlp->vki_rwlock_readers);*/

      PRE_FIELD_READ("lwp_rwlock(rwlp->mutex.mutex_type)",
                     rwlp->mutex.vki_mutex_type);
      PRE_FIELD_WRITE("lwp_rwlock(rwlp->mutex.mutex_owner)",
                      rwlp->mutex.vki_mutex_owner);
      PRE_FIELD_WRITE("lwp_rwlock(rwlp->mutex.mutex_ownerpid)",
                      rwlp->mutex.vki_mutex_ownerpid);
      /* The mutex_lockw member is not really read by the kernel for this
         syscall but it seems better to mark it that way because when locking
         an rwlock the associated mutex has to be locked. */
      PRE_FIELD_READ("lwp_rwlock(rwlp->mutex.mutex_lockw)",
                     rwlp->mutex.vki_mutex_lockw);
      /*PRE_FIELD_WRITE("lwp_rwlock(rwlp->mutex.mutex_lockw)",
                        rwlp->mutex.vki_mutex_lockw);*/
      PRE_FIELD_READ("lwp_rwlock(rwlp->mutex.mutex_waiters)",
                     rwlp->mutex.vki_mutex_waiters);
      /*PRE_FIELD_WRITE("lwp_rwlock(rwlp->mutex.mutex_waiters)",
                        rwlp->mutex.vki_mutex_waiters);*/

      if ((ARG1 == 0 || ARG1 == 1) && ARG3)
         PRE_MEM_READ("lwp_rwlock(tsp)", ARG3, sizeof(vki_timespec_t));
      break;
   case 4:
      PRINT("sys_lwp_rwlock( %ld, %#lx )", SARG1, ARG2);
      PRE_REG_READ2(long, SC2("lwp_rwlock", "unlock"), int, subcode,
                    lwp_rwlock_t *, rwlp);
      PRE_FIELD_READ("lwp_rwlock(rwlp->mutex.mutex_type)",
                     rwlp->mutex.vki_mutex_type);
      PRE_FIELD_READ("lwp_rwlock(rwlp->rwlock_readers)",
                     rwlp->vki_rwlock_readers);
      /*PRE_FIELD_WRITE("lwp_rwlock(rwlp->rwlock_readers)",
                        rwlp->vki_rwlock_readers);*/
      break;
   default:
      VG_(unimplemented)("Syswrap of the lwp_rwlock_sys call with subcode %ld.",
                         SARG1);
      /*NOTREACHED*/
      break;
   }
}

POST(sys_lwp_rwlock_sys)
{
   vki_lwp_rwlock_t *rwlp = (vki_lwp_rwlock_t *)ARG2;
   switch (ARG1 /*subcode*/) {
   case 0:
   case 1:
   case 2:
   case 3:
      POST_FIELD_WRITE(rwlp->vki_rwlock_readers);
      POST_FIELD_WRITE(rwlp->vki_rwlock_owner);
      POST_FIELD_WRITE(rwlp->vki_rwlock_ownerpid);
      POST_FIELD_WRITE(rwlp->mutex.vki_mutex_lockw);
      POST_FIELD_WRITE(rwlp->mutex.vki_mutex_waiters);
      break;
   case 4:
      POST_FIELD_WRITE(rwlp->vki_rwlock_readers);
      break;
   default:
      vg_assert(0);
      break;
   }
}

PRE(sys_lwp_sema_timedwait)
{
   /* int lwp_sema_timedwait(lwp_sema_t *sema, timespec_t *timeout,
                             int check_park); */
   vki_lwp_sema_t *sema = (vki_lwp_sema_t*)ARG1;
   *flags |= SfMayBlock;
   PRINT("sys_lwp_sema_timewait ( %#lx, %#lx, %ld )", ARG1, ARG2, SARG3);
   PRE_REG_READ3(long, "lwp_sema_timedwait", lwp_sema_t *, sema,
                 timespec_t *, timeout, int, check_park);

   PRE_FIELD_READ("lwp_sema_timedwait(sema->type)", sema->vki_sema_type);
   PRE_FIELD_READ("lwp_sema_timedwait(sema->count)", sema->vki_sema_count);
   /*PRE_FIELD_WRITE("lwp_sema_timedwait(sema->count)",
                     sema->vki_sema_count);*/
   PRE_FIELD_READ("lwp_sema_timedwait(sema->waiters)", sema->vki_sema_waiters);
   /*PRE_FIELD_WRITE("lwp_sema_timedwait(sema->waiters)",
                     sema->vki_sema_waiters);*/
   if (ARG2) {
      PRE_MEM_READ("lwp_sema_timedwait(timeout)", ARG2,
                   sizeof(vki_timespec_t));
      /*PRE_MEM_WRITE("lwp_sema_timedwait(timeout)", ARG2,
                      sizeof(vki_timespec_t));*/
   }
}

POST(sys_lwp_sema_timedwait)
{
   vki_lwp_sema_t *sema = (vki_lwp_sema_t*)ARG1;
   POST_FIELD_WRITE(sema->vki_sema_count);
   POST_FIELD_WRITE(sema->vki_sema_waiters);
   if (ARG2)
      POST_MEM_WRITE(ARG2, sizeof(vki_timespec_t));
}

PRE(sys_zone)
{
   /* Kernel: long zone(int cmd, void *arg1, void *arg2, void *arg3,
                        void *arg4);
    */
   switch (ARG1 /*cmd*/) {
   case VKI_ZONE_CREATE:
      /* Libc: zoneid_t zone_create(const char *name, const char *root,
                                    const struct priv_set *privs,
                                    const char *rctls, size_t rctlsz,
                                    const char *zfs, size_t zfssz,
                                    int *extended_error, int match,
                                    int doi, const bslabel_t *label,
                                    int flags);
        Kernel: zoneid_t zone_create(zone_def *zd);
       */
      PRINT("sys_zone ( %ld, %#lx )", SARG1, ARG2);
      PRE_REG_READ2(long, SC2("zone", "create"), int, cmd,
                    vki_zone_def *, zd);

      vki_zone_def *zd = (vki_zone_def *) ARG2;
      PRE_FIELD_READ("zone(zd.zone_name)", zd->zone_name);
      PRE_FIELD_READ("zone(zd.zone_root)", zd->zone_root);
      PRE_FIELD_READ("zone(zd.zone_privs)", zd->zone_privs);
      PRE_FIELD_READ("zone(zd.zone_privssz)", zd->zone_privssz);
      PRE_FIELD_READ("zone(zd.rctlbuf)", zd->rctlbuf);
      PRE_FIELD_READ("zone(zd.rctlbufsz)", zd->rctlbufsz);
      PRE_FIELD_READ("zone(zd.zfsbuf)", zd->zfsbuf);
      PRE_FIELD_READ("zone(zd.zfsbufsz)", zd->zfsbufsz);
      PRE_FIELD_READ("zone(zd.extended_error)", zd->extended_error);
      PRE_FIELD_READ("zone(zd.match)", zd->match);
      PRE_FIELD_READ("zone(zd.doi)", zd->doi);
      PRE_FIELD_READ("zone(zd.label)", zd->label);
      PRE_FIELD_READ("zone(zd.flags)", zd->flags);

      if (ML_(safe_to_deref((void *)ARG2, sizeof(vki_zone_def)))) {
         if (zd->zone_name)
            PRE_MEM_RASCIIZ("zone(zd.zone_name)", (Addr) zd->zone_name);
         if (zd->zone_root)
            PRE_MEM_RASCIIZ("zone(zd.zone_root)", (Addr) zd->zone_root);
         PRE_MEM_READ("zone(zd.zone_privs)", (Addr) zd->zone_privs,
                      zd->zone_privssz);
         PRE_MEM_READ("zone(zd.rctlbuf)", (Addr) zd->rctlbuf,
                      zd->rctlbufsz);
         PRE_MEM_READ("zone(zd.zfsbuf)",
                      (Addr) zd->zfsbuf, zd->zfsbufsz);
         if (zd->label)
            PRE_MEM_READ("zone(zd.label)", (Addr) zd->label,
                         sizeof(vki_bslabel_t));
      }
      break;
   case VKI_ZONE_DESTROY:
      /* Libc: int zone_destroy(zoneid_t zoneid); */
      PRINT("sys_zone ( %ld, %ld )", SARG1, SARG2);
      PRE_REG_READ2(long, SC2("zone", "destroy"), int, cmd,
                    vki_zoneid_t, zoneid);
      break;
   case VKI_ZONE_GETATTR:
      /* Libc: ssize_t zone_getattr(zoneid_t zoneid, int attr,
                                    void *valp, size_t size);
       */
      PRINT("sys_zone ( %ld, %ld, %ld, %#lx, %ld )",
            SARG1, SARG2, SARG3, ARG4, SARG5);
      PRE_REG_READ5(long, SC2("zone", "getattr"), int, cmd,
                    vki_zoneid_t, zoneid, int, attr, void *, valp,
                    vki_size_t, size);
      PRE_MEM_WRITE("zone(valp)", ARG4, ARG5);
      break;
   case VKI_ZONE_ENTER:
      /* Libc: int zone_enter(zoneid_t zoneid); */
      PRINT("sys_zone ( %ld, %ld )", SARG1, SARG2);
      PRE_REG_READ2(long, SC2("zone", "enter"), int, cmd,
                    vki_zoneid_t, zoneid);
      break;
   case VKI_ZONE_LIST:
      /* Libc: int zone_list(zoneid_t *zonelist, uint_t *numzones); */
      PRINT("sys_zone ( %ld, %#lx, %#lx )", SARG1, ARG2, ARG3);
      PRE_REG_READ3(long, SC2("zone", "list"), int, cmd,
                    vki_zoneid_t *, zonelist, vki_uint_t *, numzones);

      PRE_MEM_WRITE("zone(numzones)", ARG3, sizeof(vki_uint_t));

      if (ML_(safe_to_deref((void *) ARG3, sizeof(vki_uint_t)))) {
         if (ARG2)
            PRE_MEM_WRITE("zone(zonelist)", ARG2,
                          *(vki_uint_t *) ARG3 * sizeof(vki_zoneid_t));
      }
      break;
   case VKI_ZONE_SHUTDOWN:
      /* Libc: int zone_shutdown(zoneid_t zoneid); */
      PRINT("sys_zone ( %ld, %ld )", SARG1, SARG2);
      PRE_REG_READ2(long, SC2("zone", "shutdown"), int, cmd,
                    vki_zoneid_t, zoneid);
      break;
   case VKI_ZONE_LOOKUP:
      /* Libc: zoneid_t zone_lookup(const char *name); */
      PRINT("sys_zone ( %ld, %#lx(%s) )", SARG1, ARG2, (HChar *) ARG2);
      PRE_REG_READ2(long, SC2("zone", "lookup"), int, cmd,
                    const char *, name);
      if (ARG2)
         PRE_MEM_RASCIIZ("zone(name)", ARG2);
      break;
   case VKI_ZONE_BOOT:
      /* Libc: int zone_boot(zoneid_t zoneid); */
      PRINT("sys_zone ( %ld, %ld )", SARG1, SARG2);
      PRE_REG_READ2(long, SC2("zone", "boot"), int, cmd,
                    vki_zoneid_t, zoneid);
      break;
   case VKI_ZONE_SETATTR:
      /* Libc: int zone_setattr(zoneid_t zoneid, int attr, void *valp,
                                size_t size);
       */
      PRINT("sys_zone ( %ld, %ld, %ld, %#lx, %lu )",
            SARG1, SARG2, SARG3, ARG4, ARG5);
      PRE_REG_READ5(long, SC2("zone", "setattr"), int, cmd,
                    vki_zoneid_t, zoneid, int, attr, void *, valp,
                    vki_size_t, size);
      PRE_MEM_READ("zone(valp)", ARG4, ARG5);
      break;
   case VKI_ZONE_ADD_DATALINK:
      /* Libc: int zone_add_datalink(zoneid_t zoneid,
                                     datalink_id_t linkid);
       */
      PRINT("sys_zone ( %ld, %ld, %ld )", SARG1, SARG2, SARG3);
      PRE_REG_READ3(long, SC2("zone", "add_datalink"), int, cmd,
                    vki_zoneid_t, zoneid, vki_datalink_id_t, linkid);
      break;
   case VKI_ZONE_DEL_DATALINK:
      /* Libc: int zone_remove_datalink(zoneid_t zoneid,
                                        datalink_id_t linkid);
       */
      PRINT("sys_zone ( %ld, %ld, %ld )", SARG1, SARG2, SARG3);
      PRE_REG_READ3(long, SC2("zone", "del_datalink"), int, cmd,
                    vki_zoneid_t, zoneid, vki_datalink_id_t, linkid);
      break;
   case VKI_ZONE_CHECK_DATALINK:
      /* Libc: int zone_check_datalink(zoneid_t *zoneidp,
                                       datalink_id_t linkid);
      */
      PRINT("sys_zone ( %ld, %#lx, %ld )", SARG1, ARG2, SARG3);
      PRE_REG_READ3(long, SC2("zone", "check_datalink"), int, cmd,
                    vki_zoneid_t *, zoneidp, vki_datalink_id_t, linkid);
      PRE_MEM_WRITE("zone(zoneidp)", ARG2, sizeof(vki_zoneid_t));
      break;
   case VKI_ZONE_LIST_DATALINK:
      /* Libc: int zone_list_datalink(zoneid_t zoneid, int *dlnump,
                                      datalink_id_t *linkids);
       */
      PRINT("sys_zone ( %ld, %ld, %#lx, %#lx )", SARG1, SARG2, ARG3, ARG4);
      PRE_REG_READ4(long, SC2("zone", "list_datalink"), int, cmd,
                    vki_zoneid_t, zoneid, int *, dlnump,
                    vki_datalink_id_t *, linkids);

      PRE_MEM_WRITE("zone(dlnump)", ARG3, sizeof(int));
      if (ML_(safe_to_deref((void *) ARG3, sizeof(int)))) {
         if (ARG4)
            PRE_MEM_WRITE("zone(linkids)", ARG4,
                          *(int *) ARG3 * sizeof(vki_datalink_id_t));
      }
      break;
#if defined(SOLARIS_ZONE_DEFUNCT)
   case VKI_ZONE_LIST_DEFUNCT:
      /* Libc: int zone_list_defunct(uint64_t *uniqidlist,
                                     uint_t *numzones);
       */
      PRINT("sys_zone ( %ld, %#lx, %#lx )", SARG1, ARG2, ARG3);
      PRE_REG_READ3(long, SC2("zone", "list_defunct"), int, cmd,
                    vki_uint64_t *, uniqidlist, vki_uint_t *, numzones);

      PRE_MEM_WRITE("zone(numzones)", ARG3, sizeof(vki_uint_t));

      if (ML_(safe_to_deref((void *) ARG3, sizeof(vki_uint_t)))) {
         if (ARG2)
            PRE_MEM_WRITE("zone(uniqidlist)", ARG2,
                          *(vki_uint_t *) ARG3 * sizeof(vki_uint64_t));
      }
      break;
   case VKI_ZONE_GETATTR_DEFUNCT:
      /* Libc: ssize_t zone_getattr_defunct(uint64_t uniqid, int attr,
                                            void *valp, size_t size);
         Kernel: ssize_t zone_getattr_defunct(uint64_t *uniqid, int attr,
                                              void *valp, size_t size);
       */
      PRINT("sys_zone ( %ld, %#lx, %ld, %#lx, %lu )",
            SARG1, ARG2, SARG3, ARG4, ARG5);
      PRE_REG_READ5(long, SC2("zone", "getattr_defunct"), int, cmd,
                    vki_uint64_t *, uniqid, int, attr,
                    void *, valp, vki_size_t, size);

      PRE_MEM_READ("zone(uniqid)", ARG2, sizeof(vki_uint64_t));
      PRE_MEM_WRITE("zone(valp)", ARG4, ARG5);
      break;
#endif /* SOLARIS_ZONE_DEFUNCT */
   default:
      VG_(unimplemented)("Syswrap of the zone call with cmd %ld.", SARG1);
      /*NOTREACHED*/
      break;
   }

}

POST(sys_zone)
{
   switch (ARG1 /*cmd*/) {
   case VKI_ZONE_CREATE:
   case VKI_ZONE_DESTROY:
      break;
   case VKI_ZONE_GETATTR:
      POST_MEM_WRITE(ARG4, MIN(RES, ARG5));
      break;
   case VKI_ZONE_ENTER:
      break;
   case VKI_ZONE_LIST:
      POST_MEM_WRITE(ARG2, *(vki_uint_t *) ARG3 * sizeof(vki_zoneid_t));
      break;
   case VKI_ZONE_SHUTDOWN:
   case VKI_ZONE_LOOKUP:
   case VKI_ZONE_BOOT:
   case VKI_ZONE_SETATTR:
   case VKI_ZONE_ADD_DATALINK:
   case VKI_ZONE_DEL_DATALINK:
      break;
   case VKI_ZONE_CHECK_DATALINK:
      POST_MEM_WRITE(ARG2, sizeof(vki_zoneid_t));
      break;
   case VKI_ZONE_LIST_DATALINK:
      POST_MEM_WRITE(ARG4, *(int *) ARG3 * sizeof(vki_datalink_id_t));
      break;
#if defined(SOLARIS_ZONE_DEFUNCT)
   case VKI_ZONE_LIST_DEFUNCT:
      POST_MEM_WRITE(ARG2, *(vki_uint_t *) ARG3 * sizeof(vki_uint64_t));
      break;
   case VKI_ZONE_GETATTR_DEFUNCT:
      POST_MEM_WRITE(ARG4, MIN(RES, ARG5));
      break;
#endif /* SOLARIS_ZONE_DEFUNCT */
   default:
      vg_assert(0);
      break;
   }
}

PRE(sys_getcwd)
{
   /* int getcwd(char *buf, size_t size); */
   /* Note: Generic getcwd() syswrap can't be used because it expects
      a different return value. */
   PRINT("sys_getcwd ( %#lx, %lu )", ARG1, ARG2);
   PRE_REG_READ2(long, "getcwd", char *, buf, vki_size_t, size);
   PRE_MEM_WRITE("getcwd(buf)", ARG1, ARG2);
}

POST(sys_getcwd)
{
   POST_MEM_WRITE(ARG1, VG_(strlen)((HChar*)ARG1) + 1);
}

PRE(sys_so_socket)
{
   /* int so_socket(int family, int type, int protocol, char *devpath,
                    int version); */
   PRINT("sys_so_socket ( %ld, %ld, %ld, %#lx(%s), %ld)", SARG1, SARG2, SARG3,
         ARG4, (HChar *) ARG4, SARG5);
   PRE_REG_READ5(long, "socket", int, family, int, type, int, protocol,
                 char *, devpath, int, version);
   if (ARG4)
      PRE_MEM_RASCIIZ("socket(devpath)", ARG4);
}

POST(sys_so_socket)
{
   SysRes r;
   r = ML_(generic_POST_sys_socket)(tid, VG_(mk_SysRes_Success)(RES));
   SET_STATUS_from_SysRes(r);
}

PRE(sys_so_socketpair)
{
   /* int so_socketpair(int sv[2]); */
   /* This syscall is used to connect two already created sockets together. */
   PRINT("sys_so_socketpair ( %#lx )", ARG1);
   PRE_REG_READ1(long, "socketpair", int *, sv);
   PRE_MEM_READ("socketpair(sv)", ARG1, 2 * sizeof(int));
   /*PRE_MEM_WRITE("socketpair(sv)", ARG1, 2 * sizeof(int));*/
   if (ML_(safe_to_deref)((void*)ARG1, 2 * sizeof(int))) {
      int *fds = (int*)ARG1;
      if (!ML_(fd_allowed)(fds[0], "socketpair", tid, False))
         SET_STATUS_Failure(VKI_EBADF);
      else if (!ML_(fd_allowed)(fds[1], "socketpair", tid, False))
         SET_STATUS_Failure(VKI_EBADF);
   }
}

POST(sys_so_socketpair)
{
   /* The kernel can return new file descriptors, in such a case we have to
      validate them. */
   int *fds = (int*)ARG1;
   POST_MEM_WRITE(ARG1, 2 * sizeof(int));
   if (!ML_(fd_allowed)(fds[0], "socketpair", tid, True))
      SET_STATUS_Failure(VKI_EMFILE);
   if (!ML_(fd_allowed)(fds[1], "socketpair", tid, True))
      SET_STATUS_Failure(VKI_EMFILE);
   if (FAILURE) {
      /* One or both of the file descriptors weren't allowed, close newly
         created file descriptors but don't close the already recorded
         ones. */
      if (!ML_(fd_recorded)(fds[0]))
         VG_(close)(fds[0]);
      if (!ML_(fd_recorded)(fds[1]))
         VG_(close)(fds[1]);
   }
   else if (VG_(clo_track_fds)) {
      /* Everything went better than expected, record the newly created file
         descriptors.  Note: If the kernel actually returns the original file
         descriptors, then ML_(record_fd_open_nameless) notices that these
         file descriptors have been already recorded. */
      ML_(record_fd_open_nameless)(tid, fds[0]);
      ML_(record_fd_open_nameless)(tid, fds[1]);
   }
}

PRE(sys_bind)
{
   /* int bind(int s, struct sockaddr *name, socklen_t namelen,
               int version); */
   PRINT("sys_bind ( %ld, %#lx, %lu, %ld )", SARG1, ARG2, ARG3, SARG4);
   PRE_REG_READ4(long, "bind", int, s, struct sockaddr *, name,
                 vki_socklen_t, namelen, int, version);
   ML_(generic_PRE_sys_bind)(tid, ARG1, ARG2, ARG3);
}

PRE(sys_listen)
{
   /* int listen(int s, int backlog, int version); */
   PRINT("sys_listen ( %ld, %ld, %ld )", SARG1, SARG2, SARG3);
   PRE_REG_READ3(long, "listen", int, s, int, backlog, int, version);
}

PRE(sys_accept)
{
#if defined(SOLARIS_NEW_ACCEPT_SYSCALL)
   /* int accept(int s, struct sockaddr *addr, socklen_t *addrlen,
                 int version, int flags); */
   *flags |= SfMayBlock;
   PRINT("sys_accept ( %ld, %#lx, %#lx, %ld, %ld )", SARG1, ARG2, ARG3, SARG4,
         SARG5);
   PRE_REG_READ5(long, "accept", int, s, struct sockaddr *, addr,
                 socklen_t *, addrlen, int, version, int, flags);
#else
   /* int accept(int s, struct sockaddr *addr, socklen_t *addrlen,
                 int version); */
   *flags |= SfMayBlock;
   PRINT("sys_accept ( %ld, %#lx, %#lx, %ld )", SARG1, ARG2, ARG3, SARG4);
   PRE_REG_READ4(long, "accept", int, s, struct sockaddr *, addr,
                 socklen_t *, addrlen, int, version);
#endif /* SOLARIS_NEW_ACCEPT_SYSCALL */
   ML_(generic_PRE_sys_accept)(tid, ARG1, ARG2, ARG3);
}

POST(sys_accept)
{
   SysRes r;
   r = ML_(generic_POST_sys_accept)(tid, VG_(mk_SysRes_Success)(RES),
                                    ARG1, ARG2, ARG3);
   SET_STATUS_from_SysRes(r);
}

PRE(sys_connect)
{
   /* int connect(int s, struct sockaddr *name, socklen_t namelen,
                  int version); */
   *flags |= SfMayBlock;
   PRINT("sys_connect ( %ld, %#lx, %lu, %ld )", SARG1, ARG2, ARG3, SARG4);
   PRE_REG_READ4(long, "connect", int, s, struct sockaddr *, name,
                 vki_socklen_t, namelen, int, version);
   ML_(generic_PRE_sys_connect)(tid, ARG1, ARG2, ARG3);
}

PRE(sys_shutdown)
{
   /* Kernel: int shutdown(int sock, int how, int version);
      Libc:   int shutdown(int sock, int how);
    */
   *flags |= SfMayBlock;
   PRINT("sys_shutdown ( %ld, %ld, %ld )", SARG1, SARG2, SARG3);
   PRE_REG_READ3(int, "shutdown", int, sock, int, how, int, version);

   /* Be strict. */
   if (!ML_(fd_allowed)(ARG1, "shutdown", tid, False))
      SET_STATUS_Failure(VKI_EBADF);
}

PRE(sys_recv)
{
   /* ssize_t recv(int s, void *buf, size_t len, int flags); */
   *flags |= SfMayBlock;
   PRINT("sys_recv ( %ld, %#lx, %lu, %ld )", SARG1, ARG2, ARG3, SARG4);
   PRE_REG_READ4(long, "recv", int, s, void *, buf, vki_size_t, len,
                 int, flags);
   ML_(generic_PRE_sys_recv)(tid, ARG1, ARG2, ARG3);
}

POST(sys_recv)
{
   ML_(generic_POST_sys_recv)(tid, RES, ARG1, ARG2, ARG3);
}

PRE(sys_recvfrom)
{
   /* ssize_t recvfrom(int s, void *buf, size_t len, int flags,
                       struct sockaddr *from, socklen_t *fromlen); */
   *flags |= SfMayBlock;
   PRINT("sys_recvfrom ( %ld, %#lx, %lu, %ld, %#lx, %#lx )", SARG1, ARG2, ARG3,
         SARG4, ARG5, ARG6);
   PRE_REG_READ6(long, "recvfrom", int, s, void *, buf, vki_size_t, len,
                 int, flags, struct sockaddr *, from, socklen_t *, fromlen);
   ML_(generic_PRE_sys_recvfrom)(tid, ARG1, ARG2, ARG3, ARG4, ARG5, ARG6);
}

POST(sys_recvfrom)
{
   ML_(generic_POST_sys_recvfrom)(tid, VG_(mk_SysRes_Success)(RES),
                                  ARG1, ARG2, ARG3, ARG4, ARG5, ARG6);
}

PRE(sys_recvmsg)
{
   /* ssize_t recvmsg(int s, struct msghdr *msg, int flags); */
   *flags |= SfMayBlock;
   PRINT("sys_recvmsg ( %ld, %#lx, %ld )", SARG1, ARG2, SARG3);
   PRE_REG_READ3(long, "recvmsg", int, s, struct msghdr *, msg, int, flags);
   ML_(generic_PRE_sys_recvmsg)(tid, "msg", (struct vki_msghdr*)ARG2);
}

POST(sys_recvmsg)
{
   ML_(generic_POST_sys_recvmsg)(tid, "msg", (struct vki_msghdr*)ARG2, RES);
}

PRE(sys_send)
{
   /* ssize_t send(int s, const void *msg, size_t len, int flags); */
   *flags |= SfMayBlock;
   PRINT("sys_send ( %ld, %#lx, %lu, %ld )", SARG1, ARG2, ARG3, SARG4);
   PRE_REG_READ4(long, "send", int, s, const void *, msg, vki_size_t, len,
                 int, flags);
   ML_(generic_PRE_sys_send)(tid, ARG1, ARG2, ARG3);
}

PRE(sys_sendmsg)
{
   /* ssize_t sendmsg(int s, const struct msghdr *msg, int flags); */
   *flags |= SfMayBlock;
   PRINT("sys_sendmsg ( %ld, %#lx, %ld )", SARG1, ARG2, SARG3);
   PRE_REG_READ3(long, "sendmsg", int, s, const struct msghdr *, msg,
                 int, flags);
   ML_(generic_PRE_sys_sendmsg)(tid, "msg", (struct vki_msghdr*)ARG2);
}

PRE(sys_sendto)
{
   /* ssize_t sendto(int s, const void *msg, size_t len, int flags,
                     const struct sockaddr *to, int tolen); */
   *flags |= SfMayBlock;
   PRINT("sys_sendto ( %ld, %#lx, %lu, %ld, %#lx, %ld )", SARG1, ARG2, ARG3,
         SARG4, ARG5, SARG6);
   PRE_REG_READ6(long, "sendto", int, s, const void *, msg, vki_size_t, len,
                 int, flags, const struct sockaddr *, to, int, tolen);
   ML_(generic_PRE_sys_sendto)(tid, ARG1, ARG2, ARG3, ARG4, ARG5, ARG6);
}

PRE(sys_getpeername)
{
   /* Kernel: int getpeername(int s, struct sockaddr *name,
                              socklen_t *namelen, int version);
      Libc:   int getpeername(int s, struct sockaddr *name,
                              socklen_t *namelen);
    */
   *flags |= SfMayBlock;
   PRINT("sys_getpeername ( %ld, %#lx, %#lx, %ld )",
         SARG1, ARG2, ARG3, SARG4);
   PRE_REG_READ4(long, "getpeername", int, s, struct vki_sockaddr *, name,
                 vki_socklen_t *, namelen, int, version);
   ML_(buf_and_len_pre_check)(tid, ARG2, ARG3, "getpeername(name)",
                              "getpeername(namelen)");

   /* Be strict. */
   if (!ML_(fd_allowed)(ARG1, "getpeername", tid, False))
      SET_STATUS_Failure(VKI_EBADF);
}

POST(sys_getpeername)
{
   ML_(buf_and_len_post_check)(tid, VG_(mk_SysRes_Success)(RES),
                               ARG2, ARG3, "getpeername(namelen)");
}

PRE(sys_getsockname)
{
   /* int getsockname(int s, struct sockaddr *name, socklen_t *namelen,
                      int version); */
   PRINT("sys_getsockname ( %ld, %#lx, %#lx, %ld )", SARG1, ARG2, ARG3, SARG4);
   PRE_REG_READ4(long, "getsockname", int, s, struct sockaddr *, name,
                 socklen_t *, namelen, int, version);
   ML_(generic_PRE_sys_getsockname)(tid, ARG1, ARG2, ARG3);
}

POST(sys_getsockname)
{
   ML_(generic_POST_sys_getsockname)(tid, VG_(mk_SysRes_Success)(RES),
                                     ARG1, ARG2, ARG3);
}

PRE(sys_getsockopt)
{
   /* int getsockopt(int s, int level, int optname, void *optval,
                     socklen_t *optlen, int version); */
   PRINT("sys_getsockopt ( %ld, %ld, %ld, %#lx, %#lx, %ld )", SARG1, SARG2,
         SARG3, ARG4, ARG5, SARG6);
   PRE_REG_READ6(long, "getsockopt", int, s, int, level, int, optname,
                 void *, optval, socklen_t *, option, int, version);
   if (ARG4)
      ML_(buf_and_len_pre_check)(tid, ARG4, ARG5, "getsockopt(optval)",
                                 "getsockopt(optlen)");
}

POST(sys_getsockopt)
{
   if (ARG4)
      ML_(buf_and_len_post_check)(tid, VG_(mk_SysRes_Success)(RES), ARG4,
                                  ARG5, "getsockopt(optlen_out)");
}

PRE(sys_setsockopt)
{
   /* int setsockopt(int s, int level, int optname, const void *optval,
                     socklen_t optlen, int version); */
   PRINT("sys_setsockopt ( %ld, %ld, %ld, %#lx, %lu, %ld )", SARG1, SARG2,
         SARG3, ARG4, ARG5, SARG6);
   PRE_REG_READ6(long, "setsockopt", int, s, int, level, int, optname,
                 const void *, optval, vki_socklen_t, optlen, int, version);
   ML_(generic_PRE_sys_setsockopt)(tid, ARG1, ARG2, ARG3, ARG4, ARG5);
}

PRE(sys_lwp_mutex_register)
{
   /* int lwp_mutex_register(lwp_mutex_t *mp, caddr_t uaddr); */
   vki_lwp_mutex_t *mp = (vki_lwp_mutex_t*)ARG1;
   PRINT("sys_lwp_mutex_register ( %#lx, %#lx )", ARG1, ARG2);
   PRE_REG_READ2(long, "lwp_mutex_register", lwp_mutex_t *, mp,
                 void *, uaddr);
   PRE_FIELD_READ("lwp_mutex_register(mp->mutex_type)", mp->vki_mutex_type);
}

PRE(sys_lwp_mutex_unlock)
{
   /* int lwp_mutex_unlock(lwp_mutex_t *lp); */
   /* see https://github.com/illumos/illumos-gate/blob/master/usr/src/uts/common/syscall/lwp_sobj.c#L3137-L3138
    * (illumos, obviously) */
   vki_lwp_mutex_t *lp = (vki_lwp_mutex_t*)ARG1;
   PRINT("sys_lwp_mutex_unlock ( %#lx )", ARG1);
   PRE_REG_READ1(int, "lwp_mutex_unlock", lwp_mutex_t *, lp);
   PRE_MEM_READ("lwp_mutex_unlock(lp)", (Addr)lp, sizeof(vki_lwp_mutex_t));
   PRE_MEM_WRITE("lwp_mutex_unlock(lp)", (Addr)lp, sizeof(vki_lwp_mutex_t));
}

POST(sys_lwp_mutex_unlock)
{
   POST_MEM_WRITE(ARG1, sizeof(vki_lwp_mutex_t));
}

PRE(sys_uucopy)
{
   /* int uucopy(const void *s1, void *s2, size_t n); */
   PRINT("sys_uucopy ( %#lx, %#lx, %lu )", ARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "uucopy", const void *, s1, void *, s2, vki_size_t, n);

   /* Stay away from V segments. */
   if (!ML_(valid_client_addr)(ARG1, ARG3, tid, "uucopy(s1)")) {
      SET_STATUS_Failure(VKI_EFAULT);
   }
   if (!ML_(valid_client_addr)(ARG2, ARG3, tid, "uucopy(s2)")) {
      SET_STATUS_Failure(VKI_EFAULT);
   }

   if (FAILURE)
      return;

   /* XXX This is actually incorrect, we should be able to copy undefined
      values through to their new destination. */
   PRE_MEM_READ("uucopy(s1)", ARG1, ARG3);
   PRE_MEM_WRITE("uucopy(s2)", ARG2, ARG3);
}

POST(sys_uucopy)
{
   POST_MEM_WRITE(ARG2, ARG3);
}

PRE(sys_umount2)
{
   /* int umount2(const char *file, int mflag); */
   *flags |= SfMayBlock;
   PRINT("sys_umount2 ( %#lx(%s), %ld )", ARG1, (HChar *) ARG1, SARG2);
   PRE_REG_READ2(long, "umount2", const char *, file, int, mflag);
   PRE_MEM_RASCIIZ("umount2(file)", ARG1);
}

PRE(fast_gethrtime)
{
   PRINT("fast_gethrtime ( )");
   PRE_REG_READ0(long, "gethrtime");
}

PRE(fast_gethrvtime)
{
   PRINT("fast_gethrvtime ( )");
   PRE_REG_READ0(long, "gethrvtime");
}

PRE(fast_gethrestime)
{
   /* Used by gettimeofday(3C). */
   PRINT("fast_gethrestime ( )");
   PRE_REG_READ0(long, "gethrestime");
}

PRE(fast_getlgrp)
{
   /* Fasttrap number shared between gethomelgroup() and getcpuid(). */
   PRINT("fast_getlgrp ( )");
   PRE_REG_READ0(long, "getlgrp");
}

#if defined(SOLARIS_GETHRT_FASTTRAP)
PRE(fast_gethrt)
{
   /* Used by gethrtime(3C) when tsp & tscp HWCAPs are present. */
   PRINT("fast_gethrt ( )");
   PRE_REG_READ0(long, "gethrt");
}

POST(fast_gethrt)
{
   if (RES == 0)
      return;

   VG_(change_mapping_ownership)(RES, False);
}
#endif /* SOLARIS_GETHRT_FASTTRAP */

#if defined(SOLARIS_GETZONEOFFSET_FASTTRAP)
PRE(fast_getzoneoffset)
{
   /* Returns kernel's time zone offset data. */
   PRINT("fast_getzoneoffset ( )");
   PRE_REG_READ0(long, "get_zone_offset");
}

POST(fast_getzoneoffset)
{
   if (RES == 0)
      return;

   VG_(change_mapping_ownership)(RES, False);
}
#endif /* SOLARIS_GETZONEOFFSET_FASTTRAP */

#undef PRE
#undef POST

/* ---------------------------------------------------------------------
   The Solaris syscall table
   ------------------------------------------------------------------ */

/* Add a Solaris-specific, arch-independent wrapper to a syscall table. */
#define SOLX_(sysno, name) \
   WRAPPER_ENTRY_X_(solaris, VG_SOLARIS_SYSNO_INDEX(sysno), name)
#define SOLXY(sysno, name) \
   WRAPPER_ENTRY_XY(solaris, VG_SOLARIS_SYSNO_INDEX(sysno), name)

#if defined(VGP_x86_solaris)
/* Add an x86-solaris specific wrapper to a syscall table. */
#define PLAX_(sysno, name) \
   WRAPPER_ENTRY_X_(x86_solaris, VG_SOLARIS_SYSNO_INDEX(sysno), name)
#define PLAXY(sysno, name) \
   WRAPPER_ENTRY_XY(x86_solaris, VG_SOLARIS_SYSNO_INDEX(sysno), name)

#elif defined(VGP_amd64_solaris)
/* Add an amd64-solaris specific wrapper to a syscall table. */
#define PLAX_(sysno, name) \
   WRAPPER_ENTRY_X_(amd64_solaris, VG_SOLARIS_SYSNO_INDEX(sysno), name)
#define PLAXY(sysno, name) \
   WRAPPER_ENTRY_XY(amd64_solaris, VG_SOLARIS_SYSNO_INDEX(sysno), name)

#else
#  error "Unknown platform"
#endif

/*
   GEN   : handlers are in syswrap-generic.c
   SOL   : handlers are in this file
      X_ : PRE handler only
      XY : PRE and POST handlers
*/

static SyscallTableEntry syscall_table[] = {
   SOLX_(__NR_exit,                 sys_exit),                  /*   1 */
#if defined(SOLARIS_SPAWN_SYSCALL)
   SOLX_(__NR_spawn,                sys_spawn),                 /*   2 */
#endif /* SOLARIS_SPAWN_SYSCALL */
   GENXY(__NR_read,                 sys_read),                  /*   3 */
   GENX_(__NR_write,                sys_write),                 /*   4 */
#if defined(SOLARIS_OLD_SYSCALLS)
   SOLXY(__NR_open,                 sys_open),                  /*   5 */
#endif /* SOLARIS_OLD_SYSCALLS */
   SOLXY(__NR_close,                sys_close),                 /*   6 */
   SOLX_(__NR_linkat,               sys_linkat),                /*   7 */
#if defined(SOLARIS_OLD_SYSCALLS)
   GENX_(__NR_link,                 sys_link),                  /*   9 */
   GENX_(__NR_unlink,               sys_unlink),                /*  10 */
#endif /* SOLARIS_OLD_SYSCALLS */
   SOLX_(__NR_symlinkat,            sys_symlinkat),             /*  11 */
   GENX_(__NR_chdir,                sys_chdir),                 /*  12 */
   SOLX_(__NR_time,                 sys_time),                  /*  13 */
#if defined(SOLARIS_OLD_SYSCALLS)
   GENX_(__NR_chmod,                sys_chmod),                 /*  15 */
   GENX_(__NR_chown,                sys_chown),                 /*  16 */
#endif /* SOLARIS_OLD_SYSCALLS */
   SOLX_(__NR_brk,                  sys_brk),                   /*  17 */
#if defined(SOLARIS_OLD_SYSCALLS)
   SOLXY(__NR_stat,                 sys_stat),                  /*  18 */
#endif /* SOLARIS_OLD_SYSCALLS */
   SOLX_(__NR_lseek,                sys_lseek),                 /*  19 */
   GENX_(__NR_getpid,               sys_getpid),                /*  20 */
   SOLXY(__NR_mount,                sys_mount),                 /*  21 */
   SOLXY(__NR_readlinkat,           sys_readlinkat),            /*  22 */
   GENX_(__NR_setuid,               sys_setuid),                /*  23 */
   GENX_(__NR_getuid,               sys_getuid),                /*  24 */
   SOLX_(__NR_stime,                sys_stime),                 /*  25 */
   GENX_(__NR_alarm,                sys_alarm),                 /*  27 */
#if defined(SOLARIS_OLD_SYSCALLS)
   SOLXY(__NR_fstat,                sys_fstat),                 /*  28 */
#endif /* SOLARIS_OLD_SYSCALLS */
   GENX_(__NR_pause,                sys_pause),                 /*  29 */
#if defined(SOLARIS_FREALPATHAT_SYSCALL)
   SOLXY(__NR_frealpathat,          sys_frealpathat),           /*  30 */
#endif /* SOLARIS_FREALPATHAT_SYSCALL */
   SOLX_(__NR_stty,                 sys_stty),                  /*  31 */
   SOLXY(__NR_gtty,                 sys_gtty),                  /*  32 */
#if defined(SOLARIS_OLD_SYSCALLS)
   GENX_(__NR_access,               sys_access),                /*  33 */
#endif /* SOLARIS_OLD_SYSCALLS */
   GENX_(__NR_kill,                 sys_kill),                  /*  37 */
   SOLX_(__NR_pgrpsys,              sys_pgrpsys),               /*  39 */
   SOLXY(__NR_pipe,                 sys_pipe),                  /*  42 */
   GENXY(__NR_times,                sys_times),                 /*  43 */
   SOLX_(__NR_faccessat,            sys_faccessat),             /*  45 */
   GENX_(__NR_setgid,               sys_setgid),                /*  46 */
   GENX_(__NR_getgid,               sys_getgid),                /*  47 */
   SOLXY(__NR_mknodat,              sys_mknodat),               /*  48 */
   SOLXY(__NR_sysi86,               sys_sysi86),                /*  50 */
   SOLXY(__NR_shmsys,               sys_shmsys),                /*  52 */
   SOLXY(__NR_semsys,               sys_semsys),                /*  53 */
   SOLXY(__NR_ioctl,                sys_ioctl),                 /*  54 */
   SOLX_(__NR_fchownat,             sys_fchownat),              /*  56 */
   SOLX_(__NR_fdsync,               sys_fdsync),                /*  58 */
   SOLX_(__NR_execve,               sys_execve),                /*  59 */
   GENX_(__NR_umask,                sys_umask),                 /*  60 */
   GENX_(__NR_chroot,               sys_chroot),                /*  61 */
   SOLXY(__NR_fcntl,                sys_fcntl),                 /*  62 */
   SOLX_(__NR_renameat,             sys_renameat),              /*  64 */
   SOLX_(__NR_unlinkat,             sys_unlinkat),              /*  65 */
   SOLXY(__NR_fstatat,              sys_fstatat),               /*  66 */
#if defined(VGP_x86_solaris)
   PLAXY(__NR_fstatat64,            sys_fstatat64),             /*  67 */
#endif /* VGP_x86_solaris */
   SOLXY(__NR_openat,               sys_openat),                /*  68 */
#if defined(VGP_x86_solaris)
   PLAXY(__NR_openat64,             sys_openat64),              /*  69 */
#endif /* VGP_x86_solaris */
   SOLXY(__NR_tasksys,              sys_tasksys),               /*  70 */
   SOLXY(__NR_getpagesizes,         sys_getpagesizes),          /*  73 */
   SOLXY(__NR_lwp_park,             sys_lwp_park),              /*  77 */
   SOLXY(__NR_sendfilev,            sys_sendfilev),             /*  78 */
#if defined(SOLARIS_LWP_NAME_SYSCALL)
   SOLXY(__NR_lwp_name,             sys_lwp_name),              /*  79 */
#endif /* SOLARIS_LWP_NAME_SYSCALL */
#if defined(SOLARIS_OLD_SYSCALLS)
   GENX_(__NR_rmdir,                sys_rmdir),                 /*  79 */
   GENX_(__NR_mkdir,                sys_mkdir),                 /*  80 */
#endif /* SOLARIS_OLD_SYSCALLS */
   GENXY(__NR_getdents,             sys_getdents),              /*  81 */
   SOLXY(__NR_privsys,              sys_privsys),               /*  82 */
   SOLXY(__NR_ucredsys,             sys_ucredsys),              /*  83 */
   SOLXY(__NR_sysfs,                sys_sysfs),                 /*  84 */
   SOLXY(__NR_getmsg,               sys_getmsg),                /*  85 */
   SOLX_(__NR_putmsg,               sys_putmsg),                /*  86 */
#if defined(SOLARIS_OLD_SYSCALLS)
   SOLXY(__NR_lstat,                sys_lstat),                 /*  88 */
   GENX_(__NR_symlink,              sys_symlink),               /*  89 */
   GENX_(__NR_readlink,             sys_readlink),              /*  90 */
#endif /* SOLARIS_OLD_SYSCALLS */
   GENX_(__NR_setgroups,            sys_setgroups),             /*  91 */
   GENXY(__NR_getgroups,            sys_getgroups),             /*  92 */
#if defined(SOLARIS_OLD_SYSCALLS)
   GENX_(__NR_fchmod,               sys_fchmod),                /*  93 */
   GENX_(__NR_fchown,               sys_fchown),                /*  94 */
#endif /* SOLARIS_OLD_SYSCALLS */
   SOLXY(__NR_sigprocmask,          sys_sigprocmask),           /*  95 */
   SOLX_(__NR_sigsuspend,           sys_sigsuspend),            /*  96 */
   GENXY(__NR_sigaltstack,          sys_sigaltstack),           /*  97 */
   SOLXY(__NR_sigaction,            sys_sigaction),             /*  98 */
   SOLXY(__NR_sigpending,           sys_sigpending),            /*  99 */
   SOLX_(__NR_context,              sys_getsetcontext),         /* 100 */
   SOLX_(__NR_fchmodat,             sys_fchmodat),              /* 101 */
   SOLX_(__NR_mkdirat,              sys_mkdirat),               /* 102 */
   SOLXY(__NR_statvfs,              sys_statvfs),               /* 103 */
   SOLXY(__NR_fstatvfs,             sys_fstatvfs),              /* 104 */
   SOLXY(__NR_nfssys,               sys_nfssys),                /* 106 */
   SOLXY(__NR_waitid,               sys_waitid),                /* 107 */
   SOLX_(__NR_sigsendsys,           sys_sigsendsys),            /* 108 */
#if defined(SOLARIS_UTIMESYS_SYSCALL)
   SOLX_(__NR_utimesys,             sys_utimesys),              /* 110 */
#endif /* SOLARIS_UTIMESYS_SYSCALL */
#if defined(SOLARIS_UTIMENSAT_SYSCALL)
   SOLX_(__NR_utimensat,            sys_utimensat),             /* 110 */
#endif /* SOLARIS_UTIMENSAT_SYSCALL */
   SOLXY(__NR_sigresend,            sys_sigresend),             /* 111 */
   SOLXY(__NR_priocntlsys,          sys_priocntlsys),           /* 112 */
   SOLX_(__NR_pathconf,             sys_pathconf),              /* 113 */
   SOLX_(__NR_mmap,                 sys_mmap),                  /* 115 */
   GENXY(__NR_mprotect,             sys_mprotect),              /* 116 */
   GENXY(__NR_munmap,               sys_munmap),                /* 117 */
   GENX_(__NR_fchdir,               sys_fchdir),                /* 120 */
   GENXY(__NR_readv,                sys_readv),                 /* 121 */
   GENX_(__NR_writev,               sys_writev),                /* 122 */
#if defined(SOLARIS_UUIDSYS_SYSCALL)
   SOLXY(__NR_uuidsys,              sys_uuidsys),               /* 124 */
#endif /* SOLARIS_UUIDSYS_SYSCALL */
#if defined(HAVE_MREMAP)
   GENX_(__NR_mremap,               sys_mremap),                /* 126 */
#endif /* HAVE_MREMAP */
   SOLX_(__NR_mmapobj,              sys_mmapobj),               /* 127 */
   GENX_(__NR_setrlimit,            sys_setrlimit),             /* 128 */
   GENXY(__NR_getrlimit,            sys_getrlimit),             /* 129 */
#if defined(SOLARIS_OLD_SYSCALLS)
   GENX_(__NR_lchown,               sys_lchown),                /* 130 */
#endif /* SOLARIS_OLD_SYSCALLS */
   SOLX_(__NR_memcntl,              sys_memcntl),               /* 131 */
   SOLXY(__NR_getpmsg,              sys_getpmsg),               /* 132 */
   SOLX_(__NR_putpmsg,              sys_putpmsg),               /* 133 */
#if defined(SOLARIS_OLD_SYSCALLS)
   SOLX_(__NR_rename,               sys_rename),                /* 134 */
#endif /* SOLARIS_OLD_SYSCALLS */
   SOLXY(__NR_uname,                sys_uname),                 /* 135 */
   SOLX_(__NR_setegid,              sys_setegid),               /* 136 */
   SOLX_(__NR_sysconfig,            sys_sysconfig),             /* 137 */
   SOLXY(__NR_systeminfo,           sys_systeminfo),            /* 139 */
   SOLX_(__NR_seteuid,              sys_seteuid),               /* 141 */
   SOLX_(__NR_forksys,              sys_forksys),               /* 142 */
#if defined(SOLARIS_GETRANDOM_SYSCALL)
   SOLXY(__NR_getrandom,            sys_getrandom),             /* 143 */
#endif /* SOLARIS_GETRANDOM_SYSCALL */
   SOLXY(__NR_sigtimedwait,         sys_sigtimedwait),          /* 144 */
   SOLX_(__NR_yield,                sys_yield),                 /* 146 */
   SOLXY(__NR_lwp_sema_post,        sys_lwp_sema_post),         /* 148 */
   SOLXY(__NR_lwp_sema_trywait,     sys_lwp_sema_trywait),      /* 149 */
   SOLX_(__NR_lwp_detach,           sys_lwp_detach),            /* 150 */
   SOLXY(__NR_modctl,               sys_modctl),                /* 152 */
   SOLX_(__NR_fchroot,              sys_fchroot),               /* 153 */
#if defined(SOLARIS_SYSTEM_STATS_SYSCALL)
   SOLX_(__NR_system_stats,         sys_system_stats),          /* 154 */
#endif /* SOLARIS_SYSTEM_STATS_SYSCALL */
   SOLXY(__NR_gettimeofday,         sys_gettimeofday),          /* 156 */
   GENXY(__NR_getitimer,            sys_getitimer),             /* 157 */
   GENXY(__NR_setitimer,            sys_setitimer),             /* 158 */
   SOLX_(__NR_lwp_create,           sys_lwp_create),            /* 159 */
   SOLX_(__NR_lwp_exit,             sys_lwp_exit),              /* 160 */
   SOLX_(__NR_lwp_suspend,          sys_lwp_suspend),           /* 161 */
   SOLX_(__NR_lwp_continue,         sys_lwp_continue),          /* 162 */
#if defined(SOLARIS_LWP_SIGQUEUE_SYSCALL)
   SOLXY(__NR_lwp_sigqueue,         sys_lwp_sigqueue),          /* 163 */
#else
   SOLXY(__NR_lwp_kill,             sys_lwp_kill),              /* 163 */
#endif /* SOLARIS_LWP_SIGQUEUE_SYSCALL */
   SOLX_(__NR_lwp_self,             sys_lwp_self),              /* 164 */
   SOLX_(__NR_lwp_sigmask,          sys_lwp_sigmask),           /* 165 */
   SOLX_(__NR_lwp_private,          sys_lwp_private),           /* 166 */
   SOLXY(__NR_lwp_wait,             sys_lwp_wait),              /* 167 */
   SOLXY(__NR_lwp_mutex_wakeup,     sys_lwp_mutex_wakeup),      /* 168 */
   SOLXY(__NR_lwp_cond_wait,        sys_lwp_cond_wait),         /* 170 */
   SOLXY(__NR_lwp_cond_signal,      sys_lwp_cond_signal),       /* 171 */
   SOLX_(__NR_lwp_cond_broadcast,   sys_lwp_cond_broadcast),    /* 172 */
   SOLXY(__NR_pread,                sys_pread),                 /* 173 */
   SOLX_(__NR_pwrite,               sys_pwrite),                /* 174 */
#if defined(VGP_x86_solaris)
   PLAX_(__NR_llseek,               sys_llseek32),              /* 175 */
#endif /* VGP_x86_solaris */
   SOLXY(__NR_lgrpsys,              sys_lgrpsys),               /* 180 */
   SOLXY(__NR_rusagesys,            sys_rusagesys),             /* 181 */
   SOLXY(__NR_port,                 sys_port),                  /* 182 */
   SOLXY(__NR_pollsys,              sys_pollsys),               /* 183 */
   SOLXY(__NR_labelsys,             sys_labelsys),              /* 184 */
   SOLXY(__NR_acl,                  sys_acl),                   /* 185 */
   SOLXY(__NR_auditsys,             sys_auditsys),              /* 186 */
   SOLX_(__NR_p_online,             sys_p_online),              /* 189 */
   SOLX_(__NR_sigqueue,             sys_sigqueue),              /* 190 */
   SOLXY(__NR_clock_gettime,        sys_clock_gettime),         /* 191 */
   SOLX_(__NR_clock_settime,        sys_clock_settime),         /* 192 */
   SOLXY(__NR_clock_getres,         sys_clock_getres),          /* 193 */
   SOLXY(__NR_timer_create,         sys_timer_create),          /* 194 */
   SOLX_(__NR_timer_delete,         sys_timer_delete),          /* 195 */
   SOLXY(__NR_timer_settime,        sys_timer_settime),         /* 196 */
   SOLXY(__NR_timer_gettime,        sys_timer_gettime),         /* 197 */
   SOLX_(__NR_timer_getoverrun,     sys_timer_getoverrun),      /* 198 */
   GENXY(__NR_nanosleep,            sys_nanosleep),             /* 199 */
   SOLXY(__NR_facl,                 sys_facl),                  /* 200 */
   SOLXY(__NR_door,                 sys_door),                  /* 201 */
   GENX_(__NR_setreuid,             sys_setreuid),              /* 202 */
   GENX_(__NR_setregid,             sys_setregid),              /* 202 */
   SOLXY(__NR_schedctl,             sys_schedctl),              /* 206 */
   SOLXY(__NR_pset,                 sys_pset),                  /* 207 */
   SOLXY(__NR_resolvepath,          sys_resolvepath),           /* 209 */
   SOLXY(__NR_lwp_mutex_timedlock,  sys_lwp_mutex_timedlock),   /* 210 */
   SOLXY(__NR_lwp_sema_timedwait,   sys_lwp_sema_timedwait),    /* 211 */
   SOLXY(__NR_lwp_rwlock_sys,       sys_lwp_rwlock_sys),        /* 212 */
#if defined(VGP_x86_solaris)
   GENXY(__NR_getdents64,           sys_getdents64),            /* 213 */
   PLAX_(__NR_mmap64,               sys_mmap64),                /* 214 */
#if defined(SOLARIS_OLD_SYSCALLS)
   PLAXY(__NR_stat64,               sys_stat64),                /* 215 */
   PLAXY(__NR_lstat64,              sys_lstat64),               /* 216 */
   PLAXY(__NR_fstat64,              sys_fstat64),               /* 217 */
#endif /* SOLARIS_OLD_SYSCALLS */
   PLAXY(__NR_statvfs64,            sys_statvfs64),             /* 218 */
   PLAXY(__NR_fstatvfs64,           sys_fstatvfs64),            /* 219 */
#endif /* VGP_x86_solaris */
#if defined(VGP_x86_solaris)
   PLAX_(__NR_setrlimit64,          sys_setrlimit64),           /* 220 */
   PLAXY(__NR_getrlimit64,          sys_getrlimit64),           /* 221 */
   PLAXY(__NR_pread64,              sys_pread64),               /* 222 */
   PLAX_(__NR_pwrite64,             sys_pwrite64),              /* 223 */
#if defined(SOLARIS_OLD_SYSCALLS)
   PLAXY(__NR_open64,               sys_open64),                /* 225 */
#endif /* SOLARIS_OLD_SYSCALLS */
#endif /* VGP_x86_solaris */
   SOLXY(__NR_zone,                 sys_zone),                  /* 227 */
   SOLXY(__NR_getcwd,               sys_getcwd),                /* 229 */
   SOLXY(__NR_so_socket,            sys_so_socket),             /* 230 */
   SOLXY(__NR_so_socketpair,        sys_so_socketpair),         /* 231 */
   SOLX_(__NR_bind,                 sys_bind),                  /* 232 */
   SOLX_(__NR_listen,               sys_listen),                /* 233 */
   SOLXY(__NR_accept,               sys_accept),                /* 234 */
   SOLX_(__NR_connect,              sys_connect),               /* 235 */
   SOLX_(__NR_shutdown,             sys_shutdown),              /* 236 */
   SOLXY(__NR_recv,                 sys_recv),                  /* 237 */
   SOLXY(__NR_recvfrom,             sys_recvfrom),              /* 238 */
   SOLXY(__NR_recvmsg,              sys_recvmsg),               /* 239 */
   SOLX_(__NR_send,                 sys_send),                  /* 240 */
   SOLX_(__NR_sendmsg,              sys_sendmsg),               /* 241 */
   SOLX_(__NR_sendto,               sys_sendto),                /* 242 */
   SOLXY(__NR_getpeername,          sys_getpeername),           /* 243 */
   SOLXY(__NR_getsockname,          sys_getsockname),           /* 244 */
   SOLXY(__NR_getsockopt,           sys_getsockopt),            /* 245 */
   SOLX_(__NR_setsockopt,           sys_setsockopt),            /* 246 */
   SOLXY(__NR_lwp_mutex_unlock,     sys_lwp_mutex_unlock),      /* 250 */
   SOLX_(__NR_lwp_mutex_register,   sys_lwp_mutex_register),    /* 252 */
   SOLXY(__NR_uucopy,               sys_uucopy),                /* 254 */
   SOLX_(__NR_umount2,              sys_umount2)                /* 255 */
};

static SyscallTableEntry fasttrap_table[] = {
   SOLX_(__NR_gethrtime,            fast_gethrtime),            /*   3 */
   SOLX_(__NR_gethrvtime,           fast_gethrvtime),           /*   4 */
   SOLX_(__NR_gethrestime,          fast_gethrestime),          /*   5 */
   SOLX_(__NR_getlgrp,              fast_getlgrp)               /*   6 */
#if defined(SOLARIS_GETHRT_FASTTRAP)
   ,
   SOLXY(__NR_gethrt,               fast_gethrt)                /*   7 */
#endif /* SOLARIS_GETHRT_FASTTRAP */
#if defined(SOLARIS_GETZONEOFFSET_FASTTRAP)
   ,
   SOLXY(__NR_getzoneoffset,        fast_getzoneoffset)         /*   8 */
#endif /* SOLARIS_GETZONEOFFSET_FASTTRAP */

};

SyscallTableEntry *ML_(get_solaris_syscall_entry)(UInt sysno)
{
   const UInt syscall_table_size
      = sizeof(syscall_table) / sizeof(syscall_table[0]);
   const UInt fasttrap_table_size
      = sizeof(fasttrap_table) / sizeof(fasttrap_table[0]);

   SyscallTableEntry *table;
   Int size;

   switch (VG_SOLARIS_SYSNO_CLASS(sysno)) {
   case VG_SOLARIS_SYSCALL_CLASS_CLASSIC:
      table = syscall_table;
      size = syscall_table_size;
      break;
   case VG_SOLARIS_SYSCALL_CLASS_FASTTRAP:
      table = fasttrap_table;
      size = fasttrap_table_size;
      break;
   default:
      vg_assert(0);
      break;
   }
   sysno = VG_SOLARIS_SYSNO_INDEX(sysno);
   if (sysno < size) {
      SyscallTableEntry *sys = &table[sysno];
      if (!sys->before)
         return NULL; /* no entry */
      return sys;
   }

   /* Can't find a wrapper. */
   return NULL;
}

#endif // defined(VGO_solaris)

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
