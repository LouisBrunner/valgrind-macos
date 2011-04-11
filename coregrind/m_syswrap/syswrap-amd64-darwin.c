
/*--------------------------------------------------------------------*/
/*--- Darwin-specific syscalls, etc.        syswrap-amd64-darwin.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2005-2010 Apple Inc.
      Greg Parker  gparker@apple.com

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

#if defined(VGP_amd64_darwin)

#include "pub_core_basics.h"
#include "pub_core_vki.h"
#include "pub_core_libcsetjmp.h"   // to keep _threadstate.h happy
#include "pub_core_threadstate.h"
#include "pub_core_aspacemgr.h"
#include "pub_core_xarray.h"
#include "pub_core_clientstate.h"
#include "pub_core_debuglog.h"
#include "pub_core_debuginfo.h"    // VG_(di_notify_*)
#include "pub_core_transtab.h"     // VG_(discard_translations)
#include "pub_core_libcbase.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcfile.h"
#include "pub_core_libcprint.h"
#include "pub_core_libcproc.h"
#include "pub_core_libcsignal.h"
#include "pub_core_mallocfree.h"
#include "pub_core_options.h"
#include "pub_core_scheduler.h"
#include "pub_core_sigframe.h"      // For VG_(sigframe_destroy)()
#include "pub_core_signals.h"
#include "pub_core_syscall.h"
#include "pub_core_syswrap.h"
#include "pub_core_tooliface.h"

#include "priv_types_n_macros.h"
#include "priv_syswrap-generic.h"   /* for decls of generic wrappers */
#include "priv_syswrap-darwin.h"    /* for decls of darwin-ish wrappers */
#include "priv_syswrap-main.h"


#include <mach/mach.h>

static void x86_thread_state64_from_vex(x86_thread_state64_t *mach, 
                                        VexGuestAMD64State *vex)
{
    mach->__rax = vex->guest_RAX;
    mach->__rbx = vex->guest_RBX;
    mach->__rcx = vex->guest_RCX;
    mach->__rdx = vex->guest_RDX;
    mach->__rdi = vex->guest_RDI;
    mach->__rsi = vex->guest_RSI;
    mach->__rbp = vex->guest_RBP;
    mach->__rsp = vex->guest_RSP;
    mach->__rflags = LibVEX_GuestAMD64_get_rflags(vex);
    mach->__rip = vex->guest_RIP;
    mach->__r8  = vex->guest_R8;
    mach->__r9  = vex->guest_R9;
    mach->__r10 = vex->guest_R10;
    mach->__r11 = vex->guest_R11;
    mach->__r12 = vex->guest_R12;
    mach->__r13 = vex->guest_R13;
    mach->__r14 = vex->guest_R14;
    mach->__r15 = vex->guest_R15;
    /* GrP fixme
    mach->__cs = vex->guest_CS;
    mach->__fs = vex->guest_FS;
    mach->__gs = vex->guest_GS;
    */
}


static void x86_float_state64_from_vex(x86_float_state64_t *mach, 
                                       VexGuestAMD64State *vex)
{
   // DDD: #warning GrP fixme fp state

   VG_(memcpy)(&mach->__fpu_xmm0, &vex->guest_XMM0, 16 * sizeof(mach->__fpu_xmm0));
}


void thread_state_from_vex(thread_state_t mach_generic, 
                           thread_state_flavor_t flavor, 
                           mach_msg_type_number_t count, 
                           VexGuestArchState *vex_generic)
{
   VexGuestAMD64State *vex = (VexGuestAMD64State *)vex_generic;

   switch (flavor) {
   case x86_THREAD_STATE64:
      vg_assert(count == x86_THREAD_STATE64_COUNT);
      x86_thread_state64_from_vex((x86_thread_state64_t *)mach_generic, vex);
      break;

   case x86_FLOAT_STATE64:
      vg_assert(count == x86_FLOAT_STATE64_COUNT);
      x86_float_state64_from_vex((x86_float_state64_t *)mach_generic, vex);
      break;
       
   default:
      vg_assert(0);
   }
}


static void x86_thread_state64_to_vex(const x86_thread_state64_t *mach, 
                                      VexGuestAMD64State *vex)
{
   LibVEX_GuestAMD64_initialise(vex);
   vex->guest_RAX = mach->__rax;
   vex->guest_RBX = mach->__rbx;
   vex->guest_RCX = mach->__rcx;
   vex->guest_RDX = mach->__rdx;
   vex->guest_RDI = mach->__rdi;
   vex->guest_RSI = mach->__rsi;
   vex->guest_RBP = mach->__rbp;
   vex->guest_RSP = mach->__rsp;
   // DDD: #warning GrP fixme eflags
   vex->guest_RIP = mach->__rip;
   vex->guest_R8  = mach->__r8;
   vex->guest_R9  = mach->__r9;
   vex->guest_R10 = mach->__r10;
   vex->guest_R11 = mach->__r11;
   vex->guest_R12 = mach->__r12;
   vex->guest_R13 = mach->__r13;
   vex->guest_R14 = mach->__r14;
   vex->guest_R15 = mach->__r15;
   /* GrP fixme 
   vex->guest_CS = mach->__cs;
   vex->guest_FS = mach->__fs;
   vex->guest_GS = mach->__gs;
   */
}

static void x86_float_state64_to_vex(const x86_float_state64_t *mach, 
                                     VexGuestAMD64State *vex)
{
   // DDD: #warning GrP fixme fp state

   VG_(memcpy)(&vex->guest_XMM0, &mach->__fpu_xmm0, 16 * sizeof(mach->__fpu_xmm0));
}


void thread_state_to_vex(const thread_state_t mach_generic, 
                         thread_state_flavor_t flavor, 
                         mach_msg_type_number_t count, 
                         VexGuestArchState *vex_generic)
{
   VexGuestAMD64State *vex = (VexGuestAMD64State *)vex_generic;
   
   switch(flavor) {
   case x86_THREAD_STATE64:
      vg_assert(count == x86_THREAD_STATE64_COUNT);
      x86_thread_state64_to_vex((const x86_thread_state64_t*)mach_generic,vex);
      break;
   case x86_FLOAT_STATE64:
      vg_assert(count == x86_FLOAT_STATE64_COUNT);
      x86_float_state64_to_vex((const x86_float_state64_t*)mach_generic,vex);
      break;

   default:
      vg_assert(0);
      break;
   }
}


ThreadState *build_thread(const thread_state_t state, 
                          thread_state_flavor_t flavor, 
                          mach_msg_type_number_t count)
{
   ThreadId tid = VG_(alloc_ThreadState)();
   ThreadState *tst = VG_(get_ThreadState)(tid);
    
   vg_assert(flavor == x86_THREAD_STATE64);
   vg_assert(count == x86_THREAD_STATE64_COUNT);

   // Initialize machine registers

   thread_state_to_vex(state, flavor, count, &tst->arch.vex);

   I_die_here;
   // GrP fixme signals, sig_mask, tmp_sig_mask, os_state.parent

   find_stack_segment(tid, tst->arch.vex.guest_RSP);

   return tst;
}


// Edit the thread state to send to the real kernel.
// The real thread will run start_thread_NORETURN(tst)
// on a separate non-client stack.
void hijack_thread_state(thread_state_t mach_generic, 
                         thread_state_flavor_t flavor, 
                         mach_msg_type_number_t count, 
                         ThreadState *tst)
{
   x86_thread_state64_t *mach = (x86_thread_state64_t *)mach_generic;
   char *stack;

   vg_assert(flavor == x86_THREAD_STATE64);
   vg_assert(count == x86_THREAD_STATE64_COUNT);

   stack = (char *)allocstack(tst->tid);
   stack -= 64+320;                       // make room for top frame
   memset(stack, 0, 64+320);              // ...and clear it
   *(uintptr_t *)stack = 0;               // push fake return address

   mach->__rdi = (uintptr_t)tst;          // arg1 = tst
   mach->__rip = (uintptr_t)&start_thread_NORETURN;
   mach->__rsp = (uintptr_t)stack;
}


/* Call f(arg1), but first switch stacks, using 'stack' as the new
   stack, and use 'retaddr' as f's return-to address.  Also, clear all
   the integer registers before entering f.*/
__attribute__((noreturn))
void call_on_new_stack_0_1 ( Addr stack,
			     Addr retaddr,
			     void (*f)(Word),
                             Word arg1 );
// %rdi == stack (must be 16-byte aligned)
// %rsi == retaddr
// %rdx == f
// %rcx == arg1
asm(
".globl _call_on_new_stack_0_1\n"
"_call_on_new_stack_0_1:\n"
"   movq  %rsp, %rbp\n"     // remember old stack pointer
"   movq  %rdi, %rsp\n"     // set new stack
"   movq  %rcx, %rdi\n"     // set arg1
"   pushq %rsi\n"           // retaddr to new stack
"   pushq %rdx\n"           // f to new stack
"   movq $0, %rax\n"        // zero all other GP regs
"   movq $0, %rbx\n"
"   movq $0, %rcx\n"
"   movq $0, %rdx\n"
"   movq $0, %rsi\n"
"   movq $0, %rbp\n"
"   movq $0, %r8\n"
"   movq $0, %r9\n"
"   movq $0, %r10\n"
"   movq $0, %r11\n"
"   movq $0, %r12\n"
"   movq $0, %r13\n"
"   movq $0, %r14\n"
"   movq $0, %r15\n"
"   ret\n"                 // jump to f
"   ud2\n"                 // should never get here
);

asm(
".globl _pthread_hijack_asm\n"
"_pthread_hijack_asm:\n"
"   movq %rsp,%rbp\n"
"   push $0\n"    // alignment pad
"   push %rbp\n"  // original sp
                  // other values stay where they are in registers
"   push $0\n"    // fake return address
"   jmp _pthread_hijack\n"
    );



void pthread_hijack(Addr self, Addr kport, Addr func, Addr func_arg, 
                    Addr stacksize, Addr flags, Addr sp)
{
   vki_sigset_t blockall;
   ThreadState *tst = (ThreadState *)func_arg;
   VexGuestAMD64State *vex = &tst->arch.vex;

   // VG_(printf)("pthread_hijack pthread %p, machthread %p, func %p, arg %p, stack %p, flags %p, stack %p\n", self, kport, func, func_arg, stacksize, flags, sp);

   // Wait for parent thread's permission.
   // The parent thread holds V's lock on our behalf.
   semaphore_wait(tst->os_state.child_go);

   /* Start the thread with all signals blocked.  VG_(scheduler) will
      set the mask correctly when we finally get there. */
   VG_(sigfillset)(&blockall);
   VG_(sigprocmask)(VKI_SIG_SETMASK, &blockall, NULL);

   // Set thread's registers
   // Do this FIRST because some code below tries to collect a backtrace, 
   // which requires valid register data.
   LibVEX_GuestAMD64_initialise(vex);
   vex->guest_RIP = pthread_starter;
   vex->guest_RDI = self;
   vex->guest_RSI = kport;
   vex->guest_RDX = func;
   vex->guest_RCX = tst->os_state.func_arg;
   vex->guest_R8  = stacksize;
   vex->guest_R9  = flags;
   vex->guest_RSP = sp;

   // Record thread's stack and Mach port and pthread struct
   tst->os_state.pthread = self;
   tst->os_state.lwpid = kport;
   record_named_port(tst->tid, kport, MACH_PORT_RIGHT_SEND, "thread-%p");

   if ((flags & 0x01000000) == 0) {
      // kernel allocated stack - needs mapping
      Addr stack = VG_PGROUNDUP(sp) - stacksize;
      tst->client_stack_highest_word = stack+stacksize;
      tst->client_stack_szB = stacksize;

      // pthread structure
      ML_(notify_core_and_tool_of_mmap)(
            stack+stacksize, pthread_structsize, 
            VKI_PROT_READ|VKI_PROT_WRITE, VKI_MAP_PRIVATE, -1, 0);
      // stack contents
      ML_(notify_core_and_tool_of_mmap)(
            stack, stacksize, 
            VKI_PROT_READ|VKI_PROT_WRITE, VKI_MAP_PRIVATE, -1, 0);
      // guard page
      ML_(notify_core_and_tool_of_mmap)(
            stack-VKI_PAGE_SIZE, VKI_PAGE_SIZE,
            0, VKI_MAP_PRIVATE, -1, 0);
   } else {
      // client allocated stack
      find_stack_segment(tst->tid, sp);
   }
   ML_(sync_mappings)("after", "pthread_hijack", 0);

   // DDD: should this be here rather than in POST(sys_bsdthread_create)?
   // But we don't have ptid here...
   //VG_TRACK ( pre_thread_ll_create, ptid, tst->tid );

   // Tell parent thread's POST(sys_bsdthread_create) that we're done 
   // initializing registers and mapping memory.
   semaphore_signal(tst->os_state.child_done);
   // LOCK IS GONE BELOW THIS POINT

   // Go!
   call_on_new_stack_0_1(tst->os_state.valgrind_stack_init_SP, 0, 
                         start_thread_NORETURN, (Word)tst);

   /*NOTREACHED*/
   vg_assert(0);
}



asm(
".globl _wqthread_hijack_asm\n"
"_wqthread_hijack_asm:\n"
"   movq %rsp,%r9\n"  // original sp
                      // other values stay where they are in registers
"   push $0\n"        // fake return address
"   jmp _wqthread_hijack\n"
    );


/*  wqthread note: The kernel may create or destroy pthreads in the 
    wqthread pool at any time with no userspace interaction, 
    and wqthread_start may be entered at any time with no userspace 
    interaction.
    To handle this in valgrind, we create and destroy a valgrind 
    thread for every work item.
*/
void wqthread_hijack(Addr self, Addr kport, Addr stackaddr, Addr workitem, 
                     Int reuse, Addr sp)
{
   ThreadState *tst;
   VexGuestAMD64State *vex;
   Addr stack;
   SizeT stacksize;
   vki_sigset_t blockall;

   /* When we enter here we hold no lock (!), so we better acquire it
      pronto.  Why do we hold no lock?  Because (presumably) the only
      way to get here is as a result of a SfMayBlock syscall
      "workq_ops(WQOPS_THREAD_RETURN)", which will have dropped the
      lock.  At least that's clear for the 'reuse' case.  The
      non-reuse case?  Dunno, perhaps it's a new thread the kernel
      pulled out of a hat.  In any case we still need to take a
      lock. */
   VG_(acquire_BigLock_LL)("wqthread_hijack");

   /* Start the thread with all signals blocked.  VG_(scheduler) will
      set the mask correctly when we finally get there. */
   VG_(sigfillset)(&blockall);
   VG_(sigprocmask)(VKI_SIG_SETMASK, &blockall, NULL);

   if (reuse) {
       // This thread already exists; we're merely re-entering 
       // after leaving via workq_ops(WQOPS_THREAD_RETURN). 
       // Don't allocate any V thread resources.
       // Do reset thread registers.
       ThreadId tid = VG_(lwpid_to_vgtid)(kport);
       vg_assert(VG_(is_valid_tid)(tid));
       vg_assert(mach_thread_self() == kport);

       tst = VG_(get_ThreadState)(tid);
       vex = &tst->arch.vex;
       vg_assert(tst->os_state.pthread == self);
   }
   else {
       // This is a new thread.
       tst = VG_(get_ThreadState)(VG_(alloc_ThreadState)());        
       vex = &tst->arch.vex;
       allocstack(tst->tid);
       LibVEX_GuestAMD64_initialise(vex);
   }
       
   // Set thread's registers
   // Do this FIRST because some code below tries to collect a backtrace, 
   // which requires valid register data.
   vex->guest_RIP = wqthread_starter;
   vex->guest_RDI = self;
   vex->guest_RSI = kport;
   vex->guest_RDX = stackaddr;
   vex->guest_RCX = workitem;
   vex->guest_R8  = reuse;
   vex->guest_R9  = 0;
   vex->guest_RSP = sp;

   stacksize = 512*1024;  // wq stacks are always DEFAULT_STACK_SIZE
   stack = VG_PGROUNDUP(sp) - stacksize;

   if (reuse) {
      // Continue V's thread back in the scheduler. 
      // The client thread is of course in another location entirely.

      /* Drop the lock before going into
         ML_(wqthread_continue_NORETURN).  The latter will immediately
         attempt to reacquire it in non-LL mode, which is a bit
         wasteful but I don't think is harmful.  A better solution
         would be to not drop the lock but instead "upgrade" it from a
         LL lock to a full lock, but that's too much like hard work
         right now. */
      VG_(release_BigLock_LL)("wqthread_hijack(1)");
      ML_(wqthread_continue_NORETURN)(tst->tid);
   } 
   else {
      // Record thread's stack and Mach port and pthread struct
      tst->os_state.pthread = self;
      tst->os_state.lwpid = kport;
      record_named_port(tst->tid, kport, MACH_PORT_RIGHT_SEND, "wqthread-%p");
      
      // kernel allocated stack - needs mapping
      tst->client_stack_highest_word = stack+stacksize;
      tst->client_stack_szB = stacksize;

      // GrP fixme scheduler lock?!
      
      // pthread structure
      ML_(notify_core_and_tool_of_mmap)(
            stack+stacksize, pthread_structsize, 
            VKI_PROT_READ|VKI_PROT_WRITE, VKI_MAP_PRIVATE, -1, 0);
      // stack contents
      // GrP fixme uninitialized!
      ML_(notify_core_and_tool_of_mmap)(
            stack, stacksize, 
            VKI_PROT_READ|VKI_PROT_WRITE, VKI_MAP_PRIVATE, -1, 0);
      // guard page
      // GrP fixme ban_mem_stack!
      ML_(notify_core_and_tool_of_mmap)(
            stack-VKI_PAGE_SIZE, VKI_PAGE_SIZE,
            0, VKI_MAP_PRIVATE, -1, 0);

      ML_(sync_mappings)("after", "wqthread_hijack", 0);

      // Go!
      /* Same comments as the 'release' in the then-clause.
         start_thread_NORETURN calls run_thread_NORETURN calls
         thread_wrapper which acquires the lock before continuing.
         Let's hope nothing non-thread-local happens until that point.

         DDD: I think this is plain wrong .. if we get to
         thread_wrapper not holding the lock, and someone has recycled
         this thread slot in the meantime, we're hosed.  Is that
         possible, though? */
      VG_(release_BigLock_LL)("wqthread_hijack(2)");
      call_on_new_stack_0_1(tst->os_state.valgrind_stack_init_SP, 0, 
                            start_thread_NORETURN, (Word)tst);
   }

   /*NOTREACHED*/
   vg_assert(0);
}

#endif // defined(VGP_amd64_darwin)

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
