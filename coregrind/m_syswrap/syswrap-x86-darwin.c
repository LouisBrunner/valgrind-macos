
/*--------------------------------------------------------------------*/
/*--- Darwin-specific syscalls, etc.          syswrap-x86-darwin.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2005-2017 Apple Inc.
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
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.
*/

#if defined(VGP_x86_darwin)

#include "pub_core_basics.h"
#include "pub_core_vki.h"
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
#include "pub_core_signals.h"
#include "pub_core_syscall.h"
#include "pub_core_syswrap.h"
#include "pub_core_tooliface.h"

#include "priv_types_n_macros.h"
#include "priv_syswrap-generic.h"   /* for decls of generic wrappers */
#include "priv_syswrap-darwin.h"    /* for decls of darwin-ish wrappers */
#include "priv_syswrap-main.h"


#include <mach/mach.h>

static void x86_thread_state32_from_vex(i386_thread_state_t *mach, 
                                        VexGuestX86State *vex)
{
    mach->__eax = vex->guest_EAX;
    mach->__ebx = vex->guest_EBX;
    mach->__ecx = vex->guest_ECX;
    mach->__edx = vex->guest_EDX;
    mach->__edi = vex->guest_EDI;
    mach->__esi = vex->guest_ESI;
    mach->__ebp = vex->guest_EBP;
    mach->__esp = vex->guest_ESP;
    mach->__ss = vex->guest_SS;
    mach->__eflags = LibVEX_GuestX86_get_eflags(vex);
    mach->__eip = vex->guest_EIP;
    mach->__cs = vex->guest_CS;
    mach->__ds = vex->guest_DS;
    mach->__es = vex->guest_ES;
    mach->__fs = vex->guest_FS;
    mach->__gs = vex->guest_GS;
}


static void x86_float_state32_from_vex(i386_float_state_t *mach, 
                                       VexGuestX86State *vex)
{
   // DDD: #warning GrP fixme fp state

   VG_(memcpy)(&mach->__fpu_xmm0, &vex->guest_XMM0, 8 * sizeof(mach->__fpu_xmm0));
}


void thread_state_from_vex(thread_state_t mach_generic, 
                           thread_state_flavor_t flavor, 
                           mach_msg_type_number_t count, 
                           VexGuestArchState *vex_generic)
{
   VexGuestX86State *vex = (VexGuestX86State *)vex_generic;

   switch (flavor) {
   case i386_THREAD_STATE:
      vg_assert(count == i386_THREAD_STATE_COUNT);
      x86_thread_state32_from_vex((i386_thread_state_t *)mach_generic, vex);
      break;

   case i386_FLOAT_STATE:
      vg_assert(count == i386_FLOAT_STATE_COUNT);
      x86_float_state32_from_vex((i386_float_state_t *)mach_generic, vex);
      break;
       
   default:
      vg_assert(0);
   }
}


static void x86_thread_state32_to_vex(const i386_thread_state_t *mach, 
                                      VexGuestX86State *vex)
{
   LibVEX_GuestX86_initialise(vex);
   vex->guest_EAX = mach->__eax;
   vex->guest_EBX = mach->__ebx;
   vex->guest_ECX = mach->__ecx;
   vex->guest_EDX = mach->__edx;
   vex->guest_EDI = mach->__edi;
   vex->guest_ESI = mach->__esi;
   vex->guest_EBP = mach->__ebp;
   vex->guest_ESP = mach->__esp;
   vex->guest_SS = mach->__ss;
   // DDD: #warning GrP fixme eflags
   vex->guest_EIP = mach->__eip;
   vex->guest_CS = mach->__cs;
   vex->guest_DS = mach->__ds;
   vex->guest_ES = mach->__es;
   vex->guest_FS = mach->__fs;
   vex->guest_GS = mach->__gs;
}

static void x86_float_state32_to_vex(const i386_float_state_t *mach, 
                                     VexGuestX86State *vex)
{
   // DDD: #warning GrP fixme fp state

   VG_(memcpy)(&vex->guest_XMM0, &mach->__fpu_xmm0, 8 * sizeof(mach->__fpu_xmm0));
}


void thread_state_to_vex(const thread_state_t mach_generic, 
                         thread_state_flavor_t flavor, 
                         mach_msg_type_number_t count, 
                         VexGuestArchState *vex_generic)
{
   VexGuestX86State *vex = (VexGuestX86State *)vex_generic;
   
   switch(flavor) {
   case i386_THREAD_STATE:
      vg_assert(count == i386_THREAD_STATE_COUNT);
      x86_thread_state32_to_vex((const i386_thread_state_t*)mach_generic,vex);
      break;
   case i386_FLOAT_STATE:
      vg_assert(count == i386_FLOAT_STATE_COUNT);
      x86_float_state32_to_vex((const i386_float_state_t*)mach_generic,vex);
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
    
   vg_assert(flavor == i386_THREAD_STATE);
   vg_assert(count == i386_THREAD_STATE_COUNT);

   // Initialize machine registers

   thread_state_to_vex(state, flavor, count, &tst->arch.vex);

   I_die_here;
   // GrP fixme signals, sig_mask, tmp_sig_mask, os_state.parent

   find_stack_segment(tid, tst->arch.vex.guest_ESP);

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
   i386_thread_state_t *mach = (i386_thread_state_t *)mach_generic;
   char *stack;

   vg_assert(flavor == i386_THREAD_STATE);
   vg_assert(count == i386_THREAD_STATE_COUNT);

   stack = (char *)allocstack(tst->tid);
   stack -= 64+320;                       // make room for top frame
   memset(stack, 0, 64+320);              // ...and clear it
   *(uintptr_t *)stack = (uintptr_t)tst;  // set parameter
   stack -= sizeof(uintptr_t);
   *(uintptr_t *)stack = 0;               // push fake return address

   mach->__eip = (uintptr_t)&start_thread_NORETURN;
   mach->__esp = (uintptr_t)stack;
}


/* Call f(arg1), but first switch stacks, using 'stack' as the new
   stack, and use 'retaddr' as f's return-to address.  Also, clear all
   the integer registers before entering f.*/
__attribute__((noreturn))
void call_on_new_stack_0_1 ( Addr stack,
			     Addr retaddr,
			     void (*f)(Word),
                             Word arg1 );
//  4(%esp) == stack (must be 16-byte aligned)
//  8(%esp) == retaddr
// 12(%esp) == f
// 16(%esp) == arg1
asm(
".globl _call_on_new_stack_0_1\n"
"_call_on_new_stack_0_1:\n"
"   movl %esp, %esi\n"     // remember old stack pointer
"   movl 4(%esi), %esp\n"  // set new stack
"   pushl $0\n"            // align stack
"   pushl $0\n"            // align stack
"   pushl $0\n"            // align stack
"   pushl 16(%esi)\n"      // arg1 to stack
"   pushl  8(%esi)\n"      // retaddr to stack
"   pushl 12(%esi)\n"      // f to stack
"   movl $0, %eax\n"       // zero all GP regs
"   movl $0, %ebx\n"
"   movl $0, %ecx\n"
"   movl $0, %edx\n"
"   movl $0, %esi\n"
"   movl $0, %edi\n"
"   movl $0, %ebp\n"
"   ret\n"                 // jump to f
"   ud2\n"                 // should never get here
);


asm(
".globl _pthread_hijack_asm\n"
"_pthread_hijack_asm:\n"
"   movl %esp,%ebp\n"
"   push $0\n"    // alignment pad
"   push %ebp\n"  // original sp
"   push %esi\n"  // flags
"   push %edi\n"  // stacksize
"   push %edx\n"  // func_arg
"   push %ecx\n"  // func
"   push %ebx\n"  // kport
"   push %eax\n"  // self
"   push $0\n"    // fake return address
"   jmp _pthread_hijack\n"
    );



void pthread_hijack(Addr self, Addr kport, Addr func, Addr func_arg, 
                    Addr stacksize, Addr flags, Addr sp)
{
   vki_sigset_t blockall;
   ThreadState *tst = (ThreadState *)func_arg;
   VexGuestX86State *vex = &tst->arch.vex;

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
   // DDD: need to do post_reg_write events here?
   LibVEX_GuestX86_initialise(vex);
   vex->guest_EIP = pthread_starter;
   vex->guest_EAX = self;
   vex->guest_EBX = kport;
   vex->guest_ECX = func;
   vex->guest_EDX = tst->os_state.func_arg;
   vex->guest_EDI = stacksize;
   vex->guest_ESI = flags;
   vex->guest_ESP = sp;

   // Record thread's stack and Mach port and pthread struct
   tst->os_state.pthread = self;
   tst->os_state.lwpid = kport;
   record_named_port(tst->tid, kport, MACH_PORT_RIGHT_SEND, "thread-%p");

   if ((flags & 0x01000000) == 0) {
      // kernel allocated stack - needs mapping
      Addr stack = VG_PGROUNDUP(sp) - stacksize;
      tst->client_stack_highest_byte = stack+stacksize-1;
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
"   movl %esp,%ebp\n"
"   push $0\n"    // alignment
"   push $0\n"    // alignment
"   push %ebp\n"  // original sp
"   push %edi\n"  // reuse
"   push %edx\n"  // workitem
"   push %ecx\n"  // stackaddr
"   push %ebx\n"  // kport
"   push %eax\n"  // self
"   push $0\n"    // fake return address
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
   VexGuestX86State *vex;
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

   if (0) VG_(printf)(
             "wqthread_hijack: self %#lx, kport %#lx, "
             "stackaddr %#lx, workitem %#lx, reuse/flags %x, sp %#lx\n",
             self, kport, stackaddr, workitem, reuse, sp);

   /* Start the thread with all signals blocked.  VG_(scheduler) will
      set the mask correctly when we finally get there. */
   VG_(sigfillset)(&blockall);
   VG_(sigprocmask)(VKI_SIG_SETMASK, &blockall, NULL);

   /* For 10.7 and earlier, |reuse| appeared to be used as a simple
      boolean.  In 10.8 and later its name changed to |flags| and has
      various other bits OR-d into it too, so it's necessary to fish
      out just the relevant parts.  Hence: */
#  if DARWIN_VERS <= DARWIN_10_7
   Bool is_reuse = reuse != 0;
#  elif DARWIN_VERS > DARWIN_10_7
   Bool is_reuse = (reuse & 0x20000 /* == WQ_FLAG_THREAD_REUSE */) != 0;
#  else
#    error "Unsupported Darwin version"
#  endif

   if (is_reuse) {

      /* For whatever reason, tst->os_state.pthread appear to have a
         constant offset of 72 on 10.7, but zero on 10.6 and 10.5.  No
         idea why. */
#     if DARWIN_VERS <= DARWIN_10_6
      UWord magic_delta = 0;
#     elif DARWIN_VERS == DARWIN_10_7 || DARWIN_VERS == DARWIN_10_8
      UWord magic_delta = 0x48;
#     elif DARWIN_VERS == DARWIN_10_9 \
           || DARWIN_VERS == DARWIN_10_10 \
           || DARWIN_VERS == DARWIN_10_11 \
           || DARWIN_VERS == DARWIN_10_12 \
           || DARWIN_VERS == DARWIN_10_13
      UWord magic_delta = 0xB0;
#     else
#       error "magic_delta: to be computed on new OS version"
        // magic_delta = tst->os_state.pthread - self
#     endif

      // This thread already exists; we're merely re-entering 
      // after leaving via workq_ops(WQOPS_THREAD_RETURN). 
      // Don't allocate any V thread resources.
      // Do reset thread registers.
      ThreadId tid = VG_(lwpid_to_vgtid)(kport);
      vg_assert(VG_(is_valid_tid)(tid));
      vg_assert(mach_thread_self() == kport);

      tst = VG_(get_ThreadState)(tid);

      if (0) VG_(printf)("wqthread_hijack reuse %s: tid %u, tst %p, "
                         "tst->os_state.pthread %#lx, self %#lx\n",
                         tst->os_state.pthread == self ? "SAME" : "DIFF",
                         tid, tst, tst->os_state.pthread, self);

      vex = &tst->arch.vex;
      vg_assert(tst->os_state.pthread - magic_delta == self);
   }
   else {
      // This is a new thread.
      tst = VG_(get_ThreadState)(VG_(alloc_ThreadState)());        
      vex = &tst->arch.vex;
      allocstack(tst->tid);
      LibVEX_GuestX86_initialise(vex);
   }
        
   // Set thread's registers
   // Do this FIRST because some code below tries to collect a backtrace, 
   // which requires valid register data.
   vex->guest_EIP = wqthread_starter;
   vex->guest_EAX = self;
   vex->guest_EBX = kport;
   vex->guest_ECX = stackaddr;
   vex->guest_EDX = workitem;
   vex->guest_EDI = reuse;
   vex->guest_ESI = 0;
   vex->guest_ESP = sp;

   stacksize = 512*1024;  // wq stacks are always DEFAULT_STACK_SIZE
   stack = VG_PGROUNDUP(sp) - stacksize;

   if (is_reuse) {
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
      tst->client_stack_highest_byte = stack+stacksize-1;
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

#endif // defined(VGP_x86_darwin)

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
