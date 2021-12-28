
/*--------------------------------------------------------------------*/
/*--- Darwin-specific syscalls, etc.        syswrap-arm64-darwin.c ---*/
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

#if defined(VGP_arm64_darwin)

#include "config.h"                // DARWIN_VERS
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

static void arm_thread_state64_from_vex(arm_thread_state64_t *mach,
                                        VexGuestARM64State *vex)
{
  mach->__x[0] = vex->guest_X0;
  mach->__x[1] = vex->guest_X1;
  mach->__x[2] = vex->guest_X2;
  mach->__x[3] = vex->guest_X3;
  mach->__x[4] = vex->guest_X4;
  mach->__x[5] = vex->guest_X5;
  mach->__x[6] = vex->guest_X6;
  mach->__x[7] = vex->guest_X7;
  mach->__x[8] = vex->guest_X8;
  mach->__x[9] = vex->guest_X9;
  mach->__x[10] = vex->guest_X10;
  mach->__x[11] = vex->guest_X11;
  mach->__x[13] = vex->guest_X13;
  mach->__x[14] = vex->guest_X14;
  mach->__x[15] = vex->guest_X15;
  mach->__x[16] = vex->guest_X16;
  mach->__x[17] = vex->guest_X17;
  mach->__x[18] = vex->guest_X18;
  mach->__x[19] = vex->guest_X19;
  mach->__x[20] = vex->guest_X20;
  mach->__x[21] = vex->guest_X21;
  mach->__x[22] = vex->guest_X22;
  mach->__x[23] = vex->guest_X23;
  mach->__x[24] = vex->guest_X24;
  mach->__x[25] = vex->guest_X25;
  mach->__x[26] = vex->guest_X26;
  mach->__x[27] = vex->guest_X27;
  mach->__x[28] = vex->guest_X28;
  mach->__fp = vex->guest_X29;
  mach->__lr = vex->guest_X30;
  mach->__sp = vex->guest_XSP;
  mach->__pc = vex->guest_PC;
  mach->__cpsr = LibVEX_GuestARM64_get_nzcv(vex);
  // FIXME: not required?
  // mach->__flags = ???;
}


static void arm_neon_state64_from_vex(arm_neon_state64_t *mach,
                                       VexGuestARM64State *vex)
{
  mach->__v[0] = *(const __uint128_t*)vex->guest_Q0;
  mach->__v[1] = *(const __uint128_t*)vex->guest_Q1;
  mach->__v[2] = *(const __uint128_t*)vex->guest_Q2;
  mach->__v[3] = *(const __uint128_t*)vex->guest_Q3;
  mach->__v[4] = *(const __uint128_t*)vex->guest_Q4;
  mach->__v[5] = *(const __uint128_t*)vex->guest_Q5;
  mach->__v[6] = *(const __uint128_t*)vex->guest_Q6;
  mach->__v[7] = *(const __uint128_t*)vex->guest_Q7;
  mach->__v[8] = *(const __uint128_t*)vex->guest_Q8;
  mach->__v[9] = *(const __uint128_t*)vex->guest_Q9;
  mach->__v[10] = *(const __uint128_t*)vex->guest_Q10;
  mach->__v[11] = *(const __uint128_t*)vex->guest_Q11;
  mach->__v[13] = *(const __uint128_t*)vex->guest_Q13;
  mach->__v[14] = *(const __uint128_t*)vex->guest_Q14;
  mach->__v[15] = *(const __uint128_t*)vex->guest_Q15;
  mach->__v[16] = *(const __uint128_t*)vex->guest_Q16;
  mach->__v[17] = *(const __uint128_t*)vex->guest_Q17;
  mach->__v[18] = *(const __uint128_t*)vex->guest_Q18;
  mach->__v[19] = *(const __uint128_t*)vex->guest_Q19;
  mach->__v[20] = *(const __uint128_t*)vex->guest_Q20;
  mach->__v[21] = *(const __uint128_t*)vex->guest_Q21;
  mach->__v[22] = *(const __uint128_t*)vex->guest_Q22;
  mach->__v[23] = *(const __uint128_t*)vex->guest_Q23;
  mach->__v[24] = *(const __uint128_t*)vex->guest_Q24;
  mach->__v[25] = *(const __uint128_t*)vex->guest_Q25;
  mach->__v[26] = *(const __uint128_t*)vex->guest_Q26;
  mach->__v[27] = *(const __uint128_t*)vex->guest_Q27;
  mach->__v[28] = *(const __uint128_t*)vex->guest_Q28;
  mach->__v[29] = *(const __uint128_t*)vex->guest_Q29;
  mach->__v[30] = *(const __uint128_t*)vex->guest_Q30;
  mach->__v[31] = *(const __uint128_t*)vex->guest_Q31;
  mach->__fpsr = *(const vki_u32*)vex->guest_QCFLAG;
  mach->__fpcr = vex->guest_FPCR;
}


void thread_state_from_vex(thread_state_t mach_generic,
                           thread_state_flavor_t flavor,
                           mach_msg_type_number_t count,
                           VexGuestArchState *vex_generic)
{
   VexGuestARM64State *vex = (VexGuestARM64State *)vex_generic;

   switch (flavor) {
   case ARM_THREAD_STATE64:
      vg_assert(count == ARM_THREAD_STATE64_COUNT);
      arm_thread_state64_from_vex((arm_thread_state64_t *)mach_generic, vex);
      break;

   case ARM_NEON_STATE64:
      vg_assert(count == ARM_NEON_STATE64_COUNT);
      arm_neon_state64_from_vex((arm_neon_state64_t *)mach_generic, vex);
      break;

   case ARM_EXCEPTION_STATE:
      VG_(printf)("thread_state_from_vex: TODO, want exception state\n");
      vg_assert(0);

   default:
      VG_(printf)("thread_state_from_vex: flavor:%#x\n",  flavor);
      vg_assert(0);
   }
}


static void arm_thread_state64_to_vex(const arm_thread_state64_t *mach,
                                      VexGuestARM64State *vex)
{
  LibVEX_GuestARM64_initialise(vex);
  vex->guest_X0 = mach->__x[0];
  vex->guest_X1 = mach->__x[1];
  vex->guest_X2 = mach->__x[2];
  vex->guest_X3 = mach->__x[3];
  vex->guest_X4 = mach->__x[4];
  vex->guest_X5 = mach->__x[5];
  vex->guest_X6 = mach->__x[6];
  vex->guest_X7 = mach->__x[7];
  vex->guest_X8 = mach->__x[8];
  vex->guest_X9 = mach->__x[9];
  vex->guest_X10 = mach->__x[10];
  vex->guest_X11 = mach->__x[11];
  vex->guest_X13 = mach->__x[13];
  vex->guest_X14 = mach->__x[14];
  vex->guest_X15 = mach->__x[15];
  vex->guest_X16 = mach->__x[16];
  vex->guest_X17 = mach->__x[17];
  vex->guest_X18 = mach->__x[18];
  vex->guest_X19 = mach->__x[19];
  vex->guest_X20 = mach->__x[20];
  vex->guest_X21 = mach->__x[21];
  vex->guest_X22 = mach->__x[22];
  vex->guest_X23 = mach->__x[23];
  vex->guest_X24 = mach->__x[24];
  vex->guest_X25 = mach->__x[25];
  vex->guest_X26 = mach->__x[26];
  vex->guest_X27 = mach->__x[27];
  vex->guest_X28 = mach->__x[28];
  vex->guest_X29 = mach->__fp;
  vex->guest_X30 = mach->__lr;
  vex->guest_XSP = mach->__sp;
  vex->guest_PC = mach->__pc;
  // FIXME: sorta hacky?
  vex->guest_CC_DEP1 = mach->__cpsr;
}

static void arm_neon_state64_to_vex(const arm_neon_state64_t *mach,
                                     VexGuestARM64State *vex)
{
  memcpy(&vex->guest_Q0, (const U128 *)&mach->__v[0], sizeof(vex->guest_Q0));
  memcpy(&vex->guest_Q1, (const U128 *)&mach->__v[1], sizeof(vex->guest_Q1));
  memcpy(&vex->guest_Q2, (const U128 *)&mach->__v[2], sizeof(vex->guest_Q2));
  memcpy(&vex->guest_Q3, (const U128 *)&mach->__v[3], sizeof(vex->guest_Q3));
  memcpy(&vex->guest_Q4, (const U128 *)&mach->__v[4], sizeof(vex->guest_Q4));
  memcpy(&vex->guest_Q5, (const U128 *)&mach->__v[5], sizeof(vex->guest_Q5));
  memcpy(&vex->guest_Q6, (const U128 *)&mach->__v[6], sizeof(vex->guest_Q6));
  memcpy(&vex->guest_Q7, (const U128 *)&mach->__v[7], sizeof(vex->guest_Q7));
  memcpy(&vex->guest_Q8, (const U128 *)&mach->__v[8], sizeof(vex->guest_Q8));
  memcpy(&vex->guest_Q9, (const U128 *)&mach->__v[9], sizeof(vex->guest_Q9));
  memcpy(&vex->guest_Q10, (const U128 *)&mach->__v[10], sizeof(vex->guest_Q10));
  memcpy(&vex->guest_Q11, (const U128 *)&mach->__v[11], sizeof(vex->guest_Q11));
  memcpy(&vex->guest_Q13, (const U128 *)&mach->__v[13], sizeof(vex->guest_Q13));
  memcpy(&vex->guest_Q14, (const U128 *)&mach->__v[14], sizeof(vex->guest_Q14));
  memcpy(&vex->guest_Q15, (const U128 *)&mach->__v[15], sizeof(vex->guest_Q15));
  memcpy(&vex->guest_Q16, (const U128 *)&mach->__v[16], sizeof(vex->guest_Q16));
  memcpy(&vex->guest_Q17, (const U128 *)&mach->__v[17], sizeof(vex->guest_Q17));
  memcpy(&vex->guest_Q18, (const U128 *)&mach->__v[18], sizeof(vex->guest_Q18));
  memcpy(&vex->guest_Q19, (const U128 *)&mach->__v[19], sizeof(vex->guest_Q19));
  memcpy(&vex->guest_Q20, (const U128 *)&mach->__v[20], sizeof(vex->guest_Q20));
  memcpy(&vex->guest_Q21, (const U128 *)&mach->__v[21], sizeof(vex->guest_Q21));
  memcpy(&vex->guest_Q22, (const U128 *)&mach->__v[22], sizeof(vex->guest_Q22));
  memcpy(&vex->guest_Q23, (const U128 *)&mach->__v[23], sizeof(vex->guest_Q23));
  memcpy(&vex->guest_Q24, (const U128 *)&mach->__v[24], sizeof(vex->guest_Q24));
  memcpy(&vex->guest_Q25, (const U128 *)&mach->__v[25], sizeof(vex->guest_Q25));
  memcpy(&vex->guest_Q26, (const U128 *)&mach->__v[26], sizeof(vex->guest_Q26));
  memcpy(&vex->guest_Q27, (const U128 *)&mach->__v[27], sizeof(vex->guest_Q27));
  memcpy(&vex->guest_Q28, (const U128 *)&mach->__v[28], sizeof(vex->guest_Q28));
  memcpy(&vex->guest_Q29, (const U128 *)&mach->__v[29], sizeof(vex->guest_Q29));
  memcpy(&vex->guest_Q30, (const U128 *)&mach->__v[30], sizeof(vex->guest_Q30));
  memcpy(&vex->guest_Q31, (const U128 *)&mach->__v[31], sizeof(vex->guest_Q31));
  memcpy(&vex->guest_QCFLAG, (const U128 *)&mach->__fpsr, sizeof(vex->guest_QCFLAG));
  vex->guest_FPCR = mach->__fpcr;
}


void thread_state_to_vex(const thread_state_t mach_generic,
                         thread_state_flavor_t flavor,
                         mach_msg_type_number_t count,
                         VexGuestArchState *vex_generic)
{
   VexGuestARM64State *vex = (VexGuestARM64State *)vex_generic;

   switch(flavor) {
   case ARM_THREAD_STATE64:
      vg_assert(count == ARM_THREAD_STATE64_COUNT);
      arm_thread_state64_to_vex((const arm_thread_state64_t*)mach_generic,vex);
      break;
   case ARM_NEON_STATE64:
      vg_assert(count == ARM_NEON_STATE64_COUNT);
      arm_neon_state64_to_vex((const arm_neon_state64_t*)mach_generic,vex);
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

   vg_assert(flavor == ARM_THREAD_STATE64);
   vg_assert(count == ARM_THREAD_STATE64_COUNT);

   // Initialize machine registers

   thread_state_to_vex(state, flavor, count, &tst->arch.vex);

   I_die_here;
   // GrP fixme signals, sig_mask, tmp_sig_mask, os_state.parent

   find_stack_segment(tid, tst->arch.vex.guest_XSP);

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
   arm_thread_state64_t *mach = (arm_thread_state64_t *)mach_generic;
   char *stack;

   vg_assert(flavor == ARM_THREAD_STATE64);
   vg_assert(count == ARM_THREAD_STATE64_COUNT);

   stack = (char *)allocstack(tst->tid);
   stack -= 64+320;                       // make room for top frame
   memset(stack, 0, 64+320);              // ...and clear it
   *(uintptr_t *)stack = 0;               // push fake return address

   mach->__x[0] = (uintptr_t)tst;          // arg1 = tst
   mach->__pc = (uintptr_t)&start_thread_NORETURN;
   mach->__sp = (uintptr_t)stack;
}


/* Call f(arg1), but first switch stacks, using 'stack' as the new
   stack, and use 'retaddr' as f's return-to address.  Also, clear all
   the integer registers before entering f.*/
__attribute__((noreturn))
void call_on_new_stack_0_1 ( Addr stack,
			     Addr retaddr,
			     void (*f)(Word),
                             Word arg1 );
// x0 == stack (must be 16-byte aligned)
// x1 == retaddr
// x2 == f
// x3 == arg1
asm(
".globl _call_on_new_stack_0_1\n"
"_call_on_new_stack_0_1:\n"
"   mov    sp, x0\n\t" /* Stack pointer */
"   mov    x30, x1\n\t" /* Return address (x30 is LR) */
"   mov    x0, x3\n\t" /* First argument */
"   mov    x9, x2\n\t" /* 'f': x9 won't be zeroed at start of f.  Oh well. */
"   mov    x1, #0\n\t" /* Clear our GPRs */
"   mov    x2, #0\n\t"
"   mov    x3, #0\n\t"
"   mov    x4, #0\n\t"
"   mov    x5, #0\n\t"
"   mov    x6, #0\n\t"
"   mov    x7, #0\n\t"
"   mov    x8, #0\n\t"
/* don't zero out x9 */
"   mov    x10, #0\n\t"
"   mov    x11, #0\n\t"
"   mov    x12, #0\n\t"
"   mov    x13, #0\n\t"
"   mov    x14, #0\n\t"
"   mov    x15, #0\n\t"
"   mov    x16, #0\n\t"
"   mov    x17, #0\n\t"
// "   mov    x18, #0\n\t" // Apple doesn't want us touching it
"   mov    x19, #0\n\t"
"   mov    x20, #0\n\t"
"   mov    x21, #0\n\t"
"   mov    x22, #0\n\t"
"   mov    x23, #0\n\t"
"   mov    x24, #0\n\t"
"   mov    x25, #0\n\t"
"   mov    x26, #0\n\t"
"   mov    x27, #0\n\t"
"   mov    x28, #0\n\t"
"   mov    x29, sp\n\t" /* FP = SP, in the absence of better suggestions */
"   ret x30\n"                 // jump to f
"   udf #0\n"                 // should never get here
);

asm(
".globl _pthread_hijack_asm\n"
"_pthread_hijack_asm:\n"
"   mov x30, 0\n"  // fake return address
"   b _pthread_hijack\n"
);



void pthread_hijack(Addr self, Addr kport, Addr func, Addr func_arg,
                    Addr stacksize, Addr flags, Addr sp)
{
   vki_sigset_t blockall;
   ThreadState *tst = (ThreadState *)func_arg;
   VexGuestARM64State *vex = &tst->arch.vex;

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
   LibVEX_GuestARM64_initialise(vex);
   vex->guest_PC = pthread_starter;
   vex->guest_X0 = self;
   vex->guest_X1 = kport;
   vex->guest_X2 = func;
   vex->guest_X3 = tst->os_state.func_arg;
   vex->guest_X4 = stacksize;
   vex->guest_X5 = flags;
   vex->guest_XSP = sp;

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
"   mov x30, 0\n"  // fake return address
"   b _wqthread_hijack\n"
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
   VexGuestARM64State *vex;
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
	     self, kport, stackaddr, workitem, (UInt)reuse, sp);

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
        constant offset of 96 on 10.7, but zero on 10.6 and 10.5.  No
        idea why. */
#      if DARWIN_VERS <= DARWIN_10_6
       UWord magic_delta = 0;
#      elif DARWIN_VERS == DARWIN_10_7 || DARWIN_VERS == DARWIN_10_8
       UWord magic_delta = 0x60;
#      elif DARWIN_VERS == DARWIN_10_9 \
            || DARWIN_VERS == DARWIN_10_10 \
            || DARWIN_VERS == DARWIN_10_11 \
            || DARWIN_VERS == DARWIN_10_12 \
            || DARWIN_VERS == DARWIN_10_13 \
            || DARWIN_VERS == DARWIN_10_14 \
            || DARWIN_VERS == DARWIN_10_15 \
            || DARWIN_VERS == DARWIN_11_00
       UWord magic_delta = 0xE0;
#      else
#        error "magic_delta: to be computed on new OS version"
         // magic_delta = tst->os_state.pthread - self
#      endif

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
                          tid, (void *)tst, tst->os_state.pthread, self);

       vex = &tst->arch.vex;
       vg_assert(tst->os_state.pthread - magic_delta == self);
   }
   else {
       // This is a new thread.
       tst = VG_(get_ThreadState)(VG_(alloc_ThreadState)());
       vex = &tst->arch.vex;
       allocstack(tst->tid);
       LibVEX_GuestARM64_initialise(vex);
   }

   // Set thread's registers
   // Do this FIRST because some code below tries to collect a backtrace,
   // which requires valid register data.
   vex->guest_PC = wqthread_starter;
   vex->guest_X0 = self;
   vex->guest_X1 = kport;
   vex->guest_X2 = stackaddr;
   vex->guest_X3 = workitem;
   vex->guest_X4 = reuse;
   vex->guest_X5 = 0;
   vex->guest_XSP = sp;

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

#endif // defined(VGP_arm64_darwin)

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
