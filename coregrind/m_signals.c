
/*--------------------------------------------------------------------*/
/*--- Implementation of POSIX signals.                 m_signals.c ---*/
/*--------------------------------------------------------------------*/
 
/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2012 Julian Seward 
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
   Signal handling.

   There are 4 distinct classes of signal:

   1. Synchronous, instruction-generated (SIGILL, FPE, BUS, SEGV and
   TRAP): these are signals as a result of an instruction fault.  If
   we get one while running client code, then we just do the
   appropriate thing.  If it happens while running Valgrind code, then
   it indicates a Valgrind bug.  Note that we "manually" implement
   automatic stack growth, such that if a fault happens near the
   client process stack, it is extended in the same way the kernel
   would, and the fault is never reported to the client program.

   2. Asynchronous variants of the above signals: If the kernel tries
   to deliver a sync signal while it is blocked, it just kills the
   process.  Therefore, we can't block those signals if we want to be
   able to report on bugs in Valgrind.  This means that we're also
   open to receiving those signals from other processes, sent with
   kill.  We could get away with just dropping them, since they aren't
   really signals that processes send to each other.

   3. Synchronous, general signals.  If a thread/process sends itself
   a signal with kill, its expected to be synchronous: ie, the signal
   will have been delivered by the time the syscall finishes.
   
   4. Asynchronous, general signals.  All other signals, sent by
   another process with kill.  These are generally blocked, except for
   two special cases: we poll for them each time we're about to run a
   thread for a time quanta, and while running blocking syscalls.


   In addition, we reserve one signal for internal use: SIGVGKILL.
   SIGVGKILL is used to terminate threads.  When one thread wants
   another to exit, it will set its exitreason and send it SIGVGKILL
   if it appears to be blocked in a syscall.


   We use a kernel thread for each application thread.  When the
   thread allows itself to be open to signals, it sets the thread
   signal mask to what the client application set it to.  This means
   that we get the kernel to do all signal routing: under Valgrind,
   signals get delivered in the same way as in the non-Valgrind case
   (the exception being for the sync signal set, since they're almost
   always unblocked).
 */

/*
   Some more details...

   First off, we take note of the client's requests (via sys_sigaction
   and sys_sigprocmask) to set the signal state (handlers for each
   signal, which are process-wide, + a mask for each signal, which is
   per-thread).  This info is duly recorded in the SCSS (static Client
   signal state) in m_signals.c, and if the client later queries what
   the state is, we merely fish the relevant info out of SCSS and give
   it back.

   However, we set the real signal state in the kernel to something
   entirely different.  This is recorded in SKSS, the static Kernel
   signal state.  What's nice (to the extent that anything is nice w.r.t
   signals) is that there's a pure function to calculate SKSS from SCSS,
   calculate_SKSS_from_SCSS.  So when the client changes SCSS then we
   recompute the associated SKSS and apply any changes from the previous
   SKSS through to the kernel.

   Now, that said, the general scheme we have now is, that regardless of
   what the client puts into the SCSS (viz, asks for), what we would
   like to do is as follows:

   (1) run code on the virtual CPU with all signals blocked

   (2) at convenient moments for us (that is, when the VCPU stops, and
      control is back with the scheduler), ask the kernel "do you have
      any signals for me?"  and if it does, collect up the info, and
      deliver them to the client (by building sigframes).

   And that's almost what we do.  The signal polling is done by
   VG_(poll_signals), which calls through to VG_(sigtimedwait_zero) to
   do the dirty work.  (of which more later).

   By polling signals, rather than catching them, we get to deal with
   them only at convenient moments, rather than having to recover from
   taking a signal while generated code is running.

   Now unfortunately .. the above scheme only works for so-called async
   signals.  An async signal is one which isn't associated with any
   particular instruction, eg Control-C (SIGINT).  For those, it doesn't
   matter if we don't deliver the signal to the client immediately; it
   only matters that we deliver it eventually.  Hence polling is OK.

   But the other group -- sync signals -- are all related by the fact
   that they are various ways for the host CPU to fail to execute an
   instruction: SIGILL, SIGSEGV, SIGFPU.  And they can't be deferred,
   because obviously if a host instruction can't execute, well then we
   have to immediately do Plan B, whatever that is.

   So the next approximation of what happens is:

   (1) run code on vcpu with all async signals blocked

   (2) at convenient moments (when NOT running the vcpu), poll for async
      signals.

   (1) and (2) together imply that if the host does deliver a signal to
      async_signalhandler while the VCPU is running, something's
      seriously wrong.

   (3) when running code on vcpu, don't block sync signals.  Instead
      register sync_signalhandler and catch any such via that.  Of
      course, that means an ugly recovery path if we do -- the
      sync_signalhandler has to longjump, exiting out of the generated
      code, and the assembly-dispatcher thingy that runs it, and gets
      caught in m_scheduler, which then tells m_signals to deliver the
      signal.

   Now naturally (ha ha) even that might be tolerable, but there's
   something worse: dealing with signals delivered to threads in
   syscalls.

   Obviously from the above, SKSS's signal mask (viz, what we really run
   with) is way different from SCSS's signal mask (viz, what the client
   thread thought it asked for).  (eg) It may well be that the client
   did not block control-C, so that it just expects to drop dead if it
   receives ^C whilst blocked in a syscall, but by default we are
   running with all async signals blocked, and so that signal could be
   arbitrarily delayed, or perhaps even lost (not sure).

   So what we have to do, when doing any syscall which SfMayBlock, is to
   quickly switch in the SCSS-specified signal mask just before the
   syscall, and switch it back just afterwards, and hope that we don't
   get caught up in some wierd race condition.  This is the primary
   purpose of the ultra-magical pieces of assembly code in
   coregrind/m_syswrap/syscall-<plat>.S

   -----------

   The ways in which V can come to hear of signals that need to be
   forwarded to the client as are follows:

    sync signals: can arrive at any time whatsoever.  These are caught
                  by sync_signalhandler

    async signals:

       if    running generated code
       then  these are blocked, so we don't expect to catch them in
             async_signalhandler

       else
       if    thread is blocked in a syscall marked SfMayBlock
       then  signals may be delivered to async_sighandler, since we
             temporarily unblocked them for the duration of the syscall,
             by using the real (SCSS) mask for this thread

       else  we're doing misc housekeeping activities (eg, making a translation,
             washing our hair, etc).  As in the normal case, these signals are
             blocked, but we can  and do poll for them using VG_(poll_signals).

   Now, re VG_(poll_signals), it polls the kernel by doing
   VG_(sigtimedwait_zero).  This is trivial on Linux, since it's just a
   syscall.  But on Darwin and AIX, we have to cobble together the
   functionality in a tedious, longwinded and probably error-prone way.

   Finally, if a gdb is debugging the process under valgrind,
   the signal can be ignored if gdb tells this. So, before resuming the
   scheduler/delivering the signal, a call to VG_(gdbserver_report_signal)
   is done. If this returns True, the signal is delivered.
 */

#include "pub_core_basics.h"
#include "pub_core_vki.h"
#include "pub_core_vkiscnums.h"
#include "pub_core_debuglog.h"
#include "pub_core_libcsetjmp.h"    // to keep _threadstate.h happy
#include "pub_core_threadstate.h"
#include "pub_core_xarray.h"
#include "pub_core_clientstate.h"
#include "pub_core_aspacemgr.h"
#include "pub_core_debugger.h"      // For VG_(start_debugger)
#include "pub_core_errormgr.h"
#include "pub_core_gdbserver.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcprint.h"
#include "pub_core_libcproc.h"
#include "pub_core_libcsignal.h"
#include "pub_core_machine.h"
#include "pub_core_mallocfree.h"
#include "pub_core_options.h"
#include "pub_core_scheduler.h"
#include "pub_core_signals.h"
#include "pub_core_sigframe.h"      // For VG_(sigframe_create)()
#include "pub_core_stacks.h"        // For VG_(change_stack)()
#include "pub_core_stacktrace.h"    // For VG_(get_and_pp_StackTrace)()
#include "pub_core_syscall.h"
#include "pub_core_syswrap.h"
#include "pub_core_tooliface.h"
#include "pub_core_coredump.h"


/* ---------------------------------------------------------------------
   Forwards decls.
   ------------------------------------------------------------------ */

static void sync_signalhandler  ( Int sigNo, vki_siginfo_t *info,
                                             struct vki_ucontext * );
static void async_signalhandler ( Int sigNo, vki_siginfo_t *info,
                                             struct vki_ucontext * );
static void sigvgkill_handler	( Int sigNo, vki_siginfo_t *info,
                                             struct vki_ucontext * );

/* Maximum usable signal. */
Int VG_(max_signal) = _VKI_NSIG;

#define N_QUEUED_SIGNALS	8

typedef struct SigQueue {
   Int	next;
   vki_siginfo_t sigs[N_QUEUED_SIGNALS];
} SigQueue;

/* ------ Macros for pulling stuff out of ucontexts ------ */

/* Q: what does VG_UCONTEXT_SYSCALL_SYSRES do?  A: let's suppose the
   machine context (uc) reflects the situation that a syscall had just
   completed, quite literally -- that is, that the program counter was
   now at the instruction following the syscall.  (or we're slightly
   downstream, but we're sure no relevant register has yet changed
   value.)  Then VG_UCONTEXT_SYSCALL_SYSRES returns a SysRes reflecting
   the result of the syscall; it does this by fishing relevant bits of
   the machine state out of the uc.  Of course if the program counter
   was somewhere else entirely then the result is likely to be
   meaningless, so the caller of VG_UCONTEXT_SYSCALL_SYSRES has to be
   very careful to pay attention to the results only when it is sure
   that the said constraint on the program counter is indeed valid. */

#if defined(VGP_x86_linux)
#  define VG_UCONTEXT_INSTR_PTR(uc)       ((uc)->uc_mcontext.eip)
#  define VG_UCONTEXT_STACK_PTR(uc)       ((uc)->uc_mcontext.esp)
#  define VG_UCONTEXT_SYSCALL_SYSRES(uc)                        \
      /* Convert the value in uc_mcontext.eax into a SysRes. */ \
      VG_(mk_SysRes_x86_linux)( (uc)->uc_mcontext.eax )
#  define VG_UCONTEXT_TO_UnwindStartRegs(srP, uc)        \
      { (srP)->r_pc = (ULong)((uc)->uc_mcontext.eip);    \
        (srP)->r_sp = (ULong)((uc)->uc_mcontext.esp);    \
        (srP)->misc.X86.r_ebp = (uc)->uc_mcontext.ebp;   \
      }

#elif defined(VGP_amd64_linux)
#  define VG_UCONTEXT_INSTR_PTR(uc)       ((uc)->uc_mcontext.rip)
#  define VG_UCONTEXT_STACK_PTR(uc)       ((uc)->uc_mcontext.rsp)
#  define VG_UCONTEXT_SYSCALL_SYSRES(uc)                        \
      /* Convert the value in uc_mcontext.rax into a SysRes. */ \
      VG_(mk_SysRes_amd64_linux)( (uc)->uc_mcontext.rax )
#  define VG_UCONTEXT_TO_UnwindStartRegs(srP, uc)        \
      { (srP)->r_pc = (uc)->uc_mcontext.rip;             \
        (srP)->r_sp = (uc)->uc_mcontext.rsp;             \
        (srP)->misc.AMD64.r_rbp = (uc)->uc_mcontext.rbp; \
      }

#elif defined(VGP_ppc32_linux)
/* Comments from Paul Mackerras 25 Nov 05:

   > I'm tracking down a problem where V's signal handling doesn't
   > work properly on a ppc440gx running 2.4.20.  The problem is that
   > the ucontext being presented to V's sighandler seems completely
   > bogus.

   > V's kernel headers and hence ucontext layout are derived from
   > 2.6.9.  I compared include/asm-ppc/ucontext.h from 2.4.20 and
   > 2.6.13.

   > Can I just check my interpretation: the 2.4.20 one contains the
   > uc_mcontext field in line, whereas the 2.6.13 one has a pointer
   > to said struct?  And so if V is using the 2.6.13 struct then a
   > 2.4.20 one will make no sense to it.

   Not quite... what is inline in the 2.4.20 version is a
   sigcontext_struct, not an mcontext.  The sigcontext looks like
   this:

     struct sigcontext_struct {
        unsigned long   _unused[4];
        int             signal;
        unsigned long   handler;
        unsigned long   oldmask;
        struct pt_regs  *regs;
     };

   The regs pointer of that struct ends up at the same offset as the
   uc_regs of the 2.6 struct ucontext, and a struct pt_regs is the
   same as the mc_gregs field of the mcontext.  In fact the integer
   regs are followed in memory by the floating point regs on 2.4.20.

   Thus if you are using the 2.6 definitions, it should work on 2.4.20
   provided that you go via uc->uc_regs rather than looking in
   uc->uc_mcontext directly.

   There is another subtlety: 2.4.20 doesn't save the vector regs when
   delivering a signal, and 2.6.x only saves the vector regs if the
   process has ever used an altivec instructions.  If 2.6.x does save
   the vector regs, it sets the MSR_VEC bit in
   uc->uc_regs->mc_gregs[PT_MSR], otherwise it clears it.  That bit
   will always be clear under 2.4.20.  So you can use that bit to tell
   whether uc->uc_regs->mc_vregs is valid. */
#  define VG_UCONTEXT_INSTR_PTR(uc)  ((uc)->uc_regs->mc_gregs[VKI_PT_NIP])
#  define VG_UCONTEXT_STACK_PTR(uc)  ((uc)->uc_regs->mc_gregs[VKI_PT_R1])
#  define VG_UCONTEXT_SYSCALL_SYSRES(uc)                            \
      /* Convert the values in uc_mcontext r3,cr into a SysRes. */  \
      VG_(mk_SysRes_ppc32_linux)(                                   \
         (uc)->uc_regs->mc_gregs[VKI_PT_R3],                        \
         (((uc)->uc_regs->mc_gregs[VKI_PT_CCR] >> 28) & 1)          \
      )
#  define VG_UCONTEXT_TO_UnwindStartRegs(srP, uc)                     \
      { (srP)->r_pc = (ULong)((uc)->uc_regs->mc_gregs[VKI_PT_NIP]);   \
        (srP)->r_sp = (ULong)((uc)->uc_regs->mc_gregs[VKI_PT_R1]);    \
        (srP)->misc.PPC32.r_lr = (uc)->uc_regs->mc_gregs[VKI_PT_LNK]; \
      }

#elif defined(VGP_ppc64_linux)
#  define VG_UCONTEXT_INSTR_PTR(uc)  ((uc)->uc_mcontext.gp_regs[VKI_PT_NIP])
#  define VG_UCONTEXT_STACK_PTR(uc)  ((uc)->uc_mcontext.gp_regs[VKI_PT_R1])
   /* Dubious hack: if there is an error, only consider the lowest 8
      bits of r3.  memcheck/tests/post-syscall shows a case where an
      interrupted syscall should have produced a ucontext with 0x4
      (VKI_EINTR) in r3 but is in fact producing 0x204. */
   /* Awaiting clarification from PaulM.  Evidently 0x204 is
      ERESTART_RESTARTBLOCK, which shouldn't have made it into user
      space. */
   static inline SysRes VG_UCONTEXT_SYSCALL_SYSRES( struct vki_ucontext* uc )
   {
      ULong err = (uc->uc_mcontext.gp_regs[VKI_PT_CCR] >> 28) & 1;
      ULong r3  = uc->uc_mcontext.gp_regs[VKI_PT_R3];
      if (err) r3 &= 0xFF;
      return VG_(mk_SysRes_ppc64_linux)( r3, err );
   }
#  define VG_UCONTEXT_TO_UnwindStartRegs(srP, uc)                       \
      { (srP)->r_pc = (uc)->uc_mcontext.gp_regs[VKI_PT_NIP];            \
        (srP)->r_sp = (uc)->uc_mcontext.gp_regs[VKI_PT_R1];             \
        (srP)->misc.PPC64.r_lr = (uc)->uc_mcontext.gp_regs[VKI_PT_LNK]; \
      }

#elif defined(VGP_arm_linux)
#  define VG_UCONTEXT_INSTR_PTR(uc)       ((uc)->uc_mcontext.arm_pc)
#  define VG_UCONTEXT_STACK_PTR(uc)       ((uc)->uc_mcontext.arm_sp)
#  define VG_UCONTEXT_SYSCALL_SYSRES(uc)                        \
      /* Convert the value in uc_mcontext.rax into a SysRes. */ \
      VG_(mk_SysRes_arm_linux)( (uc)->uc_mcontext.arm_r0 )
#  define VG_UCONTEXT_TO_UnwindStartRegs(srP, uc)       \
      { (srP)->r_pc = (uc)->uc_mcontext.arm_pc;         \
        (srP)->r_sp = (uc)->uc_mcontext.arm_sp;         \
        (srP)->misc.ARM.r14 = (uc)->uc_mcontext.arm_lr; \
        (srP)->misc.ARM.r12 = (uc)->uc_mcontext.arm_ip; \
        (srP)->misc.ARM.r11 = (uc)->uc_mcontext.arm_fp; \
        (srP)->misc.ARM.r7  = (uc)->uc_mcontext.arm_r7; \
      }

#elif defined(VGP_x86_darwin)

   static inline Addr VG_UCONTEXT_INSTR_PTR( void* ucV ) {
      ucontext_t* uc = (ucontext_t*)ucV;
      struct __darwin_mcontext32* mc = uc->uc_mcontext;
      struct __darwin_i386_thread_state* ss = &mc->__ss;
      return ss->__eip;
   }
   static inline Addr VG_UCONTEXT_STACK_PTR( void* ucV ) {
      ucontext_t* uc = (ucontext_t*)ucV;
      struct __darwin_mcontext32* mc = uc->uc_mcontext;
      struct __darwin_i386_thread_state* ss = &mc->__ss;
      return ss->__esp;
   }
   static inline SysRes VG_UCONTEXT_SYSCALL_SYSRES( void* ucV,
                                                    UWord scclass ) {
      /* this is complicated by the problem that there are 3 different
         kinds of syscalls, each with its own return convention.
         NB: scclass is a host word, hence UWord is good for both
         amd64-darwin and x86-darwin */
      ucontext_t* uc = (ucontext_t*)ucV;
      struct __darwin_mcontext32* mc = uc->uc_mcontext;
      struct __darwin_i386_thread_state* ss = &mc->__ss;
      /* duplicates logic in m_syswrap.getSyscallStatusFromGuestState */
      UInt carry = 1 & ss->__eflags;
      UInt err = 0;
      UInt wLO = 0;
      UInt wHI = 0;
      switch (scclass) {
         case VG_DARWIN_SYSCALL_CLASS_UNIX:
            err = carry;
            wLO = ss->__eax;
            wHI = ss->__edx;
            break;
         case VG_DARWIN_SYSCALL_CLASS_MACH:
            wLO = ss->__eax;
            break;
         case VG_DARWIN_SYSCALL_CLASS_MDEP:
            wLO = ss->__eax;
            break;
         default: 
            vg_assert(0);
            break;
      }
      return VG_(mk_SysRes_x86_darwin)( scclass, err ? True : False, 
                                        wHI, wLO );
   }
   static inline
   void VG_UCONTEXT_TO_UnwindStartRegs( UnwindStartRegs* srP,
                                        void* ucV ) {
      ucontext_t* uc = (ucontext_t*)(ucV);
      struct __darwin_mcontext32* mc = uc->uc_mcontext;
      struct __darwin_i386_thread_state* ss = &mc->__ss;
      srP->r_pc = (ULong)(ss->__eip);
      srP->r_sp = (ULong)(ss->__esp);
      srP->misc.X86.r_ebp = (UInt)(ss->__ebp);
   }

#elif defined(VGP_amd64_darwin)

   static inline Addr VG_UCONTEXT_INSTR_PTR( void* ucV ) {
      ucontext_t* uc = (ucontext_t*)ucV;
      struct __darwin_mcontext64* mc = uc->uc_mcontext;
      struct __darwin_x86_thread_state64* ss = &mc->__ss;
      return ss->__rip;
   }
   static inline Addr VG_UCONTEXT_STACK_PTR( void* ucV ) {
      ucontext_t* uc = (ucontext_t*)ucV;
      struct __darwin_mcontext64* mc = uc->uc_mcontext;
      struct __darwin_x86_thread_state64* ss = &mc->__ss;
      return ss->__rsp;
   }
   static inline SysRes VG_UCONTEXT_SYSCALL_SYSRES( void* ucV,
                                                    UWord scclass ) {
      /* This is copied from the x86-darwin case.  I'm not sure if it
	 is correct. */
      ucontext_t* uc = (ucontext_t*)ucV;
      struct __darwin_mcontext64* mc = uc->uc_mcontext;
      struct __darwin_x86_thread_state64* ss = &mc->__ss;
      /* duplicates logic in m_syswrap.getSyscallStatusFromGuestState */
      ULong carry = 1 & ss->__rflags;
      ULong err = 0;
      ULong wLO = 0;
      ULong wHI = 0;
      switch (scclass) {
         case VG_DARWIN_SYSCALL_CLASS_UNIX:
            err = carry;
            wLO = ss->__rax;
            wHI = ss->__rdx;
            break;
         case VG_DARWIN_SYSCALL_CLASS_MACH:
            wLO = ss->__rax;
            break;
         case VG_DARWIN_SYSCALL_CLASS_MDEP:
            wLO = ss->__rax;
            break;
         default: 
            vg_assert(0);
            break;
      }
      return VG_(mk_SysRes_amd64_darwin)( scclass, err ? True : False, 
					  wHI, wLO );
   }
   static inline
   void VG_UCONTEXT_TO_UnwindStartRegs( UnwindStartRegs* srP,
                                        void* ucV ) {
      ucontext_t* uc = (ucontext_t*)ucV;
      struct __darwin_mcontext64* mc = uc->uc_mcontext;
      struct __darwin_x86_thread_state64* ss = &mc->__ss;
      srP->r_pc = (ULong)(ss->__rip);
      srP->r_sp = (ULong)(ss->__rsp);
      srP->misc.AMD64.r_rbp = (ULong)(ss->__rbp);
   }

#elif defined(VGP_s390x_linux)

#  define VG_UCONTEXT_INSTR_PTR(uc)       ((uc)->uc_mcontext.regs.psw.addr)
#  define VG_UCONTEXT_STACK_PTR(uc)       ((uc)->uc_mcontext.regs.gprs[15])
#  define VG_UCONTEXT_FRAME_PTR(uc)       ((uc)->uc_mcontext.regs.gprs[11])
#  define VG_UCONTEXT_SYSCALL_SYSRES(uc)                        \
      VG_(mk_SysRes_s390x_linux)((uc)->uc_mcontext.regs.gprs[2])
#  define VG_UCONTEXT_LINK_REG(uc) ((uc)->uc_mcontext.regs.gprs[14])

#  define VG_UCONTEXT_TO_UnwindStartRegs(srP, uc)        \
      { (srP)->r_pc = (ULong)((uc)->uc_mcontext.regs.psw.addr);    \
        (srP)->r_sp = (ULong)((uc)->uc_mcontext.regs.gprs[15]);    \
        (srP)->misc.S390X.r_fp = (uc)->uc_mcontext.regs.gprs[11];  \
        (srP)->misc.S390X.r_lr = (uc)->uc_mcontext.regs.gprs[14];  \
      }

#elif defined(VGP_mips32_linux)
#  define VG_UCONTEXT_INSTR_PTR(uc)   ((UWord)(((uc)->uc_mcontext.sc_pc)))
#  define VG_UCONTEXT_STACK_PTR(uc)   ((UWord)((uc)->uc_mcontext.sc_regs[29]))
#  define VG_UCONTEXT_FRAME_PTR(uc)       ((uc)->uc_mcontext.sc_regs[30])
#  define VG_UCONTEXT_SYSCALL_NUM(uc)     ((uc)->uc_mcontext.sc_regs[2])
#  define VG_UCONTEXT_SYSCALL_SYSRES(uc)                         \
      /* Convert the value in uc_mcontext.rax into a SysRes. */  \
      VG_(mk_SysRes_mips32_linux)( (uc)->uc_mcontext.sc_regs[2], \
                                   (uc)->uc_mcontext.sc_regs[3], \
                                   (uc)->uc_mcontext.sc_regs[7]) 
 
#  define VG_UCONTEXT_TO_UnwindStartRegs(srP, uc)              \
      { (srP)->r_pc = (uc)->uc_mcontext.sc_pc;                 \
        (srP)->r_sp = (uc)->uc_mcontext.sc_regs[29];           \
        (srP)->misc.MIPS32.r30 = (uc)->uc_mcontext.sc_regs[30]; \
        (srP)->misc.MIPS32.r31 = (uc)->uc_mcontext.sc_regs[31]; \
        (srP)->misc.MIPS32.r28 = (uc)->uc_mcontext.sc_regs[28]; \
      }


#else 
#  error Unknown platform
#endif


/* ------ Macros for pulling stuff out of siginfos ------ */

/* These macros allow use of uniform names when working with
   both the Linux and AIX vki definitions. */
#if defined(VGO_linux)
#  define VKI_SIGINFO_si_addr  _sifields._sigfault._addr
#  define VKI_SIGINFO_si_pid   _sifields._kill._pid
#elif defined(VGO_darwin)
#  define VKI_SIGINFO_si_addr  si_addr
#  define VKI_SIGINFO_si_pid   si_pid
#else
#  error Unknown OS
#endif


/* ---------------------------------------------------------------------
   HIGH LEVEL STUFF TO DO WITH SIGNALS: POLICY (MOSTLY)
   ------------------------------------------------------------------ */

/* ---------------------------------------------------------------------
   Signal state for this process.
   ------------------------------------------------------------------ */


/* Base-ment of these arrays[_VKI_NSIG].

   Valid signal numbers are 1 .. _VKI_NSIG inclusive.
   Rather than subtracting 1 for indexing these arrays, which
   is tedious and error-prone, they are simply dimensioned 1 larger,
   and entry [0] is not used. 
 */


/* -----------------------------------------------------
   Static client signal state (SCSS).  This is the state
   that the client thinks it has the kernel in.  
   SCSS records verbatim the client's settings.  These 
   are mashed around only when SKSS is calculated from it.
   -------------------------------------------------- */

typedef 
   struct {
      void* scss_handler;  /* VKI_SIG_DFL or VKI_SIG_IGN or ptr to
                              client's handler */
      UInt  scss_flags;
      vki_sigset_t scss_mask;
      void* scss_restorer; /* where sigreturn goes */
      void* scss_sa_tramp; /* sa_tramp setting, Darwin only */
      /* re _restorer and _sa_tramp, we merely record the values
         supplied when the client does 'sigaction' and give them back
         when requested.  Otherwise they are simply ignored. */
   }
   SCSS_Per_Signal;

typedef 
   struct {
      /* per-signal info */
      SCSS_Per_Signal scss_per_sig[1+_VKI_NSIG];

      /* Additional elements to SCSS not stored here:
         - for each thread, the thread's blocking mask
         - for each thread in WaitSIG, the set of waited-on sigs
      */
      } 
      SCSS;

static SCSS scss;


/* -----------------------------------------------------
   Static kernel signal state (SKSS).  This is the state
   that we have the kernel in.  It is computed from SCSS.
   -------------------------------------------------- */

/* Let's do: 
     sigprocmask assigns to all thread masks
     so that at least everything is always consistent
   Flags:
     SA_SIGINFO -- we always set it, and honour it for the client
     SA_NOCLDSTOP -- passed to kernel
     SA_ONESHOT or SA_RESETHAND -- pass through
     SA_RESTART -- we observe this but set our handlers to always restart
     SA_NOMASK or SA_NODEFER -- we observe this, but our handlers block everything
     SA_ONSTACK -- pass through
     SA_NOCLDWAIT -- pass through
*/


typedef 
   struct {
      void* skss_handler;  /* VKI_SIG_DFL or VKI_SIG_IGN 
                              or ptr to our handler */
      UInt skss_flags;
      /* There is no skss_mask, since we know that we will always ask
         for all signals to be blocked in our sighandlers. */
      /* Also there is no skss_restorer. */
   }
   SKSS_Per_Signal;

typedef 
   struct {
      SKSS_Per_Signal skss_per_sig[1+_VKI_NSIG];
   } 
   SKSS;

static SKSS skss;

/* returns True if signal is to be ignored. 
   To check this, possibly call gdbserver with tid. */
static Bool is_sig_ign(Int sigNo, ThreadId tid)
{
   vg_assert(sigNo >= 1 && sigNo <= _VKI_NSIG);

   return scss.scss_per_sig[sigNo].scss_handler == VKI_SIG_IGN
      || !VG_(gdbserver_report_signal) (sigNo, tid);
}

/* ---------------------------------------------------------------------
   Compute the SKSS required by the current SCSS.
   ------------------------------------------------------------------ */

static 
void pp_SKSS ( void )
{
   Int sig;
   VG_(printf)("\n\nSKSS:\n");
   for (sig = 1; sig <= _VKI_NSIG; sig++) {
      VG_(printf)("sig %d:  handler %p,  flags 0x%x\n", sig,
                  skss.skss_per_sig[sig].skss_handler,
                  skss.skss_per_sig[sig].skss_flags );

   }
}

/* This is the core, clever bit.  Computation is as follows:

   For each signal
      handler = if client has a handler, then our handler
                else if client is DFL, then our handler as well
                else (client must be IGN)
			then hander is IGN
*/
static
void calculate_SKSS_from_SCSS ( SKSS* dst )
{
   Int   sig;
   UInt  scss_flags;
   UInt  skss_flags;

   for (sig = 1; sig <= _VKI_NSIG; sig++) {
      void *skss_handler;
      void *scss_handler;
      
      scss_handler = scss.scss_per_sig[sig].scss_handler;
      scss_flags   = scss.scss_per_sig[sig].scss_flags;

      switch(sig) {
      case VKI_SIGSEGV:
      case VKI_SIGBUS:
      case VKI_SIGFPE:
      case VKI_SIGILL:
      case VKI_SIGTRAP:
	 /* For these, we always want to catch them and report, even
	    if the client code doesn't. */
	 skss_handler = sync_signalhandler;
	 break;

      case VKI_SIGCONT:
	 /* Let the kernel handle SIGCONT unless the client is actually
	    catching it. */
      case VKI_SIGCHLD:
      case VKI_SIGWINCH:
      case VKI_SIGURG:
         /* For signals which are have a default action of Ignore,
            only set a handler if the client has set a signal handler.
            Otherwise the kernel will interrupt a syscall which
            wouldn't have otherwise been interrupted. */
	 if (scss.scss_per_sig[sig].scss_handler == VKI_SIG_DFL)
	    skss_handler = VKI_SIG_DFL;
	 else if (scss.scss_per_sig[sig].scss_handler == VKI_SIG_IGN)
	    skss_handler = VKI_SIG_IGN;
	 else
	    skss_handler = async_signalhandler;
	 break;

      default:
         // VKI_SIGVG* are runtime variables, so we can't make them            
         // cases in the switch, so we handle them in the 'default' case.
	 if (sig == VG_SIGVGKILL)
	    skss_handler = sigvgkill_handler;
	 else {
	    if (scss_handler == VKI_SIG_IGN)
	       skss_handler = VKI_SIG_IGN;
	    else 
	       skss_handler = async_signalhandler;
	 }
	 break;
      }

      /* Flags */

      skss_flags = 0;

      /* SA_NOCLDSTOP, SA_NOCLDWAIT: pass to kernel */
      skss_flags |= scss_flags & (VKI_SA_NOCLDSTOP | VKI_SA_NOCLDWAIT);

      /* SA_ONESHOT: ignore client setting */
      
      /* SA_RESTART: ignore client setting and always set it for us.
	 Though we never rely on the kernel to restart a
	 syscall, we observe whether it wanted to restart the syscall
	 or not, which is needed by 
         VG_(fixup_guest_state_after_syscall_interrupted) */
      skss_flags |= VKI_SA_RESTART;

      /* SA_NOMASK: ignore it */

      /* SA_ONSTACK: client setting is irrelevant here */
      /* We don't set a signal stack, so ignore */

      /* always ask for SA_SIGINFO */
      skss_flags |= VKI_SA_SIGINFO;

      /* use our own restorer */
      skss_flags |= VKI_SA_RESTORER;

      /* Create SKSS entry for this signal. */
      if (sig != VKI_SIGKILL && sig != VKI_SIGSTOP)
         dst->skss_per_sig[sig].skss_handler = skss_handler;
      else
         dst->skss_per_sig[sig].skss_handler = VKI_SIG_DFL;

      dst->skss_per_sig[sig].skss_flags   = skss_flags;
   }

   /* Sanity checks. */
   vg_assert(dst->skss_per_sig[VKI_SIGKILL].skss_handler == VKI_SIG_DFL);
   vg_assert(dst->skss_per_sig[VKI_SIGSTOP].skss_handler == VKI_SIG_DFL);

   if (0)
      pp_SKSS();
}


/* ---------------------------------------------------------------------
   After a possible SCSS change, update SKSS and the kernel itself.
   ------------------------------------------------------------------ */

// We need two levels of macro-expansion here to convert __NR_rt_sigreturn
// to a number before converting it to a string... sigh.
extern void my_sigreturn(void);

#if defined(VGP_x86_linux)
#  define _MY_SIGRETURN(name) \
   ".text\n" \
   ".globl my_sigreturn\n" \
   "my_sigreturn:\n" \
   "	movl	$" #name ", %eax\n" \
   "	int	$0x80\n" \
   ".previous\n"

#elif defined(VGP_amd64_linux)
#  define _MY_SIGRETURN(name) \
   ".text\n" \
   ".globl my_sigreturn\n" \
   "my_sigreturn:\n" \
   "	movq	$" #name ", %rax\n" \
   "	syscall\n" \
   ".previous\n"

#elif defined(VGP_ppc32_linux)
#  define _MY_SIGRETURN(name) \
   ".text\n" \
   ".globl my_sigreturn\n" \
   "my_sigreturn:\n" \
   "	li	0, " #name "\n" \
   "	sc\n" \
   ".previous\n"

#elif defined(VGP_ppc64_linux)
#  define _MY_SIGRETURN(name) \
   ".align   2\n" \
   ".globl   my_sigreturn\n" \
   ".section \".opd\",\"aw\"\n" \
   ".align   3\n" \
   "my_sigreturn:\n" \
   ".quad    .my_sigreturn,.TOC.@tocbase,0\n" \
   ".previous\n" \
   ".type    .my_sigreturn,@function\n" \
   ".globl   .my_sigreturn\n" \
   ".my_sigreturn:\n" \
   "	li	0, " #name "\n" \
   "	sc\n"

#elif defined(VGP_arm_linux)
#  define _MY_SIGRETURN(name) \
   ".text\n" \
   ".globl my_sigreturn\n" \
   "my_sigreturn:\n\t" \
   "    mov  r7, #" #name "\n\t" \
   "    svc  0x00000000\n" \
   ".previous\n"

#elif defined(VGP_x86_darwin)
#  define _MY_SIGRETURN(name) \
   ".text\n" \
   ".globl my_sigreturn\n" \
   "my_sigreturn:\n" \
   "movl $" VG_STRINGIFY(__NR_DARWIN_FAKE_SIGRETURN) ",%eax\n" \
   "int $0x80"

#elif defined(VGP_amd64_darwin)
   // DDD: todo
#  define _MY_SIGRETURN(name) \
   ".text\n" \
   ".globl my_sigreturn\n" \
   "my_sigreturn:\n" \
   "ud2\n"

#elif defined(VGP_s390x_linux)
#  define _MY_SIGRETURN(name) \
   ".text\n" \
   ".globl my_sigreturn\n" \
   "my_sigreturn:\n" \
   " svc " #name "\n" \
   ".previous\n"

#elif defined(VGP_mips32_linux)
#  define _MY_SIGRETURN(name) \
   ".text\n" \
   "my_sigreturn:\n" \
   "	li	$2, " #name "\n" /* apparently $2 is v0 */ \
   "	syscall\n" \
   ".previous\n"

#else
#  error Unknown platform
#endif

#define MY_SIGRETURN(name)  _MY_SIGRETURN(name)
asm(
   MY_SIGRETURN(__NR_rt_sigreturn)
);


static void handle_SCSS_change ( Bool force_update )
{
   Int  res, sig;
   SKSS skss_old;
   vki_sigaction_toK_t   ksa;
   vki_sigaction_fromK_t ksa_old;

   /* Remember old SKSS and calculate new one. */
   skss_old = skss;
   calculate_SKSS_from_SCSS ( &skss );

   /* Compare the new SKSS entries vs the old ones, and update kernel
      where they differ. */
   for (sig = 1; sig <= VG_(max_signal); sig++) {

      /* Trying to do anything with SIGKILL is pointless; just ignore
         it. */
      if (sig == VKI_SIGKILL || sig == VKI_SIGSTOP)
         continue;

      if (!force_update) {
         if ((skss_old.skss_per_sig[sig].skss_handler
              == skss.skss_per_sig[sig].skss_handler)
             && (skss_old.skss_per_sig[sig].skss_flags
                 == skss.skss_per_sig[sig].skss_flags))
            /* no difference */
            continue;
      }

      ksa.ksa_handler = skss.skss_per_sig[sig].skss_handler;
      ksa.sa_flags    = skss.skss_per_sig[sig].skss_flags;
#     if !defined(VGP_ppc32_linux) && \
         !defined(VGP_x86_darwin) && !defined(VGP_amd64_darwin) && \
         !defined(VGP_mips32_linux)
      ksa.sa_restorer = my_sigreturn;
#     endif
      /* Re above ifdef (also the assertion below), PaulM says:
         The sa_restorer field is not used at all on ppc.  Glibc
         converts the sigaction you give it into a kernel sigaction,
         but it doesn't put anything in the sa_restorer field.
      */

      /* block all signals in handler */
      VG_(sigfillset)( &ksa.sa_mask );
      VG_(sigdelset)( &ksa.sa_mask, VKI_SIGKILL );
      VG_(sigdelset)( &ksa.sa_mask, VKI_SIGSTOP );

      if (VG_(clo_trace_signals) && VG_(clo_verbosity) > 2)
         VG_(dmsg)("setting ksig %d to: hdlr %p, flags 0x%lx, "
                   "mask(msb..lsb) 0x%llx 0x%llx\n",
                   sig, ksa.ksa_handler,
                   (UWord)ksa.sa_flags,
                   _VKI_NSIG_WORDS > 1 ? (ULong)ksa.sa_mask.sig[1] : 0,
                   (ULong)ksa.sa_mask.sig[0]);

      res = VG_(sigaction)( sig, &ksa, &ksa_old );
      vg_assert(res == 0);

      /* Since we got the old sigaction more or less for free, might
         as well extract the maximum sanity-check value from it. */
      if (!force_update) {
         vg_assert(ksa_old.ksa_handler 
                   == skss_old.skss_per_sig[sig].skss_handler);
         vg_assert(ksa_old.sa_flags 
                   == skss_old.skss_per_sig[sig].skss_flags);
#        if !defined(VGP_ppc32_linux) && \
            !defined(VGP_x86_darwin) && !defined(VGP_amd64_darwin) && \
            !defined(VGP_mips32_linux)
         vg_assert(ksa_old.sa_restorer 
                   == my_sigreturn);
#        endif
         VG_(sigaddset)( &ksa_old.sa_mask, VKI_SIGKILL );
         VG_(sigaddset)( &ksa_old.sa_mask, VKI_SIGSTOP );
         vg_assert(VG_(isfullsigset)( &ksa_old.sa_mask ));
      }
   }
}


/* ---------------------------------------------------------------------
   Update/query SCSS in accordance with client requests.
   ------------------------------------------------------------------ */

/* Logic for this alt-stack stuff copied directly from do_sigaltstack
   in kernel/signal.[ch] */

/* True if we are on the alternate signal stack.  */
static Bool on_sig_stack ( ThreadId tid, Addr m_SP )
{
   ThreadState *tst = VG_(get_ThreadState)(tid);

   return (m_SP - (Addr)tst->altstack.ss_sp < (Addr)tst->altstack.ss_size);
}

static Int sas_ss_flags ( ThreadId tid, Addr m_SP )
{
   ThreadState *tst = VG_(get_ThreadState)(tid);

   return (tst->altstack.ss_size == 0 
              ? VKI_SS_DISABLE
              : on_sig_stack(tid, m_SP) ? VKI_SS_ONSTACK : 0);
}


SysRes VG_(do_sys_sigaltstack) ( ThreadId tid, vki_stack_t* ss, vki_stack_t* oss )
{
   Addr m_SP;

   vg_assert(VG_(is_valid_tid)(tid));
   m_SP  = VG_(get_SP)(tid);

   if (VG_(clo_trace_signals))
      VG_(dmsg)("sys_sigaltstack: tid %d, "
                "ss %p{%p,sz=%llu,flags=0x%llx}, oss %p (current SP %p)\n",
                tid, (void*)ss, 
                ss ? ss->ss_sp : 0,
                (ULong)(ss ? ss->ss_size : 0),
                (ULong)(ss ? ss->ss_flags : 0),
                (void*)oss, (void*)m_SP);

   if (oss != NULL) {
      oss->ss_sp    = VG_(threads)[tid].altstack.ss_sp;
      oss->ss_size  = VG_(threads)[tid].altstack.ss_size;
      oss->ss_flags = VG_(threads)[tid].altstack.ss_flags
                      | sas_ss_flags(tid, m_SP);
   }

   if (ss != NULL) {
      if (on_sig_stack(tid, VG_(get_SP)(tid))) {
         return VG_(mk_SysRes_Error)( VKI_EPERM );
      }
      if (ss->ss_flags != VKI_SS_DISABLE 
          && ss->ss_flags != VKI_SS_ONSTACK 
          && ss->ss_flags != 0) {
         return VG_(mk_SysRes_Error)( VKI_EINVAL );
      }
      if (ss->ss_flags == VKI_SS_DISABLE) {
         VG_(threads)[tid].altstack.ss_flags = VKI_SS_DISABLE;
      } else {
         if (ss->ss_size < VKI_MINSIGSTKSZ) {
            return VG_(mk_SysRes_Error)( VKI_ENOMEM );
         }

	 VG_(threads)[tid].altstack.ss_sp    = ss->ss_sp;
	 VG_(threads)[tid].altstack.ss_size  = ss->ss_size;
	 VG_(threads)[tid].altstack.ss_flags = 0;
      }
   }
   return VG_(mk_SysRes_Success)( 0 );
}


SysRes VG_(do_sys_sigaction) ( Int signo, 
                               const vki_sigaction_toK_t* new_act, 
                               vki_sigaction_fromK_t* old_act )
{
   if (VG_(clo_trace_signals))
      VG_(dmsg)("sys_sigaction: sigNo %d, "
                "new %#lx, old %#lx, new flags 0x%llx\n",
                signo, (UWord)new_act, (UWord)old_act,
                (ULong)(new_act ? new_act->sa_flags : 0));

   /* Rule out various error conditions.  The aim is to ensure that if
      when the call is passed to the kernel it will definitely
      succeed. */

   /* Reject out-of-range signal numbers. */
   if (signo < 1 || signo > VG_(max_signal)) goto bad_signo;

   /* don't let them use our signals */
   if ( (signo > VG_SIGVGRTUSERMAX)
	&& new_act
	&& !(new_act->ksa_handler == VKI_SIG_DFL 
             || new_act->ksa_handler == VKI_SIG_IGN) )
      goto bad_signo_reserved;

   /* Reject attempts to set a handler (or set ignore) for SIGKILL. */
   if ( (signo == VKI_SIGKILL || signo == VKI_SIGSTOP)
       && new_act
       && new_act->ksa_handler != VKI_SIG_DFL)
      goto bad_sigkill_or_sigstop;

   /* If the client supplied non-NULL old_act, copy the relevant SCSS
      entry into it. */
   if (old_act) {
      old_act->ksa_handler = scss.scss_per_sig[signo].scss_handler;
      old_act->sa_flags    = scss.scss_per_sig[signo].scss_flags;
      old_act->sa_mask     = scss.scss_per_sig[signo].scss_mask;
#     if !defined(VGP_x86_darwin) && !defined(VGP_amd64_darwin)
      old_act->sa_restorer = scss.scss_per_sig[signo].scss_restorer;
#     endif
   }

   /* And now copy new SCSS entry from new_act. */
   if (new_act) {
      scss.scss_per_sig[signo].scss_handler  = new_act->ksa_handler;
      scss.scss_per_sig[signo].scss_flags    = new_act->sa_flags;
      scss.scss_per_sig[signo].scss_mask     = new_act->sa_mask;

      scss.scss_per_sig[signo].scss_restorer = NULL;
#     if !defined(VGP_x86_darwin) && !defined(VGP_amd64_darwin)
      scss.scss_per_sig[signo].scss_restorer = new_act->sa_restorer;
#     endif

      scss.scss_per_sig[signo].scss_sa_tramp = NULL;
#     if defined(VGP_x86_darwin) || defined(VGP_amd64_darwin)
      scss.scss_per_sig[signo].scss_sa_tramp = new_act->sa_tramp;
#     endif

      VG_(sigdelset)(&scss.scss_per_sig[signo].scss_mask, VKI_SIGKILL);
      VG_(sigdelset)(&scss.scss_per_sig[signo].scss_mask, VKI_SIGSTOP);
   }

   /* All happy bunnies ... */
   if (new_act) {
      handle_SCSS_change( False /* lazy update */ );
   }
   return VG_(mk_SysRes_Success)( 0 );

  bad_signo:
   if (VG_(showing_core_errors)() && !VG_(clo_xml)) {
      VG_(umsg)("Warning: bad signal number %d in sigaction()\n", signo);
   }
   return VG_(mk_SysRes_Error)( VKI_EINVAL );

  bad_signo_reserved:
   if (VG_(showing_core_errors)() && !VG_(clo_xml)) {
      VG_(umsg)("Warning: ignored attempt to set %s handler in sigaction();\n",
                VG_(signame)(signo));
      VG_(umsg)("         the %s signal is used internally by Valgrind\n", 
                VG_(signame)(signo));
   }
   return VG_(mk_SysRes_Error)( VKI_EINVAL );

  bad_sigkill_or_sigstop:
   if (VG_(showing_core_errors)() && !VG_(clo_xml)) {
      VG_(umsg)("Warning: ignored attempt to set %s handler in sigaction();\n",
                VG_(signame)(signo));
      VG_(umsg)("         the %s signal is uncatchable\n", 
                VG_(signame)(signo));
   }
   return VG_(mk_SysRes_Error)( VKI_EINVAL );
}


static
void do_sigprocmask_bitops ( Int vki_how, 
			     vki_sigset_t* orig_set,
			     vki_sigset_t* modifier )
{
   switch (vki_how) {
      case VKI_SIG_BLOCK: 
         VG_(sigaddset_from_set)( orig_set, modifier );
         break;
      case VKI_SIG_UNBLOCK:
         VG_(sigdelset_from_set)( orig_set, modifier );
         break;
      case VKI_SIG_SETMASK:
         *orig_set = *modifier;
         break;
      default:
         VG_(core_panic)("do_sigprocmask_bitops");
	 break;
   }
}

static
HChar* format_sigset ( const vki_sigset_t* set )
{
   static HChar buf[128];
   int w;

   VG_(strcpy)(buf, "");

   for (w = _VKI_NSIG_WORDS - 1; w >= 0; w--)
   {
#     if _VKI_NSIG_BPW == 32
      VG_(sprintf)(buf + VG_(strlen)(buf), "%08llx",
                   set ? (ULong)set->sig[w] : 0);
#     elif _VKI_NSIG_BPW == 64
      VG_(sprintf)(buf + VG_(strlen)(buf), "%16llx",
                   set ? (ULong)set->sig[w] : 0);
#     else
#       error "Unsupported value for _VKI_NSIG_BPW"
#     endif
   }

   return buf;
}

/* 
   This updates the thread's signal mask.  There's no such thing as a
   process-wide signal mask.

   Note that the thread signal masks are an implicit part of SCSS,
   which is why this routine is allowed to mess with them.  
*/
static
void do_setmask ( ThreadId tid,
                  Int how,
                  vki_sigset_t* newset,
		  vki_sigset_t* oldset )
{
   if (VG_(clo_trace_signals))
      VG_(dmsg)("do_setmask: tid = %d how = %d (%s), newset = %p (%s)\n", 
                tid, how,
                how==VKI_SIG_BLOCK ? "SIG_BLOCK" : (
                   how==VKI_SIG_UNBLOCK ? "SIG_UNBLOCK" : (
                      how==VKI_SIG_SETMASK ? "SIG_SETMASK" : "???")),
                newset, newset ? format_sigset(newset) : "NULL" );

   /* Just do this thread. */
   vg_assert(VG_(is_valid_tid)(tid));
   if (oldset) {
      *oldset = VG_(threads)[tid].sig_mask;
      if (VG_(clo_trace_signals))
         VG_(dmsg)("\toldset=%p %s\n", oldset, format_sigset(oldset));
   }
   if (newset) {
      do_sigprocmask_bitops (how, &VG_(threads)[tid].sig_mask, newset );
      VG_(sigdelset)(&VG_(threads)[tid].sig_mask, VKI_SIGKILL);
      VG_(sigdelset)(&VG_(threads)[tid].sig_mask, VKI_SIGSTOP);
      VG_(threads)[tid].tmp_sig_mask = VG_(threads)[tid].sig_mask;
   }
}


SysRes VG_(do_sys_sigprocmask) ( ThreadId tid,
                                 Int how, 
                                 vki_sigset_t* set,
                                 vki_sigset_t* oldset )
{
   switch(how) {
      case VKI_SIG_BLOCK:
      case VKI_SIG_UNBLOCK:
      case VKI_SIG_SETMASK:
         vg_assert(VG_(is_valid_tid)(tid));
         do_setmask ( tid, how, set, oldset );
         return VG_(mk_SysRes_Success)( 0 );

      default:
         VG_(dmsg)("sigprocmask: unknown 'how' field %d\n", how);
         return VG_(mk_SysRes_Error)( VKI_EINVAL );
   }
}


/* ---------------------------------------------------------------------
   LOW LEVEL STUFF TO DO WITH SIGNALS: IMPLEMENTATION
   ------------------------------------------------------------------ */

/* ---------------------------------------------------------------------
   Handy utilities to block/restore all host signals.
   ------------------------------------------------------------------ */

/* Block all host signals, dumping the old mask in *saved_mask. */
static void block_all_host_signals ( /* OUT */ vki_sigset_t* saved_mask )
{
   Int           ret;
   vki_sigset_t block_procmask;
   VG_(sigfillset)(&block_procmask);
   ret = VG_(sigprocmask)
            (VKI_SIG_SETMASK, &block_procmask, saved_mask);
   vg_assert(ret == 0);
}

/* Restore the blocking mask using the supplied saved one. */
static void restore_all_host_signals ( /* IN */ vki_sigset_t* saved_mask )
{
   Int ret;
   ret = VG_(sigprocmask)(VKI_SIG_SETMASK, saved_mask, NULL);
   vg_assert(ret == 0);
}

void VG_(clear_out_queued_signals)( ThreadId tid, vki_sigset_t* saved_mask )
{
   block_all_host_signals(saved_mask);
   if (VG_(threads)[tid].sig_queue != NULL) {
      VG_(arena_free)(VG_AR_CORE, VG_(threads)[tid].sig_queue);
      VG_(threads)[tid].sig_queue = NULL;
   }
   restore_all_host_signals(saved_mask);
}

/* ---------------------------------------------------------------------
   The signal simulation proper.  A simplified version of what the 
   Linux kernel does.
   ------------------------------------------------------------------ */

/* Set up a stack frame (VgSigContext) for the client's signal
   handler. */
static
void push_signal_frame ( ThreadId tid, const vki_siginfo_t *siginfo,
                                       const struct vki_ucontext *uc )
{
   Addr         esp_top_of_frame;
   ThreadState* tst;
   Int		sigNo = siginfo->si_signo;

   vg_assert(sigNo >= 1 && sigNo <= VG_(max_signal));
   vg_assert(VG_(is_valid_tid)(tid));
   tst = & VG_(threads)[tid];

   if (VG_(clo_trace_signals)) {
      VG_(dmsg)("push_signal_frame (thread %d): signal %d\n", tid, sigNo);
      VG_(get_and_pp_StackTrace)(tid, 10);
   }

   if (/* this signal asked to run on an alt stack */
       (scss.scss_per_sig[sigNo].scss_flags & VKI_SA_ONSTACK )
       && /* there is a defined and enabled alt stack, which we're not
             already using.  Logic from get_sigframe in
             arch/i386/kernel/signal.c. */
          sas_ss_flags(tid, VG_(get_SP)(tid)) == 0
      ) {
      esp_top_of_frame 
         = (Addr)(tst->altstack.ss_sp) + tst->altstack.ss_size;
      if (VG_(clo_trace_signals))
         VG_(dmsg)("delivering signal %d (%s) to thread %d: "
                   "on ALT STACK (%p-%p; %ld bytes)\n",
                   sigNo, VG_(signame)(sigNo), tid, tst->altstack.ss_sp,
                   (UChar *)tst->altstack.ss_sp + tst->altstack.ss_size,
                   (Word)tst->altstack.ss_size );

      /* Signal delivery to tools */
      VG_TRACK( pre_deliver_signal, tid, sigNo, /*alt_stack*/True );
      
   } else {
      esp_top_of_frame = VG_(get_SP)(tid) - VG_STACK_REDZONE_SZB;

      /* Signal delivery to tools */
      VG_TRACK( pre_deliver_signal, tid, sigNo, /*alt_stack*/False );
   }

   vg_assert(scss.scss_per_sig[sigNo].scss_handler != VKI_SIG_IGN);
   vg_assert(scss.scss_per_sig[sigNo].scss_handler != VKI_SIG_DFL);

   /* This may fail if the client stack is busted; if that happens,
      the whole process will exit rather than simply calling the
      signal handler. */
   VG_(sigframe_create) (tid, esp_top_of_frame, siginfo, uc,
                         scss.scss_per_sig[sigNo].scss_handler,
                         scss.scss_per_sig[sigNo].scss_flags,
                         &tst->sig_mask,
                         scss.scss_per_sig[sigNo].scss_restorer);
}


const Char *VG_(signame)(Int sigNo)
{
   static Char buf[20];

   switch(sigNo) {
      case VKI_SIGHUP:    return "SIGHUP";
      case VKI_SIGINT:    return "SIGINT";
      case VKI_SIGQUIT:   return "SIGQUIT";
      case VKI_SIGILL:    return "SIGILL";
      case VKI_SIGTRAP:   return "SIGTRAP";
      case VKI_SIGABRT:   return "SIGABRT";
      case VKI_SIGBUS:    return "SIGBUS";
      case VKI_SIGFPE:    return "SIGFPE";
      case VKI_SIGKILL:   return "SIGKILL";
      case VKI_SIGUSR1:   return "SIGUSR1";
      case VKI_SIGUSR2:   return "SIGUSR2";
      case VKI_SIGSEGV:   return "SIGSEGV";
      case VKI_SIGPIPE:   return "SIGPIPE";
      case VKI_SIGALRM:   return "SIGALRM";
      case VKI_SIGTERM:   return "SIGTERM";
#     if defined(VKI_SIGSTKFLT)
      case VKI_SIGSTKFLT: return "SIGSTKFLT";
#     endif
      case VKI_SIGCHLD:   return "SIGCHLD";
      case VKI_SIGCONT:   return "SIGCONT";
      case VKI_SIGSTOP:   return "SIGSTOP";
      case VKI_SIGTSTP:   return "SIGTSTP";
      case VKI_SIGTTIN:   return "SIGTTIN";
      case VKI_SIGTTOU:   return "SIGTTOU";
      case VKI_SIGURG:    return "SIGURG";
      case VKI_SIGXCPU:   return "SIGXCPU";
      case VKI_SIGXFSZ:   return "SIGXFSZ";
      case VKI_SIGVTALRM: return "SIGVTALRM";
      case VKI_SIGPROF:   return "SIGPROF";
      case VKI_SIGWINCH:  return "SIGWINCH";
      case VKI_SIGIO:     return "SIGIO";
#     if defined(VKI_SIGPWR)
      case VKI_SIGPWR:    return "SIGPWR";
#     endif
#     if defined(VKI_SIGUNUSED)
      case VKI_SIGUNUSED: return "SIGUNUSED";
#     endif

#  if defined(VKI_SIGRTMIN) && defined(VKI_SIGRTMAX)
   case VKI_SIGRTMIN ... VKI_SIGRTMAX:
      VG_(sprintf)(buf, "SIGRT%d", sigNo-VKI_SIGRTMIN);
      return buf;
#  endif

   default:
      VG_(sprintf)(buf, "SIG%d", sigNo);
      return buf;
   }
}

/* Hit ourselves with a signal using the default handler */
void VG_(kill_self)(Int sigNo)
{
   Int r;
   vki_sigset_t	         mask, origmask;
   vki_sigaction_toK_t   sa, origsa2;
   vki_sigaction_fromK_t origsa;   

   sa.ksa_handler = VKI_SIG_DFL;
   sa.sa_flags = 0;
#  if !defined(VGP_x86_darwin) && !defined(VGP_amd64_darwin)
   sa.sa_restorer = 0;
#  endif
   VG_(sigemptyset)(&sa.sa_mask);
      
   VG_(sigaction)(sigNo, &sa, &origsa);

   VG_(sigemptyset)(&mask);
   VG_(sigaddset)(&mask, sigNo);
   VG_(sigprocmask)(VKI_SIG_UNBLOCK, &mask, &origmask);

   r = VG_(kill)(VG_(getpid)(), sigNo);
#  if defined(VGO_linux)
   /* This sometimes fails with EPERM on Darwin.  I don't know why. */
   vg_assert(r == 0);
#  endif

   VG_(convert_sigaction_fromK_to_toK)( &origsa, &origsa2 );
   VG_(sigaction)(sigNo, &origsa2, NULL);
   VG_(sigprocmask)(VKI_SIG_SETMASK, &origmask, NULL);
}

// The si_code describes where the signal came from.  Some come from the
// kernel, eg.: seg faults, illegal opcodes.  Some come from the user, eg.:
// from kill() (SI_USER), or timer_settime() (SI_TIMER), or an async I/O
// request (SI_ASYNCIO).  There's lots of implementation-defined leeway in
// POSIX, but the user vs. kernal distinction is what we want here.  We also
// pass in some other details that can help when si_code is unreliable.
static Bool is_signal_from_kernel(ThreadId tid, int signum, int si_code)
{
#  if defined(VGO_linux)
   // On Linux, SI_USER is zero, negative values are from the user, positive
   // values are from the kernel.  There are SI_FROMUSER and SI_FROMKERNEL
   // macros but we don't use them here because other platforms don't have
   // them.
   return ( si_code > VKI_SI_USER ? True : False );

#  elif defined(VGO_darwin)
   // On Darwin 9.6.0, the si_code is completely unreliable.  It should be the
   // case that 0 means "user", and >0 means "kernel".  But:
   // - For SIGSEGV, it seems quite reliable.
   // - For SIGBUS, it's always 2.
   // - For SIGFPE, it's often 0, even for kernel ones (eg.
   //   div-by-integer-zero always gives zero).
   // - For SIGILL, it's unclear.
   // - For SIGTRAP, it's always 1.
   // You can see the "NOTIMP" (not implemented) status of a number of the
   // sub-cases in sys/signal.h.  Hopefully future versions of Darwin will
   // get this right.

   // If we're blocked waiting on a syscall, it must be a user signal, because
   // the kernel won't generate sync signals within syscalls.
   if (VG_(threads)[tid].status == VgTs_WaitSys) {
      return False;

   // If it's a SIGSEGV, use the proper condition, since it's fairly reliable.
   } else if (SIGSEGV == signum) {
      return ( si_code > 0 ? True : False );

   // If it's anything else, assume it's kernel-generated.  Reason being that
   // kernel-generated sync signals are more common, and it's probable that
   // misdiagnosing a user signal as a kernel signal is better than the
   // opposite.
   } else {
      return True;
   }
#  else
#    error Unknown OS
#  endif
}

// This is an arbitrary si_code that we only use internally.  It corresponds
// to the value SI_KERNEL on Linux, but that's not really of any significance
// as far as I can determine.
#define VKI_SEGV_MADE_UP_GPF    0x80

/* 
   Perform the default action of a signal.  If the signal is fatal, it
   marks all threads as needing to exit, but it doesn't actually kill
   the process or thread.

   If we're not being quiet, then print out some more detail about
   fatal signals (esp. core dumping signals).
 */
static void default_action(const vki_siginfo_t *info, ThreadId tid)
{
   Int  sigNo     = info->si_signo;
   Bool terminate = False;	/* kills process         */
   Bool core      = False;	/* kills process w/ core */
   struct vki_rlimit corelim;
   Bool could_core;

   vg_assert(VG_(is_running_thread)(tid));
   
   switch(sigNo) {
      case VKI_SIGQUIT:	/* core */
      case VKI_SIGILL:	/* core */
      case VKI_SIGABRT:	/* core */
      case VKI_SIGFPE:	/* core */
      case VKI_SIGSEGV:	/* core */
      case VKI_SIGBUS:	/* core */
      case VKI_SIGTRAP:	/* core */
      case VKI_SIGXCPU:	/* core */
      case VKI_SIGXFSZ:	/* core */
         terminate = True;
         core = True;
         break;

      case VKI_SIGHUP:	/* term */
      case VKI_SIGINT:	/* term */
      case VKI_SIGKILL:	/* term - we won't see this */
      case VKI_SIGPIPE:	/* term */
      case VKI_SIGALRM:	/* term */
      case VKI_SIGTERM:	/* term */
      case VKI_SIGUSR1:	/* term */
      case VKI_SIGUSR2:	/* term */
      case VKI_SIGIO:	/* term */
#     if defined(VKI_SIGPWR)
      case VKI_SIGPWR:	/* term */
#     endif
      case VKI_SIGSYS:	/* term */
      case VKI_SIGPROF:	/* term */
      case VKI_SIGVTALRM:	/* term */
#     if defined(VKI_SIGRTMIN) && defined(VKI_SIGRTMAX)
      case VKI_SIGRTMIN ... VKI_SIGRTMAX: /* term */
#     endif
         terminate = True;
         break;
   }

   vg_assert(!core || (core && terminate));

   if (VG_(clo_trace_signals))
      VG_(dmsg)("delivering %d (code %d) to default handler; action: %s%s\n",
                sigNo, info->si_code, terminate ? "terminate" : "ignore",
                core ? "+core" : "");

   if (!terminate)
      return;			/* nothing to do */

   could_core = core;

   if (core) {
      /* If they set the core-size limit to zero, don't generate a
	 core file */
	 
      VG_(getrlimit)(VKI_RLIMIT_CORE, &corelim);

      if (corelim.rlim_cur == 0)
	 core = False;
   }

   if ( (VG_(clo_verbosity) > 1 ||
         (could_core && is_signal_from_kernel(tid, sigNo, info->si_code))
        ) &&
        !VG_(clo_xml) ) {
      VG_(umsg)(
         "\n"
         "Process terminating with default action of signal %d (%s)%s\n",
         sigNo, VG_(signame)(sigNo), core ? ": dumping core" : "");

      /* Be helpful - decode some more details about this fault */
      if (is_signal_from_kernel(tid, sigNo, info->si_code)) {
	 const Char *event = NULL;
	 Bool haveaddr = True;

	 switch(sigNo) {
	 case VKI_SIGSEGV:
	    switch(info->si_code) {
	    case VKI_SEGV_MAPERR: event = "Access not within mapped region";
                                  break;
	    case VKI_SEGV_ACCERR: event = "Bad permissions for mapped region";
                                  break;
	    case VKI_SEGV_MADE_UP_GPF:
	       /* General Protection Fault: The CPU/kernel
		  isn't telling us anything useful, but this
		  is commonly the result of exceeding a
		  segment limit. */
	       event = "General Protection Fault"; 
	       haveaddr = False;
	       break;
	    }
#if 0
            {
              HChar buf[110];
              VG_(am_show_nsegments)(0,"post segfault");
              VG_(sprintf)(buf, "/bin/cat /proc/%d/maps", VG_(getpid)());
              VG_(system)(buf);
            }
#endif
	    break;

	 case VKI_SIGILL:
	    switch(info->si_code) {
	    case VKI_ILL_ILLOPC: event = "Illegal opcode"; break;
	    case VKI_ILL_ILLOPN: event = "Illegal operand"; break;
	    case VKI_ILL_ILLADR: event = "Illegal addressing mode"; break;
	    case VKI_ILL_ILLTRP: event = "Illegal trap"; break;
	    case VKI_ILL_PRVOPC: event = "Privileged opcode"; break;
	    case VKI_ILL_PRVREG: event = "Privileged register"; break;
	    case VKI_ILL_COPROC: event = "Coprocessor error"; break;
	    case VKI_ILL_BADSTK: event = "Internal stack error"; break;
	    }
	    break;

	 case VKI_SIGFPE:
	    switch (info->si_code) {
	    case VKI_FPE_INTDIV: event = "Integer divide by zero"; break;
	    case VKI_FPE_INTOVF: event = "Integer overflow"; break;
	    case VKI_FPE_FLTDIV: event = "FP divide by zero"; break;
	    case VKI_FPE_FLTOVF: event = "FP overflow"; break;
	    case VKI_FPE_FLTUND: event = "FP underflow"; break;
	    case VKI_FPE_FLTRES: event = "FP inexact"; break;
	    case VKI_FPE_FLTINV: event = "FP invalid operation"; break;
	    case VKI_FPE_FLTSUB: event = "FP subscript out of range"; break;
	    }
	    break;

	 case VKI_SIGBUS:
	    switch (info->si_code) {
	    case VKI_BUS_ADRALN: event = "Invalid address alignment"; break;
	    case VKI_BUS_ADRERR: event = "Non-existent physical address"; break;
	    case VKI_BUS_OBJERR: event = "Hardware error"; break;
	    }
	    break;
	 } /* switch (sigNo) */

	 if (event != NULL) {
	    if (haveaddr)
               VG_(umsg)(" %s at address %p\n",
                         event, info->VKI_SIGINFO_si_addr);
	    else
               VG_(umsg)(" %s\n", event);
	 }
      }
      /* Print a stack trace.  Be cautious if the thread's SP is in an
         obviously stupid place (not mapped readable) that would
         likely cause a segfault. */
      if (VG_(is_valid_tid)(tid)) {
         Word first_ip_delta = 0;
#if defined(VGO_linux)
         /* Make sure that the address stored in the stack pointer is 
            located in a mapped page. That is not necessarily so. E.g.
            consider the scenario where the stack pointer was decreased
            and now has a value that is just below the end of a page that has
            not been mapped yet. In that case VG_(am_is_valid_for_client)
            will consider the address of the stack pointer invalid and that 
            would cause a back-trace of depth 1 to be printed, instead of a
            full back-trace. */
         if (tid == 1) {           // main thread
            Addr esp  = VG_(get_SP)(tid);
            Addr base = VG_PGROUNDDN(esp - VG_STACK_REDZONE_SZB);
            if (VG_(extend_stack)(base, VG_(threads)[tid].client_stack_szB)) {
               if (VG_(clo_trace_signals))
                  VG_(dmsg)("       -> extended stack base to %#lx\n",
                            VG_PGROUNDDN(esp));
            }
         }
#endif
#if defined(VGA_s390x)
         if (sigNo == VKI_SIGILL) {
            /* The guest instruction address has been adjusted earlier to
               point to the insn following the one that could not be decoded.
               When printing the back-trace here we need to undo that
               adjustment so the first line in the back-trace reports the
               correct address. */
            Addr  addr = (Addr)info->VKI_SIGINFO_si_addr;
            UChar byte = ((UChar *)addr)[0];
            Int   insn_length = ((((byte >> 6) + 1) >> 1) + 1) << 1;

            first_ip_delta = -insn_length;
         }
#endif
         ExeContext* ec = VG_(am_is_valid_for_client)
                             (VG_(get_SP)(tid), sizeof(Addr), VKI_PROT_READ)
                        ? VG_(record_ExeContext)( tid, first_ip_delta )
                      : VG_(record_depth_1_ExeContext)( tid,
                                                        first_ip_delta );
         vg_assert(ec);
         VG_(pp_ExeContext)( ec );
      }
      if (sigNo == VKI_SIGSEGV 
          && info && is_signal_from_kernel(tid, sigNo, info->si_code)
          && info->si_code == VKI_SEGV_MAPERR) {
         VG_(umsg)(" If you believe this happened as a result of a stack\n" );
         VG_(umsg)(" overflow in your program's main thread (unlikely but\n");
         VG_(umsg)(" possible), you can try to increase the size of the\n"  );
         VG_(umsg)(" main thread stack using the --main-stacksize= flag.\n" );
         // FIXME: assumes main ThreadId == 1
         if (VG_(is_valid_tid)(1)) {
            VG_(umsg)(
               " The main thread stack size used in this run was %d.\n",
               (Int)VG_(threads)[1].client_stack_szB);
         }
      }
   }

   if (VG_(is_action_requested)( "Attach to debugger", & VG_(clo_db_attach) )) {
      VG_(start_debugger)( tid );
   }

   if (core) {
      const static struct vki_rlimit zero = { 0, 0 };

      VG_(make_coredump)(tid, info, corelim.rlim_cur);

      /* Make sure we don't get a confusing kernel-generated
	 coredump when we finally exit */
      VG_(setrlimit)(VKI_RLIMIT_CORE, &zero);
   }

   /* stash fatal signal in main thread */
   // what's this for?
   //VG_(threads)[VG_(master_tid)].os_state.fatalsig = sigNo;

   /* everyone dies */
   VG_(nuke_all_threads_except)(tid, VgSrc_FatalSig);
   VG_(threads)[tid].exitreason = VgSrc_FatalSig;
   VG_(threads)[tid].os_state.fatalsig = sigNo;
}

/* 
   This does the business of delivering a signal to a thread.  It may
   be called from either a real signal handler, or from normal code to
   cause the thread to enter the signal handler.

   This updates the thread state, but it does not set it to be
   Runnable.
*/
static void deliver_signal ( ThreadId tid, const vki_siginfo_t *info,
                                           const struct vki_ucontext *uc )
{
   Int			sigNo = info->si_signo;
   SCSS_Per_Signal	*handler = &scss.scss_per_sig[sigNo];
   void			*handler_fn;
   ThreadState		*tst = VG_(get_ThreadState)(tid);

   if (VG_(clo_trace_signals))
      VG_(dmsg)("delivering signal %d (%s):%d to thread %d\n", 
                sigNo, VG_(signame)(sigNo), info->si_code, tid );

   if (sigNo == VG_SIGVGKILL) {
      /* If this is a SIGVGKILL, we're expecting it to interrupt any
	 blocked syscall.  It doesn't matter whether the VCPU state is
	 set to restart or not, because we don't expect it will
	 execute any more client instructions. */
      vg_assert(VG_(is_exiting)(tid));
      return;
   }

   /* If the client specifies SIG_IGN, treat it as SIG_DFL.

      If deliver_signal() is being called on a thread, we want
      the signal to get through no matter what; if they're ignoring
      it, then we do this override (this is so we can send it SIGSEGV,
      etc). */
   handler_fn = handler->scss_handler;
   if (handler_fn == VKI_SIG_IGN) 
      handler_fn = VKI_SIG_DFL;

   vg_assert(handler_fn != VKI_SIG_IGN);

   if (handler_fn == VKI_SIG_DFL) {
      default_action(info, tid);
   } else {
      /* Create a signal delivery frame, and set the client's %ESP and
	 %EIP so that when execution continues, we will enter the
	 signal handler with the frame on top of the client's stack,
	 as it expects.

	 Signal delivery can fail if the client stack is too small or
	 missing, and we can't push the frame.  If that happens,
	 push_signal_frame will cause the whole process to exit when
	 we next hit the scheduler.
      */
      vg_assert(VG_(is_valid_tid)(tid));

      push_signal_frame ( tid, info, uc );

      if (handler->scss_flags & VKI_SA_ONESHOT) {
	 /* Do the ONESHOT thing. */
	 handler->scss_handler = VKI_SIG_DFL;

	 handle_SCSS_change( False /* lazy update */ );
      }

      /* At this point:
	 tst->sig_mask is the current signal mask
	 tst->tmp_sig_mask is the same as sig_mask, unless we're in sigsuspend
	 handler->scss_mask is the mask set by the handler

	 Handler gets a mask of tmp_sig_mask|handler_mask|signo
       */
      tst->sig_mask = tst->tmp_sig_mask;
      if (!(handler->scss_flags & VKI_SA_NOMASK)) {
	 VG_(sigaddset_from_set)(&tst->sig_mask, &handler->scss_mask);
	 VG_(sigaddset)(&tst->sig_mask, sigNo);
	 tst->tmp_sig_mask = tst->sig_mask;
      }
   }

   /* Thread state is ready to go - just add Runnable */
}

static void resume_scheduler(ThreadId tid)
{
   ThreadState *tst = VG_(get_ThreadState)(tid);

   vg_assert(tst->os_state.lwpid == VG_(gettid)());

   if (tst->sched_jmpbuf_valid) {
      /* Can't continue; must longjmp back to the scheduler and thus
         enter the sighandler immediately. */
      VG_MINIMAL_LONGJMP(tst->sched_jmpbuf);
   }
}

static void synth_fault_common(ThreadId tid, Addr addr, Int si_code)
{
   vki_siginfo_t info;

   vg_assert(VG_(threads)[tid].status == VgTs_Runnable);

   VG_(memset)(&info, 0, sizeof(info));
   info.si_signo = VKI_SIGSEGV;
   info.si_code = si_code;
   info.VKI_SIGINFO_si_addr = (void*)addr;

   /* even if gdbserver indicates to ignore the signal, we will deliver it */
   VG_(gdbserver_report_signal) (VKI_SIGSEGV, tid);

   /* If they're trying to block the signal, force it to be delivered */
   if (VG_(sigismember)(&VG_(threads)[tid].sig_mask, VKI_SIGSEGV))
      VG_(set_default_handler)(VKI_SIGSEGV);

   deliver_signal(tid, &info, NULL);
}

// Synthesize a fault where the address is OK, but the page
// permissions are bad.
void VG_(synth_fault_perms)(ThreadId tid, Addr addr)
{
   synth_fault_common(tid, addr, VKI_SEGV_ACCERR);
}

// Synthesize a fault where the address there's nothing mapped at the address.
void VG_(synth_fault_mapping)(ThreadId tid, Addr addr)
{
   synth_fault_common(tid, addr, VKI_SEGV_MAPERR);
}

// Synthesize a misc memory fault.
void VG_(synth_fault)(ThreadId tid)
{
   synth_fault_common(tid, 0, VKI_SEGV_MADE_UP_GPF);
}

// Synthesise a SIGILL.
void VG_(synth_sigill)(ThreadId tid, Addr addr)
{
   vki_siginfo_t info;

   vg_assert(VG_(threads)[tid].status == VgTs_Runnable);

   VG_(memset)(&info, 0, sizeof(info));
   info.si_signo = VKI_SIGILL;
   info.si_code  = VKI_ILL_ILLOPC; /* jrs: no idea what this should be */
   info.VKI_SIGINFO_si_addr = (void*)addr;

   if (VG_(gdbserver_report_signal) (VKI_SIGILL, tid)) {
      resume_scheduler(tid);
      deliver_signal(tid, &info, NULL);
   }
   else
      resume_scheduler(tid);
}

// Synthesise a SIGBUS.
void VG_(synth_sigbus)(ThreadId tid)
{
   vki_siginfo_t info;

   vg_assert(VG_(threads)[tid].status == VgTs_Runnable);

   VG_(memset)(&info, 0, sizeof(info));
   info.si_signo = VKI_SIGBUS;
   /* There are several meanings to SIGBUS (as per POSIX, presumably),
      but the most widely understood is "invalid address alignment",
      so let's use that. */
   info.si_code  = VKI_BUS_ADRALN;
   /* If we knew the invalid address in question, we could put it
      in .si_addr.  Oh well. */
   /* info.VKI_SIGINFO_si_addr = (void*)addr; */

   if (VG_(gdbserver_report_signal) (VKI_SIGBUS, tid)) {
      resume_scheduler(tid);
      deliver_signal(tid, &info, NULL);
   }
   else
      resume_scheduler(tid);
}

// Synthesise a SIGTRAP.
void VG_(synth_sigtrap)(ThreadId tid)
{
   vki_siginfo_t info;
   struct vki_ucontext uc;
#  if defined(VGP_x86_darwin)
   struct __darwin_mcontext32 mc;
#  elif defined(VGP_amd64_darwin)
   struct __darwin_mcontext64 mc;
#  endif

   vg_assert(VG_(threads)[tid].status == VgTs_Runnable);

   VG_(memset)(&info, 0, sizeof(info));
   VG_(memset)(&uc,   0, sizeof(uc));
   info.si_signo = VKI_SIGTRAP;
   info.si_code = VKI_TRAP_BRKPT; /* tjh: only ever called for a brkpt ins */

#  if defined(VGP_mips32_linux) || defined(VGP_mips64_linux)
   /* This is for teq on mips. Teq on mips for ins: 0xXXX1f4 
    * cases VKI_SIGFPE not VKI_SIGTRAP 
   */
   // JRS 2012-Jun-06: commented out until we know we need it
   // This isn't a clean solution; need something that avoids looking
   // at the guest code.
   //UInt *ins = (void*)(vgPlain_threads[tid].arch.vex.guest_PC-4);
   //UInt tcode = (((*ins) >> 6) & ((1 << 10) - 1));
   //if (tcode == VKI_BRK_OVERFLOW || tcode == VKI_BRK_DIVZERO) {
   //   if (tcode == VKI_BRK_DIVZERO)
   //      info.si_code = VKI_FPE_INTDIV;
   //   else
   //      info.si_code = VKI_FPE_INTOVF;
   //   info.si_signo = VKI_SIGFPE;
   //   info.si_errno = 0;
   //   info.VKI_SIGINFO_si_addr 
   //      = (void*)(vgPlain_threads[tid].arch.vex.guest_PC-4);
   //}
#  endif

#  if defined(VGP_x86_linux) || defined(VGP_amd64_linux)
   uc.uc_mcontext.trapno = 3;     /* tjh: this is the x86 trap number
                                          for a breakpoint trap... */
   uc.uc_mcontext.err = 0;        /* tjh: no error code for x86
                                          breakpoint trap... */
#  elif defined(VGP_x86_darwin) || defined(VGP_amd64_darwin)
   /* the same thing, but using Darwin field/struct names */
   VG_(memset)(&mc, 0, sizeof(mc));
   uc.uc_mcontext = &mc;
   uc.uc_mcontext->__es.__trapno = 3;
   uc.uc_mcontext->__es.__err = 0;
#  endif

   /* fixs390: do we need to do anything here for s390 ? */
   if (VG_(gdbserver_report_signal) (VKI_SIGTRAP, tid)) {
      resume_scheduler(tid);
      deliver_signal(tid, &info, &uc);
   }
   else
      resume_scheduler(tid);
}

/* Make a signal pending for a thread, for later delivery.
   VG_(poll_signals) will arrange for it to be delivered at the right
   time. 

   tid==0 means add it to the process-wide queue, and not sent it to a
   specific thread.
*/
static 
void queue_signal(ThreadId tid, const vki_siginfo_t *si)
{
   ThreadState *tst;
   SigQueue *sq;
   vki_sigset_t savedmask;

   tst = VG_(get_ThreadState)(tid);

   /* Protect the signal queue against async deliveries */
   block_all_host_signals(&savedmask);

   if (tst->sig_queue == NULL) {
      tst->sig_queue = VG_(arena_malloc)(VG_AR_CORE, "signals.qs.1",
                                         sizeof(*tst->sig_queue));
      VG_(memset)(tst->sig_queue, 0, sizeof(*tst->sig_queue));
   }
   sq = tst->sig_queue;

   if (VG_(clo_trace_signals))
      VG_(dmsg)("Queueing signal %d (idx %d) to thread %d\n",
                si->si_signo, sq->next, tid);

   /* Add signal to the queue.  If the queue gets overrun, then old
      queued signals may get lost. 

      XXX We should also keep a sigset of pending signals, so that at
      least a non-siginfo signal gets deliviered.
   */
   if (sq->sigs[sq->next].si_signo != 0)
      VG_(umsg)("Signal %d being dropped from thread %d's queue\n",
                sq->sigs[sq->next].si_signo, tid);

   sq->sigs[sq->next] = *si;
   sq->next = (sq->next+1) % N_QUEUED_SIGNALS;

   restore_all_host_signals(&savedmask);
}

/*
   Returns the next queued signal for thread tid which is in "set".
   tid==0 means process-wide signal.  Set si_signo to 0 when the
   signal has been delivered.

   Must be called with all signals blocked, to protect against async
   deliveries.
*/
static vki_siginfo_t *next_queued(ThreadId tid, const vki_sigset_t *set)
{
   ThreadState *tst = VG_(get_ThreadState)(tid);
   SigQueue *sq;
   Int idx;
   vki_siginfo_t *ret = NULL;

   sq = tst->sig_queue;
   if (sq == NULL)
      goto out;
   
   idx = sq->next;
   do {
      if (0)
	 VG_(printf)("idx=%d si_signo=%d inset=%d\n", idx,
		     sq->sigs[idx].si_signo,
                     VG_(sigismember)(set, sq->sigs[idx].si_signo));

      if (sq->sigs[idx].si_signo != 0 
          && VG_(sigismember)(set, sq->sigs[idx].si_signo)) {
	 if (VG_(clo_trace_signals))
            VG_(dmsg)("Returning queued signal %d (idx %d) for thread %d\n",
                      sq->sigs[idx].si_signo, idx, tid);
	 ret = &sq->sigs[idx];
	 goto out;
      }

      idx = (idx + 1) % N_QUEUED_SIGNALS;
   } while(idx != sq->next);
  out:   
   return ret;
}

static int sanitize_si_code(int si_code)
{
#if defined(VGO_linux)
   /* The linux kernel uses the top 16 bits of si_code for it's own
      use and only exports the bottom 16 bits to user space - at least
      that is the theory, but it turns out that there are some kernels
      around that forget to mask out the top 16 bits so we do it here.

      The kernel treats the bottom 16 bits as signed and (when it does
      mask them off) sign extends them when exporting to user space so
      we do the same thing here. */
   return (Short)si_code;
#elif defined(VGO_darwin)
   return si_code;
#else
#  error Unknown OS
#endif
}

/* 
   Receive an async signal from the kernel.

   This should only happen when the thread is blocked in a syscall,
   since that's the only time this set of signals is unblocked.
*/
static 
void async_signalhandler ( Int sigNo,
                           vki_siginfo_t *info, struct vki_ucontext *uc )
{
   ThreadId     tid = VG_(lwpid_to_vgtid)(VG_(gettid)());
   ThreadState* tst = VG_(get_ThreadState)(tid);
   SysRes       sres;

   /* The thread isn't currently running, make it so before going on */
   vg_assert(tst->status == VgTs_WaitSys);
   VG_(acquire_BigLock)(tid, "async_signalhandler");

   info->si_code = sanitize_si_code(info->si_code);

   if (VG_(clo_trace_signals))
      VG_(dmsg)("async signal handler: signal=%d, tid=%d, si_code=%d\n",
                sigNo, tid, info->si_code);

   /* Update thread state properly.  The signal can only have been
      delivered whilst we were in
      coregrind/m_syswrap/syscall-<PLAT>.S, and only then in the
      window between the two sigprocmask calls, since at all other
      times, we run with async signals on the host blocked.  Hence
      make enquiries on the basis that we were in or very close to a
      syscall, and attempt to fix up the guest state accordingly.

      (normal async signals occurring during computation are blocked,
      but periodically polled for using VG_(sigtimedwait_zero), and
      delivered at a point convenient for us.  Hence this routine only
      deals with signals that are delivered to a thread during a
      syscall.) */

   /* First, extract a SysRes from the ucontext_t* given to this
      handler.  If it is subsequently established by
      VG_(fixup_guest_state_after_syscall_interrupted) that the
      syscall was complete but the results had not been committed yet
      to the guest state, then it'll have to commit the results itself
      "by hand", and so we need to extract the SysRes.  Of course if
      the thread was not in that particular window then the
      SysRes will be meaningless, but that's OK too because
      VG_(fixup_guest_state_after_syscall_interrupted) will detect
      that the thread was not in said window and ignore the SysRes. */

   /* To make matters more complex still, on Darwin we need to know
      the "class" of the syscall under consideration in order to be
      able to extract the a correct SysRes.  The class will have been
      saved just before the syscall, by VG_(client_syscall), into this
      thread's tst->arch.vex.guest_SC_CLASS.  Hence: */
#  if defined(VGO_darwin)
   sres = VG_UCONTEXT_SYSCALL_SYSRES(uc, tst->arch.vex.guest_SC_CLASS);
#  else
   sres = VG_UCONTEXT_SYSCALL_SYSRES(uc);
#  endif

   /* (1) */
   VG_(fixup_guest_state_after_syscall_interrupted)(
      tid, 
      VG_UCONTEXT_INSTR_PTR(uc), 
      sres,  
      !!(scss.scss_per_sig[sigNo].scss_flags & VKI_SA_RESTART)
   );

   /* (2) */
   /* Set up the thread's state to deliver a signal */
   if (!is_sig_ign(info->si_signo, tid))
      deliver_signal(tid, info, uc);

   /* It's crucial that (1) and (2) happen in the order (1) then (2)
      and not the other way around.  (1) fixes up the guest thread
      state to reflect the fact that the syscall was interrupted --
      either to restart the syscall or to return EINTR.  (2) then sets
      up the thread state to deliver the signal.  Then we resume
      execution.  First, the signal handler is run, since that's the
      second adjustment we made to the thread state.  If that returns,
      then we resume at the guest state created by (1), viz, either
      the syscall returns EINTR or is restarted.

      If (2) was done before (1) the outcome would be completely
      different, and wrong. */

   /* longjmp back to the thread's main loop to start executing the
      handler. */
   resume_scheduler(tid);

   VG_(core_panic)("async_signalhandler: got unexpected signal "
                   "while outside of scheduler");
}

/* Extend the stack to cover addr.  maxsize is the limit the stack can grow to.

   Returns True on success, False on failure.

   Succeeds without doing anything if addr is already within a segment.

   Failure could be caused by:
   - addr not below a growable segment
   - new stack size would exceed maxsize
   - mmap failed for some other reason
 */
Bool VG_(extend_stack)(Addr addr, UInt maxsize)
{
   SizeT udelta;

   /* Find the next Segment above addr */
   NSegment const* seg
      = VG_(am_find_nsegment)(addr);
   NSegment const* seg_next 
      = seg ? VG_(am_next_nsegment)( (NSegment*)seg, True/*fwds*/ )
            : NULL;

   if (seg && seg->kind == SkAnonC)
      /* addr is already mapped.  Nothing to do. */
      return True;

   /* Check that the requested new base is in a shrink-down
      reservation section which abuts an anonymous mapping that
      belongs to the client. */
   if ( ! (seg
           && seg->kind == SkResvn
           && seg->smode == SmUpper
           && seg_next
           && seg_next->kind == SkAnonC
           && seg->end+1 == seg_next->start))
      return False;

   udelta = VG_PGROUNDUP(seg_next->start - addr);
   VG_(debugLog)(1, "signals", 
                    "extending a stack base 0x%llx down by %lld\n",
                    (ULong)seg_next->start, (ULong)udelta);
   if (! VG_(am_extend_into_adjacent_reservation_client)
            ( (NSegment*)seg_next, -(SSizeT)udelta )) {
      VG_(debugLog)(1, "signals", "extending a stack base: FAILED\n");
      return False;
   }

   /* When we change the main stack, we have to let the stack handling
      code know about it. */
   VG_(change_stack)(VG_(clstk_id), addr, VG_(clstk_end));

   if (VG_(clo_sanity_level) > 2)
      VG_(sanity_check_general)(False);

   return True;
}

static void (*fault_catcher)(Int sig, Addr addr) = NULL;

void VG_(set_fault_catcher)(void (*catcher)(Int, Addr))
{
   if (0)
      VG_(debugLog)(0, "signals", "set fault catcher to %p\n", catcher);
   vg_assert2(NULL == catcher || NULL == fault_catcher,
              "Fault catcher is already registered");

   fault_catcher = catcher;
}

static
void sync_signalhandler_from_user ( ThreadId tid,
         Int sigNo, vki_siginfo_t *info, struct vki_ucontext *uc )
{
   ThreadId qtid;

   /* If some user-process sent us a sync signal (ie. it's not the result
      of a faulting instruction), then how we treat it depends on when it
      arrives... */

   if (VG_(threads)[tid].status == VgTs_WaitSys) {
      /* Signal arrived while we're blocked in a syscall.  This means that
         the client's signal mask was applied.  In other words, so we can't
         get here unless the client wants this signal right now.  This means
         we can simply use the async_signalhandler. */
      if (VG_(clo_trace_signals))
         VG_(dmsg)("Delivering user-sent sync signal %d as async signal\n",
                   sigNo);

      async_signalhandler(sigNo, info, uc);
      VG_(core_panic)("async_signalhandler returned!?\n");

   } else {
      /* Signal arrived while in generated client code, or while running
         Valgrind core code.  That means that every thread has these signals
         unblocked, so we can't rely on the kernel to route them properly, so
         we need to queue them manually. */
      if (VG_(clo_trace_signals))
         VG_(dmsg)("Routing user-sent sync signal %d via queue\n", sigNo);

#     if defined(VGO_linux)
      /* On Linux, first we have to do a sanity check of the siginfo. */
      if (info->VKI_SIGINFO_si_pid == 0) {
         /* There's a per-user limit of pending siginfo signals.  If
            you exceed this, by having more than that number of
            pending signals with siginfo, then new signals are
            delivered without siginfo.  This condition can be caused
            by any unrelated program you're running at the same time
            as Valgrind, if it has a large number of pending siginfo
            signals which it isn't taking delivery of.

            Since we depend on siginfo to work out why we were sent a
            signal and what we should do about it, we really can't
            continue unless we get it. */
         VG_(umsg)("Signal %d (%s) appears to have lost its siginfo; "
                   "I can't go on.\n", sigNo, VG_(signame)(sigNo));
         VG_(printf)(
"  This may be because one of your programs has consumed your ration of\n"
"  siginfo structures.  For more information, see:\n"
"    http://kerneltrap.org/mailarchive/1/message/25599/thread\n"
"  Basically, some program on your system is building up a large queue of\n"
"  pending signals, and this causes the siginfo data for other signals to\n"
"  be dropped because it's exceeding a system limit.  However, Valgrind\n"
"  absolutely needs siginfo for SIGSEGV.  A workaround is to track down the\n"
"  offending program and avoid running it while using Valgrind, but there\n"
"  is no easy way to do this.  Apparently the problem was fixed in kernel\n"
"  2.6.12.\n");

         /* It's a fatal signal, so we force the default handler. */
         VG_(set_default_handler)(sigNo);
         deliver_signal(tid, info, uc);
         resume_scheduler(tid);
         VG_(exit)(99);       /* If we can't resume, then just exit */
      }
#     endif

      qtid = 0;         /* shared pending by default */
#     if defined(VGO_linux)
      if (info->si_code == VKI_SI_TKILL)
         qtid = tid;    /* directed to us specifically */
#     endif
      queue_signal(qtid, info);
   }
}

/* Returns the reported fault address for an exact address */
static Addr fault_mask(Addr in)
{
   /*  We have to use VG_PGROUNDDN because faults on s390x only deliver
       the page address but not the address within a page.
    */
#  if defined(VGA_s390x)
   return VG_PGROUNDDN(in);
#  else
   return in;
#endif
}

/* Returns True if the sync signal was due to the stack requiring extension
   and the extension was successful.
*/
static Bool extend_stack_if_appropriate(ThreadId tid, vki_siginfo_t* info)
{
   Addr fault;
   Addr esp;
   NSegment const* seg;
   NSegment const* seg_next;

   if (info->si_signo != VKI_SIGSEGV)
      return False;

   fault    = (Addr)info->VKI_SIGINFO_si_addr;
   esp      = VG_(get_SP)(tid);
   seg      = VG_(am_find_nsegment)(fault);
   seg_next = seg ? VG_(am_next_nsegment)( (NSegment*)seg, True/*fwds*/ )
                  : NULL;

   if (VG_(clo_trace_signals)) {
      if (seg == NULL)
         VG_(dmsg)("SIGSEGV: si_code=%d faultaddr=%#lx tid=%d ESP=%#lx "
                   "seg=NULL\n",
                   info->si_code, fault, tid, esp);
      else
         VG_(dmsg)("SIGSEGV: si_code=%d faultaddr=%#lx tid=%d ESP=%#lx "
                   "seg=%#lx-%#lx\n",
                   info->si_code, fault, tid, esp, seg->start, seg->end);
   }

   if (info->si_code == VKI_SEGV_MAPERR
       && seg
       && seg->kind == SkResvn
       && seg->smode == SmUpper
       && seg_next
       && seg_next->kind == SkAnonC
       && seg->end+1 == seg_next->start
       && fault >= fault_mask(esp - VG_STACK_REDZONE_SZB)) {
      /* If the fault address is above esp but below the current known
         stack segment base, and it was a fault because there was
         nothing mapped there (as opposed to a permissions fault),
         then extend the stack segment. 
       */
      Addr base = VG_PGROUNDDN(esp - VG_STACK_REDZONE_SZB);
      if (VG_(extend_stack)(base, VG_(threads)[tid].client_stack_szB)) {
         if (VG_(clo_trace_signals))
            VG_(dmsg)("       -> extended stack base to %#lx\n",
                      VG_PGROUNDDN(fault));
         return True;
      } else {
         VG_(umsg)("Stack overflow in thread %d: can't grow stack to %#lx\n",
                   tid, fault);
         return False;
      }
   } else {
      return False;
   }
}

static
void sync_signalhandler_from_kernel ( ThreadId tid,
         Int sigNo, vki_siginfo_t *info, struct vki_ucontext *uc )
{
   /* Check to see if some part of Valgrind itself is interested in faults.
      The fault catcher should never be set whilst we're in generated code, so
      check for that.  AFAIK the only use of the catcher right now is
      memcheck's leak detector. */
   if (fault_catcher) {
      vg_assert(VG_(in_generated_code) == False);

      (*fault_catcher)(sigNo, (Addr)info->VKI_SIGINFO_si_addr);
      /* If the catcher returns, then it didn't handle the fault,
         so carry on panicking. */
   }

   if (extend_stack_if_appropriate(tid, info)) {
      /* Stack extension occurred, so we don't need to do anything else; upon
         returning from this function, we'll restart the host (hence guest)
         instruction. */
   } else {
      /* OK, this is a signal we really have to deal with.  If it came
         from the client's code, then we can jump back into the scheduler
         and have it delivered.  Otherwise it's a Valgrind bug. */
      ThreadState *tst = VG_(get_ThreadState)(tid);

      if (VG_(sigismember)(&tst->sig_mask, sigNo)) {
         /* signal is blocked, but they're not allowed to block faults */
         VG_(set_default_handler)(sigNo);
      }

      if (VG_(in_generated_code)) {
         if (VG_(gdbserver_report_signal) (sigNo, tid)
             || VG_(sigismember)(&tst->sig_mask, sigNo)) {
            /* Can't continue; must longjmp back to the scheduler and thus
               enter the sighandler immediately. */
            deliver_signal(tid, info, uc);
            resume_scheduler(tid);
         }
         else
            resume_scheduler(tid);
      }

      /* If resume_scheduler returns or its our fault, it means we
         don't have longjmp set up, implying that we weren't running
         client code, and therefore it was actually generated by
         Valgrind internally.
       */
      VG_(dmsg)("VALGRIND INTERNAL ERROR: Valgrind received "
                "a signal %d (%s) - exiting\n",
                sigNo, VG_(signame)(sigNo));

      VG_(dmsg)("si_code=%x;  Faulting address: %p;  sp: %#lx\n",
                info->si_code, info->VKI_SIGINFO_si_addr,
                VG_UCONTEXT_STACK_PTR(uc));

      if (0)
         VG_(kill_self)(sigNo);  /* generate a core dump */

      //if (tid == 0)            /* could happen after everyone has exited */
      //  tid = VG_(master_tid);
      vg_assert(tid != 0);

      UnwindStartRegs startRegs;
      VG_(memset)(&startRegs, 0, sizeof(startRegs));

      VG_UCONTEXT_TO_UnwindStartRegs(&startRegs, uc);
      VG_(core_panic_at)("Killed by fatal signal", &startRegs);
   }
}

/* 
   Receive a sync signal from the host. 
*/
static
void sync_signalhandler ( Int sigNo,
                          vki_siginfo_t *info, struct vki_ucontext *uc )
{
   ThreadId tid = VG_(lwpid_to_vgtid)(VG_(gettid)());
   Bool from_user;

   if (0) 
      VG_(printf)("sync_sighandler(%d, %p, %p)\n", sigNo, info, uc);

   vg_assert(info != NULL);
   vg_assert(info->si_signo == sigNo);
   vg_assert(sigNo == VKI_SIGSEGV ||
	     sigNo == VKI_SIGBUS  ||
	     sigNo == VKI_SIGFPE  ||
	     sigNo == VKI_SIGILL  ||
	     sigNo == VKI_SIGTRAP);

   info->si_code = sanitize_si_code(info->si_code);

   from_user = !is_signal_from_kernel(tid, sigNo, info->si_code);

   if (VG_(clo_trace_signals)) {
      VG_(dmsg)("sync signal handler: "
                "signal=%d, si_code=%d, EIP=%#lx, eip=%#lx, from %s\n",
                sigNo, info->si_code, VG_(get_IP)(tid), 
                VG_UCONTEXT_INSTR_PTR(uc),
                ( from_user ? "user" : "kernel" ));
   }
   vg_assert(sigNo >= 1 && sigNo <= VG_(max_signal));

   /* // debug code:
   if (0) {
      VG_(printf)("info->si_signo  %d\n", info->si_signo);
      VG_(printf)("info->si_errno  %d\n", info->si_errno);
      VG_(printf)("info->si_code   %d\n", info->si_code);
      VG_(printf)("info->si_pid    %d\n", info->si_pid);
      VG_(printf)("info->si_uid    %d\n", info->si_uid);
      VG_(printf)("info->si_status %d\n", info->si_status);
      VG_(printf)("info->si_addr   %p\n", info->si_addr);
   }
   */

   /* Figure out if the signal is being sent from outside the process.
      (Why do we care?)  If the signal is from the user rather than the
      kernel, then treat it more like an async signal than a sync signal --
      that is, merely queue it for later delivery. */
   if (from_user) {
      sync_signalhandler_from_user(  tid, sigNo, info, uc);
   } else {
      sync_signalhandler_from_kernel(tid, sigNo, info, uc);
   }
}


/* 
   Kill this thread.  Makes it leave any syscall it might be currently
   blocked in, and return to the scheduler.  This doesn't mark the thread
   as exiting; that's the caller's job.
 */
static void sigvgkill_handler(int signo, vki_siginfo_t *si,
                                         struct vki_ucontext *uc)
{
   ThreadId     tid = VG_(lwpid_to_vgtid)(VG_(gettid)());
   ThreadStatus at_signal = VG_(threads)[tid].status;

   if (VG_(clo_trace_signals))
      VG_(dmsg)("sigvgkill for lwp %d tid %d\n", VG_(gettid)(), tid);

   VG_(acquire_BigLock)(tid, "sigvgkill_handler");

   vg_assert(signo == VG_SIGVGKILL);
   vg_assert(si->si_signo == signo);

   /* jrs 2006 August 3: the following assertion seems incorrect to
      me, and fails on AIX.  sigvgkill could be sent to a thread which
      is runnable - see VG_(nuke_all_threads_except) in the scheduler.
      Hence comment these out ..  

      vg_assert(VG_(threads)[tid].status == VgTs_WaitSys);
      VG_(post_syscall)(tid);

      and instead do:
   */
   if (at_signal == VgTs_WaitSys)
      VG_(post_syscall)(tid);
   /* jrs 2006 August 3 ends */

   resume_scheduler(tid);

   VG_(core_panic)("sigvgkill_handler couldn't return to the scheduler\n");
}

static __attribute((unused))
void pp_ksigaction ( vki_sigaction_toK_t* sa )
{
   Int i;
   VG_(printf)("pp_ksigaction: handler %p, flags 0x%x, restorer %p\n", 
               sa->ksa_handler, 
               (UInt)sa->sa_flags, 
#              if !defined(VGP_x86_darwin) && !defined(VGP_amd64_darwin)
                  sa->sa_restorer
#              else
                  (void*)0
#              endif
              );
   VG_(printf)("pp_ksigaction: { ");
   for (i = 1; i <= VG_(max_signal); i++)
      if (VG_(sigismember(&(sa->sa_mask),i)))
         VG_(printf)("%d ", i);
   VG_(printf)("}\n");
}

/* 
   Force signal handler to default
 */
void VG_(set_default_handler)(Int signo)
{
   vki_sigaction_toK_t sa;   

   sa.ksa_handler = VKI_SIG_DFL;
   sa.sa_flags = 0;
#  if !defined(VGP_x86_darwin) && !defined(VGP_amd64_darwin)
   sa.sa_restorer = 0;
#  endif
   VG_(sigemptyset)(&sa.sa_mask);
      
   VG_(do_sys_sigaction)(signo, &sa, NULL);
}

/* 
   Poll for pending signals, and set the next one up for delivery.
 */
void VG_(poll_signals)(ThreadId tid)
{
   vki_siginfo_t si, *sip;
   vki_sigset_t pollset;
   ThreadState *tst = VG_(get_ThreadState)(tid);
   vki_sigset_t saved_mask;

   /* look for all the signals this thread isn't blocking */
   /* pollset = ~tst->sig_mask */
   VG_(sigcomplementset)( &pollset, &tst->sig_mask );

   block_all_host_signals(&saved_mask); // protect signal queue

   /* First look for any queued pending signals */
   sip = next_queued(tid, &pollset); /* this thread */

   if (sip == NULL)
      sip = next_queued(0, &pollset); /* process-wide */

   /* If there was nothing queued, ask the kernel for a pending signal */
   if (sip == NULL && VG_(sigtimedwait_zero)(&pollset, &si) > 0) {
      if (VG_(clo_trace_signals))
         VG_(dmsg)("poll_signals: got signal %d for thread %d\n",
                   si.si_signo, tid);
      sip = &si;
   }

   if (sip != NULL) {
      /* OK, something to do; deliver it */
      if (VG_(clo_trace_signals))
         VG_(dmsg)("Polling found signal %d for tid %d\n", sip->si_signo, tid);
      if (!is_sig_ign(sip->si_signo, tid))
	 deliver_signal(tid, sip, NULL);
      else if (VG_(clo_trace_signals))
         VG_(dmsg)("   signal %d ignored\n", sip->si_signo);
	 
      sip->si_signo = 0;	/* remove from signal queue, if that's
				   where it came from */
   }

   restore_all_host_signals(&saved_mask);
}

/* At startup, copy the process' real signal state to the SCSS.
   Whilst doing this, block all real signals.  Then calculate SKSS and
   set the kernel to that.  Also initialise DCSS. 
*/
void VG_(sigstartup_actions) ( void )
{
   Int i, ret, vKI_SIGRTMIN;
   vki_sigset_t saved_procmask;
   vki_sigaction_fromK_t sa;

   VG_(memset)(&scss, 0, sizeof(scss));
   VG_(memset)(&skss, 0, sizeof(skss));

#  if defined(VKI_SIGRTMIN)
   vKI_SIGRTMIN = VKI_SIGRTMIN;
#  else
   vKI_SIGRTMIN = 0; /* eg Darwin */
#  endif

   /* VG_(printf)("SIGSTARTUP\n"); */
   /* Block all signals.  saved_procmask remembers the previous mask,
      which the first thread inherits.
   */
   block_all_host_signals( &saved_procmask );

   /* Copy per-signal settings to SCSS. */
   for (i = 1; i <= _VKI_NSIG; i++) {
      /* Get the old host action */
      ret = VG_(sigaction)(i, NULL, &sa);

#     if defined(VGP_x86_darwin) || defined(VGP_amd64_darwin)
      /* apparently we may not even ask about the disposition of these
         signals, let alone change them */
      if (ret != 0 && (i == VKI_SIGKILL || i == VKI_SIGSTOP))
         continue;
#     endif

      if (ret != 0)
	 break;

      /* Try setting it back to see if this signal is really
	 available */
      if (vKI_SIGRTMIN > 0 /* it actually exists on this platform */
          && i >= vKI_SIGRTMIN) {
         vki_sigaction_toK_t tsa, sa2;

	 tsa.ksa_handler = (void *)sync_signalhandler;
	 tsa.sa_flags = VKI_SA_SIGINFO;
#        if !defined(VGP_x86_darwin) && !defined(VGP_amd64_darwin)
	 tsa.sa_restorer = 0;
#        endif
	 VG_(sigfillset)(&tsa.sa_mask);

	 /* try setting it to some arbitrary handler */
	 if (VG_(sigaction)(i, &tsa, NULL) != 0) {
	    /* failed - not really usable */
	    break;
	 }

         VG_(convert_sigaction_fromK_to_toK)( &sa, &sa2 );
	 ret = VG_(sigaction)(i, &sa2, NULL);
	 vg_assert(ret == 0);
      }

      VG_(max_signal) = i;

      if (VG_(clo_trace_signals) && VG_(clo_verbosity) > 2)
         VG_(printf)("snaffling handler 0x%lx for signal %d\n", 
                     (Addr)(sa.ksa_handler), i );

      scss.scss_per_sig[i].scss_handler  = sa.ksa_handler;
      scss.scss_per_sig[i].scss_flags    = sa.sa_flags;
      scss.scss_per_sig[i].scss_mask     = sa.sa_mask;

      scss.scss_per_sig[i].scss_restorer = NULL;
#     if !defined(VGP_x86_darwin) && !defined(VGP_amd64_darwin)
      scss.scss_per_sig[i].scss_restorer = sa.sa_restorer;
#     endif

      scss.scss_per_sig[i].scss_sa_tramp = NULL;
#     if defined(VGP_x86_darwin) || defined(VGP_amd64_darwin)
      scss.scss_per_sig[i].scss_sa_tramp = NULL;
      /*sa.sa_tramp;*/
      /* We can't know what it was, because Darwin's sys_sigaction
         doesn't tell us. */
#     endif
   }

   if (VG_(clo_trace_signals))
      VG_(dmsg)("Max kernel-supported signal is %d\n", VG_(max_signal));

   /* Our private internal signals are treated as ignored */
   scss.scss_per_sig[VG_SIGVGKILL].scss_handler = VKI_SIG_IGN;
   scss.scss_per_sig[VG_SIGVGKILL].scss_flags   = VKI_SA_SIGINFO;
   VG_(sigfillset)(&scss.scss_per_sig[VG_SIGVGKILL].scss_mask);

   /* Copy the process' signal mask into the root thread. */
   vg_assert(VG_(threads)[1].status == VgTs_Init);
   for (i = 2; i < VG_N_THREADS; i++)
      vg_assert(VG_(threads)[i].status == VgTs_Empty);

   VG_(threads)[1].sig_mask = saved_procmask;
   VG_(threads)[1].tmp_sig_mask = saved_procmask;

   /* Calculate SKSS and apply it.  This also sets the initial kernel
      mask we need to run with. */
   handle_SCSS_change( True /* forced update */ );

   /* Leave with all signals still blocked; the thread scheduler loop
      will set the appropriate mask at the appropriate time. */
}

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
