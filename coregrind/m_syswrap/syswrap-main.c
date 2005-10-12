
/*--------------------------------------------------------------------*/
/*--- Handle system calls.                          syswrap-main.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2005 Julian Seward 
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

#include "libvex_guest_offsets.h"
#include "pub_core_basics.h"
#include "pub_core_threadstate.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcprint.h"
#include "pub_core_libcproc.h"      // For VG_(getpid)()
#include "pub_core_libcsignal.h"
#include "pub_core_scheduler.h"     // For VG_(set_sleeping), VG_(set_running),
                                    //   and VG_(vg_yield)
#include "pub_core_stacktrace.h"    // For VG_(get_and_pp_StackTrace)()
#include "pub_core_tooliface.h"
#include "pub_core_options.h"
#include "pub_core_signals.h"       // For VG_SIGVGKILL, VG_(poll_signals)
#include "pub_core_syscall.h"
#include "pub_core_syswrap.h"

#include "priv_types_n_macros.h"
#include "priv_syswrap-main.h"


/* Useful info which needs to be recorded somewhere:
   Use of registers in syscalls (on linux) is:

          NUM  ARG1  ARG2  ARG3  ARG4  ARG5  ARG6  RESULT
   x86    eax  ebx   ecx   edx   esi   edi   ebp   eax       (== NUM)
   amd64  rax  rdi   rsi   rdx   r10   r8    r9    rax       (== NUM)
   ppc32  r0   r3    r4    r5    r6    r7    r8    r3+CR0.SO (== ARG1)
*/

/* This is the top level of the system-call handler module.  All
   system calls are channelled through here, doing two things:

   * notify the tool of the events (mem/reg reads, writes) happening

   * perform the syscall, usually by passing it along to the kernel
     unmodified.

   A magical piece of assembly code, do_syscall_for_client_WRK, in
   syscall-$PLATFORM.S does the tricky bit of passing a syscall to the
   kernel, whilst having the simulator retain control.
*/

/* The main function is VG_(client_syscall).  The simulation calls it
   whenever a client thread wants to do a syscall.  The following is a
   sketch of what it does.

   * First, it rounds up the syscall number and args (which is a
     platform dependent activity) and puts them in a struct ("args")
     and also a copy in "orig_args".

     The pre/post wrappers refer to these structs and so no longer
     need magic macros to access any specific registers.  This struct
     is stored in thread-specific storage.


   * The pre-wrapper is called, passing it a pointer to struct
     "args".


   * The pre-wrapper examines the args and pokes the tool
     appropriately.  It may modify the args; this is why "orig_args"
     is also stored.

     The pre-wrapper may choose to 'do' the syscall itself, and
     concludes one of three outcomes:

       Success(N)    -- syscall is already complete, with success;
                        result is N

       Fail(N)       -- syscall is already complete, with failure;
                        error code is N

       HandToKernel  -- (the usual case): this needs to be given to
                        the kernel to be done, using the values in
                        the possibly-modified "args" struct.

     In addition, the pre-wrapper may set some flags:

       MayBlock   -- only applicable when outcome==HandToKernel

       PostOnFail -- only applicable when outcome==HandToKernel or Fail


   * If the pre-outcome is HandToKernel, the syscall is duly handed
     off to the kernel (perhaps involving some thread switchery, but
     that's not important).  This reduces the possible set of outcomes
     to either Success(N) or Fail(N).


   * The outcome (Success(N) or Fail(N)) is written back to the guest
     register(s).  This is platform specific:

     x86:    Success(N) ==>  eax = N
             Fail(N)    ==>  eax = -N

     ditto amd64

     ppc32:  Success(N) ==>  r3 = N, CR0.SO = 0
             Fail(N) ==>     r3 = N, CR0.SO = 1

   * The post wrapper is called if:

     - it exists, and
     - outcome==Success or (outcome==Fail and PostOnFail is set)

     The post wrapper is passed the adulterated syscall args (struct
     "args"), and the syscall outcome (viz, Success(N) or Fail(N)).

   There are several other complications, primarily to do with
   syscalls getting interrupted, explained in comments in the code.
*/

/* CAVEATS for writing wrappers.  It is important to follow these!

   The macros defined in priv_types_n_macros.h are designed to help
   decouple the wrapper logic from the actual representation of
   syscall args/results, since these wrappers are designed to work on
   multiple platforms.

   Sometimes a PRE wrapper will complete the syscall itself, without
   handing it to the kernel.  It will use one of SET_STATUS_Success,
   SET_STATUS_Failure or SET_STATUS_from_SysRes to set the return
   value.  It is critical to appreciate that use of the macro does not
   immediately cause the underlying guest state to be updated -- that
   is done by the driver logic in this file, when the wrapper returns.

   As a result, PRE wrappers of the following form will malfunction:

   PRE(fooble) 
   {
      ... do stuff ...
      SET_STATUS_Somehow(...)

      // do something that assumes guest state is up to date
   }

   In particular, direct or indirect calls to VG_(poll_signals) after
   setting STATUS can cause the guest state to be read (in order to
   build signal frames).  Do not do this.  If you want a signal poll
   after the syscall goes through, do "*flags |= SfPollAfter" and the
   driver logic will do it for you.

   -----------

   Another critical requirement following introduction of new address
   space manager (JRS, 20050923):

   In a situation where the mappedness of memory has changed, aspacem
   should be notified BEFORE the tool.  Hence the following is
   correct:

      Bool d = VG_(am_notify_munmap)(s->start, s->end+1 - s->start);
      VG_TRACK( die_mem_munmap, s->start, s->end+1 - s->start );
      if (d)
         VG_(discard_translations)(s->start, s->end+1 - s->start);

   whilst this is wrong:

      VG_TRACK( die_mem_munmap, s->start, s->end+1 - s->start );
      Bool d = VG_(am_notify_munmap)(s->start, s->end+1 - s->start);
      if (d)
         VG_(discard_translations)(s->start, s->end+1 - s->start);

   The reason is that the tool may itself ask aspacem for more shadow
   memory as a result of the VG_TRACK call.  In such a situation it is
   critical that aspacem's segment array is up to date -- hence the
   need to notify aspacem first.

   -----------

   Also .. take care to call VG_(discard_translations) whenever
   memory with execute permissions is unmapped.
*/


/* ---------------------------------------------------------------------
   Do potentially blocking syscall for the client, and mess with 
   signal masks at the same time. 
   ------------------------------------------------------------------ */

/* Perform a syscall on behalf of a client thread, using a specific
   signal mask.  On completion, the signal mask is set to restore_mask
   (which presumably blocks almost everything).  If a signal happens
   during the syscall, the handler should call
   VG_(fixup_guest_state_after_syscall_interrupted) to adjust the
   thread's context to do the right thing.

   The _WRK function is handwritten assembly, implemented per-platform
   in coregrind/m_syswrap/syscall-$PLAT.S.  It has some very magic
   properties.  See comments at the top of
   VG_(fixup_guest_state_after_syscall_interrupted) below for details.
*/
extern
void ML_(do_syscall_for_client_WRK)( Int syscallno, 
                                     void* guest_state,
                                     const vki_sigset_t *syscall_mask,
                                     const vki_sigset_t *restore_mask,
                                     Int nsigwords );

static
void do_syscall_for_client ( Int syscallno,
                             ThreadState* tst,
                             const vki_sigset_t* syscall_mask )
{
   vki_sigset_t saved;
   ML_(do_syscall_for_client_WRK)(
      syscallno, &tst->arch.vex, 
      syscall_mask, &saved, _VKI_NSIG_WORDS * sizeof(UWord)
   );
}



/* ---------------------------------------------------------------------
   Impedance matchers and misc helpers
   ------------------------------------------------------------------ */

static
Bool eq_SyscallArgs ( SyscallArgs* a1, SyscallArgs* a2 )
{
   return a1->sysno == a2->sysno
          && a1->arg1 == a2->arg1
          && a1->arg2 == a2->arg2
          && a1->arg3 == a2->arg3
          && a1->arg4 == a2->arg4
          && a1->arg5 == a2->arg5
          && a1->arg6 == a2->arg6;
}

static
Bool eq_SyscallStatus ( SyscallStatus* s1, SyscallStatus* s2 )
{
   return s1->what == s2->what 
          && s1->val == s2->val;
}


/* Convert between SysRet and SyscallStatus, to the extent possible. */

/* This is unused. */
/*
static
SysRes convert_SyscallStatus_to_SysRes ( SyscallStatus status )
{
   SysRes res;
   vg_assert(status.what == SsSuccess || status.what == SsFailure);
   res.isError = status.what == SsFailure;
   res.val     = status.val;
   return res;
}
*/

static
SyscallStatus convert_SysRes_to_SyscallStatus ( SysRes res )
{
   SyscallStatus status;
   status.what = res.isError ? SsFailure : SsSuccess;
   status.val  = res.val;
   return status;
}


/* Impedance matchers.  These convert syscall arg or result data from
   the platform-specific in-guest-state format to the canonical
   formats, and back. */

static 
void getSyscallArgsFromGuestState ( /*OUT*/SyscallArgs*       canonical,
                                    /*IN*/ VexGuestArchState* gst_vanilla )
{
#if defined(VGP_x86_linux)
   VexGuestX86State* gst = (VexGuestX86State*)gst_vanilla;
   canonical->sysno = gst->guest_EAX;
   canonical->arg1  = gst->guest_EBX;
   canonical->arg2  = gst->guest_ECX;
   canonical->arg3  = gst->guest_EDX;
   canonical->arg4  = gst->guest_ESI;
   canonical->arg5  = gst->guest_EDI;
   canonical->arg6  = gst->guest_EBP;

#elif defined(VGP_amd64_linux)
   VexGuestAMD64State* gst = (VexGuestAMD64State*)gst_vanilla;
   canonical->sysno = gst->guest_RAX;
   canonical->arg1  = gst->guest_RDI;
   canonical->arg2  = gst->guest_RSI;
   canonical->arg3  = gst->guest_RDX;
   canonical->arg4  = gst->guest_R10;
   canonical->arg5  = gst->guest_R8;
   canonical->arg6  = gst->guest_R9;

#elif defined(VGP_ppc32_linux)
   VexGuestPPC32State* gst = (VexGuestPPC32State*)gst_vanilla;
   canonical->sysno = gst->guest_GPR0;
   canonical->arg1  = gst->guest_GPR3;
   canonical->arg2  = gst->guest_GPR4;
   canonical->arg3  = gst->guest_GPR5;
   canonical->arg4  = gst->guest_GPR6;
   canonical->arg5  = gst->guest_GPR7;
   canonical->arg6  = gst->guest_GPR8;

#else
#  error "getSyscallArgsFromGuestState: unknown arch"
#endif
}

static 
void putSyscallArgsIntoGuestState ( /*IN*/ SyscallArgs*       canonical,
                                    /*OUT*/VexGuestArchState* gst_vanilla )
{
#if defined(VGP_x86_linux)
   VexGuestX86State* gst = (VexGuestX86State*)gst_vanilla;
   gst->guest_EAX = canonical->sysno;
   gst->guest_EBX = canonical->arg1;
   gst->guest_ECX = canonical->arg2;
   gst->guest_EDX = canonical->arg3;
   gst->guest_ESI = canonical->arg4;
   gst->guest_EDI = canonical->arg5;
   gst->guest_EBP = canonical->arg6;

#elif defined(VGP_amd64_linux)
   VexGuestAMD64State* gst = (VexGuestAMD64State*)gst_vanilla;
   gst->guest_RAX = canonical->sysno;
   gst->guest_RDI = canonical->arg1;
   gst->guest_RSI = canonical->arg2;
   gst->guest_RDX = canonical->arg3;
   gst->guest_R10 = canonical->arg4;
   gst->guest_R8  = canonical->arg5;
   gst->guest_R9  = canonical->arg6;

#elif defined(VGP_ppc32_linux)
   VexGuestPPC32State* gst = (VexGuestPPC32State*)gst_vanilla;
   gst->guest_GPR0 = canonical->sysno;
   gst->guest_GPR3 = canonical->arg1;
   gst->guest_GPR4 = canonical->arg2;
   gst->guest_GPR5 = canonical->arg3;
   gst->guest_GPR6 = canonical->arg4;
   gst->guest_GPR7 = canonical->arg5;
   gst->guest_GPR8 = canonical->arg6;

#else
#  error "putSyscallArgsIntoGuestState: unknown arch"
#endif
}

static
void getSyscallStatusFromGuestState ( /*OUT*/SyscallStatus*     canonical,
                                      /*IN*/ VexGuestArchState* gst_vanilla )
{
#if defined(VGP_x86_linux)
   VexGuestX86State* gst = (VexGuestX86State*)gst_vanilla;
   Int               i   = (Int)gst->guest_EAX;
   canonical->what = i >= -4095 && i <= -1  ? SsFailure  : SsSuccess;
   canonical->val  = (UWord)(canonical->what==SsFailure ? -i : i);

#elif defined(VGP_amd64_linux)
   VexGuestAMD64State* gst = (VexGuestAMD64State*)gst_vanilla;
   Long                i   = (Long)gst->guest_RAX;
   canonical->what = i >= -4095 && i <= -1  ? SsFailure  : SsSuccess;
   canonical->val  = (UWord)(canonical->what==SsFailure ? -i : i);

#elif defined(VGP_ppc32_linux)
   VexGuestPPC32State* gst = (VexGuestPPC32State*)gst_vanilla;
   UInt                cr  = LibVEX_GuestPPC32_get_CR( gst );
   UInt                err = (cr >> 28) & 1;  // CR0.SO
   canonical->what = (err == 1)  ? SsFailure  : SsSuccess;
   canonical->val  = (UWord)gst->guest_GPR3;

#else
#  error "getSyscallStatusFromGuestState: unknown arch"
#endif
}

static 
void putSyscallStatusIntoGuestState ( /*IN*/ SyscallStatus*     canonical,
                                      /*OUT*/VexGuestArchState* gst_vanilla )
{
#if defined(VGP_x86_linux)
   VexGuestX86State* gst = (VexGuestX86State*)gst_vanilla;
   vg_assert(canonical->what == SsSuccess 
             || canonical->what == SsFailure);
   if (canonical->what == SsFailure) {
      /* This isn't exactly right, in that really a Failure with res
         not in the range 1 .. 4095 is unrepresentable in the
         Linux-x86 scheme.  Oh well. */
      gst->guest_EAX = - (Int)canonical->val;
   } else {
      gst->guest_EAX = canonical->val;
   }
#elif defined(VGP_amd64_linux)
   VexGuestAMD64State* gst = (VexGuestAMD64State*)gst_vanilla;
   vg_assert(canonical->what == SsSuccess 
             || canonical->what == SsFailure);
   if (canonical->what == SsFailure) {
      /* This isn't exactly right, in that really a Failure with res
         not in the range 1 .. 4095 is unrepresentable in the
         Linux-x86 scheme.  Oh well. */
      gst->guest_RAX = - (Long)canonical->val;
   } else {
      gst->guest_RAX = canonical->val;
   }

#elif defined(VGP_ppc32_linux)
   VexGuestPPC32State* gst = (VexGuestPPC32State*)gst_vanilla;
   UInt old_cr = LibVEX_GuestPPC32_get_CR(gst);

   vg_assert(canonical->what == SsSuccess 
             || canonical->what == SsFailure);

   gst->guest_GPR3 = canonical->val;

   if (canonical->what == SsFailure) {
      /* set CR0.SO */
      LibVEX_GuestPPC32_put_CR( old_cr | (1<<28), gst );
   } else {
      /* clear CR0.SO */
      LibVEX_GuestPPC32_put_CR( old_cr & ~(1<<28), gst );
   }

#else
#  error "putSyscallStatusIntoGuestState: unknown arch"
#endif
}


/* Tell me the offsets in the guest state of the syscall params, so
   that the scalar argument checkers don't have to have this info
   hardwired. */

static
void getSyscallArgLayout ( /*OUT*/SyscallArgLayout* layout )
{
#if defined(VGP_x86_linux)
   layout->o_sysno  = OFFSET_x86_EAX;
   layout->o_arg1   = OFFSET_x86_EBX;
   layout->o_arg2   = OFFSET_x86_ECX;
   layout->o_arg3   = OFFSET_x86_EDX;
   layout->o_arg4   = OFFSET_x86_ESI;
   layout->o_arg5   = OFFSET_x86_EDI;
   layout->o_arg6   = OFFSET_x86_EBP;
   layout->o_retval = OFFSET_x86_EAX;

#elif defined(VGP_amd64_linux)
   layout->o_sysno  = OFFSET_amd64_RAX;
   layout->o_arg1   = OFFSET_amd64_RDI;
   layout->o_arg2   = OFFSET_amd64_RSI;
   layout->o_arg3   = OFFSET_amd64_RDX;
   layout->o_arg4   = OFFSET_amd64_R10;
   layout->o_arg5   = OFFSET_amd64_R8;
   layout->o_arg6   = OFFSET_amd64_R9;
   layout->o_retval = OFFSET_amd64_RAX;

#elif defined(VGP_ppc32_linux)
   layout->o_sysno  = OFFSET_ppc32_GPR0;
   layout->o_arg1   = OFFSET_ppc32_GPR3;
   layout->o_arg2   = OFFSET_ppc32_GPR4;
   layout->o_arg3   = OFFSET_ppc32_GPR5;
   layout->o_arg4   = OFFSET_ppc32_GPR6;
   layout->o_arg5   = OFFSET_ppc32_GPR7;
   layout->o_arg6   = OFFSET_ppc32_GPR8;
   layout->o_retval = OFFSET_ppc32_GPR3;

#else
#  error "getSyscallLayout: unknown arch"
#endif
}


/* ---------------------------------------------------------------------
   The main driver logic
   ------------------------------------------------------------------ */

/* Finding the handlers for a given syscall, or faking up one
   when no handler is found. */

static 
void bad_before ( ThreadId              tid,
                  SyscallArgLayout*     layout,
                  /*MOD*/SyscallArgs*   args,
                  /*OUT*/SyscallStatus* status,
                  /*OUT*/UWord*         flags )
{
   VG_(message)
      (Vg_DebugMsg,"WARNING: unhandled syscall: %llu", (ULong)args->sysno);
   if (VG_(clo_verbosity) > 1) {
      VG_(get_and_pp_StackTrace)(tid, VG_(clo_backtrace_size));
   }
   VG_(message)
      (Vg_DebugMsg,"You may be able to write your own handler.");
   VG_(message)
      (Vg_DebugMsg,"Read the file README_MISSING_SYSCALL_OR_IOCTL.");

   SET_STATUS_Failure(VKI_ENOSYS);
}

static SyscallTableEntry bad_sys =
   { bad_before, NULL };

static const SyscallTableEntry* get_syscall_entry ( UInt syscallno )
{
   const SyscallTableEntry* sys = &bad_sys;

   if (syscallno < ML_(syscall_table_size) &&
       ML_(syscall_table)[syscallno].before != NULL)
      sys = &ML_(syscall_table)[syscallno];

   return sys;
}


/* Add and remove signals from mask so that we end up telling the
   kernel the state we actually want rather than what the client
   wants. */
static void sanitize_client_sigmask(ThreadId tid, vki_sigset_t *mask)
{
   VG_(sigdelset)(mask, VKI_SIGKILL);
   VG_(sigdelset)(mask, VKI_SIGSTOP);
   VG_(sigdelset)(mask, VG_SIGVGKILL); /* never block */
}

typedef
   struct {
      SyscallArgs   orig_args;
      SyscallArgs   args;
      SyscallStatus status;
      UWord         flags;
   }
   SyscallInfo;

SyscallInfo syscallInfo[VG_N_THREADS];


/* The scheduler needs to be able to zero out these records after a
   fork, hence this is exported from m_syswrap. */
void VG_(clear_syscallInfo) ( Int tid )
{
   vg_assert(tid >= 0 && tid < VG_N_THREADS);
   VG_(memset)( & syscallInfo[tid], 0, sizeof( syscallInfo[tid] ));
   syscallInfo[tid].status.what = SsIdle;
}

static void ensure_initialised ( void )
{
   Int i;
   static Bool init_done = False;
   if (init_done) 
      return;
   init_done = True;
   for (i = 0; i < VG_N_THREADS; i++) {
      VG_(clear_syscallInfo)( i );
   }
}

/* --- This is the main function of this file. --- */

void VG_(client_syscall) ( ThreadId tid )
{
   UWord                    sysno;
   ThreadState*             tst;
   const SyscallTableEntry* ent;
   SyscallArgLayout         layout;
   SyscallInfo*             sci;

   ensure_initialised();

   vg_assert(VG_(is_valid_tid)(tid));
   vg_assert(tid >= 1 && tid < VG_N_THREADS);
   vg_assert(VG_(is_running_thread)(tid));

   tst = VG_(get_ThreadState)(tid);

   /* First off, get the syscall args and number.  This is a
      platform-dependent action. */

   sci = & syscallInfo[tid];
   vg_assert(sci->status.what == SsIdle);

   getSyscallArgsFromGuestState( &sci->orig_args, &tst->arch.vex );

   /* Copy .orig_args to .args.  The pre-handler may modify .args, but
      we want to keep the originals too, just in case. */
   sci->args = sci->orig_args;

   /* Save the syscall number in the thread state in case the syscall 
      is interrupted by a signal. */
   sysno = sci->orig_args.sysno;

   /* The default what-to-do-next thing is hand the syscall to the
      kernel, so we pre-set that here. */
   sci->status.what = SsHandToKernel;
   sci->status.val  = 0;
   sci->flags       = 0;

   /* Fetch the syscall's handlers.  If no handlers exist for this
      syscall, we are given dummy handlers which force an immediate
      return with ENOSYS. */
   ent = get_syscall_entry(sysno);

   /* Fetch the layout information, which tells us where in the guest
      state the syscall args reside.  This is a platform-dependent
      action.  This info is needed so that the scalar syscall argument
      checks (PRE_REG_READ calls) know which bits of the guest state
      they need to inspect. */
   getSyscallArgLayout( &layout );

   /* Make sure the tmp signal mask matches the real signal mask;
      sigsuspend may change this. */
   vg_assert(VG_(iseqsigset)(&tst->sig_mask, &tst->tmp_sig_mask));

   /* Right, we're finally ready to Party.  Call the pre-handler and
      see what we get back.  At this point: 

        sci->status.what  is Unset (we don't know yet).
        sci->orig_args    contains the original args.
        sci->args         is the same as sci->orig_args.
        sci->flags        is zero.
   */

   PRINT("SYSCALL[%d,%d](%3lld) ", VG_(getpid)(), tid, (ULong)sysno);

   /* Do any pre-syscall actions */
   if (VG_(needs).syscall_wrapper) {
      VG_TDICT_CALL(tool_pre_syscall, tid, sysno);
   }

   vg_assert(ent);
   vg_assert(ent->before);
   (ent->before)( tid,
                  &layout, 
                  &sci->args, &sci->status, &sci->flags );
   
   /* The pre-handler may have modified:
         sci->args
         sci->status
         sci->flags
      All else remains unchanged. 
      Although the args may be modified, pre handlers are not allowed
      to change the syscall number.
   */
   /* Now we proceed according to what the pre-handler decided. */
   vg_assert(sci->status.what == SsHandToKernel
             || sci->status.what == SsSuccess
             || sci->status.what == SsFailure);
   vg_assert(sci->args.sysno == sci->orig_args.sysno);

   if (sci->status.what == SsSuccess) {
      /* The pre-handler completed the syscall itself, declaring
         success. */
      PRINT(" --> [pre-success] Success(0x%llx)\n", (Long)sci->status.val );
                                       
      /* In this case the allowable flags are to ask for a signal-poll
         and/or a yield after the call.  Changing the args isn't
         allowed. */
      vg_assert(0 == (sci->flags & ~(SfPollAfter | SfYieldAfter)));
      vg_assert(eq_SyscallArgs(&sci->args, &sci->orig_args));
   }

   else
   if (sci->status.what == SsFailure) {
      /* The pre-handler decided to fail syscall itself. */
      PRINT(" --> [pre-fail] Failure(0x%llx)\n", (Long)sci->status.val );
      /* In this case, the pre-handler is also allowed to ask for the
         post-handler to be run anyway.  Changing the args is not
         allowed. */
      vg_assert(0 == (sci->flags & ~(SfMayBlock | SfPostOnFail | SfPollAfter)));
      vg_assert(eq_SyscallArgs(&sci->args, &sci->orig_args));
   }

   else
   if (sci->status.what != SsHandToKernel) {
      /* huh?! */
      vg_assert(0);
   }

   else /* (sci->status.what == HandToKernel) */ {
      /* Ok, this is the usual case -- and the complicated one.  There
         are two subcases: sync and async.  async is the general case
         and is to be used when there is any possibility that the
         syscall might block [a fact that the pre-handler must tell us
         via the sci->flags field.]  Because the tidying-away /
         context-switch overhead of the async case could be large, if
         we are sure that the syscall will not block, we fast-track it
         by doing it directly in this thread, which is a lot
         simpler. */

      /* Check that the given flags are allowable: MayBlock and
         PostOnFail are ok. */
      vg_assert(0 == (sci->flags & ~(SfMayBlock | SfPostOnFail)));

      if (sci->flags & SfMayBlock) {

         /* Syscall may block, so run it asynchronously */
         vki_sigset_t mask;

//         vg_assert(!(sci->flags & PadAddr));
         PRINT(" --> [async] ... \n");

         mask = tst->sig_mask;
         sanitize_client_sigmask(tid, &mask);

         /* Gack.  More impedance matching.  Copy the possibly
            modified syscall args back into the guest state. */
         vg_assert(eq_SyscallArgs(&sci->args, &sci->orig_args));
         putSyscallArgsIntoGuestState( &sci->args, &tst->arch.vex );

         /* Drop the lock */
         VG_(set_sleeping)(tid, VgTs_WaitSys);

         /* Do the call, which operates directly on the guest state,
            not on our abstracted copies of the args/result. */
         do_syscall_for_client(sysno, tst, &mask);

         /* do_syscall_for_client may not return if the syscall was
            interrupted by a signal.  In that case, flow of control is
            first to m_signals.async_sighandler, which calls
            VG_(fixup_guest_state_after_syscall_interrupted), which
            fixes up the guest state, and possibly calls
            VG_(post_syscall).  Once that's done, control drops back
            to the scheduler.  */

         /* Reacquire the lock */
         VG_(set_running)(tid);

         /* Even more impedance matching.  Extract the syscall status
            from the guest state. */
         getSyscallStatusFromGuestState( &sci->status, &tst->arch.vex );

         PRINT("SYSCALL[%d,%d](%3d) ... [async] --> %s(0x%llx)\n",
               VG_(getpid)(), tid, sysno, 
               sci->status.what==SsSuccess ? "Success" : "Failure",
               (Long)sci->status.val );

      } else {

         /* run the syscall directly */
         /* The pre-handler may have modified the syscall args, but
            since we're passing values in ->args directly to the
            kernel, there's no point in flushing them back to the
            guest state.  Indeed doing so could be construed as
            incorrect. */

//         if (sci->flags & PadAddr)
//            VG_(pad_address_space)(VG_(client_end));

         SysRes sres 
            = VG_(do_syscall6)(sysno, sci->args.arg1, sci->args.arg2, 
                                      sci->args.arg3, sci->args.arg4, 
                                      sci->args.arg5, sci->args.arg6 );
         sci->status = convert_SysRes_to_SyscallStatus(sres);

         PRINT("[sync] --> %s(0x%llx)\n",
               sci->status.what==SsSuccess ? "Success" : "Failure",
               (Long)sci->status.val );
      }
   }

   vg_assert(sci->status.what == SsFailure 
             || sci->status.what == SsSuccess);

   vg_assert(VG_(is_running_thread)(tid));

   /* Dump the syscall result back in the guest state.  This is
      a platform-specific action. */
   putSyscallStatusIntoGuestState( &sci->status, &tst->arch.vex );

   /* Situation now:
      - the guest state is now correctly modified following the syscall
      - modified args, original args and syscall status are still
        available in the syscallInfo[] entry for this syscall.

      Now go on to do the post-syscall actions (read on down ..)
   */
   VG_(post_syscall)(tid);
}


/* Perform post syscall actions.  The expected state on entry is
   precisely as at the end of VG_(client_syscall), that is:

   - guest state up to date following the syscall
   - modified args, original args and syscall status are still
     available in the syscallInfo[] entry for this syscall.
   - syscall status matches what's in the guest state.

   There are two ways to get here: the normal way -- being called by
   VG_(client_syscall), and the unusual way, from
   VG_(fixup_guest_state_after_syscall_interrupted).
*/
void VG_(post_syscall) (ThreadId tid)
{
   SyscallArgLayout         layout;
   SyscallInfo*             sci;
   const SyscallTableEntry* ent;
   SyscallStatus            test_status;
   ThreadState*             tst;
   UWord sysno;

   /* Preliminaries */
   vg_assert(VG_(is_valid_tid)(tid));
   vg_assert(tid >= 1 && tid < VG_N_THREADS);
   vg_assert(VG_(is_running_thread)(tid));

   tst = VG_(get_ThreadState)(tid);
   sci = & syscallInfo[tid];

   /* m_signals.sigvgkill_handler might call here even when not in
      a syscall. */
   if (sci->status.what == SsIdle || sci->status.what == SsHandToKernel) {
      sci->status.what = SsIdle;
      return;
   }

   /* Validate current syscallInfo entry.  In particular we require
      that the current .status matches what's actually in the guest
      state. */
   vg_assert(sci->status.what == SsFailure 
             || sci->status.what == SsSuccess);

   getSyscallStatusFromGuestState( &test_status, &tst->arch.vex );
   vg_assert(eq_SyscallStatus( &sci->status, &test_status ));
   /* Ok, looks sane */

   /* Get the system call number.  Because the pre-handler isn't
      allowed to mess with it, it should be the same for both the
      original and potentially-modified args. */
   vg_assert(sci->args.sysno == sci->orig_args.sysno);
   sysno = sci->args.sysno;
   ent = get_syscall_entry(sysno);

   /* We need the arg layout .. sigh */
   getSyscallArgLayout( &layout );

   /* Tell the tool that the assignment has occurred, so it can update
      shadow regs as necessary. */
   VG_TRACK( post_reg_write, Vg_CoreSysCall, tid, layout.o_retval, 
                                                  sizeof(UWord) );

   /* Consider, either success or failure.  Now run the post handler if:
      - it exists, and
      - status==Success or (status==Fail and PostOnFail is set)
   */
   if (ent->after
       && (sci->status.what == SsSuccess
           || (sci->status.what == SsFailure
               && (sci->flags & SfPostOnFail) ))) {

      (ent->after)( tid, &sci->args, &sci->status );
   }

   /* Because the post handler might have changed the status (eg, the
      post-handler for sys_open can change the result from success to
      failure if the kernel supplied a fd that it doesn't like), once
      again dump the syscall result back in the guest state.*/
   putSyscallStatusIntoGuestState( &sci->status, &tst->arch.vex );

   /* Do any post-syscall actions required by the tool. */
   if (VG_(needs).syscall_wrapper) {
      SysRes res;
      res.val     = sci->status.val;
      res.isError = sci->status.what == SsFailure;
      VG_TDICT_CALL(tool_post_syscall, tid, sysno, res);
   }

//zz    if (flags & PadAddr) {
//zz       vg_assert(!mayBlock);
//zz       VG_(unpad_address_space)(VG_(client_end));
//zz       //VG_(sanity_check_memory)();
//zz    }
//zz 

   /* The syscall is done. */
   sci->status.what = SsIdle;

   /* The pre/post wrappers may have concluded that pending signals
      might have been created, and will have set SfPollAfter to
      request a poll for them once the syscall is done. */
   if (sci->flags & SfPollAfter)
      VG_(poll_signals)(tid);

   /* Similarly, the wrappers might have asked for a yield
      afterwards. */
   if (sci->flags & SfYieldAfter)
      VG_(vg_yield)();
}


/* ---------------------------------------------------------------------
   Dealing with syscalls which get interrupted by a signal:
   VG_(fixup_guest_state_after_syscall_interrupted)
   ------------------------------------------------------------------ */

/* Syscalls done on behalf of the client are finally handed off to the
   kernel in VG_(client_syscall) above, either by calling
   do_syscall_for_client (the async case), or by calling
   VG_(do_syscall6) (the sync case).

   If the syscall is not interrupted by a signal (it may block and
   later unblock, but that's irrelevant here) then those functions
   eventually return and so control is passed to VG_(post_syscall).
   NB: not sure if the sync case can actually get interrupted, as it
   operates with all signals masked.

   However, the syscall may get interrupted by an async-signal.  In
   that case do_syscall_for_client/VG_(do_syscall6) do not
   return.  Instead we wind up in m_signals.async_sighandler.  We need
   to fix up the guest state to make it look like the syscall was
   interrupted for guest.  So async_sighandler calls here, and this
   does the fixup.  Note that from here we wind up calling
   VG_(post_syscall) too.
*/


/* These are addresses within ML_(do_syscall_for_client_WRK).  See
   syscall-$PLAT.S for details. 
*/
extern const Addr ML_(blksys_setup);
extern const Addr ML_(blksys_restart);
extern const Addr ML_(blksys_complete);
extern const Addr ML_(blksys_committed);
extern const Addr ML_(blksys_finished);


/* Back up guest state to restart a system call. */

void ML_(fixup_guest_state_to_restart_syscall) ( ThreadArchState* arch )
{
#if defined(VGP_x86_linux)
   arch->vex.guest_EIP -= 2;             // sizeof(int $0x80)

   /* Make sure our caller is actually sane, and we're really backing
      back over a syscall.

      int $0x80 == CD 80 
   */
   {
      UChar *p = (UChar *)arch->vex.guest_EIP;
      
      if (p[0] != 0xcd || p[1] != 0x80)
         VG_(message)(Vg_DebugMsg,
                      "?! restarting over syscall at %p %02x %02x\n",
                      arch->vex.guest_EIP, p[0], p[1]); 

      vg_assert(p[0] == 0xcd && p[1] == 0x80);
   }

#elif defined(VGP_amd64_linux)
   arch->vex.guest_RIP -= 2;             // sizeof(syscall)

   /* Make sure our caller is actually sane, and we're really backing
      back over a syscall.

      syscall == 0F 05 
   */
   {
      UChar *p = (UChar *)arch->vex.guest_RIP;
      
      if (p[0] != 0x0F || p[1] != 0x05)
         VG_(message)(Vg_DebugMsg,
                      "?! restarting over syscall at %p %02x %02x\n",
                      arch->vex.guest_RIP, p[0], p[1]); 

      vg_assert(p[0] == 0x0F && p[1] == 0x05);
   }

#elif defined(VGP_ppc32_linux)
   arch->vex.guest_CIA -= 4;             // sizeof(ppc32 instr)

   /* Make sure our caller is actually sane, and we're really backing
      back over a syscall.

      sc == 44 00 00 02
   */
   {
      UChar *p = (UChar *)arch->vex.guest_CIA;

      if (p[0] != 0x44 || p[1] != 0x0 || p[2] != 0x0 || p[3] != 0x02)
         VG_(message)(Vg_DebugMsg,
                      "?! restarting over syscall at %p %02x %02x %02x %02x\n",
                      arch->vex.guest_CIA, p[0], p[1], p[2], p[3]);

      vg_assert(p[0] == 0x44 && p[1] == 0x0 && p[2] == 0x0 && p[3] == 0x2);
   }

#else
#  error "ML_(fixup_guest_state_to_restart_syscall): unknown plat"
#endif
}

/* 
   Fix up the guest state when a syscall is interrupted by a signal
   and so has been forced to return 'sysret'.

   To do this, we determine the precise state of the syscall by
   looking at the (real) IP at the time the signal happened.  The
   syscall sequence looks like:

     1. unblock signals
     2. perform syscall
     3. save result to guest state (EAX, RAX, R3+CR0.SO)
     4. re-block signals

   If a signal
   happens at      Then     Why?
   [1-2)           restart  nothing has happened (restart syscall)
   [2]             restart  syscall hasn't started, or kernel wants to restart
   [2-3)           save     syscall complete, but results not saved
   [3-4)           syscall complete, results saved

   Sometimes we never want to restart an interrupted syscall (because
   sigaction says not to), so we only restart if "restart" is True.

   This will also call VG_(post_syscall) if the syscall has actually
   completed (either because it was interrupted, or because it
   actually finished).  It will not call VG_(post_syscall) if the
   syscall is set up for restart, which means that the pre-wrapper may
   get called multiple times.
*/

void 
VG_(fixup_guest_state_after_syscall_interrupted)( ThreadId tid, 
                                                  Addr     ip, 
                                                  UWord    sysnum, 
                                                  SysRes   sysret,
                                                  Bool     restart)
{
   /* Note that the sysnum arg seems to contain not-dependable-on info
      (I think it depends on the state the real syscall was in at
      interrupt) and so is ignored, apart from in the following
      printf. */

   static const Bool debug = False;

   ThreadState*     tst;
   SyscallStatus    canonical;
   ThreadArchState* th_regs;
   SyscallInfo*     sci;

   if (debug)
      VG_(printf)( "interrupted_syscall %d: tid=%d, IP=0x%llx, "
                   "restart=%s, sysret.isError=%s, sysret.val=%lld\n", 
                   (Int)sysnum,
                   (Int)tid,
                   (ULong)ip, 
                   restart ? "True" : "False", 
                   sysret.isError ? "True" : "False",
                   (Long)(Word)sysret.val );

   vg_assert(VG_(is_valid_tid)(tid));
   vg_assert(tid >= 1 && tid < VG_N_THREADS);
   vg_assert(VG_(is_running_thread)(tid));

   tst     = VG_(get_ThreadState)(tid);
   th_regs = &tst->arch;
   sci     = & syscallInfo[tid];

   /* Figure out what the state of the syscall was by examining the
      (real) IP at the time of the signal, and act accordingly. */

   if (ip < ML_(blksys_setup) || ip >= ML_(blksys_finished)) {
      VG_(printf)("  not in syscall (%p - %p)\n", 
                  ML_(blksys_setup), ML_(blksys_finished));
      /* Looks like we weren't in a syscall at all.  Hmm. */
      vg_assert(sci->status.what != SsIdle);
      return;
   }

   /* We should not be here unless this thread had first started up
      the machinery for a syscall by calling VG_(client_syscall).
      Hence: */
   vg_assert(sci->status.what != SsIdle);

   if (ip >= ML_(blksys_setup) && ip < ML_(blksys_restart)) {
      /* syscall hasn't even started; go around again */
      if (debug)
         VG_(printf)("  not started: restart\n");
      vg_assert(sci->status.what == SsHandToKernel);
      ML_(fixup_guest_state_to_restart_syscall)(th_regs);
   } 

   else 
   if (ip == ML_(blksys_restart)) {
      /* We're either about to run the syscall, or it was interrupted
         and the kernel restarted it.  Restart if asked, otherwise
         EINTR it. */
      if (restart)
         ML_(fixup_guest_state_to_restart_syscall)(th_regs);
      else {
         canonical = convert_SysRes_to_SyscallStatus( 
                        VG_(mk_SysRes_Error)( VKI_EINTR ) 
                     );
         putSyscallStatusIntoGuestState( &canonical, &th_regs->vex );
         sci->status = canonical;
         VG_(post_syscall)(tid);
      }
   }

   else 
   if (ip >= ML_(blksys_complete) && ip < ML_(blksys_committed)) {
      /* Syscall complete, but result hasn't been written back yet.
         Write the SysRes we were supplied with back to the guest
         state. */
      if (debug)
         VG_(printf)("  completed\n", sysret);
      canonical = convert_SysRes_to_SyscallStatus( sysret );
      putSyscallStatusIntoGuestState( &canonical, &th_regs->vex );
      sci->status = canonical;
      VG_(post_syscall)(tid);
   } 

   else 
   if (ip >= ML_(blksys_committed) && ip < ML_(blksys_finished)) {
      /* Result committed, but the signal mask has not been restored;
         we expect our caller (the signal handler) will have fixed
         this up. */
      if (debug)
         VG_(printf)("  all done\n");
      VG_(post_syscall)(tid);
   } 

   else
      VG_(core_panic)("?? strange syscall interrupt state?");

   /* In all cases, the syscall is now finished (even if we called
      ML_(fixup_guest_state_to_restart_syscall), since that just
      re-positions the guest's IP for another go at it).  So we need
      to record that fact. */
   sci->status.what = SsIdle;
}


/* ---------------------------------------------------------------------
   A place to store the where-to-call-when-really-done pointer
   ------------------------------------------------------------------ */

// When the final thread is done, where shall I call to shutdown the
// system cleanly?  Is set once at startup (in m_main) and never
// changes after that.  Is basically a pointer to the exit
// continuation.  This is all just a nasty hack to avoid calling
// directly from m_syswrap to m_main at exit, since that would cause
// m_main to become part of a module cycle, which is silly.
void (* VG_(address_of_m_main_shutdown_actions_NORETURN) )
       (ThreadId,VgSchedReturnCode)
   = NULL;

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
