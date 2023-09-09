
/*--------------------------------------------------------------------*/
/*--- Handle system calls.                          syswrap-main.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2017 Julian Seward 
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
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.
*/

#include "libvex_guest_offsets.h"
#include "libvex_trc_values.h"
#include "pub_core_basics.h"
#include "pub_core_aspacemgr.h"
#include "pub_core_vki.h"
#include "pub_core_vkiscnums.h"
#include "pub_core_threadstate.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcprint.h"
#include "pub_core_libcproc.h"      // For VG_(getpid)()
#include "pub_core_libcsignal.h"
#include "pub_core_scheduler.h"     // For VG_({acquire,release}_BigLock),
                                    //   and VG_(vg_yield)
#include "pub_core_stacktrace.h"    // For VG_(get_and_pp_StackTrace)()
#include "pub_core_tooliface.h"
#include "pub_core_options.h"
#include "pub_core_signals.h"       // For VG_SIGVGKILL, VG_(poll_signals)
#include "pub_core_syscall.h"
#include "pub_core_machine.h"
#include "pub_core_mallocfree.h"
#include "pub_core_syswrap.h"
#include "pub_core_gdbserver.h"     // VG_(gdbserver_report_syscall)

#include "priv_types_n_macros.h"
#include "priv_syswrap-main.h"

#if defined(VGO_darwin)
#include "priv_syswrap-darwin.h"
#endif

/* Useful info which needs to be recorded somewhere:
   Use of registers in syscalls is:

          NUM   ARG1 ARG2 ARG3 ARG4 ARG5 ARG6 ARG7 ARG8 RESULT
   LINUX:
   x86    eax   ebx  ecx  edx  esi  edi  ebp  n/a  n/a  eax       (== NUM)
   amd64  rax   rdi  rsi  rdx  r10  r8   r9   n/a  n/a  rax       (== NUM)
   ppc32  r0    r3   r4   r5   r6   r7   r8   n/a  n/a  r3+CR0.SO (== ARG1)
   ppc64  r0    r3   r4   r5   r6   r7   r8   n/a  n/a  r3+CR0.SO (== ARG1)
   arm    r7    r0   r1   r2   r3   r4   r5   n/a  n/a  r0        (== ARG1)
   mips32 v0    a0   a1   a2   a3 stack stack n/a  n/a  v0        (== NUM)
   mips64 v0    a0   a1   a2   a3   a4   a5   a6   a7   v0        (== NUM)
   arm64  x8    x0   x1   x2   x3   x4   x5   n/a  n/a  x0 ??     (== ARG1??)

   FreeBSD:
   x86    eax +4   +8   +12  +16  +20  +24  +28  +32  edx:eax, eflags.c
   amd64  rax rdi  rsi  rdx  rcx  r8   r9   +8   +16  rdx:rax, rflags.c

   On s390x the svc instruction is used for system calls. The system call
   number is encoded in the instruction (8 bit immediate field). Since Linux
   2.6 it is also allowed to use svc 0 with the system call number in r1.
   This was introduced for system calls >255, but works for all. It is
   also possible to see the svc 0 together with an EXecute instruction, that
   fills in the immediate field.
   s390x r1/SVC r2   r3   r4   r5   r6   r7   n/a  n/a  r2        (== ARG1)

          NUM   ARG1 ARG2 ARG3 ARG4 ARG5 ARG6 ARG7 ARG8 RESULT
   DARWIN:
   x86    eax   +4   +8   +12  +16  +20  +24  +28  +32  edx:eax, eflags.c
   amd64  rax   rdi  rsi  rdx  rcx  r8   r9   +8   +16  rdx:rax, rflags.c

   For x86-darwin and x86-freebsd, "+N" denotes "in memory at N(%esp)";
   ditto amd64-darwin/amd64-freebsd.  Apparently 0(%esp) is some kind of return address
   (perhaps for syscalls done with "sysenter"?)  I don't think it is
   relevant for syscalls done with "int $0x80/1/2".

   SOLARIS:
   x86    eax +4   +8   +12  +16  +20  +24  +28  +32  edx:eax, eflags.c
   amd64  rax rdi  rsi  rdx  r10  r8   r9   +8   +16  rdx:rax, rflags.c

   "+N" denotes "in memory at N(%esp)". Solaris also supports fasttrap
   syscalls. Fasttraps do not take any parameters (except of the sysno in eax)
   and never fail (if the sysno is valid).
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

   * Ensures the root thread's stack is suitably mapped.  Tedious and
     arcane.  See big big comment in VG_(client_syscall).

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

     FreeBSD:
     x86:    Success(N) ==>  edx:eax = N, cc = 0
             Fail(N)    ==>  edx:eax = N, cc = 1

     ditto amd64

     Darwin:
     x86:    Success(N) ==>  edx:eax = N, cc = 0
             Fail(N)    ==>  edx:eax = N, cc = 1

     s390x:  Success(N) ==>  r2 = N
             Fail(N)    ==>  r2 = -N

     Solaris:
     x86:    Success(N) ==>  edx:eax = N, cc = 0
             Fail(N)    ==>      eax = N, cc = 1
     Same applies for fasttraps except they never fail.

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

   This function (these functions) are required to return zero in case
   of success (even if the syscall itself failed), and nonzero if the
   sigprocmask-swizzling calls failed.  We don't actually care about
   the failure values from sigprocmask, although most of the assembly
   implementations do attempt to return that, using the convention
   0 for success, or 0x8000 | error-code for failure.
*/
#if defined(VGO_linux)
extern
UWord ML_(do_syscall_for_client_WRK)( Word syscallno, 
                                      void* guest_state,
                                      const vki_sigset_t *syscall_mask,
                                      const vki_sigset_t *restore_mask,
                                      Word sigsetSzB );
#elif defined(VGO_freebsd)
extern
UWord ML_(do_syscall_for_client_WRK)( Word syscallno, 
                                      void* guest_state,
                                      const vki_sigset_t *syscall_mask,
                                      const vki_sigset_t *restore_mask,
                                      Word sigsetSzB );
#elif defined(VGO_darwin)
extern
UWord ML_(do_syscall_for_client_unix_WRK)( Word syscallno, 
                                           void* guest_state,
                                           const vki_sigset_t *syscall_mask,
                                           const vki_sigset_t *restore_mask,
                                           Word sigsetSzB ); /* unused */
extern
UWord ML_(do_syscall_for_client_mach_WRK)( Word syscallno, 
                                           void* guest_state,
                                           const vki_sigset_t *syscall_mask,
                                           const vki_sigset_t *restore_mask,
                                           Word sigsetSzB ); /* unused */
extern
UWord ML_(do_syscall_for_client_mdep_WRK)( Word syscallno, 
                                           void* guest_state,
                                           const vki_sigset_t *syscall_mask,
                                           const vki_sigset_t *restore_mask,
                                           Word sigsetSzB ); /* unused */
#elif defined(VGO_solaris)
extern
UWord ML_(do_syscall_for_client_WRK)( Word syscallno,
                                      void* guest_state,
                                      const vki_sigset_t *syscall_mask,
                                      const vki_sigset_t *restore_mask,
                                      UChar *cflag);
UWord ML_(do_syscall_for_client_dret_WRK)( Word syscallno,
                                           void* guest_state,
                                           const vki_sigset_t *syscall_mask,
                                           const vki_sigset_t *restore_mask,
                                           UChar *cflag);
#else
#  error "Unknown OS"
#endif


static
void do_syscall_for_client ( Int syscallno,
                             ThreadState* tst,
                             const vki_sigset_t* syscall_mask )
{
   vki_sigset_t saved;
   UWord err;
#  if defined(VGO_freebsd)
   Int real_syscallno;
#  endif
#  if defined(VGO_linux)
   err = ML_(do_syscall_for_client_WRK)(
            syscallno, &tst->arch.vex, 
            syscall_mask, &saved, sizeof(vki_sigset_t)
         );
#  elif defined(VGO_freebsd)
   if (tst->arch.vex.guest_SC_CLASS == VG_FREEBSD_SYSCALL0)
      real_syscallno = __NR_syscall;
   else if (tst->arch.vex.guest_SC_CLASS == VG_FREEBSD_SYSCALL198)
      real_syscallno = __NR___syscall;
   else
      real_syscallno = syscallno;
   err = ML_(do_syscall_for_client_WRK)(
            real_syscallno, &tst->arch.vex,
            syscall_mask, &saved, sizeof(vki_sigset_t)
         );
#  elif defined(VGO_darwin)
   switch (VG_DARWIN_SYSNO_CLASS(syscallno)) {
      case VG_DARWIN_SYSCALL_CLASS_UNIX:
         err = ML_(do_syscall_for_client_unix_WRK)(
                  VG_DARWIN_SYSNO_FOR_KERNEL(syscallno), &tst->arch.vex, 
                  syscall_mask, &saved, 0/*unused:sigsetSzB*/
               );
         break;
      case VG_DARWIN_SYSCALL_CLASS_MACH:
         err = ML_(do_syscall_for_client_mach_WRK)(
                  VG_DARWIN_SYSNO_FOR_KERNEL(syscallno), &tst->arch.vex, 
                  syscall_mask, &saved, 0/*unused:sigsetSzB*/
               );
         break;
      case VG_DARWIN_SYSCALL_CLASS_MDEP:
         err = ML_(do_syscall_for_client_mdep_WRK)(
                  VG_DARWIN_SYSNO_FOR_KERNEL(syscallno), &tst->arch.vex, 
                  syscall_mask, &saved, 0/*unused:sigsetSzB*/
               );
         break;
      default:
         vg_assert(0);
         /*NOTREACHED*/
         break;
   }
#  elif defined(VGO_solaris)
   UChar cflag;

   /* Fasttraps or anything else cannot go through this path. */
   vg_assert(VG_SOLARIS_SYSNO_CLASS(syscallno)
             == VG_SOLARIS_SYSCALL_CLASS_CLASSIC);

   /* If the syscall is a door_return call then it has to be handled very
      differently. */
   if (tst->os_state.in_door_return)
      err = ML_(do_syscall_for_client_dret_WRK)(
                syscallno, &tst->arch.vex,
                syscall_mask, &saved, &cflag
            );
   else
      err = ML_(do_syscall_for_client_WRK)(
                syscallno, &tst->arch.vex,
                syscall_mask, &saved, &cflag
            );

   /* Save the carry flag. */
#  if defined(VGP_x86_solaris)
   LibVEX_GuestX86_put_eflag_c(cflag, &tst->arch.vex);
#  elif defined(VGP_amd64_solaris)
   LibVEX_GuestAMD64_put_rflag_c(cflag, &tst->arch.vex);
#  else
#    error "Unknown platform"
#  endif

#  else
#    error "Unknown OS"
#  endif
   vg_assert2(
      err == 0,
      "ML_(do_syscall_for_client_WRK): sigprocmask error %lu",
      err & 0xFFF
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
          && a1->arg6 == a2->arg6
          && a1->arg7 == a2->arg7
          && a1->arg8 == a2->arg8;
}

static
Bool eq_SyscallStatus ( UInt sysno, SyscallStatus* s1, SyscallStatus* s2 )
{
   /* was: return s1->what == s2->what && sr_EQ( s1->sres, s2->sres ); */
   if (s1->what == s2->what && sr_EQ( sysno, s1->sres, s2->sres ))
      return True;
#  if defined(VGO_darwin)
   /* Darwin-specific debugging guff */
   vg_assert(s1->what == s2->what);
   VG_(printf)("eq_SyscallStatus:\n");
   VG_(printf)("  {%lu %lu %u}\n", s1->sres._wLO, s1->sres._wHI, s1->sres._mode);
   VG_(printf)("  {%lu %lu %u}\n", s2->sres._wLO, s2->sres._wHI, s2->sres._mode);
   vg_assert(0);
#  endif
   return False;
}

/* Convert between SysRes and SyscallStatus, to the extent possible. */

static
SyscallStatus convert_SysRes_to_SyscallStatus ( SysRes res )
{
   SyscallStatus status;
   status.what = SsComplete;
   status.sres = res;
   return status;
}


/* Impedance matchers.  These convert syscall arg or result data from
   the platform-specific in-guest-state format to the canonical
   formats, and back. */

static 
void getSyscallArgsFromGuestState ( /*OUT*/SyscallArgs*       canonical,
                                    /*IN*/ VexGuestArchState* gst_vanilla, 
                                    /*IN*/ UInt trc )
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
   canonical->arg7  = 0;
   canonical->arg8  = 0;

#elif defined(VGP_amd64_linux)
   VexGuestAMD64State* gst = (VexGuestAMD64State*)gst_vanilla;
   canonical->sysno = gst->guest_RAX;
   canonical->arg1  = gst->guest_RDI;
   canonical->arg2  = gst->guest_RSI;
   canonical->arg3  = gst->guest_RDX;
   canonical->arg4  = gst->guest_R10;
   canonical->arg5  = gst->guest_R8;
   canonical->arg6  = gst->guest_R9;
   canonical->arg7  = 0;
   canonical->arg8  = 0;

#elif defined(VGP_ppc32_linux)
   VexGuestPPC32State* gst = (VexGuestPPC32State*)gst_vanilla;
   canonical->sysno = gst->guest_GPR0;
   canonical->arg1  = gst->guest_GPR3;
   canonical->arg2  = gst->guest_GPR4;
   canonical->arg3  = gst->guest_GPR5;
   canonical->arg4  = gst->guest_GPR6;
   canonical->arg5  = gst->guest_GPR7;
   canonical->arg6  = gst->guest_GPR8;
   canonical->arg7  = 0;
   canonical->arg8  = 0;

#elif defined(VGP_ppc64be_linux) || defined(VGP_ppc64le_linux)
   VexGuestPPC64State* gst = (VexGuestPPC64State*)gst_vanilla;
   canonical->sysno = gst->guest_GPR0;
   canonical->arg1  = gst->guest_GPR3;
   canonical->arg2  = gst->guest_GPR4;
   canonical->arg3  = gst->guest_GPR5;
   canonical->arg4  = gst->guest_GPR6;
   canonical->arg5  = gst->guest_GPR7;
   canonical->arg6  = gst->guest_GPR8;
   /* ISA 3.0 adds the scv system call instruction.
      The PPC syscalls have at most 6 args.  Arg 7 is being used to pass a
      flag to indicate which system call instruction is to be used.
      Arg7 = SC_FLAG for the sc instruction; Arg7 = SCV_FLAG for the scv
      instruction.  The guest register guest_syscall_flag was created to pass
      the flag so the actual guest state would not be changed. */
   canonical->arg7  = gst->guest_syscall_flag;
   canonical->arg8  = 0;

#elif defined(VGP_x86_freebsd)
   VexGuestX86State* gst = (VexGuestX86State*)gst_vanilla;
   UWord *stack = (UWord *)gst->guest_ESP;

   // FreeBSD supports different calling conventions
   switch (gst->guest_EAX) {
   case __NR_syscall:
      canonical->klass = VG_FREEBSD_SYSCALL0;
      canonical->sysno = stack[1];
      stack += 1;
      break;
   case __NR___syscall:
      canonical->klass = VG_FREEBSD_SYSCALL198;
      canonical->sysno = stack[1];
      stack += 2;
      break;
   default:
      canonical->klass = 0;
      canonical->sysno = gst->guest_EAX;
      break;
   }
   // stack[0] is a (fake) return address
   canonical->arg1  = stack[1];
   canonical->arg2  = stack[2];
   canonical->arg3  = stack[3];
   canonical->arg4  = stack[4];
   canonical->arg5  = stack[5];
   canonical->arg6  = stack[6];
   canonical->arg7  = stack[7];
   canonical->arg8  = stack[8];

#elif defined(VGP_amd64_freebsd)
   VexGuestAMD64State* gst = (VexGuestAMD64State*)gst_vanilla;
   UWord *stack = (UWord *)gst->guest_RSP;

   // FreeBSD supports different calling conventions
   // @todo PJF this all seems over complicated to me
   // SYSCALL_STD is OK but for the other
   // two here we overwrite canonical->sysno with
   // the final syscall number but then in do_syscall_for_client
   // we switch real_syscallno back to __NR_syscall or __NR___syscall
   switch (gst->guest_RAX) {
   case __NR_syscall:
      canonical->klass = VG_FREEBSD_SYSCALL0;
      canonical->sysno = gst->guest_RDI;
      break;
   case __NR___syscall:
      canonical->klass = VG_FREEBSD_SYSCALL198;
      canonical->sysno = gst->guest_RDI;
      break;
   default:
      canonical->klass = VG_FREEBSD_SYSCALL_STD;
      canonical->sysno = gst->guest_RAX;
      break;
   }

   // stack[0] is a (fake) return address
   if (canonical->klass == VG_FREEBSD_SYSCALL0 || canonical->klass == VG_FREEBSD_SYSCALL198) {
      // stack[0] is return address
      canonical->arg1  = gst->guest_RSI;
      canonical->arg2  = gst->guest_RDX;
      canonical->arg3  = gst->guest_R10;
      canonical->arg4  = gst->guest_R8;
      canonical->arg5  = gst->guest_R9;
      canonical->arg6  = stack[1];
      canonical->arg7  = stack[2];
      canonical->arg8  = stack[3];
   } else {
      // stack[0] is return address
      canonical->arg1  = gst->guest_RDI;
      canonical->arg2  = gst->guest_RSI;
      canonical->arg3  = gst->guest_RDX;
      canonical->arg4  = gst->guest_R10;
      canonical->arg5  = gst->guest_R8;
      canonical->arg6  = gst->guest_R9;
      canonical->arg7  = stack[1];
      canonical->arg8  = stack[2];
   }

#elif defined(VGP_arm_linux)
   VexGuestARMState* gst = (VexGuestARMState*)gst_vanilla;
   canonical->sysno = gst->guest_R7;
   canonical->arg1  = gst->guest_R0;
   canonical->arg2  = gst->guest_R1;
   canonical->arg3  = gst->guest_R2;
   canonical->arg4  = gst->guest_R3;
   canonical->arg5  = gst->guest_R4;
   canonical->arg6  = gst->guest_R5;
   canonical->arg7  = 0;
   canonical->arg8  = 0;

#elif defined(VGP_arm64_linux)
   VexGuestARM64State* gst = (VexGuestARM64State*)gst_vanilla;
   canonical->sysno = gst->guest_X8;
   canonical->arg1  = gst->guest_X0;
   canonical->arg2  = gst->guest_X1;
   canonical->arg3  = gst->guest_X2;
   canonical->arg4  = gst->guest_X3;
   canonical->arg5  = gst->guest_X4;
   canonical->arg6  = gst->guest_X5;
   canonical->arg7  = 0;
   canonical->arg8  = 0;

#elif defined(VGP_mips32_linux)
   VexGuestMIPS32State* gst = (VexGuestMIPS32State*)gst_vanilla;
   canonical->sysno = gst->guest_r2;    // v0
   if (canonical->sysno == __NR_exit) {
      canonical->arg1 = gst->guest_r4;    // a0
      canonical->arg2 = 0;
      canonical->arg3 = 0;
      canonical->arg4 = 0;
      canonical->arg5 = 0;
      canonical->arg6 = 0;
      canonical->arg8 = 0;
   } else if (canonical->sysno != __NR_syscall) {
      canonical->arg1  = gst->guest_r4;    // a0
      canonical->arg2  = gst->guest_r5;    // a1
      canonical->arg3  = gst->guest_r6;    // a2
      canonical->arg4  = gst->guest_r7;    // a3
      canonical->arg5  = *((UInt*) (gst->guest_r29 + 16));    // 16(guest_SP)
      canonical->arg6  = *((UInt*) (gst->guest_r29 + 20));    // 20(guest_SP)
      canonical->arg7  = *((UInt*) (gst->guest_r29 + 24));    // 24(guest_SP)
      canonical->arg8 = 0;
   } else {
      // Fixme hack handle syscall()
      canonical->sysno = gst->guest_r4;    // a0
      canonical->arg1  = gst->guest_r5;    // a1
      canonical->arg2  = gst->guest_r6;    // a2
      canonical->arg3  = gst->guest_r7;    // a3
      canonical->arg4  = *((UInt*) (gst->guest_r29 + 16));    // 16(guest_SP/sp)
      canonical->arg5  = *((UInt*) (gst->guest_r29 + 20));    // 20(guest_SP/sp)
      canonical->arg6  = *((UInt*) (gst->guest_r29 + 24));    // 24(guest_SP/sp)
      canonical->arg7  = *((UInt*) (gst->guest_r29 + 28));    // 28(guest_SP/sp)
      canonical->arg8 = __NR_syscall;
   }

#elif defined(VGP_mips64_linux)
   VexGuestMIPS64State* gst = (VexGuestMIPS64State*)gst_vanilla;
   canonical->sysno = gst->guest_r2;    // v0
   canonical->arg1  = gst->guest_r4;    // a0
   canonical->arg2  = gst->guest_r5;    // a1
   canonical->arg3  = gst->guest_r6;    // a2
   canonical->arg4  = gst->guest_r7;    // a3
   canonical->arg5  = gst->guest_r8;    // a4
   canonical->arg6  = gst->guest_r9;    // a5
   canonical->arg7  = gst->guest_r10;   // a6
   canonical->arg8  = gst->guest_r11;   // a7

#elif defined(VGP_nanomips_linux)
  VexGuestMIPS32State* gst = (VexGuestMIPS32State*)gst_vanilla;
   canonical->sysno = gst->guest_r2;    // t4
   canonical->arg1  = gst->guest_r4;    // a0
   canonical->arg2  = gst->guest_r5;    // a1
   canonical->arg3  = gst->guest_r6;    // a2
   canonical->arg4  = gst->guest_r7;    // a3
   canonical->arg5  = gst->guest_r8;    // a4
   canonical->arg6  = gst->guest_r9;    // a5
   canonical->arg7  = gst->guest_r10;   // a6
   canonical->arg8  = gst->guest_r11;   // a7
#elif defined(VGP_x86_darwin)
   VexGuestX86State* gst = (VexGuestX86State*)gst_vanilla;
   UWord *stack = (UWord *)gst->guest_ESP;
   // GrP fixme hope syscalls aren't called with really shallow stacks...
   canonical->sysno = gst->guest_EAX;
   if (canonical->sysno != 0) {
      // stack[0] is return address
      canonical->arg1  = stack[1];
      canonical->arg2  = stack[2];
      canonical->arg3  = stack[3];
      canonical->arg4  = stack[4];
      canonical->arg5  = stack[5];
      canonical->arg6  = stack[6];
      canonical->arg7  = stack[7];
      canonical->arg8  = stack[8];
   } else {
      // GrP fixme hack handle syscall()
      // GrP fixme what about __syscall() ?
      // stack[0] is return address
      // DDD: the tool can't see that the params have been shifted!  Can
      //      lead to incorrect checking, I think, because the PRRAn/PSARn
      //      macros will mention the pre-shifted args.
      canonical->sysno = stack[1];
      vg_assert(canonical->sysno != 0);
      canonical->arg1  = stack[2];
      canonical->arg2  = stack[3];
      canonical->arg3  = stack[4];
      canonical->arg4  = stack[5];
      canonical->arg5  = stack[6];
      canonical->arg6  = stack[7];
      canonical->arg7  = stack[8];
      canonical->arg8  = stack[9];
      
      PRINT("SYSCALL[%d,?](0) syscall(%s, ...); please stand by...\n",
            VG_(getpid)(), /*tid,*/
            VG_SYSNUM_STRING(canonical->sysno));
   }

   // Here we determine what kind of syscall it was by looking at the
   // interrupt kind, and then encode the syscall number using the 64-bit
   // encoding for Valgrind's internal use.
   //
   // DDD: Would it be better to stash the JMP kind into the Darwin
   // thread state rather than passing in the trc?
   switch (trc) {
   case VEX_TRC_JMP_SYS_INT128:
      // int $0x80 = Unix, 64-bit result
      vg_assert(canonical->sysno >= 0);
      canonical->sysno = VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(canonical->sysno);
      break;
   case VEX_TRC_JMP_SYS_SYSENTER:
      // syscall = Unix, 32-bit result
      // OR        Mach, 32-bit result
      if (canonical->sysno >= 0) {
         // GrP fixme hack:  0xffff == I386_SYSCALL_NUMBER_MASK
         canonical->sysno = VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(canonical->sysno
                                                             & 0xffff);
      } else {
         canonical->sysno = VG_DARWIN_SYSCALL_CONSTRUCT_MACH(-canonical->sysno);
      }
      break;
   case VEX_TRC_JMP_SYS_INT129:
      // int $0x81 = Mach, 32-bit result
      vg_assert(canonical->sysno < 0);
      canonical->sysno = VG_DARWIN_SYSCALL_CONSTRUCT_MACH(-canonical->sysno);
      break;
   case VEX_TRC_JMP_SYS_INT130:
      // int $0x82 = mdep, 32-bit result
      vg_assert(canonical->sysno >= 0);
      canonical->sysno = VG_DARWIN_SYSCALL_CONSTRUCT_MDEP(canonical->sysno);
      break;
   default: 
      vg_assert(0);
      break;
   }
   
#elif defined(VGP_amd64_darwin)
   VexGuestAMD64State* gst = (VexGuestAMD64State*)gst_vanilla;
   UWord *stack = (UWord *)gst->guest_RSP;

   vg_assert(trc == VEX_TRC_JMP_SYS_SYSCALL);

   // GrP fixme hope syscalls aren't called with really shallow stacks...
   canonical->sysno = gst->guest_RAX;
   if (canonical->sysno != __NR_syscall) {
      // stack[0] is return address
      canonical->arg1  = gst->guest_RDI;
      canonical->arg2  = gst->guest_RSI;
      canonical->arg3  = gst->guest_RDX;
      canonical->arg4  = gst->guest_R10;  // not rcx with syscall insn
      canonical->arg5  = gst->guest_R8;
      canonical->arg6  = gst->guest_R9;
      canonical->arg7  = stack[1];
      canonical->arg8  = stack[2];
   } else {
      // GrP fixme hack handle syscall()
      // GrP fixme what about __syscall() ?
      // stack[0] is return address
      // DDD: the tool can't see that the params have been shifted!  Can
      //      lead to incorrect checking, I think, because the PRRAn/PSARn
      //      macros will mention the pre-shifted args.
      canonical->sysno = VG_DARWIN_SYSCALL_CONSTRUCT_UNIX(gst->guest_RDI);
      vg_assert(canonical->sysno != __NR_syscall);
      canonical->arg1  = gst->guest_RSI;
      canonical->arg2  = gst->guest_RDX;
      canonical->arg3  = gst->guest_R10;  // not rcx with syscall insn
      canonical->arg4  = gst->guest_R8;
      canonical->arg5  = gst->guest_R9;
      canonical->arg6  = stack[1];
      canonical->arg7  = stack[2];
      canonical->arg8  = stack[3];
 
      PRINT("SYSCALL[%d,?](0) syscall(%s, ...); please stand by...\n",
            VG_(getpid)(), /*tid,*/
            VG_SYSNUM_STRING(canonical->sysno));
   }

   // no canonical->sysno adjustment needed

#elif defined(VGP_s390x_linux)
   VexGuestS390XState* gst = (VexGuestS390XState*)gst_vanilla;
   canonical->sysno = gst->guest_SYSNO;
   canonical->arg1  = gst->guest_r2;
   canonical->arg2  = gst->guest_r3;
   canonical->arg3  = gst->guest_r4;
   canonical->arg4  = gst->guest_r5;
   canonical->arg5  = gst->guest_r6;
   canonical->arg6  = gst->guest_r7;
   canonical->arg7  = 0;
   canonical->arg8  = 0;

#elif defined(VGP_x86_solaris)
   VexGuestX86State* gst = (VexGuestX86State*)gst_vanilla;
   UWord *stack = (UWord *)gst->guest_ESP;
   canonical->sysno = gst->guest_EAX;
   /* stack[0] is a return address. */
   canonical->arg1  = stack[1];
   canonical->arg2  = stack[2];
   canonical->arg3  = stack[3];
   canonical->arg4  = stack[4];
   canonical->arg5  = stack[5];
   canonical->arg6  = stack[6];
   canonical->arg7  = stack[7];
   canonical->arg8  = stack[8];

   switch (trc) {
   case VEX_TRC_JMP_SYS_INT145:
   case VEX_TRC_JMP_SYS_SYSENTER:
   case VEX_TRC_JMP_SYS_SYSCALL:
   /* These three are not actually valid syscall instructions on Solaris.
      Pretend for now that we handle them as normal syscalls. */
   case VEX_TRC_JMP_SYS_INT128:
   case VEX_TRC_JMP_SYS_INT129:
   case VEX_TRC_JMP_SYS_INT130:
      /* int $0x91, sysenter, syscall = normal syscall */
      break;
   case VEX_TRC_JMP_SYS_INT210:
      /* int $0xD2 = fasttrap */
      canonical->sysno
         = VG_SOLARIS_SYSCALL_CONSTRUCT_FASTTRAP(canonical->sysno);
      break;
   default:
      vg_assert(0);
      break;
   }

#elif defined(VGP_amd64_solaris)
   VexGuestAMD64State* gst = (VexGuestAMD64State*)gst_vanilla;
   UWord *stack = (UWord *)gst->guest_RSP;
   canonical->sysno = gst->guest_RAX;
   /* stack[0] is a return address. */
   canonical->arg1 = gst->guest_RDI;
   canonical->arg2 = gst->guest_RSI;
   canonical->arg3 = gst->guest_RDX;
   canonical->arg4 = gst->guest_R10;  /* Not RCX with syscall. */
   canonical->arg5 = gst->guest_R8;
   canonical->arg6 = gst->guest_R9;
   canonical->arg7 = stack[1];
   canonical->arg8 = stack[2];

   switch (trc) {
   case VEX_TRC_JMP_SYS_SYSCALL:
      /* syscall = normal syscall */
      break;
   case VEX_TRC_JMP_SYS_INT210:
      /* int $0xD2 = fasttrap */
      canonical->sysno
         = VG_SOLARIS_SYSCALL_CONSTRUCT_FASTTRAP(canonical->sysno);
      break;
   default:
      vg_assert(0);
      break;
   }

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

#elif defined(VGP_ppc64be_linux) || defined(VGP_ppc64le_linux)
   VexGuestPPC64State* gst = (VexGuestPPC64State*)gst_vanilla;
   gst->guest_GPR0 = canonical->sysno;
   gst->guest_GPR3 = canonical->arg1;
   gst->guest_GPR4 = canonical->arg2;
   gst->guest_GPR5 = canonical->arg3;
   gst->guest_GPR6 = canonical->arg4;
   gst->guest_GPR7 = canonical->arg5;
   gst->guest_GPR8 = canonical->arg6;
   gst->guest_GPR9 = canonical->arg7;

#elif defined(VGP_x86_freebsd)
   VexGuestX86State* gst = (VexGuestX86State*)gst_vanilla;
   UWord *stack = (UWord *)gst->guest_ESP;

   // stack[0] is a (fake) return address
   switch (canonical->klass) {
   case VG_FREEBSD_SYSCALL0:
      gst->guest_EAX = __NR_syscall;
      stack[1] = canonical->sysno;
      stack++;
      break;
   case VG_FREEBSD_SYSCALL198:
      gst->guest_EAX = __NR___syscall;
      stack[1] = canonical->sysno;
      stack += 2;
      break;
   default:
      gst->guest_EAX = canonical->sysno;
      break;
   }

   stack[1] = canonical->arg1;
   stack[2] = canonical->arg2;
   stack[3] = canonical->arg3;
   stack[4] = canonical->arg4;
   stack[5] = canonical->arg5;
   stack[6] = canonical->arg6;
   stack[7] = canonical->arg7;
   stack[8] = canonical->arg8;

#elif defined(VGP_amd64_freebsd)
   VexGuestAMD64State* gst = (VexGuestAMD64State*)gst_vanilla;
   UWord *stack = (UWord *)gst->guest_RSP;

   // stack[0] is a (fake) return address
   switch (canonical->klass) {
   case VG_FREEBSD_SYSCALL0:
      gst->guest_RAX = __NR_syscall;
      break;
   case VG_FREEBSD_SYSCALL198:
      gst->guest_RAX = __NR___syscall;
      break;
   default:
      gst->guest_RAX = canonical->sysno;
      break;
   }

   if (canonical->klass == VG_FREEBSD_SYSCALL0 || canonical->klass == VG_FREEBSD_SYSCALL198) {
       gst->guest_RDI = canonical->sysno;
       gst->guest_RSI = canonical->arg1;
       gst->guest_RDX = canonical->arg2;
       gst->guest_R10 = canonical->arg3;
       gst->guest_R8  = canonical->arg4;
       gst->guest_R9  = canonical->arg5;
       stack[1]       = canonical->arg6;
       stack[2]       = canonical->arg7;
       stack[3]       = canonical->arg8;
   } else {
       gst->guest_RDI = canonical->arg1;
       gst->guest_RSI = canonical->arg2;
       gst->guest_RDX = canonical->arg3;
       gst->guest_R10 = canonical->arg4;
       gst->guest_R8  = canonical->arg5;
       gst->guest_R9  = canonical->arg6;
       stack[1]       = canonical->arg7;
       stack[2]       = canonical->arg8;
   }

#elif defined(VGP_arm_linux)
   VexGuestARMState* gst = (VexGuestARMState*)gst_vanilla;
   gst->guest_R7 = canonical->sysno;
   gst->guest_R0 = canonical->arg1;
   gst->guest_R1 = canonical->arg2;
   gst->guest_R2 = canonical->arg3;
   gst->guest_R3 = canonical->arg4;
   gst->guest_R4 = canonical->arg5;
   gst->guest_R5 = canonical->arg6;

#elif defined(VGP_arm64_linux)
   VexGuestARM64State* gst = (VexGuestARM64State*)gst_vanilla;
   gst->guest_X8 = canonical->sysno;
   gst->guest_X0 = canonical->arg1;
   gst->guest_X1 = canonical->arg2;
   gst->guest_X2 = canonical->arg3;
   gst->guest_X3 = canonical->arg4;
   gst->guest_X4 = canonical->arg5;
   gst->guest_X5 = canonical->arg6;

#elif defined(VGP_x86_darwin)
   VexGuestX86State* gst = (VexGuestX86State*)gst_vanilla;
   UWord *stack = (UWord *)gst->guest_ESP;

   gst->guest_EAX = VG_DARWIN_SYSNO_FOR_KERNEL(canonical->sysno);

   // GrP fixme? gst->guest_TEMP_EFLAG_C = 0;
   // stack[0] is return address
   stack[1] = canonical->arg1;
   stack[2] = canonical->arg2;
   stack[3] = canonical->arg3;
   stack[4] = canonical->arg4;
   stack[5] = canonical->arg5;
   stack[6] = canonical->arg6;
   stack[7] = canonical->arg7;
   stack[8] = canonical->arg8;
   
#elif defined(VGP_amd64_darwin)
   VexGuestAMD64State* gst = (VexGuestAMD64State*)gst_vanilla;
   UWord *stack = (UWord *)gst->guest_RSP;

   gst->guest_RAX = VG_DARWIN_SYSNO_FOR_KERNEL(canonical->sysno);
   // GrP fixme? gst->guest_TEMP_EFLAG_C = 0;

   // stack[0] is return address
   gst->guest_RDI = canonical->arg1;
   gst->guest_RSI = canonical->arg2;
   gst->guest_RDX = canonical->arg3;
   gst->guest_RCX = canonical->arg4;
   gst->guest_R8  = canonical->arg5;
   gst->guest_R9  = canonical->arg6;
   stack[1]       = canonical->arg7;
   stack[2]       = canonical->arg8;

#elif defined(VGP_s390x_linux)
   VexGuestS390XState* gst = (VexGuestS390XState*)gst_vanilla;
   gst->guest_SYSNO  = canonical->sysno;
   gst->guest_r2     = canonical->arg1;
   gst->guest_r3     = canonical->arg2;
   gst->guest_r4     = canonical->arg3;
   gst->guest_r5     = canonical->arg4;
   gst->guest_r6     = canonical->arg5;
   gst->guest_r7     = canonical->arg6;

#elif defined(VGP_mips32_linux)
   VexGuestMIPS32State* gst = (VexGuestMIPS32State*)gst_vanilla;
   if (canonical->arg8 != __NR_syscall) {
      gst->guest_r2 = canonical->sysno;
      gst->guest_r4 = canonical->arg1;
      gst->guest_r5 = canonical->arg2;
      gst->guest_r6 = canonical->arg3;
      gst->guest_r7 = canonical->arg4;
      *((UInt*) (gst->guest_r29 + 16)) = canonical->arg5; // 16(guest_GPR29/sp)
      *((UInt*) (gst->guest_r29 + 20)) = canonical->arg6; // 20(sp)
      *((UInt*) (gst->guest_r29 + 24)) = canonical->arg7; // 24(sp)
   } else {
      canonical->arg8 = 0;
      gst->guest_r2 = __NR_syscall;
      gst->guest_r4 = canonical->sysno;
      gst->guest_r5 = canonical->arg1;
      gst->guest_r6 = canonical->arg2;
      gst->guest_r7 = canonical->arg3;
      *((UInt*) (gst->guest_r29 + 16)) = canonical->arg4; // 16(guest_GPR29/sp)
      *((UInt*) (gst->guest_r29 + 20)) = canonical->arg5; // 20(sp)
      *((UInt*) (gst->guest_r29 + 24)) = canonical->arg6; // 24(sp)
      *((UInt*) (gst->guest_r29 + 28)) = canonical->arg7; // 28(sp)
   }

#elif defined(VGP_nanomips_linux)
   VexGuestMIPS32State* gst = (VexGuestMIPS32State*)gst_vanilla;
   gst->guest_r2  = canonical->sysno;
   gst->guest_r4  = canonical->arg1;
   gst->guest_r5  = canonical->arg2;
   gst->guest_r6  = canonical->arg3;
   gst->guest_r7  = canonical->arg4;
   gst->guest_r8  = canonical->arg5;
   gst->guest_r9  = canonical->arg6;
   gst->guest_r10 = canonical->arg7;
   gst->guest_r11 = canonical->arg8;
#elif defined(VGP_mips64_linux)
   VexGuestMIPS64State* gst = (VexGuestMIPS64State*)gst_vanilla;
   gst->guest_r2 = canonical->sysno;
   gst->guest_r4 = canonical->arg1;
   gst->guest_r5 = canonical->arg2;
   gst->guest_r6 = canonical->arg3;
   gst->guest_r7 = canonical->arg4;
   gst->guest_r8 = canonical->arg5;
   gst->guest_r9 = canonical->arg6;
   gst->guest_r10 = canonical->arg7;
   gst->guest_r11 = canonical->arg8;

#elif defined(VGP_x86_solaris)
   VexGuestX86State* gst = (VexGuestX86State*)gst_vanilla;
   UWord *stack = (UWord *)gst->guest_ESP;

   /* Fasttraps or anything else cannot go through this way. */
   vg_assert(VG_SOLARIS_SYSNO_CLASS(canonical->sysno)
             == VG_SOLARIS_SYSCALL_CLASS_CLASSIC);
   gst->guest_EAX = canonical->sysno;
   /* stack[0] is a return address. */
   stack[1] = canonical->arg1;
   stack[2] = canonical->arg2;
   stack[3] = canonical->arg3;
   stack[4] = canonical->arg4;
   stack[5] = canonical->arg5;
   stack[6] = canonical->arg6;
   stack[7] = canonical->arg7;
   stack[8] = canonical->arg8;

#elif defined(VGP_amd64_solaris)
   VexGuestAMD64State* gst = (VexGuestAMD64State*)gst_vanilla;
   UWord *stack = (UWord *)gst->guest_RSP;

   /* Fasttraps or anything else cannot go through this way. */
   vg_assert(VG_SOLARIS_SYSNO_CLASS(canonical->sysno)
             == VG_SOLARIS_SYSCALL_CLASS_CLASSIC);
   gst->guest_RAX = canonical->sysno;
   /* stack[0] is a return address. */
   gst->guest_RDI = canonical->arg1;
   gst->guest_RSI = canonical->arg2;
   gst->guest_RDX = canonical->arg3;
   gst->guest_R10 = canonical->arg4;
   gst->guest_R8  = canonical->arg5;
   gst->guest_R9  = canonical->arg6;
   stack[1] = canonical->arg7;
   stack[2] = canonical->arg8;

#else
#  error "putSyscallArgsIntoGuestState: unknown arch"
#endif
}

static
void getSyscallStatusFromGuestState ( /*OUT*/SyscallStatus*     canonical,
                                      /*IN*/ VexGuestArchState* gst_vanilla )
{
#  if defined(VGP_x86_linux)
   VexGuestX86State* gst = (VexGuestX86State*)gst_vanilla;
   canonical->sres = VG_(mk_SysRes_x86_linux)( gst->guest_EAX );
   canonical->what = SsComplete;

#  elif defined(VGP_amd64_linux)
   VexGuestAMD64State* gst = (VexGuestAMD64State*)gst_vanilla;
   canonical->sres = VG_(mk_SysRes_amd64_linux)( gst->guest_RAX );
   canonical->what = SsComplete;

#  elif defined(VGP_ppc32_linux)
   VexGuestPPC32State* gst   = (VexGuestPPC32State*)gst_vanilla;
   UInt                cr    = LibVEX_GuestPPC32_get_CR( gst );
   UInt                cr0so = (cr >> 28) & 1;
   canonical->sres = VG_(mk_SysRes_ppc32_linux)( gst->guest_GPR3, cr0so );
   canonical->what = SsComplete;

#  elif defined(VGP_ppc64be_linux) || defined(VGP_ppc64le_linux)
   /* There is a Valgrind specific guest state register guest_syscall_flag
      that is set to zero to indicate if the sc instruction was used or one
      if the scv instruction was used for the system call.  */
   VexGuestPPC64State* gst   = (VexGuestPPC64State*)gst_vanilla;
   UInt                cr    = LibVEX_GuestPPC64_get_CR( gst );
   UInt                cr0so = (cr >> 28) & 1;
   UInt                flag  =  gst->guest_syscall_flag;

   canonical->sres = VG_(mk_SysRes_ppc64_linux)( gst->guest_GPR3, cr0so, flag );
   canonical->what = SsComplete;

#  elif defined(VGP_x86_freebsd)
   /* duplicates logic in m_signals.VG_UCONTEXT_SYSCALL_SYSRES */
   VexGuestX86State* gst = (VexGuestX86State*)gst_vanilla;
   UInt flags = LibVEX_GuestX86_get_eflags(gst);

   canonical->sres = VG_(mk_SysRes_x86_freebsd)(gst->guest_EAX, gst->guest_EDX,
                        (flags & 1) != 0 ? True : False);
   canonical->what = SsComplete;

#  elif defined(VGP_arm_linux)
   VexGuestARMState* gst = (VexGuestARMState*)gst_vanilla;
   canonical->sres = VG_(mk_SysRes_arm_linux)( gst->guest_R0 );
   canonical->what = SsComplete;

#  elif defined(VGP_arm64_linux)
   VexGuestARM64State* gst = (VexGuestARM64State*)gst_vanilla;
   canonical->sres = VG_(mk_SysRes_arm64_linux)( gst->guest_X0 );
   canonical->what = SsComplete;

#  elif defined(VGP_mips32_linux)
   VexGuestMIPS32State* gst = (VexGuestMIPS32State*)gst_vanilla;
   UInt                v0 = gst->guest_r2;    // v0
   UInt                v1 = gst->guest_r3;    // v1
   UInt                a3 = gst->guest_r7;    // a3
   canonical->sres = VG_(mk_SysRes_mips32_linux)( v0, v1, a3 );
   canonical->what = SsComplete;

#  elif defined(VGP_mips64_linux)
   VexGuestMIPS64State* gst = (VexGuestMIPS64State*)gst_vanilla;
   ULong                v0 = gst->guest_r2;    // v0
   ULong                v1 = gst->guest_r3;    // v1
   ULong                a3 = gst->guest_r7;    // a3
   canonical->sres = VG_(mk_SysRes_mips64_linux)(v0, v1, a3);
   canonical->what = SsComplete;

#  elif defined(VGP_nanomips_linux)
   VexGuestMIPS32State* gst = (VexGuestMIPS32State*)gst_vanilla;
   RegWord  a0 = gst->guest_r4;    // a0
   canonical->sres = VG_(mk_SysRes_nanomips_linux)(a0);
   canonical->what = SsComplete;
#  elif defined(VGP_amd64_freebsd)
   /* duplicates logic in m_signals.VG_UCONTEXT_SYSCALL_SYSRES */
   VexGuestAMD64State* gst = (VexGuestAMD64State*)gst_vanilla;
   ULong flags = LibVEX_GuestAMD64_get_rflags(gst);
   canonical->sres = VG_(mk_SysRes_amd64_freebsd)(gst->guest_RAX, gst->guest_RDX,
                        (flags & 1) != 0 ? True : False);
   canonical->what = SsComplete;

#  elif defined(VGP_x86_darwin)
   /* duplicates logic in m_signals.VG_UCONTEXT_SYSCALL_SYSRES */
   VexGuestX86State* gst = (VexGuestX86State*)gst_vanilla;
   UInt carry = 1 & LibVEX_GuestX86_get_eflags(gst);
   UInt err = 0;
   UInt wLO = 0;
   UInt wHI = 0;
   switch (gst->guest_SC_CLASS) {
      case VG_DARWIN_SYSCALL_CLASS_UNIX:
         // int $0x80 = Unix, 64-bit result
         err = carry;
         wLO = gst->guest_EAX;
         wHI = gst->guest_EDX;
         break;
      case VG_DARWIN_SYSCALL_CLASS_MACH:
         // int $0x81 = Mach, 32-bit result
         wLO = gst->guest_EAX;
         break;
      case VG_DARWIN_SYSCALL_CLASS_MDEP:
         // int $0x82 = mdep, 32-bit result
         wLO = gst->guest_EAX;
         break;
      default: 
         vg_assert(0);
         break;
   }
   canonical->sres = VG_(mk_SysRes_x86_darwin)(
                        gst->guest_SC_CLASS, err ? True : False, 
                        wHI, wLO
                     );
   canonical->what = SsComplete;

#  elif defined(VGP_amd64_darwin)
   /* duplicates logic in m_signals.VG_UCONTEXT_SYSCALL_SYSRES */
   VexGuestAMD64State* gst = (VexGuestAMD64State*)gst_vanilla;
   ULong carry = 1 & LibVEX_GuestAMD64_get_rflags(gst);
   ULong err = 0;
   ULong wLO = 0;
   ULong wHI = 0;
   switch (gst->guest_SC_CLASS) {
      case VG_DARWIN_SYSCALL_CLASS_UNIX:
         // syscall = Unix, 128-bit result
         err = carry;
         wLO = gst->guest_RAX;
         wHI = gst->guest_RDX;
         break;
      case VG_DARWIN_SYSCALL_CLASS_MACH:
         // syscall = Mach, 64-bit result
         wLO = gst->guest_RAX;
         break;
      case VG_DARWIN_SYSCALL_CLASS_MDEP:
         // syscall = mdep, 64-bit result
         wLO = gst->guest_RAX;
         break;
      default: 
         vg_assert(0);
         break;
   }
   canonical->sres = VG_(mk_SysRes_amd64_darwin)(
                        gst->guest_SC_CLASS, err ? True : False, 
                        wHI, wLO
                     );
   canonical->what = SsComplete;

#  elif defined(VGP_s390x_linux)
   VexGuestS390XState* gst   = (VexGuestS390XState*)gst_vanilla;
   canonical->sres = VG_(mk_SysRes_s390x_linux)( gst->guest_r2 );
   canonical->what = SsComplete;

#  elif defined(VGP_x86_solaris)
   VexGuestX86State* gst = (VexGuestX86State*)gst_vanilla;
   UInt carry = 1 & LibVEX_GuestX86_get_eflags(gst);

   canonical->sres = VG_(mk_SysRes_x86_solaris)(carry ? True : False,
                                                gst->guest_EAX,
                                                carry ? 0 : gst->guest_EDX);
   canonical->what = SsComplete;

#  elif defined(VGP_amd64_solaris)
   VexGuestAMD64State* gst = (VexGuestAMD64State*)gst_vanilla;
   UInt carry = 1 & LibVEX_GuestAMD64_get_rflags(gst);

   canonical->sres = VG_(mk_SysRes_amd64_solaris)(carry ? True : False,
                                                  gst->guest_RAX,
                                                  carry ? 0 : gst->guest_RDX);
   canonical->what = SsComplete;

#  else
#    error "getSyscallStatusFromGuestState: unknown arch"
#  endif
}

static 
void putSyscallStatusIntoGuestState ( /*IN*/ ThreadId tid, 
                                      /*IN*/ SyscallStatus*     canonical,
                                      /*OUT*/VexGuestArchState* gst_vanilla )
{
#  if defined(VGP_x86_linux)
   VexGuestX86State* gst = (VexGuestX86State*)gst_vanilla;
   vg_assert(canonical->what == SsComplete);
   if (sr_isError(canonical->sres)) {
      /* This isn't exactly right, in that really a Failure with res
         not in the range 1 .. 4095 is unrepresentable in the
         Linux-x86 scheme.  Oh well. */
      gst->guest_EAX = - (Int)sr_Err(canonical->sres);
   } else {
      gst->guest_EAX = sr_Res(canonical->sres);
   }
   VG_TRACK( post_reg_write, Vg_CoreSysCall, tid, 
             OFFSET_x86_EAX, sizeof(UWord) );

#  elif defined(VGP_amd64_linux)
   VexGuestAMD64State* gst = (VexGuestAMD64State*)gst_vanilla;
   vg_assert(canonical->what == SsComplete);
   if (sr_isError(canonical->sres)) {
      /* This isn't exactly right, in that really a Failure with res
         not in the range 1 .. 4095 is unrepresentable in the
         Linux-amd64 scheme.  Oh well. */
      gst->guest_RAX = - (Long)sr_Err(canonical->sres);
   } else {
      gst->guest_RAX = sr_Res(canonical->sres);
   }
   VG_TRACK( post_reg_write, Vg_CoreSysCall, tid, 
             OFFSET_amd64_RAX, sizeof(UWord) );

#  elif defined(VGP_ppc32_linux)
   VexGuestPPC32State* gst = (VexGuestPPC32State*)gst_vanilla;
   UInt old_cr = LibVEX_GuestPPC32_get_CR(gst);
   vg_assert(canonical->what == SsComplete);
   if (sr_isError(canonical->sres)) {
      /* set CR0.SO */
      LibVEX_GuestPPC32_put_CR( old_cr | (1<<28), gst );
      gst->guest_GPR3 = sr_Err(canonical->sres);
   } else {
      /* clear CR0.SO */
      LibVEX_GuestPPC32_put_CR( old_cr & ~(1<<28), gst );
      gst->guest_GPR3 = sr_Res(canonical->sres);
   }
   VG_TRACK( post_reg_write, Vg_CoreSysCall, tid, 
             OFFSET_ppc32_GPR3, sizeof(UWord) );
   VG_TRACK( post_reg_write, Vg_CoreSysCall, tid, 
             OFFSET_ppc32_CR0_0, sizeof(UChar) );

#  elif defined(VGP_ppc64be_linux) || defined(VGP_ppc64le_linux)
   VexGuestPPC64State* gst = (VexGuestPPC64State*)gst_vanilla;
   UInt old_cr = LibVEX_GuestPPC64_get_CR(gst);
   UInt flag  =  gst->guest_syscall_flag;

   vg_assert(canonical->what == SsComplete);
   if (flag == SC_FLAG) {
      /* sc syscall */
      if (sr_isError(canonical->sres)) {
         /* set CR0.SO */
         LibVEX_GuestPPC64_put_CR( old_cr | (1<<28), gst );
         gst->guest_GPR3 = sr_Err(canonical->sres);
      } else {
         /* clear CR0.SO */
         LibVEX_GuestPPC64_put_CR( old_cr & ~(1<<28), gst );
         gst->guest_GPR3 = sr_Res(canonical->sres);
      }
      VG_TRACK( post_reg_write, Vg_CoreSysCall, tid,
                OFFSET_ppc64_GPR3, sizeof(UWord) );
      VG_TRACK( post_reg_write, Vg_CoreSysCall, tid,
                OFFSET_ppc64_CR0_0, sizeof(UChar) );
   } else {
      /* scv system call instruction */
      if (sr_isError(canonical->sres))
         gst->guest_GPR3 = - (Long)canonical->sres._val;
      else
         gst->guest_GPR3 = canonical->sres._val;

      VG_TRACK( post_reg_write, Vg_CoreSysCall, tid,
                OFFSET_ppc64_GPR3, sizeof(UWord) );
   }

#  elif defined(VGP_arm_linux)
   VexGuestARMState* gst = (VexGuestARMState*)gst_vanilla;
   vg_assert(canonical->what == SsComplete);
   if (sr_isError(canonical->sres)) {
      /* This isn't exactly right, in that really a Failure with res
         not in the range 1 .. 4095 is unrepresentable in the
         Linux-arm scheme.  Oh well. */
      gst->guest_R0 = - (Int)sr_Err(canonical->sres);
   } else {
      gst->guest_R0 = sr_Res(canonical->sres);
   }
   VG_TRACK( post_reg_write, Vg_CoreSysCall, tid, 
             OFFSET_arm_R0, sizeof(UWord) );

#  elif defined(VGP_arm64_linux)
   VexGuestARM64State* gst = (VexGuestARM64State*)gst_vanilla;
   vg_assert(canonical->what == SsComplete);
   if (sr_isError(canonical->sres)) {
      /* This isn't exactly right, in that really a Failure with res
         not in the range 1 .. 4095 is unrepresentable in the
         Linux-arm64 scheme.  Oh well. */
      gst->guest_X0 = - (Long)sr_Err(canonical->sres);
   } else {
      gst->guest_X0 = sr_Res(canonical->sres);
   }
   VG_TRACK( post_reg_write, Vg_CoreSysCall, tid, 
             OFFSET_arm64_X0, sizeof(UWord) );

#elif defined(VGP_x86_freebsd)
   VexGuestX86State* gst = (VexGuestX86State*)gst_vanilla;
   vg_assert(canonical->what == SsComplete);
   if (sr_isError(canonical->sres)) {
      gst->guest_EAX = sr_Err(canonical->sres);
      LibVEX_GuestX86_put_eflag_c(1, gst);
   } else {
      gst->guest_EAX = sr_Res(canonical->sres);
      gst->guest_EDX = sr_ResHI(canonical->sres);
      LibVEX_GuestX86_put_eflag_c(0, gst);
   }
   VG_TRACK( post_reg_write, Vg_CoreSysCall, tid,
      OFFSET_x86_EAX, sizeof(UInt) );
   VG_TRACK( post_reg_write, Vg_CoreSysCall, tid,
      OFFSET_x86_EDX, sizeof(UInt) );
   // GrP fixme sets defined for entire eflags, not just bit c
   VG_TRACK( post_reg_write, Vg_CoreSysCall, tid,
      offsetof(VexGuestX86State, guest_CC_DEP1), sizeof(UInt) );
 
#elif defined(VGP_amd64_freebsd)
   VexGuestAMD64State* gst = (VexGuestAMD64State*)gst_vanilla;
   vg_assert(canonical->what == SsComplete);
   if (sr_isError(canonical->sres)) {
      gst->guest_RAX = sr_Err(canonical->sres);
      LibVEX_GuestAMD64_put_rflag_c(1, gst);
   } else {
      gst->guest_RAX = sr_Res(canonical->sres);
      gst->guest_RDX = sr_ResHI(canonical->sres);
      LibVEX_GuestAMD64_put_rflag_c(0, gst);
   }
   VG_TRACK( post_reg_write, Vg_CoreSysCall, tid,
      OFFSET_amd64_RAX, sizeof(ULong) );
   VG_TRACK( post_reg_write, Vg_CoreSysCall, tid,
      OFFSET_amd64_RDX, sizeof(ULong) );
   // GrP fixme sets defined for entire eflags, not just bit c
   VG_TRACK( post_reg_write, Vg_CoreSysCall, tid,
      offsetof(VexGuestAMD64State, guest_CC_DEP1), sizeof(ULong) );
#elif defined(VGP_x86_darwin)
   VexGuestX86State* gst = (VexGuestX86State*)gst_vanilla;
   SysRes sres = canonical->sres;
   vg_assert(canonical->what == SsComplete);
   /* Unfortunately here we have to break abstraction and look
      directly inside 'res', in order to decide what to do. */
   switch (sres._mode) {
      case SysRes_MACH: // int $0x81 = Mach, 32-bit result
      case SysRes_MDEP: // int $0x82 = mdep, 32-bit result
         gst->guest_EAX = sres._wLO;
         VG_TRACK( post_reg_write, Vg_CoreSysCall, tid, 
                   OFFSET_x86_EAX, sizeof(UInt) );
         break;
      case SysRes_UNIX_OK:  // int $0x80 = Unix, 64-bit result
      case SysRes_UNIX_ERR: // int $0x80 = Unix, 64-bit error
         gst->guest_EAX = sres._wLO;
         VG_TRACK( post_reg_write, Vg_CoreSysCall, tid, 
                   OFFSET_x86_EAX, sizeof(UInt) );
         gst->guest_EDX = sres._wHI;
         VG_TRACK( post_reg_write, Vg_CoreSysCall, tid, 
                   OFFSET_x86_EDX, sizeof(UInt) );
         LibVEX_GuestX86_put_eflag_c( sres._mode==SysRes_UNIX_ERR ? 1 : 0,
                                      gst );
         // GrP fixme sets defined for entire eflags, not just bit c
         // DDD: this breaks exp-ptrcheck.
         VG_TRACK( post_reg_write, Vg_CoreSysCall, tid, 
                   offsetof(VexGuestX86State, guest_CC_DEP1), sizeof(UInt) );
         break;
      default: 
         vg_assert(0);
         break;
   }
   
#elif defined(VGP_amd64_darwin)
   VexGuestAMD64State* gst = (VexGuestAMD64State*)gst_vanilla;
   SysRes sres = canonical->sres;
   vg_assert(canonical->what == SsComplete);
   /* Unfortunately here we have to break abstraction and look
      directly inside 'res', in order to decide what to do. */
   switch (sres._mode) {
      case SysRes_MACH: // syscall = Mach, 64-bit result
      case SysRes_MDEP: // syscall = mdep, 64-bit result
         gst->guest_RAX = sres._wLO;
         VG_TRACK( post_reg_write, Vg_CoreSysCall, tid, 
                   OFFSET_amd64_RAX, sizeof(ULong) );
         break;
      case SysRes_UNIX_OK:  // syscall = Unix, 128-bit result
      case SysRes_UNIX_ERR: // syscall = Unix, 128-bit error
         gst->guest_RAX = sres._wLO;
         VG_TRACK( post_reg_write, Vg_CoreSysCall, tid, 
                   OFFSET_amd64_RAX, sizeof(ULong) );
         gst->guest_RDX = sres._wHI;
         VG_TRACK( post_reg_write, Vg_CoreSysCall, tid, 
                   OFFSET_amd64_RDX, sizeof(ULong) );
         LibVEX_GuestAMD64_put_rflag_c( sres._mode==SysRes_UNIX_ERR ? 1 : 0,
                                        gst );
         // GrP fixme sets defined for entire rflags, not just bit c
         // DDD: this breaks exp-ptrcheck.
         VG_TRACK( post_reg_write, Vg_CoreSysCall, tid, 
                   offsetof(VexGuestAMD64State, guest_CC_DEP1), sizeof(ULong) );
         break;
      default: 
         vg_assert(0);
         break;
   }
   
#  elif defined(VGP_s390x_linux)
   VexGuestS390XState* gst = (VexGuestS390XState*)gst_vanilla;
   vg_assert(canonical->what == SsComplete);
   if (sr_isError(canonical->sres)) {
      gst->guest_r2 = - (Long)sr_Err(canonical->sres);
   } else {
      gst->guest_r2 = sr_Res(canonical->sres);
   }

#  elif defined(VGP_mips32_linux)
   VexGuestMIPS32State* gst = (VexGuestMIPS32State*)gst_vanilla;
   vg_assert(canonical->what == SsComplete);
   if (sr_isError(canonical->sres)) {
      gst->guest_r2 = (Int)sr_Err(canonical->sres);
      gst->guest_r7 = (Int)sr_Err(canonical->sres);
   } else {
      gst->guest_r2 = sr_Res(canonical->sres);
      gst->guest_r3 = sr_ResEx(canonical->sres);
      gst->guest_r7 = (Int)sr_Err(canonical->sres);
   }
   VG_TRACK( post_reg_write, Vg_CoreSysCall, tid,
             OFFSET_mips32_r2, sizeof(UWord) );
   VG_TRACK( post_reg_write, Vg_CoreSysCall, tid,
             OFFSET_mips32_r3, sizeof(UWord) );
   VG_TRACK( post_reg_write, Vg_CoreSysCall, tid,
             OFFSET_mips32_r7, sizeof(UWord) );

#  elif defined(VGP_mips64_linux)
   VexGuestMIPS64State* gst = (VexGuestMIPS64State*)gst_vanilla;
   vg_assert(canonical->what == SsComplete);
   if (sr_isError(canonical->sres)) {
      gst->guest_r2 = (Int)sr_Err(canonical->sres);
      gst->guest_r7 = (Int)sr_Err(canonical->sres);
   } else {
      gst->guest_r2 = sr_Res(canonical->sres);
      gst->guest_r3 = sr_ResEx(canonical->sres);
      gst->guest_r7 = (Int)sr_Err(canonical->sres);
   }
   VG_TRACK( post_reg_write, Vg_CoreSysCall, tid,
             OFFSET_mips64_r2, sizeof(UWord) );
   VG_TRACK( post_reg_write, Vg_CoreSysCall, tid,
             OFFSET_mips64_r3, sizeof(UWord) );
   VG_TRACK( post_reg_write, Vg_CoreSysCall, tid,
             OFFSET_mips64_r7, sizeof(UWord) );

#  elif defined(VGP_nanomips_linux)
   VexGuestMIPS32State* gst = (VexGuestMIPS32State*)gst_vanilla;
   vg_assert(canonical->what == SsComplete);
   gst->guest_r4 = canonical->sres._val;
   VG_TRACK( post_reg_write, Vg_CoreSysCall, tid,
             OFFSET_mips32_r4, sizeof(UWord) );

#  elif defined(VGP_x86_solaris)
   VexGuestX86State* gst = (VexGuestX86State*)gst_vanilla;
   SysRes sres = canonical->sres;
   vg_assert(canonical->what == SsComplete);

   if (sr_isError(sres)) {
      gst->guest_EAX = sr_Err(sres);
      VG_TRACK(post_reg_write, Vg_CoreSysCall, tid, OFFSET_x86_EAX,
               sizeof(UInt));
      LibVEX_GuestX86_put_eflag_c(1, gst);
   }
   else {
      gst->guest_EAX = sr_Res(sres);
      VG_TRACK(post_reg_write, Vg_CoreSysCall, tid, OFFSET_x86_EAX,
               sizeof(UInt));
      gst->guest_EDX = sr_ResHI(sres);
      VG_TRACK(post_reg_write, Vg_CoreSysCall, tid, OFFSET_x86_EDX,
               sizeof(UInt));
      LibVEX_GuestX86_put_eflag_c(0, gst);
   }
   /* Make CC_DEP1 and CC_DEP2 defined.  This is inaccurate because it makes
      other eflags defined too (see README.solaris). */
   VG_TRACK(post_reg_write, Vg_CoreSysCall, tid, offsetof(VexGuestX86State,
            guest_CC_DEP1), sizeof(UInt));
   VG_TRACK(post_reg_write, Vg_CoreSysCall, tid, offsetof(VexGuestX86State,
            guest_CC_DEP2), sizeof(UInt));

#  elif defined(VGP_amd64_solaris)
   VexGuestAMD64State* gst = (VexGuestAMD64State*)gst_vanilla;
   SysRes sres = canonical->sres;
   vg_assert(canonical->what == SsComplete);

   if (sr_isError(sres)) {
      gst->guest_RAX = sr_Err(sres);
      VG_TRACK(post_reg_write, Vg_CoreSysCall, tid, OFFSET_amd64_RAX,
               sizeof(ULong));
      LibVEX_GuestAMD64_put_rflag_c(1, gst);
   }
   else {
      gst->guest_RAX = sr_Res(sres);
      VG_TRACK(post_reg_write, Vg_CoreSysCall, tid, OFFSET_amd64_RAX,
               sizeof(ULong));
      gst->guest_RDX = sr_ResHI(sres);
      VG_TRACK(post_reg_write, Vg_CoreSysCall, tid, OFFSET_amd64_RDX,
               sizeof(ULong));
      LibVEX_GuestAMD64_put_rflag_c(0, gst);
   }
   /* Make CC_DEP1 and CC_DEP2 defined.  This is inaccurate because it makes
      other eflags defined too (see README.solaris). */
   VG_TRACK(post_reg_write, Vg_CoreSysCall, tid, offsetof(VexGuestAMD64State,
            guest_CC_DEP1), sizeof(ULong));
   VG_TRACK(post_reg_write, Vg_CoreSysCall, tid, offsetof(VexGuestAMD64State,
            guest_CC_DEP2), sizeof(ULong));

#  else
#    error "putSyscallStatusIntoGuestState: unknown arch"
#  endif
}


/* Tell me the offsets in the guest state of the syscall params, so
   that the scalar argument checkers don't have to have this info
   hardwired. */

static
void getSyscallArgLayout ( /*OUT*/SyscallArgLayout* layout )
{
   VG_(bzero_inline)(layout, sizeof(*layout));

#if defined(VGP_x86_linux)
   layout->o_sysno  = OFFSET_x86_EAX;
   layout->o_arg1   = OFFSET_x86_EBX;
   layout->o_arg2   = OFFSET_x86_ECX;
   layout->o_arg3   = OFFSET_x86_EDX;
   layout->o_arg4   = OFFSET_x86_ESI;
   layout->o_arg5   = OFFSET_x86_EDI;
   layout->o_arg6   = OFFSET_x86_EBP;
   layout->uu_arg7  = -1; /* impossible value */
   layout->uu_arg8  = -1; /* impossible value */

#elif defined(VGP_amd64_linux)
   layout->o_sysno  = OFFSET_amd64_RAX;
   layout->o_arg1   = OFFSET_amd64_RDI;
   layout->o_arg2   = OFFSET_amd64_RSI;
   layout->o_arg3   = OFFSET_amd64_RDX;
   layout->o_arg4   = OFFSET_amd64_R10;
   layout->o_arg5   = OFFSET_amd64_R8;
   layout->o_arg6   = OFFSET_amd64_R9;
   layout->uu_arg7  = -1; /* impossible value */
   layout->uu_arg8  = -1; /* impossible value */

#elif defined(VGP_ppc32_linux)
   layout->o_sysno  = OFFSET_ppc32_GPR0;
   layout->o_arg1   = OFFSET_ppc32_GPR3;
   layout->o_arg2   = OFFSET_ppc32_GPR4;
   layout->o_arg3   = OFFSET_ppc32_GPR5;
   layout->o_arg4   = OFFSET_ppc32_GPR6;
   layout->o_arg5   = OFFSET_ppc32_GPR7;
   layout->o_arg6   = OFFSET_ppc32_GPR8;
   layout->uu_arg7  = -1; /* impossible value */
   layout->uu_arg8  = -1; /* impossible value */

#elif defined(VGP_ppc64be_linux) || defined(VGP_ppc64le_linux)
   layout->o_sysno  = OFFSET_ppc64_GPR0;
   layout->o_arg1   = OFFSET_ppc64_GPR3;
   layout->o_arg2   = OFFSET_ppc64_GPR4;
   layout->o_arg3   = OFFSET_ppc64_GPR5;
   layout->o_arg4   = OFFSET_ppc64_GPR6;
   layout->o_arg5   = OFFSET_ppc64_GPR7;
   layout->o_arg6   = OFFSET_ppc64_GPR8;
   layout->o_arg7   = OFFSET_ppc64_GPR9;
   layout->uu_arg8  = -1; /* impossible value */

#elif defined(VGP_x86_freebsd)
   layout->o_sysno  = OFFSET_x86_EAX;
   // syscall parameters are on stack in C convention
   layout->s_arg1   = sizeof(UWord) * 1;
   layout->s_arg2   = sizeof(UWord) * 2;
   layout->s_arg3   = sizeof(UWord) * 3;
   layout->s_arg4   = sizeof(UWord) * 4;
   layout->s_arg5   = sizeof(UWord) * 5;
   layout->s_arg6   = sizeof(UWord) * 6;
   layout->s_arg7   = sizeof(UWord) * 7;
   layout->s_arg8   = sizeof(UWord) * 8;
 
#elif defined(VGP_amd64_freebsd)
   layout->o_sysno  = OFFSET_amd64_RAX;
   layout->o_arg1   = OFFSET_amd64_RDI;
   layout->o_arg2   = OFFSET_amd64_RSI;
   layout->o_arg3   = OFFSET_amd64_RDX;
   layout->o_arg4   = OFFSET_amd64_R10;
   layout->o_arg5   = OFFSET_amd64_R8;
   layout->o_arg6   = OFFSET_amd64_R9;
   layout->s_arg7   = sizeof(UWord) * 1;
   layout->s_arg8   = sizeof(UWord) * 2;
   layout->arg6_is_reg = True;

#elif defined(VGP_arm_linux)
   layout->o_sysno  = OFFSET_arm_R7;
   layout->o_arg1   = OFFSET_arm_R0;
   layout->o_arg2   = OFFSET_arm_R1;
   layout->o_arg3   = OFFSET_arm_R2;
   layout->o_arg4   = OFFSET_arm_R3;
   layout->o_arg5   = OFFSET_arm_R4;
   layout->o_arg6   = OFFSET_arm_R5;
   layout->uu_arg7  = -1; /* impossible value */
   layout->uu_arg8  = -1; /* impossible value */

#elif defined(VGP_arm64_linux)
   layout->o_sysno  = OFFSET_arm64_X8;
   layout->o_arg1   = OFFSET_arm64_X0;
   layout->o_arg2   = OFFSET_arm64_X1;
   layout->o_arg3   = OFFSET_arm64_X2;
   layout->o_arg4   = OFFSET_arm64_X3;
   layout->o_arg5   = OFFSET_arm64_X4;
   layout->o_arg6   = OFFSET_arm64_X5;
   layout->uu_arg7  = -1; /* impossible value */
   layout->uu_arg8  = -1; /* impossible value */

#elif defined(VGP_mips32_linux)
   layout->o_sysno  = OFFSET_mips32_r2;
   layout->o_arg1   = OFFSET_mips32_r4;
   layout->o_arg2   = OFFSET_mips32_r5;
   layout->o_arg3   = OFFSET_mips32_r6;
   layout->o_arg4   = OFFSET_mips32_r7;
   layout->s_arg5   = sizeof(UWord) * 4;
   layout->s_arg6   = sizeof(UWord) * 5;
   layout->s_arg7   = sizeof(UWord) * 6;
   layout->uu_arg8  = -1; /* impossible value */

#elif defined(VGP_nanomips_linux)
   layout->o_sysno  = OFFSET_mips32_r2;
   layout->o_arg1   = OFFSET_mips32_r4;
   layout->o_arg2   = OFFSET_mips32_r5;
   layout->o_arg3   = OFFSET_mips32_r6;
   layout->o_arg4   = OFFSET_mips32_r7;
   layout->o_arg5   = OFFSET_mips32_r8;
   layout->o_arg6   = OFFSET_mips32_r9;
   layout->uu_arg7  = -1; /* impossible value */
   layout->uu_arg8  = -1; /* impossible value */

#elif defined(VGP_mips64_linux)
   layout->o_sysno  = OFFSET_mips64_r2;
   layout->o_arg1   = OFFSET_mips64_r4;
   layout->o_arg2   = OFFSET_mips64_r5;
   layout->o_arg3   = OFFSET_mips64_r6;
   layout->o_arg4   = OFFSET_mips64_r7;
   layout->o_arg5   = OFFSET_mips64_r8;
   layout->o_arg6   = OFFSET_mips64_r9;
   layout->o_arg7   = OFFSET_mips64_r10;
   layout->o_arg8   = OFFSET_mips64_r11;

#elif defined(VGP_x86_darwin)
   layout->o_sysno  = OFFSET_x86_EAX;
   // syscall parameters are on stack in C convention
   layout->s_arg1   = sizeof(UWord) * 1;
   layout->s_arg2   = sizeof(UWord) * 2;
   layout->s_arg3   = sizeof(UWord) * 3;
   layout->s_arg4   = sizeof(UWord) * 4;
   layout->s_arg5   = sizeof(UWord) * 5;
   layout->s_arg6   = sizeof(UWord) * 6;
   layout->s_arg7   = sizeof(UWord) * 7;
   layout->s_arg8   = sizeof(UWord) * 8;
   
#elif defined(VGP_amd64_darwin)
   layout->o_sysno  = OFFSET_amd64_RAX;
   layout->o_arg1   = OFFSET_amd64_RDI;
   layout->o_arg2   = OFFSET_amd64_RSI;
   layout->o_arg3   = OFFSET_amd64_RDX;
   layout->o_arg4   = OFFSET_amd64_RCX;
   layout->o_arg5   = OFFSET_amd64_R8;
   layout->o_arg6   = OFFSET_amd64_R9;
   layout->s_arg7   = sizeof(UWord) * 1;
   layout->s_arg8   = sizeof(UWord) * 2;

#elif defined(VGP_s390x_linux)
   layout->o_sysno  = OFFSET_s390x_SYSNO;
   layout->o_arg1   = OFFSET_s390x_r2;
   layout->o_arg2   = OFFSET_s390x_r3;
   layout->o_arg3   = OFFSET_s390x_r4;
   layout->o_arg4   = OFFSET_s390x_r5;
   layout->o_arg5   = OFFSET_s390x_r6;
   layout->o_arg6   = OFFSET_s390x_r7;
   layout->uu_arg7  = -1; /* impossible value */
   layout->uu_arg8  = -1; /* impossible value */

#elif defined(VGP_x86_solaris)
   layout->o_sysno  = OFFSET_x86_EAX;
   /* Syscall parameters are on the stack. */
   layout->s_arg1   = sizeof(UWord) * 1;
   layout->s_arg2   = sizeof(UWord) * 2;
   layout->s_arg3   = sizeof(UWord) * 3;
   layout->s_arg4   = sizeof(UWord) * 4;
   layout->s_arg5   = sizeof(UWord) * 5;
   layout->s_arg6   = sizeof(UWord) * 6;
   layout->s_arg7   = sizeof(UWord) * 7;
   layout->s_arg8   = sizeof(UWord) * 8;

#elif defined(VGP_amd64_solaris)
   layout->o_sysno  = OFFSET_amd64_RAX;
   layout->o_arg1   = OFFSET_amd64_RDI;
   layout->o_arg2   = OFFSET_amd64_RSI;
   layout->o_arg3   = OFFSET_amd64_RDX;
   layout->o_arg4   = OFFSET_amd64_R10;
   layout->o_arg5   = OFFSET_amd64_R8;
   layout->o_arg6   = OFFSET_amd64_R9;
   layout->s_arg7   = sizeof(UWord) * 1;
   layout->s_arg8   = sizeof(UWord) * 2;

#else
#  error "getSyscallLayout: unknown arch"
#endif
}

#if defined(VGP_amd64_freebsd)
static
void getSyscallArgLayout_0_198 ( /*OUT*/SyscallArgLayout* layout )
{
   VG_(bzero_inline)(layout, sizeof(*layout));
   layout->o_sysno  = OFFSET_amd64_RDI;
   layout->o_arg1   = OFFSET_amd64_RSI;
   layout->o_arg2   = OFFSET_amd64_RDX;
   layout->o_arg3   = OFFSET_amd64_R10;
   layout->o_arg4   = OFFSET_amd64_R8;
   layout->o_arg5   = OFFSET_amd64_R9;
   layout->s_arg6   = sizeof(UWord) * 1;
   layout->s_arg7   = sizeof(UWord) * 2;
   layout->s_arg8   = sizeof(UWord) * 3;
   layout->arg6_is_reg = False;
}
#endif


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
   VG_(dmsg)("WARNING: unhandled %s syscall: %s\n",
      VG_PLATFORM, VG_SYSNUM_STRING(args->sysno));
   if (VG_(clo_verbosity) > 1) {
      VG_(get_and_pp_StackTrace)(tid, VG_(clo_backtrace_size));
   }
   VG_(dmsg)("You may be able to write your own handler.\n");
   VG_(dmsg)("Read the file README_MISSING_SYSCALL_OR_IOCTL.\n");
   VG_(dmsg)("Nevertheless we consider this a bug.  Please report\n");
   VG_(dmsg)("it at http://valgrind.org/support/bug_reports.html.\n");

   SET_STATUS_Failure(VKI_ENOSYS);

#  if defined(VGO_solaris)
   VG_(exit)(1);
#  endif
}

static SyscallTableEntry bad_sys =
   { bad_before, NULL };

static const SyscallTableEntry* get_syscall_entry ( Int syscallno )
{
   const SyscallTableEntry* sys = NULL;

#  if defined(VGO_linux)
   sys = ML_(get_linux_syscall_entry)( syscallno );

#  elif defined(VGO_freebsd)
   sys = ML_(get_freebsd_syscall_entry)( syscallno );

#  elif defined(VGO_darwin)
   Int idx = VG_DARWIN_SYSNO_INDEX(syscallno);

   switch (VG_DARWIN_SYSNO_CLASS(syscallno)) {
   case VG_DARWIN_SYSCALL_CLASS_UNIX:
      if (idx >= 0 && idx < ML_(syscall_table_size) &&
          ML_(syscall_table)[idx].before != NULL)
         sys = &ML_(syscall_table)[idx];
         break;
   case VG_DARWIN_SYSCALL_CLASS_MACH:
      if (idx >= 0 && idx < ML_(mach_trap_table_size) &&
          ML_(mach_trap_table)[idx].before != NULL)
         sys = &ML_(mach_trap_table)[idx];
         break;
   case VG_DARWIN_SYSCALL_CLASS_MDEP:
      if (idx >= 0 && idx < ML_(mdep_trap_table_size) &&
          ML_(mdep_trap_table)[idx].before != NULL)
         sys = &ML_(mdep_trap_table)[idx];
         break;
   default: 
      vg_assert(0);
      break;
   }

#  elif defined(VGO_solaris)
   sys = ML_(get_solaris_syscall_entry)(syscallno);

#  else
#    error Unknown OS
#  endif

   return sys == NULL  ? &bad_sys  : sys;
}


/* Add and remove signals from mask so that we end up telling the
   kernel the state we actually want rather than what the client
   wants. */
void VG_(sanitize_client_sigmask)(vki_sigset_t *mask)
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

SyscallInfo *syscallInfo;

/* The scheduler needs to be able to zero out these records after a
   fork, hence this is exported from m_syswrap. */
void VG_(clear_syscallInfo) ( ThreadId tid )
{
   vg_assert(syscallInfo);
   vg_assert(tid < VG_N_THREADS);
   VG_(memset)( & syscallInfo[tid], 0, sizeof( syscallInfo[tid] ));
   syscallInfo[tid].status.what = SsIdle;
}

Bool VG_(is_in_syscall) ( ThreadId tid )
{
   vg_assert(tid < VG_N_THREADS);
   return (syscallInfo && syscallInfo[tid].status.what != SsIdle);
}

Bool VG_(is_in_kernel_restart_syscall) ( ThreadId tid )
{
   vg_assert(tid < VG_N_THREADS);
   return (syscallInfo && ((syscallInfo[tid].flags & SfKernelRestart) != 0));
}

Word VG_(is_in_syscall_no) (ThreadId tid )
{
   vg_assert(tid < VG_N_THREADS);
   return syscallInfo[tid].orig_args.sysno;
}

static void ensure_initialised ( void )
{
   Int i;
   static Bool init_done = False;
   if (init_done) 
      return;
   init_done = True;

   syscallInfo = VG_(malloc)("scinfo", VG_N_THREADS * sizeof syscallInfo[0]);

   for (i = 0; i < VG_N_THREADS; i++) {
      VG_(clear_syscallInfo)( i );
   }
}

/* --- This is the main function of this file. --- */

void VG_(client_syscall) ( ThreadId tid, UInt trc )
{
   Word                     sysno;
   ThreadState*             tst;
   const SyscallTableEntry* ent;
   SyscallArgLayout         layout;
   SyscallInfo*             sci;

   ensure_initialised();

   vg_assert(VG_(is_valid_tid)(tid));
   vg_assert(tid >= 1 && tid < VG_N_THREADS);
   vg_assert(VG_(is_running_thread)(tid));

#  if !defined(VGO_darwin)
   // Resync filtering is meaningless on non-Darwin targets.
   vg_assert(VG_(clo_resync_filter) == 0);
#  endif

   tst = VG_(get_ThreadState)(tid);

   /* BEGIN ensure root thread's stack is suitably mapped */
   /* In some rare circumstances, we may do the syscall without the
      bottom page of the stack being mapped, because the stack pointer
      was moved down just a few instructions before the syscall
      instruction, and there have been no memory references since
      then, that would cause a call to VG_(extend_stack) to have
      happened.

      In native execution that's OK: the kernel automagically extends
      the stack's mapped area down to cover the stack pointer (or sp -
      redzone, really).  In simulated normal execution that's OK too,
      since any signals we get from accessing below the mapped area of
      the (guest's) stack lead us to VG_(extend_stack), where we
      simulate the kernel's stack extension logic.  But that leaves
      the problem of entering a syscall with the SP unmapped.  Because
      the kernel doesn't know that the segment immediately above SP is
      supposed to be a grow-down segment, it causes the syscall to
      fail, and thereby causes a divergence between native behaviour
      (syscall succeeds) and simulated behaviour (syscall fails).

      This is quite a rare failure mode.  It has only been seen
      affecting calls to sys_readlink on amd64-linux, and even then it
      requires a certain code sequence around the syscall to trigger
      it.  Here is one:

      extern int my_readlink ( const char* path );
      asm(
      ".text\n"
      ".globl my_readlink\n"
      "my_readlink:\n"
      "\tsubq    $0x1008,%rsp\n"
      "\tmovq    %rdi,%rdi\n"              // path is in rdi
      "\tmovq    %rsp,%rsi\n"              // &buf[0] -> rsi
      "\tmovl    $0x1000,%edx\n"           // sizeof(buf) in rdx
      "\tmovl    $"__NR_READLINK",%eax\n"  // syscall number
      "\tsyscall\n"
      "\taddq    $0x1008,%rsp\n"
      "\tret\n"
      ".previous\n"
      );

      For more details, see bug #156404
      (https://bugs.kde.org/show_bug.cgi?id=156404).

      The fix is actually very simple.  We simply need to call
      VG_(extend_stack) for this thread, handing it the lowest
      possible valid address for stack (sp - redzone), to ensure the
      pages all the way down to that address, are mapped.  Because
      this is a potentially expensive and frequent operation, we
      do the following:

      Only the main thread (tid=1) has a growdown stack.  So
      ignore all others.  It is conceivable, although highly unlikely,
      that the main thread exits, and later another thread is
      allocated tid=1, but that's harmless, I believe;
      VG_(extend_stack) will do nothing when applied to a non-root
      thread.

      All this guff is of course Linux-specific.  Hence the ifdef.
   */
#  if defined(VGO_linux)
   if (tid == 1/*ROOT THREAD*/) {
      Addr     stackMin   = VG_(get_SP)(tid) - VG_STACK_REDZONE_SZB;

      /* The precise thing to do here would be to extend the stack only
         if the system call can be proven to access unmapped user stack
         memory. That is an enormous amount of work even if a proper
         spec of system calls was available. 

         In the case where the system call does not access user memory
         the stack pointer here can have any value. A legitimate testcase
         that exercises this is none/tests/s390x/stmg.c:
         The stack pointer happens to be in the reservation segment near
         the end of the addressable memory and there is no SkAnonC segment
         above.

         So the approximation we're taking here is to extend the stack only
         if the client stack pointer does not look bogus. */
      if (VG_(am_addr_is_in_extensible_client_stack)(stackMin))
         VG_(extend_stack)( tid, stackMin );
   }
#  endif
   /* END ensure root thread's stack is suitably mapped */

   /* First off, get the syscall args and number.  This is a
      platform-dependent action. */

   sci = & syscallInfo[tid];
   vg_assert(sci->status.what == SsIdle);

   getSyscallArgsFromGuestState( &sci->orig_args, &tst->arch.vex, trc );

   /* Copy .orig_args to .args.  The pre-handler may modify .args, but
      we want to keep the originals too, just in case. */
   sci->args = sci->orig_args;

   /* Save the syscall number in the thread state in case the syscall 
      is interrupted by a signal. */
   sysno = sci->orig_args.sysno;

#  if defined(VGO_freebsd)
   tst->arch.vex.guest_SC_CLASS = sci->orig_args.klass;
#  endif
   /* It's sometimes useful, as a crude debugging hack, to get a
      stack trace at each (or selected) syscalls. */
   if (0 && sysno == __NR_ioctl) {
      VG_(umsg)("\nioctl:\n");
      VG_(get_and_pp_StackTrace)(tid, 10);
      VG_(umsg)("\n");
   }

#  if defined(VGO_darwin)
   /* Record syscall class.  But why?  Because the syscall might be
      interrupted by a signal, and in the signal handler (which will
      be m_signals.async_signalhandler) we will need to build a SysRes
      reflecting the syscall return result.  In order to do that we
      need to know the syscall class.  Hence stash it in the guest
      state of this thread.  This madness is not needed on Linux
      because it only has a single syscall return convention and so
      there is no ambiguity involved in converting the post-signal
      machine state into a SysRes. */
   tst->arch.vex.guest_SC_CLASS = VG_DARWIN_SYSNO_CLASS(sysno);
#  endif

   /* The default what-to-do-next thing is hand the syscall to the
      kernel, so we pre-set that here.  Set .sres to something
      harmless looking (is irrelevant because .what is not
      SsComplete.) */
   sci->status.what = SsHandToKernel;
   sci->status.sres = VG_(mk_SysRes_Error)(0);
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
#if defined(VGP_amd64_freebsd)
   // PJF - somewhat unfortunate uglificaton of the code, but the current code handles two
   // types of syscall with different register use. Mixing them up is not good.
   // I've avoided modifying the existing function (I could have added
   // a FreeBSD amd64-only flag to it for this purpose).
   if (sci->orig_args.klass == VG_FREEBSD_SYSCALL0 || sci->orig_args.klass == VG_FREEBSD_SYSCALL198) {
       getSyscallArgLayout_0_198( &layout );
    } else {
#endif

   getSyscallArgLayout( &layout );

#if defined(VGP_amd64_freebsd)
   }
#endif


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

   PRINT("SYSCALL[%d,%u](%s) ",
      VG_(getpid)(), tid, VG_SYSNUM_STRING(sysno));

   /* Do any pre-syscall actions */
   if (VG_(needs).syscall_wrapper) {
      UWord tmpv[8];
      tmpv[0] = sci->orig_args.arg1;
      tmpv[1] = sci->orig_args.arg2;
      tmpv[2] = sci->orig_args.arg3;
      tmpv[3] = sci->orig_args.arg4;
      tmpv[4] = sci->orig_args.arg5;
      tmpv[5] = sci->orig_args.arg6;
      tmpv[6] = sci->orig_args.arg7;
      tmpv[7] = sci->orig_args.arg8;
      VG_TDICT_CALL(tool_pre_syscall, tid, sysno,
                    &tmpv[0], sizeof(tmpv)/sizeof(tmpv[0]));
   }

   vg_assert(ent);
   vg_assert(ent->before);
   (ent->before)( tid,
                  &layout, 
                  &sci->args, &sci->status, &sci->flags );
   
   /* If needed, gdbserver will report syscall entry to GDB */
   VG_(gdbserver_report_syscall)(True, sysno, tid);

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
             || sci->status.what == SsComplete);
   vg_assert(sci->args.sysno == sci->orig_args.sysno);

   if (sci->status.what == SsComplete && !sr_isError(sci->status.sres)) {
      /* The pre-handler completed the syscall itself, declaring
         success. */
      if (sci->flags & SfNoWriteResult) {
         PRINT(" --> [pre-success] NoWriteResult");
      } else {
         PRINT(" --> [pre-success] %s", VG_(sr_as_string)(sci->status.sres));
      }                                      
      /* In this case the allowable flags are to ask for a signal-poll
         and/or a yield after the call.  Changing the args isn't
         allowed. */
      vg_assert(0 == (sci->flags 
                      & ~(SfPollAfter | SfYieldAfter | SfNoWriteResult)));
      vg_assert(eq_SyscallArgs(&sci->args, &sci->orig_args));
   }

   else
   if (sci->status.what == SsComplete && sr_isError(sci->status.sres)) {
      /* The pre-handler decided to fail syscall itself. */
      PRINT(" --> [pre-fail] %s", VG_(sr_as_string)(sci->status.sres));
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

      /* Check that the given flags are allowable: MayBlock, PollAfter
         and PostOnFail are ok. */
      vg_assert(0 == (sci->flags & ~(SfMayBlock | SfPostOnFail | SfPollAfter | SfKernelRestart)));

      if (sci->flags & SfMayBlock) {

         /* Syscall may block, so run it asynchronously */
         vki_sigset_t mask;

         PRINT(" --> [async] ... \n");

         mask = tst->sig_mask;
         VG_(sanitize_client_sigmask)(&mask);

         /* Gack.  More impedance matching.  Copy the possibly
            modified syscall args back into the guest state. */
         /* JRS 2009-Mar-16: if the syscall args are possibly modified,
            then this assertion is senseless:
              vg_assert(eq_SyscallArgs(&sci->args, &sci->orig_args));
            The case that exposed it was sys_posix_spawn on Darwin,
            which heavily modifies its arguments but then lets the call
            go through anyway, with SfToBlock set, hence we end up here. */
         putSyscallArgsIntoGuestState( &sci->args, &tst->arch.vex );

         /* SfNoWriteResult flag is invalid for blocking signals because
            do_syscall_for_client() directly modifies the guest state. */
         vg_assert(!(sci->flags & SfNoWriteResult));

         /* Drop the bigLock */
         VG_(release_BigLock)(tid, VgTs_WaitSys, "VG_(client_syscall)[async]");
         /* Urr.  We're now in a race against other threads trying to
            acquire the bigLock.  I guess that doesn't matter provided
            that do_syscall_for_client only touches thread-local
            state. */

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

         /* Darwin: do_syscall_for_client may not return if the 
            syscall was workq_ops(WQOPS_THREAD_RETURN) and the kernel 
            responded by starting the thread at wqthread_hijack(reuse=1)
            (to run another workqueue item). In that case, wqthread_hijack 
            calls ML_(wqthread_continue), which is similar to 
            VG_(fixup_guest_state_after_syscall_interrupted). */

         /* Reacquire the lock */
         VG_(acquire_BigLock)(tid, "VG_(client_syscall)[async]");

         /* Even more impedance matching.  Extract the syscall status
            from the guest state. */
         getSyscallStatusFromGuestState( &sci->status, &tst->arch.vex );
         vg_assert(sci->status.what == SsComplete);

         /* Be decorative, if required. */
         if (VG_(clo_trace_syscalls)) {
            PRINT("SYSCALL[%d,%u](%s) ... [async] --> %s",
                  VG_(getpid)(), tid, VG_SYSNUM_STRING(sysno),
                  VG_(sr_as_string)(sci->status.sres));
         }

      } else {

         /* run the syscall directly */
         /* The pre-handler may have modified the syscall args, but
            since we're passing values in ->args directly to the
            kernel, there's no point in flushing them back to the
            guest state.  Indeed doing so could be construed as
            incorrect. */
         SysRes sres 
            = VG_(do_syscall)(sysno, sci->args.arg1, sci->args.arg2, 
                                     sci->args.arg3, sci->args.arg4, 
                                     sci->args.arg5, sci->args.arg6,
                                     sci->args.arg7, sci->args.arg8 );
         sci->status = convert_SysRes_to_SyscallStatus(sres);

         /* Be decorative, if required. */
         if (VG_(clo_trace_syscalls)) {
           PRINT("[sync] --> %s", VG_(sr_as_string)(sci->status.sres));
         }
      }
   }

   vg_assert(sci->status.what == SsComplete);

   vg_assert(VG_(is_running_thread)(tid));

   /* Dump the syscall result back in the guest state.  This is
      a platform-specific action. */
   if (!(sci->flags & SfNoWriteResult))
      putSyscallStatusIntoGuestState( tid, &sci->status, &tst->arch.vex );

   /* If needed, gdbserver will report syscall return to GDB */
   VG_(gdbserver_report_syscall)(False, sysno, tid);

   /* Situation now:
      - the guest state is now correctly modified following the syscall
      - modified args, original args and syscall status are still
        available in the syscallInfo[] entry for this syscall.

      Now go on to do the post-syscall actions (read on down ..)
   */
   PRINT(" ");
   VG_(post_syscall)(tid);
   PRINT("\n");
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
   Darwin: there's a third way, ML_(wqthread_continue). 
*/
void VG_(post_syscall) (ThreadId tid)
{
   SyscallInfo*             sci;
   const SyscallTableEntry* ent;
   SyscallStatus            test_status;
   ThreadState*             tst;
   Word sysno;

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
      state.  At least in the normal case where we have actually
      previously written the result into the guest state. */
   vg_assert(sci->status.what == SsComplete);

   /* Get the system call number.  Because the pre-handler isn't
      allowed to mess with it, it should be the same for both the
      original and potentially-modified args. */
   vg_assert(sci->args.sysno == sci->orig_args.sysno);
   sysno = sci->args.sysno;

   getSyscallStatusFromGuestState( &test_status, &tst->arch.vex );
   if (!(sci->flags & SfNoWriteResult)) {
      vg_assert(eq_SyscallStatus( sysno, &sci->status, &test_status ));
   }
   /* Failure of the above assertion on Darwin can indicate a problem
      in the syscall wrappers that pre-fail or pre-succeed the
      syscall, by calling SET_STATUS_Success or SET_STATUS_Failure,
      when they really should call SET_STATUS_from_SysRes.  The former
      create a UNIX-class syscall result on Darwin, which may not be
      correct for the syscall; if that's the case then this assertion
      fires.  See PRE(thread_fast_set_cthread_self) for an example.  On
      non-Darwin platforms this assertion is should never fail, and this
      comment is completely irrelevant. */
   /* Ok, looks sane */

   /* pre: status == Complete (asserted above) */
   /* Consider either success or failure.  Now run the post handler if:
      - it exists, and
      - Success or (Failure and PostOnFail is set)
   */
   ent = get_syscall_entry(sysno);
   if (ent->after
       && ((!sr_isError(sci->status.sres))
           || (sr_isError(sci->status.sres)
               && (sci->flags & SfPostOnFail) ))) {

      (ent->after)( tid, &sci->args, &sci->status );
   }

   /* Because the post handler might have changed the status (eg, the
      post-handler for sys_open can change the result from success to
      failure if the kernel supplied a fd that it doesn't like), once
      again dump the syscall result back in the guest state.*/
   if (!(sci->flags & SfNoWriteResult))
      putSyscallStatusIntoGuestState( tid, &sci->status, &tst->arch.vex );

   /* Do any post-syscall actions required by the tool. */
   if (VG_(needs).syscall_wrapper) {
      UWord tmpv[8];
      tmpv[0] = sci->orig_args.arg1;
      tmpv[1] = sci->orig_args.arg2;
      tmpv[2] = sci->orig_args.arg3;
      tmpv[3] = sci->orig_args.arg4;
      tmpv[4] = sci->orig_args.arg5;
      tmpv[5] = sci->orig_args.arg6;
      tmpv[6] = sci->orig_args.arg7;
      tmpv[7] = sci->orig_args.arg8;
      VG_TDICT_CALL(tool_post_syscall, tid, 
                    sysno,
                    &tmpv[0], sizeof(tmpv)/sizeof(tmpv[0]),
                    sci->status.sres);
   }

   /* The syscall is done. */
   vg_assert(sci->status.what == SsComplete);
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
#if defined(VGO_linux) || defined(VGO_freebsd)
  extern const Addr ML_(blksys_setup);
  extern const Addr ML_(blksys_restart);
  extern const Addr ML_(blksys_complete);
  extern const Addr ML_(blksys_committed);
  extern const Addr ML_(blksys_finished);
#elif defined(VGO_darwin)
  /* Darwin requires extra uglyness */
  extern const Addr ML_(blksys_setup_MACH);
  extern const Addr ML_(blksys_restart_MACH);
  extern const Addr ML_(blksys_complete_MACH);
  extern const Addr ML_(blksys_committed_MACH);
  extern const Addr ML_(blksys_finished_MACH);
  extern const Addr ML_(blksys_setup_MDEP);
  extern const Addr ML_(blksys_restart_MDEP);
  extern const Addr ML_(blksys_complete_MDEP);
  extern const Addr ML_(blksys_committed_MDEP);
  extern const Addr ML_(blksys_finished_MDEP);
  extern const Addr ML_(blksys_setup_UNIX);
  extern const Addr ML_(blksys_restart_UNIX);
  extern const Addr ML_(blksys_complete_UNIX);
  extern const Addr ML_(blksys_committed_UNIX);
  extern const Addr ML_(blksys_finished_UNIX);
#elif defined(VGO_solaris)
  extern const Addr ML_(blksys_setup);
  extern const Addr ML_(blksys_complete);
  extern const Addr ML_(blksys_committed);
  extern const Addr ML_(blksys_finished);
  extern const Addr ML_(blksys_setup_DRET);
  extern const Addr ML_(blksys_complete_DRET);
  extern const Addr ML_(blksys_committed_DRET);
  extern const Addr ML_(blksys_finished_DRET);
#else
# error "Unknown OS"
#endif


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
                      "?! restarting over syscall at %#x %02x %02x\n",
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
                      "?! restarting over syscall at %#llx %02x %02x\n",
                      arch->vex.guest_RIP, p[0], p[1]); 

      vg_assert(p[0] == 0x0F && p[1] == 0x05);
   }

#elif defined(VGP_ppc32_linux) || defined(VGP_ppc64be_linux)
   arch->vex.guest_CIA -= 4;             // sizeof(ppc32 instr)

   /* Make sure our caller is actually sane, and we're really backing
      back over a syscall.

      sc == 44 00 00 02
   */
   {
      UChar *p = (UChar *)arch->vex.guest_CIA;

      if (p[0] != 0x44 || p[1] != 0x0 || p[2] != 0x0 || p[3] != 0x02)
         VG_(message)(Vg_DebugMsg,
                      "?! restarting over syscall at %#llx %02x %02x %02x %02x\n",
                      (ULong)arch->vex.guest_CIA, p[0], p[1], p[2], p[3]);

      vg_assert(p[0] == 0x44 && p[1] == 0x0 && p[2] == 0x0 && p[3] == 0x2);
   }

#elif defined(VGP_ppc64le_linux)
   arch->vex.guest_CIA -= 4;             // sizeof(ppc32 instr)

   /* Make sure our caller is actually sane, and we're really backing
      back over a syscall.

      sc == 44 00 00 02
      or
      scv == 44 00 00 01
   */
   {
      UChar *p = (UChar *)arch->vex.guest_CIA;

      if (!(p[3] == 0x44 && p[2] == 0x0 && p[1] == 0x0
            && (p[0] == 0x01 || p[0] == 0x02)))
         VG_(message)(Vg_DebugMsg,
                      "?! restarting over syscall at %#llx %02x %02x %02x %02x\n",
                      arch->vex.guest_CIA, p[3], p[2], p[1], p[0]);

      vg_assert(p[3] == 0x44 && p[2] == 0x0 && p[1] == 0x0
                && (p[0] == 0x1 || p[0] == 0x2));
   }

#elif defined(VGP_arm_linux)
   if (arch->vex.guest_R15T & 1) {
      // Thumb mode.  SVC is a encoded as
      //   1101 1111 imm8
      // where imm8 is the SVC number, and we only accept 0.
      arch->vex.guest_R15T -= 2;   // sizeof(thumb 16 bit insn)
      UChar* p     = (UChar*)(arch->vex.guest_R15T - 1);
      Bool   valid = p[0] == 0 && p[1] == 0xDF;
      if (!valid) {
         VG_(message)(Vg_DebugMsg,
                      "?! restarting over (Thumb) syscall that is not syscall "
                      "at %#x %02x %02x\n",
                      arch->vex.guest_R15T - 1, p[0], p[1]);
      }
      vg_assert(valid);
      // FIXME: NOTE, this really isn't right.  We need to back up
      // ITSTATE to what it was before the SVC instruction, but we
      // don't know what it was.  At least assert that it is now
      // zero, because if it is nonzero then it must also have
      // been nonzero for the SVC itself, which means it was
      // conditional.  Urk.
      vg_assert(arch->vex.guest_ITSTATE == 0);
   } else {
      // ARM mode.  SVC is encoded as 
      //   cond 1111 imm24
      // where imm24 is the SVC number, and we only accept 0.
      arch->vex.guest_R15T -= 4;   // sizeof(arm instr)
      UChar* p     = (UChar*)arch->vex.guest_R15T;
      Bool   valid = p[0] == 0 && p[1] == 0 && p[2] == 0
                     && (p[3] & 0xF) == 0xF;
      if (!valid) {
         VG_(message)(Vg_DebugMsg,
                      "?! restarting over (ARM) syscall that is not syscall "
                      "at %#x %02x %02x %02x %02x\n",
                      arch->vex.guest_R15T, p[0], p[1], p[2], p[3]);
      }
      vg_assert(valid);
   }

#elif defined(VGP_arm64_linux)
   arch->vex.guest_PC -= 4;             // sizeof(arm64 instr)

   /* Make sure our caller is actually sane, and we're really backing
      back over a syscall.

      svc #0 == d4 00 00 01
   */
   {
      UChar *p = (UChar *)arch->vex.guest_PC;

      if (p[0] != 0x01 || p[1] != 0x00 || p[2] != 0x00 || p[3] != 0xD4)
         VG_(message)(
            Vg_DebugMsg,
            "?! restarting over syscall at %#llx %02x %02x %02x %02x\n",
            arch->vex.guest_PC, p[0], p[1], p[2], p[3]
          );

      vg_assert(p[0] == 0x01 && p[1] == 0x00 && p[2] == 0x00 && p[3] == 0xD4);
   }

#elif defined(VGP_x86_freebsd)
   /* XXX: we support different syscall methods. */
   arch->vex.guest_EIP -= 2;             // sizeof(int $0x80)

   /* Make sure our caller is actually sane, and we're really backing
      back over a syscall.

      int $0x80 == CD 80
   */
   {
      UChar *p = (UChar *)arch->vex.guest_EIP;
 
      if (p[0] != 0xcd || p[1] != 0x80)
         VG_(message)(Vg_DebugMsg,
                      "?! restarting over syscall at %#x %02x %02x\n",
                      arch->vex.guest_EIP, p[0], p[1]);

      vg_assert(p[0] == 0xcd && p[1] == 0x80);
   }

#elif defined(VGP_amd64_freebsd)
   /* XXX: we support different syscall methods. */
   arch->vex.guest_RIP -= 2;             // sizeof(syscall)

   /* Make sure our caller is actually sane, and we're really backing
      back over a syscall.

      syscall == 0F 05 
   */
   {
      UChar *p = (UChar *)arch->vex.guest_RIP;
 
      if (p[0] != 0x0F || p[1] != 0x05)
         VG_(message)(Vg_DebugMsg,
                      "?! restarting over syscall at %#llx %02x %02x\n",
                      arch->vex.guest_RIP, p[0], p[1]);

      vg_assert(p[0] == 0x0F && p[1] == 0x05);
   }

#elif defined(VGP_x86_darwin)
   arch->vex.guest_EIP = arch->vex.guest_IP_AT_SYSCALL; 

   /* Make sure our caller is actually sane, and we're really backing
      back over a syscall.

      int $0x80 == CD 80  // Used to communicate with BSD syscalls
      int $0x81 == CD 81  // Used to communicate with Mach traps
      int $0x82 == CD 82  // Used to communicate with "thread" ?
      sysenter  == 0F 34  // Used to communicate with Unix syscalls
   */
   {
       UChar *p = (UChar *)arch->vex.guest_EIP;
       Bool  ok = (p[0] == 0xCD && p[1] == 0x80) 
                  || (p[0] == 0xCD && p[1] == 0x81)
                  || (p[0] == 0xCD && p[1] == 0x82)  
                  || (p[0] == 0x0F && p[1] == 0x34);
       if (!ok)
           VG_(message)(Vg_DebugMsg,
                        "?! restarting over syscall at %#x %02x %02x\n",
                        arch->vex.guest_EIP, p[0], p[1]);
       vg_assert(ok);
   }
   
#elif defined(VGP_amd64_darwin)
   arch->vex.guest_RIP = arch->vex.guest_IP_AT_SYSCALL;
    
   /* Make sure our caller is actually sane, and we're really backing
      back over a syscall.

      syscall   == 0F 05
   */
   {
       UChar *p = (UChar *)arch->vex.guest_RIP;
        
       Bool  ok = (p[0] == 0x0F && p[1] == 0x05);
       if (!ok)
           VG_(message)(Vg_DebugMsg,
                        "?! restarting over syscall at %#llx %02x %02x\n",
                        arch->vex.guest_RIP, p[0], p[1]);
       vg_assert(ok);
   }
   
#elif defined(VGP_s390x_linux)
   arch->vex.guest_IA -= 2;             // sizeof(syscall)

   /* Make sure our caller is actually sane, and we're really backing
      back over a syscall.

      syscall == 0A <num>
   */
   {
      UChar *p = (UChar *)arch->vex.guest_IA;
      if (p[0] != 0x0A)
         VG_(message)(Vg_DebugMsg,
                      "?! restarting over syscall at %#llx %02x %02x\n",
                      arch->vex.guest_IA, p[0], p[1]);

      vg_assert(p[0] == 0x0A);
   }

#elif defined(VGP_mips32_linux) || defined(VGP_mips64_linux)

   arch->vex.guest_PC -= 4;             // sizeof(mips instr)

   /* Make sure our caller is actually sane, and we're really backing
      back over a syscall.
      
      syscall == 00 00 00 0C 
      big endian
      syscall == 0C 00 00 00
   */
   {
      UChar *p = (UChar *)(Addr)(arch->vex.guest_PC);
#     if defined (VG_LITTLEENDIAN)
      if (p[0] != 0x0c || p[1] != 0x00 || p[2] != 0x00 || p[3] != 0x00)
         VG_(message)(Vg_DebugMsg,
                      "?! restarting over syscall at %#llx %02x %02x %02x %02x\n",
                      (ULong)arch->vex.guest_PC, p[0], p[1], p[2], p[3]);

      vg_assert(p[0] == 0x0c && p[1] == 0x00 && p[2] == 0x00 && p[3] == 0x00);
#     elif defined (VG_BIGENDIAN)
      if (p[0] != 0x00 || p[1] != 0x00 || p[2] != 0x00 || p[3] != 0x0c)
         VG_(message)(Vg_DebugMsg,
                      "?! restarting over syscall at %#llx %02x %02x %02x %02x\n",
                      (ULong)arch->vex.guest_PC, p[0], p[1], p[2], p[3]);

      vg_assert(p[0] == 0x00 && p[1] == 0x00 && p[2] == 0x00 && p[3] == 0x0c);
#     else
#        error "Unknown endianness"
#     endif
   }

#elif defined(VGP_nanomips_linux)
   {
      /* Make sure our caller is actually sane, and we're really backing
         back over a syscall.
      */
      arch->vex.guest_PC -= 2;
      /* PC has to be 16-bit aligned. */
      vg_assert((arch->vex.guest_PC & 1) == 0);

      UShort *p = ASSUME_ALIGNED(UShort *, (Addr)(arch->vex.guest_PC));

      if (((*p) & 0xFFFD) != 0x1008) {
         if (((*(p - 1)) & 0xFFFD) != 0x0008) {
            VG_(message)(Vg_DebugMsg,
                         "?! restarting over syscall at %#x %08lx\n",
                         arch->vex.guest_PC, (UWord)(*p));
            vg_assert(0);
         }
         arch->vex.guest_PC -= 2;
      }
   }
#elif defined(VGP_x86_solaris)
   arch->vex.guest_EIP -= 2;   // sizeof(int $0x91) or sizeof(syscall)

   /* Make sure our caller is actually sane, and we're really backing
      back over a syscall.

      int $0x91 == CD 91
      syscall   == 0F 05
      sysenter  == 0F 34

      Handle also other syscall instructions because we also handle them in
      the scheduler.
      int $0x80 == CD 80
      int $0x81 == CD 81
      int $0x82 == CD 82
   */
   {
      UChar *p = (UChar *)arch->vex.guest_EIP;

      Bool  ok = (p[0] == 0xCD && p[1] == 0x91)
                  || (p[0] == 0x0F && p[1] == 0x05)
                  || (p[0] == 0x0F && p[1] == 0x34)
                  || (p[0] == 0xCD && p[1] == 0x80)
                  || (p[0] == 0xCD && p[1] == 0x81)
                  || (p[0] == 0xCD && p[1] == 0x82);
      if (!ok)
         VG_(message)(Vg_DebugMsg,
                      "?! restarting over syscall at %#x %02x %02x\n",
                      arch->vex.guest_EIP, p[0], p[1]);
      vg_assert(ok);
   }

#elif defined(VGP_amd64_solaris)
   arch->vex.guest_RIP -= 2;   // sizeof(syscall)

   /* Make sure our caller is actually sane, and we're really backing
      back over a syscall.

      syscall   == 0F 05
   */
   {
      UChar *p = (UChar *)arch->vex.guest_RIP;

      Bool  ok = (p[0] == 0x0F && p[1] == 0x05);
      if (!ok)
         VG_(message)(Vg_DebugMsg,
                      "?! restarting over syscall at %#llx %02x %02x\n",
                      arch->vex.guest_RIP, p[0], p[1]);
      vg_assert(ok);
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
     3. save result to guest state (EAX, RAX, R3+CR0.SO, R0, V0)
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
                                                  SysRes   sres,
                                                  Bool     restart,
                                                  struct vki_ucontext *uc)
{
   /* Note that we don't know the syscall number here, since (1) in
      general there's no reliable way to get hold of it short of
      stashing it in the guest state before the syscall, and (2) in
      any case we don't need to know it for the actions done by this
      routine.

      Furthermore, 'sres' is only used in the case where the syscall
      is complete, but the result has not been committed to the guest
      state yet.  In any other situation it will be meaningless and
      therefore ignored. */

   ThreadState*     tst;
   SyscallStatus    canonical;
   ThreadArchState* th_regs;
   SyscallInfo*     sci;

   /* Compute some Booleans indicating which range we're in. */
   Bool outside_range, 
        in_setup_to_restart,      // [1,2) in the .S files
        at_restart,               // [2]   in the .S files
        in_complete_to_committed, // [3,4) in the .S files
        in_committed_to_finished; // [4,5) in the .S files

   if (VG_(clo_trace_signals))
      VG_(message)( Vg_DebugMsg,
                    "interrupted_syscall: tid=%u, ip=%#lx, "
                    "restart=%s, sres.isErr=%s, sres.val=%" FMT_REGWORD "u\n",
                    tid,
                    ip,
                    restart ? "True" : "False",
                    sr_isError(sres) ? "True" : "False",
                    sr_isError(sres) ? (RegWord)sr_Err(sres) :
                                       (RegWord)sr_Res(sres));

   vg_assert(VG_(is_valid_tid)(tid));
   vg_assert(tid >= 1 && tid < VG_N_THREADS);
   vg_assert(VG_(is_running_thread)(tid));

   tst     = VG_(get_ThreadState)(tid);
   th_regs = &tst->arch;
   sci     = & syscallInfo[tid];

#  if defined(VGO_linux) || defined(VGO_freebsd)
   outside_range
      = ip < ML_(blksys_setup) || ip >= ML_(blksys_finished);
   in_setup_to_restart
      = ip >= ML_(blksys_setup) && ip < ML_(blksys_restart); 

#if  defined(VGP_ppc64le_linux)
   /* Starting with ISA 3.0, Power supports two system call instructions sc
      and scv.  The code in file syscall-ppc64[be|le]-linux.S uses an input
      to call the requested system call.  The definitions for blksys_restart
      and blksys_complete must account for being at either of the two system
      calls and account for the branch to lable 3 if the sc instruction was
      called.  at_restart is true if the ip is at either system call
      instruction.  in_complete_to_committed is true if the ip is between
      blksys_complete and blksys_committed OR at the branch after the sc
      instruction.  The scv instruction is currently only supported on LE. */
   at_restart
      = (ip == ML_(blksys_restart)) || ((ip-8) == ML_(blksys_restart));
   in_complete_to_committed
      = (ip >= ML_(blksys_complete) && ip < ML_(blksys_committed)) ||
      ((ip+8) == ML_(blksys_complete));
#else
   at_restart
      = ip == ML_(blksys_restart); 
   in_complete_to_committed
      = ip >= ML_(blksys_complete) && ip < ML_(blksys_committed); 
#endif

   in_committed_to_finished
      = ip >= ML_(blksys_committed) && ip < ML_(blksys_finished);
#  elif defined(VGO_darwin)
   outside_range
      =  (ip < ML_(blksys_setup_MACH) || ip >= ML_(blksys_finished_MACH))
      && (ip < ML_(blksys_setup_MDEP) || ip >= ML_(blksys_finished_MDEP))
      && (ip < ML_(blksys_setup_UNIX) || ip >= ML_(blksys_finished_UNIX));
   in_setup_to_restart
      =  (ip >= ML_(blksys_setup_MACH) && ip < ML_(blksys_restart_MACH))
      || (ip >= ML_(blksys_setup_MDEP) && ip < ML_(blksys_restart_MDEP))
      || (ip >= ML_(blksys_setup_UNIX) && ip < ML_(blksys_restart_UNIX));
   at_restart
      =  (ip == ML_(blksys_restart_MACH))
      || (ip == ML_(blksys_restart_MDEP))
      || (ip == ML_(blksys_restart_UNIX));
   in_complete_to_committed
      =  (ip >= ML_(blksys_complete_MACH) && ip < ML_(blksys_committed_MACH))
      || (ip >= ML_(blksys_complete_MDEP) && ip < ML_(blksys_committed_MDEP))
      || (ip >= ML_(blksys_complete_UNIX) && ip < ML_(blksys_committed_UNIX));
   in_committed_to_finished
      =  (ip >= ML_(blksys_committed_MACH) && ip < ML_(blksys_finished_MACH))
      || (ip >= ML_(blksys_committed_MDEP) && ip < ML_(blksys_finished_MDEP))
      || (ip >= ML_(blksys_committed_UNIX) && ip < ML_(blksys_finished_UNIX));
   /* Wasn't that just So Much Fun?  Does your head hurt yet?  Mine does. */
#  elif defined(VGO_solaris)
   /* The solaris port is never outside the range. */
   outside_range = False;
   /* The Solaris kernel never restarts syscalls directly! */
   at_restart = False;
   if (tst->os_state.in_door_return) {
      vg_assert(ip >= ML_(blksys_setup_DRET)
                && ip < ML_(blksys_finished_DRET));

      in_setup_to_restart
         = ip >= ML_(blksys_setup_DRET) && ip < ML_(blksys_complete_DRET);
      in_complete_to_committed
         = ip >= ML_(blksys_complete_DRET) && ip < ML_(blksys_committed_DRET);
      in_committed_to_finished
         = ip >= ML_(blksys_committed_DRET) && ip < ML_(blksys_finished_DRET);
   }
   else {
      vg_assert(ip >= ML_(blksys_setup) && ip < ML_(blksys_finished));

      in_setup_to_restart
         = ip >= ML_(blksys_setup) && ip < ML_(blksys_complete);
      in_complete_to_committed
         = ip >= ML_(blksys_complete) && ip < ML_(blksys_committed);
      in_committed_to_finished
         = ip >= ML_(blksys_committed) && ip < ML_(blksys_finished);
   }
#  else
#    error "Unknown OS"
#  endif

#if defined(VGO_freebsd) || defined(VGO_darwin)
  if (outside_range)
  {
     /* This is not guaranteed to work since the compiler / link editor
        could lay out the binary functions in a different order to
        the source file. However, it seems to work. */

#if defined (VGA_amd64)

     vg_assert((Addr)_______VVVVVVVV_after_GuestAMD64_put_rflag_c_VVVVVVVV_______ >
               (Addr)LibVEX_GuestAMD64_put_rflag_c );

     vg_assert(addr________VVVVVVVV_amd64g_calculate_rflags_all_WRK_VVVVVVVV_______ >
               addr_amd64g_calculate_rflags_all_WRK);

     if ((ip >= (Addr)LibVEX_GuestAMD64_put_rflag_c &&
          ip <  (Addr)_______VVVVVVVV_after_GuestAMD64_put_rflag_c_VVVVVVVV_______) ||
         (ip >= addr_amd64g_calculate_rflags_all_WRK &&
         ip < addr________VVVVVVVV_amd64g_calculate_rflags_all_WRK_VVVVVVVV_______))
#else

     vg_assert((Addr)_______VVVVVVVV_after_LibVEX_GuestX86_put_eflag_c_VVVVVVVV_______ >
               (Addr)LibVEX_GuestX86_put_eflag_c);
 
     vg_assert(addr________VVVVVVVV_x86g_calculate_eflags_all_WRK_VVVVVVVV_______>
              addr_x86g_calculate_eflags_all_WRK);

     if ((ip >= (Addr)LibVEX_GuestX86_put_eflag_c &&
         ip <  (Addr)_______VVVVVVVV_after_LibVEX_GuestX86_put_eflag_c_VVVVVVVV_______) ||
         (ip >= addr_x86g_calculate_eflags_all_WRK &&
          ip < addr________VVVVVVVV_x86g_calculate_eflags_all_WRK_VVVVVVVV_______))
#endif
     {
        outside_range = False;
        in_complete_to_committed = True;
     }
  }
#endif


   /* Figure out what the state of the syscall was by examining the
      (real) IP at the time of the signal, and act accordingly. */
   if (outside_range) {
      if (VG_(clo_trace_signals))
         VG_(message)( Vg_DebugMsg,
                       "  not in syscall at all: hmm, very suspicious\n" );
      /* Looks like we weren't in a syscall at all.  Hmm. */
      vg_assert(sci->status.what != SsIdle);
      return;
   }

   /* We should not be here unless this thread had first started up
      the machinery for a syscall by calling VG_(client_syscall).
      Hence: */
   vg_assert(sci->status.what != SsIdle);

   /* now, do one of four fixup actions, depending on where the IP has
      got to. */

   if (in_setup_to_restart) {
      /* syscall hasn't even started; go around again */
      if (VG_(clo_trace_signals))
         VG_(message)( Vg_DebugMsg, "  not started: restarting\n");
      vg_assert(sci->status.what == SsHandToKernel);
      ML_(fixup_guest_state_to_restart_syscall)(th_regs);
   } 

   else 
   if (at_restart) {
#     if defined(VGO_solaris)
      /* We should never hit this branch on Solaris, see the comment above. */
      vg_assert(0);
#     endif

      /* We're either about to run the syscall, or it was interrupted
         and the kernel restarted it.  Restart if asked, otherwise
         EINTR it. */
      if (restart) {
         if (VG_(clo_trace_signals))
            VG_(message)( Vg_DebugMsg, "  at syscall instr: restarting\n");
         ML_(fixup_guest_state_to_restart_syscall)(th_regs);
      } else {
         if (VG_(clo_trace_signals))
            VG_(message)( Vg_DebugMsg, "  at syscall instr: returning EINTR\n");
         canonical = convert_SysRes_to_SyscallStatus( 
                        VG_(mk_SysRes_Error)( VKI_EINTR ) 
                     );
         if (!(sci->flags & SfNoWriteResult))
            putSyscallStatusIntoGuestState( tid, &canonical, &th_regs->vex );
         sci->status = canonical;
         VG_(post_syscall)(tid);
      }
   }

   else 
   if (in_complete_to_committed) {
      /* Syscall complete, but result hasn't been written back yet.
         Write the SysRes we were supplied with back to the guest
         state. */
      if (VG_(clo_trace_signals))
         VG_(message)( Vg_DebugMsg,
                       "  completed, but uncommitted: committing\n");
      canonical = convert_SysRes_to_SyscallStatus( sres );
      vg_assert(!(sci->flags & SfNoWriteResult));
      putSyscallStatusIntoGuestState( tid, &canonical, &th_regs->vex );
#     if defined(VGO_solaris)
      if (tst->os_state.in_door_return) {
#        if defined(VGP_x86_solaris)
         /* Registers %esp and %ebp were also modified by the syscall. */
         tst->arch.vex.guest_ESP = uc->uc_mcontext.gregs[VKI_UESP];
         tst->arch.vex.guest_EBP = uc->uc_mcontext.gregs[VKI_EBP];
#        elif defined(VGP_amd64_solaris)
         tst->arch.vex.guest_RSP = uc->uc_mcontext.gregs[VKI_REG_RSP];
         tst->arch.vex.guest_RBP = uc->uc_mcontext.gregs[VKI_REG_RBP];
#        endif
      }
#     endif
      sci->status = canonical;
      VG_(post_syscall)(tid);
   } 

   else  
   if (in_committed_to_finished) {
      /* Result committed, but the signal mask has not been restored;
         we expect our caller (the signal handler) will have fixed
         this up. */
/* XXX: needed? */
#if defined(VGP_x86_freebsd)
      /* On FreeBSD, the success/fail status is returned to the caller
        and still has to be fixed up here. */
      if (!(sci->flags & SfNoWriteResult)) {
        if (sr_isError(sres))
           LibVEX_GuestX86_put_eflag_c(1, &th_regs->vex);
        else
           LibVEX_GuestX86_put_eflag_c(0, &th_regs->vex);
      }
#elif defined(VGP_amd64_freebsd)
      if (!(sci->flags & SfNoWriteResult)) {
        if (sr_isError(sres))
           LibVEX_GuestAMD64_put_rflag_c(1, &th_regs->vex);
        else
           LibVEX_GuestAMD64_put_rflag_c(0, &th_regs->vex);
      }
#endif
      if (VG_(clo_trace_signals))
         VG_(message)( Vg_DebugMsg,
                       "  completed and committed: nothing to do\n");
#     if defined(VGP_x86_solaris)
      /* The %eax and %edx values are committed but the carry flag is still
         uncommitted.  Save it now. */
      LibVEX_GuestX86_put_eflag_c(sr_isError(sres), &th_regs->vex);
#     elif defined(VGP_amd64_solaris)
      LibVEX_GuestAMD64_put_rflag_c(sr_isError(sres), &th_regs->vex);
#     endif
      getSyscallStatusFromGuestState( &sci->status, &th_regs->vex );
      vg_assert(sci->status.what == SsComplete);
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


#if defined(VGO_solaris)
/* Returns True if ip is inside a fixable syscall code in syscall-*-*.S.  This
   function can be called by a 'non-running' thread! */
Bool VG_(is_ip_in_blocking_syscall)(ThreadId tid, Addr ip)
{
   ThreadState *tst = VG_(get_ThreadState)(tid);

   if (tst->os_state.in_door_return)
      return ip >= ML_(blksys_setup_DRET) && ip < ML_(blksys_finished_DRET);
   else
      return ip >= ML_(blksys_setup) && ip < ML_(blksys_finished);
}
#endif


#if defined(VGO_darwin)
// Clean up after workq_ops(WQOPS_THREAD_RETURN) jumped to wqthread_hijack. 
// This is similar to VG_(fixup_guest_state_after_syscall_interrupted).
// This longjmps back to the scheduler.
void ML_(wqthread_continue_NORETURN)(ThreadId tid)
{
   ThreadState*     tst;
   SyscallInfo*     sci;

   VG_(acquire_BigLock)(tid, "wqthread_continue_NORETURN");

   PRINT("SYSCALL[%d,%u](%s) workq_ops() starting new workqueue item\n", 
         VG_(getpid)(), tid, VG_SYSNUM_STRING(__NR_workq_ops));

   vg_assert(VG_(is_valid_tid)(tid));
   vg_assert(tid >= 1 && tid < VG_N_THREADS);
   vg_assert(VG_(is_running_thread)(tid));

   tst     = VG_(get_ThreadState)(tid);
   sci     = & syscallInfo[tid];
   vg_assert(sci->status.what != SsIdle);
   vg_assert(tst->os_state.wq_jmpbuf_valid);  // check this BEFORE post_syscall

   // Pretend the syscall completed normally, but don't touch the thread state.
   sci->status = convert_SysRes_to_SyscallStatus( VG_(mk_SysRes_Success)(0) );
   sci->flags |= SfNoWriteResult;
   VG_(post_syscall)(tid);

   ML_(sync_mappings)("in", "ML_(wqthread_continue_NORETURN)", 0);

   sci->status.what = SsIdle;

   vg_assert(tst->sched_jmpbuf_valid);
   VG_MINIMAL_LONGJMP(tst->sched_jmpbuf);

   /* NOTREACHED */
   vg_assert(0);
}
#endif


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
