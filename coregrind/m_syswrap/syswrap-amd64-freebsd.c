
/*--------------------------------------------------------------------*/
/*--- Platform-specific syscalls stuff.    syswrap-amd64-freebsd.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2005 Nicholas Nethercote
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

#if defined(VGP_amd64_freebsd)

#include "pub_core_basics.h"
#include "pub_core_vki.h"
#include "pub_core_vkiscnums.h"
#include "pub_core_threadstate.h"
#include "pub_core_aspacemgr.h"
#include "pub_core_debuglog.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcprint.h"
#include "pub_core_libcproc.h"
#include "pub_core_libcsignal.h"
#include "pub_core_machine.h"
#include "pub_core_options.h"
#include "pub_core_scheduler.h"
#include "pub_core_sigframe.h"
#include "pub_core_signals.h"
#include "pub_core_syscall.h"
#include "pub_core_syswrap.h"
#include "pub_core_tooliface.h"
#include "pub_core_stacks.h"        // VG_(register_stack)

#include "priv_types_n_macros.h"
#include "priv_syswrap-generic.h"    /* for decls of generic wrappers */
#include "priv_syswrap-freebsd.h"    /* for decls of freebsd-ish wrappers */
#include "priv_syswrap-main.h"

/* ---------------------------------------------------------------------
   clone() handling
   ------------------------------------------------------------------ */

/* Call f(arg1), but first switch stacks, using 'stack' as the new
   stack, and use 'retaddr' as f's return-to address.  Also, clear all
   the integer registers before entering f. */
__attribute__((noreturn))
void ML_(call_on_new_stack_0_1) ( Addr stack,
                                  Addr retaddr,
                                  void (*f)(Word),
                                  Word arg1 );
// %rdi == stack
// %rsi == retaddr
// %rdx == f
// %rcx == arg1
__asm__(
   ".text\n"
   ".globl vgModuleLocal_call_on_new_stack_0_1\n"
   "vgModuleLocal_call_on_new_stack_0_1:\n"
   "   movq   %rdi, %rsp\n"   // set stack
   "   pushq  %rsi\n"         // retaddr to stack
   "   pushq  %rdx\n"         // f to stack
   "   pushq  %rcx\n"         // arg1 to stack
   "   movq $0, %rax\n"       // zero all GP regs
   "   movq $0, %rbx\n"
   "   movq $0, %rcx\n"
   "   movq $0, %rdx\n"
   "   movq $0, %rsi\n"
   "   movq $0, %rdi\n"
   "   movq $0, %rbp\n"
   "   movq $0, %r8\n"
   "   movq $0, %r9\n"
   "   movq $0, %r10\n"
   "   movq $0, %r11\n"
   "   movq $0, %r12\n"
   "   movq $0, %r13\n"
   "   movq $0, %r14\n"
   "   movq $0, %r15\n"
   "   popq   %rdi\n"         // arg1 to correct arg reg
   "   ret\n"                 // jump to f
   "   ud2\n"                 // should never get here
   ".previous\n"
);


/* ---------------------------------------------------------------------
   More thread stuff
   ------------------------------------------------------------------ */

void VG_(cleanup_thread) ( ThreadArchState *arch )
{
}

/* ---------------------------------------------------------------------
   PRE/POST wrappers for amd64/FreeBSD-specific syscalls
   ------------------------------------------------------------------ */

#define PRE(name)       DEFN_PRE_TEMPLATE(freebsd, name)
#define POST(name)      DEFN_POST_TEMPLATE(freebsd, name)

// SYS_sysarch 165
// int sysarch(int number, void *args);
PRE(sys_sysarch)
{
   ThreadState *tst;
   void **p;

   PRINT("sys_sysarch ( %" FMT_REGWORD "u, %#" FMT_REGWORD "x )", ARG1, ARG2);
   PRE_REG_READ2(int, "sysarch", int, number, void *, args);
   switch (ARG1) {
   case VKI_AMD64_SET_FSBASE:
      PRINT("sys_amd64_set_fsbase ( %#lx )", ARG2);

      if (ML_(safe_to_deref)((void**)ARG2, sizeof(void*))) {
         /* On FreeBSD, the syscall loads the %gs selector for us, so do it now. */
         tst = VG_(get_ThreadState)(tid);
         p = (void**)ARG2;
         tst->arch.vex.guest_FS_CONST = (UWord)*p;
         /* "do" the syscall ourselves; the kernel never sees it */
         SET_STATUS_Success2((ULong)*p, tst->arch.vex.guest_RDX );
      } else {
         // ????
         SET_STATUS_Failure( VKI_EINVAL );
      }

      break;
   case VKI_AMD64_GET_FSBASE:
      PRINT("sys_amd64_get_fsbase ( %#lx )", ARG2);
      PRE_MEM_WRITE( "amd64_get_fsbase(basep)", ARG2, sizeof(void *) );
      if (ML_(safe_to_deref)((void**)ARG2, sizeof(void*))) {
         /* "do" the syscall ourselves; the kernel never sees it */
         tst = VG_(get_ThreadState)(tid);
         SET_STATUS_Success2( tst->arch.vex.guest_FS_CONST, tst->arch.vex.guest_RDX );
      } else {
         SET_STATUS_Failure( VKI_EINVAL );
      }
      break;
   case VKI_AMD64_GET_XFPUSTATE:
      PRINT("sys_amd64_get_xfpustate ( %#lx )", ARG2);
      PRE_MEM_WRITE( "amd64_get_xfpustate(basep)", ARG2, sizeof(void *) );

      // @todo PJF need a test for this
      // I think that it will fail in the POST if ARG2 is not a valid pointer

      /* "do" the syscall ourselves; the kernel never sees it */
      tst = VG_(get_ThreadState)(tid);
      SET_STATUS_Success2( tst->arch.vex.guest_FPTAG[0], tst->arch.vex.guest_FPTAG[0] );
      break;
   default:
      VG_(message) (Vg_UserMsg, "unhandled sysarch cmd %lu", ARG1);
      VG_(unimplemented) ("unhandled sysarch cmd");
      break;
   }
}

POST(sys_sysarch)
{
   switch (ARG1) {
   case VKI_AMD64_SET_FSBASE:
      break;
   case VKI_AMD64_GET_FSBASE:
   case VKI_AMD64_GET_XFPUSTATE:
      POST_MEM_WRITE( ARG2, sizeof(void *) );
      break;
   default:
      break;
   }
}

// freebsd6_pread 173
#if (FREEBSD_VERS <= FREEBSD_10)
PRE(sys_freebsd6_pread)
{
   *flags |= SfMayBlock;
   PRINT("sys_freebsd6_pread ( %" FMT_REGWORD "u, %#" FMT_REGWORD "x, %lu, %" FMT_REGWORD "u, %" FMT_REGWORD "u )", ARG1, ARG2, ARG3, ARG4, ARG5);
   PRE_REG_READ5(ssize_t, "read",
                 unsigned int, fd, char *, buf, vki_size_t, count,
                 int, pad, unsigned long, off);

   if (!ML_(fd_allowed)(ARG1, "freebsd6_pread", tid, False))
      SET_STATUS_Failure( VKI_EBADF );
   else
      PRE_MEM_WRITE( "freebsd6_pread(buf)", ARG2, ARG3 );
}

POST(sys_freebsd6_pread)
{
   vg_assert(SUCCESS);
   POST_MEM_WRITE( ARG2, RES );
}
#endif

// freebsd6_pwrite 174
#if (FREEBSD_VERS <= FREEBSD_10)
PRE(sys_freebsd6_pwrite)
{
   Bool ok;
   *flags |= SfMayBlock;
   PRINT("sys_freebsd6_pwrite ( %" FMT_REGWORD "d, %#" FMT_REGWORD "x, %" FMT_REGWORD "u, %" FMT_REGWORD "u, %" FMT_REGWORD "u )", SARG1, ARG2, ARG3, ARG4, ARG5);
   PRE_REG_READ5(ssize_t, "write",
                 unsigned int, fd, const char *, buf, vki_size_t, count,
                 int, pad, unsigned long, off);
   /* check to see if it is allowed.  If not, try for an exemption from
      --sim-hints=enable-outer (used for self hosting). */
   ok = ML_(fd_allowed)(ARG1, "freebsd6_pwrite", tid, False);
   if (!ok && ARG1 == 2/*stderr*/
         && SimHintiS(SimHint_enable_outer, VG_(clo_sim_hints)))
      ok = True;
   if (!ok)
      SET_STATUS_Failure( VKI_EBADF );
   else
      PRE_MEM_READ( "freebsd6_pwrite(buf)", ARG2, ARG3 );
}
#endif

// SYS_freebsd6_mmap 197
#if (FREEBSD_VERS <= FREEBSD_10)
/* This is here because on x86 the off_t is passed in 2 regs. Don't ask about pad.  */

/* caddr_t mmap(caddr_t addr, size_t len, int prot, int flags, int fd, int pad, off_t pos); */
/*              ARG1           ARG2       ARG3      ARG4       ARG5    ARG6     ARG7 */

PRE(sys_freebsd6_mmap)
{
   SysRes r;

   PRINT("sys_mmap ( %#" FMT_REGWORD "x, %" FMT_REGWORD "u, %" FMT_REGWORD "u, %" FMT_REGWORD "u, %" FMT_REGWORD "u, pad%" FMT_REGWORD "u, 0x%" FMT_REGWORD "x)",
         ARG1, (UWord)ARG2, ARG3, ARG4, ARG5, ARG6, ARG7 );
   PRE_REG_READ7(long, "mmap",
                 char *, addr, unsigned long, len, int, prot,  int, flags,
                 int, fd,  int, pad, unsigned long, pos);

   r = ML_(generic_PRE_sys_mmap)( tid, ARG1, ARG2, ARG3, ARG4, ARG5, ARG7 );
   SET_STATUS_from_SysRes(r);
}
#endif

// freebsd6_lseek 199
#if (FREEBSD_VERS <= FREEBSD_10)
PRE(sys_freebsd6_lseek)
{
   PRINT("sys_freebsd6_lseek ( %" FMT_REGWORD "u, 0x%" FMT_REGWORD "x, %#" FMT_REGWORD "x, %" FMT_REGWORD "u )", ARG1,ARG2,ARG3,ARG4);
   PRE_REG_READ4(long, "lseek",
                 unsigned int, fd, int, pad, unsigned long, offset,
                 unsigned int, whence);
}
#endif

// freebsd6_truncate 200
#if (FREEBSD_VERS <= FREEBSD_10)
PRE(sys_freebsd6_truncate)
{
   *flags |= SfMayBlock;
   PRINT("sys_truncate ( %#" FMT_REGWORD "x(%s), %" FMT_REGWORD "u )", ARG1,(char *)ARG1,ARG3);
   PRE_REG_READ3(long, "truncate",
                 const char *, path, int, pad, unsigned int, length);
   PRE_MEM_RASCIIZ( "truncate(path)", ARG1 );
}
#endif

// freebsd6_ftruncate 201
#if (FREEBSD_VERS <= FREEBSD_10)
PRE(sys_freebsd6_ftruncate)
{
   *flags |= SfMayBlock;
   PRINT("sys_ftruncate ( %" FMT_REGWORD "u, %" FMT_REGWORD "u )", ARG1,ARG3);
   PRE_REG_READ3(long, "ftruncate", unsigned int, fd, int, pad,
                 unsigned int, length);
}
#endif

// SYS_clock_getcpuclockid2   247
// no manpage for this, from syscalls.master
// int clock_getcpuclockid2(id_t id, int which, _Out_ clockid_t *clock_id);
PRE(sys_clock_getcpuclockid2)
{
   PRINT("sys_clock_getcpuclockid2( %" FMT_REGWORD "d, %" FMT_REGWORD "d, %#" FMT_REGWORD "x )",
         SARG1,SARG2,ARG3);
   PRE_REG_READ3(int, "clock_getcpuclockid2",
                 id_t, id, int, len, clockid_t *, clock_id);
   PRE_MEM_WRITE("clock_getcpuclockid2(clock_id)", ARG3, sizeof(vki_clockid_t));
}

// SYS_rfork 251
// pid_t rfork(int flags);
PRE(sys_rfork)
{
   PRINT("sys_rfork ( %#" FMT_REGWORD "x )", ARG1 );
   PRE_REG_READ1(pid_t, "rfork", int, flags);

   VG_(message)(Vg_UserMsg, "warning: rfork() not implemented\n");

   if ((UInt)ARG1 == VKI_RFSPAWN) {
      // posix_spawn uses RFSPAWN and it will fall back to vfork
      // if it sees EINVAL
      SET_STATUS_Failure(VKI_EINVAL);
   } else {
      SET_STATUS_Failure(VKI_ENOSYS);
   }
}

// SYS_preadv  289
// ssize_t preadv(int fd, const struct iovec *iov, int iovcnt, off_t offset);
PRE(sys_preadv)
{
   Int i;
   struct vki_iovec * vec;
   char buf[sizeof("preadv(iov[])") + 11];
   *flags |= SfMayBlock;
   PRINT("sys_preadv ( %" FMT_REGWORD "d, %#" FMT_REGWORD "x, %"
         FMT_REGWORD "d, %" FMT_REGWORD "d )", SARG1, ARG2, SARG3, SARG4);
   PRE_REG_READ4(ssize_t, "preadv",
                 int, fd, const struct iovec *, iov,
                 int, iovcnt, vki_off_t, offset);
   if (!ML_(fd_allowed)(ARG1, "preadv", tid, False)) {
      SET_STATUS_Failure( VKI_EBADF );
   } else {
      if ((Int)ARG3 > 0) {
         PRE_MEM_READ( "preadv(iov)", ARG2, ARG3 * sizeof(struct vki_iovec) );
      }

      if (ML_(safe_to_deref)((struct vki_iovec *)ARG2, ARG3 * sizeof(struct vki_iovec))) {
         vec = (struct vki_iovec *)(Addr)ARG2;
         for (i = 0; i < (Int)ARG3; i++) {
            VG_(sprintf)(buf, "preadv(iov[%d])", i);
            PRE_MEM_WRITE(buf, (Addr)vec[i].iov_base, vec[i].iov_len);
         }
      }
   }
}

POST(sys_preadv)
{
   vg_assert(SUCCESS);
   if (RES > 0) {
      Int i;
      struct vki_iovec * vec = (struct vki_iovec *)(Addr)ARG2;
      Int remains = RES;

      /* RES holds the number of bytes read. */
      for (i = 0; i < (Int)ARG3; i++) {
         Int nReadThisBuf = vec[i].iov_len;
         if (nReadThisBuf > remains) {
            nReadThisBuf = remains;
         }
         POST_MEM_WRITE( (Addr)vec[i].iov_base, nReadThisBuf );
         remains -= nReadThisBuf;
         if (remains < 0) {
            VG_(core_panic)("preadv: remains < 0");
         }
      }
   }
}

// SYS_pwritev 290
// ssize_t pwritev(int fd, const struct iovec *iov, int iovcnt, off_t offset);
PRE(sys_pwritev)
{
   Int i;
   struct vki_iovec * vec;
   char buf[sizeof("pwritev(iov[])") + 11];
   *flags |= SfMayBlock;
   PRINT("sys_pwritev ( %" FMT_REGWORD "d, %#" FMT_REGWORD "x, %"
         FMT_REGWORD "d, %" FMT_REGWORD "d )", SARG1, ARG2, SARG3, SARG4);

   PRE_REG_READ4(ssize_t, "pwritev",
                 int, fd, const struct iovec *, iov,
                 int, iovcnt,
                 vki_off_t, offset);
   if (!ML_(fd_allowed)(ARG1, "pwritev", tid, False)) {
      SET_STATUS_Failure( VKI_EBADF );
   } else {
      if ((Int)ARG3 >= 0) {
         PRE_MEM_READ( "pwritev(vector)", ARG2, ARG3 * sizeof(struct vki_iovec) );
      }
      if (ML_(safe_to_deref)((struct vki_iovec *)ARG2, ARG3 * sizeof(struct vki_iovec))) {
         vec = (struct vki_iovec *)(Addr)ARG2;
         for (i = 0; i < (Int)ARG3; i++) {
            VG_(sprintf)(buf, "pwritev(iov[%d])", i);
            PRE_MEM_READ(buf, (Addr)vec[i].iov_base, vec[i].iov_len );
         }
      }
   }
}

// SYS_sendfile   393
// int sendfile(int fd, int s, off_t offset, size_t nbytes,
//         struct sf_hdtr *hdtr, off_t *sbytes, int flags);
PRE(sys_sendfile)
{
   *flags |= SfMayBlock;

   PRINT("sys_sendfile ( %" FMT_REGWORD "d, %" FMT_REGWORD "d, %lu, %" FMT_REGWORD "u, %#" FMT_REGWORD "x, %#" FMT_REGWORD "x, %" FMT_REGWORD "d )",
         SARG1,SARG2,ARG3,ARG4,ARG5,ARG6,SARG7);
   PRE_REG_READ7(int, "sendfile",
                 int, fd, int, s, vki_off_t, offset, size_t, nbytes,
                 void *, hdtr, vki_off_t *, sbytes, int, flags);

   if (ARG5 != 0) {
      PRE_MEM_READ("sendfile(hdtr)", ARG5, sizeof(struct vki_sf_hdtr));
   }

   if (ARG6 != 0) {
      PRE_MEM_WRITE( "sendfile(sbytes)", ARG6, sizeof(vki_off_t) );
   }
}

POST(sys_sendfile)
{
   if (ARG6 != 0 ) {
      POST_MEM_WRITE( ARG6, sizeof( vki_off_t ) );
   }
}

// SYS_sigreturn  417
// int sigreturn(const ucontext_t *scp);
PRE(sys_sigreturn)
{
   PRINT("sys_sigreturn ( %#" FMT_REGWORD "x )", ARG1);
   PRE_REG_READ1(int, "sigreturn",
                 struct vki_ucontext *, scp);

   PRE_MEM_READ( "sigreturn(scp)", ARG1, sizeof(struct vki_ucontext) );
   PRE_MEM_WRITE( "sigreturn(scp)", ARG1, sizeof(struct vki_ucontext) );
}

static void restore_mcontext(ThreadState *tst, struct vki_mcontext *sc)
{
   tst->arch.vex.guest_RAX     = sc->rax;
   tst->arch.vex.guest_RCX     = sc->rcx;
   tst->arch.vex.guest_RDX     = sc->rdx;
   tst->arch.vex.guest_RBX     = sc->rbx;
   tst->arch.vex.guest_RBP     = sc->rbp;
   tst->arch.vex.guest_RSP     = sc->rsp;
   tst->arch.vex.guest_RSI     = sc->rsi;
   tst->arch.vex.guest_RDI     = sc->rdi;
   tst->arch.vex.guest_R8      = sc->r8;
   tst->arch.vex.guest_R9      = sc->r9;
   tst->arch.vex.guest_R10     = sc->r10;
   tst->arch.vex.guest_R11     = sc->r11;
   tst->arch.vex.guest_R12     = sc->r12;
   tst->arch.vex.guest_R13     = sc->r13;
   tst->arch.vex.guest_R14     = sc->r14;
   tst->arch.vex.guest_R15     = sc->r15;
   tst->arch.vex.guest_RIP     = sc->rip;
   /*
    * XXX: missing support for other flags.
    */
   if (sc->rflags & 0x0001)
      LibVEX_GuestAMD64_put_rflag_c(1, &tst->arch.vex);
   else
      LibVEX_GuestAMD64_put_rflag_c(0, &tst->arch.vex);
}

static void fill_mcontext(ThreadState *tst, struct vki_mcontext *sc)
{
   sc->rax = tst->arch.vex.guest_RAX;
   sc->rcx = tst->arch.vex.guest_RCX;
   sc->rdx = tst->arch.vex.guest_RDX;
   sc->rbx = tst->arch.vex.guest_RBX;
   sc->rbp = tst->arch.vex.guest_RBP;
   sc->rsp = tst->arch.vex.guest_RSP;
   sc->rsi = tst->arch.vex.guest_RSI;
   sc->rdi = tst->arch.vex.guest_RDI;
   sc->r8 = tst->arch.vex.guest_R8;
   sc->r9 = tst->arch.vex.guest_R9;
   sc->r10 = tst->arch.vex.guest_R10;
   sc->r11 = tst->arch.vex.guest_R11;
   sc->r12 = tst->arch.vex.guest_R12;
   sc->r13 = tst->arch.vex.guest_R13;
   sc->r14 = tst->arch.vex.guest_R14;
   sc->r15 = tst->arch.vex.guest_R15;
   sc->rip = tst->arch.vex.guest_RIP;
   /*
      Not supported by VEX.
      sc->cs = tst->arch.vex.guest_CS;
      sc->ss = tst->arch.vex.guest_SS;
      sc->ds = tst->arch.vex.guest_DS;
      sc->es = tst->arch.vex.guest_ES;
      sc->fs = tst->arch.vex.guest_FS;
      sc->gs = tst->arch.vex.guest_GS;
   */
   sc->rflags = LibVEX_GuestAMD64_get_rflags(&tst->arch.vex);
   /*
      not yet.
      VG_(memcpy)(&sc->fpstate, fpstate, sizeof(*fpstate));
   */
   sc->fpformat = VKI_FPFMT_NODEV;
   sc->ownedfp = VKI_FPOWNED_NONE;
   sc->len = sizeof(*sc);
   VG_(memset)(sc->spare2, 0, sizeof(sc->spare2));
}

// SYS_getcontext 421
// int getcontext(ucontext_t *ucp);
PRE(sys_getcontext)
{
   ThreadState* tst;
   struct vki_ucontext *uc;

   PRINT("sys_getcontext ( %#" FMT_REGWORD "x )", ARG1);
   PRE_REG_READ1(int, "getcontext",
                 struct vki_ucontext *, ucp);
   PRE_MEM_WRITE( "getcontext(ucp)", ARG1, sizeof(struct vki_ucontext) );
   uc = (struct vki_ucontext *)ARG1;
   if (!ML_(safe_to_deref)(uc, sizeof(struct vki_ucontext))) {
      SET_STATUS_Failure(VKI_EFAULT);
      return;
   }
   tst = VG_(get_ThreadState)(tid);
   fill_mcontext(tst, &uc->uc_mcontext);
   uc->uc_mcontext.rax = 0;
   uc->uc_mcontext.rdx = 0;
   uc->uc_mcontext.rflags &= ~0x0001;  /* PSL_C */
   uc->uc_sigmask = tst->sig_mask;
   VG_(memset)(uc->__spare__, 0, sizeof(uc->__spare__));
   SET_STATUS_Success(0);
}

// SYS_setcontext 422
// int setcontext(const ucontext_t *ucp);
PRE(sys_setcontext)
{
   ThreadState* tst;
   struct vki_ucontext *uc;

   PRINT("sys_setcontext ( %#" FMT_REGWORD "x )", ARG1);
   PRE_REG_READ1(long, "setcontext",
                 struct vki_ucontext *, ucp);

   PRE_MEM_READ( "setcontext(ucp)", ARG1, sizeof(struct vki_ucontext) );
   PRE_MEM_WRITE( "setcontext(ucp)", ARG1, sizeof(struct vki_ucontext) );

   vg_assert(VG_(is_valid_tid)(tid));
   vg_assert(tid >= 1 && tid < VG_N_THREADS);
   vg_assert(VG_(is_running_thread)(tid));

   tst = VG_(get_ThreadState)(tid);
   uc = (struct vki_ucontext *)ARG1;
   if (!ML_(safe_to_deref)(uc, sizeof(struct vki_ucontext)) || uc->uc_mcontext.len != sizeof(uc->uc_mcontext)) {
      SET_STATUS_Failure(VKI_EFAULT);
      return;
   }

   restore_mcontext(tst, &uc->uc_mcontext);
   tst->sig_mask = uc->uc_sigmask;
   tst->tmp_sig_mask = uc->uc_sigmask;

   /* Tell the driver not to update the guest state with the "result",
      and set a bogus result to keep it happy. */
   *flags |= SfNoWriteResult;
   SET_STATUS_Success(0);

   /* Check to see if some any signals arose as a result of this. */
   *flags |= SfPollAfter;
}

// SYS_swapcontext   423
// int swapcontext(ucontext_t *oucp, const ucontext_t *ucp);
PRE(sys_swapcontext)
{
   struct vki_ucontext *ucp;
   struct vki_ucontext *oucp;
   ThreadState* tst;

   PRINT("sys_swapcontext ( %#" FMT_REGWORD "x, %#" FMT_REGWORD "x )", ARG1, ARG2);
   PRE_REG_READ2(long, "swapcontext",
                 struct vki_ucontext *, oucp, struct vki_ucontext *, ucp);

   PRE_MEM_READ( "swapcontext(ucp)", ARG2, sizeof(struct vki_ucontext) );
   PRE_MEM_WRITE( "swapcontext(oucp)", ARG1, sizeof(struct vki_ucontext) );

   oucp = (struct vki_ucontext *)ARG1;
   ucp = (struct vki_ucontext *)ARG2;
   if (!ML_(safe_to_deref)(oucp, sizeof(struct vki_ucontext)) ||
         !ML_(safe_to_deref)(ucp, sizeof(struct vki_ucontext)) ||
         ucp->uc_mcontext.len != sizeof(ucp->uc_mcontext)) {
      SET_STATUS_Failure(VKI_EINVAL);
      return;
   }
   tst = VG_(get_ThreadState)(tid);

   /*
    * Save the context.
    */
   fill_mcontext(tst, &oucp->uc_mcontext);
   oucp->uc_mcontext.rax = 0;
   oucp->uc_mcontext.rdx = 0;
   oucp->uc_mcontext.rflags &= ~0x0001;   /* PSL_C */
   oucp->uc_sigmask = tst->sig_mask;
   VG_(memset)(oucp->__spare__, 0, sizeof(oucp->__spare__));

   /*
    * Switch to new one.
    */
   restore_mcontext(tst, &ucp->uc_mcontext);
   tst->sig_mask = ucp->uc_sigmask;
   tst->tmp_sig_mask = ucp->uc_sigmask;

   /* Tell the driver not to update the guest state with the "result",
      and set a bogus result to keep it happy. */
   *flags |= SfNoWriteResult;
   SET_STATUS_Success(0);

   /* Check to see if some any signals arose as a result of this. */
   *flags |= SfPollAfter;
}

// SYS_thr_new 455
// int thr_new(struct thr_param *param, int param_size);
PRE(sys_thr_new)
{
   static const Bool debug = False;

   ThreadId     ctid = VG_(alloc_ThreadState)();
   ThreadState* ptst = VG_(get_ThreadState)(tid);
   ThreadState* ctst = VG_(get_ThreadState)(ctid);
   SysRes       res;
   vki_sigset_t blockall;
   vki_sigset_t savedmask;
   struct vki_thr_param tp;
   Addr stk;

   PRINT("thr_new ( %#" FMT_REGWORD "x, %" FMT_REGWORD "u )",ARG1,ARG2);
   PRE_REG_READ2(int, "thr_new",
                 struct thr_param *, param,
                 int, param_size);

   PRE_MEM_READ( "thr_new(param)", ARG1, offsetof(struct vki_thr_param, spare));
   if (!ML_(safe_to_deref)( (void*)ARG1, offsetof(struct vki_thr_param, spare))) {
      SET_STATUS_Failure( VKI_EFAULT );
      return;
   }
   VG_(memset)(&tp, 0, sizeof(tp));
   VG_(memcpy)(&tp, (void *)ARG1, offsetof(struct vki_thr_param, spare));
   PRE_MEM_WRITE("thr_new(parent_tidptr)", (Addr)tp.parent_tid, sizeof(long));
   PRE_MEM_WRITE("thr_new(child_tidptr)", (Addr)tp.child_tid, sizeof(long));

   VG_(sigfillset)(&blockall);

   vg_assert(VG_(is_running_thread)(tid));
   vg_assert(VG_(is_valid_tid)(ctid));

   /* Copy register state

      On linux, both parent and child return to the same place, and the code
      following the clone syscall works out which is which, so we
      don't need to worry about it.
      On FreeBSD, thr_new arranges a direct call.  We don't actually need any
      of this gunk.

      The parent gets the child's new tid returned from clone, but the
      child gets 0.

      If the clone call specifies a NULL rsp for the new thread, then
      it actually gets a copy of the parent's rsp.
   */
   /* We inherit our parent's guest state. */
   ctst->arch.vex = ptst->arch.vex;
   ctst->arch.vex_shadow1 = ptst->arch.vex_shadow1;
   ctst->arch.vex_shadow2 = ptst->arch.vex_shadow2;

   /* Make thr_new appear to have returned Success(0) in the
      child. */
   ctst->arch.vex.guest_RAX = 0;
   ctst->arch.vex.guest_RDX = 0;
   LibVEX_GuestAMD64_put_rflag_c(0, &ctst->arch.vex);

   ctst->os_state.parent = tid;

   /* inherit signal mask */
   ctst->sig_mask = ptst->sig_mask;
   ctst->tmp_sig_mask = ptst->sig_mask;

   /* Linux has to guess, we don't */
   ctst->client_stack_highest_byte = (Addr)tp.stack_base + tp.stack_size;
   ctst->client_stack_szB = tp.stack_size;
   ctst->os_state.stk_id = VG_(register_stack)((Addr)tp.stack_base, (Addr)tp.stack_base + tp.stack_size);

   /* Assume the thr_new will succeed, and tell any tool that wants to
      know that this thread has come into existence.  If the thr_new
      fails, we'll send out a ll_exit notification for it at the out:
      label below, to clean up. */
   VG_TRACK ( pre_thread_ll_create, tid, ctid );

   if (debug) {
      VG_(printf)("clone child has SETTLS: tls at %#lx\n", (Addr)tp.tls_base);
   }
   ctst->arch.vex.guest_FS_CONST = (UWord)tp.tls_base;
   tp.tls_base = 0;  /* Don't have the kernel do it too */

   /* start the thread with everything blocked */
   VG_(sigprocmask)(VKI_SIG_SETMASK, &blockall, &savedmask);

   /* Set the client state for scheduler to run libthr's trampoline */
   ctst->arch.vex.guest_RDI = (Addr)tp.arg;
   /* XXX: align on 16-byte boundary? */
   ctst->arch.vex.guest_RSP = (Addr)tp.stack_base + tp.stack_size - 8;
   ctst->arch.vex.guest_RIP = (Addr)tp.start_func;

   /* But this is for thr_new() to run valgrind's trampoline */
   tp.start_func = (void *)ML_(start_thread_NORETURN);
   tp.arg = &VG_(threads)[ctid];

   /* And valgrind's trampoline on its own stack */
   stk = ML_(allocstack)(ctid);
   if (stk == (Addr)NULL) {
      res = VG_(mk_SysRes_Error)( VKI_ENOMEM );
      goto fail;
   }
   tp.stack_base = (void *)ctst->os_state.valgrind_stack_base;
   tp.stack_size = (Addr)stk - (Addr)tp.stack_base;

   /* Create the new thread */
   res = VG_(do_syscall2)(__NR_thr_new, (UWord)&tp, sizeof(tp));

   VG_(sigprocmask)(VKI_SIG_SETMASK, &savedmask, NULL);

fail:
   if (sr_isError(res)) {
      /* thr_new failed */
      VG_(cleanup_thread)(&ctst->arch);
      ctst->status = VgTs_Empty;
      /* oops.  Better tell the tool the thread exited in a hurry :-) */
      VG_TRACK( pre_thread_ll_exit, ctid );
   } else {

      POST_MEM_WRITE((Addr)tp.parent_tid, sizeof(long));
      POST_MEM_WRITE((Addr)tp.child_tid, sizeof(long));

      /* Thread creation was successful; let the child have the chance
         to run */
      *flags |= SfYieldAfter;
   }

   /* "Complete" the syscall so that the wrapper doesn't call the kernel again. */
   SET_STATUS_from_SysRes(res);
}

// SYS_pread   475
// ssize_t pread(int fd, void *buf, size_t nbytes, off_t offset);
PRE(sys_pread)
{
   *flags |= SfMayBlock;
   PRINT("sys_pread ( %" FMT_REGWORD "u, %#" FMT_REGWORD "x, %" FMT_REGWORD "u, %" FMT_REGWORD "u )", ARG1, ARG2, ARG3, ARG4);
   PRE_REG_READ4(ssize_t, "pread",
                 unsigned int, fd, char *, buf, vki_size_t, count,
                 unsigned long, off);

   if (!ML_(fd_allowed)(ARG1, "read", tid, False)) {
      SET_STATUS_Failure( VKI_EBADF );
   } else {
      PRE_MEM_WRITE( "pread(buf)", ARG2, ARG3 );
   }
}

POST(sys_pread)
{
   vg_assert(SUCCESS);
   POST_MEM_WRITE( ARG2, RES );
}

// SYS_pwrite  476
// ssize_t pwrite(int fd, const void *buf, size_t nbytes, off_t offset);
PRE(sys_pwrite)
{
   Bool ok;
   *flags |= SfMayBlock;
   PRINT("sys_pwrite ( %" FMT_REGWORD "u, %#" FMT_REGWORD "x, %" FMT_REGWORD "u, %" FMT_REGWORD "u )", ARG1, ARG2, ARG3, ARG4);
   PRE_REG_READ4(ssize_t, "pwrite",
                 int, fd, const char *, buf, vki_size_t, nbytes,
                 vki_off_t, offset);
   /* check to see if it is allowed.  If not, try for an exemption from
      --sim-hints=enable-outer (used for self hosting). */
   ok = ML_(fd_allowed)(ARG1, "pwrite", tid, False);
   if (!ok && ARG1 == 2/*stderr*/
         && SimHintiS(SimHint_enable_outer, VG_(clo_sim_hints)))
      ok = True;
   if (!ok) {
      SET_STATUS_Failure( VKI_EBADF );
   } else {
      PRE_MEM_READ( "pwrite(buf)", ARG2, ARG3 );
   }
}

// SYS_mmap 477
/* FreeBSD-7 introduces a "regular" version of mmap etc. */
// void * mmap(void *addr, size_t len, int prot, int flags, int fd, off_t offset);
PRE(sys_mmap)
{
   SysRes r;

   PRINT("sys_mmap ( %#" FMT_REGWORD "x, %" FMT_REGWORD "u, %" FMT_REGWORD "u, %" FMT_REGWORD "u, %" FMT_REGWORD "u, 0x%" FMT_REGWORD "x)",
         ARG1, (UWord)ARG2, ARG3, ARG4, ARG5, ARG6 );
   PRE_REG_READ6(void *, "mmap",
                 void *, addr, size_t, len, int, prot,  int, flags,
                 int, fd,  off_t, offset);

   r = ML_(generic_PRE_sys_mmap)( tid, ARG1, ARG2, ARG3, ARG4, ARG5, ARG6 );
   SET_STATUS_from_SysRes(r);
}

// SYS_lseek 478
// off_t lseek(int fildes, off_t offset, int whence);
PRE(sys_lseek)
{
   PRINT("sys_lseek ( %" FMT_REGWORD "u, 0x%" FMT_REGWORD "x, %" FMT_REGWORD "u )", ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "lseek",
                 unsigned int, fd, unsigned long, offset,
                 unsigned int, whence);
}

// SYS_truncate   479
// int truncate(const char *path, off_t length);
PRE(sys_truncate)
{
   *flags |= SfMayBlock;
   PRINT("sys_truncate ( %#" FMT_REGWORD "x(%s), %" FMT_REGWORD "u )", ARG1,(char *)ARG1,ARG2);
   PRE_REG_READ2(long, "truncate",
                 const char *, path, unsigned long, length);
   PRE_MEM_RASCIIZ( "truncate(path)", ARG1 );
}

// SYS_ftruncate  480
// int ftruncate(int fd, off_t length);
PRE(sys_ftruncate)
{
   *flags |= SfMayBlock;
   PRINT("sys_ftruncate ( %" FMT_REGWORD "u, %" FMT_REGWORD "u )", ARG1,ARG2);
   PRE_REG_READ2(long, "ftruncate", unsigned int, fd,
                 unsigned long, length);
}

// SYS_cpuset_setid  485
// int cpuset_setid(cpuwhich_t which, id_t id, cpusetid_t setid);
PRE(sys_cpuset_setid)
{
   PRINT("sys_cpuset_setid ( %" FMT_REGWORD "d, %" FMT_REGWORD "d, %#" FMT_REGWORD "x )",
         SARG1, SARG2, ARG3);
   PRE_REG_READ3(int, "cpuset_setid", vki_cpuwhich_t, which, vki_id_t, id,
                 vki_cpusetid_t *,setid);
}

// SYS_cpuset_getid  486
// int cpuset_getid(cpulevel_t level, cpuwhich_t which, id_t id,
//                  cpusetid_t *setid);
PRE(sys_cpuset_getid)
{
   PRINT("sys_cpuset_getid ( %" FMT_REGWORD "d, %" FMT_REGWORD "d, %" FMT_REGWORD "d, %#" FMT_REGWORD "x )",
         SARG1, SARG2, SARG3, ARG4);
   PRE_REG_READ4(int, "cpuset_getid", vki_cpulevel_t, level,
                 vki_cpuwhich_t, which, vki_id_t, id,
                 vki_cpusetid_t, setid);
   PRE_MEM_WRITE("cpuset_getid(setid)", ARG4, sizeof(vki_cpusetid_t));
}

POST(sys_cpuset_getid)
{
   POST_MEM_WRITE(ARG4, sizeof(vki_cpusetid_t));
}

// SYS_cpuset_getaffinity  487
// int cpuset_getaffinity(cpulevel_t level, cpuwhich_t which, id_t id,
//                        size_t setsize, cpuset_t *mask);
PRE(sys_cpuset_getaffinity)
{
   PRINT("sys_cpuset_getaffinity ( %" FMT_REGWORD "u, %" FMT_REGWORD "u, %" FMT_REGWORD "d, %" FMT_REGWORD "u, %#" FMT_REGWORD "x )",
         ARG1, ARG2, SARG3, ARG4, ARG5);
   PRE_REG_READ5(int, "cpuset_getaffinity",
                 vki_cpulevel_t, level, vki_cpuwhich_t, which, vki_id_t, id,
                 size_t, setsize, void *, mask);
   PRE_MEM_WRITE("cpuset_getaffinity", ARG5, ARG4);
}

POST(sys_cpuset_getaffinity)
{
   vg_assert(SUCCESS);
   if (RES == 0)
      POST_MEM_WRITE( ARG5, ARG4 );
}

// SYS_cpuset_setaffinity  488
// int cpuset_setaffinity(cpulevel_t level, cpuwhich_t which, id_t id,
//                        size_t setsize, const cpuset_t *mask);
PRE(sys_cpuset_setaffinity)
{

   PRINT("sys_cpuset_setaffinity ( %" FMT_REGWORD "u, %" FMT_REGWORD "u, %" FMT_REGWORD "d, %" FMT_REGWORD "u, %#" FMT_REGWORD "x )",
         ARG1, ARG2, SARG3, ARG4, ARG5);
   PRE_REG_READ5(int, "cpuset_setaffinity",
                 vki_cpulevel_t, level, vki_cpuwhich_t, which, vki_id_t, id,
                 size_t, setsize, void *, mask);
   PRE_MEM_READ("cpuset_setaffinity", ARG5, ARG4);
}

// SYS_posix_fallocate 530
// int posix_fallocate(int fd, off_t offset, off_t len);
PRE(sys_posix_fallocate)
{
   PRINT("sys_posix_fallocate ( %" FMT_REGWORD "d, %" FMT_REGWORD "u, %" FMT_REGWORD "u )",
         SARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "posix_fallocate",
                 int, fd, vki_off_t, offset,
                 vki_off_t, len);
}

// SYS_posix_fadvise 531
// int posix_fadvise(int fd, off_t offset, off_t len, int advice);
PRE(sys_posix_fadvise)
{
   PRINT("sys_posix_fadvise ( %" FMT_REGWORD "d, %" FMT_REGWORD "u, %" FMT_REGWORD "u, %" FMT_REGWORD "d )",
         SARG1, ARG2, ARG3, SARG4);
   PRE_REG_READ4(long, "posix_fadvise",
                 int, fd, off_t, offset,
                 off_t, len,
                 int, advice);
   // @todo PJF advice can be 0 to 5 inclusive
}

// SYS_wait6   532
// pid_t wait6(idtype_t idtype, id_t id, int *status, int options,
//             struct __wrusage *wrusage, siginfo_t *infop);
PRE(sys_wait6)
{
   PRINT("sys_wait6 ( %" FMT_REGWORD "d, %" FMT_REGWORD "d, %#" FMT_REGWORD "x, %" FMT_REGWORD "d, %#" FMT_REGWORD "x, %#" FMT_REGWORD "x )",
         SARG1, SARG2, ARG3, SARG4, ARG5, ARG6);
   PRE_REG_READ6(pid_t, "wait6", vki_idtype_t, idtype, vki_id_t, id, int *, status, int, options,
                 struct vki___wrusage *, wrusage, vki_siginfo_t *,infop);
   PRE_MEM_WRITE("wait6(status)", ARG3, sizeof(int));
   if (ARG5) {
      PRE_MEM_WRITE("wait6(wrusage)", ARG5, sizeof(struct vki___wrusage));
   }
   if (ARG6) {
      PRE_MEM_WRITE("wait6(infop)", ARG6, sizeof(vki_siginfo_t));
   }
}

POST(sys_wait6)
{
   POST_MEM_WRITE(ARG3, sizeof(int));
   if (ARG5) {
      POST_MEM_WRITE(ARG5, sizeof(struct vki___wrusage));
   }

   if (ARG6) {
      POST_MEM_WRITE(ARG6, sizeof(vki_siginfo_t));
   }
}

// the man page is inconsistent for the last argument
// See https://bugs.freebsd.org/bugzilla/show_bug.cgi?id=247386
// will stick to 'arg' for simplicity

// SYS_procctl 544
// int procctl(idtype_t idtype, id_t id, int cmd, void *arg);
PRE(sys_procctl)
{
   PRINT("sys_procctl ( %" FMT_REGWORD "d, %" FMT_REGWORD "d, %" FMT_REGWORD"d, %#" FMT_REGWORD "x )",
         SARG1, SARG2, SARG3, ARG4);
   PRE_REG_READ4(int, "procctl", vki_idtype_t, idtype, vki_id_t, id, int, cmd, void *, arg);
   switch (ARG3) {
   case VKI_PROC_ASLR_CTL:
   case VKI_PROC_SPROTECT:
   case VKI_PROC_TRACE_CTL:
   case VKI_PROC_TRAPCAP_CTL:
   case VKI_PROC_PDEATHSIG_CTL:
   case VKI_PROC_STACKGAP_CTL:
   case VKI_PROC_NO_NEW_PRIVS_CTL:
   case VKI_PROC_WXMAP_CTL:
      PRE_MEM_READ("procctl(arg)", ARG4, sizeof(int));
      break;
   case VKI_PROC_REAP_STATUS:
      PRE_MEM_READ("procctl(arg)", ARG4, sizeof(struct vki_procctl_reaper_status));
      break;
   case VKI_PROC_REAP_GETPIDS:
      PRE_MEM_READ("procctl(arg)", ARG4, sizeof(struct vki_procctl_reaper_pids));
      break;
   case VKI_PROC_REAP_KILL:
      /* The first three fields are reads
       * int rk_sig;
       * u_int rk_flags;
       * pid_t rk_subtree;
       *
       * The last two fields are writes
       * u_int rk_killed;
       * pid_t rk_fpid;
       *
       * There is also a pad field
       */
      PRE_MEM_READ("procctl(arg)", ARG4, sizeof(int) + sizeof(u_int) + sizeof(vki_pid_t));
      PRE_MEM_WRITE("procctl(arg)", ARG4+offsetof(struct vki_procctl_reaper_kill, rk_killed), sizeof(u_int) + sizeof(vki_pid_t));
      break;
   case VKI_PROC_ASLR_STATUS:
   case VKI_PROC_PDEATHSIG_STATUS:
   case VKI_PROC_STACKGAP_STATUS:
   case VKI_PROC_TRAPCAP_STATUS:
   case VKI_PROC_TRACE_STATUS:
   case VKI_PROC_NO_NEW_PRIVS_STATUS:
   case VKI_PROC_WXMAP_STATUS:
      PRE_MEM_WRITE("procctl(arg)", ARG4, sizeof(int));
   case VKI_PROC_REAP_ACQUIRE:
   case VKI_PROC_REAP_RELEASE:
   default:
      break;
   }
}

POST(sys_procctl)
{
   switch (ARG3) {
   case VKI_PROC_REAP_KILL:
      POST_MEM_WRITE(ARG4+offsetof(struct vki_procctl_reaper_kill, rk_killed), sizeof(u_int) + sizeof(vki_pid_t));
      break;
   case VKI_PROC_ASLR_STATUS:
   case VKI_PROC_PDEATHSIG_STATUS:
   case VKI_PROC_STACKGAP_STATUS:
   case VKI_PROC_TRAPCAP_STATUS:
   case VKI_PROC_TRACE_STATUS:
   case VKI_PROC_NO_NEW_PRIVS_STATUS:
   case VKI_PROC_WXMAP_STATUS:
      POST_MEM_WRITE(ARG4, sizeof(int));
   default:
      break;
   }
}

// SYS_mknodat 559
// int mknodat(int fd, const char *path, mode_t mode, dev_t dev);
PRE(sys_mknodat)
{
   PRINT("sys_mknodat ( %" FMT_REGWORD "u, %#" FMT_REGWORD "x(%s), 0x%" FMT_REGWORD "x, 0x%" FMT_REGWORD "x )", ARG1,ARG2,(char*)ARG2,ARG3,ARG4 );
   PRE_REG_READ4(long, "mknodat",
                 int, fd, const char *, path, vki_mode_t, mode, vki_dev_t, dev);
   PRE_MEM_RASCIIZ( "mknodat(pathname)", ARG2 );
}

#if (FREEBSD_VERS >= FREEBSD_12)

// SYS_cpuset_getdomain 561
// int cpuset_getdomain(cpulevel_t level, cpuwhich_t which, id_t id,
//                      size_t setsize, domainset_t *mask, int *policy);
PRE(sys_cpuset_getdomain)
{
   PRINT("sys_cpuset_getdomain ( %" FMT_REGWORD "d, %" FMT_REGWORD "d, %" FMT_REGWORD "d, %" FMT_REGWORD "u, %#" FMT_REGWORD "x, %#" FMT_REGWORD "x )",
         SARG1, SARG2, SARG3, ARG4, ARG5, ARG6);
   PRE_REG_READ6(int, "cpuset_getdomain",
                 cpulevel_t, level, cpuwhich_t, which, id_t, id,
                 size_t, setsize, vki_domainset_t *, mask, int *, policy);
   // man page says that setsize (ARG4) "is usually provided by calling sizeof(mask)"
   PRE_MEM_WRITE( "cpuset_getdomain(mask)", ARG5, ARG4 );
   PRE_MEM_WRITE( "cpuset_getdomain(policy)", ARG6, sizeof(int) );
}

POST(sys_cpuset_getdomain)
{
   POST_MEM_WRITE(ARG5, ARG4 );
   POST_MEM_WRITE(ARG6, sizeof(int) );
}

// SYS_cpuset_setdomain 562
// int cuset_setdomain(cpulevel_t level, cpuwhich_t which, id_t id,
//                     size_t setsize, const domainset_t *mask, int policy);
PRE(sys_cpuset_setdomain)
{
   PRINT("sys_cpuget_getdomain ( %" FMT_REGWORD "d, %" FMT_REGWORD "d, %" FMT_REGWORD "d, %" FMT_REGWORD "u, %#" FMT_REGWORD "x, %" FMT_REGWORD "d )",
         SARG1, SARG2, SARG3, ARG4, ARG5, SARG6);
   PRE_REG_READ6(int, "cpuset_getdomain",
                 cpulevel_t, level, cpuwhich_t, which, id_t, id,
                 size_t, setsize, vki_domainset_t *, mask, int, policy);
   // man page says that setsize (ARG4) "is usually provided by calling sizeof(mask)"
   PRE_MEM_READ( "cpuset_getdomain(mask)", ARG5, ARG4 );
}

#endif

PRE(sys_fake_sigreturn)
{
   ThreadState* tst;
   struct vki_ucontext *uc;
   ULong rflags;

   PRINT("sys_sigreturn ( %#" FMT_REGWORD "x )", ARG1);
   PRE_REG_READ1(long, "sigreturn",
                 struct vki_ucontext *, scp);

   PRE_MEM_READ( "sigreturn(scp)", ARG1, sizeof(struct vki_ucontext) );
   PRE_MEM_WRITE( "sigreturn(scp)", ARG1, sizeof(struct vki_ucontext) );

   vg_assert(VG_(is_valid_tid)(tid));
   vg_assert(tid >= 1 && tid < VG_N_THREADS);
   vg_assert(VG_(is_running_thread)(tid));

   /* Adjust esp to point to start of frame; skip back up over handler
      ret addr */
   tst = VG_(get_ThreadState)(tid);
   tst->arch.vex.guest_RSP -= sizeof(Addr);

   uc = (struct vki_ucontext *)ARG1;
   if (uc == NULL || uc->uc_mcontext.len != sizeof(uc->uc_mcontext)) {
      SET_STATUS_Failure(VKI_EINVAL);
      return;
   }

   /* This is only so that the EIP is (might be) useful to report if
      something goes wrong in the sigreturn */
   ML_(fixup_guest_state_to_restart_syscall)(&tst->arch);

   VG_(sigframe_destroy)(tid);

   /* For unclear reasons, it appears we need the syscall to return
      without changing %RAX.  Since %RAX is the return value, and can
      denote either success or failure, we must set up so that the
      driver logic copies it back unchanged.  Also, note %RAX is of
      the guest registers written by VG_(sigframe_destroy). */
   rflags = LibVEX_GuestAMD64_get_rflags(&tst->arch.vex);
   SET_STATUS_from_SysRes( VG_(mk_SysRes_amd64_freebsd)( tst->arch.vex.guest_RAX,
                           tst->arch.vex.guest_RDX, (rflags & 1U) != 0U ? True : False) );

   /*
    * Signal handler might have changed the signal mask.  Respect that.
    */
   tst->sig_mask = uc->uc_sigmask;
   tst->tmp_sig_mask = uc->uc_sigmask;

   /* Tell the driver not to update the guest state with the "result",
      and set a bogus result to keep it happy. */
   *flags |= SfNoWriteResult;
   SET_STATUS_Success(0);

   /* Check to see if some any signals arose as a result of this. */
   *flags |= SfPollAfter;
}


#undef PRE
#undef POST

#endif /* defined(VGP_amd64_freebsd) */

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
