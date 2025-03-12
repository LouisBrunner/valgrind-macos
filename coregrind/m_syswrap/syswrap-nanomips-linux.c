
/*--------------------------------------------------------------------*/
/*--- Platform-specific syscalls stuff.  syswrap-nanomips-linux.c ----*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2017-2018 RT-RK

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

#if defined(VGP_nanomips_linux)
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
#include "pub_core_options.h"
#include "pub_core_scheduler.h"
#include "pub_core_sigframe.h"     // For VG_(sigframe_destroy)()
#include "pub_core_signals.h"
#include "pub_core_syscall.h"
#include "pub_core_syswrap.h"
#include "pub_core_tooliface.h"
#include "pub_core_transtab.h"      // VG_(discard_translations)
#include "priv_types_n_macros.h"
#include "priv_syswrap-generic.h"   /* for decls of generic wrappers */
#include "priv_syswrap-linux.h"     /* for decls of linux-ish wrappers */
#include "priv_syswrap-main.h"

#include "pub_core_debuginfo.h"     // VG_(di_notify_*)
#include "pub_core_xarray.h"
#include "pub_core_clientstate.h"   // VG_(brk_base), VG_(brk_limit)
#include "pub_core_errormgr.h"
#include "pub_core_gdbserver.h"     // VG_(gdbserver)
#include "pub_core_libcfile.h"
#include "pub_core_machine.h"       // VG_(get_SP)
#include "pub_core_mallocfree.h"
#include "pub_core_stacktrace.h"    // For VG_(get_and_pp_StackTrace)()
#include "pub_core_ume.h"
#include "priv_syswrap-generic.h"
#include "config.h"

#include <errno.h>

/* ---------------------------------------------------------------------
                             clone() handling
   ------------------------------------------------------------------ */
/* Call f(arg1), but first switch stacks, using 'stack' as the new
   stack, and use 'retaddr' as f's return-to address.  Also, clear all
   the integer registers before entering f.*/

__attribute__ ((noreturn))
void ML_ (call_on_new_stack_0_1) (Addr stack, Addr retaddr,
                                  void (*f) (Word), Word arg1);
//    a0 = stack
//    a1 = retaddr
//    a2 = f
//    a3 = arg1
asm (
   ".text\n"
   ".globl vgModuleLocal_call_on_new_stack_0_1 \n"
   "vgModuleLocal_call_on_new_stack_0_1:       \n"
   "   move  $sp, $a0                        \n\t" // stack to $sp
   "   move  $ra, $a1                        \n\t" // retaddr to $ra
   "   move  $t9, $a2                        \n\t" // f to t9
   "   move  $a0, $a3                        \n\t" // arg1 to $a0
   "   li    $t4, 0                          \n\t" // zero all GP regs
   "   li    $t5, 0                          \n\t"
   "   li    $a1, 0                          \n\t"
   "   li    $a2, 0                          \n\t"
   "   li    $a3, 0                          \n\t"
   "   li    $t0, 0                          \n\t"
   "   li    $t1, 0                          \n\t"
   "   li    $t2, 0                          \n\t"
   "   li    $t3, 0                          \n\t"
   "   li    $s0, 0                          \n\t"
   "   li    $s1, 0                          \n\t"
   "   li    $s2, 0                          \n\t"
   "   li    $s3, 0                          \n\t"
   "   li    $s4, 0                          \n\t"
   "   li    $s5, 0                          \n\t"
   "   li    $s6, 0                          \n\t"
   "   li    $s7, 0                          \n\t"
   "   li    $t8, 0                          \n\t"
   "   jrc   $t9                             \n\t" // jump to dst
   "   break 0x7                               \n" // should never get here
   ".previous\n"
);

/*
        Perform a clone system call.  clone is strange because it has
        fork()-like return-twice semantics, so it needs special
        handling here.
        Upon entry, we have:
            int (fn)(void*)     in  $a0       0
            void* child_stack   in  $a1       4
            int flags           in  $a2       8
            void* arg           in  $a3      12
            pid_t* child_tid    in  $a4      16
            pid_t* parent_tid   in  $a5      20
            void* tls_ptr       in  $a6      24

        System call requires:
            int    $__NR_clone  in $t4
            int    flags        in $a0   0
            void*  child_stack  in $a1   4
            pid_t* parent_tid   in $a2   8
            void*  tls_ptr      in $a3  12
            pid_t* child_tid    in $a4  16

   int clone(int (*fn)(void *arg), void *child_stack, int flags, void *arg,
             void *parent_tidptr, void *tls, void *child_tidptr)

   Returns an Int encoded in the linux-mips way, not a SysRes.
 */
#define __NR_CLONE        VG_STRINGIFY(__NR_clone)
#define __NR_EXIT         VG_STRINGIFY(__NR_exit)

// See priv_syswrap-linux.h for arg profile.
asm (
"   .text                                              \n"
"   .set noreorder                                     \n"
"   .set nomacro                                       \n"
"   .globl   do_syscall_clone_nanomips_linux           \n"
"   do_syscall_clone_nanomips_linux:                 \n\t"
"   addiu $sp, $sp, -16                              \n\t"
"   sw    $ra,  0($sp)                               \n\t"
"   sw    $fp,  4($sp)                               \n\t"
"   sw    $gp,  8($sp)                               \n\t"

"   addiu  $a1, $a1, -16                             \n\t"
"   sw $a0, 0($a1)                                   \n\t" /* fn */
"   sw $a3, 4($a1)                                   \n\t" /* arg */

/* 1. arg for syscalls */
"   move $a0, $a2                                    \n\t" /* flags */
"   move $a2, $a5                                    \n\t" /* parent */
"   move $a3, $a6                                    \n\t" /* tls */

/* 2. do a syscall to clone */
"   li $t4, " __NR_CLONE                            "\n\t" /* __NR_clone */
"   syscall[32]                                      \n\t"

/* 3. See if we are a child, call fn and after that exit */
"   bnezc $a0, p_or_error                            \n\t"

"   lw $t9, 0($sp)                                   \n\t"
"   lw $a0, 4($sp)                                   \n\t"
"   jalrc $t9                                        \n\t"

"   li $t4, " __NR_EXIT                             "\n\t" /* NR_exit */
"   syscall[32]                                        \n"

/* 4. If we are parent or error, just return to caller */
"   p_or_error:                                      \n\t"
"   lw $ra, 0($sp)                                   \n\t"
"   lw $fp, 4($sp)                                   \n\t"
"   lw $gp, 8($sp)                                   \n\t"
"   addiu $sp, $sp, 16                               \n\t"

"   jrc $ra\n"
"   .previous\n"
);

#undef __NR_CLONE
#undef __NR_EXIT

// forward declarations
static SysRes sys_set_tls (ThreadId tid, Addr tlsptr);
static SysRes nanomips_PRE_sys_mmap (ThreadId tid,
                                 UWord arg1, UWord arg2, UWord arg3,
                                 UWord arg4, UWord arg5, Off64T arg6);
/* ---------------------------------------------------------------------
   More thread stuff
   ------------------------------------------------------------------ */

// MIPS doesn't have any architecture specific thread stuff that
// needs to be cleaned up
void
VG_ (cleanup_thread) (ThreadArchState * arch) { }

SysRes sys_set_tls ( ThreadId tid, Addr tlsptr )
{
   VG_(threads)[tid].arch.vex.guest_ULR = tlsptr;
   return VG_(mk_SysRes_Success)( 0 );
}

/* ---------------------------------------------------------------------
   mips handler for mmap and mmap2
   ------------------------------------------------------------------ */
static void notify_core_of_mmap(Addr a, SizeT len, UInt prot,
                                UInt flags, Int fd, Off64T offset)
{
   Bool d;

   /* 'a' is the return value from a real kernel mmap, hence: */
   vg_assert(VG_IS_PAGE_ALIGNED(a));
   /* whereas len is whatever the syscall supplied.  So: */
   len = VG_PGROUNDUP(len);

   d = VG_(am_notify_client_mmap)( a, len, prot, flags, fd, offset );

   if (d)
      VG_(discard_translations)( a, (ULong)len,
                                 "notify_core_of_mmap" );
}

static void notify_tool_of_mmap(Addr a, SizeT len, UInt prot, ULong di_handle)
{
   Bool rr, ww, xx;

   /* 'a' is the return value from a real kernel mmap, hence: */
   vg_assert(VG_IS_PAGE_ALIGNED(a));
   /* whereas len is whatever the syscall supplied.  So: */
   len = VG_PGROUNDUP(len);

   rr = toBool(prot & VKI_PROT_READ);
   ww = toBool(prot & VKI_PROT_WRITE);
   xx = toBool(prot & VKI_PROT_EXEC);

   VG_TRACK( new_mem_mmap, a, len, rr, ww, xx, di_handle );
}

/* Based on ML_(generic_PRE_sys_mmap) from syswrap-generic.c.
   If we are trying to do mmap with VKI_MAP_SHARED flag we need to align the
   start address on VKI_SHMLBA like we did in
   VG_(am_mmap_file_float_valgrind_flags)
 */
static SysRes nanomips_PRE_sys_mmap(ThreadId tid,
                                UWord arg1, UWord arg2, UWord arg3,
                                UWord arg4, UWord arg5, Off64T arg6)
{
   Addr       advised;
   SysRes     sres;
   MapRequest mreq;
   Bool       mreq_ok;

   if (arg2 == 0) {
      /* SuSV3 says: If len is zero, mmap() shall fail and no mapping
         shall be established. */
      return VG_(mk_SysRes_Error)( VKI_EINVAL );
   }

   if (!VG_IS_PAGE_ALIGNED(arg1)) {
      /* zap any misaligned addresses. */
      /* SuSV3 says misaligned addresses only cause the MAP_FIXED case
         to fail.   Here, we catch them all. */
      return VG_(mk_SysRes_Error)( VKI_EINVAL );
   }

   if (!VG_IS_PAGE_ALIGNED(arg6)) {
      /* zap any misaligned offsets. */
      /* SuSV3 says: The off argument is constrained to be aligned and
         sized according to the value returned by sysconf() when
         passed _SC_PAGESIZE or _SC_PAGE_SIZE. */
      return VG_(mk_SysRes_Error)( VKI_EINVAL );
   }

   /* Figure out what kind of allocation constraints there are
      (fixed/hint/any), and ask aspacem what we should do. */
   mreq.start = arg1;
   mreq.len   = arg2;

   if (arg4 & VKI_MAP_FIXED) {
      mreq.rkind = MFixed;
   } else if (arg1 != 0) {
      mreq.rkind = MHint;
   } else {
      mreq.rkind = MAny;
   }

   if ((VKI_SHMLBA > VKI_PAGE_SIZE) && (VKI_MAP_SHARED & arg4)
         && !(VKI_MAP_FIXED & arg4))
      mreq.len = arg2 + VKI_SHMLBA - VKI_PAGE_SIZE;

   /* Enquire ... */
   advised = VG_(am_get_advisory)( &mreq, True/*client*/, &mreq_ok );

   if ((VKI_SHMLBA > VKI_PAGE_SIZE) && (VKI_MAP_SHARED & arg4)
         && !(VKI_MAP_FIXED & arg4))
      advised = VG_ROUNDUP(advised, VKI_SHMLBA);

   if (!mreq_ok) {
      /* Our request was bounced, so we'd better fail. */
      return VG_(mk_SysRes_Error)( VKI_EINVAL );
   }

   /* Otherwise we're OK (so far).  Install aspacem's choice of
      address, and let the mmap go through.  */
   sres = VG_(am_do_mmap_NO_NOTIFY)(advised, arg2, arg3,
                                    arg4 | VKI_MAP_FIXED,
                                    arg5, arg6);

   /* A refinement: it may be that the kernel refused aspacem's choice
      of address.  If we were originally asked for a hinted mapping,
      there is still a last chance: try again at any address.
      Hence: */
   if (mreq.rkind == MHint && sr_isError(sres)) {
      mreq.start = 0;
      mreq.len   = arg2;
      mreq.rkind = MAny;
      advised = VG_(am_get_advisory)( &mreq, True/*client*/, &mreq_ok );

      if (!mreq_ok) {
         /* Our request was bounced, so we'd better fail. */
         return VG_(mk_SysRes_Error)( VKI_EINVAL );
      }

      /* and try again with the kernel */
      sres = VG_(am_do_mmap_NO_NOTIFY)(advised, arg2, arg3,
                                       arg4 | VKI_MAP_FIXED,
                                       arg5, arg6);
   }

   if (!sr_isError(sres)) {
      ULong di_handle;
      /* Notify aspacem. */
      notify_core_of_mmap(
         (Addr)sr_Res(sres), /* addr kernel actually assigned */
         arg2, /* length */
         arg3, /* prot */
         arg4, /* the original flags value */
         arg5, /* fd */
         arg6  /* offset */
      );
      /* Load symbols? */
      di_handle = VG_(di_notify_mmap)( (Addr)sr_Res(sres),
                                       False/*allow_SkFileV*/, (Int)arg5 );
      /* Notify the tool. */
      notify_tool_of_mmap(
         (Addr)sr_Res(sres), /* addr kernel actually assigned */
         arg2, /* length */
         arg3, /* prot */
         di_handle /* so the tool can refer to the read debuginfo later,
                      if it wants. */
      );
   }

   /* Stay sane */
   if (!sr_isError(sres) && (arg4 & VKI_MAP_FIXED))
      vg_assert(sr_Res(sres) == arg1);

   return sres;
}
/* ---------------------------------------------------------------------
   PRE/POST wrappers for mips/Linux-specific syscalls
   ------------------------------------------------------------------ */
#define PRE(name)       DEFN_PRE_TEMPLATE(mips_linux, name)
#define POST(name)      DEFN_POST_TEMPLATE(mips_linux, name)

/* Add prototypes for the wrappers declared here, so that gcc doesn't
   harass us for not having prototypes.  Really this is a kludge --
   the right thing to do is to make these wrappers 'static' since they
   aren't visible outside this file, but that requires even more macro
   magic. */
DECL_TEMPLATE (mips_linux, sys_mmap2);
DECL_TEMPLATE (mips_linux, sys_rt_sigreturn);
DECL_TEMPLATE (mips_linux, sys_set_thread_area);
DECL_TEMPLATE (mips_linux, sys_ptrace);
DECL_TEMPLATE (mips_linux, sys_unshare);
DECL_TEMPLATE (mips_linux, sys_reboot);
DECL_TEMPLATE (mips_linux, sys_setdomainname);
DECL_TEMPLATE (mips_linux, sys_sethostname);
DECL_TEMPLATE (mips_linux, sys_swapon);
DECL_TEMPLATE (mips_linux, sys_swapoff);

PRE(sys_mmap2)
{
   /* Exactly like sys_mmap() except the file offset is specified in 4096 byte
      units rather than bytes, so that it can be used for files bigger than
      2^32 bytes. */
   SysRes r;
   PRINT("sys_mmap2 ( %#lx, %lu, %ld, %ld, %ld, %ld )",
         ARG1, ARG2, SARG3, SARG4, SARG5, SARG6);
   PRE_REG_READ6(long, "mmap2", unsigned long, start, unsigned long, length,
                 unsigned long, prot, unsigned long, flags,
                 unsigned long, fd, unsigned long, offset);
   r = nanomips_PRE_sys_mmap(tid, ARG1, ARG2, ARG3, ARG4, ARG5,
                         4096 * (Off64T) ARG6);
   SET_STATUS_from_SysRes(r);
}

PRE(sys_ptrace)
{
   PRINT("sys_ptrace ( %ld, %ld, %#lx, %#lx )", SARG1, SARG2, ARG3, ARG4);
   PRE_REG_READ4(int, "ptrace",
                 long, request, long, pid, unsigned long, addr,
                 unsigned long, data);

   switch (ARG1) {
      case VKI_PTRACE_PEEKTEXT:
      case VKI_PTRACE_PEEKDATA:
      case VKI_PTRACE_PEEKUSR:
         PRE_MEM_WRITE("ptrace(peek)", ARG4, sizeof(long));
         break;

      case VKI_PTRACE_GETEVENTMSG:
         PRE_MEM_WRITE("ptrace(geteventmsg)", ARG4, sizeof(unsigned long));
         break;

      case VKI_PTRACE_GETSIGINFO:
         PRE_MEM_WRITE("ptrace(getsiginfo)", ARG4, sizeof(vki_siginfo_t));
         break;

      case VKI_PTRACE_SETSIGINFO:
         PRE_MEM_READ("ptrace(setsiginfo)", ARG4, sizeof(vki_siginfo_t));
         break;

      case VKI_PTRACE_GETREGSET:
         ML_(linux_PRE_getregset)(tid, ARG3, ARG4);
         break;

      default:
         break;
   }
}

POST(sys_ptrace)
{
   switch (ARG1) {
      case VKI_PTRACE_TRACEME:
         ML_(linux_POST_traceme)(tid);
         break;

      case VKI_PTRACE_PEEKTEXT:
      case VKI_PTRACE_PEEKDATA:
      case VKI_PTRACE_PEEKUSR:
         POST_MEM_WRITE (ARG4, sizeof(long));
         break;

      case VKI_PTRACE_GETEVENTMSG:
         POST_MEM_WRITE (ARG4, sizeof(unsigned long));
         break;

      case VKI_PTRACE_GETSIGINFO:
         POST_MEM_WRITE (ARG4, sizeof(vki_siginfo_t));
         break;

      case VKI_PTRACE_GETREGSET:
         ML_(linux_POST_getregset)(tid, ARG3, ARG4);
         break;

      default:
         break;
   }
}

PRE(sys_rt_sigreturn)
{
   ThreadState* tst;
   PRINT ("rt_sigreturn ( )");
   vg_assert (VG_ (is_valid_tid) (tid));
   vg_assert (tid >= 1 && tid < VG_N_THREADS);
   vg_assert (VG_ (is_running_thread) (tid));

   tst = VG_(get_ThreadState)(tid);

   ML_(fixup_guest_state_to_restart_syscall)(&tst->arch);

   /* Restore register state from frame and remove it */
   VG_ (sigframe_destroy) (tid, True);
   /* Tell the driver not to update the guest state with the "result",
      and set a bogus result to keep it happy. */
   *flags |= SfNoWriteResult;
   SET_STATUS_Success (0);
   /* Check to see if any signals arose as a result of this. */
   *flags |= SfPollAfter;
}

PRE(sys_set_thread_area)
{
   PRINT ("set_thread_area (%lx)", ARG1);
   PRE_REG_READ1(long, "set_thread_area", unsigned long, addr);
   SET_STATUS_from_SysRes( sys_set_tls( tid, ARG1 ) );
}

PRE(sys_unshare)
{
   PRINT("sys_unshare ( %lu )", ARG1);
   PRE_REG_READ1(long, "sys_unshare", unsigned long, flags);
}

PRE(sys_reboot)
{
   PRINT("sys_reboot ( %ld, %lu, %lu, %#lx )", SARG1, ARG2, ARG3, ARG4);
   // An approximation. ARG4 is only read conditionally by the kernel
   PRE_REG_READ4(int, "reboot",
                 int, magic1, int, magic2, unsigned int, cmd,
                 void *, arg);

   *flags |= SfMayBlock;
}

PRE(sys_setdomainname)
{
   PRINT ("sys_setdomainname ( %#lx, %ld )", ARG1, SARG2);
   PRE_REG_READ2 (long, "setdomainname", const void *, name, int, len);
}

PRE(sys_sethostname)
{
   PRINT ("sys_sethostname ( %#lx, %ld )", ARG1, SARG2);
   PRE_REG_READ2 (long, "sethostname", const void *, name, int, len);
}

PRE(sys_swapon)
{
   PRINT("sys_swapon ( %#lx, %#lx )", ARG1, ARG2);
   PRE_REG_READ2(long, "swapon", const void *, path, int, flags);
}

PRE(sys_swapoff)
{
   PRINT("sys_swapoff ( %#lx )", ARG1);
   PRE_REG_READ1(long, "swapoff", const void *, path);
}

#undef PRE
#undef POST

/* ---------------------------------------------------------------------
   The mips/Linux syscall table
   ------------------------------------------------------------------ */
#define PLAX_(sysno, name)    WRAPPER_ENTRY_X_(mips_linux, sysno, name)
#define PLAXY(sysno, name)    WRAPPER_ENTRY_XY(mips_linux, sysno, name)

// This table maps from __NR_xxx syscall numbers (from
// linux/include/asm-mips/unistd.h) to the appropriate PRE/POST sys_foo()
// wrappers on mips (as per sys_call_table in linux/arch/mips/kernel/entry.S).
//

// For those syscalls not handled by Valgrind, the annotation indicate its
// arch/OS combination, eg. */* (generic), */Linux (Linux only), ?/?
// (unknown).

static SyscallTableEntry syscall_main_table[] = {
   LINXY (__NR_io_setup,               sys_io_setup),
   LINX_ (__NR_io_destroy,             sys_io_destroy),
   LINX_ (__NR_io_submit,              sys_io_submit),
   LINXY (__NR_io_cancel,              sys_io_cancel),
   LINXY (__NR_io_getevents,           sys_io_getevents),
   LINX_ (__NR_setxattr,               sys_setxattr),
   LINX_ (__NR_lsetxattr,              sys_lsetxattr),
   LINX_ (__NR_fsetxattr,              sys_fsetxattr),
   LINXY (__NR_getxattr,               sys_getxattr),
   LINXY (__NR_lgetxattr,              sys_lgetxattr),
   LINXY (__NR_fgetxattr,              sys_fgetxattr),
   LINXY (__NR_listxattr,              sys_listxattr),
   LINXY (__NR_llistxattr,             sys_llistxattr),
   LINXY (__NR_flistxattr,             sys_flistxattr),
   LINX_ (__NR_removexattr,            sys_removexattr),
   LINX_ (__NR_lremovexattr,           sys_lremovexattr),
   LINX_ (__NR_fremovexattr,           sys_fremovexattr),
   GENXY (__NR_getcwd,                 sys_getcwd),
   LINXY (__NR_lookup_dcookie,         sys_lookup_dcookie),
   LINXY (__NR_eventfd2,               sys_eventfd2),
   LINXY (__NR_epoll_create1,          sys_epoll_create1),
   LINX_ (__NR_epoll_ctl,              sys_epoll_ctl),
   LINXY (__NR_epoll_pwait,            sys_epoll_pwait),
   GENXY (__NR_dup,                    sys_dup),
   LINXY (__NR_dup3,                   sys_dup3),
   LINXY (__NR_fcntl64,                sys_fcntl64),
   LINXY (__NR_inotify_init1,          sys_inotify_init1),
   LINX_ (__NR_inotify_add_watch,      sys_inotify_add_watch),
   LINX_ (__NR_inotify_rm_watch,       sys_inotify_rm_watch),
   LINXY (__NR_ioctl,                  sys_ioctl),
   LINX_ (__NR_ioprio_set,             sys_ioprio_set),
   LINX_ (__NR_ioprio_get,             sys_ioprio_get),
   GENX_ (__NR_flock,                  sys_flock),
   LINX_ (__NR_mknodat,                sys_mknodat),
   LINX_ (__NR_mkdirat,                sys_mkdirat),
   LINX_ (__NR_unlinkat,               sys_unlinkat),
   LINX_ (__NR_symlinkat,              sys_symlinkat),
   LINX_ (__NR_linkat,                 sys_linkat),
   LINX_ (__NR_umount2,                sys_umount),
   LINX_ (__NR_mount,                  sys_mount),
   LINX_ (__NR_pivot_root,             sys_pivot_root),
   GENX_ (__NR_truncate64,             sys_truncate64),
   GENX_ (__NR_ftruncate64,            sys_ftruncate64),
   LINX_ (__NR_fallocate,              sys_fallocate),
   LINX_ (__NR_faccessat,              sys_faccessat),
   GENX_ (__NR_chdir,                  sys_chdir),
   GENX_ (__NR_fchdir,                 sys_fchdir),
   GENX_ (__NR_chroot,                 sys_chroot),
   GENX_ (__NR_fchmod,                 sys_fchmod),
   LINX_ (__NR_fchmodat,               sys_fchmodat),
   LINX_ (__NR_fchownat,               sys_fchownat),
   GENX_ (__NR_fchown,                 sys_fchown),
   LINXY (__NR_openat,                 sys_openat),
   GENX_ (__NR_close,                  sys_close),
   LINX_ (__NR_vhangup,                sys_vhangup),
   LINXY (__NR_pipe2,                  sys_pipe2),
   LINX_ (__NR_quotactl,               sys_quotactl),
   GENXY (__NR_getdents64,             sys_getdents64),
   LINXY (__NR__llseek,                sys_llseek),
   GENXY (__NR_read,                   sys_read),
   GENX_ (__NR_write,                  sys_write),
   GENXY (__NR_readv,                  sys_readv),
   GENX_ (__NR_writev,                 sys_writev),
   GENXY (__NR_pread64,                sys_pread64),
   GENX_ (__NR_pwrite64,               sys_pwrite64),
   LINXY (__NR_preadv,                 sys_preadv),
   LINX_ (__NR_pwritev,                sys_pwritev),
   LINXY (__NR_sendfile64,             sys_sendfile64),
   LINXY (__NR_pselect6,               sys_pselect6),
   LINXY (__NR_ppoll,                  sys_ppoll),
   LINXY (__NR_signalfd4,              sys_signalfd4),
   LINX_ (__NR_vmsplice,               sys_vmsplice),
   LINX_ (__NR_splice,                 sys_splice),
   LINX_ (__NR_tee,                    sys_tee),
   LINXY (__NR_readlinkat,             sys_readlinkat),
   GENX_ (__NR_sync,                   sys_sync),
   GENX_ (__NR_fsync,                  sys_fsync),
   GENX_ (__NR_fdatasync,              sys_fdatasync),
   LINX_ (__NR_sync_file_range2,       sys_sync_file_range2),
   LINXY (__NR_timerfd_create,         sys_timerfd_create),
   LINXY (__NR_timerfd_settime,        sys_timerfd_settime),
   LINXY (__NR_timerfd_gettime,        sys_timerfd_gettime),
   LINX_ (__NR_utimensat,              sys_utimensat),
   GENX_ (__NR_acct,                   sys_acct),
   LINXY (__NR_capget,                 sys_capget),
   LINX_ (__NR_capset,                 sys_capset),
   LINX_ (__NR_personality,            sys_personality),
   GENX_ (__NR_exit,                   sys_exit),
   LINX_ (__NR_exit_group,             sys_exit_group),
   LINXY (__NR_waitid,                 sys_waitid),
   LINX_ (__NR_set_tid_address,        sys_set_tid_address),
   PLAX_ (__NR_unshare,                sys_unshare),
   LINXY (__NR_futex,                  sys_futex),
   LINX_ (__NR_set_robust_list,        sys_set_robust_list),
   LINXY (__NR_get_robust_list,        sys_get_robust_list),
   GENXY (__NR_nanosleep,              sys_nanosleep),
   GENXY (__NR_getitimer,              sys_getitimer),
   GENXY (__NR_setitimer,              sys_setitimer),
   GENX_ (__NR_kexec_load,             sys_ni_syscall),
   LINX_ (__NR_init_module,            sys_init_module),
   LINX_ (__NR_delete_module,          sys_delete_module),
   LINXY (__NR_timer_create,           sys_timer_create),
   LINXY (__NR_timer_gettime,          sys_timer_gettime),
   LINX_ (__NR_timer_getoverrun,       sys_timer_getoverrun),
   LINXY (__NR_timer_settime,          sys_timer_settime),
   LINX_ (__NR_timer_delete,           sys_timer_delete),
   LINX_ (__NR_clock_settime,          sys_clock_settime),
   LINXY (__NR_clock_gettime,          sys_clock_gettime),
   LINXY (__NR_clock_getres,           sys_clock_getres),
   LINXY (__NR_clock_nanosleep,        sys_clock_nanosleep),
   LINXY (__NR_syslog,                 sys_syslog),
   PLAXY (__NR_ptrace,                 sys_ptrace),
   LINXY (__NR_sched_setparam,         sys_sched_setparam),
   LINX_ (__NR_sched_setscheduler,     sys_sched_setscheduler),
   LINX_ (__NR_sched_getscheduler,     sys_sched_getscheduler),
   LINXY (__NR_sched_getparam,         sys_sched_getparam),
   LINX_ (__NR_sched_setaffinity,      sys_sched_setaffinity),
   LINXY (__NR_sched_getaffinity,      sys_sched_getaffinity),
   LINX_ (__NR_sched_yield,            sys_sched_yield),
   LINX_ (__NR_sched_get_priority_max, sys_sched_get_priority_max),
   LINX_ (__NR_sched_get_priority_min, sys_sched_get_priority_min),
   LINX_ (__NR_sched_rr_get_interval,  sys_sched_rr_get_interval),
   GENX_ (__NR_kill,                   sys_kill),
   LINXY (__NR_tkill,                  sys_tkill),
   LINXY (__NR_tgkill,                 sys_tgkill),
   GENXY (__NR_sigaltstack,            sys_sigaltstack),
   LINX_ (__NR_rt_sigsuspend,          sys_rt_sigsuspend),
   LINXY (__NR_rt_sigaction,           sys_rt_sigaction),
   LINXY (__NR_rt_sigprocmask,         sys_rt_sigprocmask),
   LINXY (__NR_rt_sigpending,          sys_rt_sigpending),
   LINXY (__NR_rt_sigtimedwait,        sys_rt_sigtimedwait),
   LINXY (__NR_rt_sigqueueinfo,        sys_rt_sigqueueinfo),
   PLAX_ (__NR_rt_sigreturn,           sys_rt_sigreturn),
   GENX_ (__NR_setpriority,            sys_setpriority),
   GENX_ (__NR_getpriority,            sys_getpriority),
   PLAX_ (__NR_reboot,                 sys_reboot),
   GENX_ (__NR_setregid,               sys_setregid),
   GENX_ (__NR_setgid,                 sys_setgid),
   GENX_ (__NR_setreuid,               sys_setreuid),
   GENX_ (__NR_setuid,                 sys_setuid),
   LINX_ (__NR_setresuid,              sys_setresuid),
   LINXY (__NR_getresuid,              sys_getresuid),
   LINX_ (__NR_setresgid,              sys_setresgid),
   LINXY (__NR_getresgid,              sys_getresgid),
   LINX_ (__NR_setfsuid,               sys_setfsuid),
   LINX_ (__NR_setfsgid,               sys_setfsgid),
   GENXY (__NR_times,                  sys_times),
   GENX_ (__NR_setpgid,                sys_setpgid),
   GENX_ (__NR_getpgid,                sys_getpgid),
   GENX_ (__NR_getsid,                 sys_getsid),
   GENX_ (__NR_setsid,                 sys_setsid),
   GENXY (__NR_getgroups,              sys_getgroups),
   GENX_ (__NR_setgroups,              sys_setgroups),
   GENXY (__NR_uname,                  sys_newuname),
   PLAX_ (__NR_sethostname,            sys_sethostname),
   PLAX_ (__NR_setdomainname,          sys_setdomainname),
   GENXY (__NR_getrusage,              sys_getrusage),
   GENX_ (__NR_umask,                  sys_umask),
   LINXY (__NR_prctl,                  sys_prctl),
   LINXY (__NR_getcpu,                 sys_getcpu),
   GENXY (__NR_gettimeofday,           sys_gettimeofday),
   GENX_ (__NR_settimeofday,           sys_settimeofday),
   LINXY (__NR_adjtimex,               sys_adjtimex),
   GENX_ (__NR_getpid,                 sys_getpid),
   GENX_ (__NR_getppid,                sys_getppid),
   GENX_ (__NR_getuid,                 sys_getuid),
   GENX_ (__NR_geteuid,                sys_geteuid),
   GENX_ (__NR_getgid,                 sys_getgid),
   GENX_ (__NR_getegid,                sys_getegid),
   LINX_ (__NR_gettid,                 sys_gettid),
   LINXY (__NR_sysinfo,                sys_sysinfo),
   LINXY (__NR_mq_open,                sys_mq_open),
   LINX_ (__NR_mq_unlink,              sys_mq_unlink),
   LINX_ (__NR_mq_timedsend,           sys_mq_timedsend),
   LINXY (__NR_mq_timedreceive,        sys_mq_timedreceive),
   LINX_ (__NR_mq_notify,              sys_mq_notify),
   LINXY (__NR_mq_getsetattr,          sys_mq_getsetattr),
   LINX_ (__NR_msgget,                 sys_msgget),
   LINXY (__NR_msgctl,                 sys_msgctl),
   LINXY (__NR_msgrcv,                 sys_msgrcv),
   LINX_ (__NR_msgsnd,                 sys_msgsnd),
   LINX_ (__NR_semget,                 sys_semget),
   LINXY (__NR_semctl,                 sys_semctl),
   LINX_ (__NR_semtimedop,             sys_semtimedop),
   LINX_ (__NR_semop,                  sys_semop),
   LINX_ (__NR_shmget,                 sys_shmget),
   LINXY (__NR_shmctl,                 sys_shmctl),
   LINXY (__NR_shmat,                  sys_shmat),
   LINXY (__NR_shmdt,                  sys_shmdt),
   LINXY (__NR_socket,                 sys_socket),
   LINXY (__NR_socketpair,             sys_socketpair),
   LINX_ (__NR_bind,                   sys_bind),
   LINX_ (__NR_listen,                 sys_listen),
   LINXY (__NR_accept,                 sys_accept),
   LINX_ (__NR_connect,                sys_connect),
   LINXY (__NR_getsockname,            sys_getsockname),
   LINXY (__NR_getpeername,            sys_getpeername),
   LINX_ (__NR_sendto,                 sys_sendto),
   LINXY (__NR_recvfrom,               sys_recvfrom),
   LINX_ (__NR_setsockopt,             sys_setsockopt),
   LINXY (__NR_getsockopt,             sys_getsockopt),
   LINX_ (__NR_shutdown,               sys_shutdown),
   LINX_ (__NR_sendmsg,                sys_sendmsg),
   LINXY (__NR_recvmsg,                sys_recvmsg),
   LINX_ (__NR_readahead,              sys_readahead),
   GENX_ (__NR_brk,                    sys_brk),
   GENXY (__NR_munmap,                 sys_munmap),
   GENX_ (__NR_mremap,                 sys_mremap),
   LINX_ (__NR_add_key,                sys_add_key),
   LINX_ (__NR_request_key,            sys_request_key),
   LINXY (__NR_keyctl,                 sys_keyctl),
   LINX_ (__NR_clone,                  sys_clone),
   GENX_ (__NR_execve,                 sys_execve),
   PLAX_ (__NR_mmap2,                  sys_mmap2),
   LINX_ (__NR_fadvise64_64,           sys_fadvise64_64),
   PLAX_ (__NR_swapon,                 sys_swapon),
   PLAX_ (__NR_swapoff,                sys_swapoff),
   GENXY (__NR_mprotect,               sys_mprotect),
   GENX_ (__NR_msync,                  sys_msync),
   GENX_ (__NR_mlock,                  sys_mlock),
   GENX_ (__NR_munlock,                sys_munlock),
   GENX_ (__NR_mlockall,               sys_mlockall),
   LINX_ (__NR_munlockall,             sys_munlockall),
   GENXY (__NR_mincore,                sys_mincore),
   GENX_ (__NR_madvise,                sys_madvise),
   LINX_ (__NR_mbind,                  sys_mbind),
   LINXY (__NR_get_mempolicy,          sys_get_mempolicy),
   LINX_ (__NR_set_mempolicy,          sys_set_mempolicy),
   LINXY (__NR_move_pages,             sys_move_pages),
   LINXY (__NR_rt_tgsigqueueinfo,      sys_rt_tgsigqueueinfo),
   LINXY (__NR_perf_event_open,        sys_perf_event_open),
   LINXY (__NR_accept4,                sys_accept4),
   LINXY (__NR_recvmmsg,               sys_recvmmsg),
   PLAX_ (__NR_set_thread_area,        sys_set_thread_area),
   GENXY (__NR_wait4,                  sys_wait4),
   LINXY (__NR_prlimit64,              sys_prlimit64),
   LINXY (__NR_fanotify_init,          sys_fanotify_init),
   LINX_ (__NR_fanotify_mark,          sys_fanotify_mark),
   LINXY (__NR_name_to_handle_at,      sys_name_to_handle_at),
   LINXY (__NR_open_by_handle_at,      sys_open_by_handle_at),
   LINXY (__NR_clock_adjtime,          sys_clock_adjtime),
   LINX_ (__NR_syncfs,                 sys_syncfs),
   LINXY (__NR_sendmmsg,               sys_sendmmsg),
   LINXY (__NR_process_vm_readv,       sys_process_vm_readv),
   LINX_ (__NR_process_vm_writev,      sys_process_vm_writev),
   LINX_ (__NR_kcmp,                   sys_kcmp),
   LINX_ (__NR_renameat2,              sys_renameat2),
   LINX_ (__NR_sched_setattr,          sys_sched_setattr),
   LINXY (__NR_sched_getattr,          sys_sched_getattr),
   LINXY (__NR_getrandom,              sys_getrandom),
   LINXY (__NR_memfd_create,           sys_memfd_create),
   LINXY (__NR_statx,                  sys_statx),
   LINX_ (__NR_setns,                  sys_setns),
   //    (__NR_bpf,                    sys_ni_syscall),
   LINX_ (__NR_execveat,               sys_execveat),
   //    (__NR_userfaultfd,            sys_ni_syscall),
   LINX_ (__NR_membarrier,             sys_membarrier),
   GENX_ (__NR_mlock2,                 sys_mlock2),
   //    (__NR_copy_file_range,        sys_ni_syscall),
   //    (__NR_preadv2,                sys_ni_syscall),
   //    (__NR_pwritev2,               sys_ni_syscall),
   //    (__NR_pkey_mprotect,          sys_ni_syscall),
   //    (__NR_pkey_alloc,             sys_ni_syscall),
   //    (__NR_pkey_free,              sys_ni_syscall),
   LINXY (__NR_io_uring_setup,         sys_io_uring_setup),
   LINXY (__NR_io_uring_enter,         sys_io_uring_enter),
   LINXY (__NR_io_uring_register,      sys_io_uring_register),
   LINXY (__NR_open_tree,              sys_open_tree),
   LINX_ (__NR_move_mount,             sys_move_mount),
   LINXY (__NR_fsopen,                 sys_fsopen),
   LINX_ (__NR_fsconfig,               sys_fsconfig),
   LINXY (__NR_fsmount,                sys_fsmount),
   LINXY (__NR_fspick,                 sys_fspick),
   LINXY (__NR_pidfd_open,             sys_pidfd_open),
   GENX_ (__NR_clone3,                 sys_ni_syscall),
   LINXY (__NR_close_range,            sys_close_range),
   LINXY(__NR_openat2,                 sys_openat2),
   LINXY(__NR_pidfd_getfd,             sys_pidfd_getfd),
   LINX_ (__NR_faccessat2,             sys_faccessat2),
   LINXY (__NR_epoll_pwait2,           sys_epoll_pwait2),
   LINXY (__NR_landlock_create_ruleset,sys_landlock_create_ruleset),
   LINX_ (__NR_landlock_add_rule,      sys_landlock_add_rule),
   LINX_ (__NR_landlock_restrict_self, sys_landlock_restrict_self),
   LINX_ (__NR_fchmodat2,              sys_fchmodat2),
};

SyscallTableEntry* ML_(get_linux_syscall_entry) (UInt sysno)
{
   const UInt syscall_main_table_size
      = sizeof (syscall_main_table) / sizeof (syscall_main_table[0]);

   /* Is it in the contiguous initial section of the table? */
   if (sysno < syscall_main_table_size) {
      SyscallTableEntry * sys = &syscall_main_table[sysno];

      if (sys->before == NULL)
         return NULL;  /* No entry. */
      else
         return sys;
   }

   /* Can't find a wrapper. */
   return NULL;
}

#endif // defined(VGP_nanomips_linux)

/*--------------------------------------------------------------------*/
/*--- end                                 syswrap-nanomips-linux.c ---*/
/*--------------------------------------------------------------------*/
