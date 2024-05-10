
/*--------------------------------------------------------------------*/
/*--- Platform-specific syscalls stuff.    syswrap-mips32-linux.c ----*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2010-2017 RT-RK

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

#if defined(VGP_mips32_linux)
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
".globl vgModuleLocal_call_on_new_stack_0_1\n"
"vgModuleLocal_call_on_new_stack_0_1:\n" 
"   move  $29, $4\n\t"  // stack to %sp
"   move  $31, $5\n\t"  // retaddr to $ra
"   move  $25, $6\n\t"  // f to t9/$25
"   move  $4, $7\n\t"   // arg1 to $a0
"   li    $2, 0\n\t"    // zero all GP regs
"   li    $3, 0\n\t"
"   li    $5, 0\n\t"
"   li    $6, 0\n\t"
"   li    $7, 0\n\t"
"   li    $12, 0\n\t"
"   li    $13, 0\n\t"
"   li    $14, 0\n\t"
"   li    $15, 0\n\t"
"   li    $16, 0\n\t"
"   li    $17, 0\n\t"
"   li    $18, 0\n\t"
"   li    $19, 0\n\t"
"   li    $20, 0\n\t"
"   li    $21, 0\n\t"
"   li    $22, 0\n\t"
"   li    $23, 0\n\t"
"   li    $24, 0\n\t"
"   jr    $25\n\t"      // jump to dst
"   break 0x7\n"        // should never get here
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
            void* arg           in  $a3       12
            pid_t* child_tid    in  stack     16
            pid_t* parent_tid   in  stack     20
            void* tls_ptr       in  stack     24

        System call requires:
            int    $__NR_clone  in $v0
            int    flags        in $a0   0
            void*  child_stack  in $a1   4
            pid_t* parent_tid   in $a2   8
            void*  tls_ptr      in $a3   12
            pid_t* child_tid    in stack 16

   int clone(int (*fn)(void *arg), void *child_stack, int flags, void *arg,
             void *parent_tidptr, void *tls, void *child_tidptr) 

   Returns an Int encoded in the linux-mips way, not a SysRes.
 */ 
#define __NR_CLONE        VG_STRINGIFY(__NR_clone)
#define __NR_EXIT         VG_STRINGIFY(__NR_exit)

// See priv_syswrap-linux.h for arg profile.
asm (
".text\n" 
"   .globl   do_syscall_clone_mips_linux\n" 
"   do_syscall_clone_mips_linux:\n"
"   subu    $29,$29,32\n\t"
"   sw $31, 0($29)\n\t"
"   sw $2, 4($29)\n\t"
"   sw $3, 8($29)\n\t"
"   sw $30, 12($29)\n\t"
"   sw $28, 28($29)\n\t"
    /* set up child stack with function and arg */
    /* syscall arg 2 child_stack is already in a1 */
"   subu $5, $5, 32\n\t" /* make space on stack */
"   sw $4, 0($5)\n\t" /* fn  */
"   sw $7, 4($5)\n\t" /* fn arg */
"   sw $6, 8($5)\n\t"
    /* get other args to clone */

"   move $4, $a2\n\t" /* a0 = flags */
"   lw $6,  52($29)\n\t" /* a2 = parent_tid */
"   lw $7,  48($29)\n\t" /* a3 = child_tid */
"   sw $7,  16($29)\n\t" /* 16(sp) = child_tid */
"   lw $7,  56($29)\n\t" /* a3 = tls_ptr */  
    /* do the system call */

"   li $2, " __NR_CLONE "\n\t" /* __NR_clone */
"   syscall\n\t"
"   nop\n\t"

"   bnez    $7, .Lerror\n\t" 
"   nop\n\t" 
"   beqz    $2, .Lstart\n\t" 
"   nop\n\t" 

"   lw      $31, 0($sp)\n\t" 
"   nop\n\t" 
"   lw      $30, 12($sp)\n\t" 
"   nop\n\t" 
"   addu    $29,$29,32\n\t" /* free stack */  
"   nop\n\t" 
"   jr      $31\n\t" 
"   nop\n\t" 

".Lerror:\n\t" 
"   li      $31, 5\n\t" 
"   jr      $31\n\t" 
"   nop\n\t" 

".Lstart:\n\t" 
"   lw      $4,  4($29)\n\t" 
"   nop\n\t" 
"   lw      $25, 0($29)\n\t" 
"   nop\n\t" 
"   jalr    $25\n\t" 
"   nop\n\t" 

"   move $4, $2\n\t" /* retval from fn is in $v0 */  
"   li $2, " __NR_EXIT "\n\t" /* NR_exit */  
"   syscall\n\t" 
"   nop\n\t" 
"   .previous\n" 
);

#undef __NR_CLONE
#undef __NR_EXIT

// forward declarations
static SysRes sys_set_tls (ThreadId tid, Addr tlsptr);
static SysRes mips_PRE_sys_mmap (ThreadId tid,
                                 UWord arg1, UWord arg2, UWord arg3,
                                 UWord arg4, UWord arg5, Off64T arg6);
/* ---------------------------------------------------------------------
   More thread stuff
   ------------------------------------------------------------------ */ 

// MIPS doesn't have any architecture specific thread stuff that
// needs to be cleaned up da li ????!!!!???
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
static SysRes mips_PRE_sys_mmap(ThreadId tid,
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
   } else
   if (arg1 != 0) {
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
//DECL_TEMPLATE (mips_linux, sys_syscall);
DECL_TEMPLATE (mips_linux, sys_mmap);
DECL_TEMPLATE (mips_linux, sys_mmap2);
DECL_TEMPLATE (mips_linux, sys_stat64);
DECL_TEMPLATE (mips_linux, sys_lstat64);
DECL_TEMPLATE (mips_linux, sys_fadvise64);
DECL_TEMPLATE (mips_linux, sys_fstatat64);
DECL_TEMPLATE (mips_linux, sys_fstat64);
DECL_TEMPLATE (mips_linux, sys_sigreturn);
DECL_TEMPLATE (mips_linux, sys_rt_sigreturn);
DECL_TEMPLATE (mips_linux, sys_cacheflush);
DECL_TEMPLATE (mips_linux, sys_set_thread_area);
DECL_TEMPLATE (mips_linux, sys_pipe);
DECL_TEMPLATE (mips_linux, sys_prctl);
DECL_TEMPLATE (mips_linux, sys_ptrace);
DECL_TEMPLATE (mips_linux, sys_sync_file_range);

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
  r = mips_PRE_sys_mmap(tid, ARG1, ARG2, ARG3, ARG4, ARG5,
                        4096 * (Off64T) ARG6);
  SET_STATUS_from_SysRes(r);
} 

PRE(sys_mmap) 
{
  SysRes r;
  PRINT("sys_mmap ( %#lx, %lu, %ld, %ld, %ld, %lu )",
        ARG1, ARG2, SARG3, SARG4, SARG5, ARG6);
  PRE_REG_READ6(long, "mmap", unsigned long, start, vki_size_t, length,
                int, prot, int, flags, int, fd, unsigned long, offset);
  r = mips_PRE_sys_mmap(tid, ARG1, ARG2, ARG3, ARG4, ARG5, (Off64T) ARG6);
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

// XXX: lstat64/fstat64/stat64 are generic, but not necessarily
// applicable to every architecture -- I think only to 32-bit archs.
// We're going to need something like linux/core_os32.h for such
// things, eventually, I think.  --njn
 
PRE(sys_lstat64) 
{
  PRINT ("sys_lstat64 ( %#lx(%s), %#lx )", ARG1, (HChar *) ARG1, ARG2);
  PRE_REG_READ2 (long, "lstat64", char *, file_name, struct stat64 *, buf);
  PRE_MEM_RASCIIZ ("lstat64(file_name)", ARG1);
  PRE_MEM_WRITE ("lstat64(buf)", ARG2, sizeof (struct vki_stat64));
} 

POST(sys_lstat64) 
{
  vg_assert (SUCCESS);
  if (RES == 0)
    {
      POST_MEM_WRITE (ARG2, sizeof (struct vki_stat64));
    }
} 

PRE(sys_stat64) 
{
  PRINT ("sys_stat64 ( %#lx(%s), %#lx )", ARG1, (HChar *) ARG1, ARG2);
  PRE_REG_READ2 (long, "stat64", char *, file_name, struct stat64 *, buf);
  PRE_MEM_RASCIIZ ("stat64(file_name)", ARG1);
  PRE_MEM_WRITE ("stat64(buf)", ARG2, sizeof (struct vki_stat64));
}

POST(sys_stat64)
{
  POST_MEM_WRITE (ARG2, sizeof (struct vki_stat64));
}

PRE(sys_fadvise64)
{
    PRINT("sys_fadvise64 ( %ld, %llu, %llu, %ld )",
          SARG1, MERGE64(ARG3,ARG4), MERGE64(ARG5, ARG6), SARG7);

   if (VG_(tdict).track_pre_reg_read) {
      PRRSN;
      PRA1("fadvise64", int, fd);
      PRA3("fadvise64", vki_u32, MERGE64_FIRST(offset));
      PRA4("fadvise64", vki_u32, MERGE64_SECOND(offset));
      PRA5("fadvise64", vki_u32, MERGE64_FIRST(len));
      PRA6("fadvise64", vki_u32, MERGE64_SECOND(len));
      PRA7("fadvise64", int, advice);
   }
}

PRE(sys_fstatat64)
{
  // ARG4 =  int flags;  Flags are or'ed together, therefore writing them
  // as a hex constant is more meaningful.
  PRINT("sys_fstatat64 ( %ld, %#lx(%s), %#lx, %#lx )",
        SARG1, ARG2, (HChar*)ARG2, ARG3, ARG4);
  PRE_REG_READ4(long, "fstatat64",
                 int, dfd, char *, file_name, struct stat64 *, buf, int, flags);
  PRE_MEM_RASCIIZ ("fstatat64(file_name)", ARG2);
  PRE_MEM_WRITE ("fstatat64(buf)", ARG3, sizeof (struct vki_stat64));
}

POST(sys_fstatat64)
{
  POST_MEM_WRITE (ARG3, sizeof (struct vki_stat64));
}

PRE(sys_fstat64)
{
  PRINT ("sys_fstat64 ( %ld, %#lx )", SARG1, ARG2);
  PRE_REG_READ2 (long, "fstat64", unsigned long, fd, struct stat64 *, buf);
  PRE_MEM_WRITE ("fstat64(buf)", ARG2, sizeof (struct vki_stat64));
}

POST(sys_fstat64)
{
  POST_MEM_WRITE (ARG2, sizeof (struct vki_stat64));
} 

PRE(sys_sigreturn) 
{
  PRINT ("sys_sigreturn ( )");
  vg_assert (VG_ (is_valid_tid) (tid));
  vg_assert (tid >= 1 && tid < VG_N_THREADS);
  vg_assert (VG_ (is_running_thread) (tid));
  VG_ (sigframe_destroy) (tid, False);
  /* Tell the driver not to update the guest state with the "result",
     and set a bogus result to keep it happy. */ 
  *flags |= SfNoWriteResult;
  SET_STATUS_Success (0);
   /* Check to see if any signals arose as a result of this. */ 
  *flags |= SfPollAfter;
}

PRE(sys_rt_sigreturn) 
{
  PRINT ("rt_sigreturn ( )");
  vg_assert (VG_ (is_valid_tid) (tid));
  vg_assert (tid >= 1 && tid < VG_N_THREADS);
  vg_assert (VG_ (is_running_thread) (tid));
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

/* Very much MIPS specific */
PRE(sys_cacheflush)
{
  PRINT ("cacheflush (%lx, %ld, %ld)", ARG1, SARG2, SARG3);
  PRE_REG_READ3(long, "cacheflush", unsigned long, addr,
                int, nbytes, int, cache);
  VG_ (discard_translations) ((Addr)ARG1, (ULong) ARG2,
                              "PRE(sys_cacheflush)");
  SET_STATUS_Success (0);
}

PRE(sys_pipe)
{
   PRINT("sys_pipe ( %#lx )", ARG1);
   PRE_REG_READ1(int, "pipe", int *, filedes);
   PRE_MEM_WRITE( "pipe(filedes)", ARG1, 2*sizeof(int) );
}

POST(sys_pipe)
{
   Int p0, p1;
   vg_assert(SUCCESS);
   p0 = RES;
   p1 = sr_ResEx(status->sres);

   if (!ML_(fd_allowed)(p0, "pipe", tid, True) ||
       !ML_(fd_allowed)(p1, "pipe", tid, True)) {
      VG_(close)(p0);
      VG_(close)(p1);
      SET_STATUS_Failure( VKI_EMFILE );
   } else {
      if (VG_(clo_track_fds)) {
         ML_(record_fd_open_nameless)(tid, p0);
         ML_(record_fd_open_nameless)(tid, p1);
      }
   }
}

PRE(sys_prctl)
{
   switch (ARG1) {
      case VKI_PR_SET_FP_MODE:
      {
         VexArchInfo vai;
         Int known_bits = VKI_PR_FP_MODE_FR | VKI_PR_FP_MODE_FRE;
         VG_(machine_get_VexArchInfo)(NULL, &vai);
         /* Reject unsupported modes */
         if (ARG2 & ~known_bits) {
            SET_STATUS_Failure(VKI_EOPNOTSUPP);
            return;
         }
         if ((ARG2 & VKI_PR_FP_MODE_FR) && !VEX_MIPS_HOST_FP_MODE(vai.hwcaps)) {
            SET_STATUS_Failure(VKI_EOPNOTSUPP);
            return;
         }
         if ((ARG2 & VKI_PR_FP_MODE_FRE) && !VEX_MIPS_CPU_HAS_MIPSR6(vai.hwcaps)) {
            SET_STATUS_Failure(VKI_EOPNOTSUPP);
            return;
         }
         if (!(ARG2 & VKI_PR_FP_MODE_FR) && VEX_MIPS_CPU_HAS_MIPSR6(vai.hwcaps)) {
            SET_STATUS_Failure(VKI_EOPNOTSUPP);
            return;
         }

         if ((!(VG_(threads)[tid].arch.vex.guest_CP0_status &
             MIPS_CP0_STATUS_FR) != !(ARG2 & VKI_PR_FP_MODE_FR)) ||
            (!(VG_(threads)[tid].arch.vex.guest_CP0_Config5 &
             MIPS_CONF5_FRE) != !(ARG2 & VKI_PR_FP_MODE_FRE))) {
            ThreadId t;
            for (t = 1; t < VG_N_THREADS; t++) {
               if (VG_(threads)[t].status != VgTs_Empty) {
                  if (ARG2 & VKI_PR_FP_MODE_FRE) {
                     VG_(threads)[t].arch.vex.guest_CP0_Config5 |=
                        MIPS_CONF5_FRE;
                  } else {
                     VG_(threads)[t].arch.vex.guest_CP0_Config5 &=
                        ~MIPS_CONF5_FRE;
                  }
                  if (ARG2 & VKI_PR_FP_MODE_FR) {
                     VG_(threads)[t].arch.vex.guest_CP0_status |=
                        MIPS_CP0_STATUS_FR;
                  } else {
                     VG_(threads)[t].arch.vex.guest_CP0_status &=
                        ~MIPS_CP0_STATUS_FR;
                  }
               }
            /* Discard all translations */
            VG_(discard_translations)(0, 0xfffffffful, "prctl(PR_SET_FP_MODE)");
            }
         SET_STATUS_Success(0);
         }
         break;
      }
      case VKI_PR_GET_FP_MODE:
      {
         UInt ret = 0;
         if (VG_(threads)[tid].arch.vex.guest_CP0_status & MIPS_CP0_STATUS_FR)
            ret |= VKI_PR_FP_MODE_FR;
         if (VG_(threads)[tid].arch.vex.guest_CP0_Config5 & MIPS_CONF5_FRE)
            ret |= VKI_PR_FP_MODE_FRE;
         SET_STATUS_Success(ret);
         break;
      }
      default:
         WRAPPER_PRE_NAME(linux, sys_prctl)(tid, layout, arrghs, status, flags);
         break;
   }
}

POST(sys_prctl)
{
   WRAPPER_POST_NAME(linux, sys_prctl)(tid, arrghs, status);
}

PRE(sys_sync_file_range)
{
   *flags |= SfMayBlock;

   PRINT("sys_sync_file_range ( %ld, %llu, %llu, %ld )",
          SARG1, MERGE64(ARG3,ARG4), MERGE64(ARG5, ARG6), SARG7);

   if (VG_(tdict).track_pre_reg_read) {
      PRRSN;
      PRA1("sync_file_range", int, fd);
      PRA3("sync_file_range", vki_u32, MERGE64_FIRST(offset));
      PRA4("sync_file_range", vki_u32, MERGE64_SECOND(offset));
      PRA5("sync_file_range", vki_u32, MERGE64_FIRST(nbytes));
      PRA6("sync_file_range", vki_u32, MERGE64_SECOND(nbytes));
      PRA7("sync_file_range", int, flags);
   }

   if (!ML_(fd_allowed)(ARG1, "sync_file_range", tid, False)){
      SET_STATUS_Failure( VKI_EBADF );
   }
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
   //..    PLAXY (__NR_syscall,                sys_syscall),            // 0
   GENX_ (__NR_exit,                   sys_exit),                    // 1
   GENX_ (__NR_fork,                   sys_fork),                    // 2
   GENXY (__NR_read,                   sys_read),                    // 3
   GENX_ (__NR_write,                  sys_write),                   // 4
   GENXY (__NR_open,                   sys_open),                    // 5
   GENX_ (__NR_close,                  sys_close),                   // 6
   GENXY (__NR_waitpid,                sys_waitpid),                 // 7
   GENXY (__NR_creat,                  sys_creat),                   // 8
   GENX_ (__NR_link,                   sys_link),                    // 9
   GENX_ (__NR_unlink,                 sys_unlink),                  // 10
   GENX_ (__NR_execve,                 sys_execve),                  // 11
   GENX_ (__NR_chdir,                  sys_chdir),                   // 12
   GENXY (__NR_time,                   sys_time),                    // 13
   GENX_ (__NR_mknod,                  sys_mknod),                   // 14
   GENX_ (__NR_chmod,                  sys_chmod),                   // 15
   GENX_ (__NR_lchown,                 sys_lchown),                  // 16
   //..
   LINX_ (__NR_lseek,                  sys_lseek),                   // 19
   GENX_ (__NR_getpid,                 sys_getpid),                  // 20
   LINX_ (__NR_mount,                  sys_mount),                   // 21
   LINX_ (__NR_umount,                 sys_oldumount),               // 22
   GENX_ (__NR_setuid,                 sys_setuid),                  // 23
   GENX_ (__NR_getuid,                 sys_getuid),                  // 24
   LINX_ (__NR_stime,                  sys_stime),                   // 25
   PLAXY(__NR_ptrace,                  sys_ptrace),                  // 26
   GENX_ (__NR_alarm,                  sys_alarm),                   // 27
   //..    //   (__NR_oldfstat,          sys_fstat),  // 28
   GENX_ (__NR_pause,                  sys_pause),                   // 29
   LINX_ (__NR_utime,                  sys_utime),                   // 30
   //..    GENX_(__NR_stty,              sys_ni_syscall),        // 31
   //..    GENX_(__NR_gtty,              sys_ni_syscall),        // 32
   GENX_ (__NR_access,                 sys_access),                  // 33
   //..    GENX_(__NR_nice,              sys_nice),              // 34
   //..    GENX_(__NR_ftime,             sys_ni_syscall),        // 35
   //..    GENX_(__NR_sync,              sys_sync),              // 36
   GENX_ (__NR_kill,                   sys_kill),                    // 37
   GENX_ (__NR_rename,                 sys_rename),                  // 38
   GENX_ (__NR_mkdir,                  sys_mkdir),                   // 39
   GENX_ (__NR_rmdir,                  sys_rmdir),                   // 40
   GENXY (__NR_dup,                    sys_dup),                     // 41
   PLAXY (__NR_pipe,                   sys_pipe),                    // 42
   GENXY (__NR_times,                  sys_times),                   // 43
   //..    GENX_(__NR_prof,              sys_ni_syscall),   // 44
   GENX_ (__NR_brk,                    sys_brk),                     // 45
   GENX_ (__NR_setgid,                 sys_setgid),                  // 46
   GENX_ (__NR_getgid,                 sys_getgid),                  // 47
   //..    //   (__NR_signal,            sys_signal),       // 48
   GENX_ (__NR_geteuid,                sys_geteuid),                 // 49
   GENX_ (__NR_getegid,                sys_getegid),                 // 50
   GENX_ (__NR_acct,                   sys_acct),                    // 51
   LINX_ (__NR_umount2,                sys_umount),                  // 52
   //..    GENX_(__NR_lock,              sys_ni_syscall),   // 53
   LINXY (__NR_ioctl,                  sys_ioctl),                   // 54
   LINXY (__NR_fcntl,                  sys_fcntl),                   // 55
   //..    GENX_(__NR_mpx,               sys_ni_syscall),   // 56
   GENX_ (__NR_setpgid,                sys_setpgid),                 // 57
   //..    GENX_(__NR_ulimit,            sys_ni_syscall),        // 58
   //..    //   (__NR_oldolduname,       sys_olduname),          // 59
   GENX_ (__NR_umask,                  sys_umask),                   // 60
   GENX_ (__NR_chroot,                 sys_chroot),                  // 61
   //..    //   (__NR_ustat,             sys_ustat)              // 62
   GENXY (__NR_dup2,                   sys_dup2),                    // 63
   GENX_ (__NR_getppid,                sys_getppid),                 // 64
   GENX_ (__NR_getpgrp,                sys_getpgrp),                 // 65
   GENX_ (__NR_setsid,                 sys_setsid),                  // 66
   LINXY (__NR_sigaction,              sys_sigaction),               // 67
   //..    //   (__NR_sgetmask,          sys_sgetmask),          // 68
   //..    //   (__NR_ssetmask,          sys_ssetmask),          // 69
   GENX_ (__NR_setreuid,               sys_setreuid),                // 70
   GENX_ (__NR_setregid,               sys_setregid),                // 71
   //   PLAX_(__NR_sigsuspend,        sys_sigsuspend),        // 72
   LINXY (__NR_sigpending,             sys_sigpending),              // 73
   GENX_ (__NR_sethostname,            sys_sethostname),             // 74
   GENX_ (__NR_setrlimit,              sys_setrlimit),               // 75
   GENXY (__NR_getrlimit,              sys_getrlimit),               // 76
   GENXY (__NR_getrusage,              sys_getrusage),               // 77
   GENXY (__NR_gettimeofday,           sys_gettimeofday),            // 78
   GENX_ (__NR_settimeofday,           sys_settimeofday),            // 79
   GENXY (__NR_getgroups,              sys_getgroups),               // 80
   GENX_ (__NR_setgroups,              sys_setgroups),               // 81
   //..    PLAX_(__NR_select,            old_select),            // 82
   GENX_ (__NR_symlink,                sys_symlink),                 // 83
   //..    //   (__NR_oldlstat,          sys_lstat),             // 84
   GENX_ (__NR_readlink,               sys_readlink),                // 85
   //..    //   (__NR_uselib,            sys_uselib),            // 86
   //..    //   (__NR_swapon,            sys_swapon),            // 87
   //..    //   (__NR_reboot,            sys_reboot),            // 88
   //..    //   (__NR_readdir,           old_readdir),           // 89
   PLAX_ (__NR_mmap,                   sys_mmap),                    // 90
   GENXY (__NR_munmap,                 sys_munmap),                  // 91
   GENX_ (__NR_truncate,               sys_truncate),                // 92
   GENX_ (__NR_ftruncate,              sys_ftruncate),               // 93
   GENX_ (__NR_fchmod,                 sys_fchmod),                  // 94
   GENX_ (__NR_fchown,                 sys_fchown),                  // 95
   GENX_ (__NR_getpriority,            sys_getpriority),             // 96
   GENX_ (__NR_setpriority,            sys_setpriority),             // 97
   //..    GENX_(__NR_profil,            sys_ni_syscall),        // 98
   GENXY (__NR_statfs,                 sys_statfs),                  // 99
   GENXY (__NR_fstatfs,                sys_fstatfs),                 // 100
   //..    LINX_(__NR_ioperm,            sys_ioperm),            // 101
   LINXY (__NR_socketcall,             sys_socketcall),              // 102
   LINXY (__NR_syslog,                 sys_syslog),                  // 103
   GENXY (__NR_setitimer,              sys_setitimer),               // 104
   GENXY (__NR_getitimer,              sys_getitimer),               // 105
   GENXY (__NR_stat,                   sys_newstat),                 // 106
   GENXY (__NR_lstat,                  sys_newlstat),                // 107
   GENXY (__NR_fstat,                  sys_newfstat),                // 108
   //..    //   (__NR_olduname,          sys_uname),             // 109
   //..    GENX_(__NR_iopl,              sys_iopl),              // 110
   LINX_ (__NR_vhangup,                sys_vhangup),                 // 111
   //..    GENX_(__NR_idle,              sys_ni_syscall),        // 112
   //..    //   (__NR_vm86old,           sys_vm86old),           // 113
   GENXY (__NR_wait4,                  sys_wait4),                   // 114
   //..    //   (__NR_swapoff,           sys_swapoff),           // 115
   LINXY (__NR_sysinfo,                sys_sysinfo),                 // 116
   LINXY (__NR_ipc,                    sys_ipc),                     // 117
   GENX_ (__NR_fsync,                  sys_fsync),                   // 118
   PLAX_ (__NR_sigreturn,              sys_sigreturn),               // 119
   LINX_ (__NR_clone,                  sys_clone),                   // 120
   //..    //   (__NR_setdomainname,     sys_setdomainname),     // 121
   GENXY (__NR_uname,                  sys_newuname),                // 122
   //..    PLAX_(__NR_modify_ldt,        sys_modify_ldt),        // 123
   LINXY (__NR_adjtimex,               sys_adjtimex),                // 124
   GENXY (__NR_mprotect,               sys_mprotect),                // 125
   LINXY (__NR_sigprocmask,            sys_sigprocmask),             // 126
   //..    GENX_(__NR_create_module,     sys_ni_syscall),        // 127
   //..    GENX_(__NR_init_module,       sys_init_module),       // 128
   //..    //   (__NR_delete_module,     sys_delete_module),     // 129
   //..    GENX_(__NR_get_kernel_syms,   sys_ni_syscall),        // 130
   //..    LINX_(__NR_quotactl,          sys_quotactl),          // 131
   GENX_ (__NR_getpgid,                sys_getpgid),                 // 132
   GENX_ (__NR_fchdir,                 sys_fchdir),                  // 133
   //..    //   (__NR_bdflush,           sys_bdflush),           // 134
   //..    //   (__NR_sysfs,             sys_sysfs),             // 135
   LINX_ (__NR_personality,            sys_personality),            // 136
   //..    GENX_(__NR_afs_syscall,       sys_ni_syscall),        // 137
   LINX_ (__NR_setfsuid,               sys_setfsuid),                // 138
   LINX_ (__NR_setfsgid,               sys_setfsgid),                // 139
   LINXY (__NR__llseek,                sys_llseek),                  // 140
   GENXY (__NR_getdents,               sys_getdents),                // 141
   GENX_ (__NR__newselect,             sys_select),                  // 142
   GENX_ (__NR_flock,                  sys_flock),                   // 143
   GENX_ (__NR_msync,                  sys_msync),                   // 144
   GENXY (__NR_readv,                  sys_readv),                   // 145
   GENX_ (__NR_writev,                 sys_writev),                  // 146
   PLAX_ (__NR_cacheflush,             sys_cacheflush),              // 147
   GENX_ (__NR_getsid,                 sys_getsid),                  // 151
   GENX_ (__NR_fdatasync,              sys_fdatasync),               // 152
   LINXY (__NR__sysctl,                sys_sysctl),                  // 153
   GENX_ (__NR_mlock,                  sys_mlock),                   // 154
   GENX_ (__NR_munlock,                sys_munlock),                 // 155
   GENX_ (__NR_mlockall,               sys_mlockall),                // 156
   LINX_ (__NR_munlockall,             sys_munlockall),              // 157
   LINXY (__NR_sched_setparam,         sys_sched_setparam),          // 158
   LINXY (__NR_sched_getparam,         sys_sched_getparam),          // 159
   LINX_ (__NR_sched_setscheduler,     sys_sched_setscheduler),      // 160
   LINX_ (__NR_sched_getscheduler,     sys_sched_getscheduler),      // 161
   LINX_ (__NR_sched_yield,            sys_sched_yield),             // 162
   LINX_ (__NR_sched_get_priority_max, sys_sched_get_priority_max),  // 163
   LINX_ (__NR_sched_get_priority_min, sys_sched_get_priority_min),  // 164
   LINXY (__NR_sched_rr_get_interval,  sys_sched_rr_get_interval),   // 165
   GENXY (__NR_nanosleep,              sys_nanosleep),               // 166
   GENX_ (__NR_mremap,                 sys_mremap),                  // 167
   LINXY (__NR_accept,                 sys_accept),                  // 168
   LINX_ (__NR_bind,                   sys_bind),                    // 169
   LINX_ (__NR_connect,                sys_connect),                 // 170
   LINXY (__NR_getpeername,            sys_getpeername),             // 171
   LINXY (__NR_getsockname,            sys_getsockname),             // 172
   LINXY (__NR_getsockopt,             sys_getsockopt),              // 173
   LINX_ (__NR_listen,                 sys_listen),                  // 174
   LINXY (__NR_recv,                   sys_recv),                    // 175
   LINXY (__NR_recvfrom,               sys_recvfrom),                // 176
   LINXY (__NR_recvmsg,                sys_recvmsg),                 // 177
   LINX_ (__NR_send,                   sys_send),                    // 178
   LINX_ (__NR_sendmsg,                sys_sendmsg),                 // 179
   LINX_ (__NR_sendto,                 sys_sendto),                  // 180
   LINX_ (__NR_setsockopt,             sys_setsockopt),              // 181
   LINX_ (__NR_shutdown,               sys_shutdown),                // 182
   LINXY (__NR_socket,                 sys_socket),                  // 183
   LINXY (__NR_socketpair,             sys_socketpair),              // 184
   LINX_ (__NR_setresuid,              sys_setresuid),               // 185
   LINXY (__NR_getresuid,              sys_getresuid),               // 186
   //..    GENX_(__NR_query_module,      sys_ni_syscall),        // 187
   GENXY (__NR_poll,                   sys_poll),                    // 188
   //..
   LINX_ (__NR_setresgid,              sys_setresgid),               // 190
   LINXY (__NR_getresgid,              sys_getresgid),               // 191
   PLAXY (__NR_prctl,                  sys_prctl),                   // 192
   PLAX_ (__NR_rt_sigreturn,           sys_rt_sigreturn),            // 193
   LINXY (__NR_rt_sigaction,           sys_rt_sigaction),            // 194
   LINXY (__NR_rt_sigprocmask,         sys_rt_sigprocmask),          // 195
   LINXY (__NR_rt_sigpending,          sys_rt_sigpending),           // 196
   LINXY (__NR_rt_sigtimedwait,        sys_rt_sigtimedwait),         // 197
   LINXY (__NR_rt_sigqueueinfo,        sys_rt_sigqueueinfo),         // 198
   LINX_ (__NR_rt_sigsuspend,          sys_rt_sigsuspend),           // 199
   GENXY (__NR_pread64,                sys_pread64),                 // 200
   GENX_ (__NR_pwrite64,               sys_pwrite64),                // 201
   GENX_ (__NR_chown,                  sys_chown),                   // 202
   GENXY (__NR_getcwd,                 sys_getcwd),                  // 203
   LINXY (__NR_capget,                 sys_capget),                  // 204
   //..    LINX_(__NR_capset,            sys_capset),            // 205
   GENXY (__NR_sigaltstack,            sys_sigaltstack),             // 206
   LINXY (__NR_sendfile,               sys_sendfile),                // 207
   //..    GENXY(__NR_getpmsg,           sys_getpmsg),           // 208
   //..    GENX_(__NR_putpmsg,           sys_putpmsg),           // 209
   PLAX_ (__NR_mmap2,                  sys_mmap2),                   // 210
   GENX_ (__NR_truncate64,             sys_truncate64),              // 211
   GENX_ (__NR_ftruncate64,            sys_ftruncate64),             // 212
   PLAXY (__NR_stat64,                 sys_stat64),                  // 213
   PLAXY (__NR_lstat64,                sys_lstat64),                 // 214
   PLAXY (__NR_fstat64,                sys_fstat64),                 // 215
   //..
   GENXY (__NR_mincore,                sys_mincore),                 // 217
   GENX_ (__NR_madvise,                sys_madvise),                 // 218
   GENXY (__NR_getdents64,             sys_getdents64),              // 219
   LINXY (__NR_fcntl64,                sys_fcntl64),                 // 220
   //..
   LINX_ (__NR_gettid,                 sys_gettid),                  // 222
   //..
   LINX_ (__NR_setxattr,               sys_setxattr),                // 224
   LINX_ (__NR_lsetxattr,              sys_lsetxattr),               // 225
   LINX_ (__NR_fsetxattr,              sys_fsetxattr),               // 226
   LINXY (__NR_getxattr,               sys_getxattr),                // 227
   LINXY (__NR_lgetxattr,              sys_lgetxattr),               // 228
   LINXY (__NR_fgetxattr,              sys_fgetxattr),               // 229
   LINXY (__NR_listxattr,              sys_listxattr),               // 230
   LINXY (__NR_llistxattr,             sys_llistxattr),              // 231
   LINXY (__NR_flistxattr,             sys_flistxattr),              // 232
   LINX_ (__NR_removexattr,            sys_removexattr),             // 233
   LINX_ (__NR_lremovexattr,           sys_lremovexattr),            // 234
   LINX_ (__NR_fremovexattr,           sys_fremovexattr),            // 235
   //..
   LINXY (__NR_sendfile64,             sys_sendfile64),              // 237
   LINXY (__NR_futex,                  sys_futex),                   // 238
   LINX_ (__NR_sched_setaffinity,      sys_sched_setaffinity),       // 239
   LINXY (__NR_sched_getaffinity,      sys_sched_getaffinity),       // 240
   LINX_ (__NR_io_setup,               sys_io_setup),                // 241
   LINX_ (__NR_io_destroy,             sys_io_destroy),              // 242
   LINXY (__NR_io_getevents,           sys_io_getevents),            // 243
   LINX_ (__NR_io_submit,              sys_io_submit),               // 244
   LINXY (__NR_io_cancel,              sys_io_cancel),               // 245
   LINX_ (__NR_exit_group,             sys_exit_group),              // 246
   //..
   LINXY (__NR_epoll_create,           sys_epoll_create),            // 248
   LINX_ (__NR_epoll_ctl,              sys_epoll_ctl),               // 249
   LINXY (__NR_epoll_wait,             sys_epoll_wait),              // 250
   //..
   LINX_ (__NR_set_tid_address,        sys_set_tid_address),         // 252
   PLAX_ (__NR_fadvise64,              sys_fadvise64),               // 254
   GENXY (__NR_statfs64,               sys_statfs64),                // 255
   GENXY (__NR_fstatfs64,              sys_fstatfs64),               // 256
   //..
   LINXY (__NR_timer_create,           sys_timer_create),            // 257
   LINXY (__NR_timer_settime,          sys_timer_settime),           // 258
   LINXY (__NR_timer_gettime,          sys_timer_gettime),           // 259
   LINX_ (__NR_timer_getoverrun,       sys_timer_getoverrun),        // 260
   LINX_ (__NR_timer_delete,           sys_timer_delete),            // 261
   LINX_ (__NR_clock_settime,          sys_clock_settime),           // 262
   LINXY (__NR_clock_gettime,          sys_clock_gettime),           // 263
   LINXY (__NR_clock_getres,           sys_clock_getres),            // 264
   LINXY (__NR_clock_nanosleep,        sys_clock_nanosleep),         // 265
   LINXY (__NR_tgkill,                 sys_tgkill),                  // 266
   GENX_ (__NR_utimes,                 sys_utimes),                  // 267
   LINXY (__NR_get_mempolicy,          sys_get_mempolicy),           // 269
   LINX_ (__NR_set_mempolicy,          sys_set_mempolicy),           // 270
   LINXY (__NR_mq_open,                sys_mq_open),                 // 271
   LINX_ (__NR_mq_unlink,              sys_mq_unlink),               // 272
   LINX_ (__NR_mq_timedsend,           sys_mq_timedsend),            // 273
   LINXY (__NR_mq_timedreceive,        sys_mq_timedreceive),         // 274
   LINX_ (__NR_mq_notify,              sys_mq_notify),               // 275
   LINXY (__NR_mq_getsetattr,          sys_mq_getsetattr),           // 276
   LINXY (__NR_inotify_init,           sys_inotify_init),            // 275
   LINX_ (__NR_inotify_add_watch,      sys_inotify_add_watch),       // 276
   LINX_ (__NR_inotify_rm_watch,       sys_inotify_rm_watch),        // 277
   LINXY (__NR_waitid,                 sys_waitid),                  // 278
   //..
   PLAX_ (__NR_set_thread_area,        sys_set_thread_area),         // 283
   //..
   LINXY (__NR_openat,                 sys_openat),                  // 288
   LINX_ (__NR_mkdirat,                sys_mkdirat),                 // 289
   LINX_ (__NR_mknodat,                sys_mknodat),                 // 290
   LINX_ (__NR_fchownat,               sys_fchownat),                // 291
   LINX_ (__NR_futimesat,              sys_futimesat),               // 292
   PLAXY (__NR_fstatat64,              sys_fstatat64),               // 293
   LINX_ (__NR_unlinkat,               sys_unlinkat),                // 294
   LINX_ (__NR_renameat,               sys_renameat),                // 295
   LINX_ (__NR_linkat,                 sys_linkat),                  // 296
   LINX_ (__NR_symlinkat,              sys_symlinkat),               // 297
   LINX_ (__NR_readlinkat,             sys_readlinkat),              // 298
   LINX_ (__NR_fchmodat,               sys_fchmodat),                // 299
   LINX_ (__NR_faccessat,              sys_faccessat),               // 300
   LINXY (__NR_pselect6,               sys_pselect6),                // 301
   LINXY (__NR_ppoll,                  sys_ppoll),                   // 302
   LINX_ (__NR_unshare,                sys_unshare),                 // 303
   LINX_ (__NR_splice,                 sys_splice),                  // 304
   PLAX_ (__NR_sync_file_range,        sys_sync_file_range),         // 305
   LINX_ (__NR_tee,                    sys_tee),                     // 306
   LINXY (__NR_vmsplice,               sys_vmsplice),                // 307
   //..
   LINX_ (__NR_set_robust_list,        sys_set_robust_list),         // 309
   LINXY (__NR_get_robust_list,        sys_get_robust_list),         // 310
   //..
   LINXY (__NR_getcpu,                 sys_getcpu),                  // 312
   LINXY (__NR_epoll_pwait,            sys_epoll_pwait),             // 313
   //..
   LINX_ (__NR_utimensat,              sys_utimensat),               // 316
   //..
   LINX_ (__NR_fallocate,              sys_fallocate),               // 320
   LINXY (__NR_timerfd_create,         sys_timerfd_create),          // 321
   LINXY (__NR_timerfd_gettime,        sys_timerfd_gettime),         // 322
   LINXY (__NR_timerfd_settime,        sys_timerfd_settime),         // 323
   LINXY (__NR_signalfd4,              sys_signalfd4),               // 324
   LINXY (__NR_eventfd2,               sys_eventfd2),                // 325
   //..
   LINXY (__NR_pipe2,                  sys_pipe2),                   // 328
   LINXY (__NR_inotify_init1,          sys_inotify_init1),           // 329
   LINXY (__NR_preadv,                 sys_preadv),                  // 330
   LINX_ (__NR_pwritev,                sys_pwritev),                 // 331
   //..
   LINXY (__NR_prlimit64,              sys_prlimit64),               // 338
   //..
   LINXY (__NR_clock_adjtime,          sys_clock_adjtime),           // 341
   LINX_ (__NR_syncfs,                 sys_syncfs),                  // 342
   LINX_ (__NR_setns,                  sys_setns),                   // 343
   //..
   LINXY (__NR_process_vm_readv,       sys_process_vm_readv),        // 345
   LINX_ (__NR_process_vm_writev,      sys_process_vm_writev),       // 346
   //..
   LINX_(__NR_sched_setattr,           sys_sched_setattr),           // 349
   LINXY(__NR_sched_getattr,           sys_sched_getattr),           // 350
   LINX_(__NR_renameat2,               sys_renameat2),               // 351
   //..
   LINXY (__NR_getrandom,              sys_getrandom),               // 353
   LINXY (__NR_memfd_create,           sys_memfd_create),            // 354
   //..
   LINX_ (__NR_execveat,               sys_execveat),                // 356
   //..
   LINX_ (__NR_membarrier,             sys_membarrier),              // 358
   GENX_ (__NR_mlock2,                 sys_mlock2),                  // 359
   LINX_ (__NR_copy_file_range,        sys_copy_file_range),         // 360
   LINXY (__NR_preadv2,                sys_preadv2),                 // 361
   LINX_ (__NR_pwritev2,               sys_pwritev2),                // 362
   //..
   LINXY(__NR_statx,                   sys_statx),                   // 366
   GENX_(__NR_rseq,                    sys_ni_syscall),              // 367

   LINXY(__NR_clock_gettime64,         sys_clock_gettime64),         // 403
   LINX_(__NR_clock_settime64,         sys_clock_settime64),         // 404

   LINXY(__NR_clock_getres_time64,     sys_clock_getres_time64),     // 406
   LINXY(__NR_clock_nanosleep_time64,  sys_clock_nanosleep_time64),  // 407
   LINXY(__NR_timer_gettime64,         sys_timer_gettime64),         // 408
   LINXY(__NR_timer_settime64,         sys_timer_settime64),         // 409
   LINXY(__NR_timerfd_gettime64,       sys_timerfd_gettime64),       // 410
   LINXY(__NR_timerfd_settime64,       sys_timerfd_settime64),       // 411
   LINX_(__NR_utimensat_time64,        sys_utimensat_time64),        // 412
   LINXY(__NR_pselect6_time64,         sys_pselect6_time64),         // 413
   LINXY(__NR_ppoll_time64,            sys_ppoll_time64),            // 414

   LINXY(__NR_recvmmsg_time64,         sys_recvmmsg_time64),         // 417
   LINX_(__NR_mq_timedsend_time64,     sys_mq_timedsend_time64),     // 418
   LINXY(__NR_mq_timedreceive_time64,  sys_mq_timedreceive_time64),  // 419
   LINX_(__NR_semtimedop_time64,       sys_semtimedop_time64),       // 420
   LINXY(__NR_rt_sigtimedwait_time64,  sys_rt_sigtimedwait_time64),  // 421
   LINXY(__NR_futex_time64,            sys_futex_time64),            // 422
   LINXY(__NR_sched_rr_get_interval_time64,
         sys_sched_rr_get_interval_time64),                          // 423

   LINXY(__NR_io_uring_setup,          sys_io_uring_setup),          // 425
   LINXY(__NR_io_uring_enter,          sys_io_uring_enter),          // 426
   LINXY(__NR_io_uring_register,       sys_io_uring_register),       // 427

   LINXY(__NR_pidfd_open,              sys_pidfd_open),              // 434
   GENX_(__NR_clone3,                  sys_ni_syscall),              // 435
   LINXY(__NR_close_range,       sys_close_range),       // 436
   LINXY(__NR_openat2,           sys_openat2),           // 437
   LINXY(__NR_pidfd_getfd,             sys_pidfd_getfd),             // 438
   LINX_ (__NR_faccessat2,             sys_faccessat2),              // 439

   LINXY(__NR_epoll_pwait2,      sys_epoll_pwait2),      // 441

   LINX_(__NR_fchmodat2,               sys_fchmodat2),               // 452
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

#endif // defined(VGP_mips32_linux)

/*--------------------------------------------------------------------*/ 
/*--- end                                     syswrap-mips-linux.c ---*/ 
/*--------------------------------------------------------------------*/ 
