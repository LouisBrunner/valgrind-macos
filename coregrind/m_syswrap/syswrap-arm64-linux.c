
/*--------------------------------------------------------------------*/
/*--- Platform-specific syscalls stuff.    syswrap-arm64-linux.c -----*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2013-2013 OpenWorks
      info@open-works.net

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

#if defined(VGP_arm64_linux)

#include "pub_core_basics.h"
#include "pub_core_vki.h"
#include "pub_core_vkiscnums.h"
#include "pub_core_threadstate.h"
#include "pub_core_aspacemgr.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcprint.h"
#include "pub_core_libcsignal.h"
#include "pub_core_options.h"
#include "pub_core_scheduler.h"
#include "pub_core_sigframe.h"      // For VG_(sigframe_destroy)()
#include "pub_core_syscall.h"
#include "pub_core_syswrap.h"
#include "pub_core_tooliface.h"

#include "priv_types_n_macros.h"
#include "priv_syswrap-generic.h"   /* for decls of generic wrappers */
#include "priv_syswrap-linux.h"     /* for decls of linux-ish wrappers */


/* ---------------------------------------------------------------------
   clone() handling
   ------------------------------------------------------------------ */

/* Call f(arg1), but first switch stacks, using 'stack' as the new
   stack, and use 'retaddr' as f's return-to address.  Also, clear all
   the integer registers before entering f.*/
__attribute__((noreturn))
void ML_(call_on_new_stack_0_1) ( Addr stack,
                                  Addr retaddr,
                                  void (*f)(Word),
                                  Word arg1 );
//    r0 = stack
//    r1 = retaddr
//    r2 = f
//    r3 = arg1
asm(
".text\n"
".globl vgModuleLocal_call_on_new_stack_0_1\n"
"vgModuleLocal_call_on_new_stack_0_1:\n"
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
"   mov    x18, #0\n\t"
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
"   br     x9\n\t"
".previous\n"
);


/*
        Perform a clone system call.  clone is strange because it has
        fork()-like return-twice semantics, so it needs special
        handling here.

	Upon entry, we have:

	    Word (*fn)(void*)	in x0
	    void*  child_stack	in x1
	    int    flags	in x2
	    void*  arg		in x3
	    pid_t* child_tid	in x4
	    pid_t* parent_tid	in x5
	    void*  tls_ptr      in x6

	System call requires:

	    int    $__NR_clone  in x8
	    int    flags	in x0
	    void*  child_stack	in x1
	    pid_t* parent_tid	in x2
	    void*  tls_ptr      in x3
	    pid_t* child_tid	in x4

	Returns a Long encoded in the linux-arm64 way, not a SysRes.
*/
#define __NR_CLONE        VG_STRINGIFY(__NR_clone)
#define __NR_EXIT         VG_STRINGIFY(__NR_exit)

extern
Long do_syscall_clone_arm64_linux ( Word (*fn)(void *), 
                                    void* child_stack, 
                                    Long  flags, 
                                    void* arg,
                                    Int*  child_tid,
                                    Int*  parent_tid,
                                    void* tls );
asm(
".text\n"
".globl do_syscall_clone_arm64_linux\n"
"do_syscall_clone_arm64_linux:\n"
        // set up child stack, temporarily preserving fn and arg
"       sub    x1, x1, #16\n"       // make space on stack
"       str    x3, [x1, #8]\n"      // save arg
"       str    x0, [x1, #0]\n"      // save fn 
        
        // setup syscall
"       mov    x8, #"__NR_CLONE"\n" // syscall number
"       mov    x0, x2\n"            // syscall arg1: flags
"       mov    x1, x1\n"            // syscall arg2: child_stack
"       mov    x2, x5\n"            // syscall arg3: parent_tid
"       mov    x3, x6\n"            // syscall arg4: tls_ptr
"       mov    x4, x4\n"            // syscall arg5: child_tid

"       svc    0\n"                 // clone()

"       cmp    x0, #0\n"            // child if retval == 0
"       bne    1f\n"

        // CHILD - call thread function
"       ldr    x1, [sp, #0]\n"      // pop fn
"       ldr    x0, [sp, #8]\n"      // pop fn arg1: arg
"       add    sp, sp, #16\n"
"       blr    x1\n"                // call fn

        // exit with result
"       mov    x0, x0\n"            // arg1: return value from fn
"       mov    x8, #"__NR_EXIT"\n"

"       svc    0\n"

        // Exit returned?!
"       .word 0xFFFFFFFF\n"

"1:\n"  // PARENT or ERROR.  x0 holds return value from the clone syscall.
"       ret\n"
".previous\n"
);

#undef __NR_CLONE
#undef __NR_EXIT

// forward declaration
static void setup_child ( ThreadArchState*, ThreadArchState* );
static void assign_guest_tls(ThreadId ctid, Addr tlsptr);
//ZZ static SysRes sys_set_tls ( ThreadId tid, Addr tlsptr );
            
/* 
   When a client clones, we need to keep track of the new thread.  This means:
   1. allocate a ThreadId+ThreadState+stack for the the thread

   2. initialize the thread's new VCPU state

   3. create the thread using the same args as the client requested,
   but using the scheduler entrypoint for IP, and a separate stack
   for SP.
 */
static SysRes do_clone ( ThreadId ptid, 
                         ULong flags,
                         Addr  child_xsp, 
                         Int*  parent_tidptr, 
                         Int*  child_tidptr, 
                         Addr  child_tls )
{
   ThreadId     ctid = VG_(alloc_ThreadState)();
   ThreadState* ptst = VG_(get_ThreadState)(ptid);
   ThreadState* ctst = VG_(get_ThreadState)(ctid);
   UWord*       stack;
   SysRes       res;
   ULong        x0;
   vki_sigset_t blockall, savedmask;

   VG_(sigfillset)(&blockall);

   vg_assert(VG_(is_running_thread)(ptid));
   vg_assert(VG_(is_valid_tid)(ctid));

   stack = (UWord*)ML_(allocstack)(ctid);
   if (stack == NULL) {
      res = VG_(mk_SysRes_Error)( VKI_ENOMEM );
      goto out;
   }

   /* Copy register state

      Both parent and child return to the same place, and the code
      following the clone syscall works out which is which, so we
      don't need to worry about it.

      The parent gets the child's new tid returned from clone, but the
      child gets 0.

      If the clone call specifies a NULL xsp for the new thread, then
      it actually gets a copy of the parent's xsp.
   */
   setup_child( &ctst->arch, &ptst->arch );

   /* Make sys_clone appear to have returned Success(0) in the
      child. */
   ctst->arch.vex.guest_X0 = 0;

   if (child_xsp != 0)
      ctst->arch.vex.guest_XSP = child_xsp;

   ctst->os_state.parent = ptid;

   /* inherit signal mask */
   ctst->sig_mask = ptst->sig_mask;
   ctst->tmp_sig_mask = ptst->sig_mask;

   /* Start the child with its threadgroup being the same as the
      parent's.  This is so that any exit_group calls that happen
      after the child is created but before it sets its
      os_state.threadgroup field for real (in thread_wrapper in
      syswrap-linux.c), really kill the new thread.  a.k.a this avoids
      a race condition in which the thread is unkillable (via
      exit_group) because its threadgroup is not set.  The race window
      is probably only a few hundred or a few thousand cycles long.
      See #226116. */
   ctst->os_state.threadgroup = ptst->os_state.threadgroup;

   ML_(guess_and_register_stack)(child_xsp, ctst);

   /* Assume the clone will succeed, and tell any tool that wants to
      know that this thread has come into existence.  If the clone
      fails, we'll send out a ll_exit notification for it at the out:
      label below, to clean up. */
   vg_assert(VG_(owns_BigLock_LL)(ptid));
   VG_TRACK ( pre_thread_ll_create, ptid, ctid );

   if (flags & VKI_CLONE_SETTLS) {
      /* Just assign the tls pointer in the guest TPIDR_EL0. */
      assign_guest_tls(ctid, child_tls);
   }
    
   flags &= ~VKI_CLONE_SETTLS;

   /* start the thread with everything blocked */
   VG_(sigprocmask)(VKI_SIG_SETMASK, &blockall, &savedmask);

   x0 = do_syscall_clone_arm64_linux(
      ML_(start_thread_NORETURN), stack, flags, &VG_(threads)[ctid],
      child_tidptr, parent_tidptr, NULL
   );
    
   res = VG_(mk_SysRes_arm64_linux)( x0 );

   VG_(sigprocmask)(VKI_SIG_SETMASK, &savedmask, NULL);

  out:
   if (sr_isError(res)) {
      /* clone failed */
      VG_(cleanup_thread)(&ctst->arch);
      ctst->status = VgTs_Empty;
      /* oops.  Better tell the tool the thread exited in a hurry :-) */
      VG_TRACK( pre_thread_ll_exit, ctid );
   }

   return res;
}


/* ---------------------------------------------------------------------
   More thread stuff
   ------------------------------------------------------------------ */

// ARM64 doesn't have any architecture specific thread stuff that
// needs to be cleaned up
void VG_(cleanup_thread) ( ThreadArchState* arch )
{
}  

void setup_child ( /*OUT*/ ThreadArchState *child,
                   /*IN*/  ThreadArchState *parent )
{
   child->vex = parent->vex;
   child->vex_shadow1 = parent->vex_shadow1;
   child->vex_shadow2 = parent->vex_shadow2;
}

static void assign_guest_tls(ThreadId tid, Addr tlsptr)
{
   VG_(threads)[tid].arch.vex.guest_TPIDR_EL0 = tlsptr;
}

//ZZ /* Assigns tlsptr to the guest TPIDRURO.
//ZZ    If needed for the specific hardware, really executes
//ZZ    the set_tls syscall.
//ZZ */
//ZZ static SysRes sys_set_tls ( ThreadId tid, Addr tlsptr )
//ZZ {
//ZZ    assign_guest_tls(tid, tlsptr);
//ZZ #if defined(ANDROID_HARDWARE_emulator)
//ZZ    /* Android emulator does not provide an hw tls register.
//ZZ       So, the tls register is emulated by the kernel.
//ZZ       This emulated value is set by the __NR_ARM_set_tls syscall.
//ZZ       The emulated value must be read by the kernel helper function
//ZZ       located at 0xffff0fe0.
//ZZ       
//ZZ       The emulated tlsptr is located at 0xffff0ff0
//ZZ       (so slightly after the kernel helper function).
//ZZ       Note that applications are not supposed to read this directly.
//ZZ       
//ZZ       For compatibility : if there is a hw tls register, the kernel
//ZZ       will put at 0xffff0fe0 the instructions to read it, so
//ZZ       as to have old applications calling the kernel helper
//ZZ       working properly.
//ZZ 
//ZZ       For having emulated guest TLS working correctly with
//ZZ       Valgrind, it is needed to execute the syscall to set
//ZZ       the emulated TLS value in addition to the assignment
//ZZ       of TPIDRURO.
//ZZ 
//ZZ       Note: the below means that if we need thread local storage
//ZZ       for Valgrind host, then there will be a conflict between
//ZZ       the need of the guest tls and of the host tls.
//ZZ       If all the guest code would cleanly call 0xffff0fe0,
//ZZ       then we might maybe intercept this. However, at least
//ZZ       __libc_preinit reads directly 0xffff0ff0.
//ZZ    */
//ZZ    /* ??? might call the below if auxv->u.a_val & VKI_HWCAP_TLS ???
//ZZ       Unclear if real hardware having tls hw register sets
//ZZ       VKI_HWCAP_TLS. */
//ZZ    return VG_(do_syscall1) (__NR_ARM_set_tls, tlsptr);
//ZZ #else
//ZZ    return VG_(mk_SysRes_Success)( 0 );
//ZZ #endif
//ZZ }

/* ---------------------------------------------------------------------
   PRE/POST wrappers for arm/Linux-specific syscalls
   ------------------------------------------------------------------ */

#define PRE(name)       DEFN_PRE_TEMPLATE(arm64_linux, name)
#define POST(name)      DEFN_POST_TEMPLATE(arm64_linux, name)

/* Add prototypes for the wrappers declared here, so that gcc doesn't
   harass us for not having prototypes.  Really this is a kludge --
   the right thing to do is to make these wrappers 'static' since they
   aren't visible outside this file, but that requires even more macro
   magic. */

DECL_TEMPLATE(arm64_linux, sys_fadvise64);
DECL_TEMPLATE(arm64_linux, sys_mmap);
//ZZ DECL_TEMPLATE(arm_linux, sys_stat64);
//ZZ DECL_TEMPLATE(arm_linux, sys_lstat64);
//ZZ DECL_TEMPLATE(arm_linux, sys_fstatat64);
//ZZ DECL_TEMPLATE(arm_linux, sys_fstat64);
DECL_TEMPLATE(arm64_linux, sys_clone);
//ZZ DECL_TEMPLATE(arm_linux, sys_sigreturn);
DECL_TEMPLATE(arm64_linux, sys_rt_sigreturn);
//ZZ DECL_TEMPLATE(arm_linux, sys_sigsuspend);
//ZZ DECL_TEMPLATE(arm_linux, sys_set_tls);
//ZZ DECL_TEMPLATE(arm_linux, sys_cacheflush);
//ZZ DECL_TEMPLATE(arm_linux, sys_ptrace);

//ZZ PRE(sys_mmap2)
//ZZ {
//ZZ    SysRes r;
//ZZ 
//ZZ    // Exactly like old_mmap() except:
//ZZ    //  - all 6 args are passed in regs, rather than in a memory-block.
//ZZ    //  - the file offset is specified in pagesize units rather than bytes,
//ZZ    //    so that it can be used for files bigger than 2^32 bytes.
//ZZ    // pagesize or 4K-size units in offset?  For ppc32/64-linux, this is
//ZZ    // 4K-sized.  Assert that the page size is 4K here for safety.
//ZZ    vg_assert(VKI_PAGE_SIZE == 4096);
//ZZ    PRINT("sys_mmap2 ( %#lx, %llu, %ld, %ld, %ld, %ld )",
//ZZ          ARG1, (ULong)ARG2, ARG3, ARG4, ARG5, ARG6 );
//ZZ    PRE_REG_READ6(long, "mmap2",
//ZZ                  unsigned long, start, unsigned long, length,
//ZZ                  unsigned long, prot,  unsigned long, flags,
//ZZ                  unsigned long, fd,    unsigned long, offset);
//ZZ 
//ZZ    r = ML_(generic_PRE_sys_mmap)( tid, ARG1, ARG2, ARG3, ARG4, ARG5, 
//ZZ                                        4096 * (Off64T)ARG6 );
//ZZ    SET_STATUS_from_SysRes(r);
//ZZ }

// ARM64 FIXME is this correct?
PRE(sys_fadvise64)
{
   PRINT("sys_fadvise64 ( %ld, %ld, %lu, %ld )", ARG1,ARG2,ARG3,ARG4);
   PRE_REG_READ4(long, "fadvise64",
                 int, fd, vki_loff_t, offset, vki_size_t, len, int, advice);
}

// ARM64 FIXME is this correct?
PRE(sys_mmap)
{
   SysRes r;

   PRINT("sys_mmap ( %#lx, %llu, %ld, %ld, %d, %ld )",
         ARG1, (ULong)ARG2, ARG3, ARG4, (Int)ARG5, ARG6 );
   PRE_REG_READ6(long, "mmap",
                 unsigned long, start, unsigned long, length,
                 unsigned long, prot,  unsigned long, flags,
                 unsigned long, fd,    unsigned long, offset);

   r = ML_(generic_PRE_sys_mmap)( tid, ARG1, ARG2, ARG3, ARG4, ARG5, ARG6 );
   SET_STATUS_from_SysRes(r);
}

//ZZ 
//ZZ // XXX: lstat64/fstat64/stat64 are generic, but not necessarily
//ZZ // applicable to every architecture -- I think only to 32-bit archs.
//ZZ // We're going to need something like linux/core_os32.h for such
//ZZ // things, eventually, I think.  --njn
//ZZ PRE(sys_lstat64)
//ZZ {
//ZZ    PRINT("sys_lstat64 ( %#lx(%s), %#lx )",ARG1,(char*)ARG1,ARG2);
//ZZ    PRE_REG_READ2(long, "lstat64", char *, file_name, struct stat64 *, buf);
//ZZ    PRE_MEM_RASCIIZ( "lstat64(file_name)", ARG1 );
//ZZ    PRE_MEM_WRITE( "lstat64(buf)", ARG2, sizeof(struct vki_stat64) );
//ZZ }
//ZZ 
//ZZ POST(sys_lstat64)
//ZZ {
//ZZ    vg_assert(SUCCESS);
//ZZ    if (RES == 0) {
//ZZ       POST_MEM_WRITE( ARG2, sizeof(struct vki_stat64) );
//ZZ    }
//ZZ }
//ZZ 
//ZZ PRE(sys_stat64)
//ZZ {
//ZZ    PRINT("sys_stat64 ( %#lx(%s), %#lx )",ARG1,(char*)ARG1,ARG2);
//ZZ    PRE_REG_READ2(long, "stat64", char *, file_name, struct stat64 *, buf);
//ZZ    PRE_MEM_RASCIIZ( "stat64(file_name)", ARG1 );
//ZZ    PRE_MEM_WRITE( "stat64(buf)", ARG2, sizeof(struct vki_stat64) );
//ZZ }
//ZZ 
//ZZ POST(sys_stat64)
//ZZ {
//ZZ    POST_MEM_WRITE( ARG2, sizeof(struct vki_stat64) );
//ZZ }
//ZZ 
//ZZ PRE(sys_fstatat64)
//ZZ {
//ZZ    PRINT("sys_fstatat64 ( %ld, %#lx(%s), %#lx )",ARG1,ARG2,(char*)ARG2,ARG3);
//ZZ    PRE_REG_READ3(long, "fstatat64",
//ZZ                  int, dfd, char *, file_name, struct stat64 *, buf);
//ZZ    PRE_MEM_RASCIIZ( "fstatat64(file_name)", ARG2 );
//ZZ    PRE_MEM_WRITE( "fstatat64(buf)", ARG3, sizeof(struct vki_stat64) );
//ZZ }
//ZZ 
//ZZ POST(sys_fstatat64)
//ZZ {
//ZZ    POST_MEM_WRITE( ARG3, sizeof(struct vki_stat64) );
//ZZ }
//ZZ 
//ZZ PRE(sys_fstat64)
//ZZ {
//ZZ    PRINT("sys_fstat64 ( %ld, %#lx )",ARG1,ARG2);
//ZZ    PRE_REG_READ2(long, "fstat64", unsigned long, fd, struct stat64 *, buf);
//ZZ    PRE_MEM_WRITE( "fstat64(buf)", ARG2, sizeof(struct vki_stat64) );
//ZZ }
//ZZ 
//ZZ POST(sys_fstat64)
//ZZ {
//ZZ    POST_MEM_WRITE( ARG2, sizeof(struct vki_stat64) );
//ZZ }

/* Aarch64 seems to use CONFIG_CLONE_BACKWARDS in the kernel.  See:
      http://dev.gentoo.org/~vapier/aarch64/linux-3.12.6.config
      http://people.redhat.com/wcohen/aarch64/aarch64_config
   from linux-3.10.5/kernel/fork.c 
    #ifdef CONFIG_CLONE_BACKWARDS
    SYSCALL_DEFINE5(clone, unsigned long, clone_flags, unsigned long, newsp,
                     int __user *, parent_tidptr,
                     int, tls_val,
                     int __user *, child_tidptr)
*/
PRE(sys_clone)
{
   UInt cloneflags;

   PRINT("sys_clone ( %lx, %#lx, %#lx, %#lx, %#lx )",ARG1,ARG2,ARG3,ARG4,ARG5);
   PRE_REG_READ5(int, "clone",
                 unsigned long, flags,
                 void *, child_stack,
                 int *, parent_tidptr,
                 void *, child_tls,
                 int *, child_tidptr);

   if (ARG1 & VKI_CLONE_PARENT_SETTID) {
      PRE_MEM_WRITE("clone(parent_tidptr)", ARG3, sizeof(Int));
      if (!VG_(am_is_valid_for_client)(ARG3, sizeof(Int), 
                                             VKI_PROT_WRITE)) {
         SET_STATUS_Failure( VKI_EFAULT );
         return;
      }
   }
//ZZ    if (ARG1 & VKI_CLONE_SETTLS) {
//ZZ       PRE_MEM_READ("clone(tls_user_desc)", ARG4, sizeof(vki_modify_ldt_t));
//ZZ       if (!VG_(am_is_valid_for_client)(ARG4, sizeof(vki_modify_ldt_t), 
//ZZ                                              VKI_PROT_READ)) {
//ZZ          SET_STATUS_Failure( VKI_EFAULT );
//ZZ          return;
//ZZ       }
//ZZ    }
   if (ARG1 & (VKI_CLONE_CHILD_SETTID | VKI_CLONE_CHILD_CLEARTID)) {
      PRE_MEM_WRITE("clone(child_tidptr)", ARG5, sizeof(Int));
      if (!VG_(am_is_valid_for_client)(ARG5, sizeof(Int), 
                                             VKI_PROT_WRITE)) {
         SET_STATUS_Failure( VKI_EFAULT );
         return;
      }
   }

   cloneflags = ARG1;

   if (!ML_(client_signal_OK)(ARG1 & VKI_CSIGNAL)) {
      SET_STATUS_Failure( VKI_EINVAL );
      return;
   }

   /* Only look at the flags we really care about */
   switch (cloneflags & (VKI_CLONE_VM | VKI_CLONE_FS 
                         | VKI_CLONE_FILES | VKI_CLONE_VFORK)) {
   case VKI_CLONE_VM | VKI_CLONE_FS | VKI_CLONE_FILES:
      /* thread creation */
      SET_STATUS_from_SysRes(
         do_clone(tid,
                  ARG1,         /* flags */
                  (Addr)ARG2,   /* child SP */
                  (Int*)ARG3,   /* parent_tidptr */
                  (Int*)ARG5,   /* child_tidptr */
                  (Addr)ARG4)); /* tls_val */
      break;

   case VKI_CLONE_VFORK | VKI_CLONE_VM: /* vfork */
      /* FALLTHROUGH - assume vfork == fork */
      cloneflags &= ~(VKI_CLONE_VFORK | VKI_CLONE_VM);

   case 0: /* plain fork */
      SET_STATUS_from_SysRes(
         ML_(do_fork_clone)(tid,
                       cloneflags,     /* flags */
                       (Int*)ARG3,     /* parent_tidptr */
                       (Int*)ARG5));   /* child_tidptr */
      break;

   default:
      /* should we just ENOSYS? */
      VG_(message)(Vg_UserMsg, "Unsupported clone() flags: 0x%lx\n", ARG1);
      VG_(message)(Vg_UserMsg, "\n");
      VG_(message)(Vg_UserMsg, "The only supported clone() uses are:\n");
      VG_(message)(Vg_UserMsg, " - via a threads library (LinuxThreads or NPTL)\n");
      VG_(message)(Vg_UserMsg, " - via the implementation of fork or vfork\n");
      VG_(message)(Vg_UserMsg, " - for the Quadrics Elan3 user-space driver\n");
      VG_(unimplemented)
         ("Valgrind does not support general clone().");
   }

   if (SUCCESS) {
      if (ARG1 & VKI_CLONE_PARENT_SETTID)
         POST_MEM_WRITE(ARG3, sizeof(Int));
      if (ARG1 & (VKI_CLONE_CHILD_SETTID | VKI_CLONE_CHILD_CLEARTID))
         POST_MEM_WRITE(ARG5, sizeof(Int));

      /* Thread creation was successful; let the child have the chance
         to run */
      *flags |= SfYieldAfter;
   }
}

//ZZ PRE(sys_sigreturn)
//ZZ {
//ZZ    /* See comments on PRE(sys_rt_sigreturn) in syswrap-amd64-linux.c for
//ZZ      an explanation of what follows. */
//ZZ 
//ZZ    PRINT("sys_sigreturn ( )");
//ZZ 
//ZZ    vg_assert(VG_(is_valid_tid)(tid));
//ZZ    vg_assert(tid >= 1 && tid < VG_N_THREADS);
//ZZ    vg_assert(VG_(is_running_thread)(tid));
//ZZ 
//ZZ    /* Restore register state from frame and remove it */
//ZZ    VG_(sigframe_destroy)(tid, False);
//ZZ 
//ZZ    /* Tell the driver not to update the guest state with the "result",
//ZZ       and set a bogus result to keep it happy. */
//ZZ    *flags |= SfNoWriteResult;
//ZZ    SET_STATUS_Success(0);
//ZZ 
//ZZ    /* Check to see if any signals arose as a result of this. */
//ZZ    *flags |= SfPollAfter;
//ZZ }

PRE(sys_rt_sigreturn)
{
  /* See comments on PRE(sys_rt_sigreturn) in syswrap-amd64-linux.c for
      an explanation of what follows. */

   PRINT("rt_sigreturn ( )");

   vg_assert(VG_(is_valid_tid)(tid));
   vg_assert(tid >= 1 && tid < VG_N_THREADS);
   vg_assert(VG_(is_running_thread)(tid));

   /* Restore register state from frame and remove it */
   VG_(sigframe_destroy)(tid, True);

   /* Tell the driver not to update the guest state with the "result",
      and set a bogus result to keep it happy. */
   *flags |= SfNoWriteResult;
   SET_STATUS_Success(0);

   /* Check to see if any signals arose as a result of this. */
   *flags |= SfPollAfter;
}

//ZZ /* NB: clone of x86-linux version, and ppc32-linux has an almost
//ZZ    identical one. */
//ZZ PRE(sys_sigsuspend)
//ZZ {
//ZZ    /* The C library interface to sigsuspend just takes a pointer to
//ZZ       a signal mask but this system call has three arguments - the first
//ZZ       two don't appear to be used by the kernel and are always passed as
//ZZ       zero by glibc and the third is the first word of the signal mask
//ZZ       so only 32 signals are supported.
//ZZ      
//ZZ       In fact glibc normally uses rt_sigsuspend if it is available as
//ZZ       that takes a pointer to the signal mask so supports more signals.
//ZZ     */
//ZZ    *flags |= SfMayBlock;
//ZZ    PRINT("sys_sigsuspend ( %ld, %ld, %ld )", ARG1,ARG2,ARG3 );
//ZZ    PRE_REG_READ3(int, "sigsuspend",
//ZZ                  int, history0, int, history1,
//ZZ                  vki_old_sigset_t, mask);
//ZZ }
//ZZ 
//ZZ /* Very much ARM specific */
//ZZ 
//ZZ PRE(sys_set_tls)
//ZZ {
//ZZ    PRINT("set_tls (%lx)",ARG1);
//ZZ    PRE_REG_READ1(long, "set_tls", unsigned long, addr);
//ZZ 
//ZZ    SET_STATUS_from_SysRes( sys_set_tls( tid, ARG1 ) );
//ZZ }
//ZZ 
//ZZ PRE(sys_cacheflush)
//ZZ {
//ZZ    PRINT("cacheflush (%lx, %#lx, %#lx)",ARG1,ARG2,ARG3);
//ZZ    PRE_REG_READ3(long, "cacheflush", void*, addrlow,void*, addrhigh,int, flags);
//ZZ    VG_(discard_translations)( (Addr)ARG1,
//ZZ                               ((ULong)ARG2) - ((ULong)ARG1) + 1ULL/*paranoia*/,
//ZZ                               "PRE(sys_cacheflush)" );
//ZZ    SET_STATUS_Success(0);
//ZZ }
//ZZ 
//ZZ // ARG3 is only used for pointers into the traced process's address
//ZZ // space and for offsets into the traced process's struct
//ZZ // user_regs_struct. It is never a pointer into this process's memory
//ZZ // space, and we should therefore not check anything it points to.
//ZZ PRE(sys_ptrace)
//ZZ {
//ZZ    PRINT("sys_ptrace ( %ld, %ld, %#lx, %#lx )", ARG1,ARG2,ARG3,ARG4);
//ZZ    PRE_REG_READ4(int, "ptrace", 
//ZZ                  long, request, long, pid, long, addr, long, data);
//ZZ    switch (ARG1) {
//ZZ    case VKI_PTRACE_PEEKTEXT:
//ZZ    case VKI_PTRACE_PEEKDATA:
//ZZ    case VKI_PTRACE_PEEKUSR:
//ZZ       PRE_MEM_WRITE( "ptrace(peek)", ARG4, 
//ZZ 		     sizeof (long));
//ZZ       break;
//ZZ    case VKI_PTRACE_GETREGS:
//ZZ       PRE_MEM_WRITE( "ptrace(getregs)", ARG4, 
//ZZ 		     sizeof (struct vki_user_regs_struct));
//ZZ       break;
//ZZ    case VKI_PTRACE_GETFPREGS:
//ZZ       PRE_MEM_WRITE( "ptrace(getfpregs)", ARG4, 
//ZZ 		     sizeof (struct vki_user_fp));
//ZZ       break;
//ZZ    case VKI_PTRACE_GETWMMXREGS:
//ZZ       PRE_MEM_WRITE( "ptrace(getwmmxregs)", ARG4, 
//ZZ 		     VKI_IWMMXT_SIZE);
//ZZ       break;
//ZZ    case VKI_PTRACE_GETCRUNCHREGS:
//ZZ       PRE_MEM_WRITE( "ptrace(getcrunchregs)", ARG4, 
//ZZ 		     VKI_CRUNCH_SIZE);
//ZZ       break;
//ZZ    case VKI_PTRACE_GETVFPREGS:
//ZZ       PRE_MEM_WRITE( "ptrace(getvfpregs)", ARG4, 
//ZZ                      sizeof (struct vki_user_vfp) );
//ZZ       break;
//ZZ    case VKI_PTRACE_GETHBPREGS:
//ZZ       PRE_MEM_WRITE( "ptrace(gethbpregs)", ARG4, 
//ZZ                      sizeof (unsigned long) );
//ZZ       break;
//ZZ    case VKI_PTRACE_SETREGS:
//ZZ       PRE_MEM_READ( "ptrace(setregs)", ARG4, 
//ZZ 		     sizeof (struct vki_user_regs_struct));
//ZZ       break;
//ZZ    case VKI_PTRACE_SETFPREGS:
//ZZ       PRE_MEM_READ( "ptrace(setfpregs)", ARG4, 
//ZZ 		     sizeof (struct vki_user_fp));
//ZZ       break;
//ZZ    case VKI_PTRACE_SETWMMXREGS:
//ZZ       PRE_MEM_READ( "ptrace(setwmmxregs)", ARG4, 
//ZZ 		     VKI_IWMMXT_SIZE);
//ZZ       break;
//ZZ    case VKI_PTRACE_SETCRUNCHREGS:
//ZZ       PRE_MEM_READ( "ptrace(setcrunchregs)", ARG4, 
//ZZ 		     VKI_CRUNCH_SIZE);
//ZZ       break;
//ZZ    case VKI_PTRACE_SETVFPREGS:
//ZZ       PRE_MEM_READ( "ptrace(setvfpregs)", ARG4, 
//ZZ                      sizeof (struct vki_user_vfp));
//ZZ       break;
//ZZ    case VKI_PTRACE_SETHBPREGS:
//ZZ       PRE_MEM_READ( "ptrace(sethbpregs)", ARG4, sizeof(unsigned long));
//ZZ       break;
//ZZ    case VKI_PTRACE_GET_THREAD_AREA:
//ZZ       PRE_MEM_WRITE( "ptrace(get_thread_area)", ARG4, sizeof(unsigned long));
//ZZ       break;
//ZZ    case VKI_PTRACE_GETEVENTMSG:
//ZZ       PRE_MEM_WRITE( "ptrace(geteventmsg)", ARG4, sizeof(unsigned long));
//ZZ       break;
//ZZ    case VKI_PTRACE_GETSIGINFO:
//ZZ       PRE_MEM_WRITE( "ptrace(getsiginfo)", ARG4, sizeof(vki_siginfo_t));
//ZZ       break;
//ZZ    case VKI_PTRACE_SETSIGINFO:
//ZZ       PRE_MEM_READ( "ptrace(setsiginfo)", ARG4, sizeof(vki_siginfo_t));
//ZZ       break;
//ZZ    case VKI_PTRACE_GETREGSET:
//ZZ       ML_(linux_PRE_getregset)(tid, ARG3, ARG4);
//ZZ       break;
//ZZ    case VKI_PTRACE_SETREGSET:
//ZZ       ML_(linux_PRE_setregset)(tid, ARG3, ARG4);
//ZZ       break;
//ZZ    default:
//ZZ       break;
//ZZ    }
//ZZ }
//ZZ 
//ZZ POST(sys_ptrace)
//ZZ {
//ZZ    switch (ARG1) {
//ZZ    case VKI_PTRACE_PEEKTEXT:
//ZZ    case VKI_PTRACE_PEEKDATA:
//ZZ    case VKI_PTRACE_PEEKUSR:
//ZZ       POST_MEM_WRITE( ARG4, sizeof (long));
//ZZ       break;
//ZZ    case VKI_PTRACE_GETREGS:
//ZZ       POST_MEM_WRITE( ARG4, sizeof (struct vki_user_regs_struct));
//ZZ       break;
//ZZ    case VKI_PTRACE_GETFPREGS:
//ZZ       POST_MEM_WRITE( ARG4, sizeof (struct vki_user_fp));
//ZZ       break;
//ZZ    case VKI_PTRACE_GETWMMXREGS:
//ZZ       POST_MEM_WRITE( ARG4, VKI_IWMMXT_SIZE);
//ZZ       break;
//ZZ    case VKI_PTRACE_GETCRUNCHREGS:
//ZZ       POST_MEM_WRITE( ARG4, VKI_CRUNCH_SIZE);
//ZZ       break;
//ZZ    case VKI_PTRACE_GETVFPREGS:
//ZZ       POST_MEM_WRITE( ARG4, sizeof(struct vki_user_vfp));
//ZZ       break;
//ZZ    case VKI_PTRACE_GET_THREAD_AREA:
//ZZ    case VKI_PTRACE_GETHBPREGS:
//ZZ    case VKI_PTRACE_GETEVENTMSG:
//ZZ       POST_MEM_WRITE( ARG4, sizeof(unsigned long));
//ZZ       break;
//ZZ    case VKI_PTRACE_GETSIGINFO:
//ZZ       /* XXX: This is a simplification. Different parts of the
//ZZ        * siginfo_t are valid depending on the type of signal.
//ZZ        */
//ZZ       POST_MEM_WRITE( ARG4, sizeof(vki_siginfo_t));
//ZZ       break;
//ZZ    case VKI_PTRACE_GETREGSET:
//ZZ       ML_(linux_POST_getregset)(tid, ARG3, ARG4);
//ZZ       break;
//ZZ    default:
//ZZ       break;
//ZZ    }
//ZZ }
//ZZ 
//ZZ #undef PRE
//ZZ #undef POST

/* ---------------------------------------------------------------------
   The arm64/Linux syscall table
   ------------------------------------------------------------------ */

//ZZ #if 0
//ZZ #define __NR_OABI_SYSCALL_BASE 0x900000
//ZZ #else
//ZZ #define __NR_OABI_SYSCALL_BASE 0x0
//ZZ #endif

#define PLAX_(sysno, name)    WRAPPER_ENTRY_X_(arm64_linux, sysno, name) 
#define PLAXY(sysno, name)    WRAPPER_ENTRY_XY(arm64_linux, sysno, name)

// This table maps from __NR_xxx syscall numbers (from
// linux/include/asm-arm/unistd.h) to the appropriate PRE/POST sys_foo()
// wrappers on arm64 (as per sys_call_table in linux/arch/arm/kernel/entry.S).
//
// For those syscalls not handled by Valgrind, the annotation indicate its
// arch/OS combination, eg. */* (generic), */Linux (Linux only), ?/?
// (unknown).

static SyscallTableEntry syscall_main_table[] = {
   LINXY(__NR_getxattr,          sys_getxattr),          // 8
   LINXY(__NR_lgetxattr,         sys_lgetxattr),         // 9
   GENXY(__NR_getcwd,            sys_getcwd),            // 17
   LINXY(__NR_eventfd2,          sys_eventfd2),          // 19
   LINXY(__NR_epoll_create1,     sys_epoll_create1),     // 20
   LINX_(__NR_epoll_ctl,         sys_epoll_ctl),         // 21
   LINXY(__NR_epoll_pwait,       sys_epoll_pwait),       // 22
   GENXY(__NR_dup,               sys_dup),               // 23
   LINXY(__NR_dup3,              sys_dup3),              // 24

   // FIXME IS THIS CORRECT?
   LINXY(__NR3264_fcntl,         sys_fcntl),             // 25

   LINXY(__NR_inotify_init1,     sys_inotify_init1),     // 26
   LINX_(__NR_inotify_add_watch, sys_inotify_add_watch), // 27
   LINX_(__NR_inotify_rm_watch,  sys_inotify_rm_watch),  // 28
   LINXY(__NR_ioctl,             sys_ioctl),             // 29
   GENX_(__NR_flock,             sys_flock),             // 32
   LINX_(__NR_mknodat,           sys_mknodat),           // 33
   LINX_(__NR_mkdirat,           sys_mkdirat),           // 34
   LINX_(__NR_unlinkat,          sys_unlinkat),          // 35
   LINX_(__NR_symlinkat,         sys_symlinkat),         // 36
   LINX_(__NR_linkat,            sys_linkat),            // 37
   LINX_(__NR_renameat,		 sys_renameat),          // 38

   LINX_(__NR_umount2,            sys_umount),           // 39
   LINX_(__NR_mount,              sys_mount),            // 40

   // FIXME IS THIS CORRECT?  it may well not be.
   GENXY(__NR3264_statfs,        sys_statfs),            // 43
   GENXY(__NR3264_fstatfs,       sys_fstatfs),           // 44

   // FIXME IS THIS CORRECT?  it may well not be.
   GENX_(__NR3264_ftruncate,     sys_ftruncate),         // 46

   LINX_(__NR_fallocate,         sys_fallocate),         // 47
   LINX_(__NR_faccessat,         sys_faccessat),         // 48
   GENX_(__NR_chdir,             sys_chdir),             // 49
   GENX_(__NR_fchdir,            sys_fchdir),            // 50
   GENX_(__NR_chroot,            sys_chroot),            // 51
   GENX_(__NR_fchmod,            sys_fchmod),            // 52
   LINX_(__NR_fchmodat,          sys_fchmodat),          // 53
   LINX_(__NR_fchownat,          sys_fchownat),          // 54
   GENX_(__NR_fchown,            sys_fchown),            // 55
   LINXY(__NR_openat,            sys_openat),            // 56
   GENXY(__NR_close,             sys_close),             // 57
   LINXY(__NR_pipe2,             sys_pipe2),             // 59
   LINX_(__NR_quotactl,          sys_quotactl),          // 60
   GENXY(__NR_getdents64,        sys_getdents64),        // 61

   // FIXME IS THIS CORRECT?
   LINX_(__NR3264_lseek,         sys_lseek),             // 62

   GENXY(__NR_read,              sys_read),              // 63
   GENX_(__NR_write,             sys_write),             // 64
   GENXY(__NR_readv,             sys_readv),             // 65
   GENX_(__NR_writev,            sys_writev),            // 66
   GENXY(__NR_pread64,           sys_pread64),           // 67
   GENX_(__NR_pwrite64,          sys_pwrite64),          // 68
   LINX_(__NR_pselect6,          sys_pselect6),          // 72
   LINXY(__NR_ppoll,             sys_ppoll),             // 73
   LINXY(__NR_signalfd4,         sys_signalfd4),         // 74
   LINX_(__NR_readlinkat,        sys_readlinkat),        // 78

   // FIXME IS THIS CORRECT?
   LINXY(__NR3264_fstatat,       sys_newfstatat),        // 79
   GENXY(__NR3264_fstat,         sys_newfstat),          // 80

   LINX_(__NR_utimensat,         sys_utimensat),         // 88
   GENX_(__NR_fsync,             sys_fsync),             // 82
   GENX_(__NR_fdatasync,         sys_fdatasync),         // 83
   LINXY(__NR_timerfd_create,    sys_timerfd_create),    // 85
   LINXY(__NR_timerfd_settime,   sys_timerfd_settime),   // 86
   LINXY(__NR_timerfd_gettime,   sys_timerfd_gettime),   // 87
   LINXY(__NR_capget,            sys_capget),            // 90
   GENX_(__NR_exit,              sys_exit),              // 93
   LINX_(__NR_exit_group,        sys_exit_group),        // 94
   LINX_(__NR_set_tid_address,   sys_set_tid_address),   // 96
   LINXY(__NR_futex,             sys_futex),             // 98
   LINX_(__NR_set_robust_list,   sys_set_robust_list),   // 99
   GENXY(__NR_nanosleep,         sys_nanosleep),         // 101
   GENXY(__NR_setitimer,         sys_setitimer),         // 103
   LINXY(__NR_clock_gettime,     sys_clock_gettime),     // 113
   LINXY(__NR_clock_getres,      sys_clock_getres),      // 114
   LINXY(__NR_syslog,            sys_syslog),            // 116
   LINX_(__NR_sched_setaffinity, sys_sched_setaffinity), // 122
   LINXY(__NR_sched_getaffinity, sys_sched_getaffinity), // 123
   LINX_(__NR_sched_yield,       sys_sched_yield),       // 124
   GENX_(__NR_kill,              sys_kill),              // 129
   LINX_(__NR_tgkill,            sys_tgkill),            // 131
   GENXY(__NR_sigaltstack,       sys_sigaltstack),       // 132
   LINX_(__NR_rt_sigsuspend,     sys_rt_sigsuspend),     // 133
   LINXY(__NR_rt_sigaction,      sys_rt_sigaction),      // 134
   LINXY(__NR_rt_sigprocmask,    sys_rt_sigprocmask),    // 135
   LINXY(__NR_rt_sigtimedwait,   sys_rt_sigtimedwait),   // 137
   LINXY(__NR_rt_sigqueueinfo,   sys_rt_sigqueueinfo),   // 138
   PLAX_(__NR_rt_sigreturn,      sys_rt_sigreturn),      // 139
   GENX_(__NR_setpriority,       sys_setpriority),       // 140
   GENX_(__NR_getpriority,       sys_getpriority),       // 141
   GENX_(__NR_setregid,          sys_setregid),          // 143
   GENX_(__NR_setgid,            sys_setgid),            // 144
   GENX_(__NR_setreuid,          sys_setreuid),          // 145
   LINX_(__NR_setresuid,         sys_setresuid),         // 147
   LINXY(__NR_getresuid,         sys_getresuid),         // 148
   LINXY(__NR_getresgid,         sys_getresgid),         // 150
   GENXY(__NR_times,             sys_times),             // 153
   GENX_(__NR_setpgid,           sys_setpgid),           // 154
   GENX_(__NR_getpgid,           sys_getpgid),           // 155
   GENX_(__NR_getsid,            sys_getsid),            // 156
   GENX_(__NR_setsid,            sys_setsid),            // 157
   GENXY(__NR_getgroups,         sys_getgroups),         // 158
   GENX_(__NR_setgroups,         sys_setgroups),         // 159
   GENXY(__NR_uname,             sys_newuname),          // 160
   GENXY(__NR_getrlimit,         sys_old_getrlimit),     // 163
   GENX_(__NR_setrlimit,         sys_setrlimit),         // 164
   GENXY(__NR_getrusage,         sys_getrusage),         // 165
   GENX_(__NR_umask,             sys_umask),             // 166
   LINXY(__NR_prctl,             sys_prctl),             // 167 
   GENXY(__NR_gettimeofday,      sys_gettimeofday),      // 169
   GENX_(__NR_getpid,            sys_getpid),            // 172
   GENX_(__NR_getppid,           sys_getppid),           // 173
   GENX_(__NR_getuid,            sys_getuid),            // 174
   GENX_(__NR_geteuid,           sys_geteuid),           // 175
   GENX_(__NR_getgid,            sys_getgid),            // 176
   GENX_(__NR_getegid,           sys_getegid),           // 177
   LINX_(__NR_gettid,            sys_gettid),            // 178
   LINXY(__NR_sysinfo,           sys_sysinfo),           // 179
   LINXY(__NR_mq_open,           sys_mq_open),           // 180
   LINX_(__NR_mq_unlink,         sys_mq_unlink),         // 181
   LINX_(__NR_mq_timedsend,      sys_mq_timedsend),      // 182
   LINXY(__NR_mq_timedreceive,   sys_mq_timedreceive),   // 183
   LINX_(__NR_mq_notify,         sys_mq_notify),         // 184
   LINXY(__NR_mq_getsetattr,     sys_mq_getsetattr),     // 185
   LINX_(__NR_msgget,            sys_msgget),            // 186
   LINXY(__NR_msgctl,            sys_msgctl),            // 187
   LINXY(__NR_msgrcv,            sys_msgrcv),            // 188
   LINX_(__NR_msgsnd,            sys_msgsnd),            // 189
   LINX_(__NR_semget,            sys_semget),            // 190
   LINXY(__NR_semctl,            sys_semctl),            // 191
   LINX_(__NR_semtimedop,        sys_semtimedop),        // 192
   LINX_(__NR_semop,             sys_semop),             // 193
   LINX_(__NR_shmget,            sys_shmget),            // 194
   LINXY(__NR_shmctl,            sys_shmctl),            // 195
   LINXY(__NR_shmat,             wrap_sys_shmat),        // 196
   LINXY(__NR_shmdt,             sys_shmdt),             // 197
   LINXY(__NR_socket,            sys_socket),            // 198
   LINXY(__NR_socketpair,        sys_socketpair),        // 199
   LINX_(__NR_bind,              sys_bind),              // 200
   LINX_(__NR_listen,            sys_listen),            // 201
   LINXY(__NR_accept,            sys_accept),            // 202
   LINX_(__NR_connect,           sys_connect),           // 203
   LINXY(__NR_getsockname,       sys_getsockname),       // 204
   LINXY(__NR_getpeername,       sys_getpeername),       // 205
   LINX_(__NR_sendto,            sys_sendto),            // 206
   LINXY(__NR_recvfrom,          sys_recvfrom),          // 207
   LINX_(__NR_setsockopt,        sys_setsockopt),        // 208
   LINXY(__NR_getsockopt,        sys_getsockopt),        // 209
   LINX_(__NR_shutdown,          sys_shutdown),          // 210
   LINX_(__NR_sendmsg,           sys_sendmsg),           // 211
   LINXY(__NR_recvmsg,           sys_recvmsg),           // 212
   LINX_(__NR_readahead,         sys_readahead),         // 213
   GENX_(__NR_brk,               sys_brk),               // 214
   GENXY(__NR_munmap,            sys_munmap),            // 215
   GENX_(__NR_mremap,            sys_mremap),            // 216
   LINX_(__NR_add_key,           sys_add_key),           // 217
   LINXY(__NR_keyctl,            sys_keyctl),            // 219
   PLAX_(__NR_clone,             sys_clone),             // 220
   GENX_(__NR_execve,            sys_execve),            // 221

   // FIXME IS THIS CORRECT?
   PLAX_(__NR3264_mmap,          sys_mmap),              // 222
   PLAX_(__NR3264_fadvise64,     sys_fadvise64),         // 223

   GENXY(__NR_mprotect,          sys_mprotect),          // 226
   GENX_(__NR_msync,             sys_msync),             // 227
   GENX_(__NR_mlock,             sys_mlock),             // 228
   GENX_(__NR_mlockall,          sys_mlockall),          // 230
   GENX_(__NR_madvise,           sys_madvise),           // 233
   LINX_(__NR_mbind,             sys_mbind),             // 235
   LINXY(__NR_get_mempolicy,     sys_get_mempolicy),     // 236
   LINX_(__NR_set_mempolicy,     sys_set_mempolicy),     // 237

   LINXY(__NR_recvmmsg,          sys_recvmmsg),          // 243
   LINXY(__NR_accept4,           sys_accept4),           // 242

   GENXY(__NR_wait4,             sys_wait4),             // 260

   LINX_(__NR_syncfs,            sys_syncfs),            // 267

   LINXY(__NR_sendmmsg,          sys_sendmmsg),          // 269
   LINXY(__NR_process_vm_readv,  sys_process_vm_readv),  // 270
   LINX_(__NR_process_vm_writev, sys_process_vm_writev), // 271
   LINXY(__NR_getrandom,         sys_getrandom),         // 278
   LINXY(__NR_memfd_create,      sys_memfd_create),      // 279

// The numbers below are bogus.  (See comment further down.)
// When pulling entries above this line, change the numbers
// to be correct.

//ZZ //zz    //   (restart_syscall)                             // 0
//ZZ    GENX_(__NR_fork,              sys_fork),           // 2
//ZZ 
//ZZ    GENXY(__NR_open,              sys_open),           // 5
//ZZ //   GENXY(__NR_waitpid,           sys_waitpid),        // 7
//ZZ    GENXY(__NR_creat,             sys_creat),          // 8
//ZZ    GENX_(__NR_link,              sys_link),           // 9
//ZZ 
//ZZ    GENX_(__NR_unlink,            sys_unlink),         // 10
//ZZ    GENXY(__NR_time,              sys_time),           // 13
//ZZ    GENX_(__NR_mknod,             sys_mknod),          // 14
//ZZ 
//ZZ    GENX_(__NR_chmod,             sys_chmod),          // 15
//ZZ //zz    LINX_(__NR_lchown,            sys_lchown16),       // 16
//ZZ //   GENX_(__NR_break,             sys_ni_syscall),     // 17
//ZZ //zz    //   (__NR_oldstat,           sys_stat),           // 18 (obsolete)
//ZZ    LINX_(__NR_lseek,             sys_lseek),          // 19
//ZZ 
//ZZ    GENX_(__NR_getpid,            sys_getpid),         // 20
//ZZ    LINX_(__NR_umount,            sys_oldumount),      // 22
//ZZ    LINX_(__NR_setuid,            sys_setuid16),       // 23 ## P
//ZZ    LINX_(__NR_getuid,            sys_getuid16),       // 24 ## P
//ZZ //zz 
//ZZ //zz    //   (__NR_stime,             sys_stime),          // 25 * (SVr4,SVID,X/OPEN)
//ZZ    PLAXY(__NR_ptrace,            sys_ptrace),         // 26
//ZZ    GENX_(__NR_alarm,             sys_alarm),          // 27
//ZZ //zz    //   (__NR_oldfstat,          sys_fstat),          // 28 * L -- obsolete
//ZZ    GENX_(__NR_pause,             sys_pause),          // 29
//ZZ 
//ZZ    LINX_(__NR_utime,             sys_utime),          // 30
//ZZ //   GENX_(__NR_stty,              sys_ni_syscall),     // 31
//ZZ //   GENX_(__NR_gtty,              sys_ni_syscall),     // 32
//ZZ    GENX_(__NR_access,            sys_access),         // 33
//ZZ    GENX_(__NR_nice,              sys_nice),           // 34
//ZZ 
//ZZ //   GENX_(__NR_ftime,             sys_ni_syscall),     // 35
//ZZ    GENX_(__NR_sync,              sys_sync),           // 36
//ZZ    GENX_(__NR_rename,            sys_rename),         // 38
//ZZ    GENX_(__NR_mkdir,             sys_mkdir),          // 39
//ZZ 
//ZZ    GENX_(__NR_rmdir,             sys_rmdir),          // 40
//ZZ    LINXY(__NR_pipe,              sys_pipe),           // 42
//ZZ //   GENX_(__NR_prof,              sys_ni_syscall),     // 44

//ZZ    LINX_(__NR_getgid,            sys_getgid16),       // 47
//ZZ //zz    //   (__NR_signal,            sys_signal),         // 48 */* (ANSI C)
//ZZ    LINX_(__NR_geteuid,           sys_geteuid16),      // 49
//ZZ 
//ZZ    LINX_(__NR_getegid,           sys_getegid16),      // 50
//ZZ    GENX_(__NR_acct,              sys_acct),           // 51
//ZZ //   GENX_(__NR_lock,              sys_ni_syscall),     // 53
//ZZ 
//ZZ    LINXY(__NR_fcntl,             sys_fcntl),          // 55
//ZZ //   GENX_(__NR_mpx,               sys_ni_syscall),     // 56
//ZZ //   GENX_(__NR_ulimit,            sys_ni_syscall),     // 58
//ZZ //zz    //   (__NR_oldolduname,       sys_olduname),       // 59 Linux -- obsolete
//ZZ //zz 
//ZZ //zz    //   (__NR_ustat,             sys_ustat)           // 62 SVr4 -- deprecated
//ZZ    GENXY(__NR_dup2,              sys_dup2),           // 63
//ZZ    GENX_(__NR_getppid,           sys_getppid),        // 64
//ZZ 
//ZZ    GENX_(__NR_getpgrp,           sys_getpgrp),        // 65
//ZZ    LINXY(__NR_sigaction,         sys_sigaction),      // 67
//ZZ //zz    //   (__NR_sgetmask,          sys_sgetmask),       // 68 */* (ANSI C)
//ZZ //zz    //   (__NR_ssetmask,          sys_ssetmask),       // 69 */* (ANSI C)
//ZZ //zz 
//ZZ    PLAX_(__NR_sigsuspend,        sys_sigsuspend),     // 72
//ZZ    LINXY(__NR_sigpending,        sys_sigpending),     // 73
//ZZ //zz    //   (__NR_sethostname,       sys_sethostname),    // 74 */*
//ZZ //zz 
//ZZ    GENXY(__NR_getrlimit,         sys_old_getrlimit),  // 76
//ZZ    GENX_(__NR_settimeofday,      sys_settimeofday),   // 79
//ZZ 
//ZZ    LINXY(__NR_getgroups,         sys_getgroups16),    // 80
//ZZ    LINX_(__NR_setgroups,         sys_setgroups16),    // 81
//ZZ //   PLAX_(__NR_select,            old_select),         // 82
//ZZ    GENX_(__NR_symlink,           sys_symlink),        // 83
//ZZ //zz    //   (__NR_oldlstat,          sys_lstat),          // 84 -- obsolete
//ZZ //zz 
//ZZ    GENX_(__NR_readlink,          sys_readlink),       // 85
//ZZ //zz    //   (__NR_uselib,            sys_uselib),         // 86 */Linux
//ZZ //zz    //   (__NR_swapon,            sys_swapon),         // 87 */Linux
//ZZ //zz    //   (__NR_reboot,            sys_reboot),         // 88 */Linux
//ZZ //zz    //   (__NR_readdir,           old_readdir),        // 89 -- superseded
//ZZ //zz 
//ZZ //   _____(__NR_mmap,              old_mmap),           // 90
//ZZ    GENXY(__NR_munmap,            sys_munmap),         // 91
//ZZ    GENX_(__NR_truncate,          sys_truncate),       // 92
//ZZ    GENX_(__NR_ftruncate,         sys_ftruncate),      // 93
//ZZ 
//ZZ    LINX_(__NR_fchown,            sys_fchown16),       // 95
//ZZ //   GENX_(__NR_profil,            sys_ni_syscall),     // 98
//ZZ    GENXY(__NR_statfs,            sys_statfs),         // 99
//ZZ 
//ZZ    GENXY(__NR_fstatfs,           sys_fstatfs),        // 100
//ZZ //   LINX_(__NR_ioperm,            sys_ioperm),         // 101
//ZZ    LINXY(__NR_socketcall,        sys_socketcall),     // 102
//ZZ 
//ZZ    GENXY(__NR_getitimer,         sys_getitimer),      // 105
//ZZ    GENXY(__NR_stat,              sys_newstat),        // 106
//ZZ    GENXY(__NR_lstat,             sys_newlstat),       // 107
//ZZ    GENXY(__NR_fstat,             sys_newfstat),       // 108
//ZZ //zz    //   (__NR_olduname,          sys_uname),          // 109 -- obsolete
//ZZ //zz 
//ZZ //   GENX_(__NR_iopl,              sys_iopl),           // 110
//ZZ    LINX_(__NR_vhangup,           sys_vhangup),        // 111
//ZZ //   GENX_(__NR_idle,              sys_ni_syscall),     // 112
//ZZ // PLAXY(__NR_vm86old,           sys_vm86old),        // 113 __NR_syscall... weird
//ZZ //zz 
//ZZ //zz    //   (__NR_swapoff,           sys_swapoff),        // 115 */Linux 
//ZZ //   _____(__NR_ipc,               sys_ipc),            // 117
//ZZ    GENX_(__NR_fsync,             sys_fsync),          // 118
//ZZ    PLAX_(__NR_sigreturn,         sys_sigreturn),      // 119 ?/Linux
//ZZ 
//ZZ //zz    //   (__NR_setdomainname,     sys_setdomainname),  // 121 */*(?)
//ZZ //   PLAX_(__NR_modify_ldt,        sys_modify_ldt),     // 123
//ZZ //zz    LINXY(__NR_adjtimex,          sys_adjtimex),       // 124
//ZZ //zz 
//ZZ    LINXY(__NR_sigprocmask,       sys_sigprocmask),    // 126
//ZZ //zz    // Nb: create_module() was removed 2.4-->2.6
//ZZ //   GENX_(__NR_create_module,     sys_ni_syscall),     // 127
//ZZ    LINX_(__NR_init_module,       sys_init_module),    // 128
//ZZ    LINX_(__NR_delete_module,     sys_delete_module),  // 129
//ZZ //zz 
//ZZ //zz    // Nb: get_kernel_syms() was removed 2.4-->2.6
//ZZ //   GENX_(__NR_get_kernel_syms,   sys_ni_syscall),     // 130
//ZZ    GENX_(__NR_getpgid,           sys_getpgid),        // 132
//ZZ //zz    //   (__NR_bdflush,           sys_bdflush),        // 134 */Linux
//ZZ //zz 
//ZZ //zz    //   (__NR_sysfs,             sys_sysfs),          // 135 SVr4
//ZZ    LINX_(__NR_personality,       sys_personality),    // 136
//ZZ //   GENX_(__NR_afs_syscall,       sys_ni_syscall),     // 137
//ZZ    LINX_(__NR_setfsuid,          sys_setfsuid16),     // 138
//ZZ    LINX_(__NR_setfsgid,          sys_setfsgid16),     // 139
//ZZ  
//ZZ    LINXY(__NR__llseek,           sys_llseek),         // 140
//ZZ    GENXY(__NR_getdents,          sys_getdents),       // 141
//ZZ    GENX_(__NR__newselect,        sys_select),         // 142
//ZZ 
//ZZ    LINXY(__NR__sysctl,           sys_sysctl),         // 149
//ZZ 
//ZZ    GENX_(__NR_munlock,           sys_munlock),        // 151
//ZZ    LINX_(__NR_munlockall,        sys_munlockall),     // 153
//ZZ    LINXY(__NR_sched_setparam,    sys_sched_setparam), // 154
//ZZ 
//ZZ    LINXY(__NR_sched_getparam,         sys_sched_getparam),        // 155
//ZZ    LINX_(__NR_sched_setscheduler,     sys_sched_setscheduler),    // 156
//ZZ    LINX_(__NR_sched_getscheduler,     sys_sched_getscheduler),    // 157
//ZZ    LINX_(__NR_sched_get_priority_max, sys_sched_get_priority_max),// 159
//ZZ 
//ZZ    LINX_(__NR_sched_get_priority_min, sys_sched_get_priority_min),// 160
//ZZ //zz    //LINX?(__NR_sched_rr_get_interval,  sys_sched_rr_get_interval), // 161 */*
//ZZ    LINX_(__NR_setresuid,         sys_setresuid16),    // 164
//ZZ 
//ZZ    LINXY(__NR_getresuid,         sys_getresuid16),    // 165
//ZZ //   PLAXY(__NR_vm86,              sys_vm86),           // 166 x86/Linux-only
//ZZ //   GENX_(__NR_query_module,      sys_ni_syscall),     // 167
//ZZ    GENXY(__NR_poll,              sys_poll),           // 168
//ZZ //zz    //   (__NR_nfsservctl,        sys_nfsservctl),     // 169 */Linux
//ZZ //zz 
//ZZ    LINX_(__NR_setresgid,         sys_setresgid16),    // 170
//ZZ    LINXY(__NR_getresgid,         sys_getresgid16),    // 171
//ZZ    LINXY(__NR_prctl,             sys_prctl),          // 172
//ZZ    LINXY(__NR_rt_sigaction,      sys_rt_sigaction),   // 174
//ZZ 
//ZZ    LINXY(__NR_rt_sigpending,     sys_rt_sigpending),  // 176
//ZZ    LINXY(__NR_rt_sigtimedwait,   sys_rt_sigtimedwait),// 177
//ZZ 
//ZZ    LINX_(__NR_chown,             sys_chown16),        // 182
//ZZ 
//ZZ    LINX_(__NR_capset,            sys_capset),         // 185
//ZZ    LINXY(__NR_sendfile,          sys_sendfile),       // 187
//ZZ //   GENXY(__NR_getpmsg,           sys_getpmsg),        // 188
//ZZ //   GENX_(__NR_putpmsg,           sys_putpmsg),        // 189
//ZZ 
//ZZ    // Nb: we treat vfork as fork
//ZZ    GENX_(__NR_vfork,             sys_fork),           // 190
//ZZ    GENXY(__NR_ugetrlimit,        sys_getrlimit),      // 191
//ZZ    GENX_(__NR_truncate64,        sys_truncate64),     // 193
//ZZ    GENX_(__NR_ftruncate64,       sys_ftruncate64),    // 194
//ZZ    
//ZZ    PLAXY(__NR_stat64,            sys_stat64),         // 195
//ZZ    PLAXY(__NR_lstat64,           sys_lstat64),        // 196
//ZZ    PLAXY(__NR_fstat64,           sys_fstat64),        // 197
//ZZ    GENX_(__NR_lchown32,          sys_lchown),         // 198
//ZZ    GENX_(__NR_getuid32,          sys_getuid),         // 199
//ZZ 
//ZZ    GENX_(__NR_getgid32,          sys_getgid),         // 200
//ZZ    GENX_(__NR_geteuid32,         sys_geteuid),        // 201
//ZZ    GENX_(__NR_getegid32,         sys_getegid),        // 202
//ZZ    GENX_(__NR_setreuid32,        sys_setreuid),       // 203
//ZZ    GENX_(__NR_setregid32,        sys_setregid),       // 204
//ZZ 
//ZZ    LINX_(__NR_setresuid32,       sys_setresuid),      // 208
//ZZ    LINXY(__NR_getresuid32,       sys_getresuid),      // 209
//ZZ 
//ZZ    LINX_(__NR_setresgid32,       sys_setresgid),      // 210
//ZZ    LINXY(__NR_getresgid32,       sys_getresgid),      // 211
//ZZ    GENX_(__NR_chown32,           sys_chown),          // 212
//ZZ    GENX_(__NR_setuid32,          sys_setuid),         // 213
//ZZ    GENX_(__NR_setgid32,          sys_setgid),         // 214
//ZZ 
//ZZ    LINX_(__NR_setfsuid32,        sys_setfsuid),       // 215
//ZZ    LINX_(__NR_setfsgid32,        sys_setfsgid),       // 216
//ZZ //zz    //   (__NR_pivot_root,        sys_pivot_root),     // 217 */Linux
//ZZ    GENXY(__NR_mincore,           sys_mincore),        // 218
//ZZ 
//ZZ    LINXY(__NR_fcntl64,           sys_fcntl64),        // 221
//ZZ //   GENX_(222,                    sys_ni_syscall),     // 222
//ZZ //   PLAXY(223,                    sys_syscall223),     // 223 // sys_bproc?
//ZZ 
//ZZ    LINX_(__NR_setxattr,          sys_setxattr),       // 226
//ZZ    LINX_(__NR_lsetxattr,         sys_lsetxattr),      // 227
//ZZ    LINX_(__NR_fsetxattr,         sys_fsetxattr),      // 228
//ZZ 
//ZZ    LINXY(__NR_fgetxattr,         sys_fgetxattr),      // 231
//ZZ    LINXY(__NR_listxattr,         sys_listxattr),      // 232
//ZZ    LINXY(__NR_llistxattr,        sys_llistxattr),     // 233
//ZZ    LINXY(__NR_flistxattr,        sys_flistxattr),     // 234
//ZZ 
//ZZ    LINX_(__NR_removexattr,       sys_removexattr),    // 235
//ZZ    LINX_(__NR_lremovexattr,      sys_lremovexattr),   // 236
//ZZ    LINX_(__NR_fremovexattr,      sys_fremovexattr),   // 237
//ZZ    LINXY(__NR_tkill,             sys_tkill),          // 238 */Linux
//ZZ    LINXY(__NR_sendfile64,        sys_sendfile64),     // 239
//ZZ 
//ZZ    LINXY(__NR_futex,             sys_futex),             // 240
//ZZ    LINXY(__NR_sched_getaffinity, sys_sched_getaffinity), // 242
//ZZ //   PLAX_(__NR_set_thread_area,   sys_set_thread_area),   // 243
//ZZ //   PLAX_(__NR_get_thread_area,   sys_get_thread_area),   // 244
//ZZ 
//ZZ    LINXY(__NR_io_setup,          sys_io_setup),       // 245
//ZZ    LINX_(__NR_io_destroy,        sys_io_destroy),     // 246
//ZZ    LINXY(__NR_io_getevents,      sys_io_getevents),   // 247
//ZZ    LINX_(__NR_io_submit,         sys_io_submit),      // 248
//ZZ    LINXY(__NR_io_cancel,         sys_io_cancel),      // 249
//ZZ 
//ZZ //   LINX_(__NR_fadvise64,         sys_fadvise64),      // 250 */(Linux?)
//ZZ    GENX_(251,                    sys_ni_syscall),     // 251
//ZZ //   GENXY(__NR_lookup_dcookie,    sys_lookup_dcookie), // 253
//ZZ    LINXY(__NR_epoll_create,      sys_epoll_create),   // 254
//ZZ 
//ZZ    LINX_(__NR_epoll_ctl,         sys_epoll_ctl),         // 255
//ZZ    LINXY(__NR_epoll_wait,        sys_epoll_wait),        // 256
//ZZ //zz    //   (__NR_remap_file_pages,  sys_remap_file_pages),  // 257 */Linux
//ZZ    LINX_(__NR_set_tid_address,   sys_set_tid_address),   // 258
//ZZ    LINXY(__NR_timer_create,      sys_timer_create),      // 259
//ZZ 
//ZZ    LINXY(__NR_timer_settime,     sys_timer_settime),  // (timer_create+1)
//ZZ    LINXY(__NR_timer_gettime,     sys_timer_gettime),  // (timer_create+2)
//ZZ    LINX_(__NR_timer_getoverrun,  sys_timer_getoverrun),//(timer_create+3)
//ZZ    LINX_(__NR_timer_delete,      sys_timer_delete),   // (timer_create+4)
//ZZ    LINX_(__NR_clock_settime,     sys_clock_settime),  // (timer_create+5)
//ZZ 
//ZZ    LINXY(__NR_clock_getres,      sys_clock_getres),   // (timer_create+7)
//ZZ    LINXY(__NR_clock_nanosleep,   sys_clock_nanosleep),// (timer_create+8) */*
//ZZ    GENXY(__NR_statfs64,          sys_statfs64),       // 268
//ZZ    GENXY(__NR_fstatfs64,         sys_fstatfs64),      // 269
//ZZ 
//ZZ    GENX_(__NR_utimes,            sys_utimes),         // 271
//ZZ //   LINX_(__NR_fadvise64_64,      sys_fadvise64_64),   // 272 */(Linux?)
//ZZ    GENX_(__NR_vserver,           sys_ni_syscall),     // 273
//ZZ    LINX_(__NR_mbind,             sys_mbind),          // 274 ?/?
//ZZ 
//ZZ    LINXY(__NR_get_mempolicy,     sys_get_mempolicy),  // 275 ?/?
//ZZ    LINX_(__NR_set_mempolicy,     sys_set_mempolicy),  // 276 ?/?
//ZZ 
//ZZ    LINXY(__NR_waitid,            sys_waitid),         // 280
//ZZ 
//ZZ    LINX_(__NR_send,              sys_send),
//ZZ    LINXY(__NR_recv,              sys_recv),
//ZZ    LINXY(__NR_recvfrom,          sys_recvfrom),       // 292
//ZZ    LINX_(__NR_semget,            sys_semget),         // 299
//ZZ    LINXY(__NR_semctl,            sys_semctl),         // 300
//ZZ 
//ZZ    LINX_(__NR_request_key,       sys_request_key),    // 287
//ZZ //   LINX_(__NR_ioprio_set,        sys_ioprio_set),     // 289
//ZZ 
//ZZ //   LINX_(__NR_ioprio_get,        sys_ioprio_get),     // 290
//ZZ    LINX_(__NR_inotify_init,    sys_inotify_init),   // 291
//ZZ //   LINX_(__NR_migrate_pages,    sys_migrate_pages),    // 294
//ZZ 
//ZZ    LINX_(__NR_futimesat,    sys_futimesat),        // 326 on arm
//ZZ 
//ZZ    PLAXY(__NR_fstatat64,    sys_fstatat64),        // 300
//ZZ    LINX_(__NR_renameat,       sys_renameat),         // 302
//ZZ    LINX_(__NR_symlinkat,    sys_symlinkat),        // 304
//ZZ 
//ZZ    LINX_(__NR_shmget,            sys_shmget),         //307 
//ZZ //   LINX_(__NR_pselect6,       sys_pselect6),         //
//ZZ 
//ZZ //   LINX_(__NR_unshare,       sys_unshare),          // 310
//ZZ    LINX_(__NR_set_robust_list,    sys_set_robust_list),  // 311
//ZZ    LINXY(__NR_get_robust_list,    sys_get_robust_list),  // 312
//ZZ //   LINX_(__NR_splice,            sys_ni_syscall),       // 313
//ZZ //   LINX_(__NR_sync_file_range,   sys_sync_file_range),  // 314
//ZZ 
//ZZ //   LINX_(__NR_tee,               sys_ni_syscall),       // 315
//ZZ //   LINX_(__NR_vmsplice,          sys_ni_syscall),       // 316
//ZZ    LINXY(__NR_move_pages,        sys_move_pages),       // 317
//ZZ //   LINX_(__NR_getcpu,            sys_ni_syscall),       // 318
//ZZ 
//ZZ    LINXY(__NR_signalfd,          sys_signalfd),         // 321
//ZZ    LINXY(__NR_eventfd,           sys_eventfd),          // 323
//ZZ 
//ZZ 
//ZZ    ///////////////
//ZZ 
//ZZ    // JRS 2010-Jan-03: I believe that all the numbers listed 
//ZZ    // in comments in the table prior to this point (eg "// 326",
//ZZ    // etc) are bogus since it looks to me like they are copied
//ZZ    // verbatim from syswrap-x86-linux.c and they certainly do not
//ZZ    // correspond to what's in include/vki/vki-scnums-arm-linux.h.
//ZZ    // From here onwards, please ensure the numbers are correct.
//ZZ 
//ZZ 
//ZZ    LINXY(__NR_epoll_pwait,       sys_epoll_pwait),      // 346
//ZZ 
//ZZ 
//ZZ    LINXY(__NR_eventfd2,          sys_eventfd2),         // 356
//ZZ    LINXY(__NR_epoll_create1,     sys_epoll_create1),    // 357
//ZZ    LINXY(__NR_preadv,            sys_preadv),           // 361
//ZZ    LINX_(__NR_pwritev,           sys_pwritev),          // 362
//ZZ    LINXY(__NR_rt_tgsigqueueinfo, sys_rt_tgsigqueueinfo),// 363
//ZZ    LINXY(__NR_perf_event_open,   sys_perf_event_open),  // 364
//ZZ 
//ZZ    LINXY(__NR_name_to_handle_at, sys_name_to_handle_at),// 370
//ZZ    LINXY(__NR_open_by_handle_at, sys_open_by_handle_at),// 371
//ZZ    LINXY(__NR_clock_adjtime,     sys_clock_adjtime)     // 372
};


//ZZ /* These are not in the main table because there indexes are not small
//ZZ    integers, but rather values close to one million.  So their
//ZZ    inclusion would force the main table to be huge (about 8 MB). */
//ZZ 
//ZZ static SyscallTableEntry ste___ARM_set_tls
//ZZ    = { WRAPPER_PRE_NAME(arm_linux,sys_set_tls), NULL };
//ZZ 
//ZZ static SyscallTableEntry ste___ARM_cacheflush
//ZZ    = { WRAPPER_PRE_NAME(arm_linux,sys_cacheflush), NULL };

SyscallTableEntry* ML_(get_linux_syscall_entry) ( UInt sysno )
{
   const UInt syscall_main_table_size
      = sizeof(syscall_main_table) / sizeof(syscall_main_table[0]);

   /* Is it in the contiguous initial section of the table? */
   if (sysno < syscall_main_table_size) {
      SyscallTableEntry* sys = &syscall_main_table[sysno];
      if (sys->before == NULL)
         return NULL; /* no entry */
      else
         return sys;
   }

//ZZ    /* Check if it's one of the out-of-line entries. */
//ZZ    switch (sysno) {
//ZZ       case __NR_ARM_set_tls:    return &ste___ARM_set_tls;
//ZZ       case __NR_ARM_cacheflush: return &ste___ARM_cacheflush;
//ZZ       default: break;
//ZZ    }

   /* Can't find a wrapper */
   return NULL;
}

#endif // defined(VGP_arm64_linux)

/*--------------------------------------------------------------------*/
/*--- end                                    syswrap-arm64-linux.c ---*/
/*--------------------------------------------------------------------*/
