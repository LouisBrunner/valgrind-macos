
/*--------------------------------------------------------------------*/
/*--- Platform-specific syscalls stuff.      syswrap-ppc64-linux.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2005-2017 Nicholas Nethercote <njn@valgrind.org>
   Copyright (C) 2005-2017 Cerion Armour-Brown <cerion@open-works.co.uk>

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

#if defined(VGP_ppc64be_linux) || defined(VGP_ppc64le_linux)

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
#include "pub_core_sigframe.h"      // For VG_(sigframe_destroy)()
#include "pub_core_signals.h"
#include "pub_core_syscall.h"
#include "pub_core_syswrap.h"
#include "pub_core_tooliface.h"

#include "priv_types_n_macros.h"
#include "priv_syswrap-generic.h"   /* for decls of generic wrappers */
#include "priv_syswrap-linux.h"     /* for decls of linux-ish wrappers */
#include "priv_syswrap-main.h"


/* ---------------------------------------------------------------------
   clone() handling
   ------------------------------------------------------------------ */

/* Call f(arg1), but first switch stacks, using 'stack' as the new
   stack, and use 'retaddr' as f's return-to address.  Also, clear all
   the integer registers before entering f.*/
__attribute__((noreturn))
void ML_(call_on_new_stack_0_1) ( Addr stack,
                                  Addr retaddr,
                                  void (*f_desc)(Word),
                                  Word arg1 );
//    r3 = stack
//    r4 = retaddr
//    r5 = function descriptor
//    r6 = arg1
/* On PPC64, a func ptr is represented by a TOC entry ptr.
   This TOC entry contains three words; the first word is the function
   address, the second word is the TOC ptr (r2), and the third word is
   the static chain value. */
asm(
#if defined(VGP_ppc64be_linux)
"   .align   2\n"
"   .globl   vgModuleLocal_call_on_new_stack_0_1\n"
"   .section \".opd\",\"aw\"\n"
"   .align   3\n"
"vgModuleLocal_call_on_new_stack_0_1:\n"
"   .quad    .vgModuleLocal_call_on_new_stack_0_1,.TOC.@tocbase,0\n"
"   .previous\n"
"   .type    .vgModuleLocal_call_on_new_stack_0_1,@function\n"
"   .globl   .vgModuleLocal_call_on_new_stack_0_1\n"
".vgModuleLocal_call_on_new_stack_0_1:\n"
"   mr    %r1,%r3\n\t"     // stack to %sp
"   mtlr  %r4\n\t"         // retaddr to %lr
"   ld 5,0(5)\n\t"         // load f_ptr from f_desc[0]
"   mtctr %r5\n\t"         // f_ptr to count reg
"   mr %r3,%r6\n\t"        // arg1 to %r3
"   li 0,0\n\t"            // zero all GP regs
"   li 4,0\n\t"
"   li 5,0\n\t"
"   li 6,0\n\t"
"   li 7,0\n\t"
"   li 8,0\n\t"
"   li 9,0\n\t"
"   li 10,0\n\t"
"   li 11,0\n\t"
"   li 12,0\n\t"
"   li 13,0\n\t"
"   li 14,0\n\t"
"   li 15,0\n\t"
"   li 16,0\n\t"
"   li 17,0\n\t"
"   li 18,0\n\t"
"   li 19,0\n\t"
"   li 20,0\n\t"
"   li 21,0\n\t"
"   li 22,0\n\t"
"   li 23,0\n\t"
"   li 24,0\n\t"
"   li 25,0\n\t"
"   li 26,0\n\t"
"   li 27,0\n\t"
"   li 28,0\n\t"
"   li 29,0\n\t"
"   li 30,0\n\t"
"   li 31,0\n\t"
"   mtxer 0\n\t"           // CAB: Need this?
"   mtcr 0\n\t"            // CAB: Need this?
"   bctr\n\t"              // jump to dst
"   trap\n"                // should never get here
#else
//  ppc64le_linux
"   .align   2\n"
"   .globl   vgModuleLocal_call_on_new_stack_0_1\n"
"vgModuleLocal_call_on_new_stack_0_1:\n"
"   .type    .vgModuleLocal_call_on_new_stack_0_1,@function\n"
"#if _CALL_ELF == 2 \n"
"0: addis        2,12,.TOC.-0b@ha\n"
"   addi         2,2,.TOC.-0b@l\n"
"#endif\n"
".localentry vgModuleLocal_call_on_new_stack_0_1, .-vgModuleLocal_call_on_new_stack_0_1\n"
"   mr    %r1,%r3\n\t"     // stack to %sp
"   mtlr  %r4\n\t"         // retaddr to %lr
"   mtctr %r5\n\t"         // f_ptr to count reg
"   mr %r3,%r6\n\t"        // arg1 to %r3
"   li 0,0\n\t"            // zero all GP regs
"   li 4,0\n\t"
"   li 5,0\n\t"
"   li 6,0\n\t"
"   li 7,0\n\t"
"   li 8,0\n\t"
"   li 9,0\n\t"
"   li 10,0\n\t"
"   li 11,0\n\t"
"   li 12,0\n\t"
"   li 13,0\n\t"
"   li 14,0\n\t"
"   li 15,0\n\t"
"   li 16,0\n\t"
"   li 17,0\n\t"
"   li 18,0\n\t"
"   li 19,0\n\t"
"   li 20,0\n\t"
"   li 21,0\n\t"
"   li 22,0\n\t"
"   li 23,0\n\t"
"   li 24,0\n\t"
"   li 25,0\n\t"
"   li 26,0\n\t"
"   li 27,0\n\t"
"   li 28,0\n\t"
"   li 29,0\n\t"
"   li 30,0\n\t"
"   li 31,0\n\t"
"   mtxer 0\n\t"           // CAB: Need this?
"   mtcr 0\n\t"            // CAB: Need this?
"   bctr\n\t"              // jump to dst
"   trap\n"                // should never get here
#endif
);


/*
        Perform a clone system call.  clone is strange because it has
        fork()-like return-twice semantics, so it needs special
        handling here.

        Upon entry, we have:

            word (fn)(void*)    in r3
            void* child_stack   in r4
            word flags          in r5
            void* arg           in r6
            pid_t* child_tid    in r7
            pid_t* parent_tid   in r8
            void* ???           in r9

        Note: r3 contains fn desc ptr, not fn ptr -- p_fn = p_fn_desc[0]
        System call requires:

            int    $__NR_clone  in r0  (sc number)
            int    flags        in r3  (sc arg1)
            void*  child_stack  in r4  (sc arg2)
            pid_t* parent_tid   in r5  (sc arg3)
            ??     child_tls    in r6  (sc arg4)
            pid_t* child_tid    in r7  (sc arg5)
            void*  ???          in r8  (sc arg6)

        Returns a ULong encoded as: top half is %cr following syscall,
        low half is syscall return value (r3).
 */
#define __NR_CLONE        VG_STRINGIFY(__NR_clone)
#define __NR_EXIT         VG_STRINGIFY(__NR_exit)

// See priv_syswrap-linux.h for arg profile.
asm(
#if defined(VGP_ppc64be_linux)
"   .align   2\n"
"   .globl   do_syscall_clone_ppc64_linux\n"
"   .section \".opd\",\"aw\"\n"
"   .align   3\n"
"do_syscall_clone_ppc64_linux:\n"
"   .quad    .do_syscall_clone_ppc64_linux,.TOC.@tocbase,0\n"
"   .previous\n"
"   .type    .do_syscall_clone_ppc64_linux,@function\n"
"   .globl   .do_syscall_clone_ppc64_linux\n"
".do_syscall_clone_ppc64_linux:\n"
"       stdu    1,-64(1)\n"
"       std     29,40(1)\n"
"       std     30,48(1)\n"
"       std     31,56(1)\n"
"       mr      30,3\n"              // preserve fn
"       mr      31,6\n"              // preserve arg

        // setup child stack
"       rldicr  4,4, 0,59\n"         // trim sp to multiple of 16 bytes
                                     // (r4 &= ~0xF)
"       li      0,0\n"
"       stdu    0,-32(4)\n"          // make initial stack frame
"       mr      29,4\n"              // preserve sp

        // setup syscall
"       li      0,"__NR_CLONE"\n"    // syscall number
"       mr      3,5\n"               // syscall arg1: flags
        // r4 already setup          // syscall arg2: child_stack
"       mr      5,8\n"               // syscall arg3: parent_tid
"       mr      6,13\n"              // syscall arg4: REAL THREAD tls
"       mr      7,7\n"               // syscall arg5: child_tid
"       mr      8,8\n"               // syscall arg6: ????
"       mr      9,9\n"               // syscall arg7: ????

"       sc\n"                        // clone()

"       mfcr    4\n"                 // CR now in low half r4
"       sldi    4,4,32\n"            // CR now in hi half r4

"       sldi    3,3,32\n"
"       srdi    3,3,32\n"            // zero out hi half r3

"       or      3,3,4\n"             // r3 = CR : syscall-retval
"       cmpwi   3,0\n"               // child if retval == 0 (note, cmpw)
"       bne     1f\n"                // jump if !child

        /* CHILD - call thread function */
        /* Note: 2.4 kernel doesn't set the child stack pointer,
           so we do it here.
           That does leave a small window for a signal to be delivered
           on the wrong stack, unfortunately. */
"       mr      1,29\n"
"       ld      30, 0(30)\n"         // convert fn desc ptr to fn ptr
"       mtctr   30\n"                // ctr reg = fn
"       mr      3,31\n"              // r3 = arg
"       bctrl\n"                     // call fn()

        // exit with result
"       li      0,"__NR_EXIT"\n"
"       sc\n"

        // Exit returned?!
"       .long   0\n"

        // PARENT or ERROR - return
"1:     ld      29,40(1)\n"
"       ld      30,48(1)\n"
"       ld      31,56(1)\n"
"       addi    1,1,64\n"
"       blr\n"
#else
"   .align   2\n"
"   .globl   do_syscall_clone_ppc64_linux\n"
"   .type    do_syscall_clone_ppc64_linux,@function\n"
"do_syscall_clone_ppc64_linux:\n"
"   .globl   .do_syscall_clone_ppc64_linux\n"
".do_syscall_clone_ppc64_linux:\n"
"#if _CALL_ELF == 2 \n"
"0:     addis        2,12,.TOC.-0b@ha \n"
"       addi         2,2,.TOC.-0b@l \n"
"#endif \n"
"   .localentry  do_syscall_clone_ppc64_linux, .-do_syscall_clone_ppc64_linux \n"
"       stdu    1,-64(1)\n"
"       std     29,40(1)\n"
"       std     30,48(1)\n"
"       std     31,56(1)\n"
"       mr      30,3\n"              // preserve fn
"       mr      31,6\n"              // preserve arg

        // setup child stack
"       rldicr  4,4, 0,59\n"         // trim sp to multiple of 16 bytes
                                     // (r4 &= ~0xF)
"       li      0,0\n"
"       stdu    0,-32(4)\n"          // make initial stack frame
"       mr      29,4\n"              // preserve sp

        // setup syscall
"       li      0,"__NR_CLONE"\n"    // syscall number
"       mr      3,5\n"               // syscall arg1: flags
        // r4 already setup          // syscall arg2: child_stack
"       mr      5,8\n"               // syscall arg3: parent_tid
"       mr      6,13\n"              // syscall arg4: REAL THREAD tls
"       mr      7,7\n"               // syscall arg5: child_tid
"       mr      8,8\n"               // syscall arg6: ????
"       mr      9,9\n"               // syscall arg7: ????

"       sc\n"                        // clone()

"       mfcr    4\n"                 // CR now in low half r4
"       sldi    4,4,32\n"            // CR now in hi half r4

"       sldi    3,3,32\n"
"       srdi    3,3,32\n"            // zero out hi half r3

"       or      3,3,4\n"             // r3 = CR : syscall-retval
"       cmpwi   3,0\n"               // child if retval == 0 (note, cmpw)
"       bne     1f\n"                // jump if !child

        /* CHILD - call thread function */
        /* Note: 2.4 kernel doesn't set the child stack pointer,
           so we do it here.
           That does leave a small window for a signal to be delivered
           on the wrong stack, unfortunately. */
"       mr      1,29\n"
"       mtctr   30\n"                // ctr reg = fn
"       mr      3,31\n"              // r3 = arg
"       bctrl\n"                     // call fn()

        // exit with result
"       li      0,"__NR_EXIT"\n"
"       sc\n"

        // Exit returned?!
"       .long   0\n"

        // PARENT or ERROR - return
"1:     ld      29,40(1)\n"
"       ld      30,48(1)\n"
"       ld      31,56(1)\n"
"       addi    1,1,64\n"
"       blr\n"
#endif
);

#undef __NR_CLONE
#undef __NR_EXIT

/* ---------------------------------------------------------------------
   More thread stuff
   ------------------------------------------------------------------ */

void VG_(cleanup_thread) ( ThreadArchState* arch )
{
}  

/* ---------------------------------------------------------------------
   PRE/POST wrappers for ppc64/Linux-specific syscalls
   ------------------------------------------------------------------ */

#define PRE(name)       DEFN_PRE_TEMPLATE(ppc64_linux, name)
#define POST(name)      DEFN_POST_TEMPLATE(ppc64_linux, name)

/* Add prototypes for the wrappers declared here, so that gcc doesn't
   harass us for not having prototypes.  Really this is a kludge --
   the right thing to do is to make these wrappers 'static' since they
   aren't visible outside this file, but that requires even more macro
   magic. */

DECL_TEMPLATE(ppc64_linux, sys_mmap);
//zz DECL_TEMPLATE(ppc64_linux, sys_mmap2);
//zz DECL_TEMPLATE(ppc64_linux, sys_stat64);
//zz DECL_TEMPLATE(ppc64_linux, sys_lstat64);
//zz DECL_TEMPLATE(ppc64_linux, sys_fstat64);
//zz DECL_TEMPLATE(ppc64_linux, sys_sigreturn);
DECL_TEMPLATE(ppc64_linux, sys_rt_sigreturn);
DECL_TEMPLATE(ppc64_linux, sys_fadvise64);
DECL_TEMPLATE(ppc64_linux, sys_ptrace);

PRE(sys_mmap)
{
   SysRes r;

   PRINT("sys_mmap ( %#lx, %lu, %lu, %lu, %lu, %lu )",
         ARG1, ARG2, ARG3, ARG4, ARG5, ARG6 );
   PRE_REG_READ6(long, "mmap",
                 unsigned long, start, unsigned long, length,
                 unsigned long, prot,  unsigned long, flags,
                 unsigned long, fd,    unsigned long, offset);

   r = ML_(generic_PRE_sys_mmap)( tid, ARG1, ARG2, ARG3, ARG4, ARG5, 
                                       (Off64T)ARG6 );
   SET_STATUS_from_SysRes(r);
}

//zz PRE(sys_mmap2)
//zz {
//zz    SysRes r;
//zz 
//zz    // Exactly like old_mmap() except:
//zz    //  - the file offset is specified in 4K units rather than bytes,
//zz    //    so that it can be used for files bigger than 2^32 bytes.
//zz    PRINT("sys_mmap2 ( %p, %llu, %d, %d, %d, %d )",
//zz          ARG1, (ULong)ARG2, ARG3, ARG4, ARG5, ARG6 );
//zz    PRE_REG_READ6(long, "mmap2",
//zz                  unsigned long, start, unsigned long, length,
//zz                  unsigned long, prot,  unsigned long, flags,
//zz                  unsigned long, fd,    unsigned long, offset);
//zz 
//zz    r = ML_(generic_PRE_sys_mmap)( tid, ARG1, ARG2, ARG3, ARG4, ARG5, 
//zz                                        4096 * (Off64T)ARG6 );
//zz    SET_STATUS_from_SysRes(r);
//zz }
//zz 
//zz // XXX: lstat64/fstat64/stat64 are generic, but not necessarily
//zz // applicable to every architecture -- I think only to 32-bit archs.
//zz // We're going to need something like linux/core_os32.h for such
//zz // things, eventually, I think.  --njn
//zz PRE(sys_stat64)
//zz {
//zz    PRINT("sys_stat64 ( %p, %p )",ARG1,ARG2);
//zz    PRE_REG_READ2(long, "stat64", char *, file_name, struct stat64 *, buf);
//zz    PRE_MEM_RASCIIZ( "stat64(file_name)", ARG1 );
//zz    PRE_MEM_WRITE( "stat64(buf)", ARG2, sizeof(struct vki_stat64) );
//zz }
//zz 
//zz POST(sys_stat64)
//zz {
//zz    POST_MEM_WRITE( ARG2, sizeof(struct vki_stat64) );
//zz }
//zz 
//zz PRE(sys_lstat64)
//zz {
//zz    PRINT("sys_lstat64 ( %p(%s), %p )",ARG1,ARG1,ARG2);
//zz    PRE_REG_READ2(long, "lstat64", char *, file_name, struct stat64 *, buf);
//zz    PRE_MEM_RASCIIZ( "lstat64(file_name)", ARG1 );
//zz    PRE_MEM_WRITE( "lstat64(buf)", ARG2, sizeof(struct vki_stat64) );
//zz }
//zz 
//zz POST(sys_lstat64)
//zz {
//zz    vg_assert(SUCCESS);
//zz    if (RES == 0) {
//zz       POST_MEM_WRITE( ARG2, sizeof(struct vki_stat64) );
//zz    }
//zz }
//zz 
//zz PRE(sys_fstat64)
//zz {
//zz   PRINT("sys_fstat64 ( %d, %p )",ARG1,ARG2);
//zz   PRE_REG_READ2(long, "fstat64", unsigned long, fd, struct stat64 *, buf);
//zz   PRE_MEM_WRITE( "fstat64(buf)", ARG2, sizeof(struct vki_stat64) );
//zz }
//zz 
//zz POST(sys_fstat64)
//zz {
//zz   POST_MEM_WRITE( ARG2, sizeof(struct vki_stat64) );
//zz }

PRE(sys_fadvise64)
{
   PRINT("sys_fadvise64 ( %" FMT_REGWORD "d, %" FMT_REGWORD "d, "
                         "%" FMT_REGWORD "u, %" FMT_REGWORD "d )",
         SARG1, SARG2, ARG3, SARG4);
   PRE_REG_READ4(long, "fadvise64",
                 int, fd, vki_loff_t, offset, vki_size_t, len, int, advice);
}

PRE(sys_rt_sigreturn)
{
   /* See comments on PRE(sys_rt_sigreturn) in syswrap-amd64-linux.c for
      an explanation of what follows. */

   //ThreadState* tst;
   PRINT("sys_rt_sigreturn ( )");

   vg_assert(VG_(is_valid_tid)(tid));
   vg_assert(tid >= 1 && tid < VG_N_THREADS);
   vg_assert(VG_(is_running_thread)(tid));

   ///* Adjust esp to point to start of frame; skip back up over handler
   //   ret addr */
   //tst = VG_(get_ThreadState)(tid);
   //tst->arch.vex.guest_ESP -= sizeof(Addr);
   // Should we do something equivalent on ppc64-linux?  Who knows.

   ///* This is only so that the EIP is (might be) useful to report if
   //   something goes wrong in the sigreturn */
   //ML_(fixup_guest_state_to_restart_syscall)(&tst->arch);
   // Should we do something equivalent on ppc64?  Who knows.

   /* Restore register state from frame and remove it */
   VG_(sigframe_destroy)(tid, True);

   /* Tell the driver not to update the guest state with the "result",
      and set a bogus result to keep it happy. */
   *flags |= SfNoWriteResult;
   SET_STATUS_Success(0);

   /* Check to see if any signals arose as a result of this. */
   *flags |= SfPollAfter;
}

// ARG3 is only used for pointers into the traced process's address
// space and for offsets into the traced process's struct
// user_regs_struct. It is never a pointer into this process's memory
// space, and we should therefore not check anything it points to.
// powerpc does have other ways to get/set registers, we only support
// GET/SETREGSET for now.
PRE(sys_ptrace)
{
   PRINT("sys_ptrace ( %" FMT_REGWORD "d, %" FMT_REGWORD "d, "
                      "%#" FMT_REGWORD "x, %#" FMT_REGWORD "x )",
         SARG1, SARG2, ARG3, ARG4);
   PRE_REG_READ4(int, "ptrace",
                 long, request, long, pid, long, addr, long, data);
   switch (ARG1) {
   case VKI_PTRACE_PEEKTEXT:
   case VKI_PTRACE_PEEKDATA:
   case VKI_PTRACE_PEEKUSR:
      PRE_MEM_WRITE( "ptrace(peek)", ARG4,
                     sizeof (long));
      break;
   case VKI_PTRACE_GETEVENTMSG:
      PRE_MEM_WRITE( "ptrace(geteventmsg)", ARG4, sizeof(unsigned long));
      break;
   case VKI_PTRACE_GETSIGINFO:
      PRE_MEM_WRITE( "ptrace(getsiginfo)", ARG4, sizeof(vki_siginfo_t));
      break;
   case VKI_PTRACE_SETSIGINFO:
      PRE_MEM_READ( "ptrace(setsiginfo)", ARG4, sizeof(vki_siginfo_t));
      break;
   case VKI_PTRACE_GETREGSET:
      ML_(linux_PRE_getregset)(tid, ARG3, ARG4);
      break;
   case VKI_PTRACE_SETREGSET:
      ML_(linux_PRE_setregset)(tid, ARG3, ARG4);
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
      POST_MEM_WRITE( ARG4, sizeof (long));
      break;
   case VKI_PTRACE_GETEVENTMSG:
      POST_MEM_WRITE( ARG4, sizeof(unsigned long));
      break;
   case VKI_PTRACE_GETSIGINFO:
      /* XXX: This is a simplification. Different parts of the
       * siginfo_t are valid depending on the type of signal.
       */
      POST_MEM_WRITE( ARG4, sizeof(vki_siginfo_t));
      break;
   case VKI_PTRACE_GETREGSET:
      ML_(linux_POST_getregset)(tid, ARG3, ARG4);
      break;
   default:
      break;
   }
}

#undef PRE
#undef POST

/* ---------------------------------------------------------------------
   The ppc64/Linux syscall table
   ------------------------------------------------------------------ */

/* Add an ppc64-linux specific wrapper to a syscall table. */
#define PLAX_(sysno, name)    WRAPPER_ENTRY_X_(ppc64_linux, sysno, name) 
#define PLAXY(sysno, name)    WRAPPER_ENTRY_XY(ppc64_linux, sysno, name)

// This table maps from __NR_xxx syscall numbers (from
// linux/include/asm-ppc/unistd.h) to the appropriate PRE/POST sys_foo()
// wrappers on ppc64 (as per sys_call_table in linux/arch/ppc/kernel/entry.S).
//
// For those syscalls not handled by Valgrind, the annotation indicate its
// arch/OS combination, eg. */* (generic), */Linux (Linux only), ?/?
// (unknown).

static SyscallTableEntry syscall_table[] = {
// _____(__NR_restart_syscall,   sys_restart_syscall),    //   0
   GENX_(__NR_exit,              sys_exit),               //   1
   GENX_(__NR_fork,              sys_fork),               //   2
   GENXY(__NR_read,              sys_read),               //   3
   GENX_(__NR_write,             sys_write),              //   4

   GENXY(__NR_open,              sys_open),               //   5
   GENX_(__NR_close,             sys_close),              //   6
   GENXY(__NR_waitpid,           sys_waitpid),            //   7
   GENXY(__NR_creat,             sys_creat),              //   8
   GENX_(__NR_link,              sys_link),               //   9

   GENX_(__NR_unlink,            sys_unlink),             //  10
   GENX_(__NR_execve,            sys_execve),             //  11
   GENX_(__NR_chdir,             sys_chdir),              //  12
   GENXY(__NR_time,              sys_time),               //  13
   GENX_(__NR_mknod,             sys_mknod),              //  14

   GENX_(__NR_chmod,             sys_chmod),              //  15
   GENX_(__NR_lchown,            sys_lchown),             //  16
// _____(__NR_break,             sys_break),              //  17
// _____(__NR_oldstat,           sys_oldstat),            //  18
   LINX_(__NR_lseek,             sys_lseek),              //  19

   GENX_(__NR_getpid,            sys_getpid),             //  20
   LINX_(__NR_mount,             sys_mount),              //  21
// _____(__NR_umount,            sys_umount),             //  22
   GENX_(__NR_setuid,            sys_setuid),             //  23
   GENX_(__NR_getuid,            sys_getuid),             //  24

// _____(__NR_stime,             sys_stime),              //  25
   PLAXY(__NR_ptrace,            sys_ptrace),             //  26
   GENX_(__NR_alarm,             sys_alarm),              //  27
// _____(__NR_oldfstat,          sys_oldfstat),           //  28
   GENX_(__NR_pause,             sys_pause),              //  29

   LINX_(__NR_utime,             sys_utime),              //  30
// _____(__NR_stty,              sys_stty),               //  31
// _____(__NR_gtty,              sys_gtty),               //  32
   GENX_(__NR_access,            sys_access),             //  33
// _____(__NR_nice,              sys_nice),               //  34

// _____(__NR_ftime,             sys_ftime),              //  35
   GENX_(__NR_sync,              sys_sync),               //  36
   GENX_(__NR_kill,              sys_kill),               //  37
   GENX_(__NR_rename,            sys_rename),             //  38
   GENX_(__NR_mkdir,             sys_mkdir),              //  39

   GENX_(__NR_rmdir,             sys_rmdir),              //  40
   GENXY(__NR_dup,               sys_dup),                //  41
   LINXY(__NR_pipe,              sys_pipe),               //  42
   GENXY(__NR_times,             sys_times),              //  43
// _____(__NR_prof,              sys_prof),               //  44

   GENX_(__NR_brk,               sys_brk),                //  45
   GENX_(__NR_setgid,            sys_setgid),             //  46
   GENX_(__NR_getgid,            sys_getgid),             //  47
// _____(__NR_signal,            sys_signal),             //  48
   GENX_(__NR_geteuid,           sys_geteuid),            //  49

   GENX_(__NR_getegid,           sys_getegid),            //  50
   GENX_(__NR_acct,              sys_acct),               //  51
   LINX_(__NR_umount2,           sys_umount),             //  52
// _____(__NR_lock,              sys_lock),               //  53
   LINXY(__NR_ioctl,             sys_ioctl),              //  54

   LINXY(__NR_fcntl,             sys_fcntl),              //  55
// _____(__NR_mpx,               sys_mpx),                //  56
   GENX_(__NR_setpgid,           sys_setpgid),            //  57
// _____(__NR_ulimit,            sys_ulimit),             //  58
// _____(__NR_oldolduname,       sys_oldolduname),        //  59

   GENX_(__NR_umask,             sys_umask),              //  60
   GENX_(__NR_chroot,            sys_chroot),             //  61
// _____(__NR_ustat,             sys_ustat),              //  62
   GENXY(__NR_dup2,              sys_dup2),               //  63
   GENX_(__NR_getppid,           sys_getppid),            //  64

   GENX_(__NR_getpgrp,           sys_getpgrp),            //  65
   GENX_(__NR_setsid,            sys_setsid),             //  66
// _____(__NR_sigaction,         sys_sigaction),          //  67
// _____(__NR_sgetmask,          sys_sgetmask),           //  68
// _____(__NR_ssetmask,          sys_ssetmask),           //  69

   GENX_(__NR_setreuid,          sys_setreuid),           //  70
   GENX_(__NR_setregid,          sys_setregid),           //  71
// _____(__NR_sigsuspend,        sys_sigsuspend),         //  72
// _____(__NR_sigpending,        sys_sigpending),         //  73
// _____(__NR_sethostname,       sys_sethostname),        //  74

   GENX_(__NR_setrlimit,         sys_setrlimit),          //  75
// _____(__NR_getrlimit,         sys_getrlimit),          //  76
   GENXY(__NR_getrusage,         sys_getrusage),          //  77
   GENXY(__NR_gettimeofday,      sys_gettimeofday),       //  78
// _____(__NR_settimeofday,      sys_settimeofday),       //  79

   GENXY(__NR_getgroups,         sys_getgroups),          //  80
   GENX_(__NR_setgroups,         sys_setgroups),          //  81
// _____(__NR_select,            sys_select),             //  82
   GENX_(__NR_symlink,           sys_symlink),            //  83
// _____(__NR_oldlstat,          sys_oldlstat),           //  84

   GENXY(__NR_readlink,          sys_readlink),           //  85
// _____(__NR_uselib,            sys_uselib),             //  86
// _____(__NR_swapon,            sys_swapon),             //  87
// _____(__NR_reboot,            sys_reboot),             //  88
// _____(__NR_readdir,           sys_readdir),            //  89

   PLAX_(__NR_mmap,              sys_mmap),               //  90
   GENXY(__NR_munmap,            sys_munmap),             //  91
   GENX_(__NR_truncate,          sys_truncate),           //  92
   GENX_(__NR_ftruncate,         sys_ftruncate),          //  93
   GENX_(__NR_fchmod,            sys_fchmod),             //  94
   
   GENX_(__NR_fchown,            sys_fchown),             //  95
   GENX_(__NR_getpriority,       sys_getpriority),        //  96
   GENX_(__NR_setpriority,       sys_setpriority),        //  97
// _____(__NR_profil,            sys_profil),             //  98
   GENXY(__NR_statfs,            sys_statfs),             //  99

   GENXY(__NR_fstatfs,           sys_fstatfs),            // 100
// _____(__NR_ioperm,            sys_ioperm),             // 101
   LINXY(__NR_socketcall,        sys_socketcall),         // 102
   LINXY(__NR_syslog,            sys_syslog),             // 103
   GENXY(__NR_setitimer,         sys_setitimer),          // 104

   GENXY(__NR_getitimer,         sys_getitimer),          // 105
   GENXY(__NR_stat,              sys_newstat),            // 106
   GENXY(__NR_lstat,             sys_newlstat),           // 107
   GENXY(__NR_fstat,             sys_newfstat),           // 108
// _____(__NR_olduname,          sys_olduname),           // 109

// _____(__NR_iopl,              sys_iopl),               // 110
   LINX_(__NR_vhangup,           sys_vhangup),            // 111
// _____(__NR_idle,              sys_idle),               // 112
// _____(__NR_vm86,              sys_vm86),               // 113
   GENXY(__NR_wait4,             sys_wait4),              // 114

// _____(__NR_swapoff,           sys_swapoff),            // 115
   LINXY(__NR_sysinfo,           sys_sysinfo),            // 116
   LINXY(__NR_ipc,               sys_ipc),                // 117
   GENX_(__NR_fsync,             sys_fsync),              // 118
// _____(__NR_sigreturn,         sys_sigreturn),          // 119

   LINX_(__NR_clone,             sys_clone),              // 120
// _____(__NR_setdomainname,     sys_setdomainname),      // 121
   GENXY(__NR_uname,             sys_newuname),           // 122
// _____(__NR_modify_ldt,        sys_modify_ldt),         // 123
   LINXY(__NR_adjtimex,          sys_adjtimex),           // 124

   GENXY(__NR_mprotect,          sys_mprotect),           // 125
// _____(__NR_sigprocmask,       sys_sigprocmask),        // 126
   GENX_(__NR_create_module,     sys_ni_syscall),         // 127
   LINX_(__NR_init_module,       sys_init_module),        // 128
   LINX_(__NR_delete_module,     sys_delete_module),      // 129

// _____(__NR_get_kernel_syms,   sys_get_kernel_syms),    // 130
   LINX_(__NR_quotactl,          sys_quotactl),           // 131
   GENX_(__NR_getpgid,           sys_getpgid),            // 132
   GENX_(__NR_fchdir,            sys_fchdir),             // 133
// _____(__NR_bdflush,           sys_bdflush),            // 134

// _____(__NR_sysfs,             sys_sysfs),              // 135
   LINX_(__NR_personality,       sys_personality),        // 136
// _____(__NR_afs_syscall,       sys_afs_syscall),        // 137
   LINX_(__NR_setfsuid,          sys_setfsuid),           // 138
   LINX_(__NR_setfsgid,          sys_setfsgid),           // 139

   LINXY(__NR__llseek,           sys_llseek),             // 140
   GENXY(__NR_getdents,          sys_getdents),           // 141
   GENX_(__NR__newselect,        sys_select),             // 142
   GENX_(__NR_flock,             sys_flock),              // 143
   GENX_(__NR_msync,             sys_msync),              // 144

   GENXY(__NR_readv,             sys_readv),              // 145
   GENX_(__NR_writev,            sys_writev),             // 146
   GENX_(__NR_getsid,            sys_getsid),             // 147
   GENX_(__NR_fdatasync,         sys_fdatasync),          // 148
   LINXY(__NR__sysctl,           sys_sysctl),             // 149

   GENX_(__NR_mlock,             sys_mlock),              // 150
   GENX_(__NR_munlock,           sys_munlock),            // 151
   GENX_(__NR_mlockall,          sys_mlockall),           // 152
   LINX_(__NR_munlockall,        sys_munlockall),         // 153
   LINXY(__NR_sched_setparam,    sys_sched_setparam),     // 154

   LINXY(__NR_sched_getparam,         sys_sched_getparam),        // 155
   LINX_(__NR_sched_setscheduler,     sys_sched_setscheduler),    // 156
   LINX_(__NR_sched_getscheduler,     sys_sched_getscheduler),    // 157
   LINX_(__NR_sched_yield,            sys_sched_yield),           // 158
   LINX_(__NR_sched_get_priority_max, sys_sched_get_priority_max),// 159

   LINX_(__NR_sched_get_priority_min, sys_sched_get_priority_min),// 160
   LINXY(__NR_sched_rr_get_interval,  sys_sched_rr_get_interval), // 161
   GENXY(__NR_nanosleep,         sys_nanosleep),          // 162
   GENX_(__NR_mremap,            sys_mremap),             // 163
   LINX_(__NR_setresuid,         sys_setresuid),          // 164

   LINXY(__NR_getresuid,         sys_getresuid),          // 165
// _____(__NR_query_module,      sys_query_module),       // 166
   GENXY(__NR_poll,              sys_poll),               // 167
// _____(__NR_nfsservctl,        sys_nfsservctl),         // 168
   LINX_(__NR_setresgid,         sys_setresgid),          // 169

   LINXY(__NR_getresgid,         sys_getresgid),          // 170
   LINXY(__NR_prctl,             sys_prctl),              // 171
   PLAX_(__NR_rt_sigreturn,      sys_rt_sigreturn),       // 172
   LINXY(__NR_rt_sigaction,      sys_rt_sigaction),       // 173
   LINXY(__NR_rt_sigprocmask,    sys_rt_sigprocmask),     // 174

   LINXY(__NR_rt_sigpending,     sys_rt_sigpending),      // 175
   LINXY(__NR_rt_sigtimedwait,   sys_rt_sigtimedwait),    // 176
   LINXY(__NR_rt_sigqueueinfo,   sys_rt_sigqueueinfo),    // 177
   LINX_(__NR_rt_sigsuspend,     sys_rt_sigsuspend),      // 178
   GENXY(__NR_pread64,           sys_pread64),            // 179

   GENX_(__NR_pwrite64,          sys_pwrite64),           // 180
   GENX_(__NR_chown,             sys_chown),              // 181
   GENXY(__NR_getcwd,            sys_getcwd),             // 182
   LINXY(__NR_capget,            sys_capget),             // 183
   LINX_(__NR_capset,            sys_capset),             // 184

   GENXY(__NR_sigaltstack,       sys_sigaltstack),        // 185
   LINXY(__NR_sendfile,          sys_sendfile),           // 186
// _____(__NR_getpmsg,           sys_getpmsg),            // 187
// _____(__NR_putpmsg,           sys_putpmsg),            // 188
   GENX_(__NR_vfork,             sys_fork),               // 189 treat as fork

   GENXY(__NR_ugetrlimit,        sys_getrlimit),          // 190
   LINX_(__NR_readahead,         sys_readahead),          // 191
// /* #define __NR_mmap2           192     32bit only */
// /* #define __NR_truncate64      193     32bit only */
// /* #define __NR_ftruncate64     194     32bit only */

// /* #define __NR_stat64          195     32bit only */
// /* #define __NR_lstat64         196     32bit only */
// /* #define __NR_fstat64         197     32bit only */
// _____(__NR_pciconfig_read,    sys_pciconfig_read),     // 198
// _____(__NR_pciconfig_write,   sys_pciconfig_write),    // 199

// _____(__NR_pciconfig_iobase,  sys_pciconfig_iobase),   // 200
// _____(__NR_multiplexer,       sys_multiplexer),        // 201
   GENXY(__NR_getdents64,        sys_getdents64),         // 202
   LINX_(__NR_pivot_root,        sys_pivot_root),         // 203
   LINXY(__NR_fcntl64,           sys_fcntl64),            // 204 !!!!?? 32bit only */

   GENX_(__NR_madvise,           sys_madvise),            // 205
// _____(__NR_mincore,           sys_mincore),            // 206
   LINX_(__NR_gettid,            sys_gettid),             // 207
// _____(__NR_tkill,             sys_tkill),              // 208
   LINX_(__NR_setxattr,          sys_setxattr),           // 209

   LINX_(__NR_lsetxattr,         sys_lsetxattr),          // 210
   LINX_(__NR_fsetxattr,         sys_fsetxattr),          // 211
   LINXY(__NR_getxattr,          sys_getxattr),           // 212
   LINXY(__NR_lgetxattr,         sys_lgetxattr),          // 213
   LINXY(__NR_fgetxattr,         sys_fgetxattr),          // 214
   LINXY(__NR_listxattr,         sys_listxattr),          // 215
   LINXY(__NR_llistxattr,        sys_llistxattr),         // 216
   LINXY(__NR_flistxattr,        sys_flistxattr),         // 217
   LINX_(__NR_removexattr,       sys_removexattr),        // 218
   LINX_(__NR_lremovexattr,      sys_lremovexattr),       // 219
   LINX_(__NR_fremovexattr,      sys_fremovexattr),       // 220

   LINXY(__NR_futex,             sys_futex),              // 221
   LINX_(__NR_sched_setaffinity, sys_sched_setaffinity),  // 222
   LINXY(__NR_sched_getaffinity, sys_sched_getaffinity),  // 223
// /* 224 currently unused */

// _____(__NR_tuxcall,           sys_tuxcall),            // 225
// /* #define __NR_sendfile64      226     32bit only */
   LINX_(__NR_io_setup,          sys_io_setup),           // 227
   LINX_(__NR_io_destroy,        sys_io_destroy),         // 228
   LINXY(__NR_io_getevents,      sys_io_getevents),       // 229
   LINX_(__NR_io_submit,         sys_io_submit),          // 230
   LINXY(__NR_io_cancel,         sys_io_cancel),          // 231
   LINX_(__NR_set_tid_address,   sys_set_tid_address),    // 232
   PLAX_(__NR_fadvise64,         sys_fadvise64),          // 233
   LINX_(__NR_exit_group,        sys_exit_group),         // 234

// _____(__NR_lookup_dcookie,    sys_lookup_dcookie),     // 235
   LINXY(__NR_epoll_create,      sys_epoll_create),       // 236
   LINX_(__NR_epoll_ctl,         sys_epoll_ctl),          // 237
   LINXY(__NR_epoll_wait,        sys_epoll_wait),         // 238
// _____(__NR_remap_file_pages,  sys_remap_file_pages),   // 239

   LINXY(__NR_timer_create,      sys_timer_create),       // 240
   LINXY(__NR_timer_settime,     sys_timer_settime),      // 241
   LINXY(__NR_timer_gettime,     sys_timer_gettime),      // 242
   LINX_(__NR_timer_getoverrun,  sys_timer_getoverrun),   // 243
   LINX_(__NR_timer_delete,      sys_timer_delete),       // 244
   LINX_(__NR_clock_settime,     sys_clock_settime),      // 245
   LINXY(__NR_clock_gettime,     sys_clock_gettime),      // 246
   LINXY(__NR_clock_getres,      sys_clock_getres),       // 247
   LINXY(__NR_clock_nanosleep,   sys_clock_nanosleep),    // 248

// _____(__NR_swapcontext,       sys_swapcontext),        // 249

   LINXY(__NR_tgkill,            sys_tgkill),             // 250
// _____(__NR_utimes,            sys_utimes),             // 251
   GENXY(__NR_statfs64,          sys_statfs64),           // 252
   GENXY(__NR_fstatfs64,         sys_fstatfs64),          // 253
// /* #define __NR_fadvise64_64    254     32bit only */

// _____(__NR_rtas,              sys_rtas),               // 255
// /* Number 256 is reserved for sys_debug_setcontext */
// /* Number 257 is reserved for vserver */
// /* 258 currently unused */
   LINX_(__NR_mbind,             sys_mbind),              // 259

   LINXY(__NR_get_mempolicy,     sys_get_mempolicy),      // 260
   LINX_(__NR_set_mempolicy,     sys_set_mempolicy),      // 261
   LINXY(__NR_mq_open,           sys_mq_open),            // 262
   LINX_(__NR_mq_unlink,         sys_mq_unlink),          // 263
   LINX_(__NR_mq_timedsend,      sys_mq_timedsend),       // 264

   LINXY(__NR_mq_timedreceive,   sys_mq_timedreceive),    // 265
   LINX_(__NR_mq_notify,         sys_mq_notify),          // 266
   LINXY(__NR_mq_getsetattr,     sys_mq_getsetattr),      // 267
// _____(__NR_kexec_load,        sys_kexec_load),         // 268
   LINX_(__NR_add_key,           sys_add_key),            // 269

   LINX_(__NR_request_key,       sys_request_key),        // 270
   LINXY(__NR_keyctl,            sys_keyctl),             // 271
   LINXY(__NR_waitid,            sys_waitid),             // 272
   LINX_(__NR_ioprio_set,        sys_ioprio_set),         // 273
   LINX_(__NR_ioprio_get,        sys_ioprio_get),         // 274

   LINXY(__NR_inotify_init,  sys_inotify_init),           // 275
   LINX_(__NR_inotify_add_watch,  sys_inotify_add_watch), // 276
   LINX_(__NR_inotify_rm_watch,   sys_inotify_rm_watch),  // 277

   LINXY(__NR_pselect6,          sys_pselect6),           // 280
   LINXY(__NR_ppoll,             sys_ppoll),              // 281

   LINXY(__NR_openat,            sys_openat),             // 286
   LINX_(__NR_mkdirat,           sys_mkdirat),            // 287
   LINX_(__NR_mknodat,           sys_mknodat),            // 288
   LINX_(__NR_fchownat,          sys_fchownat),           // 289
   LINX_(__NR_futimesat,         sys_futimesat),          // 290
   LINXY(__NR_newfstatat,        sys_newfstatat),         // 291
   LINX_(__NR_unlinkat,          sys_unlinkat),           // 292
   LINX_(__NR_renameat,          sys_renameat),           // 293
   LINX_(__NR_linkat,            sys_linkat),             // 294
   LINX_(__NR_symlinkat,         sys_symlinkat),          // 295
   LINXY(__NR_readlinkat,        sys_readlinkat),         // 296
   LINX_(__NR_fchmodat,          sys_fchmodat),           // 297
   LINX_(__NR_faccessat,         sys_faccessat),          // 298
   LINX_(__NR_set_robust_list,   sys_set_robust_list),    // 299
   LINXY(__NR_get_robust_list,   sys_get_robust_list),    // 300
   LINXY(__NR_move_pages,        sys_move_pages),        // 301
   LINXY(__NR_getcpu,            sys_getcpu),            // 302
   LINXY(__NR_epoll_pwait,       sys_epoll_pwait),       // 303
   LINX_(__NR_utimensat,         sys_utimensat),         // 304
   LINXY(__NR_signalfd,          sys_signalfd),          // 305
   LINXY(__NR_timerfd_create,    sys_timerfd_create),    // 306
   LINXY(__NR_eventfd,           sys_eventfd),           // 307
   LINX_(__NR_sync_file_range2,  sys_sync_file_range2),  // 308
   LINX_(__NR_fallocate,         sys_fallocate),         // 309
//   LINXY(__NR_subpage_prot,       sys_ni_syscall),       // 310
   LINXY(__NR_timerfd_settime,   sys_timerfd_settime),  // 311
   LINXY(__NR_timerfd_gettime,   sys_timerfd_gettime),  // 312
   LINXY(__NR_signalfd4,         sys_signalfd4),        // 313
   LINXY(__NR_eventfd2,          sys_eventfd2),         // 314
   LINXY(__NR_epoll_create1,     sys_epoll_create1),    // 315
   LINXY(__NR_dup3,              sys_dup3),             // 316
   LINXY(__NR_pipe2,             sys_pipe2),            // 317
   LINXY(__NR_inotify_init1,     sys_inotify_init1),    // 318
   LINXY(__NR_perf_event_open,   sys_perf_event_open),  // 319
   LINXY(__NR_preadv,            sys_preadv),           // 320
   LINX_(__NR_pwritev,           sys_pwritev),          // 321
   LINXY(__NR_rt_tgsigqueueinfo, sys_rt_tgsigqueueinfo),// 322

   LINXY(__NR_prlimit64,         sys_prlimit64),        // 325
   LINXY(__NR_socket,            sys_socket),           // 326
   LINX_(__NR_bind,              sys_bind),             // 327
   LINX_(__NR_connect,           sys_connect),          // 328
   LINX_(__NR_listen,            sys_listen),           // 329
   LINXY(__NR_accept,            sys_accept),           // 330
   LINXY(__NR_getsockname,       sys_getsockname),      // 331
   LINXY(__NR_getpeername,       sys_getpeername),      // 332
   LINXY(__NR_socketpair,        sys_socketpair),       // 333
   LINX_(__NR_send,              sys_send),             // 334
   LINX_(__NR_sendto,            sys_sendto),           // 335
   LINXY(__NR_recv,              sys_recv),             // 336
   LINXY(__NR_recvfrom,          sys_recvfrom),         // 337
   LINX_(__NR_shutdown,          sys_shutdown),         // 338
   LINX_(__NR_setsockopt,        sys_setsockopt),       // 339
   LINXY(__NR_getsockopt,        sys_getsockopt),       // 340
   LINX_(__NR_sendmsg,           sys_sendmsg),          // 341
   LINXY(__NR_recvmsg,           sys_recvmsg),          // 342
   LINXY(__NR_recvmmsg,          sys_recvmmsg),         // 343
   LINXY(__NR_accept4,           sys_accept4),          // 344
   LINXY(__NR_name_to_handle_at, sys_name_to_handle_at),// 345
   LINXY(__NR_open_by_handle_at, sys_open_by_handle_at),// 346
   LINXY(__NR_clock_adjtime,     sys_clock_adjtime),    // 347
   LINX_(__NR_syncfs,            sys_syncfs),           // 348
   LINXY(__NR_sendmmsg,          sys_sendmmsg),         // 349

   LINXY(__NR_process_vm_readv,  sys_process_vm_readv), // 351
   LINX_(__NR_process_vm_writev, sys_process_vm_writev),// 352

   LINX_(__NR_sched_setattr,     sys_sched_setattr),    // 355
   LINXY(__NR_sched_getattr,     sys_sched_getattr),    // 356
   LINX_(__NR_renameat2,         sys_renameat2),        // 357

   LINXY(__NR_getrandom,         sys_getrandom),        // 359
   LINXY(__NR_memfd_create,      sys_memfd_create),     // 360

   LINX_(__NR_execveat,          sys_execveat),         // 362

   LINX_(__NR_membarrier,        sys_membarrier),       // 365

   GENX_(__NR_mlock2,            sys_mlock2),           // 378
   LINX_(__NR_copy_file_range,   sys_copy_file_range),  // 379
   LINX_(__NR_preadv2,           sys_preadv2),          // 380
   LINX_(__NR_pwritev2,          sys_pwritev2),         // 381

   LINXY(__NR_statx,             sys_statx),            // 383

   GENX_(__NR_rseq,              sys_ni_syscall),       // 387

   LINXY(__NR_io_uring_setup,    sys_io_uring_setup),    // 425
   LINXY(__NR_io_uring_enter,    sys_io_uring_enter),    // 426
   LINXY(__NR_io_uring_register, sys_io_uring_register), // 427
   LINXY(__NR_open_tree,         sys_open_tree),         // 428
   LINX_(__NR_move_mount,        sys_move_mount),        // 429
   LINXY(__NR_fsopen,            sys_fsopen),            // 430
   LINX_(__NR_fsconfig,          sys_fsconfig),          // 431
   LINXY(__NR_fsmount,           sys_fsmount),           // 432
   LINXY(__NR_fspick,            sys_fspick),            // 433
   LINXY(__NR_pidfd_open,        sys_pidfd_open),        // 434
   GENX_(__NR_clone3,            sys_ni_syscall),        // 435
   LINXY(__NR_close_range,       sys_close_range),       // 436
   LINXY(__NR_openat2,           sys_openat2),           // 437
   LINXY(__NR_pidfd_getfd,       sys_pidfd_getfd),       // 438
   LINX_(__NR_faccessat2,        sys_faccessat2),       // 439

   LINXY (__NR_epoll_pwait2,     sys_epoll_pwait2),      // 441

   LINXY(__NR_landlock_create_ruleset, sys_landlock_create_ruleset), // 444
   LINX_(__NR_landlock_add_rule,       sys_landlock_add_rule),       // 445
   LINX_(__NR_landlock_restrict_self,  sys_landlock_restrict_self),  // 446

   LINX_ (__NR_fchmodat2,        sys_fchmodat2),         // 452
};

SyscallTableEntry* ML_(get_linux_syscall_entry) ( UInt sysno )
{
   const UInt syscall_table_size
      = sizeof(syscall_table) / sizeof(syscall_table[0]);

   /* Is it in the contiguous initial section of the table? */
   if (sysno < syscall_table_size) {
      SyscallTableEntry* sys = &syscall_table[sysno];
      if (sys->before == NULL)
         return NULL; /* no entry */
      else
         return sys;
   }

   /* Can't find a wrapper */
   return NULL;
}

#endif // defined(VGP_ppc64be_linux) || defined(VGP_ppc64le_linux)

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
