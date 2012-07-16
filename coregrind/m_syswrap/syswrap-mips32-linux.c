
/*--------------------------------------------------------------------*/
/*--- Platform-specific syscalls stuff.    syswrap-mips32-linux.c ----*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2010-2012 RT-RK
      mips-valgrind@rt-rk.com

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

#if defined(VGP_mips32_linux)
#include "pub_core_basics.h"
#include "pub_core_vki.h"
#include "pub_core_vkiscnums.h"
#include "pub_core_libcsetjmp.h"    // to keep _threadstate.h happy
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
#include "pub_core_stacks.h"        // VG_(register_stack)
#include "pub_core_transtab.h"      // VG_(discard_translations)
#include "priv_types_n_macros.h"
#include "priv_syswrap-generic.h"   /* for decls of generic wrappers */
#include "priv_syswrap-linux.h"     /* for decls of linux-ish wrappers */
#include "priv_syswrap-main.h"

#include "pub_core_debuginfo.h"     // VG_(di_notify_*)
#include "pub_core_xarray.h"
#include "pub_core_clientstate.h"   // VG_(brk_base), VG_(brk_limit)
#include "pub_core_errormgr.h"
#include "pub_tool_gdbserver.h"     // VG_(gdbserver)
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
"   move	$29, $4\n\t"	// stack to %sp
"   move	$25, $6\n\t"	// f to t9/$25 
"   move 	$4, $7\n\t"	// arg1 to $a0
"   li 		$2, 0\n\t"	// zero all GP regs
"   li 		$3, 0\n\t" 
"   li 		$5, 0\n\t" 
"   li 		$6, 0\n\t" 
"   li 		$7, 0\n\t" 

"   li 		$12, 0\n\t" 
"   li 		$13, 0\n\t" 
"   li 		$14, 0\n\t" 
"   li 		$15, 0\n\t" 
"   li 		$16, 0\n\t" 
"   li 		$17, 0\n\t" 
"   li 		$18, 0\n\t" 
"   li 		$19, 0\n\t" 
"   li 		$20, 0\n\t" 
"   li 		$21, 0\n\t" 
"   li 		$22, 0\n\t" 
"   li 		$23, 0\n\t" 
"   li 		$24, 0\n\t" 
"   jr 		$25\n\t"	// jump to dst
"   break	0x7\n"	// should never get here
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

//extern
UInt do_syscall_clone_mips_linux (Word (*fn) (void *), //a0      0     32
                                   void *stack,         //a1      4     36
                                   Int flags,           //a2      8     40
                                   void *arg,           //a3      12    44
                                   Int * child_tid,     //stack   16    48
                                   Int * parent_tid,    //stack   20    52
                                   Int tls);          //stack   24    56
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

static void setup_child (ThreadArchState *, ThreadArchState *);
static SysRes sys_set_tls (ThreadId tid, Addr tlsptr);
/* 
   When a client clones, we need to keep track of the new thread.  This means:
   1. allocate a ThreadId+ThreadState+stack for the the thread
   2. initialize the thread's new VCPU state
   3. create the thread using the same args as the client requested,
   but using the scheduler entrypoint for IP, and a separate stack
   for SP.
 */ 

static SysRes do_clone (ThreadId ptid, 
                        UInt flags, Addr sp, 
                        Int * parent_tidptr,
                        Int * child_tidptr, 
                        Addr child_tls) 
{
   const Bool debug = False;
   ThreadId ctid = VG_ (alloc_ThreadState) ();
   ThreadState * ptst = VG_ (get_ThreadState) (ptid);
   ThreadState * ctst = VG_ (get_ThreadState) (ctid);
   UInt ret = 0;
   UWord * stack;
   NSegment const *seg;
   SysRes res;
   vki_sigset_t blockall, savedmask;

   VG_ (sigfillset) (&blockall);
   vg_assert (VG_ (is_running_thread) (ptid));
   vg_assert (VG_ (is_valid_tid) (ctid));
   stack = (UWord *) ML_ (allocstack) (ctid);
   if (stack == NULL) {
      res = VG_ (mk_SysRes_Error) (VKI_ENOMEM);
      goto out;
   }
   setup_child (&ctst->arch, &ptst->arch);

   /* on MIPS we need to set V0 and A3 to zero */ 
   ctst->arch.vex.guest_r2 = 0;
   ctst->arch.vex.guest_r7 = 0;
   if (sp != 0)
      ctst->arch.vex.guest_r29 = sp;

   ctst->os_state.parent = ptid;
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
   seg = VG_ (am_find_nsegment) ((Addr) sp);

   if (seg && seg->kind != SkResvn) {
      ctst->client_stack_highest_word = (Addr) VG_PGROUNDUP (sp);
      ctst->client_stack_szB = ctst->client_stack_highest_word - seg->start;
      VG_ (register_stack) (seg->start, ctst->client_stack_highest_word);
      if (debug)
         VG_ (printf) ("tid %d: guessed client stack range %#lx-%#lx\n",

      ctid, seg->start, VG_PGROUNDUP (sp));
   } else {
      VG_ (message) (Vg_UserMsg,
                     "!? New thread %d starts with sp+%#lx) unmapped\n",
                     ctid, sp);
      ctst->client_stack_szB = 0;
   }

   VG_TRACK (pre_thread_ll_create, ptid, ctid);
   if (flags & VKI_CLONE_SETTLS) {
      if (debug)
        VG_(printf)("clone child has SETTLS: tls at %#lx\n", child_tls);
      ctst->arch.vex.guest_r27 = child_tls;
      res = sys_set_tls(ctid, child_tls);
      if (sr_isError(res))
         goto out;
      ctst->arch.vex.guest_r27 = child_tls;
  }

   flags &= ~VKI_CLONE_SETTLS;
   VG_ (sigprocmask) (VKI_SIG_SETMASK, &blockall, &savedmask);
   /* Create the new thread */ 
   ret = do_syscall_clone_mips_linux (ML_ (start_thread_NORETURN),
                                    stack, flags, &VG_ (threads)[ctid], 
                                    child_tidptr, parent_tidptr,
                                    0 /*child_tls*/);

   /* High half word64 is syscall return value.  Low half is
      the entire CR, from which we need to extract CR0.SO. */ 
   if (debug)
      VG_(printf)("ret: 0x%x\n", ret);

   res = VG_ (mk_SysRes_mips32_linux) (/*val */ ret, 0, /*errflag */ 0);

   VG_ (sigprocmask) (VKI_SIG_SETMASK, &savedmask, NULL);

   out:
   if (sr_isError (res)) {
      VG_(cleanup_thread) (&ctst->arch);
      ctst->status = VgTs_Empty;
      VG_TRACK (pre_thread_ll_exit, ctid);
   }
   ptst->arch.vex.guest_r2 = 0;

   return res;
}

/* ---------------------------------------------------------------------
   More thread stuff
   ------------------------------------------------------------------ */ 

// MIPS doesn't have any architecture specific thread stuff that
// needs to be cleaned up da li ????!!!!???
void
VG_ (cleanup_thread) (ThreadArchState * arch) { } 

void
setup_child ( /*OUT*/ ThreadArchState * child,
              /*IN*/ ThreadArchState * parent) 
{
   /* We inherit our parent's guest state. */ 
   child->vex = parent->vex;
   child->vex_shadow1 = parent->vex_shadow1;
   child->vex_shadow2 = parent->vex_shadow2;
}

SysRes sys_set_tls ( ThreadId tid, Addr tlsptr )
{
   VG_(threads)[tid].arch.vex.guest_ULR = tlsptr;
   return VG_(mk_SysRes_Success)( 0 );
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
DECL_TEMPLATE (mips_linux, sys_socketcall);
DECL_TEMPLATE (mips_linux, sys_socket);
DECL_TEMPLATE (mips_linux, sys_setsockopt);
DECL_TEMPLATE (mips_linux, sys_getsockopt);
DECL_TEMPLATE (mips_linux, sys_connect);
DECL_TEMPLATE (mips_linux, sys_accept);
DECL_TEMPLATE (mips_linux, sys_sendto);
DECL_TEMPLATE (mips_linux, sys_recvfrom);
DECL_TEMPLATE (mips_linux, sys_ipc);
DECL_TEMPLATE (mips_linux, sys_semget);
DECL_TEMPLATE (mips_linux, sys_semop);
DECL_TEMPLATE (mips_linux, sys_semctl);
DECL_TEMPLATE (mips_linux, sys_semtimedop);
DECL_TEMPLATE (mips_linux, sys_shmget);
DECL_TEMPLATE (mips_linux, sys_shmdt);
DECL_TEMPLATE (mips_linux, sys_shmctl);
DECL_TEMPLATE (mips_linux, sys_sendmsg);
DECL_TEMPLATE (mips_linux, sys_recvmsg);
DECL_TEMPLATE (mips_linux, sys_msgget);
DECL_TEMPLATE (mips_linux, sys_msgrcv);
DECL_TEMPLATE (mips_linux, sys_msgsnd);
DECL_TEMPLATE (mips_linux, sys_msgctl);
DECL_TEMPLATE (mips_linux, sys_shutdown);
DECL_TEMPLATE (mips_linux, sys_bind);
DECL_TEMPLATE (mips_linux, sys_listen);
DECL_TEMPLATE (mips_linux, sys_getsockname);
DECL_TEMPLATE (mips_linux, sys_getpeername);
DECL_TEMPLATE (mips_linux, sys_socketpair);
DECL_TEMPLATE (mips_linux, sys_send);
DECL_TEMPLATE (mips_linux, sys_recv);
DECL_TEMPLATE (mips_linux, sys_mmap);
DECL_TEMPLATE (mips_linux, sys_mmap2);
DECL_TEMPLATE (mips_linux, sys_stat64);
DECL_TEMPLATE (mips_linux, sys_lstat64);
DECL_TEMPLATE (mips_linux, sys_fstatat64);
DECL_TEMPLATE (mips_linux, sys_fstat64);
DECL_TEMPLATE (mips_linux, sys_clone);
DECL_TEMPLATE (mips_linux, sys_sigreturn);
DECL_TEMPLATE (mips_linux, sys_rt_sigreturn);
DECL_TEMPLATE (mips_linux, sys_cacheflush);
DECL_TEMPLATE (mips_linux, sys_set_thread_area);
DECL_TEMPLATE (mips_linux, sys_pipe);

PRE (sys_socketcall) 
{
#  define ARG2_0  (((UWord*)ARG2)[0])
#  define ARG2_1  (((UWord*)ARG2)[1])
#  define ARG2_2  (((UWord*)ARG2)[2])
#  define ARG2_3  (((UWord*)ARG2)[3])
#  define ARG2_4  (((UWord*)ARG2)[4])
#  define ARG2_5  (((UWord*)ARG2)[5])
  *flags |= SfMayBlock;
  PRINT ("sys_socketcall ( %ld, %#lx )", ARG1, ARG2);
  PRE_REG_READ2 (long, "socketcall", int, call, unsigned long *, args);
  switch (ARG1 /* request */ )
    {
      case VKI_SYS_SOCKETPAIR:
        /* int socketpair(int d, int type, int protocol, int sv[2]); */ 
        PRE_MEM_READ ("socketcall.socketpair(args)", ARG2,
                                                    4 * sizeof (Addr));
        ML_ (generic_PRE_sys_socketpair) (tid, ARG2_0, ARG2_1, ARG2_2, ARG2_3);
        break;
      case VKI_SYS_SOCKET:
        /* int socket(int domain, int type, int protocol); */ 
        PRE_MEM_READ ("socketcall.socket(args)", ARG2, 3 * sizeof (Addr));
        break;
      case VKI_SYS_BIND:
        /* int bind(int sockfd, struct sockaddr *my_addr,
           int addrlen); */ 
        PRE_MEM_READ ("socketcall.bind(args)", ARG2, 3 * sizeof (Addr));
        ML_ (generic_PRE_sys_bind) (tid, ARG2_0, ARG2_1, ARG2_2);
        break;
      case VKI_SYS_LISTEN:
        /* int listen(int s, int backlog); */ 
        PRE_MEM_READ ("socketcall.listen(args)", ARG2, 2 * sizeof (Addr));
        break;
      case VKI_SYS_ACCEPT:
        {
          /* int accept(int s, struct sockaddr *addr, int *addrlen); */ 
          PRE_MEM_READ ("socketcall.accept(args)", ARG2, 3 * sizeof (Addr));
          ML_ (generic_PRE_sys_accept) (tid, ARG2_0, ARG2_1, ARG2_2);
        break;
        }
      case VKI_SYS_SENDTO:
        /* int sendto(int s, const void *msg, int len,
           unsigned int flags,
           const struct sockaddr *to, int tolen); */ 
        PRE_MEM_READ ("socketcall.sendto(args)", ARG2, 6 * sizeof (Addr));
        ML_ (generic_PRE_sys_sendto) (tid, ARG2_0, ARG2_1, ARG2_2, ARG2_3,
                                      ARG2_4, ARG2_5);
        break;
      case VKI_SYS_SEND:
        /* int send(int s, const void *msg, size_t len, int flags); */ 
        PRE_MEM_READ ("socketcall.send(args)", ARG2, 4 * sizeof (Addr));
        ML_ (generic_PRE_sys_send) (tid, ARG2_0, ARG2_1, ARG2_2);
        break;
      case VKI_SYS_RECVFROM:
        /* int recvfrom(int s, void *buf, int len, unsigned int flags,
           struct sockaddr *from, int *fromlen); */
        PRE_MEM_READ ("socketcall.recvfrom(args)", ARG2, 6 * sizeof (Addr));
        ML_ (generic_PRE_sys_recvfrom) (tid, ARG2_0, ARG2_1, ARG2_2, 
                                        ARG2_3, ARG2_4, ARG2_5);
        break;
      case VKI_SYS_RECV:
        /* int recv(int s, void *buf, int len, unsigned int flags); */ 
        /* man 2 recv says:
           The  recv call is normally used only on a connected socket
           (see connect(2)) and is identical to recvfrom with a  NULL
           from parameter.
        */ 
        PRE_MEM_READ ("socketcall.recv(args)", ARG2, 4 * sizeof (Addr));
        ML_ (generic_PRE_sys_recv) (tid, ARG2_0, ARG2_1, ARG2_2);
        break;
      case VKI_SYS_CONNECT:
        /* int connect(int sockfd,
           struct sockaddr *serv_addr, int addrlen ); */ 
        PRE_MEM_READ ("socketcall.connect(args)", ARG2, 3 * sizeof (Addr));
        ML_ (generic_PRE_sys_connect) (tid, ARG2_0, ARG2_1, ARG2_2);
        break;
      case VKI_SYS_SETSOCKOPT:
        /* int setsockopt(int s, int level, int optname,
           const void *optval, int optlen); */ 
        PRE_MEM_READ ("socketcall.setsockopt(args)", ARG2, 5 * sizeof (Addr));
        ML_ (generic_PRE_sys_setsockopt) (tid, ARG2_0, ARG2_1, ARG2_2,
                                          ARG2_3, ARG2_4);
        break;
      case VKI_SYS_GETSOCKOPT:
        /* int getsockopt(int s, int level, int optname,
           void *optval, socklen_t *optlen); */ 
        PRE_MEM_READ ("socketcall.getsockopt(args)", ARG2, 5 * sizeof (Addr));
        ML_ (linux_PRE_sys_getsockopt) (tid, ARG2_0, ARG2_1, ARG2_2, ARG2_3, ARG2_4);
        break;
      case VKI_SYS_GETSOCKNAME:
        /* int getsockname(int s, struct sockaddr* name, int* namelen) */ 
        PRE_MEM_READ ("socketcall.getsockname(args)", ARG2, 3 * sizeof (Addr));
        ML_ (generic_PRE_sys_getsockname) (tid, ARG2_0, ARG2_1, ARG2_2);
        break;
      case VKI_SYS_GETPEERNAME:
        /* int getpeername(int s, struct sockaddr* name, int* namelen) */ 
        PRE_MEM_READ ("socketcall.getpeername(args)", ARG2, 3 * sizeof (Addr));
        ML_ (generic_PRE_sys_getpeername) (tid, ARG2_0, ARG2_1, ARG2_2);
        break;
      case VKI_SYS_SHUTDOWN:
        /* int shutdown(int s, int how); */ 
        PRE_MEM_READ ("socketcall.shutdown(args)", ARG2, 2 * sizeof (Addr));
        break;
      case VKI_SYS_SENDMSG:
        {
          /* int sendmsg(int s, const struct msghdr *msg, int flags); */
          /* this causes warnings, and I don't get why. glibc bug?
           * (after all it's glibc providing the arguments array)
             PRE_MEM_READ( "socketcall.sendmsg(args)", ARG2, 3*sizeof(Addr) );
          */ 
          ML_ (generic_PRE_sys_sendmsg) (tid, (UChar *)ARG2_0, (struct vki_msghdr *)ARG2_1);
          break;
        }
      case VKI_SYS_RECVMSG:
        {
          /* int recvmsg(int s, struct msghdr *msg, int flags); */ 
          /* this causes warnings, and I don't get why. glibc bug?
           * (after all it's glibc providing the arguments array)
             PRE_MEM_READ("socketcall.recvmsg(args)", ARG2, 3*sizeof(Addr) );
           */ 
          ML_ (generic_PRE_sys_recvmsg) (tid, (UChar *)ARG2_0,
                                         (struct vki_msghdr *)ARG2_1);
          break;
        }
      default:
        VG_ (message) (Vg_DebugMsg, "Warning: unhandled socketcall 0x%lx", ARG1);
        SET_STATUS_Failure (VKI_EINVAL);
        break;
  }
#  undef ARG2_0
#  undef ARG2_1
#  undef ARG2_2
#  undef ARG2_3
#  undef ARG2_4
#  undef ARG2_5
}

POST (sys_socketcall) 
{
#  define ARG2_0  (((UWord*)ARG2)[0])
#  define ARG2_1  (((UWord*)ARG2)[1])
#  define ARG2_2  (((UWord*)ARG2)[2])
#  define ARG2_3  (((UWord*)ARG2)[3])
#  define ARG2_4  (((UWord*)ARG2)[4])
#  define ARG2_5  (((UWord*)ARG2)[5])
  SysRes r;
  vg_assert (SUCCESS);
  switch (ARG1 /* request */ )
    {
      case VKI_SYS_SOCKETPAIR:
        r = ML_ (generic_POST_sys_socketpair) ( tid,
                                                VG_ (mk_SysRes_Success) (RES),
                                                ARG2_0, ARG2_1, ARG2_2, ARG2_3);
        SET_STATUS_from_SysRes (r);
        break;
      case VKI_SYS_SOCKET:
        r = ML_ (generic_POST_sys_socket) (tid, VG_ (mk_SysRes_Success) (RES));
        SET_STATUS_from_SysRes (r);
        break;
      case VKI_SYS_BIND:
        /* int bind(int sockfd, struct sockaddr *my_addr, int addrlen); */ 
        break;
      case VKI_SYS_LISTEN:
        /* int listen(int s, int backlog); */ 
        break;
      case VKI_SYS_ACCEPT:
        /* int accept(int s, struct sockaddr *addr, int *addrlen); */ 
        r = ML_ (generic_POST_sys_accept) (tid, VG_ (mk_SysRes_Success) (RES),
                                           ARG2_0, ARG2_1, ARG2_2);
        SET_STATUS_from_SysRes (r);
        break;
      case VKI_SYS_SENDTO:
        break;
      case VKI_SYS_SEND:
        break;
      case VKI_SYS_RECVFROM:
        ML_ (generic_POST_sys_recvfrom) (tid, VG_ (mk_SysRes_Success) (RES),
                                         ARG2_0, ARG2_1, ARG2_2, ARG2_3,
                                         ARG2_4, ARG2_5);
        break;
      case VKI_SYS_RECV:
        ML_ (generic_POST_sys_recv) (tid, RES, ARG2_0, ARG2_1, ARG2_2);
        break;
      case VKI_SYS_CONNECT:
        break;
      case VKI_SYS_SETSOCKOPT:
        break;
      case VKI_SYS_GETSOCKOPT:
        ML_ (linux_POST_sys_getsockopt) (tid, VG_ (mk_SysRes_Success) (RES),
                                         ARG2_0, ARG2_1, ARG2_2,
                                         ARG2_3, ARG2_4);
        break;
      case VKI_SYS_GETSOCKNAME:
        ML_ (generic_POST_sys_getsockname) (tid, VG_ (mk_SysRes_Success) (RES),
                                            ARG2_0, ARG2_1, ARG2_2);
        break;
      case VKI_SYS_GETPEERNAME:
        ML_ (generic_POST_sys_getpeername) (tid, VG_ (mk_SysRes_Success) (RES),
                                            ARG2_0, ARG2_1, ARG2_2);
        break;
      case VKI_SYS_SHUTDOWN:
        break;
      case VKI_SYS_SENDMSG:
        break;
      case VKI_SYS_RECVMSG:
        ML_(generic_PRE_sys_recvmsg)( tid, "msg", (struct vki_msghdr *)ARG2_1 );
        break;
      default:
        VG_ (message) (Vg_DebugMsg, "FATAL: unhandled socketcall 0x%lx", ARG1);
        VG_ (core_panic) ("... bye!\n");
        break;
        /*NOTREACHED*/ 
    }
#  undef ARG2_0
#  undef ARG2_1
#  undef ARG2_2
#  undef ARG2_3
#  undef ARG2_4
#  undef ARG2_5
}

PRE (sys_socket) 
{
  PRINT ("sys_socket ( %ld, %ld, %ld )", ARG1, ARG2, ARG3);
  PRE_REG_READ3 (long, "socket", int, domain, int, type, int, protocol);
} 

POST (sys_socket) 
{
  SysRes r;
  vg_assert (SUCCESS);
  r = ML_ (generic_POST_sys_socket) (tid, VG_ (mk_SysRes_Success) (RES));
  SET_STATUS_from_SysRes (r);
} 

PRE (sys_setsockopt) 
{
  PRINT ("sys_setsockopt ( %ld, %ld, %ld, %#lx, %ld )", ARG1, ARG2, ARG3,
                                                        ARG4, ARG5);
  PRE_REG_READ5 (long, "setsockopt", int, s, int, level, int, optname,
                 const void *, optval, int, optlen);
  ML_ (generic_PRE_sys_setsockopt) (tid, ARG1, ARG2, ARG3, ARG4, ARG5);
} 

PRE (sys_getsockopt) 
{
  PRINT ("sys_getsockopt ( %ld, %ld, %ld, %#lx, %#lx )", ARG1, ARG2, ARG3,
                                                         ARG4, ARG5);
  PRE_REG_READ5 (long, "getsockopt", int, s, int, level, int, optname,
                 void *, optval, int, *optlen);
  ML_ (linux_PRE_sys_getsockopt) (tid, ARG1, ARG2, ARG3, ARG4, ARG5);
} 

POST (sys_getsockopt) 
{
  vg_assert (SUCCESS);
  ML_ (linux_POST_sys_getsockopt) (tid, VG_ (mk_SysRes_Success) (RES),
                                   ARG1, ARG2, ARG3, ARG4, ARG5);
} 

PRE(sys_connect)
{
   *flags |= SfMayBlock;
   PRINT("sys_connect ( %ld, %#lx, %ld )",ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "connect",
                 int, sockfd, struct sockaddr *, serv_addr, int, addrlen);
   ML_(generic_PRE_sys_connect)(tid, ARG1,ARG2,ARG3);
}

PRE (sys_accept) 
{
  *flags |= SfMayBlock;
  PRINT ("sys_accept ( %ld, %#lx, %ld )", ARG1, ARG2, ARG3);
  PRE_REG_READ3 (long, "accept",  int, s, struct sockaddr *, addr, int,
                 *addrlen);
  ML_ (generic_PRE_sys_accept) (tid, ARG1, ARG2, ARG3);
} 

POST (sys_accept) 
{
  SysRes r;
  vg_assert (SUCCESS);
  r =
    ML_ (generic_POST_sys_accept) (tid, VG_ (mk_SysRes_Success) (RES), 
                                   ARG1, ARG2, ARG3);
  SET_STATUS_from_SysRes (r);
} 

PRE (sys_sendto) 
{
  *flags |= SfMayBlock;
  PRINT ("sys_sendto ( %ld, %#lx, %ld, %lu, %#lx, %ld )", ARG1, ARG2, ARG3,
                                                          ARG4, ARG5, ARG6);
  PRE_REG_READ6 (long, "sendto", int, s, const void *, msg, int, len,
                 unsigned int, flags, const struct sockaddr *, to, int,
                 tolen);
  ML_ (generic_PRE_sys_sendto) (tid, ARG1, ARG2, ARG3, ARG4, ARG5, ARG6);
} 

PRE (sys_recvfrom) 
{
  *flags |= SfMayBlock;
  PRINT ("sys_recvfrom ( %ld, %#lx, %ld, %lu, %#lx, %#lx )", ARG1, ARG2,
                                                ARG3, ARG4, ARG5, ARG6);
  PRE_REG_READ6 (long, "recvfrom", int, s, void *, buf, int, len,
                 unsigned int, flags, 
  struct sockaddr *, from, int *, fromlen);
  ML_ (generic_PRE_sys_recvfrom) (tid, ARG1, ARG2, ARG3, ARG4, ARG5, ARG6);
} 

POST (sys_recvfrom) 
{
  vg_assert (SUCCESS);
  ML_ (generic_POST_sys_recvfrom) (tid, VG_ (mk_SysRes_Success) (RES),
                                   ARG1, ARG2, ARG3, ARG4, ARG5, ARG6);
} 

PRE(sys_sendmsg)
{
   *flags |= SfMayBlock;
   PRINT("sys_sendmsg ( %ld, %#lx, %ld )",ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "sendmsg",
                 int, s, const struct msghdr *, msg, int, flags);
   ML_(generic_PRE_sys_sendmsg)(tid, "msg", (struct vki_msghdr *)ARG2);
}

PRE(sys_recvmsg)
{
   *flags |= SfMayBlock;
   PRINT("sys_recvmsg ( %ld, %#lx, %ld )",ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "recvmsg", int, s, struct msghdr *, msg, int, flags);
   ML_(generic_PRE_sys_recvmsg)(tid, "msg", (struct vki_msghdr *)ARG2);
}
POST(sys_recvmsg)
{
   ML_(generic_POST_sys_recvmsg)(tid, "msg", (struct vki_msghdr *)ARG2, RES);
}

PRE (sys_semget) 
{
  PRINT ("sys_semget ( %ld, %ld, %ld )", ARG1, ARG2, ARG3);
  PRE_REG_READ3 (long, "semget", vki_key_t, key, int, nsems, int, semflg);
} 

PRE (sys_semop) 
{
  *flags |= SfMayBlock;
  PRINT ("sys_semop ( %ld, %#lx, %lu )", ARG1, ARG2, ARG3);
  PRE_REG_READ3 (long, "semop", int, semid, struct sembuf *, sops,
                 unsigned, nsoops);
  ML_ (generic_PRE_sys_semop) (tid, ARG1, ARG2, ARG3);
} 

PRE (sys_semctl) 
{
  switch (ARG3 & ~VKI_IPC_64)
    {
      case VKI_IPC_INFO:
      case VKI_SEM_INFO:
        PRINT ("sys_semctl ( %ld, %ld, %ld, %#lx )", ARG1, ARG2, ARG3, ARG4);
                                             PRE_REG_READ4 (long, "semctl", 
                                        int, semid, int, semnum, int, cmd,
                                                struct seminfo *, arg);
      break;
      case VKI_IPC_STAT:
      case VKI_SEM_STAT:
      case VKI_IPC_SET:
        PRINT ("sys_semctl ( %ld, %ld, %ld, %#lx )", ARG1, ARG2, ARG3, ARG4);
        PRE_REG_READ4 (long, "semctl", int, semid, int, semnum, int, cmd,
                       struct semid_ds *, arg);
      break;
      case VKI_GETALL:
      case VKI_SETALL:
        PRINT ("sys_semctl ( %ld, %ld, %ld, %#lx )", ARG1, ARG2, ARG3, ARG4);
        PRE_REG_READ4 (long, "semctl", int, semid, int, semnum, int, cmd,
                       unsigned short *, arg);
      break;
      default:
        PRINT ("sys_semctl ( %ld, %ld, %ld )", ARG1, ARG2, ARG3);
        PRE_REG_READ3 (long, "semctl", int, semid, int, semnum, int, cmd);
        break;
    }
  ML_ (generic_PRE_sys_semctl) (tid, ARG1, ARG2, ARG3, ARG4);
}

POST (sys_semctl) 
{
  ML_ (generic_POST_sys_semctl) (tid, RES, ARG1, ARG2, ARG3, ARG4);
}

PRE (sys_semtimedop) 
{
  *flags |= SfMayBlock;
  PRINT ("sys_semtimedop ( %ld, %#lx, %lu, %#lx )", ARG1, ARG2, ARG3, ARG4);
  PRE_REG_READ4 (long, "semtimedop", int, semid, struct sembuf *, sops,
                 unsigned, nsoops, 
  struct timespec *, timeout);
  ML_ (generic_PRE_sys_semtimedop) (tid, ARG1, ARG2, ARG3, ARG4);
} 

PRE (sys_msgget) 
{
  PRINT ("sys_msgget ( %ld, %ld )", ARG1, ARG2);
  PRE_REG_READ2 (long, "msgget", vki_key_t, key, int, msgflg);
} 

PRE (sys_msgsnd) 
{
  PRINT ("sys_msgsnd ( %ld, %#lx, %ld, %ld )", ARG1, ARG2, ARG3, ARG4);
  PRE_REG_READ4 (long, "msgsnd", int, msqid, struct msgbuf *, msgp,
                 vki_size_t, msgsz, int, msgflg);
  ML_ (linux_PRE_sys_msgsnd) (tid, ARG1, ARG2, ARG3, ARG4);
  if ((ARG4 & VKI_IPC_NOWAIT) == 0)
    *flags |= SfMayBlock;
}

PRE (sys_msgrcv) 
{
  PRINT ("sys_msgrcv ( %ld, %#lx, %ld, %ld, %ld )", ARG1, ARG2, ARG3, ARG4,
                                                    ARG5);
  PRE_REG_READ5 (long, "msgrcv", int, msqid, struct msgbuf *, msgp,
                 vki_size_t, msgsz, long, msgytp, int, msgflg);
  ML_ (linux_PRE_sys_msgrcv) (tid, ARG1, ARG2, ARG3, ARG4, ARG5);
  if ((ARG4 & VKI_IPC_NOWAIT) == 0)
    *flags |= SfMayBlock;
}

POST (sys_msgrcv) 
{
  ML_ (linux_POST_sys_msgrcv) (tid, RES, ARG1, ARG2, ARG3, ARG4, ARG5);
}

PRE (sys_msgctl) 
{
  PRINT ("sys_msgctl ( %ld, %ld, %#lx )", ARG1, ARG2, ARG3);
  PRE_REG_READ3 (long, "msgctl", int, msqid, int, cmd, struct msqid_ds *, buf);
  ML_ (linux_PRE_sys_msgctl) (tid, ARG1, ARG2, ARG3);
} 

POST (sys_msgctl) 
{
  ML_ (linux_POST_sys_msgctl) (tid, RES, ARG1, ARG2, ARG3);
} 

PRE (sys_shmget) 
{
  PRINT ("sys_shmget ( %ld, %ld, %ld )", ARG1, ARG2, ARG3);
  PRE_REG_READ3 (long, "shmget", vki_key_t, key, vki_size_t, size, int,
                 shmflg);
}

PRE (sys_shmdt) 
{
  PRINT ("sys_shmdt ( %#lx )", ARG1);
  PRE_REG_READ1 (long, "shmdt", const void *, shmaddr);
  if (!ML_ (generic_PRE_sys_shmdt) (tid, ARG1))
    SET_STATUS_Failure (VKI_EINVAL);
}

POST (sys_shmdt) 
 
{
  ML_ (generic_POST_sys_shmdt) (tid, RES, ARG1);
}

PRE (sys_shmctl) 
{
  PRINT ("sys_shmctl ( %ld, %ld, %#lx )", ARG1, ARG2, ARG3);
  PRE_REG_READ3 (long, "shmctl", int, shmid, int, cmd, struct shmid_ds *, buf);
  ML_ (generic_PRE_sys_shmctl) (tid, ARG1, ARG2, ARG3);
} 

POST (sys_shmctl) 
{
  ML_ (generic_POST_sys_shmctl) (tid, RES, ARG1, ARG2, ARG3);
} 

PRE (sys_shutdown) 
{
  *flags |= SfMayBlock;
  PRINT ("sys_shutdown ( %ld, %ld )", ARG1, ARG2);
  PRE_REG_READ2 (int, "shutdown", int, s, int, how);
} 

PRE (sys_bind) 
{
  PRINT ("sys_bind ( %ld, %#lx, %ld )", ARG1, ARG2, ARG3);
  PRE_REG_READ3 (long, "bind", int, sockfd, struct sockaddr *, my_addr,
                 int, addrlen);
  ML_ (generic_PRE_sys_bind) (tid, ARG1, ARG2, ARG3);
} 

PRE (sys_listen) 
{
  PRINT ("sys_listen ( %ld, %ld )", ARG1, ARG2);
  PRE_REG_READ2 (long, "listen", int, s, int, backlog);
} 

PRE (sys_getsockname) 
{
  PRINT ("sys_getsockname ( %ld, %#lx, %#lx )", ARG1, ARG2, ARG3);
  PRE_REG_READ3 (long, "getsockname", int, s, struct sockaddr *, name,
                 int *, namelen);
  ML_ (generic_PRE_sys_getsockname) (tid, ARG1, ARG2, ARG3);
} 

POST (sys_getsockname) 
{
  vg_assert (SUCCESS);
  ML_ (generic_POST_sys_getsockname) (tid, VG_ (mk_SysRes_Success) (RES),
                                      ARG1, ARG2, ARG3);
} 

PRE (sys_getpeername) 
{
  PRINT ("sys_getpeername ( %ld, %#lx, %#lx )", ARG1, ARG2, ARG3);
  PRE_REG_READ3 (long, "getpeername", int, s, struct sockaddr *, name,
                 int *, namelen);
  ML_ (generic_PRE_sys_getpeername) (tid, ARG1, ARG2, ARG3);
} 

POST (sys_getpeername) 
{
  vg_assert (SUCCESS);
  ML_ (generic_POST_sys_getpeername) (tid, VG_ (mk_SysRes_Success) (RES),
                                      ARG1, ARG2, ARG3);
} 

PRE (sys_socketpair) 
{
  PRINT ("sys_socketpair ( %ld, %ld, %ld, %#lx )", ARG1, ARG2, ARG3, ARG4);
  PRE_REG_READ4 (long, "socketpair", int, d, int, type, int,
                 protocol, int *, sv);
  ML_ (generic_PRE_sys_socketpair) (tid, ARG1, ARG2, ARG3, ARG4);
} 

POST (sys_socketpair) 
{
  vg_assert (SUCCESS);
  ML_ (generic_POST_sys_socketpair) (tid, VG_ (mk_SysRes_Success) (RES),
                                     ARG1, ARG2, ARG3, ARG4);
} 

PRE (sys_send) 
{
  *flags |= SfMayBlock;
  PRINT ("sys_send ( %ld, %#lx, %ld, %lu )", ARG1, ARG2, ARG3, ARG4);
  PRE_REG_READ4 (long, "send", int, s, const void *, msg, int, len,
                 unsigned int, flags);
  ML_ (generic_PRE_sys_send) (tid, ARG1, ARG2, ARG3);
} 

PRE (sys_recv) 
{
  *flags |= SfMayBlock;
  PRINT ("sys_recv ( %ld, %#lx, %ld, %lu )", ARG1, ARG2, ARG3, ARG4);
  PRE_REG_READ4 (long, "recv", int, s, void *, buf, int, len,
                 unsigned int, flags);
  ML_ (generic_PRE_sys_recv) (tid, ARG1, ARG2, ARG3);
} 

POST (sys_recv) 
{
  ML_ (generic_POST_sys_recv) (tid, RES, ARG1, ARG2, ARG3);
} 

PRE (sys_mmap2) 
{
  SysRes r;
  // Exactly like old_mmap() except:
  //  - all 6 args are passed in regs, rather than in a memory-block.
  //  - the file offset is specified in pagesize units rather than bytes,
  //    so that it can be used for files bigger than 2^32 bytes.
  // pagesize or 4K-size units in offset?
  vg_assert (VKI_PAGE_SIZE == 4096 || VKI_PAGE_SIZE == 4096 * 4
             || VKI_PAGE_SIZE == 4096 * 16);
  PRINT ("sys_mmap2 ( %#lx, %llu, %ld, %ld, %ld, %ld )", ARG1, (ULong) ARG2,
                                                         ARG3, ARG4, 
                                                         ARG5, ARG6);
  PRE_REG_READ6 (long, "mmap2", unsigned long, start, unsigned long, length,
                 unsigned long, prot, unsigned long, flags,
                 unsigned long, fd, unsigned long, offset);
  r =
    ML_ (generic_PRE_sys_mmap) (tid, ARG1, ARG2, ARG3, ARG4, ARG5,
                                VKI_PAGE_SIZE * (Off64T) ARG6);
  SET_STATUS_from_SysRes (r);
} 

PRE (sys_mmap) 
{
  SysRes r;
  //vg_assert(VKI_PAGE_SIZE == 4096);
  PRINT ("sys_mmap ( %#lx, %llu, %lu, %lu, %lu, %ld )", ARG1, (ULong) ARG2,
                                                        ARG3, ARG4, ARG5, ARG6);
  PRE_REG_READ6 (long, "mmap", unsigned long, start, vki_size_t, length,
                 int, prot, int, flags, int, fd, unsigned long, offset);
  r =
    ML_ (generic_PRE_sys_mmap) (tid, ARG1, ARG2, ARG3, ARG4, ARG5,
                                (Off64T) ARG6);
  SET_STATUS_from_SysRes (r);
} 

// XXX: lstat64/fstat64/stat64 are generic, but not necessarily
// applicable to every architecture -- I think only to 32-bit archs.
// We're going to need something like linux/core_os32.h for such
// things, eventually, I think.  --njn
 
PRE (sys_lstat64) 
{
  PRINT ("sys_lstat64 ( %#lx(%s), %#lx )", ARG1, (char *) ARG1, ARG2);
  PRE_REG_READ2 (long, "lstat64", char *, file_name, struct stat64 *, buf);
  PRE_MEM_RASCIIZ ("lstat64(file_name)", ARG1);
  PRE_MEM_WRITE ("lstat64(buf)", ARG2, sizeof (struct vki_stat64));
} 

POST (sys_lstat64) 
{
  vg_assert (SUCCESS);
  if (RES == 0)
    {
      POST_MEM_WRITE (ARG2, sizeof (struct vki_stat64));
    }
} 

PRE (sys_stat64) 
{
  PRINT ("sys_stat64 ( %#lx(%s), %#lx )", ARG1, (char *) ARG1, ARG2);
  PRE_REG_READ2 (long, "stat64", char *, file_name, struct stat64 *, buf);
  PRE_MEM_RASCIIZ ("stat64(file_name)", ARG1);
  PRE_MEM_WRITE ("stat64(buf)", ARG2, sizeof (struct vki_stat64));
}

POST (sys_stat64)
{
  POST_MEM_WRITE (ARG2, sizeof (struct vki_stat64));
}

PRE (sys_fstatat64)
{
  PRINT ("sys_fstatat64 ( %ld, %#lx(%s), %#lx )", ARG1, ARG2, (char *) ARG2,
                                                  ARG3);
  PRE_REG_READ3 (long, "fstatat64", int, dfd, char *, file_name,
                 struct stat64 *, buf);
  PRE_MEM_RASCIIZ ("fstatat64(file_name)", ARG2);
  PRE_MEM_WRITE ("fstatat64(buf)", ARG3, sizeof (struct vki_stat64));
}

POST (sys_fstatat64)
{
  POST_MEM_WRITE (ARG3, sizeof (struct vki_stat64));
}

PRE (sys_fstat64)
{
  PRINT ("sys_fstat64 ( %ld, %#lx )", ARG1, ARG2);
  PRE_REG_READ2 (long, "fstat64", unsigned long, fd, struct stat64 *, buf);
  PRE_MEM_WRITE ("fstat64(buf)", ARG2, sizeof (struct vki_stat64));
}

POST (sys_fstat64)
{
  POST_MEM_WRITE (ARG2, sizeof (struct vki_stat64));
} 

static Addr
deref_Addr (ThreadId tid, Addr a, Char * s) 
{
  Addr * a_p = (Addr *) a;
  PRE_MEM_READ (s, (Addr) a_p, sizeof (Addr));
  return *a_p;
}

PRE (sys_ipc) 
{
  PRINT ("sys_ipc ( %ld, %ld, %ld, %ld, %#lx, %ld )", ARG1, ARG2, ARG3, 
                                                      ARG4, ARG5, ARG6);
  // XXX: this is simplistic -- some args are not used in all circumstances.
  PRE_REG_READ6 (int, "ipc", vki_uint, call, int, first, int, second, int,
                 third, void *, ptr, long, fifth) 
  switch (ARG1 /* call */ )
    {
      case VKI_SEMOP:
        ML_ (generic_PRE_sys_semop) (tid, ARG2, ARG5, ARG3);
        *flags |= SfMayBlock;
      break;
      case VKI_SEMGET:
      break;
      case VKI_SEMCTL:
        {
          UWord arg = deref_Addr (tid, ARG5, "semctl(arg)");
          ML_ (generic_PRE_sys_semctl) (tid, ARG2, ARG3, ARG4, arg);
          break;
        }
      case VKI_SEMTIMEDOP:
        ML_ (generic_PRE_sys_semtimedop) (tid, ARG2, ARG5, ARG3, ARG6);
        *flags |= SfMayBlock;
        break;
      case VKI_MSGSND:
        ML_ (linux_PRE_sys_msgsnd) (tid, ARG2, ARG5, ARG3, ARG4);
        if ((ARG4 & VKI_IPC_NOWAIT) == 0)
          *flags |= SfMayBlock;
        break;
      case VKI_MSGRCV:
        {
          Addr msgp;
          Word msgtyp;
          msgp = deref_Addr (tid,
                            (Addr)(&((struct vki_ipc_kludge *)ARG5)->msgp),
                            "msgrcv(msgp)");
          msgtyp = deref_Addr (tid,
                              (Addr) (&((struct vki_ipc_kludge *)ARG5)->msgtyp),
                              "msgrcv(msgp)");
          ML_ (linux_PRE_sys_msgrcv) (tid, ARG2, msgp, ARG3, msgtyp, ARG4);
          if ((ARG4 & VKI_IPC_NOWAIT) == 0)
            *flags |= SfMayBlock;
            break;
        }
      case VKI_MSGGET:
        break;
      case VKI_MSGCTL:
        ML_ (linux_PRE_sys_msgctl) (tid, ARG2, ARG3, ARG5);
        break;
      case VKI_SHMAT:
        {
          PRE_MEM_WRITE ("shmat(raddr)", ARG4, sizeof (Addr));
          break;
        }
      case VKI_SHMDT:
        if (!ML_ (generic_PRE_sys_shmdt) (tid, ARG5))
          SET_STATUS_Failure (VKI_EINVAL);
        break;
      case VKI_SHMGET:
        break;
      case VKI_SHMCTL:		/* IPCOP_shmctl */
        ML_ (generic_PRE_sys_shmctl) (tid, ARG2, ARG3, ARG5);
        break;
      default:
        VG_ (message) (Vg_DebugMsg, "FATAL: unhandled syscall(ipc) %ld\n",
                       ARG1);
        VG_ (core_panic) ("... bye!\n");
        break;
     /*NOTREACHED*/ 
  }
}

POST (sys_ipc) 
{
  vg_assert (SUCCESS);
  switch (ARG1 /* call */ )
    {
      case VKI_SEMOP:
      case VKI_SEMGET:
      break;
      case VKI_SEMCTL:
      {
        UWord arg = deref_Addr (tid, ARG5, "semctl(arg)");
        ML_ (generic_PRE_sys_semctl) (tid, ARG2, ARG3, ARG4, arg);
        break;
      }
      case VKI_SEMTIMEDOP:
      case VKI_MSGSND:
      break;
      case VKI_MSGRCV:
        {
          Addr msgp;
          Word msgtyp;
          msgp = deref_Addr (tid, (Addr) (&((struct vki_ipc_kludge *)
                             ARG5)->msgp), 
          "msgrcv(msgp)");
          msgtyp = deref_Addr (tid,
                           (Addr) (&((struct vki_ipc_kludge *) ARG5)->msgtyp),
                           "msgrcv(msgp)");
          ML_ (linux_POST_sys_msgrcv)(tid, RES, ARG2, msgp, ARG3, msgtyp, ARG4);
          break;
        }
      case VKI_MSGGET:
      break;
      case VKI_MSGCTL:
        ML_ (linux_POST_sys_msgctl) (tid, RES, ARG2, ARG3, ARG5);
      break;
      case VKI_SHMAT:
        {
          Addr addr;
          /* force readability. before the syscall it is
           * indeed uninitialized, as can be seen in
           * glibc/sysdeps/unix/sysv/linux/shmat.c */ 
          POST_MEM_WRITE (ARG4, sizeof (Addr));
          addr = deref_Addr (tid, ARG4, "shmat(addr)");
          ML_ (generic_POST_sys_shmat) (tid, addr, ARG2, ARG5, ARG3);
          break;
        }
      case VKI_SHMDT:
        ML_ (generic_POST_sys_shmdt) (tid, RES, ARG5);
      break;
      case VKI_SHMGET:
      break;
      case VKI_SHMCTL:
        ML_ (generic_POST_sys_shmctl) (tid, RES, ARG2, ARG3, ARG5);
      break;
      default:
        VG_ (message) (Vg_DebugMsg, "FATAL: unhandled syscall(ipc) %ld\n",
                       ARG1);
        VG_ (core_panic) ("... bye!\n");
      break;
     /*NOTREACHED*/
  }
}

PRE (sys_clone) 
  {
    Bool badarg = False;
    UInt cloneflags;
    PRINT ("sys_clone ( %lx, %#lx, %#lx, %#lx, %#lx )", ARG1, ARG2, ARG3,
                                                        ARG4, ARG5);
    PRE_REG_READ2 (int, "clone", unsigned long, flags,  void *, child_stack);
    if (ARG1 & VKI_CLONE_PARENT_SETTID)
      {
        if (VG_ (tdict).track_pre_reg_read)
          {
            PRA3 ("clone", int *, parent_tidptr);
          }
        PRE_MEM_WRITE ("clone(parent_tidptr)", ARG3, sizeof (Int));
        if (!VG_ (am_is_valid_for_client)(ARG3, sizeof (Int), VKI_PROT_WRITE))
        {
          badarg = True;
        }
      }
    if (ARG1 & (VKI_CLONE_CHILD_SETTID | VKI_CLONE_CHILD_CLEARTID))
      {
        if (VG_ (tdict).track_pre_reg_read)
          {
            PRA5 ("clone", int *, child_tidptr);
          }
        PRE_MEM_WRITE ("clone(child_tidptr)", ARG5, sizeof (Int));
        if (!VG_ (am_is_valid_for_client)(ARG5, sizeof (Int), VKI_PROT_WRITE))
          {
            badarg = True;
          }
      }
    if (badarg)
      {
        SET_STATUS_Failure (VKI_EFAULT);
        return;
      }
    cloneflags = ARG1;
    if (!ML_ (client_signal_OK) (ARG1 & VKI_CSIGNAL))
      {
        SET_STATUS_Failure (VKI_EINVAL);
        return;
      }
    /* Only look at the flags we really care about */ 
    switch (cloneflags & (VKI_CLONE_VM | VKI_CLONE_FS
           |VKI_CLONE_FILES | VKI_CLONE_VFORK))
      {
        case VKI_CLONE_VM | VKI_CLONE_FS | VKI_CLONE_FILES:
        /* thread creation */ 
        PRINT ("sys_clone1 ( %#lx, %#lx, %#lx, %#lx, %#lx )",
               ARG1, ARG2, ARG3, ARG4, ARG5);
        SET_STATUS_from_SysRes (do_clone (tid, 
                                          ARG1, /* flags */ 
                                          (Addr) ARG2, /* child SP */ 
                                          (Int *) ARG3, /* parent_tidptr */ 
                                          (Int *) ARG5, /* child_tidptr */ 
                                          (Addr) ARG4));	/* child_tls */

        break;
        case VKI_CLONE_VFORK | VKI_CLONE_VM:	/* vfork */
          /* FALLTHROUGH - assume vfork == fork */ 
          cloneflags &= ~(VKI_CLONE_VFORK | VKI_CLONE_VM);
        case 0:  /* plain fork */
          SET_STATUS_from_SysRes (ML_ (do_fork_clone) (tid,
                                  cloneflags, /* flags */ 
                                  (Int *) ARG3, /* parent_tidptr */ 
                                  (Int *) ARG5));	/* child_tidptr */
        break;
        default:
          /* should we just ENOSYS? */ 
          VG_ (message) (Vg_UserMsg, "Unsupported clone() flags: 0x%lx\n", ARG1);
          VG_ (message) (Vg_UserMsg, "\n");
          VG_ (message) (Vg_UserMsg, "The only supported clone() uses are:\n");
          VG_ (message) (Vg_UserMsg, 
                          " - via a threads library (LinuxThreads or NPTL)\n");
          VG_ (message) (Vg_UserMsg,
                          " - via the implementation of fork or vfork\n");
          VG_ (unimplemented)("Valgrind does not support general clone().");
    }
    if (SUCCESS)
      {
        if (ARG1 & VKI_CLONE_PARENT_SETTID)
          POST_MEM_WRITE (ARG3, sizeof (Int));
        if (ARG1 & (VKI_CLONE_CHILD_SETTID | VKI_CLONE_CHILD_CLEARTID))
          POST_MEM_WRITE (ARG5, sizeof (Int));
        /* Thread creation was successful; let the child have the chance 
         * to run */
        *flags |= SfYieldAfter;
      }
}

PRE (sys_sigreturn) 
{
  ThreadState * tst;
  PRINT ("sys_sigreturn ( )");
  vg_assert (VG_ (is_valid_tid) (tid));
  vg_assert (tid >= 1 && tid < VG_N_THREADS);
  vg_assert (VG_ (is_running_thread) (tid));
  tst = VG_ (get_ThreadState) (tid);
  VG_ (sigframe_destroy) (tid, False);
  /* Tell the driver not to update the guest state with the "result",
     and set a bogus result to keep it happy. */ 
  *flags |= SfNoWriteResult;
  SET_STATUS_Success (0);
   /* Check to see if any signals arose as a result of this. */ 
  *flags |= SfPollAfter;
}

PRE (sys_rt_sigreturn) 
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

PRE (sys_set_thread_area) 
{
   PRINT ("set_thread_area (%lx)", ARG1);
   PRE_REG_READ1(long, "set_thread_area", unsigned long, addr);
   SET_STATUS_from_SysRes( sys_set_tls( tid, ARG1 ) );
}

/* Very much MIPS specific */
PRE (sys_cacheflush)
{
  PRINT ("cacheflush (%lx, %#lx, %#lx)", ARG1, ARG2, ARG3);
  PRE_REG_READ3 (long, "cacheflush", void *, addrlow, void *, addrhigh, int,
                 flags);
  VG_ (discard_translations) ((Addr64) ARG1, ((ULong) ARG2) - ((ULong) ARG1) +
                              1ULL /*paranoia */ , "PRE(sys_cacheflush)");
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
  //PLAXY (__NR_syscall, sys_syscall),	// 0
  GENX_ (__NR_exit, sys_exit),	// 1
  GENX_ (__NR_fork, sys_fork),	// 2
  GENXY (__NR_read, sys_read),	// 3
  GENX_ (__NR_write, sys_write),	// 4
  GENXY (__NR_open, sys_open),	// 5
  GENXY (__NR_close, sys_close),	// 6
  GENXY (__NR_waitpid, sys_waitpid),	// 7
  GENXY (__NR_creat, sys_creat),	// 8
  GENX_ (__NR_link, sys_link),	// 9
  GENX_ (__NR_unlink, sys_unlink),	// 10
  GENX_ (__NR_execve, sys_execve),	// 11
  GENX_ (__NR_chdir, sys_chdir),	// 12
  GENXY (__NR_time, sys_time),	// 13
  GENX_ (__NR_mknod, sys_mknod),	// 14
  GENX_ (__NR_chmod, sys_chmod),	// 15
  GENX_ (__NR_lchown, sys_lchown),	// 16
  LINX_ (__NR_lseek, sys_lseek),	// 19
  GENX_ (__NR_getpid, sys_getpid),	// 20
  LINX_ (__NR_mount, sys_mount),	// 21
  LINX_ (__NR_umount, sys_oldumount),	// 22
  GENX_ (__NR_setuid, sys_setuid),	// 23 ## P
  GENX_ (__NR_getuid, sys_getuid),	// 24 ## P
  //..    //   (__NR_stime,             sys_stime),
  //..    PLAXY(__NR_ptrace,            sys_ptrace),            // 26
  GENX_ (__NR_alarm, sys_alarm),	// 27
  //..    //   (__NR_oldfstat,          sys_fstat), 
  GENX_ (__NR_pause, sys_pause),	// 29
  LINX_ (__NR_utime, sys_utime),	// 30
  //..    GENX_(__NR_stty,              sys_ni_syscall),        // 31
  //..    GENX_(__NR_gtty,              sys_ni_syscall),        // 32
  GENX_ (__NR_access, sys_access),	// 33
  //..    GENX_(__NR_nice,              sys_nice),              // 34
  //..    GENX_(__NR_ftime,             sys_ni_syscall),        // 35
  //..    GENX_(__NR_sync,              sys_sync),              // 36
  GENX_ (__NR_kill, sys_kill),	// 37
  GENX_ (__NR_rename, sys_rename),	// 38
  GENX_ (__NR_mkdir, sys_mkdir),	// 39
  GENX_ (__NR_rmdir, sys_rmdir),	// 40
  GENXY (__NR_dup, sys_dup),	// 41
  PLAXY (__NR_pipe, sys_pipe),	// 42
  GENXY (__NR_times, sys_times),	// 43
  //..    GENX_(__NR_prof,              sys_ni_syscall),   // 44
  //..
  GENX_ (__NR_brk, sys_brk),	// 45
  GENX_ (__NR_setgid, sys_setgid),	// 46
  GENX_ (__NR_getgid, sys_getgid),	// 47
  //..    //   (__NR_signal,            sys_signal),       // 48 */* (ANSI C)
  GENX_ (__NR_geteuid, sys_geteuid),	// 49
  GENX_ (__NR_getegid, sys_getegid),	// 50
  //..    GENX_(__NR_acct,              sys_acct),         // 51
  LINX_ (__NR_umount2, sys_umount),	// 52
  //..    GENX_(__NR_lock,              sys_ni_syscall),   // 53
  LINXY (__NR_ioctl, sys_ioctl),	// 54
  LINXY (__NR_fcntl, sys_fcntl),	// 55
  //..    GENX_(__NR_mpx,               sys_ni_syscall),   // 56
  GENX_ (__NR_setpgid, sys_setpgid),	// 57
  //..    GENX_(__NR_ulimit,            sys_ni_syscall),        // 58
  //..    //   (__NR_oldolduname,       sys_olduname),          // 59
  GENX_ (__NR_umask, sys_umask),	// 60
  GENX_ (__NR_chroot, sys_chroot),	// 61
  //..    //   (__NR_ustat,             sys_ustat)              // 62 SVr4 -- deprecated
  GENXY (__NR_dup2, sys_dup2),	// 63
  GENX_ (__NR_getppid, sys_getppid),	// 64
  GENX_ (__NR_getpgrp, sys_getpgrp),	// 65
  GENX_ (__NR_setsid, sys_setsid),	// 66
  //   PLAXY(__NR_sigaction,         sys_sigaction),         // 67
  //..    //   (__NR_sgetmask,          sys_sgetmask),          // 68 */* (ANSI C)
  //..    //   (__NR_ssetmask,          sys_ssetmask),          // 69 */* (ANSI C)
  //.. 
  GENX_ (__NR_setreuid, sys_setreuid),	// 70
  GENX_ (__NR_setregid, sys_setregid),	// 71
  //   PLAX_(__NR_sigsuspend,        sys_sigsuspend),        // 72
  LINXY (__NR_sigpending, sys_sigpending),	// 73
  //..    //   (__NR_sethostname,       sys_sethostname),       // 74 */*
  //..
  GENX_ (__NR_setrlimit, sys_setrlimit),	// 75
  //..    GENXY(__NR_getrlimit,         sys_old_getrlimit),     // 76
  GENXY (__NR_getrusage, sys_getrusage),	// 77
  GENXY (__NR_gettimeofday, sys_gettimeofday),	// 78
  //..    GENX_(__NR_settimeofday,      sys_settimeofday),      // 79
  //..
  GENXY (__NR_getgroups, sys_getgroups),	// 80
  GENX_ (__NR_setgroups, sys_setgroups),	// 81
  //..    PLAX_(__NR_select,            old_select),            // 82
  GENX_ (__NR_symlink, sys_symlink),	// 83
  //..    //   (__NR_oldlstat,          sys_lstat),             // 84 -- obsolete
  //..
  GENX_ (__NR_readlink, sys_readlink),	// 85
  //..    //   (__NR_uselib,            sys_uselib),            // 86 */Linux
  //..    //   (__NR_swapon,            sys_swapon),            // 87 */Linux
  //..    //   (__NR_reboot,            sys_reboot),            // 88 */Linux
  //..    //   (__NR_readdir,           old_readdir),           // 89 -- superseded
  PLAX_ (__NR_mmap, sys_mmap),	// 90
  GENXY (__NR_munmap, sys_munmap),	// 91
  GENX_ (__NR_truncate, sys_truncate),	// 92
  GENX_ (__NR_ftruncate, sys_ftruncate),	// 93
  GENX_ (__NR_fchmod, sys_fchmod),	// 94
  GENX_ (__NR_fchown, sys_fchown),	// 95
  GENX_ (__NR_getpriority, sys_getpriority),	// 96
  GENX_ (__NR_setpriority, sys_setpriority),	// 97
  //..    GENX_(__NR_profil,            sys_ni_syscall),        // 98
  GENXY (__NR_statfs, sys_statfs),	// 99
  //..
  GENXY (__NR_fstatfs, sys_fstatfs),	// 100
  //..    LINX_(__NR_ioperm,            sys_ioperm),            // 101
  PLAXY (__NR_socketcall, sys_socketcall),	// 102
  LINXY (__NR_syslog, sys_syslog),	// 103
  GENXY (__NR_setitimer, sys_setitimer),	// 104
  //..
  //..    GENXY(__NR_getitimer,         sys_getitimer),         // 105
  GENXY (__NR_stat, sys_newstat),	// 106
  GENXY (__NR_lstat, sys_newlstat),	// 107
  GENXY (__NR_fstat, sys_newfstat),	// 108
  //..    //   (__NR_olduname,          sys_uname),             // 109 -- obsolete
  //..
  //..    GENX_(__NR_iopl,              sys_iopl),              // 110
  //..    LINX_(__NR_vhangup,           sys_vhangup),           // 111
  //..    GENX_(__NR_idle,              sys_ni_syscall),        // 112
  //..    //   (__NR_vm86old,           sys_vm86old),           // 113 x86/Linux-only
  GENXY (__NR_wait4, sys_wait4),	// 114
  //..
  //..    //   (__NR_swapoff,           sys_swapoff),           // 115 */Linux 
  LINXY (__NR_sysinfo, sys_sysinfo),	// 116
  PLAXY (__NR_ipc, sys_ipc),	// 117
  GENX_ (__NR_fsync, sys_fsync),	// 118
  PLAX_ (__NR_sigreturn, sys_sigreturn),	// 119 ?/Linux
  //..
  PLAX_ (__NR_clone, sys_clone),	// 120
  //..    //   (__NR_setdomainname,     sys_setdomainname),     // 121 */*(?)
  GENXY (__NR_uname, sys_newuname),	// 122
  //..    PLAX_(__NR_modify_ldt,        sys_modify_ldt),        // 123
  //..    LINXY(__NR_adjtimex,          sys_adjtimex),          // 124
  //..
  GENXY (__NR_mprotect, sys_mprotect),	// 125
  LINXY (__NR_sigprocmask, sys_sigprocmask),    // 126
  //..    // Nb: create_module() was removed 2.4-->2.6
  //..    GENX_(__NR_create_module,     sys_ni_syscall),        // 127
  //..    GENX_(__NR_init_module,       sys_init_module),       // 128
  //..    //   (__NR_delete_module,     sys_delete_module),     // 129 (*/Linux)?
  //..
  //..    // Nb: get_kernel_syms() was removed 2.4-->2.6
  //..    GENX_(__NR_get_kernel_syms,   sys_ni_syscall),        // 130
  //..    LINX_(__NR_quotactl,          sys_quotactl),          // 131
  GENX_ (__NR_getpgid, sys_getpgid),	// 132
  GENX_ (__NR_fchdir, sys_fchdir),	// 133
  //..    //   (__NR_bdflush,           sys_bdflush),           // 134 */Linux
  //..
  //..    //   (__NR_sysfs,             sys_sysfs),             // 135 SVr4
  LINX_ (__NR_personality, sys_personality),	// 136
  //..    GENX_(__NR_afs_syscall,       sys_ni_syscall),        // 137
  LINX_ (__NR_setfsuid, sys_setfsuid),	// 138
  LINX_ (__NR_setfsgid, sys_setfsgid),	// 139
  LINXY (__NR__llseek, sys_llseek),	// 140
  GENXY (__NR_getdents, sys_getdents),	// 141
  GENX_ (__NR__newselect, sys_select),	// 142
  GENX_ (__NR_flock, sys_flock),	// 143
  GENX_ (__NR_msync, sys_msync),	// 144
  //..
  GENXY (__NR_readv, sys_readv),	// 145
  GENX_ (__NR_writev, sys_writev),	// 146
  PLAX_ (__NR_cacheflush, sys_cacheflush),	// 147
  GENX_ (__NR_getsid, sys_getsid),	// 151
  GENX_ (__NR_fdatasync, sys_fdatasync),	// 152
  LINXY (__NR__sysctl, sys_sysctl),	// 153
  //..
  GENX_ (__NR_mlock, sys_mlock),	// 154
  GENX_ (__NR_munlock, sys_munlock),	// 155
  GENX_ (__NR_mlockall, sys_mlockall),	// 156
  LINX_ (__NR_munlockall, sys_munlockall),	// 157
  //..    LINXY(__NR_sched_setparam,    sys_sched_setparam),    // 158
  //..
  LINXY (__NR_sched_getparam, sys_sched_getparam),	// 159
  LINX_ (__NR_sched_setscheduler, sys_sched_setscheduler),	// 160
  LINX_ (__NR_sched_getscheduler, sys_sched_getscheduler),	// 161
  LINX_ (__NR_sched_yield, sys_sched_yield),	// 162
  LINX_ (__NR_sched_get_priority_max, sys_sched_get_priority_max),	// 163
  LINX_ (__NR_sched_get_priority_min, sys_sched_get_priority_min),	// 164
  //..    //LINX?(__NR_sched_rr_get_interval,  sys_sched_rr_get_interval), // 165 */*
  GENXY (__NR_nanosleep, sys_nanosleep),	// 166
  GENX_ (__NR_mremap, sys_mremap),	// 167
  PLAXY (__NR_accept, sys_accept),	// 168
  PLAX_ (__NR_bind, sys_bind),	// 169
  PLAX_ (__NR_connect, sys_connect),	// 170
  PLAXY (__NR_getpeername, sys_getpeername),	// 171
  PLAXY (__NR_getsockname, sys_getsockname),	// 172
  PLAXY (__NR_getsockopt, sys_getsockopt),	// 173
  PLAX_ (__NR_listen, sys_listen),	// 174
  PLAXY (__NR_recv, sys_recv),	// 175
  PLAXY (__NR_recvfrom, sys_recvfrom),	// 176
  PLAXY (__NR_recvmsg, sys_recvmsg),	// 177
  PLAX_ (__NR_send, sys_send),	// 178
  PLAX_ (__NR_sendmsg, sys_sendmsg),	// 179
  PLAX_ (__NR_sendto, sys_sendto),	// 180
  PLAX_ (__NR_setsockopt, sys_setsockopt),	// 181
  PLAXY (__NR_socket, sys_socket),	// 183
  PLAXY (__NR_socketpair, sys_socketpair),	// 184
  LINX_ (__NR_setresuid, sys_setresuid),	// 185
  LINXY (__NR_getresuid, sys_getresuid),	// 186
  //..    GENX_(__NR_query_module,      sys_ni_syscall),        // 
  GENXY (__NR_poll, sys_poll),	// 188
  //..    //   (__NR_nfsservctl,        sys_nfsservctl),        // 168 */Linux
  //..
  LINX_ (__NR_setresgid, sys_setresgid),	// 190
  LINXY (__NR_getresgid, sys_getresgid),	// 191
  LINXY (__NR_prctl, sys_prctl),	// 192
  PLAX_ (__NR_rt_sigreturn, sys_rt_sigreturn),	// 193
  LINXY (__NR_rt_sigaction, sys_rt_sigaction),	// 194
  LINXY (__NR_rt_sigprocmask, sys_rt_sigprocmask),	// 195
  LINXY (__NR_rt_sigpending, sys_rt_sigpending),	// 196
  LINXY (__NR_rt_sigtimedwait, sys_rt_sigtimedwait),	// 197
  LINXY (__NR_rt_sigqueueinfo, sys_rt_sigqueueinfo),	// 198
  LINX_ (__NR_rt_sigsuspend, sys_rt_sigsuspend),	// 199
  GENXY (__NR_pread64, sys_pread64),	// 200
  GENX_ (__NR_pwrite64, sys_pwrite64),	// 201
  GENX_ (__NR_chown, sys_chown),   // 202
  GENXY (__NR_getcwd, sys_getcwd), // 203
  LINXY (__NR_capget, sys_capget), // 204
  //..
  //..    LINX_(__NR_capset,            sys_capset),            // 205
  GENXY (__NR_sigaltstack, sys_sigaltstack),	// 206
  LINXY (__NR_sendfile, sys_sendfile),	// 207
  //..    GENXY(__NR_getpmsg,           sys_getpmsg),           // 208
  //..    GENX_(__NR_putpmsg,           sys_putpmsg),           // 209
  // Nb: we treat vfork as fork
  //   GENX_(__NR_vfork,             sys_fork),              // 
  GENXY (__NR_getrlimit, sys_getrlimit),	// 76
  //__NR_readahead      // 191 ppc/Linux only?
  PLAX_ (__NR_mmap2, sys_mmap2),	// 210
  //   GENX_(__NR_truncate64,        sys_truncate64),        // 211
  GENX_ (__NR_ftruncate64, sys_ftruncate64),	// 212
  //..
  PLAXY (__NR_stat64, sys_stat64),	// 213
  PLAXY (__NR_lstat64, sys_lstat64),	// 214
  PLAXY (__NR_fstat64, sys_fstat64),	// 215
  GENXY (__NR_getdents64, sys_getdents64),	// 219
  //..    //   (__NR_pivot_root,        sys_pivot_root),        //
  LINXY (__NR_fcntl64, sys_fcntl64),	// 220
  GENX_ (__NR_madvise, sys_madvise),	// 218
  GENXY (__NR_mincore, sys_mincore),	// 217
  LINX_ (__NR_gettid, sys_gettid),	// 222
  //..    LINX_(__NR_tkill,             sys_tkill),             // 208 */Linux
  //..    LINX_(__NR_setxattr,          sys_setxattr),          // 209
  //..    LINX_(__NR_lsetxattr,         sys_lsetxattr),         // 210
  //..    LINX_(__NR_fsetxattr,         sys_fsetxattr),         // 211
  LINXY (__NR_getxattr, sys_getxattr),	// 227
  LINXY (__NR_lgetxattr, sys_lgetxattr),	// 228
  LINXY (__NR_fgetxattr, sys_fgetxattr),	// 229
  LINXY (__NR_listxattr, sys_listxattr),	// 230
  LINXY (__NR_llistxattr, sys_llistxattr),	// 231
  LINXY (__NR_flistxattr, sys_flistxattr),	// 232
  LINX_ (__NR_removexattr, sys_removexattr),	// 233
  LINX_ (__NR_lremovexattr, sys_lremovexattr),	// 234
  LINX_ (__NR_fremovexattr, sys_fremovexattr),	// 235
  LINXY (__NR_futex, sys_futex),	// 238
  LINX_ (__NR_sched_setaffinity, sys_sched_setaffinity),	// 239
  LINXY (__NR_sched_getaffinity, sys_sched_getaffinity),	// 240
  /* 224 currently unused */ 
  // __NR_tuxcall                                               // 
  LINXY (__NR_sendfile64, sys_sendfile64),	// 237
  //..
  LINX_ (__NR_io_setup, sys_io_setup),	// 241
  LINX_ (__NR_io_destroy, sys_io_destroy),	// 242
  LINXY (__NR_io_getevents, sys_io_getevents),	// 243
  LINX_ (__NR_io_submit, sys_io_submit),	// 244
  LINXY (__NR_io_cancel, sys_io_cancel),	// 245
  //..
  LINX_ (__NR_set_tid_address, sys_set_tid_address),	// 252
  LINX_ (__NR_fadvise64, sys_fadvise64),	// 254
  LINX_ (__NR_exit_group, sys_exit_group),	// 246
  //..    GENXY(__NR_lookup_dcookie,    sys_lookup_dcookie),    // 247
  LINXY (__NR_epoll_create, sys_epoll_create),	// 248
  LINX_ (__NR_epoll_ctl, sys_epoll_ctl),	// 249
  LINXY (__NR_epoll_wait, sys_epoll_wait),	// 250
  //..    //   (__NR_remap_file_pages,  sys_remap_file_pages),  // 239 */Linux
  LINXY (__NR_timer_create, sys_timer_create),	// 257
  LINXY (__NR_timer_settime, sys_timer_settime),	// 258
  LINXY (__NR_timer_gettime, sys_timer_gettime),	// 259
  LINX_ (__NR_timer_getoverrun, sys_timer_getoverrun),	// 260
  LINX_ (__NR_timer_delete, sys_timer_delete),	// 261
  LINX_ (__NR_clock_settime, sys_clock_settime),	// 262
  LINXY (__NR_clock_gettime, sys_clock_gettime),	// 263
  LINXY (__NR_clock_getres, sys_clock_getres),	// 264
  LINXY (__NR_clock_nanosleep, sys_clock_nanosleep),	// 265
  // __NR_swapcontext                                           // 
  LINXY (__NR_tgkill, sys_tgkill),	// 266 */Linux
  //..    GENX_(__NR_utimes,            sys_utimes),            // 267
  GENXY (__NR_statfs64, sys_statfs64),	// 255
  GENXY (__NR_fstatfs64, sys_fstatfs64),	// 256
  LINXY (__NR_get_mempolicy, sys_get_mempolicy),	// 269
  LINX_ (__NR_set_mempolicy, sys_set_mempolicy),	// 270
  LINXY (__NR_mq_open, sys_mq_open),	// 271
  LINX_ (__NR_mq_unlink, sys_mq_unlink),	// 272
  LINX_ (__NR_mq_timedsend, sys_mq_timedsend),	// 273
  LINXY (__NR_mq_timedreceive, sys_mq_timedreceive),	// 274
  LINX_ (__NR_mq_notify, sys_mq_notify),	// 275
  LINXY (__NR_mq_getsetattr, sys_mq_getsetattr),	// 276
  // __NR_kexec_load                                            // 
  LINX_ (__NR_inotify_init, sys_inotify_init),	// 275
  LINX_ (__NR_inotify_add_watch, sys_inotify_add_watch),	// 276
  LINX_ (__NR_inotify_rm_watch, sys_inotify_rm_watch),	// 277
  PLAX_ (__NR_set_thread_area, sys_set_thread_area),	// 283
  LINXY (__NR_openat, sys_openat),	// 288
  LINX_ (__NR_mkdirat, sys_mkdirat),	// 289
  LINX_ (__NR_mknodat, sys_mknodat),	// 290
  LINX_ (__NR_fchownat, sys_fchownat),	// 291
  LINX_ (__NR_futimesat, sys_futimesat),	// 292
  PLAXY (__NR_fstatat64, sys_fstatat64),	// 293
  LINX_ (__NR_unlinkat, sys_unlinkat),	// 294
  LINX_ (__NR_renameat, sys_renameat),	// 295
  LINX_ (__NR_linkat, sys_linkat),	// 296
  LINX_ (__NR_symlinkat, sys_symlinkat),	// 297
  LINX_ (__NR_readlinkat, sys_readlinkat),	// 298
  LINX_ (__NR_fchmodat, sys_fchmodat),	// 299
  LINX_ (__NR_faccessat, sys_faccessat),	// 300
  LINXY (__NR_ppoll, sys_ppoll),	// 302
  LINX_ (__NR_set_robust_list, sys_set_robust_list),	// 309
  LINXY (__NR_get_robust_list, sys_get_robust_list),	// 310
  LINXY (__NR_epoll_pwait, sys_epoll_pwait),	// 313
  LINX_ (__NR_utimensat, sys_utimensat),	// 316
  LINX_ (__NR_fallocate, sys_fallocate),	// 320
  LINXY (__NR_timerfd_create, sys_timerfd_create),    // 321
  LINXY (__NR_timerfd_gettime, sys_timerfd_gettime),  // 322
  LINXY (__NR_timerfd_settime, sys_timerfd_settime),  // 323
  LINXY (__NR_signalfd4, sys_signalfd4),	// 324
  LINX_ (__NR_eventfd2, sys_eventfd2),	// 325
  LINXY (__NR_pipe2, sys_pipe2),	// 328
  LINXY (__NR_inotify_init1, sys_inotify_init1),	// 329
  LINXY (__NR_prlimit64, sys_prlimit64) // 338
};

SyscallTableEntry* ML_(get_linux_syscall_entry) (UInt sysno)
{
  const UInt syscall_main_table_size
   = sizeof (syscall_main_table) / sizeof (syscall_main_table[0]);
  /* Is it in the contiguous initial section of the table? */
  if (sysno < syscall_main_table_size)
    {
      SyscallTableEntry * sys = &syscall_main_table[sysno];
      if (sys->before == NULL)
        return NULL;    /* no entry */
      else
        return sys;
    }
  /* Can't find a wrapper */
  return NULL;
}

#endif // defined(VGP_mips32_linux)

/*--------------------------------------------------------------------*/ 
/*--- end                                     syswrap-mips-linux.c ---*/ 
/*--------------------------------------------------------------------*/ 
