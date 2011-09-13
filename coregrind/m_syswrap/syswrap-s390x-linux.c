
/*--------------------------------------------------------------------*/
/*--- Platform-specific syscalls stuff.      syswrap-s390x-linux.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright IBM Corp. 2010-2011

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

/* Contributed by Christian Borntraeger */

#if defined(VGP_s390x_linux)

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
#include "pub_core_mallocfree.h"
#include "pub_core_options.h"
#include "pub_core_scheduler.h"
#include "pub_core_sigframe.h"      // For VG_(sigframe_destroy)()
#include "pub_core_signals.h"
#include "pub_core_syscall.h"
#include "pub_core_syswrap.h"
#include "pub_core_tooliface.h"
#include "pub_core_stacks.h"        // VG_(register_stack)

#include "priv_types_n_macros.h"
#include "priv_syswrap-generic.h"    /* for decls of generic wrappers */
#include "priv_syswrap-linux.h"      /* for decls of linux-ish wrappers */
#include "priv_syswrap-linux-variants.h" /* decls of linux variant wrappers */
#include "priv_syswrap-main.h"


/* ---------------------------------------------------------------------
   clone() handling
   ------------------------------------------------------------------ */

/* Call f(arg1), but first switch stacks, using 'stack' as the new
   stack, and use 'retaddr' as f's return-to address.  Also, clear all
   the integer registers before entering f.
   Thought: Why are we clearing the GPRs ? The callee pointed to by f
   is a regular C function which will play by the ABI rules. So there is
   no need to zero out the GPRs. If we assumed that f accesses registers at
   will, then it would make sense to create a defined register state.
   But then, why only for the GPRs and not the FPRs ? */
__attribute__((noreturn))
void ML_(call_on_new_stack_0_1) ( Addr stack,
                                  Addr retaddr,
                                  void (*f)(Word),
                                  Word arg1 );
/* Upon entering this function we have the following setup:
     r2 = stack
     r3 = retaddr
     r4 = f_desc
     r5 = arg1
*/
asm(
    ".text\n"
    ".align 4\n"
    ".globl vgModuleLocal_call_on_new_stack_0_1\n"
    ".type vgModuleLocal_call_on_new_stack_0_1, @function\n"
    "vgModuleLocal_call_on_new_stack_0_1:\n"
    "   lgr %r15,%r2\n"     // stack to r15
    "   lgr %r14,%r3\n"     // retaddr to r14
    "   lgr %r2,%r5\n"      // arg1 to r2
    // zero all gprs to get a defined state
    "   lghi  %r0,0\n"
    "   lghi  %r1,0\n"
    // r2 holds the argument for the callee
    "   lghi  %r3,0\n"
    // r4 holds the callee address
    "   lghi  %r5,0\n"
    "   lghi  %r6,0\n"
    "   lghi  %r7,0\n"
    "   lghi  %r8,0\n"
    "   lghi  %r9,0\n"
    "   lghi  %r10,0\n"
    "   lghi  %r11,0\n"
    "   lghi  %r12,0\n"
    "   lghi  %r13,0\n"
    // r14 holds the return address for the callee
    // r15 is the stack pointer
    "   br  %r4\n"          // jump to f
    ".previous\n"
    );

/*
        Perform a clone system call.  clone is strange because it has
        fork()-like return-twice semantics, so it needs special
        handling here.

        Upon entry, we have:
            void*  child_stack   in r2
            long   flags         in r3
            int*   parent_tid    in r4
            int*   child_tid     in r5
            int*   child_tid     in r6
            Word   (*fn)(void *) 160(r15)
            void   *arg          168(r15)

        System call requires:
            void*  child_stack  in r2  (sc arg1)
            long   flags        in r3  (sc arg2)
            int*   parent_tid   in r4  (sc arg3)
            int*   child_tid    in r5  (sc arg4)
            void*  tlsaddr      in r6  (sc arg5)

        Returns a ULong encoded as: top half is %cr following syscall,
        low half is syscall return value (r3).
 */
#define __NR_CLONE        VG_STRINGIFY(__NR_clone)
#define __NR_EXIT         VG_STRINGIFY(__NR_exit)

extern
ULong do_syscall_clone_s390x_linux ( void  *stack,
                                     ULong flags,
                                     Int   *child_tid,
                                     Int   *parent_tid,
                                     Addr  tlsaddr,
                                     Word (*fn)(void *),
                                     void  *arg);
asm(
   "   .text\n"
   "   .align  4\n"
   "do_syscall_clone_s390x_linux:\n"
   "   lg    %r1, 160(%r15)\n"   // save fn from parent stack into r1
   "   lg    %r0, 168(%r15)\n"   // save arg from parent stack into r0
   "   aghi  %r2, -160\n"        // create stack frame for child
   // all syscall parameters are already in place (r2-r6)
   "   svc " __NR_CLONE"\n"        // clone()
   "   ltgr  %r2,%r2\n"           // child if retval == 0
   "   jne   1f\n"

   // CHILD - call thread function
   "   lgr   %r2, %r0\n"            // get arg from r0
   "   basr  %r14,%r1\n"            // call fn

   // exit. The result is already in r2
   "   svc " __NR_EXIT"\n"

   // Exit returned?!
   "   j +2\n"

   "1:\n"  // PARENT or ERROR
   "   br %r14\n"
   ".previous\n"
);

#undef __NR_CLONE
#undef __NR_EXIT

void VG_(cleanup_thread) ( ThreadArchState* arch )
{
  /* only used on x86 for descriptor tables */
}

static void setup_child ( /*OUT*/ ThreadArchState *child,
                   /*IN*/  ThreadArchState *parent )
{
   /* We inherit our parent's guest state. */
   child->vex = parent->vex;
   child->vex_shadow1 = parent->vex_shadow1;
   child->vex_shadow2 = parent->vex_shadow2;
}


/*
   When a client clones, we need to keep track of the new thread.  This means:
   1. allocate a ThreadId+ThreadState+stack for the the thread

   2. initialize the thread's new VCPU state

   3. create the thread using the same args as the client requested,
   but using the scheduler entrypoint for IP, and a separate stack
   for SP.
 */
static SysRes do_clone ( ThreadId ptid,
                         Addr sp, ULong flags,
                         Int *parent_tidptr,
                         Int *child_tidptr,
                         Addr tlsaddr)
{
   static const Bool debug = False;

   ThreadId     ctid = VG_(alloc_ThreadState)();
   ThreadState* ptst = VG_(get_ThreadState)(ptid);
   ThreadState* ctst = VG_(get_ThreadState)(ctid);
   UWord*       stack;
   NSegment const* seg;
   SysRes       res;
   ULong        r2;
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

      If the clone call specifies a NULL sp for the new thread, then
      it actually gets a copy of the parent's sp.
   */
   setup_child( &ctst->arch, &ptst->arch );

   /* Make sys_clone appear to have returned Success(0) in the
      child. */
   ctst->arch.vex.guest_r2 = 0;

   if (sp != 0)
      ctst->arch.vex.guest_r15 = sp;

   ctst->os_state.parent = ptid;

   /* inherit signal mask */
   ctst->sig_mask = ptst->sig_mask;
   ctst->tmp_sig_mask = ptst->sig_mask;

   /* have the parents thread group */
   ctst->os_state.threadgroup = ptst->os_state.threadgroup;

   /* We don't really know where the client stack is, because its
      allocated by the client.  The best we can do is look at the
      memory mappings and try to derive some useful information.  We
      assume that esp starts near its highest possible value, and can
      only go down to the start of the mmaped segment. */
   seg = VG_(am_find_nsegment)((Addr)sp);
   if (seg && seg->kind != SkResvn) {
      ctst->client_stack_highest_word = (Addr)VG_PGROUNDUP(sp);
      ctst->client_stack_szB = ctst->client_stack_highest_word - seg->start;

      VG_(register_stack)(seg->start, ctst->client_stack_highest_word);

      if (debug)
	 VG_(printf)("tid %d: guessed client stack range %#lx-%#lx\n",
		     ctid, seg->start, VG_PGROUNDUP(sp));
   } else {
      VG_(message)(Vg_UserMsg,
                   "!? New thread %d starts with SP(%#lx) unmapped\n",
		   ctid, sp);
      ctst->client_stack_szB  = 0;
   }

   /* Assume the clone will succeed, and tell any tool that wants to
      know that this thread has come into existence.  If the clone
      fails, we'll send out a ll_exit notification for it at the out:
      label below, to clean up. */
   VG_TRACK ( pre_thread_ll_create, ptid, ctid );

   if (flags & VKI_CLONE_SETTLS) {
      if (debug)
	 VG_(printf)("clone child has SETTLS: tls at %#lx\n", tlsaddr);
      ctst->arch.vex.guest_a0 = (UInt) (tlsaddr >> 32);
      ctst->arch.vex.guest_a1 = (UInt) tlsaddr;
   }
   flags &= ~VKI_CLONE_SETTLS;

   /* start the thread with everything blocked */
   VG_(sigprocmask)(VKI_SIG_SETMASK, &blockall, &savedmask);

   /* Create the new thread */
   r2 = do_syscall_clone_s390x_linux(
            stack, flags, child_tidptr, parent_tidptr, tlsaddr,
            ML_(start_thread_NORETURN), &VG_(threads)[ctid]);

   res = VG_(mk_SysRes_s390x_linux)( r2 );

   VG_(sigprocmask)(VKI_SIG_SETMASK, &savedmask, NULL);

  out:
   if (sr_isError(res)) {
      /* clone failed */
      ctst->status = VgTs_Empty;
      /* oops.  Better tell the tool the thread exited in a hurry :-) */
      VG_TRACK( pre_thread_ll_exit, ctid );
   }

   return res;

}



/* ---------------------------------------------------------------------
   PRE/POST wrappers for s390x/Linux-specific syscalls
   ------------------------------------------------------------------ */

#define PRE(name)       DEFN_PRE_TEMPLATE(s390x_linux, name)
#define POST(name)      DEFN_POST_TEMPLATE(s390x_linux, name)

/* Add prototypes for the wrappers declared here, so that gcc doesn't
   harass us for not having prototypes.  Really this is a kludge --
   the right thing to do is to make these wrappers 'static' since they
   aren't visible outside this file, but that requires even more macro
   magic. */

DECL_TEMPLATE(s390x_linux, sys_ptrace);
DECL_TEMPLATE(s390x_linux, sys_socketcall);
DECL_TEMPLATE(s390x_linux, sys_mmap);
DECL_TEMPLATE(s390x_linux, sys_ipc);
DECL_TEMPLATE(s390x_linux, sys_clone);
DECL_TEMPLATE(s390x_linux, sys_sigreturn);
DECL_TEMPLATE(s390x_linux, sys_rt_sigreturn);
DECL_TEMPLATE(s390x_linux, sys_fadvise64);

// PEEK TEXT,DATA and USER are common to all architectures
// PEEKUSR_AREA and POKEUSR_AREA are special, having a memory area
// containing the real addr, data, and len field pointed to by ARG3
// instead of ARG4
PRE(sys_ptrace)
{
   PRINT("sys_ptrace ( %ld, %ld, %#lx, %#lx )", ARG1,ARG2,ARG3,ARG4);
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
   case VKI_PTRACE_PEEKUSR_AREA:
      {
         vki_ptrace_area *pa;

         /* Reads a part of the user area into memory at pa->process_addr */
	 pa = (vki_ptrace_area *) ARG3;
         PRE_MEM_READ("ptrace(peekusrarea ptrace_area->len)",
                      (unsigned long) &pa->vki_len, sizeof(pa->vki_len));
         PRE_MEM_READ("ptrace(peekusrarea ptrace_area->kernel_addr)",
                      (unsigned long) &pa->vki_kernel_addr, sizeof(pa->vki_kernel_addr));
         PRE_MEM_READ("ptrace(peekusrarea ptrace_area->process_addr)",
                      (unsigned long) &pa->vki_process_addr, sizeof(pa->vki_process_addr));
         PRE_MEM_WRITE("ptrace(peekusrarea *(ptrace_area->process_addr))",
                       pa->vki_process_addr, pa->vki_len);
         break;
      }
   case VKI_PTRACE_POKEUSR_AREA:
      {
         vki_ptrace_area *pa;

         /* Updates a part of the user area from memory at pa->process_addr */
	 pa = (vki_ptrace_area *) ARG3;
         PRE_MEM_READ("ptrace(pokeusrarea ptrace_area->len)",
                      (unsigned long) &pa->vki_len, sizeof(pa->vki_len));
         PRE_MEM_READ("ptrace(pokeusrarea ptrace_area->kernel_addr)",
                      (unsigned long) &pa->vki_kernel_addr,
                      sizeof(pa->vki_kernel_addr));
         PRE_MEM_READ("ptrace(pokeusrarea ptrace_area->process_addr)",
                      (unsigned long) &pa->vki_process_addr,
                      sizeof(pa->vki_process_addr));
         PRE_MEM_READ("ptrace(pokeusrarea *(ptrace_area->process_addr))",
                       pa->vki_process_addr, pa->vki_len);
         break;
      }
   default:
      break;
   }
}

POST(sys_ptrace)
{
   switch (ARG1) {
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
   case VKI_PTRACE_PEEKUSR_AREA:
      {
         vki_ptrace_area *pa;

	 pa = (vki_ptrace_area *) ARG3;
         POST_MEM_WRITE(pa->vki_process_addr, pa->vki_len);
      }
   default:
      break;
   }
}


PRE(sys_socketcall)
{
#  define ARG2_0  (((UWord*)ARG2)[0])
#  define ARG2_1  (((UWord*)ARG2)[1])
#  define ARG2_2  (((UWord*)ARG2)[2])
#  define ARG2_3  (((UWord*)ARG2)[3])
#  define ARG2_4  (((UWord*)ARG2)[4])
#  define ARG2_5  (((UWord*)ARG2)[5])

   *flags |= SfMayBlock;
   PRINT("sys_socketcall ( %ld, %#lx )",ARG1,ARG2);
   PRE_REG_READ2(long, "socketcall", int, call, unsigned long *, args);

   switch (ARG1 /* request */) {

   case VKI_SYS_SOCKETPAIR:
     /* int socketpair(int d, int type, int protocol, int sv[2]); */
      PRE_MEM_READ( "socketcall.socketpair(args)", ARG2, 4*sizeof(Addr) );
      if (!ML_(valid_client_addr)(ARG2, 4*sizeof(Addr), tid, NULL)) {
         SET_STATUS_Failure( VKI_EFAULT );
         break;
      }
      ML_(generic_PRE_sys_socketpair)( tid, ARG2_0, ARG2_1, ARG2_2, ARG2_3 );
      break;

   case VKI_SYS_SOCKET:
     /* int socket(int domain, int type, int protocol); */
      PRE_MEM_READ( "socketcall.socket(args)", ARG2, 3*sizeof(Addr) );
      if (!ML_(valid_client_addr)(ARG2, 3*sizeof(Addr), tid, NULL)) {
         SET_STATUS_Failure( VKI_EFAULT );
         break;
      }
      break;

   case VKI_SYS_BIND:
     /* int bind(int sockfd, struct sockaddr *my_addr,
	int addrlen); */
      PRE_MEM_READ( "socketcall.bind(args)", ARG2, 3*sizeof(Addr) );
      if (!ML_(valid_client_addr)(ARG2, 3*sizeof(Addr), tid, NULL)) {
         SET_STATUS_Failure( VKI_EFAULT );
         break;
      }
      ML_(generic_PRE_sys_bind)( tid, ARG2_0, ARG2_1, ARG2_2 );
      break;

   case VKI_SYS_LISTEN:
     /* int listen(int s, int backlog); */
      PRE_MEM_READ( "socketcall.listen(args)", ARG2, 2*sizeof(Addr) );
      if (!ML_(valid_client_addr)(ARG2, 2*sizeof(Addr), tid, NULL)) {
         SET_STATUS_Failure( VKI_EFAULT );
         break;
      }
      break;

   case VKI_SYS_ACCEPT: {
     /* int accept(int s, struct sockaddr *addr, int *addrlen); */
      PRE_MEM_READ( "socketcall.accept(args)", ARG2, 3*sizeof(Addr) );
      if (!ML_(valid_client_addr)(ARG2, 3*sizeof(Addr), tid, NULL)) {
         SET_STATUS_Failure( VKI_EFAULT );
         break;
      }
      ML_(generic_PRE_sys_accept)( tid, ARG2_0, ARG2_1, ARG2_2 );
      break;
   }

   case VKI_SYS_SENDTO:
     /* int sendto(int s, const void *msg, int len,
                    unsigned int flags,
                    const struct sockaddr *to, int tolen); */
     PRE_MEM_READ( "socketcall.sendto(args)", ARG2, 6*sizeof(Addr) );
     if (!ML_(valid_client_addr)(ARG2, 6*sizeof(Addr), tid, NULL)) {
         SET_STATUS_Failure( VKI_EFAULT );
         break;
     }
     ML_(generic_PRE_sys_sendto)( tid, ARG2_0, ARG2_1, ARG2_2,
				  ARG2_3, ARG2_4, ARG2_5 );
     break;

   case VKI_SYS_SEND:
     /* int send(int s, const void *msg, size_t len, int flags); */
     PRE_MEM_READ( "socketcall.send(args)", ARG2, 4*sizeof(Addr) );
     if (!ML_(valid_client_addr)(ARG2, 4*sizeof(Addr), tid, NULL)) {
         SET_STATUS_Failure( VKI_EFAULT );
         break;
     }
     ML_(generic_PRE_sys_send)( tid, ARG2_0, ARG2_1, ARG2_2 );
     break;

   case VKI_SYS_RECVFROM:
     /* int recvfrom(int s, void *buf, int len, unsigned int flags,
	struct sockaddr *from, int *fromlen); */
     PRE_MEM_READ( "socketcall.recvfrom(args)", ARG2, 6*sizeof(Addr) );
     if (!ML_(valid_client_addr)(ARG2, 6*sizeof(Addr), tid, NULL)) {
         SET_STATUS_Failure( VKI_EFAULT );
         break;
     }
     ML_(generic_PRE_sys_recvfrom)( tid, ARG2_0, ARG2_1, ARG2_2,
				    ARG2_3, ARG2_4, ARG2_5 );
     break;

   case VKI_SYS_RECV:
     /* int recv(int s, void *buf, int len, unsigned int flags); */
     /* man 2 recv says:
         The  recv call is normally used only on a connected socket
         (see connect(2)) and is identical to recvfrom with a  NULL
         from parameter.
     */
     PRE_MEM_READ( "socketcall.recv(args)", ARG2, 4*sizeof(Addr) );
     if (!ML_(valid_client_addr)(ARG2, 4*sizeof(Addr), tid, NULL)) {
         SET_STATUS_Failure( VKI_EFAULT );
         break;
     }
     ML_(generic_PRE_sys_recv)( tid, ARG2_0, ARG2_1, ARG2_2 );
     break;

   case VKI_SYS_CONNECT:
     /* int connect(int sockfd,
	struct sockaddr *serv_addr, int addrlen ); */
     PRE_MEM_READ( "socketcall.connect(args)", ARG2, 3*sizeof(Addr) );
     if (!ML_(valid_client_addr)(ARG2, 3*sizeof(Addr), tid, NULL)) {
         SET_STATUS_Failure( VKI_EFAULT );
         break;
     }
     ML_(generic_PRE_sys_connect)( tid, ARG2_0, ARG2_1, ARG2_2 );
     break;

   case VKI_SYS_SETSOCKOPT:
     /* int setsockopt(int s, int level, int optname,
	const void *optval, int optlen); */
     PRE_MEM_READ( "socketcall.setsockopt(args)", ARG2, 5*sizeof(Addr) );
     if (!ML_(valid_client_addr)(ARG2, 5*sizeof(Addr), tid, NULL)) {
         SET_STATUS_Failure( VKI_EFAULT );
         break;
     }
     ML_(generic_PRE_sys_setsockopt)( tid, ARG2_0, ARG2_1, ARG2_2,
				      ARG2_3, ARG2_4 );
     break;

   case VKI_SYS_GETSOCKOPT:
     /* int getsockopt(int s, int level, int optname,
	void *optval, socklen_t *optlen); */
     PRE_MEM_READ( "socketcall.getsockopt(args)", ARG2, 5*sizeof(Addr) );
     if (!ML_(valid_client_addr)(ARG2, 5*sizeof(Addr), tid, NULL)) {
         SET_STATUS_Failure( VKI_EFAULT );
         break;
     }
     ML_(linux_PRE_sys_getsockopt)( tid, ARG2_0, ARG2_1, ARG2_2,
				      ARG2_3, ARG2_4 );
     break;

   case VKI_SYS_GETSOCKNAME:
     /* int getsockname(int s, struct sockaddr* name, int* namelen) */
     PRE_MEM_READ( "socketcall.getsockname(args)", ARG2, 3*sizeof(Addr) );
     if (!ML_(valid_client_addr)(ARG2, 3*sizeof(Addr), tid, NULL)) {
         SET_STATUS_Failure( VKI_EFAULT );
         break;
     }
     ML_(generic_PRE_sys_getsockname)( tid, ARG2_0, ARG2_1, ARG2_2 );
     break;

   case VKI_SYS_GETPEERNAME:
     /* int getpeername(int s, struct sockaddr* name, int* namelen) */
     PRE_MEM_READ( "socketcall.getpeername(args)", ARG2, 3*sizeof(Addr) );
     if (!ML_(valid_client_addr)(ARG2, 3*sizeof(Addr), tid, NULL)) {
         SET_STATUS_Failure( VKI_EFAULT );
         break;
     }
     ML_(generic_PRE_sys_getpeername)( tid, ARG2_0, ARG2_1, ARG2_2 );
     break;

   case VKI_SYS_SHUTDOWN:
     /* int shutdown(int s, int how); */
     PRE_MEM_READ( "socketcall.shutdown(args)", ARG2, 2*sizeof(Addr) );
     if (!ML_(valid_client_addr)(ARG2, 2*sizeof(Addr), tid, NULL)) {
         SET_STATUS_Failure( VKI_EFAULT );
         break;
     }
     break;

   case VKI_SYS_SENDMSG: {
     /* int sendmsg(int s, const struct msghdr *msg, int flags); */
     PRE_MEM_READ( "socketcall.sendmsg(args)", ARG2, 3*sizeof(Addr) );
     if (!ML_(valid_client_addr)(ARG2, 3*sizeof(Addr), tid, NULL)) {
         SET_STATUS_Failure( VKI_EFAULT );
         break;
     }
     ML_(generic_PRE_sys_sendmsg)( tid, ARG2_0, ARG2_1 );
     break;
   }

   case VKI_SYS_RECVMSG: {
     /* int recvmsg(int s, struct msghdr *msg, int flags); */
     PRE_MEM_READ("socketcall.recvmsg(args)", ARG2, 3*sizeof(Addr) );
     if (!ML_(valid_client_addr)(ARG2, 3*sizeof(Addr), tid, NULL)) {
         SET_STATUS_Failure( VKI_EFAULT );
         break;
     }
     ML_(generic_PRE_sys_recvmsg)( tid, ARG2_0, ARG2_1 );
     break;
   }

   default:
     VG_(message)(Vg_DebugMsg,"Warning: unhandled socketcall 0x%lx\n",ARG1);
     SET_STATUS_Failure( VKI_EINVAL );
     break;
   }
#  undef ARG2_0
#  undef ARG2_1
#  undef ARG2_2
#  undef ARG2_3
#  undef ARG2_4
#  undef ARG2_5
}

POST(sys_socketcall)
{
#  define ARG2_0  (((UWord*)ARG2)[0])
#  define ARG2_1  (((UWord*)ARG2)[1])
#  define ARG2_2  (((UWord*)ARG2)[2])
#  define ARG2_3  (((UWord*)ARG2)[3])
#  define ARG2_4  (((UWord*)ARG2)[4])
#  define ARG2_5  (((UWord*)ARG2)[5])

  SysRes r;
  vg_assert(SUCCESS);
  switch (ARG1 /* request */) {

  case VKI_SYS_SOCKETPAIR:
    r = ML_(generic_POST_sys_socketpair)(
					 tid, VG_(mk_SysRes_Success)(RES),
					 ARG2_0, ARG2_1, ARG2_2, ARG2_3
					 );
    SET_STATUS_from_SysRes(r);
    break;

  case VKI_SYS_SOCKET:
    r = ML_(generic_POST_sys_socket)( tid, VG_(mk_SysRes_Success)(RES) );
    SET_STATUS_from_SysRes(r);
    break;

  case VKI_SYS_BIND:
    /* int bind(int sockfd, struct sockaddr *my_addr,
       int addrlen); */
    break;

  case VKI_SYS_LISTEN:
    /* int listen(int s, int backlog); */
    break;

  case VKI_SYS_ACCEPT:
    /* int accept(int s, struct sockaddr *addr, int *addrlen); */
    r = ML_(generic_POST_sys_accept)( tid, VG_(mk_SysRes_Success)(RES),
				      ARG2_0, ARG2_1, ARG2_2 );
    SET_STATUS_from_SysRes(r);
    break;

  case VKI_SYS_SENDTO:
    break;

  case VKI_SYS_SEND:
    break;

  case VKI_SYS_RECVFROM:
    ML_(generic_POST_sys_recvfrom)( tid, VG_(mk_SysRes_Success)(RES),
				    ARG2_0, ARG2_1, ARG2_2,
				    ARG2_3, ARG2_4, ARG2_5 );
    break;

  case VKI_SYS_RECV:
    ML_(generic_POST_sys_recv)( tid, RES, ARG2_0, ARG2_1, ARG2_2 );
    break;

  case VKI_SYS_CONNECT:
    break;

  case VKI_SYS_SETSOCKOPT:
    break;

  case VKI_SYS_GETSOCKOPT:
    ML_(linux_POST_sys_getsockopt)( tid, VG_(mk_SysRes_Success)(RES),
				      ARG2_0, ARG2_1,
				      ARG2_2, ARG2_3, ARG2_4 );
    break;

  case VKI_SYS_GETSOCKNAME:
    ML_(generic_POST_sys_getsockname)( tid, VG_(mk_SysRes_Success)(RES),
				       ARG2_0, ARG2_1, ARG2_2 );
    break;

  case VKI_SYS_GETPEERNAME:
    ML_(generic_POST_sys_getpeername)( tid, VG_(mk_SysRes_Success)(RES),
				       ARG2_0, ARG2_1, ARG2_2 );
    break;

  case VKI_SYS_SHUTDOWN:
    break;

  case VKI_SYS_SENDMSG:
    break;

  case VKI_SYS_RECVMSG:
    ML_(generic_POST_sys_recvmsg)( tid, ARG2_0, ARG2_1 );
    break;

  default:
    VG_(message)(Vg_DebugMsg,"FATAL: unhandled socketcall 0x%lx\n",ARG1);
    VG_(core_panic)("... bye!\n");
    break; /*NOTREACHED*/
  }
#  undef ARG2_0
#  undef ARG2_1
#  undef ARG2_2
#  undef ARG2_3
#  undef ARG2_4
#  undef ARG2_5
}

PRE(sys_mmap)
{
   UWord a0, a1, a2, a3, a4, a5;
   SysRes r;

   UWord* args = (UWord*)ARG1;
   PRE_REG_READ1(long, "sys_mmap", struct mmap_arg_struct *, args);
   PRE_MEM_READ( "sys_mmap(args)", (Addr) args, 6*sizeof(UWord) );

   a0 = args[0];
   a1 = args[1];
   a2 = args[2];
   a3 = args[3];
   a4 = args[4];
   a5 = args[5];

   PRINT("sys_mmap ( %#lx, %llu, %ld, %ld, %ld, %ld )",
         a0, (ULong)a1, a2, a3, a4, a5 );

   r = ML_(generic_PRE_sys_mmap)( tid, a0, a1, a2, a3, a4, (Off64T)a5 );
   SET_STATUS_from_SysRes(r);
}

static Addr deref_Addr ( ThreadId tid, Addr a, Char* s )
{
   Addr* a_p = (Addr*)a;
   PRE_MEM_READ( s, (Addr)a_p, sizeof(Addr) );
   return *a_p;
}

PRE(sys_ipc)
{
  PRINT("sys_ipc ( %ld, %ld, %ld, %ld, %#lx, %ld )",
        ARG1,ARG2,ARG3,ARG4,ARG5,ARG6);
  // XXX: this is simplistic -- some args are not used in all circumstances.
  PRE_REG_READ6(int, "ipc",
		vki_uint, call, int, first, int, second, int, third,
		void *, ptr, long, fifth)

    switch (ARG1 /* call */) {
    case VKI_SEMOP:
      ML_(generic_PRE_sys_semop)( tid, ARG2, ARG5, ARG3 );
      *flags |= SfMayBlock;
      break;
    case VKI_SEMGET:
      break;
    case VKI_SEMCTL:
      {
	UWord arg = deref_Addr( tid, ARG5, "semctl(arg)" );
	ML_(generic_PRE_sys_semctl)( tid, ARG2, ARG3, ARG4, arg );
	break;
      }
    case VKI_SEMTIMEDOP:
      ML_(generic_PRE_sys_semtimedop)( tid, ARG2, ARG5, ARG3, ARG6 );
      *flags |= SfMayBlock;
      break;
    case VKI_MSGSND:
      ML_(linux_PRE_sys_msgsnd)( tid, ARG2, ARG5, ARG3, ARG4 );
      if ((ARG4 & VKI_IPC_NOWAIT) == 0)
	*flags |= SfMayBlock;
      break;
    case VKI_MSGRCV:
      {
	Addr msgp;
	Word msgtyp;

	msgp = deref_Addr( tid,
			   (Addr) (&((struct vki_ipc_kludge *)ARG5)->msgp),
			   "msgrcv(msgp)" );
	msgtyp = deref_Addr( tid,
			     (Addr) (&((struct vki_ipc_kludge *)ARG5)->msgtyp),
			     "msgrcv(msgp)" );

	ML_(linux_PRE_sys_msgrcv)( tid, ARG2, msgp, ARG3, msgtyp, ARG4 );

	if ((ARG4 & VKI_IPC_NOWAIT) == 0)
	  *flags |= SfMayBlock;
	break;
      }
    case VKI_MSGGET:
      break;
    case VKI_MSGCTL:
      ML_(linux_PRE_sys_msgctl)( tid, ARG2, ARG3, ARG5 );
      break;
    case VKI_SHMAT:
      {
	UWord w;
	PRE_MEM_WRITE( "shmat(raddr)", ARG4, sizeof(Addr) );
	w = ML_(generic_PRE_sys_shmat)( tid, ARG2, ARG5, ARG3 );
	if (w == 0)
	  SET_STATUS_Failure( VKI_EINVAL );
	else
	  ARG5 = w;
	break;
      }
    case VKI_SHMDT:
      if (!ML_(generic_PRE_sys_shmdt)(tid, ARG5))
	SET_STATUS_Failure( VKI_EINVAL );
      break;
    case VKI_SHMGET:
      break;
    case VKI_SHMCTL: /* IPCOP_shmctl */
      ML_(generic_PRE_sys_shmctl)( tid, ARG2, ARG3, ARG5 );
      break;
    default:
      VG_(message)(Vg_DebugMsg, "FATAL: unhandled syscall(ipc) %ld", ARG1 );
      VG_(core_panic)("... bye!\n");
      break; /*NOTREACHED*/
    }
}

POST(sys_ipc)
{
  vg_assert(SUCCESS);
  switch (ARG1 /* call */) {
  case VKI_SEMOP:
  case VKI_SEMGET:
    break;
  case VKI_SEMCTL:
    {
      UWord arg = deref_Addr( tid, ARG5, "semctl(arg)" );
      ML_(generic_PRE_sys_semctl)( tid, ARG2, ARG3, ARG4, arg );
      break;
    }
  case VKI_SEMTIMEDOP:
  case VKI_MSGSND:
    break;
  case VKI_MSGRCV:
    {
      Addr msgp;
      Word msgtyp;

      msgp = deref_Addr( tid,
                         (Addr) (&((struct vki_ipc_kludge *)ARG5)->msgp),
                         "msgrcv(msgp)" );
      msgtyp = deref_Addr( tid,
                           (Addr) (&((struct vki_ipc_kludge *)ARG5)->msgtyp),
                           "msgrcv(msgp)" );

      ML_(linux_POST_sys_msgrcv)( tid, RES, ARG2, msgp, ARG3, msgtyp, ARG4 );
      break;
    }
  case VKI_MSGGET:
    break;
  case VKI_MSGCTL:
    ML_(linux_POST_sys_msgctl)( tid, RES, ARG2, ARG3, ARG5 );
    break;
  case VKI_SHMAT:
    {
      Addr addr;

      /* force readability. before the syscall it is
       * indeed uninitialized, as can be seen in
       * glibc/sysdeps/unix/sysv/linux/shmat.c */
      POST_MEM_WRITE( ARG4, sizeof( Addr ) );

      addr = deref_Addr ( tid, ARG4, "shmat(addr)" );
      ML_(generic_POST_sys_shmat)( tid, addr, ARG2, ARG5, ARG3 );
      break;
    }
  case VKI_SHMDT:
    ML_(generic_POST_sys_shmdt)( tid, RES, ARG5 );
    break;
  case VKI_SHMGET:
    break;
  case VKI_SHMCTL:
    ML_(generic_POST_sys_shmctl)( tid, RES, ARG2, ARG3, ARG5 );
    break;
  default:
    VG_(message)(Vg_DebugMsg,
		 "FATAL: unhandled syscall(ipc) %ld",
		 ARG1 );
    VG_(core_panic)("... bye!\n");
    break; /*NOTREACHED*/
  }
}

PRE(sys_clone)
{
   UInt cloneflags;

   PRINT("sys_clone ( %lx, %#lx, %#lx, %#lx, %#lx )",ARG1,ARG2,ARG3,ARG4, ARG5);
   PRE_REG_READ2(int, "clone",
                 void *,        child_stack,
                 unsigned long, flags);

   if (ARG2 & VKI_CLONE_PARENT_SETTID) {
      if (VG_(tdict).track_pre_reg_read)
         PRA3("clone(parent_tidptr)", int *, parent_tidptr);
      PRE_MEM_WRITE("clone(parent_tidptr)", ARG3, sizeof(Int));
      if (!VG_(am_is_valid_for_client)(ARG3, sizeof(Int),
                                             VKI_PROT_WRITE)) {
         SET_STATUS_Failure( VKI_EFAULT );
         return;
      }
   }
   if (ARG2 & (VKI_CLONE_CHILD_SETTID | VKI_CLONE_CHILD_CLEARTID)) {
      if (VG_(tdict).track_pre_reg_read)
         PRA4("clone(child_tidptr)", int *, child_tidptr);
      PRE_MEM_WRITE("clone(child_tidptr)", ARG4, sizeof(Int));
      if (!VG_(am_is_valid_for_client)(ARG4, sizeof(Int),
                                             VKI_PROT_WRITE)) {
         SET_STATUS_Failure( VKI_EFAULT );
         return;
      }
   }

   cloneflags = ARG2;

   if (!ML_(client_signal_OK)(ARG2 & VKI_CSIGNAL)) {
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
                  (Addr)ARG1,   /* child SP */
                  ARG2,         /* flags */
                  (Int *)ARG3,  /* parent_tidptr */
                  (Int *)ARG4, /* child_tidptr */
                  (Addr)ARG5)); /*  tlsaddr */
      break;

   case VKI_CLONE_VFORK | VKI_CLONE_VM: /* vfork */
      /* FALLTHROUGH - assume vfork == fork */
      cloneflags &= ~(VKI_CLONE_VFORK | VKI_CLONE_VM);

   case 0: /* plain fork */
      SET_STATUS_from_SysRes(
         ML_(do_fork_clone)(tid,
                       cloneflags,      /* flags */
                       (Int *)ARG3,     /* parent_tidptr */
                       (Int *)ARG4));   /* child_tidptr */
      break;

   default:
      /* should we just ENOSYS? */
      VG_(message)(Vg_UserMsg, "Unsupported clone() flags: 0x%lx", ARG2);
      VG_(message)(Vg_UserMsg, "");
      VG_(message)(Vg_UserMsg, "The only supported clone() uses are:");
      VG_(message)(Vg_UserMsg, " - via a threads library (NPTL)");
      VG_(message)(Vg_UserMsg, " - via the implementation of fork or vfork");
      VG_(unimplemented)
         ("Valgrind does not support general clone().");
   }

   if (SUCCESS) {
      if (ARG2 & VKI_CLONE_PARENT_SETTID)
         POST_MEM_WRITE(ARG3, sizeof(Int));
      if (ARG2 & (VKI_CLONE_CHILD_SETTID | VKI_CLONE_CHILD_CLEARTID))
         POST_MEM_WRITE(ARG4, sizeof(Int));

      /* Thread creation was successful; let the child have the chance
         to run */
      *flags |= SfYieldAfter;
   }
}

PRE(sys_sigreturn)
{
   ThreadState* tst;
   PRINT("sys_sigreturn ( )");

   vg_assert(VG_(is_valid_tid)(tid));
   vg_assert(tid >= 1 && tid < VG_N_THREADS);
   vg_assert(VG_(is_running_thread)(tid));

   tst = VG_(get_ThreadState)(tid);

   /* This is only so that the IA is (might be) useful to report if
      something goes wrong in the sigreturn */
   ML_(fixup_guest_state_to_restart_syscall)(&tst->arch);

   /* Restore register state from frame and remove it */
   VG_(sigframe_destroy)(tid, False);

   /* Tell the driver not to update the guest state with the "result",
      and set a bogus result to keep it happy. */
   *flags |= SfNoWriteResult;
   SET_STATUS_Success(0);

   /* Check to see if any signals arose as a result of this. */
   *flags |= SfPollAfter;
}


PRE(sys_rt_sigreturn)
{
   /* See comments on PRE(sys_rt_sigreturn) in syswrap-amd64-linux.c for
      an explanation of what follows. */

   ThreadState* tst;
   PRINT("sys_rt_sigreturn ( )");

   vg_assert(VG_(is_valid_tid)(tid));
   vg_assert(tid >= 1 && tid < VG_N_THREADS);
   vg_assert(VG_(is_running_thread)(tid));

   tst = VG_(get_ThreadState)(tid);

   /* This is only so that the IA is (might be) useful to report if
      something goes wrong in the sigreturn */
   ML_(fixup_guest_state_to_restart_syscall)(&tst->arch);

   /* Restore register state from frame and remove it */
   VG_(sigframe_destroy)(tid, True);

   /* Tell the driver not to update the guest state with the "result",
      and set a bogus result to keep it happy. */
   *flags |= SfNoWriteResult;
   SET_STATUS_Success(0);

   /* Check to see if any signals arose as a result of this. */
   *flags |= SfPollAfter;
}

/* we cant use the LINX_ version for 64 bit */
PRE(sys_fadvise64)
{
   PRINT("sys_fadvise64 ( %ld, %ld, %ld, %ld )", ARG1,ARG2,ARG3,ARG4);
   PRE_REG_READ4(long, "fadvise64",
                 int, fd, vki_loff_t, offset, vki_loff_t, len, int, advice);
}

#undef PRE
#undef POST

/* ---------------------------------------------------------------------
   The s390x/Linux syscall table
   ------------------------------------------------------------------ */

/* Add an s390x-linux specific wrapper to a syscall table. */
#define PLAX_(sysno, name)    WRAPPER_ENTRY_X_(s390x_linux, sysno, name)
#define PLAXY(sysno, name)    WRAPPER_ENTRY_XY(s390x_linux, sysno, name)

// This table maps from __NR_xxx syscall numbers from
// linux/arch/s390/kernel/syscalls.S to the appropriate PRE/POST sys_foo()
// wrappers on s390x. There are several unused numbers, which are only
// defined on s390 (31bit mode) but no longer available on s390x (64 bit).
// For those syscalls not handled by Valgrind, the annotation indicate its
// arch/OS combination, eg. */* (generic), */Linux (Linux only), ?/?
// (unknown).

static SyscallTableEntry syscall_table[] = {
   GENX_(0, sys_ni_syscall), /* unimplemented (by the kernel) */      // 0
   GENX_(__NR_exit,  sys_exit),                                       // 1
   GENX_(__NR_fork,  sys_fork),                                       // 2
   GENXY(__NR_read,  sys_read),                                       // 3
   GENX_(__NR_write,  sys_write),                                     // 4

   GENXY(__NR_open,  sys_open),                                       // 5
   GENXY(__NR_close,  sys_close),                                     // 6
// ?????(__NR_restart_syscall, ),                                     // 7
   GENXY(__NR_creat,  sys_creat),                                     // 8
   GENX_(__NR_link,  sys_link),                                       // 9

   GENX_(__NR_unlink,  sys_unlink),                                   // 10
   GENX_(__NR_execve,  sys_execve),                                   // 11
   GENX_(__NR_chdir,  sys_chdir),                                     // 12
   GENX_(13, sys_ni_syscall), /* unimplemented (by the kernel) */     // 13
   GENX_(__NR_mknod,  sys_mknod),                                     // 14

   GENX_(__NR_chmod,  sys_chmod),                                     // 15
   GENX_(16, sys_ni_syscall), /* unimplemented (by the kernel) */     // 16
   GENX_(17, sys_ni_syscall), /* unimplemented (by the kernel) */     // 17
   GENX_(18, sys_ni_syscall), /* unimplemented (by the kernel) */     // 18
   LINX_(__NR_lseek,  sys_lseek),                                     // 19

   GENX_(__NR_getpid,  sys_getpid),                                   // 20
   LINX_(__NR_mount,  sys_mount),                                     // 21
   LINX_(__NR_umount, sys_oldumount),                                 // 22
   GENX_(23, sys_ni_syscall), /* unimplemented (by the kernel) */     // 23
   GENX_(24, sys_ni_syscall), /* unimplemented (by the kernel) */     // 24

   GENX_(25, sys_ni_syscall), /* unimplemented (by the kernel) */     // 25
   PLAXY(__NR_ptrace, sys_ptrace),                                    // 26
   GENX_(__NR_alarm,  sys_alarm),                                     // 27
   GENX_(28, sys_ni_syscall), /* unimplemented (by the kernel) */     // 28
   GENX_(__NR_pause,  sys_pause),                                     // 29

   LINX_(__NR_utime,  sys_utime),                                     // 30
   GENX_(31, sys_ni_syscall), /* unimplemented (by the kernel) */     // 31
   GENX_(32, sys_ni_syscall), /* unimplemented (by the kernel) */     // 32
   GENX_(__NR_access,  sys_access),                                   // 33
   GENX_(__NR_nice, sys_nice),                                        // 34

   GENX_(35, sys_ni_syscall), /* unimplemented (by the kernel) */     // 35
   GENX_(__NR_sync, sys_sync),                                        // 36
   GENX_(__NR_kill,  sys_kill),                                       // 37
   GENX_(__NR_rename,  sys_rename),                                   // 38
   GENX_(__NR_mkdir,  sys_mkdir),                                     // 39

   GENX_(__NR_rmdir, sys_rmdir),                                      // 40
   GENXY(__NR_dup,  sys_dup),                                         // 41
   LINXY(__NR_pipe,  sys_pipe),                                       // 42
   GENXY(__NR_times,  sys_times),                                     // 43
   GENX_(44, sys_ni_syscall), /* unimplemented (by the kernel) */     // 44

   GENX_(__NR_brk,  sys_brk),                                         // 45
   GENX_(46, sys_ni_syscall), /* unimplemented (by the kernel) */     // 46
   GENX_(47, sys_ni_syscall), /* unimplemented (by the kernel) */     // 47
// ?????(__NR_signal, ),                                              // 48
   GENX_(49, sys_ni_syscall), /* unimplemented (by the kernel) */     // 49

   GENX_(50, sys_ni_syscall), /* unimplemented (by the kernel) */     // 50
   GENX_(__NR_acct, sys_acct),                                        // 51
   LINX_(__NR_umount2, sys_umount),                                   // 52
   GENX_(53, sys_ni_syscall), /* unimplemented (by the kernel) */     // 53
   LINXY(__NR_ioctl,  sys_ioctl),                                     // 54

   LINXY(__NR_fcntl,  sys_fcntl),                                     // 55
   GENX_(56, sys_ni_syscall), /* unimplemented (by the kernel) */     // 56
   GENX_(__NR_setpgid,  sys_setpgid),                                 // 57
   GENX_(58, sys_ni_syscall), /* unimplemented (by the kernel) */     // 58
   GENX_(59, sys_ni_syscall), /* unimplemented (by the kernel) */     // 59

   GENX_(__NR_umask,  sys_umask),                                     // 60
   GENX_(__NR_chroot,  sys_chroot),                                   // 61
// ?????(__NR_ustat, sys_ustat), /* deprecated in favor of statfs */  // 62
   GENXY(__NR_dup2,  sys_dup2),                                       // 63
   GENX_(__NR_getppid,  sys_getppid),                                 // 64

   GENX_(__NR_getpgrp,  sys_getpgrp),                                 // 65
   GENX_(__NR_setsid,  sys_setsid),                                   // 66
// ?????(__NR_sigaction, ),   /* userspace uses rt_sigaction */       // 67
   GENX_(68, sys_ni_syscall), /* unimplemented (by the kernel) */     // 68
   GENX_(69, sys_ni_syscall), /* unimplemented (by the kernel) */     // 69

   GENX_(70, sys_ni_syscall), /* unimplemented (by the kernel) */     // 70
   GENX_(71, sys_ni_syscall), /* unimplemented (by the kernel) */     // 71
// ?????(__NR_sigsuspend, ),                                          // 72
// ?????(__NR_sigpending, ),                                          // 73
// ?????(__NR_sethostname, ),                                         // 74

   GENX_(__NR_setrlimit,  sys_setrlimit),                             // 75
   GENXY(76,  sys_getrlimit), /* see also 191 */                      // 76
   GENXY(__NR_getrusage,  sys_getrusage),                             // 77
   GENXY(__NR_gettimeofday,  sys_gettimeofday),                       // 78
   GENX_(__NR_settimeofday, sys_settimeofday),                        // 79

   GENX_(80, sys_ni_syscall), /* unimplemented (by the kernel) */     // 80
   GENX_(81, sys_ni_syscall), /* unimplemented (by the kernel) */     // 81
   GENX_(82, sys_ni_syscall), /* unimplemented (by the kernel) */     // 82
   GENX_(__NR_symlink,  sys_symlink),                                 // 83
   GENX_(84, sys_ni_syscall), /* unimplemented (by the kernel) */     // 84

   GENX_(__NR_readlink,  sys_readlink),                               // 85
// ?????(__NR_uselib, ),                                              // 86
// ?????(__NR_swapon, ),                                              // 87
// ?????(__NR_reboot, ),                                              // 88
   GENX_(89, sys_ni_syscall), /* unimplemented (by the kernel) */     // 89

   PLAX_(__NR_mmap, sys_mmap ),                                       // 90
   GENXY(__NR_munmap,  sys_munmap),                                   // 91
   GENX_(__NR_truncate,  sys_truncate),                               // 92
   GENX_(__NR_ftruncate,  sys_ftruncate),                             // 93
   GENX_(__NR_fchmod,  sys_fchmod),                                   // 94

   GENX_(95, sys_ni_syscall), /* unimplemented (by the kernel) */     // 95
   GENX_(__NR_getpriority, sys_getpriority),                          // 96
   GENX_(__NR_setpriority, sys_setpriority),                          // 97
   GENX_(98, sys_ni_syscall), /* unimplemented (by the kernel) */     // 98
   GENXY(__NR_statfs,  sys_statfs),                                   // 99

   GENXY(__NR_fstatfs,  sys_fstatfs),                                 // 100
   GENX_(101, sys_ni_syscall), /* unimplemented (by the kernel) */    // 101
   PLAXY(__NR_socketcall, sys_socketcall),                            // 102
   LINXY(__NR_syslog,  sys_syslog),                                   // 103
   GENXY(__NR_setitimer,  sys_setitimer),                             // 104

   GENXY(__NR_getitimer,  sys_getitimer),                             // 105
   GENXY(__NR_stat, sys_newstat),                                     // 106
   GENXY(__NR_lstat, sys_newlstat),                                   // 107
   GENXY(__NR_fstat, sys_newfstat),                                   // 108
   GENX_(109, sys_ni_syscall), /* unimplemented (by the kernel) */    // 109

   LINXY(__NR_lookup_dcookie, sys_lookup_dcookie),                    // 110
   LINX_(__NR_vhangup, sys_vhangup),                                  // 111
   GENX_(112, sys_ni_syscall), /* unimplemented (by the kernel) */    // 112
   GENX_(113, sys_ni_syscall), /* unimplemented (by the kernel) */    // 113
   GENXY(__NR_wait4,  sys_wait4),                                     // 114

// ?????(__NR_swapoff, ),                                             // 115
   LINXY(__NR_sysinfo,  sys_sysinfo),                                 // 116
   PLAXY(__NR_ipc, sys_ipc),                                          // 117
   GENX_(__NR_fsync,  sys_fsync),                                     // 118
   PLAX_(__NR_sigreturn, sys_sigreturn),                              // 119

   PLAX_(__NR_clone,  sys_clone),                                     // 120
// ?????(__NR_setdomainname, ),                                       // 121
   GENXY(__NR_uname, sys_newuname),                                   // 122
   GENX_(123, sys_ni_syscall), /* unimplemented (by the kernel) */    // 123
// ?????(__NR_adjtimex, ),                                            // 124

   GENXY(__NR_mprotect,  sys_mprotect),                               // 125
// LINXY(__NR_sigprocmask, sys_sigprocmask),                          // 126
   GENX_(127, sys_ni_syscall), /* unimplemented (by the kernel) */    // 127
   LINX_(__NR_init_module,  sys_init_module),                         // 128
   LINX_(__NR_delete_module,  sys_delete_module),                     // 129

   GENX_(130, sys_ni_syscall), /* unimplemented (by the kernel) */    // 130
   LINX_(__NR_quotactl, sys_quotactl),                                // 131
   GENX_(__NR_getpgid,  sys_getpgid),                                 // 132
   GENX_(__NR_fchdir,  sys_fchdir),                                   // 133
// ?????(__NR_bdflush, ),                                             // 134

// ?????(__NR_sysfs, ),                                               // 135
   LINX_(__NR_personality, sys_personality),                          // 136
   GENX_(137, sys_ni_syscall), /* unimplemented (by the kernel) */    // 137
   GENX_(138, sys_ni_syscall), /* unimplemented (by the kernel) */    // 138
   GENX_(139, sys_ni_syscall), /* unimplemented (by the kernel) */    // 139

// LINXY(__NR__llseek, sys_llseek), /* 64 bit --> lseek */            // 140
   GENXY(__NR_getdents,  sys_getdents),                               // 141
   GENX_(__NR_select, sys_select),                                    // 142
   GENX_(__NR_flock,  sys_flock),                                     // 143
   GENX_(__NR_msync,  sys_msync),                                     // 144

   GENXY(__NR_readv,  sys_readv),                                     // 145
   GENX_(__NR_writev,  sys_writev),                                   // 146
   GENX_(__NR_getsid, sys_getsid),                                    // 147
   GENX_(__NR_fdatasync,  sys_fdatasync),                             // 148
   LINXY(__NR__sysctl, sys_sysctl),                                   // 149

   GENX_(__NR_mlock,  sys_mlock),                                     // 150
   GENX_(__NR_munlock,  sys_munlock),                                 // 151
   GENX_(__NR_mlockall,  sys_mlockall),                               // 152
   LINX_(__NR_munlockall,  sys_munlockall),                           // 153
   LINXY(__NR_sched_setparam,  sys_sched_setparam),                   // 154

   LINXY(__NR_sched_getparam,  sys_sched_getparam),                   // 155
   LINX_(__NR_sched_setscheduler,  sys_sched_setscheduler),           // 156
   LINX_(__NR_sched_getscheduler,  sys_sched_getscheduler),           // 157
   LINX_(__NR_sched_yield,  sys_sched_yield),                         // 158
   LINX_(__NR_sched_get_priority_max,  sys_sched_get_priority_max),   // 159

   LINX_(__NR_sched_get_priority_min,  sys_sched_get_priority_min),   // 160
// ?????(__NR_sched_rr_get_interval, ),                               // 161
   GENXY(__NR_nanosleep,  sys_nanosleep),                             // 162
   GENX_(__NR_mremap,  sys_mremap),                                   // 163
   GENX_(164, sys_ni_syscall), /* unimplemented (by the kernel) */    // 164

   GENX_(165, sys_ni_syscall), /* unimplemented (by the kernel) */    // 165
   GENX_(166, sys_ni_syscall), /* unimplemented (by the kernel) */    // 166
   GENX_(167, sys_ni_syscall), /* unimplemented (by the kernel) */    // 167
   GENXY(__NR_poll,  sys_poll),                                       // 168
// ?????(__NR_nfsservctl, ),                                          // 169

   GENX_(170, sys_ni_syscall), /* unimplemented (by the kernel) */    // 170
   GENX_(171, sys_ni_syscall), /* unimplemented (by the kernel) */    // 171
   LINXY(__NR_prctl, sys_prctl),                                      // 172
   PLAX_(__NR_rt_sigreturn,  sys_rt_sigreturn),                       // 173
   LINXY(__NR_rt_sigaction,  sys_rt_sigaction),                       // 174

   LINXY(__NR_rt_sigprocmask,  sys_rt_sigprocmask),                   // 175
   LINXY(__NR_rt_sigpending, sys_rt_sigpending),                      // 176
   LINXY(__NR_rt_sigtimedwait,  sys_rt_sigtimedwait),                 // 177
   LINXY(__NR_rt_sigqueueinfo, sys_rt_sigqueueinfo),                  // 178
   LINX_(__NR_rt_sigsuspend, sys_rt_sigsuspend),                      // 179

   GENXY(__NR_pread64,  sys_pread64),                                 // 180
   GENX_(__NR_pwrite64, sys_pwrite64),                                // 181
   GENX_(182, sys_ni_syscall), /* unimplemented (by the kernel) */    // 182
   GENXY(__NR_getcwd,  sys_getcwd),                                   // 183
   LINXY(__NR_capget,  sys_capget),                                   // 184

   LINX_(__NR_capset,  sys_capset),                                   // 185
   GENXY(__NR_sigaltstack,  sys_sigaltstack),                         // 186
   LINXY(__NR_sendfile, sys_sendfile),                                // 187
   GENX_(188, sys_ni_syscall), /* unimplemented (by the kernel) */    // 188
   GENX_(189, sys_ni_syscall), /* unimplemented (by the kernel) */    // 189

   GENX_(__NR_vfork,  sys_fork),                                      // 190
   GENXY(__NR_getrlimit,  sys_getrlimit),                             // 191
   GENX_(192, sys_ni_syscall), /* not exported on 64bit*/             // 192
   GENX_(193, sys_ni_syscall), /* unimplemented (by the kernel) */    // 193
   GENX_(194, sys_ni_syscall), /* unimplemented (by the kernel) */    // 194

   GENX_(195, sys_ni_syscall), /* unimplemented (by the kernel) */    // 195
   GENX_(196, sys_ni_syscall), /* unimplemented (by the kernel) */    // 196
   GENX_(197, sys_ni_syscall), /* unimplemented (by the kernel) */    // 197
   GENX_(__NR_lchown, sys_lchown),                                    // 198
   GENX_(__NR_getuid, sys_getuid),                                    // 199

   GENX_(__NR_getgid, sys_getgid),                                    // 200
   GENX_(__NR_geteuid, sys_geteuid),                                  // 201
   GENX_(__NR_getegid, sys_getegid),                                  // 202
   GENX_(__NR_setreuid, sys_setreuid),                                // 203
   GENX_(__NR_setregid, sys_setregid),                                // 204

   GENXY(__NR_getgroups, sys_getgroups),                              // 205
   GENX_(__NR_setgroups, sys_setgroups),                              // 206
   GENX_(__NR_fchown, sys_fchown),                                    // 207
   LINX_(__NR_setresuid, sys_setresuid),                              // 208
   LINXY(__NR_getresuid, sys_getresuid),                              // 209

   LINX_(__NR_setresgid, sys_setresgid),                              // 210
   LINXY(__NR_getresgid, sys_getresgid),                              // 211
   GENX_(__NR_chown, sys_chown),                                      // 212
   GENX_(__NR_setuid, sys_setuid),                                    // 213
   GENX_(__NR_setgid, sys_setgid),                                    // 214

   LINX_(__NR_setfsuid, sys_setfsuid),                                // 215
   LINX_(__NR_setfsgid, sys_setfsgid),                                // 216
// ?????(__NR_pivot_root, ),
   GENXY(__NR_mincore, sys_mincore),                                  // 218
   GENX_(__NR_madvise,  sys_madvise),                                 // 219

   GENXY(__NR_getdents64,  sys_getdents64),                           // 220
   GENX_(221, sys_ni_syscall), /* unimplemented (by the kernel) */    // 221
   LINX_(__NR_readahead, sys_readahead),                              // 222
   GENX_(223, sys_ni_syscall), /* unimplemented (by the kernel) */    // 223
   LINX_(__NR_setxattr, sys_setxattr),                                // 224

   LINX_(__NR_lsetxattr, sys_lsetxattr),                              // 225
   LINX_(__NR_fsetxattr, sys_fsetxattr),                              // 226
   LINXY(__NR_getxattr,  sys_getxattr),                               // 227
   LINXY(__NR_lgetxattr,  sys_lgetxattr),                             // 228
   LINXY(__NR_fgetxattr,  sys_fgetxattr),                             // 229

   LINXY(__NR_listxattr,  sys_listxattr),                             // 230
   LINXY(__NR_llistxattr,  sys_llistxattr),                           // 231
   LINXY(__NR_flistxattr,  sys_flistxattr),                           // 232
   LINX_(__NR_removexattr,  sys_removexattr),                         // 233
   LINX_(__NR_lremovexattr,  sys_lremovexattr),                       // 234

   LINX_(__NR_fremovexattr,  sys_fremovexattr),                       // 235
   LINX_(__NR_gettid,  sys_gettid),                                   // 236
   LINXY(__NR_tkill, sys_tkill),                                      // 237
   LINXY(__NR_futex,  sys_futex),                                     // 238
   LINX_(__NR_sched_setaffinity,  sys_sched_setaffinity),             // 239

   LINXY(__NR_sched_getaffinity,  sys_sched_getaffinity),             // 240
   LINXY(__NR_tgkill, sys_tgkill),                                    // 241
   GENX_(242, sys_ni_syscall), /* unimplemented (by the kernel) */    // 242
   LINXY(__NR_io_setup, sys_io_setup),                                // 243
   LINX_(__NR_io_destroy,  sys_io_destroy),                           // 244

   LINXY(__NR_io_getevents,  sys_io_getevents),                       // 245
   LINX_(__NR_io_submit,  sys_io_submit),                             // 246
   LINXY(__NR_io_cancel,  sys_io_cancel),                             // 247
   LINX_(__NR_exit_group,  sys_exit_group),                           // 248
   LINXY(__NR_epoll_create,  sys_epoll_create),                       // 249

   LINX_(__NR_epoll_ctl,  sys_epoll_ctl),                             // 250
   LINXY(__NR_epoll_wait,  sys_epoll_wait),                           // 251
   LINX_(__NR_set_tid_address,  sys_set_tid_address),                 // 252
   PLAX_(__NR_fadvise64, sys_fadvise64),                              // 253
   LINXY(__NR_timer_create,  sys_timer_create),                       // 254

   LINXY(__NR_timer_settime,  sys_timer_settime),                     // 255
   LINXY(__NR_timer_gettime,  sys_timer_gettime),                     // 256
   LINX_(__NR_timer_getoverrun,  sys_timer_getoverrun),               // 257
   LINX_(__NR_timer_delete,  sys_timer_delete),                       // 258
   LINX_(__NR_clock_settime,  sys_clock_settime),                     // 259

   LINXY(__NR_clock_gettime,  sys_clock_gettime),                     // 260
   LINXY(__NR_clock_getres,  sys_clock_getres),                       // 261
   LINXY(__NR_clock_nanosleep,  sys_clock_nanosleep),                 // 262
   GENX_(263, sys_ni_syscall), /* unimplemented (by the kernel) */    // 263
   GENX_(264, sys_ni_syscall), /* unimplemented (by the kernel) */    // 264

   GENXY(__NR_statfs64, sys_statfs64),                                // 265
   GENXY(__NR_fstatfs64, sys_fstatfs64),                              // 266
// ?????(__NR_remap_file_pages, ),
   GENX_(268, sys_ni_syscall), /* unimplemented (by the kernel) */    // 268
   GENX_(269, sys_ni_syscall), /* unimplemented (by the kernel) */    // 269

   GENX_(270, sys_ni_syscall), /* unimplemented (by the kernel) */    // 270
   LINXY(__NR_mq_open,  sys_mq_open),                                 // 271
   LINX_(__NR_mq_unlink,  sys_mq_unlink),                             // 272
   LINX_(__NR_mq_timedsend,  sys_mq_timedsend),                       // 273
   LINXY(__NR_mq_timedreceive, sys_mq_timedreceive),                  // 274

   LINX_(__NR_mq_notify,  sys_mq_notify),                             // 275
   LINXY(__NR_mq_getsetattr,  sys_mq_getsetattr),                     // 276
// ?????(__NR_kexec_load, ),
   LINX_(__NR_add_key,  sys_add_key),                                 // 278
   LINX_(__NR_request_key,  sys_request_key),                         // 279

   LINXY(__NR_keyctl,  sys_keyctl),                                   // 280
   LINXY(__NR_waitid, sys_waitid),                                    // 281
   LINX_(__NR_ioprio_set,  sys_ioprio_set),                           // 282
   LINX_(__NR_ioprio_get,  sys_ioprio_get),                           // 283
   LINX_(__NR_inotify_init,  sys_inotify_init),                       // 284

   LINX_(__NR_inotify_add_watch,  sys_inotify_add_watch),             // 285
   LINX_(__NR_inotify_rm_watch,  sys_inotify_rm_watch),               // 286
   GENX_(287, sys_ni_syscall), /* unimplemented (by the kernel) */    // 287
   LINXY(__NR_openat,  sys_openat),                                   // 288
   LINX_(__NR_mkdirat,  sys_mkdirat),                                 // 289

   LINX_(__NR_mknodat,  sys_mknodat),                                 // 290
   LINX_(__NR_fchownat,  sys_fchownat),                               // 291
   LINX_(__NR_futimesat,  sys_futimesat),                             // 292
   LINXY(__NR_newfstatat, sys_newfstatat),                            // 293
   LINX_(__NR_unlinkat,  sys_unlinkat),                               // 294

   LINX_(__NR_renameat,  sys_renameat),                               // 295
   LINX_(__NR_linkat,  sys_linkat),                                   // 296
   LINX_(__NR_symlinkat,  sys_symlinkat),                             // 297
   LINX_(__NR_readlinkat,  sys_readlinkat),                           // 298
   LINX_(__NR_fchmodat,  sys_fchmodat),                               // 299

   LINX_(__NR_faccessat,  sys_faccessat),                             // 300
   LINX_(__NR_pselect6, sys_pselect6),                                // 301
   LINXY(__NR_ppoll, sys_ppoll),                                      // 302
// ?????(__NR_unshare, ),
   LINX_(__NR_set_robust_list,  sys_set_robust_list),                 // 304

   LINXY(__NR_get_robust_list,  sys_get_robust_list),                 // 305
// ?????(__NR_splice, ),
   LINX_(__NR_sync_file_range, sys_sync_file_range),                  // 307
// ?????(__NR_tee, ),
// ?????(__NR_vmsplice, ),

   GENX_(310, sys_ni_syscall), /* unimplemented (by the kernel) */    // 310
// ?????(__NR_getcpu, ),
   LINXY(__NR_epoll_pwait,  sys_epoll_pwait),                         // 312
   GENX_(__NR_utimes, sys_utimes),                                    // 313
   LINX_(__NR_fallocate, sys_fallocate),                              // 314

   LINX_(__NR_utimensat,  sys_utimensat),                             // 315
   LINXY(__NR_signalfd,  sys_signalfd),                               // 316
   GENX_(317, sys_ni_syscall), /* unimplemented (by the kernel) */    // 317
   LINX_(__NR_eventfd,  sys_eventfd),                                 // 318
   LINXY(__NR_timerfd_create,  sys_timerfd_create),                   // 319

   LINXY(__NR_timerfd_settime,  sys_timerfd_settime),                 // 320
   LINXY(__NR_timerfd_gettime,  sys_timerfd_gettime),                 // 321
   LINXY(__NR_signalfd4,  sys_signalfd4),                             // 322
   LINX_(__NR_eventfd2,  sys_eventfd2),                               // 323
   LINXY(__NR_inotify_init1,  sys_inotify_init1),                     // 324

   LINXY(__NR_pipe2,  sys_pipe2),                                     // 325
   // (__NR_dup3,  ),
   LINXY(__NR_epoll_create1,  sys_epoll_create1),                     // 327
   LINXY(__NR_preadv, sys_preadv),                                    // 328
   LINX_(__NR_pwritev, sys_pwritev),                                  // 329

// ?????(__NR_rt_tgsigqueueinfo, ),
   LINXY(__NR_perf_event_open, sys_perf_event_open),                  // 331
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

#endif

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
