
/*--------------------------------------------------------------------*/
/*--- Platform-specific syscalls stuff.      syswrap-ppc32-linux.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2005-2012 Nicholas Nethercote <njn@valgrind.org>
   Copyright (C) 2005-2012 Cerion Armour-Brown <cerion@open-works.co.uk>

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

#if defined(VGP_ppc32_linux)

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
#include "pub_core_sigframe.h"      // For VG_(sigframe_destroy)()
#include "pub_core_signals.h"
#include "pub_core_syscall.h"
#include "pub_core_syswrap.h"
#include "pub_core_tooliface.h"
#include "pub_core_stacks.h"        // VG_(register_stack)

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
                                  void (*f)(Word),
                                  Word arg1 );
//    r3 = stack
//    r4 = retaddr
//    r5 = f
//    r6 = arg1
asm(
".text\n"
".globl vgModuleLocal_call_on_new_stack_0_1\n"
"vgModuleLocal_call_on_new_stack_0_1:\n"
"   mr    %r1,%r3\n\t"     // stack to %sp
"   mtlr  %r4\n\t"         // retaddr to %lr
"   mtctr %r5\n\t"         // f to count reg
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
".previous\n"
);


/*
        Perform a clone system call.  clone is strange because it has
        fork()-like return-twice semantics, so it needs special
        handling here.

        Upon entry, we have:

            int (fn)(void*)     in r3
            void* child_stack   in r4
            int flags           in r5
            void* arg           in r6
            pid_t* child_tid    in r7
            pid_t* parent_tid   in r8
            void* ???           in r9

        System call requires:

            int    $__NR_clone  in r0  (sc number)
            int    flags        in r3  (sc arg1)
            void*  child_stack  in r4  (sc arg2)
            pid_t* parent_tid   in r5  (sc arg3)
            ??     child_tls    in r6  (sc arg4)
            pid_t* child_tid    in r7  (sc arg5)
            void*  ???          in r8  (sc arg6)

        Returns an Int encoded in the linux-ppc32 way, not a SysRes.
 */
#define __NR_CLONE        VG_STRINGIFY(__NR_clone)
#define __NR_EXIT         VG_STRINGIFY(__NR_exit)

extern
ULong do_syscall_clone_ppc32_linux ( Word (*fn)(void *), 
                                     void* stack, 
                                     Int   flags, 
                                     void* arg,
                                     Int*  child_tid, 
                                     Int*  parent_tid, 
                                     vki_modify_ldt_t * );
asm(
".text\n"
".globl do_syscall_clone_ppc32_linux\n"
"do_syscall_clone_ppc32_linux:\n"
"       stwu    1,-32(1)\n"
"       stw     29,20(1)\n"
"       stw     30,24(1)\n"
"       stw     31,28(1)\n"
"       mr      30,3\n"              // preserve fn
"       mr      31,6\n"              // preserve arg

        // setup child stack
"       rlwinm  4,4,0,~0xf\n"        // trim sp to multiple of 16 bytes
"       li      0,0\n"
"       stwu    0,-16(4)\n"          // make initial stack frame
"       mr      29,4\n"              // preserve sp

        // setup syscall
"       li      0,"__NR_CLONE"\n"    // syscall number
"       mr      3,5\n"               // syscall arg1: flags
        // r4 already setup          // syscall arg2: child_stack
"       mr      5,8\n"               // syscall arg3: parent_tid
"       mr      6,2\n"               // syscall arg4: REAL THREAD tls
"       mr      7,7\n"               // syscall arg5: child_tid
"       mr      8,8\n"               // syscall arg6: ????
"       mr      9,9\n"               // syscall arg7: ????

"       sc\n"                        // clone()

"       mfcr    4\n"                 // return CR in r4 (low word of ULong)
"       cmpwi   3,0\n"               // child if retval == 0
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
"1:     lwz     29,20(1)\n"
"       lwz     30,24(1)\n"
"       lwz     31,28(1)\n"
"       addi    1,1,32\n"
"       blr\n"
".previous\n"
);

#undef __NR_CLONE
#undef __NR_EXIT

// forward declarations
static void setup_child ( ThreadArchState*, ThreadArchState* );

/* 
   When a client clones, we need to keep track of the new thread.  This means:
   1. allocate a ThreadId+ThreadState+stack for the the thread

   2. initialize the thread's new VCPU state

   3. create the thread using the same args as the client requested,
   but using the scheduler entrypoint for IP, and a separate stack
   for SP.
 */
static SysRes do_clone ( ThreadId ptid, 
                         UInt flags, Addr sp, 
                         Int *parent_tidptr, 
                         Int *child_tidptr, 
                         Addr child_tls)
{
   const Bool debug = False;

   ThreadId     ctid = VG_(alloc_ThreadState)();
   ThreadState* ptst = VG_(get_ThreadState)(ptid);
   ThreadState* ctst = VG_(get_ThreadState)(ctid);
   ULong        word64;
   UWord*       stack;
   NSegment const* seg;
   SysRes       res;
   vki_sigset_t blockall, savedmask;

   VG_(sigfillset)(&blockall);

   vg_assert(VG_(is_running_thread)(ptid));
   vg_assert(VG_(is_valid_tid)(ctid));

   stack = (UWord*)ML_(allocstack)(ctid);
   if (stack == NULL) {
      res = VG_(mk_SysRes_Error)( VKI_ENOMEM );
      goto out;
   }

//?   /* make a stack frame */
//?   stack -= 16;
//?   *(UWord *)stack = 0;


   /* Copy register state

      Both parent and child return to the same place, and the code
      following the clone syscall works out which is which, so we
      don't need to worry about it.

      The parent gets the child's new tid returned from clone, but the
      child gets 0.

      If the clone call specifies a NULL SP for the new thread, then
      it actually gets a copy of the parent's SP.

      The child's TLS register (r2) gets set to the tlsaddr argument
      if the CLONE_SETTLS flag is set.
   */
   setup_child( &ctst->arch, &ptst->arch );

   /* Make sys_clone appear to have returned Success(0) in the
      child. */
   { UInt old_cr = LibVEX_GuestPPC32_get_CR( &ctst->arch.vex );
     /* %r3 = 0 */
     ctst->arch.vex.guest_GPR3 = 0;
     /* %cr0.so = 0 */
     LibVEX_GuestPPC32_put_CR( old_cr & ~(1<<28), &ctst->arch.vex );
   }

   if (sp != 0)
      ctst->arch.vex.guest_GPR1 = sp;

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

   /* We don't really know where the client stack is, because its
      allocated by the client.  The best we can do is look at the
      memory mappings and try to derive some useful information.  We
      assume that esp starts near its highest possible value, and can
      only go down to the start of the mmaped segment. */
   seg = VG_(am_find_nsegment)(sp);
   if (seg && seg->kind != SkResvn) {
      ctst->client_stack_highest_word = (Addr)VG_PGROUNDUP(sp);
      ctst->client_stack_szB = ctst->client_stack_highest_word - seg->start;

      VG_(register_stack)(seg->start, ctst->client_stack_highest_word);

      if (debug)
	 VG_(printf)("\ntid %d: guessed client stack range %#lx-%#lx\n",
		     ctid, seg->start, VG_PGROUNDUP(sp));
   } else {
      VG_(message)(Vg_UserMsg,
                   "!? New thread %d starts with R1(%#lx) unmapped\n",
		   ctid, sp);
      ctst->client_stack_szB  = 0;
   }

   /* Assume the clone will succeed, and tell any tool that wants to
      know that this thread has come into existence.  If the clone
      fails, we'll send out a ll_exit notification for it at the out:
      label below, to clean up. */
   vg_assert(VG_(owns_BigLock_LL)(ptid));
   VG_TRACK ( pre_thread_ll_create, ptid, ctid );

   if (flags & VKI_CLONE_SETTLS) {
      if (debug)
         VG_(printf)("clone child has SETTLS: tls at %#lx\n", child_tls);
      ctst->arch.vex.guest_GPR2 = child_tls;
   }

   flags &= ~VKI_CLONE_SETTLS;

   /* start the thread with everything blocked */
   VG_(sigprocmask)(VKI_SIG_SETMASK, &blockall, &savedmask);

   /* Create the new thread */
   word64 = do_syscall_clone_ppc32_linux(
               ML_(start_thread_NORETURN), stack, flags, &VG_(threads)[ctid],
               child_tidptr, parent_tidptr, NULL
            );
   /* High half word64 is syscall return value.  Low half is
      the entire CR, from which we need to extract CR0.SO. */
   /* VG_(printf)("word64 = 0x%llx\n", word64); */
   res = VG_(mk_SysRes_ppc32_linux)( 
            /*val*/(UInt)(word64 >> 32), 
            /*errflag*/ (((UInt)word64) >> 28) & 1 
         );

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

void VG_(cleanup_thread) ( ThreadArchState* arch )
{
}  

void setup_child ( /*OUT*/ ThreadArchState *child,
                   /*IN*/  ThreadArchState *parent )
{
   /* We inherit our parent's guest state. */
   child->vex = parent->vex;
   child->vex_shadow1 = parent->vex_shadow1;
   child->vex_shadow2 = parent->vex_shadow2;
}


/* ---------------------------------------------------------------------
   PRE/POST wrappers for ppc32/Linux-specific syscalls
   ------------------------------------------------------------------ */

#define PRE(name)       DEFN_PRE_TEMPLATE(ppc32_linux, name)
#define POST(name)      DEFN_POST_TEMPLATE(ppc32_linux, name)

/* Add prototypes for the wrappers declared here, so that gcc doesn't
   harass us for not having prototypes.  Really this is a kludge --
   the right thing to do is to make these wrappers 'static' since they
   aren't visible outside this file, but that requires even more macro
   magic. */

DECL_TEMPLATE(ppc32_linux, sys_socketcall);
DECL_TEMPLATE(ppc32_linux, sys_mmap);
DECL_TEMPLATE(ppc32_linux, sys_mmap2);
DECL_TEMPLATE(ppc32_linux, sys_stat64);
DECL_TEMPLATE(ppc32_linux, sys_lstat64);
DECL_TEMPLATE(ppc32_linux, sys_fstatat64);
DECL_TEMPLATE(ppc32_linux, sys_fstat64);
DECL_TEMPLATE(ppc32_linux, sys_ipc);
DECL_TEMPLATE(ppc32_linux, sys_clone);
DECL_TEMPLATE(ppc32_linux, sys_sigreturn);
DECL_TEMPLATE(ppc32_linux, sys_rt_sigreturn);
DECL_TEMPLATE(ppc32_linux, sys_sigsuspend);
DECL_TEMPLATE(ppc32_linux, sys_spu_create);
DECL_TEMPLATE(ppc32_linux, sys_spu_run);

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
      ML_(generic_PRE_sys_socketpair)( tid, ARG2_0, ARG2_1, ARG2_2, ARG2_3 );
      break;

   case VKI_SYS_SOCKET:
     /* int socket(int domain, int type, int protocol); */
      PRE_MEM_READ( "socketcall.socket(args)", ARG2, 3*sizeof(Addr) );
      break;

   case VKI_SYS_BIND:
     /* int bind(int sockfd, struct sockaddr *my_addr,
	int addrlen); */
      PRE_MEM_READ( "socketcall.bind(args)", ARG2, 3*sizeof(Addr) );
      ML_(generic_PRE_sys_bind)( tid, ARG2_0, ARG2_1, ARG2_2 );
      break;

   case VKI_SYS_LISTEN:
     /* int listen(int s, int backlog); */
      PRE_MEM_READ( "socketcall.listen(args)", ARG2, 2*sizeof(Addr) );
      break;

   case VKI_SYS_ACCEPT: {
     /* int accept(int s, struct sockaddr *addr, int *addrlen); */
      PRE_MEM_READ( "socketcall.accept(args)", ARG2, 3*sizeof(Addr) );
      ML_(generic_PRE_sys_accept)( tid, ARG2_0, ARG2_1, ARG2_2 );
      break;
   }

   case VKI_SYS_ACCEPT4: {
     /* int accept(int s, struct sockaddr *addr, int *addrlen, int args); */
      PRE_MEM_READ( "socketcall.accept4(args)", ARG2, 4*sizeof(Addr) );
      ML_(generic_PRE_sys_accept)( tid, ARG2_0, ARG2_1, ARG2_2 );
      break;
   }

   case VKI_SYS_SENDTO:
     /* int sendto(int s, const void *msg, int len,
                    unsigned int flags,
                    const struct sockaddr *to, int tolen); */
     PRE_MEM_READ( "socketcall.sendto(args)", ARG2, 6*sizeof(Addr) );
     ML_(generic_PRE_sys_sendto)( tid, ARG2_0, ARG2_1, ARG2_2,
				  ARG2_3, ARG2_4, ARG2_5 );
     break;

   case VKI_SYS_SEND:
     /* int send(int s, const void *msg, size_t len, int flags); */
     PRE_MEM_READ( "socketcall.send(args)", ARG2, 4*sizeof(Addr) );
     ML_(generic_PRE_sys_send)( tid, ARG2_0, ARG2_1, ARG2_2 );
     break;

   case VKI_SYS_RECVFROM:
     /* int recvfrom(int s, void *buf, int len, unsigned int flags,
	struct sockaddr *from, int *fromlen); */
     PRE_MEM_READ( "socketcall.recvfrom(args)", ARG2, 6*sizeof(Addr) );
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
     ML_(generic_PRE_sys_recv)( tid, ARG2_0, ARG2_1, ARG2_2 );
     break;

   case VKI_SYS_CONNECT:
     /* int connect(int sockfd,
	struct sockaddr *serv_addr, int addrlen ); */
     PRE_MEM_READ( "socketcall.connect(args)", ARG2, 3*sizeof(Addr) );
     ML_(generic_PRE_sys_connect)( tid, ARG2_0, ARG2_1, ARG2_2 );
     break;

   case VKI_SYS_SETSOCKOPT:
     /* int setsockopt(int s, int level, int optname,
	const void *optval, int optlen); */
     PRE_MEM_READ( "socketcall.setsockopt(args)", ARG2, 5*sizeof(Addr) );
     ML_(generic_PRE_sys_setsockopt)( tid, ARG2_0, ARG2_1, ARG2_2,
				      ARG2_3, ARG2_4 );
     break;

   case VKI_SYS_GETSOCKOPT:
     /* int getsockopt(int s, int level, int optname,
	void *optval, socklen_t *optlen); */
     PRE_MEM_READ( "socketcall.getsockopt(args)", ARG2, 5*sizeof(Addr) );
     ML_(linux_PRE_sys_getsockopt)( tid, ARG2_0, ARG2_1, ARG2_2,
				    ARG2_3, ARG2_4 );
     break;

   case VKI_SYS_GETSOCKNAME:
     /* int getsockname(int s, struct sockaddr* name, int* namelen) */
     PRE_MEM_READ( "socketcall.getsockname(args)", ARG2, 3*sizeof(Addr) );
     ML_(generic_PRE_sys_getsockname)( tid, ARG2_0, ARG2_1, ARG2_2 );
     break;

   case VKI_SYS_GETPEERNAME:
     /* int getpeername(int s, struct sockaddr* name, int* namelen) */
     PRE_MEM_READ( "socketcall.getpeername(args)", ARG2, 3*sizeof(Addr) );
     ML_(generic_PRE_sys_getpeername)( tid, ARG2_0, ARG2_1, ARG2_2 );
     break;

   case VKI_SYS_SHUTDOWN:
     /* int shutdown(int s, int how); */
     PRE_MEM_READ( "socketcall.shutdown(args)", ARG2, 2*sizeof(Addr) );
     break;

   case VKI_SYS_SENDMSG: {
     /* int sendmsg(int s, const struct msghdr *msg, int flags); */

     /* this causes warnings, and I don't get why. glibc bug?
      * (after all it's glibc providing the arguments array)
       PRE_MEM_READ( "socketcall.sendmsg(args)", ARG2, 3*sizeof(Addr) );
     */
     ML_(generic_PRE_sys_sendmsg)( tid, "msg", (struct vki_msghdr *)ARG2_1 );
     break;
   }

   case VKI_SYS_RECVMSG: {
     /* int recvmsg(int s, struct msghdr *msg, int flags); */

     /* this causes warnings, and I don't get why. glibc bug?
      * (after all it's glibc providing the arguments array)
       PRE_MEM_READ("socketcall.recvmsg(args)", ARG2, 3*sizeof(Addr) );
     */
     ML_(generic_PRE_sys_recvmsg)( tid, "msg", (struct vki_msghdr *)ARG2_1 );
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
  case VKI_SYS_ACCEPT4:
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
    ML_(generic_POST_sys_recvmsg)( tid, "msg", (struct vki_msghdr *)ARG2_1, RES );
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
   SysRes r;

   PRINT("sys_mmap ( %#lx, %llu, %ld, %ld, %ld, %ld )",
         ARG1, (ULong)ARG2, ARG3, ARG4, ARG5, ARG6 );
   PRE_REG_READ6(long, "mmap",
                 unsigned long, start, unsigned long, length,
                 unsigned long, prot,  unsigned long, flags,
                 unsigned long, fd,    unsigned long, offset);

   r = ML_(generic_PRE_sys_mmap)( tid, ARG1, ARG2, ARG3, ARG4, ARG5, 
                                       (Off64T)ARG6 );
   SET_STATUS_from_SysRes(r);
}

PRE(sys_mmap2)
{
   SysRes r;

   // Exactly like old_mmap() except:
   //  - the file offset is specified in 4K units rather than bytes,
   //    so that it can be used for files bigger than 2^32 bytes.
   PRINT("sys_mmap2 ( %#lx, %llu, %ld, %ld, %ld, %ld )",
         ARG1, (ULong)ARG2, ARG3, ARG4, ARG5, ARG6 );
   PRE_REG_READ6(long, "mmap2",
                 unsigned long, start, unsigned long, length,
                 unsigned long, prot,  unsigned long, flags,
                 unsigned long, fd,    unsigned long, offset);

   r = ML_(generic_PRE_sys_mmap)( tid, ARG1, ARG2, ARG3, ARG4, ARG5, 
                                       4096 * (Off64T)ARG6 );
   SET_STATUS_from_SysRes(r);
}

// XXX: lstat64/fstat64/stat64 are generic, but not necessarily
// applicable to every architecture -- I think only to 32-bit archs.
// We're going to need something like linux/core_os32.h for such
// things, eventually, I think.  --njn
PRE(sys_stat64)
{
   PRINT("sys_stat64 ( %#lx, %#lx )",ARG1,ARG2);
   PRE_REG_READ2(long, "stat64", char *, file_name, struct stat64 *, buf);
   PRE_MEM_RASCIIZ( "stat64(file_name)", ARG1 );
   PRE_MEM_WRITE( "stat64(buf)", ARG2, sizeof(struct vki_stat64) );
}

POST(sys_stat64)
{
   POST_MEM_WRITE( ARG2, sizeof(struct vki_stat64) );
}

PRE(sys_lstat64)
{
   PRINT("sys_lstat64 ( %#lx(%s), %#lx )",ARG1,(char*)ARG1,ARG2);
   PRE_REG_READ2(long, "lstat64", char *, file_name, struct stat64 *, buf);
   PRE_MEM_RASCIIZ( "lstat64(file_name)", ARG1 );
   PRE_MEM_WRITE( "lstat64(buf)", ARG2, sizeof(struct vki_stat64) );
}

POST(sys_lstat64)
{
   vg_assert(SUCCESS);
   if (RES == 0) {
      POST_MEM_WRITE( ARG2, sizeof(struct vki_stat64) );
   }
}

PRE(sys_fstatat64)
{
  PRINT("sys_fstatat64 ( %ld, %#lx(%s), %#lx )",ARG1,ARG2,(char*)ARG2,ARG3);
   PRE_REG_READ3(long, "fstatat64",
                 int, dfd, char *, file_name, struct stat64 *, buf);
   PRE_MEM_RASCIIZ( "fstatat64(file_name)", ARG2 );
   PRE_MEM_WRITE( "fstatat64(buf)", ARG3, sizeof(struct vki_stat64) );
}

POST(sys_fstatat64)
{
   POST_MEM_WRITE( ARG3, sizeof(struct vki_stat64) );
}

PRE(sys_fstat64)
{
  PRINT("sys_fstat64 ( %ld, %#lx )",ARG1,ARG2);
  PRE_REG_READ2(long, "fstat64", unsigned long, fd, struct stat64 *, buf);
  PRE_MEM_WRITE( "fstat64(buf)", ARG2, sizeof(struct vki_stat64) );
}

POST(sys_fstat64)
{
  POST_MEM_WRITE( ARG2, sizeof(struct vki_stat64) );
}

static Addr deref_Addr ( ThreadId tid, Addr a, Char* s )
{
   Addr* a_p = (Addr*)a;
   PRE_MEM_READ( s, (Addr)a_p, sizeof(Addr) );
   return *a_p;
}

PRE(sys_ipc)
{
  PRINT("sys_ipc ( %ld, %ld, %ld, %ld, %#lx, %ld )", ARG1,ARG2,ARG3,ARG4,ARG5,ARG6);
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
      VG_(message)(Vg_DebugMsg, "FATAL: unhandled syscall(ipc) %ld\n", ARG1 );
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
		 "FATAL: unhandled syscall(ipc) %ld\n",
		 ARG1 );
    VG_(core_panic)("... bye!\n");
    break; /*NOTREACHED*/
  }
}




//.. PRE(old_select, MayBlock)
//.. {
//..    /* struct sel_arg_struct {
//..       unsigned long n;
//..       fd_set *inp, *outp, *exp;
//..       struct timeval *tvp;
//..       };
//..    */
//..    PRE_REG_READ1(long, "old_select", struct sel_arg_struct *, args);
//..    PRE_MEM_READ( "old_select(args)", ARG1, 5*sizeof(UWord) );
//.. 
//..    {
//..       UInt* arg_struct = (UInt*)ARG1;
//..       UInt a1, a2, a3, a4, a5;
//.. 
//..       a1 = arg_struct[0];
//..       a2 = arg_struct[1];
//..       a3 = arg_struct[2];
//..       a4 = arg_struct[3];
//..       a5 = arg_struct[4];
//.. 
//..       PRINT("old_select ( %d, %p, %p, %p, %p )", a1,a2,a3,a4,a5);
//..       if (a2 != (Addr)NULL)
//.. 	 PRE_MEM_READ( "old_select(readfds)",   a2, a1/8 /* __FD_SETSIZE/8 */ );
//..       if (a3 != (Addr)NULL)
//.. 	 PRE_MEM_READ( "old_select(writefds)",  a3, a1/8 /* __FD_SETSIZE/8 */ );
//..       if (a4 != (Addr)NULL)
//.. 	 PRE_MEM_READ( "old_select(exceptfds)", a4, a1/8 /* __FD_SETSIZE/8 */ );
//..       if (a5 != (Addr)NULL)
//.. 	 PRE_MEM_READ( "old_select(timeout)", a5, sizeof(struct vki_timeval) );
//..    }
//.. }

PRE(sys_clone)
{
   UInt cloneflags;

   PRINT("sys_clone ( %lx, %#lx, %#lx, %#lx, %#lx )",ARG1,ARG2,ARG3,ARG4,ARG5);
   PRE_REG_READ5(int, "clone",
                 unsigned long, flags,
                 void *,        child_stack,
                 int *,         parent_tidptr,
                 void *,        child_tls,
                 int *,         child_tidptr);

   if (ARG1 & VKI_CLONE_PARENT_SETTID) {
      PRE_MEM_WRITE("clone(parent_tidptr)", ARG3, sizeof(Int));
      if (!VG_(am_is_valid_for_client)(ARG3, sizeof(Int), 
                                             VKI_PROT_WRITE)) {
         SET_STATUS_Failure( VKI_EFAULT );
         return;
      }
   }
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
                  (Int *)ARG3,  /* parent_tidptr */
                  (Int *)ARG5,  /* child_tidptr */
                  (Addr)ARG4)); /* child_tls */
      break;

   case VKI_CLONE_VFORK | VKI_CLONE_VM: /* vfork */
      /* FALLTHROUGH - assume vfork == fork */
      cloneflags &= ~(VKI_CLONE_VFORK | VKI_CLONE_VM);

   case 0: /* plain fork */
      SET_STATUS_from_SysRes(
         ML_(do_fork_clone)(tid,
                       cloneflags,      /* flags */
                       (Int *)ARG3,     /* parent_tidptr */
                       (Int *)ARG5));   /* child_tidptr */
      break;

   default:
      /* should we just ENOSYS? */
      VG_(message)(Vg_UserMsg, "Unsupported clone() flags: 0x%lx\n", ARG1);
      VG_(message)(Vg_UserMsg, "\n");
      VG_(message)(Vg_UserMsg, "The only supported clone() uses are:\n");
      VG_(message)(Vg_UserMsg, " - via a threads library (LinuxThreads or NPTL)\n");
      VG_(message)(Vg_UserMsg, " - via the implementation of fork or vfork\n");
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

PRE(sys_sigreturn)
{
   /* See comments on PRE(sys_rt_sigreturn) in syswrap-amd64-linux.c for
      an explanation of what follows. */

   //ThreadState* tst;
   PRINT("sys_sigreturn ( )");

   vg_assert(VG_(is_valid_tid)(tid));
   vg_assert(tid >= 1 && tid < VG_N_THREADS);
   vg_assert(VG_(is_running_thread)(tid));

   ///* Adjust esp to point to start of frame; skip back up over
   //   sigreturn sequence's "popl %eax" and handler ret addr */
   //tst = VG_(get_ThreadState)(tid);
   //tst->arch.vex.guest_ESP -= sizeof(Addr)+sizeof(Word);
   // Should we do something equivalent on ppc32?  Who knows.

   ///* This is only so that the EIP is (might be) useful to report if
   //   something goes wrong in the sigreturn */
   //ML_(fixup_guest_state_to_restart_syscall)(&tst->arch);
   // Should we do something equivalent on ppc32?  Who knows.

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

   //ThreadState* tst;
   PRINT("rt_sigreturn ( )");

   vg_assert(VG_(is_valid_tid)(tid));
   vg_assert(tid >= 1 && tid < VG_N_THREADS);
   vg_assert(VG_(is_running_thread)(tid));

   ///* Adjust esp to point to start of frame; skip back up over handler
   //   ret addr */
   //tst = VG_(get_ThreadState)(tid);
   //tst->arch.vex.guest_ESP -= sizeof(Addr);
   // Should we do something equivalent on ppc32?  Who knows.

   ///* This is only so that the EIP is (might be) useful to report if
   //   something goes wrong in the sigreturn */
   //ML_(fixup_guest_state_to_restart_syscall)(&tst->arch);
   // Should we do something equivalent on ppc32?  Who knows.

   /* Restore register state from frame and remove it */
   VG_(sigframe_destroy)(tid, True);

   /* Tell the driver not to update the guest state with the "result",
      and set a bogus result to keep it happy. */
   *flags |= SfNoWriteResult;
   SET_STATUS_Success(0);

   /* Check to see if any signals arose as a result of this. */
   *flags |= SfPollAfter;
}


//.. PRE(sys_modify_ldt, Special)
//.. {
//..    PRINT("sys_modify_ldt ( %d, %p, %d )", ARG1,ARG2,ARG3);
//..    PRE_REG_READ3(int, "modify_ldt", int, func, void *, ptr,
//..                  unsigned long, bytecount);
//..    
//..    if (ARG1 == 0) {
//..       /* read the LDT into ptr */
//..       PRE_MEM_WRITE( "modify_ldt(ptr)", ARG2, ARG3 );
//..    }
//..    if (ARG1 == 1 || ARG1 == 0x11) {
//..       /* write the LDT with the entry pointed at by ptr */
//..       PRE_MEM_READ( "modify_ldt(ptr)", ARG2, sizeof(vki_modify_ldt_t) );
//..    }
//..    /* "do" the syscall ourselves; the kernel never sees it */
//..    SET_RESULT( VG_(sys_modify_ldt)( tid, ARG1, (void*)ARG2, ARG3 ) );
//.. 
//..    if (ARG1 == 0 && !VG_(is_kerror)(RES) && RES > 0) {
//..       POST_MEM_WRITE( ARG2, RES );
//..    }
//.. }

//.. PRE(sys_set_thread_area, Special)
//.. {
//..    PRINT("sys_set_thread_area ( %p )", ARG1);
//..    PRE_REG_READ1(int, "set_thread_area", struct user_desc *, u_info)
//..    PRE_MEM_READ( "set_thread_area(u_info)", ARG1, sizeof(vki_modify_ldt_t) );
//.. 
//..    /* "do" the syscall ourselves; the kernel never sees it */
//..    SET_RESULT( VG_(sys_set_thread_area)( tid, (void *)ARG1 ) );
//.. }

//.. PRE(sys_get_thread_area, Special)
//.. {
//..    PRINT("sys_get_thread_area ( %p )", ARG1);
//..    PRE_REG_READ1(int, "get_thread_area", struct user_desc *, u_info)
//..    PRE_MEM_WRITE( "get_thread_area(u_info)", ARG1, sizeof(vki_modify_ldt_t) );
//.. 
//..    /* "do" the syscall ourselves; the kernel never sees it */
//..    SET_RESULT( VG_(sys_get_thread_area)( tid, (void *)ARG1 ) );
//.. 
//..    if (!VG_(is_kerror)(RES)) {
//..       POST_MEM_WRITE( ARG1, sizeof(vki_modify_ldt_t) );
//..    }
//.. }

//.. // Parts of this are ppc32-specific, but the *PEEK* cases are generic.
//.. // XXX: Why is the memory pointed to by ARG3 never checked?
//.. PRE(sys_ptrace, 0)
//.. {
//..    PRINT("sys_ptrace ( %d, %d, %p, %p )", ARG1,ARG2,ARG3,ARG4);
//..    PRE_REG_READ4(int, "ptrace", 
//..                  long, request, long, pid, long, addr, long, data);
//..    switch (ARG1) {
//..    case VKI_PTRACE_PEEKTEXT:
//..    case VKI_PTRACE_PEEKDATA:
//..    case VKI_PTRACE_PEEKUSR:
//..       PRE_MEM_WRITE( "ptrace(peek)", ARG4, 
//.. 		     sizeof (long));
//..       break;
//..    case VKI_PTRACE_GETREGS:
//..       PRE_MEM_WRITE( "ptrace(getregs)", ARG4, 
//.. 		     sizeof (struct vki_user_regs_struct));
//..       break;
//..    case VKI_PTRACE_GETFPREGS:
//..       PRE_MEM_WRITE( "ptrace(getfpregs)", ARG4, 
//.. 		     sizeof (struct vki_user_i387_struct));
//..       break;
//..    case VKI_PTRACE_GETFPXREGS:
//..       PRE_MEM_WRITE( "ptrace(getfpxregs)", ARG4, 
//..                      sizeof(struct vki_user_fxsr_struct) );
//..       break;
//..    case VKI_PTRACE_SETREGS:
//..       PRE_MEM_READ( "ptrace(setregs)", ARG4, 
//.. 		     sizeof (struct vki_user_regs_struct));
//..       break;
//..    case VKI_PTRACE_SETFPREGS:
//..       PRE_MEM_READ( "ptrace(setfpregs)", ARG4, 
//.. 		     sizeof (struct vki_user_i387_struct));
//..       break;
//..    case VKI_PTRACE_SETFPXREGS:
//..       PRE_MEM_READ( "ptrace(setfpxregs)", ARG4, 
//..                      sizeof(struct vki_user_fxsr_struct) );
//..       break;
//..    default:
//..       break;
//..    }
//.. }

//.. POST(sys_ptrace)
//.. {
//..    switch (ARG1) {
//..    case VKI_PTRACE_PEEKTEXT:
//..    case VKI_PTRACE_PEEKDATA:
//..    case VKI_PTRACE_PEEKUSR:
//..       POST_MEM_WRITE( ARG4, sizeof (long));
//..       break;
//..    case VKI_PTRACE_GETREGS:
//..       POST_MEM_WRITE( ARG4, sizeof (struct vki_user_regs_struct));
//..       break;
//..    case VKI_PTRACE_GETFPREGS:
//..       POST_MEM_WRITE( ARG4, sizeof (struct vki_user_i387_struct));
//..       break;
//..    case VKI_PTRACE_GETFPXREGS:
//..       POST_MEM_WRITE( ARG4, sizeof(struct vki_user_fxsr_struct) );
//..       break;
//..    default:
//..       break;
//..    }
//.. }

//.. // XXX: this duplicates a function in coregrind/vg_syscalls.c, yuk
//.. static Addr deref_Addr ( ThreadId tid, Addr a, Char* s )
//.. {
//..    Addr* a_p = (Addr*)a;
//..    PRE_MEM_READ( s, (Addr)a_p, sizeof(Addr) );
//..    return *a_p;
//.. }

//.. // XXX: should use the constants here (eg. SHMAT), not the numbers directly!
//.. PRE(sys_ipc, 0)
//.. {
//..    PRINT("sys_ipc ( %d, %d, %d, %d, %p, %d )", ARG1,ARG2,ARG3,ARG4,ARG5,ARG6);
//..    // XXX: this is simplistic -- some args are not used in all circumstances.
//..    PRE_REG_READ6(int, "ipc",
//..                  vki_uint, call, int, first, int, second, int, third,
//..                  void *, ptr, long, fifth)
//.. 
//..    switch (ARG1 /* call */) {
//..    case VKI_SEMOP:
//..       ML_(generic_PRE_sys_semop)( tid, ARG2, ARG5, ARG3 );
//..       /* tst->sys_flags |= MayBlock; */
//..       break;
//..    case VKI_SEMGET:
//..       break;
//..    case VKI_SEMCTL:
//..    {
//..       UWord arg = deref_Addr( tid, ARG5, "semctl(arg)" );
//..       ML_(generic_PRE_sys_semctl)( tid, ARG2, ARG3, ARG4, arg );
//..       break;
//..    }
//..    case VKI_SEMTIMEDOP:
//..       ML_(generic_PRE_sys_semtimedop)( tid, ARG2, ARG5, ARG3, ARG6 );
//..       /* tst->sys_flags |= MayBlock; */
//..       break;
//..    case VKI_MSGSND:
//..       ML_(linux_PRE_sys_msgsnd)( tid, ARG2, ARG5, ARG3, ARG4 );
//..       /* if ((ARG4 & VKI_IPC_NOWAIT) == 0)
//..             tst->sys_flags |= MayBlock;
//..       */
//..       break;
//..    case VKI_MSGRCV:
//..    {
//..       Addr msgp;
//..       Word msgtyp;
//..  
//..       msgp = deref_Addr( tid,
//.. 			 (Addr) (&((struct vki_ipc_kludge *)ARG5)->msgp),
//.. 			 "msgrcv(msgp)" );
//..       msgtyp = deref_Addr( tid,
//.. 			   (Addr) (&((struct vki_ipc_kludge *)ARG5)->msgtyp),
//.. 			   "msgrcv(msgp)" );
//.. 
//..       ML_(linux_PRE_sys_msgrcv)( tid, ARG2, msgp, ARG3, msgtyp, ARG4 );
//.. 
//..       /* if ((ARG4 & VKI_IPC_NOWAIT) == 0)
//..             tst->sys_flags |= MayBlock;
//..       */
//..       break;
//..    }
//..    case VKI_MSGGET:
//..       break;
//..    case VKI_MSGCTL:
//..       ML_(linux_PRE_sys_msgctl)( tid, ARG2, ARG3, ARG5 );
//..       break;
//..    case VKI_SHMAT:
//..       PRE_MEM_WRITE( "shmat(raddr)", ARG4, sizeof(Addr) );
//..       ARG5 = ML_(generic_PRE_sys_shmat)( tid, ARG2, ARG5, ARG3 );
//..       if (ARG5 == 0)
//..          SET_RESULT( -VKI_EINVAL );
//..       break;
//..    case VKI_SHMDT:
//..       if (!ML_(generic_PRE_sys_shmdt)(tid, ARG5))
//.. 	 SET_RESULT( -VKI_EINVAL );
//..       break;
//..    case VKI_SHMGET:
//..       break;
//..    case VKI_SHMCTL: /* IPCOP_shmctl */
//..       ML_(generic_PRE_sys_shmctl)( tid, ARG2, ARG3, ARG5 );
//..       break;
//..    default:
//..       VG_(message)(Vg_DebugMsg, "FATAL: unhandled syscall(ipc) %d", ARG1 );
//..       VG_(core_panic)("... bye!\n");
//..       break; /*NOTREACHED*/
//..    }   
//.. }

//.. POST(sys_ipc)
//.. {
//..    switch (ARG1 /* call */) {
//..    case VKI_SEMOP:
//..    case VKI_SEMGET:
//..       break;
//..    case VKI_SEMCTL:
//..    {
//..       UWord arg = deref_Addr( tid, ARG5, "semctl(arg)" );
//..       ML_(generic_PRE_sys_semctl)( tid, ARG2, ARG3, ARG4, arg );
//..       break;
//..    }
//..    case VKI_SEMTIMEDOP:
//..    case VKI_MSGSND:
//..       break;
//..    case VKI_MSGRCV:
//..    {
//..       Addr msgp;
//..       Word msgtyp;
//.. 
//..       msgp = deref_Addr( tid,
//.. 			 (Addr) (&((struct vki_ipc_kludge *)ARG5)->msgp),
//.. 			 "msgrcv(msgp)" );
//..       msgtyp = deref_Addr( tid,
//.. 			   (Addr) (&((struct vki_ipc_kludge *)ARG5)->msgtyp),
//.. 			   "msgrcv(msgp)" );
//.. 
//..       ML_(linux_POST_sys_msgrcv)( tid, RES, ARG2, msgp, ARG3, msgtyp, ARG4 );
//..       break;
//..    }
//..    case VKI_MSGGET:
//..       break;
//..    case VKI_MSGCTL:
//..       ML_(linux_POST_sys_msgctl)( tid, RES, ARG2, ARG3, ARG5 );
//..       break;
//..    case VKI_SHMAT:
//..    {
//..       Addr addr;
//.. 
//..       /* force readability. before the syscall it is
//..        * indeed uninitialized, as can be seen in
//..        * glibc/sysdeps/unix/sysv/linux/shmat.c */
//..       POST_MEM_WRITE( ARG4, sizeof( Addr ) );
//.. 
//..       addr = deref_Addr ( tid, ARG4, "shmat(addr)" );
//..       if ( addr > 0 ) { 
//..          ML_(generic_POST_sys_shmat)( tid, addr, ARG2, ARG5, ARG3 );
//..       }
//..       break;
//..    }
//..    case VKI_SHMDT:
//..       ML_(generic_POST_sys_shmdt)( tid, RES, ARG5 );
//..       break;
//..    case VKI_SHMGET:
//..       break;
//..    case VKI_SHMCTL:
//..       ML_(generic_POST_sys_shmctl)( tid, RES, ARG2, ARG3, ARG5 );
//..       break;
//..    default:
//..       VG_(message)(Vg_DebugMsg,
//.. 		   "FATAL: unhandled syscall(ipc) %d",
//.. 		   ARG1 );
//..       VG_(core_panic)("... bye!\n");
//..       break; /*NOTREACHED*/
//..    }
//.. }

/* NB: This is an almost identical clone of versions for x86-linux and
   arm-linux, which are themselves literally identical. */
PRE(sys_sigsuspend)
{
   /* The C library interface to sigsuspend just takes a pointer to
      a signal mask but this system call only takes the first word of
      the signal mask as an argument so only 32 signals are supported.
     
      In fact glibc normally uses rt_sigsuspend if it is available as
      that takes a pointer to the signal mask so supports more signals.
    */
   *flags |= SfMayBlock;
   PRINT("sys_sigsuspend ( %ld )", ARG1 );
   PRE_REG_READ1(int, "sigsuspend", vki_old_sigset_t, mask);
}

PRE(sys_spu_create)
{
   PRE_MEM_RASCIIZ("stat64(filename)", ARG1);
}
POST(sys_spu_create)
{
   vg_assert(SUCCESS);
}

PRE(sys_spu_run)
{
   *flags |= SfMayBlock;
   if (ARG2 != 0)
      PRE_MEM_WRITE("npc", ARG2, sizeof(unsigned int));
   PRE_MEM_READ("event", ARG3, sizeof(unsigned int));
}
POST(sys_spu_run)
{
   if (ARG2 != 0)
      POST_MEM_WRITE(ARG2, sizeof(unsigned int));
}

#undef PRE
#undef POST

/* ---------------------------------------------------------------------
   The ppc32/Linux syscall table
   ------------------------------------------------------------------ */

/* Add an ppc32-linux specific wrapper to a syscall table. */
#define PLAX_(sysno, name)    WRAPPER_ENTRY_X_(ppc32_linux, sysno, name) 
#define PLAXY(sysno, name)    WRAPPER_ENTRY_XY(ppc32_linux, sysno, name)

// This table maps from __NR_xxx syscall numbers (from
// linux/include/asm-ppc/unistd.h) to the appropriate PRE/POST sys_foo()
// wrappers on ppc32 (as per sys_call_table in linux/arch/ppc/kernel/entry.S).
//
// For those syscalls not handled by Valgrind, the annotation indicate its
// arch/OS combination, eg. */* (generic), */Linux (Linux only), ?/?
// (unknown).

static SyscallTableEntry syscall_table[] = {
//..   (restart_syscall)                                      // 0
   GENX_(__NR_exit,              sys_exit),              // 1
   GENX_(__NR_fork,              sys_fork),              // 2
   GENXY(__NR_read,              sys_read),              // 3
   GENX_(__NR_write,             sys_write),             // 4

   GENXY(__NR_open,              sys_open),              // 5
   GENXY(__NR_close,             sys_close),             // 6
   GENXY(__NR_waitpid,           sys_waitpid),           // 7
   GENXY(__NR_creat,             sys_creat),             // 8
   GENX_(__NR_link,              sys_link),              // 9

   GENX_(__NR_unlink,            sys_unlink),            // 10
   GENX_(__NR_execve,            sys_execve),            // 11
   GENX_(__NR_chdir,             sys_chdir),             // 12
   GENXY(__NR_time,              sys_time),              // 13
   GENX_(__NR_mknod,             sys_mknod),             // 14
//.. 
   GENX_(__NR_chmod,             sys_chmod),             // 15
   GENX_(__NR_lchown,            sys_lchown),          // 16 ## P
//..    GENX_(__NR_break,             sys_ni_syscall),        // 17
//..    //   (__NR_oldstat,           sys_stat),              // 18 (obsolete)
   LINX_(__NR_lseek,             sys_lseek),             // 19
//.. 
   GENX_(__NR_getpid,            sys_getpid),            // 20
   LINX_(__NR_mount,             sys_mount),             // 21
   LINX_(__NR_umount,            sys_oldumount),         // 22
   GENX_(__NR_setuid,            sys_setuid),            // 23 ## P
   GENX_(__NR_getuid,            sys_getuid),            // 24 ## P
//.. 
//..    //   (__NR_stime,             sys_stime),             // 25 * (SVr4,SVID,X/OPEN)
//..    PLAXY(__NR_ptrace,            sys_ptrace),            // 26
   GENX_(__NR_alarm,             sys_alarm),             // 27
//..    //   (__NR_oldfstat,          sys_fstat),             // 28 * L -- obsolete
   GENX_(__NR_pause,             sys_pause),             // 29
//.. 
   LINX_(__NR_utime,             sys_utime),                  // 30
//..    GENX_(__NR_stty,              sys_ni_syscall),        // 31
//..    GENX_(__NR_gtty,              sys_ni_syscall),        // 32
   GENX_(__NR_access,            sys_access),            // 33
//..    GENX_(__NR_nice,              sys_nice),              // 34
//.. 
//..    GENX_(__NR_ftime,             sys_ni_syscall),        // 35
//..    GENX_(__NR_sync,              sys_sync),              // 36
   GENX_(__NR_kill,              sys_kill),              // 37
   GENX_(__NR_rename,            sys_rename),            // 38
   GENX_(__NR_mkdir,             sys_mkdir),             // 39

   GENX_(__NR_rmdir,             sys_rmdir),             // 40
   GENXY(__NR_dup,               sys_dup),               // 41
   LINXY(__NR_pipe,              sys_pipe),              // 42
   GENXY(__NR_times,             sys_times),             // 43
//..    GENX_(__NR_prof,              sys_ni_syscall),        // 44
//.. 
   GENX_(__NR_brk,               sys_brk),               // 45
   GENX_(__NR_setgid,            sys_setgid),            // 46
   GENX_(__NR_getgid,            sys_getgid),            // 47
//..    //   (__NR_signal,            sys_signal),            // 48 */* (ANSI C)
   GENX_(__NR_geteuid,           sys_geteuid),           // 49

   GENX_(__NR_getegid,           sys_getegid),           // 50
//..    GENX_(__NR_acct,              sys_acct),              // 51
   LINX_(__NR_umount2,           sys_umount),            // 52
//..    GENX_(__NR_lock,              sys_ni_syscall),        // 53
   LINXY(__NR_ioctl,             sys_ioctl),             // 54
//.. 
   LINXY(__NR_fcntl,             sys_fcntl),             // 55
//..    GENX_(__NR_mpx,               sys_ni_syscall),        // 56
   GENX_(__NR_setpgid,           sys_setpgid),           // 57
//..    GENX_(__NR_ulimit,            sys_ni_syscall),        // 58
//..    //   (__NR_oldolduname,       sys_olduname),          // 59 Linux -- obsolete

   GENX_(__NR_umask,             sys_umask),             // 60
   GENX_(__NR_chroot,            sys_chroot),            // 61
//..    //   (__NR_ustat,             sys_ustat)              // 62 SVr4 -- deprecated
   GENXY(__NR_dup2,              sys_dup2),              // 63
   GENX_(__NR_getppid,           sys_getppid),           // 64

   GENX_(__NR_getpgrp,           sys_getpgrp),           // 65
   GENX_(__NR_setsid,            sys_setsid),            // 66
   LINXY(__NR_sigaction,         sys_sigaction),         // 67
//..    //   (__NR_sgetmask,          sys_sgetmask),          // 68 */* (ANSI C)
//..    //   (__NR_ssetmask,          sys_ssetmask),          // 69 */* (ANSI C)
//.. 
   GENX_(__NR_setreuid,          sys_setreuid),          // 70
   GENX_(__NR_setregid,          sys_setregid),          // 71
   PLAX_(__NR_sigsuspend,        sys_sigsuspend),        // 72
   LINXY(__NR_sigpending,        sys_sigpending),        // 73
//..    //   (__NR_sethostname,       sys_sethostname),       // 74 */*
//.. 
   GENX_(__NR_setrlimit,         sys_setrlimit),              // 75
//..    GENXY(__NR_getrlimit,         sys_old_getrlimit),     // 76
   GENXY(__NR_getrusage,         sys_getrusage),         // 77
   GENXY(__NR_gettimeofday,      sys_gettimeofday),           // 78
//..    GENX_(__NR_settimeofday,      sys_settimeofday),      // 79
//.. 
   GENXY(__NR_getgroups,         sys_getgroups),         // 80
   GENX_(__NR_setgroups,         sys_setgroups),         // 81
//..    PLAX_(__NR_select,            old_select),            // 82
   GENX_(__NR_symlink,           sys_symlink),           // 83
//..    //   (__NR_oldlstat,          sys_lstat),             // 84 -- obsolete
//.. 
   GENX_(__NR_readlink,          sys_readlink),          // 85
//..    //   (__NR_uselib,            sys_uselib),            // 86 */Linux
//..    //   (__NR_swapon,            sys_swapon),            // 87 */Linux
//..    //   (__NR_reboot,            sys_reboot),            // 88 */Linux
//..    //   (__NR_readdir,           old_readdir),           // 89 -- superseded

   PLAX_(__NR_mmap,              sys_mmap),                   // 90
   GENXY(__NR_munmap,            sys_munmap),                 // 91
   GENX_(__NR_truncate,          sys_truncate),          // 92
   GENX_(__NR_ftruncate,         sys_ftruncate),         // 93
   GENX_(__NR_fchmod,            sys_fchmod),            // 94

   GENX_(__NR_fchown,            sys_fchown),            // 95
   GENX_(__NR_getpriority,       sys_getpriority),       // 96
   GENX_(__NR_setpriority,       sys_setpriority),       // 97
//..    GENX_(__NR_profil,            sys_ni_syscall),        // 98
   GENXY(__NR_statfs,            sys_statfs),            // 99
//.. 
   GENXY(__NR_fstatfs,           sys_fstatfs),           // 100
//..    LINX_(__NR_ioperm,            sys_ioperm),            // 101
   PLAXY(__NR_socketcall,        sys_socketcall),        // 102
   LINXY(__NR_syslog,            sys_syslog),            // 103
   GENXY(__NR_setitimer,         sys_setitimer),         // 104

   GENXY(__NR_getitimer,         sys_getitimer),         // 105
   GENXY(__NR_stat,              sys_newstat),           // 106
   GENXY(__NR_lstat,             sys_newlstat),          // 107
   GENXY(__NR_fstat,             sys_newfstat),          // 108
//..    //   (__NR_olduname,          sys_uname),             // 109 -- obsolete
//.. 
//..    GENX_(__NR_iopl,              sys_iopl),              // 110
   LINX_(__NR_vhangup,           sys_vhangup),           // 111
//..    GENX_(__NR_idle,              sys_ni_syscall),        // 112
//..    //   (__NR_vm86old,           sys_vm86old),           // 113 x86/Linux-only
   GENXY(__NR_wait4,             sys_wait4),             // 114
//.. 
//..    //   (__NR_swapoff,           sys_swapoff),           // 115 */Linux 
   LINXY(__NR_sysinfo,           sys_sysinfo),           // 116
   PLAXY(__NR_ipc,               sys_ipc),               // 117
   GENX_(__NR_fsync,             sys_fsync),             // 118
   PLAX_(__NR_sigreturn,         sys_sigreturn),         // 119 ?/Linux
//.. 
   PLAX_(__NR_clone,             sys_clone),             // 120
//..    //   (__NR_setdomainname,     sys_setdomainname),     // 121 */*(?)
   GENXY(__NR_uname,             sys_newuname),          // 122
//..    PLAX_(__NR_modify_ldt,        sys_modify_ldt),        // 123
   LINXY(__NR_adjtimex,          sys_adjtimex),          // 124

   GENXY(__NR_mprotect,          sys_mprotect),          // 125
   LINXY(__NR_sigprocmask,       sys_sigprocmask),       // 126
   GENX_(__NR_create_module,     sys_ni_syscall),        // 127
   LINX_(__NR_init_module,       sys_init_module),       // 128
   LINX_(__NR_delete_module,     sys_delete_module),     // 129
//.. 
//..    // Nb: get_kernel_syms() was removed 2.4-->2.6
//..    GENX_(__NR_get_kernel_syms,   sys_ni_syscall),        // 130
//..    LINX_(__NR_quotactl,          sys_quotactl),          // 131
   GENX_(__NR_getpgid,           sys_getpgid),           // 132
   GENX_(__NR_fchdir,            sys_fchdir),            // 133
//..    //   (__NR_bdflush,           sys_bdflush),           // 134 */Linux
//.. 
//..    //   (__NR_sysfs,             sys_sysfs),             // 135 SVr4
   LINX_(__NR_personality,       sys_personality),       // 136
//..    GENX_(__NR_afs_syscall,       sys_ni_syscall),        // 137
   LINX_(__NR_setfsuid,          sys_setfsuid),          // 138
   LINX_(__NR_setfsgid,          sys_setfsgid),          // 139

   LINXY(__NR__llseek,           sys_llseek),            // 140
   GENXY(__NR_getdents,          sys_getdents),          // 141
   GENX_(__NR__newselect,        sys_select),            // 142
   GENX_(__NR_flock,             sys_flock),             // 143
   GENX_(__NR_msync,             sys_msync),             // 144
//.. 
   GENXY(__NR_readv,             sys_readv),             // 145
   GENX_(__NR_writev,            sys_writev),            // 146
   GENX_(__NR_getsid,            sys_getsid),            // 147
   GENX_(__NR_fdatasync,         sys_fdatasync),         // 148
   LINXY(__NR__sysctl,           sys_sysctl),            // 149
//.. 
   GENX_(__NR_mlock,             sys_mlock),             // 150
   GENX_(__NR_munlock,           sys_munlock),           // 151
   GENX_(__NR_mlockall,          sys_mlockall),          // 152
   LINX_(__NR_munlockall,        sys_munlockall),        // 153
   LINXY(__NR_sched_setparam,    sys_sched_setparam),    // 154
//.. 
   LINXY(__NR_sched_getparam,         sys_sched_getparam),        // 155
   LINX_(__NR_sched_setscheduler,     sys_sched_setscheduler),    // 156
   LINX_(__NR_sched_getscheduler,     sys_sched_getscheduler),    // 157
   LINX_(__NR_sched_yield,            sys_sched_yield),           // 158
   LINX_(__NR_sched_get_priority_max, sys_sched_get_priority_max),// 159

   LINX_(__NR_sched_get_priority_min, sys_sched_get_priority_min),// 160
   LINXY(__NR_sched_rr_get_interval,  sys_sched_rr_get_interval), // 161
   GENXY(__NR_nanosleep,         sys_nanosleep),         // 162
   GENX_(__NR_mremap,            sys_mremap),            // 163
   LINX_(__NR_setresuid,         sys_setresuid),         // 164

   LINXY(__NR_getresuid,         sys_getresuid),         // 165

//..    GENX_(__NR_query_module,      sys_ni_syscall),        // 166
   GENXY(__NR_poll,              sys_poll),              // 167
//..    //   (__NR_nfsservctl,        sys_nfsservctl),        // 168 */Linux
//.. 
   LINX_(__NR_setresgid,         sys_setresgid),         // 169
   LINXY(__NR_getresgid,         sys_getresgid),         // 170
   LINXY(__NR_prctl,             sys_prctl),             // 171
   PLAX_(__NR_rt_sigreturn,      sys_rt_sigreturn),      // 172
   LINXY(__NR_rt_sigaction,      sys_rt_sigaction),      // 173

   LINXY(__NR_rt_sigprocmask,    sys_rt_sigprocmask),    // 174
   LINXY(__NR_rt_sigpending,     sys_rt_sigpending),     // 175
   LINXY(__NR_rt_sigtimedwait,   sys_rt_sigtimedwait),   // 176
   LINXY(__NR_rt_sigqueueinfo,   sys_rt_sigqueueinfo),   // 177
   LINX_(__NR_rt_sigsuspend,     sys_rt_sigsuspend),     // 178

   GENXY(__NR_pread64,           sys_pread64),           // 179
   GENX_(__NR_pwrite64,          sys_pwrite64),          // 180
   GENX_(__NR_chown,             sys_chown),             // 181
   GENXY(__NR_getcwd,            sys_getcwd),            // 182
   LINXY(__NR_capget,            sys_capget),            // 183
   LINX_(__NR_capset,            sys_capset),            // 184
   GENXY(__NR_sigaltstack,       sys_sigaltstack),       // 185
   LINXY(__NR_sendfile,          sys_sendfile),          // 186
//..    GENXY(__NR_getpmsg,           sys_getpmsg),           // 187
//..    GENX_(__NR_putpmsg,           sys_putpmsg),           // 188

   // Nb: we treat vfork as fork
   GENX_(__NR_vfork,             sys_fork),              // 189
   GENXY(__NR_ugetrlimit,        sys_getrlimit),         // 190
   LINX_(__NR_readahead,         sys_readahead),         // 191 */Linux
   PLAX_(__NR_mmap2,             sys_mmap2),             // 192
   GENX_(__NR_truncate64,        sys_truncate64),        // 193
   GENX_(__NR_ftruncate64,       sys_ftruncate64),       // 194
//..    

   PLAXY(__NR_stat64,            sys_stat64),            // 195
   PLAXY(__NR_lstat64,           sys_lstat64),           // 196
   PLAXY(__NR_fstat64,           sys_fstat64),           // 197

// __NR_pciconfig_read                                        // 198
// __NR_pciconfig_write                                       // 199
// __NR_pciconfig_iobase                                      // 200
// __NR_multiplexer                                           // 201

   GENXY(__NR_getdents64,        sys_getdents64),        // 202
//..    //   (__NR_pivot_root,        sys_pivot_root),        // 203 */Linux
   LINXY(__NR_fcntl64,           sys_fcntl64),           // 204
   GENX_(__NR_madvise,           sys_madvise),           // 205
   GENXY(__NR_mincore,           sys_mincore),           // 206
   LINX_(__NR_gettid,            sys_gettid),            // 207
//..    LINX_(__NR_tkill,             sys_tkill),             // 208 */Linux
//..    LINX_(__NR_setxattr,          sys_setxattr),          // 209
//..    LINX_(__NR_lsetxattr,         sys_lsetxattr),         // 210
//..    LINX_(__NR_fsetxattr,         sys_fsetxattr),         // 211
   LINXY(__NR_getxattr,          sys_getxattr),          // 212
   LINXY(__NR_lgetxattr,         sys_lgetxattr),         // 213
   LINXY(__NR_fgetxattr,         sys_fgetxattr),         // 214
   LINXY(__NR_listxattr,         sys_listxattr),         // 215
   LINXY(__NR_llistxattr,        sys_llistxattr),        // 216
   LINXY(__NR_flistxattr,        sys_flistxattr),        // 217
   LINX_(__NR_removexattr,       sys_removexattr),       // 218
   LINX_(__NR_lremovexattr,      sys_lremovexattr),      // 219
   LINX_(__NR_fremovexattr,      sys_fremovexattr),      // 220

   LINXY(__NR_futex,             sys_futex),                  // 221
   LINX_(__NR_sched_setaffinity, sys_sched_setaffinity), // 222
   LINXY(__NR_sched_getaffinity, sys_sched_getaffinity), // 223
/* 224 currently unused */

// __NR_tuxcall                                               // 225

   LINXY(__NR_sendfile64,        sys_sendfile64),        // 226
//.. 
   LINX_(__NR_io_setup,          sys_io_setup),          // 227
   LINX_(__NR_io_destroy,        sys_io_destroy),        // 228
   LINXY(__NR_io_getevents,      sys_io_getevents),      // 229
   LINX_(__NR_io_submit,         sys_io_submit),         // 230
   LINXY(__NR_io_cancel,         sys_io_cancel),         // 231
//.. 
   LINX_(__NR_set_tid_address,   sys_set_tid_address),   // 232

   LINX_(__NR_fadvise64,         sys_fadvise64),         // 233 */(Linux?)
   LINX_(__NR_exit_group,        sys_exit_group),        // 234
//..    GENXY(__NR_lookup_dcookie,    sys_lookup_dcookie),    // 235
   LINXY(__NR_epoll_create,      sys_epoll_create),      // 236
   LINX_(__NR_epoll_ctl,         sys_epoll_ctl),         // 237
   LINXY(__NR_epoll_wait,        sys_epoll_wait),        // 238

//..    //   (__NR_remap_file_pages,  sys_remap_file_pages),  // 239 */Linux
   LINXY(__NR_timer_create,      sys_timer_create),      // 240
   LINXY(__NR_timer_settime,     sys_timer_settime),     // 241
   LINXY(__NR_timer_gettime,     sys_timer_gettime),     // 242
   LINX_(__NR_timer_getoverrun,  sys_timer_getoverrun),  // 243
   LINX_(__NR_timer_delete,      sys_timer_delete),      // 244
   LINX_(__NR_clock_settime,     sys_clock_settime),     // 245
   LINXY(__NR_clock_gettime,     sys_clock_gettime),     // 246
   LINXY(__NR_clock_getres,      sys_clock_getres),      // 247
   LINXY(__NR_clock_nanosleep,   sys_clock_nanosleep),   // 248

// __NR_swapcontext                                           // 249

   LINXY(__NR_tgkill,            sys_tgkill),            // 250 */Linux
//..    GENX_(__NR_utimes,            sys_utimes),            // 251
   GENXY(__NR_statfs64,          sys_statfs64),          // 252
   GENXY(__NR_fstatfs64,         sys_fstatfs64),         // 253
   LINX_(__NR_fadvise64_64,      sys_fadvise64_64),      // 254 */(Linux?)

// __NR_rtas                                                  // 255

/* Number 256 is reserved for sys_debug_setcontext */
/* Number 257 is reserved for vserver */
/* Number 258 is reserved for new sys_remap_file_pages */
/* Number 259 is reserved for new sys_mbind */
   LINXY(__NR_get_mempolicy,     sys_get_mempolicy),          // 260
   LINX_(__NR_set_mempolicy,     sys_set_mempolicy),          // 261

   LINXY(__NR_mq_open,           sys_mq_open),           // 262
   LINX_(__NR_mq_unlink,         sys_mq_unlink),         // 263
   LINX_(__NR_mq_timedsend,      sys_mq_timedsend),      // 264
   LINXY(__NR_mq_timedreceive,   sys_mq_timedreceive),   // 265
   LINX_(__NR_mq_notify,         sys_mq_notify),         // 266
   LINXY(__NR_mq_getsetattr,     sys_mq_getsetattr),     // 267
// __NR_kexec_load                                            // 268

/* Number 269 is reserved for sys_add_key */
/* Number 270 is reserved for sys_request_key */
/* Number 271 is reserved for sys_keyctl */
/* Number 272 is reserved for sys_waitid */
   LINX_(__NR_ioprio_set,        sys_ioprio_set),         // 273
   LINX_(__NR_ioprio_get,        sys_ioprio_get),         // 274

   LINX_(__NR_inotify_init,  sys_inotify_init),               // 275
   LINX_(__NR_inotify_add_watch,  sys_inotify_add_watch),     // 276
   LINX_(__NR_inotify_rm_watch,   sys_inotify_rm_watch),      // 277
   PLAXY(__NR_spu_run,            sys_spu_run),               // 278
   PLAX_(__NR_spu_create,         sys_spu_create),            // 279

   LINX_(__NR_pselect6,          sys_pselect6),          // 280
   LINXY(__NR_ppoll,             sys_ppoll),             // 281

   LINXY(__NR_openat,            sys_openat),            // 286
   LINX_(__NR_mkdirat,           sys_mkdirat),           // 287
   LINX_(__NR_mknodat,           sys_mknodat),           // 288
   LINX_(__NR_fchownat,          sys_fchownat),          // 289
   LINX_(__NR_futimesat,         sys_futimesat),         // 290
   PLAXY(__NR_fstatat64,         sys_fstatat64),         // 291
   LINX_(__NR_unlinkat,          sys_unlinkat),          // 292
   LINX_(__NR_renameat,          sys_renameat),          // 293
   LINX_(__NR_linkat,            sys_linkat),            // 294
   LINX_(__NR_symlinkat,         sys_symlinkat),         // 295
   LINX_(__NR_readlinkat,        sys_readlinkat),        // 296
   LINX_(__NR_fchmodat,          sys_fchmodat),          // 297
   LINX_(__NR_faccessat,         sys_faccessat),         // 298
   LINX_(__NR_set_robust_list,   sys_set_robust_list),   // 299
   LINXY(__NR_get_robust_list,   sys_get_robust_list),   // 300
   LINXY(__NR_move_pages,        sys_move_pages),        // 301
   LINXY(__NR_getcpu,            sys_getcpu),            // 302
   LINXY(__NR_epoll_pwait,       sys_epoll_pwait),       // 303
   LINX_(__NR_utimensat,         sys_utimensat),         // 304
   LINXY(__NR_signalfd,          sys_signalfd),          // 305
   LINXY(__NR_timerfd_create,    sys_timerfd_create),    // 306
   LINX_(__NR_eventfd,           sys_eventfd),           // 307
   LINX_(__NR_sync_file_range2,  sys_sync_file_range2),  // 308
   LINX_(__NR_fallocate,         sys_fallocate),         // 309
//   LINXY(__NR_subpage_prot,       sys_ni_syscall),       // 310
   LINXY(__NR_timerfd_settime,   sys_timerfd_settime),  // 311
   LINXY(__NR_timerfd_gettime,   sys_timerfd_gettime),  // 312
   LINXY(__NR_signalfd4,         sys_signalfd4),        // 313
   LINX_(__NR_eventfd2,          sys_eventfd2),         // 314
   LINXY(__NR_epoll_create1,     sys_epoll_create1),    // 315
   LINXY(__NR_dup3,              sys_dup3),             // 316
   LINXY(__NR_pipe2,             sys_pipe2),            // 317
   LINXY(__NR_inotify_init1,     sys_inotify_init1),    // 318
   LINXY(__NR_perf_event_open,   sys_perf_event_open),  // 319
   LINXY(__NR_preadv,            sys_preadv),           // 320
   LINX_(__NR_pwritev,           sys_pwritev),          // 321
   LINXY(__NR_rt_tgsigqueueinfo, sys_rt_tgsigqueueinfo),// 322

   LINXY(__NR_process_vm_readv,  sys_process_vm_readv), // 351
   LINX_(__NR_process_vm_writev, sys_process_vm_writev) // 352
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

#endif // defined(VGP_ppc32_linux)

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
