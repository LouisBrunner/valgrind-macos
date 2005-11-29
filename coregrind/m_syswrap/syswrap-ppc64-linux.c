
/*--------------------------------------------------------------------*/
/*--- Platform-specific syscalls stuff.      syswrap-ppc64-linux.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2005 Nicholas Nethercote <njn@valgrind.org>
   Copyright (C) 2005 Cerion Armour-Brown <cerion@open-works.co.uk>

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

#include "pub_core_basics.h"
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

#include "vki_unistd.h"              /* for the __NR_* constants */


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
".globl .vgModuleLocal_call_on_new_stack_0_1\n"
".vgModuleLocal_call_on_new_stack_0_1:\n"
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

            word (fn)(void*)    in r3
            void* child_stack   in r4
            word flags          in r5
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

        Returns a ULong encoded as: top half is %cr following syscall,
        low half is syscall return value (r3).
 */
#define __NR_CLONE        VG_STRINGIFY(__NR_clone)
#define __NR_EXIT         VG_STRINGIFY(__NR_exit)

extern
ULong do_syscall_clone_ppc64_linux ( Word (*fn)(void *), 
                                     void* stack, 
                                     Int   flags, 
                                     void* arg,
                                     Int*  child_tid, 
                                     Int*  parent_tid, 
                                     void/*vki_modify_ldt_t*/ * );
asm(
".text\n"
".do_syscall_clone_ppc64_linux:\n"
"       stdu    1,-64(1)\n"
"       std     29,40(1)\n"
"       std     30,48(1)\n"
"       std     31,56(1)\n"
"       mr      30,3\n"              // preserve fn
"       mr      31,6\n"              // preserve arg

        // setup child stack
"       rlwinm  4,4,0,~0xf\n"        // trim sp to multiple of 16 bytes
"       li      0,0\n"
"       stdu    0,-32(4)\n"          // make initial stack frame
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

"       mfcr    4\n"                 // CR now in low half r4
"       slwi    4,4,16\n"
"       slwi    4,4,16\n"            // CR now in hi half r4

"       slwi    3,3,16\n"
"       slwi    3,3,16\n"
"       srwi    3,3,16\n"
"       srwi    3,3,16\n"            // zero out hi half r3

"       and     3,3,4\n"             // r3 = CR : syscall-retval
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
   NSegment*    seg;
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
   { UInt old_cr = LibVEX_GuestPPC64_get_CR( &ctst->arch.vex );
     /* %r3 = 0 */
     ctst->arch.vex.guest_GPR3 = 0;
     /* %cr0.so = 0 */
     LibVEX_GuestPPC64_put_CR( old_cr & ~(1<<28), &ctst->arch.vex );
   }

   if (sp != 0)
      ctst->arch.vex.guest_GPR1 = sp;

   ctst->os_state.parent = ptid;

   /* inherit signal mask */
   ctst->sig_mask = ptst->sig_mask;
   ctst->tmp_sig_mask = ptst->sig_mask;

   /* We don't really know where the client stack is, because its
      allocated by the client.  The best we can do is look at the
      memory mappings and try to derive some useful information.  We
      assume that esp starts near its highest possible value, and can
      only go down to the start of the mmaped segment. */
   seg = VG_(am_find_nsegment)(sp);
   if (seg && seg->kind != SkResvn) {
      ctst->client_stack_highest_word = (Addr)VG_PGROUNDUP(sp);
      ctst->client_stack_szB = ctst->client_stack_highest_word - seg->start;

      if (debug)
	 VG_(printf)("\ntid %d: guessed client stack range %p-%p\n",
		     ctid, seg->start, VG_PGROUNDUP(sp));
   } else {
      VG_(message)(Vg_UserMsg, "!? New thread %d starts with R1(%p) unmapped\n",
		   ctid, sp);
      ctst->client_stack_szB  = 0;
   }

   if (flags & VKI_CLONE_SETTLS) {
      if (debug)
         VG_(printf)("clone child has SETTLS: tls at %p\n", child_tls);
      ctst->arch.vex.guest_GPR13 = child_tls;
   }

   flags &= ~VKI_CLONE_SETTLS;

   /* start the thread with everything blocked */
   VG_(sigprocmask)(VKI_SIG_SETMASK, &blockall, &savedmask);

   /* Create the new thread */
   word64 = do_syscall_clone_ppc64_linux(
               ML_(start_thread_NORETURN), stack, flags, &VG_(threads)[ctid],
               child_tidptr, parent_tidptr, NULL
            );
   /* Low half word64 is syscall return value.  Hi half is
      the entire CR, from which we need to extract CR0.SO. */
   /* VG_(printf)("word64 = 0x%llx\n", word64); */
   res = VG_(mk_SysRes_ppc32_linux)( 
            /*val*/(UInt)(word64 & 0xFFFFFFFFULL), 
            /*errflag*/ (UInt)((word64 >> (32+28)) & 1)
         );

   VG_(sigprocmask)(VKI_SIG_SETMASK, &savedmask, NULL);

  out:
   if (res.isError) {
      /* clone failed */
      VG_(cleanup_thread)(&ctst->arch);
      ctst->status = VgTs_Empty;
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
   child->vex_shadow = parent->vex_shadow;
}


/* ---------------------------------------------------------------------
   PRE/POST wrappers for ppc64/Linux-specific syscalls
   ------------------------------------------------------------------ */

#define PRE(name)       DEFN_PRE_TEMPLATE(ppc32_linux, name)
#define POST(name)      DEFN_POST_TEMPLATE(ppc32_linux, name)

/* Add prototypes for the wrappers declared here, so that gcc doesn't
   harass us for not having prototypes.  Really this is a kludge --
   the right thing to do is to make these wrappers 'static' since they
   aren't visible outside this file, but that requires even more macro
   magic. */

//zz DECL_TEMPLATE(ppc64_linux, sys_socketcall);
//zz DECL_TEMPLATE(ppc64_linux, sys_mmap);
//zz DECL_TEMPLATE(ppc64_linux, sys_mmap2);
//zz DECL_TEMPLATE(ppc64_linux, sys_stat64);
//zz DECL_TEMPLATE(ppc64_linux, sys_lstat64);
//zz DECL_TEMPLATE(ppc64_linux, sys_fstat64);
//zz DECL_TEMPLATE(ppc64_linux, sys_ipc);
//zz DECL_TEMPLATE(ppc64_linux, sys_clone);
//zz DECL_TEMPLATE(ppc64_linux, sys_sigreturn);
//zz DECL_TEMPLATE(ppc64_linux, sys_rt_sigreturn);
//zz DECL_TEMPLATE(ppc64_linux, sys_sigaction);
//zz 
//zz PRE(sys_socketcall)
//zz {
//zz #  define ARG2_0  (((UWord*)ARG2)[0])
//zz #  define ARG2_1  (((UWord*)ARG2)[1])
//zz #  define ARG2_2  (((UWord*)ARG2)[2])
//zz #  define ARG2_3  (((UWord*)ARG2)[3])
//zz #  define ARG2_4  (((UWord*)ARG2)[4])
//zz #  define ARG2_5  (((UWord*)ARG2)[5])
//zz 
//zz    *flags |= SfMayBlock;
//zz    PRINT("sys_socketcall ( %d, %p )",ARG1,ARG2);
//zz    PRE_REG_READ2(long, "socketcall", int, call, unsigned long *, args);
//zz 
//zz    switch (ARG1 /* request */) {
//zz 
//zz    case VKI_SYS_SOCKETPAIR:
//zz      /* int socketpair(int d, int type, int protocol, int sv[2]); */
//zz       PRE_MEM_READ( "socketcall.socketpair(args)", ARG2, 4*sizeof(Addr) );
//zz       ML_(generic_PRE_sys_socketpair)( tid, ARG2_0, ARG2_1, ARG2_2, ARG2_3 );
//zz       break;
//zz 
//zz    case VKI_SYS_SOCKET:
//zz      /* int socket(int domain, int type, int protocol); */
//zz       PRE_MEM_READ( "socketcall.socket(args)", ARG2, 3*sizeof(Addr) );
//zz       break;
//zz 
//zz    case VKI_SYS_BIND:
//zz      /* int bind(int sockfd, struct sockaddr *my_addr,
//zz 	int addrlen); */
//zz       PRE_MEM_READ( "socketcall.bind(args)", ARG2, 3*sizeof(Addr) );
//zz       ML_(generic_PRE_sys_bind)( tid, ARG2_0, ARG2_1, ARG2_2 );
//zz       break;
//zz 
//zz    case VKI_SYS_LISTEN:
//zz      /* int listen(int s, int backlog); */
//zz       PRE_MEM_READ( "socketcall.listen(args)", ARG2, 2*sizeof(Addr) );
//zz       break;
//zz 
//zz    case VKI_SYS_ACCEPT: {
//zz      /* int accept(int s, struct sockaddr *addr, int *addrlen); */
//zz       PRE_MEM_READ( "socketcall.accept(args)", ARG2, 3*sizeof(Addr) );
//zz       ML_(generic_PRE_sys_accept)( tid, ARG2_0, ARG2_1, ARG2_2 );
//zz       break;
//zz    }
//zz 
//zz    case VKI_SYS_SENDTO:
//zz      /* int sendto(int s, const void *msg, int len,
//zz                     unsigned int flags,
//zz                     const struct sockaddr *to, int tolen); */
//zz      PRE_MEM_READ( "socketcall.sendto(args)", ARG2, 6*sizeof(Addr) );
//zz      ML_(generic_PRE_sys_sendto)( tid, ARG2_0, ARG2_1, ARG2_2,
//zz 				  ARG2_3, ARG2_4, ARG2_5 );
//zz      break;
//zz 
//zz    case VKI_SYS_SEND:
//zz      /* int send(int s, const void *msg, size_t len, int flags); */
//zz      PRE_MEM_READ( "socketcall.send(args)", ARG2, 4*sizeof(Addr) );
//zz      ML_(generic_PRE_sys_send)( tid, ARG2_0, ARG2_1, ARG2_2 );
//zz      break;
//zz 
//zz    case VKI_SYS_RECVFROM:
//zz      /* int recvfrom(int s, void *buf, int len, unsigned int flags,
//zz 	struct sockaddr *from, int *fromlen); */
//zz      PRE_MEM_READ( "socketcall.recvfrom(args)", ARG2, 6*sizeof(Addr) );
//zz      ML_(generic_PRE_sys_recvfrom)( tid, ARG2_0, ARG2_1, ARG2_2,
//zz 				    ARG2_3, ARG2_4, ARG2_5 );
//zz      break;
//zz 
//zz    case VKI_SYS_RECV:
//zz      /* int recv(int s, void *buf, int len, unsigned int flags); */
//zz      /* man 2 recv says:
//zz          The  recv call is normally used only on a connected socket
//zz          (see connect(2)) and is identical to recvfrom with a  NULL
//zz          from parameter.
//zz      */
//zz      PRE_MEM_READ( "socketcall.recv(args)", ARG2, 4*sizeof(Addr) );
//zz      ML_(generic_PRE_sys_recv)( tid, ARG2_0, ARG2_1, ARG2_2 );
//zz      break;
//zz 
//zz    case VKI_SYS_CONNECT:
//zz      /* int connect(int sockfd,
//zz 	struct sockaddr *serv_addr, int addrlen ); */
//zz      PRE_MEM_READ( "socketcall.connect(args)", ARG2, 3*sizeof(Addr) );
//zz      ML_(generic_PRE_sys_connect)( tid, ARG2_0, ARG2_1, ARG2_2 );
//zz      break;
//zz 
//zz    case VKI_SYS_SETSOCKOPT:
//zz      /* int setsockopt(int s, int level, int optname,
//zz 	const void *optval, int optlen); */
//zz      PRE_MEM_READ( "socketcall.setsockopt(args)", ARG2, 5*sizeof(Addr) );
//zz      ML_(generic_PRE_sys_setsockopt)( tid, ARG2_0, ARG2_1, ARG2_2,
//zz 				      ARG2_3, ARG2_4 );
//zz      break;
//zz 
//zz    case VKI_SYS_GETSOCKOPT:
//zz      /* int getsockopt(int s, int level, int optname,
//zz 	void *optval, socklen_t *optlen); */
//zz      PRE_MEM_READ( "socketcall.getsockopt(args)", ARG2, 5*sizeof(Addr) );
//zz      ML_(generic_PRE_sys_getsockopt)( tid, ARG2_0, ARG2_1, ARG2_2,
//zz 				      ARG2_3, ARG2_4 );
//zz      break;
//zz 
//zz    case VKI_SYS_GETSOCKNAME:
//zz      /* int getsockname(int s, struct sockaddr* name, int* namelen) */
//zz      PRE_MEM_READ( "socketcall.getsockname(args)", ARG2, 3*sizeof(Addr) );
//zz      ML_(generic_PRE_sys_getsockname)( tid, ARG2_0, ARG2_1, ARG2_2 );
//zz      break;
//zz 
//zz    case VKI_SYS_GETPEERNAME:
//zz      /* int getpeername(int s, struct sockaddr* name, int* namelen) */
//zz      PRE_MEM_READ( "socketcall.getpeername(args)", ARG2, 3*sizeof(Addr) );
//zz      ML_(generic_PRE_sys_getpeername)( tid, ARG2_0, ARG2_1, ARG2_2 );
//zz      break;
//zz 
//zz    case VKI_SYS_SHUTDOWN:
//zz      /* int shutdown(int s, int how); */
//zz      PRE_MEM_READ( "socketcall.shutdown(args)", ARG2, 2*sizeof(Addr) );
//zz      break;
//zz 
//zz    case VKI_SYS_SENDMSG: {
//zz      /* int sendmsg(int s, const struct msghdr *msg, int flags); */
//zz 
//zz      /* this causes warnings, and I don't get why. glibc bug?
//zz       * (after all it's glibc providing the arguments array)
//zz        PRE_MEM_READ( "socketcall.sendmsg(args)", ARG2, 3*sizeof(Addr) );
//zz      */
//zz      ML_(generic_PRE_sys_sendmsg)( tid, ARG2_0, ARG2_1 );
//zz      break;
//zz    }
//zz 
//zz    case VKI_SYS_RECVMSG: {
//zz      /* int recvmsg(int s, struct msghdr *msg, int flags); */
//zz 
//zz      /* this causes warnings, and I don't get why. glibc bug?
//zz       * (after all it's glibc providing the arguments array)
//zz        PRE_MEM_READ("socketcall.recvmsg(args)", ARG2, 3*sizeof(Addr) );
//zz      */
//zz      ML_(generic_PRE_sys_recvmsg)( tid, ARG2_0, ARG2_1 );
//zz      break;
//zz    }
//zz 
//zz    default:
//zz      VG_(message)(Vg_DebugMsg,"Warning: unhandled socketcall 0x%x",ARG1);
//zz      SET_STATUS_Failure( VKI_EINVAL );
//zz      break;
//zz    }
//zz #  undef ARG2_0
//zz #  undef ARG2_1
//zz #  undef ARG2_2
//zz #  undef ARG2_3
//zz #  undef ARG2_4
//zz #  undef ARG2_5
//zz }
//zz 
//zz POST(sys_socketcall)
//zz {
//zz #  define ARG2_0  (((UWord*)ARG2)[0])
//zz #  define ARG2_1  (((UWord*)ARG2)[1])
//zz #  define ARG2_2  (((UWord*)ARG2)[2])
//zz #  define ARG2_3  (((UWord*)ARG2)[3])
//zz #  define ARG2_4  (((UWord*)ARG2)[4])
//zz #  define ARG2_5  (((UWord*)ARG2)[5])
//zz 
//zz   SysRes r;
//zz   vg_assert(SUCCESS);
//zz   switch (ARG1 /* request */) {
//zz 
//zz   case VKI_SYS_SOCKETPAIR:
//zz     r = ML_(generic_POST_sys_socketpair)(
//zz 					 tid, VG_(mk_SysRes_Success)(RES),
//zz 					 ARG2_0, ARG2_1, ARG2_2, ARG2_3
//zz 					 );
//zz     SET_STATUS_from_SysRes(r);
//zz     break;
//zz 
//zz   case VKI_SYS_SOCKET:
//zz     r = ML_(generic_POST_sys_socket)( tid, VG_(mk_SysRes_Success)(RES) );
//zz     SET_STATUS_from_SysRes(r);
//zz     break;
//zz 
//zz   case VKI_SYS_BIND:
//zz     /* int bind(int sockfd, struct sockaddr *my_addr,
//zz        int addrlen); */
//zz     break;
//zz 
//zz   case VKI_SYS_LISTEN:
//zz     /* int listen(int s, int backlog); */
//zz     break;
//zz 
//zz   case VKI_SYS_ACCEPT:
//zz     /* int accept(int s, struct sockaddr *addr, int *addrlen); */
//zz     r = ML_(generic_POST_sys_accept)( tid, VG_(mk_SysRes_Success)(RES),
//zz 				      ARG2_0, ARG2_1, ARG2_2 );
//zz     SET_STATUS_from_SysRes(r);
//zz     break;
//zz 
//zz   case VKI_SYS_SENDTO:
//zz     break;
//zz 
//zz   case VKI_SYS_SEND:
//zz     break;
//zz 
//zz   case VKI_SYS_RECVFROM:
//zz     ML_(generic_POST_sys_recvfrom)( tid, VG_(mk_SysRes_Success)(RES),
//zz 				    ARG2_0, ARG2_1, ARG2_2,
//zz 				    ARG2_3, ARG2_4, ARG2_5 );
//zz     break;
//zz 
//zz   case VKI_SYS_RECV:
//zz     ML_(generic_POST_sys_recv)( tid, RES, ARG2_0, ARG2_1, ARG2_2 );
//zz     break;
//zz 
//zz   case VKI_SYS_CONNECT:
//zz     break;
//zz 
//zz   case VKI_SYS_SETSOCKOPT:
//zz     break;
//zz 
//zz   case VKI_SYS_GETSOCKOPT:
//zz     ML_(generic_POST_sys_getsockopt)( tid, VG_(mk_SysRes_Success)(RES),
//zz 				      ARG2_0, ARG2_1,
//zz 				      ARG2_2, ARG2_3, ARG2_4 );
//zz     break;
//zz 
//zz   case VKI_SYS_GETSOCKNAME:
//zz     ML_(generic_POST_sys_getsockname)( tid, VG_(mk_SysRes_Success)(RES),
//zz 				       ARG2_0, ARG2_1, ARG2_2 );
//zz     break;
//zz 
//zz   case VKI_SYS_GETPEERNAME:
//zz     ML_(generic_POST_sys_getpeername)( tid, VG_(mk_SysRes_Success)(RES),
//zz 				       ARG2_0, ARG2_1, ARG2_2 );
//zz     break;
//zz 
//zz   case VKI_SYS_SHUTDOWN:
//zz     break;
//zz 
//zz   case VKI_SYS_SENDMSG:
//zz     break;
//zz 
//zz   case VKI_SYS_RECVMSG:
//zz     ML_(generic_POST_sys_recvmsg)( tid, ARG2_0, ARG2_1 );
//zz     break;
//zz 
//zz   default:
//zz     VG_(message)(Vg_DebugMsg,"FATAL: unhandled socketcall 0x%x",ARG1);
//zz     VG_(core_panic)("... bye!\n");
//zz     break; /*NOTREACHED*/
//zz   }
//zz #  undef ARG2_0
//zz #  undef ARG2_1
//zz #  undef ARG2_2
//zz #  undef ARG2_3
//zz #  undef ARG2_4
//zz #  undef ARG2_5
//zz }
//zz 
//zz PRE(sys_mmap)
//zz {
//zz    SysRes r;
//zz 
//zz    PRINT("sys_mmap ( %p, %llu, %d, %d, %d, %d )",
//zz          ARG1, (ULong)ARG2, ARG3, ARG4, ARG5, ARG6 );
//zz    PRE_REG_READ6(long, "mmap",
//zz                  unsigned long, start, unsigned long, length,
//zz                  unsigned long, prot,  unsigned long, flags,
//zz                  unsigned long, fd,    unsigned long, offset);
//zz 
//zz    r = ML_(generic_PRE_sys_mmap)( tid, ARG1, ARG2, ARG3, ARG4, ARG5, 
//zz                                        (Off64T)ARG6 );
//zz    SET_STATUS_from_SysRes(r);
//zz }
//zz 
//zz PRE(sys_mmap2)
//zz {
//zz    SysRes r;
//zz 
//zz    // Exactly like old_mmap() except:
//zz    //  - the file offset is specified in pagesize units rather than bytes,
//zz    //    so that it can be used for files bigger than 2^32 bytes.
//zz    PRINT("sys_mmap2 ( %p, %llu, %d, %d, %d, %d )",
//zz          ARG1, (ULong)ARG2, ARG3, ARG4, ARG5, ARG6 );
//zz    PRE_REG_READ6(long, "mmap2",
//zz                  unsigned long, start, unsigned long, length,
//zz                  unsigned long, prot,  unsigned long, flags,
//zz                  unsigned long, fd,    unsigned long, offset);
//zz 
//zz    r = ML_(generic_PRE_sys_mmap)( tid, ARG1, ARG2, ARG3, ARG4, ARG5, 
//zz                                        VKI_PAGE_SIZE * (Off64T)ARG6 );
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
//zz 
//zz static Addr deref_Addr ( ThreadId tid, Addr a, Char* s )
//zz {
//zz    Addr* a_p = (Addr*)a;
//zz    PRE_MEM_READ( s, (Addr)a_p, sizeof(Addr) );
//zz    return *a_p;
//zz }
//zz 
//zz PRE(sys_ipc)
//zz {
//zz   PRINT("sys_ipc ( %d, %d, %d, %d, %p, %d )", ARG1,ARG2,ARG3,ARG4,ARG5,ARG6);
//zz   // XXX: this is simplistic -- some args are not used in all circumstances.
//zz   PRE_REG_READ6(int, "ipc",
//zz 		vki_uint, call, int, first, int, second, int, third,
//zz 		void *, ptr, long, fifth)
//zz 
//zz     switch (ARG1 /* call */) {
//zz     case VKI_SEMOP:
//zz       ML_(generic_PRE_sys_semop)( tid, ARG2, ARG5, ARG3 );
//zz       *flags |= SfMayBlock;
//zz       break;
//zz     case VKI_SEMGET:
//zz       break;
//zz     case VKI_SEMCTL:
//zz       {
//zz 	UWord arg = deref_Addr( tid, ARG5, "semctl(arg)" );
//zz 	ML_(generic_PRE_sys_semctl)( tid, ARG2, ARG3, ARG4, arg );
//zz 	break;
//zz       }
//zz     case VKI_SEMTIMEDOP:
//zz       ML_(generic_PRE_sys_semtimedop)( tid, ARG2, ARG5, ARG3, ARG6 );
//zz       *flags |= SfMayBlock;
//zz       break;
//zz     case VKI_MSGSND:
//zz       ML_(linux_PRE_sys_msgsnd)( tid, ARG2, ARG5, ARG3, ARG4 );
//zz       if ((ARG4 & VKI_IPC_NOWAIT) == 0)
//zz 	*flags |= SfMayBlock;
//zz       break;
//zz     case VKI_MSGRCV:
//zz       {
//zz 	Addr msgp;
//zz 	Word msgtyp;
//zz 
//zz 	msgp = deref_Addr( tid,
//zz 			   (Addr) (&((struct vki_ipc_kludge *)ARG5)->msgp),
//zz 			   "msgrcv(msgp)" );
//zz 	msgtyp = deref_Addr( tid,
//zz 			     (Addr) (&((struct vki_ipc_kludge *)ARG5)->msgtyp),
//zz 			     "msgrcv(msgp)" );
//zz 
//zz 	ML_(linux_PRE_sys_msgrcv)( tid, ARG2, msgp, ARG3, msgtyp, ARG4 );
//zz 
//zz 	if ((ARG4 & VKI_IPC_NOWAIT) == 0)
//zz 	  *flags |= SfMayBlock;
//zz 	break;
//zz       }
//zz     case VKI_MSGGET:
//zz       break;
//zz     case VKI_MSGCTL:
//zz       ML_(linux_PRE_sys_msgctl)( tid, ARG2, ARG3, ARG5 );
//zz       break;
//zz     case VKI_SHMAT:
//zz       {
//zz 	UWord w;
//zz 	PRE_MEM_WRITE( "shmat(raddr)", ARG4, sizeof(Addr) );
//zz 	w = ML_(generic_PRE_sys_shmat)( tid, ARG2, ARG5, ARG3 );
//zz 	if (w == 0)
//zz 	  SET_STATUS_Failure( VKI_EINVAL );
//zz 	else
//zz 	  ARG5 = w;
//zz 	break;
//zz       }
//zz     case VKI_SHMDT:
//zz       if (!ML_(generic_PRE_sys_shmdt)(tid, ARG5))
//zz 	SET_STATUS_Failure( VKI_EINVAL );
//zz       break;
//zz     case VKI_SHMGET:
//zz       break;
//zz     case VKI_SHMCTL: /* IPCOP_shmctl */
//zz       ML_(generic_PRE_sys_shmctl)( tid, ARG2, ARG3, ARG5 );
//zz       break;
//zz     default:
//zz       VG_(message)(Vg_DebugMsg, "FATAL: unhandled syscall(ipc) %d", ARG1 );
//zz       VG_(core_panic)("... bye!\n");
//zz       break; /*NOTREACHED*/
//zz     }
//zz }
//zz 
//zz POST(sys_ipc)
//zz {
//zz   vg_assert(SUCCESS);
//zz   switch (ARG1 /* call */) {
//zz   case VKI_SEMOP:
//zz   case VKI_SEMGET:
//zz     break;
//zz   case VKI_SEMCTL:
//zz     {
//zz       UWord arg = deref_Addr( tid, ARG5, "semctl(arg)" );
//zz       ML_(generic_PRE_sys_semctl)( tid, ARG2, ARG3, ARG4, arg );
//zz       break;
//zz     }
//zz   case VKI_SEMTIMEDOP:
//zz   case VKI_MSGSND:
//zz     break;
//zz   case VKI_MSGRCV:
//zz     {
//zz       Addr msgp;
//zz       Word msgtyp;
//zz 
//zz       msgp = deref_Addr( tid,
//zz                          (Addr) (&((struct vki_ipc_kludge *)ARG5)->msgp),
//zz                          "msgrcv(msgp)" );
//zz       msgtyp = deref_Addr( tid,
//zz                            (Addr) (&((struct vki_ipc_kludge *)ARG5)->msgtyp),
//zz                            "msgrcv(msgp)" );
//zz 
//zz       ML_(linux_POST_sys_msgrcv)( tid, RES, ARG2, msgp, ARG3, msgtyp, ARG4 );
//zz       break;
//zz     }
//zz   case VKI_MSGGET:
//zz     break;
//zz   case VKI_MSGCTL:
//zz     ML_(linux_POST_sys_msgctl)( tid, RES, ARG2, ARG3, ARG5 );
//zz     break;
//zz   case VKI_SHMAT:
//zz     {
//zz       Addr addr;
//zz 
//zz       /* force readability. before the syscall it is
//zz        * indeed uninitialized, as can be seen in
//zz        * glibc/sysdeps/unix/sysv/linux/shmat.c */
//zz       POST_MEM_WRITE( ARG4, sizeof( Addr ) );
//zz 
//zz       addr = deref_Addr ( tid, ARG4, "shmat(addr)" );
//zz       if ( addr > 0 ) {
//zz 	ML_(generic_POST_sys_shmat)( tid, addr, ARG2, ARG5, ARG3 );
//zz       }
//zz       break;
//zz     }
//zz   case VKI_SHMDT:
//zz     ML_(generic_POST_sys_shmdt)( tid, RES, ARG5 );
//zz     break;
//zz   case VKI_SHMGET:
//zz     break;
//zz   case VKI_SHMCTL:
//zz     ML_(generic_POST_sys_shmctl)( tid, RES, ARG2, ARG3, ARG5 );
//zz     break;
//zz   default:
//zz     VG_(message)(Vg_DebugMsg,
//zz 		 "FATAL: unhandled syscall(ipc) %d",
//zz 		 ARG1 );
//zz     VG_(core_panic)("... bye!\n");
//zz     break; /*NOTREACHED*/
//zz   }
//zz }
//zz 
//zz PRE(sys_clone)
//zz {
//zz    UInt cloneflags;
//zz 
//zz    PRINT("sys_clone ( %x, %p, %p, %p, %p )",ARG1,ARG2,ARG3,ARG4,ARG5);
//zz    PRE_REG_READ5(int, "clone",
//zz                  unsigned long, flags,
//zz                  void *,        child_stack,
//zz                  int *,         parent_tidptr,
//zz                  void *,        child_tls,
//zz                  int *,         child_tidptr);
//zz 
//zz    if (ARG1 & VKI_CLONE_PARENT_SETTID) {
//zz       PRE_MEM_WRITE("clone(parent_tidptr)", ARG3, sizeof(Int));
//zz       if (!VG_(am_is_valid_for_client)(ARG3, sizeof(Int), 
//zz                                              VKI_PROT_WRITE)) {
//zz          SET_STATUS_Failure( VKI_EFAULT );
//zz          return;
//zz       }
//zz    }
//zz    if (ARG1 & (VKI_CLONE_CHILD_SETTID | VKI_CLONE_CHILD_CLEARTID)) {
//zz       PRE_MEM_WRITE("clone(child_tidptr)", ARG5, sizeof(Int));
//zz       if (!VG_(am_is_valid_for_client)(ARG5, sizeof(Int), 
//zz                                              VKI_PROT_WRITE)) {
//zz          SET_STATUS_Failure( VKI_EFAULT );
//zz          return;
//zz       }
//zz    }
//zz 
//zz    cloneflags = ARG1;
//zz 
//zz    if (!ML_(client_signal_OK)(ARG1 & VKI_CSIGNAL)) {
//zz       SET_STATUS_Failure( VKI_EINVAL );
//zz       return;
//zz    }
//zz 
//zz    /* Only look at the flags we really care about */
//zz    switch (cloneflags & (VKI_CLONE_VM | VKI_CLONE_FS 
//zz                          | VKI_CLONE_FILES | VKI_CLONE_VFORK)) {
//zz    case VKI_CLONE_VM | VKI_CLONE_FS | VKI_CLONE_FILES:
//zz       /* thread creation */
//zz       SET_STATUS_from_SysRes(
//zz          do_clone(tid,
//zz                   ARG1,         /* flags */
//zz                   (Addr)ARG2,   /* child SP */
//zz                   (Int *)ARG3,  /* parent_tidptr */
//zz                   (Int *)ARG5,  /* child_tidptr */
//zz                   (Addr)ARG4)); /* child_tls */
//zz       break;
//zz 
//zz    case VKI_CLONE_VFORK | VKI_CLONE_VM: /* vfork */
//zz       /* FALLTHROUGH - assume vfork == fork */
//zz       cloneflags &= ~(VKI_CLONE_VFORK | VKI_CLONE_VM);
//zz 
//zz    case 0: /* plain fork */
//zz       SET_STATUS_from_SysRes(
//zz          ML_(do_fork_clone)(tid,
//zz                        cloneflags,      /* flags */
//zz                        (Int *)ARG3,     /* parent_tidptr */
//zz                        (Int *)ARG5));   /* child_tidptr */
//zz       break;
//zz 
//zz    default:
//zz       /* should we just ENOSYS? */
//zz       VG_(message)(Vg_UserMsg, "Unsupported clone() flags: 0x%x", ARG1);
//zz       VG_(message)(Vg_UserMsg, "");
//zz       VG_(message)(Vg_UserMsg, "The only supported clone() uses are:");
//zz       VG_(message)(Vg_UserMsg, " - via a threads library (LinuxThreads or NPTL)");
//zz       VG_(message)(Vg_UserMsg, " - via the implementation of fork or vfork");
//zz       VG_(unimplemented)
//zz          ("Valgrind does not support general clone().");
//zz    }
//zz 
//zz    if (SUCCESS) {
//zz       if (ARG1 & VKI_CLONE_PARENT_SETTID)
//zz          POST_MEM_WRITE(ARG3, sizeof(Int));
//zz       if (ARG1 & (VKI_CLONE_CHILD_SETTID | VKI_CLONE_CHILD_CLEARTID))
//zz          POST_MEM_WRITE(ARG5, sizeof(Int));
//zz 
//zz       /* Thread creation was successful; let the child have the chance
//zz          to run */
//zz       *flags |= SfYieldAfter;
//zz    }
//zz }
//zz 
//zz PRE(sys_sigreturn)
//zz {
//zz    ThreadState* tst;
//zz    PRINT("sigreturn ( )");
//zz 
//zz    vg_assert(VG_(is_valid_tid)(tid));
//zz    vg_assert(tid >= 1 && tid < VG_N_THREADS);
//zz    vg_assert(VG_(is_running_thread)(tid));
//zz 
//zz    ///* Adjust esp to point to start of frame; skip back up over
//zz    //   sigreturn sequence's "popl %eax" and handler ret addr */
//zz    tst = VG_(get_ThreadState)(tid);
//zz    //tst->arch.vex.guest_ESP -= sizeof(Addr)+sizeof(Word);
//zz    // Should we do something equivalent on ppc64?  Who knows.
//zz 
//zz    ///* This is only so that the EIP is (might be) useful to report if
//zz    //   something goes wrong in the sigreturn */
//zz    //ML_(fixup_guest_state_to_restart_syscall)(&tst->arch);
//zz    // Should we do something equivalent on ppc64?  Who knows.
//zz 
//zz    VG_(sigframe_destroy)(tid, False);
//zz 
//zz    /* For unclear reasons, it appears we need the syscall to return
//zz       without changing R3.  Since R3 is the return value, and can
//zz       denote either success or failure, we must set up so that the
//zz       driver logic copies it back unchanged.  Also, note R3 is of
//zz       the guest registers written by VG_(sigframe_destroy). */
//zz    /* jrs 16 Nov 05: for some reason this occasionally causes the 
//zz       is-this-a-sane-error-value sanity check to fail:
//zz       m_syswrap/syswrap-ppc64-linux.c:1037
//zz         (vgSysWrap_ppc64_linux_sys_sigreturn_before): 
//zz         Assertion 'wzz >= 0 && wzz < 10000' failed.
//zz       Hence use a sanity-check-free version.  
//zz       Perhaps we should ignore CR0.S0 here?
//zz       In general I have no idea what this is for or if it is necessary.
//zz       It's a conceptual copy-n-paste from the x86 equivalent, but I'm 
//zz       equally unclear as to whether it is needed there either.
//zz    */
//zz    SET_STATUS_from_SysRes_NO_SANITY_CHECK(
//zz       VG_(mk_SysRes_ppc64_linux)( 
//zz          tst->arch.vex.guest_GPR3,
//zz          /* get CR0.SO */
//zz          (LibVEX_GuestPPC32_get_CR( &tst->arch.vex ) >> 28) & 1
//zz       )
//zz    );
//zz 
//zz    /* Check to see if some any signals arose as a result of this. */
//zz    *flags |= SfPollAfter;
//zz }
//zz 
//zz PRE(sys_rt_sigreturn)
//zz {
//zz    ThreadState* tst;
//zz    PRINT("rt_sigreturn ( )");
//zz 
//zz    vg_assert(VG_(is_valid_tid)(tid));
//zz    vg_assert(tid >= 1 && tid < VG_N_THREADS);
//zz    vg_assert(VG_(is_running_thread)(tid));
//zz 
//zz    ///* Adjust esp to point to start of frame; skip back up over handler
//zz    //   ret addr */
//zz    tst = VG_(get_ThreadState)(tid);
//zz    //tst->arch.vex.guest_ESP -= sizeof(Addr);
//zz    // Should we do something equivalent on ppc64?  Who knows.
//zz 
//zz    ///* This is only so that the EIP is (might be) useful to report if
//zz    //   something goes wrong in the sigreturn */
//zz    //ML_(fixup_guest_state_to_restart_syscall)(&tst->arch);
//zz    // Should we do something equivalent on ppc64?  Who knows.
//zz 
//zz    VG_(sigframe_destroy)(tid, True);
//zz 
//zz    /* See comments above in PRE(sys_sigreturn) about this. */
//zz    SET_STATUS_from_SysRes_NO_SANITY_CHECK(
//zz       VG_(mk_SysRes_ppc64_linux)( 
//zz          tst->arch.vex.guest_GPR3,
//zz          /* get CR0.SO */
//zz          (LibVEX_GuestPPC32_get_CR( &tst->arch.vex ) >> 28) & 1
//zz       )
//zz    );
//zz 
//zz    /* Check to see if some any signals arose as a result of this. */
//zz    *flags |= SfPollAfter;
//zz }
//zz 
//zz /* Convert from non-RT to RT sigset_t's */
//zz static 
//zz void convert_sigset_to_rt(const vki_old_sigset_t *oldset, vki_sigset_t *set)
//zz {
//zz    VG_(sigemptyset)(set);
//zz    set->sig[0] = *oldset;
//zz }
//zz PRE(sys_sigaction)
//zz {
//zz    struct vki_sigaction new, old;
//zz    struct vki_sigaction *newp, *oldp;
//zz 
//zz    PRINT("sys_sigaction ( %d, %p, %p )", ARG1,ARG2,ARG3);
//zz    PRE_REG_READ3(int, "sigaction",
//zz                  int, signum, const struct old_sigaction *, act,
//zz                  struct old_sigaction *, oldact);
//zz 
//zz    newp = oldp = NULL;
//zz 
//zz    if (ARG2 != 0) {
//zz       struct vki_old_sigaction *sa = (struct vki_old_sigaction *)ARG2;
//zz       PRE_MEM_READ( "sigaction(act->sa_handler)", (Addr)&sa->ksa_handler, sizeof(sa->ksa_handler));
//zz       PRE_MEM_READ( "sigaction(act->sa_mask)", (Addr)&sa->sa_mask, sizeof(sa->sa_mask));
//zz       PRE_MEM_READ( "sigaction(act->sa_flags)", (Addr)&sa->sa_flags, sizeof(sa->sa_flags));
//zz       if (ML_(safe_to_deref)(sa,sizeof(sa)) 
//zz           && (sa->sa_flags & VKI_SA_RESTORER))
//zz          PRE_MEM_READ( "sigaction(act->sa_restorer)", (Addr)&sa->sa_restorer, sizeof(sa->sa_restorer));
//zz    }
//zz 
//zz    if (ARG3 != 0) {
//zz       PRE_MEM_WRITE( "sigaction(oldact)", ARG3, sizeof(struct vki_old_sigaction));
//zz       oldp = &old;
//zz    }
//zz 
//zz    //jrs 20050207: what?!  how can this make any sense?
//zz    //if (VG_(is_kerror)(SYSRES))
//zz    //   return;
//zz 
//zz    if (ARG2 != 0) {
//zz       struct vki_old_sigaction *oldnew = (struct vki_old_sigaction *)ARG2;
//zz 
//zz       new.ksa_handler = oldnew->ksa_handler;
//zz       new.sa_flags = oldnew->sa_flags;
//zz       new.sa_restorer = oldnew->sa_restorer;
//zz       convert_sigset_to_rt(&oldnew->sa_mask, &new.sa_mask);
//zz       newp = &new;
//zz    }
//zz 
//zz    SET_STATUS_from_SysRes( VG_(do_sys_sigaction)(ARG1, newp, oldp) );
//zz 
//zz    if (ARG3 != 0 && SUCCESS && RES == 0) {
//zz       struct vki_old_sigaction *oldold = (struct vki_old_sigaction *)ARG3;
//zz 
//zz       oldold->ksa_handler = oldp->ksa_handler;
//zz       oldold->sa_flags = oldp->sa_flags;
//zz       oldold->sa_restorer = oldp->sa_restorer;
//zz       oldold->sa_mask = oldp->sa_mask.sig[0];
//zz    }
//zz }
//zz 
//zz POST(sys_sigaction)
//zz {
//zz    vg_assert(SUCCESS);
//zz    if (RES == 0 && ARG3 != 0)
//zz       POST_MEM_WRITE( ARG3, sizeof(struct vki_old_sigaction));
//zz }

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

const SyscallTableEntry ML_(syscall_table)[] = {
// _____(__NR_restart_syscall,   sys__NR_restart_syscall),    //   0
// _____(__NR_exit,              sys__NR_exit),               //   1
// _____(__NR_fork,              sys__NR_fork),               //   2
// _____(__NR_read,              sys__NR_read),               //   3
// _____(__NR_write,             sys__NR_write),              //   4

// _____(__NR_open,              sys__NR_open),               //   5
// _____(__NR_close,             sys__NR_close),              //   6
// _____(__NR_waitpid,           sys__NR_waitpid),            //   7
// _____(__NR_creat,             sys__NR_creat),              //   8
// _____(__NR_link,              sys__NR_link),               //   9

// _____(__NR_unlink,            sys__NR_unlink),             //  10
// _____(__NR_execve,            sys__NR_execve),             //  11
// _____(__NR_chdir,             sys__NR_chdir),              //  12
// _____(__NR_time,              sys__NR_time),               //  13
// _____(__NR_mknod,             sys__NR_mknod),              //  14

// _____(__NR_chmod,             sys__NR_chmod),              //  15
// _____(__NR_lchown,            sys__NR_lchown),             //  16
// _____(__NR_break,             sys__NR_break),              //  17
// _____(__NR_oldstat,           sys__NR_oldstat),            //  18
// _____(__NR_lseek,             sys__NR_lseek),              //  19

// _____(__NR_getpid,            sys__NR_getpid),             //  20
// _____(__NR_mount,             sys__NR_mount),              //  21
// _____(__NR_umount,            sys__NR_umount),             //  22
// _____(__NR_setuid,            sys__NR_setuid),             //  23
// _____(__NR_getuid,            sys__NR_getuid),             //  24

// _____(__NR_stime,             sys__NR_stime),              //  25
// _____(__NR_ptrace,            sys__NR_ptrace),             //  26
// _____(__NR_alarm,             sys__NR_alarm),              //  27
// _____(__NR_oldfstat,          sys__NR_oldfstat),           //  28
// _____(__NR_pause,             sys__NR_pause),              //  29

// _____(__NR_utime,             sys__NR_utime),              //  30
// _____(__NR_stty,              sys__NR_stty),               //  31
// _____(__NR_gtty,              sys__NR_gtty),               //  32
// _____(__NR_access,            sys__NR_access),             //  33
// _____(__NR_nice,              sys__NR_nice),               //  34

// _____(__NR_ftime,             sys__NR_ftime),              //  35
// _____(__NR_sync,              sys__NR_sync),               //  36
// _____(__NR_kill,              sys__NR_kill),               //  37
// _____(__NR_rename,            sys__NR_rename),             //  38
// _____(__NR_mkdir,             sys__NR_mkdir),              //  39

// _____(__NR_rmdir,             sys__NR_rmdir),              //  40
// _____(__NR_dup,               sys__NR_dup),                //  41
// _____(__NR_pipe,              sys__NR_pipe),               //  42
// _____(__NR_times,             sys__NR_times),              //  43
// _____(__NR_prof,              sys__NR_prof),               //  44

// _____(__NR_brk,               sys__NR_brk),                //  45
// _____(__NR_setgid,            sys__NR_setgid),             //  46
// _____(__NR_getgid,            sys__NR_getgid),             //  47
// _____(__NR_signal,            sys__NR_signal),             //  48
// _____(__NR_geteuid,           sys__NR_geteuid),            //  49

// _____(__NR_getegid,           sys__NR_getegid),            //  50
// _____(__NR_acct,              sys__NR_acct),               //  51
// _____(__NR_umount2,           sys__NR_umount2),            //  52
// _____(__NR_lock,              sys__NR_lock),               //  53
// _____(__NR_ioctl,             sys__NR_ioctl),              //  54

// _____(__NR_fcntl,             sys__NR_fcntl),              //  55
// _____(__NR_mpx,               sys__NR_mpx),                //  56
// _____(__NR_setpgid,           sys__NR_setpgid),            //  57
// _____(__NR_ulimit,            sys__NR_ulimit),             //  58
// _____(__NR_oldolduname,       sys__NR_oldolduname),        //  59

// _____(__NR_umask,             sys__NR_umask),              //  60
// _____(__NR_chroot,            sys__NR_chroot),             //  61
// _____(__NR_ustat,             sys__NR_ustat),              //  62
// _____(__NR_dup2,              sys__NR_dup2),               //  63
// _____(__NR_getppid,           sys__NR_getppid),            //  64

// _____(__NR_getpgrp,           sys__NR_getpgrp),            //  65
// _____(__NR_setsid,            sys__NR_setsid),             //  66
// _____(__NR_sigaction,         sys__NR_sigaction),          //  67
// _____(__NR_sgetmask,          sys__NR_sgetmask),           //  68
// _____(__NR_ssetmask,          sys__NR_ssetmask),           //  69

// _____(__NR_setreuid,          sys__NR_setreuid),           //  70
// _____(__NR_setregid,          sys__NR_setregid),           //  71
// _____(__NR_sigsuspend,        sys__NR_sigsuspend),         //  72
// _____(__NR_sigpending,        sys__NR_sigpending),         //  73
// _____(__NR_sethostname,       sys__NR_sethostname),        //  74

// _____(__NR_setrlimit,         sys__NR_setrlimit),          //  75
// _____(__NR_getrlimit,         sys__NR_getrlimit),          //  76
// _____(__NR_getrusage,         sys__NR_getrusage),          //  77
// _____(__NR_gettimeofday,      sys__NR_gettimeofday),       //  78
// _____(__NR_settimeofday,      sys__NR_settimeofday),       //  79

// _____(__NR_getgroups,         sys__NR_getgroups),          //  80
// _____(__NR_setgroups,         sys__NR_setgroups),          //  81
// _____(__NR_select,            sys__NR_select),             //  82
// _____(__NR_symlink,           sys__NR_symlink),            //  83
// _____(__NR_oldlstat,          sys__NR_oldlstat),           //  84

// _____(__NR_readlink,          sys__NR_readlink),           //  85
// _____(__NR_uselib,            sys__NR_uselib),             //  86
// _____(__NR_swapon,            sys__NR_swapon),             //  87
// _____(__NR_reboot,            sys__NR_reboot),             //  88
// _____(__NR_readdir,           sys__NR_readdir),            //  89

// _____(__NR_mmap,              sys__NR_mmap),               //  90
// _____(__NR_munmap,            sys__NR_munmap),             //  91
// _____(__NR_truncate,          sys__NR_truncate),           //  92
// _____(__NR_ftruncate,         sys__NR_ftruncate),          //  93
// _____(__NR_fchmod,            sys__NR_fchmod),             //  94

// _____(__NR_fchown,            sys__NR_fchown),             //  95
// _____(__NR_getpriority,       sys__NR_getpriority),        //  96
// _____(__NR_setpriority,       sys__NR_setpriority),        //  97
// _____(__NR_profil,            sys__NR_profil),             //  98
// _____(__NR_statfs,            sys__NR_statfs),             //  99

// _____(__NR_fstatfs,           sys__NR_fstatfs),            // 100
// _____(__NR_ioperm,            sys__NR_ioperm),             // 101
// _____(__NR_socketcall,        sys__NR_socketcall),         // 102
// _____(__NR_syslog,            sys__NR_syslog),             // 103
// _____(__NR_setitimer,         sys__NR_setitimer),          // 104

// _____(__NR_getitimer,         sys__NR_getitimer),          // 105
// _____(__NR_stat,              sys__NR_stat),               // 106
// _____(__NR_lstat,             sys__NR_lstat),              // 107
// _____(__NR_fstat,             sys__NR_fstat),              // 108
// _____(__NR_olduname,          sys__NR_olduname),           // 109

// _____(__NR_iopl,              sys__NR_iopl),               // 110
// _____(__NR_vhangup,           sys__NR_vhangup),            // 111
// _____(__NR_idle,              sys__NR_idle),               // 112
// _____(__NR_vm86,              sys__NR_vm86),               // 113
// _____(__NR_wait4,             sys__NR_wait4),              // 114

// _____(__NR_swapoff,           sys__NR_swapoff),            // 115
// _____(__NR_sysinfo,           sys__NR_sysinfo),            // 116
// _____(__NR_ipc,               sys__NR_ipc),                // 117
// _____(__NR_fsync,             sys__NR_fsync),              // 118
// _____(__NR_sigreturn,         sys__NR_sigreturn),          // 119

// _____(__NR_clone,             sys__NR_clone),              // 120
// _____(__NR_setdomainname,     sys__NR_setdomainname),      // 121
// _____(__NR_uname,             sys__NR_uname),              // 122
// _____(__NR_modify_ldt,        sys__NR_modify_ldt),         // 123
// _____(__NR_adjtimex,          sys__NR_adjtimex),           // 124

// _____(__NR_mprotect,          sys__NR_mprotect),           // 125
// _____(__NR_sigprocmask,       sys__NR_sigprocmask),        // 126
// _____(__NR_create_module,     sys__NR_create_module),      // 127
// _____(__NR_init_module,       sys__NR_init_module),        // 128
// _____(__NR_delete_module,     sys__NR_delete_module),      // 129

// _____(__NR_get_kernel_syms,   sys__NR_get_kernel_syms),    // 130
// _____(__NR_quotactl,          sys__NR_quotactl),           // 131
// _____(__NR_getpgid,           sys__NR_getpgid),            // 132
// _____(__NR_fchdir,            sys__NR_fchdir),             // 133
// _____(__NR_bdflush,           sys__NR_bdflush),            // 134

// _____(__NR_sysfs,             sys__NR_sysfs),              // 135
// _____(__NR_personality,       sys__NR_personality),        // 136
// _____(__NR_afs_syscall,       sys__NR_afs_syscall),        // 137
// _____(__NR_setfsuid,          sys__NR_setfsuid),           // 138
// _____(__NR_setfsgid,          sys__NR_setfsgid),           // 139

// _____(__NR__llseek,           sys__NR__llseek),            // 140
// _____(__NR_getdents,          sys__NR_getdents),           // 141
// _____(__NR__newselect,        sys__NR__newselect),         // 142
// _____(__NR_flock,             sys__NR_flock),              // 143
// _____(__NR_msync,             sys__NR_msync),              // 144

// _____(__NR_readv,             sys__NR_readv),              // 145
// _____(__NR_writev,            sys__NR_writev),             // 146
// _____(__NR_getsid,            sys__NR_getsid),             // 147
// _____(__NR_fdatasync,         sys__NR_fdatasync),          // 148
// _____(__NR__sysctl,           sys__NR__sysctl),            // 149

// _____(__NR_mlock,             sys__NR_mlock),              // 150
// _____(__NR_munlock,           sys__NR_munlock),            // 151
// _____(__NR_mlockall,          sys__NR_mlockall),           // 152
// _____(__NR_munlockall,        sys__NR_munlockall),         // 153
// _____(__NR_sched_setparam,    sys__NR_sched_setparam),     // 154

// _____(__NR_sched_getparam,    sys__NR_sched_getparam),     // 155
// _____(__NR_sched_setscheduler,      sys__NR_sched_setscheduler),     // 156
// _____(__NR_sched_getscheduler,      sys__NR_sched_getscheduler),     // 157
// _____(__NR_sched_yield,             sys__NR_sched_yield),            // 158
// _____(__NR_sched_get_priority_max,  sys__NR_sched_get_priority_max), // 159

// _____(__NR_sched_get_priority_min,  sys__NR_sched_get_priority_min), // 160
// _____(__NR_sched_rr_get_interval,   sys__NR_sched_rr_get_interval),  // 161
// _____(__NR_nanosleep,         sys__NR_nanosleep),          // 162
// _____(__NR_mremap,            sys__NR_mremap),             // 163
// _____(__NR_setresuid,         sys__NR_setresuid),          // 164

// _____(__NR_getresuid,         sys__NR_getresuid),          // 165
// _____(__NR_query_module,      sys__NR_query_module),       // 166
// _____(__NR_poll,              sys__NR_poll),               // 167
// _____(__NR_nfsservctl,        sys__NR_nfsservctl),         // 168
// _____(__NR_setresgid,         sys__NR_setresgid),          // 169

// _____(__NR_getresgid,         sys__NR_getresgid),          // 170
// _____(__NR_prctl,             sys__NR_prctl),              // 171
// _____(__NR_rt_sigreturn,      sys__NR_rt_sigreturn),       // 172
// _____(__NR_rt_sigaction,      sys__NR_rt_sigaction),       // 173
// _____(__NR_rt_sigprocmask,    sys__NR_rt_sigprocmask),     // 174

// _____(__NR_rt_sigpending,     sys__NR_rt_sigpending),      // 175
// _____(__NR_rt_sigtimedwait,   sys__NR_rt_sigtimedwait),    // 176
// _____(__NR_rt_sigqueueinfo,   sys__NR_rt_sigqueueinfo),    // 177
// _____(__NR_rt_sigsuspend,     sys__NR_rt_sigsuspend),      // 178
// _____(__NR_pread64,           sys__NR_pread64),            // 179

// _____(__NR_pwrite64,          sys__NR_pwrite64),           // 180
// _____(__NR_chown,             sys__NR_chown),              // 181
// _____(__NR_getcwd,            sys__NR_getcwd),             // 182
// _____(__NR_capget,            sys__NR_capget),             // 183
// _____(__NR_capset,            sys__NR_capset),             // 184

// _____(__NR_sigaltstack,       sys__NR_sigaltstack),        // 185
// _____(__NR_sendfile,          sys__NR_sendfile),           // 186
// _____(__NR_getpmsg,           sys__NR_getpmsg),            // 187
// _____(__NR_putpmsg,           sys__NR_putpmsg),            // 188
// _____(__NR_vfork,             sys__NR_vfork),              // 189

// _____(__NR_ugetrlimit,        sys__NR_ugetrlimit),         // 190
// _____(__NR_readahead,         sys__NR_readahead),          // 191
// /* #define __NR_mmap2           192     32bit only */
// /* #define __NR_truncate64      193     32bit only */
// /* #define __NR_ftruncate64     194     32bit only */

// /* #define __NR_stat64          195     32bit only */
// /* #define __NR_lstat64         196     32bit only */
// /* #define __NR_fstat64         197     32bit only */
// _____(__NR_pciconfig_read,    sys__NR_pciconfig_read),     // 198
// _____(__NR_pciconfig_write,   sys__NR_pciconfig_write),    // 199

// _____(__NR_pciconfig_iobase,  sys__NR_pciconfig_iobase),   // 200
// _____(__NR_multiplexer,       sys__NR_multiplexer),        // 201
// _____(__NR_getdents64,        sys__NR_getdents64),         // 202
// _____(__NR_pivot_root,        sys__NR_pivot_root),         // 203
// /* #define __NR_fcntl64         204     32bit only */

// _____(__NR_madvise,           sys__NR_madvise),            // 205
// _____(__NR_mincore,           sys__NR_mincore),            // 206
// _____(__NR_gettid,            sys__NR_gettid),             // 207
// _____(__NR_tkill,             sys__NR_tkill),              // 208
// _____(__NR_setxattr,          sys__NR_setxattr),           // 209

// _____(__NR_lsetxattr,         sys__NR_lsetxattr),          // 210
// _____(__NR_fsetxattr,         sys__NR_fsetxattr),          // 211
// _____(__NR_getxattr,          sys__NR_getxattr),           // 212
// _____(__NR_lgetxattr,         sys__NR_lgetxattr),          // 213
// _____(__NR_fgetxattr,         sys__NR_fgetxattr),          // 214

// _____(__NR_listxattr,         sys__NR_listxattr),          // 215
// _____(__NR_llistxattr,        sys__NR_llistxattr),         // 216
// _____(__NR_flistxattr,        sys__NR_flistxattr),         // 217
// _____(__NR_removexattr,       sys__NR_removexattr),        // 218
// _____(__NR_lremovexattr,      sys__NR_lremovexattr),       // 219

// _____(__NR_fremovexattr,      sys__NR_fremovexattr),       // 220
// _____(__NR_futex,             sys__NR_futex),              // 221
// _____(__NR_sched_setaffinity, sys__NR_sched_setaffinity),  // 222
// _____(__NR_sched_getaffinity, sys__NR_sched_getaffinity),  // 223
// /* 224 currently unused */

// _____(__NR_tuxcall,           sys__NR_tuxcall),            // 225
// /* #define __NR_sendfile64      226     32bit only */
// _____(__NR_io_setup,          sys__NR_io_setup),           // 227
// _____(__NR_io_destroy,        sys__NR_io_destroy),         // 228
// _____(__NR_io_getevents,      sys__NR_io_getevents),       // 229

// _____(__NR_io_submit,         sys__NR_io_submit),          // 230
// _____(__NR_io_cancel,         sys__NR_io_cancel),          // 231
// _____(__NR_set_tid_address,   sys__NR_set_tid_address),    // 232
// _____(__NR_fadvise64,         sys__NR_fadvise64),          // 233
// _____(__NR_exit_group,        sys__NR_exit_group),         // 234

// _____(__NR_lookup_dcookie,    sys__NR_lookup_dcookie),     // 235
// _____(__NR_epoll_create,      sys__NR_epoll_create),       // 236
// _____(__NR_epoll_ctl,         sys__NR_epoll_ctl),          // 237
// _____(__NR_epoll_wait,        sys__NR_epoll_wait),         // 238
// _____(__NR_remap_file_pages,  sys__NR_remap_file_pages),   // 239

// _____(__NR_timer_create,      sys__NR_timer_create),       // 240
// _____(__NR_timer_settime,     sys__NR_timer_settime),      // 241
// _____(__NR_timer_gettime,     sys__NR_timer_gettime),      // 242
// _____(__NR_timer_getoverrun,  sys__NR_timer_getoverrun),   // 243
// _____(__NR_timer_delete,      sys__NR_timer_delete),       // 244

// _____(__NR_clock_settime,     sys__NR_clock_settime),      // 245
// _____(__NR_clock_gettime,     sys__NR_clock_gettime),      // 246
// _____(__NR_clock_getres,      sys__NR_clock_getres),       // 247
// _____(__NR_clock_nanosleep,   sys__NR_clock_nanosleep),    // 248
// _____(__NR_swapcontext,       sys__NR_swapcontext),        // 249

// _____(__NR_tgkill,            sys__NR_tgkill),             // 250
// _____(__NR_utimes,            sys__NR_utimes),             // 251
// _____(__NR_statfs64,          sys__NR_statfs64),           // 252
// _____(__NR_fstatfs64,         sys__NR_fstatfs64),          // 253
// /* #define __NR_fadvise64_64    254     32bit only */

// _____(__NR_rtas,              sys__NR_rtas),               // 255
// /* Number 256 is reserved for sys_debug_setcontext */
// /* Number 257 is reserved for vserver */
// /* 258 currently unused */
// _____(__NR_mbind,             sys__NR_mbind),              // 259

// _____(__NR_get_mempolicy,     sys__NR_get_mempolicy),      // 260
// _____(__NR_set_mempolicy,     sys__NR_set_mempolicy),      // 261
// _____(__NR_mq_open,           sys__NR_mq_open),            // 262
// _____(__NR_mq_unlink,         sys__NR_mq_unlink),          // 263
// _____(__NR_mq_timedsend,      sys__NR_mq_timedsend),       // 264

// _____(__NR_mq_timedreceive,   sys__NR_mq_timedreceive),    // 265
// _____(__NR_mq_notify,         sys__NR_mq_notify),          // 266
// _____(__NR_mq_getsetattr,     sys__NR_mq_getsetattr),      // 267
// _____(__NR_kexec_load,        sys__NR_kexec_load),         // 268
// _____(__NR_add_key,           sys__NR_add_key),            // 269

// _____(__NR_request_key,       sys__NR_request_key),        // 270
// _____(__NR_keyctl,            sys__NR_keyctl),             // 271
// _____(__NR_waitid,            sys__NR_waitid),             // 272
// _____(__NR_ioprio_set,        sys__NR_ioprio_set),         // 273
// _____(__NR_ioprio_get,        sys__NR_ioprio_get),         // 274

// _____(__NR_inotify_init,      sys__NR_inotify_init),       // 275
// _____(__NR_inotify_add_watch, sys__NR_inotify_add_watch),  // 276
// _____(__NR_inotify_rm_watch,  sys__NR_inotify_rm_watch)    // 277
};

const UInt ML_(syscall_table_size) = 
            sizeof(ML_(syscall_table)) / sizeof(ML_(syscall_table)[0]);

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
