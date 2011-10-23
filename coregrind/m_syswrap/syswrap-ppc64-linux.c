
/*--------------------------------------------------------------------*/
/*--- Platform-specific syscalls stuff.      syswrap-ppc64-linux.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2005-2011 Nicholas Nethercote <njn@valgrind.org>
   Copyright (C) 2005-2011 Cerion Armour-Brown <cerion@open-works.co.uk>

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

#if defined(VGP_ppc64_linux)

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

extern
ULong do_syscall_clone_ppc64_linux ( Word (*fn)(void *), 
                                     void* stack, 
                                     Int   flags, 
                                     void* arg,
                                     Int*  child_tid, 
                                     Int*  parent_tid, 
                                     void/*vki_modify_ldt_t*/ * );
asm(
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
   VG_TRACK ( pre_thread_ll_create, ptid, ctid );

   if (flags & VKI_CLONE_SETTLS) {
      if (debug)
         VG_(printf)("clone child has SETTLS: tls at %#lx\n", child_tls);
      ctst->arch.vex.guest_GPR13 = child_tls;
   }

   flags &= ~VKI_CLONE_SETTLS;

   /* start the thread with everything blocked */
   VG_(sigprocmask)(VKI_SIG_SETMASK, &blockall, &savedmask);

   /* Create the new thread */
   word64 = do_syscall_clone_ppc64_linux(
               ML_(start_thread_NORETURN),
               stack, flags, &VG_(threads)[ctid],
               child_tidptr, parent_tidptr, NULL
            );

   /* Low half word64 is syscall return value.  Hi half is
      the entire CR, from which we need to extract CR0.SO. */
   /* VG_(printf)("word64 = 0x%llx\n", word64); */
   res = VG_(mk_SysRes_ppc64_linux)( 
            /*val*/(UInt)(word64 & 0xFFFFFFFFULL), 
            /*errflag*/ (UInt)((word64 >> (32+28)) & 1)
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
   PRE/POST wrappers for ppc64/Linux-specific syscalls
   ------------------------------------------------------------------ */

#define PRE(name)       DEFN_PRE_TEMPLATE(ppc64_linux, name)
#define POST(name)      DEFN_POST_TEMPLATE(ppc64_linux, name)

/* Add prototypes for the wrappers declared here, so that gcc doesn't
   harass us for not having prototypes.  Really this is a kludge --
   the right thing to do is to make these wrappers 'static' since they
   aren't visible outside this file, but that requires even more macro
   magic. */

DECL_TEMPLATE(ppc64_linux, sys_socketcall);
DECL_TEMPLATE(ppc64_linux, sys_mmap);
//zz DECL_TEMPLATE(ppc64_linux, sys_mmap2);
//zz DECL_TEMPLATE(ppc64_linux, sys_stat64);
//zz DECL_TEMPLATE(ppc64_linux, sys_lstat64);
//zz DECL_TEMPLATE(ppc64_linux, sys_fstat64);
DECL_TEMPLATE(ppc64_linux, sys_ipc);
DECL_TEMPLATE(ppc64_linux, sys_clone);
//zz DECL_TEMPLATE(ppc64_linux, sys_sigreturn);
DECL_TEMPLATE(ppc64_linux, sys_rt_sigreturn);
DECL_TEMPLATE(ppc64_linux, sys_fadvise64);

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
     /* int accept4(int s, struct sockaddr *addr, int *addrlen, int flags); */
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
     ML_(generic_PRE_sys_sendmsg)( tid, ARG2_0, ARG2_1 );
     break;
   }

   case VKI_SYS_RECVMSG: {
     /* int recvmsg(int s, struct msghdr *msg, int flags); */

     /* this causes warnings, and I don't get why. glibc bug?
      * (after all it's glibc providing the arguments array)
       PRE_MEM_READ("socketcall.recvmsg(args)", ARG2, 3*sizeof(Addr) );
     */
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
  case VKI_SYS_ACCEPT4:
    /* int accept(int s, struct sockaddr *addr, int *addrlen); */
    /* int accept4(int s, struct sockaddr *addr, int *addrlen, int flags); */
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

PRE(sys_fadvise64)
{
   PRINT("sys_fadvise64 ( %ld, %ld, %lu, %ld )", ARG1,ARG2,ARG3,ARG4);
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
   GENXY(__NR_close,             sys_close),              //   6
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
// _____(__NR_ptrace,            sys_ptrace),             //  26
   GENX_(__NR_alarm,             sys_alarm),              //  27
// _____(__NR_oldfstat,          sys_oldfstat),           //  28
   GENX_(__NR_pause,             sys_pause),              //  29

   LINX_(__NR_utime,             sys_utime),              //  30
// _____(__NR_stty,              sys_stty),               //  31
// _____(__NR_gtty,              sys_gtty),               //  32
   GENX_(__NR_access,            sys_access),             //  33
// _____(__NR_nice,              sys_nice),               //  34

// _____(__NR_ftime,             sys_ftime),              //  35
// _____(__NR_sync,              sys_sync),               //  36
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
// _____(__NR_acct,              sys_acct),               //  51
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

   GENX_(__NR_readlink,          sys_readlink),           //  85
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
// _____(__NR_getpriority,       sys_getpriority),        //  96
// _____(__NR_setpriority,       sys_setpriority),        //  97
// _____(__NR_profil,            sys_profil),             //  98
   GENXY(__NR_statfs,            sys_statfs),             //  99

   GENXY(__NR_fstatfs,           sys_fstatfs),            // 100
// _____(__NR_ioperm,            sys_ioperm),             // 101
   PLAXY(__NR_socketcall,        sys_socketcall),         // 102
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
   PLAXY(__NR_ipc,               sys_ipc),                // 117
   GENX_(__NR_fsync,             sys_fsync),              // 118
// _____(__NR_sigreturn,         sys_sigreturn),          // 119

   PLAX_(__NR_clone,             sys_clone),              // 120
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
// _____(__NR_quotactl,          sys_quotactl),           // 131
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
// _____(__NR_getsid,            sys_getsid),             // 147
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
// _____(__NR_prctl,             sys_prctl),              // 171
   PLAX_(__NR_rt_sigreturn,      sys_rt_sigreturn),       // 172
   LINXY(__NR_rt_sigaction,      sys_rt_sigaction),       // 173
   LINXY(__NR_rt_sigprocmask,    sys_rt_sigprocmask),     // 174

// _____(__NR_rt_sigpending,     sys_rt_sigpending),      // 175
   LINXY(__NR_rt_sigtimedwait,   sys_rt_sigtimedwait),    // 176
   LINXY(__NR_rt_sigqueueinfo,   sys_rt_sigqueueinfo),    // 177
// _____(__NR_rt_sigsuspend,     sys_rt_sigsuspend),      // 178
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
// _____(__NR_pivot_root,        sys_pivot_root),         // 203
   LINXY(__NR_fcntl64,           sys_fcntl64),            // 204 !!!!?? 32bit only */

   GENX_(__NR_madvise,           sys_madvise),            // 205
// _____(__NR_mincore,           sys_mincore),            // 206
   LINX_(__NR_gettid,            sys_gettid),             // 207
// _____(__NR_tkill,             sys_tkill),              // 208
// _____(__NR_setxattr,          sys_setxattr),           // 209

// _____(__NR_lsetxattr,         sys_lsetxattr),          // 210
// _____(__NR_fsetxattr,         sys_fsetxattr),          // 211
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
// _____(__NR_statfs64,          sys_statfs64),           // 252
// _____(__NR_fstatfs64,         sys_fstatfs64),          // 253
// /* #define __NR_fadvise64_64    254     32bit only */

// _____(__NR_rtas,              sys_rtas),               // 255
// /* Number 256 is reserved for sys_debug_setcontext */
// /* Number 257 is reserved for vserver */
// /* 258 currently unused */
// _____(__NR_mbind,             sys_mbind),              // 259

// _____(__NR_get_mempolicy,     sys_get_mempolicy),      // 260
// _____(__NR_set_mempolicy,     sys_set_mempolicy),      // 261
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
// _____(__NR_waitid,            sys_waitid),             // 272
   LINX_(__NR_ioprio_set,        sys_ioprio_set),         // 273
   LINX_(__NR_ioprio_get,        sys_ioprio_get),         // 274

   LINX_(__NR_inotify_init,  sys_inotify_init),           // 275
   LINX_(__NR_inotify_add_watch,  sys_inotify_add_watch), // 276
   LINX_(__NR_inotify_rm_watch,   sys_inotify_rm_watch),  // 277

   LINX_(__NR_pselect6,          sys_pselect6),           // 280
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
   LINX_(__NR_readlinkat,        sys_readlinkat),         // 296
   LINX_(__NR_fchmodat,          sys_fchmodat),           // 297
   LINX_(__NR_faccessat,         sys_faccessat),          // 298
   LINX_(__NR_set_robust_list,   sys_set_robust_list),    // 299
   LINXY(__NR_get_robust_list,   sys_get_robust_list),    // 300
//   LINX_(__NR_move_pages,        sys_ni_syscall),        // 301
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
   LINXY(__NR_rt_tgsigqueueinfo, sys_rt_tgsigqueueinfo) // 322
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

#endif // defined(VGP_ppc64_linux)

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
