
/*--------------------------------------------------------------------*/
/*--- Platform-specific syscalls stuff.      syswrap-arm-linux.c -----*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2012 Nicholas Nethercote
      njn@valgrind.org
   Copyright (C) 2008-2012 Evan Geller
      gaze@bea.ms

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

#if defined(VGP_arm_linux)

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
#include "pub_core_transtab.h"      // VG_(discard_translations)

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
//    r0 = stack
//    r1 = retaddr
//    r2 = f
//    r3 = arg1
asm(
".text\n"
".globl vgModuleLocal_call_on_new_stack_0_1\n"
"vgModuleLocal_call_on_new_stack_0_1:\n"
"   mov    sp,r0\n\t" /* Stack pointer */
"   mov    lr,r1\n\t" /* Return address */
"   mov    r0,r3\n\t" /* First argument */
"   push   {r2}\n\t"  /* So we can ret to the new dest */
"   mov    r1, #0\n\t" /* Clear our GPRs */
"   mov    r2, #0\n\t"
"   mov    r3, #0\n\t"
"   mov    r4, #0\n\t"
"   mov    r5, #0\n\t"
"   mov    r6, #0\n\t"
"   mov    r7, #0\n\t"
"   mov    r8, #0\n\t"
"   mov    r9, #0\n\t"
"   mov    r10, #0\n\t"
"   mov    r11, #0\n\t"
"   mov    r12, #0\n\t"
"   pop    {pc}\n\t"  /* Herrre we go! */
".previous\n"
);


#define __NR_CLONE        VG_STRINGIFY(__NR_clone)
#define __NR_EXIT         VG_STRINGIFY(__NR_exit)

extern
ULong do_syscall_clone_arm_linux   ( Word (*fn)(void *), 
                                     void* stack, 
                                     Int   flags, 
                                     void* arg,
                                     Int*  child_tid,
                                     Int*  parent_tid,
                                     void* tls );
asm(
".text\n"
".globl do_syscall_clone_arm_linux\n"
"do_syscall_clone_arm_linux:\n"

/*Setup child stack */
"   str     r0, [r1, #-4]!\n"
"   str     r3, [r1, #-4]!\n"
"   push {r4,r7}\n" 
"   mov r0, r2\n" /* arg1: flags */
/* r1 (arg2) is already our child's stack */
"   ldr r2, [sp, #12]\n" // parent tid
"   ldr r3, [sp, #16]\n" // tls
"   ldr r4, [sp, #8]\n" // Child tid
"   mov r7, #"__NR_CLONE"\n"
"   svc 0x00000000\n"
"   cmp r0, #0\n"
"   beq 1f\n"

/* Parent */
"   pop {r4,r7}\n"
"   bx lr\n"

"1:\n" /*child*/
"   mov     lr, pc\n"
"   pop     {r0,pc}\n"
/* Retval from child is already in r0 */
"   mov r7, #"__NR_EXIT"\n"
"   svc 0x00000000\n"
/* Urh.. why did exit return? */
"   .long 0\n"
"   .previous\n"
);

#undef __NR_CLONE
#undef __NR_EXIT

// forward declarations
static void setup_child ( ThreadArchState*, ThreadArchState* );
static void assign_guest_tls(ThreadId ctid, Addr tlsptr);
static SysRes sys_set_tls ( ThreadId tid, Addr tlsptr );
            
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

   ThreadId ctid = VG_(alloc_ThreadState)();
   ThreadState* ptst = VG_(get_ThreadState)(ptid);
   ThreadState* ctst = VG_(get_ThreadState)(ctid);
   UInt r0;
   UWord *stack;
   NSegment const* seg;
   SysRes res;
   vki_sigset_t blockall, savedmask;

   VG_(sigfillset)(&blockall);

   vg_assert(VG_(is_running_thread)(ptid));
   vg_assert(VG_(is_valid_tid)(ctid));

   stack = (UWord*)ML_(allocstack)(ctid);

   if(stack == NULL) {
      res = VG_(mk_SysRes_Error)( VKI_ENOMEM );
      goto out;
   }

   setup_child( &ctst->arch, &ptst->arch );

   ctst->arch.vex.guest_R0 = 0;
   if(sp != 0)
      ctst->arch.vex.guest_R13 = sp;

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

   seg = VG_(am_find_nsegment)((Addr)sp);
   if (seg && seg->kind != SkResvn) {
      ctst->client_stack_highest_word = (Addr)VG_PGROUNDUP(sp);
      ctst->client_stack_szB = ctst->client_stack_highest_word - seg->start;
   
      VG_(register_stack)(seg->start, ctst->client_stack_highest_word);
   
      if (debug)
         VG_(printf)("tid %d: guessed client stack range %#lx-%#lx\n",
         ctid, seg->start, VG_PGROUNDUP(sp));
   } else {
      VG_(message)(Vg_UserMsg, "!? New thread %d starts with sp+%#lx) unmapped\n", ctid, sp);
      ctst->client_stack_szB  = 0;
   }

   vg_assert(VG_(owns_BigLock_LL)(ptid));
   VG_TRACK ( pre_thread_ll_create, ptid, ctid );

   if (flags & VKI_CLONE_SETTLS) {
      /* Just assign the tls pointer in the guest TPIDRURO. */
      assign_guest_tls(ctid, child_tls);
   }
    
   flags &= ~VKI_CLONE_SETTLS;

   VG_(sigprocmask)(VKI_SIG_SETMASK, &blockall, &savedmask);

   r0 = do_syscall_clone_arm_linux(
      ML_(start_thread_NORETURN), stack, flags, &VG_(threads)[ctid],
      child_tidptr, parent_tidptr, NULL
   );
   //VG_(printf)("AFTER SYSCALL, %x and %x  CHILD: %d PARENT: %d\n",child_tidptr, parent_tidptr,*child_tidptr,*parent_tidptr);
    
   res = VG_(mk_SysRes_arm_linux)( r0 );

   VG_(sigprocmask)(VKI_SIG_SETMASK, &savedmask, NULL);

out:
   if (sr_isError(res)) {
      VG_(cleanup_thread)(&ctst->arch);
      ctst->status = VgTs_Empty;
      VG_TRACK( pre_thread_ll_exit, ctid );
   }

   return res;
}



/* ---------------------------------------------------------------------
   More thread stuff
   ------------------------------------------------------------------ */

// ARM doesn't have any architecture specific thread stuff that
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
   VG_(threads)[tid].arch.vex.guest_TPIDRURO = tlsptr;
}

/* Assigns tlsptr to the guest TPIDRURO.
   If needed for the specific hardware, really executes
   the set_tls syscall.
*/
static SysRes sys_set_tls ( ThreadId tid, Addr tlsptr )
{
   assign_guest_tls(tid, tlsptr);
#if defined(ANDROID_HARDWARE_emulator)
   /* Android emulator does not provide an hw tls register.
      So, the tls register is emulated by the kernel.
      This emulated value is set by the __NR_ARM_set_tls syscall.
      The emulated value must be read by the kernel helper function
      located at 0xffff0fe0.
      
      The emulated tlsptr is located at 0xffff0ff0
      (so slightly after the kernel helper function).
      Note that applications are not supposed to read this directly.
      
      For compatibility : if there is a hw tls register, the kernel
      will put at 0xffff0fe0 the instructions to read it, so
      as to have old applications calling the kernel helper
      working properly.

      For having emulated guest TLS working correctly with
      Valgrind, it is needed to execute the syscall to set
      the emulated TLS value in addition to the assignment
      of TPIDRURO.

      Note: the below means that if we need thread local storage
      for Valgrind host, then there will be a conflict between
      the need of the guest tls and of the host tls.
      If all the guest code would cleanly call 0xffff0fe0,
      then we might maybe intercept this. However, at least
      __libc_preinit reads directly 0xffff0ff0.
   */
   /* ??? might call the below if auxv->u.a_val & VKI_HWCAP_TLS ???
      Unclear if real hardware having tls hw register sets
      VKI_HWCAP_TLS. */
   return VG_(do_syscall1) (__NR_ARM_set_tls, tlsptr);
#else
   return VG_(mk_SysRes_Success)( 0 );
#endif
}

/* ---------------------------------------------------------------------
   PRE/POST wrappers for arm/Linux-specific syscalls
   ------------------------------------------------------------------ */

#define PRE(name)       DEFN_PRE_TEMPLATE(arm_linux, name)
#define POST(name)      DEFN_POST_TEMPLATE(arm_linux, name)

/* Add prototypes for the wrappers declared here, so that gcc doesn't
   harass us for not having prototypes.  Really this is a kludge --
   the right thing to do is to make these wrappers 'static' since they
   aren't visible outside this file, but that requires even more macro
   magic. */

DECL_TEMPLATE(arm_linux, sys_socketcall);
DECL_TEMPLATE(arm_linux, sys_socket);
DECL_TEMPLATE(arm_linux, sys_setsockopt);
DECL_TEMPLATE(arm_linux, sys_getsockopt);
DECL_TEMPLATE(arm_linux, sys_connect);
DECL_TEMPLATE(arm_linux, sys_accept);
DECL_TEMPLATE(arm_linux, sys_accept4);
DECL_TEMPLATE(arm_linux, sys_sendto);
DECL_TEMPLATE(arm_linux, sys_recvfrom);
//XXX: Semaphore code ripped from AMD64.
DECL_TEMPLATE(arm_linux, sys_semget);
DECL_TEMPLATE(arm_linux, sys_semop);
DECL_TEMPLATE(arm_linux, sys_semctl);
DECL_TEMPLATE(arm_linux, sys_semtimedop);
//XXX: Shared memory code ripped from AMD64
//
DECL_TEMPLATE(arm_linux, wrap_sys_shmat);
DECL_TEMPLATE(arm_linux, sys_shmget);
DECL_TEMPLATE(arm_linux, sys_shmdt);
DECL_TEMPLATE(arm_linux, sys_shmctl);
DECL_TEMPLATE(arm_linux, sys_sendmsg);
DECL_TEMPLATE(arm_linux, sys_recvmsg);
//msg* code from AMD64
DECL_TEMPLATE(arm_linux, sys_msgget);
DECL_TEMPLATE(arm_linux, sys_msgrcv);
DECL_TEMPLATE(arm_linux, sys_msgsnd);
DECL_TEMPLATE(arm_linux, sys_msgctl);
DECL_TEMPLATE(arm_linux, sys_shutdown);
DECL_TEMPLATE(arm_linux, sys_bind);
DECL_TEMPLATE(arm_linux, sys_listen);
DECL_TEMPLATE(arm_linux, sys_getsockname);
DECL_TEMPLATE(arm_linux, sys_getpeername);
DECL_TEMPLATE(arm_linux, sys_socketpair);
DECL_TEMPLATE(arm_linux, sys_send);
DECL_TEMPLATE(arm_linux, sys_recv);
DECL_TEMPLATE(arm_linux, sys_mmap2);
DECL_TEMPLATE(arm_linux, sys_stat64);
DECL_TEMPLATE(arm_linux, sys_lstat64);
DECL_TEMPLATE(arm_linux, sys_fstatat64);
DECL_TEMPLATE(arm_linux, sys_fstat64);
DECL_TEMPLATE(arm_linux, sys_clone);
DECL_TEMPLATE(arm_linux, sys_sigreturn);
DECL_TEMPLATE(arm_linux, sys_rt_sigreturn);
DECL_TEMPLATE(arm_linux, sys_sigsuspend);
DECL_TEMPLATE(arm_linux, sys_set_tls);
DECL_TEMPLATE(arm_linux, sys_cacheflush);
DECL_TEMPLATE(arm_linux, sys_ptrace);

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
      /*int accept(int s, struct sockaddr *add, int *addrlen, int flags)*/
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
     VG_(message)(Vg_DebugMsg,"Warning: unhandled socketcall 0x%lx",ARG1);
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
    ML_(generic_POST_sys_recvmsg)( tid, "msg", (struct vki_msghdr *)ARG2_1, RES );
    break;

  default:
    VG_(message)(Vg_DebugMsg,"FATAL: unhandled socketcall 0x%lx",ARG1);
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

PRE(sys_socket)
{
   PRINT("sys_socket ( %ld, %ld, %ld )",ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "socket", int, domain, int, type, int, protocol);
}
POST(sys_socket)
{
   SysRes r;
   vg_assert(SUCCESS);
   r = ML_(generic_POST_sys_socket)(tid, VG_(mk_SysRes_Success)(RES));
   SET_STATUS_from_SysRes(r);
}

PRE(sys_setsockopt)
{
   PRINT("sys_setsockopt ( %ld, %ld, %ld, %#lx, %ld )",ARG1,ARG2,ARG3,ARG4,ARG5);
   PRE_REG_READ5(long, "setsockopt",
                 int, s, int, level, int, optname,
                 const void *, optval, int, optlen);
   ML_(generic_PRE_sys_setsockopt)(tid, ARG1,ARG2,ARG3,ARG4,ARG5);
}

PRE(sys_getsockopt)
{
   PRINT("sys_getsockopt ( %ld, %ld, %ld, %#lx, %#lx )",ARG1,ARG2,ARG3,ARG4,ARG5);
   PRE_REG_READ5(long, "getsockopt",
                 int, s, int, level, int, optname,
                 void *, optval, int, *optlen);
   ML_(linux_PRE_sys_getsockopt)(tid, ARG1,ARG2,ARG3,ARG4,ARG5);
}
POST(sys_getsockopt)
{
   vg_assert(SUCCESS);
   ML_(linux_POST_sys_getsockopt)(tid, VG_(mk_SysRes_Success)(RES),
                                         ARG1,ARG2,ARG3,ARG4,ARG5);
}

PRE(sys_connect)
{
   *flags |= SfMayBlock;
   PRINT("sys_connect ( %ld, %#lx, %ld )",ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "connect",
                 int, sockfd, struct sockaddr *, serv_addr, int, addrlen);
   ML_(generic_PRE_sys_connect)(tid, ARG1,ARG2,ARG3);
}

PRE(sys_accept)
{
   *flags |= SfMayBlock;
   PRINT("sys_accept ( %ld, %#lx, %ld )",ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "accept",
                 int, s, struct sockaddr *, addr, int, *addrlen);
   ML_(generic_PRE_sys_accept)(tid, ARG1,ARG2,ARG3);
}
POST(sys_accept)
{
   SysRes r;
   vg_assert(SUCCESS);
   r = ML_(generic_POST_sys_accept)(tid, VG_(mk_SysRes_Success)(RES),
                                         ARG1,ARG2,ARG3);
   SET_STATUS_from_SysRes(r);
}

PRE(sys_accept4)
{
   *flags |= SfMayBlock;
   PRINT("sys_accept4 ( %ld, %#lx, %ld, %ld )",ARG1,ARG2,ARG3,ARG4);
   PRE_REG_READ4(long, "accept4",
                 int, s, struct sockaddr *, addr, int, *addrlen, int, flags);
   ML_(generic_PRE_sys_accept)(tid, ARG1,ARG2,ARG3);
}
POST(sys_accept4)
{
   SysRes r;
   vg_assert(SUCCESS);
   r = ML_(generic_POST_sys_accept)(tid, VG_(mk_SysRes_Success)(RES),
                                         ARG1,ARG2,ARG3);
   SET_STATUS_from_SysRes(r);
}

PRE(sys_sendto)
{
   *flags |= SfMayBlock;
   PRINT("sys_sendto ( %ld, %#lx, %ld, %lu, %#lx, %ld )",ARG1,ARG2,ARG3,ARG4,ARG5,ARG6);
   PRE_REG_READ6(long, "sendto",
                 int, s, const void *, msg, int, len, 
                 unsigned int, flags, 
                 const struct sockaddr *, to, int, tolen);
   ML_(generic_PRE_sys_sendto)(tid, ARG1,ARG2,ARG3,ARG4,ARG5,ARG6);
}

PRE(sys_recvfrom)
{
   *flags |= SfMayBlock;
   PRINT("sys_recvfrom ( %ld, %#lx, %ld, %lu, %#lx, %#lx )",ARG1,ARG2,ARG3,ARG4,ARG5,ARG6);
   PRE_REG_READ6(long, "recvfrom",
                 int, s, void *, buf, int, len, unsigned int, flags,
                 struct sockaddr *, from, int *, fromlen);
   ML_(generic_PRE_sys_recvfrom)(tid, ARG1,ARG2,ARG3,ARG4,ARG5,ARG6);
}
POST(sys_recvfrom)
{
   vg_assert(SUCCESS);
   ML_(generic_POST_sys_recvfrom)(tid, VG_(mk_SysRes_Success)(RES),
                                       ARG1,ARG2,ARG3,ARG4,ARG5,ARG6);
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

//XXX: Semaphore code ripped from AMD64.
PRE(sys_semget)
{
   PRINT("sys_semget ( %ld, %ld, %ld )",ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "semget", vki_key_t, key, int, nsems, int, semflg);
}

PRE(sys_semop)
{
   *flags |= SfMayBlock;
   PRINT("sys_semop ( %ld, %#lx, %lu )",ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "semop",
                 int, semid, struct sembuf *, sops, unsigned, nsoops);
   ML_(generic_PRE_sys_semop)(tid, ARG1,ARG2,ARG3);
}

PRE(sys_semctl)
{
   switch (ARG3 & ~VKI_IPC_64) {
   case VKI_IPC_INFO:
   case VKI_SEM_INFO:
      PRINT("sys_semctl ( %ld, %ld, %ld, %#lx )",ARG1,ARG2,ARG3,ARG4);
      PRE_REG_READ4(long, "semctl",
                    int, semid, int, semnum, int, cmd, struct seminfo *, arg);
      break;
   case VKI_IPC_STAT:
   case VKI_SEM_STAT:
   case VKI_IPC_SET:
      PRINT("sys_semctl ( %ld, %ld, %ld, %#lx )",ARG1,ARG2,ARG3,ARG4);
      PRE_REG_READ4(long, "semctl",
                    int, semid, int, semnum, int, cmd, struct semid_ds *, arg);
      break;
   case VKI_GETALL:
   case VKI_SETALL:
      PRINT("sys_semctl ( %ld, %ld, %ld, %#lx )",ARG1,ARG2,ARG3,ARG4);
      PRE_REG_READ4(long, "semctl",
                    int, semid, int, semnum, int, cmd, unsigned short *, arg);
      break;
   default:
      PRINT("sys_semctl ( %ld, %ld, %ld )",ARG1,ARG2,ARG3);
      PRE_REG_READ3(long, "semctl",
                    int, semid, int, semnum, int, cmd);
      break;
   }
   ML_(generic_PRE_sys_semctl)(tid, ARG1,ARG2,ARG3,ARG4);
}

POST(sys_semctl)
{
   ML_(generic_POST_sys_semctl)(tid, RES,ARG1,ARG2,ARG3,ARG4);
}

PRE(sys_semtimedop)
{
   *flags |= SfMayBlock;
   PRINT("sys_semtimedop ( %ld, %#lx, %lu, %#lx )",ARG1,ARG2,ARG3,ARG4);
   PRE_REG_READ4(long, "semtimedop",
                 int, semid, struct sembuf *, sops, unsigned, nsoops,
                 struct timespec *, timeout);
   ML_(generic_PRE_sys_semtimedop)(tid, ARG1,ARG2,ARG3,ARG4);
}

//amd64
PRE(sys_msgget)
{
   PRINT("sys_msgget ( %ld, %ld )",ARG1,ARG2);
   PRE_REG_READ2(long, "msgget", vki_key_t, key, int, msgflg);
}

PRE(sys_msgsnd)
{
   PRINT("sys_msgsnd ( %ld, %#lx, %ld, %ld )",ARG1,ARG2,ARG3,ARG4);
   PRE_REG_READ4(long, "msgsnd",
                 int, msqid, struct msgbuf *, msgp, vki_size_t, msgsz, int, msgflg);
   ML_(linux_PRE_sys_msgsnd)(tid, ARG1,ARG2,ARG3,ARG4);
   if ((ARG4 & VKI_IPC_NOWAIT) == 0)
      *flags |= SfMayBlock;
}

PRE(sys_msgrcv)
{
   PRINT("sys_msgrcv ( %ld, %#lx, %ld, %ld, %ld )",ARG1,ARG2,ARG3,ARG4,ARG5);
   PRE_REG_READ5(long, "msgrcv",
                 int, msqid, struct msgbuf *, msgp, vki_size_t, msgsz,
                 long, msgytp, int, msgflg);
   ML_(linux_PRE_sys_msgrcv)(tid, ARG1,ARG2,ARG3,ARG4,ARG5);
   if ((ARG4 & VKI_IPC_NOWAIT) == 0)
      *flags |= SfMayBlock;
}
POST(sys_msgrcv)
{
   ML_(linux_POST_sys_msgrcv)(tid, RES,ARG1,ARG2,ARG3,ARG4,ARG5);
}


PRE(sys_msgctl)
{
   PRINT("sys_msgctl ( %ld, %ld, %#lx )",ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "msgctl",
                 int, msqid, int, cmd, struct msqid_ds *, buf);
   ML_(linux_PRE_sys_msgctl)(tid, ARG1,ARG2,ARG3);
}
POST(sys_msgctl)
{
   ML_(linux_POST_sys_msgctl)(tid, RES,ARG1,ARG2,ARG3);
}

//shared memory code from AMD64
PRE(sys_shmget)
{
   PRINT("sys_shmget ( %ld, %ld, %ld )",ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "shmget", vki_key_t, key, vki_size_t, size, int, shmflg);
}

PRE(wrap_sys_shmat)
{
   UWord arg2tmp;
   PRINT("wrap_sys_shmat ( %ld, %#lx, %ld )",ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "shmat",
                 int, shmid, const void *, shmaddr, int, shmflg);
   /* Round the attach address down to an VKI_SHMLBA boundary if the
      client requested rounding.  See #222545.  This is necessary only
      on arm-linux because VKI_SHMLBA is 4 * VKI_PAGE size; on all
      other linux targets it is the same as the page size. */
   if (ARG3 & VKI_SHM_RND)
      ARG2 = VG_ROUNDDN(ARG2, VKI_SHMLBA);
   arg2tmp = ML_(generic_PRE_sys_shmat)(tid, ARG1,ARG2,ARG3);
   if (arg2tmp == 0)
      SET_STATUS_Failure( VKI_EINVAL );
   else
      ARG2 = arg2tmp;
}

POST(wrap_sys_shmat)
{
   ML_(generic_POST_sys_shmat)(tid, RES,ARG1,ARG2,ARG3);
}

PRE(sys_shmdt)
{
   PRINT("sys_shmdt ( %#lx )",ARG1);
   PRE_REG_READ1(long, "shmdt", const void *, shmaddr);
   if (!ML_(generic_PRE_sys_shmdt)(tid, ARG1))
      SET_STATUS_Failure( VKI_EINVAL );
}

POST(sys_shmdt)
{
   ML_(generic_POST_sys_shmdt)(tid, RES,ARG1);
}

PRE(sys_shmctl)
{
   PRINT("sys_shmctl ( %ld, %ld, %#lx )",ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "shmctl",
                 int, shmid, int, cmd, struct shmid_ds *, buf);
   ML_(generic_PRE_sys_shmctl)(tid, ARG1,ARG2,ARG3);
}

POST(sys_shmctl)
{
   ML_(generic_POST_sys_shmctl)(tid, RES,ARG1,ARG2,ARG3);
}

PRE(sys_shutdown)
{
   *flags |= SfMayBlock;
   PRINT("sys_shutdown ( %ld, %ld )",ARG1,ARG2);
   PRE_REG_READ2(int, "shutdown", int, s, int, how);
}

PRE(sys_bind)
{
   PRINT("sys_bind ( %ld, %#lx, %ld )",ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "bind",
                 int, sockfd, struct sockaddr *, my_addr, int, addrlen);
   ML_(generic_PRE_sys_bind)(tid, ARG1,ARG2,ARG3);
}

PRE(sys_listen)
{
   PRINT("sys_listen ( %ld, %ld )",ARG1,ARG2);
   PRE_REG_READ2(long, "listen", int, s, int, backlog);
}

PRE(sys_getsockname)
{
   PRINT("sys_getsockname ( %ld, %#lx, %#lx )",ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "getsockname",
                 int, s, struct sockaddr *, name, int *, namelen);
   ML_(generic_PRE_sys_getsockname)(tid, ARG1,ARG2,ARG3);
}
POST(sys_getsockname)
{
   vg_assert(SUCCESS);
   ML_(generic_POST_sys_getsockname)(tid, VG_(mk_SysRes_Success)(RES),
                                          ARG1,ARG2,ARG3);
}

PRE(sys_getpeername)
{
   PRINT("sys_getpeername ( %ld, %#lx, %#lx )",ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "getpeername",
                 int, s, struct sockaddr *, name, int *, namelen);
   ML_(generic_PRE_sys_getpeername)(tid, ARG1,ARG2,ARG3);
}
POST(sys_getpeername)
{
   vg_assert(SUCCESS);
   ML_(generic_POST_sys_getpeername)(tid, VG_(mk_SysRes_Success)(RES),
                                          ARG1,ARG2,ARG3);
}

PRE(sys_socketpair)
{
   PRINT("sys_socketpair ( %ld, %ld, %ld, %#lx )",ARG1,ARG2,ARG3,ARG4);
   PRE_REG_READ4(long, "socketpair",
                 int, d, int, type, int, protocol, int*, sv);
   ML_(generic_PRE_sys_socketpair)(tid, ARG1,ARG2,ARG3,ARG4);
}
POST(sys_socketpair)
{
   vg_assert(SUCCESS);
   ML_(generic_POST_sys_socketpair)(tid, VG_(mk_SysRes_Success)(RES),
                                         ARG1,ARG2,ARG3,ARG4);
}

PRE(sys_send)
{
   *flags |= SfMayBlock;
   PRINT("sys_send ( %ld, %#lx, %ld, %lu )",ARG1,ARG2,ARG3,ARG4);
   PRE_REG_READ4(long, "send",
                 int, s, const void *, msg, int, len, 
                 unsigned int, flags);

   ML_(generic_PRE_sys_send)( tid, ARG1, ARG2, ARG3 );
}

PRE(sys_recv)
{
   *flags |= SfMayBlock;
   PRINT("sys_recv ( %ld, %#lx, %ld, %lu )",ARG1,ARG2,ARG3,ARG4);
   PRE_REG_READ4(long, "recv",
                 int, s, void *, buf, int, len, unsigned int, flags);
   ML_(generic_PRE_sys_recv)( tid, ARG1, ARG2, ARG3 );
}

POST(sys_recv)
{
   ML_(generic_POST_sys_recv)( tid, RES, ARG1, ARG2, ARG3 );
}

PRE(sys_mmap2)
{
   SysRes r;

   // Exactly like old_mmap() except:
   //  - all 6 args are passed in regs, rather than in a memory-block.
   //  - the file offset is specified in pagesize units rather than bytes,
   //    so that it can be used for files bigger than 2^32 bytes.
   // pagesize or 4K-size units in offset?  For ppc32/64-linux, this is
   // 4K-sized.  Assert that the page size is 4K here for safety.
   vg_assert(VKI_PAGE_SIZE == 4096);
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

PRE(sys_stat64)
{
   PRINT("sys_stat64 ( %#lx(%s), %#lx )",ARG1,(char*)ARG1,ARG2);
   PRE_REG_READ2(long, "stat64", char *, file_name, struct stat64 *, buf);
   PRE_MEM_RASCIIZ( "stat64(file_name)", ARG1 );
   PRE_MEM_WRITE( "stat64(buf)", ARG2, sizeof(struct vki_stat64) );
}

POST(sys_stat64)
{
   POST_MEM_WRITE( ARG2, sizeof(struct vki_stat64) );
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
   if (ARG1 & (VKI_CLONE_CHILD_SETTID | VKI_CLONE_CHILD_CLEARTID)) {
      PRE_MEM_WRITE("clone(child_tidptr)", ARG5, sizeof(Int));
      if (!VG_(am_is_valid_for_client)(ARG5, sizeof(Int), 
                                             VKI_PROT_WRITE)) {
         SET_STATUS_Failure( VKI_EFAULT );
         return;
      }
   }
   if (ARG1 & VKI_CLONE_SETTLS) {
      PRE_MEM_READ("clone(tls_user_desc)", ARG4, sizeof(vki_modify_ldt_t));
      if (!VG_(am_is_valid_for_client)(ARG4, sizeof(vki_modify_ldt_t), 
                                             VKI_PROT_READ)) {
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
                  (Addr)ARG2,   /* child ESP */
                  (Int *)ARG3,  /* parent_tidptr */
                  (Int *)ARG5,  /* child_tidptr */
                  (Addr)ARG4)); /* set_tls */
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
      VG_(message)(Vg_UserMsg, "");
      VG_(message)(Vg_UserMsg, "Unsupported clone() flags: 0x%lx", ARG1);
      VG_(message)(Vg_UserMsg, "");
      VG_(message)(Vg_UserMsg, "The only supported clone() uses are:");
      VG_(message)(Vg_UserMsg, " - via a threads library (LinuxThreads or NPTL)");
      VG_(message)(Vg_UserMsg, " - via the implementation of fork or vfork");
      VG_(message)(Vg_UserMsg, " - for the Quadrics Elan3 user-space driver");
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

   PRINT("sys_sigreturn ( )");

   vg_assert(VG_(is_valid_tid)(tid));
   vg_assert(tid >= 1 && tid < VG_N_THREADS);
   vg_assert(VG_(is_running_thread)(tid));

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

/* NB: clone of x86-linux version, and ppc32-linux has an almost
   identical one. */
PRE(sys_sigsuspend)
{
   /* The C library interface to sigsuspend just takes a pointer to
      a signal mask but this system call has three arguments - the first
      two don't appear to be used by the kernel and are always passed as
      zero by glibc and the third is the first word of the signal mask
      so only 32 signals are supported.
     
      In fact glibc normally uses rt_sigsuspend if it is available as
      that takes a pointer to the signal mask so supports more signals.
    */
   *flags |= SfMayBlock;
   PRINT("sys_sigsuspend ( %ld, %ld, %ld )", ARG1,ARG2,ARG3 );
   PRE_REG_READ3(int, "sigsuspend",
                 int, history0, int, history1,
                 vki_old_sigset_t, mask);
}

/* Very much ARM specific */

PRE(sys_set_tls)
{
   PRINT("set_tls (%lx)",ARG1);
   PRE_REG_READ1(long, "set_tls", unsigned long, addr);

   SET_STATUS_from_SysRes( sys_set_tls( tid, ARG1 ) );
}

PRE(sys_cacheflush)
{
   PRINT("cacheflush (%lx, %#lx, %#lx)",ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "cacheflush", void*, addrlow,void*, addrhigh,int, flags);
   VG_(discard_translations)( (Addr64)ARG1,
                              ((ULong)ARG2) - ((ULong)ARG1) + 1ULL/*paranoia*/,
                              "PRE(sys_cacheflush)" );
   SET_STATUS_Success(0);
}

// ARG3 is only used for pointers into the traced process's address
// space and for offsets into the traced process's struct
// user_regs_struct. It is never a pointer into this process's memory
// space, and we should therefore not check anything it points to.
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
   case VKI_PTRACE_GETREGS:
      PRE_MEM_WRITE( "ptrace(getregs)", ARG4, 
		     sizeof (struct vki_user_regs_struct));
      break;
   case VKI_PTRACE_GETFPREGS:
      PRE_MEM_WRITE( "ptrace(getfpregs)", ARG4, 
		     sizeof (struct vki_user_fp));
      break;
   case VKI_PTRACE_GETWMMXREGS:
      PRE_MEM_WRITE( "ptrace(getwmmxregs)", ARG4, 
		     VKI_IWMMXT_SIZE);
      break;
   case VKI_PTRACE_GETCRUNCHREGS:
      PRE_MEM_WRITE( "ptrace(getcrunchregs)", ARG4, 
		     VKI_CRUNCH_SIZE);
      break;
   case VKI_PTRACE_GETVFPREGS:
      PRE_MEM_WRITE( "ptrace(getvfpregs)", ARG4, 
                     sizeof (struct vki_user_vfp) );
      break;
   case VKI_PTRACE_GETHBPREGS:
      PRE_MEM_WRITE( "ptrace(gethbpregs)", ARG4, 
                     sizeof (unsigned long) );
      break;
   case VKI_PTRACE_SETREGS:
      PRE_MEM_READ( "ptrace(setregs)", ARG4, 
		     sizeof (struct vki_user_regs_struct));
      break;
   case VKI_PTRACE_SETFPREGS:
      PRE_MEM_READ( "ptrace(setfpregs)", ARG4, 
		     sizeof (struct vki_user_fp));
      break;
   case VKI_PTRACE_SETWMMXREGS:
      PRE_MEM_READ( "ptrace(setwmmxregs)", ARG4, 
		     VKI_IWMMXT_SIZE);
      break;
   case VKI_PTRACE_SETCRUNCHREGS:
      PRE_MEM_READ( "ptrace(setcrunchregs)", ARG4, 
		     VKI_CRUNCH_SIZE);
      break;
   case VKI_PTRACE_SETVFPREGS:
      PRE_MEM_READ( "ptrace(setvfpregs)", ARG4, 
                     sizeof (struct vki_user_vfp));
      break;
   case VKI_PTRACE_SETHBPREGS:
      PRE_MEM_READ( "ptrace(sethbpregs)", ARG4, sizeof(unsigned long));
      break;
   case VKI_PTRACE_GET_THREAD_AREA:
      PRE_MEM_WRITE( "ptrace(get_thread_area)", ARG4, sizeof(unsigned long));
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
   case VKI_PTRACE_GETREGS:
      POST_MEM_WRITE( ARG4, sizeof (struct vki_user_regs_struct));
      break;
   case VKI_PTRACE_GETFPREGS:
      POST_MEM_WRITE( ARG4, sizeof (struct vki_user_fp));
      break;
   case VKI_PTRACE_GETWMMXREGS:
      POST_MEM_WRITE( ARG4, VKI_IWMMXT_SIZE);
      break;
   case VKI_PTRACE_GETCRUNCHREGS:
      POST_MEM_WRITE( ARG4, VKI_CRUNCH_SIZE);
      break;
   case VKI_PTRACE_GETVFPREGS:
      POST_MEM_WRITE( ARG4, sizeof(struct vki_user_vfp));
      break;
   case VKI_PTRACE_GET_THREAD_AREA:
   case VKI_PTRACE_GETHBPREGS:
   case VKI_PTRACE_GETEVENTMSG:
      POST_MEM_WRITE( ARG4, sizeof(unsigned long));
      break;
   case VKI_PTRACE_GETSIGINFO:
      /* XXX: This is a simplification. Different parts of the
       * siginfo_t are valid depending on the type of signal.
       */
      POST_MEM_WRITE( ARG4, sizeof(vki_siginfo_t));
      break;
   default:
      break;
   }
}

#undef PRE
#undef POST

/* ---------------------------------------------------------------------
   The arm/Linux syscall table
   ------------------------------------------------------------------ */

#if 0
#define __NR_OABI_SYSCALL_BASE 0x900000
#else
#define __NR_OABI_SYSCALL_BASE 0x0
#endif

#define PLAX_(sysno, name)    WRAPPER_ENTRY_X_(arm_linux, sysno, name) 
#define PLAXY(sysno, name)    WRAPPER_ENTRY_XY(arm_linux, sysno, name)

// This table maps from __NR_xxx syscall numbers (from
// linux/include/asm-arm/unistd.h) to the appropriate PRE/POST sys_foo()
// wrappers on arm (as per sys_call_table in linux/arch/arm/kernel/entry.S).
//
// For those syscalls not handled by Valgrind, the annotation indicate its
// arch/OS combination, eg. */* (generic), */Linux (Linux only), ?/?
// (unknown).

static SyscallTableEntry syscall_main_table[] = {
//zz    //   (restart_syscall)                             // 0
   GENX_(__NR_exit,              sys_exit),           // 1
   GENX_(__NR_fork,              sys_fork),           // 2
   GENXY(__NR_read,              sys_read),           // 3
   GENX_(__NR_write,             sys_write),          // 4

   GENXY(__NR_open,              sys_open),           // 5
   GENXY(__NR_close,             sys_close),          // 6
//   GENXY(__NR_waitpid,           sys_waitpid),        // 7
   GENXY(__NR_creat,             sys_creat),          // 8
   GENX_(__NR_link,              sys_link),           // 9

   GENX_(__NR_unlink,            sys_unlink),         // 10
   GENX_(__NR_execve,            sys_execve),         // 11
   GENX_(__NR_chdir,             sys_chdir),          // 12
   GENXY(__NR_time,              sys_time),           // 13
   GENX_(__NR_mknod,             sys_mknod),          // 14

   GENX_(__NR_chmod,             sys_chmod),          // 15
//zz    LINX_(__NR_lchown,            sys_lchown16),       // 16
//   GENX_(__NR_break,             sys_ni_syscall),     // 17
//zz    //   (__NR_oldstat,           sys_stat),           // 18 (obsolete)
   LINX_(__NR_lseek,             sys_lseek),          // 19

   GENX_(__NR_getpid,            sys_getpid),         // 20
   LINX_(__NR_mount,             sys_mount),          // 21
   LINX_(__NR_umount,            sys_oldumount),      // 22
   LINX_(__NR_setuid,            sys_setuid16),       // 23 ## P
   LINX_(__NR_getuid,            sys_getuid16),       // 24 ## P
//zz 
//zz    //   (__NR_stime,             sys_stime),          // 25 * (SVr4,SVID,X/OPEN)
   PLAXY(__NR_ptrace,            sys_ptrace),         // 26
   GENX_(__NR_alarm,             sys_alarm),          // 27
//zz    //   (__NR_oldfstat,          sys_fstat),          // 28 * L -- obsolete
   GENX_(__NR_pause,             sys_pause),          // 29

   LINX_(__NR_utime,             sys_utime),          // 30
//   GENX_(__NR_stty,              sys_ni_syscall),     // 31
//   GENX_(__NR_gtty,              sys_ni_syscall),     // 32
   GENX_(__NR_access,            sys_access),         // 33
   GENX_(__NR_nice,              sys_nice),           // 34

//   GENX_(__NR_ftime,             sys_ni_syscall),     // 35
   GENX_(__NR_sync,              sys_sync),           // 36
   GENX_(__NR_kill,              sys_kill),           // 37
   GENX_(__NR_rename,            sys_rename),         // 38
   GENX_(__NR_mkdir,             sys_mkdir),          // 39

   GENX_(__NR_rmdir,             sys_rmdir),          // 40
   GENXY(__NR_dup,               sys_dup),            // 41
   LINXY(__NR_pipe,              sys_pipe),           // 42
   GENXY(__NR_times,             sys_times),          // 43
//   GENX_(__NR_prof,              sys_ni_syscall),     // 44
//zz 
   GENX_(__NR_brk,               sys_brk),            // 45
   LINX_(__NR_setgid,            sys_setgid16),       // 46
   LINX_(__NR_getgid,            sys_getgid16),       // 47
//zz    //   (__NR_signal,            sys_signal),         // 48 */* (ANSI C)
   LINX_(__NR_geteuid,           sys_geteuid16),      // 49

   LINX_(__NR_getegid,           sys_getegid16),      // 50
   GENX_(__NR_acct,              sys_acct),           // 51
   LINX_(__NR_umount2,           sys_umount),         // 52
//   GENX_(__NR_lock,              sys_ni_syscall),     // 53
   LINXY(__NR_ioctl,             sys_ioctl),          // 54

   LINXY(__NR_fcntl,             sys_fcntl),          // 55
//   GENX_(__NR_mpx,               sys_ni_syscall),     // 56
   GENX_(__NR_setpgid,           sys_setpgid),        // 57
//   GENX_(__NR_ulimit,            sys_ni_syscall),     // 58
//zz    //   (__NR_oldolduname,       sys_olduname),       // 59 Linux -- obsolete
//zz 
   GENX_(__NR_umask,             sys_umask),          // 60
   GENX_(__NR_chroot,            sys_chroot),         // 61
//zz    //   (__NR_ustat,             sys_ustat)           // 62 SVr4 -- deprecated
   GENXY(__NR_dup2,              sys_dup2),           // 63
   GENX_(__NR_getppid,           sys_getppid),        // 64

   GENX_(__NR_getpgrp,           sys_getpgrp),        // 65
   GENX_(__NR_setsid,            sys_setsid),         // 66
   LINXY(__NR_sigaction,         sys_sigaction),      // 67
//zz    //   (__NR_sgetmask,          sys_sgetmask),       // 68 */* (ANSI C)
//zz    //   (__NR_ssetmask,          sys_ssetmask),       // 69 */* (ANSI C)
//zz 
   LINX_(__NR_setreuid,          sys_setreuid16),     // 70
   LINX_(__NR_setregid,          sys_setregid16),     // 71
   PLAX_(__NR_sigsuspend,        sys_sigsuspend),     // 72
   LINXY(__NR_sigpending,        sys_sigpending),     // 73
//zz    //   (__NR_sethostname,       sys_sethostname),    // 74 */*
//zz 
   GENX_(__NR_setrlimit,         sys_setrlimit),      // 75
   GENXY(__NR_getrlimit,         sys_old_getrlimit),  // 76
   GENXY(__NR_getrusage,         sys_getrusage),      // 77
   GENXY(__NR_gettimeofday,      sys_gettimeofday),   // 78
   GENX_(__NR_settimeofday,      sys_settimeofday),   // 79

   LINXY(__NR_getgroups,         sys_getgroups16),    // 80
   LINX_(__NR_setgroups,         sys_setgroups16),    // 81
//   PLAX_(__NR_select,            old_select),         // 82
   GENX_(__NR_symlink,           sys_symlink),        // 83
//zz    //   (__NR_oldlstat,          sys_lstat),          // 84 -- obsolete
//zz 
   GENX_(__NR_readlink,          sys_readlink),       // 85
//zz    //   (__NR_uselib,            sys_uselib),         // 86 */Linux
//zz    //   (__NR_swapon,            sys_swapon),         // 87 */Linux
//zz    //   (__NR_reboot,            sys_reboot),         // 88 */Linux
//zz    //   (__NR_readdir,           old_readdir),        // 89 -- superseded
//zz 
//   _____(__NR_mmap,              old_mmap),           // 90
   GENXY(__NR_munmap,            sys_munmap),         // 91
   GENX_(__NR_truncate,          sys_truncate),       // 92
   GENX_(__NR_ftruncate,         sys_ftruncate),      // 93
   GENX_(__NR_fchmod,            sys_fchmod),         // 94

   LINX_(__NR_fchown,            sys_fchown16),       // 95
   GENX_(__NR_getpriority,       sys_getpriority),    // 96
   GENX_(__NR_setpriority,       sys_setpriority),    // 97
//   GENX_(__NR_profil,            sys_ni_syscall),     // 98
   GENXY(__NR_statfs,            sys_statfs),         // 99

   GENXY(__NR_fstatfs,           sys_fstatfs),        // 100
//   LINX_(__NR_ioperm,            sys_ioperm),         // 101
   PLAXY(__NR_socketcall,        sys_socketcall),     // 102
   LINXY(__NR_syslog,            sys_syslog),         // 103
   GENXY(__NR_setitimer,         sys_setitimer),      // 104

   GENXY(__NR_getitimer,         sys_getitimer),      // 105
   GENXY(__NR_stat,              sys_newstat),        // 106
   GENXY(__NR_lstat,             sys_newlstat),       // 107
   GENXY(__NR_fstat,             sys_newfstat),       // 108
//zz    //   (__NR_olduname,          sys_uname),          // 109 -- obsolete
//zz 
//   GENX_(__NR_iopl,              sys_iopl),           // 110
   LINX_(__NR_vhangup,           sys_vhangup),        // 111
//   GENX_(__NR_idle,              sys_ni_syscall),     // 112
// PLAXY(__NR_vm86old,           sys_vm86old),        // 113 __NR_syscall... weird
   GENXY(__NR_wait4,             sys_wait4),          // 114
//zz 
//zz    //   (__NR_swapoff,           sys_swapoff),        // 115 */Linux 
   LINXY(__NR_sysinfo,           sys_sysinfo),        // 116
//   _____(__NR_ipc,               sys_ipc),            // 117
   GENX_(__NR_fsync,             sys_fsync),          // 118
   PLAX_(__NR_sigreturn,         sys_sigreturn),      // 119 ?/Linux

   PLAX_(__NR_clone,             sys_clone),          // 120
//zz    //   (__NR_setdomainname,     sys_setdomainname),  // 121 */*(?)
   GENXY(__NR_uname,             sys_newuname),       // 122
//   PLAX_(__NR_modify_ldt,        sys_modify_ldt),     // 123
//zz    LINXY(__NR_adjtimex,          sys_adjtimex),       // 124
//zz 
   GENXY(__NR_mprotect,          sys_mprotect),       // 125
   LINXY(__NR_sigprocmask,       sys_sigprocmask),    // 126
//zz    // Nb: create_module() was removed 2.4-->2.6
//   GENX_(__NR_create_module,     sys_ni_syscall),     // 127
   LINX_(__NR_init_module,       sys_init_module),    // 128
   LINX_(__NR_delete_module,     sys_delete_module),  // 129
//zz 
//zz    // Nb: get_kernel_syms() was removed 2.4-->2.6
//   GENX_(__NR_get_kernel_syms,   sys_ni_syscall),     // 130
   LINX_(__NR_quotactl,          sys_quotactl),       // 131
   GENX_(__NR_getpgid,           sys_getpgid),        // 132
   GENX_(__NR_fchdir,            sys_fchdir),         // 133
//zz    //   (__NR_bdflush,           sys_bdflush),        // 134 */Linux
//zz 
//zz    //   (__NR_sysfs,             sys_sysfs),          // 135 SVr4
   LINX_(__NR_personality,       sys_personality),    // 136
//   GENX_(__NR_afs_syscall,       sys_ni_syscall),     // 137
   LINX_(__NR_setfsuid,          sys_setfsuid16),     // 138
   LINX_(__NR_setfsgid,          sys_setfsgid16),     // 139
 
   LINXY(__NR__llseek,           sys_llseek),         // 140
   GENXY(__NR_getdents,          sys_getdents),       // 141
   GENX_(__NR__newselect,        sys_select),         // 142
   GENX_(__NR_flock,             sys_flock),          // 143
   GENX_(__NR_msync,             sys_msync),          // 144

   GENXY(__NR_readv,             sys_readv),          // 145
   GENX_(__NR_writev,            sys_writev),         // 146
   GENX_(__NR_getsid,            sys_getsid),         // 147
   GENX_(__NR_fdatasync,         sys_fdatasync),      // 148
   LINXY(__NR__sysctl,           sys_sysctl),         // 149

   GENX_(__NR_mlock,             sys_mlock),          // 150
   GENX_(__NR_munlock,           sys_munlock),        // 151
   GENX_(__NR_mlockall,          sys_mlockall),       // 152
   LINX_(__NR_munlockall,        sys_munlockall),     // 153
   LINXY(__NR_sched_setparam,    sys_sched_setparam), // 154

   LINXY(__NR_sched_getparam,         sys_sched_getparam),        // 155
   LINX_(__NR_sched_setscheduler,     sys_sched_setscheduler),    // 156
   LINX_(__NR_sched_getscheduler,     sys_sched_getscheduler),    // 157
   LINX_(__NR_sched_yield,            sys_sched_yield),           // 158
   LINX_(__NR_sched_get_priority_max, sys_sched_get_priority_max),// 159

   LINX_(__NR_sched_get_priority_min, sys_sched_get_priority_min),// 160
//zz    //LINX?(__NR_sched_rr_get_interval,  sys_sched_rr_get_interval), // 161 */*
   GENXY(__NR_nanosleep,         sys_nanosleep),      // 162
   GENX_(__NR_mremap,            sys_mremap),         // 163
   LINX_(__NR_setresuid,         sys_setresuid16),    // 164

   LINXY(__NR_getresuid,         sys_getresuid16),    // 165
//   PLAXY(__NR_vm86,              sys_vm86),           // 166 x86/Linux-only
//   GENX_(__NR_query_module,      sys_ni_syscall),     // 167
   GENXY(__NR_poll,              sys_poll),           // 168
//zz    //   (__NR_nfsservctl,        sys_nfsservctl),     // 169 */Linux
//zz 
   LINX_(__NR_setresgid,         sys_setresgid16),    // 170
   LINXY(__NR_getresgid,         sys_getresgid16),    // 171
   LINXY(__NR_prctl,             sys_prctl),          // 172
   PLAX_(__NR_rt_sigreturn,      sys_rt_sigreturn),   // 173 
   LINXY(__NR_rt_sigaction,      sys_rt_sigaction),   // 174

   LINXY(__NR_rt_sigprocmask,    sys_rt_sigprocmask), // 175
   LINXY(__NR_rt_sigpending,     sys_rt_sigpending),  // 176
   LINXY(__NR_rt_sigtimedwait,   sys_rt_sigtimedwait),// 177
   LINXY(__NR_rt_sigqueueinfo,   sys_rt_sigqueueinfo),// 178
   LINX_(__NR_rt_sigsuspend,     sys_rt_sigsuspend),  // 179

   GENXY(__NR_pread64,           sys_pread64),        // 180
   GENX_(__NR_pwrite64,          sys_pwrite64),       // 181
   LINX_(__NR_chown,             sys_chown16),        // 182
   GENXY(__NR_getcwd,            sys_getcwd),         // 183
   LINXY(__NR_capget,            sys_capget),         // 184

   LINX_(__NR_capset,            sys_capset),         // 185
   GENXY(__NR_sigaltstack,       sys_sigaltstack),    // 186
   LINXY(__NR_sendfile,          sys_sendfile),       // 187
//   GENXY(__NR_getpmsg,           sys_getpmsg),        // 188
//   GENX_(__NR_putpmsg,           sys_putpmsg),        // 189

   // Nb: we treat vfork as fork
   GENX_(__NR_vfork,             sys_fork),           // 190
   GENXY(__NR_ugetrlimit,        sys_getrlimit),      // 191
   PLAX_(__NR_mmap2,             sys_mmap2),          // 192
   GENX_(__NR_truncate64,        sys_truncate64),     // 193
   GENX_(__NR_ftruncate64,       sys_ftruncate64),    // 194
   
   PLAXY(__NR_stat64,            sys_stat64),         // 195
   PLAXY(__NR_lstat64,           sys_lstat64),        // 196
   PLAXY(__NR_fstat64,           sys_fstat64),        // 197
   GENX_(__NR_lchown32,          sys_lchown),         // 198
   GENX_(__NR_getuid32,          sys_getuid),         // 199

   GENX_(__NR_getgid32,          sys_getgid),         // 200
   GENX_(__NR_geteuid32,         sys_geteuid),        // 201
   GENX_(__NR_getegid32,         sys_getegid),        // 202
   GENX_(__NR_setreuid32,        sys_setreuid),       // 203
   GENX_(__NR_setregid32,        sys_setregid),       // 204

   GENXY(__NR_getgroups32,       sys_getgroups),      // 205
   GENX_(__NR_setgroups32,       sys_setgroups),      // 206
   GENX_(__NR_fchown32,          sys_fchown),         // 207
   LINX_(__NR_setresuid32,       sys_setresuid),      // 208
   LINXY(__NR_getresuid32,       sys_getresuid),      // 209

   LINX_(__NR_setresgid32,       sys_setresgid),      // 210
   LINXY(__NR_getresgid32,       sys_getresgid),      // 211
   GENX_(__NR_chown32,           sys_chown),          // 212
   GENX_(__NR_setuid32,          sys_setuid),         // 213
   GENX_(__NR_setgid32,          sys_setgid),         // 214

   LINX_(__NR_setfsuid32,        sys_setfsuid),       // 215
   LINX_(__NR_setfsgid32,        sys_setfsgid),       // 216
//zz    //   (__NR_pivot_root,        sys_pivot_root),     // 217 */Linux
   GENXY(__NR_mincore,           sys_mincore),        // 218
   GENX_(__NR_madvise,           sys_madvise),        // 219

   GENXY(__NR_getdents64,        sys_getdents64),     // 220
   LINXY(__NR_fcntl64,           sys_fcntl64),        // 221
//   GENX_(222,                    sys_ni_syscall),     // 222
//   PLAXY(223,                    sys_syscall223),     // 223 // sys_bproc?
   LINX_(__NR_gettid,            sys_gettid),         // 224

   LINX_(__NR_readahead,         sys_readahead),      // 225 */Linux
   LINX_(__NR_setxattr,          sys_setxattr),       // 226
   LINX_(__NR_lsetxattr,         sys_lsetxattr),      // 227
   LINX_(__NR_fsetxattr,         sys_fsetxattr),      // 228
   LINXY(__NR_getxattr,          sys_getxattr),       // 229

   LINXY(__NR_lgetxattr,         sys_lgetxattr),      // 230
   LINXY(__NR_fgetxattr,         sys_fgetxattr),      // 231
   LINXY(__NR_listxattr,         sys_listxattr),      // 232
   LINXY(__NR_llistxattr,        sys_llistxattr),     // 233
   LINXY(__NR_flistxattr,        sys_flistxattr),     // 234

   LINX_(__NR_removexattr,       sys_removexattr),    // 235
   LINX_(__NR_lremovexattr,      sys_lremovexattr),   // 236
   LINX_(__NR_fremovexattr,      sys_fremovexattr),   // 237
   LINXY(__NR_tkill,             sys_tkill),          // 238 */Linux
   LINXY(__NR_sendfile64,        sys_sendfile64),     // 239

   LINXY(__NR_futex,             sys_futex),             // 240
   LINX_(__NR_sched_setaffinity, sys_sched_setaffinity), // 241
   LINXY(__NR_sched_getaffinity, sys_sched_getaffinity), // 242
//   PLAX_(__NR_set_thread_area,   sys_set_thread_area),   // 243
//   PLAX_(__NR_get_thread_area,   sys_get_thread_area),   // 244

   LINXY(__NR_io_setup,          sys_io_setup),       // 245
   LINX_(__NR_io_destroy,        sys_io_destroy),     // 246
   LINXY(__NR_io_getevents,      sys_io_getevents),   // 247
   LINX_(__NR_io_submit,         sys_io_submit),      // 248
   LINXY(__NR_io_cancel,         sys_io_cancel),      // 249

//   LINX_(__NR_fadvise64,         sys_fadvise64),      // 250 */(Linux?)
   GENX_(251,                    sys_ni_syscall),     // 251
   LINX_(__NR_exit_group,        sys_exit_group),     // 252
//   GENXY(__NR_lookup_dcookie,    sys_lookup_dcookie), // 253
   LINXY(__NR_epoll_create,      sys_epoll_create),   // 254

   LINX_(__NR_epoll_ctl,         sys_epoll_ctl),         // 255
   LINXY(__NR_epoll_wait,        sys_epoll_wait),        // 256
//zz    //   (__NR_remap_file_pages,  sys_remap_file_pages),  // 257 */Linux
   LINX_(__NR_set_tid_address,   sys_set_tid_address),   // 258
   LINXY(__NR_timer_create,      sys_timer_create),      // 259

   LINXY(__NR_timer_settime,     sys_timer_settime),  // (timer_create+1)
   LINXY(__NR_timer_gettime,     sys_timer_gettime),  // (timer_create+2)
   LINX_(__NR_timer_getoverrun,  sys_timer_getoverrun),//(timer_create+3)
   LINX_(__NR_timer_delete,      sys_timer_delete),   // (timer_create+4)
   LINX_(__NR_clock_settime,     sys_clock_settime),  // (timer_create+5)

   LINXY(__NR_clock_gettime,     sys_clock_gettime),  // (timer_create+6)
   LINXY(__NR_clock_getres,      sys_clock_getres),   // (timer_create+7)
   LINXY(__NR_clock_nanosleep,   sys_clock_nanosleep),// (timer_create+8) */*
   GENXY(__NR_statfs64,          sys_statfs64),       // 268
   GENXY(__NR_fstatfs64,         sys_fstatfs64),      // 269

   LINX_(__NR_tgkill,            sys_tgkill),         // 270 */Linux
   GENX_(__NR_utimes,            sys_utimes),         // 271
//   LINX_(__NR_fadvise64_64,      sys_fadvise64_64),   // 272 */(Linux?)
   GENX_(__NR_vserver,           sys_ni_syscall),     // 273
   LINX_(__NR_mbind,             sys_mbind),          // 274 ?/?

   LINXY(__NR_get_mempolicy,     sys_get_mempolicy),  // 275 ?/?
   LINX_(__NR_set_mempolicy,     sys_set_mempolicy),  // 276 ?/?
   LINXY(__NR_mq_open,           sys_mq_open),        // 277
   LINX_(__NR_mq_unlink,         sys_mq_unlink),      // (mq_open+1)
   LINX_(__NR_mq_timedsend,      sys_mq_timedsend),   // (mq_open+2)

   LINXY(__NR_mq_timedreceive,   sys_mq_timedreceive),// (mq_open+3)
   LINX_(__NR_mq_notify,         sys_mq_notify),      // (mq_open+4)
   LINXY(__NR_mq_getsetattr,     sys_mq_getsetattr),  // (mq_open+5)
   LINXY(__NR_waitid,            sys_waitid),         // 280

   PLAXY(__NR_socket,            sys_socket),         // 281
   PLAX_(__NR_bind,              sys_bind),           // 282
   PLAX_(__NR_connect,           sys_connect),        // 283
   PLAX_(__NR_listen,            sys_listen),         // 284
   PLAXY(__NR_accept,            sys_accept),         // 285
   PLAXY(__NR_getsockname,       sys_getsockname),    // 286
   PLAXY(__NR_getpeername,       sys_getpeername),    // 287
   PLAXY(__NR_socketpair,        sys_socketpair),     // 288
   PLAX_(__NR_send,              sys_send),
   PLAX_(__NR_sendto,            sys_sendto),         // 290
   PLAXY(__NR_recv,              sys_recv),
   PLAXY(__NR_recvfrom,          sys_recvfrom),       // 292
   PLAX_(__NR_shutdown,          sys_shutdown),       // 293
   PLAX_(__NR_setsockopt,        sys_setsockopt),     // 294
   PLAXY(__NR_getsockopt,        sys_getsockopt),     // 295
   PLAX_(__NR_sendmsg,           sys_sendmsg),        // 296
   PLAXY(__NR_recvmsg,           sys_recvmsg),        // 297
   PLAX_(__NR_semop,             sys_semop),          // 298 
   PLAX_(__NR_semget,            sys_semget),         // 299
   PLAXY(__NR_semctl,            sys_semctl),         // 300
   PLAX_(__NR_msgget,            sys_msgget),         
   PLAX_(__NR_msgsnd,            sys_msgsnd),          
   PLAXY(__NR_msgrcv,            sys_msgrcv),         
   PLAXY(__NR_msgctl,            sys_msgctl),         // 304
   PLAX_(__NR_semtimedop,        sys_semtimedop),     // 312

   LINX_(__NR_add_key,           sys_add_key),        // 286
   LINX_(__NR_request_key,       sys_request_key),    // 287
   LINXY(__NR_keyctl,            sys_keyctl),         // not 288...
//   LINX_(__NR_ioprio_set,        sys_ioprio_set),     // 289

//   LINX_(__NR_ioprio_get,        sys_ioprio_get),     // 290
   LINX_(__NR_inotify_init,    sys_inotify_init),   // 291
   LINX_(__NR_inotify_add_watch, sys_inotify_add_watch), // 292
   LINX_(__NR_inotify_rm_watch,    sys_inotify_rm_watch), // 293
//   LINX_(__NR_migrate_pages,    sys_migrate_pages),    // 294

   LINXY(__NR_openat,       sys_openat),           // 295
   LINX_(__NR_mkdirat,       sys_mkdirat),          // 296
   LINX_(__NR_mknodat,       sys_mknodat),          // 297
   LINX_(__NR_fchownat,       sys_fchownat),         // 298
   LINX_(__NR_futimesat,    sys_futimesat),        // 326 on arm

   PLAXY(__NR_fstatat64,    sys_fstatat64),        // 300
   LINX_(__NR_unlinkat,       sys_unlinkat),         // 301
   LINX_(__NR_renameat,       sys_renameat),         // 302
   LINX_(__NR_linkat,       sys_linkat),           // 303
   LINX_(__NR_symlinkat,    sys_symlinkat),        // 304

   LINX_(__NR_readlinkat,    sys_readlinkat),       // 
   LINX_(__NR_fchmodat,       sys_fchmodat),         //
   LINX_(__NR_faccessat,    sys_faccessat),        //
   PLAXY(__NR_shmat,         wrap_sys_shmat),       //305
   PLAXY(__NR_shmdt,             sys_shmdt),          //306 
   PLAX_(__NR_shmget,            sys_shmget),         //307 
   PLAXY(__NR_shmctl,            sys_shmctl),         // 308 
//   LINX_(__NR_pselect6,       sys_pselect6),         //

//   LINX_(__NR_unshare,       sys_unshare),          // 310
   LINX_(__NR_set_robust_list,    sys_set_robust_list),  // 311
   LINXY(__NR_get_robust_list,    sys_get_robust_list),  // 312
//   LINX_(__NR_splice,            sys_ni_syscall),       // 313
//   LINX_(__NR_sync_file_range,   sys_sync_file_range),  // 314

//   LINX_(__NR_tee,               sys_ni_syscall),       // 315
//   LINX_(__NR_vmsplice,          sys_ni_syscall),       // 316
   LINXY(__NR_move_pages,        sys_move_pages),       // 317
//   LINX_(__NR_getcpu,            sys_ni_syscall),       // 318

   LINX_(__NR_utimensat,         sys_utimensat),        // 320
   LINXY(__NR_signalfd,          sys_signalfd),         // 321
   LINXY(__NR_timerfd_create,    sys_timerfd_create),   // 322
   LINX_(__NR_eventfd,           sys_eventfd),          // 323

   LINXY(__NR_timerfd_settime,   sys_timerfd_settime),  // 325
   LINXY(__NR_timerfd_gettime,   sys_timerfd_gettime),   // 326

   ///////////////

   // JRS 2010-Jan-03: I believe that all the numbers listed 
   // in comments in the table prior to this point (eg "// 326",
   // etc) are bogus since it looks to me like they are copied
   // verbatim from syswrap-x86-linux.c and they certainly do not
   // correspond to what's in include/vki/vki-scnums-arm-linux.h.
   // From here onwards, please ensure the numbers are correct.

   LINX_(__NR_pselect6,          sys_pselect6),         // 335
   LINXY(__NR_ppoll,             sys_ppoll),            // 336

   LINXY(__NR_epoll_pwait,       sys_epoll_pwait),      // 346

   LINX_(__NR_fallocate,         sys_fallocate),        // 352

   LINXY(__NR_signalfd4,         sys_signalfd4),        // 355
   LINX_(__NR_eventfd2,          sys_eventfd2),         // 356
   LINXY(__NR_epoll_create1,     sys_epoll_create1),    // 357
   LINXY(__NR_dup3,              sys_dup3),             // 358
   LINXY(__NR_pipe2,             sys_pipe2),            // 359
   LINXY(__NR_inotify_init1,     sys_inotify_init1),    // 360
   LINXY(__NR_preadv,            sys_preadv),           // 361
   LINX_(__NR_pwritev,           sys_pwritev),          // 362
   LINXY(__NR_rt_tgsigqueueinfo, sys_rt_tgsigqueueinfo),// 363
   LINXY(__NR_perf_event_open,   sys_perf_event_open),  // 364

   PLAXY(__NR_accept4,           sys_accept4)           // 366
};


/* These are not in the main table because there indexes are not small
   integers, but rather values close to one million.  So their
   inclusion would force the main table to be huge (about 8 MB). */

static SyscallTableEntry ste___ARM_set_tls
   = { WRAPPER_PRE_NAME(arm_linux,sys_set_tls), NULL };

static SyscallTableEntry ste___ARM_cacheflush
   = { WRAPPER_PRE_NAME(arm_linux,sys_cacheflush), NULL };

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

   /* Check if it's one of the out-of-line entries. */
   switch (sysno) {
      case __NR_ARM_set_tls:    return &ste___ARM_set_tls;
      case __NR_ARM_cacheflush: return &ste___ARM_cacheflush;
      default: break;
   }

   /* Can't find a wrapper */
   return NULL;
}

#endif // defined(VGP_arm_linux)

/*--------------------------------------------------------------------*/
/*--- end                                      syswrap-arm-linux.c ---*/
/*--------------------------------------------------------------------*/
