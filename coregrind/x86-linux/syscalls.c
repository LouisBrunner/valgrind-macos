
/*--------------------------------------------------------------------*/
/*--- Platform-specific syscalls stuff.       x86-linux/syscalls.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, an extensible x86 protected-mode
   emulator for monitoring program execution on x86-Unixes.

   Copyright (C) 2000-2004 Nicholas Nethercote
      njn25@cam.ac.uk

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

#include "core.h"


// See the comment accompanying the declaration of VGA_(thread_syscall)() in
// coregrind/core.h for an explanation of what this does, and why.
asm(
".text\n"
"	.type vgArch_do_thread_syscall,@function\n"

".globl  vgArch_do_thread_syscall\n"
"vgArch_do_thread_syscall:\n"
"	push	%esi\n"
"	push	%edi\n"
"	push	%ebx\n"
"	push	%ebp\n"
".vgArch_sys_before:\n"
"	movl	16+ 4(%esp),%eax\n" /* syscall */
"	movl	16+ 8(%esp),%ebx\n" /* arg1 */
"	movl	16+12(%esp),%ecx\n" /* arg2 */
"	movl	16+16(%esp),%edx\n" /* arg3 */
"	movl	16+20(%esp),%esi\n" /* arg4 */
"	movl	16+24(%esp),%edi\n" /* arg5 */
"	movl	16+28(%esp),%ebp\n" /* arg6 */
".vgArch_sys_restarted:\n"
"	int	$0x80\n"
".vgArch_sys_after:\n"
"	movl	16+32(%esp),%ebx\n"	/* ebx = Int *RES */
"	movl	%eax, (%ebx)\n"		/* write the syscall retval */

"	movl	16+36(%esp),%ebx\n"	/* ebx = enum PXState * */
"	testl	%ebx, %ebx\n"
"	jz	1f\n"

"	movl	16+40(%esp),%ecx\n"	/* write the post state (must be after retval write) */
"	movl	%ecx,(%ebx)\n"

".vgArch_sys_done:\n"			/* OK, all clear from here */
"1:	popl	%ebp\n"
"	popl	%ebx\n"
"	popl	%edi\n"
"	popl	%esi\n"
"	ret\n"
"	.size vgArch_do_thread_syscall,.-vgArch_do_thread_syscall\n"
".previous\n"

".section .rodata\n"
"       .globl  vgArch_sys_before\n"
"vgArch_sys_before:	.long	.vgArch_sys_before\n"
"       .globl  vgArch_sys_restarted\n"
"vgArch_sys_restarted:	.long	.vgArch_sys_restarted\n"
"       .globl  vgArch_sys_after\n"
"vgArch_sys_after:	.long	.vgArch_sys_after\n"
"       .globl  vgArch_sys_done\n"
"vgArch_sys_done:	.long	.vgArch_sys_done\n"
".previous\n"
);

// Back up to restart a system call.
void VGA_(restart_syscall)(ThreadArchState *arch)
{
   arch->vex.guest_EIP -= 2;             // sizeof(int $0x80)

   /* Make sure our caller is actually sane, and we're really backing
      back over a syscall.

      int $0x80 == CD 80 
   */
   {
      UChar *p = (UChar *)arch->vex.guest_EIP;
      
      if (p[0] != 0xcd || p[1] != 0x80)
         VG_(message)(Vg_DebugMsg,
                      "?! restarting over syscall at %p %02x %02x\n",
                      arch->vex.guest_EIP, p[0], p[1]); 

      vg_assert(p[0] == 0xcd && p[1] == 0x80);
   }
}

/* ---------------------------------------------------------------------
   PRE/POST wrappers for x86/Linux-specific syscalls
   ------------------------------------------------------------------ */

// Nb: See the comment above the generic PRE/POST wrappers in
// coregrind/vg_syscalls.c for notes about how they work.

#define PRE(name, f)     PRE_TEMPLATE(static, x86_linux, name, f)
#define POST(name)      POST_TEMPLATE(static, x86_linux, name)

PRE(old_select, MayBlock)
{
   /* struct sel_arg_struct {
      unsigned long n;
      fd_set *inp, *outp, *exp;
      struct timeval *tvp;
      };
   */
   PRE_REG_READ1(long, "old_select", struct sel_arg_struct *, args);
   PRE_MEM_READ( "old_select(args)", ARG1, 5*sizeof(UWord) );

   {
      UInt* arg_struct = (UInt*)ARG1;
      UInt a1, a2, a3, a4, a5;

      a1 = arg_struct[0];
      a2 = arg_struct[1];
      a3 = arg_struct[2];
      a4 = arg_struct[3];
      a5 = arg_struct[4];

      PRINT("old_select ( %d, %p, %p, %p, %p )", a1,a2,a3,a4,a5);
      if (a2 != (Addr)NULL)
	 PRE_MEM_READ( "old_select(readfds)",   a2, a1/8 /* __FD_SETSIZE/8 */ );
      if (a3 != (Addr)NULL)
	 PRE_MEM_READ( "old_select(writefds)",  a3, a1/8 /* __FD_SETSIZE/8 */ );
      if (a4 != (Addr)NULL)
	 PRE_MEM_READ( "old_select(exceptfds)", a4, a1/8 /* __FD_SETSIZE/8 */ );
      if (a5 != (Addr)NULL)
	 PRE_MEM_READ( "old_select(timeout)", a5, sizeof(struct vki_timeval) );
   }
}

PRE(sys_clone, Special)
{
   PRINT("sys_clone ( %d, %p, %p, %p, %p )",ARG1,ARG2,ARG3,ARG4,ARG5);
   // XXX: really not sure about the last two args... if they are really
   // there, we should do PRE_MEM_READs for both of them...
   PRE_REG_READ4(int, "clone",
                 unsigned long, flags, void *, child_stack,
                 int *, parent_tidptr, int *, child_tidptr);

   if (ARG2 == 0 &&
       (ARG1 == (VKI_CLONE_CHILD_CLEARTID|VKI_CLONE_CHILD_SETTID|VKI_SIGCHLD)
     || ARG1 == (VKI_CLONE_PARENT_SETTID|VKI_SIGCHLD))) 
   {
      VGA_(gen_sys_fork_before)(tid, tst);
      SET_RESULT( VG_(do_syscall5)(SYSNO, ARG1, ARG2, ARG3, ARG4, ARG5) );
      VGA_(gen_sys_fork_after) (tid, tst);
   } else {
      VG_(unimplemented)
         ("clone(): not supported by Valgrind.\n   "
          "We do support programs linked against\n   "
          "libpthread.so, though.  Re-run with -v and ensure that\n   "
          "you are picking up Valgrind's implementation of libpthread.so.");
   }
}

PRE(sys_modify_ldt, Special)
{
   PRINT("sys_modify_ldt ( %d, %p, %d )", ARG1,ARG2,ARG3);
   PRE_REG_READ3(int, "modify_ldt", int, func, void *, ptr,
                 unsigned long, bytecount);
   
   if (ARG1 == 0) {
      /* read the LDT into ptr */
      PRE_MEM_WRITE( "modify_ldt(ptr)", ARG2, ARG3 );
   }
   if (ARG1 == 1 || ARG1 == 0x11) {
      /* write the LDT with the entry pointed at by ptr */
      PRE_MEM_READ( "modify_ldt(ptr)", ARG2, sizeof(vki_modify_ldt_t) );
   }
   /* "do" the syscall ourselves; the kernel never sees it */
   RES = VG_(sys_modify_ldt)( tid, ARG1, (void*)ARG2, ARG3 );

   if (ARG1 == 0 && !VG_(is_kerror)(RES) && RES > 0) {
      POST_MEM_WRITE( ARG2, RES );
   }
}

PRE(sys_set_thread_area, Special)
{
   PRINT("sys_set_thread_area ( %p )", ARG1);
   PRE_REG_READ1(int, "set_thread_area", struct user_desc *, u_info)
   PRE_MEM_READ( "set_thread_area(u_info)", ARG1, sizeof(vki_modify_ldt_t) );

   /* "do" the syscall ourselves; the kernel never sees it */
   SET_RESULT( VG_(sys_set_thread_area)( tid, (void *)ARG1 ) );
}

PRE(sys_get_thread_area, Special)
{
   PRINT("sys_get_thread_area ( %p )", ARG1);
   PRE_REG_READ1(int, "get_thread_area", struct user_desc *, u_info)
   PRE_MEM_WRITE( "get_thread_area(u_info)", ARG1, sizeof(vki_modify_ldt_t) );

   /* "do" the syscall ourselves; the kernel never sees it */
   SET_RESULT( VG_(sys_get_thread_area)( tid, (void *)ARG1 ) );

   if (!VG_(is_kerror)(RES)) {
      POST_MEM_WRITE( ARG1, sizeof(vki_modify_ldt_t) );
   }
}

// Parts of this are x86-specific, but the *PEEK* cases are generic.
// XXX: Why is the memory pointed to by ARG3 never checked?
PRE(sys_ptrace, 0)
{
   PRINT("sys_ptrace ( %d, %d, %p, %p )", ARG1,ARG2,ARG3,ARG4);
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
		     sizeof (struct vki_user_i387_struct));
      break;
   case VKI_PTRACE_GETFPXREGS:
      PRE_MEM_WRITE( "ptrace(getfpxregs)", ARG4, 
                     sizeof(struct vki_user_fxsr_struct) );
      break;
   case VKI_PTRACE_SETREGS:
      PRE_MEM_READ( "ptrace(setregs)", ARG4, 
		     sizeof (struct vki_user_regs_struct));
      break;
   case VKI_PTRACE_SETFPREGS:
      PRE_MEM_READ( "ptrace(setfpregs)", ARG4, 
		     sizeof (struct vki_user_i387_struct));
      break;
   case VKI_PTRACE_SETFPXREGS:
      PRE_MEM_READ( "ptrace(setfpxregs)", ARG4, 
                     sizeof(struct vki_user_fxsr_struct) );
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
      POST_MEM_WRITE( ARG4, sizeof (struct vki_user_i387_struct));
      break;
   case VKI_PTRACE_GETFPXREGS:
      POST_MEM_WRITE( ARG4, sizeof(struct vki_user_fxsr_struct) );
      break;
   default:
      break;
   }
}

static
UInt get_shm_size ( Int shmid )
{
   struct vki_shmid_ds buf;
   long __res = VG_(do_syscall5)(__NR_ipc, 24 /* IPCOP_shmctl */, shmid,
                                 VKI_IPC_STAT, 0, &buf);
    if ( VG_(is_kerror) ( __res ) )
       return 0;
 
   return buf.shm_segsz;
}

static
UInt get_sem_count( Int semid )
{
  struct vki_semid_ds buf;
  union vki_semun arg;
  long res;

  arg.buf = &buf;
  
  res = VG_(do_syscall5)(__NR_ipc, 3 /* IPCOP_semctl */, semid, 0,
                         VKI_IPC_STAT, &arg);
  if ( VG_(is_kerror)(res) )
    return 0;

  return buf.sem_nsems;
}

// XXX: this duplicates a function in coregrind/vg_syscalls.c, yuk
static Addr deref_Addr ( ThreadId tid, Addr a, Char* s )
{
   Addr* a_p = (Addr*)a;
   PRE_MEM_READ( s, (Addr)a_p, sizeof(Addr) );
   return *a_p;
}
 
// XXX: should use the constants here (eg. SHMAT), not the numbers directly!
PRE(sys_ipc, 0)
{
   PRINT("sys_ipc ( %d, %d, %d, %d, %p, %d )", ARG1,ARG2,ARG3,ARG4,ARG5,ARG6);
   // XXX: this is simplistic -- some args are not used in all circumstances.
   PRE_REG_READ6(int, "ipc",
                 vki_uint, call, int, first, int, second, int, third,
                 void *, ptr, long, fifth)

   switch (ARG1 /* call */) {
   case VKI_SEMOP:
      PRE_MEM_READ( "semop(sops)", ARG5, ARG3 * sizeof(struct vki_sembuf) );
      tst->sys_flags |= MayBlock;
      break;
   case VKI_SEMGET:
      break;
   case VKI_SEMCTL:
   {
      union vki_semun *arg = (union vki_semun *)ARG5;
      switch (ARG4 /* cmd */) {
      case VKI_IPC_INFO:
      case VKI_SEM_INFO:
      {
         Addr buf = deref_Addr( tid, (Addr)&arg->__buf, "semctl(IPC_INFO, arg)" );
	 PRE_MEM_WRITE( "semctl(IPC_INFO, arg->buf)", buf, 
			sizeof(struct vki_seminfo) );
	 break;
      }
      case VKI_IPC_STAT:
      case VKI_SEM_STAT:
      {
         Addr buf = deref_Addr( tid, (Addr)&arg->buf, "semctl(IPC_STAT, arg)" );
	 PRE_MEM_WRITE( "semctl(IPC_STAT, arg->buf)", buf, 
			sizeof(struct vki_semid_ds) );
	 break;
      }
      case VKI_IPC_SET:
      {
         Addr buf = deref_Addr( tid, (Addr)&arg->buf, "semctl(IPC_SET, arg)" );
	 PRE_MEM_READ( "semctl(IPC_SET, arg->buf)", buf, 
			sizeof(struct vki_semid_ds) );
	 break;
      }
      case VKI_GETALL:
      {
         Addr array = deref_Addr( tid, (Addr)&arg->array, "semctl(IPC_GETALL, arg)" );
         UInt nsems = get_sem_count( ARG2 );
	 PRE_MEM_WRITE( "semctl(IPC_GETALL, arg->array)", array, 
			sizeof(short) * nsems );
	 break;
      }
      case VKI_SETALL:
      {
         Addr array = deref_Addr( tid, (Addr)&arg->array, "semctl(IPC_SETALL, arg)" );
         UInt nsems = get_sem_count( ARG2 );
	 PRE_MEM_READ( "semctl(IPC_SETALL, arg->array)", array, 
			sizeof(short) * nsems );
	 break;
      }
      case VKI_SETVAL:
      {
	 PRE_MEM_READ( "semctl(IPC_SETVAL, arg->array)",
                        (Addr)&arg->val, sizeof(arg->val) );
	 break;
      }
      case VKI_IPC_INFO|VKI_IPC_64:
      case VKI_SEM_INFO|VKI_IPC_64:
      {
         Addr buf = deref_Addr( tid, (Addr)&arg->__buf, "semctl(IPC_INFO, arg)" );
	 PRE_MEM_WRITE( "semctl(IPC_INFO, arg->buf)", buf, 
			sizeof(struct vki_seminfo) );
	 break;
      }
      case VKI_IPC_STAT|VKI_IPC_64:
      case VKI_SEM_STAT|VKI_IPC_64:
      {
         Addr buf = deref_Addr( tid, (Addr)&arg->buf, "semctl(IPC_STAT, arg)" );
	 PRE_MEM_WRITE( "semctl(IPC_STAT, arg->buf)", buf, 
			sizeof(struct vki_semid64_ds) );
	 break;
      }
      case VKI_IPC_SET|VKI_IPC_64:
      {
         Addr buf = deref_Addr( tid, (Addr)&arg->buf, "semctl(IPC_SET, arg)" );
	 PRE_MEM_READ( "semctl(IPC_SET, arg->buf)", buf, 
			sizeof(struct vki_semid64_ds) );
	 break;
      }
      case VKI_GETALL|VKI_IPC_64:
      {
         Addr array = deref_Addr( tid, (Addr)&arg->array, "semctl(IPC_GETALL, arg)" );
         UInt nsems = get_sem_count( ARG2 );
	 PRE_MEM_WRITE( "semctl(IPC_GETALL, arg->array)", array, 
			sizeof(short) * nsems );
	 break;
      }
      case VKI_SETALL|VKI_IPC_64:
      {
         Addr array = deref_Addr( tid, (Addr)&arg->array, "semctl(IPC_SETALL, arg)" );
         UInt nsems = get_sem_count( ARG2 );
	 PRE_MEM_READ( "semctl(IPC_SETALL, arg->array)", array, 
			sizeof(short) * nsems );
	 break;
      }
      case VKI_SETVAL|VKI_IPC_64:
      {
	 PRE_MEM_READ( "semctl(IPC_SETVAL, arg->array)",
                        (Addr)&arg->val, sizeof(arg->val) );
	 break;
      }
      default:
	 break;
      }
      break;
   }
   case VKI_SEMTIMEDOP:
      PRE_MEM_READ( "semtimedop(sops)", ARG5, 
		     ARG3 * sizeof(struct vki_sembuf) );
      if (ARG6 != 0)
         PRE_MEM_READ( "semtimedop(timeout)", ARG6, 
                        sizeof(struct vki_timespec) );
      tst->sys_flags |= MayBlock;
      break;
   case VKI_MSGSND:
   {
      struct vki_msgbuf *msgp = (struct vki_msgbuf *)ARG5;
      Int msgsz = ARG3;

      PRE_MEM_READ( "msgsnd(msgp->mtype)", 
		     (Addr)&msgp->mtype, sizeof(msgp->mtype) );
      PRE_MEM_READ( "msgsnd(msgp->mtext)", 
		     (Addr)msgp->mtext, msgsz );

      if ((ARG4 & VKI_IPC_NOWAIT) == 0)
         tst->sys_flags |= MayBlock;
      break;
   }
   case VKI_MSGRCV:
   {
      struct vki_msgbuf *msgp;
      Int msgsz = ARG3;
 
      msgp = (struct vki_msgbuf *)deref_Addr( tid,
					  (Addr) (&((struct vki_ipc_kludge *)ARG5)->msgp),
					  "msgrcv(msgp)" );

      PRE_MEM_WRITE( "msgrcv(msgp->mtype)", 
		     (Addr)&msgp->mtype, sizeof(msgp->mtype) );
      PRE_MEM_WRITE( "msgrcv(msgp->mtext)", 
		     (Addr)msgp->mtext, msgsz );

      if ((ARG4 & VKI_IPC_NOWAIT) == 0)
         tst->sys_flags |= MayBlock;
      break;
   }
   case VKI_MSGGET:
      break;
   case VKI_MSGCTL:
   {
      switch (ARG3 /* cmd */) {
      case VKI_IPC_INFO:
      case VKI_MSG_INFO:
	 PRE_MEM_WRITE( "msgctl(IPC_INFO, buf)", ARG5, 
			sizeof(struct vki_msginfo) );
	 break;
      case VKI_IPC_STAT:
      case VKI_MSG_STAT:
	 PRE_MEM_WRITE( "msgctl(IPC_STAT, buf)", ARG5, 
			sizeof(struct vki_msqid_ds) );
	 break;
      case VKI_IPC_SET:
	 PRE_MEM_READ( "msgctl(IPC_SET, buf)", ARG5, 
			sizeof(struct vki_msqid_ds) );
	 break;
      case VKI_IPC_INFO|VKI_IPC_64:
      case VKI_MSG_INFO|VKI_IPC_64:
	 PRE_MEM_WRITE( "msgctl(IPC_INFO, buf)", ARG5, 
			sizeof(struct vki_msginfo) );
	 break;
      case VKI_IPC_STAT|VKI_IPC_64:
      case VKI_MSG_STAT|VKI_IPC_64:
	 PRE_MEM_WRITE( "msgctl(IPC_STAT, buf)", ARG5, 
			sizeof(struct vki_msqid64_ds) );
	 break;
      case VKI_IPC_SET|VKI_IPC_64:
	 PRE_MEM_READ( "msgctl(IPC_SET, buf)", ARG5, 
			sizeof(struct vki_msqid64_ds) );
	 break;
      default:
	 break;
      }
      break;
   }
   case VKI_SHMAT:
   {
      UInt shmid = ARG2;
      UInt segmentSize = get_shm_size ( shmid );
      
      /* If they didn't ask for a particular address, then place it
	 like an mmap. */
      if (ARG5 == 0)
	 ARG5 = VG_(find_map_space)(0, segmentSize, True);
      else if (!VG_(valid_client_addr)(ARG5, segmentSize, tid, "shmat"))
	 SET_RESULT( -VKI_EINVAL );
      break;
   }
   case VKI_SHMDT:
      if (!VG_(valid_client_addr)(ARG5, 1, tid, "shmdt"))
	 SET_RESULT( -VKI_EINVAL );
      break;
   case VKI_SHMGET:
      break;
   case VKI_SHMCTL: /* IPCOP_shmctl */
   {
      switch (ARG3 /* cmd */) {
      case VKI_IPC_INFO:
	 PRE_MEM_WRITE( "shmctl(IPC_INFO, buf)", ARG5, 
			sizeof(struct vki_shminfo) );
	 break;
      case VKI_SHM_INFO:
	 PRE_MEM_WRITE( "shmctl(SHM_INFO, buf)", ARG5, 
			sizeof(struct vki_shm_info) );
	 break;
      case VKI_IPC_STAT:
      case VKI_SHM_STAT:
	 PRE_MEM_WRITE( "shmctl(IPC_STAT, buf)", ARG5, 
			sizeof(struct vki_shmid_ds) );
	 break;
      case VKI_IPC_SET:
	 PRE_MEM_READ( "shmctl(IPC_SET, buf)", ARG5, 
			sizeof(struct vki_shmid_ds) );
	 break;
      case VKI_IPC_INFO|VKI_IPC_64:
	 PRE_MEM_WRITE( "shmctl(IPC_INFO, buf)", ARG5, 
			sizeof(struct vki_shminfo64) );
	 break;
      case VKI_SHM_INFO|VKI_IPC_64:
	 PRE_MEM_WRITE( "shmctl(SHM_INFO, buf)", ARG5, 
			sizeof(struct vki_shm_info) );
	 break;
      case VKI_IPC_STAT|VKI_IPC_64:
      case VKI_SHM_STAT|VKI_IPC_64:
	 PRE_MEM_WRITE( "shmctl(IPC_STAT, buf)", ARG5, 
			sizeof(struct vki_shmid64_ds) );
	 break;
      case VKI_IPC_SET|VKI_IPC_64:
	 PRE_MEM_READ( "shmctl(IPC_SET, buf)", ARG5, 
			sizeof(struct vki_shmid_ds) );
	 break;
      default:
	 break;
      }
      break;
   }
   default:
      VG_(message)(Vg_DebugMsg, "FATAL: unhandled syscall(ipc) %d", ARG1 );
      VG_(core_panic)("... bye!\n");
      break; /*NOTREACHED*/
   }   
}

POST(sys_ipc)
{
   switch (ARG1 /* call */) {
   case VKI_SEMOP:
   case VKI_SEMGET:
      break;
   case VKI_SEMCTL:
   {
      union vki_semun *arg = (union vki_semun *)ARG5;
      switch (ARG4 /* cmd */) {
      case VKI_IPC_INFO:
      case VKI_SEM_INFO:
      {
         Addr buf = deref_Addr( tid, (Addr)&arg->__buf, "semctl(arg)" );
	 POST_MEM_WRITE( buf, sizeof(struct vki_seminfo) );
	 break;
      }
      case VKI_IPC_STAT:
      case VKI_SEM_STAT:
      {
         Addr buf = deref_Addr( tid, (Addr)&arg->buf, "semctl(arg)" );
	 POST_MEM_WRITE( buf, sizeof(struct vki_semid_ds) );
	 break;
      }
      case VKI_GETALL:
      {
         Addr array = deref_Addr( tid, (Addr)&arg->array, "semctl(arg)" );
         UInt nsems = get_sem_count( ARG2 );
	 POST_MEM_WRITE( array, sizeof(short) * nsems );
	 break;
      }
      case VKI_IPC_INFO|VKI_IPC_64:
      case VKI_SEM_INFO|VKI_IPC_64:
      {
         Addr buf = deref_Addr( tid, (Addr)&arg->__buf, "semctl(arg)" );
	 POST_MEM_WRITE( buf, sizeof(struct vki_seminfo) );
	 break;
      }
      case VKI_IPC_STAT|VKI_IPC_64:
      case VKI_SEM_STAT|VKI_IPC_64:
      {
         Addr buf = deref_Addr( tid, (Addr)&arg->buf, "semctl(arg)" );
	 POST_MEM_WRITE( buf, sizeof(struct vki_semid64_ds) );
	 break;
      }
      case VKI_GETALL|VKI_IPC_64:
      {
         Addr array = deref_Addr( tid, (Addr)&arg->array, "semctl(arg)" );
         UInt nsems = get_sem_count( ARG2 );
	 POST_MEM_WRITE( array, sizeof(short) * nsems );
	 break;
      }
      default:
	 break;
      }
      break;
   }
   case VKI_SEMTIMEDOP:
   case VKI_MSGSND:
      break;
   case VKI_MSGRCV:
   {
      struct vki_msgbuf *msgp;
 
      msgp = (struct vki_msgbuf *)deref_Addr( tid,
					  (Addr) (&((struct vki_ipc_kludge *)ARG5)->msgp),
					  "msgrcv(msgp)" );
      if ( RES > 0 ) {
	 POST_MEM_WRITE( (Addr)&msgp->mtype, sizeof(msgp->mtype) );
	 POST_MEM_WRITE( (Addr)msgp->mtext, RES );
      }
      break;
   }
   case VKI_MSGGET:
      break;
   case VKI_MSGCTL:
   {
      switch (ARG3 /* cmd */) {
      case VKI_IPC_INFO:
      case VKI_MSG_INFO:
	 POST_MEM_WRITE( ARG5, sizeof(struct vki_msginfo) );
	 break;
      case VKI_IPC_STAT:
      case VKI_MSG_STAT:
	 POST_MEM_WRITE( ARG5, sizeof(struct vki_msqid_ds) );
	 break;
      case VKI_IPC_SET:
	 break;
      case VKI_IPC_INFO|VKI_IPC_64:
      case VKI_MSG_INFO|VKI_IPC_64:
	 POST_MEM_WRITE( ARG5, sizeof(struct vki_msginfo) );
	 break;
      case VKI_IPC_STAT|VKI_IPC_64:
      case VKI_MSG_STAT|VKI_IPC_64:
	 POST_MEM_WRITE( ARG5, sizeof(struct vki_msqid64_ds) );
	 break;
      case VKI_IPC_SET|VKI_IPC_64:
	 break;
      default:
	 break;
      }
      break;
   }
   case VKI_SHMAT:
   {
      Int shmid = ARG2;
      Int shmflag = ARG3;
      Addr addr;

      /* force readability. before the syscall it is
       * indeed uninitialized, as can be seen in
       * glibc/sysdeps/unix/sysv/linux/shmat.c */
      POST_MEM_WRITE( ARG4, sizeof( ULong ) );

      addr = deref_Addr ( tid, ARG4, "shmat(addr)" );
      if ( addr > 0 ) { 
	 UInt segmentSize = get_shm_size ( shmid );
	 if ( segmentSize > 0 ) {
	    UInt prot = VKI_PROT_READ|VKI_PROT_WRITE;
	    /* we don't distinguish whether it's read-only or
	     * read-write -- it doesn't matter really. */
	    VG_TRACK( new_mem_mmap, addr, segmentSize, True, True, False );

	    if (!(shmflag & 010000)) /* = SHM_RDONLY */
	       prot &= ~VKI_PROT_WRITE;
	    VG_(map_segment)(addr, segmentSize, prot, SF_SHARED|SF_SHM);
	 }
      }
      break;
   }
   case VKI_SHMDT:
   {
      Segment *s = VG_(find_segment)(ARG5);

      if (s != NULL && (s->flags & SF_SHM) && VG_(seg_contains)(s, ARG5, 1)) {
	 VG_TRACK( die_mem_munmap, s->addr, s->len );
	 VG_(unmap_range)(s->addr, s->len);
      }
      break;
   }
   case VKI_SHMGET:
      break;
   case VKI_SHMCTL:
   {
      switch (ARG3 /* cmd */) {
      case VKI_IPC_INFO:
	 POST_MEM_WRITE( ARG5, sizeof(struct vki_shminfo) );
	 break;
      case VKI_SHM_INFO:
	 POST_MEM_WRITE( ARG5, sizeof(struct vki_shm_info) );
	 break;
      case VKI_IPC_STAT:
      case VKI_SHM_STAT:
	 POST_MEM_WRITE( ARG5, sizeof(struct vki_shmid_ds) );
	 break;
      case VKI_IPC_INFO|VKI_IPC_64:
	 POST_MEM_WRITE( ARG5, sizeof(struct vki_shminfo64) );
	 break;
      case VKI_SHM_INFO|VKI_IPC_64:
	 POST_MEM_WRITE( ARG5, sizeof(struct vki_shm_info) );
	 break;
      case VKI_IPC_STAT|VKI_IPC_64:
      case VKI_SHM_STAT|VKI_IPC_64:
	 POST_MEM_WRITE( ARG5, sizeof(struct vki_shmid64_ds) );
	 break;
      default:
	 break;
      }
      break;
   }
   default:
      VG_(message)(Vg_DebugMsg,
		   "FATAL: unhandled syscall(ipc) %d",
		   ARG1 );
      VG_(core_panic)("... bye!\n");
      break; /*NOTREACHED*/
   }
}

PRE(sys_sigaction, Special)
{
   PRINT("sys_sigaction ( %d, %p, %p )", ARG1,ARG2,ARG3);
   PRE_REG_READ3(int, "sigaction",
                 int, signum, const struct old_sigaction *, act,
                 struct old_sigaction *, oldact)
   if (ARG2 != 0)
      PRE_MEM_READ( "sigaction(act)", ARG2, sizeof(struct vki_old_sigaction));
   if (ARG3 != 0)
      PRE_MEM_WRITE( "sigaction(oldact)", ARG3, sizeof(struct vki_old_sigaction));

   VG_(do_sys_sigaction)(tid);
}

POST(sys_sigaction)
{
   if (RES == 0 && ARG3 != 0)
      POST_MEM_WRITE( ARG3, sizeof(struct vki_old_sigaction));
}

#undef PRE
#undef POST

/* ---------------------------------------------------------------------
   The x86/Linux syscall table
   ------------------------------------------------------------------ */

// Macros for adding x86/Linux-specific wrappers to the syscall table.
#define PLAX_(const, name)    SYS_WRAPPER_ENTRY_X_(x86_linux, const, name) 
#define PLAXY(const, name)    SYS_WRAPPER_ENTRY_XY(x86_linux, const, name) 

// This table maps from __NR_xxx syscall numbers (from
// linux/include/asm-i386/unistd.h) to the appropriate PRE/POST sys_foo()
// wrappers on x86 (as per sys_call_table in linux/arch/i386/kernel/entry.S).
//
// For those syscalls not handled by Valgrind, the annotation indicate its
// arch/OS combination, eg. */* (generic), */Linux (Linux only), ?/?
// (unknown).

const struct SyscallTableEntry VGA_(syscall_table)[] = {
   //   (restart_syscall)                             // 0
   GENX_(__NR_exit,              sys_exit),           // 1
   GENXY(__NR_fork,              sys_fork),           // 2
   GENXY(__NR_read,              sys_read),           // 3
   GENX_(__NR_write,             sys_write),          // 4

   GENXY(__NR_open,              sys_open),           // 5
   GENXY(__NR_close,             sys_close),          // 6
   GENXY(__NR_waitpid,           sys_waitpid),        // 7
   GENXY(__NR_creat,             sys_creat),          // 8
   GENX_(__NR_link,              sys_link),           // 9

   GENX_(__NR_unlink,            sys_unlink),         // 10
   GENX_(__NR_execve,            sys_execve),         // 11
   GENX_(__NR_chdir,             sys_chdir),          // 12
   GENXY(__NR_time,              sys_time),           // 13
   GENX_(__NR_mknod,             sys_mknod),          // 14

   GENX_(__NR_chmod,             sys_chmod),          // 15
   //   (__NR_lchown,            sys_lchown16),       // 16 ## P
   GENX_(__NR_break,             sys_ni_syscall),     // 17
   //   (__NR_oldstat,           sys_stat),           // 18 (obsolete)
   GENX_(__NR_lseek,             sys_lseek),          // 19

   GENX_(__NR_getpid,            sys_getpid),         // 20
   LINX_(__NR_mount,             sys_mount),          // 21
   LINX_(__NR_umount,            sys_oldumount),      // 22
   GENX_(__NR_setuid,            sys_setuid16),       // 23 ## P
   GENX_(__NR_getuid,            sys_getuid16),       // 24 ## P

   //   (__NR_stime,             sys_stime),          // 25 * (SVr4,SVID,X/OPEN)
   PLAXY(__NR_ptrace,            sys_ptrace),         // 26
   GENX_(__NR_alarm,             sys_alarm),          // 27
   //   (__NR_oldfstat,          sys_fstat),          // 28 * L -- obsolete
   GENX_(__NR_pause,             sys_pause),          // 29

   GENX_(__NR_utime,             sys_utime),          // 30
   GENX_(__NR_stty,              sys_ni_syscall),     // 31
   GENX_(__NR_gtty,              sys_ni_syscall),     // 32
   GENX_(__NR_access,            sys_access),         // 33
   GENX_(__NR_nice,              sys_nice),           // 34

   GENX_(__NR_ftime,             sys_ni_syscall),     // 35
   GENX_(__NR_sync,              sys_sync),           // 36
   GENXY(__NR_kill,              sys_kill),           // 37
   GENX_(__NR_rename,            sys_rename),         // 38
   GENX_(__NR_mkdir,             sys_mkdir),          // 39

   GENX_(__NR_rmdir,             sys_rmdir),          // 40
   GENXY(__NR_dup,               sys_dup),            // 41
   GENXY(__NR_pipe,              sys_pipe),           // 42
   GENXY(__NR_times,             sys_times),          // 43
   GENX_(__NR_prof,              sys_ni_syscall),     // 44

   GENX_(__NR_brk,               sys_brk),            // 45
   GENX_(__NR_setgid,            sys_setgid16),       // 46
   GENX_(__NR_getgid,            sys_getgid16),       // 47
   //   (__NR_signal,            sys_signal),         // 48 */* (ANSI C)
   GENX_(__NR_geteuid,           sys_geteuid16),      // 49

   GENX_(__NR_getegid,           sys_getegid16),      // 50
   GENX_(__NR_acct,              sys_acct),           // 51
   LINX_(__NR_umount2,           sys_umount),         // 52
   GENX_(__NR_lock,              sys_ni_syscall),     // 53
   GENXY(__NR_ioctl,             sys_ioctl),          // 54

   GENXY(__NR_fcntl,             sys_fcntl),          // 55
   GENX_(__NR_mpx,               sys_ni_syscall),     // 56
   GENXY(__NR_setpgid,           sys_setpgid),        // 57
   GENX_(__NR_ulimit,            sys_ni_syscall),     // 58
   //   (__NR_oldolduname,       sys_olduname),       // 59 Linux -- obsolete

   GENX_(__NR_umask,             sys_umask),          // 60
   GENX_(__NR_chroot,            sys_chroot),         // 61
   //   (__NR_ustat,             sys_ustat)           // 62 SVr4 -- deprecated
   GENXY(__NR_dup2,              sys_dup2),           // 63
   GENX_(__NR_getppid,           sys_getppid),        // 64

   GENX_(__NR_getpgrp,           sys_getpgrp),        // 65
   GENX_(__NR_setsid,            sys_setsid),         // 66
   PLAXY(__NR_sigaction,         sys_sigaction),      // 67
   //   (__NR_sgetmask,          sys_sgetmask),       // 68 */* (ANSI C)
   //   (__NR_ssetmask,          sys_ssetmask),       // 69 */* (ANSI C)

   GENX_(__NR_setreuid,          sys_setreuid16),     // 70
   GENX_(__NR_setregid,          sys_setregid16),     // 71
   GENX_(__NR_sigsuspend,        sys_sigsuspend),     // 72
   GENXY(__NR_sigpending,        sys_sigpending),     // 73
   //   (__NR_sethostname,       sys_sethostname),    // 74 */*

   GENX_(__NR_setrlimit,         sys_setrlimit),      // 75
   GENXY(__NR_getrlimit,         sys_old_getrlimit),  // 76
   GENXY(__NR_getrusage,         sys_getrusage),      // 77
   GENXY(__NR_gettimeofday,      sys_gettimeofday),   // 78
   GENX_(__NR_settimeofday,      sys_settimeofday),   // 79

   GENXY(__NR_getgroups,         sys_getgroups16),    // 80
   GENX_(__NR_setgroups,         sys_setgroups16),    // 81
   PLAX_(__NR_select,            old_select),         // 82
   GENX_(__NR_symlink,           sys_symlink),        // 83
   //   (__NR_oldlstat,          sys_lstat),          // 84 -- obsolete

   GENX_(__NR_readlink,          sys_readlink),       // 85
   //   (__NR_uselib,            sys_uselib),         // 86 */Linux
   //   (__NR_swapon,            sys_swapon),         // 87 */Linux
   //   (__NR_reboot,            sys_reboot),         // 88 */Linux
   //   (__NR_readdir,           old_readdir),        // 89 -- superseded

   GENX_(__NR_mmap,              old_mmap),           // 90
   GENXY(__NR_munmap,            sys_munmap),         // 91
   GENX_(__NR_truncate,          sys_truncate),       // 92
   GENX_(__NR_ftruncate,         sys_ftruncate),      // 93
   GENX_(__NR_fchmod,            sys_fchmod),         // 94

   GENX_(__NR_fchown,            sys_fchown16),       // 95
   GENX_(__NR_getpriority,       sys_getpriority),    // 96
   GENX_(__NR_setpriority,       sys_setpriority),    // 97
   GENX_(__NR_profil,            sys_ni_syscall),     // 98
   GENXY(__NR_statfs,            sys_statfs),         // 99

   GENXY(__NR_fstatfs,           sys_fstatfs),        // 100
   LINX_(__NR_ioperm,            sys_ioperm),         // 101
   GENXY(__NR_socketcall,        sys_socketcall),     // 102
   LINXY(__NR_syslog,            sys_syslog),         // 103
   GENXY(__NR_setitimer,         sys_setitimer),      // 104

   GENXY(__NR_getitimer,         sys_getitimer),      // 105
   GENXY(__NR_stat,              sys_newstat),        // 106
   GENXY(__NR_lstat,             sys_newlstat),       // 107
   GENXY(__NR_fstat,             sys_newfstat),       // 108
   //   (__NR_olduname,          sys_uname),          // 109 -- obsolete

   GENX_(__NR_iopl,              sys_iopl),           // 110
   LINX_(__NR_vhangup,           sys_vhangup),        // 111
   GENX_(__NR_idle,              sys_ni_syscall),     // 112
   //   (__NR_vm86old,           sys_vm86old),        // 113 x86/Linux-only
   GENXY(__NR_wait4,             sys_wait4),          // 114

   //   (__NR_swapoff,           sys_swapoff),        // 115 */Linux 
   LINXY(__NR_sysinfo,           sys_sysinfo),        // 116
   PLAXY(__NR_ipc,               sys_ipc),            // 117
   GENX_(__NR_fsync,             sys_fsync),          // 118
   //   (__NR_sigreturn,         sys_sigreturn),      // 119 ?/Linux

   PLAX_(__NR_clone,             sys_clone),          // 120
   //   (__NR_setdomainname,     sys_setdomainname),  // 121 */*(?)
   GENXY(__NR_uname,             sys_newuname),       // 122
   PLAX_(__NR_modify_ldt,        sys_modify_ldt),     // 123
   LINXY(__NR_adjtimex,          sys_adjtimex),       // 124

   GENXY(__NR_mprotect,          sys_mprotect),       // 125
   GENXY(__NR_sigprocmask,       sys_sigprocmask),    // 126
   // Nb: create_module() was removed 2.4-->2.6
   GENX_(__NR_create_module,     sys_ni_syscall),     // 127
   GENX_(__NR_init_module,       sys_init_module),    // 128
   //   (__NR_delete_module,     sys_delete_module),  // 129 (*/Linux)?

   // Nb: get_kernel_syms() was removed 2.4-->2.6
   GENX_(__NR_get_kernel_syms,   sys_ni_syscall),     // 130
   GENX_(__NR_quotactl,          sys_quotactl),       // 131
   GENX_(__NR_getpgid,           sys_getpgid),        // 132
   GENX_(__NR_fchdir,            sys_fchdir),         // 133
   //   (__NR_bdflush,           sys_bdflush),        // 134 */Linux

   //   (__NR_sysfs,             sys_sysfs),          // 135 SVr4
   LINX_(__NR_personality,       sys_personality),    // 136
   GENX_(__NR_afs_syscall,       sys_ni_syscall),     // 137
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
   GENX_(__NR_munlockall,        sys_munlockall),     // 153
   GENXY(__NR_sched_setparam,    sys_sched_setparam), // 154

   GENXY(__NR_sched_getparam,         sys_sched_getparam),        // 155
   GENX_(__NR_sched_setscheduler,     sys_sched_setscheduler),    // 156
   GENX_(__NR_sched_getscheduler,     sys_sched_getscheduler),    // 157
   GENX_(__NR_sched_yield,            sys_sched_yield),           // 158
   GENX_(__NR_sched_get_priority_max, sys_sched_get_priority_max),// 159

   GENX_(__NR_sched_get_priority_min, sys_sched_get_priority_min),// 160
   //   (__NR_sched_rr_get_interval,  sys_sched_rr_get_interval), // 161 */*
   GENXY(__NR_nanosleep,         sys_nanosleep),      // 162
   GENX_(__NR_mremap,            sys_mremap),         // 163
   LINX_(__NR_setresuid,         sys_setresuid16),    // 164

   LINXY(__NR_getresuid,         sys_getresuid16),    // 165
   //   (__NR_vm86,              sys_vm86),           // 166 x86/Linux-only
   GENX_(__NR_query_module,      sys_ni_syscall),     // 167
   GENXY(__NR_poll,              sys_poll),           // 168
   //   (__NR_nfsservctl,        sys_nfsservctl),     // 169 */Linux

   LINX_(__NR_setresgid,         sys_setresgid16),    // 170
   LINXY(__NR_getresgid,         sys_getresgid16),    // 171
   LINX_(__NR_prctl,             sys_prctl),          // 172
   //   (__NR_rt_sigreturn,      sys_rt_sigreturn),   // 173 x86/Linux only?
   GENXY(__NR_rt_sigaction,      sys_rt_sigaction),   // 174

   GENXY(__NR_rt_sigprocmask,    sys_rt_sigprocmask), // 175
   GENXY(__NR_rt_sigpending,     sys_rt_sigpending),  // 176
   GENXY(__NR_rt_sigtimedwait,   sys_rt_sigtimedwait),// 177
   GENXY(__NR_rt_sigqueueinfo,   sys_rt_sigqueueinfo),// 178
   GENX_(__NR_rt_sigsuspend,     sys_rt_sigsuspend),  // 179

   GENXY(__NR_pread64,           sys_pread64),        // 180
   GENX_(__NR_pwrite64,          sys_pwrite64),       // 181
   GENX_(__NR_chown,             sys_chown16),        // 182
   GENXY(__NR_getcwd,            sys_getcwd),         // 183
   GENXY(__NR_capget,            sys_capget),         // 184

   GENX_(__NR_capset,            sys_capset),         // 185
   GENXY(__NR_sigaltstack,       sys_sigaltstack),    // 186
   LINXY(__NR_sendfile,          sys_sendfile),       // 187
   GENXY(__NR_getpmsg,           sys_getpmsg),        // 188
   GENX_(__NR_putpmsg,           sys_putpmsg),        // 189

   // Nb: we convert vfork() to fork() in VG_(pre_syscall)().
   //   (__NR_vfork,             sys_vfork),          // 190
   GENXY(__NR_ugetrlimit,        sys_getrlimit),      // 191
   GENXY(__NR_mmap2,             sys_mmap2),          // 192
   GENX_(__NR_truncate64,        sys_truncate64),     // 193
   GENX_(__NR_ftruncate64,       sys_ftruncate64),    // 194
   
   GENXY(__NR_stat64,            sys_stat64),         // 195
   GENXY(__NR_lstat64,           sys_lstat64),        // 196
   GENXY(__NR_fstat64,           sys_fstat64),        // 197
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
   //   (__NR_pivot_root,        sys_pivot_root),     // 217 */Linux
   GENXY(__NR_mincore,           sys_mincore),        // 218
   GENX_(__NR_madvise,           sys_madvise),        // 219

   GENXY(__NR_getdents64,        sys_getdents64),     // 220
   GENXY(__NR_fcntl64,           sys_fcntl64),        // 221
   GENX_(222,                    sys_ni_syscall),     // 222
   GENX_(223,                    sys_ni_syscall),     // 223
   //   (__NR_gettid,            sys_gettid),         // 224 */Linux

   //   (__NR_readahead,         sys_readahead),      // 225 */(Linux?)
   GENX_(__NR_setxattr,          sys_setxattr),       // 226
   GENX_(__NR_lsetxattr,         sys_lsetxattr),      // 227
   GENX_(__NR_fsetxattr,         sys_fsetxattr),      // 228
   GENXY(__NR_getxattr,          sys_getxattr),       // 229

   GENXY(__NR_lgetxattr,         sys_lgetxattr),      // 230
   GENXY(__NR_fgetxattr,         sys_fgetxattr),      // 231
   GENXY(__NR_listxattr,         sys_listxattr),      // 232
   GENXY(__NR_llistxattr,        sys_llistxattr),     // 233
   GENXY(__NR_flistxattr,        sys_flistxattr),     // 234

   GENX_(__NR_removexattr,       sys_removexattr),    // 235
   GENX_(__NR_lremovexattr,      sys_lremovexattr),   // 236
   GENX_(__NR_fremovexattr,      sys_fremovexattr),   // 237
   //   (__NR_tkill,             sys_tkill),          // 238 */Linux
   LINXY(__NR_sendfile64,        sys_sendfile64),     // 239

   LINXY(__NR_futex,             sys_futex),             // 240
   GENX_(__NR_sched_setaffinity, sys_sched_setaffinity), // 241
   GENXY(__NR_sched_getaffinity, sys_sched_getaffinity), // 242
   PLAX_(__NR_set_thread_area,   sys_set_thread_area),   // 243
   PLAX_(__NR_get_thread_area,   sys_get_thread_area),   // 244

   LINX_(__NR_io_setup,          sys_io_setup),       // 245
   LINX_(__NR_io_destroy,        sys_io_destroy),     // 246
   LINXY(__NR_io_getevents,      sys_io_getevents),   // 247
   LINX_(__NR_io_submit,         sys_io_submit),      // 248
   LINXY(__NR_io_cancel,         sys_io_cancel),      // 249

   //   (__NR_fadvise64,         sys_fadvise64),      // 250 */(Linux?)
   GENX_(251,                    sys_ni_syscall),     // 251
   GENX_(__NR_exit_group,        sys_exit_group),     // 252
   GENXY(__NR_lookup_dcookie,    sys_lookup_dcookie), // 253
   LINXY(__NR_epoll_create,      sys_epoll_create),   // 254

   LINX_(__NR_epoll_ctl,         sys_epoll_ctl),         // 255
   LINXY(__NR_epoll_wait,        sys_epoll_wait),        // 256
   //   (__NR_remap_file_pages,  sys_remap_file_pages),  // 257 */Linux
   GENX_(__NR_set_tid_address,   sys_set_tid_address),   // 258
   GENXY(__NR_timer_create,      sys_timer_create),      // 259

   GENXY(__NR_timer_settime,     sys_timer_settime),  // (timer_create+1)
   GENXY(__NR_timer_gettime,     sys_timer_gettime),  // (timer_create+2)
   GENX_(__NR_timer_getoverrun,  sys_timer_getoverrun),//(timer_create+3)
   GENX_(__NR_timer_delete,      sys_timer_delete),   // (timer_create+4)
   GENX_(__NR_clock_settime,     sys_clock_settime),  // (timer_create+5)

   GENXY(__NR_clock_gettime,     sys_clock_gettime),  // (timer_create+6)
   GENXY(__NR_clock_getres,      sys_clock_getres),   // (timer_create+7)
   //   (__NR_clock_nanosleep,   sys_clock_nanosleep),// (timer_create+8) */*
   GENXY(__NR_statfs64,          sys_statfs64),       // 268
   GENXY(__NR_fstatfs64,         sys_fstatfs64),      // 269

   //   (__NR_tgkill,            sys_tgkill),         // 270 */Linux
   GENX_(__NR_utimes,            sys_utimes),         // 271
   //   (__NR_fadvise64_64,      sys_fadvise64_64),   // 272 */(Linux?)
   GENX_(__NR_vserver,           sys_ni_syscall),     // 273
   //   (__NR_mbind,             sys_mbind),          // 274 ?/?

   //   (__NR_get_mempolicy,     sys_get_mempolicy),  // 275 ?/?
   //   (__NR_set_mempolicy,     sys_set_mempolicy),  // 276 ?/?
   GENXY(__NR_mq_open,           sys_mq_open),        // 277
   GENX_(__NR_mq_unlink,         sys_mq_unlink),      // (mq_open+1)
   GENX_(__NR_mq_timedsend,      sys_mq_timedsend),   // (mq_open+2)

   GENXY(__NR_mq_timedreceive,   sys_mq_timedreceive),// (mq_open+3)
   GENX_(__NR_mq_notify,         sys_mq_notify),      // (mq_open+4)
   GENXY(__NR_mq_getsetattr,     sys_mq_getsetattr),  // (mq_open+5)
   GENX_(__NR_sys_kexec_load,    sys_ni_syscall),     // 283
};

const UInt VGA_(syscall_table_size) = 
            sizeof(VGA_(syscall_table)) / sizeof(VGA_(syscall_table)[0]);

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
