
/*--------------------------------------------------------------------*/
/*--- x86/Linux-specific syscalls, etc.       x86-linux/syscalls.c ---*/
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


/* We need our own copy of VG_(do_syscall)() to handle a special
   race-condition.  If we've got signals unblocked, and we take a
   signal in the gap either just before or after the syscall, we may
   end up not running the syscall at all, or running it more than
   once.

   The solution is to make the signal handler derive the proxy's
   precise state by looking to see which eip it is executing at
   exception time.

   Ranges:

   vga_sys_before ... vga_sys_restarted:
	Setting up register arguments and running state.  If
	interrupted, then the syscall should be considered to return
	ERESTARTSYS.

   vga_sys_restarted:
	If interrupted and eip==vga_sys_restarted, then either the syscall
	was about to start running, or it has run, was interrupted and
	the kernel wants to restart it.  eax still contains the
	syscall number.  If interrupted, then the syscall return value
	should be ERESTARTSYS.

   vga_sys_after:
	If interrupted and eip==vga_sys_after, the syscall either just
	finished, or it was interrupted and the kernel doesn't want to
	restart it.  Either way, eax equals the correct return value
	(either the actual return value, or EINTR).

   vga_sys_after ... vga_sys_done:
	System call is complete, but the state hasn't been updated,
	nor has the result been written back.  eax contains the return
	value.
*/
extern void do_thread_syscall(Int sys, 
			      Int arg1, Int arg2, Int arg3, Int arg4,
                              Int arg5, Int arg6,
			      Int *result, enum PXState *statep,
                              enum PXState poststate);

asm(
".text\n"
"	.type do_thread_syscall,@function\n"

"do_thread_syscall:\n"
"	push	%esi\n"
"	push	%edi\n"
"	push	%ebx\n"
"	push	%ebp\n"
".vga_sys_before:\n"
"	movl	16+ 4(%esp),%eax\n" /* syscall */
"	movl	16+ 8(%esp),%ebx\n" /* arg1 */
"	movl	16+12(%esp),%ecx\n" /* arg2 */
"	movl	16+16(%esp),%edx\n" /* arg3 */
"	movl	16+20(%esp),%esi\n" /* arg4 */
"	movl	16+24(%esp),%edi\n" /* arg5 */
"	movl	16+28(%esp),%ebp\n" /* arg6 */
".vga_sys_restarted:\n"
"	int	$0x80\n"
".vga_sys_after:\n"
"	movl	16+32(%esp),%ebx\n"	/* ebx = Int *res */
"	movl	%eax, (%ebx)\n"		/* write the syscall retval */

"	movl	16+36(%esp),%ebx\n"	/* ebx = enum PXState * */
"	testl	%ebx, %ebx\n"
"	jz	1f\n"

"	movl	16+40(%esp),%ecx\n"	/* write the post state (must be after retval write) */
"	movl	%ecx,(%ebx)\n"

".vga_sys_done:\n"				/* OK, all clear from here */
"1:	popl	%ebp\n"
"	popl	%ebx\n"
"	popl	%edi\n"
"	popl	%esi\n"
"	ret\n"
"	.size do_thread_syscall,.-do_thread_syscall\n"
".previous\n"

".section .rodata\n"
"       .globl  vga_sys_before\n"
"vga_sys_before:	.long	.vga_sys_before\n"
"       .globl  vga_sys_restarted\n"
"vga_sys_restarted:	.long	.vga_sys_restarted\n"
"       .globl  vga_sys_after\n"
"vga_sys_after:	.long	.vga_sys_after\n"
"       .globl  vga_sys_done\n"
"vga_sys_done:	.long	.vga_sys_done\n"
".previous\n"
);

/* Run a syscall for a particular thread, getting the arguments from
   the thread's registers, and returning the result in the thread's
   eax.

   Assumes that the only thread state which matters is the contents of
   %eax-%ebp and the return value in %eax.
 */
void VGA_(thread_syscall)(Int syscallno, ThreadState *tst, 
                          enum PXState *state , enum PXState poststate)
{
   do_thread_syscall(syscallno,   /* syscall no. */
		     tst->arch.m_ebx,  /* arg 1 */
		     tst->arch.m_ecx,  /* arg 2 */
		     tst->arch.m_edx,  /* arg 3 */
		     tst->arch.m_esi,  /* arg 4 */
		     tst->arch.m_edi,  /* arg 5 */
		     tst->arch.m_ebp,  /* arg 6 */
		     &tst->arch.m_eax, /* result */
		     state,	  /* state to update */
		     poststate);  /* state when syscall has finished */
}



// Back up to restart a system call.
void VGA_(restart_syscall)(arch_thread_t *tst)
{
   tst->m_eip -= 2;             // sizeof(int $0x80)

   /* Make sure our caller is actually sane, and we're really backing
      back over a syscall.

      int $0x80 == CD 80 
   */
   {
      UChar *p = (UChar *)tst->m_eip;
      
      if (p[0] != 0xcd || p[1] != 0x80)
         VG_(message)(Vg_DebugMsg,
                      "?! restarting over syscall at %p %02x %02x\n",
                      tst->m_eip, p[0], p[1]); 

      vg_assert(p[0] == 0xcd && p[1] == 0x80);
   }
}

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
