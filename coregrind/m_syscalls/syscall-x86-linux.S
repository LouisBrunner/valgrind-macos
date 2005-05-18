
##--------------------------------------------------------------------##
##--- Support for doing system calls.          syscall-x86-linux.S ---##
##--------------------------------------------------------------------##

/*
  This file is part of Valgrind, a dynamic binary instrumentation
  framework.

  Copyright (C) 2000-2005 Julian Seward 
     jseward@acm.org

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

#include "core_asm.h"
#include "vki_unistd.h"
#include "libvex_guest_offsets.h"
		
.globl	VG_(do_syscall)

/*
	Perform a Linux syscall with int 0x80
	
	Syscall args are passed on the stack
	Int VG_(do_syscall)(Int syscall_no, UWord a1, UWord a2, UWord a3,
					    UWord a4, UWord a5, UWord a6)

	This has no effect on the virtual machine; the assumption is
	that the syscall mechanism makes no useful changes to any
	register except %eax, which is returned. (Some kernels will
	change other registers randomly, but they're just compiler
	spill values.)
 */
VG_(do_syscall):
	push	%esi
	push	%edi
	push	%ebx
	push	%ebp
	movl	16+ 4(%esp),%eax
	movl	16+ 8(%esp),%ebx
	movl	16+12(%esp),%ecx
	movl	16+16(%esp),%edx
	movl	16+20(%esp),%esi
	movl	16+24(%esp),%edi
	movl	16+28(%esp),%ebp
	int	$0x80
	popl	%ebp
	popl	%ebx
	popl	%edi
	popl	%esi
	ret

/*
	Perform a clone system call.  clone is strange because it has
	fork()-like return-twice semantics, so it needs special
	handling here.

	Upon entry, we have:

	    int (fn)(void*)	in  0+FSZ(%esp)
	    void* child_stack   in  4+FSZ(%esp)
	    int flags           in  8+FSZ(%esp)
	    void* arg           in 12+FSZ(%esp)
	    pid_t* child_tid    in 16+FSZ(%esp)
	    pid_t* parent_tid   in 20+FSZ(%esp)
	    void* tls_ptr       in 24+FSZ(%esp)

	System call requires:

	    int    $__NR_clone  in %eax
	    int    flags	in %ebx
	    void*  child_stack	in %ecx
	    pid_t* parent_tid	in %edx
	    pid_t* child_tid	in %edi
	    void*  tls_ptr      in %esi
 */
.globl VG_(clone)
VG_(clone):
#define FSZ	(4+4+4)			/* frame size = retaddr+ebx+edi */
	push	%ebx
	push	%edi
	/* set up child stack with function and arg */
	movl	 4+FSZ(%esp), %ecx	/* syscall arg2: child stack */
	movl	12+FSZ(%esp), %ebx	/* fn arg */
	movl	 0+FSZ(%esp), %eax	/* fn */
	lea	-8(%ecx), %ecx		/* make space on stack */
	movl	%ebx, 4(%ecx)		/*   fn arg */
	movl	%eax, 0(%ecx)		/*   fn */

	/* get other args to clone */
	movl	 8+FSZ(%esp), %ebx	/* syscall arg1: flags */
	movl	20+FSZ(%esp), %edx	/* syscall arg3: parent tid * */
	movl	16+FSZ(%esp), %edi	/* syscall arg4: child tid * */
	movl	24+FSZ(%esp), %esi	/* syscall arg5: tls_ptr * */
	movl	$__NR_clone, %eax
	int	$0x80			/* clone() */
	testl	%eax, %eax		/* child if retval == 0 */
	jnz	1f

	/* CHILD - call thread function */
	popl	%eax
	call	*%eax			/* call fn */

	/* exit with result */
	movl	%eax, %ebx		/* arg1: return value from fn */
	movl	$__NR_exit, %eax
	int	$0x80

	/* Hm, exit returned */
	ud2
		
1:	/* PARENT or ERROR */
	pop	%edi
	pop	%ebx
	ret
#undef FSZ
	
.globl VG_(sigreturn)
VG_(sigreturn):
	movl	$__NR_rt_sigreturn, %eax
	int	$0x80


/*
	Perform a syscall for the client.  This will run a syscall
	with the client's specific per-thread signal mask.
	
	The structure of this function is such that, if the syscall is
	interrupted by a signal, we can determine exactly what
	execution state we were in with respect to the execution of
	the syscall by examining the value of %eip in the signal
	handler.  This means that we can always do the appropriate
	thing to precisely emulate the kernel's signal/syscall
	interactions.

	The syscall number is taken from the argument, even though it
	should also be in regs->m_eax.  The syscall result is written
	back to regs->m_eax on completion.
	
	Returns 0 if the syscall was successfully called (even if the
	syscall itself failed), or a -ve error code if one of the
	sigprocmasks failed (there's no way to determine which one
	failed).

	VGA_(interrupted_syscall)() does the thread state fixup in the
	case where we were interrupted by a signal.
	
	Prototype:

	Int VGA_(_client_syscall)(Int syscallno,		// 0
				  void* guest_state,		// 4
				  const vki_sigset_t *sysmask,	// 8
				  const vki_sigset_t *postmask,	// 12
				  Int nsigwords)		// 16
				   
*/

/* from vki_arch.h */	
#define VKI_SIG_SETMASK	2
	
.globl VGA_(_client_syscall)
VGA_(_client_syscall):
	/* save callee-saved regs */
	push	%esi
	push	%edi
	push	%ebx
	push	%ebp
#define FSZ	((4+1)*4)	/* 4 args + ret addr */

1:	/* Even though we can't take a signal until the sigprocmask completes,
	   start the range early.
	   If eip is in the range [1,2), the syscall hasn't been started yet */

	/* Set the signal mask which should be current during the syscall. */
	movl	$__NR_rt_sigprocmask, %eax
	movl	$VKI_SIG_SETMASK, %ebx
	movl	8+FSZ(%esp), %ecx
	movl	12+FSZ(%esp), %edx
	movl	16+FSZ(%esp), %esi
	int	$0x80
	testl	%eax, %eax
	js	5f	/* sigprocmask failed */
	
	movl	 4+FSZ(%esp), %eax	/* eax == ThreadState * */

	movl	 OFFSET_x86_EBX(%eax), %ebx
	movl	 OFFSET_x86_ECX(%eax), %ecx
	movl	 OFFSET_x86_EDX(%eax), %edx
	movl	 OFFSET_x86_ESI(%eax), %esi
	movl	 OFFSET_x86_EDI(%eax), %edi
	movl	 OFFSET_x86_EBP(%eax), %ebp
	movl	 0+FSZ(%esp), %eax	/* use syscallno argument rather than thread EAX */
	
	/* If eip==2, then the syscall was either just about to start, 
	   or was interrupted and the kernel was restarting it. */
2:	int	$0x80
3:	/* In the range [3, 4), the syscall result is in %eax, but hasn't been
	   committed to EAX. */
	movl	4+FSZ(%esp), %ebx
	movl	%eax, OFFSET_x86_EAX(%ebx)	/* save back to EAX */

4:	/* Re-block signals.  If eip is in [4,5), then the syscall is complete and 
	   we needn't worry about it. */
	movl	$__NR_rt_sigprocmask, %eax
	movl	$VKI_SIG_SETMASK, %ebx
	movl	12+FSZ(%esp), %ecx
	xorl	%edx, %edx
	movl	16+FSZ(%esp), %esi
	int	$0x80

5:	/* now safe from signals */
		
	popl	%ebp
	popl	%ebx
	popl	%edi
	popl	%esi
#undef FSZ
	ret

.section .rodata
/* export the ranges so that VGA_(interrupted_syscall) can do the
	right thing */
	
.globl VGA_(blksys_setup)
.globl VGA_(blksys_restart)
.globl VGA_(blksys_complete)
.globl VGA_(blksys_committed)
.globl VGA_(blksys_finished)
VGA_(blksys_setup):	.long 1b
VGA_(blksys_restart):	.long 2b
VGA_(blksys_complete):	.long 3b
VGA_(blksys_committed):	.long 4b
VGA_(blksys_finished):	.long 5b
.previous
	
/* Let the linker know we don't need an executable stack */
.section .note.GNU-stack,"",@progbits

##--------------------------------------------------------------------##
##--- end                                                          ---##
##--------------------------------------------------------------------##
