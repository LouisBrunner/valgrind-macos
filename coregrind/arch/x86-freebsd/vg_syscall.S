
##--------------------------------------------------------------------##
##--- Support for doing system calls.                              ---##
##---                                                 vg_syscall.S ---##
##--------------------------------------------------------------------##

/*
  This file is part of Valgrind, an extensible x86 protected-mode
  emulator for monitoring program execution on x86-Unixes.

  Copyright (C) 2000-2004 Julian Seward 
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

#include "vg_constants.h"
#include "vg_unistd.h"

.globl	VG_(do_syscall)

/*
	Perform a Linux syscall with int 0x80
	
	Syscall args are passed on the stack
	Int VG_(do_syscall)(Int syscall_no, ...)

	This has no effect on the virtual machine; the expectation is
	that the syscall mechanism makes no useful changes to any
	register except %eax, which is returned.
 */
VG_(do_syscall):
	pop	%ecx
	pop	%eax
	push	%ecx
	int	$0x80
	push	%ecx
	jae	1f
	movl	$-1,%eax
1:
	ret

/*
	Perform a FreeBSD syscall with int 0x80, returning error flag

	Syscall args are passed on the stack
	Int VG_(do_syscall_err)(Int syscall_no, UInt edx, UInt* eflags, ...)

	This has no effect on the virtual machine; the expectation is
	that the syscall mechanism makes no useful changes to any
	register except %eax and %edx, which are returned.
 */
.globl	VG_(do_syscall_err)
VG_(do_syscall_err):
	movl	12(%esp),%ecx
	andb	$254,(%ecx)
	movl	(%esp),%eax
	movl	%eax,12(%esp)
	movl	4(%esp),%eax
	movl	8(%esp),%edx
	addl	$12,%esp
	int	$0x80
	jae	1f
	orb	$1,(%ecx)
1:
	movl	(%esp),%ecx
	push	%ecx
	push	%ecx
	push	%ecx
	ret

/*
	Perform a clone system call.  clone is strange because it has
	fork()-like return-twice semantics, so it needs special
	handling here.

	int VG_(clone)(int (*fn)(void *), void *child_stack, int flags, void *arg, 
	               0                  4                  8          12
		       pid_t *child_tid, pid_t *parent_tid)
		       16                20

 */
.globl VG_(clone)
VG_(clone):
	ret

/*
 *                8      12          16         20
 * rfork_thread(flags, stack_addr, start_fnc, start_arg);
 *
 * flags:		Flags to rfork system call.  See rfork(2).
 * stack_addr:		Top of stack for thread.
 * start_fnc:		Address of thread function to call in child.
 * start_arg:		Argument to pass to the thread function in child.
 */

.globl VG_(rfork_thread)
VG_(rfork_thread):
	pushl	%ebp
	movl	%esp, %ebp
	pushl	%esi

	/*
	 * Push thread info onto the new thread's stack
	 */
	movl	12(%ebp), %esi	# get stack addr

	subl	$4, %esi
	movl	20(%ebp), %eax	# get start argument
	movl	%eax, (%esi)

	subl	$4, %esi
	movl	16(%ebp), %eax	# get start thread address
	movl	%eax, (%esi)

	/*
	 * Prepare and execute the thread creation syscall
	 */
	pushl	8(%ebp)
	pushl	$0
	movl	$__NR_rfork, %eax
	int	$0x80
	jb 	2f

	/*
	 * Check to see if we are in the parent or child
	 */
	cmpl	$0, %edx
	jnz	1f
	addl	$8, %esp
	popl	%esi
	movl	%ebp, %esp
	popl	%ebp
	ret
	.p2align 2

	/*
	 * If we are in the child (new thread), then
	 * set-up the call to the internal subroutine.  If it
	 * returns, then call __exit.
	 */
1:
	movl	%esi,%esp
	popl	%eax
	call	*%eax
	addl	$4, %esp

	/*
	 * Exit system call
	 */
	pushl	%eax
	pushl	$0
	movl	$__NR_exit, %eax
	int	$0x80

	/*
	 * Branch here if the thread creation fails:
	 */
2:
	movl	$-1,%eax
	ret
	
.globl VG_(pipe)
VG_(pipe):
	mov	$__NR_pipe,%eax
	int	$0x80
	jb	1f
	movl	4(%esp),%ecx
	movl	%eax,(%ecx)
	movl	%edx,4(%ecx)
	movl	$0,%eax
	ret
1:
	negl	%eax
	ret


##--------------------------------------------------------------------##
##--- end                                             vg_syscall.S ---##
##--------------------------------------------------------------------##
