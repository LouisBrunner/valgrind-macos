
##--------------------------------------------------------------------##
##--- Support for doing system calls.          x86-linux/syscall.S ---##
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

#include "core_asm.h"
#include "vki_unistd.h"

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

	int VG_(clone)(int (*fn)(void *), void *child_stack, int flags, void *arg, 
	               0                  4                  8          12
		       pid_t *child_tid, pid_t *parent_tid)
		       16                20

 */
.globl VG_(clone)
VG_(clone):
#define FSZ	(4+4+4)			/* frame size = retaddr+ebx+edi */
	push	%ebx
	push	%edi
	/* set up child stack with function and arg */
	movl	 4+FSZ(%esp), %ecx	/* child stack */
	movl	12+FSZ(%esp), %ebx	/* fn arg */
	movl	 0+FSZ(%esp), %eax	/* fn */
	lea	-8(%ecx), %ecx		/* make space on stack */
	movl	%ebx, 4(%ecx)		/*   fn arg */
	movl	%eax, 0(%ecx)		/*   fn */

	/* get other args to clone */
	movl	 8+FSZ(%esp), %ebx	/* flags */
	movl	20+FSZ(%esp), %edx	/* parent tid * */
	movl	16+FSZ(%esp), %edi	/* child tid * */
	movl	$__NR_clone, %eax
	int	$0x80
	testl	%eax, %eax
	jnz	1f

	/* CHILD - call thread function */
	popl	%eax
	call	*%eax

	/* exit with result */
	movl	%eax, %ebx
	movl	$__NR_exit, %eax
	int	$0x80

	/* Hm, exit returned */
	ud2
		
1:	/* PARENT or ERROR */
	pop	%edi
	pop	%ebx
	ret

.globl VG_(sigreturn)
VG_(sigreturn):
	movl	$__NR_rt_sigreturn, %eax
	int	$0x80

/* Let the linker know we don't need an executable stack */
.section .note.GNU-stack,"",@progbits

##--------------------------------------------------------------------##
##--- end                                                          ---##
##--------------------------------------------------------------------##
