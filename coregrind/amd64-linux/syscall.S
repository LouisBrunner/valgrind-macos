
##--------------------------------------------------------------------##
##--- Support for doing system calls.        amd64-linux/syscall.S ---##
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

/*
	Perform a Linux syscall with the "syscall" instruction.
	
	Incoming args (syscall number + up to 6 args) come in 
	%rdi, %rsi, %rdx, %rcx, %r8, %r9, and the last one on the stack
	(ie. the C calling convention).

	They are passed to the syscall in the regs
	%rdi, %rsi, %rdx, %r10, %r8, %r9 (yes, really %r10, not %rcx), ie. the
	kernel's syscall calling convention.

	%rax holds the syscall number and gets the return value.
	%rcx and %r11 are clobbered by the syscall;  no matter, they
	are caller-save (the syscall clobbers no callee-save regs, so
	we don't have to do any register saving/restoring).

	This has no effect on the virtual machine; the expectation is
	that the syscall mechanism makes no useful changes to any
	register except %rax, which is returned.
*/
.globl	VG_(do_syscall)
VG_(do_syscall):
	# Convert function calling convention --> syscall calling convention
	movq	%rdi, %rax
	movq	%rsi, %rdi
	movq	%rdx, %rsi
	movq	%rcx, %rdx
	movq	%r8,  %r10
	movq	%r9,  %r8
	movq    8(%rsp), %r9		# last arg from stack
	syscall
	ret



# XXX: must reinstate comments also -- see x86-linux/syscall.S

.globl VG_(clone)
VG_(clone):
	ud2

.globl VG_(sigreturn)
VG_(sigreturn):
	ud2

/* Let the linker know we don't need an executable stack */
.section .note.GNU-stack,"",@progbits

##--------------------------------------------------------------------##
##--- end                                                          ---##
##--------------------------------------------------------------------##
