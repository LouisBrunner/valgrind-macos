
##--------------------------------------------------------------------##
##--- Support for doing system calls.        amd64-linux/syscall.S ---##
##--------------------------------------------------------------------##

/*
  This file is part of Valgrind, a dynamic binary instrumentation
  framework.

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


/*
        Perform a clone system call.  clone is strange because it has
        fork()-like return-twice semantics, so it needs special
        handling here.

	Upon entry, we have:

	    int (*fn)(void*)	in %rdi
	    void*  child_stack	in %rsi
	    int    flags	in %rdx
	    void*  arg		in %rcx
	    pid_t* child_tid	in %r8
	    pid_t* parent_tid	in %r9
 */
.globl VG_(clone)
VG_(clone):
        // set up child stack, temporarily preserving fn and arg
        subq    $16, %rsi               // make space on stack
        movq    %rcx, 8(%rsi)           // save arg
        movq    %rdi, 0(%rsi)           // save fn 
        
        // setup syscall
        movl    $__NR_clone, %eax       // syscall number
        movq    %rdx, %rdi              // syscall arg1: flags
        // %rsi already setup           // syscall arg2: child_stack
        movq    %r9,  %rdx              // syscall arg3: parent_tid
        movq    %r8,  %rcx              // syscall arg4: child_tid

        syscall                         // clone()

        testq   %rax, %rax              // child if retval == 0
        jnz     1f

        // CHILD - call thread function
        pop     %rax                    // pop fn
        pop     %rdi                    // pop fn arg1: arg
        call    *%rax                   // call fn

        // exit with result
        movq    %rax, %rdi              // arg1: return value from fn
        movq    $__NR_exit, %rax

        syscall

        // Exit returned?!
        ud2

1:      // PARENT or ERROR
        ret



# XXX: must reinstate comments also -- see x86-linux/syscall.S

.globl VG_(sigreturn)
VG_(sigreturn):
	ud2

/* Let the linker know we don't need an executable stack */
.section .note.GNU-stack,"",@progbits

##--------------------------------------------------------------------##
##--- end                                                          ---##
##--------------------------------------------------------------------##
