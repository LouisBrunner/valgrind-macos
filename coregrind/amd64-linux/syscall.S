
##--------------------------------------------------------------------##
##--- Support for doing system calls.        amd64-linux/syscall.S ---##
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
	    void*  tls_ptr      at 8(%rsp)

	System call requires:

	    int    flags	in %rdi
	    void*  child_stack	in %rsi
	    pid_t* parent_tid	in %rdx
	    pid_t* child_tid	in %r10
	    void*  tls_ptr      in %r8
        
 */
.globl VG_(clone)
VG_(clone):
        // set up child stack, temporarily preserving fn and arg
        subq    $16, %rsi               // make space on stack
        movq    %rcx, 8(%rsi)           // save arg
        movq    %rdi, 0(%rsi)           // save fn 
        
        // setup syscall
        movq    $__NR_clone, %rax       // syscall number
        movq    %rdx,     %rdi          // syscall arg1: flags
        // %rsi already setup           // syscall arg2: child_stack
        movq    %r9,      %rdx          // syscall arg3: parent_tid
        movq    %r8,      %r10          // syscall arg4: child_tid
        movq    8(%rsp),  %r8           // syscall arg5: tls_ptr

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

.globl VG_(sigreturn)
VG_(sigreturn):
        movq    $__NR_rt_sigreturn, %rax
        syscall

/*----------------------------------------------------------------*/
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
	should also be in guest_state->guest_RAX.  The syscall result
	is written back to guest_state->guest_RAX on completion.
	
	Returns 0 if the syscall was successfully called (even if the
	syscall itself failed), or a -ve error code if one of the
	sigprocmasks failed (there's no way to determine which one
	failed).

	VGA_(interrupted_syscall)() does the thread state fixup in the
	case where we were interrupted by a signal.
	
	Prototype:

	Int VGA_(_client_syscall)(Int syscallno,		// rdi
				  void* guest_state,		// rsi
				  const vki_sigset_t *sysmask,	// rdx
				  const vki_sigset_t *postmask,	// rcx
				  Int nsigwords)		// r8
				   
*/

/* from vki_arch.h */	
#define VKI_SIG_SETMASK	2
	
.globl VGA_(_client_syscall)
VGA_(_client_syscall):
	/* save callee-saved regs */
	pushq	%rbx
	pushq	%rbp
	pushq	%r12
	pushq	%r13
	pushq	%r14
	pushq	%r15

#define FSZ	((4+1)*4)	/* 4 args + ret addr */

#define PUSH_di_si_dx_cx_8	\
	pushq	%rdi ; 		\
	pushq	%rsi ;		\
	pushq	%rdx ;		\
	pushq	%rcx ;		\
	pushq	%r8

#define	POP_di_si_dx_cx_8	\
	popq	%r8 ;		\
	popq	%rcx ;		\
	popq	%rdx ;		\
	popq	%rsi ;		\
	popq	%rdi

1:	/* Even though we can't take a signal until the sigprocmask completes,
	   start the range early.
	   If eip is in the range [1,2), the syscall hasn't been started yet */

	/* Set the signal mask which should be current during the syscall. */
	/* Save and restore all 5 arg regs round the call.  This is easier
           than figuring out the minimal set to save/restore. */

	PUSH_di_si_dx_cx_8

		movq	$__NR_rt_sigprocmask, %rax	// syscall #
		movq	$VKI_SIG_SETMASK, %rdi		// how
		movq	%rdx, %rsi			// sysmask
		movq	%rcx, %rdx			// postmask
		movq	%r8, %r10			// nsigwords
		syscall

	POP_di_si_dx_cx_8
	
	testl	%eax, %eax
	js	5f	/* sigprocmask failed */

	/* OK, that worked.  Now do the syscall proper. */
	
	PUSH_di_si_dx_cx_8

		movq	%rsi, %rax	/* rax --> VexGuestAMD64State * */
		pushq	%rdi		/* syscallno -> stack */
		movq	OFFSET_amd64_RDI(%rax), %rdi
		movq	OFFSET_amd64_RSI(%rax), %rsi
		movq	OFFSET_amd64_RDX(%rax), %rdx
		movq	OFFSET_amd64_R10(%rax), %r10
		movq	OFFSET_amd64_R8(%rax), %r8
		movq	OFFSET_amd64_R9(%rax), %r9
		popq	%rax	/* syscallno -> %rax */
	
		/* If rip==2, then the syscall was either just about
		   to start, or was interrupted and the kernel was 
		   restarting it. */
2:		syscall
3:		/* In the range [3, 4), the syscall result is in %rax, 
		   but hasn't been committed to RAX. */

	POP_di_si_dx_cx_8

	movq	%rax, OFFSET_amd64_RAX(%rsi)	/* save back to RAX */

4:	/* Re-block signals.  If eip is in [4,5), then the syscall is complete and 
	   we needn't worry about it. */
	PUSH_di_si_dx_cx_8

		movq	$__NR_rt_sigprocmask, %rax	// syscall #
		movq	$VKI_SIG_SETMASK, %rdi		// how
		movq	%rcx, %rsi			// postmask
		xorq	%rdx, %rdx			// NULL
		movq	%r8, %r10			// nsigwords
		syscall

	POP_di_si_dx_cx_8

5:	/* now safe from signals */
		
	popq	%r15
	popq	%r14
	popq	%r13
	popq	%r12
	popq	%rbp
	popq	%rbx
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
VGA_(blksys_setup):	.quad 1b
VGA_(blksys_restart):	.quad 2b
VGA_(blksys_complete):	.quad 3b
VGA_(blksys_committed):	.quad 4b
VGA_(blksys_finished):	.quad 5b
.previous

/* Let the linker know we don't need an executable stack */
.section .note.GNU-stack,"",@progbits

##--------------------------------------------------------------------##
##--- end                                                          ---##
##--------------------------------------------------------------------##
