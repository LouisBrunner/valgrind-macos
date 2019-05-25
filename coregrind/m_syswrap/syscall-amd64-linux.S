
/*--------------------------------------------------------------------*/
/*--- Support for doing system calls.        syscall-amd64-linux.S ---*/
/*--------------------------------------------------------------------*/

/*
  This file is part of Valgrind, a dynamic binary instrumentation
  framework.

  Copyright (C) 2000-2017 Julian Seward 
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
  along with this program; if not, see <http://www.gnu.org/licenses/>.

  The GNU General Public License is contained in the file COPYING.
*/

#include "pub_core_basics_asm.h"

#if defined(VGP_amd64_linux)

#include "pub_core_vkiscnums_asm.h"
#include "libvex_guest_offsets.h"


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

	VG_(fixup_guest_state_after_syscall_interrupted) does the
	thread state fixup in the case where we were interrupted by a
	signal.
	
	Prototype:

	Int ML_(do_syscall_for_client_WRK(
	                          Int syscallno,		// rdi
				  void* guest_state,		// rsi
				  const vki_sigset_t *sysmask,	// rdx
				  const vki_sigset_t *postmask,	// rcx
				  Int sigsetSzB)		// r8
				   
*/

/* from vki_arch.h */	
#define VKI_SIG_SETMASK	2
	
.globl ML_(do_syscall_for_client_WRK)
ML_(do_syscall_for_client_WRK):
	.cfi_startproc
	/* save callee-saved regs */
	pushq	%rbx
	.cfi_adjust_cfa_offset 8
	.cfi_offset %rbx, -16
	pushq	%rbp
	.cfi_adjust_cfa_offset 8
	.cfi_offset %rbp, -24
	pushq	%r12
	.cfi_adjust_cfa_offset 8
	.cfi_offset %r12, -32
	pushq	%r13
	.cfi_adjust_cfa_offset 8
	.cfi_offset %r13, -40
	pushq	%r14
	.cfi_adjust_cfa_offset 8
	.cfi_offset %r14, -48
	pushq	%r15
	.cfi_adjust_cfa_offset 8
	.cfi_offset %r15, -56

#define FSZ	((4+1)*4)	/* 4 args + ret addr */

#define PUSH_di_si_dx_cx_8	   \
	pushq	%rdi ; 		   \
	.cfi_adjust_cfa_offset 8 ; \
	pushq	%rsi ;		   \
	.cfi_adjust_cfa_offset 8 ; \
	pushq	%rdx ;		   \
	.cfi_adjust_cfa_offset 8 ; \
	pushq	%rcx ;		   \
	.cfi_adjust_cfa_offset 8 ; \
	pushq	%r8 ;              \
	.cfi_adjust_cfa_offset 8

#define	POP_di_si_dx_cx_8	    \
	popq	%r8 ;		    \
	.cfi_adjust_cfa_offset -8 ; \
	popq	%rcx ;		    \
	.cfi_adjust_cfa_offset -8 ; \
	popq	%rdx ;		    \
	.cfi_adjust_cfa_offset -8 ; \
	popq	%rsi ;		    \
	.cfi_adjust_cfa_offset -8 ; \
	popq	%rdi ;              \
	.cfi_adjust_cfa_offset -8

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
	movq	%r8, %r10			// sigsetSzB
	syscall

	POP_di_si_dx_cx_8
	
	testq	%rax, %rax
	js	7f	/* sigprocmask failed */

	/* OK, that worked.  Now do the syscall proper. */
	
	PUSH_di_si_dx_cx_8

	movq	%rsi, %rax	/* rax --> VexGuestAMD64State * */
	pushq	%rdi		/* syscallno -> stack */
	.cfi_adjust_cfa_offset 8
	movq	OFFSET_amd64_RDI(%rax), %rdi
	movq	OFFSET_amd64_RSI(%rax), %rsi
	movq	OFFSET_amd64_RDX(%rax), %rdx
	movq	OFFSET_amd64_R10(%rax), %r10
	movq	OFFSET_amd64_R8(%rax), %r8
	movq	OFFSET_amd64_R9(%rax), %r9
	popq	%rax	/* syscallno -> %rax */
	.cfi_adjust_cfa_offset -8
	
	/* If rip==2, then the syscall was either just about
	   to start, or was interrupted and the kernel was 
	   restarting it. */
2:	syscall
3:	/* In the range [3, 4), the syscall result is in %rax, 
	   but hasn't been committed to RAX. */

	POP_di_si_dx_cx_8

	movq	%rax, OFFSET_amd64_RAX(%rsi)	/* save back to RAX */

4:	/* Re-block signals.  If eip is in [4,5), then the syscall 
	   is complete and we needn't worry about it. */

	PUSH_di_si_dx_cx_8

	movq	$__NR_rt_sigprocmask, %rax	// syscall #
	movq	$VKI_SIG_SETMASK, %rdi		// how
	movq	%rcx, %rsi			// postmask
	xorq	%rdx, %rdx			// NULL
	movq	%r8, %r10			// sigsetSzB
	syscall

	POP_di_si_dx_cx_8

	testq	%rax, %rax
	js	7f	/* sigprocmask failed */

5:	/* now safe from signals */
	movq	$0, %rax	/* SUCCESS */
	popq	%r15
	.cfi_adjust_cfa_offset -8
	popq	%r14
	.cfi_adjust_cfa_offset -8
	popq	%r13
	.cfi_adjust_cfa_offset -8
	popq	%r12
	.cfi_adjust_cfa_offset -8
	popq	%rbp
	.cfi_adjust_cfa_offset -8
	popq	%rbx
	.cfi_adjust_cfa_offset -8
	ret
	.cfi_adjust_cfa_offset 6*8

7:	/* failure:	 return 0x8000 | error code */
	negq	%rax
	andq	$0x7FFF, %rax
	orq	$0x8000, %rax
	popq	%r15
	.cfi_adjust_cfa_offset -8
	popq	%r14
	.cfi_adjust_cfa_offset -8
	popq	%r13
	.cfi_adjust_cfa_offset -8
	popq	%r12
	.cfi_adjust_cfa_offset -8
	popq	%rbp
	.cfi_adjust_cfa_offset -8
	popq	%rbx
	.cfi_adjust_cfa_offset -8
	ret
	.cfi_endproc
#undef FSZ

.section .rodata
/* export the ranges so that
   VG_(fixup_guest_state_after_syscall_interrupted) can do the
   right thing */
	
.globl ML_(blksys_setup)
.globl ML_(blksys_restart)
.globl ML_(blksys_complete)
.globl ML_(blksys_committed)
.globl ML_(blksys_finished)
ML_(blksys_setup):	.quad 1b
ML_(blksys_restart):	.quad 2b
ML_(blksys_complete):	.quad 3b
ML_(blksys_committed):	.quad 4b
ML_(blksys_finished):	.quad 5b
.previous

#endif // defined(VGP_amd64_linux)

/* Let the linker know we don't need an executable stack */
MARK_STACK_NO_EXEC

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
