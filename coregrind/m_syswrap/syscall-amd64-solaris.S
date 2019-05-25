
/*--------------------------------------------------------------------*/
/*--- Support for doing system calls.      syscall-amd64-solaris.S ---*/
/*--------------------------------------------------------------------*/

/*
  This file is part of Valgrind, a dynamic binary instrumentation
  framework.

  Copyright (C) 2014-2017 Petr Pavlu
     setup@dagobah.cz

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

#if defined(VGP_amd64_solaris)

#include "pub_core_vkiscnums_asm.h"
#include "libvex_guest_offsets.h"

/* From vki-solaris.h, checked at startup by m_vki.c. */
#define VKI_SIG_SETMASK 3

/* Prototype:
   Int ML_(do_syscall_for_client_WRK)(
      Int syscallno,			// %rdi = %rbp-48
      void *guest_state,		// %rsi = %rbp-40
      const vki_sigset_t *sysmask,	// %rdx = %rbp-32
      const vki_sigset_t *postmask,	// %rcx = %rbp-24
      UChar *cflag)			// %r8 = %rbp-16
*/

.macro ESTABLISH_STACKFRAME
	/* Establish stack frame. */
	pushq	%rbp
	movq	%rsp, %rbp
	pushq	%rbx				/* save %rbx */

	/* We'll use %rbx instead of %rbp to address the stack frame after the
	   door syscall is finished because %rbp is cleared by the syscall. */
	movq	%rsp, %rbx			/* %rbx = %rbp - 8 */

	/* Push the parameters on the stack. */
	pushq	%r8				/* store %r8 at %rbp-16 */
	pushq	%rcx				/* store %rcx at %rbp-24 */
	pushq	%rdx				/* store %rdx at %rbp-32 */
	pushq	%rsi				/* store %rsi at %rbp-40 */
	pushq	%rdi				/* store %rdi at %rbp-48 */
.endm

.macro UNBLOCK_SIGNALS
	/* Set the signal mask which should be current during the syscall. */
	/* Set up for sigprocmask(SIG_SETMASK, sysmask, postmask). */
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rsi
	movq	$VKI_SIG_SETMASK, %rdi
	movq	$__NR_sigprocmask, %rax
	syscall
	jc	sigprocmask_failed		/* sigprocmask failed */
.endm

.macro REBLOCK_SIGNALS
	/* Set up for sigprocmask(SIG_SETMASK, postmask, NULL). */
	movq	$0, %rdx
	movq	-24(%rbp), %rsi
	movq	$VKI_SIG_SETMASK, %rdi
	movq	$__NR_sigprocmask, %rax
	syscall
	/* The syscall above changes the carry flag.  This means that if the
	   syscall fails and we receive an interrupt after it then we've got
	   an invalid carry flag value in the fixup code.  We don't care about
	   it because this syscall should never fail and if it does then we're
	   going to stop Valgrind anyway. */
	jc	sigprocmask_failed		/* sigprocmask failed */
.endm

.macro SIMPLE_RETURN
	xorq	%rax, %rax			/* SUCCESS */
	movq	-8(%rbp), %rbx			/* restore %rbx */
	movq	%rbp, %rsp
	popq	%rbp
	ret
.endm

sigprocmask_failed:
	/* Failure: return 0x8000 | error code. */
	andq	$0x7FFF, %rax
	orq	$0x8000, %rax
	movq	-8(%rbp), %rbx			/* restore %rbx */
	movq	%rbp, %rsp
	popq	%rbp
	ret

.globl ML_(do_syscall_for_client_WRK)
ML_(do_syscall_for_client_WRK):
	ESTABLISH_STACKFRAME

1:	/* Even though we can't take a signal until the sigprocmask completes,
	   start the range early.  If %rip is in the range [1, 2), the syscall
	   hasn't been started yet. */
	UNBLOCK_SIGNALS

	/* Copy syscall parameters. */
	/* do_syscall8 */
	/* 6 register parameters. */
	movq	-40(%rbp), %rax
	movq	OFFSET_amd64_RDI(%rax), %rdi
	movq	OFFSET_amd64_RSI(%rax), %rsi
	movq	OFFSET_amd64_RDX(%rax), %rdx
	movq	OFFSET_amd64_R10(%rax), %r10
	movq	OFFSET_amd64_R8(%rax), %r8
	movq	OFFSET_amd64_R9(%rax), %r9
	/* 2 stack parameters. */
	movq	OFFSET_amd64_RSP(%rax), %rax
	movq	16(%rax), %r11
	pushq	%r11
	movq	8(%rax), %r11
	pushq	%r11
	/* Return address. */
	movq	0(%rax), %r11
	pushq	%r11

	/* Put syscall number in %rax. */
	movq	-48(%rbp), %rax

	/* Do the syscall.  Note that the Solaris kernel doesn't directly
	   restart syscalls! */
	syscall

2:	/* In the range [2, 3), the syscall result is in %rax and %rdx and C,
	   but hasn't been committed to the thread state.  If we get
	   interrupted in this section then we'll just use values saved in the
	   ucontext structure.

	   Important note for this and the following section: Don't add here
	   any code that alters the carry flag or worse, call any function.
	   That would completely break the fixup after an interrupt. */
	movq	-40(%rbp), %rcx
	movq	%rax, OFFSET_amd64_RAX(%rcx)	/* save %rax to VEX */
	movq	%rdx, OFFSET_amd64_RDX(%rcx)	/* save %rdx to VEX */
	movq	-16(%rbp), %rcx
	setc	0(%rcx)				/* save returned carry flag */

3:	/* Re-block signals. If %rip is in [3, 4), then the syscall is
	   complete and we do not need to worry about it.  We have to only
	   correctly save the carry flag.  If we get interrupted in this
	   section then we just have to propagate the carry flag from the
	   ucontext structure to the thread state, %rax and %rdx values are
	   already saved. */
	REBLOCK_SIGNALS

4:	/* Now safe from signals. */
	SIMPLE_RETURN

.section .rodata
/* Export the ranges so that
   VG_(fixup_guest_state_after_syscall_interrupted) can do the right thing. */

.globl ML_(blksys_setup)
.globl ML_(blksys_complete)
.globl ML_(blksys_committed)
.globl ML_(blksys_finished)
ML_(blksys_setup):	.quad 1b
ML_(blksys_complete):	.quad 2b
ML_(blksys_committed):	.quad 3b
ML_(blksys_finished):	.quad 4b
.previous

/* Prototype:
   Int ML_(do_syscall_for_client_dret_WRK)(
      Int syscallno,			// %rdi = %rbp-48 = %rbx-48+8
      void *guest_state,		// %rsi = %rbp-40 = %rbx-40+8
      const vki_sigset_t *sysmask,	// %rdx = %rbp-32 = %rbx-32+8
      const vki_sigset_t *postmask,	// %rcx = %rbp-24 = %rbx-24+8
      UChar *cflag)			// %r8 = %rbp-16 = %rbx-16+8
*/

/* Door_return is a very special call because the data are stored by the
   kernel directly on the stack and the stack pointer is appropriately
   modified by the kernel.  Therefore we switch to the client stack before
   doing the syscall, this is relatively trivial but an extra care has to be
   taken when we get interrupted at some point. */

.globl ML_(do_syscall_for_client_dret_WRK)
ML_(do_syscall_for_client_dret_WRK):
	ESTABLISH_STACKFRAME

1:	/* Even though we can't take a signal until the sigprocmask completes,
	   start the range early.  If %rip is in the range [1, 2), the syscall
	   hasn't been started yet. */
	UNBLOCK_SIGNALS

	/* Prepare 6 register parameters. */
	movq	-40(%rbp), %rax
	movq	OFFSET_amd64_RDI(%rax), %rdi
	movq	OFFSET_amd64_RSI(%rax), %rsi
	movq	OFFSET_amd64_RDX(%rax), %rdx
	movq	OFFSET_amd64_R10(%rax), %r10
	movq	OFFSET_amd64_R8(%rax), %r8
	movq	OFFSET_amd64_R9(%rax), %r9

	/* Switch to the client stack. */
	movq	OFFSET_amd64_RSP(%rax), %rsp	/* %rsp = simulated RSP */
	/* Change %rbp to a client value. It will always get committed by
	   the fixup code for range [2, 3) so it needs to be set to what the
	   client expects. */
	movq	OFFSET_amd64_RBP(%rax), %rbp	/* %rbp = simulated RBP */

	/* Put syscall number in %rax. */
	movq	-48+8(%rbx), %rax

	/* Do the syscall.  Note that the Solaris kernel doesn't directly
	   restart syscalls! */
	syscall

2:	/* In the range [2, 3), the syscall result is in %rax, %rdx, %rsp and
	   %rbp and C, but hasn't been committed to the thread state.  If we
	   get interrupted in this section then we'll just use values saved in
	   the ucontext structure.

	   Important note for this and the following section: Don't add here
	   any code that alters the carry flag or worse, call any function.
	   That would completely break the fixup after an interrupt. */
	movq	-40+8(%rbx), %rcx
	movq	%rax, OFFSET_amd64_RAX(%rcx)	/* save %rax to VEX */
	movq	%rdx, OFFSET_amd64_RDX(%rcx)	/* save %rdx to VEX */
	movq	%rsp, OFFSET_amd64_RSP(%rcx)	/* save %rsp to VEX */
	movq	%rbp, OFFSET_amd64_RBP(%rcx)	/* save %rbp to VEX */
	movq	-16+8(%rbx), %rcx
	setc	0(%rcx)				/* save returned carry flag */

	movq	%rbx, %rsp			/* switch to V stack */

3:	/* Re-block signals. If %rip is in [3, 4), then the syscall is
	   complete and we do not need worry about it.  We have to only
	   correctly save the carry flag.  If we get interrupted in this
	   section then we just have to propagate the carry flag from the
	   ucontext structure to the thread state, %rax, %rdx, %rsp and %rbp
	   values are already saved. */
	movq	%rbx, %rbp
	addq	$8, %rbp
	REBLOCK_SIGNALS

4:	/* Now safe from signals. */
	SIMPLE_RETURN

.section .rodata
.globl ML_(blksys_setup_DRET)
.globl ML_(blksys_complete_DRET)
.globl ML_(blksys_committed_DRET)
.globl ML_(blksys_finished_DRET)
ML_(blksys_setup_DRET):		.quad 1b
ML_(blksys_complete_DRET):	.quad 2b
ML_(blksys_committed_DRET):	.quad 3b
ML_(blksys_finished_DRET):	.quad 4b
.previous

#endif // defined(VGP_amd64_solaris)

/* Let the linker know we don't need an executable stack */
MARK_STACK_NO_EXEC

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
