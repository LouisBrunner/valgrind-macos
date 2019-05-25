
/*--------------------------------------------------------------------*/
/*--- Support for doing system calls.        syscall-x86-solaris.S ---*/
/*--------------------------------------------------------------------*/

/*
  This file is part of Valgrind, a dynamic binary instrumentation
  framework.

  Copyright (C) 2011-2017 Petr Pavlu
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

#if defined(VGP_x86_solaris)

#include "pub_core_vkiscnums_asm.h"
#include "libvex_guest_offsets.h"

/* From vki-solaris.h, checked at startup by m_vki.c. */
#define VKI_SIG_SETMASK 3

/* Prototype:
   Int ML_(do_syscall_for_client_WRK)(
      Int syscallno,			// %ebp+8
      void *guest_state,		// %ebp+12
      const vki_sigset_t *sysmask,	// %ebp+16
      const vki_sigset_t *postmask,	// %ebp+20
      UChar *cflag)			// %ebp+24
*/

.macro ESTABLISH_STACKFRAME
	/* Establish stack frame. */
	pushl	%ebp
	movl	%esp, %ebp
	pushl	%ebx				/* save %ebx */

	/* We'll use %ebx instead of %ebp to address the stack frame after the
	   door syscall is finished because %ebp is cleared by the syscall. */
	movl	%esp, %ebx			/* %ebx = %ebp - 4 */
.endm

.macro UNBLOCK_SIGNALS
	/* Set the signal mask which should be current during the syscall. */
	/* Set up for sigprocmask(SIG_SETMASK, sysmask, postmask). */
	pushl	20(%ebp)
	pushl	16(%ebp)
	pushl	$VKI_SIG_SETMASK
	pushl	$0xcafebabe			/* totally fake return address */
	movl	$__NR_sigprocmask, %eax
	int	$0x91
	jc	sigprocmask_failed		/* sigprocmask failed */
	addl	$16, %esp
.endm

.macro REBLOCK_SIGNALS
	/* Set up for sigprocmask(SIG_SETMASK, postmask, NULL). */
	pushl	$0
	pushl	20(%ebp)
	pushl	$VKI_SIG_SETMASK
	pushl	$0xcafef00d			/* totally fake return address */
	movl	$__NR_sigprocmask, %eax
	int	$0x91
	/* The syscall above changes the carry flag.  This means that if the
	   syscall fails and we receive an interrupt after it then we've got
	   an invalid carry flag value in the fixup code.  We don't care about
	   it because this syscall should never fail and if it does then we're
	   going to stop Valgrind anyway. */
	jc	sigprocmask_failed		/* sigprocmask failed */
	addl	$16, %esp
.endm

.macro SIMPLE_RETURN
	xorl	%eax, %eax			/* SUCCESS */
	movl	-4(%ebp), %ebx			/* restore %ebx */
	movl	%ebp, %esp
	popl	%ebp
	ret
.endm

sigprocmask_failed:
	/* Failure: return 0x8000 | error code. */
	/* Note that we enter here with %esp being 16 too low (4 extra words
	   on the stack).  But because we're nuking the stack frame now, that
	   doesn't matter. */
	andl	$0x7FFF, %eax
	orl	$0x8000, %eax
	movl	-4(%ebp), %ebx			/* restore %ebx */
	movl	%ebp, %esp
	popl	%ebp
	ret

.globl ML_(do_syscall_for_client_WRK)
ML_(do_syscall_for_client_WRK):
	ESTABLISH_STACKFRAME

1:	/* Even though we can't take a signal until the sigprocmask completes,
	   start the range early.  If %eip is in the range [1, 2), the syscall
	   hasn't been started yet. */
	UNBLOCK_SIGNALS

	/* Copy syscall parameters to the stack - assume no more than 8 plus
	   the return address. */
	/* do_syscall8 */
	movl	12(%ebp), %edx
	movl	OFFSET_x86_ESP(%edx), %edx	/* %edx = simulated ESP */
	movl	28+4(%edx), %eax
	pushl	%eax
	movl	24+4(%edx), %eax
	pushl	%eax
	movl	20+4(%edx), %eax
	pushl	%eax
	movl	16+4(%edx), %eax
	pushl	%eax
	movl	12+4(%edx), %eax
	pushl	%eax
	movl	8+4(%edx), %eax
	pushl	%eax
	movl	4+4(%edx), %eax
	pushl	%eax
	movl	0+4(%edx), %eax
	pushl	%eax
	/* Return address. */
	movl	0(%edx), %eax
	pushl	%eax

	/* Put syscall number in %eax. */
	movl	8(%ebp), %eax

	/* Do the syscall.  Note that the Solaris kernel doesn't directly
	   restart syscalls! */
	int	$0x91

2:	/* In the range [2, 3), the syscall result is in %eax and %edx and C,
	   but hasn't been committed to the thread state.  If we get
	   interrupted in this section then we'll just use values saved in the
	   ucontext structure.

	   Important note for this and the following section: Don't add here
	   any code that alters the carry flag or worse, call any function.
	   That would completely break the fixup after an interrupt. */
	movl	12(%ebp), %ecx
	movl	%eax, OFFSET_x86_EAX(%ecx)	/* save %eax to VEX */
	movl	%edx, OFFSET_x86_EDX(%ecx)	/* save %edx to VEX */
	movl	24(%ebp), %ecx
	setc	0(%ecx)				/* save returned carry flag */

3:	/* Re-block signals. If %eip is in [3, 4), then the syscall is
	   complete and we do not need to worry about it.  We have to only
	   correctly save the carry flag.  If we get interrupted in this
	   section then we just have to propagate the carry flag from the
	   ucontext structure to the thread state, %eax and %edx values are
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
ML_(blksys_setup):	.long 1b
ML_(blksys_complete):	.long 2b
ML_(blksys_committed):	.long 3b
ML_(blksys_finished):	.long 4b
.previous

/* Prototype:
   Int ML_(do_syscall_for_client_dret_WRK)(
      Int syscallno,			// %ebp+8 = %ebx+8+4
      void *guest_state,		// %ebp+12 = %ebx+12+4
      const vki_sigset_t *sysmask,	// %ebp+16 = %ebx+16+4
      const vki_sigset_t *postmask,	// %ebp+20 = %ebx+20+4
      UChar *cflag)			// %ebp+24 = %ebx+24+4
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
	   start the range early.  If %eip is in the range [1, 2), the syscall
	   hasn't been started yet. */
	UNBLOCK_SIGNALS

	/* Switch to the client stack. */
	movl	12(%ebp), %edx
	movl	OFFSET_x86_ESP(%edx), %esp	/* %esp = simulated ESP */
	/* Change %ebp to a client value. It will always get committed by
	   the fixup code for range [2, 3) so it needs to be set to what the
	   client expects. */
	movl	OFFSET_x86_EBP(%edx), %ebp	/* %ebp = simulated EBP */

	/* Put syscall number in %eax. */
	movl	8+4(%ebx), %eax

	/* Do the syscall.  Note that the Solaris kernel doesn't directly
	   restart syscalls! */
	int	$0x91

2:	/* In the range [2, 3), the syscall result is in %eax, %edx, %esp and
	   %ebp and C, but hasn't been committed to the thread state.  If we
	   get interrupted in this section then we'll just use values saved in
	   the ucontext structure.

	   Important note for this and the following section: Don't add here
	   any code that alters the carry flag or worse, call any function.
	   That would completely break the fixup after an interrupt. */
	movl	12+4(%ebx), %ecx
	movl	%eax, OFFSET_x86_EAX(%ecx)	/* save %eax to VEX */
	movl	%edx, OFFSET_x86_EDX(%ecx)	/* save %edx to VEX */
	movl	%esp, OFFSET_x86_ESP(%ecx)	/* save %esp to VEX */
	movl	%ebp, OFFSET_x86_EBP(%ecx)	/* save %ebp to VEX */
	movl	24+4(%ebx), %ecx
	setc	0(%ecx)				/* save returned carry flag */

	movl	%ebx, %esp			/* switch to V stack */

3:	/* Re-block signals. If %eip is in [3, 4), then the syscall is
	   complete and we do not need worry about it.  We have to only
	   correctly save the carry flag.  If we get interrupted in this
	   section then we just have to propagate the carry flag from the
	   ucontext structure to the thread state, %eax, %edx, %esp and %ebp
	   values are already saved. */
	movl	%ebx, %ebp
	addl	$4, %ebp
	REBLOCK_SIGNALS

4:	/* Now safe from signals. */
	SIMPLE_RETURN

.section .rodata
.globl ML_(blksys_setup_DRET)
.globl ML_(blksys_complete_DRET)
.globl ML_(blksys_committed_DRET)
.globl ML_(blksys_finished_DRET)
ML_(blksys_setup_DRET):		.long 1b
ML_(blksys_complete_DRET):	.long 2b
ML_(blksys_committed_DRET):	.long 3b
ML_(blksys_finished_DRET):	.long 4b
.previous

#endif // defined(VGP_x86_solaris)

/* Let the linker know we don't need an executable stack */
MARK_STACK_NO_EXEC

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
