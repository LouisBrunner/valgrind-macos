
##--------------------------------------------------------------------##
##--- The core dispatch loop, for jumping to a code address.       ---##
##---                                             amd64/dispatch.S ---##
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
#include "pub_core_dispatch_asm.h"
#include "pub_core_transtab_asm.h"
#include "libvex_guest_offsets.h"	/* for OFFSET_amd64_RIP */


/*------------------------------------------------------------*/
/*--- The dispatch loop.                                   ---*/
/*------------------------------------------------------------*/

/* signature: UWord VG_(run_innerloop) ( void* guest_state ) */

.globl VG_(run_innerloop)
VG_(run_innerloop):
	/* %rdi holds guest_state */
	
	/* ----- entry point to VG_(run_innerloop) ----- */
	pushq	%rbx
	pushq	%rcx
	pushq	%rdx
	pushq	%rsi
	pushq	%rbp
	pushq	%r8
	pushq	%r9
	pushq	%r10
	pushq	%r11
	pushq	%r12
	pushq	%r13
	pushq	%r14
	pushq	%r15
	pushq	%rdi

	movq	VG_(dispatch_ctr)@GOTPCREL(%rip), %rsi
	pushq	(%rsi)

	/* 8(%rsp) holds cached copy of guest_state */
	/* 0(%rsp) holds cached copy of VG_(dispatch_ctr) */

	/* Set up the guest state pointer */
	movq	%rdi, %rbp
	
	/* fetch %RIP into %rax */
	movq	OFFSET_amd64_RIP(%rbp), %rax

	/* set host FPU control word to the default mode expected 
           by VEX-generated code.  See comments in libvex.h for
           more info. */
	finit
	pushq	$0x027F
	fldcw	(%rsp)
	addq	$8, %rsp
	
	/* set host SSE control word to the default mode expected 
	   by VEX-generated code. */
	pushq	$0x1F80
	ldmxcsr	(%rsp)
	addq	$8, %rsp

	/* set dir flag to known value */
	cld

	/* fall into main loop */

	/* Here, %rax is the only live (real) register.  The entire
	   simulated state is saved in the ThreadState. */

dispatch_boring:
	/* save the jump address in the guest state */
	movq	%rax, OFFSET_amd64_RIP(%rbp)

	/* Are we out of timeslice?  If yes, defer to scheduler. */
	subl	$1, 0(%rsp)
	jz	counter_is_zero

	/* try a fast lookup in the translation cache */
	movq %rax, %rbx
	andq $VG_TT_FAST_MASK, %rbx
	movq VG_(tt_fast)@GOTPCREL(%rip), %rcx
	movq (%rcx,%rbx,8), %rcx
	cmpq %rax, (%rcx)
	jnz  fast_lookup_failed
	/* increment bb profile counter */
	movq VG_(tt_fastN)@GOTPCREL(%rip), %rdx
	movq (%rdx,%rbx,8), %rdx
	incl (%rdx)

	/* Found a match.  Call tce[1], which is 8 bytes along, since
           each tce element is a 64-bit int. */
	addq 	$8, %rcx
	call 	*%rcx

	/* 
	   %rax holds destination (original) address.
	   %rbp indicates further details of the control transfer
	   requested to the address in %rax.
	
	   If rbp is unchanged (== * 8(%rsp)), just jump next to %rax.

	   Otherwise fall out, back to the scheduler, and let it
	   figure out what to do next.
	*/

	cmpq	8(%rsp), %rbp
	jz	dispatch_boring

	jmp	dispatch_exceptional



/* All exits from the dispatcher go through here.  %rax holds
   the return value. 
*/
run_innerloop_exit: 
	/* We're leaving.  Check that nobody messed with
           %mxcsr or %fpucw.  We can't mess with %rax here as it
	   holds the tentative return value, but any other is OK. */
	pushq	$0
	fstcw	(%rsp)
	cmpl	$0x027F, (%rsp)
	popq	%r11 /* get rid of the word without trashing %eflags */
	jnz	invariant_violation
	pushq	$0
	stmxcsr	(%rsp)
	andl	$0xFFFFFFC0, (%rsp)  /* mask out status flags */
	cmpl	$0x1F80, (%rsp)
	popq	%r11
	jnz	invariant_violation
	/* otherwise we're OK */
	jmp	run_innerloop_exit_REALLY

invariant_violation:
	movq	$VG_TRC_INVARIANT_FAILED, %rax
	jmp	run_innerloop_exit_REALLY

run_innerloop_exit_REALLY:
	movq	VG_(dispatch_ctr)@GOTPCREL(%rip), %rsi
	popq	(%rsi)
	popq	%rdi
	popq	%r15
	popq	%r14
	popq	%r13
	popq	%r12
	popq	%r11
	popq	%r10
	popq	%r9
	popq	%r8
	popq	%rbp
	popq	%rsi
	popq	%rdx
	popq	%rcx
	popq	%rbx
	ret	

	
	
/* Other ways of getting out of the inner loop.  Placed out-of-line to
   make it look cleaner. 
*/
dispatch_exceptional:
	/* this is jumped to only, not fallen-through from above */

	/* save %rax in %RIP and defer to sched */
	movq	8(%rsp), %rdi
	movq	%rax, OFFSET_amd64_RIP(%rdi)
	movq	%rbp, %rax
	jmp	run_innerloop_exit

fast_lookup_failed:
	/* %RIP is up to date here since dispatch_boring dominates */
	addl	$1, 0(%rsp)
	movq	$VG_TRC_INNER_FASTMISS, %rax
	jmp	run_innerloop_exit

counter_is_zero:
	/* %RIP is up to date here since dispatch_boring dominates */
	addl	$1, 0(%rsp)
	movq	$VG_TRC_INNER_COUNTERZERO, %rax
	jmp	run_innerloop_exit


/* Let the linker know we don't need an executable stack */
.section .note.GNU-stack,"",@progbits

##--------------------------------------------------------------------##
##--- end                                                          ---##
##--------------------------------------------------------------------##
