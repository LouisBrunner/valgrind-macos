
##--------------------------------------------------------------------##
##--- The core dispatch loop, for jumping to a code address.       ---##
##---                                             amd64/dispatch.S ---##
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
#include "amd64_private_asm.h"

/*------------------------------------------------------------*/
/*--- The dispatch loop.                                   ---*/
/*------------------------------------------------------------*/

#define TT_LOOKUP(reg, fail)                            \
        movq %rax, reg;                                 \
        andq $VG_TT_FAST_MASK, reg;                     \
        movq VG_(tt_fast)(,reg,8), reg;                 \
        cmpq %rax, (reg);                               \
        jnz  fail
	
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

	/* 0(%rsp) holds cached copy of guest_state */

	/* Set up the guest state pointer */
	movq	%rdi, %rbp
	
	/* fetch %RIP into %rax */
	movq	VG_(instr_ptr_offset), %rsi
	movq	(%rbp, %rsi, 1), %rax
	
	/* fall into main loop */

	/* Here, %rax is the only live (real) register.  The entire
	   simulated state is saved in the ThreadState. */

dispatch_boring:
	/* save the jump address in the guest state */
	movq	VG_(instr_ptr_offset), %rsi
	movq	%rax, (%rbp, %rsi, 1)

	/* Are we out of timeslice?  If yes, defer to scheduler. */
	subl $1, VG_(dispatch_ctr)
	
	jz	counter_is_zero
	/* try a fast lookup in the translation cache */
	TT_LOOKUP(%rbx, fast_lookup_failed)

	/* Found a match.  Call the tce.payload field.  The magic 12
	   value is offsetof(TCEntry,payload) on a 64-bit platform. */
	addq	$12, %rbx
	call 	*%rbx
	
	/* 
	   %rax holds destination (original) address.
	   %rbp indicates further details of the control transfer
	   requested to the address in %rax.
	
	   If rbp is unchanged (== * 0(%rsp)), just jump next to %rax.

	   Otherwise fall out, back to the scheduler, and let it
	   figure out what to do next.
	*/

	cmpq	0(%rsp), %rbp
	jz	dispatch_boring

	jmp	dispatch_exceptional

	
fast_lookup_failed:
	/* %RIP is up to date here since dispatch_boring dominates */
	addl	$1, VG_(dispatch_ctr)
	movq	$VG_TRC_INNER_FASTMISS, %rax
	jmp	run_innerloop_exit

counter_is_zero:
	/* %RIP is up to date here since dispatch_boring dominates */
	addl	$1, VG_(dispatch_ctr)
	movq	$VG_TRC_INNER_COUNTERZERO, %rax
	jmp	run_innerloop_exit
	
run_innerloop_exit:
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
	cmpq	$VG_TRC_INNER_COUNTERZERO, %rbp
	jz	counter_is_zero

	/* save %rax in %RIP and defer to sched */
	movq	VG_(instr_ptr_offset), %rsi
	movq	0(%rsp), %rdi
	movq	%rax, (%rdi, %rsi, 1)
	movq	%rbp, %rax
	jmp	run_innerloop_exit

/* Let the linker know we don't need an executable stack */
.section .note.GNU-stack,"",@progbits

##--------------------------------------------------------------------##
##--- end                                                          ---##
##--------------------------------------------------------------------##
