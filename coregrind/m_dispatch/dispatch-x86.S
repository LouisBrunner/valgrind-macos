
##--------------------------------------------------------------------##
##--- The core dispatch loop, for jumping to a code address.       ---##
##---                                               x86/dispatch.S ---##
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

#include "pub_basics_asm.h"
#include "pub_core_dispatch_asm.h"
#include "pub_core_transtab_asm.h"
#include "libvex_guest_offsets.h"	/* for OFFSET_x86_EIP */


/*------------------------------------------------------------*/
/*--- The dispatch loop.                                   ---*/
/*------------------------------------------------------------*/

/* signature: UWord VG_(run_innerloop) ( void* guest_state ) */

.globl VG_(run_innerloop)
VG_(run_innerloop):
	/* 4(%esp) holds guest_state */
	
	/* ----- entry point to VG_(run_innerloop) ----- */
	pushl	%ebx
	pushl	%ecx
	pushl	%edx
	pushl	%esi
	pushl	%edi
	pushl	%ebp
	
	/* 28(%esp) holds guest_state */

	/* Set up the guest state pointer */
	movl	28(%esp), %ebp
	
	/* fetch %EIP into %eax */
	movl	OFFSET_x86_EIP(%ebp), %eax

	/* set host FPU control word to the default mode expected 
           by VEX-generated code.  See comments in libvex.h for
           more info. */
	finit
	pushl	$0x027F
	fldcw	(%esp)
	addl	$4, %esp
	
	/* set host SSE control word to the default mode expected 
	   by VEX-generated code. */
	pushl	$0x1F80
	ldmxcsr	(%esp)
	addl	$4, %esp

	/* set dir flag to known value */
	cld
	
	/* fall into main loop */

	/* Here, %eax is the only live (real) register.  The entire
	   simulated state is saved in the ThreadState. */

dispatch_boring:
	/* save the jump address in the guest state */
	movl	%eax, OFFSET_x86_EIP(%ebp)

	/* Are we out of timeslice?  If yes, defer to scheduler. */
	subl $1, VG_(dispatch_ctr)
	jz	counter_is_zero

	/* try a fast lookup in the translation cache */
	movl %eax, %ebx
	andl $VG_TT_FAST_MASK, %ebx
	movl VG_(tt_fast)(,%ebx,4), %ecx
	cmpl %eax, (%ecx)
	jnz  fast_lookup_failed
	/* increment bb profile counter */
	movl VG_(tt_fastN)(,%ebx,4), %edx
	incl (%edx)

	/* Found a match.  Call tce[1], which is 8 bytes along, since
           each tce element is a 64-bit int. */
	addl 	$8, %ecx
	call 	*%ecx
	
	/* 
	   %eax holds destination (original) address.
	   %ebp indicates further details of the control transfer
	   requested to the address in %eax.
	
	   If ebp is unchanged (== * 28(%esp)), just jump next to %eax.

	   Otherwise fall out, back to the scheduler, and let it
	   figure out what to do next.
	*/

	cmpl	28(%esp), %ebp
	jz	dispatch_boring

	jmp	dispatch_exceptional

	

/* All exits from the dispatcher go through here.  %eax holds
   the return value. 
*/
run_innerloop_exit: 
	/* We're leaving.  Check that nobody messed with
           %mxcsr or %fpucw.  We can't mess with %eax here as it
	   holds the tentative return value, but any other is OK. */
	pushl	$0
	fstcw	(%esp)
	cmpl	$0x027F, (%esp)
	popl	%esi /* get rid of the word without trashing %eflags */
	jnz	invariant_violation
	pushl	$0
	stmxcsr	(%esp)
	andl	$0xFFFFFFC0, (%esp)  /* mask out status flags */
	cmpl	$0x1F80, (%esp)
	popl	%esi
	jnz	invariant_violation
	/* otherwise we're OK */
	jmp	run_innerloop_exit_REALLY

invariant_violation:
	movl	$VG_TRC_INVARIANT_FAILED, %eax
	jmp	run_innerloop_exit_REALLY

run_innerloop_exit_REALLY:
	popl	%ebp
	popl	%edi
	popl	%esi
	popl	%edx
	popl	%ecx
	popl	%ebx
	ret	



/* Other ways of getting out of the inner loop.  Placed out-of-line to
   make it look cleaner. 
*/
dispatch_exceptional:
	/* this is jumped to only, not fallen-through from above */

	/* save %eax in %EIP and defer to sched */
	movl	28(%esp), %edi
	movl	%eax, OFFSET_x86_EIP(%edi)
	movl	%ebp, %eax
	jmp	run_innerloop_exit

fast_lookup_failed:
	/* %EIP is up to date here since dispatch_boring dominates */
	addl	$1, VG_(dispatch_ctr)
	movl	$VG_TRC_INNER_FASTMISS, %eax
	jmp	run_innerloop_exit

counter_is_zero:
	/* %EIP is up to date here since dispatch_boring dominates */
	addl	$1, VG_(dispatch_ctr)
	movl	$VG_TRC_INNER_COUNTERZERO, %eax
	jmp	run_innerloop_exit

	
/* Let the linker know we don't need an executable stack */
.section .note.GNU-stack,"",@progbits

##--------------------------------------------------------------------##
##--- end                                                          ---##
##--------------------------------------------------------------------##
