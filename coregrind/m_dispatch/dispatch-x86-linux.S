
/*--------------------------------------------------------------------*/
/*--- The core dispatch loop, for jumping to a code address.       ---*/
/*---                                         dispatch-x86-linux.S ---*/
/*--------------------------------------------------------------------*/

/*
  This file is part of Valgrind, a dynamic binary instrumentation
  framework.

  Copyright (C) 2000-2010 Julian Seward 
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

#if defined(VGP_x86_linux)

#include "pub_core_basics_asm.h"
#include "pub_core_dispatch_asm.h"
#include "pub_core_transtab_asm.h"
#include "libvex_guest_offsets.h"	/* for OFFSET_x86_EIP */


/*------------------------------------------------------------*/
/*---                                                      ---*/
/*--- The dispatch loop.  VG_(run_innerloop) is used to    ---*/
/*--- run all translations except no-redir ones.           ---*/
/*---                                                      ---*/
/*------------------------------------------------------------*/

/*----------------------------------------------------*/
/*--- Preamble (set everything up)                 ---*/
/*----------------------------------------------------*/

/* signature:
UWord VG_(run_innerloop) ( void* guest_state, UWord do_profiling );
*/
.text
.globl VG_(run_innerloop)
.type  VG_(run_innerloop), @function
VG_(run_innerloop):
	/* 4(%esp) holds guest_state */
	/* 8(%esp) holds do_profiling */
	
	/* ----- entry point to VG_(run_innerloop) ----- */
	pushl	%ebx
	pushl	%ecx
	pushl	%edx
	pushl	%esi
	pushl	%edi
	pushl	%ebp
	
	/* 28(%esp) holds guest_state */
	/* 32(%esp) holds do_profiling */

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
	cmpl	$0, VG_(machine_x86_have_mxcsr)
	jz	L1
	pushl	$0x1F80
	ldmxcsr	(%esp)
	addl	$4, %esp
L1:
	/* set dir flag to known value */
	cld
	
	/* fall into main loop (the right one) */
	cmpl	$0, 32(%esp) /* do_profiling */
	je	VG_(run_innerloop__dispatch_unprofiled)
	jmp	VG_(run_innerloop__dispatch_profiled)
	/*NOTREACHED*/

/*----------------------------------------------------*/
/*--- NO-PROFILING (standard) dispatcher           ---*/
/*----------------------------------------------------*/

.align	16
.global	VG_(run_innerloop__dispatch_unprofiled)
VG_(run_innerloop__dispatch_unprofiled):
	/* AT ENTRY: %eax is next guest addr, %ebp is possibly
           modified guest state ptr */

	/* Has the guest state pointer been messed with?  If yes, exit. */
	cmpl	28(%esp), %ebp
	jnz	gsp_changed

	/* save the jump address in the guest state */
	movl	%eax, OFFSET_x86_EIP(%ebp)

	/* Are we out of timeslice?  If yes, defer to scheduler. */
	subl	$1, VG_(dispatch_ctr)
	jz	counter_is_zero

	/* try a fast lookup in the translation cache */
	movl	%eax, %ebx			/* next guest addr */
	andl	$ VG_TT_FAST_MASK, %ebx		/* entry# */
	movl	0+VG_(tt_fast)(,%ebx,8), %esi	/* .guest */
	movl	4+VG_(tt_fast)(,%ebx,8), %edi	/* .host */
	cmpl	%eax, %esi
	jnz	fast_lookup_failed

	/* Found a match.  Jump to .host. */
	jmp 	*%edi
	ud2	/* persuade insn decoders not to speculate past here */
	/* generated code should run, then jump back to
	   VG_(run_innerloop__dispatch_unprofiled). */
	/*NOTREACHED*/

/*----------------------------------------------------*/
/*--- PROFILING dispatcher (can be much slower)    ---*/
/*----------------------------------------------------*/

.align	16
.global	VG_(run_innerloop__dispatch_profiled)
VG_(run_innerloop__dispatch_profiled):
	/* AT ENTRY: %eax is next guest addr, %ebp is possibly
           modified guest state ptr */

	/* Has the guest state pointer been messed with?  If yes, exit. */
	cmpl	28(%esp), %ebp
	jnz	gsp_changed

	/* save the jump address in the guest state */
	movl	%eax, OFFSET_x86_EIP(%ebp)

	/* Are we out of timeslice?  If yes, defer to scheduler. */
	subl	$1, VG_(dispatch_ctr)
	jz	counter_is_zero

	/* try a fast lookup in the translation cache */
	movl	%eax, %ebx			/* next guest addr */
	andl	$ VG_TT_FAST_MASK, %ebx		/* entry# */
	movl	0+VG_(tt_fast)(,%ebx,8), %esi	/* .guest */
	movl	4+VG_(tt_fast)(,%ebx,8), %edi	/* .host */
	cmpl	%eax, %esi
	jnz	fast_lookup_failed

	/* increment bb profile counter */
	/* note: innocuous as this sounds, it causes a huge amount more
           stress on D1 and significantly slows everything down. */
	movl	VG_(tt_fastN)(,%ebx,4), %edx
	/* Use "addl $1", not "incl", to avoid partial-flags stall on P4 */
	addl	$1, (%edx)

	/* Found a match.  Jump to .host. */
	jmp 	*%edi
	ud2	/* persuade insn decoders not to speculate past here */
	/* generated code should run, then jump back to
	   VG_(run_innerloop__dispatch_profiled). */
	/*NOTREACHED*/

/*----------------------------------------------------*/
/*--- exit points                                  ---*/
/*----------------------------------------------------*/

gsp_changed:
	/* Someone messed with the gsp.  Have to
           defer to scheduler to resolve this.  dispatch ctr
	   is not yet decremented, so no need to increment. */
	/* %EIP is NOT up to date here.  First, need to write
	   %eax back to %EIP, but without trashing %ebp since
	   that holds the value we want to return to the scheduler.
	   Hence use %esi transiently for the guest state pointer. */
	movl	28(%esp), %esi
	movl	%eax, OFFSET_x86_EIP(%esi)
	movl	%ebp, %eax
	jmp	run_innerloop_exit
	/*NOTREACHED*/

counter_is_zero:
	/* %EIP is up to date here */
	/* back out decrement of the dispatch counter */
	addl	$1, VG_(dispatch_ctr)
	movl	$ VG_TRC_INNER_COUNTERZERO, %eax
	jmp	run_innerloop_exit
	/*NOTREACHED*/

fast_lookup_failed:
	/* %EIP is up to date here */
	/* back out decrement of the dispatch counter */
	addl	$1, VG_(dispatch_ctr)
	movl	$ VG_TRC_INNER_FASTMISS, %eax
	jmp	run_innerloop_exit
	/*NOTREACHED*/



/* All exits from the dispatcher go through here.  %eax holds
   the return value. 
*/
run_innerloop_exit: 
	/* We're leaving.  Check that nobody messed with
           %mxcsr or %fpucw.  We can't mess with %eax here as it
	   holds the tentative return value, but any other is OK. */
#if !defined(ENABLE_INNER)
        /* This check fails for self-hosting, so skip in that case */
	pushl	$0
	fstcw	(%esp)
	cmpl	$0x027F, (%esp)
	popl	%esi /* get rid of the word without trashing %eflags */
	jnz	invariant_violation
#endif
	cmpl	$0, VG_(machine_x86_have_mxcsr)
	jz	L2
	pushl	$0
	stmxcsr	(%esp)
	andl	$0xFFFFFFC0, (%esp)  /* mask out status flags */
	cmpl	$0x1F80, (%esp)
	popl	%esi
	jnz	invariant_violation
L2:	/* otherwise we're OK */
	jmp	run_innerloop_exit_REALLY

invariant_violation:
	movl	$ VG_TRC_INVARIANT_FAILED, %eax
	jmp	run_innerloop_exit_REALLY

run_innerloop_exit_REALLY:
	popl	%ebp
	popl	%edi
	popl	%esi
	popl	%edx
	popl	%ecx
	popl	%ebx
	ret	
.size VG_(run_innerloop), .-VG_(run_innerloop)


/*------------------------------------------------------------*/
/*---                                                      ---*/
/*--- A special dispatcher, for running no-redir           ---*/
/*--- translations.  Just runs the given translation once. ---*/
/*---                                                      ---*/
/*------------------------------------------------------------*/

/* signature:
void VG_(run_a_noredir_translation) ( UWord* argblock );
*/

/* Run a no-redir translation.  argblock points to 4 UWords, 2 to carry args
   and 2 to carry results:
      0: input:  ptr to translation
      1: input:  ptr to guest state
      2: output: next guest PC
      3: output: guest state pointer afterwards (== thread return code)
*/
.align 16
.global VG_(run_a_noredir_translation)
.type VG_(run_a_noredir_translation), @function
VG_(run_a_noredir_translation):
	/* Save callee-saves regs */
	pushl %esi
	pushl %edi
	pushl %ebp
	pushl %ebx

	movl 20(%esp), %edi	/* %edi = argblock */
	movl 4(%edi), %ebp	/* argblock[1] */
	jmp *0(%edi)		/* argblock[0] */
	/*NOTREACHED*/
	ud2
	/* If the translation has been correctly constructed, we
	should resume at the the following label. */
.global VG_(run_a_noredir_translation__return_point)
VG_(run_a_noredir_translation__return_point):
	movl 20(%esp), %edi
	movl %eax, 8(%edi)	/* argblock[2] */
	movl %ebp, 12(%edi)	/* argblock[3] */

	popl %ebx
	popl %ebp
	popl %edi
	popl %esi
	ret
.size VG_(run_a_noredir_translation), .-VG_(run_a_noredir_translation)

			
/* Let the linker know we don't need an executable stack */
.section .note.GNU-stack,"",@progbits

#endif // defined(VGP_x86_linux)

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
