
/*--------------------------------------------------------------------*/
/*--- The core dispatch loop, for jumping to a code address.       ---*/
/*---                                        dispatch-x86-darwin.S ---*/
/*--------------------------------------------------------------------*/

/*
  This file is part of Valgrind, a dynamic binary instrumentation
  framework.

  Copyright (C) 2000-2013 Julian Seward 
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

#if defined(VGP_x86_darwin)

#include "pub_core_basics_asm.h"
#include "pub_core_dispatch_asm.h"
#include "pub_core_transtab_asm.h"
#include "libvex_guest_offsets.h"	/* for OFFSET_x86_EIP */


/*------------------------------------------------------------*/
/*---                                                      ---*/
/*--- The dispatch loop.  VG_(disp_run_translations) is    ---*/
/*--- used to run all translations,                        ---*/
/*--- including no-redir ones.                             ---*/
/*---                                                      ---*/
/*------------------------------------------------------------*/

/*----------------------------------------------------*/
/*--- Entry and preamble (set everything up)       ---*/
/*----------------------------------------------------*/

/* signature:
void VG_(disp_run_translations)( UWord* two_words,
                                 void*  guest_state, 
                                 Addr   host_addr );
*/
.text
.globl VG_(disp_run_translations)
VG_(disp_run_translations):
        /* 0(%esp) holds our return address. */
	/* 4(%esp) holds two_words */
	/* 8(%esp) holds guest_state */
	/* 12(%esp) holds host_addr */

        /* The preamble */

        /* Save integer registers, since this is a pseudo-function. */
        pushl   %eax
	pushl	%ebx
	pushl	%ecx
	pushl	%edx
	pushl	%esi
	pushl	%edi
	pushl	%ebp
	
	/* 28+4(%esp) holds two_words */
	/* 28+8(%esp) holds guest_state */
	/* 28+12(%esp) holds host_addr */

        /* Get the host CPU in the state expected by generated code. */

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

	/* Set up the guest state pointer */
	movl	28+8(%esp), %ebp

        /* and jump into the code cache.  Chained translations in
           the code cache run, until for whatever reason, they can't
           continue.  When that happens, the translation in question
           will jump (or call) to one of the continuation points
           VG_(cp_...) below. */
        jmpl    *28+12(%esp)
	/*NOTREACHED*/

/*----------------------------------------------------*/
/*--- Postamble and exit.                          ---*/
/*----------------------------------------------------*/

postamble:
        /* At this point, %eax and %edx contain two
           words to be returned to the caller.  %eax
           holds a TRC value, and %edx optionally may
           hold another word (for CHAIN_ME exits, the
           address of the place to patch.) */

	/* We're leaving.  Check that nobody messed with %mxcsr
           or %fpucw.  We can't mess with %eax or %edx here as they
	   holds the tentative return value, but any others are OK. */
#if !defined(ENABLE_INNER)
        /* This check fails for self-hosting, so skip in that case */
	pushl	$0
	fstcw	(%esp)
	cmpl	$0x027F, (%esp)
	popl	%esi /* get rid of the word without trashing %eflags */
	jnz	invariant_violation
#endif
#	cmpl	$0, VG_(machine_x86_have_mxcsr)
	jz	L2
	pushl	$0
	stmxcsr	(%esp)
	andl	$0xFFFFFFC0, (%esp)  /* mask out status flags */
	cmpl	$0x1F80, (%esp)
	popl	%esi
	jnz	invariant_violation
L2:	/* otherwise we're OK */
	jmp	remove_frame
invariant_violation:
	movl	$VG_TRC_INVARIANT_FAILED, %eax
        movl    $0, %edx

remove_frame:
        /* Stash return values */
        movl    28+4(%esp), %edi        /* two_words */
        movl    %eax, 0(%edi)
        movl    %edx, 4(%edi)
        /* Restore int regs and return. */
	popl	%ebp
	popl	%edi
	popl	%esi
	popl	%edx
	popl	%ecx
	popl	%ebx
	popl	%eax
	ret	
        
/*----------------------------------------------------*/
/*--- Continuation points                          ---*/
/*----------------------------------------------------*/

/* ------ Chain me to slow entry point ------ */
.globl VG_(disp_cp_chain_me_to_slowEP)
VG_(disp_cp_chain_me_to_slowEP):
        /* We got called.  The return address indicates
           where the patching needs to happen.  Collect
           the return address and, exit back to C land,
           handing the caller the pair (Chain_me_S, RA) */
        movl    $VG_TRC_CHAIN_ME_TO_SLOW_EP, %eax
        popl    %edx
        /* 5 = movl $VG_(disp_chain_me_to_slowEP), %edx;
           2 = call *%edx */
        subl    $5+2, %edx
        jmp     postamble

/* ------ Chain me to fast entry point ------ */
.globl VG_(disp_cp_chain_me_to_fastEP)
VG_(disp_cp_chain_me_to_fastEP):
        /* We got called.  The return address indicates
           where the patching needs to happen.  Collect
           the return address and, exit back to C land,
           handing the caller the pair (Chain_me_F, RA) */
        movl    $VG_TRC_CHAIN_ME_TO_FAST_EP, %eax
        popl    %edx
        /* 5 = movl $VG_(disp_chain_me_to_fastEP), %edx;
           2 = call *%edx */
        subl    $5+2, %edx
        jmp     postamble

/* ------ Indirect but boring jump ------ */
.globl VG_(disp_cp_xindir)
VG_(disp_cp_xindir):
	/* Where are we going? */
	movl	OFFSET_x86_EIP(%ebp), %eax

        /* stats only */
        addl    $1, VG_(stats__n_xindirs_32)
        
        /* try a fast lookup in the translation cache */
        movl    %eax, %ebx                      /* next guest addr */
        andl    $VG_TT_FAST_MASK, %ebx          /* entry# */
        movl    0+VG_(tt_fast)(,%ebx,8), %esi   /* .guest */
        movl    4+VG_(tt_fast)(,%ebx,8), %edi   /* .host */
        cmpl    %eax, %esi
        jnz     fast_lookup_failed

        /* Found a match.  Jump to .host. */
	jmp 	*%edi
	ud2	/* persuade insn decoders not to speculate past here */

fast_lookup_failed:
        /* stats only */
        addl    $1, VG_(stats__n_xindir_misses_32)

	movl	$VG_TRC_INNER_FASTMISS, %eax
        movl    $0, %edx
	jmp	postamble

/* ------ Assisted jump ------ */
.globl VG_(disp_cp_xassisted)
VG_(disp_cp_xassisted):
        /* %ebp contains the TRC */
        movl    %ebp, %eax
        movl    $0, %edx
        jmp     postamble

/* ------ Event check failed ------ */
.globl VG_(disp_cp_evcheck_fail)
VG_(disp_cp_evcheck_fail):
       	movl	$VG_TRC_INNER_COUNTERZERO, %eax
        movl    $0, %edx
	jmp	postamble


#endif // defined(VGP_x86_darwin)

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
