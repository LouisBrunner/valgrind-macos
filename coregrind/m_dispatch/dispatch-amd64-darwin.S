
/*--------------------------------------------------------------------*/
/*--- The core dispatch loop, for jumping to a code address.       ---*/
/*---                                      dispatch-amd64-darwin.S ---*/
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

#if defined(VGP_amd64_darwin)

#include "pub_core_dispatch_asm.h"
#include "pub_core_transtab_asm.h"
#include "libvex_guest_offsets.h"	/* for OFFSET_amd64_RIP */


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
        /* %rdi holds two_words    */
	/* %rsi holds guest_state  */
	/* %rdx holds host_addr    */

        /* The preamble */

        /* Save integer registers, since this is a pseudo-function. */
        pushq   %rax
	pushq	%rbx
	pushq	%rcx
        pushq   %rdx
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
        /* %rdi must be saved last */
	pushq	%rdi

        /* Get the host CPU in the state expected by generated code. */

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

	/* Set up the guest state pointer */
	movq	%rsi, %rbp

        /* and jump into the code cache.  Chained translations in
           the code cache run, until for whatever reason, they can't
           continue.  When that happens, the translation in question
           will jump (or call) to one of the continuation points
           VG_(cp_...) below. */
        jmpq    *%rdx
       	/*NOTREACHED*/	

/*----------------------------------------------------*/
/*--- Postamble and exit.                          ---*/
/*----------------------------------------------------*/

postamble:
        /* At this point, %rax and %rdx contain two
           words to be returned to the caller.  %rax
           holds a TRC value, and %rdx optionally may
           hold another word (for CHAIN_ME exits, the
           address of the place to patch.) */
        
	/* We're leaving.  Check that nobody messed with %mxcsr
           or %fpucw.  We can't mess with %rax or %rdx here as they
           hold the tentative return values, but any others are OK. */
#if !defined(ENABLE_INNER)
        /* This check fails for self-hosting, so skip in that case */
	pushq	$0
	fstcw	(%rsp)
	cmpl	$0x027F, (%rsp)
	popq	%r15 /* get rid of the word without trashing %rflags */
	jnz	invariant_violation
#endif
	pushq	$0
	stmxcsr	(%rsp)
	andl	$0xFFFFFFC0, (%rsp)  /* mask out status flags */
	cmpl	$0x1F80, (%rsp)
	popq	%r15
	jnz	invariant_violation
	/* otherwise we're OK */
	jmp	remove_frame
invariant_violation:
	movq	$VG_TRC_INVARIANT_FAILED, %rax
        movq    $0, %rdx

remove_frame:
        /* Pop %rdi, stash return values */
	popq	%rdi
        movq    %rax, 0(%rdi)
        movq    %rdx, 8(%rdi)
        /* Now pop everything else */
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
	popq	%rax
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
        movq    $VG_TRC_CHAIN_ME_TO_SLOW_EP, %rax
        popq    %rdx
        /* 10 = movabsq $VG_(disp_chain_me_to_slowEP), %r11;
           3  = call *%r11 */
        subq    $10+3, %rdx
        jmp     postamble

/* ------ Chain me to fast entry point ------ */
.globl VG_(disp_cp_chain_me_to_fastEP)
VG_(disp_cp_chain_me_to_fastEP):
        /* We got called.  The return address indicates
           where the patching needs to happen.  Collect
           the return address and, exit back to C land,
           handing the caller the pair (Chain_me_F, RA) */
        movq    $VG_TRC_CHAIN_ME_TO_FAST_EP, %rax
        popq    %rdx
        /* 10 = movabsq $VG_(disp_chain_me_to_fastEP), %r11;
           3  = call *%r11 */
        subq    $10+3, %rdx
        jmp     postamble

/* ------ Indirect but boring jump ------ */
.global VG_(disp_cp_xindir)
VG_(disp_cp_xindir):
        /* Where are we going? */
        movq    OFFSET_amd64_RIP(%rbp), %rax    // "guest"

        /* stats only */
        movabsq $VG_(stats__n_xIndirs_32), %r8
        addl    $1, (%r8)

        // LIVE: %rbp (guest state ptr), %rax (guest address to go to).
        // We use 5 temporaries:
        //   %r9 (to point at the relevant FastCacheSet),
        //   %r10, %r11 and %r12 (scratch).
        //   %r8 (scratch address)

        /* Try a fast lookup in the translation cache.  This is pretty much
           a handcoded version of VG_(lookupInFastCache). */

        // Compute %r9 = VG_TT_FAST_HASH(guest)
        movq    %rax, %r9               // guest
        shrq    $VG_TT_FAST_BITS, %r9   // (guest >> VG_TT_FAST_BITS)
        xorq    %rax, %r9               // (guest >> VG_TT_FAST_BITS) ^ guest
        andq    $VG_TT_FAST_MASK, %r9   // setNo

        // Compute %r9 = &VG_(tt_fast)[%r9]
        shlq    $VG_FAST_CACHE_SET_BITS, %r9  // setNo * sizeof(FastCacheSet)
        movabsq $VG_(tt_fast), %r10           // &VG_(tt_fast)[0]
        leaq    (%r10, %r9), %r9              // &VG_(tt_fast)[setNo]

        // LIVE: %rbp (guest state ptr), %rax (guest addr), %r9 (cache set)
        // try way 0
        cmpq    %rax, FCS_g0(%r9)   // cmp against .guest0
        jnz     1f
        // hit at way 0
        jmp    *FCS_h0(%r9)         // goto .host0
        ud2

1:      // try way 1
        cmpq    %rax, FCS_g1(%r9)   // cmp against .guest1
        jnz     2f
        // hit at way 1; swap upwards
        /* stats only */
        movabsq $VG_(stats__n_xIndir_hits1_32), %r8
        addl    $1, (%r8)
        movq    FCS_g0(%r9), %r10   // r10 = old .guest0
        movq    FCS_h0(%r9), %r11   // r11 = old .host0
        movq    FCS_h1(%r9), %r12   // r12 = old .host1
        movq    %rax, FCS_g0(%r9)   // new .guest0 = guest
        movq    %r12, FCS_h0(%r9)   // new .host0 = old .host1
        movq    %r10, FCS_g1(%r9)   // new .guest1 = old .guest0
        movq    %r11, FCS_h1(%r9)   // new .host1 = old .host0
        jmp     *%r12               // goto old .host1 a.k.a. new .host0
        ud2

2:      // try way 2
        cmpq    %rax, FCS_g2(%r9)   // cmp against .guest2
        jnz     3f
        // hit at way 2; swap upwards
        /* stats only */
        movabsq $VG_(stats__n_xIndir_hits2_32), %r8
        addl    $1, (%r8)
        movq    FCS_g1(%r9), %r10
        movq    FCS_h1(%r9), %r11
        movq    FCS_h2(%r9), %r12
        movq    %rax, FCS_g1(%r9)
        movq    %r12, FCS_h1(%r9)
        movq    %r10, FCS_g2(%r9)
        movq    %r11, FCS_h2(%r9)
        jmp     *%r12
        ud2

3:      // try way 3
        cmpq    %rax, FCS_g3(%r9)   // cmp against .guest3
        jnz     4f
        // hit at way 3; swap upwards
        /* stats only */
        movabsq $VG_(stats__n_xIndir_hits3_32), %r8
        addl    $1, (%r8)
        movq    FCS_g2(%r9), %r10
        movq    FCS_h2(%r9), %r11
        movq    FCS_h3(%r9), %r12
        movq    %rax, FCS_g2(%r9)
        movq    %r12, FCS_h2(%r9)
        movq    %r10, FCS_g3(%r9)
        movq    %r11, FCS_h3(%r9)
        jmp     *%r12
        ud2

4:      // fast lookup failed
        /* stats only */
        movabsq $VG_(stats__n_xIndir_misses_32), %r8
        addl    $1, (%r8)

	movq	$VG_TRC_INNER_FASTMISS, %rax
        movq    $0, %rdx
	jmp	postamble

/* ------ Assisted jump ------ */
.globl VG_(disp_cp_xassisted)
VG_(disp_cp_xassisted):
        /* %rbp contains the TRC */
        movq    %rbp, %rax
        movq    $0, %rdx
        jmp     postamble

/* ------ Event check failed ------ */
.globl VG_(disp_cp_evcheck_fail)
VG_(disp_cp_evcheck_fail):
       	movq	$VG_TRC_INNER_COUNTERZERO, %rax
        movq    $0, %rdx
	jmp	postamble


#endif // defined(VGP_amd64_darwin)

/* Let the linker know we don't need an executable stack */
MARK_STACK_NO_EXEC

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
