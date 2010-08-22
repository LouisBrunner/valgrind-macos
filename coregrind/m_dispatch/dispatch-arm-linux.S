/*--------------------------------------------------------------------*/
/*--- The core dispatch loop, for jumping to a code address.       ---*/
/*---                                         dispatch-arm-linux.S ---*/
/*--------------------------------------------------------------------*/

/*
  This file is part of Valgrind, a dynamic binary instrumentation
  framework.

  Copyright (C) 2008-2010 Evan Geller
     gaze@bea.ms

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

#if defined(VGP_arm_linux)
	.fpu vfp

#include "pub_core_basics_asm.h"
#include "pub_core_dispatch_asm.h"
#include "pub_core_transtab_asm.h"
#include "libvex_guest_offsets.h"	/* for OFFSET_arm_R* */


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
VG_(run_innerloop):
	push {r0, r1, r4, r5, r6, r7, r8, r9, fp, lr}

        /* set FPSCR to vex-required default value */
        mov  r4, #0
        fmxr fpscr, r4

        /* r0 (hence also [sp,#0]) holds guest_state */
        /* r1 holds do_profiling */
	mov r8, r0
	ldr r0, [r8, #OFFSET_arm_R15T]
        
       	/* fall into main loop (the right one) */
	cmp r1, #0      /* do_profiling */
	beq VG_(run_innerloop__dispatch_unprofiled)
	b   VG_(run_innerloop__dispatch_profiled)


/*----------------------------------------------------*/
/*--- NO-PROFILING (standard) dispatcher           ---*/
/*----------------------------------------------------*/

.global	VG_(run_innerloop__dispatch_unprofiled)
VG_(run_innerloop__dispatch_unprofiled):

	/* AT ENTRY: r0 is next guest addr, r8 is possibly
        modified guest state ptr */

        /* Has the guest state pointer been messed with?  If yes, exit. */
	ldr  r1, [sp, #0]
	cmp  r8, r1
	bne  gsp_changed

	/* save the jump address in the guest state */
        str  r0, [r8, #OFFSET_arm_R15T]

        /* Are we out of timeslice?  If yes, defer to scheduler. */
        ldr  r1, =VG_(dispatch_ctr)
        ldr  r2, [r1]
        subs r2, r2, #1
        str  r2, [r1]
        beq  counter_is_zero

        /* try a fast lookup in the translation cache */
        // r0 = next guest, r1,r2,r3 scratch
	ldr  r1, =VG_TT_FAST_MASK       // r1 = VG_TT_FAST_MASK
	and  r2, r1, r0, LSR #2         // r2 = entry #
	ldr  r1, =VG_(tt_fast)          // r1 = &tt_fast[0]
	add  r1, r1, r2, LSL #3         // r1 = &tt_fast[entry#]
	ldr  r3, [r1, #0]               /* .guest */
	ldr  r1, [r1, #4]               /* .host  */
	cmp  r0, r3
	bne  fast_lookup_failed
        // r1: live, next-host    r8: live, gsp
        // r2: entry # (but not live)
        // r0, r3: dead
        
        /* Found a match.  Jump to .host. */
	blx  r1
	b    VG_(run_innerloop__dispatch_unprofiled)
.ltorg
	/*NOTREACHED*/

/*----------------------------------------------------*/
/*--- PROFILING dispatcher (can be much slower)    ---*/
/*----------------------------------------------------*/

.global	VG_(run_innerloop__dispatch_profiled)
VG_(run_innerloop__dispatch_profiled):

	/* AT ENTRY: r0 is next guest addr, r8 is possibly
        modified guest state ptr */

        /* Has the guest state pointer been messed with?  If yes, exit. */
	ldr  r1, [sp, #0]
	cmp  r8, r1
	bne  gsp_changed

	/* save the jump address in the guest state */
        str  r0, [r8, #OFFSET_arm_R15T]

        /* Are we out of timeslice?  If yes, defer to scheduler. */
        ldr  r1, =VG_(dispatch_ctr)
        ldr  r2, [r1]
        subs r2, r2, #1
        str  r2, [r1]
        beq  counter_is_zero

        /* try a fast lookup in the translation cache */
        // r0 = next guest, r1,r2,r3 scratch
	ldr  r1, =VG_TT_FAST_MASK       // r1 = VG_TT_FAST_MASK
	and  r2, r1, r0, LSR #2         // r2 = entry #
	ldr  r1, =VG_(tt_fast)          // r1 = &tt_fast[0]
	add  r1, r1, r2, LSL #3         // r1 = &tt_fast[entry#]
	ldr  r3, [r1, #0]               /* .guest */
	ldr  r1, [r1, #4]               /* .host  */
	cmp  r0, r3
	bne  fast_lookup_failed
        // r1: live, next-host    r8: live, gsp
        // r2: entry # (but not live)
        // r0, r3: dead
        
        /* increment bb profile counter */
        ldr  r0, =VG_(tt_fastN)         // r0 = &tt_fastN[0]
        ldr  r0, [r0, r2, LSL #2]       // r0 = tt_fast[entry #]
        ldr  r3, [r0]                   // *r0 ++
        add  r3, r3, #1
        str  r3, [r0]

        /* Found a match.  Jump to .host. */
	blx  r1
	b    VG_(run_innerloop__dispatch_profiled)
	/*NOTREACHED*/

/*----------------------------------------------------*/
/*--- exit points                                  ---*/
/*----------------------------------------------------*/

gsp_changed:
        // r0 = next guest addr (R15T), r8 = modified gsp
        /* Someone messed with the gsp.  Have to
           defer to scheduler to resolve this.  dispatch ctr
           is not yet decremented, so no need to increment. */
        /* R15T is NOT up to date here.  First, need to write
           r0 back to R15T, but without trashing r8 since
           that holds the value we want to return to the scheduler.
           Hence use r1 transiently for the guest state pointer. */
	ldr r1, [sp, #0]
	str r0, [r1, #OFFSET_arm_R15T]
	mov r0, r8      // "return modified gsp"
	b run_innerloop_exit
        /*NOTREACHED*/

counter_is_zero:
        /* R15T is up to date here */
        /* Back out increment of the dispatch ctr */
        ldr  r1, =VG_(dispatch_ctr)
        ldr  r2, [r1]
        add  r2, r2, #1
        str  r2, [r1]
        mov  r0, #VG_TRC_INNER_COUNTERZERO
        b    run_innerloop_exit
        /*NOTREACHED*/
        
fast_lookup_failed:
        /* R15T is up to date here */
        /* Back out increment of the dispatch ctr */
        ldr  r1, =VG_(dispatch_ctr)
        ldr  r2, [r1]
        add  r2, r2, #1
        str  r2, [r1]
	mov  r0, #VG_TRC_INNER_FASTMISS
	b    run_innerloop_exit
        /*NOTREACHED*/

/* All exits from the dispatcher go through here.  %r0 holds
   the return value. 
*/
run_innerloop_exit:
        /* We're leaving.  Check that nobody messed with
           FPSCR in ways we don't expect. */
        fmrx r4, fpscr
        bic  r4, #0xF8000000 /* mask out NZCV and QC */
        bic  r4, #0x0000009F /* mask out IDC,IXC,UFC,OFC,DZC,IOC */
        cmp  r4, #0
        bne  invariant_violation
        b    run_innerloop_exit_REALLY

invariant_violation:
        mov  r0, #VG_TRC_INVARIANT_FAILED
        b    run_innerloop_exit_REALLY

run_innerloop_exit_REALLY:
	add sp, sp, #8
	pop {r4, r5, r6, r7, r8, r9, fp, pc}

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
.global VG_(run_a_noredir_translation)
VG_(run_a_noredir_translation):
	push {r0,r1 /* EABI compliance */, r4-r12, lr} 
	ldr r8, [r0, #4]
	mov lr, pc
	ldr pc, [r0, #0]

	pop {r1}
	str r0, [r1, #8]
	str r8, [r1, #12]
	pop {r1/*EABI compliance*/,r4-r12, pc}	

.size VG_(run_a_noredir_translation), .-VG_(run_a_noredir_translation)

/* Let the linker know we don't need an executable stack */
.section .note.GNU-stack,"",%progbits

#endif // defined(VGP_arm_linux)

/*--------------------------------------------------------------------*/
/*--- end                                     dispatch-arm-linux.S ---*/
/*--------------------------------------------------------------------*/
