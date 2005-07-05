
##--------------------------------------------------------------------##
##--- The core dispatch loop, for jumping to a code address.       ---##
##---                                             dispatch-ppc32.S ---##
##--------------------------------------------------------------------##

/*
  This file is part of Valgrind, a dynamic binary instrumentation
  framework.

  Copyright (C) 2005 Cerion Armour-Brown <cerion@open-works.co.uk>

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

#include "pub_tool_basics_asm.h"
#include "pub_core_dispatch_asm.h"
#include "pub_core_transtab_asm.h"
#include "libvex_guest_offsets.h"	/* for OFFSET_ppc32_CIA */


/*------------------------------------------------------------*/
/*--- The dispatch loop.                                   ---*/
/*------------------------------------------------------------*/

/* signature: UWord VG_(run_innerloop) ( void* guest_state ) */

        .globl  VG_(run_innerloop)
VG_(run_innerloop):
        /* ----- entry point to VG_(run_innerloop) ----- */

        /* Save lr, sp */
        mflr    0
        stw     0,4(1)

        /* New stack frame: save callee-saved regs */
        stwu    1,-88(1)
        stw     31,84(1)
        stw     30,80(1)
        stw     29,76(1)
        stw     28,72(1)
        stw     27,68(1)
        stw     26,64(1)
        stw     25,60(1)
        stw     24,56(1)
        stw     23,52(1)
        stw     22,48(1)
        stw     21,44(1)
        stw     20,40(1)
        stw     19,36(1)
        stw     18,32(1)
        stw     17,28(1)
        stw     16,24(1)
        stw     15,20(1)
        stw     14,16(1)

        /* r3 holds guest_state */
        mr      31,3
        stw     3,12(1)       /* spill orig guest_state ptr */

// CAB TODO: Use a caller-saved reg for orig guest_state ptr
// - rem to set non-allocateable in isel.c

        /* hold dispach_ctr in ctr reg */
        lis     17,VG_(dispatch_ctr)@ha
        lwz     17,VG_(dispatch_ctr)@l(17)
	mtctr   17

        /* fetch %CIA into r30 */
        lwz     30,OFFSET_ppc32_CIA(31)

	/* set host FPU control word to the default mode expected 
           by VEX-generated code.  See comments in libvex.h for
           more info. */
        fsub    3,3,3   /* generate zero */
        mtfsf   0xFF,3

	/* set host AltiVec control word to the default mode expected 
	   by VEX-generated code. */
        vsubuwm 3,3,3   /* generate zero */
        mtvscr  3

        /* make a stack frame for the code we are calling */
        stwu    1,-16(1)

        /* fall into main loop */

/* Live regs:
	r1 (=sp)
	r30 (=CIA = jump address)
	r31 (=guest_state)
	ctr (=dispatch_ctr)
   Stack state:
	28(r1) (=orig guest_state)
*/

dispatch_boring:
        /* save the jump address in the guest state */
        stw     30,OFFSET_ppc32_CIA(31)

        /* Are we out of timeslice?  If yes, defer to scheduler. */
        bdz     counter_is_zero  /* decrements ctr reg */

        /* try a fast lookup in the translation cache */
        rlwinm  4,30,2,VG_TT_FAST_MASK<<2  /* r4=((r30<<2) & (MASK<<2)) */
// CAB:	use a caller-saved reg for this ?
        addis   5,4,VG_(tt_fast)@ha
        lwz     5,VG_(tt_fast)@l(5)
        lwz     6,4(5)   /* big-endian, so comparing 2nd 32bit word */
        cmpw    30,6
	bne     fast_lookup_failed

        /* increment bb profile counter */
// CAB:	use a caller-saved reg for this ?
        addis   6,4,VG_(tt_fastN)@ha
        lwz     7,VG_(tt_fastN)@l(6)
        lwz     8,0(7)
        addi    8,8,1
        stw     8,0(7)

        /* Found a match.  Call tce[1], which is 8 bytes along, since
           each tce element is a 64-bit int. */
        addi    8,5,8
        mtlr    8

        /* stop ctr being clobbered */
// CAB:	use a caller-saved reg for this ?
//      but then (bdz) => (decr, cmp, bc)... still better than a stw?
        mfctr   9
        stw     9,24(1)

        blrl


        /* On return from guest code:
	   r3 holds destination (original) address.

           r31 may be unchanged (guest_state), or may indicate further
           details of the control transfer requested to *r3.

           If r31 is unchanged (== 28(r1)), just jump next to r3.

           Otherwise fall out, back to the scheduler, and let it
           figure out what to do next.
        */

	/* reinstate clobbered ctr */
        lwz     9,24(1)
        mtctr   9

	mr      30,3             /* put CIA (=r3) in r30 */
        lwz     16,28(1)         /* original guest_state ptr */
        cmpw    16,31
        beq     dispatch_boring  /* r31 unchanged... */

	mr      3,31             /* put return val (=r31) in r3 */
        b       dispatch_exceptional

/* All exits from the dispatcher go through here.
   r3 holds the return value. 
*/
run_innerloop_exit: 
        /* We're leaving.  Check that nobody messed with
           %mxcsr or %fpucw.  We can't mess with %eax here as it
           holds the tentative return value, but any other is OK. */
// CAB: TODO
	
//.. 	pushl	$0
//.. 	fstcw	(%esp)
//.. 	cmpl	$0x027F, (%esp)
//.. 	popl	%esi /* get rid of the word without trashing %eflags */
//.. 	jnz	invariant_violation

//.. 	pushl	$0
//.. 	stmxcsr	(%esp)
//.. 	andl	$0xFFFFFFC0, (%esp)  /* mask out status flags */
//.. 	cmpl	$0x1F80, (%esp)
//.. 	popl	%esi
//.. 	jnz	invariant_violation
	
	/* otherwise we're OK */
        b       run_innerloop_exit_REALLY


invariant_violation:
        li      3,VG_TRC_INVARIANT_FAILED
        b       run_innerloop_exit_REALLY

run_innerloop_exit_REALLY:
        /* r3 holds VG_TRC_* value to return */

        addi    1,1,16

        mfctr   17
        lis     18,VG_(dispatch_ctr)@ha
        stw     17,VG_(dispatch_ctr)@l(18)

        lwz     14,16(1)
        lwz     15,20(1)
        lwz     16,24(1)
        lwz     17,28(1)
        lwz     18,32(1)
        lwz     19,36(1)
        lwz     20,40(1)
        lwz     21,44(1)
        lwz     22,48(1)
        lwz     23,52(1)
        lwz     24,56(1)
        lwz     25,60(1)
        lwz     26,64(1)
        lwz     27,68(1)
        lwz     28,72(1)
        lwz     29,76(1)
        lwz     30,80(1)
        lwz     31,84(1)
        lwz     0,92(1)
        mtlr    0
        addi    1,1,88
        blr


/* Other ways of getting out of the inner loop.  Placed out-of-line to
   make it look cleaner. 
*/
dispatch_exceptional:
	/* this is jumped to only, not fallen-through from above */
	/* save r30 in %CIA and defer to sched */
        lwz     16,28(1)
        stw     30,OFFSET_ppc32_CIA(16)
        b       run_innerloop_exit

fast_lookup_failed:
	/* %CIA is up to date here since dispatch_boring dominates */
        mfctr   17
        addi    17,17,1
	mtctr   17
        li      3,VG_TRC_INNER_FASTMISS
	b       run_innerloop_exit
        

counter_is_zero:
	/* %CIA is up to date here since dispatch_boring dominates */
        mfctr   17
        addi    17,17,1
	mtctr   17
        li      3,VG_TRC_INNER_COUNTERZERO
        b       run_innerloop_exit

/* Let the linker know we don't need an executable stack */
.section .note.GNU-stack,"",@progbits

##--------------------------------------------------------------------##
##--- end                                                          ---##
##--------------------------------------------------------------------##
