
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

#include "pub_core_basics_asm.h"
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

        /* New stack frame */
        stwu    1,-432(1)  /* sp should maintain 16-byte alignment */

        /* CAB: should put this gap somewhere else - see ppc-abi */

        /* callee-saved regs
           http://developer.apple.com : PowerPCRuntime.pdf : p27 */
        stw     31,424(1)
        stw     30,420(1)
        stw     29,416(1)
        stw     28,412(1)
        stw     27,408(1)
        stw     26,404(1)
        stw     25,400(1)
        stw     24,396(1)
        stw     23,392(1)
        stw     22,388(1)
        stw     21,384(1)
        stw     20,380(1)
        stw     19,376(1)
        stw     18,372(1)
        stw     17,368(1)
        stw     16,364(1)
        stw     15,360(1)
        stw     14,356(1)
        stw     13,352(1)

        stfd    31,344(1)
        stfd    30,336(1)
        stfd    29,328(1)
        stfd    28,320(1)
        stfd    27,312(1)
        stfd    26,304(1)
        stfd    25,296(1)
        stfd    24,288(1)
        stfd    23,280(1)
        stfd    22,272(1)
        stfd    21,264(1)
        stfd    20,256(1)
        stfd    19,248(1)
        stfd    18,240(1)
        stfd    17,232(1)
        stfd    16,224(1)
        stfd    15,216(1)
        stfd    14,208(1)

        li      4,192
        stvx    31,4,1
        li      4,176
        stvx    30,4,1
        li      4,160
        stvx    29,4,1
        li      4,144
        stvx    28,4,1
        li      4,128
        stvx    27,4,1
        li      4,112
        stvx    26,4,1
        li      4,96
        stvx    25,4,1
        li      4,80
        stvx    24,4,1
        li      4,64
        stvx    23,4,1
        li      4,48
        stvx    22,4,1
        li      4,32
        stvx    21,4,1
        li      4,16
        stvx    20,4,1

        /* r3 holds guest_state */
        mr      31,3
        stw     3,12(1)       /* spill orig guest_state ptr */

        /* 8(1) used later to stop ctr reg being clobbered
           4(1) = standard LR-save space
        */

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
        lis     3,VG_(have_altivec_ppc32)@ha
        lwz     3,VG_(have_altivec_ppc32)@l(3)
        cmplwi  3,0
        beq     L1
        /* generate vector {0x0,0x0,0x0,0x00010000} */
        vspltisw 3,0x1      /* 4x 0x00000001 */
        vspltisw 4,0x0      /* generate zero */
        vsldoi  3,4,3,0x6   /* v3 = v3 >> 10 bytes */
        mtvscr  3
L1:
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
        /* r4=((r30<<2) & (VG_TT_FAST_MASK<<2)) */
        rlwinm  4,30, 2, 32-2-VG_TT_FAST_BITS, 31-2  
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
        stw     9,24(1)  /* => 24-16 = 8(1) on our parent stack */

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

        /* restore callee-saved registers */
        li      4,16
        lvx     20,4,1
        li      4,32
        lvx     21,4,1
        li      4,48
        lvx     22,4,1
        li      4,64
        lvx     23,4,1
        li      4,80
        lvx     24,4,1
        li      4,96
        lvx     25,4,1
        li      4,112
        lvx     26,4,1
        li      4,128
        lvx     27,4,1	
        li      4,144
        lvx     28,4,1
        li      4,160
        lvx     29,4,1
        li      4,176
        lvx     30,4,1
        li      4,192
        lvx     31,4,1

        lfd     14,208(1)
        lfd     15,216(1)
        lfd     16,224(1)
        lfd     17,232(1)
        lfd     18,240(1)
        lfd     19,248(1)
        lfd     20,256(1)
        lfd     21,264(1)
        lfd     22,272(1)
        lfd     23,280(1)
        lfd     24,288(1)
        lfd     25,296(1)
        lfd     26,304(1)
        lfd     27,312(1)
        lfd     28,320(1)
        lfd     29,328(1)
        lfd     30,336(1)
        lfd     31,344(1)

        lwz     13,352(1)
        lwz     14,356(1)
        lwz     15,360(1)
        lwz     16,364(1)
        lwz     17,368(1)
        lwz     18,372(1)
        lwz     19,376(1)
        lwz     20,380(1)
        lwz     21,384(1)
        lwz     22,388(1)
        lwz     23,392(1)
        lwz     24,396(1)
        lwz     25,400(1)
        lwz     26,404(1)
        lwz     27,408(1)
        lwz     28,412(1)
        lwz     29,416(1)
        lwz     30,420(1)
        lwz     31,424(1)

        /* reset lr & sp */
        lwz     0,436(1)  /* stack_size + 4 */
        mtlr    0
        addi    1,1,432   /* stack_size */
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
