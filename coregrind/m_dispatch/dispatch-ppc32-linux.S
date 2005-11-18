
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

        /* For Linux/ppc32 we need the SysV ABI, which uses
           LR->4(parent_sp), CR->anywhere.
           (The AIX ABI, used on Darwin, and maybe Linux/ppc64?,
           uses LR->8(prt_sp), CR->4(prt_sp))
        */

        /* Save lr */
        mflr    0
        stw     0,4(1)

        /* New stack frame */
        stwu    1,-496(1)  /* sp should maintain 16-byte alignment */

        /* Save callee-saved registers... */
	/* r3 is live here (guest state ptr), so use r4 */
        lis     4,VG_(machine_ppc32_has_FP)@ha
        lwz     4,VG_(machine_ppc32_has_FP)@l(4)
        cmplwi  4,0
        beq     LafterFP1

        /* Floating-point reg save area : 144 bytes */
        stfd    31,488(1)
        stfd    30,480(1)
        stfd    29,472(1)
        stfd    28,464(1)
        stfd    27,456(1)
        stfd    26,448(1)
        stfd    25,440(1)
        stfd    24,432(1)
        stfd    23,424(1)
        stfd    22,416(1)
        stfd    21,408(1)
        stfd    20,400(1)
        stfd    19,392(1)
        stfd    18,384(1)
        stfd    17,376(1)
        stfd    16,368(1)
        stfd    15,360(1)
        stfd    14,352(1)
LafterFP1:

        /* General reg save area : 72 bytes */
        stw     31,348(1)
        stw     30,344(1)
        stw     29,340(1)
        stw     28,336(1)
        stw     27,332(1)
        stw     26,328(1)
        stw     25,324(1)
        stw     24,320(1)
        stw     23,316(1)
        stw     22,312(1)
        stw     21,308(1)
        stw     20,304(1)
        stw     19,300(1)
        stw     18,296(1)
        stw     17,292(1)
        stw     16,288(1)
        stw     15,284(1)
        stw     14,280(1)
        /* Probably not necessary to save r13 (thread-specific ptr),
           as VEX stays clear of it... but what the hey. */
        stw     13,276(1)

        /* It's necessary to save/restore VRSAVE in the AIX / Darwin ABI.
           The Linux kernel might not actually use VRSAVE for its intended
           purpose, but it should be harmless to preserve anyway. */
	/* r3 is live here (guest state ptr), so use r4 */
        lis     4,VG_(machine_ppc32_has_VMX)@ha
        lwz     4,VG_(machine_ppc32_has_VMX)@l(4)
        cmplwi  4,0
        beq     LafterVMX1

        /* VRSAVE save word : 32 bytes */
        mfspr   4,256         /* vrsave reg is spr number 256 */
        stw     4,244(1)

        /* Alignment padding : 4 bytes */

        /* Vector reg save area (quadword aligned) : 192 bytes */
        li      4,224
        stvx    31,4,1
        li      4,208
        stvx    30,4,1
        li      4,192
        stvx    29,4,1
        li      4,176
        stvx    28,4,1
        li      4,160
        stvx    27,4,1
        li      4,144
        stvx    26,4,1
        li      4,128
        stvx    25,4,1
        li      4,112
        stvx    24,4,1
        li      4,96
        stvx    23,4,1
        li      4,80
        stvx    22,4,1
        li      4,64
        stvx    21,4,1
        li      4,48
        stvx    20,4,1
LafterVMX1:

        /* Save cr */
        mfcr    0
        stw     0,44(1)

        /* Local variable space... */

        /* 32(sp) used later to check FPSCR[RM] */

        /* r3 holds guest_state */
        mr      31,3
        stw     3,28(1)       /* spill orig guest_state ptr */

        /* 24(sp) used later to stop ctr reg being clobbered */

	/* 8:20(sp) free */
	
        /* Linkage Area (reserved)
           4(sp)  : LR
           0(sp)  : back-chain
        */

// CAB TODO: Use a caller-saved reg for orig guest_state ptr
// - rem to set non-allocateable in isel.c

        /* hold dispatch_ctr in ctr reg */
        lis     17,VG_(dispatch_ctr)@ha
        lwz     17,VG_(dispatch_ctr)@l(17)
        mtctr   17

        /* fetch %CIA into r30 */
        lwz     30,OFFSET_ppc32_CIA(31)

        /* set host FPU control word to the default mode expected 
           by VEX-generated code.  See comments in libvex.h for
           more info. */
        lis     3,VG_(machine_ppc32_has_FP)@ha
        lwz     3,VG_(machine_ppc32_has_FP)@l(3)
        cmplwi  3,0
        beq     LafterFP2

        fsub    3,3,3   /* generate zero */
        mtfsf   0xFF,3
LafterFP2:

        /* set host AltiVec control word to the default mode expected 
           by VEX-generated code. */
        lis     3,VG_(machine_ppc32_has_VMX)@ha
        lwz     3,VG_(machine_ppc32_has_VMX)@l(3)
        cmplwi  3,0
        beq     LafterVMX2

        vspltisw 3,0x0  /* generate zero */
        mtvscr  3
LafterVMX2:

        /* make a stack frame for the code we are calling */
        stwu    1,-16(1)

        /* fall into main loop */

/* Live regs:
	r1 (=sp)
	r30 (=CIA = jump address)
	r31 (=guest_state)
	ctr (=dispatch_ctr)
   Stack state:
	44(r1) (=orig guest_state)
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
        stw     9,40(1)  /* => 40-16 = 24(1) on our parent stack */

        blrl


        /* On return from guest code:
	   r3 holds destination (original) address.

           r31 may be unchanged (guest_state), or may indicate further
           details of the control transfer requested to *r3.

           If r31 is unchanged (== 44(r1)), just jump next to r3.

           Otherwise fall out, back to the scheduler, and let it
           figure out what to do next.
        */

	/* reinstate clobbered ctr */
        lwz     9,40(1)
        mtctr   9

        mr      30,3             /* put CIA (=r3) in r30 */
        lwz     16,44(1)         /* original guest_state ptr */
        cmpw    16,31
        beq     dispatch_boring  /* r31 unchanged... */

        mr      3,31             /* put return val (=r31) in r3 */
        b       dispatch_exceptional

/* All exits from the dispatcher go through here.
   r3 holds the return value. 
*/
run_innerloop_exit: 
        /* We're leaving.  Check that nobody messed with
           VSCR or FPSCR. */

        /* Using r10 - value used again further on, so don't trash! */
        lis     10,VG_(machine_ppc32_has_FP)@ha
        lwz     10,VG_(machine_ppc32_has_FP)@l(10)
        cmplwi  10,0
        beq     LafterFP8

        /* Check FPSCR & 0xFF == 0 (lowest 8bits are controls)  */
        mffs      4                       /* fpscr -> fpr */
        li        5,48
        stfiwx    4,5,1                   /* fpr to stack */
        lwzx      6,5,1                   /* load to gpr */
        andi.     6,6,0xFF                /* mask wanted bits */
        cmplwi    6,0x0                   /* cmp with zero */
        bne       invariant_violation     /* branch if not zero */
LafterFP8:

	/* Using r11 - value used again further on, so don't trash! */
        lis     11,VG_(machine_ppc32_has_VMX)@ha
        lwz     11,VG_(machine_ppc32_has_VMX)@l(11)
        cmplwi  11,0
        beq     LafterVMX8

        /* Check VSCR[NJ] == 1 */
        /* first generate 4x 0x00010000 */
        vspltisw  4,0x1                   /* 4x 0x00000001 */
        vspltisw  5,0x0                   /* zero */
        vsldoi    6,4,5,0x2               /* <<2*8 => 4x 0x00010000 */
        /* retrieve VSCR and mask wanted bits */
        mfvscr    7
        vand      7,7,6                   /* gives NJ flag */
        vspltw    7,7,0x3                 /* flags-word to all lanes */
        vcmpequw. 8,6,7                   /* CR[24] = 1 if v6 == v7 */
        bt        24,invariant_violation  /* branch if all_equal */
LafterVMX8:

	/* otherwise we're OK */
        b       run_innerloop_exit_REALLY


invariant_violation:
        li      3,VG_TRC_INVARIANT_FAILED
        b       run_innerloop_exit_REALLY

run_innerloop_exit_REALLY:
        /* r3 holds VG_TRC_* value to return */

        /* Return to parent stack */
        addi    1,1,16

        /* Write ctr to VG(dispatch_ctr) */
        mfctr   17
        lis     18,VG_(dispatch_ctr)@ha
        stw     17,VG_(dispatch_ctr)@l(18)

        /* Restore cr */
        lwz     0,44(1)
        mtcr    0

        /* Restore callee-saved registers... */

        /* r10 already holds VG_(machine_ppc32_has_FP) value */
        cmplwi  10,0
        beq     LafterFP9

        /* Floating-point regs */
        lfd     31,488(1)
        lfd     30,480(1)
        lfd     29,472(1)
        lfd     28,464(1)
        lfd     27,456(1)
        lfd     26,448(1)
        lfd     25,440(1)
        lfd     24,432(1)
        lfd     23,424(1)
        lfd     22,416(1)
        lfd     21,408(1)
        lfd     20,400(1)
        lfd     19,392(1)
        lfd     18,384(1)
        lfd     17,376(1)
        lfd     16,368(1)
        lfd     15,360(1)
        lfd     14,352(1)
LafterFP9:

        /* General regs */
        lwz     31,348(1)
        lwz     30,344(1)
        lwz     29,340(1)
        lwz     28,336(1)
        lwz     27,332(1)
        lwz     26,328(1)
        lwz     25,324(1)
        lwz     24,320(1)
        lwz     23,316(1)
        lwz     22,312(1)
        lwz     21,308(1)
        lwz     20,304(1)
        lwz     19,300(1)
        lwz     18,296(1)
        lwz     17,292(1)
        lwz     16,288(1)
        lwz     15,284(1)
        lwz     14,280(1)
        lwz     13,276(1)

        /* r11 already holds VG_(machine_ppc32_has_VMX) value */
        cmplwi  11,0
        beq     LafterVMX9

        /* VRSAVE */
        lwz     4,244(1)
        mfspr   4,256         /* VRSAVE reg is spr number 256 */

        /* Vector regs */
        li      4,224
        lvx     31,4,1
        li      4,208
        lvx     30,4,1
        li      4,192
        lvx     29,4,1
        li      4,176
        lvx     28,4,1
        li      4,160
        lvx     27,4,1
        li      4,144
        lvx     26,4,1
        li      4,128
        lvx     25,4,1
        li      4,112
        lvx     24,4,1
        li      4,96
        lvx     23,4,1
        li      4,80
        lvx     22,4,1
        li      4,64
        lvx     21,4,1
        li      4,48
        lvx     20,4,1
LafterVMX9:

        /* reset lr & sp */
        lwz     0,500(1)  /* stack_size + 4 */
        mtlr    0
        addi    1,1,496   /* stack_size */
        blr


/* Other ways of getting out of the inner loop.  Placed out-of-line to
   make it look cleaner. 
*/
dispatch_exceptional:
	/* this is jumped to only, not fallen-through from above */
	/* save r30 in %CIA and defer to sched */
        lwz     16,44(1)
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
