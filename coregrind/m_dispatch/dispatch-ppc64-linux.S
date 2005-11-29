
##--------------------------------------------------------------------##
##--- The core dispatch loop, for jumping to a code address.       ---##
##---                                             dispatch-ppc64.S ---##
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
#include "libvex_guest_offsets.h"	/* for OFFSET_ppc64_CIA */


/* References to globals via the TOC */

	.section        ".toc","aw"
tocent__vgPlain_tt_fast:
	.tc vgPlain_tt_fast[TC],vgPlain_tt_fast

/*------------------------------------------------------------*/
/*--- The dispatch loop.                                   ---*/
/*------------------------------------------------------------*/

	.section	".text"
	.align 2

/* signature: UWord VG_(run_innerloop) ( void* guest_state ) */

        .globl  .VG_(run_innerloop)
.VG_(run_innerloop):
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
        stwu    1,-624(1)  /* sp should maintain 16-byte alignment */

        /* Save callee-saved registers... */

        /* Floating-point reg save area : 144 bytes */
        stfd    31,616(1)
        stfd    30,608(1)
        stfd    29,600(1)
        stfd    28,592(1)
        stfd    27,584(1)
        stfd    26,576(1)
        stfd    25,568(1)
        stfd    24,560(1)
        stfd    23,552(1)
        stfd    22,544(1)
        stfd    21,536(1)
        stfd    20,528(1)
        stfd    19,520(1)
        stfd    18,512(1)
        stfd    17,504(1)
        stfd    16,496(1)
        stfd    15,488(1)
        stfd    14,480(1)

        /* General reg save area : 144 bytes */
        stw     31,472(1)
        stw     30,464(1)
        stw     29,456(1)
        stw     28,448(1)
        stw     27,440(1)
        stw     26,432(1)
        stw     25,424(1)
        stw     24,416(1)
        stw     23,408(1)
        stw     22,400(1)
        stw     21,392(1)
        stw     20,384(1)
        stw     19,376(1)
        stw     18,368(1)
        stw     17,360(1)
        stw     16,352(1)
        stw     15,344(1)
        stw     14,336(1)
        /* Probably not necessary to save r13 (thread-specific ptr),
           as VEX stays clear of it... but what the hey. */
        stw     13,328(1)

        /* It's necessary to save/restore VRSAVE in the AIX / Darwin ABI.
           The Linux kernel might not actually use VRSAVE for its intended
           purpose, but it should be harmless to preserve anyway. */
	/* r3 is live here (guest state ptr), so use r4 */
        lis     4,VG_(machine_ppc64_has_VMX)@ha
        lwz     4,VG_(machine_ppc64_has_VMX)@l(4)
        cmplwi  4,0
        beq     LafterVMX1

        /* VRSAVE save word : 32 bytes */
        mfspr   4,256         /* vrsave reg is spr number 256 */
        stw     4,324(1)

        /* Alignment padding : 4 bytes */

        /* Vector reg save area (quadword aligned) : 192 bytes */
        li      4,304
        stvx    31,4,1
        li      4,288
        stvx    30,4,1
        li      4,272
        stvx    29,4,1
        li      4,256
        stvx    28,4,1
        li      4,240
        stvx    27,4,1
        li      4,224
        stvx    26,4,1
        li      4,208
        stvx    25,4,1
        li      4,192
        stvx    24,4,1
        li      4,176
        stvx    23,4,1
        li      4,160
        stvx    22,4,1
        li      4,144
        stvx    21,4,1
        li      4,128
        stvx    20,4,1
LafterVMX1:

        /* Save cr */
        mfcr    0
        stw     0,112(1)


        /* Local variable space... */

        /* r3 holds guest_state */
        mr      31,3
        stw     3,104(1)       /* spill orig guest_state ptr */

        /* 96(sp) used later to check FPSCR[RM] */

        /* 88(sp) used later to stop ctr reg being clobbered */

	/* 48:87(sp) free */
	
        /* Linkage Area (reserved)
           40(sp) : TOC
           32(sp) : link editor doubleword
           24(sp) : compiler doubleword
           16(sp) : LR
           8(sp)  : CR
           0(sp)  : back-chain
        */

// CAB TODO: Use a caller-saved reg for orig guest_state ptr
// - rem to set non-allocateable in isel.c

        /* hold dispatch_ctr in ctr reg */
        lis     17,VG_(dispatch_ctr)@ha
        lwz     17,VG_(dispatch_ctr)@l(17)
        mtctr   17

        /* fetch %CIA into r30 */
        lwz     30,OFFSET_ppc64_CIA(31)

        /* set host FPU control word to the default mode expected 
           by VEX-generated code.  See comments in libvex.h for
           more info. */
        fsub    3,3,3   /* generate zero */
        mtfsf   0xFF,3

        /* set host AltiVec control word to the default mode expected 
           by VEX-generated code. */
        lis     3,VG_(machine_ppc64_has_VMX)@ha
        lwz     3,VG_(machine_ppc64_has_VMX)@l(3)
        cmplwi  3,0
        beq     LafterVMX2

        vspltisw 3,0x0  /* generate zero */
        mtvscr  3
LafterVMX2:

        /* make a stack frame for the code we are calling */
        stwu    1,-48(1)

        /* fall into main loop */

/* Live regs:
	r1 (=sp)
	r2 (toc pointer)
	r30 (=guest CIA = jump address)
	r31 (=guest_state)
	ctr (=dispatch_ctr)
   Stack state:
	104 (r1) (=guest_state ptr)
	96 (r1) (=var space for FPSCR[RM])
	88 (r1) (=var space for CTR)
	44:87 (r1) (=free)
        0:43 (r1) (=stack frame header)
*/

dispatch_boring:
        /* save the jump address in the guest state */
        std     30,OFFSET_ppc64_CIA(31)

        /* Are we out of timeslice?  If yes, defer to scheduler. */
        bdz     counter_is_zero  /* decrements ctr reg */

        /* try a fast lookup in the translation cache */
        /* r4=((r30<<3) & (VG_TT_FAST_MASK<<3)) */
	rldic	4,30, 3,64-3-VG_TT_FAST_BITS
// CAB:	use a caller-saved reg for this ?
	/* r5 = & VG_(tt_fast) */
	ld	5, tocent__vgPlain_tt_fast@toc(2)
	/* r5 = VG_(tt_fast)[r30 & VG_TT_FAST_MASK] */
	ldx	5, 5,4
	/* r6 = VG_(tt_fast)[r30 & VG_TT_FAST_MASK]->orig_addr */
	ld	6, 0(5)
        cmpw    30,6
        bne     fast_lookup_failed

        /* increment bb profile counter */
// CAB:	use a caller-saved reg for this ?
	/* r7 = & VG_(tt_fastN) */
	ld	7, tocent__vgPlain_tt_fast@toc(2)
	/* r7 = VG_(tt_fastN)[r30 & VG_TT_FAST_MASK] */
	srdi	4, 4,1
	lwzx	6, 7,4
	addi	6, 6,1
	stwx	6, 7,4

        /* Found a match.  Call tce[1], which is 8 bytes along, since
           each tce element is a 64-bit int. */
        addi    8,5,8
        mtlr    8

        /* stop ctr being clobbered */
// CAB:	use a caller-saved reg for this ?
//      but then (bdz) => (decr, cmp, bc)... still better than a stw?
        mfctr   9
        std     9,136(1)         /* => 88(parent_sp) */

        blrl


        /* On return from guest code:
	   r3 holds destination (original) address.

           r31 may be unchanged (guest_state), or may indicate further
           details of the control transfer requested to *r3.

           If r31 is unchanged, just jump next to r3.

           Otherwise fall out, back to the scheduler, and let it
           figure out what to do next.
        */

	/* reinstate clobbered ctr */
        ld      9,136(1)          /* => 88(parent_sp) */
        mtctr   9

        mr      30,3             /* put CIA (=r3) in r30 */
        ld      16,152(1)        /* gst_state ptr => 104(prnt_sp) */
        cmpd    16,31
        beq     dispatch_boring  /* r31 unchanged... */

        mr      3,31             /* put return val (=r31) in r3 */
        b       dispatch_exceptional

/* All exits from the dispatcher go through here.
   r3 holds the return value. 
*/
run_innerloop_exit: 
        /* We're leaving.  Check that nobody messed with
           VSCR or FPSCR. */

/* This check avoidance may be removable if stfiwx is implemented. */
#if !defined(ENABLE_INNER)
        /* Check FPSCR & 0xFF == 0 (lowest 8bits are controls)  */
        mffs      4                       /* fpscr -> fpr */
        li        5,144                   /* => 96(parent_sp) */
        stfiwx    4,5,1                   /* fpr to stack */
        lwzx      6,5,1                   /* load to gpr */
        andi.     6,6,0xFF                /* mask wanted bits */
        cmplwi    6,0x0                   /* cmp with zero */
        bne       invariant_violation     /* branch if not zero */
#endif

	/* Using r11 - value used again further on, so don't trash! */
        lis     11,VG_(machine_ppc64_has_VMX)@ha
        lwz     11,VG_(machine_ppc64_has_VMX)@l(11)
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
        addi    1,1,48

        /* Write ctr to VG(dispatch_ctr) */
        mfctr   17
        lis     18,VG_(dispatch_ctr)@ha
        stw     17,VG_(dispatch_ctr)@l(18)

        /* Restore cr */
        lwz     0,112(1)
        mtcr    0

        /* Restore callee-saved registers... */

        /* Floating-point regs */
        lfd    31,616(1)
        lfd    30,608(1)
        lfd    29,600(1)
        lfd    28,592(1)
        lfd    27,584(1)
        lfd    26,576(1)
        lfd    25,568(1)
        lfd    24,560(1)
        lfd    23,552(1)
        lfd    22,544(1)
        lfd    21,536(1)
        lfd    20,528(1)
        lfd    19,520(1)
        lfd    18,512(1)
        lfd    17,504(1)
        lfd    16,496(1)
        lfd    15,488(1)
        lfd    14,480(1)

        /* General regs */
        lwz     31,472(1)
        lwz     30,464(1)
        lwz     29,456(1)
        lwz     28,448(1)
        lwz     27,440(1)
        lwz     26,432(1)
        lwz     25,424(1)
        lwz     24,416(1)
        lwz     23,408(1)
        lwz     22,400(1)
        lwz     21,392(1)
        lwz     20,384(1)
        lwz     19,376(1)
        lwz     18,368(1)
        lwz     17,360(1)
        lwz     16,352(1)
        lwz     15,344(1)
        lwz     14,336(1)
        lwz     13,328(1)

        /* r11 already holds VG_(machine_ppc64_has_VMX) value */
        cmplwi  11,0
        beq     LafterVMX9

        /* VRSAVE */
        lwz     4,324(1)
        mfspr   4,256         /* VRSAVE reg is spr number 256 */

        /* Vector regs */
        li      4,304
        lvx     31,4,1
        li      4,288
        lvx     30,4,1
        li      4,272
        lvx     29,4,1
        li      4,256
        lvx     28,4,1
        li      4,240
        lvx     27,4,1
        li      4,224
        lvx     26,4,1
        li      4,208
        lvx     25,4,1
        li      4,192
        lvx     24,4,1
        li      4,176
        lvx     23,4,1
        li      4,160
        lvx     22,4,1
        li      4,144
        lvx     21,4,1
        li      4,128
        lvx     20,4,1
LafterVMX9:

        /* reset lr & sp */
        lwz     0,628(1)  /* stack_size + 4 */
        mtlr    0
        addi    1,1,624   /* stack_size */
        blr


/* Other ways of getting out of the inner loop.  Placed out-of-line to
   make it look cleaner. 
*/
dispatch_exceptional:
	/* this is jumped to only, not fallen-through from above */
	/* save r30 in %CIA and defer to sched */
        lwz     16,152(1)
        stw     30,OFFSET_ppc64_CIA(16)
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
