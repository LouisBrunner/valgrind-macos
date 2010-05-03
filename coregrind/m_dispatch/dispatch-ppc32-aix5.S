
/*--------------------------------------------------------------------*/
/*--- The core dispatch loop, for jumping to a code address.       ---*/
/*---                                        dispatch-ppc32-aix5.S ---*/
/*--------------------------------------------------------------------*/

/*
  This file is part of Valgrind, a dynamic binary instrumentation
  framework.

  Copyright (C) 2006-2010 OpenWorks LLP
     info@open-works.co.uk

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

  Neither the names of the U.S. Department of Energy nor the
  University of California nor the names of its contributors may be
  used to endorse or promote products derived from this software
  without prior written permission.
*/

#if defined(VGP_ppc32_aix5)

#include "pub_core_basics_asm.h"
#include "pub_core_dispatch_asm.h"
#include "pub_core_transtab_asm.h"
#include "libvex_guest_offsets.h"	/* for OFFSET_ppc32_CIA */


/*------------------------------------------------------------*/
/*---                                                      ---*/
/*--- The dispatch loop.  VG_(run_innerloop) is used to    ---*/
/*--- run all translations except no-redir ones.           ---*/
/*---                                                      ---*/
/*------------------------------------------------------------*/

/*----------------------------------------------------*/
/*--- Incomprehensible TOC mumbo-jumbo nonsense.   ---*/
/*----------------------------------------------------*/

/* No, I don't have a clue either.  I just compiled a bit of
   C with gcc and copied the assembly code it produced. */

/* Basically "lwz rd, tocent__foo(2)" gets &foo into rd. */

    .file       "dispatch-ppc32-aix5.S"
    .machine	"any"
    .toc
    .csect .text[PR]
    .toc
tocent__vgPlain_dispatch_ctr:
    .tc vgPlain_dispatch_ctr[TC],vgPlain_dispatch_ctr[RW]
tocent__vgPlain_machine_ppc32_has_VMX:
    .tc vgPlain_machine_ppc32_has_VMX[TC],vgPlain_machine_ppc32_has_VMX[RW]
tocent__vgPlain_machine_ppc32_has_FP:
    .tc vgPlain_machine_ppc32_has_FP[TC],vgPlain_machine_ppc32_has_FP[RW]
tocent__vgPlain_tt_fast:
    .tc vgPlain_tt_fast[TC],vgPlain_tt_fast[RW]
tocent__vgPlain_tt_fastN:
    .tc vgPlain_tt_fast[TC],vgPlain_tt_fastN[RW]
    .csect .text[PR]
    .align 2
    .globl vgPlain_run_innerloop
    .globl .vgPlain_run_innerloop
    .csect vgPlain_run_innerloop[DS]
vgPlain_run_innerloop:
    .long .vgPlain_run_innerloop, TOC[tc0], 0
    .csect .text[PR]

/*----------------------------------------------------*/
/*--- Preamble (set everything up)                 ---*/
/*----------------------------------------------------*/

/* signature:
UWord VG_(run_innerloop) ( void* guest_state, UWord do_profiling );
*/
.vgPlain_run_innerloop:
	/* r3 holds guest_state */
	/* r4 holds do_profiling */
	/* Rather than attempt to make sense of the AIX ABI, just
           drop r1 by 256 (to get away from the caller's frame), then
	   512 (to give ourselves a 512-byte save area), and then
	   another 256 (to clear our save area).  In all, drop r1 by 1024
	   and dump stuff on the stack at 256(1)..768(1).  */

        /* ----- entry point to VG_(run_innerloop) ----- */
        /* For AIX/ppc32 we do:	 LR-> +8(parent_sp), CR-> +4(parent_sp) */

        /* Save lr and cr*/
        mflr    0
        stw     0,8(1)
	mfcr	0
	stw	0,4(1)

        /* New stack frame */
        stwu    1,-1024(1)  /* sp should maintain 16-byte alignment */

        /* Save callee-saved registers... */
	/* r3, r4 are live here, so use r5 */
        lwz     5,tocent__vgPlain_machine_ppc32_has_FP(2)
        lwz     5,0(5)
        cmplwi  5,0
        beq     LafterFP1

        /* Floating-point reg save area : 144 bytes at r1[256..399] */
        stfd    31,392(1)
        stfd    30,384(1)
        stfd    29,376(1)
        stfd    28,368(1)
        stfd    27,360(1)
        stfd    26,352(1)
        stfd    25,344(1)
        stfd    24,336(1)
        stfd    23,328(1)
        stfd    22,320(1)
        stfd    21,312(1)
        stfd    20,304(1)
        stfd    19,296(1)
        stfd    18,288(1)
        stfd    17,280(1)
        stfd    16,272(1)
        stfd    15,264(1)
        stfd    14,256(1)
LafterFP1:

        /* General reg save area : 76 bytes at r1[400 .. 475] */
        stw     31,472(1)
        stw     30,468(1)
        stw     29,464(1)
        stw     28,460(1)
        stw     27,456(1)
        stw     26,452(1)
        stw     25,448(1)
        stw     24,444(1)
        stw     23,440(1)
        stw     22,436(1)
        stw     21,432(1)
        stw     20,428(1)
        stw     19,424(1)
        stw     18,420(1)
        stw     17,416(1)
        stw     16,412(1)
        stw     15,408(1)
        stw     14,404(1)
        /* Probably not necessary to save r13 (thread-specific ptr),
           as VEX stays clear of it... but what the hell. */
        stw     13,400(1)

        /* It's necessary to save/restore VRSAVE in the AIX / Darwin ABI.
           The Linux kernel might not actually use VRSAVE for its intended
           purpose, but it should be harmless to preserve anyway. */
	/* r3, r4 are live here, so use r5 */
        lwz     5,tocent__vgPlain_machine_ppc32_has_VMX(2)
        lwz     5,0(5)
        cmplwi  5,0
        beq     LafterVMX1

//	Sigh.  AIX 5.2 has no idea that Altivec exists.
//        /* VRSAVE save word : 4 bytes at r1[476 .. 479] */
//        mfspr   5,256        /* vrsave reg is spr number 256 */
//        stw     5,476(1)
//
//        /* Vector reg save area (quadword aligned): 
//	   192 bytes at r1[480 .. 671] */
//        li      5,656
//        stvx    31,5,1
//        li      5,640
//        stvx    30,5,1
//        li      5,624
//        stvx    29,5,1
//        li      5,608
//        stvx    28,5,1
//        li      5,592
//        stvx    27,5,1
//        li      5,576
//        stvx    26,5,1
//        li      5,560
//        stvx    25,5,1
//        li      5,544
//        stvx    25,5,1
//        li      5,528
//        stvx    23,5,1
//        li      5,512
//        stvx    22,5,1
//        li      5,496
//        stvx    21,5,1
//        li      5,480
//        stvx    20,5,1
LafterVMX1:

        /* Local variable space... */
	/* Put the original guest state pointer at r1[128].  We
           will need to refer to it each time round the dispatch loop.
	   Apart from that, we can use r1[0 .. 128] and r1[132 .. 255]
	   as scratch space. */

        /* r3 holds guest_state */
        /* r4 holds do_profiling */
        mr      31,3      /* r31 (generated code gsp) = r3 */
        stw     3,128(1)  /* stash orig guest_state ptr */

        /* hold dispatch_ctr in r29 */
        lwz     5,tocent__vgPlain_dispatch_ctr(2)
        lwz     29,0(5)

        /* set host FPU control word to the default mode expected 
           by VEX-generated code.  See comments in libvex.h for
           more info. */
        lwz     5,tocent__vgPlain_machine_ppc32_has_FP(2)
        lwz     5,0(5)
        cmplwi  5,0
        beq     LafterFP2

        /* get zero into f3 (tedious) */
        /* note: fsub 3,3,3 is not a reliable way to do this, 
           since if f3 holds a NaN or similar then we don't necessarily
           wind up with zero. */
        li      5,0
        stw     5,64(1)	/* r1[64] is scratch */
        lfs     3,64(1)
        mtfsf   0xFF,3   /* fpscr = f3 */
LafterFP2:

        /* set host AltiVec control word to the default mode expected 
           by VEX-generated code. */
        lwz     5,tocent__vgPlain_machine_ppc32_has_VMX(2)
        lwz     5,0(5)
        cmplwi  5,0
        beq     LafterVMX2

//	Sigh.  AIX 5.2 has no idea that Altivec exists.
//        vspltisw 3,0x0  /* generate zero */
//        mtvscr  3
LafterVMX2:

        /* fetch %CIA into r3 */
        lwz     3,OFFSET_ppc32_CIA(31)

        /* fall into main loop (the right one) */
	/* r4 = do_profiling.  It's probably trashed after here,
           but that's OK: we don't need it after here. */
	cmplwi	4,0
	beq	VG_(run_innerloop__dispatch_unprofiled)
	b	VG_(run_innerloop__dispatch_profiled)
	/*NOTREACHED*/

/*----------------------------------------------------*/
/*--- NO-PROFILING (standard) dispatcher           ---*/
/*----------------------------------------------------*/

.globl VG_(run_innerloop__dispatch_unprofiled)
VG_(run_innerloop__dispatch_unprofiled):
	/* At entry: Live regs:
		r1  (=sp)
		r3  (=CIA = next guest address)
		r29 (=dispatch_ctr)
		r31 (=guest_state)
	   Stack state:
		128(r1) (=orig guest_state)
	*/

	/* Has the guest state pointer been messed with?  If yes, exit.
	   Also set up & VG_(tt_fast) early in an attempt at better
           scheduling. */
        lwz     5,128(1)         /* original guest_state ptr */
        cmpw    5,31
	lwz	5,tocent__vgPlain_tt_fast(2)	/* r5 = &tt_fast */
        bne	gsp_changed

        /* save the jump address in the guest state */
        stw     3,OFFSET_ppc32_CIA(31)

        /* Are we out of timeslice?  If yes, defer to scheduler. */
	addi	29,29,-1
	cmplwi	29,0
        beq	counter_is_zero

        /* try a fast lookup in the translation cache */
        /* r4 = VG_TT_FAST_HASH(addr)           * sizeof(FastCacheEntry)
              = ((r3 >>u 2) & VG_TT_FAST_MASK)  << 3 */
        rlwinm  4,3,1, 29-VG_TT_FAST_BITS, 28   /* entry# * 8 */
	add     5,5,4	 /* & VG_(tt_fast)[entry#] */
	lwz     6,0(5)   /* .guest */
	lwz     7,4(5)   /* .host */
        cmpw    3,6
        bne     fast_lookup_failed

        /* Found a match.  Call .host. */
        mtctr   7
        bctrl

        /* On return from guest code:
	   r3  holds destination (original) address.
           r31 may be unchanged (guest_state), or may indicate further
           details of the control transfer requested to *r3.
        */
	/* start over */
	b	VG_(run_innerloop__dispatch_unprofiled)
	/*NOTREACHED*/

/*----------------------------------------------------*/
/*--- PROFILING dispatcher (can be much slower)    ---*/
/*----------------------------------------------------*/

.globl VG_(run_innerloop__dispatch_profiled)
VG_(run_innerloop__dispatch_profiled):
	/* At entry: Live regs:
		r1  (=sp)
		r3  (=CIA = next guest address)
		r29 (=dispatch_ctr)
		r31 (=guest_state)
	   Stack state:
		128(r1) (=orig guest_state)
	*/
	/* Has the guest state pointer been messed with?  If yes, exit.
	   Also set up & VG_(tt_fast) early in an attempt at better
           scheduling. */
        lwz     5,128(1)         /* original guest_state ptr */
        cmpw    5,31
	lwz	5,tocent__vgPlain_tt_fast(2)	/* r5 = &tt_fast */
        bne	gsp_changed

        /* save the jump address in the guest state */
        stw     3,OFFSET_ppc32_CIA(31)

        /* Are we out of timeslice?  If yes, defer to scheduler. */
	addi	29,29,-1
	cmplwi	29,0
        beq	counter_is_zero

        /* try a fast lookup in the translation cache */
        /* r4 = VG_TT_FAST_HASH(addr)           * sizeof(FastCacheEntry)
              = ((r3 >>u 2) & VG_TT_FAST_MASK)  << 3 */
        rlwinm  4,3,1, 29-VG_TT_FAST_BITS, 28   /* entry# * 8 */
	add     5,5,4	 /* & VG_(tt_fast)[entry#] */
	lwz     6,0(5)   /* .guest */
	lwz     7,4(5)   /* .host */
        cmpw    3,6
        bne     fast_lookup_failed

        /* increment bb profile counter */
	srwi	4,4,1   /* entry# * sizeof(UInt*) */
	lwz	9,tocent__vgPlain_tt_fastN(2)   /* r9 = &tt_fastN */
        lwzx    8,9,4   /* r7 = tt_fastN[r4] */
        lwz     10,0(8)
        addi    10,10,1
        stw     10,0(8)
	
        /* Found a match.  Call .host. */
        mtctr   7
        bctrl

        /* On return from guest code:
	   r3  holds destination (original) address.
           r31 may be unchanged (guest_state), or may indicate further
           details of the control transfer requested to *r3.
        */
	/* start over */
	b	VG_(run_innerloop__dispatch_unprofiled)
	/*NOTREACHED*/


/*----------------------------------------------------*/
/*--- exit points                                  ---*/
/*----------------------------------------------------*/

gsp_changed:
	/* Someone messed with the gsp (in r31).  Have to
           defer to scheduler to resolve this.  dispatch ctr
	   is not yet decremented, so no need to increment. */
	/* %CIA is NOT up to date here.  First, need to write
	   %r3 back to %CIA, but without trashing %r31 since
	   that holds the value we want to return to the scheduler.
	   Hence use %r5 transiently for the guest state pointer. */
        lwz     5,128(1)        /* original guest_state ptr */
        stw     3,OFFSET_ppc32_CIA(5)
	mr	3,31		/* r3 = new gsp value */
	b	run_innerloop_exit
	/*NOTREACHED*/

counter_is_zero:
	/* %CIA is up to date */
	/* back out decrement of the dispatch counter */
        addi    29,29,1
        li      3,VG_TRC_INNER_COUNTERZERO
        b       run_innerloop_exit

fast_lookup_failed:
	/* %CIA is up to date */
	/* back out decrement of the dispatch counter */
        addi    29,29,1
        li      3,VG_TRC_INNER_FASTMISS
	b       run_innerloop_exit



/* All exits from the dispatcher go through here.
   r3 holds the return value. 
*/
run_innerloop_exit: 
        /* We're leaving.  Check that nobody messed with
           VSCR or FPSCR. */

        /* Using r10 - value used again further on, so don't trash! */
        lwz     10,tocent__vgPlain_machine_ppc32_has_FP(2)
        lwz     10,0(10)
        cmplwi  10,0
        beq     LafterFP8

	/* Set fpscr back to a known state, since vex-generated code
	   may have messed with fpscr[rm]. */
        li      5,0
        stw     5,64(1)	 /* r1[64] is scratch */
        lfs     3,64(1)
        mtfsf   0xFF,3   /* fpscr = f3 */
LafterFP8:

	/* Using r11 - value used again further on, so don't trash! */
        lwz     11,tocent__vgPlain_machine_ppc32_has_VMX(2)
        lwz     11,0(11)
        cmplwi  11,0
        beq     LafterVMX8

//	Sigh.  AIX 5.2 has no idea that Altivec exists.
//        /* Check VSCR[NJ] == 1 */
//        /* first generate 4x 0x00010000 */
//        vspltisw  4,0x1                   /* 4x 0x00000001 */
//        vspltisw  5,0x0                   /* zero */
//        vsldoi    6,4,5,0x2               /* <<2*8 => 4x 0x00010000 */
//        /* retrieve VSCR and mask wanted bits */
//        mfvscr    7
//        vand      7,7,6                   /* gives NJ flag */
//        vspltw    7,7,0x3                 /* flags-word to all lanes */
//        vcmpequw. 8,6,7                   /* CR[24] = 1 if v6 == v7 */
//        bt        24,invariant_violation  /* branch if all_equal */
LafterVMX8:

	/* otherwise we're OK */
        b       run_innerloop_exit_REALLY


invariant_violation:
        li      3,VG_TRC_INVARIANT_FAILED
        b       run_innerloop_exit_REALLY

run_innerloop_exit_REALLY:
        /* r3 holds VG_TRC_* value to return */

        /* Write ctr to VG(dispatch_ctr) */
        lwz     5,tocent__vgPlain_dispatch_ctr(2)
        stw     29,0(5)

        /* Restore callee-saved registers... */

        /* r10 already holds VG_(machine_ppc32_has_FP) value */
        cmplwi  10,0
        beq     LafterFP9

        /* Floating-point regs */
        lfd     31,392(1)
        lfd     30,384(1)
        lfd     29,376(1)
        lfd     28,368(1)
        lfd     27,360(1)
        lfd     26,352(1)
        lfd     25,344(1)
        lfd     24,336(1)
        lfd     23,328(1)
        lfd     22,320(1)
        lfd     21,312(1)
        lfd     20,304(1)
        lfd     19,296(1)
        lfd     18,288(1)
        lfd     17,280(1)
        lfd     16,272(1)
        lfd     15,264(1)
        lfd     14,256(1)
LafterFP9:

        /* General regs */
        lwz     31,472(1)
        lwz     30,468(1)
        lwz     29,464(1)
        lwz     28,460(1)
        lwz     27,456(1)
        lwz     26,452(1)
        lwz     25,448(1)
        lwz     24,444(1)
        lwz     23,440(1)
        lwz     22,436(1)
        lwz     21,432(1)
        lwz     20,428(1)
        lwz     19,424(1)
        lwz     18,420(1)
        lwz     17,416(1)
        lwz     16,412(1)
        lwz     15,408(1)
        lwz     14,404(1)
        lwz     13,400(1)

        /* r11 already holds VG_(machine_ppc32_has_VMX) value */
        cmplwi  11,0
        beq     LafterVMX9

//       Sigh.  AIX 5.2 has no idea that Altivec exists.
//        /* VRSAVE */
//        lwz     4,476(1)
//        mtspr   4,256         /* VRSAVE reg is spr number 256 */
//
//        /* Vector regs */
//        li      4,656
//        lvx     31,4,1
//        li      4,640
//        lvx     30,4,1
//        li      4,624
//        lvx     29,4,1
//        li      4,608
//        lvx     28,4,1
//        li      4,592
//        lvx     27,4,1
//        li      4,576
//        lvx     26,4,1
//        li      4,560
//        lvx     25,4,1
//        li      4,544
//        lvx     24,4,1
//        li      4,528
//        lvx     23,4,1
//        li      4,512
//        lvx     22,4,1
//        li      4,496
//        lvx     21,4,1
//        li      4,480
//        lvx     20,4,1
LafterVMX9:

	/* r3 is live here; don't trash it */
        /* restore lr,cr,sp */
	addi	4,1,1024  /* r4 = old SP */
	lwz	0,8(4)
	mtlr	0
	lwz	0,4(4)
	mtcr	0
	mr	1,4
	blr

LT..vgPlain_run_innerloop:
    .long 0
    .byte 0,0,32,64,0,0,2,0
    .long 0
    .long LT..vgPlain_run_innerloop-.vgPlain_run_innerloop
    .short 13
    .byte "vgPlain_run_innerloop"
    .align 2
_section_.text:
    .csect .data[RW],3
    .long _section_.text

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
.csect .text[PR]
.align 2
.globl  .VG_(run_a_noredir_translation)
.VG_(run_a_noredir_translation):

	/* Rather than attempt to make sense of the AIX ABI, just
           drop r1 by 256 (to get away from the caller's frame), then
	   512 (to give ourselves a 512-byte save area), and then
	   another 256 (to clear our save area).  In all, drop r1 by 1024
	   and dump stuff on the stack at 256(1)..768(1).  */
	/* At entry, r3 points to argblock */

        /* ----- entry point to VG_(run_innerloop) ----- */
        /* For AIX/ppc32 we do:	 LR-> +8(parent_sp), CR-> +4(parent_sp) */

        /* Save lr and cr*/
        mflr    0
        stw     0,8(1)
	mfcr	0
	stw	0,4(1)

        /* New stack frame */
        stwu    1,-1024(1)  /* sp should maintain 16-byte alignment */

        /* General reg save area : 76 bytes at r1[400 .. 475] */
        stw     31,472(1)
        stw     30,468(1)
        stw     29,464(1)
        stw     28,460(1)
        stw     27,456(1)
        stw     26,452(1)
        stw     25,448(1)
        stw     24,444(1)
        stw     23,440(1)
        stw     22,436(1)
        stw     21,432(1)
        stw     20,428(1)
        stw     19,424(1)
        stw     18,420(1)
        stw     17,416(1)
        stw     16,412(1)
        stw     15,408(1)
        stw     14,404(1)
        stw     13,400(1)
	stw	3,396(1)	/* will need it later */
	
        lwz     31,4(3)		/* rd argblock[1] */
        lwz     30,0(3)		/* rd argblock[0] */
        mtlr    30		/* run translation */
        blrl

        lwz     4,396(1)	/* &argblock */
        stw     3,  8(4)	/* wr argblock[2] */
        stw     31,12(4)	/* wr argblock[3] */
			
        /* General regs */
        lwz     31,472(1)
        lwz     30,468(1)
        lwz     29,464(1)
        lwz     28,460(1)
        lwz     27,456(1)
        lwz     26,452(1)
        lwz     25,448(1)
        lwz     24,444(1)
        lwz     23,440(1)
        lwz     22,436(1)
        lwz     21,432(1)
        lwz     20,428(1)
        lwz     19,424(1)
        lwz     18,420(1)
        lwz     17,416(1)
        lwz     16,412(1)
        lwz     15,408(1)
        lwz     14,404(1)
        lwz     13,400(1)

        /* restore lr,cr,sp */
	addi	4,1,1024  /* r4 = old SP */
	lwz	0,8(4)
	mtlr	0
	lwz	0,4(4)
	mtcr	0
	mr	1,4
	blr

#endif // defined(VGP_ppc32_aix5)

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
